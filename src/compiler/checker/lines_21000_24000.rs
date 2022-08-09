#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{TypeFacts, WideningKind};
use crate::{
    create_symbol_table, is_function_type_node, is_identifier, is_method_signature, map, same_map,
    some, IndexInfo, NamedDeclarationInterface, SymbolInterface, SyntaxKind, TypeComparer,
    TypeMapperCallback, __String, declaration_name_to_string, get_name_of_declaration,
    get_object_flags, get_source_file_of_node, is_call_signature_declaration,
    is_check_js_enabled_for_file, is_in_js_file, is_type_node_kind, is_write_only_access,
    node_is_missing, DiagnosticMessage, Diagnostics, InferenceContext, InferenceFlags,
    InferenceInfo, InferencePriority, Node, NodeInterface, ObjectFlags, Signature, Symbol,
    SymbolFlags, Ternary, Type, TypeChecker, TypeFlags, TypeInterface, UnionReduction,
    WideningContext,
};

impl TypeChecker {
    pub(super) fn get_regular_type_of_object_literal(&self, type_: &Type) -> Rc<Type> {
        if !(self.is_object_literal_type(type_)
            && get_object_flags(type_).intersects(ObjectFlags::FreshLiteral))
        {
            return type_.type_wrapper();
        }
        let regular_type = type_
            .as_fresh_object_literal_type()
            .maybe_regular_type()
            .clone();
        if let Some(regular_type) = regular_type {
            return regular_type;
        }

        let resolved = type_.type_wrapper();
        let resolved_as_resolved_type = resolved.as_resolved_type();
        let members = self.transform_type_of_members(type_, |type_: &Type| {
            self.get_regular_type_of_object_literal(type_)
        });
        let regular_new: Rc<Type> = self
            .create_anonymous_type(
                resolved.maybe_symbol(),
                Rc::new(RefCell::new(members)),
                resolved_as_resolved_type.call_signatures().clone(),
                resolved_as_resolved_type.construct_signatures().clone(),
                resolved_as_resolved_type.index_infos().clone(),
            )
            .into();
        regular_new.set_flags(resolved.flags());
        let regular_new_as_object_flags_type = regular_new.as_object_flags_type();
        regular_new_as_object_flags_type.set_object_flags(
            regular_new_as_object_flags_type.object_flags()
                | resolved_as_resolved_type.object_flags() & !ObjectFlags::FreshLiteral,
        );
        *type_.as_fresh_object_literal_type().maybe_regular_type() = Some(regular_new.clone());
        regular_new
    }

    pub(super) fn create_widening_context(
        &self,
        parent: Option<Rc<RefCell<WideningContext>>>,
        property_name: Option<__String>,
        siblings: Option<Vec<Rc<Type>>>,
    ) -> WideningContext {
        WideningContext {
            parent,
            property_name,
            siblings,
            resolved_properties: None,
        }
    }

    pub(super) fn get_siblings_of_context(
        &self,
        context: Rc<RefCell<WideningContext>>,
    ) -> Vec<Rc<Type>> {
        if (*context).borrow().siblings.is_none() {
            let mut siblings: Vec<Rc<Type>> = vec![];
            for type_ in &self.get_siblings_of_context((*context).borrow().parent.clone().unwrap())
            {
                if self.is_object_literal_type(type_) {
                    let prop = self.get_property_of_object_type(
                        type_,
                        (*context).borrow().property_name.as_ref().unwrap(),
                    );
                    if let Some(prop) = prop.as_ref() {
                        self.for_each_type(&self.get_type_of_symbol(prop), |t: &Type| {
                            siblings.push(t.type_wrapper());
                            Option::<()>::None
                        });
                    }
                }
            }
            context.borrow_mut().siblings = Some(siblings);
        }
        (*context).borrow().siblings.clone().unwrap()
    }

    pub(super) fn get_properties_of_context(
        &self,
        context: Rc<RefCell<WideningContext>>,
    ) -> Vec<Rc<Symbol>> {
        if (*context).borrow().resolved_properties.is_none() {
            let mut names: HashMap<__String, Rc<Symbol>> = HashMap::new();
            for t in &self.get_siblings_of_context(context.clone()) {
                if self.is_object_literal_type(t)
                    && !get_object_flags(t).intersects(ObjectFlags::ContainsSpread)
                {
                    for prop in self.get_properties_of_type(t) {
                        names.insert(prop.escaped_name().clone(), prop);
                    }
                }
            }
            context.borrow_mut().resolved_properties =
                Some(names.values().map(Clone::clone).collect());
        }
        (*context).borrow().resolved_properties.clone().unwrap()
    }

    pub(super) fn get_widened_property(
        &self,
        prop: &Symbol,
        context: Option<Rc<RefCell<WideningContext>>>,
    ) -> Rc<Symbol> {
        if !prop.flags().intersects(SymbolFlags::Property) {
            return prop.symbol_wrapper();
        }
        let original = self.get_type_of_symbol(prop);
        let prop_context = context.map(|context| {
            Rc::new(RefCell::new(self.create_widening_context(
                Some(context),
                Some(prop.escaped_name().clone()),
                None,
            )))
        });
        let widened = self.get_widened_type_with_context(&original, prop_context);
        if Rc::ptr_eq(&widened, &original) {
            prop.symbol_wrapper()
        } else {
            self.create_symbol_with_type(prop, Some(widened))
        }
    }

    pub(super) fn get_undefined_property(&self, prop: &Symbol) -> Rc<Symbol> {
        let cached = self
            .undefined_properties()
            .get(prop.escaped_name())
            .map(Clone::clone);
        if let Some(cached) = cached {
            return cached;
        }
        let result = self.create_symbol_with_type(prop, Some(self.missing_type()));
        result.set_flags(result.flags() | SymbolFlags::Optional);
        self.undefined_properties()
            .insert(prop.escaped_name().clone(), result.clone());
        result
    }

    pub(super) fn get_widened_type_of_object_literal(
        &self,
        type_: &Type,
        context: Option<Rc<RefCell<WideningContext>>>,
    ) -> Rc<Type> {
        let mut members = create_symbol_table(None);
        for prop in &self.get_properties_of_object_type(type_) {
            members.insert(
                prop.escaped_name().clone(),
                self.get_widened_property(prop, context.clone()),
            );
        }
        if let Some(context) = context {
            for prop in &self.get_properties_of_context(context) {
                if !members.contains_key(prop.escaped_name()) {
                    members.insert(
                        prop.escaped_name().clone(),
                        self.get_undefined_property(prop),
                    );
                }
            }
        }
        let result: Rc<Type> = self
            .create_anonymous_type(
                type_.maybe_symbol(),
                Rc::new(RefCell::new(members)),
                vec![],
                vec![],
                same_map(
                    Some(&self.get_index_infos_of_type(type_)),
                    |info: &Rc<IndexInfo>, _| {
                        Rc::new(self.create_index_info(
                            info.key_type.clone(),
                            self.get_widened_type(&info.type_),
                            info.is_readonly,
                            None,
                        ))
                    },
                )
                .unwrap(),
            )
            .into();
        let result_as_object_flags_type = result.as_object_flags_type();
        result_as_object_flags_type.set_object_flags(
            result_as_object_flags_type.object_flags()
                | (get_object_flags(type_)
                    & (ObjectFlags::JSLiteral | ObjectFlags::NonInferrableType)),
        );
        result
    }

    pub(super) fn get_widened_type(&self, type_: &Type) -> Rc<Type> {
        self.get_widened_type_with_context(type_, None)
    }

    pub(super) fn get_widened_type_with_context(
        &self,
        type_: &Type,
        context: Option<Rc<RefCell<WideningContext>>>,
    ) -> Rc<Type> {
        if get_object_flags(type_).intersects(ObjectFlags::RequiresWidening) {
            if context.is_none() {
                if let Some(type_widened) = type_.maybe_widened().clone() {
                    return type_widened;
                }
            }
            let mut result: Option<Rc<Type>> = None;
            if type_
                .flags()
                .intersects(TypeFlags::Any | TypeFlags::Nullable)
            {
                result = Some(self.any_type());
            } else if self.is_object_literal_type(type_) {
                result = Some(self.get_widened_type_of_object_literal(type_, context.clone()));
            } else if type_.flags().intersects(TypeFlags::Union) {
                let union_context = context.clone().unwrap_or_else(|| {
                    Rc::new(RefCell::new(
                        self.create_widening_context(
                            None,
                            None,
                            Some(
                                type_
                                    .as_union_or_intersection_type_interface()
                                    .types()
                                    .to_owned(),
                            ),
                        ),
                    ))
                });
                let widened_types = same_map(
                    Some(type_.as_union_or_intersection_type_interface().types()),
                    |t: &Rc<Type>, _| {
                        if t.flags().intersects(TypeFlags::Nullable) {
                            t.clone()
                        } else {
                            self.get_widened_type_with_context(t, Some(union_context.clone()))
                        }
                    },
                )
                .unwrap();
                let union_reduction = if some(
                    Some(&widened_types),
                    Some(|type_: &Rc<Type>| self.is_empty_object_type(type_)),
                ) {
                    UnionReduction::Subtype
                } else {
                    UnionReduction::Literal
                };
                result = Some(self.get_union_type(
                    widened_types,
                    Some(union_reduction),
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                ));
            } else if type_.flags().intersects(TypeFlags::Intersection) {
                result = Some(
                    self.get_intersection_type(
                        &same_map(
                            Some(type_.as_union_or_intersection_type_interface().types()),
                            |type_: &Rc<Type>, _| self.get_widened_type(type_),
                        )
                        .unwrap(),
                        Option::<&Symbol>::None,
                        None,
                    ),
                );
            } else if self.is_array_type(type_) || self.is_tuple_type(type_) {
                result = Some(self.create_type_reference(
                    &type_.as_type_reference().target,
                    same_map(
                        Some(&self.get_type_arguments(type_)),
                        |type_: &Rc<Type>, _| self.get_widened_type(type_),
                    ),
                ));
            }
            if result.is_some() && context.is_none() {
                *type_.maybe_widened() = result.clone();
            }
            return result.unwrap_or_else(|| type_.type_wrapper());
        }
        type_.type_wrapper()
    }

    pub(super) fn report_widening_errors_in_type(&self, type_: &Type) -> bool {
        let mut error_reported = false;
        if get_object_flags(type_).intersects(ObjectFlags::ContainsWideningType) {
            if type_.flags().intersects(TypeFlags::Union) {
                if some(
                    Some(type_.as_union_or_intersection_type_interface().types()),
                    Some(|type_: &Rc<Type>| self.is_empty_object_type(type_)),
                ) {
                    error_reported = true;
                } else {
                    for t in type_.as_union_or_intersection_type_interface().types() {
                        if self.report_widening_errors_in_type(t) {
                            error_reported = true;
                        }
                    }
                }
            }
            if self.is_array_type(type_) || self.is_tuple_type(type_) {
                for t in &self.get_type_arguments(type_) {
                    if self.report_widening_errors_in_type(t) {
                        error_reported = true;
                    }
                }
            }
            if self.is_object_literal_type(type_) {
                for p in &self.get_properties_of_object_type(type_) {
                    let t = self.get_type_of_symbol(p);
                    if get_object_flags(&t).intersects(ObjectFlags::ContainsWideningType) {
                        if !self.report_widening_errors_in_type(&t) {
                            self.error(
                                p.maybe_value_declaration(),
                                &Diagnostics::Object_literal_s_property_0_implicitly_has_an_1_type,
                                Some(vec![
                                    self.symbol_to_string_(
                                        p,
                                        Option::<&Node>::None,
                                        None,
                                        None,
                                        None,
                                    ),
                                    self.type_to_string_(
                                        &self.get_widened_type(&t),
                                        Option::<&Node>::None,
                                        None,
                                        None,
                                    ),
                                ]),
                            );
                        }
                        error_reported = true;
                    }
                }
            }
        }
        error_reported
    }

    pub(super) fn report_implicit_any(
        &self,
        declaration: &Node, /*Declaration*/
        type_: &Type,
        widening_kind: Option<WideningKind>,
    ) {
        let type_as_string = self.type_to_string_(
            &self.get_widened_type(type_),
            Option::<&Node>::None,
            None,
            None,
        );
        if is_in_js_file(Some(declaration))
            && !is_check_js_enabled_for_file(
                &get_source_file_of_node(Some(declaration)).unwrap(),
                &self.compiler_options,
            )
        {
            return;
        }
        let diagnostic: &'static DiagnosticMessage;
        match declaration.kind() {
            SyntaxKind::BinaryExpression
            | SyntaxKind::PropertyDeclaration
            | SyntaxKind::PropertySignature => {
                diagnostic = if self.no_implicit_any {
                    &Diagnostics::Member_0_implicitly_has_an_1_type
                } else {
                    &Diagnostics::Member_0_implicitly_has_an_1_type_but_a_better_type_may_be_inferred_from_usage
                };
            }
            SyntaxKind::Parameter => {
                let param = declaration;
                let param_as_parameter_declaration = param.as_parameter_declaration();
                if is_identifier(&param_as_parameter_declaration.name())
                    && (is_call_signature_declaration(&param.parent())
                        || is_method_signature(&param.parent())
                        || is_function_type_node(&param.parent()))
                    && param
                        .parent()
                        .as_function_like_declaration()
                        .parameters()
                        .into_iter()
                        .position(|parameter: &Rc<Node>| ptr::eq(param, &**parameter))
                        .is_some()
                    && (self
                        .resolve_name_(
                            Some(param),
                            &param_as_parameter_declaration
                                .name()
                                .as_identifier()
                                .escaped_text,
                            SymbolFlags::Type,
                            None,
                            Some(
                                param_as_parameter_declaration
                                    .name()
                                    .as_identifier()
                                    .escaped_text
                                    .clone(),
                            ),
                            true,
                            None,
                        )
                        .is_some()
                        || matches!(
                            param_as_parameter_declaration.name().as_identifier().original_keyword_kind,
                            Some(param_name_original_keyword_kind) if is_type_node_kind(param_name_original_keyword_kind)
                        ))
                {
                    let new_name = format!(
                        "arg{}",
                        param
                            .parent()
                            .as_function_like_declaration()
                            .parameters()
                            .into_iter()
                            .position(|parameter: &Rc<Node>| ptr::eq(param, &**parameter))
                            .unwrap()
                            .to_string()
                    );
                    let type_name = format!(
                        "{}{}",
                        declaration_name_to_string(Some(param_as_parameter_declaration.name())),
                        if param_as_parameter_declaration.dot_dot_dot_token.is_some() {
                            "[]"
                        } else {
                            ""
                        }
                    );
                    self.error_or_suggestion(
                        self.no_implicit_any,
                        declaration,
                        Diagnostics::Parameter_has_a_name_but_no_type_Did_you_mean_0_Colon_1
                            .clone()
                            .into(),
                        Some(vec![new_name, type_name]),
                    );
                    return;
                }
                diagnostic = if declaration
                    .as_parameter_declaration()
                    .dot_dot_dot_token
                    .is_some()
                {
                    if self.no_implicit_any {
                        &Diagnostics::Rest_parameter_0_implicitly_has_an_any_type
                    } else {
                        &Diagnostics::Rest_parameter_0_implicitly_has_an_any_type_but_a_better_type_may_be_inferred_from_usage
                    }
                } else {
                    if self.no_implicit_any {
                        &Diagnostics::Parameter_0_implicitly_has_an_1_type
                    } else {
                        &Diagnostics::Parameter_0_implicitly_has_an_1_type_but_a_better_type_may_be_inferred_from_usage
                    }
                };
            }
            SyntaxKind::BindingElement => {
                diagnostic = &Diagnostics::Binding_element_0_implicitly_has_an_1_type;
                if !self.no_implicit_any {
                    return;
                }
            }
            SyntaxKind::JSDocFunctionType => {
                self.error(
                    Some(declaration),
                    &Diagnostics::Function_type_which_lacks_return_type_annotation_implicitly_has_an_0_return_type,
                    Some(vec![
                        type_as_string
                    ])
                );
                return;
            }
            SyntaxKind::FunctionDeclaration
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::MethodSignature
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction => {
                if self.no_implicit_any && declaration.as_named_declaration().maybe_name().is_none()
                {
                    if widening_kind == Some(WideningKind::GeneratorYield) {
                        self.error(
                            Some(declaration),
                            &Diagnostics::Generator_implicitly_has_yield_type_0_because_it_does_not_yield_any_values_Consider_supplying_a_return_type_annotation,
                            Some(vec![
                                type_as_string
                            ])
                        );
                    } else {
                        self.error(
                            Some(declaration),
                            &Diagnostics::Function_expression_which_lacks_return_type_annotation_implicitly_has_an_0_return_type,
                            Some(vec![
                                type_as_string
                            ])
                        );
                    }
                    return;
                }
                diagnostic = if !self.no_implicit_any {
                    &Diagnostics::_0_implicitly_has_an_1_return_type_but_a_better_type_may_be_inferred_from_usage
                } else if widening_kind == Some(WideningKind::GeneratorYield) {
                    &Diagnostics::_0_which_lacks_return_type_annotation_implicitly_has_an_1_yield_type
                } else {
                    &Diagnostics::_0_which_lacks_return_type_annotation_implicitly_has_an_1_return_type
                };
            }
            SyntaxKind::MappedType => {
                if self.no_implicit_any {
                    self.error(
                        Some(declaration),
                        &Diagnostics::Mapped_object_type_implicitly_has_an_any_template_type,
                        None,
                    );
                }
                return;
            }
            _ => {
                diagnostic = if self.no_implicit_any {
                    &Diagnostics::Variable_0_implicitly_has_an_1_type
                } else {
                    &Diagnostics::Variable_0_implicitly_has_an_1_type_but_a_better_type_may_be_inferred_from_usage
                };
            }
        }
        self.error_or_suggestion(
            self.no_implicit_any,
            declaration,
            // TODO: should this type contain &'static DiagnosticMessage instead of DiagnosticMessage?
            diagnostic.clone().into(),
            Some(vec![
                declaration_name_to_string(get_name_of_declaration(Some(declaration))).into_owned(),
                type_as_string,
            ]),
        );
    }

    pub(super) fn report_errors_from_widening(
        &self,
        declaration: &Node, /*Declaration*/
        type_: &Type,
        widening_kind: Option<WideningKind>,
    ) {
        if self.produce_diagnostics
            && self.no_implicit_any
            && get_object_flags(type_).intersects(ObjectFlags::ContainsWideningType)
            && (widening_kind.is_none()
                || self
                    .get_contextual_signature_for_function_like_declaration(declaration)
                    .is_none())
        {
            if !self.report_widening_errors_in_type(type_) {
                self.report_implicit_any(declaration, type_, widening_kind);
            }
        }
    }

    pub(super) fn apply_to_parameter_types<TCallback: FnMut(&Type, &Type)>(
        &self,
        source: &Signature,
        target: &Signature,
        mut callback: TCallback,
    ) {
        let source_count = self.get_parameter_count(source);
        let target_count = self.get_parameter_count(target);
        let source_rest_type = self.get_effective_rest_type(source);
        let target_rest_type = self.get_effective_rest_type(target);
        let target_non_rest_count = if target_rest_type.is_some() {
            target_count - 1
        } else {
            target_count
        };
        let param_count = if source_rest_type.is_some() {
            target_non_rest_count
        } else {
            cmp::min(source_count, target_non_rest_count)
        };
        let source_this_type = self.get_this_type_of_signature(source);
        if let Some(source_this_type) = source_this_type.as_ref() {
            let target_this_type = self.get_this_type_of_signature(target);
            if let Some(target_this_type) = target_this_type.as_ref() {
                callback(source_this_type, target_this_type);
            }
        }
        for i in 0..param_count {
            callback(
                &self.get_type_at_position(source, i),
                &self.get_type_at_position(target, i),
            );
        }
        if let Some(target_rest_type) = target_rest_type.as_ref() {
            callback(
                &self.get_rest_type_at_position(source, param_count),
                target_rest_type,
            );
        }
    }

    pub(super) fn apply_to_return_types<TCallback: FnMut(&Type, &Type)>(
        &self,
        source: Rc<Signature>,
        target: Rc<Signature>,
        mut callback: TCallback,
    ) {
        let source_type_predicate = self.get_type_predicate_of_signature(&source);
        let target_type_predicate = self.get_type_predicate_of_signature(&target);
        let mut took_if_branch = false;
        if let Some(source_type_predicate) = source_type_predicate.as_ref() {
            if let Some(target_type_predicate) = target_type_predicate.as_ref() {
                if self.type_predicate_kinds_match(source_type_predicate, target_type_predicate) {
                    if let Some(source_type_predicate_type) = source_type_predicate.type_.as_ref() {
                        if let Some(target_type_predicate_type) =
                            target_type_predicate.type_.as_ref()
                        {
                            callback(source_type_predicate_type, target_type_predicate_type);
                            took_if_branch = true;
                        }
                    }
                }
            }
        }
        if !took_if_branch {
            callback(
                &self.get_return_type_of_signature(source),
                &self.get_return_type_of_signature(target),
            );
        }
    }

    pub(super) fn create_inference_context(
        &self,
        type_parameters: &[Rc<Type /*TypeParameter*/>],
        signature: Option<Rc<Signature>>,
        flags: InferenceFlags,
        compare_types: Option<Rc<dyn TypeComparer>>,
    ) -> Rc<InferenceContext> {
        self.create_inference_context_worker(
            type_parameters
                .into_iter()
                .map(|type_parameter: &Rc<Type>| {
                    Rc::new(self.create_inference_info(type_parameter))
                })
                .collect(),
            signature,
            flags,
            compare_types.unwrap_or_else(|| {
                Rc::new(TypeComparerCompareTypesAssignable::new(self.rc_wrapper()))
            }),
        )
    }

    pub(super) fn clone_inference_context(
        &self,
        context: Option<&InferenceContext>,
        extra_flags: Option<InferenceFlags>,
    ) -> Option<Rc<InferenceContext>> {
        let extra_flags = extra_flags.unwrap_or(InferenceFlags::None);
        context.map(|context| {
            self.create_inference_context_worker(
                map(
                    Some(&context.inferences),
                    |inference: &Rc<InferenceInfo>, _| {
                        Rc::new(self.clone_inference_info(inference))
                    },
                )
                .unwrap(),
                context.signature.clone(),
                context.flags | extra_flags,
                context.compare_types.clone(),
            )
        })
    }

    pub(super) fn create_inference_context_worker(
        &self,
        inferences: Vec<Rc<InferenceInfo>>,
        signature: Option<Rc<Signature>>,
        flags: InferenceFlags,
        compare_types: Rc<dyn TypeComparer>,
    ) -> Rc<InferenceContext> {
        let context = Rc::new(InferenceContext::new(
            inferences,
            signature,
            flags,
            compare_types,
            None,
            None,
            None,
            None,
        ));
        context.set_mapper(self.make_function_type_mapper(
            CreateInferenceContextWorkerMapperCallback::new(context.clone()),
        ));
        context.set_non_fixing_mapper(self.make_function_type_mapper(
            CreateInferenceContextWorkerNonFixingMapperCallback::new(context.clone()),
        ));
        context
    }

    pub(super) fn map_to_inferred_type(
        &self,
        context: &InferenceContext,
        t: &Type,
        fix: bool,
    ) -> Rc<Type> {
        let inferences = &context.inferences;
        for (i, inference) in inferences.into_iter().enumerate() {
            if ptr::eq(t, &*inference.type_parameter) {
                if fix && !inference.is_fixed() {
                    self.clear_cached_inferences(inferences);
                    inference.set_is_fixed(true);
                }
                return self.get_inferred_type(context, i);
            }
        }
        t.type_wrapper()
    }

    pub(super) fn clear_cached_inferences(&self, inferences: &[Rc<InferenceInfo>]) {
        unimplemented!()
    }

    pub(super) fn create_inference_info(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> InferenceInfo {
        unimplemented!()
    }

    pub(super) fn clone_inference_info(&self, inference: &InferenceInfo) -> InferenceInfo {
        unimplemented!()
    }

    pub(super) fn could_contain_type_variables(&self, type_: &Type) -> bool {
        let object_flags = get_object_flags(&type_);
        if object_flags.intersects(ObjectFlags::CouldContainTypeVariablesComputed) {
            return object_flags.intersects(ObjectFlags::CouldContainTypeVariables);
        }
        let result = type_.flags().intersects(TypeFlags::Instantiable) || unimplemented!();
        if type_.flags().intersects(TypeFlags::ObjectFlagsType) {
            let type_as_has_object_flags = type_.as_object_flags_type();
            type_as_has_object_flags.set_object_flags(
                type_as_has_object_flags.object_flags()
                    | ObjectFlags::CouldContainTypeVariablesComputed
                    | if result {
                        ObjectFlags::CouldContainTypeVariables
                    } else {
                        ObjectFlags::None
                    },
            );
        }
        result
    }

    pub(super) fn infer_type_for_homomorphic_mapped_type(
        &self,
        source: &Type,
        target: &Type,     /*MappedType*/
        constraint: &Type, /*IndexType*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_reverse_mapped_symbol(
        &self,
        symbol: &Symbol, /*ReverseMappedSymbol*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn infer_reverse_mapped_type(
        &self,
        source_type: &Type,
        target: &Type,     /*MappedType*/
        constraint: &Type, /*IndexType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_unmatched_properties(
        &self,
        source: &Type,
        target: &Type,
        require_optional_properties: bool,
        match_discriminant_properties: bool,
    ) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_unmatched_property(
        &self,
        source: &Type,
        target: &Type,
        require_optional_properties: bool,
        match_discriminant_properties: bool,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn template_literal_types_definitely_unrelated(
        &self,
        source: &Type, /*TemplateLiteralType*/
        target: &Type, /*TemplateLiteralType*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_type_matched_by_template_literal_type(
        &self,
        source: &Type,
        target: &Type, /*TemplateLiteralType*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn infer_types(
        &self,
        inferences: &[Rc<InferenceInfo>],
        original_source: &Type,
        original_target: &Type,
        priority: Option<InferencePriority>,
        contravariant: Option<bool>,
    ) {
        let priority = priority.unwrap_or(InferencePriority::None);
        let contravariant = contravariant.unwrap_or(false);
        unimplemented!()
    }

    pub(super) fn is_object_literal_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::ObjectLiteral)
    }

    pub(super) fn is_object_or_array_literal_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::ObjectLiteral | ObjectFlags::ArrayLiteral)
    }

    pub(super) fn get_inferred_type(&self, context: &InferenceContext, index: usize) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_default_type_argument_type(&self, is_in_java_script_file: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_cannot_find_name_diagnostic_for_name(
        &self,
        node: &Node,
    ) -> &'static DiagnosticMessage {
        match node.as_identifier().escaped_text {
            _ => {
                if false {
                    unimplemented!()
                } else {
                    &Diagnostics::Cannot_find_name_0
                }
            }
        }
    }

    pub(super) fn get_resolved_symbol(&self, node: &Node /*Identifier*/) -> Rc<Symbol> {
        let links = self.get_node_links(node);
        let mut links_ref = links.borrow_mut();
        if links_ref.resolved_symbol.is_none() {
            links_ref.resolved_symbol = Some(if !node_is_missing(Some(node)) {
                self.resolve_name_(
                    Some(node),
                    &node.as_identifier().escaped_text,
                    SymbolFlags::Value | SymbolFlags::ExportValue,
                    Some(self.get_cannot_find_name_diagnostic_for_name(node)),
                    Some(node.node_wrapper()),
                    !is_write_only_access(node),
                    Some(false),
                )
                .unwrap_or_else(|| self.unknown_symbol())
            } else {
                self.unknown_symbol()
            });
        }
        links_ref.resolved_symbol.clone().unwrap()
    }

    pub(super) fn is_in_type_query(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_matching_reference(&self, source: &Node, target: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn find_discriminant_properties(
        &self,
        source_properties: &[Rc<Symbol>],
        target: &Type,
    ) -> Option<Vec<Rc<Symbol>>> {
        unimplemented!()
    }

    pub(super) fn get_matching_union_constituent_for_type(
        &self,
        union_type: &Type, /*UnionType*/
        type_: &Type,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn has_matching_argument(
        &self,
        expression: &Node, /*CallExpression | NewExpression*/
        reference: &Node,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_function_object_type(&self, type_: &Type /*ObjectType*/) -> bool {
        unimplemented!()
    }

    pub(super) fn get_type_facts(&self, type_: &Type, ignore_objects: Option<bool>) -> TypeFacts {
        let ignore_objects = ignore_objects.unwrap_or(false);
        unimplemented!()
    }

    pub(super) fn get_type_with_facts(&self, type_: &Type, include: TypeFacts) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_of_initializer(&self, node: &Node /*Expression*/) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_type_subset_of(&self, source: &Type, target: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn for_each_type<TReturn, TCallback: FnMut(&Type) -> Option<TReturn>>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> Option<TReturn> {
        unimplemented!()
    }

    pub(super) fn some_type<TCallback: FnMut(&Type) -> bool>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn every_type<TCallback: FnMut(&Type) -> bool>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn filter_type<TCallback: FnMut(&Type) -> bool>(
        &self,
        type_: &Type,
        mut f: TCallback,
    ) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union) {
            unimplemented!()
        }
        if type_.flags().intersects(TypeFlags::Never) || f(type_) {
            type_.type_wrapper()
        } else {
            self.never_type()
        }
    }

    pub(super) fn remove_type(&self, type_: &Type, target_type: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn count_types(&self, type_: &Type) -> usize {
        unimplemented!()
    }

    pub(super) fn map_type<TMapper: FnMut(&Type) -> Option<Rc<Type>>>(
        &self,
        type_: &Type,
        mapper: &mut TMapper,
        no_reductions: Option<bool>,
    ) -> Option<Rc<Type>> {
        let no_reductions = no_reductions.unwrap_or(false);
        if type_.flags().intersects(TypeFlags::Never) {
            return Some(type_.type_wrapper());
        }
        if !type_.flags().intersects(TypeFlags::Union) {
            return mapper(type_);
        }
        let types = type_.as_union_or_intersection_type_interface().types();
        let mut mapped_types: Vec<Rc<Type>> = vec![];
        let mut changed = false;
        for t in types {
            let mapped = if t.flags().intersects(TypeFlags::Union) {
                self.map_type(&t, mapper, Some(no_reductions))
            } else {
                mapper(&t)
            };
            changed = changed
                || match mapped.as_ref() {
                    None => true,
                    Some(mapped) => !Rc::ptr_eq(t, mapped),
                };
            if let Some(mapped) = mapped {
                mapped_types.push(mapped);
            }
        }
        if changed {
            if !mapped_types.is_empty() {
                Some(self.get_union_type(
                    mapped_types,
                    Some(if no_reductions {
                        UnionReduction::None
                    } else {
                        UnionReduction::Literal
                    }),
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                ))
            } else {
                None
            }
        } else {
            Some(type_.type_wrapper())
        }
    }

    pub(super) fn map_type_with_alias<
        TMapper: FnMut(&Type) -> Rc<Type>,
        TAliasSymbol: Borrow<Symbol>,
    >(
        &self,
        type_: &Type,
        mapper: &mut TMapper,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_constituent_count(&self, type_: &Type) -> usize {
        if type_.flags().intersects(TypeFlags::Union) {
            type_
                .as_union_or_intersection_type_interface()
                .types()
                .len()
        } else {
            1
        }
    }

    pub(super) fn extract_types_of_kind(&self, type_: &Type, kind: TypeFlags) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_flow_type_of_reference<
        TInitialType: Borrow<Type>,
        TFlowContainer: Borrow<Node>,
    >(
        &self,
        reference: &Node,
        declared_type: &Type,
        initial_type: Option<TInitialType>,
        flow_container: Option<TFlowContainer>,
    ) -> Rc<Type> {
        let initial_type = initial_type.map_or_else(
            || declared_type.type_wrapper(),
            |initial_type| initial_type.borrow().type_wrapper(),
        );
        unimplemented!()
    }
}

pub(super) struct CreateInferenceContextWorkerMapperCallback {
    inference_context: Rc<InferenceContext>,
}

impl CreateInferenceContextWorkerMapperCallback {
    pub fn new(inference_context: Rc<InferenceContext>) -> Self {
        Self { inference_context }
    }
}

impl TypeMapperCallback for CreateInferenceContextWorkerMapperCallback {
    fn call(&self, checker: &TypeChecker, t: &Type) -> Rc<Type> {
        checker.map_to_inferred_type(&self.inference_context, t, true)
    }
}

pub(super) struct CreateInferenceContextWorkerNonFixingMapperCallback {
    inference_context: Rc<InferenceContext>,
}

impl CreateInferenceContextWorkerNonFixingMapperCallback {
    pub fn new(inference_context: Rc<InferenceContext>) -> Self {
        Self { inference_context }
    }
}

impl TypeMapperCallback for CreateInferenceContextWorkerNonFixingMapperCallback {
    fn call(&self, checker: &TypeChecker, t: &Type) -> Rc<Type> {
        checker.map_to_inferred_type(&self.inference_context, t, false)
    }
}

struct TypeComparerCompareTypesAssignable {
    type_checker: Rc<TypeChecker>,
}

impl TypeComparerCompareTypesAssignable {
    pub fn new(type_checker: Rc<TypeChecker>) -> Self {
        Self { type_checker }
    }
}

impl TypeComparer for TypeComparerCompareTypesAssignable {
    fn call(&self, s: &Type, t: &Type, _report_errors: Option<bool>) -> Ternary {
        self.type_checker.compare_types_assignable(s, t)
    }
}
