use gc::{Finalize, Gc, GcCell, Trace};
use std::cmp;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;
use std::{cell::RefCell, io};

use super::{MappedTypeModifiers, WideningKind};
use crate::{
    create_symbol_table, escape_leading_underscores, filter, find_ancestor,
    get_declaration_of_kind, is_function_type_node, is_identifier, is_method_signature, map,
    same_map, some, ElementFlags, FindAncestorCallbackReturn, IndexInfo, NamedDeclarationInterface,
    ReverseMappedType, SymbolInterface, SyntaxKind, TransientSymbolInterface, TypeComparer,
    TypeMapper, TypeMapperCallback, __String, declaration_name_to_string, for_each_bool,
    get_name_of_declaration, get_object_flags, get_source_file_of_node,
    is_call_signature_declaration, is_check_js_enabled_for_file, is_in_js_file, is_type_node_kind,
    try_for_each_bool, try_map, try_some, DiagnosticMessage, Diagnostics, InferenceContext,
    InferenceFlags, InferenceInfo, IteratorExt, Node, NodeInterface, ObjectFlags, Signature,
    Symbol, SymbolFlags, Ternary, Type, TypeChecker, TypeFlags, TypeInterface, UnionReduction,
    WideningContext,
};

impl TypeChecker {
    pub(super) fn get_regular_type_of_object_literal(&self, type_: &Type) -> io::Result<Gc<Type>> {
        if !(self.is_object_literal_type(type_)
            && get_object_flags(type_).intersects(ObjectFlags::FreshLiteral))
        {
            return Ok(type_.type_wrapper());
        }
        let regular_type = type_
            .as_fresh_object_literal_type()
            .maybe_regular_type()
            .clone();
        if let Some(regular_type) = regular_type {
            return Ok(regular_type);
        }

        let resolved = type_.type_wrapper();
        let resolved_as_resolved_type = resolved.as_resolved_type();
        let members = self.transform_type_of_members(type_, |type_: &Type| {
            self.get_regular_type_of_object_literal(type_)
        })?;
        let regular_new = self.create_anonymous_type(
            resolved.maybe_symbol(),
            Gc::new(GcCell::new(members)),
            resolved_as_resolved_type.call_signatures().clone(),
            resolved_as_resolved_type.construct_signatures().clone(),
            resolved_as_resolved_type.index_infos().clone(),
        )?;
        regular_new.set_flags(resolved.flags());
        let regular_new_as_object_flags_type = regular_new.as_object_flags_type();
        regular_new_as_object_flags_type.set_object_flags(
            regular_new_as_object_flags_type.object_flags()
                | resolved_as_resolved_type.object_flags() & !ObjectFlags::FreshLiteral,
        );
        *type_.as_fresh_object_literal_type().maybe_regular_type() = Some(regular_new.clone());
        Ok(regular_new)
    }

    pub(super) fn create_widening_context(
        &self,
        parent: Option<Rc<RefCell<WideningContext>>>,
        property_name: Option<__String>,
        siblings: Option<Vec<Gc<Type>>>,
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
    ) -> io::Result<Vec<Gc<Type>>> {
        if (*context).borrow().siblings.is_none() {
            let mut siblings: Vec<Gc<Type>> = vec![];
            for type_ in
                &self.get_siblings_of_context((*context).borrow().parent.clone().unwrap())?
            {
                if self.is_object_literal_type(type_) {
                    let prop = self.get_property_of_object_type(
                        type_,
                        (*context).borrow().property_name.as_ref().unwrap(),
                    )?;
                    if let Some(prop) = prop.as_ref() {
                        self.for_each_type(&*self.get_type_of_symbol(prop)?, |t: &Type| {
                            siblings.push(t.type_wrapper());
                            Option::<()>::None
                        });
                    }
                }
            }
            context.borrow_mut().siblings = Some(siblings);
        }
        Ok((*context).borrow().siblings.clone().unwrap())
    }

    pub(super) fn get_properties_of_context(
        &self,
        context: Rc<RefCell<WideningContext>>,
    ) -> io::Result<Vec<Gc<Symbol>>> {
        if (*context).borrow().resolved_properties.is_none() {
            let mut names: HashMap<__String, Gc<Symbol>> = HashMap::new();
            for t in &self.get_siblings_of_context(context.clone())? {
                if self.is_object_literal_type(t)
                    && !get_object_flags(t).intersects(ObjectFlags::ContainsSpread)
                {
                    for prop in self.get_properties_of_type(t)? {
                        names.insert(prop.escaped_name().to_owned(), prop);
                    }
                }
            }
            context.borrow_mut().resolved_properties =
                Some(names.values().map(Clone::clone).collect());
        }
        Ok((*context).borrow().resolved_properties.clone().unwrap())
    }

    pub(super) fn get_widened_property(
        &self,
        prop: &Symbol,
        context: Option<Rc<RefCell<WideningContext>>>,
    ) -> io::Result<Gc<Symbol>> {
        if !prop.flags().intersects(SymbolFlags::Property) {
            return Ok(prop.symbol_wrapper());
        }
        let original = self.get_type_of_symbol(prop)?;
        let prop_context = context.map(|context| {
            Rc::new(RefCell::new(self.create_widening_context(
                Some(context),
                Some(prop.escaped_name().to_owned()),
                None,
            )))
        });
        let widened = self.get_widened_type_with_context(&original, prop_context)?;
        Ok(if Gc::ptr_eq(&widened, &original) {
            prop.symbol_wrapper()
        } else {
            self.create_symbol_with_type(prop, Some(widened))
        })
    }

    pub(super) fn get_undefined_property(&self, prop: &Symbol) -> Gc<Symbol> {
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
            .insert(prop.escaped_name().to_owned(), result.clone());
        result
    }

    pub(super) fn get_widened_type_of_object_literal(
        &self,
        type_: &Type,
        context: Option<Rc<RefCell<WideningContext>>>,
    ) -> io::Result<Gc<Type>> {
        let mut members = create_symbol_table(Option::<&[Gc<Symbol>]>::None);
        for ref prop in self.get_properties_of_object_type(type_)? {
            members.insert(
                prop.escaped_name().to_owned(),
                self.get_widened_property(prop, context.clone())?,
            );
        }
        if let Some(context) = context {
            for prop in &self.get_properties_of_context(context)? {
                if !members.contains_key(prop.escaped_name()) {
                    members.insert(
                        prop.escaped_name().to_owned(),
                        self.get_undefined_property(prop),
                    );
                }
            }
        }
        let result = self.create_anonymous_type(
            type_.maybe_symbol(),
            Gc::new(GcCell::new(members)),
            vec![],
            vec![],
            try_map(
                &self.get_index_infos_of_type(type_)?,
                |info: &Gc<IndexInfo>, _| -> io::Result<_> {
                    Ok(Gc::new(self.create_index_info(
                        info.key_type.clone(),
                        self.get_widened_type(&info.type_)?,
                        info.is_readonly,
                        None,
                    )))
                },
            )?,
        )?;
        let result_as_object_flags_type = result.as_object_flags_type();
        result_as_object_flags_type.set_object_flags(
            result_as_object_flags_type.object_flags()
                | (get_object_flags(type_)
                    & (ObjectFlags::JSLiteral | ObjectFlags::NonInferrableType)),
        );
        Ok(result)
    }

    pub(super) fn get_widened_type(&self, type_: &Type) -> io::Result<Gc<Type>> {
        self.get_widened_type_with_context(type_, None)
    }

    pub(super) fn get_widened_type_with_context(
        &self,
        type_: &Type,
        context: Option<Rc<RefCell<WideningContext>>>,
    ) -> io::Result<Gc<Type>> {
        if get_object_flags(type_).intersects(ObjectFlags::RequiresWidening) {
            if context.is_none() {
                if let Some(type_widened) = type_.maybe_widened().clone() {
                    return Ok(type_widened);
                }
            }
            let mut result: Option<Gc<Type>> = None;
            if type_
                .flags()
                .intersects(TypeFlags::Any | TypeFlags::Nullable)
            {
                result = Some(self.any_type());
            } else if self.is_object_literal_type(type_) {
                result = Some(self.get_widened_type_of_object_literal(type_, context.clone())?);
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
                let widened_types = try_map(
                    type_.as_union_or_intersection_type_interface().types(),
                    |t: &Gc<Type>, _| -> io::Result<_> {
                        Ok(if t.flags().intersects(TypeFlags::Nullable) {
                            t.clone()
                        } else {
                            self.get_widened_type_with_context(t, Some(union_context.clone()))?
                        })
                    },
                )?;
                let union_reduction = if try_some(
                    Some(&widened_types),
                    Some(|type_: &Gc<Type>| self.is_empty_object_type(type_)),
                )? {
                    UnionReduction::Subtype
                } else {
                    UnionReduction::Literal
                };
                result = Some(self.get_union_type(
                    &widened_types,
                    Some(union_reduction),
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                )?);
            } else if type_.flags().intersects(TypeFlags::Intersection) {
                result = Some(self.get_intersection_type(
                    &try_map(
                        type_.as_union_or_intersection_type_interface().types(),
                        |type_: &Gc<Type>, _| self.get_widened_type(type_),
                    )?,
                    Option::<&Symbol>::None,
                    None,
                )?);
            } else if self.is_array_type(type_) || self.is_tuple_type(type_) {
                result = Some(self.create_type_reference(
                    &type_.as_type_reference().target,
                    Some(try_map(
                        &self.get_type_arguments(type_)?,
                        |type_: &Gc<Type>, _| self.get_widened_type(type_),
                    )?),
                ));
            }
            if result.is_some() && context.is_none() {
                *type_.maybe_widened() = result.clone();
            }
            return Ok(result.unwrap_or_else(|| type_.type_wrapper()));
        }
        Ok(type_.type_wrapper())
    }

    pub(super) fn report_widening_errors_in_type(&self, type_: &Type) -> io::Result<bool> {
        let mut error_reported = false;
        if get_object_flags(type_).intersects(ObjectFlags::ContainsWideningType) {
            if type_.flags().intersects(TypeFlags::Union) {
                if try_some(
                    Some(type_.as_union_or_intersection_type_interface().types()),
                    Some(|type_: &Gc<Type>| self.is_empty_object_type(type_)),
                )? {
                    error_reported = true;
                } else {
                    for t in type_.as_union_or_intersection_type_interface().types() {
                        if self.report_widening_errors_in_type(t)? {
                            error_reported = true;
                        }
                    }
                }
            }
            if self.is_array_type(type_) || self.is_tuple_type(type_) {
                for t in &self.get_type_arguments(type_)? {
                    if self.report_widening_errors_in_type(t)? {
                        error_reported = true;
                    }
                }
            }
            if self.is_object_literal_type(type_) {
                for ref p in self.get_properties_of_object_type(type_)? {
                    let t = self.get_type_of_symbol(p)?;
                    if get_object_flags(&t).intersects(ObjectFlags::ContainsWideningType) {
                        if !self.report_widening_errors_in_type(&t)? {
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
                                    )?,
                                    self.type_to_string_(
                                        &*self.get_widened_type(&t)?,
                                        Option::<&Node>::None,
                                        None,
                                        None,
                                    )?,
                                ]),
                            );
                        }
                        error_reported = true;
                    }
                }
            }
        }
        Ok(error_reported)
    }

    pub(super) fn report_implicit_any(
        &self,
        declaration: &Node, /*Declaration*/
        type_: &Type,
        widening_kind: Option<WideningKind>,
    ) -> io::Result<()> {
        let type_as_string = self.type_to_string_(
            &*self.get_widened_type(type_)?,
            Option::<&Node>::None,
            None,
            None,
        )?;
        if is_in_js_file(Some(declaration))
            && !is_check_js_enabled_for_file(
                &get_source_file_of_node(declaration),
                &self.compiler_options,
            )
        {
            return Ok(());
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
                        .as_signature_declaration()
                        .parameters()
                        .into_iter()
                        .position(|parameter: &Gc<Node>| ptr::eq(param, &**parameter))
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
                                &*param_as_parameter_declaration
                                    .name()
                                    .as_identifier()
                                    .escaped_text,
                            ),
                            true,
                            None,
                        )?
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
                            .as_signature_declaration()
                            .parameters()
                            .into_iter()
                            .position(|parameter: &Gc<Node>| ptr::eq(param, &**parameter))
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
                        &*Diagnostics::Parameter_has_a_name_but_no_type_Did_you_mean_0_Colon_1,
                        Some(vec![new_name, type_name]),
                    );
                    return Ok(());
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
                    return Ok(());
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
                return Ok(());
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
                    return Ok(());
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
                return Ok(());
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
            diagnostic,
            Some(vec![
                declaration_name_to_string(get_name_of_declaration(Some(declaration))).into_owned(),
                type_as_string,
            ]),
        );

        Ok(())
    }

    pub(super) fn report_errors_from_widening(
        &self,
        declaration: &Node, /*Declaration*/
        type_: &Type,
        widening_kind: Option<WideningKind>,
    ) -> io::Result<()> {
        if self.produce_diagnostics
            && self.no_implicit_any
            && get_object_flags(type_).intersects(ObjectFlags::ContainsWideningType)
            && (widening_kind.is_none()
                || self
                    .get_contextual_signature_for_function_like_declaration(declaration)?
                    .is_none())
        {
            if !self.report_widening_errors_in_type(type_)? {
                self.report_implicit_any(declaration, type_, widening_kind)?;
            }
        }

        Ok(())
    }

    pub(super) fn apply_to_parameter_types(
        &self,
        source: &Signature,
        target: &Signature,
        mut callback: impl FnMut(&Type, &Type) -> io::Result<()>,
    ) -> io::Result<()> {
        let source_count = self.get_parameter_count(source)?;
        let target_count = self.get_parameter_count(target)?;
        let source_rest_type = self.get_effective_rest_type(source)?;
        let target_rest_type = self.get_effective_rest_type(target)?;
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
        let source_this_type = self.get_this_type_of_signature(source)?;
        if let Some(source_this_type) = source_this_type.as_ref() {
            let target_this_type = self.get_this_type_of_signature(target)?;
            if let Some(target_this_type) = target_this_type.as_ref() {
                callback(source_this_type, target_this_type)?;
            }
        }
        for i in 0..param_count {
            callback(
                &*self.get_type_at_position(source, i)?,
                &*self.get_type_at_position(target, i)?,
            )?;
        }
        if let Some(target_rest_type) = target_rest_type.as_ref() {
            callback(
                &*self.get_rest_type_at_position(source, param_count)?,
                target_rest_type,
            )?;
        }

        Ok(())
    }

    pub(super) fn apply_to_return_types(
        &self,
        source: Gc<Signature>,
        target: Gc<Signature>,
        mut callback: impl FnMut(&Type, &Type) -> io::Result<()>,
    ) -> io::Result<()> {
        let source_type_predicate = self.get_type_predicate_of_signature(&source)?;
        let target_type_predicate = self.get_type_predicate_of_signature(&target)?;
        let mut took_if_branch = false;
        if let Some(source_type_predicate) = source_type_predicate.as_ref() {
            if let Some(target_type_predicate) = target_type_predicate.as_ref() {
                if self.type_predicate_kinds_match(source_type_predicate, target_type_predicate) {
                    if let Some(source_type_predicate_type) = source_type_predicate.type_.as_ref() {
                        if let Some(target_type_predicate_type) =
                            target_type_predicate.type_.as_ref()
                        {
                            callback(source_type_predicate_type, target_type_predicate_type)?;
                            took_if_branch = true;
                        }
                    }
                }
            }
        }
        if !took_if_branch {
            callback(
                &*self.get_return_type_of_signature(source)?,
                &*self.get_return_type_of_signature(target)?,
            )?;
        }

        Ok(())
    }

    pub(super) fn create_inference_context(
        &self,
        type_parameters: &[Gc<Type /*TypeParameter*/>],
        signature: Option<Gc<Signature>>,
        flags: InferenceFlags,
        compare_types: Option<Gc<Box<dyn TypeComparer>>>,
    ) -> Gc<InferenceContext> {
        self.create_inference_context_worker(
            type_parameters
                .into_iter()
                .map(|type_parameter: &Gc<Type>| {
                    Gc::new(self.create_inference_info(type_parameter))
                })
                .collect(),
            signature,
            flags,
            compare_types.unwrap_or_else(|| {
                Gc::new(Box::new(TypeComparerCompareTypesAssignable::new(
                    self.rc_wrapper(),
                )))
            }),
        )
    }

    pub(super) fn clone_inference_context(
        &self,
        context: Option<&InferenceContext>,
        extra_flags: Option<InferenceFlags>,
    ) -> Option<Gc<InferenceContext>> {
        let extra_flags = extra_flags.unwrap_or(InferenceFlags::None);
        context.map(|context| {
            self.create_inference_context_worker(
                map(
                    &*context.inferences(),
                    |inference: &Gc<InferenceInfo>, _| {
                        Gc::new(self.clone_inference_info(inference))
                    },
                ),
                context.signature.clone(),
                context.flags() | extra_flags,
                context.compare_types.clone(),
            )
        })
    }

    pub(super) fn create_inference_context_worker(
        &self,
        inferences: Vec<Gc<InferenceInfo>>,
        signature: Option<Gc<Signature>>,
        flags: InferenceFlags,
        compare_types: Gc<Box<dyn TypeComparer>>,
    ) -> Gc<InferenceContext> {
        let context = Gc::new(InferenceContext::new(
            inferences,
            signature,
            flags,
            compare_types,
            None,
            None,
            None,
            None,
        ));
        context.set_mapper(Gc::new(self.make_function_type_mapper(
            CreateInferenceContextWorkerMapperCallback::new(context.clone()),
        )));
        context.set_non_fixing_mapper(Gc::new(self.make_function_type_mapper(
            CreateInferenceContextWorkerNonFixingMapperCallback::new(context.clone()),
        )));
        context
    }

    pub(super) fn map_to_inferred_type(
        &self,
        context: &InferenceContext,
        t: &Type,
        fix: bool,
    ) -> io::Result<Gc<Type>> {
        let inferences = context.inferences();
        let inferences = &*inferences;
        for (i, inference) in inferences.into_iter().enumerate() {
            if ptr::eq(t, &*inference.type_parameter) {
                if fix && !inference.is_fixed() {
                    self.clear_cached_inferences(inferences);
                    inference.set_is_fixed(true);
                }
                return self.get_inferred_type(context, i);
            }
        }
        Ok(t.type_wrapper())
    }

    pub(super) fn clear_cached_inferences(&self, inferences: &[Gc<InferenceInfo>]) {
        for inference in inferences {
            if !inference.is_fixed() {
                *inference.maybe_inferred_type_mut() = None;
            }
        }
    }

    pub(super) fn create_inference_info(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> InferenceInfo {
        InferenceInfo::new(
            type_parameter.type_wrapper(),
            None,
            None,
            None,
            None,
            true,
            false,
            None,
        )
    }

    pub(super) fn clone_inference_info(&self, inference: &InferenceInfo) -> InferenceInfo {
        InferenceInfo::new(
            inference.type_parameter.clone(),
            inference.maybe_candidates().clone(),
            inference.maybe_contra_candidates().clone(),
            inference.maybe_inferred_type(),
            inference.maybe_priority(),
            inference.top_level(),
            inference.is_fixed(),
            inference.maybe_implied_arity(),
        )
    }

    pub(super) fn clone_inferred_part_of_context(
        &self,
        context: &InferenceContext,
    ) -> Option<Gc<InferenceContext>> {
        let inferences = filter(&context.inferences(), |inference: &Gc<InferenceInfo>| {
            self.has_inference_candidates(inference)
        });
        if !inferences.is_empty() {
            Some(self.create_inference_context_worker(
                map(&inferences, |inference: &Gc<InferenceInfo>, _| {
                    Gc::new(self.clone_inference_info(inference))
                }),
                context.signature.clone(),
                context.flags(),
                context.compare_types.clone(),
            ))
        } else {
            None
        }
    }

    pub(super) fn get_mapper_from_context(
        &self,
        context: Option<&InferenceContext>,
    ) -> Option<Gc<TypeMapper>> {
        context.map(|context| context.mapper().clone())
    }

    pub(super) fn could_contain_type_variables(&self, type_: &Type) -> io::Result<bool> {
        let object_flags = get_object_flags(&type_);
        if object_flags.intersects(ObjectFlags::CouldContainTypeVariablesComputed) {
            return Ok(object_flags.intersects(ObjectFlags::CouldContainTypeVariables));
        }
        let result = type_.flags().intersects(TypeFlags::Instantiable)
            || type_.flags().intersects(TypeFlags::Object)
                && !self.is_non_generic_top_level_type(type_)
                && (object_flags.intersects(ObjectFlags::Reference)
                    && (type_.as_type_reference_interface().maybe_node().is_some()
                        || try_for_each_bool(
                            &self.get_type_arguments(type_)?,
                            |type_argument: &Gc<Type>, _| {
                                self.could_contain_type_variables(type_argument)
                            },
                        )?)
                    || object_flags.intersects(ObjectFlags::Anonymous)
                        && matches!(
                            type_.maybe_symbol().as_ref(),
                            Some(type_symbol) if type_symbol.flags().intersects(SymbolFlags::Function | SymbolFlags::Method | SymbolFlags::Class | SymbolFlags::TypeLiteral | SymbolFlags::ObjectLiteral) &&
                                type_symbol.maybe_declarations().is_some()
                        )
                    || object_flags.intersects(
                        ObjectFlags::Mapped
                            | ObjectFlags::ReverseMapped
                            | ObjectFlags::ObjectRestType,
                    ))
            || type_.flags().intersects(TypeFlags::UnionOrIntersection)
                && !type_.flags().intersects(TypeFlags::EnumLiteral)
                && !self.is_non_generic_top_level_type(type_)
                && try_some(
                    Some(type_.as_union_or_intersection_type_interface().types()),
                    Some(|type_: &Gc<Type>| self.could_contain_type_variables(type_)),
                )?;
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
        Ok(result)
    }

    pub(super) fn is_non_generic_top_level_type(&self, type_: &Type) -> bool {
        if let Some(ref type_alias_symbol) = type_.maybe_alias_symbol() {
            if type_.maybe_alias_type_arguments().is_none() {
                let declaration =
                    get_declaration_of_kind(type_alias_symbol, SyntaxKind::TypeAliasDeclaration);
                return matches!(
                    declaration.as_ref(),
                    Some(declaration) if find_ancestor(
                        declaration.maybe_parent(),
                        |n: &Node| {
                            if n.kind() == SyntaxKind::SourceFile {
                                FindAncestorCallbackReturn::Bool(true)
                            } else if n.kind() == SyntaxKind::ModuleDeclaration {
                                FindAncestorCallbackReturn::Bool(false)
                            } else {
                                FindAncestorCallbackReturn::Quit
                            }
                        }
                    ).is_some()
                );
            }
        }
        false
    }

    pub(super) fn is_type_parameter_at_top_level(
        &self,
        type_: &Type,
        type_parameter: &Type, /*TypeParameter*/
    ) -> io::Result<bool> {
        Ok(ptr::eq(type_, type_parameter)
            || type_.flags().intersects(TypeFlags::UnionOrIntersection)
                && try_some(
                    Some(type_.as_union_or_intersection_type_interface().types()),
                    Some(|t: &Gc<Type>| self.is_type_parameter_at_top_level(t, type_parameter)),
                )?
            || type_.flags().intersects(TypeFlags::Conditional)
                && (ptr::eq(
                    &*self.get_true_type_from_conditional_type(type_)?,
                    type_parameter,
                ) || ptr::eq(
                    &*self.get_false_type_from_conditional_type(type_)?,
                    type_parameter,
                )))
    }

    pub(super) fn create_empty_object_type_from_string_literal(
        &self,
        type_: &Type,
    ) -> io::Result<Gc<Type>> {
        let mut members = create_symbol_table(Option::<&[Gc<Symbol>]>::None);
        self.for_each_type(type_, |t: &Type| -> Option<()> {
            if !t.flags().intersects(TypeFlags::StringLiteral) {
                return None;
            }
            let name = escape_leading_underscores(&t.as_string_literal_type().value).into_owned();
            let literal_prop: Gc<Symbol> = self
                .create_symbol(SymbolFlags::Property, name.clone(), None)
                .into();
            literal_prop
                .as_transient_symbol()
                .symbol_links()
                .borrow_mut()
                .type_ = Some(self.any_type());
            if let Some(ref t_symbol) = t.maybe_symbol() {
                if let Some(t_symbol_declarations) = t_symbol.maybe_declarations().clone() {
                    literal_prop.set_declarations(t_symbol_declarations);
                }
                if let Some(t_symbol_value_declaration) = t_symbol.maybe_value_declaration() {
                    literal_prop.set_value_declaration(t_symbol_value_declaration);
                }
            }
            members.insert(name, literal_prop);
            None
        });
        let index_infos = if type_.flags().intersects(TypeFlags::String) {
            vec![Gc::new(self.create_index_info(
                self.string_type(),
                self.empty_object_type(),
                false,
                None,
            ))]
        } else {
            vec![]
        };
        self.create_anonymous_type(
            Option::<&Symbol>::None,
            Gc::new(GcCell::new(members)),
            vec![],
            vec![],
            index_infos,
        )
    }

    pub(super) fn infer_type_for_homomorphic_mapped_type(
        &self,
        source: &Type,
        target: &Type,     /*MappedType*/
        constraint: &Type, /*IndexType*/
    ) -> io::Result<Option<Gc<Type>>> {
        if self.in_infer_type_for_homomorphic_mapped_type() {
            return Ok(None);
        }
        let key = format!("{},{},{}", source.id(), target.id(), constraint.id());
        if self.reverse_mapped_cache().contains_key(&key) {
            return Ok(self
                .reverse_mapped_cache()
                .get(&key)
                .map(Clone::clone)
                .unwrap());
        }
        self.set_in_infer_type_for_homomorphic_mapped_type(true);
        let type_ = self.create_reverse_mapped_type(source, target, constraint)?;
        self.set_in_infer_type_for_homomorphic_mapped_type(false);
        self.reverse_mapped_cache().insert(key, type_.clone());
        Ok(type_)
    }

    pub(super) fn is_partially_inferable_type(&self, type_: &Type) -> io::Result<bool> {
        Ok(
            !get_object_flags(type_).intersects(ObjectFlags::NonInferrableType)
                || self.is_object_literal_type(type_)
                    && self
                        .get_properties_of_type(type_)?
                        .try_any(|ref prop: Gc<Symbol>| {
                            self.is_partially_inferable_type(&*self.get_type_of_symbol(prop)?)
                        })?
                || self.is_tuple_type(type_)
                    && try_some(
                        Some(&self.get_type_arguments(type_)?),
                        Some(|type_argument: &Gc<Type>| {
                            self.is_partially_inferable_type(type_argument)
                        }),
                    )?,
        )
    }

    pub(super) fn create_reverse_mapped_type(
        &self,
        source: &Type,
        target: &Type,     /*MappedType*/
        constraint: &Type, /*IndexType*/
    ) -> io::Result<Option<Gc<Type>>> {
        if !(self
            .get_index_info_of_type_(source, &self.string_type())?
            .is_some()
            || self.get_properties_of_type(source)?.len() != 0
                && self.is_partially_inferable_type(source)?)
        {
            return Ok(None);
        }
        if self.is_array_type(source) {
            return Ok(Some(self.create_array_type(
                &*self.infer_reverse_mapped_type(
                    &self.get_type_arguments(source)?[0],
                    target,
                    constraint,
                )?,
                Some(self.is_readonly_array_type(source)),
            )));
        }
        if self.is_tuple_type(source) {
            let element_types = try_map(&self.get_type_arguments(source)?, |t: &Gc<Type>, _| {
                self.infer_reverse_mapped_type(t, target, constraint)
            })?;
            let element_flags = if self
                .get_mapped_type_modifiers(target)
                .intersects(MappedTypeModifiers::IncludeOptional)
            {
                same_map(
                    &source
                        .as_type_reference()
                        .target
                        .as_tuple_type()
                        .element_flags,
                    |f: &ElementFlags, _| {
                        if f.intersects(ElementFlags::Optional) {
                            ElementFlags::Required
                        } else {
                            *f
                        }
                    },
                )
            } else {
                source
                    .as_type_reference()
                    .target
                    .as_tuple_type()
                    .element_flags
                    .clone()
            };
            return Ok(Some(
                self.create_tuple_type(
                    &element_types,
                    Some(&element_flags),
                    Some(source.as_type_reference().target.as_tuple_type().readonly),
                    source
                        .as_type_reference()
                        .target
                        .as_tuple_type()
                        .labeled_element_declarations
                        .as_deref(),
                )?,
            ));
        }
        let reversed = self.create_object_type(
            ObjectFlags::ReverseMapped | ObjectFlags::Anonymous,
            Option::<&Symbol>::None,
        );
        Ok(Some(
            ReverseMappedType::new(
                reversed,
                source.type_wrapper(),
                target.type_wrapper(),
                constraint.type_wrapper(),
            )
            .into(),
        ))
    }

    pub(super) fn get_type_of_reverse_mapped_symbol(
        &self,
        symbol: &Symbol, /*ReverseMappedSymbol*/
    ) -> io::Result<Gc<Type>> {
        let links = self.get_symbol_links(symbol);
        if (*links).borrow().type_.is_none() {
            let symbol_as_reverse_mapped_symbol = symbol.as_reverse_mapped_symbol();
            links.borrow_mut().type_ = Some(self.infer_reverse_mapped_type(
                &symbol_as_reverse_mapped_symbol.property_type,
                &symbol_as_reverse_mapped_symbol.mapped_type,
                &symbol_as_reverse_mapped_symbol.constraint_type,
            )?);
        }
        let ret = (*links).borrow().type_.clone().unwrap();
        Ok(ret)
    }
}

#[derive(Trace, Finalize)]
pub(super) struct CreateInferenceContextWorkerMapperCallback {
    inference_context: Gc<InferenceContext>,
}

impl CreateInferenceContextWorkerMapperCallback {
    pub fn new(inference_context: Gc<InferenceContext>) -> Self {
        Self { inference_context }
    }
}

impl TypeMapperCallback for CreateInferenceContextWorkerMapperCallback {
    fn call(&self, checker: &TypeChecker, t: &Type) -> io::Result<Gc<Type>> {
        checker.map_to_inferred_type(&self.inference_context, t, true)
    }
}

#[derive(Trace, Finalize)]
pub(super) struct CreateInferenceContextWorkerNonFixingMapperCallback {
    inference_context: Gc<InferenceContext>,
}

impl CreateInferenceContextWorkerNonFixingMapperCallback {
    pub fn new(inference_context: Gc<InferenceContext>) -> Self {
        Self { inference_context }
    }
}

impl TypeMapperCallback for CreateInferenceContextWorkerNonFixingMapperCallback {
    fn call(&self, checker: &TypeChecker, t: &Type) -> io::Result<Gc<Type>> {
        checker.map_to_inferred_type(&self.inference_context, t, false)
    }
}

#[derive(Trace, Finalize)]
pub(super) struct TypeComparerCompareTypesAssignable {
    type_checker: Gc<TypeChecker>,
}

impl TypeComparerCompareTypesAssignable {
    pub fn new(type_checker: Gc<TypeChecker>) -> Self {
        Self { type_checker }
    }
}

impl TypeComparer for TypeComparerCompareTypesAssignable {
    fn call(&self, s: &Type, t: &Type, _report_errors: Option<bool>) -> io::Result<Ternary> {
        self.type_checker.compare_types_assignable(s, t)
    }
}
