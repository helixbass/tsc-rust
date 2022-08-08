#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{TypeFacts, WideningKind};
use crate::{
    create_symbol_table, same_map, some, IndexInfo, SymbolInterface, __String, get_object_flags,
    is_write_only_access, node_is_missing, DiagnosticMessage, Diagnostics, InferenceContext,
    InferenceFlags, InferenceInfo, InferencePriority, Node, NodeInterface, ObjectFlags, Signature,
    Symbol, SymbolFlags, Ternary, Type, TypeChecker, TypeFlags, TypeInterface, UnionReduction,
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
        unimplemented!()
    }

    pub(super) fn report_implicit_any(
        &self,
        declaration: &Node, /*Declaration*/
        type_: &Type,
        widening_kind: Option<WideningKind>,
    ) {
        unimplemented!()
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

    pub(super) fn create_inference_context<
        TSignature: Borrow<Signature>,
        TCompareTypes: FnMut(&Type, &Type, Option<bool>) -> Ternary,
    >(
        &self,
        type_parameters: &[Rc<Type /*TypeParameter*/>],
        signature: Option<TSignature>,
        flags: InferenceFlags,
        compare_types: Option<TCompareTypes>,
    ) -> InferenceContext {
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
