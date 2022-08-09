#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use super::TypeFacts;
use crate::{
    get_check_flags, get_object_flags, is_write_only_access, node_is_missing, some, CheckFlags,
    DiagnosticMessage, Diagnostics, ElementFlags, InferenceContext, InferenceInfo,
    InferencePriority, Node, NodeInterface, ObjectFlags, Symbol, SymbolFlags, SymbolInterface,
    Type, TypeChecker, TypeFlags, TypeInterface, UnionReduction,
};

impl TypeChecker {
    pub(super) fn infer_reverse_mapped_type(
        &self,
        source_type: &Type,
        target: &Type,     /*MappedType*/
        constraint: &Type, /*IndexType*/
    ) -> Rc<Type> {
        let type_parameter = self.get_indexed_access_type(
            &constraint.as_index_type().type_,
            &self.get_type_parameter_from_mapped_type(target),
            None,
            Option::<&Node>::None,
            Option::<&Symbol>::None,
            None,
        );
        let template_type = self.get_template_type_from_mapped_type(target);
        let inference = Rc::new(self.create_inference_info(&type_parameter));
        self.infer_types(
            &vec![inference.clone()],
            source_type,
            &template_type,
            None,
            None,
        );
        self.get_type_from_inference(&inference)
            .unwrap_or_else(|| self.unknown_type())
    }

    pub(super) fn get_unmatched_properties(
        &self,
        source: &Type,
        target: &Type,
        require_optional_properties: bool,
        match_discriminant_properties: bool,
    ) -> Vec<Rc<Symbol>> {
        let properties = self.get_properties_of_type(target);
        let mut ret = vec![];
        for target_prop in &properties {
            if self.is_static_private_identifier_property(target_prop) {
                continue;
            }
            if require_optional_properties
                || !(target_prop.flags().intersects(SymbolFlags::Optional)
                    || get_check_flags(target_prop).intersects(CheckFlags::Partial))
            {
                let source_prop =
                    self.get_property_of_type_(source, target_prop.escaped_name(), None);
                match source_prop {
                    None => {
                        ret.push(target_prop.clone());
                    }
                    Some(ref source_prop) => {
                        if match_discriminant_properties {
                            let target_type = self.get_type_of_symbol(target_prop);
                            if target_type.flags().intersects(TypeFlags::Unit) {
                                let source_type = self.get_type_of_symbol(source_prop);
                                if !(source_type.flags().intersects(TypeFlags::Any)
                                    || Rc::ptr_eq(
                                        &self.get_regular_type_of_literal_type(&source_type),
                                        &self.get_regular_type_of_literal_type(&target_type),
                                    ))
                                {
                                    ret.push(target_prop.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
        ret
    }

    pub(super) fn get_unmatched_property(
        &self,
        source: &Type,
        target: &Type,
        require_optional_properties: bool,
        match_discriminant_properties: bool,
    ) -> Option<Rc<Symbol>> {
        let result = self.get_unmatched_properties(
            source,
            target,
            require_optional_properties,
            match_discriminant_properties,
        );
        result.get(0).map(Clone::clone)
    }

    pub(super) fn tuple_types_definitely_unrelated(
        &self,
        source: &Type, /*TupleTypeReference*/
        target: &Type, /*TupleTypeReference*/
    ) -> bool {
        let source_target_as_tuple_type = source.as_type_reference().target.as_tuple_type();
        let target_target_as_tuple_type = target.as_type_reference().target.as_tuple_type();
        !target_target_as_tuple_type
            .combined_flags
            .intersects(ElementFlags::Variadic)
            && target_target_as_tuple_type.min_length > source_target_as_tuple_type.min_length
            || !target_target_as_tuple_type.has_rest_element
                && (source_target_as_tuple_type.has_rest_element
                    || target_target_as_tuple_type.fixed_length
                        < source_target_as_tuple_type.fixed_length)
    }

    pub(super) fn types_definitely_unrelated(&self, source: &Type, target: &Type) -> bool {
        if self.is_tuple_type(source) && self.is_tuple_type(target) {
            self.tuple_types_definitely_unrelated(source, target)
        } else {
            self.get_unmatched_property(source, target, false, true)
                .is_some()
                && self
                    .get_unmatched_property(target, source, false, false)
                    .is_some()
        }
    }

    pub(super) fn get_type_from_inference(&self, inference: &InferenceInfo) -> Option<Rc<Type>> {
        if let Some(inference_candidates) = inference.candidates.as_ref() {
            Some(self.get_union_type(
                inference_candidates.clone(),
                Some(UnionReduction::Subtype),
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            ))
        } else if let Some(inference_contra_candidates) = inference.contra_candidates.as_ref() {
            Some(self.get_intersection_type(
                inference_contra_candidates,
                Option::<&Symbol>::None,
                None,
            ))
        } else {
            None
        }
    }

    pub(super) fn has_skip_direct_inference_flag(&self, node: &Node) -> bool {
        (*self.get_node_links(node)).borrow().skip_direct_inference == Some(true)
    }

    pub(super) fn is_from_inference_blocked_source(&self, type_: &Type) -> bool {
        matches!(
            type_.maybe_symbol().as_ref(),
            Some(type_symbol) if some(
                type_symbol.maybe_declarations().as_deref(),
                Some(|declaration: &Rc<Node>| self.has_skip_direct_inference_flag(declaration))
            )
        )
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
