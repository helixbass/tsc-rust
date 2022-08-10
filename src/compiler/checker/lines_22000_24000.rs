#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cmp;
use std::ptr;
use std::rc::Rc;

use super::{InferTypes, TypeFacts};
use crate::{
    find, flat_map, get_object_flags, is_write_only_access, node_is_missing, DiagnosticMessage,
    Diagnostics, InferenceContext, InferenceInfo, InferencePriority, Node, NodeInterface,
    ObjectFlags, Symbol, SymbolFlags, Type, TypeChecker, TypeFlags, TypeInterface, UnionReduction,
};

impl InferTypes {
    pub(super) fn get_inference_info_for_type(&self, type_: &Type) -> Option<Rc<InferenceInfo>> {
        if type_.flags().intersects(TypeFlags::TypeVariable) {
            for inference in &self.inferences {
                if ptr::eq(type_, &*inference.type_parameter) {
                    return Some(inference.clone());
                }
            }
        }
        None
    }

    pub(super) fn get_single_type_variable_from_intersection_types(
        &self,
        types: &[Rc<Type>],
    ) -> Option<Rc<Type>> {
        let mut type_variable: Option<Rc<Type>> = None;
        for type_ in types {
            let t = if type_.flags().intersects(TypeFlags::Intersection) {
                find(
                    type_.as_union_or_intersection_type_interface().types(),
                    |t: &Rc<Type>, _| self.get_inference_info_for_type(t).is_some(),
                )
                .map(Clone::clone)
            } else {
                None
            };
            if t.is_none() {
                return None;
            }
            let t = t.unwrap();
            if matches!(
                type_variable.as_ref(),
                Some(type_variable) if !Rc::ptr_eq(&t, type_variable)
            ) {
                return None;
            }
            type_variable = Some(t);
        }
        type_variable
    }

    pub(super) fn infer_to_multiple_types(
        &self,
        source: &Type,
        targets: &[Rc<Type>],
        target_flags: TypeFlags,
    ) {
        let mut type_variable_count = 0;
        if target_flags.intersects(TypeFlags::Union) {
            let mut naked_type_variable: Option<Rc<Type>> = None;
            let sources = if source.flags().intersects(TypeFlags::Union) {
                source
                    .as_union_or_intersection_type_interface()
                    .types()
                    .to_owned()
            } else {
                vec![source.type_wrapper()]
            };
            let mut matched = vec![false; sources.len()];
            let mut inference_circularity = false;
            for t in targets {
                if self.get_inference_info_for_type(t).is_some() {
                    naked_type_variable = Some(t.clone());
                    type_variable_count += 1;
                } else {
                    for i in 0..sources.len() {
                        let save_inference_priority = self.inference_priority();
                        self.set_inference_priority(InferencePriority::MaxValue);
                        self.infer_from_types(&sources[i], t);
                        if self.inference_priority() == self.priority() {
                            matched[i] = true;
                        }
                        inference_circularity = inference_circularity
                            || self.inference_priority() == InferencePriority::Circularity;
                        self.set_inference_priority(cmp::min(
                            self.inference_priority(),
                            save_inference_priority,
                        ));
                    }
                }
            }
            if type_variable_count == 0 {
                let intersection_type_variable =
                    self.get_single_type_variable_from_intersection_types(targets);
                if let Some(intersection_type_variable) = intersection_type_variable.as_ref() {
                    self.infer_with_priority(
                        source,
                        intersection_type_variable,
                        InferencePriority::NakedTypeVariable,
                    );
                }
                return;
            }
            if type_variable_count == 1 && !inference_circularity {
                let unmatched = flat_map(Some(&sources), |s: &Rc<Type>, i| {
                    if matched[i] {
                        vec![]
                    } else {
                        vec![s.clone()]
                    }
                });
                if !unmatched.is_empty() {
                    self.infer_from_types(
                        &self.type_checker.get_union_type(
                            unmatched,
                            None,
                            Option::<&Symbol>::None,
                            None,
                            Option::<&Type>::None,
                        ),
                        naked_type_variable.as_deref().unwrap(),
                    );
                    return;
                }
            }
        } else {
            for t in targets {
                if self.get_inference_info_for_type(t).is_some() {
                    type_variable_count += 1;
                } else {
                    self.infer_from_types(source, t);
                }
            }
        }
        if if target_flags.intersects(TypeFlags::Intersection) {
            type_variable_count == 1
        } else {
            type_variable_count > 0
        } {
            for t in targets {
                if self.get_inference_info_for_type(t).is_some() {
                    self.infer_with_priority(source, t, InferencePriority::NakedTypeVariable);
                }
            }
        }
    }

    pub(super) fn infer_to_conditional_type(
        &self,
        source: &Type,
        target: &Type, /*ConditionalType*/
    ) {
        unimplemented!()
    }

    pub(super) fn infer_to_template_literal_type(
        &self,
        source: &Type,
        target: &Type, /*TemplateLiteralType*/
    ) {
        unimplemented!()
    }

    pub(super) fn infer_from_object_types(&self, source: &Type, target: &Type) {
        unimplemented!()
    }
}

impl TypeChecker {
    pub(super) fn is_type_or_base_identical_to(&self, s: &Type, t: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_type_closely_matched_by(&self, s: &Type, t: &Type) -> bool {
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
