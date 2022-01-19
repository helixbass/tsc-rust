#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::WideningKind;
use crate::{
    every, for_each, get_object_flags, is_write_only_access, length, node_is_missing, Debug_,
    DiagnosticMessage, Diagnostics, Identifier, Node, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, Signature, Symbol, SymbolFlags, Ternary, Type, TypeChecker,
    TypeFlags, TypeInterface, TypePredicate, UnionReduction,
};

impl TypeChecker {
    pub(super) fn type_could_have_top_level_singleton_types(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Boolean) {
            return false;
        }

        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            return for_each(
                type_.as_union_or_intersection_type_interface().types(),
                |type_, _| {
                    if self.type_could_have_top_level_singleton_types(type_) {
                        Some(())
                    } else {
                        None
                    }
                },
            )
            .is_some();
        }

        if type_.flags().intersects(TypeFlags::Instantiable) {
            unimplemented!()
        }

        self.is_unit_type(type_) || type_.flags().intersects(TypeFlags::TemplateLiteral)
    }

    pub(super) fn is_matching_signature(
        &self,
        source: &Signature,
        target: &Signature,
        partial_match: bool,
    ) -> bool {
        let source_parameter_count = self.get_parameter_count(source);
        let target_parameter_count = self.get_parameter_count(target);
        let source_min_argument_count = self.get_min_argument_count(source, None);
        let target_min_argument_count = self.get_min_argument_count(target, None);
        let source_has_rest_parameter = self.has_effective_rest_parameter(source);
        let target_has_rest_parameter = self.has_effective_rest_parameter(target);
        if source_parameter_count == target_parameter_count
            && source_min_argument_count == target_min_argument_count
            && source_has_rest_parameter == target_has_rest_parameter
        {
            return true;
        }
        if partial_match && source_min_argument_count <= target_min_argument_count {
            return true;
        }
        false
    }

    pub(super) fn compare_signature_identical<TCompareTypes: FnMut(&Type, &Type) -> Ternary>(
        &self,
        mut source: Rc<Signature>,
        target: &Signature,
        partial_match: bool,
        ignore_this_types: bool,
        ignore_return_types: bool,
        compare_types: TCompareTypes,
    ) -> Ternary {
        if ptr::eq(&*source, target) {
            return Ternary::True;
        }
        if !self.is_matching_signature(source, target, partial_match) {
            return Ternary::False;
        }
        if length(source.type_parameters) != length(target.type_parameters) {
            return Ternary::False;
        }
        if let Some(target_type_parameters) = target.type_parameters {
            let source_type_parameters = source.type_parameters.as_ref().unwrap();
            let mapper = self.create_type_mapper(
                source_type_parameters.clone(),
                Some(target_type_parameters.clone()),
            );
            for (i, t) in target_type_parameters.iter().enumerate() {
                let s = &source_type_parameters[i];
                if !(Rc::ptr_eq(s, t)
                    || compare_types(
                        self.instantiate_type(
                            self.get_constraint_from_type_parameter(s),
                            Some(&mapper),
                        )
                        .unwrap_or_else(|| self.unknown_type()),
                        self.get_constraint_from_type_parameter(t)
                            .unwrap_or_else(|| self.unknown_type()),
                    ) != Ternary::False
                        && compare_types(
                            self.instantiate_type(
                                self.get_default_from_type_parameter(s),
                                Some(&mapper),
                            )
                            .unwrap_or_else(|| self.unknown_type()),
                            self.get_default_from_type_parameter(t)
                                .unwrap_or_else(|| self.unknown_type()),
                        ) != Ternary::False)
                {
                    return Ternary::False;
                }
            }
            source = self.instantiate_signature(source, &mapper, Some(true));
        }
        let mut result = Ternary::True;
        if !ignore_this_types {
            let source_this_type = self.get_this_type_of_signature(source);
            if let Some(source_this_type) = source_this_type {
                let target_this_type = self.get_this_type_of_signature(target);
                if let Some(target_this_type) = target_this_type {
                    let related = compare_types(source_this_type, target_this_type);
                    if related == Ternary::False {
                        return Ternary::False;
                    }
                    result &= related;
                }
            }
        }
        let target_len = self.get_parameter_count(target);
        for i in 0..target_len {
            let s = self.get_type_at_position(source, i);
            let t = self.get_type_at_position(target, i);
            let related = compare_types(t, s);
            if related == Ternary::False {
                return Ternary::False;
            }
            result &= related;
        }
        if !ignore_return_types {
            let source_type_predicate = self.get_type_predicate_of_signature(source);
            let target_type_predicate = self.get_type_predicate_of_signature(target);
            result &= if source_type_predicate.is_some() || target_type_predicate.is_some() {
                self.compare_type_predicates_identical(
                    source_type_predicate,
                    target_type_predicate,
                    |s, t| compare_types(s, t),
                )
            } else {
                compare_types(
                    self.get_return_type_of_signature(&source),
                    self.get_return_type_of_signature(target),
                )
            };
        }
        result
    }

    pub(super) fn compare_type_predicates_identical<
        TCompareTypes: FnOnce(&Type, &Type) -> Ternary,
    >(
        &self,
        source: Option<TypePredicate>,
        target: Option<TypePredicate>,
        compare_types: TCompareTypes,
    ) -> Ternary {
        unimplemented!()
        // match (source, target) {
        //     (Some(source), Some(target) => {
        //         if !self.type_predicate_kinds_match(source, target) {
        //             Ternary::False
        //         } else
        //     }
        //     _ => Ternary::False,
        // }
    }

    pub(super) fn is_unit_type(&self, type_: &Type) -> bool {
        type_.flags().intersects(TypeFlags::Unit)
    }

    pub(super) fn is_literal_type(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Boolean) {
            true
        } else if type_.flags().intersects(TypeFlags::Union) {
            if type_.flags().intersects(TypeFlags::EnumLiteral) {
                true
            } else {
                every(
                    type_.as_union_or_intersection_type_interface().types(),
                    |type_, _| self.is_unit_type(&**type_),
                )
            }
        } else {
            self.is_unit_type(type_)
        }
    }

    pub(super) fn get_base_type_of_literal_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::EnumLiteral) {
            unimplemented!()
        } else if type_.flags().intersects(TypeFlags::StringLiteral) {
            unimplemented!()
        } else if type_.flags().intersects(TypeFlags::NumberLiteral) {
            self.number_type()
        } else if type_.flags().intersects(TypeFlags::BigIntLiteral) {
            self.bigint_type()
        } else if type_.flags().intersects(TypeFlags::BooleanLiteral) {
            self.boolean_type()
        } else if type_.flags().intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_| Some(self.get_base_type_of_literal_type(type_)),
                None,
            )
            .unwrap()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_widened_literal_type(&self, type_: &Type) -> Rc<Type> {
        let flags = type_.flags();
        if flags.intersects(TypeFlags::EnumLiteral) && self.is_fresh_literal_type(type_) {
            unimplemented!()
        } else if flags.intersects(TypeFlags::StringLiteral) && self.is_fresh_literal_type(type_) {
            unimplemented!()
        } else if flags.intersects(TypeFlags::NumberLiteral) && self.is_fresh_literal_type(type_) {
            self.number_type()
        } else if flags.intersects(TypeFlags::BigIntLiteral) && self.is_fresh_literal_type(type_) {
            self.bigint_type()
        } else if flags.intersects(TypeFlags::BooleanLiteral) && self.is_fresh_literal_type(type_) {
            self.boolean_type()
        } else if flags.intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_| Some(self.get_widened_literal_type(type_)),
                None,
            )
            .unwrap()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_widened_unique_es_symbol_type(&self, type_: &Type) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn get_widened_literal_like_type_for_contextual_type<TTypeRef: Borrow<Type>>(
        &self,
        type_: &Type,
        contextual_type: Option<TTypeRef>,
    ) -> Rc<Type> {
        let mut type_ = type_.type_wrapper();
        if !self.is_literal_of_contextual_type(&type_, contextual_type) {
            type_ = self.get_widened_unique_es_symbol_type(&self.get_widened_literal_type(&type_));
        }
        type_
    }

    pub(super) fn is_tuple_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::Reference)
            && type_
                .as_type_reference()
                .target
                .as_object_type()
                .object_flags()
                .intersects(ObjectFlags::Tuple)
    }

    pub(super) fn get_optional_type(&self, type_: &Type, is_property: Option<bool>) -> Rc<Type> {
        let is_property = is_property.unwrap_or(false);
        Debug_.assert(self.strict_null_checks, None);
        if type_.flags().intersects(TypeFlags::Undefined) {
            type_.type_wrapper()
        } else {
            unimplemented!()
        }
    }

    pub(super) fn remove_missing_type(&self, type_: &Type, is_optional: bool) -> Rc<Type> {
        if self.exact_optional_property_types && is_optional {
            unimplemented!()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_regular_type_of_object_literal(&self, type_: &Type) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn get_widened_type(&self, type_: &Type) -> Rc<Type> {
        self.get_widened_type_with_context(type_)
    }

    pub(super) fn get_widened_type_with_context(&self, type_: &Type) -> Rc<Type> {
        type_.type_wrapper()
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

    pub(super) fn is_object_literal_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::ObjectLiteral)
    }

    pub(super) fn get_cannot_find_name_diagnostic_for_name(
        &self,
        node: &Node,
    ) -> DiagnosticMessage {
        match node.as_identifier().escaped_text {
            _ => {
                if false {
                    unimplemented!()
                } else {
                    Diagnostics::Cannot_find_name_0
                }
            }
        }
    }

    pub(super) fn get_resolved_symbol(&self, node: &Identifier) -> Rc<Symbol> {
        let links = self.get_node_links(node);
        let mut links_ref = links.borrow_mut();
        if links_ref.resolved_symbol.is_none() {
            links_ref.resolved_symbol = Some(if !node_is_missing(Some(node.node_wrapper())) {
                self.resolve_name(
                    Some(node),
                    &node.escaped_text,
                    SymbolFlags::Value | SymbolFlags::ExportValue,
                    Some(self.get_cannot_find_name_diagnostic_for_name(&*node.node_wrapper())),
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

    pub(super) fn filter_type(&self, type_: &Type, f: fn(&TypeChecker, &Type) -> bool) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union) {
            unimplemented!()
        }
        if type_.flags().intersects(TypeFlags::Never) || f(self, type_) {
            type_.type_wrapper()
        } else {
            self.never_type()
        }
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
                ))
            } else {
                None
            }
        } else {
            Some(type_.type_wrapper())
        }
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
}
