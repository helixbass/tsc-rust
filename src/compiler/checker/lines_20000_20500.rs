#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use super::{CheckTypeContainingMessageChain, CheckTypeRelatedTo, IntersectionState};
use crate::{
    for_each, IndexInfo, RelationComparisonResult, Signature, Symbol, Ternary, Type, TypeChecker,
    TypeFlags, TypeInterface, VarianceFlags,
};

impl<'type_checker, TContainingMessageChain: CheckTypeContainingMessageChain>
    CheckTypeRelatedTo<'type_checker, TContainingMessageChain>
{
    pub(super) fn index_info_related_to(
        &self,
        source_info: &IndexInfo,
        target_info: &IndexInfo,
        report_errors: bool,
    ) -> Ternary {
        unimplemented!()
    }

    pub(super) fn index_signatures_related_to(
        &self,
        source: &Type,
        target: &Type,
        source_is_primitive: bool,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        unimplemented!()
    }

    pub(super) fn constructor_visibilities_are_compatible(
        &self,
        source_signature: &Signature,
        target_signature: &Signature,
        report_errors: bool,
    ) -> bool {
        unimplemented!()
    }
}

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

    pub(super) fn get_exact_optional_unassignable_properties(
        &self,
        source: &Type,
        target: &Type,
    ) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn is_exact_optional_property_mismatch<
        TSource: Borrow<Type>,
        TTarget: Borrow<Type>,
    >(
        &self,
        source: Option<TSource>,
        target: Option<TTarget>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_best_matching_type<TIsRelatedTo: Fn(&Type, &Type) -> Ternary>(
        &self,
        source: &Type,
        target: &Type, /*UnionOrIntersectionType*/
        is_related_to: Option<TIsRelatedTo>,
    ) -> Option<Rc<Type>> {
        let is_related_to: Box<dyn Fn(&Type, &Type) -> Ternary> = is_related_to.map_or_else(
            || {
                Box::new(|source: &Type, target: &Type| {
                    self.compare_types_assignable(source, target)
                }) as Box<dyn Fn(&Type, &Type) -> Ternary>
            },
            |is_related_to| Box::new(is_related_to) as Box<dyn Fn(&Type, &Type) -> Ternary>,
        );
        unimplemented!()
    }

    pub(super) fn is_weak_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn has_common_properties(
        &self,
        source: &Type,
        target: &Type,
        is_comparing_jsx_attributes: bool,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_alias_variances(&self, symbol: &Symbol) -> Vec<VarianceFlags> {
        unimplemented!()
    }

    pub(super) fn get_variances(&self, type_: &Type /*GenericType*/) -> Vec<VarianceFlags> {
        unimplemented!()
    }

    pub(super) fn has_covariant_void_argument(
        &self,
        type_arguments: &[Rc<Type>],
        variances: &[VarianceFlags],
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_relation_key(
        &self,
        source: &Type,
        target: &Type,
        intersection_state: IntersectionState,
        relation: &HashMap<String, RelationComparisonResult>,
    ) -> String {
        unimplemented!()
    }

    pub(super) fn get_declaring_class(&self, prop: &Symbol) -> Option<Rc<Type /*InterfaceType*/>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_property_in_base_class(&self, property: &Symbol) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn is_valid_override_of(&self, source_prop: &Symbol, target_prop: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn is_deeply_nested_type(
        &self,
        type_: &Type,
        stack: &[Rc<Type>],
        depth: usize,
        max_depth: Option<usize>,
    ) -> bool {
        let max_depth = max_depth.unwrap_or(5);
        unimplemented!()
    }

    pub(super) fn compare_properties<TCompareTypes: FnMut(&Type, &Type) -> Ternary>(
        &self,
        source_prop: &Symbol,
        target_prop: &Symbol,
        mut compare_types: TCompareTypes,
    ) -> Ternary {
        unimplemented!()
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
}
