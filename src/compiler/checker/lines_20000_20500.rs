#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{
    CheckTypeContainingMessageChain, CheckTypeRelatedTo, IntersectionState, RecursionFlags,
};
use crate::{
    __String, every, for_each_bool, get_check_flags, get_selected_effective_modifier_flags, map,
    some, CheckFlags, Diagnostics, IndexInfo, InterfaceTypeInterface, ModifierFlags, Node,
    ObjectFlags, ObjectFlagsTypeInterface, RelationComparisonResult, Signature, Symbol,
    SymbolFlags, SymbolInterface, Ternary, Type, TypeChecker, TypeFlags, TypeInterface,
    UnionOrIntersectionTypeInterface, VarianceFlags,
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
        let related = self.is_related_to(
            &source_info.type_,
            &target_info.type_,
            Some(RecursionFlags::Both),
            Some(report_errors),
            None,
            None,
        );
        if related == Ternary::False && report_errors {
            if Rc::ptr_eq(&source_info.key_type, &target_info.key_type) {
                self.report_error(
                    Cow::Borrowed(&Diagnostics::_0_index_signatures_are_incompatible),
                    Some(vec![self.type_checker.type_to_string_(
                        &source_info.key_type,
                        Option::<&Node>::None,
                        None,
                        None,
                    )]),
                );
            } else {
                self.report_error(
                    Cow::Borrowed(&Diagnostics::_0_and_1_index_signatures_are_incompatible),
                    Some(vec![
                        self.type_checker.type_to_string_(
                            &source_info.key_type,
                            Option::<&Node>::None,
                            None,
                            None,
                        ),
                        self.type_checker.type_to_string_(
                            &target_info.key_type,
                            Option::<&Node>::None,
                            None,
                            None,
                        ),
                    ]),
                );
            };
        }
        related
    }

    pub(super) fn index_signatures_related_to(
        &self,
        source: &Type,
        target: &Type,
        source_is_primitive: bool,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if Rc::ptr_eq(&self.relation, &self.type_checker.identity_relation) {
            return self.index_signatures_identical_to(source, target);
        }
        let index_infos = self.type_checker.get_index_infos_of_type(target);
        let target_has_string_index = some(
            Some(&index_infos),
            Some(|info: &Rc<IndexInfo>| {
                Rc::ptr_eq(&info.key_type, &self.type_checker.string_type())
            }),
        );
        let mut result = Ternary::True;
        for target_info in &index_infos {
            let related = if !source_is_primitive
                && target_has_string_index
                && target_info.type_.flags().intersects(TypeFlags::Any)
            {
                Ternary::True
            } else if self.type_checker.is_generic_mapped_type(source) && target_has_string_index {
                self.is_related_to(
                    &self.type_checker.get_template_type_from_mapped_type(source),
                    &target_info.type_,
                    Some(RecursionFlags::Both),
                    Some(report_errors),
                    None,
                    None,
                )
            } else {
                self.type_related_to_index_info(
                    source,
                    target_info,
                    report_errors,
                    intersection_state,
                )
            };
            if related == Ternary::False {
                return Ternary::False;
            }
            result &= related;
        }
        result
    }

    pub(super) fn type_related_to_index_info(
        &self,
        source: &Type,
        target_info: &IndexInfo,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let source_info = self
            .type_checker
            .get_applicable_index_info(source, &target_info.key_type);
        if let Some(source_info) = source_info.as_ref() {
            return self.index_info_related_to(source_info, target_info, report_errors);
        }
        if !intersection_state.intersects(IntersectionState::Source)
            && self
                .type_checker
                .is_object_type_with_inferable_index(source)
        {
            return self.members_related_to_index_info(source, target_info, report_errors);
        }
        if report_errors {
            self.report_error(
                Cow::Borrowed(&Diagnostics::Index_signature_for_type_0_is_missing_in_type_1),
                Some(vec![
                    self.type_checker.type_to_string_(
                        &target_info.key_type,
                        Option::<&Node>::None,
                        None,
                        None,
                    ),
                    self.type_checker
                        .type_to_string_(source, Option::<&Node>::None, None, None),
                ]),
            );
        }
        Ternary::False
    }

    pub(super) fn index_signatures_identical_to(&self, source: &Type, target: &Type) -> Ternary {
        let source_infos = self.type_checker.get_index_infos_of_type(source);
        let target_infos = self.type_checker.get_index_infos_of_type(target);
        if source_infos.len() != target_infos.len() {
            return Ternary::False;
        }
        for target_info in &target_infos {
            let source_info = self
                .type_checker
                .get_index_info_of_type_(source, &target_info.key_type);
            if !matches!(
                source_info.as_ref(),
                Some(source_info) if self.is_related_to(
                    &source_info.type_,
                    &target_info.type_,
                    Some(RecursionFlags::Both),
                    None, None, None
                ) != Ternary::False && source_info.is_readonly == target_info.is_readonly
            ) {
                return Ternary::False;
            }
        }
        Ternary::True
    }

    pub(super) fn constructor_visibilities_are_compatible(
        &self,
        source_signature: &Signature,
        target_signature: &Signature,
        report_errors: bool,
    ) -> bool {
        if source_signature.declaration.is_none() || target_signature.declaration.is_none() {
            return true;
        }
        let source_signature_declaration = source_signature.declaration.as_ref().unwrap();
        let target_signature_declaration = target_signature.declaration.as_ref().unwrap();

        let source_accessibility = get_selected_effective_modifier_flags(
            source_signature_declaration,
            ModifierFlags::NonPublicAccessibilityModifier,
        );
        let target_accessibility = get_selected_effective_modifier_flags(
            target_signature_declaration,
            ModifierFlags::NonPublicAccessibilityModifier,
        );

        if target_accessibility == ModifierFlags::Private {
            return true;
        }

        if target_accessibility == ModifierFlags::Protected
            && source_accessibility != ModifierFlags::Private
        {
            return true;
        }

        if target_accessibility != ModifierFlags::Protected
            && source_accessibility == ModifierFlags::None
        {
            return true;
        }

        if report_errors {
            self.report_error(
                Cow::Borrowed(
                    &Diagnostics::Cannot_assign_a_0_constructor_type_to_a_1_constructor_type,
                ),
                Some(vec![
                    self.type_checker
                        .visibility_to_string(source_accessibility)
                        .to_owned(),
                    self.type_checker
                        .visibility_to_string(target_accessibility)
                        .to_owned(),
                ]),
            );
        }

        false
    }
}

impl TypeChecker {
    pub(super) fn type_could_have_top_level_singleton_types(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Boolean) {
            return false;
        }

        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            return for_each_bool(
                type_.as_union_or_intersection_type_interface().types(),
                |type_: &Rc<Type>, _| self.type_could_have_top_level_singleton_types(type_),
            );
        }

        if type_.flags().intersects(TypeFlags::Instantiable) {
            let constraint = self.get_constraint_of_type(type_);
            if let Some(constraint) = constraint
                .as_ref()
                .filter(|constraint| !ptr::eq(&***constraint, type_))
            {
                return self.type_could_have_top_level_singleton_types(constraint);
            }
        }

        self.is_unit_type(type_) || type_.flags().intersects(TypeFlags::TemplateLiteral)
    }

    pub(super) fn get_exact_optional_unassignable_properties(
        &self,
        source: &Type,
        target: &Type,
    ) -> Vec<Rc<Symbol>> {
        if self.is_tuple_type(source) && self.is_tuple_type(target) {
            return vec![];
        }
        self.get_properties_of_type(target)
            .into_iter()
            .filter(|target_prop: &Rc<Symbol>| {
                self.is_exact_optional_property_mismatch(
                    self.get_type_of_property_of_type_(source, target_prop.escaped_name()),
                    Some(self.get_type_of_symbol(target_prop)),
                )
            })
            .collect()
    }

    pub(super) fn is_exact_optional_property_mismatch<
        TSource: Borrow<Type>,
        TTarget: Borrow<Type>,
    >(
        &self,
        source: Option<TSource>,
        target: Option<TTarget>,
    ) -> bool {
        if source.is_none() || target.is_none() {
            return false;
        }
        let source = source.unwrap();
        let source = source.borrow();
        let target = target.unwrap();
        let target = target.borrow();
        self.maybe_type_of_kind(source, TypeFlags::Undefined) && self.contains_missing_type(target)
    }

    pub(super) fn get_exact_optional_properties(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        self.get_properties_of_type(type_)
            .into_iter()
            .filter(|target_prop: &Rc<Symbol>| {
                self.contains_missing_type(&self.get_type_of_symbol(target_prop))
            })
            .collect()
    }

    pub(super) fn get_best_matching_type<TIsRelatedTo: Fn(&Type, &Type) -> Ternary>(
        &self,
        source: &Type,
        target: &Type, /*UnionOrIntersectionType*/
        is_related_to: Option<TIsRelatedTo>,
    ) -> Option<Rc<Type>> {
        let is_related_to = |source: &Type, target: &Type| match is_related_to.as_ref() {
            None => self.compare_types_assignable(source, target),
            Some(is_related_to) => is_related_to(source, target),
        };
        self.find_matching_discriminant_type(source, target, is_related_to, Some(true))
            .or_else(|| self.find_matching_type_reference_or_type_alias_reference(source, target))
            .or_else(|| self.find_best_type_for_object_literal(source, target))
            .or_else(|| self.find_best_type_for_invokable(source, target))
            .or_else(|| self.find_most_overlappy_type(source, target))
    }

    pub(super) fn discriminate_type_by_discriminable_items<
        TDiscriminatorsCallback: Fn() -> Rc<Type>,
        TRelated: FnMut(&Type, &Type) -> bool,
        TDefaultValue: Borrow<Type>,
    >(
        &self,
        target: &Type, /*UnionType*/
        discriminators: &[(TDiscriminatorsCallback, __String)],
        mut related: TRelated,
        default_value: Option<TDefaultValue>,
        skip_partial: Option<bool>,
    ) -> Option<Rc<Type>> {
        let target_as_union_type = target.as_union_type();
        let target_types = target_as_union_type.types();
        let mut discriminable: Vec<Option<bool>> = target_types
            .into_iter()
            .map(|_| Option::<bool>::None)
            .collect();
        for (get_discriminating_type, property_name) in discriminators {
            let target_prop = self.get_union_or_intersection_property(target, property_name, None);
            if matches!(skip_partial, Some(true))
                && matches!(
                    target_prop.as_ref(),
                    Some(target_prop) if get_check_flags(target_prop).intersects(CheckFlags::ReadPartial)
                )
            {
                continue;
            }
            let mut i = 0;
            for type_ in target_types {
                let target_type = self.get_type_of_property_of_type_(type_, property_name);
                if matches!(
                    target_type.as_ref(),
                    Some(target_type) if related(&get_discriminating_type(), target_type)
                ) {
                    discriminable[i] = if discriminable[i].is_none() {
                        Some(true)
                    } else {
                        discriminable[i]
                    };
                } else {
                    discriminable[i] = Some(false);
                }
                i += 1;
            }
        }
        let match_ = discriminable.iter().position(|item| *item == Some(true));
        let default_value =
            default_value.map(|default_value| default_value.borrow().type_wrapper());
        if match_.is_none() {
            return default_value;
        }
        let match_ = match_.unwrap();
        let mut next_match = discriminable
            .iter()
            .skip(match_ + 1)
            .position(|item| *item == Some(true))
            .map(|position| position + match_ + 1);
        while let Some(next_match_present) = next_match {
            if !self.is_type_identical_to(&target_types[match_], &target_types[next_match_present])
            {
                return default_value;
            }
            next_match = discriminable
                .iter()
                .skip(next_match_present + 1)
                .position(|item| *item == Some(true))
                .map(|position| position + next_match_present + 1);
        }
        Some(target_types[match_].clone())
    }

    pub(super) fn is_weak_type(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(type_);
            let resolved_as_resolved_type = resolved.as_resolved_type();
            return resolved_as_resolved_type.call_signatures().is_empty()
                && resolved_as_resolved_type.construct_signatures().is_empty()
                && resolved_as_resolved_type.index_infos().is_empty()
                && !resolved_as_resolved_type.properties().is_empty()
                && every(
                    &*resolved_as_resolved_type.properties(),
                    |p: &Rc<Symbol>, _| p.flags().intersects(SymbolFlags::Optional),
                );
        }
        if type_.flags().intersects(TypeFlags::Intersection) {
            return every(
                type_.as_union_or_intersection_type_interface().types(),
                |type_: &Rc<Type>, _| self.is_weak_type(type_),
            );
        }
        false
    }

    pub(super) fn has_common_properties(
        &self,
        source: &Type,
        target: &Type,
        is_comparing_jsx_attributes: bool,
    ) -> bool {
        for prop in &self.get_properties_of_type(source) {
            if self.is_known_property(target, prop.escaped_name(), is_comparing_jsx_attributes) {
                return true;
            }
        }
        false
    }

    pub(super) fn get_marker_type_reference(
        &self,
        type_: &Type,  /*GenericType*/
        source: &Type, /*TypeParameter*/
        target: &Type,
    ) -> Rc<Type /*TypeReference*/> {
        let result = self.create_type_reference(
            type_,
            map(
                type_.as_generic_type().maybe_type_parameters(),
                |t: &Rc<Type>, _| {
                    if ptr::eq(&**t, source) {
                        target.type_wrapper()
                    } else {
                        t.clone()
                    }
                },
            ),
        );
        result
            .as_type_reference()
            .set_object_flags(result.as_type_reference().object_flags() | ObjectFlags::MarkerType);
        result
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
