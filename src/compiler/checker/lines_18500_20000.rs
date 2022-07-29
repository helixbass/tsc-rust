#![allow(non_upper_case_globals)]

use regex::{Captures, Regex};
use std::collections::HashSet;
use std::ptr;
use std::rc::Rc;

use super::{
    CheckTypeContainingMessageChain, CheckTypeRelatedTo, ExpandingFlags, IntersectionState,
    MappedTypeModifiers, RecursionFlags,
};
use crate::{
    are_rc_slices_equal, get_object_flags, same_map, AccessFlags, DiagnosticMessageChain,
    Diagnostics, Node, NodeInterface, ObjectFlags, ObjectTypeInterface, RelationComparisonResult,
    Symbol, SymbolInterface, Ternary, Type, TypeChecker, TypeFlags, TypeInterface,
    TypeMapperCallback, UnionOrIntersectionTypeInterface, VarianceFlags, __String,
};

impl<'type_checker, TContainingMessageChain: CheckTypeContainingMessageChain>
    CheckTypeRelatedTo<'type_checker, TContainingMessageChain>
{
    pub(super) fn should_check_as_excess_property(
        &self,
        prop: &Symbol,
        container: &Symbol,
    ) -> bool {
        if let Some(prop_value_declaration) = prop.maybe_value_declaration().as_ref() {
            if let Some(container_value_declaration) = container.maybe_value_declaration().as_ref()
            {
                return Rc::ptr_eq(
                    &prop_value_declaration.parent(),
                    container_value_declaration,
                );
            }
        }
        false
    }

    pub(super) fn each_type_related_to_some_type(
        &self,
        source: &Type, /*UnionOrIntersectionType*/
        target: &Type, /*UnionOrIntersectionType*/
    ) -> Ternary {
        let mut result = Ternary::True;
        let source_types = source.as_union_or_intersection_type_interface().types();
        for source_type in source_types {
            let related = self.type_related_to_some_type(source_type, target, false);
            if related == Ternary::False {
                return Ternary::False;
            }
            result &= related;
        }
        result
    }

    pub(super) fn type_related_to_some_type(
        &self,
        source: &Type,
        target: &Type, /*UnionOrIntersectionType*/
        report_errors: bool,
    ) -> Ternary {
        let target_types = target.as_union_or_intersection_type_interface().types();
        if target.flags().intersects(TypeFlags::Union) {
            if self.type_checker.contains_type(target_types, source) {
                return Ternary::True;
            }
            let match_ = self
                .type_checker
                .get_matching_union_constituent_for_type(target, source);
            if let Some(match_) = match_.as_ref() {
                let related = self.is_related_to(
                    source,
                    match_,
                    Some(RecursionFlags::Target),
                    Some(false),
                    None,
                    None,
                );
                if related != Ternary::False {
                    return related;
                }
            }
        }
        for type_ in target_types {
            let related = self.is_related_to(
                source,
                type_,
                Some(RecursionFlags::Target),
                Some(false),
                None,
                None,
            );
            if related != Ternary::False {
                return related;
            }
        }
        if report_errors {
            let best_matching_type = self.type_checker.get_best_matching_type(
                source,
                target,
                Some(|source: &Type, target: &Type| {
                    self.is_related_to(source, target, None, None, None, None)
                }),
            );
            self.is_related_to(
                source,
                &best_matching_type.unwrap_or_else(|| target_types[target_types.len() - 1].clone()),
                Some(RecursionFlags::Target),
                Some(true),
                None,
                None,
            );
        }
        Ternary::False
    }

    pub(super) fn type_related_to_each_type(
        &self,
        source: &Type,
        target: &Type, /*IntersectionType*/
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let mut result = Ternary::True;
        let target_types = target.as_union_or_intersection_type_interface().types();
        for target_type in target_types {
            let related = self.is_related_to(
                source,
                target_type,
                Some(RecursionFlags::Target),
                Some(report_errors),
                None,
                Some(intersection_state),
            );
            if related == Ternary::False {
                return Ternary::False;
            }
            result &= related;
        }
        result
    }

    pub(super) fn some_type_related_to_type(
        &self,
        source: &Type, /*UnionOrIntersectionType*/
        target: &Type,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let source_types = source.as_union_or_intersection_type_interface().types();
        if source.flags().intersects(TypeFlags::Union)
            && self.type_checker.contains_type(source_types, target)
        {
            return Ternary::True;
        }
        let len = source_types.len();
        for i in 0..len {
            let related = self.is_related_to(
                &source_types[i],
                target,
                Some(RecursionFlags::Source),
                Some(report_errors && i == len - 1),
                None,
                Some(intersection_state),
            );
            if related != Ternary::False {
                return related;
            }
        }
        Ternary::False
    }

    pub(super) fn get_undefined_stripped_target_if_needed(
        &self,
        source: &Type,
        target: &Type,
    ) -> Rc<Type> {
        if source.flags().intersects(TypeFlags::Union)
            && target.flags().intersects(TypeFlags::Union)
            && !source.as_union_or_intersection_type_interface().types()[0]
                .flags()
                .intersects(TypeFlags::Undefined)
            && target.as_union_or_intersection_type_interface().types()[0]
                .flags()
                .intersects(TypeFlags::Undefined)
        {
            return self
                .type_checker
                .extract_types_of_kind(target, !TypeFlags::Undefined);
        }
        target.type_wrapper()
    }

    pub(super) fn each_type_related_to_type(
        &self,
        source: &Type, /*UnionOrIntersectionType*/
        target: &Type,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let mut result = Ternary::True;
        let source_types = source.as_union_or_intersection_type_interface().types();
        let undefined_stripped_target =
            self.get_undefined_stripped_target_if_needed(source, target);
        for (i, source_type) in source_types.into_iter().enumerate() {
            if undefined_stripped_target
                .flags()
                .intersects(TypeFlags::Union)
            {
                let undefined_stripped_target_as_union_type =
                    undefined_stripped_target.as_union_type();
                let undefined_stripped_target_types =
                    undefined_stripped_target_as_union_type.types();
                if source_types.len() >= undefined_stripped_target_types.len()
                    && source_types.len() % undefined_stripped_target_types.len() == 0
                {
                    let related = self.is_related_to(
                        &source_type,
                        &undefined_stripped_target_types[i % undefined_stripped_target_types.len()],
                        Some(RecursionFlags::Both),
                        Some(false),
                        None,
                        Some(intersection_state),
                    );
                    if related != Ternary::False {
                        result &= related;
                        continue;
                    }
                }
            }
            let related = self.is_related_to(
                source_type,
                target,
                Some(RecursionFlags::Source),
                Some(report_errors),
                None,
                Some(intersection_state),
            );
            if related == Ternary::False {
                return Ternary::False;
            }
            result &= related;
        }
        result
    }

    pub(super) fn type_arguments_related_to(
        &self,
        sources: Option<Vec<Rc<Type>>>,
        targets: Option<Vec<Rc<Type>>>,
        variances: Option<Vec<VarianceFlags>>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let sources = sources.unwrap_or_else(|| vec![]);
        let targets = targets.unwrap_or_else(|| vec![]);
        let variances = variances.unwrap_or_else(|| vec![]);
        if sources.len() != targets.len()
            && Rc::ptr_eq(&self.relation, &self.type_checker.identity_relation)
        {
            return Ternary::False;
        }
        let length = if sources.len() <= targets.len() {
            sources.len()
        } else {
            targets.len()
        };
        let mut result = Ternary::True;
        for i in 0..length {
            let variance_flags = if i < variances.len() {
                variances[i]
            } else {
                VarianceFlags::Covariant
            };
            let variance = variance_flags & VarianceFlags::VarianceMask;
            if variance != VarianceFlags::Independent {
                let s = &sources[i];
                let t = &targets[i];
                let mut related/* = Ternary::True*/;
                if variance_flags.intersects(VarianceFlags::Unmeasurable) {
                    related = if Rc::ptr_eq(&self.relation, &self.type_checker.identity_relation) {
                        self.is_related_to(
                            s,
                            t,
                            Some(RecursionFlags::Both),
                            Some(false),
                            None,
                            None,
                        )
                    } else {
                        self.type_checker.compare_types_identical(s, t)
                    };
                } else if variance == VarianceFlags::Covariant {
                    related = self.is_related_to(
                        s,
                        t,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        Some(intersection_state),
                    );
                } else if variance == VarianceFlags::Contravariant {
                    related = self.is_related_to(
                        t,
                        s,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        Some(intersection_state),
                    );
                } else if variance == VarianceFlags::Bivariant {
                    related = self.is_related_to(
                        t,
                        s,
                        Some(RecursionFlags::Both),
                        Some(false),
                        None,
                        None,
                    );
                    if related == Ternary::False {
                        related = self.is_related_to(
                            s,
                            t,
                            Some(RecursionFlags::Both),
                            Some(report_errors),
                            None,
                            Some(intersection_state),
                        );
                    }
                } else {
                    related = self.is_related_to(
                        s,
                        t,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        Some(intersection_state),
                    );
                    if related != Ternary::False {
                        related &= self.is_related_to(
                            t,
                            s,
                            Some(RecursionFlags::Both),
                            Some(report_errors),
                            None,
                            Some(intersection_state),
                        );
                    }
                }
                if related == Ternary::False {
                    return Ternary::False;
                }
                result &= related;
            }
        }
        result
    }

    pub(super) fn recursive_type_related_to(
        &self,
        source: &Type,
        target: &Type,
        report_errors: bool,
        intersection_state: IntersectionState,
        recursion_flags: RecursionFlags,
    ) -> Ternary {
        if self.overflow() {
            return Ternary::False;
        }
        let id = self.type_checker.get_relation_key(
            source,
            target,
            intersection_state
                | (if self.in_property_check() {
                    IntersectionState::InPropertyCheck
                } else {
                    IntersectionState::None
                }),
            &(*self.relation).borrow(),
        );
        let entry = (*self.relation).borrow().get(&id).map(Clone::clone);
        if let Some(entry) = entry.as_ref() {
            if report_errors
                && entry.intersects(RelationComparisonResult::Failed)
                && !entry.intersects(RelationComparisonResult::Reported)
            {
            } else {
                // if (outofbandVarianceMarkerHandler) {
                if false {
                    let saved = *entry & RelationComparisonResult::ReportsMask;
                    if saved.intersects(RelationComparisonResult::ReportsUnmeasurable) {
                        self.type_checker.instantiate_type(
                            source,
                            Some(
                                &self
                                    .type_checker
                                    .make_function_type_mapper(ReportUnmeasurableMarkers),
                            ),
                        );
                    }
                    if saved.intersects(RelationComparisonResult::ReportsUnreliable) {
                        self.type_checker.instantiate_type(
                            source,
                            Some(
                                &self
                                    .type_checker
                                    .make_function_type_mapper(ReportUnreliableMarkers),
                            ),
                        );
                    }
                }
                return if entry.intersects(RelationComparisonResult::Succeeded) {
                    Ternary::True
                } else {
                    Ternary::False
                };
            }
        }
        if self.maybe_keys().is_none() {
            *self.maybe_keys() = Some(vec![]);
            *self.maybe_source_stack() = Some(vec![]);
            *self.maybe_target_stack() = Some(vec![]);
        } else {
            lazy_static! {
                static ref DASH_DIGITS_REGEX: Regex = Regex::new(r"-\d+").unwrap();
            }
            lazy_static! {
                static ref DASH_OR_EQUALS_REGEX: Regex = Regex::new(r"[-=]").unwrap();
            }
            let broadest_equivalent_id = id
                .split(",")
                .map(|i| {
                    DASH_DIGITS_REGEX.replace_all(i, |captures: &Captures| {
                        let offset = captures.get(0).unwrap().start();
                        let index = DASH_OR_EQUALS_REGEX.find_iter(&id[0..offset]).count();
                        format!("={}", index)
                    })
                })
                .collect::<Vec<_>>()
                .join(",");
            for i in 0..self.maybe_count() {
                if matches!(
                    self.maybe_keys().as_ref().unwrap().get(i),
                    Some(maybe_key) if &id == maybe_key || &broadest_equivalent_id == maybe_key
                ) {
                    return Ternary::Maybe;
                }
            }
            if self.source_depth() == 100 || self.target_depth() == 100 {
                self.set_overflow(true);
                return Ternary::False;
            }
        }
        let maybe_start = self.maybe_count();
        self.maybe_keys().as_mut().unwrap().push(id.clone());
        self.set_maybe_count(self.maybe_count() + 1);
        let save_expanding_flags = self.expanding_flags();
        if recursion_flags.intersects(RecursionFlags::Source) {
            self.maybe_source_stack()
                .as_mut()
                .unwrap()
                .push(source.type_wrapper());
            self.set_source_depth(self.source_depth() + 1);
            if !self.expanding_flags().intersects(ExpandingFlags::Source)
                && self.type_checker.is_deeply_nested_type(
                    source,
                    self.maybe_source_stack().as_deref().unwrap(),
                    self.source_depth(),
                    None,
                )
            {
                self.set_expanding_flags(self.expanding_flags() | ExpandingFlags::Source);
            }
        }
        if recursion_flags.intersects(RecursionFlags::Target) {
            self.maybe_target_stack()
                .as_mut()
                .unwrap()
                .push(target.type_wrapper());
            self.set_target_depth(self.target_depth() + 1);
            if !self.expanding_flags().intersects(ExpandingFlags::Target)
                && self.type_checker.is_deeply_nested_type(
                    target,
                    self.maybe_target_stack().as_deref().unwrap(),
                    self.target_depth(),
                    None,
                )
            {
                self.set_expanding_flags(self.expanding_flags() | ExpandingFlags::Target);
            }
        }
        // let originalHandler: typeof outofbandVarianceMarkerHandler;
        let mut propagating_variance_flags = RelationComparisonResult::None;
        // if (outofbandVarianceMarkerHandler) {
        //     originalHandler = outofbandVarianceMarkerHandler;
        //     outofbandVarianceMarkerHandler = onlyUnreliable => {
        //         propagatingVarianceFlags |= onlyUnreliable ? RelationComparisonResult.ReportsUnreliable : RelationComparisonResult.ReportsUnmeasurable;
        //         return originalHandler!(onlyUnreliable);
        //     };
        // }

        if self.expanding_flags() == ExpandingFlags::Both {
            // tracing?.instant(tracing.Phase.CheckTypes, "recursiveTypeRelatedTo_DepthLimit", {
            //     sourceId: source.id,
            //     sourceIdStack: sourceStack.map(t => t.id),
            //     targetId: target.id,
            //     targetIdStack: targetStack.map(t => t.id),
            //     depth: sourceDepth,
            //     targetDepth
            // });
        }

        let result = if self.expanding_flags() != ExpandingFlags::Both {
            self.structured_type_related_to(source, target, report_errors, intersection_state)
        } else {
            Ternary::Maybe
        };
        // if (outofbandVarianceMarkerHandler) {
        //     outofbandVarianceMarkerHandler = originalHandler;
        // }
        if recursion_flags.intersects(RecursionFlags::Source) {
            self.set_source_depth(self.source_depth() - 1);
            self.maybe_source_stack()
                .as_mut()
                .unwrap()
                .truncate(self.source_depth());
        }
        if recursion_flags.intersects(RecursionFlags::Target) {
            self.set_target_depth(self.target_depth() - 1);
            self.maybe_target_stack()
                .as_mut()
                .unwrap()
                .truncate(self.target_depth());
        }
        self.set_expanding_flags(save_expanding_flags);
        if result != Ternary::False {
            if result == Ternary::True || self.source_depth() == 0 && self.target_depth() == 0 {
                if matches!(result, Ternary::True | Ternary::Maybe) {
                    for i in maybe_start..self.maybe_count() {
                        self.relation.borrow_mut().insert(
                            self.maybe_keys().as_ref().unwrap()[i].clone(),
                            RelationComparisonResult::Succeeded | propagating_variance_flags,
                        );
                    }
                }
                self.set_maybe_count(maybe_start);
                self.maybe_keys()
                    .as_mut()
                    .unwrap()
                    .truncate(self.maybe_count());
            }
        } else {
            self.relation.borrow_mut().insert(
                id,
                (if report_errors {
                    RelationComparisonResult::Reported
                } else {
                    RelationComparisonResult::None
                }) | RelationComparisonResult::Failed
                    | propagating_variance_flags,
            );
            self.set_maybe_count(maybe_start);
            self.maybe_keys()
                .as_mut()
                .unwrap()
                .truncate(self.maybe_count());
        }
        result
    }

    pub(super) fn structured_type_related_to(
        &self,
        source: &Type,
        target: &Type,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        // tracing?.push(tracing.Phase.CheckTypes, "structuredTypeRelatedTo", { sourceId: source.id, targetId: target.id });
        let result = self.structured_type_related_to_worker(
            source,
            target,
            report_errors,
            intersection_state,
        );
        // tracing?.pop();
        result
    }

    pub(super) fn structured_type_related_to_worker(
        &self,
        source: &Type,
        target: &Type,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if intersection_state.intersects(IntersectionState::PropertyCheck) {
            return self.properties_related_to(
                source,
                target,
                report_errors,
                None,
                IntersectionState::None,
            );
        }
        let mut source = source.type_wrapper();
        if intersection_state.intersects(IntersectionState::UnionIntersectionCheck) {
            if source.flags().intersects(TypeFlags::Union) {
                return if Rc::ptr_eq(&self.relation, &self.type_checker.comparable_relation) {
                    self.some_type_related_to_type(
                        &source,
                        target,
                        report_errors && !source.flags().intersects(TypeFlags::Primitive),
                        intersection_state & !IntersectionState::UnionIntersectionCheck,
                    )
                } else {
                    self.each_type_related_to_type(
                        &source,
                        target,
                        report_errors && !source.flags().intersects(TypeFlags::Primitive),
                        intersection_state & !IntersectionState::UnionIntersectionCheck,
                    )
                };
            }
            if target.flags().intersects(TypeFlags::Union) {
                return self.type_related_to_some_type(
                    &self
                        .type_checker
                        .get_regular_type_of_object_literal(&source),
                    target,
                    report_errors
                        && !source.flags().intersects(TypeFlags::Primitive)
                        && !target.flags().intersects(TypeFlags::Primitive),
                );
            }
            if target.flags().intersects(TypeFlags::Intersection) {
                return self.type_related_to_each_type(
                    &self
                        .type_checker
                        .get_regular_type_of_object_literal(&source),
                    target,
                    report_errors,
                    IntersectionState::Target,
                );
            }
            if Rc::ptr_eq(&self.relation, &self.type_checker.comparable_relation)
                && target.flags().intersects(TypeFlags::Primitive)
            {
                let constraints = same_map(
                    Some(source.as_union_or_intersection_type_interface().types()),
                    |type_: &Rc<Type>, _| self.type_checker.get_base_constraint_or_type(type_),
                );
                if !are_rc_slices_equal(
                    &constraints,
                    source.as_union_or_intersection_type_interface().types(),
                ) {
                    source = self.type_checker.get_intersection_type(
                        &constraints,
                        Option::<&Symbol>::None,
                        None,
                    );
                    if !source.flags().intersects(TypeFlags::Intersection) {
                        return self.is_related_to(
                            &source,
                            target,
                            Some(RecursionFlags::Source),
                            Some(false),
                            None,
                            None,
                        );
                    }
                }
            }
            return self.some_type_related_to_type(
                &source,
                target,
                false,
                IntersectionState::Source,
            );
        }
        let flags = source.flags() & target.flags();
        if Rc::ptr_eq(&self.relation, &self.type_checker.identity_relation)
            && !flags.intersects(TypeFlags::Object)
        {
            if flags.intersects(TypeFlags::Index) {
                return self.is_related_to(
                    &source.as_index_type().type_,
                    &target.as_index_type().type_,
                    Some(RecursionFlags::Both),
                    Some(false),
                    None,
                    None,
                );
            }
            let mut result = Ternary::False;
            if flags.intersects(TypeFlags::IndexedAccess) {
                result = self.is_related_to(
                    &source.as_indexed_access_type().object_type,
                    &target.as_indexed_access_type().object_type,
                    Some(RecursionFlags::Both),
                    Some(false),
                    None,
                    None,
                );
                if result != Ternary::False {
                    result &= self.is_related_to(
                        &source.as_indexed_access_type().index_type,
                        &target.as_indexed_access_type().index_type,
                        Some(RecursionFlags::Both),
                        Some(false),
                        None,
                        None,
                    );
                    if result != Ternary::False {
                        return result;
                    }
                }
            }
            if flags.intersects(TypeFlags::Conditional) {
                if source.as_conditional_type().root.is_distributive
                    == target.as_conditional_type().root.is_distributive
                {
                    result = self.is_related_to(
                        &source.as_conditional_type().check_type,
                        &target.as_conditional_type().check_type,
                        Some(RecursionFlags::Both),
                        Some(false),
                        None,
                        None,
                    );
                    if result != Ternary::False {
                        result &= self.is_related_to(
                            &source.as_conditional_type().extends_type,
                            &target.as_conditional_type().extends_type,
                            Some(RecursionFlags::Both),
                            Some(false),
                            None,
                            None,
                        );
                        if result != Ternary::False {
                            result &= self.is_related_to(
                                &self
                                    .type_checker
                                    .get_true_type_from_conditional_type(&source),
                                &self
                                    .type_checker
                                    .get_true_type_from_conditional_type(target),
                                Some(RecursionFlags::Both),
                                Some(false),
                                None,
                                None,
                            );
                            if result != Ternary::False {
                                result &= self.is_related_to(
                                    &self
                                        .type_checker
                                        .get_false_type_from_conditional_type(&source),
                                    &self
                                        .type_checker
                                        .get_false_type_from_conditional_type(target),
                                    Some(RecursionFlags::Both),
                                    Some(false),
                                    None,
                                    None,
                                );
                                if result != Ternary::False {
                                    return result;
                                }
                            }
                        }
                    }
                }
            }
            if flags.intersects(TypeFlags::Substitution) {
                return self.is_related_to(
                    &source.as_substitution_type().substitute,
                    &target.as_substitution_type().substitute,
                    Some(RecursionFlags::Both),
                    Some(false),
                    None,
                    None,
                );
            }
            return Ternary::False;
        }

        let mut result: Ternary;
        let mut original_error_info: Option<DiagnosticMessageChain> = None;
        let mut variance_check_failed = false;
        let save_error_info = self.capture_error_calculation_state();

        if source
            .flags()
            .intersects(TypeFlags::Object | TypeFlags::Conditional)
        {
            if let Some(source_alias_symbol) = source.maybe_alias_symbol() {
                if let Some(source_alias_type_arguments) = source.maybe_alias_type_arguments() {
                    if matches!(
                        target.maybe_alias_symbol(),
                        Some(target_alias_symbol) if Rc::ptr_eq(
                            &source_alias_symbol,
                            &target_alias_symbol,
                        )
                    ) {
                        if !(matches!(
                            source.maybe_alias_type_arguments_contains_marker(),
                            Some(true)
                        ) || matches!(
                            target.maybe_alias_type_arguments_contains_marker(),
                            Some(true)
                        )) {
                            let variances =
                                self.type_checker.get_alias_variances(&source_alias_symbol);
                            if variances.is_empty() {
                                return Ternary::Unknown;
                            }
                            let variance_result = self.relate_variances(
                                Some(source_alias_type_arguments),
                                Some(target_alias_type_arguments),
                                &variances,
                                intersection_state,
                            );
                            if let Some(variance_result) = variance_result {
                                return variance_result;
                            }
                        }
                    }
                }
            }
        }

        if self
            .type_checker
            .is_single_element_generic_tuple_type(&source)
            && !source.as_type_reference().target.as_tuple_type().readonly
            && {
                result = self.is_related_to(
                    &self.type_checker.get_type_arguments(&source)[0],
                    target,
                    Some(RecursionFlags::Source),
                    None,
                    None,
                    None,
                );
                result != Ternary::False
            }
            || self
                .type_checker
                .is_single_element_generic_tuple_type(target)
                && (target.as_type_reference().target.as_tuple_type().readonly
                    || self.type_checker.is_mutable_array_or_tuple(
                        &self
                            .type_checker
                            .get_base_constraint_of_type(&source)
                            .unwrap_or_else(|| source.clone()),
                    ))
                && {
                    result = self.is_related_to(
                        &source,
                        &self.type_checker.get_type_arguments(target)[0],
                        Some(RecursionFlags::Target),
                        None,
                        None,
                        None,
                    );
                    result != Ternary::False
                }
        {
            return result;
        }

        if target.flags().intersects(TypeFlags::TypeParameter) {
            if get_object_flags(&source).intersects(ObjectFlags::Mapped)
                && source
                    .as_mapped_type()
                    .declaration
                    .as_mapped_type_node()
                    .name_type
                    .is_none()
                && self.is_related_to(
                    &self.type_checker.get_index_type(target, None, None),
                    &self
                        .type_checker
                        .get_constraint_type_from_mapped_type(&source),
                    Some(RecursionFlags::Both),
                    None,
                    None,
                    None,
                ) != Ternary::False
            {
                if !self
                    .type_checker
                    .get_mapped_type_modifiers(&source)
                    .intersects(MappedTypeModifiers::IncludeOptional)
                {
                    let template_type = self
                        .type_checker
                        .get_template_type_from_mapped_type(&source);
                    let indexed_access_type = self.type_checker.get_indexed_access_type(
                        target,
                        &self
                            .type_checker
                            .get_type_parameter_from_mapped_type(&source),
                        None,
                        Option::<&Node>::None,
                        Option::<&Symbol>::None,
                        None,
                    );
                    result = self.is_related_to(
                        &template_type,
                        &indexed_access_type,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        None,
                    );
                    if result != Ternary::False {
                        return result;
                    }
                }
            }
        } else if target.flags().intersects(TypeFlags::Index) {
            let target_type = &target.as_index_type().type_;
            if source.flags().intersects(TypeFlags::Index) {
                result = self.is_related_to(
                    target_type,
                    &source.as_index_type().type_,
                    Some(RecursionFlags::Both),
                    Some(false),
                    None,
                    None,
                );
                if result != Ternary::False {
                    return result;
                }
            }
            if self.type_checker.is_tuple_type(target_type) {
                result = self.is_related_to(
                    &source,
                    &self.type_checker.get_known_keys_of_tuple_type(target_type),
                    Some(RecursionFlags::Target),
                    Some(report_errors),
                    None,
                    None,
                );
                if result != Ternary::False {
                    return result;
                }
            } else {
                let constraint = self
                    .type_checker
                    .get_simplified_type_or_constraint(target_type);
                if let Some(constraint) = constraint.as_ref() {
                    if self.is_related_to(
                        &source,
                        &self.type_checker.get_index_type(
                            constraint,
                            Some(target.as_index_type().strings_only),
                            None,
                        ),
                        Some(RecursionFlags::Target),
                        Some(report_errors),
                        None,
                        None,
                    ) == Ternary::True
                    {
                        return Ternary::True;
                    }
                } else if self.type_checker.is_generic_mapped_type(target_type) {
                    let name_type = self
                        .type_checker
                        .get_name_type_from_mapped_type(target_type);
                    let constraint_type = self
                        .type_checker
                        .get_constraint_type_from_mapped_type(target_type);
                    let target_keys: Rc<Type>;
                    if let Some(name_type) = name_type.as_ref().filter(|name_type| {
                        self.type_checker
                            .is_mapped_type_with_keyof_constraint_declaration(target_type)
                    }) {
                        let modifiers_type = self.type_checker.get_apparent_type(
                            &self
                                .type_checker
                                .get_modifiers_type_from_mapped_type(target_type),
                        );
                        let mut mapped_keys: Vec<Rc<Type>> = vec![];
                        self.type_checker
                            .for_each_mapped_type_property_key_type_and_index_signature_key_type(
                                &modifiers_type,
                                TypeFlags::StringOrNumberLiteralOrUnique,
                                false,
                                |t| {
                                    mapped_keys.push(
                                        self.type_checker.instantiate_type(
                                            name_type,
                                            Some(
                                                &self.type_checker.append_type_mapping(
                                                    target_type
                                                        .as_mapped_type()
                                                        .maybe_mapper()
                                                        .map(Clone::clone),
                                                    &self
                                                        .type_checker
                                                        .get_type_parameter_from_mapped_type(
                                                            target_type,
                                                        ),
                                                    t,
                                                ),
                                            ),
                                        ),
                                    );
                                },
                            );
                        mapped_keys.push(name_type.clone());
                        target_keys = self.type_checker.get_union_type(
                            mapped_keys,
                            None,
                            Option::<&Symbol>::None,
                            None,
                            Option::<&Type>::None,
                        );
                    } else {
                        target_keys = name_type.unwrap_or(constraint_type);
                    }
                    if self.is_related_to(
                        &source,
                        &target_keys,
                        Some(RecursionFlags::Target),
                        Some(report_errors),
                        None,
                        None,
                    ) == Ternary::True
                    {
                        return Ternary::True;
                    }
                }
            }
        } else if target.flags().intersects(TypeFlags::IndexedAccess) {
            if source.flags().intersects(TypeFlags::IndexedAccess) {
                result = self.is_related_to(
                    &source.as_indexed_access_type().object_type,
                    &target.as_indexed_access_type().object_type,
                    Some(RecursionFlags::Both),
                    Some(report_errors),
                    None,
                    None,
                );
                if result != Ternary::False {
                    result &= self.is_related_to(
                        &source.as_indexed_access_type().index_type,
                        &target.as_indexed_access_type().index_type,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        None,
                    );
                }
                if result != Ternary::False {
                    self.reset_error_info(save_error_info);
                    return result;
                }
                if report_errors {
                    original_error_info = self.maybe_error_info().clone();
                }
            }
            if Rc::ptr_eq(&self.relation, &self.type_checker.assignable_relation)
                || Rc::ptr_eq(&self.relation, &self.type_checker.comparable_relation)
            {
                let object_type = &target.as_indexed_access_type().object_type;
                let index_type = &target.as_indexed_access_type().index_type;
                let base_object_type = self
                    .type_checker
                    .get_base_constraint_of_type(object_type)
                    .unwrap_or_else(|| object_type.clone());
                let base_index_type = self
                    .type_checker
                    .get_base_constraint_of_type(index_type)
                    .unwrap_or_else(|| index_type.clone());
                if !self.type_checker.is_generic_object_type(&base_object_type)
                    && !self.type_checker.is_generic_index_type(&base_index_type)
                {
                    let access_flags = AccessFlags::Writing
                        | if !Rc::ptr_eq(&base_object_type, object_type) {
                            AccessFlags::NoIndexSignatures
                        } else {
                            AccessFlags::None
                        };
                    let constraint = self.type_checker.get_indexed_access_type_or_undefined(
                        &base_object_type,
                        &base_index_type,
                        Some(access_flags),
                        Option::<&Node>::None,
                        Option::<&Symbol>::None,
                        None,
                    );
                    if let Some(constraint) = constraint.as_ref() {
                        if report_errors && original_error_info.is_some() {
                            self.reset_error_info(save_error_info);
                        }
                        result = self.is_related_to(
                            &source,
                            constraint,
                            Some(RecursionFlags::Target),
                            Some(report_errors),
                            None,
                            None,
                        );
                        if result != Ternary::False {
                            return result;
                        }
                        if report_errors {
                            if let Some(original_error_info) = original_error_info.clone() {
                                if let Some(error_info) = self.maybe_error_info().clone() {
                                    *self.maybe_error_info() = Some(
                                        if self.count_message_chain_breadth(Some(&vec![
                                            &original_error_info,
                                        ])) <= self
                                            .count_message_chain_breadth(Some(&vec![&error_info]))
                                        {
                                            original_error_info
                                        } else {
                                            error_info
                                        },
                                    );
                                }
                            }
                        }
                    }
                }
            }
            if report_errors {
                original_error_info = None;
            }
        } else if self.type_checker.is_generic_mapped_type(target)
            && !Rc::ptr_eq(&self.relation, &self.type_checker.identity_relation)
        {
            let keys_remapped = target
                .as_mapped_type()
                .declaration
                .as_mapped_type_node()
                .name_type
                .is_some();
            let template_type = self.type_checker.get_template_type_from_mapped_type(target);
            let modifiers = self.type_checker.get_mapped_type_modifiers(target);
            if !modifiers.intersects(MappedTypeModifiers::ExcludeOptional) {
                if !keys_remapped
                    && template_type.flags().intersects(TypeFlags::IndexedAccess)
                    && Rc::ptr_eq(&template_type.as_indexed_access_type().object_type, &source)
                    && Rc::ptr_eq(
                        &template_type.as_indexed_access_type().index_type,
                        &self
                            .type_checker
                            .get_type_parameter_from_mapped_type(target),
                    )
                {
                    return Ternary::True;
                }
                if !self.type_checker.is_generic_mapped_type(&source) {
                    let target_keys = if keys_remapped {
                        self.type_checker
                            .get_name_type_from_mapped_type(target)
                            .unwrap()
                    } else {
                        self.type_checker
                            .get_constraint_type_from_mapped_type(target)
                    };
                    let source_keys = self.type_checker.get_index_type(&source, None, Some(true));
                    let include_optional =
                        modifiers.intersects(MappedTypeModifiers::IncludeOptional);
                    let filtered_by_applicability = if include_optional {
                        self.type_checker
                            .intersect_types(Some(&*target_keys), Some(&*source_keys))
                    } else {
                        None
                    };
                    if if include_optional {
                        !filtered_by_applicability
                            .as_ref()
                            .unwrap()
                            .flags()
                            .intersects(TypeFlags::Never)
                    } else {
                        self.is_related_to(
                            &target_keys,
                            &source_keys,
                            Some(RecursionFlags::Both),
                            None,
                            None,
                            None,
                        ) != Ternary::False
                    } {
                        let template_type =
                            self.type_checker.get_template_type_from_mapped_type(target);
                        let type_parameter = self
                            .type_checker
                            .get_type_parameter_from_mapped_type(target);

                        let non_null_component = self
                            .type_checker
                            .extract_types_of_kind(&template_type, !TypeFlags::Nullable);
                        if !keys_remapped
                            && non_null_component
                                .flags()
                                .intersects(TypeFlags::IndexedAccess)
                            && Rc::ptr_eq(
                                &non_null_component.as_indexed_access_type().index_type,
                                &type_parameter,
                            )
                        {
                            result = self.is_related_to(
                                &source,
                                &non_null_component.as_indexed_access_type().object_type,
                                Some(RecursionFlags::Target),
                                Some(report_errors),
                                None,
                                None,
                            );
                            if result != Ternary::False {
                                return result;
                            }
                        } else {
                            let indexing_type = if keys_remapped {
                                filtered_by_applicability.unwrap_or(target_keys)
                            } else if let Some(filtered_by_applicability) =
                                filtered_by_applicability
                            {
                                self.type_checker.get_intersection_type(
                                    &vec![filtered_by_applicability, type_parameter],
                                    Option::<&Symbol>::None,
                                    None,
                                )
                            } else {
                                type_parameter
                            };
                            let indexed_access_type = self.type_checker.get_indexed_access_type(
                                &source,
                                &indexing_type,
                                None,
                                Option::<&Node>::None,
                                Option::<&Symbol>::None,
                                None,
                            );
                            result = self.is_related_to(
                                &indexed_access_type,
                                &template_type,
                                Some(RecursionFlags::Both),
                                Some(report_errors),
                                None,
                                None,
                            );
                            if result != Ternary::False {
                                return result;
                            }
                        }
                    }
                    original_error_info = self.maybe_error_info().clone();
                    self.reset_error_info(save_error_info);
                }
            }
        } else if target.flags().intersects(TypeFlags::Conditional) {
            if self.type_checker.is_deeply_nested_type(
                target,
                self.maybe_target_stack().as_ref(),
                self.target_depth(),
                Some(10),
            ) {
                self.reset_error_info(save_error_info);
                return Ternary::Maybe;
            }
            let c = target;
            let c_as_conditional_type = c.as_conditional_type();
            if c_as_conditional_type.root.infer_type_parameters.is_none()
                && !self
                    .type_checker
                    .is_distribution_dependent(&c_as_conditional_type.root)
            {
                let skip_true = !self.type_checker.is_type_assignable_to(
                    &self
                        .type_checker
                        .get_permissive_instantiation(&c_as_conditional_type.check_type),
                    &self
                        .type_checker
                        .get_permissive_instantiation(&c_as_conditional_type.extends_type),
                );
                let skip_false = !skip_true
                    && self.type_checker.is_type_assignable_to(
                        &self
                            .type_checker
                            .get_restrictive_instantiation(&c_as_conditional_type.check_type),
                        &self
                            .type_checker
                            .get_restrictive_instantiation(&c_as_conditional_type.extends_type),
                    );
                result = if skip_true {
                    Ternary::True
                } else {
                    self.is_related_to(
                        &source,
                        &self.type_checker.get_true_type_from_conditional_type(c),
                        Some(RecursionFlags::Target),
                        Some(false),
                        None,
                        None,
                    )
                };
                if result != Ternary::False {
                    result &= if skip_false {
                        Ternary::True
                    } else {
                        self.is_related_to(
                            &source,
                            &self.type_checker.get_false_type_from_conditional_type(c),
                            Some(RecursionFlags::Target),
                            Some(false),
                            None,
                            None,
                        )
                    };
                    if result != Ternary::False {
                        self.reset_error_info(save_error_info);
                        return result;
                    }
                }
            }
        } else if target.flags().intersects(TypeFlags::TemplateLiteral) {
        }

        if false {
            unimplemented!()
        } else {
            if source
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::Intersection)
                && target.flags().intersects(TypeFlags::Object)
            {
                let report_structural_errors = report_errors && true;
                result = self.properties_related_to(
                    &source,
                    target,
                    report_structural_errors,
                    None,
                    intersection_state,
                );
                if false && result != Ternary::False {
                    unimplemented!()
                } else if result != Ternary::False {
                    return result;
                }
            }
        }

        Ternary::False
    }

    pub(super) fn count_message_chain_breadth(
        &self,
        info: Option<&[&DiagnosticMessageChain]>,
    ) -> usize {
        unimplemented!()
    }

    pub(super) fn relate_variances(
        &self,
        source_type_arguments: Option<&[Rc<Type>]>,
        target_type_arguments: Option<&[Rc<Type>]>,
        variances: &[VarianceFlags],
        intersection_state: IntersectionState,
    ) -> Option<Ternary> {
        unimplemented!()
    }

    pub(super) fn exclude_properties(
        &self,
        properties: Vec<Rc<Symbol>>,
        excluded_properties: Option<HashSet<__String>>,
    ) -> Vec<Rc<Symbol>> {
        properties
    }

    pub(super) fn is_property_symbol_type_related(
        &self,
        source_prop: &Symbol,
        target_prop: &Symbol,
        get_type_of_source_property: fn(&TypeChecker, &Symbol) -> Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let target_is_optional = false;
        let effective_target = self.type_checker.add_optionality(
            &self
                .type_checker
                .get_non_missing_type_of_symbol(target_prop),
            Some(false),
            Some(target_is_optional),
        );
        let effective_source = get_type_of_source_property(self.type_checker, source_prop);
        self.is_related_to(
            &effective_source,
            &effective_target,
            Some(RecursionFlags::Both),
            Some(report_errors),
            None,
            Some(intersection_state),
        )
    }

    pub(super) fn property_related_to(
        &self,
        source: &Type,
        target: &Type,
        source_prop: &Symbol,
        target_prop: &Symbol,
        get_type_of_source_property: fn(&TypeChecker, &Symbol) -> Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
        skip_optional: bool,
    ) -> Ternary {
        let related = self.is_property_symbol_type_related(
            source_prop,
            target_prop,
            get_type_of_source_property,
            report_errors,
            intersection_state,
        );
        if related == Ternary::False {
            if report_errors {
                self.report_incompatible_error(
                    &Diagnostics::Types_of_property_0_are_incompatible,
                    Some(vec![self.type_checker.symbol_to_string_(
                        target_prop,
                        Option::<&Node>::None,
                        None,
                        None,
                        None,
                    )]),
                );
            }
            return Ternary::False;
        }
        related
    }

    pub(super) fn properties_related_to(
        &self,
        source: &Type,
        target: &Type,
        report_errors: bool,
        excluded_properties: Option<HashSet<__String>>,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let mut result = Ternary::True;
        let require_optional_properties = false;
        // let unmatched_property =
        //     self.get_unmatched_property(source, target, require_optional_properties, false);
        // if let Some(unmatched_property) = unmatched_property {
        //     if report_errors {
        //         self.report_unmatched_property(
        //             source,
        //             target,
        //             unmatched_property,
        //             require_optional_properties,
        //         );
        //     }
        //     return Ternary::False;
        // }
        let properties = self.type_checker.get_properties_of_type(target);
        for target_prop in self.exclude_properties(properties, excluded_properties) {
            let name = target_prop.escaped_name();
            if true {
                let source_prop = self.type_checker.get_property_of_type_(source, name, None);
                if let Some(source_prop) = source_prop {
                    if !Rc::ptr_eq(&source_prop, &target_prop) {
                        let related = self.property_related_to(
                            source,
                            target,
                            &source_prop,
                            &target_prop,
                            TypeChecker::get_non_missing_type_of_symbol,
                            report_errors,
                            intersection_state,
                            true,
                        );
                        if related == Ternary::False {
                            return Ternary::False;
                        }
                        result &= related;
                    }
                }
            }
        }
        result
    }
}

struct ReportUnmeasurableMarkers;
impl TypeMapperCallback for ReportUnmeasurableMarkers {
    fn call(&self, checker: &TypeChecker, p: &Type /*TypeParameter*/) -> Rc<Type> {
        if
        /*(outofbandVarianceMarkerHandler*/
        false
            && (ptr::eq(p, &*checker.marker_super_type())
                || ptr::eq(p, &*checker.marker_sub_type())
                || ptr::eq(p, &*checker.marker_other_type()))
        {
            // outofbandVarianceMarkerHandler(/*onlyUnreliable*/ false);
        }
        p.type_wrapper()
    }
}

struct ReportUnreliableMarkers;
impl TypeMapperCallback for ReportUnreliableMarkers {
    fn call(&self, checker: &TypeChecker, p: &Type /*TypeParameter*/) -> Rc<Type> {
        if
        /*(outofbandVarianceMarkerHandler*/
        false
            && (ptr::eq(p, &*checker.marker_super_type())
                || ptr::eq(p, &*checker.marker_sub_type())
                || ptr::eq(p, &*checker.marker_other_type()))
        {
            // outofbandVarianceMarkerHandler(/*onlyUnreliable*/ true);
        }
        p.type_wrapper()
    }
}
