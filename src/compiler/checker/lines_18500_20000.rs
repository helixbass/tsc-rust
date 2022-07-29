#![allow(non_upper_case_globals)]

use regex::{Captures, Regex};
use std::cell::RefMut;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ptr;
use std::rc::Rc;

use super::{
    CheckTypeContainingMessageChain, CheckTypeRelatedTo, ExpandingFlags, IntersectionState,
    RecursionFlags,
};
use crate::{
    Diagnostics, Node, NodeInterface, RelationComparisonResult, Symbol, SymbolInterface, Ternary,
    Type, TypeChecker, TypeFlags, TypeInterface, TypeMapperCallback,
    UnionOrIntersectionTypeInterface, VarianceFlags, __String,
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
        let result = self.structured_type_related_to_worker(
            source,
            target,
            report_errors,
            intersection_state,
        );
        result
    }

    pub(super) fn structured_type_related_to_worker(
        &self,
        source: &Type,
        target: &Type,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if intersection_state.intersects(IntersectionState::UnionIntersectionCheck) {
            if source.flags().intersects(TypeFlags::Union) {
                return if false {
                    unimplemented!()
                } else {
                    self.each_type_related_to_type(
                        source,
                        target,
                        report_errors,
                        intersection_state & !IntersectionState::UnionIntersectionCheck,
                    )
                };
            }
            if target.flags().intersects(TypeFlags::Union) {
                return self.type_related_to_some_type(
                    &self.type_checker.get_regular_type_of_object_literal(source),
                    target,
                    report_errors
                        && !(source.flags().intersects(TypeFlags::Primitive))
                        && !(target.flags().intersects(TypeFlags::Primitive)),
                );
            }
            unimplemented!()
        }

        let result: Ternary;

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
                    source,
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
