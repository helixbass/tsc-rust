use gc::{Finalize, Gc, Trace};
use regex::{Captures, Regex};
use std::ptr;
use std::rc::Rc;
use std::{cell::Cell, io};

use super::{
    CheckTypeRelatedTo, ExpandingFlags, IntersectionState, MappedTypeModifiers, RecursionFlags,
};
use crate::{
    are_gc_slices_equal, are_option_rcs_equal, are_rc_slices_equal, get_object_flags, same_map,
    try_map, AccessFlags, DiagnosticMessageChain, InferenceFlags, InferencePriority, Node,
    NodeInterface, ObjectFlags, ObjectTypeInterface, OutofbandVarianceMarkerHandler,
    RelationComparisonResult, Signature, SignatureKind, Symbol, SymbolInterface, Ternary, Type,
    TypeChecker, TypeComparer, TypeFlags, TypeInterface, TypeMapper, TypeMapperCallback,
    UnionOrIntersectionTypeInterface, VarianceFlags,
};

impl CheckTypeRelatedTo {
    pub(super) fn should_check_as_excess_property(
        &self,
        prop: &Symbol,
        container: &Symbol,
    ) -> bool {
        if let Some(prop_value_declaration) = prop.maybe_value_declaration().as_ref() {
            if let Some(container_value_declaration) = container.maybe_value_declaration().as_ref()
            {
                return Gc::ptr_eq(
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
    ) -> io::Result<Ternary> {
        let mut result = Ternary::True;
        let source_types = source.as_union_or_intersection_type_interface().types();
        for source_type in source_types {
            let related = self.type_related_to_some_type(source_type, target, false)?;
            if related == Ternary::False {
                return Ok(Ternary::False);
            }
            result &= related;
        }
        Ok(result)
    }

    pub(super) fn type_related_to_some_type(
        &self,
        source: &Type,
        target: &Type, /*UnionOrIntersectionType*/
        report_errors: bool,
    ) -> io::Result<Ternary> {
        let target_types = target.as_union_or_intersection_type_interface().types();
        if target.flags().intersects(TypeFlags::Union) {
            if self.type_checker.contains_type(target_types, source) {
                return Ok(Ternary::True);
            }
            let match_ = self
                .type_checker
                .get_matching_union_constituent_for_type(target, source)?;
            if let Some(match_) = match_.as_ref() {
                let related = self.is_related_to(
                    source,
                    match_,
                    Some(RecursionFlags::Target),
                    Some(false),
                    None,
                    None,
                )?;
                if related != Ternary::False {
                    return Ok(related);
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
            )?;
            if related != Ternary::False {
                return Ok(related);
            }
        }
        if report_errors {
            let best_matching_type = self.type_checker.get_best_matching_type(
                source,
                target,
                Some(|source: &Type, target: &Type| {
                    self.is_related_to(source, target, None, None, None, None)
                }),
            )?;
            self.is_related_to(
                source,
                &best_matching_type.unwrap_or_else(|| target_types[target_types.len() - 1].clone()),
                Some(RecursionFlags::Target),
                Some(true),
                None,
                None,
            );
        }
        Ok(Ternary::False)
    }

    pub(super) fn type_related_to_each_type(
        &self,
        source: &Type,
        target: &Type, /*IntersectionType*/
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
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
            )?;
            if related == Ternary::False {
                return Ok(Ternary::False);
            }
            result &= related;
        }
        Ok(result)
    }

    pub(super) fn some_type_related_to_type(
        &self,
        source: &Type, /*UnionOrIntersectionType*/
        target: &Type,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        let source_types = source.as_union_or_intersection_type_interface().types();
        if source.flags().intersects(TypeFlags::Union)
            && self.type_checker.contains_type(source_types, target)
        {
            return Ok(Ternary::True);
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
            )?;
            if related != Ternary::False {
                return Ok(related);
            }
        }
        Ok(Ternary::False)
    }

    pub(super) fn get_undefined_stripped_target_if_needed(
        &self,
        source: &Type,
        target: &Type,
    ) -> Gc<Type> {
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
    ) -> io::Result<Ternary> {
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
                    )?;
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
            )?;
            if related == Ternary::False {
                return Ok(Ternary::False);
            }
            result &= related;
        }
        Ok(result)
    }

    pub(super) fn type_arguments_related_to(
        &self,
        sources: Option<Vec<Gc<Type>>>,
        targets: Option<Vec<Gc<Type>>>,
        variances: Option<Vec<VarianceFlags>>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        let sources = sources.unwrap_or_else(|| vec![]);
        let targets = targets.unwrap_or_else(|| vec![]);
        let variances = variances.unwrap_or_else(|| vec![]);
        if sources.len() != targets.len()
            && Rc::ptr_eq(&self.relation, &self.type_checker.identity_relation)
        {
            return Ok(Ternary::False);
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
                let mut related: Ternary/* = Ternary::True*/;
                if variance_flags.intersects(VarianceFlags::Unmeasurable) {
                    related = if Rc::ptr_eq(&self.relation, &self.type_checker.identity_relation) {
                        self.is_related_to(
                            s,
                            t,
                            Some(RecursionFlags::Both),
                            Some(false),
                            None,
                            None,
                        )?
                    } else {
                        self.type_checker.compare_types_identical(s, t)?
                    };
                } else if variance == VarianceFlags::Covariant {
                    related = self.is_related_to(
                        s,
                        t,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        Some(intersection_state),
                    )?;
                } else if variance == VarianceFlags::Contravariant {
                    related = self.is_related_to(
                        t,
                        s,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        Some(intersection_state),
                    )?;
                } else if variance == VarianceFlags::Bivariant {
                    related = self.is_related_to(
                        t,
                        s,
                        Some(RecursionFlags::Both),
                        Some(false),
                        None,
                        None,
                    )?;
                    if related == Ternary::False {
                        related = self.is_related_to(
                            s,
                            t,
                            Some(RecursionFlags::Both),
                            Some(report_errors),
                            None,
                            Some(intersection_state),
                        )?;
                    }
                } else {
                    related = self.is_related_to(
                        s,
                        t,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        Some(intersection_state),
                    )?;
                    if related != Ternary::False {
                        related &= self.is_related_to(
                            t,
                            s,
                            Some(RecursionFlags::Both),
                            Some(report_errors),
                            None,
                            Some(intersection_state),
                        )?;
                    }
                }
                if related == Ternary::False {
                    return Ok(Ternary::False);
                }
                result &= related;
            }
        }
        Ok(result)
    }

    pub(super) fn recursive_type_related_to(
        &self,
        source: &Type,
        target: &Type,
        report_errors: bool,
        intersection_state: IntersectionState,
        recursion_flags: RecursionFlags,
    ) -> io::Result<Ternary> {
        if self.overflow() {
            return Ok(Ternary::False);
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
        )?;
        let entry = (*self.relation).borrow().get(&id).map(Clone::clone);
        if let Some(entry) = entry.as_ref() {
            if report_errors
                && entry.intersects(RelationComparisonResult::Failed)
                && !entry.intersects(RelationComparisonResult::Reported)
            {
            } else {
                if self
                    .type_checker
                    .maybe_outofband_variance_marker_handler()
                    .is_some()
                {
                    let saved = *entry & RelationComparisonResult::ReportsMask;
                    if saved.intersects(RelationComparisonResult::ReportsUnmeasurable) {
                        self.type_checker.instantiate_type(
                            source,
                            Some(Gc::new(
                                self.type_checker
                                    .make_function_type_mapper(ReportUnmeasurableMarkers),
                            )),
                        );
                    }
                    if saved.intersects(RelationComparisonResult::ReportsUnreliable) {
                        self.type_checker.instantiate_type(
                            source,
                            Some(Gc::new(
                                self.type_checker
                                    .make_function_type_mapper(ReportUnreliableMarkers),
                            )),
                        );
                    }
                }
                return Ok(if entry.intersects(RelationComparisonResult::Succeeded) {
                    Ternary::True
                } else {
                    Ternary::False
                });
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
                    return Ok(Ternary::Maybe);
                }
            }
            if self.source_depth() == 100 || self.target_depth() == 100 {
                self.set_overflow(true);
                return Ok(Ternary::False);
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
        let mut original_handler: Option<Gc<Box<dyn OutofbandVarianceMarkerHandler>>> = None;
        let propagating_variance_flags: Rc<Cell<RelationComparisonResult>> =
            Rc::new(Cell::new(RelationComparisonResult::None));
        if let Some(outofband_variance_marker_handler) =
            self.type_checker.maybe_outofband_variance_marker_handler()
        {
            original_handler = Some(outofband_variance_marker_handler);
            self.type_checker
                .set_outofband_variance_marker_handler(Some(Gc::new(Box::new(
                    RecursiveTypeRelatedToOutofbandVarianceMarkerHandler::new(
                        propagating_variance_flags.clone(),
                        original_handler.clone().unwrap(),
                    ),
                ))));
        }

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
            self.structured_type_related_to(source, target, report_errors, intersection_state)?
        } else {
            Ternary::Maybe
        };
        if self
            .type_checker
            .maybe_outofband_variance_marker_handler()
            .is_some()
        {
            self.type_checker
                .set_outofband_variance_marker_handler(original_handler);
        }
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
                            RelationComparisonResult::Succeeded | propagating_variance_flags.get(),
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
                    | propagating_variance_flags.get(),
            );
            self.set_maybe_count(maybe_start);
            self.maybe_keys()
                .as_mut()
                .unwrap()
                .truncate(self.maybe_count());
        }
        Ok(result)
    }

    pub(super) fn structured_type_related_to(
        &self,
        source: &Type,
        target: &Type,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        // tracing?.push(tracing.Phase.CheckTypes, "structuredTypeRelatedTo", { sourceId: source.id, targetId: target.id });
        let result = self.structured_type_related_to_worker(
            source,
            target,
            report_errors,
            intersection_state,
        )?;
        // tracing?.pop();
        Ok(result)
    }

    pub(super) fn structured_type_related_to_worker(
        &self,
        source: &Type,
        target: &Type,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
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
                return Ok(
                    if Rc::ptr_eq(&self.relation, &self.type_checker.comparable_relation) {
                        self.some_type_related_to_type(
                            &source,
                            target,
                            report_errors && !source.flags().intersects(TypeFlags::Primitive),
                            intersection_state & !IntersectionState::UnionIntersectionCheck,
                        )?
                    } else {
                        self.each_type_related_to_type(
                            &source,
                            target,
                            report_errors && !source.flags().intersects(TypeFlags::Primitive),
                            intersection_state & !IntersectionState::UnionIntersectionCheck,
                        )?
                    },
                );
            }
            if target.flags().intersects(TypeFlags::Union) {
                return self.type_related_to_some_type(
                    &*self
                        .type_checker
                        .get_regular_type_of_object_literal(&source)?,
                    target,
                    report_errors
                        && !source.flags().intersects(TypeFlags::Primitive)
                        && !target.flags().intersects(TypeFlags::Primitive),
                );
            }
            if target.flags().intersects(TypeFlags::Intersection) {
                return self.type_related_to_each_type(
                    &*self
                        .type_checker
                        .get_regular_type_of_object_literal(&source)?,
                    target,
                    report_errors,
                    IntersectionState::Target,
                );
            }
            if Rc::ptr_eq(&self.relation, &self.type_checker.comparable_relation)
                && target.flags().intersects(TypeFlags::Primitive)
            {
                let constraints = try_map(
                    source.as_union_or_intersection_type_interface().types(),
                    |type_: &Gc<Type>, _| self.type_checker.get_base_constraint_or_type(type_),
                )?;
                if !are_gc_slices_equal(
                    &constraints,
                    source.as_union_or_intersection_type_interface().types(),
                ) {
                    source = self.type_checker.get_intersection_type(
                        &constraints,
                        Option::<&Symbol>::None,
                        None,
                    )?;
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
            let mut result: Ternary;
            if flags.intersects(TypeFlags::IndexedAccess) {
                result = self.is_related_to(
                    &source.as_indexed_access_type().object_type,
                    &target.as_indexed_access_type().object_type,
                    Some(RecursionFlags::Both),
                    Some(false),
                    None,
                    None,
                )?;
                if result != Ternary::False {
                    result &= self.is_related_to(
                        &source.as_indexed_access_type().index_type,
                        &target.as_indexed_access_type().index_type,
                        Some(RecursionFlags::Both),
                        Some(false),
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        return Ok(result);
                    }
                }
            }
            if flags.intersects(TypeFlags::Conditional) {
                if (*source.as_conditional_type().root)
                    .borrow()
                    .is_distributive
                    == (*target.as_conditional_type().root)
                        .borrow()
                        .is_distributive
                {
                    result = self.is_related_to(
                        &source.as_conditional_type().check_type,
                        &target.as_conditional_type().check_type,
                        Some(RecursionFlags::Both),
                        Some(false),
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        result &= self.is_related_to(
                            &source.as_conditional_type().extends_type,
                            &target.as_conditional_type().extends_type,
                            Some(RecursionFlags::Both),
                            Some(false),
                            None,
                            None,
                        )?;
                        if result != Ternary::False {
                            result &= self.is_related_to(
                                &*self
                                    .type_checker
                                    .get_true_type_from_conditional_type(&source)?,
                                &*self
                                    .type_checker
                                    .get_true_type_from_conditional_type(target)?,
                                Some(RecursionFlags::Both),
                                Some(false),
                                None,
                                None,
                            )?;
                            if result != Ternary::False {
                                result &= self.is_related_to(
                                    &*self
                                        .type_checker
                                        .get_false_type_from_conditional_type(&source)?,
                                    &*self
                                        .type_checker
                                        .get_false_type_from_conditional_type(target)?,
                                    Some(RecursionFlags::Both),
                                    Some(false),
                                    None,
                                    None,
                                )?;
                                if result != Ternary::False {
                                    return Ok(result);
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
            return Ok(Ternary::False);
        }

        // The Typescript version doesn't initialize result but otherwise we'd run into type-checking issues
        let mut result = Ternary::False;
        let mut original_error_info: Option<Rc<DiagnosticMessageChain>> = None;
        let mut variance_check_failed = false;
        let save_error_info = self.capture_error_calculation_state();

        if source
            .flags()
            .intersects(TypeFlags::Object | TypeFlags::Conditional)
        {
            if let Some(source_alias_symbol) = source.maybe_alias_symbol().as_ref() {
                if let Some(source_alias_type_arguments) =
                    source.maybe_alias_type_arguments().as_ref()
                {
                    if matches!(
                        target.maybe_alias_symbol().as_ref(),
                        Some(target_alias_symbol) if Gc::ptr_eq(
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
                            let variances = self
                                .type_checker
                                .get_alias_variances(&source_alias_symbol)?;
                            if variances.is_empty() {
                                return Ok(Ternary::Unknown);
                            }
                            let variance_result = self.relate_variances(
                                &mut result,
                                report_errors,
                                &mut original_error_info,
                                &save_error_info,
                                &mut variance_check_failed,
                                Some(source_alias_type_arguments),
                                target.maybe_alias_type_arguments().as_deref(),
                                &variances,
                                intersection_state,
                            )?;
                            if let Some(variance_result) = variance_result {
                                return Ok(variance_result);
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
        {
            result = self.is_related_to(
                &self.type_checker.get_type_arguments(&source)?[0],
                target,
                Some(RecursionFlags::Source),
                None,
                None,
                None,
            )?;
            if result != Ternary::False {
                return Ok(result);
            }
        }
        if self
            .type_checker
            .is_single_element_generic_tuple_type(target)
            && (target.as_type_reference().target.as_tuple_type().readonly
                || self.type_checker.is_mutable_array_or_tuple(
                    &self
                        .type_checker
                        .get_base_constraint_of_type(&source)?
                        .unwrap_or_else(|| source.clone()),
                ))
        {
            result = self.is_related_to(
                &source,
                &self.type_checker.get_type_arguments(target)?[0],
                Some(RecursionFlags::Target),
                None,
                None,
                None,
            )?;
            if result != Ternary::False {
                return Ok(result);
            }
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
                    &*self.type_checker.get_index_type(target, None, None)?,
                    &*self
                        .type_checker
                        .get_constraint_type_from_mapped_type(&source)?,
                    Some(RecursionFlags::Both),
                    None,
                    None,
                    None,
                )? != Ternary::False
            {
                if !self
                    .type_checker
                    .get_mapped_type_modifiers(&source)
                    .intersects(MappedTypeModifiers::IncludeOptional)
                {
                    let template_type = self
                        .type_checker
                        .get_template_type_from_mapped_type(&source)?;
                    let indexed_access_type = self.type_checker.get_indexed_access_type(
                        target,
                        &*self
                            .type_checker
                            .get_type_parameter_from_mapped_type(&source)?,
                        None,
                        Option::<&Node>::None,
                        Option::<&Symbol>::None,
                        None,
                    )?;
                    result = self.is_related_to(
                        &template_type,
                        &indexed_access_type,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        return Ok(result);
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
                )?;
                if result != Ternary::False {
                    return Ok(result);
                }
            }
            if self.type_checker.is_tuple_type(target_type) {
                result = self.is_related_to(
                    &source,
                    &*self
                        .type_checker
                        .get_known_keys_of_tuple_type(target_type)?,
                    Some(RecursionFlags::Target),
                    Some(report_errors),
                    None,
                    None,
                )?;
                if result != Ternary::False {
                    return Ok(result);
                }
            } else {
                let constraint = self
                    .type_checker
                    .get_simplified_type_or_constraint(target_type)?;
                if let Some(constraint) = constraint.as_ref() {
                    if self.is_related_to(
                        &source,
                        &*self.type_checker.get_index_type(
                            constraint,
                            Some(target.as_index_type().strings_only),
                            None,
                        )?,
                        Some(RecursionFlags::Target),
                        Some(report_errors),
                        None,
                        None,
                    )? == Ternary::True
                    {
                        return Ok(Ternary::True);
                    }
                } else if self.type_checker.is_generic_mapped_type(target_type)? {
                    let name_type = self
                        .type_checker
                        .get_name_type_from_mapped_type(target_type)?;
                    let constraint_type = self
                        .type_checker
                        .get_constraint_type_from_mapped_type(target_type)?;
                    let target_keys: Gc<Type>;
                    if let Some(name_type) = name_type.as_ref().filter(|_| {
                        self.type_checker
                            .is_mapped_type_with_keyof_constraint_declaration(target_type)
                    }) {
                        let modifiers_type = self.type_checker.get_apparent_type(
                            &*self
                                .type_checker
                                .get_modifiers_type_from_mapped_type(target_type)?,
                        )?;
                        let mut mapped_keys: Vec<Gc<Type>> = vec![];
                        self.type_checker
                            .try_for_each_mapped_type_property_key_type_and_index_signature_key_type(
                                &modifiers_type,
                                TypeFlags::StringOrNumberLiteralOrUnique,
                                false,
                                |t| {
                                    mapped_keys.push(
                                        self.type_checker.instantiate_type(
                                            name_type,
                                            Some(Gc::new(
                                                self.type_checker.append_type_mapping(
                                                    target_type.as_mapped_type().maybe_mapper(),
                                                    &*self
                                                        .type_checker
                                                        .get_type_parameter_from_mapped_type(
                                                            target_type,
                                                        )?,
                                                    t,
                                                ),
                                            )),
                                        )?,
                                    );

                                    Ok(())
                                },
                            )?;
                        mapped_keys.push(name_type.clone());
                        target_keys = self.type_checker.get_union_type(
                            &mapped_keys,
                            None,
                            Option::<&Symbol>::None,
                            None,
                            Option::<&Type>::None,
                        )?;
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
                    )? == Ternary::True
                    {
                        return Ok(Ternary::True);
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
                )?;
                if result != Ternary::False {
                    result &= self.is_related_to(
                        &source.as_indexed_access_type().index_type,
                        &target.as_indexed_access_type().index_type,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        None,
                    )?;
                }
                if result != Ternary::False {
                    self.reset_error_info(save_error_info);
                    return Ok(result);
                }
                if report_errors {
                    original_error_info = self.maybe_error_info();
                }
            }
            if Rc::ptr_eq(&self.relation, &self.type_checker.assignable_relation)
                || Rc::ptr_eq(&self.relation, &self.type_checker.comparable_relation)
            {
                let object_type = &target.as_indexed_access_type().object_type;
                let index_type = &target.as_indexed_access_type().index_type;
                let base_object_type = self
                    .type_checker
                    .get_base_constraint_of_type(object_type)?
                    .unwrap_or_else(|| object_type.clone());
                let base_index_type = self
                    .type_checker
                    .get_base_constraint_of_type(index_type)?
                    .unwrap_or_else(|| index_type.clone());
                if !self.type_checker.is_generic_object_type(&base_object_type)
                    && !self.type_checker.is_generic_index_type(&base_index_type)
                {
                    let access_flags = AccessFlags::Writing
                        | if !Gc::ptr_eq(&base_object_type, object_type) {
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
                    )?;
                    if let Some(constraint) = constraint.as_ref() {
                        if report_errors && original_error_info.is_some() {
                            self.reset_error_info(save_error_info.clone());
                        }
                        result = self.is_related_to(
                            &source,
                            constraint,
                            Some(RecursionFlags::Target),
                            Some(report_errors),
                            None,
                            None,
                        )?;
                        if result != Ternary::False {
                            return Ok(result);
                        }
                        if report_errors {
                            if let Some(original_error_info) = original_error_info.clone() {
                                if let Some(error_info) = self.maybe_error_info() {
                                    *self.maybe_error_info_mut() = Some(
                                        if self.count_message_chain_breadth(Some(&*vec![
                                            &*original_error_info,
                                        ])) <= self
                                            .count_message_chain_breadth(Some(&*vec![&*error_info]))
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
        } else if self.type_checker.is_generic_mapped_type(target)?
            && !Rc::ptr_eq(&self.relation, &self.type_checker.identity_relation)
        {
            let keys_remapped = target
                .as_mapped_type()
                .declaration
                .as_mapped_type_node()
                .name_type
                .is_some();
            let template_type = self
                .type_checker
                .get_template_type_from_mapped_type(target)?;
            let modifiers = self.type_checker.get_mapped_type_modifiers(target);
            if !modifiers.intersects(MappedTypeModifiers::ExcludeOptional) {
                if !keys_remapped
                    && template_type.flags().intersects(TypeFlags::IndexedAccess)
                    && Gc::ptr_eq(&template_type.as_indexed_access_type().object_type, &source)
                    && Gc::ptr_eq(
                        &template_type.as_indexed_access_type().index_type,
                        &self
                            .type_checker
                            .get_type_parameter_from_mapped_type(target)?,
                    )
                {
                    return Ok(Ternary::True);
                }
                if !self.type_checker.is_generic_mapped_type(&source)? {
                    let target_keys = if keys_remapped {
                        self.type_checker
                            .get_name_type_from_mapped_type(target)?
                            .unwrap()
                    } else {
                        self.type_checker
                            .get_constraint_type_from_mapped_type(target)?
                    };
                    let source_keys =
                        self.type_checker
                            .get_index_type(&source, None, Some(true))?;
                    let include_optional =
                        modifiers.intersects(MappedTypeModifiers::IncludeOptional);
                    let filtered_by_applicability = if include_optional {
                        self.type_checker
                            .intersect_types(Some(&*target_keys), Some(&*source_keys))?
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
                        )? != Ternary::False
                    } {
                        let template_type = self
                            .type_checker
                            .get_template_type_from_mapped_type(target)?;
                        let type_parameter = self
                            .type_checker
                            .get_type_parameter_from_mapped_type(target)?;

                        let non_null_component = self
                            .type_checker
                            .extract_types_of_kind(&template_type, !TypeFlags::Nullable);
                        if !keys_remapped
                            && non_null_component
                                .flags()
                                .intersects(TypeFlags::IndexedAccess)
                            && Gc::ptr_eq(
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
                            )?;
                            if result != Ternary::False {
                                return Ok(result);
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
                                )?
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
                            )?;
                            result = self.is_related_to(
                                &indexed_access_type,
                                &template_type,
                                Some(RecursionFlags::Both),
                                Some(report_errors),
                                None,
                                None,
                            )?;
                            if result != Ternary::False {
                                return Ok(result);
                            }
                        }
                    }
                    original_error_info = self.maybe_error_info();
                    self.reset_error_info(save_error_info.clone());
                }
            }
        } else if target.flags().intersects(TypeFlags::Conditional) {
            if self.type_checker.is_deeply_nested_type(
                target,
                self.maybe_target_stack().as_ref().unwrap(),
                self.target_depth(),
                Some(10),
            ) {
                self.reset_error_info(save_error_info);
                return Ok(Ternary::Maybe);
            }
            let c = target;
            let c_as_conditional_type = c.as_conditional_type();
            if (*c_as_conditional_type.root)
                .borrow()
                .infer_type_parameters
                .is_none()
                && !self
                    .type_checker
                    .is_distribution_dependent(&(*c_as_conditional_type.root).borrow())?
            {
                let skip_true = !self.type_checker.is_type_assignable_to(
                    &*self
                        .type_checker
                        .get_permissive_instantiation(&c_as_conditional_type.check_type)?,
                    &*self
                        .type_checker
                        .get_permissive_instantiation(&c_as_conditional_type.extends_type)?,
                )?;
                let skip_false = !skip_true
                    && self.type_checker.is_type_assignable_to(
                        &*self
                            .type_checker
                            .get_restrictive_instantiation(&c_as_conditional_type.check_type)?,
                        &*self
                            .type_checker
                            .get_restrictive_instantiation(&c_as_conditional_type.extends_type)?,
                    )?;
                result = if skip_true {
                    Ternary::True
                } else {
                    self.is_related_to(
                        &source,
                        &*self.type_checker.get_true_type_from_conditional_type(c)?,
                        Some(RecursionFlags::Target),
                        Some(false),
                        None,
                        None,
                    )?
                };
                if result != Ternary::False {
                    result &= if skip_false {
                        Ternary::True
                    } else {
                        self.is_related_to(
                            &source,
                            &*self.type_checker.get_false_type_from_conditional_type(c)?,
                            Some(RecursionFlags::Target),
                            Some(false),
                            None,
                            None,
                        )?
                    };
                    if result != Ternary::False {
                        self.reset_error_info(save_error_info);
                        return Ok(result);
                    }
                }
            }
        } else if target.flags().intersects(TypeFlags::TemplateLiteral) {
            if source.flags().intersects(TypeFlags::TemplateLiteral) {
                if Rc::ptr_eq(&self.relation, &self.type_checker.comparable_relation) {
                    return Ok(
                        if self
                            .type_checker
                            .template_literal_types_definitely_unrelated(&source, target)
                        {
                            Ternary::False
                        } else {
                            Ternary::True
                        },
                    );
                }
                self.type_checker.instantiate_type(
                    &source,
                    Some(Gc::new(
                        self.type_checker
                            .make_function_type_mapper(ReportUnreliableMarkers),
                    )),
                );
            }
            if self
                .type_checker
                .is_type_matched_by_template_literal_type(&source, target)?
            {
                return Ok(Ternary::True);
            }
        }

        if source.flags().intersects(TypeFlags::TypeVariable) {
            if !(source.flags().intersects(TypeFlags::IndexedAccess)
                && target.flags().intersects(TypeFlags::IndexedAccess))
            {
                let constraint = self.type_checker.get_constraint_of_type(&source)?;
                if match constraint.as_ref() {
                    None => true,
                    Some(constraint) => {
                        source.flags().intersects(TypeFlags::TypeParameter)
                            && constraint.flags().intersects(TypeFlags::Any)
                    }
                } {
                    result = self.is_related_to(
                        &self.type_checker.empty_object_type(),
                        &self
                            .type_checker
                            .extract_types_of_kind(target, !TypeFlags::NonPrimitive),
                        Some(RecursionFlags::Both),
                        None,
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        self.reset_error_info(save_error_info);
                        return Ok(result);
                    }
                } else if {
                    result = self.is_related_to(
                        constraint.as_ref().unwrap(),
                        target,
                        Some(RecursionFlags::Source),
                        Some(false),
                        None,
                        Some(intersection_state),
                    )?;
                    result != Ternary::False
                } {
                    self.reset_error_info(save_error_info);
                    return Ok(result);
                } else if {
                    result = self.is_related_to(
                        &*self.type_checker.get_type_with_this_argument(
                            constraint.as_ref().unwrap(),
                            Some(&*source),
                            None,
                        )?,
                        target,
                        Some(RecursionFlags::Source),
                        Some(
                            report_errors
                                && !(target.flags() & source.flags())
                                    .intersects(TypeFlags::TypeParameter),
                        ),
                        None,
                        Some(intersection_state),
                    )?;
                    result != Ternary::False
                } {
                    self.reset_error_info(save_error_info);
                    return Ok(result);
                }
            }
        } else if source.flags().intersects(TypeFlags::Index) {
            result = self.is_related_to(
                &self.type_checker.keyof_constraint_type(),
                target,
                Some(RecursionFlags::Source),
                Some(report_errors),
                None,
                None,
            )?;
            if result != Ternary::False {
                self.reset_error_info(save_error_info);
                return Ok(result);
            }
        } else if source.flags().intersects(TypeFlags::TemplateLiteral)
            && !target.flags().intersects(TypeFlags::Object)
        {
            if !target.flags().intersects(TypeFlags::TemplateLiteral) {
                let constraint = self.type_checker.get_base_constraint_of_type(&source)?;
                if let Some(constraint) = constraint
                    .as_ref()
                    .filter(|constraint| !Gc::ptr_eq(*constraint, &source))
                {
                    result = self.is_related_to(
                        constraint,
                        target,
                        Some(RecursionFlags::Source),
                        Some(report_errors),
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        self.reset_error_info(save_error_info);
                        return Ok(result);
                    }
                }
            }
        } else if source.flags().intersects(TypeFlags::StringMapping) {
            if target.flags().intersects(TypeFlags::StringMapping)
                && Gc::ptr_eq(
                    &source.as_string_mapping_type().symbol(),
                    &target.as_string_mapping_type().symbol(),
                )
            {
                result = self.is_related_to(
                    &source.as_string_mapping_type().type_,
                    &target.as_string_mapping_type().type_,
                    Some(RecursionFlags::Both),
                    Some(report_errors),
                    None,
                    None,
                )?;
                if result != Ternary::False {
                    self.reset_error_info(save_error_info);
                    return Ok(result);
                }
            } else {
                let constraint = self.type_checker.get_base_constraint_of_type(&source)?;
                if let Some(constraint) = constraint.as_ref() {
                    result = self.is_related_to(
                        constraint,
                        target,
                        Some(RecursionFlags::Source),
                        Some(report_errors),
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        self.reset_error_info(save_error_info);
                        return Ok(result);
                    }
                }
            }
        } else if source.flags().intersects(TypeFlags::Conditional) {
            if self.type_checker.is_deeply_nested_type(
                &source,
                self.maybe_source_stack().as_ref().unwrap(),
                self.source_depth(),
                Some(10),
            ) {
                self.reset_error_info(save_error_info);
                return Ok(Ternary::Maybe);
            }
            if target.flags().intersects(TypeFlags::Conditional) {
                let source_params = (*source.as_conditional_type().root)
                    .borrow()
                    .infer_type_parameters
                    .clone();
                let mut source_extends = source.as_conditional_type().extends_type.clone();
                let mut mapper: Option<Gc<TypeMapper>> = None;
                if let Some(source_params) = source_params.as_ref() {
                    let ctx = self.type_checker.create_inference_context(
                        source_params,
                        None,
                        InferenceFlags::None,
                        Some(Gc::new(Box::new(TypeComparerIsRelatedToWorker::new(
                            self.rc_wrapper(),
                        )))),
                    );
                    self.type_checker.infer_types(
                        &ctx.inferences(),
                        &target.as_conditional_type().extends_type,
                        &source_extends,
                        Some(InferencePriority::NoConstraints | InferencePriority::AlwaysStrict),
                        None,
                    );
                    source_extends = self
                        .type_checker
                        .instantiate_type(&source_extends, Some(ctx.mapper()))?;
                    mapper = Some(ctx.mapper());
                }
                if self.type_checker.is_type_identical_to(
                    &source_extends,
                    &target.as_conditional_type().extends_type,
                )? && (self.is_related_to(
                    &source.as_conditional_type().check_type,
                    &target.as_conditional_type().check_type,
                    Some(RecursionFlags::Both),
                    None,
                    None,
                    None,
                )? != Ternary::False
                    || self.is_related_to(
                        &target.as_conditional_type().check_type,
                        &source.as_conditional_type().check_type,
                        Some(RecursionFlags::Both),
                        None,
                        None,
                        None,
                    )? != Ternary::False)
                {
                    result = self.is_related_to(
                        &*self.type_checker.instantiate_type(
                            &*self
                                .type_checker
                                .get_true_type_from_conditional_type(&source)?,
                            mapper,
                        )?,
                        &*self
                            .type_checker
                            .get_true_type_from_conditional_type(target)?,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        result &= self.is_related_to(
                            &*self
                                .type_checker
                                .get_false_type_from_conditional_type(&source)?,
                            &*self
                                .type_checker
                                .get_false_type_from_conditional_type(target)?,
                            Some(RecursionFlags::Both),
                            Some(report_errors),
                            None,
                            None,
                        )?;
                    }
                    if result != Ternary::False {
                        self.reset_error_info(save_error_info);
                        return Ok(result);
                    }
                }
            } else {
                let distributive_constraint = self
                    .type_checker
                    .get_constraint_of_distributive_conditional_type(&source)?;
                if let Some(distributive_constraint) = distributive_constraint.as_ref() {
                    result = self.is_related_to(
                        distributive_constraint,
                        target,
                        Some(RecursionFlags::Source),
                        Some(report_errors),
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        self.reset_error_info(save_error_info);
                        return Ok(result);
                    }
                }
            }

            let default_constraint = self
                .type_checker
                .get_default_constraint_of_conditional_type(&source)?;
            // if (defaultConstraint) {
            result = self.is_related_to(
                &default_constraint,
                target,
                Some(RecursionFlags::Source),
                Some(report_errors),
                None,
                None,
            )?;
            if result != Ternary::False {
                self.reset_error_info(save_error_info);
                return Ok(result);
            }
            // }
        } else {
            if !Rc::ptr_eq(&self.relation, &self.type_checker.subtype_relation)
                && !Rc::ptr_eq(&self.relation, &self.type_checker.strict_subtype_relation)
                && self.type_checker.is_partial_mapped_type(target)
                && self.type_checker.is_empty_object_type(&source)?
            {
                return Ok(Ternary::True);
            }
            if self.type_checker.is_generic_mapped_type(target)? {
                if self.type_checker.is_generic_mapped_type(&source)? {
                    result = self.mapped_type_related_to(&source, target, report_errors)?;
                    if result != Ternary::False {
                        self.reset_error_info(save_error_info);
                        return Ok(result);
                    }
                }
                return Ok(Ternary::False);
            }
            let source_is_primitive = source.flags().intersects(TypeFlags::Primitive);
            if !Rc::ptr_eq(&self.relation, &self.type_checker.identity_relation) {
                source = self.type_checker.get_apparent_type(&source)?;
            } else if self.type_checker.is_generic_mapped_type(&source)? {
                return Ok(Ternary::False);
            }
            if get_object_flags(&source).intersects(ObjectFlags::Reference)
                && get_object_flags(target).intersects(ObjectFlags::Reference)
                && Gc::ptr_eq(
                    &source.as_type_reference_interface().target(),
                    &target.as_type_reference_interface().target(),
                )
                && !self.type_checker.is_tuple_type(&source)
                && !(get_object_flags(&source).intersects(ObjectFlags::MarkerType)
                    || get_object_flags(target).intersects(ObjectFlags::MarkerType))
            {
                let variances = self
                    .type_checker
                    .get_variances(&source.as_type_reference_interface().target());
                if variances.is_empty() {
                    return Ok(Ternary::Unknown);
                }
                let variance_result = self.relate_variances(
                    &mut result,
                    report_errors,
                    &mut original_error_info,
                    &save_error_info,
                    &mut variance_check_failed,
                    Some(&*self.type_checker.get_type_arguments(&source)?),
                    Some(&*self.type_checker.get_type_arguments(target)?),
                    &variances,
                    intersection_state,
                )?;
                if let Some(variance_result) = variance_result {
                    return Ok(variance_result);
                }
            } else if if self.type_checker.is_readonly_array_type(target) {
                self.type_checker.is_array_type(&source) || self.type_checker.is_tuple_type(&source)
            } else {
                self.type_checker.is_array_type(target)
                    && self.type_checker.is_tuple_type(&source)
                    && !source
                        .as_type_reference_interface()
                        .target()
                        .as_tuple_type()
                        .readonly
            } {
                if !Rc::ptr_eq(&self.relation, &self.type_checker.identity_relation) {
                    return self.is_related_to(
                        &self
                            .type_checker
                            .get_index_type_of_type_(&source, &self.type_checker.number_type())
                            .unwrap_or_else(|| self.type_checker.any_type()),
                        &self
                            .type_checker
                            .get_index_type_of_type_(target, &self.type_checker.number_type())
                            .unwrap_or_else(|| self.type_checker.any_type()),
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        None,
                    );
                } else {
                    return Ok(Ternary::False);
                }
            } else if (Rc::ptr_eq(&self.relation, &self.type_checker.subtype_relation)
                || Rc::ptr_eq(&self.relation, &self.type_checker.strict_subtype_relation))
                && self.type_checker.is_empty_object_type(target)?
                && get_object_flags(target).intersects(ObjectFlags::FreshLiteral)
                && !self.type_checker.is_empty_object_type(&source)?
            {
                return Ok(Ternary::False);
            }
            if source
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::Intersection)
                && target.flags().intersects(TypeFlags::Object)
            {
                let report_structural_errors = report_errors
                    && are_option_rcs_equal(
                        self.maybe_error_info().as_ref(),
                        save_error_info.error_info.as_ref(),
                    )
                    && !source_is_primitive;
                result = self.properties_related_to(
                    &source,
                    target,
                    report_structural_errors,
                    None,
                    intersection_state,
                )?;
                if result != Ternary::False {
                    result &= self.signatures_related_to(
                        &source,
                        target,
                        SignatureKind::Call,
                        report_structural_errors,
                    )?;
                    if result != Ternary::False {
                        result &= self.signatures_related_to(
                            &source,
                            target,
                            SignatureKind::Construct,
                            report_structural_errors,
                        )?;
                        if result != Ternary::False {
                            result &= self.index_signatures_related_to(
                                &source,
                                target,
                                source_is_primitive,
                                report_structural_errors,
                                intersection_state,
                            )?;
                        }
                    }
                }
                if variance_check_failed && result != Ternary::False {
                    let error_info = original_error_info
                        .or_else(|| self.maybe_error_info())
                        .or_else(|| save_error_info.error_info.clone());
                    *self.maybe_error_info_mut() = error_info;
                } else if result != Ternary::False {
                    return Ok(result);
                }
            }
            if source
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::Intersection)
                && target.flags().intersects(TypeFlags::Union)
            {
                let object_only_target = self.type_checker.extract_types_of_kind(
                    target,
                    TypeFlags::Object | TypeFlags::Intersection | TypeFlags::Substitution,
                );
                if object_only_target.flags().intersects(TypeFlags::Union) {
                    let result =
                        self.type_related_to_discriminated_type(&source, &object_only_target)?;
                    if result != Ternary::False {
                        return Ok(result);
                    }
                }
            }
        }
        Ok(Ternary::False)
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ReportUnmeasurableMarkers;
impl TypeMapperCallback for ReportUnmeasurableMarkers {
    fn call(&self, checker: &TypeChecker, p: &Type /*TypeParameter*/) -> io::Result<Gc<Type>> {
        if let Some(outofband_variance_marker_handler) =
            checker.maybe_outofband_variance_marker_handler()
        {
            if ptr::eq(p, &*checker.marker_super_type())
                || ptr::eq(p, &*checker.marker_sub_type())
                || ptr::eq(p, &*checker.marker_other_type())
            {
                outofband_variance_marker_handler.call(false);
            }
        }
        Ok(p.type_wrapper())
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ReportUnreliableMarkers;
impl TypeMapperCallback for ReportUnreliableMarkers {
    fn call(&self, checker: &TypeChecker, p: &Type /*TypeParameter*/) -> io::Result<Gc<Type>> {
        if let Some(outofband_variance_marker_handler) =
            checker.maybe_outofband_variance_marker_handler()
        {
            if ptr::eq(p, &*checker.marker_super_type())
                || ptr::eq(p, &*checker.marker_sub_type())
                || ptr::eq(p, &*checker.marker_other_type())
            {
                outofband_variance_marker_handler.call(true);
            }
        }
        Ok(p.type_wrapper())
    }
}

#[derive(Trace, Finalize)]
pub(super) struct TypeComparerIsRelatedToWorker {
    check_type_related_to: Gc<CheckTypeRelatedTo>,
}

impl TypeComparerIsRelatedToWorker {
    pub fn new(check_type_related_to: Gc<CheckTypeRelatedTo>) -> Self {
        Self {
            check_type_related_to,
        }
    }
}

impl TypeComparer for TypeComparerIsRelatedToWorker {
    fn call(&self, s: &Type, t: &Type, report_errors: Option<bool>) -> io::Result<Ternary> {
        self.check_type_related_to.is_related_to_worker(
            s,
            t,
            report_errors.unwrap_or(false), // the default isn't in the Typescript version but that appears to be a type-checking bug there
        )
    }
}

#[derive(Trace, Finalize)]
struct RecursiveTypeRelatedToOutofbandVarianceMarkerHandler {
    #[unsafe_ignore_trace]
    propagating_variance_flags: Rc<Cell<RelationComparisonResult>>,
    original_handler: Gc<Box<dyn OutofbandVarianceMarkerHandler>>,
}

impl RecursiveTypeRelatedToOutofbandVarianceMarkerHandler {
    pub fn new(
        propagating_variance_flags: Rc<Cell<RelationComparisonResult>>,
        original_handler: Gc<Box<dyn OutofbandVarianceMarkerHandler>>,
    ) -> Self {
        Self {
            propagating_variance_flags,
            original_handler,
        }
    }
}

impl OutofbandVarianceMarkerHandler for RecursiveTypeRelatedToOutofbandVarianceMarkerHandler {
    fn call(&self, only_unreliable: bool) {
        self.propagating_variance_flags.set(
            self.propagating_variance_flags.get()
                | if only_unreliable {
                    RelationComparisonResult::ReportsUnreliable
                } else {
                    RelationComparisonResult::ReportsUnmeasurable
                },
        );
        self.original_handler.call(only_unreliable);
    }
}
