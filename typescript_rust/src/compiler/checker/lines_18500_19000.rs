use std::{cell::Cell, io, ptr, rc::Rc};

use gc::{Finalize, Gc, Trace};
use id_arena::Id;
use regex::{Captures, Regex};

use super::{
    CheckTypeRelatedTo, ExpandingFlags, IntersectionState, MappedTypeModifiers, RecursionFlags,
};
use crate::{
    are_gc_slices_equal, are_option_rcs_equal, get_object_flags, try_map, AccessFlags,
    DiagnosticMessageChain, HasArena, InArena, InferenceFlags, InferencePriority, Node,
    NodeInterface, ObjectFlags, ObjectTypeInterface, OutofbandVarianceMarkerHandler,
    RelationComparisonResult, SignatureKind, Symbol, SymbolInterface, Ternary, Type, TypeChecker,
    TypeComparer, TypeFlags, TypeInterface, TypeMapper, TypeMapperCallback,
    UnionOrIntersectionTypeInterface, VarianceFlags, AllArenas,
};

impl CheckTypeRelatedTo {
    pub(super) fn should_check_as_excess_property(
        &self,
        prop: Id<Symbol>,
        container: Id<Symbol>,
    ) -> bool {
        if let Some(prop_value_declaration) = prop.ref_(self).maybe_value_declaration().as_ref() {
            if let Some(container_value_declaration) =
                container.ref_(self).maybe_value_declaration()
            {
                return prop_value_declaration.ref_(self).parent() == container_value_declaration;
            }
        }
        false
    }

    pub(super) fn each_type_related_to_some_type(
        &self,
        source: Id<Type>, /*UnionOrIntersectionType*/
        target: Id<Type>, /*UnionOrIntersectionType*/
    ) -> io::Result<Ternary> {
        let mut result = Ternary::True;
        let source_types = source
            .ref_(self)
            .as_union_or_intersection_type_interface()
            .types()
            .to_owned();
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
        source: Id<Type>,
        target: Id<Type>, /*UnionOrIntersectionType*/
        report_errors: bool,
    ) -> io::Result<Ternary> {
        let ref target_types = target
            .ref_(self)
            .as_union_or_intersection_type_interface()
            .types()
            .to_owned();
        if target.ref_(self).flags().intersects(TypeFlags::Union) {
            if self.type_checker.ref_(self).contains_type(target_types, source) {
                return Ok(Ternary::True);
            }
            let match_ = self
                .type_checker
                .ref_(self).get_matching_union_constituent_for_type(target, source)?;
            if let Some(match_) = match_ {
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
        for &type_ in target_types {
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
            let best_matching_type = self.type_checker.ref_(self).get_best_matching_type(
                source,
                target,
                Some(|source: Id<Type>, target: Id<Type>| {
                    self.is_related_to(source, target, None, None, None, None)
                }),
            )?;
            self.is_related_to(
                source,
                best_matching_type.unwrap_or_else(|| target_types[target_types.len() - 1].clone()),
                Some(RecursionFlags::Target),
                Some(true),
                None,
                None,
            )?;
        }
        Ok(Ternary::False)
    }

    pub(super) fn type_related_to_each_type(
        &self,
        source: Id<Type>,
        target: Id<Type>, /*IntersectionType*/
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        let mut result = Ternary::True;
        let target_types = target
            .ref_(self)
            .as_union_or_intersection_type_interface()
            .types()
            .to_owned();
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
        source: Id<Type>, /*UnionOrIntersectionType*/
        target: Id<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        let source_ref = source.ref_(self);
        let source_types = source_ref.as_union_or_intersection_type_interface().types();
        if source.ref_(self).flags().intersects(TypeFlags::Union)
            && self.type_checker.ref_(self).contains_type(source_types, target)
        {
            return Ok(Ternary::True);
        }
        let len = source_types.len();
        for i in 0..len {
            let related = self.is_related_to(
                source_types[i],
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
        source: Id<Type>,
        target: Id<Type>,
    ) -> Id<Type> {
        if source.ref_(self).flags().intersects(TypeFlags::Union)
            && target.ref_(self).flags().intersects(TypeFlags::Union)
            && !source
                .ref_(self)
                .as_union_or_intersection_type_interface()
                .types()[0]
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Undefined)
            && target
                .ref_(self)
                .as_union_or_intersection_type_interface()
                .types()[0]
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Undefined)
        {
            return self
                .type_checker
                .ref_(self).extract_types_of_kind(target, !TypeFlags::Undefined);
        }
        target
    }

    pub(super) fn each_type_related_to_type(
        &self,
        source: Id<Type>, /*UnionOrIntersectionType*/
        target: Id<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        let mut result = Ternary::True;
        let source_types = &source
            .ref_(self)
            .as_union_or_intersection_type_interface()
            .types()
            .to_owned();
        let undefined_stripped_target =
            self.get_undefined_stripped_target_if_needed(source, target);
        for (i, source_type) in source_types.into_iter().enumerate() {
            let source_type = *source_type;
            if undefined_stripped_target
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Union)
            {
                let undefined_stripped_target_types = undefined_stripped_target
                    .ref_(self)
                    .as_union_type()
                    .types()
                    .to_owned();
                if source_types.len() >= undefined_stripped_target_types.len()
                    && source_types.len() % undefined_stripped_target_types.len() == 0
                {
                    let related = self.is_related_to(
                        source_type,
                        undefined_stripped_target_types[i % undefined_stripped_target_types.len()],
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
        sources: Option<Vec<Id<Type>>>,
        targets: Option<Vec<Id<Type>>>,
        variances: Option<Vec<VarianceFlags>>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        let sources = sources.unwrap_or_else(|| vec![]);
        let targets = targets.unwrap_or_else(|| vec![]);
        let variances = variances.unwrap_or_else(|| vec![]);
        if sources.len() != targets.len()
            && Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).identity_relation)
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
                let s = sources[i];
                let t = targets[i];
                let mut related: Ternary/* = Ternary::True*/;
                if variance_flags.intersects(VarianceFlags::Unmeasurable) {
                    related = if Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).identity_relation) {
                        self.is_related_to(
                            s,
                            t,
                            Some(RecursionFlags::Both),
                            Some(false),
                            None,
                            None,
                        )?
                    } else {
                        self.type_checker.ref_(self).compare_types_identical(s, t)?
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
        source: Id<Type>,
        target: Id<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
        recursion_flags: RecursionFlags,
    ) -> io::Result<Ternary> {
        if self.overflow() {
            return Ok(Ternary::False);
        }
        let id = self.type_checker.ref_(self).get_relation_key(
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
                    .ref_(self).maybe_outofband_variance_marker_handler()
                    .is_some()
                {
                    let saved = *entry & RelationComparisonResult::ReportsMask;
                    if saved.intersects(RelationComparisonResult::ReportsUnmeasurable) {
                        self.type_checker.ref_(self).instantiate_type(
                            source,
                            Some(
                                self.type_checker
                                    .ref_(self).make_function_type_mapper(ReportUnmeasurableMarkers),
                            ),
                        )?;
                    }
                    if saved.intersects(RelationComparisonResult::ReportsUnreliable) {
                        self.type_checker.ref_(self).instantiate_type(
                            source,
                            Some(
                                self.type_checker
                                    .ref_(self).make_function_type_mapper(ReportUnreliableMarkers),
                            ),
                        )?;
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
            self.maybe_source_stack().as_mut().unwrap().push(source);
            self.set_source_depth(self.source_depth() + 1);
            if !self.expanding_flags().intersects(ExpandingFlags::Source)
                && self.type_checker.ref_(self).is_deeply_nested_type(
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
            self.maybe_target_stack().as_mut().unwrap().push(target);
            self.set_target_depth(self.target_depth() + 1);
            if !self.expanding_flags().intersects(ExpandingFlags::Target)
                && self.type_checker.ref_(self).is_deeply_nested_type(
                    target,
                    self.maybe_target_stack().as_deref().unwrap(),
                    self.target_depth(),
                    None,
                )
            {
                self.set_expanding_flags(self.expanding_flags() | ExpandingFlags::Target);
            }
        }
        let mut original_handler: Option<Id<Box<dyn OutofbandVarianceMarkerHandler>>> = None;
        let propagating_variance_flags: Rc<Cell<RelationComparisonResult>> =
            Rc::new(Cell::new(RelationComparisonResult::None));
        if let Some(outofband_variance_marker_handler) =
            self.type_checker.ref_(self).maybe_outofband_variance_marker_handler()
        {
            original_handler = Some(outofband_variance_marker_handler);
            self.type_checker
                .ref_(self).set_outofband_variance_marker_handler(Some(self.alloc_outofband_variance_marker_handler(Box::new(
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
            .ref_(self).maybe_outofband_variance_marker_handler()
            .is_some()
        {
            self.type_checker
                .ref_(self).set_outofband_variance_marker_handler(original_handler);
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
        source: Id<Type>,
        target: Id<Type>,
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

    #[allow(clippy::if_same_then_else)]
    pub(super) fn structured_type_related_to_worker(
        &self,
        mut source: Id<Type>,
        target: Id<Type>,
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
        if intersection_state.intersects(IntersectionState::UnionIntersectionCheck) {
            if source.ref_(self).flags().intersects(TypeFlags::Union) {
                return Ok(
                    if Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).comparable_relation) {
                        self.some_type_related_to_type(
                            source,
                            target,
                            report_errors
                                && !source.ref_(self).flags().intersects(TypeFlags::Primitive),
                            intersection_state & !IntersectionState::UnionIntersectionCheck,
                        )?
                    } else {
                        self.each_type_related_to_type(
                            source,
                            target,
                            report_errors
                                && !source.ref_(self).flags().intersects(TypeFlags::Primitive),
                            intersection_state & !IntersectionState::UnionIntersectionCheck,
                        )?
                    },
                );
            }
            if target.ref_(self).flags().intersects(TypeFlags::Union) {
                return self.type_related_to_some_type(
                    self.type_checker
                        .ref_(self).get_regular_type_of_object_literal(source)?,
                    target,
                    report_errors
                        && !source.ref_(self).flags().intersects(TypeFlags::Primitive)
                        && !target.ref_(self).flags().intersects(TypeFlags::Primitive),
                );
            }
            if target
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Intersection)
            {
                return self.type_related_to_each_type(
                    self.type_checker
                        .ref_(self).get_regular_type_of_object_literal(source)?,
                    target,
                    report_errors,
                    IntersectionState::Target,
                );
            }
            if Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).comparable_relation)
                && target.ref_(self).flags().intersects(TypeFlags::Primitive)
            {
                let constraints = try_map(
                    source
                        .ref_(self)
                        .as_union_or_intersection_type_interface()
                        .types()
                        .to_owned(),
                    |type_: Id<Type>, _| self.type_checker.ref_(self).get_base_constraint_or_type(type_),
                )?;
                if &constraints
                    != source
                        .ref_(self)
                        .as_union_or_intersection_type_interface()
                        .types()
                {
                    source = self.type_checker.ref_(self).get_intersection_type(
                        &constraints,
                        Option::<Id<Symbol>>::None,
                        None,
                    )?;
                    if !source
                        .ref_(self)
                        .flags()
                        .intersects(TypeFlags::Intersection)
                    {
                        return self.is_related_to(
                            source,
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
                source,
                target,
                false,
                IntersectionState::Source,
            );
        }
        let flags = source.ref_(self).flags() & target.ref_(self).flags();
        if Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).identity_relation)
            && !flags.intersects(TypeFlags::Object)
        {
            if flags.intersects(TypeFlags::Index) {
                return self.is_related_to(
                    source.ref_(self).as_index_type().type_,
                    target.ref_(self).as_index_type().type_,
                    Some(RecursionFlags::Both),
                    Some(false),
                    None,
                    None,
                );
            }
            let mut result: Ternary;
            if flags.intersects(TypeFlags::IndexedAccess) {
                result = self.is_related_to(
                    source.ref_(self).as_indexed_access_type().object_type,
                    target.ref_(self).as_indexed_access_type().object_type,
                    Some(RecursionFlags::Both),
                    Some(false),
                    None,
                    None,
                )?;
                if result != Ternary::False {
                    result &= self.is_related_to(
                        source.ref_(self).as_indexed_access_type().index_type,
                        target.ref_(self).as_indexed_access_type().index_type,
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
                if source.ref_(self).as_conditional_type().root
                    .ref_(self)
                    .is_distributive
                    == target.ref_(self).as_conditional_type().root
                        .ref_(self)
                        .is_distributive
                {
                    result = self.is_related_to(
                        source.ref_(self).as_conditional_type().check_type,
                        target.ref_(self).as_conditional_type().check_type,
                        Some(RecursionFlags::Both),
                        Some(false),
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        result &= self.is_related_to(
                            source.ref_(self).as_conditional_type().extends_type,
                            target.ref_(self).as_conditional_type().extends_type,
                            Some(RecursionFlags::Both),
                            Some(false),
                            None,
                            None,
                        )?;
                        if result != Ternary::False {
                            result &= self.is_related_to(
                                self.type_checker
                                    .ref_(self).get_true_type_from_conditional_type(source)?,
                                self.type_checker
                                    .ref_(self).get_true_type_from_conditional_type(target)?,
                                Some(RecursionFlags::Both),
                                Some(false),
                                None,
                                None,
                            )?;
                            if result != Ternary::False {
                                result &= self.is_related_to(
                                    self.type_checker
                                        .ref_(self).get_false_type_from_conditional_type(source)?,
                                    self.type_checker
                                        .ref_(self).get_false_type_from_conditional_type(target)?,
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
                    source.ref_(self).as_substitution_type().substitute,
                    target.ref_(self).as_substitution_type().substitute,
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
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Object | TypeFlags::Conditional)
        {
            if let Some(source_alias_symbol) = {
                let alias_symbol = source.ref_(self).maybe_alias_symbol();
                alias_symbol
            } {
                if let Some(source_alias_type_arguments) = {
                    let alias_type_arguments = source.ref_(self).maybe_alias_type_arguments();
                    alias_type_arguments
                }
                .as_ref()
                {
                    if matches!(
                        target.ref_(self).maybe_alias_symbol(),
                        Some(target_alias_symbol) if
                            source_alias_symbol ==
                            target_alias_symbol
                    ) {
                        if !(matches!(
                            source
                                .ref_(self)
                                .maybe_alias_type_arguments_contains_marker(),
                            Some(true)
                        ) || matches!(
                            target
                                .ref_(self)
                                .maybe_alias_type_arguments_contains_marker(),
                            Some(true)
                        )) {
                            let variances =
                                self.type_checker.ref_(self).get_alias_variances(source_alias_symbol)?;
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
                                target.ref_(self).maybe_alias_type_arguments().as_deref(),
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
            .ref_(self).is_single_element_generic_tuple_type(source)
            && !source
                .ref_(self)
                .as_type_reference()
                .target
                .ref_(self)
                .as_tuple_type()
                .readonly
        {
            result = self.is_related_to(
                self.type_checker.ref_(self).get_type_arguments(source)?[0],
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
            .ref_(self).is_single_element_generic_tuple_type(target)
            && (target
                .ref_(self)
                .as_type_reference()
                .target
                .ref_(self)
                .as_tuple_type()
                .readonly
                || self.type_checker.ref_(self).is_mutable_array_or_tuple(
                    self.type_checker
                        .ref_(self).get_base_constraint_of_type(source)?
                        .unwrap_or_else(|| source.clone()),
                ))
        {
            result = self.is_related_to(
                source,
                self.type_checker.ref_(self).get_type_arguments(target)?[0],
                Some(RecursionFlags::Target),
                None,
                None,
                None,
            )?;
            if result != Ternary::False {
                return Ok(result);
            }
        }

        if target
            .ref_(self)
            .flags()
            .intersects(TypeFlags::TypeParameter)
        {
            if get_object_flags(&source.ref_(self)).intersects(ObjectFlags::Mapped)
                && source
                    .ref_(self)
                    .as_mapped_type()
                    .declaration
                    .ref_(self).as_mapped_type_node()
                    .name_type
                    .is_none()
                && self.is_related_to(
                    self.type_checker.ref_(self).get_index_type(target, None, None)?,
                    self.type_checker
                        .ref_(self).get_constraint_type_from_mapped_type(source)?,
                    Some(RecursionFlags::Both),
                    None,
                    None,
                    None,
                )? != Ternary::False
            {
                if !self
                    .type_checker
                    .ref_(self).get_mapped_type_modifiers(source)
                    .intersects(MappedTypeModifiers::IncludeOptional)
                {
                    let template_type = self
                        .type_checker
                        .ref_(self).get_template_type_from_mapped_type(source)?;
                    let indexed_access_type = self.type_checker.ref_(self).get_indexed_access_type(
                        target,
                        self.type_checker
                            .ref_(self).get_type_parameter_from_mapped_type(source)?,
                        None,
                        Option::<Id<Node>>::None,
                        Option::<Id<Symbol>>::None,
                        None,
                    )?;
                    result = self.is_related_to(
                        template_type,
                        indexed_access_type,
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
        } else if target.ref_(self).flags().intersects(TypeFlags::Index) {
            let target_type = target.ref_(self).as_index_type().type_;
            if source.ref_(self).flags().intersects(TypeFlags::Index) {
                result = self.is_related_to(
                    target_type,
                    source.ref_(self).as_index_type().type_,
                    Some(RecursionFlags::Both),
                    Some(false),
                    None,
                    None,
                )?;
                if result != Ternary::False {
                    return Ok(result);
                }
            }
            if self.type_checker.ref_(self).is_tuple_type(target_type) {
                result = self.is_related_to(
                    source,
                    self.type_checker
                        .ref_(self).get_known_keys_of_tuple_type(target_type)?,
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
                    .ref_(self).get_simplified_type_or_constraint(target_type)?;
                if let Some(constraint) = constraint {
                    if self.is_related_to(
                        source,
                        self.type_checker.ref_(self).get_index_type(
                            constraint,
                            Some({
                                let strings_only = target.ref_(self).as_index_type().strings_only;
                                strings_only
                            }),
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
                } else if self.type_checker.ref_(self).is_generic_mapped_type(target_type)? {
                    let name_type = self
                        .type_checker
                        .ref_(self).get_name_type_from_mapped_type(target_type)?;
                    let constraint_type = self
                        .type_checker
                        .ref_(self).get_constraint_type_from_mapped_type(target_type)?;
                    let target_keys: Id<Type>;
                    if let Some(name_type) = name_type.filter(|_| {
                        self.type_checker
                            .ref_(self).is_mapped_type_with_keyof_constraint_declaration(target_type)
                    }) {
                        let modifiers_type = self.type_checker.ref_(self).get_apparent_type(
                            self.type_checker
                                .ref_(self).get_modifiers_type_from_mapped_type(target_type)?,
                        )?;
                        let mut mapped_keys: Vec<Id<Type>> = vec![];
                        self.type_checker
                            .ref_(self).try_for_each_mapped_type_property_key_type_and_index_signature_key_type(
                                modifiers_type,
                                TypeFlags::StringOrNumberLiteralOrUnique,
                                false,
                                |t| {
                                    mapped_keys.push(
                                        self.type_checker.ref_(self).instantiate_type(
                                            name_type,
                                            Some(
                                                self.type_checker.ref_(self).append_type_mapping(
                                                    target_type.ref_(self).as_mapped_type().maybe_mapper(),
                                                    self
                                                        .type_checker
                                                        .ref_(self).get_type_parameter_from_mapped_type(
                                                            target_type,
                                                        )?,
                                                    t,
                                                ),
                                            ),
                                        )?,
                                    );

                                    Ok(())
                                },
                            )?;
                        mapped_keys.push(name_type.clone());
                        target_keys = self.type_checker.ref_(self).get_union_type(
                            &mapped_keys,
                            None,
                            Option::<Id<Symbol>>::None,
                            None,
                            None,
                        )?;
                    } else {
                        target_keys = name_type.unwrap_or(constraint_type);
                    }
                    if self.is_related_to(
                        source,
                        target_keys,
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
        } else if target
            .ref_(self)
            .flags()
            .intersects(TypeFlags::IndexedAccess)
        {
            if source
                .ref_(self)
                .flags()
                .intersects(TypeFlags::IndexedAccess)
            {
                result = self.is_related_to(
                    source.ref_(self).as_indexed_access_type().object_type,
                    target.ref_(self).as_indexed_access_type().object_type,
                    Some(RecursionFlags::Both),
                    Some(report_errors),
                    None,
                    None,
                )?;
                if result != Ternary::False {
                    result &= self.is_related_to(
                        source.ref_(self).as_indexed_access_type().index_type,
                        target.ref_(self).as_indexed_access_type().index_type,
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
            if Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).assignable_relation)
                || Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).comparable_relation)
            {
                let object_type = target.ref_(self).as_indexed_access_type().object_type;
                let index_type = target.ref_(self).as_indexed_access_type().index_type;
                let base_object_type = self
                    .type_checker
                    .ref_(self).get_base_constraint_of_type(object_type)?
                    .unwrap_or_else(|| object_type.clone());
                let base_index_type = self
                    .type_checker
                    .ref_(self).get_base_constraint_of_type(index_type)?
                    .unwrap_or_else(|| index_type.clone());
                if !self.type_checker.ref_(self).is_generic_object_type(base_object_type)?
                    && !self.type_checker.ref_(self).is_generic_index_type(base_index_type)?
                {
                    let access_flags = AccessFlags::Writing
                        | if base_object_type != object_type {
                            AccessFlags::NoIndexSignatures
                        } else {
                            AccessFlags::None
                        };
                    let constraint = self.type_checker.ref_(self).get_indexed_access_type_or_undefined(
                        base_object_type,
                        base_index_type,
                        Some(access_flags),
                        Option::<Id<Node>>::None,
                        Option::<Id<Symbol>>::None,
                        None,
                    )?;
                    if let Some(constraint) = constraint {
                        if report_errors && original_error_info.is_some() {
                            self.reset_error_info(save_error_info.clone());
                        }
                        result = self.is_related_to(
                            source,
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
        } else if self.type_checker.ref_(self).is_generic_mapped_type(target)?
            && !Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).identity_relation)
        {
            let keys_remapped = target
                .ref_(self)
                .as_mapped_type()
                .declaration
                .ref_(self).as_mapped_type_node()
                .name_type
                .is_some();
            let template_type = self
                .type_checker
                .ref_(self).get_template_type_from_mapped_type(target)?;
            let modifiers = self.type_checker.ref_(self).get_mapped_type_modifiers(target);
            if !modifiers.intersects(MappedTypeModifiers::ExcludeOptional) {
                if !keys_remapped
                    && template_type
                        .ref_(self)
                        .flags()
                        .intersects(TypeFlags::IndexedAccess)
                    && template_type
                        .ref_(self)
                        .as_indexed_access_type()
                        .object_type
                        == source
                    && template_type.ref_(self).as_indexed_access_type().index_type
                        == self
                            .type_checker
                            .ref_(self).get_type_parameter_from_mapped_type(target)?
                {
                    return Ok(Ternary::True);
                }
                if !self.type_checker.ref_(self).is_generic_mapped_type(source)? {
                    let target_keys = if keys_remapped {
                        self.type_checker
                            .ref_(self).get_name_type_from_mapped_type(target)?
                            .unwrap()
                    } else {
                        self.type_checker
                            .ref_(self).get_constraint_type_from_mapped_type(target)?
                    };
                    let source_keys = self.type_checker.ref_(self).get_index_type(source, None, Some(true))?;
                    let include_optional =
                        modifiers.intersects(MappedTypeModifiers::IncludeOptional);
                    let filtered_by_applicability = if include_optional {
                        self.type_checker
                            .ref_(self).intersect_types(Some(target_keys), Some(source_keys))?
                    } else {
                        None
                    };
                    if if include_optional {
                        !filtered_by_applicability
                            .unwrap()
                            .ref_(self)
                            .flags()
                            .intersects(TypeFlags::Never)
                    } else {
                        self.is_related_to(
                            target_keys,
                            source_keys,
                            Some(RecursionFlags::Both),
                            None,
                            None,
                            None,
                        )? != Ternary::False
                    } {
                        let template_type = self
                            .type_checker
                            .ref_(self).get_template_type_from_mapped_type(target)?;
                        let type_parameter = self
                            .type_checker
                            .ref_(self).get_type_parameter_from_mapped_type(target)?;

                        let non_null_component = self
                            .type_checker
                            .ref_(self).extract_types_of_kind(template_type, !TypeFlags::Nullable);
                        if !keys_remapped
                            && non_null_component
                                .ref_(self)
                                .flags()
                                .intersects(TypeFlags::IndexedAccess)
                            && non_null_component
                                .ref_(self)
                                .as_indexed_access_type()
                                .index_type
                                == type_parameter
                        {
                            result = self.is_related_to(
                                source,
                                non_null_component
                                    .ref_(self)
                                    .as_indexed_access_type()
                                    .object_type,
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
                                self.type_checker.ref_(self).get_intersection_type(
                                    &vec![filtered_by_applicability, type_parameter],
                                    Option::<Id<Symbol>>::None,
                                    None,
                                )?
                            } else {
                                type_parameter
                            };
                            let indexed_access_type = self.type_checker.ref_(self).get_indexed_access_type(
                                source,
                                indexing_type,
                                None,
                                Option::<Id<Node>>::None,
                                Option::<Id<Symbol>>::None,
                                None,
                            )?;
                            result = self.is_related_to(
                                indexed_access_type,
                                template_type,
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
        } else if target.ref_(self).flags().intersects(TypeFlags::Conditional) {
            if self.type_checker.ref_(self).is_deeply_nested_type(
                target,
                self.maybe_target_stack().as_ref().unwrap(),
                self.target_depth(),
                Some(10),
            ) {
                self.reset_error_info(save_error_info);
                return Ok(Ternary::Maybe);
            }
            let c = target;
            if c.ref_(self).as_conditional_type().root
                .ref_(self)
                .infer_type_parameters
                .is_none()
                && !self.type_checker.ref_(self).is_distribution_dependent(
                    &c.ref_(self).as_conditional_type().root.ref_(self),
                )?
            {
                let skip_true = !self.type_checker.ref_(self).is_type_assignable_to(
                    self.type_checker.ref_(self).get_permissive_instantiation(
                        c.ref_(self).as_conditional_type().check_type,
                    )?,
                    self.type_checker.ref_(self).get_permissive_instantiation(
                        c.ref_(self).as_conditional_type().extends_type,
                    )?,
                )?;
                let skip_false = !skip_true
                    && self.type_checker.ref_(self).is_type_assignable_to(
                        self.type_checker.ref_(self).get_restrictive_instantiation(
                            c.ref_(self).as_conditional_type().check_type,
                        )?,
                        self.type_checker.ref_(self).get_restrictive_instantiation(
                            c.ref_(self).as_conditional_type().extends_type,
                        )?,
                    )?;
                result = if skip_true {
                    Ternary::True
                } else {
                    self.is_related_to(
                        source,
                        self.type_checker.ref_(self).get_true_type_from_conditional_type(c)?,
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
                            source,
                            self.type_checker.ref_(self).get_false_type_from_conditional_type(c)?,
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
        } else if target
            .ref_(self)
            .flags()
            .intersects(TypeFlags::TemplateLiteral)
        {
            if source
                .ref_(self)
                .flags()
                .intersects(TypeFlags::TemplateLiteral)
            {
                if Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).comparable_relation) {
                    return Ok(
                        if self
                            .type_checker
                            .ref_(self).template_literal_types_definitely_unrelated(source, target)
                        {
                            Ternary::False
                        } else {
                            Ternary::True
                        },
                    );
                }
                self.type_checker.ref_(self).instantiate_type(
                    source,
                    Some(
                        self.type_checker
                            .ref_(self).make_function_type_mapper(ReportUnreliableMarkers),
                    ),
                )?;
            }
            if self
                .type_checker
                .ref_(self).is_type_matched_by_template_literal_type(source, target)?
            {
                return Ok(Ternary::True);
            }
        }

        if source
            .ref_(self)
            .flags()
            .intersects(TypeFlags::TypeVariable)
        {
            if !(source
                .ref_(self)
                .flags()
                .intersects(TypeFlags::IndexedAccess)
                && target
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::IndexedAccess))
            {
                let constraint = self.type_checker.ref_(self).get_constraint_of_type(source)?;
                if match constraint {
                    None => true,
                    Some(constraint) => {
                        source
                            .ref_(self)
                            .flags()
                            .intersects(TypeFlags::TypeParameter)
                            && constraint.ref_(self).flags().intersects(TypeFlags::Any)
                    }
                } {
                    result = self.is_related_to(
                        self.type_checker.ref_(self).empty_object_type(),
                        self.type_checker
                            .ref_(self).extract_types_of_kind(target, !TypeFlags::NonPrimitive),
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
                        constraint.unwrap(),
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
                        self.type_checker.ref_(self).get_type_with_this_argument(
                            constraint.unwrap(),
                            Some(source),
                            None,
                        )?,
                        target,
                        Some(RecursionFlags::Source),
                        Some(
                            report_errors
                                && !(target.ref_(self).flags() & source.ref_(self).flags())
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
        } else if source.ref_(self).flags().intersects(TypeFlags::Index) {
            result = self.is_related_to(
                self.type_checker.ref_(self).keyof_constraint_type(),
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
        } else if source
            .ref_(self)
            .flags()
            .intersects(TypeFlags::TemplateLiteral)
            && !target.ref_(self).flags().intersects(TypeFlags::Object)
        {
            if !target
                .ref_(self)
                .flags()
                .intersects(TypeFlags::TemplateLiteral)
            {
                let constraint = self.type_checker.ref_(self).get_base_constraint_of_type(source)?;
                if let Some(constraint) = constraint.filter(|&constraint| constraint != source) {
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
        } else if source
            .ref_(self)
            .flags()
            .intersects(TypeFlags::StringMapping)
        {
            if target
                .ref_(self)
                .flags()
                .intersects(TypeFlags::StringMapping)
                && source.ref_(self).as_string_mapping_type().symbol()
                    == target.ref_(self).as_string_mapping_type().symbol()
            {
                result = self.is_related_to(
                    source.ref_(self).as_string_mapping_type().type_,
                    target.ref_(self).as_string_mapping_type().type_,
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
                let constraint = self.type_checker.ref_(self).get_base_constraint_of_type(source)?;
                if let Some(constraint) = constraint {
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
        } else if source.ref_(self).flags().intersects(TypeFlags::Conditional) {
            if self.type_checker.ref_(self).is_deeply_nested_type(
                source,
                self.maybe_source_stack().as_ref().unwrap(),
                self.source_depth(),
                Some(10),
            ) {
                self.reset_error_info(save_error_info);
                return Ok(Ternary::Maybe);
            }
            if target.ref_(self).flags().intersects(TypeFlags::Conditional) {
                let source_params = source.ref_(self).as_conditional_type().root
                    .ref_(self)
                    .infer_type_parameters
                    .clone();
                let mut source_extends =
                    source.ref_(self).as_conditional_type().extends_type.clone();
                let mut mapper: Option<Id<TypeMapper>> = None;
                if let Some(source_params) = source_params.as_ref() {
                    let ctx = self.type_checker.ref_(self).create_inference_context(
                        source_params,
                        None,
                        InferenceFlags::None,
                        Some(self.alloc_type_comparer(Box::new(TypeComparerIsRelatedToWorker::new(
                            self.arena_id(),
                        )))),
                    );
                    self.type_checker.ref_(self).infer_types(
                        &ctx.ref_(self).inferences(),
                        target.ref_(self).as_conditional_type().extends_type,
                        source_extends,
                        Some(InferencePriority::NoConstraints | InferencePriority::AlwaysStrict),
                        None,
                    )?;
                    source_extends = self
                        .type_checker
                        .ref_(self).instantiate_type(source_extends, Some(ctx.ref_(self).mapper()))?;
                    mapper = Some(ctx.ref_(self).mapper());
                }
                if self.type_checker.ref_(self).is_type_identical_to(
                    source_extends,
                    target.ref_(self).as_conditional_type().extends_type,
                )? && (self.is_related_to(
                    source.ref_(self).as_conditional_type().check_type,
                    target.ref_(self).as_conditional_type().check_type,
                    Some(RecursionFlags::Both),
                    None,
                    None,
                    None,
                )? != Ternary::False
                    || self.is_related_to(
                        target.ref_(self).as_conditional_type().check_type,
                        source.ref_(self).as_conditional_type().check_type,
                        Some(RecursionFlags::Both),
                        None,
                        None,
                        None,
                    )? != Ternary::False)
                {
                    result = self.is_related_to(
                        self.type_checker.ref_(self).instantiate_type(
                            self.type_checker
                                .ref_(self).get_true_type_from_conditional_type(source)?,
                            mapper,
                        )?,
                        self.type_checker
                            .ref_(self).get_true_type_from_conditional_type(target)?,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        result &= self.is_related_to(
                            self.type_checker
                                .ref_(self).get_false_type_from_conditional_type(source)?,
                            self.type_checker
                                .ref_(self).get_false_type_from_conditional_type(target)?,
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
                    .ref_(self).get_constraint_of_distributive_conditional_type(source)?;
                if let Some(distributive_constraint) = distributive_constraint {
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
                .ref_(self).get_default_constraint_of_conditional_type(source)?;
            // if (defaultConstraint) {
            result = self.is_related_to(
                default_constraint,
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
            if !Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).subtype_relation)
                && !Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).strict_subtype_relation)
                && self.type_checker.ref_(self).is_partial_mapped_type(target)
                && self.type_checker.ref_(self).is_empty_object_type(source)?
            {
                return Ok(Ternary::True);
            }
            if self.type_checker.ref_(self).is_generic_mapped_type(target)? {
                if self.type_checker.ref_(self).is_generic_mapped_type(source)? {
                    result = self.mapped_type_related_to(source, target, report_errors)?;
                    if result != Ternary::False {
                        self.reset_error_info(save_error_info);
                        return Ok(result);
                    }
                }
                return Ok(Ternary::False);
            }
            let source_is_primitive = source.ref_(self).flags().intersects(TypeFlags::Primitive);
            if !Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).identity_relation) {
                source = self.type_checker.ref_(self).get_apparent_type(source)?;
            } else if self.type_checker.ref_(self).is_generic_mapped_type(source)? {
                return Ok(Ternary::False);
            }
            if get_object_flags(&source.ref_(self)).intersects(ObjectFlags::Reference)
                && get_object_flags(&target.ref_(self)).intersects(ObjectFlags::Reference)
                && source.ref_(self).as_type_reference_interface().target()
                    == target.ref_(self).as_type_reference_interface().target()
                && !self.type_checker.ref_(self).is_tuple_type(source)
                && !(get_object_flags(&source.ref_(self)).intersects(ObjectFlags::MarkerType)
                    || get_object_flags(&target.ref_(self)).intersects(ObjectFlags::MarkerType))
            {
                let variances = self.type_checker.ref_(self).get_variances({
                    let target = source.ref_(self).as_type_reference_interface().target();
                    target
                });
                if variances.is_empty() {
                    return Ok(Ternary::Unknown);
                }
                let variance_result = self.relate_variances(
                    &mut result,
                    report_errors,
                    &mut original_error_info,
                    &save_error_info,
                    &mut variance_check_failed,
                    Some(&*self.type_checker.ref_(self).get_type_arguments(source)?),
                    Some(&*self.type_checker.ref_(self).get_type_arguments(target)?),
                    &variances,
                    intersection_state,
                )?;
                if let Some(variance_result) = variance_result {
                    return Ok(variance_result);
                }
            } else if if self.type_checker.ref_(self).is_readonly_array_type(target) {
                self.type_checker.ref_(self).is_array_type(source) || self.type_checker.ref_(self).is_tuple_type(source)
            } else {
                self.type_checker.ref_(self).is_array_type(target)
                    && self.type_checker.ref_(self).is_tuple_type(source)
                    && !source
                        .ref_(self)
                        .as_type_reference_interface()
                        .target()
                        .ref_(self)
                        .as_tuple_type()
                        .readonly
            } {
                if !Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).identity_relation) {
                    return self.is_related_to(
                        self.type_checker
                            .ref_(self).get_index_type_of_type_(source, self.type_checker.ref_(self).number_type())?
                            .unwrap_or_else(|| self.type_checker.ref_(self).any_type()),
                        self.type_checker
                            .ref_(self).get_index_type_of_type_(target, self.type_checker.ref_(self).number_type())?
                            .unwrap_or_else(|| self.type_checker.ref_(self).any_type()),
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        None,
                    );
                } else {
                    return Ok(Ternary::False);
                }
            } else if (Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).subtype_relation)
                || Rc::ptr_eq(&self.relation, &self.type_checker.ref_(self).strict_subtype_relation))
                && self.type_checker.ref_(self).is_empty_object_type(target)?
                && get_object_flags(&target.ref_(self)).intersects(ObjectFlags::FreshLiteral)
                && !self.type_checker.ref_(self).is_empty_object_type(source)?
            {
                return Ok(Ternary::False);
            }
            if source
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::Intersection)
                && target.ref_(self).flags().intersects(TypeFlags::Object)
            {
                let report_structural_errors = report_errors
                    && are_option_rcs_equal(
                        self.maybe_error_info().as_ref(),
                        save_error_info.error_info.as_ref(),
                    )
                    && !source_is_primitive;
                result = self.properties_related_to(
                    source,
                    target,
                    report_structural_errors,
                    None,
                    intersection_state,
                )?;
                if result != Ternary::False {
                    result &= self.signatures_related_to(
                        source,
                        target,
                        SignatureKind::Call,
                        report_structural_errors,
                    )?;
                    if result != Ternary::False {
                        result &= self.signatures_related_to(
                            source,
                            target,
                            SignatureKind::Construct,
                            report_structural_errors,
                        )?;
                        if result != Ternary::False {
                            result &= self.index_signatures_related_to(
                                source,
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
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::Intersection)
                && target.ref_(self).flags().intersects(TypeFlags::Union)
            {
                let object_only_target = self.type_checker.ref_(self).extract_types_of_kind(
                    target,
                    TypeFlags::Object | TypeFlags::Intersection | TypeFlags::Substitution,
                );
                if object_only_target
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::Union)
                {
                    let result =
                        self.type_related_to_discriminated_type(source, object_only_target)?;
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
    fn call(
        &self,
        checker: &TypeChecker,
        p: Id<Type>, /*TypeParameter*/
    ) -> io::Result<Id<Type>> {
        if let Some(outofband_variance_marker_handler) =
            checker.maybe_outofband_variance_marker_handler()
        {
            if p == checker.marker_super_type()
                || p == checker.marker_sub_type()
                || p == checker.marker_other_type()
            {
                outofband_variance_marker_handler.ref_(checker).call(false);
            }
        }
        Ok(p)
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ReportUnreliableMarkers;
impl TypeMapperCallback for ReportUnreliableMarkers {
    fn call(
        &self,
        checker: &TypeChecker,
        p: Id<Type>, /*TypeParameter*/
    ) -> io::Result<Id<Type>> {
        if let Some(outofband_variance_marker_handler) =
            checker.maybe_outofband_variance_marker_handler()
        {
            if p == checker.marker_super_type()
                || p == checker.marker_sub_type()
                || p == checker.marker_other_type()
            {
                outofband_variance_marker_handler.ref_(checker).call(true);
            }
        }
        Ok(p)
    }
}

#[derive(Trace, Finalize)]
pub(super) struct TypeComparerIsRelatedToWorker {
    check_type_related_to: Id<CheckTypeRelatedTo>,
}

impl TypeComparerIsRelatedToWorker {
    pub fn new(check_type_related_to: Id<CheckTypeRelatedTo>) -> Self {
        Self {
            check_type_related_to,
        }
    }
}

impl TypeComparer for TypeComparerIsRelatedToWorker {
    fn call(&self, s: Id<Type>, t: Id<Type>, report_errors: Option<bool>) -> io::Result<Ternary> {
        self.check_type_related_to.ref_(self).is_related_to_worker(
            s,
            t,
            report_errors.unwrap_or(false), // the default isn't in the Typescript version but that appears to be a type-checking bug there
        )
    }
}

impl HasArena for TypeComparerIsRelatedToWorker {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct RecursiveTypeRelatedToOutofbandVarianceMarkerHandler {
    #[unsafe_ignore_trace]
    propagating_variance_flags: Rc<Cell<RelationComparisonResult>>,
    original_handler: Id<Box<dyn OutofbandVarianceMarkerHandler>>,
}

impl RecursiveTypeRelatedToOutofbandVarianceMarkerHandler {
    pub fn new(
        propagating_variance_flags: Rc<Cell<RelationComparisonResult>>,
        original_handler: Id<Box<dyn OutofbandVarianceMarkerHandler>>,
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
        self.original_handler.ref_(self).call(only_unreliable);
    }
}

impl HasArena for RecursiveTypeRelatedToOutofbandVarianceMarkerHandler {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}
