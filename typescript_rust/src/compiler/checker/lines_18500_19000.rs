use std::{cell::Cell, io, rc::Rc};

use id_arena::Id;
use regex::{Captures, Regex};

use super::{
    CheckTypeRelatedTo, ExpandingFlags, IntersectionState, MappedTypeModifiers, RecursionFlags,
};
use crate::{
    are_option_rcs_equal, get_object_flags, impl_has_arena, released, try_map, AccessFlags,
    AllArenas, DiagnosticMessageChain, HasArena, InArena, InferenceFlags, InferencePriority, Node,
    NodeInterface, ObjectFlags, ObjectTypeInterface, OutofbandVarianceMarkerHandler,
    RelationComparisonResult, SignatureKind, Symbol, SymbolInterface, Ternary, Type, TypeChecker,
    TypeComparer, TypeComparerCall, TypeFlags, TypeInterface, TypeMapper, TypeMapperCallback,
    UnionOrIntersectionTypeInterface, VarianceFlags,
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
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>, /*UnionOrIntersectionType*/
        target: Id<Type>, /*UnionOrIntersectionType*/
    ) -> io::Result<Ternary> {
        let mut result = Ternary::True;
        let source_types = source
            .ref_(arena)
            .as_union_or_intersection_type_interface()
            .types()
            .to_owned();
        for source_type in source_types {
            let related =
                Self::type_related_to_some_type(self_, arena, source_type, target, false)?;
            if related == Ternary::False {
                return Ok(Ternary::False);
            }
            result &= related;
        }
        Ok(result)
    }

    pub(super) fn type_related_to_some_type(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>, /*UnionOrIntersectionType*/
        report_errors: bool,
    ) -> io::Result<Ternary> {
        let ref target_types = target
            .ref_(arena)
            .as_union_or_intersection_type_interface()
            .types()
            .to_owned();
        if target.ref_(arena).flags().intersects(TypeFlags::Union) {
            if self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .contains_type(target_types, source)
            {
                return Ok(Ternary::True);
            }
            let match_ = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_matching_union_constituent_for_type(target, source)?;
            if let Some(match_) = match_ {
                let related = Self::is_related_to(
                    self_,
                    arena,
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
            let related = Self::is_related_to(
                self_,
                arena,
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
            let best_matching_type = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_best_matching_type(
                    source,
                    target,
                    Some(|source: Id<Type>, target: Id<Type>| {
                        Self::is_related_to(self_, arena, source, target, None, None, None, None)
                    }),
                )?;
            Self::is_related_to(
                self_,
                arena,
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
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>, /*IntersectionType*/
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        let mut result = Ternary::True;
        let target_types = target
            .ref_(arena)
            .as_union_or_intersection_type_interface()
            .types()
            .to_owned();
        for target_type in target_types {
            let related = Self::is_related_to(
                self_,
                arena,
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
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>, /*UnionOrIntersectionType*/
        target: Id<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        let source_ref = source.ref_(arena);
        let source_types = source_ref.as_union_or_intersection_type_interface().types();
        if source.ref_(arena).flags().intersects(TypeFlags::Union)
            && self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .contains_type(source_types, target)
        {
            return Ok(Ternary::True);
        }
        let len = source_types.len();
        for i in 0..len {
            let related = Self::is_related_to(
                self_,
                arena,
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
                .ref_(self)
                .extract_types_of_kind(target, !TypeFlags::Undefined);
        }
        target
    }

    pub(super) fn each_type_related_to_type(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>, /*UnionOrIntersectionType*/
        target: Id<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        let mut result = Ternary::True;
        let source_types = &source
            .ref_(arena)
            .as_union_or_intersection_type_interface()
            .types()
            .to_owned();
        let undefined_stripped_target = self_
            .ref_(arena)
            .get_undefined_stripped_target_if_needed(source, target);
        for (i, source_type) in source_types.into_iter().enumerate() {
            let source_type = *source_type;
            if undefined_stripped_target
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::Union)
            {
                let undefined_stripped_target_types = undefined_stripped_target
                    .ref_(arena)
                    .as_union_type()
                    .types()
                    .to_owned();
                if source_types.len() >= undefined_stripped_target_types.len()
                    && source_types.len() % undefined_stripped_target_types.len() == 0
                {
                    let related = Self::is_related_to(
                        self_,
                        arena,
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
            let related = Self::is_related_to(
                self_,
                arena,
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
        self_: Id<Self>,
        arena: &impl HasArena,
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
            && Rc::ptr_eq(
                &self_.ref_(arena).relation,
                &self_.ref_(arena).type_checker.ref_(arena).identity_relation,
            )
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
                    related = if Rc::ptr_eq(
                        &self_.ref_(arena).relation,
                        &self_.ref_(arena).type_checker.ref_(arena).identity_relation,
                    ) {
                        Self::is_related_to(
                            self_,
                            arena,
                            s,
                            t,
                            Some(RecursionFlags::Both),
                            Some(false),
                            None,
                            None,
                        )?
                    } else {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .compare_types_identical(s, t)?
                    };
                } else if variance == VarianceFlags::Covariant {
                    related = Self::is_related_to(
                        self_,
                        arena,
                        s,
                        t,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        Some(intersection_state),
                    )?;
                } else if variance == VarianceFlags::Contravariant {
                    related = Self::is_related_to(
                        self_,
                        arena,
                        t,
                        s,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        Some(intersection_state),
                    )?;
                } else if variance == VarianceFlags::Bivariant {
                    related = Self::is_related_to(
                        self_,
                        arena,
                        t,
                        s,
                        Some(RecursionFlags::Both),
                        Some(false),
                        None,
                        None,
                    )?;
                    if related == Ternary::False {
                        related = Self::is_related_to(
                            self_,
                            arena,
                            s,
                            t,
                            Some(RecursionFlags::Both),
                            Some(report_errors),
                            None,
                            Some(intersection_state),
                        )?;
                    }
                } else {
                    related = Self::is_related_to(
                        self_,
                        arena,
                        s,
                        t,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        Some(intersection_state),
                    )?;
                    if related != Ternary::False {
                        related &= Self::is_related_to(
                            self_,
                            arena,
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
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
        recursion_flags: RecursionFlags,
    ) -> io::Result<Ternary> {
        if self_.ref_(arena).overflow() {
            return Ok(Ternary::False);
        }
        let id = self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .get_relation_key(
                source,
                target,
                intersection_state
                    | (if self_.ref_(arena).in_property_check() {
                        IntersectionState::InPropertyCheck
                    } else {
                        IntersectionState::None
                    }),
                &(*self_.ref_(arena).relation).borrow(),
            )?;
        let entry = (*self_.ref_(arena).relation)
            .borrow()
            .get(&id)
            .map(Clone::clone);
        if let Some(entry) = entry.as_ref() {
            if report_errors
                && entry.intersects(RelationComparisonResult::Failed)
                && !entry.intersects(RelationComparisonResult::Reported)
            {
            } else {
                if self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .maybe_outofband_variance_marker_handler()
                    .is_some()
                {
                    let saved = *entry & RelationComparisonResult::ReportsMask;
                    if saved.intersects(RelationComparisonResult::ReportsUnmeasurable) {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .instantiate_type(
                                source,
                                Some(
                                    self_
                                        .ref_(arena)
                                        .type_checker
                                        .ref_(arena)
                                        .make_function_type_mapper(ReportUnmeasurableMarkers),
                                ),
                            )?;
                    }
                    if saved.intersects(RelationComparisonResult::ReportsUnreliable) {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .instantiate_type(
                                source,
                                Some(
                                    self_
                                        .ref_(arena)
                                        .type_checker
                                        .ref_(arena)
                                        .make_function_type_mapper(ReportUnreliableMarkers),
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
        if self_.ref_(arena).maybe_keys().is_none() {
            *self_.ref_(arena).maybe_keys() = Some(vec![]);
            *self_.ref_(arena).maybe_source_stack() = Some(vec![]);
            *self_.ref_(arena).maybe_target_stack() = Some(vec![]);
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
            for i in 0..self_.ref_(arena).maybe_count() {
                if matches!(
                    self_.ref_(arena).maybe_keys().as_ref().unwrap().get(i),
                    Some(maybe_key) if &id == maybe_key || &broadest_equivalent_id == maybe_key
                ) {
                    return Ok(Ternary::Maybe);
                }
            }
            if self_.ref_(arena).source_depth() == 100 || self_.ref_(arena).target_depth() == 100 {
                self_.ref_(arena).set_overflow(true);
                return Ok(Ternary::False);
            }
        }
        let maybe_start = self_.ref_(arena).maybe_count();
        self_
            .ref_(arena)
            .maybe_keys()
            .as_mut()
            .unwrap()
            .push(id.clone());
        self_
            .ref_(arena)
            .set_maybe_count(self_.ref_(arena).maybe_count() + 1);
        let save_expanding_flags = self_.ref_(arena).expanding_flags();
        if recursion_flags.intersects(RecursionFlags::Source) {
            self_
                .ref_(arena)
                .maybe_source_stack()
                .as_mut()
                .unwrap()
                .push(source);
            self_
                .ref_(arena)
                .set_source_depth(self_.ref_(arena).source_depth() + 1);
            if !self_
                .ref_(arena)
                .expanding_flags()
                .intersects(ExpandingFlags::Source)
                && self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_deeply_nested_type(
                        source,
                        self_.ref_(arena).maybe_source_stack().as_deref().unwrap(),
                        self_.ref_(arena).source_depth(),
                        None,
                    )
            {
                self_.ref_(arena).set_expanding_flags(
                    self_.ref_(arena).expanding_flags() | ExpandingFlags::Source,
                );
            }
        }
        if recursion_flags.intersects(RecursionFlags::Target) {
            self_
                .ref_(arena)
                .maybe_target_stack()
                .as_mut()
                .unwrap()
                .push(target);
            self_
                .ref_(arena)
                .set_target_depth(self_.ref_(arena).target_depth() + 1);
            if !self_
                .ref_(arena)
                .expanding_flags()
                .intersects(ExpandingFlags::Target)
                && self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_deeply_nested_type(
                        target,
                        self_.ref_(arena).maybe_target_stack().as_deref().unwrap(),
                        self_.ref_(arena).target_depth(),
                        None,
                    )
            {
                self_.ref_(arena).set_expanding_flags(
                    self_.ref_(arena).expanding_flags() | ExpandingFlags::Target,
                );
            }
        }
        let mut original_handler: Option<Id<Box<dyn OutofbandVarianceMarkerHandler>>> = None;
        let propagating_variance_flags: Rc<Cell<RelationComparisonResult>> =
            Rc::new(Cell::new(RelationComparisonResult::None));
        if let Some(outofband_variance_marker_handler) = self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .maybe_outofband_variance_marker_handler()
        {
            original_handler = Some(outofband_variance_marker_handler);
            self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .set_outofband_variance_marker_handler(Some(
                    self_
                        .ref_(arena)
                        .alloc_outofband_variance_marker_handler(Box::new(
                            RecursiveTypeRelatedToOutofbandVarianceMarkerHandler::new(
                                propagating_variance_flags.clone(),
                                original_handler.clone().unwrap(),
                                arena,
                            ),
                        )),
                ));
        }

        if self_.ref_(arena).expanding_flags() == ExpandingFlags::Both {
            // tracing?.instant(tracing.Phase.CheckTypes, "recursiveTypeRelatedTo_DepthLimit", {
            //     sourceId: source.id,
            //     sourceIdStack: sourceStack.map(t => t.id),
            //     targetId: target.id,
            //     targetIdStack: targetStack.map(t => t.id),
            //     depth: sourceDepth,
            //     targetDepth
            // });
        }

        let result = if self_.ref_(arena).expanding_flags() != ExpandingFlags::Both {
            Self::structured_type_related_to(
                self_,
                arena,
                source,
                target,
                report_errors,
                intersection_state,
            )?
        } else {
            Ternary::Maybe
        };
        if self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .maybe_outofband_variance_marker_handler()
            .is_some()
        {
            self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .set_outofband_variance_marker_handler(original_handler);
        }
        if recursion_flags.intersects(RecursionFlags::Source) {
            self_
                .ref_(arena)
                .set_source_depth(self_.ref_(arena).source_depth() - 1);
            self_
                .ref_(arena)
                .maybe_source_stack()
                .as_mut()
                .unwrap()
                .truncate(self_.ref_(arena).source_depth());
        }
        if recursion_flags.intersects(RecursionFlags::Target) {
            self_
                .ref_(arena)
                .set_target_depth(self_.ref_(arena).target_depth() - 1);
            self_
                .ref_(arena)
                .maybe_target_stack()
                .as_mut()
                .unwrap()
                .truncate(self_.ref_(arena).target_depth());
        }
        self_.ref_(arena).set_expanding_flags(save_expanding_flags);
        if result != Ternary::False {
            if result == Ternary::True
                || self_.ref_(arena).source_depth() == 0 && self_.ref_(arena).target_depth() == 0
            {
                if matches!(result, Ternary::True | Ternary::Maybe) {
                    for i in maybe_start..self_.ref_(arena).maybe_count() {
                        self_.ref_(arena).relation.borrow_mut().insert(
                            self_.ref_(arena).maybe_keys().as_ref().unwrap()[i].clone(),
                            RelationComparisonResult::Succeeded | propagating_variance_flags.get(),
                        );
                    }
                }
                self_.ref_(arena).set_maybe_count(maybe_start);
                self_
                    .ref_(arena)
                    .maybe_keys()
                    .as_mut()
                    .unwrap()
                    .truncate(self_.ref_(arena).maybe_count());
            }
        } else {
            self_.ref_(arena).relation.borrow_mut().insert(
                id,
                (if report_errors {
                    RelationComparisonResult::Reported
                } else {
                    RelationComparisonResult::None
                }) | RelationComparisonResult::Failed
                    | propagating_variance_flags.get(),
            );
            self_.ref_(arena).set_maybe_count(maybe_start);
            self_
                .ref_(arena)
                .maybe_keys()
                .as_mut()
                .unwrap()
                .truncate(self_.ref_(arena).maybe_count());
        }
        Ok(result)
    }

    pub(super) fn structured_type_related_to(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        // tracing?.push(tracing.Phase.CheckTypes, "structuredTypeRelatedTo", { sourceId: source.id, targetId: target.id });
        let result = Self::structured_type_related_to_worker(
            self_,
            arena,
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
        self_: Id<Self>,
        arena: &impl HasArena,
        mut source: Id<Type>,
        target: Id<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> io::Result<Ternary> {
        if intersection_state.intersects(IntersectionState::PropertyCheck) {
            return Self::properties_related_to(
                self_,
                arena,
                source,
                target,
                report_errors,
                None,
                IntersectionState::None,
            );
        }
        if intersection_state.intersects(IntersectionState::UnionIntersectionCheck) {
            if source.ref_(arena).flags().intersects(TypeFlags::Union) {
                return Ok(
                    if Rc::ptr_eq(
                        &self_.ref_(arena).relation,
                        &self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .comparable_relation,
                    ) {
                        Self::some_type_related_to_type(
                            self_,
                            arena,
                            source,
                            target,
                            report_errors
                                && !source.ref_(arena).flags().intersects(TypeFlags::Primitive),
                            intersection_state & !IntersectionState::UnionIntersectionCheck,
                        )?
                    } else {
                        Self::each_type_related_to_type(
                            self_,
                            arena,
                            source,
                            target,
                            report_errors
                                && !source.ref_(arena).flags().intersects(TypeFlags::Primitive),
                            intersection_state & !IntersectionState::UnionIntersectionCheck,
                        )?
                    },
                );
            }
            if target.ref_(arena).flags().intersects(TypeFlags::Union) {
                return Self::type_related_to_some_type(
                    self_,
                    arena,
                    released!(self_.ref_(arena).type_checker)
                        .ref_(arena)
                        .get_regular_type_of_object_literal(source)?,
                    target,
                    report_errors
                        && !source.ref_(arena).flags().intersects(TypeFlags::Primitive)
                        && !target.ref_(arena).flags().intersects(TypeFlags::Primitive),
                );
            }
            if target
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::Intersection)
            {
                return Self::type_related_to_each_type(
                    self_,
                    arena,
                    self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_regular_type_of_object_literal(source)?,
                    target,
                    report_errors,
                    IntersectionState::Target,
                );
            }
            if Rc::ptr_eq(
                &self_.ref_(arena).relation,
                &self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .comparable_relation,
            ) && target.ref_(arena).flags().intersects(TypeFlags::Primitive)
            {
                let constraints = try_map(
                    source
                        .ref_(arena)
                        .as_union_or_intersection_type_interface()
                        .types()
                        .to_owned(),
                    |type_: Id<Type>, _| {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_base_constraint_or_type(type_)
                    },
                )?;
                if &constraints
                    != source
                        .ref_(arena)
                        .as_union_or_intersection_type_interface()
                        .types()
                {
                    source = self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_intersection_type(&constraints, Option::<Id<Symbol>>::None, None)?;
                    if !source
                        .ref_(arena)
                        .flags()
                        .intersects(TypeFlags::Intersection)
                    {
                        return Self::is_related_to(
                            self_,
                            arena,
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
            return Self::some_type_related_to_type(
                self_,
                arena,
                source,
                target,
                false,
                IntersectionState::Source,
            );
        }
        let flags = source.ref_(arena).flags() & target.ref_(arena).flags();
        if Rc::ptr_eq(
            &self_.ref_(arena).relation,
            &self_.ref_(arena).type_checker.ref_(arena).identity_relation,
        ) && !flags.intersects(TypeFlags::Object)
        {
            if flags.intersects(TypeFlags::Index) {
                return Self::is_related_to(
                    self_,
                    arena,
                    source.ref_(arena).as_index_type().type_,
                    target.ref_(arena).as_index_type().type_,
                    Some(RecursionFlags::Both),
                    Some(false),
                    None,
                    None,
                );
            }
            let mut result: Ternary;
            if flags.intersects(TypeFlags::IndexedAccess) {
                result = Self::is_related_to(
                    self_,
                    arena,
                    source.ref_(arena).as_indexed_access_type().object_type,
                    target.ref_(arena).as_indexed_access_type().object_type,
                    Some(RecursionFlags::Both),
                    Some(false),
                    None,
                    None,
                )?;
                if result != Ternary::False {
                    result &= Self::is_related_to(
                        self_,
                        arena,
                        source.ref_(arena).as_indexed_access_type().index_type,
                        target.ref_(arena).as_indexed_access_type().index_type,
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
                if source
                    .ref_(arena)
                    .as_conditional_type()
                    .root
                    .ref_(arena)
                    .is_distributive
                    == target
                        .ref_(arena)
                        .as_conditional_type()
                        .root
                        .ref_(arena)
                        .is_distributive
                {
                    result = Self::is_related_to(
                        self_,
                        arena,
                        source.ref_(arena).as_conditional_type().check_type,
                        target.ref_(arena).as_conditional_type().check_type,
                        Some(RecursionFlags::Both),
                        Some(false),
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        result &= Self::is_related_to(
                            self_,
                            arena,
                            source.ref_(arena).as_conditional_type().extends_type,
                            target.ref_(arena).as_conditional_type().extends_type,
                            Some(RecursionFlags::Both),
                            Some(false),
                            None,
                            None,
                        )?;
                        if result != Ternary::False {
                            result &= Self::is_related_to(
                                self_,
                                arena,
                                self_
                                    .ref_(arena)
                                    .type_checker
                                    .ref_(arena)
                                    .get_true_type_from_conditional_type(source)?,
                                self_
                                    .ref_(arena)
                                    .type_checker
                                    .ref_(arena)
                                    .get_true_type_from_conditional_type(target)?,
                                Some(RecursionFlags::Both),
                                Some(false),
                                None,
                                None,
                            )?;
                            if result != Ternary::False {
                                result &= Self::is_related_to(
                                    self_,
                                    arena,
                                    self_
                                        .ref_(arena)
                                        .type_checker
                                        .ref_(arena)
                                        .get_false_type_from_conditional_type(source)?,
                                    self_
                                        .ref_(arena)
                                        .type_checker
                                        .ref_(arena)
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
                return Self::is_related_to(
                    self_,
                    arena,
                    source.ref_(arena).as_substitution_type().substitute,
                    target.ref_(arena).as_substitution_type().substitute,
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
        let save_error_info = self_.ref_(arena).capture_error_calculation_state();

        if source
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::Object | TypeFlags::Conditional)
        {
            if let Some(source_alias_symbol) = {
                let alias_symbol = source.ref_(arena).maybe_alias_symbol();
                alias_symbol
            } {
                if let Some(source_alias_type_arguments) = {
                    let alias_type_arguments = source.ref_(arena).maybe_alias_type_arguments();
                    alias_type_arguments
                }
                .as_ref()
                {
                    if matches!(
                        target.ref_(arena).maybe_alias_symbol(),
                        Some(target_alias_symbol) if
                            source_alias_symbol ==
                            target_alias_symbol
                    ) {
                        if !(matches!(
                            source
                                .ref_(arena)
                                .maybe_alias_type_arguments_contains_marker(),
                            Some(true)
                        ) || matches!(
                            target
                                .ref_(arena)
                                .maybe_alias_type_arguments_contains_marker(),
                            Some(true)
                        )) {
                            let variances = released!(self_.ref_(arena).type_checker)
                                .ref_(arena)
                                .get_alias_variances(source_alias_symbol)?;
                            if variances.is_empty() {
                                return Ok(Ternary::Unknown);
                            }
                            let variance_result = Self::relate_variances(
                                self_,
                                arena,
                                &mut result,
                                report_errors,
                                &mut original_error_info,
                                &save_error_info,
                                &mut variance_check_failed,
                                Some(source_alias_type_arguments),
                                target.ref_(arena).maybe_alias_type_arguments().as_deref(),
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

        if self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .is_single_element_generic_tuple_type(source)
            && !source
                .ref_(arena)
                .as_type_reference()
                .target
                .ref_(arena)
                .as_tuple_type()
                .readonly
        {
            result = Self::is_related_to(
                self_,
                arena,
                self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_type_arguments(source)?[0],
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
        if self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .is_single_element_generic_tuple_type(target)
            && (target
                .ref_(arena)
                .as_type_reference()
                .target
                .ref_(arena)
                .as_tuple_type()
                .readonly
                || self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_mutable_array_or_tuple(
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_base_constraint_of_type(source)?
                            .unwrap_or_else(|| source.clone()),
                    ))
        {
            result = Self::is_related_to(
                self_,
                arena,
                source,
                self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_type_arguments(target)?[0],
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
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::TypeParameter)
        {
            if get_object_flags(&source.ref_(arena)).intersects(ObjectFlags::Mapped)
                && source
                    .ref_(arena)
                    .as_mapped_type()
                    .declaration
                    .ref_(arena)
                    .as_mapped_type_node()
                    .name_type
                    .is_none()
                && Self::is_related_to(
                    self_,
                    arena,
                    self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_index_type(target, None, None)?,
                    self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_constraint_type_from_mapped_type(source)?,
                    Some(RecursionFlags::Both),
                    None,
                    None,
                    None,
                )? != Ternary::False
            {
                if !self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_mapped_type_modifiers(source)
                    .intersects(MappedTypeModifiers::IncludeOptional)
                {
                    let template_type = self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_template_type_from_mapped_type(source)?;
                    let indexed_access_type = self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_indexed_access_type(
                            target,
                            self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .get_type_parameter_from_mapped_type(source)?,
                            None,
                            Option::<Id<Node>>::None,
                            Option::<Id<Symbol>>::None,
                            None,
                        )?;
                    result = Self::is_related_to(
                        self_,
                        arena,
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
        } else if target.ref_(arena).flags().intersects(TypeFlags::Index) {
            let target_type = target.ref_(arena).as_index_type().type_;
            if source.ref_(arena).flags().intersects(TypeFlags::Index) {
                result = Self::is_related_to(
                    self_,
                    arena,
                    target_type,
                    source.ref_(arena).as_index_type().type_,
                    Some(RecursionFlags::Both),
                    Some(false),
                    None,
                    None,
                )?;
                if result != Ternary::False {
                    return Ok(result);
                }
            }
            if self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_tuple_type(target_type)
            {
                result = Self::is_related_to(
                    self_,
                    arena,
                    source,
                    self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
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
                let constraint = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_simplified_type_or_constraint(target_type)?;
                if let Some(constraint) = constraint {
                    if Self::is_related_to(
                        self_,
                        arena,
                        source,
                        self_.ref_(arena).type_checker.ref_(arena).get_index_type(
                            constraint,
                            Some({
                                let strings_only = target.ref_(arena).as_index_type().strings_only;
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
                } else if self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_generic_mapped_type(target_type)?
                {
                    let name_type = self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_name_type_from_mapped_type(target_type)?;
                    let constraint_type = self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_constraint_type_from_mapped_type(target_type)?;
                    let target_keys: Id<Type>;
                    if let Some(name_type) = name_type.filter(|_| {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .is_mapped_type_with_keyof_constraint_declaration(target_type)
                    }) {
                        let modifiers_type = self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_apparent_type(
                                self_
                                    .ref_(arena)
                                    .type_checker
                                    .ref_(arena)
                                    .get_modifiers_type_from_mapped_type(target_type)?,
                            )?;
                        let mut mapped_keys: Vec<Id<Type>> = vec![];
                        self_.ref_(arena).type_checker
                            .ref_(arena).try_for_each_mapped_type_property_key_type_and_index_signature_key_type(
                                modifiers_type,
                                TypeFlags::StringOrNumberLiteralOrUnique,
                                false,
                                |t| {
                                    mapped_keys.push(
                                        self_.ref_(arena).type_checker.ref_(arena).instantiate_type(
                                            name_type,
                                            Some(
                                                self_.ref_(arena).type_checker.ref_(arena).append_type_mapping(
                                                    target_type.ref_(arena).as_mapped_type().maybe_mapper(),
                                                    self_.ref_(arena)
                                                        .type_checker
                                                        .ref_(arena).get_type_parameter_from_mapped_type(
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
                        target_keys = self_.ref_(arena).type_checker.ref_(arena).get_union_type(
                            &mapped_keys,
                            None,
                            Option::<Id<Symbol>>::None,
                            None,
                            None,
                        )?;
                    } else {
                        target_keys = name_type.unwrap_or(constraint_type);
                    }
                    if Self::is_related_to(
                        self_,
                        arena,
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
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::IndexedAccess)
        {
            if source
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::IndexedAccess)
            {
                result = Self::is_related_to(
                    self_,
                    arena,
                    source.ref_(arena).as_indexed_access_type().object_type,
                    target.ref_(arena).as_indexed_access_type().object_type,
                    Some(RecursionFlags::Both),
                    Some(report_errors),
                    None,
                    None,
                )?;
                if result != Ternary::False {
                    result &= Self::is_related_to(
                        self_,
                        arena,
                        source.ref_(arena).as_indexed_access_type().index_type,
                        target.ref_(arena).as_indexed_access_type().index_type,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        None,
                    )?;
                }
                if result != Ternary::False {
                    self_.ref_(arena).reset_error_info(save_error_info);
                    return Ok(result);
                }
                if report_errors {
                    original_error_info = self_.ref_(arena).maybe_error_info();
                }
            }
            if Rc::ptr_eq(
                &self_.ref_(arena).relation,
                &self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .assignable_relation,
            ) || Rc::ptr_eq(
                &self_.ref_(arena).relation,
                &self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .comparable_relation,
            ) {
                let object_type = target.ref_(arena).as_indexed_access_type().object_type;
                let index_type = target.ref_(arena).as_indexed_access_type().index_type;
                let base_object_type = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_base_constraint_of_type(object_type)?
                    .unwrap_or_else(|| object_type.clone());
                let base_index_type = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_base_constraint_of_type(index_type)?
                    .unwrap_or_else(|| index_type.clone());
                if !self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_generic_object_type(base_object_type)?
                    && !self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .is_generic_index_type(base_index_type)?
                {
                    let access_flags = AccessFlags::Writing
                        | if base_object_type != object_type {
                            AccessFlags::NoIndexSignatures
                        } else {
                            AccessFlags::None
                        };
                    let constraint = released!(self_.ref_(arena).type_checker)
                        .ref_(arena)
                        .get_indexed_access_type_or_undefined(
                            base_object_type,
                            base_index_type,
                            Some(access_flags),
                            Option::<Id<Node>>::None,
                            Option::<Id<Symbol>>::None,
                            None,
                        )?;
                    if let Some(constraint) = constraint {
                        if report_errors && original_error_info.is_some() {
                            self_.ref_(arena).reset_error_info(save_error_info.clone());
                        }
                        result = Self::is_related_to(
                            self_,
                            arena,
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
                                if let Some(error_info) = self_.ref_(arena).maybe_error_info() {
                                    *self_.ref_(arena).maybe_error_info_mut() = Some(
                                        if self_.ref_(arena).count_message_chain_breadth(Some(
                                            &*vec![&*original_error_info],
                                        )) <= self_
                                            .ref_(arena)
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
        } else if self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .is_generic_mapped_type(target)?
            && !Rc::ptr_eq(
                &self_.ref_(arena).relation,
                &self_.ref_(arena).type_checker.ref_(arena).identity_relation,
            )
        {
            let keys_remapped = target
                .ref_(arena)
                .as_mapped_type()
                .declaration
                .ref_(arena)
                .as_mapped_type_node()
                .name_type
                .is_some();
            let template_type = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_template_type_from_mapped_type(target)?;
            let modifiers = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_mapped_type_modifiers(target);
            if !modifiers.intersects(MappedTypeModifiers::ExcludeOptional) {
                if !keys_remapped
                    && template_type
                        .ref_(arena)
                        .flags()
                        .intersects(TypeFlags::IndexedAccess)
                    && template_type
                        .ref_(arena)
                        .as_indexed_access_type()
                        .object_type
                        == source
                    && template_type
                        .ref_(arena)
                        .as_indexed_access_type()
                        .index_type
                        == self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_type_parameter_from_mapped_type(target)?
                {
                    return Ok(Ternary::True);
                }
                if !self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_generic_mapped_type(source)?
                {
                    let target_keys = if keys_remapped {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_name_type_from_mapped_type(target)?
                            .unwrap()
                    } else {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_constraint_type_from_mapped_type(target)?
                    };
                    let source_keys = self_.ref_(arena).type_checker.ref_(arena).get_index_type(
                        source,
                        None,
                        Some(true),
                    )?;
                    let include_optional =
                        modifiers.intersects(MappedTypeModifiers::IncludeOptional);
                    let filtered_by_applicability = if include_optional {
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .intersect_types(Some(target_keys), Some(source_keys))?
                    } else {
                        None
                    };
                    if if include_optional {
                        !filtered_by_applicability
                            .unwrap()
                            .ref_(arena)
                            .flags()
                            .intersects(TypeFlags::Never)
                    } else {
                        Self::is_related_to(
                            self_,
                            arena,
                            target_keys,
                            source_keys,
                            Some(RecursionFlags::Both),
                            None,
                            None,
                            None,
                        )? != Ternary::False
                    } {
                        let template_type = self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_template_type_from_mapped_type(target)?;
                        let type_parameter = self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_type_parameter_from_mapped_type(target)?;

                        let non_null_component = self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .extract_types_of_kind(template_type, !TypeFlags::Nullable);
                        if !keys_remapped
                            && non_null_component
                                .ref_(arena)
                                .flags()
                                .intersects(TypeFlags::IndexedAccess)
                            && non_null_component
                                .ref_(arena)
                                .as_indexed_access_type()
                                .index_type
                                == type_parameter
                        {
                            result = Self::is_related_to(
                                self_,
                                arena,
                                source,
                                non_null_component
                                    .ref_(arena)
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
                                self_
                                    .ref_(arena)
                                    .type_checker
                                    .ref_(arena)
                                    .get_intersection_type(
                                        &vec![filtered_by_applicability, type_parameter],
                                        Option::<Id<Symbol>>::None,
                                        None,
                                    )?
                            } else {
                                type_parameter
                            };
                            let indexed_access_type = self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .get_indexed_access_type(
                                    source,
                                    indexing_type,
                                    None,
                                    Option::<Id<Node>>::None,
                                    Option::<Id<Symbol>>::None,
                                    None,
                                )?;
                            result = Self::is_related_to(
                                self_,
                                arena,
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
                    original_error_info = self_.ref_(arena).maybe_error_info();
                    self_.ref_(arena).reset_error_info(save_error_info.clone());
                }
            }
        } else if target
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::Conditional)
        {
            if self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_deeply_nested_type(
                    target,
                    self_.ref_(arena).maybe_target_stack().as_ref().unwrap(),
                    self_.ref_(arena).target_depth(),
                    Some(10),
                )
            {
                self_.ref_(arena).reset_error_info(save_error_info);
                return Ok(Ternary::Maybe);
            }
            let c = target;
            if c.ref_(arena)
                .as_conditional_type()
                .root
                .ref_(arena)
                .infer_type_parameters
                .is_none()
                && !self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_distribution_dependent(
                        &c.ref_(arena).as_conditional_type().root.ref_(arena),
                    )?
            {
                let skip_true = !self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_type_assignable_to(
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_permissive_instantiation(
                                c.ref_(arena).as_conditional_type().check_type,
                            )?,
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_permissive_instantiation(
                                c.ref_(arena).as_conditional_type().extends_type,
                            )?,
                    )?;
                let skip_false = !skip_true
                    && self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .is_type_assignable_to(
                            self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .get_restrictive_instantiation(released!(
                                    c.ref_(arena).as_conditional_type().check_type
                                ))?,
                            self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .get_restrictive_instantiation(
                                    c.ref_(arena).as_conditional_type().extends_type,
                                )?,
                        )?;
                result = if skip_true {
                    Ternary::True
                } else {
                    Self::is_related_to(
                        self_,
                        arena,
                        source,
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_true_type_from_conditional_type(c)?,
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
                        Self::is_related_to(
                            self_,
                            arena,
                            source,
                            self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .get_false_type_from_conditional_type(c)?,
                            Some(RecursionFlags::Target),
                            Some(false),
                            None,
                            None,
                        )?
                    };
                    if result != Ternary::False {
                        self_.ref_(arena).reset_error_info(save_error_info);
                        return Ok(result);
                    }
                }
            }
        } else if target
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::TemplateLiteral)
        {
            if source
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::TemplateLiteral)
            {
                if Rc::ptr_eq(
                    &self_.ref_(arena).relation,
                    &self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .comparable_relation,
                ) {
                    return Ok(
                        if self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .template_literal_types_definitely_unrelated(source, target)
                        {
                            Ternary::False
                        } else {
                            Ternary::True
                        },
                    );
                }
                self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .instantiate_type(
                        source,
                        Some(
                            self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .make_function_type_mapper(ReportUnreliableMarkers),
                        ),
                    )?;
            }
            if self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_type_matched_by_template_literal_type(source, target)?
            {
                return Ok(Ternary::True);
            }
        }

        if source
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::TypeVariable)
        {
            if !(source
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::IndexedAccess)
                && target
                    .ref_(arena)
                    .flags()
                    .intersects(TypeFlags::IndexedAccess))
            {
                let constraint = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_constraint_of_type(source)?;
                if match constraint {
                    None => true,
                    Some(constraint) => {
                        source
                            .ref_(arena)
                            .flags()
                            .intersects(TypeFlags::TypeParameter)
                            && constraint.ref_(arena).flags().intersects(TypeFlags::Any)
                    }
                } {
                    result = Self::is_related_to(
                        self_,
                        arena,
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .empty_object_type(),
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .extract_types_of_kind(target, !TypeFlags::NonPrimitive),
                        Some(RecursionFlags::Both),
                        None,
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        self_.ref_(arena).reset_error_info(save_error_info);
                        return Ok(result);
                    }
                } else if {
                    result = Self::is_related_to(
                        self_,
                        arena,
                        constraint.unwrap(),
                        target,
                        Some(RecursionFlags::Source),
                        Some(false),
                        None,
                        Some(intersection_state),
                    )?;
                    result != Ternary::False
                } {
                    self_.ref_(arena).reset_error_info(save_error_info);
                    return Ok(result);
                } else if {
                    result = Self::is_related_to(
                        self_,
                        arena,
                        released!(self_.ref_(arena).type_checker)
                            .ref_(arena)
                            .get_type_with_this_argument(constraint.unwrap(), Some(source), None)?,
                        target,
                        Some(RecursionFlags::Source),
                        Some(
                            report_errors
                                && !(target.ref_(arena).flags() & source.ref_(arena).flags())
                                    .intersects(TypeFlags::TypeParameter),
                        ),
                        None,
                        Some(intersection_state),
                    )?;
                    result != Ternary::False
                } {
                    self_.ref_(arena).reset_error_info(save_error_info);
                    return Ok(result);
                }
            }
        } else if source.ref_(arena).flags().intersects(TypeFlags::Index) {
            result = Self::is_related_to(
                self_,
                arena,
                self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .keyof_constraint_type(),
                target,
                Some(RecursionFlags::Source),
                Some(report_errors),
                None,
                None,
            )?;
            if result != Ternary::False {
                self_.ref_(arena).reset_error_info(save_error_info);
                return Ok(result);
            }
        } else if source
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::TemplateLiteral)
            && !target.ref_(arena).flags().intersects(TypeFlags::Object)
        {
            if !target
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::TemplateLiteral)
            {
                let constraint = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_base_constraint_of_type(source)?;
                if let Some(constraint) = constraint.filter(|&constraint| constraint != source) {
                    result = Self::is_related_to(
                        self_,
                        arena,
                        constraint,
                        target,
                        Some(RecursionFlags::Source),
                        Some(report_errors),
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        self_.ref_(arena).reset_error_info(save_error_info);
                        return Ok(result);
                    }
                }
            }
        } else if source
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::StringMapping)
        {
            if target
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::StringMapping)
                && source.ref_(arena).as_string_mapping_type().symbol()
                    == target.ref_(arena).as_string_mapping_type().symbol()
            {
                result = Self::is_related_to(
                    self_,
                    arena,
                    source.ref_(arena).as_string_mapping_type().type_,
                    target.ref_(arena).as_string_mapping_type().type_,
                    Some(RecursionFlags::Both),
                    Some(report_errors),
                    None,
                    None,
                )?;
                if result != Ternary::False {
                    self_.ref_(arena).reset_error_info(save_error_info);
                    return Ok(result);
                }
            } else {
                let constraint = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_base_constraint_of_type(source)?;
                if let Some(constraint) = constraint {
                    result = Self::is_related_to(
                        self_,
                        arena,
                        constraint,
                        target,
                        Some(RecursionFlags::Source),
                        Some(report_errors),
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        self_.ref_(arena).reset_error_info(save_error_info);
                        return Ok(result);
                    }
                }
            }
        } else if source
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::Conditional)
        {
            if self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_deeply_nested_type(
                    source,
                    self_.ref_(arena).maybe_source_stack().as_ref().unwrap(),
                    self_.ref_(arena).source_depth(),
                    Some(10),
                )
            {
                self_.ref_(arena).reset_error_info(save_error_info);
                return Ok(Ternary::Maybe);
            }
            if target
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::Conditional)
            {
                let source_params = source
                    .ref_(arena)
                    .as_conditional_type()
                    .root
                    .ref_(arena)
                    .infer_type_parameters
                    .clone();
                let mut source_extends = source
                    .ref_(arena)
                    .as_conditional_type()
                    .extends_type
                    .clone();
                let mut mapper: Option<Id<TypeMapper>> = None;
                if let Some(source_params) = source_params.as_ref() {
                    let ctx = self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .create_inference_context(
                            source_params,
                            None,
                            InferenceFlags::None,
                            Some(self_.ref_(arena).alloc_type_comparer(Box::new(
                                TypeComparerIsRelatedToWorker::new(self_, arena),
                            ))),
                        );
                    self_.ref_(arena).type_checker.ref_(arena).infer_types(
                        &ctx.ref_(arena).inferences(),
                        target.ref_(arena).as_conditional_type().extends_type,
                        source_extends,
                        Some(InferencePriority::NoConstraints | InferencePriority::AlwaysStrict),
                        None,
                    )?;
                    source_extends = self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .instantiate_type(source_extends, Some(ctx.ref_(arena).mapper()))?;
                    mapper = Some(ctx.ref_(arena).mapper());
                }
                if self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_type_identical_to(
                        source_extends,
                        target.ref_(arena).as_conditional_type().extends_type,
                    )?
                    && (Self::is_related_to(
                        self_,
                        arena,
                        source.ref_(arena).as_conditional_type().check_type,
                        target.ref_(arena).as_conditional_type().check_type,
                        Some(RecursionFlags::Both),
                        None,
                        None,
                        None,
                    )? != Ternary::False
                        || Self::is_related_to(
                            self_,
                            arena,
                            target.ref_(arena).as_conditional_type().check_type,
                            source.ref_(arena).as_conditional_type().check_type,
                            Some(RecursionFlags::Both),
                            None,
                            None,
                            None,
                        )? != Ternary::False)
                {
                    result = Self::is_related_to(
                        self_,
                        arena,
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .instantiate_type(
                                self_
                                    .ref_(arena)
                                    .type_checker
                                    .ref_(arena)
                                    .get_true_type_from_conditional_type(source)?,
                                mapper,
                            )?,
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_true_type_from_conditional_type(target)?,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        result &= Self::is_related_to(
                            self_,
                            arena,
                            self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .get_false_type_from_conditional_type(source)?,
                            self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .get_false_type_from_conditional_type(target)?,
                            Some(RecursionFlags::Both),
                            Some(report_errors),
                            None,
                            None,
                        )?;
                    }
                    if result != Ternary::False {
                        self_.ref_(arena).reset_error_info(save_error_info);
                        return Ok(result);
                    }
                }
            } else {
                let distributive_constraint = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_constraint_of_distributive_conditional_type(source)?;
                if let Some(distributive_constraint) = distributive_constraint {
                    result = Self::is_related_to(
                        self_,
                        arena,
                        distributive_constraint,
                        target,
                        Some(RecursionFlags::Source),
                        Some(report_errors),
                        None,
                        None,
                    )?;
                    if result != Ternary::False {
                        self_.ref_(arena).reset_error_info(save_error_info);
                        return Ok(result);
                    }
                }
            }

            let default_constraint = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_default_constraint_of_conditional_type(source)?;
            // if (defaultConstraint) {
            result = Self::is_related_to(
                self_,
                arena,
                default_constraint,
                target,
                Some(RecursionFlags::Source),
                Some(report_errors),
                None,
                None,
            )?;
            if result != Ternary::False {
                self_.ref_(arena).reset_error_info(save_error_info);
                return Ok(result);
            }
            // }
        } else {
            if !Rc::ptr_eq(
                &self_.ref_(arena).relation,
                &self_.ref_(arena).type_checker.ref_(arena).subtype_relation,
            ) && !Rc::ptr_eq(
                &self_.ref_(arena).relation,
                &self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .strict_subtype_relation,
            ) && self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_partial_mapped_type(target)
                && self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_empty_object_type(source)?
            {
                return Ok(Ternary::True);
            }
            if self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_generic_mapped_type(target)?
            {
                if self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_generic_mapped_type(source)?
                {
                    result =
                        Self::mapped_type_related_to(self_, arena, source, target, report_errors)?;
                    if result != Ternary::False {
                        self_.ref_(arena).reset_error_info(save_error_info);
                        return Ok(result);
                    }
                }
                return Ok(Ternary::False);
            }
            let source_is_primitive = source.ref_(arena).flags().intersects(TypeFlags::Primitive);
            if !Rc::ptr_eq(
                &self_.ref_(arena).relation,
                &self_.ref_(arena).type_checker.ref_(arena).identity_relation,
            ) {
                source = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_apparent_type(source)?;
            } else if self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_generic_mapped_type(source)?
            {
                return Ok(Ternary::False);
            }
            if get_object_flags(&source.ref_(arena)).intersects(ObjectFlags::Reference)
                && get_object_flags(&target.ref_(arena)).intersects(ObjectFlags::Reference)
                && source.ref_(arena).as_type_reference_interface().target()
                    == target.ref_(arena).as_type_reference_interface().target()
                && !self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_tuple_type(source)
                && !(get_object_flags(&source.ref_(arena)).intersects(ObjectFlags::MarkerType)
                    || get_object_flags(&target.ref_(arena)).intersects(ObjectFlags::MarkerType))
            {
                let variances = released!(self_.ref_(arena).type_checker)
                    .ref_(arena)
                    .get_variances({
                        let target = source.ref_(arena).as_type_reference_interface().target();
                        target
                    });
                if variances.is_empty() {
                    return Ok(Ternary::Unknown);
                }
                let variance_result = Self::relate_variances(
                    self_,
                    arena,
                    &mut result,
                    report_errors,
                    &mut original_error_info,
                    &save_error_info,
                    &mut variance_check_failed,
                    Some(
                        &*released!(self_.ref_(arena).type_checker)
                            .ref_(arena)
                            .get_type_arguments(source)?,
                    ),
                    Some(
                        &*released!(self_.ref_(arena).type_checker)
                            .ref_(arena)
                            .get_type_arguments(target)?,
                    ),
                    &variances,
                    intersection_state,
                )?;
                if let Some(variance_result) = variance_result {
                    return Ok(variance_result);
                }
            } else if if self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_readonly_array_type(target)
            {
                self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_array_type(source)
                    || self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .is_tuple_type(source)
            } else {
                self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_array_type(target)
                    && self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .is_tuple_type(source)
                    && !source
                        .ref_(arena)
                        .as_type_reference_interface()
                        .target()
                        .ref_(arena)
                        .as_tuple_type()
                        .readonly
            } {
                if !Rc::ptr_eq(
                    &self_.ref_(arena).relation,
                    &self_.ref_(arena).type_checker.ref_(arena).identity_relation,
                ) {
                    return Self::is_related_to(
                        self_,
                        arena,
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_index_type_of_type_(
                                source,
                                self_.ref_(arena).type_checker.ref_(arena).number_type(),
                            )?
                            .unwrap_or_else(|| {
                                self_.ref_(arena).type_checker.ref_(arena).any_type()
                            }),
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_index_type_of_type_(
                                target,
                                self_.ref_(arena).type_checker.ref_(arena).number_type(),
                            )?
                            .unwrap_or_else(|| {
                                self_.ref_(arena).type_checker.ref_(arena).any_type()
                            }),
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None,
                        None,
                    );
                } else {
                    return Ok(Ternary::False);
                }
            } else if (Rc::ptr_eq(
                &self_.ref_(arena).relation,
                &self_.ref_(arena).type_checker.ref_(arena).subtype_relation,
            ) || Rc::ptr_eq(
                &self_.ref_(arena).relation,
                &self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .strict_subtype_relation,
            )) && self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_empty_object_type(target)?
                && get_object_flags(&target.ref_(arena)).intersects(ObjectFlags::FreshLiteral)
                && !self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_empty_object_type(source)?
            {
                return Ok(Ternary::False);
            }
            if source
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::Intersection)
                && target.ref_(arena).flags().intersects(TypeFlags::Object)
            {
                let report_structural_errors = report_errors
                    && are_option_rcs_equal(
                        self_.ref_(arena).maybe_error_info().as_ref(),
                        save_error_info.error_info.as_ref(),
                    )
                    && !source_is_primitive;
                result = Self::properties_related_to(
                    self_,
                    arena,
                    source,
                    target,
                    report_structural_errors,
                    None,
                    intersection_state,
                )?;
                if result != Ternary::False {
                    result &= Self::signatures_related_to(
                        self_,
                        arena,
                        source,
                        target,
                        SignatureKind::Call,
                        report_structural_errors,
                    )?;
                    if result != Ternary::False {
                        result &= Self::signatures_related_to(
                            self_,
                            arena,
                            source,
                            target,
                            SignatureKind::Construct,
                            report_structural_errors,
                        )?;
                        if result != Ternary::False {
                            result &= Self::index_signatures_related_to(
                                self_,
                                arena,
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
                        .or_else(|| self_.ref_(arena).maybe_error_info())
                        .or_else(|| save_error_info.error_info.clone());
                    *self_.ref_(arena).maybe_error_info_mut() = error_info;
                } else if result != Ternary::False {
                    return Ok(result);
                }
            }
            if source
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::Intersection)
                && target.ref_(arena).flags().intersects(TypeFlags::Union)
            {
                let object_only_target = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .extract_types_of_kind(
                        target,
                        TypeFlags::Object | TypeFlags::Intersection | TypeFlags::Substitution,
                    );
                if object_only_target
                    .ref_(arena)
                    .flags()
                    .intersects(TypeFlags::Union)
                {
                    let result = Self::type_related_to_discriminated_type(
                        self_,
                        arena,
                        source,
                        object_only_target,
                    )?;
                    if result != Ternary::False {
                        return Ok(result);
                    }
                }
            }
        }
        Ok(Ternary::False)
    }
}

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

pub(super) struct TypeComparerIsRelatedToWorker {
    arena: *const AllArenas,
    check_type_related_to: Id<CheckTypeRelatedTo>,
}

impl TypeComparerIsRelatedToWorker {
    pub fn new(check_type_related_to: Id<CheckTypeRelatedTo>, arena: &impl HasArena) -> Self {
        Self {
            arena: arena.arena(),
            check_type_related_to,
        }
    }
}

impl TypeComparer for TypeComparerIsRelatedToWorker {
    fn get_call(&self) -> Box<dyn TypeComparerCall> {
        Box::new(TypeComparerIsRelatedToWorkerCall::new(self))
    }
}

pub(super) struct TypeComparerIsRelatedToWorkerCall {
    arena: *const AllArenas,
    check_type_related_to: Id<CheckTypeRelatedTo>,
}

impl TypeComparerIsRelatedToWorkerCall {
    pub fn new(type_comparer_is_related_to_worker: &TypeComparerIsRelatedToWorker) -> Self {
        Self {
            arena: type_comparer_is_related_to_worker.arena,
            check_type_related_to: type_comparer_is_related_to_worker.check_type_related_to,
        }
    }
}

impl TypeComparerCall for TypeComparerIsRelatedToWorkerCall {
    fn call(&self, s: Id<Type>, t: Id<Type>, report_errors: Option<bool>) -> io::Result<Ternary> {
        CheckTypeRelatedTo::is_related_to_worker(
            self.check_type_related_to,
            self,
            s,
            t,
            report_errors.unwrap_or(false), // the default isn't in the Typescript version but that appears to be a type-checking bug there
        )
    }
}

impl_has_arena!(TypeComparerIsRelatedToWorkerCall);

struct RecursiveTypeRelatedToOutofbandVarianceMarkerHandler {
    arena: *const AllArenas,
    propagating_variance_flags: Rc<Cell<RelationComparisonResult>>,
    original_handler: Id<Box<dyn OutofbandVarianceMarkerHandler>>,
}

impl RecursiveTypeRelatedToOutofbandVarianceMarkerHandler {
    pub fn new(
        propagating_variance_flags: Rc<Cell<RelationComparisonResult>>,
        original_handler: Id<Box<dyn OutofbandVarianceMarkerHandler>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
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

impl_has_arena!(RecursiveTypeRelatedToOutofbandVarianceMarkerHandler);
