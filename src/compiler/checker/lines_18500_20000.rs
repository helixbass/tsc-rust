#![allow(non_upper_case_globals)]

use std::collections::HashSet;
use std::rc::Rc;

use super::{
    CheckTypeContainingMessageChain, CheckTypeRelatedTo, ExpandingFlags, IntersectionState,
    RecursionFlags,
};
use crate::{
    Diagnostics, Node, NodeInterface, Symbol, SymbolInterface, Ternary, Type, TypeChecker,
    TypeFlags, TypeInterface, UnionOrIntersectionType, UnionOrIntersectionTypeInterface, __String,
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

    pub(super) fn recursive_type_related_to(
        &self,
        source: &Type,
        target: &Type,
        report_errors: bool,
        intersection_state: IntersectionState,
        recursion_flags: RecursionFlags,
    ) -> Ternary {
        let result = if self.expanding_flags != ExpandingFlags::Both {
            self.structured_type_related_to(source, target, report_errors, intersection_state)
        } else {
            Ternary::Maybe
        };
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
