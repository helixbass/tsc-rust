#![allow(non_upper_case_globals)]

use std::borrow::Cow;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::ptr;
use std::rc::Rc;

use super::{
    CheckTypeContainingMessageChain, CheckTypeContainingMessageChainDummy,
    CheckTypeErrorOutputContainer, CheckTypeRelatedTo, ExpandingFlags, IntersectionState, JsxNames,
    RecursionFlags,
};
use crate::{
    append, are_option_rcs_equal, contains_rc, find_ancestor, is_identifier, is_identifier_text,
    reduce_left, some, Diagnostic, ObjectFlagsTypeInterface, UnionOrIntersectionType,
    UnionOrIntersectionTypeInterface, __String, add_related_info, chain_diagnostic_messages,
    concatenate_diagnostic_message_chains, create_diagnostic_for_node,
    create_diagnostic_for_node_from_message_chain, first_or_undefined, get_emit_script_target,
    get_object_flags, get_source_file_of_node, is_import_call, is_jsx_attribute, is_jsx_attributes,
    is_jsx_opening_like_element, is_object_literal_element_like, Debug_, DiagnosticMessage,
    DiagnosticMessageChain, DiagnosticRelatedInformation, Diagnostics, Node, NodeInterface,
    ObjectFlags, RelationComparisonResult, SignatureKind, Symbol, SymbolInterface, Ternary, Type,
    TypeChecker, TypeFlags, TypeInterface,
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
                    &container_value_declaration,
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
        unimplemented!()
    }

    pub(super) fn type_related_to_some_type(
        &self,
        source: &Type,
        target: &UnionOrIntersectionType,
        report_errors: bool,
    ) -> Ternary {
        let target_types = target.types();
        if target.flags().intersects(TypeFlags::Union) {
            if self.type_checker.contains_type(target_types, source) {
                return Ternary::True;
            }
        }

        for type_ in target_types {
            let related = self.is_related_to(
                source,
                &type_,
                Some(RecursionFlags::Target),
                Some(false),
                None,
                None,
            );
            if related != Ternary::False {
                return related;
            }
        }

        Ternary::False
    }

    pub(super) fn each_type_related_to_type(
        &self,
        source: &UnionOrIntersectionType,
        target: &Type,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let mut result = Ternary::True;
        let source_types = source.types();
        for source_type in source_types {
            let related = self.is_related_to(
                &source_type,
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
                        source.as_union_or_intersection_type(),
                        target,
                        report_errors,
                        intersection_state & !IntersectionState::UnionIntersectionCheck,
                    )
                };
            }
            if target.flags().intersects(TypeFlags::Union) {
                return self.type_related_to_some_type(
                    &self.type_checker.get_regular_type_of_object_literal(source),
                    target.as_union_or_intersection_type(),
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
