#![allow(non_upper_case_globals)]

use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use super::{ExpandingFlags, IntersectionState, RecursionFlags};
use crate::{
    UnionOrIntersectionType, UnionOrIntersectionTypeInterface, __String, chain_diagnostic_messages,
    create_diagnostic_for_node_from_message_chain, first_or_undefined, get_object_flags, Debug_,
    DiagnosticMessage, DiagnosticMessageChain, Diagnostics, Node, NodeInterface, ObjectFlags,
    RelationComparisonResult, Symbol, Ternary, Type, TypeChecker, TypeFlags, TypeInterface,
};

pub(super) struct CheckTypeRelatedTo<'type_checker> {
    type_checker: &'type_checker TypeChecker,
    source: Rc<Type>,
    target: Rc<Type>,
    relation: &'type_checker HashMap<String, RelationComparisonResult>,
    error_node: Option<&'type_checker Node>,
    head_message: Option<DiagnosticMessage>,
    error_info: RefCell<Option<DiagnosticMessageChain>>,
    expanding_flags: ExpandingFlags,
    incompatible_stack: RefCell<Vec<(DiagnosticMessage, Option<Vec<String>>)>>,
}

impl<'type_checker> CheckTypeRelatedTo<'type_checker> {
    pub(super) fn new(
        type_checker: &'type_checker TypeChecker,
        source: Rc<Type>,
        target: Rc<Type>,
        relation: &'type_checker HashMap<String, RelationComparisonResult>,
        error_node: Option<&'type_checker Node>,
        head_message: Option<DiagnosticMessage>,
    ) -> Self {
        Self {
            type_checker,
            source,
            target,
            relation,
            error_node,
            head_message,
            error_info: RefCell::new(None),
            expanding_flags: ExpandingFlags::None,
            incompatible_stack: RefCell::new(vec![]),
        }
    }

    fn error_info(&self) -> Ref<Option<DiagnosticMessageChain>> {
        self.error_info.borrow()
    }

    fn set_error_info(&self, error_info: DiagnosticMessageChain) {
        *self.error_info.borrow_mut() = Some(error_info);
    }

    fn incompatible_stack(&self) -> RefMut<Vec<(DiagnosticMessage, Option<Vec<String>>)>> {
        self.incompatible_stack.borrow_mut()
    }

    pub(super) fn call(&self) -> bool {
        let result = self.is_related_to(
            self.source.clone(),
            self.target.clone(),
            Some(RecursionFlags::Both),
            self.error_node.is_some(),
            self.head_message.as_ref(),
            None,
        );

        if !self.incompatible_stack().is_empty() {
            self.report_incompatible_stack();
        } else if false {
            unimplemented!()
        } else if self.error_info().is_some() {
            let diag = create_diagnostic_for_node_from_message_chain(
                &*self.error_node.unwrap(),
                self.error_info().clone().unwrap(),
            );
            if true {
                self.type_checker.diagnostics().add(Rc::new(diag.into()));
            }
        }

        result != Ternary::False
    }

    fn report_incompatible_error(&self, message: DiagnosticMessage, args: Option<Vec<String>>) {
        self.incompatible_stack().push((message, args));
    }

    fn report_incompatible_stack(&self) {
        unimplemented!()
    }

    fn report_error(&self, message: &DiagnosticMessage, args: Option<Vec<String>>) {
        Debug_.assert(self.error_node.is_some(), None);
        let error_info = { chain_diagnostic_messages(self.error_info().clone(), message, args) };
        self.set_error_info(error_info);
    }

    fn report_relation_error(
        &self,
        mut message: Option<&DiagnosticMessage>,
        source: Rc<Type>,
        target: Rc<Type>,
    ) {
        let (source_type, target_type) = self
            .type_checker
            .get_type_names_for_error_display(source.clone(), target.clone());
        let mut generalized_source = source.clone();
        let mut generalized_source_type = source_type;

        if self.type_checker.is_literal_type(&*source)
            && !self
                .type_checker
                .type_could_have_top_level_singleton_types(&*target)
        {
            generalized_source = self
                .type_checker
                .get_base_type_of_literal_type(source.clone());
            Debug_.assert(
                !self
                    .type_checker
                    .is_type_assignable_to(generalized_source.clone(), target),
                Some("generalized source shouldn't be assignable"),
            );
            generalized_source_type = self
                .type_checker
                .get_type_name_for_error_display(generalized_source);
        }

        if message.is_none() {
            if false {
            } else {
                message = Some(&Diagnostics::Type_0_is_not_assignable_to_type_1);
            }
        }

        self.report_error(
            message.unwrap(),
            Some(vec![generalized_source_type, target_type]),
        );
    }

    fn is_related_to(
        &self,
        original_source: Rc<Type>,
        original_target: Rc<Type>,
        recursion_flags: Option<RecursionFlags>,
        report_errors: bool,
        head_message: Option<&DiagnosticMessage>,
        intersection_state: Option<IntersectionState>,
    ) -> Ternary {
        let intersection_state = intersection_state.unwrap_or(IntersectionState::None);
        let recursion_flags = recursion_flags.unwrap_or(RecursionFlags::Both);

        let source = self.type_checker.get_normalized_type(original_source);
        let target = self
            .type_checker
            .get_normalized_type(original_target.clone());

        let report_error_results = |source, target, result| {
            if result == Ternary::False && report_errors {
                let source = source;
                let target = target;
                self.report_relation_error(head_message, source, target);
            }
        };

        let report_error = |message: DiagnosticMessage, args: Option<Vec<String>>| {
            self.report_error(&message, args);
        };

        if false
            || self.type_checker.is_simple_type_related_to(
                &*source,
                &*target,
                self.relation,
                if report_errors {
                    Some(&report_error)
                } else {
                    None
                },
            )
        {
            return Ternary::True;
        }

        let is_performing_excess_property_checks = !intersection_state
            .intersects(IntersectionState::Target)
            && (self.type_checker.is_object_literal_type(source.clone())
                && get_object_flags(&*source).intersects(ObjectFlags::FreshLiteral));
        if is_performing_excess_property_checks {
            if self.has_excess_properties(source.clone(), target.clone(), report_errors) {
                if report_errors {
                    self.report_relation_error(
                        head_message,
                        source,
                        if false { original_target } else { target },
                    );
                }
                return Ternary::False;
            }
        }

        let mut result = Ternary::False;

        if (source.flags().intersects(TypeFlags::Union)
            || target.flags().intersects(TypeFlags::Union))
            && self.type_checker.get_constituent_count(source.clone())
                * self.type_checker.get_constituent_count(target.clone())
                < 4
        {
            result = self.structured_type_related_to(
                source.clone(),
                target.clone(),
                report_errors,
                intersection_state | IntersectionState::UnionIntersectionCheck,
            );
        }
        if result == Ternary::False
            && !(source.flags().intersects(TypeFlags::Union))
            && (source
                .flags()
                .intersects(TypeFlags::StructuredOrInstantiable)
                || source
                    .flags()
                    .intersects(TypeFlags::StructuredOrInstantiable))
        {
            result = self.recursive_type_related_to(
                source.clone(),
                target.clone(),
                report_errors,
                intersection_state,
                recursion_flags,
            );
        }

        report_error_results(source, target, result);

        result
    }

    fn has_excess_properties(
        &self,
        source: Rc<Type /*FreshObjectLiteralType*/>,
        target: Rc<Type>,
        report_errors: bool,
    ) -> bool {
        if !self
            .type_checker
            .is_excess_property_check_target(target.clone())
            || false
        {
            return false;
        }
        let is_comparing_jsx_attributes = false;
        let reduced_target = target.clone();
        for prop in self.type_checker.get_properties_of_type(source.clone()) {
            if self.should_check_as_excess_property(prop.clone(), source.symbol()) && true {
                if !self.type_checker.is_known_property(
                    reduced_target.clone(),
                    &prop.escaped_name,
                    is_comparing_jsx_attributes,
                ) {
                    if report_errors {
                        let error_target = self.type_checker.filter_type(
                            reduced_target,
                            TypeChecker::is_excess_property_check_target,
                        );
                        let error_node = match self.error_node {
                            None => Debug_.fail(None),
                            Some(error_node) => error_node,
                        };
                        if false {
                            unimplemented!()
                        } else {
                            let object_literal_declaration =
                                if let Some(symbol) = source.maybe_symbol() {
                                    if let Some(declarations) = &*symbol.maybe_declarations() {
                                        first_or_undefined(declarations).map(Clone::clone)
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                };
                            if false {
                                unimplemented!()
                            } else {
                                self.report_error(
                                    &Diagnostics::Object_literal_may_only_specify_known_properties_and_0_does_not_exist_in_type_1,
                                    Some(vec![self.type_checker.symbol_to_string(prop, None, None, None, None), self.type_checker.type_to_string(error_target)])
                                );
                            }
                        }
                    }
                    return true;
                }
            }
        }
        false
    }

    fn should_check_as_excess_property(&self, prop: Rc<Symbol>, container: Rc<Symbol>) -> bool {
        if let Some(prop_value_declaration) = prop.maybe_value_declaration().as_ref() {
            if let Some(container_value_declaration) = container.maybe_value_declaration().as_ref()
            {
                return Rc::ptr_eq(
                    &prop_value_declaration.upgrade().unwrap().parent(),
                    &container_value_declaration.upgrade().unwrap(),
                );
            }
        }
        false
    }

    fn type_related_to_some_type(
        &self,
        source: Rc<Type>,
        target: &UnionOrIntersectionType,
    ) -> Ternary {
        let target_types = target.types();

        for type_ in target_types {
            let related = self.is_related_to(
                source.clone(),
                type_.clone(),
                Some(RecursionFlags::Target),
                false,
                None,
                None,
            );
            if related != Ternary::False {
                return related;
            }
        }

        Ternary::False
    }

    fn each_type_related_to_type(
        &self,
        source: &UnionOrIntersectionType,
        target: Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let mut result = Ternary::True;
        let source_types = source.types();
        for source_type in source_types {
            let related = self.is_related_to(
                source_type.clone(),
                target.clone(),
                Some(RecursionFlags::Source),
                report_errors,
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

    fn recursive_type_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
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

    fn structured_type_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
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

    fn structured_type_related_to_worker(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if intersection_state.intersects(IntersectionState::UnionIntersectionCheck) {
            if source.flags().intersects(TypeFlags::Union) {
                return if false {
                    unimplemented!()
                } else {
                    self.each_type_related_to_type(
                        match &*source {
                            Type::UnionOrIntersectionType(union_or_intersection_type) => {
                                union_or_intersection_type
                            }
                            _ => panic!("Expected UnionOrIntersectionType"),
                        },
                        target,
                        report_errors,
                        intersection_state & !IntersectionState::UnionIntersectionCheck,
                    )
                };
            }
            if target.flags().intersects(TypeFlags::Union) {
                return self.type_related_to_some_type(
                    self.type_checker.get_regular_type_of_object_literal(source),
                    match &*target {
                        Type::UnionOrIntersectionType(union_or_intersection_type) => {
                            union_or_intersection_type
                        }
                        _ => panic!("Expected UnionOrIntersectionType"),
                    },
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

    fn exclude_properties(
        &self,
        properties: Vec<Rc<Symbol>>,
        excluded_properties: Option<HashSet<__String>>,
    ) -> Vec<Rc<Symbol>> {
        properties
    }

    fn is_property_symbol_type_related(
        &self,
        source_prop: Rc<Symbol>,
        target_prop: Rc<Symbol>,
        get_type_of_source_property: fn(&TypeChecker, Rc<Symbol>) -> Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let target_is_optional = false;
        let effective_target = self.type_checker.add_optionality(
            self.type_checker
                .get_non_missing_type_of_symbol(target_prop),
            Some(false),
            Some(target_is_optional),
        );
        let effective_source = get_type_of_source_property(self.type_checker, source_prop);
        self.is_related_to(
            effective_source,
            effective_target,
            Some(RecursionFlags::Both),
            report_errors,
            None,
            Some(intersection_state),
        )
    }

    fn property_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        source_prop: Rc<Symbol>,
        target_prop: Rc<Symbol>,
        get_type_of_source_property: fn(&TypeChecker, Rc<Symbol>) -> Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
        skip_optional: bool,
    ) -> Ternary {
        let related = self.is_property_symbol_type_related(
            source_prop,
            target_prop.clone(),
            get_type_of_source_property,
            report_errors,
            intersection_state,
        );
        if related == Ternary::False {
            if report_errors {
                self.report_incompatible_error(
                    Diagnostics::Types_of_property_0_are_incompatible,
                    Some(vec![self.type_checker.symbol_to_string(
                        target_prop,
                        None,
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

    fn properties_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
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
        let properties = self.type_checker.get_properties_of_type(target.clone());
        for target_prop in self.exclude_properties(properties, excluded_properties) {
            let name = &target_prop.escaped_name;
            if true {
                let source_prop = self.type_checker.get_property_of_type(source.clone(), name);
                if let Some(source_prop) = source_prop {
                    if !Rc::ptr_eq(&source_prop, &target_prop) {
                        let related = self.property_related_to(
                            source.clone(),
                            target.clone(),
                            source_prop,
                            target_prop,
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
