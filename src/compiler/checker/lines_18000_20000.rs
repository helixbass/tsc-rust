#![allow(non_upper_case_globals)]

use std::borrow::Cow;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::ptr;
use std::rc::Rc;

use super::{
    CheckTypeContainingMessageChain, CheckTypeContainingMessageChainDummy,
    CheckTypeErrorOutputContainer, ExpandingFlags, IntersectionState, RecursionFlags,
};
use crate::compiler::scanner::is_identifier_text;
use crate::{
    append, Diagnostic, UnionOrIntersectionType, UnionOrIntersectionTypeInterface, __String,
    add_related_info, chain_diagnostic_messages, concatenate_diagnostic_message_chains,
    create_diagnostic_for_node, create_diagnostic_for_node_from_message_chain, first_or_undefined,
    get_emit_script_target, get_object_flags, is_import_call, Debug_, DiagnosticMessage,
    DiagnosticMessageChain, DiagnosticRelatedInformation, Diagnostics, Node, NodeInterface,
    ObjectFlags, RelationComparisonResult, Symbol, SymbolInterface, Ternary, Type, TypeChecker,
    TypeFlags, TypeInterface,
};

pub(super) struct CheckTypeRelatedTo<
    'type_checker,
    TContainingMessageChain: CheckTypeContainingMessageChain,
> {
    type_checker: &'type_checker TypeChecker,
    source: &'type_checker Type,
    target: &'type_checker Type,
    relation: &'type_checker HashMap<String, RelationComparisonResult>,
    error_node: Option<Rc<Node>>,
    head_message: Option<Cow<'static, DiagnosticMessage>>,
    containing_message_chain: Option<TContainingMessageChain>,
    error_output_container: Option<&'type_checker dyn CheckTypeErrorOutputContainer>,
    error_info: RefCell<Option<DiagnosticMessageChain>>,
    related_info: RefCell<Option<Vec<DiagnosticRelatedInformation>>>,
    maybe_keys: RefCell<Option<Vec<String>>>,
    source_stack: RefCell<Option<Vec<Rc<Type>>>>,
    target_stack: RefCell<Option<Vec<Rc<Type>>>>,
    maybe_count: Cell<usize>,
    source_depth: Cell<usize>,
    target_depth: Cell<usize>,
    expanding_flags: ExpandingFlags,
    overflow: Cell<bool>,
    override_next_error_info: Cell<usize>,
    last_skipped_info: RefCell<Option<(Rc<Type>, Rc<Type>)>>,
    incompatible_stack: RefCell<Vec<(&'static DiagnosticMessage, Option<Vec<String>>)>>,
    in_property_check: Cell<bool>,
}

impl<'type_checker, TContainingMessageChain: CheckTypeContainingMessageChain>
    CheckTypeRelatedTo<'type_checker, TContainingMessageChain>
{
    pub(super) fn new(
        type_checker: &'type_checker TypeChecker,
        source: &'type_checker Type,
        target: &'type_checker Type,
        relation: &'type_checker HashMap<String, RelationComparisonResult>,
        error_node: Option<Rc<Node>>,
        head_message: Option<Cow<'static, DiagnosticMessage>>,
        containing_message_chain: Option<TContainingMessageChain>,
        error_output_container: Option<&'type_checker dyn CheckTypeErrorOutputContainer>,
    ) -> Self {
        Self {
            type_checker,
            source,
            target,
            relation,
            error_node,
            head_message,
            containing_message_chain,
            error_output_container,
            error_info: RefCell::new(None),
            related_info: RefCell::new(None),
            maybe_keys: RefCell::new(None),
            source_stack: RefCell::new(None),
            target_stack: RefCell::new(None),
            maybe_count: Cell::new(0),
            source_depth: Cell::new(0),
            target_depth: Cell::new(0),
            expanding_flags: ExpandingFlags::None,
            overflow: Cell::new(false),
            override_next_error_info: Cell::new(0),
            last_skipped_info: RefCell::new(None),
            incompatible_stack: RefCell::new(vec![]),
            in_property_check: Cell::new(false),
        }
    }

    pub(super) fn maybe_error_info(&self) -> RefMut<Option<DiagnosticMessageChain>> {
        self.error_info.borrow_mut()
    }

    pub(super) fn maybe_related_info(&self) -> RefMut<Option<Vec<DiagnosticRelatedInformation>>> {
        self.related_info.borrow_mut()
    }

    pub(super) fn overflow(&self) -> bool {
        self.overflow.get()
    }

    pub(super) fn override_next_error_info(&self) -> usize {
        self.override_next_error_info.get()
    }

    pub(super) fn set_override_next_error_info(&self, override_next_error_info: usize) {
        self.override_next_error_info.set(override_next_error_info);
    }

    pub(super) fn maybe_last_skipped_info(&self) -> RefMut<Option<(Rc<Type>, Rc<Type>)>> {
        self.last_skipped_info.borrow_mut()
    }

    pub(super) fn incompatible_stack(
        &self,
    ) -> RefMut<Vec<(&'static DiagnosticMessage, Option<Vec<String>>)>> {
        self.incompatible_stack.borrow_mut()
    }

    pub(super) fn call(&self) -> bool {
        Debug_.assert(
            !ptr::eq(self.relation, &*self.type_checker.identity_relation())
                || self.error_node.is_none(),
            Some("no error reporting in identity checking"),
        );

        let result = self.is_related_to(
            self.source,
            self.target,
            Some(RecursionFlags::Both),
            self.error_node.is_some(),
            self.head_message.as_deref(),
            None,
        );
        if !self.incompatible_stack().is_empty() {
            self.report_incompatible_stack();
        }
        if self.overflow() {
            // tracing?.instant(tracing.Phase.CheckTypes, "checkTypeRelatedTo_DepthLimit", { sourceId: source.id, targetId: target.id, depth: sourceDepth, targetDepth });
            let diag = self.type_checker.error(
                self.error_node
                    .clone()
                    .or_else(|| self.type_checker.maybe_current_node()),
                &Diagnostics::Excessive_stack_depth_comparing_types_0_and_1,
                Some(vec![
                    self.type_checker.type_to_string_(
                        self.source,
                        Option::<&Node>::None,
                        None,
                        None,
                    ),
                    self.type_checker.type_to_string_(
                        self.target,
                        Option::<&Node>::None,
                        None,
                        None,
                    ),
                ]),
            );
            if let Some(error_output_container) = self.error_output_container {
                error_output_container.push_error(diag);
            }
        } else if self.maybe_error_info().is_some() {
            if let Some(containing_message_chain) = self.containing_message_chain.as_ref() {
                let chain = containing_message_chain.get();
                if let Some(chain) = chain {
                    concatenate_diagnostic_message_chains(
                        &mut chain.borrow_mut(),
                        self.maybe_error_info().clone().unwrap(),
                    );
                    // TODO: is this ever a problem? Not sure why I made containing_message_chain return an Rc<RefCell<DiagnosticMessageChain>> (vs just DiagnosticMessageChain)
                    // but seems like .clone()'ing here means that that original Rc'd DiagnosticMessageChain now no longer is "sharing this mutation"
                    *self.maybe_error_info() = Some((*chain).borrow().clone());
                }
            }

            let mut related_information: Option<Vec<Rc<DiagnosticRelatedInformation>>> = None;
            if let Some(head_message) = self.head_message.as_ref() {
                if let Some(error_node) = self.error_node.as_ref() {
                    if result == Ternary::False {
                        if let Some(source_symbol) = self.source.maybe_symbol() {
                            let links = self.type_checker.get_symbol_links(&source_symbol);
                            if let Some(links_originating_import) =
                                (*links).borrow().originating_import.clone().filter(
                                    |links_originating_import| {
                                        !is_import_call(links_originating_import)
                                    },
                                )
                            {
                                let helpful_retry = self.type_checker.check_type_related_to(
                                    &self.type_checker.get_type_of_symbol(
                                        (*links).borrow().target.as_ref().unwrap(),
                                    ),
                                    self.target,
                                    self.relation,
                                    Option::<&Node>::None,
                                    None,
                                    Option::<CheckTypeContainingMessageChainDummy>::None,
                                    None,
                                );
                                if helpful_retry {
                                    let diag: Rc<DiagnosticRelatedInformation> = Rc::new(create_diagnostic_for_node(
                                        &links_originating_import,
                                        &Diagnostics::Type_originates_at_this_import_A_namespace_style_import_cannot_be_called_or_constructed_and_will_cause_a_failure_at_runtime_Consider_using_a_default_import_or_import_require_here_instead,
                                        None,
                                    ).into());
                                    if related_information.is_none() {
                                        related_information = Some(vec![]);
                                        append(related_information.as_mut().unwrap(), Some(diag));
                                    }
                                }
                            };
                        }
                    }
                }
            }
            let diag: Rc<Diagnostic> = Rc::new(
                create_diagnostic_for_node_from_message_chain(
                    self.error_node.as_ref().unwrap(),
                    self.maybe_error_info().clone().unwrap(),
                    related_information,
                )
                .into(),
            );
            if let Some(related_info) = self.maybe_related_info().clone() {
                add_related_info(&diag, related_info.into_iter().map(Rc::new).collect());
            }
            if let Some(error_output_container) = self.error_output_container {
                error_output_container.push_error(diag.clone());
            }
            if match self.error_output_container {
                None => true,
                Some(error_output_container) => {
                    !matches!(error_output_container.skip_logging(), Some(true))
                }
            } {
                self.type_checker.diagnostics().add(diag);
            }
        }
        if self.error_node.is_some() {
            if let Some(error_output_container) =
                self.error_output_container
                    .filter(|error_output_container| {
                        matches!(error_output_container.skip_logging(), Some(true),)
                    })
            {
                if result == Ternary::False {
                    Debug_.assert(
                        error_output_container.errors_len() > 0,
                        Some("missed opportunity to interact with error."),
                    );
                }
            }
        }

        result != Ternary::False
    }

    pub(super) fn reset_error_info(&self, saved: ErrorCalculationState) {
        let ErrorCalculationState {
            error_info,
            last_skipped_info,
            incompatible_stack,
            override_next_error_info,
            related_info,
        } = saved;
        *self.maybe_error_info() = error_info;
        *self.maybe_last_skipped_info() = last_skipped_info;
        *self.incompatible_stack() = incompatible_stack;
        self.set_override_next_error_info(override_next_error_info);
        *self.maybe_related_info() = related_info;
    }

    pub(super) fn capture_error_calculation_state(&self) -> ErrorCalculationState {
        ErrorCalculationState {
            error_info: self.maybe_error_info().clone(),
            last_skipped_info: self.maybe_last_skipped_info().clone(),
            incompatible_stack: self.incompatible_stack().clone(),
            override_next_error_info: self.override_next_error_info(),
            related_info: self.maybe_related_info().clone(),
        }
    }

    pub(super) fn report_incompatible_error(
        &self,
        message: &'static DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        self.set_override_next_error_info(self.override_next_error_info() + 1);
        *self.maybe_last_skipped_info() = None;
        self.incompatible_stack().push((message, args));
    }

    pub(super) fn report_incompatible_stack(&self) {
        let mut stack = self.incompatible_stack().clone();
        *self.incompatible_stack() = vec![];
        let info = self.maybe_last_skipped_info().clone();
        *self.maybe_last_skipped_info() = None;
        if stack.len() == 1 {
            let (stack_0_error, stack_0_args) = stack.into_iter().next().unwrap();
            self.report_error(stack_0_error, stack_0_args);
            if let Some((info_0, info_1)) = info {
                self.report_relation_error(None, &info_0, &info_1);
            }
            return;
        }
        let mut path = "".to_owned();
        let mut secondary_root_errors: Vec<(&'static DiagnosticMessage, Option<Vec<String>>)> =
            vec![];
        while !stack.is_empty() {
            let (msg, args) = stack.pop().unwrap();
            if msg.code == Diagnostics::Types_of_property_0_are_incompatible.code {
                if path.starts_with("new ") {
                    path = format!("({})", path);
                }
                let args = args.unwrap();
                let str = &args[0];
                if path.is_empty() {
                    path = str.clone();
                } else if is_identifier_text(str, Some(get_emit_script_target(&self.type_checker.compiler_options)), None) {
                    path = format!("{}.{}", path, str);
                } else if {
                    let str_chars: Vec<char> = str.chars().collect();
                    !str_chars.is_empty() && str_chars[0] == '[' && str_chars[str_chars.len() - 1] == ']'
                } {
                    path = format!("{}{}", path, str);
                } else {
                    path = format!("{}[{}]", path, str);
                }
                break;
            } else if msg.code == Diagnostics::Call_signature_return_types_0_and_1_are_incompatible.code ||
                msg.code == Diagnostics::Construct_signature_return_types_0_and_1_are_incompatible.code ||
                msg.code == Diagnostics::Call_signatures_with_no_arguments_have_incompatible_return_types_0_and_1.code ||
                msg.code == Diagnostics::Construct_signatures_with_no_arguments_have_incompatible_return_types_0_and_1.code {
                if path.is_empty() {
                    let mut mapped_msg = msg;
                    if msg.code == Diagnostics::Call_signatures_with_no_arguments_have_incompatible_return_types_0_and_1.code {
                        mapped_msg = &Diagnostics::Call_signature_return_types_0_and_1_are_incompatible;
                    } else if msg.code == Diagnostics::Construct_signatures_with_no_arguments_have_incompatible_return_types_0_and_1.code {
                        mapped_msg = &Diagnostics::Construct_signature_return_types_0_and_1_are_incompatible;
                    }
                    secondary_root_errors.insert(0, (mapped_msg, args));
                } else {
                    let prefix = if msg.code == Diagnostics::Construct_signature_return_types_0_and_1_are_incompatible.code || 
                        msg.code == Diagnostics::Construct_signatures_with_no_arguments_have_incompatible_return_types_0_and_1.code {
                        "new "
                    } else {
                        ""
                    };
                    let params = if msg.code == Diagnostics::Call_signatures_with_no_arguments_have_incompatible_return_types_0_and_1.code || 
                        msg.code == Diagnostics::Construct_signatures_with_no_arguments_have_incompatible_return_types_0_and_1.code {
                        ""
                    } else {
                        "..."
                    };
                    path = format!("{}{}({})", prefix, path, params);
                }
                break;
            } else if msg.code == Diagnostics::Type_at_position_0_in_source_is_not_compatible_with_type_at_position_1_in_target.code {
                secondary_root_errors.insert(0, (&Diagnostics::Type_at_position_0_in_source_is_not_compatible_with_type_at_position_1_in_target, args));
            } else if msg.code == Diagnostics::Type_at_positions_0_through_1_in_source_is_not_compatible_with_type_at_position_2_in_target.code {
                secondary_root_errors.insert(0, (&Diagnostics::Type_at_positions_0_through_1_in_source_is_not_compatible_with_type_at_position_2_in_target, args));
            } else {
                Debug_.fail(Some(&format!("Unhandled Diagnostic: {}", msg.code)));
            }
        }
        if !path.is_empty() {
            self.report_error(
                if path.chars().last().unwrap() == ')' {
                    &Diagnostics::The_types_returned_by_0_are_incompatible_between_these_types
                } else {
                    &Diagnostics::The_types_of_0_are_incompatible_between_these_types
                },
                Some(vec![path])
            );
        } else {
            secondary_root_errors.remove(0);
        }
        for (msg, args) in secondary_root_errors {
            let original_value = msg.maybe_elided_in_compatability_pyramid();
            msg.set_elided_in_compatability_pyramid(Some(false));
            self.report_error(msg, args);
            msg.set_elided_in_compatability_pyramid(original_value);
        }
        if let Some((info_0, info_1)) = info {
            self.report_relation_error(None, &info_0, &info_1);
        }
    }

    pub(super) fn report_error(&self, message: &DiagnosticMessage, args: Option<Vec<String>>) {
        Debug_.assert(self.error_node.is_some(), None);
        let error_info =
            { chain_diagnostic_messages(self.maybe_error_info().clone(), message, args) };
        *self.maybe_error_info() = Some(error_info);
    }

    pub(super) fn report_relation_error(
        &self,
        mut message: Option<&DiagnosticMessage>,
        source: &Type,
        target: &Type,
    ) {
        let (source_type, target_type) = self
            .type_checker
            .get_type_names_for_error_display(source, target);
        let mut generalized_source = source.type_wrapper();
        let mut generalized_source_type = source_type;

        if self.type_checker.is_literal_type(source)
            && !self
                .type_checker
                .type_could_have_top_level_singleton_types(target)
        {
            generalized_source = self.type_checker.get_base_type_of_literal_type(source);
            Debug_.assert(
                !self
                    .type_checker
                    .is_type_assignable_to(&generalized_source, target),
                Some("generalized source shouldn't be assignable"),
            );
            generalized_source_type = self
                .type_checker
                .get_type_name_for_error_display(&generalized_source);
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

    pub(super) fn is_related_to(
        &self,
        original_source: &Type,
        original_target: &Type,
        recursion_flags: Option<RecursionFlags>,
        report_errors: bool,
        head_message: Option<&DiagnosticMessage>,
        intersection_state: Option<IntersectionState>,
    ) -> Ternary {
        let original_source = original_source.type_wrapper();
        let original_target = original_target.type_wrapper();
        let intersection_state = intersection_state.unwrap_or(IntersectionState::None);
        let recursion_flags = recursion_flags.unwrap_or(RecursionFlags::Both);

        let source = self
            .type_checker
            .get_normalized_type(&original_source, false);
        let target = self
            .type_checker
            .get_normalized_type(&original_target, true);

        let report_error_results = |source, target, result| {
            if result == Ternary::False && report_errors {
                let source = source;
                let target = target;
                self.report_relation_error(head_message, source, target);
            }
        };

        let mut report_error = |message: Cow<'static, DiagnosticMessage>,
                                args: Option<Vec<String>>| {
            self.report_error(&message, args);
        };

        if false
            || self.type_checker.is_simple_type_related_to(
                &*source,
                &*target,
                self.relation,
                if report_errors {
                    Some(&mut report_error)
                } else {
                    None
                },
            )
        {
            return Ternary::True;
        }

        let is_performing_excess_property_checks = !intersection_state
            .intersects(IntersectionState::Target)
            && (self.type_checker.is_object_literal_type(&source)
                && get_object_flags(&source).intersects(ObjectFlags::FreshLiteral));
        if is_performing_excess_property_checks {
            if self.has_excess_properties(&source, &target, report_errors) {
                if report_errors {
                    self.report_relation_error(
                        head_message,
                        &source,
                        if false { &original_target } else { &target },
                    );
                }
                return Ternary::False;
            }
        }

        let mut result = Ternary::False;

        if (source.flags().intersects(TypeFlags::Union)
            || target.flags().intersects(TypeFlags::Union))
            && self.type_checker.get_constituent_count(&source)
                * self.type_checker.get_constituent_count(&target)
                < 4
        {
            result = self.structured_type_related_to(
                &source,
                &target,
                report_errors,
                intersection_state | IntersectionState::UnionIntersectionCheck,
            );
        } else if source.flags().intersects(TypeFlags::UnionOrIntersection)
            || target.flags().intersects(TypeFlags::UnionOrIntersection)
        {
            result = self.recursive_type_related_to(
                &source,
                &target,
                report_errors,
                intersection_state | IntersectionState::UnionIntersectionCheck,
                recursion_flags,
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
                &source,
                &target,
                report_errors,
                intersection_state,
                recursion_flags,
            );
        }

        report_error_results(&source, &target, result);

        result
    }

    pub(super) fn has_excess_properties(
        &self,
        source: &Type, /*FreshObjectLiteralType*/
        target: &Type,
        report_errors: bool,
    ) -> bool {
        if !self.type_checker.is_excess_property_check_target(target) || false {
            return false;
        }
        let is_comparing_jsx_attributes = false;
        let reduced_target = target;
        for prop in self.type_checker.get_properties_of_type(source) {
            if self.should_check_as_excess_property(&prop, &source.symbol()) && true {
                if !self.type_checker.is_known_property(
                    reduced_target,
                    prop.escaped_name(),
                    is_comparing_jsx_attributes,
                ) {
                    if report_errors {
                        let error_target = self.type_checker.filter_type(reduced_target, |type_| {
                            self.type_checker.is_excess_property_check_target(type_)
                        });
                        let error_node = match self.error_node.as_ref().map(|rc| rc.clone()) {
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
                                    Some(
                                        vec![
                                            self.type_checker.symbol_to_string_(&prop, Option::<&Node>::None, None, None, None),
                                            self.type_checker.type_to_string_(&error_target, Option::<&Node>::None, None, None)
                                        ]
                                    )
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
            report_errors,
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

pub(super) struct ErrorCalculationState {
    pub error_info: Option<DiagnosticMessageChain>,
    pub last_skipped_info: Option<(Rc<Type>, Rc<Type>)>,
    pub incompatible_stack: Vec<(&'static DiagnosticMessage, Option<Vec<String>>)>,
    pub override_next_error_info: usize,
    pub related_info: Option<Vec<DiagnosticRelatedInformation>>,
}
