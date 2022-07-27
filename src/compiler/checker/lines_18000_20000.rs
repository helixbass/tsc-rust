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
    append, some, Diagnostic, UnionOrIntersectionType, UnionOrIntersectionTypeInterface, __String,
    add_related_info, chain_diagnostic_messages, concatenate_diagnostic_message_chains,
    create_diagnostic_for_node, create_diagnostic_for_node_from_message_chain, first_or_undefined,
    get_emit_script_target, get_object_flags, is_import_call, Debug_, DiagnosticMessage,
    DiagnosticMessageChain, DiagnosticRelatedInformation, Diagnostics, Node, NodeInterface,
    ObjectFlags, RelationComparisonResult, SignatureKind, Symbol, SymbolInterface, Ternary, Type,
    TypeChecker, TypeFlags, TypeInterface,
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

    pub(super) fn in_property_check(&self) -> bool {
        self.in_property_check.get()
    }

    pub(super) fn set_in_property_check(&self, in_property_check: bool) {
        self.in_property_check.set(in_property_check)
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
            Some(self.error_node.is_some()),
            self.head_message.clone(),
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
            self.report_error(Cow::Borrowed(stack_0_error), stack_0_args);
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
                Cow::Borrowed(if path.chars().last().unwrap() == ')' {
                    &Diagnostics::The_types_returned_by_0_are_incompatible_between_these_types
                } else {
                    &Diagnostics::The_types_of_0_are_incompatible_between_these_types
                }),
                Some(vec![path]),
            );
        } else {
            secondary_root_errors.remove(0);
        }
        for (msg, args) in secondary_root_errors {
            let original_value = msg.maybe_elided_in_compatability_pyramid();
            msg.set_elided_in_compatability_pyramid(Some(false));
            self.report_error(Cow::Borrowed(msg), args);
            msg.set_elided_in_compatability_pyramid(original_value);
        }
        if let Some((info_0, info_1)) = info {
            self.report_relation_error(None, &info_0, &info_1);
        }
    }

    pub(super) fn report_error(
        &self,
        message: Cow<'static, DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) {
        Debug_.assert(self.error_node.is_some(), None);
        if !self.incompatible_stack().is_empty() {
            self.report_incompatible_stack();
        }
        if matches!(message.maybe_elided_in_compatability_pyramid(), Some(true)) {
            return;
        }
        let error_info =
            { chain_diagnostic_messages(self.maybe_error_info().clone(), &message, args) };
        *self.maybe_error_info() = Some(error_info);
    }

    pub(super) fn associate_related_info(&self, info: DiagnosticRelatedInformation) {
        Debug_.assert(self.maybe_error_info().is_some(), None);
        if self.maybe_related_info().is_none() {
            *self.maybe_related_info() = Some(vec![info]);
        } else {
            self.maybe_related_info().as_mut().unwrap().push(info);
        }
    }

    pub(super) fn report_relation_error(
        &self,
        mut message: Option<Cow<'static, DiagnosticMessage>>,
        source: &Type,
        target: &Type,
    ) {
        if !self.incompatible_stack().is_empty() {
            self.report_incompatible_stack();
        }
        let (source_type, target_type) = self
            .type_checker
            .get_type_names_for_error_display(source, target);
        let mut generalized_source = source.type_wrapper();
        let mut generalized_source_type = source_type.clone();

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

        if target.flags().intersects(TypeFlags::TypeParameter) {
            let constraint = self.type_checker.get_base_constraint_of_type(target);
            let mut needs_original_source: Option<bool> = None;
            if let Some(constraint) = constraint.as_ref().filter(|constraint| {
                self.type_checker
                    .is_type_assignable_to(&generalized_source, constraint)
                    || {
                        needs_original_source =
                            Some(self.type_checker.is_type_assignable_to(source, constraint));
                        matches!(needs_original_source, Some(true))
                    }
            }) {
                self.report_error(
                    Cow::Borrowed(&Diagnostics::_0_is_assignable_to_the_constraint_of_type_1_but_1_could_be_instantiated_with_a_different_subtype_of_constraint_2),
                    Some(vec![
                        if matches!(
                            needs_original_source,
                            Some(true)
                        ) {
                            source_type.clone()
                        } else {
                            generalized_source_type.clone()
                        },
                        target_type.clone(),
                        self.type_checker.type_to_string_(
                            constraint,
                            Option::<&Node>::None,
                            None,
                            None,
                        ),
                    ])
                );
            } else {
                *self.maybe_error_info() = None;
                self.report_error(
                    Cow::Borrowed(&Diagnostics::_0_could_be_instantiated_with_an_arbitrary_type_which_could_be_unrelated_to_1),
                    Some(vec![
                        target_type.clone(),
                        generalized_source_type.clone(),
                    ])
                );
            }
        }

        if message.is_none() {
            if ptr::eq(self.relation, &*self.type_checker.comparable_relation()) {
                message = Some(Cow::Borrowed(
                    &Diagnostics::Type_0_is_not_comparable_to_type_1,
                ));
            } else if source_type == target_type {
                message = Some(Cow::Borrowed(&Diagnostics::Type_0_is_not_assignable_to_type_1_Two_different_types_with_this_name_exist_but_they_are_unrelated));
            } else if matches!(self.type_checker.exact_optional_property_types, Some(true))
                && !self
                    .type_checker
                    .get_exact_optional_unassignable_properties(source, target)
                    .is_empty()
            {
                message = Some(Cow::Borrowed(&Diagnostics::Type_0_is_not_assignable_to_type_1_with_exactOptionalPropertyTypes_Colon_true_Consider_adding_undefined_to_the_types_of_the_target_s_properties));
            } else {
                if source.flags().intersects(TypeFlags::StringLiteral)
                    && target.flags().intersects(TypeFlags::Union)
                {
                    let suggested_type = self
                        .type_checker
                        .get_suggested_type_for_nonexistent_string_literal_type(source, target);
                    if let Some(suggested_type) = suggested_type.as_ref() {
                        self.report_error(
                            Cow::Borrowed(
                                &Diagnostics::Type_0_is_not_assignable_to_type_1_Did_you_mean_2,
                            ),
                            Some(vec![
                                generalized_source_type.clone(),
                                target_type.clone(),
                                self.type_checker.type_to_string_(
                                    suggested_type,
                                    Option::<&Node>::None,
                                    None,
                                    None,
                                ),
                            ]),
                        );
                    }
                }
                message = Some(Cow::Borrowed(
                    &Diagnostics::Type_0_is_not_assignable_to_type_1,
                ));
            }
        } else if matches!(
            message.as_ref(),
            Some(message) if ptr::eq(&**message, &*Diagnostics::Argument_of_type_0_is_not_assignable_to_parameter_of_type_1)
        ) && matches!(self.type_checker.exact_optional_property_types, Some(true))
            && !self
                .type_checker
                .get_exact_optional_unassignable_properties(source, target)
                .is_empty()
        {
            message = Some(Cow::Borrowed(&Diagnostics::Argument_of_type_0_is_not_assignable_to_parameter_of_type_1_with_exactOptionalPropertyTypes_Colon_true_Consider_adding_undefined_to_the_types_of_the_target_s_properties));
        }

        self.report_error(
            message.unwrap(),
            Some(vec![generalized_source_type, target_type]),
        );
    }

    pub(super) fn try_elaborate_errors_for_primitives_and_objects(
        &self,
        source: &Type,
        target: &Type,
    ) {
        let source_type = if self
            .type_checker
            .symbol_value_declaration_is_context_sensitive(&source.symbol())
        {
            self.type_checker.type_to_string_(
                source,
                source.symbol().maybe_value_declaration(),
                None,
                None,
            )
        } else {
            self.type_checker
                .type_to_string_(source, Option::<&Node>::None, None, None)
        };
        let target_type = if self
            .type_checker
            .symbol_value_declaration_is_context_sensitive(&target.symbol())
        {
            self.type_checker.type_to_string_(
                target,
                target.symbol().maybe_value_declaration(),
                None,
                None,
            )
        } else {
            self.type_checker
                .type_to_string_(target, Option::<&Node>::None, None, None)
        };

        if (ptr::eq(&*self.type_checker.global_string_type(), source)
            && ptr::eq(&*self.type_checker.string_type(), target))
            || (ptr::eq(&*self.type_checker.global_number_type(), source)
                && ptr::eq(&*self.type_checker.number_type(), target))
            || (ptr::eq(&*self.type_checker.global_boolean_type(), source)
                && ptr::eq(&*self.type_checker.boolean_type(), target))
            || (ptr::eq(&*self.type_checker.get_global_es_symbol_type(false), source)
                && ptr::eq(&*self.type_checker.es_symbol_type(), target))
        {
            self.report_error(
                Cow::Borrowed(&Diagnostics::_0_is_a_primitive_but_1_is_a_wrapper_object_Prefer_using_0_when_possible),
                Some(vec![
                    target_type,
                    source_type,
                ])
            );
        }
    }

    pub(super) fn try_elaborate_array_like_errors(
        &self,
        source: &Type,
        target: &Type,
        report_errors: bool,
    ) -> bool {
        if self.type_checker.is_tuple_type(source) {
            if source.as_type_reference().target.as_tuple_type().readonly
                && self.type_checker.is_mutable_array_or_tuple(target)
            {
                if report_errors {
                    self.report_error(
                        Cow::Borrowed(&Diagnostics::The_type_0_is_readonly_and_cannot_be_assigned_to_the_mutable_type_1),
                        Some(vec![
                            self.type_checker.type_to_string_(
                                source,
                                Option::<&Node>::None,
                                None,
                                None,
                            ),
                            self.type_checker.type_to_string_(
                                target,
                                Option::<&Node>::None,
                                None,
                                None,
                            ),
                        ])
                    );
                }
                return false;
            }
            return self.type_checker.is_tuple_type(target)
                || self.type_checker.is_array_type(target);
        }
        if self.type_checker.is_readonly_array_type(source)
            && self.type_checker.is_mutable_array_or_tuple(target)
        {
            if report_errors {
                self.report_error(
                    Cow::Borrowed(&Diagnostics::The_type_0_is_readonly_and_cannot_be_assigned_to_the_mutable_type_1),
                    Some(vec![
                        self.type_checker.type_to_string_(
                            source,
                            Option::<&Node>::None,
                            None,
                            None,
                        ),
                        self.type_checker.type_to_string_(
                            target,
                            Option::<&Node>::None,
                            None,
                            None,
                        ),
                    ])
                );
            }
            return false;
        }
        if self.type_checker.is_tuple_type(target) {
            return self.type_checker.is_array_type(source);
        }
        true
    }

    pub(super) fn is_related_to_worker(
        &self,
        source: &Type,
        target: &Type,
        report_errors: bool,
    ) -> Ternary {
        self.is_related_to(
            source,
            target,
            Some(RecursionFlags::Both),
            Some(report_errors),
            None,
            None,
        )
    }

    pub(super) fn is_related_to(
        &self,
        original_source: &Type,
        original_target: &Type,
        recursion_flags: Option<RecursionFlags>,
        report_errors: Option<bool>,
        head_message: Option<Cow<'static, DiagnosticMessage>>,
        intersection_state: Option<IntersectionState>,
    ) -> Ternary {
        let original_source = original_source.type_wrapper();
        let original_target = original_target.type_wrapper();
        let recursion_flags = recursion_flags.unwrap_or(RecursionFlags::Both);
        let report_errors = report_errors.unwrap_or(false);
        let intersection_state = intersection_state.unwrap_or(IntersectionState::None);

        let mut report_error = |message: Cow<'static, DiagnosticMessage>,
                                args: Option<Vec<String>>| {
            self.report_error(message, args);
        };

        if original_source.flags().intersects(TypeFlags::Object)
            && original_target.flags().intersects(TypeFlags::Primitive)
        {
            if self.type_checker.is_simple_type_related_to(
                &original_source,
                &original_target,
                self.relation,
                if report_errors {
                    Some(&mut report_error)
                } else {
                    None
                },
            ) {
                return Ternary::True;
            }
            self.report_error_results(
                report_errors,
                head_message.clone(),
                &original_source,
                &original_target,
                Ternary::False,
                get_object_flags(&original_source).intersects(ObjectFlags::JsxAttributes),
            );
            return Ternary::False;
        }

        let source = self
            .type_checker
            .get_normalized_type(&original_source, false);
        let mut target = self
            .type_checker
            .get_normalized_type(&original_target, true);

        if Rc::ptr_eq(&source, &target) {
            return Ternary::True;
        }

        if ptr::eq(self.relation, &*self.type_checker.identity_relation()) {
            return self.is_identical_to(&source, &target, recursion_flags);
        }

        if source.flags().intersects(TypeFlags::TypeParameter)
            && matches!(
                self.type_checker.get_constraint_of_type(&source),
                Some(constraint) if Rc::ptr_eq(&constraint, &target)
            )
        {
            return Ternary::True;
        }

        if target.flags().intersects(TypeFlags::Union)
            && source.flags().intersects(TypeFlags::Object)
            && target
                .as_union_or_intersection_type_interface()
                .types()
                .len()
                <= 3
            && self
                .type_checker
                .maybe_type_of_kind(&target, TypeFlags::Nullable)
        {
            let null_stripped_target = self
                .type_checker
                .extract_types_of_kind(&target, !TypeFlags::Nullable);
            if !null_stripped_target
                .flags()
                .intersects(TypeFlags::Union | TypeFlags::Never)
            {
                target = self
                    .type_checker
                    .get_normalized_type(&null_stripped_target, true);
            }
            if Rc::ptr_eq(&source, &null_stripped_target) {
                return Ternary::True;
            }
        }

        if ptr::eq(self.relation, &*self.type_checker.comparable_relation())
            && !target.flags().intersects(TypeFlags::Never)
            && self
                .type_checker
                .is_simple_type_related_to(&target, &source, self.relation, None)
            || self.type_checker.is_simple_type_related_to(
                &source,
                &target,
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

        let is_comparing_jsx_attributes =
            get_object_flags(&source).intersects(ObjectFlags::JsxAttributes);
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
                        if original_target.maybe_alias_symbol().is_some() {
                            &original_target
                        } else {
                            &target
                        },
                    );
                }
                return Ternary::False;
            }
        }

        let is_performing_common_property_checks =
            !ptr::eq(self.relation, &*self.type_checker.comparable_relation())
                && !intersection_state.intersects(IntersectionState::Target)
                && source
                    .flags()
                    .intersects(TypeFlags::Primitive | TypeFlags::Object | TypeFlags::Intersection)
                && !Rc::ptr_eq(&source, &self.type_checker.global_object_type())
                && target
                    .flags()
                    .intersects(TypeFlags::Object | TypeFlags::Intersection)
                && self.type_checker.is_weak_type(&target)
                && (!self.type_checker.get_properties_of_type(&source).is_empty()
                    || self
                        .type_checker
                        .type_has_call_or_construct_signatures(&source));
        if is_performing_common_property_checks
            && !self.type_checker.has_common_properties(
                &source,
                &target,
                is_comparing_jsx_attributes,
            )
        {
            if report_errors {
                let source_string = self.type_checker.type_to_string_(
                    if original_source.maybe_alias_symbol().is_some() {
                        &original_source
                    } else {
                        &source
                    },
                    Option::<&Node>::None,
                    None,
                    None,
                );
                let target_string = self.type_checker.type_to_string_(
                    if original_target.maybe_alias_symbol().is_some() {
                        &original_target
                    } else {
                        &target
                    },
                    Option::<&Node>::None,
                    None,
                    None,
                );
                let calls = self
                    .type_checker
                    .get_signatures_of_type(&source, SignatureKind::Call);
                let constructs = self
                    .type_checker
                    .get_signatures_of_type(&source, SignatureKind::Construct);
                if !calls.is_empty()
                    && self.is_related_to(
                        &self
                            .type_checker
                            .get_return_type_of_signature(calls[0].clone()),
                        &target,
                        Some(RecursionFlags::Source),
                        Some(false),
                        None,
                        None,
                    ) != Ternary::False
                    || !constructs.is_empty()
                        && self.is_related_to(
                            &self
                                .type_checker
                                .get_return_type_of_signature(constructs[0].clone()),
                            &target,
                            Some(RecursionFlags::Source),
                            Some(false),
                            None,
                            None,
                        ) != Ternary::False
                {
                    self.report_error(
                        Cow::Borrowed(&Diagnostics::Value_of_type_0_has_no_properties_in_common_with_type_1_Did_you_mean_to_call_it),
                        Some(vec![
                            source_string,
                            target_string,
                        ])
                    );
                } else {
                    self.report_error(
                        Cow::Borrowed(&Diagnostics::Type_0_has_no_properties_in_common_with_type_1),
                        Some(vec![source_string, target_string]),
                    );
                }
            }
            return Ternary::False;
        }

        self.trace_unions_or_intersections_too_large(&source, &target);

        let mut result = Ternary::False;
        let save_error_info = self.capture_error_calculation_state();

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
                || target
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
            if result != Ternary::False {
                self.reset_error_info(save_error_info.clone());
            }
        }
        if result == Ternary::False
            && source
                .flags()
                .intersects(TypeFlags::Intersection | TypeFlags::TypeParameter)
        {
            let constraint = self.type_checker.get_effective_constraint_of_intersection(
                &if source.flags().intersects(TypeFlags::Intersection) {
                    source
                        .as_union_or_intersection_type_interface()
                        .types()
                        .to_owned()
                } else {
                    vec![source.clone()]
                },
                target.flags().intersects(TypeFlags::Union),
            );
            if let Some(constraint) = constraint.as_ref() {
                if source.flags().intersects(TypeFlags::Intersection)
                    || target.flags().intersects(TypeFlags::Union)
                {
                    if self
                        .type_checker
                        .every_type(constraint, |c| !ptr::eq(c, &*source))
                    {
                        result = self.is_related_to(
                            constraint,
                            &target,
                            Some(RecursionFlags::Source),
                            Some(false),
                            None,
                            Some(intersection_state),
                        );
                        if result != Ternary::False {
                            self.reset_error_info(save_error_info);
                        }
                    }
                }
            }
        }

        if result != Ternary::False
            && !self.in_property_check()
            && (target.flags().intersects(TypeFlags::Intersection)
                && (is_performing_excess_property_checks || is_performing_common_property_checks)
                || self.type_checker.is_non_generic_object_type(&target)
                    && !self.type_checker.is_array_type(&target)
                    && !self.type_checker.is_tuple_type(&target)
                    && source.flags().intersects(TypeFlags::Intersection)
                    && self
                        .type_checker
                        .get_apparent_type(&source)
                        .flags()
                        .intersects(TypeFlags::StructuredType)
                    && !some(
                        Some(source.as_union_or_intersection_type().types()),
                        Some(|t: &Rc<Type>| {
                            get_object_flags(t).intersects(ObjectFlags::NonInferrableType)
                        }),
                    ))
        {
            self.set_in_property_check(true);
            result &= self.recursive_type_related_to(
                &source,
                &target,
                report_errors,
                IntersectionState::PropertyCheck,
                recursion_flags,
            );
            self.set_in_property_check(false);
        }

        self.report_error_results(
            report_errors,
            head_message,
            &source,
            &target,
            result,
            is_comparing_jsx_attributes,
        );
        result
    }

    pub(super) fn report_error_results(
        &self,
        report_errors: bool,
        head_message: Option<Cow<'static, DiagnosticMessage>>,
        source: &Type,
        target: &Type,
        result: Ternary,
        is_comparing_jsx_attributes: bool,
    ) {
        if result == Ternary::False && report_errors {
            let source = source;
            let target = target;
            self.report_relation_error(head_message, source, target);
        }
    }

    pub(super) fn trace_unions_or_intersections_too_large(&self, source: &Type, target: &Type) {
        unimplemented!()
    }

    pub(super) fn is_identical_to(
        &self,
        source: &Type,
        target: &Type,
        recursion_flags: RecursionFlags,
    ) -> Ternary {
        unimplemented!()
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
                                    Cow::Borrowed(&*Diagnostics::Object_literal_may_only_specify_known_properties_and_0_does_not_exist_in_type_1),
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

#[derive(Clone)]
pub(super) struct ErrorCalculationState {
    pub error_info: Option<DiagnosticMessageChain>,
    pub last_skipped_info: Option<(Rc<Type>, Rc<Type>)>,
    pub incompatible_stack: Vec<(&'static DiagnosticMessage, Option<Vec<String>>)>,
    pub override_next_error_info: usize,
    pub related_info: Option<Vec<DiagnosticRelatedInformation>>,
}
