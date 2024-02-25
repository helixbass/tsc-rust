use std::{
    borrow::Cow,
    cell::{Cell, RefCell, RefMut},
    collections::HashMap,
    io, ptr,
    rc::Rc,
};

use id_arena::Id;

use super::{
    CheckTypeContainingMessageChain, CheckTypeErrorOutputContainer, ExpandingFlags,
    IntersectionState, JsxNames, RecursionFlags,
};
use crate::{
    add_related_info, append, chain_diagnostic_messages, concatenate_diagnostic_message_chains,
    contains, create_diagnostic_for_node, create_diagnostic_for_node_from_message_chain,
    find_ancestor, first_or_undefined, get_emit_script_target, get_object_flags, impl_has_arena,
    is_identifier, is_identifier_text, is_import_call, is_jsx_attribute, is_jsx_attributes,
    is_jsx_opening_like_element, is_object_literal_element_like, maybe_get_source_file_of_node,
    some, try_reduce_left, AllArenas, Debug_, Diagnostic, DiagnosticMessage,
    DiagnosticMessageChain, DiagnosticRelatedInformation, Diagnostics, HasArena, InArena, Node,
    NodeInterface, ObjectFlags, ObjectFlagsTypeInterface, OptionTry, RelationComparisonResult,
    SignatureKind, Symbol, SymbolInterface, Ternary, Type, TypeChecker, TypeFlags, TypeInterface,
    UnionOrIntersectionTypeInterface,
};

pub struct CheckTypeRelatedTo {
    arena: *const AllArenas,
    _arena_id: Cell<Option<Id<CheckTypeRelatedTo>>>,
    pub type_checker: Id<TypeChecker>,
    pub source: Id<Type>,
    pub target: Id<Type>,
    pub relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
    pub error_node: Cell<Option<Id<Node>>>,
    pub head_message: Option<Cow<'static, DiagnosticMessage>>,
    pub containing_message_chain: Option<Id<Box<dyn CheckTypeContainingMessageChain>>>,
    pub error_output_container: Option<Id<Box<dyn CheckTypeErrorOutputContainer>>>,
    pub error_info: RefCell<Option<Rc<DiagnosticMessageChain>>>,
    pub related_info: RefCell<Option<Vec<DiagnosticRelatedInformation>>>,
    pub maybe_keys: RefCell<Option<Vec<String>>>,
    pub source_stack: RefCell<Option<Vec<Id<Type>>>>,
    pub target_stack: RefCell<Option<Vec<Id<Type>>>>,
    pub maybe_count: Cell<usize>,
    pub source_depth: Cell<usize>,
    pub target_depth: Cell<usize>,
    pub(super) expanding_flags: Cell<ExpandingFlags>,
    pub overflow: Cell<bool>,
    pub override_next_error_info: Cell<usize>,
    pub last_skipped_info: Cell<Option<(Id<Type>, Id<Type>)>>,
    pub incompatible_stack: RefCell<Vec<(&'static DiagnosticMessage, Option<Vec<String>>)>>,
    pub in_property_check: Cell<bool>,
}

impl CheckTypeRelatedTo {
    pub(super) fn new(
        type_checker: &TypeChecker,
        source: Id<Type>,
        target: Id<Type>,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        error_node: Option<Id<Node>>,
        head_message: Option<Cow<'static, DiagnosticMessage>>,
        containing_message_chain: Option<Id<Box<dyn CheckTypeContainingMessageChain>>>,
        error_output_container: Option<Id<Box<dyn CheckTypeErrorOutputContainer>>>,
        arena: &impl HasArena,
    ) -> Id<Self> {
        type_checker.alloc_check_type_related_to(Self {
            arena: arena.arena(),
            _arena_id: Default::default(),
            type_checker: type_checker.arena_id(),
            source,
            target,
            relation,
            error_node: Cell::new(error_node),
            head_message,
            containing_message_chain,
            error_output_container,
            error_info: Default::default(),
            related_info: Default::default(),
            maybe_keys: Default::default(),
            source_stack: Default::default(),
            target_stack: Default::default(),
            maybe_count: Default::default(),
            source_depth: Default::default(),
            target_depth: Default::default(),
            expanding_flags: Cell::new(ExpandingFlags::None),
            overflow: Default::default(),
            override_next_error_info: Default::default(),
            last_skipped_info: Default::default(),
            incompatible_stack: RefCell::new(vec![]),
            in_property_check: Default::default(),
        })
    }

    #[allow(dead_code)]
    pub(super) fn arena_id(&self) -> Id<Self> {
        self._arena_id.get().unwrap()
    }

    pub fn set_arena_id(&self, id: Id<Self>) {
        self._arena_id.set(Some(id));
    }

    pub(super) fn maybe_error_node(&self) -> Option<Id<Node>> {
        self.error_node.get()
    }

    pub(super) fn set_error_node(&self, error_node: Option<Id<Node>>) {
        self.error_node.set(error_node);
    }

    pub(super) fn maybe_error_info(&self) -> Option<Rc<DiagnosticMessageChain>> {
        self.error_info.borrow().clone()
    }

    pub(super) fn maybe_error_info_mut(&self) -> RefMut<Option<Rc<DiagnosticMessageChain>>> {
        self.error_info.borrow_mut()
    }

    pub(super) fn maybe_related_info(&self) -> RefMut<Option<Vec<DiagnosticRelatedInformation>>> {
        self.related_info.borrow_mut()
    }

    pub(super) fn maybe_keys(&self) -> RefMut<Option<Vec<String>>> {
        self.maybe_keys.borrow_mut()
    }

    pub(super) fn maybe_source_stack(&self) -> RefMut<Option<Vec<Id<Type>>>> {
        self.source_stack.borrow_mut()
    }

    pub(super) fn maybe_target_stack(&self) -> RefMut<Option<Vec<Id<Type>>>> {
        self.target_stack.borrow_mut()
    }

    pub(super) fn maybe_count(&self) -> usize {
        self.maybe_count.get()
    }

    pub(super) fn set_maybe_count(&self, maybe_count: usize) {
        self.maybe_count.set(maybe_count);
    }

    pub(super) fn source_depth(&self) -> usize {
        self.source_depth.get()
    }

    pub(super) fn set_source_depth(&self, source_depth: usize) {
        self.source_depth.set(source_depth);
    }

    pub(super) fn target_depth(&self) -> usize {
        self.target_depth.get()
    }

    pub(super) fn set_target_depth(&self, target_depth: usize) {
        self.target_depth.set(target_depth);
    }

    pub(super) fn expanding_flags(&self) -> ExpandingFlags {
        self.expanding_flags.get()
    }

    pub(super) fn set_expanding_flags(&self, expanding_flags: ExpandingFlags) {
        self.expanding_flags.set(expanding_flags);
    }

    pub(super) fn overflow(&self) -> bool {
        self.overflow.get()
    }

    pub(super) fn set_overflow(&self, overflow: bool) {
        self.overflow.set(overflow);
    }

    pub(super) fn override_next_error_info(&self) -> usize {
        self.override_next_error_info.get()
    }

    pub(super) fn set_override_next_error_info(&self, override_next_error_info: usize) {
        self.override_next_error_info.set(override_next_error_info);
    }

    pub(super) fn maybe_last_skipped_info(&self) -> Option<(Id<Type>, Id<Type>)> {
        self.last_skipped_info.get()
    }

    pub(super) fn set_last_skipped_info(&self, last_skipped_info: Option<(Id<Type>, Id<Type>)>) {
        self.last_skipped_info.set(last_skipped_info);
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

    pub(super) fn call(self_: Id<Self>, arena: &impl HasArena) -> io::Result<bool> {
        Debug_.assert(
            !Rc::ptr_eq(
                &self_.ref_(arena).relation,
                &self_.ref_(arena).type_checker.ref_(arena).identity_relation,
            ) || self_.ref_(arena).maybe_error_node().is_none(),
            Some("no error reporting in identity checking"),
        );

        let result = Self::is_related_to(
            self_,
            arena,
            self_.ref_(arena).source,
            self_.ref_(arena).target,
            Some(RecursionFlags::Both),
            Some(self_.ref_(arena).maybe_error_node().is_some()),
            self_.ref_(arena).head_message.clone(),
            None,
        )?;
        if !self_.ref_(arena).incompatible_stack().is_empty() {
            Self::report_incompatible_stack(self_, arena)?;
        }
        if self_.ref_(arena).overflow() {
            // tracing?.instant(tracing.Phase.CheckTypes, "checkTypeRelatedTo_DepthLimit", { sourceId: source.id, targetId: target.id, depth: sourceDepth, targetDepth });
            let diag = self_.ref_(arena).type_checker.ref_(arena).error(
                self_.ref_(arena).maybe_error_node().or_else(|| {
                    self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .maybe_current_node()
                }),
                &Diagnostics::Excessive_stack_depth_comparing_types_0_and_1,
                Some(vec![
                    self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                        self_.ref_(arena).source,
                        Option::<Id<Node>>::None,
                        None,
                        None,
                    )?,
                    self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                        self_.ref_(arena).target,
                        Option::<Id<Node>>::None,
                        None,
                        None,
                    )?,
                ]),
            );
            if let Some(error_output_container) = self_.ref_(arena).error_output_container.as_ref()
            {
                error_output_container.ref_(arena).push_error(diag);
            }
        } else if self_.ref_(arena).maybe_error_info().is_some() {
            if let Some(containing_message_chain) =
                self_.ref_(arena).containing_message_chain.as_ref()
            {
                let chain = containing_message_chain.ref_(arena).get()?;
                if let Some(chain) = chain {
                    concatenate_diagnostic_message_chains(
                        &mut chain.borrow_mut(),
                        self_
                            .ref_(arena)
                            .maybe_error_info()
                            .as_deref()
                            .cloned()
                            .unwrap(),
                    );
                    // TODO: is this ever a problem? Not sure why I made containing_message_chain return an Rc<RefCell<DiagnosticMessageChain>> (vs just DiagnosticMessageChain)
                    // but seems like .clone()'ing here means that that original Rc'd DiagnosticMessageChain now no longer is "sharing this mutation"
                    *self_.ref_(arena).maybe_error_info_mut() =
                        Some(Rc::new((*chain).borrow().clone()));
                }
            }

            let mut related_information: Option<Vec<Id<DiagnosticRelatedInformation>>> = None;
            if self_.ref_(arena).head_message.is_some()
                && self_.ref_(arena).maybe_error_node().is_some()
            {
                if result == Ternary::False {
                    if let Some(source_symbol) = self_.ref_(arena).source.ref_(arena).maybe_symbol()
                    {
                        let links = self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_symbol_links(source_symbol);
                        if let Some(links_originating_import) =
                            links.ref_(arena).originating_import.clone().filter(
                                |&links_originating_import| {
                                    !is_import_call(links_originating_import, arena)
                                },
                            )
                        {
                            let helpful_retry = self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .check_type_related_to(
                                    self_
                                        .ref_(arena)
                                        .type_checker
                                        .ref_(arena)
                                        .get_type_of_symbol(links.ref_(arena).target.unwrap())?,
                                    self_.ref_(arena).target,
                                    self_.ref_(arena).relation.clone(),
                                    Option::<Id<Node>>::None,
                                    None,
                                    None,
                                    None,
                                )?;
                            if helpful_retry {
                                let diag: Id<DiagnosticRelatedInformation> = self_.ref_(arena).alloc_diagnostic_related_information(create_diagnostic_for_node(
                                    links_originating_import,
                                    &Diagnostics::Type_originates_at_this_import_A_namespace_style_import_cannot_be_called_or_constructed_and_will_cause_a_failure_at_runtime_Consider_using_a_default_import_or_import_require_here_instead,
                                    None,
                                    arena,
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
            let diag: Id<Diagnostic> = self_.ref_(arena).alloc_diagnostic(
                create_diagnostic_for_node_from_message_chain(
                    self_.ref_(arena).maybe_error_node().unwrap(),
                    self_
                        .ref_(arena)
                        .maybe_error_info()
                        .as_deref()
                        .cloned()
                        .unwrap(),
                    related_information,
                    arena,
                )
                .into(),
            );
            if let Some(related_info) = self_.ref_(arena).maybe_related_info().clone() {
                add_related_info(
                    &diag.ref_(arena),
                    related_info
                        .into_iter()
                        .map(|related_info| {
                            self_
                                .ref_(arena)
                                .alloc_diagnostic_related_information(related_info)
                        })
                        .collect(),
                );
            }
            if let Some(error_output_container) = self_.ref_(arena).error_output_container.as_ref()
            {
                error_output_container.ref_(arena).push_error(diag.clone());
            }
            if match self_.ref_(arena).error_output_container.as_ref() {
                None => true,
                Some(error_output_container) => !matches!(
                    error_output_container.ref_(arena).skip_logging(),
                    Some(true)
                ),
            } {
                self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .diagnostics()
                    .add(diag);
            }
        }
        if self_.ref_(arena).maybe_error_node().is_some() {
            if let Some(error_output_container) = self_
                .ref_(arena)
                .error_output_container
                .as_ref()
                .filter(|error_output_container| {
                    matches!(
                        error_output_container.ref_(arena).skip_logging(),
                        Some(true),
                    )
                })
            {
                if result == Ternary::False {
                    Debug_.assert(
                        error_output_container.ref_(arena).errors_len() > 0,
                        Some("missed opportunity to interact with error."),
                    );
                }
            }
        }

        Ok(result != Ternary::False)
    }

    pub(super) fn reset_error_info(&self, saved: ErrorCalculationState) {
        let ErrorCalculationState {
            error_info,
            last_skipped_info,
            incompatible_stack,
            override_next_error_info,
            related_info,
        } = saved;
        *self.maybe_error_info_mut() = error_info;
        self.set_last_skipped_info(last_skipped_info);
        *self.incompatible_stack() = incompatible_stack;
        self.set_override_next_error_info(override_next_error_info);
        *self.maybe_related_info() = related_info;
    }

    pub(super) fn capture_error_calculation_state(&self) -> ErrorCalculationState {
        ErrorCalculationState {
            error_info: self.maybe_error_info(),
            last_skipped_info: self.maybe_last_skipped_info(),
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
        self.set_last_skipped_info(None);
        self.incompatible_stack().push((message, args));
    }

    pub(super) fn report_incompatible_stack(
        self_: Id<Self>,
        arena: &impl HasArena,
    ) -> io::Result<()> {
        let mut stack = self_.ref_(arena).incompatible_stack().clone();
        *self_.ref_(arena).incompatible_stack() = vec![];
        let info = self_.ref_(arena).maybe_last_skipped_info();
        self_.ref_(arena).set_last_skipped_info(None);
        if stack.len() == 1 {
            let (stack_0_error, stack_0_args) = stack.into_iter().next().unwrap();
            Self::report_error(self_, arena, Cow::Borrowed(stack_0_error), stack_0_args)?;
            if let Some((info_0, info_1)) = info {
                Self::report_relation_error(self_, arena, None, info_0, info_1)?;
            }
            return Ok(());
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
                } else if is_identifier_text(str, Some(get_emit_script_target(&self_.ref_(arena).type_checker.ref_(arena).compiler_options.ref_(arena))), None) {
                    path = format!("{}.{}", path, str);
                } else if {
                    let str_chars: Vec<char> = str.chars().collect();
                    !str_chars.is_empty() && str_chars[0] == '[' && str_chars[str_chars.len() - 1] == ']'
                } {
                    path = format!("{}{}", path, str);
                } else {
                    path = format!("{}[{}]", path, str);
                }
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
            } else if msg.code == Diagnostics::Type_at_position_0_in_source_is_not_compatible_with_type_at_position_1_in_target.code {
                secondary_root_errors.insert(0, (&Diagnostics::Type_at_position_0_in_source_is_not_compatible_with_type_at_position_1_in_target, args));
            } else if msg.code == Diagnostics::Type_at_positions_0_through_1_in_source_is_not_compatible_with_type_at_position_2_in_target.code {
                secondary_root_errors.insert(0, (&Diagnostics::Type_at_positions_0_through_1_in_source_is_not_compatible_with_type_at_position_2_in_target, args));
            } else {
                Debug_.fail(Some(&format!("Unhandled Diagnostic: {}", msg.code)));
            }
        }
        if !path.is_empty() {
            Self::report_error(
                self_,
                arena,
                Cow::Borrowed(if path.chars().last().unwrap() == ')' {
                    &Diagnostics::The_types_returned_by_0_are_incompatible_between_these_types
                } else {
                    &Diagnostics::The_types_of_0_are_incompatible_between_these_types
                }),
                Some(vec![path]),
            )?;
        } else {
            secondary_root_errors.remove(0);
        }
        for (msg, args) in secondary_root_errors {
            let original_value = msg.maybe_elided_in_compatability_pyramid();
            msg.set_elided_in_compatability_pyramid(Some(false));
            Self::report_error(self_, arena, Cow::Borrowed(msg), args)?;
            msg.set_elided_in_compatability_pyramid(original_value);
        }
        if let Some((info_0, info_1)) = info {
            Self::report_relation_error(self_, arena, None, info_0, info_1)?;
        }

        Ok(())
    }

    pub(super) fn report_error(
        self_: Id<Self>,
        arena: &impl HasArena,
        message: Cow<'static, DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> io::Result<()> {
        Debug_.assert(self_.ref_(arena).maybe_error_node().is_some(), None);
        if !self_.ref_(arena).incompatible_stack().is_empty() {
            Self::report_incompatible_stack(self_, arena)?;
        }
        if matches!(message.maybe_elided_in_compatability_pyramid(), Some(true)) {
            return Ok(());
        }
        let error_info = {
            chain_diagnostic_messages(
                self_.ref_(arena).maybe_error_info().as_deref().cloned(),
                &message,
                args,
            )
        };
        *self_.ref_(arena).maybe_error_info_mut() = Some(Rc::new(error_info));

        Ok(())
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
        self_: Id<Self>,
        arena: &impl HasArena,
        mut message: Option<Cow<'static, DiagnosticMessage>>,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<()> {
        if !self_.ref_(arena).incompatible_stack().is_empty() {
            Self::report_incompatible_stack(self_, arena)?;
        }
        let (source_type, target_type) = self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .get_type_names_for_error_display(source, target)?;
        let mut generalized_source = source;
        let mut generalized_source_type = source_type.clone();

        if self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .is_literal_type(source)
            && !self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .type_could_have_top_level_singleton_types(target)?
        {
            generalized_source = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_base_type_of_literal_type(source)?;
            Debug_.assert(
                !self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_type_assignable_to(generalized_source, target)?,
                Some("generalized source shouldn't be assignable"),
            );
            generalized_source_type = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_type_name_for_error_display(generalized_source)?;
        }

        if target
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::TypeParameter)
        {
            let constraint = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_base_constraint_of_type(target)?;
            let mut needs_original_source: Option<bool> = None;
            if let Some(constraint) = constraint.try_filter(|&constraint| -> io::Result<_> {
                Ok(self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_type_assignable_to(generalized_source, constraint)?
                    || {
                        needs_original_source = Some(
                            self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .is_type_assignable_to(source, constraint)?,
                        );
                        matches!(needs_original_source, Some(true))
                    })
            })? {
                Self::report_error(
                    self_,
                    arena,
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
                        self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                            constraint,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                    ])
                )?;
            } else {
                *self_.ref_(arena).maybe_error_info_mut() = None;
                Self::report_error(
                    self_,
                    arena,
                    Cow::Borrowed(&Diagnostics::_0_could_be_instantiated_with_an_arbitrary_type_which_could_be_unrelated_to_1),
                    Some(vec![
                        target_type.clone(),
                        generalized_source_type.clone(),
                    ])
                )?;
            }
        }

        if message.is_none() {
            if Rc::ptr_eq(
                &self_.ref_(arena).relation,
                &self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .comparable_relation,
            ) {
                message = Some(Cow::Borrowed(
                    &Diagnostics::Type_0_is_not_comparable_to_type_1,
                ));
            } else if source_type == target_type {
                message = Some(Cow::Borrowed(&Diagnostics::Type_0_is_not_assignable_to_type_1_Two_different_types_with_this_name_exist_but_they_are_unrelated));
            } else if matches!(
                self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .exact_optional_property_types,
                Some(true)
            ) && !self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_exact_optional_unassignable_properties(source, target)?
                .is_empty()
            {
                message = Some(Cow::Borrowed(&Diagnostics::Type_0_is_not_assignable_to_type_1_with_exactOptionalPropertyTypes_Colon_true_Consider_adding_undefined_to_the_types_of_the_target_s_properties));
            } else {
                if source
                    .ref_(arena)
                    .flags()
                    .intersects(TypeFlags::StringLiteral)
                    && target.ref_(arena).flags().intersects(TypeFlags::Union)
                {
                    let suggested_type = self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_suggested_type_for_nonexistent_string_literal_type(source, target);
                    if let Some(suggested_type) = suggested_type {
                        Self::report_error(
                            self_,
                            arena,
                            Cow::Borrowed(
                                &Diagnostics::Type_0_is_not_assignable_to_type_1_Did_you_mean_2,
                            ),
                            Some(vec![
                                generalized_source_type.clone(),
                                target_type.clone(),
                                self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                                    suggested_type,
                                    Option::<Id<Node>>::None,
                                    None,
                                    None,
                                )?,
                            ]),
                        )?;
                        return Ok(());
                    }
                }
                message = Some(Cow::Borrowed(
                    &Diagnostics::Type_0_is_not_assignable_to_type_1,
                ));
            }
        } else if matches!(
            message.as_ref(),
            Some(message) if ptr::eq(&**message, &*Diagnostics::Argument_of_type_0_is_not_assignable_to_parameter_of_type_1)
        ) && matches!(
            self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .exact_optional_property_types,
            Some(true)
        ) && !self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .get_exact_optional_unassignable_properties(source, target)?
            .is_empty()
        {
            message = Some(Cow::Borrowed(&Diagnostics::Argument_of_type_0_is_not_assignable_to_parameter_of_type_1_with_exactOptionalPropertyTypes_Colon_true_Consider_adding_undefined_to_the_types_of_the_target_s_properties));
        }

        Self::report_error(
            self_,
            arena,
            message.unwrap(),
            Some(vec![generalized_source_type, target_type]),
        )?;

        Ok(())
    }

    pub(super) fn try_elaborate_errors_for_primitives_and_objects(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>,
    ) -> io::Result<()> {
        let source_type = if self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .symbol_value_declaration_is_context_sensitive(source.ref_(arena).maybe_symbol())?
        {
            self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                source,
                source
                    .ref_(arena)
                    .symbol()
                    .ref_(arena)
                    .maybe_value_declaration(),
                None,
                None,
            )?
        } else {
            self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                source,
                Option::<Id<Node>>::None,
                None,
                None,
            )?
        };
        let target_type = if self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .symbol_value_declaration_is_context_sensitive(target.ref_(arena).maybe_symbol())?
        {
            self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                target,
                target
                    .ref_(arena)
                    .symbol()
                    .ref_(arena)
                    .maybe_value_declaration(),
                None,
                None,
            )?
        } else {
            self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                target,
                Option::<Id<Node>>::None,
                None,
                None,
            )?
        };

        if (self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .global_string_type()
            == source
            && self_.ref_(arena).type_checker.ref_(arena).string_type() == target)
            || (self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .global_number_type()
                == source
                && self_.ref_(arena).type_checker.ref_(arena).number_type() == target)
            || (self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .global_boolean_type()
                == source
                && self_.ref_(arena).type_checker.ref_(arena).boolean_type() == target)
            || (self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_global_es_symbol_type(false)?
                == source
                && self_.ref_(arena).type_checker.ref_(arena).es_symbol_type() == target)
        {
            Self::report_error(
                self_,
                arena,
                Cow::Borrowed(&Diagnostics::_0_is_a_primitive_but_1_is_a_wrapper_object_Prefer_using_0_when_possible),
                Some(vec![
                    target_type,
                    source_type,
                ])
            )?;
        }

        Ok(())
    }

    pub(super) fn try_elaborate_array_like_errors(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>,
        report_errors: bool,
    ) -> io::Result<bool> {
        if self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .is_tuple_type(source)
        {
            if source
                .ref_(arena)
                .as_type_reference()
                .target
                .ref_(arena)
                .as_tuple_type()
                .readonly
                && self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_mutable_array_or_tuple(target)
            {
                if report_errors {
                    Self::report_error(
                        self_,
                        arena,
                        Cow::Borrowed(&Diagnostics::The_type_0_is_readonly_and_cannot_be_assigned_to_the_mutable_type_1),
                        Some(vec![
                            self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                                source,
                                Option::<Id<Node>>::None,
                                None,
                                None,
                            )?,
                            self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                                target,
                                Option::<Id<Node>>::None,
                                None,
                                None,
                            )?,
                        ])
                    )?;
                }
                return Ok(false);
            }
            return Ok(self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_tuple_type(target)
                || self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_array_type(target));
        }
        if self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .is_readonly_array_type(source)
            && self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_mutable_array_or_tuple(target)
        {
            if report_errors {
                Self::report_error(
                    self_,
                    arena,
                    Cow::Borrowed(&Diagnostics::The_type_0_is_readonly_and_cannot_be_assigned_to_the_mutable_type_1),
                    Some(vec![
                        self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                            source,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                        self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                            target,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                    ])
                )?;
            }
            return Ok(false);
        }
        if self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .is_tuple_type(target)
        {
            return Ok(self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_array_type(source));
        }
        Ok(true)
    }

    pub(super) fn is_related_to_worker(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>,
        report_errors: bool,
    ) -> io::Result<Ternary> {
        Self::is_related_to(
            self_,
            arena,
            source,
            target,
            Some(RecursionFlags::Both),
            Some(report_errors),
            None,
            None,
        )
    }

    pub(super) fn is_related_to(
        self_: Id<Self>,
        arena: &impl HasArena,
        original_source: Id<Type>,
        original_target: Id<Type>,
        recursion_flags: Option<RecursionFlags>,
        report_errors: Option<bool>,
        head_message: Option<Cow<'static, DiagnosticMessage>>,
        intersection_state: Option<IntersectionState>,
    ) -> io::Result<Ternary> {
        let recursion_flags = recursion_flags.unwrap_or(RecursionFlags::Both);
        let report_errors = report_errors.unwrap_or(false);
        let intersection_state = intersection_state.unwrap_or(IntersectionState::None);

        let mut report_error = |message: Cow<'static, DiagnosticMessage>,
                                args: Option<Vec<String>>| {
            Self::report_error(self_, arena, message, args)?;

            Ok(())
        };

        if original_source
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::Object)
            && original_target
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::Primitive)
        {
            if self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_simple_type_related_to(
                    original_source,
                    original_target,
                    &(*self_.ref_(arena).relation).borrow(),
                    if report_errors {
                        Some(&mut report_error)
                    } else {
                        None
                    },
                )?
            {
                return Ok(Ternary::True);
            }
            Self::report_error_results(
                self_,
                arena,
                report_errors,
                head_message.clone(),
                original_source,
                original_target,
                original_source,
                original_target,
                Ternary::False,
                get_object_flags(&original_source.ref_(arena))
                    .intersects(ObjectFlags::JsxAttributes),
            )?;
            return Ok(Ternary::False);
        }

        let source = self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .get_normalized_type(original_source, false)?;
        let mut target = self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .get_normalized_type(original_target, true)?;

        if source == target {
            return Ok(Ternary::True);
        }

        if Rc::ptr_eq(
            &self_.ref_(arena).relation,
            &self_.ref_(arena).type_checker.ref_(arena).identity_relation,
        ) {
            return Self::is_identical_to(self_, arena, source, target, recursion_flags);
        }

        if source
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::TypeParameter)
            && matches!(
                self_.ref_(arena).type_checker.ref_(arena).get_constraint_of_type(source)?,
                Some(constraint) if constraint == target
            )
        {
            return Ok(Ternary::True);
        }

        if target.ref_(arena).flags().intersects(TypeFlags::Union)
            && source.ref_(arena).flags().intersects(TypeFlags::Object)
            && target
                .ref_(arena)
                .as_union_or_intersection_type_interface()
                .types()
                .len()
                <= 3
            && self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .maybe_type_of_kind(target, TypeFlags::Nullable)
        {
            let null_stripped_target = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .extract_types_of_kind(target, !TypeFlags::Nullable);
            if !null_stripped_target
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::Union | TypeFlags::Never)
            {
                target = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_normalized_type(null_stripped_target, true)?;
            }
            if source == null_stripped_target {
                return Ok(Ternary::True);
            }
        }

        if Rc::ptr_eq(
            &self_.ref_(arena).relation,
            &self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .comparable_relation,
        ) && !target.ref_(arena).flags().intersects(TypeFlags::Never)
            && self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_simple_type_related_to(
                    target,
                    source,
                    &(*self_.ref_(arena).relation).borrow(),
                    None,
                )?
            || self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_simple_type_related_to(
                    source,
                    target,
                    &(*self_.ref_(arena).relation).borrow(),
                    if report_errors {
                        Some(&mut report_error)
                    } else {
                        None
                    },
                )?
        {
            return Ok(Ternary::True);
        }

        let is_comparing_jsx_attributes =
            get_object_flags(&source.ref_(arena)).intersects(ObjectFlags::JsxAttributes);
        let is_performing_excess_property_checks = !intersection_state
            .intersects(IntersectionState::Target)
            && (self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_object_literal_type(source)
                && get_object_flags(&source.ref_(arena)).intersects(ObjectFlags::FreshLiteral));
        if is_performing_excess_property_checks {
            if Self::has_excess_properties(self_, arena, source, target, report_errors)? {
                if report_errors {
                    Self::report_relation_error(
                        self_,
                        arena,
                        head_message,
                        source,
                        if original_target.ref_(arena).maybe_alias_symbol().is_some() {
                            original_target
                        } else {
                            target
                        },
                    )?;
                }
                return Ok(Ternary::False);
            }
        }

        let is_performing_common_property_checks = !Rc::ptr_eq(
            &self_.ref_(arena).relation,
            &self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .comparable_relation,
        ) && !intersection_state
            .intersects(IntersectionState::Target)
            && source
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::Primitive | TypeFlags::Object | TypeFlags::Intersection)
            && source
                != self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .global_object_type()
            && target
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::Intersection)
            && self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .is_weak_type(target)?
            && (!self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_properties_of_type(source)?
                .len()
                == 0
                || self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .type_has_call_or_construct_signatures(source)?);
        if is_performing_common_property_checks
            && !self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .has_common_properties(source, target, is_comparing_jsx_attributes)?
        {
            if report_errors {
                let source_string = self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                    if original_source.ref_(arena).maybe_alias_symbol().is_some() {
                        original_source
                    } else {
                        source
                    },
                    Option::<Id<Node>>::None,
                    None,
                    None,
                )?;
                let target_string = self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                    if original_target.ref_(arena).maybe_alias_symbol().is_some() {
                        original_target
                    } else {
                        target
                    },
                    Option::<Id<Node>>::None,
                    None,
                    None,
                )?;
                let calls = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_signatures_of_type(source, SignatureKind::Call)?;
                let constructs = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_signatures_of_type(source, SignatureKind::Construct)?;
                if !calls.is_empty()
                    && Self::is_related_to(
                        self_,
                        arena,
                        self_
                            .ref_(arena)
                            .type_checker
                            .ref_(arena)
                            .get_return_type_of_signature(calls[0].clone())?,
                        target,
                        Some(RecursionFlags::Source),
                        Some(false),
                        None,
                        None,
                    )? != Ternary::False
                    || !constructs.is_empty()
                        && Self::is_related_to(
                            self_,
                            arena,
                            self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .get_return_type_of_signature(constructs[0].clone())?,
                            target,
                            Some(RecursionFlags::Source),
                            Some(false),
                            None,
                            None,
                        )? != Ternary::False
                {
                    Self::report_error(
                        self_,
                        arena,
                        Cow::Borrowed(&Diagnostics::Value_of_type_0_has_no_properties_in_common_with_type_1_Did_you_mean_to_call_it),
                        Some(vec![
                            source_string,
                            target_string,
                        ])
                    )?;
                } else {
                    Self::report_error(
                        self_,
                        arena,
                        Cow::Borrowed(&Diagnostics::Type_0_has_no_properties_in_common_with_type_1),
                        Some(vec![source_string, target_string]),
                    )?;
                }
            }
            return Ok(Ternary::False);
        }

        self_
            .ref_(arena)
            .trace_unions_or_intersections_too_large(source, target);

        let mut result = Ternary::False;
        let save_error_info = self_.ref_(arena).capture_error_calculation_state();

        if (source.ref_(arena).flags().intersects(TypeFlags::Union)
            || target.ref_(arena).flags().intersects(TypeFlags::Union))
            && self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_constituent_count(source)
                * self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .get_constituent_count(target)
                < 4
        {
            result = Self::structured_type_related_to(
                self_,
                arena,
                source,
                target,
                report_errors,
                intersection_state | IntersectionState::UnionIntersectionCheck,
            )?;
        } else if source
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
            || target
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::UnionOrIntersection)
        {
            result = Self::recursive_type_related_to(
                self_,
                arena,
                source,
                target,
                report_errors,
                intersection_state | IntersectionState::UnionIntersectionCheck,
                recursion_flags,
            )?;
        }
        if result == Ternary::False
            && !(source.ref_(arena).flags().intersects(TypeFlags::Union))
            && (source
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::StructuredOrInstantiable)
                || target
                    .ref_(arena)
                    .flags()
                    .intersects(TypeFlags::StructuredOrInstantiable))
        {
            result = Self::recursive_type_related_to(
                self_,
                arena,
                source,
                target,
                report_errors,
                intersection_state,
                recursion_flags,
            )?;
            if result != Ternary::False {
                self_.ref_(arena).reset_error_info(save_error_info.clone());
            }
        }
        if result == Ternary::False
            && source
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::Intersection | TypeFlags::TypeParameter)
        {
            let constraint = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_effective_constraint_of_intersection(
                    &if source
                        .ref_(arena)
                        .flags()
                        .intersects(TypeFlags::Intersection)
                    {
                        source
                            .ref_(arena)
                            .as_union_or_intersection_type_interface()
                            .types()
                            .to_owned()
                    } else {
                        vec![source.clone()]
                    },
                    {
                        let intersects = target.ref_(arena).flags().intersects(TypeFlags::Union);
                        intersects
                    },
                )?;
            if let Some(constraint) = constraint {
                if source
                    .ref_(arena)
                    .flags()
                    .intersects(TypeFlags::Intersection)
                    || target.ref_(arena).flags().intersects(TypeFlags::Union)
                {
                    if self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .every_type(constraint, |c| c != source)
                    {
                        result = Self::is_related_to(
                            self_,
                            arena,
                            constraint,
                            target,
                            Some(RecursionFlags::Source),
                            Some(false),
                            None,
                            Some(intersection_state),
                        )?;
                        if result != Ternary::False {
                            self_.ref_(arena).reset_error_info(save_error_info);
                        }
                    }
                }
            }
        }

        if result != Ternary::False
            && !self_.ref_(arena).in_property_check()
            && (target
                .ref_(arena)
                .flags()
                .intersects(TypeFlags::Intersection)
                && (is_performing_excess_property_checks || is_performing_common_property_checks)
                || self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_non_generic_object_type(target)?
                    && !self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .is_array_type(target)
                    && !self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .is_tuple_type(target)
                    && source
                        .ref_(arena)
                        .flags()
                        .intersects(TypeFlags::Intersection)
                    && self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .get_apparent_type(source)?
                        .ref_(arena)
                        .flags()
                        .intersects(TypeFlags::StructuredType)
                    && !some(
                        Some(source.ref_(arena).as_union_or_intersection_type().types()),
                        Some(|&t: &Id<Type>| {
                            get_object_flags(&t.ref_(arena))
                                .intersects(ObjectFlags::NonInferrableType)
                        }),
                    ))
        {
            self_.ref_(arena).set_in_property_check(true);
            result &= Self::recursive_type_related_to(
                self_,
                arena,
                source,
                target,
                report_errors,
                IntersectionState::PropertyCheck,
                recursion_flags,
            )?;
            self_.ref_(arena).set_in_property_check(false);
        }

        Self::report_error_results(
            self_,
            arena,
            report_errors,
            head_message,
            original_source,
            original_target,
            source,
            target,
            result,
            is_comparing_jsx_attributes,
        )?;
        Ok(result)
    }

    pub(super) fn report_error_results(
        self_: Id<Self>,
        arena: &impl HasArena,
        report_errors: bool,
        head_message: Option<Cow<'static, DiagnosticMessage>>,
        original_source: Id<Type>,
        original_target: Id<Type>,
        source: Id<Type>,
        target: Id<Type>,
        result: Ternary,
        is_comparing_jsx_attributes: bool,
    ) -> io::Result<()> {
        if result == Ternary::False && report_errors {
            let source_has_base = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_single_base_for_non_augmenting_subtype(original_source)?
                .is_some();
            let target_has_base = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .get_single_base_for_non_augmenting_subtype(original_target)?
                .is_some();
            let source =
                if original_source.ref_(arena).maybe_alias_symbol().is_some() || source_has_base {
                    original_source
                } else {
                    source
                };
            let target =
                if original_target.ref_(arena).maybe_alias_symbol().is_some() || target_has_base {
                    original_target
                } else {
                    target
                };
            let mut maybe_suppress = self_.ref_(arena).override_next_error_info() > 0;
            if maybe_suppress {
                self_
                    .ref_(arena)
                    .set_override_next_error_info(self_.ref_(arena).override_next_error_info() - 1);
            }
            if source.ref_(arena).flags().intersects(TypeFlags::Object)
                && target.ref_(arena).flags().intersects(TypeFlags::Object)
            {
                let current_error = self_.ref_(arena).maybe_error_info();
                Self::try_elaborate_array_like_errors(self_, arena, source, target, report_errors)?;
                if !match (
                    self_.ref_(arena).maybe_error_info().as_ref(),
                    current_error.as_ref(),
                ) {
                    (None, None) => true,
                    (Some(error_info), Some(current_error)) => {
                        Rc::ptr_eq(error_info, current_error)
                    }
                    _ => false,
                } {
                    maybe_suppress = self_.ref_(arena).maybe_error_info().is_some();
                }
            }
            if source.ref_(arena).flags().intersects(TypeFlags::Object)
                && target.ref_(arena).flags().intersects(TypeFlags::Primitive)
            {
                Self::try_elaborate_errors_for_primitives_and_objects(
                    self_, arena, source, target,
                )?;
            } else if source.ref_(arena).maybe_symbol().is_some()
                && source.ref_(arena).flags().intersects(TypeFlags::Object)
                && self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .global_object_type()
                    == source
            {
                Self::report_error(
                    self_,
                    arena,
                    Cow::Borrowed(&Diagnostics::The_Object_type_is_assignable_to_very_few_other_types_Did_you_mean_to_use_the_any_type_instead),
                    None,
                )?;
            } else if is_comparing_jsx_attributes
                && target
                    .ref_(arena)
                    .flags()
                    .intersects(TypeFlags::Intersection)
            {
                let target_ref = target.ref_(arena);
                let target_types = target_ref.as_union_or_intersection_type_interface().types();
                let intrinsic_attributes =
                    self_.ref_(arena).type_checker.ref_(arena).get_jsx_type(
                        &JsxNames::IntrinsicAttributes,
                        self_.ref_(arena).maybe_error_node(),
                    )?;
                let intrinsic_class_attributes =
                    self_.ref_(arena).type_checker.ref_(arena).get_jsx_type(
                        &JsxNames::IntrinsicClassAttributes,
                        self_.ref_(arena).maybe_error_node(),
                    )?;
                if !self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_error_type(intrinsic_attributes)
                    && !self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .is_error_type(intrinsic_class_attributes)
                    && (contains(Some(target_types), &intrinsic_attributes)
                        || contains(Some(target_types), &intrinsic_class_attributes))
                {
                    return Ok(()) /*result*/;
                }
            } else {
                let error_info = self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .elaborate_never_intersection(
                        self_.ref_(arena).maybe_error_info().as_deref().cloned(),
                        original_target,
                    )?
                    .map(Rc::new);
                *self_.ref_(arena).maybe_error_info_mut() = error_info;
            }
            if head_message.is_none() && maybe_suppress {
                self_
                    .ref_(arena)
                    .set_last_skipped_info(Some((source, target)));
                return Ok(()) /*result*/;
            }
            Self::report_relation_error(self_, arena, head_message, source, target)?;
        }

        Ok(())
    }

    pub(super) fn trace_unions_or_intersections_too_large(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) {
        // if (!tracing) {
        //   return;
        // }

        if source
            .ref_(self)
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
            && target
                .ref_(self)
                .flags()
                .intersects(TypeFlags::UnionOrIntersection)
        {
            if (source
                .ref_(self)
                .as_union_or_intersection_type()
                .object_flags()
                & target
                    .ref_(self)
                    .as_union_or_intersection_type()
                    .object_flags())
            .intersects(ObjectFlags::PrimitiveUnion)
            {
                return;
            }

            let source_size = source
                .ref_(self)
                .as_union_or_intersection_type()
                .types()
                .len();
            let target_size = target
                .ref_(self)
                .as_union_or_intersection_type()
                .types()
                .len();
            if source_size * target_size > 1000000
            /*1E6*/
            {
                // tracing.instant(tracing.Phase.CheckTypes, "traceUnionsOrIntersectionsTooLarge_DepthLimit", {
                //     sourceId: source.id,
                //     sourceSize,
                //     targetId: target.id,
                //     targetSize,
                //     pos: errorNode?.pos,
                //     end: errorNode?.end,
                //  });
            }
        }
    }

    pub(super) fn is_identical_to(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>,
        target: Id<Type>,
        recursion_flags: RecursionFlags,
    ) -> io::Result<Ternary> {
        if source.ref_(arena).flags() != target.ref_(arena).flags() {
            return Ok(Ternary::False);
        }
        if source.ref_(arena).flags().intersects(TypeFlags::Singleton) {
            return Ok(Ternary::True);
        }
        self_
            .ref_(arena)
            .trace_unions_or_intersections_too_large(source, target);
        if source
            .ref_(arena)
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
        {
            let mut result = Self::each_type_related_to_some_type(self_, arena, source, target)?;
            if result != Ternary::False {
                result &= Self::each_type_related_to_some_type(self_, arena, target, source)?;
            }
            return Ok(result);
        }
        Self::recursive_type_related_to(
            self_,
            arena,
            source,
            target,
            false,
            IntersectionState::None,
            recursion_flags,
        )
    }

    pub(super) fn get_type_of_property_in_types(
        &self,
        types: &[Id<Type>],
        name: &str, /*__String*/
    ) -> io::Result<Id<Type>> {
        let append_prop_type = |mut prop_types: Option<Vec<Id<Type>>>,
                                &type_: &Id<Type>,
                                _|
         -> io::Result<Option<Vec<Id<Type>>>> {
            let type_ = self.type_checker.ref_(self).get_apparent_type(type_)?;
            let prop = if type_
                .ref_(self)
                .flags()
                .intersects(TypeFlags::UnionOrIntersection)
            {
                self.type_checker
                    .ref_(self)
                    .get_property_of_union_or_intersection_type(type_, name, None)?
            } else {
                self.type_checker
                    .ref_(self)
                    .get_property_of_object_type(type_, name)?
            };
            let prop_type = prop
                .try_map(|prop| self.type_checker.ref_(self).get_type_of_symbol(prop))?
                .try_or_else(|| -> io::Result<_> {
                    Ok(self
                        .type_checker
                        .ref_(self)
                        .get_applicable_index_info_for_name(type_, name)?
                        .map(|index_info| index_info.ref_(self).type_.clone()))
                })?
                .unwrap_or_else(|| self.type_checker.ref_(self).undefined_type());
            if prop_types.is_none() {
                prop_types = Some(vec![]);
            }
            append(prop_types.as_mut().unwrap(), Some(prop_type));
            Ok(prop_types)
        };
        self.type_checker.ref_(self).get_union_type(
            &try_reduce_left(types, append_prop_type, None, None, None)?.unwrap_or_else(|| vec![]),
            None,
            Option::<Id<Symbol>>::None,
            None,
            None,
        )
    }

    pub(super) fn has_excess_properties(
        self_: Id<Self>,
        arena: &impl HasArena,
        source: Id<Type>, /*FreshObjectLiteralType*/
        target: Id<Type>,
        report_errors: bool,
    ) -> io::Result<bool> {
        if !self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .is_excess_property_check_target(target)
            || !self_.ref_(arena).type_checker.ref_(arena).no_implicit_any
                && get_object_flags(&target.ref_(arena)).intersects(ObjectFlags::JSLiteral)
        {
            return Ok(false);
        }
        let is_comparing_jsx_attributes =
            get_object_flags(&source.ref_(arena)).intersects(ObjectFlags::JsxAttributes);
        if (Rc::ptr_eq(
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
        )) && (self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .is_type_subset_of(
                self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .global_object_type(),
                target,
            )?
            || !is_comparing_jsx_attributes
                && self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_empty_object_type(target)?)
        {
            return Ok(false);
        }
        let mut reduced_target = target;
        let mut check_types: Option<Vec<Id<Type>>> = None;
        if target.ref_(arena).flags().intersects(TypeFlags::Union) {
            reduced_target = self_
                .ref_(arena)
                .type_checker
                .ref_(arena)
                .find_matching_discriminant_type(
                    source,
                    target,
                    |source, target| {
                        Self::is_related_to(self_, arena, source, target, None, None, None, None)
                    },
                    None,
                )?
                .unwrap_or_else(|| {
                    self_
                        .ref_(arena)
                        .type_checker
                        .ref_(arena)
                        .filter_primitives_if_contains_non_primitive(target)
                });
            check_types = Some(
                if reduced_target
                    .ref_(arena)
                    .flags()
                    .intersects(TypeFlags::Union)
                {
                    reduced_target
                        .ref_(arena)
                        .as_union_or_intersection_type_interface()
                        .types()
                        .to_owned()
                } else {
                    vec![reduced_target.clone()]
                },
            );
        }
        for prop in self_
            .ref_(arena)
            .type_checker
            .ref_(arena)
            .get_properties_of_type(source)?
        {
            if self_
                .ref_(arena)
                .should_check_as_excess_property(prop, source.ref_(arena).symbol())
                && !self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_ignored_jsx_property(source, prop)
            {
                if !self_
                    .ref_(arena)
                    .type_checker
                    .ref_(arena)
                    .is_known_property(
                        reduced_target,
                        prop.ref_(arena).escaped_name(),
                        is_comparing_jsx_attributes,
                    )?
                {
                    if report_errors {
                        let error_target = self_.ref_(arena).type_checker.ref_(arena).filter_type(
                            reduced_target,
                            |type_| {
                                self_
                                    .ref_(arena)
                                    .type_checker
                                    .ref_(arena)
                                    .is_excess_property_check_target(type_)
                            },
                        );
                        if self_.ref_(arena).maybe_error_node().is_none() {
                            Debug_.fail(None);
                        }
                        let error_node_present = self_.ref_(arena).maybe_error_node().unwrap();
                        if is_jsx_attributes(&error_node_present.ref_(arena))
                            || is_jsx_opening_like_element(&error_node_present.ref_(arena))
                            || is_jsx_opening_like_element(
                                &error_node_present.ref_(arena).parent().ref_(arena),
                            )
                        {
                            if let Some(prop_value_declaration) = prop
                                .ref_(arena)
                                .maybe_value_declaration()
                                .filter(|prop_value_declaration| {
                                    is_jsx_attribute(&prop_value_declaration.ref_(arena))
                                        && maybe_get_source_file_of_node(
                                            Some(error_node_present),
                                            arena,
                                        ) == maybe_get_source_file_of_node(
                                            Some(
                                                prop_value_declaration
                                                    .ref_(arena)
                                                    .as_jsx_attribute()
                                                    .name,
                                            ),
                                            arena,
                                        )
                                })
                            {
                                self_.ref_(arena).set_error_node(Some(
                                    prop_value_declaration.ref_(arena).as_jsx_attribute().name,
                                ));
                            }
                            let prop_name = self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .symbol_to_string_(
                                    prop,
                                    Option::<Id<Node>>::None,
                                    None,
                                    None,
                                    None,
                                )?;
                            let suggestion_symbol = self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .get_suggested_symbol_for_nonexistent_jsx_attribute(
                                    &*prop_name,
                                    error_target,
                                )?;
                            let suggestion = suggestion_symbol.try_map(|suggestion_symbol| {
                                self_
                                    .ref_(arena)
                                    .type_checker
                                    .ref_(arena)
                                    .symbol_to_string_(
                                        suggestion_symbol,
                                        Option::<Id<Node>>::None,
                                        None,
                                        None,
                                        None,
                                    )
                            })?;
                            if let Some(suggestion) = suggestion {
                                Self::report_error(
                                    self_,
                                    arena,
                                    Cow::Borrowed(&Diagnostics::Property_0_does_not_exist_on_type_1_Did_you_mean_2),
                                    Some(vec![
                                        prop_name,
                                        self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                                            error_target,
                                            Option::<Id<Node>>::None,
                                            None, None,
                                        )?,
                                        suggestion
                                    ])
                                )?;
                            } else {
                                Self::report_error(
                                    self_,
                                    arena,
                                    Cow::Borrowed(
                                        &Diagnostics::Property_0_does_not_exist_on_type_1,
                                    ),
                                    Some(vec![
                                        prop_name,
                                        self_
                                            .ref_(arena)
                                            .type_checker
                                            .ref_(arena)
                                            .type_to_string_(
                                                error_target,
                                                Option::<Id<Node>>::None,
                                                None,
                                                None,
                                            )?,
                                    ]),
                                )?;
                            }
                        } else {
                            let object_literal_declaration =
                                if let Some(symbol) = source.ref_(arena).maybe_symbol() {
                                    if let Some(declarations) =
                                        &*symbol.ref_(arena).maybe_declarations()
                                    {
                                        first_or_undefined(declarations).map(Clone::clone)
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                };
                            let mut suggestion: Option<String> = None;
                            if let Some(prop_value_declaration) = prop
                                .ref_(arena)
                                .maybe_value_declaration()
                                .filter(|&prop_value_declaration| {
                                    find_ancestor(
                                        Some(prop_value_declaration),
                                        |d| object_literal_declaration == Some(d),
                                        arena,
                                    )
                                    .is_some()
                                        && maybe_get_source_file_of_node(
                                            object_literal_declaration,
                                            arena,
                                        ) == maybe_get_source_file_of_node(
                                            Some(error_node_present),
                                            arena,
                                        )
                                })
                            {
                                let prop_declaration = prop_value_declaration;
                                Debug_.assert_node(
                                    Some(prop_declaration),
                                    Some(|node: Id<Node>| {
                                        is_object_literal_element_like(&node.ref_(arena))
                                    }),
                                    None,
                                );

                                self_.ref_(arena).set_error_node(Some(prop_declaration));

                                let name =
                                    prop_declaration.ref_(arena).as_named_declaration().name();
                                if is_identifier(&name.ref_(arena)) {
                                    suggestion = self_
                                        .ref_(arena)
                                        .type_checker
                                        .ref_(arena)
                                        .get_suggestion_for_nonexistent_property(
                                            name,
                                            error_target,
                                        )?;
                                }
                            }
                            if let Some(suggestion) = suggestion {
                                Self::report_error(
                                    self_,
                                    arena,
                                    Cow::Borrowed(&Diagnostics::Object_literal_may_only_specify_known_properties_but_0_does_not_exist_in_type_1_Did_you_mean_to_write_2),
                                    Some(vec![
                                        self_.ref_(arena).type_checker.ref_(arena).symbol_to_string_(
                                            prop,
                                            Option::<Id<Node>>::None,
                                            None, None, None,
                                        )?,
                                        self_.ref_(arena).type_checker.ref_(arena).type_to_string_(
                                            error_target,
                                            Option::<Id<Node>>::None,
                                            None, None,
                                        )?,
                                        suggestion
                                    ])
                                )?;
                            } else {
                                Self::report_error(
                                    self_,
                                    arena,
                                    Cow::Borrowed(&Diagnostics::Object_literal_may_only_specify_known_properties_and_0_does_not_exist_in_type_1),
                                    Some(
                                        vec![
                                            self_.ref_(arena).type_checker.ref_(arena).symbol_to_string_(prop, Option::<Id<Node>>::None, None, None, None)?,
                                            self_.ref_(arena).type_checker.ref_(arena).type_to_string_(error_target, Option::<Id<Node>>::None, None, None)?
                                        ]
                                    )
                                )?;
                            }
                        }
                    }
                    return Ok(true);
                }
                if matches!(
                    check_types.as_ref(),
                    Some(check_types) if Self::is_related_to(
                        self_,
                        arena,
                        self_.ref_(arena).type_checker.ref_(arena).get_type_of_symbol(prop)?,
                        self_.ref_(arena).get_type_of_property_in_types(check_types, prop.ref_(arena).escaped_name())?,
                        Some(RecursionFlags::Both),
                        Some(report_errors),
                        None, None,
                    )? == Ternary::False
                ) {
                    if report_errors {
                        self_.ref_(arena).report_incompatible_error(
                            &Diagnostics::Types_of_property_0_are_incompatible,
                            Some(vec![self_
                                .ref_(arena)
                                .type_checker
                                .ref_(arena)
                                .symbol_to_string_(
                                    prop,
                                    Option::<Id<Node>>::None,
                                    None,
                                    None,
                                    None,
                                )?]),
                        );
                    }
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }
}

impl_has_arena!(CheckTypeRelatedTo);

#[derive(Clone)]
pub(super) struct ErrorCalculationState {
    pub error_info: Option<Rc<DiagnosticMessageChain>>,
    pub last_skipped_info: Option<(Id<Type>, Id<Type>)>,
    pub incompatible_stack: Vec<(&'static DiagnosticMessage, Option<Vec<String>>)>,
    pub override_next_error_info: usize,
    pub related_info: Option<Vec<DiagnosticRelatedInformation>>,
}
