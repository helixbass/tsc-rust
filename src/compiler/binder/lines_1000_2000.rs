#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{init_flow_node, ActiveLabel, BinderType, ContainerFlags, ModuleInstanceState};
use crate::{
    append, concatenate, get_host_signature_from_jsdoc, has_syntactic_modifier, is_ambient_module,
    is_binary_expression, is_dotted_name, is_export_assignment, is_export_declaration,
    is_external_module, is_for_in_or_of_statement, is_identifier,
    is_logical_or_coalescing_assignment_operator, is_module_augmentation_external, is_module_block,
    is_object_literal_or_class_expression_method_or_accessor, is_omitted_expression,
    is_optional_chain, is_optional_chain_root, is_outermost_optional_chain,
    is_parenthesized_expression, is_prefix_unary_expression, is_push_or_unshift_identifier,
    is_source_file, is_static, set_parent_recursive, skip_parentheses, try_cast, try_parse_pattern,
    unused_label_is_error, DiagnosticMessage, Diagnostics, FlowAssignment, FlowCall, FlowFlags,
    FlowNode, FlowNodeBase, FlowSwitchClause, HasInitializerInterface, ModifierFlags, NodeFlags,
    PatternAmbientModule, StringOrNodeArray, StringOrPattern, Symbol, SyntaxKind, __String,
    create_symbol_table, for_each_bool, is_binding_pattern, is_block_or_catch_scoped,
    is_class_static_block_declaration, is_function_like, set_parent, InternalSymbolName,
    NamedDeclarationInterface, Node, NodeInterface, SymbolFlags, SymbolInterface,
};

impl BinderType {
    pub(super) fn create_flow_switch_clause(
        &self,
        antecedent: Rc<FlowNode>,
        switch_statement: &Node, /*SwitchStatement*/
        clause_start: usize,
        clause_end: usize,
    ) -> Rc<FlowNode> {
        self.set_flow_node_referenced(&antecedent);
        Rc::new(init_flow_node(
            FlowSwitchClause::new(
                FlowFlags::SwitchClause,
                antecedent,
                switch_statement.node_wrapper(),
                clause_start,
                clause_end,
            )
            .into(),
        ))
    }

    pub(super) fn create_flow_mutation(
        &self,
        flags: FlowFlags,
        antecedent: Rc<FlowNode>,
        node: &Node, /*Expression | VariableDeclaration | ArrayBindingElement*/
    ) -> Rc<FlowNode> {
        self.set_flow_node_referenced(&antecedent);
        let result: Rc<FlowNode> = Rc::new(init_flow_node(
            FlowAssignment::new(flags, antecedent, node.node_wrapper()).into(),
        ));
        if let Some(current_exception_target) = self.maybe_current_exception_target() {
            self.add_antecedent(&current_exception_target, result.clone());
        }
        result
    }

    pub(super) fn create_flow_call(
        &self,
        antecedent: Rc<FlowNode>,
        node: &Node, /*CallExpression*/
    ) -> Rc<FlowNode> {
        self.set_flow_node_referenced(&antecedent);
        Rc::new(init_flow_node(
            FlowCall::new(FlowFlags::Call, antecedent, node.node_wrapper()).into(),
        ))
    }

    pub(super) fn finish_flow_label(&self, flow: Rc<FlowNode /*FlowLabel*/>) -> Rc<FlowNode> {
        let antecedents = flow.as_flow_label().maybe_antecedents();
        let antecedents = antecedents.as_ref();
        if antecedents.is_none() {
            return self.unreachable_flow();
        }
        let antecedents = antecedents.unwrap();
        if antecedents.len() == 1 {
            return antecedents[0].clone();
        }
        flow.clone()
    }

    pub(super) fn is_statement_condition(&self, node: &Node) -> bool {
        let parent = node.parent();
        match parent.kind() {
            SyntaxKind::IfStatement | SyntaxKind::WhileStatement | SyntaxKind::DoStatement => {
                ptr::eq(&*parent.as_has_expression().expression(), node)
            }
            SyntaxKind::ForStatement | SyntaxKind::ConditionalExpression => {
                matches!(parent.as_has_condition().maybe_condition(), Some(condition) if ptr::eq(&*condition, node))
            }
            _ => false,
        }
    }

    pub(super) fn is_logical_expression(&self, node: &Node) -> bool {
        let mut node = node.node_wrapper();
        loop {
            if node.kind() == SyntaxKind::ParenthesizedExpression {
                node = node.as_parenthesized_expression().expression.clone();
            } else if node.kind() == SyntaxKind::PrefixUnaryExpression
                && node.as_prefix_unary_expression().operator == SyntaxKind::ExclamationToken
            {
                node = node.as_prefix_unary_expression().operand.clone();
            } else {
                return node.kind() == SyntaxKind::BinaryExpression
                    && matches!(
                        node.as_binary_expression().operator_token.kind(),
                        SyntaxKind::AmpersandAmpersandToken
                            | SyntaxKind::BarBarToken
                            | SyntaxKind::QuestionQuestionToken
                    );
            }
        }
    }

    pub(super) fn is_logical_assignment_expression(&self, node: &Node) -> bool {
        let node = skip_parentheses(node, None);
        is_binary_expression(&node)
            && is_logical_or_coalescing_assignment_operator(
                node.as_binary_expression().operator_token.kind(),
            )
    }

    pub(super) fn is_top_level_logical_expression(&self, node: &Node) -> bool {
        let mut node = node.node_wrapper();
        while is_parenthesized_expression(&node.parent())
            || is_prefix_unary_expression(&node.parent())
                && node.parent().as_prefix_unary_expression().operator
                    == SyntaxKind::ExclamationToken
        {
            node = node.parent();
        }
        !self.is_statement_condition(&node)
            && !self.is_logical_assignment_expression(&node.parent())
            && !self.is_logical_expression(&node.parent())
            && !(is_optional_chain(&node.parent())
                && Rc::ptr_eq(&node.parent().as_has_expression().expression(), &node))
    }

    pub(super) fn do_with_conditional_branches<TArgument, TAction: FnMut(TArgument)>(
        &self,
        mut action: TAction,
        value: TArgument,
        true_target: Rc<FlowNode /*FlowLabel*/>,
        false_target: Rc<FlowNode /*FlowLabel*/>,
    ) {
        let saved_true_target = self.maybe_current_true_target();
        let saved_false_target = self.maybe_current_false_target();
        self.set_current_true_target(Some(true_target));
        self.set_current_false_target(Some(false_target));
        action(value);
        self.set_current_true_target(saved_true_target);
        self.set_current_false_target(saved_false_target);
    }

    pub(super) fn bind_condition<TNode: Borrow<Node> + Clone>(
        &self,
        node: Option<TNode>,
        true_target: Rc<FlowNode /*FlowLabel*/>,
        false_target: Rc<FlowNode /*FlowLabel*/>,
    ) {
        self.do_with_conditional_branches(
            |node| self.bind(node),
            node.clone(),
            true_target.clone(),
            false_target.clone(),
        );
        if match node.as_ref() {
            None => true,
            Some(node) => {
                let node = node.borrow();
                !self.is_logical_assignment_expression(node)
                    && !self.is_logical_expression(node)
                    && !(is_optional_chain(node) && is_outermost_optional_chain(node))
            }
        } {
            self.add_antecedent(
                &true_target,
                self.create_flow_condition(
                    FlowFlags::TrueCondition,
                    self.current_flow(),
                    node.as_ref().map(|node| node.borrow().node_wrapper()),
                ),
            );
            self.add_antecedent(
                &false_target,
                self.create_flow_condition(
                    FlowFlags::FalseCondition,
                    self.current_flow(),
                    node.as_ref().map(|node| node.borrow().node_wrapper()),
                ),
            );
        }
    }

    pub(super) fn bind_iterative_statement(
        &self,
        node: &Node, /*Statement*/
        break_target: Rc<FlowNode /*FlowLabel*/>,
        continue_target: Rc<FlowNode /*FlowLabel*/>,
    ) {
        let save_break_target = self.maybe_current_break_target();
        let save_continue_target = self.maybe_current_continue_target();
        self.set_current_break_target(Some(break_target));
        self.set_current_continue_target(Some(continue_target));
        self.bind(Some(node));
        self.set_current_break_target(save_break_target);
        self.set_current_continue_target(save_continue_target);
    }

    pub(super) fn set_continue_target(
        &self,
        node: &Node,
        target: Rc<FlowNode /*FlowLabel*/>,
    ) -> Rc<FlowNode> {
        let mut node = node.node_wrapper();
        let mut label = self.maybe_active_label_list();
        while label.is_some() && node.parent().kind() == SyntaxKind::LabeledStatement {
            let label_present = label.unwrap();
            label_present.set_continue_target(Some(target.clone()));
            label = label_present.next.clone();
            node = node.parent();
        }
        target
    }

    pub(super) fn bind_while_statement(&self, node: &Node /*WhileStatement*/) {
        let node_as_while_statement = node.as_while_statement();
        let pre_while_label = self.set_continue_target(node, self.create_loop_label());
        let pre_body_label = self.create_branch_label();
        let post_while_label = self.create_branch_label();
        self.add_antecedent(&pre_while_label, self.current_flow());
        self.set_current_flow(Some(pre_while_label.clone()));
        self.bind_condition(
            Some(&*node_as_while_statement.expression),
            pre_body_label.clone(),
            post_while_label.clone(),
        );
        self.set_current_flow(Some(self.finish_flow_label(pre_body_label)));
        self.bind_iterative_statement(
            &node_as_while_statement.statement,
            post_while_label.clone(),
            pre_while_label.clone(),
        );
        self.add_antecedent(&pre_while_label, self.current_flow());
        self.set_current_flow(Some(self.finish_flow_label(post_while_label)));
    }

    pub(super) fn bind_do_statement(&self, node: &Node /*DoStatement*/) {
        let node_as_do_statement = node.as_do_statement();
        let pre_do_label = self.create_loop_label();
        let pre_condition_label = self.set_continue_target(node, self.create_branch_label());
        let post_do_label = self.create_branch_label();
        self.add_antecedent(&pre_do_label, self.current_flow());
        self.set_current_flow(Some(pre_do_label.clone()));
        self.bind_iterative_statement(
            &node_as_do_statement.statement,
            post_do_label.clone(),
            pre_condition_label.clone(),
        );
        self.add_antecedent(&pre_condition_label, self.current_flow());
        self.set_current_flow(Some(self.finish_flow_label(pre_condition_label)));
        self.bind_condition(
            Some(&*node_as_do_statement.expression),
            pre_do_label,
            post_do_label.clone(),
        );
        self.set_current_flow(Some(self.finish_flow_label(post_do_label)));
    }

    pub(super) fn bind_for_statement(&self, node: &Node /*ForStatement*/) {
        let node_as_for_statement = node.as_for_statement();
        let pre_loop_label = self.set_continue_target(node, self.create_loop_label());
        let pre_body_label = self.create_branch_label();
        let post_loop_label = self.create_branch_label();
        self.bind(node_as_for_statement.initializer.clone());
        self.add_antecedent(&pre_loop_label, self.current_flow());
        self.set_current_flow(Some(pre_loop_label.clone()));
        self.bind_condition(
            node_as_for_statement.condition.clone(),
            pre_body_label.clone(),
            post_loop_label.clone(),
        );
        self.set_current_flow(Some(self.finish_flow_label(pre_body_label)));
        self.bind_iterative_statement(
            &node_as_for_statement.statement,
            post_loop_label.clone(),
            pre_loop_label.clone(),
        );
        self.bind(node_as_for_statement.incrementor.clone());
        self.add_antecedent(&pre_loop_label, self.current_flow());
        self.set_current_flow(Some(self.finish_flow_label(post_loop_label)));
    }

    pub(super) fn bind_for_in_or_for_of_statement(&self, node: &Node /*ForInOrOfStatement*/) {
        let pre_loop_label = self.set_continue_target(node, self.create_loop_label());
        let post_loop_label = self.create_branch_label();
        self.bind(Some(&*node.as_has_expression().expression()));
        self.add_antecedent(&pre_loop_label, self.current_flow());
        self.set_current_flow(Some(pre_loop_label.clone()));
        if node.kind() == SyntaxKind::ForOfStatement {
            self.bind(node.as_for_of_statement().await_modifier.clone());
        }
        self.add_antecedent(&post_loop_label, self.current_flow());
        let node_initializer = node.as_has_initializer().maybe_initializer().unwrap();
        self.bind(Some(&*node_initializer));
        if node_initializer.kind() != SyntaxKind::VariableDeclarationList {
            self.bind_assignment_target_flow(&node_initializer);
        }
        self.bind_iterative_statement(
            &node.as_has_statement().statement(),
            post_loop_label.clone(),
            pre_loop_label.clone(),
        );
        self.add_antecedent(&pre_loop_label, self.current_flow());
        self.set_current_flow(Some(self.finish_flow_label(post_loop_label)));
    }

    pub(super) fn bind_if_statement(&self, node: &Node /*IfStatement*/) {
        let node_as_if_statement = node.as_if_statement();
        let then_label = self.create_branch_label();
        let else_label = self.create_branch_label();
        let post_if_label = self.create_branch_label();
        self.bind_condition(
            Some(&*node_as_if_statement.expression),
            then_label.clone(),
            else_label.clone(),
        );
        self.set_current_flow(Some(self.finish_flow_label(then_label)));
        self.bind(Some(&*node_as_if_statement.then_statement));
        self.add_antecedent(&post_if_label, self.current_flow());
        self.set_current_flow(Some(self.finish_flow_label(else_label)));
        self.bind(node_as_if_statement.else_statement.clone());
        self.add_antecedent(&post_if_label, self.current_flow());
        self.set_current_flow(Some(self.finish_flow_label(post_if_label)));
    }

    pub(super) fn bind_return_or_throw(&self, node: &Node) {
        self.bind(Some(node.as_has_expression().expression()));
        if node.kind() == SyntaxKind::ReturnStatement {
            self.set_has_explicit_return(Some(true));
            if let Some(current_return_target) = self.maybe_current_return_target() {
                self.add_antecedent(&current_return_target, self.current_flow());
            }
        }
        self.set_current_flow(Some(self.unreachable_flow()));
    }

    pub(super) fn find_active_label(&self, name: &__String) -> Option<Rc<ActiveLabel>> {
        let mut label = self.maybe_active_label_list();
        while let Some(label_present) = label {
            if &label_present.name == name {
                return Some(label_present);
            }
            label = label_present.next.clone();
        }
        None
    }

    pub(super) fn bind_break_or_continue_flow(
        &self,
        node: &Node, /*BreakOrContinueStatement*/
        break_target: Option<Rc<FlowNode /*FlowLabel*/>>,
        continue_target: Option<Rc<FlowNode /*FlowLabel*/>>,
    ) {
        let flow_label = if node.kind() == SyntaxKind::BreakStatement {
            break_target
        } else {
            continue_target
        };
        if let Some(flow_label) = flow_label {
            self.add_antecedent(&flow_label, self.current_flow());
            self.set_current_flow(Some(self.unreachable_flow()));
        }
    }

    pub(super) fn bind_break_or_continue_statement(
        &self,
        node: &Node, /*BreakOrContinueStatement*/
    ) {
        let node_as_has_label = node.as_has_label();
        if let Some(node_label) = node_as_has_label.maybe_label() {
            let active_label = self.find_active_label(&node_label.as_identifier().escaped_text);
            if let Some(active_label) = active_label {
                active_label.set_referenced(true);
                self.bind_break_or_continue_flow(
                    node,
                    Some(active_label.break_target()),
                    active_label.maybe_continue_target(),
                );
            }
        } else {
            self.bind_break_or_continue_flow(
                node,
                self.maybe_current_break_target(),
                self.maybe_current_continue_target(),
            );
        }
    }

    pub(super) fn bind_try_statement(&self, node: &Node /*TryStatement*/) {
        let save_return_target = self.maybe_current_return_target();
        let save_exception_target = self.maybe_current_exception_target();
        let normal_exit_label = self.create_branch_label();
        let return_label = self.create_branch_label();
        let mut exception_label = self.create_branch_label();
        let node_as_try_statement = node.as_try_statement();
        if node_as_try_statement.finally_block.is_some() {
            self.set_current_return_target(Some(return_label.clone()));
        }
        self.add_antecedent(&exception_label, self.current_flow());
        self.set_current_exception_target(Some(exception_label.clone()));
        self.bind(Some(&*node_as_try_statement.try_block));
        self.add_antecedent(&normal_exit_label, self.current_flow());
        if node_as_try_statement.catch_clause.is_some() {
            self.set_current_flow(Some(self.finish_flow_label(exception_label.clone())));
            exception_label = self.create_branch_label();
            self.add_antecedent(&exception_label, self.current_flow());
            self.set_current_exception_target(Some(exception_label.clone()));
            self.bind(node_as_try_statement.catch_clause.clone());
            self.add_antecedent(&normal_exit_label, self.current_flow());
        }
        self.set_current_return_target(save_return_target);
        self.set_current_exception_target(save_exception_target);
        if node_as_try_statement.finally_block.is_some() {
            let finally_label = self.create_branch_label();
            finally_label
                .as_flow_label()
                .set_antecedents(Some(concatenate(
                    concatenate(
                        normal_exit_label
                            .as_flow_label()
                            .maybe_antecedents()
                            .clone()
                            .unwrap_or_else(|| vec![]),
                        exception_label
                            .as_flow_label()
                            .maybe_antecedents()
                            .clone()
                            .unwrap_or_else(|| vec![]),
                    ),
                    return_label
                        .as_flow_label()
                        .maybe_antecedents()
                        .clone()
                        .unwrap_or_else(|| vec![]),
                )));
            self.set_current_flow(Some(finally_label.clone()));
            self.bind(node_as_try_statement.finally_block.clone());
            if self
                .current_flow()
                .flags()
                .intersects(FlowFlags::Unreachable)
            {
                self.set_current_flow(Some(self.unreachable_flow()));
            } else {
                if let Some(current_return_target) = self.maybe_current_return_target() {
                    if let Some(return_label_antecedents) =
                        return_label.as_flow_label().maybe_antecedents().clone()
                    {
                        self.add_antecedent(
                            &current_return_target,
                            self.create_reduce_label(
                                finally_label.clone(),
                                return_label_antecedents,
                                self.current_flow(),
                            ),
                        );
                    }
                }
                if let Some(current_exception_target) = self.maybe_current_exception_target() {
                    if let Some(exception_label_antecedents) =
                        exception_label.as_flow_label().maybe_antecedents().clone()
                    {
                        self.add_antecedent(
                            &current_exception_target,
                            self.create_reduce_label(
                                finally_label.clone(),
                                exception_label_antecedents,
                                self.current_flow(),
                            ),
                        );
                    }
                }
                self.set_current_flow(Some(
                    match normal_exit_label
                        .as_flow_label()
                        .maybe_antecedents()
                        .clone()
                    {
                        Some(normal_exit_label_antecedents) => self.create_reduce_label(
                            finally_label,
                            normal_exit_label_antecedents,
                            self.current_flow(),
                        ),
                        None => self.unreachable_flow(),
                    },
                ));
            }
        } else {
            self.set_current_flow(Some(self.finish_flow_label(normal_exit_label)));
        }
    }

    pub(super) fn bind_switch_statement(&self, node: &Node /*SwitchStatement*/) {
        let node_as_switch_statement = node.as_switch_statement();
        let post_switch_label = self.create_branch_label();
        self.bind(Some(&*node_as_switch_statement.expression));
        let save_break_target = self.maybe_current_break_target();
        let save_pre_switch_case_flow = self.maybe_pre_switch_case_flow();
        self.set_current_break_target(Some(post_switch_label.clone()));
        self.set_pre_switch_case_flow(Some(self.current_flow()));
        self.bind(Some(&*node_as_switch_statement.case_block));
        self.add_antecedent(&post_switch_label, self.current_flow());
        let has_default = for_each_bool(
            &node_as_switch_statement.case_block.as_case_block().clauses,
            |c, _| c.kind() == SyntaxKind::DefaultClause,
        );
        node_as_switch_statement.set_possibly_exhaustive(Some(
            !has_default
                && post_switch_label
                    .as_flow_label()
                    .maybe_antecedents()
                    .is_none(),
        ));
        if !has_default {
            self.add_antecedent(
                &post_switch_label,
                self.create_flow_switch_clause(self.pre_switch_case_flow(), node, 0, 0),
            );
        }
        self.set_current_break_target(save_break_target);
        self.set_pre_switch_case_flow(save_pre_switch_case_flow);
        self.set_current_flow(Some(self.finish_flow_label(post_switch_label)));
    }

    pub(super) fn bind_case_block(&self, node: &Node /*CaseBlock*/) {
        let node_as_case_block = node.as_case_block();
        let clauses = &*node_as_case_block.clauses;
        let is_narrowing_switch =
            self.is_narrowing_expression(&node.parent().as_switch_statement().expression);
        let mut fallthrough_flow = self.unreachable_flow();
        let mut i = 0;
        while i < clauses.len() {
            let clause_start = i;
            while clauses[i].as_case_clause().statements.is_empty() && i + 1 < clauses.len() {
                self.bind(Some(&*clauses[i]));
                i += 1;
            }
            let pre_case_label = self.create_branch_label();
            self.add_antecedent(
                &pre_case_label,
                if is_narrowing_switch {
                    self.create_flow_switch_clause(
                        self.pre_switch_case_flow(),
                        &node.parent(),
                        clause_start,
                        i + 1,
                    )
                } else {
                    self.pre_switch_case_flow()
                },
            );
            self.add_antecedent(&pre_case_label, fallthrough_flow);
            self.set_current_flow(Some(self.finish_flow_label(pre_case_label.clone())));
            let clause = &clauses[i];
            self.bind(Some(&**clause));
            fallthrough_flow = self.current_flow();
            if !self
                .current_flow()
                .flags()
                .intersects(FlowFlags::Unreachable)
                && i != clauses.len() - 1
                && matches!(self.options().no_fallthrough_cases_in_switch, Some(true))
            {
                clause
                    .as_case_clause()
                    .set_fallthrough_flow_node(Some(self.current_flow()));
            }
            i += 1;
        }
    }

    pub(super) fn bind_case_clause(&self, node: &Node /*CaseClause*/) {
        let save_current_flow = self.maybe_current_flow();
        self.set_current_flow(Some(self.pre_switch_case_flow()));
        let node_as_case_clause = node.as_case_clause();
        self.bind(Some(&*node_as_case_clause.expression));
        self.set_current_flow(save_current_flow);
        self.bind_each(Some(&node_as_case_clause.statements));
    }

    pub(super) fn bind_expression_statement(&self, node: &Node /*ExpressionStatement*/) {
        let node_as_expression_statement = node.as_expression_statement();
        self.bind(Some(&*node_as_expression_statement.expression));
        self.maybe_bind_expression_flow_if_call(&node_as_expression_statement.expression);
    }

    pub(super) fn maybe_bind_expression_flow_if_call(&self, node: &Node /*Expression*/) {
        if node.kind() == SyntaxKind::CallExpression {
            let call = node;
            let call_as_call_expression = call.as_call_expression();
            if call_as_call_expression.expression.kind() != SyntaxKind::SuperKeyword
                && is_dotted_name(&call_as_call_expression.expression)
            {
                self.set_current_flow(Some(self.create_flow_call(self.current_flow(), call)));
            }
        }
    }

    pub(super) fn bind_labeled_statement(&self, node: &Node /*LabeledStatement*/) {
        let post_statement_label = self.create_branch_label();
        let node_as_labeled_statement = node.as_labeled_statement();
        self.set_active_label_list(Some(Rc::new(ActiveLabel::new(
            self.maybe_active_label_list(),
            node_as_labeled_statement
                .label
                .as_identifier()
                .escaped_text
                .clone(),
            post_statement_label.clone(),
            None,
            false,
        ))));
        self.bind(Some(&*node_as_labeled_statement.label));
        self.bind(Some(&*node_as_labeled_statement.statement));
        if !self.active_label_list().referenced()
            && !matches!(self.options().allow_unused_labels, Some(true))
        {
            self.error_or_suggestion_on_node(
                unused_label_is_error(&self.options()),
                &node_as_labeled_statement.label,
                &Diagnostics::Unused_label,
            );
        }
        self.set_active_label_list(self.active_label_list().next());
        self.add_antecedent(&post_statement_label, self.current_flow());
        self.set_current_flow(Some(self.finish_flow_label(post_statement_label)));
    }

    pub(super) fn bind_destructuring_target_flow(&self, node: &Node /*Expression*/) {
        if node.kind() == SyntaxKind::BinaryExpression
            && node.as_binary_expression().operator_token.kind() == SyntaxKind::EqualsToken
        {
            self.bind_assignment_target_flow(&node.as_binary_expression().left);
        } else {
            self.bind_assignment_target_flow(node);
        }
    }

    pub(super) fn bind_assignment_target_flow(&self, node: &Node /*Expression*/) {
        if self.is_narrowable_reference(node) {
            self.set_current_flow(Some(self.create_flow_mutation(
                FlowFlags::Assignment,
                self.current_flow(),
                node,
            )));
        } else if node.kind() == SyntaxKind::ArrayLiteralExpression {
            for e in &*node.as_array_literal_expression().elements {
                if e.kind() == SyntaxKind::SpreadElement {
                    self.bind_assignment_target_flow(&e.as_spread_element().expression);
                } else {
                    self.bind_destructuring_target_flow(e);
                }
            }
        } else if node.kind() == SyntaxKind::ObjectLiteralExpression {
            for p in &*node.as_object_literal_expression().properties {
                if p.kind() == SyntaxKind::PropertyAssignment {
                    self.bind_destructuring_target_flow(
                        &p.as_property_assignment().maybe_initializer().unwrap(),
                    );
                } else if p.kind() == SyntaxKind::ShorthandPropertyAssignment {
                    self.bind_assignment_target_flow(&p.as_shorthand_property_assignment().name());
                } else if p.kind() == SyntaxKind::SpreadAssignment {
                    self.bind_assignment_target_flow(&p.as_spread_assignment().expression);
                }
            }
        }
    }

    pub(super) fn bind_logical_like_expression(
        &self,
        node: &Node, /*BinaryExpression*/
        true_target: Rc<FlowNode /*FlowLabel*/>,
        false_target: Rc<FlowNode /*FlowLabel*/>,
    ) {
        let pre_right_label = self.create_branch_label();
        let node_as_binary_expression = node.as_binary_expression();
        if matches!(
            node_as_binary_expression.operator_token.kind(),
            SyntaxKind::AmpersandAmpersandToken | SyntaxKind::AmpersandAmpersandEqualsToken
        ) {
            self.bind_condition(
                Some(&*node_as_binary_expression.left),
                pre_right_label.clone(),
                false_target.clone(),
            );
        } else {
            self.bind_condition(
                Some(&*node_as_binary_expression.left),
                true_target.clone(),
                pre_right_label.clone(),
            );
        }
        self.set_current_flow(Some(self.finish_flow_label(pre_right_label)));
        self.bind(Some(&*node_as_binary_expression.operator_token));

        if is_logical_or_coalescing_assignment_operator(
            node_as_binary_expression.operator_token.kind(),
        ) {
            self.do_with_conditional_branches(
                |node| self.bind(Some(&**node)),
                &node_as_binary_expression.right,
                true_target.clone(),
                false_target.clone(),
            );
            self.bind_assignment_target_flow(&node_as_binary_expression.left);

            self.add_antecedent(
                &true_target,
                self.create_flow_condition(
                    FlowFlags::TrueCondition,
                    self.current_flow(),
                    Some(node),
                ),
            );
            self.add_antecedent(
                &false_target,
                self.create_flow_condition(
                    FlowFlags::FalseCondition,
                    self.current_flow(),
                    Some(node),
                ),
            );
        } else {
            self.bind_condition(
                Some(&*node_as_binary_expression.right),
                true_target,
                false_target,
            );
        }
    }

    pub(super) fn bind_prefix_unary_expression_flow(
        &self,
        node: &Node, /*PrefixUnaryExpression*/
    ) {
        let node_as_prefix_unary_expression = node.as_prefix_unary_expression();
        if node_as_prefix_unary_expression.operator == SyntaxKind::ExclamationToken {
            let save_true_target = self.maybe_current_true_target();
            self.set_current_true_target(self.maybe_current_false_target());
            self.set_current_false_target(save_true_target.clone());
            self.bind_each_child(node);
            self.set_current_false_target(self.maybe_current_true_target());
            self.set_current_true_target(save_true_target);
        } else {
            self.bind_each_child(node);
            if matches!(
                node_as_prefix_unary_expression.operator,
                SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
            ) {
                self.bind_assignment_target_flow(&node_as_prefix_unary_expression.operand);
            }
        }
    }

    pub(super) fn bind_postfix_unary_expression_flow(
        &self,
        node: &Node, /*PostfixUnaryExpression*/
    ) {
        let node_as_postfix_unary_expression = node.as_postfix_unary_expression();
        self.bind_each_child(node);
        if matches!(
            node_as_postfix_unary_expression.operator,
            SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
        ) {
            self.bind_assignment_target_flow(&node_as_postfix_unary_expression.operand);
        }
    }

    pub(super) fn bind_destructuring_assignment_flow(
        &self,
        node: &Node, /*DestructuringAssignment*/
    ) {
        let node_as_binary_expression = node.as_binary_expression();
        if self.in_assignment_pattern() {
            self.set_in_assignment_pattern(false);
            self.bind(Some(&*node_as_binary_expression.operator_token));
            self.bind(Some(&*node_as_binary_expression.right));
            self.set_in_assignment_pattern(true);
            self.bind(Some(&*node_as_binary_expression.left));
        } else {
            self.set_in_assignment_pattern(true);
            self.bind(Some(&*node_as_binary_expression.left));
            self.set_in_assignment_pattern(false);
            self.bind(Some(&*node_as_binary_expression.operator_token));
            self.bind(Some(&*node_as_binary_expression.right));
        }
        self.bind_assignment_target_flow(&node_as_binary_expression.left);
    }

    pub(super) fn bind_binary_expression_flow(&self, node: &Node /*BinaryExpression*/) {
        unimplemented!()
    }

    pub(super) fn bind_delete_expression_flow(&self, node: &Node /*DeleteExpression*/) {
        self.bind_each_child(node);
        let node_as_delete_expression = node.as_delete_expression();
        if node_as_delete_expression.expression.kind() == SyntaxKind::PropertyAccessExpression {
            self.bind_assignment_target_flow(&node_as_delete_expression.expression);
        }
    }

    pub(super) fn bind_conditional_expression_flow(
        &self,
        node: &Node, /*ConditionalExpression*/
    ) {
        let true_label = self.create_branch_label();
        let false_label = self.create_branch_label();
        let post_expression_label = self.create_branch_label();
        let node_as_conditional_expression = node.as_conditional_expression();
        self.bind_condition(
            Some(&*node_as_conditional_expression.condition),
            true_label.clone(),
            false_label.clone(),
        );
        self.set_current_flow(Some(self.finish_flow_label(true_label)));
        self.bind(Some(&*node_as_conditional_expression.question_token));
        self.bind(Some(&*node_as_conditional_expression.when_true));
        self.add_antecedent(&post_expression_label, self.current_flow());
        self.set_current_flow(Some(self.finish_flow_label(false_label)));
        self.bind(Some(&*node_as_conditional_expression.colon_token));
        self.bind(Some(&*node_as_conditional_expression.when_false));
        self.add_antecedent(&post_expression_label, self.current_flow());
        self.set_current_flow(Some(self.finish_flow_label(post_expression_label)));
    }

    pub(super) fn bind_initialized_variable_flow(
        &self,
        node: &Node, /*VariableDeclaration | ArrayBindingElement*/
    ) {
        let name: Option<Rc<Node>> = if !is_omitted_expression(node) {
            node.as_named_declaration().maybe_name()
        } else {
            None
        };
        if is_binding_pattern(Some(node)) {
            for child in node.as_has_elements().elements() {
                self.bind_initialized_variable_flow(child);
            }
        } else {
            self.set_current_flow(Some(self.create_flow_mutation(
                FlowFlags::Assignment,
                self.current_flow(),
                node,
            )));
        }
    }

    pub(super) fn bind_variable_declaration_flow(&self, node: &Node /*VariableDeclaration*/) {
        self.bind_each_child(node);
        if node.as_variable_declaration().maybe_initializer().is_some()
            || is_for_in_or_of_statement(&node.parent().parent())
        {
            self.bind_initialized_variable_flow(node);
        }
    }

    pub(super) fn bind_binding_element_flow(&self, node: &Node /*BindingElement*/) {
        let node_as_binding_element = node.as_binding_element();
        if is_binding_pattern(node_as_binding_element.maybe_name()) {
            self.bind_each(node_as_binding_element.maybe_decorators().as_deref());
            self.bind_each(node_as_binding_element.maybe_modifiers().as_deref());
            self.bind(node_as_binding_element.dot_dot_dot_token.clone());
            self.bind(node_as_binding_element.property_name.clone());
            self.bind(node_as_binding_element.maybe_initializer());
            self.bind(node_as_binding_element.maybe_name());
        } else {
            self.bind_each_child(node);
        }
    }

    pub(super) fn bind_jsdoc_type_alias(
        &self,
        node: &Node, /*JSDocTypedefTag | JSDocCallbackTag | JSDocEnumTag*/
    ) {
        let node_as_jsdoc_tag = node.as_jsdoc_tag();
        self.bind(Some(node_as_jsdoc_tag.tag_name()));
        if node.kind() != SyntaxKind::JSDocEnumTag {
            let node_as_jsdoc_typedef_or_callback_tag = node.as_jsdoc_typedef_or_callback_tag();
            if let Some(node_full_name) = node_as_jsdoc_typedef_or_callback_tag.maybe_full_name() {
                set_parent(&node_full_name, Some(node.node_wrapper()));
                set_parent_recursive(Some(node_full_name), false);
            }
        }
        if let Some(StringOrNodeArray::NodeArray(node_comment)) = node_as_jsdoc_tag.maybe_comment()
        {
            self.bind_each(Some(node_comment));
        }
    }

    pub(super) fn bind_jsdoc_class_tag(&self, node: &Node /*JSDocClassTag*/) {
        self.bind_each_child(node);
        let host = get_host_signature_from_jsdoc(node);
        if let Some(host) = host.filter(|host| host.kind() != SyntaxKind::MethodDeclaration) {
            self.add_declaration_to_symbol(&host.symbol(), &host, SymbolFlags::Class);
        }
    }

    pub(super) fn bind_optional_expression(
        &self,
        node: &Node, /*expression*/
        true_target: Rc<FlowNode /*FlowLabel*/>,
        false_target: Rc<FlowNode /*FlowLabel*/>,
    ) {
        self.do_with_conditional_branches(
            |node| self.bind(Some(node)),
            node,
            true_target.clone(),
            false_target.clone(),
        );
        if !is_optional_chain(node) || is_outermost_optional_chain(node) {
            self.add_antecedent(
                &true_target,
                self.create_flow_condition(
                    FlowFlags::TrueCondition,
                    self.current_flow(),
                    Some(node),
                ),
            );
            self.add_antecedent(
                &false_target,
                self.create_flow_condition(
                    FlowFlags::FalseCondition,
                    self.current_flow(),
                    Some(node),
                ),
            );
        }
    }

    pub(super) fn bind_optional_chain_rest(&self, node: &Node /*OptionalChain*/) {
        match node.kind() {
            SyntaxKind::PropertyAccessExpression => {
                let node_as_property_access_expression = node.as_property_access_expression();
                self.bind(
                    node_as_property_access_expression
                        .question_dot_token
                        .clone(),
                );
                self.bind(Some(&*node_as_property_access_expression.name));
            }
            SyntaxKind::ElementAccessExpression => {
                let node_as_element_access_expression = node.as_element_access_expression();
                self.bind(node_as_element_access_expression.question_dot_token.clone());
                self.bind(Some(
                    &*node_as_element_access_expression.argument_expression,
                ));
            }
            SyntaxKind::CallExpression => {
                let node_as_call_expression = node.as_call_expression();
                self.bind(node_as_call_expression.question_dot_token.clone());
                self.bind_each(node_as_call_expression.type_arguments.as_deref());
                self.bind_each(Some(&node_as_call_expression.arguments));
            }
            _ => (),
        }
    }

    pub(super) fn bind_optional_chain(
        &self,
        node: &Node, /*OptionalChain*/
        true_target: Rc<FlowNode /*FlowLabel*/>,
        false_target: Rc<FlowNode /*FlowLabel*/>,
    ) {
        let pre_chain_label = if is_optional_chain_root(node) {
            Some(self.create_branch_label())
        } else {
            None
        };
        self.bind_optional_expression(
            &node.as_has_expression().expression(),
            pre_chain_label
                .clone()
                .unwrap_or_else(|| true_target.clone()),
            false_target.clone(),
        );
        if let Some(pre_chain_label) = pre_chain_label {
            self.set_current_flow(Some(self.finish_flow_label(pre_chain_label)));
        }
        self.do_with_conditional_branches(
            |node| self.bind_optional_chain_rest(node),
            node,
            true_target.clone(),
            false_target.clone(),
        );
        if is_outermost_optional_chain(node) {
            self.add_antecedent(
                &true_target,
                self.create_flow_condition(
                    FlowFlags::TrueCondition,
                    self.current_flow(),
                    Some(node),
                ),
            );
            self.add_antecedent(
                &false_target,
                self.create_flow_condition(
                    FlowFlags::FalseCondition,
                    self.current_flow(),
                    Some(node),
                ),
            );
        }
    }

    pub(super) fn bind_optional_chain_flow(&self, node: &Node /*OptionalChain*/) {
        if self.is_top_level_logical_expression(node) {
            let post_expression_label = self.create_branch_label();
            self.bind_optional_chain(
                node,
                post_expression_label.clone(),
                post_expression_label.clone(),
            );
            self.set_current_flow(Some(self.finish_flow_label(post_expression_label)));
        } else {
            self.bind_optional_chain(
                node,
                self.current_true_target(),
                self.current_false_target(),
            );
        }
    }

    pub(super) fn bind_non_null_expression_flow(
        &self,
        node: &Node, /*NonNullExpression | NonNullChain*/
    ) {
        if is_optional_chain(node) {
            self.bind_optional_chain_flow(node);
        } else {
            self.bind_each_child(node);
        }
    }

    pub(super) fn bind_access_expression_flow(
        &self,
        node: &Node, /*AccessExpression | PropertyAccessChain | ElementAccessChain*/
    ) {
        if is_optional_chain(node) {
            self.bind_optional_chain_flow(node);
        } else {
            self.bind_each_child(node);
        }
    }

    pub(super) fn bind_call_expression_flow(
        &self,
        node: &Node, /*CallExpression | CallChain*/
    ) {
        let node_as_call_expression = node.as_call_expression();
        if is_optional_chain(node) {
            self.bind_optional_chain_flow(node);
        } else {
            let expr = skip_parentheses(&node_as_call_expression.expression, None);
            if matches!(
                expr.kind(),
                SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction
            ) {
                self.bind_each(node_as_call_expression.type_arguments.as_deref());
                self.bind_each(Some(&*node_as_call_expression.arguments));
                self.bind(Some(&*node_as_call_expression.expression));
            } else {
                self.bind_each_child(node);
                if node_as_call_expression.expression.kind() == SyntaxKind::SuperKeyword {
                    self.set_current_flow(Some(self.create_flow_call(self.current_flow(), node)));
                }
            }
        }
        if node_as_call_expression.expression.kind() == SyntaxKind::PropertyAccessExpression {
            let property_access = node_as_call_expression
                .expression
                .as_property_access_expression();
            if is_identifier(&property_access.name)
                && self.is_narrowable_operand(&property_access.expression)
                && is_push_or_unshift_identifier(&property_access.name)
            {
                self.set_current_flow(Some(self.create_flow_mutation(
                    FlowFlags::ArrayMutation,
                    self.current_flow(),
                    node,
                )));
            }
        }
    }

    pub(super) fn get_container_flags(&self, node: &Node) -> ContainerFlags {
        match node.kind() {
            SyntaxKind::ClassExpression
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::EnumDeclaration
            | SyntaxKind::ObjectLiteralExpression
            | SyntaxKind::TypeLiteral
            | SyntaxKind::JSDocTypeLiteral
            | SyntaxKind::JsxAttributes => ContainerFlags::IsContainer,

            SyntaxKind::InterfaceDeclaration => {
                ContainerFlags::IsContainer | ContainerFlags::IsInterface
            }

            SyntaxKind::ModuleDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::MappedType => ContainerFlags::IsContainer | ContainerFlags::HasLocals,

            SyntaxKind::SourceFile => {
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
            }

            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor | SyntaxKind::MethodDeclaration => {
                if is_object_literal_or_class_expression_method_or_accessor(node) {
                    return ContainerFlags::IsContainer
                        | ContainerFlags::IsControlFlowContainer
                        | ContainerFlags::HasLocals
                        | ContainerFlags::IsFunctionLike
                        | ContainerFlags::IsObjectLiteralOrClassExpressionMethodOrAccessor;
                }
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
                    | ContainerFlags::IsFunctionLike
            }

            SyntaxKind::Constructor
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::MethodSignature
            | SyntaxKind::CallSignature
            | SyntaxKind::JSDocSignature
            | SyntaxKind::JSDocFunctionType
            | SyntaxKind::FunctionType
            | SyntaxKind::ConstructSignature
            | SyntaxKind::IndexSignature
            | SyntaxKind::ConstructorType
            | SyntaxKind::ClassStaticBlockDeclaration => {
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
                    | ContainerFlags::IsFunctionLike
            }

            SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction => {
                ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
                    | ContainerFlags::IsFunctionLike
                    | ContainerFlags::IsFunctionExpression
            }

            SyntaxKind::ModuleBlock => ContainerFlags::IsControlFlowContainer,
            SyntaxKind::PropertyDeclaration => {
                if node.as_property_declaration().maybe_initializer().is_some() {
                    ContainerFlags::IsControlFlowContainer
                } else {
                    ContainerFlags::None
                }
            }

            SyntaxKind::CatchClause
            | SyntaxKind::ForStatement
            | SyntaxKind::ForInStatement
            | SyntaxKind::ForOfStatement
            | SyntaxKind::CaseBlock => ContainerFlags::IsBlockScopedContainer,

            SyntaxKind::Block => {
                if is_function_like(node.maybe_parent())
                    || is_class_static_block_declaration(&node.parent())
                {
                    ContainerFlags::None
                } else {
                    ContainerFlags::IsBlockScopedContainer
                }
            }
            _ => ContainerFlags::None,
        }
    }

    pub(super) fn add_to_container_chain(&self, next: &Node) {
        if let Some(last_container) = self.maybe_last_container() {
            last_container.set_next_container(Some(next.node_wrapper()));
        }

        self.set_last_container(Some(next.node_wrapper()));
    }

    pub(super) fn declare_symbol_and_add_to_symbol_table(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        match self.container().kind() {
            SyntaxKind::ModuleDeclaration => {
                Some(self.declare_module_member(node, symbol_flags, symbol_excludes))
            }

            SyntaxKind::SourceFile => {
                Some(self.declare_source_file_member(node, symbol_flags, symbol_excludes))
            }

            SyntaxKind::ClassExpression | SyntaxKind::ClassDeclaration => {
                Some(self.declare_class_member(node, symbol_flags, symbol_excludes))
            }

            SyntaxKind::EnumDeclaration => Some(self.declare_symbol(
                &mut *self.container().symbol().exports().borrow_mut(),
                Some(self.container().symbol()),
                node,
                symbol_flags,
                symbol_excludes,
                None,
                None,
            )),

            SyntaxKind::TypeLiteral
            | SyntaxKind::JSDocTypeLiteral
            | SyntaxKind::ObjectLiteralExpression
            | SyntaxKind::InterfaceDeclaration
            | SyntaxKind::JsxAttributes => Some(self.declare_symbol(
                &mut *self.container().symbol().members().borrow_mut(),
                Some(self.container().symbol()),
                node,
                symbol_flags,
                symbol_excludes,
                None,
                None,
            )),

            SyntaxKind::FunctionType
            | SyntaxKind::ConstructorType
            | SyntaxKind::CallSignature
            | SyntaxKind::ConstructSignature
            | SyntaxKind::JSDocSignature
            | SyntaxKind::IndexSignature
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::MethodSignature
            | SyntaxKind::Constructor
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction
            | SyntaxKind::JSDocFunctionType
            | SyntaxKind::JSDocTypedefTag
            | SyntaxKind::JSDocCallbackTag
            | SyntaxKind::ClassStaticBlockDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::MappedType => Some(self.declare_symbol(
                &mut *self.container().locals(),
                Option::<&Symbol>::None,
                node,
                symbol_flags,
                symbol_excludes,
                None,
                None,
            )),
            _ => None,
        }
    }

    pub(super) fn declare_class_member(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Rc<Symbol> {
        if is_static(node) {
            self.declare_symbol(
                &mut *self.container().symbol().exports().borrow_mut(),
                Some(self.container().symbol()),
                node,
                symbol_flags,
                symbol_excludes,
                None,
                None,
            )
        } else {
            self.declare_symbol(
                &mut *self.container().symbol().members().borrow_mut(),
                Some(self.container().symbol()),
                node,
                symbol_flags,
                symbol_excludes,
                None,
                None,
            )
        }
    }

    pub(super) fn declare_source_file_member(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Rc<Symbol> {
        if is_external_module(&self.file()) {
            self.declare_module_member(node, symbol_flags, symbol_excludes)
        } else {
            self.declare_symbol(
                &mut *self.file().locals(),
                Option::<&Symbol>::None,
                node,
                symbol_flags,
                symbol_excludes,
                None,
                None,
            )
        }
    }

    pub(super) fn has_export_declarations(
        &self,
        node: &Node, /*ModuleDeclaration | SourceFile*/
    ) -> bool {
        let body: Option<Rc<Node>> = if is_source_file(node) {
            Some(node.node_wrapper())
        } else {
            node.as_module_declaration()
                .body
                .as_ref()
                .and_then(|body| try_cast(body, |body| is_module_block(body)))
                .map(Clone::clone)
        };
        match body {
            None => false,
            Some(body) => body
                .as_has_statements()
                .statements()
                .iter()
                .any(|s| is_export_declaration(s) || is_export_assignment(s)),
        }
    }

    pub(super) fn set_export_context_flag(
        &self,
        node: &Node, /*ModuleDeclaration | SourceFile*/
    ) {
        if node.flags().intersects(NodeFlags::Ambient) && !self.has_export_declarations(node) {
            node.set_flags(node.flags() | NodeFlags::ExportContext);
        } else {
            node.set_flags(node.flags() & !NodeFlags::ExportContext);
        }
    }

    pub(super) fn bind_module_declaration(&self, node: &Node /*ModuleDeclaration*/) {
        self.set_export_context_flag(node);
        if is_ambient_module(node) {
            if has_syntactic_modifier(node, ModifierFlags::Export) {
                self.error_on_first_token(node, &Diagnostics::export_modifier_cannot_be_applied_to_ambient_modules_and_module_augmentations_since_they_are_always_visible, None);
            }
            if is_module_augmentation_external(node) {
                self.declare_module_symbol(node);
            } else {
                let mut pattern: Option<StringOrPattern> = None;
                let node_as_module_declaration = node.as_module_declaration();
                if node_as_module_declaration.name.kind() == SyntaxKind::StringLiteral {
                    let text = node_as_module_declaration
                        .name
                        .as_literal_like_node()
                        .text();
                    pattern = try_parse_pattern(&text);
                    if pattern.is_none() {
                        self.error_on_first_token(
                            &node_as_module_declaration.name,
                            &Diagnostics::Pattern_0_can_have_at_most_one_Asterisk_character,
                            Some(vec![text.to_owned()]),
                        );
                    }
                }

                let symbol = self
                    .declare_symbol_and_add_to_symbol_table(
                        node,
                        SymbolFlags::ValueModule,
                        SymbolFlags::ValueModuleExcludes,
                    )
                    .unwrap();
                let file = self.file();
                let mut pattern_ambient_modules =
                    file.as_source_file().pattern_ambient_modules_mut();
                if pattern_ambient_modules.is_none() {
                    *pattern_ambient_modules = Some(vec![]);
                }
                append(
                    pattern_ambient_modules.as_mut().unwrap(),
                    match pattern {
                        Some(StringOrPattern::Pattern(pattern)) => {
                            Some(PatternAmbientModule::new(pattern, symbol))
                        }
                        _ => None,
                    },
                );
            }
        } else {
            let state = self.declare_module_symbol(node);
            if state != ModuleInstanceState::NonInstantiated {
                let symbol = node.symbol();
                symbol.set_const_enum_only_module(Some(
                    !symbol.flags().intersects(
                        SymbolFlags::Function | SymbolFlags::Class | SymbolFlags::RegularEnum,
                    ) && state == ModuleInstanceState::ConstEnumOnly
                        && !matches!(symbol.maybe_const_enum_only_module(), Some(false)),
                ));
            }
        }
    }

    pub(super) fn declare_module_symbol(
        &self,
        node: &Node, /*ModuleDeclaration*/
    ) -> ModuleInstanceState {
        unimplemented!()
    }

    pub(super) fn bind_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) {
        self.bind_anonymous_declaration(
            node,
            SymbolFlags::ObjectLiteral,
            InternalSymbolName::Object(),
        );
    }

    pub(super) fn bind_anonymous_declaration(
        &self,
        node: &Node,
        symbol_flags: SymbolFlags,
        name: __String,
    ) -> Rc<Symbol> {
        let symbol = self.create_symbol(symbol_flags, name).wrap();
        self.file()
            .as_source_file()
            .keep_strong_reference_to_symbol(symbol.clone());
        self.add_declaration_to_symbol(&symbol, node, symbol_flags);
        symbol
    }

    pub(super) fn bind_block_scoped_declaration(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) {
        let block_scope_container = self.block_scope_container();
        {
            let mut block_scope_container_locals = block_scope_container.maybe_locals();
            if block_scope_container_locals.is_none() {
                *block_scope_container_locals = Some(create_symbol_table(None));
            }
        }
        self.declare_symbol(
            &mut *block_scope_container.locals(),
            Option::<&Symbol>::None,
            node,
            symbol_flags,
            symbol_excludes,
            None,
            None,
        );
    }

    pub(super) fn delayed_bind_jsdoc_typedef_tag(&self) {
        // unimplemented!()
    }

    pub(super) fn error_on_first_token(
        &self,
        node: &Node,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        unimplemented!()
    }

    pub(super) fn error_or_suggestion_on_node(
        &self,
        is_error: bool,
        node: &Node,
        message: &DiagnosticMessage,
    ) {
        unimplemented!()
    }

    pub(super) fn bind<TNode: Borrow<Node>>(&self, node: Option<TNode>) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        let node = node.borrow();
        set_parent(node, self.maybe_parent());

        self.bind_worker(node);

        if node.kind() > SyntaxKind::LastToken {
            let save_parent = self.maybe_parent();
            self.set_parent(Some(node.node_wrapper()));
            let container_flags = self.get_container_flags(node);
            if container_flags == ContainerFlags::None {
                self.bind_children(node);
            } else {
                self.bind_container(node, container_flags);
            }
            self.set_parent(save_parent);
        }
    }

    pub(super) fn bind_jsdoc(&self, node: &Node) {
        // unimplemented!()
    }

    pub(super) fn bind_worker(&self, node: &Node) {
        match node {
            Node::TypeParameterDeclaration(_) => self.bind_type_parameter(node),
            Node::ParameterDeclaration(_) => self.bind_parameter(node),
            Node::VariableDeclaration(_) => self.bind_variable_declaration_or_binding_element(node),
            Node::PropertySignature(_) => self.bind_property_worker(node),
            Node::PropertyAssignment(_) => self.bind_property_or_method_or_accessor(
                node,
                SymbolFlags::Property,
                SymbolFlags::PropertyExcludes,
            ),
            Node::FunctionDeclaration(_) => self.bind_function_declaration(node),
            Node::ObjectLiteralExpression(_) => self.bind_object_literal_expression(node),
            Node::InterfaceDeclaration(_) => self.bind_block_scoped_declaration(
                node,
                SymbolFlags::Interface,
                SymbolFlags::InterfaceExcludes,
            ),
            Node::TypeAliasDeclaration(_) => self.bind_block_scoped_declaration(
                node,
                SymbolFlags::TypeAlias,
                SymbolFlags::TypeAliasExcludes,
            ),
            _ => (),
        }
    }

    pub(super) fn bind_property_worker(&self, node: &Node /*PropertySignature*/) {
        self.bind_property_or_method_or_accessor(
            node,
            SymbolFlags::Property
                | if false {
                    unimplemented!()
                } else {
                    SymbolFlags::None
                },
            SymbolFlags::PropertyExcludes,
        )
    }

    pub(super) fn is_top_level_namespace_assignment(
        &self,
        property_access: &Node, /*BindableAccessExpression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn bind_variable_declaration_or_binding_element(
        &self,
        node: &Node, /*VariableDeclaration*/
    ) {
        let node_as_variable_declaration = node.as_variable_declaration();
        if !is_binding_pattern(Some(node_as_variable_declaration.name())) {
            if false {
                unimplemented!()
            } else if is_block_or_catch_scoped(node) {
                self.bind_block_scoped_declaration(
                    node,
                    SymbolFlags::BlockScopedVariable,
                    SymbolFlags::BlockScopedVariableExcludes,
                );
            } else {
                self.declare_symbol_and_add_to_symbol_table(
                    node,
                    SymbolFlags::FunctionScopedVariable,
                    SymbolFlags::FunctionScopedVariableExcludes,
                );
            }
        }
    }

    pub(super) fn bind_parameter(&self, node: &Node /*ParameterDeclaration*/) {
        if is_binding_pattern(Some(node.as_parameter_declaration().name())) {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::FunctionScopedVariable,
                SymbolFlags::ParameterExcludes,
            );
        }
    }

    pub(super) fn bind_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::Function,
                SymbolFlags::FunctionExcludes,
            );
        }
    }

    pub(super) fn bind_property_or_method_or_accessor(
        &self,
        node: &Node,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(node, symbol_flags, symbol_excludes);
        }
    }

    pub(super) fn bind_type_parameter(&self, node: &Node /*TypeParameterDeclaration*/) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::TypeParameter,
                SymbolFlags::TypeParameterExcludes,
            );
        }
    }

    pub(super) fn check_unreachable(&self, node: &Node) -> bool {
        false
        // unimplemented!()
    }
}
