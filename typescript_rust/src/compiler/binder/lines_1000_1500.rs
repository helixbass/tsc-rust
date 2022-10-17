#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{init_flow_node, ActiveLabel, BinderType};
use crate::{
    concatenate, is_binary_expression, is_dotted_name,
    is_logical_or_coalescing_assignment_operator, is_optional_chain, is_outermost_optional_chain,
    is_parenthesized_expression, is_prefix_unary_expression, skip_parentheses,
    unused_label_is_error, Diagnostics, FlowArrayMutation, FlowAssignment, FlowCall, FlowFlags,
    FlowNode, FlowNodeBase, FlowSwitchClause, HasInitializerInterface, SyntaxKind, __String,
    for_each_bool, NamedDeclarationInterface, Node, NodeInterface,
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
            if flags.intersects(FlowFlags::ArrayMutation) {
                FlowArrayMutation::new(flags, antecedent, node.node_wrapper()).into()
            } else {
                FlowAssignment::new(flags, antecedent, node.node_wrapper()).into()
            },
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
        self.bind(node.as_has_expression().maybe_expression());
        if node.kind() == SyntaxKind::ReturnStatement {
            self.set_has_explicit_return(Some(true));
            if let Some(current_return_target) = self.maybe_current_return_target() {
                self.add_antecedent(&current_return_target, self.current_flow());
            }
        }
        self.set_current_flow(Some(self.unreachable_flow()));
    }

    pub(super) fn find_active_label(
        &self,
        name: &str, /*__String*/
    ) -> Option<Rc<ActiveLabel>> {
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
            while clauses[i]
                .as_case_or_default_clause()
                .statements()
                .is_empty()
                && i + 1 < clauses.len()
            {
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
                    .as_case_or_default_clause()
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
}
