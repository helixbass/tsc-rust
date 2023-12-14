use std::borrow::Borrow;

use gc::Gc;
use id_arena::Id;

use super::{Label, TransformGenerators};
use crate::{
    id_text, is_expression, is_statement, is_variable_declaration_list, maybe_visit_node,
    visit_each_child, visit_node, Matches, NamedDeclarationInterface, Node, NodeArray,
    NodeInterface, Number, SyntaxKind, TransformFlags, VisitResult, _d, get_emit_flags,
    is_generated_identifier, EmitFlags,
};

impl TransformGenerators {
    pub(super) fn transform_and_emit_for_in_statement(
        &self,
        node: Id<Node>, /*ForInStatement*/
    ) {
        let node_as_for_in_statement = node.as_for_in_statement();
        if self.contains_yield(Some(node)) {
            let keys_array = self.declare_local(None);
            let key = self.declare_local(None);
            let keys_index = self.factory.create_loop_variable(None);
            let initializer = &node_as_for_in_statement.initializer;
            self.context.hoist_variable_declaration(&keys_index);
            self.emit_assignment(
                keys_array.clone(),
                self.factory
                    .create_array_literal_expression(Option::<Gc<NodeArray>>::None, None),
                Option::<Id<Node>>::None,
            );

            self.emit_statement(
                self.factory.create_for_in_statement(
                    key.clone(),
                    visit_node(
                        &node_as_for_in_statement.expression,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                    self.factory.create_expression_statement(
                        self.factory.create_call_expression(
                            self.factory
                                .create_property_access_expression(keys_array.clone(), "push"),
                            Option::<Gc<NodeArray>>::None,
                            Some(vec![key]),
                        ),
                    ),
                ),
            );

            self.emit_assignment(
                keys_index.clone(),
                self.factory.create_numeric_literal(Number::new(0.0), None),
                Option::<Id<Node>>::None,
            );

            let condition_label = self.define_label();
            let increment_label = self.define_label();
            let end_label = self.begin_loop_block(increment_label);

            self.mark_label(condition_label);
            self.emit_break_when_false(
                end_label,
                self.factory.create_less_than(
                    keys_index.clone(),
                    self.factory
                        .create_property_access_expression(keys_array.clone(), "length"),
                ),
                Option::<Id<Node>>::None,
            );

            let variable: Id<Node /*Expression*/> = if is_variable_declaration_list(initializer) {
                let initializer_as_variable_declaration_list =
                    initializer.as_variable_declaration_list();
                for variable in &initializer_as_variable_declaration_list.declarations {
                    self.context
                        .hoist_variable_declaration(&variable.as_variable_declaration().name());
                }

                self.factory.clone_node(
                    &initializer_as_variable_declaration_list.declarations[0]
                        .as_variable_declaration()
                        .name(),
                )
            } else {
                visit_node(
                    initializer,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )
            };

            self.emit_assignment(
                variable,
                self.factory
                    .create_element_access_expression(keys_array, keys_index.clone()),
                Option::<Id<Node>>::None,
            );
            self.transform_and_emit_embedded_statement(&node_as_for_in_statement.statement);

            self.mark_label(increment_label);
            self.emit_statement(
                self.factory
                    .create_expression_statement(self.factory.create_postfix_increment(keys_index)),
            );

            self.emit_break(condition_label, Option::<Id<Node>>::None);
            self.end_loop_block();
        } else {
            self.emit_statement(visit_node(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_statement),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ));
        }
    }

    pub(super) fn visit_for_in_statement(
        &self,
        node: Id<Node>, /*ForInStatement*/
    ) -> VisitResult {
        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.begin_script_loop_block();
        }

        let ref initializer = node.as_for_in_statement().initializer.clone();
        let mut node = node.node_wrapper();
        if is_variable_declaration_list(initializer) {
            let initializer_as_variable_declaration_list =
                initializer.as_variable_declaration_list();
            for variable in &initializer_as_variable_declaration_list.declarations {
                self.context
                    .hoist_variable_declaration(&variable.as_variable_declaration().name());
            }

            node = self.factory.update_for_in_statement(
                &node,
                initializer_as_variable_declaration_list.declarations[0]
                    .as_variable_declaration()
                    .name(),
                visit_node(
                    &node.as_for_in_statement().expression,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
                visit_node(
                    &node.as_for_in_statement().statement,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_statement),
                    Some(&|nodes: &[Id<Node>]| self.factory.lift_to_block(nodes)),
                ),
            );
        } else {
            node = visit_each_child(&node, |node: Id<Node>| self.visitor(node), &**self.context);
        }

        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.end_loop_block();
        }

        Some(node.into())
    }

    pub(super) fn transform_and_emit_continue_statement(
        &self,
        node: Id<Node>, /*ContinueStatement*/
    ) {
        let node_as_continue_statement = node.as_continue_statement();
        let label = self.find_continue_target(
            node_as_continue_statement
                .label
                .as_ref()
                .map(|node_label| id_text(node_label)),
        );
        if label > 0 {
            self.emit_break(label, Some(node));
        } else {
            self.emit_statement(node.node_wrapper());
        }
    }

    pub(super) fn visit_continue_statement(
        &self,
        node: Id<Node>, /*ContinueStatement*/
    ) -> Id<Node /*Statement*/> {
        let node_as_continue_statement = node.as_continue_statement();
        if self.maybe_in_statement_containing_yield() == Some(true) {
            let label = self.find_continue_target(
                node_as_continue_statement
                    .label
                    .as_ref()
                    .map(|node_label| id_text(node_label)),
            );
            if label > 0 {
                return self.create_inline_break(label, Some(node));
            }
        }

        visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context)
    }

    pub(super) fn transform_and_emit_break_statement(
        &self,
        node: Id<Node>, /*BreakStatement*/
    ) {
        let node_as_break_statement = node.as_break_statement();
        let label = self.find_break_target(
            node_as_break_statement
                .label
                .as_ref()
                .map(|node_label| id_text(node_label)),
        );
        if label > 0 {
            self.emit_break(label, Some(node));
        } else {
            self.emit_statement(node.node_wrapper())
        }
    }

    pub(super) fn visit_break_statement(
        &self,
        node: Id<Node>, /*BreakStatement*/
    ) -> Id<Node /*Statement*/> {
        let node_as_break_statement = node.as_break_statement();
        if self.maybe_in_statement_containing_yield() == Some(true) {
            let label = self.find_break_target(
                node_as_break_statement
                    .label
                    .as_ref()
                    .map(|node_label| id_text(node_label)),
            );
            if label > 0 {
                return self.create_inline_break(label, Some(node));
            }
        }

        visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context)
    }

    pub(super) fn transform_and_emit_return_statement(
        &self,
        node: Id<Node>, /*ReturnStatement*/
    ) {
        let node_as_return_statement = node.as_return_statement();
        self.emit_return(
            maybe_visit_node(
                node_as_return_statement.expression.as_deref(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ),
            Some(node),
        );
    }

    pub(super) fn visit_return_statement(
        &self,
        node: Id<Node>, /*ReturnStatement*/
    ) -> VisitResult {
        let node_as_return_statement = node.as_return_statement();
        Some(
            self.create_inline_return(
                maybe_visit_node(
                    node_as_return_statement.expression.as_deref(),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
                Some(node),
            )
            .into(),
        )
    }

    pub(super) fn transform_and_emit_with_statement(&self, node: Id<Node> /*WithStatement*/) {
        let node_as_with_statement = node.as_with_statement();
        if self.contains_yield(Some(node)) {
            self.begin_with_block(self.cache_expression(&visit_node(
                &node_as_with_statement.expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )));
            self.transform_and_emit_embedded_statement(&node_as_with_statement.statement);
            self.end_with_block();
        } else {
            self.emit_statement(visit_node(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_statement),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ));
        }
    }

    pub(super) fn transform_and_emit_switch_statement(
        &self,
        node: Id<Node>, /*SwitchStatement*/
    ) {
        let node_as_switch_statement = node.as_switch_statement();
        if self.contains_yield(Some(&*node_as_switch_statement.case_block)) {
            let case_block = &node_as_switch_statement.case_block;
            let case_block_as_case_block = case_block.as_case_block();
            let num_clauses = case_block_as_case_block.clauses.len();
            let end_label = self.begin_switch_block();

            let expression = self.cache_expression(&visit_node(
                &node_as_switch_statement.expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ));

            let mut clause_labels: Vec<Label> = _d();
            let mut default_clause_index: Option<usize> = _d();
            for (i, clause) in case_block_as_case_block
                .clauses
                .iter()
                .enumerate()
                .take(num_clauses)
            {
                clause_labels.push(self.define_label());
                if clause.kind() == SyntaxKind::DefaultClause && default_clause_index.is_none() {
                    default_clause_index = Some(i);
                }
            }

            let mut clauses_written = 0;
            let mut pending_clauses: Vec<Id<Node /*CaseClause*/>> = _d();
            while clauses_written < num_clauses {
                let mut default_clauses_skipped = 0;
                for (i, clause) in case_block_as_case_block
                    .clauses
                    .iter()
                    .enumerate()
                    .skip(clauses_written)
                    .take(num_clauses - clauses_written)
                {
                    if clause.kind() == SyntaxKind::CaseClause {
                        let clause_as_case_clause = clause.as_case_clause();
                        if self.contains_yield(Some(&*clause_as_case_clause.expression))
                            && !pending_clauses.is_empty()
                        {
                            break;
                        }

                        pending_clauses.push(self.factory.create_case_clause(
                            visit_node(
                                &clause_as_case_clause.expression,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(is_expression),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ),
                            vec![self.create_inline_break(
                                clause_labels[i],
                                Some(&*clause_as_case_clause.expression),
                            )],
                        ));
                    } else {
                        default_clauses_skipped += 1;
                    }
                }

                if !pending_clauses.is_empty() {
                    let pending_clauses_len = pending_clauses.len();
                    self.emit_statement(self.factory.create_switch_statement(
                        expression.clone(),
                        self.factory.create_case_block(pending_clauses),
                    ));
                    clauses_written += pending_clauses_len;
                    pending_clauses = _d();
                }
                if default_clauses_skipped > 0 {
                    clauses_written += default_clauses_skipped;
                    // defaultClausesSkipped = 0;
                }
            }

            if let Some(default_clause_index) = default_clause_index {
                self.emit_break(
                    clause_labels[default_clause_index],
                    Option::<Id<Node>>::None,
                );
            } else {
                self.emit_break(end_label, Option::<Id<Node>>::None);
            }

            for (i, &clause_label) in clause_labels.iter().enumerate().take(num_clauses) {
                self.mark_label(clause_label);
                self.transform_and_emit_statements(
                    &case_block_as_case_block.clauses[i]
                        .as_has_statements()
                        .statements(),
                    None,
                );
            }

            self.end_switch_block();
        } else {
            self.emit_statement(visit_node(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_statement),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ));
        }
    }

    pub(super) fn visit_switch_statement(
        &self,
        node: Id<Node>, /*SwitchStatement*/
    ) -> VisitResult {
        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.begin_script_switch_block();
        }

        let node = visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context);

        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.end_switch_block();
        }

        Some(node.into())
    }

    pub(super) fn transform_and_emit_labeled_statement(
        &self,
        node: Id<Node>, /*LabeledStatement*/
    ) {
        let node_as_labeled_statement = node.as_labeled_statement();
        if self.contains_yield(Some(node)) {
            self.begin_labeled_block(id_text(&node_as_labeled_statement.label).to_owned());
            self.transform_and_emit_embedded_statement(&node_as_labeled_statement.statement);
            self.end_labeled_block();
        } else {
            self.emit_statement(visit_node(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_statement),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ))
        }
    }

    pub(super) fn visit_labeled_statement(
        &self,
        node: Id<Node>, /*LabeledStatement*/
    ) -> VisitResult {
        let node_as_labeled_statement = node.as_labeled_statement();
        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.begin_script_labeled_block(id_text(&node_as_labeled_statement.label).to_owned());
        }

        let node = visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context);

        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.end_labeled_block();
        }

        Some(node.into())
    }

    pub(super) fn transform_and_emit_throw_statement(
        &self,
        node: Id<Node>, /*ThrowStatement*/
    ) {
        let node_as_throw_statement = node.as_throw_statement();
        self.emit_throw(
            visit_node(
                &node_as_throw_statement.expression, /*?? factory.createVoidZero()*/
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ),
            Some(node),
        );
    }

    pub(super) fn transform_and_emit_try_statement(&self, node: Id<Node> /*TryStatement*/) {
        let node_as_try_statement = node.as_try_statement();
        if self.contains_yield(Some(node)) {
            self.begin_exception_block();
            self.transform_and_emit_embedded_statement(&node_as_try_statement.try_block);
            if let Some(node_catch_clause) = node_as_try_statement.catch_clause.as_ref() {
                let node_catch_clause_as_catch_clause = node_catch_clause.as_catch_clause();
                self.begin_catch_block(
                    node_catch_clause_as_catch_clause
                        .variable_declaration
                        .as_ref()
                        .unwrap(),
                );
                self.transform_and_emit_embedded_statement(
                    &node_catch_clause_as_catch_clause.block,
                );
            }

            if let Some(node_finally_block) = node_as_try_statement.finally_block.as_ref() {
                self.begin_finally_block();
                self.transform_and_emit_embedded_statement(node_finally_block);
            }

            self.end_exception_block();
        } else {
            self.emit_statement(visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &**self.context,
            ));
        }
    }

    pub(super) fn contains_yield(&self, node: Option<Id<Node>>) -> bool {
        node.matches(|node| {
            let node = node.borrow();
            node.transform_flags()
                .intersects(TransformFlags::ContainsYield)
        })
    }

    pub(super) fn count_initial_nodes_without_yield(
        &self,
        nodes: &NodeArray, /*<Node>*/
    ) -> Option<usize> {
        nodes
            .iter()
            .position(|node| self.contains_yield(Some(&**node)))
    }

    pub(super) fn cache_expression(
        &self,
        node: Id<Node>, /*Expression*/
    ) -> Id<Node /*Identifier*/> {
        if is_generated_identifier(node) || get_emit_flags(node).intersects(EmitFlags::HelperName) {
            return node.node_wrapper();
        }

        let temp = self.factory.create_temp_variable(
            Some(|node: Id<Node>| {
                self.context.hoist_variable_declaration(node);
            }),
            None,
        );
        self.emit_assignment(temp.clone(), node.node_wrapper(), Some(node));
        temp
    }

    pub(super) fn declare_local(&self, name: Option<&str>) -> Id<Node /*Identifier*/> {
        let temp = name.map_or_else(
            || {
                self.factory
                    .create_temp_variable(Option::<fn(Id<Node>)>::None, None)
            },
            |name| self.factory.create_unique_name(name, None),
        );
        self.context.hoist_variable_declaration(&temp);
        temp
    }
}
