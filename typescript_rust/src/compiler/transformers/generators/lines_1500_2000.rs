use id_arena::Id;

use super::{Label, TransformGenerators};
use crate::{
    id_text, is_expression, is_statement, is_variable_declaration_list, maybe_visit_node,
    visit_each_child, visit_node, Matches, NamedDeclarationInterface, Node, NodeArray,
    NodeInterface, Number, SyntaxKind, TransformFlags, VisitResult, _d, get_emit_flags,
    is_generated_identifier, released, CoreTransformationContext, EmitFlags, InArena,
    ReadonlyTextRangeConcrete,
};

impl TransformGenerators {
    pub(super) fn transform_and_emit_for_in_statement(
        &self,
        node: Id<Node>, /*ForInStatement*/
    ) {
        if self.contains_yield(Some(node)) {
            let keys_array = self.declare_local(None);
            let key = self.declare_local(None);
            let keys_index = self.factory.ref_(self).create_loop_variable(None);
            let initializer = node.ref_(self).as_for_in_statement().initializer;
            self.context
                .ref_(self)
                .hoist_variable_declaration(keys_index);
            self.emit_assignment(
                keys_array.clone(),
                self.factory
                    .ref_(self)
                    .create_array_literal_expression(Option::<Id<NodeArray>>::None, None),
                Option::<&Node>::None,
            );

            self.emit_statement(
                self.factory.ref_(self).create_for_in_statement(
                    key.clone(),
                    visit_node(
                        released!(node.ref_(self).as_for_in_statement().expression),
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                    self.factory.ref_(self).create_expression_statement(
                        self.factory.ref_(self).create_call_expression(
                            self.factory
                                .ref_(self)
                                .create_property_access_expression(keys_array.clone(), "push"),
                            Option::<Id<NodeArray>>::None,
                            Some(vec![key]),
                        ),
                    ),
                ),
            );

            self.emit_assignment(
                keys_index.clone(),
                self.factory
                    .ref_(self)
                    .create_numeric_literal(Number::new(0.0), None),
                Option::<&Node>::None,
            );

            let condition_label = self.define_label();
            let increment_label = self.define_label();
            let end_label = self.begin_loop_block(increment_label);

            self.mark_label(condition_label);
            self.emit_break_when_false(
                end_label,
                self.factory.ref_(self).create_less_than(
                    keys_index.clone(),
                    self.factory
                        .ref_(self)
                        .create_property_access_expression(keys_array.clone(), "length"),
                ),
                Option::<&Node>::None,
            );

            let variable: Id<Node /*Expression*/> =
                if is_variable_declaration_list(&initializer.ref_(self)) {
                    let initializer_ref = initializer.ref_(self);
                    let initializer_as_variable_declaration_list =
                        initializer_ref.as_variable_declaration_list();
                    for &variable in &*initializer_as_variable_declaration_list
                        .declarations
                        .ref_(self)
                    {
                        self.context.ref_(self).hoist_variable_declaration(
                            variable.ref_(self).as_variable_declaration().name(),
                        );
                    }

                    self.factory.ref_(self).clone_node(
                        initializer_as_variable_declaration_list
                            .declarations
                            .ref_(self)[0]
                            .ref_(self)
                            .as_variable_declaration()
                            .name(),
                    )
                } else {
                    visit_node(
                        initializer,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )
                };

            self.emit_assignment(
                variable,
                self.factory
                    .ref_(self)
                    .create_element_access_expression(keys_array, keys_index.clone()),
                Option::<&Node>::None,
            );
            self.transform_and_emit_embedded_statement(
                node.ref_(self).as_for_in_statement().statement,
            );

            self.mark_label(increment_label);
            self.emit_statement(self.factory.ref_(self).create_expression_statement(
                self.factory.ref_(self).create_postfix_increment(keys_index),
            ));

            self.emit_break(condition_label, Option::<&Node>::None);
            self.end_loop_block();
        } else {
            self.emit_statement(visit_node(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_statement(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ));
        }
    }

    pub(super) fn visit_for_in_statement(
        &self,
        mut node: Id<Node>, /*ForInStatement*/
    ) -> VisitResult {
        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.begin_script_loop_block();
        }

        let node_ref = node.ref_(self);
        let initializer = node_ref.as_for_in_statement().initializer;
        if is_variable_declaration_list(&initializer.ref_(self)) {
            let initializer_ref = initializer.ref_(self);
            let initializer_as_variable_declaration_list =
                initializer_ref.as_variable_declaration_list();
            for &variable in &*initializer_as_variable_declaration_list
                .declarations
                .ref_(self)
            {
                self.context.ref_(self).hoist_variable_declaration(
                    variable.ref_(self).as_variable_declaration().name(),
                );
            }

            node = self.factory.ref_(self).update_for_in_statement(
                node,
                initializer_as_variable_declaration_list
                    .declarations
                    .ref_(self)[0]
                    .ref_(self)
                    .as_variable_declaration()
                    .name(),
                visit_node(
                    node.ref_(self).as_for_in_statement().expression,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
                visit_node(
                    node.ref_(self).as_for_in_statement().statement,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_statement(node, self)),
                    Some(&|nodes: &[Id<Node>]| self.factory.ref_(self).lift_to_block(nodes)),
                ),
            );
        } else {
            node = visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            );
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
        let node_ref = node.ref_(self);
        let node_as_continue_statement = node_ref.as_continue_statement();
        let label = self.find_continue_target(
            node_as_continue_statement
                .label
                .map(|node_label| id_text(&node_label.ref_(self)).to_owned())
                .as_deref(),
        );
        if label > 0 {
            self.emit_break(label, Some(&*node.ref_(self)));
        } else {
            self.emit_statement(node);
        }
    }

    pub(super) fn visit_continue_statement(
        &self,
        node: Id<Node>, /*ContinueStatement*/
    ) -> Id<Node /*Statement*/> {
        if self.maybe_in_statement_containing_yield() == Some(true) {
            let label = self.find_continue_target(
                node.ref_(self)
                    .as_continue_statement()
                    .label
                    .map(|node_label| id_text(&node_label.ref_(self)).to_owned())
                    .as_deref(),
            );
            if label > 0 {
                return self.create_inline_break(
                    label,
                    Some(&released!(ReadonlyTextRangeConcrete::from(
                        &*node.ref_(self)
                    ))),
                );
            }
        }

        visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }

    pub(super) fn transform_and_emit_break_statement(
        &self,
        node: Id<Node>, /*BreakStatement*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_break_statement = node_ref.as_break_statement();
        let label = self.find_break_target(
            node_as_break_statement
                .label
                .map(|node_label| id_text(&node_label.ref_(self)).to_owned())
                .as_deref(),
        );
        if label > 0 {
            self.emit_break(label, Some(&*node.ref_(self)));
        } else {
            self.emit_statement(node)
        }
    }

    pub(super) fn visit_break_statement(
        &self,
        node: Id<Node>, /*BreakStatement*/
    ) -> Id<Node /*Statement*/> {
        let node_ref = node.ref_(self);
        let node_as_break_statement = node_ref.as_break_statement();
        if self.maybe_in_statement_containing_yield() == Some(true) {
            let label = self.find_break_target(
                node_as_break_statement
                    .label
                    .map(|node_label| id_text(&node_label.ref_(self)).to_owned())
                    .as_deref(),
            );
            if label > 0 {
                return self.create_inline_break(label, Some(&*node.ref_(self)));
            }
        }

        visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }

    pub(super) fn transform_and_emit_return_statement(
        &self,
        node: Id<Node>, /*ReturnStatement*/
    ) {
        self.emit_return(
            maybe_visit_node(
                released!(node.ref_(self).as_return_statement().expression),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ),
            Some(&*node.ref_(self)),
        );
    }

    pub(super) fn visit_return_statement(
        &self,
        node: Id<Node>, /*ReturnStatement*/
    ) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_return_statement = node_ref.as_return_statement();
        Some(
            self.create_inline_return(
                maybe_visit_node(
                    node_as_return_statement.expression,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
                Some(&*node.ref_(self)),
            )
            .into(),
        )
    }

    pub(super) fn transform_and_emit_with_statement(&self, node: Id<Node> /*WithStatement*/) {
        if self.contains_yield(Some(node)) {
            self.begin_with_block(self.cache_expression(visit_node(
                released!(node.ref_(self).as_with_statement().expression),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )));
            self.transform_and_emit_embedded_statement(
                node.ref_(self).as_with_statement().statement,
            );
            self.end_with_block();
        } else {
            self.emit_statement(visit_node(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_statement(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ));
        }
    }

    pub(super) fn transform_and_emit_switch_statement(
        &self,
        node: Id<Node>, /*SwitchStatement*/
    ) {
        if self.contains_yield(Some(node.ref_(self).as_switch_statement().case_block)) {
            let case_block = node.ref_(self).as_switch_statement().case_block;
            let case_block_ref = case_block.ref_(self);
            let case_block_as_case_block = case_block_ref.as_case_block();
            let num_clauses = case_block_as_case_block.clauses.ref_(self).len();
            let end_label = self.begin_switch_block();

            let expression = self.cache_expression(visit_node(
                node.ref_(self).as_switch_statement().expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ));

            let mut clause_labels: Vec<Label> = _d();
            let mut default_clause_index: Option<usize> = _d();
            for (i, clause) in case_block_as_case_block
                .clauses
                .ref_(self)
                .iter()
                .enumerate()
                .take(num_clauses)
            {
                clause_labels.push(self.define_label());
                if clause.ref_(self).kind() == SyntaxKind::DefaultClause
                    && default_clause_index.is_none()
                {
                    default_clause_index = Some(i);
                }
            }

            let mut clauses_written = 0;
            let mut pending_clauses: Vec<Id<Node /*CaseClause*/>> = _d();
            while clauses_written < num_clauses {
                let mut default_clauses_skipped = 0;
                for (i, clause) in case_block_as_case_block
                    .clauses
                    .ref_(self)
                    .iter()
                    .enumerate()
                    .skip(clauses_written)
                    .take(num_clauses - clauses_written)
                {
                    if clause.ref_(self).kind() == SyntaxKind::CaseClause {
                        let clause_ref = clause.ref_(self);
                        let clause_as_case_clause = clause_ref.as_case_clause();
                        if self.contains_yield(Some(clause_as_case_clause.expression))
                            && !pending_clauses.is_empty()
                        {
                            break;
                        }

                        pending_clauses.push(self.factory.ref_(self).create_case_clause(
                            visit_node(
                                clause_as_case_clause.expression,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ),
                            vec![self.create_inline_break(
                                clause_labels[i],
                                Some(&*clause_as_case_clause.expression.ref_(self)),
                            )],
                        ));
                    } else {
                        default_clauses_skipped += 1;
                    }
                }

                if !pending_clauses.is_empty() {
                    let pending_clauses_len = pending_clauses.len();
                    self.emit_statement(self.factory.ref_(self).create_switch_statement(
                        expression.clone(),
                        self.factory.ref_(self).create_case_block(pending_clauses),
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
                self.emit_break(clause_labels[default_clause_index], Option::<&Node>::None);
            } else {
                self.emit_break(end_label, Option::<&Node>::None);
            }

            for (i, &clause_label) in clause_labels.iter().enumerate().take(num_clauses) {
                self.mark_label(clause_label);
                self.transform_and_emit_statements(
                    &case_block_as_case_block.clauses.ref_(self)[i]
                        .ref_(self)
                        .as_has_statements()
                        .statements()
                        .ref_(self),
                    None,
                );
            }

            self.end_switch_block();
        } else {
            self.emit_statement(visit_node(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_statement(node, self)),
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

        let node = visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        );

        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.end_switch_block();
        }

        Some(node.into())
    }

    pub(super) fn transform_and_emit_labeled_statement(
        &self,
        node: Id<Node>, /*LabeledStatement*/
    ) {
        if self.contains_yield(Some(node)) {
            self.begin_labeled_block(
                id_text(&node.ref_(self).as_labeled_statement().label.ref_(self)).to_owned(),
            );
            self.transform_and_emit_embedded_statement(released!(
                node.ref_(self).as_labeled_statement().statement
            ));
            self.end_labeled_block();
        } else {
            self.emit_statement(visit_node(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_statement(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ))
        }
    }

    pub(super) fn visit_labeled_statement(
        &self,
        node: Id<Node>, /*LabeledStatement*/
    ) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_labeled_statement = node_ref.as_labeled_statement();
        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.begin_script_labeled_block(
                id_text(&node_as_labeled_statement.label.ref_(self)).to_owned(),
            );
        }

        let node = visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        );

        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.end_labeled_block();
        }

        Some(node.into())
    }

    pub(super) fn transform_and_emit_throw_statement(
        &self,
        node: Id<Node>, /*ThrowStatement*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_throw_statement = node_ref.as_throw_statement();
        self.emit_throw(
            visit_node(
                node_as_throw_statement.expression, /*?? factory.createVoidZero()*/
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ),
            Some(&*node.ref_(self)),
        );
    }

    pub(super) fn transform_and_emit_try_statement(&self, node: Id<Node> /*TryStatement*/) {
        if self.contains_yield(Some(node)) {
            self.begin_exception_block();
            self.transform_and_emit_embedded_statement(released!(
                node.ref_(self).as_try_statement().try_block
            ));
            if let Some(node_catch_clause) =
                released!(node.ref_(self).as_try_statement().catch_clause)
            {
                self.begin_catch_block(
                    node_catch_clause
                        .ref_(self)
                        .as_catch_clause()
                        .variable_declaration
                        .unwrap(),
                );
                self.transform_and_emit_embedded_statement(
                    node_catch_clause.ref_(self).as_catch_clause().block,
                );
            }

            if let Some(node_finally_block) =
                released!(node.ref_(self).as_try_statement().finally_block)
            {
                self.begin_finally_block();
                self.transform_and_emit_embedded_statement(node_finally_block);
            }

            self.end_exception_block();
        } else {
            self.emit_statement(visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            ));
        }
    }

    pub(super) fn contains_yield(&self, node: Option<Id<Node>>) -> bool {
        node.matches(|node| {
            node.ref_(self)
                .transform_flags()
                .intersects(TransformFlags::ContainsYield)
        })
    }

    pub(super) fn count_initial_nodes_without_yield(
        &self,
        nodes: Id<NodeArray>, /*<Node>*/
    ) -> Option<usize> {
        nodes
            .ref_(self)
            .iter()
            .position(|&node| self.contains_yield(Some(node)))
    }

    pub(super) fn cache_expression(
        &self,
        node: Id<Node>, /*Expression*/
    ) -> Id<Node /*Identifier*/> {
        if is_generated_identifier(&node.ref_(self))
            || get_emit_flags(node, self).intersects(EmitFlags::HelperName)
        {
            return node;
        }

        let temp = self.factory.ref_(self).create_temp_variable(
            Some(|node: Id<Node>| {
                self.context.ref_(self).hoist_variable_declaration(node);
            }),
            None,
        );
        self.emit_assignment(temp, node, Some(&*node.ref_(self)));
        temp
    }

    pub(super) fn declare_local(&self, name: Option<&str>) -> Id<Node /*Identifier*/> {
        let temp = name.map_or_else(
            || {
                self.factory
                    .ref_(self)
                    .create_temp_variable(Option::<fn(Id<Node>)>::None, None)
            },
            |name| self.factory.ref_(self).create_unique_name(name, None),
        );
        self.context.ref_(self).hoist_variable_declaration(temp);
        temp
    }
}
