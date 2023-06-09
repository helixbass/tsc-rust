use std::borrow::Borrow;

use gc::Gc;

use super::TransformGenerators;
use crate::{
    id_text, is_expression, is_statement, is_variable_declaration_list, visit_each_child,
    visit_node, NamedDeclarationInterface, Node, NodeArray, NodeInterface, Number, VisitResult,
};

impl TransformGenerators {
    pub(super) fn transform_and_emit_for_in_statement(&self, node: &Node /*ForInStatement*/) {
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
                    .create_array_literal_expression(Option::<Gc<NodeArray>>::None, None)
                    .wrap(),
                Option::<&Node>::None,
            );

            self.emit_statement(
                self.factory
                    .create_for_in_statement(
                        key.clone(),
                        visit_node(
                            &node_as_for_in_statement.expression,
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_expression),
                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                        ),
                        self.factory
                            .create_expression_statement(
                                self.factory
                                    .create_call_expression(
                                        self.factory
                                            .create_property_access_expression(
                                                keys_array.clone(),
                                                "push",
                                            )
                                            .wrap(),
                                        Option::<Gc<NodeArray>>::None,
                                        Some(vec![key.clone()]),
                                    )
                                    .wrap(),
                            )
                            .wrap(),
                    )
                    .wrap(),
            );

            self.emit_assignment(
                keys_index.clone(),
                self.factory
                    .create_numeric_literal(Number::new(0.0), None)
                    .wrap(),
                Option::<&Node>::None,
            );

            let condition_label = self.define_label();
            let increment_label = self.define_label();
            let end_label = self.begin_loop_block(increment_label);

            self.mark_label(condition_label);
            self.emit_break_when_false(
                end_label,
                self.factory
                    .create_less_than(
                        keys_index.clone(),
                        self.factory
                            .create_property_access_expression(keys_array.clone(), "length")
                            .wrap(),
                    )
                    .wrap(),
                Option::<&Node>::None,
            );

            let variable: Gc<Node /*Expression*/>;
            if is_variable_declaration_list(initializer) {
                let initializer_as_variable_declaration_list =
                    initializer.as_variable_declaration_list();
                for variable in &initializer_as_variable_declaration_list.declarations {
                    self.context
                        .hoist_variable_declaration(&variable.as_variable_declaration().name());
                }

                variable = self.factory.clone_node(
                    &initializer_as_variable_declaration_list.declarations[0]
                        .as_variable_declaration()
                        .name(),
                );
            } else {
                variable = visit_node(
                    initializer,
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                );
            }

            self.emit_assignment(
                variable,
                self.factory
                    .create_element_access_expression(keys_array, keys_index.clone())
                    .wrap(),
                Option::<&Node>::None,
            );
            self.transform_and_emit_embedded_statement(&node_as_for_in_statement.statement);

            self.mark_label(increment_label);
            self.emit_statement(
                self.factory
                    .create_expression_statement(
                        self.factory.create_postfix_increment(keys_index).wrap(),
                    )
                    .wrap(),
            );

            self.emit_break(condition_label, Option::<&Node>::None);
            self.end_loop_block();
        } else {
            self.emit_statement(visit_node(
                node,
                Some(|node: &Node| self.visitor(node)),
                Some(is_statement),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            ));
        }
    }

    pub(super) fn visit_for_in_statement(
        &self,
        node: &Node, /*ForInStatement*/
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
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                ),
                visit_node(
                    &node.as_for_in_statement().statement,
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_statement),
                    Some(&|nodes: &[Gc<Node>]| self.factory.lift_to_block(nodes)),
                ),
            );
        } else {
            node = visit_each_child(&node, |node: &Node| self.visitor(node), &**self.context);
        }

        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.end_loop_block();
        }

        Some(node.into())
    }

    pub(super) fn transform_and_emit_continue_statement(
        &self,
        node: &Node, /*ContinueStatement*/
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
        node: &Node, /*ContinueStatement*/
    ) -> Gc<Node /*Statement*/> {
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

        visit_each_child(&node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn transform_and_emit_break_statement(&self, _node: &Node /*BreakStatement*/) {
        unimplemented!()
    }

    pub(super) fn visit_break_statement(
        &self,
        _node: &Node, /*BreakStatement*/
    ) -> Gc<Node /*Statement*/> {
        unimplemented!()
    }

    pub(super) fn transform_and_emit_return_statement(
        &self,
        _node: &Node, /*ReturnStatement*/
    ) {
        unimplemented!()
    }

    pub(super) fn visit_return_statement(
        &self,
        _node: &Node, /*ReturnStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn transform_and_emit_with_statement(&self, _node: &Node /*WithStatement*/) {
        unimplemented!()
    }

    pub(super) fn transform_and_emit_switch_statement(
        &self,
        _node: &Node, /*SwitchStatement*/
    ) {
        unimplemented!()
    }

    pub(super) fn visit_switch_statement(
        &self,
        _node: &Node, /*SwitchStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn transform_and_emit_labeled_statement(
        &self,
        _node: &Node, /*LabeledStatement*/
    ) {
        unimplemented!()
    }

    pub(super) fn visit_labeled_statement(
        &self,
        _node: &Node, /*LabeledStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn transform_and_emit_throw_statement(&self, _node: &Node /*ThrowStatement*/) {
        unimplemented!()
    }

    pub(super) fn transform_and_emit_try_statement(&self, _node: &Node /*TryStatement*/) {
        unimplemented!()
    }

    pub(super) fn contains_yield(&self, _node: Option<impl Borrow<Node>>) -> bool {
        unimplemented!()
    }

    pub(super) fn count_initial_nodes_without_yield(
        &self,
        _nodes: &NodeArray, /*<Node>*/
    ) -> usize {
        unimplemented!()
    }

    pub(super) fn cache_expression(
        &self,
        _node: &Node, /*Expression*/
    ) -> Gc<Node /*Identifier*/> {
        unimplemented!()
    }

    pub(super) fn declare_local(&self, _name: Option<&str>) -> Gc<Node /*Identifier*/> {
        unimplemented!()
    }
}
