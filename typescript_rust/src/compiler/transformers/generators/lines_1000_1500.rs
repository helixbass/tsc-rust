use gc::Gc;

use super::TransformGenerators;
use crate::{
    Node, VecExt, VisitResult, _d, create_expression_for_object_literal_element_like,
    for_each_bool, is_expression, is_import_call, is_left_hand_side_expression,
    is_object_literal_element_like, maybe_for_each_bool, maybe_visit_node, reduce_left,
    start_on_new_line, visit_each_child, visit_node, visit_nodes, CallBinding, NodeArray, NodeExt,
    NodeInterface,
};

impl TransformGenerators {
    pub(super) fn reduce_element(
        &self,
        temp: &mut Option<Gc<Node>>,
        multi_line: Option<bool>,
        leading_element: &mut Option<Gc<Node>>,
        mut expressions: Vec<Gc<Node>>,
        element: &Node,
    ) -> Vec<Gc<Node>> {
        if self.contains_yield(Some(element)) && !expressions.is_empty() {
            let has_assigned_temp = temp.is_some();
            if temp.is_none() {
                *temp = Some(self.declare_local(None));
            }

            self.emit_assignment(
                temp.clone().unwrap(),
                if has_assigned_temp {
                    self.factory.create_array_concat_call(
                        temp.clone().unwrap(),
                        vec![self
                            .factory
                            .create_array_literal_expression(Some(expressions), multi_line)
                            .wrap()],
                    )
                } else {
                    self.factory
                        .create_array_literal_expression(
                            Some(if let Some(leading_element) = leading_element.as_ref() {
                                vec![leading_element.clone()].and_extend(expressions)
                            } else {
                                expressions
                            }),
                            multi_line,
                        )
                        .wrap()
                },
                Option::<&Node>::None,
            );
            *leading_element = None;
            expressions = _d();
        }

        expressions.push(visit_node(
            element,
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        ));
        expressions
    }

    pub(super) fn visit_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) -> VisitResult {
        let node_as_object_literal_expression = node.as_object_literal_expression();
        let properties = &node_as_object_literal_expression.properties;
        let multi_line = node_as_object_literal_expression.multi_line;
        let num_initial_properties = self.count_initial_nodes_without_yield(properties);

        let temp = self.declare_local(None);
        self.emit_assignment(
            temp.clone(),
            self.factory
                .create_object_literal_expression(
                    Some(visit_nodes(
                        properties,
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_object_literal_element_like),
                        Some(0),
                        Some(num_initial_properties),
                    )),
                    multi_line,
                )
                .wrap(),
            Option::<&Node>::None,
        );

        let mut expressions = reduce_left(
            properties,
            |expressions: Vec<Gc<Node>>, property: &Gc<Node>, _| {
                self.reduce_property(&temp, multi_line, node, expressions, property)
            },
            _d(), /*as Expression[]*/
            Some(num_initial_properties),
            None,
        );
        expressions.push(if multi_line == Some(true) {
            self.factory
                .clone_node(&temp)
                .set_text_range(Some(&*temp))
                .and_set_parent(temp.maybe_parent())
                .start_on_new_line()
        } else {
            temp
        });
        Some(self.factory.inline_expressions(&expressions).into())
    }

    pub(super) fn reduce_property(
        &self,
        temp: &Node,
        multi_line: Option<bool>,
        node: &Node,
        mut expressions: Vec<Gc<Node>>,
        property: &Node,
    ) -> Vec<Gc<Node>> {
        if self.contains_yield(Some(property)) && !expressions.is_empty() {
            self.emit_statement(
                self.factory
                    .create_expression_statement(self.factory.inline_expressions(&expressions))
                    .wrap(),
            );
            expressions = _d();
        }

        let expression =
            create_expression_for_object_literal_element_like(&self.factory, node, property, temp);
        let visited = maybe_visit_node(
            expression.as_deref(),
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        );
        if let Some(visited) = visited {
            if multi_line == Some(true) {
                start_on_new_line(&*visited);
            }
            expressions.push(visited);
        }
        expressions
    }

    pub(super) fn visit_element_access_expression(
        &self,
        node: &Node, /*ElementAccessExpression*/
    ) -> VisitResult {
        let node_as_element_access_expression = node.as_element_access_expression();
        if self.contains_yield(Some(
            &*node_as_element_access_expression.argument_expression,
        )) {
            return Some(
                self.factory
                    .update_element_access_expression(
                        node,
                        self.cache_expression(&visit_node(
                            &node_as_element_access_expression.expression,
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_left_hand_side_expression),
                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                        )),
                        visit_node(
                            &node_as_element_access_expression.argument_expression,
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_expression),
                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                        ),
                    )
                    .into(),
            );
        }

        Some(visit_each_child(node, |node: &Node| self.visitor(node), &**self.context).into())
    }

    pub(super) fn visit_call_expression(&self, node: &Node /*CallExpression*/) -> VisitResult {
        let node_as_call_expression = node.as_call_expression();
        if !is_import_call(node)
            && for_each_bool(
                &node_as_call_expression.arguments,
                |argument: &Gc<Node>, _| self.contains_yield(Some(&**argument)),
            )
        {
            let CallBinding { target, this_arg } = self.factory.create_call_binding(
                &node_as_call_expression.expression,
                |node: &Node| {
                    self.context.hoist_variable_declaration(node);
                },
                Some(self.language_version),
                Some(true),
            );
            return Some(
                self.factory
                    .create_function_apply_call(
                        self.cache_expression(&visit_node(
                            &target,
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_left_hand_side_expression),
                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                        )),
                        this_arg,
                        self.visit_elements(
                            &node_as_call_expression.arguments,
                            Option::<&Node>::None,
                            Option::<&Node>::None,
                            None,
                        ),
                    )
                    .set_text_range(Some(node))
                    .set_original_node(Some(node.node_wrapper()))
                    .into(),
            );
        }

        Some(visit_each_child(node, |node: &Node| self.visitor(node), &**self.context).into())
    }

    pub(super) fn visit_new_expression(&self, node: &Node /*NewExpression*/) -> VisitResult {
        let node_as_new_expression = node.as_new_expression();
        if maybe_for_each_bool(
            node_as_new_expression.arguments.as_deref(),
            |argument: &Gc<Node>, _| self.contains_yield(Some(&**argument)),
        ) {
            let CallBinding { target, this_arg } = self.factory.create_call_binding(
                &self
                    .factory
                    .create_property_access_expression(
                        node_as_new_expression.expression.clone(),
                        "bind",
                    )
                    .wrap(),
                |node: &Node| {
                    self.context.hoist_variable_declaration(node);
                },
                None,
                None,
            );
            return Some(
                self.factory
                    .create_new_expression(
                        self.factory.create_function_apply_call(
                            self.cache_expression(&visit_node(
                                &target,
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_expression),
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            )),
                            this_arg,
                            self.visit_elements(
                                node_as_new_expression.arguments.as_ref().unwrap(),
                                Some(self.factory.create_void_zero()),
                                Option::<&Node>::None,
                                None,
                            ),
                        ),
                        Option::<Gc<NodeArray>>::None,
                        Some(vec![]),
                    )
                    .wrap()
                    .set_text_range(Some(node))
                    .set_original_node(Some(node.node_wrapper()))
                    .into(),
            );
        }

        Some(visit_each_child(node, |node: &Node| self.visitor(node), &**self.context).into())
    }

    pub(super) fn transform_and_emit_statements(
        &self,
        _statements: &[Gc<Node /*Statement*/>],
        start: Option<usize>,
    ) {
        let _start = start.unwrap_or(0);
        unimplemented!()
    }

    pub(super) fn transform_and_emit_variable_declaration_list(
        &self,
        _node: &Node, /*VariableDeclarationList*/
    ) -> Option<Gc<Node /*VariableDeclarationList*/>> {
        unimplemented!()
    }

    pub(super) fn transform_initialized_variable(
        &self,
        _node: &Node, /*InitializedVariableDeclaration*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn visit_do_statement(&self, _node: &Node /*DoStatement*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_while_statement(
        &self,
        _node: &Node, /*WhileStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_for_statement(&self, _node: &Node /*ForStatement*/) -> VisitResult {
        unimplemented!()
    }
}
