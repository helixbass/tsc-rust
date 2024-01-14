use gc::Gc;
use id_arena::Id;

use super::TransformGenerators;
use crate::{
    Node, VecExt, VisitResult, _d, create_expression_for_object_literal_element_like,
    for_each_bool, get_initialized_variables, is_block, is_expression, is_import_call,
    is_left_hand_side_expression, is_object_literal_element_like, is_statement,
    is_variable_declaration_list, map, maybe_for_each_bool, maybe_visit_node, reduce_left,
    start_on_new_line, visit_each_child, visit_iteration_body, visit_node, visit_nodes,
    CallBinding, HasInitializerInterface, NamedDeclarationInterface, NodeArray, NodeExt,
    NodeInterface, SyntaxKind,
};

impl TransformGenerators {
    pub(super) fn reduce_element(
        &self,
        temp: &mut Option<Id<Node>>,
        multi_line: Option<bool>,
        leading_element: &mut Option<Id<Node>>,
        mut expressions: Vec<Id<Node>>,
        element: Id<Node>,
    ) -> Vec<Id<Node>> {
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
                            .create_array_literal_expression(Some(expressions), multi_line)],
                    )
                } else {
                    self.factory.create_array_literal_expression(
                        Some(if let Some(leading_element) = leading_element.as_ref() {
                            vec![leading_element.clone()].and_extend(expressions)
                        } else {
                            expressions
                        }),
                        multi_line,
                    )
                },
                Option::<Id<Node>>::None,
            );
            *leading_element = None;
            expressions = _d();
        }

        expressions.push(visit_node(
            element,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        ));
        expressions
    }

    pub(super) fn visit_object_literal_expression(
        &self,
        node: Id<Node>, /*ObjectLiteralExpression*/
    ) -> VisitResult {
        let node_as_object_literal_expression = node.as_object_literal_expression();
        let properties = &node_as_object_literal_expression.properties;
        let multi_line = node_as_object_literal_expression.multi_line;
        let num_initial_properties = self
            .count_initial_nodes_without_yield(properties)
            // TODO: this sort of looks like a bug upstream that it could pass -1 to visitNodes()
            // below? Shocking that using the same type to represent the absence of a value could
            // result in such a thing
            // So unwrapping for the moment
            .unwrap();

        let temp = self.declare_local(None);
        self.emit_assignment(
            temp.clone(),
            self.factory.create_object_literal_expression(
                Some(visit_nodes(
                    properties,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_object_literal_element_like),
                    Some(0),
                    Some(num_initial_properties),
                )),
                multi_line,
            ),
            Option::<Id<Node>>::None,
        );

        let mut expressions = reduce_left(
            properties,
            |expressions: Vec<Id<Node>>, property: &Id<Node>, _| {
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
        temp: Id<Node>,
        multi_line: Option<bool>,
        node: Id<Node>,
        mut expressions: Vec<Id<Node>>,
        property: Id<Node>,
    ) -> Vec<Id<Node>> {
        if self.contains_yield(Some(property)) && !expressions.is_empty() {
            self.emit_statement(
                self.factory
                    .create_expression_statement(self.factory.inline_expressions(&expressions)),
            );
            expressions = _d();
        }

        let expression =
            create_expression_for_object_literal_element_like(&self.factory, node, property, temp, self);
        let visited = maybe_visit_node(
            expression.as_deref(),
            Some(|node: Id<Node>| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
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
        node: Id<Node>, /*ElementAccessExpression*/
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
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(is_left_hand_side_expression),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        )),
                        visit_node(
                            &node_as_element_access_expression.argument_expression,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(is_expression),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        ),
                    )
                    .into(),
            );
        }

        Some(visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context).into())
    }

    pub(super) fn visit_call_expression(
        &self,
        node: Id<Node>, /*CallExpression*/
    ) -> VisitResult {
        let node_as_call_expression = node.as_call_expression();
        if !is_import_call(node)
            && for_each_bool(
                &node_as_call_expression.arguments,
                |argument: &Id<Node>, _| self.contains_yield(Some(&**argument)),
            )
        {
            let CallBinding { target, this_arg } = self.factory.create_call_binding(
                &node_as_call_expression.expression,
                |node: Id<Node>| {
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
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(is_left_hand_side_expression),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        )),
                        this_arg,
                        self.visit_elements(
                            &node_as_call_expression.arguments,
                            Option::<Id<Node>>::None,
                            Option::<Id<Node>>::None,
                            None,
                        ),
                    )
                    .set_text_range(Some(node))
                    .set_original_node(Some(node.node_wrapper()))
                    .into(),
            );
        }

        Some(visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context).into())
    }

    pub(super) fn visit_new_expression(
        &self,
        node: Id<Node>, /*NewExpression*/
    ) -> VisitResult {
        let node_as_new_expression = node.as_new_expression();
        if maybe_for_each_bool(
            node_as_new_expression.arguments.as_deref(),
            |argument: &Id<Node>, _| self.contains_yield(Some(&**argument)),
        ) {
            let CallBinding { target, this_arg } = self.factory.create_call_binding(
                &self.factory.create_property_access_expression(
                    node_as_new_expression.expression.clone(),
                    "bind",
                ),
                |node: Id<Node>| {
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
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(is_expression),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            )),
                            this_arg,
                            self.visit_elements(
                                node_as_new_expression.arguments.as_ref().unwrap(),
                                Some(self.factory.create_void_zero()),
                                Option::<Id<Node>>::None,
                                None,
                            ),
                        ),
                        Option::<Gc<NodeArray>>::None,
                        Some(vec![]),
                    )
                    .set_text_range(Some(node))
                    .set_original_node(Some(node.node_wrapper()))
                    .into(),
            );
        }

        Some(visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context).into())
    }

    pub(super) fn transform_and_emit_statements(
        &self,
        statements: &[Id<Node /*Statement*/>],
        start: Option<usize>,
    ) {
        let start = start.unwrap_or(0);
        for statement in statements.into_iter().skip(start) {
            self.transform_and_emit_statement(statement);
        }
    }

    pub(super) fn transform_and_emit_embedded_statement(&self, node: Id<Node>) {
        if is_block(node) {
            self.transform_and_emit_statements(&node.as_block().statements, None);
        } else {
            self.transform_and_emit_statement(node);
        }
    }

    pub(super) fn transform_and_emit_statement(&self, node: Id<Node>) {
        let saved_in_statement_containing_yield = self.maybe_in_statement_containing_yield();
        if self.maybe_in_statement_containing_yield() != Some(true) {
            self.set_in_statement_containing_yield(Some(self.contains_yield(Some(node))));
        }

        self.transform_and_emit_statement_worker(node);
        self.set_in_statement_containing_yield(saved_in_statement_containing_yield);
    }

    pub(super) fn transform_and_emit_statement_worker(&self, node: Id<Node>) {
        match node.kind() {
            SyntaxKind::Block => self.transform_and_emit_block(node),
            SyntaxKind::ExpressionStatement => self.transform_and_emit_expression_statement(node),
            SyntaxKind::IfStatement => self.transform_and_emit_if_statement(node),
            SyntaxKind::DoStatement => self.transform_and_emit_do_statement(node),
            SyntaxKind::WhileStatement => self.transform_and_emit_while_statement(node),
            SyntaxKind::ForStatement => self.transform_and_emit_for_statement(node),
            SyntaxKind::ForInStatement => self.transform_and_emit_for_in_statement(node),
            SyntaxKind::ContinueStatement => self.transform_and_emit_continue_statement(node),
            SyntaxKind::BreakStatement => self.transform_and_emit_break_statement(node),
            SyntaxKind::ReturnStatement => self.transform_and_emit_return_statement(node),
            SyntaxKind::WithStatement => self.transform_and_emit_with_statement(node),
            SyntaxKind::SwitchStatement => self.transform_and_emit_switch_statement(node),
            SyntaxKind::LabeledStatement => self.transform_and_emit_labeled_statement(node),
            SyntaxKind::ThrowStatement => self.transform_and_emit_throw_statement(node),
            SyntaxKind::TryStatement => self.transform_and_emit_try_statement(node),
            _ => self.emit_statement(visit_node(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_statement),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )),
        }
    }

    pub(super) fn transform_and_emit_block(&self, node: Id<Node> /*Block*/) {
        let node_as_block = node.as_block();
        if self.contains_yield(Some(node)) {
            self.transform_and_emit_statements(&node_as_block.statements, None);
        } else {
            self.emit_statement(visit_node(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_statement),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ));
        }
    }

    pub(super) fn transform_and_emit_expression_statement(
        &self,
        node: Id<Node>, /*ExpressionStatement*/
    ) {
        self.emit_statement(visit_node(
            node,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(is_statement),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        ));
    }

    pub(super) fn transform_and_emit_variable_declaration_list(
        &self,
        node: Id<Node>, /*VariableDeclarationList*/
    ) -> Option<Id<Node /*VariableDeclarationList*/>> {
        let node_as_variable_declaration_list = node.as_variable_declaration_list();
        for variable in &node_as_variable_declaration_list.declarations {
            let variable_as_variable_declaration = variable.as_variable_declaration();
            let name = self
                .factory
                .clone_node(&variable_as_variable_declaration.name())
                .set_comment_range(&*variable_as_variable_declaration.name());
            self.context.hoist_variable_declaration(&name);
        }

        let variables = get_initialized_variables(node);
        let num_variables = variables.len();
        let mut variables_written = 0;
        let mut pending_expressions: Vec<Id<Node /*Expression*/>> = _d();
        while variables_written < num_variables {
            for variable in variables
                .iter()
                .skip(variables_written)
                .take(num_variables - variables_written)
            {
                let variable_as_variable_declaration = variable.as_variable_declaration();
                if self.contains_yield(variable_as_variable_declaration.maybe_initializer())
                    && !pending_expressions.is_empty()
                {
                    break;
                }

                pending_expressions.push(self.transform_initialized_variable(variable));
            }

            if !pending_expressions.is_empty() {
                self.emit_statement(self.factory.create_expression_statement(
                    self.factory.inline_expressions(&pending_expressions),
                ));
                variables_written += pending_expressions.len();
                pending_expressions = _d();
            }
        }

        None
    }

    pub(super) fn transform_initialized_variable(
        &self,
        node: Id<Node>, /*InitializedVariableDeclaration*/
    ) -> Id<Node> {
        let node_as_variable_declaration = node.as_variable_declaration();
        self.factory
            .create_assignment(
                self.factory
                    .clone_node(&node_as_variable_declaration.name())
                    .set_source_map_range(Some((&*node_as_variable_declaration.name()).into())),
                visit_node(
                    &node_as_variable_declaration.maybe_initializer().unwrap(),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
            )
            .set_source_map_range(Some(node.into()))
    }

    pub(super) fn transform_and_emit_if_statement(&self, node: Id<Node> /*IfStatement*/) {
        let node_as_if_statement = node.as_if_statement();
        if self.contains_yield(Some(node)) {
            if self.contains_yield(Some(&*node_as_if_statement.then_statement))
                || self.contains_yield(node_as_if_statement.else_statement.as_deref())
            {
                let end_label = self.define_label();
                let else_label = node_as_if_statement
                    .else_statement
                    .as_ref()
                    .map(|_| self.define_label());
                self.emit_break_when_false(
                    node_as_if_statement
                        .else_statement
                        .as_ref()
                        .map_or(end_label, |_| else_label.unwrap()),
                    visit_node(
                        &node_as_if_statement.expression,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                    Some(&*node_as_if_statement.expression),
                );
                self.transform_and_emit_embedded_statement(&node_as_if_statement.then_statement);
                if let Some(node_else_statement) = node_as_if_statement.else_statement.as_ref() {
                    self.emit_break(end_label, Option::<Id<Node>>::None);
                    self.mark_label(else_label.unwrap());
                    self.transform_and_emit_embedded_statement(node_else_statement);
                }
                self.mark_label(end_label);
            } else {
                self.emit_statement(visit_node(
                    node,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_statement),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ));
            }
        } else {
            self.emit_statement(visit_node(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_statement),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ));
        }
    }

    pub(super) fn transform_and_emit_do_statement(&self, node: Id<Node> /*DoStatement*/) {
        let node_as_do_statement = node.as_do_statement();
        if self.contains_yield(Some(node)) {
            let condition_label = self.define_label();
            let loop_label = self.define_label();
            self.begin_loop_block(condition_label);
            self.mark_label(loop_label);
            self.transform_and_emit_embedded_statement(&node_as_do_statement.statement);
            self.mark_label(condition_label);
            self.emit_break_when_true(
                loop_label,
                visit_node(
                    &node_as_do_statement.expression,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
                Option::<Id<Node>>::None,
            );
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

    pub(super) fn visit_do_statement(&self, node: Id<Node> /*DoStatement*/) -> VisitResult {
        let mut node = node.node_wrapper();
        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.begin_script_loop_block();
            node = visit_each_child(&node, |node: Id<Node>| self.visitor(node), &**self.context);
            self.end_loop_block();
            Some(node.into())
        } else {
            Some(
                visit_each_child(&node, |node: Id<Node>| self.visitor(node), &**self.context)
                    .into(),
            )
        }
    }

    pub(super) fn transform_and_emit_while_statement(
        &self,
        node: Id<Node>, /*WhileStatement*/
    ) {
        let node_as_while_statement = node.as_while_statement();
        if self.contains_yield(Some(node)) {
            let loop_label = self.define_label();
            let end_label = self.begin_loop_block(loop_label);
            self.mark_label(loop_label);
            self.emit_break_when_false(
                end_label,
                visit_node(
                    &node_as_while_statement.expression,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
                Option::<Id<Node>>::None,
            );
            self.transform_and_emit_embedded_statement(&node_as_while_statement.statement);
            self.emit_break(loop_label, Option::<Id<Node>>::None);
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

    pub(super) fn visit_while_statement(
        &self,
        node: Id<Node>, /*WhileStatement*/
    ) -> VisitResult {
        let mut node = node.node_wrapper();
        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.begin_script_loop_block();
            node = visit_each_child(&node, |node: Id<Node>| self.visitor(node), &**self.context);
            self.end_loop_block();
            Some(node.into())
        } else {
            Some(
                visit_each_child(&node, |node: Id<Node>| self.visitor(node), &**self.context)
                    .into(),
            )
        }
    }

    pub(super) fn transform_and_emit_for_statement(&self, node: Id<Node> /*ForStatement*/) {
        let node_as_for_statement = node.as_for_statement();
        if self.contains_yield(Some(node)) {
            let condition_label = self.define_label();
            let increment_label = self.define_label();
            let end_label = self.begin_loop_block(increment_label);
            if let Some(node_initializer) = node_as_for_statement.initializer.as_ref() {
                let initializer = node_initializer;
                if is_variable_declaration_list(initializer) {
                    self.transform_and_emit_variable_declaration_list(initializer);
                } else {
                    self.emit_statement(
                        self.factory
                            .create_expression_statement(visit_node(
                                initializer,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(is_expression),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ))
                            .set_text_range(Some(&**initializer)),
                    );
                }
            }

            self.mark_label(condition_label);
            if let Some(node_condition) = node_as_for_statement.condition.as_ref() {
                self.emit_break_when_false(
                    end_label,
                    visit_node(
                        node_condition,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                    Option::<Id<Node>>::None,
                );
            }

            self.transform_and_emit_embedded_statement(&node_as_for_statement.statement);

            self.mark_label(increment_label);
            if let Some(node_incrementor) = node_as_for_statement.incrementor.as_ref() {
                self.emit_statement(
                    self.factory
                        .create_expression_statement(visit_node(
                            node_incrementor,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(is_expression),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        ))
                        .set_text_range(Some(&**node_incrementor)),
                );
            }
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

    pub(super) fn visit_for_statement(&self, node: Id<Node> /*ForStatement*/) -> VisitResult {
        let node_as_for_statement = node.as_for_statement();
        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.begin_script_loop_block();
        }

        let initializer = node_as_for_statement.initializer.as_ref();
        let mut node = node.node_wrapper();
        if let Some(initializer) =
            initializer.filter(|initializer| is_variable_declaration_list(initializer))
        {
            for variable in &initializer.as_variable_declaration_list().declarations {
                self.context
                    .hoist_variable_declaration(&variable.as_variable_declaration().name());
            }

            let variables = get_initialized_variables(initializer);
            node = self.factory.update_for_statement(
                &node,
                (!variables.is_empty()).then(|| {
                    self.factory
                        .inline_expressions(&map(&variables, |variable: &Id<Node>, _| {
                            self.transform_initialized_variable(variable)
                        }))
                }),
                maybe_visit_node(
                    node_as_for_statement.condition.as_deref(),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
                maybe_visit_node(
                    node_as_for_statement.incrementor.as_deref(),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
                visit_iteration_body(
                    &node_as_for_statement.statement,
                    |node: Id<Node>| self.visitor(node),
                    &**self.context,
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
}
