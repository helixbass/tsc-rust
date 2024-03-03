use id_arena::Id;

use super::{OpCode, TransformGenerators};
use crate::{
    visit_each_child, Node, NodeExt, NodeInterface, VisitResult, _d, get_emit_flags,
    get_expression_associativity, get_initialized_variables,
    get_non_assignment_operator_for_compound_assignment, insert_statements_after_standard_prologue,
    is_binary_expression, is_compound_assignment, is_expression, is_left_hand_side_expression,
    is_logical_operator, map, maybe_visit_node, reduce_left, released, visit_node, visit_nodes,
    Associativity, CoreTransformationContext, EmitFlags, HasArena, InArena, IntoA,
    NamedDeclarationInterface, NodeArray, NodeArrayOrVec, ReadonlyTextRange, SyntaxKind,
    TransformFlags, VecExt,
};

impl TransformGenerators {
    pub(super) fn visit_accessor_declaration(
        &self,
        node: Id<Node>, /*AccessorDeclaration*/
    ) -> VisitResult {
        let saved_in_generator_function_body = self.maybe_in_generator_function_body();
        let saved_in_statement_containing_yield = self.maybe_in_statement_containing_yield();
        self.set_in_generator_function_body(Some(false));
        self.set_in_statement_containing_yield(Some(false));
        let node = visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        );
        self.set_in_generator_function_body(saved_in_generator_function_body);
        self.set_in_statement_containing_yield(saved_in_statement_containing_yield);
        Some(node.into())
    }

    pub(super) fn transform_generator_function_body(
        &self,
        body: Id<Node>, /*Block*/
    ) -> Id<Node> {
        let mut statements: Vec<Id<Node /*Statement*/>> = _d();
        let saved_in_generator_function_body = self.maybe_in_generator_function_body();
        let saved_in_statement_containing_yield = self.maybe_in_statement_containing_yield();
        let saved_blocks = self.maybe_blocks().clone();
        let saved_block_offsets = self.maybe_block_offsets().clone();
        let saved_block_actions = self.maybe_block_actions().clone();
        let saved_block_stack = self.maybe_block_stack().clone();
        let saved_label_offsets = self.maybe_label_offsets().clone();
        let saved_label_expressions = self.maybe_label_expressions().clone();
        let saved_next_label_id = self.next_label_id();
        let saved_operations = self.maybe_operations().clone();
        let saved_operation_arguments = self.maybe_operation_arguments().clone();
        let saved_operation_locations = self.maybe_operation_locations().clone();
        let saved_state = self.maybe_state();

        self.set_in_generator_function_body(Some(true));
        self.set_in_statement_containing_yield(Some(false));
        self.set_blocks(None);
        self.set_block_offsets(None);
        self.set_block_actions(None);
        self.set_block_stack(None);
        self.set_label_offsets(None);
        self.set_label_expressions(None);
        self.set_next_label_id(1);
        self.set_operations(None);
        self.set_operation_arguments(None);
        self.set_operation_locations(None);
        self.set_state(Some(
            self.factory
                .ref_(self)
                .create_temp_variable(Option::<fn(Id<Node>)>::None, None),
        ));

        self.context.ref_(self).resume_lexical_environment();

        let statement_offset = self.factory.ref_(self).copy_prologue(
            &body.ref_(self).as_block().statements.ref_(self),
            &mut statements,
            Some(false),
            Some(|node: Id<Node>| self.visitor(node)),
        );

        self.transform_and_emit_statements(
            &released!(body.ref_(self).as_block().statements.ref_(self).clone()),
            Some(statement_offset),
        );

        let build_result = self.build();
        insert_statements_after_standard_prologue(
            &mut statements,
            self.context.ref_(self).end_lexical_environment().as_deref(),
            self,
        );
        statements.push(
            self.factory
                .ref_(self)
                .create_return_statement(Some(build_result)),
        );

        self.set_in_generator_function_body(saved_in_generator_function_body);
        self.set_in_statement_containing_yield(saved_in_statement_containing_yield);
        self.set_blocks(saved_blocks);
        self.set_block_offsets(saved_block_offsets);
        self.set_block_actions(saved_block_actions);
        self.set_block_stack(saved_block_stack);
        self.set_label_offsets(saved_label_offsets);
        self.set_label_expressions(saved_label_expressions);
        self.set_next_label_id(saved_next_label_id);
        self.set_operations(saved_operations);
        self.set_operation_arguments(saved_operation_arguments);
        self.set_operation_locations(saved_operation_locations);
        self.set_state(saved_state);

        self.factory
            .ref_(self)
            .create_block(statements, released!(body.ref_(self).as_block().multi_line))
            .set_text_range(Some(&*body.ref_(self)), self)
    }

    pub(super) fn visit_variable_statement(
        &self,
        node: Id<Node>, /*VariableStatement*/
    ) -> Option<Id<Node /*Statement*/>> {
        if node
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsYield)
        {
            self.transform_and_emit_variable_declaration_list(released!(
                node.ref_(self).as_variable_statement().declaration_list
            ));
            None
        } else {
            if get_emit_flags(node, self).intersects(EmitFlags::CustomPrologue) {
                return Some(node);
            }

            for &variable in &*node
                .ref_(self)
                .as_variable_statement()
                .declaration_list
                .ref_(self)
                .as_variable_declaration_list()
                .declarations
                .ref_(self)
            {
                self.context.ref_(self).hoist_variable_declaration(
                    variable.ref_(self).as_variable_declaration().name(),
                );
            }

            let variables = get_initialized_variables(
                node.ref_(self).as_variable_statement().declaration_list,
                self,
            );
            if variables.is_empty() {
                return None;
            }

            Some(
                self.factory
                    .ref_(self)
                    .create_expression_statement(
                        self.factory
                            .ref_(self)
                            .inline_expressions(&map(&variables, |&variable: &Id<Node>, _| {
                                self.transform_initialized_variable(variable)
                            })),
                    )
                    .set_source_map_range(
                        Some(self.alloc_source_map_range((&*node.ref_(self)).into())),
                        self,
                    ),
            )
        }
    }

    pub(super) fn visit_binary_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> Id<Node /*Expression*/> {
        let assoc = get_expression_associativity(node, self);
        match assoc {
            Associativity::Left => self.visit_left_associative_binary_expression(node),
            Associativity::Right => self.visit_right_associative_binary_expression(node),
            // default:
            //     return Debug.assertNever(assoc);
        }
    }

    pub(super) fn visit_right_associative_binary_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> Id<Node /*Expression*/> {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        let left = node_as_binary_expression.left;
        let right = node_as_binary_expression.right;
        if self.contains_yield(Some(right)) {
            let target: Id<Node /*Expression*/> = match left.ref_(self).kind() {
                SyntaxKind::PropertyAccessExpression => {
                    let left_ref = left.ref_(self);
                    let left_as_property_access_expression =
                        left_ref.as_property_access_expression();
                    self.factory.ref_(self).update_property_access_expression(
                        left,
                        self.cache_expression(visit_node(
                            left_as_property_access_expression.expression,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node| is_left_hand_side_expression(node, self)),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        )),
                        left_as_property_access_expression.name,
                    )
                }
                SyntaxKind::ElementAccessExpression => {
                    let left_ref = left.ref_(self);
                    let left_as_element_access_expression = left_ref.as_element_access_expression();
                    self.factory.ref_(self).update_element_access_expression(
                        left,
                        self.cache_expression(visit_node(
                            left_as_element_access_expression.expression,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node| is_left_hand_side_expression(node, self)),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        )),
                        self.cache_expression(visit_node(
                            left_as_element_access_expression.argument_expression,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node| is_expression(node, self)),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        )),
                    )
                }
                _ => visit_node(
                    left,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
            };

            let operator = node_as_binary_expression.operator_token.ref_(self).kind();
            if is_compound_assignment(operator) {
                return self
                    .factory
                    .ref_(self)
                    .create_assignment(
                        target.clone(),
                        self.factory
                            .ref_(self)
                            .create_binary_expression(
                                self.cache_expression(target),
                                get_non_assignment_operator_for_compound_assignment(operator),
                                visit_node(
                                    right,
                                    Some(|node: Id<Node>| self.visitor(node)),
                                    Some(|node| is_expression(node, self)),
                                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                ),
                            )
                            .set_text_range(Some(&*node.ref_(self)), self),
                    )
                    .set_text_range(Some(&*node.ref_(self)), self);
            } else {
                return self.factory.ref_(self).update_binary_expression(
                    node,
                    target,
                    node_as_binary_expression.operator_token.clone(),
                    visit_node(
                        right,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
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

    pub(super) fn visit_left_associative_binary_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> Id<Node /*Expression*/> {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        if self.contains_yield(Some(node_as_binary_expression.right)) {
            if is_logical_operator(node_as_binary_expression.operator_token.ref_(self).kind()) {
                return self.visit_logical_binary_expression(node);
            } else if node_as_binary_expression.operator_token.ref_(self).kind()
                == SyntaxKind::CommaToken
            {
                return self.visit_comma_expression(node);
            }

            return self.factory.ref_(self).update_binary_expression(
                node,
                self.cache_expression(visit_node(
                    node_as_binary_expression.left,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )),
                node_as_binary_expression.operator_token,
                visit_node(
                    node_as_binary_expression.right,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
            );
        }

        visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }

    pub(super) fn visit_comma_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        let mut pending_expressions: Vec<Id<Node /*Expression*/>> = _d();
        self.visit_comma_expression_visit(&mut pending_expressions, node_as_binary_expression.left);
        self.visit_comma_expression_visit(
            &mut pending_expressions,
            node_as_binary_expression.right,
        );
        self.factory
            .ref_(self)
            .inline_expressions(&pending_expressions)
    }

    pub(super) fn visit_comma_expression_visit(
        &self,
        pending_expressions: &mut Vec<Id<Node>>,
        node: Id<Node>, /*Expression*/
    ) {
        if is_binary_expression(&node.ref_(self))
            && node
                .ref_(self)
                .as_binary_expression()
                .operator_token
                .ref_(self)
                .kind()
                == SyntaxKind::CommaToken
        {
            let node_ref = node.ref_(self);
            let node_as_binary_expression = node_ref.as_binary_expression();
            self.visit_comma_expression_visit(pending_expressions, node_as_binary_expression.left);
            self.visit_comma_expression_visit(pending_expressions, node_as_binary_expression.right);
        } else {
            if self.contains_yield(Some(node)) && !pending_expressions.is_empty() {
                self.emit_worker(
                    OpCode::Statement,
                    Some(
                        self.factory
                            .ref_(self)
                            .create_expression_statement(
                                self.factory
                                    .ref_(self)
                                    .inline_expressions(pending_expressions),
                            )
                            .into(),
                    ),
                    Option::<&Node>::None,
                );
                *pending_expressions = _d();
            }

            pending_expressions.push(visit_node(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ));
        }
    }

    pub(super) fn visit_comma_list_expression(
        &self,
        node: Id<Node>, /*CommaListExpression*/
    ) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_comma_list_expression = node_ref.as_comma_list_expression();
        let mut pending_expressions: Vec<Id<Node /*Expression*/>> = _d();
        for &elem in &*node_as_comma_list_expression.elements.ref_(self) {
            if is_binary_expression(&elem.ref_(self))
                && elem
                    .ref_(self)
                    .as_binary_expression()
                    .operator_token
                    .ref_(self)
                    .kind()
                    == SyntaxKind::CommaToken
            {
                pending_expressions.push(self.visit_comma_expression(elem));
            } else {
                if self.contains_yield(Some(elem)) && !pending_expressions.is_empty() {
                    self.emit_worker(
                        OpCode::Statement,
                        Some(
                            self.factory
                                .ref_(self)
                                .create_expression_statement(
                                    self.factory
                                        .ref_(self)
                                        .inline_expressions(&pending_expressions),
                                )
                                .into(),
                        ),
                        Option::<&Node>::None,
                    );
                    pending_expressions = _d();
                }
                pending_expressions.push(visit_node(
                    elem,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ));
            }
        }
        Some(
            self.factory
                .ref_(self)
                .inline_expressions(&pending_expressions)
                .into(),
        )
    }

    pub(super) fn visit_logical_binary_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        let result_label = self.define_label();
        let result_local = self.declare_local(None);

        self.emit_assignment(
            result_local.clone(),
            visit_node(
                node_as_binary_expression.left,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ),
            Some(&*node_as_binary_expression.left.ref_(self)),
        );
        if node_as_binary_expression.operator_token.ref_(self).kind()
            == SyntaxKind::AmpersandAmpersandToken
        {
            self.emit_break_when_false(
                result_label,
                result_local.clone(),
                Some(&*node_as_binary_expression.left.ref_(self)),
            );
        } else {
            self.emit_break_when_true(
                result_label,
                result_local.clone(),
                Some(&*node_as_binary_expression.left.ref_(self)),
            );
        }

        self.emit_assignment(
            result_local.clone(),
            visit_node(
                node_as_binary_expression.right,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ),
            Some(&*node_as_binary_expression.right.ref_(self)),
        );
        self.mark_label(result_label);
        result_local
    }

    pub(super) fn visit_conditional_expression(
        &self,
        node: Id<Node>, /*ConditionalExpression*/
    ) -> Id<Node /*Expression*/> {
        let node_ref = node.ref_(self);
        let node_as_conditional_expression = node_ref.as_conditional_expression();
        if self.contains_yield(Some(node_as_conditional_expression.when_true))
            || self.contains_yield(Some(node_as_conditional_expression.when_false))
        {
            let when_false_label = self.define_label();
            let result_label = self.define_label();
            let result_local = self.declare_local(None);
            self.emit_break_when_false(
                when_false_label,
                visit_node(
                    node_as_conditional_expression.condition,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
                Some(&*node_as_conditional_expression.condition.ref_(self)),
            );
            self.emit_assignment(
                result_local.clone(),
                visit_node(
                    node_as_conditional_expression.when_true,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
                Some(&*node_as_conditional_expression.when_true.ref_(self)),
            );
            self.emit_break(result_label, Option::<&Node>::None);
            self.mark_label(when_false_label);
            self.emit_assignment(
                result_local.clone(),
                visit_node(
                    node_as_conditional_expression.when_false,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
                Some(&*node_as_conditional_expression.when_false.ref_(self)),
            );
            self.mark_label(result_label);
            return result_local;
        }

        visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }

    pub(super) fn visit_yield_expression(
        &self,
        node: Id<Node>, /*YieldExpression*/
    ) -> Id<Node /*LeftHandSideExpression*/> {
        let resume_label = self.define_label();
        let expression = maybe_visit_node(
            node.ref_(self).as_yield_expression().expression,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        if node
            .ref_(self)
            .as_yield_expression()
            .asterisk_token
            .is_some()
        {
            let iterator = if !get_emit_flags(
                node.ref_(self).as_yield_expression().expression.unwrap(),
                self,
            )
            .intersects(EmitFlags::Iterator)
            {
                Some(
                    self.emit_helpers()
                        .create_values_helper(expression.unwrap())
                        .set_text_range(Some(&*node.ref_(self)), self),
                )
            } else {
                expression
            };
            self.emit_yield_star(iterator, Some(&*node.ref_(self)));
        } else {
            self.emit_yield(expression, Some(&*node.ref_(self)));
        }

        self.mark_label(resume_label);
        self.create_generator_resume(Some(&*node.ref_(self)))
    }

    pub(super) fn visit_array_literal_expression(
        &self,
        node: Id<Node>, /*ArrayLiteralExpression*/
    ) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_array_literal_expression = node_ref.as_array_literal_expression();
        Some(
            self.visit_elements(
                node_as_array_literal_expression.elements,
                Option::<Id<Node>>::None,
                Option::<&Node>::None,
                node_as_array_literal_expression.multi_line,
            )
            .into(),
        )
    }

    pub(super) fn visit_elements(
        &self,
        elements: Id<NodeArray>, /*<Expression>*/
        mut leading_element: Option<Id<Node /*Expression*/>>,
        location: Option<&impl ReadonlyTextRange>,
        multi_line: Option<bool>,
    ) -> Id<Node> {
        let num_initial_elements = self.count_initial_nodes_without_yield(elements);

        let mut temp: Option<Id<Node /*Identifier*/>> = _d();
        if let Some(num_initial_elements) =
            num_initial_elements.filter(|&num_initial_elements| num_initial_elements > 0)
        {
            temp = Some(self.declare_local(None));
            let initial_elements = visit_nodes(
                elements,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Some(0),
                Some(num_initial_elements),
                self,
            );
            self.emit_assignment(
                temp.unwrap(),
                self.factory.ref_(self).create_array_literal_expression(
                    Some(if let Some(leading_element) = leading_element.as_ref() {
                        vec![leading_element.clone()]
                            .and_extend(initial_elements.ref_(self).to_vec())
                            .into()
                    } else {
                        initial_elements.into_a::<NodeArrayOrVec>()
                    }),
                    None,
                ),
                Option::<&Node>::None,
            );
            leading_element = None;
        }

        let expressions = reduce_left(
            &elements.ref_(self),
            |expressions: Vec<Id<Node>>, &element: &Id<Node>, _| {
                self.reduce_element(
                    &mut temp,
                    multi_line,
                    &mut leading_element,
                    expressions,
                    element,
                )
            },
            _d(), /*as Expression[]*/
            num_initial_elements,
            None,
        );
        if let Some(temp) = temp {
            self.factory.ref_(self).create_array_concat_call(
                temp,
                vec![self
                    .factory
                    .ref_(self)
                    .create_array_literal_expression(Some(expressions), multi_line)],
            )
        } else {
            self.factory
                .ref_(self)
                .create_array_literal_expression(
                    Some(if let Some(leading_element) = leading_element {
                        vec![leading_element].and_extend(expressions)
                    } else {
                        expressions
                    }),
                    multi_line,
                )
                .set_text_range(location, self)
        }
    }
}
