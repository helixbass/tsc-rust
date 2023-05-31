use gc::Gc;

use super::{ClassFacts, PrivateIdentifierInfo, TransformClassFields};
use crate::{
    expand_pre_or_postfix_increment_or_decrement_expression,
    get_non_assignment_operator_for_compound_assignment, get_original_node_id,
    is_assignment_expression, is_call_chain, is_compound_assignment, is_destructuring_assignment,
    is_element_access_expression, is_expression, is_for_initializer, is_identifier,
    is_prefix_unary_expression, is_private_identifier,
    is_private_identifier_property_access_expression, is_property_access_expression,
    is_simple_inlineable_expression, is_statement, is_super_property, is_template_literal,
    maybe_visit_node, node_is_synthesized, set_text_range, visit_each_child, visit_iteration_body,
    visit_node, visit_nodes, CallBinding, EmitFlags, NamedDeclarationInterface, Node, NodeArray,
    NodeExt, NodeInterface, NonEmpty, SyntaxKind, VecExt, VisitResult,
};

impl TransformClassFields {
    pub(super) fn visit_element_access_expression(
        &self,
        node: &Node, /*ElementAccessExpression*/
    ) -> VisitResult {
        let node_as_element_access_expression = node.as_element_access_expression();
        if self.should_transform_super_in_static_initializers
            && is_super_property(node)
            && self
                .maybe_current_static_property_declaration_or_static_block()
                .is_some()
        {
            if let Some(current_class_lexical_environment) =
                self.maybe_current_class_lexical_environment()
            {
                let class_constructor =
                    current_class_lexical_environment.class_constructor.as_ref();
                let super_class_reference = current_class_lexical_environment
                    .super_class_reference
                    .as_ref();
                let facts = current_class_lexical_environment.facts;
                if facts.intersects(ClassFacts::ClassWasDecorated) {
                    return Some(self.visit_invalid_super_property(node).into());
                }

                if let Some(class_constructor) = class_constructor {
                    if let Some(super_class_reference) = super_class_reference {
                        return Some(
                            self.factory
                                .create_reflect_get_call(
                                    super_class_reference.clone(),
                                    visit_node(
                                        &node_as_element_access_expression.argument_expression,
                                        Some(|node: &Node| self.visitor(node)),
                                        Some(is_expression),
                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                    ),
                                    Some(class_constructor.clone()),
                                )
                                .set_original_node(Some(
                                    node_as_element_access_expression.expression.clone(),
                                ))
                                .set_text_range(Some(
                                    &*node_as_element_access_expression.expression,
                                ))
                                .into(),
                        );
                    }
                }
            }
        }

        Some(visit_each_child(node, |node: &Node| self.visitor(node), &**self.context).into())
    }

    pub(super) fn visit_pre_or_postfix_unary_expression(
        &self,
        node: &Node, /*PrefixUnaryExpression | PostfixUnaryExpression*/
        value_is_discarded: bool,
    ) -> VisitResult {
        let node_as_unary_expression = node.as_unary_expression();
        if matches!(
            node_as_unary_expression.operator(),
            SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
        ) {
            let node_operand = node_as_unary_expression.operand();
            if self.should_transform_private_elements_or_class_static_blocks
                && is_private_identifier_property_access_expression(&node_operand)
            {
                if let Some(info) = self
                    .access_private_identifier(&node_operand.as_property_access_expression().name())
                {
                    let ref receiver = visit_node(
                        &node_operand.as_property_access_expression().expression,
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    );
                    let CopiableReceiverExpr {
                        read_expression,
                        initialize_expression,
                    } = self.create_copiable_receiver_expr(receiver);

                    let mut expression: Gc<Node /*Expression*/> =
                        self.create_private_identifier_access(&info, &read_expression);
                    let temp =
                        (!(is_prefix_unary_expression(node) || value_is_discarded)).then(|| {
                            self.factory.create_temp_variable(
                                Some(|node: &Node| {
                                    self.context.hoist_variable_declaration(node);
                                }),
                                None,
                            )
                        });
                    expression = expand_pre_or_postfix_increment_or_decrement_expression(
                        &self.factory,
                        node,
                        &expression,
                        |node: &Node| {
                            self.context.hoist_variable_declaration(node);
                        },
                        temp.as_deref(),
                    );
                    expression = self
                        .create_private_identifier_assignment(
                            &info,
                            initialize_expression
                                .as_deref()
                                .unwrap_or(&*read_expression),
                            &expression,
                            SyntaxKind::EqualsToken,
                        )
                        .set_original_node(Some(node.node_wrapper()))
                        .set_text_range(Some(node));
                    if let Some(temp) = temp {
                        expression = self
                            .factory
                            .create_comma(expression, temp)
                            .wrap()
                            .set_text_range(Some(node));
                    }
                    return Some(expression.into());
                }
            } else if self.should_transform_super_in_static_initializers
                && is_super_property(&node_operand)
                && self
                    .maybe_current_static_property_declaration_or_static_block()
                    .is_some()
            {
                if let Some(current_class_lexical_environment) =
                    self.maybe_current_class_lexical_environment()
                {
                    let class_constructor =
                        current_class_lexical_environment.class_constructor.as_ref();
                    let super_class_reference = current_class_lexical_environment
                        .super_class_reference
                        .as_ref();
                    let facts = current_class_lexical_environment.facts;
                    if facts.intersects(ClassFacts::ClassWasDecorated) {
                        let operand = self.visit_invalid_super_property(&node_operand);
                        return Some(
                            if is_prefix_unary_expression(node) {
                                self.factory.update_prefix_unary_expression(node, operand)
                            } else {
                                self.factory.update_postfix_unary_expression(node, operand)
                            }
                            .into(),
                        );
                    }
                    if let Some(class_constructor) = class_constructor {
                        if let Some(super_class_reference) = super_class_reference {
                            let mut setter_name: Option<Gc<Node /*Expression*/>> = None;
                            let mut getter_name: Option<Gc<Node /*Expression*/>> = None;
                            if is_property_access_expression(&node_operand) {
                                let node_operand_as_property_access_expression =
                                    node_operand.as_property_access_expression();
                                if is_identifier(&node_operand_as_property_access_expression.name())
                                {
                                    setter_name = Some(
                                        self.factory
                                            .create_string_literal_from_node(
                                                &node_operand_as_property_access_expression.name(),
                                            )
                                            .wrap(),
                                    );
                                    getter_name = setter_name.clone();
                                }
                            } else {
                                let node_operand_as_element_access_expression =
                                    node_operand.as_element_access_expression();
                                if is_simple_inlineable_expression(
                                    &node_operand_as_element_access_expression.argument_expression,
                                ) {
                                    setter_name = Some(
                                        node_operand_as_element_access_expression
                                            .argument_expression
                                            .clone(),
                                    );
                                    getter_name = setter_name.clone();
                                } else {
                                    getter_name = Some(self.factory.create_temp_variable(
                                        Some(|node: &Node| {
                                            self.context.hoist_variable_declaration(node);
                                        }),
                                        None,
                                    ));
                                    setter_name = Some(
                                        self.factory
                                            .create_assignment(
                                                getter_name.clone().unwrap(),
                                                visit_node(
                                                    &node_operand_as_element_access_expression
                                                        .argument_expression,
                                                    Some(|node: &Node| self.visitor(node)),
                                                    Some(is_expression),
                                                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                                ),
                                            )
                                            .wrap(),
                                    );
                                }
                            }
                            if let (Some(setter_name), Some(getter_name)) =
                                (setter_name, getter_name)
                            {
                                let mut expression: Gc<Node /*Expression*/> = self
                                    .factory
                                    .create_reflect_get_call(
                                        super_class_reference.clone(),
                                        getter_name,
                                        Some(class_constructor.clone()),
                                    )
                                    .set_text_range(Some(&*node_operand));

                                let temp = (!value_is_discarded).then(|| {
                                    self.factory.create_temp_variable(
                                        Some(|node: &Node| {
                                            self.context.hoist_variable_declaration(node);
                                        }),
                                        None,
                                    )
                                });
                                expression =
                                    expand_pre_or_postfix_increment_or_decrement_expression(
                                        &self.factory,
                                        node,
                                        &expression,
                                        |node: &Node| {
                                            self.context.hoist_variable_declaration(node);
                                        },
                                        temp.as_deref(),
                                    );
                                expression = self
                                    .factory
                                    .create_reflect_set_call(
                                        super_class_reference.clone(),
                                        setter_name,
                                        expression,
                                        Some(class_constructor.clone()),
                                    )
                                    .set_original_node(Some(node.node_wrapper()))
                                    .set_text_range(Some(node));
                                if let Some(temp) = temp {
                                    expression = self
                                        .factory
                                        .create_comma(expression, temp)
                                        .wrap()
                                        .set_text_range(Some(node));
                                }
                                return Some(expression.into());
                            }
                        }
                    }
                }
            }
        }
        Some(visit_each_child(node, |node: &Node| self.visitor(node), &**self.context).into())
    }

    pub(super) fn visit_for_statement(&self, node: &Node /*ForStatement*/) -> VisitResult {
        let node_as_for_statement = node.as_for_statement();
        Some(
            self.factory
                .update_for_statement(
                    node,
                    maybe_visit_node(
                        node_as_for_statement.initializer.as_deref(),
                        Some(|node: &Node| self.discarded_value_visitor(node)),
                        Some(is_for_initializer),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    ),
                    maybe_visit_node(
                        node_as_for_statement.condition.as_deref(),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    ),
                    maybe_visit_node(
                        node_as_for_statement.incrementor.as_deref(),
                        Some(|node: &Node| self.discarded_value_visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    ),
                    visit_iteration_body(
                        &node_as_for_statement.statement,
                        |node: &Node| self.visitor(node),
                        &**self.context,
                    ),
                )
                .into(),
        )
    }

    pub(super) fn visit_expression_statement(
        &self,
        node: &Node, /*ExpressionStatement*/
    ) -> VisitResult {
        let node_as_expression_statement = node.as_expression_statement();
        Some(
            self.factory
                .update_expression_statement(
                    node,
                    visit_node(
                        &node_as_expression_statement.expression,
                        Some(|node: &Node| self.discarded_value_visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    ),
                )
                .into(),
        )
    }

    pub(super) fn create_copiable_receiver_expr(
        &self,
        receiver: &Node, /*Expression*/
    ) -> CopiableReceiverExpr {
        let clone = if node_is_synthesized(receiver) {
            receiver.node_wrapper()
        } else {
            self.factory.clone_node(receiver)
        };
        if is_simple_inlineable_expression(receiver) {
            return CopiableReceiverExpr {
                read_expression: clone,
                initialize_expression: None,
            };
        }
        let read_expression = self.factory.create_temp_variable(
            Some(|node: &Node| {
                self.context.hoist_variable_declaration(node);
            }),
            None,
        );
        let initialize_expression = self
            .factory
            .create_assignment(read_expression.clone(), clone)
            .wrap();
        CopiableReceiverExpr {
            read_expression,
            initialize_expression: Some(initialize_expression),
        }
    }

    pub(super) fn visit_call_expression(&self, node: &Node /*CallExpression*/) -> VisitResult {
        let node_as_call_expression = node.as_call_expression();
        if self.should_transform_private_elements_or_class_static_blocks
            && is_private_identifier_property_access_expression(&node_as_call_expression.expression)
        {
            let CallBinding { this_arg, target } = self.factory.create_call_binding(
                &node_as_call_expression.expression,
                |node: &Node| {
                    self.context.hoist_variable_declaration(node);
                },
                Some(self.language_version),
                None,
            );
            if is_call_chain(node) {
                return Some(
                    self.factory
                        .update_call_chain(
                            node,
                            self.factory
                                .create_property_access_chain(
                                    visit_node(
                                        &target,
                                        Some(|node: &Node| self.visitor(node)),
                                        Option::<fn(&Node) -> bool>::None,
                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                    ),
                                    node_as_call_expression.question_dot_token.clone(),
                                    "call",
                                )
                                .wrap(),
                            None,
                            Option::<Gc<NodeArray>>::None,
                            vec![visit_node(
                                &this_arg,
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_expression),
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            )]
                            .and_extend(
                                visit_nodes(
                                    &node_as_call_expression.arguments,
                                    Some(|node: &Node| self.visitor(node)),
                                    Some(is_expression),
                                    None,
                                    None,
                                )
                                .owned_iter(),
                            ),
                        )
                        .into(),
                );
            }
            return Some(
                self.factory
                    .update_call_expression(
                        node,
                        self.factory
                            .create_property_access_expression(
                                visit_node(
                                    &target,
                                    Some(|node: &Node| self.visitor(node)),
                                    Option::<fn(&Node) -> bool>::None,
                                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                ),
                                "call",
                            )
                            .wrap(),
                        Option::<Gc<NodeArray>>::None,
                        vec![visit_node(
                            &this_arg,
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_expression),
                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                        )]
                        .and_extend(
                            visit_nodes(
                                &node_as_call_expression.arguments,
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_expression),
                                None,
                                None,
                            )
                            .owned_iter(),
                        ),
                    )
                    .into(),
            );
        }

        if self.should_transform_super_in_static_initializers
            && is_super_property(&node_as_call_expression.expression)
            && self
                .maybe_current_static_property_declaration_or_static_block()
                .is_some()
        {
            if let Some(current_class_lexical_environment_class_constructor) = self
                .maybe_current_class_lexical_environment()
                .as_ref()
                .and_then(|current_class_lexical_environment| {
                    current_class_lexical_environment.class_constructor.as_ref()
                })
            {
                return Some(
                    self.factory
                        .create_function_call_call(
                            visit_node(
                                &node_as_call_expression.expression,
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_expression),
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            ),
                            current_class_lexical_environment_class_constructor.clone(),
                            visit_nodes(
                                &node_as_call_expression.arguments,
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_expression),
                                None,
                                None,
                            ),
                        )
                        .set_original_node(Some(node.node_wrapper()))
                        .set_text_range(Some(node))
                        .into(),
                );
            }
        }

        Some(visit_each_child(node, |node: &Node| self.visitor(node), &**self.context).into())
    }

    pub(super) fn visit_tagged_template_expression(
        &self,
        node: &Node, /*TaggedTemplateExpression*/
    ) -> VisitResult {
        let node_as_tagged_template_expression = node.as_tagged_template_expression();
        if self.should_transform_private_elements_or_class_static_blocks
            && is_private_identifier_property_access_expression(
                &node_as_tagged_template_expression.tag,
            )
        {
            let CallBinding { this_arg, target } = self.factory.create_call_binding(
                &node_as_tagged_template_expression.tag,
                |node: &Node| {
                    self.context.hoist_variable_declaration(node);
                },
                Some(self.language_version),
                None,
            );
            return Some(
                self.factory
                    .update_tagged_template_expression(
                        node,
                        self.factory
                            .create_call_expression(
                                self.factory
                                    .create_property_access_expression(
                                        visit_node(
                                            &target,
                                            Some(|node: &Node| self.visitor(node)),
                                            Option::<fn(&Node) -> bool>::None,
                                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                        ),
                                        "bind",
                                    )
                                    .wrap(),
                                Option::<Gc<NodeArray>>::None,
                                Some(vec![visit_node(
                                    &this_arg,
                                    Some(|node: &Node| self.visitor(node)),
                                    Some(is_expression),
                                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                )]),
                            )
                            .wrap(),
                        Option::<Gc<NodeArray>>::None,
                        visit_node(
                            &node_as_tagged_template_expression.template,
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_template_literal),
                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                        ),
                    )
                    .into(),
            );
        }
        if self.should_transform_super_in_static_initializers
            && is_super_property(&node_as_tagged_template_expression.tag)
            && self
                .maybe_current_static_property_declaration_or_static_block()
                .is_some()
        {
            if let Some(current_class_lexical_environment_class_constructor) = self
                .maybe_current_class_lexical_environment()
                .as_ref()
                .and_then(|current_class_lexical_environment| {
                    current_class_lexical_environment.class_constructor.as_ref()
                })
            {
                let invocation = self
                    .factory
                    .create_function_bind_call(
                        visit_node(
                            &node_as_tagged_template_expression.tag,
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_expression),
                            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                        ),
                        current_class_lexical_environment_class_constructor.clone(),
                        vec![],
                    )
                    .set_original_node(Some(node.node_wrapper()))
                    .set_text_range(Some(node));
                return Some(
                    self.factory
                        .update_tagged_template_expression(
                            node,
                            invocation,
                            Option::<Gc<NodeArray>>::None,
                            visit_node(
                                &node_as_tagged_template_expression.template,
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_template_literal),
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            ),
                        )
                        .into(),
                );
            }
        }

        Some(visit_each_child(node, |node: &Node| self.visitor(node), &**self.context).into())
    }

    pub(super) fn transform_class_static_block_declaration(
        &self,
        node: &Node, /*ClassStaticBlockDeclaration*/
    ) -> Option<Gc<Node>> {
        let node_as_class_static_block_declaration = node.as_class_static_block_declaration();
        if self.should_transform_private_elements_or_class_static_blocks {
            if let Some(current_class_lexical_environment) =
                self.maybe_current_class_lexical_environment()
            {
                self.class_lexical_environment_map_mut().insert(
                    get_original_node_id(node),
                    current_class_lexical_environment,
                );
            }

            self.context.start_lexical_environment();
            let saved_current_static_property_declaration_or_static_block =
                self.maybe_current_static_property_declaration_or_static_block();
            self.set_current_static_property_declaration_or_static_block(Some(node.node_wrapper()));
            let mut statements = visit_nodes(
                &node_as_class_static_block_declaration
                    .body
                    .as_block()
                    .statements,
                Some(|node: &Node| self.visitor(node)),
                Some(is_statement),
                None,
                None,
            );
            statements = self
                .factory
                .merge_lexical_environment(
                    statements,
                    self.context.end_lexical_environment().as_deref(),
                )
                .as_node_array();
            self.set_current_static_property_declaration_or_static_block(
                saved_current_static_property_declaration_or_static_block,
            );

            return Some(
                self.factory
                    .create_immediately_invoked_arrow_function(statements, None, None)
                    .set_original_node(Some(node.node_wrapper()))
                    .set_text_range(Some(node))
                    .add_emit_flags(EmitFlags::AdviseOnEmitNode),
            );
        }
        None
    }

    pub(super) fn visit_binary_expression(
        &self,
        node: &Node, /*BinaryExpression*/
        value_is_discarded: bool,
    ) -> VisitResult {
        let mut node = node.node_wrapper();
        if is_destructuring_assignment(&node) {
            let saved_pending_expressions = self.maybe_pending_expressions().clone();
            self.set_pending_expressions(None);
            node = self.factory.update_binary_expression(
                &node,
                visit_node(
                    &node.as_binary_expression().left,
                    Some(|node: &Node| self.visitor_destructuring_target(node)),
                    Option::<fn(&Node) -> bool>::None,
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                ),
                node.as_binary_expression().operator_token.clone(),
                visit_node(
                    &node.as_binary_expression().right,
                    Some(|node: &Node| self.visitor(node)),
                    Option::<fn(&Node) -> bool>::None,
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                ),
            );
            let expr = if self.maybe_pending_expressions().as_ref().is_non_empty() {
                self.factory.inline_expressions(
                    &/*compact(*/self.maybe_pending_expressions()
                        .clone()
                        .unwrap_or_default()
                        .and_push(node.clone()), /*)*/
                )
            } else {
                node.clone()
            };
            self.set_pending_expressions(saved_pending_expressions);
            return Some(expr.into());
        }
        let node = &*node;
        let node_as_binary_expression = node.as_binary_expression();
        if is_assignment_expression(node, None) {
            if self.should_transform_private_elements_or_class_static_blocks
                && is_private_identifier_property_access_expression(&node_as_binary_expression.left)
            {
                let node_left_as_property_access_expression = node_as_binary_expression
                    .left
                    .as_property_access_expression();
                let info =
                    self.access_private_identifier(&node_left_as_property_access_expression.name);
                if let Some(ref info) = info {
                    return Some(
                        self.create_private_identifier_assignment(
                            info,
                            &node_left_as_property_access_expression.expression,
                            &node_as_binary_expression.right,
                            node_as_binary_expression.operator_token.kind(),
                        )
                        .set_original_node(Some(node.node_wrapper()))
                        .set_text_range(Some(node))
                        .into(),
                    );
                }
            } else if self.should_transform_super_in_static_initializers
                && is_super_property(&node_as_binary_expression.left)
                && self
                    .maybe_current_static_property_declaration_or_static_block()
                    .is_some()
            {
                if let Some(current_class_lexical_environment) =
                    self.maybe_current_class_lexical_environment()
                {
                    let class_constructor =
                        current_class_lexical_environment.class_constructor.as_ref();
                    let super_class_reference = current_class_lexical_environment
                        .super_class_reference
                        .as_ref();
                    let facts = current_class_lexical_environment.facts;
                    if facts.intersects(ClassFacts::ClassWasDecorated) {
                        return Some(
                            self.factory
                                .update_binary_expression(
                                    node,
                                    self.visit_invalid_super_property(
                                        &node_as_binary_expression.left,
                                    ),
                                    node_as_binary_expression.operator_token.clone(),
                                    visit_node(
                                        &node_as_binary_expression.right,
                                        Some(|node: &Node| self.visitor(node)),
                                        Some(is_expression),
                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                    ),
                                )
                                .into(),
                        );
                    }
                    if let Some(class_constructor) = class_constructor {
                        if let Some(super_class_reference) = super_class_reference {
                            let setter_name =
                                if is_element_access_expression(&node_as_binary_expression.left) {
                                    Some(visit_node(
                                        &node_as_binary_expression
                                            .left
                                            .as_element_access_expression()
                                            .argument_expression,
                                        Some(|node: &Node| self.visitor(node)),
                                        Some(is_expression),
                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                    ))
                                } else if is_identifier(
                                    &node_as_binary_expression
                                        .left
                                        .as_property_access_expression()
                                        .name,
                                ) {
                                    Some(
                                        self.factory
                                            .create_string_literal_from_node(
                                                &node_as_binary_expression
                                                    .left
                                                    .as_property_access_expression()
                                                    .name,
                                            )
                                            .wrap(),
                                    )
                                } else {
                                    None
                                };
                            if let Some(mut setter_name) = setter_name {
                                let mut expression = visit_node(
                                    &node_as_binary_expression.right,
                                    Some(|node: &Node| self.visitor(node)),
                                    Some(is_expression),
                                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                );
                                if is_compound_assignment(
                                    node_as_binary_expression.operator_token.kind(),
                                ) {
                                    let mut getter_name = setter_name.clone();
                                    if !is_simple_inlineable_expression(&setter_name) {
                                        getter_name = self.factory.create_temp_variable(
                                            Some(|node: &Node| {
                                                self.context.hoist_variable_declaration(node);
                                            }),
                                            None,
                                        );
                                        setter_name = self
                                            .factory
                                            .create_assignment(getter_name.clone(), setter_name)
                                            .wrap();
                                    }
                                    let super_property_get = self
                                        .factory
                                        .create_reflect_get_call(
                                            super_class_reference.clone(),
                                            getter_name,
                                            Some(class_constructor.clone()),
                                        )
                                        .set_original_node(Some(
                                            node_as_binary_expression.left.clone(),
                                        ))
                                        .set_text_range(Some(&*node_as_binary_expression.left));

                                    expression = self
                                        .factory
                                        .create_binary_expression(
                                            super_property_get,
                                            get_non_assignment_operator_for_compound_assignment(
                                                node_as_binary_expression.operator_token.kind(),
                                            ),
                                            expression,
                                        )
                                        .wrap()
                                        .set_text_range(Some(node));
                                }

                                let temp = (!value_is_discarded).then(|| {
                                    self.factory.create_temp_variable(
                                        Some(|node: &Node| {
                                            self.context.hoist_variable_declaration(node);
                                        }),
                                        None,
                                    )
                                });
                                if let Some(temp) = temp.as_ref() {
                                    expression = self
                                        .factory
                                        .create_assignment(temp.clone(), expression)
                                        .wrap();
                                    set_text_range(&**temp, Some(node));
                                }

                                expression = self
                                    .factory
                                    .create_reflect_set_call(
                                        super_class_reference.clone(),
                                        setter_name,
                                        expression,
                                        Some(class_constructor.clone()),
                                    )
                                    .set_original_node(Some(node.node_wrapper()))
                                    .set_text_range(Some(node));

                                if let Some(temp) = temp {
                                    expression = self
                                        .factory
                                        .create_comma(expression, temp)
                                        .wrap()
                                        .set_text_range(Some(node));
                                }

                                return Some(expression.into());
                            }
                        }
                    }
                }
            }
        }
        if node_as_binary_expression.operator_token.kind() == SyntaxKind::InKeyword
            && is_private_identifier(&node_as_binary_expression.left)
        {
            return self.visit_private_identifier_in_in_expression(node);
        }
        Some(visit_each_child(node, |node: &Node| self.visitor(node), &**self.context).into())
    }

    pub(super) fn create_private_identifier_assignment(
        &self,
        _info: &PrivateIdentifierInfo,
        _receiver: &Node,      /*Expression*/
        _right: &Node,         /*Expression*/
        _operator: SyntaxKind, /*AssignmentOperator*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_class_like(
        &self,
        _node: &Node, /*ClassLikeDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_expression_with_type_arguments(
        &self,
        _node: &Node, /*ExpressionWithTypeArguments*/
    ) -> VisitResult {
        unimplemented!()
    }
}

pub(super) struct CopiableReceiverExpr {
    pub read_expression: Gc<Node /*Expression*/>,
    pub initialize_expression: Option<Gc<Node /*Expression*/>>,
}
