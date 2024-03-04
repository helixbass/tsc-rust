use id_arena::Id;

use super::{
    ClassFacts, PrivateIdentifierInfo, PrivateIdentifierInfoInterface, TransformClassFields,
};
use crate::{
    class_or_constructor_parameter_is_decorated,
    expand_pre_or_postfix_increment_or_decrement_expression, filter, for_each_bool,
    get_name_of_declaration, get_non_assignment_operator_for_compound_assignment,
    get_original_node, get_original_node_id, id_text, is_assignment_expression, is_call_chain,
    is_class_declaration, is_class_static_block_declaration, is_compound_assignment,
    is_destructuring_assignment, is_element_access_expression, is_expression, is_for_initializer,
    is_identifier, is_non_static_method_or_accessor_with_private_name, is_prefix_unary_expression,
    is_private_identifier, is_private_identifier_property_access_expression,
    is_property_access_expression, is_property_declaration, is_simple_inlineable_expression,
    is_statement, is_static, is_super_property, is_template_literal, maybe_visit_node,
    move_range_pos, node_is_synthesized, released, set_comment_range, set_text_range,
    visit_each_child, visit_iteration_body, visit_node, visit_nodes, CallBinding,
    CoreTransformationContext, EmitFlags, InArena, MapOrDefault, Matches,
    NamedDeclarationInterface, Node, NodeArray, NodeExt, NodeInterface, NonEmpty,
    PrivateIdentifierKind, ReadonlyTextRangeConcrete, SyntaxKind, TransformFlags,
    TransformationContext, VecExt, VisitResult,
};

impl TransformClassFields {
    pub(super) fn visit_element_access_expression(
        &self,
        node: Id<Node>, /*ElementAccessExpression*/
    ) -> VisitResult {
        if self.should_transform_super_in_static_initializers
            && is_super_property(node, self)
            && self
                .maybe_current_static_property_declaration_or_static_block()
                .is_some()
        {
            if let Some(current_class_lexical_environment) =
                self.maybe_current_class_lexical_environment()
            {
                let current_class_lexical_environment =
                    current_class_lexical_environment.ref_(self);
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
                                .ref_(self)
                                .create_reflect_get_call(
                                    super_class_reference.clone(),
                                    visit_node(
                                        released!(
                                            node.ref_(self)
                                                .as_element_access_expression()
                                                .argument_expression
                                        ),
                                        Some(|node: Id<Node>| self.visitor(node)),
                                        Some(|node| is_expression(node, self)),
                                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                    ),
                                    Some(class_constructor.clone()),
                                )
                                .set_original_node(
                                    Some(node.ref_(self).as_element_access_expression().expression),
                                    self,
                                )
                                .set_text_range(
                                    Some(
                                        &*node
                                            .ref_(self)
                                            .as_element_access_expression()
                                            .expression
                                            .ref_(self),
                                    ),
                                    self,
                                )
                                .into(),
                        );
                    }
                }
            }
        }

        Some(
            visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .into(),
        )
    }

    pub(super) fn visit_pre_or_postfix_unary_expression(
        &self,
        node: Id<Node>, /*PrefixUnaryExpression | PostfixUnaryExpression*/
        value_is_discarded: bool,
    ) -> VisitResult {
        if matches!(
            node.ref_(self).as_unary_expression().operator(),
            SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
        ) {
            let node_operand = node.ref_(self).as_unary_expression().operand();
            if self.should_transform_private_elements_or_class_static_blocks
                && is_private_identifier_property_access_expression(node_operand, self)
            {
                if let Some(info) = self.access_private_identifier(released!(node_operand
                    .ref_(self)
                    .as_property_access_expression()
                    .name()))
                {
                    let receiver = visit_node(
                        node_operand
                            .ref_(self)
                            .as_property_access_expression()
                            .expression,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    );
                    let CopiableReceiverExpr {
                        read_expression,
                        initialize_expression,
                    } = self.create_copiable_receiver_expr(receiver);

                    let mut expression: Id<Node /*Expression*/> =
                        self.create_private_identifier_access(info, read_expression);
                    let temp = (!(is_prefix_unary_expression(&node.ref_(self))
                        || value_is_discarded))
                        .then(|| {
                            self.factory.ref_(self).create_temp_variable(
                                Some(|node: Id<Node>| {
                                    self.context.ref_(self).hoist_variable_declaration(node);
                                }),
                                None,
                            )
                        });
                    expression = expand_pre_or_postfix_increment_or_decrement_expression(
                        &self.factory.ref_(self),
                        node,
                        expression,
                        |node: Id<Node>| {
                            self.context.ref_(self).hoist_variable_declaration(node);
                        },
                        temp,
                    );
                    expression = self
                        .create_private_identifier_assignment(
                            info,
                            initialize_expression.unwrap_or(read_expression),
                            expression,
                            SyntaxKind::EqualsToken,
                        )
                        .set_original_node(Some(node), self)
                        .set_text_range(Some(&*node.ref_(self)), self);
                    if let Some(temp) = temp {
                        expression = self
                            .factory
                            .ref_(self)
                            .create_comma(expression, temp)
                            .set_text_range(Some(&*node.ref_(self)), self);
                    }
                    return Some(expression.into());
                }
            } else if self.should_transform_super_in_static_initializers
                && is_super_property(node_operand, self)
                && self
                    .maybe_current_static_property_declaration_or_static_block()
                    .is_some()
            {
                if let Some(current_class_lexical_environment) =
                    self.maybe_current_class_lexical_environment()
                {
                    let current_class_lexical_environment =
                        current_class_lexical_environment.ref_(self);
                    let class_constructor =
                        current_class_lexical_environment.class_constructor.as_ref();
                    let super_class_reference = current_class_lexical_environment
                        .super_class_reference
                        .as_ref();
                    let facts = current_class_lexical_environment.facts;
                    if facts.intersects(ClassFacts::ClassWasDecorated) {
                        let operand = self.visit_invalid_super_property(node_operand);
                        return Some(
                            if is_prefix_unary_expression(&node.ref_(self)) {
                                self.factory
                                    .ref_(self)
                                    .update_prefix_unary_expression(node, operand)
                            } else {
                                self.factory
                                    .ref_(self)
                                    .update_postfix_unary_expression(node, operand)
                            }
                            .into(),
                        );
                    }
                    if let Some(class_constructor) = class_constructor {
                        if let Some(super_class_reference) = super_class_reference {
                            let mut setter_name: Option<Id<Node /*Expression*/>> = None;
                            let mut getter_name: Option<Id<Node /*Expression*/>> = None;
                            if is_property_access_expression(&node_operand.ref_(self)) {
                                let node_operand_ref = node_operand.ref_(self);
                                let node_operand_as_property_access_expression =
                                    node_operand_ref.as_property_access_expression();
                                if is_identifier(
                                    &node_operand_as_property_access_expression.name().ref_(self),
                                ) {
                                    setter_name = Some(
                                        self.factory.ref_(self).create_string_literal_from_node(
                                            node_operand_as_property_access_expression.name(),
                                        ),
                                    );
                                    getter_name = setter_name.clone();
                                }
                            } else {
                                let node_operand_ref = node_operand.ref_(self);
                                let node_operand_as_element_access_expression =
                                    node_operand_ref.as_element_access_expression();
                                if is_simple_inlineable_expression(
                                    &node_operand_as_element_access_expression
                                        .argument_expression
                                        .ref_(self),
                                ) {
                                    setter_name = Some(
                                        node_operand_as_element_access_expression
                                            .argument_expression
                                            .clone(),
                                    );
                                    getter_name = setter_name.clone();
                                } else {
                                    getter_name =
                                        Some(self.factory.ref_(self).create_temp_variable(
                                            Some(|node: Id<Node>| {
                                                self.context
                                                    .ref_(self)
                                                    .hoist_variable_declaration(node);
                                            }),
                                            None,
                                        ));
                                    setter_name = Some(
                                        self.factory.ref_(self).create_assignment(
                                            getter_name.clone().unwrap(),
                                            visit_node(
                                                node_operand_as_element_access_expression
                                                    .argument_expression,
                                                Some(|node: Id<Node>| self.visitor(node)),
                                                Some(|node| is_expression(node, self)),
                                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                            ),
                                        ),
                                    );
                                }
                            }
                            if let (Some(setter_name), Some(getter_name)) =
                                (setter_name, getter_name)
                            {
                                let mut expression: Id<Node /*Expression*/> = self
                                    .factory
                                    .ref_(self)
                                    .create_reflect_get_call(
                                        super_class_reference.clone(),
                                        getter_name,
                                        Some(class_constructor.clone()),
                                    )
                                    .set_text_range(Some(&*node_operand.ref_(self)), self);

                                let temp = (!value_is_discarded).then(|| {
                                    self.factory.ref_(self).create_temp_variable(
                                        Some(|node: Id<Node>| {
                                            self.context
                                                .ref_(self)
                                                .hoist_variable_declaration(node);
                                        }),
                                        None,
                                    )
                                });
                                expression =
                                    expand_pre_or_postfix_increment_or_decrement_expression(
                                        &self.factory.ref_(self),
                                        node,
                                        expression,
                                        |node: Id<Node>| {
                                            self.context
                                                .ref_(self)
                                                .hoist_variable_declaration(node);
                                        },
                                        temp,
                                    );
                                expression = self
                                    .factory
                                    .ref_(self)
                                    .create_reflect_set_call(
                                        super_class_reference.clone(),
                                        setter_name,
                                        expression,
                                        Some(class_constructor.clone()),
                                    )
                                    .set_original_node(Some(node), self)
                                    .set_text_range(Some(&*node.ref_(self)), self);
                                if let Some(temp) = temp {
                                    expression = self
                                        .factory
                                        .ref_(self)
                                        .create_comma(expression, temp)
                                        .set_text_range(Some(&*node.ref_(self)), self);
                                }
                                return Some(expression.into());
                            }
                        }
                    }
                }
            }
        }
        Some(
            visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .into(),
        )
    }

    pub(super) fn visit_for_statement(&self, node: Id<Node> /*ForStatement*/) -> VisitResult {
        Some(
            self.factory
                .ref_(self)
                .update_for_statement(
                    node,
                    maybe_visit_node(
                        released!(node.ref_(self).as_for_statement().initializer),
                        Some(|node: Id<Node>| self.discarded_value_visitor(node)),
                        Some(|node| is_for_initializer(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                    maybe_visit_node(
                        released!(node.ref_(self).as_for_statement().condition),
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                    maybe_visit_node(
                        released!(node.ref_(self).as_for_statement().incrementor),
                        Some(|node: Id<Node>| self.discarded_value_visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                    visit_iteration_body(
                        released!(node.ref_(self).as_for_statement().statement),
                        |node: Id<Node>| self.visitor(node),
                        &*self.context.ref_(self),
                        self,
                    ),
                )
                .into(),
        )
    }

    pub(super) fn visit_expression_statement(
        &self,
        node: Id<Node>, /*ExpressionStatement*/
    ) -> VisitResult {
        Some(
            self.factory
                .ref_(self)
                .update_expression_statement(
                    node,
                    visit_node(
                        released!(node.ref_(self).as_expression_statement().expression),
                        Some(|node: Id<Node>| self.discarded_value_visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                )
                .into(),
        )
    }

    pub(super) fn create_copiable_receiver_expr(
        &self,
        receiver: Id<Node>, /*Expression*/
    ) -> CopiableReceiverExpr {
        let clone = if node_is_synthesized(&*receiver.ref_(self)) {
            receiver
        } else {
            self.factory.ref_(self).clone_node(receiver)
        };
        if is_simple_inlineable_expression(&receiver.ref_(self)) {
            return CopiableReceiverExpr {
                read_expression: clone,
                initialize_expression: None,
            };
        }
        let read_expression = self.factory.ref_(self).create_temp_variable(
            Some(|node: Id<Node>| {
                self.context.ref_(self).hoist_variable_declaration(node);
            }),
            None,
        );
        let initialize_expression = self
            .factory
            .ref_(self)
            .create_assignment(read_expression.clone(), clone);
        CopiableReceiverExpr {
            read_expression,
            initialize_expression: Some(initialize_expression),
        }
    }

    pub(super) fn visit_call_expression(
        &self,
        node: Id<Node>, /*CallExpression*/
    ) -> VisitResult {
        if self.should_transform_private_elements_or_class_static_blocks
            && is_private_identifier_property_access_expression(
                node.ref_(self).as_call_expression().expression,
                self,
            )
        {
            let CallBinding { this_arg, target } = self.factory.ref_(self).create_call_binding(
                released!(node.ref_(self).as_call_expression().expression),
                |node: Id<Node>| {
                    self.context.ref_(self).hoist_variable_declaration(node);
                },
                Some(self.language_version),
                None,
            );
            if is_call_chain(&node.ref_(self)) {
                return Some(
                    self.factory
                        .ref_(self)
                        .update_call_chain(
                            node,
                            self.factory.ref_(self).create_property_access_chain(
                                visit_node(
                                    target,
                                    Some(|node: Id<Node>| self.visitor(node)),
                                    Option::<fn(Id<Node>) -> bool>::None,
                                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                ),
                                node.ref_(self)
                                    .as_call_expression()
                                    .question_dot_token
                                    .clone(),
                                "call",
                            ),
                            None,
                            Option::<Id<NodeArray>>::None,
                            vec![visit_node(
                                this_arg,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            )]
                            .and_extend(
                                visit_nodes(
                                    node.ref_(self).as_call_expression().arguments,
                                    Some(|node: Id<Node>| self.visitor(node)),
                                    Some(|node| is_expression(node, self)),
                                    None,
                                    None,
                                    self,
                                )
                                .ref_(self)
                                .iter()
                                .copied(),
                            ),
                        )
                        .into(),
                );
            }
            return Some(
                self.factory
                    .ref_(self)
                    .update_call_expression(
                        node,
                        self.factory.ref_(self).create_property_access_expression(
                            visit_node(
                                target,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Option::<fn(Id<Node>) -> bool>::None,
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ),
                            "call",
                        ),
                        Option::<Id<NodeArray>>::None,
                        released!(vec![visit_node(
                            this_arg,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node| is_expression(node, self)),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        )]
                        .and_extend(
                            visit_nodes(
                                node.ref_(self).as_call_expression().arguments,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                None,
                                None,
                                self,
                            )
                            .ref_(self)
                            .iter()
                            .copied(),
                        )),
                    )
                    .into(),
            );
        }

        if self.should_transform_super_in_static_initializers
            && is_super_property(node.ref_(self).as_call_expression().expression, self)
            && self
                .maybe_current_static_property_declaration_or_static_block()
                .is_some()
        {
            if let Some(ref current_class_lexical_environment_class_constructor) = self
                .maybe_current_class_lexical_environment()
                .and_then(|current_class_lexical_environment| {
                    current_class_lexical_environment
                        .ref_(self)
                        .class_constructor
                        .clone()
                })
            {
                return Some(
                    self.factory
                        .ref_(self)
                        .create_function_call_call(
                            visit_node(
                                released!(node.ref_(self).as_call_expression().expression),
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ),
                            current_class_lexical_environment_class_constructor.clone(),
                            visit_nodes(
                                released!(node.ref_(self).as_call_expression().arguments),
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                None,
                                None,
                                self,
                            ),
                        )
                        .set_original_node(Some(node), self)
                        .set_text_range(Some(&*node.ref_(self)), self)
                        .into(),
                );
            }
        }

        Some(
            visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .into(),
        )
    }

    pub(super) fn visit_tagged_template_expression(
        &self,
        node: Id<Node>, /*TaggedTemplateExpression*/
    ) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_tagged_template_expression = node_ref.as_tagged_template_expression();
        if self.should_transform_private_elements_or_class_static_blocks
            && is_private_identifier_property_access_expression(
                node_as_tagged_template_expression.tag,
                self,
            )
        {
            let CallBinding { this_arg, target } = self.factory.ref_(self).create_call_binding(
                node_as_tagged_template_expression.tag,
                |node: Id<Node>| {
                    self.context.ref_(self).hoist_variable_declaration(node);
                },
                Some(self.language_version),
                None,
            );
            return Some(
                self.factory
                    .ref_(self)
                    .update_tagged_template_expression(
                        node,
                        self.factory.ref_(self).create_call_expression(
                            self.factory.ref_(self).create_property_access_expression(
                                visit_node(
                                    target,
                                    Some(|node: Id<Node>| self.visitor(node)),
                                    Option::<fn(Id<Node>) -> bool>::None,
                                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                ),
                                "bind",
                            ),
                            Option::<Id<NodeArray>>::None,
                            Some(vec![visit_node(
                                this_arg,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            )]),
                        ),
                        Option::<Id<NodeArray>>::None,
                        visit_node(
                            node_as_tagged_template_expression.template,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node: Id<Node>| is_template_literal(&node.ref_(self))),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        ),
                    )
                    .into(),
            );
        }
        if self.should_transform_super_in_static_initializers
            && is_super_property(node_as_tagged_template_expression.tag, self)
            && self
                .maybe_current_static_property_declaration_or_static_block()
                .is_some()
        {
            if let Some(ref current_class_lexical_environment_class_constructor) = self
                .maybe_current_class_lexical_environment()
                .and_then(|current_class_lexical_environment| {
                    current_class_lexical_environment
                        .ref_(self)
                        .class_constructor
                        .clone()
                })
            {
                let invocation = self
                    .factory
                    .ref_(self)
                    .create_function_bind_call(
                        visit_node(
                            node_as_tagged_template_expression.tag,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node| is_expression(node, self)),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        ),
                        current_class_lexical_environment_class_constructor.clone(),
                        vec![],
                    )
                    .set_original_node(Some(node), self)
                    .set_text_range(Some(&*node.ref_(self)), self);
                return Some(
                    self.factory
                        .ref_(self)
                        .update_tagged_template_expression(
                            node,
                            invocation,
                            Option::<Id<NodeArray>>::None,
                            visit_node(
                                node_as_tagged_template_expression.template,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node: Id<Node>| is_template_literal(&node.ref_(self))),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ),
                        )
                        .into(),
                );
            }
        }

        Some(
            visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .into(),
        )
    }

    pub(super) fn transform_class_static_block_declaration(
        &self,
        node: Id<Node>, /*ClassStaticBlockDeclaration*/
    ) -> Option<Id<Node>> {
        if self.should_transform_private_elements_or_class_static_blocks {
            if let Some(current_class_lexical_environment) =
                self.maybe_current_class_lexical_environment()
            {
                self.class_lexical_environment_map_mut().insert(
                    get_original_node_id(node, self),
                    current_class_lexical_environment,
                );
            }

            self.context.ref_(self).start_lexical_environment();
            let saved_current_static_property_declaration_or_static_block =
                self.maybe_current_static_property_declaration_or_static_block();
            self.set_current_static_property_declaration_or_static_block(Some(node));
            let mut statements = visit_nodes(
                released!(
                    node.ref_(self)
                        .as_class_static_block_declaration()
                        .body
                        .ref_(self)
                        .as_block()
                        .statements
                ),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_statement(node, self)),
                None,
                None,
                self,
            );
            statements = self
                .factory
                .ref_(self)
                .merge_lexical_environment(
                    statements,
                    self.context.ref_(self).end_lexical_environment().as_deref(),
                )
                .as_node_array();
            self.set_current_static_property_declaration_or_static_block(
                saved_current_static_property_declaration_or_static_block,
            );

            return Some(
                self.factory
                    .ref_(self)
                    .create_immediately_invoked_arrow_function(statements, None, None)
                    .set_original_node(Some(node), self)
                    .set_text_range(Some(&*node.ref_(self)), self)
                    .add_emit_flags(EmitFlags::AdviseOnEmitNode, self),
            );
        }
        None
    }

    pub(super) fn visit_binary_expression(
        &self,
        mut node: Id<Node>, /*BinaryExpression*/
        value_is_discarded: bool,
    ) -> VisitResult {
        if is_destructuring_assignment(node, self) {
            let saved_pending_expressions = self.maybe_pending_expressions().clone();
            self.set_pending_expressions(None);
            node = self.factory.ref_(self).update_binary_expression(
                node,
                visit_node(
                    released!(node.ref_(self).as_binary_expression().left),
                    Some(|node: Id<Node>| self.visitor_destructuring_target(node)),
                    Option::<fn(Id<Node>) -> bool>::None,
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
                released!(node.ref_(self).as_binary_expression().operator_token),
                visit_node(
                    released!(node.ref_(self).as_binary_expression().right),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Option::<fn(Id<Node>) -> bool>::None,
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
            );
            let expr = if self.maybe_pending_expressions().as_ref().is_non_empty() {
                self.factory.ref_(self).inline_expressions(
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
        if is_assignment_expression(node, None, self) {
            if self.should_transform_private_elements_or_class_static_blocks
                && is_private_identifier_property_access_expression(
                    node.ref_(self).as_binary_expression().left,
                    self,
                )
            {
                let node_left = node.ref_(self).as_binary_expression().left;
                let info = self.access_private_identifier(
                    node_left.ref_(self).as_property_access_expression().name,
                );
                if let Some(info) = info {
                    return Some(
                        self.create_private_identifier_assignment(
                            info,
                            released!(
                                node_left
                                    .ref_(self)
                                    .as_property_access_expression()
                                    .expression
                            ),
                            released!(node.ref_(self).as_binary_expression().right),
                            released!(node
                                .ref_(self)
                                .as_binary_expression()
                                .operator_token
                                .ref_(self)
                                .kind()),
                        )
                        .set_original_node(Some(node), self)
                        .set_text_range(Some(&*node.ref_(self)), self)
                        .into(),
                    );
                }
            } else if self.should_transform_super_in_static_initializers
                && is_super_property(node.ref_(self).as_binary_expression().left, self)
                && self
                    .maybe_current_static_property_declaration_or_static_block()
                    .is_some()
            {
                if let Some(current_class_lexical_environment) =
                    self.maybe_current_class_lexical_environment()
                {
                    let current_class_lexical_environment =
                        current_class_lexical_environment.ref_(self);
                    let class_constructor =
                        current_class_lexical_environment.class_constructor.as_ref();
                    let super_class_reference = current_class_lexical_environment
                        .super_class_reference
                        .as_ref();
                    let facts = current_class_lexical_environment.facts;
                    if facts.intersects(ClassFacts::ClassWasDecorated) {
                        return Some(
                            self.factory
                                .ref_(self)
                                .update_binary_expression(
                                    node,
                                    self.visit_invalid_super_property(
                                        node.ref_(self).as_binary_expression().left,
                                    ),
                                    node.ref_(self).as_binary_expression().operator_token,
                                    visit_node(
                                        node.ref_(self).as_binary_expression().right,
                                        Some(|node: Id<Node>| self.visitor(node)),
                                        Some(|node| is_expression(node, self)),
                                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                    ),
                                )
                                .into(),
                        );
                    }
                    if let Some(class_constructor) = class_constructor {
                        if let Some(super_class_reference) = super_class_reference {
                            let setter_name = if is_element_access_expression(
                                &node.ref_(self).as_binary_expression().left.ref_(self),
                            ) {
                                Some(visit_node(
                                    node.ref_(self)
                                        .as_binary_expression()
                                        .left
                                        .ref_(self)
                                        .as_element_access_expression()
                                        .argument_expression,
                                    Some(|node: Id<Node>| self.visitor(node)),
                                    Some(|node| is_expression(node, self)),
                                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                ))
                            } else if is_identifier(
                                &node
                                    .ref_(self)
                                    .as_binary_expression()
                                    .left
                                    .ref_(self)
                                    .as_property_access_expression()
                                    .name
                                    .ref_(self),
                            ) {
                                Some(self.factory.ref_(self).create_string_literal_from_node(
                                    released!(node.ref_(self)
                                            .as_binary_expression()
                                            .left
                                            .ref_(self)
                                            .as_property_access_expression()
                                            .name),
                                ))
                            } else {
                                None
                            };
                            if let Some(mut setter_name) = setter_name {
                                let mut expression = visit_node(
                                    node.ref_(self).as_binary_expression().right,
                                    Some(|node: Id<Node>| self.visitor(node)),
                                    Some(|node| is_expression(node, self)),
                                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                );
                                if is_compound_assignment(
                                    node.ref_(self)
                                        .as_binary_expression()
                                        .operator_token
                                        .ref_(self)
                                        .kind(),
                                ) {
                                    let mut getter_name = setter_name;
                                    if !is_simple_inlineable_expression(&setter_name.ref_(self)) {
                                        getter_name = self.factory.ref_(self).create_temp_variable(
                                            Some(|node: Id<Node>| {
                                                self.context
                                                    .ref_(self)
                                                    .hoist_variable_declaration(node);
                                            }),
                                            None,
                                        );
                                        setter_name = self
                                            .factory
                                            .ref_(self)
                                            .create_assignment(getter_name.clone(), setter_name);
                                    }
                                    let super_property_get = self
                                        .factory
                                        .ref_(self)
                                        .create_reflect_get_call(
                                            super_class_reference.clone(),
                                            getter_name,
                                            Some(class_constructor.clone()),
                                        )
                                        .set_original_node(
                                            Some(node.ref_(self).as_binary_expression().left),
                                            self,
                                        )
                                        .set_text_range(
                                            Some(
                                                &*node
                                                    .ref_(self)
                                                    .as_binary_expression()
                                                    .left
                                                    .ref_(self),
                                            ),
                                            self,
                                        );

                                    expression = self
                                        .factory
                                        .ref_(self)
                                        .create_binary_expression(
                                            super_property_get,
                                            get_non_assignment_operator_for_compound_assignment(
                                                node.ref_(self)
                                                    .as_binary_expression()
                                                    .operator_token
                                                    .ref_(self)
                                                    .kind(),
                                            ),
                                            expression,
                                        )
                                        .set_text_range(Some(&*node.ref_(self)), self);
                                }

                                let temp = (!value_is_discarded).then(|| {
                                    self.factory.ref_(self).create_temp_variable(
                                        Some(|node: Id<Node>| {
                                            self.context
                                                .ref_(self)
                                                .hoist_variable_declaration(node);
                                        }),
                                        None,
                                    )
                                });
                                if let Some(temp) = temp {
                                    expression =
                                        self.factory.ref_(self).create_assignment(temp, expression);
                                    set_text_range(&*temp.ref_(self), Some(&*node.ref_(self)));
                                }

                                expression = self
                                    .factory
                                    .ref_(self)
                                    .create_reflect_set_call(
                                        super_class_reference.clone(),
                                        setter_name,
                                        expression,
                                        Some(class_constructor.clone()),
                                    )
                                    .set_original_node(Some(node), self)
                                    .set_text_range(Some(&*node.ref_(self)), self);

                                if let Some(temp) = temp {
                                    expression = self
                                        .factory
                                        .ref_(self)
                                        .create_comma(expression, temp)
                                        .set_text_range(Some(&*node.ref_(self)), self);
                                }

                                return Some(expression.into());
                            }
                        }
                    }
                }
            }
        }
        if node
            .ref_(self)
            .as_binary_expression()
            .operator_token
            .ref_(self)
            .kind()
            == SyntaxKind::InKeyword
            && is_private_identifier(&node.ref_(self).as_binary_expression().left.ref_(self))
        {
            return self.visit_private_identifier_in_in_expression(node);
        }
        Some(
            visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .into(),
        )
    }

    pub(super) fn create_private_identifier_assignment(
        &self,
        info: Id<PrivateIdentifierInfo>,
        receiver: Id<Node>,   /*Expression*/
        right: Id<Node>,      /*Expression*/
        operator: SyntaxKind, /*AssignmentOperator*/
    ) -> Id<Node /*Expression*/> {
        let mut receiver = visit_node(
            receiver,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        let mut right = visit_node(
            right,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );

        if is_compound_assignment(operator) {
            let CopiableReceiverExpr {
                read_expression,
                initialize_expression,
            } = self.create_copiable_receiver_expr(receiver);
            receiver = initialize_expression
                .clone()
                .unwrap_or_else(|| read_expression.clone());
            right = self.factory.ref_(self).create_binary_expression(
                self.create_private_identifier_access_helper(info, read_expression),
                get_non_assignment_operator_for_compound_assignment(operator),
                right,
            );
        }

        set_comment_range(
            receiver,
            &ReadonlyTextRangeConcrete::from(move_range_pos(&*receiver.ref_(self), -1)),
            self,
        );

        match info.ref_(self).kind() {
            PrivateIdentifierKind::Accessor => self
                .context
                .ref_(self)
                .get_emit_helper_factory()
                .ref_(self)
                .create_class_private_field_set_helper(
                    receiver,
                    info.ref_(self).brand_check_identifier(),
                    right,
                    info.ref_(self).kind(),
                    info.ref_(self)
                        .as_private_identifier_accessor_info()
                        .setter_name
                        .clone(),
                ),
            PrivateIdentifierKind::Method => self
                .context
                .ref_(self)
                .get_emit_helper_factory()
                .ref_(self)
                .create_class_private_field_set_helper(
                    receiver,
                    info.ref_(self).brand_check_identifier(),
                    right,
                    info.ref_(self).kind(),
                    None,
                ),
            PrivateIdentifierKind::Field => self
                .context
                .ref_(self)
                .get_emit_helper_factory()
                .ref_(self)
                .create_class_private_field_set_helper(
                    receiver,
                    info.ref_(self).brand_check_identifier(),
                    right,
                    info.ref_(self).kind(),
                    info.ref_(self).maybe_variable_name(),
                ),
            // default:
            // Debug.assertNever(info, "Unknown private element type");
        }
    }

    pub(super) fn visit_class_like(
        &self,
        node: Id<Node>, /*ClassLikeDeclaration*/
    ) -> VisitResult {
        if !for_each_bool(
            &*node
                .ref_(self)
                .as_class_like_declaration()
                .members()
                .ref_(self),
            |&member: &Id<Node>, _| self.does_class_element_need_transform(member),
        ) {
            return Some(
                visit_each_child(
                    node,
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )
                .into(),
            );
        }

        let saved_pending_expressions = self.maybe_pending_expressions().clone();
        self.set_pending_expressions(None);
        self.start_class_lexical_environment();

        if self.should_transform_private_elements_or_class_static_blocks {
            let name = get_name_of_declaration(Some(node), self);
            if let Some(name) = name.filter(|name| is_identifier(&name.ref_(self))) {
                self.get_private_identifier_environment()
                    .ref_mut(self)
                    .class_name = id_text(&name.ref_(self)).to_owned();
            }

            let private_instance_methods_and_accessors =
                self.get_private_instance_methods_and_accessors(node);
            if !private_instance_methods_and_accessors.is_empty() {
                self.get_private_identifier_environment()
                    .ref_mut(self)
                    .weak_set_name = Some(self.create_hoisted_variable_for_class(
                    "instances",
                    released!(private_instance_methods_and_accessors[0]
                            .ref_(self)
                            .as_named_declaration()
                            .name()),
                ));
            }
        }

        let result = if is_class_declaration(&node.ref_(self)) {
            self.visit_class_declaration(node)
        } else {
            Some(self.visit_class_expression(node).into())
        };

        self.end_class_lexical_environment();
        self.set_pending_expressions(saved_pending_expressions);
        result
    }

    pub(super) fn does_class_element_need_transform(
        &self,
        node: Id<Node>, /*ClassElement*/
    ) -> bool {
        is_property_declaration(&node.ref_(self))
            || is_class_static_block_declaration(&node.ref_(self))
            || self.should_transform_private_elements_or_class_static_blocks
                && node
                    .ref_(self)
                    .as_named_declaration()
                    .maybe_name()
                    .matches(|node_name| is_private_identifier(&node_name.ref_(self)))
    }

    pub(super) fn get_private_instance_methods_and_accessors(
        &self,
        node: Id<Node>, /*ClassLikeDeclaration*/
    ) -> Vec<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_class_like_declaration = node_ref.as_class_like_declaration();
        filter(
            &*node_as_class_like_declaration.members().ref_(self),
            |&member: &Id<Node>| is_non_static_method_or_accessor_with_private_name(member, self),
        )
    }

    pub(super) fn get_class_facts(
        &self,
        node: Id<Node>, /*ClassLikeDeclaration*/
    ) -> ClassFacts {
        let node_ref = node.ref_(self);
        let node_as_class_like_declaration = node_ref.as_class_like_declaration();
        let mut facts = ClassFacts::None;
        let original = get_original_node(node, self);
        if is_class_declaration(&original.ref_(self))
            && class_or_constructor_parameter_is_decorated(original, self)
        {
            facts |= ClassFacts::ClassWasDecorated;
        }
        for &member in &*node_as_class_like_declaration.members().ref_(self) {
            if !is_static(member, self) {
                continue;
            }
            if member
                .ref_(self)
                .as_named_declaration()
                .maybe_name()
                .matches(|member_name| is_private_identifier(&member_name.ref_(self)))
                && self.should_transform_private_elements_or_class_static_blocks
            {
                facts |= ClassFacts::NeedsClassConstructorReference;
            }
            if is_property_declaration(&member.ref_(self))
                || is_class_static_block_declaration(&member.ref_(self))
            {
                if self.should_transform_this_in_static_initializers
                    && member
                        .ref_(self)
                        .transform_flags()
                        .intersects(TransformFlags::ContainsLexicalThis)
                {
                    facts |= ClassFacts::NeedsSubstitutionForThisInClassStaticField;
                    if !facts.intersects(ClassFacts::ClassWasDecorated) {
                        facts |= ClassFacts::NeedsClassConstructorReference;
                    }
                }
                if self.should_transform_super_in_static_initializers
                    && member
                        .ref_(self)
                        .transform_flags()
                        .intersects(TransformFlags::ContainsLexicalSuper)
                {
                    if !facts.intersects(ClassFacts::ClassWasDecorated) {
                        facts |= ClassFacts::NeedsClassConstructorReference
                            | ClassFacts::NeedsClassSuperReference;
                    }
                }
            }
        }
        facts
    }

    pub(super) fn visit_expression_with_type_arguments(
        &self,
        node: Id<Node>, /*ExpressionWithTypeArguments*/
    ) -> VisitResult {
        let facts = self
            .maybe_current_class_lexical_environment()
            .map_or_default(|current_class_lexical_environment| {
                current_class_lexical_environment.ref_(self).facts
            });
        if facts.intersects(ClassFacts::NeedsClassSuperReference) {
            let temp = self.factory.ref_(self).create_temp_variable(
                Some(|node: Id<Node>| {
                    self.context.ref_(self).hoist_variable_declaration(node);
                }),
                Some(true),
            );
            self.get_class_lexical_environment()
                .ref_mut(self)
                .super_class_reference = Some(temp.clone());
            return Some(
                self.factory
                    .ref_(self)
                    .update_expression_with_type_arguments(
                        node,
                        self.factory.ref_(self).create_assignment(
                            temp,
                            visit_node(
                                released!(
                                    node.ref_(self)
                                        .as_expression_with_type_arguments()
                                        .expression
                                ),
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ),
                        ),
                        Option::<Id<NodeArray>>::None,
                    )
                    .into(),
            );
        }
        Some(
            visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .into(),
        )
    }
}

pub(super) struct CopiableReceiverExpr {
    pub read_expression: Id<Node /*Expression*/>,
    pub initialize_expression: Option<Id<Node /*Expression*/>>,
}
