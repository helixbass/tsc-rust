use gc::Gc;

use super::{ClassFacts, PrivateIdentifierInfo, TransformClassFields};
use crate::{
    expand_pre_or_postfix_increment_or_decrement_expression, is_expression, is_identifier,
    is_prefix_unary_expression, is_private_identifier_property_access_expression,
    is_property_access_expression, is_simple_inlineable_expression, is_super_property,
    visit_each_child, visit_node, NamedDeclarationInterface, Node, NodeExt, NodeInterface,
    SyntaxKind, VisitResult,
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

    pub(super) fn visit_for_statement(&self, _node: &Node /*ForStatement*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_expression_statement(
        &self,
        _node: &Node, /*ExpressionStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn create_copiable_receiver_expr(
        &self,
        _receiver: &Node, /*Expression*/
    ) -> CopiableReceiverExpr {
        unimplemented!()
    }

    pub(super) fn visit_call_expression(
        &self,
        _node: &Node, /*CallExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_tagged_template_expression(
        &self,
        _node: &Node, /*TaggedTemplateExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_binary_expression(
        &self,
        _node: &Node, /*BinaryExpression*/
        _value_is_discarded: bool,
    ) -> VisitResult {
        unimplemented!()
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
