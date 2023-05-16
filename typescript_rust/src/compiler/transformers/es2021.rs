use std::io;

use gc::{Finalize, Gc, Trace};

use crate::{
    chain_bundle, get_non_assignment_operator_for_compound_assignment, is_access_expression,
    is_expression, is_left_hand_side_expression, is_logical_or_coalescing_assignment_expression,
    is_property_access_expression, is_simple_copiable_expression, skip_parentheses,
    visit_each_child, visit_node, BaseNodeFactorySynthetic, Node, NodeArray, NodeFactory,
    NodeInterface, SyntaxKind, TransformFlags, TransformationContext, Transformer,
    TransformerFactory, TransformerFactoryInterface, TransformerInterface, VisitResult,
};

#[derive(Trace, Finalize)]
struct TransformES2021 {
    context: Gc<Box<dyn TransformationContext>>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
}

impl TransformES2021 {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self {
            factory: context.factory(),
            context,
        }
    }

    fn transform_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        if node.as_source_file().is_declaration_file() {
            return node.node_wrapper();
        }

        visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
            Option::<
                fn(
                    Option<&NodeArray>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<usize>,
                    Option<usize>,
                ) -> Option<Gc<NodeArray>>,
            >::None,
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
        .unwrap()
    }

    fn visitor(&self, node: &Node) -> VisitResult {
        if !node
            .transform_flags()
            .intersects(TransformFlags::ContainsES2021)
        {
            return Some(node.node_wrapper().into());
        }
        match node.kind() {
            SyntaxKind::BinaryExpression => {
                let binary_expression = node;
                if is_logical_or_coalescing_assignment_expression(binary_expression) {
                    return self.transform_logical_assignment(binary_expression);
                }
                visit_each_child(
                    Some(node),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                    Option::<
                        fn(
                            Option<&NodeArray>,
                            Option<&mut dyn FnMut(&Node) -> VisitResult>,
                            Option<&dyn Fn(&Node) -> bool>,
                            Option<usize>,
                            Option<usize>,
                        ) -> Option<Gc<NodeArray>>,
                    >::None,
                    Option::<fn(&Node) -> VisitResult>::None,
                    Option::<
                        fn(
                            Option<&Node>,
                            Option<&mut dyn FnMut(&Node) -> VisitResult>,
                            Option<&dyn Fn(&Node) -> bool>,
                            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                        ) -> Option<Gc<Node>>,
                    >::None,
                )
                .map(Into::into)
            }
            _ => visit_each_child(
                Some(node),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
                Option::<fn(&Node) -> VisitResult>::None,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            )
            .map(Into::into),
        }
    }

    fn transform_logical_assignment(
        &self,
        binary_expression: &Node, /*AssignmentExpression<Token<LogicalOrCoalescingAssignmentOperator>>*/
    ) -> VisitResult {
        let binary_expression_as_binary_expression = binary_expression.as_binary_expression();
        let operator = &binary_expression_as_binary_expression.operator_token;
        let non_assignment_operator =
            get_non_assignment_operator_for_compound_assignment(operator.kind());
        let mut left = skip_parentheses(
            &visit_node(
                Some(&*binary_expression_as_binary_expression.left),
                Some(|node: &Node| self.visitor(node)),
                Some(is_left_hand_side_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )
            .unwrap(),
            None,
        );
        let mut assignment_target = left.clone();
        let right = skip_parentheses(
            &visit_node(
                Some(&*binary_expression_as_binary_expression.right),
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )
            .unwrap(),
            None,
        );

        if is_access_expression(&left) {
            let ref left_expression = left.as_has_expression().expression();
            let property_access_target_simple_copiable =
                is_simple_copiable_expression(left_expression);
            let property_access_target = if property_access_target_simple_copiable {
                left_expression.clone()
            } else {
                self.factory.create_temp_variable(
                    Some(|node: &Node| {
                        self.context.hoist_variable_declaration(node);
                    }),
                    None,
                )
            };
            let property_access_target_assignment = if property_access_target_simple_copiable {
                left_expression.clone()
            } else {
                self.factory
                    .create_assignment(property_access_target.clone(), left_expression.clone())
                    .wrap()
            };

            if is_property_access_expression(&left) {
                let left_as_property_access_expression = left.as_property_access_expression();
                assignment_target = self
                    .factory
                    .create_property_access_expression(
                        property_access_target,
                        left_as_property_access_expression.name.clone(),
                    )
                    .wrap();
                left = self
                    .factory
                    .create_property_access_expression(
                        property_access_target_assignment,
                        left_as_property_access_expression.name.clone(),
                    )
                    .wrap();
            } else {
                let left_as_element_access_expression = left.as_element_access_expression();
                let element_access_argument_simple_copiable = is_simple_copiable_expression(
                    &left_as_element_access_expression.argument_expression,
                );
                let element_access_argument = if element_access_argument_simple_copiable {
                    left_as_element_access_expression
                        .argument_expression
                        .clone()
                } else {
                    self.factory.create_temp_variable(
                        Some(|node: &Node| {
                            self.context.hoist_variable_declaration(node);
                        }),
                        None,
                    )
                };

                assignment_target = self
                    .factory
                    .create_element_access_expression(
                        property_access_target,
                        element_access_argument.clone(),
                    )
                    .wrap();
                left = self
                    .factory
                    .create_element_access_expression(
                        property_access_target_assignment,
                        if element_access_argument_simple_copiable {
                            left_as_element_access_expression
                                .argument_expression
                                .clone()
                        } else {
                            self.factory
                                .create_assignment(
                                    element_access_argument,
                                    left_as_element_access_expression
                                        .argument_expression
                                        .clone(),
                                )
                                .wrap()
                        },
                    )
                    .wrap();
            }
        }

        Some(
            self.factory
                .create_binary_expression(
                    left,
                    non_assignment_operator,
                    self.factory
                        .create_parenthesized_expression(
                            self.factory
                                .create_assignment(assignment_target, right)
                                .wrap(),
                        )
                        .wrap(),
                )
                .wrap()
                .into(),
        )
    }
}

impl TransformerInterface for TransformES2021 {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        Ok(self.transform_source_file(node))
    }
}

#[derive(Trace, Finalize)]
struct TransformES2021Factory {}

impl TransformES2021Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2021Factory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            Gc::new(Box::new(TransformES2021::new(context))),
        )
    }
}

pub fn transform_es2021() -> TransformerFactory {
    Gc::new(Box::new(TransformES2021Factory::new()))
}
