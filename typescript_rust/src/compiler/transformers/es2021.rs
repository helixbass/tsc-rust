use std::{any::Any, io};

use id_arena::Id;

use crate::{
    chain_bundle, get_non_assignment_operator_for_compound_assignment, impl_has_arena,
    is_access_expression, is_expression, is_left_hand_side_expression,
    is_logical_or_coalescing_assignment_expression, is_property_access_expression,
    is_simple_copiable_expression, maybe_visit_each_child, released, skip_parentheses,
    visit_each_child, visit_node, AllArenas, CoreTransformationContext, HasArena, InArena, Node,
    NodeFactory, NodeInterface, SyntaxKind, TransformFlags, TransformNodesTransformationResult,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface,
    VisitResult,
};

struct TransformES2021 {
    arena: *const AllArenas,
    context: Id<TransformNodesTransformationResult>,
    factory: Id<NodeFactory>,
}

impl TransformES2021 {
    fn new(context: Id<TransformNodesTransformationResult>, arena: *const AllArenas) -> Self {
        let arena_ref = unsafe { &*arena };
        let context_ref = context.ref_(arena_ref);
        Self {
            arena,
            factory: context_ref.factory(),
            context,
        }
    }

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node> {
        if node.ref_(self).as_source_file().is_declaration_file() {
            return node;
        }

        visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }

    fn visitor(&self, node: Id<Node>) -> VisitResult {
        if !node
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsES2021)
        {
            return Some(node.into());
        }
        match released!(node.ref_(self).kind()) {
            SyntaxKind::BinaryExpression => {
                let binary_expression = node;
                if is_logical_or_coalescing_assignment_expression(binary_expression, self) {
                    return self.transform_logical_assignment(binary_expression);
                }
                maybe_visit_each_child(
                    Some(node),
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )
                .map(Into::into)
            }
            _ => maybe_visit_each_child(
                Some(node),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .map(Into::into),
        }
    }

    fn transform_logical_assignment(
        &self,
        binary_expression: Id<Node>, /*AssignmentExpression<Token<LogicalOrCoalescingAssignmentOperator>>*/
    ) -> VisitResult {
        let operator = binary_expression
            .ref_(self)
            .as_binary_expression()
            .operator_token;
        let non_assignment_operator =
            get_non_assignment_operator_for_compound_assignment(operator.ref_(self).kind());
        let mut left = skip_parentheses(
            visit_node(
                binary_expression.ref_(self).as_binary_expression().left,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_left_hand_side_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ),
            None,
            self,
        );
        let mut assignment_target = left;
        let right = skip_parentheses(
            visit_node(
                binary_expression.ref_(self).as_binary_expression().right,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ),
            None,
            self,
        );

        if is_access_expression(&left.ref_(self)) {
            let left_expression = left.ref_(self).as_has_expression().expression();
            let property_access_target_simple_copiable =
                is_simple_copiable_expression(&left_expression.ref_(self));
            let property_access_target = if property_access_target_simple_copiable {
                left_expression
            } else {
                self.factory.ref_(self).create_temp_variable(
                    Some(|node: Id<Node>| {
                        self.context.ref_(self).hoist_variable_declaration(node);
                    }),
                    None,
                )
            };
            let property_access_target_assignment = if property_access_target_simple_copiable {
                left_expression
            } else {
                self.factory
                    .ref_(self)
                    .create_assignment(property_access_target.clone(), left_expression.clone())
            };

            if is_property_access_expression(&left.ref_(self)) {
                let left_ref = left.ref_(self);
                let left_as_property_access_expression = left_ref.as_property_access_expression();
                assignment_target = self.factory.ref_(self).create_property_access_expression(
                    property_access_target,
                    left_as_property_access_expression.name.clone(),
                );
                left = self.factory.ref_(self).create_property_access_expression(
                    property_access_target_assignment,
                    left_as_property_access_expression.name.clone(),
                );
            } else {
                let element_access_argument_simple_copiable = is_simple_copiable_expression(
                    &left
                        .ref_(self)
                        .as_element_access_expression()
                        .argument_expression
                        .ref_(self),
                );
                let element_access_argument = if element_access_argument_simple_copiable {
                    left.ref_(self)
                        .as_element_access_expression()
                        .argument_expression
                        .clone()
                } else {
                    self.factory.ref_(self).create_temp_variable(
                        Some(|node: Id<Node>| {
                            self.context.ref_(self).hoist_variable_declaration(node);
                        }),
                        None,
                    )
                };

                assignment_target = self.factory.ref_(self).create_element_access_expression(
                    property_access_target,
                    element_access_argument.clone(),
                );
                left = self.factory.ref_(self).create_element_access_expression(
                    property_access_target_assignment,
                    if element_access_argument_simple_copiable {
                        left.ref_(self)
                            .as_element_access_expression()
                            .argument_expression
                            .clone()
                    } else {
                        self.factory.ref_(self).create_assignment(
                            element_access_argument,
                            left.ref_(self)
                                .as_element_access_expression()
                                .argument_expression
                                .clone(),
                        )
                    },
                );
            }
        }

        Some(
            self.factory
                .ref_(self)
                .create_binary_expression(
                    left,
                    non_assignment_operator,
                    self.factory.ref_(self).create_parenthesized_expression(
                        self.factory
                            .ref_(self)
                            .create_assignment(assignment_target, right),
                    ),
                )
                .into(),
        )
    }
}

impl TransformerInterface for TransformES2021 {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(self.transform_source_file(node))
    }

    fn as_dyn_any(&self) -> &dyn Any {
        self
    }
}

impl_has_arena!(TransformES2021);

struct TransformES2021Factory {
    arena: *const AllArenas,
}

impl TransformES2021Factory {
    fn new(arena: &impl HasArena) -> Self {
        Self {
            arena: arena.arena(),
        }
    }
}

impl TransformerFactoryInterface for TransformES2021Factory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle(self).ref_(self).call(
            context,
            self.alloc_transformer(Box::new(TransformES2021::new(context, self.arena))),
        )
    }
}

impl_has_arena!(TransformES2021Factory);

pub fn transform_es2021(arena: &impl HasArena) -> TransformerFactory {
    arena.alloc_transformer_factory(Box::new(TransformES2021Factory::new(arena)))
}
