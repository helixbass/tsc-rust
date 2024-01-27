use std::{io, mem, any::Any};

use gc::{Finalize, Gc, GcCell, Trace};
use id_arena::Id;

use crate::{
    cast, chain_bundle, is_call_chain, is_expression, is_identifier, is_non_null_chain,
    is_optional_chain, is_parenthesized_expression, is_simple_copiable_expression,
    is_synthetic_reference, is_tagged_template_expression, maybe_visit_each_child,
    set_original_node, skip_parentheses, skip_partially_emitted_expressions, visit_each_child,
    visit_node, visit_nodes, BaseNodeFactorySynthetic, Debug_, Node, NodeArray, NodeExt,
    NodeFactory, NodeInterface, SyntaxKind, TransformFlags, TransformationContext, Transformer,
    TransformerFactory, TransformerFactoryInterface, TransformerInterface, VisitResult,
    HasArena, AllArenas, InArena, static_arena,
    TransformNodesTransformationResult, CoreTransformationContext,
};

#[derive(Trace, Finalize)]
struct TransformES2020 {
    #[unsafe_ignore_trace]
    _arena: *const AllArenas,
    context: Id<TransformNodesTransformationResult>,
    factory: Id<NodeFactory>,
}

impl TransformES2020 {
    fn new(context: Id<TransformNodesTransformationResult>, arena: *const AllArenas) -> Transformer {
        let arena_ref = unsafe { &*arena };
        let context_ref = context.ref_(arena_ref);
        arena_ref.alloc_transformer(Box::new(Self {
            _arena: arena,
            factory: context_ref.factory(),
            context,
        }))
    }

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node> {
        if node.ref_(self).as_source_file().is_declaration_file() {
            return node;
        }

        visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self)
    }

    fn visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        if !node
            .ref_(self).transform_flags()
            .intersects(TransformFlags::ContainsES2020)
        {
            return Some(node.into());
        }
        match node.ref_(self).kind() {
            SyntaxKind::CallExpression => {
                let updated = self.visit_non_optional_call_expression(node, false);
                Debug_.assert_not_node(Some(updated), Some(|node: Id<Node>| is_synthetic_reference(&node.ref_(self))), None);
                Some(updated.into())
            }
            SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
                if is_optional_chain(&node.ref_(self)) {
                    let updated = self.visit_optional_expression(node, false, false);
                    Debug_.assert_not_node(Some(updated), Some(|node: Id<Node>| is_synthetic_reference(&node.ref_(self))), None);
                    return Some(updated.into());
                }
                maybe_visit_each_child(
                    Some(node),
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )
                .map(Into::into)
            }
            SyntaxKind::BinaryExpression => {
                if node.ref_(self).as_binary_expression().operator_token.ref_(self).kind()
                    == SyntaxKind::QuestionQuestionToken
                {
                    return self.transform_nullish_coalescing_expression(node);
                }
                maybe_visit_each_child(
                    Some(node),
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )
                .map(Into::into)
            }
            SyntaxKind::DeleteExpression => self.visit_delete_expression(node),
            _ => maybe_visit_each_child(
                Some(node),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .map(Into::into),
        }
    }

    fn flatten_chain(&self, mut chain: Id<Node> /*OptionalChain*/) -> FlattenChainReturn {
        Debug_.assert_not_node(Some(chain), Some(|node: Id<Node>| is_non_null_chain(&node.ref_(self))), None);
        let mut links: Vec<Id<Node /*OptionalChain*/>> = vec![chain];
        while chain
            .ref_(self).as_has_question_dot_token()
            .maybe_question_dot_token()
            .is_none()
            && !is_tagged_template_expression(&chain.ref_(self))
        {
            chain = cast(
                Some(skip_partially_emitted_expressions(
                    chain.ref_(self).as_has_expression().expression(),
                    self,
                )),
                |value: &Id<Node>| is_optional_chain(&value.ref_(self)),
            );
            Debug_.assert_not_node(Some(chain), Some(|node: Id<Node>| is_non_null_chain(&node.ref_(self))), None);
            links.insert(0, chain);
        }
        FlattenChainReturn {
            expression: chain.ref_(self).as_has_expression().expression(),
            chain: links,
        }
    }

    fn visit_non_optional_parenthesized_expression(
        &self,
        node: Id<Node>, /*ParenthesizedExpression*/
        capture_this_arg: bool,
        is_delete: bool,
    ) -> Id<Node /*Expression*/> {
        let expression = self.visit_non_optional_expression(
            node.ref_(self).as_parenthesized_expression().expression,
            capture_this_arg,
            is_delete,
        );
        if is_synthetic_reference(&expression.ref_(self)) {
            let expression_ref = expression.ref_(self);
            let expression_as_synthetic_reference_expression = expression_ref.as_synthetic_reference_expression();
            return self.factory.ref_(self).create_synthetic_reference_expression(
                self.factory.ref_(self).update_parenthesized_expression(
                    node,
                    expression_as_synthetic_reference_expression
                        .expression
                        .clone(),
                ),
                expression_as_synthetic_reference_expression
                    .this_arg
                    .clone(),
            );
        }
        self.factory
            .ref_(self).update_parenthesized_expression(node, expression)
    }

    fn visit_non_optional_property_or_element_access_expression(
        &self,
        node: Id<Node>, /*AccessExpression*/
        capture_this_arg: bool,
        is_delete: bool,
    ) -> Id<Node /*Expression*/> {
        if is_optional_chain(&node.ref_(self)) {
            return self.visit_optional_expression(node, capture_this_arg, is_delete);
        }

        let mut expression = visit_node(
            node.ref_(self).as_has_expression().expression(),
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        Debug_.assert_not_node(Some(expression), Some(|node: Id<Node>| is_synthetic_reference(&node.ref_(self))), None);

        let mut this_arg: Option<Id<Node /*Expression*/>> = Default::default();
        if capture_this_arg {
            if !is_simple_copiable_expression(&expression.ref_(self)) {
                this_arg = Some(self.factory.ref_(self).create_temp_variable(
                    Some(|node: Id<Node>| {
                        self.context.ref_(self).hoist_variable_declaration(node);
                    }),
                    None,
                ));
                expression = self
                    .factory
                    .ref_(self).create_assignment(this_arg.clone().unwrap(), expression);
            } else {
                this_arg = Some(expression);
            }
        }

        expression = if node.ref_(self).kind() == SyntaxKind::PropertyAccessExpression {
            self.factory.ref_(self).update_property_access_expression(
                node,
                expression.clone(),
                visit_node(
                    node.ref_(self).as_property_access_expression().name,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node: Id<Node>| is_identifier(&node.ref_(self))),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
            )
        } else {
            self.factory.ref_(self).update_element_access_expression(
                node,
                expression.clone(),
                visit_node(
                    node.ref_(self).as_element_access_expression().argument_expression,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
            )
        };
        if let Some(this_arg) = this_arg {
            self.factory
                .ref_(self).create_synthetic_reference_expression(expression, this_arg)
        } else {
            expression
        }
    }

    fn visit_non_optional_call_expression(
        &self,
        node: Id<Node>, /*CallExpression*/
        capture_this_arg: bool,
    ) -> Id<Node /*Expression*/> {
        let node_ref = node.ref_(self);
        let node_as_call_expression = node_ref.as_call_expression();
        if is_optional_chain(&node.ref_(self)) {
            return self.visit_optional_expression(node, capture_this_arg, false);
        }
        if is_parenthesized_expression(&node_as_call_expression.expression.ref_(self))
            && is_optional_chain(&skip_parentheses(node_as_call_expression.expression, None, self).ref_(self))
        {
            let expression = self.visit_non_optional_parenthesized_expression(
                node_as_call_expression.expression,
                true,
                false,
            );
            let args = visit_nodes(
                &node_as_call_expression.arguments,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                None,
                None,
                self,
            );
            if is_synthetic_reference(&expression.ref_(self)) {
                let expression_ref = expression.ref_(self);
                let expression_as_synthetic_reference_expression = expression_ref.as_synthetic_reference_expression();
                return self
                    .factory
                    .ref_(self).create_function_call_call(
                        expression_as_synthetic_reference_expression
                            .expression
                            .clone(),
                        expression_as_synthetic_reference_expression
                            .this_arg
                            .clone(),
                        args,
                    )
                    .set_text_range(Some(&*node.ref_(self)), self);
            }
            return self.factory.ref_(self).update_call_expression(
                node,
                expression,
                Option::<Gc<NodeArray>>::None,
                args,
            );
        }
        visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self)
    }

    fn visit_non_optional_expression(
        &self,
        node: Id<Node>, /*OptionalChain*/
        capture_this_arg: bool,
        is_delete: bool,
    ) -> Id<Node /*Expression*/> {
        match node.ref_(self).kind() {
            SyntaxKind::ParenthesizedExpression => {
                self.visit_non_optional_parenthesized_expression(node, capture_this_arg, is_delete)
            }
            SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => self
                .visit_non_optional_property_or_element_access_expression(
                    node,
                    capture_this_arg,
                    is_delete,
                ),
            SyntaxKind::CallExpression => {
                self.visit_non_optional_call_expression(node, capture_this_arg)
            }
            _ => visit_node(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            ),
        }
    }

    fn visit_optional_expression(
        &self,
        node: Id<Node>, /*OptionalChain*/
        capture_this_arg: bool,
        is_delete: bool,
    ) -> Id<Node /*Expression*/> {
        let FlattenChainReturn { expression, chain } = self.flatten_chain(node);
        let left = self.visit_non_optional_expression(expression, is_call_chain(&chain[0].ref_(self)), false);
        let left_this_arg = is_synthetic_reference(&left.ref_(self))
            .then(|| left.ref_(self).as_synthetic_reference_expression().this_arg);
        let mut left_expression = if is_synthetic_reference(&left.ref_(self)) {
            left.ref_(self).as_synthetic_reference_expression().expression
        } else {
            left
        };
        let mut captured_left/*Expression*/ = left_expression;
        if !is_simple_copiable_expression(&left_expression.ref_(self)) {
            captured_left = self.factory.ref_(self).create_temp_variable(
                Some(|node: Id<Node>| {
                    self.context.ref_(self).hoist_variable_declaration(node);
                }),
                None,
            );
            left_expression = self
                .factory
                .ref_(self).create_assignment(captured_left.clone(), left_expression.clone());
        }
        let mut right_expression = captured_left;
        let mut this_arg: Option<Id<Node /*Expression*/>> = Default::default();
        for (i, segment) in chain.iter().enumerate() {
            let segment = *segment;
            match segment.ref_(self).kind() {
                SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
                    if i == chain.len() - 1 && capture_this_arg {
                        if !is_simple_copiable_expression(&right_expression.ref_(self)) {
                            this_arg = Some(self.factory.ref_(self).create_temp_variable(
                                Some(|node: Id<Node>| {
                                    self.context.ref_(self).hoist_variable_declaration(node);
                                }),
                                None,
                            ));
                            right_expression = self
                                .factory
                                .ref_(self).create_assignment(this_arg.clone().unwrap(), right_expression);
                        } else {
                            this_arg = Some(right_expression.clone());
                        }
                    }
                    right_expression = if segment.ref_(self).kind() == SyntaxKind::PropertyAccessExpression {
                        self.factory.ref_(self).create_property_access_expression(
                            right_expression,
                            visit_node(
                                segment.ref_(self).as_property_access_expression().name,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node: Id<Node>| is_identifier(&node.ref_(self))),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ),
                        )
                    } else {
                        self.factory.ref_(self).create_element_access_expression(
                            right_expression,
                            visit_node(
                                segment.ref_(self).as_element_access_expression().argument_expression,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ),
                        )
                    };
                }
                SyntaxKind::CallExpression => {
                    if i == 0 && left_this_arg.is_some() {
                        let left_this_arg = left_this_arg.unwrap();
                        right_expression = self.factory.ref_(self).create_function_call_call(
                            right_expression,
                            if left_this_arg.ref_(self).kind() == SyntaxKind::SuperKeyword {
                                self.factory.ref_(self).create_this()
                            } else {
                                left_this_arg.clone()
                            },
                            visit_nodes(
                                &segment.ref_(self).as_call_expression().arguments,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                None,
                                None,
                                self,
                            ),
                        );
                    } else {
                        right_expression = self.factory.ref_(self).create_call_expression(
                            right_expression,
                            Option::<Gc<NodeArray>>::None,
                            Some(visit_nodes(
                                &segment.ref_(self).as_call_expression().arguments,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                None,
                                None,
                                self,
                            )),
                        );
                    }
                }
                _ => (),
            }
            set_original_node(right_expression, Some(segment), self);
        }

        let target = if is_delete {
            self.factory.ref_(self).create_conditional_expression(
                self.create_not_null_condition(left_expression, captured_left, Some(true)),
                None,
                self.factory.ref_(self).create_true(),
                None,
                self.factory.ref_(self).create_delete_expression(right_expression),
            )
        } else {
            self.factory.ref_(self).create_conditional_expression(
                self.create_not_null_condition(left_expression, captured_left, Some(true)),
                None,
                self.factory.ref_(self).create_void_zero(),
                None,
                right_expression,
            )
        }
        .set_text_range(Some(&*node.ref_(self)), self);
        if let Some(this_arg) = this_arg {
            self.factory
                .ref_(self).create_synthetic_reference_expression(target, this_arg)
        } else {
            target
        }
    }

    fn create_not_null_condition(
        &self,
        left: Id<Node>,  /*Expression*/
        right: Id<Node>, /*Expression*/
        invert: Option<bool>,
    ) -> Id<Node> {
        self.factory.ref_(self).create_binary_expression(
            self.factory.ref_(self).create_binary_expression(
                left,
                self.factory.ref_(self).create_token(if invert == Some(true) {
                    SyntaxKind::EqualsEqualsEqualsToken
                } else {
                    SyntaxKind::ExclamationEqualsEqualsToken
                }),
                self.factory.ref_(self).create_null(),
            ),
            self.factory.ref_(self).create_token(if invert == Some(true) {
                SyntaxKind::BarBarToken
            } else {
                SyntaxKind::AmpersandAmpersandToken
            }),
            self.factory.ref_(self).create_binary_expression(
                right,
                self.factory.ref_(self).create_token(if invert == Some(true) {
                    SyntaxKind::EqualsEqualsEqualsToken
                } else {
                    SyntaxKind::ExclamationEqualsEqualsToken
                }),
                self.factory.ref_(self).create_void_zero(),
            ),
        )
    }

    fn transform_nullish_coalescing_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        let mut left = visit_node(
            node_as_binary_expression.left,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        let mut right = left;
        if !is_simple_copiable_expression(&left.ref_(self)) {
            right = self.factory.ref_(self).create_temp_variable(
                Some(|node: Id<Node>| {
                    self.context.ref_(self).hoist_variable_declaration(node);
                }),
                None,
            );
            left = self.factory.ref_(self).create_assignment(right.clone(), left);
        }
        Some(
            self.factory
                .ref_(self).create_conditional_expression(
                    self.create_not_null_condition(left, right.clone(), None),
                    None,
                    right,
                    None,
                    visit_node(
                        node_as_binary_expression.right,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                )
                .set_text_range(Some(&*node.ref_(self)), self)
                .into(),
        )
    }

    fn visit_delete_expression(&self, node: Id<Node> /*DeleteExpression*/) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_delete_expression = node_ref.as_delete_expression();
        Some(
            if is_optional_chain(&skip_parentheses(
                node_as_delete_expression.expression,
                None,
                self,
            ).ref_(self)) {
                self.visit_non_optional_expression(
                    node_as_delete_expression.expression,
                    false,
                    true,
                )
                .set_original_node(Some(node), self)
            } else {
                self.factory.ref_(self).update_delete_expression(
                    node,
                    visit_node(
                        node_as_delete_expression.expression,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                )
            }
            .into(),
        )
    }
}

struct FlattenChainReturn {
    pub expression: Id<Node>,
    pub chain: Vec<Id<Node>>,
}

impl TransformerInterface for TransformES2020 {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(self.transform_source_file(node))
    }

    fn as_dyn_any(&self) -> &dyn Any {
        unimplemented!()
    }
}

impl HasArena for TransformES2020 {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES2020Factory {}

impl TransformES2020Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2020Factory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle(self).ref_(self).call(
            context,
            TransformES2020::new(context, &*static_arena()),
        )
    }
}

pub fn transform_es2020(arena: &impl HasArena) -> TransformerFactory {
    arena.alloc_transformer_factory(Box::new(TransformES2020Factory::new()))
}
