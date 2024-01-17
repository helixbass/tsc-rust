use std::{io, mem};

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
};

#[derive(Trace, Finalize)]
struct TransformES2020 {
    _transformer_wrapper: GcCell<Option<Transformer>>,
    context: Gc<Box<dyn TransformationContext>>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
}

impl TransformES2020 {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: Default::default(),
            factory: context.factory(),
            context,
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        downcasted
    }

    fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node> {
        if node.as_source_file().is_declaration_file() {
            return node.node_wrapper();
        }

        visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context)
    }

    fn visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        if !node
            .transform_flags()
            .intersects(TransformFlags::ContainsES2020)
        {
            return Some(node.node_wrapper().into());
        }
        match node.kind() {
            SyntaxKind::CallExpression => {
                let updated = self.visit_non_optional_call_expression(node, false);
                Debug_.assert_not_node(Some(&*updated), Some(is_synthetic_reference), None);
                Some(updated.into())
            }
            SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
                if is_optional_chain(node) {
                    let updated = self.visit_optional_expression(node, false, false);
                    Debug_.assert_not_node(Some(&*updated), Some(is_synthetic_reference), None);
                    return Some(updated.into());
                }
                maybe_visit_each_child(
                    Some(node),
                    |node: Id<Node>| self.visitor(node),
                    &**self.context,
                )
                .map(Into::into)
            }
            SyntaxKind::BinaryExpression => {
                if node.as_binary_expression().operator_token.kind()
                    == SyntaxKind::QuestionQuestionToken
                {
                    return self.transform_nullish_coalescing_expression(node);
                }
                maybe_visit_each_child(
                    Some(node),
                    |node: Id<Node>| self.visitor(node),
                    &**self.context,
                )
                .map(Into::into)
            }
            SyntaxKind::DeleteExpression => self.visit_delete_expression(node),
            _ => maybe_visit_each_child(
                Some(node),
                |node: Id<Node>| self.visitor(node),
                &**self.context,
            )
            .map(Into::into),
        }
    }

    fn flatten_chain(&self, chain: Id<Node> /*OptionalChain*/) -> FlattenChainReturn {
        Debug_.assert_not_node(Some(chain), Some(is_non_null_chain), None);
        let mut chain = chain.node_wrapper();
        let mut links: Vec<Id<Node /*OptionalChain*/>> = vec![chain.clone()];
        while chain
            .as_has_question_dot_token()
            .maybe_question_dot_token()
            .is_none()
            && !is_tagged_template_expression(&chain)
        {
            chain = cast(
                Some(skip_partially_emitted_expressions(
                    chain.as_has_expression().expression(),
                    self,
                )),
                |value: &Id<Node>| is_optional_chain(value),
            );
            Debug_.assert_not_node(Some(&*chain), Some(is_non_null_chain), None);
            links.insert(0, chain.clone());
        }
        FlattenChainReturn {
            expression: chain.as_has_expression().expression(),
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
            &node.as_parenthesized_expression().expression,
            capture_this_arg,
            is_delete,
        );
        if is_synthetic_reference(&expression) {
            let expression_as_synthetic_reference_expression =
                expression.as_synthetic_reference_expression();
            return self.factory.create_synthetic_reference_expression(
                self.factory.update_parenthesized_expression(
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
            .update_parenthesized_expression(node, expression)
    }

    fn visit_non_optional_property_or_element_access_expression(
        &self,
        node: Id<Node>, /*AccessExpression*/
        capture_this_arg: bool,
        is_delete: bool,
    ) -> Id<Node /*Expression*/> {
        if is_optional_chain(node) {
            return self.visit_optional_expression(node, capture_this_arg, is_delete);
        }

        let mut expression = visit_node(
            &node.as_has_expression().expression(),
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        Debug_.assert_not_node(Some(&*expression), Some(is_synthetic_reference), None);

        let mut this_arg: Option<Id<Node /*Expression*/>> = Default::default();
        if capture_this_arg {
            if !is_simple_copiable_expression(&expression) {
                this_arg = Some(self.factory.create_temp_variable(
                    Some(|node: Id<Node>| {
                        self.context.hoist_variable_declaration(node);
                    }),
                    None,
                ));
                expression = self
                    .factory
                    .create_assignment(this_arg.clone().unwrap(), expression);
            } else {
                this_arg = Some(expression.clone());
            }
        }

        expression = if node.kind() == SyntaxKind::PropertyAccessExpression {
            self.factory.update_property_access_expression(
                node,
                expression.clone(),
                visit_node(
                    &node.as_property_access_expression().name,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_identifier),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
            )
        } else {
            self.factory.update_element_access_expression(
                node,
                expression.clone(),
                visit_node(
                    &node.as_element_access_expression().argument_expression,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
            )
        };
        if let Some(this_arg) = this_arg {
            self.factory
                .create_synthetic_reference_expression(expression, this_arg)
        } else {
            expression
        }
    }

    fn visit_non_optional_call_expression(
        &self,
        node: Id<Node>, /*CallExpression*/
        capture_this_arg: bool,
    ) -> Id<Node /*Expression*/> {
        let node_as_call_expression = node.as_call_expression();
        if is_optional_chain(node) {
            return self.visit_optional_expression(node, capture_this_arg, false);
        }
        if is_parenthesized_expression(&node_as_call_expression.expression)
            && is_optional_chain(&skip_parentheses(node_as_call_expression.expression, None, self))
        {
            let expression = self.visit_non_optional_parenthesized_expression(
                &node_as_call_expression.expression,
                true,
                false,
            );
            let args = visit_nodes(
                &node_as_call_expression.arguments,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                None,
                None,
            );
            if is_synthetic_reference(&expression) {
                let expression_as_synthetic_reference_expression =
                    expression.as_synthetic_reference_expression();
                return self
                    .factory
                    .create_function_call_call(
                        expression_as_synthetic_reference_expression
                            .expression
                            .clone(),
                        expression_as_synthetic_reference_expression
                            .this_arg
                            .clone(),
                        args,
                    )
                    .set_text_range(Some(node));
            }
            return self.factory.update_call_expression(
                node,
                expression,
                Option::<Gc<NodeArray>>::None,
                args,
            );
        }
        visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context)
    }

    fn visit_non_optional_expression(
        &self,
        node: Id<Node>, /*OptionalChain*/
        capture_this_arg: bool,
        is_delete: bool,
    ) -> Id<Node /*Expression*/> {
        match node.kind() {
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
        let left = self.visit_non_optional_expression(&expression, is_call_chain(&chain[0]), false);
        let left_this_arg = is_synthetic_reference(&left)
            .then_some(&left.as_synthetic_reference_expression().this_arg);
        let mut left_expression = if is_synthetic_reference(&left) {
            left.as_synthetic_reference_expression().expression.clone()
        } else {
            left.clone()
        };
        let mut captured_left/*Expression*/ = left_expression.clone();
        if !is_simple_copiable_expression(&left_expression) {
            captured_left = self.factory.create_temp_variable(
                Some(|node: Id<Node>| {
                    self.context.hoist_variable_declaration(node);
                }),
                None,
            );
            left_expression = self
                .factory
                .create_assignment(captured_left.clone(), left_expression.clone());
        }
        let mut right_expression = captured_left.clone();
        let mut this_arg: Option<Id<Node /*Expression*/>> = Default::default();
        for (i, segment) in chain.iter().enumerate() {
            match segment.kind() {
                SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
                    if i == chain.len() - 1 && capture_this_arg {
                        if !is_simple_copiable_expression(&right_expression) {
                            this_arg = Some(self.factory.create_temp_variable(
                                Some(|node: Id<Node>| {
                                    self.context.hoist_variable_declaration(node);
                                }),
                                None,
                            ));
                            right_expression = self
                                .factory
                                .create_assignment(this_arg.clone().unwrap(), right_expression);
                        } else {
                            this_arg = Some(right_expression.clone());
                        }
                    }
                    right_expression = if segment.kind() == SyntaxKind::PropertyAccessExpression {
                        self.factory.create_property_access_expression(
                            right_expression,
                            visit_node(
                                &segment.as_property_access_expression().name,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(is_identifier),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ),
                        )
                    } else {
                        self.factory.create_element_access_expression(
                            right_expression,
                            visit_node(
                                &segment.as_element_access_expression().argument_expression,
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
                        right_expression = self.factory.create_function_call_call(
                            right_expression,
                            if left_this_arg.kind() == SyntaxKind::SuperKeyword {
                                self.factory.create_this()
                            } else {
                                left_this_arg.clone()
                            },
                            visit_nodes(
                                &segment.as_call_expression().arguments,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                None,
                                None,
                            ),
                        );
                    } else {
                        right_expression = self.factory.create_call_expression(
                            right_expression,
                            Option::<Gc<NodeArray>>::None,
                            Some(visit_nodes(
                                &segment.as_call_expression().arguments,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                None,
                                None,
                            )),
                        );
                    }
                }
                _ => (),
            }
            set_original_node(right_expression, Some(segment), self);
        }

        let target = if is_delete {
            self.factory.create_conditional_expression(
                self.create_not_null_condition(left_expression, captured_left, Some(true)),
                None,
                self.factory.create_true(),
                None,
                self.factory.create_delete_expression(right_expression),
            )
        } else {
            self.factory.create_conditional_expression(
                self.create_not_null_condition(left_expression, captured_left, Some(true)),
                None,
                self.factory.create_void_zero(),
                None,
                right_expression,
            )
        }
        .set_text_range(Some(node));
        if let Some(this_arg) = this_arg {
            self.factory
                .create_synthetic_reference_expression(target, this_arg)
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
        self.factory.create_binary_expression(
            self.factory.create_binary_expression(
                left,
                self.factory.create_token(if invert == Some(true) {
                    SyntaxKind::EqualsEqualsEqualsToken
                } else {
                    SyntaxKind::ExclamationEqualsEqualsToken
                }),
                self.factory.create_null(),
            ),
            self.factory.create_token(if invert == Some(true) {
                SyntaxKind::BarBarToken
            } else {
                SyntaxKind::AmpersandAmpersandToken
            }),
            self.factory.create_binary_expression(
                right,
                self.factory.create_token(if invert == Some(true) {
                    SyntaxKind::EqualsEqualsEqualsToken
                } else {
                    SyntaxKind::ExclamationEqualsEqualsToken
                }),
                self.factory.create_void_zero(),
            ),
        )
    }

    fn transform_nullish_coalescing_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> VisitResult {
        let node_as_binary_expression = node.as_binary_expression();
        let mut left = visit_node(
            &node_as_binary_expression.left,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        let mut right = left.clone();
        if !is_simple_copiable_expression(&left) {
            right = self.factory.create_temp_variable(
                Some(|node: Id<Node>| {
                    self.context.hoist_variable_declaration(node);
                }),
                None,
            );
            left = self.factory.create_assignment(right.clone(), left);
        }
        Some(
            self.factory
                .create_conditional_expression(
                    self.create_not_null_condition(left, right.clone(), None),
                    None,
                    right,
                    None,
                    visit_node(
                        &node_as_binary_expression.right,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                )
                .set_text_range(Some(node))
                .into(),
        )
    }

    fn visit_delete_expression(&self, node: Id<Node> /*DeleteExpression*/) -> VisitResult {
        let node_as_delete_expression = node.as_delete_expression();
        Some(
            if is_optional_chain(&skip_parentheses(
                node_as_delete_expression.expression,
                None,
                self,
            )) {
                self.visit_non_optional_expression(
                    &node_as_delete_expression.expression,
                    false,
                    true,
                )
                .set_original_node(Some(node.node_wrapper()))
            } else {
                self.factory.update_delete_expression(
                    node,
                    visit_node(
                        &node_as_delete_expression.expression,
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
}

#[derive(Trace, Finalize)]
struct TransformES2020Factory {}

impl TransformES2020Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2020Factory {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            TransformES2020::new(context).as_transformer(),
        )
    }
}

pub fn transform_es2020() -> TransformerFactory {
    Gc::new(Box::new(TransformES2020Factory::new()))
}
