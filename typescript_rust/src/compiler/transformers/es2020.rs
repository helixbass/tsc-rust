use std::{io, mem};

use gc::{Finalize, Gc, GcCell, Trace};

use crate::{
    cast, chain_bundle, is_expression, is_identifier, is_non_null_chain, is_optional_chain,
    is_parenthesized_expression, is_simple_copiable_expression, is_synthetic_reference,
    is_tagged_template_expression, skip_parentheses, skip_partially_emitted_expressions,
    visit_each_child, visit_node, visit_nodes, BaseNodeFactorySynthetic, Debug_, Node, NodeArray,
    NodeExt, NodeFactory, NodeInterface, SyntaxKind, TransformFlags, TransformationContext,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface,
    VisitResult,
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

    fn visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
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
            SyntaxKind::BinaryExpression => {
                if node.as_binary_expression().operator_token.kind()
                    == SyntaxKind::QuestionQuestionToken
                {
                    return self.transform_nullish_coalescing_expression(node);
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
            SyntaxKind::DeleteExpression => self.visit_delete_expression(node),
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

    fn flatten_chain(&self, chain: &Node /*OptionalChain*/) -> FlattenChainReturn {
        Debug_.assert_not_node(Some(chain), Some(is_non_null_chain), None);
        let mut chain = chain.node_wrapper();
        let mut links: Vec<Gc<Node /*OptionalChain*/>> = vec![chain.clone()];
        while chain
            .as_has_question_dot_token()
            .maybe_question_dot_token()
            .is_none()
            && !is_tagged_template_expression(&chain)
        {
            chain = cast(
                Some(skip_partially_emitted_expressions(
                    &chain.as_has_expression().expression(),
                )),
                |value: &Gc<Node>| is_optional_chain(value),
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
        node: &Node, /*ParenthesizedExpression*/
        capture_this_arg: bool,
        is_delete: bool,
    ) -> Gc<Node /*Expression*/> {
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
        node: &Node, /*AccessExpression*/
        capture_this_arg: bool,
        is_delete: bool,
    ) -> Gc<Node /*Expression*/> {
        if is_optional_chain(node) {
            return self.visit_optional_expression(node, capture_this_arg, is_delete);
        }

        let mut expression = visit_node(
            Some(&*node.as_has_expression().expression()),
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )
        .unwrap();
        Debug_.assert_not_node(Some(&*expression), Some(is_synthetic_reference), None);

        let mut this_arg: Option<Gc<Node /*Expression*/>> = Default::default();
        if capture_this_arg {
            if !is_simple_copiable_expression(&expression) {
                this_arg = Some(self.factory.create_temp_variable(
                    Some(|node: &Node| {
                        self.context.hoist_variable_declaration(node);
                    }),
                    None,
                ));
                expression = self
                    .factory
                    .create_assignment(this_arg.clone().unwrap(), expression)
                    .wrap();
            } else {
                this_arg = Some(expression.clone());
            }
        }

        expression = if node.kind() == SyntaxKind::PropertyAccessExpression {
            self.factory.update_property_access_expression(
                node,
                expression.clone(),
                visit_node(
                    Some(&*node.as_property_access_expression().name),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_identifier),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )
                .unwrap(),
            )
        } else {
            self.factory.update_element_access_expression(
                node,
                expression.clone(),
                visit_node(
                    Some(&*node.as_element_access_expression().argument_expression),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )
                .unwrap(),
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
        node: &Node, /*CallExpression*/
        capture_this_arg: bool,
    ) -> Gc<Node /*Expression*/> {
        let node_as_call_expression = node.as_call_expression();
        if is_optional_chain(node) {
            return self.visit_optional_expression(node, capture_this_arg, false);
        }
        if is_parenthesized_expression(&node_as_call_expression.expression)
            && is_optional_chain(&skip_parentheses(&node_as_call_expression.expression, None))
        {
            let expression = self.visit_non_optional_parenthesized_expression(
                &node_as_call_expression.expression,
                true,
                false,
            );
            let args = visit_nodes(
                Some(&node_as_call_expression.arguments),
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                None,
                None,
            )
            .unwrap();
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

    fn visit_non_optional_expression(
        &self,
        node: &Node, /*OptionalChain*/
        capture_this_arg: bool,
        is_delete: bool,
    ) -> Gc<Node /*Expression*/> {
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
                Some(node),
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )
            .unwrap(),
        }
    }

    fn visit_optional_expression(
        &self,
        _node: &Node, /*OptionalChain*/
        _capture_this_arg: bool,
        _is_delete: bool,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    fn transform_nullish_coalescing_expression(
        &self,
        _node: &Node, /*BinaryExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    fn visit_delete_expression(&self, _node: &Node /*DeleteExpression*/) -> VisitResult {
        unimplemented!()
    }
}

struct FlattenChainReturn {
    pub expression: Gc<Node>,
    pub chain: Vec<Gc<Node>>,
}

impl TransformerInterface for TransformES2020 {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
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
