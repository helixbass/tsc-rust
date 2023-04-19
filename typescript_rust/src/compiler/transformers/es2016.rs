use gc::{Finalize, Gc, Trace};

use crate::{
    chain_bundle, compiler::factory::utilities_public::set_text_range_rc_node,
    is_element_access_expression, is_expression, is_property_access_expression, set_text_range,
    visit_each_child, visit_node, with_synthetic_factory, BaseNodeFactorySynthetic, Node,
    NodeArray, NodeFactory, NodeInterface, SyntaxKind, TransformFlags, TransformationContext,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface,
    VisitResult,
};

#[derive(Trace, Finalize)]
struct TransformES2016 {
    context: Gc<Box<dyn TransformationContext>>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    base_factory: Gc<BaseNodeFactorySynthetic>,
}

impl TransformES2016 {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self {
            factory: context.factory(),
            base_factory: context.base_factory(),
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
            .intersects(TransformFlags::ContainsES2016)
        {
            return Some(node.node_wrapper().into());
        }
        match node.kind() {
            SyntaxKind::BinaryExpression => Some(self.visit_binary_expression(node).into()),
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

    fn visit_binary_expression(
        &self,
        node: &Node, /*BinaryExpression*/
    ) -> Gc<Node /*Expression*/> {
        match node.as_binary_expression().operator_token.kind() {
            SyntaxKind::AsteriskAsteriskEqualsToken => {
                self.visit_exponentiation_assignment_expression(node)
            }
            SyntaxKind::AsteriskAsteriskToken => self.visit_exponentiation_expression(node),
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
            .unwrap(),
        }
    }

    fn visit_exponentiation_assignment_expression(
        &self,
        node: &Node, /*BinaryExpression*/
    ) -> Gc<Node> {
        let target: Gc<Node /*Expression*/>;
        let value: Gc<Node /*Expression*/>;
        let node_as_binary_expression = node.as_binary_expression();
        let ref left = visit_node(
            Some(&*node_as_binary_expression.left),
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )
        .unwrap();
        let ref right = visit_node(
            Some(&*node_as_binary_expression.right),
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )
        .unwrap();
        if is_element_access_expression(left) {
            let left_as_element_access_expression = left.as_element_access_expression();
            let expression_temp = self.factory.create_temp_variable(
                Some(|node: &Node| self.context.hoist_variable_declaration(node)),
                None,
            );
            let argument_expression_temp = self.factory.create_temp_variable(
                Some(|node: &Node| self.context.hoist_variable_declaration(node)),
                None,
            );
            target = set_text_range_rc_node(
                self.factory
                    .create_element_access_expression(
                        set_text_range_rc_node(
                            self.factory
                                .create_assignment(
                                    expression_temp.clone(),
                                    left_as_element_access_expression.expression.clone(),
                                )
                                .wrap(),
                            Some(&*left_as_element_access_expression.expression),
                        ),
                        set_text_range_rc_node(
                            self.factory
                                .create_assignment(
                                    argument_expression_temp.clone(),
                                    left_as_element_access_expression
                                        .argument_expression
                                        .clone(),
                                )
                                .wrap(),
                            Some(&*left_as_element_access_expression.argument_expression),
                        ),
                    )
                    .wrap(),
                Some(&**left),
            );
            value = set_text_range_rc_node(
                self.factory
                    .create_element_access_expression(expression_temp, argument_expression_temp)
                    .wrap(),
                Some(&**left),
            );
        } else if is_property_access_expression(left) {
            let left_as_property_access_expression = left.as_property_access_expression();
            let expression_temp = self.factory.create_temp_variable(
                Some(|node: &Node| self.context.hoist_variable_declaration(node)),
                None,
            );
            target = set_text_range_rc_node(
                self.factory
                    .create_property_access_expression(
                        set_text_range_rc_node(
                            self.factory
                                .create_assignment(
                                    expression_temp.clone(),
                                    left_as_property_access_expression.expression.clone(),
                                )
                                .wrap(),
                            Some(&*left_as_property_access_expression.expression),
                        ),
                        left_as_property_access_expression.name.clone(),
                    )
                    .wrap(),
                Some(&**left),
            );
            value = set_text_range_rc_node(
                self.factory
                    .create_property_access_expression(
                        expression_temp,
                        left_as_property_access_expression.name.clone(),
                    )
                    .wrap(),
                Some(&**left),
            );
        } else {
            target = left.clone();
            value = left.clone();
        }
        set_text_range_rc_node(
            self.factory
                .create_assignment(
                    target,
                    set_text_range_rc_node(
                        self.factory.create_global_method_call(
                            "Math".to_owned(),
                            "pow".to_owned(),
                            vec![value, right.clone()],
                        ),
                        Some(node),
                    ),
                )
                .wrap(),
            Some(node),
        )
    }

    fn visit_exponentiation_expression(&self, node: &Node /*BinaryExpression*/) -> Gc<Node> {
        let node_as_binary_expression = node.as_binary_expression();
        let left = visit_node(
            Some(&*node_as_binary_expression.left),
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )
        .unwrap();
        let right = visit_node(
            Some(&*node_as_binary_expression.right),
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )
        .unwrap();
        set_text_range_rc_node(
            self.factory.create_global_method_call(
                "Math".to_owned(),
                "pow".to_owned(),
                vec![left, right],
            ),
            Some(node),
        )
    }
}

impl TransformerInterface for TransformES2016 {
    fn call(&self, node: &Node) -> Gc<Node> {
        self.transform_source_file(node)
    }
}

#[derive(Trace, Finalize)]
struct TransformES2016Factory {}

impl TransformES2016Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2016Factory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            Gc::new(Box::new(TransformES2016::new(context))),
        )
    }
}

pub fn transform_es2016() -> TransformerFactory {
    Gc::new(Box::new(TransformES2016Factory::new()))
}
