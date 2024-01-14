use std::io;

use gc::{Finalize, Gc, Trace};
use id_arena::Id;

use crate::{
    chain_bundle, compiler::factory::utilities_public::set_text_range_rc_node,
    is_element_access_expression, is_expression, is_property_access_expression,
    maybe_visit_each_child, visit_each_child, visit_node, BaseNodeFactorySynthetic, Node,
    NodeFactory, NodeInterface, SyntaxKind, TransformFlags, TransformationContext, Transformer,
    TransformerFactory, TransformerFactoryInterface, TransformerInterface, VisitResult,
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

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node> {
        if node.as_source_file().is_declaration_file() {
            return node.node_wrapper();
        }

        visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context)
    }

    fn visitor(&self, node: Id<Node>) -> VisitResult {
        if !node
            .transform_flags()
            .intersects(TransformFlags::ContainsES2016)
        {
            return Some(node.node_wrapper().into());
        }
        match node.kind() {
            SyntaxKind::BinaryExpression => Some(self.visit_binary_expression(node).into()),
            _ => maybe_visit_each_child(
                Some(node),
                |node: Id<Node>| self.visitor(node),
                &**self.context,
            )
            .map(Into::into),
        }
    }

    fn visit_binary_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> Id<Node /*Expression*/> {
        match node.as_binary_expression().operator_token.kind() {
            SyntaxKind::AsteriskAsteriskEqualsToken => {
                self.visit_exponentiation_assignment_expression(node)
            }
            SyntaxKind::AsteriskAsteriskToken => self.visit_exponentiation_expression(node),
            _ => visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context),
        }
    }

    fn visit_exponentiation_assignment_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> Id<Node> {
        let target: Id<Node /*Expression*/>;
        let value: Id<Node /*Expression*/>;
        let node_as_binary_expression = node.as_binary_expression();
        let ref left = visit_node(
            &node_as_binary_expression.left,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        let ref right = visit_node(
            &node_as_binary_expression.right,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        if is_element_access_expression(left) {
            let left_as_element_access_expression = left.as_element_access_expression();
            let expression_temp = self.factory.create_temp_variable(
                Some(|node: Id<Node>| self.context.hoist_variable_declaration(node)),
                None,
            );
            let argument_expression_temp = self.factory.create_temp_variable(
                Some(|node: Id<Node>| self.context.hoist_variable_declaration(node)),
                None,
            );
            target = set_text_range_rc_node(
                self.factory.create_element_access_expression(
                    set_text_range_rc_node(
                        self.factory.create_assignment(
                            expression_temp.clone(),
                            left_as_element_access_expression.expression.clone(),
                        ),
                        Some(&*left_as_element_access_expression.expression),
                    ),
                    set_text_range_rc_node(
                        self.factory.create_assignment(
                            argument_expression_temp.clone(),
                            left_as_element_access_expression
                                .argument_expression
                                .clone(),
                        ),
                        Some(&*left_as_element_access_expression.argument_expression),
                    ),
                ),
                Some(&**left),
            );
            value = set_text_range_rc_node(
                self.factory
                    .create_element_access_expression(expression_temp, argument_expression_temp),
                Some(&**left),
            );
        } else if is_property_access_expression(left) {
            let left_as_property_access_expression = left.as_property_access_expression();
            let expression_temp = self.factory.create_temp_variable(
                Some(|node: Id<Node>| self.context.hoist_variable_declaration(node)),
                None,
            );
            target = set_text_range_rc_node(
                self.factory.create_property_access_expression(
                    set_text_range_rc_node(
                        self.factory.create_assignment(
                            expression_temp.clone(),
                            left_as_property_access_expression.expression.clone(),
                        ),
                        Some(&*left_as_property_access_expression.expression),
                    ),
                    left_as_property_access_expression.name.clone(),
                ),
                Some(&**left),
            );
            value = set_text_range_rc_node(
                self.factory.create_property_access_expression(
                    expression_temp,
                    left_as_property_access_expression.name.clone(),
                ),
                Some(&**left),
            );
        } else {
            target = left.clone();
            value = left.clone();
        }
        set_text_range_rc_node(
            self.factory.create_assignment(
                target,
                set_text_range_rc_node(
                    self.factory.create_global_method_call(
                        "Math",
                        "pow",
                        vec![value, right.clone()],
                    ),
                    Some(node),
                ),
            ),
            Some(node),
        )
    }

    fn visit_exponentiation_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> Id<Node> {
        let node_as_binary_expression = node.as_binary_expression();
        let left = visit_node(
            &node_as_binary_expression.left,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        let right = visit_node(
            &node_as_binary_expression.right,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        set_text_range_rc_node(
            self.factory
                .create_global_method_call("Math", "pow", vec![left, right]),
            Some(node),
        )
    }
}

impl TransformerInterface for TransformES2016 {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(self.transform_source_file(node))
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
