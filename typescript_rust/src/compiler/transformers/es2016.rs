use std::io;

use gc::{Finalize, Gc, Trace};
use id_arena::Id;

use crate::{
    chain_bundle, compiler::factory::utilities_public::set_text_range_id_node,
    is_element_access_expression, is_expression, is_property_access_expression,
    maybe_visit_each_child, visit_each_child, visit_node, BaseNodeFactorySynthetic, Node,
    NodeFactory, NodeInterface, SyntaxKind, TransformFlags, TransformationContext, Transformer,
    TransformerFactory, TransformerFactoryInterface, TransformerInterface, VisitResult,
    HasArena, AllArenas, InArena,
};

#[derive(Trace, Finalize)]
struct TransformES2016 {
    context: Id<TransformNodesTransformationResult>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    base_factory: Gc<BaseNodeFactorySynthetic>,
}

impl TransformES2016 {
    fn new(context: Id<TransformNodesTransformationResult>) -> Self {
        Self {
            factory: context.factory(),
            base_factory: context.base_factory(),
            context,
        }
    }

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node> {
        if node.ref_(self).as_source_file().is_declaration_file() {
            return node;
        }

        visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context.ref_(self), self)
    }

    fn visitor(&self, node: Id<Node>) -> VisitResult {
        if !node
            .ref_(self).transform_flags()
            .intersects(TransformFlags::ContainsES2016)
        {
            return Some(node.into());
        }
        match node.ref_(self).kind() {
            SyntaxKind::BinaryExpression => Some(self.visit_binary_expression(node).into()),
            _ => maybe_visit_each_child(
                Some(node),
                |node: Id<Node>| self.visitor(node),
                &**self.context.ref_(self),
                self,
            )
            .map(Into::into),
        }
    }

    fn visit_binary_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> Id<Node /*Expression*/> {
        match node.ref_(self).as_binary_expression().operator_token.ref_(self).kind() {
            SyntaxKind::AsteriskAsteriskEqualsToken => {
                self.visit_exponentiation_assignment_expression(node)
            }
            SyntaxKind::AsteriskAsteriskToken => self.visit_exponentiation_expression(node),
            _ => visit_each_child(node, |node: Id<Node>| self.visitor(node), &**self.context.ref_(self), self),
        }
    }

    fn visit_exponentiation_assignment_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> Id<Node> {
        let target: Id<Node /*Expression*/>;
        let value: Id<Node /*Expression*/>;
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        let left = visit_node(
            node_as_binary_expression.left,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        let right = visit_node(
            node_as_binary_expression.right,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        if is_element_access_expression(&left.ref_(self)) {
            let left_ref = left.ref_(self);
            let left_as_element_access_expression = left_ref.as_element_access_expression();
            let expression_temp = self.factory.create_temp_variable(
                Some(|node: Id<Node>| self.context.ref_(self).hoist_variable_declaration(node)),
                None,
            );
            let argument_expression_temp = self.factory.create_temp_variable(
                Some(|node: Id<Node>| self.context.ref_(self).hoist_variable_declaration(node)),
                None,
            );
            target = set_text_range_id_node(
                self.factory.create_element_access_expression(
                    set_text_range_id_node(
                        self.factory.create_assignment(
                            expression_temp.clone(),
                            left_as_element_access_expression.expression.clone(),
                        ),
                        Some(&*left_as_element_access_expression.expression.ref_(self)),
                        self,
                    ),
                    set_text_range_id_node(
                        self.factory.create_assignment(
                            argument_expression_temp.clone(),
                            left_as_element_access_expression
                                .argument_expression
                                .clone(),
                        ),
                        Some(&*left_as_element_access_expression.argument_expression.ref_(self)),
                        self,
                    ),
                ),
                Some(&*left.ref_(self)),
                self,
            );
            value = set_text_range_id_node(
                self.factory
                    .create_element_access_expression(expression_temp, argument_expression_temp),
                Some(&*left.ref_(self)),
                self,
            );
        } else if is_property_access_expression(&left.ref_(self)) {
            let left_ref = left.ref_(self);
            let left_as_property_access_expression = left_ref.as_property_access_expression();
            let expression_temp = self.factory.create_temp_variable(
                Some(|node: Id<Node>| self.context.ref_(self).hoist_variable_declaration(node)),
                None,
            );
            target = set_text_range_id_node(
                self.factory.create_property_access_expression(
                    set_text_range_id_node(
                        self.factory.create_assignment(
                            expression_temp.clone(),
                            left_as_property_access_expression.expression.clone(),
                        ),
                        Some(&*left_as_property_access_expression.expression.ref_(self)),
                        self,
                    ),
                    left_as_property_access_expression.name,
                ),
                Some(&*left.ref_(self)),
                self,
            );
            value = set_text_range_id_node(
                self.factory.create_property_access_expression(
                    expression_temp,
                    left_as_property_access_expression.name,
                ),
                Some(&*left.ref_(self)),
                self,
            );
        } else {
            target = left.clone();
            value = left.clone();
        }
        set_text_range_id_node(
            self.factory.create_assignment(
                target,
                set_text_range_id_node(
                    self.factory.create_global_method_call(
                        "Math",
                        "pow",
                        vec![value, right.clone()],
                    ),
                    Some(&*node.ref_(self)),
                    self,
                ),
            ),
            Some(&*node.ref_(self)),
            self,
        )
    }

    fn visit_exponentiation_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        let left = visit_node(
            node_as_binary_expression.left,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        let right = visit_node(
            node_as_binary_expression.right,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        set_text_range_id_node(
            self.factory
                .create_global_method_call("Math", "pow", vec![left, right]),
            Some(&*node.ref_(self)),
            self,
        )
    }
}

impl TransformerInterface for TransformES2016 {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(self.transform_source_file(node))
    }
}

impl HasArena for TransformES2016 {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
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
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            Gc::new(Box::new(TransformES2016::new(context))),
        )
    }
}

pub fn transform_es2016() -> TransformerFactory {
    Gc::new(Box::new(TransformES2016Factory::new()))
}
