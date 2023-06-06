use gc::Gc;

use super::TransformGenerators;
use crate::{visit_each_child, Node, VisitResult};

impl TransformGenerators {
    pub(super) fn visit_accessor_declaration(
        &self,
        node: &Node, /*AccessorDeclaration*/
    ) -> VisitResult {
        let saved_in_generator_function_body = self.maybe_in_generator_function_body();
        let saved_in_statement_containing_yield = self.maybe_in_statement_containing_yield();
        self.set_in_generator_function_body(Some(false));
        self.set_in_statement_containing_yield(Some(false));
        let node = visit_each_child(&node, |node: &Node| self.visitor(node), &**self.context);
        self.set_in_generator_function_body(saved_in_generator_function_body);
        self.set_in_statement_containing_yield(saved_in_statement_containing_yield);
        Some(node.into())
    }

    pub(super) fn transform_generator_function_body(
        &self,
        _body: &Node, /*Block*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn visit_variable_statement(
        &self,
        _node: &Node, /*VariableStatement*/
    ) -> Option<Gc<Node /*Statement*/>> {
        unimplemented!()
    }

    pub(super) fn visit_binary_expression(
        &self,
        _node: &Node, /*BinaryExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_comma_list_expression(
        &self,
        _node: &Node, /*CommaListExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_conditional_expression(
        &self,
        _node: &Node, /*ConditionalExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_yield_expression(
        &self,
        _node: &Node, /*YieldExpression*/
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        unimplemented!()
    }

    pub(super) fn visit_array_literal_expression(
        &self,
        _node: &Node, /*ArrayLiteralExpression*/
    ) -> VisitResult {
        unimplemented!()
    }
}
