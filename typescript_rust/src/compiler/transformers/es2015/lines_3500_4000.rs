use gc::Gc;

use super::TransformES2015;
use crate::{Node, VisitResult};

impl TransformES2015 {
    pub(super) fn transform_object_literal_method_declaration_to_expression(
        &self,
        _method: &Node,   /*MethodDeclaration*/
        _receiver: &Node, /*Expression*/
        _container: &Node,
        _starts_on_new_line: Option<bool>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn visit_catch_clause(
        &self,
        _node: &Node, /*CatchClause*/
    ) -> Gc<Node /*CatchClause*/> {
        unimplemented!()
    }

    pub(super) fn visit_method_declaration(
        &self,
        _node: &Node, /*MethodDeclaration*/
    ) -> Gc<Node /*ObjectLiteralElementLike*/> {
        unimplemented!()
    }

    pub(super) fn visit_accessor_declaration(
        &self,
        _node: &Node, /*AccessorDeclaration*/
    ) -> Gc<Node /*AccessorDeclaration*/> {
        unimplemented!()
    }

    pub(super) fn visit_shorthand_property_assignment(
        &self,
        _node: &Node, /*ShorthandPropertyAssignment*/
    ) -> Gc<Node /*ObjectLiteralElementLike*/> {
        unimplemented!()
    }

    pub(super) fn visit_computed_property_name(
        &self,
        _node: &Node, /*ComputedPropertyName*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_yield_expression(
        &self,
        _node: &Node, /*YieldExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_array_literal_expression(
        &self,
        _node: &Node, /*ArrayLiteralExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_call_expression(
        &self,
        _node: &Node, /*CallExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_immediate_super_call_in_body(
        &self,
        _node: &Node, /*CallExpression*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn visit_new_expression(
        &self,
        _node: &Node, /*CallExpression*/
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        unimplemented!()
    }
}
