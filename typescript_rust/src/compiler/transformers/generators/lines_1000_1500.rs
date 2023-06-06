use super::TransformGenerators;
use crate::{Node, VisitResult};

impl TransformGenerators {
    pub(super) fn visit_object_literal_expression(
        &self,
        _node: &Node, /*ObjectLiteralExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_element_access_expression(
        &self,
        _node: &Node, /*ElementAccessExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_call_expression(
        &self,
        _node: &Node, /*CallExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_new_expression(&self, _node: &Node /*NewExpression*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_do_statement(&self, _node: &Node /*DoStatement*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_while_statement(
        &self,
        _node: &Node, /*WhileStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_for_statement(&self, _node: &Node /*ForStatement*/) -> VisitResult {
        unimplemented!()
    }
}
