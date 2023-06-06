use gc::Gc;

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

    pub(super) fn transform_and_emit_statements(
        &self,
        _statements: &[Gc<Node /*Statement*/>],
        start: Option<usize>,
    ) {
        let _start = start.unwrap_or(0);
        unimplemented!()
    }

    pub(super) fn transform_and_emit_variable_declaration_list(
        &self,
        _node: &Node, /*VariableDeclarationList*/
    ) -> Option<Gc<Node /*VariableDeclarationList*/>> {
        unimplemented!()
    }

    pub(super) fn transform_initialized_variable(
        &self,
        _node: &Node, /*InitializedVariableDeclaration*/
    ) -> Gc<Node> {
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
