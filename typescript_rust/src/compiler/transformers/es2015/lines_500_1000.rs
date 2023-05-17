use gc::Gc;

use super::TransformES2015;
use crate::{Node, VisitResult};

impl TransformES2015 {
    pub(super) fn visit_source_file(
        &self,
        _node: &Node, /*SourceFile*/
    ) -> Gc<Node /*SourceFile*/> {
        unimplemented!()
    }

    pub(super) fn visit_switch_statement(
        &self,
        _node: &Node, /*SwitchStatement*/
    ) -> Gc<Node /*SwitchStatement*/> {
        unimplemented!()
    }

    pub(super) fn visit_case_block(
        &self,
        _node: &Node, /*CaseBlock*/
    ) -> Gc<Node /*CaseBlock*/> {
        unimplemented!()
    }

    pub(super) fn visit_return_statement(
        &self,
        _node: &Node, /*ReturnStatement*/
    ) -> Gc<Node /*Statement*/> {
        unimplemented!()
    }

    pub(super) fn visit_this_keyword(&self, _node: &Node) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn visit_void_expression(
        &self,
        _node: &Node, /*VoidExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_identifier(
        &self,
        _node: &Node, /*Identifier*/
    ) -> Gc<Node /*Identifier*/> {
        unimplemented!()
    }

    pub(super) fn visit_break_or_continue_statement(
        &self,
        _node: &Node, /*BreakOrContinueStatement*/
    ) -> Gc<Node /*Statement*/> {
        unimplemented!()
    }

    pub(super) fn visit_class_declaration(
        &self,
        _node: &Node, /*ClassDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_class_expression(
        &self,
        _node: &Node, /*ClassExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }
}
