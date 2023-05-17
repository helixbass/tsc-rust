use std::borrow::Borrow;

use gc::Gc;

use super::TransformES2015;
use crate::{Node, VisitResult};

impl TransformES2015 {
    pub(super) fn visit_block(
        &self,
        _node: &Node, /*Block*/
        _is_function_body: bool,
    ) -> Gc<Node /*Block*/> {
        unimplemented!()
    }

    pub(super) fn visit_expression_statement(
        &self,
        _node: &Node, /*ExpressionStatement*/
    ) -> Gc<Node /*Statement*/> {
        unimplemented!()
    }

    pub(super) fn visit_variable_declaration_list(
        &self,
        _node: &Node, /*VariableDeclarationList*/
    ) -> Gc<Node /*VariableDeclarationList*/> {
        unimplemented!()
    }

    pub(super) fn visit_variable_declaration(
        &self,
        _node: &Node, /*VariableDeclaration*/
    ) -> VisitResult /*<VariableDeclaration>*/ {
        unimplemented!()
    }

    pub(super) fn visit_labeled_statement(
        &self,
        _node: &Node, /*LabeledStatement*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_do_or_while_statement(
        &self,
        _node: &Node, /*DoStatement | WhileStatement*/
        _outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_for_statement(
        &self,
        _node: &Node, /*ForStatement*/
        _outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_for_in_statement(
        &self,
        _node: &Node, /*ForInStatement*/
        _outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_for_of_statement(
        &self,
        _node: &Node, /*ForOfStatement*/
        _outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }
}
