use std::borrow::Borrow;

use gc::Gc;

use super::TransformES2015;
use crate::{Node, VisitResult};

impl TransformES2015 {
    pub(super) fn insert_capture_this_for_node_if_needed(
        &self,
        _statements: &mut Vec<Gc<Node>>, /*Statement*/
        _node: &Node,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn insert_capture_this_for_node(
        &self,
        _statements: &mut Vec<Gc<Node>>, /*Statement*/
        _node: &Node,
        _initializer: Option<impl Borrow<Node /*Expression*/>>,
    ) {
        unimplemented!()
    }

    pub(super) fn insert_capture_new_target_if_needed(
        &self,
        _statements: Vec<Gc<Node>>, /*Statement*/
        _node: &Node,               /*FunctionLikeDeclaration*/
        _copy_on_write: bool,
    ) -> Vec<Gc<Node>> /*Statement*/ {
        unimplemented!()
    }

    pub(super) fn add_class_members(
        &self,
        _statements: &mut Vec<Gc<Node /*Statement*/>>,
        _node: &Node, /*ClassExpression | ClassDeclaration*/
    ) {
        unimplemented!()
    }

    pub(super) fn visit_arrow_function(&self, _node: &Node /*ArrowFunction*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_function_expression(
        &self,
        _node: &Node, /*FunctionExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_function_declaration(
        &self,
        _node: &Node, /*FunctionDeclaration*/
    ) -> Gc<Node /*FunctionDeclaration*/> {
        unimplemented!()
    }
}
