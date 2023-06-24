use gc::Gc;

use super::TransformSystemModule;
use crate::{Node, VisitResult};

impl TransformSystemModule {
    pub(super) fn append_exports_of_hoisted_declaration(
        &self,
        _statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        _decl: &Node, /*ClassDeclaration | FunctionDeclaration*/
    ) /*: Statement[] | undefined*/
    {
        unimplemented!()
    }

    pub(super) fn append_export_statement(
        &self,
        _statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        _export_name: &Node, /*Identifier | StringLiteral*/
        _expression: &Node,  /*Expression*/
        _allow_comments: Option<bool>,
    ) /*: Statement[] | undefined*/
    {
        unimplemented!()
    }

    pub(super) fn create_export_statement(
        &self,
        _name: &Node,  /*Identifier | StringLiteral*/
        _value: &Node, /*Expression*/
        _allow_comments: Option<bool>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn create_export_expression(
        &self,
        _name: &Node,  /*Identifier | StringLiteral*/
        _value: &Node, /*Expression*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn top_level_nested_visitor(&self, _node: &Node) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }
}
