use gc::Gc;

use crate::{Node, NodeInterface};

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn is_export_of_namespace(&self, _node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_named_external_module_export(&self, _node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_default_external_module_export(&self, _node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn expression_to_statement(&self, expression: Gc<Node /*Expression*/>) -> Gc<Node> {
        self.factory.create_expression_statement(expression).wrap()
    }

    pub(super) fn add_export_member_assignment(
        &self,
        _statements: &mut Vec<Gc<Node /*Statement*/>>,
        _node: &Node, /*ClassDeclaration | FunctionDeclaration*/
    ) {
        unimplemented!()
    }

    pub(super) fn get_class_alias_if_needed(
        &self,
        _node: &Node, /*ClassDeclaration*/
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn get_class_member_prefix(
        &self,
        _node: &Node,   /*ClassExpression | ClassDeclaration*/
        _member: &Node, /*ClassElement*/
    ) -> Gc<Node> {
        unimplemented!()
    }
}
