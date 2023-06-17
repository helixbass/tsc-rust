use gc::Gc;

use super::TransformModule;
use crate::{Node, VisitResult};

impl TransformModule {
    pub(super) fn top_level_visitor(&self, _node: &Node) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }

    pub(super) fn visitor(&self, _node: &Node) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }

    pub(super) fn get_helper_expression_for_import(
        &self,
        _node: &Node,       /*ImportDeclaration*/
        _inner_expr: &Node, /*Expression*/
    ) -> Gc<Node> {
        unimplemented!()
    }
}
