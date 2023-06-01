use gc::Gc;

use super::TransformClassFields;
use crate::{Node, VisitResult};

impl TransformClassFields {
    pub(super) fn visit_class_declaration(
        &self,
        _node: &Node, /*ClassDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_class_expression(
        &self,
        _node: &Node, /*ClassExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_class_static_block_declaration(
        &self,
        _node: &Node, /*ClassStaticBlockDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }
}
