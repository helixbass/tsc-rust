use gc::Gc;

use crate::{Node, VisitResult};

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn visit_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn visit_class_declaration(
        &self,
        node: &Node, /*ClassDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_class_expression(
        &self,
        node: &Node, /*ClassExpression*/
    ) -> Gc<Node /*<Expression>*/> {
        unimplemented!()
    }
}
