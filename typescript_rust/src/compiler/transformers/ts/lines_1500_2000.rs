use crate::{Node, VisitResult};

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn visit_property_declaration(
        &self,
        node: &Node, /*PropertyDeclaration*/
    ) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }

    pub(super) fn visit_constructor(
        &self,
        node: &Node, /*ConstructorDeclaration*/
    ) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }
}
