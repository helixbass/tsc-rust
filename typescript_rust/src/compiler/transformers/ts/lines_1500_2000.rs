use gc::Gc;

use crate::{Node, VisitResult};

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn visit_heritage_clause(
        &self,
        node: &Node, /*HeritageClause*/
    ) -> Option<Gc<Node /*HeritageClause*/>> {
        unimplemented!()
    }

    pub(super) fn visit_expression_with_type_arguments(
        &self,
        node: &Node, /*ExpressionWithTypeArguments*/
    ) -> Gc<Node /*ExpressionWithTypeArguments*/> {
        unimplemented!()
    }

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
