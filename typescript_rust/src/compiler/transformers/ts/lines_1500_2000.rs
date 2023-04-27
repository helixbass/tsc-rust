use gc::Gc;

use crate::{Node, VisitResult};

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn get_expression_for_property_name(
        &self,
        member: &Node, /*ClassElement | EnumMember*/
        generate_name_for_computed_property_name: bool,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_heritage_clause(
        &self,
        _node: &Node, /*HeritageClause*/
    ) -> Option<Gc<Node /*HeritageClause*/>> {
        unimplemented!()
    }

    pub(super) fn visit_expression_with_type_arguments(
        &self,
        _node: &Node, /*ExpressionWithTypeArguments*/
    ) -> Gc<Node /*ExpressionWithTypeArguments*/> {
        unimplemented!()
    }

    pub(super) fn visit_property_declaration(
        &self,
        _node: &Node, /*PropertyDeclaration*/
    ) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }

    pub(super) fn visit_constructor(
        &self,
        _node: &Node, /*ConstructorDeclaration*/
    ) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }
}
