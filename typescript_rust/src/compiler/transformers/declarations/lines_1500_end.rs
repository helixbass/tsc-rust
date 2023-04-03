use gc::Gc;

use super::TransformDeclarations;
use crate::{AllAccessorDeclarations, ModifierFlags, Node, NodeArray, NodeArrayOrVec};

impl TransformDeclarations {
    pub(super) fn transform_variable_statement(
        &self,
        input: &Node, /*VariableStatement*/
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn recreate_binding_pattern(
        &self,
        d: &Node, /*BindingPattern*/
    ) -> Vec<Gc<Node /*VariableDeclaration*/>> {
        unimplemented!()
    }

    pub(super) fn check_name(&self, node: &Node /*DeclarationDiagnosticProducing*/) {
        unimplemented!()
    }

    pub(super) fn should_strip_internal(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn has_scope_marker(&self, statements: &NodeArray) -> bool {
        unimplemented!()
    }

    pub(super) fn ensure_modifiers(&self, node: &Node) -> Option<NodeArrayOrVec> {
        unimplemented!()
    }

    pub(super) fn get_type_annotation_from_all_accessor_declarations(
        &self,
        node: &Node, /*AccessorDeclaration*/
        accessors: &AllAccessorDeclarations,
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn transform_heritage_clauses(
        &self,
        nodes: Option<&NodeArray /*<HeritageClause>*/>,
    ) -> Gc<NodeArray> {
        unimplemented!()
    }
}

pub(super) fn mask_modifiers(
    node: &Node,
    modifier_mask: Option<ModifierFlags>,
    modifier_additions: Option<ModifierFlags>,
) -> Vec<Gc<Node /*Modifier*/>> {
    unimplemented!()
}

pub(super) fn can_have_literal_initializer(node: &Node) -> bool {
    unimplemented!()
}

pub(super) fn is_preserved_declaration_statement(node: &Node) -> bool {
    unimplemented!()
}

pub(super) fn is_processed_component(node: &Node) -> bool {
    unimplemented!()
}
