use gc::Gc;

use super::TransformDeclarations;
use crate::{AllAccessorDeclarations, ModifierFlags, Node, NodeArrayOrVec};

impl TransformDeclarations {
    pub(super) fn check_name(&self, node: &Node /*DeclarationDiagnosticProducing*/) {
        unimplemented!()
    }

    pub(super) fn should_strip_internal(&self, node: &Node) -> bool {
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

pub(super) fn is_processed_component(node: &Node) -> bool {
    unimplemented!()
}
