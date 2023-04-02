use gc::Gc;

use crate::{ModifierFlags, Node};

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
