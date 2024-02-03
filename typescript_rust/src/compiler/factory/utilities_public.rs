use id_arena::Id;

use crate::{set_text_range_pos_end, HasArena, InArena, Node, NodeArray, ReadonlyTextRange};

pub fn set_text_range<'a, TRange: ReadonlyTextRange + ?Sized>(
    range: &'a TRange,
    location: Option<&(impl ReadonlyTextRange + ?Sized)>,
) -> &'a TRange {
    match location {
        Some(location) => {
            set_text_range_pos_end(range, location.pos(), location.end());
            range
        }
        None => range,
    }
}

pub fn set_text_range_id_node(
    node: Id<Node>,
    location: Option<&(impl ReadonlyTextRange + ?Sized)>,
    arena: &impl HasArena,
) -> Id<Node> {
    set_text_range(&*node.ref_(arena), location);
    node
}

pub fn set_text_range_node_array(
    node_array: Id<NodeArray>,
    location: Option<&impl ReadonlyTextRange>,
    arena: &impl HasArena,
) -> Id<NodeArray> {
    set_text_range(&*node_array.ref_(arena), location);
    node_array
}
