use gc::Gc;
use id_arena::Id;

use crate::{set_text_range_pos_end, Node, NodeArray, ReadonlyTextRange};

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

pub fn set_text_range_rc_node(
    node: Id<Node>,
    location: Option<&(impl ReadonlyTextRange + ?Sized)>,
) -> Id<Node> {
    set_text_range(&*node, location);
    node
}

pub fn set_text_range_node_array(
    node_array: Gc<NodeArray>,
    location: Option<&impl ReadonlyTextRange>,
) -> Gc<NodeArray> {
    set_text_range(&*node_array, location);
    node_array
}
