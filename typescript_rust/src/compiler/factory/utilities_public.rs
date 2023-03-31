use gc::Gc;

use crate::{set_text_range_pos_end, Node, NodeArray, ReadonlyTextRange};

pub fn set_text_range<'a, TRange: ReadonlyTextRange>(
    range: &'a TRange,
    location: Option<&impl ReadonlyTextRange>,
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
    node: Gc<Node>,
    location: Option<&impl ReadonlyTextRange>,
) -> Gc<Node> {
    set_text_range(&*node, location);
    node
}

pub fn set_text_range_node_array(
    node_array: NodeArray,
    location: Option<&impl ReadonlyTextRange>,
) -> NodeArray {
    set_text_range(&node_array, location);
    node_array
}
