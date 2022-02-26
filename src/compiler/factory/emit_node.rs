use std::borrow::Borrow;
use std::cell::RefMut;
use std::rc::Rc;

use crate::{is_parse_tree_node, Debug_, EmitFlags, EmitNode, Node, NodeInterface};

pub(crate) fn get_or_create_emit_node(node: &Node) -> RefMut<EmitNode> {
    let mut node_emit_node = node.maybe_emit_node();
    match &*node_emit_node {
        None => {
            if is_parse_tree_node(node) {
                unimplemented!()
            }

            *node_emit_node = Some(Default::default());
        }
        Some(node_emit_node) => {
            Debug_.assert(
                !node_emit_node
                    .flags
                    .map_or(false, |flags| flags.intersects(EmitFlags::Immutable)),
                Some("Invalid attempt to mutate an immutable node."),
            );
        }
    }
    RefMut::map(node_emit_node, |node_emit_node| {
        node_emit_node.as_mut().unwrap()
    })
}

pub fn dispose_emit_nodes<TSourceFile: Borrow<Node>>(
    source_file: Option<TSourceFile /*SourceFile*/>,
) {
    unimplemented!()
}

pub fn set_emit_flags(node: Rc<Node>, emit_flags: EmitFlags) -> Rc<Node> {
    get_or_create_emit_node(&node).flags = Some(emit_flags);
    node
}

pub(crate) fn get_starts_on_new_line(node: &Node) -> bool {
    node.maybe_emit_node()
        .as_ref()
        .and_then(|emit_node| emit_node.starts_on_new_line)
        .unwrap_or(false)
}

pub(crate) fn set_starts_on_new_line(node: &Node, new_line: bool) /*-> Rc<Node>*/
{
    get_or_create_emit_node(node).starts_on_new_line = Some(new_line);
    // node
}
