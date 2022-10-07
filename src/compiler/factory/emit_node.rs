use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use crate::{
    get_parse_tree_node, get_source_file_of_node, is_parse_tree_node, Debug_, EmitFlags,
    EmitHelper, EmitNode, Node, NodeInterface, ReadonlyTextRange, SnippetElement, StringOrNumber,
    SyntaxKind, SynthesizedComment,
};

pub(crate) fn get_or_create_emit_node(node: &Node) -> Rc<RefCell<EmitNode>> {
    match node.maybe_emit_node() {
        None => {
            if is_parse_tree_node(node) {
                if node.kind() == SyntaxKind::SourceFile {
                    let ret = Rc::new(RefCell::new(EmitNode {
                        annotated_nodes: Some(vec![node.node_wrapper()]),
                        ..Default::default()
                    }));
                    *node.maybe_emit_node_mut() = Some(ret.clone());
                    return ret;
                }

                let ref source_file = get_source_file_of_node(get_parse_tree_node(
                    get_source_file_of_node(Some(node)),
                    Option::<fn(&Node) -> bool>::None,
                ))
                .unwrap_or_else(|| Debug_.fail(Some("Could not determine parsed source file.")));
                get_or_create_emit_node(source_file)
                    .borrow_mut()
                    .annotated_nodes
                    .as_mut()
                    .unwrap()
                    .push(node.node_wrapper());
            }

            *node.maybe_emit_node_mut() = Some(Rc::new(RefCell::new(Default::default())));
        }
        Some(node_emit_node) => {
            Debug_.assert(
                !(*node_emit_node)
                    .borrow()
                    .flags
                    .map_or(false, |flags| flags.intersects(EmitFlags::Immutable)),
                Some("Invalid attempt to mutate an immutable node."),
            );
        }
    }
    node.maybe_emit_node().unwrap()
}

pub fn dispose_emit_nodes<TSourceFile: Borrow<Node>>(
    source_file: Option<TSourceFile /*SourceFile*/>,
) {
    unimplemented!()
}

pub fn set_emit_flags(node: Rc<Node>, emit_flags: EmitFlags) -> Rc<Node> {
    get_or_create_emit_node(&node).borrow_mut().flags = Some(emit_flags);
    node
}

pub fn add_emit_flags(node: Rc<Node>, emit_flags: EmitFlags) -> Rc<Node> {
    let emit_node = get_or_create_emit_node(&node);
    let mut emit_node = emit_node.borrow_mut();
    emit_node.flags = Some(emit_node.flags.unwrap_or(EmitFlags::None) | emit_flags);
    node
}

pub(crate) fn get_starts_on_new_line(node: &Node) -> bool {
    node.maybe_emit_node()
        .and_then(|emit_node| (*emit_node).borrow().starts_on_new_line)
        .unwrap_or(false)
}

pub(crate) fn set_starts_on_new_line(node: &Node, new_line: bool) /*-> Rc<Node>*/
{
    get_or_create_emit_node(node)
        .borrow_mut()
        .starts_on_new_line = Some(new_line);
    // node
}

pub fn set_comment_range<TRange: ReadonlyTextRange /*TextRange*/>(node: &Node, range: &TRange)
/*-> Rc<Node>*/
{
    // unimplemented!()
}

pub fn set_synthetic_leading_comments(node: &Node, comments: Option<Vec<Rc<SynthesizedComment>>>)
/*-> Rc<Node>*/
{
    unimplemented!()
}

pub fn add_synthetic_leading_comment(
    node: &Node,
    kind: SyntaxKind, /*SyntaxKind.SingleLineCommentTrivia | SyntaxKind.MultiLineCommentTrivia*/
    text: &str,
    has_trailing_new_line: Option<bool>,
) /*-> Rc<Node>*/
{
    unimplemented!()
}

pub fn get_constant_value(node: &Node /*AccessExpression*/) -> Option<StringOrNumber> {
    node.maybe_emit_node()
        .and_then(|node_emit_node| (*node_emit_node).borrow().constant_value.clone())
}

pub fn get_emit_helpers(node: &Node) -> Option<Vec<Rc<EmitHelper>>> {
    node.maybe_emit_node()
        .and_then(|node_emit_node| (*node_emit_node).borrow().helpers.clone())
}

pub(crate) fn get_snippet_element(node: &Node) -> Option<SnippetElement> {
    unimplemented!()
}
