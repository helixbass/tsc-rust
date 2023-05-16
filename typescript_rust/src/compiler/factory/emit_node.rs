use gc::{Gc, GcCell};
use std::borrow::Borrow;
use std::rc::Rc;

use crate::{
    get_parse_tree_node, is_parse_tree_node, maybe_get_source_file_of_node, BaseTextRange, Debug_,
    EmitFlags, EmitHelper, EmitNode, Node, NodeInterface, ReadonlyTextRange, SnippetElement,
    SourceMapRange, StringOrNumber, SyntaxKind, SynthesizedComment,
};

pub(crate) fn get_or_create_emit_node(node: &Node) -> Gc<GcCell<EmitNode>> {
    match node.maybe_emit_node() {
        None => {
            if is_parse_tree_node(node) {
                if node.kind() == SyntaxKind::SourceFile {
                    let ret = Gc::new(GcCell::new(EmitNode {
                        annotated_nodes: Some(vec![node.node_wrapper()]),
                        // ..Default::default()
                        flags: Default::default(),
                        leading_comments: Default::default(),
                        trailing_comments: Default::default(),
                        comment_range: Default::default(),
                        source_map_range: Default::default(),
                        token_source_map_ranges: Default::default(),
                        constant_value: Default::default(),
                        external_helpers_module_name: Default::default(),
                        external_helpers: Default::default(),
                        helpers: Default::default(),
                        starts_on_new_line: Default::default(),
                        snippet_element: Default::default(),
                    }));
                    *node.maybe_emit_node_mut() = Some(ret.clone());
                    return ret;
                }

                let ref source_file = maybe_get_source_file_of_node(get_parse_tree_node(
                    maybe_get_source_file_of_node(Some(node)),
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

            *node.maybe_emit_node_mut() = Some(Gc::new(GcCell::new(Default::default())));
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

pub fn dispose_emit_nodes(source_file: Option<impl Borrow<Node> /*SourceFile*/>) {
    let annotated_nodes = maybe_get_source_file_of_node(get_parse_tree_node(
        source_file,
        Option::<fn(&Node) -> _>::None,
    ))
    .and_then(|source_file| source_file.maybe_emit_node())
    .and_then(|emit_node| (*emit_node).borrow().annotated_nodes.clone());
    if let Some(annotated_nodes) = annotated_nodes.as_ref() {
        for node in annotated_nodes {
            node.set_emit_node(None);
        }
    }
}

pub fn remove_all_comments<TNode: Borrow<Node>>(_node: TNode) -> TNode {
    unimplemented!()
}

pub fn set_emit_flags<TNode: Borrow<Node>>(node: TNode, emit_flags: EmitFlags) -> TNode {
    get_or_create_emit_node(node.borrow()).borrow_mut().flags = Some(emit_flags);
    node
}

pub fn add_emit_flags<TNode: Borrow<Node>>(node: TNode, emit_flags: EmitFlags) -> TNode {
    let emit_node = get_or_create_emit_node(node.borrow());
    let mut emit_node = emit_node.borrow_mut();
    emit_node.flags = Some(emit_node.flags.unwrap_or(EmitFlags::None) | emit_flags);
    node
}

pub fn get_source_map_range(node: &Node) -> Gc<SourceMapRange> {
    node.maybe_emit_node()
        .and_then(|node_emit_node| (*node_emit_node).borrow().source_map_range.clone())
        .unwrap_or_else(|| node.into())
}

pub fn set_source_map_range<TNode: Borrow<Node>>(
    node: TNode,
    range: Option<Gc<SourceMapRange>>,
) -> TNode {
    get_or_create_emit_node(node.borrow())
        .borrow_mut()
        .source_map_range = range;
    node
}

pub(crate) fn get_starts_on_new_line(node: &Node) -> Option<bool> {
    node.maybe_emit_node()
        .and_then(|emit_node| (*emit_node).borrow().starts_on_new_line)
}

pub(crate) fn set_starts_on_new_line(node: &Node, new_line: bool) /*-> Gc<Node>*/
{
    get_or_create_emit_node(node)
        .borrow_mut()
        .starts_on_new_line = Some(new_line);
    // node
}

pub fn get_comment_range(node: &Node) -> BaseTextRange {
    // TODO: these semantics wouldn't work if the return value is treated mutably?
    node.maybe_emit_node()
        .and_then(|node_emit_node| (*node_emit_node).borrow().comment_range.clone())
        .unwrap_or_else(|| BaseTextRange::new(node.pos(), node.end()))
}

pub fn set_comment_range(node: &Node, range: &impl ReadonlyTextRange /*TextRange*/)
/*-> Gc<Node>*/
{
    get_or_create_emit_node(node).borrow_mut().comment_range = Some(range.into());
}

pub fn set_comment_range_rc(
    node: Gc<Node>,
    range: &impl ReadonlyTextRange, /*TextRange*/
) -> Gc<Node> {
    set_comment_range(&node, range);
    node
}

pub fn get_synthetic_leading_comments(node: &Node) -> Option<Vec<Rc<SynthesizedComment>>> {
    node.maybe_emit_node()
        .and_then(|node_emit_node| (*node_emit_node).borrow().leading_comments.clone())
}

pub fn set_synthetic_leading_comments(node: &Node, comments: Option<Vec<Rc<SynthesizedComment>>>)
/*-> Gc<Node>*/
{
    get_or_create_emit_node(node).borrow_mut().leading_comments = comments;
}

pub fn set_synthetic_leading_comments_rc(
    node: Gc<Node>,
    comments: Option<Vec<Rc<SynthesizedComment>>>,
) -> Gc<Node> {
    set_synthetic_leading_comments(&node, comments);
    node
}

pub fn add_synthetic_leading_comment(
    node: &Node,
    kind: SyntaxKind, /*SyntaxKind.SingleLineCommentTrivia | SyntaxKind.MultiLineCommentTrivia*/
    text: &str,
    has_trailing_new_line: Option<bool>,
) /*-> Gc<Node>*/
{
    let mut synthetic_leading_comments = get_synthetic_leading_comments(node);
    if synthetic_leading_comments.is_none() {
        synthetic_leading_comments = Some(Default::default());
    }
    synthetic_leading_comments
        .as_mut()
        .unwrap()
        .push(Rc::new(SynthesizedComment {
            kind,
            has_trailing_new_line,
            text: text.to_owned(),
            has_leading_new_line: None,
        }));
    set_synthetic_leading_comments(node, synthetic_leading_comments);
}

pub fn get_synthetic_trailing_comments(node: &Node) -> Option<Vec<Rc<SynthesizedComment>>> {
    node.maybe_emit_node()
        .and_then(|node_emit_node| (*node_emit_node).borrow().trailing_comments.clone())
}

pub fn set_synthetic_trailing_comments(node: &Node, comments: Option<Vec<Rc<SynthesizedComment>>>)
/*-> Gc<Node>*/
{
    get_or_create_emit_node(node).borrow_mut().trailing_comments = comments;
}

pub fn add_synthetic_trailing_comment(
    node: &Node,
    kind: SyntaxKind, /*SyntaxKind.SingleLineCommentTrivia | SyntaxKind.MultiLineCommentTrivia*/
    text: &str,
    has_trailing_new_line: Option<bool>,
) /*-> Gc<Node>*/
{
    let mut synthetic_trailing_comments = get_synthetic_trailing_comments(node);
    if synthetic_trailing_comments.is_none() {
        synthetic_trailing_comments = Some(Default::default());
    }
    synthetic_trailing_comments
        .as_mut()
        .unwrap()
        .push(Rc::new(SynthesizedComment {
            kind,
            has_trailing_new_line,
            text: text.to_owned(),
            has_leading_new_line: None,
        }));
    set_synthetic_trailing_comments(node, synthetic_trailing_comments);
}

pub fn get_constant_value(node: &Node /*AccessExpression*/) -> Option<StringOrNumber> {
    node.maybe_emit_node()
        .and_then(|node_emit_node| (*node_emit_node).borrow().constant_value.clone())
}

pub fn set_constant_value(
    _node: &Node, /*AccessExpression*/
    _value: StringOrNumber,
) -> Gc<Node /*AccessExpression*/> {
    unimplemented!()
}

pub fn add_emit_helper(_node: &Node, _helper: Gc<EmitHelper>) -> Gc<Node> {
    unimplemented!()
}

pub fn add_emit_helpers(_node: &Node, _helpers: Option<&[Gc<EmitHelper>]>) -> Gc<Node> {
    unimplemented!()
}

pub fn get_emit_helpers(node: &Node) -> Option<Vec<Gc<EmitHelper>>> {
    node.maybe_emit_node()
        .and_then(|node_emit_node| (*node_emit_node).borrow().helpers.clone())
}

pub(crate) fn get_snippet_element(node: &Node) -> Option<SnippetElement> {
    node.maybe_emit_node()
        .and_then(|node_emit_node| (*node_emit_node).borrow().snippet_element)
}
