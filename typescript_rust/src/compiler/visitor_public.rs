use gc::Gc;
use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use crate::{
    single_or_undefined, Debug_, Node, NodeArray, NodeInterface, SyntaxKind, TransformationContext,
    VisitResult,
};

pub fn visit_node<
    TNode: Borrow<Node>,
    TVisitor: FnMut(&Node) -> VisitResult,
    TTest: Fn(&Node) -> bool,
    TLift: Fn(&[Gc<Node>]) -> Gc<Node>,
>(
    node: Option<TNode>,
    visitor: Option<TVisitor>,
    test: Option<TTest>,
    lift: Option<TLift>,
) -> Option<Gc<Node>> {
    let node = node?;
    let node = node.borrow();
    let mut visitor = visitor?;

    let visited = visitor(node);
    if matches!(
        visited.as_ref(),
        Some(visited) if visited.len() == 1 &&
            ptr::eq(
                &*visited[0],
                node,
            )
    ) {
        return Some(node.node_wrapper());
    }
    let visited = visited?;
    let visited_node = if let Some(lift) = lift {
        Some(lift(&visited))
    } else {
        extract_single_node(&visited)
    };

    Debug_.assert_node(visited_node.as_deref(), test, None);
    visited_node
}

pub fn visit_nodes<TVisitor: FnMut(&Node) -> VisitResult, TTest: Fn(&Node) -> bool>(
    nodes: Option<&NodeArray>,
    visitor: Option<TVisitor>,
    test: Option<TTest>,
    start: Option<usize>,
    count: Option<usize>,
) -> Option<NodeArray> {
    unimplemented!()
}

pub fn visit_each_child<
    TNode: Borrow<Node>,
    TVisitor: FnMut(&Node) -> VisitResult,
    TContext: TransformationContext,
    TNodesVisitorVisitor: FnMut(&Node) -> VisitResult,
    TNodesVisitorTest: Fn(&Node) -> bool,
    TNodesVisitor: FnMut(
        Option<&NodeArray>,
        Option<TNodesVisitorVisitor>,
        Option<TNodesVisitorTest>,
        Option<usize>,
        Option<usize>,
    ) -> NodeArray,
    TTokenVisitor: FnMut(&Node) -> VisitResult,
    TNodeVisitorNode: Borrow<Node>,
    TNodeVisitorVisitor: FnMut(&Node) -> VisitResult,
    TNodeVisitorTest: Fn(&Node) -> bool,
    TNodeVisitorLift: Fn(&[Gc<Node>]) -> Gc<Node>,
    TNodeVisitor: FnMut(
        Option<TNodeVisitorNode>,
        Option<TNodeVisitorVisitor>,
        Option<TNodeVisitorTest>,
        Option<TNodeVisitorLift>,
    ) -> Option<Gc<Node>>,
>(
    node: Option<TNode>,
    visitor: TVisitor,
    context: &TContext,
    nodes_visitor: Option<TNodesVisitor>,
    token_visitor: Option<TTokenVisitor>,
    node_visitor: Option<TNodeVisitor>,
) -> Option<Gc<Node>> {
    let node = node?;
    let node: &Node = node.borrow();

    let kind = node.kind();

    if kind > SyntaxKind::FirstToken && kind <= SyntaxKind::LastToken
        || kind == SyntaxKind::ThisType
    {
        return Some(node.node_wrapper());
    }

    let factory = context.factory();

    // unimplemented!()
    Some(node.node_wrapper())
}

fn extract_single_node(nodes: &[Gc<Node>]) -> Option<Gc<Node>> {
    Debug_.assert(nodes.len() <= 1, Some("Too many nodes written to output."));
    single_or_undefined(Some(nodes)).cloned()
}
