use gc::Gc;
use std::borrow::Borrow;

use crate::{
    single_or_undefined, Debug_, Node, NodeArray, NodeInterface, SingleNodeOrVecNode, SyntaxKind,
    TransformationContext, VisitResult, VisitResultInterface,
};

pub fn visit_node(
    node: Option<impl Borrow<Node>>,
    visitor: Option<impl FnMut(&Node) -> VisitResult>,
    test: Option<impl Fn(&Node) -> bool>,
    lift: Option<impl Fn(&[Gc<Node>]) -> Gc<Node>>,
) -> Option<Gc<Node>> {
    let node = node?;
    let node = node.borrow();
    let mut visitor = visitor?;

    let visited = visitor(node);
    if visited.ptr_eq_node(node) {
        return Some(node.node_wrapper());
    }
    let visited = visited?;
    let visited_node = match &visited {
        SingleNodeOrVecNode::VecNode(visited) => {
            if let Some(lift) = lift {
                Some(lift(visited))
            } else {
                extract_single_node(visited)
            }
        }
        SingleNodeOrVecNode::SingleNode(visited) => Some(visited.clone()),
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
    TContext: TransformationContext + ?Sized,
    TNodesVisitorVisitor: FnMut(&Node) -> VisitResult,
    TNodesVisitorTest: Fn(&Node) -> bool,
    TNodeVisitorNode: Borrow<Node>,
    TNodeVisitorVisitor: FnMut(&Node) -> VisitResult,
    TNodeVisitorTest: Fn(&Node) -> bool,
    TNodeVisitorLift: Fn(&[Gc<Node>]) -> Gc<Node>,
>(
    node: Option<impl Borrow<Node>>,
    visitor: impl FnMut(&Node) -> VisitResult,
    context: &TContext,
    nodes_visitor: Option<
        impl FnMut(
            Option<&NodeArray>,
            Option<TNodesVisitorVisitor>,
            Option<TNodesVisitorTest>,
            Option<usize>,
            Option<usize>,
        ) -> NodeArray,
    >,
    token_visitor: Option<impl FnMut(&Node) -> VisitResult>,
    node_visitor: Option<
        impl FnMut(
            Option<TNodeVisitorNode>,
            Option<TNodeVisitorVisitor>,
            Option<TNodeVisitorTest>,
            Option<TNodeVisitorLift>,
        ) -> Option<Gc<Node>>,
    >,
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
