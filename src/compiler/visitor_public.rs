use std::borrow::Borrow;
use std::rc::Rc;

use crate::{Node, NodeArray, NodeInterface, SyntaxKind, TransformationContext, VisitResult};

pub fn visit_node<
    TNode: Borrow<Node>,
    TVisitor: FnMut(&Node) -> VisitResult,
    TTest: Fn(&Node) -> bool,
    TLift: Fn(&[Rc<Node>]) -> Rc<Node>,
>(
    node: Option<TNode>,
    visitor: Option<TVisitor>,
    test: Option<TTest>,
    lift: Option<TLift>,
) -> Option<Rc<Node>> {
    unimplemented!()
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
    TNodeVisitorLift: Fn(&[Rc<Node>]) -> Rc<Node>,
    TNodeVisitor: FnMut(
        Option<TNodeVisitorNode>,
        Option<TNodeVisitorVisitor>,
        Option<TNodeVisitorTest>,
        Option<TNodeVisitorLift>,
    ) -> Option<Rc<Node>>,
>(
    node: Option<TNode>,
    visitor: TVisitor,
    context: &TContext,
    nodes_visitor: Option<TNodesVisitor>,
    token_visitor: Option<TTokenVisitor>,
    node_visitor: Option<TNodeVisitor>,
) -> Option<Rc<Node>> {
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
