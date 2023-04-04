use gc::Gc;
use std::borrow::Borrow;

use crate::{
    is_block, is_statement, set_text_range_pos_end, single_or_undefined, with_factory,
    with_synthetic_factory, Debug_, Node, NodeArray, NodeInterface, NonEmpty, ReadonlyTextRange,
    SingleNodeOrVecNode, SyntaxKind, TransformationContext, VisitResult, VisitResultInterface,
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

pub fn visit_nodes(
    nodes: Option<&NodeArray>,
    visitor: Option<impl FnMut(&Node) -> VisitResult>,
    test: Option<impl Fn(&Node) -> bool>,
    start: Option<usize>,
    count: Option<usize>,
) -> Option<Gc<NodeArray>> {
    let nodes = nodes?;
    if visitor.is_none() {
        return Some(nodes.rc_wrapper());
    }
    let mut visitor = visitor.unwrap();

    let mut updated: Option<Vec<Gc<Node>>> = Default::default();

    let length = nodes.len();
    let start = start.unwrap_or(0); /*start < 0*/

    let count = if count.is_none() || count.unwrap() > length - start {
        length - start
    } else {
        count.unwrap()
    };

    let mut has_trailing_comma: Option<bool> = Default::default();
    let mut pos: isize = -1;
    let mut end: isize = -1;
    if start > 0 || count < length {
        updated = Some(vec![]);
        has_trailing_comma = Some(nodes.has_trailing_comma && start + count == length);
    }

    for i in 0..count {
        let node = nodes.get(i + start);
        let visited = node.and_then(|node| visitor(node));
        if updated.is_some()
            || match visited.as_ref() {
                None => true,
                Some(visited) => !matches!(
                    node,
                    Some(node) if visited.ptr_eq_node(node)
                ),
            }
        {
            if updated.is_none() {
                updated = Some(nodes[0..i].to_owned());
                has_trailing_comma = Some(nodes.has_trailing_comma);
                pos = nodes.pos();
                end = nodes.end();
            }
            if let Some(visited) = visited {
                match &visited {
                    SingleNodeOrVecNode::VecNode(visited) => {
                        for visited_node in visited {
                            Debug_.assert_node(
                                Some(&**visited_node),
                                test.as_ref().map(|test| |node: &Node| test(node)),
                                None,
                            );
                            updated.as_mut().unwrap().push(visited_node.clone());
                        }
                    }
                    SingleNodeOrVecNode::SingleNode(visited) => {
                        Debug_.assert_node(
                            Some(&**visited),
                            test.as_ref().map(|test| |node: &Node| test(node)),
                            None,
                        );
                        updated.as_mut().unwrap().push(visited.clone());
                    }
                }
            }
        }
    }

    if let Some(updated) = updated {
        let updated_array =
            with_factory(|factory_| factory_.create_node_array(Some(updated), has_trailing_comma));
        set_text_range_pos_end(&*updated_array, pos, end);
        return Some(updated_array);
    }

    Some(nodes.rc_wrapper())
}

pub fn visit_parameter_list<
    TContext: TransformationContext + ?Sized,
    TNodesVisitorVisitor: FnMut(&Node) -> VisitResult,
    TNodesVisitorTest: Fn(&Node) -> bool,
>(
    nodes: Option<&NodeArray>,
    visitor: impl FnMut(&Node) -> VisitResult,
    context: &TContext,
    nodes_visitor: Option<
        impl FnMut(
            Option<&NodeArray>,
            Option<TNodesVisitorVisitor>,
            Option<TNodesVisitorTest>,
            Option<usize>,
            Option<usize>,
        ) -> Gc<NodeArray>,
    >,
) -> Option<Gc<NodeArray>> {
    unimplemented!()
}

pub fn visit_function_body<
    TContext: TransformationContext + ?Sized,
    TNodeVisitorNode: Borrow<Node>,
    TNodeVisitorVisitor: FnMut(&Node) -> VisitResult,
    TNodeVisitorTest: Fn(&Node) -> bool,
    TNodeVisitorLift: Fn(&[Gc<Node>]) -> Gc<Node>,
>(
    node: Option<&Node /*ConciseBody*/>,
    visitor: impl FnMut(&Node) -> VisitResult,
    context: &TContext,
    node_visitor: Option<
        impl FnMut(
            Option<TNodeVisitorNode>,
            Option<TNodeVisitorVisitor>,
            Option<TNodeVisitorTest>,
            Option<TNodeVisitorLift>,
        ) -> Option<Gc<Node>>,
    >,
) -> Option<Gc<Node /*ConciseBody*/>> {
    unimplemented!()
}

pub fn visit_iteration_body<TContext: TransformationContext + ?Sized>(
    body: &Node, /*Statement*/
    visitor: impl FnMut(&Node) -> VisitResult,
    context: &TContext,
) -> Gc<Node /*Statement*/> {
    context.start_block_scope();
    let updated = visit_node(
        Some(body),
        Some(visitor),
        Some(is_statement),
        Some(|nodes: &[Gc<Node>]| {
            with_synthetic_factory(|synthetic_factory_| {
                context.factory().lift_to_block(synthetic_factory_, nodes)
            })
        }),
    )
    .unwrap();
    let declarations = context.end_block_scope();
    if let Some(mut declarations) = declarations.non_empty()
    /*some(declarations)*/
    {
        if is_block(&updated) {
            declarations.extend(updated.as_block().statements.iter().cloned());
            return with_synthetic_factory(|synthetic_factory_| {
                context
                    .factory()
                    .update_block(synthetic_factory_, &updated, declarations)
            });
        }
        declarations.push(updated);
        return with_synthetic_factory(|synthetic_factory_| {
            context
                .factory()
                .create_block(synthetic_factory_, declarations, None)
                .into()
        });
    }
    updated
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
        ) -> Gc<NodeArray>,
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
