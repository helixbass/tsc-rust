use std::{borrow::Borrow, io};

use gc::Gc;

use crate::{
    is_block, is_statement, set_text_range_pos_end, single_or_undefined, with_factory, Debug_,
    Node, NodeArray, NodeInterface, NonEmpty, OptionTry, ReadonlyTextRange, SingleNodeOrVecNode,
    TransformationContext, VisitResult, VisitResultInterface,
};

mod try_visit_each_child;
mod visit_each_child;
pub use try_visit_each_child::*;
pub use visit_each_child::*;

use crate::return_ok_default_if_none;

pub fn visit_node(
    node: &Node,
    visitor: Option<impl FnMut(&Node) -> VisitResult>,
    test: Option<impl Fn(&Node) -> bool>,
    lift: Option<impl Fn(&[Gc<Node>]) -> Gc<Node>>,
) -> Gc<Node> {
    maybe_visit_node(Some(node), visitor, test, lift).unwrap()
}

pub fn maybe_visit_node(
    node: Option<impl Borrow<Node>>,
    visitor: Option<impl FnMut(&Node) -> VisitResult>,
    test: Option<impl Fn(&Node) -> bool>,
    lift: Option<impl Fn(&[Gc<Node>]) -> Gc<Node>>,
) -> Option<Gc<Node>> {
    try_maybe_visit_node(
        node,
        visitor.map(|mut visitor| move |node: &Node| Ok(visitor(node))),
        test,
        lift,
    )
    .unwrap()
}

pub fn try_visit_node(
    node: &Node,
    visitor: Option<impl FnMut(&Node) -> io::Result<VisitResult>>,
    test: Option<impl Fn(&Node) -> bool>,
    lift: Option<impl Fn(&[Gc<Node>]) -> Gc<Node>>,
) -> io::Result<Gc<Node>> {
    Ok(try_maybe_visit_node(Some(node), visitor, test, lift)?.unwrap())
}

pub fn try_maybe_visit_node(
    node: Option<impl Borrow<Node>>,
    visitor: Option<impl FnMut(&Node) -> io::Result<VisitResult>>,
    test: Option<impl Fn(&Node) -> bool>,
    lift: Option<impl Fn(&[Gc<Node>]) -> Gc<Node>>,
) -> io::Result<Option<Gc<Node>>> {
    let node = return_ok_default_if_none!(node);
    let node = node.borrow();
    let mut visitor = return_ok_default_if_none!(visitor);

    let visited = visitor(node)?;
    if visited.ptr_eq_node(node) {
        return Ok(Some(node.node_wrapper()));
    }
    let visited = return_ok_default_if_none!(visited);
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
    Ok(visited_node)
}

pub fn visit_nodes(
    nodes: &NodeArray,
    visitor: Option<impl FnMut(&Node) -> VisitResult>,
    test: Option<impl Fn(&Node) -> bool>,
    start: Option<usize>,
    count: Option<usize>,
) -> Gc<NodeArray> {
    maybe_visit_nodes(Some(nodes), visitor, test, start, count).unwrap()
}

pub fn maybe_visit_nodes(
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

pub fn try_visit_nodes(
    nodes: &NodeArray,
    visitor: Option<impl FnMut(&Node) -> io::Result<VisitResult>>,
    test: Option<impl Fn(&Node) -> bool>,
    start: Option<usize>,
    count: Option<usize>,
) -> io::Result<Gc<NodeArray>> {
    Ok(try_maybe_visit_nodes(Some(nodes), visitor, test, start, count)?.unwrap())
}

pub fn try_maybe_visit_nodes(
    nodes: Option<&NodeArray>,
    visitor: Option<impl FnMut(&Node) -> io::Result<VisitResult>>,
    test: Option<impl Fn(&Node) -> bool>,
    start: Option<usize>,
    count: Option<usize>,
) -> io::Result<Option<Gc<NodeArray>>> {
    if nodes.is_none() {
        return Ok(None);
    }
    let nodes = nodes.unwrap();
    if visitor.is_none() {
        return Ok(Some(nodes.rc_wrapper()));
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
        let visited = node.try_and_then(|node| visitor(node))?;
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
        return Ok(Some(updated_array));
    }

    Ok(Some(nodes.rc_wrapper()))
}

pub fn visit_lexical_environment(
    _statements: &NodeArray, /*<Statement>*/
    _visitor: impl FnMut(&Node) -> VisitResult,
    _context: &(impl TransformationContext + ?Sized),
    _start: Option<isize>,
    _ensure_use_strict: Option<bool>,
    _nodes_visitor: Option<
        impl FnMut(
            Option<&NodeArray>,
            Option<&mut dyn FnMut(&Node) -> VisitResult>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> Option<Gc<NodeArray>>,
    >,
) -> Gc<NodeArray> {
    unimplemented!()
}

pub fn try_visit_lexical_environment(
    statements: &NodeArray, /*<Statement>*/
    visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
) -> io::Result<Gc<NodeArray>> {
    try_visit_lexical_environment_full(
        statements,
        visitor,
        context,
        None,
        None,
        Option::<
            fn(
                Option<&NodeArray>,
                Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                Option<&dyn Fn(&Node) -> bool>,
                Option<usize>,
                Option<usize>,
            ) -> io::Result<Option<Gc<NodeArray>>>,
        >::None,
    )
}

pub fn try_visit_lexical_environment_full(
    _statements: &NodeArray, /*<Statement>*/
    _visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    _context: &(impl TransformationContext + ?Sized),
    _start: Option<isize>,
    _ensure_use_strict: Option<bool>,
    _nodes_visitor: Option<
        impl FnMut(
            Option<&NodeArray>,
            Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> io::Result<Option<Gc<NodeArray>>>,
    >,
) -> io::Result<Gc<NodeArray>> {
    unimplemented!()
}

pub fn visit_parameter_list(
    _nodes: Option<&NodeArray>,
    _visitor: impl FnMut(&Node) -> VisitResult,
    _context: &(impl TransformationContext + ?Sized),
    _nodes_visitor: Option<
        impl FnMut(
            Option<&NodeArray>,
            Option<&mut dyn FnMut(&Node) -> VisitResult>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> Option<Gc<NodeArray>>,
    >,
) -> Option<Gc<NodeArray>> {
    unimplemented!()
}

pub fn try_visit_parameter_list(
    nodes: Option<&NodeArray>,
    visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
) -> io::Result<Option<Gc<NodeArray>>> {
    try_visit_parameter_list_full(
        nodes,
        visitor,
        context,
        Option::<
            fn(
                Option<&NodeArray>,
                Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                Option<&dyn Fn(&Node) -> bool>,
                Option<usize>,
                Option<usize>,
            ) -> io::Result<Option<Gc<NodeArray>>>,
        >::None,
    )
}

pub fn try_visit_parameter_list_full(
    _nodes: Option<&NodeArray>,
    _visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    _context: &(impl TransformationContext + ?Sized),
    _nodes_visitor: Option<
        impl FnMut(
            Option<&NodeArray>,
            Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<usize>,
            Option<usize>,
        ) -> io::Result<Option<Gc<NodeArray>>>,
    >,
) -> io::Result<Option<Gc<NodeArray>>> {
    unimplemented!()
}

pub fn visit_function_body(
    _node: Option<&Node /*ConciseBody*/>,
    _visitor: impl FnMut(&Node) -> VisitResult,
    _context: &(impl TransformationContext + ?Sized),
    _node_visitor: Option<
        impl FnMut(
            Option<&Node>,
            Option<&mut dyn FnMut(&Node) -> VisitResult>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
        ) -> Option<Gc<Node>>,
    >,
) -> Option<Gc<Node /*ConciseBody*/>> {
    unimplemented!()
}

pub fn try_visit_function_body(
    node: Option<&Node /*ConciseBody*/>,
    visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
) -> io::Result<Option<Gc<Node /*ConciseBody*/>>> {
    try_visit_function_body_full(
        node,
        visitor,
        context,
        Option::<
            fn(
                Option<&Node>,
                Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
                Option<&dyn Fn(&Node) -> bool>,
                Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
            ) -> io::Result<Option<Gc<Node>>>,
        >::None,
    )
}

pub fn try_visit_function_body_full(
    _node: Option<&Node /*ConciseBody*/>,
    _visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    _context: &(impl TransformationContext + ?Sized),
    _node_visitor: Option<
        impl FnMut(
            Option<&Node>,
            Option<&mut dyn FnMut(&Node) -> io::Result<VisitResult>>,
            Option<&dyn Fn(&Node) -> bool>,
            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
        ) -> io::Result<Option<Gc<Node>>>,
    >,
) -> io::Result<Option<Gc<Node /*ConciseBody*/>>> {
    unimplemented!()
}

pub fn visit_iteration_body(
    body: &Node, /*Statement*/
    mut visitor: impl FnMut(&Node) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
) -> Gc<Node /*Statement*/> {
    try_visit_iteration_body(body, |a| Ok(visitor(a)), context).unwrap()
}

pub fn try_visit_iteration_body(
    body: &Node, /*Statement*/
    visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    context: &(impl TransformationContext + ?Sized),
) -> io::Result<Gc<Node /*Statement*/>> {
    context.start_block_scope();
    let updated = try_visit_node(
        body,
        Some(visitor),
        Some(is_statement),
        Some(|nodes: &[Gc<Node>]| context.factory().lift_to_block(nodes)),
    )?;
    let declarations = context.end_block_scope();
    if let Some(mut declarations) = declarations.non_empty()
    /*some(declarations)*/
    {
        if is_block(&updated) {
            declarations.extend(updated.as_block().statements.iter().cloned());
            return Ok(context.factory().update_block(&updated, declarations));
        }
        declarations.push(updated);
        return Ok(context.factory().create_block(declarations, None).wrap());
    }
    Ok(updated)
}

fn extract_single_node(nodes: &[Gc<Node>]) -> Option<Gc<Node>> {
    Debug_.assert(nodes.len() <= 1, Some("Too many nodes written to output."));
    single_or_undefined(Some(nodes)).cloned()
}
