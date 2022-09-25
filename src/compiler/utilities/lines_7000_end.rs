#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use crate::{
    for_each_child_recursively, has_jsdoc_nodes, is_jsdoc_node, CompilerOptions,
    ForEachChildRecursivelyCallbackReturn, Node, NodeArray, NodeFlags, NodeInterface, PseudoBigInt,
    ReadonlyTextRange, Symbol, SyntaxKind,
};

pub fn skip_type_checking<TIsSourceOfProjectReferenceRedirect: Fn(&str) -> bool>(
    source_file: &Node, /*SourceFile*/
    options: &CompilerOptions,
    is_source_of_project_reference_redirect: TIsSourceOfProjectReferenceRedirect,
) -> bool {
    // unimplemented!()
    false
}

pub fn parse_pseudo_big_int(string_value: &str) -> String {
    string_value.to_string()
}

pub fn pseudo_big_int_to_string(pseudo_big_int: &PseudoBigInt) -> String {
    let negative = pseudo_big_int.negative;
    let base_10_value = &pseudo_big_int.base_10_value;
    format!(
        "{}{}",
        if negative && base_10_value != "0" {
            "-"
        } else {
            ""
        },
        base_10_value
    )
}

pub fn is_valid_type_only_alias_use_site(use_site: &Node) -> bool {
    unimplemented!()
}

pub fn set_text_range_pos<TRange: ReadonlyTextRange>(range: &TRange, pos: isize) -> &TRange {
    range.set_pos(pos);
    range
}

fn set_text_range_end<TRange: ReadonlyTextRange>(range: &TRange, end: isize) -> &TRange {
    range.set_end(end);
    range
}

pub fn set_text_range_pos_end<TRange: ReadonlyTextRange>(range: &TRange, pos: isize, end: isize) {
    set_text_range_end(set_text_range_pos(range, pos), end);
}

pub fn set_text_range_pos_width<TRange: ReadonlyTextRange>(
    range: &TRange,
    pos: isize,
    width: isize,
) {
    set_text_range_pos_end(range, pos, pos + width);
}

pub fn set_node_flags<TNode: Borrow<Node>>(
    node: Option<TNode>,
    new_flags: NodeFlags,
) -> Option<Rc<Node>> {
    node.map(|node| {
        let node = node.borrow();
        node.set_flags(new_flags);
        node.node_wrapper()
    })
}

pub fn set_parent<TParent: Borrow<Node>>(child: &Node, parent: Option<TParent>) -> &Node {
    if let Some(parent) = parent {
        let parent = parent.borrow();
        child.set_parent(parent.node_wrapper());
    }
    child
}

pub fn maybe_set_parent<TChild: Borrow<Node>>(
    child: Option<TChild>,
    parent: Option<Rc<Node>>,
) -> Option<TChild> {
    if let Some(child) = child.as_ref() {
        let child = child.borrow();
        if let Some(parent) = parent {
            child.set_parent(parent.clone());
        }
    }
    child
}

pub fn set_parent_recursive<TNode: Borrow<Node>>(root_node: Option<TNode>, incremental: bool) {
    if root_node.is_none() {
        return /*rootNode*/;
    }
    let root_node = root_node.unwrap();
    let root_node: &Node = root_node.borrow();
    let is_jsdoc_node_root_node = is_jsdoc_node(root_node);
    for_each_child_recursively(
        root_node,
        |child, parent| -> Option<ForEachChildRecursivelyCallbackReturn<()>> {
            if is_jsdoc_node_root_node {
                bind_parent_to_child_ignoring_jsdoc(incremental, child, parent)
            } else {
                bind_parent_to_child(incremental, child, parent)
            }
        },
        Option::<fn(&NodeArray, &Node) -> Option<ForEachChildRecursivelyCallbackReturn<()>>>::None,
    );
}

fn bind_parent_to_child_ignoring_jsdoc(
    incremental: bool,
    child: &Node,
    parent: &Node,
) -> Option<ForEachChildRecursivelyCallbackReturn<()>> {
    if incremental && ptr::eq(&*child.parent(), parent) {
        return Some(ForEachChildRecursivelyCallbackReturn::Skip);
    }
    set_parent(child, Some(parent));
    None
}

fn bind_jsdoc(
    incremental: bool,
    child: &Node,
) -> Option<ForEachChildRecursivelyCallbackReturn<()>> {
    if has_jsdoc_nodes(child) {
        for doc in &child.maybe_js_doc().unwrap() {
            bind_parent_to_child_ignoring_jsdoc(incremental, doc, child);
            for_each_child_recursively(
                doc,
                |child, parent| -> Option<ForEachChildRecursivelyCallbackReturn<()>> {
                    bind_parent_to_child_ignoring_jsdoc(incremental, child, parent)
                },
                Option::<fn(&NodeArray, &Node) -> Option<ForEachChildRecursivelyCallbackReturn<()>>>::None,
            );
        }
    }
    None
}

fn bind_parent_to_child(
    incremental: bool,
    child: &Node,
    parent: &Node,
) -> Option<ForEachChildRecursivelyCallbackReturn<()>> {
    bind_parent_to_child_ignoring_jsdoc(incremental, child, parent)
        .or_else(|| bind_jsdoc(incremental, child))
}

pub fn expression_result_is_unused(node: &Node /*Expression*/) -> bool {
    unimplemented!()
}

pub fn contains_ignored_path(path: &str) -> bool {
    unimplemented!()
}

pub fn has_context_sensitive_parameters(node: &Node /*FunctionLikeDeclaration*/) -> bool {
    unimplemented!()
}

pub fn is_infinity_or_nan_string(name: &str) -> bool {
    unimplemented!()
}

pub fn is_parameter_or_catch_clause_variable(symbol: &Symbol) -> bool {
    unimplemented!()
}

pub fn is_function_expression_or_arrow_function(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction
    )
}
