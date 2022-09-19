#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use crate::{
    CompilerOptions, Node, NodeFlags, NodeInterface, PseudoBigInt, ReadonlyTextRange, Symbol,
    SyntaxKind,
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
    unimplemented!()
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
