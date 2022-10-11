#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use crate::{
    find_ancestor, first_or_undefined, for_each_child_recursively,
    get_effective_type_annotation_node, get_root_declaration, has_jsdoc_nodes,
    has_syntactic_modifier, ignored_paths, is_binary_expression, is_comma_list_expression,
    is_expression_node, is_expression_statement, is_for_statement, is_identifier, is_jsdoc_node,
    is_parameter, is_parenthesized_expression, is_part_of_type_query,
    is_shorthand_property_assignment, is_type_reference_node, is_void_expression, last,
    parameter_is_this_keyword, some, string_contains, CompilerOptions, Debug_,
    FindAncestorCallbackReturn, ForEachChildRecursivelyCallbackReturn, ModifierFlags,
    NamedDeclarationInterface, Node, NodeArray, NodeFlags, NodeInterface, PseudoBigInt,
    ReadonlyTextRange, Symbol, SymbolInterface, SyntaxKind,
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
    use_site.flags().intersects(NodeFlags::Ambient)
        || is_part_of_type_query(use_site)
        || is_identifier_in_non_emitting_heritage_clause(use_site)
        || is_part_of_possibly_valid_type_or_abstract_computed_property_name(use_site)
        || !(is_expression_node(use_site) || is_shorthand_property_name_use_site(use_site))
}

fn is_shorthand_property_name_use_site(use_site: &Node) -> bool {
    is_identifier(use_site)
        && is_shorthand_property_assignment(&use_site.parent())
        && ptr::eq(
            &*use_site.parent().as_shorthand_property_assignment().name(),
            use_site,
        )
}

fn is_part_of_possibly_valid_type_or_abstract_computed_property_name(node: &Node) -> bool {
    let mut node = node.node_wrapper();
    while matches!(
        node.kind(),
        SyntaxKind::Identifier | SyntaxKind::PropertyAccessExpression
    ) {
        node = node.parent();
    }
    if node.kind() != SyntaxKind::ComputedPropertyName {
        return false;
    }
    if has_syntactic_modifier(&node.parent(), ModifierFlags::Abstract) {
        return true;
    }
    let container_kind = node.parent().parent().kind();
    matches!(
        container_kind,
        SyntaxKind::InterfaceDeclaration | SyntaxKind::TypeLiteral
    )
}

fn is_identifier_in_non_emitting_heritage_clause(node: &Node) -> bool {
    if node.kind() != SyntaxKind::Identifier {
        return false;
    }
    let heritage_clause = find_ancestor(node.maybe_parent(), |parent| match parent.kind() {
        SyntaxKind::HeritageClause => true.into(),
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ExpressionWithTypeArguments => {
            false.into()
        }
        _ => FindAncestorCallbackReturn::Quit,
    });
    matches!(
        heritage_clause.as_ref(),
        Some(heritage_clause) if heritage_clause.as_heritage_clause().token == SyntaxKind::ImplementsKeyword ||
            heritage_clause.parent().kind() == SyntaxKind::InterfaceDeclaration
    )
}

pub fn is_identifier_type_reference(node: &Node) -> bool {
    is_type_reference_node(node) && is_identifier(&node.as_type_reference_node().type_name)
}

pub fn array_is_homogeneous<TItem, TComparer: FnMut(&TItem, &TItem) -> bool>(
    array: &[TItem],
    mut comparer: TComparer, /*= equateValues*/
) -> bool {
    if array.len() < 2 {
        return true;
    }
    let first = &array[0];
    for i in 1..array.len() {
        let target = &array[i];
        if !comparer(first, target) {
            return false;
        }
    }
    true
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
    if incremental
        && matches!(
            child.maybe_parent().as_ref(),
            Some(child_parent) if ptr::eq(&**child_parent, parent)
        )
    {
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
    Debug_.assert_is_defined(&node.maybe_parent(), None);
    let mut node = node.node_wrapper();
    loop {
        let parent = node.parent();
        if is_parenthesized_expression(&parent) {
            node = parent;
            continue;
        }
        if is_expression_statement(&parent)
            || is_void_expression(&parent)
            || is_for_statement(&parent) && {
                let parent_as_for_statement = parent.as_for_statement();
                matches!(
                    parent_as_for_statement.initializer.as_ref(),
                    Some(parent_initializer) if Rc::ptr_eq(
                        parent_initializer,
                        &node
                    )
                ) || matches!(
                    parent_as_for_statement.incrementor.as_ref(),
                    Some(parent_incrementor) if Rc::ptr_eq(
                        parent_incrementor,
                        &node
                    )
                )
            }
        {
            return true;
        }
        if is_comma_list_expression(&parent) {
            if !Rc::ptr_eq(&node, last(&parent.as_comma_list_expression().elements)) {
                return true;
            }
            node = parent;
            continue;
        }
        if is_binary_expression(&parent)
            && parent.as_binary_expression().operator_token.kind() == SyntaxKind::CommaToken
        {
            if Rc::ptr_eq(&node, &parent.as_binary_expression().left) {
                return true;
            }
            node = parent;
            continue;
        }
        return false;
    }
}

pub fn contains_ignored_path(path: &str) -> bool {
    some(
        Some(&**ignored_paths),
        Some(|p: &&str| string_contains(path, *p)),
    )
}

pub fn get_containing_node_array(node: &Node) -> Option<NodeArray> {
    unimplemented!()
}

pub fn has_context_sensitive_parameters(node: &Node /*FunctionLikeDeclaration*/) -> bool {
    let node_as_function_like_declaration = node.as_function_like_declaration();
    if node_as_function_like_declaration
        .maybe_type_parameters()
        .is_none()
    {
        if some(
            Some(&**node_as_function_like_declaration.parameters()),
            Some(|p: &Rc<Node>| get_effective_type_annotation_node(p).is_none()),
        ) {
            return true;
        }
        if node.kind() != SyntaxKind::ArrowFunction {
            let parameter = first_or_undefined(node_as_function_like_declaration.parameters());
            if !matches!(
                parameter,
                Some(parameter) if parameter_is_this_keyword(parameter)
            ) {
                return true;
            }
        }
    }
    false
}

pub fn is_infinity_or_nan_string(name: &str) -> bool {
    matches!(name, "Infinity" | "-Infinity" | "NaN")
}

pub fn is_catch_clause_variable_declaration(node: &Node) -> bool {
    node.kind() == SyntaxKind::VariableDeclaration
        && node.parent().kind() == SyntaxKind::CatchClause
}

pub fn is_parameter_or_catch_clause_variable(symbol: &Symbol) -> bool {
    let declaration = symbol
        .maybe_value_declaration()
        .as_ref()
        .map(|symbol_value_declaration| get_root_declaration(symbol_value_declaration));
    matches!(
        declaration.as_ref(),
        Some(declaration) if is_parameter(declaration) ||
            is_catch_clause_variable_declaration(declaration)
    )
}

pub fn is_function_expression_or_arrow_function(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction
    )
}
