#![allow(non_upper_case_globals)]

use std::ptr;
use std::rc::Rc;

use crate::{
    add_range, filter, first_or_undefined, get_assignment_declaration_kind,
    get_jsdoc_parameter_tags, get_jsdoc_parameter_tags_no_cache, get_jsdoc_type_parameter_tags,
    get_jsdoc_type_parameter_tags_no_cache, has_initializer, has_jsdoc_nodes, is_binary_expression,
    is_expression_statement, is_jsdoc, is_jsdoc_type_tag, is_module_declaration,
    is_parenthesized_expression, is_type_alias_declaration, is_variable_like,
    is_variable_statement, last, skip_outer_expressions, AssignmentDeclarationKind, Node,
    NodeInterface, OuterExpressionKinds, SyntaxKind,
};

pub fn try_get_import_from_module_specifier(
    node: &Node, /*StringLiteralLike*/
) -> Option<Rc<Node /*AnyValidImportOrReExport*/>> {
    unimplemented!()
}

pub fn is_jsdoc_type_alias(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::JSDocTypedefTag | SyntaxKind::JSDocCallbackTag | SyntaxKind::JSDocEnumTag
    )
}

pub fn is_type_alias(node: &Node) -> bool {
    is_jsdoc_type_alias(node) || is_type_alias_declaration(node)
}

fn get_source_of_defaulted_assignment(node: &Node) -> Option<Rc<Node>> {
    if !is_expression_statement(node) {
        return None;
    }
    let node_as_expression_statement = node.as_expression_statement();
    if !is_binary_expression(&*node_as_expression_statement.expression) {
        return None;
    }
    if !(get_assignment_declaration_kind(&*node_as_expression_statement.expression)
        != AssignmentDeclarationKind::None)
    {
        return None;
    }
    let node_expression_as_binary_expression = node_as_expression_statement
        .expression
        .as_binary_expression();
    if !is_binary_expression(&*node_expression_as_binary_expression.right) {
        return None;
    }
    let node_expression_right_as_binary_expression = node_expression_as_binary_expression
        .right
        .as_binary_expression();
    if matches!(
        node_expression_right_as_binary_expression
            .operator_token
            .kind(),
        SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken
    ) {
        Some(node_expression_right_as_binary_expression.right.clone())
    } else {
        None
    }
}

pub fn get_single_initializer_of_variable_statement_or_property_declaration(
    node: &Node,
) -> Option<Rc<Node /*Expression*/>> {
    match node.kind() {
        SyntaxKind::VariableStatement => {
            let v = get_single_variable_of_variable_statement(node);
            v.and_then(|v| v.as_has_initializer().maybe_initializer())
        }
        SyntaxKind::PropertyDeclaration | SyntaxKind::PropertyAssignment => {
            node.as_has_initializer().maybe_initializer()
        }
        _ => None,
    }
}

pub fn get_single_variable_of_variable_statement(
    node: &Node,
) -> Option<Rc<Node /*VariableDeclaration*/>> {
    if is_variable_statement(node) {
        first_or_undefined(
            &node
                .as_variable_statement()
                .declaration_list
                .as_variable_declaration_list()
                .declarations,
        )
        .map(Clone::clone)
    } else {
        None
    }
}

fn get_nested_module_declaration(node: &Node) -> Option<Rc<Node>> {
    if is_module_declaration(node)
        && matches!(node.as_module_declaration().body.as_ref(), Some(body) if body.kind() == SyntaxKind::ModuleDeclaration)
    {
        node.as_module_declaration().body.clone()
    } else {
        None
    }
}

pub fn get_jsdoc_comments_and_tags(
    host_node: &Node,
    no_cache: Option<bool>,
) -> Vec<Rc<Node /*JSDoc | JSDocTag*/>> {
    let no_cache = no_cache.unwrap_or(false);
    let mut result: Option<Vec<Rc<Node>>> = None;
    if is_variable_like(host_node)
        && has_initializer(host_node)
        && has_jsdoc_nodes(&*host_node.as_has_initializer().maybe_initializer().unwrap())
    {
        if result.is_none() {
            result = Some(vec![]);
        }
        /*result =*/
        add_range(
            result.as_mut().unwrap(),
            filter_owned_jsdoc_tags(
                host_node,
                &last(
                    host_node
                        .as_has_initializer()
                        .maybe_initializer()
                        .unwrap()
                        .maybe_js_doc()
                        .as_deref()
                        .unwrap(),
                ),
            )
            .as_deref(),
            None,
            None,
        );
    }

    let mut node: Option<Rc<Node>> = Some(host_node.node_wrapper());
    while matches!(node.as_ref(), Some(node) if node.maybe_parent().is_some()) {
        let node_present = node.clone().unwrap();
        if has_jsdoc_nodes(&*node_present) {
            if result.is_none() {
                result = Some(vec![]);
            }
            /*result = */
            add_range(
                result.as_mut().unwrap(),
                filter_owned_jsdoc_tags(
                    host_node,
                    &last(node_present.maybe_js_doc().as_deref().unwrap()),
                )
                .as_deref(),
                None,
                None,
            );
        }

        if node_present.kind() == SyntaxKind::Parameter {
            if result.is_none() {
                result = Some(vec![]);
            }
            /*result = */
            add_range(
                result.as_mut().unwrap(),
                Some(&(if no_cache {
                    get_jsdoc_parameter_tags_no_cache
                } else {
                    get_jsdoc_parameter_tags
                })(&node_present)),
                None,
                None,
            );
        }
        if node_present.kind() == SyntaxKind::TypeParameter {
            if result.is_none() {
                result = Some(vec![]);
            }
            /*result = */
            add_range(
                result.as_mut().unwrap(),
                Some(&(if no_cache {
                    get_jsdoc_type_parameter_tags_no_cache
                } else {
                    get_jsdoc_type_parameter_tags
                })(&node_present)),
                None,
                None,
            );
        }
        node = get_next_jsdoc_comment_location(&node_present);
    }
    result.unwrap_or(vec![])
}

fn filter_owned_jsdoc_tags(
    host_node: &Node,
    js_doc: &Node, /*JSDoc | JSDocTag*/
) -> Option<Vec<Rc<Node /*JSDoc | JSDocTag*/>>> {
    if is_jsdoc(js_doc) {
        let owned_tags = filter(js_doc.as_jsdoc().tags.as_deref(), |tag| {
            owns_jsdoc_tag(host_node, tag)
        });
        return if match (js_doc.as_jsdoc().tags.as_ref(), owned_tags.as_ref()) {
            (Some(js_doc_tags), Some(owned_tags)) if js_doc_tags.len() == owned_tags.len() => true,
            (None, None) => true,
            _ => false,
        } {
            Some(vec![js_doc.node_wrapper()])
        } else {
            owned_tags
        };
    }
    if owns_jsdoc_tag(host_node, js_doc) {
        Some(vec![js_doc.node_wrapper()])
    } else {
        None
    }
}

fn owns_jsdoc_tag(host_node: &Node, tag: &Node /*JSDocTag*/) -> bool {
    !is_jsdoc_type_tag(tag)
        || tag.maybe_parent().is_none()
        || !is_jsdoc(&*tag.parent())
        || !matches!(tag.parent().maybe_parent(), Some(grandparent) if is_parenthesized_expression(&*grandparent))
        || matches!(tag.parent().maybe_parent(), Some(grandparent) if ptr::eq(&*grandparent, host_node))
}

pub fn get_next_jsdoc_comment_location(node: &Node) -> Option<Rc<Node>> {
    let parent = node.maybe_parent();
    if matches!(
        parent.as_ref(),
        Some(parent) if parent.kind() == SyntaxKind::PropertyAssignment
            || parent.kind() == SyntaxKind::ExportAssignment
            || parent.kind() == SyntaxKind::PropertyDeclaration
            || parent.kind() == SyntaxKind::ExpressionStatement
                && node.kind() == SyntaxKind::PropertyAccessExpression
            || parent.kind() == SyntaxKind::ReturnStatement
            || get_nested_module_declaration(node).is_some()
            || is_binary_expression(node)
                && node.as_binary_expression().operator_token.kind() == SyntaxKind::EqualsToken
    ) {
        return parent;
    }
    if parent.is_none() {
        return None;
    }
    let parent = parent.unwrap();
    let grandparent = parent.maybe_parent();
    if matches!(
        grandparent.as_ref(),
        Some(grandparent) if matches!(
            get_single_variable_of_variable_statement(&grandparent),
            Some(single_variable) if ptr::eq(
                &*single_variable,
                node,
            )
        ) || is_binary_expression(&*parent)
            && parent.as_binary_expression().operator_token.kind() == SyntaxKind::EqualsToken
    ) {
        return grandparent;
    }
    if grandparent.is_none() {
        return None;
    }
    let grandparent = grandparent.unwrap();
    let great_grandparent = grandparent.maybe_parent();
    if matches!(
        great_grandparent.as_ref(),
        Some(great_grandparent) if get_single_variable_of_variable_statement(&great_grandparent).is_some()
            || matches!(
                get_single_initializer_of_variable_statement_or_property_declaration(&great_grandparent),
                Some(single_initializer) if ptr::eq(
                    &*single_initializer,
                    node,
                )
            )
            || get_source_of_defaulted_assignment(&great_grandparent).is_some()
    ) {
        return great_grandparent;
    }
    None
}

fn walk_up(node: &Node, kind: SyntaxKind) -> Option<Rc<Node>> {
    let mut node = Some(node.node_wrapper());
    loop {
        if let Some(node_present) = node.as_ref() {
            if node_present.kind() == kind {
                node = node_present.maybe_parent();
            } else {
                break;
            }
        } else {
            break;
        }
    }
    node
}

pub fn walk_up_parenthesized_expressions(node: &Node) -> Option<Rc<Node>> {
    walk_up(node, SyntaxKind::ParenthesizedExpression)
}

pub fn skip_parentheses(node: &Node, exclude_jsdoc_type_assertions: Option<bool>) -> Rc<Node> {
    let exclude_jsdoc_type_assertions = exclude_jsdoc_type_assertions.unwrap_or(false);
    let flags = if exclude_jsdoc_type_assertions {
        OuterExpressionKinds::Parentheses | OuterExpressionKinds::ExcludeJSDocTypeAssertion
    } else {
        OuterExpressionKinds::Parentheses
    };
    skip_outer_expressions(node, Some(flags))
}
