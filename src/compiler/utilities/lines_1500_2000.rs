#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use crate::{
    find, find_ancestor, first_defined, get_text_of_property_name, is_array_literal_expression,
    is_class_like, is_class_static_block_declaration, is_function_like,
    is_function_like_declaration, is_function_like_or_class_static_block_declaration,
    is_object_literal_expression, is_string_literal, try_cast, FindAncestorCallbackReturn,
    HasInitializerInterface, LiteralLikeNodeInterface, NamedDeclarationInterface, Node,
    NodeInterface, SyntaxKind, TypePredicate, TypePredicateKind,
};

pub fn introduces_arguments_exotic_object(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::MethodDeclaration
            | SyntaxKind::MethodSignature
            | SyntaxKind::Constructor
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::FunctionExpression
    )
}

pub fn unwrap_innermost_statement_of_label<TBeforeUnwrapLabelCallback: FnMut(&Node)>(
    node: &Node, /*LabeledStatement*/
    mut before_unwrap_label_callback: Option<TBeforeUnwrapLabelCallback>,
) -> Rc<Node /*Statement*/> {
    let mut node = node.node_wrapper();
    loop {
        if let Some(before_unwrap_label_callback) = before_unwrap_label_callback.as_mut() {
            before_unwrap_label_callback(&node);
        }
        let node_as_labeled_statement = node.as_labeled_statement();
        if node_as_labeled_statement.statement.kind() != SyntaxKind::LabeledStatement {
            return node_as_labeled_statement.statement.clone();
        }
        node = node_as_labeled_statement.statement.clone();
    }
}

pub fn is_function_block(node: &Node) -> bool {
    /*node &&*/
    node.kind() == SyntaxKind::Block && is_function_like(node.maybe_parent())
}

pub fn is_object_literal_method(node: &Node) -> bool {
    /*node &&*/
    node.kind() == SyntaxKind::MethodDeclaration
        && node.parent().kind() == SyntaxKind::ObjectLiteralExpression
}

pub fn is_object_literal_or_class_expression_method_or_accessor(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::MethodDeclaration | SyntaxKind::GetAccessor | SyntaxKind::SetAccessor
    ) && matches!(
        node.parent().kind(),
        SyntaxKind::ObjectLiteralExpression | SyntaxKind::ClassExpression
    )
}

pub fn is_identifier_type_predicate(predicate: &TypePredicate) -> bool {
    /*predicate &&*/
    predicate.kind == TypePredicateKind::Identifier
}

pub fn is_this_type_predicate(predicate: &TypePredicate) -> bool {
    /*predicate &&*/
    predicate.kind == TypePredicateKind::This
}

pub fn get_property_assignment(
    object_literal: &Node, /*ObjectLiteralExpression*/
    key: &str,
    key2: Option<&str>,
) -> Vec<Rc<Node /*PropertyAssignment*/>> {
    object_literal
        .as_object_literal_expression()
        .properties
        .iter()
        .filter(|property| {
            if property.kind() == SyntaxKind::PropertyAssignment {
                let prop_name =
                    get_text_of_property_name(&property.as_property_assignment().name());
                return prop_name.eq_str(key)
                    || matches!(key2, Some(key2) if prop_name.eq_str(key2));
            }
            false
        })
        .map(Clone::clone)
        .collect()
}

pub fn get_property_array_element_value(
    object_literal: &Node, /*ObjectLiteralExpression*/
    prop_key: &str,
    element_value: &str,
) -> Option<Rc<Node /*StringLiteral*/>> {
    first_defined(
        &get_property_assignment(object_literal, prop_key, None),
        |property, _| {
            let property_as_property_assignment = property.as_property_assignment();
            if is_array_literal_expression(
                &property_as_property_assignment.maybe_initializer().unwrap(),
            ) {
                find(
                    &property_as_property_assignment
                        .maybe_initializer()
                        .unwrap()
                        .as_array_literal_expression()
                        .elements,
                    |element, _| {
                        is_string_literal(element)
                            && &*element.as_string_literal().text() == element_value
                    },
                )
                .map(Clone::clone)
            } else {
                None
            }
        },
    )
}

pub fn get_ts_config_object_literal_expression<TTsConfigSourceFile: Borrow<Node>>(
    ts_config_source_file: Option<TTsConfigSourceFile /*TsConfigSourceFile*/>,
) -> Option<Rc<Node /*ObjectLiteralExpression*/>> {
    if ts_config_source_file.is_none() {
        return None;
    }
    let ts_config_source_file = ts_config_source_file.unwrap();
    let ts_config_source_file = ts_config_source_file.borrow();
    let ts_config_source_file_as_source_file = ts_config_source_file.as_source_file();
    if !ts_config_source_file_as_source_file.statements.is_empty() {
        let expression = &ts_config_source_file_as_source_file.statements[0]
            .as_expression_statement()
            .expression;
        return try_cast(expression, |expression| {
            is_object_literal_expression(expression)
        })
        .map(|expression| expression.node_wrapper());
    }
    None
}

pub fn get_ts_config_prop_array_element_value<TTsConfigSourceFile: Borrow<Node>>(
    ts_config_source_file: Option<TTsConfigSourceFile /*TsConfigSourceFile*/>,
    prop_key: &str,
    element_value: &str,
) -> Option<Rc<Node /*StringLiteral*/>> {
    first_defined(
        &get_ts_config_prop_array(ts_config_source_file, prop_key),
        |property, _| {
            let property_as_property_assignment = property.as_property_assignment();
            if is_array_literal_expression(
                &property_as_property_assignment.maybe_initializer().unwrap(),
            ) {
                find(
                    &property_as_property_assignment
                        .maybe_initializer()
                        .unwrap()
                        .as_array_literal_expression()
                        .elements,
                    |element, _| {
                        is_string_literal(element)
                            && &*element.as_string_literal().text() == element_value
                    },
                )
                .map(Clone::clone)
            } else {
                None
            }
        },
    )
}

pub fn get_ts_config_prop_array<TTsConfigSourceFile: Borrow<Node>>(
    ts_config_source_file: Option<TTsConfigSourceFile /*TsConfigSourceFile*/>,
    prop_key: &str,
) -> Vec<Rc<Node /*PropertyAssignment*/>> {
    let json_object_literal = get_ts_config_object_literal_expression(ts_config_source_file);
    match json_object_literal {
        Some(json_object_literal) => get_property_assignment(&json_object_literal, prop_key, None),
        None => vec![],
    }
}

pub fn get_containing_function(node: &Node) -> Option<Rc<Node /*SignatureDeclaration*/>> {
    find_ancestor(node.maybe_parent(), |node: &Node| {
        is_function_like(Some(node))
    })
}

pub fn get_containing_function_declaration(
    node: &Node,
) -> Option<Rc<Node /*FunctionLikeDeclaration*/>> {
    find_ancestor(node.maybe_parent(), |node: &Node| {
        is_function_like_declaration(node)
    })
}

pub fn get_containing_class(node: &Node) -> Option<Rc<Node /*ClassLikeDeclaration*/>> {
    find_ancestor(node.maybe_parent(), |node: &Node| is_class_like(node))
}

pub fn get_containing_class_static_block(node: &Node) -> Option<Rc<Node>> {
    find_ancestor(node.maybe_parent(), |n: &Node| {
        if is_class_like(n) || is_function_like(Some(n)) {
            return FindAncestorCallbackReturn::Quit;
        }
        is_class_static_block_declaration(n).into()
    })
}

pub fn get_containing_function_or_class_static_block(
    node: &Node,
) -> Option<Rc<Node /*SignatureDeclaration | ClassStaticBlockDeclaration*/>> {
    find_ancestor(node.maybe_parent(), |node: &Node| {
        is_function_like_or_class_static_block_declaration(Some(node))
    })
}

pub fn is_super_property(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression
    ) && node.as_has_expression().expression().kind() == SyntaxKind::SuperKeyword
}
