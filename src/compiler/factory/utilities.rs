use std::borrow::Borrow;
use std::rc::Rc;

use crate::{
    first_or_undefined, get_emit_flags, get_jsdoc_type_tag, is_assignment_expression,
    is_declaration_binding_element, is_identifier, is_in_js_file, is_object_literal_element_like,
    is_parenthesized_expression, is_prologue_directive, is_spread_element, is_string_literal,
    EmitFlags, HasInitializerInterface, LiteralLikeNodeInterface, NamedDeclarationInterface, Node,
    NodeInterface, OuterExpressionKinds, SyntaxKind,
};

pub fn is_local_name(node: &Node /*Identifier*/) -> bool {
    get_emit_flags(node).intersects(EmitFlags::LocalName)
}

fn is_use_strict_prologue(node: &Node /*ExpressionStatement*/) -> bool {
    let node_as_expression_statement = node.as_expression_statement();
    is_string_literal(&node_as_expression_statement.expression)
        && &*node_as_expression_statement
            .expression
            .as_string_literal()
            .text()
            == "use strict"
}

pub fn starts_with_use_strict(statements: &[Rc<Node>]) -> bool {
    let first_statement = first_or_undefined(statements);
    if first_statement.is_none() {
        return false;
    }
    let first_statement = first_statement.unwrap();
    is_prologue_directive(first_statement) && is_use_strict_prologue(first_statement)
}

pub fn is_comma_sequence(node: &Node /*Expression*/) -> bool {
    node.kind() == SyntaxKind::BinaryExpression
        && node.as_binary_expression().operator_token.kind() == SyntaxKind::CommaToken
        || node.kind() == SyntaxKind::CommaListExpression
}

pub fn is_jsdoc_type_assertion(node: &Node) -> bool {
    is_parenthesized_expression(node)
        && is_in_js_file(Some(node))
        && get_jsdoc_type_tag(node).is_some()
}

pub fn is_outer_expression(node: &Node, kinds: Option<OuterExpressionKinds>) -> bool {
    let kinds = kinds.unwrap_or(OuterExpressionKinds::All);
    match node.kind() {
        SyntaxKind::ParenthesizedExpression => {
            if kinds.intersects(OuterExpressionKinds::ExcludeJSDocTypeAssertion)
                && is_jsdoc_type_assertion(node)
            {
                return false;
            }
            kinds.intersects(OuterExpressionKinds::Parentheses)
        }
        SyntaxKind::TypeAssertionExpression | SyntaxKind::AsExpression => {
            kinds.intersects(OuterExpressionKinds::TypeAssertions)
        }
        SyntaxKind::NonNullExpression => kinds.intersects(OuterExpressionKinds::NonNullAssertions),
        SyntaxKind::PartiallyEmittedExpression => {
            kinds.intersects(OuterExpressionKinds::PartiallyEmittedExpressions)
        }
        _ => false,
    }
}

pub fn skip_outer_expressions(node: &Node, kinds: Option<OuterExpressionKinds>) -> Rc<Node> {
    let kinds = kinds.unwrap_or(OuterExpressionKinds::All);
    let mut node = node.node_wrapper();
    while is_outer_expression(&node, Some(kinds)) {
        node = node.as_has_expression().expression();
    }
    node
}

pub fn get_target_of_binding_or_assignment_element(
    binding_element: &Node, /*BindingOrAssignmentElement*/
) -> Option<Rc<Node /*BindingOrAssignmentElementTarget*/>> {
    if is_declaration_binding_element(binding_element) {
        return binding_element.as_named_declaration().maybe_name();
    }

    if is_object_literal_element_like(binding_element) {
        match binding_element.kind() {
            SyntaxKind::PropertyAssignment => {
                return get_target_of_binding_or_assignment_element(
                    &binding_element
                        .as_property_assignment()
                        .maybe_initializer()
                        .unwrap(),
                );
            }

            SyntaxKind::ShorthandPropertyAssignment => {
                return binding_element
                    .as_shorthand_property_assignment()
                    .maybe_name();
            }

            SyntaxKind::SpreadAssignment => {
                return get_target_of_binding_or_assignment_element(
                    &binding_element.as_spread_assignment().expression,
                );
            }
            _ => (),
        }

        return None;
    }

    if is_assignment_expression(binding_element, Some(true)) {
        return get_target_of_binding_or_assignment_element(
            &binding_element.as_binary_expression().left,
        );
    }

    if is_spread_element(binding_element) {
        return get_target_of_binding_or_assignment_element(
            &binding_element.as_spread_element().expression,
        );
    }

    Some(binding_element.node_wrapper())
}

pub fn get_elements_of_binding_or_assignment_pattern(
    name: &Node, /*BindingOrAssignmentPattern*/
) -> Vec<Rc<Node /*BindingOrAssignmentElement*/>> {
    match name.kind() {
        SyntaxKind::ObjectBindingPattern
        | SyntaxKind::ArrayBindingPattern
        | SyntaxKind::ArrayLiteralExpression => name.as_has_elements().elements().to_vec(),
        SyntaxKind::ObjectLiteralExpression => {
            name.as_object_literal_expression().properties.to_vec()
        }
        _ => panic!("Unexpected kind"),
    }
}

pub(crate) fn get_jsdoc_type_alias_name<TFullName: Borrow<Node>>(
    full_name: Option<TFullName /*JSDocNamespaceBody*/>,
) -> Option<Rc<Node /*Identifier*/>> {
    full_name.map(|full_name| {
        let full_name = full_name.borrow();
        let mut right_node = full_name.node_wrapper();
        loop {
            if is_identifier(&right_node)
                || right_node.as_jsdoc_namespace_declaration().body.is_none()
            {
                return if is_identifier(&right_node) {
                    right_node
                } else {
                    right_node.as_jsdoc_namespace_declaration().name.clone()
                };
            }
            right_node = right_node
                .as_jsdoc_namespace_declaration()
                .body
                .clone()
                .unwrap();
        }
    })
}
