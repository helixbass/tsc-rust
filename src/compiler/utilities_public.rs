use std::rc::Rc;

use crate::{CharacterCodes, Node, NodeInterface, SyntaxKind, TextSpan, __String};

fn create_text_span(start: usize, length: usize) -> TextSpan {
    TextSpan { start, length }
}

pub fn create_text_span_from_bounds(start: usize, end: usize) -> TextSpan {
    create_text_span(start, end - start)
}

pub fn escape_leading_underscores(identifier: &str) -> __String {
    __String::new(
        if identifier.chars().count() >= 2
            && identifier.chars().nth(0).unwrap() == CharacterCodes::underscore
            && identifier.chars().nth(1).unwrap() == CharacterCodes::underscore
        {
            format!("_{}", identifier)
        } else {
            identifier.to_string()
        },
    )
}

fn get_non_assigned_name_of_declaration(declaration: Rc<Node>) -> Option<Rc<Node>> {
    Some(declaration.as_named_declaration().name())
}

pub fn get_name_of_declaration(declaration: Rc<Node>) -> Option<Rc<Node>> {
    get_non_assigned_name_of_declaration(declaration)
}

pub fn is_member_name<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::Identifier || node.kind() == SyntaxKind::PrivateIdentifier
}

pub fn is_binding_pattern<TNode: NodeInterface>(node: &TNode) -> bool {
    if true {
        let kind = node.kind();
        return kind == SyntaxKind::ArrayBindingPattern || kind == SyntaxKind::ObjectBindingPattern;
    }

    false
}
