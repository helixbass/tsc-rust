use std::borrow::Borrow;
use std::rc::Rc;

use crate::{
    find, is_jsdoc, is_jsdoc_type_tag, skip_outer_expressions, CharacterCodes, Debug_, Node,
    NodeFlags, NodeInterface, OuterExpressionKinds, SyntaxKind, TextSpan, __String,
    compare_diagnostics, is_block, is_module_block, is_source_file, sort_and_deduplicate,
    Diagnostic, SortedArray,
};

pub fn sort_and_deduplicate_diagnostics(
    diagnostics: &[Rc<Diagnostic>],
) -> SortedArray<Rc<Diagnostic>> {
    sort_and_deduplicate(
        diagnostics,
        &|a, b| compare_diagnostics(&**a, &**b),
        Option::<&fn(&Rc<Diagnostic>, &Rc<Diagnostic>) -> bool>::None,
    )
}

fn create_text_span(start: isize, length: isize) -> TextSpan {
    TextSpan { start, length }
}

pub fn create_text_span_from_bounds(start: isize, end: isize) -> TextSpan {
    create_text_span(start, end - start)
}

fn get_combined_flags<TNode: NodeInterface, TCallback: FnMut(&Node) -> NodeFlags>(
    node: &TNode,
    mut get_flags: TCallback,
) -> NodeFlags {
    let mut node = Some(node.node_wrapper());
    let mut flags = get_flags(node.as_ref().unwrap());
    if node.as_ref().unwrap().kind() == SyntaxKind::VariableDeclaration {
        node = node.as_ref().unwrap().maybe_parent();
    }
    if let Some(node_present) = node.as_ref() {
        if node_present.kind() == SyntaxKind::VariableDeclarationList {
            flags |= get_flags(node_present);
            node = node_present.maybe_parent();
        }
    }
    if let Some(node) = node {
        if node.kind() == SyntaxKind::VariableStatement {
            flags |= get_flags(&*node);
        }
    }
    flags
}

pub fn get_combined_node_flags<TNode: NodeInterface>(node: &TNode) -> NodeFlags {
    get_combined_flags(node, |n| n.flags())
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

pub fn unescape_leading_underscores(identifier: &__String) -> String {
    let mut chars = identifier.chars();
    if
    /*chars.count() >= 3 &&*/
    matches!(chars.next(), Some(CharacterCodes::underscore))
        && matches!(chars.next(), Some(CharacterCodes::underscore))
        && matches!(chars.next(), Some(CharacterCodes::underscore))
    {
        identifier.chars().skip(1).collect()
    } else {
        identifier.chars().collect()
    }
}

pub fn id_text<TNode: NodeInterface>(
    identifier_or_private_name: &TNode, /*Identifier | PrivateIdentifier*/
) -> String {
    unescape_leading_underscores(
        &identifier_or_private_name
            .node_wrapper()
            .as_identifier()
            .escaped_text,
    )
}

fn get_non_assigned_name_of_declaration<TNode: NodeInterface>(
    declaration: &TNode,
) -> Option<Rc<Node>> {
    Some(declaration.node_wrapper().as_named_declaration().name())
}

pub fn get_name_of_declaration<TNode: NodeInterface>(declaration: &TNode) -> Option<Rc<Node>> {
    get_non_assigned_name_of_declaration(declaration)
}

pub fn get_jsdoc_type_tag(node: &Node) -> Option<Rc<Node /*JSDocTypeTag*/>> {
    let tag = get_first_jsdoc_tag(node, is_jsdoc_type_tag, None);
    // if matches!(tag, Some(tag) if matches!(tag.as_jsdoc_type_tag().type_expression, Some(type_expression) /*if type_expression.type_.is_some()*/))
    if tag.is_some() {
        return tag;
    }
    None
}

fn get_jsdoc_tags_worker(node: &Node, no_cache: Option<bool>) -> Vec<Rc<Node /*JSDocTag*/>> {
    let mut tags: Option<Vec<Rc<Node>>> = node.maybe_js_doc_cache().clone();
    if tags.is_none() || no_cache.unwrap_or(false) {
        let comments = get_jsdoc_comments_and_tags(node, no_cache);
        Debug_.assert(comments.len() < 2 || comments[0] != comments[1], None);
        tags = Some(flat_map(comments, |j| {
            if is_jsdoc(j) {
                j.as_jsdoc().tags
            } else {
                vec![j]
            }
        }));
        if !no_cache.unwrap_or(false) {
            node.set_js_doc_cache(tags.clone().unwrap());
        }
    }
    tags.unwrap()
}

fn get_first_jsdoc_tag<TPredicate: FnMut(&Node /*JSDocTag*/) -> bool>(
    node: &Node,
    predicate: TPredicate,
    no_cache: Option<bool>,
) -> Option<Rc<Node>> {
    find(&get_jsdoc_tags_worker(node, no_cache), |element, _| {
        predicate(element)
    })
    .map(Clone::clone)
}

pub fn get_effective_type_parameter_declarations(
    node: &Node,
) -> Vec<Rc<Node /*TypeParameterDeclaration*/>> {
    if let Some(type_parameters) = node.as_has_type_parameters().maybe_type_parameters() {
        return type_parameters.into();
    }
    vec![]
}

pub fn is_member_name<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::Identifier || node.kind() == SyntaxKind::PrivateIdentifier
}

fn skip_partially_emitted_expressions(node: &Node) -> Rc<Node> {
    skip_outer_expressions(
        node,
        Some(OuterExpressionKinds::PartiallyEmittedExpressions),
    )
}

pub fn is_literal_kind(kind: SyntaxKind) -> bool {
    SyntaxKind::FirstLiteralToken <= kind && kind <= SyntaxKind::LastLiteralToken
}

pub fn is_template_literal_kind(kind: SyntaxKind) -> bool {
    SyntaxKind::FirstTemplateToken <= kind && kind <= SyntaxKind::LastTemplateToken
}

pub fn is_modifier_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::AbstractKeyword
            | SyntaxKind::AsyncKeyword
            | SyntaxKind::ConstKeyword
            | SyntaxKind::DeclareKeyword
            | SyntaxKind::DefaultKeyword
            | SyntaxKind::ExportKeyword
            | SyntaxKind::PublicKeyword
            | SyntaxKind::PrivateKeyword
            | SyntaxKind::ProtectedKeyword
            | SyntaxKind::ReadonlyKeyword
            | SyntaxKind::StaticKeyword
            | SyntaxKind::OverrideKeyword
    )
}

pub fn is_function_like<TNodeRef: Borrow<Node>>(node: Option<TNodeRef>) -> bool {
    node.map_or(false, |node| is_function_like_kind(node.borrow().kind()))
}

fn is_function_like_declaration_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::FunctionDeclaration
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::Constructor
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction
    )
}

fn is_function_like_kind(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::MethodSignature
        | SyntaxKind::CallSignature
        | SyntaxKind::JSDocSignature
        | SyntaxKind::ConstructSignature
        | SyntaxKind::IndexSignature
        | SyntaxKind::FunctionType
        | SyntaxKind::JSDocFunctionType
        | SyntaxKind::ConstructorType => true,
        _ => is_function_like_declaration_kind(kind),
    }
}

pub fn is_function_or_module_block<TNode: NodeInterface>(node: &TNode) -> bool {
    is_source_file(node)
        || is_module_block(node)
        || is_block(node) && is_function_like(node.maybe_parent())
}

pub fn is_binding_pattern<TNode: NodeInterface>(node: &TNode) -> bool {
    if true {
        let kind = node.kind();
        return kind == SyntaxKind::ArrayBindingPattern || kind == SyntaxKind::ObjectBindingPattern;
    }

    false
}

pub(crate) fn is_left_hand_side_expression(node: &Node) -> bool {
    is_left_hand_side_expression_kind(skip_partially_emitted_expressions(node).kind())
}

fn is_left_hand_side_expression_kind(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::ArrayLiteralExpression
        | SyntaxKind::ObjectLiteralExpression
        | SyntaxKind::Identifier
        | SyntaxKind::NumericLiteral
        | SyntaxKind::FalseKeyword
        | SyntaxKind::TrueKeyword => true,
        _ => false,
    }
}

fn is_unary_expression_kind(kind: SyntaxKind) -> bool {
    match kind {
        SyntaxKind::PrefixUnaryExpression => true,
        _ => is_left_hand_side_expression_kind(kind),
    }
}

pub fn is_expression(node: &Node) -> bool {
    is_expression_kind(skip_partially_emitted_expressions(node).kind())
}

fn is_expression_kind(kind: SyntaxKind) -> bool {
    match kind {
        _ => is_unary_expression_kind(kind),
    }
}

pub fn has_jsdoc_nodes<TNode: NodeInterface>(node: &TNode) -> bool {
    node.maybe_js_doc().map_or(false, |jsdoc| !jsdoc.is_empty())
}

pub fn has_initializer<TNode: NodeInterface>(node: &TNode) -> bool {
    node.node_wrapper()
        .maybe_as_has_initializer()
        .and_then(|node| node.maybe_initializer())
        .is_some()
}

pub fn has_only_expression_initializer<TNode: NodeInterface>(node: &TNode) -> bool {
    match node.kind() {
        SyntaxKind::VariableDeclaration
        | SyntaxKind::Parameter
        | SyntaxKind::BindingElement
        | SyntaxKind::PropertySignature
        | SyntaxKind::PropertyDeclaration
        | SyntaxKind::PropertyAssignment
        | SyntaxKind::EnumMember => true,
        _ => false,
    }
}

pub fn is_string_literal_like<TNode: NodeInterface>(node: &TNode) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::StringLiteral | SyntaxKind::NoSubstitutionTemplateLiteral
    )
}
