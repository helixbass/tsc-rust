use std::borrow::Borrow;
use std::rc::Rc;

use crate::{
    is_class_static_block_declaration, CharacterCodes, Node, NodeFlags, NodeInterface, SyntaxKind,
    TextSpan, __String, compare_diagnostics, is_block, is_module_block, is_source_file,
    sort_and_deduplicate, Diagnostic, SortedArray,
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

pub enum FindAncestorCallbackReturn {
    Bool(bool),
    Quit,
}

impl From<bool> for FindAncestorCallbackReturn {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

pub fn find_ancestor<
    TNodeRef: Borrow<Node>,
    TCallbackReturn: Into<FindAncestorCallbackReturn>,
    TCallback: FnMut(&Node) -> TCallbackReturn,
>(
    node: Option<TNodeRef>,
    callback: TCallback,
) -> Option<Rc<Node>> {
    let mut node = node.map(|node| node.borrow().node_wrapper());
    while let Some(rc_node_ref) = node.as_ref() {
        let result = callback(&**rc_node_ref).into();
        match result {
            FindAncestorCallbackReturn::Quit => {
                return None;
            }
            FindAncestorCallbackReturn::Bool(result) if result => {
                return node;
            }
            _ => (),
        }
        node = rc_node_ref.maybe_parent();
    }
    None
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

fn skip_partially_emitted_expressions<TNode: NodeInterface>(node: &TNode) -> Rc<Node> {
    node.node_wrapper()
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

pub fn is_function_like_or_class_static_block_declaration<TNodeRef: Borrow<Node>>(
    node: Option<TNodeRef>,
) -> bool {
    match node {
        Some(node) => {
            let node = node.borrow();
            is_function_like_kind(node.kind()) || is_class_static_block_declaration(node)
        }
        None => false,
    }
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

pub fn is_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    is_expression_kind(skip_partially_emitted_expressions(node).kind())
}

fn is_expression_kind(kind: SyntaxKind) -> bool {
    match kind {
        _ => is_unary_expression_kind(kind),
    }
}

pub fn has_initializer<TNode: NodeInterface>(node: &TNode) -> bool {
    node.node_wrapper()
        .maybe_as_has_expression_initializer()
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
