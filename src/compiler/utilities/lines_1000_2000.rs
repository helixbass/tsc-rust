#![allow(non_upper_case_globals)]

use regex::Regex;
use std::convert::TryInto;
use std::rc::Rc;

use crate::{
    concatenate, create_file_diagnostic, create_text_span_from_bounds, filter, find_ancestor,
    get_leading_comment_ranges, get_source_file_of_node, get_trailing_comment_ranges,
    is_function_like, is_function_like_or_class_static_block_declaration, maybe_text_char_at_index,
    BaseDiagnostic, BaseDiagnosticRelatedInformation, CharacterCodes, CommentRange,
    DiagnosticMessage, DiagnosticMessageChain, DiagnosticRelatedInformation,
    DiagnosticWithLocation, Node, NodeInterface, ReadonlyTextRange, SourceTextAsChars, SyntaxKind,
    TextRange, TextSpan,
};

pub fn create_diagnostic_for_node(
    node: &Node,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticWithLocation {
    let source_file = get_source_file_of_node(Some(node)).unwrap();
    create_diagnostic_for_node_in_source_file(&source_file, node, message, args)
}

fn create_diagnostic_for_node_in_source_file(
    source_file: &Node, /*SourceFile*/
    node: &Node,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticWithLocation {
    let span = get_error_span_for_node(source_file, node);
    create_file_diagnostic(source_file, span.start, span.length, message, args)
}

pub fn create_diagnostic_for_node_from_message_chain(
    node: &Node,
    message_chain: DiagnosticMessageChain,
    related_information: Option<Vec<Rc<DiagnosticRelatedInformation>>>,
) -> DiagnosticWithLocation {
    let source_file = get_source_file_of_node(Some(node)).unwrap();
    let span = get_error_span_for_node(&source_file, node);
    create_file_diagnostic_from_message_chain(
        &source_file,
        span.start,
        span.length,
        message_chain,
        related_information,
    )
}

fn create_file_diagnostic_from_message_chain(
    file: &Node, /*SourceFile*/
    start: isize,
    length: isize,
    message_chain: DiagnosticMessageChain,
    related_information: Option<Vec<Rc<DiagnosticRelatedInformation>>>,
) -> DiagnosticWithLocation {
    // assert_diagnostic_location(&*file, start, length);
    DiagnosticWithLocation::new(BaseDiagnostic::new(
        BaseDiagnosticRelatedInformation::new(
            message_chain.code,
            Some(file.node_wrapper()),
            Some(start),
            Some(length),
            message_chain,
        ),
        related_information,
    ))
}

fn get_error_span_for_node(source_file: &Node /*SourceFile*/, node: &Node) -> TextSpan {
    let error_node = node;

    let pos = error_node.pos();

    create_text_span_from_bounds(pos, error_node.end())
}

pub fn is_external_or_common_js_module(file: &Node /*SourceFile*/) -> bool {
    false
}

pub fn is_import_call(n: &Node) -> bool {
    match n {
        Node::CallExpression(call_expression) => {
            call_expression.expression.kind() == SyntaxKind::ImportKeyword
        }
        _ => false,
    }
}

pub fn is_prologue_directive(node: &Node) -> bool {
    node.kind() == SyntaxKind::ExpressionStatement
        && node.as_expression_statement().expression.kind() == SyntaxKind::StringLiteral
}

pub fn is_object_literal_method(node: &Node) -> bool {
    /*node &&*/
    node.kind() == SyntaxKind::MethodDeclaration
        && node.parent().kind() == SyntaxKind::ObjectLiteralExpression
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

pub fn get_jsdoc_comment_ranges<TNode: NodeInterface>(
    node: &TNode,
    text: &SourceTextAsChars,
) -> Option<Vec<CommentRange>> {
    let comment_ranges = if matches!(
        node.kind(),
        SyntaxKind::Parameter
            | SyntaxKind::TypeParameter
            | SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction
            | SyntaxKind::ParenthesizedExpression
            | SyntaxKind::VariableDeclaration
    ) {
        Some(concatenate(
            // TODO: should get_trailing_comment_ranges()/get_leading_comment_ranges() accept isize instead?
            get_trailing_comment_ranges(text, node.pos().try_into().unwrap())
                .unwrap_or_else(|| vec![]),
            get_leading_comment_ranges(text, node.pos().try_into().unwrap())
                .unwrap_or_else(|| vec![]),
        ))
    } else {
        get_leading_comment_ranges(text, node.pos().try_into().unwrap())
    };
    filter(comment_ranges.as_deref(), |comment| {
        matches!(maybe_text_char_at_index(text, (comment.pos() + 1).try_into().unwrap()), Some(ch) if ch == CharacterCodes::asterisk)
            && matches!(maybe_text_char_at_index(text, (comment.pos() + 2).try_into().unwrap()), Some(ch) if ch == CharacterCodes::asterisk)
            && match maybe_text_char_at_index(text, (comment.pos() + 3).try_into().unwrap()) {
                None => true,
                Some(ch) if ch != CharacterCodes::slash => true,
                _ => false,
            }
    })
}

lazy_static! {
    pub static ref full_triple_slash_reference_path_reg_ex: Regex =
        Regex::new(r#"^(///\s*<reference\s+path\s*=\s*)(('[^']*')|("[^"]*")).*?/>"#).unwrap();
}

lazy_static! {
    pub(super) static ref full_triple_slash_reference_type_reference_directive_reg_ex: Regex =
        Regex::new(r#"^(///\s*<reference\s+types\s*=\s*)(('[^']*')|("[^"]*")).*?/>"#).unwrap();
}

lazy_static! {
    pub static ref full_triple_slash_amd_reference_path_reg_ex: Regex =
        Regex::new(r#"^(///\s*<amd-dependency\s+path\s*=\s*)(('[^']*')|("[^"]*")).*?/>"#).unwrap();
}

lazy_static! {
    pub(super) static ref default_lib_reference_reg_ex: Regex =
        Regex::new(r#"(///\s*<reference\s+no-default-lib\s*=\s*)(('[^']*')|("[^"]*"))\s*/>"#)
            .unwrap();
}

pub fn is_variable_like(node: &Node) -> bool {
    /* if node {*/
    match node.kind() {
        SyntaxKind::BindingElement
        | SyntaxKind::EnumMember
        | SyntaxKind::Parameter
        | SyntaxKind::PropertyAssignment
        | SyntaxKind::PropertyDeclaration
        | SyntaxKind::PropertySignature
        | SyntaxKind::ShorthandPropertyAssignment
        | SyntaxKind::VariableDeclaration => true,
        _ => false,
    }
    /*}*/
}

pub fn is_function_block(node: &Node) -> bool {
    /*node &&*/
    node.kind() == SyntaxKind::Block && is_function_like(node.maybe_parent())
}
