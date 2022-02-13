#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use regex::{Captures, Regex};
use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::cmp;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use crate::{
    add_range, compare_strings_case_sensitive_maybe, compute_line_starts, concatenate,
    create_file_diagnostic, filter, find_ancestor, first_or_undefined, flat_map,
    for_each_child_bool, get_jsdoc_parameter_tags, get_jsdoc_parameter_tags_no_cache,
    get_jsdoc_tags, get_jsdoc_type_parameter_tags, get_jsdoc_type_parameter_tags_no_cache,
    get_leading_comment_ranges, get_source_file_of_node, get_trailing_comment_ranges,
    has_initializer, has_jsdoc_nodes, id_text, is_binary_expression, is_call_expression,
    is_element_access_expression, is_export_declaration, is_expression_statement, is_function_like,
    is_function_like_or_class_static_block_declaration, is_identifier, is_jsdoc,
    is_jsdoc_signature, is_jsdoc_template_tag, is_jsdoc_type_tag, is_left_hand_side_expression,
    is_module_declaration, is_no_substituion_template_literal, is_numeric_literal,
    is_object_literal_expression, is_parenthesized_expression, is_private_identifier,
    is_property_access_expression, is_source_file, is_string_literal_like, is_variable_statement,
    is_void_expression, is_white_space_like, last, length, maybe_text_char_at_index,
    module_resolution_option_declarations, options_affecting_program_structure,
    skip_outer_expressions, some, str_to_source_text_as_chars, text_substring,
    AssignmentDeclarationKind, CommandLineOption, CommandLineOptionInterface, CommentRange,
    CompilerOptions, CompilerOptionsValue, DiagnosticWithDetachedLocation, DiagnosticWithLocation,
    EmitFlags, EmitTextWriter, Extension, LanguageVariant, LiteralLikeNodeInterface, MapLike,
    ModifierFlags, ModuleKind, Node, NodeArray, NodeFlags, NodeInterface, ObjectFlags,
    OuterExpressionKinds, PrefixUnaryExpression, PseudoBigInt, ReadonlyTextRange, ScriptKind,
    ScriptTarget, Signature, SignatureFlags, SortedArray, SourceFileLike, SourceTextAsChars,
    Symbol, SymbolFlags, SymbolInterface, SymbolTable, SymbolTracker, SymbolWriter, SyntaxKind,
    TextRange, TextSpan, TokenFlags, TransformFlags, TransientSymbolInterface, Type, TypeFlags,
    TypeInterface, UnderscoreEscapedMap, __String, compare_strings_case_sensitive, compare_values,
    create_text_span_from_bounds, escape_leading_underscores, for_each, get_combined_node_flags,
    get_name_of_declaration, insert_sorted, is_big_int_literal, is_member_name,
    is_type_alias_declaration, skip_trivia, BaseDiagnostic, BaseDiagnosticRelatedInformation,
    BaseNode, BaseSymbol, BaseType, CharacterCodes, CheckFlags, Comparison, Debug_, Diagnostic,
    DiagnosticCollection, DiagnosticInterface, DiagnosticMessage, DiagnosticMessageChain,
    DiagnosticMessageText, DiagnosticRelatedInformation, DiagnosticRelatedInformationInterface,
};
use local_macros::enum_unwrapped;

pub fn create_diagnostic_for_node(
    node: &Node,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticWithLocation {
    let source_file = get_source_file_of_node(node);
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
    let source_file = get_source_file_of_node(node);
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
