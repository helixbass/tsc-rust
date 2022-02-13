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
    add_range, compare_strings_case_sensitive_maybe, compute_line_starts, concatenate, filter,
    find_ancestor, first_or_undefined, flat_map, for_each_child_bool, get_jsdoc_parameter_tags,
    get_jsdoc_parameter_tags_no_cache, get_jsdoc_tags, get_jsdoc_type_parameter_tags,
    get_jsdoc_type_parameter_tags_no_cache, get_leading_comment_ranges,
    get_trailing_comment_ranges, has_initializer, has_jsdoc_nodes, id_text, is_binary_expression,
    is_call_expression, is_element_access_expression, is_export_declaration,
    is_expression_statement, is_function_like, is_function_like_or_class_static_block_declaration,
    is_identifier, is_jsdoc, is_jsdoc_signature, is_jsdoc_template_tag, is_jsdoc_type_tag,
    is_left_hand_side_expression, is_module_declaration, is_no_substituion_template_literal,
    is_numeric_literal, is_object_literal_expression, is_parenthesized_expression,
    is_private_identifier, is_property_access_expression, is_source_file, is_string_literal_like,
    is_variable_statement, is_void_expression, is_white_space_like, last, length,
    maybe_text_char_at_index, module_resolution_option_declarations,
    options_affecting_program_structure, skip_outer_expressions, some, str_to_source_text_as_chars,
    text_substring, AssignmentDeclarationKind, CommandLineOption, CommandLineOptionInterface,
    CommentRange, CompilerOptions, CompilerOptionsValue, DiagnosticWithDetachedLocation,
    DiagnosticWithLocation, EmitFlags, EmitTextWriter, Extension, LanguageVariant,
    LiteralLikeNodeInterface, MapLike, ModifierFlags, ModuleKind, Node, NodeArray, NodeFlags,
    NodeInterface, ObjectFlags, OuterExpressionKinds, PrefixUnaryExpression, PseudoBigInt,
    ReadonlyTextRange, ScriptKind, ScriptTarget, Signature, SignatureFlags, SortedArray,
    SourceFileLike, SourceTextAsChars, Symbol, SymbolFlags, SymbolInterface, SymbolTable,
    SymbolTracker, SymbolWriter, SyntaxKind, TextRange, TextSpan, TokenFlags, TransformFlags,
    TransientSymbolInterface, Type, TypeFlags, TypeInterface, UnderscoreEscapedMap, __String,
    compare_strings_case_sensitive, compare_values, create_text_span_from_bounds,
    escape_leading_underscores, for_each, get_combined_node_flags, get_name_of_declaration,
    insert_sorted, is_big_int_literal, is_member_name, is_type_alias_declaration, skip_trivia,
    BaseDiagnostic, BaseDiagnosticRelatedInformation, BaseNode, BaseSymbol, BaseType,
    CharacterCodes, CheckFlags, Comparison, Debug_, Diagnostic, DiagnosticCollection,
    DiagnosticInterface, DiagnosticMessage, DiagnosticMessageChain, DiagnosticMessageText,
    DiagnosticRelatedInformation, DiagnosticRelatedInformationInterface,
};
use local_macros::enum_unwrapped;

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

pub fn set_parent(child: &Node, parent: Option<Rc<Node>>) -> &Node {
    if let Some(parent) = parent {
        child.set_parent(parent.clone());
    }
    child
}

pub fn set_parent_recursive<TNode: Borrow<Node>>(root_node: Option<TNode>, incremental: bool) {
    unimplemented!()
}

pub fn is_function_expression_or_arrow_function(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction
    )
}
