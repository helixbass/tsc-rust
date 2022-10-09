use regex::Regex;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::iter::FromIterator;
use std::ptr;
use std::rc::Rc;

use super::brackets;
use crate::{
    are_option_rcs_equal, escape_jsx_attribute_string, escape_non_ascii_string, escape_string,
    for_each, get_emit_flags, get_lines_between_position_and_next_non_whitespace_character,
    get_lines_between_position_and_preceding_non_whitespace_character,
    get_lines_between_range_end_and_range_start, get_literal_text, get_original_node,
    get_source_file_of_node, get_source_text_of_node_from_source_file, get_starts_on_new_line,
    guess_indentation, id_text, is_binding_pattern, is_generated_identifier, is_identifier,
    is_literal_expression, is_numeric_literal, is_private_identifier, last_or_undefined,
    node_is_synthesized, position_is_synthesized, range_end_is_on_same_line_as_range_start,
    range_end_positions_are_on_same_line, range_is_on_single_line,
    range_start_positions_are_on_same_line, token_to_string, Debug_, EmitFlags, EmitHint,
    FunctionLikeDeclarationInterface, GeneratedIdentifierFlags, GetLiteralTextFlags, ListFormat,
    LiteralLikeNodeInterface, NamedDeclarationInterface, Node, NodeArray, NodeInterface, Printer,
    ReadonlyTextRange, ScriptTarget, SignatureDeclarationInterface, SourceMapSource, SyntaxKind,
};

impl Printer {
    pub(super) fn generate_name_cached(
        &self,
        node: &Node,
        flags: Option<GeneratedIdentifierFlags>,
    ) -> String {
        unimplemented!()
    }

    pub(super) fn make_file_level_optimistic_unique_name(&self, name: &str) -> String {
        unimplemented!()
    }

    pub(super) fn get_node_for_generated_name(
        &self,
        name: &Node, /*GeneratedIdentifier*/
    ) -> Rc<Node> {
        unimplemented!()
    }

    pub(super) fn make_name(&self, name: &Node /*GeneratedIdentifier*/) -> String {
        unimplemented!()
    }

    pub(super) fn pipeline_emit_with_comments(&self, hint: EmitHint, node: &Node) {
        unimplemented!()
    }

    pub(super) fn emit_comments_before_node(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn emit_comments_after_node(
        &self,
        node: &Node,
        saved_container_pos: isize,
        saved_container_end: isize,
        saved_declaration_list_container_end: isize,
    ) {
        unimplemented!()
    }

    pub(super) fn emit_body_with_detached_comments<
        TDetachedRange: ReadonlyTextRange,
        TEmitCallback: FnMut(&Node),
    >(
        &self,
        node: &Node,
        detached_range: &TDetachedRange,
        emit_callback: TEmitCallback,
    ) {
        // unimplemented!()
    }

    pub(super) fn original_nodes_have_same_parent(&self, nodeA: &Node, nodeB: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn sibling_node_positions_are_comparable(
        &self,
        previous_node: &Node,
        next_node: &Node,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn emit_leading_comments_of_position(&self, pos: isize) {
        // unimplemented!()
    }

    pub(super) fn emit_trailing_comments_of_position(
        &self,
        pos: isize,
        prefix_space: Option<bool>,
        force_no_newline: Option<bool>,
    ) {
        // unimplemented!()
    }

    pub(super) fn pipeline_emit_with_source_maps(&self, hint: EmitHint, node: &Node) {
        unimplemented!()
    }

    pub(super) fn emit_source_maps_before_node(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn emit_source_maps_after_node(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn emit_token_with_source_map<
        TWriter: FnMut(&str),
        TEmitCallback: FnMut(SyntaxKind, TWriter, isize),
    >(
        &self,
        node: Option<&Node>,
        token: SyntaxKind,
        writer: TWriter,
        token_pos: isize,
        emit_callback: TEmitCallback,
    ) -> isize {
        unimplemented!()
    }

    pub(super) fn set_source_map_source(&self, source: SourceMapSource) {
        unimplemented!()
    }
}

pub(super) fn create_brackets_map() -> HashMap<ListFormat, (&'static str, &'static str)> {
    HashMap::from_iter(IntoIterator::into_iter([
        (ListFormat::Braces, ("{", "}")),
        (ListFormat::Parenthesis, ("(", ")")),
        (ListFormat::AngleBrackets, ("<", ">")),
        (ListFormat::SquareBrackets, ("[", "]")),
    ]))
}

pub(super) fn get_opening_bracket(format: ListFormat) -> &'static str {
    brackets
        .get(&(format & ListFormat::BracketsMask))
        .unwrap()
        .0
}

pub(super) fn get_closing_bracket(format: ListFormat) -> &'static str {
    brackets
        .get(&(format & ListFormat::BracketsMask))
        .unwrap()
        .1
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TempFlags {
    Auto = 0x00000000,
    CountMask = 0x0FFFFFFF,
    _I = 0x10000000,
}
