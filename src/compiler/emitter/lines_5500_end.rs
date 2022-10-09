use bitflags::bitflags;
use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::FromIterator;
use std::rc::Rc;

use super::{brackets, PipelinePhase};
use crate::{
    compute_line_starts, emit_new_line_before_leading_comment_of_position,
    escape_leading_underscores, get_comment_range, get_containing_node_array, get_emit_flags,
    get_external_module_name, get_node_id, get_original_node, get_synthetic_leading_comments,
    get_synthetic_trailing_comments, id_text, is_file_level_unique_name, is_identifier,
    is_jsdoc_like_text, is_node_descendant_of, is_pinned_comment, is_string_literal,
    make_identifier_from_module_name, maybe_for_each, write_comment_range, Debug_, EmitFlags,
    EmitHint, GeneratedIdentifierFlags, ListFormat, LiteralLikeNodeInterface, Node, NodeInterface,
    Printer, ReadonlyTextRange, SourceFileLike, SourceMapSource, SourceTextAsChars, SymbolFlags,
    SymbolInterface, SyntaxKind, SynthesizedComment, TextRange,
};

impl Printer {
    pub(super) fn emit_trailing_comment(
        &self,
        comment_pos: isize,
        comment_end: isize,
        _kind: SyntaxKind,
        has_trailing_new_line: bool,
    ) {
        unimplemented!()
    }

    pub(super) fn emit_trailing_comments_of_position(
        &self,
        pos: isize,
        prefix_space: Option<bool>,
        force_no_newline: Option<bool>,
    ) {
        // unimplemented!()
    }

    pub(super) fn for_each_leading_comment_to_emit<
        TCallback: FnMut(isize, isize, SyntaxKind, bool, isize),
    >(
        &self,
        pos: isize,
        cb: TCallback,
    ) {
        unimplemented!()
    }

    pub(super) fn for_each_trailing_comment_to_emit<
        TCallback: FnMut(isize, isize, SyntaxKind, bool),
    >(
        &self,
        end: isize,
        cb: TCallback,
    ) {
        unimplemented!()
    }

    pub(super) fn emit_detached_comments_and_update_comments_info<TRange: ReadonlyTextRange>(
        &self,
        range: &TRange,
    ) {
        unimplemented!()
    }

    pub(super) fn is_triple_slash_comment(&self, comment_pos: isize, comment_end: isize) -> bool {
        unimplemented!()
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

    pub(super) fn emit_pos(&self, pos: isize) {
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

bitflags! {
    pub struct TempFlags: u32 {
        const Auto = 0x00000000;
        const CountMask = 0x0FFFFFFF;
        const _I = 0x10000000;
    }
}
