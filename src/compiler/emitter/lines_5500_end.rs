use bitflags::bitflags;
use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::FromIterator;

use super::brackets;
use crate::{
    write_comment_range, EmitHint, ListFormat, Node, Printer, ReadonlyTextRange, SourceFileLike,
    SourceMapSource, SyntaxKind,
};

impl Printer {
    pub(super) fn emit_trailing_comment(
        &self,
        comment_pos: isize,
        comment_end: isize,
        _kind: SyntaxKind,
        has_trailing_new_line: bool,
    ) {
        if !self.should_write_comment(
            &self.current_source_file().as_source_file().text_as_chars(),
            comment_pos,
        ) {
            return;
        }
        if !self.writer().is_at_start_of_line() {
            self.writer().write_space(" ");
        }

        self.emit_pos(comment_pos);
        write_comment_range(
            &self.current_source_file().as_source_file().text_as_chars(),
            &self.get_current_line_map(),
            &*self.writer(),
            comment_pos.try_into().unwrap(),
            comment_end.try_into().unwrap(),
            &self.new_line,
        );
        self.emit_pos(comment_end);

        if has_trailing_new_line {
            self.writer().write_line(None);
        }
    }

    pub(super) fn emit_trailing_comments_of_position(
        &self,
        pos: isize,
        prefix_space: Option<bool>,
        force_no_newline: Option<bool>,
    ) {
        if self.comments_disabled() {
            return;
        }
        self.enter_comment();
        self.for_each_trailing_comment_to_emit(
            pos,
            |comment_pos, comment_end, kind, has_trailing_new_line| {
                if prefix_space == Some(true) {
                    self.emit_trailing_comment(
                        comment_pos,
                        comment_end,
                        kind,
                        has_trailing_new_line,
                    )
                } else if force_no_newline == Some(true) {
                    self.emit_trailing_comment_of_position_no_newline(
                        comment_pos,
                        comment_end,
                        kind,
                    )
                } else {
                    self.emit_trailing_comment_of_position(
                        comment_pos,
                        comment_end,
                        kind,
                        has_trailing_new_line,
                    )
                }
            },
        );
        self.exit_comment();
    }

    pub(super) fn emit_trailing_comment_of_position_no_newline(
        &self,
        comment_pos: isize,
        comment_end: isize,
        kind: SyntaxKind,
    ) {
        self.emit_pos(comment_pos);
        write_comment_range(
            &self.current_source_file().as_source_file().text_as_chars(),
            &self.get_current_line_map(),
            &*self.writer(),
            comment_pos.try_into().unwrap(),
            comment_end.try_into().unwrap(),
            &self.new_line,
        );
        self.emit_pos(comment_end);

        if kind == SyntaxKind::SingleLineCommentTrivia {
            self.writer().write_line(None);
        }
    }

    pub(super) fn emit_trailing_comment_of_position(
        &self,
        comment_pos: isize,
        comment_end: isize,
        _kind: SyntaxKind,
        has_trailing_new_line: bool,
    ) {
        self.emit_pos(comment_pos);
        write_comment_range(
            &self.current_source_file().as_source_file().text_as_chars(),
            &self.get_current_line_map(),
            &*self.writer(),
            comment_pos.try_into().unwrap(),
            comment_end.try_into().unwrap(),
            &self.new_line,
        );
        self.emit_pos(comment_end);

        if has_trailing_new_line {
            self.writer().write_line(None);
        } else {
            self.writer().write_space(" ");
        }
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
