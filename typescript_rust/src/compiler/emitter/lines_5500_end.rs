use bitflags::bitflags;
use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::FromIterator;
use std::rc::Rc;

use super::{brackets, PipelinePhase};
use crate::{
    emit_detached_comments, for_each_leading_comment_range, for_each_trailing_comment_range,
    is_recognized_triple_slash_comment, last, try_parse_raw_source_map, write_comment_range,
    EmitHint, EmitTextWriter, ListFormat, Node, Printer, RawSourceMap, ReadonlyTextRange,
    SourceFileLike, SourceMapSource, SyntaxKind,
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
        mut cb: TCallback,
    ) {
        if let Some(current_source_file) = self.maybe_current_source_file().as_ref() {
            if self.container_pos() == -1 || pos != self.container_pos() {
                if self.has_detached_comments(pos) {
                    self.for_each_leading_comment_without_detached_comments(cb);
                } else {
                    for_each_leading_comment_range(
                        &current_source_file.as_source_file().text_as_chars(),
                        pos.try_into().unwrap(),
                        |pos: usize,
                         end: usize,
                         kind: SyntaxKind,
                         has_trailing_new_line: bool,
                         state: &isize|
                         -> Option<()> {
                            cb(
                                pos.try_into().unwrap(),
                                end.try_into().unwrap(),
                                kind,
                                has_trailing_new_line,
                                *state,
                            );
                            None
                        },
                        &pos,
                    );
                }
            }
        }
    }

    pub(super) fn for_each_trailing_comment_to_emit<
        TCallback: FnMut(isize, isize, SyntaxKind, bool),
    >(
        &self,
        end: isize,
        mut cb: TCallback,
    ) {
        if let Some(current_source_file) = self.maybe_current_source_file().as_ref() {
            if self.container_end() == -1
                || end != self.container_end() && end != self.declaration_list_container_end()
            {
                for_each_trailing_comment_range(
                    &current_source_file.as_source_file().text_as_chars(),
                    end,
                    |pos: usize,
                     end: usize,
                     kind: SyntaxKind,
                     has_trailing_new_line: bool,
                     _state: &()|
                     -> Option<()> {
                        cb(
                            pos.try_into().unwrap(),
                            end.try_into().unwrap(),
                            kind,
                            has_trailing_new_line,
                        );
                        None
                    },
                    &(),
                );
            }
        }
    }

    pub(super) fn has_detached_comments(&self, pos: isize) -> bool {
        matches!(
            self.maybe_detached_comments_info().as_ref(),
            Some(detached_comments_info) if last(detached_comments_info).node_pos == pos
        )
    }

    pub(super) fn for_each_leading_comment_without_detached_comments<
        TCallback: FnMut(isize, isize, SyntaxKind, bool, isize),
    >(
        &self,
        mut cb: TCallback,
    ) {
        let pos = last(&*self.detached_comments_info()).detached_comment_end_pos;
        if self.detached_comments_info().len() - 1 != 0 {
            self.detached_comments_info_mut().pop();
        } else {
            self.set_detached_comments_info(None);
        }

        for_each_leading_comment_range(
            &self.current_source_file().as_source_file().text_as_chars(),
            pos.try_into().unwrap(),
            |pos: usize,
             end: usize,
             kind: SyntaxKind,
             has_trailing_new_line: bool,
             state: &isize|
             -> Option<()> {
                cb(
                    pos.try_into().unwrap(),
                    end.try_into().unwrap(),
                    kind,
                    has_trailing_new_line,
                    *state,
                );
                None
            },
            &pos,
        );
    }

    pub(super) fn emit_detached_comments_and_update_comments_info<TRange: ReadonlyTextRange>(
        &self,
        range: &TRange,
    ) {
        let current_detached_comment_info = emit_detached_comments(
            &self.current_source_file().as_source_file().text_as_chars(),
            &*self.get_current_line_map(),
            &*self.writer(),
            |text: &SourceTextAsChars,
             line_map: &[usize],
             writer: &dyn EmitTextWriter,
             comment_pos: isize,
             comment_end: isize,
             new_line: &str| {
                self.emit_comment(text, line_map, writer, comment_pos, comment_end, new_line)
            },
            range,
            &self.new_line,
            self.comments_disabled(),
        );
    }

    pub(super) fn emit_comment(
        &self,
        text: &SourceTextAsChars,
        line_map: &[usize],
        writer: &dyn EmitTextWriter,
        comment_pos: isize,
        comment_end: isize,
        new_line: &str,
    ) {
        if !self.should_write_comment(
            &self.current_source_file().as_source_file().text_as_chars(),
            comment_pos,
        ) {
            return;
        }
        self.emit_pos(comment_pos);
        write_comment_range(
            text,
            line_map,
            &*self.writer(),
            comment_pos.try_into().unwrap(),
            comment_end.try_into().unwrap(),
            &self.new_line,
        );
        self.emit_pos(comment_end);
    }

    pub(super) fn is_triple_slash_comment(&self, comment_pos: isize, comment_end: isize) -> bool {
        is_recognized_triple_slash_comment(
            &self.current_source_file().as_source_file().text_as_chars(),
            comment_pos.try_into().unwrap(),
            comment_end.try_into().unwrap(),
        )
    }

    pub(super) fn get_parsed_source_map(
        &self,
        node: &Node, /*UnparsedSource*/
    ) -> Option<Rc<RawSourceMap>> {
        let node_as_unparsed_source = node.as_unparsed_source();
        if node_as_unparsed_source.parsed_source_map.borrow().is_none() {
            if let Some(node_source_map_text) = node_as_unparsed_source.source_map_text.as_ref() {
                *node_as_unparsed_source.parsed_source_map.borrow_mut() =
                    Some(try_parse_raw_source_map(node_source_map_text).map(Rc::new));
            }
        }
        let ret = node_as_unparsed_source
            .parsed_source_map
            .borrow()
            .clone()
            .flatten();
        ret
    }

    pub(super) fn pipeline_emit_with_source_maps(&self, hint: EmitHint, node: &Node) {
        let pipeline_phase = self.get_next_pipeline_phase(PipelinePhase::SourceMaps, hint, node);
        self.emit_source_maps_before_node(node);
        pipeline_phase(self, hint, node);
        self.emit_source_maps_after_node(node);
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

    pub(super) fn set_source_map_source(&self, source: Rc<SourceMapSource>) {
        if self.source_maps_disabled() {
            return;
        }

        self.set_source_map_source_(Some(source.clone()));

        if matches!(
            self.maybe_most_recently_added_source_map_source().as_ref(),
            Some(most_recently_added_source_map_source) if Rc::ptr_eq(
                &source,
                most_recently_added_source_map_source
            )
        ) {
            self.set_source_map_source_index(self.most_recently_added_source_map_source_index());
            return;
        }

        if self.is_json_source_map_source(&source) {
            return;
        }

        self.set_source_map_source_index(
            self.source_map_generator()
                .add_source(&source.file_name())
                .try_into()
                .unwrap(),
        );
        if self.printer_options.inline_sources == Some(true) {
            // self.source_map_generator()
            //     .set_source_content()
            unimplemented!()
        }

        self.set_most_recently_added_source_map_source(Some(source));
        self.set_most_recently_added_source_map_source_index(self.source_map_source_index());
    }

    pub(super) fn is_json_source_map_source(&self, source_file: &SourceMapSource) -> bool {
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
