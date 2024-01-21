use std::{collections::HashMap, convert::TryInto, io, iter::FromIterator, rc::Rc};

use bitflags::bitflags;
use gc::Gc;
use id_arena::Id;

use super::{brackets, PipelinePhase};
use crate::{
    emit_detached_comments, file_extension_is, for_each_leading_comment_range,
    for_each_trailing_comment_range, get_emit_flags, get_line_and_character_of_position,
    get_source_map_range, is_in_json_file, is_recognized_triple_slash_comment, is_unparsed_source,
    last, position_is_synthesized, skip_trivia, try_parse_raw_source_map, write_comment_range,
    Debug_, EmitFlags, EmitHint, EmitTextWriter, Extension, GetOrInsertDefault, LineAndCharacter,
    ListFormat, Node, NodeInterface, Printer, RawSourceMap, ReadonlyTextRange, SourceFileLike,
    SourceMapSource, SourceTextAsChars, SyntaxKind, TextRange,
    InArena,
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
            &self.current_source_file().ref_(self).as_source_file().text_as_chars(),
            comment_pos,
        ) {
            return;
        }
        if !self.writer().is_at_start_of_line() {
            self.writer().write_space(" ");
        }

        self.emit_pos(comment_pos);
        write_comment_range(
            &self.current_source_file().ref_(self).as_source_file().text_as_chars(),
            &self.get_current_line_map(),
            &**self.writer(),
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
            &self.current_source_file().ref_(self).as_source_file().text_as_chars(),
            &self.get_current_line_map(),
            &**self.writer(),
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
            &self.current_source_file().ref_(self).as_source_file().text_as_chars(),
            &self.get_current_line_map(),
            &**self.writer(),
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
                        &current_source_file.ref_(self).as_source_file().text_as_chars(),
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
                    &current_source_file.ref_(self).as_source_file().text_as_chars(),
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
            &self.current_source_file().ref_(self).as_source_file().text_as_chars(),
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

    pub(super) fn emit_detached_comments_and_update_comments_info(
        &self,
        range: &impl ReadonlyTextRange,
    ) {
        let current_detached_comment_info = emit_detached_comments(
            &self.current_source_file().ref_(self).as_source_file().text_as_chars(),
            &*self.get_current_line_map(),
            &**self.writer(),
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
        if let Some(current_detached_comment_info) = current_detached_comment_info {
            self.maybe_detached_comments_info_mut()
                .get_or_insert_default_()
                .push(current_detached_comment_info);
        }
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
            &self.current_source_file().ref_(self).as_source_file().text_as_chars(),
            comment_pos,
        ) {
            return;
        }
        self.emit_pos(comment_pos);
        write_comment_range(
            text,
            line_map,
            writer,
            comment_pos.try_into().unwrap(),
            comment_end.try_into().unwrap(),
            new_line,
        );
        self.emit_pos(comment_end);
    }

    pub(super) fn is_triple_slash_comment(&self, comment_pos: isize, comment_end: isize) -> bool {
        is_recognized_triple_slash_comment(
            &self.current_source_file().ref_(self).as_source_file().text_as_chars(),
            comment_pos.try_into().unwrap(),
            comment_end.try_into().unwrap(),
        )
    }

    pub(super) fn get_parsed_source_map(
        &self,
        node: Id<Node>, /*UnparsedSource*/
    ) -> Option<Rc<RawSourceMap>> {
        let node_ref = node.ref_(self);
        let node_as_unparsed_source = node_ref.as_unparsed_source();
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

    pub(super) fn pipeline_emit_with_source_maps(
        &self,
        hint: EmitHint,
        node: Id<Node>,
    ) -> io::Result<()> {
        let pipeline_phase = self.get_next_pipeline_phase(PipelinePhase::SourceMaps, hint, node)?;
        self.emit_source_maps_before_node(node);
        pipeline_phase(self, hint, node)?;
        self.emit_source_maps_after_node(node);

        Ok(())
    }

    pub(super) fn emit_source_maps_before_node(&self, node: Id<Node>) {
        let emit_flags = get_emit_flags(&node.ref_(self));
        let source_map_range = get_source_map_range(&node.ref_(self));

        if is_unparsed_source(&node.ref_(self)) {
            Debug_.assert_is_defined(
                &node.ref_(self).maybe_parent(),
                Some("UnparsedNodes must have parent pointers"),
            );
            let node_parent = node.ref_(self).parent();
            let parsed = self.get_parsed_source_map(node_parent);
            if let (Some(parsed), Some(source_map_generator)) =
                (parsed, self.maybe_source_map_generator())
            {
                let node_parent_as_unparsed_source = node_parent.as_unparsed_source();
                source_map_generator.append_source_map(
                    self.writer().get_line(),
                    self.writer().get_column(),
                    &parsed,
                    node_parent_as_unparsed_source
                        .source_map_path
                        .as_ref()
                        .unwrap(),
                    Some(
                        node_parent_as_unparsed_source
                            .get_line_and_character_of_position(node.ref_(self).pos().try_into().unwrap()),
                    ),
                    Some(
                        node_parent_as_unparsed_source
                            .get_line_and_character_of_position(node.ref_(self).end().try_into().unwrap()),
                    ),
                );
            }
        } else {
            let source = source_map_range
                .source
                .clone()
                .unwrap_or_else(|| self.source_map_source());
            if node.ref_(self).kind() != SyntaxKind::NotEmittedStatement
                && !emit_flags.intersects(EmitFlags::NoLeadingSourceMap)
                && source_map_range.pos() >= 0
            {
                self.emit_source_pos(
                    source_map_range
                        .source
                        .clone()
                        .unwrap_or_else(|| self.source_map_source()),
                    self.skip_source_trivia(&source, source_map_range.pos()),
                );
            }
            if emit_flags.intersects(EmitFlags::NoNestedSourceMaps) {
                self.set_source_maps_disabled(true);
            }
        }
    }

    pub(super) fn emit_source_maps_after_node(&self, node: Id<Node>) {
        let emit_flags = get_emit_flags(&node.ref_(self));
        let source_map_range = get_source_map_range(&node.ref_(self));

        if !is_unparsed_source(&node.ref_(self)) {
            if emit_flags.intersects(EmitFlags::NoNestedSourceMaps) {
                self.set_source_maps_disabled(false);
            }
            if node.ref_(self).kind() != SyntaxKind::NotEmittedStatement
                && !emit_flags.intersects(EmitFlags::NoTrailingSourceMap)
                && source_map_range.end() >= 0
            {
                self.emit_source_pos(
                    source_map_range
                        .source
                        .clone()
                        .unwrap_or_else(|| self.source_map_source()),
                    source_map_range.end(),
                );
            }
        }
    }

    pub(super) fn skip_source_trivia(&self, source: &SourceMapSource, pos: isize) -> isize {
        if let Some(source_skip_trivia) = source.skip_trivia() {
            source_skip_trivia.call(pos)
        } else {
            skip_trivia(&source.text_as_chars(), pos, None, None, None)
        }
    }

    pub(super) fn emit_token_with_source_map<TWriter: FnMut(&str)>(
        &self,
        node: Option<Id<Node>>,
        token: SyntaxKind,
        writer: TWriter,
        mut token_pos: isize,
        mut emit_callback: impl FnMut(SyntaxKind, TWriter, isize) -> isize,
    ) -> isize {
        if self.source_maps_disabled()
            || matches!(
                node,
                Some(node) if is_in_json_file(Some(&node.ref_(self)))
            )
        {
            return emit_callback(token, writer, token_pos);
        }

        let emit_node = node.and_then(|node| node.ref_(self).maybe_emit_node());
        let emit_flags = emit_node
            .as_ref()
            .and_then(|emit_node| (**emit_node).borrow().flags)
            .unwrap_or(EmitFlags::None);
        let range = emit_node.as_ref().and_then(|emit_node| {
            (**emit_node)
                .borrow()
                .token_source_map_ranges
                .as_ref()
                .and_then(|emit_node_token_source_map_ranges| {
                    emit_node_token_source_map_ranges
                        .get(&token)
                        .cloned()
                        .flatten()
                })
        });
        let source = range
            .as_ref()
            .and_then(|range| range.source.clone())
            .unwrap_or_else(|| self.source_map_source());

        token_pos = self.skip_source_trivia(
            &source,
            range.as_ref().map_or(token_pos, |range| range.pos()),
        );
        if !emit_flags.intersects(EmitFlags::NoTokenLeadingSourceMaps) && token_pos >= 0 {
            self.emit_source_pos(source.clone(), token_pos);
        }

        token_pos = emit_callback(token, writer, token_pos);

        if let Some(range) = range {
            token_pos = range.end();
        }
        if !emit_flags.intersects(EmitFlags::NoTokenTrailingSourceMaps) && token_pos >= 0 {
            self.emit_source_pos(source, token_pos);
        }

        token_pos
    }

    pub(super) fn emit_pos(&self, pos: isize) {
        if self.source_maps_disabled()
            || position_is_synthesized(pos)
            || self.is_json_source_map_source(&self.source_map_source())
        {
            return;
        }

        let LineAndCharacter {
            line: source_line,
            character: source_character,
        } = get_line_and_character_of_position(&*self.source_map_source(), pos.try_into().unwrap());
        self.source_map_generator().add_mapping(
            self.writer().get_line(),
            self.writer().get_column(),
            self.source_map_source_index().try_into().unwrap(),
            source_line,
            source_character,
            None,
        );
    }

    pub(super) fn emit_source_pos(&self, source: Gc<SourceMapSource>, pos: isize) {
        if !matches!(
            self.maybe_source_map_source(),
            Some(source_map_source) if Gc::ptr_eq(
                &source,
                &source_map_source
            )
        ) {
            let saved_source_map_source = self.maybe_source_map_source();
            let saved_source_map_source_index = self.source_map_source_index();
            self.set_source_map_source(source);
            self.reset_source_map_source(
                saved_source_map_source.unwrap(),
                saved_source_map_source_index,
            );
            self.emit_pos(pos);
        } else {
            self.emit_pos(pos);
        }
    }

    pub(super) fn set_source_map_source(&self, source: Gc<SourceMapSource>) {
        if self.source_maps_disabled() {
            return;
        }

        self.set_source_map_source_(Some(source.clone()));

        if matches!(
            self.maybe_most_recently_added_source_map_source().as_ref(),
            Some(most_recently_added_source_map_source) if Gc::ptr_eq(
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
            self.source_map_generator().set_source_content(
                // TODO: should this just be a usize?
                self.source_map_source_index().try_into().unwrap(),
                Some(source.text().clone()),
            );
        }

        self.set_most_recently_added_source_map_source(Some(source));
        self.set_most_recently_added_source_map_source_index(self.source_map_source_index());
    }

    pub(super) fn reset_source_map_source(&self, source: Gc<SourceMapSource>, source_index: isize) {
        self.set_source_map_source_(Some(source));
        self.set_source_map_source_index(source_index);
    }

    pub(super) fn is_json_source_map_source(&self, source_file: &SourceMapSource) -> bool {
        file_extension_is(&source_file.file_name(), Extension::Json.to_str())
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
