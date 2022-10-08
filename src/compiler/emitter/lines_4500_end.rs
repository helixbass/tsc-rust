use regex::Regex;
use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::FromIterator;
use std::ptr;
use std::rc::Rc;

use super::brackets;
use crate::{
    are_option_rcs_equal, get_emit_flags,
    get_lines_between_position_and_next_non_whitespace_character,
    get_lines_between_position_and_preceding_non_whitespace_character,
    get_lines_between_range_end_and_range_start, get_literal_text, get_original_node,
    get_starts_on_new_line, guess_indentation, id_text, is_identifier, last_or_undefined,
    node_is_synthesized, position_is_synthesized, range_end_is_on_same_line_as_range_start,
    range_end_positions_are_on_same_line, range_is_on_single_line,
    range_start_positions_are_on_same_line, token_to_string, Debug_, EmitFlags, EmitHint,
    GetLiteralTextFlags, ListFormat, Node, NodeArray, NodeInterface, Printer, ReadonlyTextRange,
    SourceMapSource, SyntaxKind,
};

impl Printer {
    pub(super) fn write_line(&self, count: Option<usize>) {
        let count = count.unwrap_or(1);
        for i in 0..count {
            self.writer().write_line(Some(i > 0));
        }
    }

    pub(super) fn increase_indent(&self) {
        self.writer().increase_indent();
    }

    pub(super) fn decrease_indent(&self) {
        self.writer().decrease_indent();
    }

    pub(super) fn write_token<TWriter: FnMut(&str)>(
        &self,
        token: SyntaxKind,
        pos: isize,
        writer: TWriter,
        context_node: Option<&Node>,
    ) -> Option<isize> {
        if !self.source_maps_disabled() {
            Some(self.emit_token_with_source_map(
                context_node,
                token,
                writer,
                pos,
                |token, writer, pos| {
                    self.write_token_text(token, writer, Some(pos));
                },
            ))
        } else {
            self.write_token_text(token, writer, Some(pos))
        }
    }

    pub(super) fn write_token_node(&self, node: &Node, writer: fn(&Printer, &str)) {
        self.on_before_emit_token(Some(node));
        writer(self, token_to_string(node.kind()).unwrap());
        self.on_after_emit_token(Some(node));
    }

    pub(super) fn write_token_text<TWriter: FnMut(&str)>(
        &self,
        token: SyntaxKind,
        mut writer: TWriter,
        pos: Option<isize>,
    ) -> Option<isize> {
        let token_string = token_to_string(token).unwrap();
        writer(token_string);
        pos.map(|pos| {
            if pos < 0 {
                pos
            } else {
                pos + TryInto::<isize>::try_into(token_string.len()).unwrap()
            }
        })
    }

    pub(super) fn write_line_or_space(
        &self,
        parent_node: &Node,
        prev_child_node: &Node,
        next_child_node: &Node,
    ) {
        if get_emit_flags(parent_node).intersects(EmitFlags::SingleLine) {
            self.write_space();
        } else if self.maybe_preserve_source_newlines() == Some(true) {
            let lines = self.get_lines_between_nodes(parent_node, prev_child_node, next_child_node);
            if lines != 0 {
                self.write_line(Some(lines));
            } else {
                self.write_space();
            }
        } else {
            self.write_line(None);
        }
    }

    pub(super) fn write_lines(&self, text: &str) {
        lazy_static! {
            static ref newline_regex: Regex = Regex::new(r"\r\n?|\n").unwrap();
        }
        let lines = newline_regex.split(text).collect::<Vec<_>>();
        let indentation = guess_indentation(&lines);
        for &line_text in &lines {
            let line =
                if let Some(indentation) = indentation.filter(|indentation| *indentation != 0) {
                    &line_text[indentation..]
                } else {
                    line_text
                };
            if !line.is_empty() {
                self.write_line(None);
                self.write(line);
            }
        }
    }

    pub(super) fn write_lines_and_indent(
        &self,
        line_count: usize,
        write_space_if_not_indenting: bool,
    ) {
        if line_count != 0 {
            self.increase_indent();
            self.write_line(Some(line_count));
        } else if write_space_if_not_indenting {
            self.write_space();
        }
    }

    pub(super) fn decrease_indent_if(&self, value1: bool, value2: Option<bool>) {
        if value1 {
            self.decrease_indent();
        }
        if value2 == Some(true) {
            self.decrease_indent();
        }
    }

    pub(super) fn get_leading_line_terminator_count(
        &self,
        parent_node: Option<&Node>,
        children: &[Rc<Node>],
        format: ListFormat,
    ) -> usize {
        if format.intersects(ListFormat::PreserveLines)
            || self.maybe_preserve_source_newlines() == Some(true)
        {
            if format.intersects(ListFormat::PreferNewLine) {
                return 1;
            }

            let first_child = children.get(0);
            if first_child.is_none() {
                return if match parent_node {
                    None => true,
                    Some(parent_node) => {
                        range_is_on_single_line(parent_node, &self.current_source_file())
                    }
                } {
                    0
                } else {
                    1
                };
            }
            let first_child = first_child.unwrap();
            if Some(first_child.pos()) == self.maybe_next_list_element_pos() {
                return 0;
            }
            if first_child.kind() == SyntaxKind::JsxText {
                return 0;
            }
            if let Some(parent_node) = parent_node.filter(|&parent_node| {
                !position_is_synthesized(parent_node.pos())
                    && !node_is_synthesized(&**first_child)
                    && match first_child.maybe_parent().as_ref() {
                        None => true,
                        Some(first_child_parent) => are_option_rcs_equal(
                            get_original_node(
                                Some(&**first_child_parent),
                                Option::<fn(Option<Rc<Node>>) -> bool>::None,
                            )
                            .as_ref(),
                            get_original_node(
                                Some(parent_node),
                                Option::<fn(Option<Rc<Node>>) -> bool>::None,
                            )
                            .as_ref(),
                        ),
                    }
            }) {
                if self.maybe_preserve_source_newlines() == Some(true) {
                    return self.get_effective_lines(|include_comments| {
                        get_lines_between_position_and_preceding_non_whitespace_character(
                            first_child.pos(),
                            parent_node.pos(),
                            &self.current_source_file(),
                            Some(include_comments),
                        )
                    });
                }
                return if range_start_positions_are_on_same_line(
                    parent_node,
                    &**first_child,
                    &self.current_source_file(),
                ) {
                    0
                } else {
                    1
                };
            }
            if self.synthesized_node_starts_on_new_line(first_child, format) {
                return 1;
            }
        }
        if format.intersects(ListFormat::MultiLine) {
            1
        } else {
            0
        }
    }

    pub(super) fn get_separating_line_terminator_count(
        &self,
        previous_node: Option<&Node>,
        next_node: &Node,
        format: ListFormat,
    ) -> usize {
        if format.intersects(ListFormat::PreserveLines)
            || self.maybe_preserve_source_newlines() == Some(true)
        {
            if previous_node.is_none()
            /*|| nextNode === undefined*/
            {
                return 0;
            }
            let previous_node = previous_node.unwrap();
            if next_node.kind() == SyntaxKind::JsxText {
                return 0;
            } else if !node_is_synthesized(previous_node) && !node_is_synthesized(next_node) {
                if self.maybe_preserve_source_newlines() == Some(true)
                    && self.sibling_node_positions_are_comparable(previous_node, next_node)
                {
                    return self.get_effective_lines(|include_comments| {
                        get_lines_between_range_end_and_range_start(
                            previous_node,
                            next_node,
                            &self.current_source_file(),
                            include_comments,
                        )
                    });
                } else if self.maybe_preserve_source_newlines() != Some(true)
                    && self.original_nodes_have_same_parent(previous_node, next_node)
                {
                    return if range_end_is_on_same_line_as_range_start(
                        previous_node,
                        next_node,
                        &self.current_source_file(),
                    ) {
                        0
                    } else {
                        1
                    };
                }
                return if format.intersects(ListFormat::PreferNewLine) {
                    1
                } else {
                    0
                };
            } else if self.synthesized_node_starts_on_new_line(previous_node, format)
                || self.synthesized_node_starts_on_new_line(next_node, format)
            {
                return 1;
            }
        } else if get_starts_on_new_line(next_node) == Some(true) {
            return 1;
        }
        if format.intersects(ListFormat::MultiLine) {
            1
        } else {
            0
        }
    }

    pub(super) fn get_closing_line_terminator_count(
        &self,
        parent_node: Option<&Node>,
        children: RefNodeArrayOrSlice,
        format: ListFormat,
    ) -> usize {
        if format.intersects(ListFormat::PreserveLines)
            || self.maybe_preserve_source_newlines() == Some(true)
        {
            if format.intersects(ListFormat::PreferNewLine) {
                return 1;
            }

            let last_child = last_or_undefined(children.as_slice());
            if last_child.is_none() {
                return if match parent_node {
                    None => true,
                    Some(parent_node) => {
                        range_is_on_single_line(parent_node, &self.current_source_file())
                    }
                } {
                    0
                } else {
                    1
                };
            }
            let last_child = last_child.unwrap();
            if let Some(parent_node) = parent_node.filter(|&parent_node| {
                !position_is_synthesized(parent_node.pos())
                    && !node_is_synthesized(&**last_child)
                    && match last_child.maybe_parent().as_ref() {
                        None => true,
                        Some(last_child_parent) => ptr::eq(&**last_child_parent, parent_node),
                    }
            }) {
                if self.maybe_preserve_source_newlines() == Some(true) {
                    let end = if let RefNodeArrayOrSlice::NodeArray(children) = children {
                        if !position_is_synthesized(children.end()) {
                            children.end()
                        } else {
                            last_child.end()
                        }
                    } else {
                        last_child.end()
                    };
                    return self.get_effective_lines(|include_comments| {
                        get_lines_between_position_and_next_non_whitespace_character(
                            end,
                            parent_node.end(),
                            &self.current_source_file(),
                            Some(include_comments),
                        )
                    });
                }
                return if range_end_positions_are_on_same_line(
                    parent_node,
                    &**last_child,
                    &self.current_source_file(),
                ) {
                    0
                } else {
                    1
                };
            }
            if self.synthesized_node_starts_on_new_line(last_child, format) {
                return 1;
            }
        }
        if format.intersects(ListFormat::MultiLine)
            && !format.intersects(ListFormat::NoTrailingNewLine)
        {
            return 1;
        }
        0
    }

    pub(super) fn get_effective_lines<TGetLineDifference: FnMut(bool) -> usize>(
        &self,
        mut get_line_difference: TGetLineDifference,
    ) -> usize {
        Debug_.assert(self.maybe_preserve_source_newlines() == Some(true), None);
        let lines = get_line_difference(true);
        if lines == 0 {
            return get_line_difference(false);
        }
        lines
    }

    pub(super) fn write_line_separators_and_indent_before(
        &self,
        node: &Node,
        parent: &Node,
    ) -> bool {
        let leading_newlines = if self.maybe_preserve_source_newlines() == Some(true) {
            self.get_leading_line_terminator_count(
                Some(parent),
                &[node.node_wrapper()],
                ListFormat::None,
            )
        } else {
            0
        };
        if leading_newlines != 0 {
            self.write_lines_and_indent(leading_newlines, false);
        }
        leading_newlines != 0
    }

    pub(super) fn write_line_separators_after(&self, node: &Node, parent: &Node) {
        let trailing_newlines = if self.maybe_preserve_source_newlines() == Some(true) {
            self.get_closing_line_terminator_count(
                Some(parent),
                // (&[node.node_wrapper()]).into(),
                RefNodeArrayOrSlice::Slice(&[node.node_wrapper()]),
                ListFormat::None,
            )
        } else {
            0
        };
        if trailing_newlines != 0 {
            self.write_line(Some(trailing_newlines));
        }
    }

    pub(super) fn synthesized_node_starts_on_new_line(
        &self,
        node: &Node,
        format: ListFormat,
    ) -> bool {
        if node_is_synthesized(node) {
            let starts_on_new_line = get_starts_on_new_line(node);
            if starts_on_new_line.is_none() {
                return format.intersects(ListFormat::PreferNewLine);
            }
            let starts_on_new_line = starts_on_new_line.unwrap();

            return starts_on_new_line;
        }

        format.intersects(ListFormat::PreferNewLine)
    }

    pub(super) fn get_lines_between_nodes(
        &self,
        parent: &Node,
        node1: &Node,
        node2: &Node,
    ) -> usize {
        unimplemented!()
    }

    pub(super) fn is_empty_block(&self, block: &Node /*BlockLike*/) -> bool {
        unimplemented!()
    }

    pub(super) fn get_text_of_node(&self, node: &Node, include_trivia: Option<bool>) -> String {
        if false {
            unimplemented!()
        } else if (is_identifier(node) || false) && true {
            return id_text(node);
        }

        unimplemented!()
    }

    pub(super) fn get_literal_text_of_node(
        &self,
        node: &Node,
        never_ascii_escape: Option<bool>,
        jsx_attribute_escape: bool,
    ) -> Cow<'static, str> {
        let flags = GetLiteralTextFlags::None;

        get_literal_text(node, self.maybe_current_source_file(), flags)
    }

    pub(super) fn push_name_generation_scope(&self, node: Option<&Node>) {
        unimplemented!()
    }

    pub(super) fn pop_name_generation_scope(&self, node: Option<&Node>) {
        unimplemented!()
    }

    pub(super) fn generate_names(&self, node: Option<&Node>) {
        unimplemented!()
    }

    pub(super) fn generate_member_names(&self, node: Option<&Node>) {
        unimplemented!()
    }

    pub(super) fn generate_name_if_needed(&self, name: Option<&Node /*DeclarationName*/>) {
        unimplemented!()
    }

    pub(super) fn make_file_level_optimistic_unique_name(&self, name: &str) -> String {
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

#[derive(Copy, Clone)]
pub enum RefNodeArrayOrSlice<'a> {
    NodeArray(&'a NodeArray),
    Slice(&'a [Rc<Node>]),
}

impl<'a> RefNodeArrayOrSlice<'a> {
    pub fn as_slice(&'a self) -> &'a [Rc<Node>] {
        match *self {
            Self::NodeArray(value) => &*value,
            Self::Slice(value) => value,
        }
    }
}

impl<'a> From<&'a NodeArray> for RefNodeArrayOrSlice<'a> {
    fn from(value: &'a NodeArray) -> Self {
        Self::NodeArray(value)
    }
}

impl<'a> From<&'a [Rc<Node>]> for RefNodeArrayOrSlice<'a> {
    fn from(value: &'a [Rc<Node>]) -> Self {
        Self::Slice(value)
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
