use std::borrow::{Borrow, Cow};
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::iter::FromIterator;
use std::rc::Rc;

use super::brackets;
use crate::{
    get_comment_range, get_emit_flags, get_literal_text, get_shebang, id_text, is_arrow_function,
    is_block, is_empty_statement, is_function_like, is_identifier, is_prologue_directive,
    is_source_file, is_unparsed_source, range_is_on_single_line, single_or_undefined, some,
    token_to_string, with_synthetic_factory, BundleFileSection, BundleFileSectionKind, Debug_,
    EmitFlags, EmitHint, GetLiteralTextFlags, HasInitializerInterface, HasTypeInterface,
    HasTypeParametersInterface, ListFormat, LiteralLikeNodeInterface, NamedDeclarationInterface,
    Node, NodeArray, NodeInterface, Printer, ReadonlyTextRange, SourceFileLike,
    SourceFilePrologueDirective, SourceFilePrologueDirectiveExpression, SourceFilePrologueInfo,
    SourceMapSource, Symbol, SyntaxKind, TextRange, UnparsedSectionInterface,
};

impl Printer {
    pub(super) fn write_line(&self, count: Option<usize>) {
        let count = count.unwrap_or(1);
        unimplemented!()
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
        mut writer: TWriter,
        context_node: Option<&Node>,
    ) -> Option<isize> {
        unimplemented!()
    }

    pub(super) fn write_token_node(&self, node: &Node, writer: fn(&Printer, &str)) {
        writer(self, token_to_string(node.kind()).unwrap());
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
        unimplemented!()
    }

    pub(super) fn write_lines(&self, text: &str) {
        unimplemented!()
    }

    pub(super) fn write_lines_and_indent(
        &self,
        line_count: usize,
        write_space_if_not_indenting: bool,
    ) {
        unimplemented!()
    }

    pub(super) fn decrease_indent_if(&self, value1: bool, value2: Option<bool>) {
        unimplemented!()
    }

    pub(super) fn get_leading_line_terminator_count(
        &self,
        parent_node: Option<&Node>,
        children: &[Rc<Node>],
        format: ListFormat,
    ) -> usize {
        unimplemented!()
    }

    pub(super) fn get_separating_line_terminator_count(
        &self,
        previous_node: Option<&Node>,
        next_node: &Node,
        format: ListFormat,
    ) -> usize {
        unimplemented!()
    }

    pub(super) fn get_closing_line_terminator_count(
        &self,
        parent_node: Option<&Node>,
        children: &[Rc<Node>],
        format: ListFormat,
    ) -> usize {
        unimplemented!()
    }

    pub(super) fn write_line_separators_and_indent_before(
        &self,
        node: &Node,
        parent: &Node,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn write_line_separators_after(&self, node: &Node, parent: &Node) {
        unimplemented!()
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
