use std::{convert::TryInto, io, rc::Rc};

use id_arena::Id;

use super::{PipelinePhase, TempFlags};
use crate::{
    compute_line_starts, emit_new_line_before_leading_comment_of_position,
    escape_leading_underscores, get_comment_range, get_containing_node_array, get_emit_flags,
    get_external_module_name, get_node_id, get_original_node, get_synthetic_leading_comments,
    get_synthetic_trailing_comments, id_text, is_file_level_unique_name, is_identifier,
    is_jsdoc_like_text, is_pinned_comment, is_string_literal, make_identifier_from_module_name,
    maybe_for_each, maybe_is_node_descendant_of, write_comment_range, Debug_, EmitFlags, EmitHint,
    GeneratedIdentifierFlags, InArena, LiteralLikeNodeInterface, Node, NodeInterface, Printer,
    ReadonlyTextRange, SourceFileLike, SourceTextAsChars, SymbolFlags, SymbolInterface, SyntaxKind,
    SynthesizedComment, TextRange,
};

impl Printer {
    pub(super) fn generate_name_cached(
        &self,
        node: Id<Node>,
        flags: Option<GeneratedIdentifierFlags>,
    ) -> String {
        let node_id = get_node_id(&node.ref_(self));
        if let Some(existing) = self.node_id_to_generated_name().get(&node_id).cloned() {
            return existing;
        }
        let new_value = self.generate_name_for_node(node, flags);
        self.node_id_to_generated_name_mut()
            .insert(node_id, new_value.clone());
        new_value
    }

    pub(super) fn is_unique_name(&self, name: &str) -> bool {
        self.is_file_level_unique_name(name)
            && !self.generated_names().contains(name)
            && !matches!(
                self.maybe_reserved_names(),
                Some(reserved_names) if (*reserved_names).borrow().contains(name)
            )
    }

    pub(super) fn is_file_level_unique_name(&self, name: &str) -> bool {
        self.maybe_current_source_file()
            .map_or(true, |current_source_file| {
                is_file_level_unique_name(
                    &current_source_file.ref_(self),
                    name,
                    Some(|name: &str| self.has_global_name(name) == Some(true)),
                )
            })
    }

    pub(super) fn is_unique_local_name(&self, name: &str, container: Id<Node>) -> bool {
        let mut node = Some(container);
        while maybe_is_node_descendant_of(node, Some(container), self) {
            let node_present = node.as_ref().unwrap();
            if let Some(node_locals) = node_present.ref_(self).maybe_locals() {
                let local = node_locals
                    .ref_(self)
                    .get(&*escape_leading_underscores(name))
                    .cloned();
                if matches!(
                    local,
                    Some(local) if local.ref_(self).flags().intersects(
                        SymbolFlags::Value | SymbolFlags::ExportValue | SymbolFlags::Alias
                    )
                ) {
                    return false;
                }
            }
            node = node_present.ref_(self).maybe_next_container();
        }
        true
    }

    pub(super) fn make_temp_variable_name(
        &self,
        flags: TempFlags,
        reserved_in_nested_scopes: Option<bool>,
    ) -> String {
        if flags != TempFlags::Auto && !self.temp_flags().intersects(flags) {
            let name = if flags == TempFlags::_I { "_i" } else { "_n" };
            if self.is_unique_name(name) {
                self.set_temp_flags(self.temp_flags() | flags);
                if reserved_in_nested_scopes == Some(true) {
                    self.reserve_name_in_nested_scopes(name);
                }
                return name.to_owned();
            }
        }
        loop {
            let count: usize = (self.temp_flags() & TempFlags::CountMask)
                .bits()
                .try_into()
                .unwrap();
            self.set_temp_flags(
                // TODO: could probably avoid using unsafe here by not using a bitflags for
                // TempFlags?
                unsafe { TempFlags::from_bits_unchecked(self.temp_flags().bits() + 1) },
            );
            if count != 8 && count != 13 {
                let name = if count < 26 {
                    format!("_{}", &"abcdefghijklmnopqrstuvwxyz"[count..count + 1])
                } else {
                    format!("_{}", count - 26)
                };
                if self.is_unique_name(&name) {
                    if reserved_in_nested_scopes == Some(true) {
                        self.reserve_name_in_nested_scopes(&name);
                    }
                    return name;
                }
            }
        }
    }

    pub(super) fn make_unique_name(
        &self,
        base_name: &str,
        check_fn: Option<impl Fn(&str) -> bool>,
        optimistic: Option<bool>,
        scoped: Option<bool>,
    ) -> String {
        let check_fn_present = |name: &str| match check_fn.as_ref() {
            Some(check_fn) => check_fn(name),
            None => self.is_unique_name(name),
        };
        if optimistic == Some(true) {
            if check_fn_present(base_name) {
                if scoped == Some(true) {
                    self.reserve_name_in_nested_scopes(base_name);
                } else {
                    self.generated_names_mut().insert(base_name.to_owned());
                }
                return base_name.to_owned();
            }
        }
        let mut base_name = base_name.to_owned();
        if base_name.is_empty() || &base_name[base_name.len() - 1..] != "_" {
            base_name.push_str("_");
        }
        let mut i = 1;
        loop {
            let generated_name = format!("{}{}", base_name, i,);
            if check_fn_present(&generated_name) {
                if scoped == Some(true) {
                    self.reserve_name_in_nested_scopes(&generated_name);
                } else {
                    self.generated_names_mut().insert(generated_name.clone());
                }
                return generated_name;
            }
            i += 1;
        }
    }

    pub(super) fn make_file_level_optimistic_unique_name(&self, name: &str) -> String {
        self.make_unique_name(
            name,
            Some(|name: &str| self.is_file_level_unique_name(name)),
            Some(true),
            None,
        )
    }

    pub(super) fn generate_name_for_module_or_enum(
        &self,
        node: Id<Node>, /*ModuleDeclaration | EnumDeclaration*/
    ) -> String {
        let node_name = node.ref_(self).as_named_declaration().name();
        let name = self.get_text_of_node(node_name, None);
        if self.is_unique_local_name(&name, node) {
            name.into_owned()
        } else {
            self.make_unique_name(&name, Option::<fn(&str) -> bool>::None, None, None)
        }
    }

    pub(super) fn generate_name_for_import_or_export_declaration(
        &self,
        node: Id<Node>, /*ImportDeclaration | ExportDeclaration*/
    ) -> String {
        let expr = get_external_module_name(node, self).unwrap();
        let base_name = if is_string_literal(&expr.ref_(self)) {
            make_identifier_from_module_name(&expr.ref_(self).as_string_literal().text())
        } else {
            "module".to_owned()
        };
        self.make_unique_name(&base_name, Option::<fn(&str) -> bool>::None, None, None)
    }

    pub(super) fn generate_name_for_export_default(&self) -> String {
        self.make_unique_name("default", Option::<fn(&str) -> bool>::None, None, None)
    }

    pub(super) fn generate_name_for_class_expression(&self) -> String {
        self.make_unique_name("class", Option::<fn(&str) -> bool>::None, None, None)
    }

    pub(super) fn generate_name_for_method_or_accessor(
        &self,
        node: Id<Node>, /*MethodDeclaration | AccessorDeclaration*/
    ) -> String {
        let node_name = node.ref_(self).as_named_declaration().name();
        if is_identifier(&node_name.ref_(self)) {
            return self.generate_name_cached(node_name, None);
        }
        self.make_temp_variable_name(TempFlags::Auto, None)
    }

    pub(super) fn generate_name_for_node(
        &self,
        node: Id<Node>,
        flags: Option<GeneratedIdentifierFlags>,
    ) -> String {
        match node.ref_(self).kind() {
            SyntaxKind::Identifier => {
                self.make_unique_name(
                    &self.get_text_of_node(
                        node,
                        None,
                    ),
                    Some(|name: &str| self.is_unique_name(name)),
                    Some(matches!(
                        flags,
                        Some(flags) if flags.intersects(GeneratedIdentifierFlags::Optimistic)
                    )),
                    Some(matches!(
                        flags,
                        Some(flags) if flags.intersects(GeneratedIdentifierFlags::ReservedInNestedScopes)
                    )),
                )
            }
            SyntaxKind::ModuleDeclaration |
            SyntaxKind::EnumDeclaration => {
                self.generate_name_for_module_or_enum(
                    node
                )
            }
            SyntaxKind::ImportDeclaration |
            SyntaxKind::ExportDeclaration => {
                self.generate_name_for_import_or_export_declaration(
                    node
                )
            }
            SyntaxKind::FunctionDeclaration |
            SyntaxKind::ClassDeclaration |
            SyntaxKind::ExportAssignment => {
                self.generate_name_for_export_default()
            }
            SyntaxKind::ClassExpression => {
                self.generate_name_for_class_expression()
            }
            SyntaxKind::MethodDeclaration |
            SyntaxKind::GetAccessor |
            SyntaxKind::SetAccessor => {
                self.generate_name_for_method_or_accessor(node)
            }
            SyntaxKind::ComputedPropertyName => {
                self.make_temp_variable_name(
                    TempFlags::Auto,
                    Some(true)
                )
            }
            _ => {
                self.make_temp_variable_name(
                    TempFlags::Auto,
                    None,
                )
            }
        }
    }

    pub(super) fn make_name(&self, name: Id<Node> /*GeneratedIdentifier*/) -> String {
        let name_auto_generate_flags = name.ref_(self).as_identifier().auto_generate_flags();
        match name_auto_generate_flags & GeneratedIdentifierFlags::KindMask {
            GeneratedIdentifierFlags::Auto => self.make_temp_variable_name(
                TempFlags::Auto,
                Some(
                    name_auto_generate_flags
                        .intersects(GeneratedIdentifierFlags::ReservedInNestedScopes),
                ),
            ),
            GeneratedIdentifierFlags::Loop => self.make_temp_variable_name(
                TempFlags::_I,
                Some(
                    name_auto_generate_flags
                        .intersects(GeneratedIdentifierFlags::ReservedInNestedScopes),
                ),
            ),
            GeneratedIdentifierFlags::Unique => self.make_unique_name(
                &id_text(&name.ref_(self)),
                Some(|name: &str| {
                    if name_auto_generate_flags.intersects(GeneratedIdentifierFlags::FileLevel) {
                        self.is_file_level_unique_name(name)
                    } else {
                        self.is_unique_name(name)
                    }
                }),
                Some(name_auto_generate_flags.intersects(GeneratedIdentifierFlags::Optimistic)),
                Some(
                    name_auto_generate_flags
                        .intersects(GeneratedIdentifierFlags::ReservedInNestedScopes),
                ),
            ),
            _ => Debug_.fail(Some("Unsupported GeneratedIdentifierKind.")),
        }
    }

    pub(super) fn get_node_for_generated_name(
        &self,
        name: Id<Node>, /*GeneratedIdentifier*/
    ) -> Id<Node> {
        let auto_generate_id = name.ref_(self).as_identifier().auto_generate_id;
        let mut node: Id<Node> = name;
        let mut original: Option<Id<Node>> = node.ref_(self).maybe_original();
        while let Some(original_present) = original {
            node = original_present;

            if is_identifier(&node.ref_(self)) && {
                let node_ref = node.ref_(self);
                let node_as_identifier = node_ref.as_identifier();
                node_as_identifier
                    .maybe_auto_generate_flags()
                    .unwrap_or_default()
                    .intersects(GeneratedIdentifierFlags::Node)
                    && node_as_identifier.auto_generate_id != auto_generate_id
            } {
                break;
            }

            original = node.ref_(self).maybe_original();
        }

        node
    }

    pub(super) fn pipeline_emit_with_comments(
        &self,
        hint: EmitHint,
        node: Id<Node>,
    ) -> io::Result<()> {
        let pipeline_phase = self.get_next_pipeline_phase(PipelinePhase::Comments, hint, node)?;
        let saved_container_pos = self.container_pos();
        let saved_container_end = self.container_end();
        let saved_declaration_list_container_end = self.declaration_list_container_end();
        self.emit_comments_before_node(node);
        pipeline_phase(self, hint, node)?;
        self.emit_comments_after_node(
            node,
            saved_container_pos,
            saved_container_end,
            saved_declaration_list_container_end,
        );

        Ok(())
    }

    pub(super) fn emit_comments_before_node(&self, node: Id<Node>) {
        let emit_flags = get_emit_flags(node, self);
        let comment_range = get_comment_range(node, self);

        self.emit_leading_comments_of_node(
            node,
            emit_flags,
            comment_range.pos(),
            comment_range.end(),
        );
        if emit_flags.intersects(EmitFlags::NoNestedComments) {
            self.set_comments_disabled(true);
        }
    }

    pub(super) fn emit_comments_after_node(
        &self,
        node: Id<Node>,
        saved_container_pos: isize,
        saved_container_end: isize,
        saved_declaration_list_container_end: isize,
    ) {
        let emit_flags = get_emit_flags(node, self);
        let comment_range = get_comment_range(node, self);

        if emit_flags.intersects(EmitFlags::NoNestedComments) {
            self.set_comments_disabled(false);
        }
        self.emit_trailing_comments_of_node(
            node,
            emit_flags,
            comment_range.pos(),
            comment_range.end(),
            saved_container_pos,
            saved_container_end,
            saved_declaration_list_container_end,
        );
    }

    pub(super) fn emit_leading_comments_of_node(
        &self,
        node: Id<Node>,
        emit_flags: EmitFlags,
        pos: isize,
        end: isize,
    ) {
        self.enter_comment();
        self.set_has_written_comment(false);

        let skip_leading_comments = pos < 0
            || emit_flags.intersects(EmitFlags::NoLeadingComments)
            || node.ref_(self).kind() == SyntaxKind::JsxText;
        let skip_trailing_comments = end < 0
            || emit_flags.intersects(EmitFlags::NoTrailingComments)
            || node.ref_(self).kind() == SyntaxKind::JsxText;

        if (pos > 0 || end > 0) && pos != end {
            if !skip_leading_comments {
                self.emit_leading_comments(
                    pos,
                    node.ref_(self).kind() != SyntaxKind::NotEmittedStatement,
                );
            }

            if !skip_leading_comments
                || pos >= 0 && emit_flags.intersects(EmitFlags::NoLeadingComments)
            {
                self.set_container_pos(pos);
            }

            if !skip_trailing_comments
                || end >= 0 && emit_flags.intersects(EmitFlags::NoTrailingComments)
            {
                self.set_container_end(end);

                if node.ref_(self).kind() == SyntaxKind::VariableDeclarationList {
                    self.set_declaration_list_container_end(end);
                }
            }
        }
        maybe_for_each(
            get_synthetic_leading_comments(node, self).as_ref(),
            |comment: &Rc<SynthesizedComment>, _| -> Option<()> {
                self.emit_leading_synthesized_comment(comment);
                None
            },
        );
        self.exit_comment();
    }

    pub(super) fn emit_trailing_comments_of_node(
        &self,
        node: Id<Node>,
        emit_flags: EmitFlags,
        pos: isize,
        end: isize,
        saved_container_pos: isize,
        saved_container_end: isize,
        saved_declaration_list_container_end: isize,
    ) {
        self.enter_comment();
        let skip_trailing_comments = end < 0
            || emit_flags.intersects(EmitFlags::NoTrailingComments)
            || node.ref_(self).kind() == SyntaxKind::JsxText;
        maybe_for_each(
            get_synthetic_trailing_comments(node, self).as_ref(),
            |comment: &Rc<SynthesizedComment>, _| -> Option<()> {
                self.emit_trailing_synthesized_comment(comment);
                None
            },
        );
        if (pos > 0 || end > 0) && pos != end {
            self.set_container_pos(saved_container_pos);
            self.set_container_end(saved_container_end);
            self.set_declaration_list_container_end(saved_declaration_list_container_end);

            if !skip_trailing_comments && node.ref_(self).kind() != SyntaxKind::NotEmittedStatement
            {
                self.emit_trailing_comments(end);
            }
        }
        self.exit_comment();
    }

    pub(super) fn emit_leading_synthesized_comment(&self, comment: &SynthesizedComment) {
        if comment.has_leading_new_line == Some(true)
            || comment.kind == SyntaxKind::SingleLineCommentTrivia
        {
            self.writer().write_line(None);
        }
        self.write_synthesized_comment(comment);
        if comment.has_trailing_new_line == Some(true)
            || comment.kind == SyntaxKind::SingleLineCommentTrivia
        {
            self.writer().write_line(None);
        } else {
            self.writer().write_space(" ");
        }
    }

    pub(super) fn emit_trailing_synthesized_comment(&self, comment: &SynthesizedComment) {
        if !self.writer().is_at_start_of_line() {
            self.writer().write_space(" ");
        }
        self.write_synthesized_comment(comment);
        if comment.has_trailing_new_line == Some(true) {
            self.writer().write_line(None);
        }
    }

    pub(super) fn write_synthesized_comment(&self, comment: &SynthesizedComment) {
        let ref text = self.format_synthesized_comment(comment);
        let ref text_as_chars = text.chars().collect::<Vec<_>>();
        let line_map = if comment.kind == SyntaxKind::MultiLineCommentTrivia {
            Some(compute_line_starts(text_as_chars))
        } else {
            None
        };
        write_comment_range(
            text_as_chars,
            line_map.as_ref().unwrap(),
            &**self.writer(),
            0,
            text_as_chars.len(),
            &self.new_line,
        );
    }

    pub(super) fn format_synthesized_comment(&self, comment: &SynthesizedComment) -> String {
        if comment.kind == SyntaxKind::MultiLineCommentTrivia {
            format!("/*{}*/", comment.text)
        } else {
            format!("//{}", comment.text)
        }
    }

    #[allow(dead_code)]
    pub(super) fn emit_body_with_detached_comments(
        &self,
        node: Id<Node>,
        detached_range: &impl ReadonlyTextRange,
        mut emit_callback: impl FnMut(Id<Node>),
    ) {
        self.try_emit_body_with_detached_comments(node, detached_range, |a| Ok(emit_callback(a)))
            .unwrap()
    }

    pub(super) fn try_emit_body_with_detached_comments(
        &self,
        node: Id<Node>,
        detached_range: &impl ReadonlyTextRange,
        mut emit_callback: impl FnMut(Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        self.enter_comment();
        let pos = detached_range.pos();
        let end = detached_range.end();
        let emit_flags = get_emit_flags(node, self);
        let skip_leading_comments = pos < 0 || emit_flags.intersects(EmitFlags::NoLeadingComments);
        let skip_trailing_comments = self.comments_disabled()
            || end < 0
            || emit_flags.intersects(EmitFlags::NoTrailingComments);
        if !skip_leading_comments {
            self.emit_detached_comments_and_update_comments_info(detached_range);
        }

        self.exit_comment();
        if emit_flags.intersects(EmitFlags::NoNestedComments) && !self.comments_disabled() {
            self.set_comments_disabled(true);
            emit_callback(node)?;
            self.set_comments_disabled(false);
        } else {
            emit_callback(node)?;
        }

        self.enter_comment();
        if !skip_trailing_comments {
            self.emit_leading_comments(detached_range.end(), true);
            if self.has_written_comment() && !self.writer().is_at_start_of_line() {
                self.writer().write_line(None);
            }
        }
        self.exit_comment();

        Ok(())
    }

    pub(super) fn original_nodes_have_same_parent(
        &self,
        node_a: Id<Node>,
        node_b: Id<Node>,
    ) -> bool {
        let ref node_a = get_original_node(node_a, self);
        matches!(
            node_a.ref_(self).maybe_parent(),
            Some(node_a_parent) if matches!(
                get_original_node(
                    node_b,
                    self,
                ).ref_(self).maybe_parent(),
                Some(node_b_parent) if node_a_parent == node_b_parent
            )
        )
    }

    pub(super) fn sibling_node_positions_are_comparable(
        &self,
        previous_node: Id<Node>,
        next_node: Id<Node>,
    ) -> bool {
        if next_node.ref_(self).pos() < previous_node.ref_(self).end() {
            return false;
        }

        let previous_node = get_original_node(previous_node, self);
        let next_node = get_original_node(next_node, self);
        let parent = previous_node.ref_(self).maybe_parent();
        if match parent {
            None => true,
            Some(parent) => !matches!(
                next_node.ref_(self).maybe_parent(),
                Some(next_node_parent) if parent == next_node_parent
            ),
        } {
            return false;
        }

        let parent_node_array = get_containing_node_array(previous_node);
        let prev_node_index = parent_node_array.as_ref().and_then(|parent_node_array| {
            parent_node_array
                .ref_(self)
                .into_iter()
                .position(|&node| node == previous_node)
        });
        matches!(
            prev_node_index,
            Some(prev_node_index) if matches!(
                parent_node_array.and_then(|parent_node_array| {
                    parent_node_array.ref_(self).into_iter().position(|&node| node == next_node)
                }),
                Some(value) if value == prev_node_index + 1
            )
        )
    }

    pub(super) fn emit_leading_comments(&self, pos: isize, is_emitted_node: bool) {
        self.set_has_written_comment(false);

        if is_emitted_node {
            if pos == 0
                && matches!(
                    self.maybe_current_source_file(),
                    Some(current_source_file) if current_source_file.ref_(self).as_source_file().is_declaration_file()
                )
            {
                self.for_each_leading_comment_to_emit(
                    pos,
                    |comment_pos, comment_end, kind, has_trailing_new_line, range_pos| {
                        self.emit_non_triple_slash_leading_comment(
                            comment_pos,
                            comment_end,
                            kind,
                            has_trailing_new_line,
                            range_pos,
                        )
                    },
                );
            } else {
                self.for_each_leading_comment_to_emit(
                    pos,
                    |comment_pos, comment_end, kind, has_trailing_new_line, range_pos| {
                        self.emit_leading_comment(
                            comment_pos,
                            comment_end,
                            kind,
                            has_trailing_new_line,
                            range_pos,
                        )
                    },
                );
            }
        } else if pos == 0 {
            self.for_each_leading_comment_to_emit(
                pos,
                |comment_pos, comment_end, kind, has_trailing_new_line, range_pos| {
                    self.emit_triple_slash_leading_comment(
                        comment_pos,
                        comment_end,
                        kind,
                        has_trailing_new_line,
                        range_pos,
                    )
                },
            );
        }
    }

    pub(super) fn emit_triple_slash_leading_comment(
        &self,
        comment_pos: isize,
        comment_end: isize,
        kind: SyntaxKind,
        has_trailing_new_line: bool,
        range_pos: isize,
    ) {
        if self.is_triple_slash_comment(comment_pos, comment_end) {
            self.emit_leading_comment(
                comment_pos,
                comment_end,
                kind,
                has_trailing_new_line,
                range_pos,
            );
        }
    }

    pub(super) fn emit_non_triple_slash_leading_comment(
        &self,
        comment_pos: isize,
        comment_end: isize,
        kind: SyntaxKind,
        has_trailing_new_line: bool,
        range_pos: isize,
    ) {
        if !self.is_triple_slash_comment(comment_pos, comment_end) {
            self.emit_leading_comment(
                comment_pos,
                comment_end,
                kind,
                has_trailing_new_line,
                range_pos,
            );
        }
    }

    pub(super) fn should_write_comment(&self, text: &SourceTextAsChars, pos: isize) -> bool {
        if self.printer_options.only_print_js_doc_style == Some(true) {
            return is_jsdoc_like_text(text, pos.try_into().unwrap())
                || is_pinned_comment(text, pos.try_into().unwrap());
        }
        true
    }

    pub(super) fn emit_leading_comment(
        &self,
        comment_pos: isize,
        comment_end: isize,
        kind: SyntaxKind,
        has_trailing_new_line: bool,
        range_pos: isize,
    ) {
        if !self.should_write_comment(
            &self
                .current_source_file()
                .ref_(self)
                .as_source_file()
                .text_as_chars(),
            comment_pos,
        ) {
            return;
        }
        if !self.has_written_comment() {
            emit_new_line_before_leading_comment_of_position(
                &self.get_current_line_map(),
                &**self.writer(),
                range_pos,
                comment_pos,
            );
            self.set_has_written_comment(true);
        }

        self.emit_pos(comment_pos);
        write_comment_range(
            &self
                .current_source_file()
                .ref_(self)
                .as_source_file()
                .text_as_chars(),
            &self.get_current_line_map(),
            &**self.writer(),
            comment_pos.try_into().unwrap(),
            comment_end.try_into().unwrap(),
            &self.new_line,
        );
        self.emit_pos(comment_end);

        if has_trailing_new_line {
            self.writer().write_line(None);
        } else if kind == SyntaxKind::MultiLineCommentTrivia {
            self.writer().write_space(" ");
        }
    }

    pub(super) fn emit_leading_comments_of_position(&self, pos: isize) {
        if self.comments_disabled() || pos == -1 {
            return;
        }

        self.emit_leading_comments(pos, true);
    }

    pub(super) fn emit_trailing_comments(&self, pos: isize) {
        self.for_each_trailing_comment_to_emit(
            pos,
            |comment_pos, comment_end, kind, has_trailing_new_line| {
                self.emit_trailing_comment(comment_pos, comment_end, kind, has_trailing_new_line)
            },
        );
    }
}
