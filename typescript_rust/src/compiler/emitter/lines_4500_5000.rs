use std::{borrow::Cow, cell::RefCell, collections::HashSet, convert::TryInto, ptr, rc::Rc};

use id_arena::Id;
use regex::Regex;

use super::TempFlags;
use crate::{
    escape_jsx_attribute_string, escape_non_ascii_string, escape_string, for_each, get_emit_flags,
    get_lines_between_position_and_next_non_whitespace_character,
    get_lines_between_position_and_preceding_non_whitespace_character,
    get_lines_between_range_end_and_range_start, get_literal_text, get_original_node,
    get_source_file_of_node, get_source_text_of_node_from_source_file, get_starts_on_new_line,
    guess_indentation, id_text, is_binding_pattern, is_generated_identifier, is_identifier,
    is_literal_expression, is_numeric_literal, is_private_identifier, last_or_undefined,
    node_is_synthesized, position_is_synthesized, range_end_is_on_same_line_as_range_start,
    range_end_positions_are_on_same_line, range_is_on_single_line,
    range_start_positions_are_on_same_line, token_to_string, Debug_, EmitFlags,
    FunctionLikeDeclarationInterface, GeneratedIdentifierFlags, GetLiteralTextFlags, ListFormat,
    LiteralLikeNodeInterface, NamedDeclarationInterface, Node, NodeArray, NodeInterface, Printer,
    ReadonlyTextRange, ScriptTarget, SignatureDeclarationInterface, SyntaxKind,
    InArena,
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

    pub(super) fn write_token(
        &self,
        token: SyntaxKind,
        pos: isize,
        writer: impl FnMut(&str),
        context_node: Option<Id<Node>>,
    ) -> Option<isize> {
        if !self.source_maps_disabled() {
            Some(self.emit_token_with_source_map(
                context_node,
                token,
                writer,
                pos,
                |token, writer, pos| self.write_token_text(token, writer, Some(pos)).unwrap(),
            ))
        } else {
            self.write_token_text(token, writer, Some(pos))
        }
    }

    pub(super) fn write_token_node(&self, node: Id<Node>, writer: fn(&Printer, &str)) {
        self.on_before_emit_token(Some(node));
        writer(self, token_to_string(node.ref_(self).kind()).unwrap());
        self.on_after_emit_token(Some(node));
    }

    pub(super) fn write_token_text(
        &self,
        token: SyntaxKind,
        mut writer: impl FnMut(&str),
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
        parent_node: Id<Node>,
        prev_child_node: Id<Node>,
        next_child_node: Id<Node>,
    ) {
        if get_emit_flags(parent_node, self).intersects(EmitFlags::SingleLine) {
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
                    if indentation < line_text.len() {
                        &line_text[indentation..]
                    } else {
                        line_text
                    }
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
        parent_node: Option<Id<Node>>,
        children: &[Id<Node>],
        format: ListFormat,
    ) -> usize {
        if format.intersects(ListFormat::PreserveLines)
            || self.maybe_preserve_source_newlines() == Some(true)
        {
            if format.intersects(ListFormat::PreferNewLine) {
                return 1;
            }

            let Some(first_child) = children.get(0).copied() else {
                return if match parent_node {
                    None => true,
                    Some(parent_node) => {
                        range_is_on_single_line(&*parent_node.ref_(self), &self.current_source_file().ref_(self))
                    }
                } {
                    0
                } else {
                    1
                };
            };
            if Some(first_child.ref_(self).pos()) == self.maybe_next_list_element_pos() {
                return 0;
            }
            if first_child.ref_(self).kind() == SyntaxKind::JsxText {
                return 0;
            }
            if let Some(parent_node) = parent_node.filter(|&parent_node| {
                !position_is_synthesized(parent_node.ref_(self).pos())
                    && !node_is_synthesized(&*first_child.ref_(self))
                    && match first_child.ref_(self).maybe_parent() {
                        None => true,
                        Some(first_child_parent) => get_original_node(first_child_parent, self) == get_original_node(parent_node, self),
                    }
            }) {
                if self.maybe_preserve_source_newlines() == Some(true) {
                    return self.get_effective_lines(|include_comments| {
                        get_lines_between_position_and_preceding_non_whitespace_character(
                            first_child.ref_(self).pos(),
                            parent_node.ref_(self).pos(),
                            self.current_source_file(),
                            Some(include_comments),
                        )
                    });
                }
                return if range_start_positions_are_on_same_line(
                    &*parent_node.ref_(self),
                    &*first_child.ref_(self),
                    &self.current_source_file().ref_(self),
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
        previous_node: Option<Id<Node>>,
        next_node: Id<Node>,
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
            if next_node.ref_(self).kind() == SyntaxKind::JsxText {
                return 0;
            } else if !node_is_synthesized(&*previous_node.ref_(self)) && !node_is_synthesized(&*next_node.ref_(self)) {
                if self.maybe_preserve_source_newlines() == Some(true)
                    && self.sibling_node_positions_are_comparable(previous_node, next_node)
                {
                    return self.get_effective_lines(|include_comments| {
                        get_lines_between_range_end_and_range_start(
                            &*previous_node.ref_(self),
                            &*next_node.ref_(self),
                            self.current_source_file(),
                            include_comments,
                        )
                    });
                } else if self.maybe_preserve_source_newlines() != Some(true)
                    && self.original_nodes_have_same_parent(previous_node, next_node)
                {
                    return if range_end_is_on_same_line_as_range_start(
                        &*previous_node.ref_(self),
                        &*next_node.ref_(self),
                        &self.current_source_file().ref_(self),
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
        } else if get_starts_on_new_line(next_node, self) == Some(true) {
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
        parent_node: Option<Id<Node>>,
        children: NodeArrayOrSlice,
        format: ListFormat,
    ) -> usize {
        if format.intersects(ListFormat::PreserveLines)
            || self.maybe_preserve_source_newlines() == Some(true)
        {
            if format.intersects(ListFormat::PreferNewLine) {
                return 1;
            }

            let last_child = last_or_undefined(children.as_slice()).copied();
            if last_child.is_none() {
                return if match parent_node {
                    None => true,
                    Some(parent_node) => {
                        range_is_on_single_line(&*parent_node.ref_(self), &self.current_source_file().ref_(self))
                    }
                } {
                    0
                } else {
                    1
                };
            }
            let last_child = last_child.unwrap();
            if let Some(parent_node) = parent_node.filter(|&parent_node| {
                !position_is_synthesized(parent_node.ref_(self).pos())
                    && !node_is_synthesized(&*last_child.ref_(self))
                    && match last_child.ref_(self).maybe_parent() {
                        None => true,
                        Some(last_child_parent) => last_child_parent == parent_node,
                    }
            }) {
                if self.maybe_preserve_source_newlines() == Some(true) {
                    let end = if let NodeArrayOrSlice::NodeArray(children) = &children {
                        if !position_is_synthesized(children.end()) {
                            children.end()
                        } else {
                            last_child.ref_(self).end()
                        }
                    } else {
                        last_child.ref_(self).end()
                    };
                    return self.get_effective_lines(|include_comments| {
                        get_lines_between_position_and_next_non_whitespace_character(
                            end,
                            parent_node.ref_(self).end(),
                            self.current_source_file(),
                            Some(include_comments),
                        )
                    });
                }
                return if range_end_positions_are_on_same_line(
                    &*parent_node.ref_(self),
                    &*last_child.ref_(self),
                    &self.current_source_file().ref_(self),
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
        node: Id<Node>,
        parent: Id<Node>,
    ) -> bool {
        let leading_newlines = if self.maybe_preserve_source_newlines() == Some(true) {
            self.get_leading_line_terminator_count(
                Some(parent),
                &[node],
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

    pub(super) fn write_line_separators_after(&self, node: Id<Node>, parent: Id<Node>) {
        let trailing_newlines = if self.maybe_preserve_source_newlines() == Some(true) {
            self.get_closing_line_terminator_count(
                Some(parent),
                // (&[node.node_wrapper()]).into(),
                NodeArrayOrSlice::Slice(&[node]),
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
        node: Id<Node>,
        format: ListFormat,
    ) -> bool {
        if node_is_synthesized(&*node.ref_(self)) {
            let Some(starts_on_new_line) = get_starts_on_new_line(node, self) else {
                return format.intersects(ListFormat::PreferNewLine);
            };

            return starts_on_new_line;
        }

        format.intersects(ListFormat::PreferNewLine)
    }

    pub(super) fn get_lines_between_nodes(
        &self,
        parent: Id<Node>,
        node1: Id<Node>,
        node2: Id<Node>,
    ) -> usize {
        if get_emit_flags(parent, self).intersects(EmitFlags::NoIndentation) {
            return 0;
        }

        let ref parent = self.skip_synthesized_parentheses(parent);
        let node1 = self.skip_synthesized_parentheses(node1);
        let node2 = self.skip_synthesized_parentheses(node2);

        if get_starts_on_new_line(node2, self) == Some(true) {
            return 1;
        }

        if !node_is_synthesized(&*parent.ref_(self))
            && !node_is_synthesized(&*node1.ref_(self))
            && !node_is_synthesized(&*node2.ref_(self))
        {
            if self.maybe_preserve_source_newlines() == Some(true) {
                return self.get_effective_lines(|include_comments| {
                    get_lines_between_range_end_and_range_start(
                        &*node1.ref_(self),
                        &*node2.ref_(self),
                        self.current_source_file(),
                        include_comments,
                    )
                });
            }
            return if range_end_is_on_same_line_as_range_start(
                &*node1.ref_(self),
                &*node2.ref_(self),
                &self.current_source_file().ref_(self),
            ) {
                0
            } else {
                1
            };
        }

        0
    }

    pub(super) fn is_empty_block(&self, block: Id<Node> /*BlockLike*/) -> bool {
        block.ref_(self).as_has_statements().statements().ref_(self).is_empty()
            && range_end_is_on_same_line_as_range_start(&*block.ref_(self), &*block.ref_(self), &self.current_source_file().ref_(self))
    }

    pub(super) fn skip_synthesized_parentheses(&self, mut node: Id<Node>) -> Id<Node> {
        while node.ref_(self).kind() == SyntaxKind::ParenthesizedExpression && node_is_synthesized(&*node.ref_(self)) {
            node = node.ref_(self).as_parenthesized_expression().expression;
        }

        node
    }

    pub(super) fn get_text_of_node(
        &self,
        node: Id<Node>,
        include_trivia: Option<bool>,
    ) -> Cow<'_, str> {
        if is_generated_identifier(&node.ref_(self)) {
            return self.generate_name(node).into();
        } else if (is_identifier(&node.ref_(self)) || is_private_identifier(&node.ref_(self)))
            && (node_is_synthesized(&*node.ref_(self))
                || node.ref_(self).maybe_parent().is_none()
                || self.maybe_current_source_file().is_none()
                || node.ref_(self).maybe_parent().is_some()
                    && matches!(
                        self.maybe_current_source_file(),
                        Some(current_source_file) if get_source_file_of_node(node, self) !=
                            get_original_node(
                                current_source_file,
                                self,
                            )
                    ))
        {
            return id_text(&*node.ref_(self)).to_owned().into();
        } else if node.ref_(self).kind() == SyntaxKind::StringLiteral
            && node.ref_(self).as_string_literal().text_source_node.is_some()
        {
            return self.get_text_of_node(
                node.ref_(self).as_string_literal().text_source_node.unwrap(),
                include_trivia,
            );
        } else if is_literal_expression(&node.ref_(self))
            && (node_is_synthesized(&*node.ref_(self)) || node.ref_(self).maybe_parent().is_none())
        {
            return node.ref_(self).as_literal_like_node().text().clone().into();
        }

        get_source_text_of_node_from_source_file(self.current_source_file(), node, include_trivia, self)
    }

    pub(super) fn get_literal_text_of_node(
        &self,
        node: Id<Node>,
        never_ascii_escape: Option<bool>,
        jsx_attribute_escape: bool,
    ) -> Cow<'static, str> {
        if node.ref_(self).kind() == SyntaxKind::StringLiteral {
            if let Some(text_source_node) = node.ref_(self).as_string_literal().text_source_node {
                if is_identifier(&text_source_node.ref_(self)) || is_numeric_literal(&text_source_node.ref_(self)) {
                    let text = if is_numeric_literal(&text_source_node.ref_(self)) {
                        text_source_node.ref_(self).as_numeric_literal().text().clone().into()
                    } else {
                        self.get_text_of_node(text_source_node, None)
                    };
                    return if jsx_attribute_escape {
                        escape_jsx_attribute_string(&text, None).into_owned().into()
                    } else if never_ascii_escape == Some(true)
                        || get_emit_flags(node, self).intersects(EmitFlags::NoAsciiEscaping)
                    {
                        format!("\"{}\"", escape_string(&text, None,)).into()
                    } else {
                        format!("\"{}\"", escape_non_ascii_string(&text, None,)).into()
                    };
                } else {
                    return self.get_literal_text_of_node(
                        text_source_node,
                        never_ascii_escape,
                        jsx_attribute_escape,
                    );
                }
            }
        }

        let flags = if never_ascii_escape == Some(true) {
            GetLiteralTextFlags::NeverAsciiEscape
        } else {
            GetLiteralTextFlags::None
        } | if jsx_attribute_escape {
            GetLiteralTextFlags::JsxAttributeEscape
        } else {
            GetLiteralTextFlags::None
        } | if self.printer_options.terminate_unterminated_literals == Some(true) {
            GetLiteralTextFlags::TerminateUnterminatedLiterals
        } else {
            GetLiteralTextFlags::None
        } | if matches!(
            self.printer_options.target,
            Some(printer_options_target) if printer_options_target == ScriptTarget::ESNext
        ) {
            GetLiteralTextFlags::AllowNumericSeparator
        } else {
            GetLiteralTextFlags::None
        };

        get_literal_text(node, self.maybe_current_source_file(), flags, self)
    }

    pub(super) fn push_name_generation_scope(&self, node: Option<Id<Node>>) {
        if matches!(
            node,
            Some(node) if get_emit_flags(node, self).intersects(EmitFlags::ReuseTempVariableScope)
        ) {
            return;
        }
        self.temp_flags_stack_mut().push(self.temp_flags());
        self.set_temp_flags(TempFlags::Auto);
        self.reserved_names_stack_mut()
            .push(self.maybe_reserved_names());
    }

    pub(super) fn pop_name_generation_scope(&self, node: Option<Id<Node>>) {
        if matches!(
            node,
            Some(node) if get_emit_flags(node, self).intersects(EmitFlags::ReuseTempVariableScope)
        ) {
            return;
        }
        self.set_temp_flags(self.temp_flags_stack_mut().pop().unwrap());
        self.set_reserved_names(self.reserved_names_stack_mut().pop().unwrap());
    }

    pub(super) fn reserve_name_in_nested_scopes(&self, name: &str) {
        let mut reserved_names = self.maybe_reserved_names_mut();
        if match reserved_names.as_ref() {
            None => true,
            Some(reserved_names) => matches!(
                last_or_undefined(&**self.reserved_names_stack()).cloned().flatten().as_ref(),
                Some(value) if Rc::ptr_eq(
                    reserved_names,
                    value,
                )
            ),
        } {
            *reserved_names = Some(Rc::new(RefCell::new(HashSet::new())));
        }
        (*reserved_names.as_ref().unwrap())
            .borrow_mut()
            .insert(name.to_owned());
    }

    pub(super) fn generate_names(&self, node: Option<Id<Node>>) {
        let Some(node) = node else {
            return;
        };
        match node.ref_(self).kind() {
            SyntaxKind::Block => {
                for_each(
                    &*node.ref_(self).as_block().statements.ref_(self),
                    |&statement: &Id<Node>, _| -> Option<()> {
                        self.generate_names(Some(statement));
                        None
                    },
                );
            }
            SyntaxKind::LabeledStatement
            | SyntaxKind::WithStatement
            | SyntaxKind::DoStatement
            | SyntaxKind::WhileStatement => {
                self.generate_names(Some(node.ref_(self).as_has_statement().statement()));
            }
            SyntaxKind::IfStatement => {
                let node_ref = node.ref_(self);
                let node_as_if_statement = node_ref.as_if_statement();
                self.generate_names(Some(node_as_if_statement.then_statement));
                self.generate_names(node_as_if_statement.else_statement);
            }
            SyntaxKind::ForStatement | SyntaxKind::ForOfStatement | SyntaxKind::ForInStatement => {
                self.generate_names(node.ref_(self).as_has_initializer().maybe_initializer());
                self.generate_names(Some(node.ref_(self).as_has_statement().statement()));
            }
            SyntaxKind::SwitchStatement => {
                self.generate_names(Some(node.ref_(self).as_switch_statement().case_block));
            }
            SyntaxKind::CaseBlock => {
                for_each(
                    &*node.ref_(self).as_case_block().clauses.ref_(self),
                    |&clause: &Id<Node>, _| -> Option<()> {
                        self.generate_names(Some(clause));
                        None
                    },
                );
            }
            SyntaxKind::CaseClause | SyntaxKind::DefaultClause => {
                for_each(
                    &*node.ref_(self).as_has_statements().statements().ref_(self),
                    |&statement: &Id<Node>, _| -> Option<()> {
                        self.generate_names(Some(statement));
                        None
                    },
                );
            }
            SyntaxKind::TryStatement => {
                let node_ref = node.ref_(self);
                let node_as_try_statement = node_ref.as_try_statement();
                self.generate_names(Some(node_as_try_statement.try_block));
                self.generate_names(node_as_try_statement.catch_clause);
                self.generate_names(node_as_try_statement.finally_block);
            }
            SyntaxKind::CatchClause => {
                let node_ref = node.ref_(self);
                let node_as_catch_clause = node_ref.as_catch_clause();
                self.generate_names(node_as_catch_clause.variable_declaration);
                self.generate_names(Some(node_as_catch_clause.block));
            }
            SyntaxKind::VariableStatement => {
                self.generate_names(Some(node.ref_(self).as_variable_statement().declaration_list));
            }
            SyntaxKind::VariableDeclarationList => {
                for_each(
                    &*node.ref_(self).as_variable_declaration_list().declarations.ref_(self),
                    |&declaration: &Id<Node>, _| -> Option<()> {
                        self.generate_names(Some(declaration));
                        None
                    },
                );
            }
            SyntaxKind::VariableDeclaration
            | SyntaxKind::Parameter
            | SyntaxKind::BindingElement
            | SyntaxKind::ClassDeclaration => {
                self.generate_name_if_needed(node.ref_(self).as_named_declaration().maybe_name());
            }
            SyntaxKind::FunctionDeclaration => {
                let node_ref = node.ref_(self);
                let node_as_function_declaration = node_ref.as_function_declaration();
                self.generate_name_if_needed(node_as_function_declaration.maybe_name());
                if get_emit_flags(node, self).intersects(EmitFlags::ReuseTempVariableScope) {
                    for_each(
                        &*node_as_function_declaration.parameters().ref_(self),
                        |&parameter: &Id<Node>, _| -> Option<()> {
                            self.generate_names(Some(parameter));
                            None
                        },
                    );
                    self.generate_names(node_as_function_declaration.maybe_body());
                }
            }
            SyntaxKind::ObjectBindingPattern | SyntaxKind::ArrayBindingPattern => {
                for_each(
                    &*node.ref_(self).as_has_elements().elements().ref_(self),
                    |&element: &Id<Node>, _| -> Option<()> {
                        self.generate_names(Some(element));
                        None
                    },
                );
            }
            SyntaxKind::ImportDeclaration => {
                self.generate_names(node.ref_(self).as_import_declaration().import_clause);
            }
            SyntaxKind::ImportClause => {
                let node_ref = node.ref_(self);
                let node_as_import_clause = node_ref.as_import_clause();
                self.generate_name_if_needed(node_as_import_clause.name);
                self.generate_names(node_as_import_clause.named_bindings);
            }
            SyntaxKind::NamespaceImport => {
                self.generate_name_if_needed(Some(node.ref_(self).as_namespace_import().name));
            }
            SyntaxKind::NamespaceExport => {
                self.generate_name_if_needed(Some(node.ref_(self).as_namespace_export().name));
            }
            SyntaxKind::NamedImports => {
                for_each(
                    &*node.ref_(self).as_named_imports().elements.ref_(self),
                    |&element: &Id<Node>, _| -> Option<()> {
                        self.generate_names(Some(element));
                        None
                    },
                );
            }
            SyntaxKind::ImportSpecifier => {
                let node_ref = node.ref_(self);
                let node_as_import_specifier = node_ref.as_import_specifier();
                self.generate_name_if_needed(Some(
                    node_as_import_specifier
                        .property_name
                        .unwrap_or(node_as_import_specifier.name),
                ));
            }
            _ => (),
        }
    }

    pub(super) fn generate_member_names(&self, node: Option<Id<Node>>) {
        let Some(node) = node else {
            return;
        };
        if matches!(
            node.ref_(self).kind(),
            SyntaxKind::PropertyAssignment
                | SyntaxKind::ShorthandPropertyAssignment
                | SyntaxKind::PropertyDeclaration
                | SyntaxKind::MethodDeclaration
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor
        ) {
            self.generate_name_if_needed(node.ref_(self).as_named_declaration().maybe_name())
        }
    }

    pub(super) fn generate_name_if_needed(&self, name: Option<Id<Node> /*DeclarationName*/>) {
        if let Some(name) = name {
            if is_generated_identifier(&name.ref_(self)) {
                self.generate_name(name);
            } else if is_binding_pattern(Some(&name.ref_(self))) {
                self.generate_names(Some(name));
            }
        }
    }

    pub(super) fn generate_name(&self, name: Id<Node> /*GeneratedIdentifier*/) -> String {
        let name_ref = name.ref_(self);
        let name_as_identifier = name_ref.as_identifier();
        if name_as_identifier
            .maybe_auto_generate_flags()
            .unwrap_or_default()
            & GeneratedIdentifierFlags::KindMask
            == GeneratedIdentifierFlags::Node
        {
            self.generate_name_cached(
                self.get_node_for_generated_name(name),
                name_as_identifier.maybe_auto_generate_flags(),
            )
        } else {
            let auto_generate_id = name_as_identifier.auto_generate_id.unwrap();
            self.auto_generated_id_to_generated_name_mut()
                .entry(auto_generate_id)
                .or_insert_with(|| self.make_name(name))
                .clone()
        }
    }
}

#[derive(Clone)]
pub enum NodeArrayOrSlice<'a> {
    NodeArray(&'a NodeArray),
    Slice(&'a [Id<Node>]),
}

impl<'a> NodeArrayOrSlice<'a> {
    pub fn as_slice(&'a self) -> &'a [Id<Node>] {
        match self {
            Self::NodeArray(value) => &**value,
            Self::Slice(value) => *value,
        }
    }
}

impl<'a> From<&'a NodeArray> for NodeArrayOrSlice<'a> {
    fn from(value: &'a NodeArray) -> Self {
        Self::NodeArray(value)
    }
}

impl<'a> From<&'a [Id<Node>]> for NodeArrayOrSlice<'a> {
    fn from(value: &'a [Id<Node>]) -> Self {
        Self::Slice(value)
    }
}
