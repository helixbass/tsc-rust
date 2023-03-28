use gc::Gc;
use regex::Regex;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashSet;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::TempFlags;
use crate::{
    are_option_gcs_equal, are_option_rcs_equal, escape_jsx_attribute_string,
    escape_non_ascii_string, escape_string, for_each, get_emit_flags,
    get_lines_between_position_and_next_non_whitespace_character,
    get_lines_between_position_and_preceding_non_whitespace_character,
    get_lines_between_range_end_and_range_start, get_literal_text, get_original_node,
    get_source_text_of_node_from_source_file, get_starts_on_new_line, guess_indentation, id_text,
    is_binding_pattern, is_generated_identifier, is_identifier, is_literal_expression,
    is_numeric_literal, is_private_identifier, last_or_undefined, maybe_get_source_file_of_node,
    node_is_synthesized, position_is_synthesized, range_end_is_on_same_line_as_range_start,
    range_end_positions_are_on_same_line, range_is_on_single_line,
    range_start_positions_are_on_same_line, token_to_string, Debug_, EmitFlags,
    FunctionLikeDeclarationInterface, GeneratedIdentifierFlags, GetLiteralTextFlags, ListFormat,
    LiteralLikeNodeInterface, NamedDeclarationInterface, Node, NodeArray, NodeInterface, Printer,
    ReadonlyTextRange, ScriptTarget, SignatureDeclarationInterface, SyntaxKind,
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
        children: &[Gc<Node>],
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
                        Some(first_child_parent) => are_option_gcs_equal(
                            get_original_node(
                                Some(&**first_child_parent),
                                Option::<fn(Option<Gc<Node>>) -> bool>::None,
                            )
                            .as_ref(),
                            get_original_node(
                                Some(parent_node),
                                Option::<fn(Option<Gc<Node>>) -> bool>::None,
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
        if get_emit_flags(parent).intersects(EmitFlags::NoIndentation) {
            return 0;
        }

        let ref parent = self.skip_synthesized_parentheses(parent);
        let ref node1 = self.skip_synthesized_parentheses(node1);
        let ref node2 = self.skip_synthesized_parentheses(node2);

        if get_starts_on_new_line(node2) == Some(true) {
            return 1;
        }

        if !node_is_synthesized(&**parent)
            && !node_is_synthesized(&**node1)
            && !node_is_synthesized(&**node2)
        {
            if self.maybe_preserve_source_newlines() == Some(true) {
                return self.get_effective_lines(|include_comments| {
                    get_lines_between_range_end_and_range_start(
                        &**node1,
                        &**node2,
                        &self.current_source_file(),
                        include_comments,
                    )
                });
            }
            return if range_end_is_on_same_line_as_range_start(
                &**node1,
                &**node2,
                &self.current_source_file(),
            ) {
                0
            } else {
                1
            };
        }

        0
    }

    pub(super) fn is_empty_block(&self, block: &Node /*BlockLike*/) -> bool {
        block.as_has_statements().statements().is_empty()
            && range_end_is_on_same_line_as_range_start(block, block, &self.current_source_file())
    }

    pub(super) fn skip_synthesized_parentheses(&self, node: &Node) -> Gc<Node> {
        let mut node = node.node_wrapper();
        while node.kind() == SyntaxKind::ParenthesizedExpression && node_is_synthesized(&*node) {
            node = node.as_parenthesized_expression().expression.clone();
        }

        node
    }

    pub(super) fn get_text_of_node<'node>(
        &self,
        node: &'node Node,
        include_trivia: Option<bool>,
    ) -> Cow<'node, str> {
        if is_generated_identifier(node) {
            return self.generate_name(node).into();
        } else if (is_identifier(node) || is_private_identifier(node))
            && (node_is_synthesized(node)
                || node.maybe_parent().is_none()
                || self.maybe_current_source_file().is_none()
                || node.maybe_parent().is_some()
                    && matches!(
                        self.maybe_current_source_file().as_ref(),
                        Some(current_source_file) if !are_option_gcs_equal(
                            maybe_get_source_file_of_node(Some(node)).as_ref(),
                            get_original_node(
                                Some(&**current_source_file),
                                Option::<fn(Option<Gc<Node>>) -> bool>::None
                            ).as_ref()
                        )
                    ))
        {
            return id_text(node).into();
        } else if node.kind() == SyntaxKind::StringLiteral
            && node.as_string_literal().text_source_node.is_some()
        {
            return self.get_text_of_node(
                node.as_string_literal().text_source_node.as_ref().unwrap(),
                include_trivia,
            );
        } else if is_literal_expression(node)
            && (node_is_synthesized(node) || node.maybe_parent().is_none())
        {
            return node.as_literal_like_node().text().clone().into();
        }

        get_source_text_of_node_from_source_file(&self.current_source_file(), node, include_trivia)
    }

    pub(super) fn get_literal_text_of_node(
        &self,
        node: &Node,
        never_ascii_escape: Option<bool>,
        jsx_attribute_escape: bool,
    ) -> Cow<'static, str> {
        if node.kind() == SyntaxKind::StringLiteral {
            if let Some(text_source_node) = node.as_string_literal().text_source_node.as_ref() {
                if is_identifier(text_source_node) || is_numeric_literal(text_source_node) {
                    let text = if is_numeric_literal(text_source_node) {
                        text_source_node.as_numeric_literal().text().clone().into()
                    } else {
                        self.get_text_of_node(text_source_node, None)
                    };
                    return if jsx_attribute_escape {
                        escape_jsx_attribute_string(&text, None).into_owned().into()
                    } else if never_ascii_escape == Some(true)
                        || get_emit_flags(node).intersects(EmitFlags::NoAsciiEscaping)
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

        get_literal_text(node, self.maybe_current_source_file(), flags)
    }

    pub(super) fn push_name_generation_scope(&self, node: Option<&Node>) {
        if matches!(
            node,
            Some(node) if get_emit_flags(node).intersects(EmitFlags::ReuseTempVariableScope)
        ) {
            return;
        }
        self.temp_flags_stack_mut().push(self.temp_flags());
        self.set_temp_flags(TempFlags::Auto);
        self.reserved_names_stack_mut()
            .push(self.maybe_reserved_names());
    }

    pub(super) fn pop_name_generation_scope(&self, node: Option<&Node>) {
        if matches!(
            node,
            Some(node) if get_emit_flags(node).intersects(EmitFlags::ReuseTempVariableScope)
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

    pub(super) fn generate_names(&self, node: Option<&Node>) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        match node.kind() {
            SyntaxKind::Block => {
                for_each(
                    &node.as_block().statements,
                    |statement: &Gc<Node>, _| -> Option<()> {
                        self.generate_names(Some(statement));
                        None
                    },
                );
            }
            SyntaxKind::LabeledStatement
            | SyntaxKind::WithStatement
            | SyntaxKind::DoStatement
            | SyntaxKind::WhileStatement => {
                self.generate_names(Some(&node.as_has_statement().statement()));
            }
            SyntaxKind::IfStatement => {
                let node_as_if_statement = node.as_if_statement();
                self.generate_names(Some(&node_as_if_statement.then_statement));
                self.generate_names(node_as_if_statement.else_statement.as_deref());
            }
            SyntaxKind::ForStatement | SyntaxKind::ForOfStatement | SyntaxKind::ForInStatement => {
                self.generate_names(node.as_has_initializer().maybe_initializer().as_deref());
                self.generate_names(Some(&node.as_has_statement().statement()));
            }
            SyntaxKind::SwitchStatement => {
                self.generate_names(Some(&node.as_switch_statement().case_block));
            }
            SyntaxKind::CaseBlock => {
                for_each(
                    &node.as_case_block().clauses,
                    |clause: &Gc<Node>, _| -> Option<()> {
                        self.generate_names(Some(clause));
                        None
                    },
                );
            }
            SyntaxKind::CaseClause | SyntaxKind::DefaultClause => {
                for_each(
                    node.as_has_statements().statements(),
                    |statement: &Gc<Node>, _| -> Option<()> {
                        self.generate_names(Some(statement));
                        None
                    },
                );
            }
            SyntaxKind::TryStatement => {
                let node_as_try_statement = node.as_try_statement();
                self.generate_names(Some(&node_as_try_statement.try_block));
                self.generate_names(node_as_try_statement.catch_clause.as_deref());
                self.generate_names(node_as_try_statement.finally_block.as_deref());
            }
            SyntaxKind::CatchClause => {
                let node_as_catch_clause = node.as_catch_clause();
                self.generate_names(node_as_catch_clause.variable_declaration.as_deref());
                self.generate_names(Some(&node_as_catch_clause.block));
            }
            SyntaxKind::VariableStatement => {
                self.generate_names(Some(&node.as_variable_statement().declaration_list));
            }
            SyntaxKind::VariableDeclarationList => {
                for_each(
                    &node.as_variable_declaration_list().declarations,
                    |declaration: &Gc<Node>, _| -> Option<()> {
                        self.generate_names(Some(declaration));
                        None
                    },
                );
            }
            SyntaxKind::VariableDeclaration
            | SyntaxKind::Parameter
            | SyntaxKind::BindingElement
            | SyntaxKind::ClassDeclaration => {
                self.generate_name_if_needed(node.as_named_declaration().maybe_name().as_deref());
            }
            SyntaxKind::FunctionDeclaration => {
                let node_as_function_declaration = node.as_function_declaration();
                self.generate_name_if_needed(node_as_function_declaration.maybe_name().as_deref());
                if get_emit_flags(node).intersects(EmitFlags::ReuseTempVariableScope) {
                    for_each(
                        node_as_function_declaration.parameters(),
                        |parameter: &Gc<Node>, _| -> Option<()> {
                            self.generate_names(Some(parameter));
                            None
                        },
                    );
                    self.generate_names(node_as_function_declaration.maybe_body().as_deref());
                }
            }
            SyntaxKind::ObjectBindingPattern | SyntaxKind::ArrayBindingPattern => {
                for_each(
                    node.as_has_elements().elements(),
                    |element: &Gc<Node>, _| -> Option<()> {
                        self.generate_names(Some(element));
                        None
                    },
                );
            }
            SyntaxKind::ImportDeclaration => {
                self.generate_names(node.as_import_declaration().import_clause.as_deref());
            }
            SyntaxKind::ImportClause => {
                let node_as_import_clause = node.as_import_clause();
                self.generate_name_if_needed(node_as_import_clause.name.as_deref());
                self.generate_names(node_as_import_clause.named_bindings.as_deref());
            }
            SyntaxKind::NamespaceImport => {
                self.generate_name_if_needed(Some(&node.as_namespace_import().name));
            }
            SyntaxKind::NamespaceExport => {
                self.generate_name_if_needed(Some(&node.as_namespace_export().name));
            }
            SyntaxKind::NamedImports => {
                for_each(
                    &node.as_named_imports().elements,
                    |element: &Gc<Node>, _| -> Option<()> {
                        self.generate_names(Some(element));
                        None
                    },
                );
            }
            SyntaxKind::ImportSpecifier => {
                let node_as_import_specifier = node.as_import_specifier();
                self.generate_name_if_needed(Some(
                    node_as_import_specifier
                        .property_name
                        .as_deref()
                        .unwrap_or(&*node_as_import_specifier.name),
                ));
            }
            _ => (),
        }
    }

    pub(super) fn generate_member_names(&self, node: Option<&Node>) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        if matches!(
            node.kind(),
            SyntaxKind::PropertyAssignment
                | SyntaxKind::ShorthandPropertyAssignment
                | SyntaxKind::PropertyDeclaration
                | SyntaxKind::MethodDeclaration
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor
        ) {
            self.generate_name_if_needed(node.as_named_declaration().maybe_name().as_deref())
        }
    }

    pub(super) fn generate_name_if_needed(&self, name: Option<&Node /*DeclarationName*/>) {
        if let Some(name) = name {
            if is_generated_identifier(name) {
                self.generate_name(name);
            } else if is_binding_pattern(Some(name)) {
                self.generate_names(Some(name));
            }
        }
    }

    pub(super) fn generate_name(&self, name: &Node /*GeneratedIdentifier*/) -> String {
        let name_as_identifier = name.as_identifier();
        if name_as_identifier.auto_generate_flags.unwrap() & GeneratedIdentifierFlags::KindMask
            == GeneratedIdentifierFlags::Node
        {
            self.generate_name_cached(
                &self.get_node_for_generated_name(name),
                name_as_identifier.auto_generate_flags,
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

#[derive(Copy, Clone)]
pub enum RefNodeArrayOrSlice<'a> {
    NodeArray(&'a NodeArray),
    Slice(&'a [Gc<Node>]),
}

impl<'a> RefNodeArrayOrSlice<'a> {
    pub fn as_slice(&'a self) -> &'a [Gc<Node>] {
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

impl<'a> From<&'a [Gc<Node>]> for RefNodeArrayOrSlice<'a> {
    fn from(value: &'a [Gc<Node>]) -> Self {
        Self::Slice(value)
    }
}
