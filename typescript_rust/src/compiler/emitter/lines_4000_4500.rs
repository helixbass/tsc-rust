use std::{borrow::Borrow, collections::HashSet, convert::TryInto, io};

use gc::Gc;
use id_arena::Id;

use super::{
    get_closing_bracket, get_opening_bracket,
    ParenthesizeMemberOfElementTypeCurrentParenthesizerRule,
};
use crate::{
    get_comment_range, get_emit_flags, get_shebang, is_arrow_function, is_block,
    is_empty_statement, is_function_like, is_identifier, is_prologue_directive, is_source_file,
    is_unparsed_source, range_is_on_single_line, single_or_undefined, some, AsDoubleDeref,
    BundleFileSection, BundleFileSectionKind, CurrentParenthesizerRule, Debug_, EmitFlags,
    EmitHint, GetOrInsertDefault, HasInitializerInterface, HasStatementsInterface,
    HasTypeInterface, HasTypeParametersInterface, ListFormat, LiteralLikeNodeInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, Printer, ReadonlyTextRange,
    SourceFileLike, SourceFilePrologueDirective, SourceFilePrologueDirectiveExpression,
    SourceFilePrologueInfo, Symbol, SyntaxKind, TextRange, UnparsedSectionInterface,
};

impl Printer {
    pub(super) fn emit_unparsed_prologues(
        &self,
        prologues: &[Id<Node /*UnparsedPrologue*/>],
        seen_prologue_directives: &mut HashSet<String>,
    ) -> io::Result<()> {
        for prologue in prologues {
            let prologue_as_unparsed_prologue = prologue.as_unparsed_prologue();
            if !seen_prologue_directives
                .contains(prologue_as_unparsed_prologue.maybe_data().unwrap())
            {
                self.write_line(None);
                let pos = self.writer().get_text_pos();
                self.emit(Some(&**prologue), None)?;
                if let Some(bundle_file_info) = self.maybe_bundle_file_info() {
                    bundle_file_info.borrow_mut().sections.push(Gc::new(
                        BundleFileSection::new_prologue(
                            prologue_as_unparsed_prologue
                                .maybe_data()
                                .unwrap()
                                .to_owned(),
                            pos.try_into().unwrap(),
                            self.writer().get_text_pos().try_into().unwrap(),
                        ),
                    ));
                }
                // if (seenPrologueDirectives) {
                seen_prologue_directives.insert(
                    prologue_as_unparsed_prologue
                        .maybe_data()
                        .unwrap()
                        .to_owned(),
                );
                // }
            }
        }

        Ok(())
    }

    pub(super) fn emit_prologue_directives_if_needed(
        &self,
        source_file_or_bundle: Id<Node>, /*Bundle | SourceFile*/
    ) -> io::Result<()> {
        Ok(if is_source_file(source_file_or_bundle) {
            self.emit_prologue_directives(
                &source_file_or_bundle.as_source_file().statements(),
                Some(source_file_or_bundle),
                &mut None,
                None,
            )?;
        } else {
            let source_file_or_bundle_as_bundle = source_file_or_bundle.as_bundle();
            let mut seen_prologue_directives: Option<HashSet<String>> = Some(HashSet::new());
            for prepend in &source_file_or_bundle_as_bundle.prepends {
                self.emit_unparsed_prologues(
                    &prepend.as_unparsed_source().prologues,
                    seen_prologue_directives.as_mut().unwrap(),
                )?;
            }
            for source_file in &source_file_or_bundle_as_bundle.source_files {
                let source_file = source_file.as_ref().unwrap();
                self.emit_prologue_directives(
                    &source_file.as_source_file().statements(),
                    Some(&**source_file),
                    &mut seen_prologue_directives,
                    Some(true),
                )?;
            }
            self.set_source_file(None);
        })
    }

    pub(super) fn get_prologue_directives_from_bundled_source_files(
        &self,
        bundle: Id<Node>, /*Bundle*/
    ) -> Option<Vec<SourceFilePrologueInfo>> {
        let mut seen_prologue_directives: HashSet<String> = HashSet::new();
        let mut prologues: Option<Vec<SourceFilePrologueInfo>> = None;
        let bundle_as_bundle = bundle.as_bundle();
        for index in 0..bundle_as_bundle.source_files.len() {
            let source_file = bundle_as_bundle.source_files[index].as_ref().unwrap();
            let mut directives: Option<Vec<SourceFilePrologueDirective>> = None;
            let mut end = 0;
            let source_file_as_source_file = source_file.as_source_file();
            for statement in &source_file_as_source_file.statements() {
                if !is_prologue_directive(statement) {
                    break;
                }
                let statement_as_expression_statement = statement.as_expression_statement();
                if seen_prologue_directives.contains(
                    &*statement_as_expression_statement
                        .expression
                        .as_string_literal()
                        .text(),
                ) {
                    continue;
                }
                seen_prologue_directives.insert(
                    statement_as_expression_statement
                        .expression
                        .as_string_literal()
                        .text()
                        .clone(),
                );
                directives
                    .get_or_insert_default_()
                    .push(SourceFilePrologueDirective {
                        pos: statement.pos(),
                        end: statement.end(),
                        expression: SourceFilePrologueDirectiveExpression {
                            pos: statement_as_expression_statement.expression.pos(),
                            end: statement_as_expression_statement.expression.end(),
                            text: statement_as_expression_statement
                                .expression
                                .as_string_literal()
                                .text()
                                .clone(),
                        },
                    });
                end = if end < statement.end() {
                    statement.end()
                } else {
                    end
                };
            }
            if let Some(directives) = directives {
                prologues
                    .get_or_insert_default_()
                    .push(SourceFilePrologueInfo {
                        file: index,
                        text: source_file_as_source_file.text_as_chars()
                            [0..TryInto::<usize>::try_into(end).unwrap()]
                            .into_iter()
                            .collect(),
                        directives,
                    });
            }
        }
        prologues
    }

    pub(super) fn emit_shebang_if_needed(
        &self,
        source_file_or_bundle: Id<Node>, /*Bundle | SourceFile | UnparsedSource*/
    ) -> bool {
        if is_source_file(source_file_or_bundle) || is_unparsed_source(source_file_or_bundle) {
            let source_file_or_bundle_as_source_file_like =
                source_file_or_bundle.as_source_file_like();
            let shebang = get_shebang(&source_file_or_bundle_as_source_file_like.text_as_chars());
            if let Some(shebang) = shebang {
                self.write_comment(&shebang.into_iter().collect::<String>());
                self.write_line(None);
                return true;
            }
        } else {
            let source_file_or_bundle_as_bundle = source_file_or_bundle.as_bundle();
            for prepend in &source_file_or_bundle_as_bundle.prepends {
                Debug_.assert_node(Some(&**prepend), Some(is_unparsed_source), None);
                if self.emit_shebang_if_needed(prepend) {
                    return true;
                }
            }
            for source_file in &source_file_or_bundle_as_bundle.source_files {
                if self.emit_shebang_if_needed(source_file.as_ref().unwrap()) {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn emit_node_with_writer(
        &self,
        node: Option<Id<Node>>,
        writer: fn(&Printer, &str),
    ) -> io::Result<()> {
        if node.is_none() {
            return Ok(());
        }
        let node = node.unwrap();
        let saved_write = self.write.get();
        self.write.set(writer);
        self.emit(Some(node), None)?;
        self.write.set(saved_write);

        Ok(())
    }

    pub(super) fn emit_modifiers(
        &self,
        node: Id<Node>,
        modifiers: Option<&NodeArray /*<Modifier>*/>,
    ) -> io::Result<()> {
        Ok(
            if let Some(modifiers) = modifiers.filter(|modifiers| !modifiers.is_empty()) {
                self.emit_list(
                    Some(node),
                    Some(modifiers),
                    ListFormat::Modifiers,
                    None,
                    None,
                    None,
                )?;
                self.write_space();
            },
        )
    }

    pub(super) fn emit_type_annotation(
        &self,
        node: Option<impl Borrow<Node> /*TypeNode*/>,
    ) -> io::Result<()> {
        Ok(if let Some(node) = node {
            let node = node.borrow();
            self.write_punctuation(":");
            self.write_space();
            self.emit(Some(node), None)?;
        })
    }

    pub(super) fn emit_initializer(
        &self,
        node: Option<impl Borrow<Node> /*Expression*/>,
        equal_comment_start_pos: isize,
        container: Id<Node>,
        parenthesizer_rule: Option<Gc<Box<dyn CurrentParenthesizerRule>>>,
    ) -> io::Result<()> {
        Ok(if let Some(node) = node {
            let node = node.borrow();
            self.write_space();
            self.emit_token_with_comment(
                SyntaxKind::EqualsToken,
                equal_comment_start_pos,
                |text: &str| self.write_operator(text),
                container,
                None,
            );
            self.write_space();
            self.emit_expression(Some(node), parenthesizer_rule)?;
        })
    }

    #[allow(dead_code)]
    pub(super) fn emit_node_with_prefix(
        &self,
        prefix: &str,
        prefix_writer: impl FnMut(&str),
        node: Option<Id<Node>>,
        mut emit: impl FnMut(Id<Node>),
    ) {
        self.try_emit_node_with_prefix(prefix, prefix_writer, node, |a| Ok(emit(a)))
            .unwrap()
    }

    pub(super) fn try_emit_node_with_prefix(
        &self,
        prefix: &str,
        mut prefix_writer: impl FnMut(&str),
        node: Option<Id<Node>>,
        mut emit: impl FnMut(Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        if let Some(node) = node {
            prefix_writer(prefix);
            emit(node)?;
        }

        Ok(())
    }

    pub(super) fn emit_with_leading_space(&self, node: Option<Id<Node>>) -> io::Result<()> {
        Ok(if let Some(node) = node {
            self.write_space();
            self.emit(Some(node), None)?;
        })
    }

    pub(super) fn emit_expression_with_leading_space(
        &self,
        node: Option<Id<Node>>,
        parenthesizer_rule: Option<Gc<Box<dyn CurrentParenthesizerRule>>>,
    ) -> io::Result<()> {
        Ok(if let Some(node) = node {
            self.write_space();
            self.emit_expression(Some(node), parenthesizer_rule)?;
        })
    }

    pub(super) fn emit_with_trailing_space(&self, node: Option<Id<Node>>) -> io::Result<()> {
        Ok(if let Some(node) = node {
            self.emit(Some(node), None)?;
            self.write_space();
        })
    }

    pub(super) fn emit_embedded_statement(
        &self,
        parent: Id<Node>,
        node: Id<Node>, /*Statement*/
    ) -> io::Result<()> {
        Ok(
            if is_block(node) || get_emit_flags(parent).intersects(EmitFlags::SingleLine) {
                self.write_space();
                self.emit(Some(node), None)?;
            } else {
                self.write_line(None);
                self.increase_indent();
                if is_empty_statement(node) {
                    self.pipeline_emit(EmitHint::EmbeddedStatement, node, None)?;
                } else {
                    self.emit(Some(node), None)?;
                }
                self.decrease_indent();
            },
        )
    }

    pub(super) fn emit_decorators(
        &self,
        parent_node: Id<Node>,
        decorators: Option<&NodeArray /*<Decorator>*/>,
    ) -> io::Result<()> {
        self.emit_list(
            Some(parent_node),
            decorators,
            ListFormat::Decorators,
            None,
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_type_arguments(
        &self,
        parent_node: Id<Node>,
        type_arguments: Option<&NodeArray /*<TypeNode>*/>,
    ) -> io::Result<()> {
        self.emit_list(
            Some(parent_node),
            type_arguments,
            ListFormat::TypeArguments,
            Some(Gc::new(Box::new(
                ParenthesizeMemberOfElementTypeCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_type_parameters(
        &self,
        parent_node: Id<Node>, /*SignatureDeclaration | InterfaceDeclaration | TypeAliasDeclaration | ClassDeclaration | ClassExpression*/
        type_parameters: Option<&NodeArray /*<TypeParameterDeclaration>*/>,
    ) -> io::Result<()> {
        if is_function_like(Some(parent_node)) {
            // TODO
            // && parentNode.typeArguments {
            //     return emitTypeArguments(parentNode, parentNode.typeArguments);
            // }
        }
        self.emit_list(
            Some(parent_node),
            type_parameters,
            ListFormat::TypeParameters,
            None,
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_parameters(
        &self,
        parent_node: Id<Node>,
        parameters: &NodeArray, /*<ParameterDeclaration>*/
    ) -> io::Result<()> {
        self.emit_list(
            Some(parent_node),
            Some(parameters),
            ListFormat::Parameters,
            None,
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn can_emit_simple_arrow_head(
        &self,
        parent_node: Id<Node>,     /*FunctionTypeNode | ArrowFunction*/
        parameters: &NodeArray, /*<ParameterDeclaration>*/
    ) -> bool {
        let parameter = single_or_undefined(Some(parameters));
        matches!(
            parameter,
            Some(parameter) if parameter.pos() == parent_node.pos() &&
                is_arrow_function(parent_node) && {
                    let parent_node_as_arrow_function = parent_node.as_arrow_function();
                    let parameter_as_parameter_declaration = parameter.as_parameter_declaration();
                    parent_node_as_arrow_function.maybe_type().is_none() &&
                        !some(
                            parent_node.maybe_decorators().as_double_deref(),
                            Option::<fn(&Id<Node>) -> bool>::None
                        ) &&
                        !some(
                            parent_node.maybe_modifiers().as_double_deref(),
                            Option::<fn(&Id<Node>) -> bool>::None
                        ) &&
                        !some(
                            parent_node_as_arrow_function.maybe_type_parameters().as_double_deref(),
                            Option::<fn(&Id<Node>) -> bool>::None
                        ) &&
                        !some(
                            parameter.maybe_decorators().as_double_deref(),
                            Option::<fn(&Id<Node>) -> bool>::None
                        ) &&
                        !some(
                            parameter.maybe_modifiers().as_double_deref(),
                            Option::<fn(&Id<Node>) -> bool>::None
                        ) &&
                        parameter_as_parameter_declaration.dot_dot_dot_token.is_none() &&
                        parameter_as_parameter_declaration.question_token.is_none() &&
                        parameter_as_parameter_declaration.maybe_type().is_none() &&
                        parameter_as_parameter_declaration.maybe_initializer().is_none() &&
                        is_identifier(&parameter_as_parameter_declaration.name())
                }
        )
    }

    pub(super) fn emit_parameters_for_arrow(
        &self,
        parent_node: &Node,     /*FunctionTypeNode | ArrowFunction*/
        parameters: &NodeArray, /*<ParameterDeclaration>*/
    ) -> io::Result<()> {
        Ok(
            if self.can_emit_simple_arrow_head(parent_node, parameters) {
                self.emit_list(
                    Some(parent_node),
                    Some(parameters),
                    ListFormat::Parameters & !ListFormat::Parenthesis,
                    None,
                    None,
                    None,
                )?;
            } else {
                self.emit_parameters(parent_node, parameters)?;
            },
        )
    }

    pub(super) fn emit_parameters_for_index_signature(
        &self,
        parent_node: &Node,
        parameters: &NodeArray, /*<ParameterDeclaration>*/
    ) -> io::Result<()> {
        self.emit_list(
            Some(parent_node),
            Some(parameters),
            ListFormat::IndexSignatureParameters,
            None,
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn write_delimiter(&self, format: ListFormat) {
        match format & ListFormat::DelimitersMask {
            ListFormat::None => (),
            ListFormat::CommaDelimited => {
                self.write_punctuation(",");
            }
            ListFormat::BarDelimited => {
                self.write_space();
                self.write_punctuation("|");
            }
            ListFormat::AsteriskDelimited => {
                self.write_space();
                self.write_punctuation("*");
                self.write_space();
            }
            ListFormat::AmpersandDelimited => {
                self.write_space();
                self.write_punctuation("&");
            }
            _ => (),
        }
    }

    pub(super) fn emit_list(
        &self,
        parent_node: Option<impl Borrow<Node>>,
        children: Option<&NodeArray>,
        format: ListFormat,
        parenthesizer_rule: Option<Gc<Box<dyn CurrentParenthesizerRule>>>,
        start: Option<usize>,
        count: Option<usize>,
    ) -> io::Result<()> {
        self.emit_node_list(
            Printer::emit,
            parent_node,
            children,
            format,
            parenthesizer_rule,
            start,
            count,
        )?;

        Ok(())
    }

    pub(super) fn emit_expression_list(
        &self,
        parent_node: Option<impl Borrow<Node>>,
        children: Option<&NodeArray>,
        format: ListFormat,
        parenthesizer_rule: Option<Gc<Box<dyn CurrentParenthesizerRule>>>,
        start: Option<usize>,
        count: Option<usize>,
    ) -> io::Result<()> {
        self.emit_node_list(
            Printer::emit_expression,
            parent_node,
            children,
            format,
            parenthesizer_rule,
            start,
            count,
        )?;

        Ok(())
    }

    pub(super) fn emit_node_list(
        &self,
        emit: fn(
            &Printer,
            Option<&Node>,
            Option<Gc<Box<dyn CurrentParenthesizerRule>>>,
        ) -> io::Result<()>,
        parent_node: Option<impl Borrow<Node>>,
        children: Option<&NodeArray>,
        format: ListFormat,
        parenthesizer_rule: Option<Gc<Box<dyn CurrentParenthesizerRule>>>,
        start: Option<usize>,
        count: Option<usize>,
    ) -> io::Result<()> {
        let start = start.unwrap_or(0);
        let count = count.unwrap_or_else(|| {
            if let Some(children) = children {
                children.len() - start
            } else {
                0
            }
        });
        let is_undefined = children.is_none();
        if is_undefined && format.intersects(ListFormat::OptionalIfUndefined) {
            return Ok(());
        }

        let is_empty = match children {
            None => true,
            Some(children) => start >= children.len(),
        } || count == 0;
        if is_empty && format.intersects(ListFormat::OptionalIfEmpty) {
            self.on_before_emit_node_array(children);
            self.on_after_emit_node_array(children);
            return Ok(());
        }

        if format.intersects(ListFormat::BracketsMask) {
            self.write_punctuation(get_opening_bracket(format));
            if is_empty {
                if let Some(children) = children {
                    self.emit_trailing_comments_of_position(children.pos(), Some(true), None);
                }
            }
        }

        self.on_before_emit_node_array(children);

        let parent_node = parent_node.map(|parent_node| parent_node.borrow().node_wrapper());
        if is_empty {
            if format.intersects(ListFormat::MultiLine)
                && !(self.maybe_preserve_source_newlines() == Some(true)
                    && match parent_node.as_ref() {
                        None => true,
                        Some(parent_node) => {
                            range_is_on_single_line(&**parent_node, &self.current_source_file())
                        }
                    })
            {
                self.write_line(None);
            } else if format.intersects(ListFormat::SpaceBetweenBraces)
                && !format.intersects(ListFormat::NoSpaceIfEmpty)
            {
                self.write_space();
            }
        } else {
            // Debug.type<NodeArray<Node>>(children);
            let children = children.unwrap();
            let may_emit_intervening_comments =
                !format.intersects(ListFormat::NoInterveningComments);
            let mut should_emit_intervening_comments = may_emit_intervening_comments;
            let leading_line_terminator_count =
                self.get_leading_line_terminator_count(parent_node.as_deref(), children, format);
            if leading_line_terminator_count > 0 {
                self.write_line(Some(leading_line_terminator_count));
                should_emit_intervening_comments = false;
            } else if format.intersects(ListFormat::SpaceBetweenBraces) {
                self.write_space();
            }

            if format.intersects(ListFormat::Indented) {
                self.increase_indent();
            }

            let mut previous_sibling: Option<Id<Node>> = None;
            let mut previous_source_file_text_kind: Option<BundleFileSectionKind> = None;
            let mut should_decrease_indent_after_emit = false;
            for i in 0..count {
                let child = &children[start + i];

                if format.intersects(ListFormat::AsteriskDelimited) {
                    self.write_line(None);
                    self.write_delimiter(format);
                } else if let Some(previous_sibling) = previous_sibling.as_ref() {
                    if format.intersects(ListFormat::DelimitersMask)
                        && previous_sibling.end()
                            != parent_node
                                .as_ref()
                                .map_or(-1, |parent_node| parent_node.end())
                    {
                        self.emit_leading_comments_of_position(previous_sibling.end());
                    }
                    self.write_delimiter(format);
                    self.record_bundle_file_internal_section_end(previous_source_file_text_kind);

                    let separating_line_terminator_count = self
                        .get_separating_line_terminator_count(
                            Some(previous_sibling),
                            child,
                            format,
                        );
                    if separating_line_terminator_count > 0 {
                        if format & (ListFormat::LinesMask | ListFormat::Indented)
                            == ListFormat::SingleLine
                        {
                            self.increase_indent();
                            should_decrease_indent_after_emit = true;
                        }

                        self.write_line(Some(separating_line_terminator_count));
                        should_emit_intervening_comments = false;
                    } else if
                    /*previousSibling &&*/
                    format.intersects(ListFormat::SpaceBetweenSiblings) {
                        self.write_space();
                    }
                }

                previous_source_file_text_kind =
                    self.record_bundle_file_internal_section_start(child);
                if should_emit_intervening_comments {
                    // if (emitTrailingCommentsOfPosition) {
                    let comment_range = get_comment_range(child);
                    self.emit_trailing_comments_of_position(comment_range.pos(), None, None);
                    // }
                } else {
                    should_emit_intervening_comments = may_emit_intervening_comments;
                }

                self.set_next_list_element_pos(Some(child.pos()));
                emit(self, Some(&**child), parenthesizer_rule.clone())?;

                if should_decrease_indent_after_emit {
                    self.decrease_indent();
                    should_decrease_indent_after_emit = false;
                }

                previous_sibling = Some(child.clone());
            }

            let emit_flags = previous_sibling
                .as_ref()
                .map_or(EmitFlags::None, |previous_sibling| {
                    get_emit_flags(previous_sibling)
                });
            let skip_trailing_comments =
                self.comments_disabled() || emit_flags.intersects(EmitFlags::NoTrailingComments);
            let has_trailing_comma = children.has_trailing_comma
                && format.intersects(ListFormat::AllowTrailingComma)
                && format.intersects(ListFormat::CommaDelimited);
            if has_trailing_comma {
                if let Some(previous_sibling) = previous_sibling
                    .as_ref()
                    .filter(|_| !skip_trailing_comments)
                {
                    self.emit_token_with_comment(
                        SyntaxKind::CommaToken,
                        previous_sibling.end(),
                        |text: &str| self.write_punctuation(text),
                        previous_sibling,
                        None,
                    );
                } else {
                    self.write_punctuation(",");
                }
            }

            if let Some(previous_sibling) = previous_sibling.as_ref() {
                if parent_node
                    .as_ref()
                    .map_or(-1, |parent_node| parent_node.end())
                    != previous_sibling.end()
                    && format.intersects(ListFormat::DelimitersMask)
                    && !skip_trailing_comments
                {
                    self.emit_leading_comments_of_position(
                        if has_trailing_comma {
                            let children_end = children.end();
                            if children_end != 0 {
                                Some(children_end)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                        .unwrap_or_else(|| previous_sibling.end()),
                    );
                }
            }

            if format.intersects(ListFormat::Indented) {
                self.decrease_indent();
            }

            self.record_bundle_file_internal_section_end(previous_source_file_text_kind);

            let closing_line_terminator_count = self.get_closing_line_terminator_count(
                parent_node.as_deref(),
                children.rc_wrapper().into(),
                format,
            );
            if closing_line_terminator_count != 0 {
                self.write_line(Some(closing_line_terminator_count));
            } else if format.intersects(ListFormat::SpaceAfterList | ListFormat::SpaceBetweenBraces)
            {
                self.write_space();
            }
        }

        self.on_after_emit_node_array(children);

        Ok(if format.intersects(ListFormat::BracketsMask) {
            if is_empty {
                if let Some(children) = children {
                    self.emit_leading_comments_of_position(children.end());
                }
            }
            self.write_punctuation(get_closing_bracket(format));
        })
    }

    pub(super) fn write_literal(&self, s: &str) {
        self.writer().write_literal(s);
    }

    pub(super) fn write_string_literal(&self, s: &str) {
        self.writer().write_string_literal(s);
    }

    pub(super) fn write_base(&self, s: &str) {
        self.writer().write(s);
    }

    pub(super) fn write_symbol(&self, s: &str, sym: Id<Symbol>) {
        self.writer().write_symbol(s, sym);
    }

    pub(super) fn write_punctuation(&self, s: &str) {
        self.writer().write_punctuation(s);
    }

    pub(super) fn write_trailing_semicolon(&self) {
        self.writer().write_trailing_semicolon(";");
    }

    pub(super) fn write_keyword(&self, s: &str) {
        self.writer().write_keyword(s);
    }

    pub(super) fn write_operator(&self, s: &str) {
        self.writer().write_operator(s);
    }

    pub(super) fn write_parameter(&self, s: &str) {
        self.writer().write_parameter(s);
    }

    pub(super) fn write_comment(&self, s: &str) {
        self.writer().write_comment(s);
    }

    pub(super) fn write_space(&self) {
        self.writer().write_space(" ");
    }

    pub(super) fn write_property(&self, s: &str) {
        self.writer().write_property(s);
    }

    pub(super) fn non_escaping_write(&self, s: &str) {
        if self.writer().is_non_escaping_write_supported() {
            self.writer().non_escaping_write(s);
        } else {
            self.writer().write(s);
        }
    }
}
