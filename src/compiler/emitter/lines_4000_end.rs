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
    pub(super) fn emit_unparsed_prologues(
        &self,
        prologues: &[Rc<Node /*UnparsedPrologue*/>],
        seen_prologue_directives: &mut HashSet<String>,
    ) {
        for prologue in prologues {
            let prologue_as_unparsed_prologue = prologue.as_unparsed_prologue();
            if !seen_prologue_directives
                .contains(prologue_as_unparsed_prologue.maybe_data().unwrap())
            {
                self.write_line(None);
                let pos = self.writer().get_text_pos();
                self.emit(Some(&**prologue), None);
                if let Some(bundle_file_info) = self.maybe_bundle_file_info_mut().as_mut() {
                    bundle_file_info
                        .sections
                        .push(Rc::new(BundleFileSection::new_prologue(
                            prologue_as_unparsed_prologue
                                .maybe_data()
                                .unwrap()
                                .to_owned(),
                            pos.try_into().unwrap(),
                            self.writer().get_text_pos().try_into().unwrap(),
                        )));
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
    }

    pub(super) fn emit_prologue_directives_if_needed(
        &self,
        source_file_or_bundle: &Node, /*Bundle | SourceFile*/
    ) {
        if is_source_file(source_file_or_bundle) {
            self.emit_prologue_directives(
                &source_file_or_bundle.as_source_file().statements,
                Some(source_file_or_bundle),
                &mut None,
                None,
            );
        } else {
            let source_file_or_bundle_as_bundle = source_file_or_bundle.as_bundle();
            let mut seen_prologue_directives: Option<HashSet<String>> = Some(HashSet::new());
            for prepend in &source_file_or_bundle_as_bundle.prepends {
                self.emit_unparsed_prologues(
                    &prepend.as_unparsed_source().prologues,
                    seen_prologue_directives.as_mut().unwrap(),
                );
            }
            for source_file in &source_file_or_bundle_as_bundle.source_files {
                self.emit_prologue_directives(
                    &source_file.as_source_file().statements,
                    Some(&**source_file),
                    &mut seen_prologue_directives,
                    Some(true),
                );
            }
            self.set_source_file(None);
        }
    }

    pub(super) fn get_prologue_directives_from_bundled_source_files(
        &self,
        bundle: &Node, /*Bundle*/
    ) -> Option<Vec<SourceFilePrologueInfo>> {
        let mut seen_prologue_directives: HashSet<String> = HashSet::new();
        let mut prologues: Option<Vec<SourceFilePrologueInfo>> = None;
        let bundle_as_bundle = bundle.as_bundle();
        for index in 0..bundle_as_bundle.source_files.len() {
            let source_file = &bundle_as_bundle.source_files[index];
            let mut directives: Option<Vec<SourceFilePrologueDirective>> = None;
            let mut end = 0;
            let source_file_as_source_file = source_file.as_source_file();
            for statement in &source_file_as_source_file.statements {
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
                    .get_or_insert_with(|| vec![])
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
                    .get_or_insert_with(|| vec![])
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
        source_file_or_bundle: &Node, /*Bundle | SourceFile | UnparsedSource*/
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
                if self.emit_shebang_if_needed(source_file) {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn emit_node_with_writer(&self, node: Option<&Node>, writer: fn(&Printer, &str)) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        let saved_write = self.write.get();
        self.write.set(writer);
        self.emit(Some(node), None);
        self.write.set(saved_write);
    }

    pub(super) fn emit_modifiers(&self, node: &Node, modifiers: Option<&NodeArray /*<Modifier>*/>) {
        if let Some(modifiers) = modifiers.filter(|modifiers| !modifiers.is_empty()) {
            self.emit_list(
                Some(node),
                Some(modifiers),
                ListFormat::Modifiers,
                None,
                None,
                None,
            );
            self.write_space();
        }
    }

    pub(super) fn emit_type_annotation<TNode: Borrow<Node>>(
        &self,
        node: Option<TNode /*TypeNode*/>,
    ) {
        if let Some(node) = node {
            let node = node.borrow();
            self.write_punctuation(":");
            self.write_space();
            self.emit(Some(node), None);
        }
    }

    pub(super) fn emit_initializer<TNode: Borrow<Node>>(
        &self,
        node: Option<TNode /*Expression*/>,
        equal_comment_start_pos: isize,
        container: &Node,
        parenthesizer_rule: Option<Rc<dyn Fn(&Node) -> Rc<Node>>>,
    ) {
        if let Some(node) = node {
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
            self.emit_expression(Some(node), parenthesizer_rule);
        }
    }

    pub(super) fn emit_node_with_prefix<TPrefixWriter: FnMut(&str), TEmit: FnMut(&Node)>(
        &self,
        prefix: &str,
        mut prefix_writer: TPrefixWriter,
        node: Option<&Node>,
        emit: TEmit,
    ) {
        if let Some(node) = node {
            prefix_writer(prefix);
            self.emit(Some(node), None);
        }
    }

    pub(super) fn emit_with_leading_space(&self, node: Option<&Node>) {
        if let Some(node) = node {
            self.write_space();
            self.emit(Some(node), None);
        }
    }

    pub(super) fn emit_expression_with_leading_space(
        &self,
        node: Option<&Node>,
        parenthesizer_rule: Option<Rc<dyn Fn(&Node) -> Rc<Node>>>,
    ) {
        if let Some(node) = node {
            self.write_space();
            self.emit_expression(Some(node), parenthesizer_rule);
        }
    }

    pub(super) fn emit_with_trailing_space(&self, node: Option<&Node>) {
        if let Some(node) = node {
            self.emit(Some(node), None);
            self.write_space();
        }
    }

    pub(super) fn emit_embedded_statement(&self, parent: &Node, node: &Node /*Statement*/) {
        if is_block(node) || get_emit_flags(parent).intersects(EmitFlags::SingleLine) {
            self.write_space();
            self.emit(Some(node), None);
        } else {
            self.write_line(None);
            self.increase_indent();
            if is_empty_statement(node) {
                self.pipeline_emit(EmitHint::EmbeddedStatement, node, None);
            } else {
                self.emit(Some(node), None);
            }
            self.decrease_indent();
        }
    }

    pub(super) fn emit_decorators(
        &self,
        parent_node: &Node,
        decorators: Option<&NodeArray /*<Decorator>*/>,
    ) {
        self.emit_list(
            Some(parent_node),
            decorators,
            ListFormat::Decorators,
            None,
            None,
            None,
        );
    }

    pub(super) fn emit_type_arguments(
        &self,
        parent_node: &Node,
        type_arguments: Option<&NodeArray /*<TypeNode>*/>,
    ) {
        self.emit_list(
            Some(parent_node),
            type_arguments,
            ListFormat::TypeArguments,
            Some(Rc::new({
                let parenthesizer = self.parenthesizer();
                move |node: &Node| {
                    with_synthetic_factory(|synthetic_factory| {
                        parenthesizer.parenthesize_member_of_element_type(synthetic_factory, node)
                    })
                }
            })),
            None,
            None,
        );
    }

    pub(super) fn emit_type_parameters(
        &self,
        parent_node: &Node, /*SignatureDeclaration | InterfaceDeclaration | TypeAliasDeclaration | ClassDeclaration | ClassExpression*/
        type_parameters: Option<&NodeArray /*<TypeParameterDeclaration>*/>,
    ) {
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
        );
    }

    pub(super) fn emit_parameters(
        &self,
        parent_node: &Node,
        parameters: &NodeArray, /*<ParameterDeclaration>*/
    ) {
        self.emit_list(
            Some(parent_node),
            Some(parameters),
            ListFormat::Parameters,
            None,
            None,
            None,
        );
    }

    pub(super) fn can_emit_simple_arrow_head(
        &self,
        parent_node: &Node,     /*FunctionTypeNode | ArrowFunction*/
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
                            parent_node.maybe_decorators().as_deref(),
                            Option::<fn(&Rc<Node>) -> bool>::None
                        ) &&
                        !some(
                            parent_node.maybe_modifiers().as_deref(),
                            Option::<fn(&Rc<Node>) -> bool>::None
                        ) &&
                        !some(
                            parent_node_as_arrow_function.maybe_type_parameters().as_deref(),
                            Option::<fn(&Rc<Node>) -> bool>::None
                        ) &&
                        !some(
                            parameter.maybe_decorators().as_deref(),
                            Option::<fn(&Rc<Node>) -> bool>::None
                        ) &&
                        !some(
                            parameter.maybe_modifiers().as_deref(),
                            Option::<fn(&Rc<Node>) -> bool>::None
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
    ) {
        if self.can_emit_simple_arrow_head(parent_node, parameters) {
            self.emit_list(
                Some(parent_node),
                Some(parameters),
                ListFormat::Parameters & !ListFormat::Parenthesis,
                None,
                None,
                None,
            );
        } else {
            self.emit_parameters(parent_node, parameters);
        }
    }

    pub(super) fn emit_parameters_for_index_signature(
        &self,
        parent_node: &Node,
        parameters: &NodeArray, /*<ParameterDeclaration>*/
    ) {
        self.emit_list(
            Some(parent_node),
            Some(parameters),
            ListFormat::IndexSignatureParameters,
            None,
            None,
            None,
        );
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

    pub(super) fn emit_list<TNode: Borrow<Node>>(
        &self,
        parent_node: Option<TNode>,
        children: Option<&NodeArray>,
        format: ListFormat,
        parenthesizer_rule: Option<Rc<dyn Fn(&Node) -> Rc<Node>>>,
        start: Option<usize>,
        count: Option<usize>,
    ) {
        self.emit_node_list(
            Printer::emit,
            parent_node,
            children,
            format,
            parenthesizer_rule,
            start,
            count,
        );
    }

    pub(super) fn emit_expression_list<TNode: Borrow<Node>>(
        &self,
        parent_node: Option<TNode>,
        children: Option<&NodeArray>,
        format: ListFormat,
        parenthesizer_rule: Option<Rc<dyn Fn(&Node) -> Rc<Node>>>,
        start: Option<usize>,
        count: Option<usize>,
    ) {
        self.emit_node_list(
            Printer::emit_expression,
            parent_node,
            children,
            format,
            parenthesizer_rule,
            start,
            count,
        );
    }

    pub(super) fn emit_node_list<TNode: Borrow<Node>>(
        &self,
        emit: fn(&Printer, Option<&Node>, Option<Rc<dyn Fn(&Node) -> Rc<Node>>>),
        parent_node: Option<TNode>,
        children: Option<&NodeArray>,
        format: ListFormat,
        parenthesizer_rule: Option<Rc<dyn Fn(&Node) -> Rc<Node>>>,
        start: Option<usize>,
        count: Option<usize>,
    ) {
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
            return;
        }

        let is_empty = match children {
            None => true,
            Some(children) => start >= children.len(),
        } || count == 0;
        if is_empty && format.intersects(ListFormat::OptionalIfEmpty) {
            self.on_before_emit_node_array(children);
            self.on_after_emit_node_array(children);
            return;
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

            let mut previous_sibling: Option<Rc<Node>> = None;
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
                emit(self, Some(&**child), parenthesizer_rule.clone());

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

            let closing_line_terminator_count =
                self.get_closing_line_terminator_count(parent_node.as_deref(), children, format);
            if closing_line_terminator_count != 0 {
                self.write_line(Some(closing_line_terminator_count));
            } else if format.intersects(ListFormat::SpaceAfterList | ListFormat::SpaceBetweenBraces)
            {
                self.write_space();
            }
        }

        self.on_after_emit_node_array(children);

        if format.intersects(ListFormat::BracketsMask) {
            if is_empty {
                if let Some(children) = children {
                    self.emit_leading_comments_of_position(children.end());
                }
            }
            self.write_punctuation(get_closing_bracket(format));
        }
    }

    pub(super) fn write_base(&self, s: &str) {
        self.writer().write(s);
    }

    pub(super) fn write_literal(&self, s: &str) {
        unimplemented!()
    }

    pub(super) fn write_string_literal(&self, s: &str) {
        self.writer().write_string_literal(s);
    }

    pub(super) fn write_symbol(&self, s: &str, sym: &Symbol) {
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
        unimplemented!()
    }

    pub(super) fn write_parameter(&self, s: &str) {
        unimplemented!()
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
        unimplemented!()
    }

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
