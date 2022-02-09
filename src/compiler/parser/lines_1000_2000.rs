#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::convert::TryInto;
use std::rc::Rc;

use super::{ParserType, ParsingContext, SpeculationKind};
use crate::{
    add_range, attach_file_to_diagnostics, create_detached_diagnostic, find_index,
    get_jsdoc_comment_ranges, get_language_variant, get_spelling_suggestion, id_text,
    is_declaration_file_name, is_external_module, is_identifier, is_identifier_text, is_keyword,
    is_modifier_kind, is_tagged_template_expression, is_template_literal_kind, last_or_undefined,
    map_defined, process_comment_pragmas, process_pragmas_into_fields, set_parent_recursive,
    set_text_range, set_text_range_pos_end, set_text_range_pos_width, skip_trivia, starts_with,
    text_to_keyword_obj, token_is_identifier_or_keyword, token_to_string, BaseNode,
    ComputedPropertyName, Debug_, DiagnosticMessage, DiagnosticRelatedInformationInterface,
    Diagnostics, IncrementalParser, IncrementalParserSyntaxCursor,
    IncrementalParserSyntaxCursorInterface, Node, NodeArray, NodeArrayOrVec, NodeFlags,
    NodeInterface, ReadonlyTextRange, ScriptKind, ScriptTarget, SyntaxKind, TextRange,
    TransformFlags,
};

impl ParserType {
    pub(super) fn clear_state(&mut self) {
        {
            let mut scanner = self.scanner_mut();
            scanner.clear_comment_directives();
            scanner.set_text(Some(vec![]), Some("".to_owned()), None, None);
            // scanner.set_on_error(None);
        }

        self.set_source_text(None);
        self.set_language_version(None);
        self.set_syntax_cursor(None);
        self.set_script_kind(None);
        self.set_language_variant(None);
        self.set_source_flags(NodeFlags::None);
        self.set_parse_diagnostics(None);
        self.set_js_doc_diagnostics(None);
        self.set_parsing_context(ParsingContext::None);
        self.set_identifiers(None);
        self.set_not_parenthesized_arrow(None);
        self.set_top_level(true);
    }

    pub(super) fn parse_source_file_worker(
        &self,
        language_version: ScriptTarget,
        set_parent_nodes: bool,
        script_kind: ScriptKind,
    ) -> Rc<Node /*SourceFile*/> {
        let is_declaration_file = is_declaration_file_name(self.file_name());
        if is_declaration_file {
            self.set_context_flags(self.context_flags() | NodeFlags::Ambient);
        }
        let source_flags = self.context_flags();

        self.next_token();

        let statements =
            self.parse_list(ParsingContext::SourceElements, ParserType::parse_statement);
        Debug_.assert(self.token() == SyntaxKind::EndOfFileToken, None);
        let end_of_file_token = self.add_jsdoc_comment(self.parse_token_node());

        let source_file = self.create_source_file(
            self.file_name(),
            language_version,
            script_kind,
            is_declaration_file,
            statements,
            end_of_file_token.into(),
            source_flags,
        );
        let source_file_as_source_file = source_file.as_source_file();

        process_comment_pragmas(source_file_as_source_file, self.source_text());
        let report_pragma_diagnostic = |pos: isize, end: isize, diagnostic: &DiagnosticMessage| {
            self.parse_diagnostics().push(Rc::new(
                create_detached_diagnostic(self.file_name(), pos, end, diagnostic, None).into(),
            ));
        };
        process_pragmas_into_fields(source_file_as_source_file, report_pragma_diagnostic);

        source_file_as_source_file
            .set_comment_directives(self.scanner().get_comment_directives().clone());
        source_file_as_source_file.set_node_count(self.node_count());
        source_file_as_source_file.set_identifier_count(self.identifier_count());
        source_file_as_source_file.set_identifiers(self.identifiers_rc());
        source_file_as_source_file.set_parse_diagnostics(attach_file_to_diagnostics(
            &*self.parse_diagnostics(),
            &source_file,
        ));
        {
            let maybe_js_doc_diagnostics = self.maybe_js_doc_diagnostics();
            if let Some(js_doc_diagnostics) = &*maybe_js_doc_diagnostics {
                source_file_as_source_file.set_js_doc_diagnostics(attach_file_to_diagnostics(
                    js_doc_diagnostics,
                    &source_file,
                ));
            }
        }

        if set_parent_nodes {
            self.fixup_parent_references(&source_file);
        }

        source_file
    }

    pub(super) fn with_jsdoc<TNode: NodeInterface>(&self, node: TNode, has_jsdoc: bool) -> TNode {
        if has_jsdoc {
            self.add_jsdoc_comment(node)
        } else {
            node
        }
    }

    pub(super) fn add_jsdoc_comment<TNode: NodeInterface>(&self, node: TNode) -> TNode {
        Debug_.assert(node.maybe_js_doc().is_none(), None);
        let js_doc = map_defined(
            get_jsdoc_comment_ranges(&node, self.source_text_as_chars()),
            |comment, _| {
                self.JSDocParser_parse_jsdoc_comment(
                    &node,
                    comment.pos().try_into().unwrap(),
                    (comment.end() - comment.pos()).try_into().unwrap(),
                )
            },
        );
        if !js_doc.is_empty() {
            node.set_js_doc(js_doc);
        }
        if self.has_deprecated_tag() {
            self.set_has_deprecated_tag(false);
            node.set_flags(NodeFlags::Deprecated);
        }
        node
    }

    pub(super) fn reparse_top_level_await(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Rc<Node> {
        let saved_syntax_cursor = self.take_syntax_cursor();
        let base_syntax_cursor = IncrementalParser().create_syntax_cursor(source_file);
        self.set_syntax_cursor(Some(
            IncrementalParserSyntaxCursorReparseTopLevelAwait::new(Rc::new(base_syntax_cursor))
                .into(),
        ));

        let mut statements: Vec<Rc<Node>> = vec![];
        let mut parse_diagnostics_ref = self.parse_diagnostics();
        let saved_parse_diagnostics = parse_diagnostics_ref.clone();

        *parse_diagnostics_ref = vec![];

        let mut pos: Option<usize> = Some(0);
        let source_file_as_source_file = source_file.as_source_file();
        let mut start =
            self.find_next_statement_with_await(&source_file_as_source_file.statements, 0);
        while let Some(start_present) = start {
            let prev_statement = &source_file_as_source_file.statements[pos.unwrap()];
            let next_statement = &source_file_as_source_file.statements[start_present];
            add_range(
                &mut statements,
                Some(&source_file_as_source_file.statements),
                pos.map(|pos| pos.try_into().unwrap()),
                Some(start_present.try_into().unwrap()),
            );
            pos = self.find_next_statement_without_await(
                &source_file_as_source_file.statements,
                start_present,
            );

            let diagnostic_start = find_index(
                &saved_parse_diagnostics,
                |diagnostic, _| diagnostic.start() >= prev_statement.pos(),
                None,
            );
            let diagnostic_end = diagnostic_start.and_then(|diagnostic_start| {
                find_index(
                    &saved_parse_diagnostics,
                    |diagnostic, _| diagnostic.start() >= next_statement.pos(),
                    Some(diagnostic_start),
                )
            });
            if let Some(diagnostic_start) = diagnostic_start {
                add_range(
                    &mut *parse_diagnostics_ref,
                    Some(&saved_parse_diagnostics),
                    Some(diagnostic_start.try_into().unwrap()),
                    diagnostic_end.map(|diagnostic_end| diagnostic_end.try_into().unwrap()),
                );
            }

            self.speculation_helper(
                || {
                    let saved_context_flags = self.context_flags();
                    self.set_context_flags(self.context_flags() | NodeFlags::AwaitContext);
                    self.scanner_mut()
                        .set_text_pos(next_statement.pos().try_into().unwrap());
                    self.next_token();

                    while self.token() != SyntaxKind::EndOfFileToken {
                        let start_pos = self.scanner().get_start_pos();
                        let statement: Rc<Node> = self
                            .parse_list_element(
                                ParsingContext::SourceElements,
                                ParserType::parse_statement,
                            )
                            .wrap();
                        statements.push(statement.clone());
                        if start_pos == self.scanner().get_start_pos() {
                            self.next_token();
                        }

                        if let Some(pos_present) = pos {
                            let non_await_statement =
                                &source_file_as_source_file.statements[pos_present];
                            if statement.end() == non_await_statement.pos() {
                                break;
                            }
                            if statement.end() > non_await_statement.pos() {
                                pos = self.find_next_statement_without_await(
                                    &source_file_as_source_file.statements,
                                    pos_present + 1,
                                );
                            }
                        }
                    }

                    self.set_context_flags(saved_context_flags);
                    Option::<()>::None
                },
                SpeculationKind::Reparse,
            );

            start = pos.and_then(|pos| {
                self.find_next_statement_with_await(&source_file_as_source_file.statements, pos)
            });
        }

        if let Some(pos) = pos {
            let prev_statement = &source_file_as_source_file.statements[pos];
            add_range(
                &mut statements,
                Some(&source_file_as_source_file.statements),
                Some(pos.try_into().unwrap()),
                None,
            );

            let diagnostic_start = find_index(
                &saved_parse_diagnostics,
                |diagnostic, _| diagnostic.start() >= prev_statement.pos(),
                None,
            );
            if let Some(diagnostic_start) = diagnostic_start {
                add_range(
                    &mut *parse_diagnostics_ref,
                    Some(&saved_parse_diagnostics),
                    Some(diagnostic_start.try_into().unwrap()),
                    None,
                );
            }
        }

        self.set_syntax_cursor(saved_syntax_cursor);
        let new_statements = self.factory.create_node_array(Some(statements), None);
        set_text_range(
            &new_statements,
            Some(&source_file_as_source_file.statements),
        );
        self.factory.update_source_file(
            self,
            source_file,
            new_statements,
            None,
            None,
            None,
            None,
            None,
        )
    }

    pub(super) fn contains_possible_top_level_await(&self, node: &Node) -> bool {
        !node.flags().intersects(NodeFlags::AwaitContext)
            && node
                .transform_flags()
                .intersects(TransformFlags::ContainsPossibleTopLevelAwait)
    }

    pub(super) fn find_next_statement_with_await(
        &self,
        statements: &[Rc<Node /*Statement*/>],
        start: usize,
    ) -> Option<usize> {
        for (i, statement) in statements.iter().enumerate().skip(start) {
            if self.contains_possible_top_level_await(statement) {
                return Some(i);
            }
        }

        None
    }

    pub(super) fn find_next_statement_without_await(
        &self,
        statements: &[Rc<Node /*Statement*/>],
        start: usize,
    ) -> Option<usize> {
        for (i, statement) in statements.iter().enumerate().skip(start) {
            if !self.contains_possible_top_level_await(statement) {
                return Some(i);
            }
        }

        None
    }

    pub fn fixup_parent_references(&self, root_node: &Node) {
        set_parent_recursive(Some(root_node), true);
    }

    pub(super) fn create_source_file<TNodes: Into<NodeArrayOrVec>>(
        &self,
        file_name: &str,
        language_version: ScriptTarget,
        script_kind: ScriptKind,
        is_declaration_file: bool,
        statements: TNodes,
        end_of_file_token: Rc<Node /*EndOfFileToken*/>,
        flags: NodeFlags,
    ) -> Rc<Node> {
        let mut source_file: Rc<Node> = self
            .factory
            .create_source_file(self, statements, end_of_file_token, flags)
            .into();
        set_text_range_pos_width(
            &*source_file,
            0,
            self.source_text_as_chars().len().try_into().unwrap(),
        );
        self.set_external_module_indicator(&source_file);

        if !is_declaration_file
            && is_external_module(&source_file)
            && source_file
                .transform_flags()
                .intersects(TransformFlags::ContainsPossibleTopLevelAwait)
        {
            source_file = self.reparse_top_level_await(&source_file);
        }

        let source_file_as_source_file = source_file.as_source_file();
        source_file_as_source_file.set_text(self.source_text().to_string());
        source_file_as_source_file.set_bind_diagnostics(Some(vec![]));
        source_file_as_source_file.set_bind_suggestion_diagnostics(None);
        source_file_as_source_file.set_language_version(language_version);
        source_file_as_source_file.set_file_name(file_name.to_owned());
        source_file_as_source_file.set_language_variant(get_language_variant(script_kind));
        source_file_as_source_file.set_is_declaration_file(is_declaration_file);
        source_file_as_source_file.set_script_kind(script_kind);

        source_file
    }

    pub(super) fn set_context_flag(&self, val: bool, flag: NodeFlags) {
        if val {
            self.set_context_flags(self.context_flags() | flag);
        } else {
            self.set_context_flags(self.context_flags() & !flag);
        }
    }

    pub(super) fn set_disallow_in_context(&self, val: bool) {
        self.set_context_flag(val, NodeFlags::DisallowInContext);
    }

    pub(super) fn set_yield_context(&self, val: bool) {
        self.set_context_flag(val, NodeFlags::YieldContext);
    }

    pub(super) fn set_decorator_context(&self, val: bool) {
        self.set_context_flag(val, NodeFlags::DecoratorContext);
    }

    pub(super) fn set_await_context(&self, val: bool) {
        self.set_context_flag(val, NodeFlags::AwaitContext);
    }

    pub(super) fn do_outside_of_context<TReturn, TFunc: FnOnce() -> TReturn>(
        &self,
        context: NodeFlags,
        func: TFunc,
    ) -> TReturn {
        let context_flags_to_clear = context & self.context_flags();
        if context_flags_to_clear != NodeFlags::None {
            self.set_context_flag(false, context_flags_to_clear);
            let result = func();
            self.set_context_flag(true, context_flags_to_clear);
            return result;
        }

        func()
    }

    pub(super) fn do_inside_of_context<TReturn, TFunc: FnOnce() -> TReturn>(
        &self,
        context: NodeFlags,
        func: TFunc,
    ) -> TReturn {
        let context_flags_to_set = context & !self.context_flags();
        if context_flags_to_set != NodeFlags::None {
            self.set_context_flag(true, context_flags_to_set);
            let result = func();
            self.set_context_flag(false, context_flags_to_set);
            return result;
        }

        func()
    }

    pub(super) fn allow_in_and<TReturn, TFunc: FnOnce() -> TReturn>(&self, func: TFunc) -> TReturn {
        self.do_outside_of_context(NodeFlags::DisallowInContext, func)
    }

    pub(super) fn disallow_in_and<TReturn, TFunc: FnOnce() -> TReturn>(
        &self,
        func: TFunc,
    ) -> TReturn {
        self.do_inside_of_context(NodeFlags::DisallowInContext, func)
    }

    pub(super) fn do_in_yield_context<TReturn, TFunc: FnOnce() -> TReturn>(
        &self,
        func: TFunc,
    ) -> TReturn {
        self.do_inside_of_context(NodeFlags::YieldContext, func)
    }

    pub(super) fn do_in_decorator_context<TReturn, TFunc: FnOnce() -> TReturn>(
        &self,
        func: TFunc,
    ) -> TReturn {
        self.do_inside_of_context(NodeFlags::DecoratorContext, func)
    }

    pub(super) fn do_in_await_context<TReturn, TFunc: FnOnce() -> TReturn>(
        &self,
        func: TFunc,
    ) -> TReturn {
        self.do_inside_of_context(NodeFlags::AwaitContext, func)
    }

    pub(super) fn do_outside_of_await_context<TReturn, TFunc: FnOnce() -> TReturn>(
        &self,
        func: TFunc,
    ) -> TReturn {
        self.do_outside_of_context(NodeFlags::AwaitContext, func)
    }

    pub(super) fn do_in_yield_and_await_context<TReturn, TFunc: FnOnce() -> TReturn>(
        &self,
        func: TFunc,
    ) -> TReturn {
        self.do_inside_of_context(NodeFlags::YieldContext | NodeFlags::AwaitContext, func)
    }

    pub(super) fn do_ouside_of_yield_and_await_context<TReturn, TFunc: FnOnce() -> TReturn>(
        &self,
        func: TFunc,
    ) -> TReturn {
        self.do_outside_of_context(NodeFlags::YieldContext | NodeFlags::AwaitContext, func)
    }

    pub(super) fn in_context(&self, flags: NodeFlags) -> bool {
        self.context_flags().intersects(flags)
    }

    pub(super) fn in_yield_context(&self) -> bool {
        self.in_context(NodeFlags::YieldContext)
    }

    pub(super) fn in_disallow_in_context(&self) -> bool {
        self.in_context(NodeFlags::DisallowInContext)
    }

    pub(super) fn in_decorator_context(&self) -> bool {
        self.in_context(NodeFlags::DecoratorContext)
    }

    pub(super) fn in_await_context(&self) -> bool {
        self.in_context(NodeFlags::AwaitContext)
    }

    pub(super) fn parse_error_at_current_token(
        &self,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        self.parse_error_at(
            self.scanner().get_token_pos().try_into().unwrap(),
            self.scanner().get_text_pos().try_into().unwrap(),
            message,
            args,
        );
    }

    pub(super) fn parse_error_at_position(
        &self,
        start: isize,
        length: isize,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        {
            let mut parse_diagnostics = self.parse_diagnostics();
            let last_error = last_or_undefined(&*parse_diagnostics);
            if last_error.map_or(true, |last_error| last_error.start() != start) {
                let file_name = self.file_name().to_string();
                parse_diagnostics.push(Rc::new(
                    create_detached_diagnostic(&file_name, start, length, message, args).into(),
                ));
            }
        }

        self.set_parse_error_before_next_finished_node(true);
    }

    pub(super) fn parse_error_at(
        &self,
        start: isize,
        end: isize,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        self.parse_error_at_position(start, end - start, message, args);
    }

    pub(super) fn parse_error_at_range<TRange: TextRange>(
        &self,
        range: &TRange,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        self.parse_error_at(range.pos(), range.end(), message, args);
    }

    pub(super) fn scan_error(&self, message: &DiagnosticMessage, length: usize) {
        self.parse_error_at_position(
            self.scanner().get_text_pos().try_into().unwrap(),
            length.try_into().unwrap(),
            message,
            None,
        );
    }

    pub(super) fn get_node_pos(&self) -> isize {
        self.scanner().get_start_pos().try_into().unwrap()
    }

    pub(super) fn has_preceding_jsdoc_comment(&self) -> bool {
        self.scanner().has_preceding_jsdoc_comment()
    }

    pub(super) fn token(&self) -> SyntaxKind {
        self.current_token()
    }

    pub(super) fn next_token_without_check(&self) -> SyntaxKind {
        let current_token = self
            .scanner()
            .scan(Some(&|message, length| self.scan_error(message, length)));
        self.set_current_token(current_token);
        self.current_token()
    }

    pub(super) fn next_token_and<TReturn, TCallback: FnOnce() -> TReturn>(
        &self,
        func: TCallback,
    ) -> TReturn {
        self.next_token();
        func()
    }

    pub(super) fn next_token(&self) -> SyntaxKind {
        if matches!(self.maybe_current_token(), Some(current_token) if is_keyword(current_token))
            && (self.scanner().has_unicode_escape() || self.scanner().has_extended_unicode_escape())
        {
            self.parse_error_at(
                self.scanner().get_token_pos().try_into().unwrap(),
                self.scanner().get_text_pos().try_into().unwrap(),
                &Diagnostics::Keywords_cannot_contain_escape_characters,
                None,
            );
        }
        self.next_token_without_check()
    }

    pub(super) fn next_token_jsdoc(&self) -> SyntaxKind /*JSDocSyntaxKind*/ {
        let current_token = self
            .scanner()
            .scan_js_doc_token(Some(&|message, length| self.scan_error(message, length)));
        self.set_current_token(current_token);
        self.current_token()
    }

    pub(super) fn re_scan_greater_token(&self) -> SyntaxKind {
        self.set_current_token(self.scanner().re_scan_greater_token());
        self.current_token()
    }

    pub(super) fn re_scan_slash_token(&self) -> SyntaxKind {
        self.set_current_token(
            self.scanner()
                .re_scan_slash_token(Some(&|message, length| self.scan_error(message, length))),
        );
        self.current_token()
    }

    pub(super) fn re_scan_template_token(&self, is_tagged_template: bool) -> SyntaxKind {
        self.set_current_token(self.scanner().re_scan_template_token(
            Some(&|message, length| self.scan_error(message, length)),
            is_tagged_template,
        ));
        self.current_token()
    }

    pub(super) fn re_scan_template_head_or_no_substitution_template(&self) -> SyntaxKind {
        self.set_current_token(
            self.scanner()
                .re_scan_template_head_or_no_substitution_template(Some(&|message, length| {
                    self.scan_error(message, length)
                })),
        );
        self.current_token()
    }

    pub(super) fn re_scan_less_than_token(&self) -> SyntaxKind {
        self.set_current_token(self.scanner().re_scan_less_than_token());
        self.current_token()
    }

    pub(super) fn re_scan_hash_token(&self) -> SyntaxKind {
        self.set_current_token(self.scanner().re_scan_hash_token());
        self.current_token()
    }

    pub(super) fn scan_jsx_identifier(&self) -> SyntaxKind {
        self.set_current_token(
            self.scanner()
                .scan_jsx_identifier(Some(&|message, length| self.scan_error(message, length))),
        );
        self.current_token()
    }

    pub(super) fn scan_jsx_text(&self) -> SyntaxKind {
        self.set_current_token(self.scanner().scan_jsx_token(
            Some(&|message, length| self.scan_error(message, length)),
            None,
        ));
        self.current_token()
    }

    pub(super) fn scan_jsx_attribute_value(&self) -> SyntaxKind {
        self.set_current_token(
            self.scanner()
                .scan_jsx_attribute_value(Some(&|message, length| {
                    self.scan_error(message, length)
                })),
        );
        self.current_token()
    }

    pub(super) fn speculation_helper<TReturn, TCallback: FnOnce() -> Option<TReturn>>(
        &self,
        callback: TCallback,
        speculation_kind: SpeculationKind,
    ) -> Option<TReturn> {
        let save_token = self.current_token();
        let save_parse_diagnostics_length = self.parse_diagnostics().len();
        let save_parse_error_before_next_finished_node =
            self.parse_error_before_next_finished_node();

        let save_context_flags = self.context_flags();

        let result = if speculation_kind != SpeculationKind::TryParse {
            self.scanner().look_ahead(callback)
        } else {
            self.scanner().try_scan(callback)
        };

        Debug_.assert(save_context_flags == self.context_flags(), None);

        if result.is_none() || speculation_kind != SpeculationKind::TryParse {
            self.set_current_token(save_token);
            if speculation_kind != SpeculationKind::Reparse {
                self.parse_diagnostics()
                    .truncate(save_parse_diagnostics_length);
            }
            self.set_parse_error_before_next_finished_node(
                save_parse_error_before_next_finished_node,
            );
        }

        result
    }

    pub(super) fn look_ahead<TReturn, TCallback: FnOnce() -> Option<TReturn>>(
        &self,
        callback: TCallback,
    ) -> Option<TReturn> {
        self.speculation_helper(callback, SpeculationKind::Lookahead)
    }

    pub(super) fn look_ahead_bool<TCallback: FnOnce() -> bool>(&self, callback: TCallback) -> bool {
        self.look_ahead(|| if callback() { Some(()) } else { None })
            .is_some()
    }

    pub(super) fn try_parse<TReturn, TCallback: FnOnce() -> Option<TReturn>>(
        &self,
        callback: TCallback,
    ) -> Option<TReturn> {
        self.speculation_helper(callback, SpeculationKind::TryParse)
    }

    pub(super) fn try_parse_bool<TCallback: FnOnce() -> bool>(&self, callback: TCallback) -> bool {
        self.try_parse(|| if callback() { Some(()) } else { None })
            .is_some()
    }

    pub(super) fn is_binding_identifier(&self) -> bool {
        if self.token() == SyntaxKind::Identifier {
            return true;
        }

        self.token() > SyntaxKind::LastReservedWord
    }

    pub(super) fn is_identifier(&self) -> bool {
        if self.token() == SyntaxKind::Identifier {
            return true;
        }

        if self.token() == SyntaxKind::YieldKeyword && self.in_yield_context() {
            return false;
        }

        if self.token() == SyntaxKind::AwaitKeyword && self.in_await_context() {
            return false;
        }

        self.token() > SyntaxKind::LastReservedWord
    }

    pub(super) fn parse_expected(
        &self,
        kind: SyntaxKind,
        diagnostic_message: Option<&DiagnosticMessage>,
        should_advance: Option<bool>,
    ) -> bool {
        let should_advance = should_advance.unwrap_or(true);
        if self.token() == kind {
            if should_advance {
                self.next_token();
            }
            return true;
        }

        if let Some(diagnostic_message) = diagnostic_message {
            self.parse_error_at_current_token(diagnostic_message, None);
        } else {
            self.parse_error_at_current_token(
                &Diagnostics::_0_expected,
                token_to_string(kind).map(|string| vec![string.to_owned()]),
            );
        }
        false
    }

    pub(super) fn parse_error_for_missing_semicolon_after(
        &self,
        node: &Node, /*Expression | PropertyName*/
    ) {
        if is_tagged_template_expression(node) {
            let node_as_tagged_template_expression = node.as_tagged_template_expression();
            self.parse_error_at(
                skip_trivia(
                    self.source_text_as_chars(),
                    node_as_tagged_template_expression.template.pos(),
                    None,
                    None,
                    None,
                ),
                node_as_tagged_template_expression.end(),
                &Diagnostics::Module_declaration_names_may_only_use_or_quoted_strings,
                None,
            );
            return;
        }

        let expression_text = if is_identifier(node) {
            Some(id_text(node))
        } else {
            None
        };

        if match expression_text.as_ref() {
            None => true,
            Some(expression_text) => {
                !is_identifier_text(expression_text, Some(self.language_version()), None)
            }
        } {
            self.parse_error_at_current_token(
                &Diagnostics::_0_expected,
                Some(vec![token_to_string(SyntaxKind::SemicolonToken)
                    .unwrap()
                    .to_owned()]),
            );
            return;
        }
        let expression_text = expression_text.unwrap();

        let pos = skip_trivia(self.source_text_as_chars(), node.pos(), None, None, None);

        match expression_text.as_str() {
            "const" | "let" | "var" => {
                self.parse_error_at(
                    pos,
                    node.end(),
                    &Diagnostics::Variable_declaration_not_allowed_at_this_location,
                    None,
                );
                return;
            }
            "declare" => {
                return;
            }
            "interface" => {
                self.parse_error_for_invalid_name(
                    &Diagnostics::Interface_name_cannot_be_0,
                    &Diagnostics::Interface_must_be_given_a_name,
                    SyntaxKind::OpenBraceToken,
                );
                return;
            }
            "is" => {
                self.parse_error_at(
                    pos,
                    self.scanner().get_text_pos().try_into().unwrap(),
                    &Diagnostics::A_type_predicate_is_only_allowed_in_return_type_position_for_functions_and_methods,
                    None,
                );
                return;
            }
            "module" | "namespace" => {
                self.parse_error_for_invalid_name(
                    &Diagnostics::Namespace_name_cannot_be_0,
                    &Diagnostics::Namespace_must_be_given_a_name,
                    SyntaxKind::OpenBraceToken,
                );
                return;
            }
            "type" => {
                self.parse_error_for_invalid_name(
                    &Diagnostics::Type_alias_name_cannot_be_0,
                    &Diagnostics::Type_alias_must_be_given_a_name,
                    SyntaxKind::EqualsToken,
                );
                return;
            }
            _ => (),
        }

        let suggestion: Option<String> =
            get_spelling_suggestion(&expression_text, &viable_keyword_suggestions, |n| {
                Some((*n).to_owned())
            })
            .map(|suggestion| (*suggestion).to_owned())
            .or_else(|| self.get_space_suggestion(&expression_text));
        if let Some(suggestion) = suggestion {
            self.parse_error_at(
                pos,
                node.end(),
                &Diagnostics::Unknown_keyword_or_identifier_Did_you_mean_0,
                Some(vec![suggestion]),
            );
            return;
        }

        if self.token() == SyntaxKind::Unknown {
            return;
        }

        self.parse_error_at(
            pos,
            node.end(),
            &Diagnostics::Unexpected_keyword_or_identifier,
            None,
        );
    }

    pub(super) fn parse_error_for_invalid_name(
        &self,
        name_diagnostic: &DiagnosticMessage,
        blank_diagnostic: &DiagnosticMessage,
        token_if_blank_name: SyntaxKind,
    ) {
        if self.token() == token_if_blank_name {
            self.parse_error_at_current_token(blank_diagnostic, None);
        } else {
            self.parse_error_at_current_token(
                name_diagnostic,
                Some(vec![self.scanner().get_token_value()]),
            );
        }
    }

    pub(super) fn get_space_suggestion(&self, expression_text: &str) -> Option<String> {
        for keyword in viable_keyword_suggestions.iter() {
            if expression_text.len() > keyword.len() + 2 && starts_with(expression_text, keyword) {
                return Some(format!("{} {}", keyword, &expression_text[keyword.len()..]));
            }
        }

        None
    }

    pub(super) fn parse_semicolon_after_property_name<
        TType: Borrow<Node>,
        TInitializer: Borrow<Node>,
    >(
        &self,
        name: &Node, /*PropertyName*/
        type_: Option<TType /*TypeNode*/>,
        initializer: Option<TInitializer /*Expression*/>,
    ) {
        if self.token() == SyntaxKind::AtToken && !self.scanner().has_preceding_line_break() {
            self.parse_error_at_current_token(&Diagnostics::Decorators_must_precede_the_name_and_all_keywords_of_property_declarations, None);
            return;
        }

        if self.token() == SyntaxKind::OpenParenToken {
            self.parse_error_at_current_token(
                &Diagnostics::Cannot_start_a_function_call_in_a_type_annotation,
                None,
            );
            self.next_token();
            return;
        }

        if type_.is_some() && !self.can_parse_semicolon() {
            if initializer.is_some() {
                self.parse_error_at_current_token(
                    &Diagnostics::_0_expected,
                    Some(vec![token_to_string(SyntaxKind::SemicolonToken)
                        .unwrap()
                        .to_owned()]),
                );
            } else {
                self.parse_error_at_current_token(
                    &Diagnostics::Expected_for_property_initializer,
                    None,
                );
            }
            return;
        }

        if self.try_parse_semicolon() {
            return;
        }

        if initializer.is_some() {
            if self.token() == SyntaxKind::OpenBraceToken {
                self.parse_error_at_current_token(
                    &Diagnostics::_0_expected,
                    Some(vec![token_to_string(SyntaxKind::SemicolonToken)
                        .unwrap()
                        .to_owned()]),
                );
            }

            return;
        }

        self.parse_error_for_missing_semicolon_after(name);
    }

    pub(super) fn parse_expected_jsdoc(&self, kind: SyntaxKind /*JSDocSyntaxKind*/) -> bool {
        if self.token() == kind {
            self.next_token_jsdoc();
            return true;
        }
        self.parse_error_at_current_token(
            &Diagnostics::_0_expected,
            token_to_string(kind).map(|string| vec![string.to_owned()]),
        );
        false
    }

    pub(super) fn parse_optional(&self, t: SyntaxKind) -> bool {
        if self.token() == t {
            self.next_token();
            return true;
        }
        false
    }

    pub(super) fn parse_optional_token(&self, t: SyntaxKind) -> Option<Node> {
        if self.token() == t {
            return Some(self.parse_token_node().into());
        }
        None
    }

    pub(super) fn parse_optional_token_jsdoc(&self, t: SyntaxKind) -> Option<Node> {
        if self.token() == t {
            return Some(self.parse_token_node_jsdoc().into());
        }
        None
    }

    pub(super) fn parse_expected_token(
        &self,
        t: SyntaxKind,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Node {
        self.parse_optional_token(t).unwrap_or_else(|| {
            self.create_missing_node(
                t,
                false,
                diagnostic_message.unwrap_or(&Diagnostics::_0_expected),
                Some(args.unwrap_or_else(|| vec![token_to_string(t).unwrap().to_owned()])),
            )
            .into()
        })
    }

    pub(super) fn parse_expected_token_jsdoc(&self, t: SyntaxKind) -> Node {
        self.parse_optional_token_jsdoc(t).unwrap_or_else(|| {
            self.create_missing_node(
                t,
                false,
                &Diagnostics::_0_expected,
                Some(vec![token_to_string(t).unwrap().to_owned()]),
            )
            .into()
        })
    }

    pub(super) fn parse_token_node(&self) -> BaseNode {
        let pos = self.get_node_pos();
        let kind = self.token();
        self.next_token();
        self.finish_node(self.factory.create_token(self, kind), pos, None)
    }

    pub(super) fn parse_token_node_jsdoc(&self) -> BaseNode {
        let pos = self.get_node_pos();
        let kind = self.token();
        self.next_token_jsdoc();
        self.finish_node(self.factory.create_token(self, kind), pos, None)
    }

    pub(super) fn can_parse_semicolon(&self) -> bool {
        if self.token() == SyntaxKind::SemicolonToken {
            return true;
        }

        matches!(
            self.token(),
            SyntaxKind::CloseBraceToken | SyntaxKind::EndOfFileToken
        ) || self.scanner().has_preceding_line_break()
    }

    pub(super) fn try_parse_semicolon(&self) -> bool {
        if !self.can_parse_semicolon() {
            return false;
        }

        if self.token() == SyntaxKind::SemicolonToken {
            self.next_token();
        }

        true
    }

    pub(super) fn parse_semicolon(&self) -> bool {
        self.try_parse_semicolon() || self.parse_expected(SyntaxKind::SemicolonToken, None, None)
    }

    pub(super) fn create_node_array(
        &self,
        elements: Vec<Node>,
        pos: isize,
        end: Option<isize>,
        has_trailing_comma: Option<bool>,
    ) -> NodeArray {
        let array = self.factory.create_node_array(
            Some(
                elements
                    .into_iter()
                    .map(Node::wrap)
                    .collect::<Vec<Rc<Node>>>(),
            ),
            has_trailing_comma,
        );
        set_text_range_pos_end(
            &array,
            pos,
            end.unwrap_or_else(|| self.scanner().get_start_pos().try_into().unwrap()),
        );
        array
    }

    pub(super) fn finish_node<TNode: NodeInterface>(
        &self,
        node: TNode,
        pos: isize,
        end: Option<isize>,
    ) -> TNode {
        set_text_range_pos_end(
            &node,
            pos,
            end.unwrap_or_else(|| self.scanner().get_start_pos().try_into().unwrap()),
        );
        if self.context_flags() != NodeFlags::None {
            node.set_flags(node.flags() | self.context_flags());
        }

        if self.parse_error_before_next_finished_node() {
            self.set_parse_error_before_next_finished_node(false);
            node.set_flags(node.flags() | NodeFlags::ThisNodeHasError);
        }

        node
    }

    pub(super) fn create_missing_node(
        &self,
        kind: SyntaxKind,
        report_at_current_position: bool,
        diagnostic_message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Node {
        if report_at_current_position {
            self.parse_error_at_position(
                self.scanner().get_start_pos().try_into().unwrap(),
                0,
                diagnostic_message,
                args,
            );
        } else
        /*if diagnostic_message*/
        {
            self.parse_error_at_current_token(diagnostic_message, args);
        }

        let pos = self.get_node_pos();
        let result = if kind == SyntaxKind::Identifier {
            self.factory
                .create_identifier(self, "", Option::<NodeArray>::None, None)
                .into()
        } else if is_template_literal_kind(kind) {
            self.factory
                .create_template_literal_like_node(
                    self,
                    kind,
                    "".to_owned(),
                    Some("".to_owned()),
                    None,
                )
                .into()
        } else if kind == SyntaxKind::NumericLiteral {
            self.factory
                .create_numeric_literal(self, "".to_owned(), None)
                .into()
        } else if kind == SyntaxKind::StringLiteral {
            self.factory
                .create_string_literal(self, "".to_owned(), None, None)
                .into()
        } else if kind == SyntaxKind::MissingDeclaration {
            self.factory.create_missing_declaration(self).into()
        } else {
            self.factory.create_token(self, kind).into()
        };
        self.finish_node(result, pos, None)
    }

    pub(super) fn intern_identifier(&self, text: &str) -> String {
        let identifiers = self.identifiers();
        let mut identifiers = identifiers.borrow_mut();
        let mut identifier: Option<String> = identifiers
            .get(text)
            .map(|identifier| identifier.to_owned());
        if identifier.is_none() {
            identifier = Some(text.to_owned());
            identifiers.insert(text.to_owned(), identifier.clone().unwrap());
        }
        identifier.unwrap()
    }

    pub(super) fn create_identifier(
        &self,
        is_identifier: bool,
        diagnostic_message: Option<&DiagnosticMessage>,
        private_identifier_diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Node /*Identifier*/ {
        if is_identifier {
            self.increment_identifier_count();
            let pos = self.get_node_pos();
            let original_keyword_kind = self.token();
            let text = self.intern_identifier(&self.scanner().get_token_value());
            self.next_token_without_check();
            return self.finish_node(
                self.factory
                    .create_identifier(
                        self,
                        &text,
                        Option::<NodeArray>::None,
                        Some(original_keyword_kind),
                    )
                    .into(),
                pos,
                None,
            );
        }

        if self.token() == SyntaxKind::PrivateIdentifier {
            self.parse_error_at_current_token(
                private_identifier_diagnostic_message.unwrap_or(
                    &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                ),
                None,
            );
            return self.create_identifier(true, None, None);
        }

        {
            let scanner = self.scanner();
            if self.token() == SyntaxKind::Unknown
                && scanner
                    .try_scan(|| {
                        Some(
                            scanner.re_scan_invalid_identifier(Some(&|message, length| {
                                self.scan_error(message, length)
                            })) == SyntaxKind::Identifier,
                        )
                    })
                    .unwrap()
            {
                return self.create_identifier(true, None, None);
            }
        }

        self.increment_identifier_count();
        let report_at_current_position = self.token() == SyntaxKind::EndOfFileToken;

        let is_reserved_word = self.scanner().is_reserved_word();
        let msg_arg = self.scanner().get_token_text();

        let default_message = if is_reserved_word {
            &Diagnostics::Identifier_expected_0_is_a_reserved_word_that_cannot_be_used_here
        } else {
            &Diagnostics::Identifier_expected
        };

        self.create_missing_node(
            SyntaxKind::Identifier,
            report_at_current_position,
            diagnostic_message.unwrap_or(default_message),
            Some(vec![msg_arg]),
        )
    }

    pub(super) fn parse_binding_identifier(
        &self,
        private_identifier_diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Node {
        self.create_identifier(
            self.is_binding_identifier(),
            None,
            private_identifier_diagnostic_message,
        )
    }

    pub(super) fn parse_identifier(
        &self,
        diagnostic_message: Option<&DiagnosticMessage>,
        private_identifier_diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Node {
        self.create_identifier(
            self.is_identifier(),
            diagnostic_message,
            private_identifier_diagnostic_message,
        )
    }

    pub(super) fn parse_identifier_name(
        &self,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Node {
        self.create_identifier(
            token_is_identifier_or_keyword(self.token()),
            diagnostic_message,
            None,
        )
    }

    pub(super) fn is_literal_property_name(&self) -> bool {
        token_is_identifier_or_keyword(self.token())
            || matches!(
                self.token(),
                SyntaxKind::StringLiteral | SyntaxKind::NumericLiteral
            )
    }

    pub(super) fn is_assertion_key(&self) -> bool {
        token_is_identifier_or_keyword(self.token()) || self.token() == SyntaxKind::StringLiteral
    }

    pub(super) fn parse_property_name_worker(&self, allow_computed_property_names: bool) -> Node /*PropertyName*/
    {
        if matches!(
            self.token(),
            SyntaxKind::StringLiteral | SyntaxKind::NumericLiteral
        ) {
            let node = self.parse_literal_node();
            let node_as_literal_like_node = node.as_literal_like_node();
            node_as_literal_like_node
                .set_text(self.intern_identifier(&*node_as_literal_like_node.text()));
            return node;
        }
        if allow_computed_property_names && self.token() == SyntaxKind::OpenBracketToken {
            return self.parse_computed_property_name().into();
        }
        if self.token() == SyntaxKind::PrivateIdentifier {
            return self.parse_private_identifier();
        }
        self.parse_identifier_name(None)
    }

    pub(super) fn parse_property_name(&self) -> Node /*PropertyName*/ {
        self.parse_property_name_worker(true)
    }

    pub(super) fn parse_computed_property_name(&self) -> ComputedPropertyName {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBracketToken, None, None);
        let expression = self.allow_in_and(|| self.parse_expression());
        self.parse_expected(SyntaxKind::CloseBracketToken, None, None);
        self.finish_node(
            self.factory
                .create_computed_property_name(self, expression.wrap()),
            pos,
            None,
        )
    }

    pub(super) fn intern_private_identifier(&self, text: &str) -> String {
        let private_identifiers = self.private_identifiers();
        let mut private_identifiers = private_identifiers.borrow_mut();
        let mut private_identifier: Option<String> = private_identifiers
            .get(text)
            .map(|private_identifier| private_identifier.to_owned());
        if private_identifier.is_none() {
            private_identifier = Some(text.to_owned());
            private_identifiers.insert(text.to_owned(), private_identifier.clone().unwrap());
        }
        private_identifier.unwrap()
    }

    pub(super) fn parse_private_identifier(&self) -> Node {
        let pos = self.get_node_pos();
        let node = self.factory.create_private_identifier(
            self,
            &self.intern_private_identifier(&self.scanner().get_token_text()),
        );
        self.next_token();
        self.finish_node(node.into(), pos, None)
    }

    pub(super) fn next_token_is_on_same_line_and_can_follow_modifier(&self) -> bool {
        self.next_token();
        if self.scanner().has_preceding_line_break() {
            return false;
        }
        self.can_follow_modifier()
    }

    pub(super) fn next_token_can_follow_modifier(&self) -> bool {
        match self.token() {
            SyntaxKind::ConstKeyword => self.next_token() == SyntaxKind::EnumKeyword,
            SyntaxKind::ExportKeyword => {
                self.next_token();
                if self.token() == SyntaxKind::DefaultKeyword {
                    return self.look_ahead_bool(|| self.next_token_can_follow_default_keyword());
                }
                if self.token() == SyntaxKind::TypeKeyword {
                    return self.look_ahead_bool(|| self.next_token_can_follow_export_modifier());
                }
                self.can_follow_export_modifier()
            }
            SyntaxKind::DefaultKeyword => self.next_token_can_follow_default_keyword(),
            SyntaxKind::StaticKeyword | SyntaxKind::GetKeyword | SyntaxKind::SetKeyword => {
                self.next_token();
                self.can_follow_modifier()
            }
            _ => self.next_token_is_on_same_line_and_can_follow_modifier(),
        }
    }

    pub(super) fn can_follow_export_modifier(&self) -> bool {
        self.token() != SyntaxKind::AsteriskToken
            && self.token() != SyntaxKind::AsKeyword
            && self.token() != SyntaxKind::OpenBraceToken
            && self.can_follow_modifier()
    }

    pub(super) fn next_token_can_follow_export_modifier(&self) -> bool {
        self.next_token();
        self.can_follow_export_modifier()
    }

    pub(super) fn parse_any_contextual_modifier(&self) -> bool {
        is_modifier_kind(self.token())
            && self.try_parse_bool(|| self.next_token_can_follow_modifier())
    }
}

lazy_static! {
    static ref viable_keyword_suggestions: Vec<&'static str> = text_to_keyword_obj
        .keys()
        .filter(|key| key.len() > 2)
        .copied()
        .collect();
}

pub struct IncrementalParserSyntaxCursorReparseTopLevelAwait {
    base_syntax_cursor: Rc<IncrementalParserSyntaxCursor>,
}

impl IncrementalParserSyntaxCursorReparseTopLevelAwait {
    pub fn new(base_syntax_cursor: Rc<IncrementalParserSyntaxCursor>) -> Self {
        Self { base_syntax_cursor }
    }
}

impl IncrementalParserSyntaxCursorInterface for IncrementalParserSyntaxCursorReparseTopLevelAwait {
    fn current_node(&self, parser: &ParserType, position: usize) -> Option<Rc<Node>> {
        let node = self.base_syntax_cursor.current_node(parser, position);
        if parser.top_level() {
            if let Some(node) = node.as_ref() {
                if parser.contains_possible_top_level_await(node) {
                    node.set_intersects_change(Some(true));
                }
            }
        }
        node
    }
}

impl From<IncrementalParserSyntaxCursorReparseTopLevelAwait> for IncrementalParserSyntaxCursor {
    fn from(value: IncrementalParserSyntaxCursorReparseTopLevelAwait) -> Self {
        Self::ReparseTopLevelAwait(value)
    }
}
