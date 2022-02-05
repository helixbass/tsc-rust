#![allow(non_upper_case_globals)]

use std::convert::TryInto;
use std::rc::Rc;

use super::{MissingNode, ParserType, ParsingContext, SpeculationKind};
use crate::{
    attach_file_to_diagnostics, create_detached_diagnostic, is_modifier_kind,
    is_template_literal_kind, last_or_undefined, set_text_range_pos_end,
    token_is_identifier_or_keyword, token_to_string, BaseNode, Debug_, DiagnosticMessage,
    DiagnosticRelatedInformationInterface, Diagnostics, Identifier, Node, NodeArray,
    NodeArrayOrVec, NodeFlags, NodeInterface, SourceFile, SyntaxKind,
};
use local_macros::enum_unwrapped;

impl ParserType {
    pub(super) fn parse_source_file_worker(&self) -> Rc<Node /*SourceFile*/> {
        let source_flags = self.context_flags();

        self.next_token();

        let statements =
            self.parse_list(ParsingContext::SourceElements, ParserType::parse_statement);
        Debug_.assert(self.token() == SyntaxKind::EndOfFileToken, None);
        let end_of_file_token = self.parse_token_node();

        let source_file = self.create_source_file(
            self.file_name(),
            statements,
            end_of_file_token.into(),
            source_flags,
        );
        let source_file: Rc<Node> = source_file.into();
        source_file
            .as_source_file()
            .set_parse_diagnostics(attach_file_to_diagnostics(
                &*self.parse_diagnostics(),
                &source_file,
            ));

        source_file
    }

    pub(super) fn create_source_file<TNodes: Into<NodeArrayOrVec>>(
        &self,
        file_name: &str,
        statements: TNodes,
        end_of_file_token: Rc<Node /*EndOfFileToken*/>,
        flags: NodeFlags,
    ) -> SourceFile {
        let mut source_file =
            self.factory
                .create_source_file(self, statements, end_of_file_token, flags);

        source_file.text = self.source_text().to_string();
        source_file.set_file_name(file_name.to_string());

        source_file
    }

    pub(super) fn set_context_flag(&self, val: bool, flag: NodeFlags) {
        if val {
            self.set_context_flags(self.context_flags() | flag);
        } else {
            self.set_context_flags(self.context_flags() & !flag);
        }
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

    pub(super) fn do_in_await_context<TReturn, TFunc: FnOnce() -> TReturn>(
        &self,
        func: TFunc,
    ) -> TReturn {
        self.do_inside_of_context(NodeFlags::AwaitContext, func)
    }

    pub(super) fn in_context(&self, flags: NodeFlags) -> bool {
        self.context_flags().intersects(flags)
    }

    pub(super) fn in_yield_context(&self) -> bool {
        self.in_context(NodeFlags::YieldContext)
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

    pub(super) fn get_node_pos(&self) -> isize {
        self.scanner().get_start_pos().try_into().unwrap()
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

    pub(super) fn next_token_and<TReturn>(&self, func: fn(&ParserType) -> TReturn) -> TReturn {
        self.next_token();
        func(self)
    }

    pub(super) fn next_token(&self) -> SyntaxKind {
        self.next_token_without_check()
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

    pub(super) fn speculation_helper<TReturn, TCallback: FnOnce() -> Option<TReturn>>(
        &self,
        callback: TCallback,
        speculation_kind: SpeculationKind,
    ) -> Option<TReturn> {
        let save_token = self.current_token();
        let save_parse_diagnostics_length = self.parse_diagnostics().len();
        let save_parse_error_before_next_finished_node =
            self.parse_error_before_next_finished_node();

        let result = if speculation_kind != SpeculationKind::TryParse {
            self.scanner().look_ahead(callback)
        } else {
            self.scanner().try_scan(callback)
        };

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
                token_to_string(kind).map(|string| vec![string.to_string()]),
            );
        }
        false
    }

    pub(super) fn parse_error_for_missing_semicolon_after(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn parse_optional_token(&self, t: SyntaxKind) -> Option<Node> {
        if self.token() == t {
            return Some(self.parse_token_node().into());
        }
        None
    }

    pub(super) fn parse_optional(&self, t: SyntaxKind) -> bool {
        if self.token() == t {
            self.next_token();
            return true;
        }
        false
    }

    pub(super) fn parse_expected_token(
        &self,
        t: SyntaxKind,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Node {
        self.parse_optional_token(t).unwrap_or_else(|| {
            enum_unwrapped!(
                self.create_missing_node(
                    t,
                    false,
                    diagnostic_message.unwrap_or(&Diagnostics::_0_expected),
                    Some(args.unwrap_or_else(|| vec![token_to_string(t).unwrap().to_string()])),
                ),
                [MissingNode, TemplateLiteralLikeNode]
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

    pub(super) fn can_parse_semicolon(&self) -> bool {
        if self.token() == SyntaxKind::SemicolonToken {
            return true;
        }

        self.token() == SyntaxKind::CloseBraceToken
            || self.token() == SyntaxKind::EndOfFileToken
            || self.scanner().has_preceding_line_break()
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
        // set_text_range_pos_end(
        //     array,
        //     pos,
        //     end.unwrap_or_else(|| self.scanner().get_start_pos()),
        // );
        array
    }

    pub(super) fn finish_node<TParsedNode: NodeInterface>(
        &self,
        mut node: TParsedNode,
        pos: isize,
        end: Option<isize>,
    ) -> TParsedNode {
        set_text_range_pos_end(
            &mut node,
            pos,
            end.unwrap_or_else(|| self.scanner().get_start_pos().try_into().unwrap()),
        );

        if self.parse_error_before_next_finished_node() {
            self.set_parse_error_before_next_finished_node(false);
        }

        node
    }

    pub(super) fn create_missing_node(
        &self,
        kind: SyntaxKind,
        report_at_current_position: bool,
        diagnostic_message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> MissingNode {
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
            MissingNode::Identifier(self.factory.create_identifier(
                self,
                "",
                Option::<NodeArray>::None,
                None,
            ))
        } else if is_template_literal_kind(kind) {
            MissingNode::TemplateLiteralLikeNode(self.factory.create_template_literal_like_node(
                self,
                kind,
                "".to_string(),
                Some("".to_string()),
                None,
            ))
        } else {
            unimplemented!()
        };
        self.finish_node(result, pos, None)
    }

    pub(super) fn intern_identifier(&self, text: &str) -> String {
        text.to_string()
    }

    pub(super) fn create_identifier(
        &self,
        is_identifier: bool,
        diagnostic_message: Option<&DiagnosticMessage>,
        private_identifier_diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Identifier {
        if is_identifier {
            let pos = self.get_node_pos();
            let original_keyword_kind = self.token();
            let text = self.intern_identifier(&self.scanner().get_token_value());
            self.next_token_without_check();
            return self.finish_node(
                self.factory.create_identifier(
                    self,
                    &text,
                    Option::<NodeArray>::None,
                    Some(original_keyword_kind),
                ),
                pos,
                None,
            );
        }

        let report_at_current_position = self.token() == SyntaxKind::EndOfFileToken;

        let msg_arg = self.scanner().get_token_text();

        let default_message = if false {
            unimplemented!()
        } else {
            Diagnostics::Identifier_expected
        };

        enum_unwrapped!(
            self.create_missing_node(
                SyntaxKind::Identifier,
                report_at_current_position,
                diagnostic_message.unwrap_or(&default_message),
                Some(vec![msg_arg]),
            ),
            [MissingNode, Identifier]
        )
    }

    pub(super) fn parse_binding_identifier(
        &self,
        private_identifier_diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Identifier {
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
    ) -> Identifier {
        self.create_identifier(
            self.is_identifier(),
            diagnostic_message,
            private_identifier_diagnostic_message,
        )
    }

    pub(super) fn parse_identifier_name(
        &self,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Identifier {
        self.create_identifier(
            token_is_identifier_or_keyword(self.token()),
            diagnostic_message,
            None,
        )
    }

    pub(super) fn is_literal_property_name(&self) -> bool {
        token_is_identifier_or_keyword(self.token())
            || self.token() == SyntaxKind::StringLiteral
            || self.token() == SyntaxKind::NumericLiteral
    }

    pub(super) fn parse_property_name_worker(&self) -> Node /*PropertyName*/ {
        self.parse_identifier_name(None).into()
    }

    pub(super) fn parse_property_name(&self) -> Node /*PropertyName*/ {
        self.parse_property_name_worker()
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
