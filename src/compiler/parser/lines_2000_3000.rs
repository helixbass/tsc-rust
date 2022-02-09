#![allow(non_upper_case_globals)]

use std::convert::TryInto;
use std::rc::Rc;

use super::{ParserType, ParsingContext};
use crate::{
    contains_parse_error, is_literal_kind, is_template_literal_kind, node_is_missing,
    token_is_identifier_or_keyword, token_is_identifier_or_keyword_or_greater_than,
    token_to_string, Debug_, DiagnosticMessage, Diagnostics,
    IncrementalParserSyntaxCursorInterface, Node, NodeArray, NodeFlags, NodeInterface, SyntaxKind,
    TemplateExpression, TemplateSpan, TokenFlags,
};

impl ParserType {
    pub(super) fn can_follow_modifier(&self) -> bool {
        matches!(
            self.token(),
            SyntaxKind::OpenBracketToken
                | SyntaxKind::OpenBraceToken
                | SyntaxKind::AsteriskToken
                | SyntaxKind::DotDotDotToken
        ) || self.is_literal_property_name()
    }

    pub(super) fn next_token_can_follow_default_keyword(&self) -> bool {
        self.next_token();
        matches!(
            self.token(),
            SyntaxKind::ClassKeyword | SyntaxKind::FunctionKeyword | SyntaxKind::InterfaceKeyword
        ) || self.token() == SyntaxKind::AbstractKeyword
            && self.look_ahead_bool(|| self.next_token_is_class_keyword_on_same_line())
            || self.token() == SyntaxKind::AsyncKeyword
                && self.look_ahead_bool(|| self.next_token_is_function_keyword_on_same_line())
    }

    pub(super) fn is_list_element(
        &self,
        parsing_context: ParsingContext,
        in_error_recovery: bool,
    ) -> bool {
        let node = self.current_node(parsing_context);
        if node.is_some() {
            return true;
        }

        match parsing_context {
            ParsingContext::SourceElements
            | ParsingContext::BlockStatements
            | ParsingContext::SwitchClauseStatements => {
                !(self.token() == SyntaxKind::SemicolonToken && in_error_recovery)
                    && self.is_start_of_statement()
            }
            ParsingContext::SwitchClauses => matches!(
                self.token(),
                SyntaxKind::CaseKeyword | SyntaxKind::DefaultKeyword
            ),
            ParsingContext::TypeMembers => self.look_ahead_bool(|| self.is_type_member_start()),
            ParsingContext::ClassMembers => {
                self.look_ahead_bool(|| self.is_class_member_start())
                    || self.token() == SyntaxKind::SemicolonToken && !in_error_recovery
            }
            ParsingContext::EnumMembers => {
                self.token() == SyntaxKind::OpenBracketToken || self.is_literal_property_name()
            }
            ParsingContext::ObjectLiteralMembers => match self.token() {
                SyntaxKind::OpenBracketToken
                | SyntaxKind::AsteriskToken
                | SyntaxKind::DotDotDotToken
                | SyntaxKind::DotToken => true,
                _ => self.is_literal_property_name(),
            },
            ParsingContext::RestProperties => self.is_literal_property_name(),
            ParsingContext::ObjectBindingElements => {
                matches!(
                    self.token(),
                    SyntaxKind::OpenBracketToken | SyntaxKind::DotDotDotToken
                ) || self.is_literal_property_name()
            }
            ParsingContext::AssertEntries => self.is_assertion_key(),
            ParsingContext::HeritageClauseElement => {
                if self.token() == SyntaxKind::OpenBraceToken {
                    return self.look_ahead_bool(|| self.is_valid_heritage_clause_object_literal());
                }

                if !in_error_recovery {
                    self.is_start_of_left_hand_side_expression()
                        && !self.is_heritage_clause_extends_or_implements_keyword()
                } else {
                    self.is_identifier() && !self.is_heritage_clause_extends_or_implements_keyword()
                }
            }
            ParsingContext::VariableDeclarations => {
                self.is_binding_identifier_or_private_identifier_or_pattern()
            }
            ParsingContext::ArrayBindingElements => {
                matches!(
                    self.token(),
                    SyntaxKind::CommaToken | SyntaxKind::DotDotDotToken
                ) || self.is_binding_identifier_or_private_identifier_or_pattern()
            }
            ParsingContext::TypeParameters => self.is_identifier(),
            ParsingContext::ArrayLiteralMembers => {
                matches!(
                    self.token(),
                    SyntaxKind::CommaToken | SyntaxKind::DotToken | SyntaxKind::DotDotDotToken
                ) || self.is_start_of_expression()
            }
            ParsingContext::ArgumentExpressions => {
                self.token() == SyntaxKind::DotDotDotToken || self.is_start_of_expression()
            }
            ParsingContext::Parameters => self.is_start_of_parameter(false),
            ParsingContext::JSDocParameters => self.is_start_of_parameter(true),
            ParsingContext::TypeArguments | ParsingContext::TupleElementTypes => {
                self.token() == SyntaxKind::CommaToken || self.is_start_of_type(None)
            }
            ParsingContext::HeritageClauses => self.is_heritage_clause(),
            ParsingContext::ImportOrExportSpecifiers => {
                token_is_identifier_or_keyword(self.token())
            }
            ParsingContext::JsxAttributes => {
                token_is_identifier_or_keyword(self.token())
                    || self.token() == SyntaxKind::OpenBraceToken
            }
            ParsingContext::JsxChildren => true,
            _ => Debug_.fail(Some("Non-exhaustive case in 'isListElement'.")),
        }
    }

    pub(super) fn is_valid_heritage_clause_object_literal(&self) -> bool {
        Debug_.assert(self.token() == SyntaxKind::OpenBraceToken, None);
        if self.next_token() == SyntaxKind::CloseBraceToken {
            let next = self.next_token();
            return matches!(
                next,
                SyntaxKind::CommaToken
                    | SyntaxKind::OpenBraceToken
                    | SyntaxKind::ExtendsKeyword
                    | SyntaxKind::ImplementsKeyword
            );
        }

        true
    }

    pub(super) fn next_token_is_identifier(&self) -> bool {
        self.next_token();
        self.is_identifier()
    }

    pub(super) fn next_token_is_identifier_or_keyword(&self) -> bool {
        self.next_token();
        token_is_identifier_or_keyword(self.token())
    }

    pub(super) fn next_token_is_identifier_or_keyword_or_greater_than(&self) -> bool {
        self.next_token();
        token_is_identifier_or_keyword_or_greater_than(self.token())
    }

    pub(super) fn is_heritage_clause_extends_or_implements_keyword(&self) -> bool {
        if matches!(
            self.token(),
            SyntaxKind::ImplementsKeyword | SyntaxKind::ExtendsKeyword
        ) {
            return self.look_ahead_bool(|| self.next_token_is_start_of_expression());
        }

        false
    }

    pub(super) fn next_token_is_start_of_expression(&self) -> bool {
        self.next_token();
        self.is_start_of_expression()
    }

    pub(super) fn next_token_is_start_of_type(&self) -> bool {
        self.next_token();
        self.is_start_of_type(None)
    }

    pub(super) fn is_list_terminator(&self, kind: ParsingContext) -> bool {
        if self.token() == SyntaxKind::EndOfFileToken {
            return true;
        }
        match kind {
            ParsingContext::BlockStatements
            | ParsingContext::TypeMembers
            | ParsingContext::ObjectLiteralMembers => self.token() == SyntaxKind::CloseBraceToken,
            ParsingContext::VariableDeclarations => self.is_variable_declarator_list_terminator(),
            ParsingContext::TypeParameters => {
                self.token() == SyntaxKind::GreaterThanToken
                    || self.token() == SyntaxKind::OpenParenToken
                    || self.token() == SyntaxKind::OpenBraceToken
                    || self.token() == SyntaxKind::ExtendsKeyword
                    || self.token() == SyntaxKind::ImplementsKeyword
            }
            ParsingContext::ArrayLiteralMembers => self.token() == SyntaxKind::CloseBracketToken,
            ParsingContext::Parameters => {
                self.token() == SyntaxKind::CloseParenToken
                    || self.token() == SyntaxKind::CloseBracketToken
            }
            ParsingContext::TypeArguments => self.token() != SyntaxKind::CommaToken,
            _ => false,
        }
    }

    pub(super) fn is_variable_declarator_list_terminator(&self) -> bool {
        if self.can_parse_semicolon() {
            return true;
        }

        false
    }

    pub(super) fn parse_list<TItem: Into<Node>>(
        &self,
        kind: ParsingContext,
        parse_element: fn(&ParserType) -> TItem,
    ) -> NodeArray {
        let mut list = vec![];
        let list_pos = self.get_node_pos();

        while !self.is_list_terminator(kind) {
            if self.is_list_element(kind, false) {
                list.push(self.parse_list_element(kind, parse_element).into());

                continue;
            }

            unimplemented!()
        }

        self.create_node_array(list, list_pos, None, None)
    }

    pub(super) fn parse_list_element<TItem: Into<Node>>(
        &self,
        _parsing_context: ParsingContext,
        parse_element: fn(&ParserType) -> TItem,
    ) -> TItem {
        parse_element(self)
    }

    pub(super) fn current_node(&self, parsing_context: ParsingContext) -> Option<Rc<Node>> {
        if self.maybe_syntax_cursor().is_none()
            || !self.is_reusable_parsing_context(parsing_context)
            || self.parse_error_before_next_finished_node()
        {
            return None;
        }
        let syntax_cursor = self.syntax_cursor();

        let node = syntax_cursor.current_node(self, self.scanner().get_start_pos());

        if node_is_missing(node.clone()) {
            return None;
        }
        let node = node.unwrap();

        if matches!(node.maybe_intersects_change(), Some(true)) || contains_parse_error(&node) {
            return None;
        }

        let node_context_flags = node.flags() & NodeFlags::ContextFlags;
        if node_context_flags != self.context_flags() {
            return None;
        }

        if !self.can_reuse_node(&node, parsing_context) {
            return None;
        }

        if node.maybe_js_doc_cache().is_some() {
            node.set_js_doc_cache(None);
        }

        Some(node)
    }

    pub(super) fn is_reusable_parsing_context(&self, parsing_context: ParsingContext) -> bool {
        matches!(
            parsing_context,
            ParsingContext::ClassMembers
                | ParsingContext::SwitchClauses
                | ParsingContext::SourceElements
                | ParsingContext::BlockStatements
                | ParsingContext::SwitchClauseStatements
                | ParsingContext::EnumMembers
                | ParsingContext::TypeMembers
                | ParsingContext::VariableDeclarations
                | ParsingContext::JSDocParameters
                | ParsingContext::Parameters
        )
    }

    pub(super) fn can_reuse_node(&self, node: &Node, parsing_context: ParsingContext) -> bool {
        unimplemented!()
    }

    pub(super) fn parse_delimited_list<TItem: Into<Node>>(
        &self,
        kind: ParsingContext,
        parse_element: fn(&ParserType) -> TItem,
        consider_semicolon_as_delimiter: Option<bool>,
    ) -> NodeArray {
        let consider_semicolon_as_delimiter = consider_semicolon_as_delimiter.unwrap_or(false);
        let save_parsing_context = self.parsing_context();
        self.set_parsing_context(self.parsing_context() | kind);
        let mut list: Vec<Node> = vec![];
        let list_pos = self.get_node_pos();

        let mut comma_start: isize = -1;
        loop {
            if self.is_list_element(kind, false) {
                let start_pos = self.scanner().get_start_pos();
                list.push(self.parse_list_element(kind, parse_element).into());
                comma_start = self.scanner().get_token_pos().try_into().unwrap();

                if self.parse_optional(SyntaxKind::CommaToken) {
                    continue;
                }

                comma_start = -1;
                if self.is_list_terminator(kind) {
                    break;
                }

                unimplemented!()
            }

            if self.is_list_terminator(kind) {
                break;
            }

            unimplemented!()
        }

        self.set_parsing_context(save_parsing_context);
        self.create_node_array(list, list_pos, None, Some(comma_start >= 0))
    }

    pub(super) fn create_missing_list(&self) -> NodeArray {
        let mut list = self.create_node_array(vec![], self.get_node_pos(), None, None);
        list.is_missing_list = true;
        list
    }

    pub(super) fn parse_bracketed_list<TItem: Into<Node>>(
        &self,
        kind: ParsingContext,
        parse_element: fn(&ParserType) -> TItem,
        open: SyntaxKind,
        close: SyntaxKind,
    ) -> NodeArray {
        if self.parse_expected(open, None, None) {
            let result = self.parse_delimited_list(kind, parse_element, None);
            self.parse_expected(close, None, None);
            return result;
        }

        self.create_missing_list()
    }

    pub(super) fn parse_entity_name(
        &self,
        allow_reserved_words: bool,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Node /*EntityName*/ {
        let pos = self.get_node_pos();
        let entity: Node = if allow_reserved_words {
            self.parse_identifier_name(diagnostic_message)
        } else {
            self.parse_identifier(diagnostic_message, None)
        };
        entity
    }

    pub(super) fn parse_template_spans(&self, is_tagged_template: bool) -> NodeArray /*<TemplateSpan>*/
    {
        let pos = self.get_node_pos();
        let mut list = vec![];
        let mut node: TemplateSpan;
        while {
            node = self.parse_template_span(is_tagged_template);
            let is_node_template_middle = node.literal.kind() == SyntaxKind::TemplateMiddle;
            list.push(node.into());
            is_node_template_middle
        } {}
        self.create_node_array(list, pos, None, None)
    }

    pub(super) fn parse_template_expression(&self, is_tagged_template: bool) -> TemplateExpression {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory.create_template_expression(
                self,
                self.parse_template_head(is_tagged_template).into(),
                self.parse_template_spans(is_tagged_template),
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_literal_of_template_span(&self, is_tagged_template: bool) -> Node /*TemplateMiddle | TemplateTail*/
    {
        if self.token() == SyntaxKind::CloseBraceToken {
            self.re_scan_template_token(is_tagged_template);
            self.parse_template_middle_or_template_tail().into()
        } else {
            self.parse_expected_token(
                SyntaxKind::TemplateTail,
                Some(&Diagnostics::_0_expected),
                Some(vec![token_to_string(SyntaxKind::CloseBraceToken)
                    .unwrap()
                    .to_string()]),
            )
        }
    }

    pub(super) fn parse_template_span(&self, is_tagged_template: bool) -> TemplateSpan {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory.create_template_span(
                self,
                self.allow_in_and(|| self.parse_expression()).into(),
                self.parse_literal_of_template_span(is_tagged_template)
                    .into(),
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_literal_node(&self) -> Node {
        self.parse_literal_like_node(self.token())
    }

    pub(super) fn parse_template_head(&self, is_tagged_template: bool) -> Node /*TemplateHead*/ {
        if is_tagged_template {
            self.re_scan_template_head_or_no_substitution_template();
        }
        let fragment = self.parse_literal_like_node(self.token());
        Debug_.assert(
            fragment.kind() == SyntaxKind::TemplateHead,
            Some("Template head has wrong token kind"),
        );
        fragment
    }

    pub(super) fn parse_template_middle_or_template_tail(&self) -> Node /*TemplateMiddle | TemplateTail*/
    {
        let fragment = self.parse_literal_like_node(self.token());
        Debug_.assert(
            fragment.kind() == SyntaxKind::TemplateMiddle
                || fragment.kind() == SyntaxKind::TemplateTail,
            Some("Template fragment has wrong token kind"),
        );
        fragment
    }

    pub(super) fn get_template_literal_raw_text(&self, kind: SyntaxKind) -> String {
        let is_last =
            kind == SyntaxKind::NoSubstitutionTemplateLiteral || kind == SyntaxKind::TemplateTail;
        let token_text = self.scanner().get_token_text();
        let token_text_chars = token_text.chars();
        let token_text_chars_len = token_text_chars.clone().count();
        token_text_chars
            .skip(1)
            .take(
                token_text_chars_len
                    - (if self.scanner().is_unterminated() {
                        0
                    } else if is_last {
                        1
                    } else {
                        2
                    })
                    - 1,
            )
            .collect()
    }

    pub(super) fn parse_literal_like_node(&self, kind: SyntaxKind) -> Node {
        let pos = self.get_node_pos();
        let mut node: Node = if is_template_literal_kind(kind) {
            self.factory
                .create_template_literal_like_node(
                    self,
                    kind,
                    self.scanner().get_token_value(),
                    Some(self.get_template_literal_raw_text(kind)),
                    Some(self.scanner().get_token_flags() & TokenFlags::TemplateLiteralLikeFlags),
                )
                .into()
        } else if kind == SyntaxKind::NumericLiteral {
            self.factory
                .create_numeric_literal(
                    self,
                    self.scanner().get_token_value(),
                    Some(self.scanner().get_numeric_literal_flags()),
                )
                .into()
        } else if kind == SyntaxKind::StringLiteral {
            self.factory
                .create_string_literal(
                    self,
                    self.scanner().get_token_value(),
                    None,
                    Some(self.scanner().has_extended_unicode_escape()),
                )
                .into()
        } else if is_literal_kind(kind) {
            self.factory
                .create_literal_like_node(self, kind, self.scanner().get_token_value())
        } else {
            Debug_.fail(None)
        };

        let node_as_literal_like_node = node.as_literal_like_node();
        if self.scanner().has_extended_unicode_escape() {
            node_as_literal_like_node.set_has_extended_unicode_escape(Some(true));
        }

        if self.scanner().is_unterminated() {
            node_as_literal_like_node.set_is_unterminated(Some(true));
        }

        self.next_token();
        self.finish_node(node, pos, None)
    }

    pub(super) fn parse_entity_name_of_type_reference(&self) -> Node /*EntityName*/ {
        self.parse_entity_name(true, Some(&Diagnostics::Type_expected))
    }

    pub(super) fn parse_type_arguments_of_type_reference(
        &self,
    ) -> Option<NodeArray /*<TypeNode>*/> {
        if !self.scanner().has_preceding_line_break()
            && self.re_scan_less_than_token() == SyntaxKind::LessThanToken
        {
            return Some(self.parse_bracketed_list(
                ParsingContext::TypeArguments,
                ParserType::parse_type,
                SyntaxKind::LessThanToken,
                SyntaxKind::GreaterThanToken,
            ));
        }
        None
    }

    pub(super) fn parse_type_reference(&self) -> Node {
        let pos = self.get_node_pos();
        let name = self.parse_entity_name_of_type_reference().wrap();
        let type_arguments = self.parse_type_arguments_of_type_reference();
        self.finish_node(
            self.factory
                .create_type_reference_node(self, name, type_arguments)
                .into(),
            pos,
            None,
        )
    }
}
