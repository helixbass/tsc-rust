#![allow(non_upper_case_globals)]

use std::convert::TryInto;

use super::{ParserType, ParsingContext};
use crate::{
    is_literal_kind, is_template_literal_kind, token_to_string, Debug_, DiagnosticMessage,
    Diagnostics, LiteralLikeNode, LiteralLikeNodeInterface, Node, NodeArray, NodeInterface,
    SyntaxKind, TemplateExpression, TemplateSpan, TokenFlags, TypeNode,
};

impl ParserType {
    pub(super) fn can_follow_modifier(&self) -> bool {
        self.token() == SyntaxKind::OpenBracketToken
            || self.token() == SyntaxKind::OpenBraceToken
            || self.token() == SyntaxKind::AsteriskToken
            || self.token() == SyntaxKind::DotDotDotToken
            || self.is_literal_property_name()
    }

    pub(super) fn next_token_can_follow_default_keyword(&self) -> bool {
        self.next_token();
        self.token() == SyntaxKind::ClassKeyword
            || self.token() == SyntaxKind::FunctionKeyword
            || self.token() == SyntaxKind::InterfaceKeyword
            || self.token() == SyntaxKind::AbstractKeyword
                && self.look_ahead_bool(|| self.next_token_is_class_keyword_on_same_line())
            || self.token() == SyntaxKind::AsyncKeyword
                && self.look_ahead_bool(|| self.next_token_is_function_keyword_on_same_line())
    }

    pub(super) fn is_list_element(&self, kind: ParsingContext) -> bool {
        match kind {
            ParsingContext::SourceElements | ParsingContext::BlockStatements => {
                self.is_start_of_statement()
            }
            ParsingContext::TypeMembers => self.look_ahead_bool(|| self.is_type_member_start()),
            ParsingContext::ObjectLiteralMembers => match self.token() {
                SyntaxKind::OpenBracketToken
                | SyntaxKind::AsteriskToken
                | SyntaxKind::DotDotDotToken
                | SyntaxKind::DotToken => true,
                _ => self.is_literal_property_name(),
            },
            ParsingContext::VariableDeclarations => {
                self.is_binding_identifier_or_private_identifier_or_pattern()
            }
            ParsingContext::TypeParameters => self.is_identifier(),
            ParsingContext::ArrayLiteralMembers => {
                self.token() == SyntaxKind::CommaToken
                    || self.token() == SyntaxKind::DotToken
                    || self.token() == SyntaxKind::DotDotDotToken
                    || self.is_start_of_expression()
            }
            ParsingContext::Parameters => self.is_start_of_parameter(false),
            ParsingContext::TypeArguments => {
                self.token() == SyntaxKind::CommaToken || self.is_start_of_type(None)
            }
            _ => unimplemented!(),
        }
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
            if self.is_list_element(kind) {
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
            if self.is_list_element(kind) {
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
            self.parse_identifier_name(diagnostic_message).into()
        } else {
            self.parse_identifier(diagnostic_message, None).into()
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

    pub(super) fn parse_literal_node(&self) -> LiteralLikeNode {
        self.parse_literal_like_node(self.token())
    }

    pub(super) fn parse_template_head(&self, is_tagged_template: bool) -> LiteralLikeNode /*TemplateHead*/
    {
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

    pub(super) fn parse_template_middle_or_template_tail(&self) -> LiteralLikeNode /*TemplateMiddle | TemplateTail*/
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

    pub(super) fn parse_literal_like_node(&self, kind: SyntaxKind) -> LiteralLikeNode {
        let pos = self.get_node_pos();
        let mut node: LiteralLikeNode = if is_template_literal_kind(kind) {
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

        if self.scanner().has_extended_unicode_escape() {
            node.set_has_extended_unicode_escape(Some(true));
        }

        if self.scanner().is_unterminated() {
            node.set_is_unterminated(Some(true));
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

    pub(super) fn parse_type_reference(&self) -> TypeNode {
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
