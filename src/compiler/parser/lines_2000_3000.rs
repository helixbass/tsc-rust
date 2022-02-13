#![allow(non_upper_case_globals)]

use std::convert::TryInto;
use std::rc::Rc;

use super::{ParserType, ParsingContext};
use crate::{
    contains_parse_error, is_keyword, is_literal_kind, is_template_literal_kind, node_is_missing,
    token_is_identifier_or_keyword, token_is_identifier_or_keyword_or_greater_than,
    token_to_string, BaseJSDocUnaryType, BaseNode, Debug_, DiagnosticMessage, Diagnostics,
    HasInitializerInterface, IncrementalParserSyntaxCursorInterface, NamedDeclarationInterface,
    Node, NodeArray, NodeFlags, NodeInterface, QualifiedName, ReadonlyTextRange, SyntaxKind,
    TemplateExpression, TemplateLiteralTypeNode, TemplateLiteralTypeSpan, TemplateSpan,
    ThisTypeNode, TokenFlags, TypePredicateNode,
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
            | ParsingContext::SwitchClauses
            | ParsingContext::TypeMembers
            | ParsingContext::ClassMembers
            | ParsingContext::EnumMembers
            | ParsingContext::ObjectLiteralMembers
            | ParsingContext::ObjectBindingElements
            | ParsingContext::ImportOrExportSpecifiers
            | ParsingContext::AssertEntries => self.token() == SyntaxKind::CloseBraceToken,
            ParsingContext::SwitchClauseStatements => matches!(
                self.token(),
                SyntaxKind::CloseBraceToken | SyntaxKind::CaseKeyword | SyntaxKind::DefaultKeyword
            ),
            ParsingContext::HeritageClauseElement => matches!(
                self.token(),
                SyntaxKind::OpenBraceToken
                    | SyntaxKind::ExtendsKeyword
                    | SyntaxKind::ImplementsKeyword
            ),
            ParsingContext::VariableDeclarations => self.is_variable_declarator_list_terminator(),
            ParsingContext::TypeParameters => {
                matches!(
                    self.token(),
                    SyntaxKind::GreaterThanToken
                        | SyntaxKind::OpenParenToken
                        | SyntaxKind::OpenBraceToken
                        | SyntaxKind::ExtendsKeyword
                        | SyntaxKind::ImplementsKeyword
                )
            }
            ParsingContext::ArgumentExpressions => {
                matches!(
                    self.token(),
                    SyntaxKind::CloseParenToken | SyntaxKind::SemicolonToken
                )
            }
            ParsingContext::ArrayLiteralMembers
            | ParsingContext::TupleElementTypes
            | ParsingContext::ArrayBindingElements => self.token() == SyntaxKind::CloseBracketToken,
            ParsingContext::JSDocParameters
            | ParsingContext::Parameters
            | ParsingContext::RestProperties => {
                matches!(
                    self.token(),
                    SyntaxKind::CloseParenToken | SyntaxKind::CloseBracketToken
                )
            }
            ParsingContext::TypeArguments => self.token() != SyntaxKind::CommaToken,
            ParsingContext::HeritageClauses => {
                matches!(
                    self.token(),
                    SyntaxKind::OpenBraceToken | SyntaxKind::CloseBraceToken
                )
            }
            ParsingContext::JsxAttributes => {
                matches!(
                    self.token(),
                    SyntaxKind::GreaterThanToken | SyntaxKind::SlashToken
                )
            }
            ParsingContext::JsxChildren => {
                self.token() == SyntaxKind::LessThanToken
                    && self.look_ahead_bool(|| self.next_token_is_slash())
            }
            _ => false,
        }
    }

    pub(super) fn is_variable_declarator_list_terminator(&self) -> bool {
        if self.can_parse_semicolon() {
            return true;
        }

        if self.is_in_or_of_keyword(self.token()) {
            return true;
        }

        if self.token() == SyntaxKind::EqualsGreaterThanToken {
            return true;
        }

        false
    }

    pub(super) fn is_in_some_parsing_context(&self) -> bool {
        let max = ParsingContext::Count;
        let mut shift = 0;
        let mut kind = ParsingContext::from_bits(1 << shift).unwrap();
        while kind < max {
            if self.parsing_context().intersects(kind) {
                if self.is_list_element(kind, true) || self.is_list_terminator(kind) {
                    return true;
                }
            }
            shift += 1;
            kind = ParsingContext::from_bits(1 << shift).unwrap();
        }

        false
    }

    pub(super) fn parse_list<TParseElement: FnMut() -> Rc<Node>>(
        &self,
        kind: ParsingContext,
        parse_element: &mut TParseElement,
    ) -> NodeArray {
        let save_parsing_context = self.parsing_context();
        self.set_parsing_context(self.parsing_context() | kind);
        let mut list = vec![];
        let list_pos = self.get_node_pos();

        while !self.is_list_terminator(kind) {
            if self.is_list_element(kind, false) {
                list.push(self.parse_list_element(kind, parse_element));

                continue;
            }

            if self.abort_parsing_list_or_move_to_next_token(kind) {
                break;
            }
        }

        self.set_parsing_context(save_parsing_context);
        self.create_node_array(list, list_pos, None, None)
    }

    pub(super) fn parse_list_element<TParseElement: FnMut() -> Rc<Node>>(
        &self,
        parsing_context: ParsingContext,
        parse_element: &mut TParseElement,
    ) -> Rc<Node> {
        let node = self.current_node(parsing_context);
        if let Some(node) = node {
            return self.consume_node(node);
        }

        parse_element()
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

    pub(super) fn consume_node(&self, node: Rc<Node>) -> Rc<Node> {
        self.scanner_mut()
            .set_text_pos(node.end().try_into().unwrap());
        self.next_token();
        node
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
        match parsing_context {
            ParsingContext::ClassMembers => self.is_reusable_class_member(node),

            ParsingContext::SwitchClauses => self.is_reusable_switch_clause(node),

            ParsingContext::SourceElements
            | ParsingContext::BlockStatements
            | ParsingContext::SwitchClauseStatements => self.is_reusable_statement(node),

            ParsingContext::EnumMembers => self.is_reusable_enum_member(node),

            ParsingContext::TypeMembers => self.is_reusable_type_member(node),

            ParsingContext::VariableDeclarations => self.is_reusable_variable_declaration(node),

            ParsingContext::JSDocParameters | ParsingContext::Parameters => {
                self.is_reusable_parameter(node)
            }

            _ => false,
        }
    }

    pub(super) fn is_reusable_class_member(&self, node: &Node) -> bool {
        // if (node) {
        match node.kind() {
            SyntaxKind::Constructor
            | SyntaxKind::IndexSignature
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::PropertyDeclaration
            | SyntaxKind::SemicolonClassElement => {
                return true;
            }
            SyntaxKind::MethodDeclaration => {
                let method_declaration = node.as_method_declaration();
                let name_is_constructor = method_declaration.name().kind()
                    == SyntaxKind::Identifier
                    && matches!(
                        method_declaration
                            .name()
                            .as_identifier()
                            .original_keyword_kind,
                        Some(SyntaxKind::ConstructorKeyword)
                    );

                return !name_is_constructor;
            }
            _ => (),
        }
        // }

        false
    }

    pub(super) fn is_reusable_switch_clause(&self, node: &Node) -> bool {
        // if (node) {
        match node.kind() {
            SyntaxKind::CaseClause | SyntaxKind::DefaultClause => {
                return true;
            }
            _ => (),
        }
        // }

        false
    }

    pub(super) fn is_reusable_statement(&self, node: &Node) -> bool {
        // if (node) {
        match node.kind() {
            SyntaxKind::FunctionDeclaration
            | SyntaxKind::VariableStatement
            | SyntaxKind::Block
            | SyntaxKind::IfStatement
            | SyntaxKind::ExpressionStatement
            | SyntaxKind::ThrowStatement
            | SyntaxKind::ReturnStatement
            | SyntaxKind::SwitchStatement
            | SyntaxKind::BreakStatement
            | SyntaxKind::ContinueStatement
            | SyntaxKind::ForInStatement
            | SyntaxKind::ForOfStatement
            | SyntaxKind::ForStatement
            | SyntaxKind::WhileStatement
            | SyntaxKind::WithStatement
            | SyntaxKind::EmptyStatement
            | SyntaxKind::TryStatement
            | SyntaxKind::LabeledStatement
            | SyntaxKind::DoStatement
            | SyntaxKind::DebuggerStatement
            | SyntaxKind::ImportDeclaration
            | SyntaxKind::ImportEqualsDeclaration
            | SyntaxKind::ExportDeclaration
            | SyntaxKind::ExportAssignment
            | SyntaxKind::ModuleDeclaration
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::InterfaceDeclaration
            | SyntaxKind::EnumDeclaration
            | SyntaxKind::TypeAliasDeclaration => {
                return true;
            }
            _ => (),
        }
        // }

        false
    }

    pub(super) fn is_reusable_enum_member(&self, node: &Node) -> bool {
        node.kind() == SyntaxKind::EnumMember
    }

    pub(super) fn is_reusable_type_member(&self, node: &Node) -> bool {
        // if (node) {
        match node.kind() {
            SyntaxKind::ConstructSignature
            | SyntaxKind::MethodSignature
            | SyntaxKind::IndexSignature
            | SyntaxKind::PropertySignature
            | SyntaxKind::CallSignature => {
                return true;
            }
            _ => (),
        }
        // }

        false
    }

    pub(super) fn is_reusable_variable_declaration(&self, node: &Node) -> bool {
        if node.kind() != SyntaxKind::VariableDeclaration {
            return false;
        }

        let variable_declarator = node.as_variable_declaration();
        variable_declarator.maybe_initializer().is_none()
    }

    pub(super) fn is_reusable_parameter(&self, node: &Node) -> bool {
        if node.kind() != SyntaxKind::Parameter {
            return false;
        }

        let parameter = node.as_parameter_declaration();
        parameter.maybe_initializer().is_none()
    }

    pub(super) fn abort_parsing_list_or_move_to_next_token(&self, kind: ParsingContext) -> bool {
        self.parsing_context_errors(kind);
        if self.is_in_some_parsing_context() {
            return true;
        }

        self.next_token();
        false
    }

    pub(super) fn parsing_context_errors(&self, context: ParsingContext) {
        match context {
            ParsingContext::SourceElements => {
                if self.token() == SyntaxKind::DefaultKeyword {
                    self.parse_error_at_current_token(
                        &Diagnostics::_0_expected,
                        Some(vec![token_to_string(SyntaxKind::ExportKeyword)
                            .unwrap()
                            .to_owned()]),
                    );
                } else {
                    self.parse_error_at_current_token(
                        &Diagnostics::Declaration_or_statement_expected,
                        None,
                    );
                }
            }
            ParsingContext::BlockStatements => {
                self.parse_error_at_current_token(
                    &Diagnostics::Declaration_or_statement_expected,
                    None,
                );
            }
            ParsingContext::SwitchClauses => {
                self.parse_error_at_current_token(&Diagnostics::case_or_default_expected, None);
            }
            ParsingContext::SwitchClauseStatements => {
                self.parse_error_at_current_token(&Diagnostics::Statement_expected, None);
            }
            ParsingContext::RestProperties | ParsingContext::TypeMembers => {
                self.parse_error_at_current_token(
                    &Diagnostics::Property_or_signature_expected,
                    None,
                );
            }
            ParsingContext::ClassMembers => {
                self.parse_error_at_current_token(&Diagnostics::Unexpected_token_A_constructor_method_accessor_or_property_was_expected, None);
            }
            ParsingContext::EnumMembers => {
                self.parse_error_at_current_token(&Diagnostics::Enum_member_expected, None);
            }
            ParsingContext::HeritageClauseElement => {
                self.parse_error_at_current_token(&Diagnostics::Expression_expected, None);
            }
            ParsingContext::VariableDeclarations => {
                if is_keyword(self.token()) {
                    self.parse_error_at_current_token(
                        &Diagnostics::_0_is_not_allowed_as_a_variable_declaration_name,
                        Some(vec![token_to_string(self.token()).unwrap().to_owned()]),
                    );
                } else {
                    self.parse_error_at_current_token(
                        &Diagnostics::Variable_declaration_expected,
                        None,
                    );
                }
            }
            ParsingContext::ObjectBindingElements => {
                self.parse_error_at_current_token(
                    &Diagnostics::Property_destructuring_pattern_expected,
                    None,
                );
            }
            ParsingContext::ArrayBindingElements => {
                self.parse_error_at_current_token(
                    &Diagnostics::Array_element_destructuring_pattern_expected,
                    None,
                );
            }
            ParsingContext::ArgumentExpressions => {
                self.parse_error_at_current_token(&Diagnostics::Argument_expression_expected, None);
            }
            ParsingContext::ObjectLiteralMembers => {
                self.parse_error_at_current_token(&Diagnostics::Property_assignment_expected, None);
            }
            ParsingContext::ArrayLiteralMembers => {
                self.parse_error_at_current_token(&Diagnostics::Expression_or_comma_expected, None);
            }
            ParsingContext::JSDocParameters => {
                self.parse_error_at_current_token(
                    &Diagnostics::Parameter_declaration_expected,
                    None,
                );
            }
            ParsingContext::Parameters => {
                if is_keyword(self.token()) {
                    self.parse_error_at_current_token(
                        &Diagnostics::_0_is_not_allowed_as_a_parameter_name,
                        Some(vec![token_to_string(self.token()).unwrap().to_owned()]),
                    );
                } else {
                    self.parse_error_at_current_token(
                        &Diagnostics::Parameter_declaration_expected,
                        None,
                    );
                }
            }
            ParsingContext::TypeParameters => {
                self.parse_error_at_current_token(
                    &Diagnostics::Type_parameter_declaration_expected,
                    None,
                );
            }
            ParsingContext::TypeArguments => {
                self.parse_error_at_current_token(&Diagnostics::Type_argument_expected, None);
            }
            ParsingContext::TupleElementTypes => {
                self.parse_error_at_current_token(&Diagnostics::Type_expected, None);
            }
            ParsingContext::HeritageClauses => {
                self.parse_error_at_current_token(&Diagnostics::Unexpected_token_expected, None);
            }
            ParsingContext::ImportOrExportSpecifiers => {
                self.parse_error_at_current_token(&Diagnostics::Identifier_expected, None);
            }
            ParsingContext::JsxAttributes => {
                self.parse_error_at_current_token(&Diagnostics::Identifier_expected, None);
            }
            ParsingContext::JsxChildren => {
                self.parse_error_at_current_token(&Diagnostics::Identifier_expected, None);
            }
            _ => (),
        }
    }

    pub(super) fn parse_delimited_list<TParseElement: FnMut() -> Rc<Node>>(
        &self,
        kind: ParsingContext,
        mut parse_element: TParseElement,
        consider_semicolon_as_delimiter: Option<bool>,
    ) -> NodeArray {
        let consider_semicolon_as_delimiter = consider_semicolon_as_delimiter.unwrap_or(false);
        let save_parsing_context = self.parsing_context();
        self.set_parsing_context(self.parsing_context() | kind);
        let mut list: Vec<Rc<Node>> = vec![];
        let list_pos = self.get_node_pos();

        let mut comma_start: Option<usize> = None;
        loop {
            if self.is_list_element(kind, false) {
                let start_pos = self.scanner().get_start_pos();
                list.push(self.parse_list_element(kind, &mut parse_element));
                comma_start = Some(self.scanner().get_token_pos());

                if self.parse_optional(SyntaxKind::CommaToken) {
                    continue;
                }

                comma_start = None;
                if self.is_list_terminator(kind) {
                    break;
                }

                self.parse_expected(
                    SyntaxKind::CommaToken,
                    self.get_expected_comma_diagnostic(kind),
                    None,
                );

                if consider_semicolon_as_delimiter
                    && self.token() == SyntaxKind::SemicolonToken
                    && !self.scanner().has_preceding_line_break()
                {
                    self.next_token();
                }
                if start_pos == self.scanner().get_start_pos() {
                    self.next_token();
                }
                continue;
            }

            if self.is_list_terminator(kind) {
                break;
            }

            if self.abort_parsing_list_or_move_to_next_token(kind) {
                break;
            }
        }

        self.set_parsing_context(save_parsing_context);
        self.create_node_array(list, list_pos, None, Some(comma_start.is_some()))
    }

    pub(super) fn get_expected_comma_diagnostic(
        &self,
        kind: ParsingContext,
    ) -> Option<&'static DiagnosticMessage> {
        if kind == ParsingContext::EnumMembers {
            Some(&Diagnostics::An_enum_member_name_must_be_followed_by_a_or)
        } else {
            None
        }
    }

    pub(super) fn create_missing_list(&self) -> NodeArray {
        let mut list = self.create_node_array(vec![], self.get_node_pos(), None, None);
        list.is_missing_list = true;
        list
    }

    pub(super) fn is_missing_list(&self, arr: &NodeArray) -> bool {
        arr.is_missing_list
    }

    pub(super) fn parse_bracketed_list<TParseElement: FnMut() -> Rc<Node>>(
        &self,
        kind: ParsingContext,
        parse_element: TParseElement,
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
        let mut entity: Node /*EntityName*/ = if allow_reserved_words {
            self.parse_identifier_name(diagnostic_message)
        } else {
            self.parse_identifier(diagnostic_message, None)
        };
        let mut dot_pos = self.get_node_pos();
        while self.parse_optional(SyntaxKind::DotToken) {
            if self.token() == SyntaxKind::LessThanToken {
                entity
                    .as_has_jsdoc_dot_pos()
                    .set_jsdoc_dot_pos(Some(dot_pos));
                break;
            }
            dot_pos = self.get_node_pos();
            entity = self.finish_node(
                self.factory
                    .create_qualified_name(
                        self,
                        entity.wrap(),
                        self.parse_right_side_of_dot(allow_reserved_words, false)
                            .wrap(),
                    )
                    .into(),
                pos,
                None,
            );
        }
        entity
    }

    pub(super) fn create_qualified_name(
        &self,
        entity: Rc<Node /*EntityName*/>,
        name: Rc<Node /*Identifier*/>,
    ) -> QualifiedName {
        self.finish_node(
            self.factory
                .create_qualified_name(self, entity.clone(), name),
            entity.pos(),
            None,
        )
    }

    pub(super) fn parse_right_side_of_dot(
        &self,
        allow_identifier_names: bool,
        allow_private_identifiers: bool,
    ) -> Node /*Identifier | PrivateIdentifier*/ {
        if self.scanner().has_preceding_line_break() && token_is_identifier_or_keyword(self.token())
        {
            let matches_pattern =
                self.look_ahead_bool(|| self.next_token_is_identifier_or_keyword_on_same_line());

            if matches_pattern {
                return self.create_missing_node(
                    SyntaxKind::Identifier,
                    true,
                    Some(&Diagnostics::Identifier_expected),
                    None,
                );
            }
        }

        if self.token() == SyntaxKind::PrivateIdentifier {
            let node = self.parse_private_identifier();
            return if allow_private_identifiers {
                node
            } else {
                self.create_missing_node(
                    SyntaxKind::Identifier,
                    true,
                    Some(&Diagnostics::Identifier_expected),
                    None,
                )
            };
        }

        if allow_identifier_names {
            self.parse_identifier_name(None)
        } else {
            self.parse_identifier(None, None)
        }
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
                self.parse_template_head(is_tagged_template).wrap(),
                self.parse_template_spans(is_tagged_template),
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_template_type(&self) -> TemplateLiteralTypeNode {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory.create_template_literal_type(
                self,
                self.parse_template_head(false).wrap(),
                self.parse_template_type_spans(),
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_template_type_spans(&self) -> NodeArray /*<TemplateLiteralTypeSpan>*/ {
        let pos = self.get_node_pos();
        let mut list = vec![];
        let mut node: TemplateLiteralTypeSpan;
        while {
            node = self.parse_template_type_span();
            let is_node_template_middle = node.literal.kind() == SyntaxKind::TemplateMiddle;
            list.push(node.into());
            is_node_template_middle
        } {}
        self.create_node_array(list, pos, None, None)
    }

    pub(super) fn parse_template_type_span(&self) -> TemplateLiteralTypeSpan {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory.create_template_literal_type_span(
                self,
                self.parse_type().wrap(),
                self.parse_literal_of_template_span(false).wrap(),
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_literal_of_template_span(&self, is_tagged_template: bool) -> Node /*TemplateMiddle | TemplateTail*/
    {
        if self.token() == SyntaxKind::CloseBraceToken {
            self.re_scan_template_token(is_tagged_template);
            self.parse_template_middle_or_template_tail()
        } else {
            self.parse_expected_token(
                SyntaxKind::TemplateTail,
                Some(&Diagnostics::_0_expected),
                Some(vec![token_to_string(SyntaxKind::CloseBraceToken)
                    .unwrap()
                    .to_owned()]),
            )
        }
    }

    pub(super) fn parse_template_span(&self, is_tagged_template: bool) -> TemplateSpan {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory.create_template_span(
                self,
                self.allow_in_and(|| self.parse_expression()),
                self.parse_literal_of_template_span(is_tagged_template)
                    .wrap(),
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
            matches!(
                fragment.kind(),
                SyntaxKind::TemplateMiddle | SyntaxKind::TemplateTail
            ),
            Some("Template fragment has wrong token kind"),
        );
        fragment
    }

    pub(super) fn get_template_literal_raw_text(
        &self,
        kind: SyntaxKind, /*TemplateLiteralToken["kind"]*/
    ) -> String {
        let is_last = matches!(
            kind,
            SyntaxKind::NoSubstitutionTemplateLiteral | SyntaxKind::TemplateTail
        );
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
        let node: Node = if is_template_literal_kind(kind) {
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
                || self.parse_type().wrap(),
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

    pub(super) fn type_has_arrow_function_blocking_parse_error(
        &self,
        node: &Node, /*TypeNode*/
    ) -> bool {
        match node.kind() {
            SyntaxKind::TypeReference => {
                node_is_missing(Some(&*node.as_type_reference_node().type_name))
            }
            SyntaxKind::FunctionType | SyntaxKind::ConstructorType => {
                let node_as_signature_declaration = node.as_signature_declaration();
                let parameters = node_as_signature_declaration.parameters();
                let type_ = node_as_signature_declaration.maybe_type().unwrap();
                self.is_missing_list(parameters)
                    || self.type_has_arrow_function_blocking_parse_error(&type_)
            }
            SyntaxKind::ParenthesizedType => self.type_has_arrow_function_blocking_parse_error(
                &node.as_parenthesized_type_node().type_,
            ),
            _ => false,
        }
    }

    pub(super) fn parse_this_type_predicate(
        &self,
        lhs: Rc<Node /*ThisTypeNode*/>,
    ) -> TypePredicateNode {
        self.next_token();
        self.finish_node(
            self.factory.create_type_predicate_node(
                self,
                None,
                lhs.clone(),
                Some(self.parse_type().wrap()),
            ),
            lhs.pos(),
            None,
        )
    }

    pub(super) fn parse_this_type_node(&self) -> ThisTypeNode {
        let pos = self.get_node_pos();
        self.next_token();
        self.finish_node(self.factory.create_this_type_node(self), pos, None)
    }

    pub(super) fn parse_jsdoc_all_type(&self) -> BaseNode {
        let pos = self.get_node_pos();
        self.next_token();
        self.finish_node(self.factory.create_jsdoc_all_type(self), pos, None)
    }

    pub(super) fn parse_jsdoc_non_nullable_type(&self) -> BaseJSDocUnaryType {
        let pos = self.get_node_pos();
        self.next_token();
        self.finish_node(
            self.factory
                .create_jsdoc_non_nullable_type(self, Some(self.parse_non_array_type().wrap())),
            pos,
            None,
        )
    }

    pub(super) fn parse_jsdoc_unknown_or_nullable_type(&self) -> BaseJSDocUnaryType {
        let pos = self.get_node_pos();
        self.next_token();

        if matches!(
            self.token(),
            SyntaxKind::CommaToken
                | SyntaxKind::CloseBraceToken
                | SyntaxKind::CloseParenToken
                | SyntaxKind::GreaterThanToken
                | SyntaxKind::EqualsToken
                | SyntaxKind::BarToken
        ) {
            self.finish_node(
                self.factory.create_jsdoc_unknown_type(self, None),
                pos,
                None,
            )
        } else {
            self.finish_node(
                self.factory
                    .create_jsdoc_nullable_type(self, Some(self.parse_type().wrap())),
                pos,
                None,
            )
        }
    }
}
