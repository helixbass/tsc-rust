#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::{ParserType, ParsingContext, SignatureFlags};
use crate::{
    append, is_class_member_modifier, is_identifier, is_keyword, is_modifier_kind,
    modifiers_to_flags, set_text_range_pos, some, token_is_identifier_or_keyword,
    ArrayBindingPattern, BaseNode, BindingElement, Block, CaseBlock, CatchClause, ClassDeclaration,
    ClassExpression, Debug_, DebuggerStatement, Decorator, DefaultClause, DiagnosticMessage,
    Diagnostics, FunctionDeclaration, MethodDeclaration, ModifierFlags, Node, NodeArray, NodeFlags,
    NodeInterface, ObjectBindingPattern, SwitchStatement, SyntaxKind, ThrowStatement, TryStatement,
    VariableDeclaration, VariableDeclarationList,
};

impl ParserType {
    pub(super) fn parse_default_clause(&self) -> DefaultClause {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::DefaultKeyword, None, None);
        self.parse_expected(SyntaxKind::ColonToken, None, None);
        let statements = self.parse_list(ParsingContext::SwitchClauseStatements, &mut || {
            self.parse_statement()
        });
        self.finish_node(
            self.factory.create_default_clause(self, statements),
            pos,
            None,
        )
    }

    pub(super) fn parse_case_or_default_clause(&self) -> Node /*CaseOrDefaultClause*/ {
        if self.token() == SyntaxKind::CaseKeyword {
            self.parse_case_clause().into()
        } else {
            self.parse_default_clause().into()
        }
    }

    pub(super) fn parse_case_block(&self) -> CaseBlock {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBraceToken, None, None);
        let clauses = self.parse_list(ParsingContext::SwitchClauses, &mut || {
            self.parse_case_or_default_clause().wrap()
        });
        self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        self.finish_node(self.factory.create_case_block(self, clauses), pos, None)
    }

    pub(super) fn parse_switch_statement(&self) -> SwitchStatement {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        self.parse_expected(SyntaxKind::SwitchKeyword, None, None);
        self.parse_expected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.allow_in_and(|| self.parse_expression());
        self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        let case_block = self.parse_case_block();
        self.with_jsdoc(
            self.finish_node(
                self.factory
                    .create_switch_statement(self, expression, case_block.into()),
                pos,
                None,
            ),
            has_jsdoc,
        )
    }

    pub(super) fn parse_throw_statement(&self) -> ThrowStatement {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        self.parse_expected(SyntaxKind::ThrowKeyword, None, None);

        let mut expression: Option<Rc<Node>> = if self.scanner().has_preceding_line_break() {
            None
        } else {
            Some(self.allow_in_and(|| self.parse_expression()))
        };
        if expression.is_none() {
            self.increment_identifier_count();
            expression = Some(
                self.finish_node(
                    self.factory
                        .create_identifier(self, "", Option::<NodeArray>::None, None),
                    self.get_node_pos(),
                    None,
                )
                .into(),
            );
        }
        let expression = expression.unwrap();
        if !self.try_parse_semicolon() {
            self.parse_error_for_missing_semicolon_after(&expression);
        }
        self.with_jsdoc(
            self.finish_node(
                self.factory.create_throw_statement(self, expression),
                pos,
                None,
            ),
            has_jsdoc,
        )
    }

    pub(super) fn parse_try_statement(&self) -> TryStatement {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();

        self.parse_expected(SyntaxKind::TryKeyword, None, None);
        let try_block: Rc<Node> = self.parse_block(false, None).into();
        let catch_clause: Option<Rc<Node>> = if self.token() == SyntaxKind::CatchKeyword {
            Some(self.parse_catch_clause().into())
        } else {
            None
        };

        let mut finally_block: Option<Rc<Node>> = None;
        if catch_clause.is_none() || self.token() == SyntaxKind::FinallyKeyword {
            self.parse_expected(SyntaxKind::FinallyKeyword, None, None);
            finally_block = Some(self.parse_block(false, None).into());
        }

        self.with_jsdoc(
            self.finish_node(
                self.factory
                    .create_try_statement(self, try_block, catch_clause, finally_block),
                pos,
                None,
            ),
            has_jsdoc,
        )
    }

    pub(super) fn parse_catch_clause(&self) -> CatchClause {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::CatchKeyword, None, None);

        let variable_declaration: Option<Rc<Node>>;
        if self.parse_optional(SyntaxKind::OpenParenToken) {
            variable_declaration = Some(self.parse_variable_declaration(None).into());
            self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        } else {
            variable_declaration = None;
        }

        let block = self.parse_block(false, None);
        self.finish_node(
            self.factory
                .create_catch_clause(self, variable_declaration, block.into()),
            pos,
            None,
        )
    }

    pub(super) fn parse_debugger_statement(&self) -> DebuggerStatement {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        self.parse_expected(SyntaxKind::DebuggerKeyword, None, None);
        self.parse_semicolon();
        self.with_jsdoc(
            self.finish_node(self.factory.create_debugger_statement(self), pos, None),
            has_jsdoc,
        )
    }

    pub(super) fn parse_expression_or_labeled_statement(&self) -> Node /*ExpressionStatement | LabeledStatement*/
    {
        let pos = self.get_node_pos();
        let mut has_jsdoc = self.has_preceding_jsdoc_comment();
        let node: Node;
        let has_paren = self.token() == SyntaxKind::OpenParenToken;
        let expression = self.allow_in_and(|| self.parse_expression());
        if is_identifier(&expression) && self.parse_optional(SyntaxKind::ColonToken) {
            node = self
                .factory
                .create_labeled_statement(self, expression, self.parse_statement())
                .into();
        } else {
            if !self.try_parse_semicolon() {
                self.parse_error_for_missing_semicolon_after(&expression);
            }
            node = self
                .factory
                .create_expression_statement(self, expression)
                .into();
            if has_paren {
                has_jsdoc = false;
            }
        };
        self.with_jsdoc(self.finish_node(node, pos, None), has_jsdoc)
    }

    pub(super) fn next_token_is_identifier_or_keyword_on_same_line(&self) -> bool {
        self.next_token();
        token_is_identifier_or_keyword(self.token()) && !self.scanner().has_preceding_line_break()
    }

    pub(super) fn next_token_is_class_keyword_on_same_line(&self) -> bool {
        self.next_token();
        self.token() == SyntaxKind::ClassKeyword && !self.scanner().has_preceding_line_break()
    }

    pub(super) fn next_token_is_function_keyword_on_same_line(&self) -> bool {
        self.next_token();
        self.token() == SyntaxKind::FunctionKeyword && !self.scanner().has_preceding_line_break()
    }

    pub(super) fn next_token_is_identifier_or_keyword_or_literal_on_same_line(&self) -> bool {
        self.next_token();
        (token_is_identifier_or_keyword(self.token())
            || matches!(
                self.token(),
                SyntaxKind::NumericLiteral | SyntaxKind::BigIntLiteral | SyntaxKind::StringLiteral
            ))
            && !self.scanner().has_preceding_line_break()
    }

    pub(super) fn is_declaration(&self) -> bool {
        loop {
            match self.token() {
                SyntaxKind::VarKeyword
                | SyntaxKind::LetKeyword
                | SyntaxKind::ConstKeyword
                | SyntaxKind::FunctionKeyword
                | SyntaxKind::ClassKeyword
                | SyntaxKind::EnumKeyword => {
                    return true;
                }
                SyntaxKind::InterfaceKeyword | SyntaxKind::TypeKeyword => {
                    return self.next_token_is_identifier_on_same_line();
                }
                SyntaxKind::ModuleKeyword | SyntaxKind::NamespaceKeyword => {
                    return self.next_token_is_identifier_or_string_literal_on_same_line();
                }
                SyntaxKind::AbstractKeyword
                | SyntaxKind::AsyncKeyword
                | SyntaxKind::DeclareKeyword
                | SyntaxKind::PrivateKeyword
                | SyntaxKind::ProtectedKeyword
                | SyntaxKind::PublicKeyword
                | SyntaxKind::ReadonlyKeyword => {
                    self.next_token();
                    if self.scanner().has_preceding_line_break() {
                        return false;
                    }
                    continue;
                }
                SyntaxKind::GlobalKeyword => {
                    self.next_token();
                    return matches!(
                        self.token(),
                        SyntaxKind::OpenBraceToken
                            | SyntaxKind::Identifier
                            | SyntaxKind::ExportKeyword
                    );
                }
                SyntaxKind::ImportKeyword => {
                    self.next_token();
                    return matches!(
                        self.token(),
                        SyntaxKind::StringLiteral
                            | SyntaxKind::AsteriskToken
                            | SyntaxKind::OpenBraceToken
                    ) || token_is_identifier_or_keyword(self.token());
                }
                SyntaxKind::ExportKeyword => {
                    let mut current_token = self.next_token();
                    if current_token == SyntaxKind::TypeKeyword {
                        current_token = self.look_ahead(|| Some(self.next_token())).unwrap();
                    }
                    if matches!(
                        current_token,
                        SyntaxKind::EqualsToken
                            | SyntaxKind::AsteriskToken
                            | SyntaxKind::OpenBraceToken
                            | SyntaxKind::DefaultKeyword
                            | SyntaxKind::AsKeyword
                    ) {
                        return true;
                    }
                    continue;
                }
                SyntaxKind::StaticKeyword => {
                    self.next_token();
                    continue;
                }
                _ => {
                    return false;
                }
            }
        }
    }

    pub(super) fn is_start_of_declaration(&self) -> bool {
        self.look_ahead_bool(|| self.is_declaration())
    }

    pub(super) fn is_start_of_statement(&self) -> bool {
        match self.token() {
            SyntaxKind::AtToken
            | SyntaxKind::SemicolonToken
            | SyntaxKind::OpenBraceToken
            | SyntaxKind::VarKeyword
            | SyntaxKind::LetKeyword
            | SyntaxKind::FunctionKeyword
            | SyntaxKind::ClassKeyword
            | SyntaxKind::EnumKeyword
            | SyntaxKind::IfKeyword
            | SyntaxKind::DoKeyword
            | SyntaxKind::WhileKeyword
            | SyntaxKind::ForKeyword
            | SyntaxKind::ContinueKeyword
            | SyntaxKind::BreakKeyword
            | SyntaxKind::ReturnKeyword
            | SyntaxKind::WithKeyword
            | SyntaxKind::SwitchKeyword
            | SyntaxKind::ThrowKeyword
            | SyntaxKind::TryKeyword
            | SyntaxKind::DebuggerKeyword
            | SyntaxKind::CatchKeyword
            | SyntaxKind::FinallyKeyword => true,

            SyntaxKind::ImportKeyword => {
                self.is_start_of_declaration()
                    || self.look_ahead_bool(|| self.next_token_is_open_paren_or_less_than_or_dot())
            }

            SyntaxKind::ConstKeyword | SyntaxKind::ExportKeyword => self.is_start_of_declaration(),

            SyntaxKind::AsyncKeyword
            | SyntaxKind::DeclareKeyword
            | SyntaxKind::InterfaceKeyword
            | SyntaxKind::ModuleKeyword
            | SyntaxKind::NamespaceKeyword
            | SyntaxKind::TypeKeyword
            | SyntaxKind::GlobalKeyword => true,
            SyntaxKind::PublicKeyword
            | SyntaxKind::PrivateKeyword
            | SyntaxKind::ProtectedKeyword
            | SyntaxKind::StaticKeyword
            | SyntaxKind::ReadonlyKeyword => {
                self.is_start_of_declaration()
                    || !self
                        .look_ahead_bool(|| self.next_token_is_identifier_or_keyword_on_same_line())
            }
            _ => self.is_start_of_expression(),
        }
    }

    pub(super) fn next_token_is_binding_identifier_or_start_of_destructuring(&self) -> bool {
        self.next_token();
        self.is_binding_identifier()
            || matches!(
                self.token(),
                SyntaxKind::OpenBraceToken | SyntaxKind::OpenBracketToken
            )
    }

    pub(super) fn is_let_declaration(&self) -> bool {
        self.look_ahead_bool(|| self.next_token_is_binding_identifier_or_start_of_destructuring())
    }

    pub(super) fn parse_statement(&self) -> Rc<Node> {
        match self.token() {
            SyntaxKind::SemicolonToken => return self.parse_empty_statement().wrap(),
            SyntaxKind::OpenBraceToken => return self.parse_block(false, None).into(),
            SyntaxKind::VarKeyword => {
                return self
                    .parse_variable_statement(
                        self.get_node_pos(),
                        self.has_preceding_jsdoc_comment(),
                        None,
                        None,
                    )
                    .wrap()
            }
            SyntaxKind::LetKeyword => {
                if self.is_let_declaration() {
                    return self
                        .parse_variable_statement(
                            self.get_node_pos(),
                            self.has_preceding_jsdoc_comment(),
                            None,
                            None,
                        )
                        .wrap();
                }
            }
            SyntaxKind::FunctionKeyword => {
                return self
                    .parse_function_declaration(
                        self.get_node_pos(),
                        self.has_preceding_jsdoc_comment(),
                        None,
                        None,
                    )
                    .into()
            }
            SyntaxKind::ClassKeyword => {
                return self
                    .parse_class_declaration(
                        self.get_node_pos(),
                        self.has_preceding_jsdoc_comment(),
                        None,
                        None,
                    )
                    .into()
            }
            SyntaxKind::IfKeyword => return self.parse_if_statement().into(),
            SyntaxKind::DoKeyword => return self.parse_do_statement().into(),
            SyntaxKind::WhileKeyword => return self.parse_while_statement().into(),
            SyntaxKind::ForKeyword => return self.parse_for_or_for_in_or_for_of_statement().wrap(),
            SyntaxKind::ContinueKeyword => {
                return self
                    .parse_break_or_continue_statement(SyntaxKind::ContinueStatement)
                    .wrap()
            }
            SyntaxKind::BreakKeyword => {
                return self
                    .parse_break_or_continue_statement(SyntaxKind::BreakStatement)
                    .wrap()
            }
            SyntaxKind::ReturnKeyword => return self.parse_return_statement().into(),
            SyntaxKind::WithKeyword => return self.parse_with_statement().into(),
            SyntaxKind::SwitchKeyword => return self.parse_switch_statement().into(),
            SyntaxKind::ThrowKeyword => return self.parse_throw_statement().into(),
            SyntaxKind::TryKeyword | SyntaxKind::CatchKeyword | SyntaxKind::FinallyKeyword => {
                return self.parse_try_statement().into()
            }
            SyntaxKind::DebuggerKeyword => return self.parse_debugger_statement().into(),
            SyntaxKind::AtToken => return self.parse_declaration(),
            SyntaxKind::AsyncKeyword
            | SyntaxKind::InterfaceKeyword
            | SyntaxKind::TypeKeyword
            | SyntaxKind::ModuleKeyword
            | SyntaxKind::NamespaceKeyword
            | SyntaxKind::DeclareKeyword
            | SyntaxKind::ConstKeyword
            | SyntaxKind::EnumKeyword
            | SyntaxKind::ExportKeyword
            | SyntaxKind::ImportKeyword
            | SyntaxKind::PrivateKeyword
            | SyntaxKind::ProtectedKeyword
            | SyntaxKind::PublicKeyword
            | SyntaxKind::AbstractKeyword
            | SyntaxKind::StaticKeyword
            | SyntaxKind::ReadonlyKeyword
            | SyntaxKind::GlobalKeyword => {
                if self.is_start_of_declaration() {
                    return self.parse_declaration();
                }
            }
            _ => (),
        }
        self.parse_expression_or_labeled_statement().wrap()
    }

    pub(super) fn is_declare_modifier(&self, modifier: &Node /*Modifier*/) -> bool {
        modifier.kind() == SyntaxKind::DeclareKeyword
    }

    pub(super) fn parse_declaration(&self) -> Rc<Node /*Statement*/> {
        let is_ambient = some(
            self.look_ahead(|| {
                self.parse_decorators();
                self.parse_modifiers(None, None)
            })
            .as_ref()
            .map(|node_array| {
                let node_array: &[Rc<Node>] = node_array;
                node_array
            }),
            Some(|modifier: &Rc<Node>| self.is_declare_modifier(modifier)),
        );
        if is_ambient {
            let node = self.try_reuse_ambient_declaration();
            if let Some(node) = node {
                return node;
            }
        }

        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        let decorators = self.parse_decorators();
        let modifiers = self.parse_modifiers(None, None);
        if is_ambient {
            for m in modifiers.as_ref().unwrap() {
                m.set_flags(m.flags() | NodeFlags::Ambient);
            }
            self.do_inside_of_context(NodeFlags::Ambient, move || {
                self.parse_declaration_worker(pos, has_jsdoc, decorators, modifiers)
                    .wrap()
            })
        } else {
            self.parse_declaration_worker(pos, has_jsdoc, decorators, modifiers)
                .wrap()
        }
    }

    pub(super) fn try_reuse_ambient_declaration(&self) -> Option<Rc<Node /*Statement*/>> {
        self.do_inside_of_context(NodeFlags::Ambient, || {
            let node = self.current_node(self.parsing_context());
            if let Some(node) = node {
                return Some(self.consume_node(node));
            }
            None
        })
    }

    pub(super) fn parse_declaration_worker(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> Node /*Statement*/ {
        match self.token() {
            SyntaxKind::VarKeyword | SyntaxKind::LetKeyword | SyntaxKind::ConstKeyword => {
                self.parse_variable_statement(pos, has_jsdoc, decorators, modifiers)
            }
            SyntaxKind::FunctionKeyword => self
                .parse_function_declaration(pos, has_jsdoc, decorators, modifiers)
                .into(),
            SyntaxKind::ClassKeyword => self
                .parse_class_declaration(pos, has_jsdoc, decorators, modifiers)
                .into(),
            SyntaxKind::InterfaceKeyword => self
                .parse_interface_declaration(pos, has_jsdoc, decorators, modifiers)
                .into(),
            SyntaxKind::TypeKeyword => self
                .parse_type_alias_declaration(pos, has_jsdoc, decorators, modifiers)
                .into(),
            SyntaxKind::EnumKeyword => self
                .parse_enum_declaration(pos, has_jsdoc, decorators, modifiers)
                .into(),
            SyntaxKind::GlobalKeyword
            | SyntaxKind::ModuleKeyword
            | SyntaxKind::NamespaceKeyword => self
                .parse_module_declaration(pos, has_jsdoc, decorators, modifiers)
                .into(),
            SyntaxKind::ImportKeyword => self
                .parse_import_declaration_or_import_equals_declaration(
                    pos, has_jsdoc, decorators, modifiers,
                ),
            SyntaxKind::ExportKeyword => {
                self.next_token();
                match self.token() {
                    SyntaxKind::DefaultKeyword | SyntaxKind::EqualsToken => self
                        .parse_export_assignment(pos, has_jsdoc, decorators, modifiers)
                        .into(),
                    SyntaxKind::AsKeyword => self
                        .parse_namespace_export_declaration(pos, has_jsdoc, decorators, modifiers)
                        .into(),
                    _ => self
                        .parse_export_declaration(pos, has_jsdoc, decorators, modifiers)
                        .into(),
                }
            }
            _ => {
                if decorators.is_some() || modifiers.is_some() {
                    let missing = self.create_missing_node(
                        SyntaxKind::MissingDeclaration,
                        true,
                        Some(&Diagnostics::Declaration_expected),
                        None,
                    );
                    set_text_range_pos(&missing, pos);
                    missing.set_decorators(decorators);
                    missing.set_modifiers(modifiers);
                    return missing;
                }
                // return undefined!;
                panic!("Need to make this an Option?")
            }
        }
    }

    pub(super) fn next_token_is_identifier_or_string_literal_on_same_line(&self) -> bool {
        self.next_token();
        !self.scanner().has_preceding_line_break()
            && (self.is_identifier() || self.token() == SyntaxKind::StringLiteral)
    }

    pub(super) fn parse_function_block_or_semicolon(
        &self,
        flags: SignatureFlags,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Option<Block> {
        if self.token() != SyntaxKind::OpenBraceToken && self.can_parse_semicolon() {
            self.parse_semicolon();
            return None;
        }

        Some(self.parse_function_block(flags, diagnostic_message))
    }

    pub(super) fn parse_array_binding_element(&self) -> Node /*ArrayBindingElement*/ {
        let pos = self.get_node_pos();
        if self.token() == SyntaxKind::CommaToken {
            return self.finish_node(
                self.factory.create_omitted_expression(self).into(),
                pos,
                None,
            );
        }
        let dot_dot_dot_token = self
            .parse_optional_token(SyntaxKind::DotDotDotToken)
            .map(Node::wrap);
        let name = self.parse_identifier_or_pattern(None);
        let initializer = self.parse_initializer();
        self.finish_node(
            self.factory
                .create_binding_element(
                    self,
                    dot_dot_dot_token,
                    Option::<Rc<Node>>::None,
                    name,
                    initializer,
                )
                .into(),
            pos,
            None,
        )
    }

    pub(super) fn parse_object_binding_element(&self) -> BindingElement {
        let pos = self.get_node_pos();
        let dot_dot_dot_token = self
            .parse_optional_token(SyntaxKind::DotDotDotToken)
            .map(Node::wrap);
        let token_is_identifier = self.is_binding_identifier();
        let mut property_name: Option<Rc<Node>> = Some(self.parse_property_name().wrap());
        let name: Rc<Node>;
        if token_is_identifier && self.token() != SyntaxKind::ColonToken {
            name = property_name.clone().unwrap();
            property_name = None;
        } else {
            self.parse_expected(SyntaxKind::ColonToken, None, None);
            name = self.parse_identifier_or_pattern(None);
        }
        let initializer = self.parse_initializer();
        self.finish_node(
            self.factory.create_binding_element(
                self,
                dot_dot_dot_token,
                property_name,
                name,
                initializer,
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_object_binding_pattern(&self) -> ObjectBindingPattern {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBraceToken, None, None);
        let elements = self.parse_delimited_list(
            ParsingContext::ObjectBindingElements,
            || self.parse_object_binding_element().into(),
            None,
        );
        self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        self.finish_node(
            self.factory.create_object_binding_pattern(self, elements),
            pos,
            None,
        )
    }

    pub(super) fn parse_array_binding_pattern(&self) -> ArrayBindingPattern {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBracketToken, None, None);
        let elements = self.parse_delimited_list(
            ParsingContext::ArrayBindingElements,
            || self.parse_array_binding_element().into(),
            None,
        );
        self.parse_expected(SyntaxKind::CloseBracketToken, None, None);
        self.finish_node(
            self.factory.create_array_binding_pattern(self, elements),
            pos,
            None,
        )
    }

    pub(super) fn is_binding_identifier_or_private_identifier_or_pattern(&self) -> bool {
        matches!(
            self.token(),
            SyntaxKind::OpenBraceToken
                | SyntaxKind::OpenBracketToken
                | SyntaxKind::PrivateIdentifier
        ) || self.is_binding_identifier()
    }

    pub(super) fn parse_identifier_or_pattern(
        &self,
        private_identifier_diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Rc<Node /*Identifier | BindingPattern*/> {
        if self.token() == SyntaxKind::OpenBracketToken {
            return self.parse_array_binding_pattern().into();
        }
        if self.token() == SyntaxKind::OpenBraceToken {
            return self.parse_object_binding_pattern().into();
        }
        self.parse_binding_identifier(private_identifier_diagnostic_message)
            .wrap()
    }

    pub(super) fn parse_variable_declaration_allow_exclamation(&self) -> VariableDeclaration {
        self.parse_variable_declaration(Some(true))
    }

    pub(super) fn parse_variable_declaration(
        &self,
        allow_exclamation: Option<bool>,
    ) -> VariableDeclaration {
        let allow_exclamation = allow_exclamation.unwrap_or(false);
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        let name = self.parse_identifier_or_pattern(Some(
            &Diagnostics::Private_identifiers_are_not_allowed_in_variable_declarations,
        ));
        let mut exclamation_token: Option<Rc<Node>> = None;
        if allow_exclamation
            && name.kind() == SyntaxKind::Identifier
            && self.token() == SyntaxKind::ExclamationToken
            && !self.scanner().has_preceding_line_break()
        {
            exclamation_token = Some(self.parse_token_node().into());
        }
        let type_ = self.parse_type_annotation();
        let initializer = if self.is_in_or_of_keyword(self.token()) {
            None
        } else {
            self.parse_initializer()
        };
        let node = self.factory.create_variable_declaration(
            self,
            Some(name),
            exclamation_token,
            type_.map(Node::wrap),
            initializer,
        );
        self.with_jsdoc(self.finish_node(node, pos, None), has_jsdoc)
    }

    pub(super) fn parse_variable_declaration_list(
        &self,
        in_for_statement_initializer: bool,
    ) -> VariableDeclarationList {
        let pos = self.get_node_pos();

        let mut flags = NodeFlags::None;
        match self.token() {
            SyntaxKind::VarKeyword => (),
            SyntaxKind::ConstKeyword => {
                flags |= NodeFlags::Const;
            }
            _ => Debug_.fail(None),
        }

        self.next_token();

        let declarations: NodeArray;
        if false {
            unimplemented!()
        } else {
            declarations = self.parse_delimited_list(
                ParsingContext::VariableDeclarations,
                || self.parse_variable_declaration_allow_exclamation().into(),
                None,
            );
        }

        self.finish_node(
            self.factory
                .create_variable_declaration_list(self, declarations, Some(flags)),
            pos,
            None,
        )
    }

    pub(super) fn parse_variable_statement(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> Node {
        let declaration_list = self.parse_variable_declaration_list(false);
        self.parse_semicolon();
        let node = self.factory.create_variable_statement(
            self,
            modifiers,
            Into::<Rc<Node>>::into(declaration_list),
        );
        node.set_decorators(decorators);
        self.finish_node(node.into(), pos, None)
    }

    pub(super) fn parse_function_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> FunctionDeclaration {
        let saved_await_context = self.in_await_context();

        let modifier_flags = modifiers_to_flags(modifiers.as_ref());
        self.parse_expected(SyntaxKind::FunctionKeyword, None, None);
        let asterisk_token = self.parse_optional_token(SyntaxKind::AsteriskToken);
        let name = if modifier_flags.intersects(ModifierFlags::Default) {
            self.parse_optional_binding_identifier()
        } else {
            Some(self.parse_binding_identifier(None))
        };
        let is_generator = if asterisk_token.is_some() {
            SignatureFlags::Yield
        } else {
            SignatureFlags::None
        };
        let is_async = if modifier_flags.intersects(ModifierFlags::Async) {
            SignatureFlags::Await
        } else {
            SignatureFlags::None
        };
        let type_parameters = self.parse_type_parameters();
        if modifier_flags.intersects(ModifierFlags::Export) {
            self.set_await_context(true);
        }
        let parameters = self.parse_parameters(is_generator | is_async);
        let type_ = self.parse_return_type(SyntaxKind::ColonToken, false);
        let body = self.parse_function_block_or_semicolon(
            is_generator | is_async,
            Some(&Diagnostics::or_expected),
        );
        self.set_await_context(saved_await_context);
        let node = self.factory.create_function_declaration(
            self,
            decorators,
            modifiers,
            asterisk_token.map(Into::into),
            name.map(|name| name.wrap()),
            type_parameters,
            parameters,
            type_.map(|type_| type_.wrap()),
            body.map(Into::into),
        );
        self.finish_node(node, pos, None)
    }

    pub(super) fn parse_method_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        asterisk_token: Option<Rc<Node /*AsteriskToken*/>>,
        name: Rc<Node /*PropertyName*/>,
        question_token: Option<Rc<Node /*QuestionToken*/>>,
        exclamation_token: Option<Rc<Node /*ExclamationToken*/>>,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> MethodDeclaration {
        unimplemented!()
    }

    pub(super) fn parse_accessor_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        kind: SyntaxKind, /*AccessorDeclaration["kind"]*/
    ) -> Node /*AccessorDeclaration*/ {
        unimplemented!()
    }

    pub(super) fn is_class_member_start(&self) -> bool {
        let mut id_token: Option<SyntaxKind> = None;

        if self.token() == SyntaxKind::AtToken {
            return true;
        }

        while is_modifier_kind(self.token()) {
            id_token = Some(self.token());
            if is_class_member_modifier(id_token.unwrap()) {
                return true;
            }

            self.next_token();
        }

        if self.token() == SyntaxKind::AsteriskToken {
            return true;
        }

        if self.is_literal_property_name() {
            id_token = Some(self.token());
            self.next_token();
        }

        if self.token() == SyntaxKind::OpenBracketToken {
            return true;
        }

        if let Some(id_token) = id_token {
            if !is_keyword(id_token)
                || matches!(id_token, SyntaxKind::SetKeyword | SyntaxKind::GetKeyword)
            {
                return true;
            }

            match self.token() {
                SyntaxKind::OpenParenToken
                | SyntaxKind::LessThanToken
                | SyntaxKind::ExclamationToken
                | SyntaxKind::ColonToken
                | SyntaxKind::EqualsToken
                | SyntaxKind::QuestionToken => {
                    return true;
                }
                _ => {
                    return self.can_parse_semicolon();
                }
            }
        }

        false
    }

    pub(super) fn try_parse_decorator(&self) -> Option<Decorator> {
        let pos = self.get_node_pos();
        if !self.parse_optional(SyntaxKind::AtToken) {
            return None;
        }
        unimplemented!()
    }

    pub(super) fn parse_decorators(&self) -> Option<NodeArray /*<Decorator>*/> {
        let pos = self.get_node_pos();
        let mut list: Option<Vec<Rc<Node>>> = None;
        loop {
            let decorator = self.try_parse_decorator();
            if decorator.is_none() {
                break;
            }
            let decorator = decorator.unwrap();
            if list.is_none() {
                list = Some(vec![]);
            }
            let list = list.as_mut().unwrap();
            append(list, Some(decorator.into()));
        }
        list.map(|list| self.create_node_array(list, pos, None, None))
    }

    pub(super) fn try_parse_modifier(
        &self,
        permit_invalid_const_as_modifier: Option<bool>,
        stop_on_start_of_class_static_block: Option<bool>,
        has_seen_static_modifier: Option<bool>,
    ) -> Option<Node /*Modifier*/> {
        let permit_invalid_const_as_modifier = permit_invalid_const_as_modifier.unwrap_or(false);
        let stop_on_start_of_class_static_block =
            stop_on_start_of_class_static_block.unwrap_or(false);
        let has_seen_static_modifier = has_seen_static_modifier.unwrap_or(false);
        let pos = self.get_node_pos();
        let kind = self.token();

        if self.token() == SyntaxKind::ConstKeyword && permit_invalid_const_as_modifier {
            unimplemented!()
        } else if stop_on_start_of_class_static_block
            && self.token() == SyntaxKind::StaticKeyword
            && self.look_ahead_bool(|| self.next_token_is_open_brace())
        {
            return None;
        } else if has_seen_static_modifier && self.token() == SyntaxKind::StaticKeyword {
            return None;
        } else {
            if !self.parse_any_contextual_modifier() {
                return None;
            }
        }

        Some(
            self.finish_node(self.factory.create_token(self, kind), pos, None)
                .into(),
        )
    }

    pub(super) fn parse_modifiers(
        &self,
        permit_invalid_const_as_modifier: Option<bool>,
        stop_on_start_of_class_static_block: Option<bool>,
    ) -> Option<NodeArray /*<Modifier>*/> {
        let pos = self.get_node_pos();
        let mut list: Option<Vec<Rc<Node>>> = None;
        let mut has_seen_static = false;
        loop {
            let modifier = self.try_parse_modifier(
                permit_invalid_const_as_modifier,
                stop_on_start_of_class_static_block,
                Some(has_seen_static),
            );
            if modifier.is_none() {
                break;
            }
            let modifier = modifier.unwrap();
            if modifier.kind() == SyntaxKind::StaticKeyword {
                has_seen_static = true;
            }
            if list.is_none() {
                list = Some(vec![]);
            }
            let list = list.as_mut().unwrap();
            append(list, Some(modifier.wrap()));
        }
        list.map(|list| self.create_node_array(list, pos, None, None))
    }

    pub(super) fn parse_modifiers_for_arrow_function(&self) -> Option<NodeArray /*<Modifier>*/> {
        unimplemented!()
    }

    pub(super) fn parse_class_expression(&self) -> ClassExpression {
        unimplemented!()
    }

    pub(super) fn parse_class_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> ClassDeclaration {
        unimplemented!()
    }
}
