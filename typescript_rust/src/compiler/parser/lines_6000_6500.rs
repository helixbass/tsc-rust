use gc::Gc;
use id_arena::Id;

use super::{ParserType, ParsingContext, SignatureFlags};
use crate::{
    is_identifier, set_text_range_pos, some, token_is_identifier_or_keyword, BindingElement,
    CaseBlock, CatchClause, DefaultClause, DiagnosticMessage, Diagnostics, Node, NodeArray,
    NodeFlags, NodeInterface, SyntaxKind,
    InArena,
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
            self.factory().create_default_clause_raw(statements),
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
            self.parse_case_or_default_clause().alloc(self.arena())
        });
        self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        self.finish_node(self.factory().create_case_block_raw(clauses), pos, None)
    }

    pub(super) fn parse_switch_statement(&self) -> Id<Node /*SwitchStatement*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        self.parse_expected(SyntaxKind::SwitchKeyword, None, None);
        self.parse_expected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.allow_in_and(|| self.parse_expression());
        self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        let case_block = self.parse_case_block();
        self.with_jsdoc(
            self.finish_node(
                self.factory()
                    .create_switch_statement_raw(expression, case_block.alloc(self.arena())),
                pos,
                None,
            )
            .alloc(self.arena()),
            has_jsdoc,
        )
    }

    pub(super) fn parse_throw_statement(&self) -> Id<Node /*ThrowStatement*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        self.parse_expected(SyntaxKind::ThrowKeyword, None, None);

        let mut expression: Option<Id<Node>> = if self.scanner().has_preceding_line_break() {
            None
        } else {
            Some(self.allow_in_and(|| self.parse_expression()))
        };
        if expression.is_none() {
            self.increment_identifier_count();
            expression = Some(
                self.finish_node(
                    self.factory()
                        .create_identifier_raw("", Option::<Gc<NodeArray>>::None, None),
                    self.get_node_pos(),
                    None,
                )
                .alloc(self.arena()),
            );
        }
        let expression = expression.unwrap();
        if !self.try_parse_semicolon() {
            self.parse_error_for_missing_semicolon_after(expression);
        }
        self.with_jsdoc(
            self.finish_node(
                self.factory().create_throw_statement_raw(expression),
                pos,
                None,
            )
            .alloc(self.arena()),
            has_jsdoc,
        )
    }

    pub(super) fn parse_try_statement(&self) -> Id<Node /*TryStatement*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();

        self.parse_expected(SyntaxKind::TryKeyword, None, None);
        let try_block: Id<Node> = self.parse_block(false, None);
        let catch_clause: Option<Id<Node>> = if self.token() == SyntaxKind::CatchKeyword {
            Some(self.parse_catch_clause().alloc(self.arena()))
        } else {
            None
        };

        let mut finally_block: Option<Id<Node>> = None;
        if catch_clause.is_none() || self.token() == SyntaxKind::FinallyKeyword {
            self.parse_expected(SyntaxKind::FinallyKeyword, None, None);
            finally_block = Some(self.parse_block(false, None));
        }

        self.with_jsdoc(
            self.finish_node(
                self.factory()
                    .create_try_statement_raw(try_block, catch_clause, finally_block),
                pos,
                None,
            )
            .alloc(self.arena()),
            has_jsdoc,
        )
    }

    pub(super) fn parse_catch_clause(&self) -> CatchClause {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::CatchKeyword, None, None);

        let variable_declaration: Option<Id<Node>>;
        if self.parse_optional(SyntaxKind::OpenParenToken) {
            variable_declaration = Some(self.parse_variable_declaration(None));
            self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        } else {
            variable_declaration = None;
        }

        let block = self.parse_block(false, None);
        self.finish_node(
            self.factory()
                .create_catch_clause_raw(variable_declaration, block),
            pos,
            None,
        )
    }

    pub(super) fn parse_debugger_statement(&self) -> Id<Node /*DebuggerStatement*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        self.parse_expected(SyntaxKind::DebuggerKeyword, None, None);
        self.parse_semicolon();
        self.with_jsdoc(
            self.finish_node(self.factory().create_debugger_statement_raw(), pos, None)
                .alloc(self.arena()),
            has_jsdoc,
        )
    }

    pub(super) fn parse_expression_or_labeled_statement(
        &self,
    ) -> Id<Node /*ExpressionStatement | LabeledStatement*/> {
        let pos = self.get_node_pos();
        let mut has_jsdoc = self.has_preceding_jsdoc_comment();
        let node: Node;
        let has_paren = self.token() == SyntaxKind::OpenParenToken;
        let expression = self.allow_in_and(|| self.parse_expression());
        if is_identifier(&expression.ref_(self)) && self.parse_optional(SyntaxKind::ColonToken) {
            node = self
                .factory()
                .create_labeled_statement_raw(expression, self.parse_statement())
                .into();
        } else {
            if !self.try_parse_semicolon() {
                self.parse_error_for_missing_semicolon_after(expression);
            }
            node = self
                .factory()
                .create_expression_statement_raw(expression)
                .into();
            if has_paren {
                has_jsdoc = false;
            }
        };
        self.with_jsdoc(self.finish_node(node, pos, None).alloc(self.arena()), has_jsdoc)
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

    pub(super) fn parse_statement(&self) -> Id<Node> {
        match self.token() {
            SyntaxKind::SemicolonToken => return self.parse_empty_statement(),
            SyntaxKind::OpenBraceToken => return self.parse_block(false, None),
            SyntaxKind::VarKeyword => {
                return self.parse_variable_statement(
                    self.get_node_pos(),
                    self.has_preceding_jsdoc_comment(),
                    None,
                    None,
                );
            }
            SyntaxKind::LetKeyword => {
                if self.is_let_declaration() {
                    return self.parse_variable_statement(
                        self.get_node_pos(),
                        self.has_preceding_jsdoc_comment(),
                        None,
                        None,
                    );
                }
            }
            SyntaxKind::FunctionKeyword => {
                return self.parse_function_declaration(
                    self.get_node_pos(),
                    self.has_preceding_jsdoc_comment(),
                    None,
                    None,
                );
            }
            SyntaxKind::ClassKeyword => {
                return self.parse_class_declaration(
                    self.get_node_pos(),
                    self.has_preceding_jsdoc_comment(),
                    None,
                    None,
                );
            }
            SyntaxKind::IfKeyword => return self.parse_if_statement(),
            SyntaxKind::DoKeyword => return self.parse_do_statement(),
            SyntaxKind::WhileKeyword => return self.parse_while_statement(),
            SyntaxKind::ForKeyword => return self.parse_for_or_for_in_or_for_of_statement(),
            SyntaxKind::ContinueKeyword => {
                return self.parse_break_or_continue_statement(SyntaxKind::ContinueStatement)
            }
            SyntaxKind::BreakKeyword => {
                return self.parse_break_or_continue_statement(SyntaxKind::BreakStatement)
            }
            SyntaxKind::ReturnKeyword => return self.parse_return_statement(),
            SyntaxKind::WithKeyword => return self.parse_with_statement(),
            SyntaxKind::SwitchKeyword => return self.parse_switch_statement(),
            SyntaxKind::ThrowKeyword => return self.parse_throw_statement(),
            SyntaxKind::TryKeyword | SyntaxKind::CatchKeyword | SyntaxKind::FinallyKeyword => {
                return self.parse_try_statement();
            }
            SyntaxKind::DebuggerKeyword => return self.parse_debugger_statement(),
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
        self.parse_expression_or_labeled_statement()
    }

    pub(super) fn is_declare_modifier(&self, modifier: Id<Node> /*Modifier*/) -> bool {
        modifier.ref_(self).kind() == SyntaxKind::DeclareKeyword
    }

    pub(super) fn parse_declaration(&self) -> Id<Node /*Statement*/> {
        let is_ambient = some(
            self.look_ahead(|| {
                self.parse_decorators();
                self.parse_modifiers(None, None)
            })
            .as_ref()
            .map(|node_array| {
                let node_array: &[Id<Node>] = node_array;
                node_array
            }),
            Some(|&modifier: &Id<Node>| self.is_declare_modifier(modifier)),
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
                m.ref_(self).set_flags(m.ref_(self).flags() | NodeFlags::Ambient);
            }
            self.do_inside_of_context(NodeFlags::Ambient, move || {
                self.parse_declaration_worker(pos, has_jsdoc, decorators, modifiers)
            })
        } else {
            self.parse_declaration_worker(pos, has_jsdoc, decorators, modifiers)
        }
    }

    pub(super) fn try_reuse_ambient_declaration(&self) -> Option<Id<Node /*Statement*/>> {
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
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Id<Node> /*Statement*/ {
        match self.token() {
            SyntaxKind::VarKeyword | SyntaxKind::LetKeyword | SyntaxKind::ConstKeyword => {
                self.parse_variable_statement(pos, has_jsdoc, decorators, modifiers)
            }
            SyntaxKind::FunctionKeyword => {
                self.parse_function_declaration(pos, has_jsdoc, decorators, modifiers)
            }
            SyntaxKind::ClassKeyword => {
                self.parse_class_declaration(pos, has_jsdoc, decorators, modifiers)
            }
            SyntaxKind::InterfaceKeyword => {
                self.parse_interface_declaration(pos, has_jsdoc, decorators, modifiers)
            }
            SyntaxKind::TypeKeyword => {
                self.parse_type_alias_declaration(pos, has_jsdoc, decorators, modifiers)
            }
            SyntaxKind::EnumKeyword => {
                self.parse_enum_declaration(pos, has_jsdoc, decorators, modifiers)
            }
            SyntaxKind::GlobalKeyword
            | SyntaxKind::ModuleKeyword
            | SyntaxKind::NamespaceKeyword => {
                self.parse_module_declaration(pos, has_jsdoc, decorators, modifiers)
            }
            SyntaxKind::ImportKeyword => self
                .parse_import_declaration_or_import_equals_declaration(
                    pos, has_jsdoc, decorators, modifiers,
                ),
            SyntaxKind::ExportKeyword => {
                self.next_token();
                match self.token() {
                    SyntaxKind::DefaultKeyword | SyntaxKind::EqualsToken => {
                        self.parse_export_assignment(pos, has_jsdoc, decorators, modifiers)
                    }
                    SyntaxKind::AsKeyword => self
                        .parse_namespace_export_declaration(pos, has_jsdoc, decorators, modifiers),
                    _ => self.parse_export_declaration(pos, has_jsdoc, decorators, modifiers),
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
                    return missing.alloc(self.arena());
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
    ) -> Option<Id<Node /*Block*/>> {
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
                self.factory().create_omitted_expression_raw().into(),
                pos,
                None,
            );
        }
        let dot_dot_dot_token = self
            .parse_optional_token(SyntaxKind::DotDotDotToken)
            .map(|node| node.alloc(self.arena()));
        let name = self.parse_identifier_or_pattern(None);
        let initializer = self.parse_initializer();
        self.finish_node(
            self.factory()
                .create_binding_element_raw(
                    dot_dot_dot_token,
                    Option::<Id<Node>>::None,
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
            .map(|node| node.alloc(self.arena()));
        let token_is_identifier = self.is_binding_identifier();
        let mut property_name: Option<Id<Node>> = Some(self.parse_property_name().alloc(self.arena()));
        let name: Id<Node>;
        if token_is_identifier && self.token() != SyntaxKind::ColonToken {
            name = property_name.clone().unwrap();
            property_name = None;
        } else {
            self.parse_expected(SyntaxKind::ColonToken, None, None);
            name = self.parse_identifier_or_pattern(None);
        }
        let initializer = self.parse_initializer();
        self.finish_node(
            self.factory().create_binding_element_raw(
                dot_dot_dot_token,
                property_name,
                name,
                initializer,
            ),
            pos,
            None,
        )
    }
}
