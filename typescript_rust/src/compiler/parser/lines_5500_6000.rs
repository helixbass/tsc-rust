#![allow(non_upper_case_globals)]

use std::convert::TryInto;
use std::rc::Rc;

use super::{ParserType, ParsingContext, SignatureFlags};
use crate::{
    add_related_info, create_detached_diagnostic, is_async_modifier, last_or_undefined, some,
    ArrayLiteralExpression, Block, CaseClause, Debug_, DiagnosticMessage,
    DiagnosticRelatedInformationInterface, Diagnostics, DoStatement, FunctionExpression,
    IfStatement, Node, NodeArray, NodeFlags, NodeInterface, ObjectLiteralExpression,
    ParenthesizedExpression, ReturnStatement, SyntaxKind, WhileStatement, WithStatement,
};

impl ParserType {
    pub(super) fn parse_argument_list(&self) -> NodeArray /*<Expression>*/ {
        self.parse_expected(SyntaxKind::OpenParenToken, None, None);
        let result = self.parse_delimited_list(
            ParsingContext::ArgumentExpressions,
            || self.parse_argument_expression(),
            None,
        );
        self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        result
    }

    pub(super) fn parse_type_arguments_in_expression(&self) -> Option<NodeArray /*<TypeNode>*/> {
        if self.context_flags().intersects(NodeFlags::JavaScriptFile) {
            return None;
        }

        if self.re_scan_less_than_token() != SyntaxKind::LessThanToken {
            return None;
        }
        self.next_token();

        let type_arguments =
            self.parse_delimited_list(ParsingContext::TypeArguments, || self.parse_type(), None);
        if !self.parse_expected(SyntaxKind::GreaterThanToken, None, None) {
            return None;
        }

        if
        /*typeArguments &&*/
        self.can_follow_type_arguments_in_expression() {
            Some(type_arguments)
        } else {
            None
        }
    }

    pub(super) fn can_follow_type_arguments_in_expression(&self) -> bool {
        match self.token() {
            SyntaxKind::OpenParenToken
            | SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::TemplateHead
            | SyntaxKind::DotToken
            | SyntaxKind::CloseParenToken
            | SyntaxKind::CloseBracketToken
            | SyntaxKind::ColonToken
            | SyntaxKind::SemicolonToken
            | SyntaxKind::QuestionToken
            | SyntaxKind::EqualsEqualsToken
            | SyntaxKind::EqualsEqualsEqualsToken
            | SyntaxKind::ExclamationEqualsToken
            | SyntaxKind::ExclamationEqualsEqualsToken
            | SyntaxKind::AmpersandAmpersandToken
            | SyntaxKind::BarBarToken
            | SyntaxKind::QuestionQuestionToken
            | SyntaxKind::CaretToken
            | SyntaxKind::AmpersandToken
            | SyntaxKind::BarToken
            | SyntaxKind::CloseBraceToken
            | SyntaxKind::EndOfFileToken => true,
            SyntaxKind::CommaToken | SyntaxKind::OpenBraceToken => false,
            _ => false,
        }
    }

    pub(super) fn parse_primary_expression(&self) -> Rc<Node /*PrimaryExpression*/> {
        match self.token() {
            SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::StringLiteral
            | SyntaxKind::NoSubstitutionTemplateLiteral => return self.parse_literal_node().wrap(),
            SyntaxKind::ThisKeyword
            | SyntaxKind::SuperKeyword
            | SyntaxKind::NullKeyword
            | SyntaxKind::TrueKeyword
            | SyntaxKind::FalseKeyword => return self.parse_token_node().into(),
            SyntaxKind::OpenParenToken => return self.parse_parenthesized_expression(),
            SyntaxKind::OpenBracketToken => return self.parse_array_literal_expression().into(),
            SyntaxKind::OpenBraceToken => return self.parse_object_literal_expression().into(),
            SyntaxKind::AsyncKeyword => {
                if self.look_ahead_bool(|| self.next_token_is_function_keyword_on_same_line()) {
                    return self.parse_function_expression();
                }
            }
            SyntaxKind::ClassKeyword => return self.parse_class_expression(),
            SyntaxKind::FunctionKeyword => return self.parse_function_expression(),
            SyntaxKind::NewKeyword => return self.parse_new_expression_or_new_dot_target().wrap(),
            SyntaxKind::SlashToken | SyntaxKind::SlashEqualsToken => {
                if self.re_scan_slash_token() == SyntaxKind::RegularExpressionLiteral {
                    return self.parse_literal_node().wrap();
                }
            }
            SyntaxKind::TemplateHead => return self.parse_template_expression(false).into(),
            SyntaxKind::PrivateIdentifier => return self.parse_private_identifier().wrap(),
            _ => (),
        }

        self.parse_identifier(Some(&Diagnostics::Expression_expected), None)
            .wrap()
    }

    pub(super) fn parse_parenthesized_expression(&self) -> Rc<Node /*ParenthesizedExpression*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        self.parse_expected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.allow_in_and(|| self.parse_expression());
        self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        self.with_jsdoc(
            self.finish_node(
                self.factory
                    .create_parenthesized_expression(self, expression),
                pos,
                None,
            )
            .into(),
            has_jsdoc,
        )
    }

    pub(super) fn parse_spread_element(&self) -> Node /*Expression*/ {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::DotDotDotToken, None, None);
        let expression = self.parse_assignment_expression_or_higher();
        self.finish_node(
            self.factory.create_spread_element(self, expression).into(),
            pos,
            None,
        )
    }

    pub(super) fn parse_argument_or_array_literal_element(&self) -> Rc<Node> {
        if self.token() == SyntaxKind::DotDotDotToken {
            self.parse_spread_element().wrap()
        } else if self.token() == SyntaxKind::CommaToken {
            self.finish_node(
                self.factory.create_omitted_expression(self),
                self.get_node_pos(),
                None,
            )
            .into()
        } else {
            self.parse_assignment_expression_or_higher()
        }
    }

    pub(super) fn parse_argument_expression(&self) -> Rc<Node /*Expression*/> {
        self.do_outside_of_context(self.disallow_in_and_decorator_context, || {
            self.parse_argument_or_array_literal_element()
        })
    }

    pub(super) fn parse_array_literal_expression(&self) -> ArrayLiteralExpression {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBracketToken, None, None);
        let multi_line = self.scanner().has_preceding_line_break();
        let elements = self.parse_delimited_list(
            ParsingContext::ArrayLiteralMembers,
            || self.parse_argument_or_array_literal_element(),
            None,
        );
        self.parse_expected(SyntaxKind::CloseBracketToken, None, None);
        self.finish_node(
            self.factory
                .create_array_literal_expression(self, Some(elements), Some(multi_line)),
            pos,
            None,
        )
    }

    pub(super) fn parse_object_literal_element(&self) -> Rc<Node /*ObjectLiteralElementLike*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();

        if self
            .parse_optional_token(SyntaxKind::DotDotDotToken)
            .is_some()
        {
            let expression = self.parse_assignment_expression_or_higher();
            return self.with_jsdoc(
                self.finish_node(
                    self.factory.create_spread_assignment(self, expression),
                    pos,
                    None,
                )
                .into(),
                has_jsdoc,
            );
        }

        let decorators = self.parse_decorators();
        let modifiers = self.parse_modifiers(None, None);

        if self.parse_contextual_modifier(SyntaxKind::GetKeyword) {
            return self.parse_accessor_declaration(
                pos,
                has_jsdoc,
                decorators,
                modifiers,
                SyntaxKind::GetAccessor,
            );
        }
        if self.parse_contextual_modifier(SyntaxKind::SetKeyword) {
            return self.parse_accessor_declaration(
                pos,
                has_jsdoc,
                decorators,
                modifiers,
                SyntaxKind::SetAccessor,
            );
        }

        let asterisk_token = self
            .parse_optional_token(SyntaxKind::AsteriskToken)
            .map(|asterisk_token| asterisk_token.wrap());
        let token_is_identifier = self.is_identifier();
        let name: Rc<Node> = self.parse_property_name().wrap();

        let question_token: Option<Rc<Node>> = self
            .parse_optional_token(SyntaxKind::QuestionToken)
            .map(|question_token| question_token.wrap());
        let exclamation_token: Option<Rc<Node>> = self
            .parse_optional_token(SyntaxKind::ExclamationToken)
            .map(|exclamation_token| exclamation_token.wrap());

        if asterisk_token.is_some()
            || matches!(
                self.token(),
                SyntaxKind::OpenParenToken | SyntaxKind::LessThanToken
            )
        {
            return self.parse_method_declaration(
                pos,
                has_jsdoc,
                decorators,
                modifiers,
                asterisk_token,
                name,
                question_token,
                exclamation_token,
                None,
            );
        }

        let node: Node;
        let is_shorthand_property_assignment =
            token_is_identifier && self.token() != SyntaxKind::ColonToken;
        if is_shorthand_property_assignment {
            let equals_token: Option<Rc<Node>> = self
                .parse_optional_token(SyntaxKind::EqualsToken)
                .map(|equals_token| equals_token.wrap());
            let object_assignment_initializer = if equals_token.is_some() {
                Some(self.allow_in_and(|| self.parse_assignment_expression_or_higher()))
            } else {
                None
            };
            let mut node_as_shorthand_property_assignment = self
                .factory
                .create_shorthand_property_assignment(self, name, object_assignment_initializer);
            node_as_shorthand_property_assignment.equals_token = equals_token;
            node_as_shorthand_property_assignment.question_token = question_token;
            node_as_shorthand_property_assignment.exclamation_token = exclamation_token;
            node = node_as_shorthand_property_assignment.into();
        } else {
            self.parse_expected(SyntaxKind::ColonToken, None, None);
            let initializer = self.allow_in_and(|| self.parse_assignment_expression_or_higher());
            let mut node_as_property_assignment =
                self.factory
                    .create_property_assignment(self, name, initializer);
            node_as_property_assignment.question_token = question_token;
            node_as_property_assignment.exclamation_token = exclamation_token;
            node = node_as_property_assignment.into();
        }
        node.set_decorators(decorators);
        node.set_modifiers(modifiers);
        self.with_jsdoc(self.finish_node(node, pos, None).wrap(), has_jsdoc)
    }

    pub(super) fn parse_object_literal_expression(&self) -> ObjectLiteralExpression {
        let pos = self.get_node_pos();
        let open_brace_position = self.scanner().get_token_pos();
        self.parse_expected(SyntaxKind::OpenBraceToken, None, None);
        let multi_line = self.scanner().has_preceding_line_break();
        let properties = self.parse_delimited_list(
            ParsingContext::ObjectLiteralMembers,
            || self.parse_object_literal_element(),
            Some(true),
        );
        if !self.parse_expected(SyntaxKind::CloseBraceToken, None, None) {
            let parse_diagnostics = self.parse_diagnostics();
            let last_error = last_or_undefined(&*parse_diagnostics);
            if let Some(last_error) = last_error {
                if last_error.code() == Diagnostics::_0_expected.code {
                    add_related_info(
                        last_error,
                        vec![Rc::new(
                            create_detached_diagnostic(
                                self.file_name(),
                                open_brace_position.try_into().unwrap(),
                                1,
                                &Diagnostics::The_parser_expected_to_find_a_to_match_the_token_here,
                                None,
                            )
                            .into(),
                        )],
                    );
                }
            }
        }
        self.finish_node(
            self.factory
                .create_object_literal_expression(self, Some(properties), Some(multi_line)),
            pos,
            None,
        )
    }

    pub(super) fn parse_function_expression(&self) -> Rc<Node /*FunctionExpression*/> {
        let saved_decorator_context = self.in_decorator_context();
        self.set_decorator_context(false);

        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        let modifiers = self.parse_modifiers(None, None);
        self.parse_expected(SyntaxKind::FunctionKeyword, None, None);
        let asterisk_token: Option<Rc<Node>> = self
            .parse_optional_token(SyntaxKind::AsteriskToken)
            .map(Node::wrap);
        let is_generator = if asterisk_token.is_some() {
            SignatureFlags::Yield
        } else {
            SignatureFlags::None
        };
        let is_async = if some(
            modifiers.as_deref(),
            Some(|modifier: &Rc<Node>| is_async_modifier(modifier)),
        ) {
            SignatureFlags::Await
        } else {
            SignatureFlags::None
        };
        let name: Option<Rc<Node>> =
            if is_generator != SignatureFlags::None && is_async != SignatureFlags::None {
                self.do_in_yield_and_await_context(|| {
                    self.parse_optional_binding_identifier()
                        .map(|node| node.wrap())
                })
            } else if is_generator != SignatureFlags::None {
                self.do_in_yield_context(|| {
                    self.parse_optional_binding_identifier()
                        .map(|node| node.wrap())
                })
            } else if is_async != SignatureFlags::None {
                self.do_in_await_context(|| {
                    self.parse_optional_binding_identifier()
                        .map(|node| node.wrap())
                })
            } else {
                self.parse_optional_binding_identifier()
                    .map(|node| node.wrap())
            };

        let type_parameters = self.parse_type_parameters();
        let parameters = self.parse_parameters(is_generator | is_async);
        let type_: Option<Rc<Node>> = self.parse_return_type(SyntaxKind::ColonToken, false);
        let body: Rc<Node> = self.parse_function_block(is_generator | is_async, None);

        self.set_decorator_context(saved_decorator_context);

        let node = self.factory.create_function_expression(
            self,
            modifiers,
            asterisk_token,
            name,
            type_parameters,
            parameters,
            type_,
            body,
        );
        self.with_jsdoc(self.finish_node(node, pos, None).into(), has_jsdoc)
    }

    pub(super) fn parse_optional_binding_identifier(&self) -> Option<Node /*Identifier*/> {
        if self.is_binding_identifier() {
            Some(self.parse_binding_identifier(None))
        } else {
            None
        }
    }

    pub(super) fn parse_new_expression_or_new_dot_target(&self) -> Node /*NewExpression | MetaProperty*/
    {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::NewKeyword, None, None);
        if self.parse_optional(SyntaxKind::DotToken) {
            let name: Rc<Node> = self.parse_identifier_name(None).wrap();
            return self.finish_node(
                self.factory
                    .create_meta_property(self, SyntaxKind::NewKeyword, name)
                    .into(),
                pos,
                None,
            );
        }

        let expression_pos = self.get_node_pos();
        let mut expression: Rc<Node /*MemberExpression*/> = self.parse_primary_expression();
        let mut type_arguments: Option<NodeArray>;
        loop {
            expression = self.parse_member_expression_rest(expression_pos, expression, false);
            type_arguments = self.try_parse(|| self.parse_type_arguments_in_expression());
            if self.is_template_start_of_tagged_template() {
                Debug_.assert(type_arguments.is_some(), Some("Expected a type argument list; all plain tagged template starts should be consumed in 'parseMemberExpressionRest'"));
                expression = self.parse_tagged_template_rest(
                    expression_pos,
                    expression,
                    None,
                    type_arguments,
                );
                type_arguments = None;
            }
            break;
        }

        let mut arguments_array: Option<NodeArray> = None;
        if self.token() == SyntaxKind::OpenParenToken {
            arguments_array = Some(self.parse_argument_list());
        } else if type_arguments.is_some() {
            self.parse_error_at(pos, self.scanner().get_start_pos().try_into().unwrap(), &Diagnostics::A_new_expression_with_type_arguments_must_always_be_followed_by_a_parenthesized_argument_list, None);
        }
        self.finish_node(
            self.factory
                .create_new_expression(self, expression, type_arguments, arguments_array)
                .into(),
            pos,
            None,
        )
    }

    pub(super) fn parse_block(
        &self,
        ignore_missing_open_brace: bool,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Rc<Node /*Block*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        let open_brace_position = self.scanner().get_token_pos();
        if self.parse_expected(SyntaxKind::OpenBraceToken, diagnostic_message, None)
            || ignore_missing_open_brace
        {
            let multi_line = self.scanner().has_preceding_line_break();
            let statements = self.parse_list(ParsingContext::BlockStatements, &mut || {
                self.parse_statement()
            });
            if !self.parse_expected(SyntaxKind::CloseBraceToken, None, None) {
                let parse_diagnostics = self.parse_diagnostics();
                let last_error = last_or_undefined(&*parse_diagnostics);
                if let Some(last_error) = last_error {
                    if last_error.code() == Diagnostics::_0_expected.code {
                        add_related_info(
                            last_error,
                            vec![Rc::new(
                                create_detached_diagnostic(
                                    self.file_name(),
                                    open_brace_position.try_into().unwrap(),
                                    1,
                                    &Diagnostics::The_parser_expected_to_find_a_to_match_the_token_here,
                                    None,
                                )
                                .into(),
                            )],
                        );
                    }
                }
            }
            let result = self.with_jsdoc(
                self.finish_node(
                    self.factory
                        .create_block(self, statements, Some(multi_line)),
                    pos,
                    None,
                )
                .into(),
                has_jsdoc,
            );
            if self.token() == SyntaxKind::EqualsToken {
                self.parse_error_at_current_token(&Diagnostics::Declaration_or_statement_expected_This_follows_a_block_of_statements_so_if_you_intended_to_write_a_destructuring_assignment_you_might_need_to_wrap_the_the_whole_assignment_in_parentheses, None);
                self.next_token();
            }

            result
        } else {
            let statements = self.create_missing_list();
            self.with_jsdoc(
                self.finish_node(self.factory.create_block(self, statements, None), pos, None)
                    .into(),
                has_jsdoc,
            )
        }
    }

    pub(super) fn parse_function_block(
        &self,
        flags: SignatureFlags,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Rc<Node /*Block*/> {
        let saved_yield_context = self.in_yield_context();
        self.set_yield_context(flags.intersects(SignatureFlags::Yield));

        let saved_await_context = self.in_await_context();
        self.set_await_context(flags.intersects(SignatureFlags::Await));

        let saved_top_level = self.top_level();
        self.set_top_level(false);

        let save_decorator_context = self.in_decorator_context();
        if save_decorator_context {
            self.set_decorator_context(false);
        }

        let block = self.parse_block(
            flags.intersects(SignatureFlags::IgnoreMissingOpenBrace),
            diagnostic_message,
        );

        if save_decorator_context {
            self.set_decorator_context(true);
        }

        self.set_top_level(saved_top_level);
        self.set_yield_context(saved_yield_context);
        self.set_await_context(saved_await_context);

        block
    }

    pub(super) fn parse_empty_statement(&self) -> Rc<Node /*Statement*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        self.parse_expected(SyntaxKind::SemicolonToken, None, None);
        self.with_jsdoc(
            self.finish_node(self.factory.create_empty_statement(self), pos, None)
                .into(),
            has_jsdoc,
        )
    }

    pub(super) fn parse_if_statement(&self) -> Rc<Node /*IfStatement*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        self.parse_expected(SyntaxKind::IfKeyword, None, None);
        self.parse_expected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.allow_in_and(|| self.parse_expression());
        self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        let then_statement = self.parse_statement();
        let else_statement = if self.parse_optional(SyntaxKind::ElseKeyword) {
            Some(self.parse_statement())
        } else {
            None
        };
        self.with_jsdoc(
            self.finish_node(
                self.factory
                    .create_if_statement(self, expression, then_statement, else_statement),
                pos,
                None,
            )
            .into(),
            has_jsdoc,
        )
    }

    pub(super) fn parse_do_statement(&self) -> Rc<Node /*DoStatement*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        self.parse_expected(SyntaxKind::DoKeyword, None, None);
        let statement = self.parse_statement();
        self.parse_expected(SyntaxKind::WhileKeyword, None, None);
        self.parse_expected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.allow_in_and(|| self.parse_expression());
        self.parse_expected(SyntaxKind::CloseParenToken, None, None);

        self.parse_optional(SyntaxKind::SemicolonToken);
        self.with_jsdoc(
            self.finish_node(
                self.factory
                    .create_do_statement(self, statement, expression),
                pos,
                None,
            )
            .into(),
            has_jsdoc,
        )
    }

    pub(super) fn parse_while_statement(&self) -> Rc<Node /*WhileStatement*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        self.parse_expected(SyntaxKind::WhileKeyword, None, None);
        self.parse_expected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.allow_in_and(|| self.parse_expression());
        self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        let statement = self.parse_statement();
        self.with_jsdoc(
            self.finish_node(
                self.factory
                    .create_while_statement(self, expression, statement),
                pos,
                None,
            )
            .into(),
            has_jsdoc,
        )
    }

    pub(super) fn parse_for_or_for_in_or_for_of_statement(&self) -> Rc<Node /*Statement*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        self.parse_expected(SyntaxKind::ForKeyword, None, None);
        let await_token = self.parse_optional_token(SyntaxKind::AwaitKeyword);
        self.parse_expected(SyntaxKind::OpenParenToken, None, None);

        let mut initializer: Option<Rc<Node /*VariableDeclarationList | Expression*/>> = None;
        if self.token() != SyntaxKind::SemicolonToken {
            if matches!(
                self.token(),
                SyntaxKind::VarKeyword | SyntaxKind::LetKeyword | SyntaxKind::ConstKeyword
            ) {
                initializer = Some(self.parse_variable_declaration_list(true).into());
            } else {
                initializer = Some(self.disallow_in_and(|| self.parse_expression()));
            }
        }

        let node: Node /*IterationStatement*/;
        if if await_token.is_some() {
            self.parse_expected(SyntaxKind::OfKeyword, None, None)
        } else {
            self.parse_optional(SyntaxKind::OfKeyword)
        } {
            let expression = self.allow_in_and(|| self.parse_assignment_expression_or_higher());
            self.parse_expected(SyntaxKind::CloseParenToken, None, None);
            node = self
                .factory
                .create_for_of_statement(
                    self,
                    await_token.map(Node::wrap),
                    initializer.unwrap(),
                    expression,
                    self.parse_statement(),
                )
                .into();
        } else if self.parse_optional(SyntaxKind::InKeyword) {
            let expression = self.allow_in_and(|| self.parse_expression());
            self.parse_expected(SyntaxKind::CloseParenToken, None, None);
            node = self
                .factory
                .create_for_in_statement(
                    self,
                    initializer.unwrap(),
                    expression,
                    self.parse_statement(),
                )
                .into();
        } else {
            self.parse_expected(SyntaxKind::SemicolonToken, None, None);
            let condition: Option<Rc<Node>> = if !matches!(
                self.token(),
                SyntaxKind::SemicolonToken | SyntaxKind::CloseParenToken
            ) {
                Some(self.allow_in_and(|| self.parse_expression()))
            } else {
                None
            };
            self.parse_expected(SyntaxKind::SemicolonToken, None, None);
            let incrementor: Option<Rc<Node>> = if self.token() != SyntaxKind::CloseParenToken {
                Some(self.allow_in_and(|| self.parse_expression()))
            } else {
                None
            };
            self.parse_expected(SyntaxKind::CloseParenToken, None, None);
            node = self
                .factory
                .create_for_statement(
                    self,
                    initializer,
                    condition,
                    incrementor,
                    self.parse_statement(),
                )
                .into();
        }

        self.with_jsdoc(self.finish_node(node, pos, None).wrap(), has_jsdoc)
    }

    pub(super) fn parse_break_or_continue_statement(
        &self,
        kind: SyntaxKind,
    ) -> Rc<Node /*BreakOrContinueStatement*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();

        self.parse_expected(
            if kind == SyntaxKind::BreakStatement {
                SyntaxKind::BreakKeyword
            } else {
                SyntaxKind::ContinueKeyword
            },
            None,
            None,
        );
        let label: Option<Rc<Node>> = if self.can_parse_semicolon() {
            None
        } else {
            Some(self.parse_identifier(None, None).wrap())
        };

        self.parse_semicolon();
        let node: Node = if kind == SyntaxKind::BreakStatement {
            self.factory.create_break_statement(self, label).into()
        } else {
            self.factory.create_continue_statement(self, label).into()
        };
        self.with_jsdoc(self.finish_node(node, pos, None).wrap(), has_jsdoc)
    }

    pub(super) fn parse_return_statement(&self) -> Rc<Node /*ReturnStatement*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        self.parse_expected(SyntaxKind::ReturnKeyword, None, None);
        let expression: Option<Rc<Node>> = if self.can_parse_semicolon() {
            None
        } else {
            Some(self.allow_in_and(|| self.parse_expression()))
        };
        self.parse_semicolon();
        self.with_jsdoc(
            self.finish_node(
                self.factory.create_return_statement(self, expression),
                pos,
                None,
            )
            .into(),
            has_jsdoc,
        )
    }

    pub(super) fn parse_with_statement(&self) -> Rc<Node /*WithStatement*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        self.parse_expected(SyntaxKind::WithKeyword, None, None);
        self.parse_expected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.allow_in_and(|| self.parse_expression());
        self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        let statement: Rc<Node> =
            self.do_inside_of_context(NodeFlags::InWithStatement, || self.parse_statement());
        self.with_jsdoc(
            self.finish_node(
                self.factory
                    .create_with_statement(self, expression, statement),
                pos,
                None,
            )
            .into(),
            has_jsdoc,
        )
    }

    pub(super) fn parse_case_clause(&self) -> CaseClause {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::CaseKeyword, None, None);
        let expression = self.allow_in_and(|| self.parse_expression());
        self.parse_expected(SyntaxKind::ColonToken, None, None);
        let statements = self.parse_list(ParsingContext::SwitchClauseStatements, &mut || {
            self.parse_statement()
        });
        self.finish_node(
            self.factory
                .create_case_clause(self, expression, statements),
            pos,
            None,
        )
    }
}
