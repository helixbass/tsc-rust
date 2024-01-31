use std::collections::HashSet;

use gc::Gc;
use id_arena::Id;

use super::{ParserType, SignatureFlags, Tristate};
use crate::{
    get_binary_operator_precedence, is_assignment_operator, is_async_modifier,
    is_jsdoc_function_type, is_left_hand_side_expression, is_modifier_kind, node_is_present,
    skip_trivia, some, token_to_string, AsDoubleDeref, AsExpression, AwaitExpression,
    BinaryExpression, Debug_, DeleteExpression, Diagnostics, LanguageVariant, Node, NodeArray,
    NodeFlags, NodeInterface, OperatorPrecedence, PrefixUnaryExpression, ReadonlyTextRange,
    SyntaxKind, TypeOfExpression, VoidExpression, YieldExpression,
    HasArena, InArena, OptionInArena,
};

impl ParserType {
    pub(super) fn parse_type_predicate_prefix(&self) -> Option<Node /*Identifier*/> {
        let id = self.parse_identifier(None, None);
        if self.token() == SyntaxKind::IsKeyword && !self.scanner().has_preceding_line_break() {
            self.next_token();
            return Some(id);
        }
        None
    }

    pub(super) fn parse_asserts_type_predicate(&self) -> Node /*TypeNode*/ {
        let pos = self.get_node_pos();
        let asserts_modifier = self.parse_expected_token(SyntaxKind::AssertsKeyword, None, None);
        let parameter_name: Node = if self.token() == SyntaxKind::ThisKeyword {
            self.parse_this_type_node().into()
        } else {
            self.parse_identifier(None, None)
        };
        let type_ = if self.parse_optional(SyntaxKind::IsKeyword) {
            Some(self.parse_type())
        } else {
            None
        };
        self.finish_node(
            self.factory()
                .ref_(self).create_type_predicate_node_raw(
                    Some(asserts_modifier.alloc(self.arena())),
                    parameter_name.alloc(self.arena()),
                    type_,
                )
                .into(),
            pos,
            None,
        )
    }

    pub(super) fn parse_type(&self) -> Id<Node /*TypeNode*/> {
        self.do_outside_of_context(NodeFlags::TypeExcludesFlags, || {
            self.parse_type_worker(None)
        })
    }

    pub(super) fn parse_type_worker(
        &self,
        no_conditional_types: Option<bool>,
    ) -> Id<Node /*TypeNode*/> {
        let no_conditional_types = no_conditional_types.unwrap_or(false);
        if self.is_start_of_function_type_or_constructor_type() {
            return self.parse_function_or_constructor_type();
        }
        let pos = self.get_node_pos();
        let type_ = self.parse_union_type_or_higher();
        if !no_conditional_types
            && !self.scanner().has_preceding_line_break()
            && self.parse_optional(SyntaxKind::ExtendsKeyword)
        {
            let extends_type = self.parse_type_worker(Some(true));
            self.parse_expected(SyntaxKind::QuestionToken, None, None);
            let true_type = self.parse_type_worker(None);
            self.parse_expected(SyntaxKind::ColonToken, None, None);
            let false_type = self.parse_type_worker(None);
            return self
                .finish_node(
                    self.factory().ref_(self).create_conditional_type_node_raw(
                        type_,
                        extends_type,
                        true_type,
                        false_type,
                    ),
                    pos,
                    None,
                )
                .alloc(self.arena());
        }
        type_
    }

    pub(super) fn parse_type_annotation(&self) -> Option<Id<Node /*TypeNode*/>> {
        if self.parse_optional(SyntaxKind::ColonToken) {
            Some(self.parse_type())
        } else {
            None
        }
    }

    pub(super) fn is_start_of_left_hand_side_expression(&self) -> bool {
        match self.token() {
            SyntaxKind::ThisKeyword
            | SyntaxKind::SuperKeyword
            | SyntaxKind::NullKeyword
            | SyntaxKind::TrueKeyword
            | SyntaxKind::FalseKeyword
            | SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::StringLiteral
            | SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::TemplateHead
            | SyntaxKind::OpenParenToken
            | SyntaxKind::OpenBracketToken
            | SyntaxKind::OpenBraceToken
            | SyntaxKind::FunctionKeyword
            | SyntaxKind::ClassKeyword
            | SyntaxKind::NewKeyword
            | SyntaxKind::SlashToken
            | SyntaxKind::SlashEqualsToken
            | SyntaxKind::Identifier => true,
            SyntaxKind::ImportKeyword => {
                self.look_ahead_bool(|| self.next_token_is_open_paren_or_less_than_or_dot())
            }
            _ => self.is_identifier(),
        }
    }

    pub(super) fn is_start_of_expression(&self) -> bool {
        if self.is_start_of_left_hand_side_expression() {
            return true;
        }

        match self.token() {
            SyntaxKind::PlusToken
            | SyntaxKind::MinusToken
            | SyntaxKind::TildeToken
            | SyntaxKind::ExclamationToken
            | SyntaxKind::DeleteKeyword
            | SyntaxKind::TypeOfKeyword
            | SyntaxKind::VoidKeyword
            | SyntaxKind::PlusPlusToken
            | SyntaxKind::MinusMinusToken
            | SyntaxKind::LessThanToken
            | SyntaxKind::AwaitKeyword
            | SyntaxKind::YieldKeyword
            | SyntaxKind::PrivateIdentifier => true,
            _ => {
                if self.is_binary_operator() {
                    return true;
                }

                self.is_identifier()
            }
        }
    }

    pub(super) fn is_start_of_expression_statement(&self) -> bool {
        !matches!(
            self.token(),
            SyntaxKind::OpenBraceToken
                | SyntaxKind::FunctionKeyword
                | SyntaxKind::ClassKeyword
                | SyntaxKind::AtToken
        ) && self.is_start_of_expression()
    }

    pub(super) fn parse_expression(&self) -> Id<Node> {
        let save_decorator_context = self.in_decorator_context();
        if save_decorator_context {
            self.set_decorator_context(false);
        }

        let pos = self.get_node_pos();
        let mut expr = self.parse_assignment_expression_or_higher();
        let mut operator_token: Option<Id<Node /*BinaryOperatorToken*/>>;
        while {
            operator_token = self
                .parse_optional_token(SyntaxKind::CommaToken)
                .map(|node| node.alloc(self.arena()));
            operator_token.is_some()
        } {
            expr = self
                .make_binary_expression(
                    expr,
                    operator_token.clone().unwrap(),
                    self.parse_assignment_expression_or_higher(),
                    pos,
                )
                .alloc(self.arena());
        }

        if save_decorator_context {
            self.set_decorator_context(true);
        }

        expr
    }

    pub(super) fn parse_initializer(&self) -> Option<Id<Node>> {
        if self.parse_optional(SyntaxKind::EqualsToken) {
            Some(self.parse_assignment_expression_or_higher())
        } else {
            None
        }
    }

    pub(super) fn parse_assignment_expression_or_higher(&self) -> Id<Node> {
        if self.is_yield_expression() {
            return self.parse_yield_expression().alloc(self.arena());
        }

        let arrow_expression = self
            .try_parse_parenthesized_arrow_function_expression()
            .or_else(|| self.try_parse_async_simple_arrow_function_expression());
        if let Some(arrow_expression) = arrow_expression {
            return arrow_expression;
        }

        let pos = self.get_node_pos();
        let expr = self.parse_binary_expression_or_higher(OperatorPrecedence::Lowest);

        if expr.ref_(self).kind() == SyntaxKind::Identifier
            && self.token() == SyntaxKind::EqualsGreaterThanToken
        {
            return self
                .parse_simple_arrow_function_expression(pos, expr, None)
                .into();
        }

        if is_left_hand_side_expression(expr, self)
            && is_assignment_operator(self.re_scan_greater_token())
        {
            return self
                .make_binary_expression(
                    expr,
                    self.parse_token_node().alloc(self.arena()),
                    self.parse_assignment_expression_or_higher(),
                    pos,
                )
                .alloc(self.arena());
        }

        self.parse_conditional_expression_rest(expr, pos)
    }

    pub(super) fn is_yield_expression(&self) -> bool {
        if self.token() == SyntaxKind::YieldKeyword {
            if self.in_yield_context() {
                return true;
            }

            return self.look_ahead_bool(|| {
                self.next_token_is_identifier_or_keyword_or_literal_on_same_line()
            });
        }

        false
    }

    pub(super) fn next_token_is_identifier_on_same_line(&self) -> bool {
        self.next_token();
        !self.scanner().has_preceding_line_break() && self.is_identifier()
    }

    pub(super) fn parse_yield_expression(&self) -> YieldExpression {
        let pos = self.get_node_pos();

        self.next_token();

        if !self.scanner().has_preceding_line_break()
            && (self.token() == SyntaxKind::AsteriskToken || self.is_start_of_expression())
        {
            self.finish_node(
                self.factory().ref_(self).create_yield_expression_raw(
                    self.parse_optional_token(SyntaxKind::AsteriskToken)
                        .map(|asterisk_token| asterisk_token.alloc(self.arena())),
                    Some(self.parse_assignment_expression_or_higher()),
                ),
                pos,
                None,
            )
        } else {
            self.finish_node(
                self.factory().ref_(self).create_yield_expression_raw(None, None),
                pos,
                None,
            )
        }
    }

    pub(super) fn parse_simple_arrow_function_expression(
        &self,
        pos: isize,
        identifier: Id<Node /*Identifier*/>,
        async_modifier: Option<Id<NodeArray> /*<Modifier>*/>,
    ) -> Id<Node> {
        Debug_.assert(
            self.token() == SyntaxKind::EqualsGreaterThanToken,
            Some("parseSimpleArrowFunctionExpression should only have been called if we had a =>"),
        );
        let identifier_pos = identifier.ref_(self).pos();
        let parameter = self.factory().ref_(self).create_parameter_declaration_raw(
            Option::<Id<NodeArray>>::None,
            Option::<Id<NodeArray>>::None,
            None,
            Some(identifier),
            None,
            None,
            None,
        );
        let parameter = self.finish_node(parameter, identifier_pos, None);

        let parameter_pos = parameter.pos();
        let parameter_end = parameter.end();
        let parameters = self.create_node_array(
            vec![parameter.alloc(self.arena())],
            parameter_pos,
            Some(parameter_end),
            None,
        );
        let equals_greater_than_token =
            self.parse_expected_token(SyntaxKind::EqualsGreaterThanToken, None, None);
        let body = self.parse_arrow_function_expression_body(async_modifier.is_some());
        let node = self.factory().ref_(self).create_arrow_function_raw(
            async_modifier,
            Option::<Id<NodeArray>>::None,
            parameters,
            None,
            Some(equals_greater_than_token.alloc(self.arena())),
            body,
        );
        self.add_jsdoc_comment(self.finish_node(node, pos, None).alloc(self.arena()))
    }

    pub(super) fn try_parse_parenthesized_arrow_function_expression(
        &self,
    ) -> Option<Id<Node /*Expression*/>> {
        let tri_state = self.is_parenthesized_arrow_function_expression();
        if tri_state == Tristate::False {
            return None;
        }

        if tri_state == Tristate::True {
            self.parse_parenthesized_arrow_function_expression(true)
        } else {
            self.try_parse(|| self.parse_possible_parenthesized_arrow_function_expression())
        }
    }

    pub(super) fn is_parenthesized_arrow_function_expression(&self) -> Tristate {
        if matches!(
            self.token(),
            SyntaxKind::OpenParenToken | SyntaxKind::LessThanToken | SyntaxKind::AsyncKeyword
        ) {
            return self
                .look_ahead(|| Some(self.is_parenthesized_arrow_function_expression_worker()))
                .unwrap();
        }

        if self.token() == SyntaxKind::EqualsGreaterThanToken {
            return Tristate::True;
        }

        Tristate::False
    }

    pub(super) fn is_parenthesized_arrow_function_expression_worker(&self) -> Tristate {
        if self.token() == SyntaxKind::AsyncKeyword {
            self.next_token();
            if self.scanner().has_preceding_line_break() {
                return Tristate::False;
            }
            if !matches!(
                self.token(),
                SyntaxKind::OpenParenToken | SyntaxKind::LessThanToken
            ) {
                return Tristate::False;
            }
        }

        let first = self.token();
        let second = self.next_token();

        if first == SyntaxKind::OpenParenToken {
            if second == SyntaxKind::CloseParenToken {
                let third = self.next_token();
                match third {
                    SyntaxKind::EqualsGreaterThanToken
                    | SyntaxKind::ColonToken
                    | SyntaxKind::OpenBraceToken => {
                        return Tristate::True;
                    }
                    _ => {
                        return Tristate::False;
                    }
                }
            }

            if matches!(
                second,
                SyntaxKind::OpenBracketToken | SyntaxKind::OpenBraceToken
            ) {
                return Tristate::Unknown;
            }

            if second == SyntaxKind::DotDotDotToken {
                return Tristate::True;
            }

            if is_modifier_kind(second)
                && second != SyntaxKind::AsyncKeyword
                && self.look_ahead_bool(|| self.next_token_is_identifier())
            {
                return Tristate::True;
            }

            if !self.is_identifier() && second != SyntaxKind::ThisKeyword {
                return Tristate::False;
            }

            match self.next_token() {
                SyntaxKind::ColonToken => {
                    return Tristate::True;
                }
                SyntaxKind::QuestionToken => {
                    self.next_token();
                    if matches!(
                        self.token(),
                        SyntaxKind::ColonToken
                            | SyntaxKind::CommaToken
                            | SyntaxKind::EqualsToken
                            | SyntaxKind::CloseParenToken
                    ) {
                        return Tristate::True;
                    }
                    return Tristate::False;
                }
                SyntaxKind::CommaToken | SyntaxKind::EqualsToken | SyntaxKind::CloseParenToken => {
                    return Tristate::Unknown;
                }
                _ => (),
            }
            return Tristate::False;
        } else {
            Debug_.assert(first == SyntaxKind::LessThanToken, None);

            if !self.is_identifier() {
                return Tristate::False;
            }

            if self.language_variant() == LanguageVariant::JSX {
                let is_arrow_function_in_jsx = self.look_ahead_bool(|| {
                    let third = self.next_token();
                    if third == SyntaxKind::ExtendsKeyword {
                        let fourth = self.next_token();
                        match fourth {
                            SyntaxKind::EqualsToken | SyntaxKind::GreaterThanToken => false,
                            _ => true,
                        }
                    } else if third == SyntaxKind::CommaToken {
                        true
                    } else {
                        false
                    }
                });

                if is_arrow_function_in_jsx {
                    return Tristate::True;
                }

                return Tristate::False;
            }

            return Tristate::Unknown;
        }
    }

    pub(super) fn parse_possible_parenthesized_arrow_function_expression(
        &self,
    ) -> Option<Id<Node /*ArrowFunction*/>> {
        let token_pos = self.scanner().get_token_pos();
        {
            let not_parenthesized_arrow = self.maybe_not_parenthesized_arrow();
            if matches!(&*not_parenthesized_arrow, Some(not_parenthesized_arrow) if not_parenthesized_arrow.contains(&token_pos))
            {
                return None;
            }
        }

        let result = self.parse_parenthesized_arrow_function_expression(false);
        if result.is_none() {
            let mut not_parenthesized_arrow = self.maybe_not_parenthesized_arrow();
            if not_parenthesized_arrow.is_none() {
                *not_parenthesized_arrow = Some(HashSet::new());
            }
            not_parenthesized_arrow.as_mut().unwrap().insert(token_pos);
        }

        result
    }

    pub(super) fn try_parse_async_simple_arrow_function_expression(
        &self,
    ) -> Option<Id<Node /*ArrowFunction*/>> {
        if self.token() == SyntaxKind::AsyncKeyword {
            if self
                .look_ahead(|| Some(self.is_un_parenthesized_async_arrow_function_worker()))
                .unwrap()
                == Tristate::True
            {
                let pos = self.get_node_pos();
                let async_modifier = self.parse_modifiers_for_arrow_function();
                let expr = self.parse_binary_expression_or_higher(OperatorPrecedence::Lowest);
                return Some(self.parse_simple_arrow_function_expression(
                    pos,
                    expr,
                    async_modifier,
                ));
            }
        }
        None
    }

    pub(super) fn is_un_parenthesized_async_arrow_function_worker(&self) -> Tristate {
        if self.token() == SyntaxKind::AsyncKeyword {
            self.next_token();
            if self.scanner().has_preceding_line_break()
                || self.token() == SyntaxKind::EqualsGreaterThanToken
            {
                return Tristate::False;
            }
            let expr = self.parse_binary_expression_or_higher(OperatorPrecedence::Lowest);
            if !self.scanner().has_preceding_line_break()
                && expr.ref_(self).kind() == SyntaxKind::Identifier
                && self.token() == SyntaxKind::EqualsGreaterThanToken
            {
                return Tristate::True;
            }
        }

        Tristate::False
    }

    pub(super) fn parse_parenthesized_arrow_function_expression(
        &self,
        allow_ambiguity: bool,
    ) -> Option<Id<Node /*ArrowFunction*/>> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        let modifiers = self.parse_modifiers_for_arrow_function();
        let is_async = if some(
            modifiers.refed(self).as_double_deref(),
            Some(|modifier: &Id<Node>| is_async_modifier(&modifier.ref_(self))),
        ) {
            SignatureFlags::Await
        } else {
            SignatureFlags::None
        };
        let type_parameters = self.parse_type_parameters();

        let parameters: Id<NodeArray>;
        if !self.parse_expected(SyntaxKind::OpenParenToken, None, None) {
            if !allow_ambiguity {
                return None;
            }
            parameters = self.create_missing_list();
        } else {
            parameters = self.parse_parameters_worker(is_async);
            if !self.parse_expected(SyntaxKind::CloseParenToken, None, None) && !allow_ambiguity {
                return None;
            }
        }

        let type_ = self.parse_return_type(SyntaxKind::ColonToken, false);
        if let Some(type_) = type_ {
            if !allow_ambiguity && self.type_has_arrow_function_blocking_parse_error(type_) {
                return None;
            }
        }

        let has_jsdoc_function_type =
            matches!(type_, Some(type_) if is_jsdoc_function_type(&type_.ref_(self)));
        if !allow_ambiguity
            && self.token() != SyntaxKind::EqualsGreaterThanToken
            && (has_jsdoc_function_type || self.token() != SyntaxKind::OpenBraceToken)
        {
            return None;
        }

        let last_token = self.token();
        let equals_greater_than_token =
            self.parse_expected_token(SyntaxKind::EqualsGreaterThanToken, None, None);
        let body: Id<Node> = if matches!(
            last_token,
            SyntaxKind::EqualsGreaterThanToken | SyntaxKind::OpenBraceToken
        ) {
            self.parse_arrow_function_expression_body(some(
                modifiers.refed(self).as_double_deref(),
                Some(|modifier: &Id<Node>| is_async_modifier(&modifier.ref_(self))),
            ))
        } else {
            self.parse_identifier(None, None).alloc(self.arena())
        };

        let node = self.factory().ref_(self).create_arrow_function_raw(
            modifiers,
            type_parameters,
            parameters,
            type_,
            Some(equals_greater_than_token.alloc(self.arena())),
            body,
        );
        Some(self.with_jsdoc(self.finish_node(node, pos, None).alloc(self.arena()), has_jsdoc))
    }

    pub(super) fn parse_arrow_function_expression_body(
        &self,
        is_async: bool,
    ) -> Id<Node /*Block | Expression*/> {
        if self.token() == SyntaxKind::OpenBraceToken {
            return self.parse_function_block(
                if is_async {
                    SignatureFlags::Await
                } else {
                    SignatureFlags::None
                },
                None,
            );
        }

        if !matches!(
            self.token(),
            SyntaxKind::SemicolonToken | SyntaxKind::FunctionKeyword | SyntaxKind::ClassKeyword
        ) && self.is_start_of_statement()
            && !self.is_start_of_expression_statement()
        {
            return self.parse_function_block(
                SignatureFlags::IgnoreMissingOpenBrace
                    | if is_async {
                        SignatureFlags::Await
                    } else {
                        SignatureFlags::None
                    },
                None,
            );
        }

        let saved_top_level = self.top_level();
        self.set_top_level(false);
        let node = if is_async {
            self.do_in_await_context(|| self.parse_assignment_expression_or_higher())
        } else {
            self.do_outside_of_await_context(|| self.parse_assignment_expression_or_higher())
        };
        self.set_top_level(saved_top_level);
        node
    }

    pub(super) fn parse_conditional_expression_rest(
        &self,
        left_operand: Id<Node>,
        pos: isize,
    ) -> Id<Node /*Expression*/> {
        let question_token = self.parse_optional_token(SyntaxKind::QuestionToken);
        if question_token.is_none() {
            return left_operand;
        }
        let question_token = question_token.unwrap();

        let when_true = self.do_outside_of_context(self.disallow_in_and_decorator_context, || {
            self.parse_assignment_expression_or_higher()
        });
        let colon_token = self.parse_expected_token(SyntaxKind::ColonToken, None, None);
        let is_colon_token_present = node_is_present(Some(&colon_token));
        return self
            .finish_node(
                self.factory().ref_(self).create_conditional_expression_raw(
                    left_operand,
                    Some(question_token.alloc(self.arena())),
                    when_true,
                    Some(colon_token.alloc(self.arena())),
                    if is_colon_token_present {
                        self.parse_assignment_expression_or_higher()
                    } else {
                        self.create_missing_node(
                            SyntaxKind::Identifier,
                            false,
                            Some(&Diagnostics::_0_expected),
                            Some(vec![token_to_string(SyntaxKind::ColonToken)
                                .unwrap()
                                .to_owned()]),
                        )
                        .alloc(self.arena())
                    },
                ),
                pos,
                None,
            )
            .alloc(self.arena());
    }

    pub(super) fn parse_binary_expression_or_higher(
        &self,
        precedence: OperatorPrecedence,
    ) -> Id<Node /*Expression*/> {
        let pos = self.get_node_pos();
        let left_operand = self.parse_unary_expression_or_higher();
        self.parse_binary_expression_rest(precedence, left_operand, pos)
    }

    pub(super) fn is_in_or_of_keyword(&self, t: SyntaxKind) -> bool {
        matches!(t, SyntaxKind::InKeyword | SyntaxKind::OfKeyword)
    }

    pub(super) fn parse_binary_expression_rest(
        &self,
        precedence: OperatorPrecedence,
        mut left_operand: Id<Node>,
        pos: isize,
    ) -> Id<Node> {
        loop {
            self.re_scan_greater_token();
            let new_precedence = get_binary_operator_precedence(self.token());

            let consume_current_operator = if self.token() == SyntaxKind::AsteriskAsteriskToken {
                new_precedence >= precedence
            } else {
                new_precedence > precedence
            };

            if !consume_current_operator {
                break;
            }

            if self.token() == SyntaxKind::InKeyword && self.in_disallow_in_context() {
                break;
            }

            if self.token() == SyntaxKind::AsKeyword {
                if self.scanner().has_preceding_line_break() {
                    break;
                } else {
                    self.next_token();
                    left_operand = self
                        .make_as_expression(left_operand, self.parse_type())
                        .alloc(self.arena());
                }
            } else {
                left_operand = self
                    .make_binary_expression(
                        left_operand,
                        self.parse_token_node().alloc(self.arena()),
                        self.parse_binary_expression_or_higher(new_precedence),
                        pos,
                    )
                    .alloc(self.arena());
            }
        }

        left_operand
    }

    pub(super) fn is_binary_operator(&self) -> bool {
        if self.in_disallow_in_context() && self.token() == SyntaxKind::InKeyword {
            return false;
        }

        get_binary_operator_precedence(self.token()) > OperatorPrecedence::Comma
    }

    pub(super) fn make_binary_expression(
        &self,
        left: Id<Node>,
        operator_token: Id<Node>,
        right: Id<Node>,
        pos: isize,
    ) -> BinaryExpression {
        self.finish_node(
            self.factory()
                .ref_(self).create_binary_expression_raw(left, operator_token, right),
            pos,
            None,
        )
    }

    pub(super) fn make_as_expression(&self, left: Id<Node>, right: Id<Node>) -> AsExpression {
        let left_pos = left.ref_(self).pos();
        self.finish_node(
            self.factory().ref_(self).create_as_expression_raw(left, right),
            left_pos,
            None,
        )
    }

    pub(super) fn parse_prefix_unary_expression(&self) -> PrefixUnaryExpression {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory().ref_(self).create_prefix_unary_expression_raw(
                self.token(),
                self.next_token_and(|| self.parse_simple_unary_expression()),
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_delete_expression(&self) -> DeleteExpression {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory().ref_(self).create_delete_expression_raw(
                self.next_token_and(|| self.parse_simple_unary_expression()),
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_type_of_expression(&self) -> TypeOfExpression {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory().ref_(self).create_type_of_expression_raw(
                self.next_token_and(|| self.parse_simple_unary_expression()),
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_void_expression(&self) -> VoidExpression {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory().ref_(self).create_void_expression_raw(
                self.next_token_and(|| self.parse_simple_unary_expression()),
            ),
            pos,
            None,
        )
    }

    pub(super) fn is_await_expression(&self) -> bool {
        if self.token() == SyntaxKind::AwaitKeyword {
            if self.in_await_context() {
                return true;
            }

            return self.look_ahead_bool(|| {
                self.next_token_is_identifier_or_keyword_or_literal_on_same_line()
            });
        }

        false
    }

    pub(super) fn parse_await_expression(&self) -> AwaitExpression {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory().ref_(self).create_await_expression_raw(
                self.next_token_and(|| self.parse_simple_unary_expression()),
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_unary_expression_or_higher(&self) -> Id<Node> {
        if self.is_update_expression() {
            let pos = self.get_node_pos();
            let update_expression = self.parse_update_expression();
            return if self.token() == SyntaxKind::AsteriskAsteriskToken {
                self.parse_binary_expression_rest(
                    get_binary_operator_precedence(self.token()),
                    update_expression,
                    pos,
                )
            } else {
                update_expression
            };
        }

        let unary_operator = self.token();
        let simple_unary_expression = self.parse_simple_unary_expression();
        if self.token() == SyntaxKind::AsteriskAsteriskToken {
            let pos = skip_trivia(
                &self.source_text_as_chars(),
                simple_unary_expression.ref_(self).pos(),
                None,
                None,
                None,
            );
            let end = simple_unary_expression.ref_(self).end();
            if simple_unary_expression.ref_(self).kind() == SyntaxKind::TypeAssertionExpression {
                self.parse_error_at(pos, end, &Diagnostics::A_type_assertion_expression_is_not_allowed_in_the_left_hand_side_of_an_exponentiation_expression_Consider_enclosing_the_expression_in_parentheses, None);
            } else {
                self.parse_error_at(pos, end, &Diagnostics::An_unary_expression_with_the_0_operator_is_not_allowed_in_the_left_hand_side_of_an_exponentiation_expression_Consider_enclosing_the_expression_in_parentheses,
                                    Some(vec![token_to_string(unary_operator).unwrap().to_owned()]));
            }
        }
        simple_unary_expression
    }

    pub(super) fn parse_simple_unary_expression(&self) -> Id<Node /*UnaryExpression*/> {
        match self.token() {
            SyntaxKind::PlusToken
            | SyntaxKind::MinusToken
            | SyntaxKind::TildeToken
            | SyntaxKind::ExclamationToken => self.parse_prefix_unary_expression().alloc(self.arena()),
            SyntaxKind::DeleteKeyword => self.parse_delete_expression().alloc(self.arena()),
            SyntaxKind::TypeOfKeyword => self.parse_type_of_expression().alloc(self.arena()),
            SyntaxKind::VoidKeyword => self.parse_void_expression().alloc(self.arena()),
            SyntaxKind::LessThanToken => self.parse_type_assertion().alloc(self.arena()),
            SyntaxKind::AwaitKeyword => {
                if self.is_await_expression() {
                    self.parse_await_expression().alloc(self.arena())
                } else {
                    self.parse_update_expression()
                }
            }
            _ => self.parse_update_expression(),
        }
    }

    pub(super) fn is_update_expression(&self) -> bool {
        match self.token() {
            SyntaxKind::PlusToken
            | SyntaxKind::MinusToken
            | SyntaxKind::TildeToken
            | SyntaxKind::ExclamationToken
            | SyntaxKind::DeleteKeyword
            | SyntaxKind::TypeOfKeyword
            | SyntaxKind::VoidKeyword
            | SyntaxKind::AwaitKeyword => false,
            SyntaxKind::LessThanToken => {
                if self.language_variant() != LanguageVariant::JSX {
                    false
                } else {
                    true
                }
            }
            _ => true,
        }
    }

    pub(super) fn parse_update_expression(&self) -> Id<Node> {
        if matches!(
            self.token(),
            SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
        ) {
            let pos = self.get_node_pos();
            return self
                .finish_node(
                    self.factory().ref_(self).create_prefix_unary_expression_raw(
                        self.token(),
                        self.next_token_and(|| self.parse_left_hand_side_expression_or_higher()),
                    ),
                    pos,
                    None,
                )
                .alloc(self.arena());
        } else if self.language_variant() == LanguageVariant::JSX
            && self.token() == SyntaxKind::LessThanToken
            && self.look_ahead_bool(|| self.next_token_is_identifier_or_keyword_or_greater_than())
        {
            return self
                .parse_jsx_element_or_self_closing_element_or_fragment(true, None, None)
                .alloc(self.arena());
        }

        let expression = self.parse_left_hand_side_expression_or_higher();

        Debug_.assert(is_left_hand_side_expression(expression, self), None);
        if matches!(
            self.token(),
            SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
        ) && !self.scanner().has_preceding_line_break()
        {
            let operator = self.token();
            self.next_token();
            let expression_pos = expression.ref_(self).pos();
            return self
                .finish_node(
                    self.factory()
                        .ref_(self).create_postfix_unary_expression_raw(expression, operator),
                    expression_pos,
                    None,
                )
                .alloc(self.arena());
        }

        expression
    }

    pub(super) fn parse_left_hand_side_expression_or_higher(
        &self,
    ) -> Id<Node /*LeftHandSideExpression*/> {
        let pos = self.get_node_pos();
        let expression: Id<Node>;
        if self.token() == SyntaxKind::ImportKeyword {
            if self.look_ahead_bool(|| self.next_token_is_open_paren_or_less_than()) {
                self.set_source_flags(
                    self.source_flags() | NodeFlags::PossiblyContainsDynamicImport,
                );
                expression = self.parse_token_node().alloc(self.arena());
            } else if self.look_ahead_bool(|| self.next_token_is_dot()) {
                self.next_token();
                self.next_token();
                expression = self
                    .finish_node(
                        self.factory().ref_(self).create_meta_property_raw(
                            SyntaxKind::ImportKeyword,
                            self.parse_identifier_name(None).alloc(self.arena()),
                        ),
                        pos,
                        None,
                    )
                    .alloc(self.arena());
            } else {
                expression = self.parse_member_expression_or_higher();
            }
        } else {
            expression = if self.token() == SyntaxKind::SuperKeyword {
                self.parse_super_expression().alloc(self.arena())
            } else {
                self.parse_member_expression_or_higher()
            };
        }

        self.parse_call_expression_rest(pos, expression)
    }

    pub(super) fn parse_member_expression_or_higher(&self) -> Id<Node /*MemberExpression*/> {
        let pos = self.get_node_pos();
        let expression = self.parse_primary_expression();
        self.parse_member_expression_rest(pos, expression, true)
    }
}
