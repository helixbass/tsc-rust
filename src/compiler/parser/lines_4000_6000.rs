#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::{ParserType, ParsingContext, SignatureFlags};
use crate::{
    get_binary_operator_precedence, ArrayLiteralExpression, BinaryExpression, Block,
    DiagnosticMessage, Diagnostics, Identifier, Node, NodeFlags, ObjectLiteralExpression,
    OperatorPrecedence, PrefixUnaryExpression, PropertyAssignment, ReturnStatement, SyntaxKind,
};

impl ParserType {
    pub(super) fn parse_type_predicate_prefix(&self) -> Option<Node> {
        let id = self.parse_identifier(None, None);
        if self.token() == SyntaxKind::IsKeyword && !self.scanner().has_preceding_line_break() {
            self.next_token();
            return Some(id);
        }
        None
    }

    pub(super) fn parse_type(&self) -> Node {
        self.do_outside_of_context(NodeFlags::TypeExcludesFlags, || self.parse_type_worker())
    }

    pub(super) fn parse_type_worker(&self) -> Node {
        let pos = self.get_node_pos();
        let type_ = self.parse_union_type_or_higher();
        type_
    }

    pub(super) fn parse_type_annotation(&self) -> Option<Node> {
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
                unimplemented!()
            }
            _ => self.is_identifier(),
        }
    }

    pub(super) fn is_start_of_expression(&self) -> bool {
        if self.is_start_of_left_hand_side_expression() {
            return true;
        }

        match self.token() {
            SyntaxKind::PlusPlusToken => true,
            _ => {
                if self.is_binary_operator() {
                    return true;
                }

                self.is_identifier()
            }
        }
    }

    pub(super) fn parse_expression(&self) -> Node {
        let expr = self.parse_assignment_expression_or_higher();

        expr
    }

    pub(super) fn parse_initializer(&self) -> Option<Node> {
        if self.parse_optional(SyntaxKind::EqualsToken) {
            Some(self.parse_assignment_expression_or_higher())
        } else {
            None
        }
    }

    pub(super) fn parse_assignment_expression_or_higher(&self) -> Node {
        let expr = self.parse_binary_expression_or_higher(OperatorPrecedence::Lowest);

        self.parse_conditional_expression_rest(expr)
    }

    pub(super) fn next_token_is_identifier_on_same_line(&self) -> bool {
        self.next_token();
        !self.scanner().has_preceding_line_break() && self.is_identifier()
    }

    pub(super) fn parse_conditional_expression_rest(&self, left_operand: Node) -> Node {
        left_operand
    }

    pub(super) fn parse_binary_expression_or_higher(&self, precedence: OperatorPrecedence) -> Node {
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
        mut left_operand: Node,
        pos: isize,
    ) -> Node {
        loop {
            let new_precedence = get_binary_operator_precedence(self.token());

            let consume_current_operator = new_precedence > precedence;

            if !consume_current_operator {
                break;
            }

            let operator_token = self.parse_token_node();
            let right = self.parse_binary_expression_or_higher(new_precedence);
            left_operand = self
                .make_binary_expression(left_operand, operator_token, right, pos)
                .into();
        }

        left_operand
    }

    pub(super) fn is_binary_operator(&self) -> bool {
        get_binary_operator_precedence(self.token()) > OperatorPrecedence::Comma
    }

    pub(super) fn make_binary_expression<TNode: Into<Rc<Node>>>(
        &self,
        left: Node,
        operator_token: TNode,
        right: Node,
        pos: isize,
    ) -> BinaryExpression {
        self.finish_node(
            self.factory.create_binary_expression(
                self,
                left.into(),
                operator_token.into(),
                right.into(),
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_prefix_unary_expression(&self) -> PrefixUnaryExpression {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory.create_prefix_unary_expression(
                self,
                self.token(),
                self.next_token_and(|| self.parse_simple_unary_expression().wrap()),
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_unary_expression_or_higher(&self) -> Node {
        if self.is_update_expression() {
            let update_expression = self.parse_update_expression();
            return update_expression;
        }

        panic!("Unimplemented");
    }

    pub(super) fn parse_simple_unary_expression(&self) -> Node {
        unimplemented!()
    }

    pub(super) fn is_update_expression(&self) -> bool {
        match self.token() {
            _ => true,
        }
    }

    pub(super) fn parse_update_expression(&self) -> Node {
        if self.token() == SyntaxKind::PlusPlusToken {
            let pos = self.get_node_pos();
            let operator = self.token();
            let operand = self.next_token_and(|| self.parse_left_hand_side_expression_or_higher());
            return self
                .finish_node(
                    self.factory
                        .create_prefix_unary_expression(self, operator, operand.into()),
                    pos,
                    None,
                )
                .into();
        }

        let expression = self.parse_left_hand_side_expression_or_higher();

        expression
    }

    pub(super) fn parse_left_hand_side_expression_or_higher(&self) -> Node {
        let expression = self.parse_member_expression_or_higher();

        self.parse_call_expression_rest(expression)
    }

    pub(super) fn parse_member_expression_or_higher(&self) -> Node {
        let expression = self.parse_primary_expression();
        self.parse_member_expression_rest(expression)
    }

    pub(super) fn parse_member_expression_rest(&self, expression: Node) -> Node {
        loop {
            return expression;
        }
    }

    pub(super) fn parse_call_expression_rest(&self, expression: Node) -> Node {
        expression
    }

    pub(super) fn parse_primary_expression(&self) -> Node {
        match self.token() {
            SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::StringLiteral
            | SyntaxKind::NoSubstitutionTemplateLiteral => return self.parse_literal_node().into(),
            SyntaxKind::TrueKeyword | SyntaxKind::FalseKeyword => {
                return self.parse_token_node().into()
            }
            SyntaxKind::OpenBracketToken => return self.parse_array_literal_expression().into(),
            SyntaxKind::OpenBraceToken => return self.parse_object_literal_expression().into(),
            SyntaxKind::TemplateHead => return self.parse_template_expression(false).into(),
            _ => (),
        }

        self.parse_identifier(Some(&Diagnostics::Expression_expected), None)
    }

    pub(super) fn parse_argument_or_array_literal_element(&self) -> Node {
        if false {
            unimplemented!()
        } else if false {
            unimplemented!()
        } else {
            self.parse_assignment_expression_or_higher()
        }
    }

    pub(super) fn parse_array_literal_expression(&self) -> ArrayLiteralExpression {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBracketToken, None, None);
        let multi_line = self.scanner().has_preceding_line_break();
        let elements = self.parse_delimited_list(
            ParsingContext::ArrayLiteralMembers,
            || self.parse_argument_or_array_literal_element().wrap(),
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

    pub(super) fn parse_object_literal_element(&self) -> Node {
        let pos = self.get_node_pos();

        let token_is_identifier = self.is_identifier();
        let name = self.parse_property_name();

        let node: PropertyAssignment;
        if false {
            unimplemented!()
        } else {
            self.parse_expected(SyntaxKind::ColonToken, None, None);
            let initializer = self.allow_in_and(|| self.parse_assignment_expression_or_higher());
            node = self
                .factory
                .create_property_assignment(self, name.wrap(), initializer.into());
        }
        self.finish_node(node.into(), pos, None)
    }

    pub(super) fn parse_object_literal_expression(&self) -> ObjectLiteralExpression {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBraceToken, None, None);
        let multi_line = self.scanner().has_preceding_line_break();
        let properties = self.parse_delimited_list(
            ParsingContext::ObjectLiteralMembers,
            || self.parse_object_literal_element().wrap(),
            Some(true),
        );
        if !self.parse_expected(SyntaxKind::CloseBraceToken, None, None) {
            unimplemented!()
        }
        self.finish_node(
            self.factory
                .create_object_literal_expression(self, Some(properties), Some(multi_line)),
            pos,
            None,
        )
    }

    pub(super) fn parse_optional_binding_identifier(&self) -> Option<Node> {
        if self.is_binding_identifier() {
            Some(self.parse_binding_identifier(None))
        } else {
            None
        }
    }

    pub(super) fn parse_block(
        &self,
        ignore_missing_open_brace: bool,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Block {
        let pos = self.get_node_pos();
        let open_brace_position = self.scanner().get_token_pos();
        if self.parse_expected(SyntaxKind::OpenBraceToken, diagnostic_message, None)
            || ignore_missing_open_brace
        {
            let multi_line = self.scanner().has_preceding_line_break();
            let statements = self.parse_list(ParsingContext::BlockStatements, &mut || {
                self.parse_statement().wrap()
            });
            if !self.parse_expected(SyntaxKind::CloseBraceToken, None, None) {
                unimplemented!()
            }
            let result = self.finish_node(
                self.factory
                    .create_block(self, statements, Some(multi_line)),
                pos,
                None,
            );
            if self.token() == SyntaxKind::EqualsToken {
                unimplemented!()
            }

            return result;
        } else {
            let statements = self.create_missing_list();
            self.finish_node(self.factory.create_block(self, statements, None), pos, None)
        }
    }

    pub(super) fn parse_function_block(
        &self,
        flags: SignatureFlags,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Block {
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

    pub(super) fn parse_empty_statement(&self) -> Node {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::SemicolonToken, None, None);
        self.finish_node(self.factory.create_empty_statement(self).into(), pos, None)
    }

    pub(super) fn parse_if_statement(&self) -> Node {
        let pos = self.get_node_pos();
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
        self.finish_node(
            self.factory
                .create_if_statement(
                    self,
                    expression.wrap(),
                    then_statement.wrap(),
                    else_statement.map(|else_statement| else_statement.wrap()),
                )
                .into(),
            pos,
            None,
        )
    }

    pub(super) fn parse_return_statement(&self) -> ReturnStatement {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::ReturnKeyword, None, None);
        let expression = if self.can_parse_semicolon() {
            None
        } else {
            Some(self.allow_in_and(|| self.parse_expression()))
        };
        self.parse_semicolon();
        self.finish_node(
            self.factory
                .create_return_statement(self, expression.map(Into::into)),
            pos,
            None,
        )
    }
}
