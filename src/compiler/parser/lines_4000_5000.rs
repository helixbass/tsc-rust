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

    pub(super) fn parse_asserts_type_predicate(&self) -> Node /*TypeNode*/ {
        unimplemented!()
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
}
