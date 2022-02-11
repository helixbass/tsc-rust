#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::{ParserType, ParsingContext, SignatureFlags};
use crate::{
    get_binary_operator_precedence, ArrayLiteralExpression, BinaryExpression, Block,
    DiagnosticMessage, Diagnostics, Identifier, Node, NodeFlags, ObjectLiteralExpression,
    OperatorPrecedence, PrefixUnaryExpression, PropertyAssignment, ReturnStatement, SyntaxKind,
};

impl ParserType {
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
