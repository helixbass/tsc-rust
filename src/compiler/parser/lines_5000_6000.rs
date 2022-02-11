#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::{ParserType, ParsingContext, SignatureFlags};
use crate::{
    ArrayLiteralExpression, Block, DiagnosticMessage, Diagnostics, Node, NodeArray,
    ObjectLiteralExpression, PropertyAssignment, ReturnStatement, SyntaxKind, TypeAssertion,
};

impl ParserType {
    pub(super) fn parse_super_expression(&self) -> Node /*MemberExpression*/ {
        let pos = self.get_node_pos();
        let expression = self.parse_token_node();
        if self.token() == SyntaxKind::LessThanToken {
            let start_pos = self.get_node_pos();
            let type_arguments = self.try_parse(|| self.parse_type_arguments_in_expression());
            if type_arguments.is_some() {
                self.parse_error_at(
                    start_pos,
                    self.get_node_pos(),
                    &Diagnostics::super_may_not_use_type_arguments,
                    None,
                );
            }
        }

        if matches!(
            self.token(),
            SyntaxKind::OpenParenToken | SyntaxKind::DotToken | SyntaxKind::OpenBracketToken
        ) {
            return expression.into();
        }

        self.parse_expected_token(
            SyntaxKind::DotToken,
            Some(&Diagnostics::super_must_be_followed_by_an_argument_list_or_member_access),
            None,
        );
        self.finish_node(
            self.factory
                .create_property_access_expression(
                    self,
                    expression.into(),
                    self.parse_right_side_of_dot(true, true).wrap(),
                )
                .into(),
            pos,
            None,
        )
    }

    pub(super) fn parse_jsx_element_or_self_closing_element_or_fragment(
        &self,
        in_expression_context: bool,
        top_invalid_node_position: Option<isize>,
        opening_tag: Option<Rc<Node /*JsxOpeningElement | JsxOpeningFragment*/>>,
    ) -> Node /*JsxElement | JsxSelfClosingElement | JsxFragment*/ {
        let pos = self.get_node_pos();
        let opening = self
            .parse_jsx_opening_or_self_closing_element_or_opening_fragment(in_expression_context);
        let result: Node;
        if opening.kind() == SyntaxKind::JsxOpeningElement {
            let mut children = self.parse_jsx_children(&opening);
            let closing_element: JsxClosingElement;

            let opening_as_jsx_opening_element = opening.as_jsx_opening_element();
            let last_child = if children.is_empty() {
                None
            } else {
                Some(&children[children.len() - 1])
            };
            if let Some(last_child) = last_child.filter(|last_child| {
                if last_child.kind() != SyntaxKind::JsxElement {
                    return false;
                }
                let last_child_as_jsx_element = last_child.as_jsx_element();
                !tag_names_are_equivalent(
                    &last_child.opening_element.as_jsx_opening_element().tag_name,
                    &last_child.closing_element.as_jsx_closing_element().tag_name,
                ) && tag_names_are_equivalent(
                    &opening_as_jsx_opening_element.tag_name,
                    &last_child.closing_element.as_jsx_closing_element().tag_name,
                )
            }) {
                let last_child_as_jsx_element = last_child.as_jsx_element();
                let end = last_child_as_jsx_element.children.end();
                let last_child_opening_element_pos =
                    last_child_as_jsx_element.opening_element.pos();
                let new_last = self.finish_node(
                    self.factory.create_jsx_element(
                        last_child_as_jsx_element.opening_element.clone(),
                        last_child_as_jsx_element.children.clone(),
                        self.finish_node(
                            self.factory.create_jsx_closing_element(
                                self,
                                self.finish_node(
                                    self.factory.create_identifier(self, ""),
                                    end,
                                    Some(end),
                                ),
                            ),
                            end,
                            Some(end),
                        ),
                    ),
                    last_child_opening_element_pos,
                    Some(end),
                );

                let children_pos = children.pos();
                let children = children.to_vec();
                children[children.len() - 1] = new_last;
                let children = self.create_node_array(children, children_pos, end);
                closing_element = last_child_as_jsx_element.closing_element.clone();
            } else {
                closing_element = self.parse_jsx_closing_element(&opening, in_expression_context);
                if !tag_names_are_equivalent(
                    &opening_as_jsx_opening_element.tag_name,
                    &closing_element.tag_name,
                ) {
                    if let Some(opening_tag) = opening_tag.filter(|opening_tag| {
                        is_jsx_opening_element(&opening_tag)
                            && tag_names_are_equivalent(
                                &closing_element.tag_name,
                                &opening_tag.as_jsx_opening_element().tag_name,
                            )
                    }) {
                        self.parse_error_at_range(
                            &opening_as_jsx_opening_element.tag_name,
                            &Diagnostics::JSX_element_0_has_no_corresponding_closing_tag,
                            Some(vec![get_text_of_node_from_source_text(
                                self.source_text_as_chars(),
                                &opening_as_jsx_opening_element.tag_name,
                            )]),
                        );
                    } else {
                        self.parse_error_at_range(
                            &closing_element.tag_name,
                            &Diagnostics::Expected_corresponding_JSX_closing_tag_for_0,
                            Some(vec![get_text_of_node_from_source_text(
                                self.source_text_as_chars(),
                                &opening_as_jsx_opening_element.tag_name,
                            )]),
                        );
                    }
                }
            }
            result = self.finish_node(
                self.factory
                    .create_jsx_element(self, opening, children, closing_element),
                pos,
                None,
            );
        } else if opening.kind() == SyntaxKind::JsxOpeningFragment {
            let children = self.parse_jsx_children(&opening);
            result = self.finish_node(
                self.factory.create_jsx_fragment(
                    self,
                    opening,
                    children,
                    self.parse_jsx_closing_fragment(in_expression_context),
                ),
                pos,
                None,
            );
        } else {
            Debug_.assert(opening.kind() == SyntaxKind::JsxSelfClosingElement);
            result = opening;
        }

        if in_expression_context && self.token() == SyntaxKind::LessThanToken {
            let top_bad_pos = top_invalid_node_position.unwrap_or_else(|| result.pos());
            let invalid_element = self.try_parse(|| {
                self.parse_jsx_element_or_self_closing_element_or_fragment(true, top_bad_pos)
            });
            if let Some(invalid_element) = invalid_element {
                let operator_token = self.create_missing_node(SyntaxKind::CommaToken, false);
                set_text_range_pos_width(&operator_token, invalid_element.pos(), 0);
                self.parse_error_at(
                    skip_trivia(self.source_text_as_chars(), top_bad_pos),
                    invalid_element.end(),
                    &Diagnostics::JSX_expressions_must_have_one_parent_element,
                );
                return self.finish_node(
                    self.factory.create_binary_expression(
                        self,
                        result,
                        operator_token,
                        invalid_element,
                    ),
                    pos,
                    None,
                );
            }
        }

        result
    }

    pub(super) fn parse_type_assertion(&self) -> TypeAssertion {
        unimplemented!()
    }

    pub(super) fn parse_member_expression_rest(
        &self,
        pos: isize,
        expression: Node, /*LeftHandSideExpression*/
        allow_optional_chain: bool,
    ) -> Node {
        loop {
            return expression;
        }
    }

    pub(super) fn parse_call_expression_rest(&self, pos: isize, expression: Node) -> Node {
        expression
    }

    pub(super) fn parse_type_arguments_in_expression(&self) -> Option<NodeArray /*<TypeNode>*/> {
        unimplemented!()
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

    pub(super) fn parse_argument_or_array_literal_element(&self) -> Rc<Node> {
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
                    expression,
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
