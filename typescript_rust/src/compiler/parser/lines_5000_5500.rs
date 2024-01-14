use gc::Gc;
use id_arena::Id;

use super::{ParserType, ParsingContext};
use crate::{
    get_text_of_node_from_source_text, is_jsx_opening_element, is_jsx_opening_fragment,
    is_non_null_expression, is_private_identifier, is_string_or_numeric_literal_like,
    set_text_range_pos_width, skip_trivia, tag_names_are_equivalent,
    token_is_identifier_or_keyword, Debug_, Diagnostics, ElementAccessExpression, JsxAttributes,
    JsxClosingElement, JsxClosingFragment, JsxExpression, JsxSpreadAttribute, JsxText, Matches,
    Node, NodeArray, NodeFlags, NodeInterface, PropertyAccessExpression, ReadonlyTextRange,
    SyntaxKind, TypeAssertion,
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
            self.factory()
                .create_property_access_expression_raw(
                    expression.wrap(),
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
        opening_tag: Option<Id<Node> /*JsxOpeningElement | JsxOpeningFragment*/>,
    ) -> Node /*JsxElement | JsxSelfClosingElement | JsxFragment*/ {
        let pos = self.get_node_pos();
        let opening = self
            .parse_jsx_opening_or_self_closing_element_or_opening_fragment(in_expression_context);
        let result: Node;
        if opening.kind() == SyntaxKind::JsxOpeningElement {
            let mut children = self.parse_jsx_children(&opening);
            let closing_element: Id<Node /*JsxClosingElement*/>;

            let opening_as_jsx_opening_element = opening.as_jsx_opening_element();
            let last_child = if children.is_empty() {
                None
            } else {
                Some(children[children.len() - 1].clone())
            };
            if let Some(last_child) = last_child.as_ref().filter(|last_child| {
                if last_child.kind() != SyntaxKind::JsxElement {
                    return false;
                }
                let last_child_as_jsx_element = last_child.as_jsx_element();
                !tag_names_are_equivalent(
                    &last_child_as_jsx_element
                        .opening_element
                        .as_jsx_opening_element()
                        .tag_name,
                    &last_child_as_jsx_element
                        .closing_element
                        .as_jsx_closing_element()
                        .tag_name,
                ) && tag_names_are_equivalent(
                    &opening_as_jsx_opening_element.tag_name,
                    &last_child_as_jsx_element
                        .closing_element
                        .as_jsx_closing_element()
                        .tag_name,
                )
            }) {
                let last_child_as_jsx_element = last_child.as_jsx_element();
                let end = last_child_as_jsx_element.children.end();
                let last_child_opening_element_pos =
                    last_child_as_jsx_element.opening_element.pos();
                let new_last = self.finish_node(
                    self.factory().create_jsx_element_raw(
                        last_child_as_jsx_element.opening_element.clone(),
                        last_child_as_jsx_element.children.clone(),
                        self.finish_node(
                            self.factory().create_jsx_closing_element_raw(
                                self.finish_node(
                                    self.factory().create_identifier_raw(
                                        "",
                                        Option::<Gc<NodeArray>>::None,
                                        None,
                                    ),
                                    end,
                                    Some(end),
                                )
                                .wrap(),
                            ),
                            end,
                            Some(end),
                        )
                        .wrap(),
                    ),
                    last_child_opening_element_pos,
                    Some(end),
                );

                let children_pos = children.pos();
                let mut children_vec = children.to_vec();
                let children_len = children.len();
                children_vec[children_len - 1] = new_last.wrap();
                children = self.create_node_array(children_vec, children_pos, Some(end), None);
                closing_element = last_child_as_jsx_element.closing_element.clone();
            } else {
                closing_element = self
                    .parse_jsx_closing_element(&opening, in_expression_context)
                    .wrap();
                let closing_element_as_jsx_closing_element =
                    closing_element.as_jsx_closing_element();
                if !tag_names_are_equivalent(
                    &opening_as_jsx_opening_element.tag_name,
                    &closing_element_as_jsx_closing_element.tag_name,
                ) {
                    if opening_tag.matches(|opening_tag| {
                        is_jsx_opening_element(opening_tag)
                            && tag_names_are_equivalent(
                                &closing_element_as_jsx_closing_element.tag_name,
                                &opening_tag.as_jsx_opening_element().tag_name,
                            )
                    }) {
                        self.parse_error_at_range(
                            &*opening_as_jsx_opening_element.tag_name,
                            &Diagnostics::JSX_element_0_has_no_corresponding_closing_tag,
                            Some(vec![get_text_of_node_from_source_text(
                                &self.source_text_as_chars(),
                                &opening_as_jsx_opening_element.tag_name,
                                None,
                                self,
                            )
                            .into_owned()]),
                        );
                    } else {
                        self.parse_error_at_range(
                            &*closing_element_as_jsx_closing_element.tag_name,
                            &Diagnostics::Expected_corresponding_JSX_closing_tag_for_0,
                            Some(vec![get_text_of_node_from_source_text(
                                &self.source_text_as_chars(),
                                &opening_as_jsx_opening_element.tag_name,
                                None,
                                self,
                            )
                            .into_owned()]),
                        );
                    }
                }
            }
            result = self.finish_node(
                self.factory()
                    .create_jsx_element_raw(opening.wrap(), children, closing_element)
                    .into(),
                pos,
                None,
            );
        } else if opening.kind() == SyntaxKind::JsxOpeningFragment {
            let children = self.parse_jsx_children(&opening);
            result = self.finish_node(
                self.factory()
                    .create_jsx_fragment_raw(
                        opening.wrap(),
                        children,
                        self.parse_jsx_closing_fragment(in_expression_context)
                            .wrap(),
                    )
                    .into(),
                pos,
                None,
            );
        } else {
            Debug_.assert(opening.kind() == SyntaxKind::JsxSelfClosingElement, None);
            result = opening;
        }

        if in_expression_context && self.token() == SyntaxKind::LessThanToken {
            let top_bad_pos = top_invalid_node_position.unwrap_or_else(|| result.pos());
            let invalid_element = self.try_parse(|| {
                Some(self.parse_jsx_element_or_self_closing_element_or_fragment(
                    true,
                    Some(top_bad_pos),
                    None,
                ))
            });
            if let Some(invalid_element) = invalid_element {
                let operator_token =
                    self.create_missing_node(SyntaxKind::CommaToken, false, None, None);
                set_text_range_pos_width(&operator_token, invalid_element.pos(), 0);
                self.parse_error_at(
                    skip_trivia(&self.source_text_as_chars(), top_bad_pos, None, None, None),
                    invalid_element.end(),
                    &Diagnostics::JSX_expressions_must_have_one_parent_element,
                    None,
                );
                return self.finish_node(
                    self.factory()
                        .create_binary_expression_raw(
                            result.wrap(),
                            operator_token.wrap(),
                            invalid_element.wrap(),
                        )
                        .into(),
                    pos,
                    None,
                );
            }
        }

        result
    }

    pub(super) fn parse_jsx_text(&self) -> JsxText {
        let pos = self.get_node_pos();
        let node = self.factory().create_jsx_text_raw(
            self.scanner().get_token_value().clone(),
            Some(self.current_token() == SyntaxKind::JsxTextAllWhiteSpaces),
        );
        self.set_current_token(self.scanner().scan_jsx_token(
            Some(&|message, length| self.scan_error(message, length)),
            None,
        ));
        self.finish_node(node, pos, None)
    }

    pub(super) fn parse_jsx_child(
        &self,
        opening_tag: Id<Node>, /*JsxOpeningElement | JsxOpeningFragment*/
        token: SyntaxKind,     /*JsxTokenSyntaxKind*/
    ) -> Option<Node /*JsxChild*/> {
        match token {
            SyntaxKind::EndOfFileToken => {
                if is_jsx_opening_fragment(opening_tag) {
                    self.parse_error_at_range(
                        opening_tag,
                        &Diagnostics::JSX_fragment_has_no_corresponding_closing_tag,
                        None,
                    );
                } else {
                    let opening_tag_as_jsx_opening_element = opening_tag.as_jsx_opening_element();
                    let tag = &opening_tag_as_jsx_opening_element.tag_name;
                    let start =
                        skip_trivia(&self.source_text_as_chars(), tag.pos(), None, None, None);
                    self.parse_error_at(
                        start,
                        tag.end(),
                        &Diagnostics::JSX_element_0_has_no_corresponding_closing_tag,
                        Some(vec![get_text_of_node_from_source_text(
                            &self.source_text_as_chars(),
                            &opening_tag_as_jsx_opening_element.tag_name,
                            None,
                            self,
                        )
                        .into_owned()]),
                    );
                }
                None
            }
            SyntaxKind::LessThanSlashToken | SyntaxKind::ConflictMarkerTrivia => None,
            SyntaxKind::JsxText | SyntaxKind::JsxTextAllWhiteSpaces => {
                Some(self.parse_jsx_text().into())
            }
            SyntaxKind::OpenBraceToken => self.parse_jsx_expression(false).map(Into::into),
            SyntaxKind::LessThanToken => {
                Some(self.parse_jsx_element_or_self_closing_element_or_fragment(
                    false,
                    None,
                    Some(opening_tag),
                ))
            }
            _ => Debug_.assert_never(token, None),
        }
    }

    pub(super) fn parse_jsx_children(
        &self,
        opening_tag: Id<Node>, /*JsxOpeningElement | JsxOpeningFragment*/
    ) -> Gc<NodeArray> /*<JsxChild>*/ {
        let mut list: Vec<Id<Node>> = vec![];
        let list_pos = self.get_node_pos();
        let save_parsing_context = self.parsing_context();
        self.set_parsing_context(self.parsing_context() | ParsingContext::JsxChildren);

        loop {
            self.set_current_token(self.scanner().re_scan_jsx_token(
                Some(&|message, length| self.scan_error(message, length)),
                None,
            ));
            let child = self.parse_jsx_child(opening_tag, self.current_token());
            if child.is_none() {
                break;
            }
            let child = child.unwrap();
            let child = child.wrap();
            list.push(child.clone());
            if is_jsx_opening_element(opening_tag) && child.kind() == SyntaxKind::JsxElement {
                let opening_tag_as_jsx_opening_element = opening_tag.as_jsx_opening_element();
                let child_as_jsx_element = child.as_jsx_element();
                if !tag_names_are_equivalent(
                    &child_as_jsx_element
                        .opening_element
                        .as_jsx_opening_element()
                        .tag_name,
                    &child_as_jsx_element
                        .closing_element
                        .as_jsx_closing_element()
                        .tag_name,
                ) && tag_names_are_equivalent(
                    &opening_tag_as_jsx_opening_element.tag_name,
                    &child_as_jsx_element
                        .closing_element
                        .as_jsx_closing_element()
                        .tag_name,
                ) {
                    break;
                }
            }
        }

        self.set_parsing_context(save_parsing_context);
        self.create_node_array(list, list_pos, None, None)
    }

    pub(super) fn parse_jsx_attributes(&self) -> JsxAttributes {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory().create_jsx_attributes_raw(
                self.parse_list(ParsingContext::JsxAttributes, &mut || {
                    self.parse_jsx_attribute().wrap()
                }),
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_jsx_opening_or_self_closing_element_or_opening_fragment(
        &self,
        in_expression_context: bool,
    ) -> Node /*JsxOpeningElement | JsxSelfClosingElement | JsxOpeningFragment*/ {
        let pos = self.get_node_pos();

        self.parse_expected(SyntaxKind::LessThanToken, None, None);

        if self.token() == SyntaxKind::GreaterThanToken {
            self.scan_jsx_text();
            return self.finish_node(
                self.factory().create_jsx_opening_fragment_raw().into(),
                pos,
                None,
            );
        }
        let tag_name = self.parse_jsx_element_name();
        let type_arguments = if !self.context_flags().intersects(NodeFlags::JavaScriptFile) {
            self.try_parse_type_arguments()
        } else {
            None
        };
        let attributes = self.parse_jsx_attributes();

        let node: Node /*JsxOpeningLikeElement*/;

        if self.token() == SyntaxKind::GreaterThanToken {
            self.scan_jsx_text();
            node = self
                .factory()
                .create_jsx_opening_element_raw(tag_name.wrap(), type_arguments, attributes.wrap())
                .into();
        } else {
            self.parse_expected(SyntaxKind::SlashToken, None, None);
            if self.parse_expected(SyntaxKind::GreaterThanToken, None, Some(false)) {
                if in_expression_context {
                    self.next_token();
                } else {
                    self.scan_jsx_text();
                }
            }
            node = self
                .factory()
                .create_jsx_self_closing_element_raw(
                    tag_name.wrap(),
                    type_arguments,
                    attributes.wrap(),
                )
                .into();
        }

        self.finish_node(node, pos, None)
    }

    pub(super) fn parse_jsx_element_name(&self) -> Node /*JsxTagNameExpression*/ {
        let pos = self.get_node_pos();
        self.scan_jsx_identifier();
        let mut expression: Node = if self.token() == SyntaxKind::ThisKeyword {
            self.parse_token_node().into()
        } else {
            self.parse_identifier_name(None)
        };
        while self.parse_optional(SyntaxKind::DotToken) {
            expression = self.finish_node(
                self.factory()
                    .create_property_access_expression_raw(
                        expression.wrap(),
                        self.parse_right_side_of_dot(true, false).wrap(),
                    )
                    .into(),
                pos,
                None,
            );
        }
        expression
    }

    pub(super) fn parse_jsx_expression(
        &self,
        in_expression_context: bool,
    ) -> Option<JsxExpression> {
        let pos = self.get_node_pos();
        if !self.parse_expected(SyntaxKind::OpenBraceToken, None, None) {
            return None;
        }

        let mut dot_dot_dot_token = None;
        let mut expression = None;
        if self.token() != SyntaxKind::CloseBraceToken {
            dot_dot_dot_token = self.parse_optional_token(SyntaxKind::DotDotDotToken);
            expression = Some(self.parse_expression());
        }
        if in_expression_context {
            self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        } else {
            if self.parse_expected(SyntaxKind::CloseBraceToken, None, Some(false)) {
                self.scan_jsx_text();
            }
        }

        Some(self.finish_node(
            self.factory().create_jsx_expression_raw(
                dot_dot_dot_token.map(|dot_dot_dot_token| dot_dot_dot_token.wrap()),
                expression,
            ),
            pos,
            None,
        ))
    }

    pub(super) fn parse_jsx_attribute(&self) -> Node /*JsxAttribute | JsxSpreadAttribute*/ {
        if self.token() == SyntaxKind::OpenBraceToken {
            return self.parse_jsx_spread_attribute().into();
        }

        self.scan_jsx_identifier();
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory()
                .create_jsx_attribute_raw(
                    self.parse_identifier_name(None).wrap(),
                    if self.token() != SyntaxKind::EqualsToken {
                        None
                    } else if self.scan_jsx_attribute_value() == SyntaxKind::StringLiteral {
                        Some(self.parse_literal_node().wrap())
                    } else {
                        self.parse_jsx_expression(true)
                            .map(|jsx_expression| jsx_expression.wrap())
                    },
                )
                .into(),
            pos,
            None,
        )
    }

    pub(super) fn parse_jsx_spread_attribute(&self) -> JsxSpreadAttribute {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBraceToken, None, None);
        self.parse_expected(SyntaxKind::DotDotDotToken, None, None);
        let expression = self.parse_expression();
        self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        self.finish_node(
            self.factory().create_jsx_spread_attribute_raw(expression),
            pos,
            None,
        )
    }

    pub(super) fn parse_jsx_closing_element(
        &self,
        open: Id<Node>, /*JsxOpeningElement*/
        in_expression_context: bool,
    ) -> JsxClosingElement {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::LessThanSlashToken, None, None);
        let tag_name = self.parse_jsx_element_name();
        let open_as_jsx_opening_element = open.as_jsx_opening_element();
        if self.parse_expected(SyntaxKind::GreaterThanToken, None, Some(false)) {
            if in_expression_context
                || !tag_names_are_equivalent(&open_as_jsx_opening_element.tag_name, &tag_name)
            {
                self.next_token();
            } else {
                self.scan_jsx_text();
            }
        }
        self.finish_node(
            self.factory()
                .create_jsx_closing_element_raw(tag_name.wrap()),
            pos,
            None,
        )
    }

    pub(super) fn parse_jsx_closing_fragment(
        &self,
        in_expression_context: bool,
    ) -> JsxClosingFragment {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::LessThanSlashToken, None, None);
        if token_is_identifier_or_keyword(self.token()) {
            self.parse_error_at_range(
                &self.parse_jsx_element_name(),
                &Diagnostics::Expected_corresponding_closing_tag_for_JSX_fragment,
                None,
            );
        }
        if self.parse_expected(SyntaxKind::GreaterThanToken, None, Some(false)) {
            if in_expression_context {
                self.next_token();
            } else {
                self.scan_jsx_text();
            }
        }
        self.finish_node(
            self.factory().create_jsx_jsx_closing_fragment_raw(),
            pos,
            None,
        )
    }

    pub(super) fn parse_type_assertion(&self) -> TypeAssertion {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::LessThanToken, None, None);
        let type_ = self.parse_type();
        self.parse_expected(SyntaxKind::GreaterThanToken, None, None);
        let expression = self.parse_simple_unary_expression();
        self.finish_node(
            self.factory().create_type_assertion_raw(type_, expression),
            pos,
            None,
        )
    }

    pub(super) fn next_token_is_identifier_or_keyword_or_open_bracket_or_template(&self) -> bool {
        self.next_token();
        token_is_identifier_or_keyword(self.token())
            || self.token() == SyntaxKind::OpenBracketToken
            || self.is_template_start_of_tagged_template()
    }

    pub(super) fn is_start_of_optional_property_or_element_access_chain(&self) -> bool {
        self.token() == SyntaxKind::QuestionDotToken
            && self.look_ahead_bool(|| {
                self.next_token_is_identifier_or_keyword_or_open_bracket_or_template()
            })
    }

    pub(super) fn try_reparse_optional_chain(
        &self,
        mut node: Id<Node>, /*Expression*/
    ) -> bool {
        if node.flags().intersects(NodeFlags::OptionalChain) {
            return true;
        }
        if is_non_null_expression(node) {
            let mut expr = &*node.as_non_null_expression().expression;
            while is_non_null_expression(expr) && !expr.flags().intersects(NodeFlags::OptionalChain)
            {
                expr = &*expr.as_non_null_expression().expression;
            }
            if expr.flags().intersects(NodeFlags::OptionalChain) {
                while is_non_null_expression(node) {
                    node.set_flags(node.flags() | NodeFlags::OptionalChain);
                    node = &*node.as_non_null_expression().expression;
                }
                return true;
            }
        }
        false
    }

    pub(super) fn parse_property_access_expression_rest(
        &self,
        pos: isize,
        expression: Id<Node /*LeftHandSideExpression*/>,
        question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
    ) -> PropertyAccessExpression {
        let name = self.parse_right_side_of_dot(true, true);
        let is_optional_chain =
            question_dot_token.is_some() || self.try_reparse_optional_chain(&expression);
        let property_access = if is_optional_chain {
            self.factory().create_property_access_chain_raw(
                expression,
                question_dot_token,
                name.wrap(),
            )
        } else {
            self.factory()
                .create_property_access_expression_raw(expression, name.wrap())
        };
        if is_optional_chain && is_private_identifier(&property_access.name) {
            self.parse_error_at_range(
                &*property_access.name,
                &Diagnostics::An_optional_chain_cannot_contain_private_identifiers,
                None,
            );
        }
        self.finish_node(property_access, pos, None)
    }

    pub(super) fn parse_element_access_expression_rest(
        &self,
        pos: isize,
        expression: Id<Node /*LeftHandSideExpression*/>,
        question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
    ) -> ElementAccessExpression {
        let argument_expression: Id<Node /*Expression*/>;
        if self.token() == SyntaxKind::CloseBracketToken {
            argument_expression = self
                .create_missing_node(
                    SyntaxKind::Identifier,
                    true,
                    Some(&Diagnostics::An_element_access_expression_should_take_an_argument),
                    None,
                )
                .wrap();
        } else {
            let argument = self.allow_in_and(|| self.parse_expression());
            if is_string_or_numeric_literal_like(&argument) {
                let argument_as_literal_like_node = argument.as_literal_like_node();
                let text = self.intern_identifier(&argument_as_literal_like_node.text());
                argument_as_literal_like_node.set_text(text);
            }
            argument_expression = argument;
        }

        self.parse_expected(SyntaxKind::CloseBracketToken, None, None);

        let indexed_access =
            if question_dot_token.is_some() || self.try_reparse_optional_chain(&expression) {
                self.factory().create_element_access_chain_raw(
                    expression,
                    question_dot_token,
                    argument_expression,
                )
            } else {
                self.factory()
                    .create_element_access_expression_raw(expression, argument_expression)
            };
        self.finish_node(indexed_access, pos, None)
    }

    pub(super) fn parse_member_expression_rest(
        &self,
        pos: isize,
        mut expression: Id<Node /*LeftHandSideExpression*/>,
        allow_optional_chain: bool,
    ) -> Id<Node /*MemberExpression*/> {
        loop {
            let mut question_dot_token = None;
            let is_property_access: bool /* = false*/;
            if allow_optional_chain && self.is_start_of_optional_property_or_element_access_chain()
            {
                question_dot_token =
                    Some(self.parse_expected_token(SyntaxKind::QuestionDotToken, None, None));
                is_property_access = token_is_identifier_or_keyword(self.token());
            } else {
                is_property_access = self.parse_optional(SyntaxKind::DotToken);
            }

            if is_property_access {
                expression = self
                    .parse_property_access_expression_rest(
                        pos,
                        expression,
                        question_dot_token.map(|question_dot_token| question_dot_token.wrap()),
                    )
                    .wrap();
                continue;
            }

            if question_dot_token.is_none()
                && self.token() == SyntaxKind::ExclamationToken
                && !self.scanner().has_preceding_line_break()
            {
                self.next_token();
                expression = self
                    .finish_node(
                        self.factory().create_non_null_expression_raw(expression),
                        pos,
                        None,
                    )
                    .wrap();
                continue;
            }

            if (question_dot_token.is_some() || !self.in_decorator_context())
                && self.parse_optional(SyntaxKind::OpenBracketToken)
            {
                expression = self
                    .parse_element_access_expression_rest(
                        pos,
                        expression,
                        question_dot_token.map(|question_dot_token| question_dot_token.wrap()),
                    )
                    .wrap();
                continue;
            }

            if self.is_template_start_of_tagged_template() {
                expression = self.parse_tagged_template_rest(
                    pos,
                    expression,
                    question_dot_token.map(|question_dot_token| question_dot_token.wrap()),
                    None,
                );
                continue;
            }

            return expression;
        }
    }

    pub(super) fn is_template_start_of_tagged_template(&self) -> bool {
        matches!(
            self.token(),
            SyntaxKind::NoSubstitutionTemplateLiteral | SyntaxKind::TemplateHead
        )
    }

    pub(super) fn parse_tagged_template_rest(
        &self,
        pos: isize,
        tag: Id<Node /*LeftHandSideExpression*/>,
        question_dot_token: Option<Id<Node /*QuestionDotToken*/>>,
        type_arguments: Option<Gc<NodeArray> /*TypeNode*/>,
    ) -> Id<Node> {
        let mut tag_expression = self.factory().create_tagged_template_expression_raw(
            tag.clone(),
            type_arguments,
            if self.token() == SyntaxKind::NoSubstitutionTemplateLiteral {
                self.re_scan_template_head_or_no_substitution_template();
                self.parse_literal_node().wrap()
            } else {
                self.parse_template_expression(true).wrap()
            },
        );
        if question_dot_token.is_some() || tag.flags().intersects(NodeFlags::OptionalChain) {
            tag_expression.set_flags(tag_expression.flags() | NodeFlags::OptionalChain);
        }
        tag_expression.question_dot_token = question_dot_token;
        self.finish_node(tag_expression, pos, None).wrap()
    }

    pub(super) fn parse_call_expression_rest(
        &self,
        pos: isize,
        mut expression: Id<Node>,
    ) -> Id<Node> {
        loop {
            expression = self.parse_member_expression_rest(pos, expression, true);
            let question_dot_token = self.parse_optional_token(SyntaxKind::QuestionDotToken);
            if !self.context_flags().intersects(NodeFlags::JavaScriptFile)
                && matches!(
                    self.token(),
                    SyntaxKind::LessThanToken | SyntaxKind::LessThanLessThanToken
                )
            {
                let type_arguments = self.try_parse(|| self.parse_type_arguments_in_expression());
                if let Some(type_arguments) = type_arguments {
                    if self.is_template_start_of_tagged_template() {
                        expression = self.parse_tagged_template_rest(
                            pos,
                            expression,
                            question_dot_token.map(|question_dot_token| question_dot_token.wrap()),
                            Some(type_arguments),
                        );
                        continue;
                    }

                    let argument_list = self.parse_argument_list();
                    let call_expr = if question_dot_token.is_some()
                        || self.try_reparse_optional_chain(&expression)
                    {
                        self.factory().create_call_chain_raw(
                            expression,
                            question_dot_token.map(|question_dot_token| question_dot_token.wrap()),
                            Some(type_arguments),
                            Some(argument_list),
                        )
                    } else {
                        self.factory().create_call_expression_raw(
                            expression,
                            Some(type_arguments),
                            Some(argument_list),
                        )
                    };
                    expression = self.finish_node(call_expr, pos, None).wrap();
                    continue;
                }
            } else if self.token() == SyntaxKind::OpenParenToken {
                let argument_list = self.parse_argument_list();
                let call_expr = if question_dot_token.is_some()
                    || self.try_reparse_optional_chain(&expression)
                {
                    self.factory().create_call_chain_raw(
                        expression,
                        question_dot_token.map(|question_dot_token| question_dot_token.wrap()),
                        Option::<Gc<NodeArray>>::None,
                        Some(argument_list),
                    )
                } else {
                    self.factory().create_call_expression_raw(
                        expression,
                        Option::<Gc<NodeArray>>::None,
                        Some(argument_list),
                    )
                };
                expression = self.finish_node(call_expr, pos, None).wrap();
                continue;
            }
            if let Some(question_dot_token) = question_dot_token {
                let name = self.create_missing_node(
                    SyntaxKind::Identifier,
                    false,
                    Some(&Diagnostics::Identifier_expected),
                    None,
                );
                expression = self
                    .finish_node(
                        self.factory().create_property_access_chain_raw(
                            expression,
                            Some(question_dot_token.wrap()),
                            name.wrap(),
                        ),
                        pos,
                        None,
                    )
                    .wrap();
            }
            break;
        }
        expression
    }
}
