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
    HasArena, InArena,
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
                .ref_(self).create_property_access_expression_raw(
                    expression.alloc(self.arena()),
                    self.parse_right_side_of_dot(true, true).alloc(self.arena()),
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
        opening_tag: Option<&Node /*JsxOpeningElement | JsxOpeningFragment*/>,
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
            if let Some(last_child) = last_child.filter(|last_child| {
                if last_child.ref_(self).kind() != SyntaxKind::JsxElement {
                    return false;
                }
                let last_child_ref = last_child.ref_(self);
                let last_child_as_jsx_element = last_child_ref.as_jsx_element();
                !tag_names_are_equivalent(
                    last_child_as_jsx_element
                        .opening_element
                        .ref_(self).as_jsx_opening_element()
                        .tag_name,
                    last_child_as_jsx_element
                        .closing_element
                        .ref_(self).as_jsx_closing_element()
                        .tag_name,
                    self,
                ) && tag_names_are_equivalent(
                    opening_as_jsx_opening_element.tag_name,
                    last_child_as_jsx_element
                        .closing_element
                        .ref_(self).as_jsx_closing_element()
                        .tag_name,
                    self,
                )
            }) {
                let last_child_ref = last_child.ref_(self);
                let last_child_as_jsx_element = last_child_ref.as_jsx_element();
                let end = last_child_as_jsx_element.children.end();
                let last_child_opening_element_pos =
                    last_child_as_jsx_element.opening_element.ref_(self).pos();
                let new_last = self.finish_node(
                    self.factory().ref_(self).create_jsx_element_raw(
                        last_child_as_jsx_element.opening_element.clone(),
                        last_child_as_jsx_element.children.clone(),
                        self.finish_node(
                            self.factory().ref_(self).create_jsx_closing_element_raw(
                                self.finish_node(
                                    self.factory().ref_(self).create_identifier_raw(
                                        "",
                                        Option::<Gc<NodeArray>>::None,
                                        None,
                                    ),
                                    end,
                                    Some(end),
                                )
                                .alloc(self.arena()),
                            ),
                            end,
                            Some(end),
                        )
                        .alloc(self.arena()),
                    ),
                    last_child_opening_element_pos,
                    Some(end),
                );

                let children_pos = children.pos();
                let mut children_vec = children.to_vec();
                let children_len = children.len();
                children_vec[children_len - 1] = new_last.alloc(self.arena());
                children = self.create_node_array(children_vec, children_pos, Some(end), None);
                closing_element = last_child_as_jsx_element.closing_element.clone();
            } else {
                closing_element = self
                    .parse_jsx_closing_element(&opening, in_expression_context)
                    .alloc(self.arena());
                let closing_element_ref = closing_element.ref_(self);
                let closing_element_as_jsx_closing_element = closing_element_ref.as_jsx_closing_element();
                if !tag_names_are_equivalent(
                    opening_as_jsx_opening_element.tag_name,
                    closing_element_as_jsx_closing_element.tag_name,
                    self,
                ) {
                    if opening_tag.matches(|opening_tag| {
                        is_jsx_opening_element(opening_tag)
                            && tag_names_are_equivalent(
                                closing_element_as_jsx_closing_element.tag_name,
                                opening_tag.as_jsx_opening_element().tag_name,
                                self,
                            )
                    }) {
                        self.parse_error_at_range(
                            &*opening_as_jsx_opening_element.tag_name.ref_(self),
                            &Diagnostics::JSX_element_0_has_no_corresponding_closing_tag,
                            Some(vec![get_text_of_node_from_source_text(
                                &self.source_text_as_chars(),
                                opening_as_jsx_opening_element.tag_name,
                                None,
                                self,
                            )
                            .into_owned()]),
                        );
                    } else {
                        self.parse_error_at_range(
                            &*closing_element_as_jsx_closing_element.tag_name.ref_(self),
                            &Diagnostics::Expected_corresponding_JSX_closing_tag_for_0,
                            Some(vec![get_text_of_node_from_source_text(
                                &self.source_text_as_chars(),
                                opening_as_jsx_opening_element.tag_name,
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
                    .ref_(self).create_jsx_element_raw(opening.alloc(self.arena()), children, closing_element)
                    .into(),
                pos,
                None,
            );
        } else if opening.kind() == SyntaxKind::JsxOpeningFragment {
            let children = self.parse_jsx_children(&opening);
            result = self.finish_node(
                self.factory()
                    .ref_(self).create_jsx_fragment_raw(
                        opening.alloc(self.arena()),
                        children,
                        self.parse_jsx_closing_fragment(in_expression_context)
                            .alloc(self.arena()),
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
                        .ref_(self).create_binary_expression_raw(
                            result.alloc(self.arena()),
                            operator_token.alloc(self.arena()),
                            invalid_element.alloc(self.arena()),
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
        let node = self.factory().ref_(self).create_jsx_text_raw(
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
        opening_tag: &Node, /*JsxOpeningElement | JsxOpeningFragment*/
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
                    let tag = opening_tag_as_jsx_opening_element.tag_name;
                    let start =
                        skip_trivia(&self.source_text_as_chars(), tag.ref_(self).pos(), None, None, None);
                    self.parse_error_at(
                        start,
                        tag.ref_(self).end(),
                        &Diagnostics::JSX_element_0_has_no_corresponding_closing_tag,
                        Some(vec![get_text_of_node_from_source_text(
                            &self.source_text_as_chars(),
                            opening_tag_as_jsx_opening_element.tag_name,
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
        opening_tag: &Node, /*JsxOpeningElement | JsxOpeningFragment*/
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
            let Some(child) = self.parse_jsx_child(opening_tag, self.current_token()) else {
                break;
            };
            let child = child.alloc(self.arena());
            list.push(child);
            if is_jsx_opening_element(opening_tag) && child.ref_(self).kind() == SyntaxKind::JsxElement {
                let opening_tag_as_jsx_opening_element = opening_tag.as_jsx_opening_element();
                let child_ref = child.ref_(self);
                let child_as_jsx_element = child_ref.as_jsx_element();
                if !tag_names_are_equivalent(
                    child_as_jsx_element
                        .opening_element
                        .ref_(self).as_jsx_opening_element()
                        .tag_name,
                    child_as_jsx_element
                        .closing_element
                        .ref_(self).as_jsx_closing_element()
                        .tag_name,
                    self,
                ) && tag_names_are_equivalent(
                    opening_tag_as_jsx_opening_element.tag_name,
                    child_as_jsx_element
                        .closing_element
                        .ref_(self).as_jsx_closing_element()
                        .tag_name,
                    self,
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
            self.factory().ref_(self).create_jsx_attributes_raw(
                self.parse_list(ParsingContext::JsxAttributes, &mut || {
                    self.parse_jsx_attribute().alloc(self.arena())
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
                self.factory().ref_(self).create_jsx_opening_fragment_raw().into(),
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
                .ref_(self).create_jsx_opening_element_raw(tag_name.alloc(self.arena()), type_arguments, attributes.alloc(self.arena()))
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
                .ref_(self).create_jsx_self_closing_element_raw(
                    tag_name.alloc(self.arena()),
                    type_arguments,
                    attributes.alloc(self.arena()),
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
                    .ref_(self).create_property_access_expression_raw(
                        expression.alloc(self.arena()),
                        self.parse_right_side_of_dot(true, false).alloc(self.arena()),
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
            self.factory().ref_(self).create_jsx_expression_raw(
                dot_dot_dot_token.map(|dot_dot_dot_token| dot_dot_dot_token.alloc(self.arena())),
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
                .ref_(self).create_jsx_attribute_raw(
                    self.parse_identifier_name(None).alloc(self.arena()),
                    if self.token() != SyntaxKind::EqualsToken {
                        None
                    } else if self.scan_jsx_attribute_value() == SyntaxKind::StringLiteral {
                        Some(self.parse_literal_node().alloc(self.arena()))
                    } else {
                        self.parse_jsx_expression(true)
                            .map(|jsx_expression| jsx_expression.alloc(self.arena()))
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
            self.factory().ref_(self).create_jsx_spread_attribute_raw(expression),
            pos,
            None,
        )
    }

    pub(super) fn parse_jsx_closing_element(
        &self,
        open: &Node, /*JsxOpeningElement*/
        in_expression_context: bool,
    ) -> JsxClosingElement {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::LessThanSlashToken, None, None);
        let tag_name = self.parse_jsx_element_name().alloc(self.arena());
        let open_as_jsx_opening_element = open.as_jsx_opening_element();
        if self.parse_expected(SyntaxKind::GreaterThanToken, None, Some(false)) {
            if in_expression_context
                || !tag_names_are_equivalent(open_as_jsx_opening_element.tag_name, tag_name, self)
            {
                self.next_token();
            } else {
                self.scan_jsx_text();
            }
        }
        self.finish_node(
            self.factory()
                .ref_(self).create_jsx_closing_element_raw(tag_name),
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
            self.factory().ref_(self).create_jsx_jsx_closing_fragment_raw(),
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
            self.factory().ref_(self).create_type_assertion_raw(type_, expression),
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
        if node.ref_(self).flags().intersects(NodeFlags::OptionalChain) {
            return true;
        }
        if is_non_null_expression(&node.ref_(self)) {
            let mut expr = node.ref_(self).as_non_null_expression().expression;
            while is_non_null_expression(&expr.ref_(self)) && !expr.ref_(self).flags().intersects(NodeFlags::OptionalChain)
            {
                expr = expr.ref_(self).as_non_null_expression().expression;
            }
            if expr.ref_(self).flags().intersects(NodeFlags::OptionalChain) {
                while is_non_null_expression(&node.ref_(self)) {
                    node.ref_(self).set_flags(node.ref_(self).flags() | NodeFlags::OptionalChain);
                    node = node.ref_(self).as_non_null_expression().expression;
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
            question_dot_token.is_some() || self.try_reparse_optional_chain(expression);
        let property_access = if is_optional_chain {
            self.factory().ref_(self).create_property_access_chain_raw(
                expression,
                question_dot_token,
                name.alloc(self.arena()),
            )
        } else {
            self.factory()
                .ref_(self).create_property_access_expression_raw(expression, name.alloc(self.arena()))
        };
        if is_optional_chain && is_private_identifier(&property_access.name.ref_(self)) {
            self.parse_error_at_range(
                &*property_access.name.ref_(self),
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
                .alloc(self.arena());
        } else {
            let argument = self.allow_in_and(|| self.parse_expression());
            if is_string_or_numeric_literal_like(&argument.ref_(self)) {
                let argument_ref = argument.ref_(self);
                let argument_as_literal_like_node = argument_ref.as_literal_like_node();
                let text = self.intern_identifier(&argument_as_literal_like_node.text());
                argument_as_literal_like_node.set_text(text);
            }
            argument_expression = argument;
        }

        self.parse_expected(SyntaxKind::CloseBracketToken, None, None);

        let indexed_access =
            if question_dot_token.is_some() || self.try_reparse_optional_chain(expression) {
                self.factory().ref_(self).create_element_access_chain_raw(
                    expression,
                    question_dot_token,
                    argument_expression,
                )
            } else {
                self.factory()
                    .ref_(self).create_element_access_expression_raw(expression, argument_expression)
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
                        question_dot_token.map(|question_dot_token| question_dot_token.alloc(self.arena())),
                    )
                    .alloc(self.arena());
                continue;
            }

            if question_dot_token.is_none()
                && self.token() == SyntaxKind::ExclamationToken
                && !self.scanner().has_preceding_line_break()
            {
                self.next_token();
                expression = self
                    .finish_node(
                        self.factory().ref_(self).create_non_null_expression_raw(expression),
                        pos,
                        None,
                    )
                    .alloc(self.arena());
                continue;
            }

            if (question_dot_token.is_some() || !self.in_decorator_context())
                && self.parse_optional(SyntaxKind::OpenBracketToken)
            {
                expression = self
                    .parse_element_access_expression_rest(
                        pos,
                        expression,
                        question_dot_token.map(|question_dot_token| question_dot_token.alloc(self.arena())),
                    )
                    .alloc(self.arena());
                continue;
            }

            if self.is_template_start_of_tagged_template() {
                expression = self.parse_tagged_template_rest(
                    pos,
                    expression,
                    question_dot_token.map(|question_dot_token| question_dot_token.alloc(self.arena())),
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
        let mut tag_expression = self.factory().ref_(self).create_tagged_template_expression_raw(
            tag.clone(),
            type_arguments,
            if self.token() == SyntaxKind::NoSubstitutionTemplateLiteral {
                self.re_scan_template_head_or_no_substitution_template();
                self.parse_literal_node().alloc(self.arena())
            } else {
                self.parse_template_expression(true).alloc(self.arena())
            },
        );
        if question_dot_token.is_some() || tag.ref_(self).flags().intersects(NodeFlags::OptionalChain) {
            tag_expression.set_flags(tag_expression.flags() | NodeFlags::OptionalChain);
        }
        tag_expression.question_dot_token = question_dot_token;
        self.finish_node(tag_expression, pos, None).alloc(self.arena())
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
                            question_dot_token.map(|question_dot_token| question_dot_token.alloc(self.arena())),
                            Some(type_arguments),
                        );
                        continue;
                    }

                    let argument_list = self.parse_argument_list();
                    let call_expr = if question_dot_token.is_some()
                        || self.try_reparse_optional_chain(expression)
                    {
                        self.factory().ref_(self).create_call_chain_raw(
                            expression,
                            question_dot_token.map(|question_dot_token| question_dot_token.alloc(self.arena())),
                            Some(type_arguments),
                            Some(argument_list),
                        )
                    } else {
                        self.factory().ref_(self).create_call_expression_raw(
                            expression,
                            Some(type_arguments),
                            Some(argument_list),
                        )
                    };
                    expression = self.finish_node(call_expr, pos, None).alloc(self.arena());
                    continue;
                }
            } else if self.token() == SyntaxKind::OpenParenToken {
                let argument_list = self.parse_argument_list();
                let call_expr = if question_dot_token.is_some()
                    || self.try_reparse_optional_chain(expression)
                {
                    self.factory().ref_(self).create_call_chain_raw(
                        expression,
                        question_dot_token.map(|question_dot_token| question_dot_token.alloc(self.arena())),
                        Option::<Gc<NodeArray>>::None,
                        Some(argument_list),
                    )
                } else {
                    self.factory().ref_(self).create_call_expression_raw(
                        expression,
                        Option::<Gc<NodeArray>>::None,
                        Some(argument_list),
                    )
                };
                expression = self.finish_node(call_expr, pos, None).alloc(self.arena());
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
                        self.factory().ref_(self).create_property_access_chain_raw(
                            expression,
                            Some(question_dot_token.alloc(self.arena())),
                            name.alloc(self.arena()),
                        ),
                        pos,
                        None,
                    )
                    .alloc(self.arena());
            }
            break;
        }
        expression
    }
}
