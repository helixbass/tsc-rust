#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::{ParserType, ParsingContext, SignatureFlags};
use crate::{
    get_full_width, is_modifier_kind, some, token_to_string, Diagnostics, Identifier,
    KeywordTypeNode, LiteralTypeNode, Node, NodeArray, NodeFactory, ParameterDeclaration,
    SyntaxKind, TypeParameterDeclaration, TypeQueryNode,
};

impl ParserType {
    pub(super) fn parse_jsdoc_function_type(&self) -> Node {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        if self.look_ahead_bool(|| self.next_token_is_open_paren()) {
            self.next_token();
            let parameters = self.parse_parameters(SignatureFlags::Type | SignatureFlags::JSDoc);
            let type_ = self.parse_return_type(SyntaxKind::ColonToken, false);
            return self.with_jsdoc(
                self.finish_node(
                    self.factory
                        .create_jsdoc_function_type(
                            self,
                            parameters,
                            type_.map(|type_| type_.wrap()),
                        )
                        .into(),
                    pos,
                    None,
                ),
                has_jsdoc,
            );
        }
        self.finish_node(
            self.factory
                .create_type_reference_node(
                    self,
                    self.parse_identifier_name(None).wrap(),
                    Option::<NodeArray>::None,
                )
                .into(),
            pos,
            None,
        )
    }

    pub(super) fn parse_jsdoc_parameter(&self) -> ParameterDeclaration {
        let pos = self.get_node_pos();
        let mut name: Option<Node> = None;
        if matches!(
            self.token(),
            SyntaxKind::ThisKeyword | SyntaxKind::NewKeyword
        ) {
            name = Some(self.parse_identifier_name(None));
            self.parse_expected(SyntaxKind::ColonToken, None, None);
        }
        self.finish_node(
            self.factory.create_parameter_declaration(
                self,
                Option::<NodeArray>::None,
                Option::<NodeArray>::None,
                None,
                name.map(|name| name.wrap()),
                None,
                Some(self.parse_jsdoc_type().wrap()),
                None,
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_jsdoc_type(&self) -> Node {
        self.scanner_mut().set_in_jsdoc_type(true);
        let pos = self.get_node_pos();
        if self.parse_optional(SyntaxKind::ModuleKeyword) {
            let module_tag = self.factory.create_jsdoc_namepath_type(self, None);
            loop {
                match self.token() {
                    SyntaxKind::CloseBraceToken
                    | SyntaxKind::EndOfFileToken
                    | SyntaxKind::CommaToken
                    | SyntaxKind::WhitespaceTrivia => {
                        break;
                    }
                    _ => {
                        self.next_token_jsdoc();
                    }
                }
            }

            self.scanner_mut().set_in_jsdoc_type(false);
            return self.finish_node(module_tag.into(), pos, None);
        }

        let has_dot_dot_dot = self.parse_optional(SyntaxKind::DotDotDotToken);
        let mut type_ = self.parse_type_or_type_predicate();
        self.scanner_mut().set_in_jsdoc_type(false);
        if has_dot_dot_dot {
            type_ = self.finish_node(
                self.factory
                    .create_jsdoc_variadic_type(self, Some(type_.wrap()))
                    .into(),
                pos,
                None,
            );
        }
        if self.token() == SyntaxKind::EqualsToken {
            self.next_token();
            return self.finish_node(
                self.factory
                    .create_jsdoc_optional_type(self, Some(type_.wrap()))
                    .into(),
                pos,
                None,
            );
        }
        type_
    }

    pub(super) fn parse_type_query(&self) -> TypeQueryNode {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::TypeOfKeyword, None, None);
        self.finish_node(
            self.factory
                .create_type_query_node(self, self.parse_entity_name(true, None).wrap()),
            pos,
            None,
        )
    }

    pub(super) fn parse_type_parameter(&self) -> TypeParameterDeclaration {
        let pos = self.get_node_pos();
        let name = self.parse_identifier(None, None);
        let mut constraint: Option<Node> = None;
        let mut expression: Option<Node> = None;
        if self.parse_optional(SyntaxKind::ExtendsKeyword) {
            if self.is_start_of_type(None) || !self.is_start_of_expression() {
                constraint = Some(self.parse_type());
            } else {
                expression = Some(self.parse_unary_expression_or_higher());
            }
        }

        let default_type = if self.parse_optional(SyntaxKind::EqualsToken) {
            Some(self.parse_type())
        } else {
            None
        };
        let mut node = self.factory.create_type_parameter_declaration(
            self,
            name.wrap(),
            constraint.map(|constraint| constraint.wrap()),
            default_type.map(|default_type| default_type.wrap()),
        );
        node.expression = expression.map(|expression| expression.wrap());
        self.finish_node(node, pos, None)
    }

    pub(super) fn parse_type_parameters(&self) -> Option<NodeArray /*<TypeParameterDeclaration>*/> {
        if self.token() == SyntaxKind::LessThanToken {
            return Some(self.parse_bracketed_list(
                ParsingContext::TypeParameters,
                || self.parse_type_parameter().into(),
                SyntaxKind::LessThanToken,
                SyntaxKind::GreaterThanToken,
            ));
        }
        None
    }

    pub(super) fn is_start_of_parameter(&self, is_jsdoc_parameter: bool) -> bool {
        self.token() == SyntaxKind::DotDotDotToken
            || self.is_binding_identifier_or_private_identifier_or_pattern()
            || is_modifier_kind(self.token())
            || self.token() == SyntaxKind::AtToken
            || self.is_start_of_type(Some(!is_jsdoc_parameter))
    }

    pub(super) fn parse_name_of_parameter(&self, modifiers: Option<&NodeArray>) -> Rc<Node> {
        let name = self.parse_identifier_or_pattern(Some(
            &Diagnostics::Private_identifiers_cannot_be_used_as_parameters,
        ));
        if get_full_width(&name) == 0
            && !some(
                modifiers.as_ref().map(|modifiers| {
                    let modifiers: &[Rc<Node>] = modifiers;
                    modifiers
                }),
                Option::<fn(&Rc<Node>) -> bool>::None,
            )
            && is_modifier_kind(self.token())
        {
            self.next_token();
        }
        name
    }

    pub(super) fn parse_parameter_in_outer_await_context(&self) -> ParameterDeclaration {
        self.parse_parameter_worker(true)
    }

    pub(super) fn parse_parameter(&self) -> ParameterDeclaration {
        self.parse_parameter_worker(false)
    }

    pub(super) fn parse_parameter_worker(
        &self,
        in_outer_await_context: bool,
    ) -> ParameterDeclaration {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();

        let decorators = if in_outer_await_context {
            self.do_in_await_context(|| self.parse_decorators())
        } else {
            self.parse_decorators()
        };

        if self.token() == SyntaxKind::ThisKeyword {
            let node = self.factory.create_parameter_declaration(
                self,
                decorators.clone(),
                Option::<NodeArray>::None,
                None,
                Some(self.create_identifier(true, None, None).wrap()),
                None,
                self.parse_type_annotation()
                    .map(|type_annotation| type_annotation.wrap()),
                None,
            );

            if let Some(decorators) = decorators {
                self.parse_error_at_range(
                    &*decorators[0],
                    &Diagnostics::Decorators_may_not_be_applied_to_this_parameters,
                    None,
                );
            }

            return self.with_jsdoc(self.finish_node(node, pos, None), has_jsdoc);
        }

        let saved_top_level = self.top_level();
        self.set_top_level(false);
        let modifiers = self.parse_modifiers(None, None);
        let dot_dot_dot_token = self.parse_optional_token(SyntaxKind::DotDotDotToken);
        let name = self.parse_name_of_parameter(modifiers.as_ref());
        let question_token = self.parse_optional_token(SyntaxKind::QuestionToken);
        let type_annotation = self.parse_type_annotation();
        let initializer = self.parse_initializer();
        let node = self.with_jsdoc(
            self.finish_node(
                self.factory.create_parameter_declaration(
                    self,
                    decorators,
                    modifiers,
                    dot_dot_dot_token.map(Into::into),
                    Some(name),
                    question_token.map(Into::into),
                    type_annotation.map(|type_annotation| type_annotation.wrap()),
                    initializer.map(Into::into),
                ),
                pos,
                None,
            ),
            has_jsdoc,
        );
        self.set_top_level(saved_top_level);
        node
    }

    pub(super) fn parse_return_type(
        &self,
        return_token: SyntaxKind,
        is_type: bool,
    ) -> Option<Node> {
        if self.should_parse_return_type(return_token, is_type) {
            return Some(self.parse_type_or_type_predicate());
        }
        None
    }

    pub(super) fn should_parse_return_type(&self, return_token: SyntaxKind, is_type: bool) -> bool {
        if return_token == SyntaxKind::EqualsGreaterThanToken {
            self.parse_expected(return_token, None, None);
            return true;
        } else if self.parse_optional(SyntaxKind::ColonToken) {
            return true;
        } else if is_type && self.token() == SyntaxKind::EqualsGreaterThanToken {
            self.parse_error_at_current_token(
                &Diagnostics::_0_expected,
                Some(vec![token_to_string(SyntaxKind::ColonToken)
                    .unwrap()
                    .to_string()]),
            );
            self.next_token();
            return true;
        }
        false
    }

    pub(super) fn parse_parameters_worker(&self, flags: SignatureFlags) -> NodeArray /*<ParameterDeclaration>*/
    {
        let saved_yield_context = self.in_yield_context();
        let saved_await_context = self.in_await_context();

        self.set_yield_context(flags.intersects(SignatureFlags::Yield));
        self.set_await_context(flags.intersects(SignatureFlags::Await));

        let parameters = if false {
            unimplemented!()
        } else {
            self.parse_delimited_list(
                ParsingContext::Parameters,
                || {
                    if saved_await_context {
                        self.parse_parameter_in_outer_await_context().into()
                    } else {
                        self.parse_parameter().into()
                    }
                },
                None,
            )
        };

        self.set_yield_context(saved_yield_context);
        self.set_await_context(saved_await_context);

        parameters
    }

    pub(super) fn parse_parameters(&self, flags: SignatureFlags) -> NodeArray /*<ParameterDeclaration>*/
    {
        if !self.parse_expected(SyntaxKind::OpenParenToken, None, None) {
            return self.create_missing_list();
        }

        let parameters = self.parse_parameters_worker(flags);
        self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        parameters
    }

    pub(super) fn parse_type_member_semicolon(&self) {
        if self.parse_optional(SyntaxKind::CommaToken) {
            return;
        }

        self.parse_semicolon();
    }

    pub(super) fn parse_property_or_method_signature(
        &self,
        pos: isize,
        modifiers: Option<NodeArray>,
    ) -> Node {
        let name = self.parse_property_name();
        let question_token = self.parse_optional_token(SyntaxKind::QuestionToken);
        let node: Node;
        if false {
            unimplemented!()
        } else {
            let type_ = self.parse_type_annotation();
            node = self
                .factory
                .create_property_signature(
                    self,
                    modifiers,
                    name.wrap(),
                    question_token.map(Into::into),
                    type_.map(|type_| type_.wrap()),
                )
                .into();
        }
        self.parse_type_member_semicolon();
        self.finish_node(node, pos, None)
    }

    pub(super) fn is_type_member_start(&self) -> bool {
        let mut id_token = false;

        if self.is_literal_property_name() {
            id_token = true;
            self.next_token();
        }

        if id_token {
            return self.token() == SyntaxKind::OpenParenToken
                || self.token() == SyntaxKind::LessThanToken
                || self.token() == SyntaxKind::QuestionToken
                || self.token() == SyntaxKind::ColonToken
                || self.token() == SyntaxKind::CommaToken
                || self.can_parse_semicolon();
        }
        false
    }

    pub(super) fn parse_type_member(&self) -> Node {
        let pos = self.get_node_pos();
        let modifiers = self.parse_modifiers(None, None);

        self.parse_property_or_method_signature(pos, modifiers)
    }

    pub(super) fn parse_object_type_members(&self) -> NodeArray /*<TypeElement>*/ {
        let members: NodeArray;
        if self.parse_expected(SyntaxKind::OpenBraceToken, None, None) {
            members = self.parse_list(ParsingContext::TypeMembers, &mut || {
                self.parse_type_member().wrap()
            });
            self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        } else {
            members = self.create_missing_list();
        }

        members
    }

    pub(super) fn parse_keyword_and_no_dot(&self) -> Option<Node> {
        let node = self.parse_token_node();
        if false {
            None
        } else {
            Some(Into::<KeywordTypeNode>::into(node).into())
        }
    }

    pub(super) fn parse_literal_type_node(&self, negative: Option<bool>) -> LiteralTypeNode {
        let negative = negative.unwrap_or(false);
        let pos = self.get_node_pos();
        if negative {
            self.next_token();
        }
        let expression: Node = match self.token() {
            SyntaxKind::TrueKeyword | SyntaxKind::FalseKeyword | SyntaxKind::NullKeyword => {
                self.parse_token_node().into()
            }
            _ => self.parse_literal_like_node(self.token()).into(),
        };
        if negative {
            unimplemented!()
        }
        self.finish_node(
            self.factory
                .create_literal_type_node(self, expression.into()),
            pos,
            None,
        )
    }

    pub(super) fn parse_non_array_type(&self) -> Node {
        match self.token() {
            SyntaxKind::StringKeyword
            | SyntaxKind::NumberKeyword
            | SyntaxKind::BigIntKeyword
            | SyntaxKind::BooleanKeyword => self
                .try_parse(|| self.parse_keyword_and_no_dot())
                .unwrap_or_else(|| unimplemented!()),
            SyntaxKind::StringLiteral => self.parse_literal_type_node(None).into(),
            _ => self.parse_type_reference(),
        }
    }

    pub(super) fn is_start_of_type(&self, in_start_of_parameter: Option<bool>) -> bool {
        let in_start_of_parameter = in_start_of_parameter.unwrap_or(false);
        match self.token() {
            SyntaxKind::AnyKeyword
            | SyntaxKind::UnknownKeyword
            | SyntaxKind::StringKeyword
            | SyntaxKind::NumberKeyword
            | SyntaxKind::BigIntKeyword
            | SyntaxKind::BooleanKeyword
            | SyntaxKind::ReadonlyKeyword
            | SyntaxKind::SymbolKeyword
            | SyntaxKind::UniqueKeyword
            | SyntaxKind::VoidKeyword
            | SyntaxKind::UndefinedKeyword
            | SyntaxKind::NullKeyword
            | SyntaxKind::ThisKeyword
            | SyntaxKind::TypeOfKeyword
            | SyntaxKind::NeverKeyword
            | SyntaxKind::OpenBraceToken
            | SyntaxKind::OpenBracketToken
            | SyntaxKind::LessThanToken
            | SyntaxKind::BarToken
            | SyntaxKind::AmpersandToken
            | SyntaxKind::NewKeyword
            | SyntaxKind::StringLiteral
            | SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::TrueKeyword
            | SyntaxKind::FalseKeyword
            | SyntaxKind::ObjectKeyword
            | SyntaxKind::AsteriskToken
            | SyntaxKind::QuestionToken
            | SyntaxKind::ExclamationToken
            | SyntaxKind::DotDotDotToken
            | SyntaxKind::InferKeyword
            | SyntaxKind::ImportKeyword
            | SyntaxKind::AssertsKeyword
            | SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::TemplateHead => true,
            _ => self.is_identifier(),
        }
    }

    pub(super) fn parse_postfix_type_or_higher(&self) -> Node {
        let pos = self.get_node_pos();
        let mut type_ = self.parse_non_array_type();
        while !self.scanner().has_preceding_line_break() {
            match self.token() {
                SyntaxKind::ExclamationToken => {
                    unimplemented!()
                }
                SyntaxKind::QuestionToken => {
                    unimplemented!()
                }
                SyntaxKind::OpenBracketToken => {
                    self.parse_expected(SyntaxKind::OpenBracketToken, None, None);
                    if self.is_start_of_type(None) {
                        unimplemented!()
                    } else {
                        self.parse_expected(SyntaxKind::CloseBracketToken, None, None);
                        type_ = self.finish_node(
                            self.factory
                                .create_array_type_node(self, type_.wrap())
                                .into(),
                            pos,
                            None,
                        );
                    }
                    break;
                }
                _ => {
                    return type_;
                }
            }
        }
        type_
    }

    pub(super) fn parse_type_operator_or_higher(&self) -> Node {
        // let operator = self.token();
        // match operator {

        // }
        self.parse_postfix_type_or_higher()
    }

    pub(super) fn parse_function_or_constructor_type_to_error(
        &self,
        is_in_union_type: bool,
    ) -> Option<Node> {
        None
    }

    pub(super) fn parse_union_or_intersection_type<TReturn: Into<Node>>(
        &self,
        operator: SyntaxKind, /*SyntaxKind.BarToken | SyntaxKind.AmpersandToken*/
        parse_constituent_type: fn(&ParserType) -> Node,
        create_type_node: fn(&NodeFactory<ParserType>, &ParserType, NodeArray) -> TReturn,
    ) -> Node {
        let pos = self.get_node_pos();
        let is_union_type = operator == SyntaxKind::BarToken;
        let has_leading_operator = self.parse_optional(operator);
        let mut type_: Option<Node> = Some(if has_leading_operator {
            self.parse_function_or_constructor_type_to_error(is_union_type)
                .unwrap_or_else(|| parse_constituent_type(self))
        } else {
            parse_constituent_type(self)
        });
        if self.token() == operator || has_leading_operator {
            let mut types: Vec<Rc<Node>> = vec![type_.take().unwrap().wrap()];
            while self.parse_optional(operator) {
                types.push(
                    self.parse_function_or_constructor_type_to_error(is_union_type)
                        .unwrap_or_else(|| parse_constituent_type(self))
                        .into(),
                );
            }
            type_ = Some(
                self.finish_node(
                    create_type_node(
                        &self.factory,
                        self,
                        self.create_node_array(types, pos, None, None),
                    )
                    .into(),
                    pos,
                    None,
                ),
            );
        }
        type_.unwrap()
    }

    pub(super) fn parse_intersection_type_or_higher(&self) -> Node {
        self.parse_union_or_intersection_type(
            SyntaxKind::AmpersandToken,
            ParserType::parse_type_operator_or_higher,
            NodeFactory::create_intersection_type_node,
        )
    }

    pub(super) fn parse_union_type_or_higher(&self) -> Node {
        self.parse_union_or_intersection_type(
            SyntaxKind::BarToken,
            ParserType::parse_intersection_type_or_higher,
            NodeFactory::create_union_type_node,
        )
    }

    pub(super) fn parse_type_or_type_predicate(&self) -> Node {
        let pos = self.get_node_pos();
        let type_predicate_variable = if self.is_identifier() {
            self.try_parse(|| self.parse_type_predicate_prefix())
        } else {
            None
        };
        let type_ = self.parse_type();
        if let Some(type_predicate_variable) = type_predicate_variable {
            self.finish_node(
                self.factory
                    .create_type_predicate_node(
                        self,
                        None,
                        type_predicate_variable.wrap(),
                        Some(type_.wrap()),
                    )
                    .into(),
                pos,
                None,
            )
        } else {
            type_
        }
    }
}
