use gc::Gc;
use id_arena::Id;

use super::{ParserType, ParsingContext, SignatureFlags};
use crate::{
    get_full_width, is_function_type_node, is_jsdoc_nullable_type, is_modifier_kind,
    set_text_range, some, token_is_identifier_or_keyword, token_to_string, DiagnosticMessage,
    Diagnostics, ImportTypeNode, InferTypeNode, KeywordTypeNode, LiteralTypeNode, MappedTypeNode,
    Node, NodeArray, NodeFlags, NodeInterface, ParameterDeclaration, ReadonlyTextRange, SyntaxKind,
    TupleTypeNode, TypeLiteralNode, TypeOperatorNode, TypeParameterDeclaration, TypeQueryNode,
    HasArena, InArena,
};

impl ParserType {
    pub(super) fn parse_jsdoc_function_type(&self) -> Id<Node> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        if self.look_ahead_bool(|| self.next_token_is_open_paren()) {
            self.next_token();
            let parameters = self.parse_parameters(SignatureFlags::Type | SignatureFlags::JSDoc);
            let type_ = self.parse_return_type(SyntaxKind::ColonToken, false);
            return self.with_jsdoc(
                self.finish_node(
                    self.factory()
                        .ref_(self).create_jsdoc_function_type_raw(parameters, type_),
                    pos,
                    None,
                )
                .alloc(self.arena()),
                has_jsdoc,
            );
        }
        self.finish_node(
            self.factory().ref_(self).create_type_reference_node_raw(
                self.parse_identifier_name(None).alloc(self.arena()),
                Option::<Gc<NodeArray>>::None,
            ),
            pos,
            None,
        )
        .alloc(self.arena())
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
            self.factory().ref_(self).create_parameter_declaration_raw(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                None,
                name.map(|name| name.alloc(self.arena())),
                None,
                Some(self.parse_jsdoc_type()),
                None,
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_jsdoc_type(&self) -> Id<Node> {
        self.scanner().set_in_jsdoc_type(true);
        let pos = self.get_node_pos();
        if self.parse_optional(SyntaxKind::ModuleKeyword) {
            let module_tag = self.factory().ref_(self).create_jsdoc_namepath_type_raw(None);
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
            return self.finish_node(module_tag, pos, None).alloc(self.arena());
        }

        let has_dot_dot_dot = self.parse_optional(SyntaxKind::DotDotDotToken);
        let mut type_: Id<Node> = self.parse_type_or_type_predicate();
        self.scanner().set_in_jsdoc_type(false);
        if has_dot_dot_dot {
            type_ = self
                .finish_node(
                    self.factory().ref_(self).create_jsdoc_variadic_type_raw(Some(type_)),
                    pos,
                    None,
                )
                .alloc(self.arena());
        }
        if self.token() == SyntaxKind::EqualsToken {
            self.next_token();
            return self
                .finish_node(
                    self.factory().ref_(self).create_jsdoc_optional_type_raw(Some(type_)),
                    pos,
                    None,
                )
                .alloc(self.arena());
        }
        type_
    }

    pub(super) fn parse_type_query(&self) -> TypeQueryNode {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::TypeOfKeyword, None, None);
        self.finish_node(
            self.factory()
                .ref_(self).create_type_query_node_raw(self.parse_entity_name(true, None).alloc(self.arena())),
            pos,
            None,
        )
    }

    pub(super) fn parse_type_parameter(&self) -> TypeParameterDeclaration {
        let pos = self.get_node_pos();
        let name = self.parse_identifier(None, None);
        let mut constraint: Option<Id<Node>> = None;
        let mut expression: Option<Id<Node>> = None;
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
        let mut node = self.factory().ref_(self).create_type_parameter_declaration_raw(
            name.alloc(self.arena()),
            constraint,
            default_type,
        );
        node.expression = expression;
        self.finish_node(node, pos, None)
    }

    pub(super) fn parse_type_parameters(
        &self,
    ) -> Option<Gc<NodeArray> /*<TypeParameterDeclaration>*/> {
        if self.token() == SyntaxKind::LessThanToken {
            return Some(self.parse_bracketed_list(
                ParsingContext::TypeParameters,
                || self.parse_type_parameter().alloc(self.arena()),
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

    pub(super) fn parse_name_of_parameter(&self, modifiers: Option<&NodeArray>) -> Id<Node> {
        let name = self.parse_identifier_or_pattern(Some(
            &Diagnostics::Private_identifiers_cannot_be_used_as_parameters,
        ));
        if get_full_width(&name.ref_(self)) == 0
            && !some(
                modifiers.as_ref().map(|modifiers| {
                    let modifiers: &[Id<Node>] = modifiers;
                    modifiers
                }),
                Option::<fn(&Id<Node>) -> bool>::None,
            )
            && is_modifier_kind(self.token())
        {
            self.next_token();
        }
        name
    }

    pub(super) fn parse_parameter_in_outer_await_context(
        &self,
    ) -> Id<Node /*ParameterDeclaration*/> {
        self.parse_parameter_worker(true)
    }

    pub(super) fn parse_parameter(&self) -> Id<Node /*ParameterDeclaration*/> {
        self.parse_parameter_worker(false)
    }

    pub(super) fn parse_parameter_worker(
        &self,
        in_outer_await_context: bool,
    ) -> Id<Node /*ParameterDeclaration*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();

        let decorators = if in_outer_await_context {
            self.do_in_await_context(|| self.parse_decorators())
        } else {
            self.parse_decorators()
        };

        if self.token() == SyntaxKind::ThisKeyword {
            let node = self.factory().ref_(self).create_parameter_declaration_raw(
                decorators.clone(),
                Option::<Gc<NodeArray>>::None,
                None,
                Some(self.create_identifier(true, None, None).alloc(self.arena())),
                None,
                self.parse_type_annotation(),
                None,
            );

            if let Some(decorators) = decorators {
                self.parse_error_at_range(
                    &*decorators[0].ref_(self),
                    &Diagnostics::Decorators_may_not_be_applied_to_this_parameters,
                    None,
                );
            }

            return self.with_jsdoc(self.finish_node(node, pos, None).alloc(self.arena()), has_jsdoc);
        }

        let saved_top_level = self.top_level();
        self.set_top_level(false);
        let modifiers = self.parse_modifiers(None, None);
        let dot_dot_dot_token = self.parse_optional_token(SyntaxKind::DotDotDotToken);
        let name = self.parse_name_of_parameter(modifiers.as_deref());
        let question_token = self.parse_optional_token(SyntaxKind::QuestionToken);
        let type_annotation = self.parse_type_annotation();
        let initializer = self.parse_initializer();
        let node = self.with_jsdoc(
            self.finish_node(
                self.factory().ref_(self).create_parameter_declaration_raw(
                    decorators,
                    modifiers,
                    dot_dot_dot_token.map(|node| node.alloc(self.arena())),
                    Some(name),
                    question_token.map(|node| node.alloc(self.arena())),
                    type_annotation,
                    initializer,
                ),
                pos,
                None,
            )
            .alloc(self.arena()),
            has_jsdoc,
        );
        self.set_top_level(saved_top_level);
        node
    }

    pub(super) fn parse_return_type(
        &self,
        return_token: SyntaxKind, /*SyntaxKind.ColonToken | SyntaxKind.EqualsGreaterThanToken*/
        is_type: bool,
    ) -> Option<Id<Node /*TypeNode*/>> {
        if self.should_parse_return_type(return_token, is_type) {
            return Some(self.parse_type_or_type_predicate());
        }
        None
    }

    pub(super) fn should_parse_return_type(
        &self,
        return_token: SyntaxKind, /*SyntaxKind.ColonToken | SyntaxKind.EqualsGreaterThanToken*/
        is_type: bool,
    ) -> bool {
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
                    .to_owned()]),
            );
            self.next_token();
            return true;
        }
        false
    }

    pub(super) fn parse_parameters_worker(&self, flags: SignatureFlags) -> Gc<NodeArray> /*<ParameterDeclaration>*/
    {
        let saved_yield_context = self.in_yield_context();
        let saved_await_context = self.in_await_context();

        self.set_yield_context(flags.intersects(SignatureFlags::Yield));
        self.set_await_context(flags.intersects(SignatureFlags::Await));

        let parameters = if flags.intersects(SignatureFlags::JSDoc) {
            self.parse_delimited_list(
                ParsingContext::JSDocParameters,
                || self.parse_jsdoc_parameter().alloc(self.arena()),
                None,
            )
        } else {
            self.parse_delimited_list(
                ParsingContext::Parameters,
                || {
                    if saved_await_context {
                        self.parse_parameter_in_outer_await_context()
                    } else {
                        self.parse_parameter()
                    }
                },
                None,
            )
        };

        self.set_yield_context(saved_yield_context);
        self.set_await_context(saved_await_context);

        parameters
    }

    pub(super) fn parse_parameters(&self, flags: SignatureFlags) -> Gc<NodeArray> /*<ParameterDeclaration>*/
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

    pub(super) fn parse_signature_member(
        &self,
        kind: SyntaxKind, /*SyntaxKind.CallSignature | SyntaxKind.ConstructSignature*/
    ) -> Id<Node /*CallSignatureDeclaration | ConstructSignatureDeclaration*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        if kind == SyntaxKind::ConstructSignature {
            self.parse_expected(SyntaxKind::NewKeyword, None, None);
        }

        let type_parameters = self.parse_type_parameters();
        let parameters = self.parse_parameters(SignatureFlags::Type);
        let type_ = self.parse_return_type(SyntaxKind::ColonToken, true);
        self.parse_type_member_semicolon();
        let node: Node = if kind == SyntaxKind::CallSignature {
            self.factory()
                .ref_(self).create_call_signature_raw(type_parameters, parameters, type_)
                .into()
        } else {
            self.factory()
                .ref_(self).create_construct_signature_raw(type_parameters, parameters, type_)
                .into()
        };
        self.with_jsdoc(self.finish_node(node, pos, None).alloc(self.arena()), has_jsdoc)
    }

    pub(super) fn is_index_signature(&self) -> bool {
        self.token() == SyntaxKind::OpenBracketToken
            && self.look_ahead_bool(|| self.is_unambiguously_index_signature())
    }

    pub(super) fn is_unambiguously_index_signature(&self) -> bool {
        self.next_token();
        if matches!(
            self.token(),
            SyntaxKind::DotDotDotToken | SyntaxKind::CloseBracketToken
        ) {
            return true;
        }

        if is_modifier_kind(self.token()) {
            self.next_token();
            if self.is_identifier() {
                return true;
            }
        } else if !self.is_identifier() {
            return false;
        } else {
            self.next_token();
        }

        if matches!(
            self.token(),
            SyntaxKind::ColonToken | SyntaxKind::CommaToken
        ) {
            return true;
        }

        if self.token() != SyntaxKind::QuestionToken {
            return false;
        }

        self.next_token();
        matches!(
            self.token(),
            SyntaxKind::ColonToken | SyntaxKind::CommaToken | SyntaxKind::CloseBracketToken
        )
    }

    pub(super) fn parse_index_signature_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Id<Node /*IndexSignatureDeclaration*/> {
        let parameters = self.parse_bracketed_list(
            ParsingContext::Parameters,
            || self.parse_parameter(),
            SyntaxKind::OpenBracketToken,
            SyntaxKind::CloseBracketToken,
        );
        let type_ = self.parse_type_annotation();
        self.parse_type_member_semicolon();
        let node = self
            .factory()
            .ref_(self).create_index_signature_raw(decorators, modifiers, parameters, type_);
        self.with_jsdoc(self.finish_node(node, pos, None).alloc(self.arena()), has_jsdoc)
    }

    pub(super) fn parse_property_or_method_signature(
        &self,
        pos: isize,
        has_jsdoc: bool,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Id<Node /*PropertySignature | MethodSignature*/> {
        let name = self.parse_property_name();
        let question_token = self.parse_optional_token(SyntaxKind::QuestionToken);
        let node: Node;
        if matches!(
            self.token(),
            SyntaxKind::OpenParenToken | SyntaxKind::LessThanToken
        ) {
            let type_parameters = self.parse_type_parameters();
            let parameters = self.parse_parameters(SignatureFlags::Type);
            let type_ = self.parse_return_type(SyntaxKind::ColonToken, true);
            node = self
                .factory()
                .ref_(self).create_method_signature_raw(
                    modifiers,
                    Some(name.alloc(self.arena())),
                    question_token.map(|question_token| question_token.alloc(self.arena())),
                    type_parameters,
                    Some(parameters),
                    type_,
                )
                .into();
        } else {
            let type_ = self.parse_type_annotation();
            let mut node_as_property_signature = self.factory().ref_(self).create_property_signature_raw(
                modifiers,
                name.alloc(self.arena()),
                question_token.map(|question_token| question_token.alloc(self.arena())),
                type_,
            );
            if self.token() == SyntaxKind::EqualsToken {
                node_as_property_signature.initializer = self.parse_initializer();
            }
            node = node_as_property_signature.into();
        }
        self.parse_type_member_semicolon();
        self.with_jsdoc(self.finish_node(node, pos, None).alloc(self.arena()), has_jsdoc)
    }

    pub(super) fn is_type_member_start(&self) -> bool {
        if matches!(
            self.token(),
            SyntaxKind::OpenParenToken
                | SyntaxKind::LessThanToken
                | SyntaxKind::GetKeyword
                | SyntaxKind::SetKeyword
        ) {
            return true;
        }
        let mut id_token = false;
        while is_modifier_kind(self.token()) {
            id_token = true;
            self.next_token();
        }
        if self.token() == SyntaxKind::OpenBracketToken {
            return true;
        }
        if self.is_literal_property_name() {
            id_token = true;
            self.next_token();
        }
        if id_token {
            return matches!(
                self.token(),
                SyntaxKind::OpenParenToken
                    | SyntaxKind::LessThanToken
                    | SyntaxKind::QuestionToken
                    | SyntaxKind::ColonToken
                    | SyntaxKind::CommaToken
            ) || self.can_parse_semicolon();
        }
        false
    }

    pub(super) fn parse_type_member(&self) -> Id<Node> {
        if matches!(
            self.token(),
            SyntaxKind::OpenParenToken | SyntaxKind::LessThanToken
        ) {
            return self.parse_signature_member(SyntaxKind::CallSignature);
        }
        if self.token() == SyntaxKind::NewKeyword
            && self.look_ahead_bool(|| self.next_token_is_open_paren_or_less_than())
        {
            return self.parse_signature_member(SyntaxKind::ConstructSignature);
        }
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        let modifiers = self.parse_modifiers(None, None);
        if self.parse_contextual_modifier(SyntaxKind::GetKeyword) {
            return self.parse_accessor_declaration(
                pos,
                has_jsdoc,
                None,
                modifiers,
                SyntaxKind::GetAccessor,
            );
        }

        if self.parse_contextual_modifier(SyntaxKind::SetKeyword) {
            return self.parse_accessor_declaration(
                pos,
                has_jsdoc,
                None,
                modifiers,
                SyntaxKind::SetAccessor,
            );
        }

        if self.is_index_signature() {
            return self.parse_index_signature_declaration(pos, has_jsdoc, None, modifiers);
        }
        self.parse_property_or_method_signature(pos, has_jsdoc, modifiers)
    }

    pub(super) fn next_token_is_open_paren_or_less_than(&self) -> bool {
        self.next_token();
        matches!(
            self.token(),
            SyntaxKind::OpenParenToken | SyntaxKind::LessThanToken
        )
    }

    pub(super) fn next_token_is_dot(&self) -> bool {
        self.next_token() == SyntaxKind::DotToken
    }

    pub(super) fn next_token_is_open_paren_or_less_than_or_dot(&self) -> bool {
        matches!(
            self.next_token(),
            SyntaxKind::OpenParenToken | SyntaxKind::LessThanToken | SyntaxKind::DotToken
        )
    }

    pub(super) fn parse_type_literal(&self) -> TypeLiteralNode {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory()
                .ref_(self).create_type_literal_node_raw(Some(self.parse_object_type_members())),
            pos,
            None,
        )
    }

    pub(super) fn parse_object_type_members(&self) -> Gc<NodeArray> /*<TypeElement>*/ {
        let members: Gc<NodeArray>;
        if self.parse_expected(SyntaxKind::OpenBraceToken, None, None) {
            members = self.parse_list(ParsingContext::TypeMembers, &mut || {
                self.parse_type_member()
            });
            self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        } else {
            members = self.create_missing_list();
        }

        members
    }

    pub(super) fn is_start_of_mapped_type(&self) -> bool {
        self.next_token();
        if matches!(self.token(), SyntaxKind::PlusToken | SyntaxKind::MinusToken) {
            return self.next_token() == SyntaxKind::ReadonlyKeyword;
        }
        if self.token() == SyntaxKind::ReadonlyKeyword {
            self.next_token();
        }
        self.token() == SyntaxKind::OpenBracketToken
            && self.next_token_is_identifier()
            && self.next_token() == SyntaxKind::InKeyword
    }

    pub(super) fn parse_mapped_type_parameter(&self) -> TypeParameterDeclaration {
        let pos = self.get_node_pos();
        let name = self.parse_identifier_name(None);
        self.parse_expected(SyntaxKind::InKeyword, None, None);
        let type_ = self.parse_type();
        self.finish_node(
            self.factory()
                .ref_(self).create_type_parameter_declaration_raw(name.alloc(self.arena()), Some(type_), None),
            pos,
            None,
        )
    }

    pub(super) fn parse_mapped_type(&self) -> MappedTypeNode {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBraceToken, None, None);
        let mut readonly_token: Option<Node /*ReadonlyKeyword | PlusToken | MinusToken*/> = None;
        if matches!(
            self.token(),
            SyntaxKind::ReadonlyKeyword | SyntaxKind::PlusToken | SyntaxKind::MinusToken
        ) {
            readonly_token = Some(self.parse_token_node().into());
            if readonly_token.as_ref().unwrap().kind() != SyntaxKind::ReadonlyKeyword {
                self.parse_expected(SyntaxKind::ReadonlyKeyword, None, None);
            }
        }
        self.parse_expected(SyntaxKind::OpenBracketToken, None, None);
        let type_parameter = self.parse_mapped_type_parameter();
        let name_type = if self.parse_optional(SyntaxKind::AsKeyword) {
            Some(self.parse_type())
        } else {
            None
        };
        self.parse_expected(SyntaxKind::CloseBracketToken, None, None);
        let mut question_token: Option<Node /*QuestionKeyword | PlusToken | MinusToken*/> = None;
        if matches!(
            self.token(),
            SyntaxKind::QuestionToken | SyntaxKind::PlusToken | SyntaxKind::MinusToken
        ) {
            question_token = Some(self.parse_token_node().into());
            if question_token.as_ref().unwrap().kind() != SyntaxKind::QuestionToken {
                self.parse_expected(SyntaxKind::QuestionToken, None, None);
            }
        }
        let type_ = self.parse_type_annotation();
        self.parse_semicolon();
        let members = self.parse_list(ParsingContext::TypeMembers, &mut || {
            self.parse_type_member()
        });
        self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        self.finish_node(
            self.factory().ref_(self).create_mapped_type_node_raw(
                readonly_token.map(|readonly_token| readonly_token.alloc(self.arena())),
                type_parameter.alloc(self.arena()),
                name_type,
                question_token.map(|question_token| question_token.alloc(self.arena())),
                type_,
                Some(members),
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_tuple_element_type(&self) -> Id<Node /*TypeNode*/> {
        let pos = self.get_node_pos();
        if self.parse_optional(SyntaxKind::DotDotDotToken) {
            return self
                .finish_node(
                    self.factory().ref_(self).create_rest_type_node_raw(self.parse_type()),
                    pos,
                    None,
                )
                .alloc(self.arena());
        }
        let type_ = self.parse_type();
        if is_jsdoc_nullable_type(&type_.ref_(self)) {
            let type_type = type_.ref_(self).as_base_jsdoc_unary_type().type_.unwrap();
            if type_.ref_(self).pos() == type_type.ref_(self).pos() {
                let node: Node = self
                    .factory()
                    .ref_(self).create_optional_type_node_raw(type_type)
                    .into();
                set_text_range(&node, Some(&*type_.ref_(self)));
                node.set_flags(type_.ref_(self).flags());
                return node.alloc(self.arena());
            }
        }
        return type_;
    }

    pub(super) fn is_next_token_colon_or_question_colon(&self) -> bool {
        self.next_token() == SyntaxKind::ColonToken
            || self.token() == SyntaxKind::QuestionToken
                && self.next_token() == SyntaxKind::ColonToken
    }

    pub(super) fn is_tuple_element_name(&self) -> bool {
        if self.token() == SyntaxKind::DotDotDotToken {
            return token_is_identifier_or_keyword(self.next_token())
                && self.is_next_token_colon_or_question_colon();
        }
        token_is_identifier_or_keyword(self.token()) && self.is_next_token_colon_or_question_colon()
    }

    pub(super) fn parse_tuple_element_name_or_tuple_element_type(&self) -> Id<Node /*TypeNode*/> {
        if self.look_ahead_bool(|| self.is_tuple_element_name()) {
            let pos = self.get_node_pos();
            let has_jsdoc = self.has_preceding_jsdoc_comment();
            let dot_dot_dot_token = self.parse_optional_token(SyntaxKind::DotDotDotToken);
            let name = self.parse_identifier_name(None);
            let question_token = self.parse_optional_token(SyntaxKind::QuestionToken);
            self.parse_expected(SyntaxKind::ColonToken, None, None);
            let type_ = self.parse_tuple_element_type();
            let node = self.factory().ref_(self).create_named_tuple_member_raw(
                dot_dot_dot_token.map(|dot_dot_dot_token| dot_dot_dot_token.alloc(self.arena())),
                name.alloc(self.arena()),
                question_token.map(|question_token| question_token.alloc(self.arena())),
                type_,
            );
            return self.with_jsdoc(self.finish_node(node, pos, None).alloc(self.arena()), has_jsdoc);
        }
        self.parse_tuple_element_type()
    }

    pub(super) fn parse_tuple_type(&self) -> TupleTypeNode {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory()
                .ref_(self).create_tuple_type_node_raw(Some(self.parse_bracketed_list(
                    ParsingContext::TupleElementTypes,
                    || self.parse_tuple_element_name_or_tuple_element_type(),
                    SyntaxKind::OpenBracketToken,
                    SyntaxKind::CloseBracketToken,
                ))),
            pos,
            None,
        )
    }

    pub(super) fn parse_parenthesized_type(&self) -> Node /*TypeNode*/ {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenParenToken, None, None);
        let type_ = self.parse_type();
        self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        self.finish_node(
            self.factory().ref_(self).create_parenthesized_type_raw(type_).into(),
            pos,
            None,
        )
    }

    pub(super) fn parse_modifiers_for_constructor_type(
        &self,
    ) -> Option<Gc<NodeArray> /*<Modifier>*/> {
        let mut modifiers = None;
        if self.token() == SyntaxKind::AbstractKeyword {
            let pos = self.get_node_pos();
            self.next_token();
            let modifier = self.finish_node(
                self.factory().ref_(self).create_token_raw(SyntaxKind::AbstractKeyword),
                pos,
                None,
            );
            modifiers = Some(self.create_node_array(vec![modifier.alloc(self.arena())], pos, None, None));
        }
        modifiers
    }

    pub(super) fn parse_function_or_constructor_type(&self) -> Id<Node /*TypeNode*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        let modifiers = self.parse_modifiers_for_constructor_type();
        let is_constructor_type = self.parse_optional(SyntaxKind::NewKeyword);
        let type_parameters = self.parse_type_parameters();
        let parameters = self.parse_parameters(SignatureFlags::Type);
        let type_ = self.parse_return_type(SyntaxKind::EqualsGreaterThanToken, false);
        let node: Node = if is_constructor_type {
            self.factory()
                .ref_(self).create_constructor_type_node_raw(modifiers, type_parameters, parameters, type_)
                .into()
        } else {
            let function_type_node: Node = self
                .factory()
                .ref_(self).create_function_type_node_raw(type_parameters, parameters, type_)
                .into();
            // if !is_constructor_type {
            function_type_node.set_modifiers(modifiers);
            // }
            function_type_node
        };
        self.with_jsdoc(self.finish_node(node, pos, None).alloc(self.arena()), has_jsdoc)
    }

    pub(super) fn parse_keyword_and_no_dot(&self) -> Option<Node /*TypeNode*/> {
        let node = self.parse_token_node();
        if self.token() == SyntaxKind::DotToken {
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
        let mut expression: Node = match self.token() {
            SyntaxKind::TrueKeyword | SyntaxKind::FalseKeyword | SyntaxKind::NullKeyword => {
                self.parse_token_node().into()
            }
            _ => self.parse_literal_like_node(self.token()),
        };
        if negative {
            expression = self.finish_node(
                self.factory()
                    .ref_(self).create_prefix_unary_expression_raw(SyntaxKind::MinusToken, expression.alloc(self.arena()))
                    .into(),
                pos,
                None,
            );
        }
        self.finish_node(
            self.factory()
                .ref_(self).create_literal_type_node_raw(expression.alloc(self.arena())),
            pos,
            None,
        )
    }

    pub(super) fn is_start_of_type_of_import_type(&self) -> bool {
        self.next_token();
        self.token() == SyntaxKind::ImportKeyword
    }

    pub(super) fn parse_import_type(&self) -> ImportTypeNode {
        self.set_source_flags(self.source_flags() | NodeFlags::PossiblyContainsDynamicImport);
        let pos = self.get_node_pos();
        let is_type_of = self.parse_optional(SyntaxKind::TypeOfKeyword);
        self.parse_expected(SyntaxKind::ImportKeyword, None, None);
        self.parse_expected(SyntaxKind::OpenParenToken, None, None);
        let type_ = self.parse_type();
        self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        let qualifier = if self.parse_optional(SyntaxKind::DotToken) {
            Some(self.parse_entity_name_of_type_reference())
        } else {
            None
        };
        let type_arguments = self.parse_type_arguments_of_type_reference();
        self.finish_node(
            self.factory().ref_(self).create_import_type_node_raw(
                type_,
                qualifier.map(|qualifier| qualifier.alloc(self.arena())),
                type_arguments,
                Some(is_type_of),
            ),
            pos,
            None,
        )
    }

    pub(super) fn next_token_is_numeric_or_big_int_literal(&self) -> bool {
        self.next_token();
        matches!(
            self.token(),
            SyntaxKind::NumericLiteral | SyntaxKind::BigIntLiteral
        )
    }

    pub(super) fn parse_non_array_type(&self) -> Id<Node> {
        match self.token() {
            SyntaxKind::AnyKeyword
            | SyntaxKind::UnknownKeyword
            | SyntaxKind::StringKeyword
            | SyntaxKind::NumberKeyword
            | SyntaxKind::BigIntKeyword
            | SyntaxKind::SymbolKeyword
            | SyntaxKind::BooleanKeyword
            | SyntaxKind::UndefinedKeyword
            | SyntaxKind::NeverKeyword
            | SyntaxKind::ObjectKeyword => self
                .try_parse(|| self.parse_keyword_and_no_dot().map(|node| node.alloc(self.arena())))
                .unwrap_or_else(|| self.parse_type_reference().alloc(self.arena())),
            SyntaxKind::AsteriskEqualsToken => {
                self.scanner().re_scan_asterisk_equals_token();
                self.parse_jsdoc_all_type().alloc(self.arena())
            }
            SyntaxKind::AsteriskToken => self.parse_jsdoc_all_type().alloc(self.arena()),
            SyntaxKind::QuestionQuestionToken => {
                self.scanner().re_scan_question_token();
                self.parse_jsdoc_unknown_or_nullable_type().alloc(self.arena())
            }
            SyntaxKind::QuestionToken => self.parse_jsdoc_unknown_or_nullable_type().alloc(self.arena()),
            SyntaxKind::FunctionKeyword => self.parse_jsdoc_function_type(),
            SyntaxKind::ExclamationToken => self.parse_jsdoc_non_nullable_type().alloc(self.arena()),
            SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::StringLiteral
            | SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::TrueKeyword
            | SyntaxKind::FalseKeyword
            | SyntaxKind::NullKeyword => self.parse_literal_type_node(None).alloc(self.arena()),
            SyntaxKind::MinusToken => {
                if self.look_ahead_bool(|| self.next_token_is_numeric_or_big_int_literal()) {
                    self.parse_literal_type_node(Some(true)).alloc(self.arena())
                } else {
                    self.parse_type_reference().alloc(self.arena())
                }
            }
            SyntaxKind::VoidKeyword => self.parse_token_node().alloc(self.arena()),
            SyntaxKind::ThisKeyword => {
                let this_keyword = self.parse_this_type_node();
                if self.token() == SyntaxKind::IsKeyword
                    && !self.scanner().has_preceding_line_break()
                {
                    self.parse_this_type_predicate(this_keyword.alloc(self.arena())).alloc(self.arena())
                } else {
                    this_keyword.alloc(self.arena())
                }
            }
            SyntaxKind::TypeOfKeyword => {
                if self.look_ahead_bool(|| self.is_start_of_type_of_import_type()) {
                    self.parse_import_type().alloc(self.arena())
                } else {
                    self.parse_type_query().alloc(self.arena())
                }
            }
            SyntaxKind::OpenBraceToken => {
                if self.look_ahead_bool(|| self.is_start_of_mapped_type()) {
                    self.parse_mapped_type().alloc(self.arena())
                } else {
                    self.parse_type_literal().alloc(self.arena())
                }
            }
            SyntaxKind::OpenBracketToken => self.parse_tuple_type().alloc(self.arena()),
            SyntaxKind::OpenParenToken => self.parse_parenthesized_type().alloc(self.arena()),
            SyntaxKind::ImportKeyword => self.parse_import_type().alloc(self.arena()),
            SyntaxKind::AssertsKeyword => {
                if self.look_ahead_bool(|| self.next_token_is_identifier_or_keyword_on_same_line())
                {
                    self.parse_asserts_type_predicate().alloc(self.arena())
                } else {
                    self.parse_type_reference().alloc(self.arena())
                }
            }
            SyntaxKind::TemplateHead => self.parse_template_type().alloc(self.arena()),
            _ => self.parse_type_reference().alloc(self.arena()),
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
            SyntaxKind::FunctionKeyword => !in_start_of_parameter,
            SyntaxKind::MinusToken => {
                !in_start_of_parameter
                    && self.look_ahead_bool(|| self.next_token_is_numeric_or_big_int_literal())
            }
            SyntaxKind::OpenParenToken => {
                !in_start_of_parameter
                    && self.look_ahead_bool(|| self.is_start_of_parenthesized_or_function_type())
            }
            _ => self.is_identifier(),
        }
    }

    pub(super) fn is_start_of_parenthesized_or_function_type(&self) -> bool {
        self.next_token();
        self.token() == SyntaxKind::CloseParenToken
            || self.is_start_of_parameter(false)
            || self.is_start_of_type(None)
    }

    pub(super) fn parse_postfix_type_or_higher(&self) -> Id<Node /*TypeNode*/> {
        let pos = self.get_node_pos();
        let mut type_: Id<Node> = self.parse_non_array_type();
        while !self.scanner().has_preceding_line_break() {
            match self.token() {
                SyntaxKind::ExclamationToken => {
                    self.next_token();
                    type_ = self
                        .finish_node(
                            self.factory()
                                .ref_(self).create_jsdoc_non_nullable_type_raw(Some(type_)),
                            pos,
                            None,
                        )
                        .alloc(self.arena());
                }
                SyntaxKind::QuestionToken => {
                    if self.look_ahead_bool(|| self.next_token_is_start_of_type()) {
                        return type_;
                    }
                    self.next_token();
                    type_ = self
                        .finish_node(
                            self.factory().ref_(self).create_jsdoc_nullable_type_raw(Some(type_)),
                            pos,
                            None,
                        )
                        .alloc(self.arena());
                }
                SyntaxKind::OpenBracketToken => {
                    self.parse_expected(SyntaxKind::OpenBracketToken, None, None);
                    if self.is_start_of_type(None) {
                        let index_type = self.parse_type();
                        self.parse_expected(SyntaxKind::CloseBracketToken, None, None);
                        type_ = self
                            .finish_node(
                                self.factory()
                                    .ref_(self).create_indexed_access_type_node_raw(type_, index_type),
                                pos,
                                None,
                            )
                            .alloc(self.arena());
                    } else {
                        self.parse_expected(SyntaxKind::CloseBracketToken, None, None);
                        type_ = self
                            .finish_node(
                                self.factory().ref_(self).create_array_type_node_raw(type_),
                                pos,
                                None,
                            )
                            .alloc(self.arena());
                    }
                }
                _ => {
                    return type_;
                }
            }
        }
        type_
    }

    pub(super) fn parse_type_operator(
        &self,
        operator: SyntaxKind, /*SyntaxKind.KeyOfKeyword | SyntaxKind.UniqueKeyword | SyntaxKind.ReadonlyKeyword*/
    ) -> TypeOperatorNode {
        let pos = self.get_node_pos();
        self.parse_expected(operator, None, None);
        self.finish_node(
            self.factory()
                .ref_(self).create_type_operator_node_raw(operator, self.parse_type_operator_or_higher()),
            pos,
            None,
        )
    }

    pub(super) fn parse_type_parameter_of_infer_type(&self) -> TypeParameterDeclaration {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory().ref_(self).create_type_parameter_declaration_raw(
                self.parse_identifier(None, None).alloc(self.arena()),
                None,
                None,
            ),
            pos,
            None,
        )
    }

    pub(super) fn parse_infer_type(&self) -> InferTypeNode {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::InferKeyword, None, None);
        self.finish_node(
            self.factory()
                .ref_(self).create_infer_type_node_raw(self.parse_type_parameter_of_infer_type().alloc(self.arena())),
            pos,
            None,
        )
    }

    pub(super) fn parse_type_operator_or_higher(&self) -> Id<Node> {
        let operator = self.token();
        match operator {
            SyntaxKind::KeyOfKeyword | SyntaxKind::UniqueKeyword | SyntaxKind::ReadonlyKeyword => {
                self.parse_type_operator(operator).alloc(self.arena())
            }
            SyntaxKind::InferKeyword => self.parse_infer_type().alloc(self.arena()),
            _ => self.parse_postfix_type_or_higher(),
        }
    }

    pub(super) fn parse_function_or_constructor_type_to_error(
        &self,
        is_in_union_type: bool,
    ) -> Option<Id<Node /*TypeNode*/>> {
        if self.is_start_of_function_type_or_constructor_type() {
            let type_ = self.parse_function_or_constructor_type();
            let diagnostic: &DiagnosticMessage;
            if is_function_type_node(&type_.ref_(self)) {
                diagnostic = if is_in_union_type {
                    &Diagnostics::Function_type_notation_must_be_parenthesized_when_used_in_a_union_type
                } else {
                    &Diagnostics::Function_type_notation_must_be_parenthesized_when_used_in_an_intersection_type
                };
            } else {
                diagnostic = if is_in_union_type {
                    &Diagnostics::Constructor_type_notation_must_be_parenthesized_when_used_in_a_union_type
                } else {
                    &Diagnostics::Constructor_type_notation_must_be_parenthesized_when_used_in_an_intersection_type
                };
            }
            self.parse_error_at_range(&*type_.ref_(self), diagnostic, None);
            return Some(type_);
        }
        None
    }

    pub(super) fn parse_union_or_intersection_type(
        &self,
        operator: SyntaxKind, /*SyntaxKind.BarToken | SyntaxKind.AmpersandToken*/
        mut parse_constituent_type: impl FnMut() -> Id<Node>,
        mut create_type_node: impl FnMut(Gc<NodeArray>) -> Node,
    ) -> Id<Node> {
        let pos = self.get_node_pos();
        let is_union_type = operator == SyntaxKind::BarToken;
        let has_leading_operator = self.parse_optional(operator);
        let mut type_: Option<Id<Node>> = Some(if has_leading_operator {
            self.parse_function_or_constructor_type_to_error(is_union_type)
                .unwrap_or_else(|| parse_constituent_type())
        } else {
            parse_constituent_type()
        });
        if self.token() == operator || has_leading_operator {
            let mut types: Vec<Id<Node>> = vec![type_.take().unwrap()];
            while self.parse_optional(operator) {
                types.push(
                    self.parse_function_or_constructor_type_to_error(is_union_type)
                        .unwrap_or_else(|| parse_constituent_type()),
                );
            }
            type_ = Some(
                self.finish_node(
                    create_type_node(self.create_node_array(types, pos, None, None)),
                    pos,
                    None,
                )
                .alloc(self.arena()),
            );
        }
        type_.unwrap()
    }

    pub(super) fn parse_intersection_type_or_higher(&self) -> Id<Node> {
        self.parse_union_or_intersection_type(
            SyntaxKind::AmpersandToken,
            || self.parse_type_operator_or_higher(),
            |types| self.factory().ref_(self).create_intersection_type_node_raw(types),
        )
    }

    pub(super) fn parse_union_type_or_higher(&self) -> Id<Node> {
        self.parse_union_or_intersection_type(
            SyntaxKind::BarToken,
            || self.parse_intersection_type_or_higher(),
            |types| self.factory().ref_(self).create_union_type_node_raw(types),
        )
    }

    pub(super) fn next_token_is_new_keyword(&self) -> bool {
        self.next_token();
        self.token() == SyntaxKind::NewKeyword
    }

    pub(super) fn is_start_of_function_type_or_constructor_type(&self) -> bool {
        if self.token() == SyntaxKind::LessThanToken {
            return true;
        }
        if self.token() == SyntaxKind::OpenParenToken
            && self.look_ahead_bool(|| self.is_unambiguously_start_of_function_type())
        {
            return true;
        }
        self.token() == SyntaxKind::NewKeyword
            || self.token() == SyntaxKind::AbstractKeyword
                && self.look_ahead_bool(|| self.next_token_is_new_keyword())
    }

    pub(super) fn skip_parameter_start(&self) -> bool {
        if is_modifier_kind(self.token()) {
            self.parse_modifiers(None, None);
        }
        if self.is_identifier() || self.token() == SyntaxKind::ThisKeyword {
            self.next_token();
            return true;
        }
        if matches!(
            self.token(),
            SyntaxKind::OpenBracketToken | SyntaxKind::OpenBraceToken
        ) {
            let previous_error_count = self.parse_diagnostics().len();
            self.parse_identifier_or_pattern(None);
            return previous_error_count == self.parse_diagnostics().len();
        }
        false
    }

    pub(super) fn is_unambiguously_start_of_function_type(&self) -> bool {
        self.next_token();
        if matches!(
            self.token(),
            SyntaxKind::CloseParenToken | SyntaxKind::DotDotDotToken
        ) {
            return true;
        }
        if self.skip_parameter_start() {
            if matches!(
                self.token(),
                SyntaxKind::ColonToken
                    | SyntaxKind::CommaToken
                    | SyntaxKind::QuestionToken
                    | SyntaxKind::EqualsToken
            ) {
                return true;
            }
            if self.token() == SyntaxKind::CloseParenToken {
                self.next_token();
                if self.token() == SyntaxKind::EqualsGreaterThanToken {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn parse_type_or_type_predicate(&self) -> Id<Node /*TypeNode*/> {
        let pos = self.get_node_pos();
        let type_predicate_variable = if self.is_identifier() {
            self.try_parse(|| self.parse_type_predicate_prefix())
        } else {
            None
        };
        let type_ = self.parse_type();
        if let Some(type_predicate_variable) = type_predicate_variable {
            self.finish_node(
                self.factory().ref_(self).create_type_predicate_node_raw(
                    None,
                    type_predicate_variable.alloc(self.arena()),
                    Some(type_),
                ),
                pos,
                None,
            )
            .alloc(self.arena())
        } else {
            type_
        }
    }
}
