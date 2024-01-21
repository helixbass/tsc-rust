use gc::Gc;
use id_arena::Id;

use super::{ParserType, ParsingContext, SignatureFlags};
use crate::{
    append, is_async_modifier, is_class_member_modifier, is_keyword, is_modifier_kind,
    modifiers_to_flags, some, token_is_identifier_or_keyword, ArrayBindingPattern, AsDoubleDeref,
    Debug_, Decorator, DiagnosticMessage, Diagnostics, FunctionLikeDeclarationInterface,
    HasTypeInterface, HasTypeParametersInterface, ModifierFlags, Node, NodeArray, NodeFlags,
    NodeInterface, ObjectBindingPattern, SyntaxKind, VariableDeclarationList,
    InArena,
};

impl ParserType {
    pub(super) fn parse_object_binding_pattern(&self) -> ObjectBindingPattern {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBraceToken, None, None);
        let elements = self.parse_delimited_list(
            ParsingContext::ObjectBindingElements,
            || self.parse_object_binding_element().alloc(self),
            None,
        );
        self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        self.finish_node(
            self.factory().create_object_binding_pattern_raw(elements),
            pos,
            None,
        )
    }

    pub(super) fn parse_array_binding_pattern(&self) -> ArrayBindingPattern {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBracketToken, None, None);
        let elements = self.parse_delimited_list(
            ParsingContext::ArrayBindingElements,
            || self.parse_array_binding_element().alloc(self),
            None,
        );
        self.parse_expected(SyntaxKind::CloseBracketToken, None, None);
        self.finish_node(
            self.factory().create_array_binding_pattern_raw(elements),
            pos,
            None,
        )
    }

    pub(super) fn is_binding_identifier_or_private_identifier_or_pattern(&self) -> bool {
        matches!(
            self.token(),
            SyntaxKind::OpenBraceToken
                | SyntaxKind::OpenBracketToken
                | SyntaxKind::PrivateIdentifier
        ) || self.is_binding_identifier()
    }

    pub(super) fn parse_identifier_or_pattern(
        &self,
        private_identifier_diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Id<Node /*Identifier | BindingPattern*/> {
        if self.token() == SyntaxKind::OpenBracketToken {
            return self.parse_array_binding_pattern().alloc(self);
        }
        if self.token() == SyntaxKind::OpenBraceToken {
            return self.parse_object_binding_pattern().alloc(self);
        }
        self.parse_binding_identifier(private_identifier_diagnostic_message)
            .alloc(self)
    }

    pub(super) fn parse_variable_declaration_allow_exclamation(
        &self,
    ) -> Id<Node /*VariableDeclaration*/> {
        self.parse_variable_declaration(Some(true))
    }

    pub(super) fn parse_variable_declaration(
        &self,
        allow_exclamation: Option<bool>,
    ) -> Id<Node /*VariableDeclaration*/> {
        let allow_exclamation = allow_exclamation.unwrap_or(false);
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        let name = self.parse_identifier_or_pattern(Some(
            &Diagnostics::Private_identifiers_are_not_allowed_in_variable_declarations,
        ));
        let mut exclamation_token: Option<Id<Node>> = None;
        if allow_exclamation
            && name.ref_(self).kind() == SyntaxKind::Identifier
            && self.token() == SyntaxKind::ExclamationToken
            && !self.scanner().has_preceding_line_break()
        {
            exclamation_token = Some(self.parse_token_node().alloc(self));
        }
        let type_ = self.parse_type_annotation();
        let initializer = if self.is_in_or_of_keyword(self.token()) {
            None
        } else {
            self.parse_initializer()
        };
        let node = self.factory().create_variable_declaration_raw(
            Some(name),
            exclamation_token,
            type_,
            initializer,
        );
        self.with_jsdoc(self.finish_node(node, pos, None).alloc(self), has_jsdoc)
    }

    pub(super) fn parse_variable_declaration_list(
        &self,
        in_for_statement_initializer: bool,
    ) -> VariableDeclarationList {
        let pos = self.get_node_pos();

        let mut flags = NodeFlags::None;
        match self.token() {
            SyntaxKind::VarKeyword => (),
            SyntaxKind::LetKeyword => {
                flags |= NodeFlags::Let;
            }
            SyntaxKind::ConstKeyword => {
                flags |= NodeFlags::Const;
            }
            _ => Debug_.fail(None),
        }

        self.next_token();

        let declarations: Gc<NodeArray>;
        if self.token() == SyntaxKind::OfKeyword
            && self.look_ahead_bool(|| self.can_follow_contextual_of_keyword())
        {
            declarations = self.create_missing_list();
        } else {
            let saved_disallow_in = self.in_disallow_in_context();
            self.set_disallow_in_context(in_for_statement_initializer);

            declarations = self.parse_delimited_list(
                ParsingContext::VariableDeclarations,
                || {
                    if in_for_statement_initializer {
                        self.parse_variable_declaration(None)
                    } else {
                        self.parse_variable_declaration_allow_exclamation()
                    }
                },
                None,
            );

            self.set_disallow_in_context(saved_disallow_in);
        }

        self.finish_node(
            self.factory()
                .create_variable_declaration_list_raw(declarations, Some(flags)),
            pos,
            None,
        )
    }

    pub(super) fn can_follow_contextual_of_keyword(&self) -> bool {
        self.next_token_is_identifier() && self.next_token() == SyntaxKind::CloseParenToken
    }

    pub(super) fn parse_variable_statement(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Id<Node /*VariableStatement*/> {
        let declaration_list = self.parse_variable_declaration_list(false);
        self.parse_semicolon();
        let node = self
            .factory()
            .create_variable_statement_raw(modifiers, declaration_list.alloc(self));
        node.set_decorators(decorators);
        self.with_jsdoc(self.finish_node(node, pos, None).alloc(self), has_jsdoc)
    }

    pub(super) fn parse_function_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Id<Node /*FunctionDeclaration*/> {
        let saved_await_context = self.in_await_context();

        let modifier_flags = modifiers_to_flags(modifiers.as_double_deref(), self);
        self.parse_expected(SyntaxKind::FunctionKeyword, None, None);
        let asterisk_token = self.parse_optional_token(SyntaxKind::AsteriskToken);
        let name = if modifier_flags.intersects(ModifierFlags::Default) {
            self.parse_optional_binding_identifier()
        } else {
            Some(self.parse_binding_identifier(None))
        };
        let is_generator = if asterisk_token.is_some() {
            SignatureFlags::Yield
        } else {
            SignatureFlags::None
        };
        let is_async = if modifier_flags.intersects(ModifierFlags::Async) {
            SignatureFlags::Await
        } else {
            SignatureFlags::None
        };
        let type_parameters = self.parse_type_parameters();
        if modifier_flags.intersects(ModifierFlags::Export) {
            self.set_await_context(true);
        }
        let parameters = self.parse_parameters(is_generator | is_async);
        let type_ = self.parse_return_type(SyntaxKind::ColonToken, false);
        let body = self.parse_function_block_or_semicolon(
            is_generator | is_async,
            Some(&Diagnostics::or_expected),
        );
        self.set_await_context(saved_await_context);
        let node = self.factory().create_function_declaration_raw(
            decorators,
            modifiers,
            asterisk_token.map(|node| node.alloc(self)),
            name.map(|node| node.alloc(self)),
            type_parameters,
            parameters,
            type_,
            body,
        );
        self.with_jsdoc(self.finish_node(node, pos, None).alloc(self), has_jsdoc)
    }

    pub(super) fn parse_constructor_name(&self) -> bool {
        if self.token() == SyntaxKind::ConstructorKeyword {
            return self.parse_expected(SyntaxKind::ConstructorKeyword, None, None);
        }
        if self.token() == SyntaxKind::StringLiteral
            && self.look_ahead(|| Some(self.next_token())).unwrap() == SyntaxKind::OpenParenToken
        {
            return self.try_parse_bool(|| {
                let literal_node = self.parse_literal_node();
                let literal_node_text = literal_node.as_literal_like_node().text();
                &*literal_node_text == "constructor"
            });
        }
        false
    }

    pub(super) fn try_parse_constructor_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Option<Id<Node /*ConstructorDeclaration*/>> {
        self.try_parse(|| {
            if self.parse_constructor_name() {
                let type_parameters = self.parse_type_parameters();
                let parameters = self.parse_parameters(SignatureFlags::None);
                let type_ = self.parse_return_type(SyntaxKind::ColonToken, false);
                let body = self.parse_function_block_or_semicolon(
                    SignatureFlags::None,
                    Some(&Diagnostics::or_expected),
                );
                let mut node = self.factory().create_constructor_declaration_raw(
                    decorators,
                    modifiers,
                    Some(parameters),
                    body,
                );
                *node.maybe_type_parameters_mut() = type_parameters;
                node.set_type(type_);
                return Some(self.with_jsdoc(self.finish_node(node, pos, None).alloc(self), has_jsdoc));
            }
            None
        })
    }

    pub(super) fn parse_method_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
        asterisk_token: Option<Id<Node /*AsteriskToken*/>>,
        name: Id<Node /*PropertyName*/>,
        question_token: Option<Id<Node /*QuestionToken*/>>,
        exclamation_token: Option<Id<Node /*ExclamationToken*/>>,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Id<Node /*MethodDeclaration*/> {
        let is_generator = if asterisk_token.is_some() {
            SignatureFlags::Yield
        } else {
            SignatureFlags::None
        };
        let is_async = if some(
            modifiers.as_double_deref(),
            Some(|modifier: &Id<Node>| is_async_modifier(&modifier.ref_(self))),
        ) {
            SignatureFlags::Await
        } else {
            SignatureFlags::None
        };
        let type_parameters = self.parse_type_parameters();
        let parameters = self.parse_parameters(is_generator | is_async);
        let type_ = self.parse_return_type(SyntaxKind::ColonToken, false);
        let body =
            self.parse_function_block_or_semicolon(is_generator | is_async, diagnostic_message);
        let node = self.factory().create_method_declaration_raw(
            decorators,
            modifiers,
            asterisk_token,
            name,
            question_token,
            type_parameters,
            parameters,
            type_,
            body,
        );
        *node.maybe_exclamation_token() = exclamation_token;
        self.with_jsdoc(self.finish_node(node, pos, None).alloc(self), has_jsdoc)
    }

    pub(super) fn parse_property_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
        name: Id<Node /*PropertyName*/>,
        question_token: Option<Id<Node /*QuestionToken*/>>,
    ) -> Id<Node /*PropertyDeclaration*/> {
        let exclamation_token: Option<Id<Node>> =
            if question_token.is_none() && !self.scanner().has_preceding_line_break() {
                self.parse_optional_token(SyntaxKind::ExclamationToken)
                    .map(|node| node.alloc(self))
            } else {
                None
            };
        let type_ = self.parse_type_annotation();
        let initializer = self.do_outside_of_context(
            NodeFlags::YieldContext | NodeFlags::AwaitContext | NodeFlags::DisallowInContext,
            || self.parse_initializer(),
        );
        self.parse_semicolon_after_property_name(name, type_, initializer);
        let node: Id<Node> = self.factory().create_property_declaration(
            decorators,
            modifiers,
            name,
            question_token.or(exclamation_token),
            type_,
            initializer,
        );
        self.finish_node_ref(&*node.ref_(self), pos, None);
        self.with_jsdoc(node, has_jsdoc)
    }

    pub(super) fn parse_property_or_method_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Id<Node /*PropertyDeclaration | MethodDeclaration*/> {
        let asterisk_token: Option<Id<Node>> = self
            .parse_optional_token(SyntaxKind::AsteriskToken)
            .map(|node| node.alloc(self));
        let name = self.parse_property_name().alloc(self);
        let question_token: Option<Id<Node>> = self
            .parse_optional_token(SyntaxKind::QuestionToken)
            .map(|node| node.alloc(self));
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
                None,
                Some(&Diagnostics::or_expected),
            );
        }
        self.parse_property_declaration(pos, has_jsdoc, decorators, modifiers, name, question_token)
    }

    pub(super) fn parse_accessor_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
        kind: SyntaxKind, /*AccessorDeclaration["kind"]*/
    ) -> Id<Node /*AccessorDeclaration*/> {
        let name = self.parse_property_name().alloc(self);
        let type_parameters = self.parse_type_parameters();
        let parameters = self.parse_parameters(SignatureFlags::None);
        let type_: Option<Id<Node>> = self.parse_return_type(SyntaxKind::ColonToken, false);
        let body: Option<Id<Node>> =
            self.parse_function_block_or_semicolon(SignatureFlags::None, None);
        let node: Node = if kind == SyntaxKind::GetAccessor {
            self.factory()
                .create_get_accessor_declaration_raw(
                    decorators, modifiers, name, parameters, type_, body,
                )
                .into()
        } else {
            let mut node_as_set_accessor_declaration = self
                .factory()
                .create_set_accessor_declaration_raw(decorators, modifiers, name, parameters, body);
            if let Some(type_) = type_ {
                node_as_set_accessor_declaration.set_type(Some(type_));
            }
            node_as_set_accessor_declaration.into()
        };
        *node.as_has_type_parameters().maybe_type_parameters_mut() = type_parameters;
        self.with_jsdoc(self.finish_node(node, pos, None).alloc(self), has_jsdoc)
    }

    pub(super) fn is_class_member_start(&self) -> bool {
        let mut id_token: Option<SyntaxKind> = None;

        if self.token() == SyntaxKind::AtToken {
            return true;
        }

        while is_modifier_kind(self.token()) {
            id_token = Some(self.token());
            if is_class_member_modifier(id_token.unwrap()) {
                return true;
            }

            self.next_token();
        }

        if self.token() == SyntaxKind::AsteriskToken {
            return true;
        }

        if self.is_literal_property_name() {
            id_token = Some(self.token());
            self.next_token();
        }

        if self.token() == SyntaxKind::OpenBracketToken {
            return true;
        }

        if let Some(id_token) = id_token {
            if !is_keyword(id_token)
                || matches!(id_token, SyntaxKind::SetKeyword | SyntaxKind::GetKeyword)
            {
                return true;
            }

            match self.token() {
                SyntaxKind::OpenParenToken
                | SyntaxKind::LessThanToken
                | SyntaxKind::ExclamationToken
                | SyntaxKind::ColonToken
                | SyntaxKind::EqualsToken
                | SyntaxKind::QuestionToken => {
                    return true;
                }
                _ => {
                    return self.can_parse_semicolon();
                }
            }
        }

        false
    }

    pub(super) fn parse_class_static_block_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Id<Node /*ClassStaticBlockDeclaration*/> {
        self.parse_expected_token(SyntaxKind::StaticKeyword, None, None);
        let body: Id<Node> = self.parse_class_static_block_body();
        self.with_jsdoc(
            self.finish_node(
                self.factory()
                    .create_class_static_block_declaration_raw(decorators, modifiers, body),
                pos,
                None,
            )
            .alloc(self),
            has_jsdoc,
        )
    }

    pub(super) fn parse_class_static_block_body(&self) -> Id<Node /*Block*/> {
        let saved_yield_context = self.in_yield_context();
        let saved_await_context = self.in_await_context();

        self.set_yield_context(false);
        self.set_await_context(true);

        let body = self.parse_block(false, None);

        self.set_yield_context(saved_yield_context);
        self.set_await_context(saved_await_context);

        body
    }

    pub(super) fn parse_decorator_expression(&self) -> Id<Node /*LeftHandSideExpression*/> {
        if self.in_await_context() && self.token() == SyntaxKind::AwaitKeyword {
            let pos = self.get_node_pos();
            let await_expression: Id<Node> = self
                .parse_identifier(Some(&Diagnostics::Expression_expected), None)
                .alloc(self);
            self.next_token();
            let member_expression = self.parse_member_expression_rest(pos, await_expression, true);
            return self.parse_call_expression_rest(pos, member_expression);
        }
        self.parse_left_hand_side_expression_or_higher()
    }

    pub(super) fn try_parse_decorator(&self) -> Option<Decorator> {
        let pos = self.get_node_pos();
        if !self.parse_optional(SyntaxKind::AtToken) {
            return None;
        }
        let expression = self.do_in_decorator_context(|| self.parse_decorator_expression());
        Some(self.finish_node(self.factory().create_decorator_raw(expression), pos, None))
    }

    pub(super) fn parse_decorators(&self) -> Option<Gc<NodeArray> /*<Decorator>*/> {
        let pos = self.get_node_pos();
        let mut list: Option<Vec<Id<Node>>> = None;
        loop {
            let decorator = self.try_parse_decorator();
            if decorator.is_none() {
                break;
            }
            let decorator = decorator.unwrap();
            if list.is_none() {
                list = Some(vec![]);
            }
            let list = list.as_mut().unwrap();
            append(list, Some(decorator.alloc(self)));
        }
        list.map(|list| self.create_node_array(list, pos, None, None))
    }

    pub(super) fn try_parse_modifier(
        &self,
        permit_invalid_const_as_modifier: Option<bool>,
        stop_on_start_of_class_static_block: Option<bool>,
        has_seen_static_modifier: Option<bool>,
    ) -> Option<Node /*Modifier*/> {
        let permit_invalid_const_as_modifier = permit_invalid_const_as_modifier.unwrap_or(false);
        let stop_on_start_of_class_static_block =
            stop_on_start_of_class_static_block.unwrap_or(false);
        let has_seen_static_modifier = has_seen_static_modifier.unwrap_or(false);
        let pos = self.get_node_pos();
        let kind = self.token();

        if self.token() == SyntaxKind::ConstKeyword && permit_invalid_const_as_modifier {
            if !self.try_parse_bool(|| self.next_token_is_on_same_line_and_can_follow_modifier()) {
                return None;
            }
        } else if stop_on_start_of_class_static_block
            && self.token() == SyntaxKind::StaticKeyword
            && self.look_ahead_bool(|| self.next_token_is_open_brace())
        {
            return None;
        } else if has_seen_static_modifier && self.token() == SyntaxKind::StaticKeyword {
            return None;
        } else {
            if !self.parse_any_contextual_modifier() {
                return None;
            }
        }

        Some(
            self.finish_node(self.factory().create_token_raw(kind), pos, None)
                .into(),
        )
    }

    pub(super) fn parse_modifiers(
        &self,
        permit_invalid_const_as_modifier: Option<bool>,
        stop_on_start_of_class_static_block: Option<bool>,
    ) -> Option<Gc<NodeArray> /*<Modifier>*/> {
        let pos = self.get_node_pos();
        let mut list: Option<Vec<Id<Node>>> = None;
        let mut has_seen_static = false;
        loop {
            let modifier = self.try_parse_modifier(
                permit_invalid_const_as_modifier,
                stop_on_start_of_class_static_block,
                Some(has_seen_static),
            );
            if modifier.is_none() {
                break;
            }
            let modifier = modifier.unwrap();
            if modifier.kind() == SyntaxKind::StaticKeyword {
                has_seen_static = true;
            }
            if list.is_none() {
                list = Some(vec![]);
            }
            let list = list.as_mut().unwrap();
            append(list, Some(modifier.alloc(self)));
        }
        list.map(|list| self.create_node_array(list, pos, None, None))
    }

    pub(super) fn parse_modifiers_for_arrow_function(
        &self,
    ) -> Option<Gc<NodeArray> /*<Modifier>*/> {
        let mut modifiers: Option<Gc<NodeArray>> = None;
        if self.token() == SyntaxKind::AsyncKeyword {
            let pos = self.get_node_pos();
            self.next_token();
            let modifier = self
                .finish_node(
                    self.factory().create_token_raw(SyntaxKind::AsyncKeyword),
                    pos,
                    None,
                )
                .alloc(self);
            modifiers = Some(self.create_node_array(vec![modifier], pos, None, None));
        }
        modifiers
    }

    pub(super) fn parse_class_element(&self) -> Id<Node /*ClassElement*/> {
        let pos = self.get_node_pos();
        if self.token() == SyntaxKind::SemicolonToken {
            self.next_token();
            return self
                .finish_node(
                    self.factory().create_semicolon_class_element_raw(),
                    pos,
                    None,
                )
                .alloc(self);
        }

        let has_jsdoc = self.has_preceding_jsdoc_comment();
        let decorators = self.parse_decorators();
        let modifiers = self.parse_modifiers(Some(true), Some(true));
        if self.token() == SyntaxKind::StaticKeyword
            && self.look_ahead_bool(|| self.next_token_is_open_brace())
        {
            return self
                .parse_class_static_block_declaration(pos, has_jsdoc, decorators, modifiers);
        }

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

        if matches!(
            self.token(),
            SyntaxKind::ConstructorKeyword | SyntaxKind::StringLiteral
        ) {
            let constructor_declaration = self.try_parse_constructor_declaration(
                pos,
                has_jsdoc,
                decorators.clone(),
                modifiers.clone(),
            );
            if let Some(constructor_declaration) = constructor_declaration {
                return constructor_declaration;
            }
        }

        if self.is_index_signature() {
            return self.parse_index_signature_declaration(pos, has_jsdoc, decorators, modifiers);
        }

        if token_is_identifier_or_keyword(self.token())
            || matches!(
                self.token(),
                SyntaxKind::StringLiteral
                    | SyntaxKind::NumericLiteral
                    | SyntaxKind::AsteriskToken
                    | SyntaxKind::OpenBracketToken
            )
        {
            let is_ambient = some(
                modifiers.as_ref().map(|node_array| {
                    let node_array: &[Id<Node>] = node_array;
                    node_array
                }),
                Some(|&modifier: &Id<Node>| self.is_declare_modifier(modifier)),
            );
            if is_ambient {
                for m in modifiers.as_ref().unwrap() {
                    m.ref_(self).set_flags(m.ref_(self).flags() | NodeFlags::Ambient);
                }
                return self.do_inside_of_context(NodeFlags::Ambient, || {
                    self.parse_property_or_method_declaration(pos, has_jsdoc, decorators, modifiers)
                });
            } else {
                return self
                    .parse_property_or_method_declaration(pos, has_jsdoc, decorators, modifiers);
            }
        }

        if decorators.is_some() || modifiers.is_some() {
            let name: Id<Node> = self
                .create_missing_node(
                    SyntaxKind::Identifier,
                    true,
                    Some(&Diagnostics::Declaration_expected),
                    None,
                )
                .alloc(self);
            return self
                .parse_property_declaration(pos, has_jsdoc, decorators, modifiers, name, None);
        }

        Debug_.fail(Some(
            "Should not have attempted to parse class member declaration.",
        ))
    }

    pub(super) fn parse_class_expression(&self) -> Id<Node /*ClassExpression*/> {
        self.parse_class_declaration_or_expression(
            self.get_node_pos(),
            self.has_preceding_jsdoc_comment(),
            None,
            None,
            SyntaxKind::ClassExpression,
        )
    }

    pub(super) fn parse_class_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Id<Node /*ClassDeclaration*/> {
        self.parse_class_declaration_or_expression(
            pos,
            has_jsdoc,
            decorators,
            modifiers,
            SyntaxKind::ClassDeclaration,
        )
    }
}
