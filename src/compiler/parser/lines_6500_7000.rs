#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::{ParserType, ParsingContext, SignatureFlags};
use crate::{
    append, is_class_member_modifier, is_keyword, is_modifier_kind, modifiers_to_flags,
    ArrayBindingPattern, ClassDeclaration, ClassExpression, Debug_, Decorator, DiagnosticMessage,
    Diagnostics, FunctionDeclaration, MethodDeclaration, ModifierFlags, Node, NodeArray, NodeFlags,
    NodeInterface, ObjectBindingPattern, SyntaxKind, VariableDeclaration, VariableDeclarationList,
};

impl ParserType {
    pub(super) fn parse_object_binding_pattern(&self) -> ObjectBindingPattern {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBraceToken, None, None);
        let elements = self.parse_delimited_list(
            ParsingContext::ObjectBindingElements,
            || self.parse_object_binding_element().into(),
            None,
        );
        self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        self.finish_node(
            self.factory.create_object_binding_pattern(self, elements),
            pos,
            None,
        )
    }

    pub(super) fn parse_array_binding_pattern(&self) -> ArrayBindingPattern {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBracketToken, None, None);
        let elements = self.parse_delimited_list(
            ParsingContext::ArrayBindingElements,
            || self.parse_array_binding_element().into(),
            None,
        );
        self.parse_expected(SyntaxKind::CloseBracketToken, None, None);
        self.finish_node(
            self.factory.create_array_binding_pattern(self, elements),
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
    ) -> Rc<Node /*Identifier | BindingPattern*/> {
        if self.token() == SyntaxKind::OpenBracketToken {
            return self.parse_array_binding_pattern().into();
        }
        if self.token() == SyntaxKind::OpenBraceToken {
            return self.parse_object_binding_pattern().into();
        }
        self.parse_binding_identifier(private_identifier_diagnostic_message)
            .wrap()
    }

    pub(super) fn parse_variable_declaration_allow_exclamation(&self) -> VariableDeclaration {
        self.parse_variable_declaration(Some(true))
    }

    pub(super) fn parse_variable_declaration(
        &self,
        allow_exclamation: Option<bool>,
    ) -> VariableDeclaration {
        let allow_exclamation = allow_exclamation.unwrap_or(false);
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        let name = self.parse_identifier_or_pattern(Some(
            &Diagnostics::Private_identifiers_are_not_allowed_in_variable_declarations,
        ));
        let mut exclamation_token: Option<Rc<Node>> = None;
        if allow_exclamation
            && name.kind() == SyntaxKind::Identifier
            && self.token() == SyntaxKind::ExclamationToken
            && !self.scanner().has_preceding_line_break()
        {
            exclamation_token = Some(self.parse_token_node().into());
        }
        let type_ = self.parse_type_annotation();
        let initializer = if self.is_in_or_of_keyword(self.token()) {
            None
        } else {
            self.parse_initializer()
        };
        let node = self.factory.create_variable_declaration(
            self,
            Some(name),
            exclamation_token,
            type_.map(Node::wrap),
            initializer,
        );
        self.with_jsdoc(self.finish_node(node, pos, None), has_jsdoc)
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
                flags |= NodeFlags::Const;
            }
            SyntaxKind::ConstKeyword => {
                flags |= NodeFlags::Const;
            }
            _ => Debug_.fail(None),
        }

        self.next_token();

        let declarations: NodeArray;
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
                        self.parse_variable_declaration(None).into()
                    } else {
                        self.parse_variable_declaration_allow_exclamation().into()
                    }
                },
                None,
            );

            self.set_disallow_in_context(saved_disallow_in);
        }

        self.finish_node(
            self.factory
                .create_variable_declaration_list(self, declarations, Some(flags)),
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
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> Node {
        let declaration_list = self.parse_variable_declaration_list(false);
        self.parse_semicolon();
        let node = self.factory.create_variable_statement(
            self,
            modifiers,
            Into::<Rc<Node>>::into(declaration_list),
        );
        node.set_decorators(decorators);
        self.finish_node(node.into(), pos, None)
    }

    pub(super) fn parse_function_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> FunctionDeclaration {
        let saved_await_context = self.in_await_context();

        let modifier_flags = modifiers_to_flags(modifiers.as_ref());
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
        let node = self.factory.create_function_declaration(
            self,
            decorators,
            modifiers,
            asterisk_token.map(Into::into),
            name.map(|name| name.wrap()),
            type_parameters,
            parameters,
            type_.map(|type_| type_.wrap()),
            body.map(Into::into),
        );
        self.finish_node(node, pos, None)
    }

    pub(super) fn parse_method_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        asterisk_token: Option<Rc<Node /*AsteriskToken*/>>,
        name: Rc<Node /*PropertyName*/>,
        question_token: Option<Rc<Node /*QuestionToken*/>>,
        exclamation_token: Option<Rc<Node /*ExclamationToken*/>>,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> MethodDeclaration {
        unimplemented!()
    }

    pub(super) fn parse_accessor_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        kind: SyntaxKind, /*AccessorDeclaration["kind"]*/
    ) -> Node /*AccessorDeclaration*/ {
        unimplemented!()
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

    pub(super) fn try_parse_decorator(&self) -> Option<Decorator> {
        let pos = self.get_node_pos();
        if !self.parse_optional(SyntaxKind::AtToken) {
            return None;
        }
        unimplemented!()
    }

    pub(super) fn parse_decorators(&self) -> Option<NodeArray /*<Decorator>*/> {
        let pos = self.get_node_pos();
        let mut list: Option<Vec<Rc<Node>>> = None;
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
            append(list, Some(decorator.into()));
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
            unimplemented!()
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
            self.finish_node(self.factory.create_token(self, kind), pos, None)
                .into(),
        )
    }

    pub(super) fn parse_modifiers(
        &self,
        permit_invalid_const_as_modifier: Option<bool>,
        stop_on_start_of_class_static_block: Option<bool>,
    ) -> Option<NodeArray /*<Modifier>*/> {
        let pos = self.get_node_pos();
        let mut list: Option<Vec<Rc<Node>>> = None;
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
            append(list, Some(modifier.wrap()));
        }
        list.map(|list| self.create_node_array(list, pos, None, None))
    }

    pub(super) fn parse_modifiers_for_arrow_function(&self) -> Option<NodeArray /*<Modifier>*/> {
        unimplemented!()
    }

    pub(super) fn parse_class_expression(&self) -> ClassExpression {
        unimplemented!()
    }

    pub(super) fn parse_class_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> ClassDeclaration {
        unimplemented!()
    }
}
