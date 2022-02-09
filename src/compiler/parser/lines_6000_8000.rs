#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::rc::Rc;

use super::{ParserType, SignatureFlags};
use crate::{
    append, for_each, for_each_child_returns, is_class_member_modifier, is_export_assignment,
    is_export_declaration, is_external_module_reference, is_import_declaration,
    is_import_equals_declaration, is_keyword, is_meta_property, is_modifier_kind,
    modifiers_to_flags, some, BaseNode, BaseNodeFactory, Block, Debug_, Decorator, Diagnostic,
    DiagnosticMessage, Diagnostics, FunctionDeclaration, InterfaceDeclaration, ModifierFlags, Node,
    NodeArray, NodeFlags, NodeInterface, SyntaxKind, TypeAliasDeclaration, VariableDeclaration,
    VariableDeclarationList,
};

impl ParserType {
    pub(super) fn parse_expression_or_labeled_statement(&self) -> Node {
        let pos = self.get_node_pos();
        let expression = self.parse_expression();
        let node: Node = if false {
            unimplemented!()
        } else {
            if !self.try_parse_semicolon() {
                self.parse_error_for_missing_semicolon_after(&expression);
            }
            self.factory
                .create_expression_statement(self, expression.wrap())
                .into()
        };
        self.finish_node(node, pos, None)
    }

    pub(super) fn next_token_is_class_keyword_on_same_line(&self) -> bool {
        self.next_token();
        self.token() == SyntaxKind::ClassKeyword && !self.scanner().has_preceding_line_break()
    }

    pub(super) fn next_token_is_function_keyword_on_same_line(&self) -> bool {
        self.next_token();
        self.token() == SyntaxKind::FunctionKeyword && !self.scanner().has_preceding_line_break()
    }

    pub(super) fn is_declaration(&self) -> bool {
        loop {
            match self.token() {
                SyntaxKind::VarKeyword | SyntaxKind::ConstKeyword => {
                    return true;
                }
                SyntaxKind::InterfaceKeyword | SyntaxKind::TypeKeyword => {
                    return self.next_token_is_identifier_on_same_line();
                }
                SyntaxKind::DeclareKeyword => {
                    self.next_token();
                    if self.scanner().has_preceding_line_break() {
                        return false;
                    }
                    continue;
                }
                _ => unimplemented!(),
            }
        }
    }

    pub(super) fn is_start_of_declaration(&self) -> bool {
        self.look_ahead_bool(|| self.is_declaration())
    }

    pub(super) fn is_start_of_statement(&self) -> bool {
        match self.token() {
            SyntaxKind::SemicolonToken
            | SyntaxKind::VarKeyword
            | SyntaxKind::FunctionKeyword
            | SyntaxKind::IfKeyword
            | SyntaxKind::ReturnKeyword => true,
            SyntaxKind::ConstKeyword => self.is_start_of_declaration(),
            SyntaxKind::DeclareKeyword | SyntaxKind::InterfaceKeyword | SyntaxKind::TypeKeyword => {
                true
            }
            _ => self.is_start_of_expression(),
        }
    }

    pub(super) fn parse_statement(&self) -> Node {
        match self.token() {
            SyntaxKind::SemicolonToken => return self.parse_empty_statement(),
            SyntaxKind::VarKeyword => {
                return self.parse_variable_statement(self.get_node_pos(), None, None)
            }
            SyntaxKind::FunctionKeyword => {
                return self
                    .parse_function_declaration(self.get_node_pos(), None, None)
                    .into()
            }
            SyntaxKind::OpenBraceToken => return self.parse_block(false, None).into(),
            SyntaxKind::IfKeyword => return self.parse_if_statement(),
            SyntaxKind::ReturnKeyword => return self.parse_return_statement().into(),
            SyntaxKind::ConstKeyword => {
                if self.is_start_of_declaration() {
                    return self.parse_declaration();
                }
            }
            SyntaxKind::DeclareKeyword | SyntaxKind::InterfaceKeyword | SyntaxKind::TypeKeyword => {
                if self.is_start_of_declaration() {
                    return self.parse_declaration();
                }
            }
            _ => (),
        }
        self.parse_expression_or_labeled_statement()
    }

    pub(super) fn is_declare_modifier(&self, modifier: &Node /*Modifier*/) -> bool {
        modifier.kind() == SyntaxKind::DeclareKeyword
    }

    pub(super) fn parse_declaration(&self) -> Node {
        let is_ambient = some(
            self.look_ahead(|| {
                self.parse_decorators();
                self.parse_modifiers(None, None)
            })
            .as_ref()
            .map(|node_array| {
                let node_array: &[Rc<Node>] = node_array;
                node_array
            }),
            Some(|modifier: &Rc<Node>| self.is_declare_modifier(&**modifier)),
        );
        if is_ambient {
            let node = self.try_reuse_ambient_declaration();
            if let Some(node) = node {
                return node;
            }
        }

        let pos = self.get_node_pos();
        let decorators = self.parse_decorators();
        let modifiers = self.parse_modifiers(None, None);
        if is_ambient {
            for m in modifiers.as_ref().unwrap() {
                m.set_flags(m.flags() | NodeFlags::Ambient);
            }
            self.do_inside_of_context(NodeFlags::Ambient, move || {
                self.parse_declaration_worker(pos, decorators, modifiers)
            })
        } else {
            self.parse_declaration_worker(pos, decorators, modifiers)
        }
    }

    pub(super) fn try_reuse_ambient_declaration(&self) -> Option<Node> {
        None
    }

    pub(super) fn parse_declaration_worker(
        &self,
        pos: isize,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> Node {
        match self.token() {
            SyntaxKind::VarKeyword | SyntaxKind::ConstKeyword => {
                self.parse_variable_statement(pos, decorators, modifiers)
            }
            SyntaxKind::InterfaceKeyword => self
                .parse_interface_declaration(pos, decorators, modifiers)
                .into(),
            SyntaxKind::TypeKeyword => self
                .parse_type_alias_declaration(pos, decorators, modifiers)
                .into(),
            _ => unimplemented!(),
        }
    }

    pub(super) fn parse_function_block_or_semicolon(
        &self,
        flags: SignatureFlags,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Option<Block> {
        if self.token() != SyntaxKind::OpenBraceToken && self.can_parse_semicolon() {
            self.parse_semicolon();
            return None;
        }

        Some(self.parse_function_block(flags, diagnostic_message))
    }

    pub(super) fn is_binding_identifier_or_private_identifier_or_pattern(&self) -> bool {
        self.is_binding_identifier()
    }

    pub(super) fn parse_identifier_or_pattern(
        &self,
        private_identifier_diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Rc<Node> {
        self.parse_binding_identifier(private_identifier_diagnostic_message)
            .wrap()
    }

    pub(super) fn parse_variable_declaration_no_exclamation(&self) -> VariableDeclaration {
        self.parse_variable_declaration(Some(false))
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
        let name = self.parse_identifier_or_pattern(Some(
            &Diagnostics::Private_identifiers_are_not_allowed_in_variable_declarations,
        ));
        let mut exclamation_token: Option<BaseNode> = None;
        if allow_exclamation
            && name.kind() == SyntaxKind::Identifier
            && self.token() == SyntaxKind::ExclamationToken
            && !self.scanner().has_preceding_line_break()
        {
            exclamation_token = Some(self.parse_token_node());
        }
        let type_ = self.parse_type_annotation();
        let initializer = if false {
            None
        } else {
            self.parse_initializer()
        };
        let node = self.factory.create_variable_declaration(
            self,
            Some(name),
            exclamation_token.map(Into::into),
            type_.map(|type_| type_.wrap()),
            initializer.map(|initializer| initializer.wrap()),
        );
        self.finish_node(node, pos, None)
    }

    pub(super) fn parse_variable_declaration_list(&self) -> VariableDeclarationList {
        let pos = self.get_node_pos();

        let mut flags = NodeFlags::None;
        match self.token() {
            SyntaxKind::VarKeyword => (),
            SyntaxKind::ConstKeyword => {
                flags |= NodeFlags::Const;
            }
            _ => Debug_.fail(None),
        }

        self.next_token();

        let declarations: NodeArray;
        if false {
            unimplemented!()
        } else {
            declarations = self.parse_delimited_list(
                ParsingContext::VariableDeclarations,
                ParserType::parse_variable_declaration_allow_exclamation,
                None,
            );
        }

        self.finish_node(
            self.factory
                .create_variable_declaration_list(self, declarations, Some(flags)),
            pos,
            None,
        )
    }

    pub(super) fn parse_variable_statement(
        &self,
        pos: isize,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> Node {
        let declaration_list = self.parse_variable_declaration_list();
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
        let mut list: Option<Vec<Node>> = None;
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
        let mut list: Option<Vec<Node>> = None;
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
            append(list, Some(modifier));
        }
        list.map(|list| self.create_node_array(list, pos, None, None))
    }

    pub(super) fn parse_heritage_clauses(&self) -> Option<NodeArray /*<HeritageClause>*/> {
        None
    }

    pub(super) fn is_heritage_clause(&self) -> bool {
        matches!(
            self.token(),
            SyntaxKind::ExtendsKeyword | SyntaxKind::ImplementsKeyword
        )
    }

    pub(super) fn parse_interface_declaration(
        &self,
        pos: isize,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> InterfaceDeclaration {
        self.parse_expected(SyntaxKind::InterfaceKeyword, None, None);
        let name = self.parse_identifier(None, None);
        let type_parameters = self.parse_type_parameters();
        let heritage_clauses = self.parse_heritage_clauses();
        let members = self.parse_object_type_members();
        let node = self.factory.create_interface_declaration(
            self,
            decorators,
            modifiers,
            name.wrap(),
            type_parameters,
            heritage_clauses,
            members,
        );
        self.finish_node(node, pos, None)
    }

    pub(super) fn parse_type_alias_declaration(
        &self,
        pos: isize,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> TypeAliasDeclaration {
        self.parse_expected(SyntaxKind::TypeKeyword, None, None);
        let name = self.parse_identifier(None, None);
        let type_parameters = self.parse_type_parameters();
        self.parse_expected(SyntaxKind::EqualsToken, None, None);
        let type_ = if false {
            unimplemented!()
        } else {
            self.parse_type()
        };
        self.parse_semicolon();
        let node = self.factory.create_type_alias_declaration(
            self,
            decorators,
            modifiers,
            name.wrap(),
            type_parameters,
            type_.wrap(),
        );
        self.finish_node(node, pos, None)
    }

    pub(super) fn next_token_is_open_brace(&self) -> bool {
        self.next_token() == SyntaxKind::OpenBraceToken
    }

    pub(super) fn set_external_module_indicator(&self, source_file: &Node /*SourceFile*/) {
        let source_file_as_source_file = source_file.as_source_file();
        source_file_as_source_file.set_external_module_indicator(
            for_each(&source_file_as_source_file.statements, |statement, _| {
                self.is_an_external_module_indicator_node(statement)
            })
            .or_else(|| self.get_import_meta_if_necessary(source_file)),
        );
    }

    pub(super) fn is_an_external_module_indicator_node(&self, node: &Node) -> Option<Rc<Node>> {
        if self.has_modifier_of_kind(node, SyntaxKind::ExportKeyword)
            || is_import_equals_declaration(node)
                && is_external_module_reference(
                    &node.as_import_equals_declaration().module_reference,
                )
            || is_import_declaration(node)
            || is_export_assignment(node)
            || is_export_declaration(node)
        {
            Some(node.node_wrapper())
        } else {
            None
        }
    }

    pub(super) fn get_import_meta_if_necessary(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Option<Rc<Node>> {
        if source_file
            .flags()
            .intersects(NodeFlags::PossiblyContainsImportMeta)
        {
            self.walk_tree_for_external_module_indicators(source_file)
        } else {
            None
        }
    }

    pub(super) fn walk_tree_for_external_module_indicators(&self, node: &Node) -> Option<Rc<Node>> {
        if self.is_import_meta(node) {
            Some(node.node_wrapper())
        } else {
            for_each_child_returns(
                node,
                |child| self.walk_tree_for_external_module_indicators(child),
                Option::<fn(&NodeArray) -> Option<Rc<Node>>>::None,
            )
        }
    }

    pub(super) fn has_modifier_of_kind(&self, node: &Node, kind: SyntaxKind) -> bool {
        let modifiers: Option<&[Rc<Node>]> = node.maybe_modifiers().map(|node_array| {
            let slice_ref: &[Rc<Node>] = node_array;
            slice_ref
        });
        some(modifiers, Some(|m: &Rc<Node>| m.kind() == kind))
    }

    pub(super) fn is_import_meta(&self, node: &Node) -> bool {
        if !is_meta_property(node) {
            return false;
        }
        let node_as_meta_property = node.as_meta_property();
        node_as_meta_property.keyword_token == SyntaxKind::ImportKeyword
            && node_as_meta_property
                .name
                .as_identifier()
                .escaped_text
                .eq_str("meta")
    }

    pub fn JSDocParser_parse_jsdoc_type_expression_for_tests(
        &self,
        content: String,
        start: Option<usize>,
        length: Option<usize>,
    ) -> Option<ParsedJSDocTypeExpression> {
        unimplemented!()
    }

    pub fn JSDocParser_parse_isolated_jsdoc_comment(
        &self,
        content: String,
        start: Option<usize>,
        length: Option<usize>,
    ) -> Option<ParsedIsolatedJSDocComment> {
        unimplemented!()
    }

    pub fn JSDocParser_parse_jsdoc_comment<TNode: NodeInterface>(
        &self,
        parent: &TNode,
        start: usize,
        length: usize,
    ) -> Option<Rc<Node /*JSDoc*/>> {
        unimplemented!()
    }
}

pub struct ParsedJSDocTypeExpression {
    pub js_doc_type_expression: Rc<Node /*JSDocTypeExpression*/>,
    pub diagnostics: Vec<Rc<Diagnostic>>,
}

pub struct ParsedIsolatedJSDocComment {
    pub js_doc: Rc<Node /*JSDoc*/>,
    pub diagnostics: Vec<Rc<Diagnostic>>,
}

impl BaseNodeFactory for ParserType {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.SourceFileConstructor()(kind, 0, 0))
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.IdentifierConstructor()(kind, 0, 0))
    }

    fn create_base_private_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.PrivateIdentifierConstructor()(kind, 0, 0))
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.TokenConstructor()(kind, 0, 0))
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.NodeConstructor()(kind, 0, 0))
    }
}

// lazy_static! {
//     static ref ParserMut: Mutex<ParserType> = Mutex::new(ParserType::new());
// }

#[allow(non_snake_case)]
pub(super) fn Parser() -> ParserType {
    ParserType::new()
}
// fn Parser() -> MutexGuard<'static, ParserType> {
//     ParserMut.lock().unwrap()
// }

bitflags! {
    pub struct ParsingContext: u32 {
        const None = 0;
        const SourceElements = 1 << 0;
        const BlockStatements = 1 << 1;
        const SwitchClauses = 1 << 2;
        const SwitchClauseStatements = 1 << 3;
        const TypeMembers = 1 << 4;
        const ClassMembers = 1 << 5;
        const EnumMembers = 1 << 6;
        const HeritageClauseElement = 1 << 7;
        const VariableDeclarations = 1 << 8;
        const ObjectBindingElements = 1 << 9;
        const ArrayBindingElements = 1 << 10;
        const ArgumentExpressions = 1 << 11;
        const ObjectLiteralMembers = 1 << 12;
        const JsxAttributes = 1 << 13;
        const JsxChildren = 1 << 14;
        const ArrayLiteralMembers = 1 << 15;
        const Parameters = 1 << 16;
        const JSDocParameters = 1 << 17;
        const RestProperties = 1 << 18;
        const TypeParameters = 1 << 19;
        const TypeArguments = 1 << 20;
        const TupleElementTypes = 1 << 21;
        const HeritageClauses = 1 << 22;
        const ImportOrExportSpecifiers = 1 << 23;
        const AssertEntries = 1 << 24;
        const Count = 1 << 25;
    }
}
