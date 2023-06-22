use gc::Gc;
use std::convert::TryInto;

use super::{ParserType, ParsingContext};
use crate::{
    add_related_info, create_detached_diagnostic, is_export_modifier, is_keyword,
    last_or_undefined, some, token_is_identifier_or_keyword, AssertClause, AssertEntry, Debug_,
    DiagnosticRelatedInformationInterface, Diagnostics, ExpressionWithTypeArguments,
    ExternalModuleReference, HeritageClause, ImportClause, ModuleBlock, NamespaceExport,
    NamespaceImport, Node, NodeArray, NodeFlags, NodeInterface, SyntaxKind,
};

impl ParserType {
    pub(super) fn parse_class_declaration_or_expression(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
        kind: SyntaxKind, /*ClassLikeDeclaration["kind"]*/
    ) -> Gc<Node /*ClassLikeDeclaration*/> {
        let saved_await_context = self.in_await_context();
        self.parse_expected(SyntaxKind::ClassKeyword, None, None);

        let name: Option<Gc<Node>> = self
            .parse_name_of_class_declaration_or_expression()
            .map(Node::wrap);
        let type_parameters = self.parse_type_parameters();
        if some(
            modifiers.as_ref().map(|node_array| {
                let node_array: &[Gc<Node>] = node_array;
                node_array
            }),
            Some(|modifier: &Gc<Node>| is_export_modifier(modifier)),
        ) {
            self.set_await_context(true);
        }
        let heritage_clauses = self.parse_heritage_clauses();

        let members: Gc<NodeArray>;
        if self.parse_expected(SyntaxKind::OpenBraceToken, None, None) {
            members = self.parse_class_members();
            self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        } else {
            members = self.create_missing_list();
        }
        self.set_await_context(saved_await_context);
        let node: Node = if kind == SyntaxKind::ClassDeclaration {
            self.factory()
                .create_class_declaration_raw(
                    decorators,
                    modifiers,
                    name,
                    type_parameters,
                    heritage_clauses,
                    members,
                )
                .into()
        } else {
            self.factory()
                .create_class_expression_raw(
                    decorators,
                    modifiers,
                    name,
                    type_parameters,
                    heritage_clauses,
                    members,
                )
                .into()
        };
        self.with_jsdoc(self.finish_node(node, pos, None).wrap(), has_jsdoc)
    }

    pub(super) fn parse_name_of_class_declaration_or_expression(
        &self,
    ) -> Option<Node /*Identifier*/> {
        if self.is_binding_identifier() && !self.is_implements_clause() {
            Some(self.create_identifier(self.is_binding_identifier(), None, None))
        } else {
            None
        }
    }

    pub(super) fn is_implements_clause(&self) -> bool {
        self.token() == SyntaxKind::ImplementsKeyword
            && self.look_ahead_bool(|| self.next_token_is_identifier_or_keyword())
    }

    pub(super) fn parse_heritage_clauses(&self) -> Option<Gc<NodeArray> /*<HeritageClause>*/> {
        if self.is_heritage_clause() {
            return Some(self.parse_list(ParsingContext::HeritageClauses, &mut || {
                self.parse_heritage_clause().wrap()
            }));
        }

        None
    }

    pub(super) fn parse_heritage_clause(&self) -> HeritageClause {
        let pos = self.get_node_pos();
        let tok = self.token();
        Debug_.assert(
            matches!(
                tok,
                SyntaxKind::ExtendsKeyword | SyntaxKind::ImplementsKeyword
            ),
            None,
        );
        self.next_token();
        let types = self.parse_delimited_list(
            ParsingContext::HeritageClauseElement,
            || self.parse_expression_with_type_arguments().wrap(),
            None,
        );
        self.finish_node(self.factory().create_heritage_clause_raw(tok, types), pos, None)
    }

    pub(super) fn parse_expression_with_type_arguments(&self) -> ExpressionWithTypeArguments {
        let pos = self.get_node_pos();
        let expression = self.parse_left_hand_side_expression_or_higher();
        let type_arguments = self.try_parse_type_arguments();
        self.finish_node(
            self.factory()
                .create_expression_with_type_arguments_raw(expression, type_arguments),
            pos,
            None,
        )
    }

    pub(super) fn try_parse_type_arguments(&self) -> Option<Gc<NodeArray> /*<TypeNode>*/> {
        if self.token() == SyntaxKind::LessThanToken {
            Some(self.parse_bracketed_list(
                ParsingContext::TypeArguments,
                || self.parse_type(),
                SyntaxKind::LessThanToken,
                SyntaxKind::GreaterThanToken,
            ))
        } else {
            None
        }
    }

    pub(super) fn is_heritage_clause(&self) -> bool {
        matches!(
            self.token(),
            SyntaxKind::ExtendsKeyword | SyntaxKind::ImplementsKeyword
        )
    }

    pub(super) fn parse_class_members(&self) -> Gc<NodeArray> /*<ClassElement>*/ {
        self.parse_list(ParsingContext::ClassMembers, &mut || {
            self.parse_class_element()
        })
    }

    pub(super) fn parse_interface_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Gc<Node /*InterfaceDeclaration*/> {
        self.parse_expected(SyntaxKind::InterfaceKeyword, None, None);
        let name = self.parse_identifier(None, None);
        let type_parameters = self.parse_type_parameters();
        let heritage_clauses = self.parse_heritage_clauses();
        let members = self.parse_object_type_members();
        let node = self.factory().create_interface_declaration_raw(
            decorators,
            modifiers,
            name.wrap(),
            type_parameters,
            heritage_clauses,
            members,
        );
        self.with_jsdoc(self.finish_node(node, pos, None).wrap(), has_jsdoc)
    }

    pub(super) fn parse_type_alias_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Gc<Node /*TypeAliasDeclaration*/> {
        self.parse_expected(SyntaxKind::TypeKeyword, None, None);
        let name = self.parse_identifier(None, None);
        let type_parameters = self.parse_type_parameters();
        self.parse_expected(SyntaxKind::EqualsToken, None, None);
        let type_: Gc<Node> = if self.token() == SyntaxKind::IntrinsicKeyword {
            self.try_parse(|| self.parse_keyword_and_no_dot().map(Node::wrap))
                .unwrap_or_else(|| self.parse_type())
        } else {
            self.parse_type()
        };
        self.parse_semicolon();
        let node = self.factory().create_type_alias_declaration_raw(
            decorators,
            modifiers,
            name.wrap(),
            type_parameters,
            type_,
        );
        self.with_jsdoc(self.finish_node(node, pos, None).wrap(), has_jsdoc)
    }

    pub(super) fn parse_enum_member(&self) -> Gc<Node /*EnumMember*/> {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        let name = self.parse_property_name().wrap();
        let initializer = self.allow_in_and(|| self.parse_initializer());
        self.with_jsdoc(
            self.finish_node(
                self.factory().create_enum_member_raw(name, initializer),
                pos,
                None,
            )
            .wrap(),
            has_jsdoc,
        )
    }

    pub(super) fn parse_enum_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Gc<Node /*EnumDeclaration*/> {
        self.parse_expected(SyntaxKind::EnumKeyword, None, None);
        let name: Gc<Node> = self.parse_identifier(None, None).wrap();
        let members: Gc<NodeArray>;
        if self.parse_expected(SyntaxKind::OpenBraceToken, None, None) {
            members = self.do_outside_of_yield_and_await_context(|| {
                self.parse_delimited_list(
                    ParsingContext::EnumMembers,
                    || self.parse_enum_member(),
                    None,
                )
            });
            self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        } else {
            members = self.create_missing_list();
        }
        let node =
            self.factory()
                .create_enum_declaration_raw(decorators, modifiers, name, Some(members));
        self.with_jsdoc(self.finish_node(node, pos, None).wrap(), has_jsdoc)
    }

    pub(super) fn parse_module_block(&self) -> ModuleBlock {
        let pos = self.get_node_pos();
        let statements: Gc<NodeArray>;
        if self.parse_expected(SyntaxKind::OpenBraceToken, None, None) {
            statements = self.parse_list(ParsingContext::BlockStatements, &mut || {
                self.parse_statement()
            });
            self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        } else {
            statements = self.create_missing_list();
        }
        self.finish_node(
            self.factory().create_module_block_raw(Some(statements)),
            pos,
            None,
        )
    }

    pub(super) fn parse_module_or_namespace_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
        flags: NodeFlags,
    ) -> Gc<Node /*ModuleDeclaration*/> {
        let namespace_flag = flags & NodeFlags::Namespace;
        let name = self.parse_identifier(None, None).wrap();
        let body = if self.parse_optional(SyntaxKind::DotToken) {
            self.parse_module_or_namespace_declaration(
                self.get_node_pos(),
                false,
                None,
                None,
                NodeFlags::NestedNamespace | namespace_flag,
            )
        } else {
            self.parse_module_block().wrap()
        };
        let node = self.factory().create_module_declaration_raw(
            decorators,
            modifiers,
            name,
            Some(body),
            Some(flags),
        );
        self.with_jsdoc(self.finish_node(node, pos, None).wrap(), has_jsdoc)
    }

    pub(super) fn parse_ambient_external_module_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Gc<Node /*ModuleDeclaration*/> {
        let mut flags = NodeFlags::None;
        let name: Gc<Node>;
        if self.token() == SyntaxKind::GlobalKeyword {
            name = self.parse_identifier(None, None).wrap();
            flags |= NodeFlags::GlobalAugmentation;
        } else {
            name = self.parse_literal_node().wrap();
            let name_as_literal_like_node = name.as_literal_like_node();
            let text = self.intern_identifier(&name_as_literal_like_node.text());
            name_as_literal_like_node.set_text(text);
        }
        let mut body: Option<Gc<Node>> = None;
        if self.token() == SyntaxKind::OpenBraceToken {
            body = Some(self.parse_module_block().wrap());
        } else {
            self.parse_semicolon();
        }
        let node = self.factory().create_module_declaration_raw(
            decorators,
            modifiers,
            name,
            body,
            Some(flags),
        );
        self.with_jsdoc(self.finish_node(node, pos, None).wrap(), has_jsdoc)
    }

    pub(super) fn parse_module_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Gc<Node /*ModuleDeclaration*/> {
        let mut flags = NodeFlags::None;
        if self.token() == SyntaxKind::GlobalKeyword {
            return self
                .parse_ambient_external_module_declaration(pos, has_jsdoc, decorators, modifiers);
        } else if self.parse_optional(SyntaxKind::NamespaceKeyword) {
            flags |= NodeFlags::Namespace;
        } else {
            self.parse_expected(SyntaxKind::ModuleKeyword, None, None);
            if self.token() == SyntaxKind::StringLiteral {
                return self.parse_ambient_external_module_declaration(
                    pos, has_jsdoc, decorators, modifiers,
                );
            }
        }
        self.parse_module_or_namespace_declaration(pos, has_jsdoc, decorators, modifiers, flags)
    }

    pub(super) fn is_external_module_reference(&self) -> bool {
        self.token() == SyntaxKind::RequireKeyword
            && self.look_ahead_bool(|| self.next_token_is_open_paren())
    }

    pub(super) fn next_token_is_open_paren(&self) -> bool {
        self.next_token() == SyntaxKind::OpenParenToken
    }

    pub(super) fn next_token_is_open_brace(&self) -> bool {
        self.next_token() == SyntaxKind::OpenBraceToken
    }

    pub(super) fn next_token_is_slash(&self) -> bool {
        self.next_token() == SyntaxKind::SlashToken
    }

    pub(super) fn parse_namespace_export_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Gc<Node /*NamespaceExportDeclaration*/> {
        self.parse_expected(SyntaxKind::AsKeyword, None, None);
        self.parse_expected(SyntaxKind::NamespaceKeyword, None, None);
        let name: Gc<Node> = self.parse_identifier(None, None).wrap();
        self.parse_semicolon();
        let node = self.factory().create_namespace_export_declaration_raw(name);
        node.set_decorators(decorators);
        node.set_modifiers(modifiers);
        self.with_jsdoc(self.finish_node(node, pos, None).wrap(), has_jsdoc)
    }

    pub(super) fn parse_import_declaration_or_import_equals_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Gc<Node /*ImportEqualsDeclaration | ImportDeclaration*/> {
        self.parse_expected(SyntaxKind::ImportKeyword, None, None);

        let after_import_pos = self.scanner().get_start_pos();

        let mut identifier: Option<Gc<Node /*Identifier*/>> = None;
        if self.is_identifier() {
            identifier = Some(self.parse_identifier(None, None).wrap());
        }

        let mut is_type_only = false;
        if self.token() != SyntaxKind::FromKeyword
            && matches!(identifier.as_ref(), Some(identifier) if identifier.as_identifier().escaped_text == "type")
            && (self.is_identifier()
                || self.token_after_import_definitely_produces_import_declaration())
        {
            is_type_only = true;
            identifier = if self.is_identifier() {
                Some(self.parse_identifier(None, None).wrap())
            } else {
                None
            };
        }

        if identifier.is_some()
            && !self.token_after_imported_identifier_definitely_produces_import_declaration()
        {
            return self.parse_import_equals_declaration(
                pos,
                has_jsdoc,
                decorators,
                modifiers,
                identifier.unwrap(),
                is_type_only,
            );
        }

        let mut import_clause: Option<Gc<Node>> = None;
        if identifier.is_some()
            || matches!(
                self.token(),
                SyntaxKind::AsteriskToken | SyntaxKind::OpenBraceToken
            )
        {
            import_clause = Some(
                self.parse_import_clause(identifier, after_import_pos, is_type_only)
                    .wrap(),
            );
            self.parse_expected(SyntaxKind::FromKeyword, None, None);
        }
        let module_specifier = self.parse_module_specifier();

        let mut assert_clause: Option<Gc<Node>> = None;
        if self.token() == SyntaxKind::AssertKeyword && !self.scanner().has_preceding_line_break() {
            assert_clause = Some(self.parse_assert_clause().wrap());
        }

        self.parse_semicolon();
        let node = self.factory().create_import_declaration_raw(
            decorators,
            modifiers,
            import_clause,
            module_specifier,
            assert_clause,
        );
        self.with_jsdoc(self.finish_node(node, pos, None).wrap(), has_jsdoc)
    }

    pub(super) fn parse_assert_entry(&self) -> AssertEntry {
        let pos = self.get_node_pos();
        let name: Gc<Node> = if token_is_identifier_or_keyword(self.token()) {
            self.parse_identifier_name(None).wrap()
        } else {
            self.parse_literal_like_node(SyntaxKind::StringLiteral)
                .wrap()
        };
        self.parse_expected(SyntaxKind::ColonToken, None, None);
        let value: Gc<Node> = self
            .parse_literal_like_node(SyntaxKind::StringLiteral)
            .wrap();
        self.finish_node(self.factory().create_assert_entry_raw(name, value), pos, None)
    }

    pub(super) fn parse_assert_clause(&self) -> AssertClause {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::AssertKeyword, None, None);
        let open_brace_position = self.scanner().get_token_pos();
        if self.parse_expected(SyntaxKind::OpenBraceToken, None, None) {
            let multi_line = self.scanner().has_preceding_line_break();
            let elements = self.parse_delimited_list(
                ParsingContext::AssertEntries,
                || self.parse_assert_entry().wrap(),
                Some(true),
            );
            if !self.parse_expected(SyntaxKind::CloseBraceToken, None, None) {
                let parse_diagnostics = self.parse_diagnostics();
                let last_error = last_or_undefined(&*parse_diagnostics);
                if let Some(last_error) = last_error {
                    if last_error.code() == Diagnostics::_0_expected.code {
                        add_related_info(
                            last_error,
                            vec![Gc::new(
                                create_detached_diagnostic(
                                    &self.file_name(),
                                    open_brace_position.try_into().unwrap(),
                                    1,
                                    &Diagnostics::The_parser_expected_to_find_a_to_match_the_token_here,
                                    None,
                                )
                                .into(),
                            )],
                        );
                    }
                }
            }
            self.finish_node(
                self.factory()
                    .create_assert_clause_raw(elements, Some(multi_line)),
                pos,
                None,
            )
        } else {
            let elements = self.create_node_array(vec![], self.get_node_pos(), None, Some(false));
            self.finish_node(
                self.factory().create_assert_clause_raw(elements, Some(false)),
                pos,
                None,
            )
        }
    }

    pub(super) fn token_after_import_definitely_produces_import_declaration(&self) -> bool {
        matches!(
            self.token(),
            SyntaxKind::AsteriskToken | SyntaxKind::OpenBraceToken
        )
    }

    pub(super) fn token_after_imported_identifier_definitely_produces_import_declaration(
        &self,
    ) -> bool {
        matches!(
            self.token(),
            SyntaxKind::CommaToken | SyntaxKind::FromKeyword
        )
    }

    pub(super) fn parse_import_equals_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
        identifier: Gc<Node /*Identifier*/>,
        is_type_only: bool,
    ) -> Gc<Node /*ImportEqualsDeclaration*/> {
        self.parse_expected(SyntaxKind::EqualsToken, None, None);
        let module_reference: Gc<Node> = self.parse_module_reference().wrap();
        self.parse_semicolon();
        let node = self.factory().create_import_equals_declaration_raw(
            decorators,
            modifiers,
            is_type_only,
            identifier,
            module_reference,
        );
        self.with_jsdoc(self.finish_node(node, pos, None).wrap(), has_jsdoc)
    }

    pub(super) fn parse_import_clause(
        &self,
        identifier: Option<Gc<Node /*Identifier*/>>,
        pos: usize,
        is_type_only: bool,
    ) -> ImportClause {
        let mut named_bindings: Option<Gc<Node>> = None;
        if identifier.is_none() || self.parse_optional(SyntaxKind::CommaToken) {
            named_bindings = if self.token() == SyntaxKind::AsteriskToken {
                Some(self.parse_namespace_import().wrap())
            } else {
                Some(
                    self.parse_named_imports_or_exports(SyntaxKind::NamedImports)
                        .wrap(),
                )
            }
        }

        self.finish_node(
            self.factory()
                .create_import_clause_raw(is_type_only, identifier, named_bindings),
            pos.try_into().unwrap(),
            None,
        )
    }

    pub(super) fn parse_module_reference(&self) -> Node /*ExternalModuleReference | EntityName*/ {
        if self.is_external_module_reference() {
            self.parse_external_module_reference().into()
        } else {
            self.parse_entity_name(false, None)
        }
    }

    pub(super) fn parse_external_module_reference(&self) -> ExternalModuleReference {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::RequireKeyword, None, None);
        self.parse_expected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.parse_module_specifier();
        self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        self.finish_node(
            self.factory().create_external_module_reference_raw(expression),
            pos,
            None,
        )
    }

    pub(super) fn parse_module_specifier(&self) -> Gc<Node /*Expression*/> {
        if self.token() == SyntaxKind::StringLiteral {
            let result = self.parse_literal_node();
            let result_as_literal_like_node = result.as_literal_like_node();
            let text = self.intern_identifier(&result_as_literal_like_node.text());
            result_as_literal_like_node.set_text(text);
            result.wrap()
        } else {
            self.parse_expression()
        }
    }

    pub(super) fn parse_namespace_import(&self) -> NamespaceImport {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::AsteriskToken, None, None);
        self.parse_expected(SyntaxKind::AsKeyword, None, None);
        let name = self.parse_identifier(None, None);
        self.finish_node(
            self.factory().create_namespace_import_raw(name.wrap()),
            pos,
            None,
        )
    }

    pub(super) fn parse_named_imports_or_exports(&self, kind: SyntaxKind) -> Node /*NamedImportsOrExports*/
    {
        let pos = self.get_node_pos();

        let node: Node = if kind == SyntaxKind::NamedImports {
            self.factory()
                .create_named_imports_raw(self.parse_bracketed_list(
                    ParsingContext::ImportOrExportSpecifiers,
                    || self.parse_import_specifier().wrap(),
                    SyntaxKind::OpenBraceToken,
                    SyntaxKind::CloseBraceToken,
                ))
                .into()
        } else {
            self.factory()
                .create_named_exports_raw(self.parse_bracketed_list(
                    ParsingContext::ImportOrExportSpecifiers,
                    || self.parse_export_specifier().wrap(),
                    SyntaxKind::OpenBraceToken,
                    SyntaxKind::CloseBraceToken,
                ))
                .into()
        };
        self.finish_node(node, pos, None)
    }

    pub(super) fn parse_export_specifier(&self) -> Node /*ExportSpecifier*/ {
        self.parse_import_or_export_specifier(SyntaxKind::ExportSpecifier)
    }

    pub(super) fn parse_import_specifier(&self) -> Node /*ImportSpecifier*/ {
        self.parse_import_or_export_specifier(SyntaxKind::ImportSpecifier)
    }

    pub(super) fn parse_import_or_export_specifier(&self, kind: SyntaxKind) -> Node /*ImportOrExportSpecifier*/
    {
        let pos = self.get_node_pos();
        let mut check_identifier_is_keyword = is_keyword(self.token()) && !self.is_identifier();
        let mut check_identifier_start = self.scanner().get_token_pos();
        let mut check_identifier_end = self.scanner().get_text_pos();
        let mut is_type_only = false;
        let mut property_name: Option<Gc<Node>> = None;
        let mut can_parse_as_keyword = true;
        let mut name: Gc<Node> = self.parse_identifier_name(None).wrap();
        let mut parse_name_with_keyword_check = || {
            check_identifier_is_keyword = is_keyword(self.token()) && !self.is_identifier();
            check_identifier_start = self.scanner().get_token_pos();
            check_identifier_end = self.scanner().get_text_pos();
            self.parse_identifier_name(None)
        };
        if name.as_identifier().escaped_text == "type" {
            if self.token() == SyntaxKind::AsKeyword {
                let first_as: Gc<Node> = self.parse_identifier_name(None).wrap();
                if self.token() == SyntaxKind::AsKeyword {
                    let second_as: Gc<Node> = self.parse_identifier_name(None).wrap();
                    if token_is_identifier_or_keyword(self.token()) {
                        is_type_only = true;
                        property_name = Some(first_as);
                        name = parse_name_with_keyword_check().wrap();
                        can_parse_as_keyword = false;
                    } else {
                        property_name = Some(name);
                        name = second_as;
                        can_parse_as_keyword = false;
                    }
                } else if token_is_identifier_or_keyword(self.token()) {
                    property_name = Some(name);
                    can_parse_as_keyword = false;
                    name = parse_name_with_keyword_check().wrap();
                } else {
                    is_type_only = true;
                    name = first_as;
                }
            } else if token_is_identifier_or_keyword(self.token()) {
                is_type_only = true;
                name = parse_name_with_keyword_check().wrap();
            }
        }

        if can_parse_as_keyword && self.token() == SyntaxKind::AsKeyword {
            property_name = Some(name);
            self.parse_expected(SyntaxKind::AsKeyword, None, None);
            name = parse_name_with_keyword_check().wrap();
        }
        if kind == SyntaxKind::ImportSpecifier && check_identifier_is_keyword {
            self.parse_error_at(
                check_identifier_start.try_into().unwrap(),
                check_identifier_end.try_into().unwrap(),
                &Diagnostics::Identifier_expected,
                None,
            );
        }
        let node: Node = if kind == SyntaxKind::ImportSpecifier {
            self.factory()
                .create_import_specifier_raw(is_type_only, property_name, name)
                .into()
        } else {
            self.factory()
                .create_export_specifier_raw(is_type_only, property_name, name)
                .into()
        };
        self.finish_node(node, pos, None)
    }

    pub(super) fn parse_namespace_export(&self, pos: isize) -> NamespaceExport {
        self.finish_node(
            self.factory()
                .create_namespace_export_raw(self.parse_identifier_name(None).wrap()),
            pos,
            None,
        )
    }

    pub(super) fn parse_export_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<Gc<NodeArray>>,
        modifiers: Option<Gc<NodeArray>>,
    ) -> Gc<Node /*ExportDeclaration*/> {
        let saved_await_context = self.in_await_context();
        self.set_await_context(true);
        let mut export_clause: Option<Gc<Node>> = None;
        let mut module_specifier: Option<Gc<Node>> = None;
        let mut assert_clause: Option<Gc<Node>> = None;
        let is_type_only = self.parse_optional(SyntaxKind::TypeKeyword);
        let namespace_export_pos = self.get_node_pos();
        if self.parse_optional(SyntaxKind::AsteriskToken) {
            if self.parse_optional(SyntaxKind::AsKeyword) {
                export_clause = Some(self.parse_namespace_export(namespace_export_pos).wrap());
            }
            self.parse_expected(SyntaxKind::FromKeyword, None, None);
            module_specifier = Some(self.parse_module_specifier());
        } else {
            export_clause = Some(
                self.parse_named_imports_or_exports(SyntaxKind::NamedExports)
                    .wrap(),
            );
            if self.token() == SyntaxKind::FromKeyword
                || self.token() == SyntaxKind::StringLiteral
                    && !self.scanner().has_preceding_line_break()
            {
                self.parse_expected(SyntaxKind::FromKeyword, None, None);
                module_specifier = Some(self.parse_module_specifier());
            }
        }
        if module_specifier.is_some()
            && self.token() == SyntaxKind::AssertKeyword
            && !self.scanner().has_preceding_line_break()
        {
            assert_clause = Some(self.parse_assert_clause().wrap());
        }
        self.parse_semicolon();
        self.set_await_context(saved_await_context);
        let node = self.factory().create_export_declaration_raw(
            decorators,
            modifiers,
            is_type_only,
            export_clause,
            module_specifier,
            assert_clause,
        );
        self.with_jsdoc(self.finish_node(node, pos, None).wrap(), has_jsdoc)
    }
}
