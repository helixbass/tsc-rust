#![allow(non_upper_case_globals)]

use std::rc::Rc;

use crate::{
    are_option_rcs_equal, declaration_name_to_string, for_each, get_combined_node_flags,
    get_es_module_interop, get_external_module_name, get_first_identifier, get_source_file_of_node,
    has_effective_modifiers, has_syntactic_modifier, id_text, is_ambient_module,
    is_external_module_name_relative, is_import_declaration, is_import_specifier,
    is_internal_module_import_equals_declaration, is_module_exports_access_expression,
    is_private_identifier, is_string_literal, is_type_only_import_or_export_declaration,
    node_is_missing, Debug_, DiagnosticMessage, Diagnostics, ExternalEmitHelpers,
    LiteralLikeNodeInterface, ModifierFlags, ModuleKind, NamedDeclarationInterface, Node,
    NodeFlags, NodeInterface, SymbolFlags, SymbolInterface, SyntaxKind, TypeChecker,
};

impl TypeChecker {
    pub(super) fn get_first_non_module_exports_identifier(
        &self,
        node: &Node, /*EntityNameOrEntityNameExpression*/
    ) -> Rc<Node /*Identifier*/> {
        match node.kind() {
            SyntaxKind::Identifier => node.node_wrapper(),
            SyntaxKind::QualifiedName => {
                let mut node = node.node_wrapper();
                while {
                    node = node.as_qualified_name().left.clone();
                    node.kind() != SyntaxKind::Identifier
                } {}
                node
            }
            SyntaxKind::PropertyAccessExpression => {
                let mut node = node.node_wrapper();
                while {
                    let node_as_property_access_expression = node.as_property_access_expression();
                    if is_module_exports_access_expression(
                        &node_as_property_access_expression.expression,
                    ) && !is_private_identifier(&node_as_property_access_expression.name)
                    {
                        return node_as_property_access_expression.name.clone();
                    }
                    node = node_as_property_access_expression.expression.clone();
                    node.kind() != SyntaxKind::Identifier
                } {}
                node
            }
            _ => unreachable!(),
        }
    }

    pub(super) fn check_external_import_or_export_declaration(
        &self,
        node: &Node, /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/
    ) -> bool {
        let module_name = get_external_module_name(node);
        if match module_name.as_ref() {
            None => true,
            Some(module_name) => node_is_missing(Some(&**module_name)),
        } {
            return false;
        }
        let ref module_name = module_name.unwrap();
        if !is_string_literal(module_name) {
            self.error(
                Some(&**module_name),
                &Diagnostics::String_literal_expected,
                None,
            );
            return false;
        }
        let is_ambient_external_module = node.parent().kind() == SyntaxKind::ModuleBlock
            && is_ambient_module(&node.parent().parent());
        if node.parent().kind() != SyntaxKind::SourceFile && !is_ambient_external_module {
            self.error(
                Some(&**module_name),
                if node.kind() == SyntaxKind::ExportDeclaration {
                    &*Diagnostics::Export_declarations_are_not_permitted_in_a_namespace
                } else {
                    &*Diagnostics::Import_declarations_in_a_namespace_cannot_reference_a_module
                },
                None,
            );
            return false;
        }
        if is_ambient_external_module
            && is_external_module_name_relative(&module_name.as_string_literal().text())
        {
            if !self.is_top_level_in_external_module_augmentation(node) {
                self.error(
                    Some(node),
                    &Diagnostics::Import_or_export_declaration_in_an_ambient_module_declaration_cannot_reference_module_through_relative_module_name,
                    None,
                );
                return false;
            }
        }
        true
    }

    pub(super) fn check_alias_symbol(
        &self,
        node: &Node, /*ImportEqualsDeclaration | VariableDeclaration | ImportClause | NamespaceImport | ImportSpecifier | ExportSpecifier | NamespaceExport*/
    ) {
        let mut symbol = self.get_symbol_of_node(node).unwrap();
        let target = self.resolve_alias(&symbol);

        if !Rc::ptr_eq(&target, &self.unknown_symbol()) {
            symbol = self
                .get_merged_symbol(Some(symbol.maybe_export_symbol().unwrap_or(symbol)))
                .unwrap();
            let excluded_meanings = if symbol
                .flags()
                .intersects(SymbolFlags::Value | SymbolFlags::ExportValue)
            {
                SymbolFlags::Value
            } else {
                SymbolFlags::None
            } | if symbol.flags().intersects(SymbolFlags::Type) {
                SymbolFlags::Type
            } else {
                SymbolFlags::None
            } | if symbol.flags().intersects(SymbolFlags::Namespace) {
                SymbolFlags::Namespace
            } else {
                SymbolFlags::None
            };
            if target.flags().intersects(excluded_meanings) {
                let message = if node.kind() == SyntaxKind::ExportSpecifier {
                    &*Diagnostics::Export_declaration_conflicts_with_exported_declaration_of_0
                } else {
                    &*Diagnostics::Import_declaration_conflicts_with_local_declaration_of_0
                };
                self.error(
                    Some(node),
                    message,
                    Some(vec![self.symbol_to_string_(
                        &symbol,
                        Option::<&Node>::None,
                        None,
                        None,
                        None,
                    )]),
                );
            }

            if self.compiler_options.isolated_modules == Some(true)
                && !is_type_only_import_or_export_declaration(node)
                && !node.flags().intersects(NodeFlags::Ambient)
            {
                let type_only_alias = self.get_type_only_alias_declaration(&symbol);
                let is_type = !target.flags().intersects(SymbolFlags::Value);
                if is_type || type_only_alias.is_some() {
                    match node.kind() {
                        SyntaxKind::ImportClause
                        | SyntaxKind::ImportSpecifier
                        | SyntaxKind::ImportEqualsDeclaration => {
                            if self.compiler_options.preserve_value_imports == Some(true) {
                                let node_name = node.as_named_declaration().maybe_name();
                                Debug_.assert_is_defined(
                                    &node_name,
                                    Some("An ImportClause with a symbol should have a name"),
                                );
                                let ref node_name = node_name.unwrap();
                                let message = if is_type {
                                    &*Diagnostics::_0_is_a_type_and_must_be_imported_using_a_type_only_import_when_preserveValueImports_and_isolatedModules_are_both_enabled
                                } else {
                                    &*Diagnostics::_0_resolves_to_a_type_only_declaration_and_must_be_imported_using_a_type_only_import_when_preserveValueImports_and_isolatedModules_are_both_enabled
                                };
                                let name =
                                    id_text(&*if node.kind() == SyntaxKind::ImportSpecifier {
                                        node.as_import_specifier()
                                            .property_name
                                            .clone()
                                            .unwrap_or_else(|| node_name.clone())
                                    } else {
                                        node_name.clone()
                                    });
                                self.add_type_only_declaration_related_info(
                                    self.error(Some(node), message, Some(vec![name.clone()])),
                                    if is_type {
                                        None
                                    } else {
                                        type_only_alias.as_deref()
                                    },
                                    &name,
                                );
                            }
                        }
                        SyntaxKind::ExportSpecifier => {
                            if !are_option_rcs_equal(
                                get_source_file_of_node(type_only_alias.as_deref()).as_ref(),
                                get_source_file_of_node(Some(node)).as_ref(),
                            ) {
                                let message = if is_type {
                                    &*Diagnostics::Re_exporting_a_type_when_the_isolatedModules_flag_is_provided_requires_using_export_type
                                } else {
                                    &*Diagnostics::_0_resolves_to_a_type_only_declaration_and_must_be_re_exported_using_a_type_only_re_export_when_isolatedModules_is_enabled
                                };
                                let node_as_export_specifier = node.as_export_specifier();
                                let name = id_text(
                                    node_as_export_specifier
                                        .property_name
                                        .as_ref()
                                        .unwrap_or(&node_as_export_specifier.name),
                                );
                                self.add_type_only_declaration_related_info(
                                    self.error(Some(node), message, Some(vec![name.clone()])),
                                    if is_type {
                                        None
                                    } else {
                                        type_only_alias.as_deref()
                                    },
                                    &name,
                                );
                                return;
                            }
                        }
                        _ => (),
                    }
                }
            }

            if is_import_specifier(node) {
                if let Some(target_declarations) =
                    target
                        .maybe_declarations()
                        .as_ref()
                        .filter(|target_declarations| {
                            target_declarations.into_iter().all(|d| {
                                get_combined_node_flags(d).intersects(NodeFlags::Deprecated)
                            })
                        })
                {
                    self.add_deprecated_suggestion(
                        &node.as_named_declaration().name(),
                        target_declarations,
                        symbol.escaped_name(),
                    );
                }
            }
        }
    }

    pub(super) fn check_import_binding(
        &self,
        node: &Node, /*ImportEqualsDeclaration | ImportClause | NamespaceImport | ImportSpecifier*/
    ) {
        self.check_collisions_for_declaration_name(node, node.as_named_declaration().maybe_name());
        self.check_alias_symbol(node);
        if node.kind() == SyntaxKind::ImportSpecifier && {
            let node_as_import_specifier = node.as_import_specifier();
            id_text(
                node_as_import_specifier
                    .property_name
                    .as_ref()
                    .unwrap_or(&node_as_import_specifier.name),
            ) == "default"
                && get_es_module_interop(&self.compiler_options) == Some(true)
                && self.module_kind != ModuleKind::System
                && (self.module_kind < ModuleKind::ES2015
                    || get_source_file_of_node(Some(node))
                        .unwrap()
                        .as_source_file()
                        .maybe_implied_node_format()
                        == Some(ModuleKind::CommonJS))
        } {
            self.check_external_emit_helpers(node, ExternalEmitHelpers::ImportDefault);
        }
    }

    pub(super) fn check_assert_clause(
        &self,
        declaration: &Node, /*ImportDeclaration | ExportDeclaration*/
    ) {
        if let Some(declaration_assert_clause) = declaration
            .as_has_assert_clause()
            .maybe_assert_clause()
            .as_ref()
        {
            if self.module_kind != ModuleKind::ESNext {
                self.grammar_error_on_node(
                    declaration_assert_clause,
                    &Diagnostics::Import_assertions_are_only_supported_when_the_module_option_is_set_to_esnext,
                    None,
                );
                return;
            }

            if if is_import_declaration(declaration) {
                matches!(
                    declaration.as_import_declaration().import_clause.as_ref(),
                    Some(declaration_import_clause) if declaration_import_clause.as_import_clause().is_type_only
                )
            } else {
                declaration.as_export_declaration().is_type_only
            } {
                self.grammar_error_on_node(
                    declaration_assert_clause,
                    &Diagnostics::Import_assertions_cannot_be_used_with_type_only_imports_or_exports,
                    None,
                );
                return;
            }
        }
    }

    pub(super) fn check_import_declaration(&self, node: &Node /*ImportDeclaration*/) {
        if self.check_grammar_module_element_context(
            node,
            &Diagnostics::An_import_declaration_can_only_be_used_in_a_namespace_or_module,
        ) {
            return;
        }
        if !self.check_grammar_decorators_and_modifiers(node) && has_effective_modifiers(node) {
            self.grammar_error_on_first_token(
                node,
                &Diagnostics::An_import_declaration_cannot_have_modifiers,
                None,
            );
        }
        if self.check_external_import_or_export_declaration(node) {
            let node_as_import_declaration = node.as_import_declaration();
            let import_clause = node_as_import_declaration.import_clause.as_ref();
            if let Some(import_clause) = import_clause
                .filter(|import_clause| !self.check_grammar_import_clause(import_clause))
            {
                let import_clause_as_import_clause = import_clause.as_import_clause();
                if import_clause_as_import_clause.name.is_some() {
                    self.check_import_binding(import_clause);
                }
                if let Some(import_clause_named_bindings) =
                    import_clause_as_import_clause.named_bindings.as_ref()
                {
                    if import_clause_named_bindings.kind() == SyntaxKind::NamespaceImport {
                        self.check_import_binding(import_clause_named_bindings);
                        if self.module_kind != ModuleKind::System
                            && (self.module_kind < ModuleKind::ES2015
                                || get_source_file_of_node(Some(node))
                                    .unwrap()
                                    .as_source_file()
                                    .maybe_implied_node_format()
                                    == Some(ModuleKind::CommonJS))
                            && get_es_module_interop(&self.compiler_options) == Some(true)
                        {
                            self.check_external_emit_helpers(node, ExternalEmitHelpers::ImportStar);
                        }
                    } else {
                        let module_existed = self.resolve_external_module_name_(
                            node,
                            &node_as_import_declaration.module_specifier,
                            None,
                        );
                        if module_existed.is_some() {
                            for_each(
                                &import_clause_named_bindings.as_named_imports().elements,
                                |element: &Rc<Node>, _| -> Option<()> {
                                    self.check_import_binding(element);
                                    None
                                },
                            );
                        }
                    }
                }
            }
        }
        self.check_assert_clause(node);
    }

    pub(super) fn check_import_equals_declaration(
        &self,
        node: &Node, /*ImportEqualsDeclaration*/
    ) {
        if self.check_grammar_module_element_context(
            node,
            &Diagnostics::An_import_declaration_can_only_be_used_in_a_namespace_or_module,
        ) {
            return;
        }

        self.check_grammar_decorators_and_modifiers(node);
        if is_internal_module_import_equals_declaration(node)
            || self.check_external_import_or_export_declaration(node)
        {
            self.check_import_binding(node);
            if has_syntactic_modifier(node, ModifierFlags::Export) {
                self.mark_export_as_referenced(node);
            }
            let node_as_import_equals_declaration = node.as_import_equals_declaration();
            if node_as_import_equals_declaration.module_reference.kind()
                != SyntaxKind::ExternalModuleReference
            {
                let target = self.resolve_alias(&self.get_symbol_of_node(node).unwrap());
                if !Rc::ptr_eq(&target, &self.unknown_symbol()) {
                    if target.flags().intersects(SymbolFlags::Value) {
                        let module_name = get_first_identifier(
                            &node_as_import_equals_declaration.module_reference,
                        );
                        if !self
                            .resolve_entity_name(
                                &module_name,
                                SymbolFlags::Value | SymbolFlags::Namespace,
                                None,
                                None,
                                Option::<&Node>::None,
                            )
                            .unwrap()
                            .flags()
                            .intersects(SymbolFlags::Namespace)
                        {
                            self.error(
                                Some(&*module_name),
                                &Diagnostics::Module_0_is_hidden_by_a_local_declaration_with_the_same_name,
                                Some(vec![
                                    declaration_name_to_string(Some(&*module_name)).into_owned()
                                ])
                            );
                        }
                    }
                    if target.flags().intersects(SymbolFlags::Type) {
                        self.check_type_name_is_reserved(
                            &node_as_import_equals_declaration.name(),
                            &Diagnostics::Import_name_cannot_be_0,
                        );
                    }
                }
                if node_as_import_equals_declaration.is_type_only {
                    self.grammar_error_on_node(
                        node,
                        &Diagnostics::An_import_alias_cannot_use_import_type,
                        None,
                    );
                }
            } else {
                if self.module_kind >= ModuleKind::ES2015
                    && get_source_file_of_node(Some(node))
                        .unwrap()
                        .as_source_file()
                        .maybe_implied_node_format()
                        .is_none()
                    && !node_as_import_equals_declaration.is_type_only
                    && !node.flags().intersects(NodeFlags::Ambient)
                {
                    self.grammar_error_on_node(
                        node,
                        &Diagnostics::Import_assignment_cannot_be_used_when_targeting_ECMAScript_modules_Consider_using_import_Asterisk_as_ns_from_mod_import_a_from_mod_import_d_from_mod_or_another_module_format_instead,
                        None,
                    );
                }
            }
        }
    }

    pub(super) fn check_grammar_module_element_context(
        &self,
        node: &Node, /*Statement*/
        error_message: &DiagnosticMessage,
    ) -> bool {
        unimplemented!()
    }
}
