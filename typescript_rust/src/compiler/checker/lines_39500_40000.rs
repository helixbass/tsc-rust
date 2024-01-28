use std::io;

use gc::Gc;
use id_arena::Id;

use super::{is_not_overload, is_not_overload_and_not_accessor};
use crate::{
    are_option_gcs_equal, count_where, create_diagnostic_for_node, declaration_name_to_string,
    for_each_entry_bool, get_combined_node_flags, get_effective_type_annotation_node,
    get_emit_declarations, get_es_module_interop, get_external_module_name, get_first_identifier,
    get_source_file_of_node, has_effective_modifiers, has_syntactic_modifier, id_text,
    is_ambient_module, is_entity_name_expression, is_external_module_name_relative,
    is_external_module_reference, is_import_declaration, is_import_equals_declaration,
    is_import_specifier, is_in_js_file, is_internal_module_import_equals_declaration,
    is_module_exports_access_expression, is_named_exports, is_namespace_export,
    is_private_identifier, is_string_literal, is_type_only_import_or_export_declaration, length,
    maybe_get_source_file_of_node, node_is_missing, try_for_each,
    try_for_each_import_clause_declaration_bool, unescape_leading_underscores, Debug_,
    DiagnosticMessage, Diagnostics, ExternalEmitHelpers, HasArena, HasStatementsInterface, InArena,
    LiteralLikeNodeInterface, ModifierFlags, ModuleKind, NamedDeclarationInterface, Node,
    NodeFlags, NodeInterface, OptionTry, ScriptTarget, Symbol, SymbolFlags, SymbolInterface,
    SyntaxKind, TypeChecker,
};

impl TypeChecker {
    pub(super) fn get_first_non_module_exports_identifier(
        &self,
        mut node: Id<Node>, /*EntityNameOrEntityNameExpression*/
    ) -> Id<Node /*Identifier*/> {
        match node.ref_(self).kind() {
            SyntaxKind::Identifier => node,
            SyntaxKind::QualifiedName => {
                while {
                    node = node.ref_(self).as_qualified_name().left;
                    node.ref_(self).kind() != SyntaxKind::Identifier
                } {}
                node
            }
            SyntaxKind::PropertyAccessExpression => {
                while {
                    let node_ref = node.ref_(self);
                    let node_as_property_access_expression = node_ref.as_property_access_expression();
                    if is_module_exports_access_expression(
                        node_as_property_access_expression.expression, self,
                    ) && !is_private_identifier(&node_as_property_access_expression.name.ref_(self))
                    {
                        return node_as_property_access_expression.name;
                    }
                    node = node_as_property_access_expression.expression;
                    node.ref_(self).kind() != SyntaxKind::Identifier
                } {}
                node
            }
            _ => unreachable!(),
        }
    }

    pub(super) fn check_external_import_or_export_declaration(
        &self,
        node: Id<Node>, /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/
    ) -> bool {
        let module_name = get_external_module_name(node, self);
        if match module_name {
            None => true,
            Some(module_name) => node_is_missing(Some(&module_name.ref_(self))),
        } {
            return false;
        }
        let module_name = module_name.unwrap();
        if !is_string_literal(&module_name.ref_(self)) {
            self.error(
                Some(module_name),
                &Diagnostics::String_literal_expected,
                None,
            );
            return false;
        }
        let is_ambient_external_module = node.ref_(self).parent().ref_(self).kind() == SyntaxKind::ModuleBlock
            && is_ambient_module(node.ref_(self).parent().ref_(self).parent(), self);
        if node.ref_(self).parent().ref_(self).kind() != SyntaxKind::SourceFile && !is_ambient_external_module {
            self.error(
                Some(module_name),
                if node.ref_(self).kind() == SyntaxKind::ExportDeclaration {
                    &*Diagnostics::Export_declarations_are_not_permitted_in_a_namespace
                } else {
                    &*Diagnostics::Import_declarations_in_a_namespace_cannot_reference_a_module
                },
                None,
            );
            return false;
        }
        if is_ambient_external_module
            && is_external_module_name_relative(&module_name.ref_(self).as_string_literal().text())
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
        node: Id<Node>, /*ImportEqualsDeclaration | VariableDeclaration | ImportClause | NamespaceImport | ImportSpecifier | ExportSpecifier | NamespaceExport*/
    ) -> io::Result<()> {
        let mut symbol = self.get_symbol_of_node(node)?.unwrap();
        let target = self.resolve_alias(symbol)?;

        if target != self.unknown_symbol() {
            symbol = self
                .get_merged_symbol(Some(
                    symbol.ref_(self).maybe_export_symbol().unwrap_or(symbol),
                ))
                .unwrap();
            let excluded_meanings =
                if symbol
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::Value | SymbolFlags::ExportValue)
                {
                    SymbolFlags::Value
                } else {
                    SymbolFlags::None
                } | if symbol.ref_(self).flags().intersects(SymbolFlags::Type) {
                    SymbolFlags::Type
                } else {
                    SymbolFlags::None
                } | if symbol.ref_(self).flags().intersects(SymbolFlags::Namespace) {
                    SymbolFlags::Namespace
                } else {
                    SymbolFlags::None
                };
            if target.ref_(self).flags().intersects(excluded_meanings) {
                let message = if node.ref_(self).kind() == SyntaxKind::ExportSpecifier {
                    &*Diagnostics::Export_declaration_conflicts_with_exported_declaration_of_0
                } else {
                    &*Diagnostics::Import_declaration_conflicts_with_local_declaration_of_0
                };
                self.error(
                    Some(node),
                    message,
                    Some(vec![self.symbol_to_string_(
                        symbol,
                        Option::<Id<Node>>::None,
                        None,
                        None,
                        None,
                    )?]),
                );
            }

            if self.compiler_options.ref_(self).isolated_modules == Some(true)
                && !is_type_only_import_or_export_declaration(node, self)
                && !node.ref_(self).flags().intersects(NodeFlags::Ambient)
            {
                let type_only_alias = self.get_type_only_alias_declaration(symbol);
                let is_type = !target.ref_(self).flags().intersects(SymbolFlags::Value);
                if is_type || type_only_alias.is_some() {
                    match node.ref_(self).kind() {
                        SyntaxKind::ImportClause
                        | SyntaxKind::ImportSpecifier
                        | SyntaxKind::ImportEqualsDeclaration => {
                            if self.compiler_options.ref_(self).preserve_value_imports == Some(true) {
                                let node_name = node.ref_(self).as_named_declaration().maybe_name();
                                Debug_.assert_is_defined(
                                    &node_name,
                                    Some("An ImportClause with a symbol should have a name"),
                                );
                                let node_name = node_name.unwrap();
                                let message = if is_type {
                                    &*Diagnostics::_0_is_a_type_and_must_be_imported_using_a_type_only_import_when_preserveValueImports_and_isolatedModules_are_both_enabled
                                } else {
                                    &*Diagnostics::_0_resolves_to_a_type_only_declaration_and_must_be_imported_using_a_type_only_import_when_preserveValueImports_and_isolatedModules_are_both_enabled
                                };
                                let name_node = if node.ref_(self).kind() == SyntaxKind::ImportSpecifier {
                                    node.ref_(self).as_import_specifier()
                                        .property_name
                                        .unwrap_or(node_name)
                                } else {
                                    node_name
                                };
                                let name_node_ref = name_node.ref_(self);
                                let name = id_text(&name_node_ref);
                                self.add_type_only_declaration_related_info(
                                    self.error(Some(node), message, Some(vec![name.to_owned()])),
                                    if is_type {
                                        None
                                    } else {
                                        type_only_alias
                                    },
                                    &name,
                                );
                            }
                        }
                        SyntaxKind::ExportSpecifier => {
                            if maybe_get_source_file_of_node(type_only_alias, self) !=
                                maybe_get_source_file_of_node(Some(node), self)
                            {
                                let message = if is_type {
                                    &*Diagnostics::Re_exporting_a_type_when_the_isolatedModules_flag_is_provided_requires_using_export_type
                                } else {
                                    &*Diagnostics::_0_resolves_to_a_type_only_declaration_and_must_be_re_exported_using_a_type_only_re_export_when_isolatedModules_is_enabled
                                };
                                let node_ref = node.ref_(self);
                                let node_as_export_specifier = node_ref.as_export_specifier();
                                let name_ref = node_as_export_specifier
                                    .property_name
                                    .unwrap_or(node_as_export_specifier.name)
                                    .ref_(self);
                                let name = id_text(&name_ref);
                                self.add_type_only_declaration_related_info(
                                    self.error(Some(node), message, Some(vec![name.to_owned()])),
                                    if is_type {
                                        None
                                    } else {
                                        type_only_alias
                                    },
                                    &name,
                                );
                                return Ok(());
                            }
                        }
                        _ => (),
                    }
                }
            }

            if is_import_specifier(&node.ref_(self)) {
                if let Some(target_declarations) = target
                    .ref_(self)
                    .maybe_declarations()
                    .as_ref()
                    .filter(|target_declarations| {
                        target_declarations
                            .into_iter()
                            .all(|&d| get_combined_node_flags(d, self).intersects(NodeFlags::Deprecated))
                    })
                {
                    self.add_deprecated_suggestion(
                        node.ref_(self).as_named_declaration().name(),
                        target_declarations,
                        symbol.ref_(self).escaped_name(),
                    );
                }
            }
        }

        Ok(())
    }

    pub(super) fn check_import_binding(
        &self,
        node: Id<Node>, /*ImportEqualsDeclaration | ImportClause | NamespaceImport | ImportSpecifier*/
    ) -> io::Result<()> {
        self.check_collisions_for_declaration_name(node, node.ref_(self).as_named_declaration().maybe_name());
        self.check_alias_symbol(node)?;
        if node.ref_(self).kind() == SyntaxKind::ImportSpecifier && {
            let node_ref = node.ref_(self);
            let node_as_import_specifier = node_ref.as_import_specifier();
            id_text(
                &node_as_import_specifier
                    .property_name
                    .unwrap_or(node_as_import_specifier.name)
                    .ref_(self),
            ) == "default"
                && get_es_module_interop(&self.compiler_options.ref_(self)) == Some(true)
                && self.module_kind != ModuleKind::System
                && (self.module_kind < ModuleKind::ES2015
                    || get_source_file_of_node(node, self)
                        .ref_(self).as_source_file()
                        .maybe_implied_node_format()
                        == Some(ModuleKind::CommonJS))
        } {
            self.check_external_emit_helpers(node, ExternalEmitHelpers::ImportDefault)?;
        }

        Ok(())
    }

    pub(super) fn check_assert_clause(
        &self,
        declaration: Id<Node>, /*ImportDeclaration | ExportDeclaration*/
    ) {
        if let Some(declaration_assert_clause) = declaration
            .ref_(self).as_has_assert_clause()
            .maybe_assert_clause()
        {
            if self.module_kind != ModuleKind::ESNext {
                self.grammar_error_on_node(
                    declaration_assert_clause,
                    &Diagnostics::Import_assertions_are_only_supported_when_the_module_option_is_set_to_esnext,
                    None,
                );
                return;
            }

            if if is_import_declaration(&declaration.ref_(self)) {
                matches!(
                    declaration.ref_(self).as_import_declaration().import_clause,
                    Some(declaration_import_clause) if declaration_import_clause.ref_(self).as_import_clause().is_type_only
                )
            } else {
                declaration.ref_(self).as_export_declaration().is_type_only
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

    pub(super) fn check_import_declaration(
        &self,
        node: Id<Node>, /*ImportDeclaration*/
    ) -> io::Result<()> {
        if self.check_grammar_module_element_context(
            node,
            &Diagnostics::An_import_declaration_can_only_be_used_in_a_namespace_or_module,
        ) {
            return Ok(());
        }
        if !self.check_grammar_decorators_and_modifiers(node) && has_effective_modifiers(node, self) {
            self.grammar_error_on_first_token(
                node,
                &Diagnostics::An_import_declaration_cannot_have_modifiers,
                None,
            );
        }
        if self.check_external_import_or_export_declaration(node) {
            let node_ref = node.ref_(self);
            let node_as_import_declaration = node_ref.as_import_declaration();
            let import_clause = node_as_import_declaration.import_clause;
            if let Some(import_clause) = import_clause
                .filter(|&import_clause| !self.check_grammar_import_clause(import_clause))
            {
                let import_clause_ref = import_clause.ref_(self);
                let import_clause_as_import_clause = import_clause_ref.as_import_clause();
                if import_clause_as_import_clause.name.is_some() {
                    self.check_import_binding(import_clause)?;
                }
                if let Some(import_clause_named_bindings) =
                    import_clause_as_import_clause.named_bindings
                {
                    if import_clause_named_bindings.ref_(self).kind() == SyntaxKind::NamespaceImport {
                        self.check_import_binding(import_clause_named_bindings)?;
                        if self.module_kind != ModuleKind::System
                            && (self.module_kind < ModuleKind::ES2015
                                || get_source_file_of_node(node, self)
                                    .ref_(self).as_source_file()
                                    .maybe_implied_node_format()
                                    == Some(ModuleKind::CommonJS))
                            && get_es_module_interop(&self.compiler_options.ref_(self)) == Some(true)
                        {
                            self.check_external_emit_helpers(
                                node,
                                ExternalEmitHelpers::ImportStar,
                            )?;
                        }
                    } else {
                        let module_existed = self.resolve_external_module_name_(
                            node,
                            node_as_import_declaration.module_specifier,
                            None,
                        )?;
                        if module_existed.is_some() {
                            try_for_each(
                                &*import_clause_named_bindings.ref_(self).as_named_imports().elements.ref_(self),
                                |&element: &Id<Node>, _| -> io::Result<Option<()>> {
                                    self.check_import_binding(element)?;
                                    Ok(None)
                                },
                            )?;
                        }
                    }
                }
            }
        }
        self.check_assert_clause(node);

        Ok(())
    }

    pub(super) fn check_import_equals_declaration(
        &self,
        node: Id<Node>, /*ImportEqualsDeclaration*/
    ) -> io::Result<()> {
        if self.check_grammar_module_element_context(
            node,
            &Diagnostics::An_import_declaration_can_only_be_used_in_a_namespace_or_module,
        ) {
            return Ok(());
        }

        self.check_grammar_decorators_and_modifiers(node);
        if is_internal_module_import_equals_declaration(node, self)
            || self.check_external_import_or_export_declaration(node)
        {
            self.check_import_binding(node)?;
            if has_syntactic_modifier(node, ModifierFlags::Export, self) {
                self.mark_export_as_referenced(node)?;
            }
            let node_ref = node.ref_(self);
            let node_as_import_equals_declaration = node_ref.as_import_equals_declaration();
            if node_as_import_equals_declaration.module_reference.ref_(self).kind()
                != SyntaxKind::ExternalModuleReference
            {
                let target = self.resolve_alias(self.get_symbol_of_node(node)?.unwrap())?;
                if target != self.unknown_symbol() {
                    if target.ref_(self).flags().intersects(SymbolFlags::Value) {
                        let module_name = get_first_identifier(
                            node_as_import_equals_declaration.module_reference,
                            self,
                        );
                        if !self
                            .resolve_entity_name(
                                module_name,
                                SymbolFlags::Value | SymbolFlags::Namespace,
                                None,
                                None,
                                Option::<Id<Node>>::None,
                            )?
                            .unwrap()
                            .ref_(self)
                            .flags()
                            .intersects(SymbolFlags::Namespace)
                        {
                            self.error(
                                Some(module_name),
                                &Diagnostics::Module_0_is_hidden_by_a_local_declaration_with_the_same_name,
                                Some(vec![
                                    declaration_name_to_string(Some(module_name), self).into_owned()
                                ])
                            );
                        }
                    }
                    if target.ref_(self).flags().intersects(SymbolFlags::Type) {
                        self.check_type_name_is_reserved(
                            node_as_import_equals_declaration.name(),
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
                    && get_source_file_of_node(node, self)
                        .ref_(self).as_source_file()
                        .maybe_implied_node_format()
                        .is_none()
                    && !node_as_import_equals_declaration.is_type_only
                    && !node.ref_(self).flags().intersects(NodeFlags::Ambient)
                {
                    self.grammar_error_on_node(
                        node,
                        &Diagnostics::Import_assignment_cannot_be_used_when_targeting_ECMAScript_modules_Consider_using_import_Asterisk_as_ns_from_mod_import_a_from_mod_import_d_from_mod_or_another_module_format_instead,
                        None,
                    );
                }
            }
        }

        Ok(())
    }

    pub(super) fn check_export_declaration(
        &self,
        node: Id<Node>, /*ExportDeclaration*/
    ) -> io::Result<()> {
        if self.check_grammar_module_element_context(
            node,
            &Diagnostics::An_export_declaration_can_only_be_used_in_a_module,
        ) {
            return Ok(());
        }

        if !self.check_grammar_decorators_and_modifiers(node) && has_effective_modifiers(node, self) {
            self.grammar_error_on_first_token(
                node,
                &Diagnostics::An_export_declaration_cannot_have_modifiers,
                None,
            );
        }

        let node_ref = node.ref_(self);
        let node_as_export_declaration = node_ref.as_export_declaration();
        if node_as_export_declaration.module_specifier.is_some()
            && matches!(
                node_as_export_declaration.export_clause,
                Some(node_export_clause) if is_named_exports(&node_export_clause.ref_(self)) &&
                    length(Some(&*node_export_clause.ref_(self).as_named_exports().elements.ref_(self))) > 0
            )
            && self.language_version == ScriptTarget::ES3
        {
            self.check_external_emit_helpers(node, ExternalEmitHelpers::CreateBinding)?;
        }

        self.check_grammar_export_declaration(node);
        if node_as_export_declaration.module_specifier.is_none()
            || self.check_external_import_or_export_declaration(node)
        {
            if let Some(node_export_clause) = node_as_export_declaration
                .export_clause
                .filter(|node_export_clause| !is_namespace_export(&node_export_clause.ref_(self)))
            {
                try_for_each(
                    &*node_export_clause.ref_(self).as_named_exports().elements.ref_(self),
                    |&element: &Id<Node>, _| -> io::Result<Option<()>> {
                        self.check_export_specifier(element)?;
                        Ok(None)
                    },
                )?;
                let in_ambient_external_module = node.ref_(self).parent().ref_(self).kind() == SyntaxKind::ModuleBlock
                    && is_ambient_module(node.ref_(self).parent().ref_(self).parent(), self);
                let in_ambient_namespace_declaration = !in_ambient_external_module
                    && node.ref_(self).parent().ref_(self).kind() == SyntaxKind::ModuleBlock
                    && node_as_export_declaration.module_specifier.is_none()
                    && node.ref_(self).flags().intersects(NodeFlags::Ambient);
                if node.ref_(self).parent().ref_(self).kind() != SyntaxKind::SourceFile
                    && !in_ambient_external_module
                    && !in_ambient_namespace_declaration
                {
                    self.error(
                        Some(node),
                        &Diagnostics::Export_declarations_are_not_permitted_in_a_namespace,
                        None,
                    );
                }
            } else {
                let module_symbol = self.resolve_external_module_name_(
                    node,
                    node_as_export_declaration
                        .module_specifier
                        .unwrap(),
                    None,
                )?;
                if let Some(module_symbol) = module_symbol
                    .filter(|&module_symbol| self.has_export_assignment_symbol(module_symbol))
                {
                    self.error(
                        node_as_export_declaration.module_specifier,
                        &Diagnostics::Module_0_uses_export_and_cannot_be_used_with_export_Asterisk,
                        Some(vec![self.symbol_to_string_(
                            module_symbol,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                            None,
                        )?]),
                    );
                } else if let Some(node_export_clause) =
                    node_as_export_declaration.export_clause
                {
                    self.check_alias_symbol(node_export_clause)?;
                }
                if self.module_kind != ModuleKind::System
                    && (self.module_kind < ModuleKind::ES2015
                        || get_source_file_of_node(node, self)
                            .ref_(self).as_source_file()
                            .maybe_implied_node_format()
                            == Some(ModuleKind::CommonJS))
                {
                    if node_as_export_declaration.export_clause.is_some() {
                        if get_es_module_interop(&self.compiler_options.ref_(self)) == Some(true) {
                            self.check_external_emit_helpers(
                                node,
                                ExternalEmitHelpers::ImportStar,
                            )?;
                        }
                    } else {
                        self.check_external_emit_helpers(node, ExternalEmitHelpers::ExportStar)?;
                    }
                }
            }
        }
        self.check_assert_clause(node);

        Ok(())
    }

    pub(super) fn check_grammar_export_declaration(
        &self,
        node: Id<Node>, /*ExportDeclaration*/
    ) -> bool {
        let node_ref = node.ref_(self);
        let node_as_export_declaration = node_ref.as_export_declaration();
        if node_as_export_declaration.is_type_only {
            if let Some(node_export_clause) = node_as_export_declaration
                .export_clause
                .filter(|node_export_clause| node_export_clause.ref_(self).kind() == SyntaxKind::NamedExports)
            {
                return self.check_grammar_named_imports_or_exports(node_export_clause);
            } else {
                return self.grammar_error_on_node(
                    node,
                    &Diagnostics::Only_named_exports_may_use_export_type,
                    None,
                );
            }
        }
        false
    }

    pub(super) fn check_grammar_module_element_context(
        &self,
        node: Id<Node>, /*Statement*/
        error_message: &DiagnosticMessage,
    ) -> bool {
        let is_in_appropriate_context = matches!(
            node.ref_(self).parent().ref_(self).kind(),
            SyntaxKind::SourceFile | SyntaxKind::ModuleBlock | SyntaxKind::ModuleDeclaration
        );
        if !is_in_appropriate_context {
            self.grammar_error_on_first_token(node, error_message, None);
        }
        !is_in_appropriate_context
    }

    pub(super) fn import_clause_contains_referenced_import(
        &self,
        import_clause: Id<Node>, /*ImportClause*/
    ) -> io::Result<bool> {
        try_for_each_import_clause_declaration_bool(import_clause, |declaration| -> io::Result<_> {
            Ok(
                match self
                    .get_symbol_of_node(declaration)?
                    .unwrap()
                    .ref_(self)
                    .maybe_is_referenced()
                {
                    None => false,
                    Some(is_referenced) => is_referenced != SymbolFlags::None,
                },
            )
        }, self)
    }

    pub(super) fn import_clause_contains_const_enum_used_as_value(
        &self,
        import_clause: Id<Node>, /*ImportClause*/
    ) -> io::Result<bool> {
        try_for_each_import_clause_declaration_bool(import_clause, |declaration| -> io::Result<_> {
            Ok(
                (*self.get_symbol_links(self.get_symbol_of_node(declaration)?.unwrap()).ref_(self))
                    .borrow()
                    .const_enum_referenced
                    == Some(true),
            )
        }, self)
    }

    pub(super) fn can_convert_import_declaration_to_type_only(
        &self,
        statement: Id<Node>, /*Statement*/
    ) -> io::Result<bool> {
        Ok(is_import_declaration(&statement.ref_(self))
            && matches!(
                statement.ref_(self).as_import_declaration().import_clause,
                Some(statement_import_clause) if !statement_import_clause.ref_(self).as_import_clause().is_type_only &&
                    self.import_clause_contains_referenced_import(statement_import_clause)? &&
                    !self.is_referenced_alias_declaration(statement_import_clause, Some(true))? &&
                    !self.import_clause_contains_const_enum_used_as_value(statement_import_clause)?
            ))
    }

    pub(super) fn can_convert_import_equals_declaration_to_type_only(
        &self,
        statement: Id<Node>, /*Statement*/
    ) -> io::Result<bool> {
        Ok(is_import_equals_declaration(&statement.ref_(self)) && {
            let statement_ref = statement.ref_(self);
            let statement_as_import_equals_declaration = statement_ref.as_import_equals_declaration();
            is_external_module_reference(&statement_as_import_equals_declaration.module_reference.ref_(self))
                && !statement_as_import_equals_declaration.is_type_only
                && matches!(
                    self.get_symbol_of_node(statement)?.unwrap().ref_(self).maybe_is_referenced(),
                    Some(is_referenced) if is_referenced != SymbolFlags::None
                )
                && !self.is_referenced_alias_declaration(statement, Some(false))?
                && (*self.get_symbol_links(self.get_symbol_of_node(statement)?.unwrap()).ref_(self))
                    .borrow()
                    .const_enum_referenced
                    != Some(true)
        })
    }

    pub(super) fn check_imports_for_type_only_conversion(
        &self,
        source_file: Id<Node>, /*SourceFile*/
    ) -> io::Result<()> {
        for &statement in &*source_file.ref_(self).as_source_file().statements().ref_(self) {
            if self.can_convert_import_declaration_to_type_only(statement)?
                || self.can_convert_import_equals_declaration_to_type_only(statement)?
            {
                self.error(
                    Some(statement),
                    &Diagnostics::This_import_is_never_used_as_a_value_and_must_use_import_type_because_importsNotUsedAsValues_is_set_to_error,
                    None,
                );
            }
        }

        Ok(())
    }

    pub(super) fn check_export_specifier(
        &self,
        node: Id<Node>, /*ExportSpecifier*/
    ) -> io::Result<()> {
        self.check_alias_symbol(node)?;
        let node_ref = node.ref_(self);
        let node_as_export_specifier = node_ref.as_export_specifier();
        if get_emit_declarations(&self.compiler_options.ref_(self)) {
            self.collect_linked_aliases(
                node_as_export_specifier
                    .property_name
                    .unwrap_or(node_as_export_specifier.name),
                Some(true),
            )?;
        }
        if node
            .ref_(self).parent()
            .ref_(self).parent()
            .ref_(self).as_export_declaration()
            .module_specifier
            .is_none()
        {
            let exported_name = node_as_export_specifier
                .property_name
                .unwrap_or(node_as_export_specifier.name);
            let symbol = self.resolve_name_(
                Some(exported_name),
                &exported_name.ref_(self).as_identifier().escaped_text,
                SymbolFlags::Value
                    | SymbolFlags::Type
                    | SymbolFlags::Namespace
                    | SymbolFlags::Alias,
                None,
                Option::<Id<Node>>::None,
                true,
                None,
            )?;
            if matches!(
                symbol,
                Some(symbol) if symbol == self.undefined_symbol()
                    || symbol == self.global_this_symbol()
                    || matches!(
                        symbol.ref_(self).maybe_declarations().as_ref(),
                        Some(symbol_declarations) if self.is_global_source_file(
                            self.get_declaration_container(
                                symbol_declarations[0]
                            )
                        )
                    )
            ) {
                self.error(
                    Some(exported_name),
                    &Diagnostics::Cannot_export_0_Only_local_declarations_can_be_exported_from_a_module,
                    Some(vec![
                        id_text(&exported_name.ref_(self)).to_owned()
                    ])
                );
            } else {
                self.mark_export_as_referenced(node)?;
                let target = symbol.try_map(|symbol| -> io::Result<_> {
                    Ok(
                        if symbol.ref_(self).flags().intersects(SymbolFlags::Alias) {
                            self.resolve_alias(symbol)?
                        } else {
                            symbol.clone()
                        },
                    )
                })?;
                if match target {
                    None => true,
                    Some(target) => {
                        target == self.unknown_symbol()
                            || target.ref_(self).flags().intersects(SymbolFlags::Value)
                    }
                } {
                    self.check_expression_cached(
                        node_as_export_specifier
                            .property_name
                            .unwrap_or(node_as_export_specifier.name),
                        None,
                    )?;
                }
            }
        } else {
            if get_es_module_interop(&self.compiler_options.ref_(self)) == Some(true)
                && self.module_kind != ModuleKind::System
                && (self.module_kind < ModuleKind::ES2015
                    || get_source_file_of_node(node, self)
                        .ref_(self).as_source_file()
                        .maybe_implied_node_format()
                        == Some(ModuleKind::CommonJS))
                && id_text(
                    &node_as_export_specifier
                        .property_name
                        .unwrap_or(node_as_export_specifier.name).ref_(self),
                ) == "default"
            {
                self.check_external_emit_helpers(node, ExternalEmitHelpers::ImportDefault)?;
            }
        }

        Ok(())
    }

    pub(super) fn check_export_assignment(
        &self,
        node: Id<Node>, /*ExportAssignment*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_export_assignment = node_ref.as_export_assignment();
        let illegal_context_message = if node_as_export_assignment.is_export_equals == Some(true) {
            &*Diagnostics::An_export_assignment_must_be_at_the_top_level_of_a_file_or_module_declaration
        } else {
            &*Diagnostics::A_default_export_must_be_at_the_top_level_of_a_file_or_module_declaration
        };
        if self.check_grammar_module_element_context(node, illegal_context_message) {
            return Ok(());
        }

        let container = if node.ref_(self).parent().ref_(self).kind() == SyntaxKind::SourceFile {
            node.ref_(self).parent()
        } else {
            node.ref_(self).parent().ref_(self).parent()
        };
        if container.ref_(self).kind() == SyntaxKind::ModuleDeclaration && !is_ambient_module(container, self) {
            if node_as_export_assignment.is_export_equals == Some(true) {
                self.error(
                    Some(node),
                    &Diagnostics::An_export_assignment_cannot_be_used_in_a_namespace,
                    None,
                );
            } else {
                self.error(
                    Some(node),
                    &Diagnostics::A_default_export_can_only_be_used_in_an_ECMAScript_style_module,
                    None,
                );
            }

            return Ok(());
        }
        if !self.check_grammar_decorators_and_modifiers(node) && has_effective_modifiers(node, self) {
            self.grammar_error_on_first_token(
                node,
                &Diagnostics::An_export_assignment_cannot_have_modifiers,
                None,
            );
        }

        let type_annotation_node = get_effective_type_annotation_node(node, self);
        if let Some(type_annotation_node) = type_annotation_node {
            self.check_type_assignable_to(
                self.check_expression_cached(node_as_export_assignment.expression, None)?,
                self.get_type_from_type_node_(type_annotation_node)?,
                Some(node_as_export_assignment.expression),
                None,
                None,
                None,
            )?;
        }

        if node_as_export_assignment.expression.ref_(self).kind() == SyntaxKind::Identifier {
            let id = node_as_export_assignment.expression;
            let sym =
                self.resolve_entity_name(id, SymbolFlags::All, Some(true), Some(true), Some(node))?;
            if let Some(sym) = sym {
                self.mark_alias_referenced(sym, id)?;
                let target = if sym.ref_(self).flags().intersects(SymbolFlags::Alias) {
                    self.resolve_alias(sym)?
                } else {
                    sym.clone()
                };
                if target == self.unknown_symbol()
                    || target.ref_(self).flags().intersects(SymbolFlags::Value)
                {
                    self.check_expression_cached(node_as_export_assignment.expression, None)?;
                }
            } else {
                self.check_expression_cached(node_as_export_assignment.expression, None)?;
            }

            if get_emit_declarations(&self.compiler_options.ref_(self)) {
                self.collect_linked_aliases(node_as_export_assignment.expression, Some(true))?;
            }
        } else {
            self.check_expression_cached(node_as_export_assignment.expression, None)?;
        }

        self.check_external_module_exports(container)?;

        if node.ref_(self).flags().intersects(NodeFlags::Ambient)
            && !is_entity_name_expression(node_as_export_assignment.expression, self)
        {
            self.grammar_error_on_node(
                node_as_export_assignment.expression,
                &Diagnostics::The_expression_of_an_export_assignment_must_be_an_identifier_or_qualified_name_in_an_ambient_context,
                None,
            );
        }

        if node_as_export_assignment.is_export_equals == Some(true)
            && !node.ref_(self).flags().intersects(NodeFlags::Ambient)
        {
            if self.module_kind >= ModuleKind::ES2015
                && get_source_file_of_node(node, self)
                    .ref_(self).as_source_file()
                    .maybe_implied_node_format()
                    != Some(ModuleKind::CommonJS)
            {
                self.grammar_error_on_node(
                    node,
                    &Diagnostics::Export_assignment_cannot_be_used_when_targeting_ECMAScript_modules_Consider_using_export_default_or_another_module_format_instead,
                    None,
                );
            } else if self.module_kind == ModuleKind::System {
                self.grammar_error_on_node(
                    node,
                    &Diagnostics::Export_assignment_is_not_supported_when_module_flag_is_system,
                    None,
                );
            }
        }

        Ok(())
    }

    pub(super) fn has_exported_members(&self, module_symbol: Id<Symbol>) -> bool {
        for_each_entry_bool(&*module_symbol.ref_(self).exports().ref_(self), |_, id| {
            id != "export="
        })
    }

    pub(super) fn check_external_module_exports(
        &self,
        node: Id<Node>, /*SourceFile | ModuleDeclaration*/
    ) -> io::Result<()> {
        let module_symbol = self.get_symbol_of_node(node)?.unwrap();
        let links = self.get_symbol_links(module_symbol);
        if (*links.ref_(self)).borrow().exports_checked != Some(true) {
            let export_equals_symbol = module_symbol.ref_(self).exports()
                .ref_(self)
                .get("export=")
                .cloned();
            if let Some(export_equals_symbol) = export_equals_symbol {
                if self.has_exported_members(module_symbol) {
                    let declaration = self
                        .get_declaration_of_alias_symbol(export_equals_symbol)?
                        .or_else(|| export_equals_symbol.ref_(self).maybe_value_declaration());
                    if let Some(declaration) = declaration.filter(|&declaration| {
                        !self.is_top_level_in_external_module_augmentation(declaration)
                            && !is_in_js_file(Some(&declaration.ref_(self)))
                    }) {
                        self.error(
                            Some(declaration),
                            &Diagnostics::An_export_assignment_cannot_be_used_in_a_module_with_other_exported_elements,
                            None,
                        );
                    }
                }
            }
            let exports = self.get_exports_of_module_(module_symbol)?;
            // if (exports) {
            let exports = exports.ref_(self);
            for (id, &symbol) in &*exports {
                let symbol_ref = symbol.ref_(self);
                let declarations = symbol_ref.maybe_declarations();
                let flags = symbol.ref_(self).flags();
                if id == "__export" {
                    continue;
                }
                if flags
                    .intersects(SymbolFlags::Namespace | SymbolFlags::Interface | SymbolFlags::Enum)
                {
                    continue;
                }
                let exported_declarations_count =
                    count_where(declarations.as_deref(), |declaration: &Id<Node>, _| {
                        is_not_overload_and_not_accessor(&declaration.ref_(self))
                    });
                if flags.intersects(SymbolFlags::TypeAlias) && exported_declarations_count <= 2 {
                    continue;
                }
                if exported_declarations_count > 1 {
                    if !self.is_duplicated_common_js_export(declarations.as_deref()) {
                        for &declaration in declarations.as_ref().unwrap() {
                            if is_not_overload(&declaration.ref_(self)) {
                                self.diagnostics().add(
                                    self.alloc_diagnostic(create_diagnostic_for_node(
                                        declaration,
                                        &Diagnostics::Cannot_redeclare_exported_variable_0,
                                        Some(vec![unescape_leading_underscores(id).to_owned()]),
                                        self,
                                    )
                                    .into()),
                                );
                            }
                        }
                    }
                }
            }
            // }
            links.ref_(self).borrow_mut().exports_checked = Some(true);
        }

        Ok(())
    }
}
