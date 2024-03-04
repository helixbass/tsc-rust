use std::{io, ptr};

use id_arena::Id;

use crate::{
    declaration_name_to_string, entity_name_to_string, file_extension_is, find, find_ancestor,
    find_best_pattern_match, get_alias_declaration_from_name, get_assigned_expando_initializer,
    get_assignment_declaration_kind, get_check_flags, get_declared_expando_initializer,
    get_directory_path, get_effective_jsdoc_host, get_emit_module_resolution_kind,
    get_expando_initializer, get_first_identifier, get_jsdoc_host, get_mode_for_usage_location,
    get_normalized_absolute_path, get_resolution_diagnostic, get_resolved_module,
    get_source_file_of_node, has_extension, has_json_module_emit_enabled,
    has_only_expression_initializer, is_assignment_declaration, is_binary_expression,
    is_entity_name, is_export_declaration, is_expression_statement,
    is_external_module_import_equals_declaration, is_function_like, is_import_call,
    is_import_declaration, is_import_equals_declaration, is_in_js_file,
    is_internal_module_import_equals_declaration, is_jsdoc_node, is_jsdoc_type_alias,
    is_literal_import_type_node, is_module_declaration, is_object_literal_method,
    is_property_access_expression, is_property_assignment, is_qualified_name,
    is_right_side_of_qualified_name_or_property_access, is_string_literal_like,
    is_type_of_expression, is_type_only_import_or_export_declaration, is_variable_declaration,
    node_is_missing, node_is_synthesized, path_is_relative, released, remove_extension,
    remove_prefix, resolution_extension_is_ts_or_json, return_ok_default_if_none,
    return_ok_none_if_none, starts_with, try_extract_ts_extension, unescape_leading_underscores,
    AssignmentDeclarationKind, CheckFlags, Debug_, DiagnosticMessage, Diagnostics, Extension,
    FindAncestorCallbackReturn, HasInitializerInterface, InArena, InternalSymbolName, ModuleKind,
    ModuleResolutionKind, ModuleSpecifierResolutionHost, NamedDeclarationInterface, Node,
    NodeFlags, NodeInterface, Symbol, SymbolFlags, SymbolFormatFlags, SymbolInterface, SymbolLinks,
    SyntaxKind, TypeChecker, TypeCheckerHost,
};

impl TypeChecker {
    pub(super) fn get_target_of_property_assignment(
        &self,
        node: Id<Node>, /*PropertyAssignment*/
        dont_recursively_resolve: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        let expression = node.ref_(self).as_property_assignment().initializer;
        self.get_target_of_alias_like_expression(expression, dont_recursively_resolve)
    }

    pub(super) fn get_target_of_access_expression(
        &self,
        node: Id<Node>, /*AccessExpression*/
        dont_recursively_resolve: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        let node_parent = node.ref_(self).parent();
        if !(is_binary_expression(&node_parent.ref_(self))) {
            return Ok(None);
        }
        let node_parent_ref = node_parent.ref_(self);
        let node_parent_as_binary_expression = node_parent_ref.as_binary_expression();
        if !(node_parent_as_binary_expression.left == node
            && node_parent_as_binary_expression
                .operator_token
                .ref_(self)
                .kind()
                == SyntaxKind::EqualsToken)
        {
            return Ok(None);
        }

        self.get_target_of_alias_like_expression(
            node_parent_as_binary_expression.right,
            dont_recursively_resolve,
        )
    }

    pub(super) fn get_target_of_alias_declaration(
        &self,
        node: Id<Node>, /*Declaration*/
        dont_recursively_resolve: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let dont_recursively_resolve = dont_recursively_resolve.unwrap_or(false);
        Ok(match released!(node.ref_(self).kind()) {
            SyntaxKind::ImportEqualsDeclaration | SyntaxKind::VariableDeclaration => {
                self.get_target_of_import_equals_declaration(node, dont_recursively_resolve)?
            }
            SyntaxKind::ImportClause => {
                self.get_target_of_import_clause(node, dont_recursively_resolve)?
            }
            SyntaxKind::NamespaceImport => {
                self.get_target_of_namespace_import(node, dont_recursively_resolve)?
            }
            SyntaxKind::NamespaceExport => {
                self.get_target_of_namespace_export(node, dont_recursively_resolve)?
            }
            SyntaxKind::ImportSpecifier | SyntaxKind::BindingElement => {
                self.get_target_of_import_specifier(node, dont_recursively_resolve)?
            }
            SyntaxKind::ExportSpecifier => self.get_target_of_export_specifier(
                node,
                SymbolFlags::Value | SymbolFlags::Type | SymbolFlags::Namespace,
                Some(dont_recursively_resolve),
            )?,
            SyntaxKind::ExportAssignment | SyntaxKind::BinaryExpression => {
                self.get_target_of_export_assignment(node, dont_recursively_resolve)?
            }
            SyntaxKind::NamespaceExportDeclaration => Some(
                self.get_target_of_namespace_export_declaration(node, dont_recursively_resolve)?,
            ),
            SyntaxKind::ShorthandPropertyAssignment => self.resolve_entity_name(
                node.ref_(self).as_shorthand_property_assignment().name(),
                SymbolFlags::Value | SymbolFlags::Type | SymbolFlags::Namespace,
                Some(true),
                Some(dont_recursively_resolve),
                Option::<Id<Node>>::None,
            )?,
            SyntaxKind::PropertyAssignment => {
                self.get_target_of_property_assignment(node, dont_recursively_resolve)?
            }
            SyntaxKind::ElementAccessExpression | SyntaxKind::PropertyAccessExpression => {
                self.get_target_of_access_expression(node, dont_recursively_resolve)?
            }
            _ => Debug_.fail(None),
        })
    }

    pub(super) fn is_non_local_alias(
        &self,
        symbol: Option<Id<Symbol>>,
        excludes: Option<SymbolFlags>,
    ) -> bool {
        let excludes =
            excludes.unwrap_or(SymbolFlags::Value | SymbolFlags::Type | SymbolFlags::Namespace);
        if symbol.is_none() {
            return false;
        }
        let symbol = symbol.unwrap();
        symbol.ref_(self).flags() & (SymbolFlags::Alias | excludes) == SymbolFlags::Alias
            || symbol.ref_(self).flags().intersects(SymbolFlags::Alias)
                && symbol
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::Assignment)
    }

    pub(super) fn resolve_symbol(
        &self,
        symbol: Option<Id<Symbol>>,
        dont_resolve_alias: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        Ok(
            if !matches!(dont_resolve_alias, Some(true))
                && self.is_non_local_alias(symbol.clone(), None)
            {
                Some(self.resolve_alias(symbol.unwrap())?)
            } else {
                symbol
            },
        )
    }

    pub(super) fn resolve_alias(&self, symbol: Id<Symbol>) -> io::Result<Id<Symbol>> {
        Debug_.assert(
            symbol.ref_(self).flags().intersects(SymbolFlags::Alias),
            Some("Should only get Alias here."),
        );
        let links = self.get_symbol_links(symbol);
        if links.ref_(self).target.is_none() {
            links.ref_mut(self).target = Some(self.resolving_symbol());
            let node = self.get_declaration_of_alias_symbol(symbol)?;
            if node.is_none() {
                Debug_.fail(None);
            }
            let node = node.unwrap();
            let target = self.get_target_of_alias_declaration(node, None)?;
            if links.ref_(self).target.unwrap() == self.resolving_symbol() {
                links.ref_mut(self).target = Some(target.unwrap_or_else(|| self.unknown_symbol()));
            } else {
                self.error(
                    Some(node),
                    &Diagnostics::Circular_definition_of_import_alias_0,
                    Some(vec![self.symbol_to_string_(
                        symbol,
                        Option::<Id<Node>>::None,
                        None,
                        None,
                        None,
                    )?]),
                );
            }
        } else if links.ref_(self).target.unwrap() == self.resolving_symbol() {
            links.ref_mut(self).target = Some(self.unknown_symbol());
        }
        let ret = links.ref_(self).target.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn try_resolve_alias(&self, symbol: Id<Symbol>) -> io::Result<Option<Id<Symbol>>> {
        let links = self.get_symbol_links(symbol);
        if links.ref_(self).target != Some(self.resolving_symbol()) {
            return Ok(Some(self.resolve_alias(symbol)?));
        }

        Ok(None)
    }

    pub(super) fn mark_symbol_of_alias_declaration_if_type_only(
        &self,
        alias_declaration: Option<Id<Node> /*Declaration*/>,
        immediate_target: Option<Id<Symbol>>,
        final_target: Option<Id<Symbol>>,
        overwrite_empty: bool,
    ) -> io::Result<bool> {
        let Some(alias_declaration) = alias_declaration else {
            return Ok(false);
        };
        if is_property_access_expression(&alias_declaration.ref_(self)) {
            return Ok(false);
        }

        let source_symbol = self.get_symbol_of_node(alias_declaration)?.unwrap();
        if is_type_only_import_or_export_declaration(alias_declaration, self) {
            let links = self.get_symbol_links(source_symbol);
            links.ref_mut(self).type_only_declaration = Some(Some(alias_declaration));
            return Ok(true);
        }

        let links = self.get_symbol_links(source_symbol);
        Ok(self.mark_symbol_of_alias_declaration_if_type_only_worker(
            links,
            immediate_target,
            overwrite_empty,
        ) || self.mark_symbol_of_alias_declaration_if_type_only_worker(
            links,
            final_target,
            overwrite_empty,
        ))
    }

    pub(super) fn mark_symbol_of_alias_declaration_if_type_only_worker(
        &self,
        alias_declaration_links: Id<SymbolLinks>,
        target: Option<Id<Symbol>>,
        overwrite_empty: bool,
    ) -> bool {
        if let Some(target) = target {
            if alias_declaration_links
                .ref_(self)
                .type_only_declaration
                .is_none()
                || overwrite_empty
                    && alias_declaration_links.ref_(self).type_only_declaration == Some(None)
            {
                let export_symbol = target
                    .ref_(self)
                    .maybe_exports()
                    .and_then(|exports| {
                        exports
                            .ref_(self)
                            .get(InternalSymbolName::ExportEquals)
                            .cloned()
                    })
                    .unwrap_or_else(|| target);
                let type_only = export_symbol
                    .ref_(self)
                    .maybe_declarations()
                    .as_deref()
                    .and_then(|declarations| {
                        find(declarations, |&declaration: &Id<Node>, _| {
                            is_type_only_import_or_export_declaration(declaration, self)
                        })
                        .map(Clone::clone)
                    });
                alias_declaration_links.ref_mut(self).type_only_declaration =
                    Some(type_only.or_else(|| {
                        self.get_symbol_links(export_symbol)
                            .ref_(self)
                            .type_only_declaration
                            .flatten()
                    }));
            }
        }
        matches!(
            alias_declaration_links.ref_(self).type_only_declaration,
            Some(Some(_))
        )
    }

    pub(super) fn get_type_only_alias_declaration(
        &self,
        symbol: Id<Symbol>,
    ) -> Option<Id<Node /*TypeOnlyAliasDeclaration*/>> {
        if !symbol.ref_(self).flags().intersects(SymbolFlags::Alias) {
            return None;
        }
        let links = self.get_symbol_links(symbol);
        let ret = links.ref_(self).type_only_declaration.flatten();
        ret
    }

    pub(super) fn mark_export_as_referenced(
        &self,
        node: Id<Node>, /*ImportEqualsDeclaration | ExportSpecifier*/
    ) -> io::Result<()> {
        let symbol = self.get_symbol_of_node(node)?.unwrap();
        let target = self.resolve_alias(symbol)?;
        // if (target) {
        let mark_alias = target == self.unknown_symbol()
            || target.ref_(self).flags().intersects(SymbolFlags::Value)
                && !self.is_const_enum_or_const_enum_only_module(target)
                && self.get_type_only_alias_declaration(symbol).is_none();

        if mark_alias {
            self.mark_alias_symbol_as_referenced(symbol)?;
        }
        // }

        Ok(())
    }

    pub(super) fn mark_alias_symbol_as_referenced(&self, symbol: Id<Symbol>) -> io::Result<()> {
        let links = self.get_symbol_links(symbol);
        if links.ref_(self).referenced != Some(true) {
            links.ref_mut(self).referenced = Some(true);
            let Some(node) = self.get_declaration_of_alias_symbol(symbol)? else {
                Debug_.fail(None);
            };
            if is_internal_module_import_equals_declaration(node, self) {
                let target = self.resolve_symbol(Some(symbol), None)?.unwrap();
                if target == self.unknown_symbol()
                    || target.ref_(self).flags().intersects(SymbolFlags::Value)
                {
                    self.check_expression_cached(
                        released!(
                            node.ref_(self)
                                .as_import_equals_declaration()
                                .module_reference
                        ),
                        None,
                    )?;
                }
            }
        }

        Ok(())
    }

    pub(super) fn mark_const_enum_alias_as_referenced(&self, symbol: Id<Symbol>) {
        let links = self.get_symbol_links(symbol);
        if links.ref_(self).const_enum_referenced != Some(true) {
            links.ref_mut(self).const_enum_referenced = Some(true);
        }
    }

    pub(super) fn get_symbol_of_part_of_right_hand_side_of_import_equals(
        &self,
        mut entity_name: Id<Node>, /*EntityName*/
        dont_resolve_alias: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        if entity_name.ref_(self).kind() == SyntaxKind::Identifier
            && is_right_side_of_qualified_name_or_property_access(entity_name, self)
        {
            entity_name = entity_name.ref_(self).parent();
        }
        Ok(
            if entity_name.ref_(self).kind() == SyntaxKind::Identifier
                || entity_name.ref_(self).parent().ref_(self).kind() == SyntaxKind::QualifiedName
            {
                self.resolve_entity_name(
                    entity_name,
                    SymbolFlags::Namespace,
                    Some(false),
                    dont_resolve_alias,
                    None,
                )?
            } else {
                Debug_.assert(
                    entity_name.ref_(self).parent().ref_(self).kind()
                        == SyntaxKind::ImportEqualsDeclaration,
                    None,
                );
                self.resolve_entity_name(
                    entity_name,
                    SymbolFlags::Value | SymbolFlags::Type | SymbolFlags::Namespace,
                    Some(false),
                    dont_resolve_alias,
                    None,
                )?
            },
        )
    }

    pub(super) fn get_fully_qualified_name(
        &self,
        symbol: Id<Symbol>,
        containing_location: Option<Id<Node>>,
    ) -> io::Result<String> {
        Ok(
            if let Some(symbol_parent) = symbol.ref_(self).maybe_parent() {
                format!(
                    "{}.{}",
                    self.get_fully_qualified_name(symbol_parent, containing_location)?,
                    self.symbol_to_string_(symbol, Option::<Id<Node>>::None, None, None, None)?
                )
            } else {
                self.symbol_to_string_(
                    symbol,
                    containing_location,
                    None,
                    Some(
                        SymbolFormatFlags::DoNotIncludeSymbolChain
                            | SymbolFormatFlags::AllowAnyNodeKind,
                    ),
                    None,
                )?
            },
        )
    }

    pub(super) fn get_containing_qualified_name_node(
        &self,
        mut node: Id<Node>, /*QualifiedName*/
    ) -> Id<Node> {
        while is_qualified_name(&node.ref_(self).parent().ref_(self)) {
            node = node.ref_(self).parent();
        }
        node
    }

    pub(super) fn try_get_qualified_name_as_value(
        &self,
        node: Id<Node>, /*QualifiedName*/
    ) -> io::Result<Option<Id<Symbol>>> {
        let mut left = get_first_identifier(node, self);
        let mut symbol = return_ok_none_if_none!(self.resolve_name_(
            Some(left),
            &left.ref_(self).as_identifier().escaped_text,
            SymbolFlags::Value,
            None,
            Some(left.clone()),
            true,
            None,
        )?);
        while is_qualified_name(&left.ref_(self).parent().ref_(self)) {
            let type_ = self.get_type_of_symbol(symbol)?;
            symbol = return_ok_none_if_none!(self.get_property_of_type_(
                type_,
                &left
                    .ref_(self)
                    .parent()
                    .ref_(self)
                    .as_qualified_name()
                    .right
                    .ref_(self)
                    .as_identifier()
                    .escaped_text,
                None,
            )?);
            left = left.ref_(self).parent();
        }
        Ok(Some(symbol))
    }

    pub(super) fn resolve_entity_name(
        &self,
        name: Id<Node>, /*EntityNameOrEntityNameExpression*/
        meaning: SymbolFlags,
        ignore_errors: Option<bool>,
        dont_resolve_alias: Option<bool>,
        location: Option<Id<Node>>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let ignore_errors_unwrapped = ignore_errors.unwrap_or(false);
        let dont_resolve_alias = dont_resolve_alias.unwrap_or(false);
        if node_is_missing(Some(&name.ref_(self))) {
            return Ok(None);
        }

        let namespace_meaning = SymbolFlags::Namespace
            | if is_in_js_file(Some(&name.ref_(self))) {
                meaning & SymbolFlags::Value
            } else {
                SymbolFlags::None
            };
        let symbol: Option<Id<Symbol>>;
        if name.ref_(self).kind() == SyntaxKind::Identifier {
            let message = if meaning == namespace_meaning || node_is_synthesized(&*name.ref_(self))
            {
                &Diagnostics::Cannot_find_namespace_0
            } else {
                self.get_cannot_find_name_diagnostic_for_name(get_first_identifier(name, self))
            };
            let symbol_from_js_prototype = if is_in_js_file(Some(&name.ref_(self)))
                && !node_is_synthesized(&*name.ref_(self))
            {
                self.resolve_entity_name_from_assignment_declaration(name, meaning)?
            } else {
                None
            };
            symbol = self.get_merged_symbol(self.resolve_name_(
                Some(location.unwrap_or(name)),
                &released!(name.ref_(self).as_identifier().escaped_text.clone()),
                meaning,
                if ignore_errors_unwrapped || symbol_from_js_prototype.is_some() {
                    None
                } else {
                    Some(message)
                },
                Some(name),
                true,
                Some(false),
            )?);
            if symbol.is_none() {
                return Ok(self.get_merged_symbol(symbol_from_js_prototype));
            }
        } else if matches!(
            name.ref_(self).kind(),
            SyntaxKind::QualifiedName | SyntaxKind::PropertyAccessExpression
        ) {
            let left = if name.ref_(self).kind() == SyntaxKind::QualifiedName {
                name.ref_(self).as_qualified_name().left
            } else {
                name.ref_(self).as_property_access_expression().expression
            };
            let right = if name.ref_(self).kind() == SyntaxKind::QualifiedName {
                name.ref_(self).as_qualified_name().right
            } else {
                name.ref_(self).as_property_access_expression().name
            };
            let namespace = self.resolve_entity_name(
                left,
                namespace_meaning,
                ignore_errors,
                Some(false),
                location,
            )?;
            if namespace.is_none() {
                return Ok(None);
            }
            let mut namespace = namespace.unwrap();
            if node_is_missing(Some(&right.ref_(self))) {
                return Ok(None);
            } else if namespace == self.unknown_symbol() {
                return Ok(Some(namespace));
            }
            if let Some(namespace_value_declaration) =
                namespace.ref_(self).maybe_value_declaration()
            {
                if is_in_js_file(Some(&namespace_value_declaration.ref_(self)))
                    && is_variable_declaration(&namespace_value_declaration.ref_(self))
                {
                    if let Some(namespace_value_declaration_initializer) =
                        namespace_value_declaration
                            .ref_(self)
                            .as_variable_declaration()
                            .maybe_initializer()
                    {
                        if self.is_common_js_require(namespace_value_declaration_initializer)? {
                            let module_name = namespace_value_declaration_initializer
                                .ref_(self)
                                .as_call_expression()
                                .arguments
                                .ref_(self)[0];
                            let module_sym =
                                self.resolve_external_module_name_(module_name, module_name, None)?;
                            if let Some(module_sym) = module_sym {
                                let resolved_module_symbol =
                                    self.resolve_external_module_symbol(Some(module_sym), None)?;
                                if let Some(resolved_module_symbol) = resolved_module_symbol {
                                    namespace = resolved_module_symbol;
                                }
                            }
                        }
                    }
                }
            }
            symbol = self.get_merged_symbol(self.get_symbol(
                self.get_exports_of_symbol(namespace)?,
                &right.ref_(self).as_identifier().escaped_text,
                meaning,
            )?);
            if symbol.is_none() {
                if !ignore_errors_unwrapped {
                    let namespace_name =
                        self.get_fully_qualified_name(namespace, Option::<Id<Node>>::None)?;
                    let declaration_name = declaration_name_to_string(Some(right), self);
                    let suggestion_for_nonexistent_module =
                        self.get_suggested_symbol_for_nonexistent_module(right, namespace)?;
                    if let Some(suggestion_for_nonexistent_module) =
                        suggestion_for_nonexistent_module
                    {
                        self.error(
                            Some(right),
                            &Diagnostics::_0_has_no_exported_member_named_1_Did_you_mean_2,
                            Some(vec![
                                namespace_name,
                                declaration_name.into_owned(),
                                self.symbol_to_string_(
                                    suggestion_for_nonexistent_module,
                                    Option::<Id<Node>>::None,
                                    None,
                                    None,
                                    None,
                                )?,
                            ]),
                        );
                        return Ok(None);
                    }

                    let containing_qualified_name = if is_qualified_name(&name.ref_(self)) {
                        Some(self.get_containing_qualified_name_node(name))
                    } else {
                        None
                    };
                    let can_suggest_typeof = self.maybe_global_object_type().is_some()
                        && meaning.intersects(SymbolFlags::Type)
                        && matches!(
                            containing_qualified_name,
                            Some(containing_qualified_name) if !is_type_of_expression(&containing_qualified_name.ref_(self).parent().ref_(self))
                                && self.try_get_qualified_name_as_value(containing_qualified_name)?.is_some()
                        );
                    if can_suggest_typeof {
                        self.error(
                            containing_qualified_name,
                            &Diagnostics::_0_refers_to_a_value_but_is_being_used_as_a_type_here_Did_you_mean_typeof_0,
                            Some(vec![entity_name_to_string(containing_qualified_name.unwrap(), self).into_owned()])
                        );
                        return Ok(None);
                    }

                    if meaning.intersects(SymbolFlags::Namespace)
                        && is_qualified_name(&name.ref_(self).parent().ref_(self))
                    {
                        let exported_type_symbol = self.get_merged_symbol(self.get_symbol(
                            self.get_exports_of_symbol(namespace)?,
                            &right.ref_(self).as_identifier().escaped_text,
                            SymbolFlags::Type,
                        )?);
                        if let Some(exported_type_symbol) = exported_type_symbol {
                            self.error(
                                released!(Some(name.ref_(self).parent().ref_(self).as_qualified_name().right)),
                                &Diagnostics::Cannot_access_0_1_because_0_is_a_type_but_not_a_namespace_Did_you_mean_to_retrieve_the_type_of_the_property_1_in_0_with_0_1,
                                Some(vec![
                                    self.symbol_to_string_(exported_type_symbol, Option::<Id<Node>>::None, None, None, None)?,
                                    unescape_leading_underscores(&name.ref_(self).parent().ref_(self).as_qualified_name().right.ref_(self).as_identifier().escaped_text).to_owned()
                                ])
                            );
                            return Ok(None);
                        }
                    }

                    self.error(
                        Some(right),
                        &Diagnostics::Namespace_0_has_no_exported_member_1,
                        Some(vec![namespace_name, declaration_name.into_owned()]),
                    );
                }
                return Ok(None);
            }
        } else {
            Debug_.assert_never(name, Some("Unknown entity name kind."));
        }
        let symbol = symbol.unwrap();
        Debug_.assert(
            !get_check_flags(&symbol.ref_(self)).intersects(CheckFlags::Instantiated),
            Some("Should never get an instantiated symbol here."),
        );
        if !node_is_synthesized(&*name.ref_(self))
            && is_entity_name(&name.ref_(self))
            && (symbol.ref_(self).flags().intersects(SymbolFlags::Alias)
                || name.ref_(self).parent().ref_(self).kind() == SyntaxKind::ExportAssignment)
        {
            self.mark_symbol_of_alias_declaration_if_type_only(
                get_alias_declaration_from_name(name, self),
                Some(symbol),
                Option::<Id<Symbol>>::None,
                true,
            )?;
        }
        Ok(
            if symbol.ref_(self).flags().intersects(meaning) || dont_resolve_alias {
                Some(symbol)
            } else {
                Some(self.resolve_alias(symbol)?)
            },
        )
    }

    pub(super) fn resolve_entity_name_from_assignment_declaration(
        &self,
        name: Id<Node>, /*Identifier*/
        meaning: SymbolFlags,
    ) -> io::Result<Option<Id<Symbol>>> {
        if self.is_jsdoc_type_reference(name.ref_(self).parent()) {
            let secondary_location =
                self.get_assignment_declaration_location(name.ref_(self).parent())?;
            if let Some(secondary_location) = secondary_location {
                return self.resolve_name_(
                    Some(secondary_location),
                    &name.ref_(self).as_identifier().escaped_text,
                    meaning,
                    None,
                    Some(name),
                    true,
                    None,
                );
            }
        }
        Ok(None)
    }

    pub(super) fn get_assignment_declaration_location(
        &self,
        node: Id<Node>, /*TypeReferenceNode*/
    ) -> io::Result<Option<Id<Node>>> {
        let type_alias = find_ancestor(
            Some(node),
            |node| {
                if !(is_jsdoc_node(&node.ref_(self))
                    || node.ref_(self).flags().intersects(NodeFlags::JSDoc))
                {
                    FindAncestorCallbackReturn::Quit
                } else {
                    is_jsdoc_type_alias(&node.ref_(self)).into()
                }
            },
            self,
        );
        if type_alias.is_some() {
            return Ok(None);
        }
        let host = get_jsdoc_host(node, self);
        if let Some(host) = host {
            if is_expression_statement(&host.ref_(self)) {
                let host_ref = host.ref_(self);
                let host_as_expression_statement = host_ref.as_expression_statement();
                if is_binary_expression(&host_as_expression_statement.expression.ref_(self))
                    && get_assignment_declaration_kind(
                        host_as_expression_statement.expression,
                        self,
                    ) == AssignmentDeclarationKind::PrototypeProperty
                {
                    let symbol = self.get_symbol_of_node(
                        host_as_expression_statement
                            .expression
                            .ref_(self)
                            .as_binary_expression()
                            .left,
                    )?;
                    if let Some(symbol) = symbol {
                        return Ok(self.get_declaration_of_js_prototype_container(symbol));
                    }
                }
            }
        }
        if let Some(host) = host {
            if (is_object_literal_method(host, self) || is_property_assignment(&host.ref_(self)))
                && is_binary_expression(&host.ref_(self).parent().ref_(self).parent().ref_(self))
                && get_assignment_declaration_kind(
                    host.ref_(self).parent().ref_(self).parent(),
                    self,
                ) == AssignmentDeclarationKind::Prototype
            {
                let symbol = self.get_symbol_of_node(
                    host.ref_(self)
                        .parent()
                        .ref_(self)
                        .parent()
                        .ref_(self)
                        .as_binary_expression()
                        .left,
                )?;
                if let Some(symbol) = symbol {
                    return Ok(self.get_declaration_of_js_prototype_container(symbol));
                }
            }
        }
        let sig = get_effective_jsdoc_host(node, self);
        if let Some(sig) = sig {
            if is_function_like(Some(&sig.ref_(self))) {
                let symbol = self.get_symbol_of_node(sig)?;
                return Ok(symbol.and_then(|symbol| symbol.ref_(self).maybe_value_declaration()));
            }
        }
        Ok(None)
    }

    pub(super) fn get_declaration_of_js_prototype_container(
        &self,
        symbol: Id<Symbol>,
    ) -> Option<Id<Node>> {
        let decl = symbol
            .ref_(self)
            .maybe_parent()
            .unwrap()
            .ref_(self)
            .maybe_value_declaration()?;
        let initializer = if is_assignment_declaration(&decl.ref_(self)) {
            get_assigned_expando_initializer(Some(decl), self)
        } else if has_only_expression_initializer(&decl.ref_(self)) {
            get_declared_expando_initializer(decl, self)
        } else {
            None
        };
        Some(initializer.unwrap_or(decl))
    }

    pub(super) fn get_expando_symbol(&self, symbol: Id<Symbol>) -> io::Result<Option<Id<Symbol>>> {
        let decl = return_ok_default_if_none!(symbol.ref_(self).maybe_value_declaration());
        if !is_in_js_file(Some(&decl.ref_(self)))
            || symbol.ref_(self).flags().intersects(SymbolFlags::TypeAlias)
            || get_expando_initializer(decl, false, self).is_some()
        {
            return Ok(None);
        }
        let init = if is_variable_declaration(&decl.ref_(self)) {
            get_declared_expando_initializer(decl, self)
        } else {
            get_assigned_expando_initializer(Some(decl), self)
        };
        if let Some(init) = init {
            let init_symbol = self.get_symbol_of_node(init)?;
            if let Some(init_symbol) = init_symbol {
                return self.merge_js_symbols(init_symbol, Some(symbol));
            }
        }
        Ok(None)
    }

    pub(super) fn resolve_external_module_name_(
        &self,
        location: Id<Node>,
        module_reference_expression: Id<Node>, /*Expression*/
        ignore_errors: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let is_classic = get_emit_module_resolution_kind(&self.compiler_options.ref_(self))
            == ModuleResolutionKind::Classic;
        let error_message = if is_classic {
            &*Diagnostics::Cannot_find_module_0_Did_you_mean_to_set_the_moduleResolution_option_to_node_or_to_add_aliases_to_the_paths_option
        } else {
            &*Diagnostics::Cannot_find_module_0_or_its_corresponding_type_declarations
        };
        self.resolve_external_module_name_worker(
            location,
            module_reference_expression,
            if matches!(ignore_errors, Some(true)) {
                None
            } else {
                Some(error_message)
            },
            None,
        )
    }

    pub(super) fn resolve_external_module_name_worker(
        &self,
        location: Id<Node>,
        module_reference_expression: Id<Node>, /*Expression*/
        module_not_found_error: Option<&DiagnosticMessage>,
        is_for_augmentation: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let is_for_augmentation = is_for_augmentation.unwrap_or(false);
        Ok(
            if is_string_literal_like(&module_reference_expression.ref_(self)) {
                self.resolve_external_module(
                    location,
                    &module_reference_expression
                        .ref_(self)
                        .as_literal_like_node()
                        .text(),
                    module_not_found_error,
                    module_reference_expression,
                    Some(is_for_augmentation),
                )?
            } else {
                None
            },
        )
    }

    pub(super) fn resolve_external_module(
        &self,
        location: Id<Node>,
        module_reference: &str,
        module_not_found_error: Option<&DiagnosticMessage>,
        error_node: Id<Node>,
        is_for_augmentation: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let is_for_augmentation = is_for_augmentation.unwrap_or(false);
        if starts_with(module_reference, "@types/") {
            let diag = &Diagnostics::Cannot_import_type_declaration_files_Consider_importing_0_instead_of_1;
            let without_at_type_prefix = remove_prefix(module_reference, "@types/");
            self.error(
                Some(error_node),
                diag,
                Some(vec![
                    without_at_type_prefix.to_owned(),
                    module_reference.to_owned(),
                ]),
            );
        }

        let ambient_module = self.try_find_ambient_module_(module_reference, true)?;
        if ambient_module.is_some() {
            return Ok(ambient_module);
        }
        let current_source_file = get_source_file_of_node(location, self);
        let context_specifier = if is_string_literal_like(&location.ref_(self)) {
            Some(location)
        } else {
            find_ancestor(Some(location), |node| is_import_call(node, self), self).and_then(|ancestor| ancestor.ref_(self).as_call_expression().arguments.ref_(self).get(0).copied()).or_else(|| {
                find_ancestor(Some(location), |node| is_import_declaration(&node.ref_(self)), self).map(|ancestor| ancestor.ref_(self).as_import_declaration().module_specifier)
            }).or_else(|| {
                find_ancestor(Some(location), |node| is_external_module_import_equals_declaration(node, self), self).map(|ancestor| ancestor.ref_(self).as_import_equals_declaration().module_reference.ref_(self).as_external_module_reference().expression)
            }).or_else(|| {
                find_ancestor(Some(location), |node| is_export_declaration(&node.ref_(self)), self).and_then(|ancestor| ancestor.ref_(self).as_export_declaration().module_specifier)
            }).or_else(|| {
                if is_module_declaration(&location.ref_(self)) {
                    Some(location)
                } else if matches!(
                    location.ref_(self).maybe_parent(),
                    Some(parent) if is_module_declaration(&parent.ref_(self)) && parent.ref_(self).as_module_declaration().name == location
                ) {
                    location.ref_(self).maybe_parent()
                } else {
                    None
                }.map(|node| node.ref_(self).as_module_declaration().name)
            }).or_else(|| {
                if is_literal_import_type_node(location, self) {
                    Some(location)
                } else {
                    None
                }.map(|node| node.ref_(self).as_import_type_node().argument.ref_(self).as_literal_type_node().literal)
            })
        };
        let mode = if matches!(
            context_specifier,
            Some(context_specifier) if is_string_literal_like(&context_specifier.ref_(self))
        ) {
            get_mode_for_usage_location(
                current_source_file
                    .ref_(self)
                    .as_source_file()
                    .maybe_implied_node_format(),
                context_specifier.unwrap(),
                self,
            )
        } else {
            current_source_file
                .ref_(self)
                .as_source_file()
                .maybe_implied_node_format()
        };
        let resolved_module = get_resolved_module(
            Some(&current_source_file.ref_(self)),
            module_reference,
            mode,
        );
        let resolution_diagnostic = resolved_module.as_ref().and_then(|resolved_module| {
            get_resolution_diagnostic(
                &self.compiler_options.ref_(self),
                &resolved_module.ref_(self),
            )
        });
        let source_file = resolved_module.as_ref().and_then(|resolved_module| {
            if resolution_diagnostic.is_none() {
                self.host
                    .ref_(self)
                    .get_source_file(&resolved_module.ref_(self).resolved_file_name)
            } else {
                None
            }
        });
        if let Some(source_file) = source_file {
            if let Some(source_file_symbol) = source_file.ref_(self).maybe_symbol() {
                let resolved_module = resolved_module.unwrap();
                if resolved_module.ref_(self).is_external_library_import == Some(true)
                    && !resolution_extension_is_ts_or_json(resolved_module.ref_(self).extension())
                {
                    self.error_on_implicit_any_module(
                        false,
                        error_node,
                        resolved_module,
                        module_reference,
                    );
                }
                if matches!(
                    get_emit_module_resolution_kind(&self.compiler_options.ref_(self)),
                    ModuleResolutionKind::Node12 | ModuleResolutionKind::NodeNext
                ) {
                    let is_sync_import = current_source_file
                        .ref_(self)
                        .as_source_file()
                        .maybe_implied_node_format()
                        == Some(ModuleKind::CommonJS)
                        && find_ancestor(Some(location), |node| is_import_call(node, self), self)
                            .is_none()
                        || find_ancestor(
                            Some(location),
                            |node| is_import_equals_declaration(&node.ref_(self)),
                            self,
                        )
                        .is_some();
                    if is_sync_import
                        && source_file
                            .ref_(self)
                            .as_source_file()
                            .maybe_implied_node_format()
                            == Some(ModuleKind::ESNext)
                    {
                        self.error(
                            Some(error_node),
                            &Diagnostics::Module_0_cannot_be_imported_using_this_construct_The_specifier_only_resolves_to_an_ES_module_which_cannot_be_imported_synchronously_Use_dynamic_import_instead,
                            Some(vec![module_reference.to_owned()])
                        );
                    }
                    if mode == Some(ModuleKind::ESNext)
                        && matches!(
                            self.compiler_options.ref_(self).resolve_json_module,
                            Some(true)
                        )
                        && resolved_module.ref_(self).extension() == Extension::Json
                    {
                        self.error(
                            Some(error_node),
                            &Diagnostics::JSON_imports_are_experimental_in_ES_module_mode_imports,
                            None,
                        );
                    }
                }
                return Ok(self.get_merged_symbol(Some(source_file_symbol)));
            }
            if module_not_found_error.is_some() {
                self.error(
                    Some(error_node),
                    &Diagnostics::File_0_is_not_a_module,
                    Some(vec![source_file
                        .ref_(self)
                        .as_source_file()
                        .file_name()
                        .to_owned()]),
                );
            }
            return Ok(None);
        }

        if let Some(pattern_ambient_modules) = self.maybe_pattern_ambient_modules().as_ref() {
            let pattern = find_best_pattern_match(
                pattern_ambient_modules,
                |pattern| pattern.ref_(self).pattern.clone(),
                module_reference,
            );
            if let Some(pattern) = pattern {
                let augmentation = self
                    .maybe_pattern_ambient_module_augmentations()
                    .as_ref()
                    .and_then(|pattern_ambient_module_augmentations| {
                        pattern_ambient_module_augmentations
                            .get(module_reference)
                            .map(Clone::clone)
                    });
                if let Some(augmentation) = augmentation {
                    return Ok(self.get_merged_symbol(Some(augmentation)));
                }
                return Ok(self.get_merged_symbol(Some(pattern.ref_(self).symbol)));
            }
        }

        if matches!(resolved_module.as_ref(), Some(resolved_module) if !resolution_extension_is_ts_or_json(resolved_module.ref_(self).extension()))
            && resolution_diagnostic.is_none()
            || matches!(resolution_diagnostic, Some(resolution_diagnostic) if ptr::eq(resolution_diagnostic, &*Diagnostics::Could_not_find_a_declaration_file_for_module_0_1_implicitly_has_an_any_type))
        {
            if is_for_augmentation {
                let diag = &Diagnostics::Invalid_module_name_in_augmentation_Module_0_resolves_to_an_untyped_module_at_1_which_cannot_be_augmented;
                self.error(
                    Some(error_node),
                    diag,
                    Some(vec![
                        module_reference.to_owned(),
                        resolved_module
                            .unwrap()
                            .ref_(self)
                            .resolved_file_name
                            .clone(),
                    ]),
                );
            } else {
                self.error_on_implicit_any_module(
                    self.no_implicit_any && module_not_found_error.is_some(),
                    error_node,
                    resolved_module.unwrap(),
                    module_reference,
                );
            }
            return Ok(None);
        }

        if let Some(module_not_found_error) = module_not_found_error {
            if let Some(resolved_module) = resolved_module.as_ref() {
                let redirect = TypeCheckerHost::get_project_reference_redirect(
                    &*self.host.ref_(self),
                    &resolved_module.ref_(self).resolved_file_name,
                );
                if let Some(redirect) = redirect {
                    self.error(
                        Some(error_node),
                        &Diagnostics::Output_file_0_has_not_been_built_from_source_file_1,
                        Some(vec![
                            redirect,
                            resolved_module.ref_(self).resolved_file_name.clone(),
                        ]),
                    );
                    return Ok(None);
                }
            }

            if let Some(resolution_diagnostic) = resolution_diagnostic {
                self.error(
                    Some(error_node),
                    resolution_diagnostic,
                    Some(vec![
                        module_reference.to_owned(),
                        resolved_module
                            .as_ref()
                            .unwrap()
                            .ref_(self)
                            .resolved_file_name
                            .clone(),
                    ]),
                );
            } else {
                let ts_extension = try_extract_ts_extension(module_reference);
                let is_extensionless_relative_path_import =
                    path_is_relative(module_reference) && !has_extension(module_reference);
                let module_resolution_kind =
                    get_emit_module_resolution_kind(&self.compiler_options.ref_(self));
                let resolution_is_node_12_or_next = matches!(
                    module_resolution_kind,
                    ModuleResolutionKind::Node12 | ModuleResolutionKind::NodeNext
                );
                if let Some(ts_extension) = ts_extension {
                    let diag = &Diagnostics::An_import_path_cannot_end_with_a_0_extension_Consider_importing_1_instead;
                    let import_source_without_extension =
                        remove_extension(module_reference, ts_extension.to_str());
                    let mut replaced_import_source = import_source_without_extension.to_owned();
                    if self.module_kind >= ModuleKind::ES2015 {
                        replaced_import_source.push_str(if ts_extension == Extension::Mts {
                            ".mjs"
                        } else if ts_extension == Extension::Cts {
                            ".cjs"
                        } else {
                            ".js"
                        });
                    }
                    self.error(
                        Some(error_node),
                        diag,
                        Some(vec![
                            ts_extension.to_str().to_owned(),
                            replaced_import_source,
                        ]),
                    );
                } else if !matches!(
                    self.compiler_options.ref_(self).resolve_json_module,
                    Some(true)
                ) && file_extension_is(module_reference, Extension::Json.to_str())
                    && get_emit_module_resolution_kind(&self.compiler_options.ref_(self))
                        != ModuleResolutionKind::Classic
                    && has_json_module_emit_enabled(&self.compiler_options.ref_(self))
                {
                    self.error(
                        Some(error_node),
                        &Diagnostics::Cannot_find_module_0_Consider_using_resolveJsonModule_to_import_module_with_json_extension,
                        Some(vec![
                            module_reference.to_owned()
                        ])
                    );
                } else if matches!(mode, Some(ModuleKind::ESNext))
                    && resolution_is_node_12_or_next
                    && is_extensionless_relative_path_import
                {
                    let absolute_ref = get_normalized_absolute_path(
                        module_reference,
                        Some(&get_directory_path(
                            &current_source_file.ref_(self).as_source_file().path(),
                        )),
                    );
                    let suggested_ext = self
                        .suggested_extensions
                        .iter()
                        .find(|(actual_ext, _import_ext)| {
                            self.host
                                .ref_(self)
                                .file_exists(&format!("{}{}", absolute_ref, actual_ext))
                        })
                        .map(|(_, import_ext)| import_ext);
                    if let Some(suggested_ext) = suggested_ext {
                        self.error(
                            Some(error_node),
                            &Diagnostics::Relative_import_paths_need_explicit_file_extensions_in_EcmaScript_imports_when_moduleResolution_is_node12_or_nodenext_Did_you_mean_0,
                            Some(vec![
                                format!("{}{}", module_reference, suggested_ext)
                            ])
                        );
                    } else {
                        self.error(
                            Some(error_node),
                            &Diagnostics::Relative_import_paths_need_explicit_file_extensions_in_EcmaScript_imports_when_moduleResolution_is_node12_or_nodenext_Consider_adding_an_extension_to_the_import_path,
                            None
                        );
                    }
                } else {
                    self.error(
                        Some(error_node),
                        module_not_found_error,
                        Some(vec![module_reference.to_owned()]),
                    );
                }
            }
        }
        Ok(None)
    }
}
