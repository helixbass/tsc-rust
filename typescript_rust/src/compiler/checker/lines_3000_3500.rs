use gc::{Gc, GcCell};
use std::ptr;
use std::{borrow::Borrow, io};

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
    node_is_missing, node_is_synthesized, path_is_relative, remove_extension, remove_prefix,
    resolution_extension_is_ts_or_json, return_ok_none_if_none, starts_with,
    try_extract_ts_extension, unescape_leading_underscores, AssignmentDeclarationKind, CheckFlags,
    Debug_, DiagnosticMessage, Diagnostics, Extension, FindAncestorCallbackReturn,
    HasInitializerInterface, InternalSymbolName, ModuleKind, ModuleResolutionKind,
    NamedDeclarationInterface, Node, NodeFlags, NodeInterface, Symbol, SymbolFlags,
    SymbolFormatFlags, SymbolInterface, SymbolLinks, SyntaxKind, TypeChecker, TypeCheckerHost,
};

impl TypeChecker {
    pub(super) fn get_target_of_property_assignment(
        &self,
        node: &Node, /*PropertyAssignment*/
        dont_recursively_resolve: bool,
    ) -> Option<Gc<Symbol>> {
        let expression = &node.as_property_assignment().initializer;
        self.get_target_of_alias_like_expression(expression, dont_recursively_resolve)
    }

    pub(super) fn get_target_of_access_expression(
        &self,
        node: &Node, /*AccessExpression*/
        dont_recursively_resolve: bool,
    ) -> Option<Gc<Symbol>> {
        let node_parent = node.parent();
        if !(is_binary_expression(&node_parent)) {
            return None;
        }
        let node_parent_as_binary_expression = node_parent.as_binary_expression();
        if !(ptr::eq(&*node_parent_as_binary_expression.left, node)
            && node_parent_as_binary_expression.operator_token.kind() == SyntaxKind::EqualsToken)
        {
            return None;
        }

        self.get_target_of_alias_like_expression(
            &node_parent_as_binary_expression.right,
            dont_recursively_resolve,
        )
    }

    pub(super) fn get_target_of_alias_declaration(
        &self,
        node: &Node, /*Declaration*/
        dont_recursively_resolve: Option<bool>,
    ) -> io::Result<Option<Gc<Symbol>>> {
        let dont_recursively_resolve = dont_recursively_resolve.unwrap_or(false);
        Ok(match node.kind() {
            SyntaxKind::ImportEqualsDeclaration | SyntaxKind::VariableDeclaration => {
                self.get_target_of_import_equals_declaration(node, dont_recursively_resolve)?
            }
            SyntaxKind::ImportClause => {
                self.get_target_of_import_clause(node, dont_recursively_resolve)?
            }
            SyntaxKind::NamespaceImport => {
                self.get_target_of_namespace_import(node, dont_recursively_resolve)
            }
            SyntaxKind::NamespaceExport => {
                self.get_target_of_namespace_export(node, dont_recursively_resolve)
            }
            SyntaxKind::ImportSpecifier | SyntaxKind::BindingElement => {
                self.get_target_of_import_specifier(node, dont_recursively_resolve)
            }
            SyntaxKind::ExportSpecifier => self.get_target_of_export_specifier(
                node,
                SymbolFlags::Value | SymbolFlags::Type | SymbolFlags::Namespace,
                Some(dont_recursively_resolve),
            )?,
            SyntaxKind::ExportAssignment | SyntaxKind::BinaryExpression => {
                self.get_target_of_export_assignment(node, dont_recursively_resolve)
            }
            SyntaxKind::NamespaceExportDeclaration => Some(
                self.get_target_of_namespace_export_declaration(node, dont_recursively_resolve),
            ),
            SyntaxKind::ShorthandPropertyAssignment => self.resolve_entity_name(
                &node.as_shorthand_property_assignment().name(),
                SymbolFlags::Value | SymbolFlags::Type | SymbolFlags::Namespace,
                Some(true),
                Some(dont_recursively_resolve),
                Option::<&Node>::None,
            ),
            SyntaxKind::PropertyAssignment => {
                self.get_target_of_property_assignment(node, dont_recursively_resolve)
            }
            SyntaxKind::ElementAccessExpression | SyntaxKind::PropertyAccessExpression => {
                self.get_target_of_access_expression(node, dont_recursively_resolve)
            }
            _ => Debug_.fail(None),
        })
    }

    pub(super) fn is_non_local_alias<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
        excludes: Option<SymbolFlags>,
    ) -> bool {
        let excludes =
            excludes.unwrap_or(SymbolFlags::Value | SymbolFlags::Type | SymbolFlags::Namespace);
        if symbol.is_none() {
            return false;
        }
        let symbol = symbol.unwrap();
        let symbol = symbol.borrow();
        symbol.flags() & (SymbolFlags::Alias | excludes) == SymbolFlags::Alias
            || symbol.flags().intersects(SymbolFlags::Alias)
                && symbol.flags().intersects(SymbolFlags::Assignment)
    }

    pub(super) fn resolve_symbol(
        &self,
        symbol: Option<impl Borrow<Symbol> + Clone>,
        dont_resolve_alias: Option<bool>,
    ) -> io::Result<Option<Gc<Symbol>>> {
        Ok(
            if !matches!(dont_resolve_alias, Some(true))
                && self.is_non_local_alias(symbol.clone(), None)
            {
                Some(self.resolve_alias(symbol.unwrap().borrow())?)
            } else {
                symbol.map(|symbol| symbol.borrow().symbol_wrapper())
            },
        )
    }

    pub(super) fn resolve_alias(&self, symbol: &Symbol) -> io::Result<Gc<Symbol>> {
        Debug_.assert(
            symbol.flags().intersects(SymbolFlags::Alias),
            Some("Should only get Alias here."),
        );
        let links = self.get_symbol_links(symbol);
        if (*links).borrow().target.is_none() {
            links.borrow_mut().target = Some(self.resolving_symbol());
            let node = self.get_declaration_of_alias_symbol(symbol);
            if node.is_none() {
                Debug_.fail(None);
            }
            let node = node.unwrap();
            let target = self.get_target_of_alias_declaration(&node, None)?;
            if Gc::ptr_eq(
                (*links).borrow().target.as_ref().unwrap(),
                &self.resolving_symbol(),
            ) {
                links.borrow_mut().target = Some(target.unwrap_or_else(|| self.unknown_symbol()));
            } else {
                self.error(
                    Some(&*node),
                    &Diagnostics::Circular_definition_of_import_alias_0,
                    Some(vec![self.symbol_to_string_(
                        symbol,
                        Option::<&Node>::None,
                        None,
                        None,
                        None,
                    )?]),
                );
            }
        } else if Gc::ptr_eq(
            (*links).borrow().target.as_ref().unwrap(),
            &self.resolving_symbol(),
        ) {
            links.borrow_mut().target = Some(self.unknown_symbol());
        }
        let ret = (*links).borrow().target.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn try_resolve_alias(&self, symbol: &Symbol) -> io::Result<Option<Gc<Symbol>>> {
        let links = self.get_symbol_links(symbol);
        if !matches!(
            (*links).borrow().target.as_ref(),
            Some(target) if Gc::ptr_eq(target, &self.resolving_symbol())
        ) {
            return Ok(Some(self.resolve_alias(symbol)?));
        }

        Ok(None)
    }

    pub(super) fn mark_symbol_of_alias_declaration_if_type_only<
        TAliasDeclaration: Borrow<Node>,
        TImmediateTarget: Borrow<Symbol>,
        TFinalTarget: Borrow<Symbol>,
    >(
        &self,
        alias_declaration: Option<TAliasDeclaration /*Declaration*/>,
        immediate_target: Option<TImmediateTarget>,
        final_target: Option<TFinalTarget>,
        overwrite_empty: bool,
    ) -> bool {
        if alias_declaration.is_none() {
            return false;
        }
        let alias_declaration = alias_declaration.unwrap();
        let alias_declaration = alias_declaration.borrow();
        if is_property_access_expression(alias_declaration) {
            return false;
        }

        let source_symbol = self.get_symbol_of_node(alias_declaration).unwrap();
        if is_type_only_import_or_export_declaration(alias_declaration) {
            let links = self.get_symbol_links(&source_symbol);
            links.borrow_mut().type_only_declaration = Some(Some(alias_declaration.node_wrapper()));
            return true;
        }

        let links = self.get_symbol_links(&source_symbol);
        self.mark_symbol_of_alias_declaration_if_type_only_worker(
            &links,
            immediate_target,
            overwrite_empty,
        ) || self.mark_symbol_of_alias_declaration_if_type_only_worker(
            &links,
            final_target,
            overwrite_empty,
        )
    }

    pub(super) fn mark_symbol_of_alias_declaration_if_type_only_worker<TTarget: Borrow<Symbol>>(
        &self,
        alias_declaration_links: &GcCell<SymbolLinks>,
        target: Option<TTarget>,
        overwrite_empty: bool,
    ) -> bool {
        if let Some(target) = target {
            let target = target.borrow();
            if alias_declaration_links
                .borrow()
                .type_only_declaration
                .is_none()
                || overwrite_empty
                    && matches!(
                        alias_declaration_links.borrow().type_only_declaration,
                        Some(None)
                    )
            {
                let export_symbol = target
                    .maybe_exports()
                    .as_ref()
                    .and_then(|exports| {
                        (**exports)
                            .borrow()
                            .get(InternalSymbolName::ExportEquals)
                            .cloned()
                    })
                    .unwrap_or_else(|| target.symbol_wrapper());
                let type_only =
                    export_symbol
                        .maybe_declarations()
                        .as_deref()
                        .and_then(|declarations| {
                            find(declarations, |declaration: &Gc<Node>, _| {
                                is_type_only_import_or_export_declaration(declaration)
                            })
                            .map(Clone::clone)
                        });
                alias_declaration_links.borrow_mut().type_only_declaration =
                    Some(type_only.or_else(|| {
                        match (*self.get_symbol_links(&export_symbol))
                            .borrow()
                            .type_only_declaration
                            .clone()
                        {
                            Some(type_only_declaration) => type_only_declaration,
                            None => None,
                        }
                    }));
            }
        }
        matches!(
            alias_declaration_links
                .borrow()
                .type_only_declaration
                .as_ref(),
            Some(Some(_))
        )
    }

    pub(super) fn get_type_only_alias_declaration(
        &self,
        symbol: &Symbol,
    ) -> Option<Gc<Node /*TypeOnlyAliasDeclaration*/>> {
        if !symbol.flags().intersects(SymbolFlags::Alias) {
            return None;
        }
        let links = self.get_symbol_links(symbol);
        let ret = match (*links).borrow().type_only_declaration.as_ref() {
            Some(Some(type_only_declaration)) => Some(type_only_declaration.clone()),
            _ => None,
        };
        ret
    }

    pub(super) fn mark_export_as_referenced(
        &self,
        node: &Node, /*ImportEqualsDeclaration | ExportSpecifier*/
    ) -> io::Result<()> {
        let symbol = self.get_symbol_of_node(node).unwrap();
        let target = self.resolve_alias(&symbol)?;
        // if (target) {
        let mark_alias = Gc::ptr_eq(&target, &self.unknown_symbol())
            || target.flags().intersects(SymbolFlags::Value)
                && !self.is_const_enum_or_const_enum_only_module(&target)
                && self.get_type_only_alias_declaration(&symbol).is_none();

        if mark_alias {
            self.mark_alias_symbol_as_referenced(&symbol);
        }
        // }

        Ok(())
    }

    pub(super) fn mark_alias_symbol_as_referenced(&self, symbol: &Symbol) -> io::Result<()> {
        let links = self.get_symbol_links(symbol);
        if !matches!((*links).borrow().referenced, Some(true)) {
            links.borrow_mut().referenced = Some(true);
            let node = self.get_declaration_of_alias_symbol(symbol);
            if node.is_none() {
                Debug_.fail(None);
            }
            let node = node.unwrap();
            if is_internal_module_import_equals_declaration(&node) {
                let target = self.resolve_symbol(Some(symbol), None)?.unwrap();
                if Gc::ptr_eq(&target, &self.unknown_symbol())
                    || target.flags().intersects(SymbolFlags::Value)
                {
                    self.check_expression_cached(
                        &node.as_import_equals_declaration().module_reference,
                        None,
                    );
                }
            }
        }

        Ok(())
    }

    pub(super) fn mark_const_enum_alias_as_referenced(&self, symbol: &Symbol) {
        let links = self.get_symbol_links(symbol);
        if !matches!((*links).borrow().const_enum_referenced, Some(true)) {
            links.borrow_mut().const_enum_referenced = Some(true);
        }
    }

    pub(super) fn get_symbol_of_part_of_right_hand_side_of_import_equals(
        &self,
        entity_name: &Node, /*EntityName*/
        dont_resolve_alias: Option<bool>,
    ) -> io::Result<Option<Gc<Symbol>>> {
        let mut entity_name = entity_name.node_wrapper();
        if entity_name.kind() == SyntaxKind::Identifier
            && is_right_side_of_qualified_name_or_property_access(&entity_name)
        {
            entity_name = entity_name.parent();
        }
        Ok(
            if entity_name.kind() == SyntaxKind::Identifier
                || entity_name.parent().kind() == SyntaxKind::QualifiedName
            {
                self.resolve_entity_name(
                    &entity_name,
                    SymbolFlags::Namespace,
                    Some(false),
                    dont_resolve_alias,
                    Option::<&Node>::None,
                )?
            } else {
                Debug_.assert(
                    entity_name.parent().kind() == SyntaxKind::ImportEqualsDeclaration,
                    None,
                );
                self.resolve_entity_name(
                    &entity_name,
                    SymbolFlags::Value | SymbolFlags::Type | SymbolFlags::Namespace,
                    Some(false),
                    dont_resolve_alias,
                    Option::<&Node>::None,
                )?
            },
        )
    }

    pub(super) fn get_fully_qualified_name(
        &self,
        symbol: &Symbol,
        containing_location: Option<impl Borrow<Node>>,
    ) -> io::Result<String> {
        Ok(if let Some(symbol_parent) = symbol.maybe_parent() {
            format!(
                "{}.{}",
                self.get_fully_qualified_name(&symbol_parent, containing_location)?,
                self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None)?
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
        })
    }

    pub(super) fn get_containing_qualified_name_node(
        &self,
        node: &Node, /*QualifiedName*/
    ) -> Gc<Node> {
        let mut node = node.node_wrapper();
        while is_qualified_name(&node.parent()) {
            node = node.parent();
        }
        node
    }

    pub(super) fn try_get_qualified_name_as_value(
        &self,
        node: &Node, /*QualifiedName*/
    ) -> io::Result<Option<Gc<Symbol>>> {
        let mut left = get_first_identifier(node);
        let mut symbol = return_ok_none_if_none!(self.resolve_name_(
            Some(&*left),
            &left.as_identifier().escaped_text,
            SymbolFlags::Value,
            None,
            Some(left.clone()),
            true,
            None,
        )?);
        while is_qualified_name(&left.parent()) {
            let type_ = self.get_type_of_symbol(&symbol)?;
            symbol = return_ok_none_if_none!(self.get_property_of_type_(
                &type_,
                &left
                    .parent()
                    .as_qualified_name()
                    .right
                    .as_identifier()
                    .escaped_text,
                None,
            )?);
            left = left.parent();
        }
        Ok(Some(symbol))
    }

    pub(super) fn resolve_entity_name(
        &self,
        name: &Node, /*EntityNameOrEntityNameExpression*/
        meaning: SymbolFlags,
        ignore_errors: Option<bool>,
        dont_resolve_alias: Option<bool>,
        location: Option<impl Borrow<Node>>,
    ) -> io::Result<Option<Gc<Symbol>>> {
        let ignore_errors_unwrapped = ignore_errors.unwrap_or(false);
        let dont_resolve_alias = dont_resolve_alias.unwrap_or(false);
        if node_is_missing(Some(name)) {
            return Ok(None);
        }

        let namespace_meaning = SymbolFlags::Namespace
            | if is_in_js_file(Some(name)) {
                meaning & SymbolFlags::Value
            } else {
                SymbolFlags::None
            };
        let symbol: Option<Gc<Symbol>>;
        let location = location.map(|location| location.borrow().node_wrapper());
        if name.kind() == SyntaxKind::Identifier {
            let message = if meaning == namespace_meaning || node_is_synthesized(name) {
                &Diagnostics::Cannot_find_namespace_0
            } else {
                self.get_cannot_find_name_diagnostic_for_name(&get_first_identifier(name))
            };
            let symbol_from_js_prototype =
                if is_in_js_file(Some(name)) && !node_is_synthesized(name) {
                    self.resolve_entity_name_from_assignment_declaration(name, meaning)
                } else {
                    None
                };
            symbol = self.get_merged_symbol(self.resolve_name_(
                Some(location.as_deref().unwrap_or(name)),
                &name.as_identifier().escaped_text,
                meaning,
                if ignore_errors_unwrapped || symbol_from_js_prototype.is_some() {
                    None
                } else {
                    Some(message)
                },
                Some(name.node_wrapper()),
                true,
                Some(false),
            ));
            if symbol.is_none() {
                return Ok(self.get_merged_symbol(symbol_from_js_prototype));
            }
        } else if matches!(
            name.kind(),
            SyntaxKind::QualifiedName | SyntaxKind::PropertyAccessExpression
        ) {
            let left = if name.kind() == SyntaxKind::QualifiedName {
                &name.as_qualified_name().left
            } else {
                &name.as_property_access_expression().expression
            };
            let right = if name.kind() == SyntaxKind::QualifiedName {
                &name.as_qualified_name().right
            } else {
                &name.as_property_access_expression().name
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
            if node_is_missing(Some(&**right)) {
                return Ok(None);
            } else if Gc::ptr_eq(&namespace, &self.unknown_symbol()) {
                return Ok(Some(namespace));
            }
            if let Some(namespace_value_declaration) = namespace.maybe_value_declaration() {
                if is_in_js_file(Some(&*namespace_value_declaration))
                    && is_variable_declaration(&namespace_value_declaration)
                {
                    if let Some(namespace_value_declaration_initializer) =
                        namespace_value_declaration
                            .as_variable_declaration()
                            .maybe_initializer()
                    {
                        if self.is_common_js_require(&namespace_value_declaration_initializer) {
                            let module_name = &namespace_value_declaration_initializer
                                .as_call_expression()
                                .arguments[0];
                            let module_sym = self.resolve_external_module_name_(
                                &module_name,
                                &module_name,
                                None,
                            );
                            if let Some(module_sym) = module_sym {
                                let resolved_module_symbol =
                                    self.resolve_external_module_symbol(Some(&*module_sym), None)?;
                                if let Some(resolved_module_symbol) = resolved_module_symbol {
                                    namespace = resolved_module_symbol;
                                }
                            }
                        }
                    }
                }
            }
            symbol = self.get_merged_symbol(self.get_symbol(
                &(*self.get_exports_of_symbol(&namespace)).borrow(),
                &right.as_identifier().escaped_text,
                meaning,
            )?);
            if symbol.is_none() {
                if !ignore_errors_unwrapped {
                    let namespace_name =
                        self.get_fully_qualified_name(&namespace, Option::<&Node>::None)?;
                    let declaration_name = declaration_name_to_string(Some(&**right));
                    let suggestion_for_nonexistent_module =
                        self.get_suggested_symbol_for_nonexistent_module(right, &namespace);
                    if let Some(suggestion_for_nonexistent_module) =
                        suggestion_for_nonexistent_module
                    {
                        self.error(
                            Some(&**right),
                            &Diagnostics::_0_has_no_exported_member_named_1_Did_you_mean_2,
                            Some(vec![
                                namespace_name,
                                declaration_name.into_owned(),
                                self.symbol_to_string_(
                                    &suggestion_for_nonexistent_module,
                                    Option::<&Node>::None,
                                    None,
                                    None,
                                    None,
                                )?,
                            ]),
                        );
                        return Ok(None);
                    }

                    let containing_qualified_name = if is_qualified_name(name) {
                        Some(self.get_containing_qualified_name_node(name))
                    } else {
                        None
                    };
                    let can_suggest_typeof = self.maybe_global_object_type().is_some()
                        && meaning.intersects(SymbolFlags::Type)
                        && matches!(
                            containing_qualified_name.as_ref(),
                            Some(containing_qualified_name) if !is_type_of_expression(&containing_qualified_name.parent())
                                && self.try_get_qualified_name_as_value(containing_qualified_name)?.is_some()
                        );
                    if can_suggest_typeof {
                        self.error(
                            containing_qualified_name.as_deref(),
                            &Diagnostics::_0_refers_to_a_value_but_is_being_used_as_a_type_here_Did_you_mean_typeof_0,
                            Some(vec![entity_name_to_string(containing_qualified_name.as_ref().unwrap()).into_owned()])
                        );
                        return Ok(None);
                    }

                    if meaning.intersects(SymbolFlags::Namespace)
                        && is_qualified_name(&name.parent())
                    {
                        let exported_type_symbol = self.get_merged_symbol(self.get_symbol(
                            &(*self.get_exports_of_symbol(&namespace)).borrow(),
                            &right.as_identifier().escaped_text,
                            SymbolFlags::Type,
                        )?);
                        if let Some(exported_type_symbol) = exported_type_symbol {
                            self.error(
                                Some(&*name.parent().as_qualified_name().right),
                                &Diagnostics::Cannot_access_0_1_because_0_is_a_type_but_not_a_namespace_Did_you_mean_to_retrieve_the_type_of_the_property_1_in_0_with_0_1,
                                Some(vec![
                                    self.symbol_to_string_(&exported_type_symbol, Option::<&Node>::None, None, None, None)?,
                                    unescape_leading_underscores(&name.parent().as_qualified_name().right.as_identifier().escaped_text).to_owned()
                                ])
                            );
                            return Ok(None);
                        }
                    }

                    self.error(
                        Some(&**right),
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
            !get_check_flags(&symbol).intersects(CheckFlags::Instantiated),
            Some("Should never get an instantiated symbol here."),
        );
        if !node_is_synthesized(name)
            && is_entity_name(name)
            && (symbol.flags().intersects(SymbolFlags::Alias)
                || name.parent().kind() == SyntaxKind::ExportAssignment)
        {
            self.mark_symbol_of_alias_declaration_if_type_only(
                get_alias_declaration_from_name(name),
                Some(&*symbol),
                Option::<&Symbol>::None,
                true,
            );
        }
        Ok(
            if symbol.flags().intersects(meaning) || dont_resolve_alias {
                Some(symbol)
            } else {
                Some(self.resolve_alias(&symbol)?)
            },
        )
    }

    pub(super) fn resolve_entity_name_from_assignment_declaration(
        &self,
        name: &Node, /*Identifier*/
        meaning: SymbolFlags,
    ) -> Option<Gc<Symbol>> {
        if self.is_jsdoc_type_reference(&name.parent()) {
            let secondary_location = self.get_assignment_declaration_location(&name.parent());
            if let Some(secondary_location) = secondary_location {
                return self.resolve_name_(
                    Some(secondary_location),
                    &name.as_identifier().escaped_text,
                    meaning,
                    None,
                    Some(name.node_wrapper()),
                    true,
                    None,
                );
            }
        }
        None
    }

    pub(super) fn get_assignment_declaration_location(
        &self,
        node: &Node, /*TypeReferenceNode*/
    ) -> Option<Gc<Node>> {
        let type_alias = find_ancestor(Some(node), |node| {
            if !(is_jsdoc_node(node) || node.flags().intersects(NodeFlags::JSDoc)) {
                FindAncestorCallbackReturn::Quit
            } else {
                is_jsdoc_type_alias(node).into()
            }
        });
        if type_alias.is_some() {
            return None;
        }
        let host = get_jsdoc_host(node);
        if let Some(host) = host.as_ref() {
            if is_expression_statement(host) {
                let host_as_expression_statement = host.as_expression_statement();
                if is_binary_expression(&host_as_expression_statement.expression)
                    && get_assignment_declaration_kind(&host_as_expression_statement.expression)
                        == AssignmentDeclarationKind::PrototypeProperty
                {
                    let symbol = self.get_symbol_of_node(
                        &host_as_expression_statement
                            .expression
                            .as_binary_expression()
                            .left,
                    );
                    if let Some(symbol) = symbol {
                        return self.get_declaration_of_js_prototype_container(&symbol);
                    }
                }
            }
        }
        if let Some(host) = host.as_ref() {
            if (is_object_literal_method(host) || is_property_assignment(host))
                && is_binary_expression(&host.parent().parent())
                && get_assignment_declaration_kind(&host.parent().parent())
                    == AssignmentDeclarationKind::Prototype
            {
                let symbol =
                    self.get_symbol_of_node(&host.parent().parent().as_binary_expression().left);
                if let Some(symbol) = symbol {
                    return self.get_declaration_of_js_prototype_container(&symbol);
                }
            }
        }
        let sig = get_effective_jsdoc_host(node);
        if let Some(sig) = sig {
            if is_function_like(Some(&*sig)) {
                let symbol = self.get_symbol_of_node(&sig);
                return symbol.and_then(|symbol| symbol.maybe_value_declaration());
            }
        }
        None
    }

    pub(super) fn get_declaration_of_js_prototype_container(
        &self,
        symbol: &Symbol,
    ) -> Option<Gc<Node>> {
        let decl = symbol.maybe_parent().unwrap().maybe_value_declaration()?;
        let initializer = if is_assignment_declaration(&decl) {
            get_assigned_expando_initializer(Some(&*decl))
        } else if has_only_expression_initializer(&decl) {
            get_declared_expando_initializer(&decl)
        } else {
            None
        };
        Some(initializer.unwrap_or(decl))
    }

    pub(super) fn get_expando_symbol(&self, symbol: &Symbol) -> Option<Gc<Symbol>> {
        let decl = symbol.maybe_value_declaration()?;
        if !is_in_js_file(Some(&*decl))
            || symbol.flags().intersects(SymbolFlags::TypeAlias)
            || get_expando_initializer(&decl, false).is_some()
        {
            return None;
        }
        let init = if is_variable_declaration(&decl) {
            get_declared_expando_initializer(&decl)
        } else {
            get_assigned_expando_initializer(Some(&*decl))
        };
        if let Some(init) = init {
            let init_symbol = self.get_symbol_of_node(&init);
            if let Some(init_symbol) = init_symbol {
                return self.merge_js_symbols(&init_symbol, Some(symbol));
            }
        }
        None
    }

    pub(super) fn resolve_external_module_name_(
        &self,
        location: &Node,
        module_reference_expression: &Node, /*Expression*/
        ignore_errors: Option<bool>,
    ) -> Option<Gc<Symbol>> {
        let is_classic = get_emit_module_resolution_kind(&self.compiler_options)
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
        location: &Node,
        module_reference_expression: &Node, /*Expression*/
        module_not_found_error: Option<&DiagnosticMessage>,
        is_for_augmentation: Option<bool>,
    ) -> Option<Gc<Symbol>> {
        let is_for_augmentation = is_for_augmentation.unwrap_or(false);
        if is_string_literal_like(module_reference_expression) {
            self.resolve_external_module(
                location,
                &module_reference_expression.as_literal_like_node().text(),
                module_not_found_error,
                module_reference_expression,
                Some(is_for_augmentation),
            )
        } else {
            None
        }
    }

    pub(super) fn resolve_external_module(
        &self,
        location: &Node,
        module_reference: &str,
        module_not_found_error: Option<&DiagnosticMessage>,
        error_node: &Node,
        is_for_augmentation: Option<bool>,
    ) -> Option<Gc<Symbol>> {
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

        let ambient_module = self.try_find_ambient_module_(module_reference, true);
        if ambient_module.is_some() {
            return ambient_module;
        }
        let current_source_file = get_source_file_of_node(location);
        let context_specifier = if is_string_literal_like(location) {
            Some(location.node_wrapper())
        } else {
            find_ancestor(Some(location), is_import_call).and_then(|ancestor| ancestor.as_call_expression().arguments.get(0).map(Clone::clone)).or_else(|| {
                find_ancestor(Some(location), is_import_declaration).map(|ancestor| ancestor.as_import_declaration().module_specifier.clone())
            }).or_else(|| {
                find_ancestor(Some(location), is_external_module_import_equals_declaration).map(|ancestor| ancestor.as_import_equals_declaration().module_reference.as_external_module_reference().expression.clone())
            }).or_else(|| {
                find_ancestor(Some(location), is_export_declaration).and_then(|ancestor| ancestor.as_export_declaration().module_specifier.clone())
            }).or_else(|| {
                if is_module_declaration(location) {
                    Some(location.node_wrapper())
                } else if matches!(location.maybe_parent(), Some(parent) if is_module_declaration(&parent) && ptr::eq(&*parent.as_module_declaration().name, location)) {
                    location.maybe_parent()
                } else {
                    None
                }.map(|node| node.as_module_declaration().name.clone())
            }).or_else(|| {
                if is_literal_import_type_node(location) {
                    Some(location.node_wrapper())
                } else {
                    None
                }.map(|node| node.as_import_type_node().argument.as_literal_type_node().literal.clone())
            })
        };
        let mode = if matches!(context_specifier.as_ref(), Some(context_specifier) if is_string_literal_like(context_specifier))
        {
            get_mode_for_usage_location(
                current_source_file
                    .as_source_file()
                    .maybe_implied_node_format(),
                context_specifier.as_ref().unwrap(),
            )
        } else {
            current_source_file
                .as_source_file()
                .maybe_implied_node_format()
        };
        let resolved_module =
            get_resolved_module(Some(&*current_source_file), module_reference, mode);
        let resolution_diagnostic = resolved_module.as_ref().and_then(|resolved_module| {
            get_resolution_diagnostic(&self.compiler_options, resolved_module)
        });
        let source_file = resolved_module.as_ref().and_then(|resolved_module| {
            if resolution_diagnostic.is_none() {
                self.host
                    .get_source_file(&resolved_module.resolved_file_name)
            } else {
                None
            }
        });
        if let Some(source_file) = source_file {
            if let Some(source_file_symbol) = source_file.maybe_symbol() {
                let resolved_module = resolved_module.as_ref().unwrap();
                if matches!(resolved_module.is_external_library_import, Some(true))
                    && !resolution_extension_is_ts_or_json(resolved_module.extension())
                {
                    self.error_on_implicit_any_module(
                        false,
                        error_node,
                        resolved_module,
                        module_reference,
                    );
                }
                if matches!(
                    get_emit_module_resolution_kind(&self.compiler_options),
                    ModuleResolutionKind::Node12 | ModuleResolutionKind::NodeNext
                ) {
                    let is_sync_import = matches!(
                        current_source_file
                            .as_source_file()
                            .maybe_implied_node_format(),
                        Some(ModuleKind::CommonJS)
                    ) && find_ancestor(Some(location), is_import_call)
                        .is_none()
                        || find_ancestor(Some(location), is_import_equals_declaration).is_some();
                    if is_sync_import
                        && matches!(
                            source_file.as_source_file().maybe_implied_node_format(),
                            Some(ModuleKind::ESNext)
                        )
                    {
                        self.error(
                            Some(error_node),
                            &Diagnostics::Module_0_cannot_be_imported_using_this_construct_The_specifier_only_resolves_to_an_ES_module_which_cannot_be_imported_synchronously_Use_dynamic_import_instead,
                            Some(vec![module_reference.to_owned()])
                        );
                    }
                    if matches!(mode, Some(ModuleKind::ESNext))
                        && matches!(self.compiler_options.resolve_json_module, Some(true))
                        && resolved_module.extension() == Extension::Json
                    {
                        self.error(
                            Some(error_node),
                            &Diagnostics::JSON_imports_are_experimental_in_ES_module_mode_imports,
                            None,
                        );
                    }
                }
                return self.get_merged_symbol(Some(&*source_file_symbol));
            }
            if module_not_found_error.is_some() {
                self.error(
                    Some(error_node),
                    &Diagnostics::File_0_is_not_a_module,
                    Some(vec![source_file.as_source_file().file_name().to_owned()]),
                );
            }
            return None;
        }

        if let Some(pattern_ambient_modules) = self.maybe_pattern_ambient_modules().as_ref() {
            let pattern = find_best_pattern_match(
                pattern_ambient_modules,
                |pattern| &pattern.pattern,
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
                    return self.get_merged_symbol(Some(augmentation));
                }
                return self.get_merged_symbol(Some(&*pattern.symbol));
            }
        }

        if matches!(resolved_module.as_ref(), Some(resolved_module) if !resolution_extension_is_ts_or_json(resolved_module.extension()))
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
                        resolved_module.as_ref().unwrap().resolved_file_name.clone(),
                    ]),
                );
            } else {
                self.error_on_implicit_any_module(
                    self.no_implicit_any && module_not_found_error.is_some(),
                    error_node,
                    resolved_module.as_ref().unwrap(),
                    module_reference,
                );
            }
            return None;
        }

        if let Some(module_not_found_error) = module_not_found_error {
            if let Some(resolved_module) = resolved_module.as_ref() {
                let redirect = TypeCheckerHost::get_project_reference_redirect(
                    &**self.host,
                    &resolved_module.resolved_file_name,
                );
                if let Some(redirect) = redirect {
                    self.error(
                        Some(error_node),
                        &Diagnostics::Output_file_0_has_not_been_built_from_source_file_1,
                        Some(vec![redirect, resolved_module.resolved_file_name.clone()]),
                    );
                    return None;
                }
            }

            if let Some(resolution_diagnostic) = resolution_diagnostic {
                self.error(
                    Some(error_node),
                    resolution_diagnostic,
                    Some(vec![
                        module_reference.to_owned(),
                        resolved_module.as_ref().unwrap().resolved_file_name.clone(),
                    ]),
                );
            } else {
                let ts_extension = try_extract_ts_extension(module_reference);
                let is_extensionless_relative_path_import =
                    path_is_relative(module_reference) && !has_extension(module_reference);
                let module_resolution_kind =
                    get_emit_module_resolution_kind(&self.compiler_options);
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
                } else if !matches!(self.compiler_options.resolve_json_module, Some(true))
                    && file_extension_is(module_reference, Extension::Json.to_str())
                    && get_emit_module_resolution_kind(&self.compiler_options)
                        != ModuleResolutionKind::Classic
                    && has_json_module_emit_enabled(&self.compiler_options)
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
                            &current_source_file.as_source_file().path(),
                        )),
                    );
                    let suggested_ext = self
                        .suggested_extensions
                        .iter()
                        .find(|(actual_ext, _import_ext)| {
                            self.host
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
        None
    }
}
