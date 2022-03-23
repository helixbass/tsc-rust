#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use crate::{
    declaration_name_to_string, entity_name_to_string, find, get_alias_declaration_from_name,
    get_check_flags, is_binary_expression, is_entity_name, is_in_js_file,
    is_internal_module_import_equals_declaration, is_property_access_expression, is_qualified_name,
    is_right_side_of_qualified_name_or_property_access, is_type_of_expression,
    is_type_only_import_or_export_declaration, is_variable_declaration, node_is_synthesized,
    unescape_leading_underscores, CheckFlags, DiagnosticMessage, Diagnostics,
    HasInitializerInterface, InternalSymbolName, NamedDeclarationInterface, SymbolFormatFlags,
    SymbolLinks, SymbolTable, SyntaxKind, __String, get_first_identifier, node_is_missing, Debug_,
    Node, NodeInterface, Symbol, SymbolFlags, SymbolInterface, TypeChecker,
};

impl TypeChecker {
    pub(super) fn get_target_of_property_assignment(
        &self,
        node: &Node, /*PropertyAssignment*/
        dont_recursively_resolve: bool,
    ) -> Option<Rc<Symbol>> {
        let expression = &node.as_property_assignment().initializer;
        self.get_target_of_alias_like_expression(expression, dont_recursively_resolve)
    }

    pub(super) fn get_target_of_access_expression(
        &self,
        node: &Node, /*AccessExpression*/
        dont_recursively_resolve: bool,
    ) -> Option<Rc<Symbol>> {
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
    ) -> Option<Rc<Symbol>> {
        let dont_recursively_resolve = dont_recursively_resolve.unwrap_or(false);
        match node.kind() {
            SyntaxKind::ImportEqualsDeclaration | SyntaxKind::VariableDeclaration => {
                self.get_target_of_import_equals_declaration(node, dont_recursively_resolve)
            }
            SyntaxKind::ImportClause => {
                self.get_target_of_import_clause(node, dont_recursively_resolve)
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
            ),
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
        }
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

    pub(super) fn resolve_symbol<TSymbol: Borrow<Symbol> + Clone>(
        &self,
        symbol: Option<TSymbol>,
        dont_resolve_alias: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        if !matches!(dont_resolve_alias, Some(true))
            && self.is_non_local_alias(symbol.clone(), None)
        {
            Some(self.resolve_alias(symbol.unwrap().borrow()))
        } else {
            symbol.map(|symbol| symbol.borrow().symbol_wrapper())
        }
    }

    pub(super) fn resolve_alias(&self, symbol: &Symbol) -> Rc<Symbol> {
        Debug_.assert(
            symbol.flags().intersects(SymbolFlags::Alias),
            Some("Should only get Alias here."),
        );
        let links = self.get_symbol_links(symbol);
        if RefCell::borrow(&links).target.is_none() {
            links.borrow_mut().target = Some(self.resolving_symbol());
            let node = self.get_declaration_of_alias_symbol(symbol);
            if node.is_none() {
                Debug_.fail(None);
            }
            let node = node.unwrap();
            let target = self.get_target_of_alias_declaration(&node, None);
            if Rc::ptr_eq(
                RefCell::borrow(&links).target.as_ref().unwrap(),
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
                    )]),
                );
            }
        } else if Rc::ptr_eq(
            RefCell::borrow(&links).target.as_ref().unwrap(),
            &self.resolving_symbol(),
        ) {
            links.borrow_mut().target = Some(self.unknown_symbol());
        }
        let ret = RefCell::borrow(&links).target.clone().unwrap();
        ret
    }

    pub(super) fn try_resolve_alias(&self, symbol: &Symbol) -> Option<Rc<Symbol>> {
        let links = self.get_symbol_links(symbol);
        if !matches!(
            RefCell::borrow(&links).target.as_ref(),
            Some(target) if Rc::ptr_eq(target, &self.resolving_symbol())
        ) {
            return Some(self.resolve_alias(symbol));
        }

        None
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
        alias_declaration_links: &RefCell<SymbolLinks>,
        target: Option<TTarget>,
        overwrite_empty: bool,
    ) -> bool {
        let mut alias_declaration_links = alias_declaration_links.borrow_mut();
        if let Some(target) = target {
            let target = target.borrow();
            if alias_declaration_links.type_only_declaration.is_none()
                || overwrite_empty
                    && matches!(alias_declaration_links.type_only_declaration, Some(None))
            {
                let export_symbol = target
                    .maybe_exports()
                    .as_ref()
                    .and_then(|exports| {
                        RefCell::borrow(exports)
                            .get(&InternalSymbolName::ExportEquals())
                            .map(Clone::clone)
                    })
                    .unwrap_or_else(|| target.symbol_wrapper());
                let type_only =
                    export_symbol
                        .maybe_declarations()
                        .as_deref()
                        .and_then(|declarations| {
                            find(declarations, |declaration: &Rc<Node>, _| {
                                is_type_only_import_or_export_declaration(declaration)
                            })
                            .map(Clone::clone)
                        });
                alias_declaration_links.type_only_declaration = Some(type_only.or_else(|| {
                    match RefCell::borrow(&self.get_symbol_links(&export_symbol))
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
            alias_declaration_links.type_only_declaration.as_ref(),
            Some(Some(_))
        )
    }

    pub(super) fn get_type_only_alias_declaration(
        &self,
        symbol: &Symbol,
    ) -> Option<Rc<Node /*TypeOnlyAliasDeclaration*/>> {
        if !symbol.flags().intersects(SymbolFlags::Alias) {
            return None;
        }
        let links = self.get_symbol_links(symbol);
        let ret = match RefCell::borrow(&links).type_only_declaration.as_ref() {
            Some(Some(type_only_declaration)) => Some(type_only_declaration.clone()),
            _ => None,
        };
        ret
    }

    pub(super) fn mark_export_as_referenced(
        &self,
        node: &Node, /*ImportEqualsDeclaration | ExportSpecifier*/
    ) {
        let symbol = self.get_symbol_of_node(node).unwrap();
        let target = self.resolve_alias(&symbol);
        // if (target) {
        let mark_alias = Rc::ptr_eq(&target, &self.unknown_symbol())
            || target.flags().intersects(SymbolFlags::Value)
                && !self.is_const_enum_or_const_enum_only_module(&target)
                && self.get_type_only_alias_declaration(&symbol).is_none();

        if mark_alias {
            self.mark_alias_symbol_as_referenced(&symbol);
        }
        // }
    }

    pub(super) fn mark_alias_symbol_as_referenced(&self, symbol: &Symbol) {
        let links = self.get_symbol_links(symbol);
        if !matches!(RefCell::borrow(&links).referenced, Some(true)) {
            links.borrow_mut().referenced = Some(true);
            let node = self.get_declaration_of_alias_symbol(symbol);
            if node.is_none() {
                Debug_.fail(None);
            }
            let node = node.unwrap();
            if is_internal_module_import_equals_declaration(&node) {
                let target = self.resolve_symbol(Some(symbol), None).unwrap();
                if Rc::ptr_eq(&target, &self.unknown_symbol())
                    || target.flags().intersects(SymbolFlags::Value)
                {
                    self.check_expression_cached(
                        &node.as_import_equals_declaration().module_reference,
                        None,
                    );
                }
            }
        }
    }

    pub(super) fn mark_const_enum_alias_as_referenced(&self, symbol: &Symbol) {
        let links = self.get_symbol_links(symbol);
        if !matches!(RefCell::borrow(&links).const_enum_referenced, Some(true)) {
            links.borrow_mut().const_enum_referenced = Some(true);
        }
    }

    pub(super) fn get_symbol_of_part_of_right_hand_side_of_import_equals(
        &self,
        entity_name: &Node, /*EntityName*/
        dont_resolve_alias: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let mut entity_name = entity_name.node_wrapper();
        if entity_name.kind() == SyntaxKind::Identifier
            && is_right_side_of_qualified_name_or_property_access(&entity_name)
        {
            entity_name = entity_name.parent();
        }
        if entity_name.kind() == SyntaxKind::Identifier
            || entity_name.parent().kind() == SyntaxKind::QualifiedName
        {
            self.resolve_entity_name(
                &entity_name,
                SymbolFlags::Namespace,
                Some(false),
                dont_resolve_alias,
                Option::<&Node>::None,
            )
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
            )
        }
    }

    pub(super) fn get_fully_qualified_name<TContainingLocation: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        containing_location: Option<TContainingLocation>,
    ) -> String {
        if let Some(symbol_parent) = symbol.maybe_parent() {
            format!(
                "{}.{}",
                self.get_fully_qualified_name(&symbol_parent, containing_location),
                self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None)
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
            )
        }
    }

    pub(super) fn get_containing_qualified_name_node(
        &self,
        node: &Node, /*QualifiedName*/
    ) -> Rc<Node> {
        let mut node = node.node_wrapper();
        while is_qualified_name(&node.parent()) {
            node = node.parent();
        }
        node
    }

    pub(super) fn try_get_qualified_name_as_value(
        &self,
        node: &Node, /*QualifiedName*/
    ) -> Option<Rc<Symbol>> {
        let mut left = get_first_identifier(node);
        let mut symbol = self.resolve_name_(
            Some(&*left),
            &left.as_identifier().escaped_text,
            SymbolFlags::Value,
            None,
            Some(left.clone()),
            true,
            None,
        )?;
        while is_qualified_name(&left.parent()) {
            let type_ = self.get_type_of_symbol(&symbol);
            symbol = self.get_property_of_type_(
                &type_,
                &left
                    .parent()
                    .as_qualified_name()
                    .right
                    .as_identifier()
                    .escaped_text,
                None,
            )?;
            left = left.parent();
        }
        Some(symbol)
    }

    pub(super) fn resolve_entity_name<TLocation: Borrow<Node>>(
        &self,
        name: &Node, /*EntityNameOrEntityNameExpression*/
        meaning: SymbolFlags,
        ignore_errors: Option<bool>,
        dont_resolve_alias: Option<bool>,
        location: Option<TLocation>,
    ) -> Option<Rc<Symbol>> {
        let ignore_errors_unwrapped = ignore_errors.unwrap_or(false);
        let dont_resolve_alias = dont_resolve_alias.unwrap_or(false);
        if node_is_missing(Some(name)) {
            return None;
        }

        let namespace_meaning = SymbolFlags::Namespace
            | if is_in_js_file(Some(name)) {
                meaning & SymbolFlags::Value
            } else {
                SymbolFlags::None
            };
        let symbol: Option<Rc<Symbol>>;
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
                return self.get_merged_symbol(symbol_from_js_prototype);
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
            let mut namespace = self.resolve_entity_name(
                left,
                namespace_meaning,
                ignore_errors,
                Some(false),
                location,
            )?;
            if node_is_missing(Some(&**right)) {
                return None;
            } else if Rc::ptr_eq(&namespace, &self.unknown_symbol()) {
                return Some(namespace);
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
                                    self.resolve_external_module_symbol(Some(&*module_sym), None);
                                if let Some(resolved_module_symbol) = resolved_module_symbol {
                                    namespace = resolved_module_symbol;
                                }
                            }
                        }
                    }
                }
            }
            symbol = self.get_merged_symbol(self.get_symbol(
                self.get_exports_of_symbol(&namespace),
                &right.as_identifier().escaped_text,
                meaning,
            ));
            if symbol.is_none() {
                if !ignore_errors_unwrapped {
                    let namespace_name =
                        self.get_fully_qualified_name(&namespace, Option::<&Node>::None);
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
                                ),
                            ]),
                        );
                        return None;
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
                            Some(containing_qualified_name) if !is_type_of_expression(&containing_qualified_name.parent()) && self.try_get_qualified_name_as_value(containing_qualified_name).is_some()
                        );
                    if can_suggest_typeof {
                        self.error(
                            containing_qualified_name.as_deref(),
                            &Diagnostics::_0_refers_to_a_value_but_is_being_used_as_a_type_here_Did_you_mean_typeof_0,
                            Some(vec![entity_name_to_string(containing_qualified_name.as_ref().unwrap()).into_owned()])
                        );
                        return None;
                    }

                    if meaning.intersects(SymbolFlags::Namespace)
                        && is_qualified_name(&name.parent())
                    {
                        let exported_type_symbol = self.get_merged_symbol(self.get_symbol(
                            self.get_exports_of_symbol(&namespace),
                            &right.as_identifier().escaped_text,
                            SymbolFlags::Type,
                        ));
                        if let Some(exported_type_symbol) = exported_type_symbol {
                            self.error(
                                Some(&*name.parent().as_qualified_name().right),
                                &Diagnostics::Cannot_access_0_1_because_0_is_a_type_but_not_a_namespace_Did_you_mean_to_retrieve_the_type_of_the_property_1_in_0_with_0_1,
                                Some(vec![
                                    self.symbol_to_string_(&exported_type_symbol, Option::<&Node>::None, None, None, None),
                                    unescape_leading_underscores(&name.parent().as_qualified_name().right.as_identifier().escaped_text)
                                ])
                            );
                            return None;
                        }
                    }

                    self.error(
                        Some(&**right),
                        &Diagnostics::Namespace_0_has_no_exported_member_1,
                        Some(vec![namespace_name, declaration_name.into_owned()]),
                    );
                }
                return None;
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
        if symbol.flags().intersects(meaning) || dont_resolve_alias {
            Some(symbol)
        } else {
            Some(self.resolve_alias(&symbol))
        }
    }

    pub(super) fn resolve_entity_name_from_assignment_declaration(
        &self,
        name: &Node, /*Identifier*/
        meaning: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn resolve_external_module_name_(
        &self,
        location: &Node,
        module_reference_expression: &Node, /*Expression*/
        ignore_errors: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn resolve_external_module_name_worker(
        &self,
        location: &Node,
        module_reference_expression: &Node, /*Expression*/
        module_not_found_error: Option<&DiagnosticMessage>,
        is_for_augmentation: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let is_for_augmentation = is_for_augmentation.unwrap_or(false);
        unimplemented!()
    }

    pub(super) fn resolve_external_module_symbol<TModuleSymbol: Borrow<Symbol>>(
        &self,
        module_symbol: Option<TModuleSymbol>,
        dont_resolve_alias: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn resolve_es_module_symbol<TModuleSymbol: Borrow<Symbol>>(
        &self,
        module_symbol: Option<TModuleSymbol>,
        referencing_location: &Node,
        dont_resolve_alias: bool,
        suppress_interop_error: bool,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn has_export_assignment_symbol(&self, module_symbol: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn get_exports_of_module_as_array(&self, module_symbol: &Symbol) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn try_get_member_in_module_exports_(
        &self,
        member_name: &__String,
        module_symbol: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn try_get_member_in_module_exports_and_properties_(
        &self,
        member_name: &__String,
        module_symbol: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_exports_of_symbol(&self, symbol: &Symbol) -> &SymbolTable {
        unimplemented!()
    }

    pub(super) fn get_merged_symbol<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
    ) -> Option<Rc<Symbol>> {
        symbol.map(|symbol| symbol.borrow().symbol_wrapper())
    }

    pub(super) fn get_symbol_of_node(&self, node: &Node) -> Option<Rc<Symbol>> {
        self.get_merged_symbol(node.maybe_symbol())
    }
}
