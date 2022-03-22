#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use crate::{
    is_binary_expression, is_property_access_expression, is_type_only_import_or_export_declaration,
    DiagnosticMessage, Diagnostics, NamedDeclarationInterface, SymbolLinks, SymbolTable,
    SyntaxKind, __String, get_first_identifier, node_is_missing, Debug_, Node, NodeInterface,
    Symbol, SymbolFlags, SymbolInterface, TypeChecker,
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
        unimplemented!()
    }

    pub(super) fn get_type_only_alias_declaration(
        &self,
        symbol: &Symbol,
    ) -> Option<Rc<Node /*TypeOnlyAliasDeclaration*/>> {
        unimplemented!()
    }

    pub(super) fn get_symbol_of_part_of_right_hand_side_of_import_equals(
        &self,
        entity_name: &Node, /*EntityName*/
        dont_resolve_alias: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_fully_qualified_name<TContainingLocation: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        containing_location: Option<TContainingLocation>,
    ) -> String {
        unimplemented!()
    }

    pub(super) fn resolve_entity_name(
        &self,
        name: &Node, /*EntityNameOrEntityNameExpression*/
        meaning: SymbolFlags,
        ignore_errors: Option<bool>,
        dont_resolve_alias: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let ignore_errors = ignore_errors.unwrap_or(false);
        let dont_resolve_alias = dont_resolve_alias.unwrap_or(false);
        if node_is_missing(Some(name)) {
            return None;
        }

        let symbol: Option<Rc<Symbol>>;
        match name {
            Node::Identifier(name_as_identifier) => {
                let message = if false {
                    unimplemented!()
                } else {
                    self.get_cannot_find_name_diagnostic_for_name(&*get_first_identifier(name))
                };
                let symbol_from_js_prototype: Option<Rc<Symbol>> =
                    if false { unimplemented!() } else { None };
                symbol = self.get_merged_symbol(self.resolve_name_(
                    Some(name),
                    &name_as_identifier.escaped_text,
                    meaning,
                    if ignore_errors || symbol_from_js_prototype.is_some() {
                        None
                    } else {
                        Some(message)
                    },
                    Some(name.node_wrapper()),
                    true,
                    Some(false),
                ));
                if symbol.is_none() {
                    unimplemented!()
                }
            }
            // else if name.kind() == SyntaxKind::QualifiedName
            //     || name.kind() == SyntaxKind::PropertyAccessExpression
            //     unimplemented!()
            _ => Debug_.assert_never(name, Some("Unknown entity name kind.")),
        }
        let symbol = symbol.unwrap();
        if symbol.flags().intersects(meaning) || dont_resolve_alias {
            Some(symbol)
        } else {
            Some(self.resolve_alias(&symbol))
        }
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
