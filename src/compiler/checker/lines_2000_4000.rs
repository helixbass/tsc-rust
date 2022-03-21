#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::rc::Rc;

use super::ResolveNameNameArg;
use crate::{
    add_related_info, create_diagnostic_for_node, is_valid_type_only_alias_use_site, Diagnostic,
    DiagnosticMessage, Diagnostics, SyntaxKind, __String, declaration_name_to_string,
    get_first_identifier, node_is_missing, unescape_leading_underscores, Debug_, Node,
    NodeInterface, Symbol, SymbolFlags, SymbolInterface, TypeChecker,
};

impl TypeChecker {
    pub(super) fn check_symbol_usage_in_expression_context(
        &self,
        symbol: &Symbol,
        name: &__String,
        use_site: &Node,
    ) {
        if !is_valid_type_only_alias_use_site(use_site) {
            let type_only_declaration = self.get_type_only_alias_declaration(symbol);
            if let Some(type_only_declaration) = type_only_declaration {
                let message = if type_only_declaration.kind() == SyntaxKind::ExportSpecifier {
                    &Diagnostics::_0_cannot_be_used_as_a_value_because_it_was_exported_using_export_type
                } else {
                    &Diagnostics::_0_cannot_be_used_as_a_value_because_it_was_imported_using_import_type
                };
                let unescaped_name = unescape_leading_underscores(name);
                self.add_type_only_declaration_related_info(
                    self.error(Some(use_site), message, Some(vec![unescaped_name.clone()])),
                    Some(type_only_declaration),
                    &unescaped_name,
                );
            }
        }
    }

    pub(super) fn add_type_only_declaration_related_info<
        TTypeOnlyDeclaration: Borrow<Node /*TypeOnlyCompatibleAliasDeclaration*/>,
    >(
        &self,
        diagnostic: Rc<Diagnostic>,
        type_only_declaration: Option<TTypeOnlyDeclaration>,
        unescaped_name: &str,
    ) -> Rc<Diagnostic> {
        if type_only_declaration.is_none() {
            return diagnostic;
        }
        let type_only_declaration = type_only_declaration.unwrap();
        let type_only_declaration = type_only_declaration.borrow();
        add_related_info(
            &diagnostic,
            vec![Rc::new(
                create_diagnostic_for_node(
                    type_only_declaration,
                    if type_only_declaration.kind() == SyntaxKind::ExportSpecifier {
                        &Diagnostics::_0_was_exported_here
                    } else {
                        &Diagnostics::_0_was_imported_here
                    },
                    Some(vec![unescaped_name.to_owned()]),
                )
                .into(),
            )],
        );
        diagnostic
    }

    pub(super) fn get_is_deferred_context<TLastLocation: Borrow<Node>>(
        &self,
        location: &Node,
        last_location: Option<TLastLocation>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_self_reference_location(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn diagnostic_name(&self, name_arg: ResolveNameNameArg) -> Cow<'static, str> {
        match name_arg {
            ResolveNameNameArg::__String(__string) => {
                unescape_leading_underscores(&__string).into()
            }
            ResolveNameNameArg::Node(node) => declaration_name_to_string(Some(&*node)),
        }
    }

    pub(super) fn is_type_parameter_symbol_declared_in_container(
        &self,
        symbol: &Symbol,
        container: &Node,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_and_report_error_for_missing_prefix(
        &self,
        error_location: &Node,
        name: &__String,
        name_arg: ResolveNameNameArg,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_and_report_error_for_extending_interface(
        &self,
        error_location: &Node,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_and_report_error_for_using_type_as_namespace(
        &self,
        error_location: &Node,
        name: &__String,
        meaning: SymbolFlags,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_and_report_error_for_using_value_as_type(
        &self,
        error_location: &Node,
        name: &__String,
        meaning: SymbolFlags,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_and_report_error_for_exporting_primitive_type(
        &self,
        error_location: &Node,
        name: &__String,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_and_report_error_for_using_type_as_value(
        &self,
        error_location: &Node,
        name: &__String,
        meaning: SymbolFlags,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_and_report_error_for_using_namespace_module_as_value(
        &self,
        error_location: &Node,
        name: &__String,
        meaning: SymbolFlags,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_resolved_block_scoped_variable(
        &self,
        result: &Symbol,
        error_location: &Node,
    ) {
        unimplemented!()
    }

    pub(super) fn is_same_scope_descendent_of<TParent: Borrow<Node>>(
        &self,
        initial: &Node,
        parent: Option<TParent>,
        stop_at: &Node,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn resolve_symbol<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
        dont_resolve_alias: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn resolve_alias(&self, symbol: &Symbol) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn get_type_only_alias_declaration(
        &self,
        symbol: &Symbol,
    ) -> Option<Rc<Node /*TypeOnlyAliasDeclaration*/>> {
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
