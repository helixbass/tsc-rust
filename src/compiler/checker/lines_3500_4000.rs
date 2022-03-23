#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

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
    resolution_extension_is_ts_or_json, starts_with, try_extract_ts_extension,
    unescape_leading_underscores, AssignmentDeclarationKind, CheckFlags, Debug_, DiagnosticMessage,
    Diagnostics, Extension, FindAncestorCallbackReturn, HasInitializerInterface,
    InternalSymbolName, ModuleKind, ModuleResolutionKind, NamedDeclarationInterface, Node,
    NodeFlags, NodeInterface, ResolvedModuleFull, Symbol, SymbolFlags, SymbolFormatFlags,
    SymbolInterface, SymbolLinks, SymbolTable, SyntaxKind, TypeChecker, __String,
};

impl TypeChecker {
    pub(super) fn error_on_implicit_any_module(
        &self,
        is_error: bool,
        error_node: &Node,
        resolved_module: &ResolvedModuleFull,
        module_reference: &str,
    ) {
        let package_id = &resolved_module.package_id;
        let resolved_file_name = &resolved_module.resolved_file_name;
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
