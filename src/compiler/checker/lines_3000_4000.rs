#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use super::ResolveNameNameArg;
use crate::{
    add_related_info, concatenate, create_diagnostic_for_node, deduplicate_rc, ends_with,
    escape_leading_underscores, export_assignment_is_alias, find, find_ancestor, find_last,
    get_assignment_declaration_kind, get_es_module_interop,
    get_external_module_import_equals_declaration_expression, get_external_module_require_argument,
    get_immediately_invoked_function_expression, get_jsdoc_host, get_leftmost_access_expression,
    get_mode_for_usage_location, get_name_of_declaration, get_root_declaration,
    get_source_file_of_node, get_text_of_node, get_this_container, has_syntactic_modifier,
    is_access_expression, is_aliasable_expression, is_binary_expression, is_binding_element,
    is_block_or_catch_scoped, is_class_expression, is_class_like, is_computed_property_name,
    is_entity_name, is_entity_name_expression, is_export_assignment, is_export_declaration,
    is_export_specifier, is_function_expression, is_function_like, is_function_like_declaration,
    is_identifier, is_in_js_file, is_jsdoc_template_tag, is_jsdoc_type_alias,
    is_property_access_expression, is_property_signature, is_qualified_name,
    is_require_variable_declaration, is_shorthand_ambient_module_symbol, is_source_file,
    is_source_file_js, is_static, is_string_literal_like, is_type_literal_node, is_type_query_node,
    is_valid_type_only_alias_use_site, is_variable_declaration, map, should_preserve_const_enums,
    some, AssignmentDeclarationKind, Diagnostic, DiagnosticMessage, Diagnostics, Extension,
    FindAncestorCallbackReturn, HasInitializerInterface, HasTypeInterface, InternalSymbolName,
    ModifierFlags, ModuleKind, NodeFlags, SymbolTable, SyntaxKind, TypeFlags, TypeInterface,
    __String, declaration_name_to_string, get_first_identifier, node_is_missing,
    unescape_leading_underscores, Debug_, Node, NodeInterface, Symbol, SymbolFlags,
    SymbolInterface, TypeChecker,
};

impl TypeChecker {
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
