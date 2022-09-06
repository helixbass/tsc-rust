#![allow(non_upper_case_globals)]

use std::ptr;
use std::rc::Rc;

use super::{intrinsic_type_kinds, is_instantiated_module};
use crate::{
    are_option_rcs_equal, cast_present, declaration_name_to_string, factory, for_each,
    get_declaration_of_kind, get_effective_modifier_flags, get_enclosing_block_scope_container,
    get_interface_base_type_nodes, get_name_of_declaration, get_source_file_of_node,
    get_text_of_identifier_or_literal, get_text_of_property_name, has_abstract_modifier,
    is_ambient_module, is_binding_pattern, is_computed_non_literal_name, is_entity_name_expression,
    is_enum_const, is_enum_declaration, is_external_module_augmentation,
    is_external_module_name_relative, is_finite, is_global_scope_augmentation, is_identifier,
    is_infinity_or_nan_string, is_literal_expression, is_nan, is_optional_chain,
    is_private_identifier, is_static, is_string_literal_like, length, maybe_for_each,
    node_is_missing, node_is_present, set_parent, should_preserve_const_enums, synthetic_factory,
    DiagnosticMessage, Diagnostics, EnumKind, FunctionLikeDeclarationInterface,
    HasInitializerInterface, HasTypeParametersInterface, InterfaceTypeInterface, ModifierFlags,
    NamedDeclarationInterface, Node, NodeCheckFlags, NodeFlags, NodeInterface, Number,
    ReadonlyTextRange, StringOrNumber, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeInterface, __String, escape_leading_underscores,
};

impl TypeChecker {
    pub(super) fn check_alias_symbol(
        &self,
        node: &Node, /*ImportEqualsDeclaration | VariableDeclaration | ImportClause | NamespaceImport | ImportSpecifier | ExportSpecifier | NamespaceExport*/
    ) {
        unimplemented!()
    }

    pub(super) fn check_grammar_module_element_context(
        &self,
        node: &Node, /*Statement*/
        error_message: &DiagnosticMessage,
    ) -> bool {
        unimplemented!()
    }
}
