use crate::{ModifierFlags, Node, Symbol, Type};

use super::SymbolTableToDeclarationStatements;

impl SymbolTableToDeclarationStatements {
    pub(super) fn include_private_symbol(&self, symbol: &Symbol) {
        unimplemented!()
    }

    pub(super) fn add_result(
        &self,
        node: &Node, /*Statement*/
        additional_modifier_flags: ModifierFlags,
    ) {
        unimplemented!()
    }

    pub(super) fn serialize_type_alias(
        &self,
        symbol: &Symbol,
        symbol_name: &str,
        modifier_flags: ModifierFlags,
    ) {
        unimplemented!()
    }

    pub(super) fn serialize_interface(
        &self,
        symbol: &Symbol,
        symbol_name: &str,
        modifier_flags: ModifierFlags,
    ) {
        unimplemented!()
    }

    pub(super) fn is_type_only_namespace(&self, symbol: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn serialize_module(
        &self,
        symbol: &Symbol,
        symbol_name: &str,
        modifier_flags: ModifierFlags,
    ) {
        unimplemented!()
    }

    pub(super) fn serialize_enum(
        &self,
        symbol: &Symbol,
        symbol_name: &str,
        modifier_flags: ModifierFlags,
    ) {
        unimplemented!()
    }

    pub(super) fn serialize_as_function_namespace_merge(
        &self,
        type_: &Type,
        symbol: &Symbol,
        local_name: &str,
        modifier_flags: ModifierFlags,
    ) {
        unimplemented!()
    }

    pub(super) fn serialize_as_class(
        &self,
        symbol: &Symbol,
        local_name: &str,
        modifier_flags: ModifierFlags,
    ) {
        unimplemented!()
    }

    pub(super) fn serialize_as_alias(
        &self,
        symbol: &Symbol,
        local_name: &str,
        modifier_flags: ModifierFlags,
    ) {
        unimplemented!()
    }
}
