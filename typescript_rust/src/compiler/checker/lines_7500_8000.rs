use std::borrow::Borrow;

use gc::Gc;

use crate::{Node, SignatureKind, Symbol, SymbolFlags, SyntaxKind, Type};

use super::SymbolTableToDeclarationStatements;

impl SymbolTableToDeclarationStatements {
    pub(super) fn serialize_maybe_alias_assignment(&self, symbol: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn is_type_representable_as_function_namespace_merge(
        &self,
        type_to_serialize: &Type,
        host_symbol: &Symbol,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn serialize_property_symbol_for_interface(
        &self,
        p: &Symbol,
        base_type: Option<impl Borrow<Type>>,
    ) -> Vec<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn serialize_signatures(
        &self,
        kind: SignatureKind,
        input: &Type,
        base_type: Option<impl Borrow<Type>>,
        output_kind: SyntaxKind,
    ) -> Vec<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn serialize_index_signatures(
        &self,
        input: &Type,
        base_type: Option<impl Borrow<Type>>,
    ) -> Vec<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn try_serialize_as_type_reference(
        &self,
        t: &Type,
        flags: SymbolFlags,
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn get_unused_name(
        &self,
        input: &str,
        symbol: Option<impl Borrow<Symbol>>,
    ) -> String {
        unimplemented!()
    }

    pub(super) fn get_internal_symbol_name(&self, symbol: &Symbol, local_name: &str) -> String {
        unimplemented!()
    }
}
