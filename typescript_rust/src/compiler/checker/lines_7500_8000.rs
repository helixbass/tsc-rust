use std::borrow::Borrow;

use crate::{Symbol, Type};

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
