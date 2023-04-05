use crate::{ModifierFlags, Node, Symbol};

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
}
