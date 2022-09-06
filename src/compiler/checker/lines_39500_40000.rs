#![allow(non_upper_case_globals)]

use crate::{DiagnosticMessage, Node, TypeChecker};

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
