use gc::{Finalize, Gc, Trace};

use crate::{DiagnosticMessage, Node, SymbolAccessibilityResult};

pub trait GetSymbolAccessibilityDiagnosticInterface: Trace + Finalize {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Gc<SymbolAccessibilityDiagnostic>>;
}

pub type GetSymbolAccessibilityDiagnostic = Gc<Box<dyn GetSymbolAccessibilityDiagnosticInterface>>;

#[derive(Trace, Finalize)]
pub struct SymbolAccessibilityDiagnostic {
    pub error_node: Gc<Node>,
    pub diagnostic_message: &'static DiagnosticMessage,
    pub type_name: Option<Gc<Node /*DeclarationName | QualifiedName*/>>,
}

pub fn can_produce_diagnostics(node: &Node) -> bool {
    unimplemented!()
}

pub fn create_get_symbol_accessibility_diagnostic_for_node(
    node: &Node, /*DeclarationDiagnosticProducing*/
) -> GetSymbolAccessibilityDiagnostic {
    unimplemented!()
}
