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
