#![allow(non_upper_case_globals)]

use std::cmp::Ordering;
use std::collections::HashMap;

use crate::{
    BaseNode, DiagnosticCollection, DiagnosticMessage, DiagnosticWithDetachedLocation,
    DiagnosticWithLocation, SortedArray, SyntaxKind,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum OperatorPrecedence {
    Comma,
    Multiplicative,
    Primary,
    Invalid = -1,
}

impl OperatorPrecedence {
    pub const Lowest: OperatorPrecedence = OperatorPrecedence::Comma;
}

impl Ord for OperatorPrecedence {
    fn cmp(&self, other: &Self) -> Ordering {
        (*self as isize).cmp(&(*other as isize))
    }
}

impl PartialOrd for OperatorPrecedence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub fn get_binary_operator_precedence(kind: SyntaxKind) -> OperatorPrecedence {
    match kind {
        SyntaxKind::AsteriskToken => return OperatorPrecedence::Multiplicative,
        _ => (),
    }
    OperatorPrecedence::Invalid
}

pub fn create_diagnostic_collection() -> DiagnosticCollection {
    DiagnosticCollection::new()
}

impl DiagnosticCollection {
    pub fn new() -> Self {
        DiagnosticCollection {
            file_diagnostics: HashMap::<String, SortedArray<DiagnosticWithLocation>>::new(),
        }
    }

    pub fn get_diagnostics(&self, file_name: &str) -> Vec<DiagnosticWithLocation> {
        self.file_diagnostics
            .get(file_name)
            .map(|sorted_array| sorted_array._vec.clone())
            .unwrap_or(vec![])
    }
}

#[allow(non_snake_case)]
fn Node(kind: SyntaxKind) -> BaseNode {
    BaseNode { kind }
}

#[allow(non_snake_case)]
fn Token(kind: SyntaxKind) -> BaseNode {
    BaseNode { kind }
}

#[allow(non_snake_case)]
fn Identifier(kind: SyntaxKind) -> BaseNode {
    BaseNode { kind }
}

pub struct ObjectAllocator {}

impl ObjectAllocator {
    pub fn get_node_constructor(&self) -> fn(SyntaxKind) -> BaseNode {
        Node
    }

    pub fn get_token_constructor(&self) -> fn(SyntaxKind) -> BaseNode {
        Token
    }

    pub fn get_identifier_constructor(&self) -> fn(SyntaxKind) -> BaseNode {
        Identifier
    }

    pub fn get_source_file_constructor(&self) -> fn(SyntaxKind) -> BaseNode {
        Node
    }
}

lazy_static! {
    pub static ref object_allocator: ObjectAllocator = ObjectAllocator {};
}

pub fn create_detached_diagnostic(
    file_name: &str,
    start: usize,
    length: usize,
    _message: &DiagnosticMessage,
) -> DiagnosticWithDetachedLocation {
    DiagnosticWithDetachedLocation {
        start,
        length,

        file_name: file_name.to_string(),
    }
}
