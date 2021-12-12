#![allow(non_upper_case_globals)]

use std::cmp::Ordering;

use crate::{BaseNode, DiagnosticMessage, DiagnosticWithDetachedLocation, SyntaxKind};

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum OperatorPrecedence {
    Comma,
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
    OperatorPrecedence::Invalid
}

#[allow(non_snake_case)]
fn Node(kind: SyntaxKind) -> BaseNode {
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
