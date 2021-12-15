#![allow(non_upper_case_globals)]

use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    BaseDiagnostic, BaseDiagnosticRelatedInformation, BaseNode, BaseType, Diagnostic,
    DiagnosticCollection, DiagnosticMessage, DiagnosticRelatedInformationInterface,
    DiagnosticWithDetachedLocation, DiagnosticWithLocation, Node, NodeInterface, SortedArray,
    SourceFile, SyntaxKind, TypeFlags,
};

fn get_source_file_of_node<TNode: NodeInterface>(node: &TNode) -> Rc<SourceFile> {
    if node.kind() == SyntaxKind::SourceFile {
        unimplemented!()
    }
    let parent = node.parent();
    while parent.kind() != SyntaxKind::SourceFile {
        parent = parent.parent();
    }
    match *parent {
        Node::SourceFile(source_file) => source_file.clone(),
        _ => panic!("Expected SourceFile"),
    }
}

pub fn create_diagnostic_for_node<TNode: NodeInterface>(
    node: &TNode,
    message: &DiagnosticMessage,
) -> DiagnosticWithLocation {
    let source_file = get_source_file_of_node(node);
    create_diagnostic_for_node_in_source_file(source_file, node, message)
}

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
            file_diagnostics: HashMap::<String, SortedArray<Rc<Diagnostic>>>::new(),
        }
    }

    pub fn add(&mut self, diagnostic: Rc<Diagnostic>) {
        let diagnostics = self
            .file_diagnostics
            .get(&diagnostic.file().unwrap().file_name)
            .unwrap_or_else(|| {
                let diagnostics: SortedArray<Rc<Diagnostic>> = SortedArray::new(vec![]);
                self.file_diagnostics.insert(
                    diagnostic.file().unwrap().file_name.to_string(),
                    diagnostics,
                );
                self.file_diagnostics
                    .get(&diagnostic.file().unwrap().file_name)
                    .unwrap()
            });
    }

    pub fn get_diagnostics(&self, file_name: &str) -> Vec<Rc<Diagnostic>> {
        self.file_diagnostics
            .get(file_name)
            .map(|sorted_array| sorted_array._vec.clone())
            .unwrap_or(vec![])
    }
}

#[allow(non_snake_case)]
fn Type(flags: TypeFlags) -> BaseType {
    BaseType { flags }
}

#[allow(non_snake_case)]
fn Node(kind: SyntaxKind) -> BaseNode {
    BaseNode { kind, parent: None }
}

#[allow(non_snake_case)]
fn Token(kind: SyntaxKind) -> BaseNode {
    BaseNode { kind, parent: None }
}

#[allow(non_snake_case)]
fn Identifier(kind: SyntaxKind) -> BaseNode {
    BaseNode { kind, parent: None }
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

    pub fn get_type_constructor(&self) -> fn(TypeFlags) -> BaseType {
        Type
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
        _diagnostic: BaseDiagnostic {
            _diagnostic_related_information: BaseDiagnosticRelatedInformation {
                file: None,
                start,
                length,
            },
        },
        file_name: file_name.to_string(),
    }
}
