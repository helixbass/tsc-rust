#![allow(non_upper_case_globals)]

use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    create_text_span_from_bounds, insert_sorted, BaseDiagnostic, BaseDiagnosticRelatedInformation,
    BaseNode, BaseType, Diagnostic, DiagnosticCollection, DiagnosticMessage,
    DiagnosticRelatedInformationInterface, DiagnosticWithDetachedLocation, DiagnosticWithLocation,
    Node, NodeInterface, ReadonlyTextRange, SortedArray, SourceFile, SyntaxKind, TextSpan,
    TypeFlags,
};

fn get_source_file_of_node<TNode: NodeInterface>(node: &TNode) -> Rc<SourceFile> {
    if node.kind() == SyntaxKind::SourceFile {
        unimplemented!()
    }
    let mut parent = node.parent();
    while parent.kind() != SyntaxKind::SourceFile {
        parent = parent.parent();
    }
    match &*parent {
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

fn create_diagnostic_for_node_in_source_file<TNode: NodeInterface>(
    source_file: Rc<SourceFile>,
    node: &TNode,
    message: &DiagnosticMessage,
) -> DiagnosticWithLocation {
    let span = get_error_span_for_node(source_file.clone(), node);
    create_file_diagnostic(source_file, span.start, span.length, message)
}

fn get_error_span_for_node<TNode: NodeInterface>(
    source_file: Rc<SourceFile>,
    node: &TNode,
) -> TextSpan {
    let error_node = node;

    let pos = error_node.pos();

    create_text_span_from_bounds(pos, error_node.end())
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
        // let diagnostics = self
        //     .file_diagnostics
        //     .get_mut(&diagnostic.file().unwrap().file_name)
        //     .unwrap_or_else(|| {
        //         let diagnostics: SortedArray<Rc<Diagnostic>> = SortedArray::new(vec![]);
        //         self.file_diagnostics.insert(
        //             diagnostic.file().unwrap().file_name.to_string(),
        //             diagnostics,
        //         );
        //         self.file_diagnostics
        //             .get_mut(&diagnostic.file().unwrap().file_name)
        //             .unwrap()
        //     });
        if let Some(diagnostics) = self
            .file_diagnostics
            .get_mut(&diagnostic.file().unwrap().file_name)
        {
            insert_sorted(diagnostics, diagnostic);
            return;
        }
        let diagnostics: SortedArray<Rc<Diagnostic>> = SortedArray::new(vec![]);
        self.file_diagnostics.insert(
            diagnostic.file().unwrap().file_name.to_string(),
            diagnostics,
        );
        let diagnostics = self
            .file_diagnostics
            .get_mut(&diagnostic.file().unwrap().file_name)
            .unwrap();
        insert_sorted(diagnostics, diagnostic);
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
fn Node(kind: SyntaxKind, pos: usize, end: usize) -> BaseNode {
    BaseNode::new(kind, pos, end)
}

#[allow(non_snake_case)]
fn Token(kind: SyntaxKind, pos: usize, end: usize) -> BaseNode {
    BaseNode::new(kind, pos, end)
}

#[allow(non_snake_case)]
fn Identifier(kind: SyntaxKind, pos: usize, end: usize) -> BaseNode {
    BaseNode::new(kind, pos, end)
}

pub struct ObjectAllocator {}

impl ObjectAllocator {
    pub fn get_node_constructor(&self) -> fn(SyntaxKind, usize, usize) -> BaseNode {
        Node
    }

    pub fn get_token_constructor(&self) -> fn(SyntaxKind, usize, usize) -> BaseNode {
        Token
    }

    pub fn get_identifier_constructor(&self) -> fn(SyntaxKind, usize, usize) -> BaseNode {
        Identifier
    }

    pub fn get_source_file_constructor(&self) -> fn(SyntaxKind, usize, usize) -> BaseNode {
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
    message: &DiagnosticMessage,
) -> DiagnosticWithDetachedLocation {
    DiagnosticWithDetachedLocation {
        _diagnostic: BaseDiagnostic::new(BaseDiagnosticRelatedInformation {
            file: None,
            start,
            length,
        }),
        file_name: file_name.to_string(),
    }
}

fn create_file_diagnostic(
    file: Rc<SourceFile>,
    start: usize,
    length: usize,
    message: &DiagnosticMessage,
) -> DiagnosticWithLocation {
    DiagnosticWithLocation {
        _diagnostic: BaseDiagnostic::new(BaseDiagnosticRelatedInformation {
            file: Some(file),
            start,
            length,
        }),
    }
}

fn set_text_range_pos<TRange: ReadonlyTextRange>(range: &mut TRange, pos: usize) -> &mut TRange {
    range.set_pos(pos);
    range
}

fn set_text_range_end<TRange: ReadonlyTextRange>(range: &mut TRange, end: usize) -> &mut TRange {
    range.set_end(end);
    range
}

pub fn set_text_range_pos_end<TRange: ReadonlyTextRange>(
    range: &mut TRange,
    pos: usize,
    end: usize,
) {
    set_text_range_end(set_text_range_pos(range, pos), end);
}

pub fn set_parent<TNode: NodeInterface>(child: &TNode, parent: Option<Rc<Node>>) -> &TNode {
    if let Some(parent) = parent {
        child.set_parent(parent.clone());
    }
    child
}
