#![allow(non_upper_case_globals)]

use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    create_text_span_from_bounds, escape_leading_underscores, insert_sorted, is_member_name,
    BaseDiagnostic, BaseDiagnosticRelatedInformation, BaseNode, BaseType, Diagnostic,
    DiagnosticCollection, DiagnosticMessage, DiagnosticMessageChain,
    DiagnosticRelatedInformationInterface, DiagnosticWithDetachedLocation, DiagnosticWithLocation,
    Node, NodeInterface, ReadonlyTextRange, SortedArray, SourceFile, Symbol, SymbolFlags,
    SymbolTable, SyntaxKind, TextSpan, TypeFlags, __String,
};

pub fn create_symbol_table() -> SymbolTable {
    let result = SymbolTable::new();
    result
}

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

pub fn create_diagnostic_for_node_from_message_chain<TNode: NodeInterface>(
    node: &TNode,
    message_chain: &DiagnosticMessageChain,
) -> DiagnosticWithLocation {
    let source_file = get_source_file_of_node(node);
    let span = get_error_span_for_node(source_file.clone(), node);
    create_file_diagnostic_from_message_chain(source_file, span.start, span.length, message_chain)
}

fn create_file_diagnostic_from_message_chain(
    file: Rc<SourceFile>,
    start: usize,
    length: usize,
    message_chain: &DiagnosticMessageChain,
) -> DiagnosticWithLocation {
    // assert_diagnostic_location(&*file, start, length);
    DiagnosticWithLocation {
        _diagnostic: BaseDiagnostic::new(BaseDiagnosticRelatedInformation {
            file: Some(file),
            start,
            length,
        }),
    }
}

fn get_error_span_for_node<TNode: NodeInterface>(
    source_file: Rc<SourceFile>,
    node: &TNode,
) -> TextSpan {
    let error_node = node;

    let pos = error_node.pos();

    create_text_span_from_bounds(pos, error_node.end())
}

pub fn get_effective_initializer(node: &Node, /*HasExpressionInitializer*/) -> Option<Rc<Node>> {
    node.as_has_expression_initializer().initializer()
}

pub fn set_value_declaration<TNode: NodeInterface>(symbol: &Symbol, node: &TNode) {
    {
        if !(symbol.maybe_value_declaration().is_none()) {
            return;
        }
    }
    symbol.set_value_declaration(node);
}

pub fn is_property_name_literal<TNode: NodeInterface>(node: &TNode) -> bool {
    match node.kind() {
        SyntaxKind::Identifier
        | SyntaxKind::StringLiteral
        | SyntaxKind::NoSubstitutionTemplateLiteral
        | SyntaxKind::NumericLiteral => true,
        _ => false,
    }
}

pub fn get_escaped_text_of_identifier_or_literal(node: Rc<Node>) -> __String {
    if is_member_name(&*node) {
        node.as_member_name().escaped_text()
    } else {
        escape_leading_underscores(node.as_literal_like_node().text())
    }
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
            .map(|sorted_array| sorted_array.into())
            .unwrap_or(vec![])
    }
}

pub fn get_effective_type_annotation_node(node: &Node) -> Option<Rc<Node /*TypeNode*/>> {
    let type_ = node
        .maybe_as_has_type()
        .and_then(|has_type| has_type.type_());
    type_
}

#[allow(non_snake_case)]
fn Symbol(flags: SymbolFlags, name: __String) -> Symbol {
    Symbol::new(flags, name)
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

    pub fn get_symbol_constructor(&self) -> fn(SymbolFlags, __String) -> Symbol {
        Symbol
    }

    pub fn get_type_constructor(&self) -> fn(TypeFlags) -> BaseType {
        Type
    }
}

lazy_static! {
    pub static ref object_allocator: ObjectAllocator = ObjectAllocator {};
}

fn get_locale_specific_message(message: &DiagnosticMessage) -> String {
    message.message.to_string()
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

pub fn chain_diagnostic_messages(
    details: Option<DiagnosticMessageChain>,
    message: &DiagnosticMessage,
) -> DiagnosticMessageChain {
    let text = get_locale_specific_message(message);

    DiagnosticMessageChain {
        message_text: text,
        next: details.map(|details| vec![details]),
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
