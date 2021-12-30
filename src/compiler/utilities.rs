#![allow(non_upper_case_globals)]

use regex::{Captures, Regex};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    create_text_span_from_bounds, escape_leading_underscores, insert_sorted, is_member_name,
    BaseDiagnostic, BaseDiagnosticRelatedInformation, BaseNode, BaseType, Debug_, Diagnostic,
    DiagnosticCollection, DiagnosticMessage, DiagnosticMessageChain,
    DiagnosticRelatedInformationInterface, DiagnosticWithDetachedLocation, DiagnosticWithLocation,
    EmitTextWriter, Expression, Node, NodeInterface, ReadonlyTextRange, SortedArray, SourceFile,
    Symbol, SymbolFlags, SymbolTable, SymbolTracker, SymbolWriter, SyntaxKind, TextSpan, TypeFlags,
    __String, get_name_of_declaration, skip_trivia,
};

pub fn create_symbol_table() -> SymbolTable {
    let result = SymbolTable::new();
    result
}

// lazy_static! {
//     static ref string_writer: Rc<RefCell<dyn EmitTextWriter>> = create_single_line_string_writer();
// }

fn string_writer() -> Rc<RefCell<dyn EmitTextWriter>> {
    create_single_line_string_writer()
}

fn create_single_line_string_writer() -> Rc<RefCell<dyn EmitTextWriter>> {
    Rc::new(RefCell::new(SingleLineStringWriter::new()))
}

struct SingleLineStringWriter {
    str: String,
}

impl SingleLineStringWriter {
    pub fn new() -> Self {
        Self {
            str: "".to_string(),
        }
    }

    fn write_text(&mut self, text: &str) {
        self.str.push_str(text);
    }
}

impl EmitTextWriter for SingleLineStringWriter {
    fn get_text(&self) -> String {
        self.str.clone()
    }
}

impl SymbolWriter for SingleLineStringWriter {
    fn write_keyword(&mut self, text: &str) {
        self.write_text(text);
    }

    fn clear(&mut self) {
        self.str = "".to_string();
    }
}

impl SymbolTracker for SingleLineStringWriter {}

fn get_source_text_of_node_from_source_file<TNode: NodeInterface>(
    source_file: Rc<SourceFile>,
    node: &TNode,
    include_trivia: Option<bool>,
) -> String {
    let include_trivia = include_trivia.unwrap_or(false);
    get_text_of_node_from_source_text(&source_file.text, node, Some(include_trivia))
}

fn get_text_of_node_from_source_text<TNode: NodeInterface>(
    source_text: &str,
    node: &TNode,
    include_trivia: Option<bool>,
) -> String {
    let include_trivia = include_trivia.unwrap_or(false);
    if node_is_missing(node) {
        return "".to_string();
    }

    let start = if include_trivia {
        node.pos()
    } else {
        skip_trivia(source_text, node.pos())
    };
    let end = node.end();
    if !(start >= 0 && end >= 0 && end - start >= 0) {
        return "".to_string();
    }
    let start = start as usize;
    let end = end as usize;
    let text = source_text
        .chars()
        .skip(start)
        .take(end - start)
        .collect::<String>();

    text
}

fn get_text_of_node<TNode: NodeInterface>(node: &TNode, include_trivia: Option<bool>) -> String {
    let include_trivia = include_trivia.unwrap_or(false);
    get_source_text_of_node_from_source_file(
        get_source_file_of_node(node),
        node,
        Some(include_trivia),
    )
}

pub fn using_single_line_string_writer<TAction: FnOnce(Rc<RefCell<dyn EmitTextWriter>>)>(
    action: TAction,
) -> String {
    let string_writer = string_writer();
    let old_string = string_writer.borrow().get_text();
    action(string_writer.clone());
    let mut string_writer = string_writer.borrow_mut();
    let ret = string_writer.get_text();
    string_writer.clear();
    string_writer.write_keyword(&old_string);
    ret
}

fn get_full_width<TNode: NodeInterface>(node: &TNode) -> isize {
    node.end() - node.pos()
}

pub fn get_source_file_of_node<TNode: NodeInterface>(node: &TNode) -> Rc<SourceFile> {
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

pub fn node_is_missing<TNode: NodeInterface>(node: &TNode) -> bool {
    node.pos() == node.end() && node.pos() >= 0 && node.kind() != SyntaxKind::EndOfFileToken
}

pub fn declaration_name_to_string<TNode: NodeInterface>(name: Option<&TNode>) -> String {
    match name {
        None => "(Missing)".to_string(),
        Some(name) => {
            if get_full_width(name) == 0 {
                "(Missing)".to_string()
            } else {
                get_text_of_node(name, None)
            }
        }
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
    message_chain: DiagnosticMessageChain,
) -> DiagnosticWithLocation {
    let source_file = get_source_file_of_node(node);
    let span = get_error_span_for_node(source_file.clone(), node);
    create_file_diagnostic_from_message_chain(source_file, span.start, span.length, message_chain)
}

fn create_file_diagnostic_from_message_chain(
    file: Rc<SourceFile>,
    start: isize,
    length: isize,
    message_chain: DiagnosticMessageChain,
) -> DiagnosticWithLocation {
    // assert_diagnostic_location(&*file, start, length);
    DiagnosticWithLocation::new(BaseDiagnostic::new(BaseDiagnosticRelatedInformation::new(
        Some(file),
        start,
        length,
        message_chain,
    )))
}

fn get_error_span_for_node<TNode: NodeInterface>(
    source_file: Rc<SourceFile>,
    node: &TNode,
) -> TextSpan {
    let error_node = node;

    let pos = error_node.pos();

    create_text_span_from_bounds(pos, error_node.end())
}

pub fn is_external_or_common_js_module(file: &SourceFile) -> bool {
    false
}

pub fn get_effective_initializer<TNode: NodeInterface>(
    node: &TNode, /*HasExpressionInitializer*/
) -> Option<Rc<Node>> {
    node.node_wrapper()
        .as_has_expression_initializer()
        .initializer()
}

pub fn set_value_declaration<TNode: NodeInterface>(symbol: &Symbol, node: &TNode) {
    {
        if !(symbol.maybe_value_declaration().is_none()) {
            return;
        }
    }
    symbol.set_value_declaration(node);
}

pub fn is_keyword(token: SyntaxKind) -> bool {
    SyntaxKind::FirstKeyword <= token && token <= SyntaxKind::LastKeyword
}

pub fn has_dynamic_name<TNode: NodeInterface>(declaration: &TNode /*Declaration*/) -> bool {
    let name = get_name_of_declaration(declaration);
    if let Some(name) = name {
        is_dynamic_name(&*name)
    } else {
        false
    }
}

fn is_dynamic_name(name: &Node /*DeclarationName*/) -> bool {
    false
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

pub fn get_escaped_text_of_identifier_or_literal<TNode: NodeInterface>(node: &TNode) -> __String {
    if is_member_name(&*node) {
        node.node_wrapper().as_member_name().escaped_text()
    } else {
        escape_leading_underscores(node.node_wrapper().as_literal_like_node().text())
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

#[derive(Clone)]
pub struct TextWriter {
    new_line: String,
    output: String,
}

impl TextWriter {
    pub fn new(new_line: &str) -> Self {
        Self {
            new_line: new_line.to_string(),
            output: String::new(),
        }
    }

    fn push_output(&mut self, str: &str) {
        self.output.push_str(str);
    }

    fn write_text(&mut self, s: &str) {
        if !s.is_empty() {
            self.push_output(s);
        }
    }

    fn write(&mut self, s: &str) {
        self.write_text(s);
    }

    fn reset(&mut self) {
        self.output = String::new();
    }
}

impl EmitTextWriter for TextWriter {
    fn get_text(&self) -> String {
        self.output.clone()
    }
}

impl SymbolWriter for TextWriter {
    fn write_keyword(&mut self, text: &str) {
        self.write(text);
    }

    fn clear(&mut self) {
        self.reset();
    }
}

impl SymbolTracker for TextWriter {}

pub fn create_text_writer(new_line: &str) -> TextWriter {
    TextWriter::new(new_line)
    // text_writer.reset()
}

pub fn get_effective_type_annotation_node(node: &Node) -> Option<Rc<Node /*TypeNode*/>> {
    let type_ = node
        .maybe_as_has_type()
        .and_then(|has_type| has_type.type_());
    type_
}

pub fn get_first_identifier<TNode: NodeInterface>(node: &TNode) -> Rc<Node /*Identifier*/> {
    let wrapper = node.node_wrapper();
    match &*wrapper {
        Node::Expression(Expression::Identifier(_)) => wrapper,
        _ => unimplemented!(),
    }
}

#[allow(non_snake_case)]
fn Symbol(flags: SymbolFlags, name: __String) -> Symbol {
    Symbol::new(flags, name)
}

#[allow(non_snake_case)]
fn Type(flags: TypeFlags) -> BaseType {
    BaseType::new(flags)
}

#[allow(non_snake_case)]
fn Node(kind: SyntaxKind, pos: isize, end: isize) -> BaseNode {
    BaseNode::new(kind, pos, end)
}

#[allow(non_snake_case)]
fn Token(kind: SyntaxKind, pos: isize, end: isize) -> BaseNode {
    BaseNode::new(kind, pos, end)
}

#[allow(non_snake_case)]
fn Identifier(kind: SyntaxKind, pos: isize, end: isize) -> BaseNode {
    BaseNode::new(kind, pos, end)
}

pub struct ObjectAllocator {}

impl ObjectAllocator {
    pub fn get_node_constructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        Node
    }

    pub fn get_token_constructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        Token
    }

    pub fn get_identifier_constructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        Identifier
    }

    pub fn get_source_file_constructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
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

fn format_string_from_args(text: &str, args: Vec<String>) -> String {
    let re = Regex::new(r"\{(\d+)\}").unwrap();
    re.replace_all(text, |captures: &Captures| {
        let index = captures.get(1).unwrap().as_str().parse::<usize>().unwrap();
        Debug_.check_defined(args.get(index), None)
    })
    .to_string()
}

fn get_locale_specific_message(message: &DiagnosticMessage) -> String {
    message.message.to_string()
}

pub fn create_detached_diagnostic(
    file_name: &str,
    start: isize,
    length: isize,
    message: &DiagnosticMessage,
) -> DiagnosticWithDetachedLocation {
    let text = get_locale_specific_message(message);

    DiagnosticWithDetachedLocation::new(
        BaseDiagnostic::new(BaseDiagnosticRelatedInformation::new(
            None, start, length, text,
        )),
        file_name.to_string(),
    )
}

fn create_file_diagnostic(
    file: Rc<SourceFile>,
    start: isize,
    length: isize,
    message: &DiagnosticMessage,
) -> DiagnosticWithLocation {
    let text = get_locale_specific_message(message);

    DiagnosticWithLocation::new(BaseDiagnostic::new(BaseDiagnosticRelatedInformation::new(
        Some(file),
        start,
        length,
        text,
    )))
}

pub fn chain_diagnostic_messages(
    details: Option<DiagnosticMessageChain>,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticMessageChain {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        text = format_string_from_args(&text, args);
    }
    DiagnosticMessageChain {
        message_text: text,
        next: details.map(|details| vec![details]),
    }
}

pub fn position_is_synthesized(pos: isize) -> bool {
    !(pos >= 0)
}

fn set_text_range_pos<TRange: ReadonlyTextRange>(range: &mut TRange, pos: isize) -> &mut TRange {
    range.set_pos(pos);
    range
}

fn set_text_range_end<TRange: ReadonlyTextRange>(range: &mut TRange, end: isize) -> &mut TRange {
    range.set_end(end);
    range
}

pub fn set_text_range_pos_end<TRange: ReadonlyTextRange>(
    range: &mut TRange,
    pos: isize,
    end: isize,
) {
    set_text_range_end(set_text_range_pos(range, pos), end);
}

pub fn set_parent<TNode: NodeInterface>(child: &TNode, parent: Option<Rc<Node>>) -> &TNode {
    if let Some(parent) = parent {
        child.set_parent(parent.clone());
    }
    child
}
