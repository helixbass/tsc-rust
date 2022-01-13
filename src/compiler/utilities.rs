#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use regex::{Captures, Regex};
use std::borrow::Borrow;
use std::cell::RefCell;
use std::cmp;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    SymbolTracker, SymbolWriter, SyntaxKind, TextSpan, TypeFlags, __String,
    compare_strings_case_sensitive, compare_values, create_text_span_from_bounds,
    escape_leading_underscores, for_each, get_combined_node_flags, get_name_of_declaration,
    insert_sorted, is_big_int_literal, is_member_name, skip_trivia, BaseDiagnostic,
    BaseDiagnosticRelatedInformation, BaseNode, BaseSymbol, BaseType, CharacterCodes, CheckFlags,
    Comparison, Debug_, Diagnostic, DiagnosticCollection, DiagnosticInterface, DiagnosticMessage,
    DiagnosticMessageChain, DiagnosticMessageText, DiagnosticRelatedInformation,
    DiagnosticRelatedInformationInterface, DiagnosticWithDetachedLocation, DiagnosticWithLocation,
    EmitFlags, EmitTextWriter, Expression, LiteralLikeNode, LiteralLikeNodeInterface, Node,
    NodeFlags, NodeInterface, ObjectFlags, PrefixUnaryExpression, PseudoBigInt, ReadonlyTextRange,
    SortedArray, SourceFile, Symbol, SymbolFlags, SymbolInterface, SymbolTable,
    TransientSymbolInterface, Type, TypeInterface,
};
use local_macros::enum_unwrapped;

pub fn get_declaration_of_kind(
    symbol: &Symbol,
    kind: SyntaxKind,
) -> Option<Rc<Node /*T extends Declaration*/>> {
    let maybe_declarations = symbol.maybe_declarations();
    let declarations = maybe_declarations.as_ref();
    if let Some(declarations) = declarations {
        for declaration in declarations {
            if declaration.kind() == kind {
                return Some(declaration.clone());
            }
        }
    }

    None
}

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
    fn write(&mut self, text: &str) {
        self.write_text(text);
    }

    fn write_trailing_semicolon(&mut self, text: &str) {
        self.write_text(text);
    }

    fn get_text(&self) -> String {
        self.str.clone()
    }
}

impl SymbolWriter for SingleLineStringWriter {
    fn write_keyword(&mut self, text: &str) {
        self.write_text(text);
    }

    fn write_punctuation(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_space(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_string_literal(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_property(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_symbol(&mut self, s: &str, _: &Symbol) {
        self.write_text(s);
    }

    fn clear(&mut self) {
        self.str = "".to_string();
    }
}

impl SymbolTracker for SingleLineStringWriter {}

fn get_source_text_of_node_from_source_file<TNode: NodeInterface>(
    source_file: &SourceFile,
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
        &*get_source_file_of_node(node),
        node,
        Some(include_trivia),
    )
}

fn get_emit_flags<TNode: NodeInterface>(node: &TNode) -> EmitFlags {
    EmitFlags::None
}

bitflags! {
    pub struct GetLiteralTextFlags: u32 {
        const None = 0;
        const NeverAsciiEscape = 1 << 0;
        const JsxAttributeEscape = 1 << 1;
        const TerminateUnterminatedLiterals = 1 << 2;
        const AllowNumericSeparator = 1 << 3;
    }
}

pub fn get_literal_text<TSourceFileRef: Borrow<SourceFile>>(
    node: &LiteralLikeNode,
    source_file: Option<TSourceFileRef>,
    flags: GetLiteralTextFlags,
) -> String {
    if can_use_original_text(node, flags) {
        return get_source_text_of_node_from_source_file(source_file.unwrap().borrow(), node, None);
    }

    match node {
        LiteralLikeNode::StringLiteral(node) => {
            let escape_text = if flags.intersects(GetLiteralTextFlags::JsxAttributeEscape) {
                unimplemented!()
            } else if flags.intersects(GetLiteralTextFlags::NeverAsciiEscape)
                || get_emit_flags(node).intersects(EmitFlags::NoAsciiEscaping)
            {
                unimplemented!()
            } else {
                escape_non_ascii_string
            };
            if matches!(node.single_quote, Some(true)) {
                format!(
                    "'{}'",
                    escape_text(node.text(), Some(CharacterCodes::single_quote))
                )
            } else {
                format!(
                    "\"{}\"",
                    escape_text(node.text(), Some(CharacterCodes::double_quote))
                )
            }
        }
        LiteralLikeNode::NumericLiteral(_) | LiteralLikeNode::BigIntLiteral(_) => {
            node.text().to_string()
        }
    }
}

fn can_use_original_text(node: &LiteralLikeNode, flags: GetLiteralTextFlags) -> bool {
    if node_is_synthesized(node)
        || node.maybe_parent().is_none()
        || flags.intersects(GetLiteralTextFlags::TerminateUnterminatedLiterals)
            && matches!(node.is_unterminated(), Some(true))
    {
        return false;
    }

    !is_big_int_literal(node)
}

pub fn is_block_or_catch_scoped<TNode: NodeInterface>(
    declaration: &TNode, /*Declaration*/
) -> bool {
    get_combined_node_flags(declaration).intersects(NodeFlags::BlockScoped)
        || is_catch_clause_variable_declaration_or_binding_element(declaration)
}

fn is_catch_clause_variable_declaration_or_binding_element<TNode: NodeInterface>(
    declaration: &TNode, /*Declaration*/
) -> bool {
    let node = get_root_declaration(declaration);
    node.kind() == SyntaxKind::VariableDeclaration
        && node.parent().kind() == SyntaxKind::CatchClause
}

pub fn using_single_line_string_writer<TAction: FnOnce(Rc<RefCell<dyn EmitTextWriter>>)>(
    action: TAction,
) -> String {
    let string_writer = string_writer();
    let old_string = (*string_writer).borrow().get_text();
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
    parent.as_source_file().clone()
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
    args: Option<Vec<String>>,
) -> DiagnosticWithLocation {
    let source_file = get_source_file_of_node(node);
    create_diagnostic_for_node_in_source_file(source_file, node, message, args)
}

fn create_diagnostic_for_node_in_source_file<TNode: NodeInterface>(
    source_file: Rc<SourceFile>,
    node: &TNode,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticWithLocation {
    let span = get_error_span_for_node(source_file.clone(), node);
    create_file_diagnostic(source_file, span.start, span.length, message, args)
}

pub fn create_diagnostic_for_node_from_message_chain<TNode: NodeInterface>(
    node: &TNode,
    message_chain: DiagnosticMessageChain,
    related_information: Option<Vec<DiagnosticRelatedInformation>>,
) -> DiagnosticWithLocation {
    let source_file = get_source_file_of_node(node);
    let span = get_error_span_for_node(source_file.clone(), node);
    create_file_diagnostic_from_message_chain(
        source_file,
        span.start,
        span.length,
        message_chain,
        related_information,
    )
}

fn create_file_diagnostic_from_message_chain(
    file: Rc<SourceFile>,
    start: isize,
    length: isize,
    message_chain: DiagnosticMessageChain,
    related_information: Option<Vec<DiagnosticRelatedInformation>>,
) -> DiagnosticWithLocation {
    // assert_diagnostic_location(&*file, start, length);
    DiagnosticWithLocation::new(BaseDiagnostic::new(
        BaseDiagnosticRelatedInformation::new(
            message_chain.code,
            Some(file),
            start,
            length,
            message_chain,
        ),
        related_information,
    ))
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
        .maybe_initializer()
}

pub fn set_value_declaration<TNode: NodeInterface>(symbol: &Symbol, node: &TNode) {
    {
        if !(symbol.maybe_value_declaration().is_none()) {
            return;
        }
    }
    symbol.set_value_declaration(node.node_wrapper());
}

fn walk_up<TNode: NodeInterface>(node: &TNode, kind: SyntaxKind) -> Option<Rc<Node>> {
    let mut node = Some(node.node_wrapper());
    loop {
        if let Some(node_present) = node.as_ref() {
            if node_present.kind() == kind {
                node = node_present.maybe_parent();
            } else {
                break;
            }
        } else {
            break;
        }
    }
    node
}

fn walk_up_parenthesized_expressions<TNode: NodeInterface>(node: &TNode) -> Option<Rc<Node>> {
    walk_up(node, SyntaxKind::ParenthesizedExpression)
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

fn get_root_declaration<TNode: NodeInterface>(node: &TNode) -> Rc<Node> {
    let mut node = node.node_wrapper();
    while node.kind() == SyntaxKind::BindingElement {
        node = node.parent().parent();
    }
    node
}

fn node_is_synthesized<TRange: ReadonlyTextRange>(range: &TRange) -> bool {
    position_is_synthesized(range.pos()) || position_is_synthesized(range.end())
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
        if let Some(diagnostics) = self
            .file_diagnostics
            .get_mut(&diagnostic.file().unwrap().file_name)
        {
            insert_sorted(
                diagnostics,
                diagnostic,
                |rc_diagnostic_a: &Rc<Diagnostic>, rc_diagnostic_b: &Rc<Diagnostic>| {
                    compare_diagnostics(&**rc_diagnostic_a, &**rc_diagnostic_b)
                },
            );
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
        insert_sorted(
            diagnostics,
            diagnostic,
            |rc_diagnostic_a: &Rc<Diagnostic>, rc_diagnostic_b: &Rc<Diagnostic>| {
                compare_diagnostics(&**rc_diagnostic_a, &**rc_diagnostic_b)
            },
        );
    }

    pub fn get_diagnostics(&self, file_name: &str) -> Vec<Rc<Diagnostic>> {
        self.file_diagnostics
            .get(file_name)
            .map(|sorted_array| sorted_array.into())
            .unwrap_or(vec![])
    }
}

fn escape_non_ascii_string(
    s: &str,
    quote_char: Option<
        char, /*CharacterCodes.doubleQuote | CharacterCodes.singleQuote | CharacterCodes.backtick*/
    >,
) -> String {
    s.to_string()
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

    fn reset(&mut self) {
        self.output = String::new();
    }
}

impl EmitTextWriter for TextWriter {
    fn write(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_trailing_semicolon(&mut self, text: &str) {
        self.write(text);
    }

    fn get_text(&self) -> String {
        self.output.clone()
    }
}

impl SymbolWriter for TextWriter {
    fn write_keyword(&mut self, text: &str) {
        self.write(text);
    }

    fn write_punctuation(&mut self, s: &str) {
        self.write(s);
    }

    fn write_space(&mut self, s: &str) {
        self.write(s);
    }

    fn write_string_literal(&mut self, s: &str) {
        self.write(s);
    }

    fn write_property(&mut self, s: &str) {
        self.write(s);
    }

    fn write_symbol(&mut self, s: &str, _: &Symbol) {
        self.write(s);
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

pub fn get_check_flags(symbol: &Symbol) -> CheckFlags {
    match symbol {
        Symbol::TransientSymbol(transient_symbol) => transient_symbol.check_flags(),
        _ => CheckFlags::None,
    }
}

pub fn is_write_only_access<TNode: NodeInterface>(node: &TNode) -> bool {
    access_kind(node) == AccessKind::Write
}

fn is_write_access<TNode: NodeInterface>(node: &TNode) -> bool {
    access_kind(node) != AccessKind::Read
}

#[derive(PartialEq, Eq)]
enum AccessKind {
    Read,
    Write,
    ReadWrite,
}

fn access_kind<TNode: NodeInterface>(node: &TNode) -> AccessKind {
    let parent = node.maybe_parent();
    if parent.is_none() {
        return AccessKind::Read;
    }
    let parent = parent.unwrap();

    let write_or_read_write = || {
        if let Some(grandparent) = parent.maybe_parent() {
            if walk_up_parenthesized_expressions(&*grandparent)
                .unwrap()
                .kind()
                == SyntaxKind::ExpressionStatement
            {
                return AccessKind::Write;
            }
        }
        AccessKind::ReadWrite
    };

    match &*parent {
        /*ParenthesizedExpression*/
        /*PostfixUnaryExpression*/
        Node::Expression(Expression::PrefixUnaryExpression(PrefixUnaryExpression {
            operator,
            ..
        })) => {
            if matches!(
                operator,
                SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
            ) {
                write_or_read_write()
            } else {
                AccessKind::Read
            }
        }
        Node::Expression(Expression::BinaryExpression(_)) => unimplemented!(),
        /*PropertyAccessExpression*/
        /*PropertyAssignment*/
        /*ShorthandPropertyAssignment*/
        Node::Expression(Expression::ArrayLiteralExpression(_)) => access_kind(&*parent),
        _ => AccessKind::Read,
    }
}

pub fn get_object_flags(type_: &Type) -> ObjectFlags {
    if type_.flags().intersects(TypeFlags::ObjectFlagsType) {
        type_.as_object_flags_type().object_flags()
    } else {
        ObjectFlags::None
    }
}

#[allow(non_snake_case)]
fn Symbol(flags: SymbolFlags, name: __String) -> BaseSymbol {
    BaseSymbol::new(flags, name)
}

#[allow(non_snake_case)]
fn _Type(flags: TypeFlags) -> BaseType {
    BaseType::new(flags)
}

#[allow(non_snake_case)]
fn Node(kind: SyntaxKind, pos: isize, end: isize) -> BaseNode {
    BaseNode::new(kind, NodeFlags::None, pos, end)
}

#[allow(non_snake_case)]
fn Token(kind: SyntaxKind, pos: isize, end: isize) -> BaseNode {
    BaseNode::new(kind, NodeFlags::None, pos, end)
}

#[allow(non_snake_case)]
fn Identifier(kind: SyntaxKind, pos: isize, end: isize) -> BaseNode {
    BaseNode::new(kind, NodeFlags::None, pos, end)
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

    pub fn get_symbol_constructor(&self) -> fn(SymbolFlags, __String) -> BaseSymbol {
        Symbol
    }

    pub fn get_type_constructor(&self) -> fn(TypeFlags) -> BaseType {
        _Type
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
    args: Option<Vec<String>>,
) -> DiagnosticWithDetachedLocation {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        if !args.is_empty() {
            text = format_string_from_args(&text, args);
        }
    }

    DiagnosticWithDetachedLocation::new(
        BaseDiagnostic::new(
            BaseDiagnosticRelatedInformation::new(message.code, None, start, length, text),
            None,
        ),
        file_name.to_string(),
    )
}

fn create_file_diagnostic(
    file: Rc<SourceFile>,
    start: isize,
    length: isize,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticWithLocation {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        if !args.is_empty() {
            text = format_string_from_args(&text, args);
        }
    }

    DiagnosticWithLocation::new(BaseDiagnostic::new(
        BaseDiagnosticRelatedInformation::new(message.code, Some(file), start, length, text),
        None,
    ))
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
    DiagnosticMessageChain::new(text, message.code, details.map(|details| vec![details]))
}

fn get_diagnostic_file_path<
    TDiagnosticRelatedInformation: DiagnosticRelatedInformationInterface,
>(
    diagnostic: &TDiagnosticRelatedInformation,
) -> Option<String> {
    diagnostic
        .file()
        .and_then(|file| file.path.as_ref().map(|path| path.to_string()))
}

fn compare_diagnostics<TDiagnosticRelatedInformation: DiagnosticRelatedInformationInterface>(
    d1: &TDiagnosticRelatedInformation,
    d2: &TDiagnosticRelatedInformation,
) -> Comparison {
    let mut compared = compare_diagnostics_skip_related_information(d1, d2);
    if compared != Comparison::EqualTo {
        return compared;
    }
    if let Some(d1) = d1.maybe_as_diagnostic() {
        if let Some(d2) = d2.maybe_as_diagnostic() {
            compared = compare_related_information(d1, d2);
            if compared != Comparison::EqualTo {
                return compared;
            }
        }
    }
    Comparison::EqualTo
}

fn compare_diagnostics_skip_related_information<
    TDiagnosticRelatedInformation: DiagnosticRelatedInformationInterface,
>(
    d1: &TDiagnosticRelatedInformation,
    d2: &TDiagnosticRelatedInformation,
) -> Comparison {
    let mut compared = compare_strings_case_sensitive(
        get_diagnostic_file_path(d1).as_deref(),
        get_diagnostic_file_path(d2).as_deref(),
    );
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_values(Some(d1.start()), Some(d2.start()));
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_values(Some(d1.length()), Some(d2.length()));
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_values(Some(d1.code()), Some(d2.code()));
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_message_text(d1.message_text(), d2.message_text());
    if compared != Comparison::EqualTo {
        return compared;
    }
    Comparison::EqualTo
}

fn compare_related_information(d1: &Diagnostic, d2: &Diagnostic) -> Comparison {
    if d1.maybe_related_information().is_none() && d2.maybe_related_information().is_none() {
        return Comparison::EqualTo;
    }
    if let Some(d1_related_information) = d1.maybe_related_information() {
        if let Some(d2_related_information) = d2.maybe_related_information() {
            let compared = compare_values(
                Some(d1_related_information.len()),
                Some(d2_related_information.len()),
            );
            if compared != Comparison::EqualTo {
                return compared;
            }
            let compared_maybe = for_each(d1_related_information, |d1i, index| {
                let d2i = &d2_related_information[index];
                let compared = compare_diagnostics(d1i, d2i);
                match compared {
                    Comparison::EqualTo => None,
                    compared => Some(compared),
                }
            });
            if let Some(compared) = compared_maybe {
                if compared != Comparison::EqualTo {
                    return compared;
                }
            }
            return Comparison::EqualTo;
        }
    }
    if d1.maybe_related_information().is_some() {
        Comparison::LessThan
    } else {
        Comparison::GreaterThan
    }
}

fn compare_message_text(t1: &DiagnosticMessageText, t2: &DiagnosticMessageText) -> Comparison {
    if let DiagnosticMessageText::String(t1) = t1 {
        if let DiagnosticMessageText::String(t2) = t2 {
            return compare_strings_case_sensitive(Some(t1), Some(t2));
        }
    }
    if matches!(t1, DiagnosticMessageText::String(_)) {
        return Comparison::LessThan;
    }
    if matches!(t2, DiagnosticMessageText::String(_)) {
        return Comparison::GreaterThan;
    }
    let t1 = enum_unwrapped!(t1, [DiagnosticMessageText, DiagnosticMessageChain]);
    let t2 = enum_unwrapped!(t2, [DiagnosticMessageText, DiagnosticMessageChain]);
    let mut res = compare_strings_case_sensitive(Some(&t1.message_text), Some(&t2.message_text));
    if res != Comparison::EqualTo {
        return res;
    }
    if t1.next.is_none() && t2.next.is_none() {
        return Comparison::EqualTo;
    }
    if t1.next.is_none() {
        return Comparison::LessThan;
    }
    if t2.next.is_none() {
        return Comparison::GreaterThan;
    }
    let t1_next = t1.next.as_ref().unwrap();
    let t2_next = t2.next.as_ref().unwrap();
    let len = cmp::min(t1_next.len(), t2_next.len());
    for i in 0..len {
        res = compare_message_text(&t1_next[i].clone().into(), &t2_next[i].clone().into());
        if res != Comparison::EqualTo {
            return res;
        }
    }
    if t1_next.len() < t2_next.len() {
        return Comparison::LessThan;
    }
    if t1_next.len() > t2_next.len() {
        return Comparison::GreaterThan;
    }
    Comparison::EqualTo
}

pub fn position_is_synthesized(pos: isize) -> bool {
    !(pos >= 0)
}

pub fn parse_pseudo_big_int(string_value: &str) -> String {
    string_value.to_string()
}

pub fn pseudo_big_int_to_string(pseudo_big_int: &PseudoBigInt) -> String {
    let negative = pseudo_big_int.negative;
    let base_10_value = &pseudo_big_int.base_10_value;
    format!(
        "{}{}",
        if negative && base_10_value != "0" {
            "-"
        } else {
            ""
        },
        base_10_value
    )
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
