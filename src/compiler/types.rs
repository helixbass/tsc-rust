#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::collections::HashMap;
use std::rc::Rc;

use crate::SortedArray;

pub struct Path(String);

impl Path {
    pub fn new(string: String) -> Self {
        Self(string)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum SyntaxKind {
    Unknown,
    EndOfFileToken,
    NumericLiteral,
    SemicolonToken,
    AsteriskToken,
    PlusPlusToken,
    Identifier,
    FalseKeyword,
    TrueKeyword,
    PrefixUnaryExpression,
    BinaryExpression,
    EmptyStatement,
    ExpressionStatement,
    SourceFile,
}

bitflags! {
    pub struct RelationComparisonResult: u32 {
        const Succeeded = 1 << 0;
        const Failed = 1 << 1;
    }
}

pub trait NodeInterface {
    fn kind(&self) -> SyntaxKind;
}

#[derive(Debug)]
pub enum Node {
    BaseNode(BaseNode),
    Expression(Expression),
    Statement(Statement),
}

impl NodeInterface for Node {
    fn kind(&self) -> SyntaxKind {
        match self {
            Node::BaseNode(base_node) => base_node.kind(),
            Node::Expression(expression) => expression.kind(),
            Node::Statement(statement) => statement.kind(),
        }
    }
}

#[derive(Debug)]
pub struct BaseNode {
    pub kind: SyntaxKind,
}

impl NodeInterface for BaseNode {
    fn kind(&self) -> SyntaxKind {
        self.kind
    }
}

impl From<BaseNode> for Node {
    fn from(base_node: BaseNode) -> Self {
        Node::BaseNode(base_node)
    }
}

#[derive(Debug)]
pub struct NodeArray {
    _nodes: Vec<Node>,
}

impl NodeArray {
    pub fn new(nodes: Vec<Node>) -> Self {
        NodeArray { _nodes: nodes }
    }

    pub fn iter(&self) -> NodeArrayIter {
        NodeArrayIter(Box::new(self._nodes.iter()))
    }
}

pub struct NodeArrayIter<'node_array>(Box<dyn Iterator<Item = &'node_array Node> + 'node_array>);

impl<'node_array> Iterator for NodeArrayIter<'node_array> {
    type Item = &'node_array Node;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'node_array> IntoIterator for &'node_array NodeArray {
    type Item = &'node_array Node;
    type IntoIter = NodeArrayIter<'node_array>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub enum NodeArrayOrVec {
    NodeArray(NodeArray),
    Vec(Vec<Node>),
}

impl From<NodeArray> for NodeArrayOrVec {
    fn from(node_array: NodeArray) -> Self {
        NodeArrayOrVec::NodeArray(node_array)
    }
}

impl From<Vec<Node>> for NodeArrayOrVec {
    fn from(vec: Vec<Node>) -> Self {
        NodeArrayOrVec::Vec(vec)
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub _node: BaseNode,
    pub escaped_text: String,
}

impl NodeInterface for Identifier {
    fn kind(&self) -> SyntaxKind {
        self._node.kind
    }
}

impl From<Identifier> for Expression {
    fn from(identifier: Identifier) -> Self {
        Expression::Identifier(identifier)
    }
}

#[derive(Debug)]
pub enum Expression {
    TokenExpression(BaseNode),
    Identifier(Identifier),
    PrefixUnaryExpression(PrefixUnaryExpression),
    BinaryExpression(BinaryExpression),
    LiteralLikeNode(LiteralLikeNode),
}

impl NodeInterface for Expression {
    fn kind(&self) -> SyntaxKind {
        match self {
            Expression::TokenExpression(token_expression) => token_expression.kind(),
            Expression::Identifier(identifier) => identifier.kind(),
            Expression::PrefixUnaryExpression(prefix_unary_expression) => {
                prefix_unary_expression.kind()
            }
            Expression::BinaryExpression(binary_expression) => binary_expression.kind(),
            Expression::LiteralLikeNode(literal_like_node) => literal_like_node.kind(),
        }
    }
}

impl From<Expression> for Node {
    fn from(expression: Expression) -> Self {
        Node::Expression(expression)
    }
}

impl From<BaseNode> for Expression {
    fn from(base_node: BaseNode) -> Self {
        Expression::TokenExpression(base_node)
    }
}

#[derive(Debug)]
pub struct PrefixUnaryExpression {
    pub _node: BaseNode,
    pub operator: SyntaxKind,
    pub operand: Box<Expression>,
}

impl PrefixUnaryExpression {
    pub fn new(base_node: BaseNode, operator: SyntaxKind, operand: Expression) -> Self {
        Self {
            _node: base_node,
            operator,
            operand: Box::new(operand),
        }
    }
}

impl NodeInterface for PrefixUnaryExpression {
    fn kind(&self) -> SyntaxKind {
        self._node.kind
    }
}

impl From<PrefixUnaryExpression> for Expression {
    fn from(prefix_unary_expression: PrefixUnaryExpression) -> Self {
        Expression::PrefixUnaryExpression(prefix_unary_expression)
    }
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub _node: BaseNode,
    pub left: Box<Expression>,
    pub operator_token: Box<Node>,
    pub right: Box<Expression>,
}

impl BinaryExpression {
    pub fn new(
        base_node: BaseNode,
        left: Expression,
        operator_token: Node,
        right: Expression,
    ) -> Self {
        Self {
            _node: base_node,
            left: Box::new(left),
            operator_token: Box::new(operator_token),
            right: Box::new(right),
        }
    }
}

impl NodeInterface for BinaryExpression {
    fn kind(&self) -> SyntaxKind {
        self._node.kind
    }
}

impl From<BinaryExpression> for Expression {
    fn from(binary_expression: BinaryExpression) -> Self {
        Expression::BinaryExpression(binary_expression)
    }
}

#[derive(Debug)]
pub struct BaseLiteralLikeNode {
    pub _node: BaseNode,
    pub text: String,
}

pub trait LiteralLikeNodeInterface {
    fn text(&self) -> &str;
}

#[derive(Debug)]
pub enum LiteralLikeNode {
    NumericLiteral(NumericLiteral),
}

impl NodeInterface for LiteralLikeNode {
    fn kind(&self) -> SyntaxKind {
        match self {
            LiteralLikeNode::NumericLiteral(numeric_literal) => numeric_literal.kind(),
        }
    }
}

impl From<LiteralLikeNode> for Expression {
    fn from(literal_like_node: LiteralLikeNode) -> Self {
        Expression::LiteralLikeNode(literal_like_node)
    }
}

impl LiteralLikeNodeInterface for LiteralLikeNode {
    fn text(&self) -> &str {
        match self {
            LiteralLikeNode::NumericLiteral(numeric_literal) => numeric_literal.text(),
        }
    }
}

#[derive(Debug)]
pub struct NumericLiteral {
    pub _literal_like_node: BaseLiteralLikeNode,
}

impl NodeInterface for NumericLiteral {
    fn kind(&self) -> SyntaxKind {
        self._literal_like_node._node.kind
    }
}

impl From<NumericLiteral> for LiteralLikeNode {
    fn from(numeric_literal: NumericLiteral) -> Self {
        LiteralLikeNode::NumericLiteral(numeric_literal)
    }
}

impl LiteralLikeNodeInterface for NumericLiteral {
    fn text(&self) -> &str {
        &self._literal_like_node.text
    }
}

#[derive(Debug)]
pub enum Statement {
    EmptyStatement(EmptyStatement),
    ExpressionStatement(ExpressionStatement),
}

impl NodeInterface for Statement {
    fn kind(&self) -> SyntaxKind {
        match self {
            Statement::EmptyStatement(empty_statement) => empty_statement.kind(),
            Statement::ExpressionStatement(expression_statement) => expression_statement.kind(),
        }
    }
}

impl From<Statement> for Node {
    fn from(statement: Statement) -> Self {
        Node::Statement(statement)
    }
}

#[derive(Debug)]
pub struct EmptyStatement {
    pub _node: BaseNode,
}

impl NodeInterface for EmptyStatement {
    fn kind(&self) -> SyntaxKind {
        self._node.kind
    }
}

impl From<EmptyStatement> for Statement {
    fn from(empty_statement: EmptyStatement) -> Self {
        Statement::EmptyStatement(empty_statement)
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub _node: BaseNode,
    pub expression: Expression,
}

impl NodeInterface for ExpressionStatement {
    fn kind(&self) -> SyntaxKind {
        self._node.kind
    }
}

impl From<ExpressionStatement> for Statement {
    fn from(expression_statement: ExpressionStatement) -> Self {
        Statement::ExpressionStatement(expression_statement)
    }
}

#[derive(Debug)]
pub struct SourceFile {
    pub _node: BaseNode,
    pub statements: NodeArray,

    pub file_name: String,
}

pub trait Program {
    fn get_source_files(&self) -> Vec<Rc<SourceFile>>;
    fn get_semantic_diagnostics(&mut self) -> Vec<Box<dyn Diagnostic>>;
}

#[derive(Eq, PartialEq)]
pub enum StructureIsReused {
    Not,
    Completely,
}

pub enum ExitStatus {
    Success,
    #[allow(non_camel_case_types)]
    DiagnosticsPresent_OutputsGenerated,
}

#[allow(non_snake_case)]
pub struct TypeChecker {
    pub Type: fn(TypeFlags) -> BaseType,
    pub number_type: Option<Rc<Type>>,
    pub bigint_type: Option<Rc<Type>>,
    pub true_type: Option<Rc<Type>>,
    pub number_or_big_int_type: Option<Rc<Type>>,
    pub diagnostics: DiagnosticCollection,
    pub assignable_relation: HashMap<String, RelationComparisonResult>,
}

bitflags! {
    pub struct TypeFlags: u32 {
        const Number = 1 << 3;
        const BigInt = 1 << 6;
        const BooleanLiteral = 1 << 9;
    }
}

#[derive(Clone)]
pub enum Type {
    IntrinsicType(IntrinsicType),
}

impl TypeInterface for Type {
    fn flags(&self) -> TypeFlags {
        match self {
            Type::IntrinsicType(intrinsic_type) => intrinsic_type.flags(),
        }
    }
}

pub trait TypeInterface {
    fn flags(&self) -> TypeFlags;
}

#[derive(Clone)]
pub struct BaseType {
    pub flags: TypeFlags,
}

impl TypeInterface for BaseType {
    fn flags(&self) -> TypeFlags {
        self.flags
    }
}

pub trait IntrinsicTypeInterface: TypeInterface {}

#[derive(Clone)]
pub enum IntrinsicType {
    BaseIntrinsicType(BaseIntrinsicType),
    FreshableIntrinsicType(FreshableIntrinsicType),
}

impl TypeInterface for IntrinsicType {
    fn flags(&self) -> TypeFlags {
        match self {
            IntrinsicType::BaseIntrinsicType(base_intrinsic_type) => base_intrinsic_type.flags(),
            IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                freshable_intrinsic_type.flags()
            }
        }
    }
}

impl IntrinsicTypeInterface for IntrinsicType {}

impl From<IntrinsicType> for Type {
    fn from(intrinsic_type: IntrinsicType) -> Self {
        Type::IntrinsicType(intrinsic_type)
    }
}

#[derive(Clone)]
pub struct BaseIntrinsicType {
    _type: BaseType,
}

impl BaseIntrinsicType {
    pub fn new(type_: BaseType) -> Self {
        Self { _type: type_ }
    }
}

impl TypeInterface for BaseIntrinsicType {
    fn flags(&self) -> TypeFlags {
        self._type.flags()
    }
}

impl IntrinsicTypeInterface for BaseIntrinsicType {}

impl From<BaseIntrinsicType> for IntrinsicType {
    fn from(base_intrinsic_type: BaseIntrinsicType) -> Self {
        IntrinsicType::BaseIntrinsicType(base_intrinsic_type)
    }
}

impl From<BaseIntrinsicType> for Type {
    fn from(base_intrinsic_type: BaseIntrinsicType) -> Self {
        Type::IntrinsicType(IntrinsicType::BaseIntrinsicType(base_intrinsic_type))
    }
}

#[derive(Clone)]
pub struct FreshableIntrinsicType {
    _intrinsic_type: BaseIntrinsicType,
}

impl FreshableIntrinsicType {
    pub fn new(intrinsic_type: BaseIntrinsicType) -> Self {
        Self {
            _intrinsic_type: intrinsic_type,
        }
    }
}

impl TypeInterface for FreshableIntrinsicType {
    fn flags(&self) -> TypeFlags {
        self._intrinsic_type.flags()
    }
}

impl IntrinsicTypeInterface for FreshableIntrinsicType {}

impl From<FreshableIntrinsicType> for IntrinsicType {
    fn from(freshable_intrinsic_type: FreshableIntrinsicType) -> Self {
        IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type)
    }
}

impl From<FreshableIntrinsicType> for Type {
    fn from(freshable_intrinsic_type: FreshableIntrinsicType) -> Self {
        Type::IntrinsicType(IntrinsicType::FreshableIntrinsicType(
            freshable_intrinsic_type,
        ))
    }
}

#[derive(Debug)]
pub struct ParsedCommandLine {
    pub file_names: Vec<String>,
}

pub struct CreateProgramOptions<'config> {
    pub root_names: &'config [String],
}

#[non_exhaustive]
pub struct CharacterCodes;
#[allow(non_upper_case_globals)]
impl CharacterCodes {
    pub const max_ascii_character: char = '';

    pub const space: char = ' ';

    pub const underscore: char = '_';
    pub const dollar_sign: char = '$';

    pub const _0: char = '0';
    pub const _1: char = '1';
    pub const _2: char = '2';
    pub const _3: char = '3';
    pub const _4: char = '4';
    pub const _5: char = '5';
    pub const _6: char = '6';
    pub const _7: char = '7';
    pub const _8: char = '8';
    pub const _9: char = '9';

    pub const a: char = 'a';
    pub const b: char = 'b';
    pub const c: char = 'c';
    pub const d: char = 'd';
    pub const e: char = 'e';
    pub const f: char = 'f';
    pub const g: char = 'g';
    pub const h: char = 'h';
    pub const i: char = 'i';
    pub const j: char = 'j';
    pub const k: char = 'k';
    pub const l: char = 'l';
    pub const m: char = 'm';
    pub const n: char = 'n';
    pub const o: char = 'o';
    pub const p: char = 'p';
    pub const q: char = 'q';
    pub const r: char = 'r';
    pub const s: char = 's';
    pub const t: char = 't';
    pub const u: char = 'u';
    pub const v: char = 'v';
    pub const w: char = 'w';
    pub const x: char = 'x';
    pub const y: char = 'y';
    pub const z: char = 'z';

    pub const A: char = 'A';
    pub const B: char = 'B';
    pub const C: char = 'C';
    pub const D: char = 'D';
    pub const E: char = 'E';
    pub const F: char = 'F';
    pub const G: char = 'G';
    pub const H: char = 'H';
    pub const I: char = 'I';
    pub const J: char = 'J';
    pub const K: char = 'K';
    pub const L: char = 'L';
    pub const M: char = 'M';
    pub const N: char = 'N';
    pub const O: char = 'O';
    pub const P: char = 'P';
    pub const Q: char = 'Q';
    pub const R: char = 'R';
    pub const S: char = 'S';
    pub const T: char = 'T';
    pub const U: char = 'U';
    pub const V: char = 'V';
    pub const W: char = 'W';
    pub const X: char = 'X';
    pub const Y: char = 'Y';
    pub const Z: char = 'Z';

    pub const asterisk: char = '*';
    pub const plus: char = '+';
    pub const semicolon: char = ';';
    pub const slash: char = '/';
}

pub trait ModuleResolutionHost {
    fn read_file(&self, file_name: &str) -> Option<String>;
}

pub trait CompilerHost: ModuleResolutionHost {
    fn get_source_file(&self, file_name: &str) -> Option<SourceFile>;
    fn get_current_directory(&self) -> String;
    fn get_canonical_file_name(&self, file_name: &str) -> String;
}

pub struct DiagnosticMessage {
    pub key: &'static str,
    pub category: DiagnosticCategory,
    pub code: u32,
    pub message: &'static str,
}

pub trait Diagnostic: DiagnosticRelatedInformation {}

pub trait DiagnosticRelatedInformation {}

#[derive(Clone)]
pub struct DiagnosticWithLocation {
    pub file: Rc<SourceFile>,
    pub start: usize,
    pub length: usize,
}

impl DiagnosticRelatedInformation for DiagnosticWithLocation {}

impl Diagnostic for DiagnosticWithLocation {}

pub struct DiagnosticWithDetachedLocation {
    pub file_name: String,
    pub start: usize,
    pub length: usize,
}

impl DiagnosticRelatedInformation for DiagnosticWithDetachedLocation {}

impl Diagnostic for DiagnosticWithDetachedLocation {}

pub enum DiagnosticCategory {
    Warning,
    Error,
    Suggestion,
    Message,
}

pub struct NodeFactory {}

pub struct DiagnosticCollection {
    pub file_diagnostics: HashMap<String, SortedArray<DiagnosticWithLocation>>,
}
