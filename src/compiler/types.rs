use std::rc::Rc;

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
    Identifier,
    FalseKeyword,
    TrueKeyword,
    BinaryExpression,
    EmptyStatement,
    ExpressionStatement,
    SourceFile,
}

#[derive(Debug)]
pub struct BaseNode {
    pub kind: SyntaxKind,
}

pub trait NodeInterface {
    fn kind(&self) -> SyntaxKind;
}

#[derive(Debug)]
pub enum Node {
    Token(Token),
    Expression(Expression),
    Statement(Statement),
}

impl NodeInterface for Node {
    fn kind(&self) -> SyntaxKind {
        match self {
            Node::Token(token) => token.kind(),
            Node::Expression(expression) => expression.kind(),
            Node::Statement(statement) => statement.kind(),
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub _node: BaseNode,
}

impl NodeInterface for Token {
    fn kind(&self) -> SyntaxKind {
        self._node.kind
    }
}

impl From<Token> for Node {
    fn from(token: Token) -> Self {
        Node::Token(token)
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
    Identifier(Identifier),
    BinaryExpression(BinaryExpression),
    LiteralLikeNode(LiteralLikeNode),
}

impl NodeInterface for Expression {
    fn kind(&self) -> SyntaxKind {
        match self {
            Expression::Identifier(identifier) => identifier.kind(),
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

#[derive(Debug)]
pub struct BinaryExpression {
    pub _node: BaseNode,
    pub left: Box<Expression>,
    pub operator_token: Box<Node>,
    pub right: Box<Expression>,
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
}

pub trait Program {
    fn get_source_files(&self) -> &[Rc<SourceFile>];
    fn get_semantic_diagnostics(&self) -> Vec<Box<dyn Diagnostic>>;
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
