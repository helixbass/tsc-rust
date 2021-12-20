#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::RwLock;

use crate::{Number, SortedArray, WeakSelf};

pub struct Path(String);

impl Path {
    pub fn new(string: String) -> Self {
        Self(string)
    }
}

pub trait ReadonlyTextRange {
    fn pos(&self) -> usize;
    fn set_pos(&self, pos: usize);
    fn end(&self) -> usize;
    fn set_end(&self, end: usize);
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum SyntaxKind {
    Unknown,
    EndOfFileToken,
    NumericLiteral,
    CloseBraceToken,
    SemicolonToken,
    AsteriskToken,
    PlusPlusToken,
    ColonToken,
    EqualsToken,
    Identifier,
    ConstKeyword,
    FalseKeyword,
    TrueKeyword,
    WithKeyword,
    NumberKeyword,
    OfKeyword,
    PrefixUnaryExpression,
    BinaryExpression,
    EmptyStatement,
    ExpressionStatement,
    FunctionDeclaration,
    SourceFile,
}

impl SyntaxKind {
    pub const LastReservedWord: SyntaxKind = SyntaxKind::WithKeyword;
    pub const LastKeyword: SyntaxKind = SyntaxKind::OfKeyword;
    pub const LastToken: SyntaxKind = SyntaxKind::LastKeyword;
}

bitflags! {
    pub struct NodeFlags: u32 {
        const None = 0;
        const Const = 1 << 1;
        const YieldContext = 1 << 13;
        const AwaitContext = 1 << 15;

        const TypeExcludesFlags = Self::YieldContext.bits | Self::AwaitContext.bits;
    }
}

bitflags! {
    pub struct RelationComparisonResult: u32 {
        const Succeeded = 1 << 0;
        const Failed = 1 << 1;
    }
}

pub trait NodeInterface: ReadonlyTextRange {
    fn kind(&self) -> SyntaxKind;
    fn parent(&self) -> Rc<Node>;
    fn set_parent(&self, parent: Rc<Node>);
}

#[derive(Debug)]
pub enum Node {
    BaseNode(BaseNode),
    VariableDeclaration(VariableDeclaration),
    VariableDeclarationList(VariableDeclarationList),
    TypeNode(TypeNode),
    Expression(Expression),
    Statement(Statement),
    SourceFile(Rc<SourceFile>),
}

impl ReadonlyTextRange for Node {
    fn pos(&self) -> usize {
        match self {
            Node::BaseNode(base_node) => base_node.pos(),
            Node::Expression(expression) => expression.pos(),
            Node::Statement(statement) => statement.pos(),
            Node::SourceFile(source_file) => source_file.pos(),
        }
    }

    fn set_pos(&self, pos: usize) {
        match self {
            Node::BaseNode(base_node) => base_node.set_pos(pos),
            Node::Expression(expression) => expression.set_pos(pos),
            Node::Statement(statement) => statement.set_pos(pos),
            Node::SourceFile(source_file) => source_file.set_pos(pos),
        }
    }

    fn end(&self) -> usize {
        match self {
            Node::BaseNode(base_node) => base_node.end(),
            Node::Expression(expression) => expression.end(),
            Node::Statement(statement) => statement.end(),
            Node::SourceFile(source_file) => source_file.end(),
        }
    }

    fn set_end(&self, end: usize) {
        match self {
            Node::BaseNode(base_node) => base_node.set_end(end),
            Node::Expression(expression) => expression.set_end(end),
            Node::Statement(statement) => statement.set_end(end),
            Node::SourceFile(source_file) => source_file.set_end(end),
        }
    }
}

impl NodeInterface for Node {
    fn kind(&self) -> SyntaxKind {
        match self {
            Node::BaseNode(base_node) => base_node.kind(),
            Node::Expression(expression) => expression.kind(),
            Node::Statement(statement) => statement.kind(),
            Node::SourceFile(source_file) => source_file.kind(),
        }
    }

    fn parent(&self) -> Rc<Node> {
        match self {
            Node::BaseNode(base_node) => base_node.parent(),
            Node::Expression(expression) => expression.parent(),
            Node::Statement(statement) => statement.parent(),
            Node::SourceFile(source_file) => source_file.parent(),
        }
    }

    fn set_parent(&self, parent: Rc<Node>) {
        match self {
            Node::BaseNode(base_node) => base_node.set_parent(parent),
            Node::Expression(expression) => expression.set_parent(parent),
            Node::Statement(statement) => statement.set_parent(parent),
            Node::SourceFile(source_file) => source_file.set_parent(parent),
        }
    }
}

#[derive(Debug)]
pub struct BaseNode {
    pub kind: SyntaxKind,
    pub parent: RwLock<Option<Weak<Node>>>,
    pub pos: AtomicUsize,
    pub end: AtomicUsize,
}

impl BaseNode {
    pub fn new(kind: SyntaxKind, pos: usize, end: usize) -> Self {
        Self {
            kind,
            parent: RwLock::new(None),
            pos: pos.into(),
            end: end.into(),
        }
    }
}

impl NodeInterface for BaseNode {
    fn kind(&self) -> SyntaxKind {
        self.kind
    }

    fn parent(&self) -> Rc<Node> {
        self.parent
            .try_read()
            .unwrap()
            .clone()
            .unwrap()
            .upgrade()
            .unwrap()
    }

    fn set_parent(&self, parent: Rc<Node>) {
        *self.parent.try_write().unwrap() = Some(Rc::downgrade(&parent));
    }
}

impl ReadonlyTextRange for BaseNode {
    fn pos(&self) -> usize {
        self.pos.load(Ordering::Relaxed)
    }

    fn set_pos(&self, pos: usize) {
        self.pos.store(pos, Ordering::Relaxed);
    }

    fn end(&self) -> usize {
        self.end.load(Ordering::Relaxed)
    }

    fn set_end(&self, end: usize) {
        self.end.store(end, Ordering::Relaxed);
    }
}

impl From<BaseNode> for Node {
    fn from(base_node: BaseNode) -> Self {
        Node::BaseNode(base_node)
    }
}

#[derive(Debug)]
pub struct NodeArray {
    _nodes: Vec<Rc<Node>>,
}

impl NodeArray {
    pub fn new(nodes: Vec<Rc<Node>>) -> Self {
        NodeArray { _nodes: nodes }
    }

    pub fn iter(&self) -> NodeArrayIter {
        NodeArrayIter(Box::new(self._nodes.iter()))
    }
}

pub struct NodeArrayIter<'node_array>(
    Box<dyn Iterator<Item = &'node_array Rc<Node>> + 'node_array>,
);

impl<'node_array> Iterator for NodeArrayIter<'node_array> {
    type Item = &'node_array Rc<Node>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'node_array> IntoIterator for &'node_array NodeArray {
    type Item = &'node_array Rc<Node>;
    type IntoIter = NodeArrayIter<'node_array>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub enum NodeArrayOrVec {
    NodeArray(NodeArray),
    Vec(Vec<Rc<Node>>),
}

impl From<NodeArray> for NodeArrayOrVec {
    fn from(node_array: NodeArray) -> Self {
        NodeArrayOrVec::NodeArray(node_array)
    }
}

impl From<Vec<Rc<Node>>> for NodeArrayOrVec {
    fn from(vec: Vec<Rc<Node>>) -> Self {
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

    fn parent(&self) -> Rc<Node> {
        self._node.parent()
    }

    fn set_parent(&self, parent: Rc<Node>) {
        self._node.set_parent(parent)
    }
}

impl ReadonlyTextRange for Identifier {
    fn pos(&self) -> usize {
        self._node.pos()
    }

    fn set_pos(&self, pos: usize) {
        self._node.set_pos(pos);
    }

    fn end(&self) -> usize {
        self._node.end()
    }

    fn set_end(&self, end: usize) {
        self._node.set_end(end);
    }
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub _node: BaseNode,
}

impl NodeInterface for VariableDeclaration {
    fn kind(&self) -> SyntaxKind {
        self._node.kind()
    }

    fn parent(&self) -> Rc<Node> {
        self._node.parent()
    }

    fn set_parent(&self, parent: Rc<Node>) {
        self._node.set_parent(parent)
    }
}

impl ReadonlyTextRange for VariableDeclaration {
    fn pos(&self) -> usize {
        self._node.pos()
    }

    fn set_pos(&self, pos: usize) {
        self._node.set_pos(pos);
    }

    fn end(&self) -> usize {
        self._node.end()
    }

    fn set_end(&self, end: usize) {
        self._node.set_end(end);
    }
}

impl From<VariableDeclaration> for Node {
    fn from(variable_declaration: VariableDeclaration) -> Self {
        Node::VariableDeclaration(variable_declaration)
    }
}

#[derive(Debug)]
pub struct VariableDeclarationList {
    pub _node: BaseNode,
    pub declarations: NodeArray, /*<VariableDeclaration>*/
}

impl NodeInterface for VariableDeclarationList {
    fn kind(&self) -> SyntaxKind {
        self._node.kind()
    }

    fn parent(&self) -> Rc<Node> {
        self._node.parent()
    }

    fn set_parent(&self, parent: Rc<Node>) {
        self._node.set_parent(parent)
    }
}

impl ReadonlyTextRange for VariableDeclarationList {
    fn pos(&self) -> usize {
        self._node.pos()
    }

    fn set_pos(&self, pos: usize) {
        self._node.set_pos(pos);
    }

    fn end(&self) -> usize {
        self._node.end()
    }

    fn set_end(&self, end: usize) {
        self._node.set_end(end);
    }
}

impl From<VariableDeclarationList> for Node {
    fn from(variable_declaration_list: VariableDeclarationList) -> Self {
        Node::VariableDeclarationList(variable_declaration_list)
    }
}

impl From<Identifier> for Expression {
    fn from(identifier: Identifier) -> Self {
        Expression::Identifier(identifier)
    }
}

impl From<Identifier> for Node {
    fn from(identifier: Identifier) -> Self {
        Node::Expression(Expression::Identifier(identifier))
    }
}

#[derive(Debug)]
pub enum TypeNode {
    KeywordTypeNode(KeywordTypeNode),
}

impl NodeInterface for TypeNode {
    fn kind(&self) -> SyntaxKind {
        match self {
            TypeNode::KeywordTypeNode(keyword_type_node) => keyword_type_node.kind(),
        }
    }

    fn parent(&self) -> Rc<Node> {
        match self {
            TypeNode::KeywordTypeNode(keyword_type_node) => keyword_type_node.parent(),
        }
    }

    fn set_parent(&self, parent: Rc<Node>) {
        match self {
            TypeNode::KeywordTypeNode(keyword_type_node) => keyword_type_node.set_parent(parent),
        }
    }
}

impl ReadonlyTextRange for TypeNode {
    fn pos(&self) -> usize {
        match self {
            TypeNode::KeywordTypeNode(keyword_type_node) => keyword_type_node.pos(),
        }
    }

    fn set_pos(&self, pos: usize) {
        match self {
            TypeNode::KeywordTypeNode(keyword_type_node) => keyword_type_node.set_pos(pos),
        }
    }

    fn end(&self) -> usize {
        match self {
            TypeNode::KeywordTypeNode(keyword_type_node) => keyword_type_node.end(),
        }
    }

    fn set_end(&self, end: usize) {
        match self {
            TypeNode::KeywordTypeNode(keyword_type_node) => keyword_type_node.set_end(end),
        }
    }
}

impl From<TypeNode> for Node {
    fn from(type_node: TypeNode) -> Self {
        Node::TypeNode(type_node)
    }
}

#[derive(Debug)]
pub struct KeywordTypeNode {
    _node: BaseNode,
}

impl KeywordTypeNode {
    pub fn new(base_node: BaseNode) -> Self {
        Self { _node: base_node }
    }
}

impl NodeInterface for KeywordTypeNode {
    fn kind(&self) -> SyntaxKind {
        self._node.kind()
    }

    fn parent(&self) -> Rc<Node> {
        self._node.parent()
    }

    fn set_parent(&self, parent: Rc<Node>) {
        self._node.set_parent(parent)
    }
}

impl ReadonlyTextRange for KeywordTypeNode {
    fn pos(&self) -> usize {
        self._node.pos()
    }

    fn set_pos(&self, pos: usize) {
        self._node.set_pos(pos);
    }

    fn end(&self) -> usize {
        self._node.end()
    }

    fn set_end(&self, end: usize) {
        self._node.set_end(end);
    }
}

impl From<KeywordTypeNode> for TypeNode {
    fn from(keyword_type_node: KeywordTypeNode) -> Self {
        TypeNode::KeywordTypeNode(keyword_type_node)
    }
}

impl From<BaseNode> for KeywordTypeNode {
    fn from(base_node: BaseNode) -> Self {
        KeywordTypeNode::new(base_node)
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

    fn parent(&self) -> Rc<Node> {
        match self {
            Expression::TokenExpression(token_expression) => token_expression.parent(),
            Expression::Identifier(identifier) => identifier.parent(),
            Expression::PrefixUnaryExpression(prefix_unary_expression) => {
                prefix_unary_expression.parent()
            }
            Expression::BinaryExpression(binary_expression) => binary_expression.parent(),
            Expression::LiteralLikeNode(literal_like_node) => literal_like_node.parent(),
        }
    }

    fn set_parent(&self, parent: Rc<Node>) {
        match self {
            Expression::TokenExpression(token_expression) => token_expression.set_parent(parent),
            Expression::Identifier(identifier) => identifier.set_parent(parent),
            Expression::PrefixUnaryExpression(prefix_unary_expression) => {
                prefix_unary_expression.set_parent(parent)
            }
            Expression::BinaryExpression(binary_expression) => binary_expression.set_parent(parent),
            Expression::LiteralLikeNode(literal_like_node) => literal_like_node.set_parent(parent),
        }
    }
}

impl ReadonlyTextRange for Expression {
    fn pos(&self) -> usize {
        match self {
            Expression::TokenExpression(token_expression) => token_expression.pos(),
            Expression::Identifier(identifier) => identifier.pos(),
            Expression::PrefixUnaryExpression(prefix_unary_expression) => {
                prefix_unary_expression.pos()
            }
            Expression::BinaryExpression(binary_expression) => binary_expression.pos(),
            Expression::LiteralLikeNode(literal_like_node) => literal_like_node.pos(),
        }
    }

    fn set_pos(&self, pos: usize) {
        match self {
            Expression::TokenExpression(token_expression) => token_expression.set_pos(pos),
            Expression::Identifier(identifier) => identifier.set_pos(pos),
            Expression::PrefixUnaryExpression(prefix_unary_expression) => {
                prefix_unary_expression.set_pos(pos)
            }
            Expression::BinaryExpression(binary_expression) => binary_expression.set_pos(pos),
            Expression::LiteralLikeNode(literal_like_node) => literal_like_node.set_pos(pos),
        }
    }

    fn end(&self) -> usize {
        match self {
            Expression::TokenExpression(token_expression) => token_expression.end(),
            Expression::Identifier(identifier) => identifier.end(),
            Expression::PrefixUnaryExpression(prefix_unary_expression) => {
                prefix_unary_expression.end()
            }
            Expression::BinaryExpression(binary_expression) => binary_expression.end(),
            Expression::LiteralLikeNode(literal_like_node) => literal_like_node.end(),
        }
    }

    fn set_end(&self, end: usize) {
        match self {
            Expression::TokenExpression(token_expression) => token_expression.set_end(end),
            Expression::Identifier(identifier) => identifier.set_end(end),
            Expression::PrefixUnaryExpression(prefix_unary_expression) => {
                prefix_unary_expression.set_end(end)
            }
            Expression::BinaryExpression(binary_expression) => binary_expression.set_end(end),
            Expression::LiteralLikeNode(literal_like_node) => literal_like_node.set_end(end),
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
    pub operand: Rc<Node>,
}

impl PrefixUnaryExpression {
    pub fn new(base_node: BaseNode, operator: SyntaxKind, operand: Expression) -> Self {
        Self {
            _node: base_node,
            operator,
            operand: Rc::new(operand.into()),
        }
    }
}

impl NodeInterface for PrefixUnaryExpression {
    fn kind(&self) -> SyntaxKind {
        self._node.kind()
    }

    fn parent(&self) -> Rc<Node> {
        self._node.parent()
    }

    fn set_parent(&self, parent: Rc<Node>) {
        self._node.set_parent(parent)
    }
}

impl ReadonlyTextRange for PrefixUnaryExpression {
    fn pos(&self) -> usize {
        self._node.pos()
    }

    fn set_pos(&self, pos: usize) {
        self._node.set_pos(pos);
    }

    fn end(&self) -> usize {
        self._node.end()
    }

    fn set_end(&self, end: usize) {
        self._node.set_end(end);
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
    pub left: Rc<Node>,
    pub operator_token: Box<Node>,
    pub right: Rc<Node>,
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
            left: Rc::new(left.into()),
            operator_token: Box::new(operator_token),
            right: Rc::new(right.into()),
        }
    }
}

impl NodeInterface for BinaryExpression {
    fn kind(&self) -> SyntaxKind {
        self._node.kind()
    }

    fn parent(&self) -> Rc<Node> {
        self._node.parent()
    }

    fn set_parent(&self, parent: Rc<Node>) {
        self._node.set_parent(parent)
    }
}

impl ReadonlyTextRange for BinaryExpression {
    fn pos(&self) -> usize {
        self._node.pos()
    }

    fn set_pos(&self, pos: usize) {
        self._node.set_pos(pos);
    }

    fn end(&self) -> usize {
        self._node.end()
    }

    fn set_end(&self, end: usize) {
        self._node.set_end(end);
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

impl NodeInterface for BaseLiteralLikeNode {
    fn kind(&self) -> SyntaxKind {
        self._node.kind()
    }

    fn parent(&self) -> Rc<Node> {
        self._node.parent()
    }

    fn set_parent(&self, parent: Rc<Node>) {
        self._node.set_parent(parent)
    }
}

impl ReadonlyTextRange for BaseLiteralLikeNode {
    fn pos(&self) -> usize {
        self._node.pos()
    }

    fn set_pos(&self, pos: usize) {
        self._node.set_pos(pos);
    }

    fn end(&self) -> usize {
        self._node.end()
    }

    fn set_end(&self, end: usize) {
        self._node.set_end(end);
    }
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

    fn parent(&self) -> Rc<Node> {
        match self {
            LiteralLikeNode::NumericLiteral(numeric_literal) => numeric_literal.parent(),
        }
    }

    fn set_parent(&self, parent: Rc<Node>) {
        match self {
            LiteralLikeNode::NumericLiteral(numeric_literal) => numeric_literal.set_parent(parent),
        }
    }
}

impl ReadonlyTextRange for LiteralLikeNode {
    fn pos(&self) -> usize {
        match self {
            LiteralLikeNode::NumericLiteral(numeric_literal) => numeric_literal.pos(),
        }
    }

    fn set_pos(&self, pos: usize) {
        match self {
            LiteralLikeNode::NumericLiteral(numeric_literal) => numeric_literal.set_pos(pos),
        }
    }

    fn end(&self) -> usize {
        match self {
            LiteralLikeNode::NumericLiteral(numeric_literal) => numeric_literal.end(),
        }
    }

    fn set_end(&self, end: usize) {
        match self {
            LiteralLikeNode::NumericLiteral(numeric_literal) => numeric_literal.set_end(end),
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

bitflags! {
    pub struct TokenFlags: u32 {
        const None = 0;
        const PrecedingLineBreak = 1 << 0;
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

    fn parent(&self) -> Rc<Node> {
        self._literal_like_node.parent()
    }

    fn set_parent(&self, parent: Rc<Node>) {
        self._literal_like_node.set_parent(parent)
    }
}

impl ReadonlyTextRange for NumericLiteral {
    fn pos(&self) -> usize {
        self._literal_like_node.pos()
    }

    fn set_pos(&self, pos: usize) {
        self._literal_like_node.set_pos(pos);
    }

    fn end(&self) -> usize {
        self._literal_like_node.end()
    }

    fn set_end(&self, end: usize) {
        self._literal_like_node.set_end(end);
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

    fn parent(&self) -> Rc<Node> {
        match self {
            Statement::EmptyStatement(empty_statement) => empty_statement.parent(),
            Statement::ExpressionStatement(expression_statement) => expression_statement.parent(),
        }
    }

    fn set_parent(&self, parent: Rc<Node>) {
        match self {
            Statement::EmptyStatement(empty_statement) => empty_statement.set_parent(parent),
            Statement::ExpressionStatement(expression_statement) => {
                expression_statement.set_parent(parent)
            }
        }
    }
}

impl ReadonlyTextRange for Statement {
    fn pos(&self) -> usize {
        match self {
            Statement::EmptyStatement(empty_statement) => empty_statement.pos(),
            Statement::ExpressionStatement(expression_statement) => expression_statement.pos(),
        }
    }

    fn set_pos(&self, pos: usize) {
        match self {
            Statement::EmptyStatement(empty_statement) => empty_statement.set_pos(pos),
            Statement::ExpressionStatement(expression_statement) => {
                expression_statement.set_pos(pos)
            }
        }
    }

    fn end(&self) -> usize {
        match self {
            Statement::EmptyStatement(empty_statement) => empty_statement.end(),
            Statement::ExpressionStatement(expression_statement) => expression_statement.end(),
        }
    }

    fn set_end(&self, end: usize) {
        match self {
            Statement::EmptyStatement(empty_statement) => empty_statement.set_end(end),
            Statement::ExpressionStatement(expression_statement) => {
                expression_statement.set_end(end)
            }
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
        self._node.kind()
    }

    fn parent(&self) -> Rc<Node> {
        self._node.parent()
    }

    fn set_parent(&self, parent: Rc<Node>) {
        self._node.set_parent(parent)
    }
}

impl ReadonlyTextRange for EmptyStatement {
    fn pos(&self) -> usize {
        self._node.pos()
    }

    fn set_pos(&self, pos: usize) {
        self._node.set_pos(pos);
    }

    fn end(&self) -> usize {
        self._node.end()
    }

    fn set_end(&self, end: usize) {
        self._node.set_end(end);
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
    pub expression: Rc</*Expression*/ Node>,
}

impl NodeInterface for ExpressionStatement {
    fn kind(&self) -> SyntaxKind {
        self._node.kind()
    }

    fn parent(&self) -> Rc<Node> {
        self._node.parent()
    }

    fn set_parent(&self, parent: Rc<Node>) {
        self._node.set_parent(parent)
    }
}

impl ReadonlyTextRange for ExpressionStatement {
    fn pos(&self) -> usize {
        self._node.pos()
    }

    fn set_pos(&self, pos: usize) {
        self._node.set_pos(pos);
    }

    fn end(&self) -> usize {
        self._node.end()
    }

    fn set_end(&self, end: usize) {
        self._node.set_end(end);
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

impl NodeInterface for SourceFile {
    fn kind(&self) -> SyntaxKind {
        self._node.kind()
    }

    fn parent(&self) -> Rc<Node> {
        self._node.parent() // this would always fail?
    }

    fn set_parent(&self, parent: Rc<Node>) {
        self._node.set_parent(parent)
    }
}

impl ReadonlyTextRange for SourceFile {
    fn pos(&self) -> usize {
        self._node.pos()
    }

    fn set_pos(&self, pos: usize) {
        self._node.set_pos(pos);
    }

    fn end(&self) -> usize {
        self._node.end()
    }

    fn set_end(&self, end: usize) {
        self._node.set_end(end);
    }
}

impl From<Rc<SourceFile>> for Node {
    fn from(source_file: Rc<SourceFile>) -> Self {
        Node::SourceFile(source_file)
    }
}

pub trait Program: TypeCheckerHost {
    fn get_semantic_diagnostics(&mut self) -> Vec<Rc<Diagnostic>>;
}

#[derive(Eq, PartialEq)]
pub enum StructureIsReused {
    Not,
    Completely,
}

#[allow(non_camel_case_types)]
pub enum ExitStatus {
    Success,
    DiagnosticsPresent_OutputsGenerated,
}

pub trait TypeCheckerHost: ModuleSpecifierResolutionHost {
    fn get_source_files(&self) -> Vec<Rc<Node>>;
}

#[allow(non_snake_case)]
pub struct TypeChecker {
    pub Type: fn(TypeFlags) -> BaseType,

    pub number_literal_types: HashMap<Number, Rc</*NumberLiteralType*/ Type>>,
    pub number_type: Option<Rc<Type>>,
    pub bigint_type: Option<Rc<Type>>,
    pub true_type: Option<Rc<Type>>,
    pub regular_true_type: Option<Rc<Type>>,
    pub number_or_big_int_type: Option<Rc<Type>>,
    pub diagnostics: RwLock<DiagnosticCollection>,
    pub assignable_relation: HashMap<String, RelationComparisonResult>,
}

bitflags! {
    pub struct TypeFlags: u32 {
        const Number = 1 << 3;
        const Enum = 1 << 5;
        const BigInt = 1 << 6;
        const StringLiteral = 1 << 7;
        const NumberLiteral = 1 << 8;
        const BooleanLiteral = 1 << 9;
        const BigIntLiteral = 1 << 11;
        const Object = 1 << 19;
        const Union = 1 << 20;
        const Intersection = 1 << 21;

        const Literal = Self::StringLiteral.bits | Self::NumberLiteral.bits | Self::BigIntLiteral.bits | Self::BooleanLiteral.bits;
        const NumberLike = Self::Number.bits | Self::NumberLiteral.bits | Self::Enum.bits;
        const StructuredType = Self::Object.bits | Self::Union.bits | Self::Intersection.bits;
        const StructuredOrInstantiable = Self::StructuredType.bits /*| Self::Instantiable.bits */;
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    IntrinsicType(IntrinsicType),
    LiteralType(LiteralType),
    UnionOrIntersectionType(UnionOrIntersectionType),
}

impl TypeInterface for Type {
    fn flags(&self) -> TypeFlags {
        match self {
            Type::IntrinsicType(intrinsic_type) => intrinsic_type.flags(),
            Type::LiteralType(literal_type) => literal_type.flags(),
            Type::UnionOrIntersectionType(union_or_intersection_type) => {
                union_or_intersection_type.flags()
            }
        }
    }
}

pub trait TypeInterface {
    fn flags(&self) -> TypeFlags;
}

#[derive(Clone, Debug)]
pub struct BaseType {
    pub flags: TypeFlags,
}

impl TypeInterface for BaseType {
    fn flags(&self) -> TypeFlags {
        self.flags
    }
}

pub trait IntrinsicTypeInterface: TypeInterface {}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct FreshableIntrinsicType {
    _intrinsic_type: BaseIntrinsicType,
    pub fresh_type: WeakSelf<Type>,
    pub regular_type: WeakSelf<Type>,
}

impl FreshableIntrinsicType {
    pub fn new(intrinsic_type: BaseIntrinsicType) -> Self {
        Self {
            _intrinsic_type: intrinsic_type,
            fresh_type: WeakSelf::new(),
            regular_type: WeakSelf::new(),
        }
    }

    pub fn fresh_type(&self) -> Weak<Type> {
        self.fresh_type.get()
    }

    pub fn regular_type(&self) -> Weak<Type> {
        self.regular_type.get()
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

pub trait LiteralTypeInterface: TypeInterface {
    fn fresh_type(&self) -> Option<&Weak<Type>>;
    fn set_fresh_type(&self, fresh_type: &Rc<Type>);
    fn get_or_initialize_fresh_type(
        &self,
        type_checker: &TypeChecker,
        wrapper: &Rc<Type>,
    ) -> Rc<Type>;
    fn regular_type(&self) -> Rc<Type>;
    fn set_regular_type(&self, regular_type: &Rc<Type>);
}

#[derive(Clone, Debug)]
pub enum LiteralType {
    NumberLiteralType(NumberLiteralType),
}

impl TypeInterface for LiteralType {
    fn flags(&self) -> TypeFlags {
        match self {
            LiteralType::NumberLiteralType(number_literal_type) => number_literal_type.flags(),
        }
    }
}

impl LiteralTypeInterface for LiteralType {
    fn fresh_type(&self) -> Option<&Weak<Type>> {
        match self {
            LiteralType::NumberLiteralType(number_literal_type) => number_literal_type.fresh_type(),
        }
    }

    fn set_fresh_type(&self, fresh_type: &Rc<Type>) {
        match self {
            LiteralType::NumberLiteralType(number_literal_type) => {
                number_literal_type.set_fresh_type(fresh_type)
            }
        }
    }

    fn get_or_initialize_fresh_type(
        &self,
        type_checker: &TypeChecker,
        wrapper: &Rc<Type>,
    ) -> Rc<Type> {
        match self {
            LiteralType::NumberLiteralType(number_literal_type) => {
                number_literal_type.get_or_initialize_fresh_type(type_checker, wrapper)
            }
        }
    }

    fn regular_type(&self) -> Rc<Type> {
        match self {
            LiteralType::NumberLiteralType(number_literal_type) => {
                number_literal_type.regular_type()
            }
        }
    }

    fn set_regular_type(&self, regular_type: &Rc<Type>) {
        match self {
            LiteralType::NumberLiteralType(number_literal_type) => {
                number_literal_type.set_regular_type(regular_type)
            }
        }
    }
}

impl From<LiteralType> for Type {
    fn from(literal_type: LiteralType) -> Self {
        Type::LiteralType(literal_type)
    }
}

#[derive(Clone, Debug)]
pub struct BaseLiteralType {
    _type: BaseType,
    fresh_type: WeakSelf<Type>,
    regular_type: WeakSelf<Type>,
}

impl BaseLiteralType {
    pub fn new(type_: BaseType) -> Self {
        Self {
            _type: type_,
            fresh_type: WeakSelf::new(),
            regular_type: WeakSelf::new(),
        }
    }
}

impl TypeInterface for BaseLiteralType {
    fn flags(&self) -> TypeFlags {
        self._type.flags()
    }
}

impl LiteralTypeInterface for BaseLiteralType {
    fn fresh_type(&self) -> Option<&Weak<Type>> {
        self.fresh_type.try_get()
    }

    fn set_fresh_type(&self, fresh_type: &Rc<Type>) {
        self.fresh_type.init(fresh_type, false);
    }

    fn get_or_initialize_fresh_type(
        &self,
        type_checker: &TypeChecker,
        wrapper: &Rc<Type>,
    ) -> Rc<Type> {
        panic!("Shouldn't call get_or_initialize_fresh_type() on base BaseLiteralType");
    }

    fn regular_type(&self) -> Rc<Type> {
        self.regular_type.get().upgrade().unwrap()
    }

    fn set_regular_type(&self, regular_type: &Rc<Type>) {
        self.regular_type.init(regular_type, false);
    }
}

#[derive(Clone, Debug)]
pub struct NumberLiteralType {
    _literal_type: BaseLiteralType,
    value: Number,
}

impl NumberLiteralType {
    pub fn new(literal_type: BaseLiteralType, value: Number) -> Self {
        Self {
            _literal_type: literal_type,
            value,
        }
    }

    fn create_fresh_type_from_self(
        &self,
        type_checker: &TypeChecker,
        wrapper: &Rc<Type>,
    ) -> Rc<Type> {
        let fresh_type = type_checker.create_number_literal_type(
            self.flags(),
            self.value,
            Some(wrapper.clone()),
        );
        match &*fresh_type {
            Type::LiteralType(literal_type) => {
                literal_type.set_fresh_type(&fresh_type);
            }
            _ => panic!("Expected LiteralType"),
        }
        self.set_fresh_type(&fresh_type);
        self.fresh_type().unwrap().upgrade().unwrap()
    }
}

impl TypeInterface for NumberLiteralType {
    fn flags(&self) -> TypeFlags {
        self._literal_type.flags()
    }
}

impl LiteralTypeInterface for NumberLiteralType {
    fn fresh_type(&self) -> Option<&Weak<Type>> {
        self._literal_type.fresh_type()
    }

    fn set_fresh_type(&self, fresh_type: &Rc<Type>) {
        self._literal_type.set_fresh_type(fresh_type);
    }

    fn get_or_initialize_fresh_type(
        &self,
        type_checker: &TypeChecker,
        wrapper: &Rc<Type>,
    ) -> Rc<Type> {
        if self.fresh_type().is_none() {
            let fresh_type = self.create_fresh_type_from_self(type_checker, wrapper);
            self.set_fresh_type(&fresh_type);
            return self.fresh_type().unwrap().upgrade().unwrap();
        }
        return self.fresh_type().unwrap().upgrade().unwrap();
    }

    fn regular_type(&self) -> Rc<Type> {
        self._literal_type.regular_type()
    }

    fn set_regular_type(&self, regular_type: &Rc<Type>) {
        self._literal_type.set_regular_type(regular_type);
    }
}

impl From<NumberLiteralType> for LiteralType {
    fn from(number_literal_type: NumberLiteralType) -> Self {
        LiteralType::NumberLiteralType(number_literal_type)
    }
}

impl From<NumberLiteralType> for Type {
    fn from(number_literal_type: NumberLiteralType) -> Self {
        Type::LiteralType(LiteralType::NumberLiteralType(number_literal_type))
    }
}

pub trait UnionOrIntersectionTypeInterface: TypeInterface {
    fn types(&self) -> &[Rc<Type>];
}

#[derive(Clone, Debug)]
pub enum UnionOrIntersectionType {
    UnionType(UnionType),
}

impl TypeInterface for UnionOrIntersectionType {
    fn flags(&self) -> TypeFlags {
        match self {
            UnionOrIntersectionType::UnionType(union_type) => union_type.flags(),
        }
    }
}

impl UnionOrIntersectionTypeInterface for UnionOrIntersectionType {
    fn types(&self) -> &[Rc<Type>] {
        match self {
            UnionOrIntersectionType::UnionType(union_type) => union_type.types(),
        }
    }
}

impl From<UnionOrIntersectionType> for Type {
    fn from(union_or_intersection_type: UnionOrIntersectionType) -> Self {
        Type::UnionOrIntersectionType(union_or_intersection_type)
    }
}

#[derive(Clone, Debug)]
pub struct BaseUnionOrIntersectionType {
    pub _type: BaseType,
    pub types: Vec<Rc<Type>>,
}

impl TypeInterface for BaseUnionOrIntersectionType {
    fn flags(&self) -> TypeFlags {
        self._type.flags()
    }
}

impl UnionOrIntersectionTypeInterface for BaseUnionOrIntersectionType {
    fn types(&self) -> &[Rc<Type>] {
        &self.types
    }
}

#[derive(Clone, Debug)]
pub struct UnionType {
    pub _union_or_intersection_type: BaseUnionOrIntersectionType,
}

impl TypeInterface for UnionType {
    fn flags(&self) -> TypeFlags {
        self._union_or_intersection_type.flags()
    }
}

impl UnionOrIntersectionTypeInterface for UnionType {
    fn types(&self) -> &[Rc<Type>] {
        &self._union_or_intersection_type.types
    }
}

impl From<UnionType> for UnionOrIntersectionType {
    fn from(union_type: UnionType) -> Self {
        UnionOrIntersectionType::UnionType(union_type)
    }
}

impl From<UnionType> for Type {
    fn from(union_type: UnionType) -> Self {
        Type::UnionOrIntersectionType(UnionOrIntersectionType::UnionType(union_type))
    }
}

#[derive(Eq, PartialEq)]
pub enum Ternary {
    False = 0,
    Unknown = 1,
    Maybe = 3,
    True = -1,
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

    pub const lineFeed: char = '\n';

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

#[derive(Debug)]
pub enum Diagnostic {
    DiagnosticWithLocation(DiagnosticWithLocation),
    DiagnosticWithDetachedLocation(DiagnosticWithDetachedLocation),
}

pub trait DiagnosticInterface: DiagnosticRelatedInformationInterface {}

#[derive(Clone, Debug)]
pub struct BaseDiagnostic {
    _diagnostic_related_information: BaseDiagnosticRelatedInformation,
}

impl BaseDiagnostic {
    pub fn new(diagnostic_related_information: BaseDiagnosticRelatedInformation) -> Self {
        Self {
            _diagnostic_related_information: diagnostic_related_information,
        }
    }
}

impl DiagnosticRelatedInformationInterface for BaseDiagnostic {
    fn file(&self) -> Option<Rc<SourceFile>> {
        self._diagnostic_related_information.file()
    }

    fn start(&self) -> usize {
        self._diagnostic_related_information.start()
    }

    fn length(&self) -> usize {
        self._diagnostic_related_information.length()
    }
}

impl DiagnosticRelatedInformationInterface for Diagnostic {
    fn file(&self) -> Option<Rc<SourceFile>> {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic_with_location) => {
                diagnostic_with_location.file()
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location) => {
                diagnostic_with_detached_location.file()
            }
        }
    }

    fn start(&self) -> usize {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic_with_location) => {
                diagnostic_with_location.start()
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location) => {
                diagnostic_with_detached_location.start()
            }
        }
    }

    fn length(&self) -> usize {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic_with_location) => {
                diagnostic_with_location.length()
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location) => {
                diagnostic_with_detached_location.length()
            }
        }
    }
}

impl DiagnosticInterface for Diagnostic {}

pub trait DiagnosticRelatedInformationInterface {
    fn file(&self) -> Option<Rc<SourceFile>>;
    fn start(&self) -> usize;
    fn length(&self) -> usize;
}

#[derive(Clone, Debug)]
pub struct BaseDiagnosticRelatedInformation {
    pub file: Option<Rc<SourceFile>>,
    pub start: usize,
    pub length: usize,
}

impl DiagnosticRelatedInformationInterface for BaseDiagnosticRelatedInformation {
    fn file(&self) -> Option<Rc<SourceFile>> {
        self.file.clone()
    }

    fn start(&self) -> usize {
        self.start
    }

    fn length(&self) -> usize {
        self.length
    }
}

#[derive(Clone, Debug)]
pub struct DiagnosticWithLocation {
    pub _diagnostic: BaseDiagnostic,
}

impl DiagnosticWithLocation {
    pub fn file_unwrapped(&self) -> Rc<SourceFile> {
        self.file().unwrap()
    }
}

impl DiagnosticRelatedInformationInterface for DiagnosticWithLocation {
    fn file(&self) -> Option<Rc<SourceFile>> {
        self._diagnostic.file()
    }

    fn start(&self) -> usize {
        self._diagnostic.start()
    }

    fn length(&self) -> usize {
        self._diagnostic.length()
    }
}

impl DiagnosticInterface for DiagnosticWithLocation {}

impl From<DiagnosticWithLocation> for Diagnostic {
    fn from(diagnostic_with_location: DiagnosticWithLocation) -> Self {
        Diagnostic::DiagnosticWithLocation(diagnostic_with_location)
    }
}

#[derive(Debug)]
pub struct DiagnosticWithDetachedLocation {
    pub _diagnostic: BaseDiagnostic,
    pub file_name: String,
}

impl DiagnosticRelatedInformationInterface for DiagnosticWithDetachedLocation {
    fn file(&self) -> Option<Rc<SourceFile>> {
        self._diagnostic.file()
    }

    fn start(&self) -> usize {
        self._diagnostic.start()
    }

    fn length(&self) -> usize {
        self._diagnostic.length()
    }
}

impl DiagnosticInterface for DiagnosticWithDetachedLocation {}

impl From<DiagnosticWithDetachedLocation> for Diagnostic {
    fn from(diagnostic_with_detached_location: DiagnosticWithDetachedLocation) -> Self {
        Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location)
    }
}

pub enum DiagnosticCategory {
    Warning,
    Error,
    Suggestion,
    Message,
}

pub struct NodeFactory {}

pub trait ModuleSpecifierResolutionHost {}

pub struct TextSpan {
    pub start: usize,
    pub length: usize,
}

pub struct DiagnosticCollection {
    pub file_diagnostics: HashMap<String, SortedArray<Rc<Diagnostic>>>,
}
