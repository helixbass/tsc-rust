#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::ops::BitAndAssign;
use std::rc::{Rc, Weak};

use crate::{NodeBuilder, Number, SortedArray, WeakSelf};
use local_macros::ast_type;

pub struct Path(String);

impl Path {
    pub fn new(string: String) -> Self {
        Self(string)
    }
}

pub trait ReadonlyTextRange {
    fn pos(&self) -> isize;
    fn set_pos(&self, pos: isize);
    fn end(&self) -> isize;
    fn set_end(&self, end: isize);
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum SyntaxKind {
    Unknown,
    EndOfFileToken,
    NumericLiteral,
    StringLiteral,
    NoSubstitutionTemplateLiteral,
    OpenBraceToken,
    CloseBraceToken,
    OpenParenToken,
    OpenBracketToken,
    CloseBracketToken,
    DotToken,
    DotDotDotToken,
    SemicolonToken,
    CommaToken,
    LessThanToken,
    AsteriskToken,
    PlusPlusToken,
    ExclamationToken,
    QuestionToken,
    ColonToken,
    EqualsToken,
    Identifier,
    PrivateIdentifier,
    BreakKeyword,
    ConstKeyword,
    FalseKeyword,
    TrueKeyword,
    WithKeyword,
    InterfaceKeyword,
    BooleanKeyword,
    NumberKeyword,
    OfKeyword,
    QualifiedName,
    PropertySignature,
    PropertyDeclaration,
    TypeReference,
    ArrayType,
    LiteralType,
    ObjectBindingPattern,
    ArrayBindingPattern,
    BindingElement,
    ArrayLiteralExpression,
    ObjectLiteralExpression,
    PropertyAccessExpression,
    PrefixUnaryExpression,
    BinaryExpression,
    ExpressionWithTypeArguments,
    EmptyStatement,
    VariableStatement,
    ExpressionStatement,
    VariableDeclaration,
    VariableDeclarationList,
    FunctionDeclaration,
    InterfaceDeclaration,
    PropertyAssignment,
    SourceFile,
}

impl SyntaxKind {
    pub const LastReservedWord: SyntaxKind = SyntaxKind::WithKeyword;
    pub const FirstKeyword: SyntaxKind = SyntaxKind::BreakKeyword;
    pub const LastKeyword: SyntaxKind = SyntaxKind::OfKeyword;
    pub const LastToken: SyntaxKind = SyntaxKind::LastKeyword;
}

bitflags! {
    pub struct NodeFlags: u32 {
        const None = 0;
        const Const = 1 << 1;
        const DisallowInContext = 1 << 12;
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
    fn node_wrapper(&self) -> Rc<Node>;
    fn set_node_wrapper(&self, wrapper: Rc<Node>);
    fn kind(&self) -> SyntaxKind;
    fn maybe_parent(&self) -> Option<Rc<Node>>;
    fn parent(&self) -> Rc<Node>;
    fn set_parent(&self, parent: Rc<Node>);
    fn maybe_symbol(&self) -> Option<Rc<Symbol>>;
    fn symbol(&self) -> Rc<Symbol>;
    fn set_symbol(&self, symbol: Rc<Symbol>);
    fn maybe_locals(&self) -> RefMut<Option<SymbolTable>>;
    fn locals(&self) -> RefMut<SymbolTable>;
    fn set_locals(&self, locals: SymbolTable);
}

#[derive(Debug)]
#[ast_type(impl_from = false)]
pub enum Node {
    BaseNode(BaseNode),
    VariableDeclaration(VariableDeclaration),
    VariableDeclarationList(VariableDeclarationList),
    TypeNode(TypeNode),
    Expression(Expression),
    Statement(Statement),
    TypeElement(TypeElement),
    PropertyAssignment(PropertyAssignment),
    SourceFile(Rc<SourceFile>),
}

impl Node {
    pub fn wrap(self) -> Rc<Node> {
        let rc = Rc::new(self);
        rc.set_node_wrapper(rc.clone());
        rc
    }

    pub fn as_named_declaration(&self) -> &dyn NamedDeclarationInterface {
        match self {
            Node::VariableDeclaration(variable_declaration) => variable_declaration,
            Node::Statement(Statement::InterfaceDeclaration(interface_declaration)) => {
                interface_declaration
            }
            Node::TypeElement(type_element) => type_element,
            Node::PropertyAssignment(property_assignment) => property_assignment,
            _ => panic!("Expected named declaration"),
        }
    }

    pub fn as_member_name(&self) -> &dyn MemberNameInterface {
        match self {
            Node::Expression(expression) => match expression {
                Expression::Identifier(identifier) => identifier,
                _ => panic!("Expected member name"),
            },
            _ => panic!("Expected member name"),
        }
    }

    pub fn as_literal_like_node(&self) -> &dyn LiteralLikeNodeInterface {
        match self {
            Node::Expression(expression) => match expression {
                Expression::LiteralLikeNode(literal_like_node) => literal_like_node,
                _ => panic!("Expected literal like node"),
            },
            _ => panic!("Expected literal like node"),
        }
    }

    pub fn maybe_as_has_type(&self) -> Option<&dyn HasTypeInterface> {
        match self {
            Node::VariableDeclaration(variable_declaration) => Some(variable_declaration),
            Node::TypeElement(TypeElement::PropertySignature(property_signature)) => {
                Some(property_signature)
            }
            _ => None,
        }
    }

    pub fn as_has_expression_initializer(&self) -> &dyn HasExpressionInitializerInterface {
        match self {
            Node::VariableDeclaration(variable_declaration) => variable_declaration,
            Node::TypeElement(TypeElement::PropertySignature(property_signature)) => {
                property_signature
            }
            _ => panic!("Expected has expression initializer"),
        }
    }

    pub fn maybe_as_has_expression_initializer(
        &self,
    ) -> Option<&dyn HasExpressionInitializerInterface> {
        match self {
            Node::VariableDeclaration(variable_declaration) => Some(variable_declaration),
            Node::TypeElement(TypeElement::PropertySignature(property_signature)) => {
                Some(property_signature)
            }
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct BaseNode {
    _node_wrapper: RefCell<Option<Weak<Node>>>,
    pub kind: SyntaxKind,
    pub parent: RefCell<Option<Weak<Node>>>,
    pub pos: Cell<isize>,
    pub end: Cell<isize>,
    pub symbol: RefCell<Option<Weak<Symbol>>>,
    pub locals: RefCell<Option<SymbolTable>>,
}

impl BaseNode {
    pub fn new(kind: SyntaxKind, pos: isize, end: isize) -> Self {
        Self {
            _node_wrapper: RefCell::new(None),
            kind,
            parent: RefCell::new(None),
            pos: Cell::new(pos),
            end: Cell::new(end),
            symbol: RefCell::new(None),
            locals: RefCell::new(None),
        }
    }
}

impl NodeInterface for BaseNode {
    fn node_wrapper(&self) -> Rc<Node> {
        self._node_wrapper
            .borrow()
            .as_ref()
            .unwrap()
            .upgrade()
            .unwrap()
    }

    fn set_node_wrapper(&self, wrapper: Rc<Node>) {
        *self._node_wrapper.borrow_mut() = Some(Rc::downgrade(&wrapper));
    }

    fn kind(&self) -> SyntaxKind {
        self.kind
    }

    fn maybe_parent(&self) -> Option<Rc<Node>> {
        self.parent
            .borrow()
            .as_ref()
            .map(|weak| weak.upgrade().unwrap())
    }

    fn parent(&self) -> Rc<Node> {
        self.parent.borrow().clone().unwrap().upgrade().unwrap()
    }

    fn set_parent(&self, parent: Rc<Node>) {
        *self.parent.borrow_mut() = Some(Rc::downgrade(&parent));
    }

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        self.symbol
            .borrow()
            .as_ref()
            .map(|weak| weak.upgrade().unwrap())
    }

    fn symbol(&self) -> Rc<Symbol> {
        self.symbol.borrow().as_ref().unwrap().upgrade().unwrap()
    }

    fn set_symbol(&self, symbol: Rc<Symbol>) {
        *self.symbol.borrow_mut() = Some(Rc::downgrade(&symbol));
    }

    fn maybe_locals(&self) -> RefMut<Option<SymbolTable>> {
        self.locals.borrow_mut()
    }

    fn locals(&self) -> RefMut<SymbolTable> {
        RefMut::map(self.locals.borrow_mut(), |option| option.as_mut().unwrap())
    }

    fn set_locals(&self, locals: SymbolTable) {
        *self.locals.borrow_mut() = Some(locals);
    }
}

impl ReadonlyTextRange for BaseNode {
    fn pos(&self) -> isize {
        self.pos.get()
    }

    fn set_pos(&self, pos: isize) {
        self.pos.set(pos);
    }

    fn end(&self) -> isize {
        self.end.get()
    }

    fn set_end(&self, end: isize) {
        self.end.set(end);
    }
}

impl From<BaseNode> for Node {
    fn from(base_node: BaseNode) -> Self {
        Node::BaseNode(base_node)
    }
}

impl From<BaseNode> for Rc<Node> {
    fn from(base_node: BaseNode) -> Self {
        let rc = Rc::new(Node::BaseNode(base_node));
        rc.set_node_wrapper(rc.clone());
        rc
    }
}

pub trait HasTypeInterface {
    fn type_(&self) -> Option<Rc<Node>>;
    fn set_type(&mut self, type_: Rc<Node>);
}

pub trait HasExpressionInitializerInterface {
    fn initializer(&self) -> Option<Rc<Node>>;
    fn set_initializer(&mut self, initializer: Rc<Node>);
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
#[ast_type(ancestors = "Expression")]
pub struct Identifier {
    _node: BaseNode,
    pub escaped_text: __String,
}

impl Identifier {
    pub fn new(base_node: BaseNode, escaped_text: __String) -> Self {
        Self {
            _node: base_node,
            escaped_text,
        }
    }
}

impl MemberNameInterface for Identifier {
    fn escaped_text(&self) -> __String {
        self.escaped_text.clone()
    }
}

pub trait MemberNameInterface: NodeInterface {
    fn escaped_text(&self) -> __String;
}

pub trait NamedDeclarationInterface: NodeInterface {
    fn name(&self) -> Rc<Node>;
    fn set_name(&mut self, name: Rc<Node>);
}

#[derive(Debug)]
#[ast_type(impl_from = false)]
pub struct BaseNamedDeclaration {
    _node: BaseNode,
    name: Option<Rc<Node>>,
}

impl BaseNamedDeclaration {
    pub fn new(base_node: BaseNode, name: Option<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            name,
        }
    }
}

impl NamedDeclarationInterface for BaseNamedDeclaration {
    fn name(&self) -> Rc<Node> {
        self.name.as_ref().unwrap().clone()
    }

    fn set_name(&mut self, name: Rc<Node>) {
        self.name = Some(name);
    }
}

pub trait BindingLikeDeclarationInterface:
    NamedDeclarationInterface + HasExpressionInitializerInterface
{
}

#[derive(Debug)]
#[ast_type(impl_from = false, interfaces = "NamedDeclarationInterface")]
pub struct BaseBindingLikeDeclaration {
    _named_declaration: BaseNamedDeclaration,
    initializer: Option<Rc<Node>>,
}

impl BaseBindingLikeDeclaration {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        initializer: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            initializer,
        }
    }
}

impl HasExpressionInitializerInterface for BaseBindingLikeDeclaration {
    fn initializer(&self) -> Option<Rc<Node>> {
        self.initializer.as_ref().map(Clone::clone)
    }

    fn set_initializer(&mut self, initializer: Rc<Node>) {
        self.initializer = Some(initializer);
    }
}

impl BindingLikeDeclarationInterface for BaseBindingLikeDeclaration {}

pub trait VariableLikeDeclarationInterface:
    BindingLikeDeclarationInterface + HasTypeInterface
{
}

#[derive(Debug)]
#[ast_type(
    impl_from = false,
    interfaces = "NamedDeclarationInterface, HasExpressionInitializerInterface, BindingLikeDeclarationInterface"
)]
pub struct BaseVariableLikeDeclaration {
    _binding_like_declaration: BaseBindingLikeDeclaration,
    type_: Option<Rc<Node>>,
}

impl BaseVariableLikeDeclaration {
    pub fn new(
        base_binding_like_declaration: BaseBindingLikeDeclaration,
        type_: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _binding_like_declaration: base_binding_like_declaration,
            type_,
        }
    }
}

impl HasTypeInterface for BaseVariableLikeDeclaration {
    fn type_(&self) -> Option<Rc<Node>> {
        self.type_.as_ref().map(Clone::clone)
    }

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = Some(type_);
    }
}

impl VariableLikeDeclarationInterface for BaseVariableLikeDeclaration {}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasExpressionInitializerInterface, BindingLikeDeclarationInterface, HasTypeInterface, VariableLikeDeclarationInterface"
)]
pub struct VariableDeclaration {
    _variable_like_declaration: BaseVariableLikeDeclaration,
}

impl VariableDeclaration {
    pub fn new(base_variable_like_declaration: BaseVariableLikeDeclaration) -> Self {
        Self {
            _variable_like_declaration: base_variable_like_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct VariableDeclarationList {
    _node: BaseNode,
    pub declarations: NodeArray, /*<VariableDeclaration>*/
}

impl VariableDeclarationList {
    pub fn new(base_node: BaseNode, declarations: NodeArray) -> Self {
        Self {
            _node: base_node,
            declarations,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub enum TypeNode {
    KeywordTypeNode(KeywordTypeNode),
    LiteralTypeNode(LiteralTypeNode),
    TypeReferenceNode(TypeReferenceNode),
    ArrayTypeNode(ArrayTypeNode),
}

#[derive(Debug)]
#[ast_type(ancestors = "TypeNode")]
pub struct KeywordTypeNode {
    _node: BaseNode,
}

impl KeywordTypeNode {
    pub fn new(base_node: BaseNode) -> Self {
        Self { _node: base_node }
    }
}

impl From<BaseNode> for KeywordTypeNode {
    fn from(base_node: BaseNode) -> Self {
        KeywordTypeNode::new(base_node)
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "TypeNode")]
pub struct TypeReferenceNode {
    _node: BaseNode,
    pub type_name: Rc<Node /*EntityName*/>,
}

impl TypeReferenceNode {
    pub fn new(base_node: BaseNode, type_name: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            type_name,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "TypeNode")]
pub struct ArrayTypeNode {
    _node: BaseNode,
    pub element_type: Rc<Node /*TypeNode*/>,
}

impl ArrayTypeNode {
    pub fn new(base_node: BaseNode, element_type: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            element_type,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "TypeNode")]
pub struct LiteralTypeNode {
    _node: BaseNode,
    pub literal: Rc<Node>, // TODO: should be weak?
}

impl LiteralTypeNode {
    pub fn new(base_node: BaseNode, literal: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            literal,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub enum Expression {
    TokenExpression(BaseNode),
    Identifier(Identifier),
    PrefixUnaryExpression(PrefixUnaryExpression),
    BinaryExpression(BinaryExpression),
    LiteralLikeNode(LiteralLikeNode),
    ArrayLiteralExpression(ArrayLiteralExpression),
    ObjectLiteralExpression(ObjectLiteralExpression),
}

impl From<BaseNode> for Expression {
    fn from(base_node: BaseNode) -> Self {
        Expression::TokenExpression(base_node)
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Expression")]
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
            operand: operand.into(),
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Expression")]
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
            left: left.into(),
            operator_token: Box::new(operator_token),
            right: right.into(),
        }
    }
}

#[derive(Debug)]
#[ast_type(impl_from = false)]
pub struct BaseLiteralLikeNode {
    pub _node: BaseNode,
    pub text: String,
}

impl LiteralLikeNodeInterface for BaseLiteralLikeNode {
    fn text(&self) -> &str {
        &self.text
    }
}

pub trait LiteralLikeNodeInterface {
    fn text(&self) -> &str;
}

#[derive(Debug)]
#[ast_type(ancestors = "Expression", interfaces = "LiteralLikeNodeInterface")]
pub enum LiteralLikeNode {
    NumericLiteral(NumericLiteral),
}

bitflags! {
    pub struct TokenFlags: u32 {
        const None = 0;
        const PrecedingLineBreak = 1 << 0;
    }
}

#[derive(Debug)]
#[ast_type(
    ancestors = "LiteralLikeNode, Expression",
    interfaces = "LiteralLikeNodeInterface"
)]
pub struct NumericLiteral {
    pub _literal_like_node: BaseLiteralLikeNode,
}

#[derive(Debug)]
#[ast_type(ancestors = "Expression")]
pub struct ArrayLiteralExpression {
    _node: BaseNode,
    pub elements: NodeArray, /*<Expression>*/
}

impl ArrayLiteralExpression {
    pub fn new(base_node: BaseNode, elements: NodeArray) -> Self {
        Self {
            _node: base_node,
            elements,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Expression")]
pub struct ObjectLiteralExpression {
    _node: BaseNode,
    pub properties: NodeArray, /*<ObjectLiteralElementLike>*/
}

impl ObjectLiteralExpression {
    pub fn new(base_node: BaseNode, properties: NodeArray) -> Self {
        Self {
            _node: base_node,
            properties,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub enum Statement {
    EmptyStatement(EmptyStatement),
    VariableStatement(VariableStatement),
    ExpressionStatement(ExpressionStatement),
    InterfaceDeclaration(InterfaceDeclaration),
}

#[derive(Debug)]
#[ast_type(ancestors = "Statement")]
pub struct EmptyStatement {
    pub _node: BaseNode,
}

#[derive(Debug)]
#[ast_type(ancestors = "Statement")]
pub struct VariableStatement {
    _node: BaseNode,
    pub declaration_list: Rc</*VariableDeclarationList*/ Node>,
}

impl VariableStatement {
    pub fn new(base_node: BaseNode, declaration_list: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            declaration_list,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Statement")]
pub struct ExpressionStatement {
    pub _node: BaseNode,
    pub expression: Rc</*Expression*/ Node>,
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub enum TypeElement {
    PropertySignature(PropertySignature),
}

#[derive(Debug)]
#[ast_type(ancestors = "TypeElement", interfaces = "NamedDeclarationInterface")]
pub struct PropertySignature {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub type_: Option<Rc<Node /*TypeNode*/>>,
}

impl PropertySignature {
    pub fn new(base_named_declaration: BaseNamedDeclaration, type_: Option<Rc<Node>>) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            type_,
        }
    }
}

impl HasTypeInterface for PropertySignature {
    fn type_(&self) -> Option<Rc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = Some(type_);
    }
}

impl HasExpressionInitializerInterface for PropertySignature {
    fn initializer(&self) -> Option<Rc<Node>> {
        None
    }

    fn set_initializer(&mut self, _initializer: Rc<Node>) {
        panic!("Shouldn't call set_initializer() on PropertySignature")
    }
}

impl BindingLikeDeclarationInterface for PropertySignature {}

impl VariableLikeDeclarationInterface for PropertySignature {}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct PropertyAssignment {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub initializer: Rc<Node /*Expression*/>,
}

// TODO: should implement HasExpressionInitializerInterface for PropertyAssignment? Its initializer
// isn't optional - should maybe change HasExpressionInitializerInterface initializer() ->
// maybe_initializer() and add non-optional initializer()?

impl PropertyAssignment {
    pub fn new(base_named_declaration: BaseNamedDeclaration, initializer: Rc<Node>) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            initializer,
        }
    }
}

#[derive(Debug)]
#[ast_type(impl_from = false, interfaces = "NamedDeclarationInterface")]
pub struct BaseGenericNamedDeclaration {
    _named_declaration: BaseNamedDeclaration,
}

impl BaseGenericNamedDeclaration {
    pub fn new(base_named_declaration: BaseNamedDeclaration) -> Self {
        Self {
            _named_declaration: base_named_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type(impl_from = false, interfaces = "NamedDeclarationInterface")]
pub struct BaseInterfaceOrClassLikeDeclaration {
    _generic_named_declaration: BaseGenericNamedDeclaration,
}

impl BaseInterfaceOrClassLikeDeclaration {
    pub fn new(base_generic_named_declaration: BaseGenericNamedDeclaration) -> Self {
        Self {
            _generic_named_declaration: base_generic_named_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Statement", interfaces = "NamedDeclarationInterface")]
pub struct InterfaceDeclaration {
    _interface_or_class_like_declaration: BaseInterfaceOrClassLikeDeclaration, /*name: Identifier*/
    pub members: NodeArray,                                                    /*<TypeElement>*/
}

impl InterfaceDeclaration {
    pub fn new(
        base_interface_or_class_like_declaration: BaseInterfaceOrClassLikeDeclaration,
        members: NodeArray,
    ) -> Self {
        Self {
            _interface_or_class_like_declaration: base_interface_or_class_like_declaration,
            members,
        }
    }
}

#[derive(Debug)]
#[ast_type(impl_from = false)]
pub struct SourceFile {
    _node: BaseNode,
    _symbols_without_a_symbol_table_strong_references: RefCell<Vec<Rc<Symbol>>>,
    pub statements: NodeArray,

    pub file_name: String,
    pub text: String,
}

impl SourceFile {
    pub fn new(
        base_node: BaseNode,
        statements: NodeArray,
        file_name: String,
        text: String,
    ) -> Self {
        Self {
            _node: base_node,
            _symbols_without_a_symbol_table_strong_references: RefCell::new(vec![]),
            statements,
            file_name,
            text,
        }
    }

    pub fn keep_strong_reference_to_symbol(&self, symbol: Rc<Symbol>) {
        self._symbols_without_a_symbol_table_strong_references
            .borrow_mut()
            .push(symbol);
    }
}

impl From<SourceFile> for Rc<Node> {
    fn from(source_file: SourceFile) -> Self {
        let rc = Rc::new(Node::SourceFile(Rc::new(source_file)));
        rc.set_node_wrapper(rc.clone());
        rc
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
    pub _types_needing_strong_references: RefCell<Vec<Rc<Type>>>,
    pub Symbol: fn(SymbolFlags, __String) -> Symbol,
    pub Type: fn(TypeFlags) -> BaseType,
    pub strict_null_checks: bool,
    pub fresh_object_literal_flag: ObjectFlags,
    pub exact_optional_property_types: bool,
    pub node_builder: NodeBuilder,
    pub globals: RefCell<SymbolTable>,
    pub string_literal_types: RefCell<HashMap<String, Rc</*StringLiteralType*/ Type>>>,
    pub number_literal_types: RefCell<HashMap<Number, Rc</*NumberLiteralType*/ Type>>>,
    pub unknown_symbol: Option<Rc<Symbol>>,
    pub number_type: Option<Rc<Type>>,
    pub bigint_type: Option<Rc<Type>>,
    pub true_type: Option<Rc<Type>>,
    pub regular_true_type: Option<Rc<Type>>,
    pub false_type: Option<Rc<Type>>,
    pub regular_false_type: Option<Rc<Type>>,
    pub boolean_type: Option<Rc<Type>>,
    pub number_or_big_int_type: Option<Rc<Type>>,
    pub global_array_type: Option<Rc<Type /*GenericType*/>>,
    pub diagnostics: RefCell<DiagnosticCollection>,
    pub assignable_relation: HashMap<String, RelationComparisonResult>,
}

bitflags! {
    pub struct SymbolFormatFlags: u32 {
        const None = 0;
        const WriteTypeParametersOrArguments = 1 << 0;
        const UseOnlyExternalAliasing = 1 << 1;
        const AllowAnyNodeKind = 1 << 2;
        const UseAliasDefinedOutsideCurrentScope = 1 << 3;
        const DoNotIncludeSymbolChain = 1 << 4;
    }
}

#[derive(PartialEq, Eq)]
pub enum UnionReduction {
    None,
    Literal,
    Subtype,
}

pub trait SymbolWriter: SymbolTracker {
    fn write_keyword(&mut self, text: &str);
    fn write_symbol(&mut self, text: &str, symbol: &Symbol);
    fn clear(&mut self);
}

bitflags! {
    pub struct SymbolFlags: u32 {
        const None = 0;
        const FunctionScopedVariable = 1 << 0;
        const BlockScopedVariable = 1 << 1;
        const Property = 1 << 2;
        const EnumMember = 1 << 3;
        const Function = 1 << 4;
        const Class = 1 << 5;
        const Interface = 1 << 6;
        const ConstEnum = 1 << 7;
        const RegularEnum = 1 << 8;
        const ValueModule = 1 << 9;
        const TypeLiteral = 1 << 11;
        const ObjectLiteral = 1 << 12;
        const Method = 1 << 13;
        const GetAccessor = 1 << 15;
        const SetAccessor = 1 << 16;
        const TypeParameter = 1 << 18;
        const TypeAlias = 1 << 19;
        const Optional = 1 << 24;
        const Transient = 1 << 25;

        const Enum = Self::RegularEnum.bits | Self::ConstEnum.bits;
        const Variable = Self::FunctionScopedVariable.bits | Self::BlockScopedVariable.bits;
        const Value = Self::Variable.bits | Self::Property.bits | Self::EnumMember.bits | Self::ObjectLiteral.bits | Self::Function.bits | Self::Class.bits | Self::Enum.bits | Self::ValueModule.bits | Self::Method.bits | Self::GetAccessor.bits | Self::SetAccessor.bits;
        const Type = Self::Class.bits | Self::Interface.bits | Self::Enum.bits | Self::EnumMember.bits | Self::TypeLiteral.bits | Self::TypeParameter.bits | Self::TypeAlias.bits;

        const FunctionScopedVariableExcludes = Self::Value.bits & !Self::FunctionScopedVariable.bits;

        const PropertyExcludes = Self::None.bits;
        const InterfaceExcludes = Self::Type.bits & !(Self::Interface.bits | Self::Class.bits);
    }
}

#[derive(Debug)]
pub struct Symbol {
    pub flags: Cell<SymbolFlags>,
    pub escaped_name: __String,
    declarations: RefCell<Option<Vec<Rc<Node /*Declaration*/>>>>, // TODO: should be Vec<Weak<Node>> instead of Vec<Rc<Node>>?
    value_declaration: RefCell<Option<Weak<Node>>>,
    members: RefCell<Option<Rc<RefCell<SymbolTable>>>>,
}

impl Symbol {
    pub fn new(flags: SymbolFlags, name: __String) -> Self {
        Self {
            flags: Cell::new(flags),
            escaped_name: name,
            declarations: RefCell::new(None),
            value_declaration: RefCell::new(None),
            members: RefCell::new(None),
        }
    }

    pub fn flags(&self) -> SymbolFlags {
        self.flags.get()
    }

    pub fn set_flags(&self, flags: SymbolFlags) {
        self.flags.set(flags);
    }

    pub fn maybe_declarations(&self) -> Ref<Option<Vec<Rc<Node>>>> {
        self.declarations.borrow()
    }

    pub fn set_declarations(&self, declarations: Vec<Rc<Node>>) {
        *self.declarations.borrow_mut() = Some(declarations);
    }

    pub fn maybe_value_declaration(&self) -> Ref<Option<Weak<Node>>> {
        self.value_declaration.borrow()
    }

    pub fn set_value_declaration<TNode: NodeInterface>(&self, node: &TNode) {
        *self.value_declaration.borrow_mut() = Some(Rc::downgrade(&node.node_wrapper()));
    }

    pub fn maybe_members(&self) -> RefMut<Option<Rc<RefCell<SymbolTable>>>> {
        self.members.borrow_mut()
    }

    pub fn members(&self) -> Rc<RefCell<SymbolTable>> {
        self.members.borrow_mut().as_ref().unwrap().clone()
    }
}

pub struct InternalSymbolName;

#[allow(non_snake_case)]
impl InternalSymbolName {
    pub fn Object() -> __String {
        __String::new("__object".to_string())
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct __String(String);

impl __String {
    pub fn new(string: String) -> Self {
        Self(string)
    }

    pub fn chars(&self) -> std::str::Chars {
        self.0.chars()
    }
}

pub type UnderscoreEscapedMap<TValue> = HashMap<__String, TValue>;

pub type SymbolTable = UnderscoreEscapedMap<Rc<Symbol>>;

bitflags! {
    pub struct TypeFlags: u32 {
        const String = 1 << 2;
        const Number = 1 << 3;
        const Boolean = 1 << 4;
        const Enum = 1 << 5;
        const BigInt = 1 << 6;
        const StringLiteral = 1 << 7;
        const NumberLiteral = 1 << 8;
        const BooleanLiteral = 1 << 9;
        const EnumLiteral = 1 << 10;
        const BigIntLiteral = 1 << 11;
        const ESSymbol = 1 << 12;
        const UniqueESSymbol = 1 << 13;
        const Void = 1 << 14;
        const Undefined = 1 << 15;
        const Null = 1 << 16;
        const Never = 1 << 17;
        const TypeParameter = 1 << 18;
        const Object = 1 << 19;
        const Union = 1 << 20;
        const Intersection = 1 << 21;
        const Index = 1 << 22;
        const IndexedAccess = 1 << 23;
        const Conditional = 1 << 24;
        const Substitution = 1 << 25;
        const TemplateLiteral = 1 << 27;
        const StringMapping = 1 << 28;

        const Nullable = Self::Undefined.bits | Self::Null.bits;
        const Literal = Self::StringLiteral.bits | Self::NumberLiteral.bits | Self::BigIntLiteral.bits | Self::BooleanLiteral.bits;
        const Unit = Self::Literal.bits | Self::UniqueESSymbol.bits | Self::Nullable.bits;
        const StringOrNumberLiteralOrUnique = Self::StringLiteral.bits | Self::NumberLiteral.bits | Self::UniqueESSymbol.bits;
        const Primitive = Self::String.bits | Self::Number.bits | Self::BigInt.bits | Self::Boolean.bits | Self::Enum.bits | Self::EnumLiteral.bits | Self::ESSymbol.bits | Self::Void.bits | Self::Undefined.bits | Self::Null.bits | Self::Literal.bits | Self::UniqueESSymbol.bits;
        const NumberLike = Self::Number.bits | Self::NumberLiteral.bits | Self::Enum.bits;
        const UnionOrIntersection =  Self::Union.bits | Self::Intersection.bits;
        const StructuredType = Self::Object.bits | Self::Union.bits | Self::Intersection.bits;
        const TypeVariable = Self::TypeParameter.bits | Self::IndexedAccess.bits;
        const InstantiableNonPrimitive = Self::TypeVariable.bits | Self::Conditional.bits | Self::Substitution.bits;
        const InstantiablePrimitive = Self::Index.bits | Self::TemplateLiteral.bits | Self::StringMapping.bits;
        const Instantiable = Self::InstantiableNonPrimitive.bits | Self::InstantiablePrimitive.bits;
        const StructuredOrInstantiable = Self::StructuredType.bits | Self::Instantiable.bits;
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    IntrinsicType(IntrinsicType),
    LiteralType(LiteralType),
    ObjectType(ObjectType),
    UnionOrIntersectionType(UnionOrIntersectionType),
}

impl Type {
    pub fn as_intrinsic_type(&self) -> &dyn IntrinsicTypeInterface {
        match self {
            Type::IntrinsicType(intrinsic_type) => intrinsic_type,
            _ => panic!("Expected intrinsic type"),
        }
    }

    pub fn as_union_or_intersection_type(&self) -> &dyn UnionOrIntersectionTypeInterface {
        match self {
            Type::UnionOrIntersectionType(union_or_intersection_type) => union_or_intersection_type,
            _ => panic!("Expected union or intersection type"),
        }
    }

    pub fn as_resolved_type(&self) -> &dyn ResolvedTypeInterface {
        match self {
            Type::ObjectType(object_type) => {
                if !object_type.is_resolved() {
                    panic!("Not resolved")
                }
                object_type
            }
            _ => panic!("Expected resolved type"),
        }
    }

    pub fn as_resolvable_type(&self) -> &dyn ResolvableTypeInterface {
        match self {
            Type::ObjectType(object_type) => object_type,
            _ => panic!("Expected resolvable type"),
        }
    }
}

impl TypeInterface for Type {
    fn flags(&self) -> TypeFlags {
        match self {
            Type::IntrinsicType(intrinsic_type) => intrinsic_type.flags(),
            Type::LiteralType(literal_type) => literal_type.flags(),
            Type::ObjectType(object_type) => object_type.flags(),
            Type::UnionOrIntersectionType(union_or_intersection_type) => {
                union_or_intersection_type.flags()
            }
        }
    }

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        match self {
            Type::IntrinsicType(intrinsic_type) => intrinsic_type.maybe_symbol(),
            Type::LiteralType(literal_type) => literal_type.maybe_symbol(),
            Type::ObjectType(object_type) => object_type.maybe_symbol(),
            Type::UnionOrIntersectionType(union_or_intersection_type) => {
                union_or_intersection_type.maybe_symbol()
            }
        }
    }

    fn symbol(&self) -> Rc<Symbol> {
        match self {
            Type::IntrinsicType(intrinsic_type) => intrinsic_type.symbol(),
            Type::LiteralType(literal_type) => literal_type.symbol(),
            Type::ObjectType(object_type) => object_type.symbol(),
            Type::UnionOrIntersectionType(union_or_intersection_type) => {
                union_or_intersection_type.symbol()
            }
        }
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        match self {
            Type::IntrinsicType(intrinsic_type) => intrinsic_type.set_symbol(symbol),
            Type::LiteralType(literal_type) => literal_type.set_symbol(symbol),
            Type::ObjectType(object_type) => object_type.set_symbol(symbol),
            Type::UnionOrIntersectionType(union_or_intersection_type) => {
                union_or_intersection_type.set_symbol(symbol)
            }
        }
    }
}

pub trait TypeInterface {
    fn flags(&self) -> TypeFlags;
    fn maybe_symbol(&self) -> Option<Rc<Symbol>>;
    fn symbol(&self) -> Rc<Symbol>;
    fn set_symbol(&mut self, symbol: Rc<Symbol>);
}

#[derive(Clone, Debug)]
pub struct BaseType {
    pub flags: TypeFlags,
    symbol: Option<Rc<Symbol>>,
}

impl BaseType {
    pub fn new(flags: TypeFlags) -> Self {
        Self {
            flags,
            symbol: None,
        }
    }
}

impl TypeInterface for BaseType {
    fn flags(&self) -> TypeFlags {
        self.flags
    }

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        self.symbol.as_ref().map(Clone::clone)
    }

    fn symbol(&self) -> Rc<Symbol> {
        self.symbol.as_ref().unwrap().clone()
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        self.symbol = Some(symbol);
    }
}

pub trait IntrinsicTypeInterface: TypeInterface {
    fn intrinsic_name(&self) -> &str;
}

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

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        match self {
            IntrinsicType::BaseIntrinsicType(base_intrinsic_type) => {
                base_intrinsic_type.maybe_symbol()
            }
            IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                freshable_intrinsic_type.maybe_symbol()
            }
        }
    }

    fn symbol(&self) -> Rc<Symbol> {
        match self {
            IntrinsicType::BaseIntrinsicType(base_intrinsic_type) => base_intrinsic_type.symbol(),
            IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                freshable_intrinsic_type.symbol()
            }
        }
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        match self {
            IntrinsicType::BaseIntrinsicType(base_intrinsic_type) => {
                base_intrinsic_type.set_symbol(symbol)
            }
            IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                freshable_intrinsic_type.set_symbol(symbol)
            }
        }
    }
}

impl IntrinsicTypeInterface for IntrinsicType {
    fn intrinsic_name(&self) -> &str {
        match self {
            IntrinsicType::BaseIntrinsicType(base_intrinsic_type) => {
                base_intrinsic_type.intrinsic_name()
            }
            IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                freshable_intrinsic_type.intrinsic_name()
            }
        }
    }
}

impl From<IntrinsicType> for Type {
    fn from(intrinsic_type: IntrinsicType) -> Self {
        Type::IntrinsicType(intrinsic_type)
    }
}

#[derive(Clone, Debug)]
pub struct BaseIntrinsicType {
    _type: BaseType,
    intrinsic_name: String,
}

impl BaseIntrinsicType {
    pub fn new(type_: BaseType, intrinsic_name: String) -> Self {
        Self {
            _type: type_,
            intrinsic_name,
        }
    }
}

impl TypeInterface for BaseIntrinsicType {
    fn flags(&self) -> TypeFlags {
        self._type.flags()
    }

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        self._type.maybe_symbol()
    }

    fn symbol(&self) -> Rc<Symbol> {
        self._type.symbol()
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        self._type.set_symbol(symbol)
    }
}

impl IntrinsicTypeInterface for BaseIntrinsicType {
    fn intrinsic_name(&self) -> &str {
        &self.intrinsic_name
    }
}

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

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        self._intrinsic_type.maybe_symbol()
    }

    fn symbol(&self) -> Rc<Symbol> {
        self._intrinsic_type.symbol()
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        self._intrinsic_type.set_symbol(symbol)
    }
}

impl IntrinsicTypeInterface for FreshableIntrinsicType {
    fn intrinsic_name(&self) -> &str {
        self._intrinsic_type.intrinsic_name()
    }
}

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
    StringLiteralType(StringLiteralType),
    NumberLiteralType(NumberLiteralType),
}

impl TypeInterface for LiteralType {
    fn flags(&self) -> TypeFlags {
        match self {
            LiteralType::StringLiteralType(string_literal_type) => string_literal_type.flags(),
            LiteralType::NumberLiteralType(number_literal_type) => number_literal_type.flags(),
        }
    }

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        match self {
            LiteralType::StringLiteralType(string_literal_type) => {
                string_literal_type.maybe_symbol()
            }
            LiteralType::NumberLiteralType(number_literal_type) => {
                number_literal_type.maybe_symbol()
            }
        }
    }

    fn symbol(&self) -> Rc<Symbol> {
        match self {
            LiteralType::StringLiteralType(string_literal_type) => string_literal_type.symbol(),
            LiteralType::NumberLiteralType(number_literal_type) => number_literal_type.symbol(),
        }
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        match self {
            LiteralType::StringLiteralType(string_literal_type) => {
                string_literal_type.set_symbol(symbol)
            }
            LiteralType::NumberLiteralType(number_literal_type) => {
                number_literal_type.set_symbol(symbol)
            }
        }
    }
}

impl LiteralTypeInterface for LiteralType {
    fn fresh_type(&self) -> Option<&Weak<Type>> {
        match self {
            LiteralType::StringLiteralType(string_literal_type) => string_literal_type.fresh_type(),
            LiteralType::NumberLiteralType(number_literal_type) => number_literal_type.fresh_type(),
        }
    }

    fn set_fresh_type(&self, fresh_type: &Rc<Type>) {
        match self {
            LiteralType::StringLiteralType(string_literal_type) => {
                string_literal_type.set_fresh_type(fresh_type)
            }
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
            LiteralType::StringLiteralType(string_literal_type) => {
                string_literal_type.get_or_initialize_fresh_type(type_checker, wrapper)
            }
            LiteralType::NumberLiteralType(number_literal_type) => {
                number_literal_type.get_or_initialize_fresh_type(type_checker, wrapper)
            }
        }
    }

    fn regular_type(&self) -> Rc<Type> {
        match self {
            LiteralType::StringLiteralType(string_literal_type) => {
                string_literal_type.regular_type()
            }
            LiteralType::NumberLiteralType(number_literal_type) => {
                number_literal_type.regular_type()
            }
        }
    }

    fn set_regular_type(&self, regular_type: &Rc<Type>) {
        match self {
            LiteralType::StringLiteralType(string_literal_type) => {
                string_literal_type.set_regular_type(regular_type)
            }
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

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        self._type.maybe_symbol()
    }

    fn symbol(&self) -> Rc<Symbol> {
        self._type.symbol()
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        self._type.set_symbol(symbol)
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
pub struct StringLiteralType {
    _literal_type: BaseLiteralType,
    value: String,
}

impl StringLiteralType {
    pub fn new(literal_type: BaseLiteralType, value: String) -> Self {
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
        let fresh_type = type_checker.create_string_literal_type(
            self.flags(),
            self.value.clone(),
            Some(wrapper.clone()),
        );
        match &*fresh_type {
            Type::LiteralType(literal_type) => {
                literal_type.set_fresh_type(&fresh_type);
            }
            _ => panic!("Expected LiteralType"),
        }
        self.set_fresh_type(&fresh_type);
        type_checker.keep_strong_reference_to_type(fresh_type);
        self.fresh_type().unwrap().upgrade().unwrap()
    }
}

impl TypeInterface for StringLiteralType {
    fn flags(&self) -> TypeFlags {
        self._literal_type.flags()
    }

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        self._literal_type.maybe_symbol()
    }

    fn symbol(&self) -> Rc<Symbol> {
        self._literal_type.symbol()
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        self._literal_type.set_symbol(symbol)
    }
}

impl LiteralTypeInterface for StringLiteralType {
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

impl From<StringLiteralType> for LiteralType {
    fn from(string_literal_type: StringLiteralType) -> Self {
        LiteralType::StringLiteralType(string_literal_type)
    }
}

impl From<StringLiteralType> for Type {
    fn from(string_literal_type: StringLiteralType) -> Self {
        Type::LiteralType(LiteralType::StringLiteralType(string_literal_type))
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
        type_checker.keep_strong_reference_to_type(fresh_type);
        self.fresh_type().unwrap().upgrade().unwrap()
    }
}

impl TypeInterface for NumberLiteralType {
    fn flags(&self) -> TypeFlags {
        self._literal_type.flags()
    }

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        self._literal_type.maybe_symbol()
    }

    fn symbol(&self) -> Rc<Symbol> {
        self._literal_type.symbol()
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        self._literal_type.set_symbol(symbol)
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

bitflags! {
    pub struct ObjectFlags: u32 {
        const Class = 1 << 0;
        const Interface = 1 << 1;
        const Anonymous = 1 << 4;
        const ObjectLiteral = 1 << 7;
        const FreshLiteral = 1 << 14;
        const ContainsObjectOrArrayLiteral = 1 << 18;

        const ClassOrInterface = Self::Class.bits | Self::Interface.bits;
    }
}

pub trait ObjectTypeInterface {
    fn object_flags(&self) -> ObjectFlags;
    fn set_object_flags(&mut self, object_flags: ObjectFlags);
    // fn maybe_properties(&self) -> Option<&[Rc<Symbol>]>;
    // fn properties(&self) -> &[Rc<Symbol>];
    // fn set_properties(&self, properties: Vec<Rc<Symbol>>);
}

#[derive(Clone, Debug)]
pub enum ObjectType {
    InterfaceType(InterfaceType),
    BaseObjectType(BaseObjectType),
}

impl TypeInterface for ObjectType {
    fn flags(&self) -> TypeFlags {
        match self {
            ObjectType::InterfaceType(interface_type) => interface_type.flags(),
            ObjectType::BaseObjectType(base_object_type) => base_object_type.flags(),
        }
    }

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        match self {
            ObjectType::InterfaceType(interface_type) => interface_type.maybe_symbol(),
            ObjectType::BaseObjectType(base_object_type) => base_object_type.maybe_symbol(),
        }
    }

    fn symbol(&self) -> Rc<Symbol> {
        match self {
            ObjectType::InterfaceType(interface_type) => interface_type.symbol(),
            ObjectType::BaseObjectType(base_object_type) => base_object_type.symbol(),
        }
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        match self {
            ObjectType::InterfaceType(interface_type) => interface_type.set_symbol(symbol),
            ObjectType::BaseObjectType(base_object_type) => base_object_type.set_symbol(symbol),
        }
    }
}

impl ObjectTypeInterface for ObjectType {
    fn object_flags(&self) -> ObjectFlags {
        match self {
            ObjectType::InterfaceType(interface_type) => interface_type.object_flags(),
            ObjectType::BaseObjectType(base_object_type) => base_object_type.object_flags(),
        }
    }

    fn set_object_flags(&mut self, object_flags: ObjectFlags) {
        match self {
            ObjectType::InterfaceType(interface_type) => {
                interface_type.set_object_flags(object_flags)
            }
            ObjectType::BaseObjectType(base_object_type) => {
                base_object_type.set_object_flags(object_flags)
            }
        }
    }
}

impl ResolvableTypeInterface for ObjectType {
    fn resolve(&self, members: Rc<RefCell<SymbolTable>>, properties: Vec<Rc<Symbol>>) {
        match self {
            ObjectType::InterfaceType(interface_type) => {
                interface_type.resolve(members, properties)
            }
            ObjectType::BaseObjectType(base_object_type) => {
                base_object_type.resolve(members, properties)
            }
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            ObjectType::InterfaceType(interface_type) => interface_type.is_resolved(),
            ObjectType::BaseObjectType(base_object_type) => base_object_type.is_resolved(),
        }
    }
}

impl ResolvedTypeInterface for ObjectType {
    fn members(&self) -> Rc<RefCell<SymbolTable>> {
        match self {
            ObjectType::InterfaceType(interface_type) => interface_type.members(),
            ObjectType::BaseObjectType(base_object_type) => base_object_type.members(),
        }
    }

    fn properties(&self) -> RefMut<Vec<Rc<Symbol>>> {
        match self {
            ObjectType::InterfaceType(interface_type) => interface_type.properties(),
            ObjectType::BaseObjectType(base_object_type) => base_object_type.properties(),
        }
    }

    fn set_properties(&self, properties: Vec<Rc<Symbol>>) {
        match self {
            ObjectType::InterfaceType(interface_type) => interface_type.set_properties(properties),
            ObjectType::BaseObjectType(base_object_type) => {
                base_object_type.set_properties(properties)
            }
        }
    }
}

impl From<ObjectType> for Type {
    fn from(object_type: ObjectType) -> Self {
        Type::ObjectType(object_type)
    }
}

#[derive(Clone, Debug)]
pub struct BaseObjectType {
    _type: BaseType,
    object_flags: ObjectFlags,
    members: RefCell<Option<Rc<RefCell<SymbolTable>>>>,
    properties: RefCell<Option<Vec<Rc<Symbol>>>>,
}

impl BaseObjectType {
    pub fn new(base_type: BaseType, object_flags: ObjectFlags) -> Self {
        Self {
            _type: base_type,
            object_flags,
            members: RefCell::new(None),
            properties: RefCell::new(None),
        }
    }
}

impl TypeInterface for BaseObjectType {
    fn flags(&self) -> TypeFlags {
        self._type.flags()
    }

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        self._type.maybe_symbol()
    }

    fn symbol(&self) -> Rc<Symbol> {
        self._type.symbol()
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        self._type.set_symbol(symbol)
    }
}

impl ObjectTypeInterface for BaseObjectType {
    fn object_flags(&self) -> ObjectFlags {
        self.object_flags
    }

    fn set_object_flags(&mut self, object_flags: ObjectFlags) {
        self.object_flags = object_flags;
    }
}

pub trait ResolvableTypeInterface {
    fn resolve(&self, members: Rc<RefCell<SymbolTable>>, properties: Vec<Rc<Symbol>>);
    fn is_resolved(&self) -> bool;
}

impl ResolvableTypeInterface for BaseObjectType {
    fn resolve(&self, members: Rc<RefCell<SymbolTable>>, properties: Vec<Rc<Symbol>>) {
        *self.members.borrow_mut() = Some(members);
        *self.properties.borrow_mut() = Some(properties);
    }

    fn is_resolved(&self) -> bool {
        self.members.borrow().is_some()
    }
}

impl ResolvedTypeInterface for BaseObjectType {
    fn members(&self) -> Rc<RefCell<SymbolTable>> {
        self.members.borrow_mut().as_ref().unwrap().clone()
    }

    fn properties(&self) -> RefMut<Vec<Rc<Symbol>>> {
        RefMut::map(self.properties.borrow_mut(), |option| {
            option.as_mut().unwrap()
        })
    }

    fn set_properties(&self, properties: Vec<Rc<Symbol>>) {
        *self.properties.borrow_mut() = Some(properties);
    }
}

impl From<BaseObjectType> for ObjectType {
    fn from(base_object_type: BaseObjectType) -> Self {
        ObjectType::BaseObjectType(base_object_type)
    }
}

impl From<BaseObjectType> for Type {
    fn from(base_object_type: BaseObjectType) -> Self {
        Type::ObjectType(ObjectType::BaseObjectType(base_object_type))
    }
}

#[derive(Clone, Debug)]
pub enum InterfaceType {
    BaseInterfaceType(BaseInterfaceType),
}

impl TypeInterface for InterfaceType {
    fn flags(&self) -> TypeFlags {
        match self {
            InterfaceType::BaseInterfaceType(base_interface_type) => base_interface_type.flags(),
        }
    }

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        match self {
            InterfaceType::BaseInterfaceType(base_interface_type) => {
                base_interface_type.maybe_symbol()
            }
        }
    }

    fn symbol(&self) -> Rc<Symbol> {
        match self {
            InterfaceType::BaseInterfaceType(base_interface_type) => base_interface_type.symbol(),
        }
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        match self {
            InterfaceType::BaseInterfaceType(base_interface_type) => {
                base_interface_type.set_symbol(symbol)
            }
        }
    }
}

impl ObjectTypeInterface for InterfaceType {
    fn object_flags(&self) -> ObjectFlags {
        match self {
            InterfaceType::BaseInterfaceType(base_interface_type) => {
                base_interface_type.object_flags()
            }
        }
    }

    fn set_object_flags(&mut self, object_flags: ObjectFlags) {
        match self {
            InterfaceType::BaseInterfaceType(base_interface_type) => {
                base_interface_type.set_object_flags(object_flags)
            }
        }
    }
}

impl ResolvableTypeInterface for InterfaceType {
    fn resolve(&self, members: Rc<RefCell<SymbolTable>>, properties: Vec<Rc<Symbol>>) {
        match self {
            InterfaceType::BaseInterfaceType(base_interface_type) => {
                base_interface_type.resolve(members, properties)
            }
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            InterfaceType::BaseInterfaceType(base_interface_type) => {
                base_interface_type.is_resolved()
            }
        }
    }
}

impl ResolvedTypeInterface for InterfaceType {
    fn members(&self) -> Rc<RefCell<SymbolTable>> {
        match self {
            InterfaceType::BaseInterfaceType(base_interface_type) => base_interface_type.members(),
        }
    }

    fn properties(&self) -> RefMut<Vec<Rc<Symbol>>> {
        match self {
            InterfaceType::BaseInterfaceType(base_interface_type) => {
                base_interface_type.properties()
            }
        }
    }

    fn set_properties(&self, properties: Vec<Rc<Symbol>>) {
        match self {
            InterfaceType::BaseInterfaceType(base_interface_type) => {
                base_interface_type.set_properties(properties)
            }
        }
    }
}

impl From<InterfaceType> for ObjectType {
    fn from(interface_type: InterfaceType) -> Self {
        ObjectType::InterfaceType(interface_type)
    }
}

impl From<InterfaceType> for Type {
    fn from(interface_type: InterfaceType) -> Self {
        Type::ObjectType(ObjectType::InterfaceType(interface_type))
    }
}

#[derive(Clone, Debug)]
pub struct BaseInterfaceType {
    _object_type: BaseObjectType,
    members: RefCell<Option<Rc<RefCell<SymbolTable>>>>,
    properties: RefCell<Option<Vec<Rc<Symbol>>>>,
}

impl BaseInterfaceType {
    pub fn new(object_type: BaseObjectType) -> Self {
        Self {
            _object_type: object_type,
            members: RefCell::new(None),
            properties: RefCell::new(None),
        }
    }
}

impl TypeInterface for BaseInterfaceType {
    fn flags(&self) -> TypeFlags {
        self._object_type.flags()
    }

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        self._object_type.maybe_symbol()
    }

    fn symbol(&self) -> Rc<Symbol> {
        self._object_type.symbol()
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        self._object_type.set_symbol(symbol)
    }
}

impl ObjectTypeInterface for BaseInterfaceType {
    fn object_flags(&self) -> ObjectFlags {
        self._object_type.object_flags()
    }

    fn set_object_flags(&mut self, object_flags: ObjectFlags) {
        self._object_type.set_object_flags(object_flags)
    }
}

impl ResolvableTypeInterface for BaseInterfaceType {
    fn resolve(&self, members: Rc<RefCell<SymbolTable>>, properties: Vec<Rc<Symbol>>) {
        *self.members.borrow_mut() = Some(members);
        *self.properties.borrow_mut() = Some(properties);
    }

    fn is_resolved(&self) -> bool {
        self.members.borrow().is_some()
    }
}

impl ResolvedTypeInterface for BaseInterfaceType {
    fn members(&self) -> Rc<RefCell<SymbolTable>> {
        self.members.borrow().as_ref().unwrap().clone()
    }

    fn properties(&self) -> RefMut<Vec<Rc<Symbol>>> {
        RefMut::map(self.properties.borrow_mut(), |option| {
            option.as_mut().unwrap()
        })
    }

    fn set_properties(&self, properties: Vec<Rc<Symbol>>) {
        *self.properties.borrow_mut() = Some(properties);
    }
}

impl From<BaseInterfaceType> for InterfaceType {
    fn from(base_interface_type: BaseInterfaceType) -> Self {
        InterfaceType::BaseInterfaceType(base_interface_type)
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

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        match self {
            UnionOrIntersectionType::UnionType(union_type) => union_type.maybe_symbol(),
        }
    }

    fn symbol(&self) -> Rc<Symbol> {
        match self {
            UnionOrIntersectionType::UnionType(union_type) => union_type.symbol(),
        }
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        match self {
            UnionOrIntersectionType::UnionType(union_type) => union_type.set_symbol(symbol),
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

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        self._type.maybe_symbol()
    }

    fn symbol(&self) -> Rc<Symbol> {
        self._type.symbol()
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        self._type.set_symbol(symbol)
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

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        self._union_or_intersection_type.maybe_symbol()
    }

    fn symbol(&self) -> Rc<Symbol> {
        self._union_or_intersection_type.symbol()
    }

    fn set_symbol(&mut self, symbol: Rc<Symbol>) {
        self._union_or_intersection_type.set_symbol(symbol)
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

pub trait ResolvedTypeInterface {
    fn members(&self) -> Rc<RefCell<SymbolTable>>;
    fn properties(&self) -> RefMut<Vec<Rc<Symbol>>>;
    fn set_properties(&self, properties: Vec<Rc<Symbol>>);
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Ternary {
    False = 0,
    Unknown = 1,
    Maybe = 3,
    True = -1,
}

impl TryFrom<i32> for Ternary {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            value if value == Ternary::False as i32 => Ok(Ternary::False),
            value if value == Ternary::Unknown as i32 => Ok(Ternary::Unknown),
            value if value == Ternary::Maybe as i32 => Ok(Ternary::Maybe),
            value if value == Ternary::True as i32 => Ok(Ternary::True),
            _ => Err(()),
        }
    }
}

impl BitAndAssign for Ternary {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = (*self as i32 & rhs as i32).try_into().unwrap();
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
    pub const at: char = '@';
    pub const close_brace: char = '}';
    pub const close_bracket: char = ']';
    pub const colon: char = ':';
    pub const comma: char = ',';
    pub const equals: char = '=';
    pub const hash: char = '#';
    pub const open_brace: char = '{';
    pub const open_bracket: char = '[';
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

#[derive(Clone, Debug)]
pub struct DiagnosticMessageChain {
    pub message_text: String,
    pub next: Option<Vec<DiagnosticMessageChain>>,
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

    fn start(&self) -> isize {
        self._diagnostic_related_information.start()
    }

    fn length(&self) -> isize {
        self._diagnostic_related_information.length()
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        self._diagnostic_related_information.message_text()
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

    fn start(&self) -> isize {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic_with_location) => {
                diagnostic_with_location.start()
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location) => {
                diagnostic_with_detached_location.start()
            }
        }
    }

    fn length(&self) -> isize {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic_with_location) => {
                diagnostic_with_location.length()
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location) => {
                diagnostic_with_detached_location.length()
            }
        }
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic_with_location) => {
                diagnostic_with_location.message_text()
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location) => {
                diagnostic_with_detached_location.message_text()
            }
        }
    }
}

impl DiagnosticInterface for Diagnostic {}

#[derive(Clone, Debug)]
pub enum DiagnosticMessageText {
    String(String),
    DiagnosticMessageChain(DiagnosticMessageChain),
}

impl From<String> for DiagnosticMessageText {
    fn from(string: String) -> Self {
        DiagnosticMessageText::String(string)
    }
}

impl From<DiagnosticMessageChain> for DiagnosticMessageText {
    fn from(diagnostic_message_chain: DiagnosticMessageChain) -> Self {
        DiagnosticMessageText::DiagnosticMessageChain(diagnostic_message_chain)
    }
}

pub trait DiagnosticRelatedInformationInterface {
    fn file(&self) -> Option<Rc<SourceFile>>;
    fn start(&self) -> isize;
    fn length(&self) -> isize;
    fn message_text(&self) -> &DiagnosticMessageText;
}

#[derive(Clone, Debug)]
pub struct BaseDiagnosticRelatedInformation {
    file: Option<Rc<SourceFile>>,
    start: isize,
    length: isize,
    message_text: DiagnosticMessageText,
}

impl BaseDiagnosticRelatedInformation {
    pub fn new<TDiagnosticMessageText: Into<DiagnosticMessageText>>(
        file: Option<Rc<SourceFile>>,
        start: isize,
        length: isize,
        message_text: TDiagnosticMessageText,
    ) -> Self {
        Self {
            file,
            start,
            length,
            message_text: message_text.into(),
        }
    }
}

impl DiagnosticRelatedInformationInterface for BaseDiagnosticRelatedInformation {
    fn file(&self) -> Option<Rc<SourceFile>> {
        self.file.clone()
    }

    fn start(&self) -> isize {
        self.start
    }

    fn length(&self) -> isize {
        self.length
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        &self.message_text
    }
}

#[derive(Clone, Debug)]
pub struct DiagnosticWithLocation {
    _diagnostic: BaseDiagnostic,
}

impl DiagnosticWithLocation {
    pub fn new(base_diagnostic: BaseDiagnostic) -> Self {
        Self {
            _diagnostic: base_diagnostic,
        }
    }

    pub fn file_unwrapped(&self) -> Rc<SourceFile> {
        self.file().unwrap()
    }
}

impl DiagnosticRelatedInformationInterface for DiagnosticWithLocation {
    fn file(&self) -> Option<Rc<SourceFile>> {
        self._diagnostic.file()
    }

    fn start(&self) -> isize {
        self._diagnostic.start()
    }

    fn length(&self) -> isize {
        self._diagnostic.length()
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        self._diagnostic.message_text()
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
    _diagnostic: BaseDiagnostic,
    file_name: String,
}

impl DiagnosticWithDetachedLocation {
    pub fn new(base_diagnostic: BaseDiagnostic, file_name: String) -> Self {
        Self {
            _diagnostic: base_diagnostic,
            file_name,
        }
    }
}

impl DiagnosticRelatedInformationInterface for DiagnosticWithDetachedLocation {
    fn file(&self) -> Option<Rc<SourceFile>> {
        self._diagnostic.file()
    }

    fn start(&self) -> isize {
        self._diagnostic.start()
    }

    fn length(&self) -> isize {
        self._diagnostic.length()
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        self._diagnostic.message_text()
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum EmitHint {
    Expression,
    Unspecified,
}

pub struct NodeFactory {}

pub struct Printer {
    pub writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
    pub write: fn(&Printer, &str),
}

pub struct PrinterOptions {}

pub trait EmitTextWriter: SymbolWriter {
    fn write(&mut self, s: &str);
    fn get_text(&self) -> String;
}

pub trait ModuleSpecifierResolutionHost {}

pub trait SymbolTracker {}

pub struct TextSpan {
    pub start: isize,
    pub length: isize,
}

pub struct DiagnosticCollection {
    pub file_diagnostics: HashMap<String, SortedArray<Rc<Diagnostic>>>,
}
