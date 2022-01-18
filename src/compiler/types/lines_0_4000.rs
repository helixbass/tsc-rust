#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::ops::BitAndAssign;
use std::ops::Deref;
use std::rc::{Rc, Weak};

use super::{Diagnostic, Symbol, SymbolTable, TypeCheckerHost, __String};
use crate::{NodeBuilder, Number, SortedArray, WeakSelf};
use local_macros::{ast_type, enum_unwrapped, symbol_type, type_type};

#[derive(Debug)]
pub struct Path(String);

impl Path {
    pub fn new(string: String) -> Self {
        Self(string)
    }
}

impl ToString for Path {
    fn to_string(&self) -> String {
        self.0.clone()
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
    BigIntLiteral,
    StringLiteral,
    NoSubstitutionTemplateLiteral,
    TemplateHead,
    TemplateMiddle,
    TemplateTail,
    OpenBraceToken,
    CloseBraceToken,
    OpenParenToken,
    CloseParenToken,
    OpenBracketToken,
    CloseBracketToken,
    DotToken,
    DotDotDotToken,
    SemicolonToken,
    CommaToken,
    LessThanToken,
    GreaterThanToken,
    EqualsGreaterThanToken,
    PlusToken,
    MinusToken,
    AsteriskToken,
    SlashToken,
    PlusPlusToken,
    MinusMinusToken,
    LessThanLessThanToken,
    AmpersandToken,
    BarToken,
    ExclamationToken,
    QuestionToken,
    ColonToken,
    AtToken,
    EqualsToken,
    SlashEqualsToken,
    Identifier,
    PrivateIdentifier,
    BreakKeyword,
    ClassKeyword,
    ConstKeyword,
    DefaultKeyword,
    ElseKeyword,
    EnumKeyword,
    ExportKeyword,
    ExtendsKeyword,
    FalseKeyword,
    FunctionKeyword,
    IfKeyword,
    ImportKeyword,
    NewKeyword,
    NullKeyword,
    ReturnKeyword,
    SuperKeyword,
    ThisKeyword,
    TrueKeyword,
    TypeOfKeyword,
    VarKeyword,
    VoidKeyword,
    WithKeyword,
    ImplementsKeyword,
    InterfaceKeyword,
    PrivateKeyword,
    ProtectedKeyword,
    PublicKeyword,
    StaticKeyword,
    AbstractKeyword,
    AsKeyword,
    AssertsKeyword,
    AnyKeyword,
    AsyncKeyword,
    BooleanKeyword,
    DeclareKeyword,
    GetKeyword,
    InferKeyword,
    IntrinsicKeyword,
    IsKeyword,
    NeverKeyword,
    ReadonlyKeyword,
    NumberKeyword,
    ObjectKeyword,
    SetKeyword,
    StringKeyword,
    SymbolKeyword,
    TypeKeyword,
    UndefinedKeyword,
    UniqueKeyword,
    UnknownKeyword,
    BigIntKeyword,
    OverrideKeyword,
    OfKeyword,

    QualifiedName,
    TypeParameter,
    Parameter,
    PropertySignature,
    PropertyDeclaration,
    MethodSignature,
    MethodDeclaration,
    ClassStaticBlockDeclaration,
    Constructor,
    GetAccessor,
    SetAccessor,
    CallSignature,
    ConstructSignature,
    IndexSignature,
    TypePredicate,
    TypeReference,
    FunctionType,
    ConstructorType,
    TypeLiteral,
    ArrayType,
    UnionType,
    IntersectionType,
    LiteralType,
    ObjectBindingPattern,
    ArrayBindingPattern,
    BindingElement,
    ArrayLiteralExpression,
    ObjectLiteralExpression,
    PropertyAccessExpression,
    ParenthesizedExpression,
    FunctionExpression,
    ArrowFunction,
    PrefixUnaryExpression,
    BinaryExpression,
    TemplateExpression,
    ClassExpression,
    OmittedExpression,
    ExpressionWithTypeArguments,

    TemplateSpan,
    Block,
    EmptyStatement,
    VariableStatement,
    ExpressionStatement,
    IfStatement,
    ReturnStatement,
    VariableDeclaration,
    VariableDeclarationList,
    FunctionDeclaration,
    ClassDeclaration,
    InterfaceDeclaration,
    TypeAliasDeclaration,
    ModuleBlock,

    CatchClause,

    PropertyAssignment,

    EnumMember,

    SourceFile,

    JSDocFunctionType,
    JSDocSignature,
    JSDocCallbackTag,
    JSDocEnumTag,
    JSDocTypedefTag,
}

impl SyntaxKind {
    pub const LastReservedWord: SyntaxKind = SyntaxKind::WithKeyword;
    pub const FirstKeyword: SyntaxKind = SyntaxKind::BreakKeyword;
    pub const LastKeyword: SyntaxKind = SyntaxKind::OfKeyword;
    pub const LastToken: SyntaxKind = SyntaxKind::LastKeyword;
    pub const FirstLiteralToken: SyntaxKind = SyntaxKind::NumericLiteral;
    pub const LastLiteralToken: SyntaxKind = SyntaxKind::NoSubstitutionTemplateLiteral;
    pub const FirstTemplateToken: SyntaxKind = SyntaxKind::NoSubstitutionTemplateLiteral;
    pub const LastTemplateToken: SyntaxKind = SyntaxKind::TemplateTail;
}

bitflags! {
    pub struct NodeFlags: u32 {
        const None = 0;
        const Let = 1 << 0;
        const Const = 1 << 1;
        const DisallowInContext = 1 << 12;
        const YieldContext = 1 << 13;
        const DecoratorContext = 1 << 14;
        const AwaitContext = 1 << 15;
        const Ambient = 1 << 23;

        const BlockScoped = Self::Let.bits | Self::Const.bits;

        const TypeExcludesFlags = Self::YieldContext.bits | Self::AwaitContext.bits;
    }
}

bitflags! {
    pub struct ModifierFlags: u32 {
        const None = 0;
        const Export = 1 << 0;
        const Ambient = 1 << 1;
        const Public = 1 << 2;
        const Private = 1 << 3;
        const Protected = 1 << 4;
        const Static = 1 << 5;
        const Readonly = 1 << 6;
        const Abstract = 1 << 7;
        const Async = 1 << 8;
        const Default = 1 << 9;
        const Const = 1 << 11;

        const Override = 1 << 14;
    }
}

bitflags! {
    pub struct RelationComparisonResult: u32 {
        const Succeeded = 1 << 0;
        const Failed = 1 << 1;
    }
}

pub type NodeId = u32;

pub trait NodeInterface: ReadonlyTextRange {
    fn node_wrapper(&self) -> Rc<Node>;
    fn set_node_wrapper(&self, wrapper: Rc<Node>);
    fn kind(&self) -> SyntaxKind;
    fn flags(&self) -> NodeFlags;
    fn set_flags(&self, flags: NodeFlags);
    fn maybe_decorators(&self) -> Ref<Option<NodeArray>>;
    fn set_decorators(&self, decorators: Option<NodeArray>);
    fn maybe_modifiers(&self) -> Option<&NodeArray>;
    fn maybe_id(&self) -> Option<NodeId>;
    fn id(&self) -> NodeId;
    fn set_id(&self, id: NodeId);
    fn maybe_parent(&self) -> Option<Rc<Node>>;
    fn parent(&self) -> Rc<Node>;
    fn set_parent(&self, parent: Rc<Node>);
    fn maybe_symbol(&self) -> Option<Rc<Symbol>>;
    fn symbol(&self) -> Rc<Symbol>;
    fn set_symbol(&self, symbol: Rc<Symbol>);
    fn maybe_locals(&self) -> RefMut<Option<SymbolTable>>;
    fn locals(&self) -> RefMut<SymbolTable>;
    fn set_locals(&self, locals: Option<SymbolTable>);
}

#[derive(Debug)]
#[ast_type(impl_from = false)]
pub enum Node {
    BaseNode(BaseNode),
    TypeParameterDeclaration(TypeParameterDeclaration),
    Decorator(Decorator),
    SignatureDeclarationBase(SignatureDeclarationBase),
    VariableDeclaration(VariableDeclaration),
    VariableDeclarationList(VariableDeclarationList),
    ParameterDeclaration(ParameterDeclaration),
    TypeNode(TypeNode),
    Expression(Expression),
    TemplateSpan(TemplateSpan),
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
            Node::TypeParameterDeclaration(type_parameter_declaration) => {
                type_parameter_declaration
            }
            Node::VariableDeclaration(variable_declaration) => variable_declaration,
            Node::Statement(Statement::InterfaceDeclaration(interface_declaration)) => {
                interface_declaration
            }
            Node::Statement(Statement::TypeAliasDeclaration(type_alias_declaration)) => {
                type_alias_declaration
            }
            Node::TypeElement(type_element) => type_element,
            Node::PropertyAssignment(property_assignment) => property_assignment,
            Node::Statement(Statement::FunctionDeclaration(function_declaration)) => {
                function_declaration
            }
            Node::ParameterDeclaration(parameter_declaration) => parameter_declaration,
            _ => panic!("Expected named declaration"),
        }
    }

    pub fn as_member_name(&self) -> &dyn MemberNameInterface {
        match self {
            Node::Expression(Expression::Identifier(identifier)) => identifier,
            _ => panic!("Expected member name"),
        }
    }

    pub fn as_literal_like_node(&self) -> &dyn LiteralLikeNodeInterface {
        match self {
            Node::Expression(Expression::LiteralLikeNode(literal_like_node)) => literal_like_node,
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

    pub fn as_has_type_parameters(&self) -> &dyn HasTypeParametersInterface {
        match self {
            Node::Statement(Statement::InterfaceDeclaration(interface_declaration)) => {
                interface_declaration
            }
            Node::Statement(Statement::TypeAliasDeclaration(type_alias_declaration)) => {
                type_alias_declaration
            }
            _ => panic!("Expected has type parameters"),
        }
    }

    pub fn as_has_type_arguments(&self) -> &dyn HasTypeArgumentsInterface {
        match self {
            Node::TypeNode(TypeNode::TypeReferenceNode(type_reference_node)) => type_reference_node,
            _ => panic!("Expected has type arguments"),
        }
    }

    pub fn as_union_or_intersection_type_node(&self) -> &dyn UnionOrIntersectionTypeNodeInterface {
        match self {
            Node::TypeNode(TypeNode::UnionTypeNode(union_type_node)) => union_type_node,
            Node::TypeNode(TypeNode::IntersectionTypeNode(intersection_type_node)) => {
                intersection_type_node
            }
            _ => panic!("Expected union or intersection type"),
        }
    }

    pub fn as_expression(&self) -> &Expression {
        // node_unwrapped!(self, Expression)
        enum_unwrapped!(self, [Node, Expression])
    }

    pub fn as_variable_declaration_list(&self) -> &VariableDeclarationList {
        enum_unwrapped!(self, [Node, VariableDeclarationList])
    }

    pub fn as_type_parameter_declaration(&self) -> &TypeParameterDeclaration {
        enum_unwrapped!(self, [Node, TypeParameterDeclaration])
    }

    pub fn as_property_assignment(&self) -> &PropertyAssignment {
        enum_unwrapped!(self, [Node, PropertyAssignment])
    }

    pub fn as_source_file(&self) -> &Rc<SourceFile> {
        enum_unwrapped!(self, [Node, SourceFile])
    }

    pub fn as_variable_declaration(&self) -> &VariableDeclaration {
        enum_unwrapped!(self, [Node, VariableDeclaration])
    }

    pub fn as_object_literal_expression(&self) -> &ObjectLiteralExpression {
        enum_unwrapped!(self, [Node, Expression, ObjectLiteralExpression])
    }

    pub fn as_identifier(&self) -> &Identifier {
        enum_unwrapped!(self, [Node, Expression, Identifier])
    }

    pub fn as_type_node(&self) -> &TypeNode {
        enum_unwrapped!(self, [Node, TypeNode])
    }

    pub fn as_type_alias_declaration(&self) -> &TypeAliasDeclaration {
        enum_unwrapped!(self, [Node, Statement, TypeAliasDeclaration])
    }

    pub fn as_template_span(&self) -> &TemplateSpan {
        enum_unwrapped!(self, [Node, TemplateSpan])
    }
}

#[derive(Debug)]
pub struct BaseNode {
    _node_wrapper: RefCell<Option<Weak<Node>>>,
    pub kind: SyntaxKind,
    flags: Cell<NodeFlags>,
    pub decorators: RefCell<Option<NodeArray /*<Decorator>*/>>,
    pub modifiers: Option<ModifiersArray>,
    pub id: Cell<Option<NodeId>>,
    pub parent: RefCell<Option<Weak<Node>>>,
    pub pos: Cell<isize>,
    pub end: Cell<isize>,
    pub symbol: RefCell<Option<Weak<Symbol>>>,
    pub locals: RefCell<Option<SymbolTable>>,
}

impl BaseNode {
    pub fn new(kind: SyntaxKind, flags: NodeFlags, pos: isize, end: isize) -> Self {
        Self {
            _node_wrapper: RefCell::new(None),
            kind,
            flags: Cell::new(flags),
            decorators: RefCell::new(None),
            modifiers: None,
            id: Cell::new(None),
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

    fn flags(&self) -> NodeFlags {
        self.flags.get()
    }

    fn set_flags(&self, flags: NodeFlags) {
        self.flags.set(flags);
    }

    fn maybe_decorators(&self) -> Ref<Option<NodeArray>> {
        self.decorators.borrow()
    }

    fn set_decorators(&self, decorators: Option<NodeArray>) {
        *self.decorators.borrow_mut() = decorators;
    }

    fn maybe_modifiers(&self) -> Option<&NodeArray> {
        self.modifiers.as_ref()
    }

    fn maybe_id(&self) -> Option<NodeId> {
        self.id.get().clone()
    }

    fn id(&self) -> NodeId {
        self.id.get().clone().unwrap()
    }

    fn set_id(&self, id: NodeId) {
        self.id.set(Some(id));
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

    fn set_locals(&self, locals: Option<SymbolTable>) {
        *self.locals.borrow_mut() = locals;
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
    fn maybe_type(&self) -> Option<Rc<Node>>;
    fn set_type(&mut self, type_: Rc<Node>);
}

pub trait HasExpressionInitializerInterface {
    fn maybe_initializer(&self) -> Option<Rc<Node>>;
    fn set_initializer(&mut self, initializer: Rc<Node>);
}

#[derive(Clone, Debug)]
pub struct NodeArray {
    _nodes: Vec<Rc<Node>>,
    pub is_missing_list: bool,
}

impl NodeArray {
    pub fn new(nodes: Vec<Rc<Node>>) -> Self {
        NodeArray {
            _nodes: nodes,
            is_missing_list: false,
        }
    }

    pub fn iter(&self) -> NodeArrayIter {
        NodeArrayIter(Box::new(self._nodes.iter()))
    }

    pub fn len(&self) -> usize {
        self._nodes.len()
    }
}

impl Default for NodeArray {
    fn default() -> Self {
        Self::new(vec![])
    }
}

impl From<&NodeArray> for Vec<Rc<Node>> {
    fn from(node_array: &NodeArray) -> Self {
        node_array._nodes.clone()
    }
}

impl<'node_array> From<&'node_array NodeArray> for &'node_array [Rc<Node>] {
    fn from(node_array: &'node_array NodeArray) -> Self {
        &node_array._nodes
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

impl Deref for NodeArray {
    type Target = [Rc<Node>];

    fn deref(&self) -> &Self::Target {
        &self._nodes
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

pub type ModifiersArray = NodeArray; /*<Modifier>*/

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
    fn maybe_initializer(&self) -> Option<Rc<Node>> {
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
    fn maybe_type(&self) -> Option<Rc<Node>> {
        self.type_.as_ref().map(Clone::clone)
    }

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = Some(type_);
    }
}

impl VariableLikeDeclarationInterface for BaseVariableLikeDeclaration {}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct TypeParameterDeclaration {
    _named_declaration: BaseNamedDeclaration,
}

impl TypeParameterDeclaration {
    pub fn new(base_named_declaration: BaseNamedDeclaration) -> Self {
        Self {
            _named_declaration: base_named_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct Decorator {
    _node: BaseNode,
    expression: Rc<Node /*LeftHandSideExpression*/>,
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface, SignatureDeclarationInterface")]
pub enum SignatureDeclarationBase {
    FunctionLikeDeclarationBase(FunctionLikeDeclarationBase),
}

pub trait SignatureDeclarationInterface {
    fn parameters(&self) -> &NodeArray /*<ParameterDeclaration>*/;
}

#[derive(Debug)]
#[ast_type(
    impl_from = false,
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface"
)]
pub struct BaseSignatureDeclaration {
    _generic_named_declaration: BaseGenericNamedDeclaration,
    parameters: NodeArray, /*<ParameterDeclaration>*/
    type_: Option<Rc<Node /*TypeNode*/>>,
}

impl BaseSignatureDeclaration {
    pub fn new(
        generic_named_declaration: BaseGenericNamedDeclaration,
        parameters: NodeArray,
        type_: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _generic_named_declaration: generic_named_declaration,
            parameters,
            type_,
        }
    }
}

impl HasTypeInterface for BaseSignatureDeclaration {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = Some(type_);
    }
}

impl SignatureDeclarationInterface for BaseSignatureDeclaration {
    fn parameters(&self) -> &NodeArray {
        &self.parameters
    }
}

#[derive(Debug)]
#[ast_type(
    ancestors = "SignatureDeclarationBase",
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface"
)]
pub enum FunctionLikeDeclarationBase {
    BaseFunctionLikeDeclaration(BaseFunctionLikeDeclaration),
}

pub trait FunctionLikeDeclarationInterface {
    fn maybe_body(&self) -> Option<Rc<Node>>;
    fn maybe_asterisk_token(&self) -> Option<Rc<Node>>;
    fn maybe_question_token(&self) -> Option<Rc<Node>>;
    fn maybe_exclamation_token(&self) -> Option<Rc<Node>>;
}

#[derive(Debug)]
#[ast_type(
    ancestors = "FunctionLikeDeclarationBase, SignatureDeclarationBase",
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface"
)]
pub struct BaseFunctionLikeDeclaration {
    _signature_declaration: BaseSignatureDeclaration,
    pub asterisk_token: Option<Rc<Node /*AsteriskToken*/>>,
    question_token: Option<Rc<Node /*QuestionToken*/>>,
    exclamation_token: Option<Rc<Node /*ExclamationToken*/>>,
    body: Option<Rc<Node /*Block | Expression*/>>,
}

impl BaseFunctionLikeDeclaration {
    pub fn new(signature_declaration: BaseSignatureDeclaration, body: Option<Rc<Node>>) -> Self {
        Self {
            _signature_declaration: signature_declaration,
            body,
            asterisk_token: None,
            question_token: None,
            exclamation_token: None,
        }
    }
}

impl FunctionLikeDeclarationInterface for BaseFunctionLikeDeclaration {
    fn maybe_body(&self) -> Option<Rc<Node>> {
        self.body.clone()
    }

    fn maybe_asterisk_token(&self) -> Option<Rc<Node>> {
        self.asterisk_token.clone()
    }

    fn maybe_question_token(&self) -> Option<Rc<Node>> {
        self.question_token.clone()
    }

    fn maybe_exclamation_token(&self) -> Option<Rc<Node>> {
        self.exclamation_token.clone()
    }
}

#[derive(Debug)]
#[ast_type(
    ancestors = "Statement",
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface, FunctionLikeDeclarationInterface"
)]
pub struct FunctionDeclaration {
    _function_like_declaration: BaseFunctionLikeDeclaration,
}

impl FunctionDeclaration {
    pub fn new(function_like_declaration: BaseFunctionLikeDeclaration) -> Self {
        Self {
            _function_like_declaration: function_like_declaration,
        }
    }
}

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
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasExpressionInitializerInterface, BindingLikeDeclarationInterface, HasTypeInterface, VariableLikeDeclarationInterface"
)]
pub struct ParameterDeclaration {
    _variable_like_declaration: BaseVariableLikeDeclaration,
    pub dot_dot_dot_token: Option<Rc<Node /*DotDotDotToken*/>>,
    pub question_token: Option<Rc<Node /*QuestionToken*/>>,
}

impl ParameterDeclaration {
    pub fn new(
        base_variable_like_declaration: BaseVariableLikeDeclaration,
        dot_dot_dot_token: Option<Rc<Node>>,
        question_token: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _variable_like_declaration: base_variable_like_declaration,
            dot_dot_dot_token,
            question_token,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub enum TypeNode {
    KeywordTypeNode(KeywordTypeNode),
    UnionTypeNode(UnionTypeNode),
    IntersectionTypeNode(IntersectionTypeNode),
    LiteralTypeNode(LiteralTypeNode),
    TypeReferenceNode(TypeReferenceNode),
    TypePredicateNode(TypePredicateNode),
    TypeLiteralNode(TypeLiteralNode),
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

pub trait HasTypeArgumentsInterface {
    fn maybe_type_arguments(&self) -> Option<&NodeArray>;
}

#[derive(Debug)]
#[ast_type(ancestors = "TypeNode")]
pub struct TypeReferenceNode {
    _node: BaseNode,
    pub type_name: Rc<Node /*EntityName*/>,
    pub type_arguments: Option<NodeArray /*<TypeNode>*/>,
}

impl TypeReferenceNode {
    pub fn new(
        base_node: BaseNode,
        type_name: Rc<Node>,
        type_arguments: Option<NodeArray>,
    ) -> Self {
        Self {
            _node: base_node,
            type_name,
            type_arguments,
        }
    }
}

impl HasTypeArgumentsInterface for TypeReferenceNode {
    fn maybe_type_arguments(&self) -> Option<&NodeArray> {
        self.type_arguments.as_ref()
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "TypeNode")]
pub struct TypePredicateNode {
    _node: BaseNode,
    pub asserts_modifier: Option<Rc<Node /*AssertsToken*/>>,
    pub parameter_name: Rc<Node /*Identifier | ThisTypeNode*/>,
    pub type_: Option<Rc<Node /*TypeNode*/>>,
}

impl TypePredicateNode {
    pub fn new(
        base_node: BaseNode,
        asserts_modifier: Option<Rc<Node>>,
        parameter_name: Rc<Node>,
        type_: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            asserts_modifier,
            parameter_name,
            type_,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "TypeNode")]
pub struct TypeLiteralNode {
    _node: BaseNode,
    pub members: NodeArray, /*<TypeElement>*/
}

impl TypeLiteralNode {
    pub fn new(base_node: BaseNode, members: NodeArray) -> Self {
        Self {
            _node: base_node,
            members,
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

pub trait UnionOrIntersectionTypeNodeInterface {
    fn types(&self) -> &NodeArray;
}

#[derive(Debug)]
#[ast_type(ancestors = "TypeNode")]
pub struct UnionTypeNode {
    _node: BaseNode,
    pub types: NodeArray, /*<TypeNode>*/
}

impl UnionTypeNode {
    pub fn new(base_node: BaseNode, types: NodeArray) -> Self {
        Self {
            _node: base_node,
            types,
        }
    }
}

impl UnionOrIntersectionTypeNodeInterface for UnionTypeNode {
    fn types(&self) -> &NodeArray {
        &self.types
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "TypeNode")]
pub struct IntersectionTypeNode {
    _node: BaseNode,
    pub types: NodeArray, /*<TypeNode>*/
}

impl IntersectionTypeNode {
    pub fn new(base_node: BaseNode, types: NodeArray) -> Self {
        Self {
            _node: base_node,
            types,
        }
    }
}

impl UnionOrIntersectionTypeNodeInterface for IntersectionTypeNode {
    fn types(&self) -> &NodeArray {
        &self.types
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
#[ast_type(
    ancestors = "LiteralLikeNode, Expression",
    interfaces = "LiteralLikeNodeInterface"
)]
pub struct StringLiteral {
    _literal_like_node: BaseLiteralLikeNode,
    pub single_quote: Option<bool>,
}

impl StringLiteral {
    pub fn new(base_literal_like_node: BaseLiteralLikeNode, single_quote: Option<bool>) -> Self {
        Self {
            _literal_like_node: base_literal_like_node,
            single_quote,
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
    TemplateExpression(TemplateExpression),
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
    _node: BaseNode,
    text: String,
    is_unterminated: Option<bool>,
    has_extended_unicode_escape: Option<bool>,
}

impl BaseLiteralLikeNode {
    pub fn new(base_node: BaseNode, text: String) -> Self {
        Self {
            _node: base_node,
            text,
            is_unterminated: None,
            has_extended_unicode_escape: None,
        }
    }
}

impl LiteralLikeNodeInterface for BaseLiteralLikeNode {
    fn text(&self) -> &str {
        &self.text
    }

    fn is_unterminated(&self) -> Option<bool> {
        self.is_unterminated.clone()
    }

    fn set_is_unterminated(&mut self, is_unterminated: Option<bool>) {
        self.is_unterminated = is_unterminated;
    }

    fn has_extended_unicode_escape(&self) -> Option<bool> {
        self.has_extended_unicode_escape.clone()
    }

    fn set_has_extended_unicode_escape(&mut self, has_extended_unicode_escape: Option<bool>) {
        self.has_extended_unicode_escape = has_extended_unicode_escape;
    }
}

pub trait LiteralLikeNodeInterface {
    fn text(&self) -> &str;
    fn is_unterminated(&self) -> Option<bool>;
    fn set_is_unterminated(&mut self, is_unterminated: Option<bool>);
    fn has_extended_unicode_escape(&self) -> Option<bool>;
    fn set_has_extended_unicode_escape(&mut self, has_extended_unicode_escape: Option<bool>);
}

#[derive(Debug)]
#[ast_type(ancestors = "Expression", interfaces = "LiteralLikeNodeInterface")]
pub enum LiteralLikeNode {
    StringLiteral(StringLiteral),
    TemplateLiteralLikeNode(TemplateLiteralLikeNode),
    NumericLiteral(NumericLiteral),
    BigIntLiteral(BigIntLiteral),
}

#[derive(Debug)]
#[ast_type(
    ancestors = "LiteralLikeNode, Expression",
    interfaces = "LiteralLikeNodeInterface"
)]
pub struct TemplateLiteralLikeNode {
    _literal_like_node: BaseLiteralLikeNode,
    pub raw_text: Option<String>,
    pub template_flags: Option<TokenFlags>,
}

impl TemplateLiteralLikeNode {
    pub fn new(
        literal_like_node: BaseLiteralLikeNode,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> Self {
        Self {
            _literal_like_node: literal_like_node,
            raw_text,
            template_flags,
        }
    }
}

impl TemplateLiteralLikeNodeInterface for TemplateLiteralLikeNode {
    fn maybe_raw_text(&self) -> Option<&str> {
        self.raw_text.as_deref()
    }

    fn maybe_template_flags(&self) -> Option<TokenFlags> {
        self.template_flags
    }
}

pub trait TemplateLiteralLikeNodeInterface {
    fn maybe_raw_text(&self) -> Option<&str>;
    fn maybe_template_flags(&self) -> Option<TokenFlags>;
}

bitflags! {
    pub struct TokenFlags: u32 {
        const None = 0;
        const PrecedingLineBreak = 1 << 0;
        const PrecedingJSDocComment = 1 << 1;
        const Unterminated = 1 << 2;
        const ExtendedUnicodeEscape = 1 << 3;
        const Scientific = 1 << 4;
        const Octal = 1 << 5;
        const HexSpecifier = 1 << 6;
        const BinarySpecifier = 1 << 7;
        const OctalSpecifier = 1 << 8;
        const ContainsSeparator = 1 << 9;
        const UnicodeEscape = 1 << 10;
        const ContainsInvalidEscape = 1 << 11;

        const BinaryOrOctalSpecifier = Self::BinarySpecifier.bits | Self::OctalSpecifier.bits;
        const NumericLiteralFlags = Self::Scientific.bits | Self::Octal.bits | Self::HexSpecifier.bits | Self::BinaryOrOctalSpecifier.bits | Self::ContainsSeparator.bits;
        const TemplateLiteralLikeFlags = Self::ContainsInvalidEscape.bits;
    }
}

#[derive(Debug)]
#[ast_type(
    ancestors = "LiteralLikeNode, Expression",
    interfaces = "LiteralLikeNodeInterface"
)]
pub struct NumericLiteral {
    _literal_like_node: BaseLiteralLikeNode,
}

impl NumericLiteral {
    pub fn new(base_literal_like_node: BaseLiteralLikeNode) -> Self {
        Self {
            _literal_like_node: base_literal_like_node,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    ancestors = "LiteralLikeNode, Expression",
    interfaces = "LiteralLikeNodeInterface"
)]
pub struct BigIntLiteral {
    _literal_like_node: BaseLiteralLikeNode,
}

impl BigIntLiteral {
    pub fn new(base_literal_like_node: BaseLiteralLikeNode) -> Self {
        Self {
            _literal_like_node: base_literal_like_node,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Expression")]
pub struct TemplateExpression {
    _node: BaseNode,
    pub head: Rc<Node /*TemplateHead*/>,
    pub template_spans: NodeArray, /*<TemplateSpan>*/
}

impl TemplateExpression {
    pub fn new(base_node: BaseNode, head: Rc<Node>, template_spans: NodeArray) -> Self {
        Self {
            _node: base_node,
            head,
            template_spans,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TemplateSpan {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
    pub literal: Rc<Node /*TemplateMiddle | TemplateTail*/>,
}

impl TemplateSpan {
    pub fn new(base_node: BaseNode, expression: Rc<Node>, literal: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
            literal,
        }
    }
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
    FunctionDeclaration(FunctionDeclaration),
    EmptyStatement(EmptyStatement),
    Block(Block),
    VariableStatement(VariableStatement),
    ExpressionStatement(ExpressionStatement),
    IfStatement(IfStatement),
    ReturnStatement(ReturnStatement),
    InterfaceDeclaration(InterfaceDeclaration),
    TypeAliasDeclaration(TypeAliasDeclaration),
}

#[derive(Debug)]
#[ast_type(ancestors = "Statement")]
pub struct EmptyStatement {
    pub _node: BaseNode,
}

#[derive(Debug)]
#[ast_type(ancestors = "Statement")]
pub struct Block {
    _node: BaseNode,
    pub statements: NodeArray, /*<Statement>*/
    pub(crate) multi_line: Option<bool>,
}

impl Block {
    pub fn new(base_node: BaseNode, statements: NodeArray, multi_line: Option<bool>) -> Self {
        Self {
            _node: base_node,
            statements,
            multi_line,
        }
    }
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
    _node: BaseNode,
    pub expression: Rc</*Expression*/ Node>,
}

impl ExpressionStatement {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Statement")]
pub struct IfStatement {
    _node: BaseNode,
    pub expression: Rc</*Expression*/ Node>,
    pub then_statement: Rc</*Statement*/ Node>,
    pub else_statement: Option<Rc</*Statement*/ Node>>,
}

impl IfStatement {
    pub fn new(
        base_node: BaseNode,
        expression: Rc<Node>,
        then_statement: Rc<Node>,
        else_statement: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            then_statement,
            else_statement,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Statement")]
pub struct ReturnStatement {
    _node: BaseNode,
    pub expression: Option<Rc</*Expression*/ Node>>,
}

impl ReturnStatement {
    pub fn new(base_node: BaseNode, expression: Option<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
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
    fn maybe_type(&self) -> Option<Rc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = Some(type_);
    }
}

impl HasExpressionInitializerInterface for PropertySignature {
    fn maybe_initializer(&self) -> Option<Rc<Node>> {
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

pub trait HasTypeParametersInterface {
    fn maybe_type_parameters(&self) -> Option<&NodeArray>;
}

pub trait GenericNamedDeclarationInterface:
    NamedDeclarationInterface + HasTypeParametersInterface
{
}

#[derive(Debug)]
#[ast_type(impl_from = false, interfaces = "NamedDeclarationInterface")]
pub struct BaseGenericNamedDeclaration {
    _named_declaration: BaseNamedDeclaration,
    pub type_parameters: Option<NodeArray /*<TypeParameterDeclaration>*/>,
}

impl BaseGenericNamedDeclaration {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        type_parameters: Option<NodeArray>,
    ) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            type_parameters,
        }
    }
}

impl HasTypeParametersInterface for BaseGenericNamedDeclaration {
    fn maybe_type_parameters(&self) -> Option<&NodeArray> {
        self.type_parameters.as_ref()
    }
}

impl GenericNamedDeclarationInterface for BaseGenericNamedDeclaration {}

#[derive(Debug)]
#[ast_type(
    impl_from = false,
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface"
)]
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
#[ast_type(
    ancestors = "Statement",
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface"
)]
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
#[ast_type(
    ancestors = "Statement",
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface"
)]
pub struct TypeAliasDeclaration {
    _generic_named_declaration: BaseGenericNamedDeclaration, /*name: Identifier*/
    pub type_: Rc<Node /*TypeNode*/>,
}

impl TypeAliasDeclaration {
    pub fn new(
        base_generic_named_declaration: BaseGenericNamedDeclaration,
        type_: Rc<Node>,
    ) -> Self {
        Self {
            _generic_named_declaration: base_generic_named_declaration,
            type_,
        }
    }
}

#[derive(Debug)]
#[ast_type(impl_from = false)]
pub struct SourceFile {
    _node: BaseNode,
    _symbols_without_a_symbol_table_strong_references: RefCell<Vec<Rc<Symbol>>>,
    pub statements: NodeArray,

    file_name: RefCell<String>,
    path: RefCell<Option<Path>>,
    pub text: String,

    parse_diagnostics: RefCell<Option<Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>>>,
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
            file_name: RefCell::new(file_name),
            path: RefCell::new(None),
            text,
            parse_diagnostics: RefCell::new(None),
        }
    }

    pub fn file_name(&self) -> Ref<String> {
        self.file_name.borrow()
    }

    pub fn set_file_name(&self, file_name: String) {
        *self.file_name.borrow_mut() = file_name;
    }

    pub fn maybe_path(&self) -> Ref<Option<Path>> {
        self.path.borrow()
    }

    pub fn set_path(&self, path: Path) {
        *self.path.borrow_mut() = Some(path);
    }

    pub fn parse_diagnostics(&self) -> Ref<Vec<Rc<Diagnostic>>> {
        Ref::map(self.parse_diagnostics.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn set_parse_diagnostics(&self, parse_diagnostics: Vec<Rc<Diagnostic>>) {
        *self.parse_diagnostics.borrow_mut() = Some(parse_diagnostics);
    }

    pub fn keep_strong_reference_to_symbol(&self, symbol: Rc<Symbol>) {
        self._symbols_without_a_symbol_table_strong_references
            .borrow_mut()
            .push(symbol);
    }
}

// impl From<Rc<SourceFile>> for Rc<Node> {
//     fn from(source_file: Rc<SourceFile>) -> Self {
//         let rc = Rc::new(Node::SourceFile(source_file));
//         rc.set_node_wrapper(rc.clone());
//         rc
//     }
// }

pub fn rc_source_file_into_rc_node(source_file: Rc<SourceFile>) -> Rc<Node> {
    let rc = Rc::new(Node::SourceFile(source_file));
    rc.set_node_wrapper(rc.clone());
    rc
}

pub trait Program: TypeCheckerHost {
    fn get_syntactic_diagnostics(&mut self) -> Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>;
    fn get_semantic_diagnostics(&mut self) -> Vec<Rc<Diagnostic>>;
}
