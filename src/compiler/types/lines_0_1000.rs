#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::rc::{Rc, Weak};

use super::{
    Decorator, Expression, HasTypeArgumentsInterface, HasTypeParametersInterface, Identifier,
    LiteralLikeNodeInterface, MemberNameInterface, ModifiersArray, NamedDeclarationInterface,
    NodeArray, ObjectLiteralExpression, ParameterDeclaration, PropertyAssignment,
    SignatureDeclarationBase, SourceFile, Statement, Symbol, SymbolTable, TemplateSpan,
    TypeAliasDeclaration, TypeElement, TypeNode, TypeParameterDeclaration,
    UnionOrIntersectionTypeNodeInterface, VariableDeclaration, VariableDeclarationList,
};
use local_macros::{ast_type, enum_unwrapped};

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

pub trait TextRange {
    fn pos(&self) -> isize;
    fn set_pos(&self, pos: isize);
    fn end(&self) -> isize;
    fn set_end(&self, end: isize);
}

#[derive(Clone, Debug)]
pub struct BaseTextRange {
    pos: Cell<isize>,
    end: Cell<isize>,
}

impl BaseTextRange {
    pub fn new(pos: isize, end: isize) -> Self {
        Self {
            pos: Cell::new(pos),
            end: Cell::new(end),
        }
    }
}

impl TextRange for BaseTextRange {
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

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum SyntaxKind {
    Unknown,
    EndOfFileToken,
    SingleLineCommentTrivia,
    MultiLineCommentTrivia,
    NewLineTrivia,
    WhitespaceTrivia,
    ShebangTrivia,
    ConflictMarkerTrivia,
    NumericLiteral,
    BigIntLiteral,
    StringLiteral,
    JsxText,
    JsxTextAllWhiteSpaces,
    RegularExpressionLiteral,
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
    QuestionDotToken,
    LessThanToken,
    LessThanSlashToken,
    GreaterThanToken,
    LessThanEqualsToken,
    GreaterThanEqualsToken,
    EqualsEqualsToken,
    ExclamationEqualsToken,
    EqualsEqualsEqualsToken,
    ExclamationEqualsEqualsToken,
    EqualsGreaterThanToken,
    PlusToken,
    MinusToken,
    AsteriskToken,
    AsteriskAsteriskToken,
    SlashToken,
    PercentToken,
    PlusPlusToken,
    MinusMinusToken,
    LessThanLessThanToken,
    GreaterThanGreaterThanToken,
    GreaterThanGreaterThanGreaterThanToken,
    AmpersandToken,
    BarToken,
    CaretToken,
    ExclamationToken,
    TildeToken,
    AmpersandAmpersandToken,
    BarBarToken,
    QuestionToken,
    ColonToken,
    AtToken,
    QuestionQuestionToken,
    BacktickToken,
    HashToken,
    EqualsToken,
    PlusEqualsToken,
    MinusEqualsToken,
    AsteriskEqualsToken,
    AsteriskAsteriskEqualsToken,
    SlashEqualsToken,
    PercentEqualsToken,
    LessThanLessThanEqualsToken,
    GreaterThanGreaterThanEqualsToken,
    GreaterThanGreaterThanGreaterThanEqualsToken,
    AmpersandEqualsToken,
    BarEqualsToken,
    BarBarEqualsToken,
    AmpersandAmpersandEqualsToken,
    QuestionQuestionEqualsToken,
    CaretEqualsToken,
    Identifier,
    PrivateIdentifier,
    BreakKeyword,
    CaseKeyword,
    CatchKeyword,
    ClassKeyword,
    ConstKeyword,
    ContinueKeyword,
    DebuggerKeyword,
    DefaultKeyword,
    DeleteKeyword,
    DoKeyword,
    ElseKeyword,
    EnumKeyword,
    ExportKeyword,
    ExtendsKeyword,
    FalseKeyword,
    FinallyKeyword,
    ForKeyword,
    FunctionKeyword,
    IfKeyword,
    ImportKeyword,
    InKeyword,
    InstanceOfKeyword,
    NewKeyword,
    NullKeyword,
    ReturnKeyword,
    SuperKeyword,
    SwitchKeyword,
    ThisKeyword,
    ThrowKeyword,
    TrueKeyword,
    TryKeyword,
    TypeOfKeyword,
    VarKeyword,
    VoidKeyword,
    WhileKeyword,
    WithKeyword,
    ImplementsKeyword,
    InterfaceKeyword,
    LetKeyword,
    PackageKeyword,
    PrivateKeyword,
    ProtectedKeyword,
    PublicKeyword,
    StaticKeyword,
    YieldKeyword,
    AbstractKeyword,
    AsKeyword,
    AssertsKeyword,
    AssertKeyword,
    AnyKeyword,
    AsyncKeyword,
    AwaitKeyword,
    BooleanKeyword,
    ConstructorKeyword,
    DeclareKeyword,
    GetKeyword,
    InferKeyword,
    IntrinsicKeyword,
    IsKeyword,
    KeyOfKeyword,
    ModuleKeyword,
    NamespaceKeyword,
    NeverKeyword,
    ReadonlyKeyword,
    RequireKeyword,
    NumberKeyword,
    ObjectKeyword,
    SetKeyword,
    StringKeyword,
    SymbolKeyword,
    TypeKeyword,
    UndefinedKeyword,
    UniqueKeyword,
    UnknownKeyword,
    FromKeyword,
    GlobalKeyword,
    BigIntKeyword,
    OverrideKeyword,
    OfKeyword,

    QualifiedName,
    ComputedPropertyName,
    TypeParameter,
    Parameter,
    Decorator,
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
    TypeQuery,
    TypeLiteral,
    ArrayType,
    TupleType,
    OptionalType,
    RestType,
    UnionType,
    IntersectionType,
    ConditionalType,
    InferType,
    ParenthesizedType,
    ThisType,
    TypeOperator,
    IndexedAccessType,
    MappedType,
    LiteralType,
    NamedTupleMember,
    TemplateLiteralType,
    TemplateLiteralTypeSpan,
    ImportType,
    ObjectBindingPattern,
    ArrayBindingPattern,
    BindingElement,
    ArrayLiteralExpression,
    ObjectLiteralExpression,
    PropertyAccessExpression,
    ElementAccessExpression,
    CallExpression,
    NewExpression,
    TaggedTemplateExpression,
    TypeAssertionExpression,
    ParenthesizedExpression,
    FunctionExpression,
    ArrowFunction,
    DeleteExpression,
    TypeOfExpression,
    VoidExpression,
    AwaitExpression,
    PrefixUnaryExpression,
    PostfixUnaryExpression,
    BinaryExpression,
    ConditionalExpression,
    TemplateExpression,
    YieldExpression,
    SpreadElement,
    ClassExpression,
    OmittedExpression,
    ExpressionWithTypeArguments,
    AsExpression,
    NonNullExpression,
    MetaProperty,
    SyntheticExpression,

    TemplateSpan,
    SemicolonClassElement,
    Block,
    EmptyStatement,
    VariableStatement,
    ExpressionStatement,
    IfStatement,
    DoStatement,
    WhileStatement,
    ForStatement,
    ForInStatement,
    ForOfStatement,
    ContinueStatement,
    BreakStatement,
    ReturnStatement,
    WithStatement,
    SwitchStatement,
    LabeledStatement,
    ThrowStatement,
    TryStatement,
    DebuggerStatement,
    VariableDeclaration,
    VariableDeclarationList,
    FunctionDeclaration,
    ClassDeclaration,
    InterfaceDeclaration,
    TypeAliasDeclaration,
    EnumDeclaration,
    ModuleDeclaration,
    ModuleBlock,
    CaseBlock,
    NamespaceExportDeclaration,
    ImportEqualsDeclaration,
    ImportDeclaration,
    ImportClause,
    NamespaceImport,
    NamedImports,
    ImportSpecifier,
    ExportAssignment,
    ExportDeclaration,
    NamedExports,
    NamespaceExport,
    ExportSpecifier,
    MissingDeclaration,

    ExternalModuleReference,

    JsxElement,
    JsxSelfClosingElement,
    JsxOpeningElement,
    JsxClosingElement,
    JsxFragment,
    JsxOpeningFragment,
    JsxClosingFragment,
    JsxAttribute,
    JsxAttributes,
    JsxSpreadAttribute,
    JsxExpression,

    CaseClause,
    DefaultClause,
    HeritageClause,
    CatchClause,
    AssertClause,
    AssertEntry,

    PropertyAssignment,
    ShorthandPropertyAssignment,
    SpreadAssignment,

    EnumMember,
    UnparsedPrologue,
    UnparsedPrepend,
    UnparsedText,
    UnparsedInternalText,
    UnparsedSyntheticReference,

    SourceFile,
    Bundle,
    UnparsedSource,
    InputFiles,

    JSDocTypeExpression,
    JSDocNameReference,
    JSDocMemberName,
    JSDocAllType,
    JSDocUnknownType,
    JSDocNullableType,
    JSDocNonNullableType,
    JSDocOptionalType,
    JSDocFunctionType,
    JSDocVariadicType,
    JSDocNamepathType,
    JSDocComment,
    JSDocText,
    JSDocTypeLiteral,
    JSDocSignature,
    JSDocLink,
    JSDocLinkCode,
    JSDocLinkPlain,
    JSDocTag,
    JSDocAugmentsTag,
    JSDocImplementsTag,
    JSDocAuthorTag,
    JSDocDeprecatedTag,
    JSDocClassTag,
    JSDocPublicTag,
    JSDocPrivateTag,
    JSDocProtectedTag,
    JSDocReadonlyTag,
    JSDocOverrideTag,
    JSDocCallbackTag,
    JSDocEnumTag,
    JSDocParameterTag,
    JSDocReturnTag,
    JSDocThisTag,
    JSDocTypeTag,
    JSDocTemplateTag,
    JSDocTypedefTag,
    JSDocSeeTag,
    JSDocPropertyTag,

    SyntaxList,

    NotEmittedStatement,
    PartiallyEmittedExpression,
    CommaListExpression,
    MergeDeclarationMarker,
    EndOfDeclarationMarker,
    SyntheticReferenceExpression,

    Count,
}

impl SyntaxKind {
    pub const FirstAssignment: SyntaxKind = SyntaxKind::EqualsToken;
    pub const LastAssignment: SyntaxKind = SyntaxKind::CaretEqualsToken;
    pub const FirstCompoundAssignment: SyntaxKind = SyntaxKind::PlusEqualsToken;
    pub const LastCompoundAssignment: SyntaxKind = SyntaxKind::CaretEqualsToken;
    pub const FirstReservedWord: SyntaxKind = SyntaxKind::BreakKeyword;
    pub const LastReservedWord: SyntaxKind = SyntaxKind::WithKeyword;
    pub const FirstKeyword: SyntaxKind = SyntaxKind::BreakKeyword;
    pub const LastKeyword: SyntaxKind = SyntaxKind::OfKeyword;
    pub const FirstFutureReservedWord: SyntaxKind = SyntaxKind::ImplementsKeyword;
    pub const LastFutureReservedWord: SyntaxKind = SyntaxKind::YieldKeyword;
    pub const FirstTypeNode: SyntaxKind = SyntaxKind::TypePredicate;
    pub const LastTypeNode: SyntaxKind = SyntaxKind::ImportType;
    pub const FirstPunctuation: SyntaxKind = SyntaxKind::OpenBraceToken;
    pub const LastPunctuation: SyntaxKind = SyntaxKind::CaretEqualsToken;
    pub const FirstToken: SyntaxKind = SyntaxKind::Unknown;
    pub const LastToken: SyntaxKind = SyntaxKind::LastKeyword;
    pub const FirstTriviaToken: SyntaxKind = SyntaxKind::SingleLineCommentTrivia;
    pub const LastTriviaToken: SyntaxKind = SyntaxKind::ConflictMarkerTrivia;
    pub const FirstLiteralToken: SyntaxKind = SyntaxKind::NumericLiteral;
    pub const LastLiteralToken: SyntaxKind = SyntaxKind::NoSubstitutionTemplateLiteral;
    pub const FirstTemplateToken: SyntaxKind = SyntaxKind::NoSubstitutionTemplateLiteral;
    pub const LastTemplateToken: SyntaxKind = SyntaxKind::TemplateTail;
    pub const FirstBinaryOperator: SyntaxKind = SyntaxKind::LessThanToken;
    pub const LastBinaryOperator: SyntaxKind = SyntaxKind::CaretEqualsToken;
    pub const FirstStatement: SyntaxKind = SyntaxKind::VariableStatement;
    pub const LastStatement: SyntaxKind = SyntaxKind::DebuggerStatement;
    pub const FirstNode: SyntaxKind = SyntaxKind::QualifiedName;
    pub const FirstJSDocNode: SyntaxKind = SyntaxKind::JSDocTypeExpression;
    pub const LastJSDocNode: SyntaxKind = SyntaxKind::JSDocPropertyTag;
    pub const FirstJSDocTagNode: SyntaxKind = SyntaxKind::JSDocTag;
    pub const LastJSDocTagNode: SyntaxKind = SyntaxKind::JSDocPropertyTag;
    pub(crate) const FirstContextualKeyword: SyntaxKind = SyntaxKind::AbstractKeyword;
    pub(crate) const LastContextualKeyword: SyntaxKind = SyntaxKind::OfKeyword;
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
        self.id.get()
    }

    fn id(&self) -> NodeId {
        self.id.get().unwrap()
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
