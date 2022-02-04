#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::ops::Deref;
use std::rc::{Rc, Weak};

use super::{
    ArrayBindingPattern, ArrayLiteralExpression, ArrayTypeNode, ArrowFunction, AsExpression,
    AssertClause, AssertEntry, AwaitExpression, BaseJSDocUnaryType, BigIntLiteral,
    BinaryExpression, BindingElement, Block, BreakStatement, CallExpression,
    CallSignatureDeclaration, CaseBlock, ClassDeclaration, ClassExpression,
    ClassStaticBlockDeclaration, ComputedPropertyName, ConditionalExpression, ConditionalTypeNode,
    ConstructSignatureDeclaration, ConstructorDeclaration, ConstructorTypeNode, ContinueStatement,
    DebuggerStatement, Decorator, DeleteExpression, DoStatement, ElementAccessExpression, EmitNode,
    EmptyStatement, EnumDeclaration, EnumMember, ExportAssignment, ExportDeclaration,
    ExportSpecifier, ExpressionStatement, ExpressionWithTypeArguments, ForInStatement,
    ForOfStatement, ForStatement, FunctionDeclaration, FunctionExpression,
    FunctionLikeDeclarationInterface, FunctionTypeNode, GetAccessorDeclaration,
    HasElementsInterface, HasExpressionInterface, HasIsTypeOnlyInterface,
    HasQuestionDotTokenInterface, HasTypeArgumentsInterface, HasTypeParametersInterface,
    Identifier, IfStatement, ImportClause, ImportDeclaration, ImportEqualsDeclaration,
    ImportSpecifier, ImportTypeNode, IndexSignatureDeclaration, IndexedAccessTypeNode,
    InferTypeNode, InterfaceDeclaration, IntersectionTypeNode, JSDoc, JSDocLink, JSDocLinkCode,
    JSDocLinkLikeInterface, JSDocLinkPlain, JSDocMemberName, JSDocPropertyLikeTag, JSDocTag,
    JSDocTemplateTag, JSDocText, JSDocTypeExpression, JSDocTypeLikeTagInterface, JSDocTypedefTag,
    JsxAttribute, JsxText, KeywordTypeNode, LabeledStatement, LiteralLikeNodeInterface,
    LiteralTypeNode, MappedTypeNode, MemberNameInterface, MetaProperty, MethodDeclaration,
    MethodSignature, ModifiersArray, ModuleBlock, ModuleDeclaration, NamedDeclarationInterface,
    NamedImports, NamedTupleMember, NamespaceExport, NamespaceExportDeclaration, NamespaceImport,
    NewExpression, NodeArray, NonNullExpression, NumericLiteral, ObjectBindingPattern,
    ObjectLiteralExpression, OmittedExpression, OptionalTypeNode, ParameterDeclaration,
    ParenthesizedExpression, ParenthesizedTypeNode, PartiallyEmittedExpression,
    PostfixUnaryExpression, PrefixUnaryExpression, PrivateIdentifier, PropertyAccessExpression,
    PropertyAssignment, PropertyDeclaration, PropertySignature, QualifiedName,
    RegularExpressionLiteral, RestTypeNode, ReturnStatement, SemicolonClassElement,
    SetAccessorDeclaration, ShorthandPropertyAssignment, SignatureDeclarationBase,
    SignatureDeclarationInterface, SourceFile, SpreadAssignment, SpreadElement, StringLiteral,
    SwitchStatement, Symbol, SymbolTable, TaggedTemplateExpression, TemplateExpression,
    TemplateLiteralLikeNode, TemplateLiteralLikeNodeInterface, TemplateLiteralTypeNode,
    TemplateLiteralTypeSpan, TemplateSpan, ThisTypeNode, ThrowStatement, TransformFlags,
    TryStatement, TupleTypeNode, TypeAliasDeclaration, TypeAssertion, TypeElement, TypeLiteralNode,
    TypeOfExpression, TypeOperatorNode, TypeParameterDeclaration, TypePredicateNode, TypeQueryNode,
    TypeReferenceNode, UnionOrIntersectionTypeNodeInterface, UnionTypeNode, VariableDeclaration,
    VariableDeclarationList, VariableLikeDeclarationInterface, VariableStatement, VoidExpression,
    WhileStatement, WithStatement, YieldExpression,
};
use local_macros::{ast_type, enum_unwrapped};

#[derive(Clone, Debug, Eq, PartialEq)]
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

impl Deref for Path {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
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
