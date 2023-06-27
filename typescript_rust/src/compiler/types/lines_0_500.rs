use std::{cell::Cell, ops::Deref};

use gc::{unsafe_empty_trace, Finalize, Trace};

#[derive(Clone, Debug, Eq, PartialEq, Hash, Finalize)]
pub struct Path(String);

impl Path {
    pub fn new(string: String) -> Self {
        string.into()
    }

    pub fn into_string(self) -> String {
        self.0
    }
}

unsafe impl Trace for Path {
    unsafe_empty_trace!();
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

impl From<String> for Path {
    fn from(value: String) -> Self {
        Self(value)
    }
}

pub trait ReadonlyTextRange {
    fn pos(&self) -> isize;
    fn set_pos(&self, pos: isize);
    fn end(&self) -> isize;
    fn set_end(&self, end: isize);
}

#[derive(Copy, Clone)]
pub struct ReadonlyTextRangeConcrete {
    pos: isize,
    end: isize,
}

impl ReadonlyTextRangeConcrete {
    pub fn new(pos: isize, end: isize) -> Self {
        Self { pos, end }
    }

    pub fn from_text_range(text_range: &impl TextRange) -> Self {
        Self::new(text_range.pos(), text_range.end())
    }
}

impl ReadonlyTextRange for ReadonlyTextRangeConcrete {
    fn pos(&self) -> isize {
        self.pos
    }

    fn set_pos(&self, _pos: isize) {
        unreachable!()
    }

    fn end(&self) -> isize {
        self.end
    }

    fn set_end(&self, _end: isize) {
        unreachable!()
    }
}

impl<TReadonlyTextRange: ReadonlyTextRange + ?Sized> From<&TReadonlyTextRange>
    for ReadonlyTextRangeConcrete
{
    fn from(value: &TReadonlyTextRange) -> Self {
        Self::new(value.pos(), value.end())
    }
}

impl From<BaseTextRange> for ReadonlyTextRangeConcrete {
    fn from(value: BaseTextRange) -> Self {
        Self::new(value.pos(), value.end())
    }
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

    pub fn to_readonly_text_range(&self) -> impl ReadonlyTextRange {
        ReadonlyTextRangeConcrete::new(self.pos(), self.end())
    }

    pub fn into_readonly_text_range(self) -> impl ReadonlyTextRange {
        ReadonlyTextRangeConcrete::new(self.pos(), self.end())
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

impl<TReadonlyTextRange: ReadonlyTextRange> From<&TReadonlyTextRange> for BaseTextRange {
    fn from(value: &TReadonlyTextRange) -> Self {
        Self::new(value.pos(), value.end())
    }
}

// impl<TTextRange: TextRange> ReadonlyTextRange for TTextRange {
//     fn pos(&self) -> isize {
//         TextRange::pos(self)
//     }

//     fn set_pos(&self, pos: isize) {
//         TextRange::set_pos(self, pos);
//     }

//     fn end(&self) -> isize {
//         TextRange::end(self)
//     }

//     fn set_end(&self, end: isize) {
//         TextRange::set_pos(self, end);
//     }
// }

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

impl Finalize for SyntaxKind {}
unsafe impl Trace for SyntaxKind {
    unsafe_empty_trace!();
}
