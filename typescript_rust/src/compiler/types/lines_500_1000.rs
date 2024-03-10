use std::{
    cell::{Cell, RefCell},
    fmt,
};

use bitflags::bitflags;
use id_arena::Id;
use local_macros::{ast_type, enum_unwrapped};

use super::{
    ArrayBindingPattern, ArrayLiteralExpression, ArrayTypeNode, ArrowFunction, AsExpression,
    AssertClause, AssertEntry, AwaitExpression, BaseJSDocTag, BaseJSDocTypeLikeTag,
    BaseJSDocUnaryType, BigIntLiteral, BinaryExpression, BindingElement, Block, BreakStatement,
    Bundle, CallExpression, CallSignatureDeclaration, CaseBlock, CaseClause, CatchClause,
    ClassDeclaration, ClassExpression, ClassLikeDeclarationInterface, ClassStaticBlockDeclaration,
    CommaListExpression, ComputedPropertyName, ConditionalExpression, ConditionalTypeNode,
    ConstructSignatureDeclaration, ConstructorDeclaration, ConstructorTypeNode, ContinueStatement,
    DebuggerStatement, Decorator, DefaultClause, DeleteExpression, DoStatement,
    ElementAccessExpression, EmitNode, EmptyStatement, EnumDeclaration, EnumMember,
    ExportAssignment, ExportDeclaration, ExportSpecifier, ExpressionStatement,
    ExpressionWithTypeArguments, ExternalModuleReference, FlowNode, ForInStatement, ForOfStatement,
    ForStatement, FunctionDeclaration, FunctionExpression, FunctionLikeDeclarationInterface,
    FunctionTypeNode, GetAccessorDeclaration, HasConditionInterface, HasElementsInterface,
    HasExpressionInterface, HasIsTypeOnlyInterface, HasJSDocDotPosInterface, HasLabelInterface,
    HasPropertiesInterface, HasPropertyNameInterface, HasQuestionDotTokenInterface,
    HasQuestionTokenInterface, HasStatementInterface, HasStatementsInterface,
    HasTypeArgumentsInterface, HasTypeParametersInterface, HeritageClause, Identifier, IfStatement,
    ImportClause, ImportDeclaration, ImportEqualsDeclaration, ImportSpecifier, ImportTypeNode,
    IndexSignatureDeclaration, IndexedAccessTypeNode, InferTypeNode, InputFiles,
    InterfaceDeclaration, InterfaceOrClassLikeDeclarationInterface, IntersectionTypeNode, JSDoc,
    JSDocAugmentsTag, JSDocCallbackTag, JSDocFunctionType, JSDocImplementsTag, JSDocLink,
    JSDocLinkCode, JSDocLinkLikeInterface, JSDocLinkPlain, JSDocMemberName, JSDocNameReference,
    JSDocPropertyLikeTag, JSDocSeeTag, JSDocSignature, JSDocTagInterface, JSDocTemplateTag,
    JSDocText, JSDocTypeExpression, JSDocTypeLikeTagInterface, JSDocTypeLiteral,
    JSDocTypedefOrCallbackTagInterface, JSDocTypedefTag, JsxAttribute, JsxAttributes,
    JsxClosingElement, JsxClosingFragment, JsxElement, JsxExpression, JsxFragment,
    JsxOpeningElement, JsxOpeningFragment, JsxSelfClosingElement, JsxSpreadAttribute, JsxText,
    KeywordTypeNode, LabeledStatement, LiteralLikeNodeInterface, LiteralTypeNode, MappedTypeNode,
    MemberNameInterface, MetaProperty, MethodDeclaration, MethodSignature, MissingDeclaration,
    ModifiersArray, ModuleBlock, ModuleDeclaration, NamedDeclarationInterface, NamedExports,
    NamedImports, NamedTupleMember, NamespaceExport, NamespaceExportDeclaration, NamespaceImport,
    NewExpression, NodeArray, NonNullExpression, NumericLiteral, ObjectBindingPattern,
    ObjectLiteralExpression, OmittedExpression, OptionalTypeNode, ParameterDeclaration,
    ParenthesizedExpression, ParenthesizedTypeNode, PartiallyEmittedExpression,
    PostfixUnaryExpression, PrefixUnaryExpression, PrivateIdentifier, PropertyAccessExpression,
    PropertyAssignment, PropertyDeclaration, PropertySignature, QualifiedName, ReadonlyTextRange,
    RegularExpressionLiteral, RestTypeNode, ReturnStatement, SemicolonClassElement,
    SetAccessorDeclaration, ShorthandPropertyAssignment, SignatureDeclarationBase,
    SignatureDeclarationInterface, SourceFile, SpreadAssignment, SpreadElement, StringLiteral,
    SwitchStatement, Symbol, SymbolTable, SyntaxKind, SyntaxList, TaggedTemplateExpression,
    TemplateExpression, TemplateLiteralLikeNode, TemplateLiteralLikeNodeInterface,
    TemplateLiteralTypeNode, TemplateLiteralTypeSpan, TemplateSpan, ThisTypeNode, ThrowStatement,
    TransformFlags, TryStatement, TupleTypeNode, Type, TypeAliasDeclaration, TypeAssertion,
    TypeLiteralNode, TypeOfExpression, TypeOperatorNode, TypeParameterDeclaration,
    TypePredicateNode, TypeQueryNode, TypeReferenceNode, UnaryExpressionInterface,
    UnionOrIntersectionTypeNodeInterface, UnionTypeNode, UnparsedPrepend, UnparsedPrologue,
    UnparsedSource, UnparsedTextLike, VariableDeclaration, VariableDeclarationList,
    VariableLikeDeclarationInterface, VariableStatement, VoidExpression, WhileStatement,
    WithStatement, YieldExpression,
};
use crate::{
    add_emit_flags, add_emit_helpers, add_synthetic_leading_comment,
    add_synthetic_trailing_comment, get_emit_flags, impl_has_arena, move_synthetic_comments,
    remove_all_comments, set_comment_range, set_emit_flags, set_original_node,
    set_parent_recursive, set_source_map_range, set_text_range_end, set_text_range_id_node,
    set_text_range_pos, start_on_new_line, AllArenas, CaseOrDefaultClauseInterface, EmitFlags,
    EmitHelper, HasArena, HasArgumentsInterface, HasAssertClauseInterface, HasChildrenInterface,
    HasDotDotDotTokenInterface, HasFileNameInterface, HasLeftAndRightInterface,
    HasMembersInterface, HasModuleSpecifierInterface, HasOldFileOfCurrentEmitInterface,
    HasTagNameInterface, HasTextsInterface, InArena, InferenceContext, JSDocHeritageTagInterface,
    JsxOpeningLikeElementInterface, SourceFileLike, SourceMapRange, SyntheticExpression,
    SyntheticReferenceExpression, UnparsedSyntheticReference,
};

bitflags! {
    #[derive(Default)]
    pub struct NodeFlags: u32 {
        const None = 0;
        const Let = 1 << 0;
        const Const = 1 << 1;
        const NestedNamespace = 1 << 2;
        const Synthesized = 1 << 3;
        const Namespace = 1 << 4;
        const OptionalChain = 1 << 5;
        const ExportContext = 1 << 6;
        const ContainsThis = 1 << 7;
        const HasImplicitReturn = 1 << 8;
        const HasExplicitReturn = 1 << 9;
        const GlobalAugmentation = 1 << 10;
        const HasAsyncFunctions = 1 << 11;
        const DisallowInContext = 1 << 12;
        const YieldContext = 1 << 13;
        const DecoratorContext = 1 << 14;
        const AwaitContext = 1 << 15;
        const ThisNodeHasError = 1 << 16;
        const JavaScriptFile = 1 << 17;
        const ThisNodeOrAnySubNodesHasError = 1 << 18;
        const HasAggregatedChildData = 1 << 19;

        const PossiblyContainsDynamicImport = 1 << 20;
        const PossiblyContainsImportMeta = 1 << 21;

        const JSDoc = 1 << 21;
        const Ambient = 1 << 23;
        const InWithStatement = 1 << 24;
        const JsonFile = 1 << 25;
        const TypeCached = 1 << 26;
        const Deprecated = 1 << 27;

        const BlockScoped = Self::Let.bits | Self::Const.bits;

        const ReachabilityCheckFlags = Self::HasImplicitReturn.bits | Self::HasExplicitReturn.bits;
        const ReachabilityAndEmitFlags = Self::ReachabilityCheckFlags.bits | Self::HasAsyncFunctions.bits;

        const ContextFlags = Self::DisallowInContext.bits | Self::YieldContext.bits | Self::DecoratorContext.bits | Self::AwaitContext.bits | Self::JavaScriptFile.bits | Self::InWithStatement.bits | Self::Ambient.bits;

        const TypeExcludesFlags = Self::YieldContext.bits | Self::AwaitContext.bits;

        const PermanentlySetIncrementalFlags = Self::PossiblyContainsDynamicImport.bits | Self::PossiblyContainsImportMeta.bits;
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
        const HasComputedJSDocModifiers = 1 << 12;

        const Deprecated = 1 << 13;
        const Override = 1 << 14;
        const HasComputedFlags = 1 << 29;

        const AccessibilityModifier = Self::Public.bits | Self::Private.bits | Self::Protected.bits;
        const ParameterPropertyModifier = Self::AccessibilityModifier.bits | Self::Readonly.bits | Self::Override.bits;
        const NonPublicAccessibilityModifier = Self::Private.bits | Self::Protected.bits;

        const TypeScriptModifier = Self::Ambient.bits | Self::Public.bits | Self::Private.bits | Self::Protected.bits | Self::Readonly.bits | Self::Abstract.bits | Self::Const.bits | Self::Override.bits;
        const ExportDefault = Self::Export.bits | Self::Default.bits;
        const All = Self::Export.bits | Self::Ambient.bits | Self::Public.bits | Self::Private.bits | Self::Protected.bits | Self::Static.bits | Self::Readonly.bits | Self::Abstract.bits | Self::Async.bits | Self::Default.bits | Self::Const.bits | Self::Deprecated.bits | Self::Override.bits;
    }
}

bitflags! {
    pub struct JsxFlags: u32 {
        const None = 0;
        const IntrinsicNamedElement = 1 << 0;
        const IntrinsicIndexedElement = 1 << 1;

        const IntrinsicElement = Self::IntrinsicNamedElement.bits | Self::IntrinsicIndexedElement.bits;
    }
}

bitflags! {
    pub struct RelationComparisonResult: u32 {
        const None = 0;
        const Succeeded = 1 << 0;
        const Failed = 1 << 1;
        const Reported = 1 << 2;

        const ReportsUnmeasurable = 1 << 3;
        const ReportsUnreliable = 1 << 4;
        const ReportsMask = Self::ReportsUnmeasurable.bits | Self::ReportsUnreliable.bits;
    }
}

pub type NodeId = usize;

pub trait NodeInterface: ReadonlyTextRange {
    fn arena_id(&self) -> Id<Node>;
    fn set_arena_id(&self, id: Id<Node>);
    fn alloc(self, arena: &AllArenas) -> Id<Node>;
    fn base_node(&self) -> &BaseNode;
    fn kind(&self) -> SyntaxKind;
    fn modifier_flags_cache(&self) -> ModifierFlags;
    fn set_modifier_flags_cache(&self, flags: ModifierFlags);
    fn flags(&self) -> NodeFlags;
    fn set_flags(&self, flags: NodeFlags);
    fn transform_flags(&self) -> TransformFlags;
    fn set_transform_flags(&self, flags: TransformFlags);
    fn add_transform_flags(&self, flags: TransformFlags);
    fn maybe_decorators(&self) -> Option<Id<NodeArray>>;
    fn set_decorators(&self, decorators: Option<Id<NodeArray>>);
    fn maybe_modifiers(&self) -> Option<Id<NodeArray>>;
    fn set_modifiers(&self, modifiers: Option<Id<NodeArray>>);
    fn maybe_id(&self) -> Option<NodeId>;
    fn id(&self) -> NodeId;
    fn set_id(&self, id: NodeId);
    fn set_id_override(&self, id_override: Id<Box<dyn NodeIdOverride>>);
    fn maybe_parent(&self) -> Option<Id<Node>>;
    fn parent(&self) -> Id<Node>;
    fn set_parent(&self, parent: Option<Id<Node>>);
    fn maybe_original(&self) -> Option<Id<Node>>;
    fn set_original(&self, original: Option<Id<Node>>);
    fn maybe_symbol(&self) -> Option<Id<Symbol>>;
    fn symbol(&self) -> Id<Symbol>;
    fn set_symbol(&self, symbol: Id<Symbol>);
    fn set_symbol_override(&self, symbol_override: Id<Box<dyn NodeSymbolOverride>>);
    fn maybe_locals(&self) -> Option<Id<SymbolTable>>;
    fn maybe_locals_mut(&self) -> debug_cell::RefMut<Option<Id<SymbolTable>>>;
    fn locals(&self) -> Id<SymbolTable>;
    fn locals_mut(&self) -> debug_cell::RefMut<Id<SymbolTable>>;
    fn set_locals(&self, locals: Option<Id<SymbolTable>>);
    fn maybe_next_container(&self) -> Option<Id<Node>>;
    fn set_next_container(&self, next_container: Option<Id<Node>>);
    fn maybe_local_symbol(&self) -> Option<Id<Symbol>>;
    fn set_local_symbol(&self, local_symbol: Option<Id<Symbol>>);
    fn maybe_flow_node(&self) -> Option<Id<FlowNode>>;
    fn set_flow_node(&self, emit_node: Option<Id<FlowNode>>);
    fn maybe_emit_node(&self) -> Option<Id<EmitNode>>;
    fn set_emit_node(&self, emit_node: Option<Id<EmitNode>>);
    fn maybe_contextual_type(&self) -> Option<Id<Type>>;
    fn set_contextual_type(&self, contextual_type: Option<Id<Type>>);
    fn maybe_inference_context(&self) -> Option<Id<InferenceContext>>;
    fn set_inference_context(&self, inference_context: Option<Id<InferenceContext>>);
    fn maybe_js_doc(&self) -> Option<Vec<Id<Node /*JSDoc*/>>>;
    fn set_js_doc(&self, js_doc: Option<Vec<Id<Node /*JSDoc*/>>>);
    fn maybe_js_doc_cache(&self) -> Option<Id<Vec<Id<Node /*JSDocTag*/>>>>;
    fn set_js_doc_cache(&self, js_doc_cache: Option<Id<Vec<Id<Node /*JSDocTag*/>>>>);
    // IncrementalElement
    fn maybe_intersects_change(&self) -> Option<bool>;
    fn set_intersects_change(&self, intersects_change: Option<bool>);
    // fn maybe_length(&self) -> Option<usize>;
    // fn set_length(&self, length: Option<usize>);
    // _children: Node[] | undefined;
    // IncrementalNode
    // hasBeenIncrementallyParsed: boolean;
}

pub trait NodeIdOverride: fmt::Debug {
    fn maybe_id(&self) -> Option<NodeId>;
    fn set_id(&self, id: NodeId);
}

pub trait NodeSymbolOverride: fmt::Debug {
    fn maybe_symbol(&self) -> Option<Id<Symbol>>;
    fn set_symbol(&self, symbol: Id<Symbol>);
}

#[derive(Debug)]
#[ast_type]
pub enum Node {
    BaseNode(BaseNode),
    TypeParameterDeclaration(TypeParameterDeclaration),
    Decorator(Decorator),
    SignatureDeclarationBase(SignatureDeclarationBase),
    VariableDeclaration(VariableDeclaration),
    VariableDeclarationList(VariableDeclarationList),
    ParameterDeclaration(ParameterDeclaration),
    TemplateSpan(TemplateSpan),
    PropertySignature(PropertySignature),
    PropertyAssignment(PropertyAssignment),
    SourceFile(SourceFile),
    QualifiedName(QualifiedName),
    BaseJSDocTag(BaseJSDocTag),
    JSDocAugmentsTag(JSDocAugmentsTag),
    JSDocImplementsTag(JSDocImplementsTag),
    BaseJSDocTypeLikeTag(BaseJSDocTypeLikeTag),
    JSDocTemplateTag(JSDocTemplateTag),
    JSDocSeeTag(JSDocSeeTag),
    JSDocTypedefTag(JSDocTypedefTag),
    JSDocCallbackTag(JSDocCallbackTag),
    JSDocPropertyLikeTag(JSDocPropertyLikeTag),
    BindingElement(BindingElement),
    PropertyDeclaration(PropertyDeclaration),
    EnumMember(EnumMember),
    JsxAttribute(JsxAttribute),
    JSDoc(JSDoc),
    ShorthandPropertyAssignment(ShorthandPropertyAssignment),
    ObjectBindingPattern(ObjectBindingPattern),
    ArrayBindingPattern(ArrayBindingPattern),
    JSDocMemberName(JSDocMemberName),
    JSDocText(JSDocText),
    JSDocLink(JSDocLink),
    JSDocLinkCode(JSDocLinkCode),
    JSDocLinkPlain(JSDocLinkPlain),
    SpreadAssignment(SpreadAssignment),
    JsxText(JsxText),
    PartiallyEmittedExpression(PartiallyEmittedExpression),
    Identifier(Identifier),
    PrefixUnaryExpression(PrefixUnaryExpression),
    BinaryExpression(BinaryExpression),
    TemplateExpression(TemplateExpression),
    ParenthesizedExpression(ParenthesizedExpression),
    ArrayLiteralExpression(ArrayLiteralExpression),
    ObjectLiteralExpression(ObjectLiteralExpression),
    AsExpression(AsExpression),
    TypeAssertion(TypeAssertion),
    NonNullExpression(NonNullExpression),
    CallExpression(CallExpression),
    PropertyAccessExpression(PropertyAccessExpression),
    ElementAccessExpression(ElementAccessExpression),
    VoidExpression(VoidExpression),
    NewExpression(NewExpression),
    PostfixUnaryExpression(PostfixUnaryExpression),
    ConditionalExpression(ConditionalExpression),
    TaggedTemplateExpression(TaggedTemplateExpression),
    FunctionExpression(FunctionExpression),
    SpreadElement(SpreadElement),
    StringLiteral(StringLiteral),
    TemplateLiteralLikeNode(TemplateLiteralLikeNode),
    NumericLiteral(NumericLiteral),
    BigIntLiteral(BigIntLiteral),
    RegularExpressionLiteral(RegularExpressionLiteral),
    FunctionDeclaration(FunctionDeclaration),
    EmptyStatement(EmptyStatement),
    Block(Block),
    VariableStatement(VariableStatement),
    ExpressionStatement(ExpressionStatement),
    IfStatement(IfStatement),
    ReturnStatement(ReturnStatement),
    InterfaceDeclaration(InterfaceDeclaration),
    TypeAliasDeclaration(TypeAliasDeclaration),
    ForStatement(ForStatement),
    ForInStatement(ForInStatement),
    ForOfStatement(ForOfStatement),
    ModuleDeclaration(ModuleDeclaration),
    LabeledStatement(LabeledStatement),
    ExportAssignment(ExportAssignment),
    ImportEqualsDeclaration(ImportEqualsDeclaration),
    ImportClause(ImportClause),
    ExportDeclaration(ExportDeclaration),
    ImportSpecifier(ImportSpecifier),
    ExportSpecifier(ExportSpecifier),
    PrivateIdentifier(PrivateIdentifier),
    ComputedPropertyName(ComputedPropertyName),
    MethodSignature(MethodSignature),
    MethodDeclaration(MethodDeclaration),
    ClassStaticBlockDeclaration(ClassStaticBlockDeclaration),
    ConstructorDeclaration(ConstructorDeclaration),
    GetAccessorDeclaration(GetAccessorDeclaration),
    SetAccessorDeclaration(SetAccessorDeclaration),
    CallSignatureDeclaration(CallSignatureDeclaration),
    ConstructSignatureDeclaration(ConstructSignatureDeclaration),
    IndexSignatureDeclaration(IndexSignatureDeclaration),
    KeywordTypeNode(KeywordTypeNode),
    UnionTypeNode(UnionTypeNode),
    IntersectionTypeNode(IntersectionTypeNode),
    LiteralTypeNode(LiteralTypeNode),
    TypeReferenceNode(TypeReferenceNode),
    TypePredicateNode(TypePredicateNode),
    TypeLiteralNode(TypeLiteralNode),
    ArrayTypeNode(ArrayTypeNode),
    JSDocTypeExpression(JSDocTypeExpression),
    FunctionTypeNode(FunctionTypeNode),
    ParenthesizedTypeNode(ParenthesizedTypeNode),
    BaseJSDocUnaryType(BaseJSDocUnaryType),
    TemplateLiteralTypeSpan(TemplateLiteralTypeSpan),
    ConstructorTypeNode(ConstructorTypeNode),
    TypeQueryNode(TypeQueryNode),
    TupleTypeNode(TupleTypeNode),
    NamedTupleMember(NamedTupleMember),
    OptionalTypeNode(OptionalTypeNode),
    RestTypeNode(RestTypeNode),
    ConditionalTypeNode(ConditionalTypeNode),
    InferTypeNode(InferTypeNode),
    TemplateLiteralTypeNode(TemplateLiteralTypeNode),
    ImportTypeNode(ImportTypeNode),
    ThisTypeNode(ThisTypeNode),
    TypeOperatorNode(TypeOperatorNode),
    IndexedAccessTypeNode(IndexedAccessTypeNode),
    MappedTypeNode(MappedTypeNode),
    ArrowFunction(ArrowFunction),
    DeleteExpression(DeleteExpression),
    TypeOfExpression(TypeOfExpression),
    AwaitExpression(AwaitExpression),
    YieldExpression(YieldExpression),
    SyntheticExpression(SyntheticExpression),
    ClassExpression(ClassExpression),
    OmittedExpression(OmittedExpression),
    ExpressionWithTypeArguments(ExpressionWithTypeArguments),
    MetaProperty(MetaProperty),
    SemicolonClassElement(SemicolonClassElement),
    DoStatement(DoStatement),
    WhileStatement(WhileStatement),
    ContinueStatement(ContinueStatement),
    BreakStatement(BreakStatement),
    WithStatement(WithStatement),
    SwitchStatement(SwitchStatement),
    ThrowStatement(ThrowStatement),
    TryStatement(TryStatement),
    DebuggerStatement(DebuggerStatement),
    SyntheticReferenceExpression(SyntheticReferenceExpression),
    ClassDeclaration(ClassDeclaration),
    EnumDeclaration(EnumDeclaration),
    ModuleBlock(ModuleBlock),
    CaseBlock(CaseBlock),
    NamespaceExportDeclaration(NamespaceExportDeclaration),
    ImportDeclaration(ImportDeclaration),
    AssertClause(AssertClause),
    AssertEntry(AssertEntry),
    NamespaceImport(NamespaceImport),
    NamespaceExport(NamespaceExport),
    NamedImports(NamedImports),
    NamedExports(NamedExports),
    MissingDeclaration(MissingDeclaration),
    ExternalModuleReference(ExternalModuleReference),
    JSDocFunctionType(JSDocFunctionType),
    JSDocTypeLiteral(JSDocTypeLiteral),
    JSDocSignature(JSDocSignature),
    JSDocNameReference(JSDocNameReference),
    JsxElement(JsxElement),
    JsxSelfClosingElement(JsxSelfClosingElement),
    JsxOpeningElement(JsxOpeningElement),
    JsxClosingElement(JsxClosingElement),
    JsxFragment(JsxFragment),
    JsxOpeningFragment(JsxOpeningFragment),
    JsxClosingFragment(JsxClosingFragment),
    JsxAttributes(JsxAttributes),
    JsxSpreadAttribute(JsxSpreadAttribute),
    JsxExpression(JsxExpression),
    CaseClause(CaseClause),
    DefaultClause(DefaultClause),
    HeritageClause(HeritageClause),
    CatchClause(CatchClause),
    Bundle(Bundle),
    UnparsedSource(UnparsedSource),
    UnparsedPrologue(UnparsedPrologue),
    UnparsedPrepend(UnparsedPrepend),
    UnparsedTextLike(UnparsedTextLike),
    InputFiles(InputFiles),
    CommaListExpression(CommaListExpression),
    SyntaxList(SyntaxList),
    UnparsedSyntheticReference(UnparsedSyntheticReference),
}

impl Node {
    pub fn maybe_as_named_declaration(&self) -> Option<&dyn NamedDeclarationInterface> {
        match self {
            Node::ImportClause(node) => Some(node),
            Node::NamespaceExportDeclaration(node) => Some(node),
            Node::ImportSpecifier(node) => Some(node),
            Node::ExportSpecifier(node) => Some(node),
            Node::JSDocNameReference(node) => Some(node),
            Node::JSDocFunctionType(node) => Some(node),
            Node::JSDocTypedefTag(node) => Some(node),
            Node::JSDocCallbackTag(node) => Some(node),
            Node::JSDocSignature(node) => Some(node),
            Node::JSDocPropertyLikeTag(node) => Some(node),
            Node::TypeParameterDeclaration(node) => Some(node),
            Node::CallSignatureDeclaration(node) => Some(node),
            Node::ConstructSignatureDeclaration(node) => Some(node),
            Node::FunctionDeclaration(node) => Some(node),
            Node::MethodSignature(node) => Some(node),
            Node::MethodDeclaration(node) => Some(node),
            Node::ConstructorDeclaration(node) => Some(node),
            Node::GetAccessorDeclaration(node) => Some(node),
            Node::SetAccessorDeclaration(node) => Some(node),
            Node::IndexSignatureDeclaration(node) => Some(node),
            Node::ClassStaticBlockDeclaration(node) => Some(node),
            Node::VariableDeclaration(node) => Some(node),
            Node::ParameterDeclaration(node) => Some(node),
            Node::FunctionTypeNode(node) => Some(node),
            Node::ConstructorTypeNode(node) => Some(node),
            Node::JsxAttribute(node) => Some(node),
            Node::BindingElement(node) => Some(node),
            Node::PropertySignature(node) => Some(node),
            Node::PropertyDeclaration(node) => Some(node),
            Node::PropertyAssignment(node) => Some(node),
            Node::ShorthandPropertyAssignment(node) => Some(node),
            Node::ClassDeclaration(node) => Some(node),
            Node::ClassExpression(node) => Some(node),
            Node::InterfaceDeclaration(node) => Some(node),
            Node::TypeAliasDeclaration(node) => Some(node),
            Node::EnumMember(node) => Some(node),
            Node::EnumDeclaration(node) => Some(node),
            Node::ModuleDeclaration(node) => Some(node),
            Node::ImportEqualsDeclaration(node) => Some(node),
            Node::FunctionExpression(node) => Some(node),
            Node::ArrowFunction(node) => Some(node),
            Node::PropertyAccessExpression(node) => Some(node),
            Node::NamespaceImport(node) => Some(node),
            Node::SpreadAssignment(node) => Some(node),
            Node::SemicolonClassElement(node) => Some(node),
            Node::NamespaceExport(node) => Some(node),
            Node::NamedTupleMember(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_named_declaration(&self) -> &dyn NamedDeclarationInterface {
        self.maybe_as_named_declaration()
            .expect("Expected named declaration")
    }

    pub fn as_member_name(&self) -> &dyn MemberNameInterface {
        match self {
            Node::Identifier(node) => node,
            Node::PrivateIdentifier(node) => node,
            _ => panic!("Expected member name"),
        }
    }

    pub fn as_literal_like_node(&self) -> &dyn LiteralLikeNodeInterface {
        match self {
            Node::StringLiteral(node) => node,
            Node::TemplateLiteralLikeNode(node) => node,
            Node::NumericLiteral(node) => node,
            Node::BigIntLiteral(node) => node,
            Node::RegularExpressionLiteral(node) => node,
            Node::JsxText(node) => node,
            _ => panic!("Expected literal like node"),
        }
    }

    pub fn maybe_as_has_initializer(&self) -> Option<&dyn HasInitializerInterface> {
        match self {
            Node::VariableDeclaration(node) => Some(node),
            Node::ParameterDeclaration(node) => Some(node),
            Node::BindingElement(node) => Some(node),
            Node::PropertySignature(node) => Some(node),
            Node::PropertyDeclaration(node) => Some(node),
            Node::PropertyAssignment(node) => Some(node),
            Node::EnumMember(node) => Some(node),
            Node::ForStatement(node) => Some(node),
            Node::ForInStatement(node) => Some(node),
            Node::ForOfStatement(node) => Some(node),
            Node::JsxAttribute(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_has_initializer(&self) -> &dyn HasInitializerInterface {
        self.maybe_as_has_initializer()
            .expect("Expected has initializer")
    }

    pub fn as_has_type_parameters(&self) -> &dyn HasTypeParametersInterface {
        match self {
            Node::ConstructorDeclaration(node) => node,
            Node::GetAccessorDeclaration(node) => node,
            Node::SetAccessorDeclaration(node) => node,
            Node::IndexSignatureDeclaration(node) => node,
            Node::ClassStaticBlockDeclaration(node) => node,
            Node::FunctionTypeNode(node) => node,
            Node::ConstructorTypeNode(node) => node,
            Node::JSDocFunctionType(node) => node,
            Node::JSDocTemplateTag(node) => node,
            Node::JSDocSignature(node) => node,
            Node::CallSignatureDeclaration(node) => node,
            Node::ConstructSignatureDeclaration(node) => node,
            Node::FunctionDeclaration(node) => node,
            Node::MethodSignature(node) => node,
            Node::MethodDeclaration(node) => node,
            Node::FunctionExpression(node) => node,
            Node::ArrowFunction(node) => node,
            Node::ClassDeclaration(node) => node,
            Node::ClassExpression(node) => node,
            Node::InterfaceDeclaration(node) => node,
            Node::TypeAliasDeclaration(node) => node,
            _ => panic!("Expected has type parameters, got {:?}", self.kind()),
        }
    }

    pub fn as_has_type_arguments(&self) -> &dyn HasTypeArgumentsInterface {
        match self {
            Node::TaggedTemplateExpression(node) => node,
            Node::TypeReferenceNode(node) => node,
            Node::JsxOpeningElement(node) => node,
            Node::JsxSelfClosingElement(node) => node,
            Node::Identifier(node) => node,
            Node::ImportTypeNode(node) => node,
            Node::CallExpression(node) => node,
            Node::ExpressionWithTypeArguments(node) => node,
            Node::NewExpression(node) => node,
            _ => panic!("Expected has type arguments, got {:?}", self.kind()),
        }
    }

    pub fn as_union_or_intersection_type_node(&self) -> &dyn UnionOrIntersectionTypeNodeInterface {
        match self {
            Node::UnionTypeNode(union_type_node) => union_type_node,
            Node::IntersectionTypeNode(intersection_type_node) => intersection_type_node,
            _ => panic!("Expected union or intersection type"),
        }
    }

    pub fn as_has_expression(&self) -> &dyn HasExpressionInterface {
        match self {
            Node::TemplateSpan(node) => node,
            Node::ParenthesizedExpression(node) => node,
            Node::SpreadElement(node) => node,
            Node::PropertyAccessExpression(node) => node,
            Node::ElementAccessExpression(node) => node,
            Node::CallExpression(node) => node,
            Node::ExpressionWithTypeArguments(node) => node,
            Node::NewExpression(node) => node,
            Node::AsExpression(node) => node,
            Node::TypeAssertion(node) => node,
            Node::NonNullExpression(node) => node,
            Node::ExternalModuleReference(node) => node,
            Node::ExportAssignment(node) => node,
            Node::PartiallyEmittedExpression(node) => node,
            Node::DeleteExpression(node) => node,
            Node::TypeOfExpression(node) => node,
            Node::VoidExpression(node) => node,
            Node::AwaitExpression(node) => node,
            Node::YieldExpression(node) => node,
            Node::ComputedPropertyName(node) => node,
            Node::Decorator(node) => node,
            Node::TypeParameterDeclaration(node) => node,
            Node::JsxSpreadAttribute(node) => node,
            Node::JsxExpression(node) => node,
            Node::ExpressionStatement(node) => node,
            Node::IfStatement(node) => node,
            Node::DoStatement(node) => node,
            Node::WhileStatement(node) => node,
            Node::ForInStatement(node) => node,
            Node::ForOfStatement(node) => node,
            Node::ReturnStatement(node) => node,
            Node::WithStatement(node) => node,
            Node::SwitchStatement(node) => node,
            Node::CaseClause(node) => node,
            Node::ThrowStatement(node) => node,
            Node::SpreadAssignment(node) => node,
            _ => panic!("Expected has expression"),
        }
    }

    pub fn as_signature_declaration(&self) -> &dyn SignatureDeclarationInterface {
        match self {
            Node::ConstructorDeclaration(node) => node,
            Node::GetAccessorDeclaration(node) => node,
            Node::SetAccessorDeclaration(node) => node,
            Node::IndexSignatureDeclaration(node) => node,
            Node::FunctionTypeNode(node) => node,
            Node::ConstructorTypeNode(node) => node,
            Node::FunctionExpression(node) => node,
            Node::ArrowFunction(node) => node,
            Node::JSDocFunctionType(node) => node,
            Node::JSDocSignature(node) => node,
            Node::CallSignatureDeclaration(node) => node,
            Node::ConstructSignatureDeclaration(node) => node,
            Node::FunctionDeclaration(node) => node,
            Node::MethodSignature(node) => node,
            Node::MethodDeclaration(node) => node,
            _ => panic!("Expected signature declaration"),
        }
    }

    pub fn maybe_as_has_type(&self) -> Option<&dyn HasTypeInterface> {
        match self {
            Node::FunctionExpression(node) => Some(node),
            Node::ArrowFunction(node) => Some(node),
            Node::AsExpression(node) => Some(node),
            Node::TypeAssertion(node) => Some(node),
            Node::PropertySignature(node) => Some(node),
            Node::PropertyDeclaration(node) => Some(node),
            Node::TypeAliasDeclaration(node) => Some(node),
            Node::JSDocTypeExpression(node) => Some(node),
            Node::BaseJSDocUnaryType(node) => Some(node),
            Node::JSDocFunctionType(node) => Some(node),
            Node::JSDocSignature(node) => Some(node),
            Node::ConstructorDeclaration(node) => Some(node),
            Node::GetAccessorDeclaration(node) => Some(node),
            Node::SetAccessorDeclaration(node) => Some(node),
            Node::IndexSignatureDeclaration(node) => Some(node),
            Node::VariableDeclaration(node) => Some(node),
            Node::ParameterDeclaration(node) => Some(node),
            Node::FunctionTypeNode(node) => Some(node),
            Node::ConstructorTypeNode(node) => Some(node),
            Node::TypePredicateNode(node) => Some(node),
            Node::NamedTupleMember(node) => Some(node),
            Node::OptionalTypeNode(node) => Some(node),
            Node::RestTypeNode(node) => Some(node),
            Node::ParenthesizedTypeNode(node) => Some(node),
            Node::TypeOperatorNode(node) => Some(node),
            Node::MappedTypeNode(node) => Some(node),
            Node::TemplateLiteralTypeSpan(node) => Some(node),
            Node::CallSignatureDeclaration(node) => Some(node),
            Node::ConstructSignatureDeclaration(node) => Some(node),
            Node::FunctionDeclaration(node) => Some(node),
            Node::MethodSignature(node) => Some(node),
            Node::MethodDeclaration(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_has_type(&self) -> &dyn HasTypeInterface {
        self.maybe_as_has_type().expect("Expected has type")
    }

    pub fn maybe_as_function_like_declaration(
        &self,
    ) -> Option<&dyn FunctionLikeDeclarationInterface> {
        match self {
            Node::ConstructorDeclaration(node) => Some(node),
            Node::GetAccessorDeclaration(node) => Some(node),
            Node::SetAccessorDeclaration(node) => Some(node),
            Node::FunctionDeclaration(node) => Some(node),
            Node::MethodDeclaration(node) => Some(node),
            Node::FunctionExpression(node) => Some(node),
            Node::ArrowFunction(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_function_like_declaration(&self) -> &dyn FunctionLikeDeclarationInterface {
        self.maybe_as_function_like_declaration()
            .expect("Expected function like declaration")
    }

    pub fn as_has_elements(&self) -> &dyn HasElementsInterface {
        match self {
            Node::ObjectBindingPattern(node) => node,
            Node::ArrayBindingPattern(node) => node,
            Node::NamedImports(node) => node,
            Node::NamedExports(node) => node,
            Node::CommaListExpression(node) => node,
            Node::AssertClause(node) => node,
            Node::TupleTypeNode(node) => node,
            Node::ArrayLiteralExpression(node) => node,
            _ => panic!("Expected has elements"),
        }
    }

    pub fn as_jsdoc_link_like(&self) -> &dyn JSDocLinkLikeInterface {
        match self {
            Node::JSDocLink(node) => node,
            Node::JSDocLinkCode(node) => node,
            Node::JSDocLinkPlain(node) => node,
            _ => panic!("Expected JSDoc link like"),
        }
    }

    pub fn as_has_question_dot_token(&self) -> &dyn HasQuestionDotTokenInterface {
        match self {
            Node::PropertyAccessExpression(node) => node,
            Node::ElementAccessExpression(node) => node,
            Node::CallExpression(node) => node,
            _ => panic!("Expected has question dot token"),
        }
    }

    pub fn as_has_is_type_only(&self) -> &dyn HasIsTypeOnlyInterface {
        match self {
            Node::ImportEqualsDeclaration(node) => node,
            Node::ImportClause(node) => node,
            Node::ExportDeclaration(node) => node,
            Node::ImportSpecifier(node) => node,
            Node::ExportSpecifier(node) => node,
            _ => panic!("Expected has is type only"),
        }
    }

    pub fn as_variable_like_declaration(&self) -> &dyn VariableLikeDeclarationInterface {
        match self {
            Node::PropertySignature(node) => node,
            Node::PropertyDeclaration(node) => node,
            Node::VariableDeclaration(node) => node,
            Node::ParameterDeclaration(node) => node,
            _ => panic!("Expected variable like declaration, got {:?}", self.kind()),
        }
    }

    pub fn as_jsdoc_type_like_tag(&self) -> &dyn JSDocTypeLikeTagInterface {
        match self {
            Node::BaseJSDocTypeLikeTag(node) => node,
            Node::JSDocCallbackTag(node) => node,
            Node::JSDocTypedefTag(node) => node,
            Node::JSDocPropertyLikeTag(node) => node,
            _ => panic!("Expected JSDoc type like tag"),
        }
    }

    pub fn as_template_literal_like_node(&self) -> &dyn TemplateLiteralLikeNodeInterface {
        match self {
            Node::TemplateLiteralLikeNode(node) => node,
            _ => panic!("Expected template literal like node"),
        }
    }

    pub fn as_jsdoc_tag(&self) -> &dyn JSDocTagInterface {
        match self {
            Node::BaseJSDocTag(node) => node,
            Node::JSDocAugmentsTag(node) => node,
            Node::JSDocImplementsTag(node) => node,
            Node::BaseJSDocTypeLikeTag(node) => node,
            Node::JSDocTemplateTag(node) => node,
            Node::JSDocSeeTag(node) => node,
            Node::JSDocTypedefTag(node) => node,
            Node::JSDocCallbackTag(node) => node,
            Node::JSDocPropertyLikeTag(node) => node,
            _ => panic!("Expected JSDoc tag"),
        }
    }

    pub fn as_has_jsdoc_dot_pos(&self) -> &dyn HasJSDocDotPosInterface {
        match self {
            Node::Identifier(node) => node,
            Node::QualifiedName(node) => node,
            _ => panic!("Expected has JSDoc dot pos"),
        }
    }

    pub fn maybe_as_interface_or_class_like_declaration(
        &self,
    ) -> Option<&dyn InterfaceOrClassLikeDeclarationInterface> {
        match self {
            Node::ClassDeclaration(node) => Some(node),
            Node::ClassExpression(node) => Some(node),
            Node::InterfaceDeclaration(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_interface_or_class_like_declaration(
        &self,
    ) -> &dyn InterfaceOrClassLikeDeclarationInterface {
        self.maybe_as_interface_or_class_like_declaration()
            .expect("Expected interface or class like declaration")
    }

    pub fn as_has_statements(&self) -> &dyn HasStatementsInterface {
        match self {
            Node::Block(node) => node,
            Node::ModuleBlock(node) => node,
            Node::SourceFile(node) => node,
            Node::CaseClause(node) => node,
            Node::DefaultClause(node) => node,
            _ => panic!("Expected has statements"),
        }
    }

    pub fn as_has_condition(&self) -> &dyn HasConditionInterface {
        match self {
            Node::ForStatement(node) => node,
            Node::ConditionalExpression(node) => node,
            _ => panic!("Expected has condition"),
        }
    }

    pub fn as_jsdoc_typedef_or_callback_tag(&self) -> &dyn JSDocTypedefOrCallbackTagInterface {
        match self {
            Node::JSDocTypedefTag(node) => node,
            Node::JSDocCallbackTag(node) => node,
            _ => panic!("Expected JSDoc typedef or callback tag"),
        }
    }

    pub fn as_has_statement(&self) -> &dyn HasStatementInterface {
        match self {
            Node::ForInStatement(node) => node,
            Node::ForOfStatement(node) => node,
            Node::ForStatement(node) => node,
            Node::DoStatement(node) => node,
            Node::WhileStatement(node) => node,
            Node::LabeledStatement(node) => node,
            Node::WithStatement(node) => node,
            _ => panic!("Expected has statement"),
        }
    }

    pub fn as_has_label(&self) -> &dyn HasLabelInterface {
        match self {
            Node::BreakStatement(node) => node,
            Node::ContinueStatement(node) => node,
            Node::LabeledStatement(node) => node,
            _ => panic!("Expected has label"),
        }
    }

    pub fn as_has_question_token(&self) -> &dyn HasQuestionTokenInterface {
        match self {
            Node::PropertySignature(node) => node,
            Node::PropertyDeclaration(node) => node,
            Node::PropertyAssignment(node) => node,
            Node::ShorthandPropertyAssignment(node) => node,
            Node::ConstructorDeclaration(node) => node,
            Node::GetAccessorDeclaration(node) => node,
            Node::SetAccessorDeclaration(node) => node,
            Node::ParameterDeclaration(node) => node,
            Node::NamedTupleMember(node) => node,
            Node::MappedTypeNode(node) => node,
            Node::ConditionalExpression(node) => node,
            Node::FunctionExpression(node) => node,
            Node::ArrowFunction(node) => node,
            Node::FunctionDeclaration(node) => node,
            Node::MethodSignature(node) => node,
            Node::MethodDeclaration(node) => node,
            _ => panic!("Expected has question token, got {:?}", self.kind()),
        }
    }

    pub fn as_class_like_declaration(&self) -> &dyn ClassLikeDeclarationInterface {
        match self {
            Node::ClassExpression(node) => node,
            Node::ClassDeclaration(node) => node,
            _ => panic!("Expected class like declaration"),
        }
    }

    pub fn as_has_property_name(&self) -> &dyn HasPropertyNameInterface {
        match self {
            Node::ImportSpecifier(node) => node,
            Node::ExportSpecifier(node) => node,
            Node::BindingElement(node) => node,
            _ => panic!("Expected has property name"),
        }
    }

    pub fn as_has_properties(&self) -> &dyn HasPropertiesInterface {
        match self {
            Node::ObjectLiteralExpression(node) => node,
            Node::JsxAttributes(node) => node,
            _ => panic!("Expected has properties"),
        }
    }

    pub fn as_jsx_opening_like_element(&self) -> &dyn JsxOpeningLikeElementInterface {
        match self {
            Node::JsxOpeningElement(node) => node,
            Node::JsxSelfClosingElement(node) => node,
            _ => panic!("Expected JSX opening like element"),
        }
    }

    pub fn as_has_arguments(&self) -> &dyn HasArgumentsInterface {
        match self {
            Node::CallExpression(node) => node,
            Node::NewExpression(node) => node,
            _ => panic!("Expected has arguments"),
        }
    }

    pub fn as_unary_expression(&self) -> &dyn UnaryExpressionInterface {
        match self {
            Node::PrefixUnaryExpression(node) => node,
            Node::PostfixUnaryExpression(node) => node,
            _ => panic!("Expected unary expression"),
        }
    }

    pub fn as_has_children(&self) -> &dyn HasChildrenInterface {
        match self {
            Node::JsxElement(node) => node,
            Node::JsxFragment(node) => node,
            _ => panic!("Expected has children"),
        }
    }

    pub fn as_has_tag_name(&self) -> &dyn HasTagNameInterface {
        match self {
            Node::JsxOpeningElement(node) => node,
            Node::JsxSelfClosingElement(node) => node,
            Node::JsxClosingElement(node) => node,
            _ => panic!("Expected has tag name"),
        }
    }

    pub fn as_has_dot_dot_dot_token(&self) -> &dyn HasDotDotDotTokenInterface {
        match self {
            Node::ParameterDeclaration(node) => node,
            Node::NamedTupleMember(node) => node,
            Node::BindingElement(node) => node,
            _ => panic!("Expected has dot dot dot token"),
        }
    }

    pub fn as_has_members(&self) -> &dyn HasMembersInterface {
        match self {
            Node::TypeLiteralNode(node) => node,
            Node::InterfaceDeclaration(node) => node,
            Node::ClassDeclaration(node) => node,
            Node::ClassExpression(node) => node,
            _ => panic!("Expected has members"),
        }
    }

    pub fn as_case_or_default_clause(&self) -> &dyn CaseOrDefaultClauseInterface {
        match self {
            Node::CaseClause(node) => node,
            Node::DefaultClause(node) => node,
            _ => panic!("Expected case or default clause"),
        }
    }

    pub fn as_has_assert_clause(&self) -> &dyn HasAssertClauseInterface {
        match self {
            Node::ImportDeclaration(node) => node,
            Node::ExportDeclaration(node) => node,
            _ => panic!("Expected has assert clause"),
        }
    }

    pub fn maybe_as_has_left_and_right(&self) -> Option<&dyn HasLeftAndRightInterface> {
        match self {
            Node::BinaryExpression(node) => Some(node),
            Node::QualifiedName(node) => Some(node),
            Node::JSDocMemberName(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_has_left_and_right(&self) -> &dyn HasLeftAndRightInterface {
        self.maybe_as_has_left_and_right()
            .expect("Expected has left and right")
    }

    pub fn as_has_module_specifier(&self) -> &dyn HasModuleSpecifierInterface {
        match self {
            Node::ExportDeclaration(node) => node,
            Node::ImportDeclaration(node) => node,
            _ => panic!("Expected has module specifier"),
        }
    }

    pub fn as_has_old_file_of_current_emit(&self) -> &dyn HasOldFileOfCurrentEmitInterface {
        match self {
            Node::InputFiles(node) => node,
            Node::UnparsedSource(node) => node,
            _ => panic!("Expected has old file of current emit"),
        }
    }

    pub fn as_has_texts(&self) -> &dyn HasTextsInterface {
        match self {
            Node::UnparsedPrepend(node) => node,
            Node::UnparsedSource(node) => node,
            _ => panic!("Expected has texts"),
        }
    }

    pub fn as_jsdoc_heritage_tag(&self) -> &dyn JSDocHeritageTagInterface {
        match self {
            Node::JSDocAugmentsTag(node) => node,
            Node::JSDocImplementsTag(node) => node,
            _ => panic!("Expected JSDoc heritage tag"),
        }
    }

    pub fn as_source_file_like(&self) -> &dyn SourceFileLike {
        match self {
            Node::SourceFile(node) => node,
            Node::UnparsedSource(node) => node,
            _ => panic!("Expected source file like"),
        }
    }

    pub fn as_has_file_name(&self) -> &dyn HasFileNameInterface {
        match self {
            Node::SourceFile(node) => node,
            Node::UnparsedSource(node) => node,
            _ => panic!("Expected has file name"),
        }
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

    pub fn as_source_file(&self) -> &SourceFile {
        enum_unwrapped!(self, [Node, SourceFile])
    }

    pub fn as_variable_declaration(&self) -> &VariableDeclaration {
        enum_unwrapped!(self, [Node, VariableDeclaration])
    }

    pub fn as_object_literal_expression(&self) -> &ObjectLiteralExpression {
        enum_unwrapped!(self, [Node, ObjectLiteralExpression])
    }

    pub fn maybe_as_identifier(&self) -> Option<&Identifier> {
        match self {
            Node::Identifier(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_identifier(&self) -> &Identifier {
        enum_unwrapped!(self, [Node, Identifier])
    }

    pub fn as_type_alias_declaration(&self) -> &TypeAliasDeclaration {
        enum_unwrapped!(self, [Node, TypeAliasDeclaration])
    }

    pub fn as_template_span(&self) -> &TemplateSpan {
        enum_unwrapped!(self, [Node, TemplateSpan])
    }

    pub fn as_jsdoc(&self) -> &JSDoc {
        enum_unwrapped!(self, [Node, JSDoc])
    }

    pub fn maybe_as_binary_expression(&self) -> Option<&BinaryExpression> {
        match self {
            Node::BinaryExpression(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_binary_expression(&self) -> &BinaryExpression {
        enum_unwrapped!(self, [Node, BinaryExpression])
    }

    pub fn as_module_declaration(&self) -> &ModuleDeclaration {
        enum_unwrapped!(self, [Node, ModuleDeclaration])
    }

    pub fn as_variable_statement(&self) -> &VariableStatement {
        enum_unwrapped!(self, [Node, VariableStatement])
    }

    pub fn as_expression_statement(&self) -> &ExpressionStatement {
        enum_unwrapped!(self, [Node, ExpressionStatement])
    }

    pub fn as_call_expression(&self) -> &CallExpression {
        enum_unwrapped!(self, [Node, CallExpression])
    }

    pub fn as_property_access_expression(&self) -> &PropertyAccessExpression {
        enum_unwrapped!(self, [Node, PropertyAccessExpression])
    }

    pub fn as_element_access_expression(&self) -> &ElementAccessExpression {
        enum_unwrapped!(self, [Node, ElementAccessExpression])
    }

    pub fn as_void_expression(&self) -> &VoidExpression {
        enum_unwrapped!(self, [Node, VoidExpression])
    }

    pub fn as_numeric_literal(&self) -> &NumericLiteral {
        enum_unwrapped!(self, [Node, NumericLiteral])
    }

    pub fn maybe_as_parameter_declaration(&self) -> Option<&ParameterDeclaration> {
        match self {
            Node::ParameterDeclaration(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_parameter_declaration(&self) -> &ParameterDeclaration {
        enum_unwrapped!(self, [Node, ParameterDeclaration])
    }

    pub fn as_jsdoc_property_like_tag(&self) -> &JSDocPropertyLikeTag {
        enum_unwrapped!(self, [Node, JSDocPropertyLikeTag])
    }

    pub fn as_jsdoc_template_tag(&self) -> &JSDocTemplateTag {
        enum_unwrapped!(self, [Node, JSDocTemplateTag])
    }

    pub fn as_new_expression(&self) -> &NewExpression {
        enum_unwrapped!(self, [Node, NewExpression])
    }

    pub fn as_jsdoc_typedef_tag(&self) -> &JSDocTypedefTag {
        enum_unwrapped!(self, [Node, JSDocTypedefTag])
    }

    pub fn as_export_assignment(&self) -> &ExportAssignment {
        enum_unwrapped!(self, [Node, ExportAssignment])
    }

    pub fn as_labeled_statement(&self) -> &LabeledStatement {
        enum_unwrapped!(self, [Node, LabeledStatement])
    }

    pub fn as_prefix_unary_expression(&self) -> &PrefixUnaryExpression {
        enum_unwrapped!(self, [Node, PrefixUnaryExpression])
    }

    pub fn as_jsdoc_type_expression(&self) -> &JSDocTypeExpression {
        enum_unwrapped!(self, [Node, JSDocTypeExpression])
    }

    pub fn as_type_literal_node(&self) -> &TypeLiteralNode {
        enum_unwrapped!(self, [Node, TypeLiteralNode])
    }

    pub fn as_jsdoc_member_name(&self) -> &JSDocMemberName {
        enum_unwrapped!(self, [Node, JSDocMemberName])
    }

    pub fn as_qualified_name(&self) -> &QualifiedName {
        enum_unwrapped!(self, [Node, QualifiedName])
    }

    pub fn as_jsdoc_text(&self) -> &JSDocText {
        enum_unwrapped!(self, [Node, JSDocText])
    }

    pub fn as_function_type_node(&self) -> &FunctionTypeNode {
        enum_unwrapped!(self, [Node, FunctionTypeNode])
    }

    pub fn as_type_reference_node(&self) -> &TypeReferenceNode {
        enum_unwrapped!(self, [Node, TypeReferenceNode])
    }

    pub fn as_block(&self) -> &Block {
        enum_unwrapped!(self, [Node, Block])
    }

    pub fn as_property_signature(&self) -> &PropertySignature {
        enum_unwrapped!(self, [Node, PropertySignature])
    }

    pub fn as_literal_type_node(&self) -> &LiteralTypeNode {
        enum_unwrapped!(self, [Node, LiteralTypeNode])
    }

    pub fn as_interface_declaration(&self) -> &InterfaceDeclaration {
        enum_unwrapped!(self, [Node, InterfaceDeclaration])
    }

    pub fn as_if_statement(&self) -> &IfStatement {
        enum_unwrapped!(self, [Node, IfStatement])
    }

    pub fn as_array_type_node(&self) -> &ArrayTypeNode {
        enum_unwrapped!(self, [Node, ArrayTypeNode])
    }

    pub fn as_template_expression(&self) -> &TemplateExpression {
        enum_unwrapped!(self, [Node, TemplateExpression])
    }

    pub fn as_union_type_node(&self) -> &UnionTypeNode {
        enum_unwrapped!(self, [Node, UnionTypeNode])
    }

    pub fn as_postfix_unary_expression(&self) -> &PostfixUnaryExpression {
        enum_unwrapped!(self, [Node, PostfixUnaryExpression])
    }

    pub fn as_conditional_expression(&self) -> &ConditionalExpression {
        enum_unwrapped!(self, [Node, ConditionalExpression])
    }

    pub fn as_tagged_template_expression(&self) -> &TaggedTemplateExpression {
        enum_unwrapped!(self, [Node, TaggedTemplateExpression])
    }

    pub fn as_function_declaration(&self) -> &FunctionDeclaration {
        enum_unwrapped!(self, [Node, FunctionDeclaration])
    }

    pub fn as_binding_element(&self) -> &BindingElement {
        enum_unwrapped!(self, [Node, BindingElement])
    }

    pub fn as_object_binding_pattern(&self) -> &ObjectBindingPattern {
        enum_unwrapped!(self, [Node, ObjectBindingPattern])
    }

    pub fn as_array_binding_pattern(&self) -> &ArrayBindingPattern {
        enum_unwrapped!(self, [Node, ArrayBindingPattern])
    }

    pub fn as_shorthand_property_assignment(&self) -> &ShorthandPropertyAssignment {
        enum_unwrapped!(self, [Node, ShorthandPropertyAssignment])
    }

    pub fn as_spread_element(&self) -> &SpreadElement {
        enum_unwrapped!(self, [Node, SpreadElement])
    }

    pub fn as_spread_assignment(&self) -> &SpreadAssignment {
        enum_unwrapped!(self, [Node, SpreadAssignment])
    }

    pub fn as_import_equals_declaration(&self) -> &ImportEqualsDeclaration {
        enum_unwrapped!(self, [Node, ImportEqualsDeclaration])
    }

    pub fn as_meta_property(&self) -> &MetaProperty {
        enum_unwrapped!(self, [Node, MetaProperty])
    }

    pub fn as_method_declaration(&self) -> &MethodDeclaration {
        enum_unwrapped!(self, [Node, MethodDeclaration])
    }

    pub fn as_parenthesized_type_node(&self) -> &ParenthesizedTypeNode {
        enum_unwrapped!(self, [Node, ParenthesizedTypeNode])
    }

    pub fn as_base_jsdoc_unary_type(&self) -> &BaseJSDocUnaryType {
        enum_unwrapped!(self, [Node, BaseJSDocUnaryType])
    }

    pub fn as_jsx_opening_element(&self) -> &JsxOpeningElement {
        enum_unwrapped!(self, [Node, JsxOpeningElement])
    }

    pub fn as_jsx_element(&self) -> &JsxElement {
        enum_unwrapped!(self, [Node, JsxElement])
    }

    pub fn as_jsx_closing_element(&self) -> &JsxClosingElement {
        enum_unwrapped!(self, [Node, JsxClosingElement])
    }

    pub fn as_non_null_expression(&self) -> &NonNullExpression {
        enum_unwrapped!(self, [Node, NonNullExpression])
    }

    pub fn as_syntax_list(&self) -> &SyntaxList {
        enum_unwrapped!(self, [Node, SyntaxList])
    }

    pub fn as_export_declaration(&self) -> &ExportDeclaration {
        enum_unwrapped!(self, [Node, ExportDeclaration])
    }

    pub fn as_namespace_export(&self) -> &NamespaceExport {
        enum_unwrapped!(self, [Node, NamespaceExport])
    }

    pub fn as_string_literal(&self) -> &StringLiteral {
        enum_unwrapped!(self, [Node, StringLiteral])
    }

    pub fn as_index_signature_declaration(&self) -> &IndexSignatureDeclaration {
        enum_unwrapped!(self, [Node, IndexSignatureDeclaration])
    }

    pub fn as_computed_property_name(&self) -> &ComputedPropertyName {
        enum_unwrapped!(self, [Node, ComputedPropertyName])
    }

    pub fn as_private_identifier(&self) -> &PrivateIdentifier {
        enum_unwrapped!(self, [Node, PrivateIdentifier])
    }

    pub fn as_arrow_function(&self) -> &ArrowFunction {
        enum_unwrapped!(self, [Node, ArrowFunction])
    }

    pub fn as_case_clause(&self) -> &CaseClause {
        enum_unwrapped!(self, [Node, CaseClause])
    }

    pub fn as_default_clause(&self) -> &DefaultClause {
        enum_unwrapped!(self, [Node, DefaultClause])
    }

    pub fn as_import_type_node(&self) -> &ImportTypeNode {
        enum_unwrapped!(self, [Node, ImportTypeNode])
    }

    pub fn as_type_assertion(&self) -> &TypeAssertion {
        enum_unwrapped!(self, [Node, TypeAssertion])
    }

    pub fn as_yield_expression(&self) -> &YieldExpression {
        enum_unwrapped!(self, [Node, YieldExpression])
    }

    pub fn as_synthetic_expression(&self) -> &SyntheticExpression {
        enum_unwrapped!(self, [Node, SyntheticExpression])
    }

    pub fn as_class_declaration(&self) -> &ClassDeclaration {
        enum_unwrapped!(self, [Node, ClassDeclaration])
    }

    pub fn as_class_expression(&self) -> &ClassExpression {
        enum_unwrapped!(self, [Node, ClassExpression])
    }

    pub fn as_array_literal_expression(&self) -> &ArrayLiteralExpression {
        enum_unwrapped!(self, [Node, ArrayLiteralExpression])
    }

    pub fn as_expression_with_type_arguments(&self) -> &ExpressionWithTypeArguments {
        enum_unwrapped!(self, [Node, ExpressionWithTypeArguments])
    }

    pub fn as_jsx_self_closing_element(&self) -> &JsxSelfClosingElement {
        enum_unwrapped!(self, [Node, JsxSelfClosingElement])
    }

    pub fn as_as_expression(&self) -> &AsExpression {
        enum_unwrapped!(self, [Node, AsExpression])
    }

    pub fn as_for_of_statement(&self) -> &ForOfStatement {
        enum_unwrapped!(self, [Node, ForOfStatement])
    }

    pub fn as_for_in_statement(&self) -> &ForInStatement {
        enum_unwrapped!(self, [Node, ForInStatement])
    }

    pub fn as_for_statement(&self) -> &ForStatement {
        enum_unwrapped!(self, [Node, ForStatement])
    }

    pub fn as_throw_statement(&self) -> &ThrowStatement {
        enum_unwrapped!(self, [Node, ThrowStatement])
    }

    pub fn as_switch_statement(&self) -> &SwitchStatement {
        enum_unwrapped!(self, [Node, SwitchStatement])
    }

    pub fn as_with_statement(&self) -> &WithStatement {
        enum_unwrapped!(self, [Node, WithStatement])
    }

    pub fn as_return_statement(&self) -> &ReturnStatement {
        enum_unwrapped!(self, [Node, ReturnStatement])
    }

    pub fn as_while_statement(&self) -> &WhileStatement {
        enum_unwrapped!(self, [Node, WhileStatement])
    }

    pub fn as_do_statement(&self) -> &DoStatement {
        enum_unwrapped!(self, [Node, DoStatement])
    }

    pub fn as_external_module_reference(&self) -> &ExternalModuleReference {
        enum_unwrapped!(self, [Node, ExternalModuleReference])
    }

    pub fn as_import_declaration(&self) -> &ImportDeclaration {
        enum_unwrapped!(self, [Node, ImportDeclaration])
    }

    pub fn as_import_clause(&self) -> &ImportClause {
        enum_unwrapped!(self, [Node, ImportClause])
    }

    pub fn as_named_imports(&self) -> &NamedImports {
        enum_unwrapped!(self, [Node, NamedImports])
    }

    pub fn as_method_signature(&self) -> &MethodSignature {
        enum_unwrapped!(self, [Node, MethodSignature])
    }

    pub fn as_property_declaration(&self) -> &PropertyDeclaration {
        enum_unwrapped!(self, [Node, PropertyDeclaration])
    }

    pub fn as_jsdoc_function_type(&self) -> &JSDocFunctionType {
        enum_unwrapped!(self, [Node, JSDocFunctionType])
    }

    pub fn as_jsdoc_signature(&self) -> &JSDocSignature {
        enum_unwrapped!(self, [Node, JSDocSignature])
    }

    pub fn as_import_specifier(&self) -> &ImportSpecifier {
        enum_unwrapped!(self, [Node, ImportSpecifier])
    }

    pub fn as_jsdoc_augments_tag(&self) -> &JSDocAugmentsTag {
        enum_unwrapped!(self, [Node, JSDocAugmentsTag])
    }

    pub fn as_heritage_clause(&self) -> &HeritageClause {
        enum_unwrapped!(self, [Node, HeritageClause])
    }

    pub fn maybe_as_heritage_clause(&self) -> Option<&HeritageClause> {
        match self {
            Node::HeritageClause(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_jsdoc_implements_tag(&self) -> &JSDocImplementsTag {
        enum_unwrapped!(self, [Node, JSDocImplementsTag])
    }

    pub fn as_jsx_expression(&self) -> &JsxExpression {
        enum_unwrapped!(self, [Node, JsxExpression])
    }

    pub fn as_jsx_text(&self) -> &JsxText {
        enum_unwrapped!(self, [Node, JsxText])
    }

    pub fn as_base_jsdoc_type_like_tag(&self) -> &BaseJSDocTypeLikeTag {
        enum_unwrapped!(self, [Node, BaseJSDocTypeLikeTag])
    }

    pub fn as_named_exports(&self) -> &NamedExports {
        enum_unwrapped!(self, [Node, NamedExports])
    }

    pub fn as_export_specifier(&self) -> &ExportSpecifier {
        enum_unwrapped!(self, [Node, ExportSpecifier])
    }

    pub fn as_class_static_block_declaration(&self) -> &ClassStaticBlockDeclaration {
        enum_unwrapped!(self, [Node, ClassStaticBlockDeclaration])
    }

    pub fn as_type_of_expression(&self) -> &TypeOfExpression {
        enum_unwrapped!(self, [Node, TypeOfExpression])
    }

    pub fn as_parenthesized_expression(&self) -> &ParenthesizedExpression {
        enum_unwrapped!(self, [Node, ParenthesizedExpression])
    }

    pub fn as_try_statement(&self) -> &TryStatement {
        enum_unwrapped!(self, [Node, TryStatement])
    }

    pub fn as_case_block(&self) -> &CaseBlock {
        enum_unwrapped!(self, [Node, CaseBlock])
    }

    pub fn as_delete_expression(&self) -> &DeleteExpression {
        enum_unwrapped!(self, [Node, DeleteExpression])
    }

    pub fn as_catch_clause(&self) -> &CatchClause {
        enum_unwrapped!(self, [Node, CatchClause])
    }

    pub fn as_module_block(&self) -> &ModuleBlock {
        enum_unwrapped!(self, [Node, ModuleBlock])
    }

    pub fn as_function_expression(&self) -> &FunctionExpression {
        enum_unwrapped!(self, [Node, FunctionExpression])
    }

    pub fn as_conditional_type_node(&self) -> &ConditionalTypeNode {
        enum_unwrapped!(self, [Node, ConditionalTypeNode])
    }

    pub fn as_constructor_declaration(&self) -> &ConstructorDeclaration {
        enum_unwrapped!(self, [Node, ConstructorDeclaration])
    }

    pub fn as_set_accessor_declaration(&self) -> &SetAccessorDeclaration {
        enum_unwrapped!(self, [Node, SetAccessorDeclaration])
    }

    pub fn as_infer_type_node(&self) -> &InferTypeNode {
        enum_unwrapped!(self, [Node, InferTypeNode])
    }

    pub fn as_mapped_type_node(&self) -> &MappedTypeNode {
        enum_unwrapped!(self, [Node, MappedTypeNode])
    }

    pub fn as_enum_member(&self) -> &EnumMember {
        enum_unwrapped!(self, [Node, EnumMember])
    }

    pub fn as_enum_declaration(&self) -> &EnumDeclaration {
        enum_unwrapped!(self, [Node, EnumDeclaration])
    }

    pub fn as_type_operator_node(&self) -> &TypeOperatorNode {
        enum_unwrapped!(self, [Node, TypeOperatorNode])
    }

    pub fn as_type_predicate_node(&self) -> &TypePredicateNode {
        enum_unwrapped!(self, [Node, TypePredicateNode])
    }

    pub fn as_named_tuple_member(&self) -> &NamedTupleMember {
        enum_unwrapped!(self, [Node, NamedTupleMember])
    }

    pub fn as_tuple_type_node(&self) -> &TupleTypeNode {
        enum_unwrapped!(self, [Node, TupleTypeNode])
    }

    pub fn as_type_query_node(&self) -> &TypeQueryNode {
        enum_unwrapped!(self, [Node, TypeQueryNode])
    }

    pub fn as_rest_type_node(&self) -> &RestTypeNode {
        enum_unwrapped!(self, [Node, RestTypeNode])
    }

    pub fn as_indexed_access_type_node(&self) -> &IndexedAccessTypeNode {
        enum_unwrapped!(self, [Node, IndexedAccessTypeNode])
    }

    pub fn as_optional_type_node(&self) -> &OptionalTypeNode {
        enum_unwrapped!(self, [Node, OptionalTypeNode])
    }

    pub fn as_intersection_type_node(&self) -> &IntersectionTypeNode {
        enum_unwrapped!(self, [Node, IntersectionTypeNode])
    }

    pub fn as_template_literal_type_node(&self) -> &TemplateLiteralTypeNode {
        enum_unwrapped!(self, [Node, TemplateLiteralTypeNode])
    }

    pub fn as_template_literal_type_span(&self) -> &TemplateLiteralTypeSpan {
        enum_unwrapped!(self, [Node, TemplateLiteralTypeSpan])
    }

    pub fn as_jsdoc_type_literal(&self) -> &JSDocTypeLiteral {
        enum_unwrapped!(self, [Node, JSDocTypeLiteral])
    }

    pub fn as_jsx_attributes(&self) -> &JsxAttributes {
        enum_unwrapped!(self, [Node, JsxAttributes])
    }

    pub fn as_jsx_attribute(&self) -> &JsxAttribute {
        enum_unwrapped!(self, [Node, JsxAttribute])
    }

    pub fn as_jsx_fragment(&self) -> &JsxFragment {
        enum_unwrapped!(self, [Node, JsxFragment])
    }

    pub fn as_jsx_spread_attribute(&self) -> &JsxSpreadAttribute {
        enum_unwrapped!(self, [Node, JsxSpreadAttribute])
    }

    pub fn as_decorator(&self) -> &Decorator {
        enum_unwrapped!(self, [Node, Decorator])
    }

    pub fn as_await_expression(&self) -> &AwaitExpression {
        enum_unwrapped!(self, [Node, AwaitExpression])
    }

    pub fn as_big_int_literal(&self) -> &BigIntLiteral {
        enum_unwrapped!(self, [Node, BigIntLiteral])
    }

    pub fn as_jsdoc_callback_tag(&self) -> &JSDocCallbackTag {
        enum_unwrapped!(self, [Node, JSDocCallbackTag])
    }

    pub fn as_comma_list_expression(&self) -> &CommaListExpression {
        enum_unwrapped!(self, [Node, CommaListExpression])
    }

    pub fn as_bundle(&self) -> &Bundle {
        enum_unwrapped!(self, [Node, Bundle])
    }

    pub fn as_unparsed_source(&self) -> &UnparsedSource {
        enum_unwrapped!(self, [Node, UnparsedSource])
    }

    pub fn as_unparsed_synthetic_reference(&self) -> &UnparsedSyntheticReference {
        enum_unwrapped!(self, [Node, UnparsedSyntheticReference])
    }

    pub fn as_call_signature_declaration(&self) -> &CallSignatureDeclaration {
        enum_unwrapped!(self, [Node, CallSignatureDeclaration])
    }

    pub fn as_construct_signature_declaration(&self) -> &ConstructSignatureDeclaration {
        enum_unwrapped!(self, [Node, ConstructSignatureDeclaration])
    }

    pub fn as_constructor_type_node(&self) -> &ConstructorTypeNode {
        enum_unwrapped!(self, [Node, ConstructorTypeNode])
    }

    pub fn as_continue_statement(&self) -> &ContinueStatement {
        enum_unwrapped!(self, [Node, ContinueStatement])
    }

    pub fn as_break_statement(&self) -> &BreakStatement {
        enum_unwrapped!(self, [Node, BreakStatement])
    }

    pub fn as_namespace_import(&self) -> &NamespaceImport {
        enum_unwrapped!(self, [Node, NamespaceImport])
    }

    pub fn as_assert_clause(&self) -> &AssertClause {
        enum_unwrapped!(self, [Node, AssertClause])
    }

    pub fn as_assert_entry(&self) -> &AssertEntry {
        enum_unwrapped!(self, [Node, AssertEntry])
    }

    pub fn as_namespace_export_declaration(&self) -> &NamespaceExportDeclaration {
        enum_unwrapped!(self, [Node, NamespaceExportDeclaration])
    }

    pub fn as_jsdoc_see_tag(&self) -> &JSDocSeeTag {
        enum_unwrapped!(self, [Node, JSDocSeeTag])
    }

    pub fn as_jsdoc_name_reference(&self) -> &JSDocNameReference {
        enum_unwrapped!(self, [Node, JSDocNameReference])
    }

    pub fn as_partially_emitted_expression(&self) -> &PartiallyEmittedExpression {
        enum_unwrapped!(self, [Node, PartiallyEmittedExpression])
    }

    pub fn as_unparsed_prologue(&self) -> &UnparsedPrologue {
        enum_unwrapped!(self, [Node, UnparsedPrologue])
    }

    pub fn as_get_accessor_declaration(&self) -> &GetAccessorDeclaration {
        enum_unwrapped!(self, [Node, GetAccessorDeclaration])
    }

    pub fn as_synthetic_reference_expression(&self) -> &SyntheticReferenceExpression {
        enum_unwrapped!(self, [Node, SyntheticReferenceExpression])
    }

    pub fn as_base_jsdoc_tag(&self) -> &BaseJSDocTag {
        enum_unwrapped!(self, [Node, BaseJSDocTag])
    }

    pub fn as_jsdoc_link(&self) -> &JSDocLink {
        enum_unwrapped!(self, [Node, JSDocLink])
    }

    pub fn as_jsdoc_link_code(&self) -> &JSDocLinkCode {
        enum_unwrapped!(self, [Node, JSDocLinkCode])
    }

    pub fn as_jsdoc_link_plain(&self) -> &JSDocLinkPlain {
        enum_unwrapped!(self, [Node, JSDocLinkPlain])
    }
}

pub struct BaseNode {
    arena: *const AllArenas,
    _arena_id: Cell<Option<Id<Node>>>,
    _id_override: Cell<Option<Id<Box<dyn NodeIdOverride>>>>,
    _symbol_override: Cell<Option<Id<Box<dyn NodeSymbolOverride>>>>,
    pub kind: SyntaxKind,
    flags: Cell<NodeFlags>,
    modifier_flags_cache: Cell<ModifierFlags>,
    transform_flags: Cell<TransformFlags>,
    pub decorators: Cell<Option<Id<NodeArray> /*<Decorator>*/>>,
    pub modifiers: Cell<Option<ModifiersArray>>,
    pub id: Cell<Option<NodeId>>,
    pub parent: Cell<Option<Id<Node>>>,
    pub original: Cell<Option<Id<Node>>>,
    pub pos: Cell<isize>,
    pub end: Cell<isize>,
    pub symbol: Cell<Option<Id<Symbol>>>,
    pub locals: Id<Option<Id<SymbolTable>>>,
    next_container: Cell<Option<Id<Node>>>,
    local_symbol: Cell<Option<Id<Symbol>>>,
    emit_node: Cell<Option<Id<EmitNode>>>,
    contextual_type: Cell<Option<Id<Type>>>,
    inference_context: Cell<Option<Id<InferenceContext>>>,
    flow_node: Cell<Option<Id<FlowNode>>>,
    js_doc: RefCell<Option<Vec<Id<Node>>>>,
    js_doc_cache: Cell<Option<Id<Vec<Id<Node>>>>>,
    intersects_change: Cell<Option<bool>>,
}

impl fmt::Debug for BaseNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BaseNode")
            // .field("_arena_id", &self._arena_id)
            // .field("_id_override", &self._id_override)
            // .field("_symbol_override", &self._symbol_override)
            .field("kind", &self.kind)
            .field("flags", &self.flags)
            // .field("modifier_flags_cache", &self.modifier_flags_cache)
            .field("transform_flags", &self.transform_flags)
            // .field("decorators", &self.decorators)
            // .field("modifiers", &self.modifiers)
            .field("id", &self.id)
            // .field("parent", &self.parent)
            // .field("original", &self.original)
            .field("pos", &self.pos)
            .field("end", &self.end)
            // .field("symbol", &self.symbol)
            // .field("locals", &self.locals)
            // .field("next_container", &self.next_container)
            // .field("local_symbol", &self.local_symbol)
            // .field("emit_node", &self.emit_node)
            // .field("contextual_type", &self.contextual_type)
            // .field("inference_context", &self.inference_context)
            // .field("flow_node", &self.flow_node)
            // .field("js_doc", &self.js_doc)
            // .field("js_doc_cache", &self.js_doc_cache)
            // .field("intersects_change", &self.intersects_change)
            .finish()
    }
}

impl BaseNode {
    pub fn new(
        kind: SyntaxKind,
        flags: NodeFlags,
        transform_flags: TransformFlags,
        pos: isize,
        end: isize,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            _arena_id: Default::default(),
            _id_override: Default::default(),
            _symbol_override: Default::default(),
            kind,
            flags: Cell::new(flags),
            modifier_flags_cache: Cell::new(ModifierFlags::None),
            transform_flags: Cell::new(transform_flags),
            decorators: Default::default(),
            modifiers: Default::default(),
            id: Default::default(),
            parent: Default::default(),
            original: Default::default(),
            pos: Cell::new(pos),
            end: Cell::new(end),
            symbol: Default::default(),
            locals: arena.alloc_option_symbol_table(Default::default()),
            next_container: Default::default(),
            local_symbol: Default::default(),
            emit_node: Default::default(),
            contextual_type: Default::default(),
            inference_context: Default::default(),
            flow_node: Default::default(),
            js_doc: Default::default(),
            js_doc_cache: Default::default(),
            intersects_change: Default::default(),
        }
    }
}

impl NodeInterface for BaseNode {
    fn arena_id(&self) -> Id<Node> {
        self._arena_id.get().unwrap()
    }

    fn set_arena_id(&self, id: Id<Node>) {
        self._arena_id.set(Some(id));
    }

    fn alloc(self, arena: &AllArenas) -> Id<Node> {
        let id = arena.alloc_node(self.into());
        id
    }

    fn base_node(&self) -> &BaseNode {
        self
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

    fn modifier_flags_cache(&self) -> ModifierFlags {
        self.modifier_flags_cache.get()
    }

    fn set_modifier_flags_cache(&self, flags: ModifierFlags) {
        self.modifier_flags_cache.set(flags);
    }

    fn transform_flags(&self) -> TransformFlags {
        self.transform_flags.get()
    }

    fn set_transform_flags(&self, flags: TransformFlags) {
        self.transform_flags.set(flags);
    }

    fn add_transform_flags(&self, flags: TransformFlags) {
        self.transform_flags.set(self.transform_flags.get() | flags);
    }

    fn maybe_decorators(&self) -> Option<Id<NodeArray>> {
        self.decorators.get()
    }

    fn set_decorators(&self, decorators: Option<Id<NodeArray>>) {
        self.decorators.set(decorators);
    }

    fn maybe_modifiers(&self) -> Option<Id<NodeArray>> {
        self.modifiers.get()
    }

    fn set_modifiers(&self, modifiers: Option<Id<NodeArray>>) {
        self.modifiers.set(modifiers);
    }

    fn maybe_id(&self) -> Option<NodeId> {
        match self._id_override.get() {
            Some(id_override) => id_override.ref_(self).maybe_id(),
            None => self.id.get(),
        }
    }

    fn id(&self) -> NodeId {
        self.maybe_id().unwrap()
    }

    fn set_id(&self, id: NodeId) {
        match self._id_override.get() {
            Some(id_override) => {
                id_override.ref_(self).set_id(id);
            }
            None => {
                self.id.set(Some(id));
            }
        }
    }

    fn set_id_override(&self, id_override: Id<Box<dyn NodeIdOverride>>) {
        self._id_override.set(Some(id_override));
    }

    fn maybe_parent(&self) -> Option<Id<Node>> {
        self.parent.get()
    }

    fn parent(&self) -> Id<Node> {
        self.parent.get().unwrap()
    }

    fn set_parent(&self, parent: Option<Id<Node>>) {
        self.parent.set(parent);
    }

    fn maybe_original(&self) -> Option<Id<Node>> {
        self.original.get()
    }

    fn set_original(&self, original: Option<Id<Node>>) {
        self.original.set(original);
    }

    fn maybe_symbol(&self) -> Option<Id<Symbol>> {
        match self._symbol_override.get() {
            Some(symbol_override) => symbol_override.ref_(self).maybe_symbol(),
            None => self.symbol.get(),
        }
    }

    fn symbol(&self) -> Id<Symbol> {
        self.maybe_symbol().unwrap()
    }

    fn set_symbol(&self, symbol: Id<Symbol>) {
        match self._symbol_override.get() {
            Some(symbol_override) => {
                symbol_override.ref_(self).set_symbol(symbol);
            }
            None => {
                self.symbol.set(Some(symbol));
            }
        }
    }

    fn set_symbol_override(&self, symbol_override: Id<Box<dyn NodeSymbolOverride>>) {
        self._symbol_override.set(Some(symbol_override));
    }

    fn maybe_locals(&self) -> Option<Id<SymbolTable>> {
        *self.locals.ref_(self)
    }

    fn maybe_locals_mut(&self) -> debug_cell::RefMut<Option<Id<SymbolTable>>> {
        self.locals.ref_mut(self)
    }

    fn locals(&self) -> Id<SymbolTable> {
        self.locals.ref_(self).unwrap()
    }

    fn locals_mut(&self) -> debug_cell::RefMut<Id<SymbolTable>> {
        debug_cell::RefMut::map(self.locals.ref_mut(self), |option| option.as_mut().unwrap())
    }

    fn set_locals(&self, locals: Option<Id<SymbolTable>>) {
        *self.locals.ref_mut(self) = locals;
    }

    fn maybe_next_container(&self) -> Option<Id<Node>> {
        self.next_container.get()
    }

    fn set_next_container(&self, next_container: Option<Id<Node>>) {
        self.next_container.set(next_container);
    }

    fn maybe_local_symbol(&self) -> Option<Id<Symbol>> {
        self.local_symbol.get()
    }

    fn set_local_symbol(&self, local_symbol: Option<Id<Symbol>>) {
        self.local_symbol.set(local_symbol);
    }

    fn maybe_emit_node(&self) -> Option<Id<EmitNode>> {
        self.emit_node.get()
    }

    fn set_emit_node(&self, emit_node: Option<Id<EmitNode>>) {
        self.emit_node.set(emit_node);
    }

    fn maybe_contextual_type(&self) -> Option<Id<Type>> {
        self.contextual_type.get()
    }

    fn set_contextual_type(&self, contextual_type: Option<Id<Type>>) {
        self.contextual_type.set(contextual_type);
    }

    fn maybe_inference_context(&self) -> Option<Id<InferenceContext>> {
        self.inference_context.get()
    }

    fn set_inference_context(&self, inference_context: Option<Id<InferenceContext>>) {
        self.inference_context.set(inference_context);
    }

    fn maybe_flow_node(&self) -> Option<Id<FlowNode>> {
        self.flow_node.get()
    }

    fn set_flow_node(&self, flow_node: Option<Id<FlowNode>>) {
        self.flow_node.set(flow_node);
    }

    fn maybe_js_doc(&self) -> Option<Vec<Id<Node>>> {
        self.js_doc.borrow().clone()
    }

    fn set_js_doc(&self, js_doc: Option<Vec<Id<Node>>>) {
        *self.js_doc.borrow_mut() = js_doc;
    }

    fn maybe_js_doc_cache(&self) -> Option<Id<Vec<Id<Node>>>> {
        self.js_doc_cache.get()
    }

    fn set_js_doc_cache(&self, js_doc_cache: Option<Id<Vec<Id<Node>>>>) {
        self.js_doc_cache.set(js_doc_cache);
    }

    fn maybe_intersects_change(&self) -> Option<bool> {
        self.intersects_change.get()
    }

    fn set_intersects_change(&self, intersects_change: Option<bool>) {
        self.intersects_change.set(intersects_change);
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

impl_has_arena!(BaseNode);

impl From<BaseNode> for Node {
    fn from(base_node: BaseNode) -> Self {
        Node::BaseNode(base_node)
    }
}

impl Clone for BaseNode {
    fn clone(&self) -> Self {
        Self {
            arena: self.arena,
            _arena_id: Default::default(),
            _id_override: self._id_override.clone(),
            _symbol_override: self._symbol_override.clone(),
            kind: self.kind,
            flags: self.flags.clone(),
            modifier_flags_cache: self.modifier_flags_cache.clone(),
            transform_flags: self.transform_flags.clone(),
            decorators: self.decorators.clone(),
            modifiers: self.modifiers.clone(),
            id: self.id.clone(),
            parent: self.parent.clone(),
            original: self.original.clone(),
            pos: self.pos.clone(),
            end: self.end.clone(),
            symbol: self.symbol.clone(),
            locals: self.locals.clone(),
            next_container: self.next_container.clone(),
            local_symbol: self.local_symbol.clone(),
            emit_node: self.emit_node.clone(),
            contextual_type: self.contextual_type.clone(),
            inference_context: self.inference_context.clone(),
            flow_node: self.flow_node.clone(),
            js_doc: self.js_doc.clone(),
            js_doc_cache: self.js_doc_cache.clone(),
            intersects_change: self.intersects_change.clone(),
        }
    }
}

pub trait NodeExt {
    fn set_text_range(
        self,
        location: Option<&(impl ReadonlyTextRange + ?Sized)>,
        arena: &impl HasArena,
    ) -> Self;
    fn set_text_range_pos(self, pos: isize, arena: &impl HasArena) -> Self;
    fn set_text_range_end(self, end: isize, arena: &impl HasArena) -> Self;
    fn set_emit_flags(self, emit_flags: EmitFlags, arena: &impl HasArena) -> Self;
    fn set_additional_emit_flags(self, emit_flags: EmitFlags, arena: &impl HasArena) -> Self;
    fn add_emit_flags(self, emit_flags: EmitFlags, arena: &impl HasArena) -> Self;
    fn set_original_node(self, original: Option<Id<Node>>, arena: &impl HasArena) -> Self;
    fn set_comment_range(self, range: &impl ReadonlyTextRange, arena: &impl HasArena) -> Self;
    fn set_source_map_range(self, range: Option<Id<SourceMapRange>>, arena: &impl HasArena)
        -> Self;
    fn start_on_new_line(self, arena: &impl HasArena) -> Self;
    fn and_set_parent(self, parent: Option<Id<Node>>, arena: &impl HasArena) -> Self;
    fn set_parent_recursive(self, incremental: bool, arena: &impl HasArena) -> Self;
    fn and_set_original(self, original: Option<Id<Node>>, arena: &impl HasArena) -> Self;
    fn remove_all_comments(self, arena: &impl HasArena) -> Self;
    fn add_emit_helpers(self, helpers: Option<&[Id<EmitHelper>]>, arena: &impl HasArena) -> Self;
    fn add_synthetic_leading_comment(
        self,
        kind: SyntaxKind,
        text: &str,
        has_trailing_new_line: Option<bool>,
        arena: &impl HasArena,
    ) -> Self;
    fn add_synthetic_trailing_comment(
        self,
        kind: SyntaxKind,
        text: &str,
        has_trailing_new_line: Option<bool>,
        arena: &impl HasArena,
    ) -> Self;
    fn move_synthetic_comments(self, original: Id<Node>, arena: &impl HasArena) -> Self;
}

impl NodeExt for Id<Node> {
    fn set_text_range(
        self,
        location: Option<&(impl ReadonlyTextRange + ?Sized)>,
        arena: &impl HasArena,
    ) -> Self {
        set_text_range_id_node(self, location, arena)
    }

    fn set_text_range_pos(self, pos: isize, arena: &impl HasArena) -> Self {
        set_text_range_pos(&*self.ref_(arena), pos);
        self
    }

    fn set_text_range_end(self, end: isize, arena: &impl HasArena) -> Self {
        set_text_range_end(&*self.ref_(arena), end);
        self
    }

    fn set_emit_flags(self, emit_flags: EmitFlags, arena: &impl HasArena) -> Self {
        set_emit_flags(self, emit_flags, arena)
    }

    fn set_additional_emit_flags(self, emit_flags: EmitFlags, arena: &impl HasArena) -> Self {
        let existing_emit_flags = get_emit_flags(self, arena);
        set_emit_flags(self, emit_flags | existing_emit_flags, arena)
    }

    fn add_emit_flags(self, emit_flags: EmitFlags, arena: &impl HasArena) -> Self {
        add_emit_flags(self, emit_flags, arena);
        self
    }

    fn set_original_node(self, original: Option<Id<Node>>, arena: &impl HasArena) -> Self {
        set_original_node(self, original, arena)
    }

    fn set_comment_range(self, range: &impl ReadonlyTextRange, arena: &impl HasArena) -> Self {
        set_comment_range(self, range, arena)
    }

    fn set_source_map_range(
        self,
        range: Option<Id<SourceMapRange>>,
        arena: &impl HasArena,
    ) -> Self {
        set_source_map_range(self, range, arena)
    }

    fn start_on_new_line(self, arena: &impl HasArena) -> Self {
        start_on_new_line(self, arena)
    }

    fn and_set_parent(self, parent: Option<Id<Node>>, arena: &impl HasArena) -> Self {
        self.ref_(arena).set_parent(parent);
        self
    }

    fn set_parent_recursive(self, incremental: bool, arena: &impl HasArena) -> Self {
        set_parent_recursive(Some(self), incremental, arena);
        self
    }

    fn and_set_original(self, original: Option<Id<Node>>, arena: &impl HasArena) -> Self {
        self.ref_(arena).set_original(original);
        self
    }

    fn remove_all_comments(self, arena: &impl HasArena) -> Self {
        remove_all_comments(self, arena);
        self
    }

    fn add_emit_helpers(self, helpers: Option<&[Id<EmitHelper>]>, arena: &impl HasArena) -> Self {
        add_emit_helpers(self, helpers, arena);
        self
    }

    fn add_synthetic_leading_comment(
        self,
        kind: SyntaxKind,
        text: &str,
        has_trailing_new_line: Option<bool>,
        arena: &impl HasArena,
    ) -> Self {
        add_synthetic_leading_comment(self, kind, text, has_trailing_new_line, arena);
        self
    }

    fn add_synthetic_trailing_comment(
        self,
        kind: SyntaxKind,
        text: &str,
        has_trailing_new_line: Option<bool>,
        arena: &impl HasArena,
    ) -> Self {
        add_synthetic_trailing_comment(self, kind, text, has_trailing_new_line, arena);
        self
    }

    fn move_synthetic_comments(self, original: Id<Node>, arena: &impl HasArena) -> Self {
        move_synthetic_comments(self, original, arena);
        self
    }
}

pub trait HasTypeInterface {
    fn maybe_type(&self) -> Option<Id<Node>>;
    fn set_type(&mut self, type_: Option<Id<Node>>);
}

pub trait HasInitializerInterface {
    fn maybe_initializer(&self) -> Option<Id<Node>>;
    fn set_initializer(&mut self, initializer: Id<Node>);
}
