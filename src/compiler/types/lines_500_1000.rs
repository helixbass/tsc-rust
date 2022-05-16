#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::rc::{Rc, Weak};

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
    JSDocNamespaceDeclaration, JSDocPropertyLikeTag, JSDocSeeTag, JSDocSignature,
    JSDocTagInterface, JSDocTemplateTag, JSDocText, JSDocTypeExpression, JSDocTypeLikeTagInterface,
    JSDocTypeLiteral, JSDocTypedefOrCallbackTagInterface, JSDocTypedefTag, JsxAttribute,
    JsxAttributes, JsxClosingElement, JsxClosingFragment, JsxElement, JsxExpression, JsxFragment,
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
    TransformFlags, TryStatement, TupleTypeNode, TypeAliasDeclaration, TypeAssertion,
    TypeLiteralNode, TypeOfExpression, TypeOperatorNode, TypeParameterDeclaration,
    TypePredicateNode, TypeQueryNode, TypeReferenceNode, UnionOrIntersectionTypeNodeInterface,
    UnionTypeNode, UnparsedPrepend, UnparsedPrologue, UnparsedSource, UnparsedTextLike,
    VariableDeclaration, VariableDeclarationList, VariableLikeDeclarationInterface,
    VariableStatement, VoidExpression, WhileStatement, WithStatement, YieldExpression,
};
use local_macros::{ast_type, enum_unwrapped};

bitflags! {
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
    fn modifier_flags_cache(&self) -> ModifierFlags;
    fn set_modifier_flags_cache(&self, flags: ModifierFlags);
    fn flags(&self) -> NodeFlags;
    fn set_flags(&self, flags: NodeFlags);
    fn transform_flags(&self) -> TransformFlags;
    fn set_transform_flags(&self, flags: TransformFlags);
    fn add_transform_flags(&self, flags: TransformFlags);
    fn maybe_decorators(&self) -> Ref<Option<NodeArray>>;
    fn set_decorators(&self, decorators: Option<NodeArray>);
    fn maybe_modifiers(&self) -> Ref<Option<NodeArray>>;
    fn set_modifiers(&self, modifiers: Option<NodeArray>);
    fn maybe_id(&self) -> Option<NodeId>;
    fn id(&self) -> NodeId;
    fn set_id(&self, id: NodeId);
    fn maybe_parent(&self) -> Option<Rc<Node>>;
    fn parent(&self) -> Rc<Node>;
    fn set_parent(&self, parent: Rc<Node>);
    fn maybe_original(&self) -> Option<Rc<Node>>;
    fn set_original(&self, original: Option<Rc<Node>>);
    fn maybe_symbol(&self) -> Option<Rc<Symbol>>;
    fn symbol(&self) -> Rc<Symbol>;
    fn set_symbol(&self, symbol: Rc<Symbol>);
    fn maybe_locals(&self) -> RefMut<Option<Rc<RefCell<SymbolTable>>>>;
    fn locals(&self) -> RefMut<Rc<RefCell<SymbolTable>>>;
    fn set_locals(&self, locals: Option<Rc<RefCell<SymbolTable>>>);
    fn maybe_next_container(&self) -> Option<Rc<Node>>;
    fn set_next_container(&self, next_container: Option<Rc<Node>>);
    fn maybe_local_symbol(&self) -> Option<Rc<Symbol>>;
    fn set_local_symbol(&self, local_symbol: Option<Rc<Symbol>>);
    fn maybe_flow_node(&self) -> RefMut<Option<Rc<FlowNode>>>;
    fn set_flow_node(&self, emit_node: Option<Rc<FlowNode>>);
    fn maybe_emit_node(&self) -> RefMut<Option<EmitNode>>;
    fn set_emit_node(&self, emit_node: Option<EmitNode>);
    fn maybe_js_doc(&self) -> Option<Vec<Rc<Node /*JSDoc*/>>>;
    fn set_js_doc(&self, js_doc: Vec<Rc<Node /*JSDoc*/>>);
    fn maybe_js_doc_cache(&self) -> Option<Vec<Rc<Node /*JSDocTag*/>>>;
    fn set_js_doc_cache(&self, js_doc_cache: Option<Vec<Rc<Node /*JSDocTag*/>>>);
    // IncrementalElement
    fn maybe_intersects_change(&self) -> Option<bool>;
    fn set_intersects_change(&self, intersects_change: Option<bool>);
    // fn maybe_length(&self) -> Option<usize>;
    // fn set_length(&self, length: Option<usize>);
    // _children: Node[] | undefined;
    // IncrementalNode
    // hasBeenIncrementallyParsed: boolean;
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
    JSDocNamespaceDeclaration(JSDocNamespaceDeclaration),
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
}

impl Node {
    pub fn wrap(self) -> Rc<Node> {
        let rc = Rc::new(self);
        rc.set_node_wrapper(rc.clone());
        rc
    }

    pub fn maybe_as_named_declaration(&self) -> Option<&dyn NamedDeclarationInterface> {
        match self {
            Node::TypeParameterDeclaration(node) => Some(node),
            Node::VariableDeclaration(node) => Some(node),
            Node::InterfaceDeclaration(node) => Some(node),
            Node::TypeAliasDeclaration(node) => Some(node),
            Node::PropertySignature(node) => Some(node),
            Node::PropertyAssignment(node) => Some(node),
            Node::FunctionDeclaration(node) => Some(node),
            Node::ParameterDeclaration(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_named_declaration(&self) -> &dyn NamedDeclarationInterface {
        self.maybe_as_named_declaration()
            .expect("Expected named declaration")
    }

    pub fn as_member_name(&self) -> &dyn MemberNameInterface {
        match self {
            Node::Identifier(identifier) => identifier,
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

    pub fn maybe_as_has_type(&self) -> Option<&dyn HasTypeInterface> {
        match self {
            Node::VariableDeclaration(variable_declaration) => Some(variable_declaration),
            Node::PropertySignature(property_signature) => Some(property_signature),
            Node::FunctionDeclaration(function_declaration) => Some(function_declaration),
            Node::ParameterDeclaration(parameter_declaration) => Some(parameter_declaration),
            _ => None,
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
            Node::InterfaceDeclaration(interface_declaration) => interface_declaration,
            Node::TypeAliasDeclaration(type_alias_declaration) => type_alias_declaration,
            _ => panic!("Expected has type parameters"),
        }
    }

    pub fn as_has_type_arguments(&self) -> &dyn HasTypeArgumentsInterface {
        match self {
            Node::TypeReferenceNode(type_reference_node) => type_reference_node,
            _ => panic!("Expected has type arguments"),
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
            Node::ParenthesizedExpression(node) => node,
            Node::TypeAssertion(node) => node,
            Node::AsExpression(node) => node,
            Node::NonNullExpression(node) => node,
            Node::PartiallyEmittedExpression(node) => node,
            _ => panic!("Expected has expression"),
        }
    }

    pub fn as_signature_declaration(&self) -> &dyn SignatureDeclarationInterface {
        match self {
            Node::FunctionDeclaration(node) => node,
            Node::JSDocSignature(node) => node,
            _ => panic!("Expected signature declaration"),
        }
    }

    pub fn as_has_type(&self) -> &dyn HasTypeInterface {
        self.maybe_as_has_type().expect("Expected has type")
    }

    pub fn maybe_as_function_like_declaration(
        &self,
    ) -> Option<&dyn FunctionLikeDeclarationInterface> {
        match self {
            Node::FunctionDeclaration(function_declaration) => Some(function_declaration),
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
            Node::PropertyDeclaration(node) => node,
            Node::VariableDeclaration(node) => node,
            Node::ParameterDeclaration(node) => node,
            _ => panic!("Expected variable like declaration"),
        }
    }

    pub fn as_jsdoc_type_like_tag(&self) -> &dyn JSDocTypeLikeTagInterface {
        match self {
            Node::BaseJSDocTypeLikeTag(node) => node,
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

    pub fn as_interface_or_class_like_declaration(
        &self,
    ) -> &dyn InterfaceOrClassLikeDeclarationInterface {
        match self {
            Node::ClassDeclaration(node) => node,
            Node::ClassExpression(node) => node,
            Node::InterfaceDeclaration(node) => node,
            _ => panic!("Expected interface or class like declaration"),
        }
    }

    pub fn as_has_statements(&self) -> &dyn HasStatementsInterface {
        match self {
            Node::Block(node) => node,
            Node::ModuleBlock(node) => node,
            Node::SourceFile(node) => node,
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
            _ => panic!("Expected has statement"),
        }
    }

    pub fn as_has_label(&self) -> &dyn HasLabelInterface {
        match self {
            Node::BreakStatement(node) => node,
            Node::ContinueStatement(node) => node,
            _ => panic!("Expected has label"),
        }
    }

    pub fn as_has_question_token(&self) -> &dyn HasQuestionTokenInterface {
        match self {
            Node::PropertySignature(node) => node,
            Node::PropertyDeclaration(node) => node,
            _ => panic!("Expected has question token"),
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

    pub fn as_jsdoc_namespace_declaration(&self) -> &JSDocNamespaceDeclaration {
        enum_unwrapped!(self, [Node, JSDocNamespaceDeclaration])
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

    pub fn as_type_assertion_expression(&self) -> &TypeAssertion {
        enum_unwrapped!(self, [Node, TypeAssertion])
    }

    pub fn as_yield_expression(&self) -> &YieldExpression {
        enum_unwrapped!(self, [Node, YieldExpression])
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
}

#[derive(Debug)]
pub struct BaseNode {
    _node_wrapper: RefCell<Option<Weak<Node>>>,
    pub kind: SyntaxKind,
    flags: Cell<NodeFlags>,
    modifier_flags_cache: Cell<ModifierFlags>,
    transform_flags: Cell<TransformFlags>,
    pub decorators: RefCell<Option<NodeArray /*<Decorator>*/>>,
    pub modifiers: RefCell<Option<ModifiersArray>>,
    pub id: Cell<Option<NodeId>>,
    pub parent: RefCell<Option<Weak<Node>>>,
    pub original: RefCell<Option<Weak<Node>>>,
    pub pos: Cell<isize>,
    pub end: Cell<isize>,
    pub symbol: RefCell<Option<Weak<Symbol>>>,
    pub locals: RefCell<Option<Rc<RefCell<SymbolTable>>>>,
    next_container: RefCell<Option<Rc<Node>>>,
    local_symbol: RefCell<Option<Rc<Symbol>>>,
    emit_node: RefCell<Option<EmitNode>>,
    flow_node: RefCell<Option<Rc<FlowNode>>>,
    js_doc: RefCell<Option<Vec<Weak<Node>>>>,
    js_doc_cache: RefCell<Option<Vec<Weak<Node>>>>,
    intersects_change: Cell<Option<bool>>,
}

impl BaseNode {
    pub fn new(
        kind: SyntaxKind,
        flags: NodeFlags,
        transform_flags: TransformFlags,
        pos: isize,
        end: isize,
    ) -> Self {
        Self {
            _node_wrapper: RefCell::new(None),
            kind,
            flags: Cell::new(flags),
            modifier_flags_cache: Cell::new(ModifierFlags::None),
            transform_flags: Cell::new(transform_flags),
            decorators: RefCell::new(None),
            modifiers: RefCell::new(None),
            id: Cell::new(None),
            parent: RefCell::new(None),
            original: RefCell::new(None),
            pos: Cell::new(pos),
            end: Cell::new(end),
            symbol: RefCell::new(None),
            locals: RefCell::new(None),
            next_container: RefCell::new(None),
            local_symbol: RefCell::new(None),
            emit_node: RefCell::new(None),
            flow_node: RefCell::new(None),
            js_doc: RefCell::new(None),
            js_doc_cache: RefCell::new(None),
            intersects_change: Cell::new(None),
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

    fn maybe_decorators(&self) -> Ref<Option<NodeArray>> {
        self.decorators.borrow()
    }

    fn set_decorators(&self, decorators: Option<NodeArray>) {
        *self.decorators.borrow_mut() = decorators;
    }

    fn maybe_modifiers(&self) -> Ref<Option<NodeArray>> {
        self.modifiers.borrow()
    }

    fn set_modifiers(&self, modifiers: Option<NodeArray>) {
        *self.modifiers.borrow_mut() = modifiers;
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

    fn maybe_original(&self) -> Option<Rc<Node>> {
        self.original
            .borrow()
            .as_ref()
            .map(|weak| weak.upgrade().unwrap())
    }

    fn set_original(&self, original: Option<Rc<Node>>) {
        *self.original.borrow_mut() = original.map(|rc| Rc::downgrade(&rc));
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

    fn maybe_locals(&self) -> RefMut<Option<Rc<RefCell<SymbolTable>>>> {
        self.locals.borrow_mut()
    }

    fn locals(&self) -> RefMut<Rc<RefCell<SymbolTable>>> {
        RefMut::map(self.locals.borrow_mut(), |option| option.as_mut().unwrap())
    }

    fn set_locals(&self, locals: Option<Rc<RefCell<SymbolTable>>>) {
        *self.locals.borrow_mut() = locals;
    }

    fn maybe_next_container(&self) -> Option<Rc<Node>> {
        self.next_container.borrow().clone()
    }

    fn set_next_container(&self, next_container: Option<Rc<Node>>) {
        *self.next_container.borrow_mut() = next_container;
    }

    fn maybe_local_symbol(&self) -> Option<Rc<Symbol>> {
        self.local_symbol.borrow().as_ref().map(|rc| rc.clone())
    }

    fn set_local_symbol(&self, local_symbol: Option<Rc<Symbol>>) {
        *self.local_symbol.borrow_mut() = local_symbol;
    }

    fn maybe_emit_node(&self) -> RefMut<Option<EmitNode>> {
        self.emit_node.borrow_mut()
    }

    fn set_emit_node(&self, emit_node: Option<EmitNode>) {
        *self.emit_node.borrow_mut() = emit_node;
    }

    fn maybe_flow_node(&self) -> RefMut<Option<Rc<FlowNode>>> {
        self.flow_node.borrow_mut()
    }

    fn set_flow_node(&self, flow_node: Option<Rc<FlowNode>>) {
        *self.flow_node.borrow_mut() = flow_node;
    }

    fn maybe_js_doc(&self) -> Option<Vec<Rc<Node>>> {
        self.js_doc
            .borrow()
            .clone()
            .map(|vec| vec.iter().map(|weak| weak.upgrade().unwrap()).collect())
    }

    fn set_js_doc(&self, js_doc: Vec<Rc<Node>>) {
        *self.js_doc.borrow_mut() = Some(js_doc.iter().map(|rc| Rc::downgrade(rc)).collect());
    }

    fn maybe_js_doc_cache(&self) -> Option<Vec<Rc<Node>>> {
        self.js_doc_cache
            .borrow()
            .clone()
            .map(|vec| vec.iter().map(|weak| weak.upgrade().unwrap()).collect())
    }

    fn set_js_doc_cache(&self, js_doc_cache: Option<Vec<Rc<Node>>>) {
        *self.js_doc_cache.borrow_mut() = js_doc_cache
            .map(|js_doc_cache| js_doc_cache.iter().map(|rc| Rc::downgrade(rc)).collect());
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
    fn set_type(&mut self, type_: Option<Rc<Node>>);
}

pub trait HasInitializerInterface {
    fn maybe_initializer(&self) -> Option<Rc<Node>>;
    fn set_initializer(&mut self, initializer: Rc<Node>);
}
