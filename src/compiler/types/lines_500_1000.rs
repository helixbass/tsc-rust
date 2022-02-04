#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, Ref, RefCell, RefMut};
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
    NamedExports, NamedImports, NamedTupleMember, NamespaceExport, NamespaceExportDeclaration,
    NamespaceImport, NewExpression, NodeArray, NonNullExpression, NumericLiteral,
    ObjectBindingPattern, ObjectLiteralExpression, OmittedExpression, OptionalTypeNode,
    ParameterDeclaration, ParenthesizedExpression, ParenthesizedTypeNode,
    PartiallyEmittedExpression, PostfixUnaryExpression, PrefixUnaryExpression, PrivateIdentifier,
    PropertyAccessExpression, PropertyAssignment, PropertyDeclaration, PropertySignature,
    QualifiedName, ReadonlyTextRange, RegularExpressionLiteral, RestTypeNode, ReturnStatement,
    SemicolonClassElement, SetAccessorDeclaration, ShorthandPropertyAssignment,
    SignatureDeclarationBase, SignatureDeclarationInterface, SourceFile, SpreadAssignment,
    SpreadElement, StringLiteral, SwitchStatement, Symbol, SymbolTable, SyntaxKind,
    TaggedTemplateExpression, TemplateExpression, TemplateLiteralLikeNode,
    TemplateLiteralLikeNodeInterface, TemplateLiteralTypeNode, TemplateLiteralTypeSpan,
    TemplateSpan, ThisTypeNode, ThrowStatement, TransformFlags, TryStatement, TupleTypeNode,
    TypeAliasDeclaration, TypeAssertion, TypeElement, TypeLiteralNode, TypeOfExpression,
    TypeOperatorNode, TypeParameterDeclaration, TypePredicateNode, TypeQueryNode,
    TypeReferenceNode, UnionOrIntersectionTypeNodeInterface, UnionTypeNode, VariableDeclaration,
    VariableDeclarationList, VariableLikeDeclarationInterface, VariableStatement, VoidExpression,
    WhileStatement, WithStatement, YieldExpression,
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
    fn set_transform_flags(&mut self, flags: TransformFlags);
    fn add_transform_flags(&mut self, flags: TransformFlags);
    fn maybe_decorators(&self) -> Ref<Option<NodeArray>>;
    fn set_decorators(&self, decorators: Option<NodeArray>);
    fn maybe_modifiers(&self) -> Option<&NodeArray>;
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
    fn maybe_locals(&self) -> RefMut<Option<SymbolTable>>;
    fn locals(&self) -> RefMut<SymbolTable>;
    fn set_locals(&self, locals: Option<SymbolTable>);
    fn maybe_emit_node(&self) -> RefMut<Option<EmitNode>>;
    fn set_emit_node(&self, emit_node: Option<EmitNode>);
    fn maybe_js_doc(&self) -> Option<Vec<Rc<Node /*JSDoc*/>>>;
    fn set_js_doc(&self, js_doc: Vec<Rc<Node /*JSDoc*/>>);
    fn maybe_js_doc_cache(&self) -> Option<Vec<Rc<Node /*JSDocTag*/>>>;
    fn set_js_doc_cache(&self, js_doc_cache: Vec<Rc<Node /*JSDocTag*/>>);
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
    TypeElement(TypeElement),
    PropertyAssignment(PropertyAssignment),
    SourceFile(SourceFile),
    QualifiedName(QualifiedName),
    JSDocTag(JSDocTag),
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
            Node::TypeElement(node) => Some(node),
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
            Node::TypeElement(TypeElement::PropertySignature(property_signature)) => {
                Some(property_signature)
            }
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
            Node::TypeElement(TypeElement::PropertySignature(node)) => Some(node),
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
            Node::JSDocTag(JSDocTag::BaseJSDocTypeLikeTag(node)) => node,
            _ => panic!("Expected JSDoc type like tag"),
        }
    }

    pub fn as_template_literal_like_node(&self) -> &dyn TemplateLiteralLikeNodeInterface {
        match self {
            Node::TemplateLiteralLikeNode(node) => node,
            _ => panic!("Expected template literal like node"),
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
        enum_unwrapped!(self, [Node, JSDocTag, JSDocPropertyLikeTag])
    }

    pub fn as_jsdoc_template_tag(&self) -> &JSDocTemplateTag {
        enum_unwrapped!(self, [Node, JSDocTag, JSDocTemplateTag])
    }

    pub fn as_new_expression(&self) -> &NewExpression {
        enum_unwrapped!(self, [Node, NewExpression])
    }

    pub fn as_jsdoc_typedef_tag(&self) -> &JSDocTypedefTag {
        enum_unwrapped!(self, [Node, JSDocTag, JSDocTypedefTag])
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
        enum_unwrapped!(self, [Node, TypeElement, PropertySignature])
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
}

#[derive(Debug)]
pub struct BaseNode {
    _node_wrapper: RefCell<Option<Weak<Node>>>,
    pub kind: SyntaxKind,
    flags: Cell<NodeFlags>,
    modifier_flags_cache: Cell<ModifierFlags>,
    transform_flags: TransformFlags,
    pub decorators: RefCell<Option<NodeArray /*<Decorator>*/>>,
    pub modifiers: Option<ModifiersArray>,
    pub id: Cell<Option<NodeId>>,
    pub parent: RefCell<Option<Weak<Node>>>,
    pub original: RefCell<Option<Weak<Node>>>,
    pub pos: Cell<isize>,
    pub end: Cell<isize>,
    pub symbol: RefCell<Option<Weak<Symbol>>>,
    pub locals: RefCell<Option<SymbolTable>>,
    emit_node: RefCell<Option<EmitNode>>,
    js_doc: RefCell<Option<Vec<Weak<Node>>>>,
    js_doc_cache: RefCell<Option<Vec<Weak<Node>>>>,
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
            transform_flags,
            decorators: RefCell::new(None),
            modifiers: None,
            id: Cell::new(None),
            parent: RefCell::new(None),
            original: RefCell::new(None),
            pos: Cell::new(pos),
            end: Cell::new(end),
            symbol: RefCell::new(None),
            locals: RefCell::new(None),
            emit_node: RefCell::new(None),
            js_doc: RefCell::new(None),
            js_doc_cache: RefCell::new(None),
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
        self.transform_flags
    }

    fn set_transform_flags(&mut self, flags: TransformFlags) {
        self.transform_flags = flags;
    }

    fn add_transform_flags(&mut self, flags: TransformFlags) {
        self.transform_flags |= flags;
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

    fn maybe_locals(&self) -> RefMut<Option<SymbolTable>> {
        self.locals.borrow_mut()
    }

    fn locals(&self) -> RefMut<SymbolTable> {
        RefMut::map(self.locals.borrow_mut(), |option| option.as_mut().unwrap())
    }

    fn set_locals(&self, locals: Option<SymbolTable>) {
        *self.locals.borrow_mut() = locals;
    }

    fn maybe_emit_node(&self) -> RefMut<Option<EmitNode>> {
        self.emit_node.borrow_mut()
    }

    fn set_emit_node(&self, emit_node: Option<EmitNode>) {
        *self.emit_node.borrow_mut() = emit_node;
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

    fn set_js_doc_cache(&self, js_doc_cache: Vec<Rc<Node>>) {
        *self.js_doc_cache.borrow_mut() =
            Some(js_doc_cache.iter().map(|rc| Rc::downgrade(rc)).collect());
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

pub trait HasInitializerInterface {
    fn maybe_initializer(&self) -> Option<Rc<Node>>;
    fn set_initializer(&mut self, initializer: Rc<Node>);
}
