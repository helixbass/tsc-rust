#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use super::{
    BaseType, CompilerOptions, DiagnosticCollection, ModuleSpecifierResolutionHost, Node,
    NodeCheckFlags, NodeId, NodeLinks, ObjectFlags, ParsedCommandLine, Path,
    RelationComparisonResult, SymbolTable, SymbolTracker, TransformationContext,
    TransformerFactory, Type, TypeFlags, TypeMapper, __String,
};
use crate::{NodeBuilder, Number, StringOrNumber};
use local_macros::symbol_type;

pub type RedirectTargetsMap = HashMap<Path, Vec<String>>;

pub struct ResolvedProjectReference {
    pub command_line: ParsedCommandLine,
    pub source_file: Rc<Node /*SourceFile*/>,
    pub references: Option<Vec<Option<ResolvedProjectReference>>>,
}

#[derive(Eq, PartialEq)]
pub enum StructureIsReused {
    Not,
    Completely,
}

pub type CustomTransformerFactory = fn(Rc<dyn TransformationContext>) -> Rc<dyn CustomTransformer>;

pub trait CustomTransformer {
    fn transform_source_file(&self, node: &Node /*SourceFile*/) -> Rc<Node /*SourceFile*/>;
    fn transform_bundle(&self, node: &Node /*Bundle*/) -> Rc<Node /*Bundle*/>;
}

pub enum TransformerFactoryOrCustomTransformerFactory {
    TransformerFactory(TransformerFactory),
    CustomTransformerFactory(CustomTransformerFactory),
}

impl From<TransformerFactory> for TransformerFactoryOrCustomTransformerFactory {
    fn from(value: TransformerFactory) -> Self {
        Self::TransformerFactory(value)
    }
}

impl From<CustomTransformerFactory> for TransformerFactoryOrCustomTransformerFactory {
    fn from(value: CustomTransformerFactory) -> Self {
        Self::CustomTransformerFactory(value)
    }
}

pub struct CustomTransformers {
    pub before: Option<Vec<Rc<TransformerFactoryOrCustomTransformerFactory /*<SourceFile>*/>>>,
    pub after: Option<Vec<Rc<TransformerFactoryOrCustomTransformerFactory /*<SourceFile>*/>>>,
    pub after_declarations:
        Option<Vec<Rc<TransformerFactoryOrCustomTransformerFactory /*<Bundle | SourceFile>*/>>>,
}

pub struct EmitTransformers {
    pub script_transformers: Vec<TransformerFactory /*<SourceFile | Bundle>*/>,
    pub declaration_transformers: Vec<TransformerFactory /*<SourceFile | Bundle>*/>,
}

impl EmitTransformers {
    pub fn new(
        script_transformers: Vec<TransformerFactory>,
        declaration_transformers: Vec<TransformerFactory>,
    ) -> Self {
        Self {
            script_transformers,
            declaration_transformers,
        }
    }
}

#[allow(non_camel_case_types)]
pub enum ExitStatus {
    Success,
    DiagnosticsPresent_OutputsGenerated,
}

pub trait TypeCheckerHost: ModuleSpecifierResolutionHost {
    fn get_compiler_options(&self) -> Rc<CompilerOptions>;
    fn get_source_files(&self) -> Vec<Rc<Node>>;
}

#[derive(Debug)]
#[allow(non_snake_case)]
pub struct TypeChecker {
    pub _types_needing_strong_references: RefCell<Vec<Rc<Type>>>,
    pub Symbol: fn(SymbolFlags, __String) -> BaseSymbol,
    pub Type: fn(TypeFlags) -> BaseType,
    pub(crate) type_count: Cell<u32>,
    pub(crate) empty_symbols: Rc<RefCell<SymbolTable>>,
    pub(crate) compiler_options: Rc<CompilerOptions>,
    pub strict_null_checks: bool,
    pub fresh_object_literal_flag: ObjectFlags,
    pub exact_optional_property_types: bool,
    pub node_builder: NodeBuilder,
    pub globals: RefCell<SymbolTable>,
    pub string_literal_types: RefCell<HashMap<String, Rc</*StringLiteralType*/ Type>>>,
    pub number_literal_types: RefCell<HashMap<Number, Rc</*NumberLiteralType*/ Type>>>,
    pub big_int_literal_types: RefCell<HashMap<String, Rc</*BigIntLiteralType*/ Type>>>,
    pub unknown_symbol: Option<Rc<Symbol>>,
    pub any_type: Option<Rc<Type>>,
    pub error_type: Option<Rc<Type>>,
    pub undefined_type: Option<Rc<Type>>,
    pub null_type: Option<Rc<Type>>,
    pub string_type: Option<Rc<Type>>,
    pub number_type: Option<Rc<Type>>,
    pub bigint_type: Option<Rc<Type>>,
    pub true_type: Option<Rc<Type>>,
    pub regular_true_type: Option<Rc<Type>>,
    pub false_type: Option<Rc<Type>>,
    pub regular_false_type: Option<Rc<Type>>,
    pub boolean_type: Option<Rc<Type>>,
    pub never_type: Option<Rc<Type>>,
    pub number_or_big_int_type: Option<Rc<Type>>,
    pub template_constraint_type: Option<Rc<Type>>,
    pub global_array_type: Option<Rc<Type /*GenericType*/>>,
    pub symbol_links: RefCell<HashMap<SymbolId, Rc<RefCell<SymbolLinks>>>>,
    pub node_links: RefCell<HashMap<NodeId, Rc<RefCell<NodeLinks>>>>,
    pub diagnostics: RefCell<DiagnosticCollection>,
    pub assignable_relation: HashMap<String, RelationComparisonResult>,
    pub comparable_relation: HashMap<String, RelationComparisonResult>,
}

#[derive(PartialEq, Eq)]
pub enum UnionReduction {
    None,
    Literal,
    Subtype,
}

bitflags! {
    pub struct ContextFlags: u32 {
        const None = 0;
        const Signature = 1 << 0;
        const NoConstraints = 1 << 1;
        const Completions = 1 << 2;
        const SkipBindingPatters = 1 << 3;
    }
}

bitflags! {
    pub struct NodeBuilderFlags: u32 {
        const None = 0;
        const NoTruncation = 1 << 0;
        const WriteArrayAsGenericType = 1 << 1;
        const UseStructuralFallback = 1 << 3;
        const WriteTypeArgumentsOfSignature = 1 << 5;
        const UseFullyQualifiedType = 1 << 6;
        const UseOnlyExternalAliasing = 1 << 7;
        const SuppressAnyReturnType = 1 << 8;
        const WriteTypeParametersInQualifiedName = 1 << 9;
        const MultilineObjectLiterals = 1 << 10;
        const WriteClassExpressionAsTypeLiteral = 1 << 11;
        const UseTypeOfFunction = 1 << 12;
        const OmitParameterModifiers = 1 << 13;
        const UseAliasDefinedOutsideCurrentScope = 1 << 14;
        const UseSingleQuotesForStringLiteralType = 1 << 28;
        const NoTypeReduction = 1 << 29;

        const AllowThisInObjectLiteral = 1 << 15;
        const AllowQualifiedNameInPlaceOfIdentifier = 1 << 16;
        const AllowAnonymousIdentifier = 1 << 17;
        const AllowEmptyUnionOrIntersection = 1 << 18;
        const AllowEmptyTuple = 1 << 19;
        const AllowUniqueESSymbolType = 1 << 20;
        const AllowEmptyIndexInfoType = 1 << 21;

        const AllowNodeModulesRelativePaths = 1 << 26;
        const DoNotIncludeSymbolChain = 1 << 27;

        const InTypeAlias = 1 << 23;

        const IgnoreErrors = Self::AllowThisInObjectLiteral.bits | Self::AllowQualifiedNameInPlaceOfIdentifier.bits | Self::AllowAnonymousIdentifier.bits | Self::AllowEmptyUnionOrIntersection.bits | Self::AllowEmptyTuple.bits | Self::AllowEmptyIndexInfoType.bits | Self::AllowNodeModulesRelativePaths.bits;
    }
}

bitflags! {
    pub struct TypeFormatFlags: u32 {
        const None = 0;
        const NoTruncation = 1 << 0;
        const WriteArrayAsGenericType = 1 << 1;
        const UseStructuralFallback = 1 << 3;
        const WriteTypeArgumentsOfSignature = 1 << 5;
        const UseFullyQualifiedType = 1 << 6;
        const SuppressAnyReturnType = 1 << 8;
        const MultilineObjectLiterals = 1 << 10;
        const WriteClassExpressionAsTypeLiteral = 1 << 11;
        const UseTypeOfFunction = 1 << 12;
        const OmitParameterModifiers = 1 << 13;

        const UseAliasDefinedOutsideCurrentScope = 1 << 14;
        const UseSingleQuotesForStringLiteralType = 1 << 28;
        const NoTypeReduction = 1 << 29;

        const AllowUniqueESSymbolType = 1 << 20;

        const InTypeAlias = 1 << 23;

        const NodeBuilderFlagsMask = Self::NoTruncation.bits | Self::WriteArrayAsGenericType.bits | Self::UseStructuralFallback.bits | Self::WriteTypeArgumentsOfSignature.bits | Self::UseFullyQualifiedType.bits | Self::SuppressAnyReturnType.bits | Self::MultilineObjectLiterals.bits | Self::WriteClassExpressionAsTypeLiteral.bits | Self::UseTypeOfFunction.bits | Self::OmitParameterModifiers.bits | Self::UseAliasDefinedOutsideCurrentScope.bits | Self::AllowUniqueESSymbolType.bits | Self::InTypeAlias.bits | Self::UseSingleQuotesForStringLiteralType.bits | Self::NoTypeReduction.bits;
    }
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

pub trait SymbolWriter: SymbolTracker {
    fn write_keyword(&mut self, text: &str);
    fn write_operator(&mut self, text: &str);
    fn write_punctuation(&mut self, text: &str);
    fn write_space(&mut self, text: &str);
    fn write_string_literal(&mut self, text: &str);
    fn write_parameter(&mut self, text: &str);
    fn write_property(&mut self, text: &str);
    fn write_symbol(&mut self, text: &str, symbol: &Symbol);
    fn write_line(&mut self, force: Option<bool>);
    fn increase_indent(&mut self);
    fn decrease_indent(&mut self);
    fn clear(&mut self);
}

pub enum SymbolAccessibility {
    Accessible,
    NotAccessible,
    CannotBeNamed,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TypePredicateKind {
    This,
    Identifier,
    AssertsThis,
    AssertsIdentifier,
}

#[derive(Debug)]
pub struct TypePredicate {
    pub kind: TypePredicateKind,
    pub parameter_name: Option<String>,
    pub parameter_index: Option<usize>,
    pub type_: Option<Rc<Type>>,
}

pub struct SymbolVisibilityResult {
    pub accessibility: SymbolAccessibility,
    pub aliases_to_make_visible: Option<Vec<Rc<Node /*LateVisibilityPaintedStatement*/>>>,
    pub error_symbol_name: Option<String>,
    pub error_node: Option<Rc<Node>>,
}

pub struct SymbolAccessibilityResult {
    pub accessibility: SymbolAccessibility,
    pub aliases_to_make_visible: Option<Vec<Rc<Node /*LateVisibilityPaintedStatement*/>>>,
    pub error_symbol_name: Option<String>,
    pub error_node: Option<Rc<Node>>,
    pub error_module_name: Option<String>,
}

pub struct AllAccessorDeclarations {
    pub first_accessor: Rc<Node /*AccessorDeclaration*/>,
    pub second_accessor: Option<Rc<Node /*AccessorDeclaration*/>>,
    pub get_accessor: Option<Rc<Node /*GetAccessorDeclaration*/>>,
    pub set_accessor: Option<Rc<Node /*SetAccessorDeclaration*/>>,
}

pub enum TypeReferenceSerializationKind {
    Unknown,

    TypeWithConstructSignatureAndValue,

    VoidNullableOrNeverType,

    NumberLikeType,

    BigIntLikeType,

    StringLikeType,

    BooleanType,

    ArrayLikeType,

    ESSymbolType,

    Promise,

    TypeWithCallSignature,

    ObjectType,
}

pub trait EmitResolver {
    fn has_global_name(&self, name: &str) -> bool;
    fn get_referenced_export_container(
        &self,
        node: &Node, /*Identifier*/
        prefix_locals: Option<bool>,
    ) -> Option<Rc<Node /*SourceFile | ModuleDeclaration | EnumDeclaration*/>>;
    fn get_referenced_import_declaration(
        &self,
        node: &Node, /*Identifier*/
    ) -> Option<Rc<Node /*Declaration*/>>;
    fn is_declaration_with_colliding_name(&self, node: &Node /*Declaration*/) -> bool;
    fn is_value_alias_declaration(&self, node: &Node) -> bool;
    fn is_referenced_alias_declaration(&self, node: &Node, check_children: Option<bool>) -> bool;
    fn is_top_level_value_import_equals_with_entity_name(
        &self,
        node: &Node, /*ImportEqualsDeclaration*/
    ) -> bool;
    fn get_node_check_flags(&self, node: &Node) -> NodeCheckFlags;
    fn is_declaration_visible(&self, node: &Node /*Declaration | AnyImportSyntax*/) -> bool;
    fn is_late_bound(&self, node: &Node /*Declaration*/) -> bool;
    fn collect_linked_aliases(
        &self,
        node: &Node, /*Identifier*/
        set_visibility: Option<bool>,
    ) -> Option<Vec<Rc<Node>>>;
    fn is_implementation_of_overload(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) -> Option<bool>;
    fn is_required_initialized_parameter(&self, node: &Node /*ParameterDeclaration*/) -> bool;
    fn is_optional_uninitialized_parameter_property(
        &self,
        node: &Node, /*ParameterDeclaration*/
    ) -> bool;
    fn is_expando_function_declaration(&self, node: &Node /*FunctionDeclaration*/) -> bool;
    fn get_properties_of_container_function(
        &self,
        node: &Node, /*Declaration*/
    ) -> Vec<Rc<Symbol>>;
    fn create_type_of_declaration(
        &self,
        declaration: &Node, /*AccessorDeclaration | VariableLikeDeclaration | PropertyAccessExpression*/
        enclosing_declaration: &Node,
        flags: NodeBuilderFlags,
        tracker: &dyn SymbolTracker,
        add_undefined: Option<bool>,
    ) -> Option<Rc<Node /*TypeNode*/>>;
    fn create_return_type_of_signature_declaration(
        &self,
        signature_declaration: &Node, /*SignatureDeclaration*/
        enclosing_declaration: &Node,
        flags: NodeBuilderFlags,
        tracker: &dyn SymbolTracker,
    ) -> Option<Rc<Node /*TypeNode*/>>;
    fn create_type_of_expression(
        &self,
        expr: &Node, /*Expression*/
        enclosing_declaration: &Node,
        flags: NodeBuilderFlags,
        tracker: &dyn SymbolTracker,
    ) -> Option<Rc<Node /*TypeNode*/>>;
    fn create_literal_const_value(
        &self,
        node: &Node, /*VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration*/
        tracker: &dyn SymbolTracker,
    ) -> Rc<Node /*Expression*/>;
    fn is_symbol_accessible(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<&Node>,
        meaning: Option<SymbolFlags>,
        should_compute_alias_to_mark_visible: bool,
    ) -> SymbolAccessibilityResult;
    fn is_entity_name_visible(
        &self,
        entity_name: &Node, /*EntityNameOrEntityNameExpression*/
        enclosing_declaration: &Node,
    ) -> SymbolVisibilityResult;
    fn get_constant_value(
        &self,
        node: &Node, /*EnumMember | PropertyAccessExpression | ElementAccessExpression*/
    ) -> Option<StringOrNumber>;
    fn get_referenced_value_declaration(
        &self,
        reference: &Node, /*Identifier*/
    ) -> Option<Rc<Node /*Declaration*/>>;
    fn get_type_reference_serialization_kind(
        &self,
        type_name: &Node, /*EntityName*/
        location: Option<&Node>,
    ) -> TypeReferenceSerializationKind;
    fn is_optional_parameter(&self, node: &Node /*ParameterDeclaration*/) -> bool;
    fn module_exports_some_value(
        &self,
        module_reference_expression: &Node, /*Expression*/
    ) -> bool;
    fn is_arguments_local_binding(&self, node: &Node /*Identifier*/) -> bool;
    fn get_external_module_file_from_declaration(
        &self,
        declaration: &Node, /*ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration | ModuleDeclaration | ImportTypeNode | ImportCall*/
    ) -> Option<Rc<Node /*SourceFile*/>>;
    fn get_type_reference_directives_for_entity_name(
        &self,
        name: &Node, /*EntityNameOrEntityNameExpression*/
    ) -> Option<Vec<String>>;
    fn get_type_reference_directives_for_symbol(
        &self,
        symbol: &Symbol,
        meaning: Option<SymbolFlags>,
    ) -> Option<Vec<String>>;
    fn is_literal_const_declaration(
        &self,
        node: &Node, /*VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration*/
    ) -> bool;
    fn get_jsx_factory_entity(&self, location: Option<&Node>) -> Option<Rc<Node /*EntityName*/>>;
    fn get_jsx_fragment_factory_entity(
        &self,
        location: Option<&Node>,
    ) -> Option<Rc<Node /*EntityName*/>>;
    fn get_all_accessor_declarations(
        &self,
        declaration: &Node, /*AccessorDeclaration*/
    ) -> AllAccessorDeclarations;
    fn get_symbol_of_external_module_specifier(
        &self,
        node: &Node, /*StringLiteralLike*/
    ) -> Option<Rc<Symbol>>;
    fn is_binding_captured_by_node(
        &self,
        node: &Node,
        decl: &Node, /*VariableDeclaration | BindingElement*/
    ) -> bool;
    fn get_declaration_statements_for_source_file(
        &self,
        node: &Node, /*SourceFile*/
        flags: NodeBuilderFlags,
        tracker: &dyn SymbolTracker,
        bundled: Option<bool>,
    ) -> Option<Vec<Rc<Node /*Statement*/>>>;
    fn is_import_required_by_augmentation(&self, decl: &Node /*ImportDeclaration*/) -> bool;
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
        const NamespaceModule = 1 << 10;
        const TypeLiteral = 1 << 11;
        const ObjectLiteral = 1 << 12;
        const Method = 1 << 13;
        const Constructor = 1 << 14;
        const GetAccessor = 1 << 15;
        const SetAccessor = 1 << 16;
        const Signature = 1 << 17;
        const TypeParameter = 1 << 18;
        const TypeAlias = 1 << 19;
        const ExportValue = 1 << 20;
        const Alias = 1 << 21;
        const Prototype = 1 << 22;
        const ExportStar = 1 << 23;
        const Optional = 1 << 24;
        const Transient = 1 << 25;
        const Assignment = 1 << 26;
        const ModuleExports = 1 << 27;

        const All = Self::FunctionScopedVariable.bits | Self::BlockScopedVariable.bits | Self::Property.bits | Self::EnumMember.bits | Self::Function.bits | Self::Class.bits | Self::Interface.bits | Self::ConstEnum.bits | Self::RegularEnum.bits | Self::ValueModule.bits | Self::NamespaceModule.bits | Self::TypeLiteral.bits | Self::ObjectLiteral.bits | Self::Method.bits | Self::Constructor.bits | Self::GetAccessor.bits | Self::SetAccessor.bits | Self::Signature.bits | Self::TypeParameter.bits | Self::TypeAlias.bits | Self::ExportValue.bits | Self::Alias.bits | Self::Prototype.bits | Self::ExportStar.bits | Self::Optional.bits | Self::Transient.bits;

        const Enum = Self::RegularEnum.bits | Self::ConstEnum.bits;
        const Variable = Self::FunctionScopedVariable.bits | Self::BlockScopedVariable.bits;
        const Value = Self::Variable.bits | Self::Property.bits | Self::EnumMember.bits | Self::ObjectLiteral.bits | Self::Function.bits | Self::Class.bits | Self::Enum.bits | Self::ValueModule.bits | Self::Method.bits | Self::GetAccessor.bits | Self::SetAccessor.bits;
        const Type = Self::Class.bits | Self::Interface.bits | Self::Enum.bits | Self::EnumMember.bits | Self::TypeLiteral.bits | Self::TypeParameter.bits | Self::TypeAlias.bits;
        const Namespace = Self::ValueModule.bits | Self::NamespaceModule.bits | Self::Enum.bits;
        const Module = Self::ValueModule.bits | Self::NamespaceModule.bits;
        const Accessor = Self::GetAccessor.bits | Self::SetAccessor.bits;

        const FunctionScopedVariableExcludes = Self::Value.bits & !Self::FunctionScopedVariable.bits;

        const BlockScopedVariableExcludes = Self::Value.bits;

        const ParameterExcludes = Self::Value.bits;
        const PropertyExcludes = Self::None.bits;
        const EnumMemberExcludes = Self::None.bits | Self::Type.bits;
        const FunctionExcludes = Self::Value.bits & !(Self::Function.bits | Self::ValueModule.bits | Self::Class.bits);
        const ClassExcludes = (Self::Value.bits | Self::Type.bits) & !(Self::ValueModule.bits | Self::Interface.bits | Self::Function.bits);
        const InterfaceExcludes = Self::Type.bits & !(Self::Interface.bits | Self::Class.bits);
        const RegularEnumExcludes = (Self::Value.bits | Self::Type.bits) & !(Self::RegularEnum.bits | Self::ValueModule.bits);
        const ConstEnumExcludes = (Self::Value.bits | Self::Type.bits) & !Self::ConstEnum.bits;
        const ValueModuleExcludes = Self::Value.bits & !(Self::Function.bits | Self::Class.bits | Self::RegularEnum.bits | Self::ValueModule.bits);
        const NamespaceModuleExcludes = Self::None.bits;
        const MethodExcludes = Self::Value.bits & !Self::Method.bits;
        const GetAccessorExcludes = Self::Value.bits & !Self::SetAccessor.bits;
        const SetAccessorExcludes = Self::Value.bits & !Self::GetAccessor.bits;
        const TypeParameterExcludes = Self::Type.bits & !Self::TypeParameter.bits;
        const TypeAliasExcludes = Self::Type.bits;
        const AliasExcludes = Self::Alias.bits;

        const ModuleMember = Self::Variable.bits | Self::Function.bits | Self::Class.bits | Self::Interface.bits | Self::Enum.bits | Self::Module.bits | Self::TypeAlias.bits | Self::Alias.bits;

        const ExportHasLocal = Self::Function.bits | Self::Class.bits | Self::Enum.bits | Self::ValueModule.bits;

        const BlockScoped = Self::BlockScopedVariable.bits | Self::Class.bits | Self::Enum.bits;

        const PropertyOrAccessor = Self::Property.bits | Self::Accessor.bits;

        const ClassMember = Self::Method.bits | Self::Accessor.bits | Self::Property.bits;

        const ExportSupportsDefaultModifier = Self::Class.bits | Self::Function.bits | Self::Interface.bits;

        const ExportDoesNotSupportDefaultModifier = !Self::ExportSupportsDefaultModifier.bits;

        const Classifiable = Self::Class.bits | Self::Enum.bits | Self::TypeAlias.bits | Self::Interface.bits | Self::TypeParameter.bits | Self::Module.bits | Self::Alias.bits;

        const LateBindingContainer = Self::Class.bits | Self::Interface.bits | Self::TypeLiteral.bits | Self::ObjectLiteral.bits | Self::Function.bits;
    }
}

pub type SymbolId = u32;

pub trait SymbolInterface {
    fn symbol_wrapper(&self) -> Rc<Symbol>;
    fn set_symbol_wrapper(&self, wrapper: Rc<Symbol>);
    fn flags(&self) -> SymbolFlags;
    fn set_flags(&self, flags: SymbolFlags);
    fn escaped_name(&self) -> &__String;
    fn maybe_declarations(&self) -> Ref<Option<Vec<Rc<Node>>>>;
    fn set_declarations(&self, declarations: Vec<Rc<Node>>);
    fn maybe_value_declaration(&self) -> Option<Rc<Node>>;
    fn set_value_declaration(&self, node: Rc<Node>);
    fn maybe_members(&self) -> RefMut<Option<Rc<RefCell<SymbolTable>>>>;
    fn members(&self) -> Rc<RefCell<SymbolTable>>;
    fn maybe_exports(&self) -> RefMut<Option<Rc<RefCell<SymbolTable>>>>;
    fn exports(&self) -> Rc<RefCell<SymbolTable>>;
    fn maybe_global_exports(&self) -> RefMut<Option<Rc<RefCell<SymbolTable>>>>;
    fn maybe_id(&self) -> Option<SymbolId>;
    fn id(&self) -> SymbolId;
    fn set_id(&self, id: SymbolId);
    fn maybe_parent(&self) -> Option<Rc<Symbol>>;
    fn set_parent(&self, parent: Option<Rc<Symbol>>);
    fn maybe_export_symbol(&self) -> Option<Rc<Symbol>>;
    fn set_export_symbol(&self, export_symbol: Option<Rc<Symbol>>);
    fn maybe_const_enum_only_module(&self) -> Option<bool>;
    fn set_const_enum_only_module(&self, const_enum_only_module: Option<bool>);
    fn maybe_is_replaceable_by_method(&self) -> Option<bool>;
    fn set_is_replaceable_by_method(&self, is_replaceable_by_method: Option<bool>);
}

#[derive(Debug)]
#[symbol_type(impl_from = false)]
pub enum Symbol {
    BaseSymbol(BaseSymbol),
    TransientSymbol(TransientSymbol),
}

impl Symbol {
    pub fn wrap(self) -> Rc<Symbol> {
        let rc = Rc::new(self);
        rc.set_symbol_wrapper(rc.clone());
        rc
    }
}

#[derive(Debug)]
pub struct BaseSymbol {
    _symbol_wrapper: RefCell<Option<Weak<Symbol>>>,
    flags: Cell<SymbolFlags>,
    escaped_name: __String,
    declarations: RefCell<Option<Vec<Rc<Node /*Declaration*/>>>>, // TODO: should be Vec<Weak<Node>> instead of Vec<Rc<Node>>?
    value_declaration: RefCell<Option<Weak<Node>>>,
    members: RefCell<Option<Rc<RefCell<SymbolTable>>>>,
    exports: RefCell<Option<Rc<RefCell<SymbolTable>>>>,
    global_exports: RefCell<Option<Rc<RefCell<SymbolTable>>>>,
    id: Cell<Option<SymbolId>>,
    parent: RefCell<Option<Rc<Symbol>>>,
    export_symbol: RefCell<Option<Rc<Symbol>>>,
    const_enum_only_module: Cell<Option<bool>>,
    is_replaceable_by_method: Cell<Option<bool>>,
}

impl BaseSymbol {
    pub fn new(flags: SymbolFlags, name: __String) -> Self {
        Self {
            _symbol_wrapper: RefCell::new(None),
            flags: Cell::new(flags),
            escaped_name: name,
            declarations: RefCell::new(None),
            value_declaration: RefCell::new(None),
            members: RefCell::new(None),
            exports: RefCell::new(None),
            global_exports: RefCell::new(None),
            id: Cell::new(None),
            parent: RefCell::new(None),
            export_symbol: RefCell::new(None),
            const_enum_only_module: Cell::new(None),
            is_replaceable_by_method: Cell::new(None),
        }
    }
}

impl SymbolInterface for BaseSymbol {
    fn symbol_wrapper(&self) -> Rc<Symbol> {
        self._symbol_wrapper
            .borrow()
            .as_ref()
            .unwrap()
            .upgrade()
            .unwrap()
    }

    fn set_symbol_wrapper(&self, wrapper: Rc<Symbol>) {
        *self._symbol_wrapper.borrow_mut() = Some(Rc::downgrade(&wrapper));
    }

    fn flags(&self) -> SymbolFlags {
        self.flags.get()
    }

    fn set_flags(&self, flags: SymbolFlags) {
        self.flags.set(flags);
    }

    fn escaped_name(&self) -> &__String {
        &self.escaped_name
    }

    fn maybe_declarations(&self) -> Ref<Option<Vec<Rc<Node>>>> {
        self.declarations.borrow()
    }

    fn set_declarations(&self, declarations: Vec<Rc<Node>>) {
        *self.declarations.borrow_mut() = Some(declarations);
    }

    fn maybe_value_declaration(&self) -> Option<Rc<Node>> {
        self.value_declaration
            .borrow()
            .as_ref()
            .map(|weak| weak.upgrade().unwrap())
    }

    fn set_value_declaration(&self, node: Rc<Node>) {
        *self.value_declaration.borrow_mut() = Some(Rc::downgrade(&node));
    }

    fn maybe_members(&self) -> RefMut<Option<Rc<RefCell<SymbolTable>>>> {
        self.members.borrow_mut()
    }

    fn members(&self) -> Rc<RefCell<SymbolTable>> {
        self.members.borrow_mut().as_ref().unwrap().clone()
    }

    fn maybe_exports(&self) -> RefMut<Option<Rc<RefCell<SymbolTable>>>> {
        self.exports.borrow_mut()
    }

    fn exports(&self) -> Rc<RefCell<SymbolTable>> {
        self.exports.borrow_mut().as_ref().unwrap().clone()
    }

    fn maybe_global_exports(&self) -> RefMut<Option<Rc<RefCell<SymbolTable>>>> {
        self.global_exports.borrow_mut()
    }

    fn maybe_id(&self) -> Option<SymbolId> {
        self.id.get()
    }

    fn id(&self) -> SymbolId {
        self.id.get().unwrap()
    }

    fn set_id(&self, id: SymbolId) {
        self.id.set(Some(id));
    }

    fn maybe_parent(&self) -> Option<Rc<Symbol>> {
        self.parent.borrow().as_ref().map(Clone::clone)
    }

    fn set_parent(&self, parent: Option<Rc<Symbol>>) {
        *self.parent.borrow_mut() = parent;
    }

    fn maybe_export_symbol(&self) -> Option<Rc<Symbol>> {
        self.export_symbol.borrow().as_ref().map(Clone::clone)
    }

    fn set_export_symbol(&self, export_symbol: Option<Rc<Symbol>>) {
        *self.export_symbol.borrow_mut() = export_symbol;
    }

    fn maybe_const_enum_only_module(&self) -> Option<bool> {
        self.const_enum_only_module.get()
    }

    fn set_const_enum_only_module(&self, const_enum_only_module: Option<bool>) {
        self.const_enum_only_module.set(const_enum_only_module);
    }

    fn maybe_is_replaceable_by_method(&self) -> Option<bool> {
        self.is_replaceable_by_method.get()
    }

    fn set_is_replaceable_by_method(&self, is_replaceable_by_method: Option<bool>) {
        self.is_replaceable_by_method.set(is_replaceable_by_method);
    }
}

impl From<BaseSymbol> for Symbol {
    fn from(base_symbol: BaseSymbol) -> Self {
        Symbol::BaseSymbol(base_symbol)
    }
}

#[derive(Debug)]
pub struct SymbolLinks {
    pub target: Option<Rc<Symbol>>,
    pub type_: Option<Rc<Type>>,
    pub declared_type: Option<Rc<Type>>,
    pub mapper: Option<TypeMapper>,
}

impl SymbolLinks {
    pub fn new() -> Self {
        Self {
            target: None,
            type_: None,
            declared_type: None,
            mapper: None,
        }
    }
}

bitflags! {
    pub struct CheckFlags: u32 {
        const None = 0;
        const Instantiated = 1 << 0;
        const SyntheticProperty = 1 << 1;
        const SyntheticMethod = 1 << 2;
        const Readonly = 1 << 3;
        const Late = 1 << 12;
        const OptionalParameter = 1 << 14;
        const RestParameter = 1 << 15;

        const Synthetic = Self::SyntheticProperty.bits | Self::SyntheticMethod.bits;
    }
}

pub trait TransientSymbolInterface: SymbolInterface {
    fn symbol_links(&self) -> Rc<RefCell<SymbolLinks>>;
    fn check_flags(&self) -> CheckFlags;
}

#[derive(Debug)]
#[symbol_type(interfaces = "TransientSymbolInterface")]
pub enum TransientSymbol {
    BaseTransientSymbol(BaseTransientSymbol),
}

#[derive(Debug)]
#[symbol_type(ancestors = "TransientSymbol")]
pub struct BaseTransientSymbol {
    _symbol: BaseSymbol,
    _symbol_links: Rc<RefCell<SymbolLinks>>,
    check_flags: CheckFlags,
}

impl BaseTransientSymbol {
    pub fn new(base_symbol: BaseSymbol, check_flags: CheckFlags) -> Self {
        Self {
            _symbol: base_symbol,
            _symbol_links: Rc::new(RefCell::new(SymbolLinks::new())),
            check_flags,
        }
    }
}

impl TransientSymbolInterface for BaseTransientSymbol {
    fn symbol_links(&self) -> Rc<RefCell<SymbolLinks>> {
        self._symbol_links.clone()
    }

    fn check_flags(&self) -> CheckFlags {
        self.check_flags
    }
}
