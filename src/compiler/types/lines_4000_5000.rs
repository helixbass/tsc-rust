#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::fmt;
use std::rc::{Rc, Weak};

use super::{
    BaseType, CancellationTokenDebuggable, CompilerOptions, DiagnosticCollection,
    ExportedModulesFromDeclarationEmit, ExternalEmitHelpers, ModuleKind,
    ModuleSpecifierResolutionHost, Node, NodeCheckFlags, NodeId, NodeLinks, ObjectFlags,
    ParsedCommandLine, Path, RawSourceMap, RelationComparisonResult, ScriptTarget, Signature,
    SignatureFlags, SymbolTable, SymbolTracker, TransformationContext, TransformerFactory, Type,
    TypeFlags, TypeMapper, __String,
};
use crate::{
    Diagnostic, DuplicateInfoForFiles, FlowNode, FlowType, IndexInfo, IterationTypes,
    IterationTypesResolver, NodeBuilder, Number, PatternAmbientModule, ReverseMappedSymbol,
    StringOrNumber, TypeId, TypeSystemEntity, TypeSystemPropertyName,
};
use local_macros::{enum_unwrapped, symbol_type};

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

pub(crate) struct SourceMapEmitResult {
    pub input_source_file_names: Vec<String>,
    pub source_map: RawSourceMap,
}

#[allow(non_camel_case_types)]
pub enum ExitStatus {
    Success = 0,

    DiagnosticsPresent_OutputsSkipped = 1,

    DiagnosticsPresent_OutputsGenerated = 2,

    InvalidProject_OutputsSkipped = 3,

    ProjectReferenceCycle_OutputsSkipped = 4,
}

impl ExitStatus {
    pub const ProjectReferenceCycle_OutputsSkupped: ExitStatus =
        ExitStatus::ProjectReferenceCycle_OutputsSkipped;
}

pub struct EmitResult {
    pub emit_skipped: bool,
    pub diagnostics: Vec<Rc<Diagnostic>>,
    pub emitted_files: Option<Vec<String>>,
    pub(crate) source_maps: Option<Vec<SourceMapEmitResult>>,
    pub(crate) exported_modules_from_declaration_emit: Option<ExportedModulesFromDeclarationEmit>,
}

pub trait TypeCheckerHost: ModuleSpecifierResolutionHost {
    fn get_compiler_options(&self) -> Rc<CompilerOptions>;

    fn get_source_files(&self) -> &[Rc<Node /*SourceFile*/>];
    fn get_source_file(&self, file_name: &str) -> Option<Rc<Node /*SourceFile*/>>;
    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String>;
    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool;
}

pub trait TypeCheckerHostDebuggable: TypeCheckerHost + fmt::Debug {}

#[derive(Debug)]
#[allow(non_snake_case)]
pub struct TypeChecker {
    pub(crate) host: Rc<dyn TypeCheckerHostDebuggable>,
    pub(crate) produce_diagnostics: bool,
    pub(crate) _types_needing_strong_references: RefCell<Vec<Rc<Type>>>,
    pub(crate) _packages_map: RefCell<Option<HashMap<String, bool>>>,
    pub(crate) cancellation_token: RefCell<Option<Rc<dyn CancellationTokenDebuggable>>>,
    pub(crate) requested_external_emit_helpers: Cell<Option<ExternalEmitHelpers>>,
    pub(crate) external_helpers_module: RefCell<Option<Rc<Symbol>>>,
    pub(crate) Symbol: fn(SymbolFlags, __String) -> BaseSymbol,
    pub(crate) Type: fn(TypeFlags) -> BaseType,
    pub(crate) Signature: fn(SignatureFlags) -> Signature,
    pub(crate) type_count: Cell<u32>,
    pub(crate) symbol_count: Cell<usize>,
    pub(crate) enum_count: Cell<usize>,
    pub(crate) total_instantiation_count: Cell<usize>,
    pub(crate) instantiation_count: Cell<usize>,
    pub(crate) instantiation_depth: Cell<usize>,
    pub(crate) inline_level: Cell<usize>,
    pub(crate) current_node: RefCell<Option<Rc<Node>>>,
    pub(crate) empty_symbols: Rc<RefCell<SymbolTable>>,
    pub(crate) compiler_options: Rc<CompilerOptions>,
    pub(crate) language_version: ScriptTarget,
    pub(crate) module_kind: ModuleKind,
    pub(crate) use_define_for_class_fields: bool,
    pub(crate) allow_synthetic_default_imports: bool,
    pub(crate) strict_null_checks: bool,
    pub(crate) strict_function_types: bool,
    pub(crate) strict_bind_call_apply: bool,
    pub(crate) strict_property_initialization: bool,
    pub(crate) no_implicit_any: bool,
    pub(crate) no_implicit_this: bool,
    pub(crate) use_unknown_in_catch_variables: bool,
    pub(crate) keyof_strings_only: bool,
    pub(crate) fresh_object_literal_flag: ObjectFlags,
    pub(crate) exact_optional_property_types: Option<bool>,
    pub(crate) emit_resolver: Option<Rc<dyn EmitResolverDebuggable>>,
    pub(crate) node_builder: NodeBuilder,
    pub(crate) globals: Rc<RefCell<SymbolTable>>,
    pub(crate) undefined_symbol: Option<Rc<Symbol>>,
    pub(crate) global_this_symbol: Option<Rc<Symbol>>,
    pub(crate) arguments_symbol: Option<Rc<Symbol>>,
    pub(crate) require_symbol: Option<Rc<Symbol>>,
    pub(crate) apparent_argument_count: Cell<Option<usize>>,

    pub(crate) tuple_types: RefCell<HashMap<String, Rc</*GenericType*/ Type>>>,
    pub(crate) union_types: RefCell<HashMap<String, Rc</*UnionType*/ Type>>>,
    pub(crate) intersection_types: RefCell<HashMap<String, Rc<Type>>>,
    pub(crate) string_literal_types: RefCell<HashMap<String, Rc</*StringLiteralType*/ Type>>>,
    pub(crate) number_literal_types: RefCell<HashMap<Number, Rc</*NumberLiteralType*/ Type>>>,
    pub(crate) big_int_literal_types: RefCell<HashMap<String, Rc</*BigIntLiteralType*/ Type>>>,
    pub(crate) enum_literal_types: RefCell<HashMap<String, Rc</*LiteralType*/ Type>>>,
    pub(crate) indexed_access_types: RefCell<HashMap<String, Rc</*IndexedAccessType*/ Type>>>,
    pub(crate) template_literal_types: RefCell<HashMap<String, Rc</*TemplateLiteralType*/ Type>>>,
    pub(crate) string_mapping_types: RefCell<HashMap<String, Rc</*StringMappingType*/ Type>>>,
    pub(crate) substitution_types: RefCell<HashMap<String, Rc</*SubstitutionType*/ Type>>>,
    pub(crate) subtype_reduction_cache: RefCell<HashMap<String, Vec<Rc<Type>>>>,
    pub(crate) evolving_array_types: RefCell<Vec<Rc<Type /*EvolvingArrayType*/>>>,
    pub(crate) undefined_properties: RefCell<SymbolTable>,

    pub(crate) unknown_symbol: Option<Rc<Symbol>>,
    pub(crate) resolving_symbol: Option<Rc<Symbol>>,
    pub(crate) unresolved_symbols: RefCell<HashMap<String, Rc<Symbol /*TransientSymbol*/>>>,
    pub(crate) error_types: RefCell<HashMap<String, Rc<Type>>>,

    pub(crate) any_type: Option<Rc<Type>>,
    pub(crate) auto_type: Option<Rc<Type>>,
    pub(crate) wildcard_type: Option<Rc<Type>>,
    pub(crate) error_type: Option<Rc<Type>>,
    pub(crate) unresolved_type: Option<Rc<Type>>,
    pub(crate) non_inferrable_any_type: Option<Rc<Type>>,
    pub(crate) intrinsic_marker_type: Option<Rc<Type>>,
    pub(crate) unknown_type: Option<Rc<Type>>,
    pub(crate) non_null_unknown_type: Option<Rc<Type>>,
    pub(crate) undefined_type: Option<Rc<Type>>,
    pub(crate) undefined_widening_type: Option<Rc<Type>>,
    pub(crate) optional_type: Option<Rc<Type>>,
    pub(crate) missing_type: Option<Rc<Type>>,
    pub(crate) null_type: Option<Rc<Type>>,
    pub(crate) null_widening_type: Option<Rc<Type>>,
    pub(crate) string_type: Option<Rc<Type>>,
    pub(crate) number_type: Option<Rc<Type>>,
    pub(crate) bigint_type: Option<Rc<Type>>,
    pub(crate) false_type: Option<Rc<Type>>,
    pub(crate) regular_false_type: Option<Rc<Type>>,
    pub(crate) true_type: Option<Rc<Type>>,
    pub(crate) regular_true_type: Option<Rc<Type>>,
    pub(crate) boolean_type: Option<Rc<Type>>,
    pub(crate) es_symbol_type: Option<Rc<Type>>,
    pub(crate) void_type: Option<Rc<Type>>,
    pub(crate) never_type: Option<Rc<Type>>,
    pub(crate) silent_never_type: Option<Rc<Type>>,
    pub(crate) non_inferrable_type: Option<Rc<Type>>,
    pub(crate) implicit_never_type: Option<Rc<Type>>,
    pub(crate) unreachable_never_type: Option<Rc<Type>>,
    pub(crate) non_primitive_type: Option<Rc<Type>>,
    pub(crate) string_or_number_type: Option<Rc<Type>>,
    pub(crate) string_number_symbol_type: Option<Rc<Type>>,
    pub(crate) keyof_constraint_type: Option<Rc<Type>>,
    pub(crate) number_or_big_int_type: Option<Rc<Type>>,
    pub(crate) template_constraint_type: Option<Rc<Type>>,

    pub(crate) restrictive_mapper: Option<Rc<TypeMapper>>,
    pub(crate) permissive_mapper: Option<Rc<TypeMapper>>,

    pub(crate) empty_object_type: Option<Rc<Type>>,
    pub(crate) empty_jsx_object_type: Option<Rc<Type>>,
    pub(crate) empty_type_literal_symbol: Option<Rc<Symbol>>,
    pub(crate) empty_type_literal_type: Option<Rc<Type>>,

    pub(crate) empty_generic_type: Option<Rc<Type /*GenericType*/>>,

    pub(crate) any_function_type: Option<Rc<Type>>,

    pub(crate) no_constraint_type: Option<Rc<Type /*ResolvedType*/>>,
    pub(crate) circular_constraint_type: Option<Rc<Type /*ResolvedType*/>>,
    pub(crate) resolving_default_type: Option<Rc<Type>>,

    pub(crate) marker_super_type: Option<Rc<Type>>,
    pub(crate) marker_sub_type: Option<Rc<Type>>,
    pub(crate) marker_other_type: Option<Rc<Type>>,

    pub(crate) no_type_predicate: Option<Rc<TypePredicate>>,

    pub(crate) any_signature: Option<Rc<Signature>>,
    pub(crate) unknown_signature: Option<Rc<Signature>>,
    pub(crate) resolving_signature: Option<Rc<Signature>>,
    pub(crate) silent_never_signature: Option<Rc<Signature>>,

    pub(crate) enum_number_index_info: Option<Rc<IndexInfo>>,

    pub(crate) iteration_types_cache: RefCell<HashMap<String, IterationTypes>>,
    pub(crate) no_iteration_types: Rc<IterationTypes>,

    pub(crate) any_iteration_types: Option<Rc<IterationTypes>>,
    pub(crate) any_iteration_types_except_next: Option<Rc<IterationTypes>>,
    pub(crate) default_iteration_types: Option<Rc<IterationTypes>>,

    pub(crate) async_iteration_types_resolver: IterationTypesResolver,
    pub(crate) sync_iteration_types_resolver: IterationTypesResolver,

    pub(crate) amalgamated_duplicates: RefCell<Option<HashMap<String, DuplicateInfoForFiles>>>,

    pub(crate) reverse_mapped_cache: RefCell<HashMap<String, Option<Rc<Type>>>>,
    pub(crate) in_infer_type_for_homomorphic_mapped_type: Cell<bool>,
    pub(crate) ambient_modules_cache: RefCell<Option<Vec<Rc<Symbol>>>>,

    pub(crate) pattern_ambient_modules: RefCell<Option<Vec<PatternAmbientModule>>>,
    pub(crate) pattern_ambient_module_augmentations: RefCell<Option<HashMap<String, Rc<Symbol>>>>,

    pub(crate) global_object_type: Option<Rc<Type /*ObjectType*/>>,
    pub(crate) global_function_type: Option<Rc<Type /*ObjectType*/>>,
    pub(crate) global_callable_function_type: Option<Rc<Type /*ObjectType*/>>,
    pub(crate) global_newable_function_type: Option<Rc<Type /*ObjectType*/>>,
    pub(crate) global_array_type: Option<Rc<Type /*GenericType*/>>,
    pub(crate) global_readonly_array_type: Option<Rc<Type /*GenericType*/>>,
    pub(crate) global_string_type: Option<Rc<Type /*ObjectType*/>>,
    pub(crate) global_number_type: Option<Rc<Type /*ObjectType*/>>,
    pub(crate) global_boolean_type: Option<Rc<Type /*ObjectType*/>>,
    pub(crate) global_reg_exp_type: Option<Rc<Type /*ObjectType*/>>,
    pub(crate) global_this_type: Option<Rc<Type /*GenericType*/>>,
    pub(crate) any_array_type: Option<Rc<Type>>,
    pub(crate) auto_array_type: Option<Rc<Type>>,
    pub(crate) any_readonly_array_type: Option<Rc<Type>>,
    pub(crate) deferred_global_non_nullable_type_alias: RefCell<Option<Rc<Symbol>>>,

    pub(crate) deferred_global_es_symbol_constructor_symbol: RefCell<Option<Rc<Symbol>>>,
    pub(crate) deferred_global_es_symbol_constructor_type_symbol: RefCell<Option<Rc<Symbol>>>,
    pub(crate) deferred_global_es_symbol_type: RefCell<Option<Rc<Type /*ObjectType*/>>>,
    pub(crate) deferred_global_typed_property_descriptor_type:
        RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub(crate) deferred_global_promise_type: RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub(crate) deferred_global_promise_like_type: RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub(crate) deferred_global_promise_constructor_symbol: RefCell<Option<Rc<Symbol>>>,
    pub(crate) deferred_global_promise_constructor_like_type:
        RefCell<Option<Rc<Type /*ObjectType*/>>>,
    pub(crate) deferred_global_iterable_type: RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub(crate) deferred_global_iterator_type: RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub(crate) deferred_global_iterable_iterator_type: RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub(crate) deferred_global_generator_type: RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub(crate) deferred_global_iterator_yield_result_type:
        RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub(crate) deferred_global_iterator_return_result_type:
        RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub(crate) deferred_global_async_iterable_type: RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub(crate) deferred_global_async_iterator_type: RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub(crate) deferred_global_async_iterable_iterator_type:
        RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub(crate) deferred_global_async_generator_type: RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub(crate) deferred_global_template_strings_array_type:
        RefCell<Option<Rc<Type /*ObjectType*/>>>,
    pub(crate) deferred_global_import_meta_type: RefCell<Option<Rc<Type /*ObjectType*/>>>,
    pub(crate) deferred_global_import_meta_expression_type:
        RefCell<Option<Rc<Type /*ObjectType*/>>>,
    pub(crate) deferred_global_import_call_options_type: RefCell<Option<Rc<Type /*ObjectType*/>>>,
    pub(crate) deferred_global_extract_symbol: RefCell<Option<Rc<Symbol>>>,
    pub(crate) deferred_global_omit_symbol: RefCell<Option<Rc<Symbol>>>,
    pub(crate) deferred_global_awaited_symbol: RefCell<Option<Rc<Symbol>>>,
    pub(crate) deferred_global_big_int_type: RefCell<Option<Rc<Type /*ObjectType*/>>>,

    pub(crate) all_potentially_unused_identifiers:
        RefCell<HashMap<Path, Vec<Rc<Node /*PotentiallyUnusedIdentifier*/>>>>,

    pub(crate) flow_loop_start: Cell<usize>,
    pub(crate) flow_loop_count: Cell<usize>,
    pub(crate) shared_flow_count: Cell<usize>,
    pub(crate) flow_analysis_disabled: Cell<bool>,
    pub(crate) flow_invocation_count: Cell<usize>,
    pub(crate) last_flow_node: RefCell<Option<Rc<FlowNode>>>,
    pub(crate) last_flow_node_reachable: Cell<bool>,
    pub(crate) flow_type_cache: RefCell<Option<HashMap<NodeId, Rc<Type>>>>,

    pub(crate) empty_string_type: Option<Rc<Type>>,
    pub(crate) zero_type: Option<Rc<Type>>,
    pub(crate) zero_big_int_type: Option<Rc<Type>>,

    pub(crate) resolution_targets: RefCell<Vec<TypeSystemEntity>>,
    pub(crate) resolution_results: RefCell<Vec<bool>>,
    pub(crate) resolution_property_names: RefCell<Vec<TypeSystemPropertyName>>,

    pub(crate) suggestion_count: Cell<usize>,
    pub(crate) maximum_suggestion_count: usize,
    pub(crate) merged_symbols: RefCell<HashMap<u32, Rc<Symbol>>>,
    pub(crate) symbol_links: RefCell<HashMap<SymbolId, Rc<RefCell<SymbolLinks>>>>,
    pub(crate) node_links: RefCell<HashMap<NodeId, Rc<RefCell<NodeLinks>>>>,
    pub(crate) flow_loop_caches: RefCell<Vec<HashMap<String, Rc<Type>>>>,
    pub(crate) flow_loop_nodes: RefCell<Vec<Rc<FlowNode>>>,
    pub(crate) flow_loop_keys: RefCell<Vec<String>>,
    pub(crate) flow_loop_types: RefCell<Vec<Vec<Rc<Type>>>>,
    pub(crate) shared_flow_nodes: RefCell<Vec<Rc<FlowNode>>>,
    pub(crate) shared_flow_types: RefCell<Vec<Rc<FlowType>>>,
    pub(crate) flow_node_reachable: RefCell<HashMap<u32, Option<bool>>>,
    pub(crate) flow_node_post_super: RefCell<HashMap<u32, Option<bool>>>,
    pub(crate) potential_this_collisions: RefCell<Vec<Rc<Node>>>,
    pub(crate) potential_new_target_collisions: RefCell<Vec<Rc<Node>>>,
    pub(crate) potential_weak_map_set_collisions: RefCell<Vec<Rc<Node>>>,
    pub(crate) potential_reflect_collisions: RefCell<Vec<Rc<Node>>>,
    pub(crate) awaited_type_stack: RefCell<Vec<TypeId>>,

    pub(crate) diagnostics: RefCell<DiagnosticCollection>,
    pub(crate) suggestion_diagnostics: RefCell<DiagnosticCollection>,

    pub(crate) typeof_types_by_name: Option<HashMap<&'static str, Rc<Type>>>,
    pub(crate) typeof_type: Option<Rc<Type>>,

    pub(crate) _jsx_namespace: RefCell<Option<__String>>,
    pub(crate) _jsx_factory_entity: RefCell<Option<Rc<Node /*EntityName*/>>>,

    pub(crate) subtype_relation: RefCell<HashMap<String, RelationComparisonResult>>,
    pub(crate) strict_subtype_relation: RefCell<HashMap<String, RelationComparisonResult>>,
    pub(crate) assignable_relation: RefCell<HashMap<String, RelationComparisonResult>>,
    pub(crate) comparable_relation: RefCell<HashMap<String, RelationComparisonResult>>,
    pub(crate) identity_relation: RefCell<HashMap<String, RelationComparisonResult>>,
    pub(crate) enum_relation: RefCell<HashMap<String, RelationComparisonResult>>,

    pub(crate) builtin_globals: RefCell<Option<SymbolTable>>,

    pub(crate) suggested_extensions: Vec<(&'static str, &'static str)>,
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
        const GenerateNamesForShadowedTypeParams = 1 << 2;
        const UseStructuralFallback = 1 << 3;
        const ForbidIndexedAccessSymbolReferences = 1 << 4;
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
        const NoUndefinedOptionalParameterType = 1 << 30;

        const AllowThisInObjectLiteral = 1 << 15;
        const AllowQualifiedNameInPlaceOfIdentifier = 1 << 16;
        // const AllowQualifedNameInPlaceOfIdentifier = Self::AllowQualifiedNameInPlaceOfIdentifier.bits;
        const AllowAnonymousIdentifier = 1 << 17;
        const AllowEmptyUnionOrIntersection = 1 << 18;
        const AllowEmptyTuple = 1 << 19;
        const AllowUniqueESSymbolType = 1 << 20;
        const AllowEmptyIndexInfoType = 1 << 21;

        const AllowNodeModulesRelativePaths = 1 << 26;
        const DoNotIncludeSymbolChain = 1 << 27;

        const IgnoreErrors = Self::AllowThisInObjectLiteral.bits | Self::AllowQualifiedNameInPlaceOfIdentifier.bits | Self::AllowAnonymousIdentifier.bits | Self::AllowEmptyUnionOrIntersection.bits | Self::AllowEmptyTuple.bits | Self::AllowEmptyIndexInfoType.bits | Self::AllowNodeModulesRelativePaths.bits;

        const InObjectTypeLiteral = 1 << 22;
        const InTypeAlias = 1 << 23;
        const InInitialEntityName = 1 << 24;
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

        const AddUndefined = 1 << 17;
        const WriteArrowStyleSignature = 1 << 18;

        const InArrayType = 1 << 19;
        const InElementType = 1 << 21;
        const InFirstTypeArgument = 1 << 22;
        const InTypeAlias = 1 << 23;

        // const WriteOwnNameForAnyLike = 0;

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

pub struct SymbolWalker {}

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
    fn as_symbol_tracker(&self) -> &dyn SymbolTracker;
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
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

impl SymbolVisibilityResult {
    pub fn into_symbol_accessibility_result(self) -> SymbolAccessibilityResult {
        let SymbolVisibilityResult {
            accessibility,
            aliases_to_make_visible,
            error_symbol_name,
            error_node,
        } = self;
        SymbolAccessibilityResult {
            accessibility,
            aliases_to_make_visible,
            error_symbol_name,
            error_node,
            error_module_name: None,
        }
    }
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

pub trait EmitResolverDebuggable: EmitResolver + fmt::Debug {}

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
    fn maybe_declarations_mut(&self) -> RefMut<Option<Vec<Rc<Node>>>>;
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
    fn maybe_merge_id(&self) -> Option<u32>;
    fn set_merge_id(&self, merge_id: u32);
    fn maybe_parent(&self) -> Option<Rc<Symbol>>;
    fn set_parent(&self, parent: Option<Rc<Symbol>>);
    fn maybe_export_symbol(&self) -> Option<Rc<Symbol>>;
    fn set_export_symbol(&self, export_symbol: Option<Rc<Symbol>>);
    fn maybe_const_enum_only_module(&self) -> Option<bool>;
    fn set_const_enum_only_module(&self, const_enum_only_module: Option<bool>);
    fn maybe_is_referenced(&self) -> Option<SymbolFlags>;
    fn set_is_referenced(&self, is_referenced: Option<SymbolFlags>);
    fn maybe_is_replaceable_by_method(&self) -> Option<bool>;
    fn set_is_replaceable_by_method(&self, is_replaceable_by_method: Option<bool>);
    fn maybe_assignment_declaration_members(
        &self,
    ) -> RefMut<Option<HashMap<NodeId, Rc<Node /*Declaration*/>>>>;
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

    pub fn as_transient_symbol(&self) -> &TransientSymbol {
        enum_unwrapped!(self, [Symbol, TransientSymbol])
    }

    pub fn as_reverse_mapped_symbol(&self) -> &ReverseMappedSymbol {
        enum_unwrapped!(self, [Symbol, TransientSymbol, ReverseMappedSymbol])
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
    merge_id: Cell<Option<u32>>,
    parent: RefCell<Option<Rc<Symbol>>>,
    export_symbol: RefCell<Option<Rc<Symbol>>>,
    const_enum_only_module: Cell<Option<bool>>,
    is_referenced: Cell<Option<SymbolFlags>>,
    is_replaceable_by_method: Cell<Option<bool>>,
    assignment_declaration_members: RefCell<Option<HashMap<NodeId, Rc<Node /*Declaration*/>>>>,
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
            merge_id: Cell::new(None),
            parent: RefCell::new(None),
            export_symbol: RefCell::new(None),
            const_enum_only_module: Cell::new(None),
            is_referenced: Cell::new(None),
            is_replaceable_by_method: Cell::new(None),
            assignment_declaration_members: RefCell::new(None),
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

    fn maybe_declarations_mut(&self) -> RefMut<Option<Vec<Rc<Node>>>> {
        self.declarations.borrow_mut()
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

    fn maybe_merge_id(&self) -> Option<u32> {
        self.merge_id.get()
    }

    fn set_merge_id(&self, merge_id: u32) {
        self.merge_id.set(Some(merge_id));
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

    fn maybe_is_referenced(&self) -> Option<SymbolFlags> {
        self.is_referenced.get()
    }

    fn set_is_referenced(&self, is_referenced: Option<SymbolFlags>) {
        self.is_referenced.set(is_referenced);
    }

    fn maybe_is_replaceable_by_method(&self) -> Option<bool> {
        self.is_replaceable_by_method.get()
    }

    fn set_is_replaceable_by_method(&self, is_replaceable_by_method: Option<bool>) {
        self.is_replaceable_by_method.set(is_replaceable_by_method);
    }

    fn maybe_assignment_declaration_members(&self) -> RefMut<Option<HashMap<NodeId, Rc<Node>>>> {
        self.assignment_declaration_members.borrow_mut()
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
    pub referenced: Option<bool>,
    pub const_enum_referenced: Option<bool>,
    pub resolved_exports: Option<Rc<RefCell<SymbolTable>>>,
    pub originating_import: Option<Rc<Node /*ImportDeclaration | ImportCall*/>>,
    pub extended_containers: Option<Vec<Rc<Symbol>>>,
    pub extended_containers_by_file: Option<HashMap<NodeId, Vec<Rc<Symbol>>>>,
    pub cjs_export_merged: Option<Rc<Symbol>>,
    pub type_only_declaration: Option<Option<Rc<Node /*TypeOnlyAliasDeclaration | false*/>>>,
    pub accessible_chain_cache: Option<HashMap<String, Option<Vec<Rc<Symbol>>>>>,
}

impl SymbolLinks {
    pub fn new() -> Self {
        Self {
            target: None,
            type_: None,
            declared_type: None,
            mapper: None,
            referenced: None,
            const_enum_referenced: None,
            resolved_exports: None,
            originating_import: None,
            extended_containers: None,
            extended_containers_by_file: None,
            cjs_export_merged: None,
            type_only_declaration: None,
            accessible_chain_cache: None,
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
        const ReadPartial = 1 << 4;
        const WritePartial = 1 << 5;
        const HasNonUniformType = 1 << 6;
        const HasLiteralType = 1 << 7;
        const ContainsPublic = 1 << 8;
        const ContainsProtected = 1 << 3;
        const ContainsPrivate = 1 << 10;
        const ContainsStatic = 1 << 11;
        const Late = 1 << 12;
        const ReverseMapped = 1 << 13;
        const OptionalParameter = 1 << 14;
        const RestParameter = 1 << 15;
        const DeferredType = 1 << 16;
        const HasNeverType = 1 << 17;
        const Mapped = 1 << 18;
        const StripOptional = 1 << 19;
        const Unresolved = 1 << 20;
        const Synthetic = Self::SyntheticProperty.bits | Self::SyntheticMethod.bits;
        const Discriminant = Self::HasNonUniformType.bits | Self::HasLiteralType.bits;
        const Partial = Self::ReadPartial.bits | Self::WritePartial.bits;
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
    ReverseMappedSymbol(ReverseMappedSymbol),
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
