use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt, io,
    rc::Rc,
};

use bitflags::bitflags;
use derive_builder::Builder;
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use id_arena::Id;
use local_macros::{enum_unwrapped, symbol_type};

use super::{
    BaseType, CancellationTokenDebuggable, CompilerOptions, DiagnosticCollection,
    ExportedModulesFromDeclarationEmit, ExternalEmitHelpers, ModuleKind,
    ModuleSpecifierResolutionHost, Node, NodeCheckFlags, NodeId, NodeLinks, ObjectFlags,
    ParsedCommandLine, Path, RawSourceMap, RelationComparisonResult, ScriptTarget, Signature,
    SignatureFlags, SymbolTable, SymbolTracker, TransformationContext, TransformerFactory, Type,
    TypeFlags, TypeMapper, __String,
};
use crate::{
    AllArenas, CheckBinaryExpression, Diagnostic, DuplicateInfoForFiles, FlowNode, FlowType,
    IndexInfo, IterationTypes, IterationTypesResolver, MappedSymbol, MultiMap, NodeBuilder, Number,
    PatternAmbientModule, ResolvedTypeReferenceDirective, ReverseMappedSymbol, StringOrNumber,
    TypeId, TypeSystemEntity, TypeSystemPropertyName, VarianceFlags, _d,
    TransformNodesTransformationResult,
    Program,
};

pub type RedirectTargetsMap = MultiMap<Path, String>;

#[derive(Builder, Trace, Finalize)]
#[builder(setter(into, strip_option))]
pub struct ResolvedProjectReference {
    pub command_line: Gc<ParsedCommandLine>,
    pub source_file: Id<Node /*SourceFile*/>,
    #[builder(setter(skip))]
    references: GcCell<Option<Vec<Option<Gc<ResolvedProjectReference>>>>>,
}

impl ResolvedProjectReference {
    pub fn maybe_references(&self) -> GcCellRef<Option<Vec<Option<Gc<ResolvedProjectReference>>>>> {
        self.references.borrow()
    }

    pub fn set_references(&self, references: Option<Vec<Option<Gc<ResolvedProjectReference>>>>) {
        *self.references.borrow_mut() = references;
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum StructureIsReused {
    Not,
    SafeModules,
    Completely,
}

pub type CustomTransformerFactory = Gc<Box<dyn CustomTransformerFactoryInterface>>;

pub trait CustomTransformerFactoryInterface: Trace + Finalize {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> CustomTransformer;
}

pub type CustomTransformer = Gc<Box<dyn CustomTransformerInterface>>;

pub trait CustomTransformerInterface: Trace + Finalize {
    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node /*SourceFile*/>;
    fn transform_bundle(&self, node: Id<Node> /*Bundle*/) -> Id<Node /*Bundle*/>;
}

#[derive(Trace, Finalize)]
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
    pub before: Option<Vec<Gc<TransformerFactoryOrCustomTransformerFactory /*<SourceFile>*/>>>,
    pub after: Option<Vec<Gc<TransformerFactoryOrCustomTransformerFactory /*<SourceFile>*/>>>,
    pub after_declarations:
        Option<Vec<Gc<TransformerFactoryOrCustomTransformerFactory /*<Bundle | SourceFile>*/>>>,
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

#[allow(dead_code)]
pub struct SourceMapEmitResult {
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

#[allow(dead_code)]
pub struct EmitResult {
    pub emit_skipped: bool,
    pub diagnostics: Vec<Id<Diagnostic>>,
    pub emitted_files: Option<Vec<String>>,
    pub source_maps: Option<Vec<SourceMapEmitResult>>,
    pub(crate) exported_modules_from_declaration_emit: Option<ExportedModulesFromDeclarationEmit>,
}

pub trait TypeCheckerHost: ModuleSpecifierResolutionHost {
    fn get_compiler_options(&self) -> Id<CompilerOptions>;

    fn get_source_files(&self) -> GcCellRef<Vec<Id<Node /*SourceFile*/>>>;
    fn get_source_file(&self, file_name: &str) -> Option<Id<Node /*SourceFile*/>>;
    fn get_resolved_type_reference_directives(
        &self,
    ) -> Gc<GcCell<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>>>;
    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String>;
    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool;

    // this is to support createNodeBuilder() withContext() casting host as Program
    fn get_common_source_directory(&self) -> Option<String> {
        None
    }
}

pub trait TypeCheckerHostDebuggable: TypeCheckerHost + fmt::Debug + Trace + Finalize {}

#[allow(non_snake_case)]
#[derive(Trace, Finalize)]
pub struct TypeChecker {
    #[unsafe_ignore_trace]
    pub(crate) arena: *const AllArenas,
    pub(crate) host: Id<Program /*TypeCheckerHostDebuggable*/>,
    pub(crate) produce_diagnostics: bool,
    pub(crate) _rc_wrapper: GcCell<Option<Gc<TypeChecker>>>,
    #[unsafe_ignore_trace]
    pub(crate) _packages_map: RefCell<Option<HashMap<String, bool>>>,
    pub(crate) cancellation_token: GcCell<Option<Gc<Box<dyn CancellationTokenDebuggable>>>>,
    #[unsafe_ignore_trace]
    pub(crate) requested_external_emit_helpers: Cell<ExternalEmitHelpers>,
    pub(crate) external_helpers_module: GcCell<Option<Id<Symbol>>>,
    pub(crate) Symbol: fn(SymbolFlags, __String) -> BaseSymbol,
    pub(crate) Type: fn(TypeFlags) -> BaseType,
    pub(crate) Signature: fn(SignatureFlags) -> Signature,
    #[unsafe_ignore_trace]
    pub(crate) type_count: Cell<u32>,
    #[unsafe_ignore_trace]
    pub(crate) symbol_count: Cell<usize>,
    #[unsafe_ignore_trace]
    pub(crate) enum_count: Cell<usize>,
    #[unsafe_ignore_trace]
    pub(crate) total_instantiation_count: Cell<usize>,
    #[unsafe_ignore_trace]
    pub(crate) instantiation_count: Cell<usize>,
    #[unsafe_ignore_trace]
    pub(crate) instantiation_depth: Cell<usize>,
    #[unsafe_ignore_trace]
    pub(crate) inline_level: Cell<usize>,
    pub(crate) current_node: GcCell<Option<Id<Node>>>,
    pub(crate) empty_symbols: Gc<GcCell<SymbolTable>>,
    pub(crate) compiler_options: Id<CompilerOptions>,
    #[unsafe_ignore_trace]
    pub(crate) language_version: ScriptTarget,
    #[unsafe_ignore_trace]
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
    #[unsafe_ignore_trace]
    pub(crate) fresh_object_literal_flag: ObjectFlags,
    pub(crate) exact_optional_property_types: Option<bool>,
    pub(crate) check_binary_expression: GcCell<Option<Gc<CheckBinaryExpression>>>,
    pub(crate) emit_resolver: GcCell<Option<Id<Box<dyn EmitResolver>>>>,
    pub(crate) node_builder: GcCell<Option<Gc<NodeBuilder>>>,
    pub(crate) globals: Gc<GcCell<SymbolTable>>,
    pub(crate) undefined_symbol: Option<Id<Symbol>>,
    pub(crate) global_this_symbol: Option<Id<Symbol>>,
    pub(crate) arguments_symbol: Option<Id<Symbol>>,
    pub(crate) require_symbol: Option<Id<Symbol>>,
    #[unsafe_ignore_trace]
    pub(crate) apparent_argument_count: Cell<Option<usize>>,

    pub(crate) tuple_types: GcCell<HashMap<String, Id</*GenericType*/ Type>>>,
    pub(crate) union_types: GcCell<HashMap<String, Id</*UnionType*/ Type>>>,
    pub(crate) intersection_types: GcCell<HashMap<String, Id<Type>>>,
    pub(crate) string_literal_types: GcCell<HashMap<String, Id</*StringLiteralType*/ Type>>>,
    pub(crate) number_literal_types: GcCell<HashMap<Number, Id</*NumberLiteralType*/ Type>>>,
    pub(crate) big_int_literal_types: GcCell<HashMap<String, Id</*BigIntLiteralType*/ Type>>>,
    pub(crate) enum_literal_types: GcCell<HashMap<String, Id</*LiteralType*/ Type>>>,
    pub(crate) indexed_access_types: GcCell<HashMap<String, Id</*IndexedAccessType*/ Type>>>,
    pub(crate) template_literal_types: GcCell<HashMap<String, Id</*TemplateLiteralType*/ Type>>>,
    pub(crate) string_mapping_types: GcCell<HashMap<String, Id</*StringMappingType*/ Type>>>,
    pub(crate) substitution_types: GcCell<HashMap<String, Id</*SubstitutionType*/ Type>>>,
    pub(crate) subtype_reduction_cache: GcCell<HashMap<String, Vec<Id<Type>>>>,
    pub(crate) evolving_array_types: GcCell<HashMap<TypeId, Id<Type /*EvolvingArrayType*/>>>,
    pub(crate) undefined_properties: GcCell<SymbolTable>,

    pub(crate) unknown_symbol: Option<Id<Symbol>>,
    pub(crate) resolving_symbol: Option<Id<Symbol>>,
    pub(crate) unresolved_symbols: GcCell<HashMap<String, Id<Symbol /*TransientSymbol*/>>>,
    pub(crate) error_types: GcCell<HashMap<String, Id<Type>>>,

    pub(crate) any_type: Option<Id<Type>>,
    pub(crate) auto_type: Option<Id<Type>>,
    pub(crate) wildcard_type: Option<Id<Type>>,
    pub(crate) error_type: Option<Id<Type>>,
    pub(crate) unresolved_type: Option<Id<Type>>,
    pub(crate) non_inferrable_any_type: Option<Id<Type>>,
    pub(crate) intrinsic_marker_type: Option<Id<Type>>,
    pub(crate) unknown_type: Option<Id<Type>>,
    pub(crate) non_null_unknown_type: Option<Id<Type>>,
    pub(crate) undefined_type: Option<Id<Type>>,
    pub(crate) undefined_widening_type: Option<Id<Type>>,
    pub(crate) optional_type: Option<Id<Type>>,
    pub(crate) missing_type: Option<Id<Type>>,
    pub(crate) null_type: Option<Id<Type>>,
    pub(crate) null_widening_type: Option<Id<Type>>,
    pub(crate) string_type: Option<Id<Type>>,
    pub(crate) number_type: Option<Id<Type>>,
    pub(crate) bigint_type: Option<Id<Type>>,
    pub(crate) false_type: Option<Id<Type>>,
    pub(crate) regular_false_type: Option<Id<Type>>,
    pub(crate) true_type: Option<Id<Type>>,
    pub(crate) regular_true_type: Option<Id<Type>>,
    pub(crate) boolean_type: Option<Id<Type>>,
    pub(crate) es_symbol_type: Option<Id<Type>>,
    pub(crate) void_type: Option<Id<Type>>,
    pub(crate) never_type: Option<Id<Type>>,
    pub(crate) silent_never_type: Option<Id<Type>>,
    pub(crate) non_inferrable_type: Option<Id<Type>>,
    pub(crate) implicit_never_type: Option<Id<Type>>,
    pub(crate) unreachable_never_type: Option<Id<Type>>,
    pub(crate) non_primitive_type: Option<Id<Type>>,
    pub(crate) string_or_number_type: Option<Id<Type>>,
    pub(crate) string_number_symbol_type: Option<Id<Type>>,
    pub(crate) keyof_constraint_type: Option<Id<Type>>,
    pub(crate) number_or_big_int_type: Option<Id<Type>>,
    pub(crate) template_constraint_type: Option<Id<Type>>,

    pub(crate) restrictive_mapper: Option<Id<TypeMapper>>,
    pub(crate) permissive_mapper: Option<Id<TypeMapper>>,

    pub(crate) empty_object_type: Option<Id<Type>>,
    pub(crate) empty_jsx_object_type: Option<Id<Type>>,
    pub(crate) empty_type_literal_symbol: Option<Id<Symbol>>,
    pub(crate) empty_type_literal_type: Option<Id<Type>>,

    pub(crate) empty_generic_type: Option<Id<Type /*GenericType*/>>,

    pub(crate) any_function_type: Option<Id<Type>>,

    pub(crate) no_constraint_type: Option<Id<Type /*ResolvedType*/>>,
    pub(crate) circular_constraint_type: Option<Id<Type /*ResolvedType*/>>,
    pub(crate) resolving_default_type: Option<Id<Type>>,

    pub(crate) marker_super_type: Option<Id<Type>>,
    pub(crate) marker_sub_type: Option<Id<Type>>,
    pub(crate) marker_other_type: Option<Id<Type>>,

    pub(crate) no_type_predicate: Option<Gc<TypePredicate>>,

    pub(crate) any_signature: Option<Id<Signature>>,
    pub(crate) unknown_signature: Option<Id<Signature>>,
    pub(crate) resolving_signature: Option<Id<Signature>>,
    pub(crate) silent_never_signature: Option<Id<Signature>>,

    pub(crate) enum_number_index_info: Option<Gc<IndexInfo>>,

    pub(crate) iteration_types_cache: GcCell<HashMap<String, Gc<IterationTypes>>>,
    pub(crate) no_iteration_types: Gc<IterationTypes>,

    pub(crate) any_iteration_types: Option<Gc<IterationTypes>>,
    pub(crate) any_iteration_types_except_next: Option<Gc<IterationTypes>>,
    pub(crate) default_iteration_types: Option<Gc<IterationTypes>>,

    #[unsafe_ignore_trace]
    pub(crate) async_iteration_types_resolver: IterationTypesResolver,
    #[unsafe_ignore_trace]
    pub(crate) sync_iteration_types_resolver: IterationTypesResolver,

    pub(crate) amalgamated_duplicates: GcCell<Option<HashMap<String, DuplicateInfoForFiles>>>,

    pub(crate) reverse_mapped_cache: GcCell<HashMap<String, Option<Id<Type>>>>,
    #[unsafe_ignore_trace]
    pub(crate) in_infer_type_for_homomorphic_mapped_type: Cell<bool>,
    pub(crate) ambient_modules_cache: GcCell<Option<Vec<Id<Symbol>>>>,

    pub(crate) pattern_ambient_modules: GcCell<Option<Vec<Gc<PatternAmbientModule>>>>,
    pub(crate) pattern_ambient_module_augmentations: GcCell<Option<HashMap<String, Id<Symbol>>>>,

    pub(crate) global_object_type: GcCell<Option<Id<Type /*ObjectType*/>>>,
    pub(crate) global_function_type: GcCell<Option<Id<Type /*ObjectType*/>>>,
    pub(crate) global_callable_function_type: GcCell<Option<Id<Type /*ObjectType*/>>>,
    pub(crate) global_newable_function_type: GcCell<Option<Id<Type /*ObjectType*/>>>,
    pub(crate) global_array_type: GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) global_readonly_array_type: GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) global_string_type: GcCell<Option<Id<Type /*ObjectType*/>>>,
    pub(crate) global_number_type: GcCell<Option<Id<Type /*ObjectType*/>>>,
    pub(crate) global_boolean_type: GcCell<Option<Id<Type /*ObjectType*/>>>,
    pub(crate) global_reg_exp_type: GcCell<Option<Id<Type /*ObjectType*/>>>,
    pub(crate) global_this_type: GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) any_array_type: GcCell<Option<Id<Type>>>,
    pub(crate) auto_array_type: GcCell<Option<Id<Type>>>,
    pub(crate) any_readonly_array_type: GcCell<Option<Id<Type>>>,
    pub(crate) deferred_global_non_nullable_type_alias: GcCell<Option<Id<Symbol>>>,

    pub(crate) deferred_global_es_symbol_constructor_symbol: GcCell<Option<Id<Symbol>>>,
    pub(crate) deferred_global_es_symbol_constructor_type_symbol: GcCell<Option<Id<Symbol>>>,
    pub(crate) deferred_global_es_symbol_type: GcCell<Option<Id<Type /*ObjectType*/>>>,
    pub(crate) deferred_global_typed_property_descriptor_type:
        GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) deferred_global_promise_type: GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) deferred_global_promise_like_type: GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) deferred_global_promise_constructor_symbol: GcCell<Option<Id<Symbol>>>,
    pub(crate) deferred_global_promise_constructor_like_type:
        GcCell<Option<Id<Type /*ObjectType*/>>>,
    pub(crate) deferred_global_iterable_type: GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) deferred_global_iterator_type: GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) deferred_global_iterable_iterator_type: GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) deferred_global_generator_type: GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) deferred_global_iterator_yield_result_type: GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) deferred_global_iterator_return_result_type:
        GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) deferred_global_async_iterable_type: GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) deferred_global_async_iterator_type: GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) deferred_global_async_iterable_iterator_type:
        GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) deferred_global_async_generator_type: GcCell<Option<Id<Type /*GenericType*/>>>,
    pub(crate) deferred_global_template_strings_array_type: GcCell<Option<Id<Type /*ObjectType*/>>>,
    pub(crate) deferred_global_import_meta_type: GcCell<Option<Id<Type /*ObjectType*/>>>,
    pub(crate) deferred_global_import_meta_expression_type: GcCell<Option<Id<Type /*ObjectType*/>>>,
    pub(crate) deferred_global_import_call_options_type: GcCell<Option<Id<Type /*ObjectType*/>>>,
    pub(crate) deferred_global_extract_symbol: GcCell<Option<Id<Symbol>>>,
    pub(crate) deferred_global_omit_symbol: GcCell<Option<Id<Symbol>>>,
    pub(crate) deferred_global_awaited_symbol: GcCell<Option<Id<Symbol>>>,
    pub(crate) deferred_global_big_int_type: GcCell<Option<Id<Type /*ObjectType*/>>>,

    pub(crate) all_potentially_unused_identifiers:
        GcCell<HashMap<Path, Vec<Id<Node /*PotentiallyUnusedIdentifier*/>>>>,

    #[unsafe_ignore_trace]
    pub(crate) flow_loop_start: Cell<usize>,
    #[unsafe_ignore_trace]
    pub(crate) flow_loop_count: Cell<usize>,
    #[unsafe_ignore_trace]
    pub(crate) shared_flow_count: Cell<usize>,
    #[unsafe_ignore_trace]
    pub(crate) flow_analysis_disabled: Cell<bool>,
    #[unsafe_ignore_trace]
    pub(crate) flow_invocation_count: Cell<usize>,
    pub(crate) last_flow_node: GcCell<Option<Id<FlowNode>>>,
    #[unsafe_ignore_trace]
    pub(crate) last_flow_node_reachable: Cell<bool>,
    pub(crate) flow_type_cache: GcCell<Option<HashMap<NodeId, Id<Type>>>>,

    pub(crate) empty_string_type: Option<Id<Type>>,
    pub(crate) zero_type: Option<Id<Type>>,
    pub(crate) zero_big_int_type: Option<Id<Type>>,

    pub(crate) resolution_targets: GcCell<Vec<TypeSystemEntity>>,
    #[unsafe_ignore_trace]
    pub(crate) resolution_results: RefCell<Vec<bool>>,
    #[unsafe_ignore_trace]
    pub(crate) resolution_property_names: RefCell<Vec<TypeSystemPropertyName>>,

    #[unsafe_ignore_trace]
    pub(crate) suggestion_count: Cell<usize>,
    pub(crate) maximum_suggestion_count: usize,
    pub(crate) merged_symbols: GcCell<HashMap<u32, Id<Symbol>>>,
    pub(crate) symbol_links: GcCell<HashMap<SymbolId, Id<GcCell<SymbolLinks>>>>,
    pub(crate) node_links: GcCell<HashMap<NodeId, Gc<GcCell<NodeLinks>>>>,
    pub(crate) flow_loop_caches: GcCell<HashMap<usize, Gc<GcCell<HashMap<String, Id<Type>>>>>>,
    pub(crate) flow_loop_nodes: GcCell<HashMap<usize, Id<FlowNode>>>,
    #[unsafe_ignore_trace]
    pub(crate) flow_loop_keys: RefCell<HashMap<usize, String>>,
    pub(crate) flow_loop_types: GcCell<HashMap<usize, Vec<Id<Type>>>>,
    pub(crate) shared_flow_nodes: GcCell<HashMap<usize, Id<FlowNode>>>,
    pub(crate) shared_flow_types: GcCell<HashMap<usize, FlowType>>,
    #[unsafe_ignore_trace]
    pub(crate) flow_node_reachable: RefCell<HashMap<usize, bool>>,
    #[unsafe_ignore_trace]
    pub(crate) flow_node_post_super: RefCell<HashMap<usize, bool>>,
    pub(crate) potential_this_collisions: GcCell<Vec<Id<Node>>>,
    pub(crate) potential_new_target_collisions: GcCell<Vec<Id<Node>>>,
    pub(crate) potential_weak_map_set_collisions: GcCell<Vec<Id<Node>>>,
    pub(crate) potential_reflect_collisions: GcCell<Vec<Id<Node>>>,
    #[unsafe_ignore_trace]
    pub(crate) awaited_type_stack: RefCell<Vec<TypeId>>,

    pub(crate) diagnostics: GcCell<DiagnosticCollection>,
    pub(crate) suggestion_diagnostics: GcCell<DiagnosticCollection>,

    pub(crate) typeof_types_by_name: Option<HashMap<&'static str, Id<Type>>>,
    pub(crate) typeof_type: Option<Id<Type>>,

    pub(crate) _jsx_namespace: GcCell<Option<__String>>,
    pub(crate) _jsx_factory_entity: GcCell<Option<Id<Node /*EntityName*/>>>,
    pub(crate) outofband_variance_marker_handler:
        GcCell<Option<Gc<Box<dyn OutofbandVarianceMarkerHandler>>>>,

    #[unsafe_ignore_trace]
    pub(crate) subtype_relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
    #[unsafe_ignore_trace]
    pub(crate) strict_subtype_relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
    #[unsafe_ignore_trace]
    pub(crate) assignable_relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
    #[unsafe_ignore_trace]
    pub(crate) comparable_relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
    #[unsafe_ignore_trace]
    pub(crate) identity_relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
    #[unsafe_ignore_trace]
    pub(crate) enum_relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,

    pub(crate) builtin_globals: GcCell<Option<SymbolTable>>,

    #[unsafe_ignore_trace]
    pub(crate) suggested_extensions: Vec<(&'static str, &'static str)>,
}

impl fmt::Debug for TypeChecker {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TypeChecker").finish()
    }
}

pub(crate) trait OutofbandVarianceMarkerHandler: Trace + Finalize {
    fn call(&self, value: bool);
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum MemberOverrideStatus {
    Ok,
    NeedsOverride,
    HasInvalidOverride,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
        const SkipBindingPatterns = 1 << 3;
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
    fn write_keyword(&self, text: &str);
    fn write_operator(&self, text: &str);
    fn write_punctuation(&self, text: &str);
    fn write_space(&self, text: &str);
    fn write_string_literal(&self, text: &str);
    fn write_parameter(&self, text: &str);
    fn write_property(&self, text: &str);
    fn write_symbol(&self, text: &str, symbol: Id<Symbol>);
    fn write_line(&self, force: Option<bool>);
    fn increase_indent(&self);
    fn decrease_indent(&self);
    fn clear(&self);
    // fn as_symbol_tracker(self: Gc<Box<Self>>) -> Gc<Box<dyn SymbolTracker>>;
    fn as_symbol_tracker(&self) -> Id<Box<dyn SymbolTracker>>;
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

#[derive(Debug, Trace, Finalize)]
pub struct TypePredicate {
    #[unsafe_ignore_trace]
    pub kind: TypePredicateKind,
    pub parameter_name: Option<String>,
    pub parameter_index: Option<usize>,
    pub type_: Option<Id<Type>>,
}

pub struct SymbolVisibilityResult {
    pub accessibility: SymbolAccessibility,
    pub aliases_to_make_visible: Option<Vec<Id<Node /*LateVisibilityPaintedStatement*/>>>,
    pub error_symbol_name: Option<String>,
    pub error_node: Option<Id<Node>>,
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
    pub aliases_to_make_visible: Option<Vec<Id<Node /*LateVisibilityPaintedStatement*/>>>,
    pub error_symbol_name: Option<String>,
    pub error_node: Option<Id<Node>>,
    pub error_module_name: Option<String>,
}

pub struct AllAccessorDeclarations {
    pub first_accessor: Id<Node /*AccessorDeclaration*/>,
    pub second_accessor: Option<Id<Node /*AccessorDeclaration*/>>,
    pub get_accessor: Option<Id<Node /*GetAccessorDeclaration*/>>,
    pub set_accessor: Option<Id<Node /*SetAccessorDeclaration*/>>,
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

pub trait EmitResolver: Trace + Finalize {
    fn has_global_name(&self, name: &str) -> bool;
    fn get_referenced_export_container(
        &self,
        node: Id<Node>, /*Identifier*/
        prefix_locals: Option<bool>,
    ) -> io::Result<Option<Id<Node /*SourceFile | ModuleDeclaration | EnumDeclaration*/>>>;
    fn get_referenced_import_declaration(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<Option<Id<Node /*Declaration*/>>>;
    fn get_referenced_declaration_with_colliding_name(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<Option<Id<Node /*Declaration*/>>>;
    fn is_declaration_with_colliding_name(
        &self,
        node: Id<Node>, /*Declaration*/
    ) -> io::Result<bool>;
    fn is_value_alias_declaration(&self, node: Id<Node>) -> io::Result<bool>;
    fn is_referenced_alias_declaration(
        &self,
        node: Id<Node>,
        check_children: Option<bool>,
    ) -> io::Result<bool>;
    fn is_top_level_value_import_equals_with_entity_name(
        &self,
        node: Id<Node>, /*ImportEqualsDeclaration*/
    ) -> io::Result<bool>;
    fn get_node_check_flags(&self, node: Id<Node>) -> NodeCheckFlags;
    fn is_declaration_visible(&self, node: Id<Node> /*Declaration | AnyImportSyntax*/) -> bool;
    fn is_late_bound(&self, node: Id<Node> /*Declaration*/) -> io::Result<bool>;
    fn collect_linked_aliases(
        &self,
        node: Id<Node>, /*Identifier*/
        set_visibility: Option<bool>,
    ) -> io::Result<Option<Vec<Id<Node>>>>;
    fn is_implementation_of_overload(
        &self,
        node: Id<Node>, /*SignatureDeclaration*/
    ) -> io::Result<Option<bool>>;
    fn is_required_initialized_parameter(
        &self,
        node: Id<Node>, /*ParameterDeclaration*/
    ) -> io::Result<bool>;
    fn is_optional_uninitialized_parameter_property(
        &self,
        node: Id<Node>, /*ParameterDeclaration*/
    ) -> io::Result<bool>;
    fn is_expando_function_declaration(
        &self,
        node: Id<Node>, /*FunctionDeclaration*/
    ) -> io::Result<bool>;
    fn get_properties_of_container_function(
        &self,
        node: Id<Node>, /*Declaration*/
    ) -> io::Result<Vec<Id<Symbol>>>;
    fn create_type_of_declaration(
        &self,
        declaration: Id<Node>, /*AccessorDeclaration | VariableLikeDeclaration | PropertyAccessExpression*/
        enclosing_declaration: Id<Node>,
        flags: NodeBuilderFlags,
        tracker: Id<Box<dyn SymbolTracker>>,
        add_undefined: Option<bool>,
    ) -> io::Result<Option<Id<Node /*TypeNode*/>>>;
    fn create_return_type_of_signature_declaration(
        &self,
        signature_declaration: Id<Node>, /*SignatureDeclaration*/
        enclosing_declaration: Id<Node>,
        flags: NodeBuilderFlags,
        tracker: Id<Box<dyn SymbolTracker>>,
    ) -> io::Result<Option<Id<Node /*TypeNode*/>>>;
    fn create_type_of_expression(
        &self,
        expr: Id<Node>, /*Expression*/
        enclosing_declaration: Id<Node>,
        flags: NodeBuilderFlags,
        tracker: Id<Box<dyn SymbolTracker>>,
    ) -> io::Result<Option<Id<Node /*TypeNode*/>>>;
    fn create_literal_const_value(
        &self,
        node: Id<Node>, /*VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration*/
        tracker: Id<Box<dyn SymbolTracker>>,
    ) -> io::Result<Id<Node /*Expression*/>>;
    fn is_symbol_accessible(
        &self,
        symbol: Id<Symbol>,
        enclosing_declaration: Option<Id<Node>>,
        meaning: Option<SymbolFlags>,
        should_compute_alias_to_mark_visible: bool,
    ) -> io::Result<SymbolAccessibilityResult>;
    fn is_entity_name_visible(
        &self,
        entity_name: Id<Node>, /*EntityNameOrEntityNameExpression*/
        enclosing_declaration: Id<Node>,
    ) -> io::Result<SymbolVisibilityResult>;
    fn get_constant_value(
        &self,
        node: Id<Node>, /*EnumMember | PropertyAccessExpression | ElementAccessExpression*/
    ) -> io::Result<Option<StringOrNumber>>;
    fn get_referenced_value_declaration(
        &self,
        reference: Id<Node>, /*Identifier*/
    ) -> io::Result<Option<Id<Node /*Declaration*/>>>;
    fn get_type_reference_serialization_kind(
        &self,
        type_name: Id<Node>, /*EntityName*/
        location: Option<Id<Node>>,
    ) -> io::Result<TypeReferenceSerializationKind>;
    fn is_optional_parameter(
        &self,
        node: Id<Node>, /*ParameterDeclaration*/
    ) -> io::Result<bool>;
    fn module_exports_some_value(
        &self,
        module_reference_expression: Id<Node>, /*Expression*/
    ) -> io::Result<bool>;
    fn is_arguments_local_binding(&self, node: Id<Node> /*Identifier*/) -> io::Result<bool>;
    fn get_external_module_file_from_declaration(
        &self,
        declaration: Id<Node>, /*ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration | ModuleDeclaration | ImportTypeNode | ImportCall*/
    ) -> io::Result<Option<Id<Node /*SourceFile*/>>>;
    fn get_type_reference_directives_for_entity_name(
        &self,
        name: Id<Node>, /*EntityNameOrEntityNameExpression*/
    ) -> io::Result<Option<Vec<String>>>;
    fn get_type_reference_directives_for_symbol(
        &self,
        symbol: Id<Symbol>,
        meaning: Option<SymbolFlags>,
    ) -> io::Result<Option<Vec<String>>>;
    fn is_literal_const_declaration(
        &self,
        node: Id<Node>, /*VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration*/
    ) -> io::Result<bool>;
    fn get_jsx_factory_entity(&self, location: Option<Id<Node>>)
        -> Option<Id<Node /*EntityName*/>>;
    fn get_jsx_fragment_factory_entity(
        &self,
        location: Option<Id<Node>>,
    ) -> Option<Id<Node /*EntityName*/>>;
    fn get_all_accessor_declarations(
        &self,
        declaration: Id<Node>, /*AccessorDeclaration*/
    ) -> io::Result<AllAccessorDeclarations>;
    fn get_symbol_of_external_module_specifier(
        &self,
        node: Id<Node>, /*StringLiteralLike*/
    ) -> io::Result<Option<Id<Symbol>>>;
    fn is_binding_captured_by_node(
        &self,
        node: Id<Node>,
        decl: Id<Node>, /*VariableDeclaration | BindingElement*/
    ) -> io::Result<bool>;
    fn get_declaration_statements_for_source_file(
        &self,
        node: Id<Node>, /*SourceFile*/
        flags: NodeBuilderFlags,
        tracker: Id<Box<dyn SymbolTracker>>,
        bundled: Option<bool>,
    ) -> io::Result<Option<Vec<Id<Node /*Statement*/>>>>;
    fn is_import_required_by_augmentation(
        &self,
        decl: Id<Node>, /*ImportDeclaration*/
    ) -> io::Result<bool>;
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
    fn flags(&self) -> SymbolFlags;
    fn set_flags(&self, flags: SymbolFlags);
    fn escaped_name(&self) -> &str /*__String*/;
    fn maybe_declarations(&self) -> GcCellRef<Option<Vec<Id<Node>>>>;
    fn maybe_declarations_mut(&self) -> GcCellRefMut<Option<Vec<Id<Node>>>>;
    fn set_declarations(&self, declarations: Vec<Id<Node>>);
    fn maybe_value_declaration(&self) -> Option<Id<Node>>;
    fn set_value_declaration(&self, node: Id<Node>);
    fn maybe_members(&self) -> GcCellRef<Option<Gc<GcCell<SymbolTable>>>>;
    fn maybe_members_mut(&self) -> GcCellRefMut<Option<Gc<GcCell<SymbolTable>>>>;
    fn members(&self) -> Gc<GcCell<SymbolTable>>;
    fn maybe_exports(&self) -> GcCellRef<Option<Gc<GcCell<SymbolTable>>>>;
    fn maybe_exports_mut(&self) -> GcCellRefMut<Option<Gc<GcCell<SymbolTable>>>>;
    fn exports(&self) -> Gc<GcCell<SymbolTable>>;
    fn maybe_global_exports(&self) -> GcCellRefMut<Option<Gc<GcCell<SymbolTable>>>>;
    fn maybe_id(&self) -> Option<SymbolId>;
    fn id(&self) -> SymbolId;
    fn set_id(&self, id: SymbolId);
    fn maybe_merge_id(&self) -> Option<u32>;
    fn set_merge_id(&self, merge_id: u32);
    fn maybe_parent(&self) -> Option<Id<Symbol>>;
    fn set_parent(&self, parent: Option<Id<Symbol>>);
    fn maybe_export_symbol(&self) -> Option<Id<Symbol>>;
    fn set_export_symbol(&self, export_symbol: Option<Id<Symbol>>);
    fn maybe_const_enum_only_module(&self) -> Option<bool>;
    fn set_const_enum_only_module(&self, const_enum_only_module: Option<bool>);
    fn maybe_is_referenced(&self) -> Option<SymbolFlags>;
    fn set_is_referenced(&self, is_referenced: Option<SymbolFlags>);
    fn maybe_is_replaceable_by_method(&self) -> Option<bool>;
    fn set_is_replaceable_by_method(&self, is_replaceable_by_method: Option<bool>);
    fn maybe_is_assigned(&self) -> Option<bool>;
    fn set_is_assigned(&self, is_assigned: Option<bool>);
    fn maybe_assignment_declaration_members(
        &self,
    ) -> GcCellRefMut<Option<HashMap<NodeId, Id<Node /*Declaration*/>>>>;
}

#[derive(Debug, Finalize, Trace)]
#[symbol_type(impl_from = false)]
pub enum Symbol {
    BaseSymbol(BaseSymbol),
    TransientSymbol(TransientSymbol),
}

impl Symbol {
    pub fn maybe_as_transient_symbol(&self) -> Option<&TransientSymbol> {
        match self {
            Self::TransientSymbol(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_transient_symbol(&self) -> &TransientSymbol {
        enum_unwrapped!(self, [Symbol, TransientSymbol])
    }

    pub fn as_reverse_mapped_symbol(&self) -> &ReverseMappedSymbol {
        enum_unwrapped!(self, [Symbol, TransientSymbol, ReverseMappedSymbol])
    }

    pub fn as_mapped_symbol(&self) -> &MappedSymbol {
        enum_unwrapped!(self, [Symbol, TransientSymbol, MappedSymbol])
    }
}

#[derive(Debug, Finalize, Trace)]
pub struct BaseSymbol {
    #[unsafe_ignore_trace]
    flags: Cell<SymbolFlags>,
    #[unsafe_ignore_trace]
    escaped_name: __String,
    declarations: GcCell<Option<Vec<Id<Node /*Declaration*/>>>>,
    value_declaration: GcCell<Option<Id<Node>>>,
    members: GcCell<Option<Gc<GcCell<SymbolTable>>>>,
    exports: GcCell<Option<Gc<GcCell<SymbolTable>>>>,
    global_exports: GcCell<Option<Gc<GcCell<SymbolTable>>>>,
    #[unsafe_ignore_trace]
    id: Cell<Option<SymbolId>>,
    #[unsafe_ignore_trace]
    merge_id: Cell<Option<u32>>,
    parent: GcCell<Option<Id<Symbol>>>,
    export_symbol: GcCell<Option<Id<Symbol>>>,
    #[unsafe_ignore_trace]
    const_enum_only_module: Cell<Option<bool>>,
    #[unsafe_ignore_trace]
    is_referenced: Cell<Option<SymbolFlags>>,
    #[unsafe_ignore_trace]
    is_replaceable_by_method: Cell<Option<bool>>,
    #[unsafe_ignore_trace]
    is_assigned: Cell<Option<bool>>,
    assignment_declaration_members: GcCell<Option<HashMap<NodeId, Id<Node /*Declaration*/>>>>,
}

impl BaseSymbol {
    pub fn new(flags: SymbolFlags, name: __String) -> Self {
        Self {
            flags: Cell::new(flags),
            escaped_name: name,
            declarations: GcCell::new(None),
            value_declaration: GcCell::new(None),
            members: GcCell::new(None),
            exports: GcCell::new(None),
            global_exports: GcCell::new(None),
            id: Cell::new(None),
            merge_id: Cell::new(None),
            parent: GcCell::new(None),
            export_symbol: GcCell::new(None),
            const_enum_only_module: Cell::new(None),
            is_referenced: Cell::new(None),
            is_replaceable_by_method: Cell::new(None),
            is_assigned: Cell::new(None),
            assignment_declaration_members: GcCell::new(None),
        }
    }
}

impl SymbolInterface for BaseSymbol {
    fn flags(&self) -> SymbolFlags {
        self.flags.get()
    }

    fn set_flags(&self, flags: SymbolFlags) {
        self.flags.set(flags);
    }

    fn escaped_name(&self) -> &str /*__String*/ {
        &self.escaped_name
    }

    fn maybe_declarations(&self) -> GcCellRef<Option<Vec<Id<Node>>>> {
        self.declarations.borrow()
    }

    fn maybe_declarations_mut(&self) -> GcCellRefMut<Option<Vec<Id<Node>>>> {
        self.declarations.borrow_mut()
    }

    fn set_declarations(&self, declarations: Vec<Id<Node>>) {
        *self.declarations.borrow_mut() = Some(declarations);
    }

    fn maybe_value_declaration(&self) -> Option<Id<Node>> {
        self.value_declaration.borrow().clone()
    }

    fn set_value_declaration(&self, node: Id<Node>) {
        *self.value_declaration.borrow_mut() = Some(node);
    }

    fn maybe_members(&self) -> GcCellRef<Option<Gc<GcCell<SymbolTable>>>> {
        self.members.borrow()
    }

    fn maybe_members_mut(&self) -> GcCellRefMut<Option<Gc<GcCell<SymbolTable>>>> {
        self.members.borrow_mut()
    }

    fn members(&self) -> Gc<GcCell<SymbolTable>> {
        self.members.borrow().as_ref().unwrap().clone()
    }

    fn maybe_exports(&self) -> GcCellRef<Option<Gc<GcCell<SymbolTable>>>> {
        self.exports.borrow()
    }

    fn maybe_exports_mut(&self) -> GcCellRefMut<Option<Gc<GcCell<SymbolTable>>>> {
        self.exports.borrow_mut()
    }

    fn exports(&self) -> Gc<GcCell<SymbolTable>> {
        self.exports.borrow().as_ref().unwrap().clone()
    }

    fn maybe_global_exports(&self) -> GcCellRefMut<Option<Gc<GcCell<SymbolTable>>>> {
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

    fn maybe_parent(&self) -> Option<Id<Symbol>> {
        self.parent.borrow().as_ref().map(Clone::clone)
    }

    fn set_parent(&self, parent: Option<Id<Symbol>>) {
        *self.parent.borrow_mut() = parent;
    }

    fn maybe_export_symbol(&self) -> Option<Id<Symbol>> {
        self.export_symbol.borrow().as_ref().map(Clone::clone)
    }

    fn set_export_symbol(&self, export_symbol: Option<Id<Symbol>>) {
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

    fn maybe_is_assigned(&self) -> Option<bool> {
        self.is_assigned.get()
    }

    fn set_is_assigned(&self, is_assigned: Option<bool>) {
        self.is_assigned.set(is_assigned);
    }

    fn maybe_assignment_declaration_members(
        &self,
    ) -> GcCellRefMut<Option<HashMap<NodeId, Id<Node>>>> {
        self.assignment_declaration_members.borrow_mut()
    }
}

impl From<BaseSymbol> for Symbol {
    fn from(base_symbol: BaseSymbol) -> Self {
        Symbol::BaseSymbol(base_symbol)
    }
}

#[derive(Debug, Finalize, Trace)]
pub struct SymbolLinks {
    pub immediate_target: Option<Id<Symbol>>,
    pub target: Option<Id<Symbol>>,
    pub type_: Option<Id<Type>>,
    pub write_type: Option<Id<Type>>,
    pub name_type: Option<Id<Type>>,
    pub unique_es_symbol_type: Option<Id<Type>>,
    pub declared_type: Option<Id<Type>>,
    pub type_parameters: Option<Vec<Id<Type /*TypeParameter*/>>>,
    pub instantiations: Option<HashMap<String, Id<Type>>>,
    pub inferred_class_symbol: Option<HashMap<SymbolId, Id<Symbol /*TransientSymbol*/>>>,
    pub mapper: Option<Id<TypeMapper>>,
    pub referenced: Option<bool>,
    pub const_enum_referenced: Option<bool>,
    pub containing_type: Option<Id<Type>>,
    pub left_spread: Option<Id<Symbol>>,
    pub right_spread: Option<Id<Symbol>>,
    pub synthetic_origin: Option<Id<Symbol>>,
    pub is_discriminant_property: Option<bool>,
    pub resolved_exports: Option<Gc<GcCell<SymbolTable>>>,
    pub resolved_members: Option<Gc<GcCell<SymbolTable>>>,
    pub exports_checked: Option<bool>,
    pub type_parameters_checked: Option<bool>,
    pub is_declaration_with_colliding_name: Option<bool>,
    pub binding_element: Option<Id<Node /*BindingElement*/>>,
    pub exports_some_value: Option<bool>,
    #[unsafe_ignore_trace]
    pub enum_kind: Option<EnumKind>,
    pub originating_import: Option<Id<Node /*ImportDeclaration | ImportCall*/>>,
    pub late_symbol: Option<Id<Symbol>>,
    #[unsafe_ignore_trace]
    pub specifier_cache: Option<HashMap<String, String>>,
    pub extended_containers: Option<Vec<Id<Symbol>>>,
    pub extended_containers_by_file: Option<HashMap<NodeId, Vec<Id<Symbol>>>>,
    #[unsafe_ignore_trace]
    pub variances: Rc<RefCell<Option<Vec<VarianceFlags>>>>,
    pub deferral_constituents: Option<Vec<Id<Type>>>,
    pub deferral_parent: Option<Id<Type>>,
    pub cjs_export_merged: Option<Id<Symbol>>,
    pub type_only_declaration: Option<Option<Id<Node /*TypeOnlyAliasDeclaration | false*/>>>,
    pub is_constructor_declared_property: Option<bool>,
    pub tuple_label_declaration: Option<Id<Node /*NamedTupleMember | ParameterDeclaration*/>>,
    pub accessible_chain_cache: Option<HashMap<String, Option<Vec<Id<Symbol>>>>>,
}

impl Default for SymbolLinks {
    fn default() -> Self {
        Self {
            immediate_target: None,
            target: None,
            type_: None,
            write_type: None,
            name_type: None,
            unique_es_symbol_type: None,
            declared_type: None,
            type_parameters: None,
            instantiations: None,
            inferred_class_symbol: None,
            mapper: None,
            referenced: None,
            const_enum_referenced: None,
            containing_type: None,
            left_spread: None,
            right_spread: None,
            synthetic_origin: None,
            is_discriminant_property: None,
            resolved_exports: None,
            resolved_members: None,
            exports_checked: None,
            type_parameters_checked: None,
            is_declaration_with_colliding_name: None,
            binding_element: None,
            exports_some_value: None,
            enum_kind: None,
            originating_import: None,
            late_symbol: None,
            specifier_cache: None,
            extended_containers: None,
            extended_containers_by_file: None,
            variances: Default::default(),
            deferral_constituents: None,
            deferral_parent: None,
            cjs_export_merged: None,
            type_only_declaration: None,
            is_constructor_declared_property: None,
            tuple_label_declaration: None,
            accessible_chain_cache: None,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum EnumKind {
    Numeric,
    Literal,
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
        const ContainsProtected = 1 << 9;
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
    fn symbol_links(&self) -> Id<GcCell<SymbolLinks>>;
    fn check_flags(&self) -> CheckFlags;
    fn set_check_flags(&self, check_flags: CheckFlags);
}

mod _TransientSymbolTraceDeriveScope {
    use gc::Finalize;
    use local_macros::{symbol_type, Trace};

    use super::{BaseTransientSymbol, Id, MappedSymbol, ReverseMappedSymbol, Type};

    #[derive(Debug, Finalize, Trace)]
    #[symbol_type(interfaces = "TransientSymbolInterface")]
    pub enum TransientSymbol {
        BaseTransientSymbol(BaseTransientSymbol),
        ReverseMappedSymbol(ReverseMappedSymbol),
        MappedSymbol(MappedSymbol),
    }

    impl TransientSymbol {
        pub fn into_reverse_mapped_symbol(
            self,
            property_type: Id<Type>,
            mapped_type: Id<Type>,
            constraint_type: Id<Type>,
        ) -> Self {
            match self {
                Self::BaseTransientSymbol(symbol) => Self::ReverseMappedSymbol(
                    ReverseMappedSymbol::new(symbol, property_type, mapped_type, constraint_type),
                ),
                _ => panic!("Should only call into_reverse_mapped_symbol() on BaseTransientSymbol"),
            }
        }

        pub fn into_mapped_symbol(self, mapped_type: Id<Type>, key_type: Id<Type>) -> Self {
            match self {
                Self::BaseTransientSymbol(symbol) => {
                    Self::MappedSymbol(MappedSymbol::new(symbol, mapped_type, key_type))
                }
                _ => panic!("Should only call into_mapped_symbol() on BaseTransientSymbol"),
            }
        }
    }
}
pub use _TransientSymbolTraceDeriveScope::TransientSymbol;

#[derive(Debug, Finalize, Trace)]
#[symbol_type(ancestors = "TransientSymbol")]
pub struct BaseTransientSymbol {
    _symbol: BaseSymbol,
    _symbol_links: Id<GcCell<SymbolLinks>>,
    #[unsafe_ignore_trace]
    check_flags: Cell<CheckFlags>,
}

impl BaseTransientSymbol {
    pub fn new(base_symbol: BaseSymbol, check_flags: CheckFlags) -> Self {
        Self {
            _symbol: base_symbol,
            _symbol_links: _d(),
            check_flags: Cell::new(check_flags),
        }
    }
}

impl TransientSymbolInterface for BaseTransientSymbol {
    fn symbol_links(&self) -> Id<GcCell<SymbolLinks>> {
        self._symbol_links.clone()
    }

    fn check_flags(&self) -> CheckFlags {
        self.check_flags.get()
    }

    fn set_check_flags(&self, check_flags: CheckFlags) {
        self.check_flags.set(check_flags);
    }
}
