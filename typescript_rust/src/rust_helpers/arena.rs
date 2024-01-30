use std::{any::Any, rc::Rc, collections::HashMap};

use debug_cell::{Ref, RefCell, RefMut};
use gc::GcCell;
use id_arena::{Arena, Id};
use once_cell::unsync::Lazy;

use crate::{
    Node, Symbol, Type, TypeInterface, TypeMapper, TransformNodesTransformationResult, TransformerInterface, Transformer,
    TransformerFactoryInterface, EmitTextWriter, SymbolTracker, EmitHost, ModuleSpecifierResolutionHostAndGetCommonSourceDirectory,
    FileIncludeReason, System, SourceMapRange, EmitHelper, CompilerOptions, FlowNode, Diagnostic,
    Program, Signature, DiagnosticReporter, NodeFactory, BaseNodeFactory, EmitResolver, ResolvedTypeReferenceDirective,
    CompilerHost, SymbolLinks, Printer, DiagnosticRelatedInformation, IndexInfo, CurrentParenthesizerRule,
    ParenthesizerRules, IterationTypes, TypePredicate, ActiveLabel, ToPath, ModuleResolutionHostOverrider,
    WrapCustomTransformerFactoryHandleDefault, TransformationContextOnEmitNodeOverrider, SourceMapGenerator,
    GetCanonicalFileName, EmitHelperFactory, TransformationContextOnSubstituteNodeOverrider,
    ParsedCommandLine, CancellationToken, ResolvedProjectReference, TransformerFactoryOrCustomTransformerFactory,
    SymlinkCache, WriteFileCallback, ResolvedModuleFull, NodeArray, BundleFileSection,
    BuildInfo, ProgramBuildInfo, BundleBuildInfo, BundleFileInfo, SymbolTable, InferenceInfo,
    SysFormatDiagnosticsHost, ClassLexicalEnvironment, ConvertedLoopState, EmitHelperTextCallback,
    ConditionalRoot, EmitNode, CheckBinaryExpression, SourceMapSource, OutofbandVarianceMarkerHandler,
    BindBinaryExpressionFlow, TypeChecker, ReadFileCallback, Binder, GetSourceFile, GetSymlinkCache,
    EmitBinaryExpression, RelativeToBuildInfo, PrintHandlers, GetResolvedProjectReferences,
    ForEachResolvedProjectReference, CompilerHostLike, DirectoryStructureHost,
    BuilderProgram, TypeReferenceDirectiveResolutionCache, ModuleResolutionCache,
    ParseConfigFileHost, FilePreprocessingDiagnostics, ActualResolveModuleNamesWorker,
    ActualResolveTypeReferenceDirectiveNamesWorker, GetProgramBuildInfo, LoadWithModeAwareCacheLoader,
    LoadWithLocalCacheLoader, SymbolAccessibilityDiagnostic, CodeBlock, PrivateIdentifierEnvironment,
    PrivateIdentifierInfo, ExternalModuleInfo, ResolvedModuleWithFailedLookupLocations,
    ResolvedTypeReferenceDirectiveWithFailedLookupLocations, PackageJsonInfoCache,
    ModeAwareCache, PerModuleNameCache, MultiMap, Path, GetSymbolAccessibilityDiagnosticInterface,
    PendingDeclaration, PackageJsonInfo, PatternAmbientModule, CheckTypeContainingMessageChain,
    CheckTypeErrorOutputContainer, NodeBuilder, NodeBuilderContext, TypeId, TypeComparer,
    InferenceContext, SkipTrivia, CustomTransformerFactoryInterface,
};

#[derive(Default)]
pub struct AllArenas {
    pub nodes: RefCell<Arena<Node>>,
    pub symbols: RefCell<Arena<Symbol>>,
    pub types: RefCell<Arena<Type>>,
    pub type_mappers: RefCell<Arena<TypeMapper>>,
    pub transform_nodes_transformation_results: RefCell<Arena<TransformNodesTransformationResult>>,
    pub transformers: RefCell<Arena<Box<dyn TransformerInterface>>>,
    pub transformer_factories: RefCell<Arena<Box<dyn TransformerFactoryInterface>>>,
    pub emit_text_writers: RefCell<Arena<Box<dyn EmitTextWriter>>>,
    pub symbol_trackers: RefCell<Arena<Box<dyn SymbolTracker>>>,
    pub emit_hosts: RefCell<Arena<Box<dyn EmitHost>>>,
    pub module_specifier_resolution_host_and_get_common_source_directories: RefCell<Arena<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>>>,
    pub file_include_reasons: RefCell<Arena<FileIncludeReason>>,
    pub systems: RefCell<Arena<Box<dyn System>>>,
    pub source_map_ranges: RefCell<Arena<SourceMapRange>>,
    pub emit_helpers: RefCell<Arena<EmitHelper>>,
    pub compiler_options: RefCell<Arena<CompilerOptions>>,
    pub flow_nodes: RefCell<Arena<FlowNode>>,
    pub diagnostics: RefCell<Arena<Diagnostic>>,
    pub programs: RefCell<Arena<Program>>,
    pub signatures: RefCell<Arena<Signature>>,
    pub diagnostic_reporters: RefCell<Arena<Box<dyn DiagnosticReporter>>>,
    pub node_factories: RefCell<Arena<NodeFactory>>,
    pub base_node_factories: RefCell<Arena<Box<dyn BaseNodeFactory>>>,
    pub emit_resolvers: RefCell<Arena<Box<dyn EmitResolver>>>,
    pub resolved_type_reference_directives: RefCell<Arena<ResolvedTypeReferenceDirective>>,
    pub compiler_hosts: RefCell<Arena<Box<dyn CompilerHost>>>,
    pub symbol_links: RefCell<Arena<GcCell<SymbolLinks>>>,
    pub printers: RefCell<Arena<Printer>>,
    pub diagnostic_related_informations: RefCell<Arena<DiagnosticRelatedInformation>>,
    pub index_infos: RefCell<Arena<IndexInfo>>,
    pub current_parenthesizer_rules: RefCell<Arena<Box<dyn CurrentParenthesizerRule>>>,
    pub parenthesizer_rules: RefCell<Arena<Box<dyn ParenthesizerRules>>>,
    pub iteration_types: RefCell<Arena<IterationTypes>>,
    pub type_predicates: RefCell<Arena<TypePredicate>>,
    pub active_labels: RefCell<Arena<ActiveLabel>>,
    pub to_paths: RefCell<Arena<Box<dyn ToPath>>>,
    pub module_resolution_host_overriders: RefCell<Arena<Box<dyn ModuleResolutionHostOverrider>>>,
    pub wrap_custom_transformer_factory_handle_defaults: RefCell<Arena<Box<dyn WrapCustomTransformerFactoryHandleDefault>>>,
    pub transformation_context_on_emit_node_overriders: RefCell<Arena<Box<dyn TransformationContextOnEmitNodeOverrider>>>,
    pub source_map_generators: RefCell<Arena<Box<dyn SourceMapGenerator>>>,
    pub get_canonical_file_names: RefCell<Arena<Box<dyn GetCanonicalFileName>>>,
    pub emit_helper_factories: RefCell<Arena<EmitHelperFactory>>,
    pub transformation_context_on_substitute_node_overriders: RefCell<Arena<Box<dyn TransformationContextOnSubstituteNodeOverrider>>>,
    pub parsed_command_lines: RefCell<Arena<ParsedCommandLine>>,
    pub cancellation_tokens: RefCell<Arena<Box<dyn CancellationToken>>>,
    pub resolved_project_references: RefCell<Arena<ResolvedProjectReference>>,
    pub transformer_factory_or_custom_transformer_factories: RefCell<Arena<TransformerFactoryOrCustomTransformerFactory>>,
    pub symlink_caches: RefCell<Arena<SymlinkCache>>,
    pub write_file_callbacks: RefCell<Arena<Box<dyn WriteFileCallback>>>,
    pub resolved_module_fulls: RefCell<Arena<ResolvedModuleFull>>,
    pub node_arrays: RefCell<Arena<NodeArray>>,
    pub bundle_file_sections: RefCell<Arena<BundleFileSection>>,
    pub build_infos: RefCell<Arena<BuildInfo>>,
    pub program_build_infos: RefCell<Arena<ProgramBuildInfo>>,
    pub bundle_build_infos: RefCell<Arena<BundleBuildInfo>>,
    pub bundle_file_infos: RefCell<Arena<BundleFileInfo>>,
    pub symbol_tables: RefCell<Arena<SymbolTable>>,
    pub inference_infos: RefCell<Arena<InferenceInfo>>,
    pub sys_format_diagnostics_hosts: RefCell<Arena<SysFormatDiagnosticsHost>>,
    pub class_lexical_environments: RefCell<Arena<ClassLexicalEnvironment>>,
    pub converted_loop_states: RefCell<Arena<ConvertedLoopState>>,
    pub emit_helper_text_callbacks: RefCell<Arena<Box<dyn EmitHelperTextCallback>>>,
    pub conditional_roots: RefCell<Arena<ConditionalRoot>>,
    pub emit_nodes: RefCell<Arena<EmitNode>>,
    pub check_binary_expressions: RefCell<Arena<CheckBinaryExpression>>,
    pub source_map_sources: RefCell<Arena<SourceMapSource>>,
    pub outofband_variance_marker_handlers: RefCell<Arena<Box<dyn OutofbandVarianceMarkerHandler>>>,
    pub bind_binary_expression_flows: RefCell<Arena<BindBinaryExpressionFlow>>,
    pub type_checkers: RefCell<Arena<TypeChecker>>,
    pub read_file_callbacks: RefCell<Arena<Box<dyn ReadFileCallback>>>,
    pub binders: RefCell<Arena<Binder>>,
    pub get_source_files: RefCell<Arena<Box<dyn GetSourceFile>>>,
    pub get_symlink_caches: RefCell<Arena<Box<dyn GetSymlinkCache>>>,
    pub emit_binary_expressions: RefCell<Arena<EmitBinaryExpression>>,
    pub relative_to_build_infos: RefCell<Arena<Box<dyn RelativeToBuildInfo>>>,
    pub print_handlers: RefCell<Arena<Box<dyn PrintHandlers>>>,
    pub get_resolved_project_references: RefCell<Arena<Box<dyn GetResolvedProjectReferences>>>,
    pub for_each_resolved_project_references: RefCell<Arena<Box<dyn ForEachResolvedProjectReference>>>,
    pub compiler_host_likes: RefCell<Arena<Box<dyn CompilerHostLike>>>,
    pub directory_structure_hosts: RefCell<Arena<Box<dyn DirectoryStructureHost>>>,
    pub builder_programs: RefCell<Arena<Box<dyn BuilderProgram>>>,
    pub type_reference_directive_resolution_caches: RefCell<Arena<TypeReferenceDirectiveResolutionCache>>,
    pub module_resolution_caches: RefCell<Arena<ModuleResolutionCache>>,
    pub parse_config_file_hosts: RefCell<Arena<Box<dyn ParseConfigFileHost>>>,
    pub file_preprocessing_diagnostics: RefCell<Arena<FilePreprocessingDiagnostics>>,
    pub actual_resolve_module_names_workers: RefCell<Arena<Box<dyn ActualResolveModuleNamesWorker>>>,
    pub actual_resolve_type_reference_directive_names_workers: RefCell<Arena<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>>>,
    pub get_program_build_infos: RefCell<Arena<Box<dyn GetProgramBuildInfo>>>,
    pub load_with_mode_aware_cache_loaders: RefCell<Arena<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>>>,
    pub load_with_local_cache_loaders: RefCell<Arena<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>>>,
    pub symbol_accessibility_diagnostics: RefCell<Arena<SymbolAccessibilityDiagnostic>>,
    pub code_blocks: RefCell<Arena<CodeBlock>>,
    pub private_identifier_environments: RefCell<Arena<PrivateIdentifierEnvironment>>,
    pub private_identifier_infos: RefCell<Arena<PrivateIdentifierInfo>>,
    pub external_module_infos: RefCell<Arena<ExternalModuleInfo>>,
    pub resolved_modules_with_failed_lookup_locations: RefCell<Arena<ResolvedModuleWithFailedLookupLocations>>,
    pub resolved_type_reference_directives_with_failed_lookup_locations: RefCell<Arena<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
    pub package_json_info_caches: RefCell<Arena<Box<dyn PackageJsonInfoCache>>>,
    pub mode_aware_cache_resolved_module_with_failed_lookup_locations: RefCell<Arena<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>,
    pub mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations: RefCell<Arena<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>>,
    pub per_module_name_caches: RefCell<Arena<PerModuleNameCache>>,
    pub vec_diagnostics: RefCell<Arena<Vec<Id<Diagnostic>>>>,
    pub file_reasons: RefCell<Arena<MultiMap<Path, Id<FileIncludeReason>>>>,
    pub get_symbol_accessibility_diagnostic_interfaces: RefCell<Arena<Box<dyn GetSymbolAccessibilityDiagnosticInterface>>>,
    pub option_vec_nodes: RefCell<Arena<Option<Vec<Id<Node>>>>>,
    pub vec_pending_declarations: RefCell<Arena<Vec<PendingDeclaration>>>,
    pub package_json_infos: RefCell<Arena<PackageJsonInfo>>,
    pub vec_types: RefCell<Arena<Vec<Id<Type>>>>,
    pub pattern_ambient_modules: RefCell<Arena<PatternAmbientModule>>,
    pub check_type_containing_message_chains: RefCell<Arena<Box<dyn CheckTypeContainingMessageChain>>>,
    pub check_type_error_output_containers: RefCell<Arena<Box<dyn CheckTypeErrorOutputContainer>>>,
    pub resolved_type_reference_directives_maps: RefCell<Arena<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>>>,
    pub node_builders: RefCell<Arena<NodeBuilder>>,
    pub node_builder_contexts: RefCell<Arena<NodeBuilderContext>>,
    pub option_vec_types: RefCell<Arena<Option<Vec<Id<Type>>>>>,
    pub option_type_parameter_names: RefCell<Arena<Option<HashMap<TypeId, Id<Node>>>>>,
    pub option_vec_symbols: RefCell<Arena<Option<Vec<Id<Symbol>>>>>,
    pub type_comparers: RefCell<Arena<Box<dyn TypeComparer>>>,
    pub inference_contexts: RefCell<Arena<InferenceContext>>,
    pub skip_trivias: RefCell<Arena<Box<dyn SkipTrivia>>>,
    pub custom_transformer_factory_interfaces: RefCell<Arena<Box<dyn CustomTransformerFactoryInterface>>>,
}

pub trait HasArena {
    fn arena(&self) -> &AllArenas;

    fn node(&self, node: Id<Node>) -> Ref<Node> {
        self.arena().node(node)
    }

    fn alloc_node(&self, node: Node) -> Id<Node> {
        self.arena().alloc_node(node)
    }

    fn type_(&self, type_: Id<Type>) -> Ref<Type> {
        self.arena().type_(type_)
    }

    fn alloc_type(&self, type_: Type) -> Id<Type> {
        self.arena().alloc_type(type_)
    }

    fn type_mapper(&self, type_mapper: Id<TypeMapper>) -> Ref<TypeMapper> {
        self.arena().type_mapper(type_mapper)
    }

    fn alloc_type_mapper(&self, type_mapper: TypeMapper) -> Id<TypeMapper> {
        self.arena().alloc_type_mapper(type_mapper)
    }

    fn symbol(&self, symbol: Id<Symbol>) -> Ref<Symbol> {
        self.arena().symbol(symbol)
    }

    fn alloc_symbol(&self, symbol: Symbol) -> Id<Symbol> {
        self.arena().alloc_symbol(symbol)
    }

    fn transform_nodes_transformation_result(&self, transform_nodes_transformation_result: Id<TransformNodesTransformationResult>) -> Ref<TransformNodesTransformationResult> {
        self.arena().transform_nodes_transformation_result(transform_nodes_transformation_result)
    }

    fn alloc_transform_nodes_transformation_result(&self, transform_nodes_transformation_result: TransformNodesTransformationResult) -> Id<TransformNodesTransformationResult> {
        self.arena().alloc_transform_nodes_transformation_result(transform_nodes_transformation_result)
    }

    fn transformer(&self, transformer: Id<Box<dyn TransformerInterface>>) -> Ref<Box<dyn TransformerInterface>> {
        self.arena().transformer(transformer)
    }

    fn alloc_transformer(&self, transformer: Box<dyn TransformerInterface>) -> Id<Box<dyn TransformerInterface>> {
        self.arena().alloc_transformer(transformer)
    }

    fn transformer_factory(&self, transformer_factory: Id<Box<dyn TransformerFactoryInterface>>) -> Ref<Box<dyn TransformerFactoryInterface>> {
        self.arena().transformer_factory(transformer_factory)
    }

    fn alloc_transformer_factory(&self, transformer_factory: Box<dyn TransformerFactoryInterface>) -> Id<Box<dyn TransformerFactoryInterface>> {
        self.arena().alloc_transformer_factory(transformer_factory)
    }

    fn emit_text_writer(&self, emit_text_writer: Id<Box<dyn EmitTextWriter>>) -> Ref<Box<dyn EmitTextWriter>> {
        self.arena().emit_text_writer(emit_text_writer)
    }

    fn alloc_emit_text_writer(&self, emit_text_writer: Box<dyn EmitTextWriter>) -> Id<Box<dyn EmitTextWriter>> {
        self.arena().alloc_emit_text_writer(emit_text_writer)
    }

    fn symbol_tracker(&self, symbol_tracker: Id<Box<dyn SymbolTracker>>) -> Ref<Box<dyn SymbolTracker>> {
        self.arena().symbol_tracker(symbol_tracker)
    }

    fn alloc_symbol_tracker(&self, symbol_tracker: Box<dyn SymbolTracker>) -> Id<Box<dyn SymbolTracker>> {
        self.arena().alloc_symbol_tracker(symbol_tracker)
    }

    fn emit_host(&self, emit_host: Id<Box<dyn EmitHost>>) -> Ref<Box<dyn EmitHost>> {
        self.arena().emit_host(emit_host)
    }

    fn alloc_emit_host(&self, emit_host: Box<dyn EmitHost>) -> Id<Box<dyn EmitHost>> {
        self.arena().alloc_emit_host(emit_host)
    }

    fn module_specifier_resolution_host_and_get_common_source_directory(&self, module_specifier_resolution_host_and_get_common_source_directory: Id<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>>) -> Ref<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>> {
        self.arena().module_specifier_resolution_host_and_get_common_source_directory(module_specifier_resolution_host_and_get_common_source_directory)
    }

    fn alloc_module_specifier_resolution_host_and_get_common_source_directory(&self, module_specifier_resolution_host_and_get_common_source_directory: Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>) -> Id<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>> {
        self.arena().alloc_module_specifier_resolution_host_and_get_common_source_directory(module_specifier_resolution_host_and_get_common_source_directory)
    }

    fn file_include_reason(&self, file_include_reason: Id<FileIncludeReason>) -> Ref<FileIncludeReason> {
        self.arena().file_include_reason(file_include_reason)
    }

    fn alloc_file_include_reason(&self, file_include_reason: FileIncludeReason) -> Id<FileIncludeReason> {
        self.arena().alloc_file_include_reason(file_include_reason)
    }

    fn system(&self, system: Id<Box<dyn System>>) -> Ref<Box<dyn System>> {
        self.arena().system(system)
    }

    fn alloc_system(&self, system: Box<dyn System>) -> Id<Box<dyn System>> {
        self.arena().alloc_system(system)
    }

    fn source_map_range(&self, source_map_range: Id<SourceMapRange>) -> Ref<SourceMapRange> {
        self.arena().source_map_range(source_map_range)
    }

    fn alloc_source_map_range(&self, source_map_range: SourceMapRange) -> Id<SourceMapRange> {
        self.arena().alloc_source_map_range(source_map_range)
    }

    fn emit_helper(&self, emit_helper: Id<EmitHelper>) -> Ref<EmitHelper> {
        self.arena().emit_helper(emit_helper)
    }

    fn alloc_emit_helper(&self, emit_helper: EmitHelper) -> Id<EmitHelper> {
        self.arena().alloc_emit_helper(emit_helper)
    }

    fn compiler_options(&self, compiler_options: Id<CompilerOptions>) -> Ref<CompilerOptions> {
        self.arena().compiler_options(compiler_options)
    }

    fn alloc_compiler_options(&self, compiler_options: CompilerOptions) -> Id<CompilerOptions> {
        self.arena().alloc_compiler_options(compiler_options)
    }

    fn flow_node(&self, flow_node: Id<FlowNode>) -> Ref<FlowNode> {
        self.arena().flow_node(flow_node)
    }

    fn alloc_flow_node(&self, flow_node: FlowNode) -> Id<FlowNode> {
        self.arena().alloc_flow_node(flow_node)
    }

    fn diagnostic(&self, diagnostic: Id<Diagnostic>) -> Ref<Diagnostic> {
        self.arena().diagnostic(diagnostic)
    }

    fn alloc_diagnostic(&self, diagnostic: Diagnostic) -> Id<Diagnostic> {
        self.arena().alloc_diagnostic(diagnostic)
    }

    fn program(&self, program: Id<Program>) -> Ref<Program> {
        self.arena().program(program)
    }

    fn alloc_program(&self, program: Program) -> Id<Program> {
        self.arena().alloc_program(program)
    }

    fn signature(&self, signature: Id<Signature>) -> Ref<Signature> {
        self.arena().signature(signature)
    }

    fn alloc_signature(&self, signature: Signature) -> Id<Signature> {
        self.arena().alloc_signature(signature)
    }

    fn diagnostic_reporter(&self, diagnostic_reporter: Id<Box<dyn DiagnosticReporter>>) -> Ref<Box<dyn DiagnosticReporter>> {
        self.arena().diagnostic_reporter(diagnostic_reporter)
    }

    fn alloc_diagnostic_reporter(&self, diagnostic_reporter: Box<dyn DiagnosticReporter>) -> Id<Box<dyn DiagnosticReporter>> {
        self.arena().alloc_diagnostic_reporter(diagnostic_reporter)
    }

    fn node_factory(&self, node_factory: Id<NodeFactory>) -> Ref<NodeFactory> {
        self.arena().node_factory(node_factory)
    }

    fn alloc_node_factory(&self, node_factory: NodeFactory) -> Id<NodeFactory> {
        self.arena().alloc_node_factory(node_factory)
    }

    fn base_node_factory(&self, base_node_factory: Id<Box<dyn BaseNodeFactory>>) -> Ref<Box<dyn BaseNodeFactory>> {
        self.arena().base_node_factory(base_node_factory)
    }

    fn alloc_base_node_factory(&self, base_node_factory: Box<dyn BaseNodeFactory>) -> Id<Box<dyn BaseNodeFactory>> {
        self.arena().alloc_base_node_factory(base_node_factory)
    }

    fn emit_resolver(&self, emit_resolver: Id<Box<dyn EmitResolver>>) -> Ref<Box<dyn EmitResolver>> {
        self.arena().emit_resolver(emit_resolver)
    }

    fn alloc_emit_resolver(&self, emit_resolver: Box<dyn EmitResolver>) -> Id<Box<dyn EmitResolver>> {
        self.arena().alloc_emit_resolver(emit_resolver)
    }

    fn resolved_type_reference_directive(&self, resolved_type_reference_directive: Id<ResolvedTypeReferenceDirective>) -> Ref<ResolvedTypeReferenceDirective> {
        self.arena().resolved_type_reference_directive(resolved_type_reference_directive)
    }

    fn alloc_resolved_type_reference_directive(&self, resolved_type_reference_directive: ResolvedTypeReferenceDirective) -> Id<ResolvedTypeReferenceDirective> {
        self.arena().alloc_resolved_type_reference_directive(resolved_type_reference_directive)
    }

    fn compiler_host(&self, compiler_host: Id<Box<dyn CompilerHost>>) -> Ref<Box<dyn CompilerHost>> {
        self.arena().compiler_host(compiler_host)
    }

    fn alloc_compiler_host(&self, compiler_host: Box<dyn CompilerHost>) -> Id<Box<dyn CompilerHost>> {
        self.arena().alloc_compiler_host(compiler_host)
    }

    fn symbol_links(&self, symbol_links: Id<GcCell<SymbolLinks>>) -> Ref<GcCell<SymbolLinks>> {
        self.arena().symbol_links(symbol_links)
    }

    fn alloc_symbol_links(&self, symbol_links: GcCell<SymbolLinks>) -> Id<GcCell<SymbolLinks>> {
        self.arena().alloc_symbol_links(symbol_links)
    }

    fn printer(&self, printer: Id<Printer>) -> Ref<Printer> {
        self.arena().printer(printer)
    }

    fn alloc_printer(&self, printer: Printer) -> Id<Printer> {
        self.arena().alloc_printer(printer)
    }

    fn diagnostic_related_information(&self, diagnostic_related_information: Id<DiagnosticRelatedInformation>) -> Ref<DiagnosticRelatedInformation> {
        self.arena().diagnostic_related_information(diagnostic_related_information)
    }

    fn alloc_diagnostic_related_information(&self, diagnostic_related_information: DiagnosticRelatedInformation) -> Id<DiagnosticRelatedInformation> {
        self.arena().alloc_diagnostic_related_information(diagnostic_related_information)
    }

    fn index_info(&self, index_info: Id<IndexInfo>) -> Ref<IndexInfo> {
        self.arena().index_info(index_info)
    }

    fn alloc_index_info(&self, index_info: IndexInfo) -> Id<IndexInfo> {
        self.arena().alloc_index_info(index_info)
    }

    fn current_parenthesizer_rule(&self, current_parenthesizer_rule: Id<Box<dyn CurrentParenthesizerRule>>) -> Ref<Box<dyn CurrentParenthesizerRule>> {
        self.arena().current_parenthesizer_rule(current_parenthesizer_rule)
    }

    fn alloc_current_parenthesizer_rule(&self, current_parenthesizer_rule: Box<dyn CurrentParenthesizerRule>) -> Id<Box<dyn CurrentParenthesizerRule>> {
        self.arena().alloc_current_parenthesizer_rule(current_parenthesizer_rule)
    }

    fn parenthesizer_rules(&self, parenthesizer_rules: Id<Box<dyn ParenthesizerRules>>) -> Ref<Box<dyn ParenthesizerRules>> {
        self.arena().parenthesizer_rules(parenthesizer_rules)
    }

    fn alloc_parenthesizer_rules(&self, parenthesizer_rules: Box<dyn ParenthesizerRules>) -> Id<Box<dyn ParenthesizerRules>> {
        self.arena().alloc_parenthesizer_rules(parenthesizer_rules)
    }

    fn iteration_types(&self, iteration_types: Id<IterationTypes>) -> Ref<IterationTypes> {
        self.arena().iteration_types(iteration_types)
    }

    fn alloc_iteration_types(&self, iteration_types: IterationTypes) -> Id<IterationTypes> {
        self.arena().alloc_iteration_types(iteration_types)
    }

    fn type_predicate(&self, type_predicate: Id<TypePredicate>) -> Ref<TypePredicate> {
        self.arena().type_predicate(type_predicate)
    }

    fn alloc_type_predicate(&self, type_predicate: TypePredicate) -> Id<TypePredicate> {
        self.arena().alloc_type_predicate(type_predicate)
    }

    fn active_label(&self, active_label: Id<ActiveLabel>) -> Ref<ActiveLabel> {
        self.arena().active_label(active_label)
    }

    fn alloc_active_label(&self, active_label: ActiveLabel) -> Id<ActiveLabel> {
        self.arena().alloc_active_label(active_label)
    }

    fn to_path(&self, to_path: Id<Box<dyn ToPath>>) -> Ref<Box<dyn ToPath>> {
        self.arena().to_path(to_path)
    }

    fn alloc_to_path(&self, to_path: Box<dyn ToPath>) -> Id<Box<dyn ToPath>> {
        self.arena().alloc_to_path(to_path)
    }

    fn module_resolution_host_overrider(&self, module_resolution_host_overrider: Id<Box<dyn ModuleResolutionHostOverrider>>) -> Ref<Box<dyn ModuleResolutionHostOverrider>> {
        self.arena().module_resolution_host_overrider(module_resolution_host_overrider)
    }

    fn alloc_module_resolution_host_overrider(&self, module_resolution_host_overrider: Box<dyn ModuleResolutionHostOverrider>) -> Id<Box<dyn ModuleResolutionHostOverrider>> {
        self.arena().alloc_module_resolution_host_overrider(module_resolution_host_overrider)
    }

    fn wrap_custom_transformer_factory_handle_default(&self, wrap_custom_transformer_factory_handle_default: Id<Box<dyn WrapCustomTransformerFactoryHandleDefault>>) -> Ref<Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
        self.arena().wrap_custom_transformer_factory_handle_default(wrap_custom_transformer_factory_handle_default)
    }

    fn alloc_wrap_custom_transformer_factory_handle_default(&self, wrap_custom_transformer_factory_handle_default: Box<dyn WrapCustomTransformerFactoryHandleDefault>) -> Id<Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
        self.arena().alloc_wrap_custom_transformer_factory_handle_default(wrap_custom_transformer_factory_handle_default)
    }

    fn transformation_context_on_emit_node_overrider(&self, transformation_context_on_emit_node_overrider: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>) -> Ref<Box<dyn TransformationContextOnEmitNodeOverrider>> {
        self.arena().transformation_context_on_emit_node_overrider(transformation_context_on_emit_node_overrider)
    }

    fn alloc_transformation_context_on_emit_node_overrider(&self, transformation_context_on_emit_node_overrider: Box<dyn TransformationContextOnEmitNodeOverrider>) -> Id<Box<dyn TransformationContextOnEmitNodeOverrider>> {
        self.arena().alloc_transformation_context_on_emit_node_overrider(transformation_context_on_emit_node_overrider)
    }

    fn source_map_generator(&self, source_map_generator: Id<Box<dyn SourceMapGenerator>>) -> Ref<Box<dyn SourceMapGenerator>> {
        self.arena().source_map_generator(source_map_generator)
    }

    fn alloc_source_map_generator(&self, source_map_generator: Box<dyn SourceMapGenerator>) -> Id<Box<dyn SourceMapGenerator>> {
        self.arena().alloc_source_map_generator(source_map_generator)
    }

    fn get_canonical_file_name(&self, get_canonical_file_name: Id<Box<dyn GetCanonicalFileName>>) -> Ref<Box<dyn GetCanonicalFileName>> {
        self.arena().get_canonical_file_name(get_canonical_file_name)
    }

    fn alloc_get_canonical_file_name(&self, get_canonical_file_name: Box<dyn GetCanonicalFileName>) -> Id<Box<dyn GetCanonicalFileName>> {
        self.arena().alloc_get_canonical_file_name(get_canonical_file_name)
    }

    fn emit_helper_factory(&self, emit_helper_factory: Id<EmitHelperFactory>) -> Ref<EmitHelperFactory> {
        self.arena().emit_helper_factory(emit_helper_factory)
    }

    fn alloc_emit_helper_factory(&self, emit_helper_factory: EmitHelperFactory) -> Id<EmitHelperFactory> {
        self.arena().alloc_emit_helper_factory(emit_helper_factory)
    }

    fn transformation_context_on_substitute_node_overrider(&self, transformation_context_on_substitute_node_overrider: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>) -> Ref<Box<dyn TransformationContextOnSubstituteNodeOverrider>> {
        self.arena().transformation_context_on_substitute_node_overrider(transformation_context_on_substitute_node_overrider)
    }

    fn alloc_transformation_context_on_substitute_node_overrider(&self, transformation_context_on_substitute_node_overrider: Box<dyn TransformationContextOnSubstituteNodeOverrider>) -> Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>> {
        self.arena().alloc_transformation_context_on_substitute_node_overrider(transformation_context_on_substitute_node_overrider)
    }

    fn parsed_command_line(&self, parsed_command_line: Id<ParsedCommandLine>) -> Ref<ParsedCommandLine> {
        self.arena().parsed_command_line(parsed_command_line)
    }

    fn alloc_parsed_command_line(&self, parsed_command_line: ParsedCommandLine) -> Id<ParsedCommandLine> {
        self.arena().alloc_parsed_command_line(parsed_command_line)
    }

    fn cancellation_token(&self, cancellation_token: Id<Box<dyn CancellationToken>>) -> Ref<Box<dyn CancellationToken>> {
        self.arena().cancellation_token(cancellation_token)
    }

    fn alloc_cancellation_token(&self, cancellation_token: Box<dyn CancellationToken>) -> Id<Box<dyn CancellationToken>> {
        self.arena().alloc_cancellation_token(cancellation_token)
    }

    fn resolved_project_reference(&self, resolved_project_reference: Id<ResolvedProjectReference>) -> Ref<ResolvedProjectReference> {
        self.arena().resolved_project_reference(resolved_project_reference)
    }

    fn alloc_resolved_project_reference(&self, resolved_project_reference: ResolvedProjectReference) -> Id<ResolvedProjectReference> {
        self.arena().alloc_resolved_project_reference(resolved_project_reference)
    }

    fn transformer_factory_or_custom_transformer_factory(&self, transformer_factory_or_custom_transformer_factory: Id<TransformerFactoryOrCustomTransformerFactory>) -> Ref<TransformerFactoryOrCustomTransformerFactory> {
        self.arena().transformer_factory_or_custom_transformer_factory(transformer_factory_or_custom_transformer_factory)
    }

    fn alloc_transformer_factory_or_custom_transformer_factory(&self, transformer_factory_or_custom_transformer_factory: TransformerFactoryOrCustomTransformerFactory) -> Id<TransformerFactoryOrCustomTransformerFactory> {
        self.arena().alloc_transformer_factory_or_custom_transformer_factory(transformer_factory_or_custom_transformer_factory)
    }

    fn symlink_cache(&self, symlink_cache: Id<SymlinkCache>) -> Ref<SymlinkCache> {
        self.arena().symlink_cache(symlink_cache)
    }

    fn alloc_symlink_cache(&self, symlink_cache: SymlinkCache) -> Id<SymlinkCache> {
        self.arena().alloc_symlink_cache(symlink_cache)
    }

    fn write_file_callback(&self, write_file_callback: Id<Box<dyn WriteFileCallback>>) -> Ref<Box<dyn WriteFileCallback>> {
        self.arena().write_file_callback(write_file_callback)
    }

    fn alloc_write_file_callback(&self, write_file_callback: Box<dyn WriteFileCallback>) -> Id<Box<dyn WriteFileCallback>> {
        self.arena().alloc_write_file_callback(write_file_callback)
    }

    fn resolved_module_full(&self, resolved_module_full: Id<ResolvedModuleFull>) -> Ref<ResolvedModuleFull> {
        self.arena().resolved_module_full(resolved_module_full)
    }

    fn alloc_resolved_module_full(&self, resolved_module_full: ResolvedModuleFull) -> Id<ResolvedModuleFull> {
        self.arena().alloc_resolved_module_full(resolved_module_full)
    }

    fn node_array(&self, node_array: Id<NodeArray>) -> Ref<NodeArray> {
        self.arena().node_array(node_array)
    }

    fn alloc_node_array(&self, node_array: NodeArray) -> Id<NodeArray> {
        self.arena().alloc_node_array(node_array)
    }

    fn bundle_file_section(&self, bundle_file_section: Id<BundleFileSection>) -> Ref<BundleFileSection> {
        self.arena().bundle_file_section(bundle_file_section)
    }

    fn alloc_bundle_file_section(&self, bundle_file_section: BundleFileSection) -> Id<BundleFileSection> {
        self.arena().alloc_bundle_file_section(bundle_file_section)
    }

    fn build_info(&self, build_info: Id<BuildInfo>) -> Ref<BuildInfo> {
        self.arena().build_info(build_info)
    }

    fn alloc_build_info(&self, build_info: BuildInfo) -> Id<BuildInfo> {
        self.arena().alloc_build_info(build_info)
    }

    fn program_build_info(&self, program_build_info: Id<ProgramBuildInfo>) -> Ref<ProgramBuildInfo> {
        self.arena().program_build_info(program_build_info)
    }

    fn alloc_program_build_info(&self, program_build_info: ProgramBuildInfo) -> Id<ProgramBuildInfo> {
        self.arena().alloc_program_build_info(program_build_info)
    }

    fn bundle_build_info(&self, bundle_build_info: Id<BundleBuildInfo>) -> Ref<BundleBuildInfo> {
        self.arena().bundle_build_info(bundle_build_info)
    }

    fn bundle_build_info_mut(&self, bundle_build_info: Id<BundleBuildInfo>) -> RefMut<BundleBuildInfo> {
        self.arena().bundle_build_info_mut(bundle_build_info)
    }

    fn alloc_bundle_build_info(&self, bundle_build_info: BundleBuildInfo) -> Id<BundleBuildInfo> {
        self.arena().alloc_bundle_build_info(bundle_build_info)
    }

    fn bundle_file_info(&self, bundle_file_info: Id<BundleFileInfo>) -> Ref<BundleFileInfo> {
        self.arena().bundle_file_info(bundle_file_info)
    }

    fn bundle_file_info_mut(&self, bundle_file_info: Id<BundleFileInfo>) -> RefMut<BundleFileInfo> {
        self.arena().bundle_file_info_mut(bundle_file_info)
    }

    fn alloc_bundle_file_info(&self, bundle_file_info: BundleFileInfo) -> Id<BundleFileInfo> {
        self.arena().alloc_bundle_file_info(bundle_file_info)
    }

    fn symbol_table(&self, symbol_table: Id<SymbolTable>) -> Ref<SymbolTable> {
        self.arena().symbol_table(symbol_table)
    }

    fn symbol_table_mut(&self, symbol_table: Id<SymbolTable>) -> RefMut<SymbolTable> {
        self.arena().symbol_table_mut(symbol_table)
    }

    fn alloc_symbol_table(&self, symbol_table: SymbolTable) -> Id<SymbolTable> {
        self.arena().alloc_symbol_table(symbol_table)
    }

    fn inference_info(&self, inference_info: Id<InferenceInfo>) -> Ref<InferenceInfo> {
        self.arena().inference_info(inference_info)
    }

    fn alloc_inference_info(&self, inference_info: InferenceInfo) -> Id<InferenceInfo> {
        self.arena().alloc_inference_info(inference_info)
    }

    fn sys_format_diagnostics_host(&self, sys_format_diagnostics_host: Id<SysFormatDiagnosticsHost>) -> Ref<SysFormatDiagnosticsHost> {
        self.arena().sys_format_diagnostics_host(sys_format_diagnostics_host)
    }

    fn alloc_sys_format_diagnostics_host(&self, sys_format_diagnostics_host: SysFormatDiagnosticsHost) -> Id<SysFormatDiagnosticsHost> {
        self.arena().alloc_sys_format_diagnostics_host(sys_format_diagnostics_host)
    }

    fn class_lexical_environment(&self, class_lexical_environment: Id<ClassLexicalEnvironment>) -> Ref<ClassLexicalEnvironment> {
        self.arena().class_lexical_environment(class_lexical_environment)
    }

    fn class_lexical_environment_mut(&self, class_lexical_environment: Id<ClassLexicalEnvironment>) -> RefMut<ClassLexicalEnvironment> {
        self.arena().class_lexical_environment_mut(class_lexical_environment)
    }

    fn alloc_class_lexical_environment(&self, class_lexical_environment: ClassLexicalEnvironment) -> Id<ClassLexicalEnvironment> {
        self.arena().alloc_class_lexical_environment(class_lexical_environment)
    }

    fn converted_loop_state(&self, converted_loop_state: Id<ConvertedLoopState>) -> Ref<ConvertedLoopState> {
        self.arena().converted_loop_state(converted_loop_state)
    }

    fn converted_loop_state_mut(&self, converted_loop_state: Id<ConvertedLoopState>) -> RefMut<ConvertedLoopState> {
        self.arena().converted_loop_state_mut(converted_loop_state)
    }

    fn alloc_converted_loop_state(&self, converted_loop_state: ConvertedLoopState) -> Id<ConvertedLoopState> {
        self.arena().alloc_converted_loop_state(converted_loop_state)
    }

    fn emit_helper_text_callback(&self, emit_helper_text_callback: Id<Box<dyn EmitHelperTextCallback>>) -> Ref<Box<dyn EmitHelperTextCallback>> {
        self.arena().emit_helper_text_callback(emit_helper_text_callback)
    }

    fn alloc_emit_helper_text_callback(&self, emit_helper_text_callback: Box<dyn EmitHelperTextCallback>) -> Id<Box<dyn EmitHelperTextCallback>> {
        self.arena().alloc_emit_helper_text_callback(emit_helper_text_callback)
    }

    fn conditional_root(&self, conditional_root: Id<ConditionalRoot>) -> Ref<ConditionalRoot> {
        self.arena().conditional_root(conditional_root)
    }

    fn conditional_root_mut(&self, conditional_root: Id<ConditionalRoot>) -> RefMut<ConditionalRoot> {
        self.arena().conditional_root_mut(conditional_root)
    }

    fn alloc_conditional_root(&self, conditional_root: ConditionalRoot) -> Id<ConditionalRoot> {
        self.arena().alloc_conditional_root(conditional_root)
    }

    fn emit_node(&self, emit_node: Id<EmitNode>) -> Ref<EmitNode> {
        self.arena().emit_node(emit_node)
    }

    fn emit_node_mut(&self, emit_node: Id<EmitNode>) -> RefMut<EmitNode> {
        self.arena().emit_node_mut(emit_node)
    }

    fn alloc_emit_node(&self, emit_node: EmitNode) -> Id<EmitNode> {
        self.arena().alloc_emit_node(emit_node)
    }

    fn check_binary_expression(&self, check_binary_expression: Id<CheckBinaryExpression>) -> Ref<CheckBinaryExpression> {
        self.arena().check_binary_expression(check_binary_expression)
    }

    fn alloc_check_binary_expression(&self, check_binary_expression: CheckBinaryExpression) -> Id<CheckBinaryExpression> {
        self.arena().alloc_check_binary_expression(check_binary_expression)
    }

    fn source_map_source(&self, source_map_source: Id<SourceMapSource>) -> Ref<SourceMapSource> {
        self.arena().source_map_source(source_map_source)
    }

    fn alloc_source_map_source(&self, source_map_source: SourceMapSource) -> Id<SourceMapSource> {
        self.arena().alloc_source_map_source(source_map_source)
    }

    fn outofband_variance_marker_handler(&self, outofband_variance_marker_handler: Id<Box<dyn OutofbandVarianceMarkerHandler>>) -> Ref<Box<dyn OutofbandVarianceMarkerHandler>> {
        self.arena().outofband_variance_marker_handler(outofband_variance_marker_handler)
    }

    fn alloc_outofband_variance_marker_handler(&self, outofband_variance_marker_handler: Box<dyn OutofbandVarianceMarkerHandler>) -> Id<Box<dyn OutofbandVarianceMarkerHandler>> {
        self.arena().alloc_outofband_variance_marker_handler(outofband_variance_marker_handler)
    }

    fn bind_binary_expression_flow(&self, bind_binary_expression_flow: Id<BindBinaryExpressionFlow>) -> Ref<BindBinaryExpressionFlow> {
        self.arena().bind_binary_expression_flow(bind_binary_expression_flow)
    }

    fn alloc_bind_binary_expression_flow(&self, bind_binary_expression_flow: BindBinaryExpressionFlow) -> Id<BindBinaryExpressionFlow> {
        self.arena().alloc_bind_binary_expression_flow(bind_binary_expression_flow)
    }

    fn type_checker(&self, type_checker: Id<TypeChecker>) -> Ref<TypeChecker> {
        self.arena().type_checker(type_checker)
    }

    fn alloc_type_checker(&self, type_checker: TypeChecker) -> Id<TypeChecker> {
        self.arena().alloc_type_checker(type_checker)
    }

    fn read_file_callback(&self, read_file_callback: Id<Box<dyn ReadFileCallback>>) -> Ref<Box<dyn ReadFileCallback>> {
        self.arena().read_file_callback(read_file_callback)
    }

    fn alloc_read_file_callback(&self, read_file_callback: Box<dyn ReadFileCallback>) -> Id<Box<dyn ReadFileCallback>> {
        self.arena().alloc_read_file_callback(read_file_callback)
    }

    fn binder(&self, binder: Id<Binder>) -> Ref<Binder> {
        self.arena().binder(binder)
    }

    fn alloc_binder(&self, binder: Binder) -> Id<Binder> {
        self.arena().alloc_binder(binder)
    }

    fn get_source_file_ref(&self, get_source_file: Id<Box<dyn GetSourceFile>>) -> Ref<Box<dyn GetSourceFile>> {
        self.arena().get_source_file_ref(get_source_file)
    }

    fn alloc_get_source_file(&self, get_source_file: Box<dyn GetSourceFile>) -> Id<Box<dyn GetSourceFile>> {
        self.arena().alloc_get_source_file(get_source_file)
    }

    fn get_symlink_cache(&self, get_symlink_cache: Id<Box<dyn GetSymlinkCache>>) -> Ref<Box<dyn GetSymlinkCache>> {
        self.arena().get_symlink_cache(get_symlink_cache)
    }

    fn alloc_get_symlink_cache(&self, get_symlink_cache: Box<dyn GetSymlinkCache>) -> Id<Box<dyn GetSymlinkCache>> {
        self.arena().alloc_get_symlink_cache(get_symlink_cache)
    }

    fn emit_binary_expression(&self, emit_binary_expression: Id<EmitBinaryExpression>) -> Ref<EmitBinaryExpression> {
        self.arena().emit_binary_expression(emit_binary_expression)
    }

    fn alloc_emit_binary_expression(&self, emit_binary_expression: EmitBinaryExpression) -> Id<EmitBinaryExpression> {
        self.arena().alloc_emit_binary_expression(emit_binary_expression)
    }

    fn relative_to_build_info(&self, relative_to_build_info: Id<Box<dyn RelativeToBuildInfo>>) -> Ref<Box<dyn RelativeToBuildInfo>> {
        self.arena().relative_to_build_info(relative_to_build_info)
    }

    fn alloc_relative_to_build_info(&self, relative_to_build_info: Box<dyn RelativeToBuildInfo>) -> Id<Box<dyn RelativeToBuildInfo>> {
        self.arena().alloc_relative_to_build_info(relative_to_build_info)
    }

    fn print_handlers(&self, print_handlers: Id<Box<dyn PrintHandlers>>) -> Ref<Box<dyn PrintHandlers>> {
        self.arena().print_handlers(print_handlers)
    }

    fn alloc_print_handlers(&self, print_handlers: Box<dyn PrintHandlers>) -> Id<Box<dyn PrintHandlers>> {
        self.arena().alloc_print_handlers(print_handlers)
    }

    fn get_resolved_project_references_ref(&self, get_resolved_project_references: Id<Box<dyn GetResolvedProjectReferences>>) -> Ref<Box<dyn GetResolvedProjectReferences>> {
        self.arena().get_resolved_project_references_ref(get_resolved_project_references)
    }

    fn alloc_get_resolved_project_references(&self, get_resolved_project_references: Box<dyn GetResolvedProjectReferences>) -> Id<Box<dyn GetResolvedProjectReferences>> {
        self.arena().alloc_get_resolved_project_references(get_resolved_project_references)
    }

    fn for_each_resolved_project_reference_ref(&self, for_each_resolved_project_reference: Id<Box<dyn ForEachResolvedProjectReference>>) -> Ref<Box<dyn ForEachResolvedProjectReference>> {
        self.arena().for_each_resolved_project_reference_ref(for_each_resolved_project_reference)
    }

    fn alloc_for_each_resolved_project_reference(&self, for_each_resolved_project_reference: Box<dyn ForEachResolvedProjectReference>) -> Id<Box<dyn ForEachResolvedProjectReference>> {
        self.arena().alloc_for_each_resolved_project_reference(for_each_resolved_project_reference)
    }

    fn compiler_host_like(&self, compiler_host_like: Id<Box<dyn CompilerHostLike>>) -> Ref<Box<dyn CompilerHostLike>> {
        self.arena().compiler_host_like(compiler_host_like)
    }

    fn alloc_compiler_host_like(&self, compiler_host_like: Box<dyn CompilerHostLike>) -> Id<Box<dyn CompilerHostLike>> {
        self.arena().alloc_compiler_host_like(compiler_host_like)
    }

    fn directory_structure_host(&self, directory_structure_host: Id<Box<dyn DirectoryStructureHost>>) -> Ref<Box<dyn DirectoryStructureHost>> {
        self.arena().directory_structure_host(directory_structure_host)
    }

    fn alloc_directory_structure_host(&self, directory_structure_host: Box<dyn DirectoryStructureHost>) -> Id<Box<dyn DirectoryStructureHost>> {
        self.arena().alloc_directory_structure_host(directory_structure_host)
    }

    fn builder_program(&self, builder_program: Id<Box<dyn BuilderProgram>>) -> Ref<Box<dyn BuilderProgram>> {
        self.arena().builder_program(builder_program)
    }

    fn alloc_builder_program(&self, builder_program: Box<dyn BuilderProgram>) -> Id<Box<dyn BuilderProgram>> {
        self.arena().alloc_builder_program(builder_program)
    }

    fn type_reference_directive_resolution_cache(&self, type_reference_directive_resolution_cache: Id<TypeReferenceDirectiveResolutionCache>) -> Ref<TypeReferenceDirectiveResolutionCache> {
        self.arena().type_reference_directive_resolution_cache(type_reference_directive_resolution_cache)
    }

    fn alloc_type_reference_directive_resolution_cache(&self, type_reference_directive_resolution_cache: TypeReferenceDirectiveResolutionCache) -> Id<TypeReferenceDirectiveResolutionCache> {
        self.arena().alloc_type_reference_directive_resolution_cache(type_reference_directive_resolution_cache)
    }

    fn module_resolution_cache(&self, module_resolution_cache: Id<ModuleResolutionCache>) -> Ref<ModuleResolutionCache> {
        self.arena().module_resolution_cache(module_resolution_cache)
    }

    fn alloc_module_resolution_cache(&self, module_resolution_cache: ModuleResolutionCache) -> Id<ModuleResolutionCache> {
        self.arena().alloc_module_resolution_cache(module_resolution_cache)
    }

    fn parse_config_file_host(&self, parse_config_file_host: Id<Box<dyn ParseConfigFileHost>>) -> Ref<Box<dyn ParseConfigFileHost>> {
        self.arena().parse_config_file_host(parse_config_file_host)
    }

    fn alloc_parse_config_file_host(&self, parse_config_file_host: Box<dyn ParseConfigFileHost>) -> Id<Box<dyn ParseConfigFileHost>> {
        self.arena().alloc_parse_config_file_host(parse_config_file_host)
    }

    fn file_preprocessing_diagnostics(&self, file_preprocessing_diagnostics: Id<FilePreprocessingDiagnostics>) -> Ref<FilePreprocessingDiagnostics> {
        self.arena().file_preprocessing_diagnostics(file_preprocessing_diagnostics)
    }

    fn alloc_file_preprocessing_diagnostics(&self, file_preprocessing_diagnostics: FilePreprocessingDiagnostics) -> Id<FilePreprocessingDiagnostics> {
        self.arena().alloc_file_preprocessing_diagnostics(file_preprocessing_diagnostics)
    }

    fn actual_resolve_module_names_worker(&self, actual_resolve_module_names_worker: Id<Box<dyn ActualResolveModuleNamesWorker>>) -> Ref<Box<dyn ActualResolveModuleNamesWorker>> {
        self.arena().actual_resolve_module_names_worker(actual_resolve_module_names_worker)
    }

    fn alloc_actual_resolve_module_names_worker(&self, actual_resolve_module_names_worker: Box<dyn ActualResolveModuleNamesWorker>) -> Id<Box<dyn ActualResolveModuleNamesWorker>> {
        self.arena().alloc_actual_resolve_module_names_worker(actual_resolve_module_names_worker)
    }

    fn actual_resolve_type_reference_directive_names_worker(&self, actual_resolve_type_reference_directive_names_worker: Id<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>>) -> Ref<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>> {
        self.arena().actual_resolve_type_reference_directive_names_worker(actual_resolve_type_reference_directive_names_worker)
    }

    fn alloc_actual_resolve_type_reference_directive_names_worker(&self, actual_resolve_type_reference_directive_names_worker: Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>) -> Id<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>> {
        self.arena().alloc_actual_resolve_type_reference_directive_names_worker(actual_resolve_type_reference_directive_names_worker)
    }

    fn get_program_build_info_ref(&self, get_program_build_info: Id<Box<dyn GetProgramBuildInfo>>) -> Ref<Box<dyn GetProgramBuildInfo>> {
        self.arena().get_program_build_info_ref(get_program_build_info)
    }

    fn alloc_get_program_build_info(&self, get_program_build_info: Box<dyn GetProgramBuildInfo>) -> Id<Box<dyn GetProgramBuildInfo>> {
        self.arena().alloc_get_program_build_info(get_program_build_info)
    }

    fn load_with_mode_aware_cache_loader(&self, load_with_mode_aware_cache_loader: Id<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>>) -> Ref<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>> {
        self.arena().load_with_mode_aware_cache_loader(load_with_mode_aware_cache_loader)
    }

    fn alloc_load_with_mode_aware_cache_loader(&self, load_with_mode_aware_cache_loader: Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>) -> Id<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>> {
        self.arena().alloc_load_with_mode_aware_cache_loader(load_with_mode_aware_cache_loader)
    }

    fn load_with_local_cache_loader(&self, load_with_local_cache_loader: Id<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>>) -> Ref<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>> {
        self.arena().load_with_local_cache_loader(load_with_local_cache_loader)
    }

    fn alloc_load_with_local_cache_loader(&self, load_with_local_cache_loader: Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>) -> Id<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>> {
        self.arena().alloc_load_with_local_cache_loader(load_with_local_cache_loader)
    }

    fn symbol_accessibility_diagnostic(&self, symbol_accessibility_diagnostic: Id<SymbolAccessibilityDiagnostic>) -> Ref<SymbolAccessibilityDiagnostic> {
        self.arena().symbol_accessibility_diagnostic(symbol_accessibility_diagnostic)
    }

    fn alloc_symbol_accessibility_diagnostic(&self, symbol_accessibility_diagnostic: SymbolAccessibilityDiagnostic) -> Id<SymbolAccessibilityDiagnostic> {
        self.arena().alloc_symbol_accessibility_diagnostic(symbol_accessibility_diagnostic)
    }

    fn code_block(&self, code_block: Id<CodeBlock>) -> Ref<CodeBlock> {
        self.arena().code_block(code_block)
    }

    fn code_block_mut(&self, code_block: Id<CodeBlock>) -> RefMut<CodeBlock> {
        self.arena().code_block_mut(code_block)
    }

    fn alloc_code_block(&self, code_block: CodeBlock) -> Id<CodeBlock> {
        self.arena().alloc_code_block(code_block)
    }

    fn private_identifier_environment(&self, private_identifier_environment: Id<PrivateIdentifierEnvironment>) -> Ref<PrivateIdentifierEnvironment> {
        self.arena().private_identifier_environment(private_identifier_environment)
    }

    fn private_identifier_environment_mut(&self, private_identifier_environment: Id<PrivateIdentifierEnvironment>) -> RefMut<PrivateIdentifierEnvironment> {
        self.arena().private_identifier_environment_mut(private_identifier_environment)
    }

    fn alloc_private_identifier_environment(&self, private_identifier_environment: PrivateIdentifierEnvironment) -> Id<PrivateIdentifierEnvironment> {
        self.arena().alloc_private_identifier_environment(private_identifier_environment)
    }

    fn private_identifier_info(&self, private_identifier_info: Id<PrivateIdentifierInfo>) -> Ref<PrivateIdentifierInfo> {
        self.arena().private_identifier_info(private_identifier_info)
    }

    fn private_identifier_info_mut(&self, private_identifier_info: Id<PrivateIdentifierInfo>) -> RefMut<PrivateIdentifierInfo> {
        self.arena().private_identifier_info_mut(private_identifier_info)
    }

    fn alloc_private_identifier_info(&self, private_identifier_info: PrivateIdentifierInfo) -> Id<PrivateIdentifierInfo> {
        self.arena().alloc_private_identifier_info(private_identifier_info)
    }

    fn external_module_info(&self, external_module_info: Id<ExternalModuleInfo>) -> Ref<ExternalModuleInfo> {
        self.arena().external_module_info(external_module_info)
    }

    fn alloc_external_module_info(&self, external_module_info: ExternalModuleInfo) -> Id<ExternalModuleInfo> {
        self.arena().alloc_external_module_info(external_module_info)
    }

    fn resolved_module_with_failed_lookup_locations(&self, resolved_module_with_failed_lookup_locations: Id<ResolvedModuleWithFailedLookupLocations>) -> Ref<ResolvedModuleWithFailedLookupLocations> {
        self.arena().resolved_module_with_failed_lookup_locations(resolved_module_with_failed_lookup_locations)
    }

    fn alloc_resolved_module_with_failed_lookup_locations(&self, resolved_module_with_failed_lookup_locations: ResolvedModuleWithFailedLookupLocations) -> Id<ResolvedModuleWithFailedLookupLocations> {
        self.arena().alloc_resolved_module_with_failed_lookup_locations(resolved_module_with_failed_lookup_locations)
    }

    fn resolved_type_reference_directive_with_failed_lookup_locations(&self, resolved_type_reference_directive_with_failed_lookup_locations: Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>) -> Ref<ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
        self.arena().resolved_type_reference_directive_with_failed_lookup_locations(resolved_type_reference_directive_with_failed_lookup_locations)
    }

    fn alloc_resolved_type_reference_directive_with_failed_lookup_locations(&self, resolved_type_reference_directive_with_failed_lookup_locations: ResolvedTypeReferenceDirectiveWithFailedLookupLocations) -> Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
        self.arena().alloc_resolved_type_reference_directive_with_failed_lookup_locations(resolved_type_reference_directive_with_failed_lookup_locations)
    }

    fn package_json_info_cache(&self, package_json_info_cache: Id<Box<dyn PackageJsonInfoCache>>) -> Ref<Box<dyn PackageJsonInfoCache>> {
        self.arena().package_json_info_cache(package_json_info_cache)
    }

    fn alloc_package_json_info_cache(&self, package_json_info_cache: Box<dyn PackageJsonInfoCache>) -> Id<Box<dyn PackageJsonInfoCache>> {
        self.arena().alloc_package_json_info_cache(package_json_info_cache)
    }

    fn mode_aware_cache_resolved_module_with_failed_lookup_locations(&self, mode_aware_cache_resolved_module_with_failed_lookup_locations: Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>) -> Ref<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>> {
        self.arena().mode_aware_cache_resolved_module_with_failed_lookup_locations(mode_aware_cache_resolved_module_with_failed_lookup_locations)
    }

    fn alloc_mode_aware_cache_resolved_module_with_failed_lookup_locations(&self, mode_aware_cache_resolved_module_with_failed_lookup_locations: ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>) -> Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>> {
        self.arena().alloc_mode_aware_cache_resolved_module_with_failed_lookup_locations(mode_aware_cache_resolved_module_with_failed_lookup_locations)
    }

    fn mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(&self, mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations: Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>) -> Ref<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>> {
        self.arena().mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations)
    }

    fn alloc_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(&self, mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations: ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>) -> Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>> {
        self.arena().alloc_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations)
    }

    fn per_module_name_cache(&self, per_module_name_cache: Id<PerModuleNameCache>) -> Ref<PerModuleNameCache> {
        self.arena().per_module_name_cache(per_module_name_cache)
    }

    fn alloc_per_module_name_cache(&self, per_module_name_cache: PerModuleNameCache) -> Id<PerModuleNameCache> {
        self.arena().alloc_per_module_name_cache(per_module_name_cache)
    }

    fn vec_diagnostic(&self, vec_diagnostic: Id<Vec<Id<Diagnostic>>>) -> Ref<Vec<Id<Diagnostic>>> {
        self.arena().vec_diagnostic(vec_diagnostic)
    }

    fn vec_diagnostic_mut(&self, vec_diagnostic: Id<Vec<Id<Diagnostic>>>) -> RefMut<Vec<Id<Diagnostic>>> {
        self.arena().vec_diagnostic_mut(vec_diagnostic)
    }

    fn alloc_vec_diagnostic(&self, vec_diagnostic: Vec<Id<Diagnostic>>) -> Id<Vec<Id<Diagnostic>>> {
        self.arena().alloc_vec_diagnostic(vec_diagnostic)
    }

    fn file_reasons(&self, file_reasons: Id<MultiMap<Path, Id<FileIncludeReason>>>) -> Ref<MultiMap<Path, Id<FileIncludeReason>>> {
        self.arena().file_reasons(file_reasons)
    }

    fn file_reasons_mut(&self, file_reasons: Id<MultiMap<Path, Id<FileIncludeReason>>>) -> RefMut<MultiMap<Path, Id<FileIncludeReason>>> {
        self.arena().file_reasons_mut(file_reasons)
    }

    fn alloc_file_reasons(&self, file_reasons: MultiMap<Path, Id<FileIncludeReason>>) -> Id<MultiMap<Path, Id<FileIncludeReason>>> {
        self.arena().alloc_file_reasons(file_reasons)
    }

    fn get_symbol_accessibility_diagnostic_interface(&self, get_symbol_accessibility_diagnostic_interface: Id<Box<dyn GetSymbolAccessibilityDiagnosticInterface>>) -> Ref<Box<dyn GetSymbolAccessibilityDiagnosticInterface>> {
        self.arena().get_symbol_accessibility_diagnostic_interface(get_symbol_accessibility_diagnostic_interface)
    }

    fn alloc_get_symbol_accessibility_diagnostic_interface(&self, get_symbol_accessibility_diagnostic_interface: Box<dyn GetSymbolAccessibilityDiagnosticInterface>) -> Id<Box<dyn GetSymbolAccessibilityDiagnosticInterface>> {
        self.arena().alloc_get_symbol_accessibility_diagnostic_interface(get_symbol_accessibility_diagnostic_interface)
    }

    fn option_vec_node(&self, option_vec_node: Id<Option<Vec<Id<Node>>>>) -> Ref<Option<Vec<Id<Node>>>> {
        self.arena().option_vec_node(option_vec_node)
    }

    fn option_vec_node_mut(&self, option_vec_node: Id<Option<Vec<Id<Node>>>>) -> RefMut<Option<Vec<Id<Node>>>> {
        self.arena().option_vec_node_mut(option_vec_node)
    }

    fn alloc_option_vec_node(&self, option_vec_node: Option<Vec<Id<Node>>>) -> Id<Option<Vec<Id<Node>>>> {
        self.arena().alloc_option_vec_node(option_vec_node)
    }

    fn vec_pending_declaration(&self, vec_pending_declaration: Id<Vec<PendingDeclaration>>) -> Ref<Vec<PendingDeclaration>> {
        self.arena().vec_pending_declaration(vec_pending_declaration)
    }

    fn vec_pending_declaration_mut(&self, vec_pending_declaration: Id<Vec<PendingDeclaration>>) -> RefMut<Vec<PendingDeclaration>> {
        self.arena().vec_pending_declaration_mut(vec_pending_declaration)
    }

    fn alloc_vec_pending_declaration(&self, vec_pending_declaration: Vec<PendingDeclaration>) -> Id<Vec<PendingDeclaration>> {
        self.arena().alloc_vec_pending_declaration(vec_pending_declaration)
    }

    fn package_json_info(&self, package_json_info: Id<PackageJsonInfo>) -> Ref<PackageJsonInfo> {
        self.arena().package_json_info(package_json_info)
    }

    fn alloc_package_json_info(&self, package_json_info: PackageJsonInfo) -> Id<PackageJsonInfo> {
        self.arena().alloc_package_json_info(package_json_info)
    }

    fn vec_type(&self, vec_type: Id<Vec<Id<Type>>>) -> Ref<Vec<Id<Type>>> {
        self.arena().vec_type(vec_type)
    }

    fn alloc_vec_type(&self, vec_type: Vec<Id<Type>>) -> Id<Vec<Id<Type>>> {
        self.arena().alloc_vec_type(vec_type)
    }

    fn pattern_ambient_module(&self, pattern_ambient_module: Id<PatternAmbientModule>) -> Ref<PatternAmbientModule> {
        self.arena().pattern_ambient_module(pattern_ambient_module)
    }

    fn alloc_pattern_ambient_module(&self, pattern_ambient_module: PatternAmbientModule) -> Id<PatternAmbientModule> {
        self.arena().alloc_pattern_ambient_module(pattern_ambient_module)
    }

    fn check_type_containing_message_chain(&self, check_type_containing_message_chain: Id<Box<dyn CheckTypeContainingMessageChain>>) -> Ref<Box<dyn CheckTypeContainingMessageChain>> {
        self.arena().check_type_containing_message_chain(check_type_containing_message_chain)
    }

    fn alloc_check_type_containing_message_chain(&self, check_type_containing_message_chain: Box<dyn CheckTypeContainingMessageChain>) -> Id<Box<dyn CheckTypeContainingMessageChain>> {
        self.arena().alloc_check_type_containing_message_chain(check_type_containing_message_chain)
    }

    fn check_type_error_output_container(&self, check_type_error_output_container: Id<Box<dyn CheckTypeErrorOutputContainer>>) -> Ref<Box<dyn CheckTypeErrorOutputContainer>> {
        self.arena().check_type_error_output_container(check_type_error_output_container)
    }

    fn alloc_check_type_error_output_container(&self, check_type_error_output_container: Box<dyn CheckTypeErrorOutputContainer>) -> Id<Box<dyn CheckTypeErrorOutputContainer>> {
        self.arena().alloc_check_type_error_output_container(check_type_error_output_container)
    }

    fn resolved_type_reference_directives_map(&self, resolved_type_reference_directives_map: Id<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>>) -> Ref<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        self.arena().resolved_type_reference_directives_map(resolved_type_reference_directives_map)
    }

    fn resolved_type_reference_directives_map_mut(&self, resolved_type_reference_directives_map: Id<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>>) -> RefMut<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        self.arena().resolved_type_reference_directives_map_mut(resolved_type_reference_directives_map)
    }

    fn alloc_resolved_type_reference_directives_map(&self, resolved_type_reference_directives_map: HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>) -> Id<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        self.arena().alloc_resolved_type_reference_directives_map(resolved_type_reference_directives_map)
    }

    fn node_builder(&self, node_builder: Id<NodeBuilder>) -> Ref<NodeBuilder> {
        self.arena().node_builder(node_builder)
    }

    fn alloc_node_builder(&self, node_builder: NodeBuilder) -> Id<NodeBuilder> {
        self.arena().alloc_node_builder(node_builder)
    }

    fn node_builder_context(&self, node_builder_context: Id<NodeBuilderContext>) -> Ref<NodeBuilderContext> {
        self.arena().node_builder_context(node_builder_context)
    }

    fn alloc_node_builder_context(&self, node_builder_context: NodeBuilderContext) -> Id<NodeBuilderContext> {
        self.arena().alloc_node_builder_context(node_builder_context)
    }

    fn option_vec_type(&self, option_vec_type: Id<Option<Vec<Id<Type>>>>) -> Ref<Option<Vec<Id<Type>>>> {
        self.arena().option_vec_type(option_vec_type)
    }

    fn option_vec_type_mut(&self, option_vec_type: Id<Option<Vec<Id<Type>>>>) -> RefMut<Option<Vec<Id<Type>>>> {
        self.arena().option_vec_type_mut(option_vec_type)
    }

    fn alloc_option_vec_type(&self, option_vec_type: Option<Vec<Id<Type>>>) -> Id<Option<Vec<Id<Type>>>> {
        self.arena().alloc_option_vec_type(option_vec_type)
    }

    fn option_type_parameter_names(&self, option_type_parameter_names: Id<Option<HashMap<TypeId, Id<Node>>>>) -> Ref<Option<HashMap<TypeId, Id<Node>>>> {
        self.arena().option_type_parameter_names(option_type_parameter_names)
    }

    fn option_type_parameter_names_mut(&self, option_type_parameter_names: Id<Option<HashMap<TypeId, Id<Node>>>>) -> RefMut<Option<HashMap<TypeId, Id<Node>>>> {
        self.arena().option_type_parameter_names_mut(option_type_parameter_names)
    }

    fn alloc_option_type_parameter_names(&self, option_type_parameter_names: Option<HashMap<TypeId, Id<Node>>>) -> Id<Option<HashMap<TypeId, Id<Node>>>> {
        self.arena().alloc_option_type_parameter_names(option_type_parameter_names)
    }

    fn option_vec_symbol(&self, option_vec_symbol: Id<Option<Vec<Id<Symbol>>>>) -> Ref<Option<Vec<Id<Symbol>>>> {
        self.arena().option_vec_symbol(option_vec_symbol)
    }

    fn option_vec_symbol_mut(&self, option_vec_symbol: Id<Option<Vec<Id<Symbol>>>>) -> RefMut<Option<Vec<Id<Symbol>>>> {
        self.arena().option_vec_symbol_mut(option_vec_symbol)
    }

    fn alloc_option_vec_symbol(&self, option_vec_symbol: Option<Vec<Id<Symbol>>>) -> Id<Option<Vec<Id<Symbol>>>> {
        self.arena().alloc_option_vec_symbol(option_vec_symbol)
    }

    fn type_comparer(&self, type_comparer: Id<Box<dyn TypeComparer>>) -> Ref<Box<dyn TypeComparer>> {
        self.arena().type_comparer(type_comparer)
    }

    fn alloc_type_comparer(&self, type_comparer: Box<dyn TypeComparer>) -> Id<Box<dyn TypeComparer>> {
        self.arena().alloc_type_comparer(type_comparer)
    }

    fn inference_context(&self, inference_context: Id<InferenceContext>) -> Ref<InferenceContext> {
        self.arena().inference_context(inference_context)
    }

    fn alloc_inference_context(&self, inference_context: InferenceContext) -> Id<InferenceContext> {
        self.arena().alloc_inference_context(inference_context)
    }

    fn skip_trivia(&self, skip_trivia: Id<Box<dyn SkipTrivia>>) -> Ref<Box<dyn SkipTrivia>> {
        self.arena().skip_trivia(skip_trivia)
    }

    fn alloc_skip_trivia(&self, skip_trivia: Box<dyn SkipTrivia>) -> Id<Box<dyn SkipTrivia>> {
        self.arena().alloc_skip_trivia(skip_trivia)
    }

    fn custom_transformer_factory_interface(&self, custom_transformer_factory_interface: Id<Box<dyn CustomTransformerFactoryInterface>>) -> Ref<Box<dyn CustomTransformerFactoryInterface>> {
        self.arena().custom_transformer_factory_interface(custom_transformer_factory_interface)
    }

    fn alloc_custom_transformer_factory_interface(&self, custom_transformer_factory_interface: Box<dyn CustomTransformerFactoryInterface>) -> Id<Box<dyn CustomTransformerFactoryInterface>> {
        self.arena().alloc_custom_transformer_factory_interface(custom_transformer_factory_interface)
    }
}

impl HasArena for AllArenas {
    fn arena(&self) -> &AllArenas {
        self
    }

    #[track_caller]
    fn node(&self, node: Id<Node>) -> Ref<Node> {
        Ref::map(self.nodes.borrow(), |nodes| &nodes[node])
    }

    fn alloc_node(&self, node: Node) -> Id<Node> {
        let id = self.nodes.borrow_mut().alloc(node);
        id
    }

    #[track_caller]
    fn type_(&self, type_: Id<Type>) -> Ref<Type> {
        Ref::map(self.types.borrow(), |types| &types[type_])
    }

    // #[track_caller]
    // pub fn type_mut(&self, type_: Id<Type>) -> RefMut<Type> {
    //     RefMut::map(self.types.borrow_mut(), |types| &mut types[type_])
    // }

    fn alloc_type(&self, type_: Type) -> Id<Type> {
        let id = self.types.borrow_mut().alloc(type_);
        self.type_(id).set_arena_id(id);
        id
    }

    #[track_caller]
    fn type_mapper(&self, type_mapper: Id<TypeMapper>) -> Ref<TypeMapper> {
        Ref::map(self.type_mappers.borrow(), |type_mappers| {
            &type_mappers[type_mapper]
        })
    }

    fn alloc_type_mapper(&self, type_mapper: TypeMapper) -> Id<TypeMapper> {
        let id = self.type_mappers.borrow_mut().alloc(type_mapper);
        // self.type_mapper(id).set_arena_id(id);
        id
    }

    #[track_caller]
    fn symbol(&self, symbol: Id<Symbol>) -> Ref<Symbol> {
        Ref::map(self.symbols.borrow(), |symbols| &symbols[symbol])
    }

    fn alloc_symbol(&self, symbol: Symbol) -> Id<Symbol> {
        let id = self.symbols.borrow_mut().alloc(symbol);
        id
    }

    #[track_caller]
    fn transform_nodes_transformation_result(&self, transform_nodes_transformation_result: Id<TransformNodesTransformationResult>) -> Ref<TransformNodesTransformationResult> {
        Ref::map(self.transform_nodes_transformation_results.borrow(), |transform_nodes_transformation_results| &transform_nodes_transformation_results[transform_nodes_transformation_result])
    }

    fn alloc_transform_nodes_transformation_result(&self, transform_nodes_transformation_result: TransformNodesTransformationResult) -> Id<TransformNodesTransformationResult> {
        let id = self.transform_nodes_transformation_results.borrow_mut().alloc(transform_nodes_transformation_result);
        id
    }

    #[track_caller]
    fn transformer(&self, transformer: Id<Box<dyn TransformerInterface>>) -> Ref<Box<dyn TransformerInterface>> {
        Ref::map(self.transformers.borrow(), |transformers| &transformers[transformer])
    }

    fn alloc_transformer(&self, transformer: Box<dyn TransformerInterface>) -> Id<Box<dyn TransformerInterface>> {
        let id = self.transformers.borrow_mut().alloc(transformer);
        id
    }

    #[track_caller]
    fn transformer_factory(&self, transformer_factory: Id<Box<dyn TransformerFactoryInterface>>) -> Ref<Box<dyn TransformerFactoryInterface>> {
        Ref::map(self.transformer_factories.borrow(), |transformer_factories| &transformer_factories[transformer_factory])
    }

    fn alloc_transformer_factory(&self, transformer_factory: Box<dyn TransformerFactoryInterface>) -> Id<Box<dyn TransformerFactoryInterface>> {
        let id = self.transformer_factories.borrow_mut().alloc(transformer_factory);
        id
    }

    #[track_caller]
    fn emit_text_writer(&self, emit_text_writer: Id<Box<dyn EmitTextWriter>>) -> Ref<Box<dyn EmitTextWriter>> {
        Ref::map(self.emit_text_writers.borrow(), |emit_text_writers| &emit_text_writers[emit_text_writer])
    }

    fn alloc_emit_text_writer(&self, emit_text_writer: Box<dyn EmitTextWriter>) -> Id<Box<dyn EmitTextWriter>> {
        let id = self.emit_text_writers.borrow_mut().alloc(emit_text_writer);
        id
    }

    #[track_caller]
    fn symbol_tracker(&self, symbol_tracker: Id<Box<dyn SymbolTracker>>) -> Ref<Box<dyn SymbolTracker>> {
        Ref::map(self.symbol_trackers.borrow(), |symbol_trackers| &symbol_trackers[symbol_tracker])
    }

    fn alloc_symbol_tracker(&self, symbol_tracker: Box<dyn SymbolTracker>) -> Id<Box<dyn SymbolTracker>> {
        let id = self.symbol_trackers.borrow_mut().alloc(symbol_tracker);
        id
    }

    #[track_caller]
    fn emit_host(&self, emit_host: Id<Box<dyn EmitHost>>) -> Ref<Box<dyn EmitHost>> {
        Ref::map(self.emit_hosts.borrow(), |emit_hosts| &emit_hosts[emit_host])
    }

    fn alloc_emit_host(&self, emit_host: Box<dyn EmitHost>) -> Id<Box<dyn EmitHost>> {
        let id = self.emit_hosts.borrow_mut().alloc(emit_host);
        id
    }

    #[track_caller]
    fn module_specifier_resolution_host_and_get_common_source_directory(&self, module_specifier_resolution_host_and_get_common_source_directory: Id<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>>) -> Ref<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>> {
        Ref::map(self.module_specifier_resolution_host_and_get_common_source_directories.borrow(), |module_specifier_resolution_host_and_get_common_source_directories| &module_specifier_resolution_host_and_get_common_source_directories[module_specifier_resolution_host_and_get_common_source_directory])
    }

    fn alloc_module_specifier_resolution_host_and_get_common_source_directory(&self, module_specifier_resolution_host_and_get_common_source_directory: Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>) -> Id<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>> {
        let id = self.module_specifier_resolution_host_and_get_common_source_directories.borrow_mut().alloc(module_specifier_resolution_host_and_get_common_source_directory);
        id
    }

    #[track_caller]
    fn file_include_reason(&self, file_include_reason: Id<FileIncludeReason>) -> Ref<FileIncludeReason> {
        Ref::map(self.file_include_reasons.borrow(), |file_include_reasons| &file_include_reasons[file_include_reason])
    }

    fn alloc_file_include_reason(&self, file_include_reason: FileIncludeReason) -> Id<FileIncludeReason> {
        let id = self.file_include_reasons.borrow_mut().alloc(file_include_reason);
        id
    }

    #[track_caller]
    fn system(&self, system: Id<Box<dyn System>>) -> Ref<Box<dyn System>> {
        Ref::map(self.systems.borrow(), |systems| &systems[system])
    }

    fn alloc_system(&self, system: Box<dyn System>) -> Id<Box<dyn System>> {
        let id = self.systems.borrow_mut().alloc(system);
        id
    }

    #[track_caller]
    fn source_map_range(&self, source_map_range: Id<SourceMapRange>) -> Ref<SourceMapRange> {
        Ref::map(self.source_map_ranges.borrow(), |source_map_ranges| &source_map_ranges[source_map_range])
    }

    fn alloc_source_map_range(&self, source_map_range: SourceMapRange) -> Id<SourceMapRange> {
        let id = self.source_map_ranges.borrow_mut().alloc(source_map_range);
        id
    }

    #[track_caller]
    fn emit_helper(&self, emit_helper: Id<EmitHelper>) -> Ref<EmitHelper> {
        Ref::map(self.emit_helpers.borrow(), |emit_helpers| &emit_helpers[emit_helper])
    }

    fn alloc_emit_helper(&self, emit_helper: EmitHelper) -> Id<EmitHelper> {
        let id = self.emit_helpers.borrow_mut().alloc(emit_helper);
        id
    }

    #[track_caller]
    fn compiler_options(&self, compiler_options: Id<CompilerOptions>) -> Ref<CompilerOptions> {
        Ref::map(self.compiler_options.borrow(), |compiler_options_| &compiler_options_[compiler_options])
    }

    fn alloc_compiler_options(&self, compiler_options: CompilerOptions) -> Id<CompilerOptions> {
        let id = self.compiler_options.borrow_mut().alloc(compiler_options);
        id
    }

    #[track_caller]
    fn flow_node(&self, flow_node: Id<FlowNode>) -> Ref<FlowNode> {
        Ref::map(self.flow_nodes.borrow(), |flow_nodes| &flow_nodes[flow_node])
    }

    fn alloc_flow_node(&self, flow_node: FlowNode) -> Id<FlowNode> {
        let id = self.flow_nodes.borrow_mut().alloc(flow_node);
        id
    }

    #[track_caller]
    fn diagnostic(&self, diagnostic: Id<Diagnostic>) -> Ref<Diagnostic> {
        Ref::map(self.diagnostics.borrow(), |diagnostics| &diagnostics[diagnostic])
    }

    fn alloc_diagnostic(&self, diagnostic: Diagnostic) -> Id<Diagnostic> {
        let id = self.diagnostics.borrow_mut().alloc(diagnostic);
        id
    }

    #[track_caller]
    fn program(&self, program: Id<Program>) -> Ref<Program> {
        Ref::map(self.programs.borrow(), |programs| &programs[program])
    }

    fn alloc_program(&self, program: Program) -> Id<Program> {
        let id = self.programs.borrow_mut().alloc(program);
        id
    }

    #[track_caller]
    fn signature(&self, signature: Id<Signature>) -> Ref<Signature> {
        Ref::map(self.signatures.borrow(), |signatures| &signatures[signature])
    }

    fn alloc_signature(&self, signature: Signature) -> Id<Signature> {
        let id = self.signatures.borrow_mut().alloc(signature);
        id
    }

    #[track_caller]
    fn diagnostic_reporter(&self, diagnostic_reporter: Id<Box<dyn DiagnosticReporter>>) -> Ref<Box<dyn DiagnosticReporter>> {
        Ref::map(self.diagnostic_reporters.borrow(), |diagnostic_reporters| &diagnostic_reporters[diagnostic_reporter])
    }

    fn alloc_diagnostic_reporter(&self, diagnostic_reporter: Box<dyn DiagnosticReporter>) -> Id<Box<dyn DiagnosticReporter>> {
        let id = self.diagnostic_reporters.borrow_mut().alloc(diagnostic_reporter);
        id
    }

    #[track_caller]
    fn node_factory(&self, node_factory: Id<NodeFactory>) -> Ref<NodeFactory> {
        Ref::map(self.node_factories.borrow(), |node_factories| &node_factories[node_factory])
    }

    fn alloc_node_factory(&self, node_factory: NodeFactory) -> Id<NodeFactory> {
        let id = self.node_factories.borrow_mut().alloc(node_factory);
        id
    }

    #[track_caller]
    fn base_node_factory(&self, base_node_factory: Id<Box<dyn BaseNodeFactory>>) -> Ref<Box<dyn BaseNodeFactory>> {
        Ref::map(self.base_node_factories.borrow(), |base_node_factories| &base_node_factories[base_node_factory])
    }

    fn alloc_base_node_factory(&self, base_node_factory: Box<dyn BaseNodeFactory>) -> Id<Box<dyn BaseNodeFactory>> {
        let id = self.base_node_factories.borrow_mut().alloc(base_node_factory);
        id
    }

    #[track_caller]
    fn emit_resolver(&self, emit_resolver: Id<Box<dyn EmitResolver>>) -> Ref<Box<dyn EmitResolver>> {
        Ref::map(self.emit_resolvers.borrow(), |emit_resolvers| &emit_resolvers[emit_resolver])
    }

    fn alloc_emit_resolver(&self, emit_resolver: Box<dyn EmitResolver>) -> Id<Box<dyn EmitResolver>> {
        let id = self.emit_resolvers.borrow_mut().alloc(emit_resolver);
        id
    }

    #[track_caller]
    fn resolved_type_reference_directive(&self, resolved_type_reference_directive: Id<ResolvedTypeReferenceDirective>) -> Ref<ResolvedTypeReferenceDirective> {
        Ref::map(self.resolved_type_reference_directives.borrow(), |resolved_type_reference_directives| &resolved_type_reference_directives[resolved_type_reference_directive])
    }

    fn alloc_resolved_type_reference_directive(&self, resolved_type_reference_directive: ResolvedTypeReferenceDirective) -> Id<ResolvedTypeReferenceDirective> {
        let id = self.resolved_type_reference_directives.borrow_mut().alloc(resolved_type_reference_directive);
        id
    }

    #[track_caller]
    fn compiler_host(&self, compiler_host: Id<Box<dyn CompilerHost>>) -> Ref<Box<dyn CompilerHost>> {
        Ref::map(self.compiler_hosts.borrow(), |compiler_hosts| &compiler_hosts[compiler_host])
    }

    fn alloc_compiler_host(&self, compiler_host: Box<dyn CompilerHost>) -> Id<Box<dyn CompilerHost>> {
        let id = self.compiler_hosts.borrow_mut().alloc(compiler_host);
        id
    }

    #[track_caller]
    fn symbol_links(&self, symbol_links: Id<GcCell<SymbolLinks>>) -> Ref<GcCell<SymbolLinks>> {
        Ref::map(self.symbol_links.borrow(), |symbol_links_| &symbol_links_[symbol_links])
    }

    fn alloc_symbol_links(&self, symbol_links: GcCell<SymbolLinks>) -> Id<GcCell<SymbolLinks>> {
        let id = self.symbol_links.borrow_mut().alloc(symbol_links);
        id
    }

    #[track_caller]
    fn printer(&self, printer: Id<Printer>) -> Ref<Printer> {
        Ref::map(self.printers.borrow(), |printers| &printers[printer])
    }

    fn alloc_printer(&self, printer: Printer) -> Id<Printer> {
        let id = self.printers.borrow_mut().alloc(printer);
        id
    }

    #[track_caller]
    fn diagnostic_related_information(&self, diagnostic_related_information: Id<DiagnosticRelatedInformation>) -> Ref<DiagnosticRelatedInformation> {
        Ref::map(self.diagnostic_related_informations.borrow(), |diagnostic_related_informations| &diagnostic_related_informations[diagnostic_related_information])
    }

    fn alloc_diagnostic_related_information(&self, diagnostic_related_information: DiagnosticRelatedInformation) -> Id<DiagnosticRelatedInformation> {
        let id = self.diagnostic_related_informations.borrow_mut().alloc(diagnostic_related_information);
        id
    }

    #[track_caller]
    fn index_info(&self, index_info: Id<IndexInfo>) -> Ref<IndexInfo> {
        Ref::map(self.index_infos.borrow(), |index_infos| &index_infos[index_info])
    }

    fn alloc_index_info(&self, index_info: IndexInfo) -> Id<IndexInfo> {
        let id = self.index_infos.borrow_mut().alloc(index_info);
        id
    }

    #[track_caller]
    fn current_parenthesizer_rule(&self, current_parenthesizer_rule: Id<Box<dyn CurrentParenthesizerRule>>) -> Ref<Box<dyn CurrentParenthesizerRule>> {
        Ref::map(self.current_parenthesizer_rules.borrow(), |current_parenthesizer_rules| &current_parenthesizer_rules[current_parenthesizer_rule])
    }

    fn alloc_current_parenthesizer_rule(&self, current_parenthesizer_rule: Box<dyn CurrentParenthesizerRule>) -> Id<Box<dyn CurrentParenthesizerRule>> {
        let id = self.current_parenthesizer_rules.borrow_mut().alloc(current_parenthesizer_rule);
        id
    }

    #[track_caller]
    fn parenthesizer_rules(&self, parenthesizer_rules: Id<Box<dyn ParenthesizerRules>>) -> Ref<Box<dyn ParenthesizerRules>> {
        Ref::map(self.parenthesizer_rules.borrow(), |parenthesizer_rules_| &parenthesizer_rules_[parenthesizer_rules])
    }

    fn alloc_parenthesizer_rules(&self, parenthesizer_rules: Box<dyn ParenthesizerRules>) -> Id<Box<dyn ParenthesizerRules>> {
        let id = self.parenthesizer_rules.borrow_mut().alloc(parenthesizer_rules);
        id
    }

    #[track_caller]
    fn iteration_types(&self, iteration_types: Id<IterationTypes>) -> Ref<IterationTypes> {
        Ref::map(self.iteration_types.borrow(), |iteration_types_| &iteration_types_[iteration_types])
    }

    fn alloc_iteration_types(&self, iteration_types: IterationTypes) -> Id<IterationTypes> {
        let id = self.iteration_types.borrow_mut().alloc(iteration_types);
        id
    }

    #[track_caller]
    fn type_predicate(&self, type_predicate: Id<TypePredicate>) -> Ref<TypePredicate> {
        Ref::map(self.type_predicates.borrow(), |type_predicates| &type_predicates[type_predicate])
    }

    fn alloc_type_predicate(&self, type_predicate: TypePredicate) -> Id<TypePredicate> {
        let id = self.type_predicates.borrow_mut().alloc(type_predicate);
        id
    }

    #[track_caller]
    fn active_label(&self, active_label: Id<ActiveLabel>) -> Ref<ActiveLabel> {
        Ref::map(self.active_labels.borrow(), |active_labels| &active_labels[active_label])
    }

    fn alloc_active_label(&self, active_label: ActiveLabel) -> Id<ActiveLabel> {
        let id = self.active_labels.borrow_mut().alloc(active_label);
        id
    }

    #[track_caller]
    fn to_path(&self, to_path: Id<Box<dyn ToPath>>) -> Ref<Box<dyn ToPath>> {
        Ref::map(self.to_paths.borrow(), |to_paths| &to_paths[to_path])
    }

    fn alloc_to_path(&self, to_path: Box<dyn ToPath>) -> Id<Box<dyn ToPath>> {
        let id = self.to_paths.borrow_mut().alloc(to_path);
        id
    }

    #[track_caller]
    fn module_resolution_host_overrider(&self, module_resolution_host_overrider: Id<Box<dyn ModuleResolutionHostOverrider>>) -> Ref<Box<dyn ModuleResolutionHostOverrider>> {
        Ref::map(self.module_resolution_host_overriders.borrow(), |module_resolution_host_overriders| &module_resolution_host_overriders[module_resolution_host_overrider])
    }

    fn alloc_module_resolution_host_overrider(&self, module_resolution_host_overrider: Box<dyn ModuleResolutionHostOverrider>) -> Id<Box<dyn ModuleResolutionHostOverrider>> {
        let id = self.module_resolution_host_overriders.borrow_mut().alloc(module_resolution_host_overrider);
        id
    }

    #[track_caller]
    fn wrap_custom_transformer_factory_handle_default(&self, wrap_custom_transformer_factory_handle_default: Id<Box<dyn WrapCustomTransformerFactoryHandleDefault>>) -> Ref<Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
        Ref::map(self.wrap_custom_transformer_factory_handle_defaults.borrow(), |wrap_custom_transformer_factory_handle_defaults| &wrap_custom_transformer_factory_handle_defaults[wrap_custom_transformer_factory_handle_default])
    }

    fn alloc_wrap_custom_transformer_factory_handle_default(&self, wrap_custom_transformer_factory_handle_default: Box<dyn WrapCustomTransformerFactoryHandleDefault>) -> Id<Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
        let id = self.wrap_custom_transformer_factory_handle_defaults.borrow_mut().alloc(wrap_custom_transformer_factory_handle_default);
        id
    }

    #[track_caller]
    fn transformation_context_on_emit_node_overrider(&self, transformation_context_on_emit_node_overrider: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>) -> Ref<Box<dyn TransformationContextOnEmitNodeOverrider>> {
        Ref::map(self.transformation_context_on_emit_node_overriders.borrow(), |transformation_context_on_emit_node_overriders| &transformation_context_on_emit_node_overriders[transformation_context_on_emit_node_overrider])
    }

    fn alloc_transformation_context_on_emit_node_overrider(&self, transformation_context_on_emit_node_overrider: Box<dyn TransformationContextOnEmitNodeOverrider>) -> Id<Box<dyn TransformationContextOnEmitNodeOverrider>> {
        let id = self.transformation_context_on_emit_node_overriders.borrow_mut().alloc(transformation_context_on_emit_node_overrider);
        id
    }

    #[track_caller]
    fn source_map_generator(&self, source_map_generator: Id<Box<dyn SourceMapGenerator>>) -> Ref<Box<dyn SourceMapGenerator>> {
        Ref::map(self.source_map_generators.borrow(), |source_map_generators| &source_map_generators[source_map_generator])
    }

    fn alloc_source_map_generator(&self, source_map_generator: Box<dyn SourceMapGenerator>) -> Id<Box<dyn SourceMapGenerator>> {
        let id = self.source_map_generators.borrow_mut().alloc(source_map_generator);
        id
    }

    #[track_caller]
    fn get_canonical_file_name(&self, get_canonical_file_name: Id<Box<dyn GetCanonicalFileName>>) -> Ref<Box<dyn GetCanonicalFileName>> {
        Ref::map(self.get_canonical_file_names.borrow(), |get_canonical_file_names| &get_canonical_file_names[get_canonical_file_name])
    }

    fn alloc_get_canonical_file_name(&self, get_canonical_file_name: Box<dyn GetCanonicalFileName>) -> Id<Box<dyn GetCanonicalFileName>> {
        let id = self.get_canonical_file_names.borrow_mut().alloc(get_canonical_file_name);
        id
    }

    #[track_caller]
    fn emit_helper_factory(&self, emit_helper_factory: Id<EmitHelperFactory>) -> Ref<EmitHelperFactory> {
        Ref::map(self.emit_helper_factories.borrow(), |emit_helper_factories| &emit_helper_factories[emit_helper_factory])
    }

    fn alloc_emit_helper_factory(&self, emit_helper_factory: EmitHelperFactory) -> Id<EmitHelperFactory> {
        let id = self.emit_helper_factories.borrow_mut().alloc(emit_helper_factory);
        id
    }

    #[track_caller]
    fn transformation_context_on_substitute_node_overrider(&self, transformation_context_on_substitute_node_overrider: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>) -> Ref<Box<dyn TransformationContextOnSubstituteNodeOverrider>> {
        Ref::map(self.transformation_context_on_substitute_node_overriders.borrow(), |transformation_context_on_substitute_node_overriders| &transformation_context_on_substitute_node_overriders[transformation_context_on_substitute_node_overrider])
    }

    fn alloc_transformation_context_on_substitute_node_overrider(&self, transformation_context_on_substitute_node_overrider: Box<dyn TransformationContextOnSubstituteNodeOverrider>) -> Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>> {
        let id = self.transformation_context_on_substitute_node_overriders.borrow_mut().alloc(transformation_context_on_substitute_node_overrider);
        id
    }

    #[track_caller]
    fn parsed_command_line(&self, parsed_command_line: Id<ParsedCommandLine>) -> Ref<ParsedCommandLine> {
        Ref::map(self.parsed_command_lines.borrow(), |parsed_command_lines| &parsed_command_lines[parsed_command_line])
    }

    fn alloc_parsed_command_line(&self, parsed_command_line: ParsedCommandLine) -> Id<ParsedCommandLine> {
        let id = self.parsed_command_lines.borrow_mut().alloc(parsed_command_line);
        id
    }

    #[track_caller]
    fn cancellation_token(&self, cancellation_token: Id<Box<dyn CancellationToken>>) -> Ref<Box<dyn CancellationToken>> {
        Ref::map(self.cancellation_tokens.borrow(), |cancellation_tokens| &cancellation_tokens[cancellation_token])
    }

    fn alloc_cancellation_token(&self, cancellation_token: Box<dyn CancellationToken>) -> Id<Box<dyn CancellationToken>> {
        let id = self.cancellation_tokens.borrow_mut().alloc(cancellation_token);
        id
    }

    #[track_caller]
    fn resolved_project_reference(&self, resolved_project_reference: Id<ResolvedProjectReference>) -> Ref<ResolvedProjectReference> {
        Ref::map(self.resolved_project_references.borrow(), |resolved_project_references| &resolved_project_references[resolved_project_reference])
    }

    fn alloc_resolved_project_reference(&self, resolved_project_reference: ResolvedProjectReference) -> Id<ResolvedProjectReference> {
        let id = self.resolved_project_references.borrow_mut().alloc(resolved_project_reference);
        id
    }

    #[track_caller]
    fn transformer_factory_or_custom_transformer_factory(&self, transformer_factory_or_custom_transformer_factory: Id<TransformerFactoryOrCustomTransformerFactory>) -> Ref<TransformerFactoryOrCustomTransformerFactory> {
        Ref::map(self.transformer_factory_or_custom_transformer_factories.borrow(), |transformer_factory_or_custom_transformer_factories| &transformer_factory_or_custom_transformer_factories[transformer_factory_or_custom_transformer_factory])
    }

    fn alloc_transformer_factory_or_custom_transformer_factory(&self, transformer_factory_or_custom_transformer_factory: TransformerFactoryOrCustomTransformerFactory) -> Id<TransformerFactoryOrCustomTransformerFactory> {
        let id = self.transformer_factory_or_custom_transformer_factories.borrow_mut().alloc(transformer_factory_or_custom_transformer_factory);
        id
    }

    #[track_caller]
    fn symlink_cache(&self, symlink_cache: Id<SymlinkCache>) -> Ref<SymlinkCache> {
        Ref::map(self.symlink_caches.borrow(), |symlink_caches| &symlink_caches[symlink_cache])
    }

    fn alloc_symlink_cache(&self, symlink_cache: SymlinkCache) -> Id<SymlinkCache> {
        let id = self.symlink_caches.borrow_mut().alloc(symlink_cache);
        id
    }

    #[track_caller]
    fn write_file_callback(&self, write_file_callback: Id<Box<dyn WriteFileCallback>>) -> Ref<Box<dyn WriteFileCallback>> {
        Ref::map(self.write_file_callbacks.borrow(), |write_file_callbacks| &write_file_callbacks[write_file_callback])
    }

    fn alloc_write_file_callback(&self, write_file_callback: Box<dyn WriteFileCallback>) -> Id<Box<dyn WriteFileCallback>> {
        let id = self.write_file_callbacks.borrow_mut().alloc(write_file_callback);
        id
    }

    #[track_caller]
    fn resolved_module_full(&self, resolved_module_full: Id<ResolvedModuleFull>) -> Ref<ResolvedModuleFull> {
        Ref::map(self.resolved_module_fulls.borrow(), |resolved_module_fulls| &resolved_module_fulls[resolved_module_full])
    }

    fn alloc_resolved_module_full(&self, resolved_module_full: ResolvedModuleFull) -> Id<ResolvedModuleFull> {
        let id = self.resolved_module_fulls.borrow_mut().alloc(resolved_module_full);
        id
    }

    #[track_caller]
    fn node_array(&self, node_array: Id<NodeArray>) -> Ref<NodeArray> {
        Ref::map(self.node_arrays.borrow(), |node_arrays| &node_arrays[node_array])
    }

    fn alloc_node_array(&self, node_array: NodeArray) -> Id<NodeArray> {
        let id = self.node_arrays.borrow_mut().alloc(node_array);
        id
    }

    #[track_caller]
    fn bundle_file_section(&self, bundle_file_section: Id<BundleFileSection>) -> Ref<BundleFileSection> {
        Ref::map(self.bundle_file_sections.borrow(), |bundle_file_sections| &bundle_file_sections[bundle_file_section])
    }

    fn alloc_bundle_file_section(&self, bundle_file_section: BundleFileSection) -> Id<BundleFileSection> {
        let id = self.bundle_file_sections.borrow_mut().alloc(bundle_file_section);
        id
    }

    #[track_caller]
    fn build_info(&self, build_info: Id<BuildInfo>) -> Ref<BuildInfo> {
        Ref::map(self.build_infos.borrow(), |build_infos| &build_infos[build_info])
    }

    fn alloc_build_info(&self, build_info: BuildInfo) -> Id<BuildInfo> {
        let id = self.build_infos.borrow_mut().alloc(build_info);
        id
    }

    #[track_caller]
    fn program_build_info(&self, program_build_info: Id<ProgramBuildInfo>) -> Ref<ProgramBuildInfo> {
        Ref::map(self.program_build_infos.borrow(), |program_build_infos| &program_build_infos[program_build_info])
    }

    fn alloc_program_build_info(&self, program_build_info: ProgramBuildInfo) -> Id<ProgramBuildInfo> {
        let id = self.program_build_infos.borrow_mut().alloc(program_build_info);
        id
    }

    #[track_caller]
    fn bundle_build_info(&self, bundle_build_info: Id<BundleBuildInfo>) -> Ref<BundleBuildInfo> {
        Ref::map(self.bundle_build_infos.borrow(), |bundle_build_infos| &bundle_build_infos[bundle_build_info])
    }

    fn bundle_build_info_mut(&self, bundle_build_info: Id<BundleBuildInfo>) -> RefMut<BundleBuildInfo> {
        RefMut::map(self.bundle_build_infos.borrow_mut(), |bundle_build_infos| &mut bundle_build_infos[bundle_build_info])
    }

    fn alloc_bundle_build_info(&self, bundle_build_info: BundleBuildInfo) -> Id<BundleBuildInfo> {
        let id = self.bundle_build_infos.borrow_mut().alloc(bundle_build_info);
        id
    }

    #[track_caller]
    fn bundle_file_info(&self, bundle_file_info: Id<BundleFileInfo>) -> Ref<BundleFileInfo> {
        Ref::map(self.bundle_file_infos.borrow(), |bundle_file_infos| &bundle_file_infos[bundle_file_info])
    }

    fn bundle_file_info_mut(&self, bundle_file_info: Id<BundleFileInfo>) -> RefMut<BundleFileInfo> {
        RefMut::map(self.bundle_file_infos.borrow_mut(), |bundle_file_infos| &mut bundle_file_infos[bundle_file_info])
    }

    fn alloc_bundle_file_info(&self, bundle_file_info: BundleFileInfo) -> Id<BundleFileInfo> {
        let id = self.bundle_file_infos.borrow_mut().alloc(bundle_file_info);
        id
    }

    #[track_caller]
    fn symbol_table(&self, symbol_table: Id<SymbolTable>) -> Ref<SymbolTable> {
        Ref::map(self.symbol_tables.borrow(), |symbol_tables| &symbol_tables[symbol_table])
    }

    fn symbol_table_mut(&self, symbol_table: Id<SymbolTable>) -> RefMut<SymbolTable> {
        RefMut::map(self.symbol_tables.borrow_mut(), |symbol_tables| &mut symbol_tables[symbol_table])
    }

    fn alloc_symbol_table(&self, symbol_table: SymbolTable) -> Id<SymbolTable> {
        let id = self.symbol_tables.borrow_mut().alloc(symbol_table);
        id
    }

    #[track_caller]
    fn inference_info(&self, inference_info: Id<InferenceInfo>) -> Ref<InferenceInfo> {
        Ref::map(self.inference_infos.borrow(), |inference_infos| &inference_infos[inference_info])
    }

    fn alloc_inference_info(&self, inference_info: InferenceInfo) -> Id<InferenceInfo> {
        let id = self.inference_infos.borrow_mut().alloc(inference_info);
        id
    }

    #[track_caller]
    fn sys_format_diagnostics_host(&self, sys_format_diagnostics_host: Id<SysFormatDiagnosticsHost>) -> Ref<SysFormatDiagnosticsHost> {
        Ref::map(self.sys_format_diagnostics_hosts.borrow(), |sys_format_diagnostics_hosts| &sys_format_diagnostics_hosts[sys_format_diagnostics_host])
    }

    fn alloc_sys_format_diagnostics_host(&self, sys_format_diagnostics_host: SysFormatDiagnosticsHost) -> Id<SysFormatDiagnosticsHost> {
        let id = self.sys_format_diagnostics_hosts.borrow_mut().alloc(sys_format_diagnostics_host);
        id
    }

    #[track_caller]
    fn class_lexical_environment(&self, class_lexical_environment: Id<ClassLexicalEnvironment>) -> Ref<ClassLexicalEnvironment> {
        Ref::map(self.class_lexical_environments.borrow(), |class_lexical_environments| &class_lexical_environments[class_lexical_environment])
    }

    fn class_lexical_environment_mut(&self, class_lexical_environment: Id<ClassLexicalEnvironment>) -> RefMut<ClassLexicalEnvironment> {
        RefMut::map(self.class_lexical_environments.borrow_mut(), |class_lexical_environments| &mut class_lexical_environments[class_lexical_environment])
    }

    fn alloc_class_lexical_environment(&self, class_lexical_environment: ClassLexicalEnvironment) -> Id<ClassLexicalEnvironment> {
        let id = self.class_lexical_environments.borrow_mut().alloc(class_lexical_environment);
        id
    }

    #[track_caller]
    fn converted_loop_state(&self, converted_loop_state: Id<ConvertedLoopState>) -> Ref<ConvertedLoopState> {
        Ref::map(self.converted_loop_states.borrow(), |converted_loop_states| &converted_loop_states[converted_loop_state])
    }

    fn converted_loop_state_mut(&self, converted_loop_state: Id<ConvertedLoopState>) -> RefMut<ConvertedLoopState> {
        RefMut::map(self.converted_loop_states.borrow_mut(), |converted_loop_states| &mut converted_loop_states[converted_loop_state])
    }

    fn alloc_converted_loop_state(&self, converted_loop_state: ConvertedLoopState) -> Id<ConvertedLoopState> {
        let id = self.converted_loop_states.borrow_mut().alloc(converted_loop_state);
        id
    }

    #[track_caller]
    fn emit_helper_text_callback(&self, emit_helper_text_callback: Id<Box<dyn EmitHelperTextCallback>>) -> Ref<Box<dyn EmitHelperTextCallback>> {
        Ref::map(self.emit_helper_text_callbacks.borrow(), |emit_helper_text_callbacks| &emit_helper_text_callbacks[emit_helper_text_callback])
    }

    fn alloc_emit_helper_text_callback(&self, emit_helper_text_callback: Box<dyn EmitHelperTextCallback>) -> Id<Box<dyn EmitHelperTextCallback>> {
        let id = self.emit_helper_text_callbacks.borrow_mut().alloc(emit_helper_text_callback);
        id
    }

    #[track_caller]
    fn conditional_root(&self, conditional_root: Id<ConditionalRoot>) -> Ref<ConditionalRoot> {
        Ref::map(self.conditional_roots.borrow(), |conditional_roots| &conditional_roots[conditional_root])
    }

    fn conditional_root_mut(&self, conditional_root: Id<ConditionalRoot>) -> RefMut<ConditionalRoot> {
        RefMut::map(self.conditional_roots.borrow_mut(), |conditional_roots| &mut conditional_roots[conditional_root])
    }

    fn alloc_conditional_root(&self, conditional_root: ConditionalRoot) -> Id<ConditionalRoot> {
        let id = self.conditional_roots.borrow_mut().alloc(conditional_root);
        id
    }

    #[track_caller]
    fn emit_node(&self, emit_node: Id<EmitNode>) -> Ref<EmitNode> {
        Ref::map(self.emit_nodes.borrow(), |emit_nodes| &emit_nodes[emit_node])
    }

    fn emit_node_mut(&self, emit_node: Id<EmitNode>) -> RefMut<EmitNode> {
        RefMut::map(self.emit_nodes.borrow_mut(), |emit_nodes| &mut emit_nodes[emit_node])
    }

    fn alloc_emit_node(&self, emit_node: EmitNode) -> Id<EmitNode> {
        let id = self.emit_nodes.borrow_mut().alloc(emit_node);
        id
    }

    #[track_caller]
    fn check_binary_expression(&self, check_binary_expression: Id<CheckBinaryExpression>) -> Ref<CheckBinaryExpression> {
        Ref::map(self.check_binary_expressions.borrow(), |check_binary_expressions| &check_binary_expressions[check_binary_expression])
    }

    fn alloc_check_binary_expression(&self, check_binary_expression: CheckBinaryExpression) -> Id<CheckBinaryExpression> {
        let id = self.check_binary_expressions.borrow_mut().alloc(check_binary_expression);
        id
    }

    #[track_caller]
    fn source_map_source(&self, source_map_source: Id<SourceMapSource>) -> Ref<SourceMapSource> {
        Ref::map(self.source_map_sources.borrow(), |source_map_sources| &source_map_sources[source_map_source])
    }

    fn alloc_source_map_source(&self, source_map_source: SourceMapSource) -> Id<SourceMapSource> {
        let id = self.source_map_sources.borrow_mut().alloc(source_map_source);
        id
    }

    #[track_caller]
    fn outofband_variance_marker_handler(&self, outofband_variance_marker_handler: Id<Box<dyn OutofbandVarianceMarkerHandler>>) -> Ref<Box<dyn OutofbandVarianceMarkerHandler>> {
        Ref::map(self.outofband_variance_marker_handlers.borrow(), |outofband_variance_marker_handlers| &outofband_variance_marker_handlers[outofband_variance_marker_handler])
    }

    fn alloc_outofband_variance_marker_handler(&self, outofband_variance_marker_handler: Box<dyn OutofbandVarianceMarkerHandler>) -> Id<Box<dyn OutofbandVarianceMarkerHandler>> {
        let id = self.outofband_variance_marker_handlers.borrow_mut().alloc(outofband_variance_marker_handler);
        id
    }

    #[track_caller]
    fn bind_binary_expression_flow(&self, bind_binary_expression_flow: Id<BindBinaryExpressionFlow>) -> Ref<BindBinaryExpressionFlow> {
        Ref::map(self.bind_binary_expression_flows.borrow(), |bind_binary_expression_flows| &bind_binary_expression_flows[bind_binary_expression_flow])
    }

    fn alloc_bind_binary_expression_flow(&self, bind_binary_expression_flow: BindBinaryExpressionFlow) -> Id<BindBinaryExpressionFlow> {
        let id = self.bind_binary_expression_flows.borrow_mut().alloc(bind_binary_expression_flow);
        id
    }

    #[track_caller]
    fn type_checker(&self, type_checker: Id<TypeChecker>) -> Ref<TypeChecker> {
        Ref::map(self.type_checkers.borrow(), |type_checkers| &type_checkers[type_checker])
    }

    fn alloc_type_checker(&self, type_checker: TypeChecker) -> Id<TypeChecker> {
        let id = self.type_checkers.borrow_mut().alloc(type_checker);
        id
    }

    #[track_caller]
    fn read_file_callback(&self, read_file_callback: Id<Box<dyn ReadFileCallback>>) -> Ref<Box<dyn ReadFileCallback>> {
        Ref::map(self.read_file_callbacks.borrow(), |read_file_callbacks| &read_file_callbacks[read_file_callback])
    }

    fn alloc_read_file_callback(&self, read_file_callback: Box<dyn ReadFileCallback>) -> Id<Box<dyn ReadFileCallback>> {
        let id = self.read_file_callbacks.borrow_mut().alloc(read_file_callback);
        id
    }

    #[track_caller]
    fn binder(&self, binder: Id<Binder>) -> Ref<Binder> {
        Ref::map(self.binders.borrow(), |binders| &binders[binder])
    }

    fn alloc_binder(&self, binder: Binder) -> Id<Binder> {
        let id = self.binders.borrow_mut().alloc(binder);
        id.ref_(self).set_arena_id(id);
        id
    }

    #[track_caller]
    fn get_source_file_ref(&self, get_source_file: Id<Box<dyn GetSourceFile>>) -> Ref<Box<dyn GetSourceFile>> {
        Ref::map(self.get_source_files.borrow(), |get_source_files| &get_source_files[get_source_file])
    }

    fn alloc_get_source_file(&self, get_source_file: Box<dyn GetSourceFile>) -> Id<Box<dyn GetSourceFile>> {
        let id = self.get_source_files.borrow_mut().alloc(get_source_file);
        id
    }

    #[track_caller]
    fn get_symlink_cache(&self, get_symlink_cache: Id<Box<dyn GetSymlinkCache>>) -> Ref<Box<dyn GetSymlinkCache>> {
        Ref::map(self.get_symlink_caches.borrow(), |get_symlink_caches| &get_symlink_caches[get_symlink_cache])
    }

    fn alloc_get_symlink_cache(&self, get_symlink_cache: Box<dyn GetSymlinkCache>) -> Id<Box<dyn GetSymlinkCache>> {
        let id = self.get_symlink_caches.borrow_mut().alloc(get_symlink_cache);
        id
    }

    #[track_caller]
    fn emit_binary_expression(&self, emit_binary_expression: Id<EmitBinaryExpression>) -> Ref<EmitBinaryExpression> {
        Ref::map(self.emit_binary_expressions.borrow(), |emit_binary_expressions| &emit_binary_expressions[emit_binary_expression])
    }

    fn alloc_emit_binary_expression(&self, emit_binary_expression: EmitBinaryExpression) -> Id<EmitBinaryExpression> {
        let id = self.emit_binary_expressions.borrow_mut().alloc(emit_binary_expression);
        id
    }

    #[track_caller]
    fn relative_to_build_info(&self, relative_to_build_info: Id<Box<dyn RelativeToBuildInfo>>) -> Ref<Box<dyn RelativeToBuildInfo>> {
        Ref::map(self.relative_to_build_infos.borrow(), |relative_to_build_infos| &relative_to_build_infos[relative_to_build_info])
    }

    fn alloc_relative_to_build_info(&self, relative_to_build_info: Box<dyn RelativeToBuildInfo>) -> Id<Box<dyn RelativeToBuildInfo>> {
        let id = self.relative_to_build_infos.borrow_mut().alloc(relative_to_build_info);
        id
    }

    #[track_caller]
    fn print_handlers(&self, print_handlers: Id<Box<dyn PrintHandlers>>) -> Ref<Box<dyn PrintHandlers>> {
        Ref::map(self.print_handlers.borrow(), |print_handlers_| &print_handlers_[print_handlers])
    }

    fn alloc_print_handlers(&self, print_handlers: Box<dyn PrintHandlers>) -> Id<Box<dyn PrintHandlers>> {
        let id = self.print_handlers.borrow_mut().alloc(print_handlers);
        id
    }

    #[track_caller]
    fn get_resolved_project_references_ref(&self, get_resolved_project_references: Id<Box<dyn GetResolvedProjectReferences>>) -> Ref<Box<dyn GetResolvedProjectReferences>> {
        Ref::map(self.get_resolved_project_references.borrow(), |get_resolved_project_references_| &get_resolved_project_references_[get_resolved_project_references])
    }

    fn alloc_get_resolved_project_references(&self, get_resolved_project_references: Box<dyn GetResolvedProjectReferences>) -> Id<Box<dyn GetResolvedProjectReferences>> {
        let id = self.get_resolved_project_references.borrow_mut().alloc(get_resolved_project_references);
        id
    }

    #[track_caller]
    fn for_each_resolved_project_reference_ref(&self, for_each_resolved_project_reference: Id<Box<dyn ForEachResolvedProjectReference>>) -> Ref<Box<dyn ForEachResolvedProjectReference>> {
        Ref::map(self.for_each_resolved_project_references.borrow(), |for_each_resolved_project_references| &for_each_resolved_project_references[for_each_resolved_project_reference])
    }

    fn alloc_for_each_resolved_project_reference(&self, for_each_resolved_project_reference: Box<dyn ForEachResolvedProjectReference>) -> Id<Box<dyn ForEachResolvedProjectReference>> {
        let id = self.for_each_resolved_project_references.borrow_mut().alloc(for_each_resolved_project_reference);
        id
    }

    #[track_caller]
    fn compiler_host_like(&self, compiler_host_like: Id<Box<dyn CompilerHostLike>>) -> Ref<Box<dyn CompilerHostLike>> {
        Ref::map(self.compiler_host_likes.borrow(), |compiler_host_likes| &compiler_host_likes[compiler_host_like])
    }

    fn alloc_compiler_host_like(&self, compiler_host_like: Box<dyn CompilerHostLike>) -> Id<Box<dyn CompilerHostLike>> {
        let id = self.compiler_host_likes.borrow_mut().alloc(compiler_host_like);
        id
    }

    #[track_caller]
    fn directory_structure_host(&self, directory_structure_host: Id<Box<dyn DirectoryStructureHost>>) -> Ref<Box<dyn DirectoryStructureHost>> {
        Ref::map(self.directory_structure_hosts.borrow(), |directory_structure_hosts| &directory_structure_hosts[directory_structure_host])
    }

    fn alloc_directory_structure_host(&self, directory_structure_host: Box<dyn DirectoryStructureHost>) -> Id<Box<dyn DirectoryStructureHost>> {
        let id = self.directory_structure_hosts.borrow_mut().alloc(directory_structure_host);
        id
    }

    #[track_caller]
    fn builder_program(&self, builder_program: Id<Box<dyn BuilderProgram>>) -> Ref<Box<dyn BuilderProgram>> {
        Ref::map(self.builder_programs.borrow(), |builder_programs| &builder_programs[builder_program])
    }

    fn alloc_builder_program(&self, builder_program: Box<dyn BuilderProgram>) -> Id<Box<dyn BuilderProgram>> {
        let id = self.builder_programs.borrow_mut().alloc(builder_program);
        id
    }

    #[track_caller]
    fn type_reference_directive_resolution_cache(&self, type_reference_directive_resolution_cache: Id<TypeReferenceDirectiveResolutionCache>) -> Ref<TypeReferenceDirectiveResolutionCache> {
        Ref::map(self.type_reference_directive_resolution_caches.borrow(), |type_reference_directive_resolution_caches| &type_reference_directive_resolution_caches[type_reference_directive_resolution_cache])
    }

    fn alloc_type_reference_directive_resolution_cache(&self, type_reference_directive_resolution_cache: TypeReferenceDirectiveResolutionCache) -> Id<TypeReferenceDirectiveResolutionCache> {
        let id = self.type_reference_directive_resolution_caches.borrow_mut().alloc(type_reference_directive_resolution_cache);
        id
    }

    #[track_caller]
    fn module_resolution_cache(&self, module_resolution_cache: Id<ModuleResolutionCache>) -> Ref<ModuleResolutionCache> {
        Ref::map(self.module_resolution_caches.borrow(), |module_resolution_caches| &module_resolution_caches[module_resolution_cache])
    }

    fn alloc_module_resolution_cache(&self, module_resolution_cache: ModuleResolutionCache) -> Id<ModuleResolutionCache> {
        let id = self.module_resolution_caches.borrow_mut().alloc(module_resolution_cache);
        id
    }

    #[track_caller]
    fn parse_config_file_host(&self, parse_config_file_host: Id<Box<dyn ParseConfigFileHost>>) -> Ref<Box<dyn ParseConfigFileHost>> {
        Ref::map(self.parse_config_file_hosts.borrow(), |parse_config_file_hosts| &parse_config_file_hosts[parse_config_file_host])
    }

    fn alloc_parse_config_file_host(&self, parse_config_file_host: Box<dyn ParseConfigFileHost>) -> Id<Box<dyn ParseConfigFileHost>> {
        let id = self.parse_config_file_hosts.borrow_mut().alloc(parse_config_file_host);
        id
    }

    #[track_caller]
    fn file_preprocessing_diagnostics(&self, file_preprocessing_diagnostics: Id<FilePreprocessingDiagnostics>) -> Ref<FilePreprocessingDiagnostics> {
        Ref::map(self.file_preprocessing_diagnostics.borrow(), |file_preprocessing_diagnostics_| &file_preprocessing_diagnostics_[file_preprocessing_diagnostics])
    }

    fn alloc_file_preprocessing_diagnostics(&self, file_preprocessing_diagnostics: FilePreprocessingDiagnostics) -> Id<FilePreprocessingDiagnostics> {
        let id = self.file_preprocessing_diagnostics.borrow_mut().alloc(file_preprocessing_diagnostics);
        id
    }

    #[track_caller]
    fn actual_resolve_module_names_worker(&self, actual_resolve_module_names_worker: Id<Box<dyn ActualResolveModuleNamesWorker>>) -> Ref<Box<dyn ActualResolveModuleNamesWorker>> {
        Ref::map(self.actual_resolve_module_names_workers.borrow(), |actual_resolve_module_names_workers| &actual_resolve_module_names_workers[actual_resolve_module_names_worker])
    }

    fn alloc_actual_resolve_module_names_worker(&self, actual_resolve_module_names_worker: Box<dyn ActualResolveModuleNamesWorker>) -> Id<Box<dyn ActualResolveModuleNamesWorker>> {
        let id = self.actual_resolve_module_names_workers.borrow_mut().alloc(actual_resolve_module_names_worker);
        id
    }

    #[track_caller]
    fn actual_resolve_type_reference_directive_names_worker(&self, actual_resolve_type_reference_directive_names_worker: Id<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>>) -> Ref<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>> {
        Ref::map(self.actual_resolve_type_reference_directive_names_workers.borrow(), |actual_resolve_type_reference_directive_names_workers| &actual_resolve_type_reference_directive_names_workers[actual_resolve_type_reference_directive_names_worker])
    }

    fn alloc_actual_resolve_type_reference_directive_names_worker(&self, actual_resolve_type_reference_directive_names_worker: Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>) -> Id<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>> {
        let id = self.actual_resolve_type_reference_directive_names_workers.borrow_mut().alloc(actual_resolve_type_reference_directive_names_worker);
        id
    }

    #[track_caller]
    fn get_program_build_info_ref(&self, get_program_build_info: Id<Box<dyn GetProgramBuildInfo>>) -> Ref<Box<dyn GetProgramBuildInfo>> {
        Ref::map(self.get_program_build_infos.borrow(), |get_program_build_infos| &get_program_build_infos[get_program_build_info])
    }

    fn alloc_get_program_build_info(&self, get_program_build_info: Box<dyn GetProgramBuildInfo>) -> Id<Box<dyn GetProgramBuildInfo>> {
        let id = self.get_program_build_infos.borrow_mut().alloc(get_program_build_info);
        id
    }

    #[track_caller]
    fn load_with_mode_aware_cache_loader(&self, load_with_mode_aware_cache_loader: Id<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>>) -> Ref<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>> {
        Ref::map(self.load_with_mode_aware_cache_loaders.borrow(), |load_with_mode_aware_cache_loaders| &load_with_mode_aware_cache_loaders[load_with_mode_aware_cache_loader])
    }

    fn alloc_load_with_mode_aware_cache_loader(&self, load_with_mode_aware_cache_loader: Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>) -> Id<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>> {
        let id = self.load_with_mode_aware_cache_loaders.borrow_mut().alloc(load_with_mode_aware_cache_loader);
        id
    }

    #[track_caller]
    fn load_with_local_cache_loader(&self, load_with_local_cache_loader: Id<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>>) -> Ref<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>> {
        Ref::map(self.load_with_local_cache_loaders.borrow(), |load_with_local_cache_loaders| &load_with_local_cache_loaders[load_with_local_cache_loader])
    }

    fn alloc_load_with_local_cache_loader(&self, load_with_local_cache_loader: Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>) -> Id<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>> {
        let id = self.load_with_local_cache_loaders.borrow_mut().alloc(load_with_local_cache_loader);
        id
    }

    #[track_caller]
    fn symbol_accessibility_diagnostic(&self, symbol_accessibility_diagnostic: Id<SymbolAccessibilityDiagnostic>) -> Ref<SymbolAccessibilityDiagnostic> {
        Ref::map(self.symbol_accessibility_diagnostics.borrow(), |symbol_accessibility_diagnostics| &symbol_accessibility_diagnostics[symbol_accessibility_diagnostic])
    }

    fn alloc_symbol_accessibility_diagnostic(&self, symbol_accessibility_diagnostic: SymbolAccessibilityDiagnostic) -> Id<SymbolAccessibilityDiagnostic> {
        let id = self.symbol_accessibility_diagnostics.borrow_mut().alloc(symbol_accessibility_diagnostic);
        id
    }

    #[track_caller]
    fn code_block(&self, code_block: Id<CodeBlock>) -> Ref<CodeBlock> {
        Ref::map(self.code_blocks.borrow(), |code_blocks| &code_blocks[code_block])
    }

    fn code_block_mut(&self, code_block: Id<CodeBlock>) -> RefMut<CodeBlock> {
        RefMut::map(self.code_blocks.borrow_mut(), |code_blocks| &mut code_blocks[code_block])
    }

    fn alloc_code_block(&self, code_block: CodeBlock) -> Id<CodeBlock> {
        let id = self.code_blocks.borrow_mut().alloc(code_block);
        id
    }

    #[track_caller]
    fn private_identifier_environment(&self, private_identifier_environment: Id<PrivateIdentifierEnvironment>) -> Ref<PrivateIdentifierEnvironment> {
        Ref::map(self.private_identifier_environments.borrow(), |private_identifier_environments| &private_identifier_environments[private_identifier_environment])
    }

    fn private_identifier_environment_mut(&self, private_identifier_environment: Id<PrivateIdentifierEnvironment>) -> RefMut<PrivateIdentifierEnvironment> {
        RefMut::map(self.private_identifier_environments.borrow_mut(), |private_identifier_environments| &mut private_identifier_environments[private_identifier_environment])
    }

    fn alloc_private_identifier_environment(&self, private_identifier_environment: PrivateIdentifierEnvironment) -> Id<PrivateIdentifierEnvironment> {
        let id = self.private_identifier_environments.borrow_mut().alloc(private_identifier_environment);
        id
    }

    #[track_caller]
    fn private_identifier_info(&self, private_identifier_info: Id<PrivateIdentifierInfo>) -> Ref<PrivateIdentifierInfo> {
        Ref::map(self.private_identifier_infos.borrow(), |private_identifier_infos| &private_identifier_infos[private_identifier_info])
    }

    fn private_identifier_info_mut(&self, private_identifier_info: Id<PrivateIdentifierInfo>) -> RefMut<PrivateIdentifierInfo> {
        RefMut::map(self.private_identifier_infos.borrow_mut(), |private_identifier_infos| &mut private_identifier_infos[private_identifier_info])
    }

    fn alloc_private_identifier_info(&self, private_identifier_info: PrivateIdentifierInfo) -> Id<PrivateIdentifierInfo> {
        let id = self.private_identifier_infos.borrow_mut().alloc(private_identifier_info);
        id
    }

    #[track_caller]
    fn external_module_info(&self, external_module_info: Id<ExternalModuleInfo>) -> Ref<ExternalModuleInfo> {
        Ref::map(self.external_module_infos.borrow(), |external_module_infos| &external_module_infos[external_module_info])
    }

    fn alloc_external_module_info(&self, external_module_info: ExternalModuleInfo) -> Id<ExternalModuleInfo> {
        let id = self.external_module_infos.borrow_mut().alloc(external_module_info);
        id
    }

    #[track_caller]
    fn resolved_module_with_failed_lookup_locations(&self, resolved_module_with_failed_lookup_locations: Id<ResolvedModuleWithFailedLookupLocations>) -> Ref<ResolvedModuleWithFailedLookupLocations> {
        Ref::map(self.resolved_modules_with_failed_lookup_locations.borrow(), |resolved_modules_with_failed_lookup_locations| &resolved_modules_with_failed_lookup_locations[resolved_module_with_failed_lookup_locations])
    }

    fn alloc_resolved_module_with_failed_lookup_locations(&self, resolved_module_with_failed_lookup_locations: ResolvedModuleWithFailedLookupLocations) -> Id<ResolvedModuleWithFailedLookupLocations> {
        let id = self.resolved_modules_with_failed_lookup_locations.borrow_mut().alloc(resolved_module_with_failed_lookup_locations);
        id
    }

    #[track_caller]
    fn resolved_type_reference_directive_with_failed_lookup_locations(&self, resolved_type_reference_directive_with_failed_lookup_locations: Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>) -> Ref<ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
        Ref::map(self.resolved_type_reference_directives_with_failed_lookup_locations.borrow(), |resolved_type_reference_directives_with_failed_lookup_locations| &resolved_type_reference_directives_with_failed_lookup_locations[resolved_type_reference_directive_with_failed_lookup_locations])
    }

    fn alloc_resolved_type_reference_directive_with_failed_lookup_locations(&self, resolved_type_reference_directive_with_failed_lookup_locations: ResolvedTypeReferenceDirectiveWithFailedLookupLocations) -> Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
        let id = self.resolved_type_reference_directives_with_failed_lookup_locations.borrow_mut().alloc(resolved_type_reference_directive_with_failed_lookup_locations);
        id
    }

    #[track_caller]
    fn package_json_info_cache(&self, package_json_info_cache: Id<Box<dyn PackageJsonInfoCache>>) -> Ref<Box<dyn PackageJsonInfoCache>> {
        Ref::map(self.package_json_info_caches.borrow(), |package_json_info_caches| &package_json_info_caches[package_json_info_cache])
    }

    fn alloc_package_json_info_cache(&self, package_json_info_cache: Box<dyn PackageJsonInfoCache>) -> Id<Box<dyn PackageJsonInfoCache>> {
        let id = self.package_json_info_caches.borrow_mut().alloc(package_json_info_cache);
        id
    }

    #[track_caller]
    fn mode_aware_cache_resolved_module_with_failed_lookup_locations(&self, mode_aware_cache_resolved_module_with_failed_lookup_locations: Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>) -> Ref<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>> {
        Ref::map(self.mode_aware_cache_resolved_module_with_failed_lookup_locations.borrow(), |mode_aware_cache_resolved_module_with_failed_lookup_locations_| &mode_aware_cache_resolved_module_with_failed_lookup_locations_[mode_aware_cache_resolved_module_with_failed_lookup_locations])
    }

    fn alloc_mode_aware_cache_resolved_module_with_failed_lookup_locations(&self, mode_aware_cache_resolved_module_with_failed_lookup_locations: ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>) -> Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>> {
        let id = self.mode_aware_cache_resolved_module_with_failed_lookup_locations.borrow_mut().alloc(mode_aware_cache_resolved_module_with_failed_lookup_locations);
        id
    }

    #[track_caller]
    fn mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(&self, mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations: Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>) -> Ref<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>> {
        Ref::map(self.mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations.borrow(), |mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_| &mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_[mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations])
    }

    fn alloc_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(&self, mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations: ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>) -> Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>> {
        let id = self.mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations.borrow_mut().alloc(mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations);
        id
    }

    #[track_caller]
    fn per_module_name_cache(&self, per_module_name_cache: Id<PerModuleNameCache>) -> Ref<PerModuleNameCache> {
        Ref::map(self.per_module_name_caches.borrow(), |per_module_name_caches| &per_module_name_caches[per_module_name_cache])
    }

    fn alloc_per_module_name_cache(&self, per_module_name_cache: PerModuleNameCache) -> Id<PerModuleNameCache> {
        let id = self.per_module_name_caches.borrow_mut().alloc(per_module_name_cache);
        id
    }

    #[track_caller]
    fn vec_diagnostic(&self, vec_diagnostic: Id<Vec<Id<Diagnostic>>>) -> Ref<Vec<Id<Diagnostic>>> {
        Ref::map(self.vec_diagnostics.borrow(), |vec_diagnostics| &vec_diagnostics[vec_diagnostic])
    }

    fn vec_diagnostic_mut(&self, vec_diagnostic: Id<Vec<Id<Diagnostic>>>) -> RefMut<Vec<Id<Diagnostic>>> {
        RefMut::map(self.vec_diagnostics.borrow_mut(), |vec_diagnostics| &mut vec_diagnostics[vec_diagnostic])
    }

    fn alloc_vec_diagnostic(&self, vec_diagnostic: Vec<Id<Diagnostic>>) -> Id<Vec<Id<Diagnostic>>> {
        let id = self.vec_diagnostics.borrow_mut().alloc(vec_diagnostic);
        id
    }

    #[track_caller]
    fn file_reasons(&self, file_reasons: Id<MultiMap<Path, Id<FileIncludeReason>>>) -> Ref<MultiMap<Path, Id<FileIncludeReason>>> {
        Ref::map(self.file_reasons.borrow(), |file_reasons_| &file_reasons_[file_reasons])
    }

    fn file_reasons_mut(&self, file_reasons: Id<MultiMap<Path, Id<FileIncludeReason>>>) -> RefMut<MultiMap<Path, Id<FileIncludeReason>>> {
        RefMut::map(self.file_reasons.borrow_mut(), |file_reasons_| &mut file_reasons_[file_reasons])
    }

    fn alloc_file_reasons(&self, file_reasons: MultiMap<Path, Id<FileIncludeReason>>) -> Id<MultiMap<Path, Id<FileIncludeReason>>> {
        let id = self.file_reasons.borrow_mut().alloc(file_reasons);
        id
    }

    #[track_caller]
    fn get_symbol_accessibility_diagnostic_interface(&self, get_symbol_accessibility_diagnostic_interface: Id<Box<dyn GetSymbolAccessibilityDiagnosticInterface>>) -> Ref<Box<dyn GetSymbolAccessibilityDiagnosticInterface>> {
        Ref::map(self.get_symbol_accessibility_diagnostic_interfaces.borrow(), |get_symbol_accessibility_diagnostic_interfaces| &get_symbol_accessibility_diagnostic_interfaces[get_symbol_accessibility_diagnostic_interface])
    }

    fn alloc_get_symbol_accessibility_diagnostic_interface(&self, get_symbol_accessibility_diagnostic_interface: Box<dyn GetSymbolAccessibilityDiagnosticInterface>) -> Id<Box<dyn GetSymbolAccessibilityDiagnosticInterface>> {
        let id = self.get_symbol_accessibility_diagnostic_interfaces.borrow_mut().alloc(get_symbol_accessibility_diagnostic_interface);
        id
    }

    #[track_caller]
    fn option_vec_node(&self, option_vec_node: Id<Option<Vec<Id<Node>>>>) -> Ref<Option<Vec<Id<Node>>>> {
        Ref::map(self.option_vec_nodes.borrow(), |option_vec_nodes| &option_vec_nodes[option_vec_node])
    }

    fn option_vec_node_mut(&self, option_vec_node: Id<Option<Vec<Id<Node>>>>) -> RefMut<Option<Vec<Id<Node>>>> {
        RefMut::map(self.option_vec_nodes.borrow_mut(), |option_vec_nodes| &mut option_vec_nodes[option_vec_node])
    }

    fn alloc_option_vec_node(&self, option_vec_node: Option<Vec<Id<Node>>>) -> Id<Option<Vec<Id<Node>>>> {
        let id = self.option_vec_nodes.borrow_mut().alloc(option_vec_node);
        id
    }

    #[track_caller]
    fn vec_pending_declaration(&self, vec_pending_declaration: Id<Vec<PendingDeclaration>>) -> Ref<Vec<PendingDeclaration>> {
        Ref::map(self.vec_pending_declarations.borrow(), |vec_pending_declarations| &vec_pending_declarations[vec_pending_declaration])
    }

    fn vec_pending_declaration_mut(&self, vec_pending_declaration: Id<Vec<PendingDeclaration>>) -> RefMut<Vec<PendingDeclaration>> {
        RefMut::map(self.vec_pending_declarations.borrow_mut(), |vec_pending_declarations| &mut vec_pending_declarations[vec_pending_declaration])
    }

    fn alloc_vec_pending_declaration(&self, vec_pending_declaration: Vec<PendingDeclaration>) -> Id<Vec<PendingDeclaration>> {
        let id = self.vec_pending_declarations.borrow_mut().alloc(vec_pending_declaration);
        id
    }

    #[track_caller]
    fn package_json_info(&self, package_json_info: Id<PackageJsonInfo>) -> Ref<PackageJsonInfo> {
        Ref::map(self.package_json_infos.borrow(), |package_json_infos| &package_json_infos[package_json_info])
    }

    fn alloc_package_json_info(&self, package_json_info: PackageJsonInfo) -> Id<PackageJsonInfo> {
        let id = self.package_json_infos.borrow_mut().alloc(package_json_info);
        id
    }

    #[track_caller]
    fn vec_type(&self, vec_type: Id<Vec<Id<Type>>>) -> Ref<Vec<Id<Type>>> {
        Ref::map(self.vec_types.borrow(), |vec_types| &vec_types[vec_type])
    }

    fn alloc_vec_type(&self, vec_type: Vec<Id<Type>>) -> Id<Vec<Id<Type>>> {
        let id = self.vec_types.borrow_mut().alloc(vec_type);
        id
    }

    #[track_caller]
    fn pattern_ambient_module(&self, pattern_ambient_module: Id<PatternAmbientModule>) -> Ref<PatternAmbientModule> {
        Ref::map(self.pattern_ambient_modules.borrow(), |pattern_ambient_modules| &pattern_ambient_modules[pattern_ambient_module])
    }

    fn alloc_pattern_ambient_module(&self, pattern_ambient_module: PatternAmbientModule) -> Id<PatternAmbientModule> {
        let id = self.pattern_ambient_modules.borrow_mut().alloc(pattern_ambient_module);
        id
    }

    #[track_caller]
    fn check_type_containing_message_chain(&self, check_type_containing_message_chain: Id<Box<dyn CheckTypeContainingMessageChain>>) -> Ref<Box<dyn CheckTypeContainingMessageChain>> {
        Ref::map(self.check_type_containing_message_chains.borrow(), |check_type_containing_message_chains| &check_type_containing_message_chains[check_type_containing_message_chain])
    }

    fn alloc_check_type_containing_message_chain(&self, check_type_containing_message_chain: Box<dyn CheckTypeContainingMessageChain>) -> Id<Box<dyn CheckTypeContainingMessageChain>> {
        let id = self.check_type_containing_message_chains.borrow_mut().alloc(check_type_containing_message_chain);
        id
    }

    #[track_caller]
    fn check_type_error_output_container(&self, check_type_error_output_container: Id<Box<dyn CheckTypeErrorOutputContainer>>) -> Ref<Box<dyn CheckTypeErrorOutputContainer>> {
        Ref::map(self.check_type_error_output_containers.borrow(), |check_type_error_output_containers| &check_type_error_output_containers[check_type_error_output_container])
    }

    fn alloc_check_type_error_output_container(&self, check_type_error_output_container: Box<dyn CheckTypeErrorOutputContainer>) -> Id<Box<dyn CheckTypeErrorOutputContainer>> {
        let id = self.check_type_error_output_containers.borrow_mut().alloc(check_type_error_output_container);
        id
    }

    #[track_caller]
    fn resolved_type_reference_directives_map(&self, resolved_type_reference_directives_map: Id<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>>) -> Ref<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        Ref::map(self.resolved_type_reference_directives_maps.borrow(), |resolved_type_reference_directives_maps| &resolved_type_reference_directives_maps[resolved_type_reference_directives_map])
    }

    fn resolved_type_reference_directives_map_mut(&self, resolved_type_reference_directives_map: Id<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>>) -> RefMut<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        RefMut::map(self.resolved_type_reference_directives_maps.borrow_mut(), |resolved_type_reference_directives_maps| &mut resolved_type_reference_directives_maps[resolved_type_reference_directives_map])
    }

    fn alloc_resolved_type_reference_directives_map(&self, resolved_type_reference_directives_map: HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>) -> Id<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        let id = self.resolved_type_reference_directives_maps.borrow_mut().alloc(resolved_type_reference_directives_map);
        id
    }

    #[track_caller]
    fn node_builder(&self, node_builder: Id<NodeBuilder>) -> Ref<NodeBuilder> {
        Ref::map(self.node_builders.borrow(), |node_builders| &node_builders[node_builder])
    }

    fn alloc_node_builder(&self, node_builder: NodeBuilder) -> Id<NodeBuilder> {
        let id = self.node_builders.borrow_mut().alloc(node_builder);
        id.ref_(self).set_arena_id(id);
        id
    }

    #[track_caller]
    fn node_builder_context(&self, node_builder_context: Id<NodeBuilderContext>) -> Ref<NodeBuilderContext> {
        Ref::map(self.node_builder_contexts.borrow(), |node_builder_contexts| &node_builder_contexts[node_builder_context])
    }

    fn alloc_node_builder_context(&self, node_builder_context: NodeBuilderContext) -> Id<NodeBuilderContext> {
        let id = self.node_builder_contexts.borrow_mut().alloc(node_builder_context);
        id.ref_(self).set_arena_id(id);
        id
    }

    #[track_caller]
    fn option_vec_type(&self, option_vec_type: Id<Option<Vec<Id<Type>>>>) -> Ref<Option<Vec<Id<Type>>>> {
        Ref::map(self.option_vec_types.borrow(), |option_vec_types| &option_vec_types[option_vec_type])
    }

    fn option_vec_type_mut(&self, option_vec_type: Id<Option<Vec<Id<Type>>>>) -> RefMut<Option<Vec<Id<Type>>>> {
        RefMut::map(self.option_vec_types.borrow_mut(), |option_vec_types| &mut option_vec_types[option_vec_type])
    }

    fn alloc_option_vec_type(&self, option_vec_type: Option<Vec<Id<Type>>>) -> Id<Option<Vec<Id<Type>>>> {
        let id = self.option_vec_types.borrow_mut().alloc(option_vec_type);
        id
    }

    #[track_caller]
    fn option_type_parameter_names(&self, option_type_parameter_names: Id<Option<HashMap<TypeId, Id<Node>>>>) -> Ref<Option<HashMap<TypeId, Id<Node>>>> {
        Ref::map(self.option_type_parameter_names.borrow(), |option_type_parameter_names_| &option_type_parameter_names_[option_type_parameter_names])
    }

    fn option_type_parameter_names_mut(&self, option_type_parameter_names: Id<Option<HashMap<TypeId, Id<Node>>>>) -> RefMut<Option<HashMap<TypeId, Id<Node>>>> {
        RefMut::map(self.option_type_parameter_names.borrow_mut(), |option_type_parameter_names_| &mut option_type_parameter_names_[option_type_parameter_names])
    }

    fn alloc_option_type_parameter_names(&self, option_type_parameter_names: Option<HashMap<TypeId, Id<Node>>>) -> Id<Option<HashMap<TypeId, Id<Node>>>> {
        let id = self.option_type_parameter_names.borrow_mut().alloc(option_type_parameter_names);
        id
    }

    #[track_caller]
    fn option_vec_symbol(&self, option_vec_symbol: Id<Option<Vec<Id<Symbol>>>>) -> Ref<Option<Vec<Id<Symbol>>>> {
        Ref::map(self.option_vec_symbols.borrow(), |option_vec_symbols| &option_vec_symbols[option_vec_symbol])
    }

    fn option_vec_symbol_mut(&self, option_vec_symbol: Id<Option<Vec<Id<Symbol>>>>) -> RefMut<Option<Vec<Id<Symbol>>>> {
        RefMut::map(self.option_vec_symbols.borrow_mut(), |option_vec_symbols| &mut option_vec_symbols[option_vec_symbol])
    }

    fn alloc_option_vec_symbol(&self, option_vec_symbol: Option<Vec<Id<Symbol>>>) -> Id<Option<Vec<Id<Symbol>>>> {
        let id = self.option_vec_symbols.borrow_mut().alloc(option_vec_symbol);
        id
    }

    #[track_caller]
    fn type_comparer(&self, type_comparer: Id<Box<dyn TypeComparer>>) -> Ref<Box<dyn TypeComparer>> {
        Ref::map(self.type_comparers.borrow(), |type_comparers| &type_comparers[type_comparer])
    }

    fn alloc_type_comparer(&self, type_comparer: Box<dyn TypeComparer>) -> Id<Box<dyn TypeComparer>> {
        let id = self.type_comparers.borrow_mut().alloc(type_comparer);
        id
    }

    #[track_caller]
    fn inference_context(&self, inference_context: Id<InferenceContext>) -> Ref<InferenceContext> {
        Ref::map(self.inference_contexts.borrow(), |inference_contexts| &inference_contexts[inference_context])
    }

    fn alloc_inference_context(&self, inference_context: InferenceContext) -> Id<InferenceContext> {
        let id = self.inference_contexts.borrow_mut().alloc(inference_context);
        id
    }

    #[track_caller]
    fn skip_trivia(&self, skip_trivia: Id<Box<dyn SkipTrivia>>) -> Ref<Box<dyn SkipTrivia>> {
        Ref::map(self.skip_trivias.borrow(), |skip_trivias| &skip_trivias[skip_trivia])
    }

    fn alloc_skip_trivia(&self, skip_trivia: Box<dyn SkipTrivia>) -> Id<Box<dyn SkipTrivia>> {
        let id = self.skip_trivias.borrow_mut().alloc(skip_trivia);
        id
    }

    #[track_caller]
    fn custom_transformer_factory_interface(&self, custom_transformer_factory_interface: Id<Box<dyn CustomTransformerFactoryInterface>>) -> Ref<Box<dyn CustomTransformerFactoryInterface>> {
        Ref::map(self.custom_transformer_factory_interfaces.borrow(), |custom_transformer_factory_interfaces| &custom_transformer_factory_interfaces[custom_transformer_factory_interface])
    }

    fn alloc_custom_transformer_factory_interface(&self, custom_transformer_factory_interface: Box<dyn CustomTransformerFactoryInterface>) -> Id<Box<dyn CustomTransformerFactoryInterface>> {
        let id = self.custom_transformer_factory_interfaces.borrow_mut().alloc(custom_transformer_factory_interface);
        id
    }
}

pub trait InArena {
    type Item: ?Sized;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Self::Item>;
    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, Self::Item> {
        // TODO: this is mad janky but I think this method might go away?
        unimplemented!()
    }
}

impl InArena for Id<Node> {
    type Item = Node;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Node> {
        has_arena.node(*self)
    }
}

impl InArena for Id<Type> {
    type Item = Type;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Type> {
        has_arena.type_(*self)
    }
}

impl InArena for Id<TypeMapper> {
    type Item = TypeMapper;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, TypeMapper> {
        has_arena.type_mapper(*self)
    }
}

impl InArena for Id<Symbol> {
    type Item = Symbol;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Symbol> {
        has_arena.symbol(*self)
    }
}

impl InArena for Id<TransformNodesTransformationResult> {
    type Item = TransformNodesTransformationResult;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, TransformNodesTransformationResult> {
        has_arena.transform_nodes_transformation_result(*self)
    }
}

impl InArena for Id<Box<dyn TransformerInterface>> {
    type Item = Box<dyn TransformerInterface>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn TransformerInterface>> {
        has_arena.transformer(*self)
    }
}

impl InArena for Id<Box<dyn TransformerFactoryInterface>> {
    type Item = Box<dyn TransformerFactoryInterface>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn TransformerFactoryInterface>> {
        has_arena.transformer_factory(*self)
    }
}

impl InArena for Id<Box<dyn EmitTextWriter>> {
    type Item = Box<dyn EmitTextWriter>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn EmitTextWriter>> {
        has_arena.emit_text_writer(*self)
    }
}

impl InArena for Id<Box<dyn SymbolTracker>> {
    type Item = Box<dyn SymbolTracker>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn SymbolTracker>> {
        has_arena.symbol_tracker(*self)
    }
}

impl InArena for Id<Box<dyn EmitHost>> {
    type Item = Box<dyn EmitHost>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn EmitHost>> {
        has_arena.emit_host(*self)
    }
}

impl InArena for Id<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>> {
    type Item = Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>> {
        has_arena.module_specifier_resolution_host_and_get_common_source_directory(*self)
    }
}

impl InArena for Id<FileIncludeReason> {
    type Item = FileIncludeReason;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, FileIncludeReason> {
        has_arena.file_include_reason(*self)
    }
}

impl InArena for Id<Box<dyn System>> {
    type Item = Box<dyn System>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn System>> {
        has_arena.system(*self)
    }
}

impl InArena for Id<SourceMapRange> {
    type Item = SourceMapRange;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, SourceMapRange> {
        has_arena.source_map_range(*self)
    }
}

impl InArena for Id<EmitHelper> {
    type Item = EmitHelper;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, EmitHelper> {
        has_arena.emit_helper(*self)
    }
}

impl InArena for Id<CompilerOptions> {
    type Item = CompilerOptions;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, CompilerOptions> {
        has_arena.compiler_options(*self)
    }
}

impl InArena for Id<FlowNode> {
    type Item = FlowNode;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, FlowNode> {
        has_arena.flow_node(*self)
    }
}

impl InArena for Id<Diagnostic> {
    type Item = Diagnostic;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Diagnostic> {
        has_arena.diagnostic(*self)
    }
}

impl InArena for Id<Program> {
    type Item = Program;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Program> {
        has_arena.program(*self)
    }
}

impl InArena for Id<Signature> {
    type Item = Signature;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Signature> {
        has_arena.signature(*self)
    }
}

impl InArena for Id<Box<dyn DiagnosticReporter>> {
    type Item = Box<dyn DiagnosticReporter>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn DiagnosticReporter>> {
        has_arena.diagnostic_reporter(*self)
    }
}

impl InArena for Id<NodeFactory> {
    type Item = NodeFactory;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, NodeFactory> {
        has_arena.node_factory(*self)
    }
}

impl InArena for Id<Box<dyn BaseNodeFactory>> {
    type Item = Box<dyn BaseNodeFactory>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn BaseNodeFactory>> {
        has_arena.base_node_factory(*self)
    }
}

impl InArena for Id<Box<dyn EmitResolver>> {
    type Item = Box<dyn EmitResolver>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn EmitResolver>> {
        has_arena.emit_resolver(*self)
    }
}

impl InArena for Id<ResolvedTypeReferenceDirective> {
    type Item = ResolvedTypeReferenceDirective;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ResolvedTypeReferenceDirective> {
        has_arena.resolved_type_reference_directive(*self)
    }
}

impl InArena for Id<Box<dyn CompilerHost>> {
    type Item = Box<dyn CompilerHost>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn CompilerHost>> {
        has_arena.compiler_host(*self)
    }
}

impl InArena for Id<GcCell<SymbolLinks>> {
    type Item = GcCell<SymbolLinks>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, GcCell<SymbolLinks>> {
        has_arena.symbol_links(*self)
    }
}

impl InArena for Id<Printer> {
    type Item = Printer;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Printer> {
        has_arena.printer(*self)
    }
}

impl InArena for Id<DiagnosticRelatedInformation> {
    type Item = DiagnosticRelatedInformation;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, DiagnosticRelatedInformation> {
        has_arena.diagnostic_related_information(*self)
    }
}

impl InArena for Id<IndexInfo> {
    type Item = IndexInfo;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, IndexInfo> {
        has_arena.index_info(*self)
    }
}

impl InArena for Id<Box<dyn CurrentParenthesizerRule>> {
    type Item = Box<dyn CurrentParenthesizerRule>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn CurrentParenthesizerRule>> {
        has_arena.current_parenthesizer_rule(*self)
    }
}

impl InArena for Id<Box<dyn ParenthesizerRules>> {
    type Item = Box<dyn ParenthesizerRules>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn ParenthesizerRules>> {
        has_arena.parenthesizer_rules(*self)
    }
}

impl InArena for Id<IterationTypes> {
    type Item = IterationTypes;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, IterationTypes> {
        has_arena.iteration_types(*self)
    }
}

impl InArena for Id<TypePredicate> {
    type Item = TypePredicate;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, TypePredicate> {
        has_arena.type_predicate(*self)
    }
}

impl InArena for Id<ActiveLabel> {
    type Item = ActiveLabel;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ActiveLabel> {
        has_arena.active_label(*self)
    }
}

impl InArena for Id<Box<dyn ToPath>> {
    type Item = Box<dyn ToPath>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn ToPath>> {
        has_arena.to_path(*self)
    }
}

impl InArena for Id<Box<dyn ModuleResolutionHostOverrider>> {
    type Item = Box<dyn ModuleResolutionHostOverrider>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn ModuleResolutionHostOverrider>> {
        has_arena.module_resolution_host_overrider(*self)
    }
}

impl InArena for Id<Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
    type Item = Box<dyn WrapCustomTransformerFactoryHandleDefault>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
        has_arena.wrap_custom_transformer_factory_handle_default(*self)
    }
}

impl InArena for Id<Box<dyn TransformationContextOnEmitNodeOverrider>> {
    type Item = Box<dyn TransformationContextOnEmitNodeOverrider>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn TransformationContextOnEmitNodeOverrider>> {
        has_arena.transformation_context_on_emit_node_overrider(*self)
    }
}

impl InArena for Id<Box<dyn SourceMapGenerator>> {
    type Item = Box<dyn SourceMapGenerator>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn SourceMapGenerator>> {
        has_arena.source_map_generator(*self)
    }
}

impl InArena for Id<Box<dyn GetCanonicalFileName>> {
    type Item = Box<dyn GetCanonicalFileName>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn GetCanonicalFileName>> {
        has_arena.get_canonical_file_name(*self)
    }
}

impl InArena for Id<EmitHelperFactory> {
    type Item = EmitHelperFactory;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, EmitHelperFactory> {
        has_arena.emit_helper_factory(*self)
    }
}

impl InArena for Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>> {
    type Item = Box<dyn TransformationContextOnSubstituteNodeOverrider>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn TransformationContextOnSubstituteNodeOverrider>> {
        has_arena.transformation_context_on_substitute_node_overrider(*self)
    }
}

impl InArena for Id<ParsedCommandLine> {
    type Item = ParsedCommandLine;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ParsedCommandLine> {
        has_arena.parsed_command_line(*self)
    }
}

impl InArena for Id<Box<dyn CancellationToken>> {
    type Item = Box<dyn CancellationToken>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn CancellationToken>> {
        has_arena.cancellation_token(*self)
    }
}

impl InArena for Id<ResolvedProjectReference> {
    type Item = ResolvedProjectReference;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ResolvedProjectReference> {
        has_arena.resolved_project_reference(*self)
    }
}

impl InArena for Id<TransformerFactoryOrCustomTransformerFactory> {
    type Item = TransformerFactoryOrCustomTransformerFactory;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, TransformerFactoryOrCustomTransformerFactory> {
        has_arena.transformer_factory_or_custom_transformer_factory(*self)
    }
}

impl InArena for Id<SymlinkCache> {
    type Item = SymlinkCache;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, SymlinkCache> {
        has_arena.symlink_cache(*self)
    }
}

impl InArena for Id<Box<dyn WriteFileCallback>> {
    type Item = Box<dyn WriteFileCallback>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn WriteFileCallback>> {
        has_arena.write_file_callback(*self)
    }
}

impl InArena for Id<ResolvedModuleFull> {
    type Item = ResolvedModuleFull;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ResolvedModuleFull> {
        has_arena.resolved_module_full(*self)
    }
}

impl InArena for Id<NodeArray> {
    type Item = NodeArray;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, NodeArray> {
        has_arena.node_array(*self)
    }
}

impl InArena for Id<BundleFileSection> {
    type Item = BundleFileSection;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, BundleFileSection> {
        has_arena.bundle_file_section(*self)
    }
}

impl InArena for Id<BuildInfo> {
    type Item = BuildInfo;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, BuildInfo> {
        has_arena.build_info(*self)
    }
}

impl InArena for Id<ProgramBuildInfo> {
    type Item = ProgramBuildInfo;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ProgramBuildInfo> {
        has_arena.program_build_info(*self)
    }
}

impl InArena for Id<BundleBuildInfo> {
    type Item = BundleBuildInfo;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, BundleBuildInfo> {
        has_arena.bundle_build_info(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, BundleBuildInfo> {
        has_arena.bundle_build_info_mut(*self)
    }
}

impl InArena for Id<BundleFileInfo> {
    type Item = BundleFileInfo;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, BundleFileInfo> {
        has_arena.bundle_file_info(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, BundleFileInfo> {
        has_arena.bundle_file_info_mut(*self)
    }
}

impl InArena for Id<SymbolTable> {
    type Item = SymbolTable;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, SymbolTable> {
        has_arena.symbol_table(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, SymbolTable> {
        has_arena.symbol_table_mut(*self)
    }
}

impl InArena for Id<InferenceInfo> {
    type Item = InferenceInfo;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, InferenceInfo> {
        has_arena.inference_info(*self)
    }
}

impl InArena for Id<SysFormatDiagnosticsHost> {
    type Item = SysFormatDiagnosticsHost;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, SysFormatDiagnosticsHost> {
        has_arena.sys_format_diagnostics_host(*self)
    }
}

impl InArena for Id<ClassLexicalEnvironment> {
    type Item = ClassLexicalEnvironment;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ClassLexicalEnvironment> {
        has_arena.class_lexical_environment(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, ClassLexicalEnvironment> {
        has_arena.class_lexical_environment_mut(*self)
    }
}

impl InArena for Id<ConvertedLoopState> {
    type Item = ConvertedLoopState;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ConvertedLoopState> {
        has_arena.converted_loop_state(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, ConvertedLoopState> {
        has_arena.converted_loop_state_mut(*self)
    }
}

impl InArena for Id<Box<dyn EmitHelperTextCallback>> {
    type Item = Box<dyn EmitHelperTextCallback>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn EmitHelperTextCallback>> {
        has_arena.emit_helper_text_callback(*self)
    }
}

impl InArena for Id<ConditionalRoot> {
    type Item = ConditionalRoot;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ConditionalRoot> {
        has_arena.conditional_root(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, ConditionalRoot> {
        has_arena.conditional_root_mut(*self)
    }
}

impl InArena for Id<EmitNode> {
    type Item = EmitNode;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, EmitNode> {
        has_arena.emit_node(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, EmitNode> {
        has_arena.emit_node_mut(*self)
    }
}

impl InArena for Id<CheckBinaryExpression> {
    type Item = CheckBinaryExpression;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, CheckBinaryExpression> {
        has_arena.check_binary_expression(*self)
    }
}

impl InArena for Id<SourceMapSource> {
    type Item = SourceMapSource;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, SourceMapSource> {
        has_arena.source_map_source(*self)
    }
}

impl InArena for Id<Box<dyn OutofbandVarianceMarkerHandler>> {
    type Item = Box<dyn OutofbandVarianceMarkerHandler>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn OutofbandVarianceMarkerHandler>> {
        has_arena.outofband_variance_marker_handler(*self)
    }
}

impl InArena for Id<BindBinaryExpressionFlow> {
    type Item = BindBinaryExpressionFlow;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, BindBinaryExpressionFlow> {
        has_arena.bind_binary_expression_flow(*self)
    }
}

impl InArena for Id<TypeChecker> {
    type Item = TypeChecker;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, TypeChecker> {
        has_arena.type_checker(*self)
    }
}

impl InArena for Id<Box<dyn ReadFileCallback>> {
    type Item = Box<dyn ReadFileCallback>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn ReadFileCallback>> {
        has_arena.read_file_callback(*self)
    }
}

impl InArena for Id<Binder> {
    type Item = Binder;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Binder> {
        has_arena.binder(*self)
    }
}

impl InArena for Id<Box<dyn GetSourceFile>> {
    type Item = Box<dyn GetSourceFile>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn GetSourceFile>> {
        has_arena.get_source_file_ref(*self)
    }
}

impl InArena for Id<Box<dyn GetSymlinkCache>> {
    type Item = Box<dyn GetSymlinkCache>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn GetSymlinkCache>> {
        has_arena.get_symlink_cache(*self)
    }
}

impl InArena for Id<EmitBinaryExpression> {
    type Item = EmitBinaryExpression;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, EmitBinaryExpression> {
        has_arena.emit_binary_expression(*self)
    }
}

impl InArena for Id<Box<dyn RelativeToBuildInfo>> {
    type Item = Box<dyn RelativeToBuildInfo>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn RelativeToBuildInfo>> {
        has_arena.relative_to_build_info(*self)
    }
}

impl InArena for Id<Box<dyn PrintHandlers>> {
    type Item = Box<dyn PrintHandlers>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn PrintHandlers>> {
        has_arena.print_handlers(*self)
    }
}

impl InArena for Id<Box<dyn GetResolvedProjectReferences>> {
    type Item = Box<dyn GetResolvedProjectReferences>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn GetResolvedProjectReferences>> {
        has_arena.get_resolved_project_references_ref(*self)
    }
}

impl InArena for Id<Box<dyn ForEachResolvedProjectReference>> {
    type Item = Box<dyn ForEachResolvedProjectReference>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn ForEachResolvedProjectReference>> {
        has_arena.for_each_resolved_project_reference_ref(*self)
    }
}

impl InArena for Id<Box<dyn CompilerHostLike>> {
    type Item = Box<dyn CompilerHostLike>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn CompilerHostLike>> {
        has_arena.compiler_host_like(*self)
    }
}

impl InArena for Id<Box<dyn DirectoryStructureHost>> {
    type Item = Box<dyn DirectoryStructureHost>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn DirectoryStructureHost>> {
        has_arena.directory_structure_host(*self)
    }
}

impl InArena for Id<Box<dyn BuilderProgram>> {
    type Item = Box<dyn BuilderProgram>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn BuilderProgram>> {
        has_arena.builder_program(*self)
    }
}

impl InArena for Id<TypeReferenceDirectiveResolutionCache> {
    type Item = TypeReferenceDirectiveResolutionCache;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, TypeReferenceDirectiveResolutionCache> {
        has_arena.type_reference_directive_resolution_cache(*self)
    }
}

impl InArena for Id<ModuleResolutionCache> {
    type Item = ModuleResolutionCache;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ModuleResolutionCache> {
        has_arena.module_resolution_cache(*self)
    }
}

impl InArena for Id<Box<dyn ParseConfigFileHost>> {
    type Item = Box<dyn ParseConfigFileHost>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn ParseConfigFileHost>> {
        has_arena.parse_config_file_host(*self)
    }
}

impl InArena for Id<FilePreprocessingDiagnostics> {
    type Item = FilePreprocessingDiagnostics;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, FilePreprocessingDiagnostics> {
        has_arena.file_preprocessing_diagnostics(*self)
    }
}

impl InArena for Id<Box<dyn ActualResolveModuleNamesWorker>> {
    type Item = Box<dyn ActualResolveModuleNamesWorker>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn ActualResolveModuleNamesWorker>> {
        has_arena.actual_resolve_module_names_worker(*self)
    }
}

impl InArena for Id<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>> {
    type Item = Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>> {
        has_arena.actual_resolve_type_reference_directive_names_worker(*self)
    }
}

impl InArena for Id<Box<dyn GetProgramBuildInfo>> {
    type Item = Box<dyn GetProgramBuildInfo>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn GetProgramBuildInfo>> {
        has_arena.get_program_build_info_ref(*self)
    }
}

impl InArena for Id<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>> {
    type Item = Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>> {
        has_arena.load_with_mode_aware_cache_loader(*self)
    }
}

impl InArena for Id<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>> {
    type Item = Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>> {
        has_arena.load_with_local_cache_loader(*self)
    }
}

impl InArena for Id<SymbolAccessibilityDiagnostic> {
    type Item = SymbolAccessibilityDiagnostic;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, SymbolAccessibilityDiagnostic> {
        has_arena.symbol_accessibility_diagnostic(*self)
    }
}

impl InArena for Id<CodeBlock> {
    type Item = CodeBlock;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, CodeBlock> {
        has_arena.code_block(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, CodeBlock> {
        has_arena.code_block_mut(*self)
    }
}

impl InArena for Id<PrivateIdentifierEnvironment> {
    type Item = PrivateIdentifierEnvironment;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, PrivateIdentifierEnvironment> {
        has_arena.private_identifier_environment(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, PrivateIdentifierEnvironment> {
        has_arena.private_identifier_environment_mut(*self)
    }
}

impl InArena for Id<PrivateIdentifierInfo> {
    type Item = PrivateIdentifierInfo;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, PrivateIdentifierInfo> {
        has_arena.private_identifier_info(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, PrivateIdentifierInfo> {
        has_arena.private_identifier_info_mut(*self)
    }
}

impl InArena for Id<ExternalModuleInfo> {
    type Item = ExternalModuleInfo;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ExternalModuleInfo> {
        has_arena.external_module_info(*self)
    }
}

impl InArena for Id<ResolvedModuleWithFailedLookupLocations> {
    type Item = ResolvedModuleWithFailedLookupLocations;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ResolvedModuleWithFailedLookupLocations> {
        has_arena.resolved_module_with_failed_lookup_locations(*self)
    }
}

impl InArena for Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
    type Item = ResolvedTypeReferenceDirectiveWithFailedLookupLocations;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
        has_arena.resolved_type_reference_directive_with_failed_lookup_locations(*self)
    }
}

impl InArena for Id<Box<dyn PackageJsonInfoCache>> {
    type Item = Box<dyn PackageJsonInfoCache>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn PackageJsonInfoCache>> {
        has_arena.package_json_info_cache(*self)
    }
}

impl InArena for Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>> {
    type Item = ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>> {
        has_arena.mode_aware_cache_resolved_module_with_failed_lookup_locations(*self)
    }
}

impl InArena for Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>> {
    type Item = ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>> {
        has_arena.mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(*self)
    }
}

impl InArena for Id<PerModuleNameCache> {
    type Item = PerModuleNameCache;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, PerModuleNameCache> {
        has_arena.per_module_name_cache(*self)
    }
}

impl InArena for Id<Vec<Id<Diagnostic>>> {
    type Item = Vec<Id<Diagnostic>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Vec<Id<Diagnostic>>> {
        has_arena.vec_diagnostic(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, Vec<Id<Diagnostic>>> {
        has_arena.vec_diagnostic_mut(*self)
    }
}

impl InArena for Id<MultiMap<Path, Id<FileIncludeReason>>> {
    type Item = MultiMap<Path, Id<FileIncludeReason>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, MultiMap<Path, Id<FileIncludeReason>>> {
        has_arena.file_reasons(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, MultiMap<Path, Id<FileIncludeReason>>> {
        has_arena.file_reasons_mut(*self)
    }
}

impl InArena for Id<Box<dyn GetSymbolAccessibilityDiagnosticInterface>> {
    type Item = Box<dyn GetSymbolAccessibilityDiagnosticInterface>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn GetSymbolAccessibilityDiagnosticInterface>> {
        has_arena.get_symbol_accessibility_diagnostic_interface(*self)
    }
}

impl InArena for Id<Option<Vec<Id<Node>>>> {
    type Item = Option<Vec<Id<Node>>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Option<Vec<Id<Node>>>> {
        has_arena.option_vec_node(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, Option<Vec<Id<Node>>>> {
        has_arena.option_vec_node_mut(*self)
    }
}

impl InArena for Id<Vec<PendingDeclaration>> {
    type Item = Vec<PendingDeclaration>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Vec<PendingDeclaration>> {
        has_arena.vec_pending_declaration(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, Vec<PendingDeclaration>> {
        has_arena.vec_pending_declaration_mut(*self)
    }
}

impl InArena for Id<PackageJsonInfo> {
    type Item = PackageJsonInfo;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, PackageJsonInfo> {
        has_arena.package_json_info(*self)
    }
}

impl InArena for Id<Vec<Id<Type>>> {
    type Item = Vec<Id<Type>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Vec<Id<Type>>> {
        has_arena.vec_type(*self)
    }
}

impl InArena for Id<PatternAmbientModule> {
    type Item = PatternAmbientModule;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, PatternAmbientModule> {
        has_arena.pattern_ambient_module(*self)
    }
}

impl InArena for Id<Box<dyn CheckTypeContainingMessageChain>> {
    type Item = Box<dyn CheckTypeContainingMessageChain>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn CheckTypeContainingMessageChain>> {
        has_arena.check_type_containing_message_chain(*self)
    }
}

impl InArena for Id<Box<dyn CheckTypeErrorOutputContainer>> {
    type Item = Box<dyn CheckTypeErrorOutputContainer>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn CheckTypeErrorOutputContainer>> {
        has_arena.check_type_error_output_container(*self)
    }
}

impl InArena for Id<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
    type Item = HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        has_arena.resolved_type_reference_directives_map(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        has_arena.resolved_type_reference_directives_map_mut(*self)
    }
}

impl InArena for Id<NodeBuilder> {
    type Item = NodeBuilder;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, NodeBuilder> {
        has_arena.node_builder(*self)
    }
}

impl InArena for Id<NodeBuilderContext> {
    type Item = NodeBuilderContext;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, NodeBuilderContext> {
        has_arena.node_builder_context(*self)
    }
}

impl InArena for Id<Option<Vec<Id<Type>>>> {
    type Item = Option<Vec<Id<Type>>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Option<Vec<Id<Type>>>> {
        has_arena.option_vec_type(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, Option<Vec<Id<Type>>>> {
        has_arena.option_vec_type_mut(*self)
    }
}

impl InArena for Id<Option<HashMap<TypeId, Id<Node>>>> {
    type Item = Option<HashMap<TypeId, Id<Node>>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Option<HashMap<TypeId, Id<Node>>>> {
        has_arena.option_type_parameter_names(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, Option<HashMap<TypeId, Id<Node>>>> {
        has_arena.option_type_parameter_names_mut(*self)
    }
}

impl InArena for Id<Option<Vec<Id<Symbol>>>> {
    type Item = Option<Vec<Id<Symbol>>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Option<Vec<Id<Symbol>>>> {
        has_arena.option_vec_symbol(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, Option<Vec<Id<Symbol>>>> {
        has_arena.option_vec_symbol_mut(*self)
    }
}

impl InArena for Id<Box<dyn TypeComparer>> {
    type Item = Box<dyn TypeComparer>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn TypeComparer>> {
        has_arena.type_comparer(*self)
    }
}

impl InArena for Id<InferenceContext> {
    type Item = InferenceContext;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, InferenceContext> {
        has_arena.inference_context(*self)
    }
}

impl InArena for Id<Box<dyn SkipTrivia>> {
    type Item = Box<dyn SkipTrivia>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn SkipTrivia>> {
        has_arena.skip_trivia(*self)
    }
}

impl InArena for Id<Box<dyn CustomTransformerFactoryInterface>> {
    type Item = Box<dyn CustomTransformerFactoryInterface>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn CustomTransformerFactoryInterface>> {
        has_arena.custom_transformer_factory_interface(*self)
    }
}

pub trait OptionInArena {
    type Item;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, Self::Item>>;
}

impl OptionInArena for Option<Id<Node>> {
    type Item = Node;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, Node>> {
        self.map(|node| has_arena.node(node))
    }
}

impl OptionInArena for Option<Id<FileIncludeReason>> {
    type Item = FileIncludeReason;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, FileIncludeReason>> {
        self.map(|file_include_reason| has_arena.file_include_reason(file_include_reason))
    }
}

impl OptionInArena for Option<Id<CompilerOptions>> {
    type Item = CompilerOptions;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, CompilerOptions>> {
        self.map(|compiler_options| has_arena.compiler_options(compiler_options))
    }
}

impl OptionInArena for Option<Id<Program>> {
    type Item = Program;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, Program>> {
        self.map(|program| has_arena.program(program))
    }
}

impl OptionInArena for Option<Id<NodeArray>> {
    type Item = NodeArray;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, NodeArray>> {
        self.map(|node_array| has_arena.node_array(node_array))
    }
}

impl OptionInArena for Option<Id<TypeReferenceDirectiveResolutionCache>> {
    type Item = TypeReferenceDirectiveResolutionCache;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, TypeReferenceDirectiveResolutionCache>> {
        self.map(|type_reference_directive_resolution_cache| has_arena.type_reference_directive_resolution_cache(type_reference_directive_resolution_cache))
    }
}

impl OptionInArena for Option<Id<ModuleResolutionCache>> {
    type Item = ModuleResolutionCache;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, ModuleResolutionCache>> {
        self.map(|module_resolution_cache| has_arena.module_resolution_cache(module_resolution_cache))
    }
}

impl OptionInArena for Option<Id<ExternalModuleInfo>> {
    type Item = ExternalModuleInfo;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, ExternalModuleInfo>> {
        self.map(|external_module_info| has_arena.external_module_info(external_module_info))
    }
}

impl OptionInArena for Option<Id<Box<dyn PackageJsonInfoCache>>> {
    type Item = Box<dyn PackageJsonInfoCache>;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, Box<dyn PackageJsonInfoCache>>> {
        self.map(|package_json_info_cache| has_arena.package_json_info_cache(package_json_info_cache))
    }
}

impl OptionInArena for Option<Id<PerModuleNameCache>> {
    type Item = PerModuleNameCache;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, PerModuleNameCache>> {
        self.map(|per_module_name_cache| has_arena.per_module_name_cache(per_module_name_cache))
    }
}

impl OptionInArena for Option<Id<PackageJsonInfo>> {
    type Item = PackageJsonInfo;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, PackageJsonInfo>> {
        self.map(|package_json_info| has_arena.package_json_info(package_json_info))
    }
}

pub trait ArenaAlloc: Sized {
    fn alloc(self, arena: &impl HasArena) -> Id<Self>;
}

impl ArenaAlloc for ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>> {
    fn alloc(self, arena: &impl HasArena) -> Id<Self> {
        arena.alloc_mode_aware_cache_resolved_module_with_failed_lookup_locations(self)
    }
}

impl ArenaAlloc for ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>> {
    fn alloc(self, arena: &impl HasArena) -> Id<Self> {
        arena.alloc_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(self)
    }
}

impl ArenaAlloc for PerModuleNameCache {
    fn alloc(self, arena: &impl HasArena) -> Id<Self> {
        arena.alloc_per_module_name_cache(self)
    }
}

thread_local! {
    static ARENA: Lazy<Rc<AllArenas>> = Lazy::new(Default::default);
}

pub fn static_arena() -> Rc<AllArenas> {
    ARENA.with(|arena| (**arena).clone())
}

pub fn downcast_transformer_ref<TTransformer: Any>(
    transformer: Transformer,
    arena: &impl HasArena,
) -> Ref<'_, TTransformer> {
    Ref::map(
        transformer.ref_(arena),
        |transformer| transformer.as_dyn_any().downcast_ref::<TTransformer>().unwrap()
    )
}

pub enum IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory {
    EmitHost(Id<Box<dyn EmitHost>>),
    ModuleSpecifierResolutionHostAndGetCommonSourceDirectory(Id<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>>),
}

impl From<Id<Box<dyn EmitHost>>> for IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory {
    fn from(value: Id<Box<dyn EmitHost>>) -> Self {
        Self::EmitHost(value)
    }
}

impl From<Id<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>>> for IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory {
    fn from(value: Id<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>>) -> Self {
        Self::ModuleSpecifierResolutionHostAndGetCommonSourceDirectory(value)
    }
}

impl InArena for IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory {
    type Item = dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory;

    fn ref_<'a>(&self, arena: &'a impl HasArena) -> Ref<'a, dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory + 'static> {
        match self {
            Self::EmitHost(value) => Ref::map(
                value.ref_(arena),
                |value| value.as_module_specifier_resolution_host_and_get_common_source_directory(),
            ),
            Self::ModuleSpecifierResolutionHostAndGetCommonSourceDirectory(value) => Ref::map(
                value.ref_(arena),
                |value| &**value
            ),
        }
    }
}

#[macro_export]
macro_rules! per_arena {
    ($type:ty, $arena:expr, $initializer:expr $(,)?) => {{
        use std::cell::RefCell;
        use std::collections::HashMap;
        use id_arena::Id;
        use $crate::AllArenas;

        thread_local! {
            static PER_ARENA: RefCell<HashMap<*const AllArenas, Id<$type>>> = RefCell::new(HashMap::new());
        }

        PER_ARENA.with(|per_arena| {
            let mut per_arena = per_arena.borrow_mut();
            let arena_ptr: *const AllArenas = $arena.arena();
            *per_arena.entry(arena_ptr).or_insert_with(|| $initializer)
        })
    }}
}
