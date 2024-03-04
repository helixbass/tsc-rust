use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

use debug_cell::{Ref, RefCell, RefMut};
use id_arena::{Arena, Id};

use crate::{
    ActiveLabel, ActualResolveModuleNamesWorker, ActualResolveTypeReferenceDirectiveNamesWorker,
    BaseNodeFactory, BindBinaryExpressionFlow, Binder, BuildInfo, BuilderProgram, BundleBuildInfo,
    BundleFileInfo, BundleFileSection, CacheWithRedirects, CancellationToken,
    CheckBinaryExpression, CheckTypeContainingMessageChain, CheckTypeErrorOutputContainer,
    CheckTypeRelatedTo, ClassLexicalEnvironment, CodeBlock, CommandLineOption,
    CommandLineOptionInterface, CompilerHost, CompilerHostLike, CompilerOptions, ConditionalRoot,
    ConvertedLoopState, CurrentParenthesizerRule, CustomTransformerFactoryInterface,
    CustomTransformerInterface, Diagnostic, DiagnosticRelatedInformation, DiagnosticReporter,
    DidYouMeanOptionsDiagnostics, DirectoryStructureHost, EmitBinaryExpression, EmitHelper,
    EmitHelperFactory, EmitHelperTextCallback, EmitHost, EmitNode, EmitResolver, EmitTextWriter,
    ExternalModuleInfo, FileIncludeReason, FilePreprocessingDiagnostics, FlowNode,
    ForEachResolvedProjectReference, GetCanonicalFileName, GetProgramBuildInfo,
    GetResolvedProjectReferences, GetSourceFile, GetSymbolAccessibilityDiagnosticInterface,
    GetSymlinkCache, HasArena, InArena, IncrementalParserSyntaxCursor, IndexInfo, InferenceContext,
    InferenceInfo, InputFilesInitializedState, IterationTypes, LoadWithLocalCacheLoader,
    LoadWithModeAwareCacheLoader, LoggingHost, MakeSerializePropertySymbolCreateProperty,
    ModeAwareCache, ModuleResolutionCache, ModuleResolutionHostOverrider,
    ModuleSpecifierResolutionHostAndGetCommonSourceDirectory, MultiMap, Node, NodeArray,
    NodeBuilder, NodeBuilderContext, NodeFactory, NodeIdOverride, NodeInterface, NodeLinks,
    NodeSymbolOverride, OptionsNameMap, OutofbandVarianceMarkerHandler, PackageJsonInfo,
    PackageJsonInfoCache, ParenthesizerRules, ParseCommandLineWorkerDiagnostics,
    ParseConfigFileHost, ParsedCommandLine, ParserType, Path, PatternAmbientModule,
    PendingDeclaration, PerModuleNameCache, PrintHandlers, Printer, PrivateIdentifierEnvironment,
    PrivateIdentifierInfo, Program, ProgramBuildInfo, ReadFileCallback, RelativeToBuildInfo,
    ResolvedModuleFull, ResolvedModuleWithFailedLookupLocations, ResolvedProjectReference,
    ResolvedTypeReferenceDirective, ResolvedTypeReferenceDirectiveWithFailedLookupLocations,
    Signature, SkipTrivia, SourceMapGenerator, SourceMapRange, SourceMapSource, Symbol,
    SymbolAccessibilityDiagnostic, SymbolLinks, SymbolTable, SymbolTableToDeclarationStatements,
    SymbolTracker, SymlinkCache, SysFormatDiagnosticsHost, System, ToPath,
    TransformNodesTransformationResult, TransformationContextOnEmitNodeOverrider,
    TransformationContextOnSubstituteNodeOverrider, TransformerFactoryInterface,
    TransformerFactoryOrCustomTransformerFactory, TransformerInterface, Type, TypeChecker,
    TypeComparer, TypeId, TypeInterface, TypeMapper, TypeMapperCallback, TypePredicate,
    TypeReferenceDirectiveResolutionCache, WrapCustomTransformerFactoryHandleDefault,
    WriteFileCallback,
};

pub type AllArenasId = usize;

pub struct AllArenas {
    id: AllArenasId,
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
    pub module_specifier_resolution_host_and_get_common_source_directories:
        RefCell<Arena<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>>>,
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
    pub symbol_links: RefCell<Arena<SymbolLinks>>,
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
    pub wrap_custom_transformer_factory_handle_defaults:
        RefCell<Arena<Box<dyn WrapCustomTransformerFactoryHandleDefault>>>,
    pub transformation_context_on_emit_node_overriders:
        RefCell<Arena<Box<dyn TransformationContextOnEmitNodeOverrider>>>,
    pub source_map_generators: RefCell<Arena<Box<dyn SourceMapGenerator>>>,
    pub get_canonical_file_names: RefCell<Arena<Box<dyn GetCanonicalFileName>>>,
    pub emit_helper_factories: RefCell<Arena<EmitHelperFactory>>,
    pub transformation_context_on_substitute_node_overriders:
        RefCell<Arena<Box<dyn TransformationContextOnSubstituteNodeOverrider>>>,
    pub parsed_command_lines: RefCell<Arena<ParsedCommandLine>>,
    pub cancellation_tokens: RefCell<Arena<Box<dyn CancellationToken>>>,
    pub resolved_project_references: RefCell<Arena<ResolvedProjectReference>>,
    pub transformer_factory_or_custom_transformer_factories:
        RefCell<Arena<TransformerFactoryOrCustomTransformerFactory>>,
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
    pub for_each_resolved_project_references:
        RefCell<Arena<Box<dyn ForEachResolvedProjectReference>>>,
    pub compiler_host_likes: RefCell<Arena<Box<dyn CompilerHostLike>>>,
    pub directory_structure_hosts: RefCell<Arena<Box<dyn DirectoryStructureHost>>>,
    pub builder_programs: RefCell<Arena<Box<dyn BuilderProgram>>>,
    pub type_reference_directive_resolution_caches:
        RefCell<Arena<TypeReferenceDirectiveResolutionCache>>,
    pub module_resolution_caches: RefCell<Arena<ModuleResolutionCache>>,
    pub parse_config_file_hosts: RefCell<Arena<Box<dyn ParseConfigFileHost>>>,
    pub file_preprocessing_diagnostics: RefCell<Arena<FilePreprocessingDiagnostics>>,
    pub actual_resolve_module_names_workers:
        RefCell<Arena<Box<dyn ActualResolveModuleNamesWorker>>>,
    pub actual_resolve_type_reference_directive_names_workers:
        RefCell<Arena<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>>>,
    pub get_program_build_infos: RefCell<Arena<Box<dyn GetProgramBuildInfo>>>,
    pub load_with_mode_aware_cache_loaders:
        RefCell<Arena<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>>>,
    pub load_with_local_cache_loaders:
        RefCell<Arena<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>>>,
    pub symbol_accessibility_diagnostics: RefCell<Arena<SymbolAccessibilityDiagnostic>>,
    pub code_blocks: RefCell<Arena<CodeBlock>>,
    pub private_identifier_environments: RefCell<Arena<PrivateIdentifierEnvironment>>,
    pub private_identifier_infos: RefCell<Arena<PrivateIdentifierInfo>>,
    pub external_module_infos: RefCell<Arena<ExternalModuleInfo>>,
    pub resolved_modules_with_failed_lookup_locations:
        RefCell<Arena<ResolvedModuleWithFailedLookupLocations>>,
    pub resolved_type_reference_directives_with_failed_lookup_locations:
        RefCell<Arena<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
    pub package_json_info_caches: RefCell<Arena<Box<dyn PackageJsonInfoCache>>>,
    pub mode_aware_cache_resolved_module_with_failed_lookup_locations:
        RefCell<Arena<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>,
    pub mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations:
        RefCell<Arena<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>>,
    pub per_module_name_caches: RefCell<Arena<PerModuleNameCache>>,
    pub vec_diagnostics: RefCell<Arena<Vec<Id<Diagnostic>>>>,
    pub file_reasons: RefCell<Arena<MultiMap<Path, Id<FileIncludeReason>>>>,
    pub get_symbol_accessibility_diagnostic_interfaces:
        RefCell<Arena<Box<dyn GetSymbolAccessibilityDiagnosticInterface>>>,
    pub option_vec_nodes: RefCell<Arena<Option<Vec<Id<Node>>>>>,
    pub vec_pending_declarations: RefCell<Arena<Vec<PendingDeclaration>>>,
    pub package_json_infos: RefCell<Arena<PackageJsonInfo>>,
    pub vec_types: RefCell<Arena<Vec<Id<Type>>>>,
    pub pattern_ambient_modules: RefCell<Arena<PatternAmbientModule>>,
    pub check_type_containing_message_chains:
        RefCell<Arena<Box<dyn CheckTypeContainingMessageChain>>>,
    pub check_type_error_output_containers: RefCell<Arena<Box<dyn CheckTypeErrorOutputContainer>>>,
    pub resolved_type_reference_directives_maps:
        RefCell<Arena<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>>>,
    pub node_builders: RefCell<Arena<NodeBuilder>>,
    pub node_builder_contexts: RefCell<Arena<NodeBuilderContext>>,
    pub option_vec_types: RefCell<Arena<Option<Vec<Id<Type>>>>>,
    pub option_type_parameter_names: RefCell<Arena<Option<HashMap<TypeId, Id<Node>>>>>,
    pub option_vec_symbols: RefCell<Arena<Option<Vec<Id<Symbol>>>>>,
    pub type_comparers: RefCell<Arena<Box<dyn TypeComparer>>>,
    pub inference_contexts: RefCell<Arena<InferenceContext>>,
    pub skip_trivias: RefCell<Arena<Box<dyn SkipTrivia>>>,
    pub custom_transformer_factory_interfaces:
        RefCell<Arena<Box<dyn CustomTransformerFactoryInterface>>>,
    pub custom_transformer_interfaces: RefCell<Arena<Box<dyn CustomTransformerInterface>>>,
    pub node_links: RefCell<Arena<NodeLinks>>,
    pub parsers: RefCell<Arena<ParserType>>,
    pub incremental_parser_syntax_cursors: RefCell<Arena<IncrementalParserSyntaxCursor>>,
    pub command_line_options: RefCell<Arena<CommandLineOption>>,
    pub vec_command_line_options: RefCell<Arena<Vec<Id<CommandLineOption>>>>,
    pub options_name_maps: RefCell<Arena<OptionsNameMap>>,
    pub command_line_options_maps: RefCell<Arena<HashMap<String, Id<CommandLineOption>>>>,
    pub node_symbol_overrides: RefCell<Arena<Box<dyn NodeSymbolOverride>>>,
    pub node_id_overrides: RefCell<Arena<Box<dyn NodeIdOverride>>>,
    pub make_serialize_property_symbol_create_properties:
        RefCell<Arena<Box<dyn MakeSerializePropertySymbolCreateProperty>>>,
    pub symbol_table_to_declaration_statements: RefCell<Arena<SymbolTableToDeclarationStatements>>,
    pub input_files_initialized_states: RefCell<Arena<InputFilesInitializedState>>,
    pub vec_symbol_tables: RefCell<Arena<Vec<Id<SymbolTable>>>>,
    pub check_type_related_tos: RefCell<Arena<CheckTypeRelatedTo>>,
    pub flow_loop_caches: RefCell<Arena<HashMap<String, Id<Type>>>>,
    pub vec_symbols: RefCell<Arena<Vec<Id<Symbol>>>>,
    pub vec_nodes: RefCell<Arena<Vec<Id<Node>>>>,
    pub type_mapper_callbacks: RefCell<Arena<Box<dyn TypeMapperCallback>>>,
    pub option_symbol_tables: RefCell<Arena<Option<Id<SymbolTable>>>>,
    pub cache_with_redirects_per_module_name_caches:
        RefCell<Arena<CacheWithRedirects<PerModuleNameCache>>>,
    pub cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations: RefCell<
        Arena<CacheWithRedirects<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>,
    >,
    pub cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations:
        RefCell<
            Arena<
                CacheWithRedirects<
                    ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
                >,
            >,
        >,
    pub mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps:
        RefCell<
            Arena<
                HashMap<
                    String,
                    Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
                >,
            >,
        >,
    pub path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps:
        RefCell<
            Arena<
                HashMap<
                    Path,
                    Id<
                        HashMap<
                            String,
                            Id<
                                ModeAwareCache<
                                    Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>,
                                >,
                            >,
                        >,
                    >,
                >,
            >,
        >,
    pub mode_aware_cache_resolved_module_with_failed_lookup_locations_maps: RefCell<
        Arena<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>>,
    >,
    pub path_mode_aware_cache_resolved_module_with_failed_lookup_locations_maps: RefCell<
        Arena<
            HashMap<
                Path,
                Id<
                    HashMap<
                        String,
                        Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>,
                    >,
                >,
            >,
        >,
    >,
    pub per_module_name_cache_maps: RefCell<Arena<HashMap<String, Id<PerModuleNameCache>>>>,
    pub path_per_module_name_cache_maps:
        RefCell<Arena<HashMap<Path, Id<HashMap<String, Id<PerModuleNameCache>>>>>>,
    pub logging_hosts: RefCell<Arena<Box<dyn LoggingHost>>>,
    pub parse_command_line_worker_diagnostics:
        RefCell<Arena<Box<dyn ParseCommandLineWorkerDiagnostics>>>,
    pub did_you_mean_options_diagnostics: RefCell<Arena<Box<dyn DidYouMeanOptionsDiagnostics>>>,
}

impl Default for AllArenas {
    fn default() -> Self {
        static ARENA_COUNTER: AtomicUsize = AtomicUsize::new(0);
        Self { id: ARENA_COUNTER.fetch_add(1, Ordering::SeqCst), nodes: Default::default(), symbols: Default::default(), types: Default::default(), type_mappers: Default::default(), transform_nodes_transformation_results: Default::default(), transformers: Default::default(), transformer_factories: Default::default(), emit_text_writers: Default::default(), symbol_trackers: Default::default(), emit_hosts: Default::default(), module_specifier_resolution_host_and_get_common_source_directories: Default::default(), file_include_reasons: Default::default(), systems: Default::default(), source_map_ranges: Default::default(), emit_helpers: Default::default(), compiler_options: Default::default(), flow_nodes: Default::default(), diagnostics: Default::default(), programs: Default::default(), signatures: Default::default(), diagnostic_reporters: Default::default(), node_factories: Default::default(), base_node_factories: Default::default(), emit_resolvers: Default::default(), resolved_type_reference_directives: Default::default(), compiler_hosts: Default::default(), symbol_links: Default::default(), printers: Default::default(), diagnostic_related_informations: Default::default(), index_infos: Default::default(), current_parenthesizer_rules: Default::default(), parenthesizer_rules: Default::default(), iteration_types: Default::default(), type_predicates: Default::default(), active_labels: Default::default(), to_paths: Default::default(), module_resolution_host_overriders: Default::default(), wrap_custom_transformer_factory_handle_defaults: Default::default(), transformation_context_on_emit_node_overriders: Default::default(), source_map_generators: Default::default(), get_canonical_file_names: Default::default(), emit_helper_factories: Default::default(), transformation_context_on_substitute_node_overriders: Default::default(), parsed_command_lines: Default::default(), cancellation_tokens: Default::default(), resolved_project_references: Default::default(), transformer_factory_or_custom_transformer_factories: Default::default(), symlink_caches: Default::default(), write_file_callbacks: Default::default(), resolved_module_fulls: Default::default(), node_arrays: Default::default(), bundle_file_sections: Default::default(), build_infos: Default::default(), program_build_infos: Default::default(), bundle_build_infos: Default::default(), bundle_file_infos: Default::default(), symbol_tables: Default::default(), inference_infos: Default::default(), sys_format_diagnostics_hosts: Default::default(), class_lexical_environments: Default::default(), converted_loop_states: Default::default(), emit_helper_text_callbacks: Default::default(), conditional_roots: Default::default(), emit_nodes: Default::default(), check_binary_expressions: Default::default(), source_map_sources: Default::default(), outofband_variance_marker_handlers: Default::default(), bind_binary_expression_flows: Default::default(), type_checkers: Default::default(), read_file_callbacks: Default::default(), binders: Default::default(), get_source_files: Default::default(), get_symlink_caches: Default::default(), emit_binary_expressions: Default::default(), relative_to_build_infos: Default::default(), print_handlers: Default::default(), get_resolved_project_references: Default::default(), for_each_resolved_project_references: Default::default(), compiler_host_likes: Default::default(), directory_structure_hosts: Default::default(), builder_programs: Default::default(), type_reference_directive_resolution_caches: Default::default(), module_resolution_caches: Default::default(), parse_config_file_hosts: Default::default(), file_preprocessing_diagnostics: Default::default(), actual_resolve_module_names_workers: Default::default(), actual_resolve_type_reference_directive_names_workers: Default::default(), get_program_build_infos: Default::default(), load_with_mode_aware_cache_loaders: Default::default(), load_with_local_cache_loaders: Default::default(), symbol_accessibility_diagnostics: Default::default(), code_blocks: Default::default(), private_identifier_environments: Default::default(), private_identifier_infos: Default::default(), external_module_infos: Default::default(), resolved_modules_with_failed_lookup_locations: Default::default(), resolved_type_reference_directives_with_failed_lookup_locations: Default::default(), package_json_info_caches: Default::default(), mode_aware_cache_resolved_module_with_failed_lookup_locations: Default::default(), mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations: Default::default(), per_module_name_caches: Default::default(), vec_diagnostics: Default::default(), file_reasons: Default::default(), get_symbol_accessibility_diagnostic_interfaces: Default::default(), option_vec_nodes: Default::default(), vec_pending_declarations: Default::default(), package_json_infos: Default::default(), vec_types: Default::default(), pattern_ambient_modules: Default::default(), check_type_containing_message_chains: Default::default(), check_type_error_output_containers: Default::default(), resolved_type_reference_directives_maps: Default::default(), node_builders: Default::default(), node_builder_contexts: Default::default(), option_vec_types: Default::default(), option_type_parameter_names: Default::default(), option_vec_symbols: Default::default(), type_comparers: Default::default(), inference_contexts: Default::default(), skip_trivias: Default::default(), custom_transformer_factory_interfaces: Default::default(), custom_transformer_interfaces: Default::default(), node_links: Default::default(), parsers: Default::default(), incremental_parser_syntax_cursors: Default::default(), command_line_options: Default::default(), vec_command_line_options: Default::default(), options_name_maps: Default::default(), command_line_options_maps: Default::default(), node_symbol_overrides: Default::default(), node_id_overrides: Default::default(), make_serialize_property_symbol_create_properties: Default::default(), symbol_table_to_declaration_statements: Default::default(), input_files_initialized_states: Default::default(), vec_symbol_tables: Default::default(), check_type_related_tos: Default::default(), flow_loop_caches: Default::default(), vec_symbols: Default::default(), vec_nodes: Default::default(), type_mapper_callbacks: Default::default(), option_symbol_tables: Default::default(), cache_with_redirects_per_module_name_caches: Default::default(), cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations: Default::default(), cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations: Default::default(), mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps: Default::default(), path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps: Default::default(), mode_aware_cache_resolved_module_with_failed_lookup_locations_maps: Default::default(), path_mode_aware_cache_resolved_module_with_failed_lookup_locations_maps: Default::default(), per_module_name_cache_maps: Default::default(), path_per_module_name_cache_maps: Default::default(), logging_hosts: Default::default(), parse_command_line_worker_diagnostics: Default::default(), did_you_mean_options_diagnostics: Default::default() }
    }
}

impl HasArena for AllArenas {
    fn arena(&self) -> &AllArenas {
        self
    }

    fn all_arenas_id(&self) -> AllArenasId {
        self.id
    }

    #[track_caller]
    fn node(&self, node: Id<Node>) -> Ref<Node> {
        Ref::map(self.nodes.borrow(), |nodes| &nodes[node])
    }

    fn alloc_node(&self, node: Node) -> Id<Node> {
        let id = self.nodes.borrow_mut().alloc(node);
        id.ref_(self).set_arena_id(id);
        id
    }

    #[track_caller]
    fn type_ref(&self, type_: Id<Type>) -> Ref<Type> {
        Ref::map(self.types.borrow(), |types| &types[type_])
    }

    // #[track_caller]
    // pub fn type_mut(&self, type_: Id<Type>) -> RefMut<Type> {
    //     RefMut::map(self.types.borrow_mut(), |types| &mut types[type_])
    // }

    fn alloc_type(&self, type_: Type) -> Id<Type> {
        let id = self.types.borrow_mut().alloc(type_);
        id.ref_(self).set_arena_id(id);
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
    fn symbol_ref(&self, symbol: Id<Symbol>) -> Ref<Symbol> {
        Ref::map(self.symbols.borrow(), |symbols| &symbols[symbol])
    }

    fn alloc_symbol(&self, symbol: Symbol) -> Id<Symbol> {
        let id = self.symbols.borrow_mut().alloc(symbol);
        id
    }

    #[track_caller]
    fn transform_nodes_transformation_result(
        &self,
        transform_nodes_transformation_result: Id<TransformNodesTransformationResult>,
    ) -> Ref<TransformNodesTransformationResult> {
        Ref::map(
            self.transform_nodes_transformation_results.borrow(),
            |transform_nodes_transformation_results| {
                &transform_nodes_transformation_results[transform_nodes_transformation_result]
            },
        )
    }

    fn alloc_transform_nodes_transformation_result(
        &self,
        transform_nodes_transformation_result: TransformNodesTransformationResult,
    ) -> Id<TransformNodesTransformationResult> {
        let id = self
            .transform_nodes_transformation_results
            .borrow_mut()
            .alloc(transform_nodes_transformation_result);
        id
    }

    #[track_caller]
    fn transformer(
        &self,
        transformer: Id<Box<dyn TransformerInterface>>,
    ) -> Ref<Box<dyn TransformerInterface>> {
        Ref::map(self.transformers.borrow(), |transformers| {
            &transformers[transformer]
        })
    }

    fn alloc_transformer(
        &self,
        transformer: Box<dyn TransformerInterface>,
    ) -> Id<Box<dyn TransformerInterface>> {
        let id = self.transformers.borrow_mut().alloc(transformer);
        id
    }

    #[track_caller]
    fn transformer_factory(
        &self,
        transformer_factory: Id<Box<dyn TransformerFactoryInterface>>,
    ) -> Ref<Box<dyn TransformerFactoryInterface>> {
        Ref::map(
            self.transformer_factories.borrow(),
            |transformer_factories| &transformer_factories[transformer_factory],
        )
    }

    fn alloc_transformer_factory(
        &self,
        transformer_factory: Box<dyn TransformerFactoryInterface>,
    ) -> Id<Box<dyn TransformerFactoryInterface>> {
        let id = self
            .transformer_factories
            .borrow_mut()
            .alloc(transformer_factory);
        id
    }

    #[track_caller]
    fn emit_text_writer(
        &self,
        emit_text_writer: Id<Box<dyn EmitTextWriter>>,
    ) -> Ref<Box<dyn EmitTextWriter>> {
        Ref::map(self.emit_text_writers.borrow(), |emit_text_writers| {
            &emit_text_writers[emit_text_writer]
        })
    }

    fn alloc_emit_text_writer(
        &self,
        emit_text_writer: Box<dyn EmitTextWriter>,
    ) -> Id<Box<dyn EmitTextWriter>> {
        let id = self.emit_text_writers.borrow_mut().alloc(emit_text_writer);
        id
    }

    #[track_caller]
    fn symbol_tracker(
        &self,
        symbol_tracker: Id<Box<dyn SymbolTracker>>,
    ) -> Ref<Box<dyn SymbolTracker>> {
        Ref::map(self.symbol_trackers.borrow(), |symbol_trackers| {
            &symbol_trackers[symbol_tracker]
        })
    }

    fn alloc_symbol_tracker(
        &self,
        symbol_tracker: Box<dyn SymbolTracker>,
    ) -> Id<Box<dyn SymbolTracker>> {
        let id = self.symbol_trackers.borrow_mut().alloc(symbol_tracker);
        id
    }

    #[track_caller]
    fn emit_host(&self, emit_host: Id<Box<dyn EmitHost>>) -> Ref<Box<dyn EmitHost>> {
        Ref::map(self.emit_hosts.borrow(), |emit_hosts| {
            &emit_hosts[emit_host]
        })
    }

    fn alloc_emit_host(&self, emit_host: Box<dyn EmitHost>) -> Id<Box<dyn EmitHost>> {
        let id = self.emit_hosts.borrow_mut().alloc(emit_host);
        id
    }

    #[track_caller]
    fn module_specifier_resolution_host_and_get_common_source_directory(
        &self,
        module_specifier_resolution_host_and_get_common_source_directory: Id<
            Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>,
        >,
    ) -> Ref<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>> {
        Ref::map(
            self.module_specifier_resolution_host_and_get_common_source_directories
                .borrow(),
            |module_specifier_resolution_host_and_get_common_source_directories| {
                &module_specifier_resolution_host_and_get_common_source_directories
                    [module_specifier_resolution_host_and_get_common_source_directory]
            },
        )
    }

    fn alloc_module_specifier_resolution_host_and_get_common_source_directory(
        &self,
        module_specifier_resolution_host_and_get_common_source_directory: Box<
            dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory,
        >,
    ) -> Id<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>> {
        let id = self
            .module_specifier_resolution_host_and_get_common_source_directories
            .borrow_mut()
            .alloc(module_specifier_resolution_host_and_get_common_source_directory);
        id
    }

    #[track_caller]
    fn file_include_reason(
        &self,
        file_include_reason: Id<FileIncludeReason>,
    ) -> Ref<FileIncludeReason> {
        Ref::map(self.file_include_reasons.borrow(), |file_include_reasons| {
            &file_include_reasons[file_include_reason]
        })
    }

    fn alloc_file_include_reason(
        &self,
        file_include_reason: FileIncludeReason,
    ) -> Id<FileIncludeReason> {
        let id = self
            .file_include_reasons
            .borrow_mut()
            .alloc(file_include_reason);
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
        Ref::map(self.source_map_ranges.borrow(), |source_map_ranges| {
            &source_map_ranges[source_map_range]
        })
    }

    fn alloc_source_map_range(&self, source_map_range: SourceMapRange) -> Id<SourceMapRange> {
        let id = self.source_map_ranges.borrow_mut().alloc(source_map_range);
        id
    }

    #[track_caller]
    fn emit_helper(&self, emit_helper: Id<EmitHelper>) -> Ref<EmitHelper> {
        Ref::map(self.emit_helpers.borrow(), |emit_helpers| {
            &emit_helpers[emit_helper]
        })
    }

    fn alloc_emit_helper(&self, emit_helper: EmitHelper) -> Id<EmitHelper> {
        let id = self.emit_helpers.borrow_mut().alloc(emit_helper);
        id
    }

    #[track_caller]
    fn compiler_options(&self, compiler_options: Id<CompilerOptions>) -> Ref<CompilerOptions> {
        Ref::map(self.compiler_options.borrow(), |compiler_options_| {
            &compiler_options_[compiler_options]
        })
    }

    fn alloc_compiler_options(&self, compiler_options: CompilerOptions) -> Id<CompilerOptions> {
        let id = self.compiler_options.borrow_mut().alloc(compiler_options);
        id
    }

    #[track_caller]
    fn flow_node(&self, flow_node: Id<FlowNode>) -> Ref<FlowNode> {
        Ref::map(self.flow_nodes.borrow(), |flow_nodes| {
            &flow_nodes[flow_node]
        })
    }

    fn alloc_flow_node(&self, flow_node: FlowNode) -> Id<FlowNode> {
        let id = self.flow_nodes.borrow_mut().alloc(flow_node);
        id
    }

    #[track_caller]
    fn diagnostic(&self, diagnostic: Id<Diagnostic>) -> Ref<Diagnostic> {
        Ref::map(self.diagnostics.borrow(), |diagnostics| {
            &diagnostics[diagnostic]
        })
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
        Ref::map(self.signatures.borrow(), |signatures| {
            &signatures[signature]
        })
    }

    fn alloc_signature(&self, signature: Signature) -> Id<Signature> {
        let id = self.signatures.borrow_mut().alloc(signature);
        id
    }

    #[track_caller]
    fn diagnostic_reporter(
        &self,
        diagnostic_reporter: Id<Box<dyn DiagnosticReporter>>,
    ) -> Ref<Box<dyn DiagnosticReporter>> {
        Ref::map(self.diagnostic_reporters.borrow(), |diagnostic_reporters| {
            &diagnostic_reporters[diagnostic_reporter]
        })
    }

    fn alloc_diagnostic_reporter(
        &self,
        diagnostic_reporter: Box<dyn DiagnosticReporter>,
    ) -> Id<Box<dyn DiagnosticReporter>> {
        let id = self
            .diagnostic_reporters
            .borrow_mut()
            .alloc(diagnostic_reporter);
        id
    }

    #[track_caller]
    fn node_factory(&self, node_factory: Id<NodeFactory>) -> Ref<NodeFactory> {
        Ref::map(self.node_factories.borrow(), |node_factories| {
            &node_factories[node_factory]
        })
    }

    fn alloc_node_factory(&self, node_factory: NodeFactory) -> Id<NodeFactory> {
        let id = self.node_factories.borrow_mut().alloc(node_factory);
        id
    }

    #[track_caller]
    fn base_node_factory(
        &self,
        base_node_factory: Id<Box<dyn BaseNodeFactory>>,
    ) -> Ref<Box<dyn BaseNodeFactory>> {
        Ref::map(self.base_node_factories.borrow(), |base_node_factories| {
            &base_node_factories[base_node_factory]
        })
    }

    fn alloc_base_node_factory(
        &self,
        base_node_factory: Box<dyn BaseNodeFactory>,
    ) -> Id<Box<dyn BaseNodeFactory>> {
        let id = self
            .base_node_factories
            .borrow_mut()
            .alloc(base_node_factory);
        id
    }

    #[track_caller]
    fn emit_resolver(
        &self,
        emit_resolver: Id<Box<dyn EmitResolver>>,
    ) -> Ref<Box<dyn EmitResolver>> {
        Ref::map(self.emit_resolvers.borrow(), |emit_resolvers| {
            &emit_resolvers[emit_resolver]
        })
    }

    fn alloc_emit_resolver(
        &self,
        emit_resolver: Box<dyn EmitResolver>,
    ) -> Id<Box<dyn EmitResolver>> {
        let id = self.emit_resolvers.borrow_mut().alloc(emit_resolver);
        id
    }

    #[track_caller]
    fn resolved_type_reference_directive(
        &self,
        resolved_type_reference_directive: Id<ResolvedTypeReferenceDirective>,
    ) -> Ref<ResolvedTypeReferenceDirective> {
        Ref::map(
            self.resolved_type_reference_directives.borrow(),
            |resolved_type_reference_directives| {
                &resolved_type_reference_directives[resolved_type_reference_directive]
            },
        )
    }

    fn alloc_resolved_type_reference_directive(
        &self,
        resolved_type_reference_directive: ResolvedTypeReferenceDirective,
    ) -> Id<ResolvedTypeReferenceDirective> {
        let id = self
            .resolved_type_reference_directives
            .borrow_mut()
            .alloc(resolved_type_reference_directive);
        id
    }

    #[track_caller]
    fn compiler_host(
        &self,
        compiler_host: Id<Box<dyn CompilerHost>>,
    ) -> Ref<Box<dyn CompilerHost>> {
        Ref::map(self.compiler_hosts.borrow(), |compiler_hosts| {
            &compiler_hosts[compiler_host]
        })
    }

    fn alloc_compiler_host(
        &self,
        compiler_host: Box<dyn CompilerHost>,
    ) -> Id<Box<dyn CompilerHost>> {
        let id = self.compiler_hosts.borrow_mut().alloc(compiler_host);
        id
    }

    #[track_caller]
    fn symbol_links(&self, symbol_links: Id<SymbolLinks>) -> Ref<SymbolLinks> {
        Ref::map(self.symbol_links.borrow(), |symbol_links_| {
            &symbol_links_[symbol_links]
        })
    }

    fn symbol_links_mut(&self, symbol_links: Id<SymbolLinks>) -> RefMut<SymbolLinks> {
        RefMut::map(self.symbol_links.borrow_mut(), |symbol_links_| {
            &mut symbol_links_[symbol_links]
        })
    }

    fn alloc_symbol_links(&self, symbol_links: SymbolLinks) -> Id<SymbolLinks> {
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
    fn diagnostic_related_information(
        &self,
        diagnostic_related_information: Id<DiagnosticRelatedInformation>,
    ) -> Ref<DiagnosticRelatedInformation> {
        Ref::map(
            self.diagnostic_related_informations.borrow(),
            |diagnostic_related_informations| {
                &diagnostic_related_informations[diagnostic_related_information]
            },
        )
    }

    fn alloc_diagnostic_related_information(
        &self,
        diagnostic_related_information: DiagnosticRelatedInformation,
    ) -> Id<DiagnosticRelatedInformation> {
        let id = self
            .diagnostic_related_informations
            .borrow_mut()
            .alloc(diagnostic_related_information);
        id
    }

    #[track_caller]
    fn index_info(&self, index_info: Id<IndexInfo>) -> Ref<IndexInfo> {
        Ref::map(self.index_infos.borrow(), |index_infos| {
            &index_infos[index_info]
        })
    }

    fn alloc_index_info(&self, index_info: IndexInfo) -> Id<IndexInfo> {
        let id = self.index_infos.borrow_mut().alloc(index_info);
        id
    }

    #[track_caller]
    fn current_parenthesizer_rule(
        &self,
        current_parenthesizer_rule: Id<Box<dyn CurrentParenthesizerRule>>,
    ) -> Ref<Box<dyn CurrentParenthesizerRule>> {
        Ref::map(
            self.current_parenthesizer_rules.borrow(),
            |current_parenthesizer_rules| &current_parenthesizer_rules[current_parenthesizer_rule],
        )
    }

    fn alloc_current_parenthesizer_rule(
        &self,
        current_parenthesizer_rule: Box<dyn CurrentParenthesizerRule>,
    ) -> Id<Box<dyn CurrentParenthesizerRule>> {
        let id = self
            .current_parenthesizer_rules
            .borrow_mut()
            .alloc(current_parenthesizer_rule);
        id
    }

    #[track_caller]
    fn parenthesizer_rules(
        &self,
        parenthesizer_rules: Id<Box<dyn ParenthesizerRules>>,
    ) -> Ref<Box<dyn ParenthesizerRules>> {
        Ref::map(self.parenthesizer_rules.borrow(), |parenthesizer_rules_| {
            &parenthesizer_rules_[parenthesizer_rules]
        })
    }

    fn alloc_parenthesizer_rules(
        &self,
        parenthesizer_rules: Box<dyn ParenthesizerRules>,
    ) -> Id<Box<dyn ParenthesizerRules>> {
        let id = self
            .parenthesizer_rules
            .borrow_mut()
            .alloc(parenthesizer_rules);
        id
    }

    #[track_caller]
    fn iteration_types(&self, iteration_types: Id<IterationTypes>) -> Ref<IterationTypes> {
        Ref::map(self.iteration_types.borrow(), |iteration_types_| {
            &iteration_types_[iteration_types]
        })
    }

    fn alloc_iteration_types(&self, iteration_types: IterationTypes) -> Id<IterationTypes> {
        let id = self.iteration_types.borrow_mut().alloc(iteration_types);
        id
    }

    #[track_caller]
    fn type_predicate(&self, type_predicate: Id<TypePredicate>) -> Ref<TypePredicate> {
        Ref::map(self.type_predicates.borrow(), |type_predicates| {
            &type_predicates[type_predicate]
        })
    }

    fn alloc_type_predicate(&self, type_predicate: TypePredicate) -> Id<TypePredicate> {
        let id = self.type_predicates.borrow_mut().alloc(type_predicate);
        id
    }

    #[track_caller]
    fn active_label(&self, active_label: Id<ActiveLabel>) -> Ref<ActiveLabel> {
        Ref::map(self.active_labels.borrow(), |active_labels| {
            &active_labels[active_label]
        })
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
    fn module_resolution_host_overrider(
        &self,
        module_resolution_host_overrider: Id<Box<dyn ModuleResolutionHostOverrider>>,
    ) -> Ref<Box<dyn ModuleResolutionHostOverrider>> {
        Ref::map(
            self.module_resolution_host_overriders.borrow(),
            |module_resolution_host_overriders| {
                &module_resolution_host_overriders[module_resolution_host_overrider]
            },
        )
    }

    fn alloc_module_resolution_host_overrider(
        &self,
        module_resolution_host_overrider: Box<dyn ModuleResolutionHostOverrider>,
    ) -> Id<Box<dyn ModuleResolutionHostOverrider>> {
        let id = self
            .module_resolution_host_overriders
            .borrow_mut()
            .alloc(module_resolution_host_overrider);
        id
    }

    #[track_caller]
    fn wrap_custom_transformer_factory_handle_default(
        &self,
        wrap_custom_transformer_factory_handle_default: Id<
            Box<dyn WrapCustomTransformerFactoryHandleDefault>,
        >,
    ) -> Ref<Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
        Ref::map(
            self.wrap_custom_transformer_factory_handle_defaults
                .borrow(),
            |wrap_custom_transformer_factory_handle_defaults| {
                &wrap_custom_transformer_factory_handle_defaults
                    [wrap_custom_transformer_factory_handle_default]
            },
        )
    }

    fn alloc_wrap_custom_transformer_factory_handle_default(
        &self,
        wrap_custom_transformer_factory_handle_default: Box<
            dyn WrapCustomTransformerFactoryHandleDefault,
        >,
    ) -> Id<Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
        let id = self
            .wrap_custom_transformer_factory_handle_defaults
            .borrow_mut()
            .alloc(wrap_custom_transformer_factory_handle_default);
        id
    }

    #[track_caller]
    fn transformation_context_on_emit_node_overrider(
        &self,
        transformation_context_on_emit_node_overrider: Id<
            Box<dyn TransformationContextOnEmitNodeOverrider>,
        >,
    ) -> Ref<Box<dyn TransformationContextOnEmitNodeOverrider>> {
        Ref::map(
            self.transformation_context_on_emit_node_overriders.borrow(),
            |transformation_context_on_emit_node_overriders| {
                &transformation_context_on_emit_node_overriders
                    [transformation_context_on_emit_node_overrider]
            },
        )
    }

    fn alloc_transformation_context_on_emit_node_overrider(
        &self,
        transformation_context_on_emit_node_overrider: Box<
            dyn TransformationContextOnEmitNodeOverrider,
        >,
    ) -> Id<Box<dyn TransformationContextOnEmitNodeOverrider>> {
        let id = self
            .transformation_context_on_emit_node_overriders
            .borrow_mut()
            .alloc(transformation_context_on_emit_node_overrider);
        id
    }

    #[track_caller]
    fn source_map_generator(
        &self,
        source_map_generator: Id<Box<dyn SourceMapGenerator>>,
    ) -> Ref<Box<dyn SourceMapGenerator>> {
        Ref::map(
            self.source_map_generators.borrow(),
            |source_map_generators| &source_map_generators[source_map_generator],
        )
    }

    fn alloc_source_map_generator(
        &self,
        source_map_generator: Box<dyn SourceMapGenerator>,
    ) -> Id<Box<dyn SourceMapGenerator>> {
        let id = self
            .source_map_generators
            .borrow_mut()
            .alloc(source_map_generator);
        id
    }

    #[track_caller]
    fn get_canonical_file_name_ref(
        &self,
        get_canonical_file_name: Id<Box<dyn GetCanonicalFileName>>,
    ) -> Ref<Box<dyn GetCanonicalFileName>> {
        Ref::map(
            self.get_canonical_file_names.borrow(),
            |get_canonical_file_names| &get_canonical_file_names[get_canonical_file_name],
        )
    }

    fn alloc_get_canonical_file_name(
        &self,
        get_canonical_file_name: Box<dyn GetCanonicalFileName>,
    ) -> Id<Box<dyn GetCanonicalFileName>> {
        let id = self
            .get_canonical_file_names
            .borrow_mut()
            .alloc(get_canonical_file_name);
        id
    }

    #[track_caller]
    fn emit_helper_factory(
        &self,
        emit_helper_factory: Id<EmitHelperFactory>,
    ) -> Ref<EmitHelperFactory> {
        Ref::map(
            self.emit_helper_factories.borrow(),
            |emit_helper_factories| &emit_helper_factories[emit_helper_factory],
        )
    }

    fn alloc_emit_helper_factory(
        &self,
        emit_helper_factory: EmitHelperFactory,
    ) -> Id<EmitHelperFactory> {
        let id = self
            .emit_helper_factories
            .borrow_mut()
            .alloc(emit_helper_factory);
        id
    }

    #[track_caller]
    fn transformation_context_on_substitute_node_overrider(
        &self,
        transformation_context_on_substitute_node_overrider: Id<
            Box<dyn TransformationContextOnSubstituteNodeOverrider>,
        >,
    ) -> Ref<Box<dyn TransformationContextOnSubstituteNodeOverrider>> {
        Ref::map(
            self.transformation_context_on_substitute_node_overriders
                .borrow(),
            |transformation_context_on_substitute_node_overriders| {
                &transformation_context_on_substitute_node_overriders
                    [transformation_context_on_substitute_node_overrider]
            },
        )
    }

    fn alloc_transformation_context_on_substitute_node_overrider(
        &self,
        transformation_context_on_substitute_node_overrider: Box<
            dyn TransformationContextOnSubstituteNodeOverrider,
        >,
    ) -> Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>> {
        let id = self
            .transformation_context_on_substitute_node_overriders
            .borrow_mut()
            .alloc(transformation_context_on_substitute_node_overrider);
        id
    }

    #[track_caller]
    fn parsed_command_line(
        &self,
        parsed_command_line: Id<ParsedCommandLine>,
    ) -> Ref<ParsedCommandLine> {
        Ref::map(self.parsed_command_lines.borrow(), |parsed_command_lines| {
            &parsed_command_lines[parsed_command_line]
        })
    }

    fn alloc_parsed_command_line(
        &self,
        parsed_command_line: ParsedCommandLine,
    ) -> Id<ParsedCommandLine> {
        let id = self
            .parsed_command_lines
            .borrow_mut()
            .alloc(parsed_command_line);
        id
    }

    #[track_caller]
    fn cancellation_token(
        &self,
        cancellation_token: Id<Box<dyn CancellationToken>>,
    ) -> Ref<Box<dyn CancellationToken>> {
        Ref::map(self.cancellation_tokens.borrow(), |cancellation_tokens| {
            &cancellation_tokens[cancellation_token]
        })
    }

    fn alloc_cancellation_token(
        &self,
        cancellation_token: Box<dyn CancellationToken>,
    ) -> Id<Box<dyn CancellationToken>> {
        let id = self
            .cancellation_tokens
            .borrow_mut()
            .alloc(cancellation_token);
        id
    }

    #[track_caller]
    fn resolved_project_reference(
        &self,
        resolved_project_reference: Id<ResolvedProjectReference>,
    ) -> Ref<ResolvedProjectReference> {
        Ref::map(
            self.resolved_project_references.borrow(),
            |resolved_project_references| &resolved_project_references[resolved_project_reference],
        )
    }

    fn alloc_resolved_project_reference(
        &self,
        resolved_project_reference: ResolvedProjectReference,
    ) -> Id<ResolvedProjectReference> {
        let id = self
            .resolved_project_references
            .borrow_mut()
            .alloc(resolved_project_reference);
        id
    }

    #[track_caller]
    fn transformer_factory_or_custom_transformer_factory(
        &self,
        transformer_factory_or_custom_transformer_factory: Id<
            TransformerFactoryOrCustomTransformerFactory,
        >,
    ) -> Ref<TransformerFactoryOrCustomTransformerFactory> {
        Ref::map(
            self.transformer_factory_or_custom_transformer_factories
                .borrow(),
            |transformer_factory_or_custom_transformer_factories| {
                &transformer_factory_or_custom_transformer_factories
                    [transformer_factory_or_custom_transformer_factory]
            },
        )
    }

    fn alloc_transformer_factory_or_custom_transformer_factory(
        &self,
        transformer_factory_or_custom_transformer_factory: TransformerFactoryOrCustomTransformerFactory,
    ) -> Id<TransformerFactoryOrCustomTransformerFactory> {
        let id = self
            .transformer_factory_or_custom_transformer_factories
            .borrow_mut()
            .alloc(transformer_factory_or_custom_transformer_factory);
        id
    }

    #[track_caller]
    fn symlink_cache(&self, symlink_cache: Id<SymlinkCache>) -> Ref<SymlinkCache> {
        Ref::map(self.symlink_caches.borrow(), |symlink_caches| {
            &symlink_caches[symlink_cache]
        })
    }

    fn alloc_symlink_cache(&self, symlink_cache: SymlinkCache) -> Id<SymlinkCache> {
        let id = self.symlink_caches.borrow_mut().alloc(symlink_cache);
        id
    }

    #[track_caller]
    fn write_file_callback(
        &self,
        write_file_callback: Id<Box<dyn WriteFileCallback>>,
    ) -> Ref<Box<dyn WriteFileCallback>> {
        Ref::map(self.write_file_callbacks.borrow(), |write_file_callbacks| {
            &write_file_callbacks[write_file_callback]
        })
    }

    fn alloc_write_file_callback(
        &self,
        write_file_callback: Box<dyn WriteFileCallback>,
    ) -> Id<Box<dyn WriteFileCallback>> {
        let id = self
            .write_file_callbacks
            .borrow_mut()
            .alloc(write_file_callback);
        id
    }

    #[track_caller]
    fn resolved_module_full(
        &self,
        resolved_module_full: Id<ResolvedModuleFull>,
    ) -> Ref<ResolvedModuleFull> {
        Ref::map(
            self.resolved_module_fulls.borrow(),
            |resolved_module_fulls| &resolved_module_fulls[resolved_module_full],
        )
    }

    fn alloc_resolved_module_full(
        &self,
        resolved_module_full: ResolvedModuleFull,
    ) -> Id<ResolvedModuleFull> {
        let id = self
            .resolved_module_fulls
            .borrow_mut()
            .alloc(resolved_module_full);
        id
    }

    #[track_caller]
    fn node_array(&self, node_array: Id<NodeArray>) -> Ref<NodeArray> {
        Ref::map(self.node_arrays.borrow(), |node_arrays| {
            &node_arrays[node_array]
        })
    }

    fn alloc_node_array(&self, node_array: NodeArray) -> Id<NodeArray> {
        let id = self.node_arrays.borrow_mut().alloc(node_array);
        id
    }

    #[track_caller]
    fn bundle_file_section(
        &self,
        bundle_file_section: Id<BundleFileSection>,
    ) -> Ref<BundleFileSection> {
        Ref::map(self.bundle_file_sections.borrow(), |bundle_file_sections| {
            &bundle_file_sections[bundle_file_section]
        })
    }

    fn alloc_bundle_file_section(
        &self,
        bundle_file_section: BundleFileSection,
    ) -> Id<BundleFileSection> {
        let id = self
            .bundle_file_sections
            .borrow_mut()
            .alloc(bundle_file_section);
        id
    }

    #[track_caller]
    fn build_info(&self, build_info: Id<BuildInfo>) -> Ref<BuildInfo> {
        Ref::map(self.build_infos.borrow(), |build_infos| {
            &build_infos[build_info]
        })
    }

    fn alloc_build_info(&self, build_info: BuildInfo) -> Id<BuildInfo> {
        let id = self.build_infos.borrow_mut().alloc(build_info);
        id
    }

    #[track_caller]
    fn program_build_info(
        &self,
        program_build_info: Id<ProgramBuildInfo>,
    ) -> Ref<ProgramBuildInfo> {
        Ref::map(self.program_build_infos.borrow(), |program_build_infos| {
            &program_build_infos[program_build_info]
        })
    }

    fn alloc_program_build_info(
        &self,
        program_build_info: ProgramBuildInfo,
    ) -> Id<ProgramBuildInfo> {
        let id = self
            .program_build_infos
            .borrow_mut()
            .alloc(program_build_info);
        id
    }

    #[track_caller]
    fn bundle_build_info(&self, bundle_build_info: Id<BundleBuildInfo>) -> Ref<BundleBuildInfo> {
        Ref::map(self.bundle_build_infos.borrow(), |bundle_build_infos| {
            &bundle_build_infos[bundle_build_info]
        })
    }

    fn bundle_build_info_mut(
        &self,
        bundle_build_info: Id<BundleBuildInfo>,
    ) -> RefMut<BundleBuildInfo> {
        RefMut::map(self.bundle_build_infos.borrow_mut(), |bundle_build_infos| {
            &mut bundle_build_infos[bundle_build_info]
        })
    }

    fn alloc_bundle_build_info(&self, bundle_build_info: BundleBuildInfo) -> Id<BundleBuildInfo> {
        let id = self
            .bundle_build_infos
            .borrow_mut()
            .alloc(bundle_build_info);
        id
    }

    #[track_caller]
    fn bundle_file_info(&self, bundle_file_info: Id<BundleFileInfo>) -> Ref<BundleFileInfo> {
        Ref::map(self.bundle_file_infos.borrow(), |bundle_file_infos| {
            &bundle_file_infos[bundle_file_info]
        })
    }

    fn bundle_file_info_mut(&self, bundle_file_info: Id<BundleFileInfo>) -> RefMut<BundleFileInfo> {
        RefMut::map(self.bundle_file_infos.borrow_mut(), |bundle_file_infos| {
            &mut bundle_file_infos[bundle_file_info]
        })
    }

    fn alloc_bundle_file_info(&self, bundle_file_info: BundleFileInfo) -> Id<BundleFileInfo> {
        let id = self.bundle_file_infos.borrow_mut().alloc(bundle_file_info);
        id
    }

    #[track_caller]
    fn symbol_table(&self, symbol_table: Id<SymbolTable>) -> Ref<SymbolTable> {
        Ref::map(self.symbol_tables.borrow(), |symbol_tables| {
            &symbol_tables[symbol_table]
        })
    }

    fn symbol_table_mut(&self, symbol_table: Id<SymbolTable>) -> RefMut<SymbolTable> {
        RefMut::map(self.symbol_tables.borrow_mut(), |symbol_tables| {
            &mut symbol_tables[symbol_table]
        })
    }

    fn alloc_symbol_table(&self, symbol_table: SymbolTable) -> Id<SymbolTable> {
        let id = self.symbol_tables.borrow_mut().alloc(symbol_table);
        id
    }

    #[track_caller]
    fn inference_info(&self, inference_info: Id<InferenceInfo>) -> Ref<InferenceInfo> {
        Ref::map(self.inference_infos.borrow(), |inference_infos| {
            &inference_infos[inference_info]
        })
    }

    fn alloc_inference_info(&self, inference_info: InferenceInfo) -> Id<InferenceInfo> {
        let id = self.inference_infos.borrow_mut().alloc(inference_info);
        id
    }

    #[track_caller]
    fn sys_format_diagnostics_host(
        &self,
        sys_format_diagnostics_host: Id<SysFormatDiagnosticsHost>,
    ) -> Ref<SysFormatDiagnosticsHost> {
        Ref::map(
            self.sys_format_diagnostics_hosts.borrow(),
            |sys_format_diagnostics_hosts| {
                &sys_format_diagnostics_hosts[sys_format_diagnostics_host]
            },
        )
    }

    fn alloc_sys_format_diagnostics_host(
        &self,
        sys_format_diagnostics_host: SysFormatDiagnosticsHost,
    ) -> Id<SysFormatDiagnosticsHost> {
        let id = self
            .sys_format_diagnostics_hosts
            .borrow_mut()
            .alloc(sys_format_diagnostics_host);
        id
    }

    #[track_caller]
    fn class_lexical_environment(
        &self,
        class_lexical_environment: Id<ClassLexicalEnvironment>,
    ) -> Ref<ClassLexicalEnvironment> {
        Ref::map(
            self.class_lexical_environments.borrow(),
            |class_lexical_environments| &class_lexical_environments[class_lexical_environment],
        )
    }

    fn class_lexical_environment_mut(
        &self,
        class_lexical_environment: Id<ClassLexicalEnvironment>,
    ) -> RefMut<ClassLexicalEnvironment> {
        RefMut::map(
            self.class_lexical_environments.borrow_mut(),
            |class_lexical_environments| &mut class_lexical_environments[class_lexical_environment],
        )
    }

    fn alloc_class_lexical_environment(
        &self,
        class_lexical_environment: ClassLexicalEnvironment,
    ) -> Id<ClassLexicalEnvironment> {
        let id = self
            .class_lexical_environments
            .borrow_mut()
            .alloc(class_lexical_environment);
        id
    }

    #[track_caller]
    fn converted_loop_state(
        &self,
        converted_loop_state: Id<ConvertedLoopState>,
    ) -> Ref<ConvertedLoopState> {
        Ref::map(
            self.converted_loop_states.borrow(),
            |converted_loop_states| &converted_loop_states[converted_loop_state],
        )
    }

    fn converted_loop_state_mut(
        &self,
        converted_loop_state: Id<ConvertedLoopState>,
    ) -> RefMut<ConvertedLoopState> {
        RefMut::map(
            self.converted_loop_states.borrow_mut(),
            |converted_loop_states| &mut converted_loop_states[converted_loop_state],
        )
    }

    fn alloc_converted_loop_state(
        &self,
        converted_loop_state: ConvertedLoopState,
    ) -> Id<ConvertedLoopState> {
        let id = self
            .converted_loop_states
            .borrow_mut()
            .alloc(converted_loop_state);
        id
    }

    #[track_caller]
    fn emit_helper_text_callback(
        &self,
        emit_helper_text_callback: Id<Box<dyn EmitHelperTextCallback>>,
    ) -> Ref<Box<dyn EmitHelperTextCallback>> {
        Ref::map(
            self.emit_helper_text_callbacks.borrow(),
            |emit_helper_text_callbacks| &emit_helper_text_callbacks[emit_helper_text_callback],
        )
    }

    fn alloc_emit_helper_text_callback(
        &self,
        emit_helper_text_callback: Box<dyn EmitHelperTextCallback>,
    ) -> Id<Box<dyn EmitHelperTextCallback>> {
        let id = self
            .emit_helper_text_callbacks
            .borrow_mut()
            .alloc(emit_helper_text_callback);
        id
    }

    #[track_caller]
    fn conditional_root(&self, conditional_root: Id<ConditionalRoot>) -> Ref<ConditionalRoot> {
        Ref::map(self.conditional_roots.borrow(), |conditional_roots| {
            &conditional_roots[conditional_root]
        })
    }

    fn conditional_root_mut(
        &self,
        conditional_root: Id<ConditionalRoot>,
    ) -> RefMut<ConditionalRoot> {
        RefMut::map(self.conditional_roots.borrow_mut(), |conditional_roots| {
            &mut conditional_roots[conditional_root]
        })
    }

    fn alloc_conditional_root(&self, conditional_root: ConditionalRoot) -> Id<ConditionalRoot> {
        let id = self.conditional_roots.borrow_mut().alloc(conditional_root);
        id
    }

    #[track_caller]
    fn emit_node(&self, emit_node: Id<EmitNode>) -> Ref<EmitNode> {
        Ref::map(self.emit_nodes.borrow(), |emit_nodes| {
            &emit_nodes[emit_node]
        })
    }

    fn emit_node_mut(&self, emit_node: Id<EmitNode>) -> RefMut<EmitNode> {
        RefMut::map(self.emit_nodes.borrow_mut(), |emit_nodes| {
            &mut emit_nodes[emit_node]
        })
    }

    fn alloc_emit_node(&self, emit_node: EmitNode) -> Id<EmitNode> {
        let id = self.emit_nodes.borrow_mut().alloc(emit_node);
        id
    }

    #[track_caller]
    fn check_binary_expression(
        &self,
        check_binary_expression: Id<CheckBinaryExpression>,
    ) -> Ref<CheckBinaryExpression> {
        Ref::map(
            self.check_binary_expressions.borrow(),
            |check_binary_expressions| &check_binary_expressions[check_binary_expression],
        )
    }

    fn alloc_check_binary_expression(
        &self,
        check_binary_expression: CheckBinaryExpression,
    ) -> Id<CheckBinaryExpression> {
        let id = self
            .check_binary_expressions
            .borrow_mut()
            .alloc(check_binary_expression);
        id
    }

    #[track_caller]
    fn source_map_source(&self, source_map_source: Id<SourceMapSource>) -> Ref<SourceMapSource> {
        Ref::map(self.source_map_sources.borrow(), |source_map_sources| {
            &source_map_sources[source_map_source]
        })
    }

    fn alloc_source_map_source(&self, source_map_source: SourceMapSource) -> Id<SourceMapSource> {
        let id = self
            .source_map_sources
            .borrow_mut()
            .alloc(source_map_source);
        id
    }

    #[track_caller]
    fn outofband_variance_marker_handler(
        &self,
        outofband_variance_marker_handler: Id<Box<dyn OutofbandVarianceMarkerHandler>>,
    ) -> Ref<Box<dyn OutofbandVarianceMarkerHandler>> {
        Ref::map(
            self.outofband_variance_marker_handlers.borrow(),
            |outofband_variance_marker_handlers| {
                &outofband_variance_marker_handlers[outofband_variance_marker_handler]
            },
        )
    }

    fn alloc_outofband_variance_marker_handler(
        &self,
        outofband_variance_marker_handler: Box<dyn OutofbandVarianceMarkerHandler>,
    ) -> Id<Box<dyn OutofbandVarianceMarkerHandler>> {
        let id = self
            .outofband_variance_marker_handlers
            .borrow_mut()
            .alloc(outofband_variance_marker_handler);
        id
    }

    #[track_caller]
    fn bind_binary_expression_flow(
        &self,
        bind_binary_expression_flow: Id<BindBinaryExpressionFlow>,
    ) -> Ref<BindBinaryExpressionFlow> {
        Ref::map(
            self.bind_binary_expression_flows.borrow(),
            |bind_binary_expression_flows| {
                &bind_binary_expression_flows[bind_binary_expression_flow]
            },
        )
    }

    fn alloc_bind_binary_expression_flow(
        &self,
        bind_binary_expression_flow: BindBinaryExpressionFlow,
    ) -> Id<BindBinaryExpressionFlow> {
        let id = self
            .bind_binary_expression_flows
            .borrow_mut()
            .alloc(bind_binary_expression_flow);
        id
    }

    #[track_caller]
    fn type_checker(&self, type_checker: Id<TypeChecker>) -> Ref<TypeChecker> {
        Ref::map(self.type_checkers.borrow(), |type_checkers| {
            &type_checkers[type_checker]
        })
    }

    fn alloc_type_checker(&self, type_checker: TypeChecker) -> Id<TypeChecker> {
        let id = self.type_checkers.borrow_mut().alloc(type_checker);
        id
    }

    #[track_caller]
    fn read_file_callback(
        &self,
        read_file_callback: Id<Box<dyn ReadFileCallback>>,
    ) -> Ref<Box<dyn ReadFileCallback>> {
        Ref::map(self.read_file_callbacks.borrow(), |read_file_callbacks| {
            &read_file_callbacks[read_file_callback]
        })
    }

    fn alloc_read_file_callback(
        &self,
        read_file_callback: Box<dyn ReadFileCallback>,
    ) -> Id<Box<dyn ReadFileCallback>> {
        let id = self
            .read_file_callbacks
            .borrow_mut()
            .alloc(read_file_callback);
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
    fn get_source_file_ref(
        &self,
        get_source_file: Id<Box<dyn GetSourceFile>>,
    ) -> Ref<Box<dyn GetSourceFile>> {
        Ref::map(self.get_source_files.borrow(), |get_source_files| {
            &get_source_files[get_source_file]
        })
    }

    fn alloc_get_source_file(
        &self,
        get_source_file: Box<dyn GetSourceFile>,
    ) -> Id<Box<dyn GetSourceFile>> {
        let id = self.get_source_files.borrow_mut().alloc(get_source_file);
        id
    }

    #[track_caller]
    fn get_symlink_cache(
        &self,
        get_symlink_cache: Id<Box<dyn GetSymlinkCache>>,
    ) -> Ref<Box<dyn GetSymlinkCache>> {
        Ref::map(self.get_symlink_caches.borrow(), |get_symlink_caches| {
            &get_symlink_caches[get_symlink_cache]
        })
    }

    fn alloc_get_symlink_cache(
        &self,
        get_symlink_cache: Box<dyn GetSymlinkCache>,
    ) -> Id<Box<dyn GetSymlinkCache>> {
        let id = self
            .get_symlink_caches
            .borrow_mut()
            .alloc(get_symlink_cache);
        id
    }

    #[track_caller]
    fn emit_binary_expression(
        &self,
        emit_binary_expression: Id<EmitBinaryExpression>,
    ) -> Ref<EmitBinaryExpression> {
        Ref::map(
            self.emit_binary_expressions.borrow(),
            |emit_binary_expressions| &emit_binary_expressions[emit_binary_expression],
        )
    }

    fn alloc_emit_binary_expression(
        &self,
        emit_binary_expression: EmitBinaryExpression,
    ) -> Id<EmitBinaryExpression> {
        let id = self
            .emit_binary_expressions
            .borrow_mut()
            .alloc(emit_binary_expression);
        id
    }

    #[track_caller]
    fn relative_to_build_info(
        &self,
        relative_to_build_info: Id<Box<dyn RelativeToBuildInfo>>,
    ) -> Ref<Box<dyn RelativeToBuildInfo>> {
        Ref::map(
            self.relative_to_build_infos.borrow(),
            |relative_to_build_infos| &relative_to_build_infos[relative_to_build_info],
        )
    }

    fn alloc_relative_to_build_info(
        &self,
        relative_to_build_info: Box<dyn RelativeToBuildInfo>,
    ) -> Id<Box<dyn RelativeToBuildInfo>> {
        let id = self
            .relative_to_build_infos
            .borrow_mut()
            .alloc(relative_to_build_info);
        id
    }

    #[track_caller]
    fn print_handlers(
        &self,
        print_handlers: Id<Box<dyn PrintHandlers>>,
    ) -> Ref<Box<dyn PrintHandlers>> {
        Ref::map(self.print_handlers.borrow(), |print_handlers_| {
            &print_handlers_[print_handlers]
        })
    }

    fn alloc_print_handlers(
        &self,
        print_handlers: Box<dyn PrintHandlers>,
    ) -> Id<Box<dyn PrintHandlers>> {
        let id = self.print_handlers.borrow_mut().alloc(print_handlers);
        id
    }

    #[track_caller]
    fn get_resolved_project_references_ref(
        &self,
        get_resolved_project_references: Id<Box<dyn GetResolvedProjectReferences>>,
    ) -> Ref<Box<dyn GetResolvedProjectReferences>> {
        Ref::map(
            self.get_resolved_project_references.borrow(),
            |get_resolved_project_references_| {
                &get_resolved_project_references_[get_resolved_project_references]
            },
        )
    }

    fn alloc_get_resolved_project_references(
        &self,
        get_resolved_project_references: Box<dyn GetResolvedProjectReferences>,
    ) -> Id<Box<dyn GetResolvedProjectReferences>> {
        let id = self
            .get_resolved_project_references
            .borrow_mut()
            .alloc(get_resolved_project_references);
        id
    }

    #[track_caller]
    fn for_each_resolved_project_reference_ref(
        &self,
        for_each_resolved_project_reference: Id<Box<dyn ForEachResolvedProjectReference>>,
    ) -> Ref<Box<dyn ForEachResolvedProjectReference>> {
        Ref::map(
            self.for_each_resolved_project_references.borrow(),
            |for_each_resolved_project_references| {
                &for_each_resolved_project_references[for_each_resolved_project_reference]
            },
        )
    }

    fn alloc_for_each_resolved_project_reference(
        &self,
        for_each_resolved_project_reference: Box<dyn ForEachResolvedProjectReference>,
    ) -> Id<Box<dyn ForEachResolvedProjectReference>> {
        let id = self
            .for_each_resolved_project_references
            .borrow_mut()
            .alloc(for_each_resolved_project_reference);
        id
    }

    #[track_caller]
    fn compiler_host_like(
        &self,
        compiler_host_like: Id<Box<dyn CompilerHostLike>>,
    ) -> Ref<Box<dyn CompilerHostLike>> {
        Ref::map(self.compiler_host_likes.borrow(), |compiler_host_likes| {
            &compiler_host_likes[compiler_host_like]
        })
    }

    fn alloc_compiler_host_like(
        &self,
        compiler_host_like: Box<dyn CompilerHostLike>,
    ) -> Id<Box<dyn CompilerHostLike>> {
        let id = self
            .compiler_host_likes
            .borrow_mut()
            .alloc(compiler_host_like);
        id
    }

    #[track_caller]
    fn directory_structure_host(
        &self,
        directory_structure_host: Id<Box<dyn DirectoryStructureHost>>,
    ) -> Ref<Box<dyn DirectoryStructureHost>> {
        Ref::map(
            self.directory_structure_hosts.borrow(),
            |directory_structure_hosts| &directory_structure_hosts[directory_structure_host],
        )
    }

    fn alloc_directory_structure_host(
        &self,
        directory_structure_host: Box<dyn DirectoryStructureHost>,
    ) -> Id<Box<dyn DirectoryStructureHost>> {
        let id = self
            .directory_structure_hosts
            .borrow_mut()
            .alloc(directory_structure_host);
        id
    }

    #[track_caller]
    fn builder_program(
        &self,
        builder_program: Id<Box<dyn BuilderProgram>>,
    ) -> Ref<Box<dyn BuilderProgram>> {
        Ref::map(self.builder_programs.borrow(), |builder_programs| {
            &builder_programs[builder_program]
        })
    }

    fn alloc_builder_program(
        &self,
        builder_program: Box<dyn BuilderProgram>,
    ) -> Id<Box<dyn BuilderProgram>> {
        let id = self.builder_programs.borrow_mut().alloc(builder_program);
        id
    }

    #[track_caller]
    fn type_reference_directive_resolution_cache(
        &self,
        type_reference_directive_resolution_cache: Id<TypeReferenceDirectiveResolutionCache>,
    ) -> Ref<TypeReferenceDirectiveResolutionCache> {
        Ref::map(
            self.type_reference_directive_resolution_caches.borrow(),
            |type_reference_directive_resolution_caches| {
                &type_reference_directive_resolution_caches
                    [type_reference_directive_resolution_cache]
            },
        )
    }

    fn alloc_type_reference_directive_resolution_cache(
        &self,
        type_reference_directive_resolution_cache: TypeReferenceDirectiveResolutionCache,
    ) -> Id<TypeReferenceDirectiveResolutionCache> {
        let id = self
            .type_reference_directive_resolution_caches
            .borrow_mut()
            .alloc(type_reference_directive_resolution_cache);
        id
    }

    #[track_caller]
    fn module_resolution_cache(
        &self,
        module_resolution_cache: Id<ModuleResolutionCache>,
    ) -> Ref<ModuleResolutionCache> {
        Ref::map(
            self.module_resolution_caches.borrow(),
            |module_resolution_caches| &module_resolution_caches[module_resolution_cache],
        )
    }

    fn alloc_module_resolution_cache(
        &self,
        module_resolution_cache: ModuleResolutionCache,
    ) -> Id<ModuleResolutionCache> {
        let id = self
            .module_resolution_caches
            .borrow_mut()
            .alloc(module_resolution_cache);
        id
    }

    #[track_caller]
    fn parse_config_file_host(
        &self,
        parse_config_file_host: Id<Box<dyn ParseConfigFileHost>>,
    ) -> Ref<Box<dyn ParseConfigFileHost>> {
        Ref::map(
            self.parse_config_file_hosts.borrow(),
            |parse_config_file_hosts| &parse_config_file_hosts[parse_config_file_host],
        )
    }

    fn alloc_parse_config_file_host(
        &self,
        parse_config_file_host: Box<dyn ParseConfigFileHost>,
    ) -> Id<Box<dyn ParseConfigFileHost>> {
        let id = self
            .parse_config_file_hosts
            .borrow_mut()
            .alloc(parse_config_file_host);
        id
    }

    #[track_caller]
    fn file_preprocessing_diagnostics(
        &self,
        file_preprocessing_diagnostics: Id<FilePreprocessingDiagnostics>,
    ) -> Ref<FilePreprocessingDiagnostics> {
        Ref::map(
            self.file_preprocessing_diagnostics.borrow(),
            |file_preprocessing_diagnostics_| {
                &file_preprocessing_diagnostics_[file_preprocessing_diagnostics]
            },
        )
    }

    fn alloc_file_preprocessing_diagnostics(
        &self,
        file_preprocessing_diagnostics: FilePreprocessingDiagnostics,
    ) -> Id<FilePreprocessingDiagnostics> {
        let id = self
            .file_preprocessing_diagnostics
            .borrow_mut()
            .alloc(file_preprocessing_diagnostics);
        id
    }

    #[track_caller]
    fn actual_resolve_module_names_worker(
        &self,
        actual_resolve_module_names_worker: Id<Box<dyn ActualResolveModuleNamesWorker>>,
    ) -> Ref<Box<dyn ActualResolveModuleNamesWorker>> {
        Ref::map(
            self.actual_resolve_module_names_workers.borrow(),
            |actual_resolve_module_names_workers| {
                &actual_resolve_module_names_workers[actual_resolve_module_names_worker]
            },
        )
    }

    fn alloc_actual_resolve_module_names_worker(
        &self,
        actual_resolve_module_names_worker: Box<dyn ActualResolveModuleNamesWorker>,
    ) -> Id<Box<dyn ActualResolveModuleNamesWorker>> {
        let id = self
            .actual_resolve_module_names_workers
            .borrow_mut()
            .alloc(actual_resolve_module_names_worker);
        id
    }

    #[track_caller]
    fn actual_resolve_type_reference_directive_names_worker(
        &self,
        actual_resolve_type_reference_directive_names_worker: Id<
            Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>,
        >,
    ) -> Ref<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>> {
        Ref::map(
            self.actual_resolve_type_reference_directive_names_workers
                .borrow(),
            |actual_resolve_type_reference_directive_names_workers| {
                &actual_resolve_type_reference_directive_names_workers
                    [actual_resolve_type_reference_directive_names_worker]
            },
        )
    }

    fn alloc_actual_resolve_type_reference_directive_names_worker(
        &self,
        actual_resolve_type_reference_directive_names_worker: Box<
            dyn ActualResolveTypeReferenceDirectiveNamesWorker,
        >,
    ) -> Id<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>> {
        let id = self
            .actual_resolve_type_reference_directive_names_workers
            .borrow_mut()
            .alloc(actual_resolve_type_reference_directive_names_worker);
        id
    }

    #[track_caller]
    fn get_program_build_info_ref(
        &self,
        get_program_build_info: Id<Box<dyn GetProgramBuildInfo>>,
    ) -> Ref<Box<dyn GetProgramBuildInfo>> {
        Ref::map(
            self.get_program_build_infos.borrow(),
            |get_program_build_infos| &get_program_build_infos[get_program_build_info],
        )
    }

    fn alloc_get_program_build_info(
        &self,
        get_program_build_info: Box<dyn GetProgramBuildInfo>,
    ) -> Id<Box<dyn GetProgramBuildInfo>> {
        let id = self
            .get_program_build_infos
            .borrow_mut()
            .alloc(get_program_build_info);
        id
    }

    #[track_caller]
    fn load_with_mode_aware_cache_loader(
        &self,
        load_with_mode_aware_cache_loader: Id<
            Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>,
        >,
    ) -> Ref<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>> {
        Ref::map(
            self.load_with_mode_aware_cache_loaders.borrow(),
            |load_with_mode_aware_cache_loaders| {
                &load_with_mode_aware_cache_loaders[load_with_mode_aware_cache_loader]
            },
        )
    }

    fn alloc_load_with_mode_aware_cache_loader(
        &self,
        load_with_mode_aware_cache_loader: Box<
            dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>,
        >,
    ) -> Id<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>> {
        let id = self
            .load_with_mode_aware_cache_loaders
            .borrow_mut()
            .alloc(load_with_mode_aware_cache_loader);
        id
    }

    #[track_caller]
    fn load_with_local_cache_loader(
        &self,
        load_with_local_cache_loader: Id<
            Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>,
        >,
    ) -> Ref<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>> {
        Ref::map(
            self.load_with_local_cache_loaders.borrow(),
            |load_with_local_cache_loaders| {
                &load_with_local_cache_loaders[load_with_local_cache_loader]
            },
        )
    }

    fn alloc_load_with_local_cache_loader(
        &self,
        load_with_local_cache_loader: Box<
            dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>,
        >,
    ) -> Id<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>> {
        let id = self
            .load_with_local_cache_loaders
            .borrow_mut()
            .alloc(load_with_local_cache_loader);
        id
    }

    #[track_caller]
    fn symbol_accessibility_diagnostic(
        &self,
        symbol_accessibility_diagnostic: Id<SymbolAccessibilityDiagnostic>,
    ) -> Ref<SymbolAccessibilityDiagnostic> {
        Ref::map(
            self.symbol_accessibility_diagnostics.borrow(),
            |symbol_accessibility_diagnostics| {
                &symbol_accessibility_diagnostics[symbol_accessibility_diagnostic]
            },
        )
    }

    fn alloc_symbol_accessibility_diagnostic(
        &self,
        symbol_accessibility_diagnostic: SymbolAccessibilityDiagnostic,
    ) -> Id<SymbolAccessibilityDiagnostic> {
        let id = self
            .symbol_accessibility_diagnostics
            .borrow_mut()
            .alloc(symbol_accessibility_diagnostic);
        id
    }

    #[track_caller]
    fn code_block(&self, code_block: Id<CodeBlock>) -> Ref<CodeBlock> {
        Ref::map(self.code_blocks.borrow(), |code_blocks| {
            &code_blocks[code_block]
        })
    }

    fn code_block_mut(&self, code_block: Id<CodeBlock>) -> RefMut<CodeBlock> {
        RefMut::map(self.code_blocks.borrow_mut(), |code_blocks| {
            &mut code_blocks[code_block]
        })
    }

    fn alloc_code_block(&self, code_block: CodeBlock) -> Id<CodeBlock> {
        let id = self.code_blocks.borrow_mut().alloc(code_block);
        id
    }

    #[track_caller]
    fn private_identifier_environment(
        &self,
        private_identifier_environment: Id<PrivateIdentifierEnvironment>,
    ) -> Ref<PrivateIdentifierEnvironment> {
        Ref::map(
            self.private_identifier_environments.borrow(),
            |private_identifier_environments| {
                &private_identifier_environments[private_identifier_environment]
            },
        )
    }

    fn private_identifier_environment_mut(
        &self,
        private_identifier_environment: Id<PrivateIdentifierEnvironment>,
    ) -> RefMut<PrivateIdentifierEnvironment> {
        RefMut::map(
            self.private_identifier_environments.borrow_mut(),
            |private_identifier_environments| {
                &mut private_identifier_environments[private_identifier_environment]
            },
        )
    }

    fn alloc_private_identifier_environment(
        &self,
        private_identifier_environment: PrivateIdentifierEnvironment,
    ) -> Id<PrivateIdentifierEnvironment> {
        let id = self
            .private_identifier_environments
            .borrow_mut()
            .alloc(private_identifier_environment);
        id
    }

    #[track_caller]
    fn private_identifier_info(
        &self,
        private_identifier_info: Id<PrivateIdentifierInfo>,
    ) -> Ref<PrivateIdentifierInfo> {
        Ref::map(
            self.private_identifier_infos.borrow(),
            |private_identifier_infos| &private_identifier_infos[private_identifier_info],
        )
    }

    fn private_identifier_info_mut(
        &self,
        private_identifier_info: Id<PrivateIdentifierInfo>,
    ) -> RefMut<PrivateIdentifierInfo> {
        RefMut::map(
            self.private_identifier_infos.borrow_mut(),
            |private_identifier_infos| &mut private_identifier_infos[private_identifier_info],
        )
    }

    fn alloc_private_identifier_info(
        &self,
        private_identifier_info: PrivateIdentifierInfo,
    ) -> Id<PrivateIdentifierInfo> {
        let id = self
            .private_identifier_infos
            .borrow_mut()
            .alloc(private_identifier_info);
        id
    }

    #[track_caller]
    fn external_module_info(
        &self,
        external_module_info: Id<ExternalModuleInfo>,
    ) -> Ref<ExternalModuleInfo> {
        Ref::map(
            self.external_module_infos.borrow(),
            |external_module_infos| &external_module_infos[external_module_info],
        )
    }

    fn alloc_external_module_info(
        &self,
        external_module_info: ExternalModuleInfo,
    ) -> Id<ExternalModuleInfo> {
        let id = self
            .external_module_infos
            .borrow_mut()
            .alloc(external_module_info);
        id
    }

    #[track_caller]
    fn resolved_module_with_failed_lookup_locations(
        &self,
        resolved_module_with_failed_lookup_locations: Id<ResolvedModuleWithFailedLookupLocations>,
    ) -> Ref<ResolvedModuleWithFailedLookupLocations> {
        Ref::map(
            self.resolved_modules_with_failed_lookup_locations.borrow(),
            |resolved_modules_with_failed_lookup_locations| {
                &resolved_modules_with_failed_lookup_locations
                    [resolved_module_with_failed_lookup_locations]
            },
        )
    }

    fn alloc_resolved_module_with_failed_lookup_locations(
        &self,
        resolved_module_with_failed_lookup_locations: ResolvedModuleWithFailedLookupLocations,
    ) -> Id<ResolvedModuleWithFailedLookupLocations> {
        let id = self
            .resolved_modules_with_failed_lookup_locations
            .borrow_mut()
            .alloc(resolved_module_with_failed_lookup_locations);
        id
    }

    #[track_caller]
    fn resolved_type_reference_directive_with_failed_lookup_locations(
        &self,
        resolved_type_reference_directive_with_failed_lookup_locations: Id<
            ResolvedTypeReferenceDirectiveWithFailedLookupLocations,
        >,
    ) -> Ref<ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
        Ref::map(
            self.resolved_type_reference_directives_with_failed_lookup_locations
                .borrow(),
            |resolved_type_reference_directives_with_failed_lookup_locations| {
                &resolved_type_reference_directives_with_failed_lookup_locations
                    [resolved_type_reference_directive_with_failed_lookup_locations]
            },
        )
    }

    fn alloc_resolved_type_reference_directive_with_failed_lookup_locations(
        &self,
        resolved_type_reference_directive_with_failed_lookup_locations: ResolvedTypeReferenceDirectiveWithFailedLookupLocations,
    ) -> Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
        let id = self
            .resolved_type_reference_directives_with_failed_lookup_locations
            .borrow_mut()
            .alloc(resolved_type_reference_directive_with_failed_lookup_locations);
        id
    }

    #[track_caller]
    fn package_json_info_cache(
        &self,
        package_json_info_cache: Id<Box<dyn PackageJsonInfoCache>>,
    ) -> Ref<Box<dyn PackageJsonInfoCache>> {
        Ref::map(
            self.package_json_info_caches.borrow(),
            |package_json_info_caches| &package_json_info_caches[package_json_info_cache],
        )
    }

    fn alloc_package_json_info_cache(
        &self,
        package_json_info_cache: Box<dyn PackageJsonInfoCache>,
    ) -> Id<Box<dyn PackageJsonInfoCache>> {
        let id = self
            .package_json_info_caches
            .borrow_mut()
            .alloc(package_json_info_cache);
        id
    }

    #[track_caller]
    fn mode_aware_cache_resolved_module_with_failed_lookup_locations(
        &self,
        mode_aware_cache_resolved_module_with_failed_lookup_locations: Id<
            ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>,
        >,
    ) -> Ref<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>> {
        Ref::map(
            self.mode_aware_cache_resolved_module_with_failed_lookup_locations
                .borrow(),
            |mode_aware_cache_resolved_module_with_failed_lookup_locations_| {
                &mode_aware_cache_resolved_module_with_failed_lookup_locations_
                    [mode_aware_cache_resolved_module_with_failed_lookup_locations]
            },
        )
    }

    fn alloc_mode_aware_cache_resolved_module_with_failed_lookup_locations(
        &self,
        mode_aware_cache_resolved_module_with_failed_lookup_locations: ModeAwareCache<
            Id<ResolvedModuleWithFailedLookupLocations>,
        >,
    ) -> Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>> {
        let id = self
            .mode_aware_cache_resolved_module_with_failed_lookup_locations
            .borrow_mut()
            .alloc(mode_aware_cache_resolved_module_with_failed_lookup_locations);
        id
    }

    #[track_caller]
    fn mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(
        &self,
        mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations: Id<
            ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
        >,
    ) -> Ref<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>> {
        Ref::map(
            self.mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations
                .borrow(),
            |mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_| {
                &mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_[mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations]
            },
        )
    }

    fn alloc_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(
        &self,
        mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations: ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
    ) -> Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>> {
        let id = self
            .mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations
            .borrow_mut()
            .alloc(mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations);
        id
    }

    #[track_caller]
    fn per_module_name_cache(
        &self,
        per_module_name_cache: Id<PerModuleNameCache>,
    ) -> Ref<PerModuleNameCache> {
        Ref::map(
            self.per_module_name_caches.borrow(),
            |per_module_name_caches| &per_module_name_caches[per_module_name_cache],
        )
    }

    fn alloc_per_module_name_cache(
        &self,
        per_module_name_cache: PerModuleNameCache,
    ) -> Id<PerModuleNameCache> {
        let id = self
            .per_module_name_caches
            .borrow_mut()
            .alloc(per_module_name_cache);
        id
    }

    #[track_caller]
    fn vec_diagnostic(&self, vec_diagnostic: Id<Vec<Id<Diagnostic>>>) -> Ref<Vec<Id<Diagnostic>>> {
        Ref::map(self.vec_diagnostics.borrow(), |vec_diagnostics| {
            &vec_diagnostics[vec_diagnostic]
        })
    }

    fn vec_diagnostic_mut(
        &self,
        vec_diagnostic: Id<Vec<Id<Diagnostic>>>,
    ) -> RefMut<Vec<Id<Diagnostic>>> {
        RefMut::map(self.vec_diagnostics.borrow_mut(), |vec_diagnostics| {
            &mut vec_diagnostics[vec_diagnostic]
        })
    }

    fn alloc_vec_diagnostic(&self, vec_diagnostic: Vec<Id<Diagnostic>>) -> Id<Vec<Id<Diagnostic>>> {
        let id = self.vec_diagnostics.borrow_mut().alloc(vec_diagnostic);
        id
    }

    #[track_caller]
    fn file_reasons(
        &self,
        file_reasons: Id<MultiMap<Path, Id<FileIncludeReason>>>,
    ) -> Ref<MultiMap<Path, Id<FileIncludeReason>>> {
        Ref::map(self.file_reasons.borrow(), |file_reasons_| {
            &file_reasons_[file_reasons]
        })
    }

    fn file_reasons_mut(
        &self,
        file_reasons: Id<MultiMap<Path, Id<FileIncludeReason>>>,
    ) -> RefMut<MultiMap<Path, Id<FileIncludeReason>>> {
        RefMut::map(self.file_reasons.borrow_mut(), |file_reasons_| {
            &mut file_reasons_[file_reasons]
        })
    }

    fn alloc_file_reasons(
        &self,
        file_reasons: MultiMap<Path, Id<FileIncludeReason>>,
    ) -> Id<MultiMap<Path, Id<FileIncludeReason>>> {
        let id = self.file_reasons.borrow_mut().alloc(file_reasons);
        id
    }

    #[track_caller]
    fn get_symbol_accessibility_diagnostic_interface(
        &self,
        get_symbol_accessibility_diagnostic_interface: Id<
            Box<dyn GetSymbolAccessibilityDiagnosticInterface>,
        >,
    ) -> Ref<Box<dyn GetSymbolAccessibilityDiagnosticInterface>> {
        Ref::map(
            self.get_symbol_accessibility_diagnostic_interfaces.borrow(),
            |get_symbol_accessibility_diagnostic_interfaces| {
                &get_symbol_accessibility_diagnostic_interfaces
                    [get_symbol_accessibility_diagnostic_interface]
            },
        )
    }

    fn alloc_get_symbol_accessibility_diagnostic_interface(
        &self,
        get_symbol_accessibility_diagnostic_interface: Box<
            dyn GetSymbolAccessibilityDiagnosticInterface,
        >,
    ) -> Id<Box<dyn GetSymbolAccessibilityDiagnosticInterface>> {
        let id = self
            .get_symbol_accessibility_diagnostic_interfaces
            .borrow_mut()
            .alloc(get_symbol_accessibility_diagnostic_interface);
        id
    }

    #[track_caller]
    fn option_vec_node(
        &self,
        option_vec_node: Id<Option<Vec<Id<Node>>>>,
    ) -> Ref<Option<Vec<Id<Node>>>> {
        Ref::map(self.option_vec_nodes.borrow(), |option_vec_nodes| {
            &option_vec_nodes[option_vec_node]
        })
    }

    fn option_vec_node_mut(
        &self,
        option_vec_node: Id<Option<Vec<Id<Node>>>>,
    ) -> RefMut<Option<Vec<Id<Node>>>> {
        RefMut::map(self.option_vec_nodes.borrow_mut(), |option_vec_nodes| {
            &mut option_vec_nodes[option_vec_node]
        })
    }

    fn alloc_option_vec_node(
        &self,
        option_vec_node: Option<Vec<Id<Node>>>,
    ) -> Id<Option<Vec<Id<Node>>>> {
        let id = self.option_vec_nodes.borrow_mut().alloc(option_vec_node);
        id
    }

    #[track_caller]
    fn vec_pending_declaration(
        &self,
        vec_pending_declaration: Id<Vec<PendingDeclaration>>,
    ) -> Ref<Vec<PendingDeclaration>> {
        Ref::map(
            self.vec_pending_declarations.borrow(),
            |vec_pending_declarations| &vec_pending_declarations[vec_pending_declaration],
        )
    }

    fn vec_pending_declaration_mut(
        &self,
        vec_pending_declaration: Id<Vec<PendingDeclaration>>,
    ) -> RefMut<Vec<PendingDeclaration>> {
        RefMut::map(
            self.vec_pending_declarations.borrow_mut(),
            |vec_pending_declarations| &mut vec_pending_declarations[vec_pending_declaration],
        )
    }

    fn alloc_vec_pending_declaration(
        &self,
        vec_pending_declaration: Vec<PendingDeclaration>,
    ) -> Id<Vec<PendingDeclaration>> {
        let id = self
            .vec_pending_declarations
            .borrow_mut()
            .alloc(vec_pending_declaration);
        id
    }

    #[track_caller]
    fn package_json_info(&self, package_json_info: Id<PackageJsonInfo>) -> Ref<PackageJsonInfo> {
        Ref::map(self.package_json_infos.borrow(), |package_json_infos| {
            &package_json_infos[package_json_info]
        })
    }

    fn alloc_package_json_info(&self, package_json_info: PackageJsonInfo) -> Id<PackageJsonInfo> {
        let id = self
            .package_json_infos
            .borrow_mut()
            .alloc(package_json_info);
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
    fn pattern_ambient_module(
        &self,
        pattern_ambient_module: Id<PatternAmbientModule>,
    ) -> Ref<PatternAmbientModule> {
        Ref::map(
            self.pattern_ambient_modules.borrow(),
            |pattern_ambient_modules| &pattern_ambient_modules[pattern_ambient_module],
        )
    }

    fn alloc_pattern_ambient_module(
        &self,
        pattern_ambient_module: PatternAmbientModule,
    ) -> Id<PatternAmbientModule> {
        let id = self
            .pattern_ambient_modules
            .borrow_mut()
            .alloc(pattern_ambient_module);
        id
    }

    #[track_caller]
    fn check_type_containing_message_chain(
        &self,
        check_type_containing_message_chain: Id<Box<dyn CheckTypeContainingMessageChain>>,
    ) -> Ref<Box<dyn CheckTypeContainingMessageChain>> {
        Ref::map(
            self.check_type_containing_message_chains.borrow(),
            |check_type_containing_message_chains| {
                &check_type_containing_message_chains[check_type_containing_message_chain]
            },
        )
    }

    fn alloc_check_type_containing_message_chain(
        &self,
        check_type_containing_message_chain: Box<dyn CheckTypeContainingMessageChain>,
    ) -> Id<Box<dyn CheckTypeContainingMessageChain>> {
        let id = self
            .check_type_containing_message_chains
            .borrow_mut()
            .alloc(check_type_containing_message_chain);
        id
    }

    #[track_caller]
    fn check_type_error_output_container(
        &self,
        check_type_error_output_container: Id<Box<dyn CheckTypeErrorOutputContainer>>,
    ) -> Ref<Box<dyn CheckTypeErrorOutputContainer>> {
        Ref::map(
            self.check_type_error_output_containers.borrow(),
            |check_type_error_output_containers| {
                &check_type_error_output_containers[check_type_error_output_container]
            },
        )
    }

    fn alloc_check_type_error_output_container(
        &self,
        check_type_error_output_container: Box<dyn CheckTypeErrorOutputContainer>,
    ) -> Id<Box<dyn CheckTypeErrorOutputContainer>> {
        let id = self
            .check_type_error_output_containers
            .borrow_mut()
            .alloc(check_type_error_output_container);
        id
    }

    #[track_caller]
    fn resolved_type_reference_directives_map(
        &self,
        resolved_type_reference_directives_map: Id<
            HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>,
        >,
    ) -> Ref<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        Ref::map(
            self.resolved_type_reference_directives_maps.borrow(),
            |resolved_type_reference_directives_maps| {
                &resolved_type_reference_directives_maps[resolved_type_reference_directives_map]
            },
        )
    }

    fn resolved_type_reference_directives_map_mut(
        &self,
        resolved_type_reference_directives_map: Id<
            HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>,
        >,
    ) -> RefMut<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        RefMut::map(
            self.resolved_type_reference_directives_maps.borrow_mut(),
            |resolved_type_reference_directives_maps| {
                &mut resolved_type_reference_directives_maps[resolved_type_reference_directives_map]
            },
        )
    }

    fn alloc_resolved_type_reference_directives_map(
        &self,
        resolved_type_reference_directives_map: HashMap<
            String,
            Option<Id<ResolvedTypeReferenceDirective>>,
        >,
    ) -> Id<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        let id = self
            .resolved_type_reference_directives_maps
            .borrow_mut()
            .alloc(resolved_type_reference_directives_map);
        id
    }

    #[track_caller]
    fn node_builder(&self, node_builder: Id<NodeBuilder>) -> Ref<NodeBuilder> {
        Ref::map(self.node_builders.borrow(), |node_builders| {
            &node_builders[node_builder]
        })
    }

    fn alloc_node_builder(&self, node_builder: NodeBuilder) -> Id<NodeBuilder> {
        let id = self.node_builders.borrow_mut().alloc(node_builder);
        id.ref_(self).set_arena_id(id);
        id
    }

    #[track_caller]
    fn node_builder_context(
        &self,
        node_builder_context: Id<NodeBuilderContext>,
    ) -> Ref<NodeBuilderContext> {
        Ref::map(
            self.node_builder_contexts.borrow(),
            |node_builder_contexts| &node_builder_contexts[node_builder_context],
        )
    }

    fn alloc_node_builder_context(
        &self,
        node_builder_context: NodeBuilderContext,
    ) -> Id<NodeBuilderContext> {
        let id = self
            .node_builder_contexts
            .borrow_mut()
            .alloc(node_builder_context);
        id.ref_(self).set_arena_id(id);
        id
    }

    #[track_caller]
    fn option_vec_type(
        &self,
        option_vec_type: Id<Option<Vec<Id<Type>>>>,
    ) -> Ref<Option<Vec<Id<Type>>>> {
        Ref::map(self.option_vec_types.borrow(), |option_vec_types| {
            &option_vec_types[option_vec_type]
        })
    }

    fn option_vec_type_mut(
        &self,
        option_vec_type: Id<Option<Vec<Id<Type>>>>,
    ) -> RefMut<Option<Vec<Id<Type>>>> {
        RefMut::map(self.option_vec_types.borrow_mut(), |option_vec_types| {
            &mut option_vec_types[option_vec_type]
        })
    }

    fn alloc_option_vec_type(
        &self,
        option_vec_type: Option<Vec<Id<Type>>>,
    ) -> Id<Option<Vec<Id<Type>>>> {
        let id = self.option_vec_types.borrow_mut().alloc(option_vec_type);
        id
    }

    #[track_caller]
    fn option_type_parameter_names(
        &self,
        option_type_parameter_names: Id<Option<HashMap<TypeId, Id<Node>>>>,
    ) -> Ref<Option<HashMap<TypeId, Id<Node>>>> {
        Ref::map(
            self.option_type_parameter_names.borrow(),
            |option_type_parameter_names_| {
                &option_type_parameter_names_[option_type_parameter_names]
            },
        )
    }

    fn option_type_parameter_names_mut(
        &self,
        option_type_parameter_names: Id<Option<HashMap<TypeId, Id<Node>>>>,
    ) -> RefMut<Option<HashMap<TypeId, Id<Node>>>> {
        RefMut::map(
            self.option_type_parameter_names.borrow_mut(),
            |option_type_parameter_names_| {
                &mut option_type_parameter_names_[option_type_parameter_names]
            },
        )
    }

    fn alloc_option_type_parameter_names(
        &self,
        option_type_parameter_names: Option<HashMap<TypeId, Id<Node>>>,
    ) -> Id<Option<HashMap<TypeId, Id<Node>>>> {
        let id = self
            .option_type_parameter_names
            .borrow_mut()
            .alloc(option_type_parameter_names);
        id
    }

    #[track_caller]
    fn option_vec_symbol(
        &self,
        option_vec_symbol: Id<Option<Vec<Id<Symbol>>>>,
    ) -> Ref<Option<Vec<Id<Symbol>>>> {
        Ref::map(self.option_vec_symbols.borrow(), |option_vec_symbols| {
            &option_vec_symbols[option_vec_symbol]
        })
    }

    fn option_vec_symbol_mut(
        &self,
        option_vec_symbol: Id<Option<Vec<Id<Symbol>>>>,
    ) -> RefMut<Option<Vec<Id<Symbol>>>> {
        RefMut::map(self.option_vec_symbols.borrow_mut(), |option_vec_symbols| {
            &mut option_vec_symbols[option_vec_symbol]
        })
    }

    fn alloc_option_vec_symbol(
        &self,
        option_vec_symbol: Option<Vec<Id<Symbol>>>,
    ) -> Id<Option<Vec<Id<Symbol>>>> {
        let id = self
            .option_vec_symbols
            .borrow_mut()
            .alloc(option_vec_symbol);
        id
    }

    #[track_caller]
    fn type_comparer(
        &self,
        type_comparer: Id<Box<dyn TypeComparer>>,
    ) -> Ref<Box<dyn TypeComparer>> {
        Ref::map(self.type_comparers.borrow(), |type_comparers| {
            &type_comparers[type_comparer]
        })
    }

    fn alloc_type_comparer(
        &self,
        type_comparer: Box<dyn TypeComparer>,
    ) -> Id<Box<dyn TypeComparer>> {
        let id = self.type_comparers.borrow_mut().alloc(type_comparer);
        id
    }

    #[track_caller]
    fn inference_context(&self, inference_context: Id<InferenceContext>) -> Ref<InferenceContext> {
        Ref::map(self.inference_contexts.borrow(), |inference_contexts| {
            &inference_contexts[inference_context]
        })
    }

    fn alloc_inference_context(&self, inference_context: InferenceContext) -> Id<InferenceContext> {
        let id = self
            .inference_contexts
            .borrow_mut()
            .alloc(inference_context);
        id
    }

    #[track_caller]
    fn skip_trivia(&self, skip_trivia: Id<Box<dyn SkipTrivia>>) -> Ref<Box<dyn SkipTrivia>> {
        Ref::map(self.skip_trivias.borrow(), |skip_trivias| {
            &skip_trivias[skip_trivia]
        })
    }

    fn alloc_skip_trivia(&self, skip_trivia: Box<dyn SkipTrivia>) -> Id<Box<dyn SkipTrivia>> {
        let id = self.skip_trivias.borrow_mut().alloc(skip_trivia);
        id
    }

    #[track_caller]
    fn custom_transformer_factory_interface(
        &self,
        custom_transformer_factory_interface: Id<Box<dyn CustomTransformerFactoryInterface>>,
    ) -> Ref<Box<dyn CustomTransformerFactoryInterface>> {
        Ref::map(
            self.custom_transformer_factory_interfaces.borrow(),
            |custom_transformer_factory_interfaces| {
                &custom_transformer_factory_interfaces[custom_transformer_factory_interface]
            },
        )
    }

    fn alloc_custom_transformer_factory_interface(
        &self,
        custom_transformer_factory_interface: Box<dyn CustomTransformerFactoryInterface>,
    ) -> Id<Box<dyn CustomTransformerFactoryInterface>> {
        let id = self
            .custom_transformer_factory_interfaces
            .borrow_mut()
            .alloc(custom_transformer_factory_interface);
        id
    }

    #[track_caller]
    fn custom_transformer_interface(
        &self,
        custom_transformer_interface: Id<Box<dyn CustomTransformerInterface>>,
    ) -> Ref<Box<dyn CustomTransformerInterface>> {
        Ref::map(
            self.custom_transformer_interfaces.borrow(),
            |custom_transformer_interfaces| {
                &custom_transformer_interfaces[custom_transformer_interface]
            },
        )
    }

    fn alloc_custom_transformer_interface(
        &self,
        custom_transformer_interface: Box<dyn CustomTransformerInterface>,
    ) -> Id<Box<dyn CustomTransformerInterface>> {
        let id = self
            .custom_transformer_interfaces
            .borrow_mut()
            .alloc(custom_transformer_interface);
        id
    }

    #[track_caller]
    fn node_links(&self, node_links: Id<NodeLinks>) -> Ref<NodeLinks> {
        Ref::map(self.node_links.borrow(), |node_links_| {
            &node_links_[node_links]
        })
    }

    fn node_links_mut(&self, node_links: Id<NodeLinks>) -> RefMut<NodeLinks> {
        RefMut::map(self.node_links.borrow_mut(), |node_links_| {
            &mut node_links_[node_links]
        })
    }

    fn alloc_node_links(&self, node_links: NodeLinks) -> Id<NodeLinks> {
        let id = self.node_links.borrow_mut().alloc(node_links);
        id
    }

    #[track_caller]
    fn parser(&self, parser: Id<ParserType>) -> Ref<ParserType> {
        Ref::map(self.parsers.borrow(), |parsers| &parsers[parser])
    }

    fn alloc_parser(&self, parser: ParserType) -> Id<ParserType> {
        let id = self.parsers.borrow_mut().alloc(parser);
        id
    }

    #[track_caller]
    fn incremental_parser_syntax_cursor(
        &self,
        incremental_parser_syntax_cursor: Id<IncrementalParserSyntaxCursor>,
    ) -> Ref<IncrementalParserSyntaxCursor> {
        Ref::map(
            self.incremental_parser_syntax_cursors.borrow(),
            |incremental_parser_syntax_cursors| {
                &incremental_parser_syntax_cursors[incremental_parser_syntax_cursor]
            },
        )
    }

    fn alloc_incremental_parser_syntax_cursor(
        &self,
        incremental_parser_syntax_cursor: IncrementalParserSyntaxCursor,
    ) -> Id<IncrementalParserSyntaxCursor> {
        let id = self
            .incremental_parser_syntax_cursors
            .borrow_mut()
            .alloc(incremental_parser_syntax_cursor);
        id
    }

    #[track_caller]
    fn command_line_option(
        &self,
        command_line_option: Id<CommandLineOption>,
    ) -> Ref<CommandLineOption> {
        Ref::map(self.command_line_options.borrow(), |command_line_options| {
            &command_line_options[command_line_option]
        })
    }

    fn alloc_command_line_option(
        &self,
        command_line_option: CommandLineOption,
    ) -> Id<CommandLineOption> {
        let id = self
            .command_line_options
            .borrow_mut()
            .alloc(command_line_option);
        id.ref_(self).set_arena_id(id);
        id
    }

    #[track_caller]
    fn vec_command_line_option(
        &self,
        vec_command_line_option: Id<Vec<Id<CommandLineOption>>>,
    ) -> Ref<Vec<Id<CommandLineOption>>> {
        Ref::map(
            self.vec_command_line_options.borrow(),
            |vec_command_line_options| &vec_command_line_options[vec_command_line_option],
        )
    }

    fn alloc_vec_command_line_option(
        &self,
        vec_command_line_option: Vec<Id<CommandLineOption>>,
    ) -> Id<Vec<Id<CommandLineOption>>> {
        let id = self
            .vec_command_line_options
            .borrow_mut()
            .alloc(vec_command_line_option);
        id
    }

    #[track_caller]
    fn options_name_map(&self, options_name_map: Id<OptionsNameMap>) -> Ref<OptionsNameMap> {
        Ref::map(self.options_name_maps.borrow(), |options_name_maps| {
            &options_name_maps[options_name_map]
        })
    }

    fn alloc_options_name_map(&self, options_name_map: OptionsNameMap) -> Id<OptionsNameMap> {
        let id = self.options_name_maps.borrow_mut().alloc(options_name_map);
        id
    }

    #[track_caller]
    fn command_line_options_map(
        &self,
        command_line_options_map: Id<HashMap<String, Id<CommandLineOption>>>,
    ) -> Ref<HashMap<String, Id<CommandLineOption>>> {
        Ref::map(
            self.command_line_options_maps.borrow(),
            |command_line_options_maps| &command_line_options_maps[command_line_options_map],
        )
    }

    fn alloc_command_line_options_map(
        &self,
        command_line_options_map: HashMap<String, Id<CommandLineOption>>,
    ) -> Id<HashMap<String, Id<CommandLineOption>>> {
        let id = self
            .command_line_options_maps
            .borrow_mut()
            .alloc(command_line_options_map);
        id
    }

    #[track_caller]
    fn node_symbol_override(
        &self,
        node_symbol_override: Id<Box<dyn NodeSymbolOverride>>,
    ) -> Ref<Box<dyn NodeSymbolOverride>> {
        Ref::map(
            self.node_symbol_overrides.borrow(),
            |node_symbol_overrides| &node_symbol_overrides[node_symbol_override],
        )
    }

    fn alloc_node_symbol_override(
        &self,
        node_symbol_override: Box<dyn NodeSymbolOverride>,
    ) -> Id<Box<dyn NodeSymbolOverride>> {
        let id = self
            .node_symbol_overrides
            .borrow_mut()
            .alloc(node_symbol_override);
        id
    }

    #[track_caller]
    fn node_id_override(
        &self,
        node_id_override: Id<Box<dyn NodeIdOverride>>,
    ) -> Ref<Box<dyn NodeIdOverride>> {
        Ref::map(self.node_id_overrides.borrow(), |node_id_overrides| {
            &node_id_overrides[node_id_override]
        })
    }

    fn alloc_node_id_override(
        &self,
        node_id_override: Box<dyn NodeIdOverride>,
    ) -> Id<Box<dyn NodeIdOverride>> {
        let id = self.node_id_overrides.borrow_mut().alloc(node_id_override);
        id
    }

    #[track_caller]
    fn make_serialize_property_symbol_create_property(
        &self,
        make_serialize_property_symbol_create_property: Id<
            Box<dyn MakeSerializePropertySymbolCreateProperty>,
        >,
    ) -> Ref<Box<dyn MakeSerializePropertySymbolCreateProperty>> {
        Ref::map(
            self.make_serialize_property_symbol_create_properties
                .borrow(),
            |make_serialize_property_symbol_create_properties| {
                &make_serialize_property_symbol_create_properties
                    [make_serialize_property_symbol_create_property]
            },
        )
    }

    fn alloc_make_serialize_property_symbol_create_property(
        &self,
        make_serialize_property_symbol_create_property: Box<
            dyn MakeSerializePropertySymbolCreateProperty,
        >,
    ) -> Id<Box<dyn MakeSerializePropertySymbolCreateProperty>> {
        let id = self
            .make_serialize_property_symbol_create_properties
            .borrow_mut()
            .alloc(make_serialize_property_symbol_create_property);
        id
    }

    #[track_caller]
    fn symbol_table_to_declaration_statements(
        &self,
        symbol_table_to_declaration_statements: Id<SymbolTableToDeclarationStatements>,
    ) -> Ref<SymbolTableToDeclarationStatements> {
        Ref::map(
            self.symbol_table_to_declaration_statements.borrow(),
            |symbol_table_to_declaration_statements_| {
                &symbol_table_to_declaration_statements_[symbol_table_to_declaration_statements]
            },
        )
    }

    fn alloc_symbol_table_to_declaration_statements(
        &self,
        symbol_table_to_declaration_statements: SymbolTableToDeclarationStatements,
    ) -> Id<SymbolTableToDeclarationStatements> {
        let id = self
            .symbol_table_to_declaration_statements
            .borrow_mut()
            .alloc(symbol_table_to_declaration_statements);
        id.ref_(self).set_arena_id(id);
        id
    }

    #[track_caller]
    fn input_files_initialized_state(
        &self,
        input_files_initialized_state: Id<InputFilesInitializedState>,
    ) -> Ref<InputFilesInitializedState> {
        Ref::map(
            self.input_files_initialized_states.borrow(),
            |input_files_initialized_states| {
                &input_files_initialized_states[input_files_initialized_state]
            },
        )
    }

    fn alloc_input_files_initialized_state(
        &self,
        input_files_initialized_state: InputFilesInitializedState,
    ) -> Id<InputFilesInitializedState> {
        let id = self
            .input_files_initialized_states
            .borrow_mut()
            .alloc(input_files_initialized_state);
        id
    }

    #[track_caller]
    fn vec_symbol_table(
        &self,
        vec_symbol_table: Id<Vec<Id<SymbolTable>>>,
    ) -> Ref<Vec<Id<SymbolTable>>> {
        Ref::map(self.vec_symbol_tables.borrow(), |vec_symbol_tables| {
            &vec_symbol_tables[vec_symbol_table]
        })
    }

    fn vec_symbol_table_mut(
        &self,
        vec_symbol_table: Id<Vec<Id<SymbolTable>>>,
    ) -> RefMut<Vec<Id<SymbolTable>>> {
        RefMut::map(self.vec_symbol_tables.borrow_mut(), |vec_symbol_tables| {
            &mut vec_symbol_tables[vec_symbol_table]
        })
    }

    fn alloc_vec_symbol_table(
        &self,
        vec_symbol_table: Vec<Id<SymbolTable>>,
    ) -> Id<Vec<Id<SymbolTable>>> {
        let id = self.vec_symbol_tables.borrow_mut().alloc(vec_symbol_table);
        id
    }

    #[track_caller]
    fn check_type_related_to(
        &self,
        check_type_related_to: Id<CheckTypeRelatedTo>,
    ) -> Ref<CheckTypeRelatedTo> {
        Ref::map(
            self.check_type_related_tos.borrow(),
            |check_type_related_tos| &check_type_related_tos[check_type_related_to],
        )
    }

    fn alloc_check_type_related_to(
        &self,
        check_type_related_to: CheckTypeRelatedTo,
    ) -> Id<CheckTypeRelatedTo> {
        let id = self
            .check_type_related_tos
            .borrow_mut()
            .alloc(check_type_related_to);
        id.ref_(self).set_arena_id(id);
        id
    }

    #[track_caller]
    fn flow_loop_cache(
        &self,
        flow_loop_cache: Id<HashMap<String, Id<Type>>>,
    ) -> Ref<HashMap<String, Id<Type>>> {
        Ref::map(self.flow_loop_caches.borrow(), |flow_loop_caches| {
            &flow_loop_caches[flow_loop_cache]
        })
    }

    fn flow_loop_cache_mut(
        &self,
        flow_loop_cache: Id<HashMap<String, Id<Type>>>,
    ) -> RefMut<HashMap<String, Id<Type>>> {
        RefMut::map(self.flow_loop_caches.borrow_mut(), |flow_loop_caches| {
            &mut flow_loop_caches[flow_loop_cache]
        })
    }

    fn alloc_flow_loop_cache(
        &self,
        flow_loop_cache: HashMap<String, Id<Type>>,
    ) -> Id<HashMap<String, Id<Type>>> {
        let id = self.flow_loop_caches.borrow_mut().alloc(flow_loop_cache);
        id
    }

    #[track_caller]
    fn vec_symbol(&self, vec_symbol: Id<Vec<Id<Symbol>>>) -> Ref<Vec<Id<Symbol>>> {
        Ref::map(self.vec_symbols.borrow(), |vec_symbols| {
            &vec_symbols[vec_symbol]
        })
    }

    fn alloc_vec_symbol(&self, vec_symbol: Vec<Id<Symbol>>) -> Id<Vec<Id<Symbol>>> {
        let id = self.vec_symbols.borrow_mut().alloc(vec_symbol);
        id
    }

    #[track_caller]
    fn vec_node(&self, vec_node: Id<Vec<Id<Node>>>) -> Ref<Vec<Id<Node>>> {
        Ref::map(self.vec_nodes.borrow(), |vec_nodes| &vec_nodes[vec_node])
    }

    fn alloc_vec_node(&self, vec_node: Vec<Id<Node>>) -> Id<Vec<Id<Node>>> {
        let id = self.vec_nodes.borrow_mut().alloc(vec_node);
        id
    }

    #[track_caller]
    fn type_mapper_callback(
        &self,
        type_mapper_callback: Id<Box<dyn TypeMapperCallback>>,
    ) -> Ref<Box<dyn TypeMapperCallback>> {
        Ref::map(
            self.type_mapper_callbacks.borrow(),
            |type_mapper_callbacks| &type_mapper_callbacks[type_mapper_callback],
        )
    }

    fn alloc_type_mapper_callback(
        &self,
        type_mapper_callback: Box<dyn TypeMapperCallback>,
    ) -> Id<Box<dyn TypeMapperCallback>> {
        let id = self
            .type_mapper_callbacks
            .borrow_mut()
            .alloc(type_mapper_callback);
        id
    }

    #[track_caller]
    fn option_symbol_table(
        &self,
        option_symbol_table: Id<Option<Id<SymbolTable>>>,
    ) -> Ref<Option<Id<SymbolTable>>> {
        Ref::map(self.option_symbol_tables.borrow(), |option_symbol_tables| {
            &option_symbol_tables[option_symbol_table]
        })
    }

    fn option_symbol_table_mut(
        &self,
        option_symbol_table: Id<Option<Id<SymbolTable>>>,
    ) -> RefMut<Option<Id<SymbolTable>>> {
        RefMut::map(
            self.option_symbol_tables.borrow_mut(),
            |option_symbol_tables| &mut option_symbol_tables[option_symbol_table],
        )
    }

    fn alloc_option_symbol_table(
        &self,
        option_symbol_table: Option<Id<SymbolTable>>,
    ) -> Id<Option<Id<SymbolTable>>> {
        let id = self
            .option_symbol_tables
            .borrow_mut()
            .alloc(option_symbol_table);
        id
    }

    #[track_caller]
    fn cache_with_redirects_per_module_name_cache(
        &self,
        cache_with_redirects_per_module_name_cache: Id<CacheWithRedirects<PerModuleNameCache>>,
    ) -> Ref<CacheWithRedirects<PerModuleNameCache>> {
        Ref::map(
            self.cache_with_redirects_per_module_name_caches.borrow(),
            |cache_with_redirects_per_module_name_caches| {
                &cache_with_redirects_per_module_name_caches
                    [cache_with_redirects_per_module_name_cache]
            },
        )
    }

    fn alloc_cache_with_redirects_per_module_name_cache(
        &self,
        cache_with_redirects_per_module_name_cache: CacheWithRedirects<PerModuleNameCache>,
    ) -> Id<CacheWithRedirects<PerModuleNameCache>> {
        let id = self
            .cache_with_redirects_per_module_name_caches
            .borrow_mut()
            .alloc(cache_with_redirects_per_module_name_cache);
        id
    }

    #[track_caller]
    fn cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations(
        &self,
        cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations: Id<
            CacheWithRedirects<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>,
        >,
    ) -> Ref<CacheWithRedirects<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>> {
        Ref::map(
            self.cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations
                .borrow(),
            |cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations_| {
                &cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations_[cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations]
            },
        )
    }

    fn alloc_cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations(
        &self,
        cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations: CacheWithRedirects<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>,
    ) -> Id<CacheWithRedirects<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>> {
        let id = self
            .cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations
            .borrow_mut()
            .alloc(
                cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations,
            );
        id
    }

    #[track_caller]
    fn cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(
        &self,
        cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations: Id<CacheWithRedirects<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>>,
    ) -> Ref<
        CacheWithRedirects<
            ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
        >,
    > {
        Ref::map(self.cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations.borrow(), |cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_| &cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_[cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations])
    }

    fn alloc_cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(
        &self,
        cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations: CacheWithRedirects<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
    ) -> Id<
        CacheWithRedirects<
            ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
        >,
    > {
        let id = self.cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations.borrow_mut().alloc(cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations);
        id
    }

    #[track_caller]
    fn mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map(
        &self,
        mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map: Id<
            HashMap<
                String,
                Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
            >,
        >,
    ) -> Ref<
        HashMap<
            String,
            Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
        >,
    > {
        Ref::map(self.mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps.borrow(), |mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps| &mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps[mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map])
    }

    fn mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map_mut(
        &self,
        mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map: Id<
            HashMap<
                String,
                Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
            >,
        >,
    ) -> RefMut<
        HashMap<
            String,
            Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
        >,
    > {
        RefMut::map(
            self.mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps.borrow_mut(),
            |mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps| &mut mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps[mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map],
        )
    }

    fn alloc_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map(
        &self,
        mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map: HashMap<String, Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>>,
    ) -> Id<
        HashMap<
            String,
            Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
        >,
    > {
        let id = self
            .mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps
            .borrow_mut()
            .alloc(
                mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map,
            );
        id
    }

    #[track_caller]
    fn path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map(
        &self,
        path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map: Id<HashMap<Path, Id<HashMap<String, Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>>>>>,
    ) -> Ref<
        HashMap<
            Path,
            Id<
                HashMap<
                    String,
                    Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
                >,
            >,
        >,
    > {
        Ref::map(self.path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps.borrow(), |path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps| &path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps[path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map])
    }

    fn alloc_path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map(
        &self,
        path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map: HashMap<Path, Id<HashMap<String, Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>>>>,
    ) -> Id<
        HashMap<
            Path,
            Id<
                HashMap<
                    String,
                    Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
                >,
            >,
        >,
    > {
        let id = self.path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_maps.borrow_mut().alloc(path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map);
        id
    }

    #[track_caller]
    fn mode_aware_cache_resolved_module_with_failed_lookup_locations_map(
        &self,
        mode_aware_cache_resolved_module_with_failed_lookup_locations_map: Id<
            HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>,
        >,
    ) -> Ref<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>> {
        Ref::map(
            self.mode_aware_cache_resolved_module_with_failed_lookup_locations_maps
                .borrow(),
            |mode_aware_cache_resolved_module_with_failed_lookup_locations_maps| {
                &mode_aware_cache_resolved_module_with_failed_lookup_locations_maps
                    [mode_aware_cache_resolved_module_with_failed_lookup_locations_map]
            },
        )
    }

    fn mode_aware_cache_resolved_module_with_failed_lookup_locations_map_mut(
        &self,
        mode_aware_cache_resolved_module_with_failed_lookup_locations_map: Id<
            HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>,
        >,
    ) -> RefMut<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>>
    {
        RefMut::map(
            self.mode_aware_cache_resolved_module_with_failed_lookup_locations_maps
                .borrow_mut(),
            |mode_aware_cache_resolved_module_with_failed_lookup_locations_maps| {
                &mut mode_aware_cache_resolved_module_with_failed_lookup_locations_maps
                    [mode_aware_cache_resolved_module_with_failed_lookup_locations_map]
            },
        )
    }

    fn alloc_mode_aware_cache_resolved_module_with_failed_lookup_locations_map(
        &self,
        mode_aware_cache_resolved_module_with_failed_lookup_locations_map: HashMap<
            String,
            Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>,
        >,
    ) -> Id<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>> {
        let id = self
            .mode_aware_cache_resolved_module_with_failed_lookup_locations_maps
            .borrow_mut()
            .alloc(mode_aware_cache_resolved_module_with_failed_lookup_locations_map);
        id
    }

    #[track_caller]
    fn path_mode_aware_cache_resolved_module_with_failed_lookup_locations_map(
        &self,
        path_mode_aware_cache_resolved_module_with_failed_lookup_locations_map: Id<
            HashMap<
                Path,
                Id<
                    HashMap<
                        String,
                        Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>,
                    >,
                >,
            >,
        >,
    ) -> Ref<
        HashMap<
            Path,
            Id<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>>,
        >,
    > {
        Ref::map(
            self.path_mode_aware_cache_resolved_module_with_failed_lookup_locations_maps
                .borrow(),
            |path_mode_aware_cache_resolved_module_with_failed_lookup_locations_maps| {
                &path_mode_aware_cache_resolved_module_with_failed_lookup_locations_maps
                    [path_mode_aware_cache_resolved_module_with_failed_lookup_locations_map]
            },
        )
    }

    fn alloc_path_mode_aware_cache_resolved_module_with_failed_lookup_locations_map(
        &self,
        path_mode_aware_cache_resolved_module_with_failed_lookup_locations_map: HashMap<
            Path,
            Id<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>>,
        >,
    ) -> Id<
        HashMap<
            Path,
            Id<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>>,
        >,
    > {
        let id = self
            .path_mode_aware_cache_resolved_module_with_failed_lookup_locations_maps
            .borrow_mut()
            .alloc(path_mode_aware_cache_resolved_module_with_failed_lookup_locations_map);
        id
    }

    #[track_caller]
    fn per_module_name_cache_map(
        &self,
        per_module_name_cache_map: Id<HashMap<String, Id<PerModuleNameCache>>>,
    ) -> Ref<HashMap<String, Id<PerModuleNameCache>>> {
        Ref::map(
            self.per_module_name_cache_maps.borrow(),
            |per_module_name_cache_maps| &per_module_name_cache_maps[per_module_name_cache_map],
        )
    }

    fn per_module_name_cache_map_mut(
        &self,
        per_module_name_cache_map: Id<HashMap<String, Id<PerModuleNameCache>>>,
    ) -> RefMut<HashMap<String, Id<PerModuleNameCache>>> {
        RefMut::map(
            self.per_module_name_cache_maps.borrow_mut(),
            |per_module_name_cache_maps| &mut per_module_name_cache_maps[per_module_name_cache_map],
        )
    }

    fn alloc_per_module_name_cache_map(
        &self,
        per_module_name_cache_map: HashMap<String, Id<PerModuleNameCache>>,
    ) -> Id<HashMap<String, Id<PerModuleNameCache>>> {
        let id = self
            .per_module_name_cache_maps
            .borrow_mut()
            .alloc(per_module_name_cache_map);
        id
    }

    #[track_caller]
    fn path_per_module_name_cache_map(
        &self,
        path_per_module_name_cache_map: Id<
            HashMap<Path, Id<HashMap<String, Id<PerModuleNameCache>>>>,
        >,
    ) -> Ref<HashMap<Path, Id<HashMap<String, Id<PerModuleNameCache>>>>> {
        Ref::map(
            self.path_per_module_name_cache_maps.borrow(),
            |path_per_module_name_cache_maps| {
                &path_per_module_name_cache_maps[path_per_module_name_cache_map]
            },
        )
    }

    fn alloc_path_per_module_name_cache_map(
        &self,
        path_per_module_name_cache_map: HashMap<Path, Id<HashMap<String, Id<PerModuleNameCache>>>>,
    ) -> Id<HashMap<Path, Id<HashMap<String, Id<PerModuleNameCache>>>>> {
        let id = self
            .path_per_module_name_cache_maps
            .borrow_mut()
            .alloc(path_per_module_name_cache_map);
        id
    }

    #[track_caller]
    fn logging_host(&self, logging_host: Id<Box<dyn LoggingHost>>) -> Ref<Box<dyn LoggingHost>> {
        Ref::map(self.logging_hosts.borrow(), |logging_hosts| {
            &logging_hosts[logging_host]
        })
    }

    fn alloc_logging_host(&self, logging_host: Box<dyn LoggingHost>) -> Id<Box<dyn LoggingHost>> {
        let id = self.logging_hosts.borrow_mut().alloc(logging_host);
        id
    }

    #[track_caller]
    fn parse_command_line_worker_diagnostics(
        &self,
        parse_command_line_worker_diagnostics: Id<Box<dyn ParseCommandLineWorkerDiagnostics>>,
    ) -> Ref<Box<dyn ParseCommandLineWorkerDiagnostics>> {
        Ref::map(
            self.parse_command_line_worker_diagnostics.borrow(),
            |parse_command_line_worker_diagnostics_| {
                &parse_command_line_worker_diagnostics_[parse_command_line_worker_diagnostics]
            },
        )
    }

    fn alloc_parse_command_line_worker_diagnostics(
        &self,
        parse_command_line_worker_diagnostics: Box<dyn ParseCommandLineWorkerDiagnostics>,
    ) -> Id<Box<dyn ParseCommandLineWorkerDiagnostics>> {
        let id = self
            .parse_command_line_worker_diagnostics
            .borrow_mut()
            .alloc(parse_command_line_worker_diagnostics);
        id
    }

    #[track_caller]
    fn did_you_mean_options_diagnostics(
        &self,
        did_you_mean_options_diagnostics: Id<Box<dyn DidYouMeanOptionsDiagnostics>>,
    ) -> Ref<Box<dyn DidYouMeanOptionsDiagnostics>> {
        Ref::map(
            self.did_you_mean_options_diagnostics.borrow(),
            |did_you_mean_options_diagnostics_| {
                &did_you_mean_options_diagnostics_[did_you_mean_options_diagnostics]
            },
        )
    }

    fn alloc_did_you_mean_options_diagnostics(
        &self,
        did_you_mean_options_diagnostics: Box<dyn DidYouMeanOptionsDiagnostics>,
    ) -> Id<Box<dyn DidYouMeanOptionsDiagnostics>> {
        let id = self
            .did_you_mean_options_diagnostics
            .borrow_mut()
            .alloc(did_you_mean_options_diagnostics);
        id
    }
}
