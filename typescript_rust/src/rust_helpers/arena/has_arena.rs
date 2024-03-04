use std::collections::HashMap;

use debug_cell::{Ref, RefMut};
use id_arena::Id;

use crate::{
    ActiveLabel, ActualResolveModuleNamesWorker, ActualResolveTypeReferenceDirectiveNamesWorker,
    AllArenas, AllArenasId, BaseNodeFactory, BindBinaryExpressionFlow, Binder, BuildInfo,
    BuilderProgram, BundleBuildInfo, BundleFileInfo, BundleFileSection, CacheWithRedirects,
    CancellationToken, CheckBinaryExpression, CheckTypeContainingMessageChain,
    CheckTypeErrorOutputContainer, CheckTypeRelatedTo, ClassLexicalEnvironment, CodeBlock,
    CommandLineOption, CompilerHost, CompilerHostLike, CompilerOptions, ConditionalRoot,
    ConvertedLoopState, CurrentParenthesizerRule, CustomTransformerFactoryInterface,
    CustomTransformerInterface, Diagnostic, DiagnosticRelatedInformation, DiagnosticReporter,
    DidYouMeanOptionsDiagnostics, DirectoryStructureHost, EmitBinaryExpression, EmitHelper,
    EmitHelperFactory, EmitHelperTextCallback, EmitHost, EmitNode, EmitResolver, EmitTextWriter,
    ExternalModuleInfo, FileIncludeReason, FilePreprocessingDiagnostics, FlowNode,
    ForEachResolvedProjectReference, GetCanonicalFileName, GetProgramBuildInfo,
    GetResolvedProjectReferences, GetSourceFile, GetSymbolAccessibilityDiagnosticInterface,
    GetSymlinkCache, IncrementalParserSyntaxCursor, IndexInfo, InferenceContext, InferenceInfo,
    InputFilesInitializedState, IterationTypes, LoadWithLocalCacheLoader,
    LoadWithModeAwareCacheLoader, LoggingHost, MakeSerializePropertySymbolCreateProperty,
    ModeAwareCache, ModuleResolutionCache, ModuleResolutionHostOverrider,
    ModuleSpecifierResolutionHostAndGetCommonSourceDirectory, MultiMap, Node, NodeArray,
    NodeBuilder, NodeBuilderContext, NodeFactory, NodeIdOverride, NodeLinks, NodeSymbolOverride,
    OptionsNameMap, OutofbandVarianceMarkerHandler, PackageJsonInfo, PackageJsonInfoCache,
    ParenthesizerRules, ParseCommandLineWorkerDiagnostics, ParseConfigFileHost, ParsedCommandLine,
    ParserType, Path, PatternAmbientModule, PendingDeclaration, PerModuleNameCache, PrintHandlers,
    Printer, PrivateIdentifierEnvironment, PrivateIdentifierInfo, Program, ProgramBuildInfo,
    ReadFileCallback, RelativeToBuildInfo, ResolvedModuleFull,
    ResolvedModuleWithFailedLookupLocations, ResolvedProjectReference,
    ResolvedTypeReferenceDirective, ResolvedTypeReferenceDirectiveWithFailedLookupLocations,
    Signature, SkipTrivia, SourceMapGenerator, SourceMapRange, SourceMapSource, Symbol,
    SymbolAccessibilityDiagnostic, SymbolLinks, SymbolTable, SymbolTableToDeclarationStatements,
    SymbolTracker, SymlinkCache, SysFormatDiagnosticsHost, System, ToPath,
    TransformNodesTransformationResult, TransformationContextOnEmitNodeOverrider,
    TransformationContextOnSubstituteNodeOverrider, TransformerFactoryInterface,
    TransformerFactoryOrCustomTransformerFactory, TransformerInterface, Type, TypeChecker,
    TypeComparer, TypeId, TypeMapper, TypeMapperCallback, TypePredicate,
    TypeReferenceDirectiveResolutionCache, WrapCustomTransformerFactoryHandleDefault,
    WriteFileCallback,
};

pub trait HasArena {
    fn arena(&self) -> &AllArenas;

    fn all_arenas_id(&self) -> AllArenasId {
        self.arena().all_arenas_id()
    }

    #[track_caller]
    fn node(&self, node: Id<Node>) -> Ref<Node> {
        self.arena().node(node)
    }

    #[track_caller]
    fn alloc_node(&self, node: Node) -> Id<Node> {
        self.arena().alloc_node(node)
    }

    #[track_caller]
    fn type_ref(&self, type_: Id<Type>) -> Ref<Type> {
        self.arena().type_ref(type_)
    }

    #[track_caller]
    fn alloc_type(&self, type_: Type) -> Id<Type> {
        self.arena().alloc_type(type_)
    }

    #[track_caller]
    fn type_mapper(&self, type_mapper: Id<TypeMapper>) -> Ref<TypeMapper> {
        self.arena().type_mapper(type_mapper)
    }

    #[track_caller]
    fn alloc_type_mapper(&self, type_mapper: TypeMapper) -> Id<TypeMapper> {
        self.arena().alloc_type_mapper(type_mapper)
    }

    #[track_caller]
    fn symbol_ref(&self, symbol: Id<Symbol>) -> Ref<Symbol> {
        self.arena().symbol_ref(symbol)
    }

    #[track_caller]
    fn alloc_symbol(&self, symbol: Symbol) -> Id<Symbol> {
        self.arena().alloc_symbol(symbol)
    }

    #[track_caller]
    fn transform_nodes_transformation_result(
        &self,
        transform_nodes_transformation_result: Id<TransformNodesTransformationResult>,
    ) -> Ref<TransformNodesTransformationResult> {
        self.arena()
            .transform_nodes_transformation_result(transform_nodes_transformation_result)
    }

    #[track_caller]
    fn alloc_transform_nodes_transformation_result(
        &self,
        transform_nodes_transformation_result: TransformNodesTransformationResult,
    ) -> Id<TransformNodesTransformationResult> {
        self.arena()
            .alloc_transform_nodes_transformation_result(transform_nodes_transformation_result)
    }

    #[track_caller]
    fn transformer(
        &self,
        transformer: Id<Box<dyn TransformerInterface>>,
    ) -> Ref<Box<dyn TransformerInterface>> {
        self.arena().transformer(transformer)
    }

    #[track_caller]
    fn alloc_transformer(
        &self,
        transformer: Box<dyn TransformerInterface>,
    ) -> Id<Box<dyn TransformerInterface>> {
        self.arena().alloc_transformer(transformer)
    }

    #[track_caller]
    fn transformer_factory(
        &self,
        transformer_factory: Id<Box<dyn TransformerFactoryInterface>>,
    ) -> Ref<Box<dyn TransformerFactoryInterface>> {
        self.arena().transformer_factory(transformer_factory)
    }

    #[track_caller]
    fn alloc_transformer_factory(
        &self,
        transformer_factory: Box<dyn TransformerFactoryInterface>,
    ) -> Id<Box<dyn TransformerFactoryInterface>> {
        self.arena().alloc_transformer_factory(transformer_factory)
    }

    #[track_caller]
    fn emit_text_writer(
        &self,
        emit_text_writer: Id<Box<dyn EmitTextWriter>>,
    ) -> Ref<Box<dyn EmitTextWriter>> {
        self.arena().emit_text_writer(emit_text_writer)
    }

    #[track_caller]
    fn alloc_emit_text_writer(
        &self,
        emit_text_writer: Box<dyn EmitTextWriter>,
    ) -> Id<Box<dyn EmitTextWriter>> {
        self.arena().alloc_emit_text_writer(emit_text_writer)
    }

    #[track_caller]
    fn symbol_tracker(
        &self,
        symbol_tracker: Id<Box<dyn SymbolTracker>>,
    ) -> Ref<Box<dyn SymbolTracker>> {
        self.arena().symbol_tracker(symbol_tracker)
    }

    #[track_caller]
    fn alloc_symbol_tracker(
        &self,
        symbol_tracker: Box<dyn SymbolTracker>,
    ) -> Id<Box<dyn SymbolTracker>> {
        self.arena().alloc_symbol_tracker(symbol_tracker)
    }

    #[track_caller]
    fn emit_host(&self, emit_host: Id<Box<dyn EmitHost>>) -> Ref<Box<dyn EmitHost>> {
        self.arena().emit_host(emit_host)
    }

    #[track_caller]
    fn alloc_emit_host(&self, emit_host: Box<dyn EmitHost>) -> Id<Box<dyn EmitHost>> {
        self.arena().alloc_emit_host(emit_host)
    }

    #[track_caller]
    fn module_specifier_resolution_host_and_get_common_source_directory(
        &self,
        module_specifier_resolution_host_and_get_common_source_directory: Id<
            Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>,
        >,
    ) -> Ref<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>> {
        self.arena()
            .module_specifier_resolution_host_and_get_common_source_directory(
                module_specifier_resolution_host_and_get_common_source_directory,
            )
    }

    #[track_caller]
    fn alloc_module_specifier_resolution_host_and_get_common_source_directory(
        &self,
        module_specifier_resolution_host_and_get_common_source_directory: Box<
            dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory,
        >,
    ) -> Id<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>> {
        self.arena()
            .alloc_module_specifier_resolution_host_and_get_common_source_directory(
                module_specifier_resolution_host_and_get_common_source_directory,
            )
    }

    #[track_caller]
    fn file_include_reason(
        &self,
        file_include_reason: Id<FileIncludeReason>,
    ) -> Ref<FileIncludeReason> {
        self.arena().file_include_reason(file_include_reason)
    }

    #[track_caller]
    fn alloc_file_include_reason(
        &self,
        file_include_reason: FileIncludeReason,
    ) -> Id<FileIncludeReason> {
        self.arena().alloc_file_include_reason(file_include_reason)
    }

    #[track_caller]
    fn system(&self, system: Id<Box<dyn System>>) -> Ref<Box<dyn System>> {
        self.arena().system(system)
    }

    #[track_caller]
    fn alloc_system(&self, system: Box<dyn System>) -> Id<Box<dyn System>> {
        self.arena().alloc_system(system)
    }

    #[track_caller]
    fn source_map_range(&self, source_map_range: Id<SourceMapRange>) -> Ref<SourceMapRange> {
        self.arena().source_map_range(source_map_range)
    }

    #[track_caller]
    fn alloc_source_map_range(&self, source_map_range: SourceMapRange) -> Id<SourceMapRange> {
        self.arena().alloc_source_map_range(source_map_range)
    }

    #[track_caller]
    fn emit_helper(&self, emit_helper: Id<EmitHelper>) -> Ref<EmitHelper> {
        self.arena().emit_helper(emit_helper)
    }

    #[track_caller]
    fn alloc_emit_helper(&self, emit_helper: EmitHelper) -> Id<EmitHelper> {
        self.arena().alloc_emit_helper(emit_helper)
    }

    #[track_caller]
    fn compiler_options(&self, compiler_options: Id<CompilerOptions>) -> Ref<CompilerOptions> {
        self.arena().compiler_options(compiler_options)
    }

    #[track_caller]
    fn alloc_compiler_options(&self, compiler_options: CompilerOptions) -> Id<CompilerOptions> {
        self.arena().alloc_compiler_options(compiler_options)
    }

    #[track_caller]
    fn flow_node(&self, flow_node: Id<FlowNode>) -> Ref<FlowNode> {
        self.arena().flow_node(flow_node)
    }

    #[track_caller]
    fn alloc_flow_node(&self, flow_node: FlowNode) -> Id<FlowNode> {
        self.arena().alloc_flow_node(flow_node)
    }

    #[track_caller]
    fn diagnostic(&self, diagnostic: Id<Diagnostic>) -> Ref<Diagnostic> {
        self.arena().diagnostic(diagnostic)
    }

    #[track_caller]
    fn alloc_diagnostic(&self, diagnostic: Diagnostic) -> Id<Diagnostic> {
        self.arena().alloc_diagnostic(diagnostic)
    }

    #[track_caller]
    fn program(&self, program: Id<Program>) -> Ref<Program> {
        self.arena().program(program)
    }

    #[track_caller]
    fn alloc_program(&self, program: Program) -> Id<Program> {
        self.arena().alloc_program(program)
    }

    #[track_caller]
    fn signature(&self, signature: Id<Signature>) -> Ref<Signature> {
        self.arena().signature(signature)
    }

    #[track_caller]
    fn alloc_signature(&self, signature: Signature) -> Id<Signature> {
        self.arena().alloc_signature(signature)
    }

    #[track_caller]
    fn diagnostic_reporter(
        &self,
        diagnostic_reporter: Id<Box<dyn DiagnosticReporter>>,
    ) -> Ref<Box<dyn DiagnosticReporter>> {
        self.arena().diagnostic_reporter(diagnostic_reporter)
    }

    #[track_caller]
    fn alloc_diagnostic_reporter(
        &self,
        diagnostic_reporter: Box<dyn DiagnosticReporter>,
    ) -> Id<Box<dyn DiagnosticReporter>> {
        self.arena().alloc_diagnostic_reporter(diagnostic_reporter)
    }

    #[track_caller]
    fn node_factory(&self, node_factory: Id<NodeFactory>) -> Ref<NodeFactory> {
        self.arena().node_factory(node_factory)
    }

    #[track_caller]
    fn alloc_node_factory(&self, node_factory: NodeFactory) -> Id<NodeFactory> {
        self.arena().alloc_node_factory(node_factory)
    }

    #[track_caller]
    fn base_node_factory(
        &self,
        base_node_factory: Id<Box<dyn BaseNodeFactory>>,
    ) -> Ref<Box<dyn BaseNodeFactory>> {
        self.arena().base_node_factory(base_node_factory)
    }

    #[track_caller]
    fn alloc_base_node_factory(
        &self,
        base_node_factory: Box<dyn BaseNodeFactory>,
    ) -> Id<Box<dyn BaseNodeFactory>> {
        self.arena().alloc_base_node_factory(base_node_factory)
    }

    #[track_caller]
    fn emit_resolver(
        &self,
        emit_resolver: Id<Box<dyn EmitResolver>>,
    ) -> Ref<Box<dyn EmitResolver>> {
        self.arena().emit_resolver(emit_resolver)
    }

    #[track_caller]
    fn alloc_emit_resolver(
        &self,
        emit_resolver: Box<dyn EmitResolver>,
    ) -> Id<Box<dyn EmitResolver>> {
        self.arena().alloc_emit_resolver(emit_resolver)
    }

    #[track_caller]
    fn resolved_type_reference_directive(
        &self,
        resolved_type_reference_directive: Id<ResolvedTypeReferenceDirective>,
    ) -> Ref<ResolvedTypeReferenceDirective> {
        self.arena()
            .resolved_type_reference_directive(resolved_type_reference_directive)
    }

    #[track_caller]
    fn alloc_resolved_type_reference_directive(
        &self,
        resolved_type_reference_directive: ResolvedTypeReferenceDirective,
    ) -> Id<ResolvedTypeReferenceDirective> {
        self.arena()
            .alloc_resolved_type_reference_directive(resolved_type_reference_directive)
    }

    #[track_caller]
    fn compiler_host(
        &self,
        compiler_host: Id<Box<dyn CompilerHost>>,
    ) -> Ref<Box<dyn CompilerHost>> {
        self.arena().compiler_host(compiler_host)
    }

    #[track_caller]
    fn alloc_compiler_host(
        &self,
        compiler_host: Box<dyn CompilerHost>,
    ) -> Id<Box<dyn CompilerHost>> {
        self.arena().alloc_compiler_host(compiler_host)
    }

    #[track_caller]
    fn symbol_links(&self, symbol_links: Id<SymbolLinks>) -> Ref<SymbolLinks> {
        self.arena().symbol_links(symbol_links)
    }

    #[track_caller]
    fn symbol_links_mut(&self, symbol_links: Id<SymbolLinks>) -> RefMut<SymbolLinks> {
        self.arena().symbol_links_mut(symbol_links)
    }

    #[track_caller]
    fn alloc_symbol_links(&self, symbol_links: SymbolLinks) -> Id<SymbolLinks> {
        self.arena().alloc_symbol_links(symbol_links)
    }

    #[track_caller]
    fn printer(&self, printer: Id<Printer>) -> Ref<Printer> {
        self.arena().printer(printer)
    }

    #[track_caller]
    fn alloc_printer(&self, printer: Printer) -> Id<Printer> {
        self.arena().alloc_printer(printer)
    }

    #[track_caller]
    fn diagnostic_related_information(
        &self,
        diagnostic_related_information: Id<DiagnosticRelatedInformation>,
    ) -> Ref<DiagnosticRelatedInformation> {
        self.arena()
            .diagnostic_related_information(diagnostic_related_information)
    }

    #[track_caller]
    fn alloc_diagnostic_related_information(
        &self,
        diagnostic_related_information: DiagnosticRelatedInformation,
    ) -> Id<DiagnosticRelatedInformation> {
        self.arena()
            .alloc_diagnostic_related_information(diagnostic_related_information)
    }

    #[track_caller]
    fn index_info(&self, index_info: Id<IndexInfo>) -> Ref<IndexInfo> {
        self.arena().index_info(index_info)
    }

    #[track_caller]
    fn alloc_index_info(&self, index_info: IndexInfo) -> Id<IndexInfo> {
        self.arena().alloc_index_info(index_info)
    }

    #[track_caller]
    fn current_parenthesizer_rule(
        &self,
        current_parenthesizer_rule: Id<Box<dyn CurrentParenthesizerRule>>,
    ) -> Ref<Box<dyn CurrentParenthesizerRule>> {
        self.arena()
            .current_parenthesizer_rule(current_parenthesizer_rule)
    }

    #[track_caller]
    fn alloc_current_parenthesizer_rule(
        &self,
        current_parenthesizer_rule: Box<dyn CurrentParenthesizerRule>,
    ) -> Id<Box<dyn CurrentParenthesizerRule>> {
        self.arena()
            .alloc_current_parenthesizer_rule(current_parenthesizer_rule)
    }

    #[track_caller]
    fn parenthesizer_rules(
        &self,
        parenthesizer_rules: Id<Box<dyn ParenthesizerRules>>,
    ) -> Ref<Box<dyn ParenthesizerRules>> {
        self.arena().parenthesizer_rules(parenthesizer_rules)
    }

    #[track_caller]
    fn alloc_parenthesizer_rules(
        &self,
        parenthesizer_rules: Box<dyn ParenthesizerRules>,
    ) -> Id<Box<dyn ParenthesizerRules>> {
        self.arena().alloc_parenthesizer_rules(parenthesizer_rules)
    }

    #[track_caller]
    fn iteration_types(&self, iteration_types: Id<IterationTypes>) -> Ref<IterationTypes> {
        self.arena().iteration_types(iteration_types)
    }

    #[track_caller]
    fn alloc_iteration_types(&self, iteration_types: IterationTypes) -> Id<IterationTypes> {
        self.arena().alloc_iteration_types(iteration_types)
    }

    #[track_caller]
    fn type_predicate(&self, type_predicate: Id<TypePredicate>) -> Ref<TypePredicate> {
        self.arena().type_predicate(type_predicate)
    }

    #[track_caller]
    fn alloc_type_predicate(&self, type_predicate: TypePredicate) -> Id<TypePredicate> {
        self.arena().alloc_type_predicate(type_predicate)
    }

    #[track_caller]
    fn active_label(&self, active_label: Id<ActiveLabel>) -> Ref<ActiveLabel> {
        self.arena().active_label(active_label)
    }

    #[track_caller]
    fn alloc_active_label(&self, active_label: ActiveLabel) -> Id<ActiveLabel> {
        self.arena().alloc_active_label(active_label)
    }

    #[track_caller]
    fn to_path(&self, to_path: Id<Box<dyn ToPath>>) -> Ref<Box<dyn ToPath>> {
        self.arena().to_path(to_path)
    }

    #[track_caller]
    fn alloc_to_path(&self, to_path: Box<dyn ToPath>) -> Id<Box<dyn ToPath>> {
        self.arena().alloc_to_path(to_path)
    }

    #[track_caller]
    fn module_resolution_host_overrider(
        &self,
        module_resolution_host_overrider: Id<Box<dyn ModuleResolutionHostOverrider>>,
    ) -> Ref<Box<dyn ModuleResolutionHostOverrider>> {
        self.arena()
            .module_resolution_host_overrider(module_resolution_host_overrider)
    }

    #[track_caller]
    fn alloc_module_resolution_host_overrider(
        &self,
        module_resolution_host_overrider: Box<dyn ModuleResolutionHostOverrider>,
    ) -> Id<Box<dyn ModuleResolutionHostOverrider>> {
        self.arena()
            .alloc_module_resolution_host_overrider(module_resolution_host_overrider)
    }

    #[track_caller]
    fn wrap_custom_transformer_factory_handle_default(
        &self,
        wrap_custom_transformer_factory_handle_default: Id<
            Box<dyn WrapCustomTransformerFactoryHandleDefault>,
        >,
    ) -> Ref<Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
        self.arena().wrap_custom_transformer_factory_handle_default(
            wrap_custom_transformer_factory_handle_default,
        )
    }

    #[track_caller]
    fn alloc_wrap_custom_transformer_factory_handle_default(
        &self,
        wrap_custom_transformer_factory_handle_default: Box<
            dyn WrapCustomTransformerFactoryHandleDefault,
        >,
    ) -> Id<Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
        self.arena()
            .alloc_wrap_custom_transformer_factory_handle_default(
                wrap_custom_transformer_factory_handle_default,
            )
    }

    #[track_caller]
    fn transformation_context_on_emit_node_overrider(
        &self,
        transformation_context_on_emit_node_overrider: Id<
            Box<dyn TransformationContextOnEmitNodeOverrider>,
        >,
    ) -> Ref<Box<dyn TransformationContextOnEmitNodeOverrider>> {
        self.arena().transformation_context_on_emit_node_overrider(
            transformation_context_on_emit_node_overrider,
        )
    }

    #[track_caller]
    fn alloc_transformation_context_on_emit_node_overrider(
        &self,
        transformation_context_on_emit_node_overrider: Box<
            dyn TransformationContextOnEmitNodeOverrider,
        >,
    ) -> Id<Box<dyn TransformationContextOnEmitNodeOverrider>> {
        self.arena()
            .alloc_transformation_context_on_emit_node_overrider(
                transformation_context_on_emit_node_overrider,
            )
    }

    #[track_caller]
    fn source_map_generator(
        &self,
        source_map_generator: Id<Box<dyn SourceMapGenerator>>,
    ) -> Ref<Box<dyn SourceMapGenerator>> {
        self.arena().source_map_generator(source_map_generator)
    }

    #[track_caller]
    fn alloc_source_map_generator(
        &self,
        source_map_generator: Box<dyn SourceMapGenerator>,
    ) -> Id<Box<dyn SourceMapGenerator>> {
        self.arena()
            .alloc_source_map_generator(source_map_generator)
    }

    #[track_caller]
    fn get_canonical_file_name_ref(
        &self,
        get_canonical_file_name: Id<Box<dyn GetCanonicalFileName>>,
    ) -> Ref<Box<dyn GetCanonicalFileName>> {
        self.arena()
            .get_canonical_file_name_ref(get_canonical_file_name)
    }

    #[track_caller]
    fn alloc_get_canonical_file_name(
        &self,
        get_canonical_file_name: Box<dyn GetCanonicalFileName>,
    ) -> Id<Box<dyn GetCanonicalFileName>> {
        self.arena()
            .alloc_get_canonical_file_name(get_canonical_file_name)
    }

    #[track_caller]
    fn emit_helper_factory(
        &self,
        emit_helper_factory: Id<EmitHelperFactory>,
    ) -> Ref<EmitHelperFactory> {
        self.arena().emit_helper_factory(emit_helper_factory)
    }

    #[track_caller]
    fn alloc_emit_helper_factory(
        &self,
        emit_helper_factory: EmitHelperFactory,
    ) -> Id<EmitHelperFactory> {
        self.arena().alloc_emit_helper_factory(emit_helper_factory)
    }

    #[track_caller]
    fn transformation_context_on_substitute_node_overrider(
        &self,
        transformation_context_on_substitute_node_overrider: Id<
            Box<dyn TransformationContextOnSubstituteNodeOverrider>,
        >,
    ) -> Ref<Box<dyn TransformationContextOnSubstituteNodeOverrider>> {
        self.arena()
            .transformation_context_on_substitute_node_overrider(
                transformation_context_on_substitute_node_overrider,
            )
    }

    #[track_caller]
    fn alloc_transformation_context_on_substitute_node_overrider(
        &self,
        transformation_context_on_substitute_node_overrider: Box<
            dyn TransformationContextOnSubstituteNodeOverrider,
        >,
    ) -> Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>> {
        self.arena()
            .alloc_transformation_context_on_substitute_node_overrider(
                transformation_context_on_substitute_node_overrider,
            )
    }

    #[track_caller]
    fn parsed_command_line(
        &self,
        parsed_command_line: Id<ParsedCommandLine>,
    ) -> Ref<ParsedCommandLine> {
        self.arena().parsed_command_line(parsed_command_line)
    }

    #[track_caller]
    fn alloc_parsed_command_line(
        &self,
        parsed_command_line: ParsedCommandLine,
    ) -> Id<ParsedCommandLine> {
        self.arena().alloc_parsed_command_line(parsed_command_line)
    }

    #[track_caller]
    fn cancellation_token(
        &self,
        cancellation_token: Id<Box<dyn CancellationToken>>,
    ) -> Ref<Box<dyn CancellationToken>> {
        self.arena().cancellation_token(cancellation_token)
    }

    #[track_caller]
    fn alloc_cancellation_token(
        &self,
        cancellation_token: Box<dyn CancellationToken>,
    ) -> Id<Box<dyn CancellationToken>> {
        self.arena().alloc_cancellation_token(cancellation_token)
    }

    #[track_caller]
    fn resolved_project_reference(
        &self,
        resolved_project_reference: Id<ResolvedProjectReference>,
    ) -> Ref<ResolvedProjectReference> {
        self.arena()
            .resolved_project_reference(resolved_project_reference)
    }

    #[track_caller]
    fn alloc_resolved_project_reference(
        &self,
        resolved_project_reference: ResolvedProjectReference,
    ) -> Id<ResolvedProjectReference> {
        self.arena()
            .alloc_resolved_project_reference(resolved_project_reference)
    }

    #[track_caller]
    fn transformer_factory_or_custom_transformer_factory(
        &self,
        transformer_factory_or_custom_transformer_factory: Id<
            TransformerFactoryOrCustomTransformerFactory,
        >,
    ) -> Ref<TransformerFactoryOrCustomTransformerFactory> {
        self.arena()
            .transformer_factory_or_custom_transformer_factory(
                transformer_factory_or_custom_transformer_factory,
            )
    }

    #[track_caller]
    fn alloc_transformer_factory_or_custom_transformer_factory(
        &self,
        transformer_factory_or_custom_transformer_factory: TransformerFactoryOrCustomTransformerFactory,
    ) -> Id<TransformerFactoryOrCustomTransformerFactory> {
        self.arena()
            .alloc_transformer_factory_or_custom_transformer_factory(
                transformer_factory_or_custom_transformer_factory,
            )
    }

    #[track_caller]
    fn symlink_cache(&self, symlink_cache: Id<SymlinkCache>) -> Ref<SymlinkCache> {
        self.arena().symlink_cache(symlink_cache)
    }

    #[track_caller]
    fn alloc_symlink_cache(&self, symlink_cache: SymlinkCache) -> Id<SymlinkCache> {
        self.arena().alloc_symlink_cache(symlink_cache)
    }

    #[track_caller]
    fn write_file_callback(
        &self,
        write_file_callback: Id<Box<dyn WriteFileCallback>>,
    ) -> Ref<Box<dyn WriteFileCallback>> {
        self.arena().write_file_callback(write_file_callback)
    }

    #[track_caller]
    fn alloc_write_file_callback(
        &self,
        write_file_callback: Box<dyn WriteFileCallback>,
    ) -> Id<Box<dyn WriteFileCallback>> {
        self.arena().alloc_write_file_callback(write_file_callback)
    }

    #[track_caller]
    fn resolved_module_full(
        &self,
        resolved_module_full: Id<ResolvedModuleFull>,
    ) -> Ref<ResolvedModuleFull> {
        self.arena().resolved_module_full(resolved_module_full)
    }

    #[track_caller]
    fn alloc_resolved_module_full(
        &self,
        resolved_module_full: ResolvedModuleFull,
    ) -> Id<ResolvedModuleFull> {
        self.arena()
            .alloc_resolved_module_full(resolved_module_full)
    }

    #[track_caller]
    fn node_array(&self, node_array: Id<NodeArray>) -> Ref<NodeArray> {
        self.arena().node_array(node_array)
    }

    #[track_caller]
    fn alloc_node_array(&self, node_array: NodeArray) -> Id<NodeArray> {
        self.arena().alloc_node_array(node_array)
    }

    #[track_caller]
    fn bundle_file_section(
        &self,
        bundle_file_section: Id<BundleFileSection>,
    ) -> Ref<BundleFileSection> {
        self.arena().bundle_file_section(bundle_file_section)
    }

    #[track_caller]
    fn alloc_bundle_file_section(
        &self,
        bundle_file_section: BundleFileSection,
    ) -> Id<BundleFileSection> {
        self.arena().alloc_bundle_file_section(bundle_file_section)
    }

    #[track_caller]
    fn build_info(&self, build_info: Id<BuildInfo>) -> Ref<BuildInfo> {
        self.arena().build_info(build_info)
    }

    #[track_caller]
    fn alloc_build_info(&self, build_info: BuildInfo) -> Id<BuildInfo> {
        self.arena().alloc_build_info(build_info)
    }

    #[track_caller]
    fn program_build_info(
        &self,
        program_build_info: Id<ProgramBuildInfo>,
    ) -> Ref<ProgramBuildInfo> {
        self.arena().program_build_info(program_build_info)
    }

    #[track_caller]
    fn alloc_program_build_info(
        &self,
        program_build_info: ProgramBuildInfo,
    ) -> Id<ProgramBuildInfo> {
        self.arena().alloc_program_build_info(program_build_info)
    }

    #[track_caller]
    fn bundle_build_info(&self, bundle_build_info: Id<BundleBuildInfo>) -> Ref<BundleBuildInfo> {
        self.arena().bundle_build_info(bundle_build_info)
    }

    #[track_caller]
    fn bundle_build_info_mut(
        &self,
        bundle_build_info: Id<BundleBuildInfo>,
    ) -> RefMut<BundleBuildInfo> {
        self.arena().bundle_build_info_mut(bundle_build_info)
    }

    #[track_caller]
    fn alloc_bundle_build_info(&self, bundle_build_info: BundleBuildInfo) -> Id<BundleBuildInfo> {
        self.arena().alloc_bundle_build_info(bundle_build_info)
    }

    #[track_caller]
    fn bundle_file_info(&self, bundle_file_info: Id<BundleFileInfo>) -> Ref<BundleFileInfo> {
        self.arena().bundle_file_info(bundle_file_info)
    }

    #[track_caller]
    fn bundle_file_info_mut(&self, bundle_file_info: Id<BundleFileInfo>) -> RefMut<BundleFileInfo> {
        self.arena().bundle_file_info_mut(bundle_file_info)
    }

    #[track_caller]
    fn alloc_bundle_file_info(&self, bundle_file_info: BundleFileInfo) -> Id<BundleFileInfo> {
        self.arena().alloc_bundle_file_info(bundle_file_info)
    }

    #[track_caller]
    fn symbol_table(&self, symbol_table: Id<SymbolTable>) -> Ref<SymbolTable> {
        self.arena().symbol_table(symbol_table)
    }

    #[track_caller]
    fn symbol_table_mut(&self, symbol_table: Id<SymbolTable>) -> RefMut<SymbolTable> {
        self.arena().symbol_table_mut(symbol_table)
    }

    #[track_caller]
    fn alloc_symbol_table(&self, symbol_table: SymbolTable) -> Id<SymbolTable> {
        self.arena().alloc_symbol_table(symbol_table)
    }

    #[track_caller]
    fn inference_info(&self, inference_info: Id<InferenceInfo>) -> Ref<InferenceInfo> {
        self.arena().inference_info(inference_info)
    }

    #[track_caller]
    fn alloc_inference_info(&self, inference_info: InferenceInfo) -> Id<InferenceInfo> {
        self.arena().alloc_inference_info(inference_info)
    }

    #[track_caller]
    fn sys_format_diagnostics_host(
        &self,
        sys_format_diagnostics_host: Id<SysFormatDiagnosticsHost>,
    ) -> Ref<SysFormatDiagnosticsHost> {
        self.arena()
            .sys_format_diagnostics_host(sys_format_diagnostics_host)
    }

    #[track_caller]
    fn alloc_sys_format_diagnostics_host(
        &self,
        sys_format_diagnostics_host: SysFormatDiagnosticsHost,
    ) -> Id<SysFormatDiagnosticsHost> {
        self.arena()
            .alloc_sys_format_diagnostics_host(sys_format_diagnostics_host)
    }

    #[track_caller]
    fn class_lexical_environment(
        &self,
        class_lexical_environment: Id<ClassLexicalEnvironment>,
    ) -> Ref<ClassLexicalEnvironment> {
        self.arena()
            .class_lexical_environment(class_lexical_environment)
    }

    #[track_caller]
    fn class_lexical_environment_mut(
        &self,
        class_lexical_environment: Id<ClassLexicalEnvironment>,
    ) -> RefMut<ClassLexicalEnvironment> {
        self.arena()
            .class_lexical_environment_mut(class_lexical_environment)
    }

    #[track_caller]
    fn alloc_class_lexical_environment(
        &self,
        class_lexical_environment: ClassLexicalEnvironment,
    ) -> Id<ClassLexicalEnvironment> {
        self.arena()
            .alloc_class_lexical_environment(class_lexical_environment)
    }

    #[track_caller]
    fn converted_loop_state(
        &self,
        converted_loop_state: Id<ConvertedLoopState>,
    ) -> Ref<ConvertedLoopState> {
        self.arena().converted_loop_state(converted_loop_state)
    }

    #[track_caller]
    fn converted_loop_state_mut(
        &self,
        converted_loop_state: Id<ConvertedLoopState>,
    ) -> RefMut<ConvertedLoopState> {
        self.arena().converted_loop_state_mut(converted_loop_state)
    }

    #[track_caller]
    fn alloc_converted_loop_state(
        &self,
        converted_loop_state: ConvertedLoopState,
    ) -> Id<ConvertedLoopState> {
        self.arena()
            .alloc_converted_loop_state(converted_loop_state)
    }

    #[track_caller]
    fn emit_helper_text_callback(
        &self,
        emit_helper_text_callback: Id<Box<dyn EmitHelperTextCallback>>,
    ) -> Ref<Box<dyn EmitHelperTextCallback>> {
        self.arena()
            .emit_helper_text_callback(emit_helper_text_callback)
    }

    #[track_caller]
    fn alloc_emit_helper_text_callback(
        &self,
        emit_helper_text_callback: Box<dyn EmitHelperTextCallback>,
    ) -> Id<Box<dyn EmitHelperTextCallback>> {
        self.arena()
            .alloc_emit_helper_text_callback(emit_helper_text_callback)
    }

    #[track_caller]
    fn conditional_root(&self, conditional_root: Id<ConditionalRoot>) -> Ref<ConditionalRoot> {
        self.arena().conditional_root(conditional_root)
    }

    #[track_caller]
    fn conditional_root_mut(
        &self,
        conditional_root: Id<ConditionalRoot>,
    ) -> RefMut<ConditionalRoot> {
        self.arena().conditional_root_mut(conditional_root)
    }

    #[track_caller]
    fn alloc_conditional_root(&self, conditional_root: ConditionalRoot) -> Id<ConditionalRoot> {
        self.arena().alloc_conditional_root(conditional_root)
    }

    #[track_caller]
    fn emit_node(&self, emit_node: Id<EmitNode>) -> Ref<EmitNode> {
        self.arena().emit_node(emit_node)
    }

    #[track_caller]
    fn emit_node_mut(&self, emit_node: Id<EmitNode>) -> RefMut<EmitNode> {
        self.arena().emit_node_mut(emit_node)
    }

    #[track_caller]
    fn alloc_emit_node(&self, emit_node: EmitNode) -> Id<EmitNode> {
        self.arena().alloc_emit_node(emit_node)
    }

    #[track_caller]
    fn check_binary_expression(
        &self,
        check_binary_expression: Id<CheckBinaryExpression>,
    ) -> Ref<CheckBinaryExpression> {
        self.arena()
            .check_binary_expression(check_binary_expression)
    }

    #[track_caller]
    fn alloc_check_binary_expression(
        &self,
        check_binary_expression: CheckBinaryExpression,
    ) -> Id<CheckBinaryExpression> {
        self.arena()
            .alloc_check_binary_expression(check_binary_expression)
    }

    #[track_caller]
    fn source_map_source(&self, source_map_source: Id<SourceMapSource>) -> Ref<SourceMapSource> {
        self.arena().source_map_source(source_map_source)
    }

    #[track_caller]
    fn alloc_source_map_source(&self, source_map_source: SourceMapSource) -> Id<SourceMapSource> {
        self.arena().alloc_source_map_source(source_map_source)
    }

    #[track_caller]
    fn outofband_variance_marker_handler(
        &self,
        outofband_variance_marker_handler: Id<Box<dyn OutofbandVarianceMarkerHandler>>,
    ) -> Ref<Box<dyn OutofbandVarianceMarkerHandler>> {
        self.arena()
            .outofband_variance_marker_handler(outofband_variance_marker_handler)
    }

    #[track_caller]
    fn alloc_outofband_variance_marker_handler(
        &self,
        outofband_variance_marker_handler: Box<dyn OutofbandVarianceMarkerHandler>,
    ) -> Id<Box<dyn OutofbandVarianceMarkerHandler>> {
        self.arena()
            .alloc_outofband_variance_marker_handler(outofband_variance_marker_handler)
    }

    #[track_caller]
    fn bind_binary_expression_flow(
        &self,
        bind_binary_expression_flow: Id<BindBinaryExpressionFlow>,
    ) -> Ref<BindBinaryExpressionFlow> {
        self.arena()
            .bind_binary_expression_flow(bind_binary_expression_flow)
    }

    #[track_caller]
    fn alloc_bind_binary_expression_flow(
        &self,
        bind_binary_expression_flow: BindBinaryExpressionFlow,
    ) -> Id<BindBinaryExpressionFlow> {
        self.arena()
            .alloc_bind_binary_expression_flow(bind_binary_expression_flow)
    }

    #[track_caller]
    fn type_checker(&self, type_checker: Id<TypeChecker>) -> Ref<TypeChecker> {
        self.arena().type_checker(type_checker)
    }

    #[track_caller]
    fn alloc_type_checker(&self, type_checker: TypeChecker) -> Id<TypeChecker> {
        self.arena().alloc_type_checker(type_checker)
    }

    #[track_caller]
    fn read_file_callback(
        &self,
        read_file_callback: Id<Box<dyn ReadFileCallback>>,
    ) -> Ref<Box<dyn ReadFileCallback>> {
        self.arena().read_file_callback(read_file_callback)
    }

    #[track_caller]
    fn alloc_read_file_callback(
        &self,
        read_file_callback: Box<dyn ReadFileCallback>,
    ) -> Id<Box<dyn ReadFileCallback>> {
        self.arena().alloc_read_file_callback(read_file_callback)
    }

    #[track_caller]
    fn binder(&self, binder: Id<Binder>) -> Ref<Binder> {
        self.arena().binder(binder)
    }

    #[track_caller]
    fn alloc_binder(&self, binder: Binder) -> Id<Binder> {
        self.arena().alloc_binder(binder)
    }

    #[track_caller]
    fn get_source_file_ref(
        &self,
        get_source_file: Id<Box<dyn GetSourceFile>>,
    ) -> Ref<Box<dyn GetSourceFile>> {
        self.arena().get_source_file_ref(get_source_file)
    }

    #[track_caller]
    fn alloc_get_source_file(
        &self,
        get_source_file: Box<dyn GetSourceFile>,
    ) -> Id<Box<dyn GetSourceFile>> {
        self.arena().alloc_get_source_file(get_source_file)
    }

    #[track_caller]
    fn get_symlink_cache(
        &self,
        get_symlink_cache: Id<Box<dyn GetSymlinkCache>>,
    ) -> Ref<Box<dyn GetSymlinkCache>> {
        self.arena().get_symlink_cache(get_symlink_cache)
    }

    #[track_caller]
    fn alloc_get_symlink_cache(
        &self,
        get_symlink_cache: Box<dyn GetSymlinkCache>,
    ) -> Id<Box<dyn GetSymlinkCache>> {
        self.arena().alloc_get_symlink_cache(get_symlink_cache)
    }

    #[track_caller]
    fn emit_binary_expression(
        &self,
        emit_binary_expression: Id<EmitBinaryExpression>,
    ) -> Ref<EmitBinaryExpression> {
        self.arena().emit_binary_expression(emit_binary_expression)
    }

    #[track_caller]
    fn alloc_emit_binary_expression(
        &self,
        emit_binary_expression: EmitBinaryExpression,
    ) -> Id<EmitBinaryExpression> {
        self.arena()
            .alloc_emit_binary_expression(emit_binary_expression)
    }

    #[track_caller]
    fn relative_to_build_info(
        &self,
        relative_to_build_info: Id<Box<dyn RelativeToBuildInfo>>,
    ) -> Ref<Box<dyn RelativeToBuildInfo>> {
        self.arena().relative_to_build_info(relative_to_build_info)
    }

    #[track_caller]
    fn alloc_relative_to_build_info(
        &self,
        relative_to_build_info: Box<dyn RelativeToBuildInfo>,
    ) -> Id<Box<dyn RelativeToBuildInfo>> {
        self.arena()
            .alloc_relative_to_build_info(relative_to_build_info)
    }

    #[track_caller]
    fn print_handlers(
        &self,
        print_handlers: Id<Box<dyn PrintHandlers>>,
    ) -> Ref<Box<dyn PrintHandlers>> {
        self.arena().print_handlers(print_handlers)
    }

    #[track_caller]
    fn alloc_print_handlers(
        &self,
        print_handlers: Box<dyn PrintHandlers>,
    ) -> Id<Box<dyn PrintHandlers>> {
        self.arena().alloc_print_handlers(print_handlers)
    }

    #[track_caller]
    fn get_resolved_project_references_ref(
        &self,
        get_resolved_project_references: Id<Box<dyn GetResolvedProjectReferences>>,
    ) -> Ref<Box<dyn GetResolvedProjectReferences>> {
        self.arena()
            .get_resolved_project_references_ref(get_resolved_project_references)
    }

    #[track_caller]
    fn alloc_get_resolved_project_references(
        &self,
        get_resolved_project_references: Box<dyn GetResolvedProjectReferences>,
    ) -> Id<Box<dyn GetResolvedProjectReferences>> {
        self.arena()
            .alloc_get_resolved_project_references(get_resolved_project_references)
    }

    #[track_caller]
    fn for_each_resolved_project_reference_ref(
        &self,
        for_each_resolved_project_reference: Id<Box<dyn ForEachResolvedProjectReference>>,
    ) -> Ref<Box<dyn ForEachResolvedProjectReference>> {
        self.arena()
            .for_each_resolved_project_reference_ref(for_each_resolved_project_reference)
    }

    #[track_caller]
    fn alloc_for_each_resolved_project_reference(
        &self,
        for_each_resolved_project_reference: Box<dyn ForEachResolvedProjectReference>,
    ) -> Id<Box<dyn ForEachResolvedProjectReference>> {
        self.arena()
            .alloc_for_each_resolved_project_reference(for_each_resolved_project_reference)
    }

    #[track_caller]
    fn compiler_host_like(
        &self,
        compiler_host_like: Id<Box<dyn CompilerHostLike>>,
    ) -> Ref<Box<dyn CompilerHostLike>> {
        self.arena().compiler_host_like(compiler_host_like)
    }

    #[track_caller]
    fn alloc_compiler_host_like(
        &self,
        compiler_host_like: Box<dyn CompilerHostLike>,
    ) -> Id<Box<dyn CompilerHostLike>> {
        self.arena().alloc_compiler_host_like(compiler_host_like)
    }

    #[track_caller]
    fn directory_structure_host(
        &self,
        directory_structure_host: Id<Box<dyn DirectoryStructureHost>>,
    ) -> Ref<Box<dyn DirectoryStructureHost>> {
        self.arena()
            .directory_structure_host(directory_structure_host)
    }

    #[track_caller]
    fn alloc_directory_structure_host(
        &self,
        directory_structure_host: Box<dyn DirectoryStructureHost>,
    ) -> Id<Box<dyn DirectoryStructureHost>> {
        self.arena()
            .alloc_directory_structure_host(directory_structure_host)
    }

    #[track_caller]
    fn builder_program(
        &self,
        builder_program: Id<Box<dyn BuilderProgram>>,
    ) -> Ref<Box<dyn BuilderProgram>> {
        self.arena().builder_program(builder_program)
    }

    #[track_caller]
    fn alloc_builder_program(
        &self,
        builder_program: Box<dyn BuilderProgram>,
    ) -> Id<Box<dyn BuilderProgram>> {
        self.arena().alloc_builder_program(builder_program)
    }

    #[track_caller]
    fn type_reference_directive_resolution_cache(
        &self,
        type_reference_directive_resolution_cache: Id<TypeReferenceDirectiveResolutionCache>,
    ) -> Ref<TypeReferenceDirectiveResolutionCache> {
        self.arena()
            .type_reference_directive_resolution_cache(type_reference_directive_resolution_cache)
    }

    #[track_caller]
    fn alloc_type_reference_directive_resolution_cache(
        &self,
        type_reference_directive_resolution_cache: TypeReferenceDirectiveResolutionCache,
    ) -> Id<TypeReferenceDirectiveResolutionCache> {
        self.arena()
            .alloc_type_reference_directive_resolution_cache(
                type_reference_directive_resolution_cache,
            )
    }

    #[track_caller]
    fn module_resolution_cache(
        &self,
        module_resolution_cache: Id<ModuleResolutionCache>,
    ) -> Ref<ModuleResolutionCache> {
        self.arena()
            .module_resolution_cache(module_resolution_cache)
    }

    #[track_caller]
    fn alloc_module_resolution_cache(
        &self,
        module_resolution_cache: ModuleResolutionCache,
    ) -> Id<ModuleResolutionCache> {
        self.arena()
            .alloc_module_resolution_cache(module_resolution_cache)
    }

    #[track_caller]
    fn parse_config_file_host(
        &self,
        parse_config_file_host: Id<Box<dyn ParseConfigFileHost>>,
    ) -> Ref<Box<dyn ParseConfigFileHost>> {
        self.arena().parse_config_file_host(parse_config_file_host)
    }

    #[track_caller]
    fn alloc_parse_config_file_host(
        &self,
        parse_config_file_host: Box<dyn ParseConfigFileHost>,
    ) -> Id<Box<dyn ParseConfigFileHost>> {
        self.arena()
            .alloc_parse_config_file_host(parse_config_file_host)
    }

    #[track_caller]
    fn file_preprocessing_diagnostics(
        &self,
        file_preprocessing_diagnostics: Id<FilePreprocessingDiagnostics>,
    ) -> Ref<FilePreprocessingDiagnostics> {
        self.arena()
            .file_preprocessing_diagnostics(file_preprocessing_diagnostics)
    }

    #[track_caller]
    fn alloc_file_preprocessing_diagnostics(
        &self,
        file_preprocessing_diagnostics: FilePreprocessingDiagnostics,
    ) -> Id<FilePreprocessingDiagnostics> {
        self.arena()
            .alloc_file_preprocessing_diagnostics(file_preprocessing_diagnostics)
    }

    #[track_caller]
    fn actual_resolve_module_names_worker(
        &self,
        actual_resolve_module_names_worker: Id<Box<dyn ActualResolveModuleNamesWorker>>,
    ) -> Ref<Box<dyn ActualResolveModuleNamesWorker>> {
        self.arena()
            .actual_resolve_module_names_worker(actual_resolve_module_names_worker)
    }

    #[track_caller]
    fn alloc_actual_resolve_module_names_worker(
        &self,
        actual_resolve_module_names_worker: Box<dyn ActualResolveModuleNamesWorker>,
    ) -> Id<Box<dyn ActualResolveModuleNamesWorker>> {
        self.arena()
            .alloc_actual_resolve_module_names_worker(actual_resolve_module_names_worker)
    }

    #[track_caller]
    fn actual_resolve_type_reference_directive_names_worker(
        &self,
        actual_resolve_type_reference_directive_names_worker: Id<
            Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>,
        >,
    ) -> Ref<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>> {
        self.arena()
            .actual_resolve_type_reference_directive_names_worker(
                actual_resolve_type_reference_directive_names_worker,
            )
    }

    #[track_caller]
    fn alloc_actual_resolve_type_reference_directive_names_worker(
        &self,
        actual_resolve_type_reference_directive_names_worker: Box<
            dyn ActualResolveTypeReferenceDirectiveNamesWorker,
        >,
    ) -> Id<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>> {
        self.arena()
            .alloc_actual_resolve_type_reference_directive_names_worker(
                actual_resolve_type_reference_directive_names_worker,
            )
    }

    #[track_caller]
    fn get_program_build_info_ref(
        &self,
        get_program_build_info: Id<Box<dyn GetProgramBuildInfo>>,
    ) -> Ref<Box<dyn GetProgramBuildInfo>> {
        self.arena()
            .get_program_build_info_ref(get_program_build_info)
    }

    #[track_caller]
    fn alloc_get_program_build_info(
        &self,
        get_program_build_info: Box<dyn GetProgramBuildInfo>,
    ) -> Id<Box<dyn GetProgramBuildInfo>> {
        self.arena()
            .alloc_get_program_build_info(get_program_build_info)
    }

    #[track_caller]
    fn load_with_mode_aware_cache_loader(
        &self,
        load_with_mode_aware_cache_loader: Id<
            Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>,
        >,
    ) -> Ref<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>> {
        self.arena()
            .load_with_mode_aware_cache_loader(load_with_mode_aware_cache_loader)
    }

    #[track_caller]
    fn alloc_load_with_mode_aware_cache_loader(
        &self,
        load_with_mode_aware_cache_loader: Box<
            dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>,
        >,
    ) -> Id<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>> {
        self.arena()
            .alloc_load_with_mode_aware_cache_loader(load_with_mode_aware_cache_loader)
    }

    #[track_caller]
    fn load_with_local_cache_loader(
        &self,
        load_with_local_cache_loader: Id<
            Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>,
        >,
    ) -> Ref<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>> {
        self.arena()
            .load_with_local_cache_loader(load_with_local_cache_loader)
    }

    #[track_caller]
    fn alloc_load_with_local_cache_loader(
        &self,
        load_with_local_cache_loader: Box<
            dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>,
        >,
    ) -> Id<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>> {
        self.arena()
            .alloc_load_with_local_cache_loader(load_with_local_cache_loader)
    }

    #[track_caller]
    fn symbol_accessibility_diagnostic(
        &self,
        symbol_accessibility_diagnostic: Id<SymbolAccessibilityDiagnostic>,
    ) -> Ref<SymbolAccessibilityDiagnostic> {
        self.arena()
            .symbol_accessibility_diagnostic(symbol_accessibility_diagnostic)
    }

    #[track_caller]
    fn alloc_symbol_accessibility_diagnostic(
        &self,
        symbol_accessibility_diagnostic: SymbolAccessibilityDiagnostic,
    ) -> Id<SymbolAccessibilityDiagnostic> {
        self.arena()
            .alloc_symbol_accessibility_diagnostic(symbol_accessibility_diagnostic)
    }

    #[track_caller]
    fn code_block(&self, code_block: Id<CodeBlock>) -> Ref<CodeBlock> {
        self.arena().code_block(code_block)
    }

    #[track_caller]
    fn code_block_mut(&self, code_block: Id<CodeBlock>) -> RefMut<CodeBlock> {
        self.arena().code_block_mut(code_block)
    }

    #[track_caller]
    fn alloc_code_block(&self, code_block: CodeBlock) -> Id<CodeBlock> {
        self.arena().alloc_code_block(code_block)
    }

    #[track_caller]
    fn private_identifier_environment(
        &self,
        private_identifier_environment: Id<PrivateIdentifierEnvironment>,
    ) -> Ref<PrivateIdentifierEnvironment> {
        self.arena()
            .private_identifier_environment(private_identifier_environment)
    }

    #[track_caller]
    fn private_identifier_environment_mut(
        &self,
        private_identifier_environment: Id<PrivateIdentifierEnvironment>,
    ) -> RefMut<PrivateIdentifierEnvironment> {
        self.arena()
            .private_identifier_environment_mut(private_identifier_environment)
    }

    #[track_caller]
    fn alloc_private_identifier_environment(
        &self,
        private_identifier_environment: PrivateIdentifierEnvironment,
    ) -> Id<PrivateIdentifierEnvironment> {
        self.arena()
            .alloc_private_identifier_environment(private_identifier_environment)
    }

    #[track_caller]
    fn private_identifier_info(
        &self,
        private_identifier_info: Id<PrivateIdentifierInfo>,
    ) -> Ref<PrivateIdentifierInfo> {
        self.arena()
            .private_identifier_info(private_identifier_info)
    }

    #[track_caller]
    fn private_identifier_info_mut(
        &self,
        private_identifier_info: Id<PrivateIdentifierInfo>,
    ) -> RefMut<PrivateIdentifierInfo> {
        self.arena()
            .private_identifier_info_mut(private_identifier_info)
    }

    #[track_caller]
    fn alloc_private_identifier_info(
        &self,
        private_identifier_info: PrivateIdentifierInfo,
    ) -> Id<PrivateIdentifierInfo> {
        self.arena()
            .alloc_private_identifier_info(private_identifier_info)
    }

    #[track_caller]
    fn external_module_info(
        &self,
        external_module_info: Id<ExternalModuleInfo>,
    ) -> Ref<ExternalModuleInfo> {
        self.arena().external_module_info(external_module_info)
    }

    #[track_caller]
    fn alloc_external_module_info(
        &self,
        external_module_info: ExternalModuleInfo,
    ) -> Id<ExternalModuleInfo> {
        self.arena()
            .alloc_external_module_info(external_module_info)
    }

    #[track_caller]
    fn resolved_module_with_failed_lookup_locations(
        &self,
        resolved_module_with_failed_lookup_locations: Id<ResolvedModuleWithFailedLookupLocations>,
    ) -> Ref<ResolvedModuleWithFailedLookupLocations> {
        self.arena().resolved_module_with_failed_lookup_locations(
            resolved_module_with_failed_lookup_locations,
        )
    }

    #[track_caller]
    fn alloc_resolved_module_with_failed_lookup_locations(
        &self,
        resolved_module_with_failed_lookup_locations: ResolvedModuleWithFailedLookupLocations,
    ) -> Id<ResolvedModuleWithFailedLookupLocations> {
        self.arena()
            .alloc_resolved_module_with_failed_lookup_locations(
                resolved_module_with_failed_lookup_locations,
            )
    }

    #[track_caller]
    fn resolved_type_reference_directive_with_failed_lookup_locations(
        &self,
        resolved_type_reference_directive_with_failed_lookup_locations: Id<
            ResolvedTypeReferenceDirectiveWithFailedLookupLocations,
        >,
    ) -> Ref<ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
        self.arena()
            .resolved_type_reference_directive_with_failed_lookup_locations(
                resolved_type_reference_directive_with_failed_lookup_locations,
            )
    }

    #[track_caller]
    fn alloc_resolved_type_reference_directive_with_failed_lookup_locations(
        &self,
        resolved_type_reference_directive_with_failed_lookup_locations: ResolvedTypeReferenceDirectiveWithFailedLookupLocations,
    ) -> Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
        self.arena()
            .alloc_resolved_type_reference_directive_with_failed_lookup_locations(
                resolved_type_reference_directive_with_failed_lookup_locations,
            )
    }

    #[track_caller]
    fn package_json_info_cache(
        &self,
        package_json_info_cache: Id<Box<dyn PackageJsonInfoCache>>,
    ) -> Ref<Box<dyn PackageJsonInfoCache>> {
        self.arena()
            .package_json_info_cache(package_json_info_cache)
    }

    #[track_caller]
    fn alloc_package_json_info_cache(
        &self,
        package_json_info_cache: Box<dyn PackageJsonInfoCache>,
    ) -> Id<Box<dyn PackageJsonInfoCache>> {
        self.arena()
            .alloc_package_json_info_cache(package_json_info_cache)
    }

    #[track_caller]
    fn mode_aware_cache_resolved_module_with_failed_lookup_locations(
        &self,
        mode_aware_cache_resolved_module_with_failed_lookup_locations: Id<
            ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>,
        >,
    ) -> Ref<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>> {
        self.arena()
            .mode_aware_cache_resolved_module_with_failed_lookup_locations(
                mode_aware_cache_resolved_module_with_failed_lookup_locations,
            )
    }

    #[track_caller]
    fn alloc_mode_aware_cache_resolved_module_with_failed_lookup_locations(
        &self,
        mode_aware_cache_resolved_module_with_failed_lookup_locations: ModeAwareCache<
            Id<ResolvedModuleWithFailedLookupLocations>,
        >,
    ) -> Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>> {
        self.arena()
            .alloc_mode_aware_cache_resolved_module_with_failed_lookup_locations(
                mode_aware_cache_resolved_module_with_failed_lookup_locations,
            )
    }

    #[track_caller]
    fn mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(
        &self,
        mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations: Id<
            ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
        >,
    ) -> Ref<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>> {
        self.arena()
            .mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(
                mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations,
            )
    }

    #[track_caller]
    fn alloc_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(
        &self,
        mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations: ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
    ) -> Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>> {
        self.arena()
            .alloc_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(
                mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations,
            )
    }

    #[track_caller]
    fn per_module_name_cache(
        &self,
        per_module_name_cache: Id<PerModuleNameCache>,
    ) -> Ref<PerModuleNameCache> {
        self.arena().per_module_name_cache(per_module_name_cache)
    }

    #[track_caller]
    fn alloc_per_module_name_cache(
        &self,
        per_module_name_cache: PerModuleNameCache,
    ) -> Id<PerModuleNameCache> {
        self.arena()
            .alloc_per_module_name_cache(per_module_name_cache)
    }

    #[track_caller]
    fn vec_diagnostic(&self, vec_diagnostic: Id<Vec<Id<Diagnostic>>>) -> Ref<Vec<Id<Diagnostic>>> {
        self.arena().vec_diagnostic(vec_diagnostic)
    }

    #[track_caller]
    fn vec_diagnostic_mut(
        &self,
        vec_diagnostic: Id<Vec<Id<Diagnostic>>>,
    ) -> RefMut<Vec<Id<Diagnostic>>> {
        self.arena().vec_diagnostic_mut(vec_diagnostic)
    }

    #[track_caller]
    fn alloc_vec_diagnostic(&self, vec_diagnostic: Vec<Id<Diagnostic>>) -> Id<Vec<Id<Diagnostic>>> {
        self.arena().alloc_vec_diagnostic(vec_diagnostic)
    }

    #[track_caller]
    fn file_reasons(
        &self,
        file_reasons: Id<MultiMap<Path, Id<FileIncludeReason>>>,
    ) -> Ref<MultiMap<Path, Id<FileIncludeReason>>> {
        self.arena().file_reasons(file_reasons)
    }

    #[track_caller]
    fn file_reasons_mut(
        &self,
        file_reasons: Id<MultiMap<Path, Id<FileIncludeReason>>>,
    ) -> RefMut<MultiMap<Path, Id<FileIncludeReason>>> {
        self.arena().file_reasons_mut(file_reasons)
    }

    #[track_caller]
    fn alloc_file_reasons(
        &self,
        file_reasons: MultiMap<Path, Id<FileIncludeReason>>,
    ) -> Id<MultiMap<Path, Id<FileIncludeReason>>> {
        self.arena().alloc_file_reasons(file_reasons)
    }

    #[track_caller]
    fn get_symbol_accessibility_diagnostic_interface(
        &self,
        get_symbol_accessibility_diagnostic_interface: Id<
            Box<dyn GetSymbolAccessibilityDiagnosticInterface>,
        >,
    ) -> Ref<Box<dyn GetSymbolAccessibilityDiagnosticInterface>> {
        self.arena().get_symbol_accessibility_diagnostic_interface(
            get_symbol_accessibility_diagnostic_interface,
        )
    }

    #[track_caller]
    fn alloc_get_symbol_accessibility_diagnostic_interface(
        &self,
        get_symbol_accessibility_diagnostic_interface: Box<
            dyn GetSymbolAccessibilityDiagnosticInterface,
        >,
    ) -> Id<Box<dyn GetSymbolAccessibilityDiagnosticInterface>> {
        self.arena()
            .alloc_get_symbol_accessibility_diagnostic_interface(
                get_symbol_accessibility_diagnostic_interface,
            )
    }

    #[track_caller]
    fn option_vec_node(
        &self,
        option_vec_node: Id<Option<Vec<Id<Node>>>>,
    ) -> Ref<Option<Vec<Id<Node>>>> {
        self.arena().option_vec_node(option_vec_node)
    }

    #[track_caller]
    fn option_vec_node_mut(
        &self,
        option_vec_node: Id<Option<Vec<Id<Node>>>>,
    ) -> RefMut<Option<Vec<Id<Node>>>> {
        self.arena().option_vec_node_mut(option_vec_node)
    }

    #[track_caller]
    fn alloc_option_vec_node(
        &self,
        option_vec_node: Option<Vec<Id<Node>>>,
    ) -> Id<Option<Vec<Id<Node>>>> {
        self.arena().alloc_option_vec_node(option_vec_node)
    }

    #[track_caller]
    fn vec_pending_declaration(
        &self,
        vec_pending_declaration: Id<Vec<PendingDeclaration>>,
    ) -> Ref<Vec<PendingDeclaration>> {
        self.arena()
            .vec_pending_declaration(vec_pending_declaration)
    }

    #[track_caller]
    fn vec_pending_declaration_mut(
        &self,
        vec_pending_declaration: Id<Vec<PendingDeclaration>>,
    ) -> RefMut<Vec<PendingDeclaration>> {
        self.arena()
            .vec_pending_declaration_mut(vec_pending_declaration)
    }

    #[track_caller]
    fn alloc_vec_pending_declaration(
        &self,
        vec_pending_declaration: Vec<PendingDeclaration>,
    ) -> Id<Vec<PendingDeclaration>> {
        self.arena()
            .alloc_vec_pending_declaration(vec_pending_declaration)
    }

    #[track_caller]
    fn package_json_info(&self, package_json_info: Id<PackageJsonInfo>) -> Ref<PackageJsonInfo> {
        self.arena().package_json_info(package_json_info)
    }

    #[track_caller]
    fn alloc_package_json_info(&self, package_json_info: PackageJsonInfo) -> Id<PackageJsonInfo> {
        self.arena().alloc_package_json_info(package_json_info)
    }

    #[track_caller]
    fn vec_type(&self, vec_type: Id<Vec<Id<Type>>>) -> Ref<Vec<Id<Type>>> {
        self.arena().vec_type(vec_type)
    }

    #[track_caller]
    fn alloc_vec_type(&self, vec_type: Vec<Id<Type>>) -> Id<Vec<Id<Type>>> {
        self.arena().alloc_vec_type(vec_type)
    }

    #[track_caller]
    fn pattern_ambient_module(
        &self,
        pattern_ambient_module: Id<PatternAmbientModule>,
    ) -> Ref<PatternAmbientModule> {
        self.arena().pattern_ambient_module(pattern_ambient_module)
    }

    #[track_caller]
    fn alloc_pattern_ambient_module(
        &self,
        pattern_ambient_module: PatternAmbientModule,
    ) -> Id<PatternAmbientModule> {
        self.arena()
            .alloc_pattern_ambient_module(pattern_ambient_module)
    }

    #[track_caller]
    fn check_type_containing_message_chain(
        &self,
        check_type_containing_message_chain: Id<Box<dyn CheckTypeContainingMessageChain>>,
    ) -> Ref<Box<dyn CheckTypeContainingMessageChain>> {
        self.arena()
            .check_type_containing_message_chain(check_type_containing_message_chain)
    }

    #[track_caller]
    fn alloc_check_type_containing_message_chain(
        &self,
        check_type_containing_message_chain: Box<dyn CheckTypeContainingMessageChain>,
    ) -> Id<Box<dyn CheckTypeContainingMessageChain>> {
        self.arena()
            .alloc_check_type_containing_message_chain(check_type_containing_message_chain)
    }

    #[track_caller]
    fn check_type_error_output_container(
        &self,
        check_type_error_output_container: Id<Box<dyn CheckTypeErrorOutputContainer>>,
    ) -> Ref<Box<dyn CheckTypeErrorOutputContainer>> {
        self.arena()
            .check_type_error_output_container(check_type_error_output_container)
    }

    #[track_caller]
    fn alloc_check_type_error_output_container(
        &self,
        check_type_error_output_container: Box<dyn CheckTypeErrorOutputContainer>,
    ) -> Id<Box<dyn CheckTypeErrorOutputContainer>> {
        self.arena()
            .alloc_check_type_error_output_container(check_type_error_output_container)
    }

    #[track_caller]
    fn resolved_type_reference_directives_map(
        &self,
        resolved_type_reference_directives_map: Id<
            HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>,
        >,
    ) -> Ref<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        self.arena()
            .resolved_type_reference_directives_map(resolved_type_reference_directives_map)
    }

    #[track_caller]
    fn resolved_type_reference_directives_map_mut(
        &self,
        resolved_type_reference_directives_map: Id<
            HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>,
        >,
    ) -> RefMut<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        self.arena()
            .resolved_type_reference_directives_map_mut(resolved_type_reference_directives_map)
    }

    #[track_caller]
    fn alloc_resolved_type_reference_directives_map(
        &self,
        resolved_type_reference_directives_map: HashMap<
            String,
            Option<Id<ResolvedTypeReferenceDirective>>,
        >,
    ) -> Id<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        self.arena()
            .alloc_resolved_type_reference_directives_map(resolved_type_reference_directives_map)
    }

    #[track_caller]
    fn node_builder(&self, node_builder: Id<NodeBuilder>) -> Ref<NodeBuilder> {
        self.arena().node_builder(node_builder)
    }

    #[track_caller]
    fn alloc_node_builder(&self, node_builder: NodeBuilder) -> Id<NodeBuilder> {
        self.arena().alloc_node_builder(node_builder)
    }

    #[track_caller]
    fn node_builder_context(
        &self,
        node_builder_context: Id<NodeBuilderContext>,
    ) -> Ref<NodeBuilderContext> {
        self.arena().node_builder_context(node_builder_context)
    }

    #[track_caller]
    fn alloc_node_builder_context(
        &self,
        node_builder_context: NodeBuilderContext,
    ) -> Id<NodeBuilderContext> {
        self.arena()
            .alloc_node_builder_context(node_builder_context)
    }

    #[track_caller]
    fn option_vec_type(
        &self,
        option_vec_type: Id<Option<Vec<Id<Type>>>>,
    ) -> Ref<Option<Vec<Id<Type>>>> {
        self.arena().option_vec_type(option_vec_type)
    }

    #[track_caller]
    fn option_vec_type_mut(
        &self,
        option_vec_type: Id<Option<Vec<Id<Type>>>>,
    ) -> RefMut<Option<Vec<Id<Type>>>> {
        self.arena().option_vec_type_mut(option_vec_type)
    }

    #[track_caller]
    fn alloc_option_vec_type(
        &self,
        option_vec_type: Option<Vec<Id<Type>>>,
    ) -> Id<Option<Vec<Id<Type>>>> {
        self.arena().alloc_option_vec_type(option_vec_type)
    }

    #[track_caller]
    fn option_type_parameter_names(
        &self,
        option_type_parameter_names: Id<Option<HashMap<TypeId, Id<Node>>>>,
    ) -> Ref<Option<HashMap<TypeId, Id<Node>>>> {
        self.arena()
            .option_type_parameter_names(option_type_parameter_names)
    }

    #[track_caller]
    fn option_type_parameter_names_mut(
        &self,
        option_type_parameter_names: Id<Option<HashMap<TypeId, Id<Node>>>>,
    ) -> RefMut<Option<HashMap<TypeId, Id<Node>>>> {
        self.arena()
            .option_type_parameter_names_mut(option_type_parameter_names)
    }

    #[track_caller]
    fn alloc_option_type_parameter_names(
        &self,
        option_type_parameter_names: Option<HashMap<TypeId, Id<Node>>>,
    ) -> Id<Option<HashMap<TypeId, Id<Node>>>> {
        self.arena()
            .alloc_option_type_parameter_names(option_type_parameter_names)
    }

    #[track_caller]
    fn option_vec_symbol(
        &self,
        option_vec_symbol: Id<Option<Vec<Id<Symbol>>>>,
    ) -> Ref<Option<Vec<Id<Symbol>>>> {
        self.arena().option_vec_symbol(option_vec_symbol)
    }

    #[track_caller]
    fn option_vec_symbol_mut(
        &self,
        option_vec_symbol: Id<Option<Vec<Id<Symbol>>>>,
    ) -> RefMut<Option<Vec<Id<Symbol>>>> {
        self.arena().option_vec_symbol_mut(option_vec_symbol)
    }

    #[track_caller]
    fn alloc_option_vec_symbol(
        &self,
        option_vec_symbol: Option<Vec<Id<Symbol>>>,
    ) -> Id<Option<Vec<Id<Symbol>>>> {
        self.arena().alloc_option_vec_symbol(option_vec_symbol)
    }

    #[track_caller]
    fn type_comparer(
        &self,
        type_comparer: Id<Box<dyn TypeComparer>>,
    ) -> Ref<Box<dyn TypeComparer>> {
        self.arena().type_comparer(type_comparer)
    }

    #[track_caller]
    fn alloc_type_comparer(
        &self,
        type_comparer: Box<dyn TypeComparer>,
    ) -> Id<Box<dyn TypeComparer>> {
        self.arena().alloc_type_comparer(type_comparer)
    }

    #[track_caller]
    fn inference_context(&self, inference_context: Id<InferenceContext>) -> Ref<InferenceContext> {
        self.arena().inference_context(inference_context)
    }

    #[track_caller]
    fn alloc_inference_context(&self, inference_context: InferenceContext) -> Id<InferenceContext> {
        self.arena().alloc_inference_context(inference_context)
    }

    #[track_caller]
    fn skip_trivia(&self, skip_trivia: Id<Box<dyn SkipTrivia>>) -> Ref<Box<dyn SkipTrivia>> {
        self.arena().skip_trivia(skip_trivia)
    }

    #[track_caller]
    fn alloc_skip_trivia(&self, skip_trivia: Box<dyn SkipTrivia>) -> Id<Box<dyn SkipTrivia>> {
        self.arena().alloc_skip_trivia(skip_trivia)
    }

    #[track_caller]
    fn custom_transformer_factory_interface(
        &self,
        custom_transformer_factory_interface: Id<Box<dyn CustomTransformerFactoryInterface>>,
    ) -> Ref<Box<dyn CustomTransformerFactoryInterface>> {
        self.arena()
            .custom_transformer_factory_interface(custom_transformer_factory_interface)
    }

    #[track_caller]
    fn alloc_custom_transformer_factory_interface(
        &self,
        custom_transformer_factory_interface: Box<dyn CustomTransformerFactoryInterface>,
    ) -> Id<Box<dyn CustomTransformerFactoryInterface>> {
        self.arena()
            .alloc_custom_transformer_factory_interface(custom_transformer_factory_interface)
    }

    #[track_caller]
    fn custom_transformer_interface(
        &self,
        custom_transformer_interface: Id<Box<dyn CustomTransformerInterface>>,
    ) -> Ref<Box<dyn CustomTransformerInterface>> {
        self.arena()
            .custom_transformer_interface(custom_transformer_interface)
    }

    #[track_caller]
    fn alloc_custom_transformer_interface(
        &self,
        custom_transformer_interface: Box<dyn CustomTransformerInterface>,
    ) -> Id<Box<dyn CustomTransformerInterface>> {
        self.arena()
            .alloc_custom_transformer_interface(custom_transformer_interface)
    }

    #[track_caller]
    fn node_links(&self, node_links: Id<NodeLinks>) -> Ref<NodeLinks> {
        self.arena().node_links(node_links)
    }

    #[track_caller]
    fn node_links_mut(&self, node_links: Id<NodeLinks>) -> RefMut<NodeLinks> {
        self.arena().node_links_mut(node_links)
    }

    #[track_caller]
    fn alloc_node_links(&self, node_links: NodeLinks) -> Id<NodeLinks> {
        self.arena().alloc_node_links(node_links)
    }

    #[track_caller]
    fn parser(&self, parser: Id<ParserType>) -> Ref<ParserType> {
        self.arena().parser(parser)
    }

    #[track_caller]
    fn alloc_parser(&self, parser: ParserType) -> Id<ParserType> {
        self.arena().alloc_parser(parser)
    }

    #[track_caller]
    fn incremental_parser_syntax_cursor(
        &self,
        incremental_parser_syntax_cursor: Id<IncrementalParserSyntaxCursor>,
    ) -> Ref<IncrementalParserSyntaxCursor> {
        self.arena()
            .incremental_parser_syntax_cursor(incremental_parser_syntax_cursor)
    }

    #[track_caller]
    fn alloc_incremental_parser_syntax_cursor(
        &self,
        incremental_parser_syntax_cursor: IncrementalParserSyntaxCursor,
    ) -> Id<IncrementalParserSyntaxCursor> {
        self.arena()
            .alloc_incremental_parser_syntax_cursor(incremental_parser_syntax_cursor)
    }

    #[track_caller]
    fn command_line_option(
        &self,
        command_line_option: Id<CommandLineOption>,
    ) -> Ref<CommandLineOption> {
        self.arena().command_line_option(command_line_option)
    }

    #[track_caller]
    fn alloc_command_line_option(
        &self,
        command_line_option: CommandLineOption,
    ) -> Id<CommandLineOption> {
        self.arena().alloc_command_line_option(command_line_option)
    }

    #[track_caller]
    fn vec_command_line_option(
        &self,
        vec_command_line_option: Id<Vec<Id<CommandLineOption>>>,
    ) -> Ref<Vec<Id<CommandLineOption>>> {
        self.arena()
            .vec_command_line_option(vec_command_line_option)
    }

    #[track_caller]
    fn alloc_vec_command_line_option(
        &self,
        vec_command_line_option: Vec<Id<CommandLineOption>>,
    ) -> Id<Vec<Id<CommandLineOption>>> {
        self.arena()
            .alloc_vec_command_line_option(vec_command_line_option)
    }

    #[track_caller]
    fn options_name_map(&self, options_name_map: Id<OptionsNameMap>) -> Ref<OptionsNameMap> {
        self.arena().options_name_map(options_name_map)
    }

    #[track_caller]
    fn alloc_options_name_map(&self, options_name_map: OptionsNameMap) -> Id<OptionsNameMap> {
        self.arena().alloc_options_name_map(options_name_map)
    }

    #[track_caller]
    fn command_line_options_map(
        &self,
        command_line_options_map: Id<HashMap<String, Id<CommandLineOption>>>,
    ) -> Ref<HashMap<String, Id<CommandLineOption>>> {
        self.arena()
            .command_line_options_map(command_line_options_map)
    }

    #[track_caller]
    fn alloc_command_line_options_map(
        &self,
        command_line_options_map: HashMap<String, Id<CommandLineOption>>,
    ) -> Id<HashMap<String, Id<CommandLineOption>>> {
        self.arena()
            .alloc_command_line_options_map(command_line_options_map)
    }

    #[track_caller]
    fn node_symbol_override(
        &self,
        node_symbol_override: Id<Box<dyn NodeSymbolOverride>>,
    ) -> Ref<Box<dyn NodeSymbolOverride>> {
        self.arena().node_symbol_override(node_symbol_override)
    }

    #[track_caller]
    fn alloc_node_symbol_override(
        &self,
        node_symbol_override: Box<dyn NodeSymbolOverride>,
    ) -> Id<Box<dyn NodeSymbolOverride>> {
        self.arena()
            .alloc_node_symbol_override(node_symbol_override)
    }

    #[track_caller]
    fn node_id_override(
        &self,
        node_id_override: Id<Box<dyn NodeIdOverride>>,
    ) -> Ref<Box<dyn NodeIdOverride>> {
        self.arena().node_id_override(node_id_override)
    }

    #[track_caller]
    fn alloc_node_id_override(
        &self,
        node_id_override: Box<dyn NodeIdOverride>,
    ) -> Id<Box<dyn NodeIdOverride>> {
        self.arena().alloc_node_id_override(node_id_override)
    }

    #[track_caller]
    fn make_serialize_property_symbol_create_property(
        &self,
        make_serialize_property_symbol_create_property: Id<
            Box<dyn MakeSerializePropertySymbolCreateProperty>,
        >,
    ) -> Ref<Box<dyn MakeSerializePropertySymbolCreateProperty>> {
        self.arena().make_serialize_property_symbol_create_property(
            make_serialize_property_symbol_create_property,
        )
    }

    #[track_caller]
    fn alloc_make_serialize_property_symbol_create_property(
        &self,
        make_serialize_property_symbol_create_property: Box<
            dyn MakeSerializePropertySymbolCreateProperty,
        >,
    ) -> Id<Box<dyn MakeSerializePropertySymbolCreateProperty>> {
        self.arena()
            .alloc_make_serialize_property_symbol_create_property(
                make_serialize_property_symbol_create_property,
            )
    }

    #[track_caller]
    fn symbol_table_to_declaration_statements(
        &self,
        symbol_table_to_declaration_statements: Id<SymbolTableToDeclarationStatements>,
    ) -> Ref<SymbolTableToDeclarationStatements> {
        self.arena()
            .symbol_table_to_declaration_statements(symbol_table_to_declaration_statements)
    }

    #[track_caller]
    fn alloc_symbol_table_to_declaration_statements(
        &self,
        symbol_table_to_declaration_statements: SymbolTableToDeclarationStatements,
    ) -> Id<SymbolTableToDeclarationStatements> {
        self.arena()
            .alloc_symbol_table_to_declaration_statements(symbol_table_to_declaration_statements)
    }

    #[track_caller]
    fn input_files_initialized_state(
        &self,
        input_files_initialized_state: Id<InputFilesInitializedState>,
    ) -> Ref<InputFilesInitializedState> {
        self.arena()
            .input_files_initialized_state(input_files_initialized_state)
    }

    #[track_caller]
    fn alloc_input_files_initialized_state(
        &self,
        input_files_initialized_state: InputFilesInitializedState,
    ) -> Id<InputFilesInitializedState> {
        self.arena()
            .alloc_input_files_initialized_state(input_files_initialized_state)
    }

    #[track_caller]
    fn vec_symbol_table(
        &self,
        vec_symbol_table: Id<Vec<Id<SymbolTable>>>,
    ) -> Ref<Vec<Id<SymbolTable>>> {
        self.arena().vec_symbol_table(vec_symbol_table)
    }

    #[track_caller]
    fn vec_symbol_table_mut(
        &self,
        vec_symbol_table: Id<Vec<Id<SymbolTable>>>,
    ) -> RefMut<Vec<Id<SymbolTable>>> {
        self.arena().vec_symbol_table_mut(vec_symbol_table)
    }

    #[track_caller]
    fn alloc_vec_symbol_table(
        &self,
        vec_symbol_table: Vec<Id<SymbolTable>>,
    ) -> Id<Vec<Id<SymbolTable>>> {
        self.arena().alloc_vec_symbol_table(vec_symbol_table)
    }

    #[track_caller]
    fn check_type_related_to(
        &self,
        check_type_related_to: Id<CheckTypeRelatedTo>,
    ) -> Ref<CheckTypeRelatedTo> {
        self.arena().check_type_related_to(check_type_related_to)
    }

    #[track_caller]
    fn alloc_check_type_related_to(
        &self,
        check_type_related_to: CheckTypeRelatedTo,
    ) -> Id<CheckTypeRelatedTo> {
        self.arena()
            .alloc_check_type_related_to(check_type_related_to)
    }

    #[track_caller]
    fn flow_loop_cache(
        &self,
        flow_loop_cache: Id<HashMap<String, Id<Type>>>,
    ) -> Ref<HashMap<String, Id<Type>>> {
        self.arena().flow_loop_cache(flow_loop_cache)
    }

    #[track_caller]
    fn flow_loop_cache_mut(
        &self,
        flow_loop_cache: Id<HashMap<String, Id<Type>>>,
    ) -> RefMut<HashMap<String, Id<Type>>> {
        self.arena().flow_loop_cache_mut(flow_loop_cache)
    }

    #[track_caller]
    fn alloc_flow_loop_cache(
        &self,
        flow_loop_cache: HashMap<String, Id<Type>>,
    ) -> Id<HashMap<String, Id<Type>>> {
        self.arena().alloc_flow_loop_cache(flow_loop_cache)
    }

    #[track_caller]
    fn vec_symbol(&self, vec_symbol: Id<Vec<Id<Symbol>>>) -> Ref<Vec<Id<Symbol>>> {
        self.arena().vec_symbol(vec_symbol)
    }

    #[track_caller]
    fn alloc_vec_symbol(&self, vec_symbol: Vec<Id<Symbol>>) -> Id<Vec<Id<Symbol>>> {
        self.arena().alloc_vec_symbol(vec_symbol)
    }

    #[track_caller]
    fn vec_node(&self, vec_node: Id<Vec<Id<Node>>>) -> Ref<Vec<Id<Node>>> {
        self.arena().vec_node(vec_node)
    }

    #[track_caller]
    fn alloc_vec_node(&self, vec_node: Vec<Id<Node>>) -> Id<Vec<Id<Node>>> {
        self.arena().alloc_vec_node(vec_node)
    }

    #[track_caller]
    fn type_mapper_callback(
        &self,
        type_mapper_callback: Id<Box<dyn TypeMapperCallback>>,
    ) -> Ref<Box<dyn TypeMapperCallback>> {
        self.arena().type_mapper_callback(type_mapper_callback)
    }

    #[track_caller]
    fn alloc_type_mapper_callback(
        &self,
        type_mapper_callback: Box<dyn TypeMapperCallback>,
    ) -> Id<Box<dyn TypeMapperCallback>> {
        self.arena()
            .alloc_type_mapper_callback(type_mapper_callback)
    }

    #[track_caller]
    fn option_symbol_table(
        &self,
        option_symbol_table: Id<Option<Id<SymbolTable>>>,
    ) -> Ref<Option<Id<SymbolTable>>> {
        self.arena().option_symbol_table(option_symbol_table)
    }

    #[track_caller]
    fn option_symbol_table_mut(
        &self,
        option_symbol_table: Id<Option<Id<SymbolTable>>>,
    ) -> RefMut<Option<Id<SymbolTable>>> {
        self.arena().option_symbol_table_mut(option_symbol_table)
    }

    #[track_caller]
    fn alloc_option_symbol_table(
        &self,
        option_symbol_table: Option<Id<SymbolTable>>,
    ) -> Id<Option<Id<SymbolTable>>> {
        self.arena().alloc_option_symbol_table(option_symbol_table)
    }

    #[track_caller]
    fn cache_with_redirects_per_module_name_cache(
        &self,
        cache_with_redirects_per_module_name_cache: Id<CacheWithRedirects<PerModuleNameCache>>,
    ) -> Ref<CacheWithRedirects<PerModuleNameCache>> {
        self.arena()
            .cache_with_redirects_per_module_name_cache(cache_with_redirects_per_module_name_cache)
    }

    #[track_caller]
    fn alloc_cache_with_redirects_per_module_name_cache(
        &self,
        cache_with_redirects_per_module_name_cache: CacheWithRedirects<PerModuleNameCache>,
    ) -> Id<CacheWithRedirects<PerModuleNameCache>> {
        self.arena()
            .alloc_cache_with_redirects_per_module_name_cache(
                cache_with_redirects_per_module_name_cache,
            )
    }

    #[track_caller]
    fn cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations(
        &self,
        cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations: Id<
            CacheWithRedirects<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>,
        >,
    ) -> Ref<CacheWithRedirects<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>> {
        self.arena()
            .cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations(
                cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations,
            )
    }

    #[track_caller]
    fn alloc_cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations(
        &self,
        cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations: CacheWithRedirects<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>,
    ) -> Id<CacheWithRedirects<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>> {
        self.arena().alloc_cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations(cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations)
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
        self.arena().cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations)
    }

    #[track_caller]
    fn alloc_cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(
        &self,
        cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations: CacheWithRedirects<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
    ) -> Id<
        CacheWithRedirects<
            ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
        >,
    > {
        self.arena().alloc_cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations)
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
        self.arena()
            .mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map(
                mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map,
            )
    }

    #[track_caller]
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
        self.arena()
            .mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map_mut(
                mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map,
            )
    }

    #[track_caller]
    fn alloc_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map(
        &self,
        mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map: HashMap<String, Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>>,
    ) -> Id<
        HashMap<
            String,
            Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
        >,
    > {
        self.arena().alloc_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map(mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map)
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
        self.arena().path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map(path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map)
    }

    #[track_caller]
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
        self.arena().alloc_path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map(path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map)
    }

    #[track_caller]
    fn mode_aware_cache_resolved_module_with_failed_lookup_locations_map(
        &self,
        mode_aware_cache_resolved_module_with_failed_lookup_locations_map: Id<
            HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>,
        >,
    ) -> Ref<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>> {
        self.arena()
            .mode_aware_cache_resolved_module_with_failed_lookup_locations_map(
                mode_aware_cache_resolved_module_with_failed_lookup_locations_map,
            )
    }

    #[track_caller]
    fn mode_aware_cache_resolved_module_with_failed_lookup_locations_map_mut(
        &self,
        mode_aware_cache_resolved_module_with_failed_lookup_locations_map: Id<
            HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>,
        >,
    ) -> RefMut<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>>
    {
        self.arena()
            .mode_aware_cache_resolved_module_with_failed_lookup_locations_map_mut(
                mode_aware_cache_resolved_module_with_failed_lookup_locations_map,
            )
    }

    #[track_caller]
    fn alloc_mode_aware_cache_resolved_module_with_failed_lookup_locations_map(
        &self,
        mode_aware_cache_resolved_module_with_failed_lookup_locations_map: HashMap<
            String,
            Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>,
        >,
    ) -> Id<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>> {
        self.arena()
            .alloc_mode_aware_cache_resolved_module_with_failed_lookup_locations_map(
                mode_aware_cache_resolved_module_with_failed_lookup_locations_map,
            )
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
        self.arena()
            .path_mode_aware_cache_resolved_module_with_failed_lookup_locations_map(
                path_mode_aware_cache_resolved_module_with_failed_lookup_locations_map,
            )
    }

    #[track_caller]
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
        self.arena()
            .alloc_path_mode_aware_cache_resolved_module_with_failed_lookup_locations_map(
                path_mode_aware_cache_resolved_module_with_failed_lookup_locations_map,
            )
    }

    #[track_caller]
    fn per_module_name_cache_map(
        &self,
        per_module_name_cache_map: Id<HashMap<String, Id<PerModuleNameCache>>>,
    ) -> Ref<HashMap<String, Id<PerModuleNameCache>>> {
        self.arena()
            .per_module_name_cache_map(per_module_name_cache_map)
    }

    #[track_caller]
    fn per_module_name_cache_map_mut(
        &self,
        per_module_name_cache_map: Id<HashMap<String, Id<PerModuleNameCache>>>,
    ) -> RefMut<HashMap<String, Id<PerModuleNameCache>>> {
        self.arena()
            .per_module_name_cache_map_mut(per_module_name_cache_map)
    }

    #[track_caller]
    fn alloc_per_module_name_cache_map(
        &self,
        per_module_name_cache_map: HashMap<String, Id<PerModuleNameCache>>,
    ) -> Id<HashMap<String, Id<PerModuleNameCache>>> {
        self.arena()
            .alloc_per_module_name_cache_map(per_module_name_cache_map)
    }

    #[track_caller]
    fn path_per_module_name_cache_map(
        &self,
        path_per_module_name_cache_map: Id<
            HashMap<Path, Id<HashMap<String, Id<PerModuleNameCache>>>>,
        >,
    ) -> Ref<HashMap<Path, Id<HashMap<String, Id<PerModuleNameCache>>>>> {
        self.arena()
            .path_per_module_name_cache_map(path_per_module_name_cache_map)
    }

    #[track_caller]
    fn alloc_path_per_module_name_cache_map(
        &self,
        path_per_module_name_cache_map: HashMap<Path, Id<HashMap<String, Id<PerModuleNameCache>>>>,
    ) -> Id<HashMap<Path, Id<HashMap<String, Id<PerModuleNameCache>>>>> {
        self.arena()
            .alloc_path_per_module_name_cache_map(path_per_module_name_cache_map)
    }

    #[track_caller]
    fn logging_host(&self, logging_host: Id<Box<dyn LoggingHost>>) -> Ref<Box<dyn LoggingHost>> {
        self.arena().logging_host(logging_host)
    }

    #[track_caller]
    fn alloc_logging_host(&self, logging_host: Box<dyn LoggingHost>) -> Id<Box<dyn LoggingHost>> {
        self.arena().alloc_logging_host(logging_host)
    }

    #[track_caller]
    fn parse_command_line_worker_diagnostics(
        &self,
        parse_command_line_worker_diagnostics: Id<Box<dyn ParseCommandLineWorkerDiagnostics>>,
    ) -> Ref<Box<dyn ParseCommandLineWorkerDiagnostics>> {
        self.arena()
            .parse_command_line_worker_diagnostics(parse_command_line_worker_diagnostics)
    }

    #[track_caller]
    fn alloc_parse_command_line_worker_diagnostics(
        &self,
        parse_command_line_worker_diagnostics: Box<dyn ParseCommandLineWorkerDiagnostics>,
    ) -> Id<Box<dyn ParseCommandLineWorkerDiagnostics>> {
        self.arena()
            .alloc_parse_command_line_worker_diagnostics(parse_command_line_worker_diagnostics)
    }

    #[track_caller]
    fn did_you_mean_options_diagnostics(
        &self,
        did_you_mean_options_diagnostics: Id<Box<dyn DidYouMeanOptionsDiagnostics>>,
    ) -> Ref<Box<dyn DidYouMeanOptionsDiagnostics>> {
        self.arena()
            .did_you_mean_options_diagnostics(did_you_mean_options_diagnostics)
    }

    #[track_caller]
    fn alloc_did_you_mean_options_diagnostics(
        &self,
        did_you_mean_options_diagnostics: Box<dyn DidYouMeanOptionsDiagnostics>,
    ) -> Id<Box<dyn DidYouMeanOptionsDiagnostics>> {
        self.arena()
            .alloc_did_you_mean_options_diagnostics(did_you_mean_options_diagnostics)
    }
}
