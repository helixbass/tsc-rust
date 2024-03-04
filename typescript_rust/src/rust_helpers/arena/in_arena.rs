use std::collections::HashMap;

use debug_cell::{Ref, RefMut};
use id_arena::Id;

use crate::{
    ActiveLabel, ActualResolveModuleNamesWorker, ActualResolveTypeReferenceDirectiveNamesWorker,
    BaseNodeFactory, BindBinaryExpressionFlow, Binder, BuildInfo, BuilderProgram, BundleBuildInfo,
    BundleFileInfo, BundleFileSection, CacheWithRedirects, CancellationToken,
    CheckBinaryExpression, CheckTypeContainingMessageChain, CheckTypeErrorOutputContainer,
    CheckTypeRelatedTo, ClassLexicalEnvironment, CodeBlock, CommandLineOption, CompilerHost,
    CompilerHostLike, CompilerOptions, ConditionalRoot, ConvertedLoopState,
    CurrentParenthesizerRule, CustomTransformerFactoryInterface, CustomTransformerInterface,
    Diagnostic, DiagnosticRelatedInformation, DiagnosticReporter, DidYouMeanOptionsDiagnostics,
    DirectoryStructureHost, EmitBinaryExpression, EmitHelper, EmitHelperFactory,
    EmitHelperTextCallback, EmitHost, EmitNode, EmitResolver, EmitTextWriter, ExternalModuleInfo,
    FileIncludeReason, FilePreprocessingDiagnostics, FlowNode, ForEachResolvedProjectReference,
    GetCanonicalFileName, GetProgramBuildInfo, GetResolvedProjectReferences, GetSourceFile,
    GetSymbolAccessibilityDiagnosticInterface, GetSymlinkCache, HasArena,
    IncrementalParserSyntaxCursor, IndexInfo, InferenceContext, InferenceInfo,
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

pub trait InArena {
    type Item: ?Sized;

    #[track_caller]
    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Self::Item>;

    #[track_caller]
    fn ref_mut<'a>(&self, _has_arena: &'a impl HasArena) -> RefMut<'a, Self::Item> {
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
        has_arena.type_ref(*self)
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
        has_arena.symbol_ref(*self)
    }
}

impl InArena for Id<TransformNodesTransformationResult> {
    type Item = TransformNodesTransformationResult;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, TransformNodesTransformationResult> {
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn TransformerFactoryInterface>> {
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>> {
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

impl InArena for Id<SymbolLinks> {
    type Item = SymbolLinks;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, SymbolLinks> {
        has_arena.symbol_links(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, SymbolLinks> {
        has_arena.symbol_links_mut(*self)
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn ModuleResolutionHostOverrider>> {
        has_arena.module_resolution_host_overrider(*self)
    }
}

impl InArena for Id<Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
    type Item = Box<dyn WrapCustomTransformerFactoryHandleDefault>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
        has_arena.wrap_custom_transformer_factory_handle_default(*self)
    }
}

impl InArena for Id<Box<dyn TransformationContextOnEmitNodeOverrider>> {
    type Item = Box<dyn TransformationContextOnEmitNodeOverrider>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn TransformationContextOnEmitNodeOverrider>> {
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
        has_arena.get_canonical_file_name_ref(*self)
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn TransformationContextOnSubstituteNodeOverrider>> {
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, TransformerFactoryOrCustomTransformerFactory> {
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn OutofbandVarianceMarkerHandler>> {
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn GetResolvedProjectReferences>> {
        has_arena.get_resolved_project_references_ref(*self)
    }
}

impl InArena for Id<Box<dyn ForEachResolvedProjectReference>> {
    type Item = Box<dyn ForEachResolvedProjectReference>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn ForEachResolvedProjectReference>> {
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, TypeReferenceDirectiveResolutionCache> {
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn ActualResolveModuleNamesWorker>> {
        has_arena.actual_resolve_module_names_worker(*self)
    }
}

impl InArena for Id<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>> {
    type Item = Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>> {
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>> {
        has_arena.load_with_mode_aware_cache_loader(*self)
    }
}

impl InArena for Id<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>> {
    type Item = Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>> {
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

    fn ref_mut<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> RefMut<'a, PrivateIdentifierEnvironment> {
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, ResolvedModuleWithFailedLookupLocations> {
        has_arena.resolved_module_with_failed_lookup_locations(*self)
    }
}

impl InArena for Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
    type Item = ResolvedTypeReferenceDirectiveWithFailedLookupLocations;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, ResolvedTypeReferenceDirectiveWithFailedLookupLocations> {
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>> {
        has_arena.mode_aware_cache_resolved_module_with_failed_lookup_locations(*self)
    }
}

impl InArena for Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>> {
    type Item = ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>> {
        has_arena
            .mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(*self)
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, MultiMap<Path, Id<FileIncludeReason>>> {
        has_arena.file_reasons(*self)
    }

    fn ref_mut<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> RefMut<'a, MultiMap<Path, Id<FileIncludeReason>>> {
        has_arena.file_reasons_mut(*self)
    }
}

impl InArena for Id<Box<dyn GetSymbolAccessibilityDiagnosticInterface>> {
    type Item = Box<dyn GetSymbolAccessibilityDiagnosticInterface>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn GetSymbolAccessibilityDiagnosticInterface>> {
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn CheckTypeContainingMessageChain>> {
        has_arena.check_type_containing_message_chain(*self)
    }
}

impl InArena for Id<Box<dyn CheckTypeErrorOutputContainer>> {
    type Item = Box<dyn CheckTypeErrorOutputContainer>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn CheckTypeErrorOutputContainer>> {
        has_arena.check_type_error_output_container(*self)
    }
}

impl InArena for Id<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
    type Item = HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        has_arena.resolved_type_reference_directives_map(*self)
    }

    fn ref_mut<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> RefMut<'a, HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
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

    fn ref_mut<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> RefMut<'a, Option<HashMap<TypeId, Id<Node>>>> {
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

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn CustomTransformerFactoryInterface>> {
        has_arena.custom_transformer_factory_interface(*self)
    }
}

impl InArena for Id<Box<dyn CustomTransformerInterface>> {
    type Item = Box<dyn CustomTransformerInterface>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn CustomTransformerInterface>> {
        has_arena.custom_transformer_interface(*self)
    }
}

impl InArena for Id<NodeLinks> {
    type Item = NodeLinks;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, NodeLinks> {
        has_arena.node_links(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, NodeLinks> {
        has_arena.node_links_mut(*self)
    }
}

impl InArena for Id<ParserType> {
    type Item = ParserType;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, ParserType> {
        has_arena.parser(*self)
    }
}

impl InArena for Id<IncrementalParserSyntaxCursor> {
    type Item = IncrementalParserSyntaxCursor;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, IncrementalParserSyntaxCursor> {
        has_arena.incremental_parser_syntax_cursor(*self)
    }
}

impl InArena for Id<CommandLineOption> {
    type Item = CommandLineOption;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, CommandLineOption> {
        has_arena.command_line_option(*self)
    }
}

impl InArena for Id<Vec<Id<CommandLineOption>>> {
    type Item = Vec<Id<CommandLineOption>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Vec<Id<CommandLineOption>>> {
        has_arena.vec_command_line_option(*self)
    }
}

impl InArena for Id<OptionsNameMap> {
    type Item = OptionsNameMap;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, OptionsNameMap> {
        has_arena.options_name_map(*self)
    }
}

impl InArena for Id<HashMap<String, Id<CommandLineOption>>> {
    type Item = HashMap<String, Id<CommandLineOption>>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, HashMap<String, Id<CommandLineOption>>> {
        has_arena.command_line_options_map(*self)
    }
}

impl InArena for Id<Box<dyn NodeSymbolOverride>> {
    type Item = Box<dyn NodeSymbolOverride>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn NodeSymbolOverride>> {
        has_arena.node_symbol_override(*self)
    }
}

impl InArena for Id<Box<dyn NodeIdOverride>> {
    type Item = Box<dyn NodeIdOverride>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn NodeIdOverride>> {
        has_arena.node_id_override(*self)
    }
}

impl InArena for Id<Box<dyn MakeSerializePropertySymbolCreateProperty>> {
    type Item = Box<dyn MakeSerializePropertySymbolCreateProperty>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn MakeSerializePropertySymbolCreateProperty>> {
        has_arena.make_serialize_property_symbol_create_property(*self)
    }
}

impl InArena for Id<SymbolTableToDeclarationStatements> {
    type Item = SymbolTableToDeclarationStatements;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, SymbolTableToDeclarationStatements> {
        has_arena.symbol_table_to_declaration_statements(*self)
    }
}

impl InArena for Id<InputFilesInitializedState> {
    type Item = InputFilesInitializedState;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, InputFilesInitializedState> {
        has_arena.input_files_initialized_state(*self)
    }
}

impl InArena for Id<Vec<Id<SymbolTable>>> {
    type Item = Vec<Id<SymbolTable>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Vec<Id<SymbolTable>>> {
        has_arena.vec_symbol_table(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, Vec<Id<SymbolTable>>> {
        has_arena.vec_symbol_table_mut(*self)
    }
}

impl InArena for Id<CheckTypeRelatedTo> {
    type Item = CheckTypeRelatedTo;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, CheckTypeRelatedTo> {
        has_arena.check_type_related_to(*self)
    }
}

impl InArena for Id<HashMap<String, Id<Type>>> {
    type Item = HashMap<String, Id<Type>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, HashMap<String, Id<Type>>> {
        has_arena.flow_loop_cache(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, HashMap<String, Id<Type>>> {
        has_arena.flow_loop_cache_mut(*self)
    }
}

impl InArena for Id<Vec<Id<Symbol>>> {
    type Item = Vec<Id<Symbol>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Vec<Id<Symbol>>> {
        has_arena.vec_symbol(*self)
    }
}

impl InArena for Id<Vec<Id<Node>>> {
    type Item = Vec<Id<Node>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Vec<Id<Node>>> {
        has_arena.vec_node(*self)
    }
}

impl InArena for Id<Box<dyn TypeMapperCallback>> {
    type Item = Box<dyn TypeMapperCallback>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn TypeMapperCallback>> {
        has_arena.type_mapper_callback(*self)
    }
}

impl InArena for Id<Option<Id<SymbolTable>>> {
    type Item = Option<Id<SymbolTable>>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Option<Id<SymbolTable>>> {
        has_arena.option_symbol_table(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArena) -> RefMut<'a, Option<Id<SymbolTable>>> {
        has_arena.option_symbol_table_mut(*self)
    }
}

impl InArena for Id<CacheWithRedirects<PerModuleNameCache>> {
    type Item = CacheWithRedirects<PerModuleNameCache>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, CacheWithRedirects<PerModuleNameCache>> {
        has_arena.cache_with_redirects_per_module_name_cache(*self)
    }
}

impl InArena
    for Id<CacheWithRedirects<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>
{
    type Item = CacheWithRedirects<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, CacheWithRedirects<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>
    {
        has_arena
            .cache_with_redirects_mode_aware_cache_resolved_module_with_failed_lookup_locations(
                *self,
            )
    }
}

impl InArena
    for Id<
        CacheWithRedirects<
            ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
        >,
    >
{
    type Item = CacheWithRedirects<
        ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
    >;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<
        'a,
        CacheWithRedirects<
            ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>,
        >,
    > {
        has_arena.cache_with_redirects_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(*self)
    }
}

impl InArena
    for Id<
        HashMap<
            String,
            Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
        >,
    >
{
    type Item = HashMap<
        String,
        Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
    >;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<
        'a,
        HashMap<
            String,
            Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
        >,
    > {
        has_arena
            .mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map(
                *self,
            )
    }

    fn ref_mut<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> RefMut<
        'a,
        HashMap<
            String,
            Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
        >,
    > {
        has_arena
            .mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map_mut(
                *self,
            )
    }
}

impl InArena
    for Id<
        HashMap<
            Path,
            Id<
                HashMap<
                    String,
                    Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
                >,
            >,
        >,
    >
{
    type Item = HashMap<
        Path,
        Id<
            HashMap<
                String,
                Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
            >,
        >,
    >;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<
        'a,
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
        has_arena.path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map(*self)
    }
}

impl InArena
    for Id<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>>
{
    type Item = HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>>
    {
        has_arena.mode_aware_cache_resolved_module_with_failed_lookup_locations_map(*self)
    }

    fn ref_mut<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> RefMut<'a, HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>>
    {
        has_arena.mode_aware_cache_resolved_module_with_failed_lookup_locations_map_mut(*self)
    }
}

impl InArena
    for Id<
        HashMap<
            Path,
            Id<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>>,
        >,
    >
{
    type Item = HashMap<
        Path,
        Id<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>>,
    >;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<
        'a,
        HashMap<
            Path,
            Id<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>>,
        >,
    > {
        has_arena.path_mode_aware_cache_resolved_module_with_failed_lookup_locations_map(*self)
    }
}

impl InArena for Id<HashMap<String, Id<PerModuleNameCache>>> {
    type Item = HashMap<String, Id<PerModuleNameCache>>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, HashMap<String, Id<PerModuleNameCache>>> {
        has_arena.per_module_name_cache_map(*self)
    }

    fn ref_mut<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> RefMut<'a, HashMap<String, Id<PerModuleNameCache>>> {
        has_arena.per_module_name_cache_map_mut(*self)
    }
}

impl InArena for Id<HashMap<Path, Id<HashMap<String, Id<PerModuleNameCache>>>>> {
    type Item = HashMap<Path, Id<HashMap<String, Id<PerModuleNameCache>>>>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, HashMap<Path, Id<HashMap<String, Id<PerModuleNameCache>>>>> {
        has_arena.path_per_module_name_cache_map(*self)
    }
}

impl InArena for Id<Box<dyn LoggingHost>> {
    type Item = Box<dyn LoggingHost>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Box<dyn LoggingHost>> {
        has_arena.logging_host(*self)
    }
}

impl InArena for Id<Box<dyn ParseCommandLineWorkerDiagnostics>> {
    type Item = Box<dyn ParseCommandLineWorkerDiagnostics>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn ParseCommandLineWorkerDiagnostics>> {
        has_arena.parse_command_line_worker_diagnostics(*self)
    }
}

impl InArena for Id<Box<dyn DidYouMeanOptionsDiagnostics>> {
    type Item = Box<dyn DidYouMeanOptionsDiagnostics>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArena,
    ) -> Ref<'a, Box<dyn DidYouMeanOptionsDiagnostics>> {
        has_arena.did_you_mean_options_diagnostics(*self)
    }
}
