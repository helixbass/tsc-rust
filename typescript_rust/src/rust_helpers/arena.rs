use std::{any::Any, rc::Rc};

use debug_cell::{Ref, RefCell};
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
};

#[derive(Default)]
pub struct AllArenas {
    pub nodes: RefCell<Arena<Node>>,
    // pub node_arrays: RefCell<Arena<NodeArray>>,
    // pub emit_nodes: RefCell<Arena<EmitNode>>,
    pub symbols: RefCell<Arena<Symbol>>,
    // pub symbol_tables: RefCell<Arena<SymbolTable>>,
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
}

pub trait InArena {
    type Item: ?Sized;

    fn ref_<'a>(&self, has_arena: &'a impl HasArena) -> Ref<'a, Self::Item>;
    // fn ref_mut(&self) -> RefMut<Type>;
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
