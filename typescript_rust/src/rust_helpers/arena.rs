use std::{any::Any, rc::Rc};

use debug_cell::{Ref, RefCell};
use id_arena::{Arena, Id};
use once_cell::unsync::Lazy;

use crate::{
    Node, Symbol, Type, TypeInterface, TypeMapper, TransformNodesTransformationResult, TransformerInterface, Transformer,
    TransformerFactoryInterface, EmitTextWriter, SymbolTracker, EmitHost, ModuleSpecifierResolutionHostAndGetCommonSourceDirectory,
    FileIncludeReason,
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
