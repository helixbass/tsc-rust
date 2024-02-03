use typescript_rust::{
    debug_cell::{Ref, RefCell, RefMut},
    id_arena::{Arena, Id},
    AllArenas, HasArena,
};

use crate::{
    collections::{Metadata, SortOptionsComparer},
    compiler::CompilationOutput,
    documents::TextDocument,
    harness::harness_io::NodeIO,
    vfs::{FileSystem, FileSystemResolverHost},
    MetaValue, RunnerBaseSub,
};

pub struct AllArenasHarness {
    all_arenas: AllArenas,
    node_ios: RefCell<Arena<NodeIO>>,
    file_system_resolver_hosts: RefCell<Arena<Box<dyn FileSystemResolverHost>>>,
    text_documents: RefCell<Arena<TextDocument>>,
    compilation_outputs: RefCell<Arena<CompilationOutput>>,
    file_systems: RefCell<Arena<FileSystem>>,
    sort_options_comparer_strings: RefCell<Arena<Box<dyn SortOptionsComparer<String>>>>,
    metadata_strings: RefCell<Arena<Metadata<String>>>,
    metadata_metavalues: RefCell<Arena<Metadata<MetaValue>>>,
    runner_base_subs: RefCell<Arena<Box<dyn RunnerBaseSub>>>,
}

pub trait HasArenaHarness: HasArena {
    fn arena_harness(&self) -> &AllArenasHarness;

    fn node_io(&self, node_io: Id<NodeIO>) -> Ref<NodeIO> {
        self.arena_harness().node_io(node_io)
    }

    fn alloc_node_io(&self, node_io: NodeIO) -> Id<NodeIO> {
        self.arena_harness().alloc_node_io(node_io)
    }

    fn file_system_resolver_host(
        &self,
        file_system_resolver_host: Id<Box<dyn FileSystemResolverHost>>,
    ) -> Ref<Box<dyn FileSystemResolverHost>> {
        self.arena_harness()
            .file_system_resolver_host(file_system_resolver_host)
    }

    fn alloc_file_system_resolver_host(
        &self,
        file_system_resolver_host: Box<dyn FileSystemResolverHost>,
    ) -> Id<Box<dyn FileSystemResolverHost>> {
        self.arena_harness()
            .alloc_file_system_resolver_host(file_system_resolver_host)
    }

    fn text_document(&self, text_document: Id<TextDocument>) -> Ref<TextDocument> {
        self.arena_harness().text_document(text_document)
    }

    fn alloc_text_document(&self, text_document: TextDocument) -> Id<TextDocument> {
        self.arena_harness().alloc_text_document(text_document)
    }

    fn compilation_output(
        &self,
        compilation_output: Id<CompilationOutput>,
    ) -> Ref<CompilationOutput> {
        self.arena_harness().compilation_output(compilation_output)
    }

    fn alloc_compilation_output(
        &self,
        compilation_output: CompilationOutput,
    ) -> Id<CompilationOutput> {
        self.arena_harness()
            .alloc_compilation_output(compilation_output)
    }

    fn file_system(&self, file_system: Id<FileSystem>) -> Ref<FileSystem> {
        self.arena_harness().file_system(file_system)
    }

    fn alloc_file_system(&self, file_system: FileSystem) -> Id<FileSystem> {
        self.arena_harness().alloc_file_system(file_system)
    }

    fn sort_options_comparer_string(
        &self,
        sort_options_comparer_string: Id<Box<dyn SortOptionsComparer<String>>>,
    ) -> Ref<Box<dyn SortOptionsComparer<String>>> {
        self.arena_harness()
            .sort_options_comparer_string(sort_options_comparer_string)
    }

    fn alloc_sort_options_comparer_string(
        &self,
        sort_options_comparer_string: Box<dyn SortOptionsComparer<String>>,
    ) -> Id<Box<dyn SortOptionsComparer<String>>> {
        self.arena_harness()
            .alloc_sort_options_comparer_string(sort_options_comparer_string)
    }

    fn metadata_string(&self, metadata_string: Id<Metadata<String>>) -> Ref<Metadata<String>> {
        self.arena_harness().metadata_string(metadata_string)
    }

    fn metadata_string_mut(
        &self,
        metadata_string: Id<Metadata<String>>,
    ) -> RefMut<Metadata<String>> {
        self.arena_harness().metadata_string_mut(metadata_string)
    }

    fn alloc_metadata_string(&self, metadata_string: Metadata<String>) -> Id<Metadata<String>> {
        self.arena_harness().alloc_metadata_string(metadata_string)
    }

    fn metadata_metavalue(
        &self,
        metadata_metavalue: Id<Metadata<MetaValue>>,
    ) -> Ref<Metadata<MetaValue>> {
        self.arena_harness().metadata_metavalue(metadata_metavalue)
    }

    fn metadata_metavalue_mut(
        &self,
        metadata_metavalue: Id<Metadata<MetaValue>>,
    ) -> RefMut<Metadata<MetaValue>> {
        self.arena_harness()
            .metadata_metavalue_mut(metadata_metavalue)
    }

    fn alloc_metadata_metavalue(
        &self,
        metadata_metavalue: Metadata<MetaValue>,
    ) -> Id<Metadata<MetaValue>> {
        self.arena_harness()
            .alloc_metadata_metavalue(metadata_metavalue)
    }

    fn runner_base_sub(
        &self,
        runner_base_sub: Id<Box<dyn RunnerBaseSub>>,
    ) -> Ref<Box<dyn RunnerBaseSub>> {
        self.arena_harness().runner_base_sub(runner_base_sub)
    }

    fn alloc_runner_base_sub(
        &self,
        runner_base_sub: Box<dyn RunnerBaseSub>,
    ) -> Id<Box<dyn RunnerBaseSub>> {
        self.arena_harness().alloc_runner_base_sub(runner_base_sub)
    }
}

impl HasArena for AllArenasHarness {
    fn arena(&self) -> &AllArenas {
        &self.all_arenas
    }
}

impl HasArenaHarness for AllArenasHarness {
    fn arena_harness(&self) -> &AllArenasHarness {
        self
    }

    fn node_io(&self, node_io: Id<NodeIO>) -> Ref<NodeIO> {
        Ref::map(self.node_ios.borrow(), |node_ios| &node_ios[node_io])
    }

    fn alloc_node_io(&self, node_io: NodeIO) -> Id<NodeIO> {
        let id = self.node_ios.borrow_mut().alloc(node_io);
        id
    }

    fn file_system_resolver_host(
        &self,
        file_system_resolver_host: Id<Box<dyn FileSystemResolverHost>>,
    ) -> Ref<Box<dyn FileSystemResolverHost>> {
        Ref::map(
            self.file_system_resolver_hosts.borrow(),
            |file_system_resolver_hosts| &file_system_resolver_hosts[file_system_resolver_host],
        )
    }

    fn alloc_file_system_resolver_host(
        &self,
        file_system_resolver_host: Box<dyn FileSystemResolverHost>,
    ) -> Id<Box<dyn FileSystemResolverHost>> {
        let id = self
            .file_system_resolver_hosts
            .borrow_mut()
            .alloc(file_system_resolver_host);
        id
    }

    fn text_document(&self, text_document: Id<TextDocument>) -> Ref<TextDocument> {
        Ref::map(self.text_documents.borrow(), |text_documents| {
            &text_documents[text_document]
        })
    }

    fn alloc_text_document(&self, text_document: TextDocument) -> Id<TextDocument> {
        let id = self.text_documents.borrow_mut().alloc(text_document);
        id
    }

    fn compilation_output(
        &self,
        compilation_output: Id<CompilationOutput>,
    ) -> Ref<CompilationOutput> {
        Ref::map(self.compilation_outputs.borrow(), |compilation_outputs| {
            &compilation_outputs[compilation_output]
        })
    }

    fn alloc_compilation_output(
        &self,
        compilation_output: CompilationOutput,
    ) -> Id<CompilationOutput> {
        let id = self
            .compilation_outputs
            .borrow_mut()
            .alloc(compilation_output);
        id
    }

    fn file_system(&self, file_system: Id<FileSystem>) -> Ref<FileSystem> {
        Ref::map(self.file_systems.borrow(), |file_systems| {
            &file_systems[file_system]
        })
    }

    fn alloc_file_system(&self, file_system: FileSystem) -> Id<FileSystem> {
        let id = self.file_systems.borrow_mut().alloc(file_system);
        id
    }

    fn sort_options_comparer_string(
        &self,
        sort_options_comparer_string: Id<Box<dyn SortOptionsComparer<String>>>,
    ) -> Ref<Box<dyn SortOptionsComparer<String>>> {
        Ref::map(
            self.sort_options_comparer_strings.borrow(),
            |sort_options_comparer_strings| {
                &sort_options_comparer_strings[sort_options_comparer_string]
            },
        )
    }

    fn alloc_sort_options_comparer_string(
        &self,
        sort_options_comparer_string: Box<dyn SortOptionsComparer<String>>,
    ) -> Id<Box<dyn SortOptionsComparer<String>>> {
        let id = self
            .sort_options_comparer_strings
            .borrow_mut()
            .alloc(sort_options_comparer_string);
        id
    }

    fn metadata_string(&self, metadata_string: Id<Metadata<String>>) -> Ref<Metadata<String>> {
        Ref::map(self.metadata_strings.borrow(), |metadata_strings| {
            &metadata_strings[metadata_string]
        })
    }

    fn metadata_string_mut(
        &self,
        metadata_string: Id<Metadata<String>>,
    ) -> RefMut<Metadata<String>> {
        RefMut::map(self.metadata_strings.borrow_mut(), |metadata_strings| {
            &mut metadata_strings[metadata_string]
        })
    }

    fn alloc_metadata_string(&self, metadata_string: Metadata<String>) -> Id<Metadata<String>> {
        let id = self.metadata_strings.borrow_mut().alloc(metadata_string);
        id
    }

    fn metadata_metavalue(
        &self,
        metadata_metavalue: Id<Metadata<MetaValue>>,
    ) -> Ref<Metadata<MetaValue>> {
        Ref::map(self.metadata_metavalues.borrow(), |metadata_metavalues| {
            &metadata_metavalues[metadata_metavalue]
        })
    }

    fn metadata_metavalue_mut(
        &self,
        metadata_metavalue: Id<Metadata<MetaValue>>,
    ) -> RefMut<Metadata<MetaValue>> {
        RefMut::map(
            self.metadata_metavalues.borrow_mut(),
            |metadata_metavalues| &mut metadata_metavalues[metadata_metavalue],
        )
    }

    fn alloc_metadata_metavalue(
        &self,
        metadata_metavalue: Metadata<MetaValue>,
    ) -> Id<Metadata<MetaValue>> {
        let id = self
            .metadata_metavalues
            .borrow_mut()
            .alloc(metadata_metavalue);
        id
    }

    fn runner_base_sub(
        &self,
        runner_base_sub: Id<Box<dyn RunnerBaseSub>>,
    ) -> Ref<Box<dyn RunnerBaseSub>> {
        Ref::map(self.runner_base_subs.borrow(), |runner_base_subs| {
            &runner_base_subs[runner_base_sub]
        })
    }

    fn alloc_runner_base_sub(
        &self,
        runner_base_sub: Box<dyn RunnerBaseSub>,
    ) -> Id<Box<dyn RunnerBaseSub>> {
        let id = self.runner_base_subs.borrow_mut().alloc(runner_base_sub);
        id
    }
}

pub trait InArenaHarness {
    type Item: ?Sized;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, Self::Item>;
    fn ref_mut<'a>(&self, _has_arena: &'a impl HasArenaHarness) -> RefMut<'a, Self::Item> {
        unimplemented!()
    }
}

impl InArenaHarness for Id<NodeIO> {
    type Item = NodeIO;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, NodeIO> {
        has_arena.node_io(*self)
    }
}

impl InArenaHarness for Id<Box<dyn FileSystemResolverHost>> {
    type Item = Box<dyn FileSystemResolverHost>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArenaHarness,
    ) -> Ref<'a, Box<dyn FileSystemResolverHost>> {
        has_arena.file_system_resolver_host(*self)
    }
}

impl InArenaHarness for Id<TextDocument> {
    type Item = TextDocument;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, TextDocument> {
        has_arena.text_document(*self)
    }
}

impl InArenaHarness for Id<CompilationOutput> {
    type Item = CompilationOutput;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, CompilationOutput> {
        has_arena.compilation_output(*self)
    }
}

impl InArenaHarness for Id<FileSystem> {
    type Item = FileSystem;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, FileSystem> {
        has_arena.file_system(*self)
    }
}

impl InArenaHarness for Id<Box<dyn SortOptionsComparer<String>>> {
    type Item = Box<dyn SortOptionsComparer<String>>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArenaHarness,
    ) -> Ref<'a, Box<dyn SortOptionsComparer<String>>> {
        has_arena.sort_options_comparer_string(*self)
    }
}

impl InArenaHarness for Id<Metadata<String>> {
    type Item = Metadata<String>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, Metadata<String>> {
        has_arena.metadata_string(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArenaHarness) -> RefMut<'a, Metadata<String>> {
        has_arena.metadata_string_mut(*self)
    }
}

impl InArenaHarness for Id<Metadata<MetaValue>> {
    type Item = Metadata<MetaValue>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, Metadata<MetaValue>> {
        has_arena.metadata_metavalue(*self)
    }

    fn ref_mut<'a>(&self, has_arena: &'a impl HasArenaHarness) -> RefMut<'a, Metadata<MetaValue>> {
        has_arena.metadata_metavalue_mut(*self)
    }
}

impl InArenaHarness for Id<Box<dyn RunnerBaseSub>> {
    type Item = Box<dyn RunnerBaseSub>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, Box<dyn RunnerBaseSub>> {
        has_arena.runner_base_sub(*self)
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum IdForFileSystemResolverHost {
    FileSystemResolverHost(Id<Box<dyn FileSystemResolverHost>>),
    NodeIO(Id<NodeIO>),
}

impl InArenaHarness for IdForFileSystemResolverHost {
    type Item = dyn FileSystemResolverHost;

    fn ref_<'a>(
        &self,
        arena: &'a impl HasArenaHarness,
    ) -> Ref<'a, dyn FileSystemResolverHost + 'static> {
        match self {
            Self::FileSystemResolverHost(value) => Ref::map(value.ref_(arena), |value| &**value),
            Self::NodeIO(value) => Ref::map(value.ref_(arena), |value| {
                value as &dyn FileSystemResolverHost
            }),
        }
    }
}

impl From<Id<Box<dyn FileSystemResolverHost>>> for IdForFileSystemResolverHost {
    fn from(value: Id<Box<dyn FileSystemResolverHost>>) -> Self {
        Self::FileSystemResolverHost(value)
    }
}

impl From<Id<NodeIO>> for IdForFileSystemResolverHost {
    fn from(value: Id<NodeIO>) -> Self {
        Self::NodeIO(value)
    }
}
