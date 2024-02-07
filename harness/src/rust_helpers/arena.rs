use typescript_rust::{
    debug_cell::{Ref, RefCell, RefMut},
    id_arena::{Arena, Id},
    AllArenas, HasArena,
};

use crate::{
    collections::{Metadata, SortOptionsComparer, SortedMap},
    compiler::CompilationOutput,
    documents::TextDocument,
    fakes::{ParseConfigHost, System},
    harness::harness_io::NodeIO,
    vfs::{
        FileSystem, FileSystemResolver, FileSystemResolverHost, StringComparer,
        TimestampOrNowOrSystemTimeOrCallbackCallback,
    },
    Compiler::TestFile,
    Inode, MetaValue, RunnerBaseSub,
    Utils::{DiagnosticMessageReplacer, Replacer},
};

#[derive(Default)]
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
    test_files: RefCell<Arena<TestFile>>,
    replacers: RefCell<Arena<Box<dyn Replacer>>>,
    diagnostic_message_replacers: RefCell<Arena<DiagnosticMessageReplacer>>,
    systems: RefCell<Arena<System>>,
    parse_config_hosts: RefCell<Arena<ParseConfigHost>>,
    string_comparers: RefCell<Arena<Box<dyn StringComparer>>>,
    file_system_resolvers: RefCell<Arena<FileSystemResolver>>,
    inodes: RefCell<Arena<Inode>>,
    links: RefCell<Arena<SortedMap<String, Id<Inode>>>>,
    timestamp_or_now_or_system_time_or_callback_callbacks:
        RefCell<Arena<Box<dyn TimestampOrNowOrSystemTimeOrCallbackCallback>>>,
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

    fn test_file(&self, test_file: Id<TestFile>) -> Ref<TestFile> {
        self.arena_harness().test_file(test_file)
    }

    fn alloc_test_file(&self, test_file: TestFile) -> Id<TestFile> {
        self.arena_harness().alloc_test_file(test_file)
    }

    fn replacer(&self, replacer: Id<Box<dyn Replacer>>) -> Ref<Box<dyn Replacer>> {
        self.arena_harness().replacer(replacer)
    }

    fn alloc_replacer(&self, replacer: Box<dyn Replacer>) -> Id<Box<dyn Replacer>> {
        self.arena_harness().alloc_replacer(replacer)
    }

    fn diagnostic_message_replacer(
        &self,
        diagnostic_message_replacer: Id<DiagnosticMessageReplacer>,
    ) -> Ref<DiagnosticMessageReplacer> {
        self.arena_harness()
            .diagnostic_message_replacer(diagnostic_message_replacer)
    }

    fn alloc_diagnostic_message_replacer(
        &self,
        diagnostic_message_replacer: DiagnosticMessageReplacer,
    ) -> Id<DiagnosticMessageReplacer> {
        self.arena_harness()
            .alloc_diagnostic_message_replacer(diagnostic_message_replacer)
    }

    fn fakes_system(&self, system: Id<System>) -> Ref<System> {
        self.arena_harness().fakes_system(system)
    }

    fn alloc_fakes_system(&self, system: System) -> Id<System> {
        self.arena_harness().alloc_fakes_system(system)
    }

    fn fakes_parse_config_host(
        &self,
        parse_config_host: Id<ParseConfigHost>,
    ) -> Ref<ParseConfigHost> {
        self.arena_harness()
            .fakes_parse_config_host(parse_config_host)
    }

    fn alloc_fakes_parse_config_host(
        &self,
        parse_config_host: ParseConfigHost,
    ) -> Id<ParseConfigHost> {
        self.arena_harness()
            .alloc_fakes_parse_config_host(parse_config_host)
    }

    fn string_comparer(
        &self,
        string_comparer: Id<Box<dyn StringComparer>>,
    ) -> Ref<Box<dyn StringComparer>> {
        self.arena_harness().string_comparer(string_comparer)
    }

    fn alloc_string_comparer(
        &self,
        string_comparer: Box<dyn StringComparer>,
    ) -> Id<Box<dyn StringComparer>> {
        self.arena_harness().alloc_string_comparer(string_comparer)
    }

    fn file_system_resolver(
        &self,
        file_system_resolver: Id<FileSystemResolver>,
    ) -> Ref<FileSystemResolver> {
        self.arena_harness()
            .file_system_resolver(file_system_resolver)
    }

    fn alloc_file_system_resolver(
        &self,
        file_system_resolver: FileSystemResolver,
    ) -> Id<FileSystemResolver> {
        self.arena_harness()
            .alloc_file_system_resolver(file_system_resolver)
    }

    fn inode(&self, inode: Id<Inode>) -> Ref<Inode> {
        self.arena_harness().inode(inode)
    }

    fn alloc_inode(&self, inode: Inode) -> Id<Inode> {
        self.arena_harness().alloc_inode(inode)
    }

    fn links(&self, links: Id<SortedMap<String, Id<Inode>>>) -> Ref<SortedMap<String, Id<Inode>>> {
        self.arena_harness().links(links)
    }

    fn links_mut(
        &self,
        links: Id<SortedMap<String, Id<Inode>>>,
    ) -> RefMut<SortedMap<String, Id<Inode>>> {
        self.arena_harness().links_mut(links)
    }

    fn alloc_links(&self, links: SortedMap<String, Id<Inode>>) -> Id<SortedMap<String, Id<Inode>>> {
        self.arena_harness().alloc_links(links)
    }

    fn timestamp_or_now_or_system_time_or_callback_callback(
        &self,
        timestamp_or_now_or_system_time_or_callback_callback: Id<
            Box<dyn TimestampOrNowOrSystemTimeOrCallbackCallback>,
        >,
    ) -> Ref<Box<dyn TimestampOrNowOrSystemTimeOrCallbackCallback>> {
        self.arena_harness()
            .timestamp_or_now_or_system_time_or_callback_callback(
                timestamp_or_now_or_system_time_or_callback_callback,
            )
    }

    fn alloc_timestamp_or_now_or_system_time_or_callback_callback(
        &self,
        timestamp_or_now_or_system_time_or_callback_callback: Box<
            dyn TimestampOrNowOrSystemTimeOrCallbackCallback,
        >,
    ) -> Id<Box<dyn TimestampOrNowOrSystemTimeOrCallbackCallback>> {
        self.arena_harness()
            .alloc_timestamp_or_now_or_system_time_or_callback_callback(
                timestamp_or_now_or_system_time_or_callback_callback,
            )
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

    fn test_file(&self, test_file: Id<TestFile>) -> Ref<TestFile> {
        Ref::map(self.test_files.borrow(), |test_files| {
            &test_files[test_file]
        })
    }

    fn alloc_test_file(&self, test_file: TestFile) -> Id<TestFile> {
        let id = self.test_files.borrow_mut().alloc(test_file);
        id
    }

    fn replacer(&self, replacer: Id<Box<dyn Replacer>>) -> Ref<Box<dyn Replacer>> {
        Ref::map(self.replacers.borrow(), |replacers| &replacers[replacer])
    }

    fn alloc_replacer(&self, replacer: Box<dyn Replacer>) -> Id<Box<dyn Replacer>> {
        let id = self.replacers.borrow_mut().alloc(replacer);
        id
    }

    fn diagnostic_message_replacer(
        &self,
        diagnostic_message_replacer: Id<DiagnosticMessageReplacer>,
    ) -> Ref<DiagnosticMessageReplacer> {
        Ref::map(
            self.diagnostic_message_replacers.borrow(),
            |diagnostic_message_replacers| {
                &diagnostic_message_replacers[diagnostic_message_replacer]
            },
        )
    }

    fn alloc_diagnostic_message_replacer(
        &self,
        diagnostic_message_replacer: DiagnosticMessageReplacer,
    ) -> Id<DiagnosticMessageReplacer> {
        let id = self
            .diagnostic_message_replacers
            .borrow_mut()
            .alloc(diagnostic_message_replacer);
        id
    }

    fn fakes_system(&self, system: Id<System>) -> Ref<System> {
        Ref::map(self.systems.borrow(), |systems| &systems[system])
    }

    fn alloc_fakes_system(&self, system: System) -> Id<System> {
        let id = self.systems.borrow_mut().alloc(system);
        id
    }

    fn fakes_parse_config_host(
        &self,
        parse_config_host: Id<ParseConfigHost>,
    ) -> Ref<ParseConfigHost> {
        Ref::map(self.parse_config_hosts.borrow(), |parse_config_hosts| {
            &parse_config_hosts[parse_config_host]
        })
    }

    fn alloc_fakes_parse_config_host(
        &self,
        parse_config_host: ParseConfigHost,
    ) -> Id<ParseConfigHost> {
        let id = self
            .parse_config_hosts
            .borrow_mut()
            .alloc(parse_config_host);
        id
    }

    fn string_comparer(
        &self,
        string_comparer: Id<Box<dyn StringComparer>>,
    ) -> Ref<Box<dyn StringComparer>> {
        Ref::map(self.string_comparers.borrow(), |string_comparers| {
            &string_comparers[string_comparer]
        })
    }

    fn alloc_string_comparer(
        &self,
        string_comparer: Box<dyn StringComparer>,
    ) -> Id<Box<dyn StringComparer>> {
        let id = self.string_comparers.borrow_mut().alloc(string_comparer);
        id
    }

    fn file_system_resolver(
        &self,
        file_system_resolver: Id<FileSystemResolver>,
    ) -> Ref<FileSystemResolver> {
        Ref::map(
            self.file_system_resolvers.borrow(),
            |file_system_resolvers| &file_system_resolvers[file_system_resolver],
        )
    }

    fn alloc_file_system_resolver(
        &self,
        file_system_resolver: FileSystemResolver,
    ) -> Id<FileSystemResolver> {
        let id = self
            .file_system_resolvers
            .borrow_mut()
            .alloc(file_system_resolver);
        id
    }

    fn inode(&self, inode: Id<Inode>) -> Ref<Inode> {
        Ref::map(self.inodes.borrow(), |inodes| &inodes[inode])
    }

    fn alloc_inode(&self, inode: Inode) -> Id<Inode> {
        let id = self.inodes.borrow_mut().alloc(inode);
        id
    }

    fn links(&self, links: Id<SortedMap<String, Id<Inode>>>) -> Ref<SortedMap<String, Id<Inode>>> {
        Ref::map(self.links.borrow(), |links_| &links_[links])
    }

    fn links_mut(
        &self,
        links: Id<SortedMap<String, Id<Inode>>>,
    ) -> RefMut<SortedMap<String, Id<Inode>>> {
        RefMut::map(self.links.borrow_mut(), |links_| &mut links_[links])
    }

    fn alloc_links(&self, links: SortedMap<String, Id<Inode>>) -> Id<SortedMap<String, Id<Inode>>> {
        let id = self.links.borrow_mut().alloc(links);
        id
    }

    fn timestamp_or_now_or_system_time_or_callback_callback(
        &self,
        timestamp_or_now_or_system_time_or_callback_callback: Id<
            Box<dyn TimestampOrNowOrSystemTimeOrCallbackCallback>,
        >,
    ) -> Ref<Box<dyn TimestampOrNowOrSystemTimeOrCallbackCallback>> {
        Ref::map(
            self.timestamp_or_now_or_system_time_or_callback_callbacks
                .borrow(),
            |timestamp_or_now_or_system_time_or_callback_callbacks| {
                &timestamp_or_now_or_system_time_or_callback_callbacks
                    [timestamp_or_now_or_system_time_or_callback_callback]
            },
        )
    }

    fn alloc_timestamp_or_now_or_system_time_or_callback_callback(
        &self,
        timestamp_or_now_or_system_time_or_callback_callback: Box<
            dyn TimestampOrNowOrSystemTimeOrCallbackCallback,
        >,
    ) -> Id<Box<dyn TimestampOrNowOrSystemTimeOrCallbackCallback>> {
        let id = self
            .timestamp_or_now_or_system_time_or_callback_callbacks
            .borrow_mut()
            .alloc(timestamp_or_now_or_system_time_or_callback_callback);
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

impl InArenaHarness for Id<TestFile> {
    type Item = TestFile;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, TestFile> {
        has_arena.test_file(*self)
    }
}

impl InArenaHarness for Id<Box<dyn Replacer>> {
    type Item = Box<dyn Replacer>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, Box<dyn Replacer>> {
        has_arena.replacer(*self)
    }
}

impl InArenaHarness for Id<DiagnosticMessageReplacer> {
    type Item = DiagnosticMessageReplacer;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, DiagnosticMessageReplacer> {
        has_arena.diagnostic_message_replacer(*self)
    }
}

impl InArenaHarness for Id<System> {
    type Item = System;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, System> {
        has_arena.fakes_system(*self)
    }
}

impl InArenaHarness for Id<ParseConfigHost> {
    type Item = ParseConfigHost;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, ParseConfigHost> {
        has_arena.fakes_parse_config_host(*self)
    }
}

impl InArenaHarness for Id<Box<dyn StringComparer>> {
    type Item = Box<dyn StringComparer>;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, Box<dyn StringComparer>> {
        has_arena.string_comparer(*self)
    }
}

impl InArenaHarness for Id<FileSystemResolver> {
    type Item = FileSystemResolver;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, FileSystemResolver> {
        has_arena.file_system_resolver(*self)
    }
}

impl InArenaHarness for Id<Inode> {
    type Item = Inode;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, Inode> {
        has_arena.inode(*self)
    }
}

impl InArenaHarness for Id<SortedMap<String, Id<Inode>>> {
    type Item = SortedMap<String, Id<Inode>>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArenaHarness,
    ) -> Ref<'a, SortedMap<String, Id<Inode>>> {
        has_arena.links(*self)
    }

    fn ref_mut<'a>(
        &self,
        has_arena: &'a impl HasArenaHarness,
    ) -> RefMut<'a, SortedMap<String, Id<Inode>>> {
        has_arena.links_mut(*self)
    }
}

impl InArenaHarness for Id<Box<dyn TimestampOrNowOrSystemTimeOrCallbackCallback>> {
    type Item = Box<dyn TimestampOrNowOrSystemTimeOrCallbackCallback>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArenaHarness,
    ) -> Ref<'a, Box<dyn TimestampOrNowOrSystemTimeOrCallbackCallback>> {
        has_arena.timestamp_or_now_or_system_time_or_callback_callback(*self)
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

pub trait OptionInArenaHarness {
    type Item;

    fn refed<'a>(self, has_arena: &'a impl HasArenaHarness) -> Option<Ref<'a, Self::Item>>;
}

impl OptionInArenaHarness for Option<Id<Inode>> {
    type Item = Inode;

    fn refed<'a>(self, has_arena: &'a impl HasArenaHarness) -> Option<Ref<'a, Inode>> {
        self.map(|inode| has_arena.inode(inode))
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
