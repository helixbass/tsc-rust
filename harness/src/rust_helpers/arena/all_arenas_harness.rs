use std::sync::atomic::{AtomicUsize, Ordering};

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
    HasArenaHarness, Inode, MetaValue, RunnerBaseSub,
    Utils::{DiagnosticMessageReplacer, Replacer},
};

pub type AllArenasHarnessId = usize;

pub struct AllArenasHarness {
    id: AllArenasHarnessId,
    pub all_arenas: AllArenas,
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

impl Default for AllArenasHarness {
    fn default() -> Self {
        static ARENA_COUNTER: AtomicUsize = AtomicUsize::new(0);
        Self {
            id: ARENA_COUNTER.fetch_add(1, Ordering::SeqCst),
            all_arenas: Default::default(),
            node_ios: Default::default(),
            file_system_resolver_hosts: Default::default(),
            text_documents: Default::default(),
            compilation_outputs: Default::default(),
            file_systems: Default::default(),
            sort_options_comparer_strings: Default::default(),
            metadata_strings: Default::default(),
            metadata_metavalues: Default::default(),
            runner_base_subs: Default::default(),
            test_files: Default::default(),
            replacers: Default::default(),
            diagnostic_message_replacers: Default::default(),
            systems: Default::default(),
            parse_config_hosts: Default::default(),
            string_comparers: Default::default(),
            file_system_resolvers: Default::default(),
            inodes: Default::default(),
            links: Default::default(),
            timestamp_or_now_or_system_time_or_callback_callbacks: Default::default(),
        }
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

    fn all_arenas_harness_id(&self) -> AllArenasHarnessId {
        self.id
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
