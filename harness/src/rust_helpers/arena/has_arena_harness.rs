use typescript_rust::{
    debug_cell::{Ref, RefMut},
    id_arena::Id,
    HasArena,
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
    AllArenasHarness, AllArenasHarnessId,
    Compiler::TestFile,
    Inode, MetaValue, RunnerBaseSub,
    Utils::{DiagnosticMessageReplacer, Replacer},
};

pub trait HasArenaHarness: HasArena {
    fn arena_harness(&self) -> &AllArenasHarness;

    fn all_arenas_harness_id(&self) -> AllArenasHarnessId {
        self.arena_harness().all_arenas_harness_id()
    }

    #[track_caller]
    fn node_io(&self, node_io: Id<NodeIO>) -> Ref<NodeIO> {
        self.arena_harness().node_io(node_io)
    }

    #[track_caller]
    fn alloc_node_io(&self, node_io: NodeIO) -> Id<NodeIO> {
        self.arena_harness().alloc_node_io(node_io)
    }

    #[track_caller]
    fn file_system_resolver_host(
        &self,
        file_system_resolver_host: Id<Box<dyn FileSystemResolverHost>>,
    ) -> Ref<Box<dyn FileSystemResolverHost>> {
        self.arena_harness()
            .file_system_resolver_host(file_system_resolver_host)
    }

    #[track_caller]
    fn alloc_file_system_resolver_host(
        &self,
        file_system_resolver_host: Box<dyn FileSystemResolverHost>,
    ) -> Id<Box<dyn FileSystemResolverHost>> {
        self.arena_harness()
            .alloc_file_system_resolver_host(file_system_resolver_host)
    }

    #[track_caller]
    fn text_document(&self, text_document: Id<TextDocument>) -> Ref<TextDocument> {
        self.arena_harness().text_document(text_document)
    }

    #[track_caller]
    fn alloc_text_document(&self, text_document: TextDocument) -> Id<TextDocument> {
        self.arena_harness().alloc_text_document(text_document)
    }

    #[track_caller]
    fn compilation_output(
        &self,
        compilation_output: Id<CompilationOutput>,
    ) -> Ref<CompilationOutput> {
        self.arena_harness().compilation_output(compilation_output)
    }

    #[track_caller]
    fn alloc_compilation_output(
        &self,
        compilation_output: CompilationOutput,
    ) -> Id<CompilationOutput> {
        self.arena_harness()
            .alloc_compilation_output(compilation_output)
    }

    #[track_caller]
    fn file_system(&self, file_system: Id<FileSystem>) -> Ref<FileSystem> {
        self.arena_harness().file_system(file_system)
    }

    #[track_caller]
    fn alloc_file_system(&self, file_system: FileSystem) -> Id<FileSystem> {
        self.arena_harness().alloc_file_system(file_system)
    }

    #[track_caller]
    fn sort_options_comparer_string(
        &self,
        sort_options_comparer_string: Id<Box<dyn SortOptionsComparer<String>>>,
    ) -> Ref<Box<dyn SortOptionsComparer<String>>> {
        self.arena_harness()
            .sort_options_comparer_string(sort_options_comparer_string)
    }

    #[track_caller]
    fn alloc_sort_options_comparer_string(
        &self,
        sort_options_comparer_string: Box<dyn SortOptionsComparer<String>>,
    ) -> Id<Box<dyn SortOptionsComparer<String>>> {
        self.arena_harness()
            .alloc_sort_options_comparer_string(sort_options_comparer_string)
    }

    #[track_caller]
    fn metadata_string(&self, metadata_string: Id<Metadata<String>>) -> Ref<Metadata<String>> {
        self.arena_harness().metadata_string(metadata_string)
    }

    #[track_caller]
    fn metadata_string_mut(
        &self,
        metadata_string: Id<Metadata<String>>,
    ) -> RefMut<Metadata<String>> {
        self.arena_harness().metadata_string_mut(metadata_string)
    }

    #[track_caller]
    fn alloc_metadata_string(&self, metadata_string: Metadata<String>) -> Id<Metadata<String>> {
        self.arena_harness().alloc_metadata_string(metadata_string)
    }

    #[track_caller]
    fn metadata_metavalue(
        &self,
        metadata_metavalue: Id<Metadata<MetaValue>>,
    ) -> Ref<Metadata<MetaValue>> {
        self.arena_harness().metadata_metavalue(metadata_metavalue)
    }

    #[track_caller]
    fn metadata_metavalue_mut(
        &self,
        metadata_metavalue: Id<Metadata<MetaValue>>,
    ) -> RefMut<Metadata<MetaValue>> {
        self.arena_harness()
            .metadata_metavalue_mut(metadata_metavalue)
    }

    #[track_caller]
    fn alloc_metadata_metavalue(
        &self,
        metadata_metavalue: Metadata<MetaValue>,
    ) -> Id<Metadata<MetaValue>> {
        self.arena_harness()
            .alloc_metadata_metavalue(metadata_metavalue)
    }

    #[track_caller]
    fn runner_base_sub(
        &self,
        runner_base_sub: Id<Box<dyn RunnerBaseSub>>,
    ) -> Ref<Box<dyn RunnerBaseSub>> {
        self.arena_harness().runner_base_sub(runner_base_sub)
    }

    #[track_caller]
    fn alloc_runner_base_sub(
        &self,
        runner_base_sub: Box<dyn RunnerBaseSub>,
    ) -> Id<Box<dyn RunnerBaseSub>> {
        self.arena_harness().alloc_runner_base_sub(runner_base_sub)
    }

    #[track_caller]
    fn test_file(&self, test_file: Id<TestFile>) -> Ref<TestFile> {
        self.arena_harness().test_file(test_file)
    }

    #[track_caller]
    fn alloc_test_file(&self, test_file: TestFile) -> Id<TestFile> {
        self.arena_harness().alloc_test_file(test_file)
    }

    #[track_caller]
    fn replacer(&self, replacer: Id<Box<dyn Replacer>>) -> Ref<Box<dyn Replacer>> {
        self.arena_harness().replacer(replacer)
    }

    #[track_caller]
    fn alloc_replacer(&self, replacer: Box<dyn Replacer>) -> Id<Box<dyn Replacer>> {
        self.arena_harness().alloc_replacer(replacer)
    }

    #[track_caller]
    fn diagnostic_message_replacer(
        &self,
        diagnostic_message_replacer: Id<DiagnosticMessageReplacer>,
    ) -> Ref<DiagnosticMessageReplacer> {
        self.arena_harness()
            .diagnostic_message_replacer(diagnostic_message_replacer)
    }

    #[track_caller]
    fn alloc_diagnostic_message_replacer(
        &self,
        diagnostic_message_replacer: DiagnosticMessageReplacer,
    ) -> Id<DiagnosticMessageReplacer> {
        self.arena_harness()
            .alloc_diagnostic_message_replacer(diagnostic_message_replacer)
    }

    #[track_caller]
    fn fakes_system(&self, system: Id<System>) -> Ref<System> {
        self.arena_harness().fakes_system(system)
    }

    #[track_caller]
    fn alloc_fakes_system(&self, system: System) -> Id<System> {
        self.arena_harness().alloc_fakes_system(system)
    }

    #[track_caller]
    fn fakes_parse_config_host(
        &self,
        parse_config_host: Id<ParseConfigHost>,
    ) -> Ref<ParseConfigHost> {
        self.arena_harness()
            .fakes_parse_config_host(parse_config_host)
    }

    #[track_caller]
    fn alloc_fakes_parse_config_host(
        &self,
        parse_config_host: ParseConfigHost,
    ) -> Id<ParseConfigHost> {
        self.arena_harness()
            .alloc_fakes_parse_config_host(parse_config_host)
    }

    #[track_caller]
    fn string_comparer(
        &self,
        string_comparer: Id<Box<dyn StringComparer>>,
    ) -> Ref<Box<dyn StringComparer>> {
        self.arena_harness().string_comparer(string_comparer)
    }

    #[track_caller]
    fn alloc_string_comparer(
        &self,
        string_comparer: Box<dyn StringComparer>,
    ) -> Id<Box<dyn StringComparer>> {
        self.arena_harness().alloc_string_comparer(string_comparer)
    }

    #[track_caller]
    fn file_system_resolver(
        &self,
        file_system_resolver: Id<FileSystemResolver>,
    ) -> Ref<FileSystemResolver> {
        self.arena_harness()
            .file_system_resolver(file_system_resolver)
    }

    #[track_caller]
    fn alloc_file_system_resolver(
        &self,
        file_system_resolver: FileSystemResolver,
    ) -> Id<FileSystemResolver> {
        self.arena_harness()
            .alloc_file_system_resolver(file_system_resolver)
    }

    #[track_caller]
    fn inode(&self, inode: Id<Inode>) -> Ref<Inode> {
        self.arena_harness().inode(inode)
    }

    #[track_caller]
    fn alloc_inode(&self, inode: Inode) -> Id<Inode> {
        self.arena_harness().alloc_inode(inode)
    }

    #[track_caller]
    fn links(&self, links: Id<SortedMap<String, Id<Inode>>>) -> Ref<SortedMap<String, Id<Inode>>> {
        self.arena_harness().links(links)
    }

    #[track_caller]
    fn links_mut(
        &self,
        links: Id<SortedMap<String, Id<Inode>>>,
    ) -> RefMut<SortedMap<String, Id<Inode>>> {
        self.arena_harness().links_mut(links)
    }

    #[track_caller]
    fn alloc_links(&self, links: SortedMap<String, Id<Inode>>) -> Id<SortedMap<String, Id<Inode>>> {
        self.arena_harness().alloc_links(links)
    }

    #[track_caller]
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

    #[track_caller]
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
