use typescript_rust::{
    debug_cell::{Ref, RefMut},
    id_arena::Id,
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

pub trait InArenaHarness {
    type Item: ?Sized;

    #[track_caller]
    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, Self::Item>;

    #[track_caller]
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
