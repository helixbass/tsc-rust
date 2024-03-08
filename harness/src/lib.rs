#![allow(non_upper_case_globals, non_snake_case)]
#[macro_use]
extern crate lazy_static;

mod harness;
pub mod mocha;
mod rust_helpers;

pub use harness::vfs_util::vfs::{Inode, MetaValue, StringOrBuffer}; // these are for enum_unwrapped!() to work
pub use harness::{
    collections_impl::collections,
    compiler_impl::compiler,
    documents_util::documents,
    fakes_hosts::fakes,
    harness_io::{
        get_file_based_test_configuration_description, get_file_based_test_configurations, get_io,
        get_io_id, get_light_mode, user_specified_root, Baseline, Compiler, FileBasedTest,
        FileBasedTestConfiguration, ListFilesOptions, TestCaseParser, IO,
    },
    runnerbase::{
        EnumerateFilesOptions, RunnerBase, RunnerBaseSub, StringOrFileBasedTest, TestRunnerKind,
    },
    source_map_recorder::SourceMapRecorder,
    type_writer::{TypeWriterSymbolResult, TypeWriterTypeResult, TypeWriterWalker},
    vfs_util::vfs,
    virtual_file_system_with_watch::TestFSWithWatch,
    vpath_util::vpath,
};
pub use mocha::{after, before, describe, it, MochaArgs};
pub use rust_helpers::arena::{
    AllArenasHarness, AllArenasHarnessId, HasArenaHarness, IdForFileSystemResolverHost,
    InArenaHarness, OptionInArenaHarness, VecEqArena,
};

pub mod Utils {
    pub use super::harness::{harness_utils::*, util::*};
}
