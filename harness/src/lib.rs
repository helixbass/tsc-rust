#![allow(non_upper_case_globals, non_snake_case)]
#[macro_use]
extern crate lazy_static;

mod harness;
pub mod mocha;

pub use harness::collections_impl::collections;
pub use harness::compiler_impl::compiler;
pub use harness::documents_util::documents;
pub use harness::fakes_hosts::fakes;
pub use harness::harness_io::{
    get_file_based_test_configuration_description, get_file_based_test_configurations, get_io,
    get_light_mode, user_specified_root, with_io, Baseline, Compiler, FileBasedTest,
    FileBasedTestConfiguration, ListFilesOptions, TestCaseParser, IO,
};
pub use harness::runnerbase::{
    EnumerateFilesOptions, RunnerBase, RunnerBaseSub, StringOrFileBasedTest, TestRunnerKind,
};
pub use harness::vfs_util::vfs;
pub use harness::vfs_util::vfs::{Inode, MetaValue, StringOrBuffer}; // these are for enum_unwrapped!() to work
pub use harness::virtual_file_system_with_watch::TestFSWithWatch;
pub use harness::vpath_util::vpath;
pub use mocha::{after, before, describe, it, MochaArgs};

pub mod Utils {
    pub use super::harness::harness_utils::*;
    pub use super::harness::util::*;
}
