#[macro_use]
extern crate lazy_static;

mod harness;
mod mocha;

pub use harness::compiler_impl::compiler;
pub use harness::documents_util::documents;
pub use harness::harness_io::{
    get_file_based_test_configuration_description, get_file_based_test_configurations,
    user_specified_root, with_io, Compiler, FileBasedTest, FileBasedTestConfiguration,
    ListFilesOptions, TestCaseParser, IO,
};
pub use harness::runnerbase::{
    EnumerateFilesOptions, RunnerBase, RunnerBaseSub, StringOrFileBasedTest, TestRunnerKind,
};
pub use harness::vfs_util::vfs;
pub use harness::vpath_util::vpath;
pub use mocha::{after, before, describe, it};

pub mod Utils {
    pub use super::harness::harness_utils::*;
}
