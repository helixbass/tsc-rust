use regex::Regex;
use std::rc::Rc;
use typescript_rust::{map, normalize_slashes};

use crate::{user_specified_root, with_io, FileBasedTest, ListFilesOptions};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TestRunnerKind {
    // CompilerTestKind
    Conformance,
    Compiler,
    // FourslashTestKind
    Fourslash,
    FourslashShims,
    FourslashShimsPP,
    FourslashServer,
    Project,
    Rwc,
    Test262,
    User,
    Dt,
    Docker,
}

pub struct RunnerBase {
    sub: Rc<dyn RunnerBaseSub>,
    tests: Vec<StringOrFileBasedTest>,
}

impl RunnerBase {
    pub fn new(sub: Rc<dyn RunnerBaseSub>) -> Self {
        Self { sub, tests: vec![] }
    }

    pub fn add_test(&mut self, file_name: String) {
        self.tests.push(file_name.into());
    }

    pub fn enumerate_files(
        &self,
        folder: &str,
        regex: Option<&Regex>,
        options: Option<EnumerateFilesOptions>,
    ) -> Vec<String> {
        with_io(|IO| {
            map(
                IO.list_files(
                    &format!("{}{}", user_specified_root, folder),
                    regex,
                    Some(ListFilesOptions {
                        recursive: Some(if let Some(options) = &options {
                            options.recursive
                        } else {
                            false
                        }),
                    }),
                ),
                |file, _| normalize_slashes(&file),
            )
        })
    }

    pub fn initialize_tests(&self) {
        self.sub.initialize_tests();
    }
}

pub enum StringOrFileBasedTest {
    String(String),
    FileBasedTest(FileBasedTest),
}

impl From<String> for StringOrFileBasedTest {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<FileBasedTest> for StringOrFileBasedTest {
    fn from(value: FileBasedTest) -> Self {
        Self::FileBasedTest(value)
    }
}

pub struct EnumerateFilesOptions {
    pub recursive: bool,
}

pub trait RunnerBaseSub {
    fn initialize_tests(&self);
}
