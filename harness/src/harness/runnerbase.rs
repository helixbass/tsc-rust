use std::cell::Cell;

use regex::Regex;
use typescript_rust::{id_arena::Id, map, normalize_slashes, AllArenas, HasArena};

use crate::{
    get_io, user_specified_root, AllArenasHarness, FileBasedTest, HasArenaHarness, InArenaHarness,
    ListFilesOptions, IO,
};

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

thread_local! {
    static shards_: Cell<usize> = Cell::new(1);
    static shard_id_: Cell<usize> = Cell::new(1);
}

#[allow(dead_code)]
pub fn set_shards(count: usize) {
    shards_.with(|shards| {
        shards.set(count);
    });
}

fn get_shards() -> usize {
    shards_.with(|shards| shards.get())
}

#[allow(dead_code)]
pub fn set_shard_id(id: usize) {
    shard_id_.with(|shard_id| {
        shard_id.set(id);
    });
}

fn get_shard_id() -> usize {
    shard_id_.with(|shard_id| shard_id.get())
}

pub struct RunnerBase {
    sub: Id<Box<dyn RunnerBaseSub>>,
    pub tests: Vec<StringOrFileBasedTest>,
}

impl RunnerBase {
    pub fn new(sub: Id<Box<dyn RunnerBaseSub>>) -> Self {
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
        map(
            get_io(self).list_files(
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
            |file, _| normalize_slashes(file.to_str().unwrap()),
        )
    }

    pub fn kind(&self) -> TestRunnerKind {
        self.sub.ref_(self).kind(self)
    }

    pub fn enumerate_test_files(&self) -> Vec<StringOrFileBasedTest> {
        self.sub.ref_(self).enumerate_test_files(self)
    }

    pub fn get_test_files(&self) -> Vec<StringOrFileBasedTest> {
        let all = self.enumerate_test_files();
        let shards = get_shards();
        if shards == 1 {
            return all;
        }
        let shard_id = get_shard_id();
        all.into_iter()
            .enumerate()
            .filter(|(idx, _val)| idx % shards == shard_id - 1)
            .map(|(_, value)| value)
            .collect()
    }

    pub fn initialize_tests(&self) {
        self.sub.ref_(self).initialize_tests(self)
    }
}

impl HasArena for RunnerBase {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

impl HasArenaHarness for RunnerBase {
    fn arena_harness(&self) -> &AllArenasHarness {
        unimplemented!()
    }
}

#[derive(Clone)]
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
    fn kind(&self, runner_base: &RunnerBase) -> TestRunnerKind;
    fn initialize_tests(&self, runner_base: &RunnerBase);
    fn enumerate_test_files(&self, runner_base: &RunnerBase) -> Vec<StringOrFileBasedTest>;
}
