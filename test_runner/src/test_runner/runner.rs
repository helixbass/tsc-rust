use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
};

use clap::{Parser, ValueEnum};
use harness::{mocha, vpath, HasArenaHarness, MochaArgs, RunnerBase, StringOrFileBasedTest};
use once_cell::unsync::OnceCell;
use regex::Regex;
use typescript_rust::_d;

use crate::{CompilerBaselineRunner, CompilerTestType};

thread_local! {
    static runners_: RefCell<Vec<RunnerBase>> = Default::default();
}

fn with_runners<TReturn>(mut callback: impl FnMut(&[RunnerBase]) -> TReturn) -> TReturn {
    runners_.with(|runners| callback(&runners.borrow()))
}

fn with_runners_mut<TReturn>(mut callback: impl FnMut(&mut Vec<RunnerBase>) -> TReturn) -> TReturn {
    runners_.with(|runners| callback(&mut runners.borrow_mut()))
}

thread_local! {
    static iterations_: Cell<usize> = Cell::new(1);
}

fn get_iterations() -> usize {
    iterations_.with(|iterations| iterations.get())
}

fn run_tests(runners: &[RunnerBase]) {
    let mut i = get_iterations();
    while i > 0 {
        let mut seen: HashMap<String, String> = HashMap::new();
        let mut dupes: Vec<(String, String)> = vec![];
        for runner in runners {
            if true
            /*runner instanceof CompilerBaselineRunner || runner instanceof FourSlashRunner*/
            {
                for sf in runner.enumerate_test_files() {
                    let full = match &sf {
                        StringOrFileBasedTest::String(sf) => sf.clone(),
                        StringOrFileBasedTest::FileBasedTest(sf) => sf.file.clone(),
                    };
                    let base = vpath::basename(&full, None, None).to_lowercase();
                    if seen.contains_key(&base) && {
                        lazy_static! {
                            static ref fourslash_regex: Regex =
                                Regex::new(r"fourslash/(shim|server)").unwrap();
                        }
                        !fourslash_regex.is_match(&full)
                    } {
                        dupes.push((seen.get(&base).unwrap().clone(), full));
                    } else {
                        seen.insert(base, full);
                    }
                }
            }
            runner.initialize_tests();
        }
        if !dupes.is_empty() {
            panic!(
                "{} Tests with duplicate baseline names: {:?}",
                dupes.len(),
                dupes,
            );
        }
        i -= 1;
    }
}

fn handle_test_config(arena: &impl HasArenaHarness) -> bool {
    if false {
        unimplemented!()
    }

    with_runners_mut(|runners| {
        if runners.is_empty() {
            // unimplemented!()
            runners.push(CompilerBaselineRunner::new_runner_base(
                CompilerTestType::Conformance,
                arena,
            ));
            runners.push(CompilerBaselineRunner::new_runner_base(
                CompilerTestType::Regression,
                arena,
            ));
            // unimplemented!()
        }
    });
    if false {
        unimplemented!()
    }
    false
}

fn begin_tests() {
    // unimplemented!()

    with_runners(|runners| {
        run_tests(runners);
    });
    // unimplemented!()
}

thread_local! {
    static is_worker: Cell<bool> = Cell::new(false);
}

fn get_is_worker() -> bool {
    is_worker.with(|is_worker_| is_worker_.get())
}

fn set_is_worker(value: bool) {
    is_worker.with(|is_worker_| {
        is_worker_.set(value);
    })
}

fn start_test_environment(arena: &impl HasArenaHarness) {
    set_is_worker(handle_test_config(arena));
    if get_is_worker() {
        unimplemented!()
    } else if false {
        unimplemented!()
    }
    begin_tests()
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
pub enum TestCategory {
    VerifyDiagnostics,
    VerifyModuleResolution,
    VerifyJavaScriptOutput,
    VerifySourceMapRecord,
}

thread_local! {
    static TEST_CATEGORY: OnceCell<Option<TestCategory>> = _d();
}
fn initialize_test_category(category: Option<TestCategory>) {
    TEST_CATEGORY.with(|test_category| {
        test_category.set(category).unwrap();
    })
}
pub(crate) fn should_run_category(category: TestCategory) -> bool {
    TEST_CATEGORY.with(|test_category| {
        test_category
            .get()
            .unwrap()
            .filter(|test_category| *test_category != category)
            .is_none()
    })
}

#[derive(Parser)]
pub struct Args {
    #[clap(flatten)]
    pub mocha_args: MochaArgs,

    #[arg(long, value_enum)]
    pub category: Option<TestCategory>,
}

pub fn run(args: &Args, arena: &impl HasArenaHarness) {
    mocha::register_config(&args.mocha_args);
    initialize_test_category(args.category);
    start_test_environment(arena);
    mocha::print_results();
}
