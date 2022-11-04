use harness::{
    describe, EnumerateFilesOptions, RunnerBase, RunnerBaseSub, StringOrFileBasedTest,
    TestRunnerKind,
};
use regex::Regex;
use std::rc::Rc;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum CompilerTestType {
    Conformance,
    Regression,
    Test262,
}

pub struct CompilerBaselineRunner {
    base_path: String,
    test_suite_name: TestRunnerKind,
    emit: bool,
}

impl CompilerBaselineRunner {
    pub fn new(test_type: CompilerTestType) -> Self {
        let test_suite_name = match test_type {
            CompilerTestType::Conformance => TestRunnerKind::Conformance,
            CompilerTestType::Regression => TestRunnerKind::Compiler,
            CompilerTestType::Test262 => TestRunnerKind::Test262,
        };
        Self {
            base_path: format!("tests/cases/{:?}", test_suite_name).to_lowercase(),
            emit: true,
            test_suite_name,
        }
    }

    pub fn new_runner_base(test_type: CompilerTestType) -> RunnerBase {
        RunnerBase::new(Rc::new(Self::new(test_type)))
    }
}

impl RunnerBaseSub for CompilerBaselineRunner {
    fn kind(&self, _runner_base: &RunnerBase) -> TestRunnerKind {
        self.test_suite_name
    }

    fn enumerate_test_files(&self, runner_base: &RunnerBase) -> Vec<StringOrFileBasedTest> {
        runner_base
            .enumerate_files(
                &self.base_path,
                Some(&Regex::new(r"\.tsx?").unwrap()),
                Some(EnumerateFilesOptions { recursive: true }),
            )
            .into_iter()
            .map(|file| unimplemented!())
            .collect()
    }

    fn initialize_tests(&self, _runner_base: &RunnerBase) {
        describe(
            &format!("{:?} tests", self.test_suite_name),
            || unimplemented!(),
        );
    }
}
