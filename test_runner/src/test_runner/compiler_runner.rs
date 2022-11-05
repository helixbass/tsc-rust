use harness::{
    describe, get_file_based_test_configurations, with_io, EnumerateFilesOptions, FileBasedTest,
    RunnerBase, RunnerBaseSub, StringOrFileBasedTest, TestCaseParser, TestRunnerKind,
};
use regex::Regex;
use std::path::Path as StdPath;
use std::rc::Rc;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum CompilerTestType {
    Conformance,
    Regression,
    Test262,
}

pub type CompilerFileBasedTest = FileBasedTest;

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
            base_path: format!(
                // "tests/cases/{:?}",
                "../typescript_rust/typescript_src/tests/cases/{:?}",
                test_suite_name
            )
            .to_lowercase(),
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
            .map(|file| CompilerTest::get_configurations(&file).into())
            .collect()
    }

    fn initialize_tests(&self, _runner_base: &RunnerBase) {
        describe(
            &format!("{:?} tests", self.test_suite_name),
            || unimplemented!(),
        );
    }
}

struct CompilerTest {}

impl CompilerTest {
    fn vary_by() -> Vec<&'static str> {
        vec![
            "module",
            "moduleResolution",
            "target",
            "jsx",
            "removeComments",
            "importHelpers",
            "importHelpers",
            "downlevelIteration",
            "isolatedModules",
            "strict",
            "noImplicitAny",
            "strictNullChecks",
            "strictFunctionTypes",
            "strictBindCallApply",
            "strictPropertyInitialization",
            "noImplicitThis",
            "alwaysStrict",
            "allowSyntheticDefaultImports",
            "esModuleInterop",
            "emitDecoratorMetadata",
            "skipDefaultLibCheck",
            "preserveConstEnums",
            "skipLibCheck",
            "exactOptionalPropertyTypes",
            "useUnknownInCatchVariables",
        ]
    }

    pub fn get_configurations<TFile: AsRef<StdPath>>(file: TFile) -> CompilerFileBasedTest {
        let file = file.as_ref();
        let content = with_io(|IO| IO.read_file(file)).unwrap();
        let settings = TestCaseParser::extract_compiler_settings(&content);
        let configurations =
            get_file_based_test_configurations(&settings, &CompilerTest::vary_by());
        CompilerFileBasedTest {
            file: file.to_str().unwrap().to_owned(),
            configurations,
            content: Some(content),
        }
    }
}
