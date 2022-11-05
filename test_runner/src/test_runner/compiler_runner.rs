use harness::{
    after, before, describe, get_file_based_test_configuration_description,
    get_file_based_test_configurations, it, vpath, with_io, EnumerateFilesOptions, FileBasedTest,
    FileBasedTestConfiguration, RunnerBase, RunnerBaseSub, StringOrFileBasedTest, TestCaseParser,
    TestRunnerKind,
};
use regex::Regex;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path as StdPath;
use std::rc::Rc;
use typescript_rust::{get_directory_path, some};

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
    pub options: Option<String>,
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
            options: None,
        }
    }

    pub fn new_runner_base(test_type: CompilerTestType) -> RunnerBase {
        RunnerBase::new(Rc::new(Self::new(test_type)))
    }

    fn check_test_code_output(&self, file_name: &str, test: Option<&CompilerFileBasedTest>) {
        if let Some(test) = test.filter(|test| {
            some(
                test.configurations.as_deref(),
                Option::<fn(&HashMap<String, String>) -> bool>::None,
            )
        }) {
            for configuration in test.configurations.as_ref().unwrap() {
                describe(
                    &format!(
                        "{:?} tests for {}{}",
                        self.test_suite_name,
                        file_name,
                        /*configuration ? */
                        get_file_based_test_configuration_description(configuration) /*: ``*/
                    ),
                    || {
                        self.run_suite(file_name, Some(test), Some(configuration));
                    },
                )
            }
        } else {
            describe(
                &format!("{:?} tests for {}", self.test_suite_name, file_name,),
                || {
                    self.run_suite(file_name, test, None);
                },
            )
        }
    }

    fn run_suite(
        &self,
        file_name: &str,
        test: Option<&CompilerFileBasedTest>,
        configuration: Option<&FileBasedTestConfiguration>,
    ) {
        let compiler_test: RefCell<Option<CompilerTest>> = RefCell::new(None);
        before(|| {
            let mut payload: Option<TestCaseParser::TestCaseContent> = None;
            if let Some(test) = test.filter(|test| {
                matches!(
                    test.content.as_ref(),
                    Some(test_content) if !test_content.is_empty()
                )
            }) {
                let root_dir = if !test.file.contains("conformance") {
                    // "tests/cases/compiler/"
                    "../typescript_rust/typescript_src/tests/cases/compiler/".to_owned()
                } else {
                    format!("{}/", get_directory_path(&test.file))
                };
                payload = Some(TestCaseParser::make_units_from_test(
                    test.content.as_ref().unwrap(),
                    &test.file,
                    Some(&root_dir),
                    None,
                ));
            }
            *compiler_test.borrow_mut() = Some(CompilerTest::new(
                file_name.to_owned(),
                payload,
                configuration.cloned(),
            ));
        });
        it(&format!("Correct errors for {}", file_name), || {
            compiler_test
                .borrow()
                .as_ref()
                .unwrap()
                .verify_diagnostics();
        });
        after(|| {
            *compiler_test.borrow_mut() = None;
        });
    }

    fn parse_options(&self) {
        if let Some(options) = self.options.as_ref().filter(|options| !options.is_empty()) {
            unimplemented!()
        }
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

    fn initialize_tests(&self, runner_base: &RunnerBase) {
        describe(&format!("{:?} tests", self.test_suite_name), || {
            describe("Setup compiler for compiler baselines", || {
                self.parse_options();
            });

            let files = if !runner_base.tests.is_empty() {
                runner_base.tests.clone()
            } else {
                with_io(|IO| IO.enumerate_test_files(runner_base))
            };
            for test in files {
                let file = match &test {
                    StringOrFileBasedTest::String(test) => test.clone(),
                    StringOrFileBasedTest::FileBasedTest(test) => test.file.clone(),
                };
                self.check_test_code_output(
                    &vpath::normalize_separators(&file),
                    Some(&match &test {
                        StringOrFileBasedTest::String(test) => {
                            CompilerTest::get_configurations(test)
                        }
                        StringOrFileBasedTest::FileBasedTest(test) => test.clone(),
                    }),
                );
            }
        });
    }
}

struct CompilerTest {}

impl CompilerTest {
    pub fn new(
        file_name: String,
        test_case_content: Option<TestCaseParser::TestCaseContent>,
        configuration_overrides: Option<TestCaseParser::CompilerSettings>,
    ) -> Self {
        unimplemented!()
    }

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

    pub fn verify_diagnostics(&self) {
        unimplemented!()
    }
}
