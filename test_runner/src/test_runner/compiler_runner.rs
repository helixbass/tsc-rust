use std::{borrow::Cow, collections::HashMap, io, path::Path as StdPath};

use harness::{
    compiler, describe, get_file_based_test_configuration_description,
    get_file_based_test_configurations, get_io, it, vpath, AllArenasHarness, Baseline, Compiler,
    EnumerateFilesOptions, FileBasedTest, FileBasedTestConfiguration, HasArenaHarness, RunnerBase,
    RunnerBaseSub, StringOrFileBasedTest, TestCaseParser, TestRunnerKind, Utils, IO,
};
use itertools::Itertools;
use jsonxf::Formatter;
use regex::Regex;
use typescript_rust::{
    combine_paths, file_extension_is, get_directory_path, get_normalized_absolute_path,
    id_arena::Id, is_rooted_disk_path, regex, some, to_path, AllArenas, CompilerOptions, Extension,
    HasArena, InArena, OptionInArena,
};

use super::runner::{should_run_category, TestCategory};

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
                "tests/cases/{:?}",
                // "../typescript_rust/typescript_src/tests/cases/{:?}",
                test_suite_name
            )
            .to_lowercase(),
            emit: true,
            test_suite_name,
            options: None,
        }
    }

    pub fn new_runner_base(
        test_type: CompilerTestType,
        arena: &impl HasArenaHarness,
    ) -> RunnerBase {
        RunnerBase::new(
            arena.alloc_runner_base_sub(Box::new(Self::new(test_type))),
            arena,
        )
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
        // let compiler_test: Rc<RefCell<Option<CompilerTest>>> = Rc::new(RefCell::new(None));
        // before({
        //     let compiler_test = compiler_test.clone();
        //     let configuration = configuration.cloned();
        //     let test = test.cloned();
        //     let file_name = file_name.to_owned();
        //     move || {
        //         let mut payload: Option<TestCaseParser::TestCaseContent> = None;
        //         if let Some(test) = test.as_ref().filter(|test| {
        //             matches!(
        //                 test.content.as_ref(),
        //                 Some(test_content) if !test_content.is_empty()
        //             )
        //         }) {
        //             let root_dir = if !test.file.contains("conformance") {
        //                 "tests/cases/compiler/".to_owned()
        //                 // "../typescript_rust/typescript_src/tests/cases/compiler/".to_owned()
        //             } else {
        //                 format!("{}/", get_directory_path(&test.file))
        //             };
        //             payload = Some(TestCaseParser::make_units_from_test(
        //                 test.content.as_ref().unwrap(),
        //                 &test.file,
        //                 Some(&root_dir),
        //                 None,
        //             ));
        //         }
        //         *compiler_test.borrow_mut() =
        //             Some(CompilerTest::new(file_name, payload, configuration));
        //     }
        // });
        if should_run_category(TestCategory::VerifyDiagnostics) {
            it(&format!("Correct errors for {}", file_name), {
                let configuration = configuration.cloned();
                let test = test.cloned();
                let file_name = file_name.to_owned();
                move || {
                    let ref arena = AllArenasHarness::default();
                    let mut payload: Option<TestCaseParser::TestCaseContent> = None;
                    if let Some(test) = test.as_ref().filter(|test| {
                        matches!(
                            test.content.as_ref(),
                            Some(test_content) if !test_content.is_empty()
                        )
                    }) {
                        let root_dir = if !test.file.contains("conformance") {
                            // "tests/cases/compiler/".to_owned()
                            "../typescript_rust/typescript_src/tests/cases/compiler/".to_owned()
                        } else {
                            format!("{}/", get_directory_path(&test.file))
                        }
                        .replace("../typescript_rust/typescript_src/", "");
                        payload = Some(
                            TestCaseParser::make_units_from_test(
                                test.content.as_ref().unwrap(),
                                &test.file,
                                Some(&root_dir),
                                None,
                                arena,
                            )
                            .unwrap(),
                        );
                    }
                    let compiler_test =
                        CompilerTest::new(file_name, payload, configuration, arena).unwrap();
                    compiler_test.verify_diagnostics().unwrap();
                }
            });
        }
        if should_run_category(TestCategory::VerifyModuleResolution) {
            it(
                &format!("Correct module resolution tracing for {}", file_name),
                {
                    let configuration = configuration.cloned();
                    let test = test.cloned();
                    let file_name = file_name.to_owned();
                    move || {
                        let ref arena = AllArenasHarness::default();
                        let mut payload: Option<TestCaseParser::TestCaseContent> = None;
                        if let Some(test) = test.as_ref().filter(|test| {
                            matches!(
                                test.content.as_ref(),
                                Some(test_content) if !test_content.is_empty()
                            )
                        }) {
                            let root_dir = if !test.file.contains("conformance") {
                                // "tests/cases/compiler/".to_owned()
                                "../typescript_rust/typescript_src/tests/cases/compiler/".to_owned()
                            } else {
                                format!("{}/", get_directory_path(&test.file))
                            }
                            .replace("../typescript_rust/typescript_src/", "");
                            payload = Some(
                                TestCaseParser::make_units_from_test(
                                    test.content.as_ref().unwrap(),
                                    &test.file,
                                    Some(&root_dir),
                                    None,
                                    arena,
                                )
                                .unwrap(),
                            );
                        }
                        let compiler_test =
                            CompilerTest::new(file_name, payload, configuration, arena).unwrap();
                        compiler_test.verify_module_resolution();
                    }
                },
            );
        }
        if should_run_category(TestCategory::VerifyJavaScriptOutput) {
            it(&format!("Correct JS output for {}", file_name), {
                let configuration = configuration.cloned();
                let test = test.cloned();
                let file_name = file_name.to_owned();
                let emit = self.emit;
                move || {
                    let ref arena = AllArenasHarness::default();
                    if emit {
                        let mut payload: Option<TestCaseParser::TestCaseContent> = None;
                        if let Some(test) = test.as_ref().filter(|test| {
                            matches!(
                                test.content.as_ref(),
                                Some(test_content) if !test_content.is_empty()
                            )
                        }) {
                            let root_dir = if !test.file.contains("conformance") {
                                // "tests/cases/compiler/".to_owned()
                                "../typescript_rust/typescript_src/tests/cases/compiler/".to_owned()
                            } else {
                                format!("{}/", get_directory_path(&test.file))
                            }
                            .replace("../typescript_rust/typescript_src/", "");
                            payload = Some(
                                TestCaseParser::make_units_from_test(
                                    test.content.as_ref().unwrap(),
                                    &test.file,
                                    Some(&root_dir),
                                    None,
                                    arena,
                                )
                                .unwrap(),
                            );
                        }
                        let compiler_test =
                            CompilerTest::new(file_name, payload, configuration, arena).unwrap();
                        compiler_test.verify_java_script_output().unwrap();
                    }
                }
            });
        }
        if should_run_category(TestCategory::VerifySourceMapRecord) {
            it(&format!("Correct sourcemap content for {}", file_name), {
                let configuration = configuration.cloned();
                let test = test.cloned();
                let file_name = file_name.to_owned();
                let emit = self.emit;
                move || {
                    let ref arena = AllArenasHarness::default();
                    if emit {
                        let mut payload: Option<TestCaseParser::TestCaseContent> = None;
                        if let Some(test) = test.as_ref().filter(|test| {
                            matches!(
                                test.content.as_ref(),
                                Some(test_content) if !test_content.is_empty()
                            )
                        }) {
                            let root_dir = if !test.file.contains("conformance") {
                                // "tests/cases/compiler/".to_owned()
                                "../typescript_rust/typescript_src/tests/cases/compiler/".to_owned()
                            } else {
                                format!("{}/", get_directory_path(&test.file))
                            }
                            .replace("../typescript_rust/typescript_src/", "");
                            payload = Some(
                                TestCaseParser::make_units_from_test(
                                    test.content.as_ref().unwrap(),
                                    &test.file,
                                    Some(&root_dir),
                                    None,
                                    arena,
                                )
                                .unwrap(),
                            );
                        }
                        let compiler_test =
                            CompilerTest::new(file_name, payload, configuration, arena).unwrap();
                        compiler_test.verify_source_map_record().unwrap();
                    }
                }
            });
        }
        // after({
        //     let compiler_test = compiler_test.clone();
        //     move || {
        //         *compiler_test.borrow_mut() = None;
        //     }
        // });
    }

    fn parse_options(&self) {
        if let Some(_options) = self.options.as_ref().filter(|options| !options.is_empty()) {
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
            .map(|file| CompilerTest::get_configurations(&file, self).into())
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
                get_io(self).enumerate_test_files(runner_base)
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
                            CompilerTest::get_configurations(test, self)
                        }
                        StringOrFileBasedTest::FileBasedTest(test) => test.clone(),
                    }),
                );
            }
        });
    }
}

impl HasArena for CompilerBaselineRunner {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

impl HasArenaHarness for CompilerBaselineRunner {
    fn arena_harness(&self) -> &AllArenasHarness {
        unimplemented!()
    }
}

#[allow(dead_code)]
struct CompilerTest {
    file_name: String,
    just_name: String,
    configured_name: String,
    last_unit: TestCaseParser::TestUnitData,
    harness_settings: TestCaseParser::CompilerSettings,
    has_non_dts_files: bool,
    result: compiler::CompilationResult,
    options: Id<CompilerOptions>,
    ts_config_files: Vec<Id<Compiler::TestFile>>,
    to_be_compiled: Vec<Id<Compiler::TestFile>>,
    other_files: Vec<Id<Compiler::TestFile>>,
}

impl CompilerTest {
    pub fn new(
        file_name: String,
        mut test_case_content: Option<TestCaseParser::TestCaseContent>,
        configuration_overrides: Option<TestCaseParser::CompilerSettings>,
        arena: &impl HasArenaHarness,
    ) -> io::Result<Self> {
        let just_name = vpath::basename(&file_name, None, None);
        let mut configured_name = just_name.clone();
        if let Some(configuration_overrides) = configuration_overrides.as_ref() {
            let mut configured_name_ = String::new();
            let mut keys = configuration_overrides.keys().cloned().collect::<Vec<_>>();
            keys.sort();
            for key in &keys {
                if !configured_name_.is_empty() {
                    configured_name_.push_str(",");
                }
                configured_name_.push_str(&format!(
                    "{}={}",
                    key.to_lowercase(),
                    configuration_overrides.get(key).unwrap().to_lowercase(),
                ));
            }
            if !configured_name_.is_empty() {
                let extname = vpath::extname(&just_name, Option::<&[String]>::None, None);
                let basename = vpath::basename(&just_name, Some(&[&extname]), Some(true));
                configured_name = format!("{}({}){}", basename, configured_name_, extname,);
            }
        }

        let root_dir = if !file_name.contains("conformance") {
            // "tests/cases/compiler/".to_owned()
            "../typescript_rust/typescript_src/tests/cases/compiler/".to_owned()
        } else {
            format!("{}/", get_directory_path(&file_name))
        }
        .replace("../typescript_rust/typescript_src/", "");

        if test_case_content.is_none() {
            test_case_content = Some(TestCaseParser::make_units_from_test(
                &IO::read_file(&*get_io(arena), file_name.as_ref()).unwrap(),
                &file_name,
                Some(&root_dir),
                None,
                arena,
            )?);
        }
        let mut test_case_content = test_case_content.unwrap();

        if let Some(configuration_overrides) = configuration_overrides {
            test_case_content.settings.extend(configuration_overrides);
        }

        let units = &test_case_content.test_unit_data;
        let mut harness_settings = test_case_content.settings.clone();
        let mut ts_config_options: Option<Id<CompilerOptions>> = None;
        let mut ts_config_files: Vec<Id<Compiler::TestFile>> = vec![];
        if let Some(test_case_content_ts_config) = test_case_content.ts_config.as_ref() {
            assert!(
                test_case_content_ts_config.file_names.is_empty(),
                "list of files in tsconfig is not currently supported"
            );
            assert!(
                !matches!(
                    test_case_content_ts_config.raw.as_ref(),
                    Some(raw) if raw.get("exclude").is_some()
                ),
                "exclude in tsconfig is not currently supported"
            );

            ts_config_options = Some(test_case_content_ts_config.options.clone());
            ts_config_files.push(
                arena.alloc_test_file(Self::create_harness_test_file(
                    test_case_content.ts_config_file_unit_data.as_ref().unwrap(),
                    &root_dir,
                    Some(&combine_paths(
                        &root_dir,
                        &[ts_config_options
                            .unwrap()
                            .ref_(arena)
                            .config_file_path
                            .as_deref()],
                    )),
                    arena,
                )),
            );
        } else {
            let base_url = harness_settings.get("baseUrl").cloned();
            if let Some(base_url) = base_url.filter(|base_url| !is_rooted_disk_path(base_url)) {
                harness_settings.insert(
                    "baseUrl".to_owned(),
                    get_normalized_absolute_path(&base_url, Some(&root_dir)),
                );
            }
        }

        let last_unit = units[units.len() - 1].clone();
        let has_non_dts_files = units
            .iter()
            .any(|unit| !file_extension_is(&unit.name, Extension::Dts.to_str()));
        let mut to_be_compiled: Vec<Id<Compiler::TestFile>> = vec![];
        let mut other_files: Vec<Id<Compiler::TestFile>> = vec![];

        if test_case_content
            .settings
            .get("noImplicitReferences")
            .filter(|value| !value.is_empty())
            .is_some()
            || {
                lazy_static! {
                    static ref require_regex: Regex = Regex::new(r"require\(").unwrap();
                }
                require_regex.is_match(last_unit.content.as_ref().unwrap())
            }
            || {
                lazy_static! {
                    static ref reference_path_regex: Regex =
                        Regex::new(r"reference\spath").unwrap();
                }
                reference_path_regex.is_match(last_unit.content.as_ref().unwrap())
            }
        {
            to_be_compiled.push(arena.alloc_test_file(Self::create_harness_test_file(
                &last_unit, &root_dir, None, arena,
            )));
            for unit in units {
                if unit.name != last_unit.name {
                    other_files.push(arena.alloc_test_file(Self::create_harness_test_file(
                        unit, &root_dir, None, arena,
                    )));
                }
            }
        } else {
            to_be_compiled = units
                .into_iter()
                .map(|unit| {
                    arena.alloc_test_file(Self::create_harness_test_file(
                        unit, &root_dir, None, arena,
                    ))
                })
                .collect();
        }

        if let Some(ts_config_options_present) = ts_config_options
            .clone()
            .filter(|ts_config_options| ts_config_options.ref_(arena).config_file_path.is_some())
        {
            let mut options = ts_config_options_present.ref_(arena).clone();
            options.config_file_path = Some(combine_paths(
                &root_dir,
                &[ts_config_options_present
                    .ref_(arena)
                    .config_file_path
                    .as_deref()],
            ));
            options
                .config_file
                .unwrap()
                .ref_(arena)
                .as_source_file()
                .set_file_name(options.config_file_path.clone().unwrap());
            ts_config_options = Some(arena.alloc_compiler_options(options));
        }

        let result = Compiler::compile_files(
            &to_be_compiled,
            &other_files,
            Some(&harness_settings),
            ts_config_options.refed(arena).as_deref(),
            harness_settings
                .get("currentDirectory")
                .map(|value| &**value),
            test_case_content.symlinks.as_ref(),
            arena,
        )?;

        let options = result.options.clone();

        Ok(Self {
            file_name,
            just_name,
            configured_name,
            last_unit,
            harness_settings,
            has_non_dts_files,
            result,
            options,
            ts_config_files,
            to_be_compiled,
            other_files,
        })
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

    pub fn get_configurations(
        file: impl AsRef<StdPath>,
        arena: &impl HasArenaHarness,
    ) -> CompilerFileBasedTest {
        let file = file.as_ref();
        let content = IO::read_file(&*get_io(arena), file).unwrap();
        let settings = TestCaseParser::extract_compiler_settings(&content);
        let configurations =
            get_file_based_test_configurations(&settings, &CompilerTest::vary_by(), arena);
        CompilerFileBasedTest {
            file: file.to_str().unwrap().to_owned(),
            configurations,
            content: Some(content),
        }
    }

    pub fn verify_diagnostics(&self) -> io::Result<()> {
        Compiler::do_error_baseline(
            &self.configured_name,
            &[
                &*self.ts_config_files,
                &*self.to_be_compiled,
                &*self.other_files,
            ]
            .concat(),
            &self.result.diagnostics,
            Some(self.options.ref_(self).pretty == Some(true)),
            self,
        )?;

        Ok(())
    }

    pub fn verify_module_resolution(&self) {
        if self.options.ref_(self).trace_resolution == Some(true) {
            Baseline::run_baseline(
                &regex!(".tsx?$").replace(&self.configured_name, ".trace.json"),
                Some(&{
                    let mut fmt = Formatter::pretty_printer();
                    fmt.indent = "    ".to_owned();
                    fmt.format(
                        &serde_json::to_string(
                            &self
                                .result
                                .traces()
                                .iter()
                                .map(|trace| {
                                    Utils::sanitize_trace_resolution_log_entry(trace, self)
                                })
                                .collect_vec(),
                        )
                        .unwrap(),
                    )
                    .unwrap()
                }),
                None,
                self,
            );
        }
    }

    pub fn verify_source_map_record(&self) -> io::Result<()> {
        if self.options.ref_(self).source_map == Some(true)
            || self.options.ref_(self).inline_source_map == Some(true)
            || self.options.ref_(self).declaration_map == Some(true)
        {
            let record = Utils::maybe_remove_test_path_prefixes(
                self.result.get_source_map_record().as_deref(),
                None,
            )
            .map(Cow::into_owned);
            let baseline = record.and_then(|record| {
                (!(self.options.ref_(self).no_emit_on_error == Some(true)
                    && !self.result.diagnostics.is_empty()))
                .then_some(record)
            });
            Baseline::run_baseline(
                &regex!(r#"\.tsx?"#).replace(&self.configured_name, ".sourcemap.txt"),
                baseline.as_deref(),
                None,
                self,
            );
        }

        Ok(())
    }

    pub fn verify_java_script_output(&self) -> io::Result<()> {
        if self.has_non_dts_files {
            Compiler::do_js_emit_baseline(
                &self.configured_name,
                &self
                    .file_name
                    .replace("../typescript_rust/typescript_src/", ""),
                self.options.clone(),
                &self.result,
                &self.ts_config_files,
                &self.to_be_compiled,
                &self.other_files,
                &self.harness_settings,
                self,
            )?;
        }

        Ok(())
    }

    pub fn make_unit_name(name: &str, root: &str, arena: &impl HasArenaHarness) -> String {
        let path = to_path(name, Some(root), |file_name| file_name.to_owned());
        let path_start = to_path(
            &get_io(arena).get_current_directory().unwrap(),
            Some(""),
            |file_name| file_name.to_owned(),
        );
        if !path_start.is_empty() {
            path.replace(&*path_start, "/")
        } else {
            path.into_string()
        }
    }

    pub fn create_harness_test_file(
        last_unit: &TestCaseParser::TestUnitData,
        root_dir: &str,
        unit_name: Option<&str>,
        arena: &impl HasArenaHarness,
    ) -> Compiler::TestFile {
        Compiler::TestFile {
            unit_name: unit_name.map_or_else(
                || Self::make_unit_name(&last_unit.name, root_dir, arena),
                ToOwned::to_owned,
            ),
            content: last_unit.content.clone().unwrap(),
            file_options: Some(last_unit.file_options.clone()),
        }
    }
}

impl HasArena for CompilerTest {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

impl HasArenaHarness for CompilerTest {
    fn arena_harness(&self) -> &AllArenasHarness {
        unimplemented!()
    }
}
