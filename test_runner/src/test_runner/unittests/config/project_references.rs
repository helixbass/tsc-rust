#![cfg(test)]

use std::{collections::HashMap, rc::Rc};

use derive_builder::Builder;
use harness::{fakes, vfs, AllArenasHarness, HasArenaHarness, InArenaHarness, TestFSWithWatch};
use itertools::Itertools;
use serde::Serialize;
use speculoos::prelude::*;
use typescript_rust::{
    combine_paths, create_program, flatten_diagnostic_message_text, get_directory_path,
    id_arena::Id, parse_config_host_from_compiler_host_like, parse_json_config_file_content,
    read_config_file, CompilerHostLikeRcDynCompilerHost, CompilerOptions, CompilerOptionsBuilder,
    CreateProgramOptionsBuilder, Diagnostic, DiagnosticMessage,
    DiagnosticRelatedInformationInterface, Diagnostics, HasArena, InArena, MapOrDefault,
    ModuleResolutionHost, NonEmpty, OptionInArena, Owned, Program, ProjectReference,
    ProjectReferenceBuilder, ReadConfigFileReturn, UnwrapOrEmpty,
};

#[derive(Builder, Clone, Default)]
#[builder(default, setter(strip_option, into))]
struct TestProjectSpecification {
    pub config_file_name: Option<String>,
    pub references: Option<Vec<StringOrProjectReference>>,
    pub files: HashMap<String, String>,
    pub output_files: Option<HashMap<String, String>>,
    pub config: Option<serde_json::Map<String, serde_json::Value>>,
    pub options: Option<Id<CompilerOptions>>,
}

#[derive(Clone)]
enum StringOrProjectReference {
    String(String),
    ProjectReference(Rc<ProjectReference>),
}

impl From<String> for StringOrProjectReference {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<Rc<ProjectReference>> for StringOrProjectReference {
    fn from(value: Rc<ProjectReference>) -> Self {
        Self::ProjectReference(value)
    }
}

type TestSpecification = HashMap<String, TestProjectSpecification>;

fn assert_has_error(
    message: &str,
    errors: &[Id<Diagnostic>],
    diag: &DiagnosticMessage,
    arena: &impl HasArena,
) {
    if !errors
        .into_iter()
        .any(|e| e.ref_(arena).code() == diag.code)
    {
        let error_string = errors
            .into_iter()
            .map(|e| {
                format!(
                    "    {}: {:?}",
                    if let Some(e_file) = e.ref_(arena).maybe_file() {
                        e_file.ref_(arena).as_source_file().file_name().clone()
                    } else {
                        "[global]".to_owned()
                    },
                    e.ref_(arena).message_text()
                )
            })
            .collect_vec()
            .join("\r\n");
        asserting(&format!(
            "{message}: Did not find any diagnostic for {} in:\r\n{error_string}",
            diag.message
        ))
        .that(&false)
        .is_true();
    }
}

fn assert_no_errors(message: &str, errors: &[Id<Diagnostic>], arena: &impl HasArena) {
    if
    /*errors &&*/
    !errors.is_empty() {
        asserting(&format!(
            "{message}: Expected no errors, but found:\r\n{}",
            errors
                .into_iter()
                .map(|e| format!("    {:?}", e.ref_(arena).message_text()))
                .collect_vec()
                .join("\r\n")
        ))
        .that(&false)
        .is_true();
    }
}

fn combine_all_paths(paths: &[String]) -> String {
    let mut result = paths[0].clone();
    for i in 1..paths.len() {
        result = combine_paths(&result, &[Some(&paths[i])]);
    }
    result
}

const empty_module: &'static str = "export { };";

fn module_importing(names: &[String]) -> String {
    names
        .into_iter()
        .enumerate()
        .map(|(i, n)| format!("import * as mod_{i} from {n}"))
        .collect_vec()
        .join("\r\n")
}

fn test_project_references(
    spec: &TestSpecification,
    entry_point_config_file_name: &str,
    mut check_result: impl FnMut(&Program, &fakes::CompilerHost),
    arena: &impl HasArenaHarness,
) {
    let mut files: HashMap<String, String> = Default::default();
    for (key, sp) in spec {
        let config_file_name = combine_all_paths(&[
            "/".to_owned(),
            key.clone(),
            sp.config_file_name
                .clone()
                .non_empty()
                .unwrap_or_else(|| "tsconfig.json".to_owned()),
        ]);
        let mut compiler_options = sp
            .options
            .refed(arena)
            .as_deref()
            .map_or_default(Clone::clone);
        if compiler_options.composite.is_none() {
            compiler_options.composite = Some(true);
        }
        if compiler_options.out_dir.is_none() {
            compiler_options.out_dir = Some("bin".to_owned());
        }
        let options = TestProjectReferencesOptions::new(
            arena.alloc_compiler_options(compiler_options),
            sp.references
                .as_ref()
                .map(IntoIterator::into_iter)
                .unwrap_or_empty()
                .map(|r| match r {
                    StringOrProjectReference::String(r) => {
                        Rc::new(ProjectReferenceBuilder::default().path(r).build().unwrap())
                    }
                    StringOrProjectReference::ProjectReference(r) => r.clone(),
                })
                .collect(),
            sp.config.clone(),
        );
        let config_content = serde_json::to_string(&options).unwrap();
        let ref out_dir = options
            .compiler_options
            .ref_(arena)
            .out_dir
            .clone()
            .unwrap();
        files.insert(config_file_name, config_content);
        for (source_file, value) in &sp.files {
            files.insert(source_file.clone(), value.clone());
        }
        if let Some(sp_output_files) = sp.output_files.as_ref() {
            for (out_file, value) in sp_output_files {
                files.insert(
                    combine_all_paths(&[
                        "/".to_owned(),
                        key.clone(),
                        out_dir.clone(),
                        out_file.clone(),
                    ]),
                    value.clone(),
                );
            }
        }
    }

    let vfsys = arena.alloc_file_system(
        vfs::FileSystem::new(
            false,
            Some(
                vfs::FileSystemOptionsBuilder::default()
                    .files(HashMap::from_iter([(
                        "/lib.d.ts".to_owned(),
                        Some(TestFSWithWatch::lib_file().content.into()),
                    )]))
                    .build()
                    .unwrap(),
            ),
            arena,
        )
        .unwrap(),
    );
    for (k, v) in files {
        vfsys
            .ref_(arena)
            .mkdirp_sync(&get_directory_path(&k))
            .unwrap();
        vfsys.ref_(arena).write_file_sync(&k, v, None).unwrap();
    }
    let host = fakes::CompilerHost::new(
        arena.alloc_fakes_system(fakes::System::new(vfsys, None, arena).unwrap()),
        None,
        None,
        arena,
    )
    .unwrap();
    let ReadConfigFileReturn { config, error } = read_config_file(
        entry_point_config_file_name,
        |name: &str| host.ref_(arena).read_file(name),
        arena,
    )
    .unwrap();

    asserting(&flatten_diagnostic_message_text(
        error.map(|error| error.ref_(arena).message_text()),
        "\n",
        None,
    ))
    .that(&(config.is_some() && error.is_none()))
    .is_true();
    let mut file = parse_json_config_file_content(
        config,
        &parse_config_host_from_compiler_host_like(
            arena.alloc_compiler_host_like(Box::new(CompilerHostLikeRcDynCompilerHost::from(host))),
            None,
            arena,
        ),
        &get_directory_path(entry_point_config_file_name),
        Some(arena.alloc_compiler_options(Default::default())),
        Some(entry_point_config_file_name),
        None,
        None,
        None,
        None,
        arena,
    )
    .unwrap();
    file.options = arena.alloc_compiler_options({
        let mut file_options = file.options.ref_(arena).clone();
        file_options.config_file_path = Some(entry_point_config_file_name.to_owned());
        file_options
    });
    let prog = create_program(
        CreateProgramOptionsBuilder::default()
            .root_names(file.file_names.clone())
            .options(file.options.clone())
            .host(host)
            .project_references(file.project_references.clone())
            .build()
            .unwrap(),
        arena,
    )
    .unwrap();
    check_result(
        &prog.ref_(arena),
        &host
            .ref_(arena)
            .as_dyn_any()
            .downcast_ref::<fakes::CompilerHost>()
            .unwrap(),
    );
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct TestProjectReferencesOptions {
    compiler_options: Id<CompilerOptions>,
    references: Vec<Rc<ProjectReference>>,
    #[serde(flatten)]
    sp_config: Option<serde_json::Map<String, serde_json::Value>>,
}

impl TestProjectReferencesOptions {
    fn new(
        compiler_options: Id<CompilerOptions>,
        references: Vec<Rc<ProjectReference>>,
        sp_config: Option<serde_json::Map<String, serde_json::Value>>,
    ) -> Self {
        Self {
            compiler_options,
            references,
            sp_config,
        }
    }
}

mod meta_check {
    use super::*;

    #[test]
    fn test_default_setup_was_created_correctly() {
        let ref arena = AllArenasHarness::default();
        let spec = TestSpecification::from_iter([
            (
                "/primary".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [("/primary/a.ts", empty_module)].owned(),
                    ))
                    .references(vec![])
                    .build()
                    .unwrap(),
            ),
            (
                "/reference".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [(
                            "/secondary/b.ts",
                            &*module_importing(&["../primary/a"].owned()),
                        )]
                        .owned(),
                    ))
                    .references(vec!["../primary".to_owned().into()])
                    .build()
                    .unwrap(),
            ),
        ]);
        test_project_references(
            &spec,
            "/primary/tsconfig.json",
            |prog: &Program, _| {
                // assert.isTrue(!!prog, "Program should exist");
                assert_no_errors(
                    "Sanity check should not produce errors",
                    &prog.get_options_diagnostics(None),
                    arena,
                );
            },
            arena,
        );
    }
}

mod constraint_checking_for_settings {
    use serde_json::json;
    use typescript_rust::ScriptReferenceHost;

    use super::*;

    #[test]
    fn test_errors_when_declaration_false() {
        let ref arena = AllArenasHarness::default();
        let spec = TestSpecification::from_iter([(
            "/primary".to_owned(),
            TestProjectSpecificationBuilder::default()
                .files(HashMap::from_iter(
                    [("/primary/a.ts", empty_module)].owned(),
                ))
                .references(vec![])
                .options(
                    arena.alloc_compiler_options(
                        CompilerOptionsBuilder::default()
                            .declaration(false)
                            .build()
                            .unwrap(),
                    ),
                )
                .build()
                .unwrap(),
        )]);

        test_project_references(
            &spec,
            "/primary/tsconfig.json",
            |program: &Program, _| {
                let errs = program.get_options_diagnostics(None);
                assert_has_error(
                    "Reports an error about the wrong decl setting",
                    &errs,
                    &Diagnostics::Composite_projects_may_not_disable_declaration_emit,
                    arena,
                );
            },
            arena,
        );
    }

    #[test]
    fn test_errors_when_the_referenced_project_doesnt_have_composite_true() {
        let ref arena = AllArenasHarness::default();
        let spec = TestSpecification::from_iter([
            (
                "/primary".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [("/primary/a.ts", empty_module)].owned(),
                    ))
                    .references(vec![])
                    .options(
                        arena.alloc_compiler_options(
                            CompilerOptionsBuilder::default()
                                .composite(false)
                                .build()
                                .unwrap(),
                        ),
                    )
                    .build()
                    .unwrap(),
            ),
            (
                "/reference".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [(
                            "/secondary/b.ts",
                            &*module_importing(&["../primary/a"].owned()),
                        )]
                        .owned(),
                    ))
                    .references(["../primary".to_owned().into()])
                    .config(
                        json!({
                            "files": ["b.ts"],
                        })
                        .as_object()
                        .unwrap()
                        .clone(),
                    )
                    .build()
                    .unwrap(),
            ),
        ]);

        test_project_references(
            &spec,
            "/reference/tsconfig.json",
            |program: &Program, _| {
                let errs = program.get_options_diagnostics(None);
                assert_has_error(
                    "Reports an error about 'composite' not being set",
                    &errs,
                    &Diagnostics::Referenced_project_0_must_have_setting_composite_Colon_true,
                    arena,
                );
            },
            arena,
        );
    }

    #[test]
    fn test_does_not_error_when_the_referenced_project_doesnt_have_composite_true_if_its_a_container_project(
    ) {
        let ref arena = AllArenasHarness::default();
        let spec = TestSpecification::from_iter([
            (
                "/primary".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [("/primary/a.ts", empty_module)].owned(),
                    ))
                    .references(vec![])
                    .options(
                        arena.alloc_compiler_options(
                            CompilerOptionsBuilder::default()
                                .composite(false)
                                .build()
                                .unwrap(),
                        ),
                    )
                    .build()
                    .unwrap(),
            ),
            (
                "/reference".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [(
                            "/secondary/b.ts",
                            &*module_importing(&["../primary/a"].owned()),
                        )]
                        .owned(),
                    ))
                    .references(["../primary".to_owned().into()])
                    .build()
                    .unwrap(),
            ),
        ]);

        test_project_references(
            &spec,
            "/reference/tsconfig.json",
            |program: &Program, _| {
                let errs = program.get_options_diagnostics(None);
                assert_no_errors(
                    "Reports an error about 'composite' not being set",
                    &errs,
                    arena,
                );
            },
            arena,
        );
    }

    #[test]
    fn test_errors_when_the_file_list_is_not_exhaustive() {
        let ref arena = AllArenasHarness::default();
        let spec = TestSpecification::from_iter([(
            "/primary".to_owned(),
            TestProjectSpecificationBuilder::default()
                .files(HashMap::from_iter(
                    [
                        ("/primary/a.ts", "import * as b from './b'"),
                        ("/primary/b.ts", "export {}"),
                    ]
                    .owned(),
                ))
                .config(
                    json!({
                        "files": ["a.ts"],
                    })
                    .as_object()
                    .unwrap()
                    .clone(),
                )
                .build()
                .unwrap(),
        )]);

        test_project_references(
            &spec,
            "/primary/tsconfig.json",
            |program: &Program, _| {
                let errs = program
                    .get_semantic_diagnostics(program.get_source_file("/primary/a.ts"), None)
                    .unwrap();
                assert_has_error(
                "Reports an error about b.ts not being in the list",
                &errs,
                &Diagnostics::File_0_is_not_listed_within_the_file_list_of_project_1_Projects_must_list_all_files_or_use_an_include_pattern,
                arena,
            );
            },
            arena,
        );
    }

    #[test]
    fn test_errors_when_the_referenced_project_doesnt_exist() {
        let ref arena = AllArenasHarness::default();
        let spec = TestSpecification::from_iter([(
            "/primary".to_owned(),
            TestProjectSpecificationBuilder::default()
                .files(HashMap::from_iter(
                    [("/primary/a.ts", empty_module)].owned(),
                ))
                .references(["../foo".to_owned().into()])
                .build()
                .unwrap(),
        )]);
        test_project_references(
            &spec,
            "/primary/tsconfig.json",
            |program: &Program, _| {
                let errs = program.get_options_diagnostics(None);
                assert_has_error(
                    "Reports an error about a missing file",
                    &errs,
                    &Diagnostics::File_0_not_found,
                    arena,
                );
            },
            arena,
        );
    }

    #[test]
    fn test_errors_when_a_prepended_project_reference_doesnt_set_out_file() {
        let ref arena = AllArenasHarness::default();
        let spec = TestSpecification::from_iter([
            (
                "/primary".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [("/primary/a.ts", empty_module)].owned(),
                    ))
                    .references([Rc::new(
                        ProjectReferenceBuilder::default()
                            .path("../someProj")
                            .prepend(true)
                            .build()
                            .unwrap(),
                    )
                    .into()])
                    .build()
                    .unwrap(),
            ),
            (
                "/someProj".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [("/someProj/b.ts", "const x = 100;")].owned(),
                    ))
                    .build()
                    .unwrap(),
            ),
        ]);
        test_project_references(
            &spec,
            "/primary/tsconfig.json",
            |program: &Program, _| {
                let errs = program.get_options_diagnostics(None);
                assert_has_error(
                    "Reports an error about outFile not being set",
                    &errs,
                    &Diagnostics::Cannot_prepend_project_0_because_it_does_not_have_outFile_set,
                    arena,
                );
            },
            arena,
        );
    }

    #[test]
    fn test_errors_when_a_prepended_project_reference_output_doesnt_exist() {
        let ref arena = AllArenasHarness::default();
        let spec = TestSpecification::from_iter([
            (
                "/primary".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [("/primary/a.ts", "const y = x;")].owned(),
                    ))
                    .references([Rc::new(
                        ProjectReferenceBuilder::default()
                            .path("../someProj")
                            .prepend(true)
                            .build()
                            .unwrap(),
                    )
                    .into()])
                    .build()
                    .unwrap(),
            ),
            (
                "/someProj".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [("/someProj/b.ts", "const x = 100;")].owned(),
                    ))
                    .options(
                        arena.alloc_compiler_options(
                            CompilerOptionsBuilder::default()
                                .out_file("foo.js")
                                .build()
                                .unwrap(),
                        ),
                    )
                    .build()
                    .unwrap(),
            ),
        ]);
        test_project_references(
            &spec,
            "/primary/tsconfig.json",
            |program: &Program, _| {
                let errs = program.get_options_diagnostics(None);
                assert_has_error(
                    "Reports an error about outFile being missing",
                    &errs,
                    &Diagnostics::Output_file_0_from_project_1_does_not_exist,
                    arena,
                );
            },
            arena,
        );
    }
}

mod path_mapping {
    use super::*;

    #[test]
    fn test_redirects_to_the_output_d_ts_file() {
        let ref arena = AllArenasHarness::default();
        let spec = TestSpecification::from_iter([
            (
                "/alpha".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [("/alpha/a.ts", "export const m: number = 3;")].owned(),
                    ))
                    .output_files(HashMap::from_iter([("a.d.ts", empty_module)].owned()))
                    .build()
                    .unwrap(),
            ),
            (
                "/beta".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [("/beta/b.ts", "import { m } from '../alpha/a'")].owned(),
                    ))
                    .references(["../alpha".to_owned().into()])
                    .build()
                    .unwrap(),
            ),
        ]);
        test_project_references(
            &spec,
            "/beta/tsconfig.json",
            |program: &Program, _| {
                assert_no_errors(
                    "File setup should be correct",
                    &program.get_options_diagnostics(None),
                    arena,
                );
                assert_has_error(
                    "Found a type error",
                    &program.get_semantic_diagnostics(None, None).unwrap(),
                    &Diagnostics::Module_0_has_no_exported_member_1,
                    arena,
                );
            },
            arena,
        );
    }
}

mod nice_behavior {
    use super::*;

    #[test]
    fn test_issues_a_nice_error_when_the_input_file_is_missing() {
        let ref arena = AllArenasHarness::default();
        let spec = TestSpecification::from_iter([
            (
                "/alpha".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [("/alpha/a.ts", "export const m: number = 3;")].owned(),
                    ))
                    .build()
                    .unwrap(),
            ),
            (
                "/beta".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [("/beta/b.ts", "import { m } from '../alpha/a'")].owned(),
                    ))
                    .references(["../alpha".to_owned().into()])
                    .build()
                    .unwrap(),
            ),
        ]);
        test_project_references(
            &spec,
            "/beta/tsconfig.json",
            |program: &Program, _| {
                assert_has_error(
                    "Issues a useful error",
                    &program.get_semantic_diagnostics(None, None).unwrap(),
                    &Diagnostics::Output_file_0_has_not_been_built_from_source_file_1,
                    arena,
                );
            },
            arena,
        );
    }

    #[test]
    fn test_issues_a_nice_error_when_the_input_file_is_missing_when_module_reference_is_not_relative(
    ) {
        let ref arena = AllArenasHarness::default();
        let spec = TestSpecification::from_iter([
            (
                "/alpha".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [("/alpha/a.ts", "export const m: number = 3;")].owned(),
                    ))
                    .build()
                    .unwrap(),
            ),
            (
                "/beta".to_owned(),
                TestProjectSpecificationBuilder::default()
                    .files(HashMap::from_iter(
                        [("/beta/b.ts", "import { m } from '@alpha/a'")].owned(),
                    ))
                    .references(["../alpha".to_owned().into()])
                    .options(
                        arena.alloc_compiler_options(
                            CompilerOptionsBuilder::default()
                                .base_url("./")
                                .paths(HashMap::from_iter([("@alpha/*", ["/alpha/*"])].owned()))
                                .build()
                                .unwrap(),
                        ),
                    )
                    .build()
                    .unwrap(),
            ),
        ]);
        test_project_references(
            &spec,
            "/beta/tsconfig.json",
            |program: &Program, _| {
                assert_has_error(
                    "Issues a useful error",
                    &program.get_semantic_diagnostics(None, None).unwrap(),
                    &Diagnostics::Output_file_0_has_not_been_built_from_source_file_1,
                    arena,
                );
            },
            arena,
        );
    }
}

mod behavior_changes_under_composite_true {
    use typescript_rust::VecExtOrd;

    use super::*;

    #[test]
    fn test_doesnt_infer_the_root_dir_from_source_paths() {
        let ref arena = AllArenasHarness::default();
        let spec = TestSpecification::from_iter([(
            "/alpha".to_owned(),
            TestProjectSpecificationBuilder::default()
                .files(HashMap::from_iter(
                    [("/alpha/src/a.ts", "export const m: number = 3;")].owned(),
                ))
                .options(
                    arena.alloc_compiler_options(
                        CompilerOptionsBuilder::default()
                            .declaration(true)
                            .out_dir("bin")
                            .build()
                            .unwrap(),
                    ),
                )
                .build()
                .unwrap(),
        )]);
        test_project_references(
            &spec,
            "/alpha/tsconfig.json",
            |program: &Program, host: &fakes::CompilerHost| {
                program.emit(None, None, None, None, None, None).unwrap();
                assert_that!(host
                    .outputs()
                    .iter()
                    .map(|e| e.ref_(arena).file.clone())
                    .collect_vec()
                    .and_sort())
                .is_equal_to(
                    [
                        "/alpha/bin/src/a.d.ts",
                        "/alpha/bin/src/a.js",
                        "/alpha/bin/tsconfig.tsbuildinfo",
                    ]
                    .owned(),
                );
            },
            arena,
        );
    }
}

mod errors_when_a_file_in_a_composite_project_occurs_outside_the_root {
    use typescript_rust::ScriptReferenceHost;

    use super::*;

    #[test]
    fn test_errors_when_a_file_is_outside_the_rootdir() {
        let ref arena = AllArenasHarness::default();
        let spec = TestSpecification::from_iter([(
            "/alpha".to_owned(),
            TestProjectSpecificationBuilder::default()
                .files(HashMap::from_iter(
                    [
                        ("/alpha/src/a.ts", "import * from '../../beta/b'"),
                        ("/beta/b.ts", "export { }"),
                    ]
                    .owned(),
                ))
                .options(
                    arena.alloc_compiler_options(
                        CompilerOptionsBuilder::default()
                            .declaration(true)
                            .out_dir("bin")
                            .build()
                            .unwrap(),
                    ),
                )
                .build()
                .unwrap(),
        )]);
        test_project_references(
            &spec,
            "/alpha/tsconfig.json",
            |program: &Program, _| {
                let semantic_diagnostics = program
                    .get_semantic_diagnostics(program.get_source_file("/alpha/src/a.ts"), None)
                    .unwrap();
                assert_has_error(
                    "Issues an error about the rootDir",
                    &semantic_diagnostics,
                    &Diagnostics::File_0_is_not_under_rootDir_1_rootDir_is_expected_to_contain_all_source_files,
                    arena,
                );
                assert_has_error(
                    "Issues an error about the fileList",
                    &semantic_diagnostics,
                    &Diagnostics::File_0_is_not_listed_within_the_file_list_of_project_1_Projects_must_list_all_files_or_use_an_include_pattern,
                    arena,
                );
            },
            arena,
        );
    }
}
