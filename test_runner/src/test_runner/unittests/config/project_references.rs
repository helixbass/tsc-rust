#![cfg(test)]

use std::{collections::HashMap, rc::Rc};

use derive_builder::Builder;
use gc::Gc;
use harness::{fakes, vfs, TestFSWithWatch};
use itertools::Itertools;
use serde::Serialize;
use speculoos::prelude::*;
use typescript_rust::{
    combine_paths, create_program, flatten_diagnostic_message_text, get_directory_path,
    parse_config_host_from_compiler_host_like, parse_json_config_file_content, read_config_file,
    CompilerHostLikeRcDynCompilerHost, CompilerOptions, CreateProgramOptionsBuilder, Diagnostic,
    DiagnosticMessage, DiagnosticRelatedInformationInterface, MapOrDefault, ModuleResolutionHost,
    NonEmpty, Program, ProjectReference, ProjectReferenceBuilder, ReadConfigFileReturn,
    UnwrapOrEmpty,
};

#[derive(Builder, Clone, Default)]
#[builder(default, setter(strip_option, into))]
struct TestProjectSpecification {
    pub config_file_name: Option<String>,
    pub references: Option<Vec<StringOrProjectReference>>,
    pub files: HashMap<String, String>,
    pub output_files: Option<HashMap<String, String>>,
    pub config: Option<serde_json::Map<String, serde_json::Value>>,
    pub options: Option<Gc<CompilerOptions>>,
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

fn assert_has_error(message: &str, errors: &[Gc<Diagnostic>], diag: &DiagnosticMessage) {
    if !errors.into_iter().any(|e| e.code() == diag.code) {
        let error_string = errors
            .into_iter()
            .map(|e| {
                format!(
                    "    {}: {:?}",
                    if let Some(e_file) = e.maybe_file() {
                        e_file.as_source_file().file_name().clone()
                    } else {
                        "[global]".to_owned()
                    },
                    e.message_text()
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

fn assert_no_errors(message: &str, errors: &[Gc<Diagnostic>]) {
    if
    /*errors &&*/
    !errors.is_empty() {
        asserting(&format!(
            "{message}: Expected no errors, but found:\r\n{}",
            errors
                .into_iter()
                .map(|e| format!("    {:?}", e.message_text()))
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
        let mut compiler_options = sp.options.as_deref().map_or_default(Clone::clone);
        if compiler_options.composite.is_none() {
            compiler_options.composite = Some(true);
        }
        if compiler_options.out_dir.is_none() {
            compiler_options.out_dir = Some("bin".to_owned());
        }
        let options = TestProjectReferencesOptions::new(
            Gc::new(compiler_options),
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
        let out_dir = options.compiler_options.out_dir.as_ref().unwrap();
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

    let vfsys = Gc::new(vfs::FileSystem::new(
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
    ));
    for (k, v) in files {
        vfsys.mkdirp_sync(&get_directory_path(&k));
        vfsys.write_file_sync(&k, v, None);
    }
    let host = fakes::CompilerHost::new(Gc::new(fakes::System::new(vfsys, None)), None, None);
    let ReadConfigFileReturn { config, error } =
        read_config_file(entry_point_config_file_name, |name: &str| {
            host.read_file(name)
        });

    asserting(&flatten_diagnostic_message_text(
        error.as_ref().map(|error| error.message_text()),
        "\n",
        None,
    ))
    .that(&(config.is_some() && error.is_none()))
    .is_true();
    let mut file = parse_json_config_file_content(
        config,
        &parse_config_host_from_compiler_host_like(
            Gc::new(Box::new(CompilerHostLikeRcDynCompilerHost::from(
                host.as_dyn_compiler_host(),
            ))),
            None,
        ),
        &get_directory_path(entry_point_config_file_name),
        Some(Default::default()),
        Some(entry_point_config_file_name),
        None,
        None,
        None,
        None,
    );
    file.options = Gc::new({
        let mut file_options = (*file.options).clone();
        file_options.config_file_path = Some(entry_point_config_file_name.to_owned());
        file_options
    });
    let prog = create_program(
        CreateProgramOptionsBuilder::default()
            .root_names(file.file_names.clone())
            .options(file.options.clone())
            .host(host.as_dyn_compiler_host())
            .project_references(file.project_references.clone())
            .build()
            .unwrap(),
    );
    check_result(&prog, &host);
}

#[derive(Serialize)]
struct TestProjectReferencesOptions {
    compiler_options: Gc<CompilerOptions>,
    references: Vec<Rc<ProjectReference>>,
    #[serde(flatten)]
    sp_config: Option<serde_json::Map<String, serde_json::Value>>,
}

impl TestProjectReferencesOptions {
    fn new(
        compiler_options: Gc<CompilerOptions>,
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

mod project_references_meta_check {
    use typescript_rust::Owned;

    use super::*;

    #[test]
    fn test_default_setup_was_created_correctly() {
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
        test_project_references(&spec, "/primary/tsconfig.json", |prog: &Program, _| {
            // assert.isTrue(!!prog, "Program should exist");
            assert_no_errors(
                "Sanity check should not produce errors",
                &prog.get_options_diagnostics(None),
            );
        });
    }
}
