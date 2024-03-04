#![cfg(test)]

use speculoos::prelude::*;

mod parse_config_file_text_to_json {
    use std::env;

    use derive_builder::Builder;
    use harness::{fakes, vfs, AllArenasHarness, HasArenaHarness};
    use serde::Serialize;
    use serde_json::json;
    use typescript_rust::{
        convert_to_object, create_compiler_diagnostic, get_sys, id_arena::Id,
        parse_config_file_text_to_json, parse_json_config_file_content,
        parse_json_source_file_config_file_content, parse_json_text, AllArenas, Diagnostic,
        DiagnosticRelatedInformationInterface, Diagnostics, HasArena, InArena, Owned,
        ParsedCommandLine, SliceExtCloneOrd,
    };

    use super::*;

    #[derive(Builder, Default, Serialize)]
    #[builder(default, setter(strip_option))]
    struct ExpectedConfigObject {
        pub config: Option<serde_json::Value>,
        // TODO: this looks like in the Typescript version it is never
        // populated and is an array vs single node so would never actually
        // compare equal, upstream?
        // pub error: Option<Vec<Id<Diagnostic>>>,
    }

    fn assert_parse_result(
        json_text: impl Into<String>,
        expected_config_object: ExpectedConfigObject,
    ) {
        let json_text = json_text.into();
        let arena = AllArenas::default();
        let parsed =
            parse_config_file_text_to_json("/apath/tsconfig.json", json_text, &arena).unwrap();
        assert_that!(serde_json::to_string(&parsed.config).unwrap())
            .is_equal_to(serde_json::to_string(&expected_config_object.config).unwrap());
        assert_that!(&parsed.error).is_none();
    }

    fn assert_parse_error_with_excludes_keyword(json_text: impl Into<String>) {
        let json_text = json_text.into();
        let ref arena = AllArenas::default();
        let parsed =
            parse_config_file_text_to_json("/apath/tsconfig.json", json_text.clone(), arena)
                .unwrap();
        let parsed_command = parse_json_config_file_content(
            parsed.config,
            get_sys(arena)
                .ref_(arena)
                .maybe_as_dyn_parse_config_host()
                .unwrap(),
            // "tests/cases/unittests",
            &format!(
                "{}/prj/tsc-rust/typescript_rust/typescript_src/tests/cases/unittests",
                env::var("HOME").unwrap()
            ),
            None,
            None,
            None,
            None,
            None,
            None,
            arena,
        )
        .unwrap();
        assert_that!(/*parsedCommandLine.errors &&*/ &*parsed_command.errors.ref_(arena))
            .has_length(1);
        assert_that!(&parsed_command.errors.ref_(arena)[0].ref_(arena).code())
            .is_equal_to(Diagnostics::Unknown_option_excludes_Did_you_mean_exclude.code);

        let parsed = parse_json_text("/apath/tsconfig.json", json_text, arena);
        let parsed_command = parse_json_source_file_config_file_content(
            parsed,
            get_sys(arena)
                .ref_(arena)
                .maybe_as_dyn_parse_config_host()
                .unwrap(),
            // "tests/cases/unittests",
            &format!(
                "{}/prj/tsc-rust/typescript_rust/typescript_src/tests/cases/unittests",
                env::var("HOME").unwrap()
            ),
            None,
            None,
            None,
            None,
            None,
            None,
            arena,
        )
        .unwrap();
        let parsed_command_errors = parsed_command.errors.ref_(arena);
        assert_that!(/*parsedCommand.errors &&*/ &*parsed_command_errors).has_length(1);
        assert_that!(&parsed_command_errors[0].ref_(arena).code())
            .is_equal_to(Diagnostics::Unknown_option_excludes_Did_you_mean_exclude.code);
    }

    fn get_parsed_command_json(
        json_text: impl Into<String>,
        config_file_name: &str,
        base_path: &str,
        all_file_list: &[String],
        arena: &impl HasArenaHarness,
    ) -> ParsedCommandLine {
        let json_text = json_text.into();
        let parsed = parse_config_file_text_to_json(config_file_name, json_text, arena).unwrap();
        let mut files = vfs::FileSet::from_iter(all_file_list.into_iter().map(|value| {
            (
                value.clone(),
                Some(vfs::FileSetValue::String("".to_owned())),
            )
        }));
        let host = fakes::ParseConfigHost::new(
            arena.alloc_file_system(
                vfs::FileSystem::new(
                    false,
                    Some(
                        vfs::FileSystemOptionsBuilder::default()
                            .cwd(base_path)
                            .files({
                                files.insert(
                                    "/".to_owned(),
                                    Some(vfs::FileSetValue::FileSet(Default::default())),
                                );
                                files
                            })
                            .build()
                            .unwrap(),
                    ),
                    arena,
                )
                .unwrap(),
            ),
            arena,
        )
        .unwrap();
        parse_json_config_file_content(
            parsed.config,
            &host,
            base_path,
            None,
            Some(config_file_name),
            None,
            None,
            None,
            None,
            arena,
        )
        .unwrap()
    }

    fn get_parsed_command_json_node(
        json_text: impl Into<String>,
        config_file_name: &str,
        base_path: &str,
        all_file_list: &[String],
        arena: &impl HasArenaHarness,
    ) -> ParsedCommandLine {
        let json_text = json_text.into();
        let parsed = parse_json_text(config_file_name, json_text, arena);
        let mut files = vfs::FileSet::from_iter(all_file_list.into_iter().map(|value| {
            (
                value.clone(),
                Some(vfs::FileSetValue::String("".to_owned())),
            )
        }));
        let host = fakes::ParseConfigHost::new(
            arena.alloc_file_system(
                vfs::FileSystem::new(
                    false,
                    Some(
                        vfs::FileSystemOptionsBuilder::default()
                            .cwd(base_path)
                            .files({
                                files.insert(
                                    "/".to_owned(),
                                    Some(vfs::FileSetValue::FileSet(Default::default())),
                                );
                                files
                            })
                            .build()
                            .unwrap(),
                    ),
                    arena,
                )
                .unwrap(),
            ),
            arena,
        )
        .unwrap();
        parse_json_source_file_config_file_content(
            parsed,
            &host,
            base_path,
            None,
            Some(config_file_name),
            None,
            None,
            None,
            None,
            arena,
        )
        .unwrap()
    }

    fn assert_parse_file_list(
        json_text: impl Into<String>,
        config_file_name: &str,
        base_path: &str,
        all_file_list: &[String],
        expected_file_list: &[String],
    ) {
        let ref arena = AllArenasHarness::default();
        let json_text = json_text.into();
        let parsed = get_parsed_command_json(
            json_text.clone(),
            config_file_name,
            base_path,
            all_file_list,
            arena,
        );
        assert_that!(&parsed.file_names.sorted()).is_equal_to(expected_file_list.sorted());

        let parsed = get_parsed_command_json_node(
            json_text,
            config_file_name,
            base_path,
            all_file_list,
            arena,
        );
        assert_that!(&parsed.file_names.sorted()).is_equal_to(expected_file_list.sorted());
    }

    fn assert_parse_file_diagnostics(
        json_text: impl Into<String>,
        config_file_name: &str,
        base_path: &str,
        all_file_list: &[String],
        // TODO: introduce type alias for diagnostic codes eg DiagnosticCode?
        expected_diagnostic_code: u32,
        no_location: Option<bool>,
    ) {
        let ref arena = AllArenasHarness::default();
        let json_text = json_text.into();
        let base_path = &format!(
            "{}/prj/tsc-rust/typescript_rust/typescript_src/{base_path}",
            env::var("HOME").unwrap()
        );
        let parsed = get_parsed_command_json(
            json_text.clone(),
            config_file_name,
            base_path,
            all_file_list,
            arena,
        );
        // TODO: this looks trivially true, maybe meant > 0? Upstream?
        // assert.isTrue(parsed.errors.length >= 0);
        asserting(&format!(
            "Expected error code {expected_diagnostic_code} to be in {:?}",
            *parsed.errors.ref_(arena)
        ))
        .that(
            &parsed
                .errors
                .ref_(arena)
                .iter()
                .filter(|e| e.ref_(arena).code() == expected_diagnostic_code)
                .count(),
        )
        .is_greater_than(0);

        let parsed = get_parsed_command_json_node(
            json_text,
            config_file_name,
            base_path,
            all_file_list,
            arena,
        );
        let parsed_errors = parsed.errors.ref_(arena);
        // assert.isTrue(parsed.errors.length >= 0);
        asserting(&format!(
            "Expected error code {expected_diagnostic_code} to be in {:?}",
            *parsed_errors
        ))
        .that(
            &parsed_errors
                .iter()
                .filter(|e| e.ref_(arena).code() == expected_diagnostic_code)
                .count(),
        )
        .is_greater_than(0);
        if no_location != Some(true) {
            asserting(&format!(
                "Expected error code {expected_diagnostic_code} to be in {:?} with location information",
                *parsed_errors
            ))
            .that(
                &parsed_errors
                    .iter()
                    .filter(|e| {
                        e.ref_(arena).code() == expected_diagnostic_code &&
                            e.ref_(arena).maybe_file().is_some() &&
                            e.ref_(arena).start() != 0 &&
                            e.ref_(arena).length() != 0
                    })
                    .count(),
            )
            .is_greater_than(0);
        }
    }

    fn assert_parse_file_diagnostics_exclusion(
        json_text: impl Into<String>,
        config_file_name: &str,
        base_path: &str,
        all_file_list: &[String],
        expected_excluded_diagnostic_code: u32,
    ) {
        let ref arena = AllArenasHarness::default();
        let json_text = json_text.into();
        let parsed = get_parsed_command_json(
            json_text.clone(),
            config_file_name,
            base_path,
            all_file_list,
            arena,
        );
        // assert.isTrue(parsed.errors.length >= 0);
        asserting(&format!(
            "Expected error code {expected_excluded_diagnostic_code} to not be in {:?}",
            *parsed.errors.ref_(arena),
        ))
        .that(
            &parsed
                .errors
                .ref_(arena)
                .iter()
                .position(|e| e.ref_(arena).code() == expected_excluded_diagnostic_code),
        )
        .is_none();

        let parsed = get_parsed_command_json_node(
            json_text,
            config_file_name,
            base_path,
            all_file_list,
            arena,
        );
        let parsed_errors = parsed.errors.ref_(arena);
        // assert.isTrue(parsed.errors.length >= 0);
        asserting(&format!(
            "Expected error code {expected_excluded_diagnostic_code} to not be in {:?}",
            *parsed_errors
        ))
        .that(
            &parsed_errors
                .iter()
                .position(|e| e.ref_(arena).code() == expected_excluded_diagnostic_code),
        )
        .is_none();
    }

    #[test]
    fn test_returns_empty_config_for_file_with_only_whitespaces() {
        assert_parse_result(
            "",
            ExpectedConfigObjectBuilder::default()
                .config(json!({}))
                .build()
                .unwrap(),
        );
        assert_parse_result(
            " ",
            ExpectedConfigObjectBuilder::default()
                .config(json!({}))
                .build()
                .unwrap(),
        );
    }

    #[test]
    fn test_returns_empty_config_for_file_with_comments_only() {
        assert_parse_result(
            "// Comment",
            ExpectedConfigObjectBuilder::default()
                .config(json!({}))
                .build()
                .unwrap(),
        );
        assert_parse_result(
            "/* Comment*/",
            ExpectedConfigObjectBuilder::default()
                .config(json!({}))
                .build()
                .unwrap(),
        );
    }

    #[test]
    fn test_returns_empty_config_when_config_is_empty_object() {
        assert_parse_result(
            "{}",
            ExpectedConfigObjectBuilder::default()
                .config(json!({}))
                .build()
                .unwrap(),
        );
    }

    #[test]
    fn test_returns_config_object_without_comments() {
        assert_parse_result(
            r#"{ // Excluded files
                "exclude": [
                    // Exclude d.ts
                    "file.d.ts"
                ]
            }"#,
            ExpectedConfigObjectBuilder::default()
                .config(json!({ "exclude": ["file.d.ts"] }))
                .build()
                .unwrap(),
        );
        assert_parse_result(
            r#"{
                /* Excluded
                     Files
                */
                "exclude": [
                    /* multiline comments can be in the middle of a line */"file.d.ts"
                ]
            }"#,
            ExpectedConfigObjectBuilder::default()
                .config(json!({ "exclude": ["file.d.ts"] }))
                .build()
                .unwrap(),
        );
    }

    #[test]
    fn test_keeps_string_content_untouched() {
        assert_parse_result(
            r#"{
                "exclude": [
                    "xx//file.d.ts"
                ]
            }"#,
            ExpectedConfigObjectBuilder::default()
                .config(json!({ "exclude": ["xx//file.d.ts"] }))
                .build()
                .unwrap(),
        );
        assert_parse_result(
            r#"{
                "exclude": [
                    "xx/*file.d.ts*/"
                ]
            }"#,
            ExpectedConfigObjectBuilder::default()
                .config(json!({ "exclude": ["xx/*file.d.ts*/"] }))
                .build()
                .unwrap(),
        );
    }

    #[test]
    fn test_handles_escape_characters_in_strings_correctly() {
        assert_parse_result(
            "{
                \"exclude\": [
                    \"xx\\\"//files\"
                ]
            }",
            ExpectedConfigObjectBuilder::default()
                .config(json!({ "exclude": ["xx\"//files"] }))
                .build()
                .unwrap(),
        );
        assert_parse_result(
            "{
                \"exclude\": [
                    \"xx\\\\\" // end of line comment
                ]
            }",
            ExpectedConfigObjectBuilder::default()
                .config(json!({ "exclude": ["xx\\"] }))
                .build()
                .unwrap(),
        );
    }

    #[test]
    fn test_returns_object_with_error_when_json_is_invalid() {
        let ref arena = AllArenasHarness::default();
        let parsed =
            parse_config_file_text_to_json("/apath/tsconfig.json", "invalid".to_owned(), arena)
                .unwrap();
        assert_that!(&parsed.config).is_equal_to(Some(json!({})));
        let expected: Id<Diagnostic> = arena.alloc_diagnostic(
            create_compiler_diagnostic(&Diagnostics::_0_expected, Some(vec!["{".to_owned()]))
                .into(),
        );
        let error = parsed.error.clone().unwrap();
        assert_that!(error.ref_(arena).message_text())
            .is_equal_to(expected.ref_(arena).message_text());
        assert_that!(error.ref_(arena).category()).is_equal_to(expected.ref_(arena).category());
        assert_that!(error.ref_(arena).code()).is_equal_to(expected.ref_(arena).code());
        assert_that!(error.ref_(arena).start()).is_equal_to(0);
        assert_that!(error.ref_(arena).length())
            .is_equal_to(isize::try_from("invalid".len()).unwrap());
    }

    #[test]
    fn test_returns_object_when_users_correctly_specify_library() {
        assert_parse_result(
            r#"{
                "compilerOptions": {
                    "lib": ["es5"]
                }
            }"#,
            ExpectedConfigObjectBuilder::default()
                .config(json!({ "compilerOptions": { "lib": ["es5"] } }))
                .build()
                .unwrap(),
        );
        assert_parse_result(
            r#"{
                "compilerOptions": {
                    "lib": ["es5", "es6"]
                }
            }"#,
            ExpectedConfigObjectBuilder::default()
                .config(json!({ "compilerOptions": { "lib": ["es5", "es6"] } }))
                .build()
                .unwrap(),
        );
    }

    #[test]
    fn test_returns_error_when_tsconfig_have_excludes() {
        assert_parse_error_with_excludes_keyword(
            r#"{
                "compilerOptions": {
                    "lib": ["es5"]
                },
                "excludes": [
                    "foge.ts"
                ]
            }"#,
        )
    }

    #[test]
    fn test_ignore_dotted_files_and_folders() {
        assert_parse_file_list(
            "{}",
            "tsconfig.json",
            "/apath",
            &[
                "/apath/test.ts",
                "/apath/.git/a.ts",
                "/apath/.b.ts",
                "/apath/..c.ts",
            ]
            .owned(),
            &["/apath/test.ts"].owned(),
        );
    }

    #[test]
    fn test_allow_dotted_files_and_folders_when_explicitly_requested() {
        assert_parse_file_list(
            r#"{
                "files": ["/apath/.git/a.ts", "/apath/.b.ts", "/apath/..c.ts"]
            }"#,
            "tsconfig.json",
            "/apath",
            &[
                "/apath/test.ts",
                "/apath/.git/a.ts",
                "/apath/.b.ts",
                "/apath/..c.ts",
            ]
            .owned(),
            &["/apath/.git/a.ts", "/apath/.b.ts", "/apath/..c.ts"].owned(),
        );
    }

    #[test]
    fn test_exclude_out_dir_unless_overridden() {
        let tsconfig_without_exclude = r#"{
            "compilerOptions": {
                "outDir": "bin"
            }
        }"#;
        let tsconfig_with_exclude = r#"{
            "compilerOptions": {
                "outDir": "bin"
            },
            "exclude": [ "obj" ]
        }"#;
        let root_dir = "/";
        let all_files = ["/bin/a.ts", "/b.ts"].owned();
        let expected_files = ["/b.ts"].owned();
        assert_parse_file_list(
            tsconfig_without_exclude,
            "tsconfig.json",
            root_dir,
            &all_files,
            &expected_files,
        );
        assert_parse_file_list(
            tsconfig_with_exclude,
            "tsconfig.json",
            root_dir,
            &all_files,
            &all_files,
        );
    }

    #[test]
    fn test_exclude_declaration_dir_unless_overridden() {
        let tsconfig_without_exclude = r#"{
            "compilerOptions": {
                "declarationDir": "declarations"
            }
        }"#;
        let tsconfig_with_exclude = r#"{
            "compilerOptions": {
                "declarationDir": "declarations"
            },
            "exclude": [ "types" ]
        }"#;
        let root_dir = "/";
        let all_files = ["/declarations/a.d.ts", "/a.ts"].owned();
        let expected_files = ["/a.ts"].owned();

        assert_parse_file_list(
            tsconfig_without_exclude,
            "tsconfig.json",
            root_dir,
            &all_files,
            &expected_files,
        );
        assert_parse_file_list(
            tsconfig_with_exclude,
            "tsconfig.json",
            root_dir,
            &all_files,
            &all_files,
        );
    }

    #[test]
    fn test_implicitly_exclude_common_package_folders() {
        assert_parse_file_list(
            "{}",
            "tsconfig.json",
            "/",
            &[
                "/node_modules/a.ts",
                "/bower_components/b.ts",
                "/jspm_packages/c.ts",
                "/d.ts",
                "/folder/e.ts",
            ]
            .owned(),
            &["/d.ts", "/folder/e.ts"].owned(),
        );
    }

    #[test]
    fn test_parse_and_re_emit_tsconfig_json_file_with_diagnostics() {
        let ref arena = AllArenasHarness::default();
        let content = r#"{
            "compilerOptions": {
                "allowJs": true
                // Some comments
                "outDir": "bin"
            }
            "files": ["file1.ts"]
        }"#;
        let result = parse_json_text("config.json", content.to_owned(), arena);
        let diagnostics = result.ref_(arena).as_source_file().parse_diagnostics();
        let config_json_object = convert_to_object(result, diagnostics.clone(), arena)
            .unwrap()
            .unwrap();
        let diagnostics = diagnostics.ref_(arena);
        let expected_result = json!({
            "compilerOptions": {
                "allowJs": true,
                "outDir": "bin"
            },
            "files": ["file1.ts"]
        });
        assert_that!(&*diagnostics).has_length(2);
        assert_that!(&serde_json::to_string(&config_json_object).unwrap())
            .is_equal_to(serde_json::to_string(&expected_result).unwrap());
    }

    #[test]
    fn test_generates_errors_for_empty_files_list() {
        let content = r#"{
            "files": []
        }"#;
        assert_parse_file_diagnostics(
            content,
            "/apath/tsconfig.json",
            "tests/cases/unittests",
            &["/apath/a.ts"].owned(),
            Diagnostics::The_files_list_in_config_file_0_is_empty.code,
            None,
        );
    }

    #[test]
    fn test_generates_errors_for_empty_files_list_when_no_references_are_provided() {
        let content = r#"{
            "files": [],
            "references": []
        }"#;
        assert_parse_file_diagnostics(
            content,
            "/apath/tsconfig.json",
            "tests/cases/unittests",
            &["/apath/a.ts"].owned(),
            Diagnostics::The_files_list_in_config_file_0_is_empty.code,
            None,
        );
    }

    #[test]
    fn test_does_not_generate_errors_for_empty_files_list_when_one_or_more_references_are_provided()
    {
        let content = r#"{
            "files": [],
            "references": [{ "path": "/apath" }]
        }"#;
        assert_parse_file_diagnostics_exclusion(
            content,
            "/apath/tsconfig.json",
            "tests/cases/unittests",
            &["/apath/a.ts"].owned(),
            Diagnostics::The_files_list_in_config_file_0_is_empty.code,
        );
    }

    #[test]
    fn test_generates_errors_for_directory_with_no_ts_files() {
        let content = r#"{
        }"#;
        assert_parse_file_diagnostics(
            content,
            "/apath/tsconfig.json",
            "tests/cases/unittests",
            &["/apath/a.js"].owned(),
            Diagnostics::No_inputs_were_found_in_config_file_0_Specified_include_paths_were_1_and_exclude_paths_were_2.code,
            Some(true),
        );
    }

    #[test]
    fn test_generates_errors_for_empty_directory() {
        let content = r#"{
            "compilerOptions": {
                "allowJs": true
            }
        }"#;
        assert_parse_file_diagnostics(
            content,
            "/apath/tsconfig.json",
            "tests/cases/unittests",
            &[],
            Diagnostics::No_inputs_were_found_in_config_file_0_Specified_include_paths_were_1_and_exclude_paths_were_2.code,
            Some(true),
        );
    }

    #[test]
    fn test_generates_errors_for_empty_include() {
        let content = r#"{
            "include": []
        }"#;
        assert_parse_file_diagnostics(
            content,
            "/apath/tsconfig.json",
            "tests/cases/unittests",
            &["/apath/a.ts"].owned(),
            Diagnostics::No_inputs_were_found_in_config_file_0_Specified_include_paths_were_1_and_exclude_paths_were_2.code,
            Some(true),
        );
    }

    #[test]
    fn test_generates_errors_for_includes_with_out_dir() {
        let content = r#"{
            "compilerOptions": {
                "outDir": "./"
            },
            "include": ["**/*"]
        }"#;
        assert_parse_file_diagnostics(
            content,
            "/apath/tsconfig.json",
            "tests/cases/unittests",
            &["/apath/a.ts"].owned(),
            Diagnostics::No_inputs_were_found_in_config_file_0_Specified_include_paths_were_1_and_exclude_paths_were_2.code,
            Some(true),
        );
    }

    #[test]
    fn test_generates_errors_for_when_invalid_comment_type_present_in_tsconfig() {
        let ref arena = AllArenasHarness::default();
        let json_text = r#"{
           "compilerOptions": {
             ## this comment does cause issues
             "types" : [
             ]
           }
        }"#;
        let parsed = get_parsed_command_json_node(
            json_text,
            "/apath/tsconfig.json",
            "tests/cases/unittests",
            &["/apath/a.ts"].owned(),
            arena,
        );
        // TODO: the Typescript version has length >= 0 which seems trivially true and
        // seems to disagree with the test name, upstream?
        assert_that!(&parsed.errors.ref_(arena).len()).is_greater_than(0);
    }

    #[test]
    fn test_generates_errors_when_files_is_not_string() {
        assert_parse_file_diagnostics(
            json!({
                "files": [{
                    "compilerOptions": {
                        "experimentalDecorators": true,
                        "allowJs": true
                    }
                }]
            })
            .to_string(),
            "/apath/tsconfig.json",
            "tests/cases/unittests",
            &["/apath/a.ts"].owned(),
            Diagnostics::Compiler_option_0_requires_a_value_of_type_1.code,
            Some(true),
        );
    }

    #[test]
    fn test_generates_errors_when_include_is_not_string() {
        assert_parse_file_diagnostics(
            json!({
                "include": [
                    ["./**/*.ts"]
                ]
            })
            .to_string(),
            "/apath/tsconfig.json",
            "tests/cases/unittests",
            &["/apath/a.ts"].owned(),
            Diagnostics::Compiler_option_0_requires_a_value_of_type_1.code,
            Some(true),
        );
    }
}
