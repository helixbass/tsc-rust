#![cfg(test)]

use speculoos::prelude::*;

mod parse_config_file_text_to_json {
    use super::*;

    use derive_builder::Builder;
    use gc::Gc;
    use harness::{fakes, vfs};
    use serde::Serialize;
    use serde_json::json;
    use typescript_rust::{
        get_sys_concrete, parse_config_file_text_to_json, parse_json_config_file_content,
        parse_json_source_file_config_file_content, parse_json_text,
        DiagnosticRelatedInformationInterface, Diagnostics, ParsedCommandLine, SliceExtCloneOrd,
    };

    #[derive(Builder, Default, Serialize)]
    #[builder(default, setter(strip_option))]
    struct ExpectedConfigObject {
        pub config: Option<serde_json::Value>,
        // TODO: this looks like in the Typescript version it is never
        // populated and is an array vs single node so would never actually
        // compare equal, upstream?
        // pub error: Option<Vec<Gc<Diagnostic>>>,
    }

    fn assert_parse_result(
        json_text: impl Into<String>,
        expected_config_object: ExpectedConfigObject,
    ) {
        let json_text = json_text.into();
        let parsed = parse_config_file_text_to_json("/apath/tsconfig.json", json_text).unwrap();
        assert_that!(serde_json::to_string(&parsed.config).unwrap())
            .is_equal_to(serde_json::to_string(&expected_config_object.config).unwrap());
        assert_that!(&parsed.error).is_none();
    }

    fn assert_parse_error_with_excludes_keyword(json_text: impl Into<String>) {
        let json_text = json_text.into();
        let parsed =
            parse_config_file_text_to_json("/apath/tsconfig.json", json_text.clone()).unwrap();
        let parsed_command = parse_json_config_file_content(
            parsed.config,
            &**get_sys_concrete(),
            "tests/cases/unittests",
            None,
            None,
            None,
            None,
            None,
            None,
        )
        .unwrap();
        let parsed_command_errors = (*parsed_command.errors).borrow();
        assert_that!(/*parsedCommandLine.errors &&*/ &*parsed_command_errors).has_length(1);
        assert_that!(&parsed_command_errors[0].code())
            .is_equal_to(Diagnostics::Unknown_option_excludes_Did_you_mean_exclude.code);

        let ref parsed = parse_json_text("/apath/tsconfig.json", json_text);
        let parsed_command = parse_json_source_file_config_file_content(
            parsed,
            &**get_sys_concrete(),
            "tests/cases/unittests",
            None,
            None,
            None,
            None,
            None,
            None,
        )
        .unwrap();
        let parsed_command_errors = (*parsed_command.errors).borrow();
        assert_that!(/*parsedCommand.errors &&*/ &*parsed_command_errors).has_length(1);
        assert_that!(&parsed_command_errors[0].code())
            .is_equal_to(Diagnostics::Unknown_option_excludes_Did_you_mean_exclude.code);
    }

    fn get_parsed_command_json(
        json_text: impl Into<String>,
        config_file_name: &str,
        base_path: &str,
        all_file_list: &[String],
    ) -> ParsedCommandLine {
        let json_text = json_text.into();
        let parsed = parse_config_file_text_to_json(config_file_name, json_text).unwrap();
        let mut files = vfs::FileSet::from_iter(all_file_list.into_iter().map(|value| {
            (
                value.clone(),
                Some(vfs::FileSetValue::String("".to_owned())),
            )
        }));
        let host = fakes::ParseConfigHost::new(Gc::new(
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
            )
            .unwrap(),
        ))
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
        )
        .unwrap()
    }

    fn get_parsed_command_json_node(
        json_text: impl Into<String>,
        config_file_name: &str,
        base_path: &str,
        all_file_list: &[String],
    ) -> ParsedCommandLine {
        let json_text = json_text.into();
        let ref parsed = parse_json_text(config_file_name, json_text);
        let mut files = vfs::FileSet::from_iter(all_file_list.into_iter().map(|value| {
            (
                value.clone(),
                Some(vfs::FileSetValue::String("".to_owned())),
            )
        }));
        let host = fakes::ParseConfigHost::new(Gc::new(
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
            )
            .unwrap(),
        ))
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
        let json_text = json_text.into();
        let parsed = get_parsed_command_json(
            json_text.clone(),
            config_file_name,
            base_path,
            all_file_list,
        );
        assert_that!(&parsed.file_names.sorted()).is_equal_to(expected_file_list.sorted());

        let parsed =
            get_parsed_command_json_node(json_text, config_file_name, base_path, all_file_list);
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
        let json_text = json_text.into();
        let parsed = get_parsed_command_json(
            json_text.clone(),
            config_file_name,
            base_path,
            all_file_list,
        );
        let parsed_errors = (*parsed.errors).borrow();
        // TODO: this looks trivially true, maybe meant > 0? Upstream?
        // assert.isTrue(parsed.errors.length >= 0);
        asserting(&format!(
            "Expected error code {expected_diagnostic_code} to be in {:?}",
            *parsed_errors
        ))
        .that(
            &parsed_errors
                .iter()
                .filter(|e| e.code() == expected_diagnostic_code)
                .count(),
        )
        .is_greater_than(0);

        let parsed =
            get_parsed_command_json_node(json_text, config_file_name, base_path, all_file_list);
        let parsed_errors = (*parsed.errors).borrow();
        // assert.isTrue(parsed.errors.length >= 0);
        asserting(&format!(
            "Expected error code {expected_diagnostic_code} to be in {:?}",
            *parsed_errors
        ))
        .that(
            &parsed_errors
                .iter()
                .filter(|e| e.code() == expected_diagnostic_code)
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
                    .filter(|e| e.code() == expected_diagnostic_code && e.maybe_file().is_some() && e.start() != 0 && e.length() != 0)
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
        let json_text = json_text.into();
        let parsed = get_parsed_command_json(
            json_text.clone(),
            config_file_name,
            base_path,
            all_file_list,
        );
        let parsed_errors = (*parsed.errors).borrow();
        // assert.isTrue(parsed.errors.length >= 0);
        asserting(&format!(
            "Expected error code {expected_excluded_diagnostic_code} to not be in {:?}",
            *parsed_errors
        ))
        .that(
            &parsed_errors
                .iter()
                .position(|e| e.code() == expected_excluded_diagnostic_code),
        )
        .is_none();

        let parsed =
            get_parsed_command_json_node(json_text, config_file_name, base_path, all_file_list);
        let parsed_errors = (*parsed.errors).borrow();
        // assert.isTrue(parsed.errors.length >= 0);
        asserting(&format!(
            "Expected error code {expected_excluded_diagnostic_code} to not be in {:?}",
            *parsed_errors
        ))
        .that(
            &parsed_errors
                .iter()
                .position(|e| e.code() == expected_excluded_diagnostic_code),
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
}
