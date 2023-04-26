#![cfg(test)]

use speculoos::prelude::*;

mod parse_command_line {
    use std::{io, iter, rc::Rc};

    use super::*;
    use gc::Gc;
    use itertools::Itertools;
    use typescript_rust::{
        compiler_options_did_you_mean_diagnostics, parse_command_line_worker, BaseDiagnostic,
        BaseDiagnosticRelatedInformationBuilder, CompilerOptionsBuilder, DiagnosticMessageText,
        DiagnosticRelatedInformationInterface, Diagnostics, ParseCommandLineWorkerDiagnostics,
        ParsedCommandLine, ParsedCommandLineBuilder,
    };

    fn assert_parse_result(
        command_line: impl IntoIterator<Item = &'static str>,
        expected_parsed_command_line: ParsedCommandLine,
        worker_diagnostic: Option<impl FnMut() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>,
    ) {
        let parsed = parse_command_line_worker(
            &*if let Some(mut worker_diagnostic) = worker_diagnostic {
                worker_diagnostic()
            } else {
                compiler_options_did_you_mean_diagnostics()
            },
            &command_line
                .into_iter()
                .map(ToOwned::to_owned)
                .collect_vec(),
            Option::<fn(&str) -> io::Result<Option<String>>>::None,
        )
        .into_parsed_command_line();
        assert_that(&*parsed.options).is_equal_to(&*expected_parsed_command_line.options);
        assert_that(&parsed.watch_options.as_deref())
            .is_equal_to(expected_parsed_command_line.watch_options.as_deref());

        let parsed_errors = (*parsed.errors).borrow();
        let expected_errors = (*expected_parsed_command_line.errors).borrow();
        asserting(&format!(
            "Expected error: {:?}. Actual error: {:?}",
            &*expected_errors, &*parsed_errors,
        ))
        .that(&*parsed_errors)
        .has_length(expected_errors.len());
        for (parsed_error, expected_error) in iter::zip(&*parsed_errors, &*expected_errors) {
            assert_that(&parsed_error.code()).is_equal_to(expected_error.code());
            assert_that(&parsed_error.category()).is_equal_to(expected_error.category());
            match expected_error.message_text() {
                DiagnosticMessageText::String(expected_error_message_text)
                    if expected_error_message_text.contains("[...]") =>
                {
                    // TODO: upstream fix where it looks like this currently isn't testing anything
                    // but should presumably be comparing against parsedError.messageText?
                    let prefix = expected_error_message_text.split("[...]").next().unwrap();
                    assert_that(parsed_error.message_text()).matches(|parsed_error_message_text| {
                        matches!(
                            parsed_error_message_text,
                            DiagnosticMessageText::String(parsed_error_message_text) if parsed_error_message_text.starts_with(prefix)
                        )
                    });
                }
                DiagnosticMessageText::String(expected_error_message_text) => {
                    assert_that(parsed_error.message_text()).matches(|parsed_error_message_text| {
                        matches!(
                            parsed_error_message_text,
                            DiagnosticMessageText::String(parsed_error_message_text) if parsed_error_message_text == expected_error_message_text
                        )
                    });
                }
                DiagnosticMessageText::DiagnosticMessageChain(expected_error_message_text) => {
                    assert_that(parsed_error.message_text()).matches(|parsed_error_message_text| {
                        matches!(
                            parsed_error_message_text,
                            DiagnosticMessageText::DiagnosticMessageChain(parsed_error_message_text) if parsed_error_message_text == expected_error_message_text
                        )
                    });
                }
            }
        }

        let parsed_file_names = &parsed.file_names;
        let expected_file_names = &expected_parsed_command_line.file_names;
        asserting(&format!(
            "Expected fileNames: [{:?}]. Actual fileNames: [{:?}]",
            expected_file_names, parsed_file_names,
        ))
        .that(parsed_file_names)
        .has_length(expected_file_names.len());
        for (parsed_file_name, expected_file_name) in
            iter::zip(parsed_file_names, expected_file_names)
        {
            assert_that(parsed_file_name).is_equal_to(expected_file_name);
        }
    }

    #[test]
    fn test_parse_single_option_of_library_flag() {
        assert_parse_result(
            ["--lib", "es6", "0.ts"],
            ParsedCommandLineBuilder::default()
                .file_names(vec!["0.ts".to_owned()])
                .options(
                    CompilerOptionsBuilder::default()
                        .lib(Some(vec!["lib.es2015.d.ts".to_owned()]))
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_handles_may_only_be_used_with_build_flags() {
        let build_flags = ["--clean", "--dry", "--force", "--verbose"];

        assert_parse_result(
            build_flags.clone(),
            ParsedCommandLineBuilder::default()
                .errors(
                    build_flags.into_iter().map(|build_flag| {
                        Gc::new(BaseDiagnostic::new(
                            BaseDiagnosticRelatedInformationBuilder::default()
                            .message_text(
                                format!(
                                    "Compiler option '{build_flag}' may only be used with '--build'."
                                )
                            )
                            .category(Diagnostics::Compiler_option_0_may_only_be_used_with_build.category)
                            .code(Diagnostics::Compiler_option_0_may_only_be_used_with_build.code)
                            .build().unwrap(),
                            None,
                        ).into())
                    }).collect_vec()
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_handles_did_you_mean_for_misspelt_flags() {
        assert_parse_result(
            ["--declarations", "--allowTS"],
            ParsedCommandLineBuilder::default()
                .errors([
                    Gc::new(BaseDiagnostic::new(
                        BaseDiagnosticRelatedInformationBuilder::default()
                        .message_text(
                            format!(
                                "Unknown compiler option '--declarations'. Did you mean 'declaration'?"
                            )
                        )
                        .category(Diagnostics::Unknown_compiler_option_0_Did_you_mean_1.category)
                        .code(Diagnostics::Unknown_compiler_option_0_Did_you_mean_1.code)
                        .build().unwrap(),
                        None,
                    ).into()),
                    Gc::new(BaseDiagnostic::new(
                        BaseDiagnosticRelatedInformationBuilder::default()
                        .message_text(
                            format!(
                                "Unknown compiler option '--allowTS'. Did you mean 'allowJs'?"
                            )
                        )
                        .category(Diagnostics::Unknown_compiler_option_0_Did_you_mean_1.category)
                        .code(Diagnostics::Unknown_compiler_option_0_Did_you_mean_1.code)
                        .build().unwrap(),
                        None,
                    ).into()),
                ])
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }
}
