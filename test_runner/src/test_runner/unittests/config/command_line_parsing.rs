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
        DiagnosticRelatedInformationInterface, Diagnostics, ModuleKind,
        ParseCommandLineWorkerDiagnostics, ParsedCommandLine, ParsedCommandLineBuilder,
        ScriptTarget,
    };

    fn assert_parse_result<'command_line>(
        command_line: impl IntoIterator<Item = &'command_line str>,
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
                        .lib(["lib.es2015.d.ts".to_owned()])
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

    #[test]
    fn test_parse_multiple_options_of_library_flags() {
        assert_parse_result(
            ["--lib", "es5,es2015.symbol.wellknown", "0.ts"],
            ParsedCommandLineBuilder::default()
                .file_names(["0.ts".to_owned()])
                .options(
                    CompilerOptionsBuilder::default()
                        .lib([
                            "lib.es5.d.ts".to_owned(),
                            "lib.es2015.symbol.wellknown.d.ts".to_owned(),
                        ])
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_invalid_option_of_library_flags() {
        assert_parse_result(
            ["--lib", "es5,invalidOption", "0.ts"],
            ParsedCommandLineBuilder::default()
                .file_names(["0.ts".to_owned()])
                .errors([Gc::new(
                    BaseDiagnostic::new(
                        BaseDiagnosticRelatedInformationBuilder::default()
                            .message_text(
                                "Argument for '--lib' option must be: 'es5', 'es6', [...]"
                                    .to_owned(),
                            )
                            .category(Diagnostics::Argument_for_0_option_must_be_Colon_1.category)
                            .code(Diagnostics::Argument_for_0_option_must_be_Colon_1.code)
                            .build()
                            .unwrap(),
                        None,
                    )
                    .into(),
                )])
                .options(
                    CompilerOptionsBuilder::default()
                        .lib(["lib.es5.d.ts".to_owned()])
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_empty_options_of_jsx() {
        assert_parse_result(
            ["0.ts", "--jsx"],
            ParsedCommandLineBuilder::default()
                .file_names(["0.ts".to_owned()])
                .errors([
                    Gc::new(
                        BaseDiagnostic::new(
                            BaseDiagnosticRelatedInformationBuilder::default()
                                .message_text(
                                    "Compiler option 'jsx' expects an argument."
                                        .to_owned(),
                                )
                                .category(Diagnostics::Compiler_option_0_expects_an_argument.category)
                                .code(Diagnostics::Compiler_option_0_expects_an_argument.code)
                                .build()
                                .unwrap(),
                            None,
                        )
                        .into(),
                    ),
                    Gc::new(
                        BaseDiagnostic::new(
                            BaseDiagnosticRelatedInformationBuilder::default()
                                .message_text(
                                    "Argument for '--jsx' option must be: 'preserve', 'react-native', 'react', 'react-jsx', 'react-jsxdev'.".to_owned(),
                                )
                                .category(Diagnostics::Argument_for_0_option_must_be_Colon_1.category)
                                .code(Diagnostics::Argument_for_0_option_must_be_Colon_1.code)
                                .build()
                                .unwrap(),
                            None,
                        )
                        .into(),
                    )
                ])
                .file_names([
                    "0.ts".to_owned()
                ])
                .options(
                    CompilerOptionsBuilder::default()
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_empty_options_of_module() {
        assert_parse_result(
            ["0.ts", "--module"],
            ParsedCommandLineBuilder::default()
                .errors([
                    Gc::new(
                        BaseDiagnostic::new(
                            BaseDiagnosticRelatedInformationBuilder::default()
                                .message_text(
                                    "Compiler option 'module' expects an argument."
                                        .to_owned(),
                                )
                                .category(Diagnostics::Compiler_option_0_expects_an_argument.category)
                                .code(Diagnostics::Compiler_option_0_expects_an_argument.code)
                                .build()
                                .unwrap(),
                            None,
                        )
                        .into(),
                    ),
                    Gc::new(
                        BaseDiagnostic::new(
                            BaseDiagnosticRelatedInformationBuilder::default()
                                .message_text(
                                    "Argument for '--module' option must be: 'none', 'commonjs', 'amd', 'system', 'umd', 'es6', 'es2015', 'es2020', 'es2022', 'esnext', 'node12', 'nodenext'.".to_owned()
                                )
                                .category(Diagnostics::Argument_for_0_option_must_be_Colon_1.category)
                                .code(Diagnostics::Argument_for_0_option_must_be_Colon_1.code)
                                .build()
                                .unwrap(),
                            None,
                        )
                        .into(),
                    )
                ])
                .file_names([
                    "0.ts".to_owned()
                ])
                .options(
                    CompilerOptionsBuilder::default()
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_empty_options_of_new_line() {
        assert_parse_result(
            ["0.ts", "--newLine"],
            ParsedCommandLineBuilder::default()
                .errors([
                    Gc::new(
                        BaseDiagnostic::new(
                            BaseDiagnosticRelatedInformationBuilder::default()
                                .message_text(
                                    "Compiler option 'newLine' expects an argument.".to_owned(),
                                )
                                .category(
                                    Diagnostics::Compiler_option_0_expects_an_argument.category,
                                )
                                .code(Diagnostics::Compiler_option_0_expects_an_argument.code)
                                .build()
                                .unwrap(),
                            None,
                        )
                        .into(),
                    ),
                    Gc::new(
                        BaseDiagnostic::new(
                            BaseDiagnosticRelatedInformationBuilder::default()
                                .message_text(
                                    "Argument for '--newLine' option must be: 'crlf', 'lf'."
                                        .to_owned(),
                                )
                                .category(
                                    Diagnostics::Argument_for_0_option_must_be_Colon_1.category,
                                )
                                .code(Diagnostics::Argument_for_0_option_must_be_Colon_1.code)
                                .build()
                                .unwrap(),
                            None,
                        )
                        .into(),
                    ),
                ])
                .file_names(["0.ts".to_owned()])
                .options(CompilerOptionsBuilder::default().build().unwrap())
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_empty_options_of_target() {
        assert_parse_result(
            ["0.ts", "--target"],
            ParsedCommandLineBuilder::default()
                .errors([
                    Gc::new(
                        BaseDiagnostic::new(
                            BaseDiagnosticRelatedInformationBuilder::default()
                                .message_text(
                                    "Compiler option 'target' expects an argument.".to_owned(),
                                )
                                .category(
                                    Diagnostics::Compiler_option_0_expects_an_argument.category,
                                )
                                .code(Diagnostics::Compiler_option_0_expects_an_argument.code)
                                .build()
                                .unwrap(),
                            None,
                        )
                        .into(),
                    ),
                    Gc::new(
                        BaseDiagnostic::new(
                            BaseDiagnosticRelatedInformationBuilder::default()
                                .message_text(
                                    "Argument for '--target' option must be: 'es3', 'es5', 'es6', 'es2015', 'es2016', 'es2017', 'es2018', 'es2019', 'es2020', 'es2021', 'esnext'.".to_owned(),
                                )
                                .category(
                                    Diagnostics::Argument_for_0_option_must_be_Colon_1.category,
                                )
                                .code(Diagnostics::Argument_for_0_option_must_be_Colon_1.code)
                                .build()
                                .unwrap(),
                            None,
                        )
                        .into(),
                    ),
                ])
                .file_names(["0.ts".to_owned()])
                .options(CompilerOptionsBuilder::default().build().unwrap())
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_empty_options_of_module_resolution() {
        assert_parse_result(
            ["0.ts", "--moduleResolution"],
            ParsedCommandLineBuilder::default()
                .errors([
                    Gc::new(
                        BaseDiagnostic::new(
                            BaseDiagnosticRelatedInformationBuilder::default()
                                .message_text(
                                    "Compiler option 'moduleResolution' expects an argument.".to_owned(),
                                )
                                .category(
                                    Diagnostics::Compiler_option_0_expects_an_argument.category,
                                )
                                .code(Diagnostics::Compiler_option_0_expects_an_argument.code)
                                .build()
                                .unwrap(),
                            None,
                        )
                        .into(),
                    ),
                    Gc::new(
                        BaseDiagnostic::new(
                            BaseDiagnosticRelatedInformationBuilder::default()
                                .message_text(
                                    "Argument for '--moduleResolution' option must be: 'node', 'classic', 'node12', 'nodenext'.".to_owned(),
                                )
                                .category(
                                    Diagnostics::Argument_for_0_option_must_be_Colon_1.category,
                                )
                                .code(Diagnostics::Argument_for_0_option_must_be_Colon_1.code)
                                .build()
                                .unwrap(),
                            None,
                        )
                        .into(),
                    ),
                ])
                .file_names(["0.ts".to_owned()])
                .options(CompilerOptionsBuilder::default().build().unwrap())
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_empty_options_of_lib() {
        assert_parse_result(
            ["0.ts", "--lib"],
            ParsedCommandLineBuilder::default()
                .errors([Gc::new(
                    BaseDiagnostic::new(
                        BaseDiagnosticRelatedInformationBuilder::default()
                            .message_text("Compiler option 'lib' expects an argument.".to_owned())
                            .category(Diagnostics::Compiler_option_0_expects_an_argument.category)
                            .code(Diagnostics::Compiler_option_0_expects_an_argument.code)
                            .build()
                            .unwrap(),
                        None,
                    )
                    .into(),
                )])
                .file_names(["0.ts".to_owned()])
                .options(
                    CompilerOptionsBuilder::default()
                        .lib(vec![])
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_empty_string_of_lib() {
        assert_parse_result(
            ["0.ts", "--lib", ""],
            ParsedCommandLineBuilder::default()
                .errors([Gc::new(
                    BaseDiagnostic::new(
                        BaseDiagnosticRelatedInformationBuilder::default()
                            .message_text("Compiler option 'lib' expects an argument.".to_owned())
                            .category(Diagnostics::Compiler_option_0_expects_an_argument.category)
                            .code(Diagnostics::Compiler_option_0_expects_an_argument.code)
                            .build()
                            .unwrap(),
                        None,
                    )
                    .into(),
                )])
                .file_names(["0.ts".to_owned()])
                .options(
                    CompilerOptionsBuilder::default()
                        .lib(vec![])
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_immediately_following_command_line_argument_of_lib() {
        assert_parse_result(
            ["0.ts", "--lib", "--sourcemap"],
            ParsedCommandLineBuilder::default()
                .file_names(["0.ts".to_owned()])
                .options(
                    CompilerOptionsBuilder::default()
                        .lib(vec![])
                        .source_map(true)
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_lib_option_with_extra_comma() {
        assert_parse_result(
            ["--lib", "es5,", "es7", "0.ts"],
            ParsedCommandLineBuilder::default()
                .errors([Gc::new(
                    BaseDiagnostic::new(
                        BaseDiagnosticRelatedInformationBuilder::default()
                            .message_text(
                                "Argument for '--lib' option must be: 'es5', 'es6', [...]."
                                    .to_owned(),
                            )
                            .category(Diagnostics::Argument_for_0_option_must_be_Colon_1.category)
                            .code(Diagnostics::Argument_for_0_option_must_be_Colon_1.code)
                            .build()
                            .unwrap(),
                        None,
                    )
                    .into(),
                )])
                .file_names(["es7".to_owned(), "0.ts".to_owned()])
                .options(
                    CompilerOptionsBuilder::default()
                        .lib(vec!["lib.es5.d.ts".to_owned()])
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_lib_option_with_trailing_white_space() {
        assert_parse_result(
            ["--lib", "es5, ", "es7", "0.ts"],
            ParsedCommandLineBuilder::default()
                .errors([Gc::new(
                    BaseDiagnostic::new(
                        BaseDiagnosticRelatedInformationBuilder::default()
                            .message_text(
                                "Argument for '--lib' option must be: 'es5', 'es6', [...]."
                                    .to_owned(),
                            )
                            .category(Diagnostics::Argument_for_0_option_must_be_Colon_1.category)
                            .code(Diagnostics::Argument_for_0_option_must_be_Colon_1.code)
                            .build()
                            .unwrap(),
                        None,
                    )
                    .into(),
                )])
                .file_names(["es7".to_owned(), "0.ts".to_owned()])
                .options(
                    CompilerOptionsBuilder::default()
                        .lib(vec!["lib.es5.d.ts".to_owned()])
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_multiple_compiler_flags_with_input_files_at_the_end() {
        assert_parse_result(
            [
                "--lib",
                "es5,es2015.symbol.wellknown",
                "--target",
                "es5",
                "0.ts",
            ],
            ParsedCommandLineBuilder::default()
                .file_names(["0.ts".to_owned()])
                .options(
                    CompilerOptionsBuilder::default()
                        .lib(vec![
                            "lib.es5.d.ts".to_owned(),
                            "lib.es2015.symbol.wellknown.d.ts".to_owned(),
                        ])
                        .target(ScriptTarget::ES5)
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_multiple_compiler_flags_with_input_files_in_the_middle() {
        assert_parse_result(
            [
                "--module",
                "commonjs",
                "--target",
                "es5",
                "0.ts",
                "--lib",
                "es5,es2015.symbol.wellknown",
            ],
            ParsedCommandLineBuilder::default()
                .file_names(["0.ts".to_owned()])
                .options(
                    CompilerOptionsBuilder::default()
                        .module(ModuleKind::CommonJS)
                        .target(ScriptTarget::ES5)
                        .lib(vec![
                            "lib.es5.d.ts".to_owned(),
                            "lib.es2015.symbol.wellknown.d.ts".to_owned(),
                        ])
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_multiple_library_compiler_flags() {
        assert_parse_result(
            [
                "--module",
                "commonjs",
                "--target",
                "es5",
                "--lib",
                "es5",
                "0.ts",
                "--lib",
                "es2015.core, es2015.symbol.wellknown ",
            ],
            ParsedCommandLineBuilder::default()
                .file_names(["0.ts".to_owned()])
                .options(
                    CompilerOptionsBuilder::default()
                        .module(ModuleKind::CommonJS)
                        .target(ScriptTarget::ES5)
                        .lib(vec![
                            "lib.es2015.core.d.ts".to_owned(),
                            "lib.es2015.symbol.wellknown.d.ts".to_owned(),
                        ])
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_explicit_boolean_flag_value() {
        assert_parse_result(
            ["--strictNullChecks", "false", "0.ts"],
            ParsedCommandLineBuilder::default()
                .file_names(["0.ts".to_owned()])
                .options(
                    CompilerOptionsBuilder::default()
                        .strict_null_checks(false)
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_non_boolean_argument_after_boolean_flag() {
        assert_parse_result(
            ["--noImplicitAny", "t", "0.ts"],
            ParsedCommandLineBuilder::default()
                .file_names(["t".to_owned(), "0.ts".to_owned()])
                .options(
                    CompilerOptionsBuilder::default()
                        .no_implicit_any(true)
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_implicit_boolean_flag_value() {
        assert_parse_result(
            ["--strictNullChecks"],
            ParsedCommandLineBuilder::default()
                .options(
                    CompilerOptionsBuilder::default()
                        .strict_null_checks(true)
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_incremental() {
        assert_parse_result(
            ["--incremental", "0.ts"],
            ParsedCommandLineBuilder::default()
                .file_names(["0.ts".to_owned()])
                .options(
                    CompilerOptionsBuilder::default()
                        .incremental(true)
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    #[test]
    fn test_parse_ts_build_info_file() {
        assert_parse_result(
            ["--tsBuildInfoFile", "build.tsbuildinfo", "0.ts"],
            ParsedCommandLineBuilder::default()
                .file_names(["0.ts".to_owned()])
                .options(
                    CompilerOptionsBuilder::default()
                        .ts_build_info_file("build.tsbuildinfo")
                        .build()
                        .unwrap(),
                )
                .build()
                .unwrap(),
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }

    mod parses_command_line_null_for_tsconfig_only_option {
        use derive_builder::Builder;
        use typescript_rust::{
            create_option_name_map, format_string_from_args, AlternateModeDiagnostics,
            CommandLineOption, CommandLineOptionBaseBuilder, CommandLineOptionType,
            DiagnosticMessage, DidYouMeanOptionsDiagnostics, GcVec, NonEmpty, OptionsNameMap,
            VecExt,
        };

        use super::*;

        #[derive(Builder)]
        #[builder(setter(strip_option, into))]
        struct VerifyNull {
            pub option_name: String,
            #[builder(default)]
            pub non_null_value: Option<String>,
            #[builder(default)]
            pub worker_diagnostic:
                Option<Rc<dyn Fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>>,
            pub diagnostic_message: &'static DiagnosticMessage,
        }

        fn verify_null_allows_setting_it_to_null(
            option_name: &str,
            worker_diagnostic: Option<impl FnMut() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>,
        ) {
            assert_parse_result(
                [&*format!("--{option_name}"), "null", "0.ts"],
                ParsedCommandLineBuilder::default()
                    .file_names(["0.ts".to_owned()])
                    .options(CompilerOptionsBuilder::default().build().unwrap())
                    .build()
                    .unwrap(),
                worker_diagnostic,
            );
        }

        fn verify_null_errors_if_non_null_value_is_passed(
            option_name: &str,
            non_null_value: &str,
            diagnostic_message: &DiagnosticMessage,
            worker_diagnostic: Option<impl FnMut() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>,
        ) {
            assert_parse_result(
                [&*format!("--{option_name}"), non_null_value, "0.ts"],
                ParsedCommandLineBuilder::default()
                    .errors([Gc::new(
                        BaseDiagnostic::new(
                            BaseDiagnosticRelatedInformationBuilder::default()
                                .message_text(format_string_from_args(
                                    &diagnostic_message.message,
                                    vec![option_name.to_owned()],
                                ))
                                .category(diagnostic_message.category)
                                .code(diagnostic_message.code)
                                .build()
                                .unwrap(),
                            None,
                        )
                        .into(),
                    )])
                    .file_names(["0.ts".to_owned()])
                    .options(CompilerOptionsBuilder::default().build().unwrap())
                    .build()
                    .unwrap(),
                worker_diagnostic,
            );
        }

        fn verify_null_errors_if_its_followed_by_another_option(
            option_name: &str,
            diagnostic_message: &DiagnosticMessage,
            worker_diagnostic: Option<impl FnMut() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>,
        ) {
            assert_parse_result(
                ["0.ts", "--strictNullChecks", &*format!("--{option_name}")],
                ParsedCommandLineBuilder::default()
                    .errors([Gc::new(
                        BaseDiagnostic::new(
                            BaseDiagnosticRelatedInformationBuilder::default()
                                .message_text(format_string_from_args(
                                    &diagnostic_message.message,
                                    vec![option_name.to_owned()],
                                ))
                                .category(diagnostic_message.category)
                                .code(diagnostic_message.code)
                                .build()
                                .unwrap(),
                            None,
                        )
                        .into(),
                    )])
                    .file_names(["0.ts".to_owned()])
                    .options(
                        CompilerOptionsBuilder::default()
                            .strict_null_checks(true)
                            .build()
                            .unwrap(),
                    )
                    .build()
                    .unwrap(),
                worker_diagnostic,
            );
        }

        fn verify_null_errors_if_its_last_option(
            option_name: &str,
            diagnostic_message: &DiagnosticMessage,
            worker_diagnostic: Option<impl FnMut() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>,
        ) {
            assert_parse_result(
                ["0.ts", &*format!("--{option_name}")],
                ParsedCommandLineBuilder::default()
                    .errors([Gc::new(
                        BaseDiagnostic::new(
                            BaseDiagnosticRelatedInformationBuilder::default()
                                .message_text(format_string_from_args(
                                    &diagnostic_message.message,
                                    vec![option_name.to_owned()],
                                ))
                                .category(diagnostic_message.category)
                                .code(diagnostic_message.code)
                                .build()
                                .unwrap(),
                            None,
                        )
                        .into(),
                    )])
                    .file_names(["0.ts".to_owned()])
                    .options(CompilerOptionsBuilder::default().build().unwrap())
                    .build()
                    .unwrap(),
                worker_diagnostic,
            );
        }

        fn verify_null(
            VerifyNull {
                option_name,
                non_null_value,
                worker_diagnostic,
                diagnostic_message,
            }: VerifyNull,
        ) {
            verify_null_allows_setting_it_to_null(
                &option_name,
                worker_diagnostic
                    .as_ref()
                    .map(|worker_diagnostic| || worker_diagnostic()),
            );

            if let Some(non_null_value) = non_null_value.as_ref().non_empty() {
                verify_null_errors_if_non_null_value_is_passed(
                    &option_name,
                    non_null_value,
                    diagnostic_message,
                    worker_diagnostic
                        .as_ref()
                        .map(|worker_diagnostic| || worker_diagnostic()),
                );
            }

            verify_null_errors_if_its_followed_by_another_option(
                &option_name,
                diagnostic_message,
                worker_diagnostic
                    .as_ref()
                    .map(|worker_diagnostic| || worker_diagnostic()),
            );

            verify_null_errors_if_its_last_option(
                &option_name,
                diagnostic_message,
                worker_diagnostic
                    .as_ref()
                    .map(|worker_diagnostic| || worker_diagnostic()),
            );
        }

        struct VerifyNullNonIncludedOptionParseCommandLineWorkerDiagnostics {
            option_declarations: GcVec<Gc<CommandLineOption>>,
            compiler_options_did_you_mean_diagnostics: Rc<dyn ParseCommandLineWorkerDiagnostics>,
        }

        impl ParseCommandLineWorkerDiagnostics
            for VerifyNullNonIncludedOptionParseCommandLineWorkerDiagnostics
        {
            fn get_options_name_map(&self) -> Rc<OptionsNameMap> {
                Rc::new(create_option_name_map(&self.option_declarations))
            }

            fn option_type_mismatch_diagnostic(&self) -> &DiagnosticMessage {
                self.compiler_options_did_you_mean_diagnostics
                    .option_type_mismatch_diagnostic()
            }

            fn as_did_you_mean_options_diagnostics(&self) -> &dyn DidYouMeanOptionsDiagnostics {
                self
            }
        }

        impl DidYouMeanOptionsDiagnostics for VerifyNullNonIncludedOptionParseCommandLineWorkerDiagnostics {
            fn option_declarations(&self) -> GcVec<Gc<CommandLineOption>> {
                self.option_declarations.clone()
            }

            fn maybe_alternate_mode(&self) -> Option<Rc<AlternateModeDiagnostics>> {
                self.compiler_options_did_you_mean_diagnostics
                    .maybe_alternate_mode()
            }

            fn unknown_option_diagnostic(&self) -> &DiagnosticMessage {
                self.compiler_options_did_you_mean_diagnostics
                    .unknown_option_diagnostic()
            }

            fn unknown_did_you_mean_diagnostic(&self) -> &DiagnosticMessage {
                self.compiler_options_did_you_mean_diagnostics
                    .unknown_did_you_mean_diagnostic()
            }
        }

        struct VerifyNullNonIncludedOption {
            pub type_: CommandLineOptionType,
            pub non_null_value: Option<String>,
        }

        fn verify_null_non_included_option(
            VerifyNullNonIncludedOption {
                type_,
                non_null_value,
            }: VerifyNullNonIncludedOption,
        ) {
            verify_null(
                VerifyNull {
                    option_name: "optionName".to_owned(),
                    non_null_value,
                    diagnostic_message: &Diagnostics::Option_0_can_only_be_specified_in_tsconfig_json_file_or_set_to_null_on_command_line,
                    worker_diagnostic: Some(Rc::new(move || {
                        let option_declarations: GcVec<Gc<CommandLineOption>> =
                            compiler_options_did_you_mean_diagnostics()
                                .option_declarations()
                                .to_vec()
                                .and_push(
                                    CommandLineOptionBaseBuilder::default()
                                        .name("optionName")
                                        .type_(type_.clone())
                                        .is_tsconfig_only(true)
                                        .category(&*Diagnostics::Backwards_Compatibility)
                                        .description(&*Diagnostics::Enable_project_compilation)
                                        .default_value_description("undefined".to_owned())
                                        .build().unwrap().try_into().unwrap()
                                ).into();
                        // TODO: this looks like it should be getting `Gc`-wrapped rather than
                        // `Rc`-wrapped?
                        Rc::new(VerifyNullNonIncludedOptionParseCommandLineWorkerDiagnostics {
                            option_declarations,
                            compiler_options_did_you_mean_diagnostics: compiler_options_did_you_mean_diagnostics()
                        })
                    })),
                }
            );
        }

        mod option_of_type_boolean {
            use super::*;

            #[test]
            fn test_allows_setting_it_to_false() {
                assert_parse_result(
                    ["--composite", "false", "0.ts"],
                    ParsedCommandLineBuilder::default()
                        .file_names(["0.ts".to_owned()])
                        .options(
                            CompilerOptionsBuilder::default()
                                .composite(false)
                                .build()
                                .unwrap(),
                        )
                        .build()
                        .unwrap(),
                    Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
                );
            }

            #[test]
            fn test_verify_null() {
                verify_null(
                    VerifyNullBuilder::default()
                        .option_name("composite")
                        .non_null_value("true")
                        .diagnostic_message(&*Diagnostics::Option_0_can_only_be_specified_in_tsconfig_json_file_or_set_to_false_or_null_on_command_line)
                        .build().unwrap()
                );
            }
        }
    }
}
