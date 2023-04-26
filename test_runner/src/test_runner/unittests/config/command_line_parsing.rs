#![cfg(test)]

use speculoos::prelude::*;

mod parse_command_line {
    use std::{io, rc::Rc};

    use super::*;
    use gc::Gc;
    use itertools::Itertools;
    use typescript_rust::{
        compiler_options_did_you_mean_diagnostics, parse_command_line_worker,
        CompilerOptionsBuilder, ParseCommandLineWorkerDiagnostics, ParsedCommandLine,
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
    }

    #[test]
    fn test_parse_single_option_of_library_flag() {
        assert_parse_result(
            ["--lib", "es6", "0.ts"],
            ParsedCommandLine {
                errors: Default::default(),
                file_names: vec!["0.ts".to_owned()],
                options: Gc::new(
                    CompilerOptionsBuilder::default()
                        .lib(Some(vec!["lib.es2015.d.ts".to_owned()]))
                        .build()
                        .unwrap(),
                ),
                type_acquisition: Default::default(),
                project_references: Default::default(),
                watch_options: Default::default(),
                raw: Default::default(),
                wildcard_directories: Default::default(),
                compile_on_save: Default::default(),
            },
            Option::<fn() -> Rc<dyn ParseCommandLineWorkerDiagnostics>>::None,
        );
    }
}
