use std::fs;
use std::rc::Rc;

use typescript_rust::{
    create_compiler_host_worker, create_program, format_diagnostics, get_pre_emit_diagnostics,
    get_sys, CompilerOptions, CreateProgramOptions, Diagnostic, FormatDiagnosticsHost, Node,
};

#[test]
fn run_compiler_baselines() {
    let options: Rc<CompilerOptions> = Rc::new(Default::default());
    let host = create_compiler_host_worker(options.clone(), None, Some(get_sys()));
    let program = create_program(CreateProgramOptions {
        root_names: vec!["typescript_src/tests/cases/compiler/yieldStringLiteral.ts".to_owned()],
        options,
        host: Some(Rc::new(host)),
        project_references: None,
        old_program: None,
        config_file_parsing_diagnostics: None,
    });
    // let emit_result = program.emit(None, None, None, None, None, None);
    let errors = get_pre_emit_diagnostics(&program.clone().into(), Option::<&Node>::None, None);
    compare_baselines("yieldStringLiteral", &errors)
}

fn compare_baselines(name: &str, diagnostics: &[Rc<Diagnostic>]) {
    let ref baseline_file_contents = fs::read_to_string(&format!(
        "typescript_src/tests/baselines/reference/{name}.errors.txt"
    ))
    .unwrap();
    let baseline_error_lines = parse_baseline_errors(baseline_file_contents);
    let formatted_diagnostic_lines = format_diagnostics(diagnostics, &DummyFormatDiagnosticsHost);
    assert_eq!(baseline_error_lines, formatted_diagnostic_lines,);
}

struct DummyFormatDiagnosticsHost;

impl FormatDiagnosticsHost for DummyFormatDiagnosticsHost {
    fn get_current_directory(&self) -> String {
        "".to_owned()
    }

    fn get_new_line(&self) -> &str {
        "\n"
    }

    fn get_canonical_file_name(&self, file_name: &str) -> String {
        file_name.to_owned()
    }
}

fn parse_baseline_errors(baseline_file_contents: &str) -> String {
    baseline_file_contents
        .split("\n")
        .filter(|line| line.starts_with("tests/cases/compiler"))
        .map(|line| {
            line.replace(
                "tests/cases/compiler",
                "typescript_src/tests/cases/compiler",
            )
            .replace("\r", "\n")
        })
        .collect::<Vec<_>>()
        .join("")
}
