use lazy_static::lazy_static;
use regex::Regex;
use rstest::rstest;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::rc::Rc;

use typescript_rust::{
    create_compiler_host_worker, create_program, format_diagnostics, get_pre_emit_diagnostics,
    get_sys, option_declarations, CommandLineOption, CommandLineOptionInterface,
    CommandLineOptionType, CompilerOptions, CompilerOptionsBuilder, CompilerOptionsValue,
    CreateProgramOptions, Diagnostic, FormatDiagnosticsHost, Node,
};

#[rstest]
#[case("yieldStringLiteral.ts")]
fn run_compiler_baseline(#[case] case_filename: &str) {
    let case_file_path = format!("typescript_src/tests/cases/compiler/{case_filename}");
    let case_file_contents = fs::read_to_string(&case_file_path).unwrap();
    let compiler_settings = extract_compiler_settings(&case_file_contents);
    let mut options: CompilerOptions = CompilerOptionsBuilder::default()
        .no_resolve(Some(false))
        .build()
        .unwrap();
    set_compiler_options_from_harness_settings(&compiler_settings, &mut options);
    let options = Rc::new(options);
    let host = create_compiler_host_worker(options.clone(), None, Some(get_sys()));
    let program = create_program(CreateProgramOptions {
        root_names: vec![case_file_path],
        options,
        host: Some(Rc::new(host)),
        project_references: None,
        old_program: None,
        config_file_parsing_diagnostics: None,
    });
    // let emit_result = program.emit(None, None, None, None, None, None);
    let errors = get_pre_emit_diagnostics(&program.clone().into(), Option::<&Node>::None, None);
    compare_baselines(
        Path::new(case_filename)
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap(),
        &errors,
    )
}

fn set_compiler_options_from_harness_settings(
    settings: &HashMap<&str, &str>,
    options: &mut CompilerOptions,
) {
    for (&name, &value) in settings {
        let option = get_command_line_option(name);
        if let Some(option) = option.as_ref() {
            let mut errors: Vec<Rc<Diagnostic>> = vec![];
            options.set_value_from_command_line_option(
                option,
                option_value(option, value, &mut errors),
            );
            if !errors.is_empty() {
                panic!("Unknown value '{value}' for compiler option '{name}'.")
            }
        } else {
            panic!("Unknown compiler option '{name}'.")
        }
    }
}

fn option_value(
    option: &CommandLineOption,
    value: &str,
    errors: &mut Vec<Rc<Diagnostic>>,
) -> CompilerOptionsValue {
    match option.type_() {
        CommandLineOptionType::Boolean => Some(value.to_lowercase() == "true").into(),
        CommandLineOptionType::String => Some(value.to_owned()).into(),
        CommandLineOptionType::Number => unimplemented!(),
        CommandLineOptionType::Object => unimplemented!(),
        CommandLineOptionType::List => unimplemented!(),
        CommandLineOptionType::Map(_) => unimplemented!(),
    }
}

thread_local! {
    static options_index: RefCell<Option<HashMap<String, Rc<CommandLineOption>>>> = RefCell::new(None);
}
fn get_command_line_option(name: &str) -> Option<Rc<CommandLineOption>> {
    options_index.with(|options_index_| {
        options_index_
            .borrow_mut()
            .get_or_insert_with(|| {
                let mut options_index_ = HashMap::new();
                option_declarations.with(|option_declarations_| {
                    for option in option_declarations_ {
                        options_index_.insert(option.name().to_lowercase(), option.clone());
                    }
                });
                options_index_
            })
            .get(name)
            .cloned()
    })
}

lazy_static! {
    static ref option_regex: Regex = Regex::new(r"(?m)^[/]{2}\s*@(w+)\s*:\s*([^\r\n]*)").unwrap();
}

fn extract_compiler_settings(case_file_contents: &str) -> HashMap<&str, &str> {
    let mut opts: HashMap<&str, &str> = HashMap::new();

    for match_ in option_regex.captures_iter(case_file_contents) {
        opts.insert(
            match_.get(1).unwrap().as_str(),
            match_.get(2).unwrap().as_str(),
        );
    }

    opts
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
