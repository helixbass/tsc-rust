use lazy_static::lazy_static;
use regex::{Captures, Regex};
use rstest::rstest;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::rc::Rc;

use typescript_rust::{
    create_compiler_host_worker, create_program, format_diagnostics, get_pre_emit_diagnostics,
    get_sys, option_declarations, parse_custom_type_option, parse_list_type_option,
    CommandLineOption, CommandLineOptionInterface, CommandLineOptionType, CompilerOptions,
    CompilerOptionsBuilder, CompilerOptionsValue, CreateProgramOptions, Diagnostic,
    FormatDiagnosticsHost, Node,
};

#[rstest]
#[case("unusedTypeParameterInLambda1.ts")]
#[case("unusedTypeParameterInLambda2.ts")]
#[case("unusedTypeParameterInLambda3.ts")]
#[case("unusedTypeParameterInMethod1.ts")]
#[case("unusedTypeParameterInMethod2.ts")]
#[case("unusedTypeParameterInMethod3.ts")]
#[case("unusedTypeParameterInMethod4.ts")]
#[case("unusedTypeParameterInMethod5.ts")]
#[case("unusedTypeParameters1.ts")]
#[case("unusedTypeParameters10.ts")]
#[case("unusedTypeParameters2.ts")]
#[case("unusedTypeParameters3.ts")]
#[case("unusedTypeParameters4.ts")]
#[case("unusedTypeParameters5.ts")]
// #[case("unusedTypeParameters8.ts")] uses @fileName/multiple files
#[case("unusedTypeParametersCheckedByNoUnusedParameters.ts")]
#[case("unusedTypeParametersWithUnderscore.ts")]
#[case("unusedTypeParameters_infer.ts")]
// #[case("unusedTypeParameters_templateTag.ts")] these two use @Filename but not multiple files?
// #[case("unusedTypeParameters_templateTag2.ts")]
#[case("unusedVariablesWithUnderscoreInBindingElement.ts")]
#[case("unusedVariablesWithUnderscoreInForOfLoop.ts")]
#[case("unusedVariablesinBlocks1.ts")]
#[case("unusedVariablesinBlocks2.ts")]
#[case("unusedVariablesinForLoop.ts")]
#[case("unusedVariablesinForLoop2.ts")]
#[case("unusedVariablesinForLoop3.ts")]
#[case("unusedVariablesinForLoop4.ts")]
#[case("unusedVariablesinModules1.ts")]
#[case("unusedVariablesinNamespaces1.ts")]
#[case("unusedVariablesinNamespaces2.ts")]
#[case("unusedVariablesinNamespaces3.ts")]
#[case("useBeforeDeclaration_destructuring.ts")]
#[case("useBeforeDeclaration_jsx.tsx")]
#[case("useBeforeDeclaration_propertyAssignment.ts")]
#[case("useBeforeDeclaration_superClass.ts")]
#[case("useUnknownInCatchVariables01.ts")]
#[case("validRegexp.ts")]
#[case("varAndFunctionShareName.ts")]
#[case("varArgConstructorMemberParameter.ts")]
#[case("varArgWithNoParamName.ts")]
#[case("varBlock.ts")]
#[case("varNameConflictsWithImportInDifferentPartOfModule.ts")]
#[case("vararg.ts")]
#[case("variableDeclarationInStrictMode1.ts")]
#[case("variableDeclaratorResolvedDuringContextualTyping.ts")]
// #[case("varianceMeasurement.ts")] // looks like it's finding the correct # of errors but not showing exactly the same diagnostics for some of them?
#[case("voidArrayLit.ts")]
// #[case("voidAsNonAmbiguousReturnType.ts")] // has more than one source file "inline"
// #[case("weakType.ts")] // failing on sometimes flipping `Spoiler & Weak` vs `Weak & Spoiler`
#[case("widenToAny1.ts")]
#[case("widenToAny2.ts")]
#[case("widenedTypes.ts")]
#[case("withStatement.ts")]
#[case("withStatementErrors.ts")]
#[case("withStatementNestedScope.ts")]
#[case("wrappedRecursiveGenericType.ts")]
#[case("yieldExpression1.ts")]
#[case("yieldExpressionInFlowLoop.ts")]
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
        &case_file_contents,
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
                panic!("Unknown value '{}' for compiler option '{}'.", value, name);
            }
        } else {
            panic!("Unknown compiler option '{}'.", name);
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
        CommandLineOptionType::List => {
            CompilerOptionsValue::VecString(parse_list_type_option(option, Some(value), errors))
        }
        CommandLineOptionType::Map(_) => parse_custom_type_option(option, Some(value), errors),
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
            .get(&name.to_lowercase())
            .cloned()
    })
}

lazy_static! {
    static ref option_regex: Regex = Regex::new(r"(?m)^[/]{2}\s*@(\w+)\s*:\s*([^\r\n]*)").unwrap();
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

fn compare_baselines(name: &str, diagnostics: &[Rc<Diagnostic>], case_file_contents: &str) {
    let ref baseline_file_contents = fs::read_to_string(&format!(
        "typescript_src/tests/baselines/reference/{name}.errors.txt"
    ))
    .unwrap();
    let baseline_error_lines = parse_baseline_errors(baseline_file_contents);
    let formatted_diagnostic_lines = adjust_diagnostic_line_numbers_and_lib_file_paths(
        &format_diagnostics(diagnostics, &DummyFormatDiagnosticsHost),
        case_file_contents,
    );
    assert_eq!(baseline_error_lines, formatted_diagnostic_lines,);
}

fn adjust_diagnostic_line_numbers_and_lib_file_paths(
    formatted_diagnostics: &str,
    case_file_contents: &str,
) -> String {
    let number_of_leading_lines_to_remove =
        get_number_of_leading_lines_to_remove(case_file_contents);
    lazy_static! {
        static ref formatted_diagnostic_regex: Regex =
            Regex::new(r"^(typescript_src/tests/cases/compiler/[^(]+\()(\d+)(,\d+\))").unwrap();
        static ref lib_path_regex: Regex =
            Regex::new(r"^[.\w/]*TypeScript/built/local/(.+)$").unwrap();
    }
    formatted_diagnostics
        .split("\n")
        .map(|line| {
            formatted_diagnostic_regex.replace(line, |captures: &Captures| {
                format!(
                    "{}{}{}",
                    &captures[1],
                    (captures.get(2).unwrap().as_str().parse::<usize>().unwrap()
                        - number_of_leading_lines_to_remove),
                    &captures[3],
                )
            })
        })
        .map(|line| {
            lib_path_regex
                .replace(&line, |captures: &Captures| format!("{}", &captures[1],))
                .into_owned()
        })
        .collect::<Vec<_>>()
        .join("\n")
}

lazy_static! {
    static ref whitespace_only_regex: Regex = Regex::new(r"^\s*$").unwrap();
}

fn get_number_of_leading_lines_to_remove(case_file_contents: &str) -> usize {
    case_file_contents
        .split("\n")
        .take_while(|&line| option_regex.is_match(line) || whitespace_only_regex.is_match(line))
        .count()
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
        // .filter(|line| line.starts_with("tests/cases/compiler"))
        .take_while(|line| {
            lazy_static! {
                static ref blank_line_regex: Regex = Regex::new(r"^\s*$").unwrap();
            }
            !blank_line_regex.is_match(line)
        })
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
