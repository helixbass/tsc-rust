#![cfg(test)]

use std::io;

use harness::Baseline;
use typescript_rust::{generate_tsconfig, parse_command_line, regex, Owned};

fn init_ts_config_correctly(name: &str, command_lines_args: &[String]) {
    let command_line = parse_command_line(
        command_lines_args,
        Option::<fn(&str) -> io::Result<Option<String>>>::None,
    );
    let init_result = generate_tsconfig(&command_line.options, &command_line.file_names, "\n");
    let output_file_name = format!(
        "tsConfig/{}/tsconfig.json",
        regex!(r#"(?i)[^a-z0-9\-. ]"#).replace_all(name, "")
    );

    Baseline::run_baseline(&output_file_name, Some(&init_result), None)
}

#[test]
fn test_default_initialized_ts_config() {
    init_ts_config_correctly("Default initialized TSConfig", &["--init"].owned());
}

#[test]
fn test_initialized_ts_config_with_files_options() {
    init_ts_config_correctly(
        "Initialized TSConfig with files options",
        &["--init", "file0.st", "file1.ts", "file2.ts"].owned(),
    );
}

#[test]
fn test_initialized_ts_config_with_boolean_value_compiler_options() {
    init_ts_config_correctly(
        "Initialized TSConfig with boolean value compiler options",
        &["--init", "--noUnusedLocals"].owned(),
    );
}

#[test]
fn test_initialized_ts_config_with_enum_value_compiler_options() {
    init_ts_config_correctly(
        "Initialized TSConfig with enum value compiler options",
        &["--init", "--target", "es5", "--jsx", "react"].owned(),
    );
}

#[test]
fn test_initialized_ts_config_with_list_compiler_options() {
    init_ts_config_correctly(
        "Initialized TSConfig with list compiler options",
        &["--init", "--types", "jquery,mocha"].owned(),
    );
}

#[test]
fn test_initialized_ts_config_with_list_compiler_options_with_enum_value() {
    init_ts_config_correctly(
        "Initialized TSConfig with list compiler options with enum value",
        &["--init", "--lib", "es5,es2015.core"].owned(),
    );
}

#[test]
fn test_initialized_ts_config_with_incorrect_compiler_option() {
    init_ts_config_correctly(
        "Initialized TSConfig with incorrect compiler option",
        &["--init", "--someNonExistOption"].owned(),
    );
}

#[test]
fn test_initialized_ts_config_with_incorrect_compiler_option_value() {
    init_ts_config_correctly(
        "Initialized TSConfig with incorrect compiler option value",
        &["--init", "--lib", "nonExistLib,es5,es2015.promise"].owned(),
    );
}

#[test]
fn test_initialized_ts_config_with_advanced_options() {
    init_ts_config_correctly(
        "Initialized TSConfig with advanced options",
        &[
            "--init",
            "--declaration",
            "--declarationDir",
            "lib",
            "--skipLibCheck",
            "--noErrorTruncation",
        ]
        .owned(),
    );
}
