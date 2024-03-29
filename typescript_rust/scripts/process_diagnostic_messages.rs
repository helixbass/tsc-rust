#!/usr/bin/env rust-script
//!
//! ```cargo
//! [dependencies]
//! serde = { version = "1.0", features = ["derive"] }
//! serde_json = "1.0"
//! pathdiff = "0.2.1"
//! regex = "1.5.4"
//! ```

use regex::Regex;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Clone, Copy, Debug, Deserialize)]
enum DiagnosticCategory {
    Error,
    Message,
    Suggestion,
}

#[derive(Clone, Copy, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct DiagnosticMessageSpec {
    category: DiagnosticCategory,
    code: u32,
    elided_in_compatability_pyramid: Option<bool>,
    // TODO: also need to parse reportsUnnecessary and reportsDeprecated?
}

fn main() {
    let args = env::args().collect::<Vec<_>>();
    if args.len() < 2 {
        println!("Usage:");
        println!("\t./scripts/process_diagnostic_messages.rs <diagnostic-json-input-file>");
        return;
    }
    let input_file_path = Path::new(&args[1])/*.replace(/\\/g, "/")*/;
    println!("Reading diagnostics from {}", input_file_path.display());
    let input_str = fs::read_to_string(input_file_path).expect("Couldn't read input file");
    let diagnostic_messages_json: HashMap<String, DiagnosticMessageSpec> =
        serde_json::from_str(&input_str).expect("Couldn't parse input file as JSON");
    let diagnostic_messages = diagnostic_messages_json;
    let output_files_dir = input_file_path
        .parent()
        .expect("Couldn't get input file path parent");
    let this_file_path_rel = pathdiff::diff_paths(
        fs::canonicalize(output_files_dir).expect("Couldn't get absolute path of output directory"),
        env::current_dir().expect("Couldn't get current directory"),
    )
    .expect("Couldn't get relative path from current directory to output directory");
    let info_file_output = build_info_file_output(
        &diagnostic_messages,
        "./diagnostic_information_map_generated.rs",
        this_file_path_rel,
    );
    check_for_unique_codes(&diagnostic_messages);
    write_file(
        &input_file_path,
        "diagnostic_information_map_generated.rs",
        info_file_output,
    );
}

fn write_file(input_file_path: &Path, file_name: &str, contents: String) {
    fs::write(
        input_file_path
            .parent()
            .expect("Couldn't get input file path parent")
            .join(file_name),
        contents,
    )
    .expect("Couldn't write file");
}

fn check_for_unique_codes(diagnostic_table: &HashMap<String, DiagnosticMessageSpec>) {
    let mut all_codes: HashSet<u32> = HashSet::new();
    for (_, DiagnosticMessageSpec { code, .. }) in diagnostic_table {
        if all_codes.contains(&code) {
            panic!("Diagnostic code {} appears more than once.", code);
        }
        all_codes.insert(*code);
    }
}

fn build_info_file_output(
    message_table: &HashMap<String, DiagnosticMessageSpec>,
    input_file_path_rel: &str,
    this_file_path_rel: PathBuf,
) -> String {
    let mut result = format!(
        "// <auto-generated>
// generated from '{}' by '{}'

pub mod Diagnostics {{
    use crate::{{DiagnosticCategory, DiagnosticMessage}};

    fn diag(
        code: u32,
        category: DiagnosticCategory,
        key: &'static str,
        message: &'static str,
        elided_in_compatability_pyramid: Option<bool>,
    ) -> DiagnosticMessage {{
        DiagnosticMessage::new(
            code,
            category,
            key,
            message.into(),
            elided_in_compatability_pyramid,
        )
    }}

",
        input_file_path_rel,
        this_file_path_rel
            .to_str()
            .expect("Couldn't treat relative path as string") /*.replace(/\\/g, "/")*/
    );
    let mut entries: Vec<(String, DiagnosticMessageSpec)> = message_table
        .iter()
        .map(|(key, value)| (key.to_string(), *value))
        .collect();
    entries.sort_by(|a, b| a.1.code.cmp(&b.1.code));
    for (
        name,
        DiagnosticMessageSpec {
            code,
            category,
            elided_in_compatability_pyramid,
        },
    ) in entries.into_iter()
    {
        let prop_name = convert_property_name(&name);
        result.push_str(&format!(
            "    lazy_static! {{
        pub static ref {}: DiagnosticMessage = diag(
            {},
            DiagnosticCategory::{:?},
            {:?},
            {:?},
            {:?},
        );
    }}
",
            prop_name,
            code,
            category,
            create_key(&prop_name, code),
            name,
            elided_in_compatability_pyramid,
        ));
    }
    result.push_str(
        "}
",
    );
    result
}

fn create_key(name: &str, code: u32) -> String {
    let mut key: String = name.chars().take(100).collect();
    key.push_str("_");
    key.push_str(&code.to_string());
    key
}
fn convert_property_name(orig_name: &str) -> String {
    let word_character_regex = Regex::new(r"\w").unwrap();
    let result = orig_name
        .chars()
        .map(|char| match char {
            '*' => "_Asterisk".to_string(),
            '/' => "_Slash".to_string(),
            ':' => "_Colon".to_string(),
            _ => {
                let char_as_string = char.to_string();
                if word_character_regex.is_match(&char_as_string) {
                    char_as_string
                } else {
                    "_".to_string()
                }
            }
        })
        .collect::<Vec<String>>()
        .join("");
    let multi_underscore_regex = Regex::new(r"_+").unwrap();
    let result = multi_underscore_regex.replace_all(&result, "_");
    let leading_underscore_not_followed_by_number_regex = Regex::new(r"^_([^\d])").unwrap();
    let result = leading_underscore_not_followed_by_number_regex.replace(&result, "$1");
    let trailing_underscore_regex = Regex::new(r"_$").unwrap();
    let result = trailing_underscore_regex.replace(&result, "");
    result.to_string()
}
