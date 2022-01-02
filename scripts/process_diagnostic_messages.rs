#!/usr/bin/env rust-script
//!
//! ```cargo
//! [dependencies]
//! serde = { version = "1.0", features = ["derive"] }
//! serde_json = "1.0"
//! ```

use serde::Deserialize;
use std::collections::HashMap;
use std::env;
use std::fs;

#[derive(Debug, Deserialize)]
enum DiagnosticCategory {
    Error,
    Message,
    Suggestion,
}

#[derive(Debug, Deserialize)]
struct DiagnosticMessageSpec {
    category: DiagnosticCategory,
    code: u32,
}

fn main() {
    let args = env::args().collect::<Vec<_>>();
    if args.len() < 2 {
        println!("Usage:");
        println!("\t./scripts/process_diagnostic_messages.rs <diagnostic-json-input-file>");
        return;
    }
    let input_file_path = &args[1]/*.replace(/\\/g, "/")*/;
    println!("Reading diagnostics from {}", input_file_path);
    let input_str = fs::read_to_string(input_file_path).expect("Couldn't read input file");
    let diagnostic_messages_json: HashMap<String, DiagnosticMessageSpec> =
        serde_json::from_str(&input_str).expect("Couldn't parse input file as JSON");
}

// fn write_file(input_file_path: )
