#!/usr/bin/env rust-script

use std::env;

fn main() {
    let args = env::args();
    if args.len() < 2 {
        eprintln!("Usage:");
        eprintln!("\t./scripts/process_diagnostic_messages.rs <diagnostic-json-input-file>");
        return;
    }
}
