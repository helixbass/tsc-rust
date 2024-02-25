use std::borrow::Cow;

use itertools::Itertools;
use regex::{Captures, Regex, SubCaptureMatches};
use typescript_rust::{
    format_string_from_args, id_arena::Id, per_arena, reg_exp_escape, regex, DiagnosticMessage,
    Diagnostics, Owned,
};

use crate::{impl_has_arena_harness, AllArenasHarness, HasArenaHarness, InArenaHarness};

fn test_path_prefix_reg_exp() -> &'static Regex {
    regex!(r"(?:(file:/{3})|/)\.(ts|lib|src)/")
}

pub fn remove_test_path_prefixes(
    text: &str,
    retain_trailing_directory_separator: Option<bool>,
) -> Cow<'_, str> {
    test_path_prefix_reg_exp().replace_all(text, |captures: &Captures| {
        captures
            .get(1)
            .filter(|capture| !capture.as_str().is_empty())
            .map_or_else(
                || {
                    if retain_trailing_directory_separator == Some(true) {
                        "/"
                    } else {
                        ""
                    }
                    .to_owned()
                },
                |capture| capture.as_str().to_owned(),
            )
    })
}

pub fn maybe_remove_test_path_prefixes(
    text: Option<&str>,
    retain_trailing_directory_separator: Option<bool>,
) -> Option<Cow<'_, str>> {
    text.map(|text| remove_test_path_prefixes(text, retain_trailing_directory_separator))
}

fn create_diagnostic_message_replacer(
    diagnostic_message: &'static DiagnosticMessage,
    replacer: Id<Box<dyn Replacer>>,
    arena: &impl HasArenaHarness,
) -> DiagnosticMessageReplacer {
    let message_parts = regex!(r#"\{\d+\}"#).split(&diagnostic_message.message);
    let reg_exp = Regex::new(&format!(
        "^(?:{})$",
        message_parts
            .map(reg_exp_escape)
            .collect_vec()
            .join("(.*?)")
    ))
    .unwrap();
    DiagnosticMessageReplacer::new(reg_exp, replacer, diagnostic_message, arena)
}

pub struct DiagnosticMessageReplacer {
    arena: *const AllArenasHarness,
    reg_exp: Regex,
    replacer: Id<Box<dyn Replacer>>,
    diagnostic_message: &'static DiagnosticMessage,
}

impl DiagnosticMessageReplacer {
    fn new(
        reg_exp: Regex,
        replacer: Id<Box<dyn Replacer>>,
        diagnostic_message: &'static DiagnosticMessage,
        arena: &impl HasArenaHarness,
    ) -> Self {
        Self {
            arena: arena.arena_harness(),
            reg_exp,
            replacer,
            diagnostic_message,
        }
    }
}

impl DiagnosticMessageReplacer {
    pub fn call<'text>(&self, text: &'text str, args: Vec<String>) -> Cow<'text, str> {
        self.reg_exp.replace(text, |captures: &Captures| {
            format_string_from_args(
                &self.diagnostic_message.message,
                self.replacer.ref_(self).call(captures.iter(), args.clone()),
            )
        })
    }
}

impl_has_arena_harness!(DiagnosticMessageReplacer);

pub trait Replacer {
    fn call(&self, message_args: SubCaptureMatches<'_, '_>, args: Vec<String>) -> Vec<String>;
}

fn replace_types_version_message(arena: &impl HasArenaHarness) -> Id<DiagnosticMessageReplacer> {
    per_arena!(
        DiagnosticMessageReplacer,
        arena,
        arena.alloc_diagnostic_message_replacer(
            create_diagnostic_message_replacer(
                &Diagnostics::package_json_has_a_typesVersions_entry_0_that_matches_compiler_version_1_looking_for_a_pattern_to_match_module_name_2,
                ReplaceTypesVersionMessageReplacer::new(arena),
                arena,
            )
        )
    )
}

struct ReplaceTypesVersionMessageReplacer;

impl ReplaceTypesVersionMessageReplacer {
    pub fn new(arena: &impl HasArenaHarness) -> Id<Box<dyn Replacer>> {
        arena.alloc_replacer(Box::new(Self))
    }
}

impl Replacer for ReplaceTypesVersionMessageReplacer {
    fn call(&self, mut message_args: SubCaptureMatches<'_, '_>, args: Vec<String>) -> Vec<String> {
        let entry = message_args.next().unwrap().unwrap();
        message_args.next();
        let module_name = message_args.next().unwrap().unwrap();
        let compiler_version = args.into_iter().next().unwrap();
        vec![
            entry.as_str().to_owned(),
            compiler_version,
            module_name.as_str().to_owned(),
        ]
    }
}

pub fn sanitize_trace_resolution_log_entry(text: &str, arena: &impl HasArenaHarness) -> String {
    if !text.is_empty() {
        remove_test_path_prefixes(
            &replace_types_version_message(arena)
                .ref_(arena)
                .call(text, ["3.1.0-dev"].owned()),
            None,
        )
        .into_owned()
    } else {
        text.to_owned()
    }
}

pub fn get_byte_order_mark_length(text: &str) -> usize {
    if !text.is_empty() {
        let mut text_chars = text.chars();
        let ch0 = text_chars.next().unwrap();
        if ch0 == '\u{feff}' {
            return 1;
        }
        if ch0 == '\u{fe}' {
            return if text_chars.next() == Some('\u{ff}') {
                2
            } else {
                0
            };
        }
        if ch0 == '\u{ff}' {
            return if text_chars.next() == Some('\u{fe}') {
                2
            } else {
                0
            };
        }
        if ch0 == '\u{ef}' {
            return if text_chars.next() == Some('\u{bb}') && text_chars.next() == Some('\u{bf}') {
                3
            } else {
                0
            };
        }
    }
    0
}

pub fn remove_byte_order_mark(text: String) -> String {
    let length = get_byte_order_mark_length(&text);
    if length > 0 {
        text.chars().skip(length).collect()
    } else {
        text
    }
}

pub fn add_utf8_byte_order_mark(text: String) -> String {
    if get_byte_order_mark_length(&text) == 0 {
        format!("\u{00ef}\u{00BB}\u{00BF}{text}")
    } else {
        text
    }
}
