use once_cell::sync::OnceCell;
use regex::{Captures, Regex};
use std::borrow::Cow;

fn test_path_prefix_reg_exp() -> &'static Regex {
    static test_path_prefix_reg_exp: OnceCell<Regex> = OnceCell::new();
    test_path_prefix_reg_exp
        .get_or_init(|| Regex::new(r"(?:(file:/{3})|/)\.(ts|lib|src)/").unwrap())
}

pub fn remove_test_path_prefixes(
    text: &str,
    retain_trailing_directory_separator: Option<bool>,
) -> Cow<'_, str> {
    // text !== undefined ?
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
    // : undefined!
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
    unimplemented!()
}
