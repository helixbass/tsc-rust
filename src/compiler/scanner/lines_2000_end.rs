#![allow(non_upper_case_globals)]

use super::{is_identifier_part, is_identifier_start, ErrorCallback, Scanner};
use crate::{Debug_, ScriptTarget, SourceTextAsChars, SyntaxKind};

impl Scanner {
    pub(super) fn scan_identifier(
        &self,
        start_character: char,
        language_version: ScriptTarget,
    ) -> Option<SyntaxKind> {
        let mut ch = start_character;
        if is_identifier_start(ch, Some(language_version)) {
            self.set_pos(self.pos() + char_size(ch));
            loop {
                if !(self.pos() < self.end()) {
                    break;
                }
                ch = code_point_at(self.text(), self.pos());
                if !is_identifier_part(ch, Some(language_version), None) {
                    break;
                }
                self.set_pos(self.pos() + char_size(ch));
            }
            self.set_token_value(self.text_substring(self.token_pos(), self.pos()));
            return Some(self.get_identifier_token());
        }
        None
    }

    pub fn re_scan_template_token(
        &self,
        on_error: Option<ErrorCallback>,
        is_tagged_template: bool,
    ) -> SyntaxKind {
        Debug_.assert(
            self.token() == SyntaxKind::CloseBraceToken,
            Some("'reScanTemplateToken' should only be called on a '}'"),
        );
        self.set_pos(self.token_pos());
        self.set_token(self.scan_template_and_set_token_value(on_error, is_tagged_template))
    }

    pub fn re_scan_template_head_or_no_substitution_template(
        &self,
        on_error: Option<ErrorCallback>,
    ) -> SyntaxKind {
        self.set_pos(self.token_pos());
        self.set_token(self.scan_template_and_set_token_value(on_error, true))
    }

    pub fn re_scan_less_than_token(&self) -> SyntaxKind {
        if self.token() == SyntaxKind::LessThanLessThanToken {
            unimplemented!()
        }
        self.token()
    }

    pub(super) fn speculation_helper<TReturn, TCallback: FnOnce() -> Option<TReturn>>(
        &self,
        callback: TCallback,
        is_lookahead: bool,
    ) -> Option<TReturn> {
        let save_pos = self.pos();
        let save_start_pos = self.start_pos();
        let save_token_pos = self.token_pos();
        let save_token = self.token();
        let save_token_value = self.token_value();
        let save_token_flags = self.token_flags();
        let result = callback();

        if result.is_none() || is_lookahead {
            self.set_pos(save_pos);
            self.set_start_pos(save_start_pos);
            self.set_token_pos(save_token_pos);
            self.set_token(save_token);
            self.set_token_value(save_token_value);
            self.set_token_flags(save_token_flags);
        }
        result
    }

    pub fn look_ahead<TReturn, TCallback: FnOnce() -> Option<TReturn>>(
        &self,
        callback: TCallback,
    ) -> Option<TReturn> {
        self.speculation_helper(callback, true)
    }

    pub fn try_scan<TReturn, TCallback: FnOnce() -> Option<TReturn>>(
        &self,
        callback: TCallback,
    ) -> Option<TReturn> {
        self.speculation_helper(callback, false)
    }

    pub fn set_text(
        &mut self,
        new_text_as_chars: Option<SourceTextAsChars>,
        new_text: Option<String>,
        start: Option<usize>,
        length: Option<usize>,
    ) {
        let text = new_text.unwrap_or_else(|| "".to_string());
        let text_as_chars = new_text_as_chars.unwrap_or_else(|| vec![]);
        let text_len = text.len();
        self.set_text_(text_as_chars, text);
        self.set_end(length.map_or(text_len, |length| start.unwrap() + length));
        self.set_text_pos(start.unwrap_or(0));
    }

    // pub fn set_on_error(&mut self, error_callback: Option<ErrorCallback<'on_error>>) {
    //     self.on_error = error_callback;
    // }

    pub fn set_text_pos(&mut self, text_pos: usize) {
        // Debug_.assert(text_pos >= 0);
        self.set_pos(text_pos);
        self.set_start_pos(text_pos);
        self.set_token_pos(text_pos);
        self.set_token(SyntaxKind::Unknown);
    }
}

pub(super) fn code_point_at(s: &SourceTextAsChars, i: usize) -> char {
    s[i]
}

pub(super) fn char_size(ch: char) -> usize {
    1
}

pub(crate) fn utf16_encode_as_string(code_point: u32) -> String {
    char::from_u32(code_point).unwrap().to_string()
}

pub(super) fn hex_digits_to_u32(str: &str) -> Result<u32, String> {
    u32::from_str_radix(str, 16).map_err(|_| "Couldn't convert hex digits to u32".to_string())
}
