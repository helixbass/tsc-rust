#![allow(non_upper_case_globals)]

use std::borrow::Cow;
use std::rc::Rc;

use super::{
    char_size, code_point_at, comment_directive_reg_ex_multi_line,
    comment_directive_reg_ex_single_line, hex_digits_to_u32, is_code_point,
    is_conflict_marker_trivia, is_digit, is_hex_digit, is_identifier_part, is_identifier_start,
    is_line_break, is_octal_digit, is_shebang_trivia, is_white_space_single_line,
    maybe_code_point_at, scan_conflict_marker_trivia, scan_shebang_trivia, text_to_keyword,
    utf16_encode_as_string, ErrorCallback, Scanner,
};
use crate::{
    parse_pseudo_big_int, reduce_source_text_slice_or_static_cows, CharacterCodes,
    CharacterCodesChar, Debug_, DiagnosticMessage, Diagnostics, ScriptTarget, SourceTextSlice,
    SourceTextSliceOrString, SyntaxKind, TokenFlags,
};

impl Scanner {
    pub(super) fn error(
        &self,
        on_error: Option<ErrorCallback>,
        message: &DiagnosticMessage,
        err_pos: Option<usize>,
        length: Option<usize>,
    ) {
        let err_pos = err_pos.unwrap_or_else(|| self.pos());
        if let Some(on_error) = on_error {
            let old_pos = self.pos();
            self.set_pos(err_pos);
            on_error(message, length.unwrap_or(0));
            self.set_pos(old_pos);
        }
    }

    pub(super) fn scan_number_fragment(
        &self,
        on_error: Option<ErrorCallback>,
    ) -> SourceTextSliceOrString {
        let mut start = self.pos();
        let mut allow_separator = false;
        let mut is_previous_token_separator = false;
        let mut result: Option<Vec<SourceTextSlice>> = None;
        loop {
            let ch = self.maybe_text_char_at_index(self.pos());
            let ch = match ch {
                Some(ch) => ch,
                None => break,
            };
            if ch == CharacterCodes::underscore {
                self.add_token_flag(TokenFlags::ContainsSeparator);
                if allow_separator {
                    allow_separator = false;
                    is_previous_token_separator = true;
                    result
                        .get_or_insert_with(|| vec![])
                        .push(self.text_slice(start, self.pos()));
                } else if is_previous_token_separator {
                    self.error(
                        on_error,
                        &Diagnostics::Multiple_consecutive_numeric_separators_are_not_permitted,
                        Some(self.pos()),
                        Some(1),
                    );
                } else {
                    self.error(
                        on_error,
                        &Diagnostics::Numeric_separators_are_not_allowed_here,
                        Some(self.pos()),
                        Some(1),
                    );
                }
                self.increment_pos();
                start = self.pos();
                continue;
            }
            if is_digit(ch) {
                allow_separator = true;
                is_previous_token_separator = false;
                self.increment_pos();
                continue;
            }
            break;
        }
        if self.text_char_at_index(self.pos() - 1) == CharacterCodes::underscore {
            self.error(
                on_error,
                &Diagnostics::Numeric_separators_are_not_allowed_here,
                Some(self.pos() - 1),
                Some(1),
            );
        }
        match result {
            None => {
                if self.pos() > start {
                    self.text_slice(start, self.pos()).into()
                } else {
                    "".into()
                }
            }
            Some(result) => {
                result.push(self.text_slice(start, self.pos()));
                result.iter().map(|slice| &**slice).concat().into()
            }
        }
    }

    pub(super) fn scan_number(&self, on_error: Option<ErrorCallback>) -> ScanNumberReturn {
        let start = self.pos();
        let main_fragment = self.scan_number_fragment(on_error);
        let mut decimal_fragment: Option<SourceTextSliceOrString> = None;
        let mut scientific_fragment: Option<SourceTextSliceOrString> = None;
        if self.maybe_text_char_at_index(self.pos()) == Some(CharacterCodes::dot) {
            self.increment_pos();
            decimal_fragment = Some(self.scan_number_fragment(on_error));
        }
        let mut end = self.pos();

        if matches!(
            self.maybe_text_char_at_index(self.pos()),
            Some(CharacterCodes::E | CharacterCodes::e)
        ) {
            self.increment_pos();
            self.add_token_flag(TokenFlags::Scientific);
            if matches!(
                self.maybe_text_char_at_index(self.pos()),
                Some(CharacterCodes::plus | CharacterCodes::minus)
            ) {
                self.increment_pos();
            }
            let pre_numeric_part = self.pos();
            let final_fragment = self.scan_number_fragment(on_error);
            if final_fragment.is_empty() {
                self.error(on_error, &Diagnostics::Digit_expected, None, None);
            } else {
                let slice = self.text_slice(end, pre_numeric_part);
                scientific_fragment = Some(match final_fragment {
                    SourceTextSliceOrString::SourceTextSlice(final_fragment) => {
                        slice.extended(final_fragment.end()).into()
                    }
                    SourceTextSliceOrString::StaticStr(final_fragment) => {
                        format!("{}{}", &*slice, final_fragment)
                    }
                    SourceTextSliceOrString::String(final_fragment) => {
                        format!("{}{}", &*slice, final_fragment)
                    }
                });
                end = self.pos();
            }
        }
        let mut result: SourceTextSliceOrString;
        if self.token_flags().intersects(TokenFlags::ContainsSeparator) {
            result = main_fragment;
            if let Some(decimal_fragment) = decimal_fragment.as_ref() {
                result = format!("{}.{}", &*result, &**decimal_fragment).into();
            }
            if let Some(scientific_fragment) = scientific_fragment {
                result = format!("{}{}", &*result, &**scientific_fragment).into();
            }
        } else {
            result = self.text_slice(start, end).into();
        }

        if decimal_fragment.is_some() || self.token_flags().intersects(TokenFlags::Scientific) {
            self.check_for_identifier_start_after_numeric_literal(
                on_error,
                start,
                Some(
                    decimal_fragment.is_none()
                        && self.token_flags().intersects(TokenFlags::Scientific),
                ),
            );
            ScanNumberReturn {
                type_: SyntaxKind::NumericLiteral,
                value: result.parse::<f64>().unwrap().to_string().into(),
            }
        } else {
            self.set_token_value(result);
            let type_ = self.check_big_int_suffix();
            self.check_for_identifier_start_after_numeric_literal(on_error, start, None);
            ScanNumberReturn {
                type_,
                value: self.token_value().clone(),
            }
        }
    }

    pub(super) fn check_for_identifier_start_after_numeric_literal(
        &self,
        on_error: Option<ErrorCallback>,
        numeric_start: usize,
        is_scientific: Option<bool>,
    ) {
        let is_scientific = is_scientific.unwrap_or(false);
        if !matches!(
            maybe_code_point_at(&self.text(), self.pos()),
            Some(ch) if is_identifier_start(
                ch,
                Some(self.language_version),
            )
        ) {
            return;
        }

        let identifier_start = self.pos();
        let length = self.scan_identifier_parts(on_error).len();

        if length == 1 && self.text_char_at_index(identifier_start) == CharacterCodes::n {
            if is_scientific {
                self.error(
                    on_error,
                    &Diagnostics::A_bigint_literal_cannot_use_exponential_notation,
                    Some(numeric_start),
                    Some(identifier_start - numeric_start + 1),
                );
            } else {
                self.error(
                    on_error,
                    &Diagnostics::A_bigint_literal_must_be_an_integer,
                    Some(numeric_start),
                    Some(identifier_start - numeric_start + 1),
                );
            }
        } else {
            self.error(
                on_error,
                &Diagnostics::An_identifier_or_keyword_cannot_immediately_follow_a_numeric_literal,
                Some(identifier_start),
                Some(length),
            );
            self.set_pos(identifier_start);
        }
    }

    pub(super) fn scan_octal_digits(&self) -> u32 {
        let start = self.pos();
        while matches!(self.maybe_text_char_at_index(self.pos()), Some(ch) if is_octal_digit(ch)) {
            self.increment_pos();
        }
        u32::from_str_radix(&self.text_slice(start, self.pos()), 10).unwrap()
    }

    pub(super) fn scan_exact_number_of_hex_digits(
        &self,
        on_error: Option<ErrorCallback>,
        count: usize,
        can_have_separators: bool,
    ) -> Result<u32, String> {
        let value_string = self.scan_hex_digits(on_error, count, false, can_have_separators);
        match value_string {
            Some(value_string) => hex_digits_to_u32(&value_string),
        }
        if !value_string.is_empty() {
        } else {
            Err("Couldn't scan any hex digits".to_string())
        }
    }

    pub(super) fn scan_minimum_number_of_hex_digits(
        &self,
        on_error: Option<ErrorCallback>,
        count: usize,
        can_have_separators: bool,
    ) -> Option<SourceTextSliceOrString> {
        self.scan_hex_digits(on_error, count, true, can_have_separators)
    }

    pub(super) fn scan_hex_digits(
        &self,
        on_error: Option<ErrorCallback>,
        min_count: usize,
        scan_as_many_as_possible: bool,
        can_have_separators: bool,
    ) -> Option<SourceTextSliceOrString> {
        let mut value_chunks: Option<Vec<SourceTextSliceOrString>> = None;
        let mut allow_separator = false;
        let mut is_previous_token_separator = false;
        let start = self.pos();
        let mut last_push_pos: Option<usize> = None;
        while self.pos() - start < min_count || scan_as_many_as_possible {
            let ch = self.text_char_at_index(self.pos());
            if can_have_separators && ch == CharacterCodes::underscore {
                self.add_token_flag(TokenFlags::ContainsSeparator);
                if allow_separator {
                    allow_separator = false;
                    is_previous_token_separator = true;
                } else if is_previous_token_separator {
                    self.error(
                        on_error,
                        &Diagnostics::Multiple_consecutive_numeric_separators_are_not_permitted,
                        Some(self.pos()),
                        Some(1),
                    );
                } else {
                    self.error(
                        on_error,
                        &Diagnostics::Numeric_separators_are_not_allowed_here,
                        Some(self.pos()),
                        Some(1),
                    );
                }
                self.increment_pos();
                continue;
            }
            allow_separator = can_have_separators;
            if ch >= CharacterCodes::A && ch <= CharacterCodes::F {
                let should_push_initial_source_text_slice =
                    self.pos() > start && value_chunks.is_none();
                let value_chunks = value_chunks.get_or_insert_with(|| vec![]);
                if should_push_initial_source_text_slice {
                    value_chunks.push(self.text_slice(start, self.pos() + 1).into());
                }
                value_chunks.push(
                    match ch {
                        CharacterCodes::A => CharacterCodes::a,
                        CharacterCodes::B => CharacterCodes::b,
                        CharacterCodes::C => CharacterCodes::c,
                        CharacterCodes::D => CharacterCodes::d,
                        CharacterCodes::E => CharacterCodes::e,
                        CharacterCodes::F => CharacterCodes::f,
                        _ => panic!("Expected uppercase hex digit"),
                    }
                    .into(),
                );
                last_push_pos = Some(self.pos());
            } else if !(ch >= CharacterCodes::_0 && ch <= CharacterCodes::_9
                || ch >= CharacterCodes::a && ch <= CharacterCodes::f)
            {
                break;
            }
            self.increment_pos();
            is_previous_token_separator = false;
        }
        if matches!(
            last_push_pos,
            Some(last_push_pos) if self.pos() > last_push_pos + 1
        ) {
            value_chunks
                .as_mut()
                .unwrap()
                .push(self.text_slice(last_push_pos + 1, self.pos()).into());
        }
        let mut should_return_none = false;
        if self.pos() - start < min_count {
            should_return_none = true;
        }
        if self.text_char_at_index(self.pos() - 1) == CharacterCodes::underscore {
            self.error(
                on_error,
                &Diagnostics::Numeric_separators_are_not_allowed_here,
                Some(self.pos() - 1),
                Some(1),
            );
        }
        if should_return_none {
            None
        } else {
            match value_chunks {
                None => {
                    if self.pos() > start {
                        Some(self.text_slice(start, self.pos()).into())
                    } else {
                        None
                    }
                }
                Some(value_chunks) => Some(
                    value_chunks
                        .iter()
                        .map(|value_chunk| &**value_chunk)
                        .concat()
                        .into(),
                ),
            }
        }
    }

    pub(super) fn scan_string(
        &self,
        on_error: Option<ErrorCallback>,
        jsx_attribute_string: Option<bool>,
    ) -> SourceTextSliceOrString {
        let jsx_attribute_string = jsx_attribute_string.unwrap_or(false);
        let quote = self.text_char_at_index(self.pos());
        self.increment_pos();
        let mut result: Option<Vec<SourceTextSliceOrString>> = None;
        let mut start = self.pos();
        loop {
            if self.pos() >= self.end() {
                self.add_token_flag(TokenFlags::Unterminated);
                self.error(
                    on_error,
                    &Diagnostics::Unterminated_string_literal,
                    None,
                    None,
                );
                let slice = self.text_slice(start, self.pos());
                match result.as_mut() {
                    None => return slice.into(),
                    Some(result) => {
                        result.push(slice.into());
                    }
                }
                break;
            }
            let ch = self.text_char_at_index(self.pos());
            if ch == quote {
                let slice = self.text_slice(start, self.pos());
                self.increment_pos();
                match result.as_mut() {
                    None => return slice.into(),
                    Some(result) => {
                        result.push(slice.into());
                    }
                }
                break;
            }
            if ch == CharacterCodes::backslash && !jsx_attribute_string {
                let result = result.get_or_insert_with(|| vec![]);
                result.push(self.text_slice(start, self.pos()).into());
                result.push(self.scan_escape_sequence(on_error, None));
                start = self.pos();
                continue;
            }
            if is_line_break(ch) && !jsx_attribute_string {
                self.add_token_flag(TokenFlags::Unterminated);
                self.error(
                    on_error,
                    &Diagnostics::Unterminated_string_literal,
                    None,
                    None,
                );
                let slice = self.text_slice(start, self.pos());
                match result.as_mut() {
                    None => return slice.into(),
                    Some(result) => {
                        result.push(slice.into());
                    }
                }
                break;
            }
            self.increment_pos();
        }
        match result {
            None => "".into(),
            Some(result) => {
                Debug_.assert(
                    result.len() > 1,
                    Some("Should've early-returned without allocating a result Vec"),
                );
                reduce_source_text_slice_or_static_cows(result)
            }
        }
    }

    pub(super) fn scan_template_and_set_token_value(
        &self,
        on_error: Option<ErrorCallback>,
        is_tagged_template: bool,
    ) -> SyntaxKind {
        let started_with_backtick = self.text_char_at_index(self.pos()) == CharacterCodes::backtick;

        self.increment_pos();
        let mut start = self.pos();
        let mut contents: Option<Vec<SourceTextSliceOrString>> = None;
        let resulting_token: SyntaxKind;

        loop {
            if self.pos() >= self.end() {
                self.add_token_flag(TokenFlags::Unterminated);
                self.error(
                    on_error,
                    &Diagnostics::Unterminated_template_literal,
                    None,
                    None,
                );
                resulting_token = if started_with_backtick {
                    SyntaxKind::NoSubstitutionTemplateLiteral
                } else {
                    SyntaxKind::TemplateTail
                };
                let slice = self.text_slice(start, self.pos());
                match contents.as_mut() {
                    None => {
                        self.set_token_value(slice.into());
                        return resulting_token;
                    }
                    Some(contents) => {
                        contents.push(slice.into());
                    }
                }
                break;
            }

            let curr_char = self.text_char_at_index(self.pos());

            if curr_char == CharacterCodes::backtick {
                self.increment_pos();
                resulting_token = if started_with_backtick {
                    SyntaxKind::NoSubstitutionTemplateLiteral
                } else {
                    SyntaxKind::TemplateTail
                };
                let slice = self.text_slice(start, self.pos());
                match contents.as_mut() {
                    None => {
                        self.set_token_value(slice.into());
                        return resulting_token;
                    }
                    Some(contents) => {
                        contents.push(slice.into());
                    }
                }
                break;
            }

            if curr_char == CharacterCodes::dollar_sign
                && self.pos() + 1 < self.end()
                && self.text_char_at_index(self.pos() + 1) == CharacterCodes::open_brace
            {
                self.increment_pos_by(2);
                resulting_token = if started_with_backtick {
                    SyntaxKind::TemplateHead
                } else {
                    SyntaxKind::TemplateMiddle
                };
                let slice = self.text_slice(start, self.pos());
                match contents.as_mut() {
                    None => {
                        self.set_token_value(slice.into());
                        return resulting_token;
                    }
                    Some(contents) => {
                        contents.push(slice.into());
                    }
                }
                break;
            }

            if curr_char == CharacterCodes::backslash {
                let contents = contents.get_or_insert_with(|| vec![]);
                contents.push(self.text_slice(start, self.pos()).into());
                contents.push(self.scan_escape_sequence(on_error, Some(is_tagged_template)));
                start = self.pos();
                continue;
            }

            if curr_char == CharacterCodes::carriage_return {
                let contents = contents.get_or_insert_with(|| vec![]);
                contents.push(self.text_slice(start, self.pos()).into());
                self.increment_pos();

                if self.pos() < self.end()
                    && self.text_char_at_index(self.pos()) == CharacterCodes::line_feed
                {
                    self.increment_pos();
                }

                contents.push("\n".into());
                start = self.pos();
                continue;
            }

            self.increment_pos();
        }

        // Debug.assert(resultingToken !== undefined);

        self.set_token_value(match contents {
            None => "".into(),
            Some(contents) => {
                Debug_.assert(
                    contents.len() > 1,
                    Some("Should've early-returned without allocating a result Vec"),
                );
                reduce_source_text_slice_or_static_cows(contents)
            }
        });
        resulting_token
    }

    pub(super) fn scan_escape_sequence(
        &self,
        on_error: Option<ErrorCallback>,
        is_tagged_template: Option<bool>,
    ) -> SourceTextSliceOrString {
        let is_tagged_template = is_tagged_template.unwrap_or(false);
        let start = self.pos();
        self.increment_pos();
        if self.pos() >= self.end() {
            self.error(on_error, &Diagnostics::Unexpected_end_of_text, None, None);
            return "".into();
        }
        let ch = self.text_char_at_index(self.pos());
        self.increment_pos();
        match ch {
            CharacterCodes::_0 => {
                if is_tagged_template
                    && self.pos() < self.end()
                    && is_digit(self.text_char_at_index(self.pos()))
                {
                    self.increment_pos();
                    self.add_token_flag(TokenFlags::ContainsInvalidEscape);
                    return self.text_slice(start, self.pos()).into();
                }
                "\0".into()
            }
            CharacterCodes::b => "\u{0008}".into(),
            CharacterCodes::t => "\t".into(),
            CharacterCodes::n => "\n".into(),
            CharacterCodes::v => "\u{000b}".into(),
            CharacterCodes::f => "\u{000c}".into(),
            CharacterCodes::r => "\r".into(),
            CharacterCodes::single_quote => "'".into(),
            CharacterCodes::double_quote => "\"".into(),
            CharacterCodes::u => {
                if is_tagged_template {
                    let mut escape_pos = self.pos();
                    while escape_pos < self.pos() + 4 {
                        if escape_pos < self.end()
                            && !is_hex_digit(self.text_char_at_index(escape_pos))
                            && self.text_char_at_index(escape_pos) != CharacterCodes::open_brace
                        {
                            self.set_pos(escape_pos);
                            self.add_token_flag(TokenFlags::ContainsInvalidEscape);
                            return self.text_slice(start, self.pos()).into();
                        }
                        escape_pos += 1;
                    }
                }
                if self.pos() < self.end()
                    && self.text_char_at_index(self.pos()) == CharacterCodes::open_brace
                {
                    self.increment_pos();

                    if is_tagged_template && !is_hex_digit(self.text_char_at_index(self.pos())) {
                        self.add_token_flag(TokenFlags::ContainsInvalidEscape);
                        return self.text_slice(start, self.pos());
                    }

                    if is_tagged_template {
                        let save_pos = self.pos();
                        let escaped_value_string =
                            self.scan_minimum_number_of_hex_digits(on_error, 1, false);
                        let is_escaped_value_empty = escaped_value_string.is_none();
                        let escaped_value = if !is_escaped_value_empty {
                            hex_digits_to_u32(&escaped_value_string.unwrap())
                        } else {
                            Err("Couldn't scan any hex digits".to_owned())
                        };

                        if !match (is_escaped_value_empty, escaped_value) {
                            (true, _) => true,
                            (_, Err(_)) => false,
                            (_, Ok(escaped_value)) => is_code_point(escaped_value),
                        } || self.text_char_at_index(self.pos()) != CharacterCodes::close_brace
                        {
                            self.add_token_flag(TokenFlags::ContainsInvalidEscape);
                            return self.text_slice(start, self.pos()).into();
                        } else {
                            self.set_pos(save_pos);
                        }
                    }
                    self.add_token_flag(TokenFlags::ExtendedUnicodeEscape);
                    return self.scan_extended_unicode_escape(on_error).into();
                }

                self.add_token_flag(TokenFlags::UnicodeEscape);
                self.scan_hexadecimal_escape(on_error, 4).into()
            }
            CharacterCodes::x => {
                if is_tagged_template {
                    if !is_hex_digit(self.text_char_at_index(self.pos())) {
                        self.add_token_flag(TokenFlags::ContainsInvalidEscape);
                        return self.text_slice(start, self.pos()).into();
                    } else if !is_hex_digit(self.text_char_at_index(self.pos() + 1)) {
                        self.increment_pos();
                        self.add_token_flag(TokenFlags::ContainsInvalidEscape);
                        return self.text_slice(start, self.pos()).into();
                    }
                }

                self.scan_hexadecimal_escape(on_error, 2).into()
            }
            CharacterCodes::carriage_return => {
                if self.pos() < self.end()
                    && self.text_char_at_index(self.pos()) == CharacterCodes::line_feed
                {
                    self.increment_pos();
                }
                "".into()
            }
            CharacterCodes::line_feed
            | CharacterCodes::line_separator
            | CharacterCodes::paragraph_separator => "".into(),
            _ => self.text_slice(self.pos() - 1, self.pos()).into(),
        }
    }

    pub(super) fn scan_hexadecimal_escape(
        &self,
        on_error: Option<ErrorCallback>,
        num_digits: usize,
    ) -> Cow<'static, str> {
        let escaped_value = self.scan_exact_number_of_hex_digits(on_error, num_digits, false);

        match escaped_value {
            Ok(escaped_value) => escaped_value.to_string().into(),
            Err(_) => {
                self.error(
                    on_error,
                    &Diagnostics::Hexadecimal_digit_expected,
                    None,
                    None,
                );
                "".into()
            }
        }
    }

    pub(super) fn scan_extended_unicode_escape(&self, on_error: Option<ErrorCallback>) -> String {
        let escaped_value_string = self.scan_minimum_number_of_hex_digits(on_error, 1, false);
        let escaped_value = if !escaped_value_string.is_none() {
            hex_digits_to_u32(&escaped_value_string.unwrap())
        } else {
            Err("Couldn't scan any hex digits")
        };
        let mut is_invalid_extended_escape = false;

        match escaped_value {
            Err(_) => {
                self.error(
                    on_error,
                    &Diagnostics::Hexadecimal_digit_expected,
                    None,
                    None,
                );
                is_invalid_extended_escape = true;
            }
            Ok(escaped_value) if escaped_value > 0x10ffff => {
                self.error(on_error, &Diagnostics::An_extended_Unicode_escape_value_must_be_between_0x0_and_0x10FFFF_inclusive, None, None);
                is_invalid_extended_escape = true;
            }
            _ => (),
        }

        if self.pos() >= self.end() {
            self.error(on_error, &Diagnostics::Unexpected_end_of_text, None, None);
            is_invalid_extended_escape = true;
        } else if self.text_char_at_index(self.pos()) == CharacterCodes::close_brace {
            self.increment_pos();
        } else {
            self.error(
                on_error,
                &Diagnostics::Unterminated_Unicode_escape_sequence,
                None,
                None,
            );
            is_invalid_extended_escape = true;
        }

        if is_invalid_extended_escape {
            return "".to_owned();
        }

        utf16_encode_as_string(escaped_value.unwrap())
    }

    pub(super) fn peek_unicode_escape(
        &self,
        on_error: Option<ErrorCallback>,
    ) -> Result<u32, String> {
        if self.pos() + 5 < self.end()
            && self.text_char_at_index(self.pos() + 1) == CharacterCodes::u
        {
            let start = self.pos();
            self.increment_pos_by(2);
            let value = self.scan_exact_number_of_hex_digits(on_error, 4, false);
            self.set_pos(start);
            return value;
        }
        Err("Didn't find Unicode escape".to_string())
    }

    pub(super) fn peek_extended_unicode_escape(
        &self,
        on_error: Option<ErrorCallback>,
    ) -> Result<u32, &'static str> {
        if self.language_version >= ScriptTarget::ES2015
            && matches!(
                maybe_code_point_at(&self.text(), self.pos() + 1),
                Some(CharacterCodes::u)
            )
            && matches!(
                maybe_code_point_at(&self.text(), self.pos() + 2),
                Some(CharacterCodes::open_brace)
            )
        {
            let start = self.pos();
            self.increment_pos_by(3);
            let escaped_value_string = self.scan_minimum_number_of_hex_digits(on_error, 1, false);
            let escaped_value = if !escaped_value_string.is_none() {
                hex_digits_to_u32(&escaped_value_string.unwrap())
            } else {
                Err("Couldn't scan any hex digits")
            };
            self.set_pos(start);
            return escaped_value;
        }
        Err("Didn't find extended Unicode escape")
    }

    pub(super) fn scan_identifier_parts(
        &self,
        on_error: Option<ErrorCallback>,
    ) -> SourceTextSliceOrString {
        let mut result: Option<Vec<SourceTextSliceOrString>> = None;
        let mut start = self.pos();
        while self.pos() < self.end() {
            let ch = code_point_at(&self.text(), self.pos());
            if is_identifier_part(
                ch.chars().next().unwrap(),
                Some(self.language_version),
                None,
            ) {
                self.increment_pos_by(char_size(ch));
            } else if ch == CharacterCodes::backslash {
                let ch_u32 = self.peek_extended_unicode_escape(on_error);
                if let Ok(ch_u32) = ch_u32 {
                    let ch = char::from_u32(ch_u32);
                    if matches!(ch, Some(ch) if is_identifier_part(ch, Some(self.language_version), None))
                    {
                        self.increment_pos_by(3);
                        self.add_token_flag(TokenFlags::ExtendedUnicodeEscape);
                        result
                            .get_or_insert_with(|| vec![])
                            .push(self.scan_extended_unicode_escape(on_error).into());
                        start = self.pos();
                        continue;
                    }
                }
                let ch_u32 = self.peek_unicode_escape(on_error);
                let ch = match ch_u32 {
                    Err(_) => {
                        break;
                    }
                    Ok(ch_u32) => {
                        let ch = char::from_u32(ch_u32);
                        match ch {
                            Some(ch)
                                if is_identifier_part(ch, Some(self.language_version), None) =>
                            {
                                ch
                            }
                            _ => {
                                break;
                            }
                        }
                    }
                };
                self.add_token_flag(TokenFlags::UnicodeEscape);
                let result = result.get_or_insert_with(|| vec![]);
                result.push(self.text_slice(start, self.pos()).into());
                // result.push_str(&utf16_encode_as_string(ch));
                result.push(ch.to_string().into());
                self.increment_pos_by(6);
                start = self.pos();
            } else {
                break;
            }
        }
        let slice = self.text_slice(start, self.pos());
        match result {
            None => slice.into(),
            Some(result) => {
                result.push(slice.into());
                result.iter().map(|value| &**value).concat().into()
            }
        }
    }

    pub(super) fn get_identifier_token(&self) -> SyntaxKind {
        let len = self.token_value().len();
        if len >= 2 && len <= 12 {
            if self.token_value().starts_with(|ch| ch >= 'a' && ch <= 'z') {
                let token_value_as_str: &str = &self.token_value();
                let keyword = text_to_keyword.get(&token_value_as_str);
                if let Some(keyword) = keyword {
                    return self.set_token(*keyword);
                }
            }
        }
        self.set_token(SyntaxKind::Identifier)
    }

    pub(super) fn scan_binary_or_octal_digits(
        &self,
        on_error: Option<ErrorCallback>,
        base: u32, /*2 | 8*/
    ) -> String {
        let mut value = "".to_owned();
        let mut separator_allowed = false;
        let mut is_previous_token_separator = false;
        loop {
            let ch = self.maybe_text_char_at_index(self.pos());
            if matches!(ch, Some(CharacterCodes::underscore)) {
                self.add_token_flag(TokenFlags::ContainsSeparator);
                if separator_allowed {
                    separator_allowed = false;
                    is_previous_token_separator = true;
                } else if is_previous_token_separator {
                    self.error(
                        on_error,
                        &Diagnostics::Multiple_consecutive_numeric_separators_are_not_permitted,
                        Some(self.pos()),
                        Some(1),
                    );
                } else {
                    self.error(
                        on_error,
                        &Diagnostics::Numeric_separators_are_not_allowed_here,
                        Some(self.pos()),
                        Some(1),
                    );
                }
                self.increment_pos();
                continue;
            }
            separator_allowed = true;
            if !matches!(ch, Some(ch) if is_digit(ch))
                || ch.unwrap().chars().next().unwrap() as u32 - CharacterCodesChar::_0 as u32
                    >= base
            {
                break;
            }
            value.push_str(self.text_char_at_index(self.pos()));
            self.increment_pos();
            is_previous_token_separator = false;
        }
        if self.text_char_at_index(self.pos() - 1) == CharacterCodes::underscore {
            self.error(
                on_error,
                &Diagnostics::Numeric_separators_are_not_allowed_here,
                Some(self.pos() - 1),
                Some(1),
            );
        }
        value
    }

    pub(super) fn check_big_int_suffix(&self) -> SyntaxKind {
        if self.maybe_text_char_at_index(self.pos()) == Some(CharacterCodes::n) {
            self.set_token_value(match &*self.token_value() {
                SourceTextSliceOrString::SourceTextSlice(token_value) => token_value
                    .extended(Some(token_value.end.unwrap() + 1))
                    .into(),
                SourceTextSliceOrString::StaticCow(token_value) => {
                    format!("{}n", token_value).into()
                }
                SourceTextSliceOrString::String(token_value) => format!("{}n", token_value).into(),
            });
            if self
                .token_flags()
                .intersects(TokenFlags::BinaryOrOctalSpecifier)
            {
                self.set_token_value(
                    format!("{}n", parse_pseudo_big_int(&self.token_value())).into(),
                );
            }
            self.increment_pos();
            SyntaxKind::BigIntLiteral
        } else {
            let numeric_value = if self.token_flags().intersects(TokenFlags::BinarySpecifier) {
                u32::from_str_radix(&self.token_value()[2..], 2).unwrap()
            } else if self.token_flags().intersects(TokenFlags::OctalSpecifier) {
                u32::from_str_radix(&self.token_value()[2..], 8).unwrap()
            } else {
                if self.token_flags().intersects(TokenFlags::HexSpecifier) {
                    u32::from_str_radix(&self.token_value()[2..], 16).unwrap()
                } else {
                    u32::from_str_radix(&self.token_value(), 10).unwrap()
                }
            };
            self.set_token_value(numeric_value.to_string().into());
            SyntaxKind::NumericLiteral
        }
    }

    pub fn scan(&self, on_error: Option<ErrorCallback>) -> SyntaxKind {
        self.set_start_pos(self.pos());
        self.set_token_flags(TokenFlags::None);
        let mut asterisk_seen = false;
        loop {
            self.set_token_pos(self.pos());
            if self.pos() >= self.end() {
                return self.set_token(SyntaxKind::EndOfFileToken);
            }
            let ch = code_point_at(&self.text(), self.pos());

            if ch == CharacterCodes::hash
                && self.pos() == 0
                && is_shebang_trivia(&self.text(), self.pos())
            {
                self.set_pos(scan_shebang_trivia(self.text(), self.pos()));
                if self.skip_trivia {
                    continue;
                } else {
                    return self.set_token(SyntaxKind::ShebangTrivia);
                }
            }

            match ch {
                CharacterCodes::line_feed | CharacterCodes::carriage_return => {
                    self.add_token_flag(TokenFlags::PrecedingLineBreak);
                    if self.skip_trivia {
                        self.increment_pos();
                        continue;
                    } else {
                        if ch == CharacterCodes::carriage_return
                            && self.pos() + 1 < self.end()
                            && self.text_char_at_index(self.pos() + 1) == CharacterCodes::line_feed
                        {
                            self.increment_pos_by(2);
                        } else {
                            self.increment_pos();
                        }
                        return self.set_token(SyntaxKind::NewLineTrivia);
                    }
                }
                CharacterCodes::tab
                | CharacterCodes::vertical_tab
                | CharacterCodes::form_feed
                | CharacterCodes::space
                | CharacterCodes::non_breaking_space
                | CharacterCodes::ogham
                | CharacterCodes::en_quad
                | CharacterCodes::em_quad
                | CharacterCodes::en_space
                | CharacterCodes::em_space
                | CharacterCodes::three_per_em_space
                | CharacterCodes::four_per_em_space
                | CharacterCodes::six_per_em_space
                | CharacterCodes::figure_space
                | CharacterCodes::punctuation_space
                | CharacterCodes::thin_space
                | CharacterCodes::hair_space
                | CharacterCodes::zero_width_space
                | CharacterCodes::narrow_no_break_space
                | CharacterCodes::mathematical_space
                | CharacterCodes::ideographic_space
                | CharacterCodes::byte_order_mark => {
                    if self.skip_trivia {
                        self.increment_pos();
                        continue;
                    } else {
                        while self.pos() < self.end()
                            && is_white_space_single_line(self.text_char_at_index(self.pos()))
                        {
                            self.increment_pos();
                        }
                        return self.set_token(SyntaxKind::WhitespaceTrivia);
                    }
                }
                CharacterCodes::exclamation => {
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::equals)
                    ) {
                        if matches!(
                            self.maybe_text_char_at_index(self.pos() + 2),
                            Some(CharacterCodes::equals)
                        ) {
                            self.increment_pos_by(3);
                            return self.set_token(SyntaxKind::ExclamationEqualsEqualsToken);
                        }
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::ExclamationEqualsToken);
                    }
                    self.increment_pos();
                    return self.set_token(SyntaxKind::ExclamationToken);
                }
                CharacterCodes::double_quote | CharacterCodes::single_quote => {
                    self.set_token_value(self.scan_string(on_error, None));
                    return self.set_token(SyntaxKind::StringLiteral);
                }
                CharacterCodes::backtick => {
                    return self.set_token(self.scan_template_and_set_token_value(on_error, false));
                }
                CharacterCodes::percent => {
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::equals)
                    ) {
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::PercentEqualsToken);
                    }
                    self.increment_pos();
                    return self.set_token(SyntaxKind::PercentToken);
                }
                CharacterCodes::ampersand => {
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::ampersand)
                    ) {
                        if matches!(
                            self.maybe_text_char_at_index(self.pos() + 2),
                            Some(CharacterCodes::equals)
                        ) {
                            self.increment_pos_by(3);
                            return self.set_token(SyntaxKind::AmpersandAmpersandEqualsToken);
                        }
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::AmpersandAmpersandToken);
                    }
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::equals)
                    ) {
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::AmpersandEqualsToken);
                    }
                    self.increment_pos();
                    return self.set_token(SyntaxKind::AmpersandToken);
                }
                CharacterCodes::open_paren => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::OpenParenToken);
                }
                CharacterCodes::close_paren => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::CloseParenToken);
                }
                CharacterCodes::asterisk => {
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::equals)
                    ) {
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::AsteriskEqualsToken);
                    }
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::asterisk)
                    ) {
                        if matches!(
                            self.maybe_text_char_at_index(self.pos() + 2),
                            Some(CharacterCodes::equals)
                        ) {
                            self.increment_pos_by(3);
                            return self.set_token(SyntaxKind::AsteriskAsteriskEqualsToken);
                        }
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::AsteriskAsteriskToken);
                    }
                    self.increment_pos();
                    if self.in_jsdoc_type() != 0
                        && !asterisk_seen
                        && self
                            .token_flags()
                            .intersects(TokenFlags::PrecedingLineBreak)
                    {
                        asterisk_seen = true;
                        continue;
                    }
                    return self.set_token(SyntaxKind::AsteriskToken);
                }
                CharacterCodes::plus => {
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::plus)
                    ) {
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::PlusPlusToken);
                    }
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::equals)
                    ) {
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::PlusEqualsToken);
                    }
                    self.increment_pos();
                    return self.set_token(SyntaxKind::PlusToken);
                }
                CharacterCodes::comma => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::CommaToken);
                }
                CharacterCodes::minus => {
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::minus)
                    ) {
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::MinusMinusToken);
                    }
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::equals)
                    ) {
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::MinusEqualsToken);
                    }
                    self.increment_pos();
                    return self.set_token(SyntaxKind::MinusToken);
                }
                CharacterCodes::dot => {
                    if matches!(self.maybe_text_char_at_index(self.pos() + 1), Some(ch) if is_digit(ch))
                    {
                        self.set_token_value(self.scan_number(on_error).value);
                        return self.set_token(SyntaxKind::NumericLiteral);
                    }
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::dot)
                    ) && matches!(
                        self.maybe_text_char_at_index(self.pos() + 2),
                        Some(CharacterCodes::dot)
                    ) {
                        self.increment_pos_by(3);
                        return self.set_token(SyntaxKind::DotDotDotToken);
                    }
                    self.increment_pos();
                    return self.set_token(SyntaxKind::DotToken);
                }
                CharacterCodes::slash => {
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::slash)
                    ) {
                        self.increment_pos_by(2);

                        while self.pos() < self.end() {
                            if is_line_break(self.text_char_at_index(self.pos())) {
                                break;
                            }
                            self.increment_pos();
                        }

                        /*self.set_comment_directives(*/
                        self.append_if_comment_directive(
                            &mut self.maybe_comment_directives_mut(),
                            &self.text_slice(self.token_pos(), self.pos()),
                            &comment_directive_reg_ex_single_line,
                            self.token_pos(),
                        )/*)*/;

                        if self.skip_trivia {
                            continue;
                        } else {
                            return self.set_token(SyntaxKind::SingleLineCommentTrivia);
                        }
                    }

                    if self.maybe_text_char_at_index(self.pos() + 1)
                        == Some(CharacterCodes::asterisk)
                    {
                        self.increment_pos_by(2);
                        if matches!(
                            self.maybe_text_char_at_index(self.pos()),
                            Some(CharacterCodes::asterisk)
                        ) && !matches!(
                            self.maybe_text_char_at_index(self.pos() + 1),
                            Some(CharacterCodes::slash)
                        ) {
                            self.add_token_flag(TokenFlags::PrecedingJSDocComment);
                        }

                        let mut comment_closed = false;
                        let mut last_line_start = self.token_pos();
                        while self.pos() < self.end() {
                            let ch = self.text_char_at_index(self.pos());

                            if ch == CharacterCodes::asterisk
                                && matches!(
                                    self.maybe_text_char_at_index(self.pos() + 1),
                                    Some(CharacterCodes::slash)
                                )
                            {
                                self.increment_pos_by(2);
                                comment_closed = true;
                                break;
                            }

                            self.increment_pos();

                            if is_line_break(ch) {
                                last_line_start = self.pos();
                                self.add_token_flag(TokenFlags::PrecedingLineBreak);
                            }
                        }

                        /*self.set_comment_directives(*/
                        self.append_if_comment_directive(
                            &mut self.maybe_comment_directives_mut(),
                            &self.text_slice(last_line_start, self.pos()),
                            &comment_directive_reg_ex_multi_line,
                            last_line_start
                        )/*)*/;

                        if !comment_closed {
                            self.error(on_error, &Diagnostics::Asterisk_Slash_expected, None, None);
                        }

                        if self.skip_trivia {
                            continue;
                        } else {
                            if !comment_closed {
                                self.add_token_flag(TokenFlags::Unterminated);
                            }
                            return self.set_token(SyntaxKind::MultiLineCommentTrivia);
                        }
                    }

                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::equals)
                    ) {
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::SlashEqualsToken);
                    }

                    self.increment_pos();
                    return self.set_token(SyntaxKind::SlashToken);
                }
                CharacterCodes::_0 => {
                    if self.pos() + 2 < self.end()
                        && self.text_char_at_index(self.pos() + 1) == CharacterCodes::X
                        || self.text_char_at_index(self.pos() + 1) == CharacterCodes::x
                    {
                        self.increment_pos_by(2);
                        self.set_token_value(
                            match self.scan_minimum_number_of_hex_digits(on_error, 1, true) {
                                None => "".into(),
                                Some(value) => Some(Rc::new(value.into())),
                            },
                        );
                        if self.token_value().is_empty() {
                            self.error(
                                on_error,
                                &Diagnostics::Hexadecimal_digit_expected,
                                None,
                                None,
                            );
                            self.set_token_value("0".into());
                        }
                        self.set_token_value(format!("0x{}", &*self.token_value()).into());
                        self.add_token_flag(TokenFlags::HexSpecifier);
                        return self.set_token(self.check_big_int_suffix());
                    } else if self.pos() + 2 < self.end()
                        && self.text_char_at_index(self.pos() + 1) == CharacterCodes::B
                        || self.text_char_at_index(self.pos() + 1) == CharacterCodes::b
                    {
                        self.increment_pos_by(2);
                        self.set_token_value(self.scan_binary_or_octal_digits(on_error, 2).into());
                        if self.token_value().is_empty() {
                            self.error(on_error, &Diagnostics::Binary_digit_expected, None, None);
                            self.set_token_value("0".into());
                        }
                        self.set_token_value(format!("0b{}", &**self.token_value()).into());
                        self.add_token_flag(TokenFlags::BinarySpecifier);
                        return self.set_token(self.check_big_int_suffix());
                    } else if self.pos() + 2 < self.end()
                        && self.text_char_at_index(self.pos() + 1) == CharacterCodes::O
                        || self.text_char_at_index(self.pos() + 1) == CharacterCodes::o
                    {
                        self.increment_pos_by(2);
                        self.set_token_value(self.scan_binary_or_octal_digits(on_error, 8));
                        if self.token_value().is_empty() {
                            self.error(on_error, &Diagnostics::Octal_digit_expected, None, None);
                            self.set_token_value("0".to_string());
                        }
                        self.set_token_value(format!("0o{}", self.token_value()));
                        self.add_token_flag(TokenFlags::OctalSpecifier);
                        return self.set_token(self.check_big_int_suffix());
                    }
                    if self.pos() + 1 < self.end()
                        && is_octal_digit(self.text_char_at_index(self.pos() + 1))
                    {
                        self.set_token_value(self.scan_octal_digits().to_string().into());
                        self.add_token_flag(TokenFlags::Octal);
                        return self.set_token(SyntaxKind::NumericLiteral);
                    }

                    let ScanNumberReturn {
                        type_: token,
                        value: token_value,
                    } = self.scan_number(on_error);
                    self.set_token(token);
                    self.set_token_value(token_value);
                    return token;
                }
                CharacterCodes::_1
                | CharacterCodes::_2
                | CharacterCodes::_3
                | CharacterCodes::_4
                | CharacterCodes::_5
                | CharacterCodes::_6
                | CharacterCodes::_7
                | CharacterCodes::_8
                | CharacterCodes::_9 => {
                    let ScanNumberReturn {
                        type_: token,
                        value: token_value,
                    } = self.scan_number(on_error);
                    self.set_token(token);
                    self.set_token_value(token_value);
                    return token;
                }
                CharacterCodes::colon => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::ColonToken);
                }
                CharacterCodes::semicolon => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::SemicolonToken);
                }
                CharacterCodes::less_than => {
                    if is_conflict_marker_trivia(&self.text(), self.pos()) {
                        self.set_pos(scan_conflict_marker_trivia(
                            &self.text(),
                            self.pos(),
                            // Some(|diag, pos, len| self.error(on_error, diag, pos, len)),
                            |diag, pos, len| self.error(on_error, diag, pos, len),
                        ));
                        if self.skip_trivia {
                            continue;
                        } else {
                            return self.set_token(SyntaxKind::ConflictMarkerTrivia);
                        }
                    }

                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::less_than)
                    ) {
                        if matches!(
                            self.maybe_text_char_at_index(self.pos() + 2),
                            Some(CharacterCodes::equals)
                        ) {
                            self.increment_pos_by(3);
                            return self.set_token(SyntaxKind::LessThanLessThanEqualsToken);
                        }
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::LessThanLessThanToken);
                    }
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::equals)
                    ) {
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::LessThanEqualsToken);
                    }
                    self.increment_pos();
                    return self.set_token(SyntaxKind::LessThanToken);
                }
                CharacterCodes::equals => {
                    if is_conflict_marker_trivia(&self.text(), self.pos()) {
                        self.set_pos(scan_conflict_marker_trivia(
                            &self.text(),
                            self.pos(),
                            // Some(|diag, pos, len| self.error(on_error, diag, pos, len)),
                            |diag, pos, len| self.error(on_error, diag, pos, len),
                        ));
                        if self.skip_trivia {
                            continue;
                        } else {
                            return self.set_token(SyntaxKind::ConflictMarkerTrivia);
                        }
                    }

                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::equals)
                    ) {
                        if matches!(
                            self.maybe_text_char_at_index(self.pos() + 2),
                            Some(CharacterCodes::equals)
                        ) {
                            self.increment_pos_by(3);
                            return self.set_token(SyntaxKind::EqualsEqualsEqualsToken);
                        }
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::EqualsEqualsToken);
                    }
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::greater_than)
                    ) {
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::EqualsGreaterThanToken);
                    }
                    self.increment_pos();
                    return self.set_token(SyntaxKind::EqualsToken);
                }
                CharacterCodes::greater_than => {
                    if is_conflict_marker_trivia(&self.text(), self.pos()) {
                        self.set_pos(scan_conflict_marker_trivia(
                            &self.text(),
                            self.pos(),
                            // Some(|diag, pos, len| self.error(on_error, diag, pos, len)),
                            |diag, pos, len| self.error(on_error, diag, pos, len),
                        ));
                        if self.skip_trivia {
                            continue;
                        } else {
                            return self.set_token(SyntaxKind::ConflictMarkerTrivia);
                        }
                    }

                    self.increment_pos();
                    return self.set_token(SyntaxKind::GreaterThanToken);
                }
                CharacterCodes::question => {
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::dot)
                    ) && matches!(
                        self.maybe_text_char_at_index(self.pos() + 2),
                        Some(ch) if !is_digit(ch)
                    ) {
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::QuestionDotToken);
                    }
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::question)
                    ) {
                        if matches!(
                            self.maybe_text_char_at_index(self.pos() + 2),
                            Some(CharacterCodes::equals)
                        ) {
                            self.increment_pos_by(3);
                            return self.set_token(SyntaxKind::QuestionQuestionEqualsToken);
                        }
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::QuestionQuestionToken);
                    }
                    self.increment_pos();
                    return self.set_token(SyntaxKind::QuestionToken);
                }
                CharacterCodes::open_bracket => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::OpenBracketToken);
                }
                CharacterCodes::close_bracket => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::CloseBracketToken);
                }
                CharacterCodes::caret => {
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::equals)
                    ) {
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::CaretEqualsToken);
                    }
                    self.increment_pos();
                    return self.set_token(SyntaxKind::CaretToken);
                }
                CharacterCodes::open_brace => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::OpenBraceToken);
                }
                CharacterCodes::bar => {
                    if is_conflict_marker_trivia(&self.text(), self.pos()) {
                        self.set_pos(scan_conflict_marker_trivia(
                            &self.text(),
                            self.pos(),
                            // Some(|diag, pos, len| self.error(on_error, diag, pos, len)),
                            |diag, pos, len| self.error(on_error, diag, pos, len),
                        ));
                        if self.skip_trivia {
                            continue;
                        } else {
                            return self.set_token(SyntaxKind::ConflictMarkerTrivia);
                        }
                    }

                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::bar)
                    ) {
                        if matches!(
                            self.maybe_text_char_at_index(self.pos() + 2),
                            Some(CharacterCodes::equals)
                        ) {
                            self.increment_pos_by(3);
                            return self.set_token(SyntaxKind::BarBarEqualsToken);
                        }
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::BarBarToken);
                    }
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 1),
                        Some(CharacterCodes::equals)
                    ) {
                        self.increment_pos_by(2);
                        return self.set_token(SyntaxKind::BarEqualsToken);
                    }
                    self.increment_pos();
                    return self.set_token(SyntaxKind::BarToken);
                }
                CharacterCodes::close_brace => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::CloseBraceToken);
                }
                CharacterCodes::tilde => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::TildeToken);
                }
                CharacterCodes::at => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::AtToken);
                }
                CharacterCodes::backslash => {
                    let extended_cooked_char = self.peek_extended_unicode_escape(on_error);
                    if matches!(
                        extended_cooked_char,
                        Ok(extended_cooked_char) if matches!(
                            char::from_u32(extended_cooked_char),
                            Some(extended_cooked_char) if is_identifier_start(extended_cooked_char, Some(self.language_version))
                        )
                    ) {
                        self.increment_pos_by(3);
                        self.add_token_flag(TokenFlags::ExtendedUnicodeEscape);
                        self.set_token_value(
                            format!(
                                "{}{}",
                                self.scan_extended_unicode_escape(on_error),
                                &*self.scan_identifier_parts(on_error)
                            )
                            .into(),
                        );
                        return self.set_token(self.get_identifier_token());
                    }

                    let cooked_char = self.peek_unicode_escape(on_error);
                    if matches!(
                        cooked_char,
                        Ok(cooked_char) if matches!(
                            char::from_u32(cooked_char),
                            Some(cooked_char) if is_identifier_start(cooked_char, Some(self.language_version))
                        )
                    ) {
                        self.increment_pos_by(6);
                        self.add_token_flag(TokenFlags::UnicodeEscape);
                        self.set_token_value(
                            format!(
                                "{}{}",
                                char::from_u32(cooked_char.unwrap()).unwrap(),
                                &*self.scan_identifier_parts(on_error)
                            )
                            .into(),
                        );
                        return self.set_token(self.get_identifier_token());
                    }

                    self.error(on_error, &Diagnostics::Invalid_character, None, None);
                    self.increment_pos();
                    return self.set_token(SyntaxKind::Unknown);
                }
                CharacterCodes::hash => {
                    if self.pos() != 0
                        && matches!(
                            self.maybe_text_char_at_index(self.pos() + 1),
                            Some(CharacterCodes::exclamation)
                        )
                    {
                        self.error(
                            on_error,
                            &Diagnostics::can_only_be_used_at_the_start_of_a_file,
                            None,
                            None,
                        );
                        self.increment_pos();
                        return self.set_token(SyntaxKind::Unknown);
                    }

                    if matches!(maybe_code_point_at(&self.text(), self.pos() + 1), Some(ch) if is_identifier_start(ch.chars().next().unwrap(), Some(self.language_version)))
                    {
                        self.increment_pos();
                        self.scan_identifier(
                            on_error,
                            code_point_at(&self.text(), self.pos()),
                            self.language_version,
                        );
                    } else {
                        self.set_token_value(
                            code_point_at(&self.text(), self.pos()).to_owned().into(),
                        );
                        self.error(
                            on_error,
                            &Diagnostics::Invalid_character,
                            Some(self.pos()),
                            Some(char_size(ch)),
                        );
                        self.increment_pos();
                    }
                    return self.set_token(SyntaxKind::PrivateIdentifier);
                }
                _ch => {
                    let identifier_kind = self.scan_identifier(on_error, ch, self.language_version);
                    if let Some(identifier_kind) = identifier_kind {
                        return self.set_token(identifier_kind);
                    } else if is_white_space_single_line(ch) {
                        self.increment_pos_by(char_size(ch));
                        continue;
                    } else if is_line_break(ch) {
                        self.add_token_flag(TokenFlags::PrecedingLineBreak);
                        self.increment_pos_by(char_size(ch));
                        continue;
                    }
                    let size = char_size(ch);
                    self.error(
                        on_error,
                        &Diagnostics::Invalid_character,
                        Some(self.pos()),
                        Some(size),
                    );
                    self.increment_pos_by(size);
                    return self.set_token(SyntaxKind::Unknown);
                }
            }
        }
    }
}

pub(super) struct ScanNumberReturn {
    type_: SyntaxKind,
    value: SourceTextSliceOrString,
}
