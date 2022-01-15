#![allow(non_upper_case_globals)]

use std::array::IntoIter;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::iter::FromIterator;

use crate::{
    position_is_synthesized, CharacterCodes, DiagnosticMessage, Diagnostics, SyntaxKind, TokenFlags,
};

pub type ErrorCallback<'callback> = &'callback dyn Fn(&DiagnosticMessage, usize);

pub fn token_is_identifier_or_keyword(token: SyntaxKind) -> bool {
    token >= SyntaxKind::Identifier
}

lazy_static! {
    static ref text_to_keyword_obj: HashMap<String, SyntaxKind> =
        HashMap::from_iter(IntoIter::new([
            ("bigint".to_string(), SyntaxKind::BigIntKeyword),
            ("boolean".to_string(), SyntaxKind::BooleanKeyword),
            ("const".to_string(), SyntaxKind::ConstKeyword),
            ("declare".to_string(), SyntaxKind::DeclareKeyword),
            ("else".to_string(), SyntaxKind::ElseKeyword),
            ("false".to_string(), SyntaxKind::FalseKeyword),
            ("if".to_string(), SyntaxKind::IfKeyword),
            ("interface".to_string(), SyntaxKind::InterfaceKeyword),
            ("number".to_string(), SyntaxKind::NumberKeyword),
            ("true".to_string(), SyntaxKind::TrueKeyword),
            ("type".to_string(), SyntaxKind::TypeKeyword),
            ("var".to_string(), SyntaxKind::VarKeyword),
        ]));
}

lazy_static! {
    static ref text_to_keyword: HashMap<String, SyntaxKind> = text_to_keyword_obj.clone();
}

lazy_static! {
    static ref text_to_token: HashMap<String, SyntaxKind> = {
        let mut ret = text_to_keyword_obj.clone();
        ret.extend(IntoIter::new([(
            ";".to_string(),
            SyntaxKind::SemicolonToken,
        )]));
        ret
    };
}

fn is_unicode_identifier_start(ch: char) -> bool {
    false
}

fn make_reverse_map(source: &HashMap<String, SyntaxKind>) -> HashMap<SyntaxKind, String> {
    let mut result = HashMap::new();
    for (key, val) in source.iter() {
        result.insert(*val, key.clone());
    }
    result
}

lazy_static! {
    static ref token_strings: HashMap<SyntaxKind, String> = make_reverse_map(&text_to_token);
}

pub fn token_to_string(t: SyntaxKind) -> Option<&'static String> {
    token_strings.get(&t)
}

fn is_line_break(ch: char) -> bool {
    ch == CharacterCodes::line_feed
        || ch == CharacterCodes::carriage_return
        || ch == CharacterCodes::line_separator
        || ch == CharacterCodes::paragraph_separator
}

fn is_digit(ch: char) -> bool {
    ch >= CharacterCodes::_0 && ch <= CharacterCodes::_9
}

fn is_hex_digit(ch: char) -> bool {
    is_digit(ch)
        || ch >= CharacterCodes::A && ch <= CharacterCodes::F
        || ch >= CharacterCodes::a && ch <= CharacterCodes::f
}

fn is_code_point(code: u32) -> bool {
    code <= 0x10ffff
}

pub fn skip_trivia(text: &str, pos: isize) -> isize {
    if position_is_synthesized(pos) {
        return pos;
    }

    let mut pos = usize::try_from(pos).unwrap();

    loop {
        let ch = text.chars().nth(pos);
        if matches!(ch, None) {
            return isize::try_from(pos).unwrap();
        }
        let ch = ch.unwrap();
        match ch {
            CharacterCodes::line_feed => {
                pos += 1;
                continue;
            }
            CharacterCodes::space => {
                pos += 1;
                continue;
            }
            _ => (),
        }
        return isize::try_from(pos).unwrap();
    }
}

fn is_identifier_start(ch: char) -> bool {
    ch >= CharacterCodes::A && ch <= CharacterCodes::Z
        || ch >= CharacterCodes::a && ch <= CharacterCodes::z
        || ch == CharacterCodes::dollar_sign
        || ch == CharacterCodes::underscore
        || ch > CharacterCodes::max_ascii_character && is_unicode_identifier_start(ch)
}

fn is_identifier_part(ch: char) -> bool {
    ch >= CharacterCodes::A && ch <= CharacterCodes::Z
        || ch >= CharacterCodes::a && ch <= CharacterCodes::z
        || ch >= CharacterCodes::_0 && ch <= CharacterCodes::_9
        || ch == CharacterCodes::dollar_sign
        || ch == CharacterCodes::underscore
        || ch > CharacterCodes::max_ascii_character && is_unicode_identifier_start(ch)
}

pub fn is_identifier_text(name: &str) -> bool {
    let ch = code_point_at(name, 0);
    if !is_identifier_start(ch) {
        return false;
    }
    for ch in name.chars().skip(1) {
        if !is_identifier_part(ch) {
            return false;
        }
    }

    true
}

struct ScanNumberReturn {
    type_: SyntaxKind,
    value: String,
}

pub struct Scanner /*<'on_error>*/ {
    skip_trivia: bool,
    // on_error: Option<ErrorCallback<'on_error>>,
    text: Option<String>,
    pos: RefCell<Option<usize>>,
    end: Option<usize>,
    start_pos: RefCell<Option<usize>>,
    token_pos: RefCell<Option<usize>>,
    token: RefCell<Option<SyntaxKind>>,
    token_value: RefCell<Option<String>>,
    token_flags: RefCell<Option<TokenFlags>>,
}

impl Scanner {
    pub fn get_start_pos(&self) -> usize {
        self.start_pos()
    }

    pub fn get_text_pos(&self) -> usize {
        self.pos()
    }

    pub fn get_token_pos(&self) -> usize {
        self.token_pos()
    }

    pub fn get_token_text(&self) -> String {
        self.text_substring(self.token_pos(), self.pos())
    }

    pub fn get_token_value(&self) -> String {
        self.token_value()
    }

    pub fn has_extended_unicode_escape(&self) -> bool {
        self.token_flags()
            .intersects(TokenFlags::ExtendedUnicodeEscape)
    }

    pub fn is_unterminated(&self) -> bool {
        self.token_flags().intersects(TokenFlags::Unterminated)
    }

    pub fn has_preceding_line_break(&self) -> bool {
        self.token_flags()
            .intersects(TokenFlags::PrecedingLineBreak)
    }

    pub fn get_numeric_literal_flags(&self) -> TokenFlags {
        self.token_flags() & TokenFlags::NumericLiteralFlags
    }

    pub fn get_token_flags(&self) -> TokenFlags {
        self.token_flags()
    }

    fn get_identifier_token(&self) -> SyntaxKind {
        let len = self.token_value().len();
        if len >= 2 && len <= 12 {
            let ch = self.token_value().chars().nth(0).unwrap();
            if ch >= CharacterCodes::a && ch <= CharacterCodes::z {
                let keyword = text_to_keyword.get(&self.token_value());
                if let Some(keyword) = keyword {
                    return self.set_token(*keyword);
                }
            }
        }
        self.set_token(SyntaxKind::Identifier)
    }

    pub fn scan(&self, on_error: Option<ErrorCallback>) -> SyntaxKind {
        self.set_start_pos(self.pos());
        self.set_token_flags(TokenFlags::None);

        loop {
            self.set_token_pos(self.pos());
            if self.pos() >= self.end() {
                return self.set_token(SyntaxKind::EndOfFileToken);
            }
            let ch = code_point_at(self.text(), self.pos());

            match ch {
                CharacterCodes::line_feed => {
                    self.add_token_flag(TokenFlags::PrecedingLineBreak);
                    if self.skip_trivia {
                        self.increment_pos();
                        continue;
                    } else {
                        unimplemented!()
                    }
                }
                CharacterCodes::space => {
                    if self.skip_trivia {
                        self.increment_pos();
                        continue;
                    }
                }
                CharacterCodes::double_quote | CharacterCodes::single_quote => {
                    self.set_token_value(self.scan_string(on_error, None));
                    return self.set_token(SyntaxKind::StringLiteral);
                }
                CharacterCodes::backtick => {
                    return self.set_token(self.scan_template_and_set_token_value(on_error, false));
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
                    self.increment_pos();
                    return self.set_token(SyntaxKind::AsteriskToken);
                }
                CharacterCodes::plus => {
                    if let Some(next_char) = self.text().chars().nth(self.pos() + 1) {
                        if next_char == CharacterCodes::plus {
                            self.increment_pos_by(2);
                            return self.set_token(SyntaxKind::PlusPlusToken);
                        }
                        unimplemented!();
                    }
                    unimplemented!();
                }
                CharacterCodes::comma => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::CommaToken);
                }
                CharacterCodes::slash => {
                    if self.text_char_at_index(self.pos() + 1) == CharacterCodes::slash {
                        self.increment_pos_by(2);

                        while self.pos() < self.end() {
                            if is_line_break(self.text_char_at_index(self.pos())) {
                                break;
                            }
                            self.increment_pos();
                        }

                        if self.skip_trivia {
                            continue;
                        } else {
                            unimplemented!()
                        }
                    }

                    if self.text_char_at_index(self.pos() + 1) == CharacterCodes::asterisk {
                        self.increment_pos_by(2);
                        if self.text_char_at_index(self.pos()) == CharacterCodes::asterisk
                            && self.text_char_at_index(self.pos() + 1) != CharacterCodes::slash
                        {
                            self.add_token_flag(TokenFlags::PrecedingJSDocComment);
                        }

                        let mut comment_closed = false;
                        let mut last_line_start = self.token_pos();
                        while self.pos() < self.end() {
                            let ch = self.text_char_at_index(self.pos());

                            if ch == CharacterCodes::asterisk
                                && self.text_char_at_index(self.pos() + 1) == CharacterCodes::slash
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

                        if !comment_closed {
                            self.error(on_error, &Diagnostics::Asterisk_Slash_expected, None, None);
                        }

                        if self.skip_trivia {
                            continue;
                        } else {
                            unimplemented!()
                        }
                    }

                    self.increment_pos();
                    return self.set_token(SyntaxKind::SlashToken);
                }
                CharacterCodes::_0
                | CharacterCodes::_1
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
                    } = self.scan_number();
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
                    self.increment_pos();
                    return self.set_token(SyntaxKind::LessThanToken);
                }
                CharacterCodes::greater_than => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::GreaterThanToken);
                }
                CharacterCodes::equals => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::EqualsToken);
                }
                CharacterCodes::open_bracket => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::OpenBracketToken);
                }
                CharacterCodes::close_bracket => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::CloseBracketToken);
                }
                CharacterCodes::open_brace => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::OpenBraceToken);
                }
                CharacterCodes::bar => {
                    self.increment_pos();
                    return self.set_token(SyntaxKind::BarToken);
                }
                CharacterCodes::close_brace => {
                    self.set_pos(self.pos() + 1);
                    return self.set_token(SyntaxKind::CloseBraceToken);
                }
                _ch => {
                    let identifier_kind = self.scan_identifier(ch);
                    if let Some(identifier_kind) = identifier_kind {
                        return self.set_token(identifier_kind);
                    }
                    unimplemented!()
                }
            }
        }
    }

    fn scan_identifier(&self, start_character: char) -> Option<SyntaxKind> {
        let mut ch = start_character;
        if is_identifier_start(ch) {
            self.set_pos(self.pos() + char_size(ch));
            loop {
                if !(self.pos() < self.end()) {
                    break;
                }
                ch = code_point_at(self.text(), self.pos());
                if !is_identifier_part(ch) {
                    break;
                }
                self.set_pos(self.pos() + char_size(ch));
            }
            self.set_token_value(
                self.text()
                    .chars()
                    .skip(self.token_pos())
                    .take(self.pos() - self.token_pos())
                    .collect::<String>(),
            );
            return Some(self.get_identifier_token());
        }
        None
    }

    pub fn re_scan_less_than_token(&self) -> SyntaxKind {
        if self.token() == SyntaxKind::LessThanLessThanToken {
            unimplemented!()
        }
        self.token()
    }

    pub fn set_text(
        &mut self,
        new_text: Option<&str>,
        start: Option<usize>,
        length: Option<usize>,
    ) {
        let text = new_text.unwrap_or("");
        self.set_text_(text);
        self.set_end(length.map_or(text.len(), |length| start.unwrap() + length));
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

    fn new(skip_trivia: bool) -> Self {
        Scanner {
            skip_trivia,
            // on_error: None,
            text: None,
            pos: RefCell::new(None),
            end: None,
            start_pos: RefCell::new(None),
            token_pos: RefCell::new(None),
            token: RefCell::new(None),
            token_value: RefCell::new(None),
            token_flags: RefCell::new(None),
        }
    }

    fn text(&self) -> &str {
        self.text.as_ref().unwrap()
    }

    fn set_text_(&mut self, text: &str) {
        self.text = Some(text.to_string());
    }

    fn text_char_at_index(&self, index: usize) -> char {
        self.text().chars().nth(index).unwrap()
    }

    fn text_substring(&self, start: usize, end: usize) -> String {
        self.text().chars().skip(start).take(end - start).collect()
    }

    fn pos(&self) -> usize {
        self.pos.borrow().unwrap()
    }

    fn set_pos(&self, pos: usize) {
        *self.pos.borrow_mut() = Some(pos);
    }

    fn increment_pos(&self) {
        self.set_pos(self.pos() + 1);
    }

    fn increment_pos_by(&self, by: usize) {
        self.set_pos(self.pos() + by);
    }

    fn end(&self) -> usize {
        self.end.unwrap()
    }

    fn set_end(&mut self, end: usize) {
        self.end = Some(end);
    }

    fn start_pos(&self) -> usize {
        self.start_pos.borrow().unwrap()
    }

    fn set_start_pos(&self, start_pos: usize) {
        *self.start_pos.borrow_mut() = Some(start_pos);
    }

    fn token_pos(&self) -> usize {
        self.token_pos.borrow().unwrap()
    }

    fn set_token_pos(&self, token_pos: usize) {
        *self.token_pos.borrow_mut() = Some(token_pos);
    }

    fn token(&self) -> SyntaxKind {
        self.token.borrow().unwrap()
    }

    fn set_token(&self, token: SyntaxKind) -> SyntaxKind {
        *self.token.borrow_mut() = Some(token);
        token
    }

    fn token_value(&self) -> String {
        self.token_value.borrow().as_ref().unwrap().to_string()
    }

    fn set_token_value(&self, token_value: String) {
        *self.token_value.borrow_mut() = Some(token_value);
    }

    fn token_flags(&self) -> TokenFlags {
        self.token_flags.borrow().unwrap()
    }

    fn set_token_flags(&self, token_flags: TokenFlags) {
        *self.token_flags.borrow_mut() = Some(token_flags);
    }

    fn add_token_flag(&self, flag: TokenFlags) {
        self.set_token_flags(self.token_flags() | flag);
    }

    fn error(
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

    fn scan_number_fragment(&self) -> String {
        let start = self.pos();
        let result = "".to_string();
        loop {
            let ch = self.text().chars().nth(self.pos());
            let ch = match ch {
                Some(ch) => ch,
                None => break,
            };
            if is_digit(ch) {
                self.set_pos(self.pos() + 1);
                continue;
            }
            break;
        }
        let mut ret = result;
        ret.push_str(
            &self
                .text()
                .chars()
                .skip(start)
                .take(self.pos() - start)
                .collect::<String>(),
        );
        ret
    }

    fn scan_number(&self) -> ScanNumberReturn {
        let start = self.pos();
        let main_fragment = self.scan_number_fragment();
        let end = self.pos();
        let result: String = self.text_substring(start, end);

        if false {
            unimplemented!()
        } else {
            self.set_token_value(result);
            let type_ = self.check_big_int_suffix();
            ScanNumberReturn {
                type_,
                value: self.token_value().to_string(),
            }
        }
    }

    fn scan_exact_number_of_hex_digits(
        &self,
        on_error: Option<ErrorCallback>,
        count: usize,
        can_have_separators: bool,
    ) -> Result<u32, String> {
        let value_string = self.scan_hex_digits(on_error, count, false, can_have_separators);
        if !value_string.is_empty() {
            hex_digits_to_u32(&value_string)
        } else {
            Err("Couldn't scan any hex digits".to_string())
        }
    }

    fn scan_minimum_number_of_hex_digits(
        &self,
        on_error: Option<ErrorCallback>,
        count: usize,
        can_have_separators: bool,
    ) -> String {
        self.scan_hex_digits(on_error, count, true, can_have_separators)
    }

    fn scan_hex_digits(
        &self,
        on_error: Option<ErrorCallback>,
        min_count: usize,
        scan_as_many_as_possible: bool,
        can_have_separators: bool,
    ) -> String {
        let mut value_chars: Vec<char> = vec![];
        let mut allow_separator = false;
        let mut is_previous_token_separator = false;
        while value_chars.len() < min_count || scan_as_many_as_possible {
            let mut ch = self.text_char_at_index(self.pos());
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
                ch = match ch {
                    CharacterCodes::A => CharacterCodes::a,
                    CharacterCodes::B => CharacterCodes::b,
                    CharacterCodes::C => CharacterCodes::c,
                    CharacterCodes::D => CharacterCodes::d,
                    CharacterCodes::E => CharacterCodes::e,
                    CharacterCodes::F => CharacterCodes::f,
                    _ => panic!("Expected uppercase hex digit"),
                };
            } else if !(ch >= CharacterCodes::_0 && ch <= CharacterCodes::_9
                || ch >= CharacterCodes::a && ch <= CharacterCodes::f)
            {
                break;
            }
            value_chars.push(ch);
            self.increment_pos();
            is_previous_token_separator = false;
        }
        if value_chars.len() < min_count {
            value_chars = vec![];
        }
        if self.text_char_at_index(self.pos() - 1) == CharacterCodes::underscore {
            self.error(
                on_error,
                &Diagnostics::Numeric_separators_are_not_allowed_here,
                Some(self.pos() - 1),
                Some(1),
            );
        }
        value_chars.into_iter().collect()
    }

    fn scan_string(
        &self,
        on_error: Option<ErrorCallback>,
        jsx_attribute_string: Option<bool>,
    ) -> String {
        let jsx_attribute_string = jsx_attribute_string.unwrap_or(false);
        let quote = self.text_char_at_index(self.pos());
        self.increment_pos();
        let mut result = String::new();
        let start = self.pos();
        loop {
            if self.pos() >= self.end() {
                result.push_str(&self.text_substring(start, self.pos()));
                self.add_token_flag(TokenFlags::Unterminated);
                self.error(
                    on_error,
                    &Diagnostics::Unterminated_string_literal,
                    None,
                    None,
                );
                break;
            }
            let ch = self.text_char_at_index(self.pos());
            if ch == quote {
                result.push_str(&self.text_substring(start, self.pos()));
                self.increment_pos();
                break;
            }
            if ch == CharacterCodes::backslash && !jsx_attribute_string {
                unimplemented!()
            }
            if is_line_break(ch) && !jsx_attribute_string {
                unimplemented!()
            }
            self.increment_pos();
        }
        result
    }

    fn scan_template_and_set_token_value(
        &self,
        on_error: Option<ErrorCallback>,
        is_tagged_template: bool,
    ) -> SyntaxKind {
        let started_with_backtick = self.text_char_at_index(self.pos()) == CharacterCodes::backtick;

        self.increment_pos();
        let mut start = self.pos();
        let mut contents = String::new();
        let resulting_token: SyntaxKind;

        loop {
            if self.pos() >= self.end() {
                contents.push_str(&self.text_substring(start, self.pos()));
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
                break;
            }

            let curr_char = self.text_char_at_index(self.pos());

            if curr_char == CharacterCodes::backtick {
                contents.push_str(&self.text_substring(start, self.pos()));
                self.increment_pos();
                resulting_token = if started_with_backtick {
                    SyntaxKind::NoSubstitutionTemplateLiteral
                } else {
                    SyntaxKind::TemplateTail
                };
                break;
            }

            if curr_char == CharacterCodes::dollar_sign
                && self.pos() + 1 < self.end()
                && self.text_char_at_index(self.pos() + 1) == CharacterCodes::open_brace
            {
                contents.push_str(&self.text_substring(start, self.pos()));
                self.increment_pos_by(2);
                resulting_token = if started_with_backtick {
                    SyntaxKind::TemplateHead
                } else {
                    SyntaxKind::TemplateMiddle
                };
                break;
            }

            if curr_char == CharacterCodes::backslash {
                contents.push_str(&self.text_substring(start, self.pos()));
                contents.push_str(&self.scan_escape_sequence(on_error, Some(is_tagged_template)));
                start = self.pos();
                continue;
            }

            if curr_char == CharacterCodes::carriage_return {
                contents.push_str(&self.text_substring(start, self.pos()));
                self.increment_pos();

                if self.pos() < self.end()
                    && self.text_char_at_index(self.pos()) == CharacterCodes::line_feed
                {
                    self.increment_pos();
                }

                contents.push_str("\n");
                start = self.pos();
                continue;
            }

            self.increment_pos();
        }

        // Debug.assert(resultingToken !== undefined);

        self.set_token_value(contents);
        resulting_token
    }

    fn scan_escape_sequence(
        &self,
        on_error: Option<ErrorCallback>,
        is_tagged_template: Option<bool>,
    ) -> String {
        let is_tagged_template = is_tagged_template.unwrap_or(false);
        let start = self.pos();
        self.increment_pos();
        if self.pos() >= self.end() {
            self.error(on_error, &Diagnostics::Unexpected_end_of_text, None, None);
            return "".to_string();
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
                    return self.text_substring(start, self.pos());
                }
                "\0".to_string()
            }
            CharacterCodes::b => "\u{0008}".to_string(),
            CharacterCodes::t => "\t".to_string(),
            CharacterCodes::n => "\n".to_string(),
            CharacterCodes::v => "\u{000b}".to_string(),
            CharacterCodes::f => "\u{000c}".to_string(),
            CharacterCodes::r => "\r".to_string(),
            CharacterCodes::single_quote => "'".to_string(),
            CharacterCodes::double_quote => "\"".to_string(),
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
                            return self.text_substring(start, self.pos());
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
                        return self.text_substring(start, self.pos());
                    }

                    if is_tagged_template {
                        let save_pos = self.pos();
                        let escaped_value_string =
                            self.scan_minimum_number_of_hex_digits(on_error, 1, false);
                        let is_escaped_value_empty = escaped_value_string.is_empty();
                        let escaped_value = if !is_escaped_value_empty {
                            hex_digits_to_u32(&escaped_value_string)
                        } else {
                            Err("Couldn't scan any hex digits".to_string())
                        };

                        if !match (is_escaped_value_empty, escaped_value) {
                            (true, _) => true,
                            (_, Err(_)) => false,
                            (_, Ok(escaped_value)) => is_code_point(escaped_value),
                        } || self.text_char_at_index(self.pos()) != CharacterCodes::close_brace
                        {
                            self.add_token_flag(TokenFlags::ContainsInvalidEscape);
                            return self.text_substring(start, self.pos());
                        } else {
                            self.set_pos(save_pos);
                        }
                    }
                    self.add_token_flag(TokenFlags::ExtendedUnicodeEscape);
                    return self.scan_extended_unicode_escape(on_error);
                }

                self.add_token_flag(TokenFlags::UnicodeEscape);
                self.scan_hexadecimal_escape(on_error, 4)
            }
            CharacterCodes::x => {
                if is_tagged_template {
                    if !is_hex_digit(self.text_char_at_index(self.pos())) {
                        self.add_token_flag(TokenFlags::ContainsInvalidEscape);
                        return self.text_substring(start, self.pos());
                    } else if !is_hex_digit(self.text_char_at_index(self.pos() + 1)) {
                        self.increment_pos();
                        self.add_token_flag(TokenFlags::ContainsInvalidEscape);
                        return self.text_substring(start, self.pos());
                    }
                }

                self.scan_hexadecimal_escape(on_error, 2)
            }
            CharacterCodes::carriage_return => {
                if self.pos() < self.end()
                    && self.text_char_at_index(self.pos()) == CharacterCodes::line_feed
                {
                    self.increment_pos();
                }
                "".to_string()
            }
            CharacterCodes::line_feed
            | CharacterCodes::line_separator
            | CharacterCodes::paragraph_separator => "".to_string(),
            _ => ch.to_string(),
        }
    }

    fn scan_hexadecimal_escape(
        &self,
        on_error: Option<ErrorCallback>,
        num_digits: usize,
    ) -> String {
        let escaped_value = self.scan_exact_number_of_hex_digits(on_error, num_digits, false);

        match escaped_value {
            Ok(escaped_value) => escaped_value.to_string(),
            Err(_) => {
                self.error(
                    on_error,
                    &Diagnostics::Hexadecimal_digit_expected,
                    None,
                    None,
                );
                "".to_string()
            }
        }
    }

    fn scan_extended_unicode_escape(&self, on_error: Option<ErrorCallback>) -> String {
        let escaped_value_string = self.scan_minimum_number_of_hex_digits(on_error, 1, false);
        let escaped_value = if !escaped_value_string.is_empty() {
            hex_digits_to_u32(&escaped_value_string)
        } else {
            Err("Couldn't scan any hex digits".to_string())
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
            return "".to_string();
        }

        utf16_encode_as_string(escaped_value.unwrap())
    }

    fn check_big_int_suffix(&self) -> SyntaxKind {
        if self.text_char_at_index(self.pos()) == CharacterCodes::n {
            self.set_token_value(format!("{}n", self.token_value()));
            self.increment_pos();
            SyntaxKind::BigIntLiteral
        } else {
            SyntaxKind::NumericLiteral
        }
    }

    fn speculation_helper<TReturn, TCallback: FnOnce() -> Option<TReturn>>(
        &self,
        mut callback: TCallback,
        is_lookahead: bool,
    ) -> Option<TReturn> {
        let save_pos = self.pos();
        let save_start_pos = self.start_pos();
        let save_token_pos = self.token_pos();
        let save_token = self.token();
        let save_token_value = self.token_value().to_string();
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
}

pub fn create_scanner(skip_trivia: bool) -> Scanner {
    Scanner::new(skip_trivia)
}

fn code_point_at(s: &str, i: usize) -> char {
    s.chars().nth(i).unwrap()
}

fn char_size(ch: char) -> usize {
    1
}

fn utf16_encode_as_string(code_point: u32) -> String {
    char::from_u32(code_point).unwrap().to_string()
}

fn hex_digits_to_u32(str: &str) -> Result<u32, String> {
    u32::from_str_radix(str, 16).map_err(|_| "Couldn't convert hex digits to u32".to_string())
}
