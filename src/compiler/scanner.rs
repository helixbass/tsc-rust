#![allow(non_upper_case_globals)]

use std::array::IntoIter;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::sync::RwLock;

use crate::{CharacterCodes, SyntaxKind, TokenFlags};

lazy_static! {
    static ref text_to_keyword_obj: HashMap<String, SyntaxKind> =
        HashMap::from_iter(IntoIter::new([
            ("const".to_string(), SyntaxKind::ConstKeyword),
            ("false".to_string(), SyntaxKind::FalseKeyword),
            ("number".to_string(), SyntaxKind::NumberKeyword),
            ("true".to_string(), SyntaxKind::TrueKeyword),
        ]));
}

lazy_static! {
    static ref text_to_keyword: HashMap<String, SyntaxKind> = text_to_keyword_obj.clone();
}

fn is_unicode_identifier_start(ch: char) -> bool {
    false
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

struct ScanNumberReturn {
    type_: SyntaxKind,
    value: String,
}

pub struct Scanner {
    skip_trivia: bool,
    text: Option<String>,
    pos: RwLock<Option<usize>>,
    end: Option<usize>,
    start_pos: RwLock<Option<usize>>,
    token_pos: RwLock<Option<usize>>,
    token: RwLock<Option<SyntaxKind>>,
    token_value: RwLock<Option<String>>,
    token_flags: RwLock<Option<TokenFlags>>,
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

    pub fn get_token_value(&self) -> String {
        self.token_value()
    }

    pub fn has_preceding_line_break(&self) -> bool {
        self.token_flags()
            .intersects(TokenFlags::PrecedingLineBreak)
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

    pub fn scan(&self) -> SyntaxKind {
        self.set_start_pos(self.pos());
        self.set_token_flags(TokenFlags::None);

        loop {
            self.set_token_pos(self.pos());
            if self.pos() >= self.end() {
                return self.set_token(SyntaxKind::EndOfFileToken);
            }
            let ch = code_point_at(self.text(), self.pos());

            match ch {
                CharacterCodes::lineFeed => {
                    self.set_token_flags(self.token_flags() | TokenFlags::PrecedingLineBreak);
                    if self.skip_trivia {
                        self.set_pos(self.pos() + 1);
                        continue;
                    } else {
                        unimplemented!()
                    }
                }
                CharacterCodes::space => {
                    if self.skip_trivia {
                        self.set_pos(self.pos() + 1);
                        continue;
                    }
                }
                CharacterCodes::asterisk => {
                    self.set_pos(self.pos() + 1);
                    return self.set_token(SyntaxKind::AsteriskToken);
                }
                CharacterCodes::plus => {
                    if let Some(next_char) = self.text().chars().nth(self.pos() + 1) {
                        if next_char == CharacterCodes::plus {
                            self.set_pos(self.pos() + 2);
                            return self.set_token(SyntaxKind::PlusPlusToken);
                        }
                        unimplemented!();
                    }
                    unimplemented!();
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
                    self.set_token_value(&token_value);
                    return token;
                }
                CharacterCodes::colon => {
                    self.set_pos(self.pos() + 1);
                    return self.set_token(SyntaxKind::ColonToken);
                }
                CharacterCodes::semicolon => {
                    self.set_pos(self.pos() + 1);
                    return self.set_token(SyntaxKind::SemicolonToken);
                }
                CharacterCodes::equals => {
                    self.set_pos(self.pos() + 1);
                    return self.set_token(SyntaxKind::EqualsToken);
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
                &self
                    .text()
                    .chars()
                    .skip(self.token_pos())
                    .take(self.pos() - self.token_pos())
                    .collect::<String>(),
            );
            return Some(self.get_identifier_token());
        }
        None
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
            text: None,
            pos: RwLock::new(None),
            end: None,
            start_pos: RwLock::new(None),
            token_pos: RwLock::new(None),
            token: RwLock::new(None),
            token_value: RwLock::new(None),
            token_flags: RwLock::new(None),
        }
    }

    fn text(&self) -> &str {
        self.text.as_ref().unwrap()
    }

    fn set_text_(&mut self, text: &str) {
        self.text = Some(text.to_string());
    }

    fn pos(&self) -> usize {
        self.pos.try_read().unwrap().unwrap()
    }

    fn set_pos(&self, pos: usize) {
        *self.pos.try_write().unwrap() = Some(pos);
    }

    fn end(&self) -> usize {
        self.end.unwrap()
    }

    fn set_end(&mut self, end: usize) {
        self.end = Some(end);
    }

    fn start_pos(&self) -> usize {
        self.start_pos.try_read().unwrap().unwrap()
    }

    fn set_start_pos(&self, start_pos: usize) {
        *self.start_pos.try_write().unwrap() = Some(start_pos);
    }

    fn token_pos(&self) -> usize {
        self.token_pos.try_read().unwrap().unwrap()
    }

    fn set_token_pos(&self, token_pos: usize) {
        *self.token_pos.try_write().unwrap() = Some(token_pos);
    }

    fn token(&self) -> SyntaxKind {
        self.token.try_read().unwrap().unwrap()
    }

    fn set_token(&self, token: SyntaxKind) -> SyntaxKind {
        *self.token.try_write().unwrap() = Some(token);
        token
    }

    fn token_value(&self) -> String {
        self.token_value
            .try_read()
            .unwrap()
            .as_ref()
            .unwrap()
            .to_string()
    }

    fn set_token_value(&self, token_value: &str) {
        *self.token_value.try_write().unwrap() = Some(token_value.to_string());
    }

    fn token_flags(&self) -> TokenFlags {
        self.token_flags.try_read().unwrap().unwrap()
    }

    fn set_token_flags(&self, token_flags: TokenFlags) {
        *self.token_flags.try_write().unwrap() = Some(token_flags);
    }

    fn is_digit(&self, ch: char) -> bool {
        ch >= CharacterCodes::_0 && ch <= CharacterCodes::_9
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
            if self.is_digit(ch) {
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
        let result: String = self.text().chars().skip(start).take(end - start).collect();

        self.set_token_value(&result);
        let type_ = self.check_big_int_suffix();
        ScanNumberReturn {
            type_,
            value: self.token_value().to_string(),
        }
    }

    fn check_big_int_suffix(&self) -> SyntaxKind {
        SyntaxKind::NumericLiteral
    }

    // fn scan_identifier(&self, start_character: char) -> {
    // }

    fn speculation_helper<TReturn, TCallback: FnMut() -> Option<TReturn>>(
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
            self.set_token_value(&save_token_value);
            self.set_token_flags(save_token_flags);
        }
        result
    }

    pub fn look_ahead<TReturn, TCallback: FnMut() -> Option<TReturn>>(
        &self,
        callback: TCallback,
    ) -> Option<TReturn> {
        self.speculation_helper(callback, true)
    }

    pub fn try_scan<TReturn, TCallback: FnMut() -> Option<TReturn>>(
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
