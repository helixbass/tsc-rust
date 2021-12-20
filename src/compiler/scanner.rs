#![allow(non_upper_case_globals)]

use std::array::IntoIter;
use std::collections::HashMap;
use std::iter::FromIterator;

use crate::{CharacterCodes, SyntaxKind};

lazy_static! {
    static ref text_to_keyword_obj: HashMap<&'static str, SyntaxKind> =
        HashMap::from_iter(IntoIter::new([
            ("false", SyntaxKind::FalseKeyword),
            ("true", SyntaxKind::TrueKeyword),
        ]));
}

lazy_static! {
    static ref text_to_keyword: HashMap<&'static str, SyntaxKind> = text_to_keyword_obj.clone();
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
    pos: Option<usize>,
    end: Option<usize>,
    start_pos: Option<usize>,
    token_pos: Option<usize>,
    token: Option<SyntaxKind>,
    token_value: Option<String>,
}

impl Scanner {
    pub fn get_text_pos(&self) -> usize {
        self.pos()
    }

    pub fn get_token_pos(&self) -> usize {
        self.token_pos()
    }

    pub fn get_token_value(&self) -> &str {
        self.token_value()
    }

    fn get_identifier_token(&mut self) -> SyntaxKind {
        let len = self.token_value().len();
        if len >= 2 && len <= 12 {
            let ch = self.token_value().chars().nth(0).unwrap();
            if ch >= CharacterCodes::a && ch <= CharacterCodes::z {
                let keyword = text_to_keyword.get(self.token_value());
                if let Some(keyword) = keyword {
                    return self.set_token(*keyword);
                }
            }
        }
        unimplemented!()
    }

    pub fn scan(&mut self) -> SyntaxKind {
        self.set_start_pos(self.pos());

        loop {
            self.set_token_pos(self.pos());
            if self.pos() >= self.end() {
                return self.set_token(SyntaxKind::EndOfFileToken);
            }
            let ch = code_point_at(self.text(), self.pos());

            match ch {
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
                CharacterCodes::semicolon => {
                    self.set_pos(self.pos() + 1);
                    return self.set_token(SyntaxKind::SemicolonToken);
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

    fn scan_identifier(&mut self, start_character: char) -> Option<SyntaxKind> {
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
            pos: None,
            end: None,
            start_pos: None,
            token_pos: None,
            token: None,
            token_value: None,
        }
    }

    fn text(&self) -> &str {
        self.text.as_ref().unwrap()
    }

    fn set_text_(&mut self, text: &str) {
        self.text = Some(text.to_string());
    }

    fn pos(&self) -> usize {
        self.pos.unwrap()
    }

    fn set_pos(&mut self, pos: usize) {
        self.pos = Some(pos);
    }

    fn end(&self) -> usize {
        self.end.unwrap()
    }

    fn set_end(&mut self, end: usize) {
        self.end = Some(end);
    }

    fn start_pos(&self) -> usize {
        self.start_pos.unwrap()
    }

    fn set_start_pos(&mut self, start_pos: usize) {
        self.start_pos = Some(start_pos);
    }

    fn token_pos(&self) -> usize {
        self.token_pos.unwrap()
    }

    fn set_token_pos(&mut self, token_pos: usize) {
        self.token_pos = Some(token_pos);
    }

    fn token(&self) -> SyntaxKind {
        self.token.unwrap()
    }

    fn set_token(&mut self, token: SyntaxKind) -> SyntaxKind {
        self.token = Some(token);
        token
    }

    fn token_value(&self) -> &str {
        self.token_value.as_ref().unwrap()
    }

    fn set_token_value(&mut self, token_value: &str) {
        self.token_value = Some(token_value.to_string());
    }

    fn is_digit(&self, ch: char) -> bool {
        ch >= CharacterCodes::_0 && ch <= CharacterCodes::_9
    }

    fn scan_number_fragment(&mut self) -> String {
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

    fn scan_number(&mut self) -> ScanNumberReturn {
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
