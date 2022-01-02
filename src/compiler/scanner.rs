#![allow(non_upper_case_globals)]

use std::array::IntoIter;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::iter::FromIterator;

use crate::{position_is_synthesized, CharacterCodes, SyntaxKind, TokenFlags};

pub fn token_is_identifier_or_keyword(token: SyntaxKind) -> bool {
    token >= SyntaxKind::Identifier
}

lazy_static! {
    static ref text_to_keyword_obj: HashMap<String, SyntaxKind> =
        HashMap::from_iter(IntoIter::new([
            ("boolean".to_string(), SyntaxKind::BooleanKeyword),
            ("const".to_string(), SyntaxKind::ConstKeyword),
            ("false".to_string(), SyntaxKind::FalseKeyword),
            ("interface".to_string(), SyntaxKind::InterfaceKeyword),
            ("number".to_string(), SyntaxKind::NumberKeyword),
            ("true".to_string(), SyntaxKind::TrueKeyword),
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

pub struct Scanner {
    skip_trivia: bool,
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
                CharacterCodes::line_feed => {
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
                CharacterCodes::comma => {
                    self.set_pos(self.pos() + 1);
                    return self.set_token(SyntaxKind::CommaToken);
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
                CharacterCodes::open_bracket => {
                    self.set_pos(self.pos() + 1);
                    return self.set_token(SyntaxKind::OpenBracketToken);
                }
                CharacterCodes::close_bracket => {
                    self.set_pos(self.pos() + 1);
                    return self.set_token(SyntaxKind::CloseBracketToken);
                }
                CharacterCodes::open_brace => {
                    self.set_pos(self.pos() + 1);
                    return self.set_token(SyntaxKind::OpenBraceToken);
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

    fn pos(&self) -> usize {
        self.pos.borrow().unwrap()
    }

    fn set_pos(&self, pos: usize) {
        *self.pos.borrow_mut() = Some(pos);
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

    fn set_token_value(&self, token_value: &str) {
        *self.token_value.borrow_mut() = Some(token_value.to_string());
    }

    fn token_flags(&self) -> TokenFlags {
        self.token_flags.borrow().unwrap()
    }

    fn set_token_flags(&self, token_flags: TokenFlags) {
        *self.token_flags.borrow_mut() = Some(token_flags);
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
