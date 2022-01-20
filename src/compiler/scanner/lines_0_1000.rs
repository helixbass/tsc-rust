#![allow(non_upper_case_globals)]

use std::array::IntoIter;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::iter::FromIterator;

use super::code_point_at;
use crate::{position_is_synthesized, CharacterCodes, DiagnosticMessage, SyntaxKind, TokenFlags};

pub type ErrorCallback<'callback> = &'callback dyn Fn(&DiagnosticMessage, usize);

pub fn token_is_identifier_or_keyword(token: SyntaxKind) -> bool {
    token >= SyntaxKind::Identifier
}

pub(crate) fn token_is_identifier_or_keyword_or_greater_than(token: SyntaxKind) -> bool {
    token == SyntaxKind::GreaterThanToken || token_is_identifier_or_keyword(token)
}

lazy_static! {
    pub(super) static ref text_to_keyword_obj: HashMap<String, SyntaxKind> =
        HashMap::from_iter(IntoIter::new([
            ("abstract".to_string(), SyntaxKind::AbstractKeyword),
            ("any".to_string(), SyntaxKind::AnyKeyword),
            ("as".to_string(), SyntaxKind::AsKeyword),
            ("asserts".to_string(), SyntaxKind::AssertsKeyword),
            ("assert".to_string(), SyntaxKind::AssertKeyword),
            ("bigint".to_string(), SyntaxKind::BigIntKeyword),
            ("boolean".to_string(), SyntaxKind::BooleanKeyword),
            ("break".to_string(), SyntaxKind::BreakKeyword),
            ("case".to_string(), SyntaxKind::CaseKeyword),
            ("catch".to_string(), SyntaxKind::CatchKeyword),
            ("class".to_string(), SyntaxKind::ClassKeyword),
            ("continue".to_string(), SyntaxKind::ContinueKeyword),
            ("const".to_string(), SyntaxKind::ConstKeyword),
            ("constructor".to_string(), SyntaxKind::ConstructorKeyword),
            ("debugger".to_string(), SyntaxKind::DebuggerKeyword),
            ("declare".to_string(), SyntaxKind::DeclareKeyword),
            ("default".to_string(), SyntaxKind::DefaultKeyword),
            ("delete".to_string(), SyntaxKind::DeleteKeyword),
            ("do".to_string(), SyntaxKind::DoKeyword),
            ("else".to_string(), SyntaxKind::ElseKeyword),
            ("enum".to_string(), SyntaxKind::EnumKeyword),
            ("export".to_string(), SyntaxKind::ExportKeyword),
            ("extends".to_string(), SyntaxKind::ExtendsKeyword),
            ("false".to_string(), SyntaxKind::FalseKeyword),
            ("finally".to_string(), SyntaxKind::FinallyKeyword),
            ("for".to_string(), SyntaxKind::ForKeyword),
            ("from".to_string(), SyntaxKind::FromKeyword),
            ("function".to_string(), SyntaxKind::FunctionKeyword),
            ("get".to_string(), SyntaxKind::GetKeyword),
            ("if".to_string(), SyntaxKind::IfKeyword),
            ("implements".to_string(), SyntaxKind::ImplementsKeyword),
            ("import".to_string(), SyntaxKind::ImportKeyword),
            ("in".to_string(), SyntaxKind::InKeyword),
            ("infer".to_string(), SyntaxKind::InferKeyword),
            ("instanceof".to_string(), SyntaxKind::InstanceOfKeyword),
            ("interface".to_string(), SyntaxKind::InterfaceKeyword),
            ("intrinsic".to_string(), SyntaxKind::IntrinsicKeyword),
            ("is".to_string(), SyntaxKind::IsKeyword),
            ("keyof".to_string(), SyntaxKind::KeyOfKeyword),
            ("let".to_string(), SyntaxKind::LetKeyword),
            ("module".to_string(), SyntaxKind::ModuleKeyword),
            ("namespace".to_string(), SyntaxKind::NamespaceKeyword),
            ("never".to_string(), SyntaxKind::NeverKeyword),
            ("new".to_string(), SyntaxKind::NewKeyword),
            ("null".to_string(), SyntaxKind::NullKeyword),
            ("number".to_string(), SyntaxKind::NumberKeyword),
            ("object".to_string(), SyntaxKind::ObjectKeyword),
            ("package".to_string(), SyntaxKind::PackageKeyword),
            ("private".to_string(), SyntaxKind::PrivateKeyword),
            ("protected".to_string(), SyntaxKind::ProtectedKeyword),
            ("public".to_string(), SyntaxKind::PublicKeyword),
            ("override".to_string(), SyntaxKind::OverrideKeyword),
            ("readonly".to_string(), SyntaxKind::ReadonlyKeyword),
            ("require".to_string(), SyntaxKind::RequireKeyword),
            ("global".to_string(), SyntaxKind::GlobalKeyword),
            ("return".to_string(), SyntaxKind::ReturnKeyword),
            ("set".to_string(), SyntaxKind::SetKeyword),
            ("static".to_string(), SyntaxKind::StaticKeyword),
            ("string".to_string(), SyntaxKind::StringKeyword),
            ("super".to_string(), SyntaxKind::SuperKeyword),
            ("switch".to_string(), SyntaxKind::SwitchKeyword),
            ("symbol".to_string(), SyntaxKind::SymbolKeyword),
            ("this".to_string(), SyntaxKind::ThisKeyword),
            ("throw".to_string(), SyntaxKind::ThrowKeyword),
            ("true".to_string(), SyntaxKind::TrueKeyword),
            ("try".to_string(), SyntaxKind::TryKeyword),
            ("type".to_string(), SyntaxKind::TypeKeyword),
            ("typeof".to_string(), SyntaxKind::TypeOfKeyword),
            ("undefined".to_string(), SyntaxKind::UndefinedKeyword),
            ("unique".to_string(), SyntaxKind::UniqueKeyword),
            ("unknown".to_string(), SyntaxKind::UnknownKeyword),
            ("var".to_string(), SyntaxKind::VarKeyword),
            ("void".to_string(), SyntaxKind::VoidKeyword),
            ("while".to_string(), SyntaxKind::WhileKeyword),
            ("with".to_string(), SyntaxKind::WithKeyword),
            ("yield".to_string(), SyntaxKind::YieldKeyword),
            ("async".to_string(), SyntaxKind::AsyncKeyword),
            ("await".to_string(), SyntaxKind::AwaitKeyword),
            ("of".to_string(), SyntaxKind::OfKeyword),
        ]));
}

lazy_static! {
    pub(super) static ref text_to_keyword: HashMap<String, SyntaxKind> =
        text_to_keyword_obj.clone();
}

lazy_static! {
    pub(super) static ref text_to_token: HashMap<String, SyntaxKind> = {
        let mut ret = text_to_keyword_obj.clone();
        ret.extend(IntoIter::new([(
            ";".to_string(),
            SyntaxKind::SemicolonToken,
        )]));
        ret
    };
}

pub(super) fn is_unicode_identifier_start(ch: char) -> bool {
    false
}

pub(super) fn make_reverse_map(
    source: &HashMap<String, SyntaxKind>,
) -> HashMap<SyntaxKind, String> {
    let mut result = HashMap::new();
    for (key, val) in source.iter() {
        result.insert(*val, key.clone());
    }
    result
}

lazy_static! {
    pub(super) static ref token_strings: HashMap<SyntaxKind, String> =
        make_reverse_map(&text_to_token);
}

pub fn token_to_string(t: SyntaxKind) -> Option<&'static String> {
    token_strings.get(&t)
}

pub(super) fn is_line_break(ch: char) -> bool {
    ch == CharacterCodes::line_feed
        || ch == CharacterCodes::carriage_return
        || ch == CharacterCodes::line_separator
        || ch == CharacterCodes::paragraph_separator
}

pub(super) fn is_digit(ch: char) -> bool {
    ch >= CharacterCodes::_0 && ch <= CharacterCodes::_9
}

pub(super) fn is_hex_digit(ch: char) -> bool {
    is_digit(ch)
        || ch >= CharacterCodes::A && ch <= CharacterCodes::F
        || ch >= CharacterCodes::a && ch <= CharacterCodes::f
}

pub(super) fn is_code_point(code: u32) -> bool {
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

pub(super) fn is_identifier_start(ch: char) -> bool {
    ch >= CharacterCodes::A && ch <= CharacterCodes::Z
        || ch >= CharacterCodes::a && ch <= CharacterCodes::z
        || ch == CharacterCodes::dollar_sign
        || ch == CharacterCodes::underscore
        || ch > CharacterCodes::max_ascii_character && is_unicode_identifier_start(ch)
}

pub(super) fn is_identifier_part(ch: char) -> bool {
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

pub fn create_scanner(skip_trivia: bool) -> Scanner {
    Scanner::new(skip_trivia)
}

pub struct Scanner /*<'on_error>*/ {
    pub(super) skip_trivia: bool,
    // on_error: Option<ErrorCallback<'on_error>>,
    pub(super) text: Option<String>,
    pub(super) pos: RefCell<Option<usize>>,
    pub(super) end: Option<usize>,
    pub(super) start_pos: RefCell<Option<usize>>,
    pub(super) token_pos: RefCell<Option<usize>>,
    pub(super) token: RefCell<Option<SyntaxKind>>,
    pub(super) token_value: RefCell<Option<String>>,
    pub(super) token_flags: RefCell<Option<TokenFlags>>,
}

impl Scanner {
    pub(super) fn new(skip_trivia: bool) -> Self {
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

    pub(super) fn text(&self) -> &str {
        self.text.as_ref().unwrap()
    }

    pub(super) fn set_text_(&mut self, text: &str) {
        self.text = Some(text.to_string());
    }

    pub(super) fn text_char_at_index(&self, index: usize) -> char {
        self.text().chars().nth(index).unwrap()
    }

    pub(super) fn text_substring(&self, start: usize, end: usize) -> String {
        self.text().chars().skip(start).take(end - start).collect()
    }

    pub(super) fn pos(&self) -> usize {
        self.pos.borrow().unwrap()
    }

    pub(super) fn set_pos(&self, pos: usize) {
        *self.pos.borrow_mut() = Some(pos);
    }

    pub(super) fn increment_pos(&self) {
        self.set_pos(self.pos() + 1);
    }

    pub(super) fn increment_pos_by(&self, by: usize) {
        self.set_pos(self.pos() + by);
    }

    pub(super) fn end(&self) -> usize {
        self.end.unwrap()
    }

    pub(super) fn set_end(&mut self, end: usize) {
        self.end = Some(end);
    }

    pub(super) fn start_pos(&self) -> usize {
        self.start_pos.borrow().unwrap()
    }

    pub(super) fn set_start_pos(&self, start_pos: usize) {
        *self.start_pos.borrow_mut() = Some(start_pos);
    }

    pub(super) fn token_pos(&self) -> usize {
        self.token_pos.borrow().unwrap()
    }

    pub(super) fn set_token_pos(&self, token_pos: usize) {
        *self.token_pos.borrow_mut() = Some(token_pos);
    }

    pub(super) fn token(&self) -> SyntaxKind {
        self.token.borrow().unwrap()
    }

    pub(super) fn set_token(&self, token: SyntaxKind) -> SyntaxKind {
        *self.token.borrow_mut() = Some(token);
        token
    }

    pub(super) fn token_value(&self) -> String {
        self.token_value.borrow().as_ref().unwrap().to_string()
    }

    pub(super) fn set_token_value(&self, token_value: String) {
        *self.token_value.borrow_mut() = Some(token_value);
    }

    pub(super) fn token_flags(&self) -> TokenFlags {
        self.token_flags.borrow().unwrap()
    }

    pub(super) fn set_token_flags(&self, token_flags: TokenFlags) {
        *self.token_flags.borrow_mut() = Some(token_flags);
    }

    pub(super) fn add_token_flag(&self, flag: TokenFlags) {
        self.set_token_flags(self.token_flags() | flag);
    }

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
}
