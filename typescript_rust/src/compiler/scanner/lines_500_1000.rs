#![allow(non_upper_case_globals)]

use regex::Regex;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::convert::{TryFrom, TryInto};
use std::rc::Rc;

use super::{
    is_line_break, is_unicode_identifier_part, is_unicode_identifier_start, is_white_space_like,
    maybe_code_point_at,
};
use crate::{
    position_is_synthesized, CharacterCodes, CharacterCodesChar, CommentDirective, CommentKind,
    CommentRange, Debug_, DiagnosticMessage, Diagnostics, LanguageVariant, ScriptTarget,
    SourceText, SourceTextSlice, SourceTextSliceOrString, SyntaxKind, TokenFlags,
};

pub(super) fn is_digit(ch: &str) -> bool {
    ch >= CharacterCodes::_0 && ch <= CharacterCodes::_9
}

pub(super) fn is_hex_digit(ch: &str) -> bool {
    is_digit(ch)
        || ch >= CharacterCodes::A && ch <= CharacterCodes::F
        || ch >= CharacterCodes::a && ch <= CharacterCodes::f
}

pub(super) fn is_code_point(code: u32) -> bool {
    code <= 0x10ffff
}

pub(crate) fn is_octal_digit(ch: &str) -> bool {
    ch >= CharacterCodes::_0 && ch <= CharacterCodes::_7
}

pub fn could_start_trivia(text: &SourceTextAsChars, pos: usize) -> bool {
    let ch = maybe_text_char_at_index(text, pos);
    match ch {
        None => false,
        Some(
            CharacterCodes::carriage_return
            | CharacterCodes::line_feed
            | CharacterCodes::tab
            | CharacterCodes::vertical_tab
            | CharacterCodes::form_feed
            | CharacterCodes::space
            | CharacterCodes::slash
            | CharacterCodes::less_than
            | CharacterCodes::bar
            | CharacterCodes::equals
            | CharacterCodes::greater_than,
        ) => true,
        Some(CharacterCodes::hash) => pos == 0,
        Some(ch) => ch > CharacterCodes::max_ascii_character,
    }
}

pub(crate) fn skip_trivia(
    ref text: Rc<SourceText>,
    pos: isize,
    stop_after_line_break: Option<bool>,
    stop_at_comments: Option<bool>,
    in_jsdoc: Option<bool>,
) -> isize {
    let stop_after_line_break = stop_after_line_break.unwrap_or(false);
    let stop_at_comments = stop_at_comments.unwrap_or(false);
    let in_jsdoc = in_jsdoc.unwrap_or(false);
    if position_is_synthesized(pos) {
        return pos;
    }

    let mut pos = usize::try_from(pos).unwrap();

    let mut can_consume_star = false;
    loop {
        let ch = text.maybe_char_at_index(pos);
        if ch.is_none() {
            return isize::try_from(pos).unwrap();
        }
        let ch = ch.unwrap();
        match ch {
            CharacterCodes::carriage_return => {
                if matches!(
                    text.maybe_char_at_index(pos + 1),
                    Some(CharacterCodes::line_feed)
                ) {
                    pos += 1;
                }
                pos += 1;
                if stop_after_line_break {
                    return pos.try_into().unwrap();
                }
                can_consume_star = in_jsdoc;
                continue;
            }
            CharacterCodes::line_feed => {
                pos += 1;
                if stop_after_line_break {
                    return pos.try_into().unwrap();
                }
                can_consume_star = in_jsdoc;
                continue;
            }
            CharacterCodes::tab
            | CharacterCodes::vertical_tab
            | CharacterCodes::form_feed
            | CharacterCodes::space => {
                pos += 1;
                continue;
            }
            CharacterCodes::slash => match stop_at_comments {
                true => (),
                false => {
                    if matches!(
                        text.maybe_char_at_index(pos + 1),
                        Some(CharacterCodes::slash)
                    ) {
                        pos += 2;
                        while pos < text_len(text) {
                            if is_line_break(text.char_at_index(pos)) {
                                break;
                            }
                            pos += 1;
                        }
                        can_consume_star = false;
                        continue;
                    }

                    if matches!(
                        text.maybe_char_at_index(pos + 1),
                        Some(CharacterCodes::asterisk)
                    ) {
                        pos += 2;
                        while pos < text_len(text) {
                            if text.char_at_index(pos) == CharacterCodes::asterisk
                                && matches!(
                                    text.maybe_char_at_index(pos + 1),
                                    Some(CharacterCodes::slash)
                                )
                            {
                                pos += 2;
                                break;
                            }
                            pos += 1;
                        }
                        can_consume_star = false;
                        continue;
                    }
                }
            },
            CharacterCodes::less_than
            | CharacterCodes::bar
            | CharacterCodes::equals
            | CharacterCodes::greater_than => {
                if is_conflict_marker_trivia(text, pos) {
                    pos = scan_conflict_marker_trivia(
                        text,
                        pos,
                        // Option::<fn(&DiagnosticMessage, Option<usize>, Option<usize>)>::None,
                        |_, _, _| {},
                    );
                    can_consume_star = false;
                    continue;
                }
            }
            CharacterCodes::hash => {
                if pos == 0 && is_shebang_trivia(text, pos) {
                    pos = scan_shebang_trivia(text.clone(), pos);
                    can_consume_star = false;
                    continue;
                }
            }
            CharacterCodes::asterisk => {
                if can_consume_star {
                    pos += 1;
                    can_consume_star = false;
                    continue;
                }
            }
            _ => {
                if ch > CharacterCodes::max_ascii_character && is_white_space_like(ch) {
                    pos += 1;
                    continue;
                }
            }
        }
        return isize::try_from(pos).unwrap();
    }
}

// lazy_static! {
//     pub(super) static ref merge_conflict_marker_length: usize = "<<<<<<<".len();
// }
pub(super) const fn merge_conflict_marker_length() -> usize {
    // "<<<<<<<".len()
    7
}

pub(super) fn is_conflict_marker_trivia(text: &SourceText, pos: usize) -> bool {
    // Debug_.assert(pos >= 0);

    if pos == 0 || matches!(text.maybe_char_at_index(pos - 1), Some(ch) if is_line_break(ch)) {
        let ch = text.maybe_char_at_index(pos);

        if pos + merge_conflict_marker_length() < text.len() {
            let ch = ch.unwrap();
            for i in 0..merge_conflict_marker_length() {
                if text.char_at_index(pos + i) != ch {
                    return false;
                }
            }

            return ch == CharacterCodes::equals
                || text.char_at_index(pos + merge_conflict_marker_length())
                    == CharacterCodes::space;
        }
    }

    false
}

pub(super) fn scan_conflict_marker_trivia<
    TError: FnOnce(&DiagnosticMessage, Option<usize>, Option<usize>),
>(
    text: &SourceText,
    mut pos: usize,
    // error: Option<TError>,
    error: TError,
) -> usize {
    // if let Some(mut error) = error {
    error(
        &Diagnostics::Merge_conflict_marker_encountered,
        Some(pos),
        Some(merge_conflict_marker_length()),
    );
    // }

    let ch = text.char_at_index(pos);
    let len = text.len();

    if matches!(ch, CharacterCodes::less_than | CharacterCodes::greater_than) {
        while pos < len && !is_line_break(text.char_at_index(pos)) {
            pos += 1;
        }
    } else {
        Debug_.assert(
            matches!(ch, CharacterCodes::bar | CharacterCodes::equals),
            None,
        );
        while pos < len {
            let current_char = text.char_at_index(pos);
            if matches!(
                current_char,
                CharacterCodes::equals | CharacterCodes::greater_than
            ) && current_char != ch
                && is_conflict_marker_trivia(text, pos)
            {
                break;
            }

            pos += 1;
        }
    }

    pos
}

lazy_static! {
    pub(super) static ref shebang_trivia_regex: Regex = Regex::new(r"^#!.*").unwrap();
}

pub(crate) fn is_shebang_trivia(text: &SourceText, pos: usize) -> bool {
    Debug_.assert(pos == 0, None);
    shebang_trivia_regex.is_match(text.str())
}

// pub(crate) fn scan_shebang_trivia(
//     text: &str,
//     mut pos: usize, // this is a char index, not a string (byte) index, it's only ok because it's always 0 here
// ) -> usize {
//     let original_pos = pos;
//     let shebang = shebang_trivia_regex.find(text).unwrap().as_str();
//     pos = pos + shebang.len();
//     text_str_num_chars(text, original_pos, pos)
// }
pub(crate) fn scan_shebang_trivia(text: Rc<SourceText>, pos: usize) -> usize {
    get_shebang(text).unwrap().len()
}

pub(super) fn iterate_comment_ranges<
    TState,
    TMemo,
    TCallback: FnMut(usize, usize, CommentKind, bool, &TState, Option<TMemo>) -> TMemo,
>(
    reduce: bool,
    text: Rc<SourceText>,
    mut pos: isize,
    trailing: bool,
    mut cb: TCallback,
    state: &TState,
    initial: Option<TMemo>,
) -> Option<TMemo> {
    let mut pending_pos: Option<usize> = None;
    let mut pending_end: Option<usize> = None;
    let mut pending_kind: Option<CommentKind> = None;
    let mut pending_has_trailing_new_line: Option<bool> = None;
    let mut has_pending_comment_range = false;
    let mut collecting = trailing;
    let mut accumulator = initial;
    if pos == 0 {
        collecting = true;
        let shebang = get_shebang(text.clone());
        if let Some(shebang) = shebang {
            pos = shebang.len().try_into().unwrap();
        }
    }
    while pos >= 0 && TryInto::<usize>::try_into(pos).unwrap() < text.len() {
        let mut pos_as_usize: usize = pos.try_into().unwrap();
        let ch = text.char_at_index(pos_as_usize);
        match ch {
            CharacterCodes::carriage_return => {
                if matches!(
                    text.maybe_char_at_index(pos_as_usize + 1),
                    Some(CharacterCodes::line_feed)
                ) {
                    pos += 1;
                    pos_as_usize += 1;
                }
                pos += 1;
                pos_as_usize += 1;
                if trailing {
                    break;
                }

                collecting = true;
                if has_pending_comment_range {
                    pending_has_trailing_new_line = Some(true);
                }

                continue;
            }
            CharacterCodes::line_feed => {
                pos += 1;
                pos_as_usize += 1;
                if trailing {
                    break;
                }

                collecting = true;
                if has_pending_comment_range {
                    pending_has_trailing_new_line = Some(true);
                }

                continue;
            }
            CharacterCodes::tab
            | CharacterCodes::vertical_tab
            | CharacterCodes::form_feed
            | CharacterCodes::space => {
                pos += 1;
                pos_as_usize += 1;
                continue;
            }
            CharacterCodes::slash => {
                let next_char = text.maybe_char_at_index(pos_as_usize + 1);
                let mut has_trailing_new_line = false;
                if matches!(
                    next_char,
                    Some(CharacterCodes::slash | CharacterCodes::asterisk)
                ) {
                    let next_char = next_char.unwrap();
                    let kind = if next_char == CharacterCodes::slash {
                        SyntaxKind::SingleLineCommentTrivia
                    } else {
                        SyntaxKind::MultiLineCommentTrivia
                    };
                    let start_pos = pos_as_usize;
                    pos += 2;
                    pos_as_usize += 2;
                    if next_char == CharacterCodes::slash {
                        while pos_as_usize < text.len() {
                            if is_line_break(text.char_at_index(pos_as_usize)) {
                                has_trailing_new_line = true;
                                break;
                            }
                            pos += 1;
                            pos_as_usize += 1;
                        }
                    } else {
                        while pos_as_usize < text.len() {
                            if text.char_at_index(pos_as_usize) == CharacterCodes::asterisk
                                && matches!(
                                    text.maybe_char_at_index(pos_as_usize + 1),
                                    Some(CharacterCodes::slash)
                                )
                            {
                                pos += 2;
                                pos_as_usize += 2;
                                break;
                            }
                            pos += 1;
                            pos_as_usize += 1;
                        }
                    }

                    if collecting {
                        if has_pending_comment_range {
                            accumulator = Some(cb(
                                pending_pos.unwrap(),
                                pending_end.unwrap(),
                                pending_kind.unwrap(),
                                pending_has_trailing_new_line.unwrap(),
                                state,
                                accumulator,
                            ));
                            if !reduce && accumulator.is_some() {
                                return accumulator;
                            }
                        }

                        pending_pos = Some(start_pos);
                        pending_end = Some(pos_as_usize);
                        pending_kind = Some(kind);
                        pending_has_trailing_new_line = Some(has_trailing_new_line);
                        has_pending_comment_range = true;
                    }

                    continue;
                }

                break;
            }
            _ => {
                if ch > CharacterCodes::max_ascii_character && is_white_space_like(ch) {
                    if has_pending_comment_range && is_line_break(ch) {
                        pending_has_trailing_new_line = Some(true);
                    }
                    pos += 1;
                    pos_as_usize += 1;
                    continue;
                }
                break;
            }
        }
    }

    if has_pending_comment_range {
        accumulator = Some(cb(
            pending_pos.unwrap(),
            pending_end.unwrap(),
            pending_kind.unwrap(),
            pending_has_trailing_new_line.unwrap(),
            state,
            accumulator,
        ));
    }

    accumulator
}

pub fn for_each_leading_comment_range<
    TState,
    TMemo,
    TCallback: FnMut(usize, usize, CommentKind, bool, &TState) -> TMemo,
>(
    text: Rc<SourceText>,
    pos: isize,
    mut cb: TCallback,
    state: &TState, // TODO: expose a for_each_leading_comment_no_state variant (with different callback args and no state arg)?
) -> Option<TMemo> {
    iterate_comment_ranges(
        false,
        text,
        pos,
        false,
        |pos, end, kind, has_trailing_new_line, state, _| {
            cb(pos, end, kind, has_trailing_new_line, state)
        },
        state,
        None,
    )
}

pub fn for_each_trailing_comment_range<
    TState,
    TMemo,
    TCallback: FnMut(usize, usize, CommentKind, bool, &TState) -> TMemo,
>(
    text: Rc<SourceText>,
    pos: isize,
    mut cb: TCallback,
    state: &TState,
) -> Option<TMemo> {
    iterate_comment_ranges(
        false,
        text,
        pos,
        true,
        |pos, end, kind, has_trailing_new_line, state, _| {
            cb(pos, end, kind, has_trailing_new_line, state)
        },
        state,
        None,
    )
}

pub fn reduce_each_leading_comment_range<
    TState,
    TMemo,
    TCallback: FnMut(usize, usize, CommentKind, bool, &TState, TMemo) -> TMemo,
>(
    text: Rc<SourceText>,
    pos: isize,
    mut cb: TCallback,
    state: &TState,
    initial: TMemo,
) -> TMemo {
    iterate_comment_ranges(
        true,
        text,
        pos,
        false,
        |pos, end, kind, has_trailing_new_line, state, memo| {
            cb(pos, end, kind, has_trailing_new_line, state, memo.unwrap())
        },
        state,
        Some(initial),
    )
    .unwrap()
}

pub fn reduce_each_trailing_comment_range<
    TState,
    TMemo,
    TCallback: FnMut(usize, usize, CommentKind, bool, &TState, TMemo) -> TMemo,
>(
    text: Rc<SourceText>,
    pos: isize,
    mut cb: TCallback,
    state: &TState,
    initial: TMemo,
) -> TMemo {
    iterate_comment_ranges(
        true,
        text,
        pos,
        true,
        |pos, end, kind, has_trailing_new_line, state, memo| {
            cb(pos, end, kind, has_trailing_new_line, state, memo.unwrap())
        },
        state,
        Some(initial),
    )
    .unwrap()
}

pub(super) fn append_comment_range(
    pos: usize,
    end: usize,
    kind: CommentKind,
    has_trailing_new_line: bool,
    _state: &(),
    mut comments: Option<Vec<CommentRange>>,
) -> Option<Vec<CommentRange>> {
    if comments.is_none() {
        comments = Some(vec![]);
    }
    let mut comments = comments.unwrap();

    comments.push(CommentRange::new(
        kind,
        pos.try_into().unwrap(),
        end.try_into().unwrap(),
        Some(has_trailing_new_line),
    ));
    Some(comments)
}

pub fn get_leading_comment_ranges(text: Rc<SourceText>, pos: isize) -> Option<Vec<CommentRange>> {
    reduce_each_leading_comment_range(text, pos, append_comment_range, &(), None)
}

pub fn get_trailing_comment_ranges(text: Rc<SourceText>, pos: isize) -> Option<Vec<CommentRange>> {
    reduce_each_trailing_comment_range(text, pos, append_comment_range, &(), None)
}

pub fn get_shebang(text: Rc<SourceText>) -> Option<SourceTextSlice> {
    let match_ = shebang_trivia_regex.find(text.str());
    match_.map(|match_| text.slice_from_str_offsets(match_.start(), Some(match_.end())))
}

pub fn is_identifier_start(ch: char, language_version: Option<ScriptTarget>) -> bool {
    ch >= CharacterCodesChar::A && ch <= CharacterCodesChar::Z
        || ch >= CharacterCodesChar::a && ch <= CharacterCodesChar::z
        || ch == CharacterCodesChar::dollar_sign
        || ch == CharacterCodesChar::underscore
        || ch > CharacterCodesChar::max_ascii_character
            && is_unicode_identifier_start(ch, language_version)
}

pub fn is_identifier_part(
    ch: char,
    language_version: Option<ScriptTarget>,
    identifier_variant: Option<LanguageVariant>,
) -> bool {
    ch >= CharacterCodes::A && ch <= CharacterCodes::Z
        || ch >= CharacterCodes::a && ch <= CharacterCodes::z
        || ch >= CharacterCodes::_0 && ch <= CharacterCodes::_9
        || ch == CharacterCodes::dollar_sign
        || ch == CharacterCodes::underscore
        || match identifier_variant {
            Some(LanguageVariant::JSX) => {
                matches!(ch, CharacterCodes::minus | CharacterCodes::colon)
            }
            _ => false,
        }
        || ch > CharacterCodes::max_ascii_character
            && is_unicode_identifier_part(ch, language_version)
}

pub fn is_identifier_text(
    name: &str,
    language_version: Option<ScriptTarget>,
    identifier_variant: Option<LanguageVariant>,
) -> bool {
    let ch = maybe_code_point_at(&name.chars().collect(), 0);
    if !matches!(
        ch,
        Some(ch) if is_identifier_start(ch, language_version)
    ) {
        return false;
    }

    for ch in name.chars().skip(1) {
        if !is_identifier_part(ch, language_version, identifier_variant) {
            return false;
        }
    }

    true
}

pub fn create_scanner(
    language_version: ScriptTarget,
    skip_trivia: bool,
    language_variant: Option<LanguageVariant>,
    text_initial: Option<Rc<SourceText>>,
    /*onError?: ErrorCallback,*/ start: Option<usize>,
    length: Option<usize>,
) -> Scanner {
    let mut scanner = Scanner::new(language_version, skip_trivia, language_variant);
    scanner.set_text(text_initial, start, length);
    /*if Debug.isDebugging {
    Object.defineProperty(scanner, "__debugShowCurrentPositionInText"*/
    scanner
}

pub struct Scanner /*<'on_error>*/ {
    pub(super) language_version: ScriptTarget,
    pub(super) skip_trivia: bool,
    pub(super) language_variant: Option<LanguageVariant>,
    // on_error: Option<ErrorCallback<'on_error>>,
    pub(super) text: RefCell<Option<Rc<SourceText>>>,
    pub(super) pos: RefCell<Option<usize>>,
    pub(super) end: Cell<Option<usize>>,
    pub(super) start_pos: RefCell<Option<usize>>,
    pub(super) token_pos: RefCell<Option<usize>>,
    pub(super) token: RefCell<Option<SyntaxKind>>,
    pub(super) token_value: RefCell<Option<SourceTextSliceOrString>>,
    pub(super) token_flags: RefCell<Option<TokenFlags>>,
    pub(super) comment_directives: RefCell<Option<Vec<Rc<CommentDirective>>>>,
    pub(super) in_jsdoc_type: Cell<isize>,
}

impl Scanner {
    pub(super) fn new(
        language_version: ScriptTarget,
        skip_trivia: bool,
        language_variant: Option<LanguageVariant>,
    ) -> Self {
        Scanner {
            language_version,
            skip_trivia,
            language_variant,
            // on_error: None,
            text: RefCell::new(None),
            pos: RefCell::new(None),
            end: Cell::new(None),
            start_pos: RefCell::new(None),
            token_pos: RefCell::new(None),
            token: RefCell::new(None),
            token_value: RefCell::new(None),
            token_flags: RefCell::new(None),
            comment_directives: RefCell::new(None),
            in_jsdoc_type: Cell::new(0),
        }
    }

    pub(super) fn text(&self) -> Rc<SourceText> {
        self.text.borrow().clone().unwrap()
    }

    // pub(super) fn text_str(&self) -> Ref<String> {
    //     Ref::map(self.text_str.borrow(), |text_str| {
    //         text_str.as_ref().unwrap()
    //     })
    // }

    pub(super) fn set_text_(&self, text: Rc<SourceText>) {
        *self.text.borrow_mut() = Some(text);
    }

    pub(super) fn maybe_text_char_at_index(&self, index: usize) -> Option<&str> {
        self.text().maybe_char_at_index(index)
    }

    pub(super) fn text_char_at_index(&self, index: usize) -> &str {
        self.text().char_at_index(index)
    }

    // pub(super) fn text_substring(&self, start: usize, end: usize) -> String {
    //     text_substring(&self.text(), start, end)
    // }

    pub(super) fn text_slice(&self, start: usize, end: usize) -> SourceTextSlice {
        self.text().slice(start, Some(end))
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
        self.end.get().unwrap()
    }

    pub(super) fn set_end(&self, end: usize) {
        self.end.set(Some(end));
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

    pub(super) fn token_value(&self) -> Ref<SourceTextSliceOrString> {
        Ref::map(self.token_value.borrow(), |token_value| {
            token_value.as_ref().unwrap()
        })
    }

    pub(super) fn maybe_token_value(&self) -> Ref<Option<SourceTextSliceOrString>> {
        self.token_value.borrow()
    }

    pub(super) fn set_token_value(&self, token_value: SourceTextSliceOrString) {
        *self.token_value.borrow_mut() = Some(token_value);
    }

    pub(super) fn set_maybe_token_value(&self, token_value: Option<SourceTextSliceOrString>) {
        *self.token_value.borrow_mut() = token_value;
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

    pub(super) fn maybe_comment_directives(&self) -> Ref<Option<Vec<Rc<CommentDirective>>>> {
        self.comment_directives.borrow()
    }

    pub(super) fn maybe_comment_directives_mut(&self) -> RefMut<Option<Vec<Rc<CommentDirective>>>> {
        self.comment_directives.borrow_mut()
    }

    pub(super) fn in_jsdoc_type(&self) -> isize {
        self.in_jsdoc_type.get()
    }

    pub fn get_start_pos(&self) -> usize {
        self.start_pos()
    }

    pub fn get_text_pos(&self) -> usize {
        self.pos()
    }

    pub fn get_token(&self) -> SyntaxKind {
        self.token()
    }

    pub fn get_token_pos(&self) -> usize {
        self.token_pos()
    }

    pub fn get_token_text(&self) -> SourceTextSlice {
        self.text_slice(self.token_pos(), self.pos())
    }

    pub fn get_token_value(&self) -> Ref<SourceTextSliceOrString> {
        self.token_value()
    }

    pub fn has_unicode_escape(&self) -> bool {
        self.token_flags().intersects(TokenFlags::UnicodeEscape)
    }

    pub fn has_extended_unicode_escape(&self) -> bool {
        self.token_flags()
            .intersects(TokenFlags::ExtendedUnicodeEscape)
    }

    pub fn has_preceding_line_break(&self) -> bool {
        self.token_flags()
            .intersects(TokenFlags::PrecedingLineBreak)
    }

    pub fn has_preceding_jsdoc_comment(&self) -> bool {
        self.token_flags()
            .intersects(TokenFlags::PrecedingJSDocComment)
    }

    pub fn is_identifier(&self) -> bool {
        self.token() == SyntaxKind::Identifier || self.token() > SyntaxKind::LastReservedWord
    }

    pub fn is_reserved_word(&self) -> bool {
        self.token() >= SyntaxKind::FirstReservedWord
            && self.token() <= SyntaxKind::LastReservedWord
    }

    pub fn is_unterminated(&self) -> bool {
        self.token_flags().intersects(TokenFlags::Unterminated)
    }

    pub fn get_comment_directives(&self) -> Ref<Option<Vec<Rc<CommentDirective>>>> {
        self.comment_directives.borrow()
    }

    pub fn get_numeric_literal_flags(&self) -> TokenFlags {
        self.token_flags() & TokenFlags::NumericLiteralFlags
    }

    pub fn get_token_flags(&self) -> TokenFlags {
        self.token_flags()
    }
}
