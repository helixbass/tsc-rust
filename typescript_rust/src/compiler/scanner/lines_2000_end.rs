use std::{
    cell::{Ref, RefMut},
    convert::TryInto,
    rc::Rc,
};

use regex::Regex;

use super::{
    is_conflict_marker_trivia, is_identifier_part, is_identifier_start, is_line_break,
    is_white_space_like, is_white_space_single_line, scan_conflict_marker_trivia,
    token_is_identifier_or_keyword, ErrorCallback, Scanner,
};
use crate::{
    append, trim_string_start, BaseTextRange, CharacterCodes, CommentDirective,
    CommentDirectiveType, Debug_, Diagnostics, LanguageVariant, ScriptTarget, SourceTextAsChars,
    SyntaxKind, TokenFlags,
};

impl Scanner {
    pub fn re_scan_invalid_identifier(&self, on_error: Option<ErrorCallback>) -> SyntaxKind {
        Debug_.assert(self.token() == SyntaxKind::Unknown, Some("'reScanInvalidIdentifier' should only be called when the current token is 'SyntaxKind.Unknown'."));
        self.set_pos(self.start_pos());
        self.set_token_pos(self.start_pos());
        self.set_token_flags(TokenFlags::None);
        let ch = code_point_at(&self.text(), self.pos());
        let identifier_kind = self.scan_identifier(on_error, ch, ScriptTarget::ESNext);
        if let Some(identifier_kind) = identifier_kind {
            return self.set_token(identifier_kind);
        }
        self.increment_pos_by(char_size(ch));
        self.token()
    }

    pub(super) fn scan_identifier(
        &self,
        on_error: Option<ErrorCallback>,
        start_character: char,
        language_version: ScriptTarget,
    ) -> Option<SyntaxKind> {
        let mut ch = start_character;
        if is_identifier_start(ch, Some(language_version)) {
            self.increment_pos_by(char_size(ch));
            loop {
                if !(self.pos() < self.end()) {
                    break;
                }
                ch = code_point_at(&self.text(), self.pos());
                if !is_identifier_part(ch, Some(language_version), None) {
                    break;
                }
                self.increment_pos_by(char_size(ch));
            }
            self.set_token_value(self.text_substring(self.token_pos(), self.pos()));
            if ch == CharacterCodes::backslash {
                self.set_token_value(format!(
                    "{}{}",
                    self.token_value(),
                    self.scan_identifier_parts(on_error)
                ));
            }
            return Some(self.get_identifier_token());
        }
        None
    }

    pub fn re_scan_greater_token(&self) -> SyntaxKind {
        if self.token() == SyntaxKind::GreaterThanToken {
            if self.text_char_at_index(self.pos()) == CharacterCodes::greater_than {
                if matches!(
                    self.maybe_text_char_at_index(self.pos() + 1),
                    Some(CharacterCodes::greater_than)
                ) {
                    if matches!(
                        self.maybe_text_char_at_index(self.pos() + 2),
                        Some(CharacterCodes::equals)
                    ) {
                        self.increment_pos_by(3);
                        return self
                            .set_token(SyntaxKind::GreaterThanGreaterThanGreaterThanEqualsToken);
                    }
                    self.increment_pos_by(2);
                    return self.set_token(SyntaxKind::GreaterThanGreaterThanGreaterThanToken);
                }
                if matches!(
                    self.maybe_text_char_at_index(self.pos() + 1),
                    Some(CharacterCodes::equals)
                ) {
                    self.increment_pos_by(2);
                    return self.set_token(SyntaxKind::GreaterThanGreaterThanEqualsToken);
                }
                self.increment_pos();
                return self.set_token(SyntaxKind::GreaterThanGreaterThanToken);
            }
            if self.text_char_at_index(self.pos()) == CharacterCodes::equals {
                self.increment_pos();
                return self.set_token(SyntaxKind::GreaterThanEqualsToken);
            }
        }
        self.token()
    }

    pub fn re_scan_asterisk_equals_token(&self) -> SyntaxKind {
        Debug_.assert(
            self.token() == SyntaxKind::AsteriskEqualsToken,
            Some("'reScanAsteriskEqualsToken' should only be called on a '*='"),
        );
        self.set_pos(self.token_pos() + 1);
        self.set_token(SyntaxKind::EqualsToken)
    }

    pub fn re_scan_slash_token(&self, on_error: Option<ErrorCallback>) -> SyntaxKind {
        if matches!(
            self.token(),
            SyntaxKind::SlashToken | SyntaxKind::SlashEqualsToken
        ) {
            let mut p = self.token_pos() + 1;
            let mut in_escape = false;
            let mut in_character_class = false;
            loop {
                if p >= self.end() {
                    self.add_token_flag(TokenFlags::Unterminated);
                    self.error(
                        on_error,
                        &Diagnostics::Unterminated_regular_expression_literal,
                        None,
                        None,
                    );
                    break;
                }

                let ch = self.text_char_at_index(p);
                if is_line_break(ch) {
                    self.add_token_flag(TokenFlags::Unterminated);
                    self.error(
                        on_error,
                        &Diagnostics::Unterminated_regular_expression_literal,
                        None,
                        None,
                    );
                    break;
                }

                if in_escape {
                    in_escape = false;
                } else if ch == CharacterCodes::slash && !in_character_class {
                    p += 1;
                    break;
                } else if ch == CharacterCodes::open_bracket {
                    in_character_class = true;
                } else if ch == CharacterCodes::backslash {
                    in_escape = true;
                } else if ch == CharacterCodes::close_bracket {
                    in_character_class = false;
                }
                p += 1;
            }

            while p < self.end()
                && is_identifier_part(
                    self.text_char_at_index(p),
                    Some(self.language_version),
                    None,
                )
            {
                p += 1;
            }
            self.set_pos(p);
            self.set_token_value(self.text_substring(self.token_pos(), self.pos()));
            self.set_token(SyntaxKind::RegularExpressionLiteral);
        }
        self.token()
    }

    pub(super) fn append_if_comment_directive(
        &self,
        mut comment_directives: RefMut<Option<Vec<Rc<CommentDirective>>>>,
        text: &str,
        comment_directive_reg_ex: &Regex,
        line_start: usize,
    ) /*Vec<CommentDirective>*/
    {
        let type_ =
            self.get_directive_from_comment(&trim_string_start(text), comment_directive_reg_ex);
        if type_.is_none() {
            return /*comment_directives*/;
        }
        let type_ = type_.unwrap();

        if comment_directives.is_none() {
            *comment_directives = Some(vec![]);
        }
        let mut comment_directives =
            RefMut::map(comment_directives, |option| option.as_mut().unwrap());

        /*return*/
        append(
            &mut comment_directives,
            Some(Rc::new(CommentDirective {
                range: BaseTextRange::new(
                    line_start.try_into().unwrap(),
                    self.pos().try_into().unwrap(),
                ),
                type_,
            })),
        )
    }

    pub(super) fn get_directive_from_comment(
        &self,
        text: &str,
        comment_directive_reg_ex: &Regex,
    ) -> Option<CommentDirectiveType> {
        let match_ = comment_directive_reg_ex.captures(text);
        if match_.is_none() {
            return None;
        }
        let match_ = match_.unwrap();

        match match_.get(1).unwrap().as_str() {
            "ts-expect-error" => {
                return Some(CommentDirectiveType::ExpectError);
            }
            "ts-ignore" => {
                return Some(CommentDirectiveType::Ignore);
            }
            _ => (),
        }

        None
    }

    pub(crate) fn re_scan_template_token(
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

    pub(crate) fn re_scan_template_head_or_no_substitution_template(
        &self,
        on_error: Option<ErrorCallback>,
    ) -> SyntaxKind {
        self.set_pos(self.token_pos());
        self.set_token(self.scan_template_and_set_token_value(on_error, true))
    }

    pub(crate) fn re_scan_jsx_token(
        &self,
        on_error: Option<ErrorCallback>,
        allow_multiline_jsx_text: Option<bool>,
    ) -> SyntaxKind /*JsxTokenSyntaxKind*/ {
        let allow_multiline_jsx_text = allow_multiline_jsx_text.unwrap_or(true);
        self.set_pos(self.start_pos());
        self.set_token_pos(self.start_pos());
        self.set_token(self.scan_jsx_token(on_error, Some(allow_multiline_jsx_text)))
    }

    pub(crate) fn re_scan_less_than_token(&self) -> SyntaxKind {
        if self.token() == SyntaxKind::LessThanLessThanToken {
            self.set_pos(self.token_pos() + 1);
            return self.set_token(SyntaxKind::LessThanToken);
        }
        self.token()
    }

    pub(crate) fn re_scan_hash_token(&self) -> SyntaxKind {
        if self.token() == SyntaxKind::PrivateIdentifier {
            self.set_pos(self.token_pos() + 1);
            return self.set_token(SyntaxKind::HashToken);
        }
        self.token()
    }

    pub(crate) fn re_scan_question_token(&self) -> SyntaxKind {
        Debug_.assert(
            self.token() == SyntaxKind::QuestionQuestionToken,
            Some("'reScanQuestionToken' should only be called on a '??'"),
        );
        self.set_pos(self.token_pos() + 1);
        self.set_token(SyntaxKind::QuestionToken)
    }

    pub fn scan_jsx_token(
        &self,
        on_error: Option<ErrorCallback>,
        allow_multiline_jsx_text: Option<bool>,
    ) -> SyntaxKind /*JsxTokenSyntaxKind*/ {
        let allow_multiline_jsx_text = allow_multiline_jsx_text.unwrap_or(true);
        self.set_start_pos(self.pos());
        self.set_token_pos(self.pos());

        if self.pos() >= self.end() {
            return self.set_token(SyntaxKind::EndOfFileToken);
        }

        let mut char_ = self.text_char_at_index(self.pos());
        if char_ == CharacterCodes::less_than {
            if matches!(
                self.maybe_text_char_at_index(self.pos() + 1),
                Some(CharacterCodes::slash)
            ) {
                self.increment_pos_by(2);
                return self.set_token(SyntaxKind::LessThanSlashToken);
            }
            self.increment_pos();
            return self.set_token(SyntaxKind::LessThanToken);
        }

        if char_ == CharacterCodes::open_brace {
            self.increment_pos();
            return self.set_token(SyntaxKind::OpenBraceToken);
        }

        let mut first_non_whitespace: isize = 0;

        while self.pos() < self.end() {
            char_ = self.text_char_at_index(self.pos());
            if char_ == CharacterCodes::open_brace {
                break;
            }
            if char_ == CharacterCodes::less_than {
                if is_conflict_marker_trivia(&self.text(), self.pos()) {
                    self.set_pos(scan_conflict_marker_trivia(
                        &self.text(),
                        self.pos(),
                        |diag, pos, len| self.error(on_error, diag, pos, len),
                    ));
                    return self.set_token(SyntaxKind::ConflictMarkerTrivia);
                }
                break;
            }
            if char_ == CharacterCodes::greater_than {
                self.error(
                    on_error,
                    &Diagnostics::Unexpected_token_Did_you_mean_or_gt,
                    Some(self.pos()),
                    Some(1),
                );
            }
            if char_ == CharacterCodes::close_brace {
                self.error(
                    on_error,
                    &Diagnostics::Unexpected_token_Did_you_mean_or_rbrace,
                    Some(self.pos()),
                    Some(1),
                );
            }

            if is_line_break(char_) && first_non_whitespace == 0 {
                first_non_whitespace = -1;
            } else if !allow_multiline_jsx_text && is_line_break(char_) && first_non_whitespace > 0
            {
                break;
            } else if !is_white_space_like(char_) {
                first_non_whitespace = self.pos().try_into().unwrap();
            }

            self.increment_pos();
        }

        self.set_token_value(self.text_substring(self.start_pos(), self.pos()));

        if first_non_whitespace == -1 {
            SyntaxKind::JsxTextAllWhiteSpaces
        } else {
            SyntaxKind::JsxText
        }
    }

    pub fn scan_jsx_identifier(&self, on_error: Option<ErrorCallback>) -> SyntaxKind {
        if token_is_identifier_or_keyword(self.token()) {
            let mut namespace_separator = false;
            while self.pos() < self.end() {
                let ch = self.text_char_at_index(self.pos());
                if ch == CharacterCodes::minus {
                    self.set_token_value(format!("{}-", self.token_value()));
                    self.increment_pos();
                    continue;
                } else if ch == CharacterCodes::colon && !namespace_separator {
                    self.set_token_value(format!("{}:", self.token_value()));
                    self.increment_pos();
                    namespace_separator = true;
                    self.set_token(SyntaxKind::Identifier);
                    continue;
                }
                let old_pos = self.pos();
                self.set_token_value(format!(
                    "{}{}",
                    self.token_value(),
                    self.scan_identifier_parts(on_error)
                ));
                if self.pos() == old_pos {
                    break;
                }
            }
            if self.token_value().ends_with(":") {
                let mut token_value = self.token_value().clone();
                token_value.pop().unwrap();
                self.set_token_value(token_value);
                self.set_pos(self.pos() - 1);
            }
        }
        self.token()
    }

    pub fn scan_jsx_attribute_value(&self, on_error: Option<ErrorCallback>) -> SyntaxKind {
        self.set_start_pos(self.pos());

        match self.text_char_at_index(self.pos()) {
            CharacterCodes::double_quote | CharacterCodes::single_quote => {
                self.set_token_value(self.scan_string(on_error, Some(true)));
                self.set_token(SyntaxKind::StringLiteral)
            }
            _ => self.scan(on_error),
        }
    }

    pub fn re_scan_jsx_attribute_value(&self, on_error: Option<ErrorCallback>) -> SyntaxKind {
        self.set_pos(self.start_pos());
        self.set_token_pos(self.start_pos());
        self.scan_jsx_attribute_value(on_error)
    }

    pub fn scan_js_doc_token(&self, on_error: Option<ErrorCallback>) -> SyntaxKind /*JSDocSyntaxKind*/
    {
        self.set_start_pos(self.pos());
        self.set_token_pos(self.pos());
        self.set_token_flags(TokenFlags::None);
        if self.pos() >= self.end() {
            return self.set_token(SyntaxKind::EndOfFileToken);
        }

        let ch = code_point_at(&self.text(), self.pos());
        self.increment_pos_by(char_size(ch));
        match ch {
            CharacterCodes::tab
            | CharacterCodes::vertical_tab
            | CharacterCodes::form_feed
            | CharacterCodes::space => {
                while self.pos() < self.end()
                    && is_white_space_single_line(self.text_char_at_index(self.pos()))
                {
                    self.increment_pos();
                }
                return self.set_token(SyntaxKind::WhitespaceTrivia);
            }
            CharacterCodes::at => {
                return self.set_token(SyntaxKind::AtToken);
            }
            CharacterCodes::carriage_return => {
                if self.text_char_at_index(self.pos()) == CharacterCodes::line_feed {
                    self.increment_pos();
                }
                self.add_token_flag(TokenFlags::PrecedingLineBreak);
                return self.set_token(SyntaxKind::NewLineTrivia);
            }
            CharacterCodes::line_feed => {
                self.add_token_flag(TokenFlags::PrecedingLineBreak);
                return self.set_token(SyntaxKind::NewLineTrivia);
            }
            CharacterCodes::asterisk => {
                return self.set_token(SyntaxKind::AsteriskToken);
            }
            CharacterCodes::open_brace => {
                return self.set_token(SyntaxKind::OpenBraceToken);
            }
            CharacterCodes::close_brace => {
                return self.set_token(SyntaxKind::CloseBraceToken);
            }
            CharacterCodes::open_bracket => {
                return self.set_token(SyntaxKind::OpenBracketToken);
            }
            CharacterCodes::close_bracket => {
                return self.set_token(SyntaxKind::CloseBracketToken);
            }
            CharacterCodes::less_than => {
                return self.set_token(SyntaxKind::LessThanToken);
            }
            CharacterCodes::greater_than => {
                return self.set_token(SyntaxKind::GreaterThanToken);
            }
            CharacterCodes::equals => {
                return self.set_token(SyntaxKind::EqualsToken);
            }
            CharacterCodes::comma => {
                return self.set_token(SyntaxKind::CommaToken);
            }
            CharacterCodes::dot => {
                return self.set_token(SyntaxKind::DotToken);
            }
            CharacterCodes::backtick => {
                return self.set_token(SyntaxKind::BacktickToken);
            }
            CharacterCodes::hash => {
                return self.set_token(SyntaxKind::HashToken);
            }
            CharacterCodes::backslash => {
                self.set_pos(self.pos() - 1);
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
                    self.set_token_value(format!(
                        "{}{}",
                        self.scan_extended_unicode_escape(on_error),
                        self.scan_identifier_parts(on_error)
                    ));
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
                    self.set_token_value(format!(
                        "{}{}",
                        char::from_u32(cooked_char.unwrap()).unwrap(),
                        self.scan_identifier_parts(on_error)
                    ));
                    return self.set_token(self.get_identifier_token());
                }
                self.increment_pos();
                return self.set_token(SyntaxKind::Unknown);
            }
            _ => (),
        }

        if is_identifier_start(ch, Some(self.language_version)) {
            let mut char_ = ch;
            loop {
                if self.pos() < self.end() {
                    char_ = code_point_at(&self.text(), self.pos());
                    if !is_identifier_part(char_, Some(self.language_version), None) {
                        break;
                    }
                } else if !matches!(
                    self.maybe_text_char_at_index(self.pos()),
                    Some(CharacterCodes::minus)
                ) {
                    break;
                }
                self.increment_pos_by(char_size(char_));
            }
            self.set_token_value(self.text_substring(self.token_pos(), self.pos()));
            if char_ == CharacterCodes::backslash {
                self.set_token_value(format!(
                    "{}{}",
                    self.token_value(),
                    self.scan_identifier_parts(on_error)
                ));
            }
            self.set_token(self.get_identifier_token())
        } else {
            self.set_token(SyntaxKind::Unknown)
        }
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
        let save_token_value = self.maybe_token_value().clone();
        let save_token_flags = self.token_flags();
        let result = callback();

        if result.is_none() /*TODO: this doesn't cover the "falsy" case, should look for those and maybe expose a different variant?*/ || is_lookahead
        {
            self.set_pos(save_pos);
            self.set_start_pos(save_start_pos);
            self.set_token_pos(save_token_pos);
            self.set_token(save_token);
            self.set_maybe_token_value(save_token_value);
            self.set_token_flags(save_token_flags);
        }
        result
    }

    pub fn scan_range<TReturn, TCallback: FnOnce() -> TReturn>(
        &self,
        start: usize,
        length: usize,
        callback: TCallback,
    ) -> TReturn {
        let save_end = self.end();
        let save_pos = self.pos();
        let save_start_pos = self.start_pos();
        let save_token_pos = self.token_pos();
        let save_token = self.token();
        let save_token_value = self.maybe_token_value().clone();
        let save_token_flags = self.token_flags();
        let save_error_expectations = (*self.maybe_comment_directives()).clone();

        self.set_text_start_and_length(start, length);
        let result = callback();

        self.set_end(save_end);
        self.set_pos(save_pos);
        self.set_start_pos(save_start_pos);
        self.set_token_pos(save_token_pos);
        self.set_token(save_token);
        self.set_maybe_token_value(save_token_value);
        self.set_token_flags(save_token_flags);
        *self.maybe_comment_directives_mut() = save_error_expectations;

        result
    }

    pub fn look_ahead<TReturn>(
        &self,
        callback: impl FnOnce() -> Option<TReturn>,
    ) -> Option<TReturn> {
        self.speculation_helper(callback, true)
    }

    pub fn try_scan<TReturn>(&self, callback: impl FnOnce() -> Option<TReturn>) -> Option<TReturn> {
        self.speculation_helper(callback, false)
    }

    pub fn get_text(&self) -> Ref<String> {
        self.text_str()
    }

    pub fn clear_comment_directives(&self) {
        *self.maybe_comment_directives_mut() = None;
    }

    pub fn set_text(
        &self,
        new_text_as_chars: Option<SourceTextAsChars>,
        new_text: Option<String>,
        start: Option<usize>,
        length: Option<usize>,
    ) {
        let text = new_text.unwrap_or_else(|| "".to_string());
        let text_as_chars = new_text_as_chars.unwrap_or_else(|| vec![]);
        let text_as_chars_len = text_as_chars.len();
        self.set_text_(text_as_chars, text);
        self.set_end(length.map_or(text_as_chars_len, |length| start.unwrap() + length));
        self.set_text_pos(start.unwrap_or(0));
    }

    pub fn set_text_start_and_length(&self, start: usize, length: usize) {
        self.set_end(start + length);
        self.set_text_pos(start);
    }

    // pub fn set_on_error(&mut self, error_callback: Option<ErrorCallback<'on_error>>) {
    //     self.on_error = error_callback;
    // }

    pub fn set_script_target(&mut self, script_target: ScriptTarget) {
        self.language_version = script_target;
    }

    pub fn set_language_variant(&mut self, variant: LanguageVariant) {
        self.language_variant = Some(variant);
    }

    pub fn set_text_pos(&self, text_pos: usize) {
        // Debug_.assert(text_pos >= 0);
        self.set_pos(text_pos);
        self.set_start_pos(text_pos);
        self.set_token_pos(text_pos);
        self.set_token(SyntaxKind::Unknown);
        self.set_maybe_token_value(None);
        self.set_token_flags(TokenFlags::None);
    }

    pub fn set_in_jsdoc_type(&self, in_type: bool) {
        self.in_jsdoc_type
            .set(self.in_jsdoc_type.get() + if in_type { 1 } else { -1 });
    }
}

pub(super) fn maybe_code_point_at(s: &SourceTextAsChars, i: usize) -> Option<char> {
    s.get(i).map(|ch| *ch)
}

pub(super) fn code_point_at(s: &SourceTextAsChars, i: usize) -> char {
    s[i]
}

pub(super) fn char_size(_ch: char) -> usize {
    1
}

pub(crate) fn utf16_encode_as_string(code_point: u32) -> String {
    char::from_u32(code_point).unwrap().to_string()
}

pub(super) fn hex_digits_to_u32(str: &str) -> Result<u32, String> {
    u32::from_str_radix(str, 16).map_err(|_| "Couldn't convert hex digits to u32".to_string())
}
