use crate::{CharacterCodes, SyntaxKind};

struct ScanNumberReturn {
    type_: SyntaxKind,
    value: String,
}

pub struct Scanner {
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

    pub fn scan(&mut self) -> SyntaxKind {
        self.set_start_pos(self.pos());

        loop {
            self.set_token_pos(self.pos());
            if self.pos() >= self.end() {
                return self.set_token(SyntaxKind::EndOfFileToken);
            }
            let ch = code_point_at(self.text(), self.pos());

            match ch {
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
                    panic!("Unimplemented");
                    // let identifier_kind = self.scan_identifier(ch);
                }
            }
        }
    }

    pub fn set_text(
        &mut self,
        new_text: Option<&str>,
        start: Option<usize>,
        length: Option<usize>,
    ) {
        let text = new_text.unwrap_or("");
        self.set_text_(text);
        self.set_end(match length {
            None => text.len(),
            Some(length) => start.unwrap() + length,
        });
        self.set_text_pos(start.unwrap_or(0));
    }

    pub fn set_text_pos(&mut self, text_pos: usize) {
        // Debug_.assert(text_pos >= 0);
        self.set_pos(text_pos);
        self.set_start_pos(text_pos);
        self.set_token_pos(text_pos);
        self.set_token(SyntaxKind::Unknown);
    }

    fn new() -> Self {
        Scanner {
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

pub fn create_scanner() -> Scanner {
    Scanner::new()
}

fn code_point_at(s: &str, i: usize) -> char {
    s.chars().nth(i).unwrap()
}
