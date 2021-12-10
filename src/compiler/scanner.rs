use crate::{CharacterCodes, SyntaxKind};

pub struct Scanner {
    text: Option<String>,
    pos: Option<usize>,
    end: Option<usize>,
    start_pos: Option<usize>,
    token_pos: Option<usize>,
    token: Option<SyntaxKind>,
}

impl Scanner {
    pub fn get_text_pos(&self) -> usize {
        self.pos()
    }

    pub fn get_token_pos(&self) -> usize {
        self.token_pos()
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
                ch => {
                    panic!("Unimplemented");
                    // let identifier_kind = self.scan_identifier(ch);
                }
            }
        }
    }

    fn new() -> Self {
        Scanner {
            text: None,
            pos: None,
            end: None,
            start_pos: None,
            token_pos: None,
            token: None,
        }
    }

    fn text(&self) -> &str {
        &self.text.unwrap()
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
        return token;
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
