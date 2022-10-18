use regex::{Matches, Regex};

// copied from regex::Regex::split()
pub fn split_matches<'regex, 'text>(
    regex: &'regex Regex,
    text: &'text str,
) -> SplitMatches<'regex, 'text> {
    SplitMatches {
        finder: regex.find_iter(text),
        last: 0,
    }
}

pub struct SplitMatches<'regex, 'text> {
    finder: Matches<'regex, 'text>,
    last: usize,
}

impl<'regex, 'text> Iterator for SplitMatches<'regex, 'text> {
    type Item = Match<'text>;

    fn next(&mut self) -> Option<Match<'text>> {
        let text = self.finder.0.text();
        match self.finder.next() {
            None => {
                if self.last > text.len() {
                    None
                } else {
                    let start = self.last;
                    let s = &text[start..];
                    self.last = text.len() + 1;
                    Some(Match {
                        text: s,
                        start,
                        end: None,
                    })
                }
            }
            Some(m) => {
                let start = self.last;
                let end = m.start();
                let matched = &text[start..end];
                self.last = m.end();
                Some(Match {
                    text: matched,
                    start,
                    end: Some(end),
                })
            }
        }
    }
}

pub struct Match<'text> {
    pub text: &'text str,
    pub start: usize,
    pub end: Option<usize>,
}
