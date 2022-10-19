use std::borrow::Cow;
use std::convert::TryInto;
use std::ops::Deref;
use std::rc::Rc;

use crate::Debug_;

#[derive(Debug)]
pub enum SourceText {
    AsciiOnly(SourceTextAsciiOnly),
    ContainsNonAscii(SourceTextContainsNonAscii),
}

impl SourceText {
    pub fn new(text: String) -> Self {
        if text.is_ascii() {
            Self::AsciiOnly(SourceTextAsciiOnly {
                text: Rc::new(text),
            })
        } else {
            let char_indices = text.char_indices().collect();
            Self::ContainsNonAscii(SourceTextContainsNonAscii {
                text: Rc::new(text),
                char_indices,
            })
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::AsciiOnly(value) => value.text.len(),
            Self::ContainsNonAscii(value) => value.char_indices.len(),
        }
    }

    pub fn str(&self) -> &str {
        match self {
            Self::AsciiOnly(value) => &*value.text,
            Self::ContainsNonAscii(value) => &*value.text,
        }
    }

    pub fn starts_with(&self, value: &str) -> bool {
        self.str().starts_with(value)
    }

    // pub fn slice(&self, start: usize, end: Option<usize>) -> SourceTextSlice {
    //     match self {
    //         Self::AsciiOnly(value) => match end {
    //             None => &value.text[start..],
    //             Some(end) => &value.text[start..end],
    //         },
    //         Self::ContainsNonAscii(value) => {
    //             let start_str = value.char_indices[start].0;
    //             match end {
    //                 None => &value.text[start_str..],
    //                 Some(end) => {
    //                     let end_str = value.char_indices[end].0;
    //                     &value.text[start_str..end_str]
    //                 }
    //             }
    //         }
    //     }
    // }

    pub fn slice(self: Rc<Self>, start: usize, end: Option<usize>) -> SourceTextSlice {
        SourceTextSlice::new(self, start, end)
    }

    pub fn slice_from_str_offsets(
        self: Rc<Self>,
        start: usize,
        end: Option<usize>,
    ) -> SourceTextSlice {
        self.slice(
            self.str_index_to_char_index(start),
            end.map(|end| self.str_index_to_char_index(end)),
        )
    }

    pub fn last_index_of(&self, ch: char, position: Option<usize>) -> Option<usize> {
        match self {
            Self::AsciiOnly(value) => {
                if let Some(position) = position {
                    value.text[0..=position].rfind(ch)
                } else {
                    value.text.rfind(ch)
                }
            }
            Self::ContainsNonAscii(value) => {
                if let Some(position) = position {
                    value.char_indices[0..=position]
                        .iter()
                        .rfind(ch)
                        .map(|char_index| char_index.0)
                } else {
                    value
                        .char_indices
                        .iter()
                        .rposition(|char_index| char_index.1 == ch)
                        .map(|char_index| char_index.0)
                }
            }
        }
    }

    pub fn last_index_of_returns_isize(&self, ch: char, position: Option<usize>) -> isize {
        self.last_index_of(ch, position)
            .map_or(-1, |index| index.try_into().unwrap())
    }

    pub fn maybe_char_at_index(&self, index: usize) -> Option<&str> {
        match self {
            Self::AsciiOnly(value) => value.text.get(index..index + 1),
            Self::ContainsNonAscii(value) => {
                let start_str = value.char_indices.get(index).map(|char_index| char_index.0);
                let end_str = value
                    .char_indices
                    .get(index + 1)
                    .map(|char_index| char_index.0);
                start_str.map(|start_str| match end_str {
                    None => &value.text[start_str..],
                    Some(end_str) => &value.text[start_str..end_str],
                })
            }
        }
    }

    pub fn char_at_index(&self, index: usize) -> &str {
        self.maybe_char_at_index(index).unwrap()
    }

    pub fn char_index_to_str_index(&self, index: usize) -> usize {
        match self {
            Self::AsciiOnly(_) => index,
            Self::ContainsNonAscii(value) => value.char_indices[index].0,
        }
    }

    pub fn str_index_to_char_index(&self, index: usize) -> usize {
        match self {
            Self::AsciiOnly(_) => index,
            Self::ContainsNonAscii(value) => value
                .char_indices
                .iter()
                .position(|char_index| char_index.0 == index)
                .map(|position| index + position)
                .expect("Str index didn't correspond to char boundary"),
        }
    }
}

impl Default for SourceText {
    fn default() -> Self {
        Self::AsciiOnly(SourceTextAsciiOnly {
            text: Rc::new("".to_owned()),
        })
    }
}

// this is tempting but allows for accidentally misusing the underlying &str?
// impl Deref for SourceText {
//     type Target = str;

//     fn deref(&self) -> &Self::Target {
//         match self {
//             Self::AsciiOnly(value) => &*value.text,
//             Self::ContainsNonAscii(value) => &*value.text,
//         }
//     }
// }

// this didn't work because I'd want to use the SliceIndex to both index into `char_indices` and
// `text` but its type apparently wouldn't allow that
// impl<TIndex> Index<TIndex> for SourceText
//     where TIndex: SliceIndex<str> {
//     type Output = TIndex::Output;

//     fn index(&self, index: TIndex) -> &Self::Output {
//         match self {
//             Self::AsciiOnly(value) => &value.text[index],
//             Self::ContainsNonAscii(value) => {
//                 let char_indices_slice = &value.char_indices[index];
//             }
//         }
//     }
// }

#[derive(Debug)]
pub struct SourceTextAsciiOnly {
    pub text: Rc<String>,
}

#[derive(Debug)]
pub struct SourceTextContainsNonAscii {
    pub text: Rc<String>,
    pub char_indices: Vec<(usize, char)>,
}

#[derive(Clone, Debug)]
pub struct SourceTextSlice {
    pub source_text: Rc<SourceText>,
    pub start: usize,
    pub end: Option<usize>,
    start_str: usize,
    end_str: Option<usize>,
}

impl SourceTextSlice {
    pub fn new(source_text: Rc<SourceText>, start: usize, end: Option<usize>) -> Self {
        let start_str = source_text.char_index_to_str_index(start);
        let end_str = end.map(|end| source_text.char_index_to_str_index(end));
        Self {
            source_text,
            start,
            end,
            start_str,
            end_str,
        }
    }

    pub fn len(&self) -> usize {
        match self.end {
            Some(end) => end - self.start,
            None => self.source_text.len() - self.start,
        }
    }

    pub fn slice_from_str_offsets(&self, start: usize, end: Option<usize>) -> SourceTextSlice {
        self.source_text
            .slice_from_str_offsets(self.start_str + start, end.map(|end| self.start_str + end))
    }

    pub fn str_index_to_char_index(&self, index: usize) -> usize {
        self.source_text
            .str_index_to_char_index(self.start_str + index)
    }

    pub fn extended(&self, end: Option<usize>) -> Self {
        Self::new(self.source_text.clone(), self.start, end)
    }
}

impl Deref for SourceTextSlice {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self.end_str {
            None => &self.source_text.str()[self.start_str..],
            Some(end_str) => &self.source_text.str()[self.start_str..end_str],
        }
    }
}

#[derive(Clone)]
pub enum SourceTextSliceOrStaticCow {
    SourceTextSlice(SourceTextSlice),
    StaticCow(Cow<'static, str>),
}

impl Deref for SourceTextSliceOrStaticCow {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::SourceTextSlice(value) => &**value,
            Self::StaticCow(value) => &**value,
        }
    }
}

impl From<SourceTextSlice> for SourceTextSliceOrStaticCow {
    fn from(value: SourceTextSlice) -> Self {
        Self::SourceTextSlice(value)
    }
}

impl From<&'static str> for SourceTextSliceOrStaticCow {
    fn from(value: &'static str) -> Self {
        Self::StaticCow(value.into())
    }
}

impl From<String> for SourceTextSliceOrStaticCow {
    fn from(value: String) -> Self {
        Self::StaticCow(value.into())
    }
}

impl From<Cow<'static, str>> for SourceTextSliceOrStaticCow {
    fn from(value: Cow<'static, str>) -> Self {
        Self::StaticCow(value)
    }
}

impl From<SourceTextSliceOrString> for SourceTextSliceOrStaticCow {
    fn from(value: SourceTextSliceOrString) -> Self {
        match value {
            SourceTextSliceOrString::SourceTextSlice(value) => Self::SourceTextSlice(value),
            SourceTextSliceOrString::String(value) => Self::StaticCow(value.into()),
        }
    }
}

impl From<SourceTextSliceOrStaticCow> for String {
    fn from(value: SourceTextSliceOrStaticCow) -> Self {
        match value {
            SourceTextSliceOrStaticCow::SourceTextSlice(value) => (*value).to_owned(),
            SourceTextSliceOrStaticCow::StaticCow(value) => value.into_owned(),
        }
    }
}

pub fn reduce_source_text_slice_or_static_cows(
    items: Vec<SourceTextSliceOrStaticCow>,
) -> SourceTextSliceOrStaticCow {
    let mut last_slice_end: Option<usize> = None;
    items
        .into_iter()
        .reduce(|a, b| match a {
            SourceTextSliceOrStaticCow::SourceTextSlice(a) => match b {
                SourceTextSliceOrStaticCow::SourceTextSlice(b) => {
                    Debug_.assert(
                        last_slice_end == Some(a.start),
                        Some("Source text slices should be contiguous"),
                    );
                    last_slice_end = b.end;
                    a.extended(b.end).into()
                }
                SourceTextSliceOrStaticCow::StaticCow(b) => format!("{}{}", &*a, b).into(),
            },
            SourceTextSliceOrStaticCow::StaticCow(a) => format!("{}{}", a, &*b).into(),
        })
        .unwrap()
}

pub enum SourceTextSliceOrStaticStr {
    SourceTextSlice(SourceTextSlice),
    StaticStr(&'static str),
}

impl From<SourceTextSlice> for SourceTextSliceOrStaticStr {
    fn from(value: SourceTextSlice) -> Self {
        Self::SourceTextSlice(value)
    }
}

impl From<&'static str> for SourceTextSliceOrStaticStr {
    fn from(value: &'static str) -> Self {
        Self::StaticStr(value.into())
    }
}

pub enum SourceTextSliceOrString {
    SourceTextSlice(SourceTextSlice),
    String(String),
}

impl From<SourceTextSlice> for SourceTextSliceOrString {
    fn from(value: SourceTextSlice) -> Self {
        Self::SourceTextSlice(value)
    }
}

impl From<String> for SourceTextSliceOrString {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl Deref for SourceTextSliceOrString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::SourceTextSlice(value) => &**value,
            Self::String(value) => &**value,
        }
    }
}
