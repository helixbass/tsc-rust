use std::borrow::{Borrow, Cow};
use std::cmp::PartialEq;
use std::convert::TryInto;
use std::hash::{Hash, Hasher};
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
                        .rfind(|char_index| char_index.1 == ch)
                        .map(|char_index| char_index.0)
                } else {
                    value
                        .char_indices
                        .iter()
                        .rfind(|char_index| char_index.1 == ch)
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
        self.source_text.slice_from_str_offsets(
            self.start_str + start,
            match end {
                None => self.end_str,
                Some(end) => Some(self.start_str + end),
            },
        )
    }

    pub fn str_index_to_char_index(&self, index: usize) -> usize {
        self.source_text
            .str_index_to_char_index(self.start_str + index)
    }

    pub fn extended(&self, end: Option<usize>) -> Self {
        Self::new(self.source_text.clone(), self.start, end)
    }

    pub fn slice(&self, start: usize, end: Option<usize>) -> SourceTextSlice {
        self.source_text.slice(
            self.start + start,
            match end {
                None => self.end,
                Some(end) => Some(self.start + end),
            },
        )
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

pub fn join_source_text_slices<'iter, TItems: IntoIterator<Item = &'iter SourceTextSlice>>(
    items: TItems,
) -> SourceTextSliceOrString {
    let items = items.into_iter();
    let first_item = items.next();
    if first_item.is_none() {
        return "".into();
    }
    let first_item = first_item.unwrap();
    let mut last_slice_end: Option<usize> = first_item.end;
    let mut result: SourceTextSlice = first_item.clone();
    loop {
        let item = items.next();
        match item {
            None => {
                return result.into();
            }
            Some(item) => {
                if Some(item.start) == last_slice_end {
                    last_slice_end = item.end;
                    result = result.extended(item.end);
                } else {
                    use itertools::Itertools;
                    return format!("{}{}", &*result, items.map(|item| &**item).concat()).into();
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum SourceTextSliceOrString {
    SourceTextSlice(SourceTextSlice),
    StaticStr(&'static str),
    String(Rc<String>),
}

impl SourceTextSliceOrString {
    pub fn escape_leading_underscores(self) -> Self {
        if self.starts_with("__") {
            format!("_{}", &*self).into()
        } else {
            self
        }
    }

    // pub fn unescape_leading_underscores(&self) -> Cow<'_, Self> {
    //     if self.starts_with("___") {
    //         match self {
    //             Self::SourceTextSlice(value) => SourceTextSlice::new(value.source_text.clone(), value.start + 1, value.end).into(),
    //             Self::StaticStr(value) => Self::StaticStr(&value[1..]),
    //             // TODO: this is actually doing extra allocation vs "simple"
    //             // unescape_leading_underscores() never allocates?
    //             Self::String(value) => Self::String(Rc::new(value[1..].to_owned())),
    //         }
    //     } else {
    //         self.into()
    //     }
    // }

    pub fn len(&self) -> usize {
        match self {
            Self::SourceTextSlice(value) => value.len(),
            Self::StaticStr(value) => value.chars().count(),
            Self::String(value) => value.chars().count(),
        }
    }
}

impl Deref for SourceTextSliceOrString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::SourceTextSlice(value) => &**value,
            Self::StaticStr(value) => *value,
            Self::String(value) => &***value,
        }
    }
}

impl PartialEq<&str> for SourceTextSliceOrString {
    fn eq(&self, other: &&str) -> bool {
        &**self == *other
    }
}

impl PartialEq for SourceTextSliceOrString {
    fn eq(&self, other: &SourceTextSliceOrString) -> bool {
        &**self == &**other
    }
}

impl Eq for SourceTextSliceOrString {}

impl Hash for SourceTextSliceOrString {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        (&**self).hash(state)
    }
}

impl Borrow<str> for SourceTextSliceOrString {
    fn borrow(&self) -> &str {
        &**self
    }
}

impl From<SourceTextSlice> for SourceTextSliceOrString {
    fn from(value: SourceTextSlice) -> Self {
        Self::SourceTextSlice(value)
    }
}

impl From<&'static str> for SourceTextSliceOrString {
    fn from(value: &'static str) -> Self {
        Self::StaticStr(value)
    }
}

impl From<String> for SourceTextSliceOrString {
    fn from(value: String) -> Self {
        Self::String(Rc::new(value))
    }
}

impl From<Cow<'static, str>> for SourceTextSliceOrString {
    fn from(value: Cow<'static, str>) -> Self {
        match value {
            Cow::Owned(value) => Self::String(Rc::new(value)),
            Cow::Borrowed(value) => Self::StaticStr(value),
        }
    }
}

impl From<StrOrSourceTextSliceOrString<'_>> for SourceTextSliceOrString {
    fn from(value: StrOrSourceTextSliceOrString<'_>) -> Self {
        match value {
            StrOrSourceTextSliceOrString::Str(value) => Self::String(value.to_owned()),
            StrOrSourceTextSliceOrString::SourceTextSliceOrString(value) => value,
        }
    }
}

// impl From<SourceTextSliceOrString> for String {
//     fn from(value: SourceTextSliceOrString) -> Self {
//         match value {
//             SourceTextSliceOrString::SourceTextSlice(value) => (*value).to_owned(),
//             SourceTextSliceOrString::StaticStr(value) => value.to_owned(),
//             SourceTextSliceOrString::String(value) => value,
//         }
//     }
// }

pub fn reduce_source_text_slice_or_strings<TItems: IntoIterator<Item = SourceTextSliceOrString>>(
    items: TItems,
    should_assert_consecutive_slices: bool,
) -> SourceTextSliceOrString {
    let mut last_slice_end: Option<usize> = None;
    items
        .into_iter()
        .reduce(|a, b| match a {
            SourceTextSliceOrString::SourceTextSlice(a) => match b {
                SourceTextSliceOrString::SourceTextSlice(b) => {
                    if should_assert_consecutive_slices {
                        Debug_.assert(
                            last_slice_end == Some(a.start),
                            Some("Source text slices should be contiguous"),
                        );
                    }
                    last_slice_end = b.end;
                    a.extended(b.end).into()
                }
                SourceTextSliceOrString::StaticStr(b) => format!("{}{}", &*a, b).into(),
                SourceTextSliceOrString::String(b) => format!("{}{}", &*a, b).into(),
            },
            SourceTextSliceOrString::StaticStr(a) => format!("{}{}", a, &*b).into(),
            SourceTextSliceOrString::String(a) => format!("{}{}", a, &*b).into(),
        })
        .unwrap()
}

pub fn reduce_source_text_slice_or_string_refs<
    'iter,
    TItems: IntoIterator<Item = &'iter SourceTextSliceOrString>,
>(
    items: TItems,
    should_assert_consecutive_slices: bool,
) -> SourceTextSliceOrString {
    reduce_source_text_slice_or_strings(items.cloned(), should_assert_consecutive_slices)
}

pub enum StrOrSourceTextSliceOrString<'str> {
    Str(&'str str),
    SourceTextSliceOrString(SourceTextSliceOrString),
}

impl Deref for StrOrSourceTextSliceOrString<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Str(value) => *value,
            Self::SourceTextSliceOrString(value) => &**value,
        }
    }
}

impl<'str> From<&'str str> for StrOrSourceTextSliceOrString<'str> {
    fn from(value: &'str str) -> Self {
        Self::Str(value)
    }
}

impl<'str> From<SourceTextSliceOrString> for StrOrSourceTextSliceOrString<'str> {
    fn from(value: SourceTextSliceOrString) -> Self {
        Self::SourceTextSliceOrString(value)
    }
}
