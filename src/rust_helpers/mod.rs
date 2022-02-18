use std::convert::TryInto;
use std::mem;

use crate::SourceTextAsChars;

pub mod number;
pub mod weak_self;

pub fn is_same_variant<TEnum>(value: &TEnum, other_value: &TEnum) -> bool {
    mem::discriminant(value) == mem::discriminant(other_value)
}

pub fn last_index_of(text: &SourceTextAsChars, ch: char, position: usize) -> isize {
    let mut index = position;
    while index >= 0 {
        if text[index] == ch {
            return index.try_into().unwrap();
        }
        index -= 1;
    }
    -1
}
