use std::{
    convert::{TryFrom, TryInto},
    mem,
    rc::Rc,
};

use gc::{Finalize, Gc, Trace};

pub mod cell;
pub mod combinators;
pub mod debugging;
pub mod default;
pub mod deref;
pub mod hash_map;
pub mod io;
pub mod iterator;
pub mod number;
pub mod option;
pub mod sys;
pub mod uri;
pub mod vec;
pub mod weak_self;

pub fn is_same_variant<TEnum>(value: &TEnum, other_value: &TEnum) -> bool {
    mem::discriminant(value) == mem::discriminant(other_value)
}

pub fn last_index_of<TItem, TComparer: FnMut(&TItem, &TItem) -> bool>(
    slice: &[TItem],
    item: &TItem,
    mut comparer: TComparer,
    position: Option<usize>,
) -> Option<usize> {
    if slice.is_empty() {
        return None;
    }
    let mut index = position.unwrap_or_else(|| slice.len() - 1);
    loop {
        if comparer(&slice[index], item) {
            return Some(index);
        }
        if index == 0 {
            return None;
        }
        index -= 1;
    }
}

pub fn last_index_of_returns_isize<TItem, TComparer: FnMut(&TItem, &TItem) -> bool>(
    slice: &[TItem],
    item: &TItem,
    comparer: TComparer,
    position: Option<usize>,
) -> isize {
    match last_index_of(slice, item, comparer, position) {
        None => -1,
        Some(value) => value.try_into().unwrap(),
    }
}

pub fn index_of<TItem, TComparer: FnMut(&TItem, &TItem) -> bool>(
    slice: &[TItem],
    item: &TItem,
    mut comparer: TComparer,
) -> isize {
    let mut index: isize = 0;
    #[allow(clippy::int_plus_one)]
    while index <= isize::try_from(slice.len()).unwrap() - 1 {
        if comparer(&slice[usize::try_from(index).unwrap()], item) {
            return index;
        }
        index += 1;
    }
    -1
}

pub fn index_of_rc<TItem>(slice: &[Rc<TItem>], item: &Rc<TItem>) -> isize {
    index_of(slice, item, |a: &Rc<TItem>, b: &Rc<TItem>| Rc::ptr_eq(a, b))
}

pub fn index_of_gc<TItem: Trace + Finalize>(slice: &[Gc<TItem>], item: &Gc<TItem>) -> isize {
    index_of(slice, item, |a: &Gc<TItem>, b: &Gc<TItem>| Gc::ptr_eq(a, b))
}

pub fn are_option_rcs_equal<TItem: ?Sized>(a: Option<&Rc<TItem>>, b: Option<&Rc<TItem>>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

pub fn are_option_gcs_equal<TItem: Trace + Finalize>(
    a: Option<&Gc<TItem>>,
    b: Option<&Gc<TItem>>,
) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) => Gc::ptr_eq(a, b),
        _ => false,
    }
}

pub fn are_rc_slices_equal<TItem>(a: &[Rc<TItem>], b: &[Rc<TItem>]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    for (index, a_item) in a.iter().enumerate() {
        if !Rc::ptr_eq(a_item, &b[index]) {
            return false;
        }
    }
    true
}

pub fn are_gc_slices_equal<TItem: Trace + Finalize>(a: &[Gc<TItem>], b: &[Gc<TItem>]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    for (index, a_item) in a.iter().enumerate() {
        if !Gc::ptr_eq(a_item, &b[index]) {
            return false;
        }
    }
    true
}

// https://stackoverflow.com/a/38406885
pub fn capitalize(string: &str) -> String {
    let mut c = string.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

pub fn uncapitalize(string: &str) -> String {
    let mut c = string.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_lowercase().collect::<String>() + c.as_str(),
    }
}

#[derive(Copy, Clone, Debug)]
pub enum UsizeOrNegativeInfinity {
    Usize(usize),
    NegativeInfinity,
}

pub fn push_or_replace<TValue>(vec: &mut Vec<TValue>, index: usize, value: TValue) {
    if index >= vec.len() {
        vec.push(value);
    } else {
        vec[index] = value;
    }
}

pub fn is_option_str_empty(value: Option<&str>) -> bool {
    !matches!(
        value,
        Some(value) if !value.is_empty()
    )
}

pub fn typed_as<TValue>(value: TValue) -> TValue {
    value
}

pub trait IntoA {
    fn into_a<TTarget>(self) -> TTarget
    where
        Self: Into<TTarget> + Sized;
}

impl<T> IntoA for T {
    fn into_a<TTarget>(self) -> TTarget
    where
        Self: Into<TTarget> + Sized,
    {
        self.into()
    }
}
