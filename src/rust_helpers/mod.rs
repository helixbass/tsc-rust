use std::convert::TryInto;
use std::mem;
use std::rc::Rc;

pub mod number;
pub mod weak_self;

pub fn is_same_variant<TEnum>(value: &TEnum, other_value: &TEnum) -> bool {
    mem::discriminant(value) == mem::discriminant(other_value)
}

pub fn last_index_of<TItem, TComparer: FnMut(&TItem, &TItem) -> bool>(
    slice: &[TItem],
    item: &TItem,
    mut comparer: TComparer,
    position: usize,
) -> isize {
    let mut index = position;
    while index >= 0 {
        if comparer(&slice[index], item) {
            return index.try_into().unwrap();
        }
        index -= 1;
    }
    -1
}

pub fn index_of<TItem, TComparer: FnMut(&TItem, &TItem) -> bool>(
    slice: &[TItem],
    item: &TItem,
    mut comparer: TComparer,
) -> isize {
    let mut index = 0;
    while index <= slice.len() - 1 {
        if comparer(&slice[index], item) {
            return index.try_into().unwrap();
        }
        index += 1;
    }
    -1
}

pub fn index_of_rc<TItem>(slice: &[Rc<TItem>], item: &Rc<TItem>) -> isize {
    index_of(slice, item, |a: &Rc<TItem>, b: &Rc<TItem>| Rc::ptr_eq(a, b))
}

pub fn are_option_rcs_equal<TItem>(a: Option<&Rc<TItem>>, b: Option<&Rc<TItem>>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}
