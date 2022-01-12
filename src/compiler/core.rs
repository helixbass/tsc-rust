use std::convert::TryInto;
use std::ptr;

use crate::{Comparison, SortedArray};

pub fn for_each<
    TCollection: IntoIterator,
    TReturn,
    TCallback: FnMut(TCollection::Item, usize) -> Option<TReturn>,
>(
    array: TCollection,
    mut callback: TCallback,
) -> Option<TReturn> {
    array
        .into_iter()
        .enumerate()
        .find_map(|(index, item)| callback(item, index))
}

pub fn maybe_for_each<
    TCollection: IntoIterator,
    TReturn,
    TCallback: FnMut(TCollection::Item, usize) -> Option<TReturn>,
>(
    array: Option<TCollection>,
    callback: TCallback,
) -> Option<TReturn> {
    match array {
        Some(array) => for_each(array, callback),
        None => None,
    }
}

pub fn first_defined<
    TCollection: IntoIterator,
    TReturn,
    TCallback: FnMut(TCollection::Item, usize) -> Option<TReturn>,
>(
    array: TCollection,
    callback: TCallback,
) -> Option<TReturn> {
    for_each(array, callback)
}

pub fn every<TItem, TCallback: FnMut(&TItem, usize) -> bool>(
    array: &[TItem],
    mut predicate: TCallback,
) -> bool {
    array
        .into_iter()
        .enumerate()
        .all(|(index, value)| predicate(value, index))
}

pub fn map<
    TCollection: IntoIterator,
    TReturn,
    TCallback: FnMut(TCollection::Item, usize) -> TReturn,
>(
    array: Option<TCollection>,
    mut f: TCallback,
) -> Option<Vec<TReturn>> {
    let mut result: Option<Vec<_>> = None;
    if let Some(array) = array {
        let mut some_result = vec![];
        for (i, item) in array.into_iter().enumerate() {
            some_result.push(f(item, i));
        }
        result = Some(some_result);
    }
    result
}

pub fn some<TItem>(array: &[TItem], predicate: Option<Box<dyn FnMut(&TItem) -> bool>>) -> bool {
    predicate.map_or(!array.is_empty(), |predicate| array.iter().any(predicate))
}

pub fn concatenate<TItem>(mut array1: Vec<TItem>, mut array2: Vec<TItem>) -> Vec<TItem> {
    if !some(&array2, None) {
        return array1;
    }
    if !some(&array1, None) {
        return array2;
    }
    array1.append(&mut array2);
    array1
}

fn identity_key_selector<TItem>(item: TItem, _: Option<usize>) -> TItem {
    item
}

pub fn insert_sorted<TItem /*, TComparer: Comparer<&'array_or_item TItem>*/>(
    array: &mut SortedArray<TItem>,
    insert: TItem,
    // compare: Comparer<&'array_or_item TItem>,
    // compare: Comparer<&TItem, &TItem>,
    compare: fn(&TItem, &TItem) -> Comparison,
) {
    if array.is_empty() {
        array.push(insert);
        return;
    }

    // let insert_index = binary_search(array, &insert, identity_key_selector, compare, None);
    let insert_index = binary_search(array, &insert, |item, _| item, compare, None);
    if insert_index < 0 {
        array.insert((!insert_index).try_into().unwrap(), insert);
    }
}

fn push_if_unique<TItem>(array: &mut Vec<TItem>, to_add: TItem) -> bool {
    if false {
        unimplemented!()
    } else {
        array.push(to_add);
        true
    }
}

pub fn append_if_unique<TItem>(array: Option<Vec<TItem>>, to_add: TItem) -> Vec<TItem> {
    if let Some(mut array) = array {
        push_if_unique(&mut array, to_add);
        array
    } else {
        vec![to_add]
    }
}

pub fn range_equals<TItem>(array1: &[TItem], array2: &[TItem], mut pos: usize, end: usize) -> bool {
    while pos < end {
        if !ptr::eq(&array1[pos], &array2[pos]) {
            return false;
        }
        pos += 1;
    }
    true
}

pub fn first_or_undefined<TItem>(array: &[TItem]) -> Option<&TItem> {
    array.first()
}

pub fn last_or_undefined<TItem>(array: &[TItem]) -> Option<&TItem> {
    array.last()
}

pub fn binary_search<
    TKey,
    TItem,
    TKeySelector: Fn(&TItem, Option<usize>) -> &TKey,
    // TComparer: Comparer<TKey>,
>(
    array: &[TItem],
    value: &TItem,
    // key_selector: fn(&TItem, Option<usize>) -> &TKey,
    key_selector: TKeySelector,
    // key_comparer: TComparer,
    // key_comparer: Comparer<&'array TKey>,
    // key_comparer: Comparer<&TKey, &TKey>,
    key_comparer: fn(&TKey, &TKey) -> Comparison,
    offset: Option<usize>,
) -> isize
// where
//     for<'key> &'key TKey: Clone,
{
    binary_search_key(
        array,
        // key_selector(value, None),
        value,
        key_selector,
        key_comparer,
        offset,
    )
}

pub fn binary_search_copy_key<
    TKey: Copy,
    TItem,
    TKeySelector: Fn(&TItem, Option<usize>) -> TKey,
    // TComparer: Comparer<TKey>,
>(
    array: &[TItem],
    value: &TItem,
    key_selector: TKeySelector,
    key_comparer: fn(TKey, TKey) -> Comparison,
    offset: Option<usize>,
) -> isize {
    binary_search_key_copy_key(
        array,
        key_selector(value, None),
        key_selector,
        key_comparer,
        offset,
    )
}

fn binary_search_key<
    TItem,
    TKey,
    TKeySelector: Fn(&TItem, Option<usize>) -> &TKey,
    // TComparer: Comparer<TKey>,
>(
    array: &[TItem],
    item: &TItem,
    // key: &TSearchedKey,
    key_selector: TKeySelector,
    // key_comparer: TComparer,
    // key_comparer: Comparer<&TKey>,
    key_comparer: fn(&TKey, &TKey) -> Comparison,
    offset: Option<usize>,
) -> isize
// where
//     for<'key> &'key TKey: Clone,
{
    let key = key_selector(item, None);
    if array.is_empty()
    /* !some(array)*/
    {
        return -1;
    }

    let mut low = offset.unwrap_or(0);
    let mut high = array.len() - 1;
    while low <= high {
        let middle = low + ((high - low) >> 1);
        let mid_key = key_selector(&array[middle], Some(middle));
        match key_comparer(mid_key, key) {
            Comparison::LessThan => {
                low = middle + 1;
            }
            Comparison::EqualTo => {
                return middle.try_into().unwrap();
            }
            Comparison::GreaterThan => {
                high = middle - 1;
            }
        }
    }

    let low: isize = low.try_into().unwrap();
    !low
}

fn binary_search_key_copy_key<
    TItem,
    TKey: Copy,
    TKeySelector: Fn(&TItem, Option<usize>) -> TKey,
>(
    array: &[TItem],
    key: TKey,
    key_selector: TKeySelector,
    key_comparer: fn(TKey, TKey) -> Comparison,
    offset: Option<usize>,
) -> isize {
    if array.is_empty()
    /* !some(array)*/
    {
        return -1;
    }

    let mut low: isize = offset.unwrap_or(0).try_into().unwrap();
    let mut high: isize = (array.len() - 1).try_into().unwrap();
    while low <= high {
        let middle: usize = (low + ((high - low) >> 1)).try_into().unwrap();
        let mid_key = key_selector(&array[middle], Some(middle));
        let middle: isize = middle.try_into().unwrap();
        match key_comparer(mid_key, key) {
            Comparison::LessThan => {
                low = middle + 1;
            }
            Comparison::EqualTo => {
                return middle;
            }
            Comparison::GreaterThan => {
                high = middle - 1;
            }
        }
    }

    !low
}

fn identity<TValue>(x: TValue) -> TValue {
    x
}

fn compare_comparable_values<TValue: Eq + Ord>(
    a: Option<TValue>, /*number | string | undefined*/
    b: Option<TValue>, /*number | string | undefined*/
) -> Comparison {
    if let Some(a) = a.as_ref() {
        if let Some(b) = b.as_ref() {
            if *a == *b {
                return Comparison::EqualTo;
            }
        }
    }
    if a.is_none() {
        return Comparison::LessThan;
    } else if b.is_none() {
        return Comparison::GreaterThan;
    }
    let a = a.unwrap();
    let b = b.unwrap();
    if a < b {
        Comparison::LessThan
    } else {
        Comparison::GreaterThan
    }
}

pub fn compare_values<TValue: Eq + Ord>(
    a: Option<TValue>, /*number | undefined*/
    b: Option<TValue>, /*number | undefined*/
) -> Comparison {
    compare_comparable_values(a, b)
}

pub fn compare_strings_case_sensitive(a: Option<&str>, b: Option<&str>) -> Comparison {
    compare_comparable_values(a, b)
}
