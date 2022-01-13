use std::ptr;

use crate::SortedArray;

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

pub fn insert_sorted<TItem>(array: &mut SortedArray<TItem>, insert: TItem) {
    if array.is_empty() {
        array.push(insert);
        return;
    }
    unimplemented!()
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
