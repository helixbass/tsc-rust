use regex::Regex;
use std::cmp::Ordering;
use std::convert::{TryFrom, TryInto};
use std::ptr;

use crate::{Comparison, Debug_, SortedArray};

pub fn length<TItem>(array: Option<&[TItem]>) -> usize {
    array.map_or(0, |array| array.len())
}

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

pub fn find<TItem, TCallback: FnMut(&TItem, usize) -> bool>(
    array: &[TItem],
    mut predicate: TCallback,
) -> Option<&TItem> {
    array
        .into_iter()
        .enumerate()
        .find(|(index, value)| predicate(value, *index))
        .map(|(_, value)| value)
}

pub fn arrays_equal<TItem: Eq>(a: &[TItem], b: &[TItem]) -> bool {
    // TODO: separate eg arrays_equal_by() helper taking equality_comparer callback and not imposing `Eq` bound?
    a.len() == b.len() && every(a, |item_a, i| *item_a == b[i])
}

pub fn filter<TItem: Clone, TCallback: FnMut(&TItem) -> bool>(
    array: Option<&[TItem]>,
    mut predicate: TCallback,
) -> Option<Vec<TItem>> {
    array.map(|array| {
        array
            .into_iter()
            .filter(|item| predicate(item))
            .map(Clone::clone)
            .collect()
    })
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

pub fn some<TItem, TPredicate: FnMut(&TItem) -> bool>(
    array: Option<&[TItem]>,
    predicate: Option<TPredicate>,
) -> bool {
    array.map_or(false, |array| {
        predicate.map_or(!array.is_empty(), |predicate| array.iter().any(predicate))
    })
}

pub fn concatenate<TItem>(mut array1: Vec<TItem>, mut array2: Vec<TItem>) -> Vec<TItem> {
    if !some(Some(&array2), Option::<fn(&TItem) -> bool>::None) {
        return array1;
    }
    if !some(Some(&array1), Option::<fn(&TItem) -> bool>::None) {
        return array2;
    }
    array1.append(&mut array2);
    array1
}

enum ComparerOrEqualityComparer<'closure, TItem> {
    Comparer(&'closure dyn Fn(&TItem, &TItem) -> Comparison),
    EqualityComparer(&'closure dyn Fn(&TItem, &TItem) -> bool),
}

fn deduplicate_sorted<TItem: Clone>(
    array: &SortedArray<TItem>,
    comparer: ComparerOrEqualityComparer<TItem>,
) -> SortedArray<TItem> {
    if array.is_empty() {
        return SortedArray::new(vec![]);
    }

    let mut last = &array[0];
    let mut deduplicated = vec![last.clone()];
    for next in array.iter().skip(1) {
        match comparer {
            ComparerOrEqualityComparer::Comparer(comparer) => match comparer(next, last) {
                Comparison::EqualTo => {
                    continue;
                }
                Comparison::LessThan => Debug_.fail(Some("Array is unsorted.")),
                Comparison::GreaterThan => (),
            },
            ComparerOrEqualityComparer::EqualityComparer(equality_comparer) => {
                match equality_comparer(next, last) {
                    true => {
                        continue;
                    }
                    false => (),
                }
            }
        }

        last = next;
        deduplicated.push(next.clone());
    }

    SortedArray::new(deduplicated)
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

    let insert_index = binary_search(array, &insert, |item, _| item, compare, None);
    if insert_index < 0 {
        array.insert((!insert_index).try_into().unwrap(), insert);
    }
}

pub fn sort_and_deduplicate<
    TItem: Clone,
    TComparer: Fn(&TItem, &TItem) -> Comparison,
    TEqualityComparer: Fn(&TItem, &TItem) -> bool,
>(
    array: &[TItem],
    comparer: /*Option<*/ &TComparer, /*>*/
    equality_comparer: Option<&TEqualityComparer>,
) -> SortedArray<TItem> {
    deduplicate_sorted(
        &sort(array, comparer),
        match equality_comparer {
            Some(equality_comparer) => {
                ComparerOrEqualityComparer::EqualityComparer(equality_comparer)
            }
            None => ComparerOrEqualityComparer::Comparer(comparer),
        },
    )
}

pub fn append<TItem>(to: &mut Vec<TItem>, value: Option<TItem>) {
    if value.is_none() {
        return /*to*/;
    }
    let value = value.unwrap();
    // if to === undefined
    to.push(value);
    /*to*/
}

fn to_offset<TItem>(array: &[TItem], offset: isize) -> usize {
    if offset < 0 {
        (isize::try_from(array.len()).unwrap() + offset)
            .try_into()
            .unwrap()
    } else {
        offset.try_into().unwrap()
    }
}

pub fn add_range<TItem: Clone>(
    to: /*Option<*/ &mut Vec<TItem>, /*>*/
    from: Option<&[TItem]>,
    start: Option<isize>,
    end: Option<isize>,
) /*-> Option<Vec<TItem>>*/
{
    if from.is_none() {
        return /*to*/;
    }
    let from = from.unwrap();
    if from.is_empty() {
        return /*to*/;
    }
    // if to.is_none()
    let start: usize = match start {
        None => 0,
        Some(start) => to_offset(from, start),
    };
    let end: usize = match end {
        None => from.len(),
        Some(end) => to_offset(from, end),
    };
    let mut i = start;
    while i < end && i < from.len() {
        // if from[i] !== undefined
        to.push(from[i].clone());
        i += 1;
    }
    // to
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

fn comparison_to_ordering(comparison: Comparison) -> Ordering {
    match comparison {
        Comparison::EqualTo => Ordering::Equal,
        Comparison::LessThan => Ordering::Less,
        Comparison::GreaterThan => Ordering::Greater,
    }
}

// fn comparer_to_orderer<
//     'comparer,
//     TItem: Clone,
//     TComparer: Fn(&TItem, &TItem) -> Comparison + 'comparer,
// >(
//     comparer: TComparer,
// ) -> impl Fn(&TItem, &TItem) -> Ordering + 'comparer {
//     |a, b| comparison_to_ordering(comparer(a, b))
// }

fn sort<TItem: Clone, TComparer: Fn(&TItem, &TItem) -> Comparison>(
    array: &[TItem],
    comparer: TComparer,
) -> SortedArray<TItem> {
    SortedArray::new(if array.len() == 0 {
        vec![]
    } else {
        let mut array: Vec<TItem> = array.to_vec();
        array.sort_by(|a, b| comparison_to_ordering(comparer(a, b)));
        array
    })
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

pub fn last<TItem>(array: &[TItem]) -> &TItem {
    Debug_.assert(!array.is_empty(), None);
    array.last().unwrap()
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

pub fn trim_string_start(s: &str) -> String {
    lazy_static! {
        static ref regex: Regex = Regex::new(r"^\s+").unwrap();
    }
    regex.replace_all(s, "").to_string()
}
