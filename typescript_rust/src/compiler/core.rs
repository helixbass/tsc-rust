use gc::{Finalize, Gc, Trace};
use indexmap::IndexMap;
use regex::{Captures, Regex};
use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::convert::{TryFrom, TryInto};
use std::hash::Hash;
use std::mem;
use std::rc::Rc;
use std::{hash, iter};

use crate::{
    __String, text_char_at_index, Comparison, Debug_, IteratorExt, Node, NodeArrayOrVec, OptionTry,
    PeekableExt, SortedArray, SourceTextAsChars,
};

pub fn length<TItem>(array: Option<&[TItem]>) -> usize {
    array.map_or(0, |array| array.len())
}

pub fn for_each<TCollection: IntoIterator, TReturn>(
    array: TCollection,
    mut callback: impl FnMut(TCollection::Item, usize) -> Option<TReturn>,
) -> Option<TReturn> {
    array
        .into_iter()
        .enumerate()
        .find_map(|(index, item)| callback(item, index))
}

pub fn try_for_each<TCollection: IntoIterator, TReturn, TError>(
    array: TCollection,
    mut callback: impl FnMut(TCollection::Item, usize) -> Result<Option<TReturn>, TError>,
) -> Result<Option<TReturn>, TError> {
    for (index, item) in array.into_iter().enumerate() {
        let result = callback(item, index)?;
        if result.is_some() {
            return Ok(result);
        }
    }
    Ok(None)
}

pub fn for_each_bool<TCollection: IntoIterator>(
    array: TCollection,
    mut callback: impl FnMut(TCollection::Item, usize) -> bool,
) -> bool {
    array
        .into_iter()
        .enumerate()
        .any(|(index, item)| callback(item, index))
}

pub fn try_for_each_bool<TCollection: IntoIterator, TError>(
    array: TCollection,
    mut callback: impl FnMut(TCollection::Item, usize) -> Result<bool, TError>,
) -> Result<bool, TError> {
    array
        .into_iter()
        .enumerate()
        .try_any(|(index, item)| callback(item, index))
}

pub fn maybe_for_each<TCollection: IntoIterator, TReturn>(
    array: Option<TCollection>,
    callback: impl FnMut(TCollection::Item, usize) -> Option<TReturn>,
) -> Option<TReturn> {
    match array {
        Some(array) => for_each(array, callback),
        None => None,
    }
}

pub fn try_maybe_for_each<TCollection: IntoIterator, TReturn, TError>(
    array: Option<TCollection>,
    callback: impl FnMut(TCollection::Item, usize) -> Result<Option<TReturn>, TError>,
) -> Result<Option<TReturn>, TError> {
    match array {
        Some(array) => try_for_each(array, callback),
        None => Ok(None),
    }
}

pub fn maybe_for_each_bool<TCollection: IntoIterator>(
    array: Option<TCollection>,
    callback: impl FnMut(TCollection::Item, usize) -> bool,
) -> bool {
    match array {
        Some(array) => for_each_bool(array, callback),
        None => false,
    }
}

pub fn first_defined<TCollection: IntoIterator, TReturn>(
    array: TCollection,
    callback: impl FnMut(TCollection::Item, usize) -> Option<TReturn>,
) -> Option<TReturn> {
    for_each(array, callback)
}

pub fn try_first_defined<TCollection: IntoIterator, TReturn, TError>(
    array: TCollection,
    callback: impl FnMut(TCollection::Item, usize) -> Result<Option<TReturn>, TError>,
) -> Result<Option<TReturn>, TError> {
    try_for_each(array, callback)
}

pub fn maybe_first_defined<TCollection: IntoIterator, TReturn>(
    array: Option<TCollection>,
    callback: impl FnMut(TCollection::Item, usize) -> Option<TReturn>,
) -> Option<TReturn> {
    maybe_for_each(array, callback)
}

pub fn try_maybe_first_defined<TCollection: IntoIterator, TReturn, TError>(
    array: Option<TCollection>,
    callback: impl FnMut(TCollection::Item, usize) -> Result<Option<TReturn>, TError>,
) -> Result<Option<TReturn>, TError> {
    try_maybe_for_each(array, callback)
}

pub fn every<TItem>(array: &[TItem], mut predicate: impl FnMut(&TItem, usize) -> bool) -> bool {
    array
        .into_iter()
        .enumerate()
        .all(|(index, value)| predicate(value, index))
}

pub fn try_every<TItem, TError>(
    array: &[TItem],
    mut predicate: impl FnMut(&TItem, usize) -> Result<bool, TError>,
) -> Result<bool, TError> {
    array
        .into_iter()
        .enumerate()
        .try_all(|(index, value)| predicate(value, index))
}

pub fn maybe_every<TItem>(
    array: Option<&[TItem]>,
    predicate: impl FnMut(&TItem, usize) -> bool,
) -> bool {
    array.map_or(false, |array| every(array, predicate))
}

pub fn try_maybe_every<TItem, TError>(
    array: Option<&[TItem]>,
    predicate: impl FnMut(&TItem, usize) -> Result<bool, TError>,
) -> Result<bool, TError> {
    array.try_map_or(false, |array| try_every(array, predicate))
}

pub fn find<TItem>(
    array: &[TItem],
    mut predicate: impl FnMut(&TItem, usize) -> bool,
) -> Option<&TItem> {
    array
        .into_iter()
        .enumerate()
        .find(|(index, value)| predicate(value, *index))
        .map(|(_, value)| value)
}

pub fn try_find<TItem, TError>(
    array: &[TItem],
    mut predicate: impl FnMut(&TItem, usize) -> Result<bool, TError>,
) -> Result<Option<&TItem>, TError> {
    Ok(array
        .into_iter()
        .enumerate()
        .try_find_(|(index, value)| predicate(value, *index))?
        .map(|(_, value)| value))
}

pub fn find_last<TItem>(
    array: &[TItem],
    mut predicate: impl FnMut(&TItem, usize) -> bool,
) -> Option<&TItem> {
    array
        .into_iter()
        .rev()
        .enumerate()
        .find(|(index, value)| predicate(value, *index))
        .map(|(_, value)| value)
}

pub fn try_find_last<TItem, TError>(
    array: &[TItem],
    mut predicate: impl FnMut(&TItem, usize) -> Result<bool, TError>,
) -> Result<Option<&TItem>, TError> {
    for (index, value) in array.into_iter().rev().enumerate() {
        if predicate(value, *index)? {
            return Ok(Some(value));
        }
    }
    Ok(None)
}

pub fn find_index<TItem>(
    array: &[TItem],
    mut predicate: impl FnMut(&TItem, usize) -> bool,
    start_index: Option<usize>,
) -> Option<usize> {
    array
        .into_iter()
        .enumerate()
        .skip(start_index.unwrap_or(0))
        .position(|(index, value)| predicate(value, index))
}

pub fn try_find_index<TItem, TError>(
    array: &[TItem],
    mut predicate: impl FnMut(&TItem, usize) -> Result<bool, TError>,
    start_index: Option<usize>,
) -> Result<Option<usize>, TError> {
    for (index, value) in array.into_iter().enumerate().skip(start_index.unwrap_or(0)) {
        if predicate(value, index)? {
            return Ok(Some(index));
        }
    }
    Ok(None)
}

pub fn find_last_index<TItem>(
    array: &[TItem],
    mut predicate: impl FnMut(&TItem, usize) -> bool,
    start_index: Option<usize>,
) -> Option<usize> {
    if array.is_empty() {
        return None;
    }
    let mut i = start_index.unwrap_or_else(|| array.len() - 1);
    loop {
        if predicate(&array[i], i) {
            return Some(i);
        }
        if i == 0 {
            break;
        }
        i -= 1;
    }
    None
}

pub fn find_last_index_returns_isize<TItem>(
    array: &[TItem],
    predicate: impl FnMut(&TItem, usize) -> bool,
    start_index: Option<usize>,
) -> isize {
    match find_last_index(array, predicate, start_index) {
        None => -1,
        Some(index) => index.try_into().unwrap(),
    }
}

pub fn contains<TItem: Eq>(array: Option<&[TItem]>, value: &TItem) -> bool {
    array.map_or(false, |array| array.iter().any(|item| item == value))
}

pub fn contains_rc<TItem>(array: Option<&[Rc<TItem>]>, value: &Rc<TItem>) -> bool {
    array.map_or(false, |array| {
        array.iter().any(|item| Rc::ptr_eq(item, value))
    })
}

pub fn contains_gc<TItem: Trace + Finalize>(
    array: Option<&[Gc<TItem>]>,
    value: &Gc<TItem>,
) -> bool {
    array.map_or(false, |array| {
        array.iter().any(|item| Gc::ptr_eq(item, value))
    })
}

pub fn contains_comparer<TItem>(
    array: Option<&[TItem]>,
    value: &TItem,
    equality_comparer: impl Fn(&TItem, &TItem) -> bool,
) -> bool {
    array.map_or(false, |array| {
        array.iter().any(|item| equality_comparer(item, value))
    })
}

pub fn arrays_equal<TItem: Eq>(a: &[TItem], b: &[TItem]) -> bool {
    // TODO: separate eg arrays_equal_by() helper taking equality_comparer callback and not imposing `Eq` bound?
    a.len() == b.len() && every(a, |item_a, i| *item_a == b[i])
}

pub fn index_of_any_char_code(
    text: &SourceTextAsChars,
    char_codes: &[char],
    start: Option<usize>,
) -> Option<usize> {
    let mut i = start.unwrap_or(0);
    while i < text.len() {
        if contains(Some(char_codes), &text_char_at_index(text, i)) {
            return Some(i);
        }
        i += 1;
    }
    None
}

pub fn count_where<TItem, TPredicate: FnMut(&TItem, usize) -> bool>(
    array: Option<&[TItem]>,
    mut predicate: TPredicate,
) -> usize {
    let mut count = 0;
    if let Some(array) = array {
        for (i, v) in array.iter().enumerate() {
            if predicate(v, i) {
                count += 1;
            }
        }
    }
    count
}

pub fn filter<TItem: Clone>(
    array: &[TItem],
    mut predicate: impl FnMut(&TItem) -> bool,
) -> Vec<TItem> {
    array
        .into_iter()
        .filter(|item| predicate(item))
        .map(Clone::clone)
        .collect()
}

pub fn try_filter<TItem: Clone, TError>(
    array: &[TItem],
    mut predicate: impl FnMut(&TItem) -> Result<bool, TError>,
) -> Result<Vec<TItem>, TError> {
    let mut results: Vec<TItem> = Default::default();
    for item in array {
        if predicate(item)? {
            results.push(item.clone());
        }
    }
    Ok(results)
}

pub fn filter_iter<TItem, TArray: IntoIterator<Item = TItem>, TCallback: FnMut(&TItem) -> bool>(
    array: TArray,
    predicate: TCallback,
) -> impl Iterator<Item = TItem> {
    array.into_iter().filter(predicate)
}

pub fn maybe_filter<TItem: Clone>(
    array: Option<&[TItem]>,
    predicate: impl FnMut(&TItem) -> bool,
) -> Option<Vec<TItem>> {
    array.map(|array| filter(array, predicate))
}

pub fn try_maybe_filter<TItem: Clone, TError>(
    array: Option<&[TItem]>,
    predicate: impl FnMut(&TItem) -> Result<bool, TError>,
) -> Option<Result<Vec<TItem>, TError>> {
    array.map(|array| try_filter(array, predicate))
}

pub fn filter_owning<TItem, TCallback: FnMut(&TItem) -> bool>(
    array: Vec<TItem>,
    mut predicate: TCallback,
) -> Vec<TItem> {
    array.into_iter().filter(|item| predicate(item)).collect()
}

pub fn filter_mutate<TItem: Clone, TCallback: FnMut(&TItem) -> bool>(
    array: &mut Vec<TItem>,
    mut predicate: TCallback,
) {
    *array = array
        .into_iter()
        .filter(|item| predicate(item))
        .map(|item| item.clone())
        .collect();
}

pub fn clear<TItem>(array: &mut Vec<TItem>) {
    array.clear();
}

pub fn map<TCollection: IntoIterator, TReturn>(
    array: TCollection,
    mut f: impl FnMut(TCollection::Item, usize) -> TReturn,
) -> Vec<TReturn> {
    let mut result = vec![];
    for (i, item) in array.into_iter().enumerate() {
        result.push(f(item, i));
    }
    result
}

pub fn try_map<TCollection: IntoIterator, TReturn, TError>(
    array: TCollection,
    mut f: impl FnMut(TCollection::Item, usize) -> Result<TReturn, TError>,
) -> Result<Vec<TReturn>, TError> {
    let mut result = vec![];
    for (i, item) in array.into_iter().enumerate() {
        result.push(f(item, i)?);
    }
    Ok(result)
}

pub fn maybe_map<TCollection: IntoIterator, TReturn>(
    array: Option<TCollection>,
    f: impl FnMut(TCollection::Item, usize) -> TReturn,
) -> Option<Vec<TReturn>> {
    array.map(|array| map(array, f))
}

pub fn try_maybe_map<TCollection: IntoIterator, TReturn, TError>(
    array: Option<TCollection>,
    f: impl FnMut(TCollection::Item, usize) -> Result<TReturn, TError>,
) -> Option<Result<Vec<TReturn>, TError>> {
    array.map(|array| try_map(array, f))
}

// TODO: this currently just mimics map(), I think could do the intended avoiding allocation by
// returning Cow?
pub fn same_map<TItem: Clone, TCallback: FnMut(&TItem, usize) -> TItem>(
    array: &[TItem],
    mut f: TCallback,
) -> Vec<TItem> {
    let mut result = vec![];
    for (i, item) in array.into_iter().enumerate() {
        result.push(f(item, i));
    }
    result
}

pub fn maybe_same_map<TItem: Clone, TCallback: FnMut(&TItem, usize) -> TItem>(
    array: Option<&[TItem]>,
    f: TCallback,
) -> Option<Vec<TItem>> {
    array.map(|array| same_map(array, f))
}

pub fn flatten<TItem: Clone>(array: &[Vec<TItem>]) -> Vec<TItem> {
    let mut result = vec![];
    for v in array {
        // if (v) {
        // if isArray(v) {
        add_range(&mut result, Some(v), None, None);
        // }
        // }
    }
    result
}

pub fn flat_map<TCollection: IntoIterator, TReturn: Clone>(
    array: Option<TCollection>,
    mut mapfn: impl FnMut(TCollection::Item, usize) -> Vec<TReturn>, /* | undefined */
) -> Vec<TReturn> {
    let mut result: Option<Vec<_>> = None;
    if let Some(array) = array {
        let mut some_result = vec![];
        for (i, item) in array.into_iter().enumerate() {
            let v = mapfn(item, i);
            /*some_result = */
            add_range(&mut some_result, Some(&v), None, None);
        }
        result = Some(some_result);
    }
    result.unwrap_or(vec![])
}

pub fn try_flat_map<TCollection: IntoIterator, TReturn: Clone, TError>(
    array: Option<TCollection>,
    mut mapfn: impl FnMut(TCollection::Item, usize) -> Result<Vec<TReturn>, TError>, /* | undefined */
) -> Result<Vec<TReturn>, TError> {
    let mut result: Option<Vec<_>> = None;
    if let Some(array) = array {
        let mut some_result = vec![];
        for (i, item) in array.into_iter().enumerate() {
            let v = mapfn(item, i)?;
            /*some_result = */
            add_range(&mut some_result, Some(&v), None, None);
        }
        result = Some(some_result);
    }
    Ok(result.unwrap_or_default())
}

pub fn flat_map_to_mutable<
    TCollection: IntoIterator,
    TReturn: Clone,
    TCallback: FnMut(TCollection::Item, usize) -> Vec<TReturn>, /* | undefined */
>(
    array: Option<TCollection>,
    mapfn: TCallback,
) -> Vec<TReturn> {
    flat_map(array, mapfn)
}

pub fn same_flat_map_rc_node(
    array: NodeArrayOrVec,
    mut mapfn: impl FnMut(&Gc<Node>, usize) -> SingleOrVec<Gc<Node>>,
) -> NodeArrayOrVec {
    let mut result: Option<Vec<Gc<Node>>> = Default::default();
    // if (array) {
    for (i, item) in array.iter().enumerate() {
        let mapped = mapfn(item, i);
        if result.is_some()
            || match &mapped {
                SingleOrVec::Single(mapped) => !Gc::ptr_eq(item, mapped),
                SingleOrVec::Vec(_) => true,
            }
        {
            let result = result.get_or_insert_with(|| array[..i].to_owned());
            match mapped {
                SingleOrVec::Vec(mapped) => {
                    add_range(result, Some(&mapped), None, None);
                }
                SingleOrVec::Single(mapped) => {
                    result.push(mapped);
                }
            }
        }
    }
    // }
    result.map_or(array, Into::into)
}

pub enum SingleOrVec<TItem> {
    Single(TItem),
    Vec(Vec<TItem>),
}

impl<TItem> From<TItem> for SingleOrVec<TItem> {
    fn from(value: TItem) -> Self {
        Self::Single(value)
    }
}

impl<TItem> From<Vec<TItem>> for SingleOrVec<TItem> {
    fn from(value: Vec<TItem>) -> Self {
        Self::Vec(value)
    }
}

pub fn map_defined<TCollection: IntoIterator, TReturn>(
    array: Option<TCollection>,
    mut map_fn: impl FnMut(TCollection::Item, usize) -> Option<TReturn>,
) -> Vec<TReturn> {
    let mut result = vec![];
    if let Some(array) = array {
        for (i, item) in array.into_iter().enumerate() {
            let mapped = map_fn(item, i);
            if let Some(mapped) = mapped {
                result.push(mapped);
            }
        }
    }
    result
}

pub fn try_map_defined<TCollection: IntoIterator, TReturn, TError>(
    array: Option<TCollection>,
    mut map_fn: impl FnMut(TCollection::Item, usize) -> Result<Option<TReturn>, TError>,
) -> Result<Vec<TReturn>, TError> {
    let mut result = vec![];
    if let Some(array) = array {
        for (i, item) in array.into_iter().enumerate() {
            let mapped = map_fn(item, i)?;
            if let Some(mapped) = mapped {
                result.push(mapped);
            }
        }
    }
    Ok(result)
}

pub fn get_or_update<TKey: Eq + Hash, TValue, TCallback: FnOnce() -> TValue>(
    map: &mut HashMap<TKey, TValue>,
    key: TKey,
    callback: TCallback,
) -> &mut TValue {
    map.entry(key).or_insert_with(callback)
}

// TODO: try and "reconcile" this with get_or_update() (for HashMap) above?
pub fn get_or_update_indexmap<TKey: Eq + Hash, TValue, TCallback: FnOnce() -> TValue>(
    map: &mut IndexMap<TKey, TValue>,
    key: TKey,
    callback: TCallback,
) -> &mut TValue {
    map.entry(key).or_insert_with(callback)
}

pub fn try_add_to_set<TItem: Eq + Hash>(set: &mut HashSet<TItem>, value: TItem) -> bool {
    if !set.contains(&value) {
        set.insert(value);
        return true;
    }
    false
}

pub fn some<'item, TItem, TArray>(
    array: Option<TArray>,
    predicate: Option<impl FnMut(&TItem) -> bool>,
) -> bool
where
    TItem: 'item,
    TArray: IntoIterator<Item = &'item TItem>,
    TArray::IntoIter: Clone,
{
    array.map_or(false, |array| {
        let mut array = array.into_iter();
        predicate.map_or_else(
            {
                let array_clone = array.clone();
                || !array_clone.peekable().is_empty_()
            },
            |predicate| array.any(predicate),
        )
    })
}

pub fn try_some<'item, TItem, TArray, TError>(
    array: Option<TArray>,
    predicate: Option<impl FnMut(&TItem) -> Result<bool, TError>>,
) -> Result<bool, TError>
where
    TItem: 'item,
    TArray: IntoIterator<Item = &'item TItem>,
    TArray::IntoIter: Clone,
{
    match array {
        None => Ok(false),
        Some(array) => {
            let mut array = array.into_iter();
            Ok(if let Some(predicate) = predicate {
                for item in array {
                    if predicate(item)? {
                        return Ok(true);
                    }
                }
                false
            } else {
                !array.peekable().is_empty_()
            })
        }
    }
}

pub fn get_ranges_where<TItem, TPred: FnMut(&TItem) -> bool, TCallback: FnMut(usize, usize)>(
    arr: &[TItem],
    mut pred: TPred,
    mut cb: TCallback,
) {
    let mut start: Option<usize> = None;
    for i in 0..arr.len() {
        if pred(&arr[i]) {
            start = Some(match start {
                None => i,
                Some(start) => start,
            });
        } else {
            if let Some(start_present) = start {
                cb(start_present, i);
                start = None;
            }
        }
    }
    if let Some(start) = start {
        cb(start, arr.len());
    }
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

pub fn maybe_concatenate<TItem>(
    array1: Option<Vec<TItem>>,
    array2: Option<Vec<TItem>>,
) -> Option<Vec<TItem>> {
    if !some(array2.as_deref(), Option::<fn(&TItem) -> bool>::None) {
        return array1;
    }
    if !some(array1.as_deref(), Option::<fn(&TItem) -> bool>::None) {
        return array2;
    }
    let mut array1 = array1.unwrap();
    let mut array2 = array2.unwrap();
    array1.append(&mut array2);
    Some(array1)
}

fn select_index<TItem>(_: TItem, i: usize) -> usize {
    i
}

pub fn indices_of<TItem>(array: &[TItem]) -> Vec<usize> {
    array
        .into_iter()
        .enumerate()
        .map(|(i, item)| select_index(item, i))
        .collect()
}

enum ComparerOrEqualityComparer<'closure, TItem> {
    Comparer(&'closure dyn Fn(&TItem, &TItem) -> Comparison),
    EqualityComparer(&'closure dyn Fn(&TItem, &TItem) -> bool),
}

fn deduplicate_equality_gc<TItem: Trace + Finalize>(array: &[Gc<TItem>]) -> Vec<Gc<TItem>> {
    let mut result = vec![];
    for item in array {
        push_if_unique_gc(&mut result, item);
    }
    result
}

fn deduplicate_equality_rc<TItem>(array: &[Rc<TItem>]) -> Vec<Rc<TItem>> {
    let mut result = vec![];
    for item in array {
        push_if_unique_rc(&mut result, item);
    }
    result
}

pub fn deduplicate_rc<TItem>(array: &[Rc<TItem>]) -> Vec<Rc<TItem>> {
    if array.is_empty() {
        vec![]
    } else if array.len() == 1 {
        vec![array[0].clone()]
    } else {
        deduplicate_equality_rc(array)
    }
}

pub fn deduplicate_gc<TItem: Trace + Finalize>(array: &[Gc<TItem>]) -> Vec<Gc<TItem>> {
    if array.is_empty() {
        vec![]
    } else if array.len() == 1 {
        vec![array[0].clone()]
    } else {
        deduplicate_equality_gc(array)
    }
}

fn deduplicate_sorted<TItem: Clone + Trace + Finalize>(
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

pub fn insert_sorted<
    TItem: Trace + Finalize, /*, TComparer: Comparer<&'array_or_item TItem>*/
>(
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
    TItem: Clone + Trace + Finalize,
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

pub fn array_is_equal_to<TItem>(
    array1: Option<&[TItem]>,
    array2: Option<&[TItem]>,
    mut equality_comparer: impl FnMut(&TItem, &TItem, usize) -> bool,
) -> bool {
    if array1.is_none() && array2.is_none() {
        return true;
    }
    if array1.is_none() || array2.is_none() {
        return false;
    }
    let array1 = array1.unwrap();
    let array2 = array2.unwrap();

    if array1.len() != array2.len() {
        return false;
    }

    for (i, (array1_i, array2_i)) in iter::zip(array1, array2).enumerate() {
        if !equality_comparer(array1_i, array2_i, i) {
            return false;
        }
    }

    true
}

pub fn relative_complement<TItem: Clone, TComparer: FnMut(&TItem, &TItem) -> Comparison>(
    array_a: &[TItem],
    array_b: &[TItem],
    mut comparer: TComparer,
) -> Vec<TItem> {
    if array_b.is_empty() || array_a.is_empty() {
        return array_b.to_owned();
    }
    let mut result: Vec<TItem> = vec![];
    let mut offset_a = 0;
    let mut offset_b = 0;
    'loop_b: while offset_b < array_b.len() {
        if offset_b > 0 {
            Debug_.assert(
                comparer(&array_b[offset_b], &array_b[offset_b - 1]) != Comparison::LessThan,
                None,
            );
        }

        let start_a = offset_a;
        'loop_a: while offset_a < array_a.len() {
            if offset_a > start_a {
                Debug_.assert(
                    comparer(&array_a[offset_a], &array_a[offset_a - 1]) != Comparison::LessThan,
                    None,
                );
            }

            match comparer(&array_b[offset_b], &array_a[offset_a]) {
                Comparison::LessThan => {
                    result.push(array_b[offset_b].clone());
                    offset_a += 1;
                    offset_b += 1;
                    continue 'loop_b;
                }
                Comparison::EqualTo => {
                    offset_a += 1;
                    offset_b += 1;
                    continue 'loop_b;
                }
                Comparison::GreaterThan => {
                    offset_a += 1;
                    continue 'loop_a;
                }
            }
        }
    }
    result
}

pub fn sum<TItem, TGetValue: FnMut(&TItem) -> usize>(
    array: &[TItem],
    mut get_value: TGetValue,
) -> usize {
    let mut result = 0;
    for v in array {
        result += get_value(v);
    }
    result
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

pub fn maybe_add_range<TItem: Clone>(
    mut to: Option<Vec<TItem>>,
    from: Option<&[TItem]>,
    start: Option<isize>,
    end: Option<isize>,
) -> Option<Vec<TItem>> {
    if from.is_none() {
        return to;
    }
    let from = from.unwrap();
    if from.is_empty() {
        return to;
    }
    if to.is_none() {
        to = Some(vec![]);
    }
    add_range(to.as_mut().unwrap(), Some(from), start, end);
    to
}

pub fn add_range<TItem: Clone>(
    to: &mut Vec<TItem>,
    from: Option<&[TItem]>,
    start: Option<isize>,
    end: Option<isize>,
) {
    if from.is_none() {
        return;
    }
    let from = from.unwrap();
    if from.is_empty() {
        return;
    }
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
}

pub fn push_if_unique_rc<TItem>(array: &mut Vec<Rc<TItem>>, to_add: &Rc<TItem>) -> bool {
    if contains_rc(Some(array), to_add) {
        false
    } else {
        array.push(to_add.clone());
        true
    }
}

pub fn push_if_unique_gc<TItem: Trace + Finalize>(
    array: &mut Vec<Gc<TItem>>,
    to_add: &Gc<TItem>,
) -> bool {
    if contains_gc(Some(array), to_add) {
        false
    } else {
        array.push(to_add.clone());
        true
    }
}

pub fn append_if_unique_rc<TItem>(array: &mut Vec<Rc<TItem>>, to_add: &Rc<TItem>) {
    push_if_unique_rc(array, to_add);
}

pub fn append_if_unique_gc<TItem: Trace + Finalize>(
    array: &mut Vec<Gc<TItem>>,
    to_add: &Gc<TItem>,
) {
    push_if_unique_gc(array, to_add);
}

pub fn maybe_append_if_unique_rc<TItem>(
    array: Option<Vec<Rc<TItem>>>,
    to_add: &Rc<TItem>,
) -> Vec<Rc<TItem>> {
    if let Some(mut array) = array {
        push_if_unique_rc(&mut array, to_add);
        array
    } else {
        vec![to_add.clone()]
    }
}

pub fn maybe_append_if_unique_gc<TItem: Trace + Finalize>(
    array: Option<Vec<Gc<TItem>>>,
    to_add: &Gc<TItem>,
) -> Vec<Gc<TItem>> {
    if let Some(mut array) = array {
        push_if_unique_gc(&mut array, to_add);
        array
    } else {
        vec![to_add.clone()]
    }
}

pub fn comparison_to_ordering(comparison: Comparison) -> Ordering {
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

pub fn stable_sort_indices<TItem, TComparer: Fn(&TItem, &TItem) -> Comparison>(
    array: &[TItem],
    indices: &mut Vec<usize>,
    comparer: TComparer,
) {
    indices.sort_by(|x, y| {
        let by_comparer = comparer(&array[*x], &array[*y]);
        comparison_to_ordering(if by_comparer == Comparison::EqualTo {
            compare_values(Some(*x), Some(*y))
        } else {
            by_comparer
        })
    });
}

pub fn sort<TItem: Clone + Trace + Finalize, TComparer: Fn(&TItem, &TItem) -> Comparison>(
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

pub fn range_equals_rc<TItem>(
    array1: &[Rc<TItem>],
    array2: &[Rc<TItem>],
    mut pos: usize,
    end: usize,
) -> bool {
    while pos < end {
        if !Rc::ptr_eq(&array1[pos], &array2[pos]) {
            return false;
        }
        pos += 1;
    }
    true
}

pub fn range_equals_gc<TItem: Trace + Finalize>(
    array1: &[Gc<TItem>],
    array2: &[Gc<TItem>],
    mut pos: usize,
    end: usize,
) -> bool {
    while pos < end {
        if !Gc::ptr_eq(&array1[pos], &array2[pos]) {
            return false;
        }
        pos += 1;
    }
    true
}

pub fn stable_sort<TItem: Clone + Trace + Finalize, TComparer: Fn(&TItem, &TItem) -> Comparison>(
    array: &[TItem],
    comparer: TComparer,
) -> SortedArray<TItem> {
    let mut indices = indices_of(array);
    stable_sort_indices(array, &mut indices, comparer);
    indices
        .into_iter()
        .map(|i| array[i].clone())
        .collect::<Vec<_>>()
        .into()
}

pub fn first<TItem>(array: &[TItem]) -> &TItem {
    Debug_.assert(!array.is_empty(), None);
    &array[0]
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

pub fn single_or_undefined<TItem>(array: Option<&[TItem]>) -> Option<&TItem> {
    match array {
        Some(array) if array.len() == 1 => Some(&array[0]),
        _ => None,
    }
}

pub fn replace_element<TItem: Clone>(array: &[TItem], index: usize, value: TItem) -> Vec<TItem> {
    let mut result = array.to_owned();
    result[index] = value;
    result
}

pub fn binary_search<
    TKey,
    TItem,
    TKeySelector: Fn(&TItem, Option<usize>) -> &TKey,
    TKeyComparer: Fn(&TKey, &TKey) -> Comparison,
    // TComparer: Comparer<TKey>,
>(
    array: &[TItem],
    value: &TItem,
    // key_selector: fn(&TItem, Option<usize>) -> &TKey,
    key_selector: TKeySelector,
    // key_comparer: TComparer,
    // key_comparer: Comparer<&'array TKey>,
    // key_comparer: Comparer<&TKey, &TKey>,
    key_comparer: TKeyComparer,
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
    TKeyComparer: Fn(&TKey, &TKey) -> Comparison,
    // TComparer: Comparer<TKey>,
>(
    array: &[TItem],
    item: &TItem,
    // key: &TSearchedKey,
    key_selector: TKeySelector,
    // key_comparer: TComparer,
    // key_comparer: Comparer<&TKey>,
    key_comparer: TKeyComparer,
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

    let mut low: isize = offset.unwrap_or(0).try_into().unwrap();
    let mut high: isize = (array.len() - 1).try_into().unwrap();
    while low <= high {
        let middle = low + ((high - low) >> 1);
        let middle_as_usize: usize = middle.try_into().unwrap();
        let mid_key = key_selector(&array[middle_as_usize], Some(middle_as_usize));
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

pub fn reduce_left<TItem, TMemo, TCallback: FnMut(TMemo, &TItem, usize) -> TMemo>(
    array: &[TItem],
    mut f: TCallback,
    initial: TMemo,
    start: Option<usize>,
    count: Option<usize>,
) -> TMemo {
    if
    /*array &&*/
    !array.is_empty() {
        let size = array.len();
        // if (size > 0) {
        let mut pos = if start.is_none() /*|| start < 0*/ {
            0
        } else {
            start.unwrap()
        };
        let end = if match count {
            None => true,
            Some(count) => pos + count > size - 1,
        } {
            size - 1
        } else {
            pos + count.unwrap()
        };
        let mut result: TMemo;
        // if (arguments.length <= 2) {
        //     result = array[pos];
        //     pos++;
        // } else {
        result = initial;
        while pos <= end {
            result = f(result, &array[pos], pos);
            pos += 1;
        }
        return result;
        // }
        // }
    }
    initial
}

pub fn reduce_left_no_initial_value<TItem: Clone>(
    array: &[TItem],
    mut f: impl FnMut(TItem, &TItem, usize) -> TItem,
    start: Option<usize>,
    count: Option<usize>,
) -> TItem {
    if
    /*array &&*/
    !array.is_empty() {
        let size = array.len();
        // if (size > 0) {
        let mut pos = if start.is_none() /*|| start < 0*/ {
            0
        } else {
            start.unwrap()
        };
        let end = if match count {
            None => true,
            Some(count) => pos + count > size - 1,
        } {
            size - 1
        } else {
            pos + count.unwrap()
        };
        let mut result = array[0].clone();
        pos += 1;
        while pos <= end {
            result = f(result, &array[pos], pos);
            pos += 1;
        }
        return result;
        // }
        // }
    }
    panic!("Shouldn't call reduce_left_no_initial_value() with empty slice")
}

pub fn reduce_left_no_initial_value_optional<TItem: Clone>(
    array: &[TItem],
    mut f: impl FnMut(Option<TItem>, &TItem, usize) -> Option<TItem>,
    start: Option<usize>,
    count: Option<usize>,
) -> Option<TItem> {
    try_reduce_left_no_initial_value_optional(
        array,
        |a: Option<TItem>, b: &TItem, c: usize| -> Result<_, ()> { Ok(f(a, b, c)) },
        start,
        count,
    )
    .unwrap()
}

pub fn try_reduce_left_no_initial_value_optional<TItem: Clone, TError>(
    array: &[TItem],
    mut f: impl FnMut(Option<TItem>, &TItem, usize) -> Result<Option<TItem>, TError>,
    start: Option<usize>,
    count: Option<usize>,
) -> Result<Option<TItem>, TError> {
    if
    /*array &&*/
    !array.is_empty() {
        let size = array.len();
        // if (size > 0) {
        let mut pos = if start.is_none() /*|| start < 0*/ {
            0
        } else {
            start.unwrap()
        };
        let end = if match count {
            None => true,
            Some(count) => pos + count > size - 1,
        } {
            size - 1
        } else {
            pos + count.unwrap()
        };
        let mut result = Some(array[0].clone());
        pos += 1;
        while pos <= end {
            result = f(result, &array[pos], pos)?;
            pos += 1;
        }
        return Ok(result);
        // }
        // }
    }
    Ok(None)
}

pub fn array_of<TItem, TCallback: FnMut(usize) -> TItem>(
    count: usize,
    mut f: TCallback,
) -> Vec<TItem> {
    let mut result = Vec::with_capacity(count);
    for i in 0..count {
        result[i] = f(i);
    }
    result
}

pub fn array_to_map<TItem, TKey: hash::Hash + Eq, TValue>(
    array: &[TItem],
    mut make_key: impl FnMut(&TItem) -> Option<TKey>,
    mut make_value: impl FnMut(&TItem) -> TValue,
) -> HashMap<TKey, TValue> {
    let mut result = HashMap::new();
    for value in array {
        let key = make_key(value);
        if let Some(key) = key {
            result.insert(key, make_value(value));
        }
    }
    result
}

pub fn array_to_multi_map<
    TItem,
    TKey: hash::Hash + Eq + Trace + Finalize,
    TValue: Clone + Trace + Finalize,
>(
    values: &[TItem],
    mut make_key: impl FnMut(&TItem) -> TKey,
    mut make_value: impl FnMut(&TItem) -> TValue,
) -> MultiMap<TKey, TValue> {
    let mut result: MultiMap<TKey, TValue> = create_multi_map();
    for value in values {
        result.add(make_key(value), make_value(value));
    }
    result
}

pub fn group<TItem: Clone + Trace + Finalize, TKey: hash::Hash + Eq + Trace + Finalize, TResult>(
    values: &[TItem],
    get_group_id: impl FnMut(&TItem) -> TKey,
    mut result_selector: impl FnMut(Vec<TItem>) -> TResult,
) -> Vec<TResult> {
    array_to_multi_map(values, get_group_id, |item: &TItem| item.clone())
        .into_values()
        .map(|values: Vec<TItem>| result_selector(values))
        .collect()
}

// TODO: did I actually use this anywhere? Eg I ended up using Clone for Node
pub trait Cloneable {
    fn cloned(&self) -> Self;
}

pub fn clone<TValue: Cloneable>(object: &TValue) -> TValue {
    object.cloned()
}

mod _MultiMapDeriveTraceScope {
    use std::collections::hash_map::{IntoValues, Values};

    use super::*;
    use local_macros::Trace;

    #[derive(Debug, Trace, Finalize)]
    pub struct MultiMap<TKey: Trace + Finalize, TValue: Trace + Finalize>(
        // TODO: make the nested hash map private and implement iteration on the wrapper
        pub HashMap<TKey, Vec<TValue>>,
    );

    impl<TKey: Hash + Eq + Trace + Finalize, TValue: Clone + Trace + Finalize> MultiMap<TKey, TValue> {
        pub fn add(&mut self, key: TKey, value: TValue) {
            let values = self.0.entry(key).or_insert(vec![]);
            values.push(value);
        }

        pub fn remove<TComparer: Fn(&TValue, &TValue) -> bool>(
            &mut self,
            key: TKey,
            value: &TValue,
            comparer: TComparer,
        ) {
            {
                let values = self.0.entry(key);
                match values {
                    Entry::Occupied(mut values) => {
                        unordered_remove_item(values.get_mut(), value, comparer);
                        if values.get().is_empty() {
                            values.remove_entry();
                        }
                    }
                    _ => (),
                }
            }
        }

        pub fn get(&self, key: &TKey) -> Option<&Vec<TValue>> {
            self.0.get(key)
        }

        pub fn contains_key(&self, key: &TKey) -> bool {
            self.0.contains_key(key)
        }

        pub fn values(&self) -> Values<TKey, Vec<TValue>> {
            self.0.values()
        }

        pub fn into_values(self) -> IntoValues<TKey, Vec<TValue>> {
            self.0.into_values()
        }
    }

    impl<TKey: Hash + Eq + Trace + Finalize, TValue: Clone + Trace + Finalize> IntoIterator
        for MultiMap<TKey, TValue>
    {
        type Item = (TKey, Vec<TValue>);
        type IntoIter = <HashMap<TKey, Vec<TValue>> as IntoIterator>::IntoIter;

        fn into_iter(self) -> Self::IntoIter {
            self.0.into_iter()
        }
    }
}
pub use _MultiMapDeriveTraceScope::MultiMap;

pub fn create_multi_map<TKey: Trace + Finalize, TValue: Trace + Finalize>() -> MultiMap<TKey, TValue>
{
    MultiMap(Default::default())
}

pub type UnderscoreEscapedMultiMap<TValue> = MultiMap<__String, TValue>;

pub fn create_underscore_escaped_multi_map<TValue: Trace + Finalize>(
) -> UnderscoreEscapedMultiMap<TValue> {
    create_multi_map()
}

mod _MultiMapOrderedDeriveTraceScope {
    use indexmap::map::{Entry, IntoValues, Values};

    use super::*;
    use local_macros::Trace;

    #[derive(Debug, Trace, Finalize)]
    pub struct MultiMapOrdered<TKey: Trace + Finalize, TValue: Trace + Finalize>(
        // TODO: make the nested hash map private and implement iteration on the wrapper
        pub IndexMap<TKey, Vec<TValue>>,
    );

    impl<TKey: Hash + Eq + Trace + Finalize, TValue: Clone + Trace + Finalize>
        MultiMapOrdered<TKey, TValue>
    {
        pub fn add(&mut self, key: TKey, value: TValue) {
            let values = self.0.entry(key).or_insert(vec![]);
            values.push(value);
        }

        pub fn remove<TComparer: Fn(&TValue, &TValue) -> bool>(
            &mut self,
            key: TKey,
            value: &TValue,
            comparer: TComparer,
        ) {
            {
                let values = self.0.entry(key);
                match values {
                    Entry::Occupied(mut values) => {
                        unordered_remove_item(values.get_mut(), value, comparer);
                        if values.get().is_empty() {
                            values.remove_entry();
                        }
                    }
                    _ => (),
                }
            }
        }

        pub fn get(&self, key: &TKey) -> Option<&Vec<TValue>> {
            self.0.get(key)
        }

        pub fn contains_key(&self, key: &TKey) -> bool {
            self.0.contains_key(key)
        }

        pub fn values(&self) -> Values<TKey, Vec<TValue>> {
            self.0.values()
        }

        pub fn into_values(self) -> IntoValues<TKey, Vec<TValue>> {
            self.0.into_values()
        }
    }

    impl<TKey: Hash + Eq + Trace + Finalize, TValue: Clone + Trace + Finalize> IntoIterator
        for MultiMapOrdered<TKey, TValue>
    {
        type Item = (TKey, Vec<TValue>);
        type IntoIter = <IndexMap<TKey, Vec<TValue>> as IntoIterator>::IntoIter;

        fn into_iter(self) -> Self::IntoIter {
            self.0.into_iter()
        }
    }
}
pub use _MultiMapOrderedDeriveTraceScope::MultiMapOrdered;

pub fn create_multi_map_ordered<TKey: Trace + Finalize, TValue: Trace + Finalize>(
) -> MultiMapOrdered<TKey, TValue> {
    MultiMapOrdered(Default::default())
}

pub fn try_cast<TIn, TTest: FnOnce(&TIn) -> bool>(value: TIn, test: TTest) -> Option<TIn> {
    if
    /*value !== undefined &&*/
    test(&value) {
        Some(value)
    } else {
        None
    }
}

pub fn cast<TIn>(value: Option<TIn>, test: impl FnOnce(&TIn) -> bool) -> TIn {
    if let Some(value) = value {
        if test(&value) {
            return value;
        }
    }

    Debug_.fail(Some("Invalid cast. The supplied value {:?} did not pass the test." /*'${Debug.getFunctionName(test)'*/));
}

pub fn cast_present<TIn, TTest: FnOnce(&TIn) -> bool>(value: TIn, test: TTest) -> TIn {
    if test(&value) {
        return value;
    }

    Debug_.fail(Some("Invalid cast. The supplied value {:?} did not pass the test." /*'${Debug.getFunctionName(test)'*/));
}

pub fn identity<TValue>(x: TValue) -> TValue {
    x
}

pub fn to_lower_case(x: &str) -> String {
    x.to_lowercase()
}

lazy_static! {
    static ref file_name_lower_case_reg_exp: Regex = Regex::new(r#"[^\u0130\u0131\u00DFa-z0-9\\/:\-_\. ]+"#/*/g*/).unwrap();
}

pub fn to_file_name_lower_case(x: &str) -> String {
    if file_name_lower_case_reg_exp.is_match(x) {
        file_name_lower_case_reg_exp
            .replace_all(x, |captures: &Captures| to_lower_case(&captures[0]))
            .into_owned()
    } else {
        x.to_owned()
    }
}

pub fn not_implemented() -> ! {
    unimplemented!()
}

// pub fn memoize<TReturn: Clone, TCallback: Fn() -> TReturn>(callback: TCallback) -> impl Fn() -> TReturn {

// }

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum AssertionLevel {
    None = 0,
    Normal = 1,
    Aggressive = 2,
    VeryAggressive = 3,
}

pub fn equate_values<TValue: PartialEq + ?Sized>(a: &TValue, b: &TValue) -> bool {
    a == b
}

pub fn equate_strings_case_insensitive(a: &str, b: &str) -> bool {
    a == b || a.to_uppercase() == b.to_uppercase()
}

pub fn equate_strings_case_sensitive(a: &str, b: &str) -> bool {
    equate_values(a, b)
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
    if a.is_none() && b.is_none() {
        return Comparison::EqualTo;
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

pub fn compare_strings_case_insensitive(a: &str, b: &str) -> Comparison {
    if a == b {
        return Comparison::EqualTo;
    }
    // if (a === undefined) return Comparison.LessThan;
    // if (b === undefined) return Comparison.GreaterThan;
    let a = a.to_uppercase();
    let b = b.to_uppercase();
    if a < b {
        Comparison::LessThan
    } else if a > b {
        Comparison::GreaterThan
    } else {
        Comparison::EqualTo
    }
}

pub fn compare_strings_case_sensitive_maybe(a: Option<&str>, b: Option<&str>) -> Comparison {
    compare_comparable_values(a, b)
}

pub fn compare_strings_case_sensitive(a: &str, b: &str) -> Comparison {
    compare_strings_case_sensitive_maybe(Some(a), Some(b))
}

pub fn get_string_comparer(ignore_case: Option<bool>) -> fn(&str, &str) -> Comparison {
    let ignore_case = ignore_case.unwrap_or(false);
    if ignore_case {
        compare_strings_case_insensitive
    } else {
        compare_strings_case_sensitive
    }
}

thread_local! {
    static ui_comparer_case_sensitive: RefCell<Option<fn(&str, &str) -> Comparison/*Comparer<string>*/>> = RefCell::new(None);
}

thread_local! {
    static ui_locale: RefCell<Option<String>> = RefCell::new(None);
}

pub fn get_ui_locale() -> Option<String> {
    ui_locale.with(|ui_locale_| ui_locale_.borrow().clone())
}

pub fn set_ui_locale(value: Option<String>) {
    ui_locale.with(|ui_locale_| {
        let mut ui_locale_ = ui_locale_.borrow_mut();
        if *ui_locale_ != value {
            *ui_locale_ = value;
            ui_comparer_case_sensitive.with(|ui_comparer_case_sensitive_| {
                *ui_comparer_case_sensitive_.borrow_mut() = None;
            })
        }
    })
}

pub fn compare_booleans(a: bool, b: bool) -> Comparison {
    compare_values(Some(if a { 1 } else { 0 }), Some(if b { 1 } else { 0 }))
}

// TODO: it looked like all the `candidates` types are cheaply cloneable
// so returning a cloned item was easier than messing more with how to try
// and still return a reference (Option<&'candidates TCandidate>) when
// `candidates` now needed to iterate over `impl Borrow<TCandidate>` (vs
// only supporting iterating over references)
pub fn get_spelling_suggestion<'candidates, TCandidate: Clone>(
    name: &str,
    candidates: impl IntoIterator<Item = impl Borrow<TCandidate>>,
    mut get_name: impl FnMut(&TCandidate) -> Option<String>,
) -> Option<TCandidate> {
    try_get_spelling_suggestion(
        name,
        candidates,
        |candidate: &TCandidate| -> Result<_, ()> { Ok(get_name(candidate)) },
    )
    .unwrap()
}

pub fn try_get_spelling_suggestion<'candidates, TCandidate: Clone, TError>(
    name: &str,
    candidates: impl IntoIterator<Item = impl Borrow<TCandidate>>,
    mut get_name: impl FnMut(&TCandidate) -> Result<Option<String>, TError>,
) -> Result<Option<TCandidate>, TError> {
    let name_len_as_f64 = name.len() as f64;
    let maximum_length_difference = f64::min(2.0, (name_len_as_f64 * 0.34).floor());
    let mut best_distance = (name_len_as_f64 * 0.4).floor() + 1.0;
    let mut best_candidate: Option<TCandidate> = None;
    for candidate in candidates {
        let candidate: &TCandidate = candidate.borrow();
        let candidate_name = get_name(candidate)?;
        if let Some(candidate_name) = candidate_name {
            if (TryInto::<isize>::try_into(candidate_name.len()).unwrap()
                - TryInto::<isize>::try_into(name.len()).unwrap())
            .abs() as f64
                <= maximum_length_difference
            {
                if candidate_name == name {
                    continue;
                }
                if candidate_name.len() < 3 && candidate_name.to_lowercase() != name.to_lowercase()
                {
                    continue;
                }

                let distance = levenshtein_with_max(
                    &name.chars().collect(),
                    &candidate_name.chars().collect(),
                    best_distance - 0.1,
                );
                if distance.is_none() {
                    continue;
                }
                let distance = distance.unwrap();

                Debug_.assert(distance < best_distance, None);
                best_distance = distance;
                best_candidate = Some(candidate.clone());
            }
        }
    }
    Ok(best_candidate)
}

fn levenshtein_with_max(s1: &SourceTextAsChars, s2: &SourceTextAsChars, max: f64) -> Option<f64> {
    let mut previous: Vec<f64> = vec![0.0; s2.len() + 1];
    let mut current: Vec<f64> = vec![0.0; s2.len() + 1];
    let big = max + 0.01;

    for i in 0..=s2.len() {
        previous[i] = i as f64;
    }

    let s2_len_as_f64 = s2.len() as f64;
    for i in 1..=s1.len() {
        let i_as_f64 = i as f64;
        let c1 = text_char_at_index(s1, i - 1);
        let min_j = (if i_as_f64 > max { i_as_f64 - max } else { 1.0 }).ceil() as usize;
        let max_j = (if s2_len_as_f64 > max + i_as_f64 {
            max + i_as_f64
        } else {
            s2_len_as_f64
        })
        .floor() as usize;
        current[0] = i_as_f64;
        let mut col_min = i_as_f64;
        for j in 1..min_j {
            current[j] = big;
        }
        for j in min_j..=max_j {
            let substitution_distance = if s1[i - 1].to_lowercase().eq(s2[j - 1].to_lowercase()) {
                previous[j - 1] + 0.1
            } else {
                previous[j - 1] + 2.0
            };
            let dist = if c1 == text_char_at_index(s2, j - 1) {
                previous[j - 1]
            } else {
                (previous[j] + 1.0)
                    .min(current[j - 1] + 1.0)
                    .min(substitution_distance)
            };
            current[j] = dist;
            col_min = col_min.min(dist);
        }
        for j in max_j + 1..=s2.len() {
            current[j] = big;
        }
        if col_min > max {
            return None;
        }

        mem::swap(&mut previous, &mut current);
    }

    let res = previous[s2.len()];
    if res > max {
        None
    } else {
        Some(res)
    }
}

pub fn ends_with(str_: &str, suffix: &str) -> bool {
    str_.ends_with(suffix)
}

pub fn remove_suffix<'str>(str_: &'str str, suffix: &str) -> &'str str {
    if ends_with(str_, suffix) {
        &str_[0..str_.len() - suffix.len()]
    } else {
        str_
    }
}

pub fn string_contains(str_: &str, substring: &str) -> bool {
    str_.find(substring).is_some()
}

pub fn ordered_remove_item_at<TItem: Clone>(array: &mut Vec<TItem>, index: usize) {
    array.remove(index);
}

pub fn unordered_remove_item_at<TItem: Clone>(array: &mut Vec<TItem>, index: usize) {
    array[index] = array[array.len() - 1].clone();
    array.pop();
}

pub fn unordered_remove_item<TItem: Clone, TComparer: Fn(&TItem, &TItem) -> bool>(
    array: &mut Vec<TItem>,
    item: &TItem,
    comparer: TComparer,
) -> bool {
    unordered_remove_first_item_where(array, |element| comparer(element, item))
}

pub fn unordered_remove_first_item_where<TItem: Clone, TPredicate: Fn(&TItem) -> bool>(
    array: &mut Vec<TItem>,
    predicate: TPredicate,
) -> bool {
    for i in 0..array.len() {
        if predicate(&array[i]) {
            unordered_remove_item_at(array, i);
            return true;
        }
    }
    false
}

pub fn create_get_canonical_file_name(use_case_sensitive_file_names: bool) -> fn(&str) -> String {
    if use_case_sensitive_file_names {
        identity_str_to_owned
    } else {
        to_file_name_lower_case
    }
}

pub fn identity_str_to_cow(str_: &str) -> Cow<'_, str> {
    str_.into()
}

pub fn identity_str_to_owned(str_: &str) -> String {
    str_.to_owned()
}

#[derive(Clone, Debug, Trace, Finalize)]
pub struct Pattern {
    pub prefix: String,
    pub suffix: String,
}

impl Pattern {
    pub fn new(prefix: String, suffix: String) -> Self {
        Self { prefix, suffix }
    }
}

pub fn pattern_text(pattern: &Pattern) -> String {
    format!("{}*{}", pattern.prefix, pattern.suffix)
}

pub fn matched_text(pattern: &Pattern, candidate: &str) -> String {
    Debug_.assert(is_pattern_match(pattern, candidate), None);
    candidate[pattern.prefix.len()..candidate.len() - pattern.suffix.len()].to_owned()
}

pub fn find_best_pattern_match<'array, TItem, TGetPattern: Fn(&TItem) -> &Pattern>(
    values: &'array [TItem],
    get_pattern: TGetPattern,
    candidate: &str,
) -> Option<&'array TItem> {
    let mut matched_value: Option<&TItem> = None;
    let mut longest_match_prefix_length: isize = -1;

    for v in values {
        let pattern = get_pattern(v);
        let pattern_prefix_len_as_isize: isize = pattern.prefix.len().try_into().unwrap();
        if is_pattern_match(pattern, candidate)
            && pattern_prefix_len_as_isize > longest_match_prefix_length
        {
            longest_match_prefix_length = pattern_prefix_len_as_isize;
            matched_value = Some(v);
        }
    }

    matched_value
}

pub fn starts_with(str_: &str, prefix: &str) -> bool {
    str_.starts_with(prefix)
}

pub fn remove_prefix<'str>(str_: &'str str, prefix: &str) -> &'str str {
    if starts_with(str_, prefix) {
        &str_[prefix.len()..]
    } else {
        str_
    }
}

fn is_pattern_match(pattern: &Pattern, candidate: &str) -> bool {
    let prefix = &pattern.prefix;
    let suffix = &pattern.suffix;
    candidate.len() >= prefix.len() + suffix.len()
        && starts_with(candidate, prefix)
        && ends_with(candidate, suffix)
}

// pub fn and<TValue, TFirstCallback: FnMut(TValue) -> bool, TSecondCallback: FnMut(TValue) -> bool>()

pub fn single_element_array<TItem>(t: Option<TItem>) -> Option<Vec<TItem>> {
    t.map(|t| vec![t])
}

pub fn fill<TItem, TCallback: FnMut(usize) -> TItem>(
    length: usize,
    mut cb: TCallback,
) -> Vec<TItem> {
    let mut result = Vec::with_capacity(length);
    for i in 0..length {
        result[i] = cb(i);
    }
    result
}

pub fn cartesian_product<TItem: Clone>(arrays: &[Vec<TItem>]) -> Vec<Vec<TItem>> {
    let mut result = vec![];
    cartesian_product_worker(arrays, &mut result, None, 0);
    result
}

fn cartesian_product_worker<TItem: Clone>(
    arrays: &[Vec<TItem>],
    result: &mut Vec<Vec<TItem>>,
    outer: Option<&[TItem]>,
    index: usize,
) {
    for element in &arrays[index] {
        let mut inner: Vec<TItem>;
        match outer {
            Some(outer) => {
                inner = outer.iter().map(Clone::clone).collect();
                inner.push(element.clone());
            }
            None => {
                inner = vec![element.clone()];
            }
        }
        if index == arrays.len() - 1 {
            result.push(inner);
        } else {
            cartesian_product_worker(arrays, result, Some(&inner), index + 1);
        }
    }
}

pub fn pad_left<'str>(
    s: &'str str,
    length: usize,
    pad_string: Option<&str /*" " | "0"*/>,
) -> Cow<'str, str> {
    let pad_string = pad_string.unwrap_or(" ");
    if length <= s.len() {
        Cow::Borrowed(s)
    } else {
        Cow::Owned(format!("{}{}", pad_string.repeat(length - s.len()), s))
    }
}

pub fn pad_right(s: &str, length: usize /*, padString: " " = " "*/) -> Cow<str> {
    let pad_string = " ";
    if length <= s.len() {
        Cow::Borrowed(s)
    } else {
        Cow::Owned(format!("{}{}", s, pad_string.repeat(length - s.len())))
    }
}

pub fn take_while<TItem, TPredicate: FnMut(&TItem) -> bool>(
    array: &[TItem],
    mut predicate: TPredicate,
) -> &[TItem] {
    let len = array.len();
    let mut index = 0;
    while index < len && predicate(&array[index]) {
        index += 1;
    }
    &array[0..index]
}

pub fn trim_string(s: &str) -> &str {
    s.trim()
}

pub fn trim_string_end(s: &str) -> &str {
    s.trim_end()
}

pub fn trim_string_start(s: &str) -> &str {
    s.trim_start()
}
