use regex::{Captures, Regex};
use std::borrow::Cow;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::hash;
use std::hash::Hash;
use std::mem;
use std::ops::Add;
use std::ptr;
use std::rc::Rc;

use crate::{text_char_at_index, Comparison, Debug_, SortedArray, SourceTextAsChars};

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

pub fn for_each_bool<
    TCollection: IntoIterator,
    TCallback: FnMut(TCollection::Item, usize) -> bool,
>(
    array: TCollection,
    mut callback: TCallback,
) -> bool {
    array
        .into_iter()
        .enumerate()
        .any(|(index, item)| callback(item, index))
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

pub fn maybe_for_each_bool<
    TCollection: IntoIterator,
    TCallback: FnMut(TCollection::Item, usize) -> bool,
>(
    array: Option<TCollection>,
    callback: TCallback,
) -> bool {
    match array {
        Some(array) => for_each_bool(array, callback),
        None => false,
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

pub fn find_last<TItem, TCallback: FnMut(&TItem, usize) -> bool>(
    array: &[TItem],
    mut predicate: TCallback,
) -> Option<&TItem> {
    array
        .into_iter()
        .rev()
        .enumerate()
        .find(|(index, value)| predicate(value, *index))
        .map(|(_, value)| value)
}

pub fn find_index<TItem, TCallback: FnMut(&TItem, usize) -> bool>(
    array: &[TItem],
    mut predicate: TCallback,
    start_index: Option<usize>,
) -> Option<usize> {
    array
        .into_iter()
        .enumerate()
        .skip(start_index.unwrap_or(0))
        .position(|(index, value)| predicate(value, index))
}

pub fn contains<TItem: Eq>(array: Option<&[TItem]>, value: &TItem) -> bool {
    array.map_or(false, |array| array.iter().any(|item| item == value))
}

pub fn contains_rc<TItem>(array: Option<&[Rc<TItem>]>, value: &Rc<TItem>) -> bool {
    array.map_or(false, |array| {
        array.iter().any(|item| Rc::ptr_eq(item, value))
    })
}

pub fn contains_comparer<TItem, TEqualityComparer: Fn(&TItem, &TItem) -> bool>(
    array: Option<&[TItem]>,
    value: &TItem,
    equality_comparer: TEqualityComparer,
) -> bool {
    array.map_or(false, |array| {
        array.iter().any(|item| equality_comparer(item, value))
    })
}

pub fn arrays_equal<TItem: Eq>(a: &[TItem], b: &[TItem]) -> bool {
    // TODO: separate eg arrays_equal_by() helper taking equality_comparer callback and not imposing `Eq` bound?
    a.len() == b.len() && every(a, |item_a, i| *item_a == b[i])
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

// TODO: this currently just mimics map(), I think could do the intended avoiding allocation by
// returning Cow?
pub fn same_map<TItem: Clone, TCallback: FnMut(&TItem, usize) -> TItem>(
    array: Option<&[TItem]>,
    mut f: TCallback,
) -> Option<Vec<TItem>> {
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

pub fn flat_map<
    TCollection: IntoIterator,
    TReturn: Clone,
    TCallback: FnMut(TCollection::Item, usize) -> Vec<TReturn>, /* | undefined */
>(
    array: Option<TCollection>,
    mut mapfn: TCallback,
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

pub fn flat_map_to_mutable<
    TCollection: IntoIterator,
    TReturn: Clone,
    TCallback: FnMut(TCollection::Item, usize) -> Vec<TReturn>, /* | undefined */
>(
    array: Option<TCollection>,
    mut mapfn: TCallback,
) -> Vec<TReturn> {
    flat_map(array, mapfn)
}

pub fn map_defined<
    TCollection: IntoIterator,
    TReturn,
    TCallback: FnMut(TCollection::Item, usize) -> Option<TReturn>,
>(
    array: Option<TCollection>,
    mut map_fn: TCallback,
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

pub fn get_or_update<TKey: Eq + Hash, TValue, TCallback: FnOnce() -> TValue>(
    map: &mut HashMap<TKey, TValue>,
    key: TKey,
    callback: TCallback,
) -> &mut TValue {
    map.entry(key).or_insert_with(callback)
}

pub fn some<TItem, TPredicate: FnMut(&TItem) -> bool>(
    array: Option<&[TItem]>,
    predicate: Option<TPredicate>,
) -> bool {
    array.map_or(false, |array| {
        predicate.map_or(!array.is_empty(), |predicate| array.iter().any(predicate))
    })
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

enum ComparerOrEqualityComparer<'closure, TItem> {
    Comparer(&'closure dyn Fn(&TItem, &TItem) -> Comparison),
    EqualityComparer(&'closure dyn Fn(&TItem, &TItem) -> bool),
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

pub fn push_if_unique_rc<TItem>(array: &mut Vec<Rc<TItem>>, to_add: &Rc<TItem>) -> bool {
    if contains_rc(Some(array), to_add) {
        false
    } else {
        array.push(to_add.clone());
        true
    }
}

pub fn append_if_unique_rc<TItem>(array: &mut Vec<Rc<TItem>>, to_add: &Rc<TItem>) {
    push_if_unique_rc(array, to_add);
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

pub fn sort<TItem: Clone, TComparer: Fn(&TItem, &TItem) -> Comparison>(
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

pub fn array_to_map<
    TItem,
    TKey: hash::Hash + Eq,
    TValue,
    TMakeKey: FnMut(&TItem) -> Option<TKey>,
    TMakeValue: FnMut(&TItem) -> TValue,
>(
    array: &[TItem],
    mut make_key: TMakeKey,
    mut make_value: TMakeValue,
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

// TODO: make the nested hash map private and implement iteration on the wrapper
pub struct MultiMap<TKey, TValue>(pub HashMap<TKey, Vec<TValue>>);

impl<TKey: Hash + Eq, TValue: Clone> MultiMap<TKey, TValue> {
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
            let mut values = self.0.entry(key);
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
}

pub fn create_multi_map<TKey, TValue>() -> MultiMap<TKey, TValue> {
    MultiMap(HashMap::new())
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

pub fn cast<TIn, TTest: FnOnce(&TIn) -> bool>(value: Option<TIn>, test: TTest) -> TIn {
    if let Some(value) = value {
        if test(&value) {
            return value;
        }
    }

    Debug_.fail(Some("Invalid cast. The supplied value {:?} did not pass the test." /*'${Debug.getFunctionName(test)'*/));
}

fn identity<TValue>(x: TValue) -> TValue {
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

pub fn get_spelling_suggestion<
    'candidates,
    TCandidate,
    TGetName: FnMut(&TCandidate) -> Option<String>,
>(
    name: &str,
    candidates: &'candidates [TCandidate],
    mut get_name: TGetName,
) -> Option<&'candidates TCandidate> {
    let name_len_as_f64 = name.len() as f64;
    let maximum_length_difference = f64::min(2.0, (name_len_as_f64 * 0.34).floor());
    let mut best_distance = (name_len_as_f64 * 0.4).floor() + 1.0;
    let mut best_candidate = None;
    for candidate in candidates {
        let candidate_name = get_name(candidate);
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
                best_candidate = Some(candidate);
            }
        }
    }
    best_candidate
}

fn levenshtein_with_max(s1: &SourceTextAsChars, s2: &SourceTextAsChars, max: f64) -> Option<f64> {
    let mut previous: Vec<f64> = Vec::with_capacity(s2.len() + 1);
    let mut current: Vec<f64> = Vec::with_capacity(s2.len() + 1);
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

pub fn string_contains(str_: &str, substring: &str) -> bool {
    str_.find(substring).is_some()
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

pub type GetCanonicalFileName = fn(&str) -> String;
pub fn create_get_canonical_file_name(use_case_sensitive_file_names: bool) -> GetCanonicalFileName {
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

#[derive(Clone, Debug)]
pub struct Pattern {
    pub prefix: String,
    pub suffix: String,
}

impl Pattern {
    pub fn new(prefix: String, suffix: String) -> Self {
        Self { prefix, suffix }
    }
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
