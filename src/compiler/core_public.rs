use std::collections::{hash_map, hash_set, HashMap, HashSet};
use std::hash::Hash;
use std::ops::Deref;

pub type MapLike<TValue> = HashMap<String, TValue>;

#[derive(Debug)]
pub struct SortedArray<TItem> {
    _vec: Vec<TItem>,
}

impl<TItem> SortedArray<TItem> {
    pub fn new(vec: Vec<TItem>) -> Self {
        Self { _vec: vec }
    }

    pub fn push(&mut self, item: TItem) {
        self._vec.push(item);
    }

    pub fn is_empty(&self) -> bool {
        self._vec.is_empty()
    }

    pub fn insert(&mut self, index: usize, element: TItem) {
        self._vec.insert(index, element)
    }
}

impl<TItem: Clone> From<SortedArray<TItem>> for Vec<TItem> {
    fn from(sorted_array: SortedArray<TItem>) -> Self {
        sorted_array._vec
    }
}

impl<TItem: Clone> From<&SortedArray<TItem>> for Vec<TItem> {
    fn from(sorted_array: &SortedArray<TItem>) -> Self {
        sorted_array._vec.clone()
    }
}

impl<TItem> Deref for SortedArray<TItem> {
    type Target = [TItem];

    fn deref(&self) -> &Self::Target {
        &self._vec
    }
}

pub trait ReadonlyCollection<TKeyRef> {
    type Iter: Iterator<Item = TKeyRef>;
    fn size(&self) -> usize;
    fn has(&self, key: TKeyRef) -> bool;
    fn keys(&self) -> Self::Iter;
}

impl<'hash_map, TKey: Eq + Hash, TValue> ReadonlyCollection<&'hash_map TKey>
    for &'hash_map HashMap<TKey, TValue>
{
    type Iter = hash_map::Keys<'hash_map, TKey, TValue>;

    fn size(&self) -> usize {
        self.len()
    }

    fn has(&self, key: &TKey) -> bool {
        self.contains_key(key)
    }

    fn keys(&self) -> Self::Iter {
        HashMap::keys(self)
    }
}

impl<'hash_set, TKey: Eq + Hash> ReadonlyCollection<&'hash_set TKey> for &'hash_set HashSet<TKey> {
    type Iter = hash_set::Iter<'hash_set, TKey>;

    fn size(&self) -> usize {
        self.len()
    }

    fn has(&self, key: &TKey) -> bool {
        self.contains(key)
    }

    fn keys(&self) -> Self::Iter {
        self.iter()
    }
}

// pub enum ReadonlyCollection<'collection, TKey: Eq + Hash, TValue> {
//     HashMap(&'collection HashMap<TKey, TValue>),
//     HashSet(&'collection HashSet<TKey>),
// }

// impl<'collection, TKey: Eq + Hash, TValue> ReadonlyCollection<'collection, TKey, TValue> {
//     pub fn size(&self) -> usize {
//         match self {
//             ReadonlyCollection::HashMap(hash_map) => hash_map.len(),
//             ReadonlyCollection::HashSet(hash_set) => hash_set.len(),
//         }
//     }

//     pub fn has(&self, key: &TKey) -> bool {
//         match self {
//             ReadonlyCollection::HashMap(hash_map) => hash_map.contains_key(key),
//             ReadonlyCollection::HashSet(hash_set) => hash_set.contains(key),
//         }
//     }
// }

// impl<'collection, TKey: Eq + Hash, TValue> From<&'collection HashMap<TKey, TValue>>
//     for ReadonlyCollection<'collection, TKey, TValue>
// {
//     fn from(value: &'collection HashMap<TKey, TValue>) -> Self {
//         Self::HashMap(value)
//     }
// }

// impl<'collection, TKey: Eq + Hash, TValue> From<&'collection HashSet<TKey>>
//     for ReadonlyCollection<'collection, TKey, TValue>
// {
//     fn from(value: &'collection HashSet<TKey>) -> Self {
//         Self::HashSet(value)
//     }
// }

pub type Push<TItem> = Vec<TItem>;

// pub trait ReadonlyCollection<TKey> {
//     fn size(&self) -> usize;
//     fh has(&self, key: &TKey) -> bool;
//     fn keys(&self) ->
// }

pub type Comparer<TValue> = fn(a: TValue, b: TValue) -> Comparison;
// pub trait Comparer<TValue>: Fn(TValue, TValue) -> Comparison + 'static {}

// impl<TComparer: 'static, TValue> Comparer<TValue> for TComparer where
//     TComparer: Fn(TValue, TValue) -> Comparison
// {
// }

#[derive(Debug, PartialEq, Eq)]
pub enum Comparison {
    LessThan = -1,
    EqualTo = 0,
    GreaterThan = 1,
}
