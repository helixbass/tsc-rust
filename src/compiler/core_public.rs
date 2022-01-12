use std::ops::Deref;

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

pub type Comparer<TFirstValue, TSecondValue> = fn(a: TFirstValue, b: TSecondValue) -> Comparison;
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
