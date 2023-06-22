use std::ops::Deref;

use gc::{Finalize, Gc, Trace};

use crate::add_range;

pub trait VecExt {
    type Item;

    fn and_push(self, item: Self::Item) -> Self;
    fn and_extend(self, iter: impl IntoIterator<Item = Self::Item>) -> Self;
}

impl<TItem> VecExt for Vec<TItem> {
    type Item = TItem;

    fn and_push(mut self, item: TItem) -> Self {
        self.push(item);
        self
    }

    fn and_extend(mut self, iter: impl IntoIterator<Item = TItem>) -> Self {
        self.extend(iter);
        self
    }
}

pub trait VecExtClone {
    type Item: Clone;

    fn add_range(&mut self, from: Option<&[Self::Item]>, start: Option<isize>, end: Option<isize>);
}

impl<TItem: Clone> VecExtClone for Vec<TItem> {
    type Item = TItem;

    fn add_range(&mut self, from: Option<&[Self::Item]>, start: Option<isize>, end: Option<isize>) {
        add_range(self, from, start, end);
    }
}

pub trait VecExtOrd {
    type Item: Ord;

    fn and_sort(self) -> Self;
}

impl<TItem: Ord> VecExtOrd for Vec<TItem> {
    type Item = TItem;

    fn and_sort(mut self) -> Self {
        self.sort();
        self
    }
}

pub trait SliceExtCloneOrd {
    type Item: Clone + Ord;

    fn sorted(self) -> Vec<Self::Item>;
}

impl<TItem: Clone + Ord> SliceExtCloneOrd for &[TItem] {
    type Item = TItem;

    fn sorted(self) -> Vec<TItem> {
        let mut cloned = self.to_owned();
        cloned.sort();
        cloned
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
pub struct GcVec<TItem: Trace + Finalize + 'static>(Gc<Vec<TItem>>);

impl<TItem: Clone + Trace + Finalize + 'static> GcVec<TItem> {
    pub fn owned_iter(&self) -> GcVecOwnedIter<TItem> {
        self.clone().into()
    }
}

impl<TItem: Trace + Finalize + 'static> Deref for GcVec<TItem> {
    type Target = Vec<TItem>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<TItem: Trace + Finalize + 'static> From<Gc<Vec<TItem>>> for GcVec<TItem> {
    fn from(value: Gc<Vec<TItem>>) -> Self {
        Self(value)
    }
}

impl<TItem: Trace + Finalize + 'static> From<Vec<TItem>> for GcVec<TItem> {
    fn from(value: Vec<TItem>) -> Self {
        Gc::new(value).into()
    }
}

#[derive(Clone)]
pub struct GcVecOwnedIter<TItem: Clone + Trace + Finalize + 'static> {
    gc_vec: GcVec<TItem>,
    index: usize,
}

impl<TItem: Clone + Trace + Finalize + 'static> Iterator for GcVecOwnedIter<TItem> {
    type Item = TItem;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.gc_vec.len() {
            None
        } else {
            let ret = self.gc_vec[self.index].clone();
            self.index += 1;
            Some(ret)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.index >= self.gc_vec.len() {
            (0, Some(0))
        } else {
            let remaining = self.gc_vec.len() - 1 - self.index;
            (remaining, Some(remaining))
        }
    }
}

impl<TItem: Clone + Trace + Finalize + 'static> DoubleEndedIterator for GcVecOwnedIter<TItem> {
    fn next_back(&mut self) -> Option<Self::Item> {
        unimplemented!()
    }
}

impl<TItem: Clone + Trace + Finalize + 'static> ExactSizeIterator for GcVecOwnedIter<TItem> {}

impl<TItem: Clone + Trace + Finalize> From<GcVec<TItem>> for GcVecOwnedIter<TItem> {
    fn from(value: GcVec<TItem>) -> Self {
        Self {
            gc_vec: value,
            index: 0,
        }
    }
}
