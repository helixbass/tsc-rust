use std::{
    iter::Peekable,
    path::{Path, PathBuf},
};

use either_n::Either3;
use peekmore::PeekMoreIterator;

pub trait IteratorExt {
    type Item;

    fn empty(self) -> bool;
    fn try_all<TError>(
        self,
        callback: impl FnMut(Self::Item) -> Result<bool, TError>,
    ) -> Result<bool, TError>;
    fn try_any<TError>(
        self,
        callback: impl FnMut(Self::Item) -> Result<bool, TError>,
    ) -> Result<bool, TError>;
    fn try_find_<TError>(
        self,
        callback: impl FnMut(&Self::Item) -> Result<bool, TError>,
    ) -> Result<Option<Self::Item>, TError>;
}

impl<TIterator> IteratorExt for TIterator
where
    TIterator: Iterator,
{
    type Item = TIterator::Item;

    fn empty(mut self) -> bool {
        self.next().is_none()
    }

    fn try_all<TError>(
        self,
        mut callback: impl FnMut(Self::Item) -> Result<bool, TError>,
    ) -> Result<bool, TError> {
        for item in self {
            if !callback(item)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn try_any<TError>(
        self,
        mut callback: impl FnMut(Self::Item) -> Result<bool, TError>,
    ) -> Result<bool, TError> {
        for item in self {
            if callback(item)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn try_find_<TError>(
        self,
        mut callback: impl FnMut(&Self::Item) -> Result<bool, TError>,
    ) -> Result<Option<Self::Item>, TError> {
        for item in self {
            if callback(&item)? {
                return Ok(Some(item));
            }
        }
        Ok(None)
    }
}

pub trait PeekableExt {
    fn is_empty_(&mut self) -> bool;
}

impl<I> PeekableExt for Peekable<I>
where
    I: Iterator,
{
    fn is_empty_(&mut self) -> bool {
        self.peek().is_none()
    }
}

pub trait PeekMoreExt {
    fn is_len_greater_than(&mut self, n: usize) -> bool;
    fn is_len_equal_to(&mut self, n: usize) -> bool;
    fn is_empty(&mut self) -> bool {
        self.is_len_equal_to(0)
    }
}

impl<I> PeekMoreExt for PeekMoreIterator<I>
where
    I: Iterator,
{
    fn is_len_greater_than(&mut self, n: usize) -> bool {
        self.peek_nth(n).is_some()
    }

    fn is_len_equal_to(&mut self, n: usize) -> bool {
        (if n == 0 {
            true
        } else {
            self.peek_nth(n - 1).is_some()
        }) && self.peek_nth(n).is_none()
    }
}

pub fn maybe_concat<TItem>(
    a: Option<impl IntoIterator<Item = TItem>>,
    b: Option<impl IntoIterator<Item = TItem>>,
) -> Option<impl Iterator<Item = TItem>> {
    match (a, b) {
        (None, None) => None,
        (Some(a), None) => Some(Either3::One(a.into_iter())),
        (None, Some(b)) => Some(Either3::Two(b.into_iter())),
        (Some(a), Some(b)) => Some(Either3::Three(a.into_iter().chain(b.into_iter()))),
    }
}

pub fn maybe_concat_exact_size<TItem, TIntoIteratorA, TIntoIteratorB>(
    a: Option<TIntoIteratorA>,
    b: Option<TIntoIteratorB>,
    // ) -> Option<impl ExactSizeIterator<Item = TItem>>
) -> Option<(impl Iterator<Item = TItem>, usize)>
where
    TIntoIteratorA: IntoIterator<Item = TItem>,
    TIntoIteratorB: IntoIterator<Item = TItem>,
    TIntoIteratorA::IntoIter: ExactSizeIterator<Item = TItem>,
    TIntoIteratorB::IntoIter: ExactSizeIterator<Item = TItem>,
{
    match (a, b) {
        (None, None) => None,
        (Some(a), None) => {
            let a = a.into_iter();
            let a_len = a.len();
            Some((Either3::One(a), a_len))
        }
        (None, Some(b)) => {
            let b = b.into_iter();
            let b_len = b.len();
            Some((Either3::Two(b), b_len))
        }
        (Some(a), Some(b)) => {
            let a = a.into_iter();
            let a_len = a.len();
            let b = b.into_iter();
            let b_len = b.len();
            Some((Either3::Three(a.chain(b)), a_len + b_len))
        }
    }
}

pub trait ToOwnedNonTrivial {
    type Owned;

    fn to_owned_non_trivial(&self) -> Self::Owned;
}

impl ToOwnedNonTrivial for str {
    type Owned = String;

    fn to_owned_non_trivial(&self) -> Self::Owned {
        self.to_owned()
    }
}

impl ToOwnedNonTrivial for Path {
    type Owned = PathBuf;

    fn to_owned_non_trivial(&self) -> Self::Owned {
        self.to_owned()
    }
}

impl<TItem> ToOwnedNonTrivial for [TItem]
where
    TItem: ToOwnedNonTrivial,
{
    type Owned = Vec<TItem::Owned>;

    fn to_owned_non_trivial(&self) -> Self::Owned {
        self.into_iter()
            .map(|item| item.to_owned_non_trivial())
            .collect()
    }
}

impl<const TLen: usize, TItem> ToOwnedNonTrivial for [TItem; TLen]
where
    TItem: ToOwnedNonTrivial,
{
    type Owned = Vec<TItem::Owned>;

    fn to_owned_non_trivial(&self) -> Self::Owned {
        self.into_iter()
            .map(|item| item.to_owned_non_trivial())
            .collect()
    }
}

impl<TItem> ToOwnedNonTrivial for Vec<TItem>
where
    TItem: ToOwnedNonTrivial,
{
    type Owned = Vec<TItem::Owned>;

    fn to_owned_non_trivial(&self) -> Self::Owned {
        self.into_iter()
            .map(|item| item.to_owned_non_trivial())
            .collect()
    }
}

impl<'any, TToOwnedNonTrivial> ToOwnedNonTrivial for &'any TToOwnedNonTrivial
where
    TToOwnedNonTrivial: ToOwnedNonTrivial + ?Sized,
{
    type Owned = TToOwnedNonTrivial::Owned;

    fn to_owned_non_trivial(&self) -> Self::Owned {
        (*self).to_owned_non_trivial()
    }
}

impl<A, B> ToOwnedNonTrivial for (A, B)
where
    A: ToOwnedNonTrivial,
    B: ToOwnedNonTrivial,
{
    type Owned = (A::Owned, B::Owned);

    fn to_owned_non_trivial(&self) -> Self::Owned {
        (self.0.to_owned_non_trivial(), self.1.to_owned_non_trivial())
    }
}

pub trait Owned {
    type TargetItem;

    fn owned(self) -> Vec<Self::TargetItem>;
}

impl<TIntoIter> Owned for TIntoIter
where
    TIntoIter: IntoIterator,
    TIntoIter::Item: ToOwnedNonTrivial,
{
    type TargetItem = <TIntoIter::Item as ToOwnedNonTrivial>::Owned;

    fn owned(self) -> Vec<Self::TargetItem> {
        self.into_iter()
            .map(|item| item.to_owned_non_trivial())
            .collect()
    }
}
