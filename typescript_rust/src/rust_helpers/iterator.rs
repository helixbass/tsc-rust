use either_n::Either3;
use peekmore::PeekMoreIterator;
use std::{
    iter::Peekable,
    path::{Path, PathBuf},
};

pub trait Empty {
    fn empty(self) -> bool;
}

impl<TIterator> Empty for TIterator
where
    TIterator: Iterator,
{
    fn empty(mut self) -> bool {
        self.next().is_none()
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

impl<'any, TToOwnedNonTrivial> ToOwnedNonTrivial for &'any TToOwnedNonTrivial
where
    TToOwnedNonTrivial: ToOwnedNonTrivial + ?Sized,
{
    type Owned = TToOwnedNonTrivial::Owned;

    fn to_owned_non_trivial(&self) -> Self::Owned {
        (*self).to_owned_non_trivial()
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

// pub trait Owned {
//     fn owned<TResolvedAsImplToOwnedNonTrivial: ToOwnedNonTrivial<Owned = TTargetItem>, TTargetItem: Borrow<TResolvedAsImplToOwnedNonTrivial>>(self) -> Vec<TTargetItem>;
// }

// impl<TIntoIter> Owned for TIntoIter
//     where TIntoIter: IntoIterator {
//     fn owned<TResolvedAsImplToOwnedNonTrivial: ToOwnedNonTrivial<Owned = TTargetItem>, TTargetItem: Borrow<TResolvedAsImplToOwnedNonTrivial>>(self) -> Vec<TTargetItem> {
//     }
// }

// pub trait Owned {
//     type TargetItem;

//     // fn owned<TResolvedAsImplToOwnedNonTrivial: ToOwnedNonTrivial<Owned = Self::TargetItem>>(
//     fn owned(self) -> Vec<Self::TargetItem>;
// }

// impl<'any, TSourceIntoIter, TSourceItem> Owned for TSourceIntoIter
// where
//     TSourceIntoIter: IntoIterator<Item = TSourceItem>,
//     TSourceItem: AsRef<str>,
// {
//     type TargetItem = String;

//     fn owned(self) -> Vec<Self::TargetItem> {
//         self.into_iter()
//             .map(|item| item.as_ref().to_owned())
//             .collect()
//     }
// }

// impl<'any, TSourceIntoIter> Owned for TSourceIntoIter
// where
//     TSourceIntoIter: IntoIterator<Item = &'any Path>,
// {
//     type TargetItem = PathBuf;

//     fn owned(self) -> Vec<Self::TargetItem> {
//         self.into_iter().map(|item| item.to_owned()).collect()
//     }
// }

// impl<'any, 'any2, TSourceIntoIter> Owned for TSourceIntoIter
// where
//     TSourceIntoIter: IntoIterator<Item = &'any &'any2 str>,
// {
//     type TargetItem = String;

//     fn owned(self) -> Vec<Self::TargetItem> {
//         self.into_iter().map(|item| item.to_owned()).collect()
//     }
// }

// impl<TSourceItem, TSourceItemResolvedAsImplToOwnedNonTrivial, TTargetItem, TSourceIntoIter> Owned
//     for TSourceIntoIter
// where
//     TSourceIntoIter: IntoIterator<Item = TSourceItem>,
//     TSourceItem: AsRef<TSourceItemResolvedAsImplToOwnedNonTrivial>,
//     TTargetItem: Borrow<TSourceItemResolvedAsImplToOwnedNonTrivial>,
//     // TSourceItemResolvedAsImplToOwnedNonTrivial: ToOwnedNonTrivial<Owned = TTargetItem>,
// {
//     // type ResolvedAsImplToOwnedNonTrivial = TSourceItemResolvedAsImplToOwnedNonTrivial;
//     type TargetItem = TTargetItem;

//     fn owned<TResolvedAsImplToOwnedNonTrivial>(self) -> Vec<TTargetItem> {
//         self.into_iter()
//             .map(|item| item.as_ref().to_owned_non_trivial())
//             .collect()
//     }
// }

// impl<'any, TSourceItem, TTargetItem, TSourceIntoIter> Owned for TSourceIntoIter
// where
//     TSourceIntoIter: IntoIterator<Item = &'any TSourceItem>,
//     TTargetItem: Borrow<TSourceItem>,
//     TSourceItem: ToOwned<Owned = TTargetItem> + 'any,
// {
//     type Target = Vec<TTargetItem>;

//     fn owned(self) -> Vec<TTargetItem> {
//         self.into_iter().map(|item| item.to_owned()).collect()
//     }
// }

// impl<'any, TSourceItem, TTargetItem> Owned for [&'any TSourceItem]
// where
//     TTargetItem: Borrow<TSourceItem>,
//     TSourceItem: ToOwned<Owned = TTargetItem> + 'any + ?Sized,
// {
//     type Target = Vec<TTargetItem>;

//     fn owned(&self) -> Vec<TTargetItem> {
//         self.iter().map(|item| (*item).to_owned()).collect()
//     }
// }
