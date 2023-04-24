use either_n::Either3;
use peekmore::PeekMoreIterator;
use std::iter::Peekable;

pub trait IteratorExt {
    fn empty(self) -> bool;
}

impl<TIterator> IteratorExt for TIterator
where
    TIterator: Iterator,
{
    fn empty(mut self) -> bool {
        self.next().is_none()
    }
}

pub trait IsEmpty {
    fn is_empty(&mut self) -> bool;
}

impl<I> IsEmpty for Peekable<I>
where
    I: Iterator,
{
    fn is_empty(&mut self) -> bool {
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
        self.peek_nth(n + 1).is_some()
    }

    fn is_len_equal_to(&mut self, n: usize) -> bool {
        self.peek_nth(n).is_some() && self.peek_nth(n + 1).is_none()
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
