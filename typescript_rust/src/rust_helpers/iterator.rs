use itertools::Either;
use peekmore::PeekMoreIterator;
use std::iter::Peekable;

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

pub fn either_concat<TItem>(
    a: impl IntoIterator<Item = TItem>,
    b: impl IntoIterator<Item = TItem>,
) -> impl Iterator<Item = TItem> {
    let a = a.into_iter();
    let b = b.into_iter();
    a.map(Either::Left).chain(b.map(Either::Right))
}
