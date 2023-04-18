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
