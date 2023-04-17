pub trait VecExt {
    type Item;

    fn and_push(self, item: Self::Item) -> Self;
}

impl<TItem> VecExt for Vec<TItem> {
    type Item = TItem;

    fn and_push(mut self, item: TItem) -> Self {
        self.push(item);
        self
    }
}
