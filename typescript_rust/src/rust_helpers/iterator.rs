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
