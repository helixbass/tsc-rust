pub trait With {
    type Self_;

    fn with<TMapped>(self, mapper: impl FnMut(Self::Self_) -> TMapped) -> TMapped;
}

impl<T> With for T {
    type Self_ = T;

    fn with<TMapped>(self, mut mapper: impl FnMut(Self) -> TMapped) -> TMapped {
        mapper(self)
    }
}
