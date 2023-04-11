pub trait NonEmpty {
    fn non_empty(self) -> Self;
    fn is_non_empty(self) -> bool;
}

impl NonEmpty for Option<&str> {
    fn non_empty(self) -> Self {
        self.filter(|value| !value.is_empty())
    }

    fn is_non_empty(self) -> bool {
        self.non_empty().is_some()
    }
}

impl NonEmpty for Option<&String> {
    fn non_empty(self) -> Self {
        self.filter(|value| !value.is_empty())
    }

    fn is_non_empty(self) -> bool {
        self.non_empty().is_some()
    }
}

impl NonEmpty for Option<String> {
    fn non_empty(self) -> Self {
        self.filter(|value| !value.is_empty())
    }

    fn is_non_empty(self) -> bool {
        self.non_empty().is_some()
    }
}

impl<TItem> NonEmpty for Option<&[TItem]> {
    fn non_empty(self) -> Self {
        self.filter(|value| !value.is_empty())
    }

    fn is_non_empty(self) -> bool {
        self.non_empty().is_some()
    }
}

impl<TItem> NonEmpty for Option<Vec<TItem>> {
    fn non_empty(self) -> Self {
        self.filter(|value| !value.is_empty())
    }

    fn is_non_empty(self) -> bool {
        self.non_empty().is_some()
    }
}

pub trait GetOrInsertDefault {
    type Unwrapped;

    fn get_or_insert_default(&mut self) -> &mut Self::Unwrapped;
}

impl<TValue: Default> GetOrInsertDefault for Option<TValue> {
    type Unwrapped = TValue;

    fn get_or_insert_default(&mut self) -> &mut TValue {
        self.get_or_insert_with(|| Default::default())
    }
}

pub trait MapOrDefault {
    type Unwrapped;

    fn map_or_default<TMapped: Default>(
        self,
        mapper: impl FnOnce(Self::Unwrapped) -> TMapped,
    ) -> TMapped;
}

impl<TValue> MapOrDefault for Option<TValue> {
    type Unwrapped = TValue;

    fn map_or_default<TMapped: Default>(
        self,
        mapper: impl FnOnce(Self::Unwrapped) -> TMapped,
    ) -> TMapped {
        self.map(mapper).unwrap_or_default()
    }
}

pub trait ThenAnd {
    fn then_and<TMapped>(self, mapper: impl FnOnce() -> Option<TMapped>) -> Option<TMapped>;
}

impl ThenAnd for bool {
    fn then_and<TMapped>(self, mapper: impl FnOnce() -> Option<TMapped>) -> Option<TMapped> {
        self.then(mapper).flatten()
    }
}
