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
