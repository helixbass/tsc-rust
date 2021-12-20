pub struct SortedArray<TItem> {
    _vec: Vec<TItem>,
}

impl<TItem> SortedArray<TItem> {
    pub fn new(vec: Vec<TItem>) -> Self {
        Self { _vec: vec }
    }

    pub fn push(&mut self, item: TItem) {
        self._vec.push(item);
    }

    pub fn is_empty(&self) -> bool {
        self._vec.is_empty()
    }
}

impl<TItem: Clone> From<SortedArray<TItem>> for Vec<TItem> {
    fn from(sorted_array: SortedArray<TItem>) -> Self {
        sorted_array._vec
    }
}

impl<TItem: Clone> From<&SortedArray<TItem>> for Vec<TItem> {
    fn from(sorted_array: &SortedArray<TItem>) -> Self {
        sorted_array._vec.clone()
    }
}
