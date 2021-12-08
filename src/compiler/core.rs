pub fn for_each<TItem, TReturn>(
    array: &[TItem],
    callback: &mut dyn FnMut(&TItem, usize) -> Option<TReturn>,
) -> Option<TReturn> {
    for (index, item) in array.iter().enumerate() {
        let result = callback(item, index);
        if let Some(value) = result {
            return Some(value);
        }
    }
    None
}

pub fn last_or_undefined<'vec, TItem>(array: &'vec Vec<TItem>) -> Option<&'vec TItem> {
    array.last()
}
