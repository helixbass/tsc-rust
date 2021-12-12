pub fn for_each<TItem, TReturn>(
    array: &[TItem],
    callback: &mut dyn FnMut(&TItem, usize) -> Option<TReturn>,
) -> Option<TReturn> {
    array
        .iter()
        .enumerate()
        .find_map(|(index, item)| callback(item, index))
}

pub fn last_or_undefined<TItem>(array: &[TItem]) -> Option<&TItem> {
    array.last()
}
