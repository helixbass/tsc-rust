pub fn for_each<TItem, TReturn>(
    array: &[TItem],
    callback: &mut dyn FnMut(&TItem, usize) -> Option<TReturn>,
) -> Option<TReturn> {
    array
        .iter()
        .enumerate()
        .find_map(|(index, item)| callback(item, index))
}

fn some<TItem>(array: &[TItem], predicate: Option<Box<dyn FnMut(&TItem) -> bool>>) -> bool {
    predicate.map_or(!array.is_empty(), |predicate| array.iter().any(predicate))
}

pub fn concatenate<TItem>(mut array1: Vec<TItem>, mut array2: Vec<TItem>) -> Vec<TItem> {
    if !some(&array2, None) {
        return array1;
    }
    if !some(&array1, None) {
        return array2;
    }
    array1.append(&mut array2);
    array1
}

pub fn last_or_undefined<TItem>(array: &[TItem]) -> Option<&TItem> {
    array.last()
}
