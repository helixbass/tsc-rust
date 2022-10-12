use crate::{set_text_range_pos_end, ReadonlyTextRange};

pub fn set_text_range<'a, TRange: ReadonlyTextRange, TLocation: ReadonlyTextRange>(
    range: &'a TRange,
    location: Option<&TLocation>,
) -> &'a TRange {
    match location {
        Some(location) => {
            set_text_range_pos_end(range, location.pos(), location.end());
            range
        }
        None => range,
    }
}
