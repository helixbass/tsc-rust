pub fn get_byte_order_mark_length(text: &str) -> usize {
    if !text.is_empty() {
        let mut text_chars = text.chars();
        let ch0 = text_chars.next().unwrap();
        if ch0 == '\u{feff}' {
            return 1;
        }
        if ch0 == '\u{fe}' {
            return if text_chars.next() == Some('\u{ff}') {
                2
            } else {
                0
            };
        }
        if ch0 == '\u{ff}' {
            return if text_chars.next() == Some('\u{fe}') {
                2
            } else {
                0
            };
        }
        if ch0 == '\u{ef}' {
            return if text_chars.next() == Some('\u{bb}') && text_chars.next() == Some('\u{bf}') {
                3
            } else {
                0
            };
        }
    }
    0
}

pub fn remove_byte_order_mark(text: String) -> String {
    let length = get_byte_order_mark_length(&text);
    if length > 0 {
        text.chars().skip(length).collect()
    } else {
        text
    }
}

pub fn add_utf8_byte_order_mark(text: String) -> String {
    unimplemented!()
}
