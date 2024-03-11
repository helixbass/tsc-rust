pub fn from_char_code(code_points: &[u32]) -> String {
    code_points
        .into_iter()
        .map(|&code_point| {
            char::from_u32(code_point).expect(
                "not supporting surrogate code points (or other invalid u32 chars?) right now",
            )
        })
        .collect()
}

// fn is_surrogate_code_point(code_point: u32) -> bool {
//     code_point >= 0xd800 && code_point < 0xdfff
// }
