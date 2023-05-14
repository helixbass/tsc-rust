#![cfg(test)]

mod comment_parsing {
    use once_cell::sync::Lazy;
    use speculoos::prelude::*;
    use typescript_rust::{
        get_leading_comment_ranges, str_to_source_text_as_chars, SourceTextAsChars, SyntaxKind,
    };

    static with_shebang: Lazy<SourceTextAsChars> = Lazy::new(|| {
        str_to_source_text_as_chars(
            "#! node
/** comment */
// another one
;",
        )
    });
    static no_shebang: Lazy<SourceTextAsChars> = Lazy::new(|| {
        str_to_source_text_as_chars(
            "/** comment */
// another one
;",
        )
    });
    static with_trailing: Lazy<SourceTextAsChars> = Lazy::new(|| {
        str_to_source_text_as_chars(
            ";/* comment */
// another one
",
        )
    });

    #[test]
    fn test_skips_shebang() {
        let result = get_leading_comment_ranges(&with_shebang, 0);
        assert_that(&result).is_some().has_length(2);
    }

    #[test]
    fn test_treats_all_comments_at_start_of_file_as_leading_comments() {
        let result = get_leading_comment_ranges(&no_shebang, 0);
        assert_that(&result).is_some().has_length(2);
    }

    #[test]
    fn test_returns_leading_comments_if_position_is_not_0() {
        let result = get_leading_comment_ranges(&with_trailing, 1);
        assert_that(&result).is_some();
        let result = result.unwrap();
        assert_that(&result).has_length(1);
        assert_that(&result[0].kind).is_equal_to(SyntaxKind::SingleLineCommentTrivia);
    }
}
