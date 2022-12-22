pub mod documents {
    use gc::{Finalize, Trace};
    use std::collections::HashMap;

    use crate::Compiler;

    #[derive(Trace, Finalize)]
    pub struct TextDocument {
        pub meta: HashMap<String, String>,
        pub file: String,
        pub text: String,

        _line_starts: Option<Vec<usize>>,
        _test_file: Option<Compiler::TestFile>,
    }

    impl TextDocument {
        pub fn new(file: String, text: String, meta: Option<HashMap<String, String>>) -> Self {
            Self {
                file,
                text,
                meta: meta.unwrap_or_default(),
                _line_starts: None,
                _test_file: None,
            }
        }

        pub fn from_test_file(file: &Compiler::TestFile) -> Self {
            Self::new(
                file.unit_name.clone(),
                file.content.clone(),
                file.file_options.clone(),
            )
        }
    }
}
