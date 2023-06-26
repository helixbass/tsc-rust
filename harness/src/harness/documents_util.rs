pub mod documents {
    use std::collections::HashMap;

    use gc::{Finalize, Gc, GcCell, Trace};
    use typescript_rust::_d;

    use crate::Compiler;

    #[derive(Trace, Finalize)]
    pub struct TextDocument {
        pub meta: HashMap<String, String>,
        pub file: String,
        pub text: String,

        _line_starts: Option<Vec<usize>>,
        _test_file: GcCell<Option<Gc<Compiler::TestFile>>>,
    }

    impl TextDocument {
        pub fn new(file: String, text: String, meta: Option<HashMap<String, String>>) -> Self {
            Self {
                file,
                text,
                meta: meta.unwrap_or_default(),
                _line_starts: _d(),
                _test_file: _d(),
            }
        }

        pub fn from_test_file(file: &Compiler::TestFile) -> Self {
            Self::new(
                file.unit_name.clone(),
                file.content.clone(),
                file.file_options.clone(),
            )
        }

        pub fn as_test_file(&self) -> Gc<Compiler::TestFile> {
            self._test_file
                .borrow_mut()
                .get_or_insert_with(|| {
                    Gc::new(Compiler::TestFile {
                        unit_name: self.file.clone(),
                        content: self.text.clone(),
                        file_options: Some(self.meta.clone()),
                    })
                })
                .clone()
        }
    }
}
