pub mod documents {
    use std::collections::HashMap;

    use gc::{Finalize, Gc, GcCell, GcCellRef, Trace};
    use typescript_rust::{
        GetOrInsertDefault, _d, compute_line_starts, gc_cell_ref_unwrapped, SourceTextAsChars,
    };

    use crate::Compiler;

    #[derive(Trace, Finalize)]
    pub struct TextDocument {
        pub meta: HashMap<String, String>,
        pub file: String,
        pub text: String,
        pub text_as_chars: SourceTextAsChars,

        _line_starts: GcCell<Option<Vec<usize>>>,
        _test_file: GcCell<Option<Gc<Compiler::TestFile>>>,
    }

    impl TextDocument {
        pub fn new(
            file: String,
            text: String,
            text_as_chars: SourceTextAsChars,
            meta: Option<HashMap<String, String>>,
        ) -> Self {
            Self {
                file,
                text,
                text_as_chars,
                meta: meta.unwrap_or_default(),
                _line_starts: _d(),
                _test_file: _d(),
            }
        }

        pub fn line_starts(&self) -> GcCellRef<Vec<usize>> {
            self._line_starts
                .borrow_mut()
                .get_or_insert_with(|| compute_line_starts(&self.text_as_chars));
            gc_cell_ref_unwrapped(&self._line_starts)
        }

        pub fn from_test_file(file: &Compiler::TestFile) -> Self {
            Self::new(
                file.unit_name.clone(),
                file.content.clone(),
                file.content.chars().collect(),
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
