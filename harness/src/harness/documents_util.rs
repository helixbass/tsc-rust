pub mod documents {
    use crate::Compiler;

    pub struct TextDocument {}

    impl TextDocument {
        pub fn from_test_file(
            file: &Compiler::TestFile,
        ) -> Self {
            unimplemented!()
        }
    }
}
