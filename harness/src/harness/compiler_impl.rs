pub mod compiler {
    use std::rc::Rc;
    use typescript_rust::CompilerOptions;

    pub struct CompilationResult {
        pub options: Rc<CompilerOptions>,
    }
}
