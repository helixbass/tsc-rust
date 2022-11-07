pub mod compiler {
    use std::rc::Rc;
    use typescript_rust::CompilerOptions;

    use crate::{fakes, vfs};

    pub struct CompilationResult {
        pub options: Rc<CompilerOptions>,
        pub symlinks: Option<vfs::FileSet>,
    }

    pub fn compile_files(
        host: &fakes::CompilerHost,
        root_files: Option<&[String]>,
        compiler_options: &CompilerOptions,
    ) -> CompilationResult {
        unimplemented!()
    }
}
