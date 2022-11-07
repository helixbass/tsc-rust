use typescript_rust::{CompilerOptions, CompilerOptionsBuilder, ScriptTarget, JsxEmit};

pub fn get_default_compiler_options() -> CompilerOptions {
    CompilerOptionsBuilder::default()
        .target(Some(ScriptTarget::ES5))
        .jsx(Some(JsxEmit::Preserve))
        .build().unwrap()
}
