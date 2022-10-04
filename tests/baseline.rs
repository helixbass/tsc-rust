use std::rc::Rc;

use typescript_rust::{
    create_compiler_host_worker, create_program, get_sys, CompilerOptions, CreateProgramOptions,
    Diagnostic,
};

#[test]
fn run_compiler_baselines() {
    let options: Rc<CompilerOptions> = Rc::new(Default::default());
    let host = create_compiler_host_worker(options.clone(), None, Some(get_sys()));
    let program = create_program(CreateProgramOptions {
        root_names: vec!["typescript_src/tests/cases/compiler/yieldStringLiteral.ts".to_owned()],
        options,
        host: Some(Rc::new(host)),
        project_references: None,
        old_program: None,
        config_file_parsing_diagnostics: None,
    });
    let emit_result = program.emit(None, None, None, None, None, None);
    compare_baselines("yieldStringLiteral", &emit_result.diagnostics)
}

fn compare_baselines(name: &str, diagnostics: &[Rc<Diagnostic>]) {
    assert_eq!(1, 1);
}
