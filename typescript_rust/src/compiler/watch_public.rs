use gc::Gc;
use std::rc::Rc;

use crate::{
    create_compiler_host_worker, get_sys, BuilderProgram, CompilerHost, CompilerOptions,
    ConfigFileDiagnosticsReporter, Diagnostic, System,
};

pub fn create_incremental_compiler_host(
    options: Gc<CompilerOptions>,
    system: Option<Rc<dyn System>>,
) -> impl CompilerHost {
    let system = system.unwrap_or_else(|| get_sys());
    let host = create_compiler_host_worker(options, None, Some(system));
    host
}

pub trait WatchStatusReporter {
    fn call(
        &self,
        diagnostic: Rc<Diagnostic>,
        new_line: &str,
        options: Gc<CompilerOptions>,
        error_count: Option<usize>,
    );
}

pub trait CreateProgram<TBuilderProgram: BuilderProgram> {}

pub trait WatchHost {}

pub trait ProgramHost<TBuilderProgram: BuilderProgram> {}

pub trait WatchCompilerHost<TBuilderProgram: BuilderProgram>:
    ProgramHost<TBuilderProgram> + WatchHost
{
    fn as_program_host(&self) -> &dyn ProgramHost<TBuilderProgram>;
}

pub trait WatchCompilerHostOfConfigFile<TBuilderProgram: BuilderProgram>:
    WatchCompilerHost<TBuilderProgram> + ConfigFileDiagnosticsReporter
{
}

pub fn create_watch_program<TBuilderProgram: BuilderProgram>(
    host: &dyn WatchCompilerHostOfConfigFile<TBuilderProgram>,
) /* -> */
{
    unimplemented!()
}
