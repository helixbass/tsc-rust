use gc::Gc;
use id_arena::Id;

use crate::{
    create_compiler_host_worker, get_sys, BuilderProgram, CompilerHost, CompilerOptions,
    ConfigFileDiagnosticsReporter, Diagnostic, System,
    HasArena,
};

pub fn create_incremental_compiler_host(
    options: Gc<CompilerOptions>,
    system: Option<Id<Box<dyn System>>>,
    arena: &impl HasArena,
) -> impl CompilerHost {
    let system = system.unwrap_or_else(|| get_sys(arena));
    let host = create_compiler_host_worker(options, None, Some(system), arena);
    host
}

pub trait WatchStatusReporter {
    fn call(
        &self,
        diagnostic: Gc<Diagnostic>,
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

pub fn create_watch_program(_host: &dyn WatchCompilerHostOfConfigFile<impl BuilderProgram>) /* -> */
{
    unimplemented!()
}
