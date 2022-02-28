use std::rc::Rc;

use crate::{
    create_compiler_host_worker, get_sys, BuilderProgram, CompilerHost, CompilerOptions, System,
};

pub fn create_incremental_compiler_host(
    options: &CompilerOptions,
    system: Option<Rc<dyn System>>,
) -> impl CompilerHost {
    let system = system.unwrap_or_else(|| get_sys());
    let host = create_compiler_host_worker(options, None, Some(system));
    host
}

pub type WatchStatusReporter = ();

pub trait CreateProgram<TBuilderProgram: BuilderProgram> {}

pub trait WatchHost {}

pub trait ProgramHost<TBuilderProgram: BuilderProgram> {}
