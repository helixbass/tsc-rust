use crate::BuilderProgram;

pub type WatchStatusReporter = ();

pub trait CreateProgram<TBuilderProgram: BuilderProgram> {}

pub trait WatchHost {}
