use gc::{Finalize, Trace};
use serde::Serialize;

#[derive(Serialize, Trace, Finalize)]
pub struct ProgramBuildInfo {}
