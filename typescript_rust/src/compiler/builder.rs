use gc::{Finalize, Trace};
use serde::Serialize;

#[derive(Clone, Serialize, Trace, Finalize)]
pub struct ProgramBuildInfo {}
