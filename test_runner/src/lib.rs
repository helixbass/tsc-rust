#[macro_use]
extern crate lazy_static;

mod test_runner;

pub use test_runner::compiler_runner::{CompilerBaselineRunner, CompilerTestType};
pub use test_runner::runner::run;
