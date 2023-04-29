#![allow(non_upper_case_globals, non_snake_case)]
#[macro_use]
extern crate lazy_static;

mod rust_helpers;
mod test_runner;

pub use crate::test_runner::compiler_runner::{CompilerBaselineRunner, CompilerTestType};
pub use crate::test_runner::runner::{run, Args};

#[cfg(test)]
use crate::rust_helpers::test::GcSlicesAreEqual;
