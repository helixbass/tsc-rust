#![allow(non_upper_case_globals, non_snake_case)]
#[macro_use]
extern crate lazy_static;

mod rust_helpers;
mod test_runner;

pub use crate::test_runner::compiler_runner::{CompilerBaselineRunner, CompilerTestType};
pub use crate::test_runner::runner::{run, Args};
pub use crate::test_runner::unittests::tsc_watch::helpers::{
    get_diagnostic_message_chain, get_diagnostic_of_file_from, get_diagnostic_of_file_from_program,
};

#[cfg(test)]
#[allow(unused_imports)]
use crate::rust_helpers::test::GcSlicesAreEqual;
