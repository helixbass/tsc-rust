use gc::Gc;
use std::rc::Rc;

use crate::{CompilerOptions, Node, Program};

pub trait BuilderProgram {
    fn get_program(&self) -> Gc<Box<Program>>;
    fn get_compiler_options(&self) -> Gc<CompilerOptions>;
    fn get_source_files(&self) -> &[Gc<Node /*SourceFile*/>];
}

pub trait SemanticDiagnosticsBuilderProgram: BuilderProgram {}

pub trait EmitAndSemanticDiagnosticsBuilderProgram: SemanticDiagnosticsBuilderProgram {}
