use std::rc::Rc;

use crate::{CompilerOptions, Node, Program};

pub trait BuilderProgram {
    fn get_program(&self) -> Rc<Program>;
    fn get_compiler_options(&self) -> Rc<CompilerOptions>;
    fn get_source_files(&self) -> &[Rc<Node /*SourceFile*/>];
}

pub trait SemanticDiagnosticsBuilderProgram: BuilderProgram {}

pub trait EmitAndSemanticDiagnosticsBuilderProgram: SemanticDiagnosticsBuilderProgram {}
