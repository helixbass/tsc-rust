use gc::{Finalize, Gc, Trace};

use crate::{CompilerOptions, Node, Program};

pub trait BuilderProgram: Trace + Finalize {
    fn get_program(&self) -> Gc<Box<Program>>;
    fn get_compiler_options(&self) -> Gc<CompilerOptions>;
    fn get_source_files(&self) -> &[Gc<Node /*SourceFile*/>];
}

pub trait SemanticDiagnosticsBuilderProgram: BuilderProgram {}

pub trait EmitAndSemanticDiagnosticsBuilderProgram: SemanticDiagnosticsBuilderProgram {}
