use gc::{Finalize, Gc, Trace};
use id_arena::Id;

use crate::{CompilerOptions, Node, Program};

pub trait BuilderProgram: Trace + Finalize {
    fn get_program(&self) -> Id<Program>;
    fn get_compiler_options(&self) -> Id<CompilerOptions>;
    fn get_source_files(&self) -> &[Id<Node /*SourceFile*/>];
}

pub trait SemanticDiagnosticsBuilderProgram: BuilderProgram {}

pub trait EmitAndSemanticDiagnosticsBuilderProgram: SemanticDiagnosticsBuilderProgram {}
