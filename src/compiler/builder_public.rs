use std::rc::Rc;

use crate::Program;

pub trait BuilderProgram {
    fn get_program(&self) -> Rc<Program>;
}

pub trait SemanticDiagnosticsBuilderProgram: BuilderProgram {}

pub trait EmitAndSemanticDiagnosticsBuilderProgram: SemanticDiagnosticsBuilderProgram {}
