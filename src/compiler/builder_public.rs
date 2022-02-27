pub trait BuilderProgram {}

pub trait SemanticDiagnosticsBuilderProgram: BuilderProgram {}

pub trait EmitAndSemanticDiagnosticsBuilderProgram: SemanticDiagnosticsBuilderProgram {}
