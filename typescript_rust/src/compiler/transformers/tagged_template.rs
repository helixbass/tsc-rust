use gc::Gc;

use crate::{Node, TransformationContext, VisitResult};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ProcessLevel {
    LiftRestriction,
    All,
}

pub fn process_tagged_template_expression(
    context: &(impl TransformationContext + ?Sized),
    node: &Node, /*TaggedTemplateExpression*/
    visitor: impl FnMut(&Node) -> VisitResult,
    current_source_file: &Node, /*SourceFile*/
    record_tagged_template_string: impl FnMut(&Node /*Identifier*/),
    level: ProcessLevel,
) -> Gc<Node> {
    unimplemented!()
}
