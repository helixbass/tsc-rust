use gc::Gc;

use crate::{Node, TransformationContext, VisitResult};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ProcessLevel {
    LiftRestriction,
    All,
}

pub fn process_tagged_template_expression(
    _context: &(impl TransformationContext + ?Sized),
    _node: &Node, /*TaggedTemplateExpression*/
    _visitor: impl FnMut(&Node) -> VisitResult,
    _current_source_file: &Node, /*SourceFile*/
    _record_tagged_template_string: impl FnMut(&Node /*Identifier*/),
    _level: ProcessLevel,
) -> Gc<Node> {
    unimplemented!()
}
