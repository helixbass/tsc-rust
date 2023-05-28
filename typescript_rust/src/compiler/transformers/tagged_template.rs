use std::io;

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
    mut visitor: impl FnMut(&Node) -> VisitResult,
    current_source_file: &Node, /*SourceFile*/
    record_tagged_template_string: impl FnMut(&Node /*Identifier*/),
    level: ProcessLevel,
) -> Gc<Node> {
    try_process_tagged_template_expression(
        context,
        node,
        |node: &Node| Ok(visitor(node)),
        current_source_file,
        record_tagged_template_string,
        level,
    )
    .unwrap()
}

pub fn try_process_tagged_template_expression(
    _context: &(impl TransformationContext + ?Sized),
    _node: &Node, /*TaggedTemplateExpression*/
    _visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    _current_source_file: &Node, /*SourceFile*/
    _record_tagged_template_string: impl FnMut(&Node /*Identifier*/),
    _level: ProcessLevel,
) -> io::Result<Gc<Node>> {
    unimplemented!()
}
