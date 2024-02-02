use std::io;

use id_arena::Id;

use crate::{Node, TransformationContext, VisitResult};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ProcessLevel {
    LiftRestriction,
    All,
}

pub fn process_tagged_template_expression(
    context: &(impl TransformationContext + ?Sized),
    node: Id<Node>, /*TaggedTemplateExpression*/
    mut visitor: impl FnMut(Id<Node>) -> VisitResult,
    current_source_file: Id<Node>, /*SourceFile*/
    record_tagged_template_string: impl FnMut(Id<Node> /*Identifier*/),
    level: ProcessLevel,
) -> Id<Node> {
    try_process_tagged_template_expression(
        context,
        node,
        |node: Id<Node>| Ok(visitor(node)),
        current_source_file,
        record_tagged_template_string,
        level,
    )
    .unwrap()
}

pub fn try_process_tagged_template_expression(
    _context: &(impl TransformationContext + ?Sized),
    _node: Id<Node>, /*TaggedTemplateExpression*/
    _visitor: impl FnMut(Id<Node>) -> io::Result<VisitResult>,
    _current_source_file: Id<Node>, /*SourceFile*/
    _record_tagged_template_string: impl FnMut(Id<Node> /*Identifier*/),
    _level: ProcessLevel,
) -> io::Result<Id<Node>> {
    unimplemented!()
}
