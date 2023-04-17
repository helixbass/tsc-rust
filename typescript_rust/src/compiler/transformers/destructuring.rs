use gc::Gc;

use crate::{Node, ReadonlyTextRange, TransformationContext, VisitResult};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FlattenLevel {
    All,
    ObjectRest,
}

pub fn flatten_destructuring_assignment<TCreateAssignmentCallbackLocation: ReadonlyTextRange>(
    node: &Node, /*VariableDeclaration | DestructuringAssignment*/
    visitor: Option<impl FnMut(&Node) -> VisitResult>,
    context: &(impl TransformationContext + ?Sized),
    level: FlattenLevel,
    needs_value: Option<bool>,
    create_assignment_callback: Option<
        impl FnMut(
            &Node, /*Identifier*/
            &Node, /*Expression*/
            Option<&TCreateAssignmentCallbackLocation>,
        ) -> Gc<Node /*Expression*/>,
    >,
) -> Gc<Node /*Expression*/> {
    unimplemented!()
}
