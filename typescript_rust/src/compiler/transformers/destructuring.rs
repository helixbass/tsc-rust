use std::borrow::Borrow;

use gc::Gc;

use crate::{Node, ReadonlyTextRange, TransformationContext, VisitResult};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FlattenLevel {
    All,
    ObjectRest,
}

pub fn flatten_destructuring_assignment<TCreateAssignmentCallbackLocation: ReadonlyTextRange>(
    _node: &Node, /*VariableDeclaration | DestructuringAssignment*/
    _visitor: Option<impl FnMut(&Node) -> VisitResult>,
    _context: &(impl TransformationContext + ?Sized),
    _level: FlattenLevel,
    _needs_value: Option<bool>,
    _create_assignment_callback: Option<
        impl FnMut(
            &Node, /*Identifier*/
            &Node, /*Expression*/
            Option<&TCreateAssignmentCallbackLocation>,
        ) -> Gc<Node /*Expression*/>,
    >,
) -> Gc<Node /*Expression*/> {
    unimplemented!()
}

pub fn flatten_destructuring_binding(
    _node: &Node, /*VariableDeclaration | ParameterDeclaration*/
    _visitor: impl FnMut(&Node) -> VisitResult,
    _context: &(impl TransformationContext + ?Sized),
    _level: FlattenLevel,
    _rval: Option<impl Borrow<Node /*Expression*/>>,
    hoist_temp_variables: Option<bool>,
    _skip_initializer: Option<bool>,
) -> Vec<Gc<Node /*VariableDeclaration*/>> {
    let _hoist_temp_variables = hoist_temp_variables.unwrap_or(false);
    unimplemented!()
}
