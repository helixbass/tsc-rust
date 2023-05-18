use std::{borrow::Borrow, io};

use gc::Gc;

use crate::{Node, ReadonlyTextRange, TransformationContext, VisitResult};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FlattenLevel {
    All,
    ObjectRest,
}

pub fn flatten_destructuring_assignment(
    _node: &Node, /*VariableDeclaration | DestructuringAssignment*/
    _visitor: Option<impl FnMut(&Node) -> VisitResult>,
    _context: &(impl TransformationContext + ?Sized),
    _level: FlattenLevel,
    _needs_value: Option<bool>,
    _create_assignment_callback: Option<
        impl FnMut(
            &Node, /*Identifier*/
            &Node, /*Expression*/
            Option<&dyn ReadonlyTextRange>,
        ) -> Gc<Node /*Expression*/>,
    >,
) -> Gc<Node /*Expression*/> {
    unimplemented!()
}

pub fn try_flatten_destructuring_assignment(
    _node: &Node, /*VariableDeclaration | DestructuringAssignment*/
    _visitor: Option<impl FnMut(&Node) -> io::Result<VisitResult>>,
    _context: &(impl TransformationContext + ?Sized),
    _level: FlattenLevel,
    _needs_value: Option<bool>,
    _create_assignment_callback: Option<
        impl FnMut(
            &Node, /*Identifier*/
            &Node, /*Expression*/
            Option<&dyn ReadonlyTextRange>,
        ) -> io::Result<Gc<Node /*Expression*/>>,
    >,
) -> io::Result<Gc<Node /*Expression*/>> {
    unimplemented!()
}

pub fn flatten_destructuring_binding(
    node: &Node, /*VariableDeclaration | ParameterDeclaration*/
    mut visitor: impl FnMut(&Node) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
    level: FlattenLevel,
    rval: Option<impl Borrow<Node /*Expression*/>>,
    hoist_temp_variables: Option<bool>,
    skip_initializer: Option<bool>,
) -> Vec<Gc<Node /*VariableDeclaration*/>> {
    try_flatten_destructuring_binding(
        node,
        |node: &Node| Ok(visitor(node)),
        context,
        level,
        rval,
        hoist_temp_variables,
        skip_initializer,
    )
    .unwrap()
}

pub fn try_flatten_destructuring_binding(
    _node: &Node, /*VariableDeclaration | ParameterDeclaration*/
    _visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    _context: &(impl TransformationContext + ?Sized),
    _level: FlattenLevel,
    _rval: Option<impl Borrow<Node /*Expression*/>>,
    hoist_temp_variables: Option<bool>,
    _skip_initializer: Option<bool>,
) -> io::Result<Vec<Gc<Node /*VariableDeclaration*/>>> {
    let _hoist_temp_variables = hoist_temp_variables.unwrap_or(false);
    unimplemented!()
}
