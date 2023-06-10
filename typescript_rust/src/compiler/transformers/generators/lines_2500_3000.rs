use gc::Gc;

use super::{Label, OpCode, OperationArguments, TransformGenerators};
use crate::{Node, ReadonlyTextRange};

impl TransformGenerators {
    pub(super) fn emit_statement(&self, _node: Gc<Node /*Statement*/>) {
        unimplemented!()
    }

    pub(super) fn emit_assignment(
        &self,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        self.emit_worker(OpCode::Assign, Some(&(left, right).into()), location);
    }

    pub(super) fn emit_break(&self, label: Label, location: Option<&impl ReadonlyTextRange>) {
        self.emit_worker(OpCode::Break, Some(&label.into()), location);
    }

    pub(super) fn emit_break_when_true(
        &self,
        label: Label,
        condition: Gc<Node /*Expression*/>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        self.emit_worker(
            OpCode::BreakWhenTrue,
            Some(&(label, condition).into()),
            location,
        );
    }

    pub(super) fn emit_break_when_false(
        &self,
        label: Label,
        condition: Gc<Node /*Expression*/>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        self.emit_worker(
            OpCode::BreakWhenFalse,
            Some(&(label, condition).into()),
            location,
        );
    }

    pub(super) fn emit_yield_star(
        &self,
        expression: Option<Gc<Node /*Expression*/>>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        self.emit_worker(OpCode::YieldStar, Some(&expression.into()), location);
    }

    pub(super) fn emit_yield(
        &self,
        expression: Option<Gc<Node /*Expression*/>>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        self.emit_worker(OpCode::Yield, Some(&expression.into()), location);
    }

    pub(super) fn emit_return(
        &self,
        expression: Option<Gc<Node /*Expression*/>>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        self.emit_worker(OpCode::Return, Some(&expression.into()), location);
    }

    pub(super) fn emit_throw(
        &self,
        expression: Gc<Node /*Expression*/>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        self.emit_worker(OpCode::Throw, Some(&expression.into()), location);
    }

    pub(super) fn build(&self) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn emit_worker(
        &self,
        _code: OpCode,
        _args: Option<&OperationArguments>,
        _location: Option<&impl ReadonlyTextRange>,
    ) {
        unimplemented!()
    }
}
