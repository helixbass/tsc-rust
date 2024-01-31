use gc::Gc;
use id_arena::Id;

use super::{Instruction, Label, TransformGenerators};
use crate::{
    EmitFlags, Node, NodeExt, ReadonlyTextRange,
    InArena,
};

impl TransformGenerators {
    pub(super) fn write_throw(
        &self,
        expression: Id<Node /*Expression*/>,
        operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        self.set_last_operation_was_abrupt(true);
        self.set_last_operation_was_completion(true);
        self.write_statement(
            self.factory
                .ref_(self).create_throw_statement(expression)
                .set_text_range(operation_location, self),
        );
    }

    pub(super) fn write_return(
        &self,
        expression: Option<Id<Node>>,
        operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        self.set_last_operation_was_abrupt(true);
        self.set_last_operation_was_completion(true);
        self.write_statement(
            self.factory
                .ref_(self).create_return_statement(Some(self.factory.ref_(self).create_array_literal_expression(
                    Some(if let Some(expression) = expression {
                        vec![self.create_instruction(Instruction::Return), expression]
                    } else {
                        vec![self.create_instruction(Instruction::Return)]
                    }),
                    None,
                )))
                .set_text_range(operation_location, self)
                .set_emit_flags(EmitFlags::NoTokenSourceMaps, self),
        );
    }

    pub(super) fn write_break(
        &self,
        label: Label,
        operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        self.set_last_operation_was_abrupt(true);
        self.write_statement(
            self.factory
                .ref_(self).create_return_statement(Some(self.factory.ref_(self).create_array_literal_expression(
                    Some(vec![
                        self.create_instruction(Instruction::Break),
                        self.create_label(Some(label)),
                    ]),
                    None,
                )))
                .set_text_range(operation_location, self)
                .set_emit_flags(EmitFlags::NoTokenSourceMaps, self),
        );
    }

    pub(super) fn write_break_when_true(
        &self,
        label: Label,
        condition: Id<Node /*Expression*/>,
        operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        self.write_statement(
            self.factory
                .ref_(self).create_if_statement(
                    condition,
                    self.factory
                        .ref_(self).create_return_statement(Some(
                            self.factory.ref_(self).create_array_literal_expression(
                                Some(vec![
                                    self.create_instruction(Instruction::Break),
                                    self.create_label(Some(label)),
                                ]),
                                None,
                            ),
                        ))
                        .set_text_range(operation_location, self)
                        .set_emit_flags(EmitFlags::NoTokenSourceMaps, self),
                    None,
                )
                .set_emit_flags(EmitFlags::SingleLine, self),
        );
    }

    pub(super) fn write_break_when_false(
        &self,
        label: Label,
        condition: Id<Node /*Expression*/>,
        operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        self.write_statement(
            self.factory
                .ref_(self).create_if_statement(
                    self.factory.ref_(self).create_logical_not(condition),
                    self.factory
                        .ref_(self).create_return_statement(Some(
                            self.factory.ref_(self).create_array_literal_expression(
                                Some(vec![
                                    self.create_instruction(Instruction::Break),
                                    self.create_label(Some(label)),
                                ]),
                                None,
                            ),
                        ))
                        .set_text_range(operation_location, self)
                        .set_emit_flags(EmitFlags::NoTokenSourceMaps, self),
                    None,
                )
                .set_emit_flags(EmitFlags::SingleLine, self),
        );
    }

    pub(super) fn write_yield(
        &self,
        expression: Id<Node /*Expression*/>,
        operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        self.set_last_operation_was_abrupt(true);
        self.write_statement(
            self.factory
                .ref_(self).create_return_statement(Some(self.factory.ref_(self).create_array_literal_expression(
                    // expression ?
                    Some(vec![
                        self.create_instruction(Instruction::Yield),
                        expression,
                    ]),
                    // : [createInstruction(Instruction.Yield)]
                    None,
                )))
                .set_text_range(operation_location, self)
                .set_emit_flags(EmitFlags::NoTokenSourceMaps, self),
        );
    }

    pub(super) fn write_yield_star(
        &self,
        expression: Id<Node /*Expression*/>,
        operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        self.set_last_operation_was_abrupt(true);
        self.write_statement(
            self.factory
                .ref_(self).create_return_statement(Some(self.factory.ref_(self).create_array_literal_expression(
                    Some(vec![
                        self.create_instruction(Instruction::YieldStar),
                        expression,
                    ]),
                    None,
                )))
                .set_text_range(operation_location, self)
                .set_emit_flags(EmitFlags::NoTokenSourceMaps, self),
        );
    }

    pub(super) fn write_end_finally(&self) {
        self.set_last_operation_was_abrupt(true);
        self.write_statement(self.factory.ref_(self).create_return_statement(Some(
            self.factory.ref_(self).create_array_literal_expression(
                Some(vec![self.create_instruction(Instruction::Endfinally)]),
                None,
            ),
        )));
    }
}
