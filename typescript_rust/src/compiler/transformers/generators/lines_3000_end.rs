use gc::Gc;

use super::{Instruction, Label, TransformGenerators};
use crate::{EmitFlags, Node, NodeExt, ReadonlyTextRange};

impl TransformGenerators {
    pub(super) fn write_throw(
        &self,
        expression: Gc<Node /*Expression*/>,
        operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        self.set_last_operation_was_abrupt(true);
        self.set_last_operation_was_completion(true);
        self.write_statement(
            self.factory
                .create_throw_statement(expression)
                .set_text_range(operation_location),
        );
    }

    pub(super) fn write_return(
        &self,
        expression: Option<Gc<Node>>,
        operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        self.set_last_operation_was_abrupt(true);
        self.set_last_operation_was_completion(true);
        self.write_statement(
            self.factory
                .create_return_statement(Some(self.factory.create_array_literal_expression(
                    Some(if let Some(expression) = expression {
                        vec![self.create_instruction(Instruction::Return), expression]
                    } else {
                        vec![self.create_instruction(Instruction::Return)]
                    }),
                    None,
                )))
                .set_text_range(operation_location)
                .set_emit_flags(EmitFlags::NoTokenSourceMaps),
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
                .create_return_statement(Some(self.factory.create_array_literal_expression(
                    Some(vec![
                        self.create_instruction(Instruction::Break),
                        self.create_label(Some(label)),
                    ]),
                    None,
                )))
                .set_text_range(operation_location)
                .set_emit_flags(EmitFlags::NoTokenSourceMaps),
        );
    }

    pub(super) fn write_break_when_true(
        &self,
        label: Label,
        condition: Gc<Node /*Expression*/>,
        operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        self.write_statement(
            self.factory
                .create_if_statement(
                    condition,
                    self.factory
                        .create_return_statement(Some(
                            self.factory.create_array_literal_expression(
                                Some(vec![
                                    self.create_instruction(Instruction::Break),
                                    self.create_label(Some(label)),
                                ]),
                                None,
                            ),
                        ))
                        .set_text_range(operation_location)
                        .set_emit_flags(EmitFlags::NoTokenSourceMaps),
                    None,
                )
                .set_emit_flags(EmitFlags::SingleLine),
        );
    }

    pub(super) fn write_break_when_false(
        &self,
        label: Label,
        condition: Gc<Node /*Expression*/>,
        operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        self.write_statement(
            self.factory
                .create_if_statement(
                    self.factory.create_logical_not(condition),
                    self.factory
                        .create_return_statement(Some(
                            self.factory.create_array_literal_expression(
                                Some(vec![
                                    self.create_instruction(Instruction::Break),
                                    self.create_label(Some(label)),
                                ]),
                                None,
                            ),
                        ))
                        .set_text_range(operation_location)
                        .set_emit_flags(EmitFlags::NoTokenSourceMaps),
                    None,
                )
                .set_emit_flags(EmitFlags::SingleLine),
        );
    }

    pub(super) fn write_yield(
        &self,
        expression: Gc<Node /*Expression*/>,
        operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        self.set_last_operation_was_abrupt(true);
        self.write_statement(
            self.factory
                .create_return_statement(Some(self.factory.create_array_literal_expression(
                    // expression ?
                    Some(vec![
                        self.create_instruction(Instruction::Yield),
                        expression,
                    ]),
                    // : [createInstruction(Instruction.Yield)]
                    None,
                )))
                .set_text_range(operation_location)
                .set_emit_flags(EmitFlags::NoTokenSourceMaps),
        );
    }

    pub(super) fn write_yield_star(
        &self,
        expression: Gc<Node /*Expression*/>,
        operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        self.set_last_operation_was_abrupt(true);
        self.write_statement(
            self.factory
                .create_return_statement(Some(self.factory.create_array_literal_expression(
                    Some(vec![
                        self.create_instruction(Instruction::YieldStar),
                        expression,
                    ]),
                    None,
                )))
                .set_text_range(operation_location)
                .set_emit_flags(EmitFlags::NoTokenSourceMaps),
        );
    }

    pub(super) fn write_end_finally(&self) {
        self.set_last_operation_was_abrupt(true);
        self.write_statement(self.factory.create_return_statement(Some(
            self.factory.create_array_literal_expression(
                Some(vec![self.create_instruction(Instruction::Endfinally)]),
                None,
            ),
        )));
    }
}
