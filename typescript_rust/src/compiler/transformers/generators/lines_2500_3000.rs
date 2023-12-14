use gc::Gc;
use id_arena::Id;

use super::{BlockAction, CodeBlockKind, Label, OpCode, OperationArguments, TransformGenerators};
use crate::{
    EmitFlags, GetOrInsertDefault, Node, NodeArray, NodeExt, Number, ReadonlyTextRange, _d,
};

impl TransformGenerators {
    pub(super) fn emit_nop(&self) {
        self.emit_worker(OpCode::Nop, None, Option::<Id<Node>>::None);
    }

    pub(super) fn emit_statement(&self, node: Id<Node /*Statement*/>) {
        // if (node) {
        self.emit_worker(
            OpCode::Statement,
            Some(node.into()),
            Option::<Id<Node>>::None,
        );
        // } else {
        //     emitNop();
        // }
    }

    pub(super) fn emit_assignment(
        &self,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        self.emit_worker(OpCode::Assign, Some((left, right).into()), location);
    }

    pub(super) fn emit_break(&self, label: Label, location: Option<&impl ReadonlyTextRange>) {
        self.emit_worker(OpCode::Break, Some(label.into()), location);
    }

    pub(super) fn emit_break_when_true(
        &self,
        label: Label,
        condition: Id<Node /*Expression*/>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        self.emit_worker(
            OpCode::BreakWhenTrue,
            Some((label, condition).into()),
            location,
        );
    }

    pub(super) fn emit_break_when_false(
        &self,
        label: Label,
        condition: Id<Node /*Expression*/>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        self.emit_worker(
            OpCode::BreakWhenFalse,
            Some((label, condition).into()),
            location,
        );
    }

    pub(super) fn emit_yield_star(
        &self,
        expression: Option<Id<Node /*Expression*/>>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        self.emit_worker(OpCode::YieldStar, Some(expression.into()), location);
    }

    pub(super) fn emit_yield(
        &self,
        expression: Option<Id<Node /*Expression*/>>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        self.emit_worker(OpCode::Yield, Some(expression.into()), location);
    }

    pub(super) fn emit_return(
        &self,
        expression: Option<Id<Node /*Expression*/>>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        self.emit_worker(OpCode::Return, Some(expression.into()), location);
    }

    pub(super) fn emit_throw(
        &self,
        expression: Id<Node /*Expression*/>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        self.emit_worker(OpCode::Throw, Some(expression.into()), location);
    }

    pub(super) fn emit_end_finally(&self) {
        self.emit_worker(OpCode::Endfinally, None, Option::<Id<Node>>::None);
    }

    pub(super) fn emit_worker(
        &self,
        code: OpCode,
        args: Option<OperationArguments>,
        location: Option<&impl ReadonlyTextRange>,
    ) {
        if self.maybe_operations().is_none() {
            self.set_operations(Some(_d()));
            self.set_operation_arguments(Some(_d()));
            self.set_operation_locations(Some(_d()));
        }

        if self.maybe_label_offsets().is_none() {
            self.mark_label(self.define_label());
        }

        // const operationIndex = operations.length;
        self.operations_mut().push(code);
        self.operation_arguments_mut().push(args);
        self.operation_locations_mut()
            .push(location.map(Into::into));
    }

    pub(super) fn build(&self) -> Id<Node> {
        self.set_block_index(0);
        self.set_label_number(0);
        self.set_label_numbers(None);
        self.set_last_operation_was_abrupt(false);
        self.set_last_operation_was_completion(false);
        self.set_clauses(None);
        self.set_statements(None);
        self.set_exception_block_stack(None);
        self.set_current_exception_block(None);
        self.set_with_block_stack(None);

        let build_result = self.build_statements();
        let build_result_is_empty = build_result.is_empty();
        self.emit_helpers().create_generator_helper(
            self.factory
                .create_function_expression(
                    Option::<Gc<NodeArray>>::None,
                    None,
                    Option::<Id<Node>>::None,
                    Option::<Gc<NodeArray>>::None,
                    Some(vec![self.factory.create_parameter_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        None,
                        self.maybe_state(),
                        None,
                        None,
                        None,
                    )]),
                    None,
                    self.factory
                        .create_block(build_result, Some(!build_result_is_empty)),
                )
                .set_emit_flags(EmitFlags::ReuseTempVariableScope),
        )
    }

    pub(super) fn build_statements(&self) -> Vec<Id<Node /*Statement*/>> {
        if let Some(operations) = self.maybe_operations().as_ref() {
            for operation_index in 0..operations.len() {
                self.write_operation(operation_index);
            }

            self.flush_final_label(operations.len());
        } else {
            self.flush_final_label(0);
        }

        if let Some(clauses) = self.maybe_clauses().as_ref() {
            let label_expression = self
                .factory
                .create_property_access_expression(self.state(), "label");
            let switch_statement = self.factory.create_switch_statement(
                label_expression,
                self.factory.create_case_block(clauses.clone()),
            );
            return vec![switch_statement.start_on_new_line()];
        }

        if let Some(statements) = self.maybe_statements().as_ref() {
            return statements.clone();
        }

        _d()
    }

    pub(super) fn flush_label(&self) {
        if self.maybe_statements().is_none() {
            return;
        }

        self.append_label(!self.last_operation_was_abrupt());

        self.set_last_operation_was_abrupt(false);
        self.set_last_operation_was_completion(false);
        self.set_label_number(self.label_number() + 1);
    }

    pub(super) fn flush_final_label(&self, operation_index: usize) {
        if self.is_final_label_reachable(operation_index) {
            self.try_enter_label(operation_index);
            self.set_with_block_stack(None);
            self.write_return(None, Option::<Id<Node>>::None);
        }

        if self.maybe_statements().is_some() && self.maybe_clauses().is_some() {
            self.append_label(false);
        }

        self.update_label_expressions();
    }

    pub(super) fn is_final_label_reachable(&self, operation_index: usize) -> bool {
        if !self.last_operation_was_completion() {
            return true;
        }

        if self.maybe_label_offsets().is_none() || self.maybe_label_expressions().is_none() {
            return false;
        }

        for (&label, &label_offset) in &*self.label_offsets() {
            if label_offset == Some(operation_index)
                && self.label_expressions().contains_key(&label)
            {
                return true;
            }
        }

        false
    }

    pub(super) fn append_label(&self, mark_label_end: bool) {
        let mut clauses = self.maybe_clauses_mut();
        let clauses = clauses.get_or_insert_default_();

        if let Some(statements) = self.maybe_statements_mut().as_mut() {
            if let Some(with_block_stack) = self.maybe_with_block_stack().as_ref() {
                for with_block in with_block_stack.into_iter().rev() {
                    *statements = vec![self.factory.create_with_statement(
                        (**with_block).borrow().as_with_block().expression.clone(),
                        self.factory.create_block(statements.clone(), None),
                    )];
                }
            }

            if let Some(current_exception_block) = self.maybe_current_exception_block() {
                let current_exception_block = (*current_exception_block).borrow();
                let current_exception_block_as_exception_block =
                    current_exception_block.as_exception_block();
                let start_label = current_exception_block_as_exception_block.start_label;
                let catch_label = current_exception_block_as_exception_block.catch_label;
                let finally_label = current_exception_block_as_exception_block.finally_label;
                let end_label = current_exception_block_as_exception_block.end_label;
                statements.insert(
                    0,
                    self.factory.create_expression_statement(
                        self.factory.create_call_expression(
                            self.factory.create_property_access_expression(
                                self.factory
                                    .create_property_access_expression(self.state(), "trys"),
                                "push",
                            ),
                            Option::<Gc<NodeArray>>::None,
                            Some(vec![self.factory.create_array_literal_expression(
                                Some(vec![
                                    self.create_label(Some(start_label)),
                                    self.create_label(catch_label),
                                    self.create_label(finally_label),
                                    self.create_label(Some(end_label)),
                                ]),
                                None,
                            )]),
                        ),
                    ),
                );

                self.set_current_exception_block(None);
            }

            if mark_label_end {
                statements.push(
                    self.factory.create_expression_statement(
                        self.factory.create_assignment(
                            self.factory
                                .create_property_access_expression(self.state(), "label"),
                            self.factory.create_numeric_literal(
                                Number::new((self.label_number() + 1) as f64),
                                None,
                            ),
                        ),
                    ),
                );
            }
        }

        clauses.push(
            self.factory.create_case_clause(
                self.factory
                    .create_numeric_literal(Number::new(self.label_number() as f64), None),
                self.maybe_statements().clone().unwrap_or_default(),
            ),
        );

        self.set_statements(None);
    }

    pub(super) fn try_enter_label(&self, operation_index: usize) {
        if self.maybe_label_offsets().is_none() {
            return;
        }

        for (&label, &label_offset) in &*self.label_offsets() {
            if label_offset == Some(operation_index) {
                self.flush_label();
                self.maybe_label_numbers_mut()
                    .get_or_insert_default_()
                    .entry(self.label_number())
                    .or_default()
                    .push(label);
            }
        }
    }

    pub(super) fn update_label_expressions(&self) {
        if let (Some(label_expressions), Some(label_numbers)) = (
            self.maybe_label_expressions().as_ref(),
            self.maybe_label_numbers().as_ref(),
        ) {
            for (label_number, labels) in label_numbers {
                for label in labels {
                    let expressions = label_expressions.get(label);
                    if let Some(expressions) = expressions {
                        for expression in expressions {
                            expression
                                .as_literal_like_node()
                                .set_text(format!("{label_number}"));
                        }
                    }
                }
            }
        }
    }

    pub(super) fn try_enter_or_leave_block(&self, operation_index: usize) {
        if let Some(blocks) = self.maybe_blocks().as_ref() {
            while self.block_index() < self.block_actions().len()
                && self.block_offsets()[self.block_index()] <= operation_index
            {
                let block = blocks[self.block_index()].clone();
                let block_action = self.block_actions()[self.block_index()];
                match (*block).borrow().kind() {
                    CodeBlockKind::Exception => match block_action {
                        BlockAction::Open => {
                            self.maybe_statements_mut().get_or_insert_default_();
                            self.maybe_exception_block_stack_mut()
                                .get_or_insert_default_()
                                .push(self.current_exception_block());
                            self.set_current_exception_block(Some(block.clone()));
                        }
                        BlockAction::Close => {
                            self.set_current_exception_block(
                                self.exception_block_stack_mut().pop(),
                            );
                        }
                    },
                    CodeBlockKind::With => match block_action {
                        BlockAction::Open => {
                            self.maybe_with_block_stack_mut()
                                .get_or_insert_default_()
                                .push(block.clone());
                        }
                        BlockAction::Close => {
                            self.with_block_stack_mut().pop();
                        }
                    },
                    _ => (),
                }
                self.set_block_index(self.block_index() + 1);
            }
        }
    }

    pub(super) fn write_operation(&self, operation_index: usize) {
        self.try_enter_label(operation_index);
        self.try_enter_or_leave_block(operation_index);

        if self.last_operation_was_abrupt() {
            return;
        }

        self.set_last_operation_was_abrupt(false);
        self.set_last_operation_was_completion(false);

        let opcode = self.operations()[operation_index];
        if opcode == OpCode::Nop {
            return;
        } else if opcode == OpCode::Endfinally {
            return self.write_end_finally();
        }

        let args = self.operation_arguments()[operation_index].clone().unwrap();
        if opcode == OpCode::Statement {
            return self.write_statement(args.as_node());
        }

        let location = self.operation_locations()[operation_index];
        match opcode {
            OpCode::Assign => {
                let (args_0, args_1) = args.as_node_and_node();
                self.write_assign(args_0, args_1, location.as_ref());
            }
            OpCode::Break => self.write_break(args.as_label(), location.as_ref()),
            OpCode::BreakWhenTrue => {
                let (args_0, args_1) = args.as_label_and_node();
                self.write_break_when_true(args_0, args_1, location.as_ref());
            }
            OpCode::BreakWhenFalse => {
                let (args_0, args_1) = args.as_label_and_node();
                self.write_break_when_false(args_0, args_1, location.as_ref());
            }
            OpCode::Yield => self.write_yield(args.as_node(), location.as_ref()),
            OpCode::YieldStar => self.write_yield_star(args.as_node(), location.as_ref()),
            OpCode::Return => self.write_return(Some(args.as_node()), location.as_ref()),
            OpCode::Throw => self.write_throw(args.as_node(), location.as_ref()),
            _ => (),
        }
    }

    pub(super) fn write_statement(&self, statement: Id<Node /*Statement*/>) {
        // if (statement) {
        self.maybe_statements_mut()
            .get_or_insert_default_()
            .push(statement);
        // }
    }

    pub(super) fn write_assign(
        &self,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
        operation_location: Option<&impl ReadonlyTextRange>,
    ) {
        self.write_statement(
            self.factory
                .create_expression_statement(self.factory.create_assignment(left, right))
                .set_text_range(operation_location),
        );
    }
}
