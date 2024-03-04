use id_arena::Id;

use super::{
    get_instruction_name, BlockAction, CodeBlock, CodeBlockKind, ExceptionBlock,
    ExceptionBlockState, Instruction, Label, LabeledBlock, LoopBlock, SwitchBlock,
    TransformGenerators, WithBlock,
};
use crate::{
    Debug_, Node, ReadonlyTextRange, _d, get_original_node_id, id_text, is_generated_identifier,
    last_or_undefined, CoreTransformationContext, GetOrInsertDefault, HasArena, InArena,
    NamedDeclarationInterface, NodeArray, NodeExt, NonEmpty, Number, SyntaxKind,
    TransformationContext,
};

impl TransformGenerators {
    pub(super) fn define_label(&self) -> Label {
        let label = self.next_label_id();
        self.set_next_label_id(self.next_label_id() + 1);
        self.maybe_label_offsets_mut()
            .get_or_insert_default_()
            .insert(label, None);
        label
    }

    pub(super) fn mark_label(&self, label: Label) {
        let mut label_offsets = self.maybe_label_offsets_mut();
        Debug_.assert(label_offsets.is_some(), Some("No labels were defined."));
        let label_offsets = label_offsets.as_mut().unwrap();
        label_offsets.insert(
            label,
            Some(
                self.maybe_operations()
                    .as_ref()
                    .map_or(0, |operations| operations.len()),
            ),
        );
    }

    pub(super) fn begin_block(&self, block: Id<CodeBlock>) -> usize {
        if self.maybe_blocks().is_none() {
            self.set_blocks(Some(_d()));
            self.set_block_actions(Some(_d()));
            self.set_block_offsets(Some(_d()));
            self.set_block_stack(Some(_d()));
        }

        let index = self.block_actions().len();
        self.block_actions_mut().push(BlockAction::Open);
        self.block_offsets_mut().push(
            self.maybe_operations()
                .as_ref()
                .map_or(0, |operations| operations.len()),
        );
        self.blocks_mut().push(block.clone());
        self.block_stack_mut().push(block);
        index
    }

    pub(super) fn end_block(&self) -> Id<CodeBlock> {
        let block = self.peek_block();
        if block.is_none() {
            Debug_.fail(Some("beginBlock was never called."));
        }
        let block = block.unwrap();

        // const index = blockActions!.length;
        self.block_actions_mut().push(BlockAction::Close);
        self.block_offsets_mut().push(
            self.maybe_operations()
                .as_ref()
                .map_or(0, |operations| operations.len()),
        );
        self.blocks_mut().push(block.clone());
        self.block_stack_mut().pop();
        block
    }

    pub(super) fn peek_block(&self) -> Option<Id<CodeBlock>> {
        last_or_undefined(&self.block_stack()).cloned()
    }

    pub(super) fn peek_block_kind(&self) -> Option<CodeBlockKind> {
        self.peek_block().map(|block| block.ref_(self).kind())
    }

    pub(super) fn begin_with_block(&self, expression: Id<Node /*Identifier*/>) {
        let start_label = self.define_label();
        let end_label = self.define_label();
        self.mark_label(start_label);
        self.begin_block(
            self.alloc_code_block(WithBlock::new(expression, start_label, end_label).into()),
        );
    }

    pub(super) fn end_with_block(&self) {
        Debug_.assert(self.peek_block_kind() == Some(CodeBlockKind::With), None);
        let block = self.end_block();
        self.mark_label(block.ref_(self).as_with_block().end_label);
    }

    pub(super) fn begin_exception_block(&self) -> Label {
        let start_label = self.define_label();
        let end_label = self.define_label();
        self.mark_label(start_label);
        self.begin_block(
            self.alloc_code_block(
                ExceptionBlock::new(
                    ExceptionBlockState::Try,
                    start_label,
                    None,
                    None,
                    None,
                    end_label,
                )
                .into(),
            ),
        );
        self.emit_nop();
        end_label
    }

    pub(super) fn begin_catch_block(&self, variable: Id<Node> /*VariableDeclaration*/) {
        Debug_.assert(
            self.peek_block_kind() == Some(CodeBlockKind::Exception),
            None,
        );

        let name: Id<Node /*Identifier*/>;
        let variable_name = variable.ref_(self).as_variable_declaration().name();
        if is_generated_identifier(&variable_name.ref_(self)) {
            name = variable_name.clone();
            self.context
                .ref_(self)
                .hoist_variable_declaration(variable_name);
        } else {
            let ref text = id_text(&variable_name.ref_(self)).to_owned();
            name = self.declare_local(Some(text));
            if self.maybe_renamed_catch_variables().is_none() {
                self.set_renamed_catch_variables(Some(_d()));
                self.set_renamed_catch_variable_declarations(_d());
                self.context
                    .ref_(self)
                    .enable_substitution(SyntaxKind::Identifier);
            }

            self.renamed_catch_variables_mut()
                .insert(text.to_owned(), true);
            self.renamed_catch_variable_declarations_mut()
                .insert(get_original_node_id(variable, self), name);
        }

        let exception = self.peek_block().unwrap();
        let mut exception = exception.ref_mut(self);
        Debug_.assert(
            exception.as_exception_block().state < ExceptionBlockState::Catch,
            None,
        );

        let end_label = exception.as_exception_block().end_label;
        self.emit_break(end_label, Option::<&Node>::None);

        let catch_label = self.define_label();
        self.mark_label(catch_label);
        exception.as_exception_block_mut().state = ExceptionBlockState::Catch;
        exception.as_exception_block_mut().catch_variable = Some(name.clone());
        exception.as_exception_block_mut().catch_label = Some(catch_label);

        self.emit_assignment(
            name,
            self.factory.ref_(self).create_call_expression(
                self.factory
                    .ref_(self)
                    .create_property_access_expression(self.state(), "sent"),
                Option::<Id<NodeArray>>::None,
                Some(vec![]),
            ),
            Option::<&Node>::None,
        );
        self.emit_nop();
    }

    pub(super) fn begin_finally_block(&self) {
        Debug_.assert(
            self.peek_block_kind() == Some(CodeBlockKind::Exception),
            None,
        );

        let exception = self.peek_block().unwrap();
        let mut exception = exception.ref_mut(self);
        Debug_.assert(
            exception.as_exception_block().state < ExceptionBlockState::Finally,
            None,
        );

        let end_label = exception.as_exception_block().end_label;
        self.emit_break(end_label, Option::<&Node>::None);

        let finally_label = self.define_label();
        self.mark_label(finally_label);
        exception.as_exception_block_mut().state = ExceptionBlockState::Finally;
        exception.as_exception_block_mut().finally_label = Some(finally_label);
    }

    pub(super) fn end_exception_block(&self) {
        Debug_.assert(
            self.peek_block_kind() == Some(CodeBlockKind::Exception),
            None,
        );
        let exception = self.end_block();
        let mut exception = exception.ref_mut(self);
        let state = exception.as_exception_block().state;
        if state < ExceptionBlockState::Finally {
            self.emit_break(
                exception.as_exception_block().end_label,
                Option::<&Node>::None,
            );
        } else {
            self.emit_end_finally();
        }

        self.mark_label(exception.as_exception_block().end_label);
        self.emit_nop();
        exception.as_exception_block_mut().state = ExceptionBlockState::Done;
    }

    pub(super) fn begin_script_loop_block(&self) {
        self.begin_block(self.alloc_code_block(LoopBlock::new(-1, true, -1).into()));
    }

    pub(super) fn begin_loop_block(&self, continue_label: Label) -> Label {
        let break_label = self.define_label();
        self.begin_block(
            self.alloc_code_block(LoopBlock::new(continue_label, false, break_label).into()),
        );
        break_label
    }

    pub(super) fn end_loop_block(&self) {
        Debug_.assert(self.peek_block_kind() == Some(CodeBlockKind::Loop), None);
        let block = self.end_block();
        let block = block.ref_(self);
        let block_as_switch_block = block.as_switch_block();
        let break_label = block_as_switch_block.break_label;
        if !block_as_switch_block.is_script {
            self.mark_label(break_label);
        }
    }

    pub(super) fn begin_script_switch_block(&self) {
        self.begin_block(self.alloc_code_block(SwitchBlock::new(true, -1).into()));
    }

    pub(super) fn begin_switch_block(&self) -> Label {
        let break_label = self.define_label();
        self.begin_block(self.alloc_code_block(SwitchBlock::new(false, break_label).into()));
        break_label
    }

    pub(super) fn end_switch_block(&self) {
        Debug_.assert(self.peek_block_kind() == Some(CodeBlockKind::Switch), None);
        let block = self.end_block();
        let block = block.ref_(self);
        let block_as_switch_block = block.as_switch_block();
        let break_label = block_as_switch_block.break_label;
        if !block_as_switch_block.is_script {
            self.mark_label(break_label);
        }
    }

    pub(super) fn begin_script_labeled_block(&self, label_text: String) {
        self.begin_block(self.alloc_code_block(LabeledBlock::new(label_text, true, -1).into()));
    }

    pub(super) fn begin_labeled_block(&self, label_text: String) {
        let break_label = self.define_label();
        self.begin_block(
            self.alloc_code_block(LabeledBlock::new(label_text, false, break_label).into()),
        );
    }

    pub(super) fn end_labeled_block(&self) {
        Debug_.assert(self.peek_block_kind() == Some(CodeBlockKind::Labeled), None);
        let block = self.end_block();
        let block = block.ref_(self);
        let block_as_labeled_block = block.as_labeled_block();
        if !block_as_labeled_block.is_script {
            self.mark_label(block_as_labeled_block.break_label);
        }
    }

    pub(super) fn supports_unlabeled_break(&self, block: &CodeBlock) -> bool {
        matches!(block.kind(), CodeBlockKind::Switch | CodeBlockKind::Loop)
    }

    pub(super) fn supports_labeled_break_or_continue(&self, block: &CodeBlock) -> bool {
        block.kind() == CodeBlockKind::Labeled
    }

    pub(super) fn supports_unlabeled_continue(&self, block: &CodeBlock) -> bool {
        block.kind() == CodeBlockKind::Loop
    }

    pub(super) fn has_immediate_containing_labeled_block(
        &self,
        label_text: &str,
        start: usize,
    ) -> bool {
        let mut j = start;
        loop {
            let containing_block = self.block_stack()[j].clone();
            let containing_block = containing_block.ref_(self);
            if self.supports_labeled_break_or_continue(&containing_block) {
                if containing_block.as_labeled_block().label_text == label_text {
                    return true;
                }
            } else {
                break;
            }
            if j == 0 {
                break;
            }
            j -= 1;
        }

        false
    }

    pub(super) fn find_break_target(&self, label_text: Option<&str>) -> Label {
        if let Some(block_stack) = self.maybe_block_stack().as_ref() {
            if let Some(label_text) = label_text.non_empty() {
                let mut i = block_stack.len() - 1;
                loop {
                    let block = block_stack[i].clone();
                    let block = block.ref_(self);
                    #[allow(clippy::if_same_then_else)]
                    if self.supports_labeled_break_or_continue(&block)
                        && block.as_labeled_block().label_text == label_text
                    {
                        return block.break_label();
                    } else if self.supports_unlabeled_break(&block)
                        && self.has_immediate_containing_labeled_block(label_text, i - 1)
                    {
                        return block.break_label();
                    }
                    if i == 0 {
                        break;
                    }
                    i -= 1;
                }
            } else {
                let mut i = block_stack.len() - 1;
                loop {
                    let block = block_stack[i].clone();
                    let block = block.ref_(self);
                    if self.supports_unlabeled_break(&block) {
                        return block.break_label();
                    }
                    if i == 0 {
                        break;
                    }
                    i -= 1;
                }
            }
        }
        0
    }

    pub(super) fn find_continue_target(&self, label_text: Option<&str>) -> Label {
        if let Some(block_stack) = self.maybe_block_stack().as_ref() {
            if let Some(label_text) = label_text.non_empty() {
                let mut i = block_stack.len() - 1;
                loop {
                    let block = block_stack[i].clone();
                    let block = block.ref_(self);
                    if self.supports_unlabeled_continue(&block)
                        && self.has_immediate_containing_labeled_block(label_text, i - 1)
                    {
                        return block.as_loop_block().continue_label;
                    }
                    if i == 0 {
                        break;
                    }
                    i -= 1;
                }
            } else {
                let mut i = block_stack.len() - 1;
                loop {
                    let block = block_stack[i].clone();
                    let block = block.ref_(self);
                    if self.supports_unlabeled_continue(&block) {
                        return block.as_loop_block().continue_label;
                    }
                    if i == 0 {
                        break;
                    }
                    i -= 1;
                }
            }
        }
        0
    }

    pub(super) fn create_label(&self, label: Option<Label>) -> Id<Node /*Expression*/> {
        if let Some(label) = label.filter(|&label| label > 0) {
            let mut label_expressions = self.maybe_label_expressions_mut();
            let label_expressions = label_expressions.get_or_insert_default_();

            let expression = self
                .factory
                .ref_(self)
                .create_numeric_literal(Number::new(-1.0), None);
            label_expressions
                .entry(label)
                .or_default()
                .push(expression.clone());

            return expression;
        }

        self.factory.ref_(self).create_omitted_expression()
    }

    pub(super) fn create_instruction(
        &self,
        instruction: Instruction,
    ) -> Id<Node /*NumericLiteral*/> {
        self.factory
            .ref_(self)
            .create_numeric_literal(Number::new(instruction as u8 as f64), None)
            .add_synthetic_trailing_comment(
                SyntaxKind::MultiLineCommentTrivia,
                get_instruction_name(instruction).unwrap(),
                None,
                self,
            )
    }

    pub(super) fn create_inline_break(
        &self,
        label: Label,
        location: Option<&impl ReadonlyTextRange>,
    ) -> Id<Node /*ReturnStatement*/> {
        Debug_.assert_less_than(0, label, Some("Invalid label"));
        self.factory
            .ref_(self)
            .create_return_statement(Some(
                self.factory.ref_(self).create_array_literal_expression(
                    Some(vec![
                        self.create_instruction(Instruction::Break),
                        self.create_label(Some(label)),
                    ]),
                    None,
                ),
            ))
            .set_text_range(location, self)
    }

    pub(super) fn create_inline_return(
        &self,
        expression: Option<Id<Node>>,
        location: Option<&impl ReadonlyTextRange>,
    ) -> Id<Node /*ReturnStatement*/> {
        self.factory
            .ref_(self)
            .create_return_statement(Some(
                self.factory.ref_(self).create_array_literal_expression(
                    Some(if let Some(expression) = expression {
                        vec![self.create_instruction(Instruction::Return), expression]
                    } else {
                        vec![self.create_instruction(Instruction::Return)]
                    }),
                    None,
                ),
            ))
            .set_text_range(location, self)
    }

    pub(super) fn create_generator_resume(
        &self,
        location: Option<&impl ReadonlyTextRange>,
    ) -> Id<Node /*LeftHandSideExpression*/> {
        self.factory
            .ref_(self)
            .create_call_expression(
                self.factory
                    .ref_(self)
                    .create_property_access_expression(self.state(), "sent"),
                Option::<Id<NodeArray>>::None,
                Some(vec![]),
            )
            .set_text_range(location, self)
    }
}
