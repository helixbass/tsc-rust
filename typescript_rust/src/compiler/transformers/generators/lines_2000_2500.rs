use gc::{Gc, GcCell};

use super::{
    BlockAction, CodeBlock, CodeBlockKind, ExceptionBlock, ExceptionBlockState, Label,
    TransformGenerators, WithBlock,
};
use crate::{
    Debug_, Node, ReadonlyTextRange, _d, get_original_node_id, id_text, is_generated_identifier,
    last_or_undefined, NamedDeclarationInterface, NodeArray, NodeInterface, SyntaxKind,
};

impl TransformGenerators {
    pub(super) fn define_label(&self) -> Label {
        let label = self.next_label_id();
        self.set_next_label_id(self.next_label_id() + 1);
        self.maybe_label_offsets_mut()
            .get_or_insert_with(|| _d())
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

    pub(super) fn begin_block(&self, block: Gc<GcCell<CodeBlock>>) -> usize {
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

    pub(super) fn end_block(&self) -> Gc<GcCell<CodeBlock>> {
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

    pub(super) fn peek_block(&self) -> Option<Gc<GcCell<CodeBlock>>> {
        last_or_undefined(&self.block_stack()).cloned()
    }

    pub(super) fn peek_block_kind(&self) -> Option<CodeBlockKind> {
        self.peek_block().map(|block| (*block).borrow().kind())
    }

    pub(super) fn begin_with_block(&self, expression: Gc<Node /*Identifier*/>) {
        let start_label = self.define_label();
        let end_label = self.define_label();
        self.mark_label(start_label);
        self.begin_block(WithBlock::new(expression, start_label, end_label).into());
    }

    pub(super) fn end_with_block(&self) {
        Debug_.assert(self.peek_block_kind() == Some(CodeBlockKind::With), None);
        let block = self.end_block();
        self.mark_label((*block).borrow().as_with_block().end_label);
    }

    pub(super) fn begin_exception_block(&self) -> Label {
        let start_label = self.define_label();
        let end_label = self.define_label();
        self.mark_label(start_label);
        self.begin_block(
            ExceptionBlock::new(
                ExceptionBlockState::Try,
                start_label,
                None,
                None,
                None,
                end_label,
            )
            .into(),
        );
        self.emit_nop();
        end_label
    }

    pub(super) fn begin_catch_block(&self, variable: &Node /*VariableDeclaration*/) {
        let variable_as_variable_declaration = variable.as_variable_declaration();
        Debug_.assert(
            self.peek_block_kind() == Some(CodeBlockKind::Exception),
            None,
        );

        let name: Gc<Node /*Identifier*/>;
        let ref variable_name = variable_as_variable_declaration.name();
        if is_generated_identifier(variable_name) {
            name = variable_name.clone();
            self.context.hoist_variable_declaration(variable_name);
        } else {
            let text = id_text(variable_name);
            name = self.declare_local(Some(text));
            if self.maybe_renamed_catch_variables().is_none() {
                self.set_renamed_catch_variables(Some(_d()));
                self.set_renamed_catch_variable_declarations(_d());
                self.context.enable_substitution(SyntaxKind::Identifier);
            }

            self.renamed_catch_variables_mut()
                .insert(text.to_owned(), true);
            self.renamed_catch_variable_declarations_mut()
                .insert(get_original_node_id(variable), name.clone());
        }

        let exception = self.peek_block().unwrap();
        let mut exception = exception.borrow_mut();
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
            self.factory
                .create_call_expression(
                    self.factory
                        .create_property_access_expression(self.state(), "sent")
                        .wrap(),
                    Option::<Gc<NodeArray>>::None,
                    Some(vec![]),
                )
                .wrap(),
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
        let mut exception = exception.borrow_mut();
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
        let mut exception = exception.borrow_mut();
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
        unimplemented!()
    }

    pub(super) fn begin_loop_block(&self, _continue_label: Label) -> Label {
        unimplemented!()
    }

    pub(super) fn end_loop_block(&self) {
        unimplemented!()
    }

    pub(super) fn begin_script_switch_block(&self) {
        unimplemented!()
    }

    pub(super) fn begin_switch_block(&self) -> Label {
        unimplemented!()
    }

    pub(super) fn end_switch_block(&self) {
        unimplemented!()
    }

    pub(super) fn begin_script_labeled_block(&self, _label_text: &str) {
        unimplemented!()
    }

    pub(super) fn begin_labeled_block(&self, _label_text: &str) {
        unimplemented!()
    }

    pub(super) fn end_labeled_block(&self) {
        unimplemented!()
    }

    pub(super) fn find_break_target(&self, _label_text: Option<&str>) -> Label {
        unimplemented!()
    }

    pub(super) fn find_continue_target(&self, _label_text: Option<&str>) -> Label {
        unimplemented!()
    }

    pub(super) fn create_inline_break(
        &self,
        _label: Label,
        _location: Option<&impl ReadonlyTextRange>,
    ) -> Gc<Node /*ReturnStatement*/> {
        unimplemented!()
    }

    pub(super) fn create_inline_return(
        &self,
        _expression: Option<Gc<Node>>,
        _location: Option<&impl ReadonlyTextRange>,
    ) -> Gc<Node /*ReturnStatement*/> {
        unimplemented!()
    }

    pub(super) fn create_generator_resume(
        &self,
        _location: Option<&impl ReadonlyTextRange>,
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        unimplemented!()
    }
}
