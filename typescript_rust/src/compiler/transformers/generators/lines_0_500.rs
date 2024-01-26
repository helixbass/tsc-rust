use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    collections::HashMap,
    io, mem, any::Any,
};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use id_arena::Id;
use local_macros::enum_unwrapped;

use crate::{
    BaseNodeFactorySynthetic, CompilerOptions, EmitHelperFactory, EmitHint, EmitResolver, Node,
    NodeFactory, NodeId, ReadonlyTextRangeConcrete, TransformationContext,
    TransformationContextOnSubstituteNodeOverrider, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface, _d, chain_bundle, gc_cell_ref_mut_unwrapped,
    gc_cell_ref_unwrapped, get_emit_script_target, get_original_node, get_original_node_id,
    id_text, is_function_like_declaration, is_generated_identifier, is_identifier,
    ref_mut_unwrapped, ref_unwrapped, visit_each_child, visit_parameter_list, Debug_,
    FunctionLikeDeclarationInterface, Matches, NamedDeclarationInterface, NodeArray, NodeExt,
    NodeInterface, ScriptTarget, SignatureDeclarationInterface, SyntaxKind, TransformFlags,
    VisitResult,
    HasArena, AllArenas, InArena, static_arena, downcast_transformer_ref,
    TransformNodesTransformationResult, CoreTransformationContext,
};

pub(super) type Label = i32;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(super) enum OpCode {
    Nop,
    Statement,
    Assign,
    Break,
    BreakWhenTrue,
    BreakWhenFalse,
    Yield,
    YieldStar,
    Return,
    Throw,
    Endfinally,
}

#[derive(Clone, Trace, Finalize)]
pub(super) enum OperationArguments {
    Label(Label),
    LabelAndNode((Label, Id<Node /*Expression*/>)),
    Node(Id<Node /*Statement*/>),
    OptionNode(Option<Id<Node /*Expression*/>>),
    NodeAndNode((Id<Node /*Expression*/>, Id<Node /*Expression*/>)),
}

impl OperationArguments {
    pub fn as_label(&self) -> Label {
        *enum_unwrapped!(self, [OperationArguments, Label])
    }

    pub fn as_label_and_node(&self) -> (Label, Id<Node>) {
        enum_unwrapped!(self, [OperationArguments, LabelAndNode]).clone()
    }

    pub fn as_node(&self) -> Id<Node> {
        enum_unwrapped!(self, [OperationArguments, Node]).clone()
    }

    #[allow(dead_code)]
    pub fn as_option_node(&self) -> Option<Id<Node>> {
        enum_unwrapped!(self, [OperationArguments, OptionNode]).clone()
    }

    pub fn as_node_and_node(&self) -> (Id<Node>, Id<Node>) {
        enum_unwrapped!(self, [OperationArguments, NodeAndNode]).clone()
    }
}

impl From<Label> for OperationArguments {
    fn from(value: Label) -> Self {
        Self::Label(value)
    }
}

impl From<(Label, Id<Node /*Expression*/>)> for OperationArguments {
    fn from(value: (Label, Id<Node /*Expression*/>)) -> Self {
        Self::LabelAndNode(value)
    }
}

impl From<Id<Node /*Statement*/>> for OperationArguments {
    fn from(value: Id<Node /*Statement*/>) -> Self {
        Self::Node(value)
    }
}

impl From<Option<Id<Node /*Expression*/>>> for OperationArguments {
    fn from(value: Option<Id<Node /*Expression*/>>) -> Self {
        Self::OptionNode(value)
    }
}

impl From<(Id<Node /*Expression*/>, Id<Node /*Expression*/>)> for OperationArguments {
    fn from(value: (Id<Node /*Expression*/>, Id<Node /*Expression*/>)) -> Self {
        Self::NodeAndNode(value)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(super) enum BlockAction {
    Open,
    Close,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(super) enum CodeBlockKind {
    Exception,
    With,
    Switch,
    Loop,
    Labeled,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub(super) enum ExceptionBlockState {
    Try,
    Catch,
    Finally,
    Done,
}

#[derive(Trace, Finalize)]
pub(super) enum CodeBlock {
    ExceptionBlock(ExceptionBlock),
    LabeledBlock(LabeledBlock),
    SwitchBlock(SwitchBlock),
    LoopBlock(LoopBlock),
    WithBlock(WithBlock),
}

impl CodeBlock {
    pub fn kind(&self) -> CodeBlockKind {
        match self {
            Self::ExceptionBlock(value) => value.kind,
            Self::LabeledBlock(value) => value.kind,
            Self::SwitchBlock(value) => value.kind,
            Self::LoopBlock(value) => value.kind,
            Self::WithBlock(value) => value.kind,
        }
    }

    pub fn break_label(&self) -> Label {
        match self {
            Self::ExceptionBlock(_) => unreachable!(),
            Self::LabeledBlock(value) => value.break_label,
            Self::SwitchBlock(value) => value.break_label,
            Self::LoopBlock(value) => value.break_label,
            Self::WithBlock(_) => unreachable!(),
        }
    }

    pub(super) fn as_exception_block(&self) -> &ExceptionBlock {
        enum_unwrapped!(self, [CodeBlock, ExceptionBlock])
    }

    pub(super) fn as_exception_block_mut(&mut self) -> &mut ExceptionBlock {
        enum_unwrapped!(self, [CodeBlock, ExceptionBlock])
    }

    pub(super) fn as_labeled_block(&self) -> &LabeledBlock {
        enum_unwrapped!(self, [CodeBlock, LabeledBlock])
    }

    pub(super) fn as_switch_block(&self) -> &SwitchBlock {
        enum_unwrapped!(self, [CodeBlock, SwitchBlock])
    }

    pub(super) fn as_loop_block(&self) -> &LoopBlock {
        enum_unwrapped!(self, [CodeBlock, LoopBlock])
    }

    pub(super) fn as_with_block(&self) -> &WithBlock {
        enum_unwrapped!(self, [CodeBlock, WithBlock])
    }
}

impl From<ExceptionBlock> for CodeBlock {
    fn from(value: ExceptionBlock) -> Self {
        Self::ExceptionBlock(value)
    }
}

impl From<ExceptionBlock> for Gc<GcCell<CodeBlock>> {
    fn from(value: ExceptionBlock) -> Self {
        Gc::new(GcCell::new(value.into()))
    }
}

impl From<LabeledBlock> for CodeBlock {
    fn from(value: LabeledBlock) -> Self {
        Self::LabeledBlock(value)
    }
}

impl From<LabeledBlock> for Gc<GcCell<CodeBlock>> {
    fn from(value: LabeledBlock) -> Self {
        Gc::new(GcCell::new(value.into()))
    }
}

impl From<SwitchBlock> for CodeBlock {
    fn from(value: SwitchBlock) -> Self {
        Self::SwitchBlock(value)
    }
}

impl From<SwitchBlock> for Gc<GcCell<CodeBlock>> {
    fn from(value: SwitchBlock) -> Self {
        Gc::new(GcCell::new(value.into()))
    }
}

impl From<LoopBlock> for CodeBlock {
    fn from(value: LoopBlock) -> Self {
        Self::LoopBlock(value)
    }
}

impl From<LoopBlock> for Gc<GcCell<CodeBlock>> {
    fn from(value: LoopBlock) -> Self {
        Gc::new(GcCell::new(value.into()))
    }
}

impl From<WithBlock> for CodeBlock {
    fn from(value: WithBlock) -> Self {
        Self::WithBlock(value)
    }
}

impl From<WithBlock> for Gc<GcCell<CodeBlock>> {
    fn from(value: WithBlock) -> Self {
        Gc::new(GcCell::new(value.into()))
    }
}

#[derive(Trace, Finalize)]
pub(super) struct ExceptionBlock {
    #[unsafe_ignore_trace]
    pub kind: CodeBlockKind, /*CodeBlockKind.Exception*/
    #[unsafe_ignore_trace]
    pub state: ExceptionBlockState,
    pub start_label: Label,
    pub catch_variable: Option<Id<Node /*Identifier*/>>,
    pub catch_label: Option<Label>,
    pub finally_label: Option<Label>,
    pub end_label: Label,
}

impl ExceptionBlock {
    pub(super) fn new(
        state: ExceptionBlockState,
        start_label: Label,
        catch_variable: Option<Id<Node /*Identifier*/>>,
        catch_label: Option<Label>,
        finally_label: Option<Label>,
        end_label: Label,
    ) -> Self {
        Self {
            kind: CodeBlockKind::Exception,
            state,
            start_label,
            catch_variable,
            catch_label,
            finally_label,
            end_label,
        }
    }
}

#[derive(Trace, Finalize)]
pub(super) struct LabeledBlock {
    #[unsafe_ignore_trace]
    pub kind: CodeBlockKind, /*CodeBlockKind.Labeled*/
    pub label_text: String,
    pub is_script: bool,
    pub break_label: Label,
}

impl LabeledBlock {
    pub(super) fn new(label_text: String, is_script: bool, break_label: Label) -> Self {
        Self {
            kind: CodeBlockKind::Labeled,
            label_text,
            is_script,
            break_label,
        }
    }
}

#[derive(Trace, Finalize)]
pub(super) struct SwitchBlock {
    #[unsafe_ignore_trace]
    pub kind: CodeBlockKind, /*CodeBlockKind.Switch*/
    pub is_script: bool,
    pub break_label: Label,
}

impl SwitchBlock {
    pub(super) fn new(is_script: bool, break_label: Label) -> Self {
        Self {
            kind: CodeBlockKind::Switch,
            is_script,
            break_label,
        }
    }
}

#[derive(Trace, Finalize)]
pub(super) struct LoopBlock {
    #[unsafe_ignore_trace]
    pub kind: CodeBlockKind, /*CodeBlockKind.Loop*/
    pub continue_label: Label,
    pub is_script: bool,
    pub break_label: Label,
}

impl LoopBlock {
    pub(super) fn new(continue_label: Label, is_script: bool, break_label: Label) -> Self {
        Self {
            kind: CodeBlockKind::Loop,
            continue_label,
            is_script,
            break_label,
        }
    }
}

#[derive(Trace, Finalize)]
pub(super) struct WithBlock {
    #[unsafe_ignore_trace]
    pub kind: CodeBlockKind, /*CodeBlockKind.With*/
    pub expression: Id<Node /*Identifier*/>,
    pub start_label: Label,
    pub end_label: Label,
}

impl WithBlock {
    pub fn new(expression: Id<Node /*Identifier*/>, start_label: Label, end_label: Label) -> Self {
        Self {
            kind: CodeBlockKind::With,
            expression,
            start_label,
            end_label,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(super) enum Instruction {
    #[allow(dead_code)]
    Next = 0,
    #[allow(dead_code)]
    Throw = 1,
    Return = 2,
    Break = 3,
    Yield = 4,
    YieldStar = 5,
    #[allow(dead_code)]
    Catch = 6,
    Endfinally = 7,
}

pub(super) fn get_instruction_name(instruction: Instruction) -> Option<&'static str> {
    match instruction {
        Instruction::Return => Some("return"),
        Instruction::Break => Some("break"),
        Instruction::Yield => Some("yield"),
        Instruction::YieldStar => Some("yield"),
        Instruction::Endfinally => Some("endfinally"),
        _ => None,
    }
}

#[derive(Trace, Finalize)]
pub(super) struct TransformGenerators {
    #[unsafe_ignore_trace]
    pub(super) _arena: *const AllArenas,
    pub(super) context: Id<TransformNodesTransformationResult>,
    pub(super) factory: Id<NodeFactory>,
    pub(super) compiler_options: Id<CompilerOptions>,
    #[unsafe_ignore_trace]
    pub(super) language_version: ScriptTarget,
    pub(super) resolver: Id<Box<dyn EmitResolver>>,
    pub(super) renamed_catch_variables: GcCell<Option<HashMap<String, bool>>>,
    pub(super) renamed_catch_variable_declarations:
        GcCell<HashMap<NodeId, Id<Node /*Identifier*/>>>,
    #[unsafe_ignore_trace]
    pub(super) in_generator_function_body: Cell<Option<bool>>,
    #[unsafe_ignore_trace]
    pub(super) in_statement_containing_yield: Cell<Option<bool>>,
    pub(super) blocks: GcCell<Option<Vec<Gc<GcCell<CodeBlock>>>>>,
    #[unsafe_ignore_trace]
    pub(super) block_offsets: RefCell<Option<Vec<usize>>>,
    #[unsafe_ignore_trace]
    pub(super) block_actions: RefCell<Option<Vec<BlockAction>>>,
    pub(super) block_stack: GcCell<Option<Vec<Gc<GcCell<CodeBlock>>>>>,
    #[unsafe_ignore_trace]
    pub(super) label_offsets: RefCell<Option<HashMap<Label, Option<usize>>>>,
    pub(super) label_expressions:
        GcCell<Option<HashMap<Label, Vec<Id<Node /*Mutable<LiteralExpression>*/>>>>>,
    #[unsafe_ignore_trace]
    pub(super) next_label_id: Cell<Label>,
    #[unsafe_ignore_trace]
    pub(super) operations: RefCell<Option<Vec<OpCode>>>,
    pub(super) operation_arguments: GcCell<Option<Vec<Option<OperationArguments>>>>,
    #[unsafe_ignore_trace]
    pub(super) operation_locations: RefCell<Option<Vec<Option<ReadonlyTextRangeConcrete>>>>,
    pub(super) state: GcCell<Option<Id<Node /*Identifier*/>>>,
    #[unsafe_ignore_trace]
    pub(super) block_index: Cell<usize>,
    #[unsafe_ignore_trace]
    pub(super) label_number: Cell<u32>,
    #[unsafe_ignore_trace]
    pub(super) label_numbers: RefCell<Option<HashMap<u32, Vec<Label>>>>,
    #[unsafe_ignore_trace]
    pub(super) last_operation_was_abrupt: Cell<bool>,
    #[unsafe_ignore_trace]
    pub(super) last_operation_was_completion: Cell<bool>,
    pub(super) clauses: GcCell<Option<Vec<Id<Node /*CaseClause*/>>>>,
    pub(super) statements: GcCell<Option<Vec<Id<Node /*Statement*/>>>>,
    pub(super) exception_block_stack: GcCell<Option<Vec<Gc<GcCell<CodeBlock /*ExceptionBlock*/>>>>>,
    pub(super) current_exception_block: GcCell<Option<Gc<GcCell<CodeBlock /*ExceptionBlock*/>>>>,
    pub(super) with_block_stack: GcCell<Option<Vec<Gc<GcCell<CodeBlock /*WithBlock*/>>>>>,
}

impl TransformGenerators {
    pub fn new(context: Id<TransformNodesTransformationResult>, arena: *const AllArenas) -> Transformer {
        let arena_ref = unsafe { &*arena };
        let context_ref = context.ref_(arena_ref);
        let compiler_options = context_ref.get_compiler_options();
        let ret = arena_ref.alloc_transformer(Box::new(Self {
            _arena: arena,
            factory: context_ref.factory(),
            language_version: get_emit_script_target(&compiler_options.ref_(arena_ref)),
            compiler_options,
            resolver: context_ref.get_emit_resolver(),
            context: context.clone(),
            renamed_catch_variables: _d(),
            renamed_catch_variable_declarations: _d(),
            in_generator_function_body: _d(),
            in_statement_containing_yield: _d(),
            blocks: _d(),
            block_offsets: _d(),
            block_actions: _d(),
            block_stack: _d(),
            label_offsets: _d(),
            label_expressions: _d(),
            next_label_id: Cell::new(1),
            operations: _d(),
            operation_arguments: _d(),
            operation_locations: _d(),
            state: _d(),
            block_index: _d(),
            label_number: _d(),
            label_numbers: _d(),
            last_operation_was_abrupt: _d(),
            last_operation_was_completion: _d(),
            clauses: _d(),
            statements: _d(),
            exception_block_stack: _d(),
            current_exception_block: _d(),
            with_block_stack: _d(),
        }));
        context_ref.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(TransformGeneratorsOnSubstituteNodeOverrider::new(
                ret,
                previous_on_substitute_node,
            )))
        });
        ret
    }

    pub(super) fn maybe_renamed_catch_variables(&self) -> GcCellRef<Option<HashMap<String, bool>>> {
        self.renamed_catch_variables.borrow()
    }

    pub(super) fn renamed_catch_variables_mut(
        &self,
    ) -> GcCellRefMut<Option<HashMap<String, bool>>, HashMap<String, bool>> {
        gc_cell_ref_mut_unwrapped(&self.renamed_catch_variables)
    }

    pub(super) fn set_renamed_catch_variables(
        &self,
        renamed_catch_variables: Option<HashMap<String, bool>>,
    ) {
        *self.renamed_catch_variables.borrow_mut() = renamed_catch_variables;
    }

    pub(super) fn renamed_catch_variable_declarations(
        &self,
    ) -> GcCellRef<HashMap<NodeId, Id<Node /*Identifier*/>>> {
        self.renamed_catch_variable_declarations.borrow()
    }

    pub(super) fn renamed_catch_variable_declarations_mut(
        &self,
    ) -> GcCellRefMut<HashMap<NodeId, Id<Node /*Identifier*/>>> {
        self.renamed_catch_variable_declarations.borrow_mut()
    }

    pub(super) fn set_renamed_catch_variable_declarations(
        &self,
        renamed_catch_variable_declarations: HashMap<NodeId, Id<Node /*Identifier*/>>,
    ) {
        *self.renamed_catch_variable_declarations.borrow_mut() =
            renamed_catch_variable_declarations;
    }

    pub(super) fn maybe_in_generator_function_body(&self) -> Option<bool> {
        self.in_generator_function_body.get()
    }

    pub(super) fn set_in_generator_function_body(&self, in_generator_function_body: Option<bool>) {
        self.in_generator_function_body
            .set(in_generator_function_body);
    }

    pub(super) fn maybe_in_statement_containing_yield(&self) -> Option<bool> {
        self.in_statement_containing_yield.get()
    }

    pub(super) fn set_in_statement_containing_yield(
        &self,
        in_statement_containing_yield: Option<bool>,
    ) {
        self.in_statement_containing_yield
            .set(in_statement_containing_yield);
    }

    pub(super) fn maybe_blocks(&self) -> GcCellRef<Option<Vec<Gc<GcCell<CodeBlock>>>>> {
        self.blocks.borrow()
    }

    pub(super) fn blocks_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<GcCell<CodeBlock>>>>, Vec<Gc<GcCell<CodeBlock>>>> {
        gc_cell_ref_mut_unwrapped(&self.blocks)
    }

    pub(super) fn set_blocks(&self, blocks: Option<Vec<Gc<GcCell<CodeBlock>>>>) {
        *self.blocks.borrow_mut() = blocks;
    }

    pub(super) fn maybe_block_offsets(&self) -> Ref<Option<Vec<usize>>> {
        self.block_offsets.borrow()
    }

    pub(super) fn block_offsets(&self) -> Ref<Vec<usize>> {
        ref_unwrapped(&self.block_offsets)
    }

    pub(super) fn block_offsets_mut(&self) -> RefMut<Vec<usize>> {
        ref_mut_unwrapped(&self.block_offsets)
    }

    pub(super) fn set_block_offsets(&self, block_offsets: Option<Vec<usize>>) {
        *self.block_offsets.borrow_mut() = block_offsets;
    }

    pub(super) fn maybe_block_actions(&self) -> Ref<Option<Vec<BlockAction>>> {
        self.block_actions.borrow()
    }

    pub(super) fn block_actions(&self) -> Ref<Vec<BlockAction>> {
        ref_unwrapped(&self.block_actions)
    }

    pub(super) fn block_actions_mut(&self) -> RefMut<Vec<BlockAction>> {
        ref_mut_unwrapped(&self.block_actions)
    }

    pub(super) fn set_block_actions(&self, block_actions: Option<Vec<BlockAction>>) {
        *self.block_actions.borrow_mut() = block_actions;
    }

    pub(super) fn maybe_block_stack(&self) -> GcCellRef<Option<Vec<Gc<GcCell<CodeBlock>>>>> {
        self.block_stack.borrow()
    }

    pub(super) fn block_stack(&self) -> GcCellRef<Vec<Gc<GcCell<CodeBlock>>>> {
        gc_cell_ref_unwrapped(&self.block_stack)
    }

    pub(super) fn block_stack_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<GcCell<CodeBlock>>>>, Vec<Gc<GcCell<CodeBlock>>>> {
        gc_cell_ref_mut_unwrapped(&self.block_stack)
    }

    pub(super) fn set_block_stack(&self, block_stack: Option<Vec<Gc<GcCell<CodeBlock>>>>) {
        *self.block_stack.borrow_mut() = block_stack;
    }

    pub(super) fn maybe_label_offsets(&self) -> Ref<Option<HashMap<Label, Option<usize>>>> {
        self.label_offsets.borrow()
    }

    pub(super) fn label_offsets(&self) -> Ref<HashMap<Label, Option<usize>>> {
        ref_unwrapped(&self.label_offsets)
    }

    pub(super) fn maybe_label_offsets_mut(&self) -> RefMut<Option<HashMap<Label, Option<usize>>>> {
        self.label_offsets.borrow_mut()
    }

    pub(super) fn set_label_offsets(&self, label_offsets: Option<HashMap<Label, Option<usize>>>) {
        *self.label_offsets.borrow_mut() = label_offsets;
    }

    pub(super) fn maybe_label_expressions(
        &self,
    ) -> GcCellRef<Option<HashMap<Label, Vec<Id<Node /*Mutable<LiteralExpression>*/>>>>> {
        self.label_expressions.borrow()
    }

    pub(super) fn label_expressions(
        &self,
    ) -> GcCellRef<HashMap<Label, Vec<Id<Node /*Mutable<LiteralExpression>*/>>>> {
        gc_cell_ref_unwrapped(&self.label_expressions)
    }

    pub(super) fn maybe_label_expressions_mut(
        &self,
    ) -> GcCellRefMut<Option<HashMap<Label, Vec<Id<Node /*Mutable<LiteralExpression>*/>>>>> {
        self.label_expressions.borrow_mut()
    }

    pub(super) fn set_label_expressions(
        &self,
        label_expressions: Option<HashMap<Label, Vec<Id<Node /*Mutable<LiteralExpression>*/>>>>,
    ) {
        *self.label_expressions.borrow_mut() = label_expressions;
    }

    pub(super) fn next_label_id(&self) -> Label {
        self.next_label_id.get()
    }

    pub(super) fn set_next_label_id(&self, next_label_id: Label) {
        self.next_label_id.set(next_label_id);
    }

    pub(super) fn maybe_operations(&self) -> Ref<Option<Vec<OpCode>>> {
        self.operations.borrow()
    }

    pub(super) fn operations(&self) -> Ref<Vec<OpCode>> {
        ref_unwrapped(&self.operations)
    }

    pub(super) fn operations_mut(&self) -> RefMut<Vec<OpCode>> {
        ref_mut_unwrapped(&self.operations)
    }

    pub(super) fn set_operations(&self, operations: Option<Vec<OpCode>>) {
        *self.operations.borrow_mut() = operations;
    }

    pub(super) fn maybe_operation_arguments(
        &self,
    ) -> GcCellRef<Option<Vec<Option<OperationArguments>>>> {
        self.operation_arguments.borrow()
    }

    pub(super) fn operation_arguments(&self) -> GcCellRef<Vec<Option<OperationArguments>>> {
        gc_cell_ref_unwrapped(&self.operation_arguments)
    }

    pub(super) fn operation_arguments_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Option<OperationArguments>>>, Vec<Option<OperationArguments>>>
    {
        gc_cell_ref_mut_unwrapped(&self.operation_arguments)
    }

    pub(super) fn set_operation_arguments(
        &self,
        operation_arguments: Option<Vec<Option<OperationArguments>>>,
    ) {
        *self.operation_arguments.borrow_mut() = operation_arguments;
    }

    pub(super) fn maybe_operation_locations(
        &self,
    ) -> Ref<Option<Vec<Option<ReadonlyTextRangeConcrete>>>> {
        self.operation_locations.borrow()
    }

    pub(super) fn operation_locations(&self) -> Ref<Vec<Option<ReadonlyTextRangeConcrete>>> {
        ref_unwrapped(&self.operation_locations)
    }

    pub(super) fn operation_locations_mut(&self) -> RefMut<Vec<Option<ReadonlyTextRangeConcrete>>> {
        ref_mut_unwrapped(&self.operation_locations)
    }

    pub(super) fn set_operation_locations(
        &self,
        operation_locations: Option<Vec<Option<ReadonlyTextRangeConcrete>>>,
    ) {
        *self.operation_locations.borrow_mut() = operation_locations;
    }

    pub(super) fn maybe_state(&self) -> Option<Id<Node /*Identifier*/>> {
        self.state.borrow().clone()
    }

    pub(super) fn state(&self) -> Id<Node /*Identifier*/> {
        self.state.borrow().clone().unwrap()
    }

    pub(super) fn set_state(&self, state: Option<Id<Node /*Identifier*/>>) {
        *self.state.borrow_mut() = state;
    }

    pub(super) fn block_index(&self) -> usize {
        self.block_index.get()
    }

    pub(super) fn set_block_index(&self, block_index: usize) {
        self.block_index.set(block_index);
    }

    pub(super) fn label_number(&self) -> u32 {
        self.label_number.get()
    }

    pub(super) fn set_label_number(&self, label_number: u32) {
        self.label_number.set(label_number);
    }

    pub(super) fn maybe_label_numbers(&self) -> Ref<Option<HashMap<u32, Vec<Label>>>> {
        self.label_numbers.borrow()
    }

    pub(super) fn maybe_label_numbers_mut(&self) -> RefMut<Option<HashMap<u32, Vec<Label>>>> {
        self.label_numbers.borrow_mut()
    }

    pub(super) fn set_label_numbers(&self, label_numbers: Option<HashMap<u32, Vec<Label>>>) {
        *self.label_numbers.borrow_mut() = label_numbers;
    }

    pub(super) fn last_operation_was_abrupt(&self) -> bool {
        self.last_operation_was_abrupt.get()
    }

    pub(super) fn set_last_operation_was_abrupt(&self, last_operation_was_abrupt: bool) {
        self.last_operation_was_abrupt
            .set(last_operation_was_abrupt);
    }

    pub(super) fn last_operation_was_completion(&self) -> bool {
        self.last_operation_was_completion.get()
    }

    pub(super) fn set_last_operation_was_completion(&self, last_operation_was_completion: bool) {
        self.last_operation_was_completion
            .set(last_operation_was_completion);
    }

    pub(super) fn maybe_clauses(&self) -> GcCellRef<Option<Vec<Id<Node /*CaseClause*/>>>> {
        self.clauses.borrow()
    }

    pub(super) fn maybe_clauses_mut(&self) -> GcCellRefMut<Option<Vec<Id<Node /*CaseClause*/>>>> {
        self.clauses.borrow_mut()
    }

    pub(super) fn set_clauses(&self, clauses: Option<Vec<Id<Node /*CaseClause*/>>>) {
        *self.clauses.borrow_mut() = clauses;
    }

    pub(super) fn maybe_statements(&self) -> GcCellRef<Option<Vec<Id<Node /*Statement*/>>>> {
        self.statements.borrow()
    }

    pub(super) fn maybe_statements_mut(&self) -> GcCellRefMut<Option<Vec<Id<Node /*Statement*/>>>> {
        self.statements.borrow_mut()
    }

    pub(super) fn set_statements(&self, statements: Option<Vec<Id<Node /*Statement*/>>>) {
        *self.statements.borrow_mut() = statements;
    }

    pub(super) fn maybe_exception_block_stack_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<GcCell<CodeBlock /*ExceptionBlock*/>>>>> {
        self.exception_block_stack.borrow_mut()
    }

    pub(super) fn exception_block_stack_mut(
        &self,
    ) -> GcCellRefMut<
        Option<Vec<Gc<GcCell<CodeBlock /*ExceptionBlock*/>>>>,
        Vec<Gc<GcCell<CodeBlock /*ExceptionBlock*/>>>,
    > {
        gc_cell_ref_mut_unwrapped(&self.exception_block_stack)
    }

    pub(super) fn set_exception_block_stack(
        &self,
        exception_block_stack: Option<Vec<Gc<GcCell<CodeBlock /*ExceptionBlock*/>>>>,
    ) {
        *self.exception_block_stack.borrow_mut() = exception_block_stack;
    }

    pub(super) fn maybe_current_exception_block(
        &self,
    ) -> Option<Gc<GcCell<CodeBlock /*ExceptionBlock*/>>> {
        self.current_exception_block.borrow().clone()
    }

    pub(super) fn current_exception_block(&self) -> Gc<GcCell<CodeBlock /*ExceptionBlock*/>> {
        self.current_exception_block.borrow().clone().unwrap()
    }

    pub(super) fn set_current_exception_block(
        &self,
        current_exception_block: Option<Gc<GcCell<CodeBlock /*ExceptionBlock*/>>>,
    ) {
        *self.current_exception_block.borrow_mut() = current_exception_block;
    }

    pub(super) fn maybe_with_block_stack(
        &self,
    ) -> GcCellRef<Option<Vec<Gc<GcCell<CodeBlock /*WithBlock*/>>>>> {
        self.with_block_stack.borrow()
    }

    pub(super) fn maybe_with_block_stack_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<GcCell<CodeBlock /*WithBlock*/>>>>> {
        self.with_block_stack.borrow_mut()
    }

    pub(super) fn with_block_stack_mut(
        &self,
    ) -> GcCellRefMut<
        Option<Vec<Gc<GcCell<CodeBlock /*WithBlock*/>>>>,
        Vec<Gc<GcCell<CodeBlock /*WithBlock*/>>>,
    > {
        gc_cell_ref_mut_unwrapped(&self.with_block_stack)
    }

    pub(super) fn set_with_block_stack(
        &self,
        with_block_stack: Option<Vec<Gc<GcCell<CodeBlock /*WithBlock*/>>>>,
    ) {
        *self.with_block_stack.borrow_mut() = with_block_stack;
    }

    pub(super) fn emit_helpers(&self) -> Gc<EmitHelperFactory> {
        self.context.ref_(self).get_emit_helper_factory()
    }

    pub(super) fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_source_file = node_ref.as_source_file();
        if node_as_source_file.is_declaration_file()
            || !node
                .ref_(self).transform_flags()
                .intersects(TransformFlags::ContainsGenerator)
        {
            return node;
        }

        visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self)
            .add_emit_helpers(self.context.ref_(self).read_emit_helpers().as_deref(), self)
    }

    pub(super) fn visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        let node_ref = node.ref_(self);
        let transform_flags = node_ref.transform_flags();
        if self.maybe_in_statement_containing_yield() == Some(true) {
            self.visit_java_script_in_statement_containing_yield(node)
        } else if self.maybe_in_generator_function_body() == Some(true) {
            self.visit_java_script_in_generator_function_body(node)
        } else if is_function_like_declaration(&node.ref_(self))
            && node
                .ref_(self).as_function_like_declaration()
                .maybe_asterisk_token()
                .is_some()
        {
            self.visit_generator(node)
        } else if transform_flags.intersects(TransformFlags::ContainsGenerator) {
            Some(
                visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self).into(),
            )
        } else {
            Some(node.into())
        }
    }

    pub(super) fn visit_java_script_in_statement_containing_yield(
        &self,
        node: Id<Node>,
    ) -> VisitResult /*<Node>*/ {
        match node.ref_(self).kind() {
            SyntaxKind::DoStatement => self.visit_do_statement(node),
            SyntaxKind::WhileStatement => self.visit_while_statement(node),
            SyntaxKind::SwitchStatement => self.visit_switch_statement(node),
            SyntaxKind::LabeledStatement => self.visit_labeled_statement(node),
            _ => self.visit_java_script_in_generator_function_body(node),
        }
    }

    pub(super) fn visit_java_script_in_generator_function_body(
        &self,
        node: Id<Node>,
    ) -> VisitResult /*<Node>*/ {
        match node.ref_(self).kind() {
            SyntaxKind::FunctionDeclaration => {
                self.visit_function_declaration(node).map(Into::into)
            }
            SyntaxKind::FunctionExpression => Some(self.visit_function_expression(node).into()),
            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                self.visit_accessor_declaration(node)
            }
            SyntaxKind::VariableStatement => self.visit_variable_statement(node).map(Into::into),
            SyntaxKind::ForStatement => self.visit_for_statement(node),
            SyntaxKind::ForInStatement => self.visit_for_in_statement(node),
            SyntaxKind::BreakStatement => Some(self.visit_break_statement(node).into()),
            SyntaxKind::ContinueStatement => Some(self.visit_continue_statement(node).into()),
            SyntaxKind::ReturnStatement => self.visit_return_statement(node),
            _ => {
                if node
                    .ref_(self).transform_flags()
                    .intersects(TransformFlags::ContainsYield)
                {
                    self.visit_java_script_containing_yield(node)
                } else if node.ref_(self).transform_flags().intersects(
                    TransformFlags::ContainsGenerator
                        | TransformFlags::ContainsHoistedDeclarationOrCompletion,
                ) {
                    Some(
                        visit_each_child(
                            node,
                            |node: Id<Node>| self.visitor(node),
                            &*self.context.ref_(self),
                            self,
                        )
                        .into(),
                    )
                } else {
                    Some(node.into())
                }
            }
        }
    }

    pub(super) fn visit_java_script_containing_yield(&self, node: Id<Node>) -> VisitResult /*<Node>*/
    {
        match node.ref_(self).kind() {
            SyntaxKind::BinaryExpression => Some(self.visit_binary_expression(node).into()),
            SyntaxKind::CommaListExpression => self.visit_comma_list_expression(node),
            SyntaxKind::ConditionalExpression => {
                Some(self.visit_conditional_expression(node).into())
            }
            SyntaxKind::YieldExpression => Some(self.visit_yield_expression(node).into()),
            SyntaxKind::ArrayLiteralExpression => self.visit_array_literal_expression(node),
            SyntaxKind::ObjectLiteralExpression => self.visit_object_literal_expression(node),
            SyntaxKind::ElementAccessExpression => self.visit_element_access_expression(node),
            SyntaxKind::CallExpression => self.visit_call_expression(node),
            SyntaxKind::NewExpression => self.visit_new_expression(node),
            _ => Some(
                visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self).into(),
            ),
        }
    }

    pub(super) fn visit_generator(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        match node.ref_(self).kind() {
            SyntaxKind::FunctionDeclaration => {
                self.visit_function_declaration(node).map(Into::into)
            }
            SyntaxKind::FunctionExpression => Some(self.visit_function_expression(node).into()),
            _ => Debug_.fail_bad_syntax_kind(&node.ref_(self), None),
        }
    }

    pub(super) fn visit_function_declaration(
        &self,
        mut node: Id<Node>, /*FunctionDeclaration*/
    ) -> Option<Id<Node /*Statement*/>> {
        if node
            .ref_(self).as_function_declaration()
            .maybe_asterisk_token()
            .is_some()
        {
            node = self
                .factory
                .ref_(self).create_function_declaration(
                    Option::<Gc<NodeArray>>::None,
                    node.ref_(self).maybe_modifiers(),
                    None,
                    node.ref_(self).as_function_declaration().maybe_name(),
                    Option::<Gc<NodeArray>>::None,
                    visit_parameter_list(
                        Some(&node.ref_(self).as_function_declaration().parameters()),
                        |node: Id<Node>| self.visitor(node),
                        &*self.context.ref_(self),
                        self,
                    )
                    .unwrap(),
                    None,
                    Some(self.transform_generator_function_body(
                        node.ref_(self).as_function_declaration().maybe_body().unwrap(),
                    )),
                )
                .set_text_range(Some(&*node.ref_(self)), self)
                .set_original_node(Some(node), self);
        } else {
            let saved_in_generator_function_body = self.maybe_in_generator_function_body();
            let saved_in_statement_containing_yield = self.maybe_in_statement_containing_yield();
            self.set_in_generator_function_body(Some(false));
            self.set_in_statement_containing_yield(Some(false));
            node = visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self);
            self.set_in_generator_function_body(saved_in_generator_function_body);
            self.set_in_statement_containing_yield(saved_in_statement_containing_yield);
        }

        if self.maybe_in_generator_function_body() == Some(true) {
            self.context.ref_(self).hoist_function_declaration(node);
            None
        } else {
            Some(node)
        }
    }

    pub(super) fn visit_function_expression(
        &self,
        mut node: Id<Node>, /*FunctionExpression*/
    ) -> Id<Node /*Expression*/> {
        if node
            .ref_(self).as_function_expression()
            .maybe_asterisk_token()
            .is_some()
        {
            node = self
                .factory
                .ref_(self).create_function_expression(
                    Option::<Gc<NodeArray>>::None,
                    None,
                    node.ref_(self).as_function_expression().maybe_name(),
                    Option::<Gc<NodeArray>>::None,
                    visit_parameter_list(
                        Some(&node.ref_(self).as_function_expression().parameters()),
                        |node: Id<Node>| self.visitor(node),
                        &*self.context.ref_(self),
                        self,
                    ),
                    None,
                    self.transform_generator_function_body(
                        node.ref_(self).as_function_expression().maybe_body().unwrap(),
                    ),
                )
                .set_text_range(Some(&*node.ref_(self)), self)
                .set_original_node(Some(node), self);
        } else {
            let saved_in_generator_function_body = self.maybe_in_generator_function_body();
            let saved_in_statement_containing_yield = self.maybe_in_statement_containing_yield();
            self.set_in_generator_function_body(Some(false));
            self.set_in_statement_containing_yield(Some(false));
            node = visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self);
            self.set_in_generator_function_body(saved_in_generator_function_body);
            self.set_in_statement_containing_yield(saved_in_statement_containing_yield);
        }

        node
    }
}

impl TransformerInterface for TransformGenerators {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(self.transform_source_file(node))
    }

    fn as_dyn_any(&self) -> &dyn Any {
        self
    }
}

impl HasArena for TransformGenerators {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformGeneratorsOnSubstituteNodeOverrider {
    transform_generators: Transformer,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformGeneratorsOnSubstituteNodeOverrider {
    fn new(
        transform_generators: Transformer,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_generators,
            previous_on_substitute_node,
        }
    }

    fn transform_generators(&self) -> debug_cell::Ref<'_, TransformGenerators> {
        downcast_transformer_ref(self.transform_generators, self)
    }

    fn substitute_expression(
        &self,
        node: Id<Node>, /*Expression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        if is_identifier(&node.ref_(self)) {
            return self.substitute_expression_identifier(node);
        }
        Ok(node)
    }

    fn substitute_expression_identifier(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<Id<Node>> {
        if !is_generated_identifier(&node.ref_(self))
            && self
                .transform_generators()
                .maybe_renamed_catch_variables()
                .as_ref()
                .matches(|renamed_catch_variables| {
                    renamed_catch_variables.contains_key(id_text(&node.ref_(self)))
                })
        {
            let original = get_original_node(node, self);
            if is_identifier(&original.ref_(self)) && original.ref_(self).maybe_parent().is_some() {
                let declaration = self
                    .transform_generators()
                    .resolver
                    .get_referenced_value_declaration(original)?;
                if let Some(declaration) = declaration {
                    let name = self
                        .transform_generators()
                        .renamed_catch_variable_declarations()
                        .get(&get_original_node_id(declaration, self))
                        .cloned();
                    if let Some(name) = name {
                        return Ok(self
                            .transform_generators()
                            .factory
                            .ref_(self).clone_node(name)
                            .set_text_range(Some(&*name.ref_(self)), self)
                            .and_set_parent(name.ref_(self).maybe_parent(), self)
                            .set_source_map_range(Some(self.alloc_source_map_range((&*node.ref_(self)).into())), self)
                            .set_comment_range((&*node.ref_(self)), self));
                    }
                }
            }
        }

        Ok(node)
    }
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformGeneratorsOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        let node = self
            .previous_on_substitute_node
            .on_substitute_node(hint, node)?;
        if hint == EmitHint::Expression {
            return self.substitute_expression(node);
        }
        Ok(node)
    }
}

impl HasArena for TransformGeneratorsOnSubstituteNodeOverrider {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
pub(super) struct TransformGeneratorsFactory {}

impl TransformGeneratorsFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformGeneratorsFactory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            TransformGenerators::new(context, &*static_arena()),
        )
    }
}

pub fn transform_generators(arena: &impl HasArena) -> TransformerFactory {
    arena.alloc_transformer_factory(Box::new(TransformGeneratorsFactory::new()))
}
