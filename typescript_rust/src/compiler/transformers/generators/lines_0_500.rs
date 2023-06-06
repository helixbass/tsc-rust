use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    io, mem,
    rc::Rc,
};

use gc::{Finalize, Gc, GcCell, Trace};

use crate::{
    BaseNodeFactorySynthetic, CompilerOptions, EmitHelperFactory, EmitHint, EmitResolver, Node,
    NodeFactory, NodeId, ReadonlyTextRangeConcrete, TransformationContext,
    TransformationContextOnSubstituteNodeOverrider, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface, _d, chain_bundle,
};

pub(super) type Label = u32;

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

// type OperationArguments = [Label] | [Label, Expression] | [Statement] | [Expression | undefined] | [Expression, Expression]
type OperationArguments = ();

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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

#[derive(Trace, Finalize)]
pub(super) struct ExceptionBlock {
    #[unsafe_ignore_trace]
    pub kind: CodeBlockKind, /*CodeBlockKind.Exception*/
    #[unsafe_ignore_trace]
    pub state: ExceptionBlockState,
    pub start_label: Label,
    pub catch_variable: Option<Gc<Node /*Identifier*/>>,
    pub catch_label: Option<Label>,
    pub finally_label: Option<Label>,
    pub end_label: Label,
}

#[derive(Trace, Finalize)]
pub(super) struct LabeledBlock {
    #[unsafe_ignore_trace]
    pub kind: CodeBlockKind, /*CodeBlockKind.Labeled*/
    pub label_text: String,
    pub is_script: bool,
    pub break_label: Label,
}

#[derive(Trace, Finalize)]
pub(super) struct SwitchBlock {
    #[unsafe_ignore_trace]
    pub kind: CodeBlockKind, /*CodeBlockKind.Switch*/
    pub is_script: bool,
    pub break_label: Label,
}

#[derive(Trace, Finalize)]
pub(super) struct LoopBlock {
    #[unsafe_ignore_trace]
    pub kind: CodeBlockKind, /*CodeBlockKind.Loop*/
    pub continue_label: Label,
    pub is_script: bool,
    pub break_label: Label,
}

#[derive(Trace, Finalize)]
pub(super) struct WithBlock {
    #[unsafe_ignore_trace]
    pub kind: CodeBlockKind, /*CodeBlockKind.With*/
    pub expression: Gc<Node /*Identifier*/>,
    pub start_label: Label,
    pub end_label: Label,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(super) enum Instruction {
    Next = 0,
    Throw = 1,
    Return = 2,
    Break = 3,
    Yield = 4,
    YieldStar = 5,
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
    pub(super) _transformer_wrapper: GcCell<Option<Transformer>>,
    pub(super) context: Gc<Box<dyn TransformationContext>>,
    pub(super) factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    pub(super) compiler_options: Gc<CompilerOptions>,
    pub(super) resolver: Gc<Box<dyn EmitResolver>>,
    pub(super) renamed_catch_variables: GcCell<HashMap<String, bool>>,
    pub(super) renamed_catch_variable_declarations:
        GcCell<HashMap<NodeId, Gc<Node /*Identifier*/>>>,
    #[unsafe_ignore_trace]
    pub(super) is_generator_function_body: Cell<bool>,
    #[unsafe_ignore_trace]
    pub(super) is_statement_containing_yield: Cell<bool>,
    pub(super) blocks: GcCell<Option<Vec<Gc<CodeBlock>>>>,
    #[unsafe_ignore_trace]
    pub(super) block_offsets: RefCell<Option<Vec<usize>>>,
    #[unsafe_ignore_trace]
    pub(super) block_actions: RefCell<Option<Vec<BlockAction>>>,
    pub(super) block_stack: GcCell<Option<Vec<Gc<CodeBlock>>>>,
    #[unsafe_ignore_trace]
    pub(super) label_offsets: RefCell<Option<Vec<usize>>>,
    pub(super) label_expressions: GcCell<Option<Vec<Vec<Gc<Node /*Mutable<LiteralExpression>*/>>>>>,
    #[unsafe_ignore_trace]
    pub(super) next_label_id: Cell<u32>,
    #[unsafe_ignore_trace]
    pub(super) operations: RefCell<Option<Vec<OpCode>>>,
    pub(super) operation_arguments: GcCell<Option<Vec<Option<OperationArguments>>>>,
    #[unsafe_ignore_trace]
    pub(super) operation_locations: RefCell<Option<Vec<Option<ReadonlyTextRangeConcrete>>>>,
    pub(super) state: GcCell<Option<Gc<Node /*Identifier*/>>>,
    #[unsafe_ignore_trace]
    pub(super) block_index: Cell<usize>,
    #[unsafe_ignore_trace]
    pub(super) label_number: Cell<u32>,
    #[unsafe_ignore_trace]
    pub(super) label_numbers: RefCell<Option<Vec<Vec<u32>>>>,
    #[unsafe_ignore_trace]
    pub(super) last_operation_was_abrupt: Cell<bool>,
    #[unsafe_ignore_trace]
    pub(super) last_operation_was_completion: Cell<bool>,
    pub(super) clauses: GcCell<Option<Vec<Gc<Node /*CaseClause*/>>>>,
    pub(super) statements: GcCell<Option<Vec<Gc<Node /*Statement*/>>>>,
    pub(super) exception_block_stack: GcCell<Option<Vec<Gc<Node /*ExceptionBlock*/>>>>,
    pub(super) current_exception_block: GcCell<Option<Gc<Node /*ExceptionBlock*/>>>,
    pub(super) with_block_stack: GcCell<Option<Vec<Gc<Node /*WithBlock*/>>>>,
}

impl TransformGenerators {
    pub fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: _d(),
            factory: context.factory(),
            compiler_options: context.get_compiler_options(),
            resolver: context.get_emit_resolver(),
            context: context.clone(),
            renamed_catch_variables: _d(),
            renamed_catch_variable_declarations: _d(),
            is_generator_function_body: _d(),
            is_statement_containing_yield: _d(),
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
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(TransformGeneratorsOnSubstituteNodeOverrider::new(
                downcasted.clone(),
                previous_on_substitute_node,
            )))
        });
        downcasted
    }

    pub(super) fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    pub(super) fn emit_helpers(&self) -> Rc<EmitHelperFactory> {
        self.context.get_emit_helper_factory()
    }

    pub(super) fn transform_source_file(&self, _node: &Node /*SourceFile*/) -> Gc<Node> {
        unimplemented!()
    }
}

impl TransformerInterface for TransformGenerators {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        Ok(self.transform_source_file(node))
    }
}

#[derive(Trace, Finalize)]
struct TransformGeneratorsOnSubstituteNodeOverrider {
    transform_generators: Gc<Box<TransformGenerators>>,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformGeneratorsOnSubstituteNodeOverrider {
    fn new(
        transform_generators: Gc<Box<TransformGenerators>>,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_generators,
            previous_on_substitute_node,
        }
    }
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformGeneratorsOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, _hint: EmitHint, _node: &Node) -> io::Result<Gc<Node>> {
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
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            TransformGenerators::new(context).as_transformer(),
        )
    }
}

pub fn transform_generators() -> TransformerFactory {
    Gc::new(Box::new(TransformGeneratorsFactory::new()))
}
