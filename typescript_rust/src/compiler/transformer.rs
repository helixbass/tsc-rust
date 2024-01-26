use std::{
    cell::{Cell, RefCell, RefMut},
    collections::HashMap,
    io, mem, any::Any,
};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use id_arena::Id;

use crate::{
    add_range, append, chain_bundle, create_emit_helper_factory, dispose_emit_nodes,
    gc_cell_ref_unwrapped, get_emit_flags, get_emit_module_kind, get_emit_script_target,
    get_factory, get_jsx_transform_enabled, get_parse_tree_node, get_synthetic_factory, is_bundle,
    is_source_file, maybe_get_source_file_of_node, maybe_map, not_implemented, set_emit_flags,
    some, transform_class_fields, transform_declarations, transform_ecmascript_module,
    transform_es2015, transform_es2016, transform_es2017, transform_es2018, transform_es2019,
    transform_es2020, transform_es2021, transform_es5, transform_esnext, transform_generators,
    transform_jsx, transform_module, transform_node_module, transform_system_module,
    transform_type_script, BaseNodeFactorySynthetic, CompilerOptions, CoreTransformationContext,
    CustomTransformer, CustomTransformers, Debug_, Diagnostic, EmitFlags, EmitHelper,
    EmitHelperBase, EmitHelperFactory, EmitHint, EmitHost, EmitResolver, EmitTransformers,
    GetOrInsertDefault, LexicalEnvironmentFlags, ModuleKind, Node, NodeArray, NodeFactory,
    NodeFlags, NodeInterface, ScriptTarget, SyntaxKind, TransformationContext,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    TransformationResult, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerFactoryOrCustomTransformerFactory, TransformerInterface, _d,
    HasArena, AllArenas, InArena, static_arena,
    BaseNodeFactory,
};

fn get_module_transformer(module_kind: ModuleKind, arena: &impl HasArena) -> TransformerFactory {
    match module_kind {
        ModuleKind::ESNext | ModuleKind::ES2022 | ModuleKind::ES2020 | ModuleKind::ES2015 => {
            transform_ecmascript_module(arena)
        }
        ModuleKind::System => transform_system_module(arena),
        ModuleKind::Node12 | ModuleKind::NodeNext => transform_node_module(arena),
        _ => transform_module(arena),
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum TransformationState {
    Uninitialized,
    Initialized,
    Completed,
    Disposed,
}

bitflags! {
    struct SyntaxKindFeatureFlags: u32 {
        const None = 0;
        const Substitution = 1 << 0;
        const EmitNotifications = 1 << 1;
    }
}

// lazy_static! {
//     pub static ref no_transformers: EmitTransformers = EmitTransformers::new(vec![], vec![]);
// }
pub fn no_transformers() -> EmitTransformers {
    EmitTransformers::new(vec![], vec![])
}

pub fn get_transformers(
    compiler_options: &CompilerOptions,
    custom_transformers: Option<&CustomTransformers>,
    emit_only_dts_files: Option<bool>,
    arena: &impl HasArena,
) -> EmitTransformers {
    EmitTransformers::new(
        get_script_transformers(compiler_options, custom_transformers, emit_only_dts_files, arena),
        get_declaration_transformers(custom_transformers, arena),
    )
}

fn get_script_transformers(
    compiler_options: &CompilerOptions,
    custom_transformers: Option<&CustomTransformers>,
    emit_only_dts_files: Option<bool>,
    arena: &impl HasArena,
) -> Vec<TransformerFactory> {
    let emit_only_dts_files = emit_only_dts_files.unwrap_or(false);
    if emit_only_dts_files {
        return vec![];
    }

    let language_version = get_emit_script_target(compiler_options);
    let module_kind = get_emit_module_kind(compiler_options);
    let mut transformers: Vec<TransformerFactory> = vec![];

    add_range(
        &mut transformers,
        custom_transformers
            .and_then(|custom_transformers| {
                maybe_map(custom_transformers.before.as_ref(), |factory, _| {
                    wrap_script_transformer_factory(factory.clone(), arena)
                })
            })
            .as_deref(),
        None,
        None,
    );

    transformers.push(transform_type_script(arena));
    transformers.push(transform_class_fields(arena));

    if get_jsx_transform_enabled(compiler_options) {
        transformers.push(transform_jsx(arena));
    }

    if language_version < ScriptTarget::ESNext {
        transformers.push(transform_esnext(arena));
    }

    if language_version < ScriptTarget::ES2021 {
        transformers.push(transform_es2021(arena));
    }

    if language_version < ScriptTarget::ES2020 {
        transformers.push(transform_es2020(arena));
    }

    if language_version < ScriptTarget::ES2019 {
        transformers.push(transform_es2019(arena));
    }

    if language_version < ScriptTarget::ES2018 {
        transformers.push(transform_es2018(arena));
    }

    if language_version < ScriptTarget::ES2017 {
        transformers.push(transform_es2017(arena));
    }

    if language_version < ScriptTarget::ES2016 {
        transformers.push(transform_es2016(arena));
    }

    if language_version < ScriptTarget::ES2015 {
        transformers.push(transform_es2015(arena));
        transformers.push(transform_generators(arena));
    }

    transformers.push(get_module_transformer(module_kind, arena));

    if language_version < ScriptTarget::ES5 {
        transformers.push(transform_es5(arena));
    }

    add_range(
        &mut transformers,
        custom_transformers
            .and_then(|custom_transformers| {
                maybe_map(custom_transformers.after.as_ref(), |factory, _| {
                    wrap_script_transformer_factory(factory.clone(), arena)
                })
            })
            .as_deref(),
        None,
        None,
    );

    transformers
}

fn get_declaration_transformers(
    custom_transformers: Option<&CustomTransformers>,
    arena: &impl HasArena,
) -> Vec<TransformerFactory> {
    let mut transformers: Vec<TransformerFactory> = vec![];
    transformers.push(transform_declarations(arena));
    add_range(
        &mut transformers,
        custom_transformers
            .and_then(|custom_transformers| {
                maybe_map(
                    custom_transformers.after_declarations.as_ref(),
                    |factory, _| wrap_declaration_transformer_factory(factory.clone(), arena),
                )
            })
            .as_deref(),
        None,
        None,
    );
    transformers
}

fn wrap_custom_transformer(transformer: CustomTransformer, arena: &impl HasArena) -> Transformer /*<Bundle | SourceFile>*/
{
    arena.alloc_transformer(Box::new(WrapCustomTransformer::new(transformer)))
}

#[derive(Trace, Finalize)]
pub struct WrapCustomTransformer {
    transformer: CustomTransformer,
}

impl WrapCustomTransformer {
    pub fn new(transformer: CustomTransformer) -> Self {
        Self { transformer }
    }
}

impl TransformerInterface for WrapCustomTransformer {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(if is_bundle(&node.ref_(self)) {
            self.transformer.transform_bundle(node)
        } else {
            self.transformer.transform_source_file(node)
        })
    }

    fn as_dyn_any(&self) -> &dyn Any {
        self
    }
}

impl HasArena for WrapCustomTransformer {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub trait WrapCustomTransformerFactoryHandleDefault: Trace + Finalize {
    fn call(
        &self,
        context: Id<TransformNodesTransformationResult>,
        transformer: Transformer,
    ) -> Transformer /*<SourceFile | Bundle>*/;
}

fn wrap_custom_transformer_factory(
    transformer: Gc<TransformerFactoryOrCustomTransformerFactory>,
    handle_default: Gc<Box<dyn WrapCustomTransformerFactoryHandleDefault>>,
    arena: &impl HasArena,
) -> TransformerFactory /*<SourceFile | Bundle>*/ {
    arena.alloc_transformer_factory(Box::new(WrapCustomTransformerFactory::new(
        transformer,
        handle_default,
    )))
}

#[derive(Trace, Finalize)]
pub struct WrapCustomTransformerFactory {
    transformer: Gc<TransformerFactoryOrCustomTransformerFactory>,
    handle_default: Gc<Box<dyn WrapCustomTransformerFactoryHandleDefault>>,
}

impl WrapCustomTransformerFactory {
    pub fn new(
        transformer: Gc<TransformerFactoryOrCustomTransformerFactory>,
        handle_default: Gc<Box<dyn WrapCustomTransformerFactoryHandleDefault>>,
    ) -> Self {
        Self {
            transformer,
            handle_default,
        }
    }
}

impl TransformerFactoryInterface for WrapCustomTransformerFactory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        match &*self.transformer {
            TransformerFactoryOrCustomTransformerFactory::TransformerFactory(transformer) => {
                let custom_transformer = transformer.ref_(self).call(context.clone());
                self.handle_default.call(context, custom_transformer)
            }
            TransformerFactoryOrCustomTransformerFactory::CustomTransformerFactory(transformer) => {
                let custom_transformer = transformer.call(context);
                wrap_custom_transformer(custom_transformer, self)
            }
        }
    }
}

impl HasArena for WrapCustomTransformerFactory {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

fn wrap_script_transformer_factory(
    transformer: Gc<TransformerFactoryOrCustomTransformerFactory /*<SourceFile>*/>,
    arena: &impl HasArena,
) -> TransformerFactory /*<Bundle | SourceFile>*/ {
    wrap_custom_transformer_factory(transformer, chain_bundle(), arena)
}

fn wrap_declaration_transformer_factory(
    transformer: Gc<TransformerFactoryOrCustomTransformerFactory /*<Bundle | SourceFile>*/>,
    arena: &impl HasArena,
) -> TransformerFactory /*<Bundle | SourceFile>*/ {
    wrap_custom_transformer_factory(transformer, passthrough_transformer(), arena)
}

#[derive(Trace, Finalize)]
struct PassthroughTransformer;

impl WrapCustomTransformerFactoryHandleDefault for PassthroughTransformer {
    fn call(
        &self,
        _context: Id<TransformNodesTransformationResult>,
        transform_source_file: Transformer,
    ) -> Transformer {
        transform_source_file
    }
}

fn passthrough_transformer() -> Gc<Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
    thread_local! {
        static PASSTHROUGH_TRANSFORMER: Gc<Box<dyn WrapCustomTransformerFactoryHandleDefault>> = Gc::new(Box::new(PassthroughTransformer));
    }
    PASSTHROUGH_TRANSFORMER.with(|passthrough_transformer| passthrough_transformer.clone())
}

pub fn no_emit_substitution(_hint: EmitHint, node: Id<Node>) -> Id<Node> {
    node
}

pub fn no_emit_notification(
    hint: EmitHint,
    node: Id<Node>,
    callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
) -> io::Result<()> {
    callback(hint, node)
}

pub fn transform_nodes(
    resolver: Option<Gc<Box<dyn EmitResolver>>>,
    host: Option<Id<Box<dyn EmitHost>>>,
    factory: Id<NodeFactory>,
    options: Id<CompilerOptions>,
    nodes: &[Id<Node>],
    transformers: &[TransformerFactory],
    allow_dts_files: bool,
    arena: &impl HasArena,
    // TODO: would presumably have to do some further Gc-dyn-casting shenanigans in order to return
    // this type, but looks like there aren't any other TransformationResult implementers so maybe
    // returning the concrete type is fine?
    // ) -> Gc<Box<dyn TransformationResult>> {
) -> io::Result<Id<TransformNodesTransformationResult>> {
    let transformation_result = TransformNodesTransformationResult::new(
        vec![],
        TransformationState::Uninitialized,
        nodes.to_owned(),
        None,
        None,
        vec![],
        vec![],
        None,
        vec![],
        transformers.to_owned(),
        allow_dts_files,
        options,
        resolver,
        host,
        factory,
        &*static_arena(),
    );
    transformation_result.ref_(arena).call()?;
    Ok(transformation_result)
}

#[derive(Trace, Finalize)]
pub struct TransformNodesTransformationResult {
    #[unsafe_ignore_trace]
    _arena: *const AllArenas,
    #[unsafe_ignore_trace]
    _arena_id: Cell<Option<Id<Self>>>,
    on_emit_node_outermost_override_or_original_method:
        GcCell<Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>>,
    on_emit_node_previous_override_or_original_method:
        GcCell<Option<Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>>>,
    on_substitute_node_outermost_override_or_original_method:
        GcCell<Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>>,
    on_substitute_node_previous_override_or_original_method:
        GcCell<Option<Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>>>,
    transformed: GcCell<Vec<Id<Node>>>,
    #[unsafe_ignore_trace]
    state: Cell<TransformationState>,
    nodes: Vec<Id<Node>>,
    #[unsafe_ignore_trace]
    enabled_syntax_kind_features: RefCell<HashMap<SyntaxKind, SyntaxKindFeatureFlags>>,
    lexical_environment_variable_declarations:
        GcCell<Option<Vec<Id<Node /*VariableDeclaration*/>>>>,
    lexical_environment_function_declarations:
        GcCell<Option<Vec<Id<Node /*FunctionDeclaration*/>>>>,
    lexical_environment_statements: GcCell<Option<Vec<Id<Node /*Statement*/>>>>,
    #[unsafe_ignore_trace]
    lexical_environment_flags: Cell<LexicalEnvironmentFlags>,
    lexical_environment_variable_declarations_stack:
        GcCell<Option<Vec<Option<Vec<Id<Node /*VariableDeclaration*/>>>>>>,
    lexical_environment_function_declarations_stack:
        GcCell<Option<Vec<Option<Vec<Id<Node /*FunctionDeclaration*/>>>>>>,
    lexical_environment_statements_stack: GcCell<Option<Vec<Option<Vec<Id<Node /*Statement*/>>>>>>,
    #[unsafe_ignore_trace]
    lexical_environment_flags_stack: RefCell<Option<Vec<LexicalEnvironmentFlags>>>,
    #[unsafe_ignore_trace]
    lexical_environment_suspended: Cell<bool>,
    block_scoped_variable_declarations_stack:
        GcCell<Option<Vec<Option<Vec<Id<Node /*Identifier*/>>>>>>,
    #[unsafe_ignore_trace]
    block_scope_stack_offset: Cell<usize>,
    block_scoped_variable_declarations: GcCell<Option<Vec<Id<Node /*Identifier*/>>>>,
    emit_helpers: GcCell<Option<Vec<Id<EmitHelper>>>>,
    diagnostics: GcCell<Vec<Id<Diagnostic>>>,
    transformers: Vec<TransformerFactory>,
    transformers_with_context: GcCell<Option<Vec<Transformer>>>,
    allow_dts_files: bool,
    options: Id<CompilerOptions>,
    resolver: Option<Gc<Box<dyn EmitResolver>>>,
    host: Option<Id<Box<dyn EmitHost>>>,
    created_emit_helper_factory: GcCell<Option<Gc<EmitHelperFactory>>>,
    factory: Id<NodeFactory>,
}

impl TransformNodesTransformationResult {
    pub fn new(
        transformed: Vec<Id<Node>>,
        state: TransformationState,
        nodes: Vec<Id<Node>>,
        lexical_environment_variable_declarations: Option<Vec<Id<Node>>>,
        lexical_environment_function_declarations: Option<Vec<Id<Node>>>,
        lexical_environment_variable_declarations_stack: Vec<Option<Vec<Id<Node>>>>,
        lexical_environment_function_declarations_stack: Vec<Option<Vec<Id<Node>>>>,
        emit_helpers: Option<Vec<Id<EmitHelper>>>,
        diagnostics: Vec<Id<Diagnostic>>,
        transformers: Vec<TransformerFactory>,
        allow_dts_files: bool,
        options: Id<CompilerOptions>,
        resolver: Option<Gc<Box<dyn EmitResolver>>>,
        host: Option<Id<Box<dyn EmitHost>>>,
        factory: Id<NodeFactory>,
        arena: *const AllArenas,
    ) -> Id<Self> {
        let arena_ref = unsafe { &*arena };
        let ret = arena_ref.alloc_transform_nodes_transformation_result(Self {
            _arena: arena,
            _arena_id: _d(),
            on_emit_node_outermost_override_or_original_method: GcCell::new(Gc::new(Box::new(
                NoEmitNotificationTransformationContextOnEmitNodeOverrider,
            ))),
            on_emit_node_previous_override_or_original_method: _d(),
            on_substitute_node_outermost_override_or_original_method: GcCell::new(Gc::new(
                Box::new(NoEmitNotificationTransformationContextOnSubstituteNodeOverrider),
            )),
            on_substitute_node_previous_override_or_original_method: _d(),
            transformed: GcCell::new(transformed),
            state: Cell::new(state),
            nodes,
            lexical_environment_variable_declarations: GcCell::new(
                lexical_environment_variable_declarations,
            ),
            lexical_environment_function_declarations: GcCell::new(
                lexical_environment_function_declarations,
            ),
            lexical_environment_statements: GcCell::new(Some(vec![])),
            lexical_environment_variable_declarations_stack: GcCell::new(Some(
                lexical_environment_variable_declarations_stack,
            )),
            lexical_environment_function_declarations_stack: GcCell::new(Some(
                lexical_environment_function_declarations_stack,
            )),
            lexical_environment_statements_stack: GcCell::new(Some(vec![])),
            lexical_environment_flags_stack: RefCell::new(Some(vec![])),
            emit_helpers: GcCell::new(emit_helpers),
            diagnostics: GcCell::new(diagnostics),
            transformers,
            transformers_with_context: Default::default(),
            allow_dts_files,
            enabled_syntax_kind_features: Default::default(),
            lexical_environment_flags: Cell::new(LexicalEnvironmentFlags::None),
            lexical_environment_suspended: Default::default(),
            block_scoped_variable_declarations_stack: GcCell::new(Some(vec![])),
            block_scoped_variable_declarations: GcCell::new(Some(vec![])),
            block_scope_stack_offset: Default::default(),
            options,
            resolver,
            host,
            created_emit_helper_factory: Default::default(),
            factory,
        });
        ret.ref_(arena_ref).set_arena_id(ret);
        ret
    }

    fn arena_id(&self) -> Id<Self> {
        self._arena_id.get().unwrap()
    }

    fn set_arena_id(&self, id: Id<Self>) {
        self._arena_id.set(Some(id))
    }

    fn state(&self) -> TransformationState {
        self.state.get()
    }

    fn lexical_environment_variable_declarations(&self) -> GcCellRefMut<Option<Vec<Id<Node>>>> {
        self.lexical_environment_variable_declarations.borrow_mut()
    }

    fn set_lexical_environment_variable_declarations(
        &self,
        lexical_environment_variable_declarations: Option<Vec<Id<Node>>>,
    ) {
        *self.lexical_environment_variable_declarations.borrow_mut() =
            lexical_environment_variable_declarations;
    }

    fn lexical_environment_function_declarations(&self) -> GcCellRefMut<Option<Vec<Id<Node>>>> {
        self.lexical_environment_function_declarations.borrow_mut()
    }

    fn set_lexical_environment_function_declarations(
        &self,
        lexical_environment_function_declarations: Option<Vec<Id<Node>>>,
    ) {
        *self.lexical_environment_function_declarations.borrow_mut() =
            lexical_environment_function_declarations;
    }

    fn lexical_environment_statements(&self) -> GcCellRefMut<Option<Vec<Id<Node>>>> {
        self.lexical_environment_statements.borrow_mut()
    }

    fn lexical_environment_variable_declarations_stack(
        &self,
    ) -> GcCellRefMut<Option<Vec<Option<Vec<Id<Node>>>>>, Vec<Option<Vec<Id<Node>>>>> {
        GcCellRefMut::map(
            self.lexical_environment_variable_declarations_stack
                .borrow_mut(),
            |option| option.as_mut().unwrap(),
        )
    }

    fn set_lexical_environment_variable_declarations_stack(
        &self,
        lexical_environment_variable_declarations_stack: Option<Vec<Option<Vec<Id<Node>>>>>,
    ) {
        *self
            .lexical_environment_variable_declarations_stack
            .borrow_mut() = lexical_environment_variable_declarations_stack;
    }

    fn lexical_environment_function_declarations_stack(
        &self,
    ) -> GcCellRefMut<Option<Vec<Option<Vec<Id<Node>>>>>, Vec<Option<Vec<Id<Node>>>>> {
        GcCellRefMut::map(
            self.lexical_environment_function_declarations_stack
                .borrow_mut(),
            |option| option.as_mut().unwrap(),
        )
    }

    fn set_lexical_environment_function_declarations_stack(
        &self,
        lexical_environment_function_declarations_stack: Option<Vec<Option<Vec<Id<Node>>>>>,
    ) {
        *self
            .lexical_environment_function_declarations_stack
            .borrow_mut() = lexical_environment_function_declarations_stack;
    }

    fn lexical_environment_statements_stack(
        &self,
    ) -> GcCellRefMut<Option<Vec<Option<Vec<Id<Node>>>>>, Vec<Option<Vec<Id<Node>>>>> {
        GcCellRefMut::map(
            self.lexical_environment_statements_stack.borrow_mut(),
            |option| option.as_mut().unwrap(),
        )
    }

    fn lexical_environment_flags_stack(&self) -> RefMut<Vec<LexicalEnvironmentFlags>> {
        RefMut::map(
            self.lexical_environment_flags_stack.borrow_mut(),
            |option| option.as_mut().unwrap(),
        )
    }

    fn block_scoped_variable_declarations_stack(
        &self,
    ) -> GcCellRefMut<Option<Vec<Option<Vec<Id<Node>>>>>, Vec<Option<Vec<Id<Node>>>>> {
        GcCellRefMut::map(
            self.block_scoped_variable_declarations_stack.borrow_mut(),
            |option| option.as_mut().unwrap(),
        )
    }

    fn set_block_scoped_variable_declarations_stack(
        &self,
        block_scoped_variable_declarations_stack: Option<Vec<Option<Vec<Id<Node>>>>>,
    ) {
        *self.block_scoped_variable_declarations_stack.borrow_mut() =
            block_scoped_variable_declarations_stack;
    }

    fn maybe_block_scoped_variable_declarations(&self) -> GcCellRef<Option<Vec<Id<Node>>>> {
        self.block_scoped_variable_declarations.borrow()
    }

    fn block_scoped_variable_declarations(&self) -> GcCellRef<Vec<Id<Node>>> {
        gc_cell_ref_unwrapped(&self.block_scoped_variable_declarations)
    }

    fn maybe_block_scoped_variable_declarations_mut(&self) -> GcCellRefMut<Option<Vec<Id<Node>>>> {
        self.block_scoped_variable_declarations.borrow_mut()
    }

    fn set_block_scoped_variable_declarations(
        &self,
        block_scoped_variable_declarations: Option<Vec<Id<Node>>>,
    ) {
        *self.block_scoped_variable_declarations.borrow_mut() = block_scoped_variable_declarations;
    }

    fn emit_helpers(&self) -> Option<Vec<Id<EmitHelper>>> {
        self.emit_helpers.borrow().clone()
    }

    fn emit_helpers_mut(&self) -> GcCellRefMut<Option<Vec<Id<EmitHelper>>>> {
        self.emit_helpers.borrow_mut()
    }

    fn set_emit_helpers(&self, emit_helpers: Option<Vec<Id<EmitHelper>>>) {
        *self.emit_helpers.borrow_mut() = emit_helpers;
    }

    fn set_state(&self, state: TransformationState) {
        self.state.set(state);
    }

    fn diagnostics(&self) -> GcCellRefMut<Vec<Id<Diagnostic>>> {
        self.diagnostics.borrow_mut()
    }

    fn transformers_with_context(&self) -> GcCellRef<Vec<Transformer>> {
        gc_cell_ref_unwrapped(&self.transformers_with_context)
    }

    fn set_transformers_with_context(&self, transformers_with_context: Vec<Transformer>) {
        *self.transformers_with_context.borrow_mut() = Some(transformers_with_context);
    }

    fn transformed(&self) -> GcCellRefMut<Vec<Id<Node>>> {
        self.transformed.borrow_mut()
    }

    fn enabled_syntax_kind_features(&self) -> RefMut<HashMap<SyntaxKind, SyntaxKindFeatureFlags>> {
        self.enabled_syntax_kind_features.borrow_mut()
    }

    fn lexical_environment_flags(&self) -> LexicalEnvironmentFlags {
        self.lexical_environment_flags.get()
    }

    fn set_lexical_environment_flags_(&self, lexical_environment_flags: LexicalEnvironmentFlags) {
        self.lexical_environment_flags
            .set(lexical_environment_flags)
    }

    fn lexical_environment_suspended(&self) -> bool {
        self.lexical_environment_suspended.get()
    }

    fn set_lexical_environment_suspended(&self, lexical_environment_suspended: bool) {
        self.lexical_environment_suspended
            .set(lexical_environment_suspended)
    }

    fn block_scope_stack_offset(&self) -> usize {
        self.block_scope_stack_offset.get()
    }

    fn set_block_scope_stack_offset(&self, block_scope_stack_offset: usize) {
        self.block_scope_stack_offset.set(block_scope_stack_offset)
    }

    fn created_emit_helper_factory(&self) -> GcCellRefMut<Option<Gc<EmitHelperFactory>>> {
        self.created_emit_helper_factory.borrow_mut()
    }

    fn call(&self) -> io::Result<()> {
        for &node in &self.nodes {
            dispose_emit_nodes(maybe_get_source_file_of_node(get_parse_tree_node(
                Some(node),
                Option::<fn(Id<Node>) -> bool>::None,
                self,
            ), self), self)
        }

        // performance.mark("beforeTransform");

        self.set_transformers_with_context(
            self.transformers
                .iter()
                .map(|t| t.ref_(self).call(self.arena_id()))
                .collect(),
        );

        self.set_state(TransformationState::Initialized);

        for &node in &self.nodes {
            // tracing?.push(tracing.Phase.Emit, "transformNodes", node.kind === SyntaxKind.SourceFile ? { path: (node as any as SourceFile).path } : { kind: node.kind, pos: node.pos, end: node.end });}
            self.transformed().push(if self.allow_dts_files {
                self.transformation(node)?
            } else {
                self.transform_root(node)?
            });
            // tracing?.pop();
        }

        self.set_state(TransformationState::Completed);

        // performance.mark("afterTransform");
        // performance.measure("transformTime", "beforeTransform", "afterTransform");

        Ok(())
    }

    fn transformation(&self, mut node: Id<Node>) -> io::Result<Id<Node>> {
        for transform in self.transformers_with_context().iter() {
            node = transform.ref_(self).call(node)?;
        }
        Ok(node)
    }

    fn transform_root(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(
            if
            /*node &&*/
            !is_source_file(&node.ref_(self)) || !node.ref_(self).as_source_file().is_declaration_file() {
                self.transformation(node)?
            } else {
                node
            },
        )
    }
}

impl CoreTransformationContext for TransformNodesTransformationResult {
    fn factory(&self) -> Id<NodeFactory> {
        self.factory.clone()
    }

    fn get_compiler_options(&self) -> Id<CompilerOptions> {
        self.options.clone()
    }

    fn start_lexical_environment(&self) {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot modify the lexical environment during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the lexical environment after transformation has completed."),
        );
        Debug_.assert(
            !self.lexical_environment_suspended(),
            Some("Lexical environment is suspended."),
        );

        let mut lexical_environment_variable_declarations_stack =
            self.lexical_environment_variable_declarations_stack();
        lexical_environment_variable_declarations_stack
            .push(self.lexical_environment_variable_declarations().take());
        let mut lexical_environment_function_declarations_stack =
            self.lexical_environment_function_declarations_stack();
        lexical_environment_function_declarations_stack
            .push(self.lexical_environment_function_declarations().take());
        let mut lexical_environment_statements_stack = self.lexical_environment_statements_stack();
        lexical_environment_statements_stack.push(self.lexical_environment_statements().take());
        let mut lexical_environment_flags_stack = self.lexical_environment_flags_stack();
        lexical_environment_flags_stack.push(self.lexical_environment_flags());
        // lexicalEnvironmentStackOffset++;
        // lexicalEnvironmentVariableDeclarations = undefined!;
        // lexicalEnvironmentFunctionDeclarations = undefined!;
        // lexicalEnvironmentStatements = undefined!;
        self.set_lexical_environment_flags_(LexicalEnvironmentFlags::None);
    }

    fn set_lexical_environment_flags(&self, flags: LexicalEnvironmentFlags, value: bool) {
        self.set_lexical_environment_flags_(if value {
            self.lexical_environment_flags() | flags
        } else {
            self.lexical_environment_flags() & !flags
        });
    }

    fn get_lexical_environment_flags(&self) -> LexicalEnvironmentFlags {
        self.lexical_environment_flags()
    }

    fn suspend_lexical_environment(&self) {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot modify the lexical environment during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the lexical environment after transformation has completed."),
        );
        Debug_.assert(
            !self.lexical_environment_suspended(),
            Some("Lexical environment is already suspended."),
        );
        self.set_lexical_environment_suspended(true);
    }

    fn resume_lexical_environment(&self) {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot modify the lexical environment during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the lexical environment after transformation has completed."),
        );
        Debug_.assert(
            self.lexical_environment_suspended(),
            Some("Lexical environment is not suspended."),
        );
        self.set_lexical_environment_suspended(false);
    }

    fn end_lexical_environment(&self) -> Option<Vec<Id<Node /*Statement*/>>> {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot modify the lexical environment during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the lexical environment after transformation has completed."),
        );
        Debug_.assert(
            !self.lexical_environment_suspended(),
            Some("Lexical environment is suspended."),
        );

        let mut statements: Option<Vec<Id<Node>>> = None;
        let mut lexical_environment_variable_declarations =
            self.lexical_environment_variable_declarations();
        let mut lexical_environment_function_declarations =
            self.lexical_environment_function_declarations();
        let mut lexical_environment_statements = self.lexical_environment_statements();
        if lexical_environment_variable_declarations.is_some()
            || lexical_environment_function_declarations.is_some()
            || lexical_environment_statements.is_some()
        {
            if let Some(lexical_environment_function_declarations) =
                lexical_environment_function_declarations.as_ref()
            {
                statements = Some(lexical_environment_function_declarations.clone());
            }

            if let Some(lexical_environment_variable_declarations) =
                lexical_environment_variable_declarations.as_ref()
            {
                let statement = self.factory.create_variable_statement(
                    Option::<Gc<NodeArray>>::None,
                    self.factory.create_variable_declaration_list(
                        lexical_environment_variable_declarations.clone(),
                        None,
                    ),
                );

                let statement = set_emit_flags(statement, EmitFlags::CustomPrologue, self);

                if statements.is_none() {
                    statements = Some(vec![statement]);
                } else {
                    statements.as_mut().unwrap().push(statement);
                }
            }

            if let Some(lexical_environment_statements) = lexical_environment_statements.as_ref() {
                if statements.is_none() {
                    statements = Some(lexical_environment_statements.clone());
                } else {
                    statements
                        .as_mut()
                        .unwrap()
                        .append(&mut lexical_environment_statements.clone());
                }
            }
        }

        // lexicalEnvironmentStackOffset--;
        *lexical_environment_variable_declarations = self
            .lexical_environment_variable_declarations_stack()
            .pop()
            .unwrap();
        *lexical_environment_function_declarations = self
            .lexical_environment_function_declarations_stack()
            .pop()
            .unwrap();
        *lexical_environment_statements =
            self.lexical_environment_statements_stack().pop().unwrap();
        self.set_lexical_environment_flags_(self.lexical_environment_flags_stack().pop().unwrap());
        // if (lexicalEnvironmentStackOffset === 0) {
        //   lexicalEnvironmentVariableDeclarationsStack = [];
        //   lexicalEnvironmentFunctionDeclarationsStack = [];
        //   lexicalEnvironmentStatementsStack = [];
        //   lexicalEnvironmentFlagsStack = [];
        // }
        statements
    }

    fn hoist_function_declaration(&self, func: Id<Node> /*FunctionDeclaration*/) {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot modify the lexical environment during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the lexical environment after transformation has completed."),
        );
        set_emit_flags(func, EmitFlags::CustomPrologue, self);
        let mut lexical_environment_function_declarations =
            self.lexical_environment_function_declarations();
        if lexical_environment_function_declarations.is_none() {
            *lexical_environment_function_declarations = Some(vec![func]);
        } else {
            lexical_environment_function_declarations
                .as_mut()
                .unwrap()
                .push(func);
        }
    }

    fn hoist_variable_declaration(&self, name: Id<Node> /*Identifier*/) {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot modify the lexical environment during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the lexical environment after transformation has completed."),
        );
        let decl = set_emit_flags(
            self.factory()
                .create_variable_declaration(Some(name), None, None, None),
            EmitFlags::NoNestedSourceMaps,
            self,
        );
        let mut lexical_environment_variable_declarations =
            self.lexical_environment_variable_declarations();
        if lexical_environment_variable_declarations.is_none() {
            *lexical_environment_variable_declarations = Some(vec![decl]);
        } else {
            lexical_environment_variable_declarations
                .as_mut()
                .unwrap()
                .push(decl);
        }
        let lexical_environment_flags = self.lexical_environment_flags();
        if lexical_environment_flags.intersects(LexicalEnvironmentFlags::InParameters) {
            self.set_lexical_environment_flags_(
                lexical_environment_flags | LexicalEnvironmentFlags::VariablesHoistedInParameters,
            );
        }
    }

    fn start_block_scope(&self) {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot start a block scope during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot start a block scope after transformation has completed."),
        );
        let mut block_scoped_variable_declarations_stack =
            self.block_scoped_variable_declarations_stack();
        block_scoped_variable_declarations_stack
            .push(self.maybe_block_scoped_variable_declarations_mut().take());
        self.set_block_scope_stack_offset(self.block_scope_stack_offset() + 1);
        // blockScopedVariableDeclarations = undefined!;
    }

    fn end_block_scope(&self) -> Option<Vec<Id<Node /*Statement*/>>> {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot end a block scope during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot end a block scope after transformation has completed."),
        );
        let statements: Option<Vec<Id<Node>>> = if some(
            self.maybe_block_scoped_variable_declarations().as_deref(),
            Option::<fn(&Id<Node>) -> bool>::None,
        ) {
            Some(vec![self.factory().create_variable_statement(
                Option::<Gc<NodeArray>>::None,
                self.factory().create_variable_declaration_list(
                    self.block_scoped_variable_declarations()
                        .iter()
                        .map(|identifier| {
                            self.factory().create_variable_declaration(
                                Some(identifier.clone()),
                                None,
                                None,
                                None,
                            )
                        })
                        .collect::<Vec<_>>(),
                    Some(NodeFlags::Let),
                ),
            )])
        } else {
            None
        };
        self.set_block_scope_stack_offset(self.block_scope_stack_offset() - 1);
        self.set_block_scoped_variable_declarations(
            self.block_scoped_variable_declarations_stack()
                .pop()
                .unwrap(),
        );
        if self.block_scope_stack_offset() == 0 {
            self.set_block_scoped_variable_declarations_stack(Some(vec![]));
        }
        statements
    }

    fn add_block_scoped_variable(&self, name: Id<Node> /*Identifier*/) {
        Debug_.assert(
            self.block_scope_stack_offset() > 0,
            Some("Cannot add a block scoped variable outside of an iteration body."),
        );
        self.maybe_block_scoped_variable_declarations_mut()
            .get_or_insert_default_()
            .push(name);
    }

    fn add_initialization_statement(&self, node: Id<Node> /*Statement*/) {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot modify the lexical environment during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the lexical environment after transformation has completed."),
        );
        set_emit_flags(node, EmitFlags::CustomPrologue, self);
        let mut lexical_environment_statements = self.lexical_environment_statements();
        if lexical_environment_statements.is_none() {
            *lexical_environment_statements = Some(vec![node]);
        } else {
            lexical_environment_statements
                .as_mut()
                .unwrap()
                .push(node);
        }
    }
}

impl TransformationContext for TransformNodesTransformationResult {
    fn get_emit_resolver(&self) -> Gc<Box<dyn EmitResolver>> {
        self.resolver.clone().unwrap()
    }

    fn get_emit_host(&self) -> Id<Box<dyn EmitHost>> {
        self.host.clone().unwrap()
    }

    fn get_emit_helper_factory(&self) -> Gc<EmitHelperFactory> {
        /*memoize(*/
        let mut created_emit_helper_factory = self.created_emit_helper_factory();
        if created_emit_helper_factory.is_none() {
            *created_emit_helper_factory = Some(Gc::new(create_emit_helper_factory(
                self.arena_id(),
                self,
            )));
        }
        created_emit_helper_factory.clone().unwrap()
    }

    fn request_emit_helper(&self, helper: Id<EmitHelper>) {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot modify the transformation context during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the transformation context after transformation has completed."),
        );
        Debug_.assert(
            !helper.ref_(self).scoped(),
            Some("Cannot request a scoped emit helper."),
        );
        if let Some(helper_dependencies) = helper.ref_(self).dependencies() {
            for h in helper_dependencies {
                self.request_emit_helper(h.clone());
            }
        }
        let mut emit_helpers = self.emit_helpers_mut();
        if emit_helpers.is_none() {
            *emit_helpers = Some(vec![]);
        }
        append(emit_helpers.as_mut().unwrap(), Some(helper));
    }

    fn read_emit_helpers(&self) -> Option<Vec<Id<EmitHelper>>> {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot modify the transformation context during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the transformation context after transformation has completed."),
        );
        let helpers = self.emit_helpers();
        self.set_emit_helpers(None);
        helpers
    }

    fn enable_substitution(&self, kind: SyntaxKind) {
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the transformation context after transformation has completed."),
        );
        let mut enabled_syntax_kind_features = self.enabled_syntax_kind_features();
        let entry = enabled_syntax_kind_features
            .entry(kind)
            .or_insert(SyntaxKindFeatureFlags::None);
        *entry |= SyntaxKindFeatureFlags::Substitution;
    }

    fn is_substitution_enabled(&self, node: Id<Node>) -> bool {
        matches!(
            self.enabled_syntax_kind_features().get(&node.ref_(self).kind()),
            Some(syntax_kind_feature_flags) if syntax_kind_feature_flags.intersects(SyntaxKindFeatureFlags::Substitution)
        ) && !get_emit_flags(&node.ref_(self)).intersects(EmitFlags::NoSubstitution)
    }

    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        self.on_substitute_node_outermost_override_or_original_method
            .borrow()
            .on_substitute_node(hint, node)
    }

    fn override_on_substitute_node(
        &self,
        overrider: &mut dyn FnMut(
            Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
        )
            -> Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) {
        let mut on_substitute_node_outermost_override_or_original_method = self
            .on_substitute_node_outermost_override_or_original_method
            .borrow_mut();
        let previous_on_substitute_node =
            on_substitute_node_outermost_override_or_original_method.clone();
        *on_substitute_node_outermost_override_or_original_method =
            overrider(previous_on_substitute_node.clone());
        *self
            .on_substitute_node_previous_override_or_original_method
            .borrow_mut() = Some(previous_on_substitute_node);
    }

    fn pop_overridden_on_substitute_node(
        &self,
    ) -> Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>> {
        let previous = self.on_substitute_node_previous_override_or_original_method.borrow().clone().expect("Should only call .pop_overridden_on_substitute_node() when there's currently an override");
        let current = self
            .on_substitute_node_outermost_override_or_original_method
            .borrow()
            .clone();
        *self
            .on_substitute_node_outermost_override_or_original_method
            .borrow_mut() = previous;
        current
    }

    fn enable_emit_notification(&self, kind: SyntaxKind) {
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the transformation context after transformation has completed."),
        );
        let mut enabled_syntax_kind_features = self.enabled_syntax_kind_features();
        let entry = enabled_syntax_kind_features
            .entry(kind)
            .or_insert(SyntaxKindFeatureFlags::None);
        *entry |= SyntaxKindFeatureFlags::EmitNotifications;
    }

    fn is_emit_notification_enabled(&self, node: Id<Node>) -> bool {
        matches!(
            self.enabled_syntax_kind_features().get(&node.ref_(self).kind()),
            Some(syntax_kind_feature_flags) if syntax_kind_feature_flags.intersects(SyntaxKindFeatureFlags::EmitNotifications)
        ) || get_emit_flags(&node.ref_(self)).intersects(EmitFlags::AdviseOnEmitNode)
    }

    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        self.on_emit_node_outermost_override_or_original_method
            .borrow()
            .on_emit_node(hint, node, emit_callback)
    }

    fn override_on_emit_node(
        &self,
        overrider: &mut dyn FnMut(
            Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
        ) -> Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) {
        let mut on_emit_node_outermost_override_or_original_method = self
            .on_emit_node_outermost_override_or_original_method
            .borrow_mut();
        let previous_on_emit_node = on_emit_node_outermost_override_or_original_method.clone();
        *on_emit_node_outermost_override_or_original_method =
            overrider(previous_on_emit_node.clone());
        *self
            .on_emit_node_previous_override_or_original_method
            .borrow_mut() = Some(previous_on_emit_node);
    }

    fn pop_overridden_on_emit_node(&self) -> Gc<Box<dyn TransformationContextOnEmitNodeOverrider>> {
        let previous = self.on_emit_node_previous_override_or_original_method.borrow().clone().expect("Should only call .pop_overridden_on_emit_node() when there's currently an override");
        let current = self
            .on_emit_node_outermost_override_or_original_method
            .borrow()
            .clone();
        *self
            .on_emit_node_outermost_override_or_original_method
            .borrow_mut() = previous;
        current
    }

    fn add_diagnostic(&self, diag: Id<Diagnostic /*DiagnosticWithLocation*/>) {
        self.diagnostics().push(diag);
    }
}

impl HasArena for TransformNodesTransformationResult {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct NoEmitNotificationTransformationContextOnEmitNodeOverrider;

impl TransformationContextOnEmitNodeOverrider
    for NoEmitNotificationTransformationContextOnEmitNodeOverrider
{
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        no_emit_notification(hint, node, emit_callback)
    }
}

#[derive(Trace, Finalize)]
struct NoEmitNotificationTransformationContextOnSubstituteNodeOverrider;

impl TransformationContextOnSubstituteNodeOverrider
    for NoEmitNotificationTransformationContextOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(no_emit_substitution(hint, node))
    }
}

impl TransformationResult for TransformNodesTransformationResult {
    fn transformed(&self) -> Vec<Id<Node>> {
        self.transformed.borrow().clone()
    }

    fn diagnostics(&self) -> Option<Vec<Id<Diagnostic /*DiagnosticWithLocation*/>>> {
        Some(self.diagnostics.borrow().clone())
    }

    fn substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        Debug_.assert(
            self.state() < TransformationState::Disposed,
            Some("Cannot substitute a node after the result is disposed."),
        );
        /*node &&*/
        Ok(if self.is_substitution_enabled(node) {
            self.on_substitute_node(hint, node)?
        } else {
            node
        })
    }

    fn emit_node_with_notification(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        Debug_.assert(
            self.state() < TransformationState::Disposed,
            Some("Cannot invoke TransformationResult callbacks after the result is disposed."),
        );
        // if (node) {
        if TransformationResult::is_emit_notification_enabled(self, node).unwrap_or(false) {
            self.on_emit_node(hint, node, emit_callback)?;
        } else {
            emit_callback(hint, node)?;
        }
        // }

        Ok(())
    }

    fn is_emit_notification_enabled(&self, node: Id<Node>) -> Option<bool> {
        Some(TransformationContext::is_emit_notification_enabled(
            self, node,
        ))
    }

    fn dispose(&self) {
        if self.state() < TransformationState::Disposed {
            for &node in &self.nodes {
                dispose_emit_nodes(maybe_get_source_file_of_node(get_parse_tree_node(
                    Some(node),
                    Option::<fn(Id<Node>) -> bool>::None,
                    self,
                ), self), self)
            }

            self.set_lexical_environment_variable_declarations(None);
            self.set_lexical_environment_variable_declarations_stack(None);
            self.set_lexical_environment_function_declarations(None);
            self.set_lexical_environment_function_declarations_stack(None);
            // onSubstituteNode = undefined!;
            // onEmitNode = undefined!;
            self.set_emit_helpers(None);

            self.set_state(TransformationState::Disposed);
        }
    }
}

lazy_static! {
    pub static ref null_transformation_context: TransformationContextNull =
        TransformationContextNull::new();
}

#[derive(Trace, Finalize)]
pub struct TransformationContextNull {}

impl TransformationContextNull {
    pub fn new() -> Self {
        Self {}
    }
}

impl CoreTransformationContext for TransformationContextNull {
    fn factory(&self) -> Id<NodeFactory> {
        get_factory()
    }

    fn get_compiler_options(&self) -> Id<CompilerOptions> {
        self.alloc_compiler_options(Default::default())
    }

    fn start_lexical_environment(&self) {}

    fn set_lexical_environment_flags(&self, _flags: LexicalEnvironmentFlags, _value: bool) {}

    fn get_lexical_environment_flags(&self) -> LexicalEnvironmentFlags {
        LexicalEnvironmentFlags::None
    }

    fn suspend_lexical_environment(&self) {}

    fn resume_lexical_environment(&self) {}

    fn end_lexical_environment(&self) -> Option<Vec<Id<Node /*Statement*/>>> {
        None
    }

    fn hoist_function_declaration(&self, _node: Id<Node> /*FunctionDeclaration*/) {}

    fn hoist_variable_declaration(&self, _node: Id<Node> /*Identifier*/) {}

    fn start_block_scope(&self) {}

    fn end_block_scope(&self) -> Option<Vec<Id<Node /*Statement*/>>> {
        None
    }

    fn add_block_scoped_variable(&self, _node: Id<Node> /*Identifier*/) {}

    fn add_initialization_statement(&self, _node: Id<Node> /*Statement*/) {}
}

impl TransformationContext for TransformationContextNull {
    fn get_emit_resolver(&self) -> Gc<Box<dyn EmitResolver>> {
        not_implemented()
    }

    fn get_emit_host(&self) -> Id<Box<dyn EmitHost>> {
        not_implemented()
    }

    fn get_emit_helper_factory(&self) -> Gc<EmitHelperFactory> {
        not_implemented()
    }

    fn request_emit_helper(&self, _helper: Id<EmitHelper>) {}

    fn read_emit_helpers(&self) -> Option<Vec<Id<EmitHelper>>> {
        not_implemented()
    }

    fn enable_substitution(&self, _kind: SyntaxKind) {}

    fn is_substitution_enabled(&self, _node: Id<Node>) -> bool {
        not_implemented()
    }

    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(no_emit_substitution(hint, node))
    }

    fn override_on_substitute_node(
        &self,
        _overrider: &mut dyn FnMut(
            Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
        )
            -> Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) {
        unreachable!("maybe?")
    }

    fn pop_overridden_on_substitute_node(
        &self,
    ) -> Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>> {
        unreachable!("maybe?")
    }

    fn enable_emit_notification(&self, _kind: SyntaxKind) {}

    fn is_emit_notification_enabled(&self, _node: Id<Node>) -> bool {
        not_implemented()
    }

    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        no_emit_notification(hint, node, emit_callback)
    }

    fn override_on_emit_node(
        &self,
        _overrider: &mut dyn FnMut(
            Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
        )
            -> Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) {
        unreachable!("maybe?")
    }

    fn pop_overridden_on_emit_node(&self) -> Gc<Box<dyn TransformationContextOnEmitNodeOverrider>> {
        unreachable!("maybe?")
    }

    fn add_diagnostic(&self, _diag: Id<Diagnostic /*DiagnosticWithLocation*/>) {}
}

impl HasArena for TransformationContextNull {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}
