#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRefMut, Trace};
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use crate::{
    add_range, append, chain_bundle, create_emit_helper_factory, dispose_emit_nodes,
    factory as factory_static, get_emit_flags, get_emit_module_kind, get_emit_script_target,
    get_jsx_transform_enabled, get_parse_tree_node, get_source_file_of_node, is_bundle,
    is_source_file, maybe_map, not_implemented, set_emit_flags, some, synthetic_factory,
    transform_class_fields, transform_declarations, transform_ecmascript_module, transform_es2015,
    transform_es2016, transform_es2017, transform_es2018, transform_es2019, transform_es2020,
    transform_es2021, transform_es5, transform_esnext, transform_generators, transform_jsx,
    transform_module, transform_node_module, transform_system_module, transform_type_script,
    BaseNodeFactorySynthetic, CompilerOptions, CoreTransformationContext, CustomTransformer,
    CustomTransformers, Debug_, Diagnostic, EmitFlags, EmitHelper, EmitHelperBase,
    EmitHelperFactory, EmitHint, EmitHost, EmitResolver, EmitTransformers, LexicalEnvironmentFlags,
    ModuleKind, Node, NodeArray, NodeFactory, NodeFlags, NodeInterface, ScriptTarget, SyntaxKind,
    TransformationContext, TransformationResult, Transformer, TransformerFactory,
    TransformerFactoryOrCustomTransformerFactory,
};

fn get_module_transformer(module_kind: ModuleKind) -> TransformerFactory {
    match module_kind {
        ModuleKind::ESNext | ModuleKind::ES2022 | ModuleKind::ES2020 | ModuleKind::ES2015 => {
            transform_ecmascript_module()
        }
        ModuleKind::System => transform_system_module(),
        ModuleKind::Node12 | ModuleKind::NodeNext => transform_node_module(),
        _ => transform_module(),
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
) -> EmitTransformers {
    EmitTransformers::new(
        get_script_transformers(compiler_options, custom_transformers, emit_only_dts_files),
        get_declaration_transformers(custom_transformers),
    )
}

fn get_script_transformers(
    compiler_options: &CompilerOptions,
    custom_transformers: Option<&CustomTransformers>,
    emit_only_dts_files: Option<bool>,
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
                    wrap_script_transformer_factory(factory.clone())
                })
            })
            .as_deref(),
        None,
        None,
    );

    transformers.push(transform_type_script());
    transformers.push(transform_class_fields());

    if get_jsx_transform_enabled(compiler_options) {
        transformers.push(transform_jsx());
    }

    if language_version < ScriptTarget::ESNext {
        transformers.push(transform_esnext());
    }

    if language_version < ScriptTarget::ES2021 {
        transformers.push(transform_es2021());
    }

    if language_version < ScriptTarget::ES2020 {
        transformers.push(transform_es2020());
    }

    if language_version < ScriptTarget::ES2019 {
        transformers.push(transform_es2019());
    }

    if language_version < ScriptTarget::ES2018 {
        transformers.push(transform_es2018());
    }

    if language_version < ScriptTarget::ES2017 {
        transformers.push(transform_es2017());
    }

    if language_version < ScriptTarget::ES2016 {
        transformers.push(transform_es2016());
    }

    if language_version < ScriptTarget::ES2015 {
        transformers.push(transform_es2015());
        transformers.push(transform_generators());
    }

    transformers.push(get_module_transformer(module_kind));

    if language_version < ScriptTarget::ES5 {
        transformers.push(transform_es5());
    }

    add_range(
        &mut transformers,
        custom_transformers
            .and_then(|custom_transformers| {
                maybe_map(custom_transformers.after.as_ref(), |factory, _| {
                    wrap_script_transformer_factory(factory.clone())
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
) -> Vec<TransformerFactory> {
    let mut transformers: Vec<TransformerFactory> = vec![];
    transformers.push(transform_declarations());
    add_range(
        &mut transformers,
        custom_transformers
            .and_then(|custom_transformers| {
                maybe_map(
                    custom_transformers.after_declarations.as_ref(),
                    |factory, _| wrap_declaration_transformer_factory(factory.clone()),
                )
            })
            .as_deref(),
        None,
        None,
    );
    transformers
}

fn wrap_custom_transformer(transformer: Rc<dyn CustomTransformer>) -> Transformer /*<Bundle | SourceFile>*/
{
    let wrapped_transformer = move |node: &Node| -> Gc<Node> {
        if is_bundle(node) {
            transformer.transform_bundle(node)
        } else {
            transformer.transform_source_file(node)
        }
    };
    Rc::new(wrapped_transformer)
}

fn wrap_custom_transformer_factory(
    transformer: Rc<TransformerFactoryOrCustomTransformerFactory>,
    handle_default: fn(Rc<dyn TransformationContext>, Transformer) -> Transformer, /*<SourceFile | Bundle>*/
) -> TransformerFactory /*<SourceFile | Bundle>*/ {
    let factory = move |context: Rc<dyn TransformationContext>| match &*transformer.clone() {
        TransformerFactoryOrCustomTransformerFactory::TransformerFactory(transformer) => {
            let custom_transformer = transformer(context.clone());
            handle_default(context.clone(), custom_transformer)
        }
        TransformerFactoryOrCustomTransformerFactory::CustomTransformerFactory(transformer) => {
            let custom_transformer = transformer(context.clone());
            wrap_custom_transformer(custom_transformer)
        }
    };
    Rc::new(factory)
}

fn wrap_script_transformer_factory(
    transformer: Rc<TransformerFactoryOrCustomTransformerFactory /*<SourceFile>*/>,
) -> TransformerFactory /*<Bundle | SourceFile>*/ {
    wrap_custom_transformer_factory(transformer, chain_bundle)
}

fn wrap_declaration_transformer_factory(
    transformer: Rc<TransformerFactoryOrCustomTransformerFactory /*<Bundle | SourceFile>*/>,
) -> TransformerFactory /*<Bundle | SourceFile>*/ {
    wrap_custom_transformer_factory(transformer, passthrough_transformer)
}

fn passthrough_transformer(
    _context: Rc<dyn TransformationContext>,
    transform_source_file: Transformer,
) -> Transformer {
    transform_source_file
}

pub fn no_emit_substitution(_hint: EmitHint, node: &Node) -> Gc<Node> {
    node.node_wrapper()
}

pub fn no_emit_notification(hint: EmitHint, node: &Node, callback: &dyn Fn(EmitHint, &Node)) {
    callback(hint, node)
}

pub fn transform_nodes(
    resolver: Option<Rc<dyn EmitResolver>>,
    host: Option<Rc<dyn EmitHost>>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    options: Gc<CompilerOptions>,
    nodes: &[Gc<Node>],
    transformers: &[TransformerFactory],
    allow_dts_files: bool,
    // ) -> impl TransformationResult {
) -> Rc<dyn TransformationResult> {
    let mut lexical_environment_variable_declarations: Option<Vec<Gc<Node>>> = None;
    let mut lexical_environment_function_declarations: Option<Vec<Gc<Node>>> = None;
    let mut lexical_environment_variable_declarations_stack: Vec<Option<Vec<Gc<Node>>>> = vec![];
    let mut lexical_environment_function_declarations_stack: Vec<Option<Vec<Gc<Node>>>> = vec![];
    let mut emit_helpers: Option<Vec<Gc<EmitHelper>>> = None;
    let state = TransformationState::Uninitialized;
    let diagnostics: Vec<Gc<Diagnostic /*DiagnosticWithLocation*/>> = vec![];
    let mut transformed: Vec<Gc<Node>> = vec![];

    let transformation_result = TransformNodesTransformationResult::new(
        transformed,
        state,
        nodes.to_owned(),
        lexical_environment_variable_declarations,
        lexical_environment_function_declarations,
        lexical_environment_variable_declarations_stack,
        lexical_environment_function_declarations_stack,
        emit_helpers,
        diagnostics,
        transformers.to_owned(),
        allow_dts_files,
        options,
        resolver,
        host,
        factory,
    );
    transformation_result.call();
    transformation_result
}

// #[derive(Trace, Finalize)]
pub struct TransformNodesTransformationResult {
    transformed: GcCell<Vec<Gc<Node>>>,
    // #[unsafe_ignore_trace]
    state: Cell<TransformationState>,
    nodes: Vec<Gc<Node>>,
    // rc_wrapper: GcCell<Option<Gc<TransformNodesTransformationResult>>>,
    rc_wrapper: RefCell<Option<Weak<TransformNodesTransformationResult>>>,
    // #[unsafe_ignore_trace]
    enabled_syntax_kind_features: RefCell<HashMap<SyntaxKind, SyntaxKindFeatureFlags>>,
    lexical_environment_variable_declarations:
        GcCell<Option<Vec<Gc<Node /*VariableDeclaration*/>>>>,
    lexical_environment_function_declarations:
        GcCell<Option<Vec<Gc<Node /*FunctionDeclaration*/>>>>,
    lexical_environment_statements: GcCell<Option<Vec<Gc<Node /*Statement*/>>>>,
    // #[unsafe_ignore_trace]
    lexical_environment_flags: Cell<LexicalEnvironmentFlags>,
    lexical_environment_variable_declarations_stack:
        GcCell<Option<Vec<Option<Vec<Gc<Node /*VariableDeclaration*/>>>>>>,
    lexical_environment_function_declarations_stack:
        GcCell<Option<Vec<Option<Vec<Gc<Node /*FunctionDeclaration*/>>>>>>,
    lexical_environment_statements_stack: GcCell<Option<Vec<Option<Vec<Gc<Node /*Statement*/>>>>>>,
    // #[unsafe_ignore_trace]
    lexical_environment_flags_stack: RefCell<Option<Vec<LexicalEnvironmentFlags>>>,
    // #[unsafe_ignore_trace]
    lexical_environment_suspended: Cell<bool>,
    block_scoped_variable_declarations_stack:
        GcCell<Option<Vec<Option<Vec<Gc<Node /*Identifier*/>>>>>>,
    // #[unsafe_ignore_trace]
    block_scope_stack_offset: Cell<usize>,
    block_scoped_variable_declarations: GcCell<Option<Vec<Gc<Node /*Identifier*/>>>>,
    emit_helpers: GcCell<Option<Vec<Gc<EmitHelper>>>>,
    diagnostics: GcCell<Vec<Gc<Diagnostic>>>,
    transformers: Vec<TransformerFactory>,
    // TODO: this should become trace-able, just didn't want to do all the transformer stuff right
    // now
    // #[unsafe_ignore_trace]
    transformers_with_context: RefCell<Option<Vec<Transformer>>>,
    allow_dts_files: bool,
    options: Gc<CompilerOptions>,
    // resolver: Option<Gc<Box<dyn EmitResolver>>>,
    resolver: Option<Rc<dyn EmitResolver>>,
    // host: Option<Gc<Box<dyn EmitHost>>>,
    host: Option<Rc<dyn EmitHost>>,
    // #[unsafe_ignore_trace]
    created_emit_helper_factory: RefCell<Option<Rc<EmitHelperFactory>>>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
}

impl TransformNodesTransformationResult {
    pub fn new(
        transformed: Vec<Gc<Node>>,
        state: TransformationState,
        nodes: Vec<Gc<Node>>,
        lexical_environment_variable_declarations: Option<Vec<Gc<Node>>>,
        lexical_environment_function_declarations: Option<Vec<Gc<Node>>>,
        lexical_environment_variable_declarations_stack: Vec<Option<Vec<Gc<Node>>>>,
        lexical_environment_function_declarations_stack: Vec<Option<Vec<Gc<Node>>>>,
        emit_helpers: Option<Vec<Gc<EmitHelper>>>,
        diagnostics: Vec<Gc<Diagnostic>>,
        transformers: Vec<TransformerFactory>,
        allow_dts_files: bool,
        options: Gc<CompilerOptions>,
        resolver: Option<Rc<dyn EmitResolver>>,
        host: Option<Rc<dyn EmitHost>>,
        factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    ) -> Rc<Self> {
        let rc = Rc::new(Self {
            transformed: GcCell::new(transformed),
            state: Cell::new(state),
            nodes,
            rc_wrapper: Default::default(),
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
        rc.set_rc_wrapper(rc.clone());
        rc
    }

    fn state(&self) -> TransformationState {
        self.state.get()
    }

    fn set_rc_wrapper(&self, rc: Rc<TransformNodesTransformationResult>) {
        *self.rc_wrapper.borrow_mut() = Some(Rc::downgrade(&rc));
    }

    fn rc_wrapper(&self) -> Rc<TransformNodesTransformationResult> {
        self.rc_wrapper.borrow().clone().unwrap().upgrade().unwrap()
    }

    fn lexical_environment_variable_declarations(&self) -> GcCellRefMut<Option<Vec<Gc<Node>>>> {
        self.lexical_environment_variable_declarations.borrow_mut()
    }

    fn set_lexical_environment_variable_declarations(
        &self,
        lexical_environment_variable_declarations: Option<Vec<Gc<Node>>>,
    ) {
        *self.lexical_environment_variable_declarations.borrow_mut() =
            lexical_environment_variable_declarations;
    }

    fn lexical_environment_function_declarations(&self) -> GcCellRefMut<Option<Vec<Gc<Node>>>> {
        self.lexical_environment_function_declarations.borrow_mut()
    }

    fn set_lexical_environment_function_declarations(
        &self,
        lexical_environment_function_declarations: Option<Vec<Gc<Node>>>,
    ) {
        *self.lexical_environment_function_declarations.borrow_mut() =
            lexical_environment_function_declarations;
    }

    fn lexical_environment_statements(&self) -> GcCellRefMut<Option<Vec<Gc<Node>>>> {
        self.lexical_environment_statements.borrow_mut()
    }

    fn set_lexical_environment_statements(
        &self,
        lexical_environment_statements: Option<Vec<Gc<Node>>>,
    ) {
        *self.lexical_environment_statements.borrow_mut() = lexical_environment_statements;
    }

    fn lexical_environment_variable_declarations_stack(
        &self,
    ) -> GcCellRefMut<Option<Vec<Option<Vec<Gc<Node>>>>>, Vec<Option<Vec<Gc<Node>>>>> {
        GcCellRefMut::map(
            self.lexical_environment_variable_declarations_stack
                .borrow_mut(),
            |option| option.as_mut().unwrap(),
        )
    }

    fn set_lexical_environment_variable_declarations_stack(
        &self,
        lexical_environment_variable_declarations_stack: Option<Vec<Option<Vec<Gc<Node>>>>>,
    ) {
        *self
            .lexical_environment_variable_declarations_stack
            .borrow_mut() = lexical_environment_variable_declarations_stack;
    }

    fn lexical_environment_function_declarations_stack(
        &self,
    ) -> GcCellRefMut<Option<Vec<Option<Vec<Gc<Node>>>>>, Vec<Option<Vec<Gc<Node>>>>> {
        GcCellRefMut::map(
            self.lexical_environment_function_declarations_stack
                .borrow_mut(),
            |option| option.as_mut().unwrap(),
        )
    }

    fn set_lexical_environment_function_declarations_stack(
        &self,
        lexical_environment_function_declarations_stack: Option<Vec<Option<Vec<Gc<Node>>>>>,
    ) {
        *self
            .lexical_environment_function_declarations_stack
            .borrow_mut() = lexical_environment_function_declarations_stack;
    }

    fn lexical_environment_statements_stack(
        &self,
    ) -> GcCellRefMut<Option<Vec<Option<Vec<Gc<Node>>>>>, Vec<Option<Vec<Gc<Node>>>>> {
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
    ) -> GcCellRefMut<Option<Vec<Option<Vec<Gc<Node>>>>>, Vec<Option<Vec<Gc<Node>>>>> {
        GcCellRefMut::map(
            self.block_scoped_variable_declarations_stack.borrow_mut(),
            |option| option.as_mut().unwrap(),
        )
    }

    fn set_block_scoped_variable_declarations_stack(
        &self,
        block_scoped_variable_declarations_stack: Option<Vec<Option<Vec<Gc<Node>>>>>,
    ) {
        *self.block_scoped_variable_declarations_stack.borrow_mut() =
            block_scoped_variable_declarations_stack;
    }

    fn block_scoped_variable_declarations(&self) -> GcCellRefMut<Option<Vec<Gc<Node>>>> {
        self.block_scoped_variable_declarations.borrow_mut()
    }

    fn set_block_scoped_variable_declarations(
        &self,
        block_scoped_variable_declarations: Option<Vec<Gc<Node>>>,
    ) {
        *self.block_scoped_variable_declarations.borrow_mut() = block_scoped_variable_declarations;
    }

    fn emit_helpers(&self) -> Option<Vec<Gc<EmitHelper>>> {
        self.emit_helpers.borrow().clone()
    }

    fn emit_helpers_mut(&self) -> GcCellRefMut<Option<Vec<Gc<EmitHelper>>>> {
        self.emit_helpers.borrow_mut()
    }

    fn set_emit_helpers(&self, emit_helpers: Option<Vec<Gc<EmitHelper>>>) {
        *self.emit_helpers.borrow_mut() = emit_helpers;
    }

    fn set_state(&self, state: TransformationState) {
        self.state.set(state);
    }

    fn diagnostics(&self) -> GcCellRefMut<Vec<Gc<Diagnostic>>> {
        self.diagnostics.borrow_mut()
    }

    fn transformers_with_context(&self) -> Ref<Vec<Transformer>> {
        Ref::map(self.transformers_with_context.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    fn set_transformers_with_context(&self, transformers_with_context: Vec<Transformer>) {
        *self.transformers_with_context.borrow_mut() = Some(transformers_with_context);
    }

    fn transformed(&self) -> GcCellRefMut<Vec<Gc<Node>>> {
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

    fn created_emit_helper_factory(&self) -> RefMut<Option<Rc<EmitHelperFactory>>> {
        self.created_emit_helper_factory.borrow_mut()
    }

    fn call(&self) {
        for node in &self.nodes {
            dispose_emit_nodes(get_source_file_of_node(get_parse_tree_node(
                Some(&**node),
                Option::<fn(&Node) -> bool>::None,
            )))
        }

        // performance.mark("beforeTransform");

        self.set_transformers_with_context(
            self.transformers
                .iter()
                .map(|t| t(self.rc_wrapper()))
                .collect(),
        );

        self.set_state(TransformationState::Initialized);

        for node in &self.nodes {
            // tracing?.push(tracing.Phase.Emit, "transformNodes", node.kind === SyntaxKind.SourceFile ? { path: (node as any as SourceFile).path } : { kind: node.kind, pos: node.pos, end: node.end });}
            self.transformed().push(if self.allow_dts_files {
                self.transformation(node)
            } else {
                self.transform_root(node)
            });
            // tracing?.pop();
        }

        self.set_state(TransformationState::Completed);

        // performance.mark("afterTransform");
        // performance.measure("transformTime", "beforeTransform", "afterTransform");
    }

    fn transformation(&self, node: &Node) -> Gc<Node> {
        let mut node = node.node_wrapper();
        for transform in self.transformers_with_context().iter() {
            node = transform(&node);
        }
        node
    }

    fn transform_root(&self, node: &Node) -> Gc<Node> {
        if
        /*node &&*/
        !is_source_file(node) || !node.as_source_file().is_declaration_file() {
            self.transformation(node)
        } else {
            node.node_wrapper()
        }
    }
}

impl CoreTransformationContext<BaseNodeFactorySynthetic> for TransformNodesTransformationResult {
    fn factory(&self) -> Gc<NodeFactory<BaseNodeFactorySynthetic>> {
        self.factory.clone()
    }

    fn get_compiler_options(&self) -> Gc<CompilerOptions> {
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

    fn end_lexical_environment(&self) -> Option<Vec<Gc<Node /*Statement*/>>> {
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

        let mut statements: Option<Vec<Gc<Node>>> = None;
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
                let statement = synthetic_factory
                    .with(|synthetic_factory_| {
                        self.factory.create_variable_statement(
                            synthetic_factory_,
                            Option::<NodeArray>::None,
                            Into::<Gc<Node>>::into(self.factory.create_variable_declaration_list(
                                synthetic_factory_,
                                lexical_environment_variable_declarations.clone(),
                                None,
                            )),
                        )
                    })
                    .into();

                let statement = set_emit_flags(statement, EmitFlags::CustomPrologue);

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

    fn hoist_function_declaration(&self, func: &Node /*FunctionDeclaration*/) {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot modify the lexical environment during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the lexical environment after transformation has completed."),
        );
        set_emit_flags(func.node_wrapper(), EmitFlags::CustomPrologue);
        let mut lexical_environment_function_declarations =
            self.lexical_environment_function_declarations();
        if lexical_environment_function_declarations.is_none() {
            *lexical_environment_function_declarations = Some(vec![func.node_wrapper()]);
        } else {
            lexical_environment_function_declarations
                .as_mut()
                .unwrap()
                .push(func.node_wrapper());
        }
    }

    fn hoist_variable_declaration(&self, name: &Node /*Identifier*/) {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot modify the lexical environment during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the lexical environment after transformation has completed."),
        );
        let decl = set_emit_flags(
            synthetic_factory.with(|synthetic_factory_| {
                self.factory()
                    .create_variable_declaration(
                        synthetic_factory_,
                        Some(name.node_wrapper()),
                        None,
                        None,
                        None,
                    )
                    .into()
            }),
            EmitFlags::NoNestedSourceMaps,
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
            .push(self.block_scoped_variable_declarations().take());
        self.set_block_scope_stack_offset(self.block_scope_stack_offset() + 1);
        // blockScopedVariableDeclarations = undefined!;
    }

    fn end_block_scope(&self) -> Option<Vec<Gc<Node /*Statement*/>>> {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot end a block scope during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot end a block scope after transformation has completed."),
        );
        let block_scoped_variable_declarations = self.block_scoped_variable_declarations();
        let statements: Option<Vec<Gc<Node>>> = if some(
            block_scoped_variable_declarations.as_deref(),
            Option::<fn(&Gc<Node>) -> bool>::None,
        ) {
            Some(vec![synthetic_factory.with(|synthetic_factory_| {
                self.factory()
                    .create_variable_statement(
                        synthetic_factory_,
                        Option::<NodeArray>::None,
                        Into::<Gc<Node>>::into(
                            self.factory().create_variable_declaration_list(
                                synthetic_factory_,
                                block_scoped_variable_declarations
                                    .as_ref()
                                    .unwrap()
                                    .iter()
                                    .map(|identifier| {
                                        self.factory()
                                            .create_variable_declaration(
                                                synthetic_factory_,
                                                Some(identifier.clone()),
                                                None,
                                                None,
                                                None,
                                            )
                                            .into()
                                    })
                                    .collect::<Vec<Gc<Node>>>(),
                                Some(NodeFlags::Let),
                            ),
                        ),
                    )
                    .into()
            })])
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

    fn add_block_scoped_variable(&self, name: &Node /*Identifier*/) {
        Debug_.assert(
            self.block_scope_stack_offset() > 0,
            Some("Cannot add a block scoped variable outside of an iteration body."),
        );
        let mut block_scoped_variable_declarations = self.block_scoped_variable_declarations();
        if block_scoped_variable_declarations.is_none() {
            *block_scoped_variable_declarations = Some(vec![]);
        }
        block_scoped_variable_declarations
            .as_mut()
            .unwrap()
            .push(name.node_wrapper());
    }

    fn add_initialization_statement(&self, node: &Node /*Statement*/) {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot modify the lexical environment during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the lexical environment after transformation has completed."),
        );
        set_emit_flags(node.node_wrapper(), EmitFlags::CustomPrologue);
        let mut lexical_environment_statements = self.lexical_environment_statements();
        if lexical_environment_statements.is_none() {
            *lexical_environment_statements = Some(vec![node.node_wrapper()]);
        } else {
            lexical_environment_statements
                .as_mut()
                .unwrap()
                .push(node.node_wrapper());
        }
    }
}

impl TransformationContext for TransformNodesTransformationResult {
    fn get_emit_resolver(&self) -> Rc<dyn EmitResolver> {
        self.resolver.as_ref().map(Clone::clone).unwrap()
    }

    fn get_emit_host(&self) -> Rc<dyn EmitHost> {
        self.host.as_ref().map(Clone::clone).unwrap()
    }

    fn get_emit_helper_factory(&self) -> Rc<EmitHelperFactory> {
        /*memoize(*/
        let mut created_emit_helper_factory = self.created_emit_helper_factory();
        if created_emit_helper_factory.is_none() {
            *created_emit_helper_factory =
                Some(Rc::new(create_emit_helper_factory(self.rc_wrapper())));
        }
        created_emit_helper_factory
            .as_ref()
            .map(Clone::clone)
            .unwrap()
    }

    fn request_emit_helper(&self, helper: Gc<EmitHelper>) {
        Debug_.assert(
            self.state() > TransformationState::Uninitialized,
            Some("Cannot modify the transformation context during initialization."),
        );
        Debug_.assert(
            self.state() < TransformationState::Completed,
            Some("Cannot modify the transformation context after transformation has completed."),
        );
        Debug_.assert(
            !helper.scoped(),
            Some("Cannot request a scoped emit helper."),
        );
        if let Some(helper_dependencies) = helper.dependencies() {
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

    fn read_emit_helpers(&self) -> Option<Vec<Gc<EmitHelper>>> {
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

    fn is_substitution_enabled(&self, node: &Node) -> bool {
        matches!(self.enabled_syntax_kind_features().get(&node.kind()), Some(syntax_kind_feature_flags) if syntax_kind_feature_flags.intersects(SyntaxKindFeatureFlags::Substitution))
            && !get_emit_flags(node).intersects(EmitFlags::NoSubstitution)
    }

    // TODO: need to support setting of onSubstituteNode?
    fn on_substitute_node(&self, hint: EmitHint, node: &Node) -> Gc<Node> {
        no_emit_substitution(hint, node)
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

    fn is_emit_notification_enabled(&self, node: &Node) -> bool {
        matches!(self.enabled_syntax_kind_features().get(&node.kind()), Some(syntax_kind_feature_flags) if syntax_kind_feature_flags.intersects(SyntaxKindFeatureFlags::EmitNotifications))
            && !get_emit_flags(node).intersects(EmitFlags::AdviseOnEmitNode)
    }

    // TODO: need to support setting of onEmitNode?
    fn on_emit_node(&self, hint: EmitHint, node: &Node, emit_callback: &dyn Fn(EmitHint, &Node)) {
        no_emit_notification(hint, node, emit_callback)
    }

    fn add_diagnostic(&self, diag: Gc<Diagnostic /*DiagnosticWithLocation*/>) {
        self.diagnostics().push(diag);
    }
}

impl TransformationResult for TransformNodesTransformationResult {
    fn transformed(&self) -> Vec<Gc<Node>> {
        self.transformed.borrow().clone()
    }

    fn diagnostics(&self) -> Option<Vec<Gc<Diagnostic /*DiagnosticWithLocation*/>>> {
        Some(self.diagnostics.borrow().clone())
    }

    fn substitute_node(&self, hint: EmitHint, node: &Node) -> Gc<Node> {
        Debug_.assert(
            self.state() < TransformationState::Disposed,
            Some("Cannot substitute a node after the result is disposed."),
        );
        /*node &&*/
        if self.is_substitution_enabled(node) {
            self.on_substitute_node(hint, node)
        } else {
            node.node_wrapper()
        }
    }

    fn emit_node_with_notification(
        &self,
        hint: EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(EmitHint, &Node),
    ) {
        Debug_.assert(
            self.state() < TransformationState::Disposed,
            Some("Cannot invoke TransformationResult callbacks after the result is disposed."),
        );
        // if (node) {
        if TransformationResult::is_emit_notification_enabled(self, node).unwrap_or(false) {
            self.on_emit_node(hint, node, emit_callback);
        } else {
            emit_callback(hint, node);
        }
        // }
    }

    fn is_emit_notification_enabled(&self, node: &Node) -> Option<bool> {
        Some(TransformationContext::is_emit_notification_enabled(
            self, node,
        ))
    }

    fn dispose(&self) {
        if self.state() < TransformationState::Disposed {
            for node in &self.nodes {
                dispose_emit_nodes(get_source_file_of_node(get_parse_tree_node(
                    Some(&**node),
                    Option::<fn(&Node) -> bool>::None,
                )))
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

pub struct TransformationContextNull {}

impl TransformationContextNull {
    pub fn new() -> Self {
        Self {}
    }
}

impl CoreTransformationContext<BaseNodeFactorySynthetic> for TransformationContextNull {
    fn factory(&self) -> Gc<NodeFactory<BaseNodeFactorySynthetic>> {
        factory_static.with(|factory_| factory_.clone())
    }

    fn get_compiler_options(&self) -> Gc<CompilerOptions> {
        Gc::new(Default::default())
    }

    fn start_lexical_environment(&self) {}

    fn set_lexical_environment_flags(&self, _flags: LexicalEnvironmentFlags, _value: bool) {}

    fn get_lexical_environment_flags(&self) -> LexicalEnvironmentFlags {
        LexicalEnvironmentFlags::None
    }

    fn suspend_lexical_environment(&self) {}

    fn resume_lexical_environment(&self) {}

    fn end_lexical_environment(&self) -> Option<Vec<Gc<Node /*Statement*/>>> {
        None
    }

    fn hoist_function_declaration(&self, _node: &Node /*FunctionDeclaration*/) {}

    fn hoist_variable_declaration(&self, _node: &Node /*Identifier*/) {}

    fn start_block_scope(&self) {}

    fn end_block_scope(&self) -> Option<Vec<Gc<Node /*Statement*/>>> {
        None
    }

    fn add_block_scoped_variable(&self, _node: &Node /*Identifier*/) {}

    fn add_initialization_statement(&self, _node: &Node /*Statement*/) {}
}

impl TransformationContext for TransformationContextNull {
    fn get_emit_resolver(&self) -> Rc<dyn EmitResolver> {
        not_implemented()
    }

    fn get_emit_host(&self) -> Rc<dyn EmitHost> {
        not_implemented()
    }

    fn get_emit_helper_factory(&self) -> Rc<EmitHelperFactory> {
        not_implemented()
    }

    fn request_emit_helper(&self, _helper: Gc<EmitHelper>) {}

    fn read_emit_helpers(&self) -> Option<Vec<Gc<EmitHelper>>> {
        not_implemented()
    }

    fn enable_substitution(&self, _kind: SyntaxKind) {}

    fn is_substitution_enabled(&self, _node: &Node) -> bool {
        not_implemented()
    }

    fn on_substitute_node(&self, hint: EmitHint, node: &Node) -> Gc<Node> {
        no_emit_substitution(hint, node)
    }

    fn enable_emit_notification(&self, _kind: SyntaxKind) {}

    fn is_emit_notification_enabled(&self, _node: &Node) -> bool {
        not_implemented()
    }

    fn on_emit_node(&self, hint: EmitHint, node: &Node, emit_callback: &dyn Fn(EmitHint, &Node)) {
        no_emit_notification(hint, node, emit_callback)
    }

    fn add_diagnostic(&self, _diag: Gc<Diagnostic /*DiagnosticWithLocation*/>) {}
}
