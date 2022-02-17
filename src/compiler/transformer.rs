use bitflags::bitflags;
use std::cell::{Cell, RefCell};
use std::rc::{Rc, Weak};

use crate::{
    add_range, chain_bundle, dispose_emit_nodes, factory as factory_static, get_emit_module_kind,
    get_emit_script_target, get_jsx_transform_enabled, get_parse_tree_node,
    get_source_file_of_node, is_bundle, map, transform_class_fields, transform_declarations,
    transform_ecmascript_module, transform_es2015, transform_es2016, transform_es2017,
    transform_es2018, transform_es2019, transform_es2020, transform_es2021, transform_es5,
    transform_esnext, transform_generators, transform_jsx, transform_module, transform_node_module,
    transform_system_module, transform_type_script, BaseNodeFactory, BaseNodeFactorySynthetic,
    CompilerOptions, CoreTransformationContext, CustomTransformer, CustomTransformers, Debug_,
    Diagnostic, EmitHelper, EmitHelperFactory, EmitHint, EmitHost, EmitResolver, EmitTransformers,
    LexicalEnvironmentFlags, ModuleKind, Node, NodeFactory, NodeInterface, ScriptTarget,
    SyntaxKind, TransformationContext, TransformationResult, Transformer, TransformerFactory,
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
                map(custom_transformers.before.as_ref(), |factory, _| {
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
                map(custom_transformers.after.as_ref(), |factory, _| {
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
                map(
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
    let wrapped_transformer = move |node: &Node| -> Rc<Node> {
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

pub fn no_emit_substitution(_hint: EmitHint, node: &Node) -> Rc<Node> {
    node.node_wrapper()
}

pub fn no_emit_notification(
    hint: EmitHint,
    node: &Node,
    callback: &mut dyn FnMut(EmitHint, &Node),
) {
    callback(hint, node)
}

pub fn transform_nodes<
    TResolver: EmitResolver,
    THost: EmitHost,
    TBaseNodeFactory: BaseNodeFactory,
>(
    resolver: Option<&TResolver>,
    host: Option<&THost>,
    factory: Rc<NodeFactory<TBaseNodeFactory>>,
    options: &CompilerOptions,
    nodes: &[Rc<Node>],
    transformers: &[TransformerFactory],
    allow_dts_files: bool,
    // ) -> impl TransformationResult {
) -> Rc<dyn TransformationResult> {
    let mut lexical_environment_variable_declarations: Option<Vec<Rc<Node>>> = None;
    let mut lexical_environment_function_declarations: Option<Vec<Rc<Node>>> = None;
    let mut lexical_environment_variable_declarations_stack: Vec<Option<Vec<Rc<Node>>>> = vec![];
    let mut lexical_environment_function_declarations_stack: Vec<Option<Vec<Rc<Node>>>> = vec![];
    let mut emit_helpers: Option<Vec<Rc<EmitHelper>>> = None;
    let state = TransformationState::Uninitialized;
    let mut transformed: Vec<Rc<Node>> = vec![];
    TransformNodesTransformationResult::new(
        transformed,
        state,
        // TransformNodesTransformationContext::new(),
        nodes.to_owned(),
        lexical_environment_variable_declarations,
        lexical_environment_function_declarations,
        lexical_environment_variable_declarations_stack,
        lexical_environment_function_declarations_stack,
        emit_helpers,
    )
}

// pub struct TransformNodesTransformationContext {}

// impl TransformNodesTransformationContext {
//     fn new() -> Self {
//         Self {}
//     }
// }

// impl CoreTransformationContext<BaseNodeFactorySynthetic> for TransformNodesTransformationContext {
//     fn factory(&self) -> Rc<NodeFactory<BaseNodeFactorySynthetic>> {
//         unimplemented!()
//     }
//     fn get_compiler_options(&self) -> Rc<CompilerOptions> {
//         unimplemented!()
//     }
//     fn start_lexical_environment(&self) {
//         unimplemented!()
//     }
//     fn set_lexical_environment_flags(&self, flags: LexicalEnvironmentFlags, value: bool) {
//         unimplemented!()
//     }
//     fn get_lexical_environment_flags(&self) -> LexicalEnvironmentFlags {
//         unimplemented!()
//     }
//     fn suspend_lexical_environment(&self) {
//         unimplemented!()
//     }
//     fn resume_lexical_environment(&self) {
//         unimplemented!()
//     }
//     fn end_lexical_environment(&self) -> Option<Vec<Rc<Node /*Statement*/>>> {
//         unimplemented!()
//     }
//     fn hoist_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
//         unimplemented!()
//     }
//     fn hoist_variable_declaration(&self, _node: &Node /*Identifier*/) {
//         unimplemented!()
//     }
//     fn start_block_scope(&self) {
//         unimplemented!()
//     }
//     fn end_block_scope(&self) -> Option<Vec<Rc<Node /*Statement*/>>> {
//         unimplemented!()
//     }
//     fn add_block_scoped_variable(&self, node: &Node /*Identifier*/) {
//         unimplemented!()
//     }
//     fn add_initialization_statement(&self, node: &Node /*Statement*/) {
//         unimplemented!()
//     }
// }

// impl TransformationContext for TransformNodesTransformationContext {
//     fn get_emit_resolver(&self) -> Rc<dyn EmitResolver> {
//         unimplemented!()
//     }
//     fn get_emit_host(&self) -> Rc<dyn EmitHost> {
//         unimplemented!()
//     }
//     fn get_emit_helper_factory(&self) -> Rc<dyn EmitHelperFactory> {
//         unimplemented!()
//     }
//     fn request_emit_helper(&self, _helper: Rc<EmitHelper>) {
//         unimplemented!()
//     }
//     fn read_emit_helpers(&self) -> Option<Vec<Rc<EmitHelper>>> {
//         unimplemented!()
//     }
//     fn enable_substitution(&self, _kind: SyntaxKind) {
//         unimplemented!()
//     }
//     fn is_substitution_enabled(&self, _node: &Node) -> bool {
//         unimplemented!()
//     }
//     fn on_substitute_node(&self, hint: EmitHint, node: &Node) -> Rc<Node> {
//         unimplemented!()
//     }
//     fn enable_emit_notification(&self, _kind: SyntaxKind) {
//         unimplemented!()
//     }
//     fn is_emit_notification_enabled(&self, _node: &Node) -> bool {
//         unimplemented!()
//     }
//     fn on_emit_node(
//         &self,
//         hint: EmitHint,
//         node: &Node,
//         emit_callback: &mut dyn FnMut(EmitHint, &Node),
//     ) {
//         unimplemented!()
//     }
//     fn add_diagnostic(&self, _diag: Rc<Diagnostic /*DiagnosticWithLocation*/>) {
//         unimplemented!()
//     }
// }

pub struct TransformNodesTransformationResult {
    transformed: Vec<Rc<Node>>,
    state: Cell<TransformationState>,
    // context: TransformNodesTransformationContext,
    nodes: Vec<Rc<Node>>,
    rc_wrapper: RefCell<Option<Weak<TransformNodesTransformationResult>>>,
    lexical_environment_variable_declarations:
        RefCell<Option<Vec<Rc<Node /*VariableDeclaration*/>>>>,
    lexical_environment_function_declarations:
        RefCell<Option<Vec<Rc<Node /*FunctionDeclaration*/>>>>,
    lexical_environment_variable_declarations_stack:
        RefCell<Option<Vec<Option<Vec<Rc<Node /*VariableDeclaration*/>>>>>>,
    lexical_environment_function_declarations_stack:
        RefCell<Option<Vec<Option<Vec<Rc<Node /*FunctionDeclaration*/>>>>>>,
    emit_helpers: RefCell<Option<Vec<Rc<EmitHelper>>>>,
}

impl TransformNodesTransformationResult {
    pub fn new(
        transformed: Vec<Rc<Node>>,
        state: TransformationState,
        // context: TransformNodesTransformationContext,
        nodes: Vec<Rc<Node>>,
        lexical_environment_variable_declarations: Option<Vec<Rc<Node>>>,
        lexical_environment_function_declarations: Option<Vec<Rc<Node>>>,
        lexical_environment_variable_declarations_stack: Vec<Option<Vec<Rc<Node>>>>,
        lexical_environment_function_declarations_stack: Vec<Option<Vec<Rc<Node>>>>,
        emit_helpers: Option<Vec<Rc<EmitHelper>>>,
    ) -> Rc<Self> {
        let rc = Rc::new(Self {
            transformed,
            state: Cell::new(state),
            // context,
            nodes,
            rc_wrapper: RefCell::new(None),
            lexical_environment_variable_declarations: RefCell::new(
                lexical_environment_variable_declarations,
            ),
            lexical_environment_function_declarations: RefCell::new(
                lexical_environment_function_declarations,
            ),
            lexical_environment_variable_declarations_stack: RefCell::new(Some(
                lexical_environment_variable_declarations_stack,
            )),
            lexical_environment_function_declarations_stack: RefCell::new(Some(
                lexical_environment_function_declarations_stack,
            )),
            emit_helpers: RefCell::new(emit_helpers),
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

    fn set_lexical_environment_variable_declarations(
        &self,
        lexical_environment_variable_declarations: Option<Vec<Rc<Node>>>,
    ) {
        *self.lexical_environment_variable_declarations.borrow_mut() =
            lexical_environment_variable_declarations;
    }

    fn set_lexical_environment_function_declarations(
        &self,
        lexical_environment_function_declarations: Option<Vec<Rc<Node>>>,
    ) {
        *self.lexical_environment_function_declarations.borrow_mut() =
            lexical_environment_function_declarations;
    }

    fn set_lexical_environment_variable_declarations_stack(
        &self,
        lexical_environment_variable_declarations_stack: Option<Vec<Option<Vec<Rc<Node>>>>>,
    ) {
        *self
            .lexical_environment_variable_declarations_stack
            .borrow_mut() = lexical_environment_variable_declarations_stack;
    }

    fn set_lexical_environment_function_declarations_stack(
        &self,
        lexical_environment_function_declarations_stack: Option<Vec<Option<Vec<Rc<Node>>>>>,
    ) {
        *self
            .lexical_environment_function_declarations_stack
            .borrow_mut() = lexical_environment_function_declarations_stack;
    }

    fn set_emit_helpers(&self, emit_helpers: Option<Vec<Rc<EmitHelper>>>) {
        *self.emit_helpers.borrow_mut() = emit_helpers;
    }

    fn set_state(&self, state: TransformationState) {
        self.state.set(state);
    }
}

impl CoreTransformationContext<BaseNodeFactorySynthetic> for TransformNodesTransformationResult {
    fn factory(&self) -> Rc<NodeFactory<BaseNodeFactorySynthetic>> {
        unimplemented!()
    }
    fn get_compiler_options(&self) -> Rc<CompilerOptions> {
        unimplemented!()
    }
    fn start_lexical_environment(&self) {
        unimplemented!()
    }
    fn set_lexical_environment_flags(&self, flags: LexicalEnvironmentFlags, value: bool) {
        unimplemented!()
    }
    fn get_lexical_environment_flags(&self) -> LexicalEnvironmentFlags {
        unimplemented!()
    }
    fn suspend_lexical_environment(&self) {
        unimplemented!()
    }
    fn resume_lexical_environment(&self) {
        unimplemented!()
    }
    fn end_lexical_environment(&self) -> Option<Vec<Rc<Node /*Statement*/>>> {
        unimplemented!()
    }
    fn hoist_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        unimplemented!()
    }
    fn hoist_variable_declaration(&self, _node: &Node /*Identifier*/) {
        unimplemented!()
    }
    fn start_block_scope(&self) {
        unimplemented!()
    }
    fn end_block_scope(&self) -> Option<Vec<Rc<Node /*Statement*/>>> {
        unimplemented!()
    }
    fn add_block_scoped_variable(&self, node: &Node /*Identifier*/) {
        unimplemented!()
    }
    fn add_initialization_statement(&self, node: &Node /*Statement*/) {
        unimplemented!()
    }
}

impl TransformationContext for TransformNodesTransformationResult {
    fn get_emit_resolver(&self) -> Rc<dyn EmitResolver> {
        unimplemented!()
    }
    fn get_emit_host(&self) -> Rc<dyn EmitHost> {
        unimplemented!()
    }
    fn get_emit_helper_factory(&self) -> Rc<dyn EmitHelperFactory> {
        unimplemented!()
    }
    fn request_emit_helper(&self, _helper: Rc<EmitHelper>) {
        unimplemented!()
    }
    fn read_emit_helpers(&self) -> Option<Vec<Rc<EmitHelper>>> {
        unimplemented!()
    }
    fn enable_substitution(&self, _kind: SyntaxKind) {
        unimplemented!()
    }
    fn is_substitution_enabled(&self, _node: &Node) -> bool {
        unimplemented!()
    }
    fn on_substitute_node(&self, hint: EmitHint, node: &Node) -> Rc<Node> {
        unimplemented!()
    }
    fn enable_emit_notification(&self, _kind: SyntaxKind) {
        unimplemented!()
    }
    fn is_emit_notification_enabled(&self, _node: &Node) -> bool {
        unimplemented!()
    }
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: &Node,
        emit_callback: &mut dyn FnMut(EmitHint, &Node),
    ) {
        unimplemented!()
    }
    fn add_diagnostic(&self, _diag: Rc<Diagnostic /*DiagnosticWithLocation*/>) {
        unimplemented!()
    }
}

impl TransformationResult for TransformNodesTransformationResult {
    fn transformed(&self) -> Vec<Rc<Node>> {
        self.transformed.clone()
    }
    fn diagnostics(&self) -> Option<Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>> {
        unimplemented!()
    }
    fn substitute_node(&self, hint: EmitHint, node: &Node) -> Rc<Node> {
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
        emit_callback: &mut dyn FnMut(EmitHint, &Node),
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
    fn factory(&self) -> Rc<NodeFactory<BaseNodeFactorySynthetic>> {
        factory_static.with(|factory_| factory_.clone())
    }
    fn get_compiler_options(&self) -> Rc<CompilerOptions> {
        Rc::new(Default::default())
    }
    fn start_lexical_environment(&self) {}
    fn set_lexical_environment_flags(&self, _flags: LexicalEnvironmentFlags, _value: bool) {}
    fn get_lexical_environment_flags(&self) -> LexicalEnvironmentFlags {
        LexicalEnvironmentFlags::None
    }
    fn suspend_lexical_environment(&self) {}
    fn resume_lexical_environment(&self) {}
    fn end_lexical_environment(&self) -> Option<Vec<Rc<Node /*Statement*/>>> {
        None
    }
    fn hoist_function_declaration(&self, _node: &Node /*FunctionDeclaration*/) {}
    fn hoist_variable_declaration(&self, _node: &Node /*Identifier*/) {}
    fn start_block_scope(&self) {}
    fn end_block_scope(&self) -> Option<Vec<Rc<Node /*Statement*/>>> {
        None
    }
    fn add_block_scoped_variable(&self, _node: &Node /*Identifier*/) {}
    fn add_initialization_statement(&self, _node: &Node /*Statement*/) {}
}

impl TransformationContext for TransformationContextNull {
    fn get_emit_resolver(&self) -> Rc<dyn EmitResolver> {
        unimplemented!()
    }
    fn get_emit_host(&self) -> Rc<dyn EmitHost> {
        unimplemented!()
    }
    fn get_emit_helper_factory(&self) -> Rc<dyn EmitHelperFactory> {
        unimplemented!()
    }
    fn request_emit_helper(&self, _helper: Rc<EmitHelper>) {}
    fn read_emit_helpers(&self) -> Option<Vec<Rc<EmitHelper>>> {
        unimplemented!()
    }
    fn enable_substitution(&self, _kind: SyntaxKind) {}
    fn is_substitution_enabled(&self, _node: &Node) -> bool {
        unimplemented!()
    }
    fn on_substitute_node(&self, hint: EmitHint, node: &Node) -> Rc<Node> {
        no_emit_substitution(hint, node)
    }
    fn enable_emit_notification(&self, _kind: SyntaxKind) {}
    fn is_emit_notification_enabled(&self, _node: &Node) -> bool {
        unimplemented!()
    }
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: &Node,
        emit_callback: &mut dyn FnMut(EmitHint, &Node),
    ) {
        no_emit_notification(hint, node, emit_callback)
    }
    fn add_diagnostic(&self, _diag: Rc<Diagnostic /*DiagnosticWithLocation*/>) {}
}
