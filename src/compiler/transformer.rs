use bitflags::bitflags;
use std::rc::Rc;

use crate::{
    add_range, chain_bundle, get_emit_module_kind, get_emit_script_target,
    get_jsx_transform_enabled, map, transform_class_fields, transform_declarations,
    transform_ecmascript_module, transform_es2015, transform_es2016, transform_es2017,
    transform_es2018, transform_es2019, transform_es2020, transform_es2021, transform_es5,
    transform_esnext, transform_generators, transform_jsx, transform_module, transform_node_module,
    transform_system_module, transform_type_script, BaseNodeFactory, BaseNodeFactorySynthetic,
    CompilerOptions, CoreTransformationContext, CustomTransformers, EmitTransformers,
    LexicalEnvironmentFlags, ModuleKind, Node, NodeFactory, ScriptTarget, TransformationContext,
    Transformer, TransformerFactory, TransformerFactoryOrCustomTransformerFactory,
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum TransformationState {
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
                    wrap_script_transformer_factory(factory)
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
                    wrap_script_transformer_factory(factory)
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
                    |factory, _| wrap_declaration_transformer_factory(factory),
                )
            })
            .as_deref(),
        None,
        None,
    );
    transformers
}

fn wrap_custom_transformer_factory(
    transformer: &TransformerFactoryOrCustomTransformerFactory,
    handle_default: fn(Rc<TransformationContext>, Transformer) -> Transformer, /*<SourceFile | Bundle>*/
) -> TransformerFactory /*<SourceFile | Bundle>*/ {
    unimplemented!()
}

fn wrap_script_transformer_factory(
    transformer: &TransformerFactoryOrCustomTransformerFactory, /*<SourceFile>*/
) -> TransformerFactory /*<Bundle | SourceFile>*/ {
    wrap_custom_transformer_factory(transformer, chain_bundle)
}

fn wrap_declaration_transformer_factory(
    transformer: &TransformerFactoryOrCustomTransformerFactory, /*<Bundle | SourceFile>*/
) -> TransformerFactory /*<Bundle | SourceFile>*/ {
    wrap_custom_transformer_factory(transformer, passthrough_transformer)
}

fn passthrough_transformer<
    TBaseNodeFactory: BaseNodeFactory,
    TContext: CoreTransformationContext<TBaseNodeFactory>,
>(
    _context: Rc<TContext>,
    transform_source_file: Transformer,
) -> Transformer {
    transform_source_file
}

impl CoreTransformationContext<BaseNodeFactorySynthetic> for TransformationContext {
    fn factory(&self) -> &NodeFactory<BaseNodeFactorySynthetic> {
        unimplemented!()
    }
    fn get_compiler_options(&self) -> &CompilerOptions {
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
