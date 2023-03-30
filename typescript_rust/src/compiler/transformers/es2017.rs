use std::{
    cell::{Cell, RefCell},
    collections::HashSet,
};

use bitflags::bitflags;
use gc::{Finalize, Gc, Trace};

use crate::{
    chain_bundle, get_emit_script_target, BaseNodeFactorySynthetic, CompilerOptions, EmitResolver,
    Node, NodeCheckFlags, NodeFactory, ScriptTarget, TransformationContext,
    TransformationContextOnEmitNodeOverrider, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface, __String,
};

bitflags! {
    pub(crate) struct ES2017SubstitutionFlags: u32 {
        const None = 0;
        const AsyncMethodsWithSuper = 1 << 0;
    }
}

bitflags! {
    pub(crate) struct ContextFlags: u32 {
        const None = 0;
        const NonTopLevel = 1 << 0;
        const HasLexicalThis = 1 << 1;
    }
}

#[derive(Trace, Finalize)]
struct TransformES2017 {
    context: Gc<Box<dyn TransformationContext>>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    resolver: Gc<Box<dyn EmitResolver>>,
    compiler_options: Gc<CompilerOptions>,
    #[unsafe_ignore_trace]
    language_version: ScriptTarget,
    #[unsafe_ignore_trace]
    enabled_substitutions: Cell<Option<ES2017SubstitutionFlags>>,
    #[unsafe_ignore_trace]
    enclosing_super_container_flags: Cell<NodeCheckFlags>,
    #[unsafe_ignore_trace]
    enclosing_function_parameter_names: RefCell<Option<HashSet<__String>>>,
    #[unsafe_ignore_trace]
    captured_super_properties: RefCell<Option<HashSet<__String>>>,
    #[unsafe_ignore_trace]
    has_super_element_access: Cell<Option<bool>>,
    #[unsafe_ignore_trace]
    substituted_super_accessors: RefCell<Vec<bool>>,
    #[unsafe_ignore_trace]
    context_flags: Cell<ContextFlags>,
}

impl TransformES2017 {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        let compiler_options = context.get_compiler_options();

        let ret: Transformer = Gc::new(Box::new(Self {
            factory: context.factory(),
            resolver: context.get_emit_resolver(),
            context: context.clone(),
            language_version: get_emit_script_target(&compiler_options),
            compiler_options,
            enabled_substitutions: Default::default(),
            enclosing_super_container_flags: Cell::new(NodeCheckFlags::None),
            enclosing_function_parameter_names: Default::default(),
            captured_super_properties: Default::default(),
            has_super_element_access: Default::default(),
            substituted_super_accessors: Default::default(),
            context_flags: Cell::new(ContextFlags::None),
        }));
        context.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformES2017OnEmitNodeOverrider::new(
                ret.clone(),
                previous_on_emit_node,
            )))
        });
        ret
    }

    fn transform_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        unimplemented!()
    }
}

impl TransformerInterface for TransformES2017 {
    fn call(&self, node: &Node) -> Gc<Node> {
        self.transform_source_file(node)
    }
}

#[derive(Trace, Finalize)]
struct TransformES2017OnEmitNodeOverrider {
    transform_es2017: Transformer,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformES2017OnEmitNodeOverrider {
    fn new(
        transform_es2017: Transformer,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_es2017,
            previous_on_emit_node,
        }
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformES2017OnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: crate::EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(crate::EmitHint, &Node),
    ) {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES2017Factory {}

impl TransformES2017Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2017Factory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(context.clone(), TransformES2017::new(context))
    }
}

pub fn transform_es2017() -> TransformerFactory {
    Gc::new(Box::new(TransformES2017Factory::new()))
}
