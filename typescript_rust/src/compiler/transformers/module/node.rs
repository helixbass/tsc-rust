use std::{io, mem, any::Any, cell::Cell};

use id_arena::Id;

use crate::{
    EmitHint, Node, NodeInterface, TransformationContext, TransformationContextOnEmitNodeOverrider,
    TransformationContextOnSubstituteNodeOverrider, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface, _d, is_source_file,
    transform_ecmascript_module, transform_module, try_map, Debug_, ModuleKind, SyntaxKind,
    HasArena, AllArenas, InArena, static_arena, downcast_transformer_ref,
    TransformNodesTransformationResult, CoreTransformationContext,
};

struct TransformNodeModule {
    _arena: *const AllArenas,
    context: Id<TransformNodesTransformationResult>,
    esm_transform: Transformer,
    esm_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    esm_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    cjs_transform: Transformer,
    cjs_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    cjs_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    current_source_file: Cell<Option<Id<Node /*SourceFile*/>>>,
}

impl TransformNodeModule {
    fn new(context: Id<TransformNodesTransformationResult>, arena: *const AllArenas) -> Transformer {
        let arena_ref = unsafe { &*arena };
        let context_ref = context.ref_(arena_ref);
        let esm_transform = transform_ecmascript_module(arena_ref).ref_(arena_ref).call(context.clone());

        let esm_on_substitute_node = context_ref.pop_overridden_on_substitute_node();
        let esm_on_emit_node = context_ref.pop_overridden_on_emit_node();

        let cjs_transform = transform_module(arena_ref).ref_(arena_ref).call(context.clone());

        let cjs_on_substitute_node = context_ref.pop_overridden_on_substitute_node();
        let cjs_on_emit_node = context_ref.pop_overridden_on_emit_node();

        let ret = arena_ref.alloc_transformer(Box::new(Self {
            _arena: arena,
            context: context.clone(),
            esm_transform,
            esm_on_substitute_node,
            esm_on_emit_node,
            cjs_transform,
            cjs_on_substitute_node,
            cjs_on_emit_node,
            current_source_file: _d(),
        }));

        context_ref.override_on_emit_node(&mut |previous_on_emit_node| {
            arena_ref.alloc_transformation_context_on_emit_node_overrider(Box::new(TransformNodeModuleOnEmitNodeOverrider::new(
                ret,
                previous_on_emit_node,
            )))
        });
        context_ref.override_on_substitute_node(&mut |previous_on_substitute_node| {
            arena_ref.alloc_transformation_context_on_substitute_node_overrider(Box::new(TransformNodeModuleOnSubstituteNodeOverrider::new(
                ret,
                previous_on_substitute_node,
            )))
        });
        context_ref.enable_substitution(SyntaxKind::SourceFile);
        context_ref.enable_emit_notification(SyntaxKind::SourceFile);
        ret
    }

    fn maybe_current_source_file(&self) -> Option<Id<Node /*SourceFile*/>> {
        self.current_source_file.get()
    }

    fn set_current_source_file(&self, current_source_file: Option<Id<Node /*SourceFile*/>>) {
        self.current_source_file.set(current_source_file);
    }

    fn get_module_transform_for_file(&self, file: Id<Node> /*SourceFile*/) -> Transformer {
        if file.ref_(self).as_source_file().maybe_implied_node_format() == Some(ModuleKind::ESNext) {
            self.esm_transform.clone()
        } else {
            self.cjs_transform.clone()
        }
    }

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_source_file = node_ref.as_source_file();
        if node_as_source_file.is_declaration_file() {
            return Ok(node);
        }

        self.set_current_source_file(Some(node));
        let result = self.get_module_transform_for_file(node).ref_(self).call(node)?;
        self.set_current_source_file(None);
        Debug_.assert(is_source_file(&result.ref_(self)), None);
        Ok(result)
    }

    fn transform_source_file_or_bundle(
        &self,
        node: Id<Node>, /*SourceFile | Bundle*/
    ) -> io::Result<Id<Node>> {
        Ok(match node.ref_(self).kind() {
            SyntaxKind::SourceFile => self.transform_source_file(node)?,
            _ => self.transform_bundle(node)?,
        })
    }

    fn transform_bundle(&self, node: Id<Node> /*Bundle*/) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_bundle = node_ref.as_bundle();
        Ok(self.context.ref_(self).factory().ref_(self).create_bundle(
            try_map(
                &node_as_bundle.source_files,
                |source_file: &Option<Id<Node>>, _| -> io::Result<_> {
                    Ok(Some(
                        self.transform_source_file(source_file.unwrap())?,
                    ))
                },
            )?,
            Some(node_as_bundle.prepends.clone()),
        ))
    }
}

impl TransformerInterface for TransformNodeModule {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        self.transform_source_file_or_bundle(node)
    }

    fn as_dyn_any(&self) -> &dyn Any {
        self
    }
}

impl HasArena for TransformNodeModule {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

struct TransformNodeModuleOnEmitNodeOverrider {
    transform_node_module: Transformer,
    previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformNodeModuleOnEmitNodeOverrider {
    fn new(
        transform_node_module: Transformer,
        previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_node_module,
            previous_on_emit_node,
        }
    }

    fn transform_node_module(&self) -> debug_cell::Ref<'_, TransformNodeModule> {
        downcast_transformer_ref(self.transform_node_module, self)
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformNodeModuleOnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        if is_source_file(&node.ref_(self)) {
            self.transform_node_module()
                .set_current_source_file(Some(node));
        }
        let Some(current_source_file) = self.transform_node_module().maybe_current_source_file() else {
            return self
                .previous_on_emit_node
                .ref_(self).on_emit_node(hint, node, emit_callback);
        };
        if current_source_file
            .ref_(self).as_source_file()
            .maybe_implied_node_format()
            == Some(ModuleKind::ESNext)
        {
            return self.transform_node_module().esm_on_emit_node.ref_(self).on_emit_node(
                hint,
                node,
                emit_callback,
            );
        }
        self.transform_node_module()
            .cjs_on_emit_node
            .ref_(self).on_emit_node(hint, node, emit_callback)
    }
}

impl HasArena for TransformNodeModuleOnEmitNodeOverrider {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

struct TransformNodeModuleOnSubstituteNodeOverrider {
    transform_node_module: Transformer,
    previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformNodeModuleOnSubstituteNodeOverrider {
    fn new(
        transform_node_module: Transformer,
        previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_node_module,
            previous_on_substitute_node,
        }
    }

    fn transform_node_module(&self) -> debug_cell::Ref<'_, TransformNodeModule> {
        downcast_transformer_ref(self.transform_node_module, self)
    }
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformNodeModuleOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        if is_source_file(&node.ref_(self)) {
            self.transform_node_module()
                .set_current_source_file(Some(node));
            self.previous_on_substitute_node
                .ref_(self).on_substitute_node(hint, node)
        } else {
            let current_source_file = self.transform_node_module().maybe_current_source_file();
            if current_source_file.is_none() {
                return self
                    .previous_on_substitute_node
                    .ref_(self).on_substitute_node(hint, node);
            }
            let current_source_file = current_source_file.unwrap();
            if current_source_file
                .ref_(self).as_source_file()
                .maybe_implied_node_format()
                == Some(ModuleKind::ESNext)
            {
                return self
                    .transform_node_module()
                    .esm_on_substitute_node
                    .ref_(self).on_substitute_node(hint, node);
            }
            self.transform_node_module()
                .cjs_on_substitute_node
                .ref_(self).on_substitute_node(hint, node)
        }
    }
}

impl HasArena for TransformNodeModuleOnSubstituteNodeOverrider {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

struct TransformNodeModuleFactory {}

impl TransformNodeModuleFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformNodeModuleFactory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        TransformNodeModule::new(context, &*static_arena())
    }
}

pub fn transform_node_module(arena: &impl HasArena) -> TransformerFactory {
    arena.alloc_transformer_factory(Box::new(TransformNodeModuleFactory::new()))
}
