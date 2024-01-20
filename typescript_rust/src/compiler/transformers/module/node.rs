use std::{io, mem};

use gc::{Finalize, Gc, GcCell, Trace};
use id_arena::Id;

use crate::{
    EmitHint, Node, NodeInterface, TransformationContext, TransformationContextOnEmitNodeOverrider,
    TransformationContextOnSubstituteNodeOverrider, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface, _d, is_source_file,
    transform_ecmascript_module, transform_module, try_map, Debug_, ModuleKind, SyntaxKind,
    HasArena, AllArenas, InArena,
};

#[derive(Trace, Finalize)]
struct TransformNodeModule {
    _transformer_wrapper: GcCell<Option<Transformer>>,
    context: Gc<Box<dyn TransformationContext>>,
    esm_transform: Transformer,
    esm_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    esm_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    cjs_transform: Transformer,
    cjs_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    cjs_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    current_source_file: GcCell<Option<Id<Node /*SourceFile*/>>>,
}

impl TransformNodeModule {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let esm_transform = transform_ecmascript_module().call(context.clone());

        let esm_on_substitute_node = context.pop_overridden_on_substitute_node();
        let esm_on_emit_node = context.pop_overridden_on_emit_node();

        let cjs_transform = transform_module().call(context.clone());

        let cjs_on_substitute_node = context.pop_overridden_on_substitute_node();
        let cjs_on_emit_node = context.pop_overridden_on_emit_node();

        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: _d(),
            context: context.clone(),
            esm_transform,
            esm_on_substitute_node,
            esm_on_emit_node,
            cjs_transform,
            cjs_on_substitute_node,
            cjs_on_emit_node,
            current_source_file: _d(),
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);

        context.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformNodeModuleOnEmitNodeOverrider::new(
                downcasted.clone(),
                previous_on_emit_node,
            )))
        });
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(TransformNodeModuleOnSubstituteNodeOverrider::new(
                downcasted.clone(),
                previous_on_substitute_node,
            )))
        });
        context.enable_substitution(SyntaxKind::SourceFile);
        context.enable_emit_notification(SyntaxKind::SourceFile);
        downcasted
    }

    fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    fn maybe_current_source_file(&self) -> Option<Id<Node /*SourceFile*/>> {
        self.current_source_file.borrow().clone()
    }

    fn set_current_source_file(&self, current_source_file: Option<Id<Node /*SourceFile*/>>) {
        *self.current_source_file.borrow_mut() = current_source_file;
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
        let result = self.get_module_transform_for_file(node).call(node)?;
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
        Ok(self.context.factory().create_bundle(
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
}

impl HasArena for TransformNodeModule {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformNodeModuleOnEmitNodeOverrider {
    transform_node_module: Gc<Box<TransformNodeModule>>,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformNodeModuleOnEmitNodeOverrider {
    fn new(
        transform_node_module: Gc<Box<TransformNodeModule>>,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_node_module,
            previous_on_emit_node,
        }
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
            self.transform_node_module
                .set_current_source_file(Some(node));
        }
        let Some(current_source_file) = self.transform_node_module.maybe_current_source_file() else {
            return self
                .previous_on_emit_node
                .on_emit_node(hint, node, emit_callback);
        };
        if current_source_file
            .ref_(self).as_source_file()
            .maybe_implied_node_format()
            == Some(ModuleKind::ESNext)
        {
            return self.transform_node_module.esm_on_emit_node.on_emit_node(
                hint,
                node,
                emit_callback,
            );
        }
        self.transform_node_module
            .cjs_on_emit_node
            .on_emit_node(hint, node, emit_callback)
    }
}

impl HasArena for TransformNodeModuleOnEmitNodeOverrider {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformNodeModuleOnSubstituteNodeOverrider {
    transform_node_module: Gc<Box<TransformNodeModule>>,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformNodeModuleOnSubstituteNodeOverrider {
    fn new(
        transform_node_module: Gc<Box<TransformNodeModule>>,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_node_module,
            previous_on_substitute_node,
        }
    }
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformNodeModuleOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        if is_source_file(&node.ref_(self)) {
            self.transform_node_module
                .set_current_source_file(Some(node));
            self.previous_on_substitute_node
                .on_substitute_node(hint, node)
        } else {
            let current_source_file = self.transform_node_module.maybe_current_source_file();
            if current_source_file.is_none() {
                return self
                    .previous_on_substitute_node
                    .on_substitute_node(hint, node);
            }
            let current_source_file = current_source_file.unwrap();
            if current_source_file
                .ref_(self).as_source_file()
                .maybe_implied_node_format()
                == Some(ModuleKind::ESNext)
            {
                return self
                    .transform_node_module
                    .esm_on_substitute_node
                    .on_substitute_node(hint, node);
            }
            self.transform_node_module
                .cjs_on_substitute_node
                .on_substitute_node(hint, node)
        }
    }
}

impl HasArena for TransformNodeModuleOnSubstituteNodeOverrider {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformNodeModuleFactory {}

impl TransformNodeModuleFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformNodeModuleFactory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        TransformNodeModule::new(context).as_transformer()
    }
}

pub fn transform_node_module() -> TransformerFactory {
    Gc::new(Box::new(TransformNodeModuleFactory::new()))
}
