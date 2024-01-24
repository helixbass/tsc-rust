use std::{io, any::Any};

use gc::{Finalize, Gc, Trace};
use id_arena::Id;

use crate::{
    chain_bundle, visit_each_child, Node, NodeInterface, TransformFlags, TransformationContext,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface,
    VisitResult,
    HasArena, AllArenas, InArena,
    TransformNodesTransformationResult,
};

#[derive(Trace, Finalize)]
struct TransformESNext {
    context: Id<TransformNodesTransformationResult>,
}

impl TransformESNext {
    fn new(context: Id<TransformNodesTransformationResult>) -> Self {
        Self { context }
    }

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_source_file = node_ref.as_source_file();
        if node_as_source_file.is_declaration_file() {
            return node;
        }

        visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self)
    }

    fn visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        if !node
            .ref_(self).transform_flags()
            .intersects(TransformFlags::ContainsESNext)
        {
            return Some(node.into());
        }
        #[allow(clippy::match_single_binding)]
        match node.ref_(self).kind() {
            _ => Some(
                visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self).into(),
            ),
        }
    }
}

impl TransformerInterface for TransformESNext {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(self.transform_source_file(node))
    }

    fn as_dyn_any(&self) -> &dyn Any {
        self
    }
}

impl HasArena for TransformESNext {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformESNextFactory {}

impl TransformESNextFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformESNextFactory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle().call(
            context,
            self.alloc_transformer(Box::new(TransformESNext::new(context))),
        )
    }
}

impl HasArena for TransformESNextFactory {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub fn transform_esnext(arena: &impl HasArena) -> TransformerFactory {
    arena.alloc_transformer_factory(Box::new(TransformESNextFactory::new()))
}
