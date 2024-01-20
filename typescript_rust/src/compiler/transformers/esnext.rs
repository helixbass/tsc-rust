use std::io;

use gc::{Finalize, Gc, Trace};
use id_arena::Id;

use crate::{
    chain_bundle, visit_each_child, Node, NodeInterface, TransformFlags, TransformationContext,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface,
    VisitResult,
    HasArena, AllArenas, InArena,
};

#[derive(Trace, Finalize)]
struct TransformESNext {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformESNext {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node> {
        let node_ref = node.ref_(arena);
        let node_as_source_file = node_ref.as_source_file();
        if node_as_source_file.is_declaration_file() {
            return node;
        }

        visit_each_child(&node.ref_(arena), |node: Id<Node>| self.visitor(node), &**self.context)
    }

    fn visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        if !node
            .ref_(arena).transform_flags()
            .intersects(TransformFlags::ContainsESNext)
        {
            return Some(node.into());
        }
        #[allow(clippy::match_single_binding)]
        match node.ref_(arena).kind() {
            _ => Some(
                visit_each_child(&node.ref_(arena), |node: Id<Node>| self.visitor(node), &**self.context).into(),
            ),
        }
    }
}

impl TransformerInterface for TransformESNext {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(self.transform_source_file(node))
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
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            Gc::new(Box::new(TransformESNext::new(context))),
        )
    }
}

pub fn transform_esnext() -> TransformerFactory {
    Gc::new(Box::new(TransformESNextFactory::new()))
}
