use std::io;

use gc::{Finalize, Gc, Trace};

use crate::{
    chain_bundle, visit_each_child, Node, NodeInterface, TransformFlags, TransformationContext,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface,
    VisitResult,
};

#[derive(Trace, Finalize)]
struct TransformESNext {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformESNext {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }

    fn transform_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        let node_as_source_file = node.as_source_file();
        if node_as_source_file.is_declaration_file() {
            return node.node_wrapper();
        }

        visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    fn visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        if !node
            .transform_flags()
            .intersects(TransformFlags::ContainsESNext)
        {
            return Some(node.node_wrapper().into());
        }
        #[allow(clippy::match_single_binding)]
        match node.kind() {
            _ => Some(
                visit_each_child(node, |node: &Node| self.visitor(node), &**self.context).into(),
            ),
        }
    }
}

impl TransformerInterface for TransformESNext {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        Ok(self.transform_source_file(node))
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
