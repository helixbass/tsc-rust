use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};

#[derive(Trace, Finalize)]
struct TransformNodeModule {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformNodeModule {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformNodeModule {
    fn call(&self, _node: &crate::Node) -> Gc<Node> {
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
        Gc::new(Box::new(TransformNodeModule::new(context)))
    }
}

pub fn transform_node_module() -> TransformerFactory {
    Gc::new(Box::new(TransformNodeModuleFactory::new()))
}
