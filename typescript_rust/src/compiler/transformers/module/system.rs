use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};

#[derive(Trace, Finalize)]
struct TransformSystemModule {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformSystemModule {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformSystemModule {
    fn call(&self, _node: &crate::Node) -> Gc<Node> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformSystemModuleFactory {}

impl TransformSystemModuleFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformSystemModuleFactory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformSystemModule::new(context)))
    }
}

pub fn transform_system_module() -> TransformerFactory {
    Gc::new(Box::new(TransformSystemModuleFactory::new()))
}
