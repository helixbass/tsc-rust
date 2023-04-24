use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};

#[derive(Trace, Finalize)]
struct TransformEcmascriptModule {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformEcmascriptModule {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformEcmascriptModule {
    fn call(&self, _node: &crate::Node) -> Gc<Node> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformEcmascriptModuleFactory {}

impl TransformEcmascriptModuleFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformEcmascriptModuleFactory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformEcmascriptModule::new(context)))
    }
}

pub fn transform_ecmascript_module() -> TransformerFactory {
    Gc::new(Box::new(TransformEcmascriptModuleFactory::new()))
}
