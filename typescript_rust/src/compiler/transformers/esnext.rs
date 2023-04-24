use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};

#[derive(Trace, Finalize)]
struct TransformESNext {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformESNext {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformESNext {
    fn call(&self, _node: &crate::Node) -> Gc<Node> {
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
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformESNext::new(context)))
    }
}

pub fn transform_esnext() -> TransformerFactory {
    Gc::new(Box::new(TransformESNextFactory::new()))
}
