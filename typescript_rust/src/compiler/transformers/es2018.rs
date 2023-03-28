use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};

#[derive(Trace, Finalize)]
struct TransformES2018 {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformES2018 {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformES2018 {
    fn call(&self, node: &crate::Node) -> Gc<Node> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES2018Factory {}

impl TransformES2018Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2018Factory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformES2018::new(context)))
    }
}

pub fn transform_es2018() -> TransformerFactory {
    Gc::new(Box::new(TransformES2018Factory::new()))
}
