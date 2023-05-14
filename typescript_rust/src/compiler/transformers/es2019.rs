use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};

#[derive(Trace, Finalize)]
struct TransformES2019 {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformES2019 {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformES2019 {
    fn call(&self, _node: &crate::Node) -> Gc<Node> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES2019Factory {}

impl TransformES2019Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2019Factory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformES2019::new(context)))
    }
}

pub fn transform_es2019() -> TransformerFactory {
    Gc::new(Box::new(TransformES2019Factory::new()))
}
