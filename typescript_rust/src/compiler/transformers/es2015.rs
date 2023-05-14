use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};

#[derive(Trace, Finalize)]
struct TransformES2015 {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformES2015 {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformES2015 {
    fn call(&self, _node: &crate::Node) -> Gc<Node> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES2015Factory {}

impl TransformES2015Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2015Factory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformES2015::new(context)))
    }
}

pub fn transform_es2015() -> TransformerFactory {
    Gc::new(Box::new(TransformES2015Factory::new()))
}
