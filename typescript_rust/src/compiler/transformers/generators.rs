use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};
use std::io;

#[derive(Trace, Finalize)]
struct TransformGenerators {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformGenerators {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformGenerators {
    fn call(&self, _node: &Node) -> io::Result<Gc<Node>> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformGeneratorsFactory {}

impl TransformGeneratorsFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformGeneratorsFactory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformGenerators::new(context)))
    }
}

pub fn transform_generators() -> TransformerFactory {
    Gc::new(Box::new(TransformGeneratorsFactory::new()))
}
