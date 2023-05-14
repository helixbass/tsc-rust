use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};
use std::io;

#[derive(Trace, Finalize)]
struct TransformES2021 {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformES2021 {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformES2021 {
    fn call(&self, _node: &Node) -> io::Result<Gc<Node>> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES2021Factory {}

impl TransformES2021Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2021Factory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformES2021::new(context)))
    }
}

pub fn transform_es2021() -> TransformerFactory {
    Gc::new(Box::new(TransformES2021Factory::new()))
}
