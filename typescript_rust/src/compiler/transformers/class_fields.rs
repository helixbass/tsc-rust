use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};
use std::io;

#[derive(Trace, Finalize)]
struct TransformClassFields {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformClassFields {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformClassFields {
    fn call(&self, _node: &Node) -> io::Result<Gc<Node>> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformClassFieldsFactory {}

impl TransformClassFieldsFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformClassFieldsFactory {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformClassFields::new(context)))
    }
}

pub fn transform_class_fields() -> TransformerFactory {
    Gc::new(Box::new(TransformClassFieldsFactory::new()))
}
