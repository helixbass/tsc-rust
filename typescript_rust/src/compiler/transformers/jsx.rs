use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};

#[derive(Trace, Finalize)]
struct TransformJsx {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformJsx {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformJsx {
    fn call(&self, _node: &crate::Node) -> Gc<Node> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformJsxFactory {}

impl TransformJsxFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformJsxFactory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformJsx::new(context)))
    }
}

pub fn transform_jsx() -> TransformerFactory {
    Gc::new(Box::new(TransformJsxFactory::new()))
}
