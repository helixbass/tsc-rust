use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};

#[derive(Trace, Finalize)]
struct TransformTypeScript {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformTypeScript {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformTypeScript {
    fn call(&self, node: &crate::Node) -> Gc<Node> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformTypeScriptFactory {}

impl TransformTypeScriptFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformTypeScriptFactory {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformTypeScript::new(context)))
    }
}

pub fn transform_type_script() -> TransformerFactory {
    Gc::new(Box::new(TransformTypeScriptFactory::new()))
}
