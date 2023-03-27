use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};

pub fn is_internal_declaration(
    node: &Node,
    current_source_file: &Node, /*SourceFile*/
) -> bool {
    unimplemented!()
}

#[derive(Trace, Finalize)]
struct TransformDeclarations {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformDeclarations {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformDeclarations {
    fn call(&self, node: &crate::Node) -> Gc<Node> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformDeclarationsFactory {}

impl TransformDeclarationsFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformDeclarationsFactory {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformDeclarations::new(context)))
    }
}

pub fn transform_declarations() -> TransformerFactory {
    Gc::new(Box::new(TransformDeclarationsFactory::new()))
}
