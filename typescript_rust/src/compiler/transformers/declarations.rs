use std::rc::Rc;

use crate::{Node, TransformationContext, Transformer, TransformerFactory};

pub fn is_internal_declaration(
    node: &Node,
    current_source_file: &Node, /*SourceFile*/
) -> bool {
    unimplemented!()
}

fn transform_declarations_fn(context: Rc<dyn TransformationContext>) -> Transformer {
    unimplemented!()
}

pub fn transform_declarations() -> TransformerFactory {
    Rc::new(|context| transform_declarations_fn(context))
}
