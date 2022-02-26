use std::rc::Rc;

use crate::{TransformationContext, Transformer, TransformerFactory};

fn transform_node_module_fn(context: Rc<dyn TransformationContext>) -> Transformer {
    unimplemented!()
}

pub fn transform_node_module() -> TransformerFactory {
    Rc::new(|context| transform_node_module_fn(context))
}
