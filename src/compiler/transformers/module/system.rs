use std::rc::Rc;

use crate::{TransformationContext, Transformer, TransformerFactory};

fn transform_system_module_fn(context: TransformationContext) -> Transformer {
    unimplemented!()
}

pub fn transform_system_module() -> TransformerFactory {
    Rc::new(|context| transform_system_module_fn(context))
}
