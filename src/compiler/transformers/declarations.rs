use std::rc::Rc;

use crate::{TransformationContext, Transformer, TransformerFactory};

fn transform_declarations_fn(context: Rc<dyn TransformationContext>) -> Transformer {
    unimplemented!()
}

pub fn transform_declarations() -> TransformerFactory {
    Rc::new(|context| transform_declarations_fn(context))
}
