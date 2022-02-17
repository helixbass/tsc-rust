use std::rc::Rc;

use crate::{TransformationContext, Transformer, TransformerFactory};

fn transform_generators_fn(context: Rc<TransformationContext>) -> Transformer {
    unimplemented!()
}

pub fn transform_generators() -> TransformerFactory {
    Rc::new(|context| transform_generators_fn(context))
}
