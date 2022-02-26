use std::rc::Rc;

use crate::{TransformationContext, Transformer, TransformerFactory};

fn transform_jsx_fn(context: Rc<dyn TransformationContext>) -> Transformer {
    unimplemented!()
}

pub fn transform_jsx() -> TransformerFactory {
    Rc::new(|context| transform_jsx_fn(context))
}
