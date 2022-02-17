use std::rc::Rc;

use crate::{TransformationContext, Transformer, TransformerFactory};

fn transform_es2016_fn(context: TransformationContext) -> Transformer {
    unimplemented!()
}

pub fn transform_es2016() -> TransformerFactory {
    Rc::new(|context| transform_es2016_fn(context))
}
