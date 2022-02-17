use std::rc::Rc;

use crate::{TransformationContext, Transformer, TransformerFactory};

fn transform_esnext_fn(context: TransformationContext) -> Transformer {
    unimplemented!()
}

pub fn transform_esnext() -> TransformerFactory {
    Rc::new(|context| transform_esnext_fn(context))
}
