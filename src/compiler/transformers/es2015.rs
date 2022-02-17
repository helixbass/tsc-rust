use std::rc::Rc;

use crate::{TransformationContext, Transformer, TransformerFactory};

fn transform_es2015_fn(context: Rc<TransformationContext>) -> Transformer {
    unimplemented!()
}

pub fn transform_es2015() -> TransformerFactory {
    Rc::new(|context| transform_es2015_fn(context))
}
