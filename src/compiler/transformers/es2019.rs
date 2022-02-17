use std::rc::Rc;

use crate::{TransformationContext, Transformer, TransformerFactory};

fn transform_es2019_fn(context: Rc<dyn TransformationContext>) -> Transformer {
    unimplemented!()
}

pub fn transform_es2019() -> TransformerFactory {
    Rc::new(|context| transform_es2019_fn(context))
}
