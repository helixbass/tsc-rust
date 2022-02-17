use std::rc::Rc;

use crate::{TransformationContext, Transformer, TransformerFactory};

fn transform_es2017_fn(context: Rc<TransformationContext>) -> Transformer {
    unimplemented!()
}

pub fn transform_es2017() -> TransformerFactory {
    Rc::new(|context| transform_es2017_fn(context))
}
