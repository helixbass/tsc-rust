use std::rc::Rc;

use crate::{TransformationContext, Transformer, TransformerFactory};

fn transform_es5_fn(context: Rc<TransformationContext>) -> Transformer {
    unimplemented!()
}

pub fn transform_es5() -> TransformerFactory {
    Rc::new(|context| transform_es5_fn(context))
}
