use std::rc::Rc;

use crate::{TransformationContext, Transformer, TransformerFactory};

fn transform_class_fields_fn(context: Rc<dyn TransformationContext>) -> Transformer {
    unimplemented!()
}

pub fn transform_class_fields() -> TransformerFactory {
    Rc::new(|context| transform_class_fields_fn(context))
}
