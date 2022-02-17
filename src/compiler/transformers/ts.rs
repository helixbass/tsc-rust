use std::rc::Rc;

use crate::{TransformationContext, Transformer, TransformerFactory};

fn transform_type_script_fn(context: TransformationContext) -> Transformer {
    unimplemented!()
}

pub fn transform_type_script() -> TransformerFactory {
    Rc::new(|context| transform_type_script_fn(context))
}
