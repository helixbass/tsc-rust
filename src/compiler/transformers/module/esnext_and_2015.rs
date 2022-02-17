use std::rc::Rc;

use crate::{TransformationContext, Transformer, TransformerFactory};

fn transform_ecmascript_module_fn(context: Rc<TransformationContext>) -> Transformer {
    unimplemented!()
}

pub fn transform_ecmascript_module() -> TransformerFactory {
    Rc::new(|context| transform_ecmascript_module_fn(context))
}
