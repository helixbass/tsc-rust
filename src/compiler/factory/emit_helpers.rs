use std::rc::Rc;

use crate::{Comparison, EmitHelper, TransformationContext};

pub struct EmitHelperFactory {}

pub fn create_emit_helper_factory(context: Rc<dyn TransformationContext>) -> EmitHelperFactory {
    unimplemented!()
}

pub(crate) fn compare_emit_helpers(x: &EmitHelper, y: &EmitHelper) -> Comparison {
    unimplemented!()
}
