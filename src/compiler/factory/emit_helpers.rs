use std::rc::Rc;

use crate::TransformationContext;

pub struct EmitHelperFactory {}

pub fn create_emit_helper_factory(context: Rc<dyn TransformationContext>) -> EmitHelperFactory {
    unimplemented!()
}
