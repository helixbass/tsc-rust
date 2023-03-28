use gc::Gc;

use crate::{Comparison, EmitHelper, TransformationContext};

// TODO: remove #[unsafe_ignore_trace] from TransformNodesTransformationResult if this ends up
// needing to be traced
// #[derive(Trace, Finalize)]
pub struct EmitHelperFactory {}

pub fn create_emit_helper_factory(
    context: Gc<Box<dyn TransformationContext>>,
) -> EmitHelperFactory {
    unimplemented!()
}

pub(crate) fn compare_emit_helpers(x: &EmitHelper, y: &EmitHelper) -> Comparison {
    unimplemented!()
}
