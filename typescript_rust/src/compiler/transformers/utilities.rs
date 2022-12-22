use std::rc::Rc;

use crate::{TransformationContext, Transformer};

// TODO: does chain_bundle() need to accept any CoreTnansformationContext's that aren't TransformationContext's?
// pub fn chain_bundle<
//     TBaseNodeFactory: BaseNodeFactory,
//     TContext: CoreTransformationContext<TBaseNodeFactory>,
// >(
//     context: Rc<TContext>,
//     transform_source_file: Transformer,
// ) -> Transformer {
pub fn chain_bundle(
    context: Rc<dyn TransformationContext>,
    transform_source_file: Transformer,
) -> Transformer {
    unimplemented!()
}
