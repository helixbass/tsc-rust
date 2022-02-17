use std::rc::Rc;

use crate::{BaseNodeFactory, CoreTransformationContext, Transformer};

pub fn chain_bundle<
    TBaseNodeFactory: BaseNodeFactory,
    TContext: CoreTransformationContext<TBaseNodeFactory>,
>(
    context: Rc<TContext>,
    transform_source_file: Transformer,
) -> Transformer {
    unimplemented!()
}
