use std::borrow::Borrow;

use gc::{Finalize, Gc, Trace};

use crate::{
    Node, NodeId, TransformationContext, Transformer, WrapCustomTransformerFactoryHandleDefault,
};

pub fn get_original_node_id(node: &Node) -> NodeId {
    unimplemented!()
}

pub fn maybe_get_original_node_id(node: Option<impl Borrow<Node>>) -> NodeId {
    unimplemented!()
}

// TODO: does chain_bundle() need to accept any CoreTnansformationContext's that aren't TransformationContext's?
// pub fn chain_bundle<
//     TBaseNodeFactory: BaseNodeFactory,
//     TContext: CoreTransformationContext<TBaseNodeFactory>,
// >(
//     context: Rc<TContext>,
//     transform_source_file: Transformer,
// ) -> Transformer {

#[derive(Trace, Finalize)]
struct ChainBundle;

impl WrapCustomTransformerFactoryHandleDefault for ChainBundle {
    fn call(
        &self,
        _context: Gc<Box<dyn TransformationContext>>,
        transform_source_file: Transformer,
    ) -> Transformer {
        transform_source_file
    }
}

pub fn chain_bundle() -> Gc<Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
    thread_local! {
        static CHAIN_BUNDLE: Gc<Box<dyn WrapCustomTransformerFactoryHandleDefault>> = Gc::new(Box::new(ChainBundle));
    }
    CHAIN_BUNDLE.with(|chain_bundle| chain_bundle.clone())
}
