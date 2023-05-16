use std::{borrow::Borrow, io};

use gc::{Finalize, Gc, Trace};

use crate::{
    get_node_id, get_original_node, BaseNodeFactory, Node, NodeFactory, NodeId,
    TransformationContext, Transformer, VisitResult, WrapCustomTransformerFactoryHandleDefault,
};

pub fn get_original_node_id(node: &Node) -> NodeId {
    let node = get_original_node(Some(node), Option::<fn(Option<Gc<Node>>) -> bool>::None);
    if let Some(node) = node {
        get_node_id(&node)
    } else {
        0
    }
}

pub fn maybe_get_original_node_id(node: Option<impl Borrow<Node>>) -> NodeId {
    let node = get_original_node(node, Option::<fn(Option<Gc<Node>>) -> bool>::None);
    if let Some(node) = node {
        get_node_id(&node)
    } else {
        0
    }
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

pub fn is_simple_copiable_expression(_expression: &Node /*Expression*/) -> bool {
    unimplemented!()
}

pub fn is_simple_inlineable_expression(_expression: &Node /*Expression*/) -> bool {
    unimplemented!()
}

pub fn add_prologue_directives_and_initial_super_call(
    _factory: &NodeFactory<impl 'static + BaseNodeFactory + Trace + Finalize>,
    _ctor: &Node, /*ConstructorDeclaration*/
    _result: &mut Vec<Gc<Node /*Statement*/>>,
    _visitor: impl FnMut(&Node) -> VisitResult,
) -> usize {
    unimplemented!()
}

pub fn try_add_prologue_directives_and_initial_super_call(
    _factory: &NodeFactory<impl 'static + BaseNodeFactory + Trace + Finalize>,
    _ctor: &Node, /*ConstructorDeclaration*/
    _result: &mut Vec<Gc<Node /*Statement*/>>,
    _visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
) -> io::Result<usize> {
    unimplemented!()
}

pub fn get_properties(
    _node: &Node, /*ClassExpression | ClassDeclaration*/
    _require_initializer: bool,
    _is_static: bool,
) -> Vec<Gc<Node /*PropertyDeclaration*/>> {
    unimplemented!()
}
