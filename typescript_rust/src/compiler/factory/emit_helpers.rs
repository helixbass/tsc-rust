use gc::Gc;

use crate::{Comparison, EmitHelper, Node, TransformationContext};

// TODO: remove #[unsafe_ignore_trace] from TransformNodesTransformationResult if this ends up
// needing to be traced
// #[derive(Trace, Finalize)]
pub struct EmitHelperFactory {}

impl EmitHelperFactory {
    pub fn create_assign_helper(
        &self,
        attributes_segments: &[Gc<Node /*Expression*/>],
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_await_helper(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_async_delegator_helper(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_async_values_helper(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_awaiter_helper(
        &self,
        has_lexical_this: bool,
        has_lexical_arguments: bool,
        promise_constructor: Option<Gc<Node /*EntityName | Expression*/>>,
        body: Gc<Node /*Block*/>,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }
}

pub fn create_emit_helper_factory(
    context: Gc<Box<dyn TransformationContext>>,
) -> EmitHelperFactory {
    unimplemented!()
}

pub(crate) fn compare_emit_helpers(x: &EmitHelper, y: &EmitHelper) -> Comparison {
    unimplemented!()
}

pub fn async_super_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn advanced_async_super_helper() -> Gc<EmitHelper> {
    unimplemented!()
}
