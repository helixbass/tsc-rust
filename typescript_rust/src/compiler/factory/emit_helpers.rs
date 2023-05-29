use std::borrow::Borrow;

use gc::Gc;

use crate::{Comparison, EmitHelper, Node, PrivateIdentifierKind, TransformationContext};

// TODO: remove #[unsafe_ignore_trace] from TransformNodesTransformationResult if this ends up
// needing to be traced
// #[derive(Trace, Finalize)]
pub struct EmitHelperFactory {}

impl EmitHelperFactory {
    pub fn create_decorate_helper(
        &self,
        _decorator_expressions: &[Gc<Node /*Expression*/>],
        _target: &Node, /*Expression*/
        _member_name: Option<impl Borrow<Node /*Expression*/>>,
        _descriptor: Option<impl Borrow<Node /*Expression*/>>,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_metadata_helper(
        &self,
        _metadata_key: &str,
        _metadata_value: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_param_helper(
        &self,
        _expression: Gc<Node /*Expression*/>,
        _parameter_offset: usize,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_assign_helper(
        &self,
        _attributes_segments: &[Gc<Node /*Expression*/>],
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_await_helper(
        &self,
        _expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_async_generator_helper(
        &self,
        _generator_func: Gc<Node /*FunctionExpression*/>,
        _has_lexical_this: bool,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_async_delegator_helper(
        &self,
        _expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_async_values_helper(
        &self,
        _expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_awaiter_helper(
        &self,
        _has_lexical_this: bool,
        _has_lexical_arguments: bool,
        _promise_constructor: Option<Gc<Node /*EntityName | Expression*/>>,
        _body: Gc<Node /*Block*/>,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_extends_helper(&self, _name: Gc<Node /*Identifier*/>) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_spread_array_helper(
        &self,
        _to: Gc<Node /*Expression*/>,
        _from: Gc<Node /*Expression*/>,
        _pack_from: bool,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_values_helper(
        &self,
        _expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_read_helper(
        &self,
        _iterator_record: Gc<Node /*Expression*/>,
        _count: Option<usize>,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_class_private_field_get_helper(
        &self,
        _receiver: Gc<Node /*Expression*/>,
        _state: Gc<Node /*Identifier*/>,
        _kind: PrivateIdentifierKind,
        _f: Option<Gc<Node /*Identifier*/>>,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub fn create_class_private_field_in_helper(
        &self,
        _state: Gc<Node /*Identifier*/>,
        _receiver: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }
}

pub fn create_emit_helper_factory(
    _context: Gc<Box<dyn TransformationContext>>,
) -> EmitHelperFactory {
    unimplemented!()
}

pub(crate) fn compare_emit_helpers(_x: &EmitHelper, _y: &EmitHelper) -> Comparison {
    unimplemented!()
}

pub fn async_super_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn advanced_async_super_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn is_call_to_helper(
    _first_segment: &Node, /*Expression*/
    _helper_name: &str,    /*__String*/
) -> bool {
    unimplemented!()
}
