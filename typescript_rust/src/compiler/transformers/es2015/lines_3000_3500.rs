use gc::Gc;

use super::{ConvertedLoopState, CopyDirection, LoopOutParameter, TransformES2015};
use crate::Node;

impl TransformES2015 {
    pub(super) fn copy_out_parameter(
        &self,
        _out_param: &LoopOutParameter,
        _copy_direction: CopyDirection,
    ) -> Gc<Node /*BinaryExpression*/> {
        unimplemented!()
    }

    pub(super) fn set_labeled_jump(
        &self,
        _state: &mut ConvertedLoopState,
        _is_break: bool,
        _label_text: &str,
        _label_marker: &str,
    ) {
        unimplemented!()
    }
}
