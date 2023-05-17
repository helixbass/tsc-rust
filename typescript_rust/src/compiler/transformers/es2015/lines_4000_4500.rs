use gc::Gc;

use super::TransformES2015;
use crate::Node;

impl TransformES2015 {
    pub(super) fn visit_super_keyword(
        &self,
        _is_expression_of_call: bool,
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        unimplemented!()
    }
}
