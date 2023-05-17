use gc::Gc;

use super::TransformES2015;
use crate::Node;

impl TransformES2015 {
    pub(super) fn visit_object_literal_expression(
        &self,
        _node: &Node, /*ObjectLiteralExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }
    pub(super) fn should_convert_iteration_statement(
        &self,
        _node: &Node, /*IterationStatement*/
    ) -> bool {
        unimplemented!()
    }
}
