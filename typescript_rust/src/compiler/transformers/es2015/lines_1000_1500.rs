use gc::Gc;

use super::TransformES2015;
use crate::Node;

impl TransformES2015 {
    pub(super) fn visit_parameter(
        &self,
        _node: &Node, /*ParameterDeclaration*/
    ) -> Option<Gc<Node /*ParameterDeclaration*/>> {
        unimplemented!()
    }
}
