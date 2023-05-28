use super::TransformClassFields;
use crate::{Node, VisitResult};

impl TransformClassFields {
    pub(super) fn visit_class_static_block_declaration(
        &self,
        _node: &Node, /*ClassStaticBlockDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }
}
