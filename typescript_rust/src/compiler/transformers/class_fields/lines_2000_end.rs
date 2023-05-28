use super::TransformClassFields;
use crate::{Node, VisitResult};

impl TransformClassFields {
    pub(super) fn visit_assignment_pattern(
        &self,
        _node: &Node, /*AssignmentPattern*/
    ) -> VisitResult {
        unimplemented!()
    }
}
