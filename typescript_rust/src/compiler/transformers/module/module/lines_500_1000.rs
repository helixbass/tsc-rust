use super::TransformModule;
use crate::{Node, VisitResult};

impl TransformModule {
    pub(super) fn top_level_visitor(&self, _node: &Node) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }
}
