use super::TransformSystemModule;
use crate::{Node, VisitResult};

impl TransformSystemModule {
    pub(super) fn top_level_visitor(&self, _node: &Node) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }
}
