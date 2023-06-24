use gc::Gc;

use super::TransformSystemModule;
use crate::{get_node_id, GetOrInsertDefault, Node, VisitResult};

impl TransformSystemModule {
    pub(super) fn visitor(&self, _node: &Node) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }

    pub(super) fn modifier_visitor(
        &self,
        _node: &Node, /*FunctionDeclaration*/
    ) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }

    pub(super) fn prevent_substitution(&self, node: Gc<Node>) -> Gc<Node> {
        self.maybe_no_substitution_mut()
            .get_or_insert_default_()
            .insert(get_node_id(&node), true);
        node
    }
}
