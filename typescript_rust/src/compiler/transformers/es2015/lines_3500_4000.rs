use gc::Gc;

use crate::{Node, TransformES2015};

impl TransformES2015 {
    pub(super) fn visit_catch_clause(
        &self,
        node: &Node, /*CatchClause*/
    ) -> Gc<Node /*CatchClause*/> {
        unimplemented!()
    }
}
