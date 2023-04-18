use gc::Gc;

use crate::Node;

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn visit_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        unimplemented!()
    }
}
