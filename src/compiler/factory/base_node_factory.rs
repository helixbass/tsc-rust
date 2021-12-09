use crate::{BaseNode, SyntaxKind};

pub trait BaseNodeFactory {
    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode;
}
