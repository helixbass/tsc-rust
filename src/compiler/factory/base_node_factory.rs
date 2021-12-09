use crate::{Node, SyntaxKind};

pub trait BaseNodeFactory {
    fn create_base_node(&self, kind: SyntaxKind) -> Box<dyn Node>;
}
