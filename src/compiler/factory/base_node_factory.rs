use crate::{BaseNode, SyntaxKind};

pub trait BaseNodeFactory {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode;
    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode;
    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode;
    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode;
}
