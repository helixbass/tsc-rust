use crate::{NodeInterface, SyntaxKind};

pub fn is_variable_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::VariableDeclaration
}
