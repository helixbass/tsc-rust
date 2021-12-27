use crate::{NodeInterface, SyntaxKind};

pub fn is_private_identifier<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PrivateIdentifier
}

pub fn is_property_signature<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PropertySignature
}

pub fn is_binding_element<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::BindingElement
}

pub fn is_variable_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::VariableDeclaration
}
