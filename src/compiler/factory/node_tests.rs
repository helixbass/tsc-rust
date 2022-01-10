use crate::{NodeInterface, SyntaxKind};

pub fn is_big_int_literal<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::BigIntLiteral
}

pub fn is_identifier<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::Identifier
}

pub fn is_private_identifier<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PrivateIdentifier
}

pub fn is_property_signature<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PropertySignature
}

pub fn is_property_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PropertyDeclaration
}

pub fn is_class_static_block_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ClassStaticBlockDeclaration
}

pub fn is_binding_element<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::BindingElement
}

pub fn is_object_literal_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ObjectLiteralExpression
}

pub fn is_omitted_expression<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::OmittedExpression
}

pub fn is_block<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::Block
}

pub fn is_variable_declaration<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::VariableDeclaration
}

pub fn is_module_block<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::ModuleBlock
}

pub fn is_property_assignment<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::PropertyAssignment
}

pub fn is_source_file<TNode: NodeInterface>(node: &TNode) -> bool {
    node.kind() == SyntaxKind::SourceFile
}
