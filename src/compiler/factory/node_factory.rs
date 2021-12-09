use crate::{BaseNode, BaseNodeFactory, EmptyStatement, NodeFactory, SyntaxKind};

impl NodeFactory {
    pub fn create_empty_statement<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
    ) -> EmptyStatement {
    }

    fn create_base_node<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
    ) -> BaseNode {
        base_factory.create_base_node(kind)
    }
}

pub fn create_node_factory() -> NodeFactory {
    NodeFactory {}
}
