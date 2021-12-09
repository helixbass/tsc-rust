use crate::{BaseNodeFactory, EmptyStatement, NodeFactory};

impl NodeFactory {
    pub fn create_empty_statement<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
    ) -> EmptyStatement {
    }

    fn create_base_node<TBaseNodeFactory: BaseNodeFactory, TNode: Node>(
        &self,
        base_factory: &TBaseNodeFactory,
    ) -> TNode {
    }
}

pub fn create_node_factory() -> NodeFactory {
    NodeFactory {}
}
