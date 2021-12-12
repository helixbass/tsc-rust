use crate::{
    BaseNode, BaseNodeFactory, EmptyStatement, Expression, ExpressionStatement, Identifier,
    NodeArray, NodeArrayOrVec, NodeFactory, SourceFile, SyntaxKind,
};

impl NodeFactory {
    pub fn create_node_array<TElements: Into<NodeArrayOrVec>>(
        &self,
        elements: TElements,
    ) -> NodeArray {
        match elements.into() {
            NodeArrayOrVec::NodeArray(node_array) => node_array,
            NodeArrayOrVec::Vec(elements) => NodeArray::new(elements),
        }
    }

    fn create_base_identifier<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        text: &str,
    ) -> Identifier {
        let node = base_factory.create_base_identifier_node(SyntaxKind::Identifier);
        let node = Identifier {
            _node: node,
            escaped_text: text.to_string(),
        };
        node
    }

    pub fn create_identifier<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        text: &str,
    ) -> Identifier {
        let node = self.create_base_identifier(base_factory, text);
        node
    }

    pub fn create_empty_statement<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
    ) -> EmptyStatement {
        EmptyStatement {
            _node: self.create_base_node(base_factory, SyntaxKind::EmptyStatement),
        }
    }

    pub fn create_expression_statement<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Expression,
    ) -> ExpressionStatement {
        ExpressionStatement {
            _node: self.create_base_node(base_factory, SyntaxKind::ExpressionStatement),
            expression,
        }
    }

    pub fn create_source_file<TBaseNodeFactory: BaseNodeFactory, TNodes: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        statements: TNodes,
    ) -> SourceFile {
        let node = base_factory.create_base_source_file_node(SyntaxKind::SourceFile);
        let node = SourceFile {
            _node: node,
            statements: self.create_node_array(statements),
        };
        node
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
