use std::rc::Rc;

use crate::{
    create_base_node_factory, escape_leading_underscores, BaseBindingLikeDeclaration,
    BaseLiteralLikeNode, BaseNamedDeclaration, BaseNode, BaseNodeFactory, BaseNodeFactoryConcrete,
    BaseVariableLikeDeclaration, BinaryExpression, EmptyStatement, Expression, ExpressionStatement,
    Identifier, LiteralTypeNode, Node, NodeArray, NodeArrayOrVec, NodeFactory, NodeFlags,
    NodeInterface, NumericLiteral, PrefixUnaryExpression, SourceFile, SyntaxKind,
    VariableDeclaration, VariableDeclarationList, VariableStatement,
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

    fn create_base_node<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
    ) -> BaseNode {
        base_factory.create_base_node(kind)
    }

    fn create_base_declaration<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
    ) -> BaseNode {
        let node = self.create_base_node(base_factory, kind);
        node
    }

    fn create_base_named_declaration<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        name: Option<Rc<Node>>,
    ) -> BaseNamedDeclaration {
        let node = self.create_base_declaration(base_factory, kind);
        BaseNamedDeclaration::new(node, name)
    }

    fn create_base_binding_like_declaration<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        name: Option<Rc<Node>>,
        initializer: Option<Rc<Node>>,
    ) -> BaseBindingLikeDeclaration {
        let node = self.create_base_named_declaration(base_factory, kind, name);
        BaseBindingLikeDeclaration::new(node, initializer)
    }

    fn create_base_variable_like_declaration<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        name: Option<Rc<Node>>,
        type_: Option<Rc<Node>>,
        initializer: Option<Rc<Node>>,
    ) -> BaseVariableLikeDeclaration {
        let node = self.create_base_binding_like_declaration(base_factory, kind, name, initializer);
        BaseVariableLikeDeclaration::new(node, type_)
    }

    fn create_base_literal<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        value: &str,
    ) -> BaseLiteralLikeNode {
        let node = self.create_base_token(base_factory, kind);
        BaseLiteralLikeNode {
            _node: node,
            text: value.to_string(),
        }
    }

    pub fn create_numeric_literal<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        value: &str,
    ) -> NumericLiteral {
        let node = self.create_base_literal(base_factory, SyntaxKind::NumericLiteral, value);
        NumericLiteral {
            _literal_like_node: node,
        }
    }

    fn create_base_identifier<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        text: &str,
    ) -> Identifier {
        let node = base_factory.create_base_identifier_node(SyntaxKind::Identifier);
        let node = Identifier::new(node, escape_leading_underscores(text));
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

    pub fn create_base_token<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
    ) -> BaseNode {
        base_factory.create_base_token_node(kind)
    }

    pub fn create_token<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        token: SyntaxKind,
    ) -> BaseNode {
        let node = self.create_base_token(base_factory, token);
        node
    }

    pub fn create_true<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
    ) -> BaseNode {
        self.create_token(base_factory, SyntaxKind::TrueKeyword)
    }

    pub fn create_false<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
    ) -> BaseNode {
        self.create_token(base_factory, SyntaxKind::FalseKeyword)
    }

    pub fn create_keyword_type_node<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        token: SyntaxKind,
    ) -> BaseNode {
        self.create_token(base_factory, token)
    }

    pub fn create_literal_type_node<TBaseNodeFactory: BaseNodeFactory, TNode: NodeInterface>(
        &self,
        base_factory: &TBaseNodeFactory,
        literal: &TNode,
    ) -> LiteralTypeNode {
        let node = self.create_token(base_factory, SyntaxKind::LiteralType);
        let node = LiteralTypeNode::new(node, literal.node_wrapper());
        node
    }

    fn create_base_expression<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
    ) -> BaseNode {
        let node = self.create_base_node(base_factory, kind);
        node
    }

    pub fn create_prefix_unary_expression<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        operator: SyntaxKind,
        operand: Expression,
    ) -> PrefixUnaryExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::PrefixUnaryExpression);
        let node = PrefixUnaryExpression::new(node, operator, operand);
        node
    }

    pub fn create_binary_expression<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Expression,
        operator: Node,
        right: Expression,
    ) -> BinaryExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::BinaryExpression);
        let node = BinaryExpression::new(node, left, operator, right);
        node
    }

    pub fn create_variable_statement<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        declaration_list: VariableDeclarationList,
    ) -> VariableStatement {
        let node = self.create_base_declaration(base_factory, SyntaxKind::VariableStatement);
        let node = VariableStatement::new(node, declaration_list.into());
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
            expression: expression.into(),
        }
    }

    pub fn create_variable_declaration<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Option<Rc<Node>>,
        type_: Option<Rc<Node>>,
        initializer: Option<Rc<Node>>,
    ) -> VariableDeclaration {
        let node = self.create_base_variable_like_declaration(
            base_factory,
            SyntaxKind::VariableDeclaration,
            name,
            type_,
            initializer,
        );
        VariableDeclaration::new(node)
    }

    pub fn create_variable_declaration_list<
        TBaseNodeFactory: BaseNodeFactory,
        TDeclarations: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        declarations: TDeclarations,
        flags: Option<NodeFlags>,
    ) -> VariableDeclarationList {
        let node = self.create_base_node(base_factory, SyntaxKind::VariableDeclarationList);
        let node = VariableDeclarationList::new(node, self.create_node_array(declarations));
        node
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
            file_name: "".to_string(),
        };
        node
    }
}

pub fn create_node_factory() -> NodeFactory {
    NodeFactory {}
}

lazy_static! {
    static ref base_factory_static: BaseNodeFactoryConcrete = create_base_node_factory();
}

fn make_synthetic(node: BaseNode) -> BaseNode {
    node
}

lazy_static! {
    pub static ref synthetic_factory: BaseNodeFactorySynthetic = BaseNodeFactorySynthetic::new();
}

pub fn get_synthetic_factory() -> &'static impl BaseNodeFactory {
    &*synthetic_factory
}

struct BaseNodeFactorySynthetic {}

impl BaseNodeFactorySynthetic {
    pub fn new() -> Self {
        Self {}
    }
}

impl BaseNodeFactory for BaseNodeFactorySynthetic {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(base_factory_static.create_base_source_file_node(kind))
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(base_factory_static.create_base_identifier_node(kind))
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(base_factory_static.create_base_token_node(kind))
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(base_factory_static.create_base_node(kind))
    }
}

lazy_static! {
    pub static ref factory: NodeFactory = create_node_factory();
}
