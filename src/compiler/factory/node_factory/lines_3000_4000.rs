#![allow(non_upper_case_globals)]

use std::rc::Rc;

use crate::{
    BaseNodeFactory, Block, EmptyStatement, ExpressionStatement, FunctionDeclaration, IfStatement,
    InterfaceDeclaration, Node, NodeArray, NodeArrayOrVec, NodeFactory, NodeFlags, NodeInterface,
    OmittedExpression, ReturnStatement, SyntaxKind, TemplateSpan, TypeAliasDeclaration,
    VariableDeclaration, VariableDeclarationList, VariableStatement,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn create_omitted_expression(&self, base_factory: &TBaseNodeFactory) -> OmittedExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::OmittedExpression);
        OmittedExpression::new(node)
    }

    pub fn create_template_span(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
        literal: Rc<Node /*TemplateMiddle | TemplateTail*/>,
    ) -> TemplateSpan {
        let node = self.create_base_node(base_factory, SyntaxKind::TemplateSpan);
        let node = TemplateSpan::new(node, expression, literal);
        node
    }

    pub fn create_block<TStatements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        statements: TStatements, /*Statement*/
        multi_line: Option<bool>,
    ) -> Block {
        let node = self.create_base_node(base_factory, SyntaxKind::Block);
        let node = Block::new(
            node,
            self.create_node_array(Some(statements), None),
            multi_line,
        );
        node
    }

    pub fn create_variable_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        modifiers: Option<NodeArray>,
        declaration_list: VariableDeclarationList,
    ) -> VariableStatement {
        let node = self.create_base_declaration(
            base_factory,
            SyntaxKind::VariableStatement,
            Option::<NodeArray>::None,
            modifiers,
        );
        let node = VariableStatement::new(node, declaration_list.into());
        node
    }

    pub fn create_empty_statement(&self, base_factory: &TBaseNodeFactory) -> EmptyStatement {
        EmptyStatement {
            _node: self.create_base_node(base_factory, SyntaxKind::EmptyStatement),
        }
    }

    pub fn create_expression_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node>,
    ) -> ExpressionStatement {
        ExpressionStatement::new(
            self.create_base_node(base_factory, SyntaxKind::ExpressionStatement),
            expression,
        )
    }

    pub fn create_if_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
        then_statement: Rc<Node /*Statement*/>,
        else_statement: Option<Rc<Node /*Statement*/>>,
    ) -> IfStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::IfStatement);
        let node = IfStatement::new(
            node,
            expression,
            self.as_embedded_statement(Some(then_statement)).unwrap(),
            self.as_embedded_statement(else_statement),
        );
        node
    }

    pub fn create_return_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Option<Rc<Node>>,
    ) -> ReturnStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::ReturnStatement);
        let node = ReturnStatement::new(node, expression);
        node
    }

    pub fn create_variable_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Option<Rc<Node>>,
        type_: Option<Rc<Node>>,
        initializer: Option<Rc<Node>>,
    ) -> VariableDeclaration {
        let node = self.create_base_variable_like_declaration(
            base_factory,
            SyntaxKind::VariableDeclaration,
            Option::<NodeArray>::None,
            Option::<NodeArray>::None,
            name,
            type_,
            initializer,
        );
        VariableDeclaration::new(node)
    }

    pub fn create_variable_declaration_list<TDeclarations: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        declarations: TDeclarations,
        flags: Option<NodeFlags>,
    ) -> VariableDeclarationList {
        let flags = flags.unwrap_or(NodeFlags::None);
        let node = self.create_base_node(base_factory, SyntaxKind::VariableDeclarationList);
        node.set_flags(node.flags() & NodeFlags::BlockScoped);
        let node =
            VariableDeclarationList::new(node, self.create_node_array(Some(declarations), None));
        node
    }

    pub fn create_function_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        asterisk_token: Option<Rc<Node>>,
        name: Option<Rc<Node>>,
        type_parameters: Option<NodeArray>,
        parameters: NodeArray,
        type_: Option<Rc<Node>>,
        body: Option<Rc<Node>>,
    ) -> FunctionDeclaration {
        let mut node = self.create_base_function_like_declaration(
            base_factory,
            SyntaxKind::FunctionDeclaration,
            decorators,
            modifiers,
            name,
            type_parameters,
            Some(parameters),
            type_,
            body,
        );
        node.asterisk_token = asterisk_token;
        FunctionDeclaration::new(node)
    }

    pub fn create_interface_declaration<
        TMembers: Into<NodeArrayOrVec>,
        TTypeParameters: Into<NodeArrayOrVec>,
        THeritageClauses: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        name: Rc<Node>,
        type_parameters: Option<TTypeParameters>,
        heritage_clauses: Option<THeritageClauses>,
        members: TMembers,
    ) -> InterfaceDeclaration {
        let node = self.create_base_interface_or_class_like_declaration(
            base_factory,
            SyntaxKind::InterfaceDeclaration,
            decorators,
            modifiers,
            Some(name),
            type_parameters,
            heritage_clauses,
        );
        let node = InterfaceDeclaration::new(node, self.create_node_array(Some(members), None));
        node
    }

    pub fn create_type_alias_declaration<TTypeParameters: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        name: Rc<Node>,
        type_parameters: Option<TTypeParameters>,
        type_: Rc<Node>,
    ) -> TypeAliasDeclaration {
        let node = self.create_base_generic_named_declaration(
            base_factory,
            SyntaxKind::TypeAliasDeclaration,
            decorators,
            modifiers,
            Some(name),
            type_parameters,
        );
        let node = TypeAliasDeclaration::new(node, type_);
        node
    }
}
