#![allow(non_upper_case_globals)]

use std::rc::Rc;

use super::{propagate_child_flags, propagate_children_flags};
use crate::{
    AsExpression, BaseNodeFactory, Block, Debug_, EmptyStatement, ExpressionStatement,
    ExpressionWithTypeArguments, FunctionDeclaration, IfStatement, InterfaceDeclaration,
    MetaProperty, Node, NodeArray, NodeArrayOrVec, NodeFactory, NodeFlags, NodeInterface,
    NonNullExpression, OmittedExpression, ReturnStatement, SyntaxKind, TemplateSpan,
    TransformFlags, TypeAliasDeclaration, VariableDeclaration, VariableDeclarationList,
    VariableStatement,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn create_omitted_expression(&self, base_factory: &TBaseNodeFactory) -> OmittedExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::OmittedExpression);
        OmittedExpression::new(node)
    }

    pub fn create_expression_with_type_arguments<TTypeArguments: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node>, /*Expression*/
        type_arguments: Option<TTypeArguments>,
    ) -> ExpressionWithTypeArguments {
        let node = self.create_base_node(base_factory, SyntaxKind::ExpressionWithTypeArguments);
        let mut node = ExpressionWithTypeArguments::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &expression),
            type_arguments.and_then(|type_arguments| {
                self.parenthesizer_rules()
                    .parenthesize_type_arguments(base_factory, Some(type_arguments.into()))
            }),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_children_flags(node.type_arguments.as_ref())
                | TransformFlags::ContainsES2015,
        );
        node
    }

    pub fn create_as_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node>, /*Expression*/
        type_: Rc<Node /*TypeNode*/>,
    ) -> AsExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::AsExpression);
        let mut node = AsExpression::new(node, expression, type_);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.type_))
                | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn create_non_null_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node>, /*Expression*/
    ) -> NonNullExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::NonNullExpression);
        let mut node = NonNullExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression)) | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn create_non_null_chain(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node>, /*Expression*/
    ) -> NonNullExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::NonNullExpression);
        node.set_flags(node.flags() | NodeFlags::OptionalChain);
        let mut node = NonNullExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression)) | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn create_meta_property(
        &self,
        base_factory: &TBaseNodeFactory,
        keyword_token: SyntaxKind, /*MetaProperty["keywordToken"]*/
        name: Rc<Node>,            /*Identifier*/
    ) -> MetaProperty {
        let node = self.create_base_expression(base_factory, SyntaxKind::MetaProperty);
        let mut node = MetaProperty::new(node, keyword_token, name);
        node.add_transform_flags(propagate_child_flags(Some(&*node.name)));
        match keyword_token {
            SyntaxKind::NewKeyword => {
                node.add_transform_flags(TransformFlags::ContainsES2015);
            }
            SyntaxKind::ImportKeyword => {
                node.add_transform_flags(TransformFlags::ContainsESNext);
            }
            _ => {
                Debug_.assert_never(keyword_token, None);
            }
        }
        node
    }

    pub fn create_template_span(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
        literal: Rc<Node /*TemplateMiddle | TemplateTail*/>,
    ) -> TemplateSpan {
        let node = self.create_base_node(base_factory, SyntaxKind::TemplateSpan);
        let mut node = TemplateSpan::new(node, expression, literal);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_child_flags(Some(&*node.literal))
                | TransformFlags::ContainsES2015,
        );
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
