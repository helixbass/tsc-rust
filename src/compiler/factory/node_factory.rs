#![allow(non_upper_case_globals)]

use std::rc::Rc;

use crate::{
    create_base_node_factory, escape_leading_underscores, is_omitted_expression, last_or_undefined,
    ArrayLiteralExpression, ArrayTypeNode, BaseBindingLikeDeclaration, BaseGenericNamedDeclaration,
    BaseInterfaceOrClassLikeDeclaration, BaseLiteralLikeNode, BaseNamedDeclaration, BaseNode,
    BaseNodeFactory, BaseNodeFactoryConcrete, BaseVariableLikeDeclaration, BinaryExpression,
    EmptyStatement, Expression, ExpressionStatement, Identifier, InterfaceDeclaration,
    IntersectionTypeNode, LiteralLikeNodeInterface, LiteralTypeNode, Node, NodeArray,
    NodeArrayOrVec, NodeFactory, NodeFlags, NumericLiteral, ObjectLiteralExpression,
    PrefixUnaryExpression, PropertyAssignment, PropertySignature, SourceFile, StringLiteral,
    SyntaxKind, TypeLiteralNode, TypeNode, TypeParameterDeclaration, TypeReferenceNode,
    UnionTypeNode, VariableDeclaration, VariableDeclarationList, VariableStatement,
};

impl NodeFactory {
    pub fn create_node_array<TElements: Into<NodeArrayOrVec>>(
        &self,
        elements: TElements,
        has_trailing_comma: Option<bool>,
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

    fn create_base_generic_named_declaration<
        TBaseNodeFactory: BaseNodeFactory,
        TTypeParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        name: Rc<Node>,
        type_parameters: Option<TTypeParameters>,
    ) -> BaseGenericNamedDeclaration {
        let node = self.create_base_named_declaration(base_factory, kind, Some(name));
        let node = BaseGenericNamedDeclaration::new(node, self.as_node_array(type_parameters));
        node
    }

    fn create_base_interface_or_class_like_declaration<
        TBaseNodeFactory: BaseNodeFactory,
        TTypeParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        name: Rc<Node>,
        type_parameters: Option<TTypeParameters>,
    ) -> BaseInterfaceOrClassLikeDeclaration {
        let node =
            self.create_base_generic_named_declaration(base_factory, kind, name, type_parameters);
        let node = BaseInterfaceOrClassLikeDeclaration::new(node);
        node
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
        value: String,
    ) -> BaseLiteralLikeNode {
        let node = self.create_base_token(base_factory, kind);
        BaseLiteralLikeNode::new(node, value)
    }

    pub fn create_numeric_literal<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        value: String,
    ) -> NumericLiteral {
        let node = self.create_base_literal(base_factory, SyntaxKind::NumericLiteral, value);
        NumericLiteral::new(node)
    }

    pub fn create_base_string_literal<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        text: String,
        is_single_quote: Option<bool>,
    ) -> StringLiteral {
        let node = self.create_base_literal(base_factory, SyntaxKind::StringLiteral, text);
        StringLiteral::new(node, is_single_quote)
    }

    pub fn create_string_literal<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        text: String,
        is_single_quote: Option<bool>,
        has_extended_unicode_escape: Option<bool>,
    ) -> StringLiteral {
        let mut node = self.create_base_string_literal(base_factory, text, is_single_quote);
        node.set_has_extended_unicode_escape(has_extended_unicode_escape);
        node
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

    pub fn create_type_parameter_declaration<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Rc<Node>,
    ) -> TypeParameterDeclaration {
        let node =
            self.create_base_named_declaration(base_factory, SyntaxKind::TypeParameter, Some(name));
        let node = TypeParameterDeclaration::new(node);
        node
    }

    pub fn create_property_signature<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Rc<Node>,
        type_: Option<Rc<Node>>,
    ) -> PropertySignature {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::PropertySignature,
            Some(name),
        );
        let node = PropertySignature::new(node, type_);
        node
    }

    pub fn create_keyword_type_node<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        token: SyntaxKind,
    ) -> BaseNode {
        self.create_token(base_factory, token)
    }

    pub fn create_type_reference_node<
        TBaseNodeFactory: BaseNodeFactory,
        TTypeArguments: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        type_name: Rc<Node>,
        type_arguments: Option<TTypeArguments>,
    ) -> TypeReferenceNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TypeReference);
        let node = TypeReferenceNode::new(
            node,
            self.as_name(type_name),
            type_arguments.map(|type_arguments| self.create_node_array(type_arguments, None)),
        );
        node
    }

    pub fn create_type_literal_node<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        members: Option<Vec<Rc<Node>>>,
    ) -> TypeLiteralNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TypeLiteral);
        let node = TypeLiteralNode::new(
            node,
            self.create_node_array(members.unwrap_or_else(|| vec![]), None),
        );
        node
    }

    pub fn create_array_type_node<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        element_type: Rc<Node>,
    ) -> ArrayTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::ArrayType);
        let node = ArrayTypeNode::new(node, element_type);
        node
    }

    pub fn create_union_or_intersection_type_node<
        TBaseNodeFactory: BaseNodeFactory,
        TElements: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind, /*SyntaxKind.UnionType | SyntaxKind.IntersectionType*/
        types: TElements, /*<TypeNode>*/
    ) -> TypeNode {
        let node = self.create_base_node(base_factory, kind);
        let types = match types.into() {
            NodeArrayOrVec::NodeArray(node_array) => node_array,
            NodeArrayOrVec::Vec(types) => NodeArray::new(types),
        };
        match kind {
            SyntaxKind::UnionType => UnionTypeNode::new(node, types).into(),
            SyntaxKind::IntersectionType => IntersectionTypeNode::new(node, types).into(),
            _ => panic!("Expected UnionType or IntersectionType"),
        }
    }

    pub fn create_union_type_node<
        TBaseNodeFactory: BaseNodeFactory,
        TElements: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        types: TElements, /*<TypeNode>*/
    ) -> TypeNode {
        self.create_union_or_intersection_type_node(base_factory, SyntaxKind::UnionType, types)
    }

    pub fn create_intersection_type_node<
        TBaseNodeFactory: BaseNodeFactory,
        TElements: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        types: TElements, /*<TypeNode>*/
    ) -> TypeNode {
        self.create_union_or_intersection_type_node(
            base_factory,
            SyntaxKind::IntersectionType,
            types,
        )
    }

    pub fn create_literal_type_node<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        literal: Rc<Node>,
    ) -> LiteralTypeNode {
        let node = self.create_token(base_factory, SyntaxKind::LiteralType);
        let node = LiteralTypeNode::new(node, literal);
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

    pub fn create_array_literal_expression<
        TBaseNodeFactory: BaseNodeFactory,
        TElements: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: TElements, /*Expression*/
    ) -> ArrayLiteralExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::ArrayLiteralExpression);
        let elements_as_node_array = match elements.into() {
            NodeArrayOrVec::NodeArray(node_array) => node_array,
            NodeArrayOrVec::Vec(elements) => NodeArray::new(elements),
        };
        let has_trailing_comma = {
            let last_element =
                last_or_undefined(Into::<&[Rc<Node>]>::into(&elements_as_node_array));
            if let Some(last_element) = last_element {
                if is_omitted_expression(&**last_element) {
                    Some(true)
                } else {
                    None
                }
            } else {
                None
            }
        };
        let elements_array = self.create_node_array(elements_as_node_array, has_trailing_comma);
        let node = ArrayLiteralExpression::new(node, elements_array);
        node
    }

    pub fn create_object_literal_expression<
        TBaseNodeFactory: BaseNodeFactory,
        TProperties: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        properties: TProperties, /*ObjectLiteralElementLike*/
    ) -> ObjectLiteralExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::ObjectLiteralExpression);
        let node = ObjectLiteralExpression::new(node, self.create_node_array(properties, None));
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
        let node = VariableDeclarationList::new(node, self.create_node_array(declarations, None));
        node
    }

    pub fn create_interface_declaration<
        TBaseNodeFactory: BaseNodeFactory,
        TMembers: Into<NodeArrayOrVec>,
        TTypeParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Rc<Node>,
        type_parameters: Option<TTypeParameters>,
        members: TMembers,
    ) -> InterfaceDeclaration {
        let node = self.create_base_interface_or_class_like_declaration(
            base_factory,
            SyntaxKind::InterfaceDeclaration,
            name,
            type_parameters,
        );
        let node = InterfaceDeclaration::new(node, self.create_node_array(members, None));
        node
    }

    pub fn create_property_assignment<TBaseNodeFactory: BaseNodeFactory>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Rc<Node>,
        initializer: Rc<Node>,
    ) -> PropertyAssignment {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::PropertyAssignment,
            Some(name),
        );
        let node = PropertyAssignment::new(node, initializer);
        node
    }

    pub fn create_source_file<TBaseNodeFactory: BaseNodeFactory, TNodes: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        statements: TNodes,
    ) -> SourceFile {
        let node = base_factory.create_base_source_file_node(SyntaxKind::SourceFile);
        let node = SourceFile::new(
            node,
            self.create_node_array(statements, None),
            "".to_string(),
            "".to_string(),
        );
        node
    }

    fn as_node_array<TArray: Into<NodeArrayOrVec>>(
        &self,
        array: Option<TArray>,
    ) -> Option<NodeArray> {
        array.map(|array| self.create_node_array(array, None))
    }

    fn as_name(&self, name: Rc<Node>) -> Rc<Node> {
        name
    }
}

pub fn create_node_factory() -> NodeFactory {
    NodeFactory {}
}

// lazy_static! {
//     static ref base_factory_static: BaseNodeFactoryConcrete = create_base_node_factory();
// }

fn make_synthetic(node: BaseNode) -> BaseNode {
    node
}

// lazy_static! {
//     pub static ref synthetic_factory: BaseNodeFactorySynthetic = BaseNodeFactorySynthetic::new();
// }

// pub fn get_synthetic_factory() -> &'static impl BaseNodeFactory {
//     &*synthetic_factory
// }

pub fn get_synthetic_factory() -> BaseNodeFactorySynthetic {
    BaseNodeFactorySynthetic::new()
}

#[derive(Debug)]
pub struct BaseNodeFactorySynthetic {
    base_factory: BaseNodeFactoryConcrete,
}

impl BaseNodeFactorySynthetic {
    pub fn new() -> Self {
        Self {
            base_factory: create_base_node_factory(),
        }
    }
}

impl BaseNodeFactory for BaseNodeFactorySynthetic {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(self.base_factory.create_base_source_file_node(kind))
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(self.base_factory.create_base_identifier_node(kind))
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(self.base_factory.create_base_token_node(kind))
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(self.base_factory.create_base_node(kind))
    }
}

lazy_static! {
    pub static ref factory: NodeFactory = create_node_factory();
}
