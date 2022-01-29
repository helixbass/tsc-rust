#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use crate::{
    add_range, append_if_unique, create_base_node_factory, create_parenthesizer_rules,
    escape_leading_underscores, is_call_chain, is_import_keyword, is_named_declaration,
    is_omitted_expression, is_property_name, is_super_property, last_or_undefined,
    null_parenthesizer_rules, pseudo_big_int_to_string, set_text_range, ArrayLiteralExpression,
    ArrayTypeNode, BaseBindingLikeDeclaration, BaseFunctionLikeDeclaration,
    BaseGenericNamedDeclaration, BaseInterfaceOrClassLikeDeclaration, BaseLiteralLikeNode,
    BaseNamedDeclaration, BaseNode, BaseNodeFactory, BaseNodeFactoryConcrete,
    BaseSignatureDeclaration, BaseVariableLikeDeclaration, BigIntLiteral, BinaryExpression, Block,
    CallExpression, Debug_, EmitFlags, EmitNode, EmptyStatement, Expression, ExpressionStatement,
    FunctionDeclaration, Identifier, IfStatement, InterfaceDeclaration, IntersectionTypeNode,
    LiteralLikeNode, LiteralLikeNodeInterface, LiteralTypeNode, Node, NodeArray, NodeArrayOrVec,
    NodeFactory, NodeFlags, NodeInterface, NumericLiteral, ObjectLiteralExpression,
    ParameterDeclaration, ParenthesizedExpression, ParenthesizerRules, PrefixUnaryExpression,
    PropertyAssignment, PropertySignature, PseudoBigInt, ReadonlyTextRange, ReturnStatement,
    ShorthandPropertyAssignment, SourceFile, SourceMapRange, Statement, StringLiteral, SyntaxKind,
    TemplateExpression, TemplateLiteralLikeNode, TemplateSpan, TokenFlags, TransformFlags,
    TypeAliasDeclaration, TypeLiteralNode, TypeNode, TypeParameterDeclaration, TypePredicateNode,
    TypeReferenceNode, UnionTypeNode, VariableDeclaration, VariableDeclarationList,
    VariableStatement,
};

bitflags! {
    pub struct NodeFactoryFlags: u32 {
        const None = 0;
        const NoParenthesizerRules = 1 << 0;
        const NoNodeConverters = 1 << 1;
        const NoIndentationOnFreshPropertyAccess = 1 << 2;
        const NoOriginalNode = 1 << 3;
    }
}

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn new(flags: NodeFactoryFlags) -> Rc<Self> {
        let factory_ = Rc::new(Self {
            flags,
            parenthesizer_rules: RefCell::new(None),
        });
        factory_.set_parenthesizer_rules(
            /*memoize(*/
            if flags.intersects(NodeFactoryFlags::NoParenthesizerRules) {
                Box::new(null_parenthesizer_rules())
            } else {
                Box::new(create_parenthesizer_rules(factory_.clone()))
            },
        );
        factory_
    }

    fn update(&self, updated: Rc<Node>, original: &Node) -> Rc<Node> {
        if self.flags.intersects(NodeFactoryFlags::NoOriginalNode) {
            update_without_original(updated, original)
        } else {
            update_with_original(updated, original)
        }
    }

    fn set_parenthesizer_rules(
        &self,
        parenthesizer_rules: Box<dyn ParenthesizerRules<TBaseNodeFactory>>,
    ) {
        *self.parenthesizer_rules.borrow_mut() = Some(parenthesizer_rules);
    }

    fn parenthesizer_rules(&self) -> Ref<Box<dyn ParenthesizerRules<TBaseNodeFactory>>> {
        Ref::map(self.parenthesizer_rules.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn create_node_array<TElements: Into<NodeArrayOrVec>>(
        &self,
        elements: Option<TElements>,
        has_trailing_comma: Option<bool>,
    ) -> NodeArray {
        let elements_is_none = elements.is_none();
        let elements = match elements {
            None => NodeArrayOrVec::Vec(vec![]),
            Some(elements) => elements.into(),
        };
        match elements {
            NodeArrayOrVec::NodeArray(mut elements) => {
                if match has_trailing_comma {
                    None => true,
                    Some(has_trailing_comma) => elements.has_trailing_comma == has_trailing_comma,
                } {
                    if elements.transform_flags.is_none() {
                        aggregate_children_flags(&mut elements);
                    }
                    Debug_.attach_node_array_debug_info(&mut elements);
                    return elements;
                }

                let mut array = NodeArray::new(
                    elements.to_vec(),
                    elements.pos(),
                    elements.end(),
                    has_trailing_comma.unwrap(),
                    elements.transform_flags,
                );
                Debug_.attach_node_array_debug_info(&mut array);
                array
            }
            NodeArrayOrVec::Vec(elements) => {
                // let length = elements.len();
                let array = /*length >= 1 && length <= 4 ? elements.slice() :*/ elements;
                let mut array =
                    NodeArray::new(array, -1, -1, has_trailing_comma.unwrap_or(false), None);
                aggregate_children_flags(&mut array);
                Debug_.attach_node_array_debug_info(&mut array);
                array
            }
        }
    }

    fn create_base_node(&self, base_factory: &TBaseNodeFactory, kind: SyntaxKind) -> BaseNode {
        base_factory.create_base_node(kind)
    }

    fn create_base_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> BaseNode {
        let mut node = self.create_base_node(base_factory, kind);
        node.set_decorators(self.as_node_array(decorators));
        node.modifiers = self.as_node_array(modifiers);
        node
    }

    fn create_base_named_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        name: Option<Rc<Node>>,
    ) -> BaseNamedDeclaration {
        let node = self.create_base_declaration(base_factory, kind, decorators, modifiers);
        BaseNamedDeclaration::new(node, name)
    }

    fn create_base_generic_named_declaration<TTypeParameters: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        name: Option<Rc<Node>>,
        type_parameters: Option<TTypeParameters>,
    ) -> BaseGenericNamedDeclaration {
        let node =
            self.create_base_named_declaration(base_factory, kind, decorators, modifiers, name);
        let node = BaseGenericNamedDeclaration::new(node, self.as_node_array(type_parameters));
        node
    }

    fn create_base_interface_or_class_like_declaration<TTypeParameters: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        name: Option<Rc<Node>>,
        type_parameters: Option<TTypeParameters>,
    ) -> BaseInterfaceOrClassLikeDeclaration {
        let node = self.create_base_generic_named_declaration(
            base_factory,
            kind,
            decorators,
            modifiers,
            name,
            type_parameters,
        );
        let node = BaseInterfaceOrClassLikeDeclaration::new(node);
        node
    }

    fn create_base_signature_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        name: Option<Rc<Node>>,
        type_parameters: Option<NodeArray>,
        parameters: Option<NodeArray>,
        type_: Option<Rc<Node>>,
    ) -> BaseSignatureDeclaration {
        let node = self.create_base_generic_named_declaration(
            base_factory,
            kind,
            decorators,
            modifiers,
            name,
            type_parameters,
        );
        let node =
            BaseSignatureDeclaration::new(node, self.create_node_array(parameters, None), type_);
        node
    }

    fn create_base_function_like_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        name: Option<Rc<Node>>,
        type_parameters: Option<NodeArray>,
        parameters: Option<NodeArray>,
        type_: Option<Rc<Node>>,
        body: Option<Rc<Node>>,
    ) -> BaseFunctionLikeDeclaration {
        let node = self.create_base_signature_declaration(
            base_factory,
            kind,
            decorators,
            modifiers,
            name,
            type_parameters,
            parameters,
            type_,
        );
        let node = BaseFunctionLikeDeclaration::new(node, body);
        node
    }

    fn create_base_binding_like_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        name: Option<Rc<Node>>,
        initializer: Option<Rc<Node>>,
    ) -> BaseBindingLikeDeclaration {
        let node =
            self.create_base_named_declaration(base_factory, kind, decorators, modifiers, name);
        BaseBindingLikeDeclaration::new(node, initializer)
    }

    fn create_base_variable_like_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        name: Option<Rc<Node>>,
        type_: Option<Rc<Node>>,
        initializer: Option<Rc<Node>>,
    ) -> BaseVariableLikeDeclaration {
        let node = self.create_base_binding_like_declaration(
            base_factory,
            kind,
            decorators,
            modifiers,
            name,
            initializer,
        );
        BaseVariableLikeDeclaration::new(node, type_)
    }

    fn create_base_literal(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        value: String,
    ) -> BaseLiteralLikeNode {
        let node = self.create_base_token(base_factory, kind);
        BaseLiteralLikeNode::new(node, value)
    }

    pub fn create_numeric_literal(
        &self,
        base_factory: &TBaseNodeFactory,
        value: String,
        numeric_literal_flags: Option<TokenFlags>,
    ) -> NumericLiteral {
        let numeric_literal_flags = numeric_literal_flags.unwrap_or(TokenFlags::None);
        let node = self.create_base_literal(base_factory, SyntaxKind::NumericLiteral, value);
        NumericLiteral::new(node)
    }

    pub fn create_big_int_literal<TPseudoBigIntOrString: Into<PseudoBigIntOrString>>(
        &self,
        base_factory: &TBaseNodeFactory,
        value: TPseudoBigIntOrString,
    ) -> BigIntLiteral {
        let value = value.into();
        let node = self.create_base_literal(
            base_factory,
            SyntaxKind::BigIntLiteral,
            match value {
                PseudoBigIntOrString::PseudoBigInt(pseudo_big_int) => {
                    format!("{}n", pseudo_big_int_to_string(&pseudo_big_int))
                }
                PseudoBigIntOrString::String(string) => string,
            },
        );
        BigIntLiteral::new(node)
    }

    pub fn create_base_string_literal(
        &self,
        base_factory: &TBaseNodeFactory,
        text: String,
        is_single_quote: Option<bool>,
    ) -> StringLiteral {
        let node = self.create_base_literal(base_factory, SyntaxKind::StringLiteral, text);
        StringLiteral::new(node, is_single_quote)
    }

    pub fn create_string_literal(
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

    pub fn create_literal_like_node(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind, /*LiteralToken["kind"] | SyntaxKind.JsxTextAllWhiteSpaces*/
        text: String,
    ) -> LiteralLikeNode {
        match kind {
            SyntaxKind::NumericLiteral => self
                .create_numeric_literal(base_factory, text, Some(TokenFlags::None))
                .into(),
            SyntaxKind::BigIntLiteral => self.create_big_int_literal(base_factory, text).into(),
            SyntaxKind::StringLiteral => self
                .create_string_literal(base_factory, text, None, None)
                .into(),
            _ => panic!("Unexpected kind"),
        }
    }

    fn create_base_identifier(&self, base_factory: &TBaseNodeFactory, text: &str) -> Identifier {
        let node = base_factory.create_base_identifier_node(SyntaxKind::Identifier);
        let node = Identifier::new(node, escape_leading_underscores(text));
        node
    }

    pub fn create_identifier(&self, base_factory: &TBaseNodeFactory, text: &str) -> Identifier {
        let node = self.create_base_identifier(base_factory, text);
        node
    }

    pub fn create_base_token(&self, base_factory: &TBaseNodeFactory, kind: SyntaxKind) -> BaseNode {
        base_factory.create_base_token_node(kind)
    }

    pub fn create_token(&self, base_factory: &TBaseNodeFactory, token: SyntaxKind) -> BaseNode {
        let node = self.create_base_token(base_factory, token);
        node
    }

    pub fn create_true(&self, base_factory: &TBaseNodeFactory) -> BaseNode {
        self.create_token(base_factory, SyntaxKind::TrueKeyword)
    }

    pub fn create_false(&self, base_factory: &TBaseNodeFactory) -> BaseNode {
        self.create_token(base_factory, SyntaxKind::FalseKeyword)
    }

    pub fn create_type_parameter_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Rc<Node>,
        constraint: Option<Rc<Node /*TypeNode*/>>,
        default_type: Option<Rc<Node /*TypeNode*/>>,
    ) -> TypeParameterDeclaration {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::TypeParameter,
            None,
            None,
            Some(name),
        );
        let node = TypeParameterDeclaration::new(node, constraint, default_type);
        node
    }

    pub fn create_parameter_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        dot_dot_dot_token: Option<Rc<Node /*DotDotDotToken*/>>,
        name: Rc<Node>,
        question_token: Option<Rc<Node /*QuestionToken*/>>,
        type_: Option<Rc<Node /*TypeNode*/>>,
        initializer: Option<Rc<Node /*Expression*/>>,
    ) -> ParameterDeclaration {
        let node = self.create_base_variable_like_declaration(
            base_factory,
            SyntaxKind::Parameter,
            decorators,
            modifiers,
            Some(name),
            type_,
            initializer,
        );
        let node = ParameterDeclaration::new(node, dot_dot_dot_token, question_token);
        node
    }

    pub fn create_property_signature(
        &self,
        base_factory: &TBaseNodeFactory,
        modifiers: Option<NodeArray>,
        name: Rc<Node>,
        question_token: Option<Rc<Node>>,
        type_: Option<Rc<Node>>,
    ) -> PropertySignature {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::PropertySignature,
            None,
            modifiers,
            Some(name),
        );
        let node = PropertySignature::new(node, question_token, type_);
        node
    }

    pub fn create_keyword_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        token: SyntaxKind,
    ) -> BaseNode {
        self.create_token(base_factory, token)
    }

    pub fn create_type_predicate_node(
        &self,
        base_factory: &TBaseNodeFactory,
        asserts_modifier: Option<Rc<Node>>,
        parameter_name: Rc<Node>,
        type_: Option<Rc<Node>>,
    ) -> TypePredicateNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TypePredicate);
        let node = TypePredicateNode::new(node, asserts_modifier, parameter_name, type_);
        node
    }

    pub fn create_type_reference_node<TTypeArguments: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        type_name: Rc<Node>,
        type_arguments: Option<TTypeArguments>,
    ) -> TypeReferenceNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TypeReference);
        let node = TypeReferenceNode::new(
            node,
            self.as_name(type_name),
            type_arguments.map(|type_arguments| self.create_node_array(Some(type_arguments), None)),
        );
        node
    }

    pub fn create_type_literal_node(
        &self,
        base_factory: &TBaseNodeFactory,
        members: Option<Vec<Rc<Node>>>,
    ) -> TypeLiteralNode {
        let node = self.create_base_node(base_factory, SyntaxKind::TypeLiteral);
        let node = TypeLiteralNode::new(node, self.create_node_array(members, None));
        node
    }

    pub fn create_array_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        element_type: Rc<Node>,
    ) -> ArrayTypeNode {
        let node = self.create_base_node(base_factory, SyntaxKind::ArrayType);
        let node = ArrayTypeNode::new(node, element_type);
        node
    }

    pub fn create_union_or_intersection_type_node<TElements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind, /*SyntaxKind.UnionType | SyntaxKind.IntersectionType*/
        types: TElements, /*<TypeNode>*/
    ) -> TypeNode {
        let node = self.create_base_node(base_factory, kind);
        let types = self
            .parenthesizer_rules()
            .parenthesize_constituent_types_of_union_or_intersection_type(
                base_factory,
                types.into(),
            );
        match kind {
            SyntaxKind::UnionType => UnionTypeNode::new(node, types).into(),
            SyntaxKind::IntersectionType => IntersectionTypeNode::new(node, types).into(),
            _ => panic!("Expected UnionType or IntersectionType"),
        }
    }

    pub fn create_union_type_node<TElements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        types: TElements, /*<TypeNode>*/
    ) -> TypeNode {
        self.create_union_or_intersection_type_node(base_factory, SyntaxKind::UnionType, types)
    }

    pub fn create_intersection_type_node<TElements: Into<NodeArrayOrVec>>(
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

    pub fn create_literal_type_node(
        &self,
        base_factory: &TBaseNodeFactory,
        literal: Rc<Node>,
    ) -> LiteralTypeNode {
        let node = self.create_token(base_factory, SyntaxKind::LiteralType);
        let node = LiteralTypeNode::new(node, literal);
        node
    }

    fn create_base_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
    ) -> BaseNode {
        let node = self.create_base_node(base_factory, kind);
        node
    }

    pub fn create_array_literal_expression<TElements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: Option<TElements>, /*Expression*/
    ) -> ArrayLiteralExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::ArrayLiteralExpression);
        let elements = elements.map(|elements| match elements.into() {
            NodeArrayOrVec::NodeArray(node_array) => node_array.to_vec(),
            NodeArrayOrVec::Vec(elements) => elements,
        });
        let last_element = elements
            .as_ref()
            .and_then(|elements| last_or_undefined(&elements));
        let has_trailing_comma = last_element.and_then(|last_element| {
            if is_omitted_expression(last_element) {
                Some(true)
            } else {
                None
            }
        });
        let elements_array = self.create_node_array(elements, has_trailing_comma);
        let node = ArrayLiteralExpression::new(node, elements_array);
        node
    }

    pub fn create_object_literal_expression<TProperties: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        properties: Option<TProperties>, /*ObjectLiteralElementLike*/
    ) -> ObjectLiteralExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::ObjectLiteralExpression);
        let node = ObjectLiteralExpression::new(node, self.create_node_array(properties, None));
        node
    }

    pub fn create_call_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
        type_arguments: Option<Vec<Rc<Node /*TypeNode*/>>>,
        arguments_array: Vec<Rc<Node /*expression*/>>,
    ) -> CallExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::CallExpression);
        let node = CallExpression::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_left_side_of_access(base_factory, &expression),
            None,
            self.as_node_array(type_arguments),
            self.parenthesizer_rules()
                .parenthesize_expressions_of_comma_delimited_list(
                    base_factory,
                    self.create_node_array(Some(arguments_array), None),
                ),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_children_flags(node.type_arguments.as_ref())
                | propagate_children_flags(Some(&node.arguments)),
        );
        if node.type_arguments.is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        if is_import_keyword(&node.expression) {
            node.add_transform_flags(TransformFlags::ContainsDynamicImport);
        } else if is_super_property(&node.expression) {
            node.add_transform_flags(TransformFlags::ContainsLexicalThis);
        }
        node
    }

    pub fn update_call_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node,       /*CallExpression*/
        expression: &Node, /*Expression*/
        type_arguments: Option<&[Rc<Node /*TypeNode*/>]>,
        arguments_array: &[Rc<Node /*expression*/>],
    ) -> Rc<Node /*CallExpression*/> {
        let node_as_call_expression = node.as_call_expression();
        if is_call_chain(node) {
            return self.update_call_chain(
                node,
                expression,
                &node_as_call_expression.question_dot_token,
                type_arguments,
                arguments_array,
            );
        }
        if !ptr::eq(&*node_as_call_expression.expression, expression)
            || !match (
                node_as_call_expression.type_arguments.as_ref(),
                type_arguments,
            ) {
                (Some(node_type_arguments), Some(type_arguments)) => {
                    node_type_arguments.len() == type_arguments.len()
                        && node_type_arguments.iter().enumerate().all(
                            |(index, node_type_argument)| {
                                Rc::ptr_eq(node_type_argument, &type_arguments[index])
                            },
                        )
                }
                (None, None) => true,
                _ => false,
            }
            || !(node_as_call_expression.arguments.len() == arguments_array.len()
                && node_as_call_expression.arguments.iter().enumerate().all(
                    |(index, node_argument)| Rc::ptr_eq(node_argument, &arguments_array[index]),
                ))
        {
            self.update(
                self.create_call_expression(
                    base_factory,
                    expression.node_wrapper(),
                    type_arguments.map(|type_arguments| type_arguments.to_vec()),
                    arguments_array.to_vec(),
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_parenthesized_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
    ) -> ParenthesizedExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::ParenthesizedExpression);
        let node = ParenthesizedExpression::new(node, expression);
        node
    }

    pub fn create_prefix_unary_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        operator: SyntaxKind,
        operand: Rc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::PrefixUnaryExpression);
        let node = PrefixUnaryExpression::new(node, operator, operand);
        node
    }

    pub fn create_binary_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        operator: Rc<Node>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::BinaryExpression);
        let node = BinaryExpression::new(node, left, operator, right);
        node
    }

    pub fn create_template_expression<TTemplateSpans: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        head: Rc<Node /*TemplateHead*/>,
        template_spans: TTemplateSpans,
    ) -> TemplateExpression {
        let node = self.create_base_expression(base_factory, SyntaxKind::TemplateExpression);
        let node = TemplateExpression::new(
            node,
            head,
            self.create_node_array(Some(template_spans), None),
        );
        node
    }

    pub fn create_template_literal_like_node(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        text: String,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> TemplateLiteralLikeNode {
        let template_flags = template_flags.unwrap_or(TokenFlags::None);
        let node = self.create_base_token(base_factory, kind);
        let node = BaseLiteralLikeNode::new(node, text);
        let node = TemplateLiteralLikeNode::new(
            node,
            raw_text,
            Some(template_flags & TokenFlags::TemplateLiteralLikeFlags),
        );
        node
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
            None,
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
        expression: Expression,
    ) -> ExpressionStatement {
        ExpressionStatement::new(
            self.create_base_node(base_factory, SyntaxKind::ExpressionStatement),
            expression.into(),
        )
    }

    pub fn create_if_statement(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Expression,
        then_statement: Statement,
        else_statement: Option<Statement>,
    ) -> IfStatement {
        let node = self.create_base_node(base_factory, SyntaxKind::IfStatement);
        let node = IfStatement::new(
            node,
            expression.into(),
            self.as_embedded_statement(Some(then_statement.into()))
                .unwrap(),
            self.as_embedded_statement(else_statement.map(|else_statement| else_statement.into())),
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
            None,
            None,
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
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        name: Rc<Node>,
        type_parameters: Option<TTypeParameters>,
        members: TMembers,
    ) -> InterfaceDeclaration {
        let node = self.create_base_interface_or_class_like_declaration(
            base_factory,
            SyntaxKind::InterfaceDeclaration,
            decorators,
            modifiers,
            Some(name),
            type_parameters,
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

    pub fn create_property_assignment(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Rc<Node>,
        initializer: Rc<Node>,
    ) -> PropertyAssignment {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::PropertyAssignment,
            None,
            None,
            Some(name),
        );
        let node = PropertyAssignment::new(node, initializer);
        node
    }

    pub fn create_shorthand_property_assignment(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Rc<Node>,
        object_assignment_initializer: Option<Rc<Node>>,
    ) -> ShorthandPropertyAssignment {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::PropertyAssignment,
            None,
            None,
            Some(name),
        );
        let mut node = ShorthandPropertyAssignment::new(
            node,
            object_assignment_initializer.map(|object_assignment_initializer| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(
                        base_factory,
                        &object_assignment_initializer,
                    )
            }),
        );
        node.add_transform_flags(
            propagate_child_flags(
                node.object_assignment_initializer
                    .as_ref()
                    .map(|rc| rc.clone()),
            ) | TransformFlags::ContainsES2015,
        );
        node
    }

    pub fn create_source_file<TNodes: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        statements: TNodes,
    ) -> SourceFile {
        let node = base_factory.create_base_source_file_node(SyntaxKind::SourceFile);
        let node = SourceFile::new(
            node,
            self.create_node_array(Some(statements), None),
            "".to_string(),
            "".to_string(),
        );
        node
    }

    fn as_node_array<TArray: Into<NodeArrayOrVec>>(
        &self,
        array: Option<TArray>,
    ) -> Option<NodeArray> {
        array.map(|array| self.create_node_array(Some(array), None))
    }

    fn as_name(&self, name: Rc<Node>) -> Rc<Node> {
        name
    }

    fn as_embedded_statement(&self, statement: Option<Rc<Node>>) -> Option<Rc<Node>> {
        if false {
            unimplemented!()
        } else {
            statement
        }
    }
}

pub fn create_node_factory<TBaseNodeFactory: 'static + BaseNodeFactory>(
    flags: NodeFactoryFlags, /*, baseFactory: BaseNodeFactory*/
) -> Rc<NodeFactory<TBaseNodeFactory>> {
    NodeFactory::new(flags)
}

fn update_without_original(updated: Rc<Node>, original: &Node) -> Rc<Node> {
    if !ptr::eq(&*updated, original) {
        set_text_range(&*updated, Some(original));
    }
    updated
}

fn update_with_original(updated: Rc<Node>, original: &Node) -> Rc<Node> {
    if !ptr::eq(&*updated, original) {
        set_original_node(updated, Some(original.node_wrapper()));
        set_text_range(&*updated, Some(original));
    }
    updated
}

fn propagate_property_name_flags_of_child(
    node: &Node, /*PropertyName*/
    transform_flags: TransformFlags,
) -> TransformFlags {
    transform_flags | (node.transform_flags() & TransformFlags::PropertyNamePropagatingFlags)
}

fn propagate_child_flags<TNode: Borrow<Node>>(child: Option<TNode>) -> TransformFlags {
    if child.is_none() {
        return TransformFlags::None;
    }
    let child = child.unwrap();
    let child = child.borrow();
    let child_flags =
        child.transform_flags() & !get_transform_flags_subtree_exclusions(child.kind());
    if is_named_declaration(child) && is_property_name(&*child.as_named_declaration().name()) {
        propagate_property_name_flags_of_child(&child.as_named_declaration().name(), child_flags)
    } else {
        child_flags
    }
}

fn propagate_children_flags(children: Option<&NodeArray>) -> TransformFlags {
    children.map_or(TransformFlags::None, |children| {
        children.transform_flags.unwrap()
    })
}

fn aggregate_children_flags(children: &mut NodeArray) {
    let mut subtree_flags = TransformFlags::None;
    for child in children.iter() {
        subtree_flags |= propagate_child_flags(Some(&**child));
    }
    children.transform_flags = Some(subtree_flags);
}

pub(crate) fn get_transform_flags_subtree_exclusions(kind: SyntaxKind) -> TransformFlags {
    if kind >= SyntaxKind::FirstTypeNode && kind <= SyntaxKind::LastTypeNode {
        return TransformFlags::TypeExcludes;
    }

    match kind {
        SyntaxKind::CallExpression
        | SyntaxKind::NewExpression
        | SyntaxKind::ArrayLiteralExpression => TransformFlags::ArrayLiteralOrCallOrNewExcludes,
        SyntaxKind::ModuleDeclaration => TransformFlags::ModuleExcludes,
        SyntaxKind::Parameter => TransformFlags::ParameterExcludes,
        SyntaxKind::ArrowFunction => TransformFlags::ArrowFunctionExcludes,
        SyntaxKind::FunctionExpression | SyntaxKind::FunctionDeclaration => {
            TransformFlags::FunctionExcludes
        }
        SyntaxKind::VariableDeclarationList => TransformFlags::VariableDeclarationListExcludes,
        SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression => TransformFlags::ClassExcludes,
        SyntaxKind::Constructor => TransformFlags::ConstructorExcludes,
        SyntaxKind::PropertyDeclaration => TransformFlags::PropertyExcludes,
        SyntaxKind::MethodDeclaration | SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
            TransformFlags::MethodOrAccessorExcludes
        }
        SyntaxKind::AnyKeyword
        | SyntaxKind::NumberKeyword
        | SyntaxKind::BigIntKeyword
        | SyntaxKind::NeverKeyword
        | SyntaxKind::StringKeyword
        | SyntaxKind::ObjectKeyword
        | SyntaxKind::BooleanKeyword
        | SyntaxKind::SymbolKeyword
        | SyntaxKind::VoidKeyword
        | SyntaxKind::TypeParameter
        | SyntaxKind::PropertySignature
        | SyntaxKind::MethodSignature
        | SyntaxKind::CallSignature
        | SyntaxKind::ConstructSignature
        | SyntaxKind::IndexSignature
        | SyntaxKind::InterfaceDeclaration
        | SyntaxKind::TypeAliasDeclaration => TransformFlags::TypeExcludes,
        SyntaxKind::ObjectLiteralExpression => TransformFlags::ObjectLiteralExcludes,
        SyntaxKind::CatchClause => TransformFlags::CatchClauseExcludes,
        SyntaxKind::ObjectBindingPattern | SyntaxKind::ArrayBindingPattern => {
            TransformFlags::BindingPatternExcludes
        }
        SyntaxKind::TypeAssertionExpression
        | SyntaxKind::AsExpression
        | SyntaxKind::PartiallyEmittedExpression
        | SyntaxKind::ParenthesizedExpression
        | SyntaxKind::SuperKeyword => TransformFlags::OuterExpressionExcludes,
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
            TransformFlags::PropertyAccessExcludes
        }
        _ => TransformFlags::NodeExcludes,
    }
}

thread_local! {
    static base_factory_static: BaseNodeFactoryConcrete = create_base_node_factory();
}

fn make_synthetic(node: BaseNode) -> BaseNode {
    node.set_flags(node.flags() | NodeFlags::Synthesized);
    node
}

thread_local! {
    pub static synthetic_factory: BaseNodeFactorySynthetic = BaseNodeFactorySynthetic::new();
}

// pub fn get_synthetic_factory() -> BaseNodeFactorySynthetic {
//     BaseNodeFactorySynthetic::new()
// }

#[derive(Debug)]
pub struct BaseNodeFactorySynthetic {}

impl BaseNodeFactorySynthetic {
    pub fn new() -> Self {
        Self {}
    }
}

impl BaseNodeFactory for BaseNodeFactorySynthetic {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(
            base_factory_static
                .with(|base_factory| base_factory.create_base_source_file_node(kind)),
        )
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(
            base_factory_static.with(|base_factory| base_factory.create_base_identifier_node(kind)),
        )
    }

    fn create_base_private_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(
            base_factory_static
                .with(|base_factory| base_factory.create_base_private_identifier_node(kind)),
        )
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(
            base_factory_static.with(|base_factory| base_factory.create_base_token_node(kind)),
        )
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(base_factory_static.with(|base_factory| base_factory.create_base_node(kind)))
    }
}

thread_local! {
    pub static factory: Rc<NodeFactory<BaseNodeFactorySynthetic>> =
        create_node_factory::<BaseNodeFactorySynthetic>(NodeFactoryFlags::NoIndentationOnFreshPropertyAccess);
}

pub enum PseudoBigIntOrString {
    PseudoBigInt(PseudoBigInt),
    String(String),
}

impl From<PseudoBigInt> for PseudoBigIntOrString {
    fn from(pseudo_big_int: PseudoBigInt) -> Self {
        PseudoBigIntOrString::PseudoBigInt(pseudo_big_int)
    }
}

impl From<String> for PseudoBigIntOrString {
    fn from(string: String) -> Self {
        PseudoBigIntOrString::String(string)
    }
}

pub fn set_original_node(node: Rc<Node>, original: Option<Rc<Node>>) -> Rc<Node> {
    node.set_original(original.clone());
    if let Some(original) = original {
        let emit_node = original.maybe_emit_node();
        if let Some(emit_node) = emit_node.as_ref() {
            let node_emit_node = node.maybe_emit_node();
            if node_emit_node.is_none() {
                *node_emit_node = Some(Default::default());
            }
            merge_emit_node(
                emit_node,
                &mut *RefMut::map(node_emit_node, |option| option.as_mut().unwrap()),
            );
            // node.set_emit_node(node_emit_node);
        }
    }
    node
}

fn merge_emit_node(
    source_emit_node: &EmitNode,
    dest_emit_node: /*Option<*/ &mut EmitNode, /*>*/
) /*-> EmitNode*/
{
    let flags = source_emit_node.flags.as_ref();
    let leading_comments = source_emit_node.leading_comments.as_ref();
    let trailing_comments = source_emit_node.trailing_comments.as_ref();
    let comment_range = source_emit_node.comment_range.as_ref();
    let source_map_range = source_emit_node.source_map_range.as_ref();
    let token_source_map_ranges = source_emit_node.token_source_map_ranges.as_ref();
    let constant_value = source_emit_node.constant_value.as_ref();
    let helpers = source_emit_node.helpers.as_ref();
    let starts_on_new_line = source_emit_node.starts_on_new_line.as_ref();
    if let Some(leading_comments) = leading_comments {
        let mut new_leading_comments = leading_comments.to_vec();
        add_range(
            &mut new_leading_comments,
            dest_emit_node.leading_comments.as_deref(),
            None,
            None,
        );
        dest_emit_node.leading_comments = Some(new_leading_comments);
    }
    if let Some(trailing_comments) = trailing_comments {
        let mut new_trailing_comments = trailing_comments.to_vec();
        add_range(
            &mut new_trailing_comments,
            dest_emit_node.trailing_comments.as_deref(),
            None,
            None,
        );
        dest_emit_node.trailing_comments = Some(new_trailing_comments);
    }
    // TODO: should this technically also check "truthiness" of flags?
    if let Some(flags) = flags {
        dest_emit_node.flags = Some(*flags & !EmitFlags::Immutable);
    }
    if comment_range.is_some() {
        dest_emit_node.comment_range = comment_range.map(Clone::clone);
    }
    if source_map_range.is_some() {
        dest_emit_node.source_map_range = source_map_range.map(Clone::clone);
    }
    if let Some(token_source_map_ranges) = token_source_map_ranges {
        dest_emit_node.token_source_map_ranges = Some(merge_token_source_map_ranges(
            token_source_map_ranges,
            dest_emit_node.token_source_map_ranges.as_ref(),
        ));
    }
    if constant_value.is_some() {
        dest_emit_node.constant_value = constant_value.map(Clone::clone);
    }
    if let Some(helpers) = helpers {
        let mut dest_emit_node_helpers = dest_emit_node.helpers.clone();
        for helper in helpers {
            dest_emit_node_helpers = Some(append_if_unique(dest_emit_node_helpers, helper.clone()));
        }
        dest_emit_node.helpers = dest_emit_node_helpers;
    }
    if starts_on_new_line.is_some() {
        dest_emit_node.starts_on_new_line = starts_on_new_line.map(Clone::clone);
    }
    // return destEmitNode
}

fn merge_token_source_map_ranges(
    source_ranges: &HashMap<SyntaxKind, Option<Rc<SourceMapRange>>>,
    dest_ranges: Option<&HashMap<SyntaxKind, Option<Rc<SourceMapRange>>>>,
) -> HashMap<SyntaxKind, Option<Rc<SourceMapRange>>> {
    let mut dest_ranges =
        dest_ranges.map_or_else(|| HashMap::new(), |dest_ranges| dest_ranges.clone());
    for (key, value) in source_ranges {
        dest_ranges.insert(*key, value.clone());
    }
    dest_ranges
}
