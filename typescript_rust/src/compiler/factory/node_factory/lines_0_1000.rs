#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use gc::{Gc, GcCellRef};
use std::cell::RefCell;

use super::{
    aggregate_children_flags, propagate_child_flags, propagate_children_flags,
    propagate_identifier_name_flags, update_with_original, update_without_original,
    PseudoBigIntOrString,
};
use crate::{
    create_node_converters, create_parenthesizer_rules, escape_leading_underscores,
    get_text_of_identifier_or_literal, id_text, is_identifier, null_node_converters,
    null_parenthesizer_rules, pseudo_big_int_to_string, starts_with, string_to_token,
    BaseBindingLikeDeclaration, BaseFunctionLikeDeclaration, BaseGenericNamedDeclaration,
    BaseInterfaceOrClassLikeDeclaration, BaseJSDocTag, BaseJSDocTypeLikeTag, BaseJSDocUnaryType,
    BaseLiteralLikeNode, BaseNamedDeclaration, BaseNode, BaseNodeFactory, BaseSignatureDeclaration,
    BaseVariableLikeDeclaration, BigIntLiteral, BinaryExpression, ClassLikeDeclarationBase,
    ClassLikeDeclarationInterface, Debug_, FunctionLikeDeclarationInterface,
    GeneratedIdentifierFlags, HasInitializerInterface, HasTypeArgumentsInterface, HasTypeInterface,
    HasTypeParametersInterface, Identifier, InterfaceOrClassLikeDeclarationInterface,
    LiteralLikeNodeInterface, Node, NodeArray, NodeArrayOrVec, NodeConverters, NodeFactory,
    NodeInterface, Number, NumericLiteral, ParenthesizerRules, PostfixUnaryExpression,
    PrefixUnaryExpression, PrivateIdentifier, ReadonlyTextRange, RegularExpressionLiteral,
    SignatureDeclarationInterface, StringLiteral, StringOrNodeArray, SyntaxKind, TokenFlags,
    TransformFlags,
};

thread_local! {
    pub(super) static next_auto_generate_id: RefCell<usize> = RefCell::new(0);
}

pub(super) fn get_next_auto_generate_id() -> usize {
    next_auto_generate_id.with(|_next_auto_generate_id| *_next_auto_generate_id.borrow())
}

pub(super) fn increment_next_auto_generate_id() {
    next_auto_generate_id.with(|_next_auto_generate_id| {
        *_next_auto_generate_id.borrow_mut() += 1;
    });
}

bitflags! {
    pub struct NodeFactoryFlags: u32 {
        const None = 0;
        const NoParenthesizerRules = 1 << 0;
        const NoNodeConverters = 1 << 1;
        const NoIndentationOnFreshPropertyAccess = 1 << 2;
        const NoOriginalNode = 1 << 3;
    }
}

pub fn create_node_factory<TBaseNodeFactory: 'static + BaseNodeFactory>(
    flags: NodeFactoryFlags, /*, baseFactory: BaseNodeFactory*/
) -> Gc<NodeFactory<TBaseNodeFactory>> {
    NodeFactory::new(flags)
}

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn new(flags: NodeFactoryFlags) -> Gc<Self> {
        let factory_ = Gc::new(Self {
            flags,
            parenthesizer_rules: Default::default(),
            converters: Default::default(),
        });
        factory_.set_parenthesizer_rules(
            /*memoize(*/
            if flags.intersects(NodeFactoryFlags::NoParenthesizerRules) {
                Gc::new(Box::new(null_parenthesizer_rules()))
            } else {
                Gc::new(Box::new(create_parenthesizer_rules(factory_.clone())))
            },
        );
        factory_.set_converters(
            /*memoize(*/
            if flags.intersects(NodeFactoryFlags::NoParenthesizerRules) {
                Box::new(null_node_converters())
            } else {
                Box::new(create_node_converters(factory_.clone()))
            },
        );
        factory_
    }

    pub(crate) fn update(&self, updated: Gc<Node>, original: &Node) -> Gc<Node> {
        if self.flags.intersects(NodeFactoryFlags::NoOriginalNode) {
            update_without_original(updated, original)
        } else {
            update_with_original(updated, original)
        }
    }

    pub(crate) fn set_parenthesizer_rules(
        &self,
        parenthesizer_rules: Gc<Box<dyn ParenthesizerRules<TBaseNodeFactory>>>,
    ) {
        *self.parenthesizer_rules.borrow_mut() = Some(parenthesizer_rules);
    }

    pub(crate) fn parenthesizer_rules(&self) -> Gc<Box<dyn ParenthesizerRules<TBaseNodeFactory>>> {
        self.parenthesizer_rules.borrow().clone().unwrap()
    }

    pub fn parenthesizer(&self) -> Gc<Box<dyn ParenthesizerRules<TBaseNodeFactory>>> {
        self.parenthesizer_rules()
    }

    pub(crate) fn set_converters(
        &self,
        node_converters: Box<dyn NodeConverters<TBaseNodeFactory>>,
    ) {
        *self.converters.borrow_mut() = Some(node_converters);
    }

    pub fn converters(&self) -> GcCellRef<Box<dyn NodeConverters<TBaseNodeFactory>>> {
        GcCellRef::map(self.converters.borrow(), |option| option.as_ref().unwrap())
    }

    pub fn create_jsdoc_all_type(&self, base_factory: &TBaseNodeFactory) -> BaseNode {
        self.create_jsdoc_primary_type_worker(base_factory, SyntaxKind::JSDocAllType)
    }

    pub fn create_jsdoc_unknown_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(base_factory, SyntaxKind::JSDocUnknownType, type_)
    }

    pub fn create_jsdoc_non_nullable_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(base_factory, SyntaxKind::JSDocNonNullableType, type_)
    }

    pub fn create_jsdoc_nullable_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(base_factory, SyntaxKind::JSDocNullableType, type_)
    }

    pub fn create_jsdoc_optional_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(base_factory, SyntaxKind::JSDocOptionalType, type_)
    }

    pub fn create_jsdoc_variadic_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(base_factory, SyntaxKind::JSDocVariadicType, type_)
    }

    pub fn create_jsdoc_namepath_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(base_factory, SyntaxKind::JSDocNamepathType, type_)
    }

    pub fn create_jsdoc_type_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
        comment: Option<TComment /*<JSDocComment>*/>,
    ) -> BaseJSDocTypeLikeTag {
        self.create_jsdoc_type_like_tag_worker(
            base_factory,
            SyntaxKind::JSDocTypeTag,
            tag_name,
            type_expression,
            comment,
        )
    }

    pub fn create_jsdoc_return_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
        comment: Option<TComment /*<JSDocComment>*/>,
    ) -> BaseJSDocTypeLikeTag {
        self.create_jsdoc_type_like_tag_worker(
            base_factory,
            SyntaxKind::JSDocReturnTag,
            tag_name,
            type_expression,
            comment,
        )
    }

    pub fn create_jsdoc_this_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
        comment: Option<TComment /*<JSDocComment>*/>,
    ) -> BaseJSDocTypeLikeTag {
        self.create_jsdoc_type_like_tag_worker(
            base_factory,
            SyntaxKind::JSDocThisTag,
            tag_name,
            type_expression,
            comment,
        )
    }

    pub fn create_jsdoc_enum_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
        comment: Option<TComment /*<JSDocComment>*/>,
    ) -> BaseJSDocTypeLikeTag {
        self.create_jsdoc_type_like_tag_worker(
            base_factory,
            SyntaxKind::JSDocEnumTag,
            tag_name,
            type_expression,
            comment,
        )
    }

    pub fn create_jsdoc_author_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        comment: Option<TComment /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocAuthorTag,
            tag_name,
            comment,
        )
    }

    pub fn create_jsdoc_class_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        comment: Option<TComment /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocClassTag,
            tag_name,
            comment,
        )
    }

    pub fn create_jsdoc_public_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        comment: Option<TComment /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocPublicTag,
            tag_name,
            comment,
        )
    }

    pub fn create_jsdoc_private_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        comment: Option<TComment /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocPrivateTag,
            tag_name,
            comment,
        )
    }

    pub fn create_jsdoc_protected_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        comment: Option<TComment /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocProtectedTag,
            tag_name,
            comment,
        )
    }

    pub fn create_jsdoc_readonly_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        comment: Option<TComment /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocReadonlyTag,
            tag_name,
            comment,
        )
    }

    pub fn create_jsdoc_override_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        comment: Option<TComment /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocOverrideTag,
            tag_name,
            comment,
        )
    }

    pub fn create_jsdoc_deprecated_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        comment: Option<TComment /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocDeprecatedTag,
            tag_name,
            comment,
        )
    }

    pub fn create_comma(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::CommaToken, right)
    }

    pub fn create_assignment(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::EqualsToken, right)
    }

    pub fn create_logical_or(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::BarBarToken, right)
    }

    pub fn create_logical_and(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(
            base_factory,
            left,
            SyntaxKind::AmpersandAmpersandToken,
            right,
        )
    }

    pub fn create_bitwise_or(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::BarToken, right)
    }

    pub fn create_bitwise_xor(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::CaretToken, right)
    }

    pub fn create_bitwise_and(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::AmpersandToken, right)
    }

    pub fn create_strict_equality(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(
            base_factory,
            left,
            SyntaxKind::EqualsEqualsEqualsToken,
            right,
        )
    }

    pub fn create_strict_inequality(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(
            base_factory,
            left,
            SyntaxKind::ExclamationEqualsEqualsToken,
            right,
        )
    }

    pub fn create_equality(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::EqualsEqualsToken, right)
    }

    pub fn create_inequality(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(
            base_factory,
            left,
            SyntaxKind::ExclamationEqualsToken,
            right,
        )
    }

    pub fn create_less_than(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::LessThanToken, right)
    }

    pub fn create_less_than_equals(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::LessThanEqualsToken, right)
    }

    pub fn create_greater_than(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::GreaterThanToken, right)
    }

    pub fn create_greater_than_equals(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(
            base_factory,
            left,
            SyntaxKind::GreaterThanEqualsToken,
            right,
        )
    }

    pub fn create_left_shift(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::LessThanLessThanToken, right)
    }

    pub fn create_right_shift(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(
            base_factory,
            left,
            SyntaxKind::GreaterThanGreaterThanToken,
            right,
        )
    }

    pub fn create_unsigned_right_shift(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(
            base_factory,
            left,
            SyntaxKind::GreaterThanGreaterThanGreaterThanToken,
            right,
        )
    }

    pub fn create_add(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::PlusToken, right)
    }

    pub fn create_subtract(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::MinusToken, right)
    }

    pub fn create_multiply(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::AsteriskToken, right)
    }

    pub fn create_divide(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::SlashToken, right)
    }

    pub fn create_modulo(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::PercentToken, right)
    }

    pub fn create_exponent(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::AsteriskAsteriskToken, right)
    }

    pub fn create_prefix_plus(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Gc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression(base_factory, SyntaxKind::MinusToken, operand)
    }

    pub fn create_prefix_increment(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Gc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression(base_factory, SyntaxKind::PlusPlusToken, operand)
    }

    pub fn create_prefix_decrement(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Gc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression(base_factory, SyntaxKind::MinusMinusToken, operand)
    }

    pub fn create_bitwise_not(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Gc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression(base_factory, SyntaxKind::TildeToken, operand)
    }

    pub fn create_logical_not(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Gc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression(base_factory, SyntaxKind::ExclamationToken, operand)
    }

    pub fn create_postfix_increment(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Gc<Node /*Expression*/>,
    ) -> PostfixUnaryExpression {
        self.create_postfix_unary_expression(base_factory, operand, SyntaxKind::PlusPlusToken)
    }

    pub fn create_postfix_decrement(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Gc<Node /*Expression*/>,
    ) -> PostfixUnaryExpression {
        self.create_postfix_unary_expression(base_factory, operand, SyntaxKind::MinusMinusToken)
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

    pub(crate) fn create_base_node(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
    ) -> BaseNode {
        base_factory.create_base_node(kind)
    }

    pub(crate) fn create_base_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
    ) -> BaseNode {
        let mut node = self.create_base_node(base_factory, kind);
        node.set_decorators(self.as_node_array(decorators));
        node.set_modifiers(self.as_node_array(modifiers));
        let flags = propagate_children_flags(node.maybe_decorators().as_ref())
            | propagate_children_flags(node.maybe_modifiers().as_ref());
        node.add_transform_flags(flags);
        // node.symbol = undefined!;
        // node.localSymbol = undefined!;
        // node.locals = undefined!;
        // node.nextContainer = undefined!;
        node
    }

    pub(crate) fn create_base_named_declaration<
        'name,
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StrOrRcNode<'name>>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: Option<TName>,
    ) -> BaseNamedDeclaration {
        let node = self.create_base_declaration(base_factory, kind, decorators, modifiers);
        let name = self.as_name(base_factory, name);
        let mut node = BaseNamedDeclaration::new(node, name.clone());

        if let Some(name) = name {
            match node.kind() {
                SyntaxKind::MethodDeclaration
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor
                | SyntaxKind::PropertyDeclaration
                | SyntaxKind::PropertyAssignment => {
                    if is_identifier(&name) {
                        node.add_transform_flags(propagate_identifier_name_flags(&name));
                    }
                }
                _ => {
                    node.add_transform_flags(propagate_child_flags(Some(&*name)));
                }
            }
        }
        node
    }

    pub(crate) fn create_base_generic_named_declaration<
        'name,
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StrOrRcNode<'name>>,
        TTypeParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: Option<TName>,
        type_parameters: Option<TTypeParameters>,
    ) -> BaseGenericNamedDeclaration {
        let node =
            self.create_base_named_declaration(base_factory, kind, decorators, modifiers, name);
        let mut node = BaseGenericNamedDeclaration::new(node, self.as_node_array(type_parameters));
        let flags = propagate_children_flags(node.maybe_type_parameters().as_ref());
        node.add_transform_flags(flags);
        if node.maybe_type_parameters().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub(crate) fn create_base_signature_declaration<
        'name,
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StrOrRcNode<'name>>,
        TTypeParameters: Into<NodeArrayOrVec>,
        TParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: Option<TName>,
        type_parameters: Option<TTypeParameters>,
        parameters: Option<TParameters>,
        type_: Option<Gc<Node>>,
    ) -> BaseSignatureDeclaration {
        let node = self.create_base_generic_named_declaration(
            base_factory,
            kind,
            decorators,
            modifiers,
            name,
            type_parameters,
        );
        let mut node =
            BaseSignatureDeclaration::new(node, self.create_node_array(parameters, None), type_);
        node.add_transform_flags(
            propagate_children_flags(Some(node.parameters()))
                | propagate_child_flags(node.maybe_type()),
        );
        if node.maybe_type().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub(crate) fn create_base_function_like_declaration<
        'name,
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StrOrRcNode<'name>>,
        TTypeParameters: Into<NodeArrayOrVec>,
        TParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: Option<TName>,
        type_parameters: Option<TTypeParameters>,
        parameters: Option<TParameters>,
        type_: Option<Gc<Node>>,
        body: Option<Gc<Node>>,
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
        let body_is_none = body.is_none();
        let mut node = BaseFunctionLikeDeclaration::new(node, body);
        node.add_transform_flags(
            propagate_child_flags(node.maybe_body())
                & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        if body_is_none {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub(crate) fn create_base_interface_or_class_like_declaration<
        'name,
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StrOrRcNode<'name>>,
        TTypeParameters: Into<NodeArrayOrVec>,
        THeritageClauses: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: Option<TName>,
        type_parameters: Option<TTypeParameters>,
        heritage_clauses: Option<THeritageClauses>,
    ) -> BaseInterfaceOrClassLikeDeclaration {
        let node = self.create_base_generic_named_declaration(
            base_factory,
            kind,
            decorators,
            modifiers,
            name,
            type_parameters,
        );
        let mut node =
            BaseInterfaceOrClassLikeDeclaration::new(node, self.as_node_array(heritage_clauses));
        node.add_transform_flags(propagate_children_flags(node.maybe_heritage_clauses()));
        node
    }

    pub(crate) fn create_base_class_like_declaration<
        'name,
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StrOrRcNode<'name>>,
        TTypeParameters: Into<NodeArrayOrVec>,
        THeritageClauses: Into<NodeArrayOrVec>,
        TMembers: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: Option<TName>,
        type_parameters: Option<TTypeParameters>,
        heritage_clauses: Option<THeritageClauses>,
        members: TMembers,
    ) -> ClassLikeDeclarationBase {
        let node = self.create_base_interface_or_class_like_declaration(
            base_factory,
            kind,
            decorators,
            modifiers,
            name,
            type_parameters,
            heritage_clauses,
        );
        let mut node =
            ClassLikeDeclarationBase::new(node, self.create_node_array(Some(members), None));
        node.add_transform_flags(propagate_children_flags(Some(node.members())));
        node
    }

    pub(crate) fn create_base_binding_like_declaration<
        'name,
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StrOrRcNode<'name>>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: Option<TName>,
        initializer: Option<Gc<Node>>,
    ) -> BaseBindingLikeDeclaration {
        let node =
            self.create_base_named_declaration(base_factory, kind, decorators, modifiers, name);
        let mut node = BaseBindingLikeDeclaration::new(node, initializer);
        node.add_transform_flags(propagate_child_flags(node.maybe_initializer()));
        node
    }

    pub(crate) fn create_base_variable_like_declaration<
        'name,
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
        TName: Into<StrOrRcNode<'name>>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        name: Option<TName>,
        type_: Option<Gc<Node>>,
        initializer: Option<Gc<Node>>,
    ) -> BaseVariableLikeDeclaration {
        let node = self.create_base_binding_like_declaration(
            base_factory,
            kind,
            decorators,
            modifiers,
            name,
            initializer,
        );
        let type_is_some = type_.is_some();
        let mut node = BaseVariableLikeDeclaration::new(node, type_);
        node.add_transform_flags(propagate_child_flags(node.maybe_type()));
        if type_is_some {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub(crate) fn create_base_literal(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        text: String,
    ) -> BaseLiteralLikeNode {
        let node = self.create_base_token(base_factory, kind);
        BaseLiteralLikeNode::new(node, text)
    }

    pub fn create_numeric_literal<TValue: Into<StringOrNumber>>(
        &self,
        base_factory: &TBaseNodeFactory,
        value: TValue,
        numeric_literal_flags: Option<TokenFlags>,
    ) -> NumericLiteral {
        let numeric_literal_flags = numeric_literal_flags.unwrap_or(TokenFlags::None);
        let value = value.into();
        let node = self.create_base_literal(
            base_factory,
            SyntaxKind::NumericLiteral,
            match value {
                StringOrNumber::String(value) => value,
                StringOrNumber::Number(value) => value.to_string(),
            },
        );
        let mut node = NumericLiteral::new(node, numeric_literal_flags);
        if numeric_literal_flags.intersects(TokenFlags::BinaryOrOctalSpecifier) {
            node.add_transform_flags(TransformFlags::ContainsES2015);
        }
        node
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
                PseudoBigIntOrString::String(value) => value,
                PseudoBigIntOrString::PseudoBigInt(value) => {
                    format!("{}n", pseudo_big_int_to_string(&value))
                }
            },
        );
        let mut node = BigIntLiteral::new(node);
        node.add_transform_flags(TransformFlags::ContainsESNext);
        node
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
        if matches!(has_extended_unicode_escape, Some(true)) {
            node.add_transform_flags(TransformFlags::ContainsES2015);
        }
        node
    }

    pub fn create_string_literal_from_node(
        &self,
        base_factory: &TBaseNodeFactory,
        source_node: &Node, /*PropertyNameLiteral*/
    ) -> StringLiteral {
        let mut node = self.create_base_string_literal(
            base_factory,
            get_text_of_identifier_or_literal(source_node).into_owned(),
            None,
        );
        node.text_source_node = Some(source_node.node_wrapper());
        node
    }

    pub fn create_regular_expression_literal(
        &self,
        base_factory: &TBaseNodeFactory,
        text: String,
    ) -> RegularExpressionLiteral {
        let node =
            self.create_base_literal(base_factory, SyntaxKind::RegularExpressionLiteral, text);
        RegularExpressionLiteral::new(node)
    }

    pub fn create_literal_like_node(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind, /*LiteralToken["kind"] | SyntaxKind.JsxTextAllWhiteSpaces*/
        text: String,
    ) -> Node {
        match kind {
            SyntaxKind::NumericLiteral => self
                .create_numeric_literal(base_factory, text, Some(TokenFlags::None))
                .into(),
            SyntaxKind::BigIntLiteral => self.create_big_int_literal(base_factory, text).into(),
            SyntaxKind::StringLiteral => self
                .create_string_literal(base_factory, text, None, None)
                .into(),
            SyntaxKind::JsxText => self.create_jsx_text(base_factory, text, Some(false)).into(),
            SyntaxKind::JsxTextAllWhiteSpaces => {
                self.create_jsx_text(base_factory, text, Some(true)).into()
            }
            SyntaxKind::RegularExpressionLiteral => self
                .create_regular_expression_literal(base_factory, text)
                .into(),
            SyntaxKind::NoSubstitutionTemplateLiteral => self
                .create_template_literal_like_node(
                    base_factory,
                    kind,
                    text,
                    None,
                    Some(TokenFlags::None),
                )
                .into(),
            _ => panic!("Unexpected kind"),
        }
    }

    pub(crate) fn create_base_identifier(
        &self,
        base_factory: &TBaseNodeFactory,
        text: &str,
        mut original_keyword_kind: Option<SyntaxKind>,
    ) -> Identifier {
        if original_keyword_kind.is_none() && !text.is_empty() {
            original_keyword_kind = string_to_token(text);
        }
        if matches!(original_keyword_kind, Some(SyntaxKind::Identifier)) {
            original_keyword_kind = None;
        }
        let node = base_factory.create_base_identifier_node(SyntaxKind::Identifier);
        let mut node = Identifier::new(node, escape_leading_underscores(text).into_owned());
        node.original_keyword_kind = original_keyword_kind;
        node
    }

    pub fn create_base_generated_identifier(
        &self,
        base_factory: &TBaseNodeFactory,
        text: &str,
        auto_generate_flags: GeneratedIdentifierFlags,
    ) -> Identifier {
        let mut node = self.create_base_identifier(base_factory, text, None);
        node.auto_generate_flags = Some(auto_generate_flags);
        node.auto_generate_id = Some(get_next_auto_generate_id());
        increment_next_auto_generate_id();
        node
    }

    pub fn create_identifier(
        &self,
        base_factory: &TBaseNodeFactory,
        text: &str,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
        original_keyword_kind: Option<SyntaxKind>,
    ) -> Identifier {
        let mut node = self.create_base_identifier(base_factory, text, original_keyword_kind);
        if let Some(type_arguments) = type_arguments {
            *node.maybe_type_arguments_mut() =
                Some(self.create_node_array(Some(type_arguments), None));
        }
        if matches!(node.original_keyword_kind, Some(SyntaxKind::AwaitKeyword)) {
            node.add_transform_flags(TransformFlags::ContainsPossibleTopLevelAwait);
        }
        node
    }

    pub fn update_identifier<TTypeArguments: Into<MaybeChangedNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node,                    /*Identifier*/
        type_arguments: TTypeArguments, /*<TypeNode | TypeParameterDeclaration>*/
    ) -> Gc<Node> {
        let type_arguments = type_arguments.into();
        let node_type_arguments = node.as_identifier().maybe_type_arguments();
        if has_option_node_array_changed(node_type_arguments.as_ref(), &type_arguments) {
            self.update(
                self.create_identifier(
                    base_factory,
                    &id_text(node),
                    type_arguments.into_update_value(node_type_arguments.as_ref()),
                    None,
                )
                .into(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_temp_variable(
        &self,
        record_temp_variable: Option<impl FnMut(&Node /*Identifier*/)>,
        reserved_in_nested_scopes: Option<bool>,
    ) -> Gc<Node /*GeneratedIdentifier*/> {
        unimplemented!()
    }

    pub fn create_unique_name(
        &self,
        base_factory: &TBaseNodeFactory,
        text: &str,
        flags: Option<GeneratedIdentifierFlags>,
    ) -> Gc<Node /*Identifier*/> {
        let flags = flags.unwrap_or_default();
        unimplemented!()
    }

    pub fn create_private_identifier(
        &self,
        base_factory: &TBaseNodeFactory,
        text: &str,
    ) -> PrivateIdentifier {
        if !starts_with(text, "#") {
            Debug_.fail(Some(&format!(
                "First character of private identifier must be #: {}",
                text
            )));
        }
        let node = base_factory.create_base_private_identifier_node(SyntaxKind::PrivateIdentifier);
        let mut node = PrivateIdentifier::new(node, escape_leading_underscores(text).into_owned());
        node.add_transform_flags(TransformFlags::ContainsClassFields);
        node
    }

    pub fn create_base_token(&self, base_factory: &TBaseNodeFactory, kind: SyntaxKind) -> BaseNode {
        base_factory.create_base_token_node(kind)
    }

    pub fn create_token(&self, base_factory: &TBaseNodeFactory, token: SyntaxKind) -> BaseNode {
        Debug_.assert(
            token >= SyntaxKind::FirstToken && token <= SyntaxKind::LastToken,
            Some("Invalid token"),
        );
        Debug_.assert(
            token <= SyntaxKind::FirstTemplateToken || token >= SyntaxKind::LastTemplateToken,
            Some("Invalid token. Use 'createTemplateLiteralLikeNode' to create template literals."),
        );
        Debug_.assert(
            token <= SyntaxKind::FirstLiteralToken || token >= SyntaxKind::LastLiteralToken,
            Some("Invalid token. Use 'createLiteralLikeNode' to create literals."),
        );
        Debug_.assert(
            token != SyntaxKind::Identifier,
            Some("Invalid token. Use 'createIdentifier' to create identifiers"),
        );
        let mut node = self.create_base_token(base_factory, token);
        let mut transform_flags = TransformFlags::None;
        match token {
            SyntaxKind::AsyncKeyword => {
                transform_flags = TransformFlags::ContainsES2017 | TransformFlags::ContainsES2018;
            }
            SyntaxKind::PublicKeyword
            | SyntaxKind::PrivateKeyword
            | SyntaxKind::ProtectedKeyword
            | SyntaxKind::ReadonlyKeyword
            | SyntaxKind::AbstractKeyword
            | SyntaxKind::DeclareKeyword
            | SyntaxKind::ConstKeyword
            | SyntaxKind::AnyKeyword
            | SyntaxKind::NumberKeyword
            | SyntaxKind::BigIntKeyword
            | SyntaxKind::NeverKeyword
            | SyntaxKind::ObjectKeyword
            | SyntaxKind::OverrideKeyword
            | SyntaxKind::StringKeyword
            | SyntaxKind::BooleanKeyword
            | SyntaxKind::SymbolKeyword
            | SyntaxKind::VoidKeyword
            | SyntaxKind::UnknownKeyword
            | SyntaxKind::UndefinedKeyword => {
                transform_flags = TransformFlags::ContainsTypeScript;
            }
            SyntaxKind::SuperKeyword => {
                transform_flags =
                    TransformFlags::ContainsES2015 | TransformFlags::ContainsLexicalSuper;
            }
            SyntaxKind::StaticKeyword => {
                transform_flags = TransformFlags::ContainsES2015;
            }
            SyntaxKind::ThisKeyword => {
                transform_flags = TransformFlags::ContainsLexicalThis;
            }
            _ => (),
        }
        if transform_flags != TransformFlags::None {
            node.add_transform_flags(transform_flags);
        }
        node
    }
}

pub fn has_option_node_array_changed(
    existing: Option<&NodeArray>,
    maybe_changed: &MaybeChangedNodeArray,
) -> bool {
    match maybe_changed {
        MaybeChangedNodeArray::Unchanged => false,
        MaybeChangedNodeArray::Changed(None) => existing.is_some(),
        _ => true,
    }
}

pub enum MaybeChangedNodeArray {
    Unchanged,
    Changed(Option<NodeArray>),
}

impl MaybeChangedNodeArray {
    pub fn into_update_value(self, existing_value: Option<&NodeArray>) -> Option<NodeArray> {
        match self {
            // TODO: I think this doesn't technically match the Typescript version's semantics
            // since we're not sharing identity in the "reuse existing" case, but I guess that
            // would only matter if there are cases where the "updated node" or "existing node"
            // then mutate the NodeArray (do NodeArray's ever get mutated?) (and the
            // shared-identity both-get-mutated semantics is "used" somewhere)?
            Self::Unchanged => existing_value.cloned(),
            Self::Changed(value) => value,
        }
    }
}

impl From<Option<NodeArray>> for MaybeChangedNodeArray {
    fn from(value: Option<NodeArray>) -> Self {
        Self::Changed(value)
    }
}

#[derive(Clone)]
pub enum StringOrRcNode {
    String(String),
    RcNode(Gc<Node>),
}

impl From<String> for StringOrRcNode {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<Gc<Node>> for StringOrRcNode {
    fn from(value: Gc<Node>) -> Self {
        Self::RcNode(value)
    }
}

#[derive(Clone)]
pub enum StrOrRcNode<'str> {
    Str(&'str str),
    RcNode(Gc<Node>),
}

impl<'str> From<&'str str> for StrOrRcNode<'str> {
    fn from(value: &'str str) -> Self {
        Self::Str(value)
    }
}

impl<'str> From<Gc<Node>> for StrOrRcNode<'str> {
    fn from(value: Gc<Node>) -> Self {
        Self::RcNode(value)
    }
}

#[derive(Clone, Debug)]
pub enum StringOrNumber {
    String(String),
    Number(Number),
}

impl From<String> for StringOrNumber {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<Number> for StringOrNumber {
    fn from(value: Number) -> Self {
        Self::Number(value)
    }
}

pub enum StringOrNumberOrBoolOrRcNode {
    String(String),
    Number(Number),
    Bool(bool),
    RcNode(Gc<Node>),
}

impl From<String> for StringOrNumberOrBoolOrRcNode {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<Gc<Node>> for StringOrNumberOrBoolOrRcNode {
    fn from(value: Gc<Node>) -> Self {
        Self::RcNode(value)
    }
}

impl From<Number> for StringOrNumberOrBoolOrRcNode {
    fn from(value: Number) -> Self {
        Self::Number(value)
    }
}

impl From<bool> for StringOrNumberOrBoolOrRcNode {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}
