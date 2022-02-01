#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Ref, RefCell};
use std::rc::Rc;

use super::{
    aggregate_children_flags, propagate_child_flags, propagate_children_flags,
    propagate_identifier_name_flags, update_with_original, update_without_original,
    PseudoBigIntOrString,
};
use crate::{
    create_node_converters, create_parenthesizer_rules, escape_leading_underscores, is_identifier,
    null_node_converters, null_parenthesizer_rules, pseudo_big_int_to_string,
    BaseBindingLikeDeclaration, BaseFunctionLikeDeclaration, BaseGenericNamedDeclaration,
    BaseInterfaceOrClassLikeDeclaration, BaseJSDocTag, BaseJSDocTypeLikeTag, BaseJSDocUnaryType,
    BaseLiteralLikeNode, BaseNamedDeclaration, BaseNode, BaseNodeFactory, BaseSignatureDeclaration,
    BaseVariableLikeDeclaration, BigIntLiteral, BinaryExpression, Debug_,
    FunctionLikeDeclarationInterface, HasTypeInterface, HasTypeParametersInterface, Identifier,
    LiteralLikeNode, LiteralLikeNodeInterface, Node, NodeArray, NodeArrayOrVec, NodeConverters,
    NodeFactory, NodeInterface, NumericLiteral, ParenthesizerRules, PostfixUnaryExpression,
    PrefixUnaryExpression, ReadonlyTextRange, SignatureDeclarationInterface, StringLiteral,
    SyntaxKind, TokenFlags, TransformFlags,
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
) -> Rc<NodeFactory<TBaseNodeFactory>> {
    NodeFactory::new(flags)
}

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn new(flags: NodeFactoryFlags) -> Rc<Self> {
        let factory_ = Rc::new(Self {
            flags,
            parenthesizer_rules: RefCell::new(None),
            converters: RefCell::new(None),
        });
        factory_.set_parenthesizer_rules(
            /*memoize(*/
            if flags.intersects(NodeFactoryFlags::NoParenthesizerRules) {
                Box::new(null_parenthesizer_rules())
            } else {
                Box::new(create_parenthesizer_rules(factory_.clone()))
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

    pub(crate) fn update(&self, updated: Rc<Node>, original: &Node) -> Rc<Node> {
        if self.flags.intersects(NodeFactoryFlags::NoOriginalNode) {
            update_without_original(updated, original)
        } else {
            update_with_original(updated, original)
        }
    }

    pub(crate) fn set_parenthesizer_rules(
        &self,
        parenthesizer_rules: Box<dyn ParenthesizerRules<TBaseNodeFactory>>,
    ) {
        *self.parenthesizer_rules.borrow_mut() = Some(parenthesizer_rules);
    }

    pub(crate) fn parenthesizer_rules(&self) -> Ref<Box<dyn ParenthesizerRules<TBaseNodeFactory>>> {
        Ref::map(self.parenthesizer_rules.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn parenthesizer(&self) -> Ref<Box<dyn ParenthesizerRules<TBaseNodeFactory>>> {
        self.parenthesizer_rules()
    }

    pub(crate) fn set_converters(
        &self,
        node_converters: Box<dyn NodeConverters<TBaseNodeFactory>>,
    ) {
        *self.converters.borrow_mut() = Some(node_converters);
    }

    pub fn converters(&self) -> Ref<Box<dyn NodeConverters<TBaseNodeFactory>>> {
        Ref::map(self.converters.borrow(), |option| option.as_ref().unwrap())
    }

    pub fn create_jsdoc_all_type(&self, base_factory: &TBaseNodeFactory) -> BaseNode {
        self.create_jsdoc_primary_type_worker(base_factory, SyntaxKind::JSDocAllType)
    }

    pub fn create_jsdoc_unknown_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Option<Rc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(base_factory, SyntaxKind::JSDocUnknownType, type_)
    }

    pub fn create_jsdoc_non_nullable_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Option<Rc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(base_factory, SyntaxKind::JSDocNonNullableType, type_)
    }

    pub fn create_jsdoc_nullable_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Option<Rc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(base_factory, SyntaxKind::JSDocNullableType, type_)
    }

    pub fn create_jsdoc_optional_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Option<Rc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(base_factory, SyntaxKind::JSDocOptionalType, type_)
    }

    pub fn create_jsdoc_variadic_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Option<Rc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(base_factory, SyntaxKind::JSDocVariadicType, type_)
    }

    pub fn create_jsdoc_namepath_type(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Option<Rc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(base_factory, SyntaxKind::JSDocNamepathType, type_)
    }

    pub fn create_jsdoc_type_tag(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        type_expression: Option<Rc<Node /*JSDocTypeExpression*/>>,
        comment: Option<NodeArray /*<JSDocComment>*/>,
    ) -> BaseJSDocTypeLikeTag {
        self.create_jsdoc_type_like_tag_worker(
            base_factory,
            SyntaxKind::JSDocTypeTag,
            tag_name,
            type_expression,
            comment,
        )
    }

    pub fn create_jsdoc_return_tag(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        type_expression: Option<Rc<Node /*JSDocTypeExpression*/>>,
        comment: Option<NodeArray /*<JSDocComment>*/>,
    ) -> BaseJSDocTypeLikeTag {
        self.create_jsdoc_type_like_tag_worker(
            base_factory,
            SyntaxKind::JSDocReturnTag,
            tag_name,
            type_expression,
            comment,
        )
    }

    pub fn create_jsdoc_this_tag(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        type_expression: Option<Rc<Node /*JSDocTypeExpression*/>>,
        comment: Option<NodeArray /*<JSDocComment>*/>,
    ) -> BaseJSDocTypeLikeTag {
        self.create_jsdoc_type_like_tag_worker(
            base_factory,
            SyntaxKind::JSDocThisTag,
            tag_name,
            type_expression,
            comment,
        )
    }

    pub fn create_jsdoc_enum_tag(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        type_expression: Option<Rc<Node /*JSDocTypeExpression*/>>,
        comment: Option<NodeArray /*<JSDocComment>*/>,
    ) -> BaseJSDocTypeLikeTag {
        self.create_jsdoc_type_like_tag_worker(
            base_factory,
            SyntaxKind::JSDocEnumTag,
            tag_name,
            type_expression,
            comment,
        )
    }

    pub fn create_jsdoc_author_tag(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        comment: Option<NodeArray /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocAuthorTag,
            tag_name,
            comment,
        )
    }

    pub fn create_jsdoc_class_tag(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        comment: Option<NodeArray /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocClassTag,
            tag_name,
            comment,
        )
    }

    pub fn create_jsdoc_public_tag(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        comment: Option<NodeArray /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocPublicTag,
            tag_name,
            comment,
        )
    }

    pub fn create_jsdoc_private_tag(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        comment: Option<NodeArray /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocPrivateTag,
            tag_name,
            comment,
        )
    }

    pub fn create_jsdoc_protected_tag(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        comment: Option<NodeArray /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocProtectedTag,
            tag_name,
            comment,
        )
    }

    pub fn create_jsdoc_readonly_tag(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        comment: Option<NodeArray /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocReadonlyTag,
            tag_name,
            comment,
        )
    }

    pub fn create_jsdoc_override_tag(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        comment: Option<NodeArray /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            base_factory,
            SyntaxKind::JSDocOverrideTag,
            tag_name,
            comment,
        )
    }

    pub fn create_jsdoc_deprecated_tag(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        comment: Option<NodeArray /*<JSDocComment>*/>,
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
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::CommaToken, right)
    }

    pub fn create_assignment(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::EqualsToken, right)
    }

    pub fn create_logical_or(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::BarBarToken, right)
    }

    pub fn create_logical_and(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
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
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::BarToken, right)
    }

    pub fn create_bitwise_xor(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::CaretToken, right)
    }

    pub fn create_bitwise_and(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::AmpersandToken, right)
    }

    pub fn create_strict_equality(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
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
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
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
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::EqualsEqualsToken, right)
    }

    pub fn create_inequality(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
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
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::LessThanToken, right)
    }

    pub fn create_less_than_equals(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::LessThanEqualsToken, right)
    }

    pub fn create_greater_than(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::GreaterThanToken, right)
    }

    pub fn create_greater_than_equals(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
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
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::LessThanLessThanToken, right)
    }

    pub fn create_right_shift(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
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
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
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
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::PlusToken, right)
    }

    pub fn create_subtract(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::MinusToken, right)
    }

    pub fn create_multiply(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::AsteriskToken, right)
    }

    pub fn create_divide(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::SlashToken, right)
    }

    pub fn create_modulo(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::PercentToken, right)
    }

    pub fn create_exponent(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Rc<Node /*Expression*/>,
        right: Rc<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression(base_factory, left, SyntaxKind::AsteriskAsteriskToken, right)
    }

    pub fn create_prefix_plus(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Rc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression(base_factory, SyntaxKind::MinusToken, operand)
    }

    pub fn create_prefix_increment(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Rc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression(base_factory, SyntaxKind::PlusPlusToken, operand)
    }

    pub fn create_prefix_decrement(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Rc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression(base_factory, SyntaxKind::MinusMinusToken, operand)
    }

    pub fn create_bitwise_not(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Rc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression(base_factory, SyntaxKind::TildeToken, operand)
    }

    pub fn create_logical_not(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Rc<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression(base_factory, SyntaxKind::ExclamationToken, operand)
    }

    pub fn create_postfix_increment(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Rc<Node /*Expression*/>,
    ) -> PostfixUnaryExpression {
        self.create_postfix_unary_expression(base_factory, operand, SyntaxKind::PlusPlusToken)
    }

    pub fn create_postfix_decrement(
        &self,
        base_factory: &TBaseNodeFactory,
        operand: Rc<Node /*Expression*/>,
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

    pub(crate) fn create_base_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> BaseNode {
        let mut node = self.create_base_node(base_factory, kind);
        node.set_decorators(self.as_node_array(decorators));
        node.modifiers = self.as_node_array(modifiers);
        let flags = propagate_children_flags(node.maybe_decorators().as_ref())
            | propagate_children_flags(node.modifiers.as_ref());
        node.add_transform_flags(flags);
        // node.symbol = undefined!;
        // node.localSymbol = undefined!;
        // node.locals = undefined!;
        // node.nextContainer = undefined!;
        node
    }

    pub(crate) fn create_base_named_declaration<TName: Into<StringOrRcNode>>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
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
        TName: Into<StringOrRcNode>,
        TTypeParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        name: Option<TName>,
        type_parameters: Option<TTypeParameters>,
    ) -> BaseGenericNamedDeclaration {
        let node =
            self.create_base_named_declaration(base_factory, kind, decorators, modifiers, name);
        let mut node = BaseGenericNamedDeclaration::new(node, self.as_node_array(type_parameters));
        node.add_transform_flags(propagate_children_flags(node.maybe_type_parameters()));
        if node.maybe_type_parameters().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub(crate) fn create_base_signature_declaration<
        TName: Into<StringOrRcNode>,
        TTypeParameters: Into<NodeArrayOrVec>,
        TParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        name: Option<TName>,
        type_parameters: Option<TTypeParameters>,
        parameters: Option<TParameters>,
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
        TName: Into<StringOrRcNode>,
        TTypeParameters: Into<NodeArrayOrVec>,
        TParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        name: Option<TName>,
        type_parameters: Option<TTypeParameters>,
        parameters: Option<TParameters>,
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
        TTypeParameters: Into<NodeArrayOrVec>,
    >(
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

    pub(crate) fn create_base_binding_like_declaration(
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

    pub(crate) fn create_base_variable_like_declaration(
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

    pub(crate) fn create_base_literal(
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

    pub(crate) fn create_base_identifier(
        &self,
        base_factory: &TBaseNodeFactory,
        text: &str,
    ) -> Identifier {
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
}

pub enum StringOrRcNode {
    String(String),
    RcNode(Rc<Node>),
}

impl From<String> for StringOrRcNode {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<Rc<Node>> for StringOrRcNode {
    fn from(value: Rc<Node>) -> Self {
        Self::RcNode(value)
    }
}
