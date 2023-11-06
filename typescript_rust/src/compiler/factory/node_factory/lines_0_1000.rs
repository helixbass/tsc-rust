use std::{borrow::Borrow, cell::RefCell, ptr};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCellRef, Trace};
use id_arena::{Arena, Id};
use local_macros::generate_node_factory_method_wrapper;

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
    BaseVariableLikeDeclaration, BigIntLiteral, BinaryExpression, BoolOrRcNode,
    ClassLikeDeclarationBase, ClassLikeDeclarationInterface, Debug_,
    FunctionLikeDeclarationInterface, GeneratedIdentifierFlags, HasInitializerInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface, Identifier,
    InterfaceOrClassLikeDeclarationInterface, LiteralLikeNodeInterface, Node, NodeArray,
    NodeArrayOrVec, NodeConverters, NodeFactory, NodeInterface, Number, NumericLiteral,
    ParenthesizerRules, PostfixUnaryExpression, PrefixUnaryExpression, PrivateIdentifier,
    ReadonlyTextRange, RegularExpressionLiteral, SignatureDeclarationInterface, StringLiteral,
    StringOrNodeArray, SyntaxKind, TokenFlags, TransformFlags,
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

pub fn create_node_factory<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>(
    flags: NodeFactoryFlags, /*, baseFactory: BaseNodeFactory*/
    base_factory: Gc<TBaseNodeFactory>,
) -> Gc<NodeFactory<TBaseNodeFactory>> {
    NodeFactory::new(flags, base_factory)
}

impl<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize> NodeFactory<TBaseNodeFactory> {
    pub fn new(flags: NodeFactoryFlags, base_factory: Gc<TBaseNodeFactory>) -> Gc<Self> {
        let factory_ = Gc::new(Self {
            base_factory,
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

    pub(crate) fn update(
        &self,
        arena: &RefCell<Arena<Node>>,
        updated: Id<Node>,
        original: Id<Node>,
    ) -> Id<Node> {
        if self.flags.intersects(NodeFactoryFlags::NoOriginalNode) {
            update_without_original(arena, updated, original)
        } else {
            update_with_original(arena, updated, original)
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

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_all_type_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
    ) -> BaseNode {
        self.create_jsdoc_primary_type_worker(id, SyntaxKind::JSDocAllType)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_unknown_type_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        type_: Option<Id<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(id, SyntaxKind::JSDocUnknownType, type_)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_non_nullable_type_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        type_: Option<Id<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(id, SyntaxKind::JSDocNonNullableType, type_)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_nullable_type_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        type_: Option<Id<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(id, SyntaxKind::JSDocNullableType, type_)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_optional_type_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        type_: Option<Id<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(id, SyntaxKind::JSDocOptionalType, type_)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_variadic_type_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        type_: Option<Id<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(id, SyntaxKind::JSDocVariadicType, type_)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_namepath_type_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        type_: Option<Id<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        self.create_jsdoc_unary_type_worker(id, SyntaxKind::JSDocNamepathType, type_)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_type_tag_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        tag_name: Option<Id<Node /*Identifier*/>>,
        type_expression: Option<Id<Node /*JSDocTypeExpression*/>>,
        comment: Option<impl Into<StringOrNodeArray> /*<JSDocComment>*/>,
    ) -> BaseJSDocTypeLikeTag {
        self.create_jsdoc_type_like_tag_worker(
            arena,
            id,
            SyntaxKind::JSDocTypeTag,
            tag_name,
            type_expression,
            comment,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_return_tag_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        tag_name: Option<Id<Node /*Identifier*/>>,
        type_expression: Option<Id<Node /*JSDocTypeExpression*/>>,
        comment: Option<impl Into<StringOrNodeArray> /*<JSDocComment>*/>,
    ) -> BaseJSDocTypeLikeTag {
        self.create_jsdoc_type_like_tag_worker(
            arena,
            id,
            SyntaxKind::JSDocReturnTag,
            tag_name,
            type_expression,
            comment,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_this_tag_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        tag_name: Option<Id<Node /*Identifier*/>>,
        type_expression: Option<Id<Node /*JSDocTypeExpression*/>>,
        comment: Option<impl Into<StringOrNodeArray> /*<JSDocComment>*/>,
    ) -> BaseJSDocTypeLikeTag {
        self.create_jsdoc_type_like_tag_worker(
            arena,
            id,
            SyntaxKind::JSDocThisTag,
            tag_name,
            type_expression,
            comment,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_enum_tag_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        tag_name: Option<Id<Node /*Identifier*/>>,
        type_expression: Option<Id<Node /*JSDocTypeExpression*/>>,
        comment: Option<impl Into<StringOrNodeArray> /*<JSDocComment>*/>,
    ) -> BaseJSDocTypeLikeTag {
        self.create_jsdoc_type_like_tag_worker(
            arena,
            id,
            SyntaxKind::JSDocEnumTag,
            tag_name,
            type_expression,
            comment,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_author_tag_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        tag_name: Option<Id<Node /*Identifier*/>>,
        comment: Option<impl Into<StringOrNodeArray> /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            arena,
            id,
            SyntaxKind::JSDocAuthorTag,
            tag_name,
            comment,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_class_tag_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        tag_name: Option<Id<Node /*Identifier*/>>,
        comment: Option<impl Into<StringOrNodeArray> /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(arena, id, SyntaxKind::JSDocClassTag, tag_name, comment)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_public_tag_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        tag_name: Option<Id<Node /*Identifier*/>>,
        comment: Option<impl Into<StringOrNodeArray> /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            arena,
            id,
            SyntaxKind::JSDocPublicTag,
            tag_name,
            comment,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_private_tag_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        tag_name: Option<Id<Node /*Identifier*/>>,
        comment: Option<impl Into<StringOrNodeArray> /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            arena,
            id,
            SyntaxKind::JSDocPrivateTag,
            tag_name,
            comment,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_protected_tag_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        tag_name: Option<Id<Node /*Identifier*/>>,
        comment: Option<impl Into<StringOrNodeArray> /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            arena,
            id,
            SyntaxKind::JSDocProtectedTag,
            tag_name,
            comment,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_readonly_tag_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        tag_name: Option<Id<Node /*Identifier*/>>,
        comment: Option<impl Into<StringOrNodeArray> /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            arena,
            id,
            SyntaxKind::JSDocReadonlyTag,
            tag_name,
            comment,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_override_tag_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        tag_name: Option<Id<Node /*Identifier*/>>,
        comment: Option<impl Into<StringOrNodeArray> /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            arena,
            id,
            SyntaxKind::JSDocOverrideTag,
            tag_name,
            comment,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_deprecated_tag_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        tag_name: Option<Id<Node /*Identifier*/>>,
        comment: Option<impl Into<StringOrNodeArray> /*<JSDocComment>*/>,
    ) -> BaseJSDocTag {
        self.create_jsdoc_simple_tag_worker(
            arena,
            id,
            SyntaxKind::JSDocDeprecatedTag,
            tag_name,
            comment,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_comma_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::CommaToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_assignment_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::EqualsToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_logical_or_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::BarBarToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_logical_and_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(
            arena,
            id,
            left,
            SyntaxKind::AmpersandAmpersandToken,
            right,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_bitwise_or_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::BarToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_bitwise_xor_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::CaretToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_bitwise_and_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::AmpersandToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_strict_equality_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(
            arena,
            id,
            left,
            SyntaxKind::EqualsEqualsEqualsToken,
            right,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_strict_inequality_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(
            arena,
            id,
            left,
            SyntaxKind::ExclamationEqualsEqualsToken,
            right,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_equality_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::EqualsEqualsToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_inequality_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(
            arena,
            id,
            left,
            SyntaxKind::ExclamationEqualsToken,
            right,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_less_than_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::LessThanToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_less_than_equals_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::LessThanEqualsToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_greater_than_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::GreaterThanToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_greater_than_equals_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(
            arena,
            id,
            left,
            SyntaxKind::GreaterThanEqualsToken,
            right,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_left_shift_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::LessThanLessThanToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_right_shift_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(
            arena,
            id,
            left,
            SyntaxKind::GreaterThanGreaterThanToken,
            right,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_unsigned_right_shift_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(
            arena,
            id,
            left,
            SyntaxKind::GreaterThanGreaterThanGreaterThanToken,
            right,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_add_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::PlusToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_subtract_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::MinusToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_multiply_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::AsteriskToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_divide_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::SlashToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_modulo_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::PercentToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_exponent_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        left: Id<Node /*Expression*/>,
        right: Id<Node /*Expression*/>,
    ) -> BinaryExpression {
        self.create_binary_expression_raw(arena, id, left, SyntaxKind::AsteriskAsteriskToken, right)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_prefix_plus_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        operand: Id<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression_raw(arena, id, SyntaxKind::MinusToken, operand)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_prefix_increment_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        operand: Id<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression_raw(arena, id, SyntaxKind::PlusPlusToken, operand)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_prefix_decrement_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        operand: Id<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression_raw(arena, id, SyntaxKind::MinusMinusToken, operand)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_bitwise_not_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        operand: Id<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression_raw(arena, id, SyntaxKind::TildeToken, operand)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_logical_not_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        operand: Id<Node /*Expression*/>,
    ) -> PrefixUnaryExpression {
        self.create_prefix_unary_expression_raw(arena, id, SyntaxKind::ExclamationToken, operand)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_postfix_increment_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        operand: Id<Node /*Expression*/>,
    ) -> PostfixUnaryExpression {
        self.create_postfix_unary_expression_raw(arena, id, operand, SyntaxKind::PlusPlusToken)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_postfix_decrement_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        operand: Id<Node /*Expression*/>,
    ) -> PostfixUnaryExpression {
        self.create_postfix_unary_expression_raw(arena, id, operand, SyntaxKind::MinusMinusToken)
    }

    pub fn create_node_array(
        &self,
        arena: &RefCell<Arena<NodeArray>>,
        elements: Option<impl Into<NodeArrayOrVec>>,
        has_trailing_comma: Option<bool>,
    ) -> Id<NodeArray> {
        let elements = match elements {
            None => NodeArrayOrVec::Vec(vec![]),
            Some(elements) => elements.into(),
        };
        match elements {
            NodeArrayOrVec::NodeArray(elements) => {
                if match has_trailing_comma {
                    None => true,
                    Some(has_trailing_comma) => {
                        arena.borrow()[elements].has_trailing_comma == has_trailing_comma
                    }
                } {
                    if arena.borrow()[elements].maybe_transform_flags().is_none() {
                        aggregate_children_flags(&elements);
                    }
                    Debug_.attach_node_array_debug_info(&elements);
                    return elements;
                }

                let mut array = NodeArray::new(
                    arena,
                    elements.to_vec(),
                    elements.pos(),
                    elements.end(),
                    has_trailing_comma.unwrap(),
                    elements.maybe_transform_flags(),
                );
                Debug_.attach_node_array_debug_info(&array);
                array
            }
            NodeArrayOrVec::Vec(elements) => {
                // let length = elements.len();
                let array = /*length >= 1 && length <= 4 ? elements.slice() :*/ elements;
                let array = NodeArray::new(
                    arena,
                    array,
                    -1,
                    -1,
                    has_trailing_comma.unwrap_or(false),
                    None,
                );
                aggregate_children_flags(&array);
                Debug_.attach_node_array_debug_info(&array);
                array
            }
        }
    }

    pub(crate) fn create_base_node(&self, id: Id<Node>, kind: SyntaxKind) -> BaseNode {
        self.base_factory.create_base_node(id, kind)
    }

    pub(crate) fn create_base_declaration(
        &self,
        id: Id<Node>,
        kind: SyntaxKind,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
    ) -> BaseNode {
        let mut node = self.create_base_node(id, kind);
        node.set_decorators(self.as_node_array(decorators));
        node.set_modifiers(self.as_node_array(modifiers));
        let flags = propagate_children_flags(node.maybe_decorators().as_deref())
            | propagate_children_flags(node.maybe_modifiers().as_deref());
        node.add_transform_flags(flags);
        // node.symbol = undefined!;
        // node.localSymbol = undefined!;
        // node.locals = undefined!;
        // node.nextContainer = undefined!;
        node
    }

    pub(crate) fn create_base_named_declaration<'name>(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        kind: SyntaxKind,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
    ) -> BaseNamedDeclaration {
        let node = self.create_base_declaration(id, kind, decorators, modifiers);
        let name = self.as_name(name);
        let node = BaseNamedDeclaration::new(node, name.clone());

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
                    node.add_transform_flags(propagate_child_flags(arena, Some(&*name)));
                }
            }
        }
        node
    }

    pub(crate) fn create_base_generic_named_declaration<'name>(
        &self,
        id: Id<Node>,
        kind: SyntaxKind,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
    ) -> BaseGenericNamedDeclaration {
        let node = self.create_base_named_declaration(id, kind, decorators, modifiers, name);
        let node = BaseGenericNamedDeclaration::new(node, self.as_node_array(type_parameters));
        let flags = propagate_children_flags(node.maybe_type_parameters().as_deref());
        node.add_transform_flags(flags);
        if node.maybe_type_parameters().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub(crate) fn create_base_signature_declaration<'name>(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        kind: SyntaxKind,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: Option<impl Into<NodeArrayOrVec>>,
        type_: Option<Id<Node>>,
    ) -> BaseSignatureDeclaration {
        let node = self.create_base_generic_named_declaration(
            id,
            kind,
            decorators,
            modifiers,
            name,
            type_parameters,
        );
        let node =
            BaseSignatureDeclaration::new(node, self.create_node_array(parameters, None), type_);
        node.add_transform_flags(
            propagate_children_flags(Some(&node.parameters()))
                | propagate_child_flags(arena, node.maybe_type()),
        );
        if node.maybe_type().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub(crate) fn update_base_signature_declaration(
        &self,
        arena: &RefCell<Arena<Node>>,
        updated: Id<Node>,
        original: Id<Node>,
    ) -> Id<Node> {
        // TODO: haven't added maybe_type_arguments() on SignatureDeclarationInterface yet (looks
        // like this logic is duplicated on updateBaseFunctionLikeDeclaration())
        // if let Some(original_type_arguments) = original_as_function_like_declaration.maybe_type_arguments().clone() {
        // }
        self.update(arena, updated, original)
    }

    pub(crate) fn create_base_function_like_declaration<'name>(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        kind: SyntaxKind,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        parameters: Option<impl Into<NodeArrayOrVec>>,
        type_: Option<Id<Node>>,
        body: Option<Id<Node>>,
    ) -> BaseFunctionLikeDeclaration {
        let node = self.create_base_signature_declaration(
            arena,
            id,
            kind,
            decorators,
            modifiers,
            name,
            type_parameters,
            parameters,
            type_,
        );
        let body_is_none = body.is_none();
        let node = BaseFunctionLikeDeclaration::new(node, body);
        node.add_transform_flags(
            propagate_child_flags(arena, node.maybe_body())
                & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        if body_is_none {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub(crate) fn update_base_function_like_declaration(
        &self,
        arena: &RefCell<Arena<Node>>,
        updated: Id<Node>,
        original: Id<Node>,
    ) -> Id<Node> {
        let updated_as_function_like_declaration = updated.as_function_like_declaration();
        if let Some(original_exclamation_token) = arena.borrow()[original]
            .as_function_like_declaration()
            .maybe_exclamation_token()
        {
            arena.borrow()[updated]
                .as_function_like_declaration_mut()
                .set_exclamation_token(Some(original_exclamation_token));
        }
        // TODO: haven't added maybe_type_arguments() on SignatureDeclarationInterface yet
        // if let Some(original_type_arguments) = original_as_function_like_declaration.maybe_type_arguments().clone() {
        // }
        self.update_base_signature_declaration(arena, updated, original)
    }

    pub(crate) fn create_base_interface_or_class_like_declaration<'name>(
        &self,
        id: Id<Node>,
        kind: SyntaxKind,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        heritage_clauses: Option<impl Into<NodeArrayOrVec>>,
    ) -> BaseInterfaceOrClassLikeDeclaration {
        let node = self.create_base_generic_named_declaration(
            id,
            kind,
            decorators,
            modifiers,
            name,
            type_parameters,
        );
        let node =
            BaseInterfaceOrClassLikeDeclaration::new(node, self.as_node_array(heritage_clauses));
        node.add_transform_flags(propagate_children_flags(
            node.maybe_heritage_clauses().as_deref(),
        ));
        node
    }

    pub(crate) fn create_base_class_like_declaration<'name>(
        &self,
        id: Id<Node>,
        kind: SyntaxKind,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        type_parameters: Option<impl Into<NodeArrayOrVec>>,
        heritage_clauses: Option<impl Into<NodeArrayOrVec>>,
        members: impl Into<NodeArrayOrVec>,
    ) -> ClassLikeDeclarationBase {
        let node = self.create_base_interface_or_class_like_declaration(
            id,
            kind,
            decorators,
            modifiers,
            name,
            type_parameters,
            heritage_clauses,
        );
        let node = ClassLikeDeclarationBase::new(node, self.create_node_array(Some(members), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.members())));
        node
    }

    pub(crate) fn create_base_binding_like_declaration<'name>(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        kind: SyntaxKind,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        initializer: Option<Gc<Node>>,
    ) -> BaseBindingLikeDeclaration {
        let node = self.create_base_named_declaration(arena, id, kind, decorators, modifiers, name);
        let node = BaseBindingLikeDeclaration::new(node, initializer);
        node.add_transform_flags(propagate_child_flags(arena, node.maybe_initializer()));
        node
    }

    pub(crate) fn create_base_variable_like_declaration<'name>(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        kind: SyntaxKind,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        name: Option<impl Into<StrOrRcNode<'name>>>,
        type_: Option<Gc<Node>>,
        initializer: Option<Gc<Node>>,
    ) -> BaseVariableLikeDeclaration {
        let node = self.create_base_binding_like_declaration(
            arena,
            id,
            kind,
            decorators,
            modifiers,
            name,
            initializer,
        );
        let type_is_some = type_.is_some();
        let node = BaseVariableLikeDeclaration::new(node, type_);
        node.add_transform_flags(propagate_child_flags(arena, node.maybe_type()));
        if type_is_some {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub(crate) fn create_base_literal(
        &self,
        id: Id<Node>,
        kind: SyntaxKind,
        text: String,
    ) -> BaseLiteralLikeNode {
        let node = self.create_base_token(id, kind);
        BaseLiteralLikeNode::new(node, text)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_numeric_literal_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        value: impl Into<StringOrNumber>,
        numeric_literal_flags: Option<TokenFlags>,
    ) -> NumericLiteral {
        let numeric_literal_flags = numeric_literal_flags.unwrap_or_default();
        let value = value.into();
        let node = self.create_base_literal(
            id,
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

    #[generate_node_factory_method_wrapper]
    pub fn create_big_int_literal_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        value: impl Into<PseudoBigIntOrString>,
    ) -> BigIntLiteral {
        let value = value.into();
        let node = self.create_base_literal(
            id,
            SyntaxKind::BigIntLiteral,
            match value {
                PseudoBigIntOrString::String(value) => value,
                PseudoBigIntOrString::PseudoBigInt(value) => {
                    format!("{}n", pseudo_big_int_to_string(&value))
                }
            },
        );
        let node = BigIntLiteral::new(node);
        node.add_transform_flags(TransformFlags::ContainsESNext);
        node
    }

    pub fn create_base_string_literal(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        text: String,
        is_single_quote: Option<bool>,
    ) -> StringLiteral {
        let node = self.create_base_literal(id, SyntaxKind::StringLiteral, text);
        StringLiteral::new(node, is_single_quote)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_string_literal_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        text: String,
        is_single_quote: Option<bool>,
        has_extended_unicode_escape: Option<bool>,
    ) -> StringLiteral {
        let node = self.create_base_string_literal(arena, id, text, is_single_quote);
        node.set_has_extended_unicode_escape(has_extended_unicode_escape);
        if matches!(has_extended_unicode_escape, Some(true)) {
            node.add_transform_flags(TransformFlags::ContainsES2015);
        }
        node
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_string_literal_from_node_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        source_node: Id<Node>, /*PropertyNameLiteral*/
    ) -> StringLiteral {
        let mut node = self.create_base_string_literal(
            arena,
            id,
            get_text_of_identifier_or_literal(&arena.borrow()[source_node]).into_owned(),
            None,
        );
        node.text_source_node = Some(source_node);
        node
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_regular_expression_literal_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        text: String,
    ) -> RegularExpressionLiteral {
        let node = self.create_base_literal(id, SyntaxKind::RegularExpressionLiteral, text);
        RegularExpressionLiteral::new(node)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_literal_like_node_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        kind: SyntaxKind, /*LiteralToken["kind"] | SyntaxKind.JsxTextAllWhiteSpaces*/
        text: String,
    ) -> Node {
        match kind {
            SyntaxKind::NumericLiteral => self
                .create_numeric_literal_raw(arena, id, text, Some(TokenFlags::None))
                .into(),
            SyntaxKind::BigIntLiteral => self.create_big_int_literal_raw(arena, id, text).into(),
            SyntaxKind::StringLiteral => self
                .create_string_literal_raw(arena, id, text, None, None)
                .into(),
            SyntaxKind::JsxText => self
                .create_jsx_text_raw(arena, id, text, Some(false))
                .into(),
            SyntaxKind::JsxTextAllWhiteSpaces => {
                self.create_jsx_text_raw(arena, id, text, Some(true)).into()
            }
            SyntaxKind::RegularExpressionLiteral => self
                .create_regular_expression_literal_raw(arena, id, text)
                .into(),
            SyntaxKind::NoSubstitutionTemplateLiteral => self
                .create_template_literal_like_node_raw(
                    arena,
                    id,
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
        id: Id<Node>,
        text: &str,
        mut original_keyword_kind: Option<SyntaxKind>,
    ) -> Identifier {
        if original_keyword_kind.is_none() && !text.is_empty() {
            original_keyword_kind = string_to_token(text);
        }
        if matches!(original_keyword_kind, Some(SyntaxKind::Identifier)) {
            original_keyword_kind = None;
        }
        let node = self
            .base_factory
            .create_base_identifier_node(id, SyntaxKind::Identifier);
        let mut node = Identifier::new(node, escape_leading_underscores(text).into_owned());
        node.original_keyword_kind = original_keyword_kind;
        node
    }

    pub fn create_base_generated_identifier(
        &self,
        id: Id<Node>,
        text: &str,
        auto_generate_flags: GeneratedIdentifierFlags,
    ) -> Identifier {
        let mut node = self.create_base_identifier(id, text, None);
        node.set_auto_generate_flags(Some(auto_generate_flags));
        node.auto_generate_id = Some(get_next_auto_generate_id());
        increment_next_auto_generate_id();
        node
    }

    pub fn create_identifier_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        text: &str,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
        original_keyword_kind: Option<SyntaxKind>,
    ) -> Identifier {
        let node = self.create_base_identifier(id, text, original_keyword_kind);
        if let Some(type_arguments) = type_arguments {
            *node.maybe_type_arguments_mut() =
                Some(self.create_node_array(Some(type_arguments), None));
        }
        if matches!(node.original_keyword_kind, Some(SyntaxKind::AwaitKeyword)) {
            node.add_transform_flags(TransformFlags::ContainsPossibleTopLevelAwait);
        }
        node
    }

    pub fn create_identifier_full(
        &self,
        arena: &RefCell<Arena<Node>>,
        text: &str,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
        original_keyword_kind: Option<SyntaxKind>,
    ) -> Id<Node> {
        arena.alloc_with_id(|id| {
            self.create_identifier_raw(arena, id, text, type_arguments, original_keyword_kind)
        })
    }

    pub fn create_identifier(&self, arena: &RefCell<Arena<Node>>, text: &str) -> Id<Node> {
        self.create_identifier_full(arena, text, Option::<Gc<NodeArray>>::None, None)
    }

    pub fn update_identifier(
        &self,
        arena: &RefCell<Arena<Node>>,
        node: Id<Node>, /*Identifier*/
        type_arguments: Option<
            impl Into<NodeArrayOrVec>, /*<TypeNode | TypeParameterDeclaration>*/
        >,
    ) -> Id<Node> {
        let type_arguments = type_arguments.map(Into::into);
        let node_type_arguments = arena.borrow()[node].as_identifier().maybe_type_arguments();
        if has_option_node_array_changed(node_type_arguments.as_deref(), type_arguments.as_ref()) {
            self.update(
                arena,
                self.create_identifier_full(arena, id_text(node), type_arguments, None),
                node,
            )
        } else {
            node
        }
    }

    pub fn create_temp_variable(
        &self,
        arena: &RefCell<Arena<Node>>,
        record_temp_variable: Option<impl FnMut(&Node /*Identifier*/)>,
        reserved_in_nested_scopes: Option<bool>,
    ) -> Id<Node /*GeneratedIdentifier*/> {
        let mut flags = GeneratedIdentifierFlags::Auto;
        if reserved_in_nested_scopes == Some(true) {
            flags |= GeneratedIdentifierFlags::ReservedInNestedScopes;
        }
        let name = arena
            .borrow_mut()
            .alloc_with_id(|id| self.create_base_generated_identifier(id, "", flags));
        if let Some(mut record_temp_variable) = record_temp_variable {
            record_temp_variable(&arena.borrow()[name]);
        }
        name
    }

    pub fn create_loop_variable(
        &self,
        arena: &RefCell<Arena<Node>>,
        reserved_in_nested_scopes: Option<bool>,
    ) -> Id<Node /*Identifier*/> {
        let mut flags = GeneratedIdentifierFlags::Loop;
        if reserved_in_nested_scopes == Some(true) {
            flags |= GeneratedIdentifierFlags::ReservedInNestedScopes;
        }
        arena
            .borrow_mut()
            .alloc_with_id(|id| self.create_base_generated_identifier(id, "", flags))
    }

    pub fn create_unique_name(
        &self,
        arena: &RefCell<Arena<Node>>,
        text: &str,
        flags: Option<GeneratedIdentifierFlags>,
    ) -> Id<Node /*Identifier*/> {
        let flags = flags.unwrap_or_default();
        Debug_.assert(
            !flags.intersects(GeneratedIdentifierFlags::KindMask),
            Some("Argument out of range: flags"),
        );
        Debug_.assert(
            (flags & (GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel)) != GeneratedIdentifierFlags::FileLevel,
            Some("GeneratedIdentifierFlags.FileLevel cannot be set without also setting GeneratedIdentifierFlags.Optimistic")
        );
        arena.borrow_mut().alloc_with_id(|id| {
            self.create_base_generated_identifier(
                id,
                text,
                GeneratedIdentifierFlags::Unique | flags,
            )
        })
    }

    pub fn get_generated_name_for_node(
        &self,
        arena: &RefCell<Arena<Node>>,
        node: Option<Id<Node>>,
        flags: Option<GeneratedIdentifierFlags>,
    ) -> Id<Node /*Identifier*/> {
        let flags = flags.unwrap_or_default();
        Debug_.assert(
            !flags.intersects(GeneratedIdentifierFlags::KindMask),
            Some("Argument out of range: flags"),
        );
        let text = if let Some(node) = node.filter(|node| is_identifier(&arena.borrow()[node])) {
            id_text(node)
        } else {
            ""
        };
        let name = arena.borrow_mut().alloc_with_id(|id| {
            self.create_base_generated_identifier(id, text, GeneratedIdentifierFlags::Node | flags)
        });
        arena.borrow_mut()[name].set_original(node);
        name
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_private_identifier_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        text: &str,
    ) -> PrivateIdentifier {
        if !starts_with(text, "#") {
            Debug_.fail(Some(&format!(
                "First character of private identifier must be #: {}",
                text
            )));
        }
        let node = self
            .base_factory
            .create_base_private_identifier_node(id, SyntaxKind::PrivateIdentifier);
        let mut node = PrivateIdentifier::new(node, escape_leading_underscores(text).into_owned());
        node.add_transform_flags(TransformFlags::ContainsClassFields);
        node
    }

    pub fn create_base_token(&self, id: Id<Node>, kind: SyntaxKind) -> BaseNode {
        self.base_factory.create_base_token_node(id, kind)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_token_raw(
        &self,
        arena: &RefCell<Arena<Node>>,
        id: Id<Node>,
        token: SyntaxKind,
    ) -> BaseNode {
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
        let mut node = self.create_base_token(id, token);
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
    maybe_changed: Option<&NodeArrayOrVec>,
) -> bool {
    match (existing, maybe_changed) {
        (None, None) => false,
        (Some(existing), Some(NodeArrayOrVec::NodeArray(maybe_changed))) => {
            !ptr::eq(existing, &**maybe_changed)
        }
        _ => true,
    }
}

pub fn has_node_array_changed(existing: &Gc<NodeArray>, maybe_changed: &NodeArrayOrVec) -> bool {
    !matches!(
        maybe_changed,
        NodeArrayOrVec::NodeArray(maybe_changed) if Gc::ptr_eq(
            existing,
            maybe_changed
        )
    )
}

pub fn has_option_str_or_node_changed(
    existing: Option<&Gc<Node>>,
    maybe_changed: Option<&StrOrRcNode<'_>>,
) -> bool {
    match (existing, maybe_changed) {
        (None, None) => false,
        (Some(existing), Some(StrOrRcNode::RcNode(maybe_changed))) => {
            !Gc::ptr_eq(existing, maybe_changed)
        }
        _ => true,
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

impl From<NumberOrRcNode> for StringOrNumberOrBoolOrRcNode {
    fn from(value: NumberOrRcNode) -> Self {
        match value {
            NumberOrRcNode::Number(value) => Self::Number(value),
            NumberOrRcNode::RcNode(value) => Self::RcNode(value),
        }
    }
}

impl<'str> From<StrOrRcNode<'str>> for StringOrNumberOrBoolOrRcNode {
    fn from(value: StrOrRcNode<'str>) -> Self {
        match value {
            StrOrRcNode::Str(value) => Self::String(value.to_owned()),
            StrOrRcNode::RcNode(value) => Self::RcNode(value),
        }
    }
}

impl From<BoolOrRcNode> for StringOrNumberOrBoolOrRcNode {
    fn from(value: BoolOrRcNode) -> Self {
        match value {
            BoolOrRcNode::Bool(value) => Self::Bool(value),
            BoolOrRcNode::RcNode(value) => Self::RcNode(value),
        }
    }
}

pub enum NumberOrRcNode {
    Number(Number),
    RcNode(Gc<Node>),
}

impl From<Number> for NumberOrRcNode {
    fn from(value: Number) -> Self {
        Self::Number(value)
    }
}

impl From<Gc<Node>> for NumberOrRcNode {
    fn from(value: Gc<Node>) -> Self {
        Self::RcNode(value)
    }
}
