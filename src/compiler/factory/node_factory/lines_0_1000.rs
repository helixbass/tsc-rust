#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Ref, RefCell};
use std::rc::Rc;

use super::{
    aggregate_children_flags, update_with_original, update_without_original, PseudoBigIntOrString,
};
use crate::{
    create_parenthesizer_rules, escape_leading_underscores, null_parenthesizer_rules,
    pseudo_big_int_to_string, BaseBindingLikeDeclaration, BaseFunctionLikeDeclaration,
    BaseGenericNamedDeclaration, BaseInterfaceOrClassLikeDeclaration, BaseLiteralLikeNode,
    BaseNamedDeclaration, BaseNode, BaseNodeFactory, BaseSignatureDeclaration,
    BaseVariableLikeDeclaration, BigIntLiteral, Debug_, Identifier, LiteralLikeNode,
    LiteralLikeNodeInterface, Node, NodeArray, NodeArrayOrVec, NodeFactory, NodeInterface,
    NumericLiteral, ParenthesizerRules, ReadonlyTextRange, StringLiteral, SyntaxKind, TokenFlags,
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
        node
    }

    pub(crate) fn create_base_named_declaration(
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

    pub(crate) fn create_base_generic_named_declaration<TTypeParameters: Into<NodeArrayOrVec>>(
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

    pub(crate) fn create_base_signature_declaration(
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

    pub(crate) fn create_base_function_like_declaration(
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
