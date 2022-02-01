use std::rc::Rc;

use super::get_default_tag_name_for_kind;
use crate::{
    BaseJSDocTag, BaseJSDocTypeLikeTag, BaseJSDocUnaryType, BaseNode, BaseNodeFactory, Node,
    NodeFactory, StringOrNodeArray, SyntaxKind,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub(crate) fn create_jsdoc_primary_type_worker(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
    ) -> BaseNode {
        self.create_base_node(base_factory, kind)
    }

    pub(crate) fn create_jsdoc_unary_type_worker(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        type_: Option<Rc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        let node = self.create_base_node(base_factory, kind);
        let node = BaseJSDocUnaryType::new(node, type_);
        node
    }

    pub(crate) fn create_base_jsdoc_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        tag_name: Rc<Node /*Identifier*/>,
        comment: Option<TComment>,
    ) -> BaseJSDocTag {
        let node = self.create_base_node(base_factory, kind);
        let node = BaseJSDocTag::new(node, tag_name, comment.map(Into::into));
        node
    }

    pub(crate) fn create_jsdoc_simple_tag_worker<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        comment: Option<TComment>,
    ) -> BaseJSDocTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            kind,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(base_factory, get_default_tag_name_for_kind(kind))
                    .into()
            }),
            comment,
        );
        node
    }

    pub(crate) fn create_jsdoc_type_like_tag_worker<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        type_expression: Option<Rc<Node /*JSDocTypeExpression*/>>,
        comment: Option<TComment>,
    ) -> BaseJSDocTypeLikeTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            kind,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(base_factory, get_default_tag_name_for_kind(kind))
                    .into()
            }),
            comment,
        );
        let node = BaseJSDocTypeLikeTag::new(node, type_expression);
        node
    }
}
