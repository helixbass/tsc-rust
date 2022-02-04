use std::rc::Rc;

use super::{get_default_tag_name_for_kind, propagate_child_flags};
use crate::{
    AssertClause, AssertEntry, BaseJSDocTag, BaseJSDocTypeLikeTag, BaseJSDocUnaryType, BaseNode,
    BaseNodeFactory, JsxText, NamespaceExport, NamespaceImport, Node, NodeArray, NodeFactory,
    NodeInterface, StringOrNodeArray, SyntaxKind, TransformFlags,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn create_assert_clause(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: NodeArray, /*<AssertEntry>*/
        multi_line: Option<bool>,
    ) -> AssertClause {
        let node = self.create_base_node(base_factory, SyntaxKind::AssertClause);
        let mut node = AssertClause::new(node, elements, multi_line);
        node.add_transform_flags(TransformFlags::ContainsESNext);
        node
    }

    pub fn create_assert_entry(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Rc<Node /*AssertionKey*/>,
        value: Rc<Node /*StringLiteral*/>,
    ) -> AssertEntry {
        let node = self.create_base_node(base_factory, SyntaxKind::AssertEntry);
        let mut node = AssertEntry::new(node, name, value);
        node.add_transform_flags(TransformFlags::ContainsESNext);
        node
    }

    pub fn create_namespace_import(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Rc<Node /*Identifier*/>,
    ) -> NamespaceImport {
        let node = self.create_base_node(base_factory, SyntaxKind::NamespaceImport);
        let mut node = NamespaceImport::new(node, name);
        node.add_transform_flags(propagate_child_flags(Some(&*node.name)));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn create_namespace_export(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Rc<Node /*Identifier*/>,
    ) -> NamespaceExport {
        let node = self.create_base_node(base_factory, SyntaxKind::NamespaceExport);
        let mut node = NamespaceExport::new(node, name);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.name)) | TransformFlags::ContainsESNext,
        );
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

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
                self.create_identifier(
                    base_factory,
                    get_default_tag_name_for_kind(kind),
                    Option::<NodeArray>::None,
                    None,
                )
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
                self.create_identifier(
                    base_factory,
                    get_default_tag_name_for_kind(kind),
                    Option::<NodeArray>::None,
                    None,
                )
                .into()
            }),
            comment,
        );
        let node = BaseJSDocTypeLikeTag::new(node, type_expression);
        node
    }

    pub fn create_jsx_text(
        &self,
        base_factory: &TBaseNodeFactory,
        text: String,
        contains_only_trivia_white_spaces: Option<bool>,
    ) -> JsxText {
        let node = self.create_base_node(base_factory, SyntaxKind::JsxText);
        let mut node = JsxText::new(
            node,
            text,
            contains_only_trivia_white_spaces.unwrap_or(false),
        );
        node.add_transform_flags(TransformFlags::ContainsJsx);
        node
    }
}
