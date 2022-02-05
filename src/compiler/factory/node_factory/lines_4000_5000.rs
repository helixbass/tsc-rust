use std::rc::Rc;

use super::{get_default_tag_name_for_kind, propagate_child_flags, propagate_children_flags};
use crate::{
    escape_leading_underscores, get_jsdoc_type_alias_name, AssertClause, AssertEntry, BaseJSDocTag,
    BaseJSDocTypeLikeTag, BaseJSDocUnaryType, BaseNode, BaseNodeFactory, ExportAssignment,
    ExportDeclaration, ExportSpecifier, ExternalModuleReference, ImportSpecifier,
    JSDocFunctionType, JSDocPropertyLikeTag, JSDocSignature, JSDocTemplateTag, JSDocTypeExpression,
    JSDocTypeLiteral, JSDocTypedefTag, JsxText, MissingDeclaration, NamedExports, NamedImports,
    NamespaceExport, NamespaceImport, Node, NodeArray, NodeArrayOrVec, NodeFactory, NodeInterface,
    StringOrNodeArray, StringOrRcNode, SyntaxKind, TransformFlags,
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

    pub fn create_named_imports<TElements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: TElements,
    ) -> NamedImports {
        let node = self.create_base_node(base_factory, SyntaxKind::NamedImports);
        let mut node = NamedImports::new(node, self.create_node_array(Some(elements), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.elements)));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn create_import_specifier(
        &self,
        base_factory: &TBaseNodeFactory,
        is_type_only: bool,
        property_name: Option<Rc<Node /*Identifier*/>>,
        name: Rc<Node /*Identifier*/>,
    ) -> ImportSpecifier {
        let node = self.create_base_node(base_factory, SyntaxKind::ImportSpecifier);
        let mut node = ImportSpecifier::new(node, is_type_only, property_name, name);
        node.add_transform_flags(
            propagate_child_flags(node.property_name.clone())
                | propagate_child_flags(Some(&*node.name)),
        );
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn create_export_assignment<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        is_export_equals: Option<bool>,
        expression: Rc<Node /*Expression*/>,
    ) -> ExportAssignment {
        let node = self.create_base_declaration(
            base_factory,
            SyntaxKind::ExportAssignment,
            decorators,
            modifiers,
        );
        let mut node = ExportAssignment::new(
            node,
            is_export_equals,
            if matches!(is_export_equals, Some(true)) {
                self.parenthesizer_rules()
                    .parenthesize_right_side_of_binary(
                        base_factory,
                        SyntaxKind::EqualsToken,
                        None,
                        &expression,
                    )
            } else {
                self.parenthesizer_rules()
                    .parenthesize_expression_of_export_default(base_factory, &expression)
            },
        );
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn create_export_declaration<
        TDecorators: Into<NodeArrayOrVec>,
        TModifiers: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<TDecorators>,
        modifiers: Option<TModifiers>,
        is_type_only: bool,
        export_clause: Option<Rc<Node /*NamedExportBindings*/>>,
        module_specifier: Option<Rc<Node /*Expression*/>>,
        assert_clause: Option<Rc<Node /*AssertClause*/>>,
    ) -> ExportDeclaration {
        let node = self.create_base_declaration(
            base_factory,
            SyntaxKind::ExportDeclaration,
            decorators,
            modifiers,
        );
        let mut node = ExportDeclaration::new(
            node,
            is_type_only,
            export_clause,
            module_specifier,
            assert_clause,
        );
        node.add_transform_flags(
            propagate_child_flags(node.export_clause.clone())
                | propagate_child_flags(node.module_specifier.clone()),
        );
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn create_named_exports<TElements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: TElements,
    ) -> NamedExports {
        let node = self.create_base_node(base_factory, SyntaxKind::NamedExports);
        let mut node = NamedExports::new(node, self.create_node_array(Some(elements), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.elements)));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn create_export_specifier<
        TPropertyName: Into<StringOrRcNode>,
        TName: Into<StringOrRcNode>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        is_type_only: bool,
        property_name: Option<TPropertyName /*Identifier*/>,
        name: TName, /*Identifier*/
    ) -> ExportSpecifier {
        let node = self.create_base_node(base_factory, SyntaxKind::ExportSpecifier);
        let mut node = ExportSpecifier::new(
            node,
            is_type_only,
            self.as_name(base_factory, property_name),
            self.as_name(base_factory, Some(name)).unwrap(),
        );
        node.add_transform_flags(
            propagate_child_flags(node.property_name.clone())
                | propagate_child_flags(Some(&*node.name)),
        );
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn create_missing_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
    ) -> MissingDeclaration {
        let node = self.create_base_declaration(
            base_factory,
            SyntaxKind::MissingDeclaration,
            Option::<NodeArray>::None,
            Option::<NodeArray>::None,
        );
        MissingDeclaration::new(node)
    }

    pub fn create_external_module_reference(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Rc<Node /*Expression*/>,
    ) -> ExternalModuleReference {
        let node = self.create_base_node(base_factory, SyntaxKind::ExternalModuleReference);
        let mut node = ExternalModuleReference::new(node, expression);
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
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

    pub fn create_jsdoc_function_type<TParameters: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        parameters: TParameters,
        type_: Option<Rc<Node /*TypeNode*/>>,
    ) -> JSDocFunctionType {
        let node = self.create_base_signature_declaration(
            base_factory,
            SyntaxKind::JSDocFunctionType,
            Option::<NodeArray>::None,
            Option::<NodeArray>::None,
            Option::<Rc<Node>>::None,
            Option::<NodeArray>::None,
            Some(parameters),
            type_,
        );
        JSDocFunctionType::new(node)
    }

    pub fn create_jsdoc_type_literal<TPropertyTags: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        property_tags: Option<TPropertyTags>,
        is_array_type: Option<bool>,
    ) -> JSDocTypeLiteral {
        let is_array_type = is_array_type.unwrap_or(false);
        let node = self.create_base_node(base_factory, SyntaxKind::JSDocTypeLiteral);
        JSDocTypeLiteral::new(node, self.as_node_array(property_tags), is_array_type)
    }

    pub fn create_jsdoc_type_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Rc<Node /*TypeNode*/>,
    ) -> JSDocTypeExpression {
        let node = self.create_base_node(base_factory, SyntaxKind::JSDocTypeExpression);
        JSDocTypeExpression::new(node, type_)
    }

    pub fn create_jsdoc_signature<
        TTypeParameters: Into<NodeArrayOrVec>,
        TParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        type_parameters: Option<TTypeParameters /*<JSDocTemplateTag>*/>,
        parameters: TParameters, /*<JSDocParameterTag>*/
        type_: Option<Rc<Node /*JSDocReturnTag*/>>,
    ) -> JSDocSignature {
        let node = self.create_base_node(base_factory, SyntaxKind::JSDocSignature);
        JSDocSignature::new(
            node,
            self.as_node_array(type_parameters),
            self.create_node_array(Some(parameters), None),
            type_,
        )
    }

    fn get_default_tag_name(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*JSDocTag*/
    ) -> Rc<Node /*Identifier*/> {
        let default_tag_name = get_default_tag_name_for_kind(node.kind());
        let node_as_jsdoc_tag = node.as_jsdoc_tag();
        if node_as_jsdoc_tag.tag_name().as_identifier().escaped_text
            == escape_leading_underscores(default_tag_name)
        {
            node_as_jsdoc_tag.tag_name()
        } else {
            self.create_identifier(
                base_factory,
                default_tag_name,
                Option::<NodeArray>::None,
                None,
            )
            .into()
        }
    }

    pub(crate) fn create_base_jsdoc_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        tag_name: Rc<Node /*Identifier*/>,
        comment: Option<TComment>,
    ) -> BaseJSDocTag {
        let node = self.create_base_node(base_factory, kind);
        BaseJSDocTag::new(node, tag_name, comment.map(Into::into))
    }

    pub(crate) fn create_jsdoc_template_tag<
        TTypeParameters: Into<NodeArrayOrVec>,
        TComment: Into<StringOrNodeArray>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        constraint: Option<Rc<Node /*JSDocTypeExpression*/>>,
        type_parameters: TTypeParameters, /*<TypeParameterDeclaration>*/
        comment: Option<TComment>,
    ) -> JSDocTemplateTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            SyntaxKind::JSDocTemplateTag,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(base_factory, "template", Option::<NodeArray>::None, None)
                    .into()
            }),
            comment,
        );
        JSDocTemplateTag::new(
            node,
            constraint,
            self.create_node_array(Some(type_parameters), None),
        )
    }

    pub(crate) fn create_jsdoc_typedef_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        type_expression: Option<Rc<Node /*JSDocTypeExpression*/>>,
        full_name: Option<Rc<Node /*Identifier | JSDocNamespaceDeclaration*/>>,
        comment: Option<TComment>,
    ) -> JSDocTypedefTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            SyntaxKind::JSDocTypedefTag,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(base_factory, "typedef", Option::<NodeArray>::None, None)
                    .into()
            }),
            comment,
        );
        JSDocTypedefTag::new(
            node,
            type_expression,
            full_name.clone(),
            get_jsdoc_type_alias_name(full_name),
        )
    }

    pub(crate) fn create_jsdoc_parameter_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        name: Rc<Node /*EntityName*/>,
        is_bracketed: bool,
        type_expression: Option<Rc<Node /*JSDocTypeExpression*/>>,
        is_name_first: Option<bool>,
        comment: Option<TComment>,
    ) -> JSDocPropertyLikeTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            SyntaxKind::JSDocParameterTag,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(base_factory, "param", Option::<NodeArray>::None, None)
                    .into()
            }),
            comment,
        );
        JSDocPropertyLikeTag::new(
            node,
            type_expression,
            name,
            is_name_first.unwrap_or(false),
            is_bracketed,
        )
    }

    pub(crate) fn create_jsdoc_property_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Rc<Node /*Identifier*/>>,
        name: Rc<Node /*EntityName*/>,
        is_bracketed: bool,
        type_expression: Option<Rc<Node /*JSDocTypeExpression*/>>,
        is_name_first: Option<bool>,
        comment: Option<TComment>,
    ) -> JSDocPropertyLikeTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            SyntaxKind::JSDocPropertyTag,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(base_factory, "prop", Option::<NodeArray>::None, None)
                    .into()
            }),
            comment,
        );
        JSDocPropertyLikeTag::new(
            node,
            type_expression,
            name,
            is_name_first.unwrap_or(false),
            is_bracketed,
        )
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
