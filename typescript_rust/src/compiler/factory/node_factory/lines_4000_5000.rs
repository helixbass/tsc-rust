use gc::{Finalize, Gc, Trace};

use super::{get_default_tag_name_for_kind, propagate_child_flags, propagate_children_flags};
use crate::{
    are_option_gcs_equal, escape_leading_underscores, get_jsdoc_type_alias_name,
    has_node_array_changed, has_option_node_array_changed, is_variable_declaration, AssertClause,
    AssertEntry, BaseJSDocTag, BaseJSDocTypeLikeTag, BaseJSDocUnaryType, BaseNode, BaseNodeFactory,
    CaseClause, CatchClause, Debug_, DefaultClause, ExportAssignment, ExportDeclaration,
    ExportSpecifier, ExternalModuleReference, HasTypeArgumentsInterface, HeritageClause,
    ImportSpecifier, JSDoc, JSDocAugmentsTag, JSDocCallbackTag, JSDocFunctionType,
    JSDocImplementsTag, JSDocLink, JSDocLinkCode, JSDocLinkPlain, JSDocMemberName,
    JSDocNameReference, JSDocPropertyLikeTag, JSDocSeeTag, JSDocSignature, JSDocTemplateTag,
    JSDocText, JSDocTypeExpression, JSDocTypeLiteral, JSDocTypedefTag, JsxAttribute, JsxAttributes,
    JsxClosingElement, JsxClosingFragment, JsxElement, JsxExpression, JsxFragment,
    JsxOpeningElement, JsxOpeningFragment, JsxSelfClosingElement, JsxSpreadAttribute, JsxText,
    LiteralLikeNodeInterface, MissingDeclaration, NamedExports, NamedImports, NamespaceExport,
    NamespaceImport, Node, NodeArray, NodeArrayOrVec, NodeFactory, NodeInterface, StrOrRcNode,
    StringOrNodeArray, SyntaxKind, TransformFlags,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize> NodeFactory<TBaseNodeFactory> {
    pub fn create_assert_clause(
        &self,
        elements: Gc<NodeArray>, /*<AssertEntry>*/
        multi_line: Option<bool>,
    ) -> AssertClause {
        let node = self.create_base_node(SyntaxKind::AssertClause);
        let node = AssertClause::new(node, elements, multi_line);
        node.add_transform_flags(TransformFlags::ContainsESNext);
        node
    }

    pub fn update_assert_clause(
        &self,
        node: &Node,             /*AssertClause*/
        elements: Gc<NodeArray>, /*<AssertEntry>*/
        multi_line: Option<bool>,
    ) -> Gc<Node> {
        let node_as_assert_clause = node.as_assert_clause();
        if !Gc::ptr_eq(&node_as_assert_clause.elements, &elements)
            || node_as_assert_clause.multi_line != multi_line
        {
            self.update(self.create_assert_clause(elements, multi_line).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_assert_entry(
        &self,
        name: Gc<Node /*AssertionKey*/>,
        value: Gc<Node /*StringLiteral*/>,
    ) -> AssertEntry {
        let node = self.create_base_node(SyntaxKind::AssertEntry);
        let node = AssertEntry::new(node, name, value);
        node.add_transform_flags(TransformFlags::ContainsESNext);
        node
    }

    pub fn update_assert_entry(
        &self,
        node: &Node, /*AssertEntry*/
        name: Gc<Node /*AssertionKey*/>,
        value: Gc<Node /*StringLiteral*/>,
    ) -> Gc<Node> {
        let node_as_assert_entry = node.as_assert_entry();
        if !Gc::ptr_eq(&node_as_assert_entry.name, &name)
            || !Gc::ptr_eq(&node_as_assert_entry.value, &value)
        {
            self.update(self.create_assert_entry(name, value).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_namespace_import(&self, name: Gc<Node /*Identifier*/>) -> NamespaceImport {
        let node = self.create_base_node(SyntaxKind::NamespaceImport);
        let node = NamespaceImport::new(node, name);
        node.add_transform_flags(propagate_child_flags(Some(&*node.name)));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_namespace_import(
        &self,
        node: &Node, /*NamespaceImport*/
        name: Gc<Node /*Identifier*/>,
    ) -> Gc<Node> {
        let node_as_namespace_import = node.as_namespace_import();
        if !Gc::ptr_eq(&node_as_namespace_import.name, &name) {
            self.update(self.create_namespace_import(name).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_namespace_export(&self, name: Gc<Node /*Identifier*/>) -> NamespaceExport {
        let node = self.create_base_node(SyntaxKind::NamespaceExport);
        let node = NamespaceExport::new(node, name);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.name)) | TransformFlags::ContainsESNext,
        );
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_namespace_export(
        &self,
        node: &Node, /*NamespaceExport*/
        name: Gc<Node /*Identifier*/>,
    ) -> Gc<Node> {
        let node_as_namespace_export = node.as_namespace_export();
        if !Gc::ptr_eq(&node_as_namespace_export.name, &name) {
            self.update(self.create_namespace_export(name).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_named_imports(&self, elements: impl Into<NodeArrayOrVec>) -> NamedImports {
        let node = self.create_base_node(SyntaxKind::NamedImports);
        let node = NamedImports::new(node, self.create_node_array(Some(elements), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.elements)));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_named_imports(
        &self,
        node: &Node, /*NamedImports*/
        elements: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_named_imports = node.as_named_imports();
        let elements = elements.into();
        if has_node_array_changed(&node_as_named_imports.elements, &elements) {
            self.update(self.create_named_imports(elements).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_import_specifier(
        &self,
        is_type_only: bool,
        property_name: Option<Gc<Node /*Identifier*/>>,
        name: Gc<Node /*Identifier*/>,
    ) -> ImportSpecifier {
        let node = self.create_base_node(SyntaxKind::ImportSpecifier);
        let node = ImportSpecifier::new(node, is_type_only, property_name, name);
        node.add_transform_flags(
            propagate_child_flags(node.property_name.clone())
                | propagate_child_flags(Some(&*node.name)),
        );
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_import_specifier(
        &self,
        node: &Node, /*ImportSpecifier*/
        is_type_only: bool,
        property_name: Option<Gc<Node /*Identifier*/>>,
        name: Gc<Node /*Identifier*/>,
    ) -> Gc<Node> {
        let node_as_import_specifier = node.as_import_specifier();
        if node_as_import_specifier.is_type_only != is_type_only
            || !are_option_gcs_equal(
                node_as_import_specifier.property_name.as_ref(),
                property_name.as_ref(),
            )
            || !Gc::ptr_eq(&node_as_import_specifier.name, &name)
        {
            self.update(
                self.create_import_specifier(is_type_only, property_name, name)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_export_assignment(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        is_export_equals: Option<bool>,
        expression: Gc<Node /*Expression*/>,
    ) -> ExportAssignment {
        let node =
            self.create_base_declaration(SyntaxKind::ExportAssignment, decorators, modifiers);
        let node = ExportAssignment::new(
            node,
            is_export_equals,
            if matches!(is_export_equals, Some(true)) {
                self.parenthesizer_rules()
                    .parenthesize_right_side_of_binary(SyntaxKind::EqualsToken, None, &expression)
            } else {
                self.parenthesizer_rules()
                    .parenthesize_expression_of_export_default(&expression)
            },
        );
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_export_assignment(
        &self,
        node: &Node, /*ExportAssignment*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_export_assignment = node.as_export_assignment();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || !Gc::ptr_eq(&node_as_export_assignment.expression, &expression)
        {
            self.update(
                self.create_export_assignment(
                    decorators,
                    modifiers,
                    node_as_export_assignment.is_export_equals,
                    expression,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_export_declaration(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        is_type_only: bool,
        export_clause: Option<Gc<Node /*NamedExportBindings*/>>,
        module_specifier: Option<Gc<Node /*Expression*/>>,
        assert_clause: Option<Gc<Node /*AssertClause*/>>,
    ) -> ExportDeclaration {
        let node =
            self.create_base_declaration(SyntaxKind::ExportDeclaration, decorators, modifiers);
        let node = ExportDeclaration::new(
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

    pub fn update_export_declaration(
        &self,
        node: &Node, /*ExportDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        is_type_only: bool,
        export_clause: Option<Gc<Node /*NamedExportBindings*/>>,
        module_specifier: Option<Gc<Node /*Expression*/>>,
        assert_clause: Option<Gc<Node /*AssertClause*/>>,
    ) -> Gc<Node> {
        let node_as_export_declaration = node.as_export_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.maybe_modifiers().as_deref(), modifiers.as_ref())
            || node_as_export_declaration.is_type_only != is_type_only
            || !are_option_gcs_equal(
                node_as_export_declaration.export_clause.as_ref(),
                export_clause.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_export_declaration.module_specifier.as_ref(),
                module_specifier.as_ref(),
            )
            || !are_option_gcs_equal(
                node_as_export_declaration.assert_clause.as_ref(),
                assert_clause.as_ref(),
            )
        {
            self.update(
                self.create_export_declaration(
                    decorators,
                    modifiers,
                    is_type_only,
                    export_clause,
                    module_specifier,
                    assert_clause,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_named_exports(&self, elements: impl Into<NodeArrayOrVec>) -> NamedExports {
        let node = self.create_base_node(SyntaxKind::NamedExports);
        let node = NamedExports::new(node, self.create_node_array(Some(elements), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.elements)));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_named_exports(
        &self,
        node: &Node, /*NamedExports*/
        elements: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_named_exports = node.as_named_exports();
        let elements = elements.into();
        if has_node_array_changed(&node_as_named_exports.elements, &elements) {
            self.update(self.create_named_exports(elements).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_export_specifier<'property_name, 'name>(
        &self,
        is_type_only: bool,
        property_name: Option<
            impl Into<StrOrRcNode<'property_name>>,
            /*Identifier*/
        >,
        name: impl Into<StrOrRcNode<'name>>,
        /*Identifier*/
    ) -> ExportSpecifier {
        let node = self.create_base_node(SyntaxKind::ExportSpecifier);
        let node = ExportSpecifier::new(
            node,
            is_type_only,
            self.as_name(property_name),
            self.as_name(Some(name)).unwrap(),
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

    pub fn update_export_specifier(
        &self,
        node: &Node, /*ExportSpecifier*/
        is_type_only: bool,
        property_name: Option<Gc<Node> /*Identifier*/>,
        name: Gc<Node>,
        /*Identifier*/
    ) -> Gc<Node> {
        let node_as_export_specifier = node.as_export_specifier();
        if node_as_export_specifier.is_type_only != is_type_only
            || !are_option_gcs_equal(
                node_as_export_specifier.property_name.as_ref(),
                property_name.as_ref(),
            )
            || !Gc::ptr_eq(&node_as_export_specifier.name, &name)
        {
            self.update(
                self.create_export_specifier(is_type_only, property_name, name)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_missing_declaration(&self) -> MissingDeclaration {
        let node = self.create_base_declaration(
            SyntaxKind::MissingDeclaration,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
        );
        MissingDeclaration::new(node)
    }

    pub fn create_external_module_reference(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> ExternalModuleReference {
        let node = self.create_base_node(SyntaxKind::ExternalModuleReference);
        let node = ExternalModuleReference::new(node, expression);
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_external_module_reference(
        &self,
        node: &Node, /*ExternalModuleReference*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_external_module_reference = node.as_external_module_reference();
        if !Gc::ptr_eq(&node_as_external_module_reference.expression, &expression) {
            self.update(
                self.create_external_module_reference(expression).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub(crate) fn create_jsdoc_primary_type_worker(&self, kind: SyntaxKind) -> BaseNode {
        self.create_base_node(kind)
    }

    pub(crate) fn create_jsdoc_unary_type_worker(
        &self,
        kind: SyntaxKind,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        let node = self.create_base_node(kind);
        let node = BaseJSDocUnaryType::new(node, type_);
        node
    }

    pub fn create_jsdoc_function_type<TParameters: Into<NodeArrayOrVec>>(
        &self,
        parameters: TParameters,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> JSDocFunctionType {
        let node = self.create_base_signature_declaration(
            SyntaxKind::JSDocFunctionType,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<Node>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(parameters),
            type_,
        );
        JSDocFunctionType::new(node)
    }

    pub fn create_jsdoc_type_literal<TPropertyTags: Into<NodeArrayOrVec>>(
        &self,
        property_tags: Option<TPropertyTags>,
        is_array_type: Option<bool>,
    ) -> JSDocTypeLiteral {
        let is_array_type = is_array_type.unwrap_or(false);
        let node = self.create_base_node(SyntaxKind::JSDocTypeLiteral);
        JSDocTypeLiteral::new(node, self.as_node_array(property_tags), is_array_type)
    }

    pub fn create_jsdoc_type_expression(
        &self,
        type_: Gc<Node /*TypeNode*/>,
    ) -> JSDocTypeExpression {
        let node = self.create_base_node(SyntaxKind::JSDocTypeExpression);
        JSDocTypeExpression::new(node, type_)
    }

    pub fn create_jsdoc_signature<
        TTypeParameters: Into<NodeArrayOrVec>,
        TParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        type_parameters: Option<TTypeParameters /*<JSDocTemplateTag>*/>,
        parameters: TParameters, /*<JSDocParameterTag>*/
        type_: Option<Gc<Node /*JSDocReturnTag*/>>,
    ) -> JSDocSignature {
        let node = self.create_base_node(SyntaxKind::JSDocSignature);
        JSDocSignature::new(
            node,
            self.as_node_array(type_parameters),
            self.create_node_array(Some(parameters), None),
            type_,
        )
    }

    #[allow(dead_code)]
    fn get_default_tag_name(&self, node: &Node /*JSDocTag*/) -> Gc<Node /*Identifier*/> {
        let default_tag_name = get_default_tag_name_for_kind(node.kind());
        let node_as_jsdoc_tag = node.as_jsdoc_tag();
        if node_as_jsdoc_tag.tag_name().as_identifier().escaped_text
            == escape_leading_underscores(default_tag_name)
        {
            node_as_jsdoc_tag.tag_name()
        } else {
            self.create_identifier(default_tag_name)
        }
    }

    pub(crate) fn create_base_jsdoc_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        kind: SyntaxKind,
        tag_name: Gc<Node /*Identifier*/>,
        comment: Option<TComment>,
    ) -> BaseJSDocTag {
        let node = self.create_base_node(kind);
        BaseJSDocTag::new(node, tag_name, comment.map(Into::into))
    }

    pub fn create_jsdoc_template_tag<
        TTypeParameters: Into<NodeArrayOrVec>,
        TComment: Into<StringOrNodeArray>,
    >(
        &self,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        constraint: Option<Gc<Node /*JSDocTypeExpression*/>>,
        type_parameters: TTypeParameters, /*<TypeParameterDeclaration>*/
        comment: Option<TComment>,
    ) -> JSDocTemplateTag {
        let node = self.create_base_jsdoc_tag(
            SyntaxKind::JSDocTemplateTag,
            tag_name.unwrap_or_else(|| self.create_identifier("template")),
            comment,
        );
        JSDocTemplateTag::new(
            node,
            constraint,
            self.create_node_array(Some(type_parameters), None),
        )
    }

    pub fn create_jsdoc_typedef_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
        full_name: Option<Gc<Node /*Identifier | JSDocNamespaceDeclaration*/>>,
        comment: Option<TComment>,
    ) -> JSDocTypedefTag {
        let node = self.create_base_jsdoc_tag(
            SyntaxKind::JSDocTypedefTag,
            tag_name.unwrap_or_else(|| self.create_identifier("typedef")),
            comment,
        );
        JSDocTypedefTag::new(
            node,
            full_name.clone(),
            get_jsdoc_type_alias_name(full_name),
            type_expression,
        )
    }

    pub fn create_jsdoc_parameter_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        name: Gc<Node /*EntityName*/>,
        is_bracketed: bool,
        type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
        is_name_first: Option<bool>,
        comment: Option<TComment>,
    ) -> JSDocPropertyLikeTag {
        let node = self.create_base_jsdoc_tag(
            SyntaxKind::JSDocParameterTag,
            tag_name.unwrap_or_else(|| self.create_identifier("param")),
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

    pub fn create_jsdoc_property_tag(
        &self,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        name: Gc<Node /*EntityName*/>,
        is_bracketed: bool,
        type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
        is_name_first: Option<bool>,
        comment: Option<impl Into<StringOrNodeArray>>,
    ) -> JSDocPropertyLikeTag {
        let node = self.create_base_jsdoc_tag(
            SyntaxKind::JSDocPropertyTag,
            tag_name.unwrap_or_else(|| self.create_identifier("prop")),
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

    pub fn create_jsdoc_callback_tag(
        &self,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        type_expression: Gc<Node /*JSDocSignature*/>,
        full_name: Option<Gc<Node /*Identifier | JSDocNamespaceDeclaration*/>>,
        comment: Option<impl Into<StringOrNodeArray>>,
    ) -> JSDocCallbackTag {
        let node = self.create_base_jsdoc_tag(
            SyntaxKind::JSDocCallbackTag,
            tag_name.unwrap_or_else(|| self.create_identifier("callback")),
            comment,
        );
        JSDocCallbackTag::new(
            node,
            type_expression,
            full_name.clone(),
            get_jsdoc_type_alias_name(full_name),
        )
    }

    pub fn create_jsdoc_augments_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        class_name: Gc<Node /*JSDocAugmentsTag["class"]*/>,
        comment: Option<TComment>,
    ) -> JSDocAugmentsTag {
        let node = self.create_base_jsdoc_tag(
            SyntaxKind::JSDocAugmentsTag,
            tag_name.unwrap_or_else(|| self.create_identifier("augments")),
            comment,
        );
        JSDocAugmentsTag::new(node, class_name)
    }

    pub fn create_jsdoc_implements_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        class_name: Gc<Node /*JSDocImplementsTag["class"]*/>,
        comment: Option<TComment>,
    ) -> JSDocImplementsTag {
        let node = self.create_base_jsdoc_tag(
            SyntaxKind::JSDocImplementsTag,
            tag_name.unwrap_or_else(|| self.create_identifier("implements")),
            comment,
        );
        JSDocImplementsTag::new(node, class_name)
    }

    pub fn create_jsdoc_see_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        name: Option<Gc<Node /*JSDocNameReference*/>>,
        comment: Option<TComment>,
    ) -> JSDocSeeTag {
        let node = self.create_base_jsdoc_tag(
            SyntaxKind::JSDocSeeTag,
            tag_name.unwrap_or_else(|| self.create_identifier("see")),
            comment,
        );
        JSDocSeeTag::new(node, name)
    }

    pub fn create_jsdoc_name_reference(
        &self,
        name: Gc<Node /*EntityName | JSDocMemberName*/>,
    ) -> JSDocNameReference {
        let node = self.create_base_node(SyntaxKind::JSDocNameReference);
        JSDocNameReference::new(node, name)
    }

    pub fn create_jsdoc_member_name(
        &self,
        left: Gc<Node /*EntityName | JSDocMemberName*/>,
        right: Gc<Node /*Identifier*/>,
    ) -> JSDocMemberName {
        let node = self.create_base_node(SyntaxKind::JSDocMemberName);
        let node = JSDocMemberName::new(node, left, right);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.left)) | propagate_child_flags(Some(&*node.right)),
        );
        node
    }

    pub fn create_jsdoc_link(
        &self,
        name: Option<Gc<Node /*EntityName | JSDocMemberName*/>>,
        text: String,
    ) -> JSDocLink {
        let node = self.create_base_node(SyntaxKind::JSDocLink);
        JSDocLink::new(node, name, text)
    }

    pub fn create_jsdoc_link_code(
        &self,
        name: Option<Gc<Node /*EntityName | JSDocMemberName*/>>,
        text: String,
    ) -> JSDocLinkCode {
        let node = self.create_base_node(SyntaxKind::JSDocLinkCode);
        JSDocLinkCode::new(node, name, text)
    }

    pub fn create_jsdoc_link_plain(
        &self,
        name: Option<Gc<Node /*EntityName | JSDocMemberName*/>>,
        text: String,
    ) -> JSDocLinkPlain {
        let node = self.create_base_node(SyntaxKind::JSDocLinkPlain);
        JSDocLinkPlain::new(node, name, text)
    }

    pub(crate) fn create_jsdoc_simple_tag_worker<TComment: Into<StringOrNodeArray>>(
        &self,
        kind: SyntaxKind,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        comment: Option<TComment>,
    ) -> BaseJSDocTag {
        self.create_base_jsdoc_tag(
            kind,
            tag_name.unwrap_or_else(|| self.create_identifier(get_default_tag_name_for_kind(kind))),
            comment,
        )
    }

    pub(crate) fn create_jsdoc_type_like_tag_worker<TComment: Into<StringOrNodeArray>>(
        &self,
        kind: SyntaxKind,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
        comment: Option<TComment>,
    ) -> BaseJSDocTypeLikeTag {
        let node = self.create_base_jsdoc_tag(
            kind,
            tag_name.unwrap_or_else(|| self.create_identifier(get_default_tag_name_for_kind(kind))),
            comment,
        );
        BaseJSDocTypeLikeTag::new(node, type_expression)
    }

    pub fn create_jsdoc_unknown_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        tag_name: Gc<Node /*Identifier*/>,
        comment: Option<TComment>,
    ) -> BaseJSDocTag {
        self.create_base_jsdoc_tag(SyntaxKind::JSDocTag, tag_name, comment)
    }

    pub fn create_jsdoc_text(&self, text: String) -> JSDocText {
        let node = self.create_base_node(SyntaxKind::JSDocText);
        JSDocText::new(node, text)
    }

    pub fn create_jsdoc_comment<TComment: Into<StringOrNodeArray>, TTags: Into<NodeArrayOrVec>>(
        &self,
        comment: Option<TComment>,
        tags: Option<TTags>,
    ) -> JSDoc {
        let node = self.create_base_node(SyntaxKind::JSDocComment);
        JSDoc::new(node, comment.map(Into::into), self.as_node_array(tags))
    }

    pub fn create_jsx_element(
        &self,
        opening_element: Gc<Node /*JsxOpeningElement*/>,
        children: impl Into<NodeArrayOrVec>,
        closing_element: Gc<Node /*JsxClosingElement*/>,
    ) -> JsxElement {
        let node = self.create_base_node(SyntaxKind::JsxElement);
        let node = JsxElement::new(
            node,
            opening_element,
            self.create_node_array(Some(children), None),
            closing_element,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.opening_element))
                | propagate_children_flags(Some(&node.children))
                | propagate_child_flags(Some(&*node.closing_element))
                | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_element(
        &self,
        node: &Node, /*JsxElement*/
        opening_element: Gc<Node /*JsxOpeningElement*/>,
        children: impl Into<NodeArrayOrVec>,
        closing_element: Gc<Node /*JsxClosingElement*/>,
    ) -> Gc<Node> {
        let node_as_jsx_element = node.as_jsx_element();
        let children = children.into();
        if !Gc::ptr_eq(&node_as_jsx_element.opening_element, &opening_element)
            || has_node_array_changed(&node_as_jsx_element.children, &children)
            || !Gc::ptr_eq(&node_as_jsx_element.closing_element, &closing_element)
        {
            self.update(
                self.create_jsx_element(opening_element, children, closing_element)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_self_closing_element(
        &self,
        tag_name: Gc<Node /*JsxTagNameExpression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
        attributes: Gc<Node /*JsxAttributes*/>,
    ) -> JsxSelfClosingElement {
        let node = self.create_base_node(SyntaxKind::JsxSelfClosingElement);
        let node = JsxSelfClosingElement::new(
            node,
            tag_name,
            self.as_node_array(type_arguments),
            attributes,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.tag_name))
                | propagate_children_flags(node.maybe_type_arguments().as_deref())
                | propagate_child_flags(Some(&*node.attributes))
                | TransformFlags::ContainsJsx,
        );
        if node.maybe_type_arguments().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub fn update_jsx_self_closing_element(
        &self,
        node: &Node, /*JsxSelfClosingElement*/
        tag_name: Gc<Node /*JsxTagNameExpression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
        attributes: Gc<Node /*JsxAttributes*/>,
    ) -> Gc<Node> {
        let node_as_jsx_self_closing_element = node.as_jsx_self_closing_element();
        let type_arguments = type_arguments.map(Into::into);
        if !Gc::ptr_eq(&node_as_jsx_self_closing_element.tag_name, &tag_name)
            || has_option_node_array_changed(
                node_as_jsx_self_closing_element
                    .maybe_type_arguments()
                    .as_deref(),
                type_arguments.as_ref(),
            )
            || !Gc::ptr_eq(&node_as_jsx_self_closing_element.attributes, &attributes)
        {
            self.update(
                self.create_jsx_self_closing_element(tag_name, type_arguments, attributes)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_opening_element(
        &self,
        tag_name: Gc<Node /*JsxTagNameExpression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
        attributes: Gc<Node /*JsxAttributes*/>,
    ) -> JsxOpeningElement {
        let node = self.create_base_node(SyntaxKind::JsxOpeningElement);
        let node = JsxOpeningElement::new(
            node,
            tag_name,
            self.as_node_array(type_arguments),
            attributes,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.tag_name))
                | propagate_children_flags(node.maybe_type_arguments().as_deref())
                | propagate_child_flags(Some(&*node.attributes))
                | TransformFlags::ContainsJsx,
        );
        if node.maybe_type_arguments().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub fn update_jsx_opening_element(
        &self,
        node: &Node, /*JsxOpeningElement*/
        tag_name: Gc<Node /*JsxTagNameExpression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
        attributes: Gc<Node /*JsxAttributes*/>,
    ) -> Gc<Node> {
        let node_as_jsx_opening_element = node.as_jsx_opening_element();
        let type_arguments = type_arguments.map(Into::into);
        if !Gc::ptr_eq(&node_as_jsx_opening_element.tag_name, &tag_name)
            || has_option_node_array_changed(
                node_as_jsx_opening_element
                    .maybe_type_arguments()
                    .as_deref(),
                type_arguments.as_ref(),
            )
            || !Gc::ptr_eq(&node_as_jsx_opening_element.attributes, &attributes)
        {
            self.update(
                self.create_jsx_opening_element(tag_name, type_arguments, attributes)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_closing_element(
        &self,
        tag_name: Gc<Node /*JsxTagNameExpression*/>,
    ) -> JsxClosingElement {
        let node = self.create_base_node(SyntaxKind::JsxClosingElement);
        let node = JsxClosingElement::new(node, tag_name);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.tag_name)) | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_closing_element(
        &self,
        node: &Node, /*JsxClosingElement*/
        tag_name: Gc<Node /*JsxTagNameExpression*/>,
    ) -> Gc<Node> {
        let node_as_jsx_closing_element = node.as_jsx_closing_element();
        if !Gc::ptr_eq(&node_as_jsx_closing_element.tag_name, &tag_name) {
            self.update(self.create_jsx_closing_element(tag_name).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_fragment(
        &self,
        opening_fragment: Gc<Node /*JsxOpeningFragment*/>,
        children: impl Into<NodeArrayOrVec>,
        closing_fragment: Gc<Node /*JsxClosingFragment*/>,
    ) -> JsxFragment {
        let node = self.create_base_node(SyntaxKind::JsxFragment);
        let node = JsxFragment::new(
            node,
            opening_fragment,
            self.create_node_array(Some(children), None),
            closing_fragment,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.opening_fragment))
                | propagate_children_flags(Some(&node.children))
                | propagate_child_flags(Some(&*node.closing_fragment))
                | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_fragment(
        &self,
        node: &Node, /*JsxFragment*/
        opening_fragment: Gc<Node /*JsxOpeningFragment*/>,
        children: impl Into<NodeArrayOrVec>,
        closing_fragment: Gc<Node /*JsxClosingFragment*/>,
    ) -> Gc<Node> {
        let node_as_jsx_fragment = node.as_jsx_fragment();
        let children = children.into();
        if !Gc::ptr_eq(&node_as_jsx_fragment.opening_fragment, &opening_fragment)
            || has_node_array_changed(&node_as_jsx_fragment.children, &children)
            || !Gc::ptr_eq(&node_as_jsx_fragment.closing_fragment, &closing_fragment)
        {
            self.update(
                self.create_jsx_fragment(opening_fragment, children, closing_fragment)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_text(
        &self,
        text: String,
        contains_only_trivia_white_spaces: Option<bool>,
    ) -> JsxText {
        let node = self.create_base_node(SyntaxKind::JsxText);
        let node = JsxText::new(
            node,
            text,
            contains_only_trivia_white_spaces.unwrap_or(false),
        );
        node.add_transform_flags(TransformFlags::ContainsJsx);
        node
    }

    pub fn update_jsx_text(
        &self,
        node: &Node, /*JsxText*/
        text: String,
        contains_only_trivia_white_spaces: Option<bool>,
    ) -> Gc<Node> {
        let node_as_jsx_text = node.as_jsx_text();
        if &*node_as_jsx_text.text() != &text
            || Some(node_as_jsx_text.contains_only_trivia_white_spaces)
                != contains_only_trivia_white_spaces
        {
            self.update(
                self.create_jsx_text(text, contains_only_trivia_white_spaces)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_opening_fragment(&self) -> JsxOpeningFragment {
        let node = self.create_base_node(SyntaxKind::JsxOpeningFragment);
        let node = JsxOpeningFragment::new(node);
        node.add_transform_flags(TransformFlags::ContainsJsx);
        node
    }

    pub fn create_jsx_jsx_closing_fragment(&self) -> JsxClosingFragment {
        let node = self.create_base_node(SyntaxKind::JsxClosingFragment);
        let node = JsxClosingFragment::new(node);
        node.add_transform_flags(TransformFlags::ContainsJsx);
        node
    }

    pub fn create_jsx_attribute(
        &self,
        name: Gc<Node /*Identifier*/>,
        initializer: Option<Gc<Node /*StringLiteral | JsxExpression*/>>,
    ) -> JsxAttribute {
        let node = self.create_base_node(SyntaxKind::JsxAttribute);
        let node = JsxAttribute::new(node, name, initializer);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.name))
                | propagate_child_flags(node.initializer.clone())
                | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_attribute(
        &self,
        node: &Node, /*JsxAttribute*/
        name: Gc<Node /*Identifier*/>,
        initializer: Option<Gc<Node /*StringLiteral | JsxExpression*/>>,
    ) -> Gc<Node> {
        let node_as_jsx_attribute = node.as_jsx_attribute();
        if !Gc::ptr_eq(&node_as_jsx_attribute.name, &name)
            || !are_option_gcs_equal(
                node_as_jsx_attribute.initializer.as_ref(),
                initializer.as_ref(),
            )
        {
            self.update(self.create_jsx_attribute(name, initializer).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_attributes(&self, properties: impl Into<NodeArrayOrVec>) -> JsxAttributes {
        let node = self.create_base_node(SyntaxKind::JsxAttributes);
        let node = JsxAttributes::new(node, self.create_node_array(Some(properties), None));
        node.add_transform_flags(
            propagate_children_flags(Some(&node.properties)) | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_attributes(
        &self,
        node: &Node, /*JsxAttributes*/
        properties: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_jsx_attributes = node.as_jsx_attributes();
        let properties = properties.into();
        if has_node_array_changed(&node_as_jsx_attributes.properties, &properties) {
            self.update(self.create_jsx_attributes(properties).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_spread_attribute(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> JsxSpreadAttribute {
        let node = self.create_base_node(SyntaxKind::JsxSpreadAttribute);
        let node = JsxSpreadAttribute::new(node, expression);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression)) | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_spread_attribute(
        &self,
        node: &Node, /*JsxSpreadAttribute*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_jsx_spread_attribute = node.as_jsx_spread_attribute();
        if !Gc::ptr_eq(&node_as_jsx_spread_attribute.expression, &expression) {
            self.update(self.create_jsx_spread_attribute(expression).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_expression(
        &self,
        dot_dot_dot_token: Option<Gc<Node /*DotDotDotToken*/>>,
        expression: Option<Gc<Node /*Expression*/>>,
    ) -> JsxExpression {
        let node = self.create_base_node(SyntaxKind::JsxExpression);
        let node = JsxExpression::new(node, dot_dot_dot_token, expression);
        node.add_transform_flags(
            propagate_child_flags(node.dot_dot_dot_token.clone())
                | propagate_child_flags(node.expression.clone())
                | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_expression(
        &self,
        node: &Node, /*JsxExpression*/
        expression: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> {
        let node_as_jsx_expression = node.as_jsx_expression();
        if !are_option_gcs_equal(
            node_as_jsx_expression.expression.as_ref(),
            expression.as_ref(),
        ) {
            self.update(
                self.create_jsx_expression(
                    node_as_jsx_expression.dot_dot_dot_token.clone(),
                    expression,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_case_clause(
        &self,
        expression: Gc<Node /*Expression*/>,
        statements: impl Into<NodeArrayOrVec>,
    ) -> CaseClause {
        let node = self.create_base_node(SyntaxKind::CaseClause);
        let node = CaseClause::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_for_disallowed_comma(&expression),
            self.create_node_array(Some(statements), None),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression))
                | propagate_children_flags(Some(&node.statements)),
        );
        node
    }

    pub fn update_case_clause(
        &self,
        node: &Node, /*CaseClause*/
        expression: Gc<Node /*Expression*/>,
        statements: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_case_clause = node.as_case_clause();
        let statements = statements.into();
        if !Gc::ptr_eq(&node_as_case_clause.expression, &expression)
            || has_node_array_changed(&node_as_case_clause.statements, &statements)
        {
            self.update(self.create_case_clause(expression, statements).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_default_clause(&self, statements: impl Into<NodeArrayOrVec>) -> DefaultClause {
        let node = self.create_base_node(SyntaxKind::DefaultClause);
        let node = DefaultClause::new(node, self.create_node_array(Some(statements), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.statements)));
        node
    }

    pub fn update_default_clause(
        &self,
        node: &Node, /*DefaultClause*/
        statements: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_default_clause = node.as_default_clause();
        let statements = statements.into();
        if has_node_array_changed(&node_as_default_clause.statements, &statements) {
            self.update(self.create_default_clause(statements).wrap(), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_heritage_clause(
        &self,
        token: SyntaxKind,                /*HeritageClause["token"]*/
        types: impl Into<NodeArrayOrVec>, /*<ExpressionWithTypeArguments>*/
    ) -> HeritageClause {
        let node = self.create_base_node(SyntaxKind::HeritageClause);
        let node = HeritageClause::new(node, token, self.create_node_array(Some(types), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.types)));
        match token {
            SyntaxKind::ExtendsKeyword => {
                node.add_transform_flags(TransformFlags::ContainsES2015);
            }
            SyntaxKind::ImplementsKeyword => {
                node.add_transform_flags(TransformFlags::ContainsTypeScript);
            }
            _ => Debug_.assert_never(token, None),
        }
        node
    }

    pub fn update_heritage_clause(
        &self,
        node: &Node,                      /*HeritageClause*/
        types: impl Into<NodeArrayOrVec>, /*<ExpressionWithTypeArguments>*/
    ) -> Gc<Node> {
        let node_as_heritage_clause = node.as_heritage_clause();
        let types = types.into();
        if has_node_array_changed(&node_as_heritage_clause.types, &types) {
            self.update(
                self.create_heritage_clause(node_as_heritage_clause.token, types)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_catch_clause<'variable_declaration>(
        &self,
        variable_declaration: Option<
            impl Into<StrOrRcNode<'variable_declaration>>,
            /*BindingName | VariableDeclaration*/
        >,
        block: Gc<Node /*Block*/>,
    ) -> CatchClause {
        let node = self.create_base_node(SyntaxKind::CatchClause);
        let variable_declaration: Option<Gc<Node>> =
            variable_declaration.map(|variable_declaration| {
                let variable_declaration = variable_declaration.into();
                match variable_declaration {
                    StrOrRcNode::Str(variable_declaration) => self
                        .create_variable_declaration(Some(variable_declaration), None, None, None)
                        .wrap(),
                    StrOrRcNode::RcNode(variable_declaration)
                        if !is_variable_declaration(&variable_declaration) =>
                    {
                        self.create_variable_declaration(
                            Some(variable_declaration),
                            None,
                            None,
                            None,
                        )
                        .wrap()
                    }
                    StrOrRcNode::RcNode(variable_declaration) => variable_declaration,
                }
            });
        let variable_declaration_is_some = variable_declaration.is_some();
        let node = CatchClause::new(node, variable_declaration, block);
        node.add_transform_flags(
            propagate_child_flags(node.variable_declaration.clone())
                | propagate_child_flags(Some(&*node.block)),
        );
        if !variable_declaration_is_some {
            node.add_transform_flags(TransformFlags::ContainsES2019);
        }
        node
    }

    pub fn update_catch_clause(
        &self,
        node: &Node, /*CatchClause*/
        variable_declaration: Option<Gc<Node> /*VariableDeclaration*/>,
        block: Gc<Node /*Block*/>,
    ) -> Gc<Node> {
        let node_as_catch_clause = node.as_catch_clause();
        if !are_option_gcs_equal(
            node_as_catch_clause.variable_declaration.as_ref(),
            variable_declaration.as_ref(),
        ) || !Gc::ptr_eq(&node_as_catch_clause.block, &block)
        {
            self.update(
                self.create_catch_clause(variable_declaration, block).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }
}
