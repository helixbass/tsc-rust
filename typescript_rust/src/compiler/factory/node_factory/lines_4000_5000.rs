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
        base_factory: &TBaseNodeFactory,
        elements: Gc<NodeArray>, /*<AssertEntry>*/
        multi_line: Option<bool>,
    ) -> AssertClause {
        let node = self.create_base_node(base_factory, SyntaxKind::AssertClause);
        let mut node = AssertClause::new(node, elements, multi_line);
        node.add_transform_flags(TransformFlags::ContainsESNext);
        node
    }

    pub fn update_assert_clause(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node,             /*AssertClause*/
        elements: Gc<NodeArray>, /*<AssertEntry>*/
        multi_line: Option<bool>,
    ) -> Gc<Node> {
        let node_as_assert_clause = node.as_assert_clause();
        if !Gc::ptr_eq(&node_as_assert_clause.elements, &elements)
            || node_as_assert_clause.multi_line != multi_line
        {
            self.update(
                self.create_assert_clause(base_factory, elements, multi_line)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_assert_entry(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Gc<Node /*AssertionKey*/>,
        value: Gc<Node /*StringLiteral*/>,
    ) -> AssertEntry {
        let node = self.create_base_node(base_factory, SyntaxKind::AssertEntry);
        let mut node = AssertEntry::new(node, name, value);
        node.add_transform_flags(TransformFlags::ContainsESNext);
        node
    }

    pub fn update_assert_entry(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*AssertEntry*/
        name: Gc<Node /*AssertionKey*/>,
        value: Gc<Node /*StringLiteral*/>,
    ) -> Gc<Node> {
        let node_as_assert_entry = node.as_assert_entry();
        if !Gc::ptr_eq(&node_as_assert_entry.name, &name)
            || !Gc::ptr_eq(&node_as_assert_entry.value, &value)
        {
            self.update(
                self.create_assert_entry(base_factory, name, value).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_namespace_import(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Gc<Node /*Identifier*/>,
    ) -> NamespaceImport {
        let node = self.create_base_node(base_factory, SyntaxKind::NamespaceImport);
        let mut node = NamespaceImport::new(node, name);
        node.add_transform_flags(propagate_child_flags(Some(&*node.name)));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_namespace_import(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*NamespaceImport*/
        name: Gc<Node /*Identifier*/>,
    ) -> Gc<Node> {
        let node_as_namespace_import = node.as_namespace_import();
        if !Gc::ptr_eq(&node_as_namespace_import.name, &name) {
            self.update(
                self.create_namespace_import(base_factory, name).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_namespace_export(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Gc<Node /*Identifier*/>,
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

    pub fn update_namespace_export(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*NamespaceExport*/
        name: Gc<Node /*Identifier*/>,
    ) -> Gc<Node> {
        let node_as_namespace_export = node.as_namespace_export();
        if !Gc::ptr_eq(&node_as_namespace_export.name, &name) {
            self.update(
                self.create_namespace_export(base_factory, name).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_named_imports(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: impl Into<NodeArrayOrVec>,
    ) -> NamedImports {
        let node = self.create_base_node(base_factory, SyntaxKind::NamedImports);
        let mut node = NamedImports::new(node, self.create_node_array(Some(elements), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.elements)));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_named_imports(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*NamedImports*/
        elements: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_named_imports = node.as_named_imports();
        let elements = elements.into();
        if has_node_array_changed(&node_as_named_imports.elements, &elements) {
            self.update(
                self.create_named_imports(base_factory, elements).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_import_specifier(
        &self,
        base_factory: &TBaseNodeFactory,
        is_type_only: bool,
        property_name: Option<Gc<Node /*Identifier*/>>,
        name: Gc<Node /*Identifier*/>,
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

    pub fn update_import_specifier(
        &self,
        base_factory: &TBaseNodeFactory,
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
                self.create_import_specifier(base_factory, is_type_only, property_name, name)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_export_assignment(
        &self,
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        is_export_equals: Option<bool>,
        expression: Gc<Node /*Expression*/>,
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

    pub fn update_export_assignment(
        &self,
        base_factory: &TBaseNodeFactory,
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
                    base_factory,
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
        base_factory: &TBaseNodeFactory,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        is_type_only: bool,
        export_clause: Option<Gc<Node /*NamedExportBindings*/>>,
        module_specifier: Option<Gc<Node /*Expression*/>>,
        assert_clause: Option<Gc<Node /*AssertClause*/>>,
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

    pub fn update_export_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
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
                    base_factory,
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

    pub fn create_named_exports(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: impl Into<NodeArrayOrVec>,
    ) -> NamedExports {
        let node = self.create_base_node(base_factory, SyntaxKind::NamedExports);
        let mut node = NamedExports::new(node, self.create_node_array(Some(elements), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.elements)));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_named_exports(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*NamedExports*/
        elements: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_named_exports = node.as_named_exports();
        let elements = elements.into();
        if has_node_array_changed(&node_as_named_exports.elements, &elements) {
            self.update(
                self.create_named_exports(base_factory, elements).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_export_specifier<'property_name, 'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        is_type_only: bool,
        property_name: Option<
            impl Into<StrOrRcNode<'property_name>>,
            /*Identifier*/
        >,
        name: impl Into<StrOrRcNode<'name>>,
        /*Identifier*/
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

    pub fn update_export_specifier(
        &self,
        base_factory: &TBaseNodeFactory,
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
                self.create_export_specifier(base_factory, is_type_only, property_name, name)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_missing_declaration(
        &self,
        base_factory: &TBaseNodeFactory,
    ) -> MissingDeclaration {
        let node = self.create_base_declaration(
            base_factory,
            SyntaxKind::MissingDeclaration,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
        );
        MissingDeclaration::new(node)
    }

    pub fn create_external_module_reference(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
    ) -> ExternalModuleReference {
        let node = self.create_base_node(base_factory, SyntaxKind::ExternalModuleReference);
        let mut node = ExternalModuleReference::new(node, expression);
        node.add_transform_flags(propagate_child_flags(Some(&*node.expression)));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_external_module_reference(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*ExternalModuleReference*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_external_module_reference = node.as_external_module_reference();
        if !Gc::ptr_eq(&node_as_external_module_reference.expression, &expression) {
            self.update(
                self.create_external_module_reference(base_factory, expression)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
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
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        let node = self.create_base_node(base_factory, kind);
        let node = BaseJSDocUnaryType::new(node, type_);
        node
    }

    pub fn create_jsdoc_function_type<TParameters: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        parameters: TParameters,
        type_: Option<Gc<Node /*TypeNode*/>>,
    ) -> JSDocFunctionType {
        let node = self.create_base_signature_declaration(
            base_factory,
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
        type_: Gc<Node /*TypeNode*/>,
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
        type_: Option<Gc<Node /*JSDocReturnTag*/>>,
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
    ) -> Gc<Node /*Identifier*/> {
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
                Option::<Gc<NodeArray>>::None,
                None,
            )
            .wrap()
        }
    }

    pub(crate) fn create_base_jsdoc_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        tag_name: Gc<Node /*Identifier*/>,
        comment: Option<TComment>,
    ) -> BaseJSDocTag {
        let node = self.create_base_node(base_factory, kind);
        BaseJSDocTag::new(node, tag_name, comment.map(Into::into))
    }

    pub fn create_jsdoc_template_tag<
        TTypeParameters: Into<NodeArrayOrVec>,
        TComment: Into<StringOrNodeArray>,
    >(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        constraint: Option<Gc<Node /*JSDocTypeExpression*/>>,
        type_parameters: TTypeParameters, /*<TypeParameterDeclaration>*/
        comment: Option<TComment>,
    ) -> JSDocTemplateTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            SyntaxKind::JSDocTemplateTag,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(
                    base_factory,
                    "template",
                    Option::<Gc<NodeArray>>::None,
                    None,
                )
                .wrap()
            }),
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
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
        full_name: Option<Gc<Node /*Identifier | JSDocNamespaceDeclaration*/>>,
        comment: Option<TComment>,
    ) -> JSDocTypedefTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            SyntaxKind::JSDocTypedefTag,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(base_factory, "typedef", Option::<Gc<NodeArray>>::None, None)
                    .wrap()
            }),
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
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        name: Gc<Node /*EntityName*/>,
        is_bracketed: bool,
        type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
        is_name_first: Option<bool>,
        comment: Option<TComment>,
    ) -> JSDocPropertyLikeTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            SyntaxKind::JSDocParameterTag,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(base_factory, "param", Option::<Gc<NodeArray>>::None, None)
                    .wrap()
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

    pub fn create_jsdoc_property_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        name: Gc<Node /*EntityName*/>,
        is_bracketed: bool,
        type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
        is_name_first: Option<bool>,
        comment: Option<TComment>,
    ) -> JSDocPropertyLikeTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            SyntaxKind::JSDocPropertyTag,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(base_factory, "prop", Option::<Gc<NodeArray>>::None, None)
                    .wrap()
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

    pub fn create_jsdoc_callback_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        type_expression: Gc<Node /*JSDocSignature*/>,
        full_name: Option<Gc<Node /*Identifier | JSDocNamespaceDeclaration*/>>,
        comment: Option<TComment>,
    ) -> JSDocCallbackTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            SyntaxKind::JSDocCallbackTag,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(
                    base_factory,
                    "callback",
                    Option::<Gc<NodeArray>>::None,
                    None,
                )
                .wrap()
            }),
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
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        class_name: Gc<Node /*JSDocAugmentsTag["class"]*/>,
        comment: Option<TComment>,
    ) -> JSDocAugmentsTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            SyntaxKind::JSDocAugmentsTag,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(
                    base_factory,
                    "augments",
                    Option::<Gc<NodeArray>>::None,
                    None,
                )
                .wrap()
            }),
            comment,
        );
        JSDocAugmentsTag::new(node, class_name)
    }

    pub fn create_jsdoc_implements_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        class_name: Gc<Node /*JSDocImplementsTag["class"]*/>,
        comment: Option<TComment>,
    ) -> JSDocImplementsTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            SyntaxKind::JSDocImplementsTag,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(
                    base_factory,
                    "implements",
                    Option::<Gc<NodeArray>>::None,
                    None,
                )
                .wrap()
            }),
            comment,
        );
        JSDocImplementsTag::new(node, class_name)
    }

    pub fn create_jsdoc_see_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        name: Option<Gc<Node /*JSDocNameReference*/>>,
        comment: Option<TComment>,
    ) -> JSDocSeeTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            SyntaxKind::JSDocSeeTag,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(base_factory, "see", Option::<Gc<NodeArray>>::None, None)
                    .wrap()
            }),
            comment,
        );
        JSDocSeeTag::new(node, name)
    }

    pub fn create_jsdoc_name_reference(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Gc<Node /*EntityName | JSDocMemberName*/>,
    ) -> JSDocNameReference {
        let node = self.create_base_node(base_factory, SyntaxKind::JSDocNameReference);
        JSDocNameReference::new(node, name)
    }

    pub fn create_jsdoc_member_name(
        &self,
        base_factory: &TBaseNodeFactory,
        left: Gc<Node /*EntityName | JSDocMemberName*/>,
        right: Gc<Node /*Identifier*/>,
    ) -> JSDocMemberName {
        let node = self.create_base_node(base_factory, SyntaxKind::JSDocMemberName);
        let mut node = JSDocMemberName::new(node, left, right);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.left)) | propagate_child_flags(Some(&*node.right)),
        );
        node
    }

    pub fn create_jsdoc_link(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Option<Gc<Node /*EntityName | JSDocMemberName*/>>,
        text: String,
    ) -> JSDocLink {
        let node = self.create_base_node(base_factory, SyntaxKind::JSDocLink);
        JSDocLink::new(node, name, text)
    }

    pub fn create_jsdoc_link_code(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Option<Gc<Node /*EntityName | JSDocMemberName*/>>,
        text: String,
    ) -> JSDocLinkCode {
        let node = self.create_base_node(base_factory, SyntaxKind::JSDocLinkCode);
        JSDocLinkCode::new(node, name, text)
    }

    pub fn create_jsdoc_link_plain(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Option<Gc<Node /*EntityName | JSDocMemberName*/>>,
        text: String,
    ) -> JSDocLinkPlain {
        let node = self.create_base_node(base_factory, SyntaxKind::JSDocLinkPlain);
        JSDocLinkPlain::new(node, name, text)
    }

    pub(crate) fn create_jsdoc_simple_tag_worker<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        comment: Option<TComment>,
    ) -> BaseJSDocTag {
        self.create_base_jsdoc_tag(
            base_factory,
            kind,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(
                    base_factory,
                    get_default_tag_name_for_kind(kind),
                    Option::<Gc<NodeArray>>::None,
                    None,
                )
                .wrap()
            }),
            comment,
        )
    }

    pub(crate) fn create_jsdoc_type_like_tag_worker<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        tag_name: Option<Gc<Node /*Identifier*/>>,
        type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
        comment: Option<TComment>,
    ) -> BaseJSDocTypeLikeTag {
        let node = self.create_base_jsdoc_tag(
            base_factory,
            kind,
            tag_name.unwrap_or_else(|| {
                self.create_identifier(
                    base_factory,
                    get_default_tag_name_for_kind(kind),
                    Option::<Gc<NodeArray>>::None,
                    None,
                )
                .wrap()
            }),
            comment,
        );
        BaseJSDocTypeLikeTag::new(node, type_expression)
    }

    pub fn create_jsdoc_unknown_tag<TComment: Into<StringOrNodeArray>>(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Gc<Node /*Identifier*/>,
        comment: Option<TComment>,
    ) -> BaseJSDocTag {
        self.create_base_jsdoc_tag(base_factory, SyntaxKind::JSDocTag, tag_name, comment)
    }

    pub fn create_jsdoc_text(&self, base_factory: &TBaseNodeFactory, text: String) -> JSDocText {
        let node = self.create_base_node(base_factory, SyntaxKind::JSDocText);
        JSDocText::new(node, text)
    }

    pub fn create_jsdoc_comment<TComment: Into<StringOrNodeArray>, TTags: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        comment: Option<TComment>,
        tags: Option<TTags>,
    ) -> JSDoc {
        let node = self.create_base_node(base_factory, SyntaxKind::JSDocComment);
        JSDoc::new(node, comment.map(Into::into), self.as_node_array(tags))
    }

    pub fn create_jsx_element(
        &self,
        base_factory: &TBaseNodeFactory,
        opening_element: Gc<Node /*JsxOpeningElement*/>,
        children: impl Into<NodeArrayOrVec>,
        closing_element: Gc<Node /*JsxClosingElement*/>,
    ) -> JsxElement {
        let node = self.create_base_node(base_factory, SyntaxKind::JsxElement);
        let mut node = JsxElement::new(
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
        base_factory: &TBaseNodeFactory,
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
                self.create_jsx_element(base_factory, opening_element, children, closing_element)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_self_closing_element(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Gc<Node /*JsxTagNameExpression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
        attributes: Gc<Node /*JsxAttributes*/>,
    ) -> JsxSelfClosingElement {
        let node = self.create_base_node(base_factory, SyntaxKind::JsxSelfClosingElement);
        let mut node = JsxSelfClosingElement::new(
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
        base_factory: &TBaseNodeFactory,
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
                self.create_jsx_self_closing_element(
                    base_factory,
                    tag_name,
                    type_arguments,
                    attributes,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_opening_element(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Gc<Node /*JsxTagNameExpression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
        attributes: Gc<Node /*JsxAttributes*/>,
    ) -> JsxOpeningElement {
        let node = self.create_base_node(base_factory, SyntaxKind::JsxOpeningElement);
        let mut node = JsxOpeningElement::new(
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
        base_factory: &TBaseNodeFactory,
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
                self.create_jsx_opening_element(base_factory, tag_name, type_arguments, attributes)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_closing_element(
        &self,
        base_factory: &TBaseNodeFactory,
        tag_name: Gc<Node /*JsxTagNameExpression*/>,
    ) -> JsxClosingElement {
        let node = self.create_base_node(base_factory, SyntaxKind::JsxClosingElement);
        let mut node = JsxClosingElement::new(node, tag_name);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.tag_name)) | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_closing_element(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*JsxClosingElement*/
        tag_name: Gc<Node /*JsxTagNameExpression*/>,
    ) -> Gc<Node> {
        let node_as_jsx_closing_element = node.as_jsx_closing_element();
        if !Gc::ptr_eq(&node_as_jsx_closing_element.tag_name, &tag_name) {
            self.update(
                self.create_jsx_closing_element(base_factory, tag_name)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_fragment(
        &self,
        base_factory: &TBaseNodeFactory,
        opening_fragment: Gc<Node /*JsxOpeningFragment*/>,
        children: impl Into<NodeArrayOrVec>,
        closing_fragment: Gc<Node /*JsxClosingFragment*/>,
    ) -> JsxFragment {
        let node = self.create_base_node(base_factory, SyntaxKind::JsxFragment);
        let mut node = JsxFragment::new(
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
        base_factory: &TBaseNodeFactory,
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
                self.create_jsx_fragment(
                    base_factory,
                    opening_fragment,
                    children,
                    closing_fragment,
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
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

    pub fn update_jsx_text(
        &self,
        base_factory: &TBaseNodeFactory,
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
                self.create_jsx_text(base_factory, text, contains_only_trivia_white_spaces)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_opening_fragment(
        &self,
        base_factory: &TBaseNodeFactory,
    ) -> JsxOpeningFragment {
        let node = self.create_base_node(base_factory, SyntaxKind::JsxOpeningFragment);
        let mut node = JsxOpeningFragment::new(node);
        node.add_transform_flags(TransformFlags::ContainsJsx);
        node
    }

    pub fn create_jsx_jsx_closing_fragment(
        &self,
        base_factory: &TBaseNodeFactory,
    ) -> JsxClosingFragment {
        let node = self.create_base_node(base_factory, SyntaxKind::JsxClosingFragment);
        let mut node = JsxClosingFragment::new(node);
        node.add_transform_flags(TransformFlags::ContainsJsx);
        node
    }

    pub fn create_jsx_attribute(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Gc<Node /*Identifier*/>,
        initializer: Option<Gc<Node /*StringLiteral | JsxExpression*/>>,
    ) -> JsxAttribute {
        let node = self.create_base_node(base_factory, SyntaxKind::JsxAttribute);
        let mut node = JsxAttribute::new(node, name, initializer);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.name))
                | propagate_child_flags(node.initializer.clone())
                | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_attribute(
        &self,
        base_factory: &TBaseNodeFactory,
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
            self.update(
                self.create_jsx_attribute(base_factory, name, initializer)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_attributes(
        &self,
        base_factory: &TBaseNodeFactory,
        properties: impl Into<NodeArrayOrVec>,
    ) -> JsxAttributes {
        let node = self.create_base_node(base_factory, SyntaxKind::JsxAttributes);
        let mut node = JsxAttributes::new(node, self.create_node_array(Some(properties), None));
        node.add_transform_flags(
            propagate_children_flags(Some(&node.properties)) | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_attributes(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*JsxAttributes*/
        properties: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_jsx_attributes = node.as_jsx_attributes();
        let properties = properties.into();
        if has_node_array_changed(&node_as_jsx_attributes.properties, &properties) {
            self.update(
                self.create_jsx_attributes(base_factory, properties).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_spread_attribute(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
    ) -> JsxSpreadAttribute {
        let node = self.create_base_node(base_factory, SyntaxKind::JsxSpreadAttribute);
        let mut node = JsxSpreadAttribute::new(node, expression);
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.expression)) | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_spread_attribute(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*JsxSpreadAttribute*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_jsx_spread_attribute = node.as_jsx_spread_attribute();
        if !Gc::ptr_eq(&node_as_jsx_spread_attribute.expression, &expression) {
            self.update(
                self.create_jsx_spread_attribute(base_factory, expression)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_jsx_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        dot_dot_dot_token: Option<Gc<Node /*DotDotDotToken*/>>,
        expression: Option<Gc<Node /*Expression*/>>,
    ) -> JsxExpression {
        let node = self.create_base_node(base_factory, SyntaxKind::JsxExpression);
        let mut node = JsxExpression::new(node, dot_dot_dot_token, expression);
        node.add_transform_flags(
            propagate_child_flags(node.dot_dot_dot_token.clone())
                | propagate_child_flags(node.expression.clone())
                | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_expression(
        &self,
        base_factory: &TBaseNodeFactory,
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
                    base_factory,
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
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
        statements: impl Into<NodeArrayOrVec>,
    ) -> CaseClause {
        let node = self.create_base_node(base_factory, SyntaxKind::CaseClause);
        let mut node = CaseClause::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_for_disallowed_comma(base_factory, &expression),
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
        base_factory: &TBaseNodeFactory,
        node: &Node, /*CaseClause*/
        expression: Gc<Node /*Expression*/>,
        statements: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_case_clause = node.as_case_clause();
        let statements = statements.into();
        if !Gc::ptr_eq(&node_as_case_clause.expression, &expression)
            || has_node_array_changed(&node_as_case_clause.statements, &statements)
        {
            self.update(
                self.create_case_clause(base_factory, expression, statements)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_default_clause(
        &self,
        base_factory: &TBaseNodeFactory,
        statements: impl Into<NodeArrayOrVec>,
    ) -> DefaultClause {
        let node = self.create_base_node(base_factory, SyntaxKind::DefaultClause);
        let mut node = DefaultClause::new(node, self.create_node_array(Some(statements), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.statements)));
        node
    }

    pub fn update_default_clause(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*DefaultClause*/
        statements: impl Into<NodeArrayOrVec>,
    ) -> Gc<Node> {
        let node_as_default_clause = node.as_default_clause();
        let statements = statements.into();
        if has_node_array_changed(&node_as_default_clause.statements, &statements) {
            self.update(
                self.create_default_clause(base_factory, statements).wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_heritage_clause(
        &self,
        base_factory: &TBaseNodeFactory,
        token: SyntaxKind,                /*HeritageClause["token"]*/
        types: impl Into<NodeArrayOrVec>, /*<ExpressionWithTypeArguments>*/
    ) -> HeritageClause {
        let node = self.create_base_node(base_factory, SyntaxKind::HeritageClause);
        let mut node = HeritageClause::new(node, token, self.create_node_array(Some(types), None));
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
        base_factory: &TBaseNodeFactory,
        node: &Node,                      /*HeritageClause*/
        types: impl Into<NodeArrayOrVec>, /*<ExpressionWithTypeArguments>*/
    ) -> Gc<Node> {
        let node_as_heritage_clause = node.as_heritage_clause();
        let types = types.into();
        if has_node_array_changed(&node_as_heritage_clause.types, &types) {
            self.update(
                self.create_heritage_clause(base_factory, node_as_heritage_clause.token, types)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_catch_clause<'variable_declaration>(
        &self,
        base_factory: &TBaseNodeFactory,
        variable_declaration: Option<
            impl Into<StrOrRcNode<'variable_declaration>>,
            /*BindingName | VariableDeclaration*/
        >,
        block: Gc<Node /*Block*/>,
    ) -> CatchClause {
        let node = self.create_base_node(base_factory, SyntaxKind::CatchClause);
        let variable_declaration: Option<Gc<Node>> =
            variable_declaration.map(|variable_declaration| {
                let variable_declaration = variable_declaration.into();
                match variable_declaration {
                    StrOrRcNode::Str(variable_declaration) => self
                        .create_variable_declaration(
                            base_factory,
                            Some(variable_declaration),
                            None,
                            None,
                            None,
                        )
                        .wrap(),
                    StrOrRcNode::RcNode(variable_declaration)
                        if !is_variable_declaration(&variable_declaration) =>
                    {
                        self.create_variable_declaration(
                            base_factory,
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
        let mut node = CatchClause::new(node, variable_declaration, block);
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
        base_factory: &TBaseNodeFactory,
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
                self.create_catch_clause(base_factory, variable_declaration, block)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }
}
