use gc::{Finalize, Gc, Trace};
use id_arena::Id;
use local_macros::generate_node_factory_method_wrapper;

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
    InArena,
};

impl NodeFactory {
    #[generate_node_factory_method_wrapper]
    pub fn create_assert_clause_raw(
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
        node: Id<Node>,          /*AssertClause*/
        elements: Gc<NodeArray>, /*<AssertEntry>*/
        multi_line: Option<bool>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_assert_clause = node_ref.as_assert_clause();
        if !Gc::ptr_eq(&node_as_assert_clause.elements, &elements)
            || node_as_assert_clause.multi_line != multi_line
        {
            self.update(self.create_assert_clause(elements, multi_line), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_assert_entry_raw(
        &self,
        name: Id<Node /*AssertionKey*/>,
        value: Id<Node /*StringLiteral*/>,
    ) -> AssertEntry {
        let node = self.create_base_node(SyntaxKind::AssertEntry);
        let node = AssertEntry::new(node, name, value);
        node.add_transform_flags(TransformFlags::ContainsESNext);
        node
    }

    pub fn update_assert_entry(
        &self,
        node: Id<Node>, /*AssertEntry*/
        name: Id<Node /*AssertionKey*/>,
        value: Id<Node /*StringLiteral*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_assert_entry = node_ref.as_assert_entry();
        if node_as_assert_entry.name != name
            || node_as_assert_entry.value != value
        {
            self.update(self.create_assert_entry(name, value), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_namespace_import_raw(&self, name: Id<Node /*Identifier*/>) -> NamespaceImport {
        let node = self.create_base_node(SyntaxKind::NamespaceImport);
        let node = NamespaceImport::new(node, name);
        node.add_transform_flags(propagate_child_flags(Some(node.name), self));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_namespace_import(
        &self,
        node: Id<Node>, /*NamespaceImport*/
        name: Id<Node /*Identifier*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_namespace_import = node_ref.as_namespace_import();
        if node_as_namespace_import.name != name {
            self.update(self.create_namespace_import(name), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_namespace_export_raw(&self, name: Id<Node /*Identifier*/>) -> NamespaceExport {
        let node = self.create_base_node(SyntaxKind::NamespaceExport);
        let node = NamespaceExport::new(node, name);
        node.add_transform_flags(
            propagate_child_flags(Some(node.name), self) | TransformFlags::ContainsESNext,
        );
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_namespace_export(
        &self,
        node: Id<Node>, /*NamespaceExport*/
        name: Id<Node /*Identifier*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_namespace_export = node_ref.as_namespace_export();
        if node_as_namespace_export.name != name {
            self.update(self.create_namespace_export(name), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_named_imports_raw(&self, elements: impl Into<NodeArrayOrVec>) -> NamedImports {
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
        node: Id<Node>, /*NamedImports*/
        elements: impl Into<NodeArrayOrVec>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_named_imports = node_ref.as_named_imports();
        let elements = elements.into();
        if has_node_array_changed(&node_as_named_imports.elements, &elements) {
            self.update(self.create_named_imports(elements), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_import_specifier_raw(
        &self,
        is_type_only: bool,
        property_name: Option<Id<Node /*Identifier*/>>,
        name: Id<Node /*Identifier*/>,
    ) -> ImportSpecifier {
        let node = self.create_base_node(SyntaxKind::ImportSpecifier);
        let node = ImportSpecifier::new(node, is_type_only, property_name, name);
        node.add_transform_flags(
            propagate_child_flags(node.property_name.clone(), self)
                | propagate_child_flags(Some(node.name), self),
        );
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_import_specifier(
        &self,
        node: Id<Node>, /*ImportSpecifier*/
        is_type_only: bool,
        property_name: Option<Id<Node /*Identifier*/>>,
        name: Id<Node /*Identifier*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_import_specifier = node_ref.as_import_specifier();
        if node_as_import_specifier.is_type_only != is_type_only
            || node_as_import_specifier.property_name != property_name
            || node_as_import_specifier.name != name
        {
            self.update(
                self.create_import_specifier(is_type_only, property_name, name),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_export_assignment_raw(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        is_export_equals: Option<bool>,
        expression: Id<Node /*Expression*/>,
    ) -> ExportAssignment {
        let node =
            self.create_base_declaration(SyntaxKind::ExportAssignment, decorators, modifiers);
        let node = ExportAssignment::new(
            node,
            is_export_equals,
            if matches!(is_export_equals, Some(true)) {
                self.parenthesizer_rules()
                    .ref_(self).parenthesize_right_side_of_binary(SyntaxKind::EqualsToken, None, expression)
            } else {
                self.parenthesizer_rules()
                    .ref_(self).parenthesize_expression_of_export_default(expression)
            },
        );
        node.add_transform_flags(propagate_child_flags(Some(node.expression), self));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_export_assignment(
        &self,
        node: Id<Node>, /*ExportAssignment*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_export_assignment = node_ref.as_export_assignment();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.ref_(self).maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.ref_(self).maybe_modifiers().as_deref(), modifiers.as_ref())
            || node_as_export_assignment.expression != expression
        {
            self.update(
                self.create_export_assignment(
                    decorators,
                    modifiers,
                    node_as_export_assignment.is_export_equals,
                    expression,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_export_declaration_raw(
        &self,
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        is_type_only: bool,
        export_clause: Option<Id<Node /*NamedExportBindings*/>>,
        module_specifier: Option<Id<Node /*Expression*/>>,
        assert_clause: Option<Id<Node /*AssertClause*/>>,
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
            propagate_child_flags(node.export_clause.clone(), self)
                | propagate_child_flags(node.module_specifier.clone(), self),
        );
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_export_declaration(
        &self,
        node: Id<Node>, /*ExportDeclaration*/
        decorators: Option<impl Into<NodeArrayOrVec>>,
        modifiers: Option<impl Into<NodeArrayOrVec>>,
        is_type_only: bool,
        export_clause: Option<Id<Node /*NamedExportBindings*/>>,
        module_specifier: Option<Id<Node /*Expression*/>>,
        assert_clause: Option<Id<Node /*AssertClause*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_export_declaration = node_ref.as_export_declaration();
        let decorators = decorators.map(Into::into);
        let modifiers = modifiers.map(Into::into);
        if has_option_node_array_changed(node.ref_(self).maybe_decorators().as_deref(), decorators.as_ref())
            || has_option_node_array_changed(node.ref_(self).maybe_modifiers().as_deref(), modifiers.as_ref())
            || node_as_export_declaration.is_type_only != is_type_only
            || node_as_export_declaration.export_clause != export_clause
            || node_as_export_declaration.module_specifier != module_specifier
            || node_as_export_declaration.assert_clause != assert_clause
        {
            self.update(
                self.create_export_declaration(
                    decorators,
                    modifiers,
                    is_type_only,
                    export_clause,
                    module_specifier,
                    assert_clause,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_named_exports_raw(&self, elements: impl Into<NodeArrayOrVec>) -> NamedExports {
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
        node: Id<Node>, /*NamedExports*/
        elements: impl Into<NodeArrayOrVec>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_named_exports = node_ref.as_named_exports();
        let elements = elements.into();
        if has_node_array_changed(&node_as_named_exports.elements, &elements) {
            self.update(self.create_named_exports(elements), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_export_specifier_raw<'property_name, 'name>(
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
            propagate_child_flags(node.property_name.clone(), self)
                | propagate_child_flags(Some(node.name), self),
        );
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_export_specifier(
        &self,
        node: Id<Node>, /*ExportSpecifier*/
        is_type_only: bool,
        property_name: Option<Id<Node> /*Identifier*/>,
        name: Id<Node>,
        /*Identifier*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_export_specifier = node_ref.as_export_specifier();
        if node_as_export_specifier.is_type_only != is_type_only
            || node_as_export_specifier.property_name != property_name
            || node_as_export_specifier.name != name
        {
            self.update(
                self.create_export_specifier(is_type_only, property_name, name),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_missing_declaration_raw(&self) -> MissingDeclaration {
        let node = self.create_base_declaration(
            SyntaxKind::MissingDeclaration,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
        );
        MissingDeclaration::new(node)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_external_module_reference_raw(
        &self,
        expression: Id<Node /*Expression*/>,
    ) -> ExternalModuleReference {
        let node = self.create_base_node(SyntaxKind::ExternalModuleReference);
        let node = ExternalModuleReference::new(node, expression);
        node.add_transform_flags(propagate_child_flags(Some(node.expression), self));
        node.set_transform_flags(
            node.transform_flags() & !TransformFlags::ContainsPossibleTopLevelAwait,
        );
        node
    }

    pub fn update_external_module_reference(
        &self,
        node: Id<Node>, /*ExternalModuleReference*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_external_module_reference = node_ref.as_external_module_reference();
        if node_as_external_module_reference.expression != expression {
            self.update(self.create_external_module_reference(expression), node)
        } else {
            node
        }
    }

    pub(crate) fn create_jsdoc_primary_type_worker(&self, kind: SyntaxKind) -> BaseNode {
        self.create_base_node(kind)
    }

    pub(crate) fn create_jsdoc_unary_type_worker(
        &self,
        kind: SyntaxKind,
        type_: Option<Id<Node /*TypeNode*/>>,
    ) -> BaseJSDocUnaryType {
        let node = self.create_base_node(kind);
        let node = BaseJSDocUnaryType::new(node, type_);
        node
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_function_type_raw<TParameters: Into<NodeArrayOrVec>>(
        &self,
        parameters: TParameters,
        type_: Option<Id<Node /*TypeNode*/>>,
    ) -> JSDocFunctionType {
        let node = self.create_base_signature_declaration(
            SyntaxKind::JSDocFunctionType,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            Option::<Id<Node>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(parameters),
            type_,
        );
        JSDocFunctionType::new(node)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_type_literal_raw<TPropertyTags: Into<NodeArrayOrVec>>(
        &self,
        property_tags: Option<TPropertyTags>,
        is_array_type: Option<bool>,
    ) -> JSDocTypeLiteral {
        let is_array_type = is_array_type.unwrap_or(false);
        let node = self.create_base_node(SyntaxKind::JSDocTypeLiteral);
        JSDocTypeLiteral::new(node, self.as_node_array(property_tags), is_array_type)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_type_expression_raw(
        &self,
        type_: Id<Node /*TypeNode*/>,
    ) -> JSDocTypeExpression {
        let node = self.create_base_node(SyntaxKind::JSDocTypeExpression);
        JSDocTypeExpression::new(node, type_)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_signature_raw<
        TTypeParameters: Into<NodeArrayOrVec>,
        TParameters: Into<NodeArrayOrVec>,
    >(
        &self,
        type_parameters: Option<TTypeParameters /*<JSDocTemplateTag>*/>,
        parameters: TParameters, /*<JSDocParameterTag>*/
        type_: Option<Id<Node /*JSDocReturnTag*/>>,
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
    fn get_default_tag_name(&self, node: Id<Node> /*JSDocTag*/) -> Id<Node /*Identifier*/> {
        let default_tag_name = get_default_tag_name_for_kind(node.ref_(self).kind());
        let node_ref = node.ref_(self);
        let node_as_jsdoc_tag = node_ref.as_jsdoc_tag();
        if node_as_jsdoc_tag.tag_name().ref_(self).as_identifier().escaped_text
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
        tag_name: Id<Node /*Identifier*/>,
        comment: Option<TComment>,
    ) -> BaseJSDocTag {
        let node = self.create_base_node(kind);
        BaseJSDocTag::new(node, tag_name, comment.map(Into::into))
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_template_tag_raw<
        TTypeParameters: Into<NodeArrayOrVec>,
        TComment: Into<StringOrNodeArray>,
    >(
        &self,
        tag_name: Option<Id<Node /*Identifier*/>>,
        constraint: Option<Id<Node /*JSDocTypeExpression*/>>,
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

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_typedef_tag_raw<TComment: Into<StringOrNodeArray>>(
        &self,
        tag_name: Option<Id<Node /*Identifier*/>>,
        type_expression: Option<Id<Node /*JSDocTypeExpression*/>>,
        full_name: Option<Id<Node /*Identifier | JSDocNamespaceDeclaration*/>>,
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
            get_jsdoc_type_alias_name(full_name, self),
            type_expression,
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_parameter_tag_raw<TComment: Into<StringOrNodeArray>>(
        &self,
        tag_name: Option<Id<Node /*Identifier*/>>,
        name: Id<Node /*EntityName*/>,
        is_bracketed: bool,
        type_expression: Option<Id<Node /*JSDocTypeExpression*/>>,
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

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_property_tag_raw(
        &self,
        tag_name: Option<Id<Node /*Identifier*/>>,
        name: Id<Node /*EntityName*/>,
        is_bracketed: bool,
        type_expression: Option<Id<Node /*JSDocTypeExpression*/>>,
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

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_callback_tag_raw(
        &self,
        tag_name: Option<Id<Node /*Identifier*/>>,
        type_expression: Id<Node /*JSDocSignature*/>,
        full_name: Option<Id<Node /*Identifier | JSDocNamespaceDeclaration*/>>,
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
            get_jsdoc_type_alias_name(full_name, self),
        )
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_augments_tag_raw<TComment: Into<StringOrNodeArray>>(
        &self,
        tag_name: Option<Id<Node /*Identifier*/>>,
        class_name: Id<Node /*JSDocAugmentsTag["class"]*/>,
        comment: Option<TComment>,
    ) -> JSDocAugmentsTag {
        let node = self.create_base_jsdoc_tag(
            SyntaxKind::JSDocAugmentsTag,
            tag_name.unwrap_or_else(|| self.create_identifier("augments")),
            comment,
        );
        JSDocAugmentsTag::new(node, class_name)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_implements_tag_raw<TComment: Into<StringOrNodeArray>>(
        &self,
        tag_name: Option<Id<Node /*Identifier*/>>,
        class_name: Id<Node /*JSDocImplementsTag["class"]*/>,
        comment: Option<TComment>,
    ) -> JSDocImplementsTag {
        let node = self.create_base_jsdoc_tag(
            SyntaxKind::JSDocImplementsTag,
            tag_name.unwrap_or_else(|| self.create_identifier("implements")),
            comment,
        );
        JSDocImplementsTag::new(node, class_name)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_see_tag_raw<TComment: Into<StringOrNodeArray>>(
        &self,
        tag_name: Option<Id<Node /*Identifier*/>>,
        name: Option<Id<Node /*JSDocNameReference*/>>,
        comment: Option<TComment>,
    ) -> JSDocSeeTag {
        let node = self.create_base_jsdoc_tag(
            SyntaxKind::JSDocSeeTag,
            tag_name.unwrap_or_else(|| self.create_identifier("see")),
            comment,
        );
        JSDocSeeTag::new(node, name)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_name_reference_raw(
        &self,
        name: Id<Node /*EntityName | JSDocMemberName*/>,
    ) -> JSDocNameReference {
        let node = self.create_base_node(SyntaxKind::JSDocNameReference);
        JSDocNameReference::new(node, name)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_member_name_raw(
        &self,
        left: Id<Node /*EntityName | JSDocMemberName*/>,
        right: Id<Node /*Identifier*/>,
    ) -> JSDocMemberName {
        let node = self.create_base_node(SyntaxKind::JSDocMemberName);
        let node = JSDocMemberName::new(node, left, right);
        node.add_transform_flags(
            propagate_child_flags(Some(node.left), self) | propagate_child_flags(Some(node.right), self),
        );
        node
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_link_raw(
        &self,
        name: Option<Id<Node /*EntityName | JSDocMemberName*/>>,
        text: String,
    ) -> JSDocLink {
        let node = self.create_base_node(SyntaxKind::JSDocLink);
        JSDocLink::new(node, name, text)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_link_code_raw(
        &self,
        name: Option<Id<Node /*EntityName | JSDocMemberName*/>>,
        text: String,
    ) -> JSDocLinkCode {
        let node = self.create_base_node(SyntaxKind::JSDocLinkCode);
        JSDocLinkCode::new(node, name, text)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_link_plain_raw(
        &self,
        name: Option<Id<Node /*EntityName | JSDocMemberName*/>>,
        text: String,
    ) -> JSDocLinkPlain {
        let node = self.create_base_node(SyntaxKind::JSDocLinkPlain);
        JSDocLinkPlain::new(node, name, text)
    }

    pub(crate) fn create_jsdoc_simple_tag_worker<TComment: Into<StringOrNodeArray>>(
        &self,
        kind: SyntaxKind,
        tag_name: Option<Id<Node /*Identifier*/>>,
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
        tag_name: Option<Id<Node /*Identifier*/>>,
        type_expression: Option<Id<Node /*JSDocTypeExpression*/>>,
        comment: Option<TComment>,
    ) -> BaseJSDocTypeLikeTag {
        let node = self.create_base_jsdoc_tag(
            kind,
            tag_name.unwrap_or_else(|| self.create_identifier(get_default_tag_name_for_kind(kind))),
            comment,
        );
        BaseJSDocTypeLikeTag::new(node, type_expression)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_unknown_tag_raw<TComment: Into<StringOrNodeArray>>(
        &self,
        tag_name: Id<Node /*Identifier*/>,
        comment: Option<TComment>,
    ) -> BaseJSDocTag {
        self.create_base_jsdoc_tag(SyntaxKind::JSDocTag, tag_name, comment)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_text_raw(&self, text: String) -> JSDocText {
        let node = self.create_base_node(SyntaxKind::JSDocText);
        JSDocText::new(node, text)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsdoc_comment_raw<
        TComment: Into<StringOrNodeArray>,
        TTags: Into<NodeArrayOrVec>,
    >(
        &self,
        comment: Option<TComment>,
        tags: Option<TTags>,
    ) -> JSDoc {
        let node = self.create_base_node(SyntaxKind::JSDocComment);
        JSDoc::new(node, comment.map(Into::into), self.as_node_array(tags))
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsx_element_raw(
        &self,
        opening_element: Id<Node /*JsxOpeningElement*/>,
        children: impl Into<NodeArrayOrVec>,
        closing_element: Id<Node /*JsxClosingElement*/>,
    ) -> JsxElement {
        let node = self.create_base_node(SyntaxKind::JsxElement);
        let node = JsxElement::new(
            node,
            opening_element,
            self.create_node_array(Some(children), None),
            closing_element,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.opening_element), self)
                | propagate_children_flags(Some(&node.children))
                | propagate_child_flags(Some(node.closing_element), self)
                | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_element(
        &self,
        node: Id<Node>, /*JsxElement*/
        opening_element: Id<Node /*JsxOpeningElement*/>,
        children: impl Into<NodeArrayOrVec>,
        closing_element: Id<Node /*JsxClosingElement*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_jsx_element = node_ref.as_jsx_element();
        let children = children.into();
        if node_as_jsx_element.opening_element != opening_element
            || has_node_array_changed(&node_as_jsx_element.children, &children)
            || node_as_jsx_element.closing_element != closing_element
        {
            self.update(
                self.create_jsx_element(opening_element, children, closing_element),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsx_self_closing_element_raw(
        &self,
        tag_name: Id<Node /*JsxTagNameExpression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
        attributes: Id<Node /*JsxAttributes*/>,
    ) -> JsxSelfClosingElement {
        let node = self.create_base_node(SyntaxKind::JsxSelfClosingElement);
        let node = JsxSelfClosingElement::new(
            node,
            tag_name,
            self.as_node_array(type_arguments),
            attributes,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.tag_name), self)
                | propagate_children_flags(node.maybe_type_arguments().as_deref())
                | propagate_child_flags(Some(node.attributes), self)
                | TransformFlags::ContainsJsx,
        );
        if node.maybe_type_arguments().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub fn update_jsx_self_closing_element(
        &self,
        node: Id<Node>, /*JsxSelfClosingElement*/
        tag_name: Id<Node /*JsxTagNameExpression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
        attributes: Id<Node /*JsxAttributes*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_jsx_self_closing_element = node_ref.as_jsx_self_closing_element();
        let type_arguments = type_arguments.map(Into::into);
        if node_as_jsx_self_closing_element.tag_name != tag_name
            || has_option_node_array_changed(
                node_as_jsx_self_closing_element
                    .maybe_type_arguments()
                    .as_deref(),
                type_arguments.as_ref(),
            )
            || node_as_jsx_self_closing_element.attributes != attributes
        {
            self.update(
                self.create_jsx_self_closing_element(tag_name, type_arguments, attributes),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsx_opening_element_raw(
        &self,
        tag_name: Id<Node /*JsxTagNameExpression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
        attributes: Id<Node /*JsxAttributes*/>,
    ) -> JsxOpeningElement {
        let node = self.create_base_node(SyntaxKind::JsxOpeningElement);
        let node = JsxOpeningElement::new(
            node,
            tag_name,
            self.as_node_array(type_arguments),
            attributes,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.tag_name), self)
                | propagate_children_flags(node.maybe_type_arguments().as_deref())
                | propagate_child_flags(Some(node.attributes), self)
                | TransformFlags::ContainsJsx,
        );
        if node.maybe_type_arguments().is_some() {
            node.add_transform_flags(TransformFlags::ContainsTypeScript);
        }
        node
    }

    pub fn update_jsx_opening_element(
        &self,
        node: Id<Node>, /*JsxOpeningElement*/
        tag_name: Id<Node /*JsxTagNameExpression*/>,
        type_arguments: Option<impl Into<NodeArrayOrVec>>,
        attributes: Id<Node /*JsxAttributes*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_jsx_opening_element = node_ref.as_jsx_opening_element();
        let type_arguments = type_arguments.map(Into::into);
        if node_as_jsx_opening_element.tag_name != tag_name
            || has_option_node_array_changed(
                node_as_jsx_opening_element
                    .maybe_type_arguments()
                    .as_deref(),
                type_arguments.as_ref(),
            )
            || node_as_jsx_opening_element.attributes != attributes
        {
            self.update(
                self.create_jsx_opening_element(tag_name, type_arguments, attributes),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsx_closing_element_raw(
        &self,
        tag_name: Id<Node /*JsxTagNameExpression*/>,
    ) -> JsxClosingElement {
        let node = self.create_base_node(SyntaxKind::JsxClosingElement);
        let node = JsxClosingElement::new(node, tag_name);
        node.add_transform_flags(
            propagate_child_flags(Some(node.tag_name), self) | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_closing_element(
        &self,
        node: Id<Node>, /*JsxClosingElement*/
        tag_name: Id<Node /*JsxTagNameExpression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_jsx_closing_element = node_ref.as_jsx_closing_element();
        if node_as_jsx_closing_element.tag_name != tag_name {
            self.update(self.create_jsx_closing_element(tag_name), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsx_fragment_raw(
        &self,
        opening_fragment: Id<Node /*JsxOpeningFragment*/>,
        children: impl Into<NodeArrayOrVec>,
        closing_fragment: Id<Node /*JsxClosingFragment*/>,
    ) -> JsxFragment {
        let node = self.create_base_node(SyntaxKind::JsxFragment);
        let node = JsxFragment::new(
            node,
            opening_fragment,
            self.create_node_array(Some(children), None),
            closing_fragment,
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.opening_fragment), self)
                | propagate_children_flags(Some(&node.children))
                | propagate_child_flags(Some(node.closing_fragment), self)
                | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_fragment(
        &self,
        node: Id<Node>, /*JsxFragment*/
        opening_fragment: Id<Node /*JsxOpeningFragment*/>,
        children: impl Into<NodeArrayOrVec>,
        closing_fragment: Id<Node /*JsxClosingFragment*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_jsx_fragment = node_ref.as_jsx_fragment();
        let children = children.into();
        if node_as_jsx_fragment.opening_fragment != opening_fragment
            || has_node_array_changed(&node_as_jsx_fragment.children, &children)
            || node_as_jsx_fragment.closing_fragment != closing_fragment
        {
            self.update(
                self.create_jsx_fragment(opening_fragment, children, closing_fragment),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsx_text_raw(
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
        node: Id<Node>, /*JsxText*/
        text: String,
        contains_only_trivia_white_spaces: Option<bool>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_jsx_text = node_ref.as_jsx_text();
        if &*node_as_jsx_text.text() != &text
            || Some(node_as_jsx_text.contains_only_trivia_white_spaces)
                != contains_only_trivia_white_spaces
        {
            self.update(
                self.create_jsx_text(text, contains_only_trivia_white_spaces),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsx_opening_fragment_raw(&self) -> JsxOpeningFragment {
        let node = self.create_base_node(SyntaxKind::JsxOpeningFragment);
        let node = JsxOpeningFragment::new(node);
        node.add_transform_flags(TransformFlags::ContainsJsx);
        node
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsx_jsx_closing_fragment_raw(&self) -> JsxClosingFragment {
        let node = self.create_base_node(SyntaxKind::JsxClosingFragment);
        let node = JsxClosingFragment::new(node);
        node.add_transform_flags(TransformFlags::ContainsJsx);
        node
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsx_attribute_raw(
        &self,
        name: Id<Node /*Identifier*/>,
        initializer: Option<Id<Node /*StringLiteral | JsxExpression*/>>,
    ) -> JsxAttribute {
        let node = self.create_base_node(SyntaxKind::JsxAttribute);
        let node = JsxAttribute::new(node, name, initializer);
        node.add_transform_flags(
            propagate_child_flags(Some(node.name), self)
                | propagate_child_flags(node.initializer.clone(), self)
                | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_attribute(
        &self,
        node: Id<Node>, /*JsxAttribute*/
        name: Id<Node /*Identifier*/>,
        initializer: Option<Id<Node /*StringLiteral | JsxExpression*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_jsx_attribute = node_ref.as_jsx_attribute();
        if node_as_jsx_attribute.name != name
            || node_as_jsx_attribute.initializer != initializer
        {
            self.update(self.create_jsx_attribute(name, initializer), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsx_attributes_raw(
        &self,
        properties: impl Into<NodeArrayOrVec>,
    ) -> JsxAttributes {
        let node = self.create_base_node(SyntaxKind::JsxAttributes);
        let node = JsxAttributes::new(node, self.create_node_array(Some(properties), None));
        node.add_transform_flags(
            propagate_children_flags(Some(&node.properties)) | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_attributes(
        &self,
        node: Id<Node>, /*JsxAttributes*/
        properties: impl Into<NodeArrayOrVec>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_jsx_attributes = node_ref.as_jsx_attributes();
        let properties = properties.into();
        if has_node_array_changed(&node_as_jsx_attributes.properties, &properties) {
            self.update(self.create_jsx_attributes(properties), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsx_spread_attribute_raw(
        &self,
        expression: Id<Node /*Expression*/>,
    ) -> JsxSpreadAttribute {
        let node = self.create_base_node(SyntaxKind::JsxSpreadAttribute);
        let node = JsxSpreadAttribute::new(node, expression);
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self) | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_spread_attribute(
        &self,
        node: Id<Node>, /*JsxSpreadAttribute*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_jsx_spread_attribute = node_ref.as_jsx_spread_attribute();
        if node_as_jsx_spread_attribute.expression != expression {
            self.update(self.create_jsx_spread_attribute(expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_jsx_expression_raw(
        &self,
        dot_dot_dot_token: Option<Id<Node /*DotDotDotToken*/>>,
        expression: Option<Id<Node /*Expression*/>>,
    ) -> JsxExpression {
        let node = self.create_base_node(SyntaxKind::JsxExpression);
        let node = JsxExpression::new(node, dot_dot_dot_token, expression);
        node.add_transform_flags(
            propagate_child_flags(node.dot_dot_dot_token.clone(), self)
                | propagate_child_flags(node.expression.clone(), self)
                | TransformFlags::ContainsJsx,
        );
        node
    }

    pub fn update_jsx_expression(
        &self,
        node: Id<Node>, /*JsxExpression*/
        expression: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_jsx_expression = node_ref.as_jsx_expression();
        if node_as_jsx_expression.expression != expression {
            self.update(
                self.create_jsx_expression(
                    node_as_jsx_expression.dot_dot_dot_token.clone(),
                    expression,
                ),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_case_clause_raw(
        &self,
        expression: Id<Node /*Expression*/>,
        statements: impl Into<NodeArrayOrVec>,
    ) -> CaseClause {
        let node = self.create_base_node(SyntaxKind::CaseClause);
        let node = CaseClause::new(
            node,
            self.parenthesizer_rules()
                .ref_(self).parenthesize_expression_for_disallowed_comma(expression),
            self.create_node_array(Some(statements), None),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression), self)
                | propagate_children_flags(Some(&node.statements)),
        );
        node
    }

    pub fn update_case_clause(
        &self,
        node: Id<Node>, /*CaseClause*/
        expression: Id<Node /*Expression*/>,
        statements: impl Into<NodeArrayOrVec>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_case_clause = node_ref.as_case_clause();
        let statements = statements.into();
        if node_as_case_clause.expression != expression
            || has_node_array_changed(&node_as_case_clause.statements, &statements)
        {
            self.update(self.create_case_clause(expression, statements), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_default_clause_raw(
        &self,
        statements: impl Into<NodeArrayOrVec>,
    ) -> DefaultClause {
        let node = self.create_base_node(SyntaxKind::DefaultClause);
        let node = DefaultClause::new(node, self.create_node_array(Some(statements), None));
        node.add_transform_flags(propagate_children_flags(Some(&node.statements)));
        node
    }

    pub fn update_default_clause(
        &self,
        node: Id<Node>, /*DefaultClause*/
        statements: impl Into<NodeArrayOrVec>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_default_clause = node_ref.as_default_clause();
        let statements = statements.into();
        if has_node_array_changed(&node_as_default_clause.statements, &statements) {
            self.update(self.create_default_clause(statements), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_heritage_clause_raw(
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
        node: Id<Node>,                   /*HeritageClause*/
        types: impl Into<NodeArrayOrVec>, /*<ExpressionWithTypeArguments>*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_heritage_clause = node_ref.as_heritage_clause();
        let types = types.into();
        if has_node_array_changed(&node_as_heritage_clause.types, &types) {
            self.update(
                self.create_heritage_clause(node_as_heritage_clause.token, types),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_catch_clause_raw<'variable_declaration>(
        &self,
        variable_declaration: Option<
            impl Into<StrOrRcNode<'variable_declaration>>,
            /*BindingName | VariableDeclaration*/
        >,
        block: Id<Node /*Block*/>,
    ) -> CatchClause {
        let node = self.create_base_node(SyntaxKind::CatchClause);
        let variable_declaration: Option<Id<Node>> =
            variable_declaration.map(|variable_declaration| {
                let variable_declaration = variable_declaration.into();
                match variable_declaration {
                    StrOrRcNode::Str(variable_declaration) => self.create_variable_declaration(
                        Some(variable_declaration),
                        None,
                        None,
                        None,
                    ),
                    StrOrRcNode::RcNode(variable_declaration)
                        if !is_variable_declaration(&variable_declaration.ref_(self)) =>
                    {
                        self.create_variable_declaration(
                            Some(variable_declaration),
                            None,
                            None,
                            None,
                        )
                    }
                    StrOrRcNode::RcNode(variable_declaration) => variable_declaration,
                }
            });
        let variable_declaration_is_some = variable_declaration.is_some();
        let node = CatchClause::new(node, variable_declaration, block);
        node.add_transform_flags(
            propagate_child_flags(node.variable_declaration.clone(), self)
                | propagate_child_flags(Some(node.block), self),
        );
        if !variable_declaration_is_some {
            node.add_transform_flags(TransformFlags::ContainsES2019);
        }
        node
    }

    pub fn update_catch_clause(
        &self,
        node: Id<Node>, /*CatchClause*/
        variable_declaration: Option<Id<Node> /*VariableDeclaration*/>,
        block: Id<Node /*Block*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_catch_clause = node_ref.as_catch_clause();
        if node_as_catch_clause.variable_declaration != variable_declaration
            || node_as_catch_clause.block != block
        {
            self.update(self.create_catch_clause(variable_declaration, block), node)
        } else {
            node
        }
    }
}
