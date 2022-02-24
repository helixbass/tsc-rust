#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use super::BinderType;
use crate::{
    get_assignment_declaration_kind, is_expression,
    is_function_like_or_class_static_block_declaration, is_in_js_file, is_jsdoc_type_alias,
    is_object_literal_method, is_part_of_type_query, is_special_property_declaration,
    is_this_initialized_declaration, AssignmentDeclarationKind, Debug_,
    FunctionLikeDeclarationInterface, SyntaxKind, __String, is_assignment_expression,
    is_binding_pattern, is_block_or_catch_scoped, is_exports_identifier, is_identifier,
    is_module_exports_access_expression, is_source_file, is_variable_declaration,
    HasInitializerInterface, NamedDeclarationInterface, Node, NodeInterface, Symbol, SymbolFlags,
    SymbolInterface,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum ElementKind {
    Property = 1,
    Accessor = 2,
}

impl BinderType {
    pub(super) fn bind_worker(&self, node: &Node) {
        match node.kind() {
            SyntaxKind::Identifier => {
                if matches!(
                    node.as_identifier().maybe_is_in_jsdoc_namespace(),
                    Some(true)
                ) {
                    let mut parent_node = node.parent();
                    while
                    /*parentNode &&*/
                    !is_jsdoc_type_alias(&parent_node) {
                        parent_node = parent_node.parent();
                    }
                    self.bind_block_scoped_declaration(
                        &parent_node,
                        SymbolFlags::TypeAlias,
                        SymbolFlags::TypeAliasExcludes,
                    );
                } else {
                    if let Some(current_flow) = self.maybe_current_flow() {
                        if is_expression(node)
                            || self.parent().kind() == SyntaxKind::ShorthandPropertyAssignment
                        {
                            node.set_flow_node(Some(current_flow));
                        }
                    }
                    self.check_contextual_identifier(node);
                }
            }
            SyntaxKind::ThisKeyword => {
                if let Some(current_flow) = self.maybe_current_flow() {
                    if is_expression(node)
                        || self.parent().kind() == SyntaxKind::ShorthandPropertyAssignment
                    {
                        node.set_flow_node(Some(current_flow));
                    }
                }
                self.check_contextual_identifier(node);
            }
            SyntaxKind::QualifiedName => {
                if let Some(current_flow) = self.maybe_current_flow() {
                    if is_part_of_type_query(node) {
                        node.set_flow_node(Some(current_flow));
                    }
                }
            }
            SyntaxKind::MetaProperty | SyntaxKind::SuperKeyword => {
                node.set_flow_node(self.maybe_current_flow());
            }
            SyntaxKind::PrivateIdentifier => {
                self.check_private_identifier(node);
            }
            SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
                let expr = node;
                if let Some(current_flow) = self.maybe_current_flow() {
                    if self.is_narrowable_reference(expr) {
                        expr.set_flow_node(Some(current_flow));
                    }
                }
                if is_special_property_declaration(expr) {
                    self.bind_special_property_declaration(expr);
                }
                if is_in_js_file(Some(expr))
                    && self
                        .file()
                        .as_source_file()
                        .maybe_common_js_module_indicator()
                        .is_some()
                    && is_module_exports_access_expression(expr)
                    && lookup_symbol_for_name(
                        &self.block_scope_container(),
                        &__String::new("module".to_owned()),
                    )
                    .is_none()
                {
                    self.declare_symbol(
                        &mut self.file().locals(),
                        Option::<&Symbol>::None,
                        &expr.as_has_expression().expression(),
                        SymbolFlags::FunctionScopedVariable | SymbolFlags::ModuleExports,
                        SymbolFlags::FunctionScopedVariableExcludes,
                        None,
                        None,
                    );
                }
            }
            SyntaxKind::BinaryExpression => {
                let special_kind = get_assignment_declaration_kind(node);
                match special_kind {
                    AssignmentDeclarationKind::ExportsProperty => {
                        self.bind_exports_property_assignment(node);
                    }
                    AssignmentDeclarationKind::ModuleExports => {
                        self.bind_module_exports_assignment(node);
                    }
                    AssignmentDeclarationKind::PrototypeProperty => {
                        self.bind_prototype_property_assignment(
                            &node.as_binary_expression().left,
                            node,
                        );
                    }
                    AssignmentDeclarationKind::Prototype => {
                        self.bind_prototype_assignment(node);
                    }
                    AssignmentDeclarationKind::ThisProperty => {
                        self.bind_this_property_assignment(node);
                    }
                    AssignmentDeclarationKind::Property => {
                        let expression = node
                            .as_binary_expression()
                            .left
                            .as_has_expression()
                            .expression();
                        if is_in_js_file(Some(node)) && is_identifier(&expression) {
                            let symbol = lookup_symbol_for_name(
                                &self.block_scope_container(),
                                &expression.as_identifier().escaped_text,
                            );
                            if is_this_initialized_declaration(
                                symbol.and_then(|symbol| symbol.maybe_value_declaration()),
                            ) {
                                self.bind_this_property_assignment(node);
                            } else {
                                self.bind_special_property_assignment(node);
                            }
                        } else {
                            self.bind_special_property_assignment(node);
                        }
                    }
                    AssignmentDeclarationKind::None => (),
                    _ => Debug_.fail(Some(
                        "Unknown binary expression special property assignment kind",
                    )),
                }
                self.check_strict_mode_binary_expression(node);
            }
            SyntaxKind::CatchClause => {
                self.check_strict_mode_catch_clause(node);
            }
            SyntaxKind::DeleteExpression => {
                self.check_strict_mode_delete_expression(node);
            }
            SyntaxKind::NumericLiteral => {
                self.check_strict_mode_numeric_literal(node);
            }
            SyntaxKind::PostfixUnaryExpression => {
                self.check_strict_mode_postfix_unary_expression(node);
            }
            SyntaxKind::PrefixUnaryExpression => {
                self.check_strict_mode_prefix_unary_expression(node);
            }
            SyntaxKind::WithStatement => {
                self.check_strict_mode_with_statement(node);
            }
            SyntaxKind::LabeledStatement => {
                self.check_strict_mode_labeled_statement(node);
            }
            SyntaxKind::ThisType => {
                self.set_seen_this_keyword(Some(true));
            }
            SyntaxKind::TypePredicate => (),
            SyntaxKind::TypeParameter => {
                self.bind_type_parameter(node);
            }
            SyntaxKind::Parameter => {
                self.bind_parameter(node);
            }
            SyntaxKind::VariableDeclaration => {
                self.bind_variable_declaration_or_binding_element(node);
            }
            SyntaxKind::BindingElement => {
                node.set_flow_node(self.maybe_current_flow());
                self.bind_variable_declaration_or_binding_element(node);
            }
            SyntaxKind::PropertyDeclaration | SyntaxKind::PropertySignature => {
                self.bind_property_worker(node);
            }
            SyntaxKind::PropertyAssignment | SyntaxKind::ShorthandPropertyAssignment => {
                self.bind_property_or_method_or_accessor(
                    node,
                    SymbolFlags::Property,
                    SymbolFlags::PropertyExcludes,
                );
            }
            SyntaxKind::EnumMember => {
                self.bind_property_or_method_or_accessor(
                    node,
                    SymbolFlags::EnumMember,
                    SymbolFlags::EnumMemberExcludes,
                );
            }

            SyntaxKind::CallSignature
            | SyntaxKind::ConstructSignature
            | SyntaxKind::IndexSignature => {
                self.declare_symbol_and_add_to_symbol_table(
                    node,
                    SymbolFlags::Signature,
                    SymbolFlags::None,
                );
            }
            SyntaxKind::MethodDeclaration | SyntaxKind::MethodSignature => {
                self.bind_property_or_method_or_accessor(
                    node,
                    SymbolFlags::Method
                        | if matches!(node.kind(), SyntaxKind::MethodDeclaration)
                            && node
                                .as_method_declaration()
                                .maybe_question_token()
                                .is_some()
                        {
                            SymbolFlags::Optional
                        } else {
                            SymbolFlags::None
                        },
                    if is_object_literal_method(node) {
                        SymbolFlags::PropertyExcludes
                    } else {
                        SymbolFlags::MethodExcludes
                    },
                );
            }
            SyntaxKind::FunctionDeclaration => {
                self.bind_function_declaration(node);
            }
            SyntaxKind::Constructor => {
                self.declare_symbol_and_add_to_symbol_table(
                    node,
                    SymbolFlags::Constructor,
                    SymbolFlags::None,
                );
            }
            SyntaxKind::GetAccessor => {
                self.bind_property_or_method_or_accessor(
                    node,
                    SymbolFlags::GetAccessor,
                    SymbolFlags::GetAccessorExcludes,
                );
            }
            SyntaxKind::SetAccessor => {
                self.bind_property_or_method_or_accessor(
                    node,
                    SymbolFlags::SetAccessor,
                    SymbolFlags::SetAccessorExcludes,
                );
            }
            SyntaxKind::FunctionType
            | SyntaxKind::JSDocFunctionType
            | SyntaxKind::JSDocSignature
            | SyntaxKind::ConstructorType => {
                self.bind_function_or_constructor_type(node);
            }
            SyntaxKind::TypeLiteral | SyntaxKind::JSDocTypeLiteral | SyntaxKind::MappedType => {
                self.bind_anonymous_type_worker(node);
            }
            SyntaxKind::JSDocClassTag => {
                self.bind_jsdoc_class_tag(node);
            }
            SyntaxKind::ObjectLiteralExpression => {
                self.bind_object_literal_expression(node);
            }
            SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction => {
                self.bind_function_expression(node);
            }

            SyntaxKind::CallExpression => {
                let assignment_kind = get_assignment_declaration_kind(node);
                match assignment_kind {
                    AssignmentDeclarationKind::ObjectDefinePropertyValue => {
                        return self.bind_object_define_property_assignment(node);
                    }
                    AssignmentDeclarationKind::ObjectDefinePropertyExports => {
                        return self.bind_object_define_property_export(node);
                    }
                    AssignmentDeclarationKind::ObjectDefinePrototypeProperty => {
                        return self.bind_object_define_prototype_property(node);
                    }
                    AssignmentDeclarationKind::None => (),
                    _ => Debug_.fail(Some("Unknown call expression assignment declaration kind")),
                }
                if is_in_js_file(Some(node)) {
                    self.bind_call_expression(node);
                }
            }

            SyntaxKind::ClassExpression | SyntaxKind::ClassDeclaration => {
                self.set_in_strict_mode(Some(true));
                self.bind_class_like_declaration(node);
            }
            SyntaxKind::InterfaceDeclaration => {
                self.bind_block_scoped_declaration(
                    node,
                    SymbolFlags::Interface,
                    SymbolFlags::InterfaceExcludes,
                );
            }
            SyntaxKind::TypeAliasDeclaration => {
                self.bind_block_scoped_declaration(
                    node,
                    SymbolFlags::TypeAlias,
                    SymbolFlags::TypeAliasExcludes,
                );
            }
            SyntaxKind::EnumDeclaration => {
                self.bind_enum_declaration(node);
            }
            SyntaxKind::ModuleDeclaration => {
                self.bind_module_declaration(node);
            }
            SyntaxKind::JsxAttributes => {
                self.bind_jsx_attributes(node);
            }
            SyntaxKind::JsxAttribute => {
                self.bind_jsx_attribute(node, SymbolFlags::Property, SymbolFlags::PropertyExcludes);
            }

            SyntaxKind::ImportEqualsDeclaration
            | SyntaxKind::NamespaceImport
            | SyntaxKind::ImportSpecifier
            | SyntaxKind::ExportSpecifier => {
                self.declare_symbol_and_add_to_symbol_table(
                    node,
                    SymbolFlags::Alias,
                    SymbolFlags::AliasExcludes,
                );
            }
            SyntaxKind::NamespaceExportDeclaration => {
                self.bind_namespace_export_declaration(node);
            }
            SyntaxKind::ImportClause => {
                self.bind_import_clause(node);
            }
            SyntaxKind::ExportDeclaration => {
                self.bind_export_declaration(node);
            }
            SyntaxKind::ExportAssignment => {
                self.bind_export_assignment(node);
            }
            SyntaxKind::SourceFile => {
                self.update_strict_mode_statement_list(&node.as_source_file().statements);
                self.bind_source_file_if_external_module();
            }
            SyntaxKind::Block => {
                if !is_function_like_or_class_static_block_declaration(node.maybe_parent()) {
                    return;
                }
                self.update_strict_mode_statement_list(&node.as_block().statements);
            }
            SyntaxKind::ModuleBlock => {
                self.update_strict_mode_statement_list(&node.as_module_block().statements);
            }

            SyntaxKind::JSDocParameterTag => {
                if node.parent().kind() == SyntaxKind::JSDocSignature {
                    return self.bind_parameter(node);
                }
                if node.parent().kind() != SyntaxKind::JSDocTypeLiteral {
                    return;
                }
                let prop_tag = node;
                let prop_tag_as_jsdoc_property_like_tag = prop_tag.as_jsdoc_property_like_tag();
                let flags = if prop_tag_as_jsdoc_property_like_tag.is_bracketed
                    || matches!(prop_tag_as_jsdoc_property_like_tag.type_expression.as_ref(), Some(type_expression) if type_expression.as_jsdoc_type_expression().type_.kind() == SyntaxKind::JSDocOptionalType)
                {
                    SymbolFlags::Property | SymbolFlags::Optional
                } else {
                    SymbolFlags::Property
                };
                self.declare_symbol_and_add_to_symbol_table(
                    prop_tag,
                    flags,
                    SymbolFlags::PropertyExcludes,
                );
            }
            SyntaxKind::JSDocPropertyTag => {
                let prop_tag = node;
                let prop_tag_as_jsdoc_property_like_tag = prop_tag.as_jsdoc_property_like_tag();
                let flags = if prop_tag_as_jsdoc_property_like_tag.is_bracketed
                    || matches!(prop_tag_as_jsdoc_property_like_tag.type_expression.as_ref(), Some(type_expression) if type_expression.as_jsdoc_type_expression().type_.kind() == SyntaxKind::JSDocOptionalType)
                {
                    SymbolFlags::Property | SymbolFlags::Optional
                } else {
                    SymbolFlags::Property
                };
                self.declare_symbol_and_add_to_symbol_table(
                    prop_tag,
                    flags,
                    SymbolFlags::PropertyExcludes,
                );
            }
            SyntaxKind::JSDocTypedefTag
            | SyntaxKind::JSDocCallbackTag
            | SyntaxKind::JSDocEnumTag => {
                let mut delayed_type_aliases = self.maybe_delayed_type_aliases();
                if delayed_type_aliases.is_none() {
                    *delayed_type_aliases = Some(vec![]);
                }
                delayed_type_aliases
                    .as_mut()
                    .unwrap()
                    .push(node.node_wrapper());
            }
            _ => (),
        }
    }

    pub(super) fn bind_property_worker(&self, node: &Node /*PropertySignature*/) {
        self.bind_property_or_method_or_accessor(
            node,
            SymbolFlags::Property
                | if false {
                    unimplemented!()
                } else {
                    SymbolFlags::None
                },
            SymbolFlags::PropertyExcludes,
        )
    }

    pub(super) fn bind_anonymous_type_worker(
        &self,
        node: &Node, /*TypeLiteralNode | MappedTypeNode | JSDocTypeLiteral*/
    ) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn bind_source_file_if_external_module(&self) {
        // unimplemented!()
    }

    pub(super) fn bind_export_assignment(&self, node: &Node /*ExportAssignment*/) {
        unimplemented!()
    }

    pub(super) fn bind_namespace_export_declaration(
        &self,
        node: &Node, /*NamespaceExportDeclaration*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_export_declaration(&self, node: &Node /*ExportDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn bind_import_clause(&self, node: &Node /*ImportClause*/) {
        unimplemented!()
    }

    pub(super) fn bind_object_define_property_export(
        &self,
        node: &Node, /*BindableObjectDefinePropertyCall*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_exports_property_assignment(
        &self,
        node: &Node, /*BindableStaticPropertyAssignmentExpression*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_module_exports_assignment(
        &self,
        node: &Node, /*BindablePropertyAssignmentExpression*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_export_assigned_object_member_alias(
        &self,
        node: &Node, /*ShorthandPropertyAssignment*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_this_property_assignment(
        &self,
        node: &Node, /*BindablePropertyAssignmentExpression | PropertyAccessExpression | LiteralLikeElementAccessExpression*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_special_property_declaration(
        &self,
        node: &Node, /*PropertyAccessExpression | LiteralLikeElementAccessExpression*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_prototype_assignment(
        &self,
        node: &Node, /*BindableStaticPropertyAssignmentExpression*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_object_define_prototype_property(
        &self,
        node: &Node, /*BindableObjectDefinePropertyCall*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_prototype_property_assignment(
        &self,
        lhs: &Node, /*BindableStaticAccessExpression*/
        parent: &Node,
    ) {
        unimplemented!()
    }

    pub(super) fn bind_object_define_property_assignment(
        &self,
        node: &Node, /*BindableObjectDefinePropertyCall*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_special_property_assignment(
        &self,
        node: &Node, /*BindablePropertyAssignmentExpression*/
    ) {
        unimplemented!()
    }

    pub(super) fn bind_potentially_missing_namespaces<TNamespaceSymbol: Borrow<Symbol>>(
        &self,
        namespace_symbol: Option<TNamespaceSymbol>,
        entity_name: &Node, /*BindableStaticNameExpression*/
        is_top_level: bool,
        is_prototype_property: bool,
        container_is_class: bool,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn is_top_level_namespace_assignment(
        &self,
        property_access: &Node, /*BindableAccessExpression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn bind_call_expression(&self, node: &Node /*CallExpression*/) {
        unimplemented!()
    }

    pub(super) fn bind_class_like_declaration(&self, node: &Node /*ClassLikeDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn bind_enum_declaration(&self, node: &Node /*EnumDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn bind_variable_declaration_or_binding_element(
        &self,
        node: &Node, /*VariableDeclaration*/
    ) {
        let node_as_variable_declaration = node.as_variable_declaration();
        if !is_binding_pattern(Some(node_as_variable_declaration.name())) {
            if false {
                unimplemented!()
            } else if is_block_or_catch_scoped(node) {
                self.bind_block_scoped_declaration(
                    node,
                    SymbolFlags::BlockScopedVariable,
                    SymbolFlags::BlockScopedVariableExcludes,
                );
            } else {
                self.declare_symbol_and_add_to_symbol_table(
                    node,
                    SymbolFlags::FunctionScopedVariable,
                    SymbolFlags::FunctionScopedVariableExcludes,
                );
            }
        }
    }

    pub(super) fn bind_parameter(&self, node: &Node /*ParameterDeclaration*/) {
        if is_binding_pattern(Some(node.as_parameter_declaration().name())) {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::FunctionScopedVariable,
                SymbolFlags::ParameterExcludes,
            );
        }
    }

    pub(super) fn bind_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::Function,
                SymbolFlags::FunctionExcludes,
            );
        }
    }

    pub(super) fn bind_function_expression(&self, node: &Node /*FunctionExpression*/) {
        unimplemented!()
    }

    pub(super) fn bind_property_or_method_or_accessor(
        &self,
        node: &Node,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(node, symbol_flags, symbol_excludes);
        }
    }

    pub(super) fn bind_type_parameter(&self, node: &Node /*TypeParameterDeclaration*/) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::TypeParameter,
                SymbolFlags::TypeParameterExcludes,
            );
        }
    }

    pub(super) fn check_unreachable(&self, node: &Node) -> bool {
        false
        // unimplemented!()
    }
}

pub fn is_exports_or_module_exports_or_alias(
    source_file: &Node, /*SourceFile*/
    node: &Node,        /*Expression*/
) -> bool {
    let mut node = node.node_wrapper();
    let mut i = 0;
    let mut q = vec![node];
    while !q.is_empty() && i < 100 {
        i += 1;
        node = q.remove(0);
        if is_exports_identifier(&node) || is_module_exports_access_expression(&node) {
            return true;
        } else if is_identifier(&node) {
            let symbol = lookup_symbol_for_name(&source_file, &node.as_identifier().escaped_text);
            if let Some(symbol) = symbol {
                if let Some(symbol_value_declaration) =
                    symbol
                        .maybe_value_declaration()
                        .filter(|value_declaration| {
                            is_variable_declaration(&value_declaration)
                                && value_declaration
                                    .as_variable_declaration()
                                    .maybe_initializer()
                                    .is_some()
                        })
                {
                    let init = symbol_value_declaration
                        .as_variable_declaration()
                        .maybe_initializer()
                        .unwrap();
                    q.push(init.clone());
                    if is_assignment_expression(&init, Some(true)) {
                        let init_as_binary_expression = init.as_binary_expression();
                        q.push(init_as_binary_expression.left.clone());
                        q.push(init_as_binary_expression.right.clone());
                    }
                }
            }
        }
    }
    false
}

fn lookup_symbol_for_name(container: &Node, name: &__String) -> Option<Rc<Symbol>> {
    let container_locals = container.maybe_locals();
    let local = container_locals
        .as_ref()
        .and_then(|locals| locals.get(name));
    if let Some(local) = local {
        return Some(local.maybe_export_symbol().unwrap_or(local.clone()));
    }
    if is_source_file(container) {
        let container_as_source_file = container.as_source_file();
        if let Some(container_js_global_augmentations) = container_as_source_file
            .maybe_js_global_augmentations()
            .as_ref()
        {
            let container_js_global_augmentations = container_js_global_augmentations.borrow_mut(); // TODO: doesn't actually need to be mut
            if container_js_global_augmentations.contains_key(name) {
                return container_js_global_augmentations
                    .get(name)
                    .map(Clone::clone);
            }
        }
    }
    container
        .maybe_symbol()
        .and_then(|symbol| symbol.maybe_exports().clone())
        .and_then(|exports| exports.borrow_mut().get(name).map(Clone::clone)) // TODO: same here doesn't need to be mut
}
