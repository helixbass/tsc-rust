#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{is_exports_or_module_exports_or_alias, lookup_symbol_for_name, BinderType};
use crate::{
    create_diagnostic_for_node, create_symbol_table, every, export_assignment_is_alias, for_each,
    get_assignment_declaration_kind, get_node_id, get_right_most_assigned_expression,
    get_this_container, has_dynamic_name, is_aliasable_expression, is_binary_expression,
    is_bindable_static_access_expression, is_empty_object_literal, is_expression,
    is_external_module, is_function_like_or_class_static_block_declaration, is_in_js_file,
    is_jsdoc_type_alias, is_json_source_file, is_namespace_export, is_object_literal_expression,
    is_object_literal_method, is_part_of_type_query, is_private_identifier,
    is_property_access_expression, is_prototype_access, is_shorthand_property_assignment,
    is_special_property_declaration, is_static, is_this_initialized_declaration,
    remove_file_extension, set_parent, set_value_declaration, AssignmentDeclarationKind, Debug_,
    Diagnostics, HasQuestionTokenInterface, InternalSymbolName, SymbolTable, SyntaxKind, __String,
    is_exports_identifier, is_identifier, is_module_exports_access_expression, is_source_file,
    Node, NodeInterface, Symbol, SymbolFlags, SymbolInterface,
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
                        &mut self.file().locals().borrow_mut(),
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
                        | if node
                            .as_has_question_token()
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

    pub(super) fn bind_property_worker(
        &self,
        node: &Node, /*PropertyDeclaration | PropertySignature*/
    ) -> Option<Rc<Symbol>> {
        self.bind_property_or_method_or_accessor(
            node,
            SymbolFlags::Property
                | if node
                    .as_has_question_token()
                    .maybe_question_token()
                    .is_some()
                {
                    SymbolFlags::Optional
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
        self.bind_anonymous_declaration(node, SymbolFlags::TypeLiteral, InternalSymbolName::Type())
    }

    pub(super) fn bind_source_file_if_external_module(&self) {
        let file = self.file();
        self.set_export_context_flag(&file);
        if is_external_module(&file) {
            self.bind_source_file_as_external_module();
        } else if is_json_source_file(&file) {
            self.bind_source_file_as_external_module();
            let original_symbol = file.symbol();
            self.declare_symbol(
                &mut file.symbol().exports().borrow_mut(),
                Some(file.symbol()),
                &file,
                SymbolFlags::Property,
                SymbolFlags::All,
                None,
                None,
            );
            file.set_symbol(original_symbol);
        }
    }

    pub(super) fn bind_source_file_as_external_module(&self) {
        self.bind_anonymous_declaration(
            &self.file(),
            SymbolFlags::ValueModule,
            __String::new(format!(
                "\"{}\"",
                remove_file_extension(&self.file().as_source_file().file_name())
            )),
        );
    }

    pub(super) fn bind_export_assignment(&self, node: &Node /*ExportAssignment*/) {
        if !matches!(self.container().maybe_symbol(), Some(symbol) if symbol.maybe_exports().is_some())
        {
            self.bind_anonymous_declaration(
                node,
                SymbolFlags::Value,
                self.get_declaration_name(node).unwrap(),
            );
        } else {
            let flags = if export_assignment_is_alias(node) {
                SymbolFlags::Alias
            } else {
                SymbolFlags::Property
            };
            let symbol = self.declare_symbol(
                &mut self.container().symbol().exports().borrow_mut(),
                Some(self.container().symbol()),
                node,
                flags,
                SymbolFlags::All,
                None,
                None,
            );

            if matches!(node.as_export_assignment().is_export_equals, Some(true)) {
                set_value_declaration(&symbol, node);
            }
        }
    }

    pub(super) fn bind_namespace_export_declaration(
        &self,
        node: &Node, /*NamespaceExportDeclaration*/
    ) {
        if matches!(node.maybe_modifiers().as_ref(), Some(modifiers) if !modifiers.is_empty()) {
            self.file()
                .as_source_file()
                .bind_diagnostics_mut()
                .push(Rc::new(
                    create_diagnostic_for_node(
                        node,
                        &Diagnostics::Modifiers_cannot_appear_here,
                        None,
                    )
                    .into(),
                ));
        }
        let diag = if !is_source_file(&node.parent()) {
            Some(&*Diagnostics::Global_module_exports_may_only_appear_at_top_level)
        } else if !is_external_module(&node.parent()) {
            Some(&*Diagnostics::Global_module_exports_may_only_appear_in_module_files)
        } else if !node.parent().as_source_file().is_declaration_file() {
            Some(&*Diagnostics::Global_module_exports_may_only_appear_in_declaration_files)
        } else {
            None
        };
        if let Some(diag) = diag {
            self.file()
                .as_source_file()
                .bind_diagnostics_mut()
                .push(Rc::new(create_diagnostic_for_node(node, diag, None).into()));
        } else {
            let file_symbol = self.file().symbol();
            let mut global_exports = file_symbol.maybe_global_exports();
            if global_exports.is_none() {
                *global_exports = Some(Rc::new(RefCell::new(create_symbol_table(None))));
                self.declare_symbol(
                    &mut global_exports.as_ref().unwrap().borrow_mut(),
                    Some(self.file().symbol()),
                    node,
                    SymbolFlags::Alias,
                    SymbolFlags::AliasExcludes,
                    None,
                    None,
                );
            }
        }
    }

    pub(super) fn bind_export_declaration(&self, node: &Node /*ExportDeclaration*/) {
        let node_as_export_declaration = node.as_export_declaration();
        if !matches!(self.container().maybe_symbol(), Some(symbol) if symbol.maybe_exports().is_some())
        {
            self.bind_anonymous_declaration(
                node,
                SymbolFlags::ExportStar,
                self.get_declaration_name(node).unwrap(),
            );
        } else if node_as_export_declaration.export_clause.is_none() {
            self.declare_symbol(
                &mut self.container().symbol().exports().borrow_mut(),
                Some(self.container().symbol()),
                node,
                SymbolFlags::ExportStar,
                SymbolFlags::None,
                None,
                None,
            );
        } else if is_namespace_export(node_as_export_declaration.export_clause.as_ref().unwrap()) {
            self.declare_symbol(
                &mut self.container().symbol().exports().borrow_mut(),
                Some(self.container().symbol()),
                node_as_export_declaration.export_clause.as_ref().unwrap(),
                SymbolFlags::Alias,
                SymbolFlags::AliasExcludes,
                None,
                None,
            );
        }
    }

    pub(super) fn bind_import_clause(&self, node: &Node /*ImportClause*/) {
        if node.as_import_clause().name.is_some() {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::Alias,
                SymbolFlags::AliasExcludes,
            );
        }
    }

    pub(super) fn set_common_js_module_indicator(&self, node: &Node) -> bool {
        let file = self.file();
        let file_as_source_file = file.as_source_file();
        if file_as_source_file
            .maybe_external_module_indicator()
            .is_some()
        {
            return false;
        }
        let mut file_common_js_module_indicator =
            file_as_source_file.maybe_common_js_module_indicator_mut();
        if file_common_js_module_indicator.is_none() {
            *file_common_js_module_indicator = Some(node.node_wrapper());
            self.bind_source_file_as_external_module();
        }
        true
    }

    pub(super) fn bind_object_define_property_export(
        &self,
        node: &Node, /*BindableObjectDefinePropertyCall*/
    ) {
        if !self.set_common_js_module_indicator(node) {
            return;
        }
        let symbol = self.for_each_identifier_in_entity_name(
            &node.as_call_expression().arguments[0],
            Option::<&Symbol>::None,
            &mut |id, symbol, _| {
                if let Some(symbol) = symbol.as_ref() {
                    self.add_declaration_to_symbol(
                        symbol,
                        id,
                        SymbolFlags::Module | SymbolFlags::Assignment,
                    );
                }
                symbol
            },
        );
        if let Some(symbol) = symbol {
            let flags = SymbolFlags::Property | SymbolFlags::ExportValue;
            self.declare_symbol(
                &mut symbol.exports().borrow_mut(),
                Some(symbol),
                node,
                flags,
                SymbolFlags::None,
                None,
                None,
            );
        }
    }

    pub(super) fn bind_exports_property_assignment(
        &self,
        node: &Node, /*BindableStaticPropertyAssignmentExpression*/
    ) {
        if !self.set_common_js_module_indicator(node) {
            return;
        }
        let node_as_binary_expression = node.as_binary_expression();
        let symbol = self.for_each_identifier_in_entity_name(
            &node_as_binary_expression
                .left
                .as_has_expression()
                .expression(),
            Option::<&Symbol>::None,
            &mut |id, symbol, _| {
                if let Some(symbol) = symbol.as_ref() {
                    self.add_declaration_to_symbol(
                        symbol,
                        id,
                        SymbolFlags::Module | SymbolFlags::Assignment,
                    );
                }
                symbol
            },
        );
        if let Some(symbol) = symbol {
            let is_alias = is_aliasable_expression(&node_as_binary_expression.right)
                && (is_exports_identifier(
                    &node_as_binary_expression
                        .left
                        .as_has_expression()
                        .expression(),
                ) || is_module_exports_access_expression(
                    &node_as_binary_expression
                        .left
                        .as_has_expression()
                        .expression(),
                ));
            let flags = if is_alias {
                SymbolFlags::Alias
            } else {
                SymbolFlags::Property | SymbolFlags::ExportValue
            };
            set_parent(&node_as_binary_expression.left, Some(node.node_wrapper()));
            self.declare_symbol(
                &mut symbol.exports().borrow_mut(),
                Some(symbol),
                &node_as_binary_expression.left,
                flags,
                SymbolFlags::None,
                None,
                None,
            );
        }
    }

    pub(super) fn bind_module_exports_assignment(
        &self,
        node: &Node, /*BindablePropertyAssignmentExpression*/
    ) {
        if !self.set_common_js_module_indicator(node) {
            return;
        }
        let node_as_binary_expression = node.as_binary_expression();
        let assigned_expression =
            get_right_most_assigned_expression(&node_as_binary_expression.right);
        if is_empty_object_literal(&assigned_expression)
            || Rc::ptr_eq(&self.container(), &self.file())
                && is_exports_or_module_exports_or_alias(&self.file(), &assigned_expression)
        {
            return;
        }

        if is_object_literal_expression(&assigned_expression) {
            let assigned_expression_as_object_literal_expression =
                assigned_expression.as_object_literal_expression();
            if every(
                &assigned_expression_as_object_literal_expression.properties,
                |property, _| is_shorthand_property_assignment(property),
            ) {
                for_each(
                    &assigned_expression_as_object_literal_expression.properties,
                    |property, _| {
                        self.bind_export_assigned_object_member_alias(property);
                        Option::<()>::None
                    },
                );
                return;
            }
        }

        let flags = if export_assignment_is_alias(node) {
            SymbolFlags::Alias
        } else {
            SymbolFlags::Property | SymbolFlags::ExportValue | SymbolFlags::ValueModule
        };
        let symbol = self.declare_symbol(
            &mut self.file().symbol().exports().borrow_mut(),
            Some(self.file().symbol()),
            node,
            flags | SymbolFlags::Assignment,
            SymbolFlags::None,
            None,
            None,
        );
        set_value_declaration(&symbol, node);
    }

    pub(super) fn bind_export_assigned_object_member_alias(
        &self,
        node: &Node, /*ShorthandPropertyAssignment*/
    ) {
        self.declare_symbol(
            &mut self.file().symbol().exports().borrow_mut(),
            Some(self.file().symbol()),
            node,
            SymbolFlags::Alias | SymbolFlags::Assignment,
            SymbolFlags::None,
            None,
            None,
        );
    }

    pub(super) fn bind_this_property_assignment(
        &self,
        node: &Node, /*BindablePropertyAssignmentExpression | PropertyAccessExpression | LiteralLikeElementAccessExpression*/
    ) {
        Debug_.assert(is_in_js_file(Some(node)), None);
        let has_private_identifier = is_binary_expression(node) && {
            let node_as_binary_expression = node.as_binary_expression();
            is_property_access_expression(&node_as_binary_expression.left)
                && is_private_identifier(
                    &node_as_binary_expression
                        .left
                        .as_property_access_expression()
                        .name,
                )
        } || is_property_access_expression(node)
            && is_private_identifier(&node.as_property_access_expression().name);
        if has_private_identifier {
            return;
        }
        let this_container = get_this_container(node, false);
        match this_container.kind() {
            SyntaxKind::FunctionDeclaration | SyntaxKind::FunctionExpression => {
                let mut constructor_symbol = this_container.maybe_symbol();
                if is_binary_expression(&this_container.parent()) {
                    let this_container_parent = this_container.parent();
                    let this_container_parent_as_binary_expression =
                        this_container_parent.as_binary_expression();
                    if this_container_parent_as_binary_expression
                        .operator_token
                        .kind()
                        == SyntaxKind::EqualsToken
                    {
                        let l = &this_container_parent_as_binary_expression.left;
                        if is_bindable_static_access_expression(l, None)
                            && is_prototype_access(&l.as_has_expression().expression())
                        {
                            constructor_symbol = self.lookup_symbol_for_property_access(
                                &l.as_has_expression()
                                    .expression()
                                    .as_has_expression()
                                    .expression(),
                                Some(self.this_parent_container()),
                            );
                        }
                    }
                }

                if let Some(constructor_symbol) = constructor_symbol {
                    if let Some(constructor_symbol_value_declaration) =
                        constructor_symbol.maybe_value_declaration()
                    {
                        let mut constructor_symbol_members = constructor_symbol.maybe_members();
                        if constructor_symbol_members.is_none() {
                            *constructor_symbol_members =
                                Some(Rc::new(RefCell::new(create_symbol_table(None))));
                        }
                        let mut constructor_symbol_members =
                            constructor_symbol_members.as_ref().unwrap().borrow_mut();
                        if has_dynamic_name(node) {
                            self.bind_dynamically_named_this_property_assignment(
                                node,
                                &constructor_symbol,
                                &mut constructor_symbol_members,
                            );
                        } else {
                            self.declare_symbol(
                                &mut constructor_symbol_members,
                                Some(constructor_symbol.clone()),
                                node,
                                SymbolFlags::Property | SymbolFlags::Assignment,
                                SymbolFlags::PropertyExcludes & !SymbolFlags::Property,
                                None,
                                None,
                            );
                        }
                        self.add_declaration_to_symbol(
                            &constructor_symbol,
                            &constructor_symbol_value_declaration,
                            SymbolFlags::Class,
                        );
                    }
                }
            }

            SyntaxKind::Constructor
            | SyntaxKind::PropertyDeclaration
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::ClassStaticBlockDeclaration => {
                let containing_class = this_container.parent();
                let symbol_table = if is_static(&this_container) {
                    containing_class.symbol().exports()
                } else {
                    containing_class.symbol().members()
                };
                let mut symbol_table = symbol_table.borrow_mut();
                if has_dynamic_name(node) {
                    self.bind_dynamically_named_this_property_assignment(
                        node,
                        &containing_class.symbol(),
                        &mut symbol_table,
                    );
                } else {
                    self.declare_symbol(
                        &mut symbol_table,
                        Some(containing_class.symbol()),
                        node,
                        SymbolFlags::Property | SymbolFlags::Assignment,
                        SymbolFlags::None,
                        Some(true),
                        None,
                    );
                }
            }
            SyntaxKind::SourceFile => {
                if has_dynamic_name(node) {
                    return;
                } else if this_container
                    .as_source_file()
                    .maybe_common_js_module_indicator()
                    .is_some()
                {
                    self.declare_symbol(
                        &mut this_container.symbol().exports().borrow_mut(),
                        Some(this_container.symbol()),
                        node,
                        SymbolFlags::Property | SymbolFlags::ExportValue,
                        SymbolFlags::None,
                        None,
                        None,
                    );
                } else {
                    self.declare_symbol_and_add_to_symbol_table(
                        node,
                        SymbolFlags::FunctionScopedVariable,
                        SymbolFlags::FunctionScopedVariableExcludes,
                    );
                }
            }

            _ => Debug_.fail_bad_syntax_kind(&this_container, None),
        }
    }

    pub(super) fn bind_dynamically_named_this_property_assignment(
        &self,
        node: &Node, /*BinaryExpression | DynamicNamedDeclaration*/
        symbol: &Symbol,
        symbol_table: &mut SymbolTable,
    ) {
        self.declare_symbol(
            symbol_table,
            Some(symbol),
            node,
            SymbolFlags::Property,
            SymbolFlags::None,
            Some(true),
            Some(true),
        );
        self.add_late_bound_assignment_declaration_to_symbol(node, Some(symbol));
    }

    pub(super) fn add_late_bound_assignment_declaration_to_symbol<TSymbol: Borrow<Symbol>>(
        &self,
        node: &Node, /*BinaryExpression | DynamicNamedDeclaration*/
        symbol: Option<TSymbol>,
    ) {
        if let Some(symbol) = symbol {
            let symbol = symbol.borrow();
            let mut symbol_assignment_declaration_members =
                symbol.maybe_assignment_declaration_members();
            if symbol_assignment_declaration_members.is_none() {
                *symbol_assignment_declaration_members = Some(HashMap::new());
            }
            symbol_assignment_declaration_members
                .as_mut()
                .unwrap()
                .insert(get_node_id(node), node.node_wrapper());
        }
    }

    pub(super) fn bind_special_property_declaration(
        &self,
        node: &Node, /*PropertyAccessExpression | LiteralLikeElementAccessExpression*/
    ) {
        let node_as_has_expression = node.as_has_expression();
        if node_as_has_expression.expression().kind() == SyntaxKind::ThisKeyword {
            self.bind_this_property_assignment(node);
        } else if is_bindable_static_access_expression(node, None)
            && node.parent().parent().kind() == SyntaxKind::SourceFile
        {
            if is_prototype_access(&node_as_has_expression.expression()) {
                self.bind_prototype_property_assignment(node, &node.parent());
            } else {
                self.bind_static_property_assignment(node);
            }
        }
    }
}
