#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{is_declaration_name_or_import_property_name, CheckMode, EmitResolverCreateResolver};
use crate::{
    add_related_info, concatenate, create_diagnostic_for_node, create_symbol_table,
    escape_leading_underscores, external_helpers_module_name_text, find_ancestor,
    get_all_accessor_declarations, get_ancestor, get_assignment_declaration_kind,
    get_combined_local_and_export_symbol_flags, get_containing_class, get_declaration_of_kind,
    get_external_module_import_equals_declaration_expression, get_external_module_name,
    get_host_signature_from_jsdoc, get_name_of_declaration, get_parameter_symbol_from_jsdoc,
    get_source_file_of_node, get_this_container, get_type_parameter_from_js_doc,
    has_syntactic_modifier, introduces_arguments_exotic_object, is_ambient_module,
    is_bindable_object_define_property_call, is_binding_pattern, is_call_expression, is_class_like,
    is_constructor_declaration, is_declaration_name, is_effective_external_module,
    is_element_access_expression, is_entity_name, is_entity_name_expression, is_export_assignment,
    is_expression_node, is_expression_with_type_arguments_in_class_extends_clause,
    is_external_module, is_external_module_import_equals_declaration, is_function_like,
    is_function_like_declaration, is_global_scope_augmentation, is_identifier, is_import_call,
    is_import_or_export_specifier, is_in_expression_context, is_in_js_file,
    is_indexed_access_type_node, is_interface_declaration, is_jsdoc_link_like,
    is_jsdoc_member_name, is_jsdoc_name_reference, is_jsx_tag_name,
    is_literal_computed_property_declaration_name, is_literal_import_type_node,
    is_literal_type_node, is_meta_property, is_named_declaration, is_private_identifier,
    is_private_identifier_class_element_declaration, is_property_declaration, is_qualified_name,
    is_require_call, is_right_side_of_qualified_name_or_property_access_or_jsdoc_member_name,
    is_static, is_string_literal, modifier_to_flag, node_can_be_decorated, node_is_missing,
    node_is_present, some, token_to_string, try_cast, AssignmentDeclarationKind, Debug_,
    Diagnostics, ExternalEmitHelpers, FindAncestorCallbackReturn, FunctionLikeDeclarationInterface,
    InternalSymbolName, ModifierFlags, NamedDeclarationInterface, NodeCheckFlags, NodeFlags,
    ObjectFlags, Signature, SymbolInterface, SymbolTable, SyntaxKind, TypeInterface, __String,
    bind_source_file, is_external_or_common_js_module, Diagnostic, EmitResolverDebuggable,
    IndexInfo, Node, NodeInterface, StringOrNumber, Symbol, SymbolFlags, Type, TypeChecker,
};

impl TypeChecker {
    pub(super) fn get_global_diagnostics(&self) -> Vec<Rc<Diagnostic>> {
        self.throw_if_non_diagnostics_producing();
        self.diagnostics().get_global_diagnostics()
    }

    pub(super) fn throw_if_non_diagnostics_producing(&self) {
        if !self.produce_diagnostics {
            unimplemented!()
        }
    }

    pub(super) fn get_symbols_in_scope_(
        &self,
        location: &Node,
        meaning: SymbolFlags,
    ) -> Vec<Rc<Symbol>> {
        if location.flags().intersects(NodeFlags::InWithStatement) {
            return vec![];
        }

        let mut symbols = create_symbol_table(None);
        let mut is_static_symbol = false;

        let mut location = Some(location.node_wrapper());
        self.populate_symbols(&mut location, meaning, &mut symbols, &mut is_static_symbol);

        symbols.remove(&InternalSymbolName::This());
        self.symbols_to_array(&symbols)
    }

    pub(super) fn populate_symbols(
        &self,
        location: &mut Option<Rc<Node>>,
        meaning: SymbolFlags,
        symbols: &mut SymbolTable,
        is_static_symbol: &mut bool,
    ) {
        while let Some(location_present) = location.as_ref() {
            if let Some(location_locals) = location_present.maybe_locals().clone() {
                if !self.is_global_source_file(location_present) {
                    self.copy_symbols(symbols, &(*location_locals).borrow(), meaning);
                }
            }

            match location_present.kind() {
                SyntaxKind::SourceFile => {
                    if is_external_module(location_present) {
                        self.copy_locally_visible_export_symbols(
                            symbols,
                            &(*self.get_symbol_of_node(location_present).unwrap().exports())
                                .borrow(),
                            meaning & SymbolFlags::ModuleMember,
                        );
                    }
                }
                SyntaxKind::ModuleDeclaration => {
                    self.copy_locally_visible_export_symbols(
                        symbols,
                        &(*self.get_symbol_of_node(location_present).unwrap().exports()).borrow(),
                        meaning & SymbolFlags::ModuleMember,
                    );
                }
                SyntaxKind::EnumDeclaration => {
                    self.copy_symbols(
                        symbols,
                        &(*self.get_symbol_of_node(location_present).unwrap().exports()).borrow(),
                        meaning & SymbolFlags::EnumMember,
                    );
                }
                SyntaxKind::ClassExpression => {
                    let class_name = location_present.as_class_expression().maybe_name();
                    if let Some(class_name) = class_name.as_ref() {
                        self.copy_symbol(symbols, &location_present.symbol(), meaning);
                    }

                    if !*is_static_symbol {
                        self.copy_symbols(
                            symbols,
                            &(*self.get_members_of_symbol(
                                &self.get_symbol_of_node(location_present).unwrap(),
                            ))
                            .borrow(),
                            meaning & SymbolFlags::Type,
                        );
                    }
                }
                SyntaxKind::ClassDeclaration | SyntaxKind::InterfaceDeclaration => {
                    if !*is_static_symbol {
                        self.copy_symbols(
                            symbols,
                            &(*self.get_members_of_symbol(
                                &self.get_symbol_of_node(location_present).unwrap(),
                            ))
                            .borrow(),
                            meaning & SymbolFlags::Type,
                        );
                    }
                }
                SyntaxKind::FunctionExpression => {
                    let func_name = location_present.as_function_expression().maybe_name();
                    if func_name.is_some() {
                        self.copy_symbol(symbols, &location_present.symbol(), meaning);
                    }
                }
                _ => (),
            }

            if introduces_arguments_exotic_object(location_present) {
                self.copy_symbol(symbols, &self.arguments_symbol(), meaning);
            }

            *is_static_symbol = is_static(location_present);
            *location = location_present.maybe_parent();
        }

        self.copy_symbols(symbols, &self.globals(), meaning);
    }

    pub(super) fn copy_symbol(
        &self,
        symbols: &mut SymbolTable,
        symbol: &Symbol,
        meaning: SymbolFlags,
    ) {
        if get_combined_local_and_export_symbol_flags(symbol).intersects(meaning) {
            let id = symbol.escaped_name();
            if !symbols.contains_key(id) {
                symbols.insert(id.clone(), symbol.symbol_wrapper());
            }
        }
    }

    pub(super) fn copy_symbols(
        &self,
        symbols: &mut SymbolTable,
        source: &SymbolTable,
        meaning: SymbolFlags,
    ) {
        if meaning != SymbolFlags::None {
            for symbol in source.values() {
                self.copy_symbol(symbols, symbol, meaning);
            }
        }
    }

    pub(super) fn copy_locally_visible_export_symbols(
        &self,
        symbols: &mut SymbolTable,
        source: &SymbolTable,
        meaning: SymbolFlags,
    ) {
        if meaning != SymbolFlags::None {
            for symbol in source.values() {
                if get_declaration_of_kind(symbol, SyntaxKind::ExportSpecifier).is_none()
                    && get_declaration_of_kind(symbol, SyntaxKind::NamespaceExport).is_none()
                {
                    self.copy_symbol(symbols, symbol, meaning);
                }
            }
        }
    }

    pub(super) fn is_type_declaration_name(&self, name: &Node) -> bool {
        name.kind() == SyntaxKind::Identifier
            && self.is_type_declaration(&name.parent())
            && matches!(
                get_name_of_declaration(name.maybe_parent()).as_ref(),
                Some(name_of_declaration) if ptr::eq(
                    &**name_of_declaration,
                    name
                )
            )
    }

    pub(super) fn is_type_declaration(&self, node: &Node) -> bool {
        match node.kind() {
            SyntaxKind::TypeParameter
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::InterfaceDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::EnumDeclaration
            | SyntaxKind::JSDocTypedefTag
            | SyntaxKind::JSDocCallbackTag
            | SyntaxKind::JSDocEnumTag => true,
            SyntaxKind::ImportClause => node.as_import_clause().is_type_only,
            SyntaxKind::ImportSpecifier | SyntaxKind::ExportSpecifier => {
                node.parent().parent().as_has_is_type_only().is_type_only()
            }
            _ => false,
        }
    }

    pub(super) fn is_type_reference_identifier(&self, node: &Node /*EntityName*/) -> bool {
        let mut node = node.node_wrapper();
        while node.parent().kind() == SyntaxKind::QualifiedName {
            node = node.parent();
        }

        node.parent().kind() == SyntaxKind::TypeReference
    }

    pub(super) fn is_heritage_clause_element_identifier(&self, node: &Node) -> bool {
        let mut node = node.node_wrapper();
        while node.parent().kind() == SyntaxKind::PropertyAccessExpression {
            node = node.parent();
        }

        node.parent().kind() == SyntaxKind::ExpressionWithTypeArguments
    }

    pub(super) fn for_each_enclosing_class<TReturn, TCallback: FnMut(&Node) -> Option<TReturn>>(
        &self,
        node: &Node,
        mut callback: TCallback,
    ) -> Option<TReturn> {
        let mut result: Option<TReturn> = None;

        let mut node = Some(node.node_wrapper());
        loop {
            node = get_containing_class(&node.as_ref().unwrap());
            if node.is_none() {
                break;
            }
            let node = node.as_ref().unwrap();
            result = callback(node);
            if result.is_some() {
                break;
            }
        }

        result
    }

    pub(super) fn for_each_enclosing_class_bool<TCallback: FnMut(&Node) -> bool>(
        &self,
        node: &Node,
        mut callback: TCallback,
    ) -> bool {
        let mut result = false;

        let mut node = Some(node.node_wrapper());
        loop {
            node = get_containing_class(&node.as_ref().unwrap());
            if node.is_none() {
                break;
            }
            let node = node.as_ref().unwrap();
            result = callback(node);
            if result {
                break;
            }
        }

        result
    }

    pub(super) fn is_node_used_during_class_initialization(&self, node: &Node) -> bool {
        find_ancestor(Some(node), |element| {
            if is_constructor_declaration(element)
                && node_is_present(element.as_constructor_declaration().maybe_body())
                || is_property_declaration(element)
            {
                return true.into();
            } else if is_class_like(element) || is_function_like_declaration(element) {
                return FindAncestorCallbackReturn::Quit;
            }

            false.into()
        })
        .is_some()
    }

    pub(super) fn is_node_within_class(
        &self,
        node: &Node,
        class_declaration: &Node, /*ClassLikeDeclaration*/
    ) -> bool {
        self.for_each_enclosing_class_bool(node, |n| ptr::eq(n, class_declaration))
    }

    pub(super) fn get_left_side_of_import_equals_or_export_assignment(
        &self,
        node_on_right_side: &Node, /*EntityName*/
    ) -> Option<Rc<Node /*ImportEqualsDeclaration | ExportAssignment*/>> {
        let mut node_on_right_side = node_on_right_side.node_wrapper();
        while node_on_right_side.parent().kind() == SyntaxKind::QualifiedName {
            node_on_right_side = node_on_right_side.parent();
        }

        if node_on_right_side.parent().kind() == SyntaxKind::ImportEqualsDeclaration {
            return if Rc::ptr_eq(
                &node_on_right_side
                    .parent()
                    .as_import_equals_declaration()
                    .module_reference,
                &node_on_right_side,
            ) {
                node_on_right_side.maybe_parent()
            } else {
                None
            };
        }

        if node_on_right_side.parent().kind() == SyntaxKind::ExportAssignment {
            return if Rc::ptr_eq(
                &node_on_right_side
                    .parent()
                    .as_export_assignment()
                    .expression,
                &node_on_right_side,
            ) {
                node_on_right_side.maybe_parent()
            } else {
                None
            };
        }

        None
    }

    pub(super) fn is_in_right_side_of_import_or_export_assignment(
        &self,
        node: &Node, /*EntityName*/
    ) -> bool {
        self.get_left_side_of_import_equals_or_export_assignment(node)
            .is_some()
    }

    pub(super) fn get_special_property_assignment_symbol_from_entity_name(
        &self,
        entity_name: &Node, /*EntityName | PropertyAccessExpression*/
    ) -> Option<Rc<Symbol>> {
        let special_property_assignment_kind =
            get_assignment_declaration_kind(&entity_name.parent().parent());
        match special_property_assignment_kind {
            AssignmentDeclarationKind::ExportsProperty
            | AssignmentDeclarationKind::PrototypeProperty => {
                self.get_symbol_of_node(&entity_name.parent())
            }
            AssignmentDeclarationKind::ThisProperty
            | AssignmentDeclarationKind::ModuleExports
            | AssignmentDeclarationKind::Property => {
                self.get_symbol_of_node(&entity_name.parent().parent())
            }
            _ => None,
        }
    }

    pub(super) fn is_import_type_qualifier_part(
        &self,
        node: &Node, /*EntityName*/
    ) -> Option<Rc<Node /*ImportTypeNode*/>> {
        let mut parent = node.parent();
        let mut node = node.node_wrapper();
        while is_qualified_name(&parent) {
            node = parent.clone();
            parent = parent.parent();
        }
        if
        /*parent &&*/
        parent.kind() == SyntaxKind::ImportType
            && matches!(
                parent.as_import_type_node().qualifier.as_ref(),
                Some(parent_qualifier) if Rc::ptr_eq(
                    parent_qualifier,
                    &node,
                )
            )
        {
            return Some(parent);
        }
        None
    }

    pub(super) fn get_symbol_of_name_or_property_access_expression(
        &self,
        name: &Node, /*EntityName | PrivateIdentifier | PropertyAccessExpression | JSDocMemberName*/
    ) -> Option<Rc<Symbol>> {
        if is_declaration_name(name) {
            return self.get_symbol_of_node(&name.parent());
        }

        if is_in_js_file(Some(name))
            && name.parent().kind() == SyntaxKind::PropertyAccessExpression
            && Rc::ptr_eq(
                &name.parent(),
                &name.parent().parent().as_binary_expression().left,
            )
        {
            if !is_private_identifier(name) && !is_jsdoc_member_name(name) {
                let special_property_assignment_symbol =
                    self.get_special_property_assignment_symbol_from_entity_name(name);
                if special_property_assignment_symbol.is_some() {
                    return special_property_assignment_symbol;
                }
            }
        }

        if name.parent().kind() == SyntaxKind::ExportAssignment && is_entity_name_expression(name) {
            let success = self.resolve_entity_name(
                name,
                SymbolFlags::Value
                    | SymbolFlags::Type
                    | SymbolFlags::Namespace
                    | SymbolFlags::Alias,
                Some(true),
                None,
                Option::<&Node>::None,
            );
            if matches!(
                success.as_ref(),
                Some(success) if !Rc::ptr_eq(
                    success,
                    &self.unknown_symbol()
                )
            ) {
                return success;
            }
        } else if is_entity_name(name) && self.is_in_right_side_of_import_or_export_assignment(name)
        {
            let import_equals_declaration =
                get_ancestor(Some(name), SyntaxKind::ImportEqualsDeclaration);
            Debug_.assert(import_equals_declaration.is_some(), None);
            return self.get_symbol_of_part_of_right_hand_side_of_import_equals(name, Some(true));
        }

        if is_entity_name(name) {
            let possible_import_node = self.is_import_type_qualifier_part(name);
            if let Some(possible_import_node) = possible_import_node.as_ref() {
                self.get_type_from_type_node_(possible_import_node);
                let sym = (*self.get_node_links(name))
                    .borrow()
                    .resolved_symbol
                    .clone();
                return sym.filter(|sym| !Rc::ptr_eq(sym, &self.unknown_symbol()));
            }
        }

        let mut name = name.node_wrapper();
        while is_right_side_of_qualified_name_or_property_access_or_jsdoc_member_name(&name) {
            name = name.parent();
        }
        let name = &name;

        if self.is_heritage_clause_element_identifier(name) {
            let mut meaning = SymbolFlags::None;
            if name.parent().kind() == SyntaxKind::ExpressionWithTypeArguments {
                meaning = SymbolFlags::Type;

                if is_expression_with_type_arguments_in_class_extends_clause(&name.parent()) {
                    meaning |= SymbolFlags::Value;
                }
            } else {
                meaning = SymbolFlags::Namespace;
            }

            meaning |= SymbolFlags::Alias;
            let entity_name_symbol = if is_entity_name_expression(name) {
                self.resolve_entity_name(name, meaning, None, None, Option::<&Node>::None)
            } else {
                None
            };
            if entity_name_symbol.is_some() {
                return entity_name_symbol;
            }
        }

        if name.parent().kind() == SyntaxKind::JSDocParameterTag {
            return get_parameter_symbol_from_jsdoc(&name.parent());
        }

        if name.parent().kind() == SyntaxKind::TypeParameter
            && name.parent().parent().kind() == SyntaxKind::JSDocTemplateTag
        {
            Debug_.assert(!is_in_js_file(Some(&**name)), None);
            let type_parameter = get_type_parameter_from_js_doc(&name.parent());
            return type_parameter.and_then(|type_parameter| type_parameter.maybe_symbol());
        }

        if is_expression_node(name) {
            if node_is_missing(Some(&**name)) {
                return None;
            }

            let is_jsdoc = find_ancestor(Some(&**name), |ancestor| {
                is_jsdoc_link_like(ancestor)
                    || is_jsdoc_name_reference(ancestor)
                    || is_jsdoc_member_name(ancestor)
            })
            .is_some();
            let meaning = if is_jsdoc {
                SymbolFlags::Type | SymbolFlags::Namespace | SymbolFlags::Value
            } else {
                SymbolFlags::Value
            };
            if name.kind() == SyntaxKind::Identifier {
                if is_jsx_tag_name(name) && self.is_jsx_intrinsic_identifier(name) {
                    let symbol = self.get_intrinsic_tag_symbol(&name.parent());
                    return if Rc::ptr_eq(&symbol, &self.unknown_symbol()) {
                        None
                    } else {
                        Some(symbol)
                    };
                }
                let result = self.resolve_entity_name(
                    name,
                    meaning,
                    Some(false),
                    Some(!is_jsdoc),
                    get_host_signature_from_jsdoc(name),
                );
                if result.is_none() && is_jsdoc {
                    let container = find_ancestor(Some(&**name), |ancestor| {
                        is_class_like(ancestor) || is_interface_declaration(ancestor)
                    });
                    if let Some(container) = container.as_ref() {
                        return self
                            .resolve_jsdoc_member_name(name, self.get_symbol_of_node(container));
                    }
                }
                return result;
            } else if is_private_identifier(name) {
                return self.get_symbol_for_private_identifier_expression(name);
            } else if matches!(
                name.kind(),
                SyntaxKind::PropertyAccessExpression | SyntaxKind::QualifiedName
            ) {
                let links = self.get_node_links(name);
                let links_resolved_symbol = (*links).borrow().resolved_symbol.clone();
                if links_resolved_symbol.is_some() {
                    return links_resolved_symbol;
                }

                if name.kind() == SyntaxKind::PropertyAccessExpression {
                    self.check_property_access_expression(name, Some(CheckMode::Normal));
                } else {
                    self.check_qualified_name(name, Some(CheckMode::Normal));
                }
                if (*links).borrow().resolved_symbol.is_none()
                    && is_jsdoc
                    && is_qualified_name(name)
                {
                    return self.resolve_jsdoc_member_name(name, Option::<&Symbol>::None);
                }
                return (*links).borrow().resolved_symbol.clone();
            } else if is_jsdoc_member_name(name) {
                return self.resolve_jsdoc_member_name(name, Option::<&Symbol>::None);
            }
        } else if self.is_type_reference_identifier(name) {
            let meaning = if name.parent().kind() == SyntaxKind::TypeReference {
                SymbolFlags::Type
            } else {
                SymbolFlags::Namespace
            };
            let symbol = self.resolve_entity_name(
                name,
                meaning,
                Some(false),
                Some(true),
                Option::<&Node>::None,
            );
            return if matches!(
                symbol.as_ref(),
                Some(symbol) if !Rc::ptr_eq(
                    symbol,
                    &self.unknown_symbol()
                )
            ) {
                symbol
            } else {
                Some(self.get_unresolved_symbol_for_entity_name(name))
            };
        }
        if name.parent().kind() == SyntaxKind::TypePredicate {
            return self.resolve_entity_name(
                name,
                SymbolFlags::FunctionScopedVariable,
                None,
                None,
                Option::<&Node>::None,
            );
        }

        None
    }

    pub(super) fn resolve_jsdoc_member_name<TContainer: Borrow<Symbol>>(
        &self,
        name: &Node, /*EntityName | JSDocMemberName*/
        container: Option<TContainer>,
    ) -> Option<Rc<Symbol>> {
        let container = container.map(|container| container.borrow().symbol_wrapper());
        if is_entity_name(name) {
            let meaning = SymbolFlags::Type | SymbolFlags::Namespace | SymbolFlags::Value;
            let mut symbol = self.resolve_entity_name(
                name,
                meaning,
                Some(false),
                Some(true),
                get_host_signature_from_jsdoc(name),
            );
            if symbol.is_none() && is_identifier(name) {
                if let Some(container) = container.as_ref() {
                    symbol = self.get_merged_symbol(self.get_symbol(
                        &(*self.get_exports_of_symbol(container)).borrow(),
                        &name.as_identifier().escaped_text,
                        meaning,
                    ));
                }
            }
            if symbol.is_some() {
                return symbol;
            }
        }
        let left = if is_identifier(name) {
            container.clone()
        } else {
            self.resolve_jsdoc_member_name(
                &name.as_has_left_and_right().left(),
                Option::<&Symbol>::None,
            )
        };
        let right = if is_identifier(name) {
            name.as_identifier().escaped_text.clone()
        } else {
            name.as_has_left_and_right()
                .right()
                .as_identifier()
                .escaped_text
                .clone()
        };
        if let Some(left) = left.as_ref() {
            let proto = if left.flags().intersects(SymbolFlags::Value) {
                self.get_property_of_type_(
                    &self.get_type_of_symbol(left),
                    &__String::new("prototype".to_owned()),
                    None,
                )
            } else {
                None
            };
            let t = if let Some(proto) = proto.as_ref() {
                self.get_type_of_symbol(proto)
            } else {
                self.get_declared_type_of_symbol(left)
            };
            return self.get_property_of_type_(&t, &right, None);
        }
        None
    }

    pub(super) fn get_symbol_at_location_(
        &self,
        node: &Node,
        ignore_errors: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        if node.kind() == SyntaxKind::SourceFile {
            return if is_external_module(node) {
                self.get_merged_symbol(node.maybe_symbol())
            } else {
                None
            };
        }
        let ref parent = node.parent();
        let ref grand_parent = parent.parent();

        if node.flags().intersects(NodeFlags::InWithStatement) {
            return None;
        }

        if is_declaration_name_or_import_property_name(node) {
            let parent_symbol = self.get_symbol_of_node(parent);
            return if is_import_or_export_specifier(&node.parent())
                && matches!(
                    node.parent().as_has_property_name().maybe_property_name().as_ref(),
                    Some(node_parent_property_name) if ptr::eq(
                        &**node_parent_property_name,
                        node
                    )
                ) {
                self.get_immediate_aliased_symbol(&parent_symbol.unwrap())
            } else {
                parent_symbol
            };
        } else if is_literal_computed_property_declaration_name(node) {
            return self.get_symbol_of_node(&parent.parent());
        }

        if node.kind() == SyntaxKind::Identifier {
            if self.is_in_right_side_of_import_or_export_assignment(node) {
                return self.get_symbol_of_name_or_property_access_expression(node);
            } else if parent.kind() == SyntaxKind::BindingElement
                && grand_parent.kind() == SyntaxKind::ObjectBindingPattern
                && matches!(
                    parent.as_binding_element().property_name.as_ref(),
                    Some(parent_property_name) if ptr::eq(
                        node,
                        &**parent_property_name
                    )
                )
            {
                let ref type_of_pattern = self.get_type_of_node(grand_parent);
                let property_declaration = self.get_property_of_type_(
                    type_of_pattern,
                    &node.as_identifier().escaped_text,
                    None,
                );

                if property_declaration.is_some() {
                    return property_declaration;
                }
            } else if is_meta_property(parent) {
                let ref parent_type = self.get_type_of_node(parent);
                let property_declaration = self.get_property_of_type_(
                    parent_type,
                    &node.as_identifier().escaped_text,
                    None,
                );
                if property_declaration.is_some() {
                    return property_declaration;
                }
                if parent.as_meta_property().keyword_token == SyntaxKind::NewKeyword {
                    return self.check_new_target_meta_property(parent).maybe_symbol();
                }
            }
        }

        match node.kind() {
            SyntaxKind::Identifier
            | SyntaxKind::PrivateIdentifier
            | SyntaxKind::PropertyAccessExpression
            | SyntaxKind::QualifiedName => {
                self.get_symbol_of_name_or_property_access_expression(node)
            }
            SyntaxKind::ThisKeyword => {
                let ref container = get_this_container(node, false);
                if is_function_like(Some(&**container)) {
                    let sig = self.get_signature_from_declaration_(container);
                    let sig_this_parameter = sig.maybe_this_parameter().clone();
                    if sig_this_parameter.is_some() {
                        return sig_this_parameter;
                    }
                }
                if is_in_expression_context(node) {
                    return self.check_expression(node, None, None).maybe_symbol();
                }

                self.get_type_from_this_type_node(node).maybe_symbol()
            }

            SyntaxKind::ThisType => self.get_type_from_this_type_node(node).maybe_symbol(),

            SyntaxKind::SuperKeyword => self.check_expression(node, None, None).maybe_symbol(),

            SyntaxKind::ConstructorKeyword => {
                let constructor_declaration = node.maybe_parent();
                if let Some(constructor_declaration) =
                    constructor_declaration
                        .as_ref()
                        .filter(|constructor_declaration| {
                            constructor_declaration.kind() == SyntaxKind::Constructor
                        })
                {
                    return constructor_declaration.parent().maybe_symbol();
                }
                None
            }

            SyntaxKind::StringLiteral | SyntaxKind::NoSubstitutionTemplateLiteral => {
                if is_external_module_import_equals_declaration(&node.parent().parent())
                    && ptr::eq(
                        &*get_external_module_import_equals_declaration_expression(
                            &node.parent().parent(),
                        ),
                        node,
                    )
                    || matches!(
                        node.parent().kind(),
                        SyntaxKind::ImportDeclaration | SyntaxKind::ExportDeclaration
                    ) && matches!(
                        node.parent().as_has_module_specifier().maybe_module_specifier().as_ref(),
                        Some(node_parent_module_specifier) if ptr::eq(
                            &**node_parent_module_specifier,
                            node,
                        )
                    )
                    || (is_in_js_file(Some(node)) && is_require_call(&node.parent(), false)
                        || is_import_call(&node.parent()))
                    || is_literal_type_node(&node.parent())
                        && is_literal_import_type_node(&node.parent().parent())
                        && Rc::ptr_eq(
                            &node.parent().parent().as_import_type_node().argument,
                            &node.parent(),
                        )
                {
                    return self.resolve_external_module_name_(node, node, ignore_errors);
                }
                if is_call_expression(parent)
                    && is_bindable_object_define_property_call(parent)
                    && ptr::eq(&*parent.as_call_expression().arguments[1], node)
                {
                    return self.get_symbol_of_node(parent);
                }

                let object_type = if is_element_access_expression(parent) {
                    let parent_as_element_access_expression = parent.as_element_access_expression();
                    if ptr::eq(
                        &*parent_as_element_access_expression.argument_expression,
                        node,
                    ) {
                        Some(self.get_type_of_expression(
                            &parent_as_element_access_expression.expression,
                        ))
                    } else {
                        None
                    }
                } else if is_literal_type_node(parent) && is_indexed_access_type_node(grand_parent)
                {
                    Some(self.get_type_from_type_node_(
                        &grand_parent.as_indexed_access_type_node().object_type,
                    ))
                } else {
                    None
                };
                object_type.as_ref().and_then(|object_type| {
                    self.get_property_of_type_(
                        object_type,
                        &escape_leading_underscores(&node.as_literal_like_node().text()),
                        None,
                    )
                })
            }

            SyntaxKind::NumericLiteral => {
                let object_type = if is_element_access_expression(parent) {
                    let parent_as_element_access_expression = parent.as_element_access_expression();
                    if ptr::eq(
                        &*parent_as_element_access_expression.argument_expression,
                        node,
                    ) {
                        Some(self.get_type_of_expression(
                            &parent_as_element_access_expression.expression,
                        ))
                    } else {
                        None
                    }
                } else if is_literal_type_node(parent) && is_indexed_access_type_node(grand_parent)
                {
                    Some(self.get_type_from_type_node_(
                        &grand_parent.as_indexed_access_type_node().object_type,
                    ))
                } else {
                    None
                };
                object_type.as_ref().and_then(|object_type| {
                    self.get_property_of_type_(
                        object_type,
                        &escape_leading_underscores(&node.as_literal_like_node().text()),
                        None,
                    )
                })
            }

            SyntaxKind::DefaultKeyword
            | SyntaxKind::FunctionKeyword
            | SyntaxKind::EqualsGreaterThanToken
            | SyntaxKind::ClassKeyword => self.get_symbol_of_node(&node.parent()),
            SyntaxKind::ImportType => {
                if is_literal_import_type_node(node) {
                    self.get_symbol_at_location_(
                        &node
                            .as_import_type_node()
                            .argument
                            .as_literal_type_node()
                            .literal,
                        None,
                    )
                } else {
                    None
                }
            }

            SyntaxKind::ExportKeyword => {
                if is_export_assignment(&node.parent()) {
                    Some(Debug_.check_defined(node.parent().maybe_symbol(), None))
                } else {
                    None
                }
            }

            SyntaxKind::ImportKeyword | SyntaxKind::NewKeyword => {
                if is_meta_property(&node.parent()) {
                    self.check_meta_property_keyword(&node.parent())
                        .maybe_symbol()
                } else {
                    None
                }
            }
            SyntaxKind::MetaProperty => self.check_expression(node, None, None).maybe_symbol(),

            _ => None,
        }
    }

    pub(super) fn get_index_infos_at_location_(&self, node: &Node) -> Option<Vec<Rc<IndexInfo>>> {
        unimplemented!()
    }

    pub(super) fn get_shorthand_assignment_value_symbol_<TNode: Borrow<Node>>(
        &self,
        location: Option<TNode>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_export_specifier_local_target_symbol_(
        &self,
        node: &Node, /*Identifier | ExportSpecifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_node(&self, node: &Node) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_of_assignment_pattern_(
        &self,
        expr: &Node, /*AssignmentPattern*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_property_symbol_of_destructuring_assignment_(
        &self,
        location: &Node, /*Identifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_parent_type_of_class_element(
        &self,
        node: &Node, /*ClassElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_class_element_property_key_type(
        &self,
        element: &Node, /*ClassElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_augmented_properties_of_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn type_has_call_or_construct_signatures(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_const_enum_or_const_enum_only_module(&self, s: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn is_referenced_alias_declaration(
        &self,
        node: &Node,
        check_children: Option<bool>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_implementation_of_overload_(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_node_check_flags(&self, node: &Node) -> NodeCheckFlags {
        unimplemented!()
    }

    pub(super) fn get_enum_member_value(
        &self,
        node: &Node, /*EnumMember*/
    ) -> Option<StringOrNumber> {
        unimplemented!()
    }

    pub(super) fn can_have_constant_value(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn get_constant_value_(
        &self,
        node: &Node, /*EnumMember | AccessExpression*/
    ) -> Option<StringOrNumber> {
        unimplemented!()
    }

    pub(super) fn is_function_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_referenced_value_symbol(
        &self,
        reference: &Node, /*Identifier*/
        start_in_declaration_container: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_factory_entity(
        &self,
        location: &Node,
    ) -> Option<Rc<Node /*EntityName*/>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_fragment_factory_entity(
        &self,
        location: &Node,
    ) -> Option<Rc<Node /*EntityName*/>> {
        unimplemented!()
    }

    pub(super) fn create_resolver(&self) -> Rc<dyn EmitResolverDebuggable> {
        Rc::new(EmitResolverCreateResolver::new())
    }

    pub(super) fn get_external_module_file_from_declaration(
        &self,
        declaration: &Node, /*AnyImportOrReExport | ModuleDeclaration | ImportTypeNode | ImportCall*/
    ) -> Option<Rc<Node /*SourceFile*/>> {
        let specifier = if declaration.kind() == SyntaxKind::ModuleDeclaration {
            try_cast(
                declaration.as_module_declaration().name(),
                |node: &Rc<Node>| is_string_literal(node),
            )
        } else {
            get_external_module_name(declaration)
        };
        let module_symbol = self.resolve_external_module_name_worker(
            specifier.as_ref().unwrap(),
            specifier.as_ref().unwrap(),
            None,
            None,
        )?;
        get_declaration_of_kind(&module_symbol, SyntaxKind::SourceFile)
    }

    pub(super) fn initialize_type_checker(&mut self) {
        for file in self.host.get_source_files() {
            bind_source_file(file, self.compiler_options.clone());
            // println!("post-binding: {:#?}", file);
        }

        *self.maybe_amalgamated_duplicates() = Some(HashMap::new());

        let mut augmentations: Option<Vec<Vec<Rc<Node /*StringLiteral | Identifier*/>>>> = None;
        for file in self.host.get_source_files() {
            let file_as_source_file = file.as_source_file();
            if file_as_source_file.maybe_redirect_info().is_some() {
                continue;
            }
            if !is_external_or_common_js_module(file) {
                let file_global_this_symbol = (**file.locals())
                    .borrow()
                    .get(&__String::new("globalThis".to_owned()))
                    .cloned();
                if let Some(ref file_global_this_symbol_declarations) = file_global_this_symbol
                    .as_ref()
                    .and_then(|file_global_this_symbol| {
                        file_global_this_symbol.maybe_declarations().clone()
                    })
                {
                    for declaration in file_global_this_symbol_declarations {
                        self.diagnostics().add(
                            Rc::new(
                                create_diagnostic_for_node(
                                    declaration,
                                    &Diagnostics::Declaration_name_conflicts_with_built_in_global_identifier_0,
                                    Some(vec![
                                        "globalThis".to_owned()
                                    ])
                                ).into()
                            )
                        );
                    }
                }
                self.merge_symbol_table(
                    &mut *self.globals_mut(),
                    &RefCell::borrow(&file.locals()),
                    None,
                );
            }
            if let Some(file_js_global_augmentations) =
                file_as_source_file.maybe_js_global_augmentations().clone()
            {
                self.merge_symbol_table(
                    &mut *self.globals_mut(),
                    &(*file_js_global_augmentations).borrow(),
                    None,
                );
            }
            if let Some(file_pattern_ambient_modules) = file_as_source_file
                .maybe_pattern_ambient_modules()
                .as_ref()
                .filter(|file_pattern_ambient_modules| !file_pattern_ambient_modules.is_empty())
            {
                let mut pattern_ambient_modules = self.maybe_pattern_ambient_modules();
                *pattern_ambient_modules = Some(concatenate(
                    pattern_ambient_modules.clone().unwrap_or_else(|| vec![]),
                    file_pattern_ambient_modules.clone(),
                ));
            }
            let file_module_augmentations = file_as_source_file.maybe_module_augmentations();
            // TODO: this should end up being .unwrap()'able
            // let file_module_augmentations = file_module_augmentations.as_ref().unwrap();
            let file_module_augmentations =
                file_module_augmentations.clone().unwrap_or_else(|| vec![]);
            let file_module_augmentations = &file_module_augmentations;
            if !file_module_augmentations.is_empty() {
                if augmentations.is_none() {
                    augmentations = Some(vec![]);
                }
                augmentations
                    .as_mut()
                    .unwrap()
                    .push(file_module_augmentations.clone());
            }
            if let Some(file_symbol_global_exports) = file
                .maybe_symbol()
                .as_ref()
                .and_then(|file_symbol| file_symbol.maybe_global_exports().clone())
            {
                let source = file_symbol_global_exports;
                let mut globals = self.globals_mut();
                for (id, source_symbol) in &*(*source).borrow() {
                    if !globals.contains_key(id) {
                        globals.insert(id.clone(), source_symbol.clone());
                    }
                }
            }
        }

        if let Some(augmentations) = augmentations.as_ref() {
            for list in augmentations {
                for augmentation in list {
                    if !is_global_scope_augmentation(&augmentation.parent()) {
                        continue;
                    }
                    self.merge_module_augmentation(augmentation);
                }
            }
        }

        self.add_to_symbol_table(
            &mut *self.globals_mut(),
            &self.builtin_globals(),
            &Diagnostics::Declaration_name_conflicts_with_built_in_global_identifier_0,
        );

        self.get_symbol_links(&self.undefined_symbol())
            .borrow_mut()
            .type_ = Some(self.undefined_widening_type());
        self.get_symbol_links(&self.arguments_symbol())
            .borrow_mut()
            .type_ = self.get_global_type(&__String::new("IArguments".to_owned()), 0, true);
        self.get_symbol_links(&self.unknown_symbol())
            .borrow_mut()
            .type_ = Some(self.error_type());
        self.get_symbol_links(&self.global_this_symbol())
            .borrow_mut()
            .type_ = Some(Rc::new(
            self.create_object_type(ObjectFlags::Anonymous, Some(self.global_this_symbol()))
                .into(),
        ));

        self.global_array_type = self.get_global_type(&__String::new("Array".to_owned()), 1, true);
        self.global_object_type =
            self.get_global_type(&__String::new("Object".to_owned()), 0, true);
        self.global_function_type =
            self.get_global_type(&__String::new("Function".to_owned()), 0, true);
        self.global_callable_function_type = Some(
            if self.strict_bind_call_apply {
                self.get_global_type(&__String::new("CallableFunction".to_owned()), 0, true)
            } else {
                None
            }
            .unwrap_or_else(|| self.global_function_type()),
        );
        self.global_newable_function_type = Some(
            if self.strict_bind_call_apply {
                self.get_global_type(&__String::new("NewableFunction".to_owned()), 0, true)
            } else {
                None
            }
            .unwrap_or_else(|| self.global_function_type()),
        );
        self.global_string_type =
            self.get_global_type(&__String::new("String".to_owned()), 0, true);
        self.global_number_type =
            self.get_global_type(&__String::new("Number".to_owned()), 0, true);
        self.global_boolean_type =
            self.get_global_type(&__String::new("Boolean".to_owned()), 0, true);
        self.global_reg_exp_type =
            self.get_global_type(&__String::new("RegExp".to_owned()), 0, true);
        self.any_array_type = Some(self.create_array_type(&self.any_type(), None));

        self.auto_array_type = Some(self.create_array_type(&self.auto_type(), None));
        if Rc::ptr_eq(&self.auto_array_type(), &self.empty_object_type()) {
            self.auto_array_type = Some(Rc::new(
                self.create_anonymous_type(
                    Option::<&Symbol>::None,
                    self.empty_symbols(),
                    vec![],
                    vec![],
                    vec![],
                )
                .into(),
            ));
        }

        self.global_readonly_array_type = self
            .get_global_type_or_undefined(&__String::new("ReadonlyArray".to_owned()), Some(1))
            .or_else(|| self.global_array_type.clone());
        self.any_readonly_array_type = Some(
            if let Some(global_readonly_array_type) = self.global_readonly_array_type.as_ref() {
                self.create_type_from_generic_global_type(
                    global_readonly_array_type,
                    vec![self.any_type()],
                )
            } else {
                self.any_array_type()
            },
        );
        self.global_this_type =
            self.get_global_type_or_undefined(&__String::new("ThisType".to_owned()), Some(1));

        if let Some(augmentations) = augmentations.as_ref() {
            for list in augmentations {
                for augmentation in list {
                    if is_global_scope_augmentation(&augmentation.parent()) {
                        continue;
                    }
                    self.merge_module_augmentation(augmentation);
                }
            }
        }

        for duplicate_info_for_files in self
            .maybe_amalgamated_duplicates()
            .as_ref()
            .unwrap()
            .values()
        {
            let first_file = &duplicate_info_for_files.first_file;
            let second_file = &duplicate_info_for_files.second_file;
            let conflicting_symbols = &duplicate_info_for_files.conflicting_symbols;
            if conflicting_symbols.len() < 8 {
                for (symbol_name, duplicate_info_for_symbol) in conflicting_symbols {
                    let is_block_scoped = duplicate_info_for_symbol.is_block_scoped;
                    let first_file_locations = &duplicate_info_for_symbol.first_file_locations;
                    let second_file_locations = &duplicate_info_for_symbol.second_file_locations;
                    let message = if is_block_scoped {
                        &*Diagnostics::Cannot_redeclare_block_scoped_variable_0
                    } else {
                        &*Diagnostics::Duplicate_identifier_0
                    };
                    for node in first_file_locations {
                        self.add_duplicate_declaration_error(
                            node,
                            message,
                            symbol_name,
                            Some(second_file_locations),
                        );
                    }
                    for node in second_file_locations {
                        self.add_duplicate_declaration_error(
                            node,
                            message,
                            symbol_name,
                            Some(first_file_locations),
                        );
                    }
                }
            } else {
                let list: String = conflicting_symbols
                    .keys()
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", ");
                self.diagnostics().add(
                    {
                        let diagnostic: Rc<Diagnostic> = Rc::new(
                            create_diagnostic_for_node(
                                first_file,
                                &Diagnostics::Definitions_of_the_following_identifiers_conflict_with_those_in_another_file_Colon_0,
                                Some(vec![
                                    list.clone()
                                ])
                            ).into()
                        );
                        add_related_info(
                            &diagnostic,
                            vec![
                                Rc::new(
                                    create_diagnostic_for_node(
                                        second_file,
                                        &Diagnostics::Conflicts_are_in_this_file,
                                        None,
                                    ).into()
                                )
                            ]
                        );
                        diagnostic
                    }
                );
                self.diagnostics().add(
                    {
                        let diagnostic: Rc<Diagnostic> = Rc::new(
                            create_diagnostic_for_node(
                                second_file,
                                &Diagnostics::Definitions_of_the_following_identifiers_conflict_with_those_in_another_file_Colon_0,
                                Some(vec![
                                    list.clone()
                                ])
                            ).into()
                        );
                        add_related_info(
                            &diagnostic,
                            vec![
                                Rc::new(
                                    create_diagnostic_for_node(
                                        first_file,
                                        &Diagnostics::Conflicts_are_in_this_file,
                                        None,
                                    ).into()
                                )
                            ]
                        );
                        diagnostic
                    }
                );
            }
        }
        *self.maybe_amalgamated_duplicates() = None;
    }

    pub(super) fn check_external_emit_helpers(
        &self,
        location: &Node,
        helpers: ExternalEmitHelpers,
    ) {
        if self.requested_external_emit_helpers() & helpers != helpers
            && self.compiler_options.import_helpers == Some(true)
        {
            let source_file = get_source_file_of_node(Some(location)).unwrap();
            if is_effective_external_module(&source_file, &self.compiler_options)
                && !location.flags().intersects(NodeFlags::Ambient)
            {
                let helpers_module = self.resolve_helpers_module(&source_file, location);
                if !Rc::ptr_eq(&helpers_module, &self.unknown_symbol()) {
                    let unchecked_helpers = helpers & !self.requested_external_emit_helpers();
                    let mut helper = ExternalEmitHelpers::FirstEmitHelper;
                    while helper <= ExternalEmitHelpers::LastEmitHelper {
                        if unchecked_helpers.intersects(helper) {
                            let name = self.get_helper_name(helper);
                            let symbol = self.get_symbol(
                                &(*helpers_module.maybe_exports().clone().unwrap()).borrow(),
                                &escape_leading_underscores(name),
                                SymbolFlags::Value,
                            );
                            match symbol.as_ref() {
                                None => {
                                    self.error(
                                        Some(location),
                                        &Diagnostics::This_syntax_requires_an_imported_helper_named_1_which_does_not_exist_in_0_Consider_upgrading_your_version_of_0,
                                        Some(vec![
                                            external_helpers_module_name_text.to_owned(),
                                            name.to_owned(),
                                        ])
                                    );
                                }
                                Some(symbol) => {
                                    if helper.intersects(ExternalEmitHelpers::ClassPrivateFieldGet)
                                    {
                                        if !some(
                                            Some(&*self.get_signatures_of_symbol(Some(&**symbol))),
                                            Some(|signature: &Rc<Signature>| {
                                                self.get_parameter_count(signature) > 3
                                            }),
                                        ) {
                                            self.error(
                                                Some(location),
                                                &Diagnostics::This_syntax_requires_an_imported_helper_named_1_with_2_parameters_which_is_not_compatible_with_the_one_in_0_Consider_upgrading_your_version_of_0,
                                                Some(vec![
                                                    external_helpers_module_name_text.to_owned(),
                                                    name.to_owned(),
                                                    4_usize.to_string(),
                                                ])
                                            );
                                        }
                                    } else if helper
                                        .intersects(ExternalEmitHelpers::ClassPrivateFieldSet)
                                    {
                                        if !some(
                                            Some(&*self.get_signatures_of_symbol(Some(&**symbol))),
                                            Some(|signature: &Rc<Signature>| {
                                                self.get_parameter_count(signature) > 4
                                            }),
                                        ) {
                                            self.error(
                                                Some(location),
                                                &Diagnostics::This_syntax_requires_an_imported_helper_named_1_with_2_parameters_which_is_not_compatible_with_the_one_in_0_Consider_upgrading_your_version_of_0,
                                                Some(vec![
                                                    external_helpers_module_name_text.to_owned(),
                                                    name.to_owned(),
                                                    5_usize.to_string(),
                                                ])
                                            );
                                        }
                                    } else if helper.intersects(ExternalEmitHelpers::SpreadArray) {
                                        if !some(
                                            Some(&*self.get_signatures_of_symbol(Some(&**symbol))),
                                            Some(|signature: &Rc<Signature>| {
                                                self.get_parameter_count(signature) > 2
                                            }),
                                        ) {
                                            self.error(
                                                Some(location),
                                                &Diagnostics::This_syntax_requires_an_imported_helper_named_1_with_2_parameters_which_is_not_compatible_with_the_one_in_0_Consider_upgrading_your_version_of_0,
                                                Some(vec![
                                                    external_helpers_module_name_text.to_owned(),
                                                    name.to_owned(),
                                                    3_usize.to_string(),
                                                ])
                                            );
                                        }
                                    }
                                }
                            }
                        }
                        // helper <<= 1;
                        helper = ExternalEmitHelpers::from_bits(helper.bits() << 1).unwrap();
                    }
                }
                self.set_requested_external_emit_helpers(
                    self.requested_external_emit_helpers() | helpers,
                );
            }
        }
    }

    pub(super) fn get_helper_name(&self, helper: ExternalEmitHelpers) -> &'static str {
        match helper {
            ExternalEmitHelpers::Extends => "__extends",
            ExternalEmitHelpers::Assign => "__assign",
            ExternalEmitHelpers::Rest => "__rest",
            ExternalEmitHelpers::Decorate => "__decorate",
            ExternalEmitHelpers::Metadata => "__metadata",
            ExternalEmitHelpers::Param => "__param",
            ExternalEmitHelpers::Awaiter => "__awaiter",
            ExternalEmitHelpers::Generator => "__generator",
            ExternalEmitHelpers::Values => "__values",
            ExternalEmitHelpers::Read => "__read",
            ExternalEmitHelpers::SpreadArray => "__spreadArray",
            ExternalEmitHelpers::Await => "__await",
            ExternalEmitHelpers::AsyncGenerator => "__asyncGenerator",
            ExternalEmitHelpers::AsyncDelegator => "__asyncDelegator",
            ExternalEmitHelpers::AsyncValues => "__asyncValues",
            ExternalEmitHelpers::ExportStar => "__exportStar",
            ExternalEmitHelpers::ImportStar => "__importStar",
            ExternalEmitHelpers::ImportDefault => "__importDefault",
            ExternalEmitHelpers::MakeTemplateObject => "__makeTemplateObject",
            ExternalEmitHelpers::ClassPrivateFieldGet => "__classPrivateFieldGet",
            ExternalEmitHelpers::ClassPrivateFieldSet => "__classPrivateFieldSet",
            ExternalEmitHelpers::ClassPrivateFieldIn => "__classPrivateFieldIn",
            ExternalEmitHelpers::CreateBinding => "__createBinding",
            _ => Debug_.fail(Some("Unrecognized helper")),
        }
    }

    pub(super) fn resolve_helpers_module(
        &self,
        node: &Node, /*SourceFile*/
        error_node: &Node,
    ) -> Rc<Symbol> {
        let mut external_helpers_module = self.maybe_external_helpers_module();
        if external_helpers_module.is_none() {
            *external_helpers_module = Some(
                self.resolve_external_module(
                    node,
                    external_helpers_module_name_text,
                    Some(&Diagnostics::This_syntax_requires_an_imported_helper_but_module_0_cannot_be_found),
                    error_node,
                    None,
                ).unwrap_or_else(|| self.unknown_symbol())
            );
        }
        external_helpers_module.clone().unwrap()
    }

    pub(super) fn check_grammar_decorators_and_modifiers(&self, node: &Node) -> bool {
        self.check_grammar_decorators(node) || self.check_grammar_modifiers(node)
    }

    pub(super) fn check_grammar_decorators(&self, node: &Node) -> bool {
        let node_decorators = node.maybe_decorators();
        if node_decorators.is_none() {
            return false;
        }
        let node_decorators = node_decorators.as_ref().unwrap();
        if !node_can_be_decorated(node, Some(node.parent()), node.parent().maybe_parent()) {
            if node.kind() == SyntaxKind::MethodDeclaration
                && !node_is_present(node.as_method_declaration().maybe_body())
            {
                return self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::A_decorator_can_only_decorate_a_method_implementation_not_an_overload,
                    None,
                );
            } else {
                return self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::Decorators_are_not_valid_here,
                    None,
                );
            }
        } else if matches!(
            node.kind(),
            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor
        ) {
            let accessors = get_all_accessor_declarations(
                node.parent().as_class_like_declaration().members(),
                node,
            );
            if accessors.first_accessor.maybe_decorators().is_some()
                && matches!(
                    accessors.second_accessor.as_ref(),
                    Some(accessors_second_accessor) if ptr::eq(
                        node,
                        &**accessors_second_accessor
                    )
                )
            {
                return self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::Decorators_cannot_be_applied_to_multiple_get_Slashset_accessors_of_the_same_name,
                    None,
                );
            }
        }
        false
    }

    pub(super) fn check_grammar_modifiers(&self, node: &Node) -> bool {
        let quick_result = self.report_obvious_modifier_errors(node);
        if let Some(quick_result) = quick_result {
            return quick_result;
        }

        let mut last_static: Option<Rc<Node>> = None;
        let mut last_declare: Option<Rc<Node>> = None;
        let mut last_async: Option<Rc<Node>> = None;
        let mut last_readonly: Option<Rc<Node>> = None;
        let mut last_override: Option<Rc<Node>> = None;
        let mut flags = ModifierFlags::None;
        for modifier in node.maybe_modifiers().as_ref().unwrap() {
            if modifier.kind() != SyntaxKind::ReadonlyKeyword {
                if matches!(
                    node.kind(),
                    SyntaxKind::PropertySignature | SyntaxKind::MethodSignature
                ) {
                    return self.grammar_error_on_node(
                        modifier,
                        &Diagnostics::_0_modifier_cannot_appear_on_a_type_member,
                        Some(vec![token_to_string(modifier.kind()).unwrap().to_owned()]),
                    );
                }
                if node.kind() == SyntaxKind::IndexSignature
                    && (modifier.kind() != SyntaxKind::StaticKeyword
                        || !is_class_like(&node.parent()))
                {
                    return self.grammar_error_on_node(
                        modifier,
                        &Diagnostics::_0_modifier_cannot_appear_on_an_index_signature,
                        Some(vec![token_to_string(modifier.kind()).unwrap().to_owned()]),
                    );
                }
            }
            match modifier.kind() {
                SyntaxKind::ConstKeyword => {
                    if node.kind() != SyntaxKind::EnumDeclaration {
                        return self.grammar_error_on_node(
                            node,
                            &Diagnostics::A_class_member_cannot_have_the_0_keyword,
                            Some(vec![token_to_string(SyntaxKind::ConstKeyword)
                                .unwrap()
                                .to_owned()]),
                        );
                    }
                }
                SyntaxKind::OverrideKeyword => {
                    if flags.intersects(ModifierFlags::Override) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["override".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Ambient) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                            Some(vec!["override".to_owned(), "declare".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Readonly) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["override".to_owned(), "readonly".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["override".to_owned(), "async".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Override;
                    last_override = Some(modifier.clone());
                }

                SyntaxKind::PublicKeyword
                | SyntaxKind::ProtectedKeyword
                | SyntaxKind::PrivateKeyword => {
                    let text = self.visibility_to_string(modifier_to_flag(modifier.kind()));

                    if flags.intersects(ModifierFlags::AccessibilityModifier) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::Accessibility_modifier_already_seen,
                            None,
                        );
                    } else if flags.intersects(ModifierFlags::Override) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec![text.to_owned(), "override".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Static) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec![text.to_owned(), "static".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Readonly) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec![text.to_owned(), "readonly".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec![text.to_owned(), "async".to_owned()]),
                        );
                    } else if matches!(
                        node.parent().kind(),
                        SyntaxKind::ModuleBlock | SyntaxKind::SourceFile
                    ) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_module_or_namespace_element,
                            Some(vec![
                                text.to_owned(),
                            ])
                        );
                    } else if flags.intersects(ModifierFlags::Abstract) {
                        if modifier.kind() == SyntaxKind::PrivateKeyword {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                                Some(vec![text.to_owned(), "abstract".to_owned()]),
                            );
                        } else {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_must_precede_1_modifier,
                                Some(vec![text.to_owned(), "abstract".to_owned()]),
                            );
                        }
                    } else if is_private_identifier_class_element_declaration(node) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::An_accessibility_modifier_cannot_be_used_with_a_private_identifier,
                            None
                        );
                    }
                    flags |= modifier_to_flag(modifier.kind());
                }

                SyntaxKind::StaticKeyword => {
                    if flags.intersects(ModifierFlags::Static) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["static".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Readonly) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["static".to_owned(), "readonly".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["static".to_owned(), "async".to_owned()]),
                        );
                    } else if matches!(
                        node.parent().kind(),
                        SyntaxKind::ModuleBlock | SyntaxKind::SourceFile
                    ) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_module_or_namespace_element,
                            Some(vec![
                                "static".to_owned(),
                            ])
                        );
                    } else if node.kind() == SyntaxKind::Parameter {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_parameter,
                            Some(vec!["static".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Abstract) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                            Some(vec!["static".to_owned(), "abstract".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Override) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["static".to_owned(), "override".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Static;
                    last_static = Some(modifier.clone());
                }

                SyntaxKind::ReadonlyKeyword => {
                    if flags.intersects(ModifierFlags::Readonly) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["readonly".to_owned()]),
                        );
                    } else if !matches!(
                        node.kind(),
                        SyntaxKind::PropertyDeclaration
                            | SyntaxKind::PropertySignature
                            | SyntaxKind::IndexSignature
                            | SyntaxKind::Parameter
                    ) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::readonly_modifier_can_only_appear_on_a_property_declaration_or_index_signature,
                            None,
                        );
                    }
                    flags |= ModifierFlags::Readonly;
                    last_readonly = Some(modifier.clone());
                }

                SyntaxKind::ExportKeyword => {
                    if flags.intersects(ModifierFlags::Export) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["export".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Ambient) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["export".to_owned(), "declare".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Abstract) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["export".to_owned(), "abstract".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["export".to_owned(), "async".to_owned()]),
                        );
                    } else if is_class_like(&node.parent()) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_class_elements_of_this_kind,
                            Some(vec!["export".to_owned()]),
                        );
                    } else if node.kind() == SyntaxKind::Parameter {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_parameter,
                            Some(vec!["export".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Export;
                }
                SyntaxKind::DefaultKeyword => {
                    let container = if node.parent().kind() == SyntaxKind::SourceFile {
                        node.parent()
                    } else {
                        node.parent().parent()
                    };
                    if container.kind() == SyntaxKind::ModuleDeclaration
                        && !is_ambient_module(&container)
                    {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::A_default_export_can_only_be_used_in_an_ECMAScript_style_module,
                            None,
                        );
                    } else if !flags.intersects(ModifierFlags::Export) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["export".to_owned(), "default".to_owned()]),
                        );
                    }

                    flags |= ModifierFlags::Default;
                }
                SyntaxKind::DeclareKeyword => {
                    if flags.intersects(ModifierFlags::Ambient) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["declare".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_in_an_ambient_context,
                            Some(vec!["async".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Override) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_in_an_ambient_context,
                            Some(vec!["override".to_owned()]),
                        );
                    } else if is_class_like(&node.parent()) && !is_property_declaration(node) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_class_elements_of_this_kind,
                            Some(vec!["declare".to_owned()]),
                        );
                    } else if node.kind() == SyntaxKind::Parameter {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_parameter,
                            Some(vec!["declare".to_owned()]),
                        );
                    } else if node.parent().flags().intersects(NodeFlags::Ambient)
                        && node.parent().kind() == SyntaxKind::ModuleBlock
                    {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::A_declare_modifier_cannot_be_used_in_an_already_ambient_context,
                            None,
                        );
                    } else if is_private_identifier_class_element_declaration(node) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_a_private_identifier,
                            Some(vec!["declare".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Ambient;
                    last_declare = Some(modifier.clone());
                }

                SyntaxKind::AbstractKeyword => {
                    if flags.intersects(ModifierFlags::Abstract) {
                        self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["abstract".to_owned()]),
                        );
                    }
                    if !matches!(
                        node.kind(),
                        SyntaxKind::ClassDeclaration | SyntaxKind::ConstructorType
                    ) {
                        if !matches!(
                            node.kind(),
                            SyntaxKind::MethodDeclaration
                                | SyntaxKind::PropertyDeclaration
                                | SyntaxKind::GetAccessor
                                | SyntaxKind::SetAccessor
                        ) {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::abstract_modifier_can_only_appear_on_a_class_method_or_property_declaration,
                                None,
                            );
                        }
                        if !(node.parent().kind() == SyntaxKind::ClassDeclaration
                            && has_syntactic_modifier(&node.parent(), ModifierFlags::Abstract))
                        {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::Abstract_methods_can_only_appear_within_an_abstract_class,
                                None,
                            );
                        }
                        if flags.intersects(ModifierFlags::Static) {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                                Some(vec!["static".to_owned(), "abstract".to_owned()]),
                            );
                        }
                        if flags.intersects(ModifierFlags::Private) {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                                Some(vec!["private".to_owned(), "abstract".to_owned()]),
                            );
                        }
                        if flags.intersects(ModifierFlags::Async) {
                            if let Some(last_async) = last_async.as_ref() {
                                return self.grammar_error_on_node(
                                    last_async,
                                    &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                                    Some(vec!["async".to_owned(), "abstract".to_owned()]),
                                );
                            }
                        }
                        if flags.intersects(ModifierFlags::Override) {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_must_precede_1_modifier,
                                Some(vec!["abstract".to_owned(), "override".to_owned()]),
                            );
                        }
                    }
                    if is_named_declaration(node)
                        && node.as_named_declaration().name().kind()
                            == SyntaxKind::PrivateIdentifier
                    {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_a_private_identifier,
                            Some(vec!["abstract".to_owned()]),
                        );
                    }

                    flags |= ModifierFlags::Abstract;
                }

                SyntaxKind::AsyncKeyword => {
                    if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["async".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Ambient)
                        || node.parent().flags().intersects(NodeFlags::Ambient)
                    {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_in_an_ambient_context,
                            Some(vec!["async".to_owned()]),
                        );
                    } else if node.kind() == SyntaxKind::Parameter {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_parameter,
                            Some(vec!["async".to_owned()]),
                        );
                    }
                    if flags.intersects(ModifierFlags::Abstract) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                            Some(vec!["async".to_owned(), "abstract".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Async;
                    last_async = Some(modifier.clone());
                }
                _ => (),
            }
        }

        if node.kind() == SyntaxKind::Constructor {
            if flags.intersects(ModifierFlags::Static) {
                return self.grammar_error_on_node(
                    last_static.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["static".to_owned()]),
                );
            }
            if flags.intersects(ModifierFlags::Abstract) {
                return self.grammar_error_on_node(
                    // TODO: this is what's in the Typescript version but seems like it should be lastDeclare instead?
                    last_static.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["abstract".to_owned()]),
                );
            }
            if flags.intersects(ModifierFlags::Override) {
                return self.grammar_error_on_node(
                    last_override.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["override".to_owned()]),
                );
            } else if flags.intersects(ModifierFlags::Async) {
                return self.grammar_error_on_node(
                    last_async.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["async".to_owned()]),
                );
            } else if flags.intersects(ModifierFlags::Readonly) {
                return self.grammar_error_on_node(
                    last_readonly.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["readonly".to_owned()]),
                );
            }
            return false;
        } else if matches!(
            node.kind(),
            SyntaxKind::ImportDeclaration | SyntaxKind::ImportEqualsDeclaration
        ) && flags.intersects(ModifierFlags::Ambient)
        {
            return self.grammar_error_on_node(
                last_declare.as_ref().unwrap(),
                &Diagnostics::A_0_modifier_cannot_be_used_with_an_import_declaration,
                Some(vec!["declare".to_owned()]),
            );
        } else if node.kind() == SyntaxKind::Parameter
            && flags.intersects(ModifierFlags::ParameterPropertyModifier)
            && is_binding_pattern(node.as_parameter_declaration().maybe_name())
        {
            return self.grammar_error_on_node(
                node,
                &Diagnostics::A_parameter_property_may_not_be_declared_using_a_binding_pattern,
                None,
            );
        } else if node.kind() == SyntaxKind::Parameter
            && flags.intersects(ModifierFlags::ParameterPropertyModifier)
            && node.as_parameter_declaration().dot_dot_dot_token.is_some()
        {
            return self.grammar_error_on_node(
                node,
                &Diagnostics::A_parameter_property_cannot_be_declared_using_a_rest_parameter,
                None,
            );
        }
        if flags.intersects(ModifierFlags::Async) {
            return self.check_grammar_async_modifier(node, last_async.as_ref().unwrap());
        }
        false
    }
}
