#![allow(non_upper_case_globals)]

use gc::Gc;
use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{is_declaration_name_or_import_property_name, CheckMode};
use crate::{
    create_symbol_table, escape_leading_underscores, find_ancestor, get_ancestor,
    get_assignment_declaration_kind, get_combined_local_and_export_symbol_flags,
    get_containing_class, get_declaration_of_kind,
    get_external_module_import_equals_declaration_expression, get_host_signature_from_jsdoc,
    get_name_of_declaration, get_parameter_symbol_from_jsdoc, get_this_container,
    get_type_parameter_from_js_doc, introduces_arguments_exotic_object,
    is_bindable_object_define_property_call, is_call_expression, is_class_like,
    is_constructor_declaration, is_declaration_name, is_element_access_expression, is_entity_name,
    is_entity_name_expression, is_export_assignment, is_expression_node,
    is_expression_with_type_arguments_in_class_extends_clause, is_external_module,
    is_external_module_import_equals_declaration, is_function_like, is_function_like_declaration,
    is_identifier, is_import_call, is_import_or_export_specifier, is_in_expression_context,
    is_in_js_file, is_indexed_access_type_node, is_interface_declaration, is_jsdoc_link_like,
    is_jsdoc_member_name, is_jsdoc_name_reference, is_jsx_tag_name,
    is_literal_computed_property_declaration_name, is_literal_import_type_node,
    is_literal_type_node, is_meta_property, is_private_identifier, is_property_declaration,
    is_qualified_name, is_require_call,
    is_right_side_of_qualified_name_or_property_access_or_jsdoc_member_name, is_static,
    node_is_missing, node_is_present, AssignmentDeclarationKind, Debug_, Diagnostic,
    FindAncestorCallbackReturn, FunctionLikeDeclarationInterface, InternalSymbolName,
    NamedDeclarationInterface, Node, NodeFlags, NodeInterface, Symbol, SymbolFlags,
    SymbolInterface, SymbolTable, SyntaxKind, TypeChecker, TypeInterface, __String,
};

impl TypeChecker {
    pub fn get_global_diagnostics(&self) -> Vec<Gc<Diagnostic>> {
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
    ) -> Vec<Gc<Symbol>> {
        if location.flags().intersects(NodeFlags::InWithStatement) {
            return vec![];
        }

        let mut symbols = create_symbol_table(None);
        let mut is_static_symbol = false;

        let mut location = Some(location.node_wrapper());
        self.populate_symbols(&mut location, meaning, &mut symbols, &mut is_static_symbol);

        symbols.remove(InternalSymbolName::This);
        self.symbols_to_array(&symbols)
    }

    pub(super) fn populate_symbols(
        &self,
        location: &mut Option<Gc<Node>>,
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
                symbols.insert(id.to_owned(), symbol.symbol_wrapper());
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
    ) -> Option<Gc<Node /*ImportEqualsDeclaration | ExportAssignment*/>> {
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
    ) -> Option<Gc<Symbol>> {
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
    ) -> Option<Gc<Node /*ImportTypeNode*/>> {
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
    ) -> Option<Gc<Symbol>> {
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
    ) -> Option<Gc<Symbol>> {
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
                self.get_property_of_type_(&self.get_type_of_symbol(left), "prototype", None)
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
    ) -> Option<Gc<Symbol>> {
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
}
