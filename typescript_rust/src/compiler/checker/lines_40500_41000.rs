use std::io;

use id_arena::Id;

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
    FindAncestorCallbackReturn, FunctionLikeDeclarationInterface, InArena, InternalSymbolName,
    NamedDeclarationInterface, Node, NodeFlags, NodeInterface, OptionInArena, OptionTry, Symbol,
    SymbolFlags, SymbolInterface, SymbolTable, SyntaxKind, TypeChecker, TypeInterface,
};

impl TypeChecker {
    pub fn get_global_diagnostics(&self) -> Vec<Id<Diagnostic>> {
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
        location: Id<Node>,
        meaning: SymbolFlags,
    ) -> io::Result<Vec<Id<Symbol>>> {
        if location
            .ref_(self)
            .flags()
            .intersects(NodeFlags::InWithStatement)
        {
            return Ok(vec![]);
        }

        let mut symbols = create_symbol_table(Option::<&[Id<Symbol>]>::None, self);
        let mut is_static_symbol = false;

        let mut location = Some(location);
        self.populate_symbols(&mut location, meaning, &mut symbols, &mut is_static_symbol)?;

        symbols.shift_remove(InternalSymbolName::This);
        Ok(self.symbols_to_array(&symbols))
    }

    pub(super) fn populate_symbols(
        &self,
        location: &mut Option<Id<Node>>,
        meaning: SymbolFlags,
        symbols: &mut SymbolTable,
        is_static_symbol: &mut bool,
    ) -> io::Result<()> {
        while let Some(location_present) = *location {
            if let Some(location_locals) = location_present.ref_(self).maybe_locals().clone() {
                if !self.is_global_source_file(location_present) {
                    self.copy_symbols(symbols, &location_locals.ref_(self), meaning);
                }
            }

            match location_present.ref_(self).kind() {
                SyntaxKind::SourceFile => {
                    if is_external_module(&location_present.ref_(self)) {
                        self.copy_locally_visible_export_symbols(
                            symbols,
                            &self
                                .get_symbol_of_node(location_present)?
                                .unwrap()
                                .ref_(self)
                                .exports()
                                .ref_(self),
                            meaning & SymbolFlags::ModuleMember,
                        );
                    }
                }
                SyntaxKind::ModuleDeclaration => {
                    self.copy_locally_visible_export_symbols(
                        symbols,
                        &self
                            .get_symbol_of_node(location_present)?
                            .unwrap()
                            .ref_(self)
                            .exports()
                            .ref_(self),
                        meaning & SymbolFlags::ModuleMember,
                    );
                }
                SyntaxKind::EnumDeclaration => {
                    self.copy_symbols(
                        symbols,
                        &self
                            .get_symbol_of_node(location_present)?
                            .unwrap()
                            .ref_(self)
                            .exports()
                            .ref_(self),
                        meaning & SymbolFlags::EnumMember,
                    );
                }
                SyntaxKind::ClassExpression => {
                    let class_name = location_present
                        .ref_(self)
                        .as_class_expression()
                        .maybe_name();
                    if class_name.is_some() {
                        self.copy_symbol(symbols, location_present.ref_(self).symbol(), meaning);
                    }

                    if !*is_static_symbol {
                        self.copy_symbols(
                            symbols,
                            &self
                                .get_members_of_symbol(
                                    self.get_symbol_of_node(location_present)?.unwrap(),
                                )?
                                .ref_(self),
                            meaning & SymbolFlags::Type,
                        );
                    }
                }
                SyntaxKind::ClassDeclaration | SyntaxKind::InterfaceDeclaration => {
                    if !*is_static_symbol {
                        self.copy_symbols(
                            symbols,
                            &self
                                .get_members_of_symbol(
                                    self.get_symbol_of_node(location_present)?.unwrap(),
                                )?
                                .ref_(self),
                            meaning & SymbolFlags::Type,
                        );
                    }
                }
                SyntaxKind::FunctionExpression => {
                    let func_name = location_present
                        .ref_(self)
                        .as_function_expression()
                        .maybe_name();
                    if func_name.is_some() {
                        self.copy_symbol(symbols, location_present.ref_(self).symbol(), meaning);
                    }
                }
                _ => (),
            }

            if introduces_arguments_exotic_object(&location_present.ref_(self)) {
                self.copy_symbol(symbols, self.arguments_symbol(), meaning);
            }

            *is_static_symbol = is_static(location_present, self);
            *location = location_present.ref_(self).maybe_parent();
        }

        self.copy_symbols(symbols, &self.globals(), meaning);

        Ok(())
    }

    pub(super) fn copy_symbol(
        &self,
        symbols: &mut SymbolTable,
        symbol: Id<Symbol>,
        meaning: SymbolFlags,
    ) {
        if get_combined_local_and_export_symbol_flags(symbol).intersects(meaning) {
            let symbol_ref = symbol.ref_(self);
            let id = symbol_ref.escaped_name();
            if !symbols.contains_key(id) {
                symbols.insert(id.to_owned(), symbol);
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
            for &symbol in source.values() {
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
            for &symbol in source.values() {
                if get_declaration_of_kind(symbol, SyntaxKind::ExportSpecifier, self).is_none()
                    && get_declaration_of_kind(symbol, SyntaxKind::NamespaceExport, self).is_none()
                {
                    self.copy_symbol(symbols, symbol, meaning);
                }
            }
        }
    }

    pub(super) fn is_type_declaration_name(&self, name: Id<Node>) -> bool {
        name.ref_(self).kind() == SyntaxKind::Identifier
            && self.is_type_declaration(name.ref_(self).parent())
            && get_name_of_declaration(name.ref_(self).maybe_parent(), self) == Some(name)
    }

    pub(super) fn is_type_declaration(&self, node: Id<Node>) -> bool {
        match node.ref_(self).kind() {
            SyntaxKind::TypeParameter
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::InterfaceDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::EnumDeclaration
            | SyntaxKind::JSDocTypedefTag
            | SyntaxKind::JSDocCallbackTag
            | SyntaxKind::JSDocEnumTag => true,
            SyntaxKind::ImportClause => node.ref_(self).as_import_clause().is_type_only,
            SyntaxKind::ImportSpecifier | SyntaxKind::ExportSpecifier => node
                .ref_(self)
                .parent()
                .ref_(self)
                .parent()
                .ref_(self)
                .as_has_is_type_only()
                .is_type_only(),
            _ => false,
        }
    }

    pub(super) fn is_type_reference_identifier(
        &self,
        mut node: Id<Node>, /*EntityName*/
    ) -> bool {
        while node.ref_(self).parent().ref_(self).kind() == SyntaxKind::QualifiedName {
            node = node.ref_(self).parent();
        }

        node.ref_(self).parent().ref_(self).kind() == SyntaxKind::TypeReference
    }

    pub(super) fn is_heritage_clause_element_identifier(&self, mut node: Id<Node>) -> bool {
        while node.ref_(self).parent().ref_(self).kind() == SyntaxKind::PropertyAccessExpression {
            node = node.ref_(self).parent();
        }

        node.ref_(self).parent().ref_(self).kind() == SyntaxKind::ExpressionWithTypeArguments
    }

    #[allow(dead_code)]
    pub(super) fn for_each_enclosing_class<TReturn>(
        &self,
        node: Id<Node>,
        mut callback: impl FnMut(Id<Node>) -> Option<TReturn>,
    ) -> Option<TReturn> {
        self.try_for_each_enclosing_class(node, |node: Id<Node>| Ok(callback(node)))
            .unwrap()
    }

    pub(super) fn try_for_each_enclosing_class<TReturn>(
        &self,
        node: Id<Node>,
        mut callback: impl FnMut(Id<Node>) -> io::Result<Option<TReturn>>,
    ) -> io::Result<Option<TReturn>> {
        let mut result: Option<TReturn> = None;

        let mut node = Some(node);
        loop {
            node = get_containing_class(node.unwrap(), self);
            if node.is_none() {
                break;
            }
            let node = node.unwrap();
            result = callback(node)?;
            if result.is_some() {
                break;
            }
        }

        Ok(result)
    }

    pub(super) fn for_each_enclosing_class_bool(
        &self,
        node: Id<Node>,
        mut callback: impl FnMut(Id<Node>) -> bool,
    ) -> bool {
        let mut result = false;

        let mut node = Some(node);
        loop {
            node = get_containing_class(node.unwrap(), self);
            if node.is_none() {
                break;
            }
            let node = node.unwrap();
            result = callback(node);
            if result {
                break;
            }
        }

        result
    }

    pub(super) fn is_node_used_during_class_initialization(&self, node: Id<Node>) -> bool {
        find_ancestor(
            Some(node),
            |element| {
                if is_constructor_declaration(&element.ref_(self))
                    && node_is_present(
                        element
                            .ref_(self)
                            .as_constructor_declaration()
                            .maybe_body()
                            .refed(self)
                            .as_deref(),
                    )
                    || is_property_declaration(&element.ref_(self))
                {
                    return true.into();
                } else if is_class_like(&element.ref_(self))
                    || is_function_like_declaration(&element.ref_(self))
                {
                    return FindAncestorCallbackReturn::Quit;
                }

                false.into()
            },
            self,
        )
        .is_some()
    }

    pub(super) fn is_node_within_class(
        &self,
        node: Id<Node>,
        class_declaration: Id<Node>, /*ClassLikeDeclaration*/
    ) -> bool {
        self.for_each_enclosing_class_bool(node, |n| n == class_declaration)
    }

    pub(super) fn get_left_side_of_import_equals_or_export_assignment(
        &self,
        mut node_on_right_side: Id<Node>, /*EntityName*/
    ) -> Option<Id<Node /*ImportEqualsDeclaration | ExportAssignment*/>> {
        while node_on_right_side.ref_(self).parent().ref_(self).kind() == SyntaxKind::QualifiedName
        {
            node_on_right_side = node_on_right_side.ref_(self).parent();
        }

        if node_on_right_side.ref_(self).parent().ref_(self).kind()
            == SyntaxKind::ImportEqualsDeclaration
        {
            return if node_on_right_side
                .ref_(self)
                .parent()
                .ref_(self)
                .as_import_equals_declaration()
                .module_reference
                == node_on_right_side
            {
                node_on_right_side.ref_(self).maybe_parent()
            } else {
                None
            };
        }

        if node_on_right_side.ref_(self).parent().ref_(self).kind() == SyntaxKind::ExportAssignment
        {
            return if node_on_right_side
                .ref_(self)
                .parent()
                .ref_(self)
                .as_export_assignment()
                .expression
                == node_on_right_side
            {
                node_on_right_side.ref_(self).maybe_parent()
            } else {
                None
            };
        }

        None
    }

    pub(super) fn is_in_right_side_of_import_or_export_assignment(
        &self,
        node: Id<Node>, /*EntityName*/
    ) -> bool {
        self.get_left_side_of_import_equals_or_export_assignment(node)
            .is_some()
    }

    pub(super) fn get_special_property_assignment_symbol_from_entity_name(
        &self,
        entity_name: Id<Node>, /*EntityName | PropertyAccessExpression*/
    ) -> io::Result<Option<Id<Symbol>>> {
        let special_property_assignment_kind = get_assignment_declaration_kind(
            entity_name.ref_(self).parent().ref_(self).parent(),
            self,
        );
        Ok(match special_property_assignment_kind {
            AssignmentDeclarationKind::ExportsProperty
            | AssignmentDeclarationKind::PrototypeProperty => {
                self.get_symbol_of_node(entity_name.ref_(self).parent())?
            }
            AssignmentDeclarationKind::ThisProperty
            | AssignmentDeclarationKind::ModuleExports
            | AssignmentDeclarationKind::Property => {
                self.get_symbol_of_node(entity_name.ref_(self).parent().ref_(self).parent())?
            }
            _ => None,
        })
    }

    pub(super) fn is_import_type_qualifier_part(
        &self,
        mut node: Id<Node>, /*EntityName*/
    ) -> Option<Id<Node /*ImportTypeNode*/>> {
        let mut parent = node.ref_(self).parent();
        while is_qualified_name(&parent.ref_(self)) {
            node = parent;
            parent = parent.ref_(self).parent();
        }
        if
        /*parent &&*/
        parent.ref_(self).kind() == SyntaxKind::ImportType
            && parent.ref_(self).as_import_type_node().qualifier == Some(node)
        {
            return Some(parent);
        }
        None
    }

    pub(super) fn get_symbol_of_name_or_property_access_expression(
        &self,
        mut name: Id<Node>, /*EntityName | PrivateIdentifier | PropertyAccessExpression | JSDocMemberName*/
    ) -> io::Result<Option<Id<Symbol>>> {
        if is_declaration_name(name, self) {
            return self.get_symbol_of_node(name.ref_(self).parent());
        }

        if is_in_js_file(Some(&name.ref_(self)))
            && name.ref_(self).parent().ref_(self).kind() == SyntaxKind::PropertyAccessExpression
            && name.ref_(self).parent()
                == name
                    .ref_(self)
                    .parent()
                    .ref_(self)
                    .parent()
                    .ref_(self)
                    .as_binary_expression()
                    .left
        {
            if !is_private_identifier(&name.ref_(self)) && !is_jsdoc_member_name(&name.ref_(self)) {
                let special_property_assignment_symbol =
                    self.get_special_property_assignment_symbol_from_entity_name(name)?;
                if special_property_assignment_symbol.is_some() {
                    return Ok(special_property_assignment_symbol);
                }
            }
        }

        if name.ref_(self).parent().ref_(self).kind() == SyntaxKind::ExportAssignment
            && is_entity_name_expression(name, self)
        {
            let success = self.resolve_entity_name(
                name,
                SymbolFlags::Value
                    | SymbolFlags::Type
                    | SymbolFlags::Namespace
                    | SymbolFlags::Alias,
                Some(true),
                None,
                Option::<Id<Node>>::None,
            )?;
            if matches!(
                success,
                Some(success) if success != self.unknown_symbol()
            ) {
                return Ok(success);
            }
        } else if is_entity_name(&name.ref_(self))
            && self.is_in_right_side_of_import_or_export_assignment(name)
        {
            let import_equals_declaration =
                get_ancestor(Some(name), SyntaxKind::ImportEqualsDeclaration, self);
            Debug_.assert(import_equals_declaration.is_some(), None);
            return self.get_symbol_of_part_of_right_hand_side_of_import_equals(name, Some(true));
        }

        if is_entity_name(&name.ref_(self)) {
            let possible_import_node = self.is_import_type_qualifier_part(name);
            if let Some(possible_import_node) = possible_import_node {
                self.get_type_from_type_node_(possible_import_node)?;
                let sym = self.get_node_links(name).ref_(self).resolved_symbol.clone();
                return Ok(sym.filter(|&sym| sym != self.unknown_symbol()));
            }
        }

        while is_right_side_of_qualified_name_or_property_access_or_jsdoc_member_name(name, self) {
            name = name.ref_(self).parent();
        }

        if self.is_heritage_clause_element_identifier(name) {
            let mut meaning;
            if name.ref_(self).parent().ref_(self).kind() == SyntaxKind::ExpressionWithTypeArguments
            {
                meaning = SymbolFlags::Type;

                if is_expression_with_type_arguments_in_class_extends_clause(
                    name.ref_(self).parent(),
                    self,
                ) {
                    meaning |= SymbolFlags::Value;
                }
            } else {
                meaning = SymbolFlags::Namespace;
            }

            meaning |= SymbolFlags::Alias;
            let entity_name_symbol = if is_entity_name_expression(name, self) {
                self.resolve_entity_name(name, meaning, None, None, Option::<Id<Node>>::None)?
            } else {
                None
            };
            if entity_name_symbol.is_some() {
                return Ok(entity_name_symbol);
            }
        }

        if name.ref_(self).parent().ref_(self).kind() == SyntaxKind::JSDocParameterTag {
            return Ok(get_parameter_symbol_from_jsdoc(
                name.ref_(self).parent(),
                self,
            ));
        }

        if name.ref_(self).parent().ref_(self).kind() == SyntaxKind::TypeParameter
            && name
                .ref_(self)
                .parent()
                .ref_(self)
                .parent()
                .ref_(self)
                .kind()
                == SyntaxKind::JSDocTemplateTag
        {
            Debug_.assert(!is_in_js_file(Some(&name.ref_(self))), None);
            let type_parameter = get_type_parameter_from_js_doc(name.ref_(self).parent(), self);
            return Ok(
                type_parameter.and_then(|type_parameter| type_parameter.ref_(self).maybe_symbol())
            );
        }

        if is_expression_node(name, self) {
            if node_is_missing(Some(&name.ref_(self))) {
                return Ok(None);
            }

            let is_jsdoc = find_ancestor(
                Some(name),
                |ancestor| {
                    is_jsdoc_link_like(&ancestor.ref_(self))
                        || is_jsdoc_name_reference(&ancestor.ref_(self))
                        || is_jsdoc_member_name(&ancestor.ref_(self))
                },
                self,
            )
            .is_some();
            let meaning = if is_jsdoc {
                SymbolFlags::Type | SymbolFlags::Namespace | SymbolFlags::Value
            } else {
                SymbolFlags::Value
            };
            if name.ref_(self).kind() == SyntaxKind::Identifier {
                if is_jsx_tag_name(name, self) && self.is_jsx_intrinsic_identifier(name) {
                    let symbol = self.get_intrinsic_tag_symbol(name.ref_(self).parent())?;
                    return Ok(if symbol == self.unknown_symbol() {
                        None
                    } else {
                        Some(symbol)
                    });
                }
                let result = self.resolve_entity_name(
                    name,
                    meaning,
                    Some(false),
                    Some(!is_jsdoc),
                    get_host_signature_from_jsdoc(name, self),
                )?;
                if result.is_none() && is_jsdoc {
                    let container = find_ancestor(
                        Some(name),
                        |ancestor| {
                            is_class_like(&ancestor.ref_(self))
                                || is_interface_declaration(&ancestor.ref_(self))
                        },
                        self,
                    );
                    if let Some(container) = container {
                        return self
                            .resolve_jsdoc_member_name(name, self.get_symbol_of_node(container)?);
                    }
                }
                return Ok(result);
            } else if is_private_identifier(&name.ref_(self)) {
                return Ok(self.get_symbol_for_private_identifier_expression(name));
            } else if matches!(
                name.ref_(self).kind(),
                SyntaxKind::PropertyAccessExpression | SyntaxKind::QualifiedName
            ) {
                let links = self.get_node_links(name);
                let links_resolved_symbol = links.ref_(self).resolved_symbol.clone();
                if links_resolved_symbol.is_some() {
                    return Ok(links_resolved_symbol);
                }

                if name.ref_(self).kind() == SyntaxKind::PropertyAccessExpression {
                    self.check_property_access_expression(name, Some(CheckMode::Normal))?;
                } else {
                    self.check_qualified_name(name, Some(CheckMode::Normal))?;
                }
                if links.ref_(self).resolved_symbol.is_none()
                    && is_jsdoc
                    && is_qualified_name(&name.ref_(self))
                {
                    return self.resolve_jsdoc_member_name(name, Option::<Id<Symbol>>::None);
                }
                return Ok(links.ref_(self).resolved_symbol.clone());
            } else if is_jsdoc_member_name(&name.ref_(self)) {
                return self.resolve_jsdoc_member_name(name, Option::<Id<Symbol>>::None);
            }
        } else if self.is_type_reference_identifier(name) {
            let meaning = if name.ref_(self).parent().ref_(self).kind() == SyntaxKind::TypeReference
            {
                SymbolFlags::Type
            } else {
                SymbolFlags::Namespace
            };
            let symbol = self.resolve_entity_name(
                name,
                meaning,
                Some(false),
                Some(true),
                Option::<Id<Node>>::None,
            )?;
            return Ok(
                if matches!(
                    symbol,
                    Some(symbol) if symbol != self.unknown_symbol()
                ) {
                    symbol
                } else {
                    Some(self.get_unresolved_symbol_for_entity_name(name))
                },
            );
        }
        if name.ref_(self).parent().ref_(self).kind() == SyntaxKind::TypePredicate {
            return self.resolve_entity_name(
                name,
                SymbolFlags::FunctionScopedVariable,
                None,
                None,
                Option::<Id<Node>>::None,
            );
        }

        Ok(None)
    }

    pub(super) fn resolve_jsdoc_member_name(
        &self,
        name: Id<Node>, /*EntityName | JSDocMemberName*/
        container: Option<Id<Symbol>>,
    ) -> io::Result<Option<Id<Symbol>>> {
        if is_entity_name(&name.ref_(self)) {
            let meaning = SymbolFlags::Type | SymbolFlags::Namespace | SymbolFlags::Value;
            let mut symbol = self.resolve_entity_name(
                name,
                meaning,
                Some(false),
                Some(true),
                get_host_signature_from_jsdoc(name, self),
            )?;
            if symbol.is_none() && is_identifier(&name.ref_(self)) {
                if let Some(container) = container {
                    symbol = self.get_merged_symbol(self.get_symbol(
                        &self.get_exports_of_symbol(container)?.ref_(self),
                        &name.ref_(self).as_identifier().escaped_text,
                        meaning,
                    )?);
                }
            }
            if symbol.is_some() {
                return Ok(symbol);
            }
        }
        let left = if is_identifier(&name.ref_(self)) {
            container
        } else {
            self.resolve_jsdoc_member_name(
                name.ref_(self).as_has_left_and_right().left(),
                Option::<Id<Symbol>>::None,
            )?
        };
        let right = if is_identifier(&name.ref_(self)) {
            name.ref_(self).as_identifier().escaped_text.clone()
        } else {
            name.ref_(self)
                .as_has_left_and_right()
                .right()
                .ref_(self)
                .as_identifier()
                .escaped_text
                .clone()
        };
        if let Some(left) = left {
            let proto = if left.ref_(self).flags().intersects(SymbolFlags::Value) {
                self.get_property_of_type_(self.get_type_of_symbol(left)?, "prototype", None)?
            } else {
                None
            };
            let t = if let Some(proto) = proto {
                self.get_type_of_symbol(proto)?
            } else {
                self.get_declared_type_of_symbol(left)?
            };
            return self.get_property_of_type_(t, &right, None);
        }
        Ok(None)
    }

    pub(super) fn get_symbol_at_location_(
        &self,
        node: Id<Node>,
        ignore_errors: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        if node.ref_(self).kind() == SyntaxKind::SourceFile {
            return Ok(if is_external_module(&node.ref_(self)) {
                self.get_merged_symbol(node.ref_(self).maybe_symbol())
            } else {
                None
            });
        }
        let parent = node.ref_(self).parent();
        let grand_parent = parent.ref_(self).parent();

        if node
            .ref_(self)
            .flags()
            .intersects(NodeFlags::InWithStatement)
        {
            return Ok(None);
        }

        if is_declaration_name_or_import_property_name(node, self) {
            let parent_symbol = self.get_symbol_of_node(parent)?;
            return Ok(
                if is_import_or_export_specifier(&node.ref_(self).parent().ref_(self))
                    && node
                        .ref_(self)
                        .parent()
                        .ref_(self)
                        .as_has_property_name()
                        .maybe_property_name()
                        == Some(node)
                {
                    self.get_immediate_aliased_symbol(parent_symbol.unwrap())?
                } else {
                    parent_symbol
                },
            );
        } else if is_literal_computed_property_declaration_name(node, self) {
            return self.get_symbol_of_node(parent.ref_(self).parent());
        }

        if node.ref_(self).kind() == SyntaxKind::Identifier {
            if self.is_in_right_side_of_import_or_export_assignment(node) {
                return self.get_symbol_of_name_or_property_access_expression(node);
            } else if parent.ref_(self).kind() == SyntaxKind::BindingElement
                && grand_parent.ref_(self).kind() == SyntaxKind::ObjectBindingPattern
                && parent.ref_(self).as_binding_element().property_name == Some(node)
            {
                let type_of_pattern = self.get_type_of_node(grand_parent)?;
                let property_declaration = self.get_property_of_type_(
                    type_of_pattern,
                    &node.ref_(self).as_identifier().escaped_text,
                    None,
                )?;

                if property_declaration.is_some() {
                    return Ok(property_declaration);
                }
            } else if is_meta_property(&parent.ref_(self)) {
                let parent_type = self.get_type_of_node(parent)?;
                let property_declaration = self.get_property_of_type_(
                    parent_type,
                    &node.ref_(self).as_identifier().escaped_text,
                    None,
                )?;
                if property_declaration.is_some() {
                    return Ok(property_declaration);
                }
                if parent.ref_(self).as_meta_property().keyword_token == SyntaxKind::NewKeyword {
                    return Ok(self
                        .check_new_target_meta_property(parent)?
                        .ref_(self)
                        .maybe_symbol());
                }
            }
        }

        Ok(match node.ref_(self).kind() {
            SyntaxKind::Identifier
            | SyntaxKind::PrivateIdentifier
            | SyntaxKind::PropertyAccessExpression
            | SyntaxKind::QualifiedName => {
                self.get_symbol_of_name_or_property_access_expression(node)?
            }
            SyntaxKind::ThisKeyword => {
                let container = get_this_container(node, false, self);
                if is_function_like(Some(&container.ref_(self))) {
                    let sig = self.get_signature_from_declaration_(container)?;
                    let sig_this_parameter = sig.ref_(self).maybe_this_parameter().clone();
                    if sig_this_parameter.is_some() {
                        return Ok(sig_this_parameter);
                    }
                }
                if is_in_expression_context(node, self) {
                    return Ok(self
                        .check_expression(node, None, None)?
                        .ref_(self)
                        .maybe_symbol());
                }

                self.get_type_from_this_type_node(node)?
                    .ref_(self)
                    .maybe_symbol()
            }

            SyntaxKind::ThisType => self
                .get_type_from_this_type_node(node)?
                .ref_(self)
                .maybe_symbol(),

            SyntaxKind::SuperKeyword => self
                .check_expression(node, None, None)?
                .ref_(self)
                .maybe_symbol(),

            SyntaxKind::ConstructorKeyword => {
                let constructor_declaration = node.ref_(self).maybe_parent();
                if let Some(constructor_declaration) =
                    constructor_declaration.filter(|constructor_declaration| {
                        constructor_declaration.ref_(self).kind() == SyntaxKind::Constructor
                    })
                {
                    return Ok(constructor_declaration
                        .ref_(self)
                        .parent()
                        .ref_(self)
                        .maybe_symbol());
                }
                None
            }

            SyntaxKind::StringLiteral | SyntaxKind::NoSubstitutionTemplateLiteral => {
                if is_external_module_import_equals_declaration(
                    node.ref_(self).parent().ref_(self).parent(),
                    self,
                ) && get_external_module_import_equals_declaration_expression(
                    node.ref_(self).parent().ref_(self).parent(),
                    self,
                ) == node
                    || matches!(
                        node.ref_(self).parent().ref_(self).kind(),
                        SyntaxKind::ImportDeclaration | SyntaxKind::ExportDeclaration
                    ) && node
                        .ref_(self)
                        .parent()
                        .ref_(self)
                        .as_has_module_specifier()
                        .maybe_module_specifier()
                        == Some(node)
                    || (is_in_js_file(Some(&node.ref_(self)))
                        && is_require_call(node.ref_(self).parent(), false, self)
                        || is_import_call(node.ref_(self).parent(), self))
                    || is_literal_type_node(&node.ref_(self).parent().ref_(self))
                        && is_literal_import_type_node(
                            node.ref_(self).parent().ref_(self).parent(),
                            self,
                        )
                        && node
                            .ref_(self)
                            .parent()
                            .ref_(self)
                            .parent()
                            .ref_(self)
                            .as_import_type_node()
                            .argument
                            == node.ref_(self).parent()
                {
                    return self.resolve_external_module_name_(node, node, ignore_errors);
                }
                if is_call_expression(&parent.ref_(self))
                    && is_bindable_object_define_property_call(parent, self)
                    && parent.ref_(self).as_call_expression().arguments.ref_(self)[1] == node
                {
                    return self.get_symbol_of_node(parent);
                }

                let object_type = if is_element_access_expression(&parent.ref_(self)) {
                    let parent_ref = parent.ref_(self);
                    let parent_as_element_access_expression =
                        parent_ref.as_element_access_expression();
                    if parent_as_element_access_expression.argument_expression == node {
                        Some(self.get_type_of_expression(
                            parent_as_element_access_expression.expression,
                        )?)
                    } else {
                        None
                    }
                } else if is_literal_type_node(&parent.ref_(self))
                    && is_indexed_access_type_node(&grand_parent.ref_(self))
                {
                    Some(
                        self.get_type_from_type_node_(
                            grand_parent
                                .ref_(self)
                                .as_indexed_access_type_node()
                                .object_type,
                        )?,
                    )
                } else {
                    None
                };
                object_type.try_and_then(|object_type| {
                    self.get_property_of_type_(
                        object_type,
                        &escape_leading_underscores(&node.ref_(self).as_literal_like_node().text()),
                        None,
                    )
                })?
            }

            SyntaxKind::NumericLiteral => {
                let object_type = if is_element_access_expression(&parent.ref_(self)) {
                    let parent_ref = parent.ref_(self);
                    let parent_as_element_access_expression =
                        parent_ref.as_element_access_expression();
                    if parent_as_element_access_expression.argument_expression == node {
                        Some(self.get_type_of_expression(
                            parent_as_element_access_expression.expression,
                        )?)
                    } else {
                        None
                    }
                } else if is_literal_type_node(&parent.ref_(self))
                    && is_indexed_access_type_node(&grand_parent.ref_(self))
                {
                    Some(
                        self.get_type_from_type_node_(
                            grand_parent
                                .ref_(self)
                                .as_indexed_access_type_node()
                                .object_type,
                        )?,
                    )
                } else {
                    None
                };
                object_type.try_and_then(|object_type| {
                    self.get_property_of_type_(
                        object_type,
                        &escape_leading_underscores(&node.ref_(self).as_literal_like_node().text()),
                        None,
                    )
                })?
            }

            SyntaxKind::DefaultKeyword
            | SyntaxKind::FunctionKeyword
            | SyntaxKind::EqualsGreaterThanToken
            | SyntaxKind::ClassKeyword => self.get_symbol_of_node(node.ref_(self).parent())?,
            SyntaxKind::ImportType => {
                if is_literal_import_type_node(node, self) {
                    self.get_symbol_at_location_(
                        node.ref_(self)
                            .as_import_type_node()
                            .argument
                            .ref_(self)
                            .as_literal_type_node()
                            .literal,
                        None,
                    )?
                } else {
                    None
                }
            }

            SyntaxKind::ExportKeyword => {
                if is_export_assignment(&node.ref_(self).parent().ref_(self)) {
                    Some(
                        Debug_.check_defined(
                            node.ref_(self).parent().ref_(self).maybe_symbol(),
                            None,
                        ),
                    )
                } else {
                    None
                }
            }

            SyntaxKind::ImportKeyword | SyntaxKind::NewKeyword => {
                if is_meta_property(&node.ref_(self).parent().ref_(self)) {
                    self.check_meta_property_keyword(node.ref_(self).parent())?
                        .ref_(self)
                        .maybe_symbol()
                } else {
                    None
                }
            }
            SyntaxKind::MetaProperty => self
                .check_expression(node, None, None)?
                .ref_(self)
                .maybe_symbol(),

            _ => None,
        })
    }
}
