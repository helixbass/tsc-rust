#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::{
    is_declaration_name_or_import_property_name, EmitResolverCreateResolver, IterationUse,
};
use crate::{
    add_related_info, cast_present, concatenate, create_diagnostic_for_node, create_symbol_table,
    escape_leading_underscores, external_helpers_module_name_text, filter, find_ancestor,
    first_or_undefined, flat_map, for_each, for_each_entry_bool, get_all_accessor_declarations,
    get_check_flags, get_declaration_of_kind, get_enclosing_block_scope_container,
    get_external_module_name, get_parse_tree_node, get_source_file_of_node, has_syntactic_modifier,
    id_text, index_of_node, is_ambient_module, is_array_literal_expression, is_assignment_pattern,
    is_binding_element, is_binding_pattern, is_block_scoped_container_top_level, is_class_like,
    is_declaration, is_effective_external_module, is_export_specifier, is_expression_node,
    is_external_module, is_generated_identifier, is_global_scope_augmentation, is_identifier,
    is_import_equals_declaration, is_internal_module_import_equals_declaration,
    is_iteration_statement, is_meta_property, is_module_or_enum_declaration, is_named_declaration,
    is_namespace_export, is_object_literal_expression, is_part_of_type_node,
    is_private_identifier_class_element_declaration, is_property_access_expression,
    is_property_assignment, is_property_declaration,
    is_right_side_of_qualified_name_or_property_access, is_shorthand_ambient_module_symbol,
    is_source_file, is_statement_with_locals, is_static, is_string_literal, map_defined,
    modifier_to_flag, node_can_be_decorated, node_is_missing, node_is_present,
    single_element_array, some, token_to_string, try_cast,
    try_get_class_implementing_or_extending_expression_with_type_arguments,
    type_has_call_or_construct_signatures, walk_up_binding_elements_and_patterns, CheckFlags,
    Debug_, Diagnostics, ExternalEmitHelpers, FunctionLikeDeclarationInterface,
    InterfaceTypeInterface, ModifierFlags, NamedDeclarationInterface, NodeCheckFlags, NodeFlags,
    ObjectFlags, Signature, SignatureKind, SymbolInterface, SyntaxKind, TransientSymbolInterface,
    TypeFlags, TypeInterface, UnionOrIntersectionTypeInterface, __String, bind_source_file,
    is_external_or_common_js_module, Diagnostic, EmitResolverDebuggable, IndexInfo, Node,
    NodeInterface, StringOrNumber, Symbol, SymbolFlags, Type, TypeChecker,
};

impl TypeChecker {
    pub(super) fn get_index_infos_at_location_(&self, node: &Node) -> Option<Vec<Rc<IndexInfo>>> {
        if is_identifier(node)
            && is_property_access_expression(&node.parent())
            && ptr::eq(&*node.parent().as_property_access_expression().name, node)
        {
            let ref key_type = self.get_literal_type_from_property_name(node);
            let ref object_type = self
                .get_type_of_expression(&node.parent().as_property_access_expression().expression);
            let object_types = if object_type.flags().intersects(TypeFlags::Union) {
                object_type.as_union_type().types().to_owned()
            } else {
                vec![object_type.clone()]
            };
            return Some(flat_map(Some(&object_types), |t: &Rc<Type>, _| {
                filter(&self.get_index_infos_of_type(t), |info: &Rc<IndexInfo>| {
                    self.is_applicable_index_type(key_type, &info.key_type)
                })
            }));
        }
        None
    }

    pub(super) fn get_shorthand_assignment_value_symbol_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
    ) -> Option<Rc<Symbol>> {
        if let Some(location) = location {
            let location: &Node = location.borrow();
            if location.kind() == SyntaxKind::ShorthandPropertyAssignment {
                return self.resolve_entity_name(
                    &location.as_shorthand_property_assignment().name(),
                    SymbolFlags::Value | SymbolFlags::Alias,
                    None,
                    None,
                    Option::<&Node>::None,
                );
            }
        }
        None
    }

    pub(super) fn get_export_specifier_local_target_symbol_(
        &self,
        node: &Node, /*Identifier | ExportSpecifier*/
    ) -> Option<Rc<Symbol>> {
        if is_export_specifier(node) {
            let node_as_export_specifier = node.as_export_specifier();
            if node
                .parent()
                .parent()
                .as_export_declaration()
                .module_specifier
                .is_some()
            {
                self.get_external_module_member(&node.parent().parent(), node, None)
            } else {
                self.resolve_entity_name(
                    node_as_export_specifier
                        .property_name
                        .as_deref()
                        .unwrap_or(&*node_as_export_specifier.name),
                    SymbolFlags::Value
                        | SymbolFlags::Type
                        | SymbolFlags::Namespace
                        | SymbolFlags::Alias,
                    None,
                    None,
                    Option::<&Node>::None,
                )
            }
        } else {
            self.resolve_entity_name(
                node,
                SymbolFlags::Value
                    | SymbolFlags::Type
                    | SymbolFlags::Namespace
                    | SymbolFlags::Alias,
                None,
                None,
                Option::<&Node>::None,
            )
        }
    }

    pub(super) fn get_type_of_node(&self, node: &Node) -> Rc<Type> {
        if is_source_file(node) && !is_external_module(node) {
            return self.error_type();
        }

        if node.flags().intersects(NodeFlags::InWithStatement) {
            return self.error_type();
        }

        let class_decl =
            try_get_class_implementing_or_extending_expression_with_type_arguments(node);
        let class_type = class_decl.as_ref().map(|class_decl| {
            self.get_declared_type_of_class_or_interface(
                &self.get_symbol_of_node(&class_decl.class).unwrap(),
            )
        });
        if is_part_of_type_node(node) {
            let ref type_from_type_node = self.get_type_from_type_node_(node);
            return if let Some(class_type) = class_type.as_ref() {
                self.get_type_with_this_argument(
                    type_from_type_node,
                    class_type.as_interface_type().maybe_this_type(),
                    None,
                )
            } else {
                type_from_type_node.clone()
            };
        }

        if is_expression_node(node) {
            return self.get_regular_type_of_expression(node);
        }

        if let Some(class_type) = class_type.as_ref() {
            if !class_decl.as_ref().unwrap().is_implements {
                let base_types = self.get_base_types(class_type);
                let base_type = first_or_undefined(&base_types);
                return if let Some(base_type) = base_type {
                    self.get_type_with_this_argument(
                        base_type,
                        class_type.as_interface_type().maybe_this_type(),
                        None,
                    )
                } else {
                    self.error_type()
                };
            }
        }

        if self.is_type_declaration(node) {
            let ref symbol = self.get_symbol_of_node(node).unwrap();
            return self.get_declared_type_of_symbol(symbol);
        }

        if self.is_type_declaration_name(node) {
            let symbol = self.get_symbol_at_location_(node, None);
            return if let Some(symbol) = symbol.as_ref() {
                self.get_declared_type_of_symbol(symbol)
            } else {
                self.error_type()
            };
        }

        if is_declaration(node) {
            let ref symbol = self.get_symbol_of_node(node).unwrap();
            return self.get_type_of_symbol(symbol);
        }

        if is_declaration_name_or_import_property_name(node) {
            let symbol = self.get_symbol_at_location_(node, None);
            if let Some(symbol) = symbol.as_ref() {
                return self.get_type_of_symbol(symbol);
            }
            return self.error_type();
        }

        if is_binding_pattern(Some(node)) {
            return self
                .get_type_for_variable_like_declaration(&node.parent(), true)
                .unwrap_or_else(|| self.error_type());
        }

        if self.is_in_right_side_of_import_or_export_assignment(node) {
            let symbol = self.get_symbol_at_location_(node, None);
            if let Some(symbol) = symbol.as_ref() {
                let ref declared_type = self.get_declared_type_of_symbol(symbol);
                return if !self.is_error_type(declared_type) {
                    declared_type.clone()
                } else {
                    self.get_type_of_symbol(symbol)
                };
            }
        }

        if is_meta_property(&node.parent())
            && node.parent().as_meta_property().keyword_token == node.kind()
        {
            return self.check_meta_property_keyword(&node.parent());
        }

        self.error_type()
    }

    pub(super) fn get_type_of_assignment_pattern_(
        &self,
        expr: &Node, /*AssignmentPattern*/
    ) -> Option<Rc<Type>> {
        Debug_.assert(
            matches!(
                expr.kind(),
                SyntaxKind::ObjectLiteralExpression | SyntaxKind::ArrayLiteralExpression
            ),
            None,
        );
        if expr.parent().kind() == SyntaxKind::ForOfStatement {
            let ref iterated_type = self.check_right_hand_side_of_for_of(&expr.parent());
            return Some(self.check_destructuring_assignment(
                expr,
                iterated_type, /*|| errorType*/
                None,
                None,
            ));
        }
        if expr.parent().kind() == SyntaxKind::BinaryExpression {
            let ref iterated_type =
                self.get_type_of_expression(&expr.parent().as_binary_expression().right);
            return Some(self.check_destructuring_assignment(
                expr,
                &iterated_type, /*|| errorType*/
                None,
                None,
            ));
        }
        if expr.parent().kind() == SyntaxKind::PropertyAssignment {
            let ref node = cast_present(expr.parent().parent(), |node: &Rc<Node>| {
                is_object_literal_expression(node)
            });
            let ref type_of_parent_object_literal = self
                .get_type_of_assignment_pattern_(node)
                .unwrap_or_else(|| self.error_type());
            let property_index = index_of_node(
                &node.as_object_literal_expression().properties,
                &expr.parent(),
            );
            return self.check_object_literal_destructuring_property_assignment(
                node,
                type_of_parent_object_literal,
                property_index.try_into().unwrap(),
                None,
                None,
            );
        }
        let node = cast_present(expr.parent(), |node: &Rc<Node>| {
            is_array_literal_expression(node)
        });
        let ref type_of_array_literal = self
            .get_type_of_assignment_pattern_(&node)
            .unwrap_or_else(|| self.error_type());
        let ref element_type = self.check_iterated_type_or_element_type(
            IterationUse::Destructuring,
            type_of_array_literal,
            &self.undefined_type(),
            expr.maybe_parent()
        ) /*|| errorType*/;
        self.check_array_literal_destructuring_element_assignment(
            &node,
            type_of_array_literal,
            {
                let ret = node
                    .as_array_literal_expression()
                    .elements
                    .iter()
                    .position(|element| ptr::eq(&**element, expr))
                    .unwrap();
                ret
            },
            element_type,
            None,
        )
    }

    pub(super) fn get_property_symbol_of_destructuring_assignment_(
        &self,
        location: &Node, /*Identifier*/
    ) -> Option<Rc<Symbol>> {
        let type_of_object_literal = self.get_type_of_assignment_pattern_(&*cast_present(
            location.parent().parent(),
            |node: &Rc<Node>| is_assignment_pattern(node),
        ));
        type_of_object_literal
            .as_ref()
            .and_then(|type_of_object_literal| {
                self.get_property_of_type_(
                    type_of_object_literal,
                    &location.as_identifier().escaped_text,
                    None,
                )
            })
    }

    pub(super) fn get_regular_type_of_expression(
        &self,
        expr: &Node, /*Expression*/
    ) -> Rc<Type> {
        let mut expr = expr.node_wrapper();
        if is_right_side_of_qualified_name_or_property_access(&expr) {
            expr = expr.parent();
        }
        self.get_regular_type_of_literal_type(&self.get_type_of_expression(&expr))
    }

    pub(super) fn get_parent_type_of_class_element(
        &self,
        node: &Node, /*ClassElement*/
    ) -> Rc<Type> {
        let ref class_symbol = self.get_symbol_of_node(&node.parent()).unwrap();
        if is_static(node) {
            self.get_type_of_symbol(class_symbol)
        } else {
            self.get_declared_type_of_symbol(class_symbol)
        }
    }

    pub(super) fn get_class_element_property_key_type(
        &self,
        element: &Node, /*ClassElement*/
    ) -> Rc<Type> {
        let ref name = element.as_named_declaration().name();
        match name.kind() {
            SyntaxKind::Identifier => self.get_string_literal_type(&id_text(name)),
            SyntaxKind::NumericLiteral | SyntaxKind::StringLiteral => {
                self.get_string_literal_type(&name.as_literal_like_node().text())
            }
            SyntaxKind::ComputedPropertyName => {
                let ref name_type = self.check_computed_property_name(name);
                if self.is_type_assignable_to_kind(name_type, TypeFlags::ESSymbolLike, None) {
                    name_type.clone()
                } else {
                    self.string_type()
                }
            }
            _ => Debug_.fail(Some("Unsupported property name.")),
        }
    }

    pub(super) fn get_augmented_properties_of_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        let ref type_ = self.get_apparent_type(type_);
        let mut props_by_name = create_symbol_table(Some(&self.get_properties_of_type(type_)));
        let function_type = if !self
            .get_signatures_of_type(type_, SignatureKind::Call)
            .is_empty()
        {
            Some(self.global_callable_function_type())
        } else if !self
            .get_signatures_of_type(type_, SignatureKind::Construct)
            .is_empty()
        {
            Some(self.global_newable_function_type())
        } else {
            None
        };
        if let Some(function_type) = function_type.as_ref() {
            for_each(
                &self.get_properties_of_type(function_type),
                |p: &Rc<Symbol>, _| -> Option<()> {
                    if !props_by_name.contains_key(p.escaped_name()) {
                        props_by_name.insert(p.escaped_name().clone(), p.clone());
                    }
                    None
                },
            );
        }
        self.get_named_members(&props_by_name)
    }

    pub(super) fn type_has_call_or_construct_signatures(&self, type_: &Type) -> bool {
        type_has_call_or_construct_signatures(type_, self)
    }

    pub(super) fn get_root_symbols(&self, symbol: &Symbol) -> Vec<Rc<Symbol>> {
        let roots = self.get_immediate_root_symbols(symbol);
        if let Some(roots) = roots.as_ref() {
            flat_map(Some(roots), |root: &Rc<Symbol>, _| {
                self.get_root_symbols(root)
            })
        } else {
            vec![symbol.symbol_wrapper()]
        }
    }

    pub(super) fn get_immediate_root_symbols(&self, symbol: &Symbol) -> Option<Vec<Rc<Symbol>>> {
        if get_check_flags(symbol).intersects(CheckFlags::Synthetic) {
            return Some(map_defined(
                Some(
                    (*self.get_symbol_links(symbol))
                        .borrow()
                        .containing_type
                        .as_ref()
                        .unwrap()
                        .as_union_or_intersection_type_interface()
                        .types(),
                ),
                |type_: &Rc<Type>, _| {
                    self.get_property_of_type_(type_, symbol.escaped_name(), None)
                },
            ));
        } else if symbol.flags().intersects(SymbolFlags::Transient) {
            let (left_spread, right_spread, synthetic_origin) = {
                let symbol_links = symbol.as_transient_symbol().symbol_links();
                let symbol_links = (*symbol_links).borrow();
                (
                    symbol_links.left_spread.clone(),
                    symbol_links.right_spread.clone(),
                    symbol_links.synthetic_origin.clone(),
                )
            };
            return if let Some(left_spread) = left_spread {
                Some(vec![left_spread, right_spread.unwrap()])
            } else if let Some(synthetic_origin) = synthetic_origin {
                Some(vec![synthetic_origin])
            } else {
                single_element_array(self.try_get_alias_target(symbol))
            };
        }
        None
    }

    pub(super) fn try_get_alias_target(&self, symbol: &Symbol) -> Option<Rc<Symbol>> {
        let mut target: Option<Rc<Symbol>> = None;
        let mut next: Option<Rc<Symbol>> = Some(symbol.symbol_wrapper());
        while {
            next = (*self.get_symbol_links(next.as_ref().unwrap()))
                .borrow()
                .target
                .clone();
            next.is_some()
        } {
            target = next.clone();
        }
        target
    }

    pub(super) fn is_arguments_local_binding(&self, node_in: &Node /*Identifier*/) -> bool {
        if is_generated_identifier(node_in) {
            return false;
        }
        let node = get_parse_tree_node(Some(node_in), Some(is_identifier));
        if node.is_none() {
            return false;
        }
        let ref node = node.unwrap();
        let parent = node.maybe_parent();
        if parent.is_none() {
            return false;
        }
        let ref parent = parent.unwrap();
        let is_property_name = (is_property_access_expression(parent)
            || is_property_assignment(parent))
            && Rc::ptr_eq(&parent.as_named_declaration().name(), node);
        !is_property_name
            && matches!(
                self.get_referenced_value_symbol(
                    node,
                    None,
                ).as_ref(),
                Some(referenced_value_symbol) if Rc::ptr_eq(
                    referenced_value_symbol,
                    &self.arguments_symbol()
                )
            )
    }

    pub(super) fn module_exports_some_value(
        &self,
        module_reference_expression: &Node, /*Expression*/
    ) -> bool {
        let module_symbol = self.resolve_external_module_name_(
            &module_reference_expression.parent(),
            module_reference_expression,
            None,
        );
        if match module_symbol.as_ref() {
            None => true,
            Some(module_symbol) => is_shorthand_ambient_module_symbol(module_symbol),
        } {
            return true;
        }
        let module_symbol = module_symbol.as_ref().unwrap();

        let has_export_assignment = self.has_export_assignment_symbol(module_symbol);
        let ref module_symbol = self
            .resolve_external_module_symbol(Some(&**module_symbol), None)
            .unwrap();

        let symbol_links = self.get_symbol_links(module_symbol);
        if (*symbol_links).borrow().exports_some_value.is_none() {
            symbol_links.borrow_mut().exports_some_value = Some(if has_export_assignment {
                module_symbol.flags().intersects(SymbolFlags::Value)
            } else {
                for_each_entry_bool(
                    &(*self.get_exports_of_module_(module_symbol)).borrow(),
                    |s: &Rc<Symbol>, _| self.is_value(s),
                )
            });
        }
        let ret = (*symbol_links).borrow().exports_some_value.unwrap();
        ret
    }

    pub(super) fn is_value(&self, s: &Symbol) -> bool {
        let s = self.resolve_symbol(Some(s), None);
        matches!(
            s.as_ref(),
            Some(s) if s.flags().intersects(SymbolFlags::Value)
        )
    }

    pub(super) fn is_name_of_module_or_enum_declaration(
        &self,
        node: &Node, /*Identifier*/
    ) -> bool {
        is_module_or_enum_declaration(&node.parent())
            && ptr::eq(node, &*node.parent().as_named_declaration().name())
    }

    pub(super) fn get_referenced_export_container(
        &self,
        node_in: &Node, /*Identifier*/
        prefix_locals: Option<bool>,
    ) -> Option<Rc<Node /*SourceFile | ModuleDeclaration | EnumDeclaration*/>> {
        let node = get_parse_tree_node(Some(node_in), Some(is_identifier));
        if let Some(node) = node.as_ref() {
            let symbol = self.get_referenced_value_symbol(
                node,
                Some(self.is_name_of_module_or_enum_declaration(node)),
            );
            if let Some(mut symbol) = symbol {
                if symbol.flags().intersects(SymbolFlags::ExportValue) {
                    let ref export_symbol = self
                        .get_merged_symbol(symbol.maybe_export_symbol())
                        .unwrap();
                    if prefix_locals != Some(true)
                        && export_symbol
                            .flags()
                            .intersects(SymbolFlags::ExportHasLocal)
                        && !export_symbol.flags().intersects(SymbolFlags::Variable)
                    {
                        return None;
                    }
                    symbol = export_symbol.clone();
                }
                let parent_symbol = self.get_parent_of_symbol(&symbol);
                if let Some(parent_symbol) = parent_symbol.as_ref() {
                    if parent_symbol.flags().intersects(SymbolFlags::ValueModule) {
                        if let Some(parent_symbol_value_declaration) = parent_symbol
                            .maybe_value_declaration()
                            .as_ref()
                            .filter(|parent_symbol_value_declaration| {
                                parent_symbol_value_declaration.kind() == SyntaxKind::SourceFile
                            })
                        {
                            let symbol_file = parent_symbol_value_declaration;
                            let ref reference_file =
                                get_source_file_of_node(Some(&**node)).unwrap();
                            let symbol_is_umd_export = !Rc::ptr_eq(symbol_file, reference_file);
                            return if symbol_is_umd_export {
                                None
                            } else {
                                Some(symbol_file.clone())
                            };
                        }
                    }
                    return find_ancestor(node.maybe_parent(), |n| {
                        is_module_or_enum_declaration(n)
                            && matches!(
                                self.get_symbol_of_node(n).as_ref(),
                                Some(symbol) if Rc::ptr_eq(
                                    symbol,
                                    parent_symbol
                                )
                            )
                    });
                }
            }
        }
        None
    }

    pub(super) fn get_referenced_import_declaration(
        &self,
        node_in: &Node, /*Identifier*/
    ) -> Option<Rc<Node /*Declaration*/>> {
        if let Some(node_in_generated_import_reference) = node_in
            .as_identifier()
            .maybe_generated_import_reference()
            .clone()
        {
            return Some(node_in_generated_import_reference);
        }
        let node = get_parse_tree_node(Some(node_in), Some(is_identifier));
        if let Some(node) = node.as_ref() {
            let symbol = self.get_referenced_value_symbol(node, None);
            if self.is_non_local_alias(symbol.as_deref(), Some(SymbolFlags::Value))
                && self
                    .get_type_only_alias_declaration(symbol.as_ref().unwrap())
                    .is_none()
            {
                return self.get_declaration_of_alias_symbol(symbol.as_ref().unwrap());
            }
        }

        None
    }

    pub(super) fn is_symbol_of_destructured_element_of_catch_binding(
        &self,
        symbol: &Symbol,
    ) -> bool {
        matches!(
            symbol.maybe_value_declaration().as_ref(),
            Some(symbol_value_declaration) if is_binding_element(symbol_value_declaration) &&
                walk_up_binding_elements_and_patterns(symbol_value_declaration).parent().kind() == SyntaxKind::CatchClause
        )
    }

    pub(super) fn is_symbol_of_declaration_with_colliding_name(&self, symbol: &Symbol) -> bool {
        if symbol.flags().intersects(SymbolFlags::BlockScoped) {
            if let Some(symbol_value_declaration) = symbol
                .maybe_value_declaration()
                .as_ref()
                .filter(|symbol_value_declaration| !is_source_file(symbol_value_declaration))
            {
                let links = self.get_symbol_links(symbol);
                if (*links)
                    .borrow()
                    .is_declaration_with_colliding_name
                    .is_none()
                {
                    let ref container =
                        get_enclosing_block_scope_container(symbol_value_declaration).unwrap();
                    if is_statement_with_locals(container)
                        || self.is_symbol_of_destructured_element_of_catch_binding(symbol)
                    {
                        let node_links = self.get_node_links(symbol_value_declaration);
                        if self
                            .resolve_name_(
                                container.maybe_parent(),
                                symbol.escaped_name(),
                                SymbolFlags::Value,
                                None,
                                Option::<Rc<Node>>::None,
                                false,
                                None,
                            )
                            .is_some()
                        {
                            links.borrow_mut().is_declaration_with_colliding_name = Some(true);
                        } else if (*node_links)
                            .borrow()
                            .flags
                            .intersects(NodeCheckFlags::CapturedBlockScopedBinding)
                        {
                            let is_declared_in_loop = (*node_links)
                                .borrow()
                                .flags
                                .intersects(NodeCheckFlags::BlockScopedBindingInLoop);
                            let in_loop_initializer = is_iteration_statement(container, false);
                            let in_loop_body_block = container.kind() == SyntaxKind::Block
                                && is_iteration_statement(&container.parent(), false);

                            links.borrow_mut().is_declaration_with_colliding_name = Some(
                                !is_block_scoped_container_top_level(container)
                                    && (!is_declared_in_loop
                                        || !in_loop_initializer && !in_loop_body_block),
                            );
                        } else {
                            links.borrow_mut().is_declaration_with_colliding_name = Some(false);
                        }
                    }
                }
                return (*links)
                    .borrow()
                    .is_declaration_with_colliding_name
                    .unwrap();
            }
        }
        false
    }

    pub(super) fn get_referenced_declaration_with_colliding_name(
        &self,
        node_in: &Node, /*Identifier*/
    ) -> Option<Rc<Node /*Declaration*/>> {
        if !is_generated_identifier(node_in) {
            let node = get_parse_tree_node(Some(node_in), Some(is_identifier));
            if let Some(node) = node.as_ref() {
                let symbol = self.get_referenced_value_symbol(node, None);
                if let Some(symbol) = symbol
                    .as_ref()
                    .filter(|symbol| self.is_symbol_of_declaration_with_colliding_name(symbol))
                {
                    return symbol.maybe_value_declaration();
                }
            }
        }

        None
    }

    pub(super) fn is_declaration_with_colliding_name(
        &self,
        node_in: &Node, /*Declaration*/
    ) -> bool {
        let node = get_parse_tree_node(Some(node_in), Some(is_declaration));
        if let Some(node) = node.as_ref() {
            let symbol = self.get_symbol_of_node(node);
            if let Some(symbol) = symbol.as_ref() {
                return self.is_symbol_of_declaration_with_colliding_name(symbol);
            }
        }

        false
    }

    pub(super) fn is_value_alias_declaration(&self, node: &Node) -> bool {
        match node.kind() {
            SyntaxKind::ImportEqualsDeclaration => {
                self.is_alias_resolved_to_value(self.get_symbol_of_node(node))
            }
            SyntaxKind::ImportClause
            | SyntaxKind::NamespaceImport
            | SyntaxKind::ImportSpecifier
            | SyntaxKind::ExportSpecifier => {
                let symbol = self.get_symbol_of_node(node);
                matches!(
                    symbol.as_ref(),
                    Some(symbol) if self.is_alias_resolved_to_value(Some(&**symbol)) &&
                        self.get_type_only_alias_declaration(
                            symbol
                        ).is_none()
                )
            }
            SyntaxKind::ExportDeclaration => {
                let export_clause = node.as_export_declaration().export_clause.as_ref();
                matches!(
                    export_clause,
                    Some(export_clause) if is_namespace_export(export_clause) ||
                        some(
                            Some(&*export_clause.as_named_exports().elements),
                            Some(|element: &Rc<Node>| self.is_value_alias_declaration(element))
                        )
                )
            }
            SyntaxKind::ExportAssignment => {
                if
                /*(node as ExportAssignment).expression &&*/
                node.as_export_assignment().expression.kind() == SyntaxKind::Identifier {
                    self.is_alias_resolved_to_value(self.get_symbol_of_node(node))
                } else {
                    true
                }
            }
            _ => false,
        }
    }

    pub(super) fn is_top_level_value_import_equals_with_entity_name(
        &self,
        node_in: &Node, /*ImportEqualsDeclaration*/
    ) -> bool {
        let node = get_parse_tree_node(Some(node_in), Some(is_import_equals_declaration));
        if match node.as_ref() {
            None => true,
            Some(node) => {
                node.parent().kind() != SyntaxKind::SourceFile
                    || !is_internal_module_import_equals_declaration(node)
            }
        } {
            return false;
        }
        let node = node.as_ref().unwrap();

        let is_value = self.is_alias_resolved_to_value(self.get_symbol_of_node(node));
        is_value && /*node.moduleReference &&*/
            !node_is_missing(Some(&*node.as_import_equals_declaration().module_reference))
    }

    pub(super) fn is_alias_resolved_to_value<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
    ) -> bool {
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
