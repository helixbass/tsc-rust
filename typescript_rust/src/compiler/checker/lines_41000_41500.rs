use std::{borrow::Borrow, convert::TryInto, io, ptr};

use gc::Gc;
use id_arena::Id;

use super::{is_declaration_name_or_import_property_name, IterationUse};
use crate::{
    cast_present, create_symbol_table, first_or_undefined, for_each, get_check_flags,
    get_enclosing_block_scope_container, get_parse_tree_node, get_source_file_of_node, id_text,
    index_of_node, is_array_literal_expression, is_assignment_pattern, is_binding_element,
    is_binding_pattern, is_block_scoped_container_top_level, is_declaration, is_export_specifier,
    is_expression_node, is_external_module, is_generated_identifier, is_identifier,
    is_import_equals_declaration, is_internal_module_import_equals_declaration,
    is_iteration_statement, is_meta_property, is_module_or_enum_declaration, is_namespace_export,
    is_object_literal_expression, is_part_of_type_node, is_property_access_expression,
    is_property_assignment, is_right_side_of_qualified_name_or_property_access,
    is_shorthand_ambient_module_symbol, is_source_file, is_statement_with_locals, is_static,
    node_is_missing, single_element_array, try_filter, try_find_ancestor, try_flat_map,
    try_for_each_entry_bool,
    try_get_class_implementing_or_extending_expression_with_type_arguments, try_map_defined,
    try_some, type_has_call_or_construct_signatures, walk_up_binding_elements_and_patterns,
    CheckFlags, Debug_, IndexInfo, InterfaceTypeInterface, NamedDeclarationInterface, Node,
    NodeCheckFlags, NodeFlags, NodeInterface, OptionTry, SignatureKind, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind, TransientSymbolInterface, Type, TypeChecker, TypeFlags,
    TypeInterface, UnionOrIntersectionTypeInterface, HasArena, InArena,
};

impl TypeChecker {
    pub(super) fn get_index_infos_at_location_(
        &self,
        node: &Node,
    ) -> io::Result<Option<Vec<Gc<IndexInfo>>>> {
        if is_identifier(node)
            && is_property_access_expression(&node.parent())
            && ptr::eq(&*node.parent().as_property_access_expression().name, node)
        {
            let key_type = self.get_literal_type_from_property_name(node)?;
            let object_type = self.get_type_of_expression(
                &node.parent().as_property_access_expression().expression,
            )?;
            let object_types = if object_type.ref_(self).flags().intersects(TypeFlags::Union) {
                object_type.ref_(self).as_union_type().types().to_owned()
            } else {
                vec![object_type.clone()]
            };
            return Ok(Some(try_flat_map(
                Some(&object_types),
                |&t: &Id<Type>, _| {
                    try_filter(
                        &*self.get_index_infos_of_type(t)?,
                        |info: &Gc<IndexInfo>| {
                            self.is_applicable_index_type(key_type, info.key_type)
                        },
                    )
                },
            )?));
        }
        Ok(None)
    }

    pub(super) fn get_shorthand_assignment_value_symbol_(
        &self,
        location: Option<impl Borrow<Node>>,
    ) -> io::Result<Option<Id<Symbol>>> {
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
        Ok(None)
    }

    pub(super) fn get_export_specifier_local_target_symbol_(
        &self,
        node: &Node, /*Identifier | ExportSpecifier*/
    ) -> io::Result<Option<Id<Symbol>>> {
        Ok(if is_export_specifier(node) {
            let node_as_export_specifier = node.as_export_specifier();
            if node
                .parent()
                .parent()
                .as_export_declaration()
                .module_specifier
                .is_some()
            {
                self.get_external_module_member(&node.parent().parent(), node, None)?
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
                )?
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
            )?
        })
    }

    pub(super) fn get_type_of_node(&self, node: &Node) -> io::Result<Id<Type>> {
        if is_source_file(node) && !is_external_module(node) {
            return Ok(self.error_type());
        }

        if node.flags().intersects(NodeFlags::InWithStatement) {
            return Ok(self.error_type());
        }

        let class_decl =
            try_get_class_implementing_or_extending_expression_with_type_arguments(node);
        let class_type = class_decl.as_ref().try_map(|class_decl| {
            self.get_declared_type_of_class_or_interface(
                self.get_symbol_of_node(&class_decl.class)?.unwrap(),
            )
        })?;
        if is_part_of_type_node(node) {
            let type_from_type_node = self.get_type_from_type_node_(node)?;
            return Ok(if let Some(class_type) = class_type {
                self.get_type_with_this_argument(
                    type_from_type_node,
                    class_type.ref_(self).as_interface_type().maybe_this_type(),
                    None,
                )?
            } else {
                type_from_type_node.clone()
            });
        }

        if is_expression_node(node) {
            return self.get_regular_type_of_expression(node);
        }

        if let Some(class_type) = class_type {
            if !class_decl.as_ref().unwrap().is_implements {
                let base_types = self.get_base_types(class_type)?;
                let base_type = first_or_undefined(&base_types).copied();
                return Ok(if let Some(base_type) = base_type {
                    self.get_type_with_this_argument(
                        base_type,
                        class_type.ref_(self).as_interface_type().maybe_this_type(),
                        None,
                    )?
                } else {
                    self.error_type()
                });
            }
        }

        if self.is_type_declaration(node) {
            let symbol = self.get_symbol_of_node(node)?.unwrap();
            return self.get_declared_type_of_symbol(symbol);
        }

        if self.is_type_declaration_name(node) {
            let symbol = self.get_symbol_at_location_(node, None)?;
            return Ok(if let Some(symbol) = symbol {
                self.get_declared_type_of_symbol(symbol)?
            } else {
                self.error_type()
            });
        }

        if is_declaration(node) {
            let symbol = self.get_symbol_of_node(node)?.unwrap();
            return self.get_type_of_symbol(symbol);
        }

        if is_declaration_name_or_import_property_name(node) {
            let symbol = self.get_symbol_at_location_(node, None)?;
            if let Some(symbol) = symbol {
                return self.get_type_of_symbol(symbol);
            }
            return Ok(self.error_type());
        }

        if is_binding_pattern(Some(node)) {
            return Ok(self
                .get_type_for_variable_like_declaration(&node.parent(), true)?
                .unwrap_or_else(|| self.error_type()));
        }

        if self.is_in_right_side_of_import_or_export_assignment(node) {
            let symbol = self.get_symbol_at_location_(node, None)?;
            if let Some(symbol) = symbol {
                let declared_type = self.get_declared_type_of_symbol(symbol)?;
                return Ok(if !self.is_error_type(declared_type) {
                    declared_type.clone()
                } else {
                    self.get_type_of_symbol(symbol)?
                });
            }
        }

        if is_meta_property(&node.parent())
            && node.parent().as_meta_property().keyword_token == node.kind()
        {
            return self.check_meta_property_keyword(&node.parent());
        }

        Ok(self.error_type())
    }

    pub(super) fn get_type_of_assignment_pattern_(
        &self,
        expr: &Node, /*AssignmentPattern*/
    ) -> io::Result<Option<Id<Type>>> {
        Debug_.assert(
            matches!(
                expr.kind(),
                SyntaxKind::ObjectLiteralExpression | SyntaxKind::ArrayLiteralExpression
            ),
            None,
        );
        if expr.parent().kind() == SyntaxKind::ForOfStatement {
            let iterated_type = self.check_right_hand_side_of_for_of(&expr.parent())?;
            return Ok(Some(self.check_destructuring_assignment(
                expr,
                iterated_type, /*|| errorType*/
                None,
                None,
            )?));
        }
        if expr.parent().kind() == SyntaxKind::BinaryExpression {
            let iterated_type =
                self.get_type_of_expression(&expr.parent().as_binary_expression().right)?;
            return Ok(Some(self.check_destructuring_assignment(
                expr,
                iterated_type, /*|| errorType*/
                None,
                None,
            )?));
        }
        if expr.parent().kind() == SyntaxKind::PropertyAssignment {
            let ref node = cast_present(expr.parent().parent(), |node: &Gc<Node>| {
                is_object_literal_expression(node)
            });
            let type_of_parent_object_literal = self
                .get_type_of_assignment_pattern_(node)?
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
        let node = cast_present(expr.parent(), |node: &Gc<Node>| {
            is_array_literal_expression(node)
        });
        let type_of_array_literal = self
            .get_type_of_assignment_pattern_(&node)?
            .unwrap_or_else(|| self.error_type());
        let element_type = self.check_iterated_type_or_element_type(
            IterationUse::Destructuring,
            type_of_array_literal,
            self.undefined_type(),
            expr.maybe_parent()
        )? /*|| errorType*/;
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
    ) -> io::Result<Option<Id<Symbol>>> {
        let type_of_object_literal = self.get_type_of_assignment_pattern_(&*cast_present(
            location.parent().parent(),
            |node: &Gc<Node>| is_assignment_pattern(node),
        ))?;
        type_of_object_literal.try_and_then(|type_of_object_literal| {
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
    ) -> io::Result<Id<Type>> {
        let mut expr = expr.node_wrapper();
        if is_right_side_of_qualified_name_or_property_access(&expr) {
            expr = expr.parent();
        }
        Ok(self.get_regular_type_of_literal_type(self.get_type_of_expression(&expr)?))
    }

    pub(super) fn get_parent_type_of_class_element(
        &self,
        node: &Node, /*ClassElement*/
    ) -> io::Result<Id<Type>> {
        let class_symbol = self.get_symbol_of_node(&node.parent())?.unwrap();
        Ok(if is_static(node) {
            self.get_type_of_symbol(class_symbol)?
        } else {
            self.get_declared_type_of_symbol(class_symbol)?
        })
    }

    pub(super) fn get_class_element_property_key_type(
        &self,
        element: &Node, /*ClassElement*/
    ) -> io::Result<Id<Type>> {
        let ref name = element.as_named_declaration().name();
        Ok(match name.kind() {
            SyntaxKind::Identifier => self.get_string_literal_type(&id_text(name)),
            SyntaxKind::NumericLiteral | SyntaxKind::StringLiteral => {
                self.get_string_literal_type(&name.as_literal_like_node().text())
            }
            SyntaxKind::ComputedPropertyName => {
                let name_type = self.check_computed_property_name(name)?;
                if self.is_type_assignable_to_kind(name_type, TypeFlags::ESSymbolLike, None)? {
                    name_type.clone()
                } else {
                    self.string_type()
                }
            }
            _ => Debug_.fail(Some("Unsupported property name.")),
        })
    }

    pub(super) fn get_augmented_properties_of_type(
        &self,
        type_: Id<Type>,
    ) -> io::Result<Vec<Id<Symbol>>> {
        let type_ = self.get_apparent_type(type_)?;
        let mut props_by_name =
            create_symbol_table(self.arena(), Some(self.get_properties_of_type(type_)?));
        let function_type = if !self
            .get_signatures_of_type(type_, SignatureKind::Call)?
            .is_empty()
        {
            Some(self.global_callable_function_type())
        } else if !self
            .get_signatures_of_type(type_, SignatureKind::Construct)?
            .is_empty()
        {
            Some(self.global_newable_function_type())
        } else {
            None
        };
        if let Some(function_type) = function_type {
            for_each(
                self.get_properties_of_type(function_type)?,
                |p: Id<Symbol>, _| -> Option<()> {
                    if !props_by_name.contains_key(p.ref_(self).escaped_name()) {
                        props_by_name.insert(p.ref_(self).escaped_name().to_owned(), p.clone());
                    }
                    None
                },
            );
        }
        self.get_named_members(&props_by_name)
    }

    pub(super) fn type_has_call_or_construct_signatures(
        &self,
        type_: Id<Type>,
    ) -> io::Result<bool> {
        type_has_call_or_construct_signatures(type_, self)
    }

    pub fn get_root_symbols(&self, symbol: Id<Symbol>) -> io::Result<Vec<Id<Symbol>>> {
        let roots = self.get_immediate_root_symbols(symbol)?;
        Ok(if let Some(roots) = roots.as_ref() {
            try_flat_map(Some(roots), |&root: &Id<Symbol>, _| {
                self.get_root_symbols(root)
            })?
        } else {
            vec![symbol]
        })
    }

    pub(super) fn get_immediate_root_symbols(
        &self,
        symbol: Id<Symbol>,
    ) -> io::Result<Option<Vec<Id<Symbol>>>> {
        if get_check_flags(&symbol.ref_(self)).intersects(CheckFlags::Synthetic) {
            return Ok(Some(try_map_defined(
                Some(
                    (*self.get_symbol_links(symbol))
                            .borrow()
                            .containing_type
                            .unwrap().ref_(self)
                    .as_union_or_intersection_type_interface()
                    .types(),
                ),
                |&type_: &Id<Type>, _| {
                    self.get_property_of_type_(type_, symbol.ref_(self).escaped_name(), None)
                },
            )?));
        } else if symbol.ref_(self)
            .flags()
            .intersects(SymbolFlags::Transient)
        {
            let (left_spread, right_spread, synthetic_origin) = {
                let symbol_links = symbol.ref_(self).as_transient_symbol().symbol_links();
                let symbol_links = (*symbol_links).borrow();
                (
                    symbol_links.left_spread.clone(),
                    symbol_links.right_spread.clone(),
                    symbol_links.synthetic_origin.clone(),
                )
            };
            return Ok(if let Some(left_spread) = left_spread {
                Some(vec![left_spread, right_spread.unwrap()])
            } else if let Some(synthetic_origin) = synthetic_origin {
                Some(vec![synthetic_origin])
            } else {
                single_element_array(self.try_get_alias_target(symbol))
            });
        }
        Ok(None)
    }

    pub(super) fn try_get_alias_target(&self, symbol: Id<Symbol>) -> Option<Id<Symbol>> {
        let mut target: Option<Id<Symbol>> = None;
        let mut next = Some(symbol);
        while {
            next = (*self.get_symbol_links(next.unwrap()))
                .borrow()
                .target
                .clone();
            next.is_some()
        } {
            target = next.clone();
        }
        target
    }

    pub(super) fn is_arguments_local_binding(
        &self,
        node_in: &Node, /*Identifier*/
    ) -> io::Result<bool> {
        if is_generated_identifier(node_in) {
            return Ok(false);
        }
        let node = get_parse_tree_node(Some(node_in), Some(is_identifier));
        if node.is_none() {
            return Ok(false);
        }
        let ref node = node.unwrap();
        let parent = node.maybe_parent();
        if parent.is_none() {
            return Ok(false);
        }
        let ref parent = parent.unwrap();
        let is_property_name = (is_property_access_expression(parent)
            || is_property_assignment(parent))
            && Gc::ptr_eq(&parent.as_named_declaration().name(), node);
        Ok(!is_property_name
            && matches!(
                self.get_referenced_value_symbol(
                    node,
                    None,
                )?,
                Some(referenced_value_symbol) if referenced_value_symbol == self.arguments_symbol()
            ))
    }

    pub(super) fn module_exports_some_value(
        &self,
        module_reference_expression: &Node, /*Expression*/
    ) -> io::Result<bool> {
        let module_symbol = self.resolve_external_module_name_(
            &module_reference_expression.parent(),
            module_reference_expression,
            None,
        )?;
        if match module_symbol {
            None => true,
            Some(module_symbol) => is_shorthand_ambient_module_symbol(&module_symbol.ref_(self)),
        } {
            return Ok(true);
        }
        let module_symbol = module_symbol.unwrap();

        let has_export_assignment = self.has_export_assignment_symbol(module_symbol);
        let module_symbol = self
            .resolve_external_module_symbol(Some(module_symbol), None)?
            .unwrap();

        let symbol_links = self.get_symbol_links(module_symbol);
        if (*symbol_links).borrow().exports_some_value.is_none() {
            symbol_links.borrow_mut().exports_some_value = Some(if has_export_assignment {
                module_symbol.ref_(self)
                    .flags()
                    .intersects(SymbolFlags::Value)
            } else {
                try_for_each_entry_bool(
                    &*(*self.get_exports_of_module_(module_symbol)?).borrow(),
                    |&s: &Id<Symbol>, _| self.is_value(s),
                )?
            });
        }
        let ret = (*symbol_links).borrow().exports_some_value.unwrap();
        Ok(ret)
    }

    pub(super) fn is_value(&self, s: Id<Symbol>) -> io::Result<bool> {
        let s = self.resolve_symbol(Some(s), None)?;
        Ok(matches!(
            s,
            Some(s) if s.ref_(self).flags().intersects(SymbolFlags::Value)
        ))
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
    ) -> io::Result<Option<Gc<Node /*SourceFile | ModuleDeclaration | EnumDeclaration*/>>> {
        let node = get_parse_tree_node(Some(node_in), Some(is_identifier));
        if let Some(node) = node.as_ref() {
            let symbol = self.get_referenced_value_symbol(
                node,
                Some(self.is_name_of_module_or_enum_declaration(node)),
            )?;
            if let Some(mut symbol) = symbol {
                if symbol.ref_(self)
                    .flags()
                    .intersects(SymbolFlags::ExportValue)
                {
                    let export_symbol = self
                        .get_merged_symbol(symbol.ref_(self).maybe_export_symbol())
                        .unwrap();
                    if prefix_locals != Some(true)
                        && export_symbol.ref_(self)
                            .flags()
                            .intersects(SymbolFlags::ExportHasLocal)
                        && !export_symbol.ref_(self)
                            .flags()
                            .intersects(SymbolFlags::Variable)
                    {
                        return Ok(None);
                    }
                    symbol = export_symbol.clone();
                }
                let parent_symbol = self.get_parent_of_symbol(symbol)?;
                if let Some(parent_symbol) = parent_symbol {
                    if parent_symbol.ref_(self)
                        .flags()
                        .intersects(SymbolFlags::ValueModule)
                    {
                        if let Some(parent_symbol_value_declaration) = parent_symbol.ref_(self)
                            .maybe_value_declaration()
                            .as_ref()
                            .filter(|parent_symbol_value_declaration| {
                                parent_symbol_value_declaration.kind() == SyntaxKind::SourceFile
                            })
                        {
                            let symbol_file = parent_symbol_value_declaration;
                            let ref reference_file = get_source_file_of_node(node);
                            let symbol_is_umd_export = !Gc::ptr_eq(symbol_file, reference_file);
                            return Ok(if symbol_is_umd_export {
                                None
                            } else {
                                Some(symbol_file.clone())
                            });
                        }
                    }
                    return try_find_ancestor(node.maybe_parent(), |n| -> io::Result<_> {
                        Ok(is_module_or_enum_declaration(n)
                            && matches!(
                                self.get_symbol_of_node(n)?,
                                Some(symbol) if symbol == parent_symbol
                            ))
                    });
                }
            }
        }
        Ok(None)
    }

    pub(super) fn get_referenced_import_declaration(
        &self,
        node_in: &Node, /*Identifier*/
    ) -> io::Result<Option<Gc<Node /*Declaration*/>>> {
        if let Some(node_in_generated_import_reference) = node_in
            .as_identifier()
            .maybe_generated_import_reference()
            .clone()
        {
            return Ok(Some(node_in_generated_import_reference));
        }
        let node = get_parse_tree_node(Some(node_in), Some(is_identifier));
        if let Some(node) = node.as_ref() {
            let symbol = self.get_referenced_value_symbol(node, None)?;
            if self.is_non_local_alias(symbol, Some(SymbolFlags::Value))
                && self
                    .get_type_only_alias_declaration(symbol.unwrap())
                    .is_none()
            {
                return self.get_declaration_of_alias_symbol(symbol.unwrap());
            }
        }

        Ok(None)
    }

    pub(super) fn is_symbol_of_destructured_element_of_catch_binding(
        &self,
        symbol: Id<Symbol>,
    ) -> bool {
        matches!(
            symbol.ref_(self).maybe_value_declaration().as_ref(),
            Some(symbol_value_declaration) if is_binding_element(symbol_value_declaration) &&
                walk_up_binding_elements_and_patterns(symbol_value_declaration).parent().kind() == SyntaxKind::CatchClause
        )
    }

    pub(super) fn is_symbol_of_declaration_with_colliding_name(
        &self,
        symbol: Id<Symbol>,
    ) -> io::Result<bool> {
        if symbol.ref_(self)
            .flags()
            .intersects(SymbolFlags::BlockScoped)
        {
            if let Some(symbol_value_declaration) = symbol.ref_(self)
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
                                symbol.ref_(self).escaped_name(),
                                SymbolFlags::Value,
                                None,
                                Option::<Gc<Node>>::None,
                                false,
                                None,
                            )?
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
                return Ok((*links).borrow().is_declaration_with_colliding_name == Some(true));
            }
        }
        Ok(false)
    }

    pub(super) fn get_referenced_declaration_with_colliding_name(
        &self,
        node_in: &Node, /*Identifier*/
    ) -> io::Result<Option<Gc<Node /*Declaration*/>>> {
        if !is_generated_identifier(node_in) {
            let node = get_parse_tree_node(Some(node_in), Some(is_identifier));
            if let Some(node) = node.as_ref() {
                let symbol = self.get_referenced_value_symbol(node, None)?;
                if let Some(symbol) = symbol.try_filter(|&symbol| {
                    self.is_symbol_of_declaration_with_colliding_name(symbol)
                })? {
                    return Ok(symbol.ref_(self).maybe_value_declaration());
                }
            }
        }

        Ok(None)
    }

    pub(super) fn is_declaration_with_colliding_name(
        &self,
        node_in: &Node, /*Declaration*/
    ) -> io::Result<bool> {
        let node = get_parse_tree_node(Some(node_in), Some(is_declaration));
        if let Some(node) = node.as_ref() {
            let symbol = self.get_symbol_of_node(node)?;
            if let Some(symbol) = symbol {
                return self.is_symbol_of_declaration_with_colliding_name(symbol);
            }
        }

        Ok(false)
    }

    pub(super) fn is_value_alias_declaration(&self, node: &Node) -> io::Result<bool> {
        Ok(match node.kind() {
            SyntaxKind::ImportEqualsDeclaration => {
                self.is_alias_resolved_to_value(self.get_symbol_of_node(node)?)?
            }
            SyntaxKind::ImportClause
            | SyntaxKind::NamespaceImport
            | SyntaxKind::ImportSpecifier
            | SyntaxKind::ExportSpecifier => {
                let symbol = self.get_symbol_of_node(node)?;
                matches!(
                    symbol,
                    Some(symbol) if self.is_alias_resolved_to_value(Some(symbol))? &&
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
                        try_some(
                            Some(&*export_clause.as_named_exports().elements),
                            Some(|element: &Gc<Node>| self.is_value_alias_declaration(element))
                        )?
                )
            }
            SyntaxKind::ExportAssignment => {
                if
                /*(node as ExportAssignment).expression &&*/
                node.as_export_assignment().expression.kind() == SyntaxKind::Identifier {
                    self.is_alias_resolved_to_value(self.get_symbol_of_node(node)?)?
                } else {
                    true
                }
            }
            _ => false,
        })
    }

    pub(super) fn is_top_level_value_import_equals_with_entity_name(
        &self,
        node_in: &Node, /*ImportEqualsDeclaration*/
    ) -> io::Result<bool> {
        let node = get_parse_tree_node(Some(node_in), Some(is_import_equals_declaration));
        if match node.as_ref() {
            None => true,
            Some(node) => {
                node.parent().kind() != SyntaxKind::SourceFile
                    || !is_internal_module_import_equals_declaration(node)
            }
        } {
            return Ok(false);
        }
        let node = node.as_ref().unwrap();

        let is_value = self.is_alias_resolved_to_value(self.get_symbol_of_node(node)?)?;
        Ok(is_value && /*node.moduleReference &&*/
            !node_is_missing(Some(&*node.as_import_equals_declaration().module_reference)))
    }
}
