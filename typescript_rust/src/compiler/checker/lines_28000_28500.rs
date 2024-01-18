use std::{borrow::Borrow, io, ptr};

use gc::Gc;
use id_arena::Id;

use super::{anon, CheckMode};
use crate::{
    add_related_info, chain_diagnostic_messages, create_diagnostic_for_node,
    create_diagnostic_for_node_from_message_chain, declaration_name_to_string, find_ancestor,
    for_each, get_assignment_declaration_property_access_kind, get_assignment_target_kind,
    get_containing_class, get_source_file_of_node, get_symbol_name_for_private_identifier,
    get_this_container, get_this_parameter, id_text, is_access_expression, is_assignment_target,
    is_block, is_call_or_new_expression, is_class_static_block_declaration, is_delete_target,
    is_expression_node, is_function_like, is_identifier, is_method_declaration,
    is_named_declaration, is_part_of_type_query, is_private_identifier,
    is_property_access_expression, is_static, is_this_identifier, is_this_property,
    is_write_access, maybe_for_each, maybe_get_source_file_of_node, should_preserve_const_enums,
    symbol_name, unescape_leading_underscores, AssignmentDeclarationKind, AssignmentKind, Debug_,
    Diagnostic, DiagnosticMessageChain, DiagnosticRelatedInformation, Diagnostics,
    ExternalEmitHelpers, FindAncestorCallbackReturn, HasArena, InArena, Node, NodeFlags,
    NodeInterface, OptionTry, ScriptKind, ScriptTarget, Symbol, SymbolFlags, SymbolInterface,
    SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface, UnionOrIntersectionTypeInterface,
};

impl TypeChecker {
    pub(super) fn get_this_parameter_from_node_context(&self, node: Id<Node>) -> Option<Id<Node>> {
        let this_container = get_this_container(node, false, self);
        /*thisContainer &&*/
        if is_function_like(Some(&this_container.ref_(self))) {
            get_this_parameter(this_container, self)
        } else {
            None
        }
    }

    pub(super) fn symbol_has_non_method_declaration(&self, symbol: Id<Symbol>) -> io::Result<bool> {
        self.for_each_property_bool(symbol, &mut |prop: Id<Symbol>| {
            Ok(!prop.ref_(self).flags().intersects(SymbolFlags::Method))
        })
    }

    pub(super) fn check_non_null_expression(
        &self,
        node: Id<Node>, /*Expression | QualifiedName*/
    ) -> io::Result<Id<Type>> {
        self.check_non_null_type(self.check_expression(node, None, None)?, node)
    }

    pub(super) fn is_nullable_type(&self, type_: Id<Type>) -> bool {
        if self.strict_null_checks {
            self.get_falsy_flags(type_)
        } else {
            type_.ref_(self).flags()
        }
        .intersects(TypeFlags::Nullable)
    }

    pub(super) fn get_non_nullable_type_if_needed(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        Ok(if self.is_nullable_type(type_) {
            self.get_non_nullable_type(type_)?
        } else {
            type_
        })
    }

    pub(super) fn report_object_possibly_null_or_undefined_error(
        &self,
        node: Id<Node>,
        flags: TypeFlags,
    ) {
        self.error(
            Some(node),
            if flags.intersects(TypeFlags::Undefined) {
                if flags.intersects(TypeFlags::Null) {
                    &*Diagnostics::Object_is_possibly_null_or_undefined
                } else {
                    &*Diagnostics::Object_is_possibly_undefined
                }
            } else {
                &*Diagnostics::Object_is_possibly_null
            },
            None,
        );
    }

    pub(super) fn report_cannot_invoke_possibly_null_or_undefined_error(
        &self,
        node: Id<Node>,
        flags: TypeFlags,
    ) {
        self.error(
            Some(node),
            if flags.intersects(TypeFlags::Undefined) {
                if flags.intersects(TypeFlags::Null) {
                    &*Diagnostics::Cannot_invoke_an_object_which_is_possibly_null_or_undefined
                } else {
                    &*Diagnostics::Cannot_invoke_an_object_which_is_possibly_undefined
                }
            } else {
                &*Diagnostics::Cannot_invoke_an_object_which_is_possibly_null
            },
            None,
        );
    }

    pub(super) fn check_non_null_type_with_reporter(
        &self,
        type_: Id<Type>,
        node: Id<Node>,
        mut report_error: impl FnMut(Id<Node>, TypeFlags),
    ) -> io::Result<Id<Type>> {
        if self.strict_null_checks && type_.ref_(self).flags().intersects(TypeFlags::Unknown) {
            self.error(Some(node), &Diagnostics::Object_is_of_type_unknown, None);
            return Ok(self.error_type());
        }
        let kind = if self.strict_null_checks {
            self.get_falsy_flags(type_)
        } else {
            type_.ref_(self).flags()
        } & TypeFlags::Nullable;
        if kind != TypeFlags::None {
            report_error(node, kind);
            let t = self.get_non_nullable_type(type_)?;
            return Ok(
                if t.ref_(self)
                    .flags()
                    .intersects(TypeFlags::Nullable | TypeFlags::Never)
                {
                    self.error_type()
                } else {
                    t
                },
            );
        }
        Ok(type_)
    }

    pub(super) fn check_non_null_type(
        &self,
        type_: Id<Type>,
        node: Id<Node>,
    ) -> io::Result<Id<Type>> {
        self.check_non_null_type_with_reporter(type_, node, |node: Id<Node>, flags: TypeFlags| {
            self.report_object_possibly_null_or_undefined_error(node, flags)
        })
    }

    pub(super) fn check_non_null_non_void_type(
        &self,
        type_: Id<Type>,
        node: Id<Node>,
    ) -> io::Result<Id<Type>> {
        let non_null_type = self.check_non_null_type(type_, node)?;
        if non_null_type.ref_(self).flags().intersects(TypeFlags::Void) {
            self.error(Some(node), &Diagnostics::Object_is_possibly_undefined, None);
        }
        Ok(non_null_type)
    }

    pub(super) fn check_property_access_expression(
        &self,
        node: Id<Node>, /*PropertyAccessExpression*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let node_ref = node.ref_(self);
        let node_as_property_access_expression = node_ref.as_property_access_expression();
        Ok(if node.ref_(self).flags().intersects(NodeFlags::OptionalChain) {
            self.check_property_access_chain(node, check_mode)?
        } else {
            self.check_property_access_expression_or_qualified_name(
                node,
                &node_as_property_access_expression.expression,
                self.check_non_null_expression(&node_as_property_access_expression.expression)?,
                &node_as_property_access_expression.name,
                check_mode,
            )?
        })
    }

    pub(super) fn check_property_access_chain(
        &self,
        node: Id<Node>, /*PropertyAccessChain*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let node_ref = node.ref_(self);
        let node_as_property_access_expression = node_ref.as_property_access_expression();
        let left_type =
            self.check_expression(&node_as_property_access_expression.expression, None, None)?;
        let non_optional_type = self.get_optional_expression_type(
            left_type,
            &node_as_property_access_expression.expression,
        )?;
        self.propagate_optional_type_marker(
            self.check_property_access_expression_or_qualified_name(
                node,
                &node_as_property_access_expression.expression,
                self.check_non_null_type(
                    non_optional_type,
                    &node_as_property_access_expression.expression,
                )?,
                &node_as_property_access_expression.name,
                check_mode,
            )?,
            node,
            non_optional_type != left_type,
        )
    }

    pub(super) fn check_qualified_name(
        &self,
        node: Id<Node>, /*QualifiedName*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let node_ref = node.ref_(self);
        let node_as_qualified_name = node_ref.as_qualified_name();
        let left_type = if is_part_of_type_query(node, self)
            && is_this_identifier(Some(&*node_as_qualified_name.left))
        {
            self.check_non_null_type(
                self.check_this_expression(&node_as_qualified_name.left)?,
                &node_as_qualified_name.left,
            )?
        } else {
            self.check_non_null_expression(&node_as_qualified_name.left)?
        };
        self.check_property_access_expression_or_qualified_name(
            node,
            &node_as_qualified_name.left,
            left_type,
            &node_as_qualified_name.right,
            check_mode,
        )
    }

    pub(super) fn is_method_access_for_call(&self, mut node: Id<Node>) -> bool {
        while node.ref_(self).parent().ref_(self).kind() == SyntaxKind::ParenthesizedExpression {
            node = node.ref_(self).parent();
        }
        is_call_or_new_expression(&node.ref_(self).parent().ref_(self))
            && node.ref_(self).parent().as_has_expression().expression() == node
    }

    pub(super) fn lookup_symbol_for_private_identifier_declaration(
        &self,
        prop_name: &str, /*__String*/
        location: Id<Node>,
    ) -> Option<Id<Symbol>> {
        let mut containing_class = get_containing_class(location, self);
        while let Some(containing_class_present) = containing_class {
            let symbol = containing_class_present.ref_(self).symbol();
            let name = get_symbol_name_for_private_identifier(&symbol.ref_(self), prop_name);
            let prop = symbol
                .ref_(self)
                .maybe_members()
                .as_ref()
                .and_then(|symbol_members| (**symbol_members).borrow().get(&name).cloned())
                .or_else(|| {
                    symbol
                        .ref_(self)
                        .maybe_exports()
                        .as_ref()
                        .and_then(|symbol_exports| (**symbol_exports).borrow().get(&name).cloned())
                });
            if prop.is_some() {
                return prop;
            }
            containing_class = get_containing_class(containing_class_present, self);
        }
        None
    }

    pub(super) fn check_grammar_private_identifier_expression(
        &self,
        priv_id: Id<Node>, /*PrivateIdentifier*/
    ) -> bool {
        if get_containing_class(priv_id, self).is_none() {
            return self.grammar_error_on_node(
                priv_id,
                &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                None,
            );
        }
        if !is_expression_node(priv_id, self) {
            return self.grammar_error_on_node(
                priv_id,
                &Diagnostics::Private_identifiers_are_only_allowed_in_class_bodies_and_may_only_be_used_as_part_of_a_class_member_declaration_property_access_or_on_the_left_hand_side_of_an_in_expression,
                None,
            );
        }
        if self
            .get_symbol_for_private_identifier_expression(priv_id)
            .is_none()
        {
            return self.grammar_error_on_node(
                priv_id,
                &Diagnostics::Cannot_find_name_0,
                Some(vec![id_text(&priv_id.ref_(self)).to_owned()]),
            );
        }
        false
    }

    pub(super) fn check_private_identifier_expression(
        &self,
        priv_id: Id<Node>, /*PrivateIdentifier*/
    ) -> Id<Type> {
        self.check_grammar_private_identifier_expression(priv_id);
        let symbol = self.get_symbol_for_private_identifier_expression(priv_id);
        if let Some(symbol) = symbol {
            self.mark_property_as_referenced(symbol, Option::<Id<Node>>::None, false);
        }
        self.any_type()
    }

    pub(super) fn get_symbol_for_private_identifier_expression(
        &self,
        priv_id: Id<Node>, /*PrivateIdentifier*/
    ) -> Option<Id<Symbol>> {
        if !is_expression_node(priv_id, self) {
            return None;
        }

        let links = self.get_node_links(priv_id);
        if (*links).borrow().resolved_symbol.is_none() {
            links.borrow_mut().resolved_symbol = self
                .lookup_symbol_for_private_identifier_declaration(
                    &priv_id.ref_(self).as_private_identifier().escaped_text,
                    priv_id,
                );
        }
        let ret = (*links).borrow().resolved_symbol.clone();
        ret
    }

    pub(super) fn get_private_identifier_property_of_type_(
        &self,
        left_type: Id<Type>,
        lexically_scoped_identifier: Id<Symbol>,
    ) -> io::Result<Option<Id<Symbol>>> {
        self.get_property_of_type_(
            left_type,
            lexically_scoped_identifier.ref_(self).escaped_name(),
            None,
        )
    }

    pub(super) fn check_private_identifier_property_access(
        &self,
        left_type: Id<Type>,
        right: Id<Node>, /*PrivateIdentifier*/
        lexically_scoped_identifier: Option<Id<Symbol>>,
    ) -> io::Result<bool> {
        let mut property_on_type: Option<Id<Symbol>> = None;
        let properties = self.get_properties_of_type(left_type)?;
        // if (properties) {
        let right_ref = right.ref_(self);
        let right_as_private_identifier = right_ref.as_private_identifier();
        for_each(properties, |symbol: Id<Symbol>, _| {
            let decl = symbol.ref_(self).maybe_value_declaration();
            if matches!(
                decl,
                Some(decl) if is_named_declaration(&decl.ref_(self)) &&
                    is_private_identifier(&decl.ref_(self).as_named_declaration().name().ref_(self)) &&
                    decl.ref_(self).as_named_declaration().name().ref_(self).as_identifier().escaped_text == right_as_private_identifier.escaped_text
            ) {
                property_on_type = Some(symbol.clone());
                return Some(());
            }
            None
        });
        // }
        let diag_name = self
            .diagnostic_name(right.into())
            .into_owned();
        if let Some(property_on_type) = property_on_type {
            let type_value_decl =
                Debug_.check_defined(property_on_type.ref_(self).maybe_value_declaration(), None);
            let type_class = Debug_.check_defined(get_containing_class(type_value_decl, self), None);
            if let Some(lexically_scoped_identifier_value_declaration) = lexically_scoped_identifier
                .and_then(|lexically_scoped_identifier| {
                    lexically_scoped_identifier
                        .ref_(self)
                        .maybe_value_declaration()
                })
            {
                let lexical_value_decl = lexically_scoped_identifier_value_declaration;
                let lexical_class = get_containing_class(lexical_value_decl, self);
                Debug_.assert(lexical_class.is_some(), None);
                let lexical_class = lexical_class.unwrap();
                if find_ancestor(Some(lexical_class), |n: Id<Node>| {
                    type_class == n
                }, self)
                .is_some()
                {
                    let diagnostic = self.error(
                        Some(right),
                        &Diagnostics::The_property_0_cannot_be_accessed_on_type_1_within_this_class_because_it_is_shadowed_by_another_private_identifier_with_the_same_spelling,
                        Some(vec![
                            diag_name.clone(),
                            self.type_to_string_(
                                left_type,
                                Option::<Id<Node>>::None,
                                None, None,
                            )?
                        ])
                    );

                    add_related_info(
                        &diagnostic,
                        vec![
                            Gc::new(
                                create_diagnostic_for_node(
                                    lexical_value_decl,
                                    &Diagnostics::The_shadowing_declaration_of_0_is_defined_here,
                                    Some(vec![
                                        diag_name.clone(),
                                    ]),
                                    self,
                                ).into()
                            ),
                            Gc::new(
                                create_diagnostic_for_node(
                                    type_value_decl,
                                    &Diagnostics::The_declaration_of_0_that_you_probably_intended_to_use_is_defined_here,
                                    Some(vec![
                                        diag_name.clone(),
                                    ]),
                                    self,
                                ).into()
                            ),
                        ]
                    );
                    return Ok(true);
                }
            }
            self.error(
                Some(right),
                &Diagnostics::Property_0_is_not_accessible_outside_class_1_because_it_has_a_private_identifier,
                Some(vec![
                    diag_name,
                    self.diagnostic_name(
                        type_class.ref_(self).as_named_declaration().maybe_name().map_or_else(|| {
                            anon.into()
                        }, Into::into)
                    ).into_owned()
                ])
            );
            return Ok(true);
        }
        Ok(false)
    }

    pub(super) fn is_this_property_access_in_constructor(
        &self,
        node: Id<Node>, /*ElementAccessExpression | PropertyAccessExpression | QualifiedName*/
        prop: Id<Symbol>,
    ) -> io::Result<bool> {
        Ok((self.is_constructor_declared_property(prop)?
            || is_this_property(node, self) && self.is_auto_typed_property(prop))
            && self.get_declaring_constructor(prop)? == Some(get_this_container(node, true, self)))
    }

    pub(super) fn check_property_access_expression_or_qualified_name(
        &self,
        node: Id<Node>, /*PropertyAccessExpression | QualifiedName*/
        left: Id<Node>, /*Expression | QualifiedName*/
        left_type: Id<Type>,
        right: Id<Node>, /*Identifier | PrivateIdentifier*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let parent_symbol = (*self.get_node_links(left))
            .borrow()
            .resolved_symbol
            .clone();
        let assignment_kind = get_assignment_target_kind(node, self);
        let apparent_type = self.get_apparent_type(
            if assignment_kind != AssignmentKind::None || self.is_method_access_for_call(node) {
                self.get_widened_type(left_type)?
            } else {
                left_type
            },
        )?;
        let is_any_like =
            self.is_type_any(Some(apparent_type)) || apparent_type == self.silent_never_type();
        let prop: Option<Id<Symbol>>;
        if is_private_identifier(&right.ref_(self)) {
            if self.language_version < ScriptTarget::ESNext {
                if assignment_kind != AssignmentKind::None {
                    self.check_external_emit_helpers(
                        node,
                        ExternalEmitHelpers::ClassPrivateFieldSet,
                    )?;
                }
                if assignment_kind != AssignmentKind::Definite {
                    self.check_external_emit_helpers(
                        node,
                        ExternalEmitHelpers::ClassPrivateFieldGet,
                    )?;
                }
            }

            let lexically_scoped_symbol = self.lookup_symbol_for_private_identifier_declaration(
                &right.ref_(self).as_private_identifier().escaped_text,
                right,
            );
            if assignment_kind != AssignmentKind::None
                && matches!(
                    lexically_scoped_symbol.and_then(|lexically_scoped_symbol| lexically_scoped_symbol.ref_(self).maybe_value_declaration()),
                    Some(lexically_scoped_symbol_value_declaration) if is_method_declaration(&lexically_scoped_symbol_value_declaration.ref_(self))
                )
            {
                self.grammar_error_on_node(
                    right,
                    &Diagnostics::Cannot_assign_to_private_method_0_Private_methods_are_not_writable,
                    Some(vec![
                        id_text(&right.ref_(self)).to_owned()
                    ])
                );
            }

            if is_any_like {
                if lexically_scoped_symbol.is_some() {
                    return Ok(if self.is_error_type(apparent_type) {
                        self.error_type()
                    } else {
                        apparent_type
                    });
                }
                if get_containing_class(right, self).is_none() {
                    self.grammar_error_on_node(
                        right,
                        &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                        None,
                    );
                    return Ok(self.any_type());
                }
            }
            prop = lexically_scoped_symbol.try_and_then(|lexically_scoped_symbol| {
                self.get_private_identifier_property_of_type_(left_type, lexically_scoped_symbol)
            })?;
            if prop.is_none()
                && self.check_private_identifier_property_access(
                    left_type,
                    right,
                    lexically_scoped_symbol,
                )?
            {
                return Ok(self.error_type());
            } else {
                let is_setonly_accessor = matches!(
                    prop,
                    Some(prop) if prop.ref_(self).flags().intersects(SymbolFlags::SetAccessor) &&
                        !prop.ref_(self).flags().intersects(SymbolFlags::GetAccessor)
                );
                if is_setonly_accessor && assignment_kind != AssignmentKind::Definite {
                    self.error(
                        Some(node),
                        &Diagnostics::Private_accessor_was_defined_without_a_getter,
                        None,
                    );
                }
            }
        } else {
            if is_any_like {
                if is_identifier(&left.ref_(self)) {
                    if let Some(parent_symbol) = parent_symbol {
                        self.mark_alias_referenced(parent_symbol, node)?;
                    }
                }
                return Ok(if self.is_error_type(apparent_type) {
                    self.error_type()
                } else {
                    apparent_type
                });
            }
            prop = self.get_property_of_type_(
                apparent_type,
                &right.ref_(self).as_identifier().escaped_text,
                None,
            )?;
        }
        if is_identifier(&left.ref_(self)) {
            if let Some(parent_symbol) = parent_symbol {
                if self.compiler_options.isolated_modules == Some(true)
                    || !matches!(
                        prop,
                        Some(prop) if self.is_const_enum_or_const_enum_only_module(prop)
                    )
                    || should_preserve_const_enums(&self.compiler_options)
                        && self.is_export_or_export_expression(node)
                {
                    self.mark_alias_referenced(parent_symbol, node)?;
                }
            }
        }

        let prop_type: Id<Type>;
        match prop {
            None => {
                let index_info = if !is_private_identifier(&right.ref_(self))
                    && (assignment_kind == AssignmentKind::None
                        || !self.is_generic_object_type(left_type)?
                        || self.is_this_type_parameter(left_type))
                {
                    self.get_applicable_index_info_for_name(
                        apparent_type,
                        &right.ref_(self).as_identifier().escaped_text,
                    )?
                } else {
                    None
                };
                if !index_info.is_some()
                /*&& indexInfo.type*/
                {
                    let is_unchecked_js = self.is_unchecked_js_suggestion(
                        Some(node),
                        left_type.ref_(self).maybe_symbol(),
                        true,
                    );
                    if !is_unchecked_js && self.is_js_literal_type(left_type)? {
                        return Ok(self.any_type());
                    }
                    if matches!(
                        left_type.ref_(self).maybe_symbol(),
                        Some(left_type_symbol) if left_type_symbol == self.global_this_symbol()
                    ) {
                        let global_this_symbol_exports = self
                            .global_this_symbol()
                            .ref_(self)
                            .maybe_exports()
                            .clone()
                            .unwrap();
                        if (*global_this_symbol_exports)
                            .borrow()
                            .contains_key(right.ref_(self).as_member_name().escaped_text())
                            && (*global_this_symbol_exports)
                                .borrow()
                                .get(right.ref_(self).as_member_name().escaped_text())
                                .copied()
                                .unwrap()
                                .ref_(self)
                                .flags()
                                .intersects(SymbolFlags::BlockScoped)
                        {
                            self.error(
                                Some(right),
                                &Diagnostics::Property_0_does_not_exist_on_type_1,
                                Some(vec![
                                    unescape_leading_underscores(
                                        &right.ref_(self).as_member_name().escaped_text(),
                                    )
                                    .to_owned(),
                                    self.type_to_string_(
                                        left_type,
                                        Option::<Id<Node>>::None,
                                        None,
                                        None,
                                    )?,
                                ]),
                            );
                        } else if self.no_implicit_any {
                            self.error(
                                Some(right),
                                &Diagnostics::Element_implicitly_has_an_any_type_because_type_0_has_no_index_signature,
                                Some(vec![
                                    self.type_to_string_(
                                        left_type,
                                        Option::<Id<Node>>::None,
                                        None, None,
                                    )?
                                ])
                            );
                        }
                        return Ok(self.any_type());
                    }
                    if !right.ref_(self).as_member_name().escaped_text().is_empty()
                        && !self.check_and_report_error_for_extending_interface(node)?
                    {
                        self.report_nonexistent_property(
                            right,
                            if self.is_this_type_parameter(left_type) {
                                apparent_type
                            } else {
                                left_type
                            },
                            is_unchecked_js,
                        )?;
                    }
                    return Ok(self.error_type());
                }
                let index_info = index_info.unwrap();
                if index_info.is_readonly && (is_assignment_target(node, self) || is_delete_target(node, self))
                {
                    self.error(
                        Some(node),
                        &Diagnostics::Index_signature_in_type_0_only_permits_reading,
                        Some(vec![self.type_to_string_(
                            apparent_type,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?]),
                    );
                }

                prop_type = if self.compiler_options.no_unchecked_indexed_access == Some(true)
                    && !is_assignment_target(node, self)
                {
                    self.get_union_type(
                        &[index_info.type_.clone(), self.undefined_type()],
                        None,
                        Option::<Id<Symbol>>::None,
                        None,
                        None,
                    )?
                } else {
                    index_info.type_.clone()
                };
                if self
                    .compiler_options
                    .no_property_access_from_index_signature
                    == Some(true)
                    && is_property_access_expression(&node.ref_(self))
                {
                    self.error(
                        Some(right),
                        &Diagnostics::Property_0_comes_from_an_index_signature_so_it_must_be_accessed_with_0,
                        Some(vec![
                            unescape_leading_underscores(&right.ref_(self).as_member_name().escaped_text()).to_owned()
                        ])
                    );
                }
            }
            Some(prop) => {
                if let Some(prop_declarations) = prop.ref_(self).maybe_declarations().as_ref() {
                    if self
                        .get_declaration_node_flags_from_symbol(prop)
                        .intersects(NodeFlags::Deprecated)
                        && self.is_uncalled_function_reference(node, prop)?
                    {
                        self.add_deprecated_suggestion(
                            right,
                            prop_declarations,
                            &*right.ref_(self).as_member_name().escaped_text(),
                        );
                    }
                }
                self.check_property_not_used_before_declaration(prop, node, right)?;
                self.mark_property_as_referenced(
                    prop,
                    Some(node),
                    self.is_self_type_access(left, parent_symbol)?,
                );
                self.get_node_links(node).borrow_mut().resolved_symbol = Some(prop.clone());
                let writing = is_write_access(node, self);
                self.check_property_accessibility(
                    node,
                    left.ref_(self).kind() == SyntaxKind::SuperKeyword,
                    writing,
                    apparent_type,
                    prop,
                    None,
                )?;
                if self.is_assignment_to_readonly_entity(node, prop, assignment_kind)? {
                    self.error(
                        Some(right),
                        &Diagnostics::Cannot_assign_to_0_because_it_is_a_read_only_property,
                        Some(vec![id_text(ref_(self).right).to_owned()]),
                    );
                    return Ok(self.error_type());
                }

                prop_type = if self.is_this_property_access_in_constructor(node, prop)? {
                    self.auto_type()
                } else if writing {
                    self.get_set_accessor_type_of_symbol(prop)?
                } else {
                    self.get_type_of_symbol(prop)?
                };
            }
        }

        self.get_flow_type_of_access_expression(node, prop, prop_type, right, check_mode)
    }

    pub(super) fn is_unchecked_js_suggestion(
        &self,
        node: Option<Id<Node>>,
        suggestion: Option<Id<Symbol>>,
        exclude_classes: bool,
    ) -> bool {
        let file = maybe_get_source_file_of_node(node, self);
        if let Some(file) = file {
            let file_ref = file.ref_(self);
            let file_as_source_file = file_ref.as_source_file();
            if self.compiler_options.check_js.is_none()
                && file_as_source_file.maybe_check_js_directive().is_none()
                && matches!(
                    file_as_source_file.script_kind(),
                    ScriptKind::JS | ScriptKind::JSX
                )
            {
                let declaration_file = maybe_for_each(
                    suggestion
                        .and_then(|suggestion| suggestion.ref_(self).maybe_declarations().clone())
                        .as_ref(),
                    |&declaration: &Id<Node>, _| maybe_get_source_file_of_node(Some(declaration), self),
                );
                return !matches!(
                    declaration_file,
                    Some(declaration_file) if file != declaration_file &&
                        self.is_global_source_file(declaration_file)
                ) && !(exclude_classes
                    && matches!(
                        suggestion,
                        Some(suggestion) if suggestion.ref_(self).flags().intersects(SymbolFlags::Class)
                    ))
                    && !(matches!(
                        node,
                        Some(node) if exclude_classes &&
                            is_property_access_expression(&node.ref_(self)) &&
                            node.ref_(self).as_property_access_expression().expression.ref_(self).kind() == SyntaxKind::ThisKeyword
                    ));
            }
        }
        false
    }

    pub(super) fn get_flow_type_of_access_expression(
        &self,
        node: Id<Node>, /*ElementAccessExpression | PropertyAccessExpression | QualifiedName*/
        prop: Option<Id<Symbol>>,
        mut prop_type: Id<Type>,
        error_node: Id<Node>,
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let assignment_kind = get_assignment_target_kind(node, self);
        if assignment_kind == AssignmentKind::Definite {
            return Ok(self.remove_missing_type(
                prop_type,
                matches!(
                    prop,
                    Some(prop) if prop.ref_(self).flags().intersects(SymbolFlags::Optional)
                ),
            ));
        }
        if matches!(
            prop,
            Some(prop) if !prop.ref_(self).flags().intersects(SymbolFlags::Variable | SymbolFlags::Property | SymbolFlags::Accessor) &&
                !(prop.ref_(self).flags().intersects(SymbolFlags::Method) && prop_type.ref_(self).flags().intersects(TypeFlags::Union)) &&
                !self.is_duplicated_common_js_export(prop.ref_(self).maybe_declarations().as_deref())
        ) {
            return Ok(prop_type);
        }
        if prop_type == self.auto_type() {
            return self.get_flow_type_of_property(node, prop);
        }
        prop_type = self.get_narrowable_type_for_reference(prop_type, node, check_mode)?;
        let mut assume_uninitialized = false;
        if self.strict_null_checks
            && self.strict_property_initialization
            && is_access_expression(&node.ref_(self))
            && node.ref_(self).as_has_expression().expression().ref_(self).kind() == SyntaxKind::ThisKeyword
        {
            let declaration = prop.and_then(|prop| prop.ref_(self).maybe_value_declaration());
            if let Some(declaration) = declaration
                .filter(|&declaration| self.is_property_without_initializer(declaration))
            {
                if !is_static(declaration, self) {
                    let flow_container = self.get_control_flow_container(node);
                    if flow_container.ref_(self).kind() == SyntaxKind::Constructor
                        && flow_container.ref_(self).parent() == declaration.ref_(self).parent()
                        && !declaration.ref_(self).flags().intersects(NodeFlags::Ambient)
                    {
                        assume_uninitialized = true;
                    }
                }
            }
        } else if self.strict_null_checks
            && matches!(
                prop.and_then(|prop| prop.ref_(self).maybe_value_declaration()),
                Some(prop_value_declaration) if is_property_access_expression(&prop_value_declaration.ref_(self)) &&
                    get_assignment_declaration_property_access_kind(prop_value_declaration, self) != AssignmentDeclarationKind::None &&
                    self.get_control_flow_container(node) == self.get_control_flow_container(prop_value_declaration)
            )
        {
            assume_uninitialized = true;
        }
        let flow_type = self.get_flow_type_of_reference(
            node,
            prop_type,
            Some(if assume_uninitialized {
                self.get_optional_type_(prop_type, None)?
            } else {
                prop_type.clone()
            }),
            Option::<Id<Node>>::None,
        )?;
        if assume_uninitialized
            && !self
                .get_falsy_flags(prop_type)
                .intersects(TypeFlags::Undefined)
            && self
                .get_falsy_flags(flow_type)
                .intersects(TypeFlags::Undefined)
        {
            self.error(
                Some(error_node),
                &Diagnostics::Property_0_is_used_before_being_assigned,
                Some(vec![self.symbol_to_string_(
                    prop.unwrap(),
                    Option::<Id<Node>>::None,
                    None,
                    None,
                    None,
                )?]),
            );
            return Ok(prop_type);
        }
        Ok(if assignment_kind != AssignmentKind::None {
            self.get_base_type_of_literal_type(flow_type)?
        } else {
            flow_type
        })
    }

    pub(super) fn check_property_not_used_before_declaration(
        &self,
        prop: Id<Symbol>,
        node: Id<Node>,  /*PropertyAccessExpression | QualifiedName*/
        right: Id<Node>, /*Identifier | PrivateIdentifier*/
    ) -> io::Result<()> {
        let value_declaration = prop.ref_(self).maybe_value_declaration();
        if value_declaration.is_none()
            || get_source_file_of_node(node, self)
                .ref_(self).as_source_file()
                .is_declaration_file()
        {
            return Ok(());
        }
        let value_declaration = value_declaration.unwrap();

        let mut diagnostic_message: Option<Gc<Diagnostic>> = None;
        let declaration_name = id_text(&right.ref_(self));
        if self.is_in_property_initializer_or_class_static_block(node)
            && !self.is_optional_property_declaration(value_declaration)
            && !(is_access_expression(&node.ref_(self))
                && is_access_expression(&node.ref_(self).as_has_expression().ref_(self).expression().ref_(self)))
            && !self.is_block_scoped_name_declared_before_use(value_declaration, right)?
            && (self.compiler_options.use_define_for_class_fields == Some(true)
                || !self.is_property_declared_in_ancestor_class(prop)?)
        {
            diagnostic_message = Some(self.error(
                Some(right),
                &Diagnostics::Property_0_is_used_before_its_initialization,
                Some(vec![declaration_name.to_owned()]),
            ));
        } else if value_declaration.ref_(self).kind() == SyntaxKind::ClassDeclaration
            && node.ref_(self).parent().ref_(self).kind() != SyntaxKind::TypeReference
            && !value_declaration.ref_(self).flags().intersects(NodeFlags::Ambient)
            && !self.is_block_scoped_name_declared_before_use(value_declaration, right)?
        {
            diagnostic_message = Some(self.error(
                Some(right),
                &Diagnostics::Class_0_used_before_its_declaration,
                Some(vec![declaration_name.to_owned()]),
            ));
        }

        if let Some(diagnostic_message) = diagnostic_message.as_ref() {
            add_related_info(
                diagnostic_message,
                vec![Gc::new(
                    create_diagnostic_for_node(
                        value_declaration,
                        &Diagnostics::_0_is_declared_here,
                        Some(vec![declaration_name.to_owned()]),
                        self,
                    )
                    .into(),
                )],
            );
        }

        Ok(())
    }

    pub(super) fn is_in_property_initializer_or_class_static_block(&self, node: Id<Node>) -> bool {
        find_ancestor(Some(node), |node: Id<Node>| match node.ref_(self).kind() {
            SyntaxKind::PropertyDeclaration => true.into(),
            SyntaxKind::PropertyAssignment
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::SpreadAssignment
            | SyntaxKind::ComputedPropertyName
            | SyntaxKind::TemplateSpan
            | SyntaxKind::JsxExpression
            | SyntaxKind::JsxAttribute
            | SyntaxKind::JsxAttributes
            | SyntaxKind::JsxSpreadAttribute
            | SyntaxKind::JsxOpeningElement
            | SyntaxKind::ExpressionWithTypeArguments
            | SyntaxKind::HeritageClause => false.into(),
            SyntaxKind::ArrowFunction | SyntaxKind::ExpressionStatement => {
                if is_block(&node.ref_(self).parent().ref_(self))
                    && is_class_static_block_declaration(&node.ref_(self).parent().ref_(self).parent().ref_(self))
                {
                    true.into()
                } else {
                    FindAncestorCallbackReturn::Quit
                }
            }
            _ => {
                if is_expression_node(node, self) {
                    false.into()
                } else {
                    FindAncestorCallbackReturn::Quit
                }
            }
        }, self)
        .is_some()
    }

    pub(super) fn is_property_declared_in_ancestor_class(
        &self,
        prop: Id<Symbol>,
    ) -> io::Result<bool> {
        if !prop
            .ref_(self)
            .maybe_parent()
            .unwrap()
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::Class)
        {
            return Ok(false);
        }
        let mut class_type: Option<Id<Type>> =
            Some(self.get_type_of_symbol(prop.ref_(self).maybe_parent().unwrap())?);
        loop {
            let class_type_present = class_type.unwrap();
            class_type = if class_type_present.ref_(self).maybe_symbol().is_some() {
                self.get_super_class(class_type_present)?
            } else {
                None
            };
            if class_type.is_none() {
                return Ok(false);
            }
            let class_type_present = class_type.unwrap();
            let super_property = self.get_property_of_type_(
                class_type_present,
                prop.ref_(self).escaped_name(),
                None,
            )?;
            if super_property
                .and_then(|super_property| super_property.ref_(self).maybe_value_declaration())
                .is_some()
            {
                return Ok(true);
            }
        }
    }

    pub(super) fn get_super_class(
        &self,
        class_type: Id<Type>, /*InterfaceType*/
    ) -> io::Result<Option<Id<Type>>> {
        let x = self.get_base_types(class_type)?;
        if x.is_empty() {
            return Ok(None);
        }
        Ok(Some(self.get_intersection_type(
            &x,
            Option::<Id<Symbol>>::None,
            None,
        )?))
    }

    pub(super) fn report_nonexistent_property(
        &self,
        prop_node: Id<Node>, /*Identifier | PrivateIdentifier*/
        containing_type: Id<Type>,
        is_unchecked_js: bool,
    ) -> io::Result<()> {
        let mut error_info: Option<DiagnosticMessageChain> = None;
        let mut related_info: Option<Gc<DiagnosticRelatedInformation>> = None;
        if !is_private_identifier(&prop_node.ref_(self))
            && containing_type
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Union)
            && !containing_type
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Primitive)
        {
            for &subtype in containing_type.ref_(self).as_union_type().types() {
                if self
                    .get_property_of_type_(subtype, &prop_node.ref_(self).as_identifier().escaped_text, None)?
                    .is_none()
                    && self
                        .get_applicable_index_info_for_name(
                            subtype,
                            &prop_node.ref_(self).as_identifier().escaped_text,
                        )?
                        .is_none()
                {
                    error_info = Some(chain_diagnostic_messages(
                        error_info,
                        &Diagnostics::Property_0_does_not_exist_on_type_1,
                        Some(vec![
                            declaration_name_to_string(Some(prop_node), self).into_owned(),
                            self.type_to_string_(subtype, Option::<Id<Node>>::None, None, None)?,
                        ]),
                    ));
                    break;
                }
            }
        }
        if self
            .type_has_static_property(&prop_node.ref_(self).as_member_name().escaped_text(), containing_type)?
        {
            let prop_name = declaration_name_to_string(Some(prop_node), self).into_owned();
            let type_name =
                self.type_to_string_(containing_type, Option::<Id<Node>>::None, None, None)?;
            error_info = Some(chain_diagnostic_messages(
                error_info,
                &Diagnostics::Property_0_does_not_exist_on_type_1_Did_you_mean_to_access_the_static_member_2_instead,
                Some(vec![
                    prop_name.clone(),
                    type_name.clone(),
                    format!("{}.{}", type_name, prop_name)
                ])
            ));
        } else {
            let promised_type =
                self.get_promised_type_of_promise(containing_type, Option::<Id<Node>>::None)?;
            if matches!(
                promised_type,
                Some(promised_type) if self.get_property_of_type_(
                    promised_type,
                    &prop_node.ref_(self).as_member_name().escaped_text(),
                    None
                )?.is_some()
            ) {
                error_info = Some(chain_diagnostic_messages(
                    error_info,
                    &Diagnostics::Property_0_does_not_exist_on_type_1,
                    Some(vec![
                        declaration_name_to_string(Some(prop_node), self).into_owned(),
                        self.type_to_string_(
                            containing_type,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                    ]),
                ));
                related_info = Some(Gc::new(
                    create_diagnostic_for_node(
                        prop_node,
                        &Diagnostics::Did_you_forget_to_use_await,
                        None,
                        self,
                    )
                    .into(),
                ));
            } else {
                let missing_property = declaration_name_to_string(Some(prop_node), self).into_owned();
                let container =
                    self.type_to_string_(containing_type, Option::<Id<Node>>::None, None, None)?;
                let lib_suggestion = self.get_suggested_lib_for_non_existent_property(
                    &missing_property,
                    containing_type,
                )?;
                if let Some(lib_suggestion) = lib_suggestion {
                    error_info = Some(chain_diagnostic_messages(
                        error_info,
                        &Diagnostics::Property_0_does_not_exist_on_type_1_Do_you_need_to_change_your_target_library_Try_changing_the_lib_compiler_option_to_2_or_later,
                        Some(vec![
                            missing_property,
                            container,
                            lib_suggestion
                        ])
                    ));
                } else {
                    let suggestion = self.get_suggested_symbol_for_nonexistent_property(
                        prop_node,
                        containing_type,
                    )?;
                    if let Some(suggestion) = suggestion {
                        let suggested_name = symbol_name(suggestion, self).into_owned();
                        let message = if is_unchecked_js {
                            &*Diagnostics::Property_0_may_not_exist_on_type_1_Did_you_mean_2
                        } else {
                            &*Diagnostics::Property_0_does_not_exist_on_type_1_Did_you_mean_2
                        };
                        error_info = Some(chain_diagnostic_messages(
                            error_info,
                            message,
                            Some(vec![missing_property, container, suggested_name.clone()]),
                        ));
                        related_info = suggestion
                            .ref_(self)
                            .maybe_value_declaration()
                            .as_ref()
                            .map(|suggestion_value_declaration| {
                                Gc::new(
                                    create_diagnostic_for_node(
                                        suggestion_value_declaration,
                                        &Diagnostics::_0_is_declared_here,
                                        Some(vec![suggested_name]),
                                        self,
                                    )
                                    .into(),
                                )
                            });
                    } else {
                        let diagnostic = if self
                            .container_seems_to_be_empty_dom_element(containing_type)?
                        {
                            &*Diagnostics::Property_0_does_not_exist_on_type_1_Try_changing_the_lib_compiler_option_to_include_dom
                        } else {
                            &*Diagnostics::Property_0_does_not_exist_on_type_1
                        };
                        error_info = Some(chain_diagnostic_messages(
                            self.elaborate_never_intersection(error_info, containing_type)?,
                            diagnostic,
                            Some(vec![missing_property, container]),
                        ));
                    }
                }
            }
        }
        let error_info = error_info.unwrap();
        let error_info_code = error_info.code;
        let result_diagnostic: Gc<Diagnostic> = Gc::new(
            create_diagnostic_for_node_from_message_chain(prop_node, error_info, None, self).into(),
        );
        if let Some(related_info) = related_info {
            add_related_info(&result_diagnostic, vec![related_info]);
        }
        self.add_error_or_suggestion(
            !is_unchecked_js
                || error_info_code
                    != Diagnostics::Property_0_may_not_exist_on_type_1_Did_you_mean_2.code,
            result_diagnostic,
        );

        Ok(())
    }
}
