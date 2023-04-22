#![allow(non_upper_case_globals)]

use gc::Gc;
use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

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
    ExternalEmitHelpers, FindAncestorCallbackReturn, Node, NodeFlags, NodeInterface, ScriptKind,
    ScriptTarget, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags,
    TypeInterface, UnionOrIntersectionTypeInterface,
};

impl TypeChecker {
    pub(super) fn get_this_parameter_from_node_context(&self, node: &Node) -> Option<Gc<Node>> {
        let this_container = get_this_container(node, false);
        /*thisContainer &&*/
        if is_function_like(Some(&*this_container)) {
            get_this_parameter(&this_container)
        } else {
            None
        }
    }

    pub(super) fn symbol_has_non_method_declaration(&self, symbol: &Symbol) -> bool {
        self.for_each_property_bool(symbol, &mut |prop: &Symbol| {
            !prop.flags().intersects(SymbolFlags::Method)
        })
    }

    pub(super) fn check_non_null_expression(
        &self,
        node: &Node, /*Expression | QualifiedName*/
    ) -> Gc<Type> {
        self.check_non_null_type(&self.check_expression(node, None, None), node)
    }

    pub(super) fn is_nullable_type(&self, type_: &Type) -> bool {
        if self.strict_null_checks {
            self.get_falsy_flags(type_)
        } else {
            type_.flags()
        }
        .intersects(TypeFlags::Nullable)
    }

    pub(super) fn get_non_nullable_type_if_needed(&self, type_: &Type) -> Gc<Type> {
        if self.is_nullable_type(type_) {
            self.get_non_nullable_type(&type_)
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn report_object_possibly_null_or_undefined_error(
        &self,
        node: &Node,
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
        node: &Node,
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

    pub(super) fn check_non_null_type_with_reporter<TReportError: FnMut(&Node, TypeFlags)>(
        &self,
        type_: &Type,
        node: &Node,
        mut report_error: TReportError,
    ) -> Gc<Type> {
        if self.strict_null_checks && type_.flags().intersects(TypeFlags::Unknown) {
            self.error(Some(node), &Diagnostics::Object_is_of_type_unknown, None);
            return self.error_type();
        }
        let kind = if self.strict_null_checks {
            self.get_falsy_flags(type_)
        } else {
            type_.flags()
        } & TypeFlags::Nullable;
        if kind != TypeFlags::None {
            report_error(node, kind);
            let t = self.get_non_nullable_type(type_);
            return if t.flags().intersects(TypeFlags::Nullable | TypeFlags::Never) {
                self.error_type()
            } else {
                t
            };
        }
        type_.type_wrapper()
    }

    pub(super) fn check_non_null_type(&self, type_: &Type, node: &Node) -> Gc<Type> {
        self.check_non_null_type_with_reporter(type_, node, |node: &Node, flags: TypeFlags| {
            self.report_object_possibly_null_or_undefined_error(node, flags)
        })
    }

    pub(super) fn check_non_null_non_void_type(&self, type_: &Type, node: &Node) -> Gc<Type> {
        let non_null_type = self.check_non_null_type(type_, node);
        if non_null_type.flags().intersects(TypeFlags::Void) {
            self.error(Some(node), &Diagnostics::Object_is_possibly_undefined, None);
        }
        non_null_type
    }

    pub(super) fn check_property_access_expression(
        &self,
        node: &Node, /*PropertyAccessExpression*/
        check_mode: Option<CheckMode>,
    ) -> Gc<Type> {
        let node_as_property_access_expression = node.as_property_access_expression();
        if node.flags().intersects(NodeFlags::OptionalChain) {
            self.check_property_access_chain(node, check_mode)
        } else {
            self.check_property_access_expression_or_qualified_name(
                node,
                &node_as_property_access_expression.expression,
                &self.check_non_null_expression(&node_as_property_access_expression.expression),
                &node_as_property_access_expression.name,
                check_mode,
            )
        }
    }

    pub(super) fn check_property_access_chain(
        &self,
        node: &Node, /*PropertyAccessChain*/
        check_mode: Option<CheckMode>,
    ) -> Gc<Type> {
        let node_as_property_access_expression = node.as_property_access_expression();
        let left_type =
            self.check_expression(&node_as_property_access_expression.expression, None, None);
        let non_optional_type = self.get_optional_expression_type(
            &left_type,
            &node_as_property_access_expression.expression,
        );
        self.propagate_optional_type_marker(
            &self.check_property_access_expression_or_qualified_name(
                node,
                &node_as_property_access_expression.expression,
                &self.check_non_null_type(
                    &non_optional_type,
                    &node_as_property_access_expression.expression,
                ),
                &node_as_property_access_expression.name,
                check_mode,
            ),
            node,
            !Gc::ptr_eq(&non_optional_type, &left_type),
        )
    }

    pub(super) fn check_qualified_name(
        &self,
        node: &Node, /*QualifiedName*/
        check_mode: Option<CheckMode>,
    ) -> Gc<Type> {
        let node_as_qualified_name = node.as_qualified_name();
        let left_type = if is_part_of_type_query(node)
            && is_this_identifier(Some(&*node_as_qualified_name.left))
        {
            self.check_non_null_type(
                &self.check_this_expression(&node_as_qualified_name.left),
                &node_as_qualified_name.left,
            )
        } else {
            self.check_non_null_expression(&node_as_qualified_name.left)
        };
        self.check_property_access_expression_or_qualified_name(
            node,
            &node_as_qualified_name.left,
            &left_type,
            &node_as_qualified_name.right,
            check_mode,
        )
    }

    pub(super) fn is_method_access_for_call(&self, node: &Node) -> bool {
        let mut node = node.node_wrapper();
        while node.parent().kind() == SyntaxKind::ParenthesizedExpression {
            node = node.parent();
        }
        is_call_or_new_expression(&node.parent())
            && Gc::ptr_eq(&node.parent().as_has_expression().expression(), &node)
    }

    pub(super) fn lookup_symbol_for_private_identifier_declaration(
        &self,
        prop_name: &str, /*__String*/
        location: &Node,
    ) -> Option<Gc<Symbol>> {
        let mut containing_class = get_containing_class(location);
        while let Some(containing_class_present) = containing_class.as_ref() {
            let symbol = containing_class_present.symbol();
            let name = get_symbol_name_for_private_identifier(&symbol, prop_name);
            let prop = symbol
                .maybe_members()
                .as_ref()
                .and_then(|symbol_members| (**symbol_members).borrow().get(&name).cloned())
                .or_else(|| {
                    symbol
                        .maybe_exports()
                        .as_ref()
                        .and_then(|symbol_exports| (**symbol_exports).borrow().get(&name).cloned())
                });
            if prop.is_some() {
                return prop;
            }
            containing_class = get_containing_class(containing_class_present);
        }
        None
    }

    pub(super) fn check_grammar_private_identifier_expression(
        &self,
        priv_id: &Node, /*PrivateIdentifier*/
    ) -> bool {
        if get_containing_class(priv_id).is_none() {
            return self.grammar_error_on_node(
                priv_id,
                &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                None,
            );
        }
        if !is_expression_node(priv_id) {
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
                Some(vec![id_text(priv_id).to_owned()]),
            );
        }
        false
    }

    pub(super) fn check_private_identifier_expression(
        &self,
        priv_id: &Node, /*PrivateIdentifier*/
    ) -> Gc<Type> {
        self.check_grammar_private_identifier_expression(priv_id);
        let symbol = self.get_symbol_for_private_identifier_expression(priv_id);
        if let Some(symbol) = symbol.as_ref() {
            self.mark_property_as_referenced(symbol, Option::<&Node>::None, false);
        }
        self.any_type()
    }

    pub(super) fn get_symbol_for_private_identifier_expression(
        &self,
        priv_id: &Node, /*PrivateIdentifier*/
    ) -> Option<Gc<Symbol>> {
        if !is_expression_node(priv_id) {
            return None;
        }

        let links = self.get_node_links(priv_id);
        if (*links).borrow().resolved_symbol.is_none() {
            links.borrow_mut().resolved_symbol = self
                .lookup_symbol_for_private_identifier_declaration(
                    &priv_id.as_private_identifier().escaped_text,
                    priv_id,
                );
        }
        let ret = (*links).borrow().resolved_symbol.clone();
        ret
    }

    pub(super) fn get_private_identifier_property_of_type_(
        &self,
        left_type: &Type,
        lexically_scoped_identifier: &Symbol,
    ) -> Option<Gc<Symbol>> {
        self.get_property_of_type_(left_type, lexically_scoped_identifier.escaped_name(), None)
    }

    pub(super) fn check_private_identifier_property_access<
        TLexicallyScopedIdentifier: Borrow<Symbol>,
    >(
        &self,
        left_type: &Type,
        right: &Node, /*PrivateIdentifier*/
        lexically_scoped_identifier: Option<TLexicallyScopedIdentifier>,
    ) -> bool {
        let mut property_on_type: Option<Gc<Symbol>> = None;
        let properties = self.get_properties_of_type(left_type);
        // if (properties) {
        let right_as_private_identifier = right.as_private_identifier();
        for_each(properties, |ref symbol: Gc<Symbol>, _| {
            let decl = symbol.maybe_value_declaration();
            if matches!(
                decl.as_ref(),
                Some(decl) if is_named_declaration(decl) &&
                    is_private_identifier(&decl.as_named_declaration().name()) &&
                    decl.as_named_declaration().name().as_identifier().escaped_text == right_as_private_identifier.escaped_text
            ) {
                property_on_type = Some(symbol.clone());
                return Some(());
            }
            None
        });
        // }
        let diag_name = self
            .diagnostic_name(right.node_wrapper().into())
            .into_owned();
        if let Some(property_on_type) = property_on_type.as_ref() {
            let type_value_decl =
                Debug_.check_defined(property_on_type.maybe_value_declaration(), None);
            let type_class = Debug_.check_defined(get_containing_class(&type_value_decl), None);
            if let Some(lexically_scoped_identifier_value_declaration) = lexically_scoped_identifier
                .and_then(|lexically_scoped_identifier| {
                    lexically_scoped_identifier
                        .borrow()
                        .maybe_value_declaration()
                })
            {
                let lexical_value_decl = lexically_scoped_identifier_value_declaration;
                let lexical_class = get_containing_class(&lexical_value_decl);
                Debug_.assert(lexical_class.is_some(), None);
                let lexical_class = lexical_class.unwrap();
                if find_ancestor(Some(&*lexical_class), |n: &Node| ptr::eq(&*type_class, n))
                    .is_some()
                {
                    let diagnostic = self.error(
                        Some(right),
                        &Diagnostics::The_property_0_cannot_be_accessed_on_type_1_within_this_class_because_it_is_shadowed_by_another_private_identifier_with_the_same_spelling,
                        Some(vec![
                            diag_name.clone(),
                            self.type_to_string_(
                                left_type,
                                Option::<&Node>::None,
                                None, None,
                            )
                        ])
                    );

                    add_related_info(
                        &diagnostic,
                        vec![
                            Gc::new(
                                create_diagnostic_for_node(
                                    &lexical_value_decl,
                                    &Diagnostics::The_shadowing_declaration_of_0_is_defined_here,
                                    Some(vec![
                                        diag_name.clone(),
                                    ])
                                ).into()
                            ),
                            Gc::new(
                                create_diagnostic_for_node(
                                    &type_value_decl,
                                    &Diagnostics::The_declaration_of_0_that_you_probably_intended_to_use_is_defined_here,
                                    Some(vec![
                                        diag_name.clone(),
                                    ])
                                ).into()
                            ),
                        ]
                    );
                    return true;
                }
            }
            self.error(
                Some(right),
                &Diagnostics::Property_0_is_not_accessible_outside_class_1_because_it_has_a_private_identifier,
                Some(vec![
                    diag_name,
                    self.diagnostic_name(
                        type_class.as_named_declaration().maybe_name().map_or_else(|| {
                            anon.into()
                        }, Into::into)
                    ).into_owned()
                ])
            );
            return true;
        }
        false
    }

    pub(super) fn is_this_property_access_in_constructor(
        &self,
        node: &Node, /*ElementAccessExpression | PropertyAccessExpression | QualifiedName*/
        prop: &Symbol,
    ) -> bool {
        (self.is_constructor_declared_property(prop)
            || is_this_property(node) && self.is_auto_typed_property(prop))
            && matches!(
                self.get_declaring_constructor(prop).as_ref(),
                Some(declaring_constructor) if Gc::ptr_eq(
                    &get_this_container(node, true),
                    declaring_constructor
                )
            )
    }

    pub(super) fn check_property_access_expression_or_qualified_name(
        &self,
        node: &Node, /*PropertyAccessExpression | QualifiedName*/
        left: &Node, /*Expression | QualifiedName*/
        left_type: &Type,
        right: &Node, /*Identifier | PrivateIdentifier*/
        check_mode: Option<CheckMode>,
    ) -> Gc<Type> {
        let parent_symbol = (*self.get_node_links(left))
            .borrow()
            .resolved_symbol
            .clone();
        let assignment_kind = get_assignment_target_kind(node);
        let apparent_type = self.get_apparent_type(&*if assignment_kind != AssignmentKind::None
            || self.is_method_access_for_call(node)
        {
            self.get_widened_type(left_type)
        } else {
            left_type.type_wrapper()
        });
        let is_any_like = self.is_type_any(Some(&*apparent_type))
            || Gc::ptr_eq(&apparent_type, &self.silent_never_type());
        let mut prop: Option<Gc<Symbol>> = None;
        if is_private_identifier(right) {
            if self.language_version < ScriptTarget::ESNext {
                if assignment_kind != AssignmentKind::None {
                    self.check_external_emit_helpers(
                        node,
                        ExternalEmitHelpers::ClassPrivateFieldSet,
                    );
                }
                if assignment_kind != AssignmentKind::Definite {
                    self.check_external_emit_helpers(
                        node,
                        ExternalEmitHelpers::ClassPrivateFieldGet,
                    );
                }
            }

            let lexically_scoped_symbol = self.lookup_symbol_for_private_identifier_declaration(
                &right.as_private_identifier().escaped_text,
                right,
            );
            if assignment_kind != AssignmentKind::None
                && matches!(
                    lexically_scoped_symbol.as_ref().and_then(|lexically_scoped_symbol| lexically_scoped_symbol.maybe_value_declaration()).as_ref(),
                    Some(lexically_scoped_symbol_value_declaration) if is_method_declaration(lexically_scoped_symbol_value_declaration)
                )
            {
                self.grammar_error_on_node(
                    right,
                    &Diagnostics::Cannot_assign_to_private_method_0_Private_methods_are_not_writable,
                    Some(vec![
                        id_text(right).to_owned()
                    ])
                );
            }

            if is_any_like {
                if lexically_scoped_symbol.is_some() {
                    return if self.is_error_type(&apparent_type) {
                        self.error_type()
                    } else {
                        apparent_type
                    };
                }
                if get_containing_class(right).is_none() {
                    self.grammar_error_on_node(
                        right,
                        &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                        None,
                    );
                    return self.any_type();
                }
            }
            prop = lexically_scoped_symbol
                .as_ref()
                .and_then(|lexically_scoped_symbol| {
                    self.get_private_identifier_property_of_type_(
                        left_type,
                        lexically_scoped_symbol,
                    )
                });
            if prop.is_none()
                && self.check_private_identifier_property_access(
                    left_type,
                    right,
                    lexically_scoped_symbol.as_deref(),
                )
            {
                return self.error_type();
            } else {
                let is_setonly_accessor = matches!(
                    prop.as_ref(),
                    Some(prop) if prop.flags().intersects(SymbolFlags::SetAccessor) &&
                        !prop.flags().intersects(SymbolFlags::GetAccessor)
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
                if is_identifier(left) {
                    if let Some(parent_symbol) = parent_symbol.as_ref() {
                        self.mark_alias_referenced(parent_symbol, node);
                    }
                }
                return if self.is_error_type(&apparent_type) {
                    self.error_type()
                } else {
                    apparent_type
                };
            }
            prop = self.get_property_of_type_(
                &apparent_type,
                &right.as_identifier().escaped_text,
                None,
            );
        }
        if is_identifier(left) {
            if let Some(parent_symbol) = parent_symbol.as_ref() {
                if self.compiler_options.isolated_modules == Some(true)
                    || !matches!(
                        prop.as_ref(),
                        Some(prop) if self.is_const_enum_or_const_enum_only_module(prop)
                    )
                    || should_preserve_const_enums(&self.compiler_options)
                        && self.is_export_or_export_expression(node)
                {
                    self.mark_alias_referenced(parent_symbol, node);
                }
            }
        }

        let prop_type: Gc<Type>;
        match prop.as_ref() {
            None => {
                let index_info = if !is_private_identifier(right)
                    && (assignment_kind == AssignmentKind::None
                        || !self.is_generic_object_type(left_type)
                        || self.is_this_type_parameter(left_type))
                {
                    self.get_applicable_index_info_for_name(
                        &apparent_type,
                        &right.as_identifier().escaped_text,
                    )
                } else {
                    None
                };
                if !index_info.is_some()
                /*&& indexInfo.type*/
                {
                    let is_unchecked_js =
                        self.is_unchecked_js_suggestion(Some(node), left_type.maybe_symbol(), true);
                    if !is_unchecked_js && self.is_js_literal_type(left_type) {
                        return self.any_type();
                    }
                    if matches!(
                        left_type.maybe_symbol().as_ref(),
                        Some(left_type_symbol) if Gc::ptr_eq(
                            left_type_symbol,
                            &self.global_this_symbol()
                        )
                    ) {
                        let global_this_symbol_exports =
                            self.global_this_symbol().maybe_exports().clone().unwrap();
                        if (*global_this_symbol_exports)
                            .borrow()
                            .contains_key(right.as_member_name().escaped_text())
                            && (*global_this_symbol_exports)
                                .borrow()
                                .get(right.as_member_name().escaped_text())
                                .unwrap()
                                .flags()
                                .intersects(SymbolFlags::BlockScoped)
                        {
                            self.error(
                                Some(right),
                                &Diagnostics::Property_0_does_not_exist_on_type_1,
                                Some(vec![
                                    unescape_leading_underscores(
                                        &right.as_member_name().escaped_text(),
                                    )
                                    .to_owned(),
                                    self.type_to_string_(
                                        left_type,
                                        Option::<&Node>::None,
                                        None,
                                        None,
                                    ),
                                ]),
                            );
                        } else if self.no_implicit_any {
                            self.error(
                                Some(right),
                                &Diagnostics::Element_implicitly_has_an_any_type_because_type_0_has_no_index_signature,
                                Some(vec![
                                    self.type_to_string_(
                                        left_type,
                                        Option::<&Node>::None,
                                        None, None,
                                    )
                                ])
                            );
                        }
                        return self.any_type();
                    }
                    if !right.as_member_name().escaped_text().is_empty()
                        && !self.check_and_report_error_for_extending_interface(node)
                    {
                        self.report_nonexistent_property(
                            right,
                            if self.is_this_type_parameter(left_type) {
                                &*apparent_type
                            } else {
                                left_type
                            },
                            is_unchecked_js,
                        );
                    }
                    return self.error_type();
                }
                let index_info = index_info.unwrap();
                if index_info.is_readonly && (is_assignment_target(node) || is_delete_target(node))
                {
                    self.error(
                        Some(node),
                        &Diagnostics::Index_signature_in_type_0_only_permits_reading,
                        Some(vec![self.type_to_string_(
                            &apparent_type,
                            Option::<&Node>::None,
                            None,
                            None,
                        )]),
                    );
                }

                prop_type = if self.compiler_options.no_unchecked_indexed_access == Some(true)
                    && !is_assignment_target(node)
                {
                    self.get_union_type(
                        &[index_info.type_.clone(), self.undefined_type()],
                        None,
                        Option::<&Symbol>::None,
                        None,
                        Option::<&Type>::None,
                    )
                } else {
                    index_info.type_.clone()
                };
                if self
                    .compiler_options
                    .no_property_access_from_index_signature
                    == Some(true)
                    && is_property_access_expression(node)
                {
                    self.error(
                        Some(right),
                        &Diagnostics::Property_0_comes_from_an_index_signature_so_it_must_be_accessed_with_0,
                        Some(vec![
                            unescape_leading_underscores(&right.as_member_name().escaped_text()).to_owned()
                        ])
                    );
                }
            }
            Some(prop) => {
                if let Some(prop_declarations) = prop.maybe_declarations().as_ref() {
                    if self
                        .get_declaration_node_flags_from_symbol(prop)
                        .intersects(NodeFlags::Deprecated)
                        && self.is_uncalled_function_reference(node, prop)
                    {
                        self.add_deprecated_suggestion(
                            right,
                            prop_declarations,
                            &*right.as_member_name().escaped_text(),
                        );
                    }
                }
                self.check_property_not_used_before_declaration(prop, node, right);
                self.mark_property_as_referenced(
                    prop,
                    Some(node),
                    self.is_self_type_access(left, parent_symbol.as_deref()),
                );
                self.get_node_links(node).borrow_mut().resolved_symbol = Some(prop.clone());
                let writing = is_write_access(node);
                self.check_property_accessibility(
                    node,
                    left.kind() == SyntaxKind::SuperKeyword,
                    writing,
                    &apparent_type,
                    prop,
                    None,
                );
                if self.is_assignment_to_readonly_entity(node, prop, assignment_kind) {
                    self.error(
                        Some(right),
                        &Diagnostics::Cannot_assign_to_0_because_it_is_a_read_only_property,
                        Some(vec![id_text(right).to_owned()]),
                    );
                    return self.error_type();
                }

                prop_type = if self.is_this_property_access_in_constructor(node, prop) {
                    self.auto_type()
                } else if writing {
                    self.get_set_accessor_type_of_symbol(prop)
                } else {
                    self.get_type_of_symbol(prop)
                };
            }
        }

        self.get_flow_type_of_access_expression(node, prop, &prop_type, right, check_mode)
    }

    pub(super) fn is_unchecked_js_suggestion<TNode: Borrow<Node>, TSuggestion: Borrow<Symbol>>(
        &self,
        node: Option<TNode>,
        suggestion: Option<TSuggestion>,
        exclude_classes: bool,
    ) -> bool {
        let node = node.map(|node| node.borrow().node_wrapper());
        let suggestion = suggestion.map(|suggestion| suggestion.borrow().symbol_wrapper());
        let file = maybe_get_source_file_of_node(node.as_deref());
        if let Some(file) = file.as_ref() {
            let file_as_source_file = file.as_source_file();
            if self.compiler_options.check_js.is_none()
                && file_as_source_file.maybe_check_js_directive().is_none()
                && matches!(
                    file_as_source_file.script_kind(),
                    ScriptKind::JS | ScriptKind::JSX
                )
            {
                let declaration_file = maybe_for_each(
                    suggestion
                        .as_ref()
                        .and_then(|suggestion| suggestion.maybe_declarations().clone())
                        .as_ref(),
                    |declaration: &Gc<Node>, _| maybe_get_source_file_of_node(Some(&**declaration)),
                );
                return !matches!(
                    declaration_file.as_ref(),
                    Some(declaration_file) if !Gc::ptr_eq(
                        file,
                        declaration_file
                    ) && self.is_global_source_file(declaration_file)
                ) && !(exclude_classes
                    && matches!(
                        suggestion.as_ref(),
                        Some(suggestion) if suggestion.flags().intersects(SymbolFlags::Class)
                    ))
                    && !(matches!(
                        node.as_ref(),
                        Some(node) if exclude_classes &&
                            is_property_access_expression(node) &&
                            node.as_property_access_expression().expression.kind() == SyntaxKind::ThisKeyword
                    ));
            }
        }
        false
    }

    pub(super) fn get_flow_type_of_access_expression<TProp: Borrow<Symbol>>(
        &self,
        node: &Node, /*ElementAccessExpression | PropertyAccessExpression | QualifiedName*/
        prop: Option<TProp>,
        prop_type: &Type,
        error_node: &Node,
        check_mode: Option<CheckMode>,
    ) -> Gc<Type> {
        let assignment_kind = get_assignment_target_kind(node);
        let prop = prop.map(|prop| prop.borrow().symbol_wrapper());
        if assignment_kind == AssignmentKind::Definite {
            return self.remove_missing_type(
                prop_type,
                matches!(
                    prop.as_ref(),
                    Some(prop) if prop.flags().intersects(SymbolFlags::Optional)
                ),
            );
        }
        if matches!(
            prop.as_ref(),
            Some(prop) if !prop.flags().intersects(SymbolFlags::Variable | SymbolFlags::Property | SymbolFlags::Accessor) &&
                !(prop.flags().intersects(SymbolFlags::Method) && prop_type.flags().intersects(TypeFlags::Union)) &&
                !self.is_duplicated_common_js_export(prop.maybe_declarations().as_deref())
        ) {
            return prop_type.type_wrapper();
        }
        if ptr::eq(prop_type, &*self.auto_type()) {
            return self.get_flow_type_of_property(node, prop);
        }
        let mut prop_type = prop_type.type_wrapper();
        prop_type = self.get_narrowable_type_for_reference(&prop_type, node, check_mode);
        let mut assume_uninitialized = false;
        if self.strict_null_checks
            && self.strict_property_initialization
            && is_access_expression(node)
            && node.as_has_expression().expression().kind() == SyntaxKind::ThisKeyword
        {
            let declaration = prop
                .as_ref()
                .and_then(|prop| prop.maybe_value_declaration());
            if let Some(declaration) = declaration
                .as_ref()
                .filter(|declaration| self.is_property_without_initializer(declaration))
            {
                if !is_static(declaration) {
                    let flow_container = self.get_control_flow_container(node);
                    if flow_container.kind() == SyntaxKind::Constructor
                        && Gc::ptr_eq(&flow_container.parent(), &declaration.parent())
                        && !declaration.flags().intersects(NodeFlags::Ambient)
                    {
                        assume_uninitialized = true;
                    }
                }
            }
        } else if self.strict_null_checks
            && matches!(
                prop.as_ref().and_then(|prop| prop.maybe_value_declaration()).as_ref(),
                Some(prop_value_declaration) if is_property_access_expression(prop_value_declaration) &&
                    get_assignment_declaration_property_access_kind(prop_value_declaration) != AssignmentDeclarationKind::None &&
                    Gc::ptr_eq(
                        &self.get_control_flow_container(node),
                        &self.get_control_flow_container(prop_value_declaration),
                    )
            )
        {
            assume_uninitialized = true;
        }
        let flow_type = self.get_flow_type_of_reference(
            node,
            &prop_type,
            Some(if assume_uninitialized {
                self.get_optional_type_(&prop_type, None)
            } else {
                prop_type.clone()
            }),
            Option::<&Node>::None,
        );
        if assume_uninitialized
            && !self
                .get_falsy_flags(&prop_type)
                .intersects(TypeFlags::Undefined)
            && self
                .get_falsy_flags(&flow_type)
                .intersects(TypeFlags::Undefined)
        {
            self.error(
                Some(error_node),
                &Diagnostics::Property_0_is_used_before_being_assigned,
                Some(vec![self.symbol_to_string_(
                    prop.as_ref().unwrap(),
                    Option::<&Node>::None,
                    None,
                    None,
                    None,
                )]),
            );
            return prop_type;
        }
        if assignment_kind != AssignmentKind::None {
            self.get_base_type_of_literal_type(&flow_type)
        } else {
            flow_type
        }
    }

    pub(super) fn check_property_not_used_before_declaration(
        &self,
        prop: &Symbol,
        node: &Node,  /*PropertyAccessExpression | QualifiedName*/
        right: &Node, /*Identifier | PrivateIdentifier*/
    ) {
        let value_declaration = prop.maybe_value_declaration();
        if value_declaration.is_none()
            || get_source_file_of_node(node)
                .as_source_file()
                .is_declaration_file()
        {
            return;
        }
        let value_declaration = value_declaration.unwrap();

        let mut diagnostic_message: Option<Gc<Diagnostic>> = None;
        let declaration_name = id_text(right);
        if self.is_in_property_initializer_or_class_static_block(node)
            && !self.is_optional_property_declaration(&value_declaration)
            && !(is_access_expression(node)
                && is_access_expression(&node.as_has_expression().expression()))
            && !self.is_block_scoped_name_declared_before_use(&value_declaration, right)
            && (self.compiler_options.use_define_for_class_fields == Some(true)
                || !self.is_property_declared_in_ancestor_class(prop))
        {
            diagnostic_message = Some(self.error(
                Some(right),
                &Diagnostics::Property_0_is_used_before_its_initialization,
                Some(vec![declaration_name.to_owned()]),
            ));
        } else if value_declaration.kind() == SyntaxKind::ClassDeclaration
            && node.parent().kind() != SyntaxKind::TypeReference
            && !value_declaration.flags().intersects(NodeFlags::Ambient)
            && !self.is_block_scoped_name_declared_before_use(&value_declaration, right)
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
                        &value_declaration,
                        &Diagnostics::_0_is_declared_here,
                        Some(vec![declaration_name.to_owned()]),
                    )
                    .into(),
                )],
            );
        }
    }

    pub(super) fn is_in_property_initializer_or_class_static_block(&self, node: &Node) -> bool {
        find_ancestor(Some(node), |node: &Node| match node.kind() {
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
                if is_block(&node.parent())
                    && is_class_static_block_declaration(&node.parent().parent())
                {
                    true.into()
                } else {
                    FindAncestorCallbackReturn::Quit
                }
            }
            _ => {
                if is_expression_node(node) {
                    false.into()
                } else {
                    FindAncestorCallbackReturn::Quit
                }
            }
        })
        .is_some()
    }

    pub(super) fn is_property_declared_in_ancestor_class(&self, prop: &Symbol) -> bool {
        if !prop
            .maybe_parent()
            .unwrap()
            .flags()
            .intersects(SymbolFlags::Class)
        {
            return false;
        }
        let mut class_type: Option<Gc<Type>> =
            Some(self.get_type_of_symbol(&prop.maybe_parent().unwrap()));
        loop {
            let class_type_present = class_type.as_ref().unwrap();
            class_type = if class_type_present.maybe_symbol().is_some() {
                self.get_super_class(class_type_present)
            } else {
                None
            };
            if class_type.is_none() {
                return false;
            }
            let class_type_present = class_type.as_ref().unwrap();
            let super_property =
                self.get_property_of_type_(class_type_present, prop.escaped_name(), None);
            if super_property
                .as_ref()
                .and_then(|super_property| super_property.maybe_value_declaration())
                .is_some()
            {
                return true;
            }
        }
    }

    pub(super) fn get_super_class(
        &self,
        class_type: &Type, /*InterfaceType*/
    ) -> Option<Gc<Type>> {
        let x = self.get_base_types(class_type);
        if x.is_empty() {
            return None;
        }
        Some(self.get_intersection_type(&x, Option::<&Symbol>::None, None))
    }

    pub(super) fn report_nonexistent_property(
        &self,
        prop_node: &Node, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
        is_unchecked_js: bool,
    ) {
        let mut error_info: Option<DiagnosticMessageChain> = None;
        let mut related_info: Option<Gc<DiagnosticRelatedInformation>> = None;
        if !is_private_identifier(prop_node)
            && containing_type.flags().intersects(TypeFlags::Union)
            && !containing_type.flags().intersects(TypeFlags::Primitive)
        {
            for subtype in containing_type.as_union_type().types() {
                if self
                    .get_property_of_type_(subtype, &prop_node.as_identifier().escaped_text, None)
                    .is_none()
                    && self
                        .get_applicable_index_info_for_name(
                            subtype,
                            &prop_node.as_identifier().escaped_text,
                        )
                        .is_none()
                {
                    error_info = Some(chain_diagnostic_messages(
                        error_info,
                        &Diagnostics::Property_0_does_not_exist_on_type_1,
                        Some(vec![
                            declaration_name_to_string(Some(prop_node)).into_owned(),
                            self.type_to_string_(subtype, Option::<&Node>::None, None, None),
                        ]),
                    ));
                    break;
                }
            }
        }
        if self
            .type_has_static_property(&prop_node.as_member_name().escaped_text(), containing_type)
        {
            let prop_name = declaration_name_to_string(Some(prop_node)).into_owned();
            let type_name =
                self.type_to_string_(containing_type, Option::<&Node>::None, None, None);
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
                self.get_promised_type_of_promise(containing_type, Option::<&Node>::None);
            if matches!(
                promised_type.as_ref(),
                Some(promised_type) if self.get_property_of_type_(promised_type, &prop_node.as_member_name().escaped_text(), None).is_some()
            ) {
                error_info = Some(chain_diagnostic_messages(
                    error_info,
                    &Diagnostics::Property_0_does_not_exist_on_type_1,
                    Some(vec![
                        declaration_name_to_string(Some(prop_node)).into_owned(),
                        self.type_to_string_(containing_type, Option::<&Node>::None, None, None),
                    ]),
                ));
                related_info = Some(Gc::new(
                    create_diagnostic_for_node(
                        prop_node,
                        &Diagnostics::Did_you_forget_to_use_await,
                        None,
                    )
                    .into(),
                ));
            } else {
                let missing_property = declaration_name_to_string(Some(prop_node)).into_owned();
                let container =
                    self.type_to_string_(containing_type, Option::<&Node>::None, None, None);
                let lib_suggestion = self.get_suggested_lib_for_non_existent_property(
                    &missing_property,
                    containing_type,
                );
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
                        prop_node.node_wrapper(),
                        containing_type,
                    );
                    if let Some(suggestion) = suggestion.as_ref() {
                        let suggested_name = symbol_name(suggestion).into_owned();
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
                        related_info = suggestion.maybe_value_declaration().as_ref().map(
                            |suggestion_value_declaration| {
                                Gc::new(
                                    create_diagnostic_for_node(
                                        suggestion_value_declaration,
                                        &Diagnostics::_0_is_declared_here,
                                        Some(vec![suggested_name]),
                                    )
                                    .into(),
                                )
                            },
                        );
                    } else {
                        let diagnostic = if self
                            .container_seems_to_be_empty_dom_element(containing_type)
                        {
                            &*Diagnostics::Property_0_does_not_exist_on_type_1_Try_changing_the_lib_compiler_option_to_include_dom
                        } else {
                            &*Diagnostics::Property_0_does_not_exist_on_type_1
                        };
                        error_info = Some(chain_diagnostic_messages(
                            self.elaborate_never_intersection(error_info, containing_type),
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
            create_diagnostic_for_node_from_message_chain(prop_node, error_info, None).into(),
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
    }
}
