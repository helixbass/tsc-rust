use std::{borrow::Borrow, collections::HashMap, convert::TryInto};

use gc::{Gc, GcCell};
use id_arena::Id;

use super::{
    get_module_instance_state, init_flow_node, is_exports_or_module_exports_or_alias, BinderType,
    ContainerFlags, ModuleInstanceState,
};
use crate::{
    append, get_source_text_of_node_from_source_file, has_jsdoc_nodes, is_in_js_file,
    is_prologue_directive, set_parent_recursive, BaseTextRange,
    DiagnosticRelatedInformationInterface, NodeArray, TextRange, TokenFlags, __String,
    create_file_diagnostic, create_symbol_table, declaration_name_to_string, find_ancestor,
    get_assignment_declaration_property_access_kind, get_containing_class, get_emit_script_target,
    get_enclosing_block_scope_container, get_error_span_for_node, get_name_of_declaration,
    get_span_of_token_at_position, get_token_pos_of_node, id_text, is_assignment_operator,
    is_assignment_target, is_declaration_statement, is_external_module,
    is_external_or_common_js_module, is_function_like_or_class_static_block_declaration,
    is_identifier, is_identifier_name, is_in_top_level_context, is_jsdoc_enum_tag,
    is_left_hand_side_expression, is_property_access_entity_name_expression,
    is_property_access_expression, is_variable_statement, set_parent, AssignmentDeclarationKind,
    Debug_, DiagnosticCategory, DiagnosticMessage, Diagnostics, FlowFlags, FlowStart, HasArena,
    InArena, InternalSymbolName, NamedDeclarationInterface, Node, NodeFlags, NodeInterface,
    ReadonlyTextRange, ScriptTarget, Symbol, SymbolFlags, SymbolInterface, SyntaxKind,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum ElementKind {
    Property = 1,
    Accessor = 2,
}

impl BinderType {
    pub(super) fn declare_module_symbol(
        &self,
        node: Id<Node>, /*ModuleDeclaration*/
    ) -> ModuleInstanceState {
        let state = get_module_instance_state(node, None, self);
        let instantiated = state != ModuleInstanceState::NonInstantiated;
        self.declare_symbol_and_add_to_symbol_table(
            node,
            if instantiated {
                SymbolFlags::ValueModule
            } else {
                SymbolFlags::NamespaceModule
            },
            if instantiated {
                SymbolFlags::ValueModuleExcludes
            } else {
                SymbolFlags::NamespaceModuleExcludes
            },
        );
        state
    }

    pub(super) fn bind_function_or_constructor_type(
        &self,
        node: Id<Node>, /*SignatureDeclaration | JSDocSignature*/
    ) {
        let symbol = self.alloc_symbol(self.create_symbol(
            SymbolFlags::Signature,
            self.get_declaration_name(node).unwrap().into_owned(),
        ));
        self.add_declaration_to_symbol(symbol, node, SymbolFlags::Signature);

        let type_literal_symbol = self.alloc_symbol(self.create_symbol(
            SymbolFlags::TypeLiteral,
            InternalSymbolName::Type.to_owned(),
        ));
        self.add_declaration_to_symbol(type_literal_symbol, node, SymbolFlags::TypeLiteral);
        let type_literal_symbol_ref = type_literal_symbol.ref_(self);
        let mut type_literal_symbol_members = type_literal_symbol_ref.maybe_members_mut();
        *type_literal_symbol_members = Some(Gc::new(GcCell::new(create_symbol_table(
            self.arena(),
            Option::<&[Id<Symbol>]>::None,
        ))));
        type_literal_symbol_members
            .as_ref()
            .unwrap()
            .borrow_mut()
            .insert(symbol.ref_(self).escaped_name().to_owned(), symbol);
    }

    pub(super) fn bind_object_literal_expression(
        &self,
        node: Id<Node>, /*ObjectLiteralExpression*/
    ) {
        if matches!(self.maybe_in_strict_mode(), Some(true)) && !is_assignment_target(node, self) {
            let mut seen = HashMap::<__String, ElementKind>::new();

            let node_ref = node.ref_(self);
            let node_as_object_literal_expression = node_ref.as_object_literal_expression();
            for prop in &*node_as_object_literal_expression.properties {
                if prop.ref_(self).kind() == SyntaxKind::SpreadAssignment
                    || prop.ref_(self).as_named_declaration().name().ref_(self).kind() != SyntaxKind::Identifier
                {
                    continue;
                }

                let identifier = prop.ref_(self).as_named_declaration().name();

                let current_kind = if matches!(
                    prop.ref_(self).kind(),
                    SyntaxKind::PropertyAssignment
                        | SyntaxKind::ShorthandPropertyAssignment
                        | SyntaxKind::MethodDeclaration
                ) {
                    ElementKind::Property
                } else {
                    ElementKind::Accessor
                };

                let identifier_ref = identifier.ref_(self);
                let identifier_as_identifier = identifier_ref.as_identifier();
                let existing_kind = seen.get(&identifier_as_identifier.escaped_text);
                if existing_kind.is_none() {
                    seen.insert(identifier_as_identifier.escaped_text.clone(), current_kind);
                    continue;
                }
                let existing_kind = *existing_kind.unwrap();

                if current_kind == ElementKind::Property && existing_kind == ElementKind::Property {
                    let file = self.file();
                    let span = get_error_span_for_node(file, identifier, self);
                    file.ref_(self).as_source_file().bind_diagnostics_mut().push(
                        Gc::new(
                            create_file_diagnostic(
                                file,
                                span.start,
                                span.length,
                                &Diagnostics::An_object_literal_cannot_have_multiple_properties_with_the_same_name_in_strict_mode,
                                None,
                            ).into()
                        )
                    );
                }
            }
        }

        self.bind_anonymous_declaration(
            node,
            SymbolFlags::ObjectLiteral,
            InternalSymbolName::Object.to_owned(),
        );
    }

    pub(super) fn bind_jsx_attributes(&self, node: Id<Node> /*JsxAttributes*/) -> Id<Symbol> {
        self.bind_anonymous_declaration(
            node,
            SymbolFlags::ObjectLiteral,
            InternalSymbolName::JSXAttributes.to_owned(),
        )
    }

    pub(super) fn bind_jsx_attribute(
        &self,
        node: Id<Node>, /*JsxAttribute*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Option<Id<Symbol>> {
        self.declare_symbol_and_add_to_symbol_table(node, symbol_flags, symbol_excludes)
    }

    pub(super) fn bind_anonymous_declaration(
        &self,
        node: Id<Node>,
        symbol_flags: SymbolFlags,
        name: __String,
    ) -> Id<Symbol> {
        let symbol = self.alloc_symbol(self.create_symbol(symbol_flags, name));
        if symbol_flags.intersects(SymbolFlags::EnumMember | SymbolFlags::ClassMember) {
            symbol
                .ref_(self)
                .set_parent(self.container().ref_(self).maybe_symbol());
        }
        self.add_declaration_to_symbol(symbol, node, symbol_flags);
        symbol
    }

    pub(super) fn bind_block_scoped_declaration(
        &self,
        node: Id<Node>, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) {
        let block_scope_container = self.block_scope_container();
        match block_scope_container.ref_(self).kind() {
            SyntaxKind::ModuleDeclaration => {
                self.declare_module_member(node, symbol_flags, symbol_excludes);
            }
            SyntaxKind::SourceFile => {
                if is_external_or_common_js_module(&self.container().ref_(self)) {
                    self.declare_module_member(node, symbol_flags, symbol_excludes);
                } else {
                    {
                        let block_scope_container_ref = block_scope_container.ref_(self);
                        let mut block_scope_container_locals =
                            block_scope_container_ref.maybe_locals_mut();
                        if block_scope_container_locals.is_none() {
                            *block_scope_container_locals = Some(Gc::new(GcCell::new(
                                create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None),
                            )));
                            self.add_to_container_chain(block_scope_container);
                        }
                    }
                    self.declare_symbol(
                        &mut *block_scope_container.ref_(self).locals().borrow_mut(),
                        None,
                        node,
                        symbol_flags,
                        symbol_excludes,
                        None,
                        None,
                    );
                }
            }
            _ => {
                {
                    let block_scope_container_ref = block_scope_container.ref_(self);
                    let mut block_scope_container_locals = block_scope_container_ref.maybe_locals_mut();
                    if block_scope_container_locals.is_none() {
                        *block_scope_container_locals = Some(Gc::new(GcCell::new(
                            create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None),
                        )));
                        self.add_to_container_chain(block_scope_container);
                    }
                }
                self.declare_symbol(
                    &mut block_scope_container.ref_(self).locals().borrow_mut(),
                    None,
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                );
            }
        }
    }

    pub(super) fn delayed_bind_jsdoc_typedef_tag(&self) {
        let delayed_type_aliases = self.maybe_delayed_type_aliases();
        if delayed_type_aliases.is_none() {
            return;
        }
        let delayed_type_aliases = delayed_type_aliases.as_deref().unwrap();
        let save_container = self.maybe_container();
        let save_last_container = self.maybe_last_container();
        let save_block_scope_container = self.maybe_block_scope_container();
        let save_parent = self.maybe_parent();
        let save_current_flow = self.maybe_current_flow();
        for &type_alias in delayed_type_aliases {
            let host = type_alias.ref_(self).parent().ref_(self).parent();
            self.set_container(Some(
                find_ancestor(host.ref_(self).maybe_parent(), |n| {
                    self.get_container_flags(n)
                        .intersects(ContainerFlags::IsContainer)
                }, self)
                .unwrap_or_else(|| self.file()),
            ));
            self.set_block_scope_container(Some(
                get_enclosing_block_scope_container(host, self).unwrap_or_else(|| self.file()),
            ));
            self.set_current_flow(Some(Gc::new(init_flow_node(
                FlowStart::new(FlowFlags::Start, None).into(),
            ))));
            self.set_parent(Some(type_alias.clone()));
            let type_alias_ref = type_alias.ref_(self);
            let type_alias_as_jsdoc_type_like_tag = type_alias_ref.as_jsdoc_type_like_tag();
            self.bind(type_alias_as_jsdoc_type_like_tag.maybe_type_expression());
            let decl_name = get_name_of_declaration(Some(type_alias), self);
            if (is_jsdoc_enum_tag(&type_alias.ref_(self))
                || type_alias
                    .ref_(self).as_jsdoc_typedef_or_callback_tag()
                    .maybe_full_name()
                    .is_none())
                && matches!(
                    decl_name,
                    Some(decl_name) if is_property_access_entity_name_expression(decl_name.ref_(self).parent(), self)
                )
            {
                let decl_name = decl_name.unwrap();
                let is_top_level = self.is_top_level_namespace_assignment(decl_name.ref_(self).parent());
                if is_top_level {
                    self.bind_potentially_missing_namespaces(
                        self.file().ref_(self).maybe_symbol(),
                        decl_name.ref_(self).parent(),
                        is_top_level,
                        find_ancestor(Some(decl_name), |d| {
                            is_property_access_expression(&d.ref_(self))
                                && d.ref_(self).as_property_access_expression()
                                    .name
                                    .ref_(self).as_member_name()
                                    .escaped_text()
                                    == "prototype"
                        }, self)
                        .is_some(),
                        false,
                    );
                    let old_container = self.maybe_container();
                    match get_assignment_declaration_property_access_kind(decl_name.ref_(self).parent(), self) {
                        AssignmentDeclarationKind::ExportsProperty
                        | AssignmentDeclarationKind::ModuleExports => {
                            if !is_external_or_common_js_module(&self.file().ref_(self)) {
                                self.set_container(None);
                            } else {
                                self.set_container(Some(self.file()));
                            }
                        }
                        AssignmentDeclarationKind::ThisProperty => {
                            self.set_container(Some(
                                decl_name.ref_(self).parent().ref_(self).as_has_expression().expression(),
                            ));
                        }
                        AssignmentDeclarationKind::PrototypeProperty => {
                            self.set_container(Some(
                                decl_name
                                    .ref_(self).parent()
                                    .ref_(self).as_has_expression()
                                    .expression()
                                    .ref_(self).as_property_access_expression()
                                    .name
                            ));
                        }
                        AssignmentDeclarationKind::Property => {
                            self.set_container(Some(
                                if is_exports_or_module_exports_or_alias(
                                    self,
                                    self.file(),
                                    decl_name.ref_(self).parent().ref_(self).as_has_expression().expression(),
                                ) {
                                    self.file()
                                } else if is_property_access_expression(
                                    &decl_name.ref_(self).parent().ref_(self).as_has_expression().expression().ref_(self),
                                ) {
                                    decl_name
                                        .ref_(self).parent()
                                        .ref_(self).as_has_expression()
                                        .expression()
                                        .ref_(self).as_property_access_expression()
                                        .name
                                } else {
                                    decl_name.ref_(self).parent().ref_(self).as_has_expression().expression()
                                },
                            ));
                        }
                        AssignmentDeclarationKind::None => {
                            Debug_.fail(Some("Shouldn't have detected typedef or enum or non-assignment declaration"));
                        }
                        _ => (),
                    }
                    if self.maybe_container().is_some() {
                        self.declare_module_member(
                            type_alias,
                            SymbolFlags::TypeAlias,
                            SymbolFlags::TypeAliasExcludes,
                        );
                    }
                    self.set_container(old_container);
                }
            } else if is_jsdoc_enum_tag(&type_alias.ref_(self))
                || match type_alias
                    .ref_(self).as_jsdoc_typedef_or_callback_tag()
                    .maybe_full_name()
                {
                    None => true,
                    Some(full_name) => full_name.ref_(self).kind() == SyntaxKind::Identifier,
                }
            {
                self.set_parent(Some(type_alias.ref_(self).parent()));
                self.bind_block_scoped_declaration(
                    type_alias,
                    SymbolFlags::TypeAlias,
                    SymbolFlags::TypeAliasExcludes,
                );
            } else {
                self.bind(
                    type_alias
                        .ref_(self).as_jsdoc_typedef_or_callback_tag()
                        .maybe_full_name(),
                );
            }
        }
        self.set_container(save_container);
        self.set_last_container(save_last_container);
        self.set_block_scope_container(save_block_scope_container);
        self.set_parent(save_parent);
        self.set_current_flow(save_current_flow);
    }

    pub(super) fn check_contextual_identifier(
        &self,
        node: Id<Node>, /*Identifier (and whatever ThisKeyword is) */
    ) {
        if (*self.file().ref_(self).as_source_file().parse_diagnostics())
            .borrow()
            .is_empty()
            && !node.ref_(self).flags().intersects(NodeFlags::Ambient)
            && !node.ref_(self).flags().intersects(NodeFlags::JSDoc)
            && !is_identifier_name(node, self)
        {
            let node_original_keyword_kind = node
                .ref_(self).maybe_as_identifier()
                .and_then(|node| node.original_keyword_kind);
            if matches!(self.maybe_in_strict_mode(), Some(true))
                && matches!(node_original_keyword_kind, Some(original_keyword_kind) if original_keyword_kind >= SyntaxKind::FirstFutureReservedWord && original_keyword_kind <= SyntaxKind::LastFutureReservedWord)
            {
                self.file()
                    .ref_(self).as_source_file()
                    .bind_diagnostics_mut()
                    .push(Gc::new(
                        self.create_diagnostic_for_node(
                            node,
                            self.get_strict_mode_identifier_message(node),
                            Some(vec![declaration_name_to_string(Some(node), self).into_owned()]),
                        )
                        .into(),
                    ));
            } else if matches!(node_original_keyword_kind, Some(SyntaxKind::AwaitKeyword)) {
                if is_external_module(&self.file().ref_(self)) && is_in_top_level_context(node, self) {
                    self.file()
                        .ref_(self).as_source_file()
                        .bind_diagnostics_mut()
                        .push(Gc::new(
                            self.create_diagnostic_for_node(
                                node,
                                &Diagnostics::Identifier_expected_0_is_a_reserved_word_at_the_top_level_of_a_module,
                                Some(vec![declaration_name_to_string(Some(node), self).into_owned()]),
                            )
                            .into(),
                        ));
                } else if node.ref_(self).flags().intersects(NodeFlags::AwaitContext) {
                    self.file()
                        .ref_(self).as_source_file()
                        .bind_diagnostics_mut()
                        .push(Gc::new(
                            self.create_diagnostic_for_node(
                                node,
                                &Diagnostics::Identifier_expected_0_is_a_reserved_word_that_cannot_be_used_here,
                                Some(vec![declaration_name_to_string(Some(node), self).into_owned()]),
                            )
                            .into(),
                        ));
                }
            } else if matches!(node_original_keyword_kind, Some(SyntaxKind::YieldKeyword))
                && node.ref_(self).flags().intersects(NodeFlags::YieldContext)
            {
                self.file()
                    .ref_(self).as_source_file()
                    .bind_diagnostics_mut()
                    .push(Gc::new(
                        self.create_diagnostic_for_node(
                            node,
                            &Diagnostics::Identifier_expected_0_is_a_reserved_word_that_cannot_be_used_here,
                            Some(vec![declaration_name_to_string(Some(node), self).into_owned()]),
                        )
                        .into(),
                    ));
            }
        }
    }

    pub(super) fn get_strict_mode_identifier_message(
        &self,
        node: Id<Node>,
    ) -> &'static DiagnosticMessage {
        if get_containing_class(node, self).is_some() {
            return &Diagnostics::Identifier_expected_0_is_a_reserved_word_in_strict_mode_Class_definitions_are_automatically_in_strict_mode;
        }

        if self
            .file()
            .ref_(self).as_source_file()
            .maybe_external_module_indicator()
            .is_some()
        {
            return &Diagnostics::Identifier_expected_0_is_a_reserved_word_in_strict_mode_Modules_are_automatically_in_strict_mode;
        }

        &Diagnostics::Identifier_expected_0_is_a_reserved_word_in_strict_mode
    }

    pub(super) fn check_private_identifier(&self, node: Id<Node> /*PrivateIdentifier*/) {
        if node.ref_(self).as_private_identifier().escaped_text == "#constructor" {
            let file = self.file();
            let file_ref = file.ref_(self);
            let file_as_source_file = file_ref.as_source_file();
            if (*file_as_source_file.parse_diagnostics())
                .borrow()
                .is_empty()
            {
                file_as_source_file.bind_diagnostics_mut().push(Gc::new(
                    self.create_diagnostic_for_node(
                        node,
                        &Diagnostics::constructor_is_a_reserved_word,
                        Some(vec![declaration_name_to_string(Some(node), self).into_owned()]),
                    )
                    .into(),
                ));
            }
        }
    }

    pub(super) fn check_strict_mode_binary_expression(
        &self,
        node: Id<Node>, /*BinaryExpression*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        if matches!(self.maybe_in_strict_mode(), Some(true))
            && is_left_hand_side_expression(node_as_binary_expression.left, self)
            && is_assignment_operator(node_as_binary_expression.operator_token.ref_(self).kind())
        {
            self.check_strict_mode_eval_or_arguments(node, Some(node_as_binary_expression.left));
        }
    }

    pub(super) fn check_strict_mode_catch_clause(&self, node: Id<Node> /*CatchClause*/) {
        let node_ref = node.ref_(self);
        let node_as_catch_clause = node_ref.as_catch_clause();
        if self.maybe_in_strict_mode() == Some(true) {
            if let Some(node_variable_declaration) =
                node_as_catch_clause.variable_declaration
            {
                self.check_strict_mode_eval_or_arguments(
                    node,
                    node_variable_declaration
                        .ref_(self).as_variable_declaration()
                        .maybe_name(),
                );
            }
        }
    }

    pub(super) fn check_strict_mode_delete_expression(
        &self,
        node: Id<Node>, /*DeleteExpression*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_delete_expression = node_ref.as_delete_expression();
        if self.maybe_in_strict_mode() == Some(true) {
            if node_as_delete_expression.expression.ref_(self).kind() == SyntaxKind::Identifier {
                let span =
                    get_error_span_for_node(self.file(), node_as_delete_expression.expression, self);
                self.file()
                    .ref_(self).as_source_file()
                    .bind_diagnostics_mut()
                    .push(Gc::new(
                        create_file_diagnostic(
                            self.file(),
                            span.start,
                            span.length,
                            &Diagnostics::delete_cannot_be_called_on_an_identifier_in_strict_mode,
                            None,
                        )
                        .into(),
                    ));
            }
        }
    }

    pub(super) fn is_eval_or_arguments_identifier(&self, node: Id<Node>) -> bool {
        is_identifier(&node.ref_(self)) && matches!(
            &*node.ref_(self).as_identifier().escaped_text,
            "eval" | "arguments"
        )
    }

    pub(super) fn check_strict_mode_eval_or_arguments(
        &self,
        context_node: Id<Node>,
        name: Option<Id<Node>>,
    ) {
        let Some(name) = name else {
            return;
        };
        if name.ref_(self).kind() == SyntaxKind::Identifier {
            let identifier = name;
            if self.is_eval_or_arguments_identifier(identifier) {
                let span = get_error_span_for_node(self.file(), name, self);
                self.file()
                    .ref_(self).as_source_file()
                    .bind_diagnostics_mut()
                    .push(Gc::new(
                        create_file_diagnostic(
                            self.file(),
                            span.start,
                            span.length,
                            self.get_strict_mode_eval_or_arguments_message(context_node),
                            Some(vec![id_text(&identifier.ref_(self)).to_owned()]),
                        )
                        .into(),
                    ));
            }
        }
    }

    pub(super) fn get_strict_mode_eval_or_arguments_message(
        &self,
        node: Id<Node>,
    ) -> &'static DiagnosticMessage {
        if get_containing_class(node, self).is_some() {
            return &Diagnostics::Code_contained_in_a_class_is_evaluated_in_JavaScript_s_strict_mode_which_does_not_allow_this_use_of_0_For_more_information_see_https_Colon_Slash_Slashdeveloper_mozilla_org_Slashen_US_Slashdocs_SlashWeb_SlashJavaScript_SlashReference_SlashStrict_mode;
        }

        if self
            .file()
            .ref_(self).as_source_file()
            .maybe_external_module_indicator()
            .is_some()
        {
            return &Diagnostics::Invalid_use_of_0_Modules_are_automatically_in_strict_mode;
        }

        &Diagnostics::Invalid_use_of_0_in_strict_mode
    }

    pub(super) fn check_strict_mode_function_name(
        &self,
        node: Id<Node>, /*FunctionLikeDeclaration*/
    ) {
        if self.maybe_in_strict_mode() == Some(true) {
            self.check_strict_mode_eval_or_arguments(
                node,
                node.ref_(self).as_named_declaration().maybe_name(),
            );
        }
    }

    pub(super) fn get_strict_mode_block_scope_function_declaration_message(
        &self,
        node: Id<Node>,
    ) -> &'static DiagnosticMessage {
        if get_containing_class(node, self).is_some() {
            return &Diagnostics::Function_declarations_are_not_allowed_inside_blocks_in_strict_mode_when_targeting_ES3_or_ES5_Class_definitions_are_automatically_in_strict_mode;
        }

        if self
            .file()
            .ref_(self).as_source_file()
            .maybe_external_module_indicator()
            .is_some()
        {
            return &Diagnostics::Function_declarations_are_not_allowed_inside_blocks_in_strict_mode_when_targeting_ES3_or_ES5_Modules_are_automatically_in_strict_mode;
        }

        &Diagnostics::Function_declarations_are_not_allowed_inside_blocks_in_strict_mode_when_targeting_ES3_or_ES5
    }

    pub(super) fn check_strict_mode_function_declaration(
        &self,
        node: Id<Node>, /*FunctionDeclaration*/
    ) {
        if matches!(self.maybe_language_version(), Some(language_version) if language_version < ScriptTarget::ES2015)
        {
            let block_scope_container = self.block_scope_container();
            if !matches!(
                block_scope_container.ref_(self).kind(),
                SyntaxKind::SourceFile | SyntaxKind::ModuleDeclaration
            ) && !is_function_like_or_class_static_block_declaration(Some(
                &block_scope_container.ref_(self),
            )) {
                let error_span = get_error_span_for_node(self.file(), node, self);
                self.file()
                    .ref_(self).as_source_file()
                    .bind_diagnostics_mut()
                    .push(Gc::new(
                        create_file_diagnostic(
                            self.file(),
                            error_span.start,
                            error_span.length,
                            self.get_strict_mode_block_scope_function_declaration_message(node),
                            None,
                        )
                        .into(),
                    ));
            }
        }
    }

    pub(super) fn check_strict_mode_numeric_literal(&self, node: Id<Node> /*NumericLiteral*/) {
        if self.maybe_in_strict_mode() == Some(true) {
            let node_ref = node.ref_(self);
            let node_as_numeric_literal = node_ref.as_numeric_literal();
            if node_as_numeric_literal
                .numeric_literal_flags
                .intersects(TokenFlags::Octal)
            {
                self.file()
                    .ref_(self).as_source_file()
                    .bind_diagnostics_mut()
                    .push(Gc::new(
                        self.create_diagnostic_for_node(
                            node,
                            &Diagnostics::Octal_literals_are_not_allowed_in_strict_mode,
                            None,
                        )
                        .into(),
                    ));
            }
        }
    }

    pub(super) fn check_strict_mode_postfix_unary_expression(
        &self,
        node: Id<Node>, /*PostfixUnaryExpression*/
    ) {
        if matches!(self.maybe_in_strict_mode(), Some(true)) {
            self.check_strict_mode_eval_or_arguments(
                node,
                Some(node.ref_(self).as_postfix_unary_expression().operand),
            );
        }
    }

    pub(super) fn check_strict_mode_prefix_unary_expression(
        &self,
        node: Id<Node>, /*PrefixUnaryExpression*/
    ) {
        if self.maybe_in_strict_mode() == Some(true) {
            let node_ref = node.ref_(self);
            let node_as_prefix_unary_expression = node_ref.as_prefix_unary_expression();
            if matches!(
                node_as_prefix_unary_expression.operator,
                SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
            ) {
                self.check_strict_mode_eval_or_arguments(
                    node,
                    Some(node_as_prefix_unary_expression.operand),
                );
            }
        }
    }

    pub(super) fn check_strict_mode_with_statement(&self, node: Id<Node> /*WithStatement*/) {
        if matches!(self.maybe_in_strict_mode(), Some(true)) {
            self.error_on_first_token(
                node,
                &Diagnostics::with_statements_are_not_allowed_in_strict_mode,
                None,
            );
        }
    }

    pub(super) fn check_strict_mode_labeled_statement(
        &self,
        node: Id<Node>, /*LabeledStatement*/
    ) {
        if self.maybe_in_strict_mode() == Some(true)
            && get_emit_script_target(&self.options().ref_(self)) >= ScriptTarget::ES2015
        {
            let node_ref = node.ref_(self);
            let node_as_labeled_statement = node_ref.as_labeled_statement();
            if is_declaration_statement(&node_as_labeled_statement.statement.ref_(self))
                || is_variable_statement(&node_as_labeled_statement.statement.ref_(self))
            {
                self.error_on_first_token(
                    node_as_labeled_statement.label,
                    &Diagnostics::A_label_is_not_allowed_here,
                    None,
                );
            }
        }
    }

    pub(super) fn error_on_first_token(
        &self,
        node: Id<Node>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        let span = get_span_of_token_at_position(&self.file().ref_(self), node.ref_(self).pos().try_into().unwrap());
        self.file()
            .ref_(self).as_source_file()
            .bind_diagnostics_mut()
            .push(Gc::new(
                create_file_diagnostic(self.file(), span.start, span.length, message, args).into(),
            ));
    }

    pub(super) fn error_or_suggestion_on_node(
        &self,
        is_error: bool,
        node: Id<Node>,
        message: &DiagnosticMessage,
    ) {
        self.error_or_suggestion_on_range(is_error, node, node, message);
    }

    pub(super) fn error_or_suggestion_on_range(
        &self,
        is_error: bool,
        start_node: Id<Node>,
        end_node: Id<Node>,
        message: &DiagnosticMessage,
    ) {
        self.add_error_or_suggestion_diagnostic(
            is_error,
            BaseTextRange::new(
                get_token_pos_of_node(start_node, Some(self.file()), None, self),
                end_node.ref_(self).end(),
            ),
            message,
        );
    }

    pub(super) fn add_error_or_suggestion_diagnostic(
        &self,
        is_error: bool,
        range: BaseTextRange,
        message: &DiagnosticMessage,
    ) {
        let diag = Gc::new(
            create_file_diagnostic(
                self.file(),
                range.pos(),
                range.end() - range.pos(),
                message,
                None,
            )
            .into(),
        );
        if is_error {
            self.file()
                .ref_(self).as_source_file()
                .bind_diagnostics_mut()
                .push(diag);
        } else {
            diag.set_category(DiagnosticCategory::Suggestion);
            let file = self.file();
            let file_ref = file.ref_(self);
            let mut file_bind_suggestion_diagnostics =
                file_ref.as_source_file().maybe_bind_suggestion_diagnostics();
            if file_bind_suggestion_diagnostics.is_none() {
                *file_bind_suggestion_diagnostics = Some(vec![]);
            }
            append(
                file_bind_suggestion_diagnostics.as_mut().unwrap(),
                Some(diag),
            );
        }
    }

    pub(super) fn bind(&self, node: Option<Id<Node>>) {
        let Some(node) = node else {
            return;
        };
        set_parent(&node.ref_(self), self.maybe_parent());
        let save_in_strict_mode = self.maybe_in_strict_mode();

        self.bind_worker(node);

        if node.ref_(self).kind() > SyntaxKind::LastToken {
            let save_parent = self.maybe_parent();
            self.set_parent(Some(node));
            let container_flags = self.get_container_flags(node);
            if container_flags == ContainerFlags::None {
                self.bind_children(node);
            } else {
                self.bind_container(node, container_flags);
            }
            self.set_parent(save_parent);
        } else {
            let save_parent = self.maybe_parent();
            if node.ref_(self).kind() == SyntaxKind::EndOfFileToken {
                self.set_parent(Some(node));
            }
            self.bind_jsdoc(node);
            self.set_parent(save_parent);
        }
        self.set_in_strict_mode(save_in_strict_mode);
    }

    pub(super) fn bind_jsdoc(&self, node: Id<Node>) {
        if has_jsdoc_nodes(&node.ref_(self)) {
            if is_in_js_file(Some(&node.ref_(self))) {
                for j in node.ref_(self).maybe_js_doc().unwrap() {
                    self.bind(Some(j));
                }
            } else {
                for j in node.ref_(self).maybe_js_doc().unwrap() {
                    set_parent(&j.ref_(self), Some(node));
                    set_parent_recursive(Some(j), false, self);
                }
            }
        }
    }

    pub(super) fn update_strict_mode_statement_list(
        &self,
        statements: &NodeArray, /*<Statement>*/
    ) {
        if self.maybe_in_strict_mode() != Some(true) {
            for &statement in statements {
                if !is_prologue_directive(statement, self) {
                    return;
                }

                if self.is_use_strict_prologue_directive(statement) {
                    self.set_in_strict_mode(Some(true));
                    return;
                }
            }
        }
    }

    pub(super) fn is_use_strict_prologue_directive(
        &self,
        node: Id<Node>, /*ExpressionStatement*/
    ) -> bool {
        let node_text = get_source_text_of_node_from_source_file(
            self.file(),
            node.ref_(self).as_expression_statement().expression,
            None,
            self,
        );

        matches!(&*node_text, "\"use strict\"" | "'use strict'")
    }
}
