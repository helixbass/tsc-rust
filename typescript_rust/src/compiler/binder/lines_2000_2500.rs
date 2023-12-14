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
    Debug_, DiagnosticCategory, DiagnosticMessage, Diagnostics, FlowFlags, FlowStart,
    InternalSymbolName, NamedDeclarationInterface, Node, NodeFlags, NodeInterface,
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
        node: &Node, /*ModuleDeclaration*/
    ) -> ModuleInstanceState {
        let state = get_module_instance_state(node, None);
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
        node: &Node, /*SignatureDeclaration | JSDocSignature*/
    ) {
        let symbol = self.alloc_symbol(self.create_symbol(
            SymbolFlags::Signature,
            self.get_declaration_name(node).unwrap().into_owned(),
        ));
        self.add_declaration_to_symbol(symbol, node, SymbolFlags::Signature);

        let type_literal_symbol = self.alloc_symbol(self
            .create_symbol(
                SymbolFlags::TypeLiteral,
                InternalSymbolName::Type.to_owned(),
            ));
        self.add_declaration_to_symbol(type_literal_symbol, node, SymbolFlags::TypeLiteral);
        let mut type_literal_symbol_members = self.symbol(type_literal_symbol).maybe_members_mut();
        *type_literal_symbol_members = Some(Gc::new(GcCell::new(create_symbol_table(
            self.arena(),
            Option::<&[Id<Symbol>]>::None,
        ))));
        type_literal_symbol_members
            .as_ref()
            .unwrap()
            .borrow_mut()
            .insert(self.symbol(symbol).escaped_name().to_owned(), symbol);
    }

    pub(super) fn bind_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) {
        if matches!(self.maybe_in_strict_mode(), Some(true)) && !is_assignment_target(node) {
            let mut seen = HashMap::<__String, ElementKind>::new();

            let node_as_object_literal_expression = node.as_object_literal_expression();
            for prop in &*node_as_object_literal_expression.properties {
                if prop.kind() == SyntaxKind::SpreadAssignment
                    || prop.as_named_declaration().name().kind() != SyntaxKind::Identifier
                {
                    continue;
                }

                let identifier = prop.as_named_declaration().name();

                let current_kind = if matches!(
                    prop.kind(),
                    SyntaxKind::PropertyAssignment
                        | SyntaxKind::ShorthandPropertyAssignment
                        | SyntaxKind::MethodDeclaration
                ) {
                    ElementKind::Property
                } else {
                    ElementKind::Accessor
                };

                let identifier_as_identifier = identifier.as_identifier();
                let existing_kind = seen.get(&identifier_as_identifier.escaped_text);
                if existing_kind.is_none() {
                    seen.insert(identifier_as_identifier.escaped_text.clone(), current_kind);
                    continue;
                }
                let existing_kind = *existing_kind.unwrap();

                if current_kind == ElementKind::Property && existing_kind == ElementKind::Property {
                    let file = self.file();
                    let span = get_error_span_for_node(&file, &identifier);
                    file.as_source_file().bind_diagnostics_mut().push(
                        Gc::new(
                            create_file_diagnostic(
                                &file,
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

    pub(super) fn bind_jsx_attributes(&self, node: &Node /*JsxAttributes*/) -> Id<Symbol> {
        self.bind_anonymous_declaration(
            node,
            SymbolFlags::ObjectLiteral,
            InternalSymbolName::JSXAttributes.to_owned(),
        )
    }

    pub(super) fn bind_jsx_attribute(
        &self,
        node: &Node, /*JsxAttribute*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Option<Id<Symbol>> {
        self.declare_symbol_and_add_to_symbol_table(node, symbol_flags, symbol_excludes)
    }

    pub(super) fn bind_anonymous_declaration(
        &self,
        node: &Node,
        symbol_flags: SymbolFlags,
        name: __String,
    ) -> Id<Symbol> {
        let symbol = self.alloc_symbol(self.create_symbol(symbol_flags, name));
        if symbol_flags.intersects(SymbolFlags::EnumMember | SymbolFlags::ClassMember) {
            self.symbol(symbol)
                .set_parent(self.container().maybe_symbol());
        }
        self.add_declaration_to_symbol(symbol, node, symbol_flags);
        symbol
    }

    pub(super) fn bind_block_scoped_declaration(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) {
        let block_scope_container = self.block_scope_container();
        match block_scope_container.kind() {
            SyntaxKind::ModuleDeclaration => {
                self.declare_module_member(node, symbol_flags, symbol_excludes);
            }
            SyntaxKind::SourceFile => {
                if is_external_or_common_js_module(&self.container()) {
                    self.declare_module_member(node, symbol_flags, symbol_excludes);
                } else {
                    {
                        let mut block_scope_container_locals =
                            block_scope_container.maybe_locals_mut();
                        if block_scope_container_locals.is_none() {
                            *block_scope_container_locals = Some(Gc::new(GcCell::new(
                                create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None),
                            )));
                            self.add_to_container_chain(&block_scope_container);
                        }
                    }
                    self.declare_symbol(
                        &mut *block_scope_container.locals().borrow_mut(),
                        Option::<Id<Symbol>>::None,
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
                    let mut block_scope_container_locals = block_scope_container.maybe_locals_mut();
                    if block_scope_container_locals.is_none() {
                        *block_scope_container_locals = Some(Gc::new(GcCell::new(
                            create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None),
                        )));
                        self.add_to_container_chain(&block_scope_container);
                    }
                }
                self.declare_symbol(
                    &mut block_scope_container.locals().borrow_mut(),
                    Option::<Id<Symbol>>::None,
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
        for type_alias in delayed_type_aliases {
            let host = type_alias.parent().parent();
            self.set_container(Some(
                find_ancestor(host.maybe_parent(), |n| {
                    self.get_container_flags(n)
                        .intersects(ContainerFlags::IsContainer)
                })
                .unwrap_or_else(|| self.file()),
            ));
            self.set_block_scope_container(Some(
                get_enclosing_block_scope_container(&host).unwrap_or_else(|| self.file()),
            ));
            self.set_current_flow(Some(Gc::new(init_flow_node(
                FlowStart::new(FlowFlags::Start, None).into(),
            ))));
            self.set_parent(Some(type_alias.clone()));
            let type_alias_as_jsdoc_type_like_tag = type_alias.as_jsdoc_type_like_tag();
            self.bind(type_alias_as_jsdoc_type_like_tag.maybe_type_expression());
            let decl_name = get_name_of_declaration(Some(&**type_alias));
            if (is_jsdoc_enum_tag(type_alias)
                || type_alias
                    .as_jsdoc_typedef_or_callback_tag()
                    .maybe_full_name()
                    .is_none())
                && matches!(decl_name.as_ref(), Some(decl_name) if is_property_access_entity_name_expression(&decl_name.parent()))
            {
                let decl_name = decl_name.unwrap();
                let is_top_level = self.is_top_level_namespace_assignment(&decl_name.parent());
                if is_top_level {
                    self.bind_potentially_missing_namespaces(
                        self.file().maybe_symbol(),
                        &decl_name.parent(),
                        is_top_level,
                        find_ancestor(Some(&*decl_name), |d| {
                            is_property_access_expression(d)
                                && d.as_property_access_expression()
                                    .name
                                    .as_member_name()
                                    .escaped_text()
                                    == "prototype"
                        })
                        .is_some(),
                        false,
                    );
                    let old_container = self.maybe_container();
                    match get_assignment_declaration_property_access_kind(&decl_name.parent()) {
                        AssignmentDeclarationKind::ExportsProperty
                        | AssignmentDeclarationKind::ModuleExports => {
                            if !is_external_or_common_js_module(&self.file()) {
                                self.set_container(None);
                            } else {
                                self.set_container(Some(self.file()));
                            }
                        }
                        AssignmentDeclarationKind::ThisProperty => {
                            self.set_container(Some(
                                decl_name.parent().as_has_expression().expression(),
                            ));
                        }
                        AssignmentDeclarationKind::PrototypeProperty => {
                            self.set_container(Some(
                                decl_name
                                    .parent()
                                    .as_has_expression()
                                    .expression()
                                    .as_property_access_expression()
                                    .name
                                    .clone(),
                            ));
                        }
                        AssignmentDeclarationKind::Property => {
                            self.set_container(Some(
                                if is_exports_or_module_exports_or_alias(
                                    self,
                                    &self.file(),
                                    &decl_name.parent().as_has_expression().expression(),
                                ) {
                                    self.file()
                                } else if is_property_access_expression(
                                    &decl_name.parent().as_has_expression().expression(),
                                ) {
                                    decl_name
                                        .parent()
                                        .as_has_expression()
                                        .expression()
                                        .as_property_access_expression()
                                        .name
                                        .clone()
                                } else {
                                    decl_name.parent().as_has_expression().expression()
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
            } else if is_jsdoc_enum_tag(type_alias)
                || match type_alias
                    .as_jsdoc_typedef_or_callback_tag()
                    .maybe_full_name()
                {
                    None => true,
                    Some(full_name) => full_name.kind() == SyntaxKind::Identifier,
                }
            {
                self.set_parent(Some(type_alias.parent()));
                self.bind_block_scoped_declaration(
                    type_alias,
                    SymbolFlags::TypeAlias,
                    SymbolFlags::TypeAliasExcludes,
                );
            } else {
                self.bind(
                    type_alias
                        .as_jsdoc_typedef_or_callback_tag()
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
        node: &Node, /*Identifier (and whatever ThisKeyword is) */
    ) {
        if (*self.file().as_source_file().parse_diagnostics())
            .borrow()
            .is_empty()
            && !node.flags().intersects(NodeFlags::Ambient)
            && !node.flags().intersects(NodeFlags::JSDoc)
            && !is_identifier_name(node)
        {
            let node_original_keyword_kind = node
                .maybe_as_identifier()
                .and_then(|node| node.original_keyword_kind);
            if matches!(self.maybe_in_strict_mode(), Some(true))
                && matches!(node_original_keyword_kind, Some(original_keyword_kind) if original_keyword_kind >= SyntaxKind::FirstFutureReservedWord && original_keyword_kind <= SyntaxKind::LastFutureReservedWord)
            {
                self.file()
                    .as_source_file()
                    .bind_diagnostics_mut()
                    .push(Gc::new(
                        self.create_diagnostic_for_node(
                            node,
                            self.get_strict_mode_identifier_message(node),
                            Some(vec![declaration_name_to_string(Some(node)).into_owned()]),
                        )
                        .into(),
                    ));
            } else if matches!(node_original_keyword_kind, Some(SyntaxKind::AwaitKeyword)) {
                if is_external_module(&self.file()) && is_in_top_level_context(node) {
                    self.file()
                        .as_source_file()
                        .bind_diagnostics_mut()
                        .push(Gc::new(
                            self.create_diagnostic_for_node(
                                node,
                                &Diagnostics::Identifier_expected_0_is_a_reserved_word_at_the_top_level_of_a_module,
                                Some(vec![declaration_name_to_string(Some(node)).into_owned()]),
                            )
                            .into(),
                        ));
                } else if node.flags().intersects(NodeFlags::AwaitContext) {
                    self.file()
                        .as_source_file()
                        .bind_diagnostics_mut()
                        .push(Gc::new(
                            self.create_diagnostic_for_node(
                                node,
                                &Diagnostics::Identifier_expected_0_is_a_reserved_word_that_cannot_be_used_here,
                                Some(vec![declaration_name_to_string(Some(node)).into_owned()]),
                            )
                            .into(),
                        ));
                }
            } else if matches!(node_original_keyword_kind, Some(SyntaxKind::YieldKeyword))
                && node.flags().intersects(NodeFlags::YieldContext)
            {
                self.file()
                    .as_source_file()
                    .bind_diagnostics_mut()
                    .push(Gc::new(
                        self.create_diagnostic_for_node(
                            node,
                            &Diagnostics::Identifier_expected_0_is_a_reserved_word_that_cannot_be_used_here,
                            Some(vec![declaration_name_to_string(Some(node)).into_owned()]),
                        )
                        .into(),
                    ));
            }
        }
    }

    pub(super) fn get_strict_mode_identifier_message(
        &self,
        node: &Node,
    ) -> &'static DiagnosticMessage {
        if get_containing_class(node).is_some() {
            return &Diagnostics::Identifier_expected_0_is_a_reserved_word_in_strict_mode_Class_definitions_are_automatically_in_strict_mode;
        }

        if self
            .file()
            .as_source_file()
            .maybe_external_module_indicator()
            .is_some()
        {
            return &Diagnostics::Identifier_expected_0_is_a_reserved_word_in_strict_mode_Modules_are_automatically_in_strict_mode;
        }

        &Diagnostics::Identifier_expected_0_is_a_reserved_word_in_strict_mode
    }

    pub(super) fn check_private_identifier(&self, node: &Node /*PrivateIdentifier*/) {
        if node.as_private_identifier().escaped_text == "#constructor" {
            let file = self.file();
            let file_as_source_file = file.as_source_file();
            if (*file_as_source_file.parse_diagnostics())
                .borrow()
                .is_empty()
            {
                file_as_source_file.bind_diagnostics_mut().push(Gc::new(
                    self.create_diagnostic_for_node(
                        node,
                        &Diagnostics::constructor_is_a_reserved_word,
                        Some(vec![declaration_name_to_string(Some(node)).into_owned()]),
                    )
                    .into(),
                ));
            }
        }
    }

    pub(super) fn check_strict_mode_binary_expression(
        &self,
        node: &Node, /*BinaryExpression*/
    ) {
        let node_as_binary_expression = node.as_binary_expression();
        if matches!(self.maybe_in_strict_mode(), Some(true))
            && is_left_hand_side_expression(&node_as_binary_expression.left)
            && is_assignment_operator(node_as_binary_expression.operator_token.kind())
        {
            self.check_strict_mode_eval_or_arguments(node, Some(&*node_as_binary_expression.left));
        }
    }

    pub(super) fn check_strict_mode_catch_clause(&self, node: &Node /*CatchClause*/) {
        let node_as_catch_clause = node.as_catch_clause();
        if matches!(self.maybe_in_strict_mode(), Some(true)) {
            if let Some(node_variable_declaration) =
                node_as_catch_clause.variable_declaration.as_ref()
            {
                self.check_strict_mode_eval_or_arguments(
                    node,
                    node_variable_declaration
                        .as_variable_declaration()
                        .maybe_name(),
                );
            }
        }
    }

    pub(super) fn check_strict_mode_delete_expression(
        &self,
        node: &Node, /*DeleteExpression*/
    ) {
        let node_as_delete_expression = node.as_delete_expression();
        if matches!(self.maybe_in_strict_mode(), Some(true)) {
            if node_as_delete_expression.expression.kind() == SyntaxKind::Identifier {
                let span =
                    get_error_span_for_node(&self.file(), &node_as_delete_expression.expression);
                self.file()
                    .as_source_file()
                    .bind_diagnostics_mut()
                    .push(Gc::new(
                        create_file_diagnostic(
                            &self.file(),
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

    pub(super) fn is_eval_or_arguments_identifier(&self, node: &Node) -> bool {
        is_identifier(node) && matches!(&*node.as_identifier().escaped_text, "eval" | "arguments")
    }

    pub(super) fn check_strict_mode_eval_or_arguments<TName: Borrow<Node>>(
        &self,
        context_node: &Node,
        name: Option<TName>,
    ) {
        if name.is_none() {
            return;
        }
        let name = name.unwrap();
        let name = name.borrow();
        if name.kind() == SyntaxKind::Identifier {
            let identifier = name;
            if self.is_eval_or_arguments_identifier(identifier) {
                let span = get_error_span_for_node(&self.file(), name);
                self.file()
                    .as_source_file()
                    .bind_diagnostics_mut()
                    .push(Gc::new(
                        create_file_diagnostic(
                            &self.file(),
                            span.start,
                            span.length,
                            self.get_strict_mode_eval_or_arguments_message(context_node),
                            Some(vec![id_text(identifier).to_owned()]),
                        )
                        .into(),
                    ));
            }
        }
    }

    pub(super) fn get_strict_mode_eval_or_arguments_message(
        &self,
        node: &Node,
    ) -> &'static DiagnosticMessage {
        if get_containing_class(node).is_some() {
            return &Diagnostics::Code_contained_in_a_class_is_evaluated_in_JavaScript_s_strict_mode_which_does_not_allow_this_use_of_0_For_more_information_see_https_Colon_Slash_Slashdeveloper_mozilla_org_Slashen_US_Slashdocs_SlashWeb_SlashJavaScript_SlashReference_SlashStrict_mode;
        }

        if self
            .file()
            .as_source_file()
            .maybe_external_module_indicator()
            .is_some()
        {
            return &Diagnostics::Invalid_use_of_0_Modules_are_automatically_in_strict_mode;
        }

        &Diagnostics::Invalid_use_of_0_in_strict_mode
    }

    pub(super) fn check_strict_mode_function_name(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) {
        if matches!(self.maybe_in_strict_mode(), Some(true)) {
            self.check_strict_mode_eval_or_arguments(
                node,
                node.as_named_declaration().maybe_name(),
            );
        }
    }

    pub(super) fn get_strict_mode_block_scope_function_declaration_message(
        &self,
        node: &Node,
    ) -> &'static DiagnosticMessage {
        if get_containing_class(node).is_some() {
            return &Diagnostics::Function_declarations_are_not_allowed_inside_blocks_in_strict_mode_when_targeting_ES3_or_ES5_Class_definitions_are_automatically_in_strict_mode;
        }

        if self
            .file()
            .as_source_file()
            .maybe_external_module_indicator()
            .is_some()
        {
            return &Diagnostics::Function_declarations_are_not_allowed_inside_blocks_in_strict_mode_when_targeting_ES3_or_ES5_Modules_are_automatically_in_strict_mode;
        }

        &Diagnostics::Function_declarations_are_not_allowed_inside_blocks_in_strict_mode_when_targeting_ES3_or_ES5
    }

    pub(super) fn check_strict_mode_function_declaration(
        &self,
        node: &Node, /*FunctionDeclaration*/
    ) {
        if matches!(self.maybe_language_version(), Some(language_version) if language_version < ScriptTarget::ES2015)
        {
            let block_scope_container = self.block_scope_container();
            if !matches!(
                block_scope_container.kind(),
                SyntaxKind::SourceFile | SyntaxKind::ModuleDeclaration
            ) && !is_function_like_or_class_static_block_declaration(Some(
                &*block_scope_container,
            )) {
                let error_span = get_error_span_for_node(&self.file(), node);
                self.file()
                    .as_source_file()
                    .bind_diagnostics_mut()
                    .push(Gc::new(
                        create_file_diagnostic(
                            &self.file(),
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

    pub(super) fn check_strict_mode_numeric_literal(&self, node: &Node /*NumericLiteral*/) {
        if matches!(self.maybe_in_strict_mode(), Some(true)) {
            let node_as_numeric_literal = node.as_numeric_literal();
            if node_as_numeric_literal
                .numeric_literal_flags
                .intersects(TokenFlags::Octal)
            {
                self.file()
                    .as_source_file()
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
        node: &Node, /*PostfixUnaryExpression*/
    ) {
        if matches!(self.maybe_in_strict_mode(), Some(true)) {
            self.check_strict_mode_eval_or_arguments(
                node,
                Some(&*node.as_postfix_unary_expression().operand),
            );
        }
    }

    pub(super) fn check_strict_mode_prefix_unary_expression(
        &self,
        node: &Node, /*PrefixUnaryExpression*/
    ) {
        if matches!(self.maybe_in_strict_mode(), Some(true)) {
            let node_as_prefix_unary_expression = node.as_prefix_unary_expression();
            if matches!(
                node_as_prefix_unary_expression.operator,
                SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
            ) {
                self.check_strict_mode_eval_or_arguments(
                    node,
                    Some(&*node_as_prefix_unary_expression.operand),
                );
            }
        }
    }

    pub(super) fn check_strict_mode_with_statement(&self, node: &Node /*WithStatement*/) {
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
        node: &Node, /*LabeledStatement*/
    ) {
        if matches!(self.maybe_in_strict_mode(), Some(true))
            && get_emit_script_target(&self.options()) >= ScriptTarget::ES2015
        {
            let node_as_labeled_statement = node.as_labeled_statement();
            if is_declaration_statement(&node_as_labeled_statement.statement)
                || is_variable_statement(&node_as_labeled_statement.statement)
            {
                self.error_on_first_token(
                    &node_as_labeled_statement.label,
                    &Diagnostics::A_label_is_not_allowed_here,
                    None,
                );
            }
        }
    }

    pub(super) fn error_on_first_token(
        &self,
        node: &Node,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        let span = get_span_of_token_at_position(&self.file(), node.pos().try_into().unwrap());
        self.file()
            .as_source_file()
            .bind_diagnostics_mut()
            .push(Gc::new(
                create_file_diagnostic(&self.file(), span.start, span.length, message, args).into(),
            ));
    }

    pub(super) fn error_or_suggestion_on_node(
        &self,
        is_error: bool,
        node: &Node,
        message: &DiagnosticMessage,
    ) {
        self.error_or_suggestion_on_range(is_error, node, node, message);
    }

    pub(super) fn error_or_suggestion_on_range(
        &self,
        is_error: bool,
        start_node: &Node,
        end_node: &Node,
        message: &DiagnosticMessage,
    ) {
        self.add_error_or_suggestion_diagnostic(
            is_error,
            BaseTextRange::new(
                get_token_pos_of_node(start_node, Some(&*self.file()), None),
                end_node.end(),
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
                &self.file(),
                range.pos(),
                range.end() - range.pos(),
                message,
                None,
            )
            .into(),
        );
        if is_error {
            self.file()
                .as_source_file()
                .bind_diagnostics_mut()
                .push(diag);
        } else {
            diag.set_category(DiagnosticCategory::Suggestion);
            let file = self.file();
            let mut file_bind_suggestion_diagnostics =
                file.as_source_file().maybe_bind_suggestion_diagnostics();
            if file_bind_suggestion_diagnostics.is_none() {
                *file_bind_suggestion_diagnostics = Some(vec![]);
            }
            append(
                file_bind_suggestion_diagnostics.as_mut().unwrap(),
                Some(diag),
            );
        }
    }

    pub(super) fn bind(&self, node: Option<impl Borrow<Node>>) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        let node = node.borrow();
        set_parent(node, self.maybe_parent());
        let save_in_strict_mode = self.maybe_in_strict_mode();

        self.bind_worker(node);

        if node.kind() > SyntaxKind::LastToken {
            let save_parent = self.maybe_parent();
            self.set_parent(Some(node.node_wrapper()));
            let container_flags = self.get_container_flags(node);
            if container_flags == ContainerFlags::None {
                self.bind_children(node);
            } else {
                self.bind_container(node, container_flags);
            }
            self.set_parent(save_parent);
        } else {
            let save_parent = self.maybe_parent();
            if node.kind() == SyntaxKind::EndOfFileToken {
                self.set_parent(Some(node.node_wrapper()));
            }
            self.bind_jsdoc(node);
            self.set_parent(save_parent);
        }
        self.set_in_strict_mode(save_in_strict_mode);
    }

    pub(super) fn bind_jsdoc(&self, node: &Node) {
        if has_jsdoc_nodes(node) {
            if is_in_js_file(Some(node)) {
                for j in node.maybe_js_doc().unwrap() {
                    self.bind(Some(j));
                }
            } else {
                for j in node.maybe_js_doc().unwrap() {
                    set_parent(&j, Some(node.node_wrapper()));
                    set_parent_recursive(Some(j), false);
                }
            }
        }
    }

    pub(super) fn update_strict_mode_statement_list(
        &self,
        statements: &NodeArray, /*<Statement>*/
    ) {
        if !matches!(self.maybe_in_strict_mode(), Some(true)) {
            for statement in statements {
                if !is_prologue_directive(statement) {
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
        node: &Node, /*ExpressionStatement*/
    ) -> bool {
        let node_text = get_source_text_of_node_from_source_file(
            &self.file(),
            &node.as_expression_statement().expression,
            None,
        );

        matches!(&*node_text, "\"use strict\"" | "'use strict'")
    }
}
