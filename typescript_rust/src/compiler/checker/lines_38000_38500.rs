use std::{convert::TryInto, io};

use id_arena::Id;

use crate::{
    try_maybe_for_each, Type, TypeChecker, __String, declaration_name_to_string, find_ancestor,
    for_each_key, get_class_extends_heritage_element, get_declaration_of_kind,
    get_effective_base_type_node, get_effective_constraint_of_type_parameter,
    get_effective_implements_type_nodes, get_effective_type_annotation_node,
    get_effective_type_parameter_declarations, get_name_of_declaration, get_object_flags,
    get_root_declaration, get_source_file_of_node, get_span_of_token_at_position, get_text_of_node,
    has_static_modifier, has_syntactic_modifier, is_class_like, is_entity_name_expression,
    is_function_like, is_identifier, is_optional_chain, is_private_identifier,
    is_private_identifier_class_element_declaration, is_static, length, released, some,
    try_for_each, try_for_each_bool, try_for_each_child, try_some, AsDoubleDeref,
    ClassLikeDeclarationInterface, DiagnosticMessage, Diagnostics, ExternalEmitHelpers,
    FindAncestorCallbackReturn, HasInitializerInterface, HasTypeArgumentsInterface, InArena,
    IndexInfo, InterfaceTypeInterface, ModifierFlags, ModuleKind, NamedDeclarationInterface, Node,
    NodeArray, NodeFlags, NodeInterface, ObjectFlags, OptionInArena, OptionTry, ReadonlyTextRange,
    ScriptTarget, Signature, SignatureFlags, SignatureKind, Symbol, SymbolFlags, SymbolInterface,
    SyntaxKind, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn check_with_statement(
        &self,
        node: Id<Node>, /*WithStatement*/
    ) -> io::Result<()> {
        if !self.check_grammar_statement_in_ambient_context(node) {
            if node.ref_(self).flags().intersects(NodeFlags::AwaitContext) {
                self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::with_statements_are_not_allowed_in_an_async_function_block,
                    None,
                );
            }
        }

        let node_ref = node.ref_(self);
        let node_as_with_statement = node_ref.as_with_statement();
        self.check_expression(node_as_with_statement.expression, None, None)?;

        let source_file = get_source_file_of_node(node, self);
        if !self.has_parse_diagnostics(source_file) {
            let start = get_span_of_token_at_position(
                &source_file.ref_(self),
                node.ref_(self).pos().try_into().unwrap(),
            )
            .start;
            let end = node_as_with_statement.statement.ref_(self).pos();
            self.grammar_error_at_pos(
                source_file,
                start,
                end - start,
                &Diagnostics::The_with_statement_is_not_supported_All_symbols_in_a_with_block_will_have_type_any,
                None,
            );
        }

        Ok(())
    }

    pub(super) fn check_switch_statement(
        &self,
        node: Id<Node>, /*SwitchStatement*/
    ) -> io::Result<()> {
        self.check_grammar_statement_in_ambient_context(node);

        let mut first_default_clause: Option<Id<Node /*CaseOrDefaultClause*/>> = None;
        let mut has_duplicate_default_clause = false;

        let expression_type =
            self.check_expression(node.ref_(self).as_switch_statement().expression, None, None)?;
        let expression_is_literal = self.is_literal_type(expression_type);
        try_for_each(
            &*released!(node
                .ref_(self)
                .as_switch_statement()
                .case_block
                .ref_(self)
                .as_case_block()
                .clauses
                .ref_(self)
                .clone()),
            |&clause: &Id<Node>, _| -> io::Result<Option<()>> {
                if clause.ref_(self).kind() == SyntaxKind::DefaultClause
                    && !has_duplicate_default_clause
                {
                    if first_default_clause.is_none() {
                        first_default_clause = Some(clause.clone());
                    } else {
                        self.grammar_error_on_node(
                            clause,
                            &Diagnostics::A_default_clause_cannot_appear_more_than_once_in_a_switch_statement,
                            None,
                        );
                        has_duplicate_default_clause = true;
                    }
                }

                if self.produce_diagnostics && clause.ref_(self).kind() == SyntaxKind::CaseClause {
                    let mut case_type = self.check_expression(
                        clause.ref_(self).as_case_clause().expression,
                        None,
                        None,
                    )?;
                    let case_is_literal = self.is_literal_type(case_type);
                    let mut compared_expression_type = expression_type.clone();
                    if !case_is_literal || !expression_is_literal {
                        case_type = if case_is_literal {
                            self.get_base_type_of_literal_type(case_type)?
                        } else {
                            case_type
                        };
                        compared_expression_type =
                            self.get_base_type_of_literal_type(expression_type)?;
                    }
                    if !self.is_type_equality_comparable_to(compared_expression_type, case_type)? {
                        self.check_type_comparable_to(
                            case_type,
                            compared_expression_type,
                            released!(clause.ref_(self).as_case_clause().expression),
                            None,
                            None,
                        )?;
                    }
                }
                try_for_each(
                    &*released!(clause.ref_(self).as_case_or_default_clause().statements())
                        .ref_(self),
                    |&statement: &Id<Node>, _| -> io::Result<Option<()>> {
                        self.check_source_element(Some(statement))?;
                        Ok(None)
                    },
                )?;
                if self
                    .compiler_options
                    .ref_(self)
                    .no_fallthrough_cases_in_switch
                    == Some(true)
                    && matches!(
                        clause.ref_(self).as_case_or_default_clause().maybe_fallthrough_flow_node().as_ref(),
                        Some(clause_fallthrough_flow_node) if self.is_reachable_flow_node(
                            clause_fallthrough_flow_node.clone()
                        )?
                    )
                {
                    self.error(Some(clause), &Diagnostics::Fallthrough_case_in_switch, None);
                }
                Ok(None)
            },
        )?;
        if node
            .ref_(self)
            .as_switch_statement()
            .case_block
            .ref_(self)
            .maybe_locals()
            .is_some()
        {
            self.register_for_unused_identifiers_check(
                node.ref_(self).as_switch_statement().case_block,
            );
        }

        Ok(())
    }

    pub(super) fn check_labeled_statement(
        &self,
        node: Id<Node>, /*LabeledStatement*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_labeled_statement = node_ref.as_labeled_statement();
        if !self.check_grammar_statement_in_ambient_context(node) {
            find_ancestor(
                node.ref_(self).maybe_parent(),
                |current| {
                    if is_function_like(Some(&current.ref_(self))) {
                        return FindAncestorCallbackReturn::Quit;
                    }
                    if current.ref_(self).kind() == SyntaxKind::LabeledStatement
                        && current
                            .ref_(self)
                            .as_labeled_statement()
                            .label
                            .ref_(self)
                            .as_identifier()
                            .escaped_text
                            == node_as_labeled_statement
                                .label
                                .ref_(self)
                                .as_identifier()
                                .escaped_text
                    {
                        self.grammar_error_on_node(
                            node_as_labeled_statement.label,
                            &Diagnostics::Duplicate_label_0,
                            Some(vec![get_text_of_node(
                                node_as_labeled_statement.label,
                                None,
                                self,
                            )
                            .into_owned()]),
                        );
                        return true.into();
                    }
                    false.into()
                },
                self,
            );
        }

        self.check_source_element(Some(node_as_labeled_statement.statement))?;

        return Ok(());
    }

    pub(super) fn check_throw_statement(
        &self,
        node: Id<Node>, /*ThrowStatement*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_throw_statement = node_ref.as_throw_statement();
        if !self.check_grammar_statement_in_ambient_context(node) {
            if is_identifier(&node_as_throw_statement.expression.ref_(self))
                && node_as_throw_statement
                    .expression
                    .ref_(self)
                    .as_identifier()
                    .escaped_text
                    .is_empty()
            {
                self.grammar_error_after_first_token(
                    node,
                    &Diagnostics::Line_break_not_permitted_here,
                    None,
                );
            }
        }

        // if (node.expression) {
        self.check_expression(node_as_throw_statement.expression, None, None)?;
        // }

        Ok(())
    }

    pub(super) fn check_try_statement(
        &self,
        node: Id<Node>, /*TryStatement*/
    ) -> io::Result<()> {
        self.check_grammar_statement_in_ambient_context(node);

        self.check_block(released!(node.ref_(self).as_try_statement().try_block))?;
        let catch_clause = node.ref_(self).as_try_statement().catch_clause;
        if let Some(catch_clause) = catch_clause {
            if let Some(catch_clause_variable_declaration) = catch_clause
                .ref_(self)
                .as_catch_clause()
                .variable_declaration
            {
                let declaration = catch_clause_variable_declaration;
                let type_node = get_effective_type_annotation_node(
                    get_root_declaration(declaration, self),
                    self,
                );
                if let Some(type_node) = type_node {
                    let type_ = self.get_type_for_variable_like_declaration(declaration, false)?;
                    if matches!(
                        type_,
                        Some(type_) if !type_.ref_(self).flags().intersects(TypeFlags::AnyOrUnknown)
                    ) {
                        self.grammar_error_on_first_token(
                            type_node,
                            &Diagnostics::Catch_clause_variable_type_annotation_must_be_any_or_unknown_if_specified,
                            None,
                        );
                    }
                } else if let Some(declaration_initializer) = declaration
                    .ref_(self)
                    .as_variable_declaration()
                    .maybe_initializer()
                {
                    self.grammar_error_on_first_token(
                        declaration_initializer,
                        &Diagnostics::Catch_clause_variable_cannot_have_an_initializer,
                        None,
                    );
                } else {
                    let block_locals = catch_clause
                        .ref_(self)
                        .as_catch_clause()
                        .block
                        .ref_(self)
                        .maybe_locals()
                        .clone();
                    if let Some(block_locals) = block_locals {
                        let block_locals = block_locals.ref_(self);
                        for_each_key(
                            &*catch_clause
                                .ref_(self)
                                .maybe_locals()
                                .clone()
                                .unwrap()
                                .ref_(self),
                            |caught_name: &__String| -> Option<()> {
                                let block_local = block_locals.get(caught_name);
                                if let Some(&block_local) = block_local {
                                    if let Some(block_local_value_declaration) =
                                        block_local.ref_(self).maybe_value_declaration()
                                    {
                                        if block_local
                                            .ref_(self)
                                            .flags()
                                            .intersects(SymbolFlags::BlockScopedVariable)
                                        {
                                            self.grammar_error_on_node(
                                                block_local_value_declaration,
                                                &Diagnostics::Cannot_redeclare_identifier_0_in_catch_clause,
                                                Some(vec![
                                                    caught_name.clone()
                                                ])
                                            );
                                        }
                                    }
                                }
                                None
                            },
                        );
                    }
                }
            }

            self.check_block(released!(catch_clause.ref_(self).as_catch_clause().block))?;
        }

        if let Some(node_finally_block) = node.ref_(self).as_try_statement().finally_block {
            self.check_block(node_finally_block)?;
        }

        Ok(())
    }

    pub(super) fn check_index_constraints(
        &self,
        type_: Id<Type>,
        symbol: Id<Symbol>,
        is_static_index: Option<bool>,
    ) -> io::Result<()> {
        let index_infos = self.get_index_infos_of_type(type_)?;
        if index_infos.is_empty() {
            return Ok(());
        }
        for prop in self.get_properties_of_object_type(type_)? {
            if !(is_static_index == Some(true)
                && prop.ref_(self).flags().intersects(SymbolFlags::Prototype))
            {
                self.check_index_constraint_for_property(
                    type_,
                    prop,
                    self.get_literal_type_from_property(
                        prop,
                        TypeFlags::StringOrNumberLiteralOrUnique,
                        Some(true),
                    )?,
                    self.get_non_missing_type_of_symbol(prop)?,
                )?;
            }
        }
        let type_declaration = symbol.ref_(self).maybe_value_declaration();
        if let Some(type_declaration) =
            type_declaration.filter(|type_declaration| is_class_like(&type_declaration.ref_(self)))
        {
            for &member in &*released!(type_declaration
                .ref_(self)
                .as_class_like_declaration()
                .members())
            .ref_(self)
            {
                if !is_static(member, self) && !self.has_bindable_name(member)? {
                    let symbol = self.get_symbol_of_node(member)?.unwrap();
                    self.check_index_constraint_for_property(
                        type_,
                        symbol,
                        self.get_type_of_expression(released!(
                            member
                                .ref_(self)
                                .as_named_declaration()
                                .name()
                                .ref_(self)
                                .as_computed_property_name()
                                .expression
                        ))?,
                        self.get_non_missing_type_of_symbol(symbol)?,
                    )?;
                }
            }
        }
        if index_infos.len() > 1 {
            for &info in &index_infos {
                self.check_index_constraint_for_index_signature(type_, info)?;
            }
        }

        Ok(())
    }

    pub(super) fn check_index_constraint_for_property(
        &self,
        type_: Id<Type>,
        prop: Id<Symbol>,
        prop_name_type: Id<Type>,
        prop_type: Id<Type>,
    ) -> io::Result<()> {
        let declaration = prop.ref_(self).maybe_value_declaration();
        let name = get_name_of_declaration(declaration, self);
        if matches!(
            name,
            Some(name) if is_private_identifier(&name.ref_(self))
        ) {
            return Ok(());
        }
        let index_infos = self.get_applicable_index_infos(type_, prop_name_type)?;
        let interface_declaration =
            if get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::Interface) {
                get_declaration_of_kind(
                    type_.ref_(self).symbol(),
                    SyntaxKind::InterfaceDeclaration,
                    self,
                )
            } else {
                None
            };
        let local_prop_declaration = if matches!(
            declaration,
            Some(declaration) if declaration.ref_(self).kind() == SyntaxKind::BinaryExpression
        ) || matches!(
            name,
            Some(name) if name.ref_(self).kind() == SyntaxKind::ComputedPropertyName
        ) || self.get_parent_of_symbol(prop)?
            == type_.ref_(self).maybe_symbol()
        {
            declaration
        } else {
            None
        };
        for info in &index_infos {
            let local_index_declaration =
                info.ref_(self)
                    .declaration
                    .try_filter(|&info_declaration| -> io::Result<_> {
                        Ok(self.get_parent_of_symbol(
                            self.get_symbol_of_node(info_declaration)?.unwrap(),
                        )? == type_.ref_(self).maybe_symbol())
                    })?;
            let error_node = local_prop_declaration
                .clone()
                .or_else(|| local_index_declaration.clone())
                .try_or_else(|| {
                    interface_declaration
                        .clone()
                        .try_filter(|_| -> io::Result<_> {
                            Ok(!try_some(
                                Some(&self.get_base_types(type_)?),
                                Some(|&base: &Id<Type>| -> io::Result<_> {
                                    Ok(self
                                        .get_property_of_object_type(
                                            base,
                                            prop.ref_(self).escaped_name(),
                                        )?
                                        .is_some()
                                        && self
                                            .get_index_type_of_type_(
                                                base,
                                                info.ref_(self).key_type,
                                            )?
                                            .is_some())
                                }),
                            )?)
                        })
                })?;
            if error_node.is_some()
                && !self.is_type_assignable_to(prop_type, released!(info.ref_(self).type_))?
            {
                self.error(
                    error_node,
                    &Diagnostics::Property_0_of_type_1_is_not_assignable_to_2_index_type_3,
                    Some(vec![
                        self.symbol_to_string_(prop, Option::<Id<Node>>::None, None, None, None)?,
                        self.type_to_string_(prop_type, Option::<Id<Node>>::None, None, None)?,
                        self.type_to_string_(
                            info.ref_(self).key_type,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                        self.type_to_string_(
                            info.ref_(self).type_,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                    ]),
                );
            }
        }

        Ok(())
    }

    pub(super) fn check_index_constraint_for_index_signature(
        &self,
        type_: Id<Type>,
        check_info: Id<IndexInfo>,
    ) -> io::Result<()> {
        let declaration = check_info.ref_(self).declaration;
        let index_infos = self.get_applicable_index_infos(type_, check_info.ref_(self).key_type)?;
        let interface_declaration =
            if get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::Interface) {
                get_declaration_of_kind(
                    type_.ref_(self).symbol(),
                    SyntaxKind::InterfaceDeclaration,
                    self,
                )
            } else {
                None
            };
        let local_check_declaration = declaration.try_filter(|&declaration| -> io::Result<_> {
            Ok(
                self.get_parent_of_symbol(self.get_symbol_of_node(declaration)?.unwrap())?
                    == type_.ref_(self).maybe_symbol(),
            )
        })?;
        for &info in &index_infos {
            if info == check_info {
                continue;
            }
            let local_index_declaration =
                info.ref_(self)
                    .declaration
                    .try_filter(|&info_declaration| -> io::Result<_> {
                        Ok(self.get_parent_of_symbol(
                            self.get_symbol_of_node(info_declaration)?.unwrap(),
                        )? == type_.ref_(self).maybe_symbol())
                    })?;
            let error_node = local_check_declaration
                .clone()
                .or_else(|| local_index_declaration.clone())
                .try_or_else(|| {
                    interface_declaration
                        .clone()
                        .try_filter(|_| -> io::Result<_> {
                            Ok(!try_some(
                                Some(&self.get_base_types(type_)?),
                                Some(|&base: &Id<Type>| -> io::Result<_> {
                                    Ok(self
                                        .get_index_info_of_type_(
                                            base,
                                            check_info.ref_(self).key_type,
                                        )?
                                        .is_some()
                                        && self
                                            .get_index_type_of_type_(
                                                base,
                                                info.ref_(self).key_type,
                                            )?
                                            .is_some())
                                }),
                            )?)
                        })
                })?;
            if error_node.is_some()
                && !self
                    .is_type_assignable_to(check_info.ref_(self).type_, info.ref_(self).type_)?
            {
                self.error(
                    error_node,
                    &Diagnostics::_0_index_type_1_is_not_assignable_to_2_index_type_3,
                    Some(vec![
                        self.type_to_string_(
                            check_info.ref_(self).key_type,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                        self.type_to_string_(
                            check_info.ref_(self).type_,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                        self.type_to_string_(
                            info.ref_(self).key_type,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                        self.type_to_string_(
                            info.ref_(self).type_,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                    ]),
                );
            }
        }

        Ok(())
    }

    pub(super) fn check_type_name_is_reserved(
        &self,
        name: Id<Node>, /*Identifier*/
        message: &'static DiagnosticMessage,
    ) {
        let name_ref = name.ref_(self);
        let name_as_identifier = name_ref.as_identifier();
        match &*name_as_identifier.escaped_text {
            "any" | "unknown" | "never" | "number" | "bigint" | "boolean" | "string" | "symbol"
            | "void" | "object" => {
                self.error(
                    Some(name),
                    message,
                    Some(vec![name_as_identifier.escaped_text.clone()]),
                );
            }
            _ => (),
        }
    }

    pub(super) fn check_class_name_collision_with_object(
        &self,
        name: Id<Node>, /*Identifier*/
    ) {
        if self.language_version >= ScriptTarget::ES5
            && name.ref_(self).as_identifier().escaped_text == "Object"
            && (self.module_kind <= ModuleKind::ES2015
                || get_source_file_of_node(name, self)
                    .ref_(self)
                    .as_source_file()
                    .maybe_implied_node_format()
                    == Some(ModuleKind::CommonJS))
        {
            self.error(
                Some(name),
                &Diagnostics::Class_name_cannot_be_Object_when_targeting_ES5_with_module_0,
                Some(vec![format!("{:?}", self.module_kind)]),
            );
        }
    }

    pub(super) fn check_type_parameters(
        &self,
        type_parameter_declarations: Option<&[Id<Node /*TypeParameterDeclaration*/>]>,
    ) -> io::Result<()> {
        if let Some(type_parameter_declarations) = type_parameter_declarations {
            let mut seen_default = false;
            for i in 0..type_parameter_declarations.len() {
                let node = type_parameter_declarations[i];
                self.check_type_parameter(node)?;

                if self.produce_diagnostics {
                    let node_ref = node.ref_(self);
                    let node_as_type_parameter_declaration =
                        node_ref.as_type_parameter_declaration();
                    if let Some(node_default) = node_as_type_parameter_declaration.default {
                        seen_default = true;
                        self.check_type_parameters_not_referenced(
                            node_default,
                            type_parameter_declarations,
                            i,
                        )?;
                    } else if seen_default {
                        self.error(
                            Some(node),
                            &Diagnostics::Required_type_parameters_may_not_follow_optional_type_parameters,
                            None,
                        );
                    }
                    for j in 0..i {
                        if type_parameter_declarations[j].ref_(self).maybe_symbol()
                            == node.ref_(self).maybe_symbol()
                        {
                            self.error(
                                node_as_type_parameter_declaration.maybe_name(),
                                &Diagnostics::Duplicate_identifier_0,
                                Some(vec![declaration_name_to_string(
                                    node_as_type_parameter_declaration.maybe_name(),
                                    self,
                                )
                                .into_owned()]),
                            );
                        }
                    }
                }
            }
        }

        Ok(())
    }

    pub(super) fn check_type_parameters_not_referenced(
        &self,
        root: Id<Node>, /*TypeNode*/
        type_parameters: &[Id<Node /*TypeParameterDeclaration*/>],
        index: usize,
    ) -> io::Result<()> {
        self.check_type_parameters_not_referenced_visit(index, type_parameters, root)?;

        Ok(())
    }

    pub(super) fn check_type_parameters_not_referenced_visit(
        &self,
        index: usize,
        type_parameters: &[Id<Node /*TypeParameterDeclaration*/>],
        node: Id<Node>,
    ) -> io::Result<()> {
        if node.ref_(self).kind() == SyntaxKind::TypeReference {
            let type_ = self.get_type_from_type_reference(node)?;
            if type_
                .ref_(self)
                .flags()
                .intersects(TypeFlags::TypeParameter)
            {
                for i in index..type_parameters.len() {
                    if type_.ref_(self).maybe_symbol()
                        == self.get_symbol_of_node(type_parameters[i])?
                    {
                        self.error(
                            Some(node),
                            &Diagnostics::Type_parameter_defaults_can_only_reference_previously_declared_type_parameters,
                            None,
                        );
                    }
                }
            }
        }
        try_for_each_child(
            node,
            |child| self.check_type_parameters_not_referenced_visit(index, type_parameters, child),
            Option::<fn(Id<NodeArray>) -> io::Result<()>>::None,
            self,
        )?;

        Ok(())
    }

    pub(super) fn check_type_parameter_lists_identical(
        &self,
        symbol: Id<Symbol>,
    ) -> io::Result<()> {
        if matches!(
            symbol.ref_(self).maybe_declarations().as_ref(),
            Some(symbol_declarations) if symbol_declarations.len() == 1
        ) {
            return Ok(());
        }

        let links = self.get_symbol_links(symbol);
        if links.ref_(self).type_parameters_checked != Some(true) {
            links.ref_mut(self).type_parameters_checked = Some(true);
            let declarations = self.get_class_or_interface_declarations_of_symbol(symbol);
            if match declarations.as_ref() {
                None => true,
                Some(declarations) => declarations.len() <= 1,
            } {
                return Ok(());
            }
            let declarations = declarations.unwrap();

            let type_ = self.get_declared_type_of_symbol(symbol)?;
            if !self.are_type_parameters_identical(
                &declarations,
                type_
                    .ref_(self)
                    .as_interface_type()
                    .maybe_local_type_parameters(),
            )? {
                let name =
                    self.symbol_to_string_(symbol, Option::<Id<Node>>::None, None, None, None)?;
                for declaration in &declarations {
                    self.error(
                        declaration.ref_(self).as_named_declaration().maybe_name(),
                        &Diagnostics::All_declarations_of_0_must_have_identical_type_parameters,
                        Some(vec![name.clone()]),
                    );
                }
            }
        }

        Ok(())
    }

    pub(super) fn are_type_parameters_identical(
        &self,
        declarations: &[Id<Node /*ClassDeclaration | InterfaceDeclaration*/>],
        target_parameters: Option<&[Id<Type /*TypeParameter*/>]>,
    ) -> io::Result<bool> {
        let max_type_argument_count = length(target_parameters);
        let min_type_argument_count = self.get_min_type_argument_count(target_parameters);

        for &declaration in declarations {
            let source_parameters = get_effective_type_parameter_declarations(declaration, self);
            let num_type_parameters = source_parameters.len();
            if num_type_parameters < min_type_argument_count
                || num_type_parameters > max_type_argument_count
            {
                return Ok(false);
            }

            for i in 0..num_type_parameters {
                let target_parameters = target_parameters.unwrap();
                let source = source_parameters[i];
                let target = target_parameters[i];

                let source_ref = source.ref_(self);
                let source_as_type_parameter_declaration =
                    source_ref.as_type_parameter_declaration();
                if &source_as_type_parameter_declaration
                    .name()
                    .ref_(self)
                    .as_identifier()
                    .escaped_text
                    != target.ref_(self).symbol().ref_(self).escaped_name()
                {
                    return Ok(false);
                }

                let constraint = get_effective_constraint_of_type_parameter(source, self);
                let source_constraint =
                    constraint.try_map(|constraint| self.get_type_from_type_node_(constraint))?;
                let target_constraint = self.get_constraint_of_type_parameter(target)?;
                if matches!(
                    (source_constraint, target_constraint),
                    (Some(source_constraint), Some(target_constraint)) if !self.is_type_identical_to(
                        source_constraint,
                        target_constraint
                    )?
                ) {
                    return Ok(false);
                }

                let source_default = source_as_type_parameter_declaration
                    .default
                    .try_map(|source_default| self.get_type_from_type_node_(source_default))?;
                let target_default = self.get_default_from_type_parameter_(target)?;
                if matches!(
                    (source_default, target_default),
                    (Some(source_default), Some(target_default)) if !self.is_type_identical_to(
                        source_default, target_default
                    )?
                ) {
                    return Ok(false);
                }
            }
        }

        Ok(true)
    }

    pub(super) fn check_class_expression(
        &self,
        node: Id<Node>, /*ClassExpression*/
    ) -> io::Result<Id<Type>> {
        self.check_class_like_declaration(node)?;
        self.check_node_deferred(node);
        self.get_type_of_symbol(self.get_symbol_of_node(node)?.unwrap())
    }

    pub(super) fn check_class_expression_deferred(
        &self,
        node: Id<Node>, /*ClassExpression*/
    ) -> io::Result<()> {
        try_for_each(
            &*released!(node
                .ref_(self)
                .as_class_expression()
                .members()
                .ref_(self)
                .clone()),
            |&member: &Id<Node>, _| -> io::Result<Option<()>> {
                self.check_source_element(Some(member))?;
                Ok(None)
            },
        )?;
        self.register_for_unused_identifiers_check(node);

        Ok(())
    }

    pub(super) fn check_class_declaration(
        &self,
        node: Id<Node>, /*ClassDeclaration*/
    ) -> io::Result<()> {
        if some(
            node.ref_(self)
                .maybe_decorators()
                .refed(self)
                .as_double_deref(),
            Option::<fn(&Id<Node>) -> bool>::None,
        ) && some(
            Some(&*node.ref_(self).as_class_declaration().members().ref_(self)),
            Some(|&p: &Id<Node>| {
                has_static_modifier(p, self)
                    && is_private_identifier_class_element_declaration(p, self)
            }),
        ) {
            self.grammar_error_on_node(
                node.ref_(self).maybe_decorators().as_ref().unwrap().ref_(self)[0],
                &Diagnostics::Class_decorators_can_t_be_used_with_static_private_identifier_Consider_removing_the_experimental_decorator,
                None,
            );
        }
        if node
            .ref_(self)
            .as_class_declaration()
            .maybe_name()
            .is_none()
            && !has_syntactic_modifier(node, ModifierFlags::Default, self)
        {
            self.grammar_error_on_first_token(
                node,
                &Diagnostics::A_class_declaration_without_the_default_modifier_must_have_a_name,
                None,
            );
        }
        self.check_class_like_declaration(node)?;
        try_for_each(
            &*released!(node
                .ref_(self)
                .as_class_declaration()
                .members()
                .ref_(self)
                .clone()),
            |&member: &Id<Node>, _| -> io::Result<Option<()>> {
                self.check_source_element(Some(member))?;
                Ok(None)
            },
        )?;

        self.register_for_unused_identifiers_check(node);

        Ok(())
    }

    pub(super) fn check_class_like_declaration(
        &self,
        node: Id<Node>, /*ClassLikeDeclaration*/
    ) -> io::Result<()> {
        self.check_grammar_class_like_declaration(node);
        self.check_decorators(node)?;
        self.check_collisions_for_declaration_name(
            node,
            node.ref_(self).as_class_like_declaration().maybe_name(),
        );
        self.check_type_parameters(Some(&get_effective_type_parameter_declarations(node, self)))?;
        self.check_exports_on_merged_declarations(node)?;
        let symbol = self.get_symbol_of_node(node)?.unwrap();
        let type_ = self.get_declared_type_of_symbol(symbol)?;
        let type_with_this = self.get_type_with_this_argument(type_, None, None)?;
        let static_type = self.get_type_of_symbol(symbol)?;
        self.check_type_parameter_lists_identical(symbol)?;
        self.check_function_or_constructor_symbol(symbol)?;
        self.check_class_for_duplicate_declarations(node);

        let node_in_ambient_context = node.ref_(self).flags().intersects(NodeFlags::Ambient);
        if !node_in_ambient_context {
            self.check_class_for_static_property_name_conflicts(node)?;
        }

        let base_type_node = get_effective_base_type_node(node, self);
        if let Some(base_type_node) = base_type_node {
            try_maybe_for_each(
                base_type_node
                    .ref_(self)
                    .as_expression_with_type_arguments()
                    .maybe_type_arguments()
                    .refed(self)
                    .as_deref(),
                |&type_argument: &Id<Node>, _| -> io::Result<Option<()>> {
                    self.check_source_element(Some(type_argument))?;
                    Ok(None)
                },
            )?;
            if self.language_version < ScriptTarget::ES2015 {
                self.check_external_emit_helpers(
                    base_type_node.ref_(self).parent(),
                    ExternalEmitHelpers::Extends,
                )?;
            }
            let extends_node = get_class_extends_heritage_element(node, self);
            if let Some(extends_node) =
                extends_node.filter(|&extends_node| extends_node != base_type_node)
            {
                self.check_expression(
                    extends_node
                        .ref_(self)
                        .as_expression_with_type_arguments()
                        .expression,
                    None,
                    None,
                )?;
            }

            let base_types = self.get_base_types(type_)?;
            if !base_types.is_empty() && self.produce_diagnostics {
                let base_type = base_types[0];
                let base_constructor_type = self.get_base_constructor_type_of_class(type_)?;
                let static_base_type = self.get_apparent_type(base_constructor_type)?;
                self.check_base_type_accessibility(static_base_type, base_type_node)?;
                self.check_source_element(Some(
                    base_type_node
                        .ref_(self)
                        .as_expression_with_type_arguments()
                        .expression,
                ))?;
                if some(
                    base_type_node
                        .ref_(self)
                        .as_expression_with_type_arguments()
                        .maybe_type_arguments()
                        .refed(self)
                        .as_double_deref(),
                    Option::<fn(&Id<Node>) -> bool>::None,
                ) {
                    try_maybe_for_each(
                        base_type_node
                            .ref_(self)
                            .as_expression_with_type_arguments()
                            .maybe_type_arguments()
                            .refed(self)
                            .as_deref(),
                        |&type_argument: &Id<Node>, _| -> io::Result<Option<()>> {
                            self.check_source_element(Some(type_argument))?;
                            Ok(None)
                        },
                    )?;
                    for constructor in &self.get_constructors_for_type_arguments(
                        static_base_type,
                        released!(base_type_node
                            .ref_(self)
                            .as_expression_with_type_arguments()
                            .maybe_type_arguments())
                        .refed(self)
                        .as_double_deref(),
                        base_type_node,
                    )? {
                        if !self.check_type_argument_constraints(
                            base_type_node,
                            constructor
                                .ref_(self)
                                .maybe_type_parameters()
                                .as_ref()
                                .unwrap(),
                        )? {
                            break;
                        }
                    }
                }
                let base_with_this = self.get_type_with_this_argument(
                    base_type,
                    {
                        let this_type = type_.ref_(self).as_interface_type().maybe_this_type();
                        this_type
                    },
                    None,
                )?;
                if !self.check_type_assignable_to(
                    type_with_this,
                    base_with_this,
                    Option::<Id<Node>>::None,
                    None,
                    None,
                    None,
                )? {
                    self.issue_member_specific_error(
                        node,
                        type_with_this,
                        base_with_this,
                        &Diagnostics::Class_0_incorrectly_extends_base_class_1,
                    )?;
                } else {
                    self.check_type_assignable_to(
                        static_type,
                        self.get_type_without_signatures(static_base_type)?,
                        released!(Some(node.ref_(self).as_class_like_declaration().maybe_name().unwrap_or(node))),
                        Some(&Diagnostics::Class_static_side_0_incorrectly_extends_base_class_static_side_1),
                        None, None,
                    )?;
                }
                if base_constructor_type
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::TypeVariable)
                {
                    if !self.is_mixin_constructor_type(static_type)? {
                        self.error(
                            Some(node.ref_(self).as_class_like_declaration().maybe_name().unwrap_or(node)),
                            &Diagnostics::A_mixin_class_must_have_a_constructor_with_a_single_rest_parameter_of_type_any,
                            None,
                        );
                    } else {
                        let construct_signatures = self.get_signatures_of_type(
                            base_constructor_type,
                            SignatureKind::Construct,
                        )?;
                        if construct_signatures.iter().any(|signature| {
                            signature
                                .ref_(self)
                                .flags
                                .intersects(SignatureFlags::Abstract)
                        }) && !has_syntactic_modifier(node, ModifierFlags::Abstract, self)
                        {
                            self.error(
                                Some(node.ref_(self).as_class_like_declaration().maybe_name().unwrap_or(node)),
                                &Diagnostics::A_mixin_class_that_extends_from_a_type_variable_containing_an_abstract_construct_signature_must_also_be_declared_abstract,
                                None,
                            );
                        }
                    }
                }

                if !matches!(
                    static_base_type.ref_(self).maybe_symbol(),
                    Some(static_base_type_symbol) if static_base_type_symbol.ref_(self).flags().intersects(SymbolFlags::Class)
                ) && !base_constructor_type
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::TypeVariable)
                {
                    let constructors = self.get_instantiated_constructors_for_type_arguments(
                        static_base_type,
                        base_type_node
                            .ref_(self)
                            .as_expression_with_type_arguments()
                            .maybe_type_arguments()
                            .refed(self)
                            .as_double_deref(),
                        base_type_node,
                    )?;
                    if try_for_each_bool(
                        &constructors,
                        |sig: &Id<Signature>, _| -> io::Result<_> {
                            Ok(!self.is_js_constructor(sig.ref_(self).declaration)?
                                && !self.is_type_identical_to(
                                    self.get_return_type_of_signature(sig.clone())?,
                                    base_type,
                                )?)
                        },
                    )? {
                        self.error(
                            Some(
                                base_type_node
                                    .ref_(self)
                                    .as_expression_with_type_arguments()
                                    .expression,
                            ),
                            &Diagnostics::Base_constructors_must_all_have_the_same_return_type,
                            None,
                        );
                    }
                }
                self.check_kinds_of_property_member_overrides(type_, base_type)?;
            }
        }

        self.check_members_for_override_modifier(node, type_, type_with_this, static_type)?;

        let implemented_type_nodes = get_effective_implements_type_nodes(node, self);
        if let Some(implemented_type_nodes) = implemented_type_nodes.as_ref() {
            for &type_ref_node in implemented_type_nodes {
                if !is_entity_name_expression(
                    type_ref_node
                        .ref_(self)
                        .as_expression_with_type_arguments()
                        .expression,
                    self,
                ) || is_optional_chain(
                    &type_ref_node
                        .ref_(self)
                        .as_expression_with_type_arguments()
                        .expression
                        .ref_(self),
                ) {
                    self.error(
                        Some(type_ref_node.ref_(self).as_expression_with_type_arguments().expression),
                        &Diagnostics::A_class_can_only_implement_an_identifier_Slashqualified_name_with_optional_type_arguments,
                        None,
                    );
                }
                self.check_type_reference_node(type_ref_node)?;
                if self.produce_diagnostics {
                    let t = self.get_reduced_type(self.get_type_from_type_node_(type_ref_node)?)?;
                    if !self.is_error_type(t) {
                        if self.is_valid_base_type(t)? {
                            let generic_diag = if matches!(
                                t.ref_(self).maybe_symbol(),
                                Some(t_symbol) if t_symbol.ref_(self).flags().intersects(SymbolFlags::Class)
                            ) {
                                &*Diagnostics::Class_0_incorrectly_implements_class_1_Did_you_mean_to_extend_1_and_inherit_its_members_as_a_subclass
                            } else {
                                &*Diagnostics::Class_0_incorrectly_implements_interface_1
                            };
                            let base_with_this = self.get_type_with_this_argument(
                                t,
                                released!(type_.ref_(self).as_interface_type().maybe_this_type()),
                                None,
                            )?;
                            if !self.check_type_assignable_to(
                                type_with_this,
                                base_with_this,
                                Option::<Id<Node>>::None,
                                None,
                                None,
                                None,
                            )? {
                                self.issue_member_specific_error(
                                    node,
                                    type_with_this,
                                    base_with_this,
                                    generic_diag,
                                )?;
                            }
                        } else {
                            self.error(
                                Some(type_ref_node),
                                &Diagnostics::A_class_can_only_implement_an_object_type_or_intersection_of_object_types_with_statically_known_members,
                                None,
                            );
                        }
                    }
                }
            }
        }

        if self.produce_diagnostics {
            self.check_index_constraints(type_, symbol, None)?;
            self.check_index_constraints(static_type, symbol, Some(true))?;
            self.check_type_for_duplicate_index_signatures(node)?;
            self.check_property_initialization(node)?;
        }

        Ok(())
    }
}
