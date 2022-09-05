#![allow(non_upper_case_globals)]

use std::convert::TryInto;
use std::rc::Rc;

use crate::{
    find_ancestor, for_each, get_source_file_of_node, get_span_of_token_at_position,
    get_text_of_node, is_function_like, is_identifier, DiagnosticMessage, Diagnostics,
    FindAncestorCallbackReturn, HasTypeParametersInterface, Node, NodeFlags, NodeInterface,
    ReadonlyTextRange, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker,
    __String, for_each_key, get_effective_type_annotation_node, get_root_declaration,
    HasInitializerInterface, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn check_with_statement(&self, node: &Node /*WithStatement*/) {
        if !self.check_grammar_statement_in_ambient_context(node) {
            if node.flags().intersects(NodeFlags::AwaitContext) {
                self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::with_statements_are_not_allowed_in_an_async_function_block,
                    None,
                );
            }
        }

        let node_as_with_statement = node.as_with_statement();
        self.check_expression(&node_as_with_statement.expression, None, None);

        let source_file = get_source_file_of_node(Some(node)).unwrap();
        if !self.has_parse_diagnostics(&source_file) {
            let start =
                get_span_of_token_at_position(&source_file, node.pos().try_into().unwrap()).start;
            let end = node_as_with_statement.statement.pos();
            self.grammar_error_at_pos(
                &source_file,
                start,
                end - start,
                &Diagnostics::The_with_statement_is_not_supported_All_symbols_in_a_with_block_will_have_type_any,
                None,
            );
        }
    }

    pub(super) fn check_switch_statement(&self, node: &Node /*SwitchStatement*/) {
        self.check_grammar_statement_in_ambient_context(node);

        let mut first_default_clause: Option<Rc<Node /*CaseOrDefaultClause*/>> = None;
        let mut has_duplicate_default_clause = false;

        let node_as_switch_statement = node.as_switch_statement();
        let expression_type =
            self.check_expression(&node_as_switch_statement.expression, None, None);
        let expression_is_literal = self.is_literal_type(&expression_type);
        for_each(
            &node_as_switch_statement.case_block.as_case_block().clauses,
            |clause: &Rc<Node>, _| -> Option<()> {
                if clause.kind() == SyntaxKind::DefaultClause && !has_duplicate_default_clause {
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

                if self.produce_diagnostics && clause.kind() == SyntaxKind::CaseClause {
                    let clause_as_case_clause = clause.as_case_clause();
                    let mut case_type =
                        self.check_expression(&clause_as_case_clause.expression, None, None);
                    let case_is_literal = self.is_literal_type(&case_type);
                    let mut compared_expression_type = expression_type.clone();
                    if !case_is_literal || !expression_is_literal {
                        case_type = if case_is_literal {
                            self.get_base_type_of_literal_type(&case_type)
                        } else {
                            case_type
                        };
                        compared_expression_type =
                            self.get_base_type_of_literal_type(&expression_type);
                    }
                    if !self.is_type_equality_comparable_to(&compared_expression_type, &case_type) {
                        self.check_type_comparable_to(
                            &case_type,
                            &compared_expression_type,
                            &clause_as_case_clause.expression,
                            None,
                            None,
                        );
                    }
                }
                let clause_as_case_or_default_clause = clause.as_case_or_default_clause();
                for_each(
                    clause_as_case_or_default_clause.statements(),
                    |statement: &Rc<Node>, _| -> Option<()> {
                        self.check_source_element(Some(&**statement));
                        None
                    },
                );
                if self.compiler_options.no_fallthrough_cases_in_switch == Some(true)
                    && matches!(
                        clause_as_case_or_default_clause.maybe_fallthrough_flow_node().as_ref(),
                        Some(clause_fallthrough_flow_node) if self.is_reachable_flow_node(
                            clause_fallthrough_flow_node.clone()
                        )
                    )
                {
                    self.error(
                        Some(&**clause),
                        &Diagnostics::Fallthrough_case_in_switch,
                        None,
                    );
                }
                None
            },
        );
        if node_as_switch_statement.case_block.maybe_locals().is_some() {
            self.register_for_unused_identifiers_check(&node_as_switch_statement.case_block);
        }
    }

    pub(super) fn check_labeled_statement(&self, node: &Node /*LabeledStatement*/) {
        let node_as_labeled_statement = node.as_labeled_statement();
        if !self.check_grammar_statement_in_ambient_context(node) {
            find_ancestor(node.maybe_parent(), |current| {
                if is_function_like(Some(current)) {
                    return FindAncestorCallbackReturn::Quit;
                }
                if current.kind() == SyntaxKind::LabeledStatement
                    && current
                        .as_labeled_statement()
                        .label
                        .as_identifier()
                        .escaped_text
                        == node_as_labeled_statement.label.as_identifier().escaped_text
                {
                    self.grammar_error_on_node(
                        &node_as_labeled_statement.label,
                        &Diagnostics::Duplicate_label_0,
                        Some(vec![get_text_of_node(
                            &node_as_labeled_statement.label,
                            None,
                        )
                        .into_owned()]),
                    );
                    return true.into();
                }
                false.into()
            });
        }

        self.check_source_element(Some(&*node_as_labeled_statement.statement));
    }

    pub(super) fn check_throw_statement(&self, node: &Node /*ThrowStatement*/) {
        let node_as_throw_statement = node.as_throw_statement();
        if !self.check_grammar_statement_in_ambient_context(node) {
            if is_identifier(&node_as_throw_statement.expression)
                && node_as_throw_statement
                    .expression
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
        self.check_expression(&node_as_throw_statement.expression, None, None);
        // }
    }

    pub(super) fn check_try_statement(&self, node: &Node /*TryStatement*/) {
        self.check_grammar_statement_in_ambient_context(node);

        let node_as_try_statement = node.as_try_statement();
        self.check_block(&node_as_try_statement.try_block);
        let catch_clause = node_as_try_statement.catch_clause.as_ref();
        if let Some(catch_clause) = catch_clause {
            let catch_clause_as_catch_clause = catch_clause.as_catch_clause();
            if let Some(catch_clause_variable_declaration) =
                catch_clause_as_catch_clause.variable_declaration.as_ref()
            {
                let declaration = catch_clause_variable_declaration;
                let type_node =
                    get_effective_type_annotation_node(&get_root_declaration(declaration));
                if let Some(type_node) = type_node.as_ref() {
                    let type_ = self.get_type_for_variable_like_declaration(declaration, false);
                    if matches!(
                        type_.as_ref(),
                        Some(type_) if !type_.flags().intersects(TypeFlags::AnyOrUnknown)
                    ) {
                        self.grammar_error_on_first_token(
                            type_node,
                            &Diagnostics::Catch_clause_variable_type_annotation_must_be_any_or_unknown_if_specified,
                            None,
                        );
                    }
                } else if let Some(declaration_initializer) = declaration
                    .as_variable_declaration()
                    .maybe_initializer()
                    .as_ref()
                {
                    self.grammar_error_on_first_token(
                        declaration_initializer,
                        &Diagnostics::Catch_clause_variable_cannot_have_an_initializer,
                        None,
                    );
                } else {
                    let block_locals = catch_clause_as_catch_clause.block.maybe_locals().clone();
                    if let Some(block_locals) = block_locals {
                        let block_locals = (*block_locals).borrow();
                        for_each_key(
                            &*(*catch_clause.maybe_locals().clone().unwrap()).borrow(),
                            |caught_name: &__String| -> Option<()> {
                                let block_local = block_locals.get(caught_name);
                                if let Some(block_local) = block_local {
                                    if let Some(block_local_value_declaration) =
                                        block_local.maybe_value_declaration().as_ref()
                                    {
                                        if block_local
                                            .flags()
                                            .intersects(SymbolFlags::BlockScopedVariable)
                                        {
                                            self.grammar_error_on_node(
                                                block_local_value_declaration,
                                                &Diagnostics::Cannot_redeclare_identifier_0_in_catch_clause,
                                                Some(vec![
                                                    caught_name.clone().into_string()
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

            self.check_block(&catch_clause_as_catch_clause.block);
        }

        if let Some(node_finally_block) = node_as_try_statement.finally_block.as_ref() {
            self.check_block(node_finally_block);
        }
    }

    pub(super) fn check_index_constraints(
        &self,
        type_: &Type,
        symbol: &Symbol,
        is_static_index: Option<bool>,
    ) {
        unimplemented!()
    }

    pub(super) fn check_type_name_is_reserved(
        &self,
        name: &Node, /*Identifier*/
        message: &'static DiagnosticMessage,
    ) {
        unimplemented!()
    }

    pub(super) fn check_class_name_collision_with_object(&self, name: &Node /*Identifier*/) {
        unimplemented!()
    }

    pub(super) fn check_type_parameters(
        &self,
        type_parameter_declarations: Option<&[Rc<Node /*TypeParameterDeclaration*/>]>,
    ) {
        if let Some(type_parameter_declarations) = type_parameter_declarations {
            for node in type_parameter_declarations {
                self.check_type_parameter(&node);
            }
        }
    }

    pub(super) fn check_class_expression(&self, node: &Node /*ClassExpression*/) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_target_symbol(&self, s: &Symbol) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn is_property_without_initializer(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_property_initialized_in_static_blocks(
        &self,
        prop_name: &Node, /*Identifier | PrivateIdentifier*/
        prop_type: &Type,
        static_blocks: &[Rc<Node /*ClassStaticBlockDeclaration*/>],
        start_pos: isize,
        end_pos: isize,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_interface_declaration(&self, node: &Node /*InterfaceDeclaration*/) {
        let node_as_interface_declaration = node.as_interface_declaration();
        self.check_type_parameters(
            node_as_interface_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        for_each(&node_as_interface_declaration.members, |member, _| {
            self.check_source_element(Some(&**member));
            Option::<()>::None
        });
    }

    pub(super) fn check_type_alias_declaration(&self, node: &Node /*TypeAliasDeclaration*/) {
        let node_as_type_alias_declaration = node.as_type_alias_declaration();
        self.check_type_parameters(
            node_as_type_alias_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        if false {
            unimplemented!()
        } else {
            self.check_source_element(Some(&*node_as_type_alias_declaration.type_));
        }
    }

    pub(super) fn check_alias_symbol(
        &self,
        node: &Node, /*ImportEqualsDeclaration | VariableDeclaration | ImportClause | NamespaceImport | ImportSpecifier | ExportSpecifier | NamespaceExport*/
    ) {
        unimplemented!()
    }
}
