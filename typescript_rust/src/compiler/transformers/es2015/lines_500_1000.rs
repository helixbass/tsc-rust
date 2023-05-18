use std::{borrow::Borrow, io};

use gc::Gc;

use super::{CopyDirection, HierarchyFacts, Jump, TransformES2015};
use crate::{
    add_range, concatenate, create_token_range, get_class_extends_heritage_element, get_emit_flags,
    get_first_constructor_with_body, has_syntactic_modifier, id_text,
    insert_statements_after_standard_prologue, is_expression,
    is_identifier_a_non_contextual_keyword, is_statement, set_emit_flags, single_or_many_node,
    skip_trivia, try_visit_each_child, try_visit_node, try_visit_nodes, try_visit_parameter_list,
    visit_each_child, visit_node, visit_nodes, AsDoubleDeref, EmitFlags, GeneratedIdentifierFlags,
    HasStatementsInterface, MapOrDefault, ModifierFlags, Node, NodeArray, NodeArrayExt,
    NodeArrayOrVec, NodeExt, NodeInterface, NodeWrappered, OptionTry, ReadonlyTextRange,
    SignatureDeclarationInterface, SyntaxKind, TextRange, VisitResult,
};

impl TransformES2015 {
    pub(super) fn visit_source_file(
        &self,
        node: &Node, /*SourceFile*/
    ) -> io::Result<Gc<Node /*SourceFile*/>> {
        let node_as_source_file = node.as_source_file();
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::SourceFileExcludes,
            HierarchyFacts::SourceFileIncludes,
        );
        let mut prologue: Vec<Gc<Node /*Statement*/>> = Default::default();
        let mut statements: Vec<Gc<Node /*Statement*/>> = Default::default();
        self.context.start_lexical_environment();
        let statement_offset = self.factory.try_copy_prologue(
            &node_as_source_file.statements(),
            &mut prologue,
            Some(false),
            Some(|node: &Node| self.visitor(node)),
        )?;
        add_range(
            &mut statements,
            try_visit_nodes(
                Some(&node_as_source_file.statements()),
                Some(|node: &Node| self.visitor(node)),
                Some(is_statement),
                Some(statement_offset),
                None,
            )?
            .as_double_deref(),
            None,
            None,
        );
        if let Some(tagged_template_string_declarations) =
            self.maybe_tagged_template_string_declarations().as_ref()
        {
            statements.push(
                self.factory
                    .create_variable_statement(
                        Option::<Gc<NodeArray>>::None,
                        self.factory
                            .create_variable_declaration_list(
                                tagged_template_string_declarations.clone(),
                                None,
                            )
                            .wrap(),
                    )
                    .wrap(),
            );
        }
        prologue = self
            .factory
            .merge_lexical_environment(prologue, self.context.end_lexical_environment().as_deref())
            .as_vec_owned();
        self.insert_capture_this_for_node_if_needed(&mut prologue, node);
        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(self.factory.update_source_file(
            node,
            self.factory
                .create_node_array(Some(concatenate(prologue, statements)), None)
                .set_text_range(Some(&*node_as_source_file.statements())),
            None,
            None,
            None,
            None,
            None,
        ))
    }

    pub(super) fn visit_switch_statement(
        &self,
        node: &Node, /*SwitchStatement*/
    ) -> io::Result<Gc<Node /*SwitchStatement*/>> {
        if self.maybe_converted_loop_state().is_some() {
            let saved_allowed_non_labeled_jumps = self
                .maybe_converted_loop_state()
                .as_ref()
                .unwrap()
                .allowed_non_labeled_jumps
                .clone();
            {
                let mut converted_loop_state = self.maybe_converted_loop_state_mut();
                let converted_loop_state = converted_loop_state.as_mut().unwrap();
                converted_loop_state.allowed_non_labeled_jumps = Some(
                    converted_loop_state
                        .allowed_non_labeled_jumps
                        .unwrap_or_default()
                        | Jump::Break,
                );
            }
            let result =
                try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?;
            self.maybe_converted_loop_state_mut()
                .as_mut()
                .unwrap()
                .allowed_non_labeled_jumps = saved_allowed_non_labeled_jumps;
            return Ok(result);
        }
        try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn visit_case_block(
        &self,
        node: &Node, /*CaseBlock*/
    ) -> io::Result<Gc<Node /*CaseBlock*/>> {
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::BlockScopeExcludes,
            HierarchyFacts::BlockScopeIncludes,
        );
        let updated =
            try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?;
        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(updated)
    }

    pub(super) fn return_captured_this(&self, node: &Node) -> Gc<Node /*ReturnStatement*/> {
        self.factory
            .create_return_statement(Some(self.factory.create_unique_name(
                "_this",
                Some(GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel),
            )))
            .wrap()
            .set_original_node(Some(node.node_wrapper()))
    }

    pub(super) fn visit_return_statement(
        &self,
        node: &Node, /*ReturnStatement*/
    ) -> io::Result<Gc<Node /*Statement*/>> {
        let mut node = node.node_wrapper();
        if self.maybe_converted_loop_state().is_some() {
            {
                let mut converted_loop_state = self.maybe_converted_loop_state_mut();
                let converted_loop_state = converted_loop_state.as_mut().unwrap();
                converted_loop_state.non_local_jumps =
                    Some(converted_loop_state.non_local_jumps.unwrap_or_default() | Jump::Return);
            }
            if self.is_return_void_statement_in_constructor_with_captured_super(&node) {
                node = self.return_captured_this(&node);
            }
            return Ok(self
                .factory
                .create_return_statement(Some(
                    self.factory
                        .create_object_literal_expression(
                            Some(vec![self
                                .factory
                                .create_property_assignment(
                                    self.factory
                                        .create_identifier(
                                            "value",
                                            Option::<Gc<NodeArray>>::None,
                                            None,
                                        )
                                        .wrap(),
                                    node.as_return_statement()
                                        .expression
                                        .as_ref()
                                        .try_map_or_else(
                                            || Ok(self.factory.create_void_zero()),
                                            |node_expression| -> io::Result<_> {
                                                Ok(try_visit_node(
                                                    Some(&**node_expression),
                                                    Some(|node: &Node| self.visitor(node)),
                                                    Some(is_expression),
                                                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                                )?
                                                .unwrap())
                                            },
                                        )?,
                                )
                                .wrap()]),
                            None,
                        )
                        .wrap(),
                ))
                .wrap());
        } else if self.is_return_void_statement_in_constructor_with_captured_super(&node) {
            return Ok(self.return_captured_this(&node));
        }
        try_visit_each_child(&node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn visit_this_keyword(&self, node: &Node) -> Gc<Node> {
        if self
            .maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::ArrowFunction)
            && !self
                .maybe_hierarchy_facts()
                .unwrap_or_default()
                .intersects(HierarchyFacts::StaticInitializer)
        {
            self.set_hierarchy_facts(Some(
                self.maybe_hierarchy_facts().unwrap_or_default()
                    | HierarchyFacts::CapturedLexicalThis,
            ));
        }
        if self.maybe_converted_loop_state().is_some() {
            if self
                .maybe_hierarchy_facts()
                .unwrap_or_default()
                .intersects(HierarchyFacts::ArrowFunction)
            {
                self.converted_loop_state_mut().contains_lexical_this = Some(true);
                return node.node_wrapper();
            }
            return self
                .converted_loop_state_mut()
                .this_name
                .get_or_insert_with(|| self.factory.create_unique_name("this", None))
                .clone();
        }
        node.node_wrapper()
    }

    pub(super) fn visit_void_expression(
        &self,
        node: &Node, /*VoidExpression*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        try_visit_each_child(
            &node,
            |node: &Node| self.visitor_with_unused_expression_result(node),
            &**self.context,
        )
    }

    pub(super) fn visit_identifier(
        &self,
        node: &Node, /*Identifier*/
    ) -> io::Result<Gc<Node /*Identifier*/>> {
        if self.maybe_converted_loop_state().is_none() {
            return Ok(node.node_wrapper());
        }
        if self.resolver.is_arguments_local_binding(node)? {
            return Ok(self
                .converted_loop_state_mut()
                .arguments_name
                .get_or_insert_with(|| self.factory.create_unique_name("arguments", None))
                .clone());
        }
        Ok(node.node_wrapper())
    }

    pub(super) fn visit_break_or_continue_statement(
        &self,
        node: &Node, /*BreakOrContinueStatement*/
    ) -> io::Result<Gc<Node /*Statement*/>> {
        let node_as_has_label = node.as_has_label();
        if self.maybe_converted_loop_state().is_some() {
            let jump = if node.kind() == SyntaxKind::BreakStatement {
                Jump::Break
            } else {
                Jump::Continue
            };
            let can_use_break_or_continue = matches!(
                (node_as_has_label.maybe_label().as_ref(), self.converted_loop_state().labels.as_ref()),
                (Some(node_label), Some(converted_loop_state_labels)) if converted_loop_state_labels.get(
                    id_text(node_label)
                ).copied() == Some(true)
            ) || node_as_has_label.maybe_label().is_none()
                && self
                    .converted_loop_state()
                    .allowed_non_labeled_jumps
                    .unwrap_or_default()
                    .intersects(jump);

            if !can_use_break_or_continue {
                let label_marker: String;
                let label = node_as_has_label.maybe_label();
                match label.as_ref() {
                    None => {
                        if node.kind() == SyntaxKind::BreakStatement {
                            *self
                                .converted_loop_state_mut()
                                .non_local_jumps
                                .get_or_insert_with(|| Default::default()) |= Jump::Break;
                            label_marker = "break".to_owned();
                        } else {
                            *self
                                .converted_loop_state_mut()
                                .non_local_jumps
                                .get_or_insert_with(|| Default::default()) |= Jump::Continue;
                            label_marker = "continue".to_owned();
                        }
                    }
                    Some(label) => {
                        if node.kind() == SyntaxKind::BreakStatement {
                            label_marker = format!("break-{}", label.as_identifier().escaped_text);
                            self.set_labeled_jump(
                                &mut self.converted_loop_state_mut(),
                                true,
                                id_text(label),
                                &label_marker,
                            );
                        } else {
                            label_marker =
                                format!("continue-{}", label.as_identifier().escaped_text);
                            self.set_labeled_jump(
                                &mut self.converted_loop_state_mut(),
                                false,
                                id_text(label),
                                &label_marker,
                            );
                        }
                    }
                }
                let mut return_expression = self
                    .factory
                    .create_string_literal(label_marker, None, None)
                    .wrap();
                if !self.converted_loop_state().loop_out_parameters.is_empty() {
                    let converted_loop_state = self.converted_loop_state();
                    let out_params = &converted_loop_state.loop_out_parameters;
                    let mut expr: Option<Gc<Node>> = None;
                    for (i, out_param) in out_params.iter().enumerate() {
                        let copy_expr =
                            self.copy_out_parameter(out_param, CopyDirection::ToOutParameter);
                        if i == 0 {
                            expr = Some(copy_expr);
                        } else {
                            expr = Some(
                                self.factory
                                    .create_binary_expression(
                                        expr.unwrap(),
                                        SyntaxKind::CommaToken,
                                        copy_expr,
                                    )
                                    .wrap(),
                            );
                        }
                    }
                    return_expression = self
                        .factory
                        .create_binary_expression(
                            expr.unwrap(),
                            SyntaxKind::CommaToken,
                            return_expression,
                        )
                        .wrap();
                }
                return Ok(self
                    .factory
                    .create_return_statement(Some(return_expression))
                    .wrap());
            }
        }
        try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn visit_class_declaration(
        &self,
        node: &Node, /*ClassDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let variable = self
            .factory
            .create_variable_declaration(
                Some(self.factory.get_local_name(node, Some(true), None)),
                None,
                None,
                Some(self.transform_class_like_declaration_to_expression(node)?),
            )
            .wrap()
            .set_original_node(Some(node.node_wrapper()));

        let mut statements: Vec<Gc<Node /*Statement*/>> = Default::default();
        let statement = self
            .factory
            .create_variable_statement(
                Option::<Gc<NodeArray>>::None,
                self.factory
                    .create_variable_declaration_list(vec![variable], None)
                    .wrap(),
            )
            .wrap()
            .set_original_node(Some(node.node_wrapper()))
            .set_text_range(Some(node))
            .start_on_new_line();
        statements.push(statement.clone());

        if has_syntactic_modifier(node, ModifierFlags::Export) {
            let export_statement = if has_syntactic_modifier(node, ModifierFlags::Default) {
                self.factory
                    .create_export_default(self.factory.get_local_name(node, None, None))
            } else {
                self.factory
                    .create_external_module_export(self.factory.get_local_name(node, None, None))
            }
            .set_original_node(Some(statement.clone()));
            statements.push(export_statement);
        }

        let emit_flags = get_emit_flags(node);
        if !emit_flags.intersects(EmitFlags::HasEndOfDeclarationMarker) {
            statements.push(
                self.factory
                    .create_end_of_declaration_marker(node.node_wrapper()),
            );
            set_emit_flags(
                &*statement,
                emit_flags | EmitFlags::HasEndOfDeclarationMarker,
            );
        }

        Ok(Some(single_or_many_node(statements)))
    }

    pub(super) fn visit_class_expression(
        &self,
        node: &Node, /*ClassExpression*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        self.transform_class_like_declaration_to_expression(node)
    }

    pub(super) fn transform_class_like_declaration_to_expression(
        &self,
        node: &Node, /*ClassExpression | ClassDeclaration*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        let node_as_class_like_declaration = node.as_class_like_declaration();
        if node_as_class_like_declaration.maybe_name().is_some() {
            self.enable_substitutions_for_block_scoped_bindings();
        }

        let extends_clause_element = get_class_extends_heritage_element(node);
        let class_function = self
            .factory
            .create_function_expression(
                Option::<Gc<NodeArray>>::None,
                None,
                Option::<Gc<Node>>::None,
                Option::<Gc<NodeArray>>::None,
                Some(extends_clause_element.as_ref().map_or_default(|_| {
                    vec![self
                        .factory
                        .create_parameter_declaration(
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            None,
                            Some(self.factory.create_unique_name(
                                "_super",
                                Some(
                                    GeneratedIdentifierFlags::Optimistic
                                        | GeneratedIdentifierFlags::FileLevel,
                                ),
                            )),
                            None,
                            None,
                            None,
                        )
                        .wrap()]
                })),
                None,
                self.transform_class_body(node, extends_clause_element.as_deref())?,
            )
            .wrap()
            .set_emit_flags(
                (get_emit_flags(node) & EmitFlags::Indented) | EmitFlags::ReuseTempVariableScope,
            );

        let inner = self
            .factory
            .create_partially_emitted_expression(class_function, None)
            .wrap()
            .set_text_range_end(node.end())
            .set_emit_flags(EmitFlags::NoComments);

        let outer = self
            .factory
            .create_partially_emitted_expression(inner, None)
            .wrap()
            .set_text_range_end(skip_trivia(
                &self.current_text(),
                node.pos(),
                None,
                None,
                None,
            ))
            .set_emit_flags(EmitFlags::NoComments);

        Ok(self
            .factory
            .create_parenthesized_expression(
                self.factory
                    .create_call_expression(
                        outer,
                        Option::<Gc<NodeArray>>::None,
                        Some(extends_clause_element.as_ref().try_map_or_default(
                            |extends_clause_element| -> io::Result<_> {
                                Ok(vec![try_visit_node(
                                    Some(
                                        &*extends_clause_element
                                            .as_expression_with_type_arguments()
                                            .expression,
                                    ),
                                    Some(|node: &Node| self.visitor(node)),
                                    Some(is_expression),
                                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                )?
                                .unwrap()])
                            },
                        )?),
                    )
                    .wrap(),
            )
            .wrap()
            .add_synthetic_leading_comment(SyntaxKind::MultiLineCommentTrivia, "* @class ", None))
    }

    pub(super) fn transform_class_body(
        &self,
        node: &Node, /*ClassExpression | ClassDeclaration*/
        extends_clause_element: Option<impl Borrow<Node>>, /*ExpressionWithTypeArguments*/
    ) -> io::Result<Gc<Node /*Block*/>> {
        let node_as_class_like_declaration = node.as_class_like_declaration();
        let mut statements: Vec<Gc<Node /*Statement*/>> = Default::default();
        let name = self.factory.get_internal_name(node, None, None);
        let constructor_like_name = if is_identifier_a_non_contextual_keyword(&name) {
            self.factory.get_generated_name_for_node(Some(name), None)
        } else {
            name
        };
        self.context.start_lexical_environment();
        let extends_clause_element = extends_clause_element.node_wrappered();
        self.add_extends_helper_if_needed(&mut statements, node, extends_clause_element.as_deref());
        self.add_constructor(
            &mut statements,
            node,
            &constructor_like_name,
            extends_clause_element.as_deref(),
        )?;
        self.add_class_members(&mut statements, node);

        let closing_brace_location = create_token_range(
            skip_trivia(
                &self.current_text(),
                node_as_class_like_declaration.members().end(),
                None,
                None,
                None,
            ),
            SyntaxKind::CloseBraceToken,
        );

        let outer = self
            .factory
            .create_partially_emitted_expression(constructor_like_name, None)
            .wrap()
            .set_text_range_end(closing_brace_location.end())
            .set_emit_flags(EmitFlags::NoComments);

        let statement = self
            .factory
            .create_return_statement(Some(outer))
            .wrap()
            .set_text_range_pos(closing_brace_location.pos())
            .set_emit_flags(EmitFlags::NoComments | EmitFlags::NoTokenSourceMaps);
        statements.push(statement);

        insert_statements_after_standard_prologue(
            &mut statements,
            self.context.end_lexical_environment().as_deref(),
        );

        Ok(self
            .factory
            .create_block(
                self.factory
                    .create_node_array(Some(statements), None)
                    .set_text_range(Some(&*node_as_class_like_declaration.members())),
                Some(true),
            )
            .wrap()
            .set_emit_flags(EmitFlags::NoComments))
    }

    pub(super) fn add_extends_helper_if_needed(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        node: &Node, /*ClassExpression | ClassDeclaration*/
        extends_clause_element: Option<impl Borrow<Node>>, /*ExpressionWithTypeArguments*/
    ) {
        if let Some(extends_clause_element) = extends_clause_element {
            let extends_clause_element: &Node = extends_clause_element.borrow();
            statements.push(
                self.factory
                    .create_expression_statement(
                        self.emit_helpers().create_extends_helper(
                            self.factory.get_internal_name(node, None, None),
                        ),
                    )
                    .wrap()
                    .set_text_range(Some(extends_clause_element)),
            );
        }
    }

    pub(super) fn add_constructor(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        node: &Node, /*ClassExpression | ClassDeclaration*/
        name: &Node, /*Identifier*/
        extends_clause_element: Option<impl Borrow<Node>>, /*ExpressionWithTypeArguments*/
    ) -> io::Result<()> {
        let saved_converted_loop_state = self.maybe_converted_loop_state().clone();
        self.set_converted_loop_state(None);
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::ConstructorExcludes,
            HierarchyFacts::ConstructorIncludes,
        );
        let constructor = get_first_constructor_with_body(node);
        let has_synthesized_super = self.has_synthesized_default_super_call(
            constructor.as_deref(),
            extends_clause_element.is_some(),
        );
        let extends_clause_element = extends_clause_element.node_wrappered();
        let constructor_function = self
            .factory
            .create_function_declaration(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                None,
                Some(name.node_wrapper()),
                Option::<Gc<NodeArray>>::None,
                self.transform_constructor_parameters(
                    constructor.as_deref(),
                    has_synthesized_super,
                )?,
                None,
                Some(self.transform_constructor_body(
                    constructor.as_deref(),
                    node,
                    extends_clause_element.as_deref(),
                    has_synthesized_super,
                )),
            )
            .wrap()
            .set_text_range(Some(constructor.as_deref().unwrap_or(node)));
        if extends_clause_element.is_some() {
            set_emit_flags(&*constructor_function, EmitFlags::CapturesThis);
        }

        statements.push(constructor_function);
        self.exit_subtree(
            ancestor_facts,
            HierarchyFacts::FunctionSubtreeExcludes,
            HierarchyFacts::None,
        );
        self.set_converted_loop_state(saved_converted_loop_state);

        Ok(())
    }

    pub(super) fn transform_constructor_parameters(
        &self,
        constructor: Option<impl Borrow<Node>>, /*ConstructorDeclaration*/
        has_synthesized_super: bool,
    ) -> io::Result<NodeArrayOrVec> {
        let constructor = constructor.node_wrappered();
        Ok(try_visit_parameter_list(
            constructor
                .filter(|_| !has_synthesized_super)
                .map(|constructor| constructor.as_constructor_declaration().parameters())
                .as_deref(),
            |node: &Node| self.visitor(node),
            &**self.context,
        )?
        .map_or_else(|| vec![].into(), Into::into))
    }

    pub(super) fn create_default_constructor_body(
        &self,
        node: &Node, /*ClassExpression | ClassDeclaration*/
        is_derived_class: bool,
    ) -> Gc<Node> {
        let mut statements: Vec<Gc<Node /*Statement*/>> = Default::default();
        self.context.resume_lexical_environment();
        statements = self
            .factory
            .merge_lexical_environment(
                statements,
                self.context.end_lexical_environment().as_deref(),
            )
            .as_vec_owned();

        if is_derived_class {
            statements.push(
                self.factory
                    .create_return_statement(Some(self.create_default_super_call_or_this()))
                    .wrap(),
            );
        }

        let statements_array = self
            .factory
            .create_node_array(Some(statements), None)
            .set_text_range(Some(&*node.as_class_like_declaration().members()));

        self.factory
            .create_block(statements_array, Some(true))
            .wrap()
            .set_text_range(Some(node))
            .set_emit_flags(EmitFlags::NoComments)
    }

    pub(super) fn transform_constructor_body(
        &self,
        constructor: Option<impl Borrow<Node>>, /*ConstructorDeclaration & { body: FunctionBody } */
        node: &Node,                            /*ClassExpression | ClassDeclaration*/
        extends_clause_element: Option<impl Borrow<Node>>, /*ExpressionWithTypeArguments*/
        has_synthesized_super: bool,
    ) -> Gc<Node> {
        unimplemented!()
    }
}
