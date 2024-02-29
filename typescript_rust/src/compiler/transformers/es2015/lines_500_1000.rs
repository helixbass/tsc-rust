use std::io;

use id_arena::Id;

use super::{CopyDirection, HierarchyFacts, Jump, TransformES2015};
use crate::{
    add_range, cast_present, concatenate, create_token_range, get_class_extends_heritage_element,
    get_comment_range, get_emit_flags, get_first_constructor_with_body, has_syntactic_modifier,
    id_text, insert_statements_after_standard_prologue, is_binary_expression, is_call_expression,
    is_expression, is_expression_statement, is_identifier_a_non_contextual_keyword, is_statement,
    is_super_call, released, set_emit_flags, single_or_many_node, skip_outer_expressions,
    skip_trivia, try_visit_each_child, try_visit_node, try_visit_nodes, try_visit_parameter_list,
    CoreTransformationContext, EmitFlags, FunctionLikeDeclarationInterface,
    GeneratedIdentifierFlags, GetOrInsertDefault, HasStatementsInterface, InArena, MapOrDefault,
    Matches, ModifierFlags, Node, NodeArray, NodeArrayExt, NodeArrayOrVec, NodeExt, NodeInterface,
    OptionTry, ReadonlyTextRange, ReadonlyTextRangeConcrete, SignatureDeclarationInterface,
    SyntaxKind, TextRange, TransformFlags, VisitResult,
};

impl TransformES2015 {
    pub(super) fn visit_source_file(
        &self,
        node: Id<Node>, /*SourceFile*/
    ) -> io::Result<Id<Node /*SourceFile*/>> {
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::SourceFileExcludes,
            HierarchyFacts::SourceFileIncludes,
        );
        let mut prologue: Vec<Id<Node /*Statement*/>> = Default::default();
        let mut statements: Vec<Id<Node /*Statement*/>> = Default::default();
        self.context.ref_(self).start_lexical_environment();
        let statement_offset = self.factory.ref_(self).try_copy_prologue(
            &node.ref_(self).as_source_file().statements().ref_(self),
            &mut prologue,
            Some(false),
            Some(|node: Id<Node>| self.visitor(node)),
        )?;
        add_range(
            &mut statements,
            Some(
                &try_visit_nodes(
                    released!(node.ref_(self).as_source_file().statements()),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_statement(node, self)),
                    Some(statement_offset),
                    None,
                    self,
                )?
                .ref_(self),
            ),
            None,
            None,
        );
        if let Some(tagged_template_string_declarations) =
            self.maybe_tagged_template_string_declarations().as_ref()
        {
            statements.push(self.factory.ref_(self).create_variable_statement(
                Option::<Id<NodeArray>>::None,
                self.factory.ref_(self).create_variable_declaration_list(
                    tagged_template_string_declarations.clone(),
                    None,
                ),
            ));
        }
        prologue = self
            .factory
            .ref_(self)
            .merge_lexical_environment(
                prologue,
                self.context.ref_(self).end_lexical_environment().as_deref(),
            )
            .as_vec_owned();
        self.insert_capture_this_for_node_if_needed(&mut prologue, node);
        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(self.factory.ref_(self).update_source_file(
            node,
            self.factory
                .ref_(self)
                .create_node_array(Some(concatenate(prologue, statements)), None)
                .set_text_range(
                    Some(&*released!(node.ref_(self).as_source_file().statements()).ref_(self)),
                    self,
                ),
            None,
            None,
            None,
            None,
            None,
        ))
    }

    pub(super) fn visit_switch_statement(
        &self,
        node: Id<Node>, /*SwitchStatement*/
    ) -> io::Result<Id<Node /*SwitchStatement*/>> {
        if let Some(converted_loop_state) = self.maybe_converted_loop_state() {
            let saved_allowed_non_labeled_jumps = converted_loop_state
                .ref_(self)
                .allowed_non_labeled_jumps
                .clone();
            {
                let mut converted_loop_state = converted_loop_state.ref_mut(self);
                converted_loop_state.allowed_non_labeled_jumps = Some(
                    converted_loop_state
                        .allowed_non_labeled_jumps
                        .unwrap_or_default()
                        | Jump::Break,
                );
            }
            let result = try_visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?;
            converted_loop_state.ref_mut(self).allowed_non_labeled_jumps =
                saved_allowed_non_labeled_jumps;
            return Ok(result);
        }
        try_visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }

    pub(super) fn visit_case_block(
        &self,
        node: Id<Node>, /*CaseBlock*/
    ) -> io::Result<Id<Node /*CaseBlock*/>> {
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::BlockScopeExcludes,
            HierarchyFacts::BlockScopeIncludes,
        );
        let updated = try_visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )?;
        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(updated)
    }

    pub(super) fn return_captured_this(&self, node: Id<Node>) -> Id<Node /*ReturnStatement*/> {
        self.factory
            .ref_(self)
            .create_return_statement(Some(self.factory.ref_(self).create_unique_name(
                "_this",
                Some(GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel),
            )))
            .set_original_node(Some(node), self)
    }

    pub(super) fn visit_return_statement(
        &self,
        mut node: Id<Node>, /*ReturnStatement*/
    ) -> io::Result<Id<Node /*Statement*/>> {
        if let Some(converted_loop_state) = self.maybe_converted_loop_state() {
            {
                let mut converted_loop_state = converted_loop_state.ref_mut(self);
                converted_loop_state.non_local_jumps =
                    Some(converted_loop_state.non_local_jumps.unwrap_or_default() | Jump::Return);
            }
            if self.is_return_void_statement_in_constructor_with_captured_super(node) {
                node = self.return_captured_this(node);
            }
            return Ok(self.factory.ref_(self).create_return_statement(Some(
                self.factory.ref_(self).create_object_literal_expression(
                    Some(vec![self.factory.ref_(self).create_property_assignment(
                        self.factory.ref_(self).create_identifier("value"),
                        node.ref_(self)
                            .as_return_statement()
                            .expression
                            .try_map_or_else(
                                || Ok(self.factory.ref_(self).create_void_zero()),
                                |node_expression| {
                                    try_visit_node(
                                        node_expression,
                                        Some(|node: Id<Node>| self.visitor(node)),
                                        Some(|node| is_expression(node, self)),
                                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                    )
                                },
                            )?,
                    )]),
                    None,
                ),
            )));
        } else if self.is_return_void_statement_in_constructor_with_captured_super(node) {
            return Ok(self.return_captured_this(node));
        }
        try_visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }

    pub(super) fn visit_this_keyword(&self, node: Id<Node>) -> Id<Node> {
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
                self.converted_loop_state()
                    .ref_mut(self)
                    .contains_lexical_this = Some(true);
                return node;
            }
            return self
                .converted_loop_state()
                .ref_mut(self)
                .this_name
                .get_or_insert_with(|| self.factory.ref_(self).create_unique_name("this", None))
                .clone();
        }
        node
    }

    pub(super) fn visit_void_expression(
        &self,
        node: Id<Node>, /*VoidExpression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        try_visit_each_child(
            node,
            |node: Id<Node>| self.visitor_with_unused_expression_result(node),
            &*self.context.ref_(self),
            self,
        )
    }

    pub(super) fn visit_identifier(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<Id<Node /*Identifier*/>> {
        if self.maybe_converted_loop_state().is_none() {
            return Ok(node);
        }
        if self.resolver.ref_(self).is_arguments_local_binding(node)? {
            return Ok(self
                .converted_loop_state()
                .ref_mut(self)
                .arguments_name
                .get_or_insert_with(|| {
                    self.factory
                        .ref_(self)
                        .create_unique_name("arguments", None)
                })
                .clone());
        }
        Ok(node)
    }

    pub(super) fn visit_break_or_continue_statement(
        &self,
        node: Id<Node>, /*BreakOrContinueStatement*/
    ) -> io::Result<Id<Node /*Statement*/>> {
        let node_ref = node.ref_(self);
        let node_as_has_label = node_ref.as_has_label();
        if self.maybe_converted_loop_state().is_some() {
            let jump = if node.ref_(self).kind() == SyntaxKind::BreakStatement {
                Jump::Break
            } else {
                Jump::Continue
            };
            let can_use_break_or_continue = matches!(
                (node_as_has_label.maybe_label().as_ref(), self.converted_loop_state().ref_(self).labels.as_ref()),
                (Some(node_label), Some(converted_loop_state_labels)) if converted_loop_state_labels.get(
                    id_text(&node_label.ref_(self))
                ).copied() == Some(true)
            ) || node_as_has_label.maybe_label().is_none()
                && self
                    .converted_loop_state()
                    .ref_(self)
                    .allowed_non_labeled_jumps
                    .unwrap_or_default()
                    .intersects(jump);

            if !can_use_break_or_continue {
                let label_marker: String;
                let label = node_as_has_label.maybe_label();
                match label {
                    None => {
                        if node.ref_(self).kind() == SyntaxKind::BreakStatement {
                            *self
                                .converted_loop_state()
                                .ref_mut(self)
                                .non_local_jumps
                                .get_or_insert_default_() |= Jump::Break;
                            label_marker = "break".to_owned();
                        } else {
                            *self
                                .converted_loop_state()
                                .ref_mut(self)
                                .non_local_jumps
                                .get_or_insert_default_() |= Jump::Continue;
                            label_marker = "continue".to_owned();
                        }
                    }
                    Some(label) => {
                        if node.ref_(self).kind() == SyntaxKind::BreakStatement {
                            label_marker =
                                format!("break-{}", label.ref_(self).as_identifier().escaped_text);
                            self.set_labeled_jump(
                                &mut self.converted_loop_state().ref_mut(self),
                                true,
                                id_text(&label.ref_(self)).to_owned(),
                                label_marker.clone(),
                            );
                        } else {
                            label_marker = format!(
                                "continue-{}",
                                label.ref_(self).as_identifier().escaped_text
                            );
                            self.set_labeled_jump(
                                &mut self.converted_loop_state().ref_mut(self),
                                false,
                                id_text(&label.ref_(self)).to_owned(),
                                label_marker.clone(),
                            );
                        }
                    }
                }
                let mut return_expression =
                    self.factory
                        .ref_(self)
                        .create_string_literal(label_marker, None, None);
                if !self
                    .converted_loop_state()
                    .ref_(self)
                    .loop_out_parameters
                    .is_empty()
                {
                    let converted_loop_state = self.converted_loop_state();
                    let converted_loop_state = converted_loop_state.ref_(self);
                    let out_params = &converted_loop_state.loop_out_parameters;
                    let mut expr: Option<Id<Node>> = None;
                    for (i, out_param) in out_params.iter().enumerate() {
                        let copy_expr =
                            self.copy_out_parameter(out_param, CopyDirection::ToOutParameter);
                        if i == 0 {
                            expr = Some(copy_expr);
                        } else {
                            expr = Some(self.factory.ref_(self).create_binary_expression(
                                expr.unwrap(),
                                SyntaxKind::CommaToken,
                                copy_expr,
                            ));
                        }
                    }
                    return_expression = self.factory.ref_(self).create_binary_expression(
                        expr.unwrap(),
                        SyntaxKind::CommaToken,
                        return_expression,
                    );
                }
                return Ok(self
                    .factory
                    .ref_(self)
                    .create_return_statement(Some(return_expression)));
            }
        }
        try_visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }

    pub(super) fn visit_class_declaration(
        &self,
        node: Id<Node>, /*ClassDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let variable = self
            .factory
            .ref_(self)
            .create_variable_declaration(
                Some(
                    self.factory
                        .ref_(self)
                        .get_local_name(node, Some(true), None),
                ),
                None,
                None,
                Some(self.transform_class_like_declaration_to_expression(node)?),
            )
            .set_original_node(Some(node), self);

        let mut statements: Vec<Id<Node /*Statement*/>> = Default::default();
        let statement = self
            .factory
            .ref_(self)
            .create_variable_statement(
                Option::<Id<NodeArray>>::None,
                self.factory
                    .ref_(self)
                    .create_variable_declaration_list(vec![variable], None),
            )
            .set_original_node(Some(node), self)
            .set_text_range(Some(&*node.ref_(self)), self)
            .start_on_new_line(self);
        statements.push(statement);

        if has_syntactic_modifier(node, ModifierFlags::Export, self) {
            let export_statement = if has_syntactic_modifier(node, ModifierFlags::Default, self) {
                self.factory
                    .ref_(self)
                    .create_export_default(self.factory.ref_(self).get_local_name(node, None, None))
            } else {
                self.factory.ref_(self).create_external_module_export(
                    self.factory.ref_(self).get_local_name(node, None, None),
                )
            }
            .set_original_node(Some(statement), self);
            statements.push(export_statement);
        }

        let emit_flags = get_emit_flags(node, self);
        if !emit_flags.intersects(EmitFlags::HasEndOfDeclarationMarker) {
            statements.push(
                self.factory
                    .ref_(self)
                    .create_end_of_declaration_marker(node),
            );
            set_emit_flags(
                statement,
                emit_flags | EmitFlags::HasEndOfDeclarationMarker,
                self,
            );
        }

        Ok(Some(single_or_many_node(statements)))
    }

    pub(super) fn visit_class_expression(
        &self,
        node: Id<Node>, /*ClassExpression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        self.transform_class_like_declaration_to_expression(node)
    }

    pub(super) fn transform_class_like_declaration_to_expression(
        &self,
        node: Id<Node>, /*ClassExpression | ClassDeclaration*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        if node
            .ref_(self)
            .as_class_like_declaration()
            .maybe_name()
            .is_some()
        {
            self.enable_substitutions_for_block_scoped_bindings();
        }

        let extends_clause_element = get_class_extends_heritage_element(node, self);
        let class_function = self
            .factory
            .ref_(self)
            .create_function_expression(
                Option::<Id<NodeArray>>::None,
                None,
                Option::<Id<Node>>::None,
                Option::<Id<NodeArray>>::None,
                Some(extends_clause_element.as_ref().map_or_default(|_| {
                    vec![self.factory.ref_(self).create_parameter_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        None,
                        Some(self.factory.ref_(self).create_unique_name(
                            "_super",
                            Some(
                                GeneratedIdentifierFlags::Optimistic
                                    | GeneratedIdentifierFlags::FileLevel,
                            ),
                        )),
                        None,
                        None,
                        None,
                    )]
                })),
                None,
                self.transform_class_body(node, extends_clause_element)?,
            )
            .set_emit_flags(
                (get_emit_flags(node, self) & EmitFlags::Indented)
                    | EmitFlags::ReuseTempVariableScope,
                self,
            );

        let inner = self
            .factory
            .ref_(self)
            .create_partially_emitted_expression(class_function, None)
            .set_text_range_end(node.ref_(self).end(), self)
            .set_emit_flags(EmitFlags::NoComments, self);

        let outer = self
            .factory
            .ref_(self)
            .create_partially_emitted_expression(inner, None)
            .set_text_range_end(
                skip_trivia(
                    &self.current_text(),
                    node.ref_(self).pos(),
                    None,
                    None,
                    None,
                ),
                self,
            )
            .set_emit_flags(EmitFlags::NoComments, self);

        Ok(self
            .factory
            .ref_(self)
            .create_parenthesized_expression(self.factory.ref_(self).create_call_expression(
                outer,
                Option::<Id<NodeArray>>::None,
                Some(extends_clause_element.as_ref().try_map_or_default(
                    |extends_clause_element| -> io::Result<_> {
                        Ok(vec![try_visit_node(
                            extends_clause_element
                                .ref_(self)
                                .as_expression_with_type_arguments()
                                .expression,
                            Some(|node: Id<Node>| self.visitor(node)),
                            Some(|node| is_expression(node, self)),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        )?])
                    },
                )?),
            ))
            .add_synthetic_leading_comment(
                SyntaxKind::MultiLineCommentTrivia,
                "* @class ",
                None,
                self,
            ))
    }

    pub(super) fn transform_class_body(
        &self,
        node: Id<Node>, /*ClassExpression | ClassDeclaration*/
        extends_clause_element: Option<Id<Node>>, /*ExpressionWithTypeArguments*/
    ) -> io::Result<Id<Node /*Block*/>> {
        let mut statements: Vec<Id<Node /*Statement*/>> = Default::default();
        let name = self.factory.ref_(self).get_internal_name(node, None, None);
        let constructor_like_name = if is_identifier_a_non_contextual_keyword(&name.ref_(self)) {
            self.factory
                .ref_(self)
                .get_generated_name_for_node(Some(name), None)
        } else {
            name
        };
        self.context.ref_(self).start_lexical_environment();
        self.add_extends_helper_if_needed(&mut statements, node, extends_clause_element);
        self.add_constructor(
            &mut statements,
            node,
            constructor_like_name,
            extends_clause_element,
        )?;
        self.add_class_members(&mut statements, node)?;

        let closing_brace_location = create_token_range(
            skip_trivia(
                &self.current_text(),
                node.ref_(self)
                    .as_class_like_declaration()
                    .members()
                    .ref_(self)
                    .end(),
                None,
                None,
                None,
            ),
            SyntaxKind::CloseBraceToken,
        );

        let outer = self
            .factory
            .ref_(self)
            .create_partially_emitted_expression(constructor_like_name, None)
            .set_text_range_end(closing_brace_location.end(), self)
            .set_emit_flags(EmitFlags::NoComments, self);

        let statement = self
            .factory
            .ref_(self)
            .create_return_statement(Some(outer))
            .set_text_range_pos(closing_brace_location.pos(), self)
            .set_emit_flags(EmitFlags::NoComments | EmitFlags::NoTokenSourceMaps, self);
        statements.push(statement);

        insert_statements_after_standard_prologue(
            &mut statements,
            self.context.ref_(self).end_lexical_environment().as_deref(),
            self,
        );

        Ok(self
            .factory
            .ref_(self)
            .create_block(
                released!(self
                    .factory
                    .ref_(self)
                    .create_node_array(Some(statements), None)
                    .set_text_range(
                        Some(
                            &*released!(node.ref_(self).as_class_like_declaration().members())
                                .ref_(self),
                        ),
                        self,
                    )),
                Some(true),
            )
            .set_emit_flags(EmitFlags::NoComments, self))
    }

    pub(super) fn add_extends_helper_if_needed(
        &self,
        statements: &mut Vec<Id<Node /*Statement*/>>,
        node: Id<Node>, /*ClassExpression | ClassDeclaration*/
        extends_clause_element: Option<Id<Node>>, /*ExpressionWithTypeArguments*/
    ) {
        if let Some(extends_clause_element) = extends_clause_element {
            statements.push(
                self.factory
                    .ref_(self)
                    .create_expression_statement(self.emit_helpers().create_extends_helper(
                        self.factory.ref_(self).get_internal_name(node, None, None),
                    ))
                    .set_text_range(Some(&*extends_clause_element.ref_(self)), self),
            );
        }
    }

    pub(super) fn add_constructor(
        &self,
        statements: &mut Vec<Id<Node /*Statement*/>>,
        node: Id<Node>, /*ClassExpression | ClassDeclaration*/
        name: Id<Node>, /*Identifier*/
        extends_clause_element: Option<Id<Node>>, /*ExpressionWithTypeArguments*/
    ) -> io::Result<()> {
        let saved_converted_loop_state = self.maybe_converted_loop_state();
        self.set_converted_loop_state(None);
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::ConstructorExcludes,
            HierarchyFacts::ConstructorIncludes,
        );
        let constructor = get_first_constructor_with_body(node, self);
        let has_synthesized_super =
            self.has_synthesized_default_super_call(constructor, extends_clause_element.is_some());
        let constructor_function = self
            .factory
            .ref_(self)
            .create_function_declaration(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                None,
                Some(name),
                Option::<Id<NodeArray>>::None,
                self.transform_constructor_parameters(constructor, has_synthesized_super)?,
                None,
                Some(self.transform_constructor_body(
                    constructor,
                    node,
                    extends_clause_element,
                    has_synthesized_super,
                )?),
            )
            .set_text_range(Some(&*constructor.unwrap_or(node).ref_(self)), self);
        if extends_clause_element.is_some() {
            set_emit_flags(constructor_function, EmitFlags::CapturesThis, self);
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
        constructor: Option<Id<Node>>, /*ConstructorDeclaration*/
        has_synthesized_super: bool,
    ) -> io::Result<NodeArrayOrVec> {
        Ok(try_visit_parameter_list(
            constructor
                .filter(|_| !has_synthesized_super)
                .map(|constructor| {
                    constructor
                        .ref_(self)
                        .as_constructor_declaration()
                        .parameters()
                }),
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )?
        .map_or_else(|| vec![].into(), Into::into))
    }

    pub(super) fn create_default_constructor_body(
        &self,
        node: Id<Node>, /*ClassExpression | ClassDeclaration*/
        is_derived_class: bool,
    ) -> Id<Node> {
        let mut statements: Vec<Id<Node /*Statement*/>> = Default::default();
        self.context.ref_(self).resume_lexical_environment();
        statements = self
            .factory
            .ref_(self)
            .merge_lexical_environment(
                statements,
                self.context.ref_(self).end_lexical_environment().as_deref(),
            )
            .as_vec_owned();

        if is_derived_class {
            statements.push(
                self.factory
                    .ref_(self)
                    .create_return_statement(Some(self.create_default_super_call_or_this())),
            );
        }

        let statements_array = self
            .factory
            .ref_(self)
            .create_node_array(Some(statements), None)
            .set_text_range(
                Some(
                    &*node
                        .ref_(self)
                        .as_class_like_declaration()
                        .members()
                        .ref_(self),
                ),
                self,
            );

        self.factory
            .ref_(self)
            .create_block(statements_array, Some(true))
            .set_text_range(Some(&*node.ref_(self)), self)
            .set_emit_flags(EmitFlags::NoComments, self)
    }

    pub(super) fn transform_constructor_body(
        &self,
        constructor: Option<Id<Node>>, /*ConstructorDeclaration & { body: FunctionBody } */
        node: Id<Node>,                /*ClassExpression | ClassDeclaration*/
        extends_clause_element: Option<Id<Node>>, /*ExpressionWithTypeArguments*/
        has_synthesized_super: bool,
    ) -> io::Result<Id<Node>> {
        let is_derived_class = extends_clause_element.matches(|extends_clause_element| {
            skip_outer_expressions(
                extends_clause_element
                    .ref_(self)
                    .as_expression_with_type_arguments()
                    .expression,
                None,
                self,
            )
            .ref_(self)
            .kind()
                != SyntaxKind::NullKeyword
        });

        if constructor.is_none() {
            return Ok(self.create_default_constructor_body(node, is_derived_class));
        }
        let constructor = constructor.unwrap();
        let constructor_ref = constructor.ref_(self);
        let constructor_as_constructor_declaration = constructor_ref.as_constructor_declaration();
        let constructor_body = constructor_as_constructor_declaration.maybe_body().unwrap();
        let constructor_body_ref = constructor_body.ref_(self);
        let constructor_body_as_block = constructor_body_ref.as_block();

        let mut prologue: Vec<Id<Node /*Statement*/>> = Default::default();
        let mut statements: Vec<Id<Node /*Statement*/>> = Default::default();
        self.context.ref_(self).resume_lexical_environment();

        let mut statement_offset = 0;
        if !has_synthesized_super {
            statement_offset = self.factory.ref_(self).copy_standard_prologue(
                &constructor_body_as_block.statements.ref_(self),
                &mut prologue,
                Some(false),
            );
        }
        self.add_default_value_assignments_if_needed(&mut statements, constructor)?;
        self.add_rest_parameter_if_needed(&mut statements, constructor, has_synthesized_super)?;
        if !has_synthesized_super {
            statement_offset = self
                .factory
                .ref_(self)
                .try_copy_custom_prologue(
                    &constructor_body_as_block.statements.ref_(self),
                    &mut statements,
                    Some(statement_offset),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Option::<fn(Id<Node>) -> bool>::None,
                )?
                .unwrap();
        }

        let mut super_call_expression: Option<Id<Node /*Expression*/>> = None;
        if has_synthesized_super {
            super_call_expression = Some(self.create_default_super_call_or_this());
        } else if is_derived_class
            && statement_offset < constructor_body_as_block.statements.ref_(self).len()
        {
            let first_statement =
                &constructor_body_as_block.statements.ref_(self)[statement_offset];
            if is_expression_statement(&first_statement.ref_(self))
                && is_super_call(
                    first_statement
                        .ref_(self)
                        .as_expression_statement()
                        .expression,
                    self,
                )
            {
                super_call_expression = Some(
                    self.visit_immediate_super_call_in_body(
                        first_statement
                            .ref_(self)
                            .as_expression_statement()
                            .expression,
                    )?,
                );
            }
        }

        if super_call_expression.is_some() {
            self.set_hierarchy_facts(Some(
                self.maybe_hierarchy_facts().unwrap_or_default()
                    | HierarchyFacts::ConstructorWithCapturedSuper,
            ));
            statement_offset += 1;
        }

        add_range(
            &mut statements,
            Some(
                &try_visit_nodes(
                    constructor_body_as_block.statements,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_statement(node, self)),
                    None,
                    None,
                    self,
                )?
                .ref_(self),
            ),
            None,
            None,
        );

        prologue = self
            .factory
            .ref_(self)
            .merge_lexical_environment(
                prologue,
                self.context.ref_(self).end_lexical_environment().as_deref(),
            )
            .as_vec_owned();
        prologue = self.insert_capture_new_target_if_needed(prologue, constructor, false);

        if is_derived_class {
            if let Some(super_call_expression) = super_call_expression.clone().filter(|_| {
                statement_offset == constructor_body_as_block.statements.ref_(self).len()
                    && !constructor_body
                        .ref_(self)
                        .transform_flags()
                        .intersects(TransformFlags::ContainsLexicalThis)
            }) {
                let super_call = cast_present(
                    cast_present(&super_call_expression, |node: &&Id<Node>| {
                        is_binary_expression(&node.ref_(self))
                    })
                    .ref_(self)
                    .as_binary_expression()
                    .left,
                    |node: &Id<Node>| is_call_expression(&node.ref_(self)),
                );
                let return_statement = self
                    .factory
                    .ref_(self)
                    .create_return_statement(Some(super_call_expression.clone()))
                    .set_comment_range(
                        &ReadonlyTextRangeConcrete::from(get_comment_range(super_call, self)),
                        self,
                    );
                set_emit_flags(super_call, EmitFlags::NoComments, self);
                statements.push(return_statement);
            } else {
                self.insert_capture_this_for_node(
                    &mut statements,
                    constructor,
                    Some(super_call_expression.unwrap_or_else(|| self.create_actual_this())),
                );

                if !self.is_sufficiently_covered_by_return_statements(constructor_body) {
                    statements.push(self.factory.ref_(self).create_return_statement(Some(
                        self.factory.ref_(self).create_unique_name(
                            "this",
                            Some(
                                GeneratedIdentifierFlags::Optimistic
                                    | GeneratedIdentifierFlags::FileLevel,
                            ),
                        ),
                    )));
                }
            }
        } else {
            self.insert_capture_this_for_node_if_needed(&mut prologue, constructor);
        }

        Ok(self
            .factory
            .ref_(self)
            .create_block(
                self.factory
                    .ref_(self)
                    .create_node_array(Some(concatenate(prologue, statements)), None)
                    .set_text_range(
                        Some(&*constructor_body_as_block.statements.ref_(self)),
                        self,
                    ),
                Some(true),
            )
            .set_text_range(Some(&*constructor_body.ref_(self)), self))
    }
}
