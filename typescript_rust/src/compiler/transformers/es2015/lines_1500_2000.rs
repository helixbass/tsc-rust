use std::{borrow::Borrow, io};

use gc::Gc;
use id_arena::Id;

use super::{HierarchyFacts, TransformES2015};
use crate::{
    add_range, are_gc_slices_equal, concatenate, create_expression_for_property_name,
    create_member_access_for_property_name, get_all_accessor_declarations, get_comment_range,
    get_emit_flags, get_source_map_range, get_use_define_for_class_fields,
    insert_statement_after_custom_prologue, is_block, is_class_like, is_computed_property_name,
    is_expression, is_hoisted_function, is_hoisted_variable_statement, is_identifier, is_modifier,
    is_private_identifier, is_property_name, is_statement, is_static, move_range_end,
    node_is_synthesized, range_end_is_on_same_line_as_range_start, set_emit_flags,
    set_original_node, set_source_map_range, set_token_source_map_range, start_on_new_line,
    try_maybe_visit_nodes, try_visit_node, try_visit_parameter_list, unescape_leading_underscores,
    AllAccessorDeclarations, AsDoubleDeref, Debug_, EmitFlags, FunctionLikeDeclarationInterface,
    GeneratedIdentifierFlags, Matches, NamedDeclarationInterface, Node, NodeArray, NodeArrayExt,
    NodeExt, NodeInterface, PropertyDescriptorAttributesBuilder, ReadonlyTextRange,
    ReadonlyTextRangeConcrete, SignatureDeclarationInterface, SyntaxKind, TransformFlags,
    VisitResult,
};

impl TransformES2015 {
    pub(super) fn insert_capture_this_for_node_if_needed(
        &self,
        statements: &mut Vec<Id<Node>>, /*Statement*/
        node: Id<Node>,
    ) -> bool {
        if self
            .maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::CapturedLexicalThis)
            && node.kind() != SyntaxKind::ArrowFunction
        {
            self.insert_capture_this_for_node(statements, node, Some(self.factory.create_this()));
            return true;
        }
        false
    }

    pub(super) fn insert_capture_this_for_node(
        &self,
        statements: &mut Vec<Id<Node>>, /*Statement*/
        node: Id<Node>,
        initializer: Option<impl Borrow<Node /*Expression*/>>,
    ) {
        let initializer = initializer.node_wrappered();
        self.enable_substitutions_for_captured_this();
        let capture_this_statement = self
            .factory
            .create_variable_statement(
                Option::<Gc<NodeArray>>::None,
                self.factory.create_variable_declaration_list(
                    vec![self.factory.create_variable_declaration(
                        Some(self.factory.create_unique_name(
                            "this",
                            Some(
                                GeneratedIdentifierFlags::Optimistic
                                    | GeneratedIdentifierFlags::FileLevel,
                            ),
                        )),
                        None,
                        None,
                        initializer,
                    )],
                    None,
                ),
            )
            .set_emit_flags(EmitFlags::NoComments | EmitFlags::CustomPrologue)
            .set_source_map_range(Some(node.into()));
        insert_statement_after_custom_prologue(statements, Some(capture_this_statement));
    }

    pub(super) fn insert_capture_new_target_if_needed(
        &self,
        mut statements: Vec<Id<Node>>, /*Statement*/
        node: Id<Node>,                /*FunctionLikeDeclaration*/
        copy_on_write: bool,
    ) -> Vec<Id<Node>> /*Statement*/ {
        if self
            .maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::NewTarget)
        {
            let new_target: Id<Node /*Expression*/>;
            match node.kind() {
                SyntaxKind::ArrowFunction => {
                    return statements;
                }
                SyntaxKind::MethodDeclaration
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor => {
                    new_target = self.factory.create_void_zero();
                }
                SyntaxKind::Constructor => {
                    new_target = self.factory.create_property_access_expression(
                        self.factory
                            .create_this()
                            .set_emit_flags(EmitFlags::NoSubstitution),
                        "constructor",
                    );
                }
                SyntaxKind::FunctionDeclaration | SyntaxKind::FunctionExpression => {
                    new_target = self.factory.create_conditional_expression(
                        self.factory.create_logical_and(
                            self.factory
                                .create_this()
                                .set_emit_flags(EmitFlags::NoSubstitution),
                            self.factory.create_binary_expression(
                                self.factory
                                    .create_this()
                                    .set_emit_flags(EmitFlags::NoSubstitution),
                                SyntaxKind::InstanceOfKeyword,
                                self.factory.get_local_name(node, None, None),
                            ),
                        ),
                        None,
                        self.factory.create_property_access_expression(
                            self.factory
                                .create_this()
                                .set_emit_flags(EmitFlags::NoSubstitution),
                            "constructor",
                        ),
                        None,
                        self.factory.create_void_zero(),
                    );
                }
                _ => Debug_.fail_bad_syntax_kind(node, None),
            }

            let capture_new_target_statement = self
                .factory
                .create_variable_statement(
                    Option::<Gc<NodeArray>>::None,
                    self.factory.create_variable_declaration_list(
                        vec![self.factory.create_variable_declaration(
                            Some(self.factory.create_unique_name(
                                "_newTarget",
                                Some(
                                    GeneratedIdentifierFlags::Optimistic
                                        | GeneratedIdentifierFlags::FileLevel,
                                ),
                            )),
                            None,
                            None,
                            Some(new_target),
                        )],
                        None,
                    ),
                )
                .set_emit_flags(EmitFlags::NoComments | EmitFlags::CustomPrologue);

            if copy_on_write {
                statements = statements.clone();
            }

            insert_statement_after_custom_prologue(
                &mut statements,
                Some(capture_new_target_statement),
            );
        }

        statements
    }

    pub(super) fn add_class_members(
        &self,
        statements: &mut Vec<Id<Node /*Statement*/>>,
        node: Id<Node>, /*ClassExpression | ClassDeclaration*/
    ) -> io::Result<()> {
        let node_as_class_like_declaration = node.as_class_like_declaration();
        for member in &node_as_class_like_declaration.members() {
            match member.kind() {
                SyntaxKind::SemicolonClassElement => {
                    statements.push(self.transform_semicolon_class_element_to_statement(member));
                }
                SyntaxKind::MethodDeclaration => {
                    statements.push(self.transform_class_method_declaration_to_statement(
                        &self.get_class_member_prefix(node, member),
                        member,
                        node,
                    )?);
                }
                SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                    let accessors = get_all_accessor_declarations(
                        &node_as_class_like_declaration.members(),
                        member,
                        self,
                    );
                    if Gc::ptr_eq(member, &accessors.first_accessor) {
                        statements.push(self.transform_accessors_to_statement(
                            &self.get_class_member_prefix(node, member),
                            &accessors,
                            node,
                        )?);
                    }
                }
                SyntaxKind::Constructor | SyntaxKind::ClassStaticBlockDeclaration => (),
                _ => Debug_.fail_bad_syntax_kind(
                    member,
                    self.maybe_current_source_file()
                        .as_ref()
                        .map(|current_source_file| {
                            current_source_file.as_source_file().file_name().clone()
                        })
                        .as_deref(),
                ),
            }
        }

        Ok(())
    }

    pub(super) fn transform_semicolon_class_element_to_statement(
        &self,
        member: Id<Node>, /*SemicolonClassElement*/
    ) -> Id<Node> {
        self.factory
            .create_empty_statement()
            .set_text_range(Some(member))
    }

    pub(super) fn transform_class_method_declaration_to_statement(
        &self,
        receiver: Id<Node>, /*LeftHandSideExpression*/
        member: Id<Node>,   /*MethodDeclaration*/
        container: Id<Node>,
    ) -> io::Result<Id<Node>> {
        let member_as_method_declaration = member.as_method_declaration();
        let comment_range: ReadonlyTextRangeConcrete = get_comment_range(member).into();
        let source_map_range = get_source_map_range(member);
        let member_function = self.transform_function_like_to_expression(
            member,
            Some(member),
            Option::<Id<Node>>::None,
            Some(container),
        )?;
        let ref property_name = try_visit_node(
            &member_as_method_declaration.name(),
            Some(|node: Id<Node>| self.visitor(node)),
            Some(is_property_name),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;
        let e: Id<Node /*Expression*/>;
        if !is_private_identifier(property_name)
            && get_use_define_for_class_fields(&self.context.get_compiler_options())
        {
            let name = if is_computed_property_name(property_name) {
                property_name.as_computed_property_name().expression.clone()
            } else if is_identifier(property_name) {
                self.factory.create_string_literal(
                    unescape_leading_underscores(&property_name.as_identifier().escaped_text)
                        .to_owned(),
                    None,
                    None,
                )
            } else {
                property_name.clone()
            };
            e = self.factory.create_object_define_property_call(
                receiver.node_wrapper(),
                name,
                self.factory.create_property_descriptor(
                    PropertyDescriptorAttributesBuilder::default()
                        .value(member_function.clone())
                        .enumerable(false)
                        .writable(true)
                        .configurable(true)
                        .build()
                        .unwrap(),
                    None,
                ),
            );
        } else {
            let member_name = create_member_access_for_property_name(
                &self.factory,
                receiver,
                property_name,
                member_as_method_declaration.maybe_name().as_deref(),
            );
            e = self
                .factory
                .create_assignment(member_name, member_function.clone());
        }
        set_emit_flags(&*member_function, EmitFlags::NoComments);
        set_source_map_range(&*member_function, Some(source_map_range));
        Ok(self
            .factory
            .create_expression_statement(e)
            .set_text_range(Some(member))
            .set_original_node(Some(member.node_wrapper()))
            .set_comment_range(&comment_range)
            .set_emit_flags(EmitFlags::NoSourceMap))
    }

    pub(super) fn transform_accessors_to_statement(
        &self,
        receiver: Id<Node>, /*LeftHandSideExpression*/
        accessors: &AllAccessorDeclarations,
        container: Id<Node>,
    ) -> io::Result<Id<Node /*Statement*/>> {
        Ok(self
            .factory
            .create_expression_statement(
                self.transform_accessors_to_expression(receiver, accessors, container, false)?,
            )
            .set_emit_flags(EmitFlags::NoComments)
            .set_source_map_range(Some(get_source_map_range(&accessors.first_accessor))))
    }

    pub(super) fn transform_accessors_to_expression(
        &self,
        receiver: Id<Node>, /*LeftHandSideExpression*/
        accessors: &AllAccessorDeclarations,
        container: Id<Node>,
        starts_on_new_line: bool,
    ) -> io::Result<Id<Node /*Expression*/>> {
        let first_accessor = &accessors.first_accessor;
        let get_accessor = accessors.get_accessor.as_ref();
        let set_accessor = accessors.set_accessor.as_ref();
        let target = self
            .factory
            .clone_node(receiver)
            .set_text_range(Some(receiver))
            .and_set_parent(receiver.maybe_parent())
            .set_emit_flags(EmitFlags::NoComments | EmitFlags::NoTrailingSourceMap)
            .set_source_map_range(
                first_accessor
                    .as_named_declaration()
                    .maybe_name()
                    .as_deref()
                    .map(Into::into),
            );

        let ref visited_accessor_name = try_visit_node(
            &first_accessor.as_named_declaration().name(),
            Some(|node: Id<Node>| self.visitor(node)),
            Some(is_property_name),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;
        if is_private_identifier(visited_accessor_name) {
            Debug_.fail_bad_syntax_kind(
                visited_accessor_name,
                Some("Encountered unhandled private identifier while transforming ES2015."),
            );
        }
        let property_name =
            create_expression_for_property_name(&self.factory, visited_accessor_name)
                .set_emit_flags(EmitFlags::NoComments | EmitFlags::NoLeadingSourceMap)
                .set_source_map_range(
                    first_accessor
                        .as_named_declaration()
                        .maybe_name()
                        .as_deref()
                        .map(Into::into),
                );

        let mut properties: Vec<Id<Node /*ObjectLiteralElementLike*/>> = Default::default();
        if let Some(get_accessor) = get_accessor {
            let getter_function = self
                .transform_function_like_to_expression(
                    get_accessor,
                    Option::<Id<Node>>::None,
                    Option::<Id<Node>>::None,
                    Some(container),
                )?
                .set_source_map_range(Some(get_source_map_range(get_accessor)))
                .set_emit_flags(EmitFlags::NoLeadingComments);
            let getter = self
                .factory
                .create_property_assignment("get", getter_function)
                .set_comment_range(&ReadonlyTextRangeConcrete::from(get_comment_range(
                    get_accessor,
                )));
            properties.push(getter);
        }

        if let Some(set_accessor) = set_accessor {
            let setter_function = self
                .transform_function_like_to_expression(
                    set_accessor,
                    Option::<Id<Node>>::None,
                    Option::<Id<Node>>::None,
                    Some(container),
                )?
                .set_source_map_range(Some(get_source_map_range(set_accessor)))
                .set_emit_flags(EmitFlags::NoLeadingComments);
            let setter = self
                .factory
                .create_property_assignment("set", setter_function)
                .set_comment_range(&ReadonlyTextRangeConcrete::from(get_comment_range(
                    set_accessor,
                )));
            properties.push(setter);
        }

        properties.push(self.factory.create_property_assignment(
            "enumerable",
            if get_accessor.is_some() || set_accessor.is_some() {
                self.factory.create_false()
            } else {
                self.factory.create_true()
            },
        ));
        properties.push(
            self.factory
                .create_property_assignment("configurable", self.factory.create_true()),
        );

        let call = self.factory.create_call_expression(
            self.factory.create_property_access_expression(
                self.factory.create_identifier("Object"),
                "defineProperty",
            ),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                target,
                property_name,
                self.factory
                    .create_object_literal_expression(Some(properties), Some(true)),
            ]),
        );
        if starts_on_new_line {
            start_on_new_line(&*call);
        }

        Ok(call)
    }

    pub(super) fn visit_arrow_function(
        &self,
        node: Id<Node>, /*ArrowFunction*/
    ) -> io::Result<VisitResult> {
        let node_as_arrow_function = node.as_arrow_function();
        if node
            .transform_flags()
            .intersects(TransformFlags::ContainsLexicalThis)
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

        let saved_converted_loop_state = self.maybe_converted_loop_state();
        self.set_converted_loop_state(None);
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::ArrowFunctionExcludes,
            HierarchyFacts::ArrowFunctionIncludes,
        );
        let func = self
            .factory
            .create_function_expression(
                Option::<Gc<NodeArray>>::None,
                None,
                Option::<Id<Node>>::None,
                Option::<Gc<NodeArray>>::None,
                try_visit_parameter_list(
                    Some(&node_as_arrow_function.parameters()),
                    |node: Id<Node>| self.visitor(node),
                    &**self.context,
                )?,
                Option::<Id<Node>>::None,
                self.transform_function_body(node)?,
            )
            .set_text_range(Some(node))
            .set_original_node(Some(node.node_wrapper()))
            .set_emit_flags(EmitFlags::CapturesThis);

        self.exit_subtree(
            ancestor_facts,
            HierarchyFacts::ArrowFunctionSubtreeExcludes,
            HierarchyFacts::None,
        );

        self.set_converted_loop_state(saved_converted_loop_state);
        Ok(Some(func.into()))
    }

    pub(super) fn visit_function_expression(
        &self,
        node: Id<Node>, /*FunctionExpression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_as_function_expression = node.as_function_expression();
        let ancestor_facts = if get_emit_flags(node).intersects(EmitFlags::AsyncFunctionBody) {
            self.enter_subtree(
                HierarchyFacts::AsyncFunctionBodyExcludes,
                HierarchyFacts::AsyncFunctionBodyIncludes,
            )
        } else {
            self.enter_subtree(
                HierarchyFacts::FunctionExcludes,
                HierarchyFacts::FunctionIncludes,
            )
        };
        let saved_converted_loop_state = self.maybe_converted_loop_state();
        self.set_converted_loop_state(None);

        let parameters = try_visit_parameter_list(
            Some(&node_as_function_expression.parameters()),
            |node: Id<Node>| self.visitor(node),
            &**self.context,
        )?
        .unwrap();
        let body = self.transform_function_body(node)?;
        let name = if self
            .maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::NewTarget)
        {
            Some(self.factory.get_local_name(node, None, None))
        } else {
            node_as_function_expression.maybe_name()
        };

        self.exit_subtree(
            ancestor_facts,
            HierarchyFacts::FunctionSubtreeExcludes,
            HierarchyFacts::None,
        );
        self.set_converted_loop_state(saved_converted_loop_state);
        Ok(self.factory.update_function_expression(
            node,
            Option::<Gc<NodeArray>>::None,
            node_as_function_expression.maybe_asterisk_token(),
            name,
            Option::<Gc<NodeArray>>::None,
            parameters,
            None,
            body,
        ))
    }

    pub(super) fn visit_function_declaration(
        &self,
        node: Id<Node>, /*FunctionDeclaration*/
    ) -> io::Result<Id<Node /*FunctionDeclaration*/>> {
        let node_as_function_declaration = node.as_function_declaration();
        let saved_converted_loop_state = self.maybe_converted_loop_state();
        self.set_converted_loop_state(None);
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::FunctionExcludes,
            HierarchyFacts::FunctionIncludes,
        );
        let parameters = try_visit_parameter_list(
            Some(&node_as_function_declaration.parameters()),
            |node: Id<Node>| self.visitor(node),
            &**self.context,
        )?
        .unwrap();
        let body = self.transform_function_body(node)?;
        let name = if self
            .maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::NewTarget)
        {
            Some(self.factory.get_local_name(node, None, None))
        } else {
            node_as_function_declaration.maybe_name()
        };

        self.exit_subtree(
            ancestor_facts,
            HierarchyFacts::FunctionSubtreeExcludes,
            HierarchyFacts::None,
        );
        self.set_converted_loop_state(saved_converted_loop_state);
        Ok(self.factory.update_function_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            try_maybe_visit_nodes(
                node.maybe_modifiers().as_deref(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_modifier),
                None,
                None,
            )?,
            node_as_function_declaration.maybe_asterisk_token(),
            name,
            Option::<Gc<NodeArray>>::None,
            parameters,
            None,
            Some(body),
        ))
    }

    pub(super) fn transform_function_like_to_expression(
        &self,
        node: Id<Node>, /*FunctionLikeDeclaration*/
        location: Option<&impl ReadonlyTextRange>,
        name: Option<Id<Node /*Identifier*/>>,
        container: Option<Id<Node>>,
    ) -> io::Result<Id<Node /*FunctionExpression*/>> {
        let node_as_function_like_declaration = node.as_function_like_declaration();
        let saved_converted_loop_state = self.maybe_converted_loop_state();
        self.set_converted_loop_state(None);
        let container = container.node_wrappered();
        let ancestor_facts =
            if container.matches(|ref container| is_class_like(container)) && !is_static(node, self) {
                self.enter_subtree(
                    HierarchyFacts::FunctionExcludes,
                    HierarchyFacts::FunctionIncludes | HierarchyFacts::NonStaticClassElement,
                )
            } else {
                self.enter_subtree(
                    HierarchyFacts::FunctionExcludes,
                    HierarchyFacts::FunctionIncludes,
                )
            };
        let parameters = try_visit_parameter_list(
            Some(&node_as_function_like_declaration.parameters()),
            |node: Id<Node>| self.visitor(node),
            &**self.context,
        )?
        .unwrap();
        let body = self.transform_function_body(node)?;
        let mut name = name.node_wrappered();
        if self
            .maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::NewTarget)
            && name.is_none()
            && matches!(
                node.kind(),
                SyntaxKind::FunctionDeclaration | SyntaxKind::FunctionExpression
            )
        {
            name = Some(self.factory.get_generated_name_for_node(Some(node), None));
        }

        self.exit_subtree(
            ancestor_facts,
            HierarchyFacts::FunctionSubtreeExcludes,
            HierarchyFacts::None,
        );
        self.set_converted_loop_state(saved_converted_loop_state);
        Ok(self
            .factory
            .create_function_expression(
                Option::<Gc<NodeArray>>::None,
                node_as_function_like_declaration.maybe_asterisk_token(),
                name,
                Option::<Gc<NodeArray>>::None,
                Some(parameters),
                None,
                body,
            )
            .set_text_range(location)
            .set_original_node(Some(node.node_wrapper())))
    }

    pub(super) fn transform_function_body(
        &self,
        node: Id<Node>, /*FunctionLikeDeclaration*/
    ) -> io::Result<Id<Node>> {
        let node_as_function_like_declaration = node.as_function_like_declaration();
        let mut multi_line = false;
        let mut single_line = false;
        let statements_location: Option<ReadonlyTextRangeConcrete /*TextRange*/>;
        let mut close_brace_location: Option<Id<Node /*TextRange*/>> = Default::default();

        let mut prologue: Vec<Id<Node /*Statement*/>> = Default::default();
        let mut statements: Vec<Id<Node /*Statement*/>> = Default::default();
        let ref body = node_as_function_like_declaration.maybe_body().unwrap();
        let mut statement_offset: Option<usize> = Default::default();

        self.context.resume_lexical_environment();
        if is_block(body) {
            let body_as_block = body.as_block();
            statement_offset = Some(self.factory.copy_standard_prologue(
                &body_as_block.statements,
                &mut prologue,
                Some(false),
            ));
            statement_offset = self.factory.try_copy_custom_prologue(
                &body_as_block.statements,
                &mut prologue,
                statement_offset,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_hoisted_function),
            )?;
            statement_offset = self.factory.try_copy_custom_prologue(
                &body_as_block.statements,
                &mut prologue,
                statement_offset,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_hoisted_variable_statement),
            )?;
        }

        multi_line =
            self.add_default_value_assignments_if_needed(&mut statements, node)? || multi_line;
        multi_line = self.add_rest_parameter_if_needed(&mut statements, node, false)? || multi_line;

        if is_block(body) {
            let body_as_block = body.as_block();
            statement_offset = self.factory.try_copy_custom_prologue(
                &body_as_block.statements,
                &mut statements,
                statement_offset,
                Some(|node: Id<Node>| self.visitor(node)),
                Option::<fn(Id<Node>) -> bool>::None,
            )?;

            statements_location = Some((&*body_as_block.statements).into());
            add_range(
                &mut statements,
                try_maybe_visit_nodes(
                    Some(&body_as_block.statements),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(is_statement),
                    statement_offset,
                    None,
                )?
                .as_double_deref(),
                None,
                None,
            );

            if !multi_line && body_as_block.multi_line == Some(true) {
                multi_line = true;
            }
        } else {
            Debug_.assert(node.kind() == SyntaxKind::ArrowFunction, None);

            statements_location = Some(move_range_end(&**body, -1).into());

            let equals_greater_than_token = &node.as_arrow_function().equals_greater_than_token;
            if !node_is_synthesized(&**equals_greater_than_token) && !node_is_synthesized(&**body) {
                if range_end_is_on_same_line_as_range_start(
                    &**equals_greater_than_token,
                    &**body,
                    &self.current_source_file(),
                ) {
                    single_line = true;
                } else {
                    multi_line = true;
                }
            }

            let expression = try_visit_node(
                body,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?;
            let return_statement = self
                .factory
                .create_return_statement(Some(expression))
                .set_text_range(Some(&**body))
                .move_synthetic_comments(body)
                .set_emit_flags(
                    EmitFlags::NoTokenSourceMaps
                        | EmitFlags::NoTrailingSourceMap
                        | EmitFlags::NoTrailingComments,
                );
            statements.push(return_statement);

            close_brace_location = Some(body.clone());
        }

        prologue = self
            .factory
            .merge_lexical_environment(prologue, self.context.end_lexical_environment().as_deref())
            .as_vec_owned();
        prologue = self.insert_capture_new_target_if_needed(prologue, node, false);
        self.insert_capture_this_for_node_if_needed(&mut prologue, node);

        if !prologue.is_empty() {
            multi_line = true;
        }

        statements = concatenate(prologue, statements);
        if is_block(body) && are_gc_slices_equal(&statements, &body.as_block().statements) {
            return Ok(body.clone());
        }

        let block = self
            .factory
            .create_block(
                self.factory
                    .create_node_array(Some(statements), None)
                    .set_text_range(statements_location.as_ref()),
                Some(multi_line),
            )
            .set_text_range(node_as_function_like_declaration.maybe_body().as_deref());
        if !multi_line && single_line {
            set_emit_flags(&*block, EmitFlags::SingleLine);
        }

        if let Some(close_brace_location) = close_brace_location {
            set_token_source_map_range(
                &*block,
                SyntaxKind::CloseBraceToken,
                Some((&*close_brace_location).into()),
            );
        }

        set_original_node(&*block, node_as_function_like_declaration.maybe_body());
        Ok(block)
    }
}
