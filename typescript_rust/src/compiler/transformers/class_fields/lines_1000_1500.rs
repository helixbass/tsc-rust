use std::borrow::Borrow;

use gc::Gc;
use itertools::Itertools;

use super::{
    create_private_instance_field_initializer, create_private_instance_method_initializer,
    create_private_static_field_initializer, ClassFacts, ClassPropertySubstitutionFlags,
    PrivateIdentifierInfoInterface, TransformClassFields,
};
use crate::{
    get_static_properties_and_class_static_block, InterfaceOrClassLikeDeclarationInterface,
    Matches, NamedDeclarationInterface, Node, NodeInterface, NonEmpty, VisitResult, _d,
    add_emit_flags, add_prologue_directives_and_initial_super_call, add_range, continue_if_none,
    create_member_access_for_property_name, filter, find_index, get_effective_base_type_node,
    get_emit_flags, get_first_constructor_with_body, get_original_node, get_original_node_id,
    get_properties, has_static_modifier, has_syntactic_modifier, is_class_element,
    is_class_static_block_declaration, is_computed_property_name, is_constructor_declaration,
    is_decorator, is_expression, is_heritage_clause, is_identifier, is_initialized_property,
    is_parameter_property_declaration, is_private_identifier,
    is_private_identifier_class_element_declaration, is_simple_inlineable_expression, is_statement,
    is_static, maybe_map, maybe_visit_node, maybe_visit_nodes, move_range_past_modifiers,
    set_emit_flags, set_original_node, skip_outer_expressions, unescape_leading_underscores,
    visit_each_child, visit_function_body, visit_nodes, visit_parameter_list, Debug_, EmitFlags,
    FunctionLikeDeclarationInterface, GeneratedIdentifierFlags, HasInitializerInterface,
    ModifierFlags, NodeArray, NodeArrayExt, NodeArrayOrVec, NodeCheckFlags, NodeExt, NodeWrappered,
    PrivateIdentifierKind, PropertyDescriptorAttributesBuilder, ScriptTarget, SyntaxKind,
};

impl TransformClassFields {
    pub(super) fn visit_class_declaration(
        &self,
        node: &Node, /*ClassDeclaration*/
    ) -> VisitResult {
        let node_as_class_declaration = node.as_class_declaration();
        let facts = self.get_class_facts(node);
        if facts != ClassFacts::None {
            self.get_class_lexical_environment().borrow_mut().facts = facts;
        }
        if facts.intersects(ClassFacts::NeedsSubstitutionForThisInClassStaticField) {
            self.enable_substitution_for_class_static_this_or_super_reference();
        }

        let static_properties = get_static_properties_and_class_static_block(node);

        let mut pending_class_reference_assignment: Option<Gc<Node /*BinaryExpression*/>> = _d();
        if facts.intersects(ClassFacts::NeedsClassConstructorReference) {
            let temp = self.factory.create_temp_variable(
                Some(|node: &Node| {
                    self.context.hoist_variable_declaration(node);
                }),
                Some(true),
            );
            self.get_class_lexical_environment()
                .borrow_mut()
                .class_constructor = Some(self.factory.clone_node(&temp));
            pending_class_reference_assignment = Some(
                self.factory
                    .create_assignment(temp, self.factory.get_internal_name(node, None, None))
                    .wrap(),
            );
        }

        let extends_clause_element = get_effective_base_type_node(node);
        let is_derived_class = extends_clause_element.matches(|extends_clause_element| {
            skip_outer_expressions(
                &extends_clause_element
                    .as_expression_with_type_arguments()
                    .expression,
                None,
            )
            .kind()
                != SyntaxKind::NullKeyword
        });

        let mut statements: Vec<Gc<Node /*Statement*/>> =
            vec![self.factory.update_class_declaration(
                node,
                Option::<Gc<NodeArray>>::None,
                node.maybe_modifiers(),
                node_as_class_declaration.maybe_name(),
                Option::<Gc<NodeArray>>::None,
                maybe_visit_nodes(
                    node_as_class_declaration
                        .maybe_heritage_clauses()
                        .as_deref(),
                    Some(|node: &Node| self.heritage_clause_visitor(node)),
                    Some(is_heritage_clause),
                    None,
                    None,
                ),
                self.transform_class_members(node, is_derived_class),
            )];

        if let Some(pending_class_reference_assignment) = pending_class_reference_assignment {
            self.get_pending_expressions()
                .insert(0, pending_class_reference_assignment);
        }

        if let Some(pending_expressions) = self.maybe_pending_expressions().as_ref().non_empty() {
            statements.push(
                self.factory
                    .create_expression_statement(
                        self.factory.inline_expressions(pending_expressions),
                    )
                    .wrap(),
            );
        }

        if !static_properties.is_empty() {
            self.add_property_or_class_static_block_statements(
                &mut statements,
                &static_properties,
                &self.factory.get_internal_name(node, None, None),
            );
        }

        Some(statements.into())
    }

    pub(super) fn visit_class_expression(
        &self,
        node: &Node, /*ClassExpression*/
    ) -> Gc<Node /*Expression*/> {
        let node_as_class_expression = node.as_class_expression();
        let facts = self.get_class_facts(node);
        if facts != ClassFacts::None {
            self.get_class_lexical_environment().borrow_mut().facts = facts;
        }

        if facts.intersects(ClassFacts::NeedsSubstitutionForThisInClassStaticField) {
            self.enable_substitution_for_class_static_this_or_super_reference();
        }

        let is_decorated_class_declaration = facts.intersects(ClassFacts::ClassWasDecorated);

        let static_properties_or_class_static_blocks =
            get_static_properties_and_class_static_block(node);

        let extends_clause_element = get_effective_base_type_node(node);
        let is_derived_class = extends_clause_element.matches(|extends_clause_element| {
            skip_outer_expressions(
                &extends_clause_element
                    .as_expression_with_type_arguments()
                    .expression,
                None,
            )
            .kind()
                != SyntaxKind::NullKeyword
        });

        let is_class_with_constructor_reference = self
            .resolver
            .get_node_check_flags(node)
            .intersects(NodeCheckFlags::ClassWithConstructorReference);
        let mut temp: Option<Gc<Node /*Identifier*/>> = _d();

        if facts.intersects(ClassFacts::NeedsClassConstructorReference) {
            temp = Some(self.create_class_temp_var(node));
            self.get_class_lexical_environment()
                .borrow_mut()
                .class_constructor = Some(self.factory.clone_node(temp.as_ref().unwrap()));
        }

        let class_expression = self.factory.update_class_expression(
            node,
            maybe_visit_nodes(
                node.maybe_decorators().as_deref(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_decorator),
                None,
                None,
            ),
            node.maybe_modifiers(),
            node_as_class_expression.maybe_name(),
            Option::<Gc<NodeArray>>::None,
            maybe_visit_nodes(
                node_as_class_expression.maybe_heritage_clauses().as_deref(),
                Some(|node: &Node| self.heritage_clause_visitor(node)),
                Some(is_heritage_clause),
                None,
                None,
            ),
            self.transform_class_members(node, is_derived_class),
        );

        let has_transformable_statics = static_properties_or_class_static_blocks.iter().any(|p| {
            is_class_static_block_declaration(p) || {
                let p_as_property_declaration = p.as_property_declaration();
                p_as_property_declaration.maybe_initializer().is_some()
                    || self.should_transform_private_elements_or_class_static_blocks
                        && is_private_identifier(&p_as_property_declaration.name())
            }
        });
        if has_transformable_statics || self.maybe_pending_expressions().as_ref().is_non_empty() {
            if is_decorated_class_declaration {
                Debug_.assert_is_defined(
                    &self.maybe_pending_statements().as_ref(),
                    Some("Decorated classes transformed by TypeScript are expected to be within a variable declaration.")
                );

                if
                /*pendingStatements &&*/
                let Some(pending_expressions) = self.maybe_pending_expressions().as_ref() {
                    self.pending_statements_mut().push(
                        self.factory
                            .create_expression_statement(
                                self.factory.inline_expressions(pending_expressions),
                            )
                            .wrap(),
                    );
                }

                if
                /*pendingStatements &&*/
                !static_properties_or_class_static_blocks.is_empty() {
                    self.add_property_or_class_static_block_statements(
                        &mut self.pending_statements_mut(),
                        &static_properties_or_class_static_blocks,
                        &self.factory.get_internal_name(node, None, None),
                    );
                }
                if let Some(temp) = temp {
                    return self.factory.inline_expressions(&[
                        self.factory
                            .create_assignment(temp.clone(), class_expression)
                            .wrap(),
                        temp,
                    ]);
                }
                return class_expression;
            } else {
                let mut expressions: Vec<Gc<Node /*Expression*/>> = _d();
                temp.get_or_insert_with(|| self.create_class_temp_var(node));
                let temp = temp.unwrap();
                if is_class_with_constructor_reference {
                    self.enable_substitution_for_class_aliases();
                    let alias = self.factory.clone_node(&temp);
                    alias.as_identifier().set_auto_generate_flags(Some(
                        alias
                            .as_identifier()
                            .maybe_auto_generate_flags()
                            .unwrap_or_default()
                            & !GeneratedIdentifierFlags::ReservedInNestedScopes,
                    ));
                    self.class_aliases_mut()
                        .insert(get_original_node_id(node), alias);
                }

                set_emit_flags(
                    &*class_expression,
                    EmitFlags::Indented | get_emit_flags(&class_expression),
                );
                expressions.push(
                    self.factory
                        .create_assignment(temp.clone(), class_expression)
                        .wrap()
                        .start_on_new_line(),
                );
                add_range(
                    &mut expressions,
                    maybe_map(
                        self.maybe_pending_expressions().as_ref(),
                        |pending_expression: &Gc<Node>, _| {
                            pending_expression.clone().start_on_new_line()
                        },
                    )
                    .as_deref(),
                    None,
                    None,
                );
                add_range(
                    &mut expressions,
                    Some(
                        &self.generate_initialized_property_expressions_or_class_static_block(
                            &static_properties_or_class_static_blocks,
                            &temp,
                        ),
                    ),
                    None,
                    None,
                );
                expressions.push(temp.start_on_new_line());

                return self.factory.inline_expressions(&expressions);
            }
        }

        class_expression
    }

    pub(super) fn create_class_temp_var(&self, node: &Node) -> Gc<Node> {
        let class_check_flags = self.resolver.get_node_check_flags(node);
        let is_class_with_constructor_reference =
            class_check_flags.intersects(NodeCheckFlags::ClassWithConstructorReference);
        let requires_block_scoped_var =
            class_check_flags.intersects(NodeCheckFlags::BlockScopedBindingInLoop);
        self.factory.create_temp_variable(
            Some(|node: &Node| {
                if requires_block_scoped_var {
                    self.context.add_block_scoped_variable(node);
                } else {
                    self.context.hoist_variable_declaration(node);
                }
            }),
            Some(is_class_with_constructor_reference),
        )
    }

    pub(super) fn visit_class_static_block_declaration(
        &self,
        node: &Node, /*ClassStaticBlockDeclaration*/
    ) -> VisitResult {
        if !self.should_transform_private_elements_or_class_static_blocks {
            return Some(
                visit_each_child(
                    node,
                    |node: &Node| self.class_element_visitor(node),
                    &**self.context,
                )
                .into(),
            );
        }
        None
    }

    pub(super) fn transform_class_members(
        &self,
        node: &Node, /*ClassDeclaration | ClassExpression*/
        is_derived_class: bool,
    ) -> Gc<NodeArray> {
        let node_as_class_like_declaration = node.as_class_like_declaration();
        if self.should_transform_private_elements_or_class_static_blocks {
            for member in &node_as_class_like_declaration.members() {
                if is_private_identifier_class_element_declaration(member) {
                    self.add_private_identifier_to_environment(member);
                }
            }

            if !self
                .get_private_instance_methods_and_accessors(node)
                .is_empty()
            {
                self.create_brand_check_weak_set_for_private_methods();
            }
        }

        let mut members: Vec<Gc<Node /*ClassElement*/>> = _d();
        let constructor = self.transform_constructor(node, is_derived_class);
        if let Some(constructor) = constructor {
            members.push(constructor);
        }
        add_range(
            &mut members,
            Some(&visit_nodes(
                &node_as_class_like_declaration.members(),
                Some(|node: &Node| self.class_element_visitor(node)),
                Some(is_class_element),
                None,
                None,
            )),
            None,
            None,
        );
        self.factory
            .create_node_array(Some(members), None)
            .set_text_range(Some(&*node_as_class_like_declaration.members()))
    }

    pub(super) fn create_brand_check_weak_set_for_private_methods(&self) {
        let private_identifier_environment = self.get_private_identifier_environment();
        let private_identifier_environment = (*private_identifier_environment).borrow();
        let weak_set_name = private_identifier_environment.weak_set_name.as_ref();
        Debug_.assert(
            weak_set_name.is_some(),
            Some("weakSetName should be set in private identifier environment"),
        );
        let weak_set_name = weak_set_name.unwrap();

        self.get_pending_expressions().push(
            self.factory
                .create_assignment(
                    weak_set_name.clone(),
                    self.factory
                        .create_new_expression(
                            self.factory.create_identifier("WeakSet"),
                            Option::<Gc<NodeArray>>::None,
                            Some(vec![]),
                        )
                        .wrap(),
                )
                .wrap(),
        );
    }

    pub(super) fn is_class_element_that_requires_constructor_statement(
        &self,
        member: &Node, /*ClassElement*/
    ) -> bool {
        if is_static(member)
            || has_syntactic_modifier(&get_original_node(member), ModifierFlags::Abstract)
        {
            return false;
        }
        if self.use_define_for_class_fields {
            return self.language_version < ScriptTarget::ESNext;
        }
        is_initialized_property(member)
            || self.should_transform_private_elements_or_class_static_blocks
                && is_private_identifier_class_element_declaration(member)
    }

    pub(super) fn transform_constructor(
        &self,
        node: &Node, /*ClassDeclaration | ClassExpression*/
        is_derived_class: bool,
    ) -> Option<Gc<Node>> {
        let node_as_class_like_declaration = node.as_class_like_declaration();
        let constructor = maybe_visit_node(
            get_first_constructor_with_body(node),
            Some(|node: &Node| self.visitor(node)),
            Some(is_constructor_declaration),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        );
        let elements = node_as_class_like_declaration
            .members()
            .owned_iter()
            .filter(|member| self.is_class_element_that_requires_constructor_statement(member))
            .collect_vec();
        if elements.is_empty() {
            return constructor;
        }
        let parameters = visit_parameter_list(
            constructor
                .as_ref()
                .map(|constructor| constructor.as_signature_declaration().parameters())
                .as_deref(),
            |node: &Node| self.visitor(node),
            &**self.context,
            Option::<
                fn(
                    Option<&NodeArray>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<usize>,
                    Option<usize>,
                ) -> Option<Gc<NodeArray>>,
            >::None,
        );
        let body =
            self.transform_constructor_body(node, constructor.as_deref(), is_derived_class)?;
        Some(
            self.factory
                .create_constructor_declaration(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    Some(parameters.map_or_else(|| vec![].into(), NodeArrayOrVec::from)),
                    Some(body),
                )
                .wrap()
                .set_text_range(Some(constructor.as_deref().unwrap_or(node)))
                .set_original_node(constructor)
                .start_on_new_line(),
        )
    }

    pub(super) fn transform_constructor_body(
        &self,
        node: &Node, /*ClassDeclaration | ClassExpression*/
        constructor: Option<impl Borrow<Node /*ConstructorDeclaration*/>>,
        is_derived_class: bool,
    ) -> Option<Gc<Node>> {
        let node_as_class_like_declaration = node.as_class_like_declaration();
        let mut properties = get_properties(node, false, false);
        if !self.use_define_for_class_fields {
            properties = filter(&properties, |property: &Gc<Node>| {
                let property_as_property_declaration = property.as_property_declaration();
                property_as_property_declaration
                    .maybe_initializer()
                    .is_some()
                    || is_private_identifier(&property_as_property_declaration.name())
            });
        }

        let private_methods_and_accessors = self.get_private_instance_methods_and_accessors(node);
        let needs_constructor_body =
            !properties.is_empty() || !private_methods_and_accessors.is_empty();

        if constructor.is_none() && !needs_constructor_body {
            return visit_function_body(
                None,
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            );
        }

        self.context.resume_lexical_environment();

        let mut index_of_first_statement = 0;
        let mut statements: Vec<Gc<Node /*Statement*/>> = _d();

        if constructor.is_none() && is_derived_class {
            statements.push(
                self.factory
                    .create_expression_statement(
                        self.factory
                            .create_call_expression(
                                self.factory.create_super().wrap(),
                                Option::<Gc<NodeArray>>::None,
                                Some(vec![self
                                    .factory
                                    .create_spread_element(
                                        self.factory.create_identifier("arguments"),
                                    )
                                    .wrap()]),
                            )
                            .wrap(),
                    )
                    .wrap(),
            );
        }

        let constructor = constructor.node_wrappered();
        if let Some(constructor) = constructor.as_ref() {
            index_of_first_statement = add_prologue_directives_and_initial_super_call(
                &self.factory,
                constructor,
                &mut statements,
                |node: &Node| self.visitor(node),
            );
        }
        if let Some(constructor) = constructor.as_ref() {
            let constructor_as_constructor_declaration = constructor.as_constructor_declaration();
            if let Some(constructor_body) = constructor_as_constructor_declaration.maybe_body() {
                let after_parameter_properties = find_index(
                    &constructor_body.as_block().statements,
                    |s: &Gc<Node>, _| {
                        !is_parameter_property_declaration(&get_original_node(s), constructor)
                    },
                    Some(index_of_first_statement),
                );
                let after_parameter_properties = after_parameter_properties
                    .unwrap_or_else(|| constructor_body.as_block().statements.len());
                if after_parameter_properties > index_of_first_statement {
                    if !self.use_define_for_class_fields {
                        add_range(
                            &mut statements,
                            Some(&visit_nodes(
                                &constructor_body.as_block().statements,
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_statement),
                                Some(index_of_first_statement),
                                Some(after_parameter_properties - index_of_first_statement),
                            )),
                            None,
                            None,
                        );
                    }
                    index_of_first_statement = after_parameter_properties;
                }
            }
        }
        let ref receiver = self.factory.create_this().wrap();
        self.add_method_statements(&mut statements, &private_methods_and_accessors, receiver);
        self.add_property_or_class_static_block_statements(&mut statements, &properties, receiver);

        if let Some(constructor) = constructor.as_ref() {
            add_range(
                &mut statements,
                Some(&visit_nodes(
                    &constructor
                        .as_constructor_declaration()
                        .maybe_body()
                        .unwrap()
                        .as_block()
                        .statements,
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_statement),
                    Some(index_of_first_statement),
                    None,
                )),
                None,
                None,
            );
        }

        statements = self
            .factory
            .merge_lexical_environment(
                statements,
                self.context.end_lexical_environment().as_deref(),
            )
            .as_vec_owned();

        Some(
            self.factory
                .create_block(
                    self.factory
                        .create_node_array(Some(statements), None)
                        .set_text_range(Some(&*constructor.as_ref().map_or_else(
                            || node_as_class_like_declaration.members(),
                            |constructor| {
                                constructor
                                    .as_constructor_declaration()
                                    .maybe_body()
                                    .unwrap()
                                    .as_block()
                                    .statements
                                    .clone()
                            },
                        ))),
                    Some(true),
                )
                .wrap()
                .set_text_range(
                    constructor
                        .as_ref()
                        .and_then(|constructor| {
                            constructor.as_constructor_declaration().maybe_body()
                        })
                        .as_deref(),
                ),
        )
    }

    pub(super) fn add_property_or_class_static_block_statements(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        properties: &[Gc<Node /*PropertyDeclaration | ClassStaticBlockDeclaration*/>],
        receiver: &Node, /*LeftHandSideExpression*/
    ) {
        for property in properties {
            let expression = continue_if_none!(if is_class_static_block_declaration(property) {
                self.transform_class_static_block_declaration(property)
            } else {
                self.transform_property(property, receiver)
            });
            let statement = self
                .factory
                .create_expression_statement(expression)
                .wrap()
                .set_source_map_range(Some((&move_range_past_modifiers(property)).into()))
                .set_comment_range(&**property)
                .set_original_node(Some(property.clone()));
            statements.push(statement);
        }
    }

    pub(super) fn generate_initialized_property_expressions_or_class_static_block(
        &self,
        properties_or_class_static_blocks: &[Gc<
            Node, /*PropertyDeclaration | ClassStaticBlockDeclaration*/
        >],
        receiver: &Node, /*LeftHandSideExpression*/
    ) -> Vec<Gc<Node>> {
        let mut expressions: Vec<Gc<Node /*Expression*/>> = _d();
        for property in properties_or_class_static_blocks {
            let expression = continue_if_none!(if is_class_static_block_declaration(property) {
                self.transform_class_static_block_declaration(property)
            } else {
                self.transform_property(property, receiver)
            })
            .start_on_new_line()
            .set_source_map_range(Some((&move_range_past_modifiers(property)).into()))
            .set_comment_range(&**property)
            .set_original_node(Some(property.clone()));
            expressions.push(expression);
        }

        expressions
    }

    pub(super) fn transform_property(
        &self,
        property: &Node, /*PropertyDeclaration*/
        receiver: &Node, /*LeftHandSideExpression*/
    ) -> Option<Gc<Node>> {
        let saved_current_static_property_declaration_or_static_block =
            self.maybe_current_static_property_declaration_or_static_block();
        let transformed = self.transform_property_worker(property, receiver);
        if let Some(transformed) = transformed.as_ref() {
            if has_static_modifier(property) {
                if let Some(current_class_lexical_environment) = self
                    .maybe_current_class_lexical_environment()
                    .filter(|current_class_lexical_environment| {
                        (**current_class_lexical_environment).borrow().facts != ClassFacts::None
                    })
                {
                    set_original_node(&**transformed, Some(property.node_wrapper()));
                    add_emit_flags(&**transformed, EmitFlags::AdviseOnEmitNode);
                    self.class_lexical_environment_map_mut().insert(
                        get_original_node_id(transformed),
                        current_class_lexical_environment,
                    );
                }
            }
        }
        self.set_current_static_property_declaration_or_static_block(
            saved_current_static_property_declaration_or_static_block,
        );
        transformed
    }

    pub(super) fn transform_property_worker(
        &self,
        property: &Node, /*PropertyDeclaration*/
        receiver: &Node, /*LeftHandSideExpression*/
    ) -> Option<Gc<Node>> {
        let property_as_property_declaration = property.as_property_declaration();
        let emit_assignment = !self.use_define_for_class_fields;
        let ref property_name =
            if is_computed_property_name(&property_as_property_declaration.name())
                && !is_simple_inlineable_expression(
                    &property_as_property_declaration
                        .name()
                        .as_computed_property_name()
                        .expression,
                )
            {
                self.factory.update_computed_property_name(
                    &property_as_property_declaration.name(),
                    self.factory.get_generated_name_for_node(
                        Some(property_as_property_declaration.name()),
                        None,
                    ),
                )
            } else {
                property_as_property_declaration.name()
            };

        if has_static_modifier(property) {
            self.set_current_static_property_declaration_or_static_block(Some(
                property.node_wrapper(),
            ));
        }

        if self.should_transform_private_elements_or_class_static_blocks
            && is_private_identifier(property_name)
        {
            let private_identifier_info = self.access_private_identifier(property_name);
            if let Some(private_identifier_info) = private_identifier_info {
                let private_identifier_info = (*private_identifier_info).borrow();
                if private_identifier_info.kind() == PrivateIdentifierKind::Field {
                    if !private_identifier_info.is_static() {
                        return Some(create_private_instance_field_initializer(
                            receiver.node_wrapper(),
                            maybe_visit_node(
                                property_as_property_declaration.maybe_initializer(),
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_expression),
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            ),
                            private_identifier_info.brand_check_identifier(),
                        ));
                    } else {
                        return Some(create_private_static_field_initializer(
                            private_identifier_info.maybe_variable_name().unwrap(),
                            maybe_visit_node(
                                property_as_property_declaration.maybe_initializer(),
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_expression),
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            ),
                        ));
                    }
                } else {
                    return None;
                }
            } else {
                Debug_.fail(Some("Undeclared private name for property declaration."));
            }
        }
        if (is_private_identifier(property_name) || has_static_modifier(property))
            && property_as_property_declaration
                .maybe_initializer()
                .is_none()
        {
            return None;
        }

        let ref property_original_node = get_original_node(property);
        if has_syntactic_modifier(property_original_node, ModifierFlags::Abstract) {
            return None;
        }

        let initializer = if property_as_property_declaration
            .maybe_initializer()
            .is_some()
            || emit_assignment
        {
            maybe_visit_node(
                property_as_property_declaration.maybe_initializer(),
                Some(|node: &Node| self.visitor(node)),
                Some(is_expression),
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )
            .unwrap_or_else(|| self.factory.create_void_zero())
        } else if is_parameter_property_declaration(
            property_original_node,
            &property_original_node.parent(),
        ) && is_identifier(property_name)
        {
            property_name.clone()
        } else {
            self.factory.create_void_zero()
        };

        Some(if emit_assignment || is_private_identifier(property_name) {
            let member_access = create_member_access_for_property_name(
                &self.factory,
                receiver,
                property_name,
                Some(&**property_name),
            );
            self.factory
                .create_assignment(member_access, initializer)
                .wrap()
        } else {
            let name = if is_computed_property_name(property_name) {
                property_name.as_computed_property_name().expression.clone()
            } else if is_identifier(property_name) {
                self.factory
                    .create_string_literal(
                        unescape_leading_underscores(&property_name.as_identifier().escaped_text)
                            .to_owned(),
                        None,
                        None,
                    )
                    .wrap()
            } else {
                property_name.clone()
            };
            let descriptor = self.factory.create_property_descriptor(
                PropertyDescriptorAttributesBuilder::default()
                    .value(initializer)
                    .configurable(true)
                    .writable(true)
                    .enumerable(true)
                    .build()
                    .unwrap(),
                None,
            );
            self.factory.create_object_define_property_call(
                receiver.node_wrapper(),
                name,
                descriptor,
            )
        })
    }

    pub(super) fn enable_substitution_for_class_aliases(&self) {
        if !self
            .maybe_enabled_substitutions()
            .unwrap_or_default()
            .intersects(ClassPropertySubstitutionFlags::ClassAliases)
        {
            self.set_enabled_substitutions(Some(
                self.maybe_enabled_substitutions().unwrap_or_default()
                    | ClassPropertySubstitutionFlags::ClassAliases,
            ));

            self.context.enable_substitution(SyntaxKind::Identifier);

            self.set_class_aliases(Some(_d()));
        }
    }

    pub(super) fn enable_substitution_for_class_static_this_or_super_reference(&self) {
        if !self
            .maybe_enabled_substitutions()
            .unwrap_or_default()
            .intersects(ClassPropertySubstitutionFlags::ClassStaticThisOrSuperReference)
        {
            self.set_enabled_substitutions(Some(
                self.maybe_enabled_substitutions().unwrap_or_default()
                    | ClassPropertySubstitutionFlags::ClassStaticThisOrSuperReference,
            ));

            self.context.enable_substitution(SyntaxKind::ThisKeyword);

            self.context
                .enable_emit_notification(SyntaxKind::FunctionDeclaration);
            self.context
                .enable_emit_notification(SyntaxKind::FunctionExpression);
            self.context
                .enable_emit_notification(SyntaxKind::Constructor);

            self.context
                .enable_emit_notification(SyntaxKind::GetAccessor);
            self.context
                .enable_emit_notification(SyntaxKind::SetAccessor);
            self.context
                .enable_emit_notification(SyntaxKind::MethodDeclaration);
            self.context
                .enable_emit_notification(SyntaxKind::PropertyDeclaration);

            self.context
                .enable_emit_notification(SyntaxKind::ComputedPropertyName);
        }
    }

    pub(super) fn add_method_statements(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        methods: &[Gc<Node /*MethodDeclaration | AccessorDeclaration*/>],
        receiver: &Node, /*LeftHandSideExpression*/
    ) {
        if !self.should_transform_private_elements_or_class_static_blocks || methods.is_empty() {
            return;
        }

        let private_identifier_environment = self.get_private_identifier_environment();
        let private_identifier_environment = (*private_identifier_environment).borrow();
        let weak_set_name = private_identifier_environment.weak_set_name.as_ref();
        Debug_.assert(
            weak_set_name.is_some(),
            Some("weakSetName should be set in private identifier environment"),
        );
        let weak_set_name = weak_set_name.unwrap();
        statements.push(
            self.factory
                .create_expression_statement(create_private_instance_method_initializer(
                    receiver.node_wrapper(),
                    weak_set_name.clone(),
                ))
                .wrap(),
        );
    }
}
