use std::borrow::Borrow;

use gc::Gc;
use id_arena::Id;
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
    ModifierFlags, NodeArray, NodeArrayExt, NodeArrayOrVec, NodeCheckFlags, NodeExt,
    PrivateIdentifierKind, PropertyDescriptorAttributesBuilder, ScriptTarget, SyntaxKind,
    HasArena, InArena, OptionInArena,
    CoreTransformationContext, TransformationContext,
};

impl TransformClassFields {
    pub(super) fn visit_class_declaration(
        &self,
        node: Id<Node>, /*ClassDeclaration*/
    ) -> VisitResult {
        let node_ref = node.ref_(self);
        let node_as_class_declaration = node_ref.as_class_declaration();
        let facts = self.get_class_facts(node);
        if facts != ClassFacts::None {
            self.get_class_lexical_environment().borrow_mut().facts = facts;
        }
        if facts.intersects(ClassFacts::NeedsSubstitutionForThisInClassStaticField) {
            self.enable_substitution_for_class_static_this_or_super_reference();
        }

        let static_properties = get_static_properties_and_class_static_block(node, self);

        let mut pending_class_reference_assignment: Option<Id<Node /*BinaryExpression*/>> = _d();
        if facts.intersects(ClassFacts::NeedsClassConstructorReference) {
            let temp = self.factory.ref_(self).create_temp_variable(
                Some(|node: Id<Node>| {
                    self.context.ref_(self).hoist_variable_declaration(node);
                }),
                Some(true),
            );
            self.get_class_lexical_environment()
                .borrow_mut()
                .class_constructor = Some(self.factory.ref_(self).clone_node(temp));
            pending_class_reference_assignment = Some(
                self.factory
                    .ref_(self).create_assignment(temp, self.factory.ref_(self).get_internal_name(node, None, None)),
            );
        }

        let extends_clause_element = get_effective_base_type_node(node, self);
        let is_derived_class = extends_clause_element.matches(|extends_clause_element| {
            skip_outer_expressions(
                extends_clause_element
                    .ref_(self).as_expression_with_type_arguments()
                    .expression,
                None,
                self,
            )
            .ref_(self).kind()
                != SyntaxKind::NullKeyword
        });

        let mut statements: Vec<Id<Node /*Statement*/>> =
            vec![self.factory.ref_(self).update_class_declaration(
                node,
                Option::<Gc<NodeArray>>::None,
                node.ref_(self).maybe_modifiers(),
                node_as_class_declaration.maybe_name(),
                Option::<Gc<NodeArray>>::None,
                maybe_visit_nodes(
                    node_as_class_declaration
                        .maybe_heritage_clauses()
                        .as_deref(),
                    Some(|node: Id<Node>| self.heritage_clause_visitor(node)),
                    Some(|node: Id<Node>| is_heritage_clause(&node.ref_(self))),
                    None,
                    None,
                    self,
                ),
                self.transform_class_members(node, is_derived_class),
            )];

        if let Some(pending_class_reference_assignment) = pending_class_reference_assignment {
            self.get_pending_expressions()
                .insert(0, pending_class_reference_assignment);
        }

        if let Some(pending_expressions) = self.maybe_pending_expressions().as_ref().non_empty() {
            statements.push(
                self.factory.ref_(self).create_expression_statement(
                    self.factory.ref_(self).inline_expressions(pending_expressions),
                ),
            );
        }

        if !static_properties.is_empty() {
            self.add_property_or_class_static_block_statements(
                &mut statements,
                &static_properties,
                self.factory.ref_(self).get_internal_name(node, None, None),
            );
        }

        Some(statements.into())
    }

    pub(super) fn visit_class_expression(
        &self,
        node: Id<Node>, /*ClassExpression*/
    ) -> Id<Node /*Expression*/> {
        let node_ref = node.ref_(self);
        let node_as_class_expression = node_ref.as_class_expression();
        let facts = self.get_class_facts(node);
        if facts != ClassFacts::None {
            self.get_class_lexical_environment().borrow_mut().facts = facts;
        }

        if facts.intersects(ClassFacts::NeedsSubstitutionForThisInClassStaticField) {
            self.enable_substitution_for_class_static_this_or_super_reference();
        }

        let is_decorated_class_declaration = facts.intersects(ClassFacts::ClassWasDecorated);

        let static_properties_or_class_static_blocks =
            get_static_properties_and_class_static_block(node, self);

        let extends_clause_element = get_effective_base_type_node(node, self);
        let is_derived_class = extends_clause_element.matches(|extends_clause_element| {
            skip_outer_expressions(
                extends_clause_element
                    .ref_(self).as_expression_with_type_arguments()
                    .expression,
                None,
                self,
            )
            .ref_(self).kind()
                != SyntaxKind::NullKeyword
        });

        let is_class_with_constructor_reference = self
            .resolver
            .get_node_check_flags(node)
            .intersects(NodeCheckFlags::ClassWithConstructorReference);
        let mut temp: Option<Id<Node /*Identifier*/>> = _d();

        if facts.intersects(ClassFacts::NeedsClassConstructorReference) {
            temp = Some(self.create_class_temp_var(node));
            self.get_class_lexical_environment()
                .borrow_mut()
                .class_constructor = Some(self.factory.ref_(self).clone_node(temp.unwrap()));
        }

        let class_expression = self.factory.ref_(self).update_class_expression(
            node,
            maybe_visit_nodes(
                node.ref_(self).maybe_decorators().as_deref(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node: Id<Node>| is_decorator(&node.ref_(self))),
                None,
                None,
                self,
            ),
            node.ref_(self).maybe_modifiers(),
            node_as_class_expression.maybe_name(),
            Option::<Gc<NodeArray>>::None,
            maybe_visit_nodes(
                node_as_class_expression.maybe_heritage_clauses().as_deref(),
                Some(|node: Id<Node>| self.heritage_clause_visitor(node)),
                Some(|node: Id<Node>| is_heritage_clause(&node.ref_(self))),
                None,
                None,
                self,
            ),
            self.transform_class_members(node, is_derived_class),
        );

        let has_transformable_statics = static_properties_or_class_static_blocks.iter().any(|p| {
            is_class_static_block_declaration(&p.ref_(self)) || {
                let p_ref = p.ref_(self);
                let p_as_property_declaration = p_ref.as_property_declaration();
                p_as_property_declaration.maybe_initializer().is_some()
                    || self.should_transform_private_elements_or_class_static_blocks
                        && is_private_identifier(&p_as_property_declaration.name().ref_(self))
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
                    self.pending_statements_mut()
                        .push(self.factory.ref_(self).create_expression_statement(
                            self.factory.ref_(self).inline_expressions(pending_expressions),
                        ));
                }

                if
                /*pendingStatements &&*/
                !static_properties_or_class_static_blocks.is_empty() {
                    self.add_property_or_class_static_block_statements(
                        &mut self.pending_statements_mut(),
                        &static_properties_or_class_static_blocks,
                        self.factory.ref_(self).get_internal_name(node, None, None),
                    );
                }
                if let Some(temp) = temp {
                    return self.factory.ref_(self).inline_expressions(&[
                        self.factory
                            .ref_(self).create_assignment(temp.clone(), class_expression),
                        temp,
                    ]);
                }
                return class_expression;
            } else {
                let mut expressions: Vec<Id<Node /*Expression*/>> = _d();
                temp.get_or_insert_with(|| self.create_class_temp_var(node));
                let temp = temp.unwrap();
                if is_class_with_constructor_reference {
                    self.enable_substitution_for_class_aliases();
                    let alias = self.factory.ref_(self).clone_node(temp);
                    alias.ref_(self).as_identifier().set_auto_generate_flags(Some(
                        alias
                            .ref_(self).as_identifier()
                            .maybe_auto_generate_flags()
                            .unwrap_or_default()
                            & !GeneratedIdentifierFlags::ReservedInNestedScopes,
                    ));
                    self.class_aliases_mut()
                        .insert(get_original_node_id(node, self), alias);
                }

                set_emit_flags(
                    class_expression,
                    EmitFlags::Indented | get_emit_flags(&class_expression.ref_(self)),
                    self,
                );
                expressions.push(
                    self.factory
                        .ref_(self).create_assignment(temp, class_expression)
                        .start_on_new_line(self),
                );
                add_range(
                    &mut expressions,
                    maybe_map(
                        self.maybe_pending_expressions().as_ref(),
                        |&pending_expression: &Id<Node>, _| {
                            pending_expression.start_on_new_line(self)
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
                            temp,
                        ),
                    ),
                    None,
                    None,
                );
                expressions.push(temp.start_on_new_line(self));

                return self.factory.ref_(self).inline_expressions(&expressions);
            }
        }

        class_expression
    }

    pub(super) fn create_class_temp_var(&self, node: Id<Node>) -> Id<Node> {
        let class_check_flags = self.resolver.get_node_check_flags(node);
        let is_class_with_constructor_reference =
            class_check_flags.intersects(NodeCheckFlags::ClassWithConstructorReference);
        let requires_block_scoped_var =
            class_check_flags.intersects(NodeCheckFlags::BlockScopedBindingInLoop);
        self.factory.ref_(self).create_temp_variable(
            Some(|node: Id<Node>| {
                if requires_block_scoped_var {
                    self.context.ref_(self).add_block_scoped_variable(node);
                } else {
                    self.context.ref_(self).hoist_variable_declaration(node);
                }
            }),
            Some(is_class_with_constructor_reference),
        )
    }

    pub(super) fn visit_class_static_block_declaration(
        &self,
        node: Id<Node>, /*ClassStaticBlockDeclaration*/
    ) -> VisitResult {
        if !self.should_transform_private_elements_or_class_static_blocks {
            return Some(
                visit_each_child(
                    node,
                    |node: Id<Node>| self.class_element_visitor(node),
                    &*self.context.ref_(self),
                    self,
                )
                .into(),
            );
        }
        None
    }

    pub(super) fn transform_class_members(
        &self,
        node: Id<Node>, /*ClassDeclaration | ClassExpression*/
        is_derived_class: bool,
    ) -> Gc<NodeArray> {
        let node_ref = node.ref_(self);
        let node_as_class_like_declaration = node_ref.as_class_like_declaration();
        if self.should_transform_private_elements_or_class_static_blocks {
            for &member in &node_as_class_like_declaration.members() {
                if is_private_identifier_class_element_declaration(member, self) {
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

        let mut members: Vec<Id<Node /*ClassElement*/>> = _d();
        let constructor = self.transform_constructor(node, is_derived_class);
        if let Some(constructor) = constructor {
            members.push(constructor);
        }
        add_range(
            &mut members,
            Some(&visit_nodes(
                &node_as_class_like_declaration.members(),
                Some(|node: Id<Node>| self.class_element_visitor(node)),
                Some(|node: Id<Node>| is_class_element(&node.ref_(self))),
                None,
                None,
                self,
            )),
            None,
            None,
        );
        self.factory
            .ref_(self).create_node_array(Some(members), None)
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

        self.get_pending_expressions()
            .push(self.factory.ref_(self).create_assignment(
                weak_set_name.clone(),
                self.factory.ref_(self).create_new_expression(
                    self.factory.ref_(self).create_identifier("WeakSet"),
                    Option::<Gc<NodeArray>>::None,
                    Some(vec![]),
                ),
            ));
    }

    pub(super) fn is_class_element_that_requires_constructor_statement(
        &self,
        member: Id<Node>, /*ClassElement*/
    ) -> bool {
        if is_static(member, self)
            || has_syntactic_modifier(get_original_node(member, self), ModifierFlags::Abstract, self)
        {
            return false;
        }
        if self.use_define_for_class_fields {
            return self.language_version < ScriptTarget::ESNext;
        }
        is_initialized_property(&member.ref_(self))
            || self.should_transform_private_elements_or_class_static_blocks
                && is_private_identifier_class_element_declaration(member, self)
    }

    pub(super) fn transform_constructor(
        &self,
        node: Id<Node>, /*ClassDeclaration | ClassExpression*/
        is_derived_class: bool,
    ) -> Option<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_class_like_declaration = node_ref.as_class_like_declaration();
        let constructor = maybe_visit_node(
            get_first_constructor_with_body(node, self),
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node: Id<Node>| is_constructor_declaration(&node.ref_(self))),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        let elements = node_as_class_like_declaration
            .members()
            .owned_iter()
            .filter(|&member| self.is_class_element_that_requires_constructor_statement(member))
            .collect_vec();
        if elements.is_empty() {
            return constructor;
        }
        let parameters = visit_parameter_list(
            constructor
                .map(|constructor| constructor.ref_(self).as_signature_declaration().parameters())
                .as_deref(),
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        );
        let body =
            self.transform_constructor_body(node, constructor, is_derived_class)?;
        Some(
            self.factory
                .ref_(self).create_constructor_declaration(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    Some(parameters.map_or_else(|| vec![].into(), NodeArrayOrVec::from)),
                    Some(body),
                )
                .set_text_range(Some(&*constructor.unwrap_or(node).ref_(self)), self)
                .set_original_node(constructor, self)
                .start_on_new_line(self),
        )
    }

    pub(super) fn transform_constructor_body(
        &self,
        node: Id<Node>, /*ClassDeclaration | ClassExpression*/
        constructor: Option<Id<Node /*ConstructorDeclaration*/>>,
        is_derived_class: bool,
    ) -> Option<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_class_like_declaration = node_ref.as_class_like_declaration();
        let mut properties = get_properties(node, false, false, self);
        if !self.use_define_for_class_fields {
            properties = filter(&properties, |property: &Id<Node>| {
                let property_ref = property.ref_(self);
                let property_as_property_declaration = property_ref.as_property_declaration();
                property_as_property_declaration
                    .maybe_initializer()
                    .is_some()
                    || is_private_identifier(&property_as_property_declaration.name().ref_(self))
            });
        }

        let private_methods_and_accessors = self.get_private_instance_methods_and_accessors(node);
        let needs_constructor_body =
            !properties.is_empty() || !private_methods_and_accessors.is_empty();

        if constructor.is_none() && !needs_constructor_body {
            return visit_function_body(None, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self);
        }

        self.context.ref_(self).resume_lexical_environment();

        let mut index_of_first_statement = 0;
        let mut statements: Vec<Id<Node /*Statement*/>> = _d();

        if constructor.is_none() && is_derived_class {
            statements.push(self.factory.ref_(self).create_expression_statement(
                self.factory.ref_(self).create_call_expression(
                    self.factory.ref_(self).create_super(),
                    Option::<Gc<NodeArray>>::None,
                    Some(vec![self.factory.ref_(self).create_spread_element(
                        self.factory.ref_(self).create_identifier("arguments"),
                    )]),
                ),
            ));
        }

        if let Some(constructor) = constructor {
            index_of_first_statement = add_prologue_directives_and_initial_super_call(
                &self.factory.ref_(self),
                constructor,
                &mut statements,
                |node: Id<Node>| self.visitor(node),
                self,
            );
        }
        if let Some(constructor) = constructor {
            let constructor_ref = constructor.ref_(self);
            let constructor_as_constructor_declaration = constructor_ref.as_constructor_declaration();
            if let Some(constructor_body) = constructor_as_constructor_declaration.maybe_body() {
                let after_parameter_properties = find_index(
                    &constructor_body.ref_(self).as_block().statements,
                    |&s: &Id<Node>, _| {
                        !is_parameter_property_declaration(get_original_node(s, self), constructor, self)
                    },
                    Some(index_of_first_statement),
                );
                let after_parameter_properties = after_parameter_properties
                    .unwrap_or_else(|| constructor_body.ref_(self).as_block().statements.len());
                if after_parameter_properties > index_of_first_statement {
                    if !self.use_define_for_class_fields {
                        add_range(
                            &mut statements,
                            Some(&visit_nodes(
                                &constructor_body.ref_(self).as_block().statements,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_statement(node, self)),
                                Some(index_of_first_statement),
                                Some(after_parameter_properties - index_of_first_statement),
                                self,
                            )),
                            None,
                            None,
                        );
                    }
                    index_of_first_statement = after_parameter_properties;
                }
            }
        }
        let receiver = self.factory.ref_(self).create_this();
        self.add_method_statements(&mut statements, &private_methods_and_accessors, receiver);
        self.add_property_or_class_static_block_statements(&mut statements, &properties, receiver);

        if let Some(constructor) = constructor {
            add_range(
                &mut statements,
                Some(&visit_nodes(
                    &constructor
                        .ref_(self).as_constructor_declaration()
                        .maybe_body()
                        .unwrap()
                        .ref_(self).as_block()
                        .statements,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_statement(node, self)),
                    Some(index_of_first_statement),
                    None,
                    self,
                )),
                None,
                None,
            );
        }

        statements = self
            .factory
            .ref_(self).merge_lexical_environment(
                statements,
                self.context.ref_(self).end_lexical_environment().as_deref(),
            )
            .as_vec_owned();

        Some(
            self.factory
                .ref_(self).create_block(
                    self.factory
                        .ref_(self).create_node_array(Some(statements), None)
                        .set_text_range(Some(&*constructor.as_ref().map_or_else(
                            || node_as_class_like_declaration.members(),
                            |constructor| {
                                constructor
                                    .ref_(self).as_constructor_declaration()
                                    .maybe_body()
                                    .unwrap()
                                    .ref_(self).as_block()
                                    .statements
                                    .clone()
                            },
                        ))),
                    Some(true),
                )
                .set_text_range(
                    constructor
                        .and_then(|constructor| {
                            constructor.ref_(self).as_constructor_declaration().maybe_body()
                        })
                        .refed(self).as_deref(),
                    self,
                ),
        )
    }

    pub(super) fn add_property_or_class_static_block_statements(
        &self,
        statements: &mut Vec<Id<Node /*Statement*/>>,
        properties: &[Id<Node /*PropertyDeclaration | ClassStaticBlockDeclaration*/>],
        receiver: Id<Node>, /*LeftHandSideExpression*/
    ) {
        for &property in properties {
            let expression = continue_if_none!(if is_class_static_block_declaration(&property.ref_(self)) {
                self.transform_class_static_block_declaration(property)
            } else {
                self.transform_property(property, receiver)
            });
            let statement = self
                .factory
                .ref_(self).create_expression_statement(expression)
                .set_source_map_range(Some(self.alloc_source_map_range((&move_range_past_modifiers(&property.ref_(self))).into())), self)
                .set_comment_range(&*property.ref_(self), self)
                .set_original_node(Some(property), self);
            statements.push(statement);
        }
    }

    pub(super) fn generate_initialized_property_expressions_or_class_static_block(
        &self,
        properties_or_class_static_blocks: &[Id<
            Node, /*PropertyDeclaration | ClassStaticBlockDeclaration*/
        >],
        receiver: Id<Node>, /*LeftHandSideExpression*/
    ) -> Vec<Id<Node>> {
        let mut expressions: Vec<Id<Node /*Expression*/>> = _d();
        for &property in properties_or_class_static_blocks {
            let expression = continue_if_none!(if is_class_static_block_declaration(&property.ref_(self)) {
                self.transform_class_static_block_declaration(property)
            } else {
                self.transform_property(property, receiver)
            })
            .start_on_new_line(self)
            .set_source_map_range(Some(self.alloc_source_map_range((&move_range_past_modifiers(&property.ref_(self))).into())), self)
            .set_comment_range(&*property.ref_(self), self)
            .set_original_node(Some(property), self);
            expressions.push(expression);
        }

        expressions
    }

    pub(super) fn transform_property(
        &self,
        property: Id<Node>, /*PropertyDeclaration*/
        receiver: Id<Node>, /*LeftHandSideExpression*/
    ) -> Option<Id<Node>> {
        let saved_current_static_property_declaration_or_static_block =
            self.maybe_current_static_property_declaration_or_static_block();
        let transformed = self.transform_property_worker(property, receiver);
        if let Some(transformed) = transformed {
            if has_static_modifier(property, self) {
                if let Some(current_class_lexical_environment) = self
                    .maybe_current_class_lexical_environment()
                    .filter(|current_class_lexical_environment| {
                        (**current_class_lexical_environment).borrow().facts != ClassFacts::None
                    })
                {
                    set_original_node(transformed, Some(property), self);
                    add_emit_flags(transformed, EmitFlags::AdviseOnEmitNode, self);
                    self.class_lexical_environment_map_mut().insert(
                        get_original_node_id(transformed, self),
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
        property: Id<Node>, /*PropertyDeclaration*/
        receiver: Id<Node>, /*LeftHandSideExpression*/
    ) -> Option<Id<Node>> {
        let property_ref = property.ref_(self);
        let property_as_property_declaration = property_ref.as_property_declaration();
        let emit_assignment = !self.use_define_for_class_fields;
        let property_name =
            if is_computed_property_name(&property_as_property_declaration.name().ref_(self))
                && !is_simple_inlineable_expression(
                    &property_as_property_declaration
                        .name()
                        .ref_(self).as_computed_property_name()
                        .expression.ref_(self),
                )
            {
                self.factory.ref_(self).update_computed_property_name(
                    property_as_property_declaration.name(),
                    self.factory.ref_(self).get_generated_name_for_node(
                        Some(property_as_property_declaration.name()),
                        None,
                    ),
                )
            } else {
                property_as_property_declaration.name()
            };

        if has_static_modifier(property, self) {
            self.set_current_static_property_declaration_or_static_block(Some(
                property,
            ));
        }

        if self.should_transform_private_elements_or_class_static_blocks
            && is_private_identifier(&property_name.ref_(self))
        {
            let private_identifier_info = self.access_private_identifier(property_name);
            if let Some(private_identifier_info) = private_identifier_info {
                let private_identifier_info = (*private_identifier_info).borrow();
                if private_identifier_info.kind() == PrivateIdentifierKind::Field {
                    if !private_identifier_info.is_static() {
                        return Some(create_private_instance_field_initializer(
                            receiver,
                            maybe_visit_node(
                                property_as_property_declaration.maybe_initializer(),
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ),
                            private_identifier_info.brand_check_identifier(),
                            self,
                        ));
                    } else {
                        return Some(create_private_static_field_initializer(
                            private_identifier_info.maybe_variable_name().unwrap(),
                            maybe_visit_node(
                                property_as_property_declaration.maybe_initializer(),
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ),
                            self,
                        ));
                    }
                } else {
                    return None;
                }
            } else {
                Debug_.fail(Some("Undeclared private name for property declaration."));
            }
        }
        if (is_private_identifier(&property_name.ref_(self)) || has_static_modifier(property, self))
            && property_as_property_declaration
                .maybe_initializer()
                .is_none()
        {
            return None;
        }

        let property_original_node = get_original_node(property, self);
        if has_syntactic_modifier(property_original_node, ModifierFlags::Abstract, self) {
            return None;
        }

        let initializer = if property_as_property_declaration
            .maybe_initializer()
            .is_some()
            || emit_assignment
        {
            maybe_visit_node(
                property_as_property_declaration.maybe_initializer(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )
            .unwrap_or_else(|| self.factory.ref_(self).create_void_zero())
        } else if is_parameter_property_declaration(
            property_original_node,
            property_original_node.ref_(self).parent(),
            self,
        ) && is_identifier(&property_name.ref_(self))
        {
            property_name
        } else {
            self.factory.ref_(self).create_void_zero()
        };

        Some(if emit_assignment || is_private_identifier(&property_name.ref_(self)) {
            let member_access = create_member_access_for_property_name(
                &self.factory.ref_(self),
                receiver,
                property_name,
                Some(&*property_name.ref_(self)),
            );
            self.factory.ref_(self).create_assignment(member_access, initializer)
        } else {
            let name = if is_computed_property_name(&property_name.ref_(self)) {
                property_name.ref_(self).as_computed_property_name().expression
            } else if is_identifier(&property_name.ref_(self)) {
                self.factory.ref_(self).create_string_literal(
                    unescape_leading_underscores(&property_name.ref_(self).as_identifier().escaped_text)
                        .to_owned(),
                    None,
                    None,
                )
            } else {
                property_name
            };
            let descriptor = self.factory.ref_(self).create_property_descriptor(
                PropertyDescriptorAttributesBuilder::default()
                    .value(initializer)
                    .configurable(true)
                    .writable(true)
                    .enumerable(true)
                    .build()
                    .unwrap(),
                None,
            );
            self.factory.ref_(self).create_object_define_property_call(
                receiver,
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

            self.context.ref_(self).enable_substitution(SyntaxKind::Identifier);

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

            self.context.ref_(self).enable_substitution(SyntaxKind::ThisKeyword);

            self.context
                .ref_(self).enable_emit_notification(SyntaxKind::FunctionDeclaration);
            self.context
                .ref_(self).enable_emit_notification(SyntaxKind::FunctionExpression);
            self.context
                .ref_(self).enable_emit_notification(SyntaxKind::Constructor);

            self.context
                .ref_(self).enable_emit_notification(SyntaxKind::GetAccessor);
            self.context
                .ref_(self).enable_emit_notification(SyntaxKind::SetAccessor);
            self.context
                .ref_(self).enable_emit_notification(SyntaxKind::MethodDeclaration);
            self.context
                .ref_(self).enable_emit_notification(SyntaxKind::PropertyDeclaration);

            self.context
                .ref_(self).enable_emit_notification(SyntaxKind::ComputedPropertyName);
        }
    }

    pub(super) fn add_method_statements(
        &self,
        statements: &mut Vec<Id<Node /*Statement*/>>,
        methods: &[Id<Node /*MethodDeclaration | AccessorDeclaration*/>],
        receiver: Id<Node>, /*LeftHandSideExpression*/
    ) {
        if !self.should_transform_private_elements_or_class_static_blocks || methods.is_empty() {
            return;
        }

        let private_identifier_environment = self.get_private_identifier_environment();
        let private_identifier_environment = (*private_identifier_environment).borrow();
        let weak_set_name = private_identifier_environment.weak_set_name;
        Debug_.assert(
            weak_set_name.is_some(),
            Some("weakSetName should be set in private identifier environment"),
        );
        let weak_set_name = weak_set_name.unwrap();
        statements.push(self.factory.ref_(self).create_expression_statement(
            create_private_instance_method_initializer(
                receiver,
                weak_set_name,
                self,
            ),
        ));
    }
}
