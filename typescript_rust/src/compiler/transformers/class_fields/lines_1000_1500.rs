use gc::Gc;

use super::{ClassFacts, TransformClassFields};
use crate::{
    get_static_properties_and_class_static_block, InterfaceOrClassLikeDeclarationInterface,
    Matches, NamedDeclarationInterface, Node, NodeInterface, NonEmpty, VisitResult, _d, add_range,
    get_effective_base_type_node, get_emit_flags, get_original_node_id,
    is_class_static_block_declaration, is_decorator, is_heritage_clause, is_private_identifier,
    maybe_map, maybe_visit_nodes, set_emit_flags, skip_outer_expressions, Debug_, EmitFlags,
    GeneratedIdentifierFlags, HasInitializerInterface, NodeArray, NodeCheckFlags, NodeExt,
    SyntaxKind,
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
        _node: &Node, /*ClassStaticBlockDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn transform_class_members(
        &self,
        _node: &Node, /*ClassDeclaration | ClassExpression*/
        _is_derived_class: bool,
    ) -> Gc<NodeArray> {
        unimplemented!()
    }

    pub(super) fn add_property_or_class_static_block_statements(
        &self,
        _statements: &mut Vec<Gc<Node /*Statement*/>>,
        _properties: &[Gc<Node /*PropertyDeclaration | ClassStaticBlockDeclaration*/>],
        _receiver: &Node, /*LeftHandSideExpression*/
    ) {
        unimplemented!()
    }

    pub(super) fn generate_initialized_property_expressions_or_class_static_block(
        &self,
        _properties_or_class_static_blocks: &[Gc<
            Node, /*PropertyDeclaration | ClassStaticBlockDeclaration*/
        >],
        _receiver: &Node, /*LeftHandSideExpression*/
    ) -> Vec<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn enable_substitution_for_class_aliases(&self) {
        unimplemented!()
    }

    pub(super) fn enable_substitution_for_class_static_this_or_super_reference(&self) {
        unimplemented!()
    }
}
