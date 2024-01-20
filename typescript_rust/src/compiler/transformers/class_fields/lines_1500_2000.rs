use gc::{Gc, GcCell, GcCellRefMut};
use id_arena::Id;

use super::{
    is_reserved_private_name, ClassFacts, ClassLexicalEnvironment, PrivateIdentifierAccessorInfo,
    PrivateIdentifierEnvironment, PrivateIdentifierInfo, PrivateIdentifierInfoInterface,
    PrivateIdentifierInstanceFieldInfo, PrivateIdentifierMethodInfo,
    PrivateIdentifierStaticFieldInfo, TransformClassFields,
};
use crate::{
    NamedDeclarationInterface, Node, _d, continue_if_none,
    get_initializer_of_binding_or_assignment_element, get_target_of_binding_or_assignment_element,
    get_text_of_property_name, has_static_modifier, is_accessor, is_assignment_expression,
    is_computed_property_name, is_element_access_expression, is_expression,
    is_generated_identifier, is_get_accessor, is_get_accessor_declaration, is_identifier,
    is_method_declaration, is_object_binding_or_assignment_element,
    is_private_identifier_property_access_expression, is_property_access_expression,
    is_property_assignment, is_property_declaration, is_property_name, is_set_accessor_declaration,
    is_shorthand_property_assignment, is_simple_copiable_expression,
    is_simple_inlineable_expression, is_spread_assignment, is_spread_element, is_super_property,
    is_this_property, skip_partially_emitted_expressions, visit_each_child, visit_node, Debug_,
    GeneratedIdentifierFlags, GetOrInsertDefault, NodeArray, NodeCheckFlags, PrivateIdentifierKind,
    SyntaxKind, VisitResult,
    InArena,
};

impl TransformClassFields {
    pub(super) fn visit_invalid_super_property(
        &self,
        node: Id<Node>, /*SuperProperty*/
    ) -> Id<Node> {
        if is_property_access_expression(&node.ref_(self)) {
            self.factory.update_property_access_expression(
                node,
                self.factory.create_void_zero(),
                node.ref_(self).as_property_access_expression().name(),
            )
        } else {
            self.factory.update_element_access_expression(
                node,
                self.factory.create_void_zero(),
                visit_node(
                    node.ref_(self).as_element_access_expression().argument_expression,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
            )
        }
    }

    pub(super) fn get_property_name_expression_if_needed(
        &self,
        name: Id<Node>, /*PropertyName*/
        should_hoist: bool,
    ) -> Option<Id<Node /*Expression*/>> {
        if is_computed_property_name(&name.ref_(self)) {
            let expression = visit_node(
                name.ref_(self).as_computed_property_name().expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            );
            let inner_expression = skip_partially_emitted_expressions(expression, self);
            let inlinable = is_simple_inlineable_expression(&inner_expression.ref_(self));
            let already_transformed = is_assignment_expression(inner_expression, None, self)
                && is_generated_identifier(&inner_expression.ref_(self).as_binary_expression().left.ref_(self));
            if !already_transformed && !inlinable && should_hoist {
                let generated_name = self.factory.get_generated_name_for_node(Some(name), None);
                if self
                    .resolver
                    .get_node_check_flags(name)
                    .intersects(NodeCheckFlags::BlockScopedBindingInLoop)
                {
                    self.context.add_block_scoped_variable(generated_name);
                } else {
                    self.context.hoist_variable_declaration(generated_name);
                }
                return Some(self.factory.create_assignment(generated_name, expression));
            }
            return (!(inlinable || is_identifier(&inner_expression.ref_(self)))).then_some(expression);
        }
        None
    }

    pub(super) fn start_class_lexical_environment(&self) {
        self.class_lexical_environment_stack_mut()
            .push(self.maybe_current_class_lexical_environment());
        self.set_current_class_lexical_environment(None);
    }

    pub(super) fn end_class_lexical_environment(&self) {
        self.set_current_class_lexical_environment(
            self.class_lexical_environment_stack_mut().pop().flatten(),
        );
    }

    pub(super) fn get_class_lexical_environment(&self) -> Gc<GcCell<ClassLexicalEnvironment>> {
        self.maybe_current_class_lexical_environment()
            .get_or_insert_default_()
            .clone()
    }

    pub(super) fn get_private_identifier_environment(
        &self,
    ) -> Gc<GcCell<PrivateIdentifierEnvironment>> {
        let lex = self.get_class_lexical_environment();
        let ret = lex
            .borrow_mut()
            .private_identifier_environment
            .get_or_insert_default_()
            .clone();
        ret
    }

    pub(super) fn get_pending_expressions(
        &self,
    ) -> GcCellRefMut<Option<Vec<Id<Node>>>, Vec<Id<Node>>> {
        self.maybe_pending_expressions_mut()
            .get_or_insert_default_();
        self.pending_expressions_mut()
    }

    pub(super) fn add_private_identifier_to_environment(
        &self,
        node: Id<Node>, /*PrivateClassElementDeclaration*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_named_declaration = node_ref.as_named_declaration();
        let node_name = node_as_named_declaration.name();
        let node_name_ref = node_name.ref_(self);
        let node_name_as_private_identifier = node_name_ref.as_private_identifier();
        let text = get_text_of_property_name(node_name, self);
        let lex = self.get_class_lexical_environment();
        let lex = (*lex).borrow();
        let class_constructor = lex.class_constructor;

        let private_env = self.get_private_identifier_environment();
        let weak_set_name = (*private_env).borrow().weak_set_name.clone();

        let mut assignment_expressions: Vec<Id<Node /*Expression*/>> = _d();

        let private_name = &node_name_as_private_identifier.escaped_text;
        let previous_info = (*private_env)
            .borrow()
            .identifiers
            .get(private_name)
            .cloned();
        let is_valid = !is_reserved_private_name(node_name) && previous_info.is_none();

        if has_static_modifier(node, self) {
            Debug_.assert(
                class_constructor.is_some(),
                // TODO: this message looks copy/pasted inaccurate, upstream?
                Some("weakSetName should be set in private identifier environment"),
            );
            let class_constructor = class_constructor.unwrap();
            if is_property_declaration(&node.ref_(self)) {
                let variable_name = self.create_hoisted_variable_for_private_name(&text, node);
                private_env.borrow_mut().identifiers.insert(
                    private_name.clone(),
                    PrivateIdentifierStaticFieldInfo::new(
                        class_constructor.clone(),
                        is_valid,
                        variable_name,
                    )
                    .into(),
                );
            } else if is_method_declaration(&node.ref_(self)) {
                let function_name = self.create_hoisted_variable_for_private_name(&text, node);
                private_env.borrow_mut().identifiers.insert(
                    private_name.clone(),
                    PrivateIdentifierMethodInfo::new(
                        class_constructor.clone(),
                        true,
                        is_valid,
                        function_name,
                    )
                    .into(),
                );
            } else if is_get_accessor_declaration(&node.ref_(self)) {
                let getter_name =
                    self.create_hoisted_variable_for_private_name(&format!("{text}_get"), node);
                if let Some(previous_info) = previous_info.as_ref().filter(|previous_info| {
                    let previous_info = (**previous_info).borrow();
                    previous_info.kind() == PrivateIdentifierKind::Accessor
                        && previous_info.is_static()
                        && previous_info
                            .as_private_identifier_accessor_info()
                            .getter_name
                            .is_none()
                }) {
                    previous_info
                        .borrow_mut()
                        .as_private_identifier_accessor_info_mut()
                        .getter_name = Some(getter_name);
                } else {
                    private_env.borrow_mut().identifiers.insert(
                        private_name.clone(),
                        PrivateIdentifierAccessorInfo::new(
                            class_constructor.clone(),
                            true,
                            is_valid,
                            Some(getter_name),
                            None,
                        )
                        .into(),
                    );
                }
            } else if is_set_accessor_declaration(&node.ref_(self)) {
                let setter_name =
                    self.create_hoisted_variable_for_private_name(&format!("{text}_set"), node);
                if let Some(previous_info) = previous_info.as_ref().filter(|previous_info| {
                    let previous_info = (**previous_info).borrow();
                    previous_info.kind() == PrivateIdentifierKind::Accessor
                        && previous_info.is_static()
                        && previous_info
                            .as_private_identifier_accessor_info()
                            .setter_name
                            .is_none()
                }) {
                    previous_info
                        .borrow_mut()
                        .as_private_identifier_accessor_info_mut()
                        .setter_name = Some(setter_name);
                } else {
                    private_env.borrow_mut().identifiers.insert(
                        private_name.clone(),
                        PrivateIdentifierAccessorInfo::new(
                            class_constructor.clone(),
                            true,
                            is_valid,
                            None,
                            Some(setter_name),
                        )
                        .into(),
                    );
                }
            } else {
                Debug_.assert_never(node, Some("Unknown class element type."));
            }
        } else if is_property_declaration(&node.ref_(self)) {
            let weak_map_name = self.create_hoisted_variable_for_private_name(&text, node);
            private_env.borrow_mut().identifiers.insert(
                private_name.clone(),
                PrivateIdentifierInstanceFieldInfo::new(weak_map_name.clone(), is_valid).into(),
            );

            assignment_expressions.push(self.factory.create_assignment(
                weak_map_name,
                self.factory.create_new_expression(
                    self.factory.create_identifier("WeakMap"),
                    Option::<Gc<NodeArray>>::None,
                    Some(vec![]),
                ),
            ));
        } else if is_method_declaration(&node.ref_(self)) {
            Debug_.assert(
                weak_set_name.is_some(),
                Some("weakSetName should be set in private identifier environment"),
            );
            let weak_set_name = weak_set_name.as_ref().unwrap();

            private_env.borrow_mut().identifiers.insert(
                private_name.clone(),
                PrivateIdentifierMethodInfo::new(
                    weak_set_name.clone(),
                    false,
                    is_valid,
                    self.create_hoisted_variable_for_private_name(&text, node),
                )
                .into(),
            );
        } else if is_accessor(&node.ref_(self)) {
            Debug_.assert(
                weak_set_name.is_some(),
                Some("weakSetName should be set in private identifier environment"),
            );
            let weak_set_name = weak_set_name.as_ref().unwrap();

            if is_get_accessor(&node.ref_(self)) {
                let getter_name =
                    self.create_hoisted_variable_for_private_name(&format!("{text}_get"), node);
                if let Some(previous_info) = previous_info.as_ref().filter(|previous_info| {
                    let previous_info = (**previous_info).borrow();
                    previous_info.kind() == PrivateIdentifierKind::Accessor
                        && !previous_info.is_static()
                        && previous_info
                            .as_private_identifier_accessor_info()
                            .getter_name
                            .is_none()
                }) {
                    previous_info
                        .borrow_mut()
                        .as_private_identifier_accessor_info_mut()
                        .getter_name = Some(getter_name);
                } else {
                    private_env.borrow_mut().identifiers.insert(
                        private_name.clone(),
                        PrivateIdentifierAccessorInfo::new(
                            weak_set_name.clone(),
                            false,
                            is_valid,
                            Some(getter_name),
                            None,
                        )
                        .into(),
                    );
                }
            } else {
                let setter_name =
                    self.create_hoisted_variable_for_private_name(&format!("{text}_set"), node);
                if let Some(previous_info) = previous_info.as_ref().filter(|previous_info| {
                    let previous_info = (**previous_info).borrow();
                    previous_info.kind() == PrivateIdentifierKind::Accessor
                        && !previous_info.is_static()
                        && previous_info
                            .as_private_identifier_accessor_info()
                            .setter_name
                            .is_none()
                }) {
                    previous_info
                        .borrow_mut()
                        .as_private_identifier_accessor_info_mut()
                        .setter_name = Some(setter_name);
                } else {
                    private_env.borrow_mut().identifiers.insert(
                        private_name.clone(),
                        PrivateIdentifierAccessorInfo::new(
                            weak_set_name.clone(),
                            false,
                            is_valid,
                            None,
                            Some(setter_name),
                        )
                        .into(),
                    );
                }
            }
        } else {
            Debug_.assert_never(node, Some("Unknown class element type."));
        }

        self.get_pending_expressions()
            .extend(assignment_expressions);
    }

    pub(super) fn create_hoisted_variable_for_class(
        &self,
        name: &str,
        node: Id<Node>, /*PrivateIdentifier | ClassStaticBlockDeclaration*/
    ) -> Id<Node /*Identifier*/> {
        let private_identifier_environment = self.get_private_identifier_environment();
        let private_identifier_environment = (*private_identifier_environment).borrow();
        let class_name = &private_identifier_environment.class_name;
        let prefix = if !class_name.is_empty() {
            format!("_{class_name}")
        } else {
            "".to_owned()
        };
        let identifier = self.factory.create_unique_name(
            &format!("{prefix}_{name}"),
            Some(GeneratedIdentifierFlags::Optimistic),
        );

        if self
            .resolver
            .get_node_check_flags(node)
            .intersects(NodeCheckFlags::BlockScopedBindingInLoop)
        {
            self.context.add_block_scoped_variable(identifier);
        } else {
            self.context.hoist_variable_declaration(identifier);
        }

        identifier
    }

    pub(super) fn create_hoisted_variable_for_private_name(
        &self,
        private_name: &str,
        node: Id<Node>, /*PrivateClassElementDeclaration*/
    ) -> Id<Node /*Identifier*/> {
        self.create_hoisted_variable_for_class(
            &private_name[1..],
            node.ref_(self).as_named_declaration().name(),
        )
    }

    pub(super) fn access_private_identifier(
        &self,
        name: Id<Node>, /*PrivateIdentifier*/
    ) -> Option<Gc<GcCell<PrivateIdentifierInfo>>> {
        let name_ref = name.ref_(self);
        let name_as_private_identifier = name_ref.as_private_identifier();
        if let Some(current_class_lexical_environment_private_identifier_environment) = self
            .maybe_current_class_lexical_environment()
            .and_then(|current_class_lexical_environment| {
                (*current_class_lexical_environment)
                    .borrow()
                    .private_identifier_environment
                    .clone()
            })
        {
            let current_class_lexical_environment_private_identifier_environment =
                (*current_class_lexical_environment_private_identifier_environment).borrow();
            let info = current_class_lexical_environment_private_identifier_environment
                .identifiers
                .get(&name_as_private_identifier.escaped_text);
            if let Some(info) = info {
                return Some(info.clone());
            }
        }
        for env in self.class_lexical_environment_stack().iter().rev() {
            let env = continue_if_none!(env);
            let info = (**env)
                .borrow()
                .private_identifier_environment
                .as_ref()
                .and_then(|env_private_identifier_environment| {
                    (**env_private_identifier_environment)
                        .borrow()
                        .identifiers
                        .get(&name_as_private_identifier.escaped_text)
                        .cloned()
                });
            if info.is_some() {
                return info;
            }
        }
        None
    }

    pub(super) fn wrap_private_identifier_for_destructuring_target(
        &self,
        node: Id<Node>, /*PrivateIdentifierPropertyAccessExpression*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_property_access_expression = node_ref.as_property_access_expression();
        let parameter = self.factory.get_generated_name_for_node(Some(node), None);
        let info = self.access_private_identifier(node_as_property_access_expression.name());
        if info.is_none() {
            return visit_each_child(&node.ref_(self), |node: Id<Node>| self.visitor(node), &**self.context);
        }
        let info = info.unwrap();
        let mut receiver = node_as_property_access_expression.expression;
        if is_this_property(node, self)
            || is_super_property(node, self)
            || !is_simple_copiable_expression(&node_as_property_access_expression.expression)
        {
            receiver = self.factory.create_temp_variable(
                Some(|node: Id<Node>| {
                    self.context.hoist_variable_declaration(node);
                }),
                Some(true),
            );
            self.get_pending_expressions()
                .push(self.factory.create_binary_expression(
                    receiver.clone(),
                    SyntaxKind::EqualsToken,
                    visit_node(
                        &node_as_property_access_expression.expression,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    ),
                ));
        }
        let ret = self.factory.create_assignment_target_wrapper(
            parameter,
            self.create_private_identifier_assignment(
                &(*info).borrow(),
                receiver,
                parameter,
                SyntaxKind::EqualsToken,
            ),
        );
        ret
    }

    pub(super) fn visit_array_assignment_target(
        &self,
        node: Id<Node>, /*BindingOrAssignmentElement*/
    ) -> VisitResult {
        let target = get_target_of_binding_or_assignment_element(node, self);
        if let Some(target) = target {
            let mut wrapped: Option<Id<Node /*LeftHandSideExpression*/>> = _d();
            if is_private_identifier_property_access_expression(target, self) {
                wrapped = Some(self.wrap_private_identifier_for_destructuring_target(target));
            } else if self.should_transform_super_in_static_initializers
                && is_super_property(target, self)
                && self
                    .maybe_current_static_property_declaration_or_static_block()
                    .is_some()
            {
                if let Some(current_class_lexical_environment) =
                    self.maybe_current_class_lexical_environment()
                {
                    let current_class_lexical_environment =
                        (*current_class_lexical_environment).borrow();
                    let class_constructor =
                        current_class_lexical_environment.class_constructor.as_ref();
                    let super_class_reference = current_class_lexical_environment
                        .super_class_reference
                        .as_ref();
                    let facts = current_class_lexical_environment.facts;
                    if facts.intersects(ClassFacts::ClassWasDecorated) {
                        wrapped = Some(self.visit_invalid_super_property(target));
                    } else if let (Some(class_constructor), Some(super_class_reference)) =
                        (class_constructor, super_class_reference)
                    {
                        let name = if is_element_access_expression(&target.ref_(self)) {
                            Some(visit_node(
                                target.ref_(self).as_element_access_expression().argument_expression,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node: Id<Node>| is_expression(node, self)),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ))
                        } else if is_identifier(&target.ref_(self).as_property_access_expression().name().ref_(self)) {
                            Some(self.factory.create_string_literal_from_node(
                                target.ref_(self).as_property_access_expression().name(),
                            ))
                        } else {
                            None
                        };
                        if let Some(name) = name {
                            let temp = self
                                .factory
                                .create_temp_variable(Option::<fn(Id<Node>)>::None, None);
                            wrapped = Some(self.factory.create_assignment_target_wrapper(
                                temp.clone(),
                                self.factory.create_reflect_set_call(
                                    super_class_reference.clone(),
                                    name,
                                    temp,
                                    Some(class_constructor.clone()),
                                ),
                            ));
                        }
                    }
                }
            }
            if let Some(wrapped) = wrapped {
                return Some(
                    if is_assignment_expression(node, None, self) {
                        let node_ref = node.ref_(self);
                        let node_as_binary_expression = node_ref.as_binary_expression();
                        self.factory.update_binary_expression(
                            node,
                            wrapped,
                            node_as_binary_expression.operator_token.clone(),
                            visit_node(
                                &node_as_binary_expression.right,
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node| is_expression(node, self)),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ),
                        )
                    } else if is_spread_element(&node.ref_(self)) {
                        self.factory.update_spread_element(node, wrapped)
                    } else {
                        wrapped
                    }
                    .into(),
                );
            }
        }
        Some(
            visit_node(
                node,
                Some(|node: Id<Node>| self.visitor_destructuring_target(node)),
                Option::<fn(Id<Node>) -> bool>::None,
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )
            .into(),
        )
    }

    pub(super) fn visit_object_assignment_target(
        &self,
        node: Id<Node>, /*ObjectLiteralElementLike*/
    ) -> VisitResult {
        if is_object_binding_or_assignment_element(&node.ref_(self)) && !is_shorthand_property_assignment(&node.ref_(self))
        {
            let target = get_target_of_binding_or_assignment_element(node, self);
            let mut wrapped: Option<Id<Node /*LeftHandSideExpression*/>> = _d();
            if let Some(target) = target {
                if is_private_identifier_property_access_expression(target, self) {
                    wrapped = Some(self.wrap_private_identifier_for_destructuring_target(target));
                } else if self.should_transform_super_in_static_initializers
                    && is_super_property(target, self)
                    && self
                        .maybe_current_static_property_declaration_or_static_block()
                        .is_some()
                {
                    if let Some(current_class_lexical_environment) =
                        self.maybe_current_class_lexical_environment()
                    {
                        let current_class_lexical_environment =
                            (*current_class_lexical_environment).borrow();
                        let class_constructor =
                            current_class_lexical_environment.class_constructor.as_ref();
                        let super_class_reference = current_class_lexical_environment
                            .super_class_reference
                            .as_ref();
                        let facts = current_class_lexical_environment.facts;
                        if facts.intersects(ClassFacts::ClassWasDecorated) {
                            wrapped = Some(self.visit_invalid_super_property(target));
                        } else if let (Some(class_constructor), Some(super_class_reference)) =
                            (class_constructor, super_class_reference)
                        {
                            let name = if is_element_access_expression(&target.ref_(self)) {
                                Some(visit_node(
                                    target.ref_(self).as_element_access_expression().argument_expression,
                                    Some(|node: Id<Node>| self.visitor(node)),
                                    Some(|node| is_expression(node, self)),
                                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                ))
                            } else if is_identifier(&target.ref_(self).as_property_access_expression().name().ref_(self))
                            {
                                Some(self.factory.create_string_literal_from_node(
                                    target.ref_(self).as_property_access_expression().name(),
                                ))
                            } else {
                                None
                            };
                            if let Some(name) = name {
                                let temp = self
                                    .factory
                                    .create_temp_variable(Option::<fn(Id<Node>)>::None, None);
                                wrapped = Some(self.factory.create_assignment_target_wrapper(
                                    temp.clone(),
                                    self.factory.create_reflect_set_call(
                                        super_class_reference.clone(),
                                        name,
                                        temp,
                                        Some(class_constructor.clone()),
                                    ),
                                ));
                            }
                        }
                    }
                }
            }
            if is_property_assignment(&node.ref_(self)) {
                let node_as_property_assignment = node.ref_(self).as_property_assignment();
                let initializer = get_initializer_of_binding_or_assignment_element(node, self);
                return Some(
                    self.factory
                        .update_property_assignment(
                            node,
                            visit_node(
                                &node_as_property_assignment.name(),
                                Some(|node: Id<Node>| self.visitor(node)),
                                Some(|node: Id<Node>| is_property_name(&node.ref_(self))),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            ),
                            if let Some(wrapped) = wrapped {
                                if let Some(initializer) = initializer {
                                    self.factory.create_assignment(
                                        wrapped,
                                        visit_node(
                                            initializer,
                                            Some(|node: Id<Node>| self.visitor(node)),
                                            Option::<fn(Id<Node>) -> bool>::None,
                                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                        ),
                                    )
                                } else {
                                    wrapped
                                }
                            } else {
                                visit_node(
                                    &node_as_property_assignment.initializer,
                                    Some(|node: Id<Node>| self.visitor_destructuring_target(node)),
                                    Some(|node| is_expression(node, self)),
                                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                )
                            },
                        )
                        .into(),
                );
            }
            if is_spread_assignment(&node.ref_(self)) {
                let node_ref = node.ref_(self);
                let node_as_spread_assignment = node_ref.as_spread_assignment();
                return Some(
                    self.factory
                        .update_spread_assignment(
                            node,
                            wrapped.unwrap_or_else(|| {
                                visit_node(
                                    &node_as_spread_assignment.expression,
                                    Some(|node: Id<Node>| self.visitor_destructuring_target(node)),
                                    Some(|node| is_expression(node, self)),
                                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                                )
                            }),
                        )
                        .into(),
                );
            }
            Debug_.assert(
                wrapped.is_none(),
                Some("Should not have generated a wrapped target"),
            );
        }
        Some(
            visit_node(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                Option::<fn(Id<Node>) -> bool>::None,
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )
            .into(),
        )
    }
}
