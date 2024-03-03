use std::io;

use id_arena::Id;

use super::MappedTypeModifiers;
use crate::{
    add_related_info, create_diagnostic_for_node, filter, find_ancestor, for_each,
    get_class_extends_heritage_element, get_combined_modifier_flags,
    get_declaration_modifier_flags_from_symbol, get_declaration_of_kind,
    get_effective_constraint_of_type_parameter, get_effective_modifier_flags,
    get_emit_script_target, get_name_of_declaration, get_object_flags, has_effective_modifier,
    has_question_token, has_syntactic_modifier, is_assignment_target, is_global_scope_augmentation,
    is_in_js_file, is_in_jsdoc, is_module_block, is_module_declaration, is_named_tuple_member,
    is_private_identifier_class_element_declaration, is_prologue_directive, is_static,
    is_super_call, is_type_reference_type, node_is_missing, node_is_present, released,
    return_ok_default_if_none, some, symbol_name, try_cast, try_for_each, try_for_each_child,
    try_maybe_for_each, try_maybe_map, unescape_leading_underscores, DiagnosticRelatedInformation,
    Diagnostics, ElementFlags, FunctionLikeDeclarationInterface, HasArena, HasInitializerInterface,
    InArena, ModifierFlags, Node, NodeArray, NodeCheckFlags, NodeFlags, NodeInterface, ObjectFlags,
    OptionInArena, ReadonlyTextRange, ScriptTarget, SignatureDeclarationInterface, Symbol,
    SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
    TypeMapper,
};

impl TypeChecker {
    pub(super) fn check_class_static_block_declaration(
        &self,
        node: Id<Node>, /*ClassStaticBlockDeclaration*/
    ) -> io::Result<()> {
        self.check_grammar_decorators_and_modifiers(node);

        try_for_each_child(
            node,
            |child: Id<Node>| self.check_source_element(Some(child)),
            Option::<fn(Id<NodeArray>) -> io::Result<()>>::None,
            self,
        )?;

        Ok(())
    }

    pub(super) fn check_constructor_declaration(
        &self,
        node: Id<Node>, /*ConstructorDeclaration*/
    ) -> io::Result<()> {
        self.check_signature_declaration(node)?;
        if !self.check_grammar_constructor_type_parameters(node) {
            self.check_grammar_constructor_type_annotation(node);
        }

        self.check_source_element(released!(node
            .ref_(self)
            .as_constructor_declaration()
            .maybe_body()))?;

        let symbol = self.get_symbol_of_node(node)?.unwrap();
        let first_declaration = get_declaration_of_kind(symbol, node.ref_(self).kind(), self);

        if first_declaration == Some(node) {
            self.check_function_or_constructor_symbol(symbol)?;
        }

        if node_is_missing(
            node.ref_(self)
                .as_constructor_declaration()
                .maybe_body()
                .refed(self)
                .as_deref(),
        ) {
            return Ok(());
        }
        let node_body = node
            .ref_(self)
            .as_constructor_declaration()
            .maybe_body()
            .unwrap();

        if !self.produce_diagnostics {
            return Ok(());
        }

        let containing_class_decl = node.ref_(self).parent();
        if get_class_extends_heritage_element(containing_class_decl, self).is_some() {
            self.capture_lexical_this(node.ref_(self).parent(), containing_class_decl);
            let class_extends_null = self.class_declaration_extends_null(containing_class_decl)?;
            let super_call = self.find_first_super_call(node_body);
            if let Some(super_call) = super_call {
                if class_extends_null {
                    self.error(
                        Some(super_call),
                        &Diagnostics::A_constructor_cannot_contain_a_super_call_when_its_class_extends_null,
                        None,
                    );
                }

                let super_call_should_be_first = (get_emit_script_target(
                    &self.compiler_options.ref_(self),
                ) != ScriptTarget::ESNext
                    || !self.use_define_for_class_fields)
                    && (some(
                        Some(
                            &*node
                                .ref_(self)
                                .parent()
                                .ref_(self)
                                .as_class_like_declaration()
                                .members()
                                .ref_(self),
                        ),
                        Some(|&member: &Id<Node>| {
                            self.is_instance_property_with_initializer_or_private_identifier_property(member)
                        }),
                    ) || some(
                        Some(
                            &*node
                                .ref_(self)
                                .as_constructor_declaration()
                                .parameters()
                                .ref_(self),
                        ),
                        Some(|&p: &Id<Node>| {
                            has_syntactic_modifier(
                                p,
                                ModifierFlags::ParameterPropertyModifier,
                                self,
                            )
                        }),
                    ));

                if super_call_should_be_first {
                    let node_body_ref = node_body.ref_(self);
                    let statements = &node_body_ref.as_block().statements;
                    let mut super_call_statement: Option<Id<Node /*ExpressionStatement*/>> = None;

                    for &statement in &*statements.ref_(self) {
                        if statement.ref_(self).kind() == SyntaxKind::ExpressionStatement
                            && is_super_call(
                                statement.ref_(self).as_expression_statement().expression,
                                self,
                            )
                        {
                            super_call_statement = Some(statement);
                            break;
                        }
                        if !is_prologue_directive(statement, self) {
                            break;
                        }
                    }
                    if super_call_statement.is_none() {
                        self.error(
                            Some(node),
                            &Diagnostics::A_super_call_must_be_the_first_statement_in_the_constructor_when_a_class_contains_initialized_properties_parameter_properties_or_private_identifiers,
                            None,
                        );
                    }
                }
            } else if !class_extends_null {
                self.error(
                    Some(node),
                    &Diagnostics::Constructors_for_derived_classes_must_contain_a_super_call,
                    None,
                );
            }
        }

        Ok(())
    }

    pub(super) fn is_instance_property_with_initializer_or_private_identifier_property(
        &self,
        n: Id<Node>,
    ) -> bool {
        if is_private_identifier_class_element_declaration(n, self) {
            return true;
        }
        n.ref_(self).kind() == SyntaxKind::PropertyDeclaration
            && !is_static(n, self)
            && n.ref_(self)
                .as_property_declaration()
                .maybe_initializer()
                .is_some()
    }

    pub(super) fn check_accessor_declaration(
        &self,
        node: Id<Node>, /*AccessorDeclaration*/
    ) -> io::Result<()> {
        if self.produce_diagnostics {
            if !self.check_grammar_function_like_declaration(node)?
                && !self.check_grammar_accessor(node)
            {
                self.check_grammar_computed_property_name(
                    node.ref_(self).as_function_like_declaration().name(),
                );
            }

            self.check_decorators(node)?;
            self.check_signature_declaration(node)?;
            if node.ref_(self).kind() == SyntaxKind::GetAccessor {
                if !node.ref_(self).flags().intersects(NodeFlags::Ambient)
                    && node_is_present(
                        node.ref_(self)
                            .as_function_like_declaration()
                            .maybe_body()
                            .refed(self)
                            .as_deref(),
                    )
                    && node
                        .ref_(self)
                        .flags()
                        .intersects(NodeFlags::HasImplicitReturn)
                {
                    if !node
                        .ref_(self)
                        .flags()
                        .intersects(NodeFlags::HasExplicitReturn)
                    {
                        self.error(
                            node.ref_(self).as_function_like_declaration().maybe_name(),
                            &Diagnostics::A_get_accessor_must_return_a_value,
                            None,
                        );
                    }
                }
            }
            if node
                .ref_(self)
                .as_function_like_declaration()
                .name()
                .ref_(self)
                .kind()
                == SyntaxKind::ComputedPropertyName
            {
                self.check_computed_property_name(
                    node.ref_(self).as_function_like_declaration().name(),
                )?;
            }

            if self.has_bindable_name(node)? {
                let symbol = self.get_symbol_of_node(node)?.unwrap();
                let getter = get_declaration_of_kind(symbol, SyntaxKind::GetAccessor, self);
                let setter = get_declaration_of_kind(symbol, SyntaxKind::SetAccessor, self);
                if let Some(getter) = getter {
                    if let Some(setter) = setter {
                        if !self
                            .get_node_check_flags(getter)
                            .intersects(NodeCheckFlags::TypeChecked)
                        {
                            self.get_node_links(getter).ref_mut(self).flags |=
                                NodeCheckFlags::TypeChecked;
                            let getter_flags = get_effective_modifier_flags(getter, self);
                            let setter_flags = get_effective_modifier_flags(setter, self);
                            if getter_flags & ModifierFlags::Abstract
                                != setter_flags & ModifierFlags::Abstract
                            {
                                self.error(
                                    getter.ref_(self).as_named_declaration().maybe_name(),
                                    &Diagnostics::Accessors_must_both_be_abstract_or_non_abstract,
                                    None,
                                );
                                self.error(
                                    setter.ref_(self).as_named_declaration().maybe_name(),
                                    &Diagnostics::Accessors_must_both_be_abstract_or_non_abstract,
                                    None,
                                );
                            }
                            if getter_flags.intersects(ModifierFlags::Protected)
                                && !setter_flags
                                    .intersects(ModifierFlags::Protected | ModifierFlags::Private)
                                || getter_flags.intersects(ModifierFlags::Private)
                                    && !setter_flags.intersects(ModifierFlags::Private)
                            {
                                self.error(
                                    getter.ref_(self).as_named_declaration().maybe_name(),
                                    &Diagnostics::A_get_accessor_must_be_at_least_as_accessible_as_the_setter,
                                    None,
                                );
                                self.error(
                                    setter.ref_(self).as_named_declaration().maybe_name(),
                                    &Diagnostics::A_get_accessor_must_be_at_least_as_accessible_as_the_setter,
                                    None,
                                );
                            }

                            let getter_type = self.get_annotated_accessor_type(Some(getter))?;
                            let setter_type = self.get_annotated_accessor_type(Some(setter))?;
                            if let (Some(getter_type), Some(setter_type)) =
                                (getter_type, setter_type)
                            {
                                self.check_type_assignable_to(
                                    getter_type,
                                    setter_type,
                                    Some(getter),
                                    Some(&Diagnostics::The_return_type_of_a_get_accessor_must_be_assignable_to_its_set_accessor_type),
                                    None, None,
                                )?;
                            }
                        }
                    }
                }
            }
            let return_type =
                self.get_type_of_accessors(self.get_symbol_of_node(node)?.unwrap())?;
            if node.ref_(self).kind() == SyntaxKind::GetAccessor {
                self.check_all_code_paths_in_non_void_function_return_or_throw(
                    node,
                    Some(return_type),
                )?;
            }
        }
        self.check_source_element(released!(node
            .ref_(self)
            .as_function_like_declaration()
            .maybe_body()))?;
        self.set_node_links_for_private_identifier_scope(node);

        Ok(())
    }

    pub(super) fn check_missing_declaration(&self, node: Id<Node>) -> io::Result<()> {
        self.check_decorators(node)?;

        Ok(())
    }

    pub(super) fn get_effective_type_arguments(
        &self,
        node: Id<Node>, /*TypeReferenceNode | ExpressionWithTypeArguments*/
        type_parameters: Option<&[Id<Type /*TypeParameter*/>]>,
    ) -> io::Result<Vec<Id<Type>>> {
        Ok(self
            .fill_missing_type_arguments(
                try_maybe_map(
                    node.ref_(self)
                        .as_has_type_arguments()
                        .maybe_type_arguments()
                        .refed(self)
                        .as_deref(),
                    |&type_argument, _| self.get_type_from_type_node_(type_argument),
                )
                .transpose()?,
                type_parameters,
                self.get_min_type_argument_count(type_parameters),
                is_in_js_file(Some(&node.ref_(self))),
            )?
            .unwrap())
    }

    pub(super) fn check_type_argument_constraints(
        &self,
        node: Id<Node>, /*TypeReferenceNode | ExpressionWithTypeArguments*/
        type_parameters: &[Id<Type /*TypeParameter*/>],
    ) -> io::Result<bool> {
        let mut type_arguments: Option<Vec<Id<Type>>> = None;
        let mut mapper: Option<Id<TypeMapper>> = None;
        let mut result = true;
        for i in 0..type_parameters.len() {
            let constraint = self.get_constraint_of_type_parameter(type_parameters[i])?;
            if let Some(constraint) = constraint {
                if type_arguments.is_none() {
                    type_arguments =
                        Some(self.get_effective_type_arguments(node, Some(type_parameters))?);
                    mapper = Some(
                        self.create_type_mapper(type_parameters.to_owned(), type_arguments.clone()),
                    );
                }
                result = result
                    && self.check_type_assignable_to(
                        type_arguments.as_ref().unwrap()[i],
                        self.instantiate_type(constraint, mapper.clone())?,
                        released!(node
                            .ref_(self)
                            .as_has_type_arguments()
                            .maybe_type_arguments()
                            .as_ref()
                            .unwrap()
                            .ref_(self)
                            .get(i)
                            .copied()),
                        Some(&Diagnostics::Type_0_does_not_satisfy_the_constraint_1),
                        None,
                        None,
                    )?;
            }
        }
        Ok(result)
    }

    pub(super) fn get_type_parameters_for_type_reference(
        &self,
        node: Id<Node>, /*TypeReferenceNode | ExpressionWithTypeArguments*/
    ) -> io::Result<Option<Vec<Id<Type>>>> {
        let type_ = self.get_type_from_type_reference(node)?;
        if !self.is_error_type(type_) {
            let symbol = self.get_node_links(node).ref_(self).resolved_symbol.clone();
            if let Some(symbol) = symbol {
                return Ok(
                    if symbol.ref_(self).flags().intersects(SymbolFlags::TypeAlias) {
                        self.get_symbol_links(symbol)
                            .ref_(self)
                            .type_parameters
                            .clone()
                    } else {
                        None
                    }
                    .or_else(|| {
                        if get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::Reference) {
                            type_
                                .ref_(self)
                                .as_type_reference_interface()
                                .target()
                                .ref_(self)
                                .as_interface_type_interface()
                                .maybe_local_type_parameters()
                                .map(ToOwned::to_owned)
                        } else {
                            None
                        }
                    }),
                );
            }
        }
        Ok(None)
    }

    pub(super) fn check_type_reference_node(
        &self,
        node: Id<Node>, /*TypeReferenceNode | ExpressionWithTypeArguments*/
    ) -> io::Result<()> {
        self.check_grammar_type_arguments(
            node,
            node.ref_(self)
                .as_has_type_arguments()
                .maybe_type_arguments(),
        );
        if node.ref_(self).kind() == SyntaxKind::TypeReference {
            if let Some(node_type_name_jsdoc_dot_pos) = node
                .ref_(self)
                .as_type_reference_node()
                .type_name
                .ref_(self)
                .as_has_jsdoc_dot_pos()
                .maybe_jsdoc_dot_pos()
            {
                if !is_in_js_file(Some(&node.ref_(self))) && !is_in_jsdoc(Some(&node.ref_(self))) {
                    self.grammar_error_at_pos(
                        node,
                        node_type_name_jsdoc_dot_pos,
                        1,
                        &Diagnostics::JSDoc_types_can_only_be_used_inside_documentation_comments,
                        None,
                    );
                }
            }
        }
        try_maybe_for_each(
            released!(node
                .ref_(self)
                .as_has_type_arguments()
                .maybe_type_arguments())
            .refed(self)
            .as_deref(),
            |&type_argument, _| -> io::Result<_> {
                self.check_source_element(Some(type_argument))?;
                Ok(Option::<()>::None)
            },
        )?;
        let type_ = self.get_type_from_type_reference(node)?;
        if !self.is_error_type(type_) {
            if node
                .ref_(self)
                .as_has_type_arguments()
                .maybe_type_arguments()
                .is_some()
                && self.produce_diagnostics
            {
                let type_parameters = self.get_type_parameters_for_type_reference(node)?;
                if let Some(type_parameters) = type_parameters.as_ref() {
                    self.check_type_argument_constraints(node, type_parameters)?;
                }
            }
            let symbol = self.get_node_links(node).ref_(self).resolved_symbol.clone();
            if let Some(symbol) = symbol {
                if some(
                    symbol.ref_(self).maybe_declarations().as_deref(),
                    Some(|&d: &Id<Node>| {
                        self.is_type_declaration(d)
                            && d.ref_(self).flags().intersects(NodeFlags::Deprecated)
                    }),
                ) {
                    self.add_deprecated_suggestion(
                        self.get_deprecated_suggestion_node(node),
                        symbol.ref_(self).maybe_declarations().as_ref().unwrap(),
                        symbol.ref_(self).escaped_name(),
                    );
                }
                if type_.ref_(self).flags().intersects(TypeFlags::Enum)
                    && symbol
                        .ref_(self)
                        .flags()
                        .intersects(SymbolFlags::EnumMember)
                {
                    self.error(
                        Some(node),
                        &Diagnostics::Enum_type_0_has_members_with_initializers_that_are_not_literals,
                        Some(vec![
                            self.type_to_string_(
                                type_,
                                Option::<Id<Node>>::None,
                                None, None,
                            )?
                        ])
                    );
                }
            }
        }

        Ok(())
    }

    pub(super) fn get_type_argument_constraint_(
        &self,
        node: Id<Node>, /*TypeNode*/
    ) -> io::Result<Option<Id<Type>>> {
        let type_reference_node =
            return_ok_default_if_none!(try_cast(node.ref_(self).parent(), |parent: &Id<Node>| {
                is_type_reference_type(&parent.ref_(self))
            }));
        let type_parameters = return_ok_default_if_none!(
            self.get_type_parameters_for_type_reference(type_reference_node)?
        );
        let constraint = return_ok_default_if_none!(self.get_constraint_of_type_parameter(
            type_parameters[type_reference_node
                .ref_(self)
                .as_has_type_arguments()
                .maybe_type_arguments()
                .as_ref()
                .unwrap()
                .ref_(self)
                .into_iter()
                .position(|&type_argument| type_argument == node)
                .unwrap()],
        )?);
        Ok(Some(self.instantiate_type(
            constraint,
            Some(self.create_type_mapper(
                type_parameters.clone(),
                Some(
                    self.get_effective_type_arguments(type_reference_node, Some(&type_parameters))?,
                ),
            )),
        )?))
    }

    pub(super) fn check_type_query(&self, node: Id<Node> /*TypeQueryNode*/) -> io::Result<()> {
        self.get_type_from_type_query_node(node)?;

        Ok(())
    }

    pub(super) fn check_type_literal(
        &self,
        node: Id<Node>, /*TypeLiteralNode*/
    ) -> io::Result<()> {
        try_for_each(
            &*released!(node
                .ref_(self)
                .as_type_literal_node()
                .members
                .ref_(self)
                .clone()),
            |&member: &Id<Node>, _| -> io::Result<Option<()>> {
                self.check_source_element(Some(member))?;
                Ok(None)
            },
        )?;
        if self.produce_diagnostics {
            let type_ =
                self.get_type_from_type_literal_or_function_or_constructor_type_node(node)?;
            self.check_index_constraints(type_, released!(type_.ref_(self).symbol()), None)?;
            self.check_type_for_duplicate_index_signatures(node)?;
            self.check_object_type_for_duplicate_declarations(node);
        }

        Ok(())
    }

    pub(super) fn check_array_type(&self, node: Id<Node> /*ArrayTypeNode*/) -> io::Result<()> {
        self.check_source_element(Some(released!(
            node.ref_(self).as_array_type_node().element_type
        )))?;

        Ok(())
    }

    pub(super) fn check_tuple_type(&self, node: Id<Node> /*TupleTypeNode*/) -> io::Result<()> {
        let element_types = node.ref_(self).as_tuple_type_node().elements;
        let mut seen_optional_element = false;
        let mut seen_rest_element = false;
        let has_named_element = some(
            Some(&*element_types.ref_(self)),
            Some(|element_type: &Id<Node>| is_named_tuple_member(&element_type.ref_(self))),
        );
        for &e in &*element_types.ref_(self) {
            if e.ref_(self).kind() != SyntaxKind::NamedTupleMember && has_named_element {
                self.grammar_error_on_node(
                    e,
                    &Diagnostics::Tuple_members_must_all_have_names_or_all_not_have_names,
                    None,
                );
                break;
            }
            let flags = self.get_tuple_element_flags(e);
            if flags.intersects(ElementFlags::Variadic) {
                let type_ = self
                    .get_type_from_type_node_(e.ref_(self).as_has_type().maybe_type().unwrap())?;
                if !self.is_array_like_type(type_)? {
                    self.error(
                        Some(e),
                        &Diagnostics::A_rest_element_type_must_be_an_array_type,
                        None,
                    );
                    break;
                }
                if self.is_array_type(type_)
                    || self.is_tuple_type(type_)
                        && type_
                            .ref_(self)
                            .as_type_reference_interface()
                            .target()
                            .ref_(self)
                            .as_tuple_type()
                            .combined_flags
                            .intersects(ElementFlags::Rest)
                {
                    seen_rest_element = true;
                }
            } else if flags.intersects(ElementFlags::Rest) {
                if seen_rest_element {
                    self.grammar_error_on_node(
                        e,
                        &Diagnostics::A_rest_element_cannot_follow_another_rest_element,
                        None,
                    );
                    break;
                }
                seen_rest_element = true;
            } else if flags.intersects(ElementFlags::Optional) {
                if seen_rest_element {
                    self.grammar_error_on_node(
                        e,
                        &Diagnostics::An_optional_element_cannot_follow_a_rest_element,
                        None,
                    );
                    break;
                }
                seen_optional_element = true;
            } else if seen_optional_element {
                self.grammar_error_on_node(
                    e,
                    &Diagnostics::A_required_element_cannot_follow_an_optional_element,
                    None,
                );
                break;
            }
        }
        try_for_each(
            &*node.ref_(self).as_tuple_type_node().elements.ref_(self),
            |&element: &Id<Node>, _| -> io::Result<Option<()>> {
                self.check_source_element(Some(element))?;
                Ok(None)
            },
        )?;
        self.get_type_from_type_node_(node)?;

        Ok(())
    }

    pub(super) fn check_union_or_intersection_type(
        &self,
        node: Id<Node>, /*UnionOrIntersectionTypeNode*/
    ) -> io::Result<()> {
        try_for_each(
            &*released!(node
                .ref_(self)
                .as_union_or_intersection_type_node()
                .types()
                .ref_(self)
                .clone()),
            |&type_, _| -> io::Result<_> {
                self.check_source_element(Some(type_))?;
                Ok(Option::<()>::None)
            },
        )?;
        self.get_type_from_type_node_(node)?;

        Ok(())
    }

    pub(super) fn check_indexed_access_index_type(
        &self,
        type_: Id<Type>,
        access_node: Id<Node>, /*IndexedAccessTypeNode | ElementAccessExpression*/
    ) -> io::Result<Id<Type>> {
        if !type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::IndexedAccess)
        {
            return Ok(type_);
        }
        let (object_type, index_type) = {
            let type_ = type_.ref_(self);
            let type_as_indexed_access_type = type_.as_indexed_access_type();
            (
                type_as_indexed_access_type.object_type,
                type_as_indexed_access_type.index_type,
            )
        };
        if self.is_type_assignable_to(
            index_type,
            self.get_index_type(object_type, Some(false), None)?,
        )? {
            if access_node.ref_(self).kind() == SyntaxKind::ElementAccessExpression
                && is_assignment_target(access_node, self)
                && get_object_flags(&object_type.ref_(self)).intersects(ObjectFlags::Mapped)
                && self
                    .get_mapped_type_modifiers(object_type)
                    .intersects(MappedTypeModifiers::IncludeReadonly)
            {
                self.error(
                    Some(access_node),
                    &Diagnostics::Index_signature_in_type_0_only_permits_reading,
                    Some(vec![self.type_to_string_(
                        object_type,
                        Option::<Id<Node>>::None,
                        None,
                        None,
                    )?]),
                );
            }
            return Ok(type_);
        }
        let apparent_object_type = self.get_apparent_type(object_type)?;
        if self
            .get_index_info_of_type_(apparent_object_type, self.number_type())?
            .is_some()
            && self.is_type_assignable_to_kind(index_type, TypeFlags::NumberLike, None)?
        {
            return Ok(type_);
        }
        if self.is_generic_object_type(object_type)? {
            let property_name = self.get_property_name_from_index(index_type, Some(access_node));
            if let Some(property_name) = property_name.as_ref() {
                let property_symbol = self
                    .try_for_each_type(apparent_object_type, |t: Id<Type>| {
                        self.get_property_of_type_(t, property_name, None)
                    })?;
                if matches!(
                    property_symbol,
                    Some(property_symbol) if get_declaration_modifier_flags_from_symbol(
                        property_symbol,
                        None,
                        self,
                    ).intersects(ModifierFlags::NonPublicAccessibilityModifier)
                ) {
                    self.error(
                        Some(access_node),
                        &Diagnostics::Private_or_protected_member_0_cannot_be_accessed_on_a_type_parameter,
                        Some(vec![
                            unescape_leading_underscores(property_name).to_owned()
                        ])
                    );
                    return Ok(self.error_type());
                }
            }
        }
        self.error(
            Some(access_node),
            &Diagnostics::Type_0_cannot_be_used_to_index_type_1,
            Some(vec![
                self.type_to_string_(index_type, Option::<Id<Node>>::None, None, None)?,
                self.type_to_string_(object_type, Option::<Id<Node>>::None, None, None)?,
            ]),
        );
        Ok(self.error_type())
    }

    pub(super) fn check_indexed_access_type(
        &self,
        node: Id<Node>, /*IndexedAccessTypeNode*/
    ) -> io::Result<()> {
        self.check_source_element(Some(released!(
            node.ref_(self).as_indexed_access_type_node().object_type
        )))?;
        self.check_source_element(Some(
            node.ref_(self).as_indexed_access_type_node().index_type,
        ))?;
        self.check_indexed_access_index_type(
            self.get_type_from_indexed_access_type_node(node)?,
            node,
        )?;

        Ok(())
    }

    pub(super) fn check_mapped_type(
        &self,
        node: Id<Node>, /*MappedTypeNode*/
    ) -> io::Result<()> {
        self.check_grammar_mapped_type(node);
        self.check_source_element(Some(node.ref_(self).as_mapped_type_node().type_parameter))?;
        self.check_source_element(node.ref_(self).as_mapped_type_node().name_type)?;
        self.check_source_element(node.ref_(self).as_mapped_type_node().type_)?;

        if node.ref_(self).as_mapped_type_node().type_.is_none() {
            self.report_implicit_any(node, self.any_type(), None)?;
        }

        let type_ = self.get_type_from_mapped_type_node(node)?;
        let name_type = self.get_name_type_from_mapped_type(type_)?;
        if let Some(name_type) = name_type {
            self.check_type_assignable_to(
                name_type,
                self.keyof_constraint_type(),
                node.ref_(self).as_mapped_type_node().name_type,
                None,
                None,
                None,
            )?;
        } else {
            let constraint_type = self.get_constraint_type_from_mapped_type(type_)?;
            self.check_type_assignable_to(
                constraint_type,
                self.keyof_constraint_type(),
                get_effective_constraint_of_type_parameter(
                    released!(node.ref_(self).as_mapped_type_node().type_parameter),
                    self,
                ),
                None,
                None,
                None,
            )?;
        }

        Ok(())
    }

    pub(super) fn check_grammar_mapped_type(&self, node: Id<Node> /*MappedTypeNode*/) -> bool {
        if let Some(node_members) = node
            .ref_(self)
            .as_mapped_type_node()
            .members
            .as_ref()
            .filter(|node_members| !node_members.ref_(self).is_empty())
        {
            return self.grammar_error_on_node(
                node_members.ref_(self)[0],
                &Diagnostics::A_mapped_type_may_not_declare_properties_or_methods,
                None,
            );
        }
        false
    }

    pub(super) fn check_this_type(&self, node: Id<Node> /*ThisTypeNode*/) -> io::Result<()> {
        self.get_type_from_this_type_node(node)?;

        Ok(())
    }

    pub(super) fn check_type_operator(
        &self,
        node: Id<Node>, /*TypeOperatorNode*/
    ) -> io::Result<()> {
        self.check_grammar_type_operator_node(node);
        self.check_source_element(Some(node.ref_(self).as_type_operator_node().type_))?;

        Ok(())
    }

    pub(super) fn check_conditional_type(
        &self,
        node: Id<Node>, /*ConditionalTypeNode*/
    ) -> io::Result<()> {
        try_for_each_child(
            node,
            |child: Id<Node>| self.check_source_element(Some(child)),
            Option::<fn(Id<NodeArray>) -> io::Result<()>>::None,
            self,
        )?;

        Ok(())
    }

    pub(super) fn check_infer_type(&self, node: Id<Node> /*InferTypeNode*/) -> io::Result<()> {
        if find_ancestor(
            Some(node),
            |n: Id<Node>| {
                matches!(
                    n.ref_(self).maybe_parent(),
                    Some(n_parent) if n_parent.ref_(self).kind() == SyntaxKind::ConditionalType &&
                        n_parent.ref_(self).as_conditional_type_node().extends_type == n
                )
            },
            self,
        )
        .is_none()
        {
            self.grammar_error_on_node(
                node,
                &Diagnostics::infer_declarations_are_only_permitted_in_the_extends_clause_of_a_conditional_type,
                None,
            );
        }
        self.check_source_element(Some(node.ref_(self).as_infer_type_node().type_parameter))?;
        self.register_for_unused_identifiers_check(node);

        Ok(())
    }

    pub(super) fn check_template_literal_type(
        &self,
        node: Id<Node>, /*TemplateLiteralTypeNode*/
    ) -> io::Result<()> {
        for span in &*released!(node
            .ref_(self)
            .as_template_literal_type_node()
            .template_spans
            .ref_(self)
            .clone())
        {
            self.check_source_element(Some(span.ref_(self).as_template_literal_type_span().type_))?;
            let type_ = self
                .get_type_from_type_node_(span.ref_(self).as_template_literal_type_span().type_)?;
            self.check_type_assignable_to(
                type_,
                self.template_constraint_type(),
                released!(Some(span.ref_(self).as_template_literal_type_span().type_)),
                None,
                None,
                None,
            )?;
        }
        self.get_type_from_type_node_(node)?;

        Ok(())
    }

    pub(super) fn check_import_type(
        &self,
        node: Id<Node>, /*ImportTypeNode*/
    ) -> io::Result<()> {
        self.check_source_element(Some(node.ref_(self).as_import_type_node().argument))?;
        self.get_type_from_type_node_(node)?;

        Ok(())
    }

    pub(super) fn check_named_tuple_member(
        &self,
        node: Id<Node>, /*NamedTupleMember*/
    ) -> io::Result<()> {
        if node
            .ref_(self)
            .as_named_tuple_member()
            .dot_dot_dot_token
            .is_some()
            && node
                .ref_(self)
                .as_named_tuple_member()
                .question_token
                .is_some()
        {
            self.grammar_error_on_node(
                node,
                &Diagnostics::A_tuple_member_cannot_be_both_optional_and_rest,
                None,
            );
        }
        if node
            .ref_(self)
            .as_named_tuple_member()
            .type_
            .ref_(self)
            .kind()
            == SyntaxKind::OptionalType
        {
            self.grammar_error_on_node(
                node.ref_(self).as_named_tuple_member().type_,
                &Diagnostics::A_labeled_tuple_element_is_declared_as_optional_with_a_question_mark_after_the_name_and_before_the_colon_rather_than_after_the_type,
                None,
            );
        }
        if node
            .ref_(self)
            .as_named_tuple_member()
            .type_
            .ref_(self)
            .kind()
            == SyntaxKind::RestType
        {
            self.grammar_error_on_node(
                node.ref_(self).as_named_tuple_member().type_,
                &Diagnostics::A_labeled_tuple_element_is_declared_as_rest_with_a_before_the_name_rather_than_before_the_type,
                None,
            );
        }
        self.check_source_element(Some(node.ref_(self).as_named_tuple_member().type_))?;
        self.get_type_from_type_node_(node)?;

        Ok(())
    }

    pub(super) fn is_private_within_ambient(&self, node: Id<Node>) -> bool {
        (has_effective_modifier(node, ModifierFlags::Private, self)
            || is_private_identifier_class_element_declaration(node, self))
            && node.ref_(self).flags().intersects(NodeFlags::Ambient)
    }

    pub(super) fn get_effective_declaration_flags(
        &self,
        n: Id<Node>, /*Declaration*/
        flags_to_check: ModifierFlags,
    ) -> ModifierFlags {
        let mut flags = get_combined_modifier_flags(n, self);

        if !matches!(
            n.ref_(self).parent().ref_(self).kind(),
            SyntaxKind::InterfaceDeclaration
                | SyntaxKind::ClassDeclaration
                | SyntaxKind::ClassExpression
        ) && n.ref_(self).flags().intersects(NodeFlags::Ambient)
        {
            if !flags.intersects(ModifierFlags::Ambient)
                && !(is_module_block(&n.ref_(self).parent().ref_(self))
                    && is_module_declaration(&n.ref_(self).parent().ref_(self).parent().ref_(self))
                    && is_global_scope_augmentation(
                        &n.ref_(self).parent().ref_(self).parent().ref_(self),
                    ))
            {
                flags |= ModifierFlags::Export;
            }
            flags |= ModifierFlags::Ambient;
        }

        flags & flags_to_check
    }

    pub(super) fn check_function_or_constructor_symbol(
        &self,
        symbol: Id<Symbol>,
    ) -> io::Result<()> {
        if !self.produce_diagnostics {
            return Ok(());
        }

        let flags_to_check = ModifierFlags::Export
            | ModifierFlags::Ambient
            | ModifierFlags::Private
            | ModifierFlags::Protected
            | ModifierFlags::Abstract;
        let mut some_node_flags = ModifierFlags::None;
        let mut all_node_flags = flags_to_check;
        let mut some_have_question_token = false;
        let mut all_have_question_token = true;
        let mut has_overloads = false;
        let mut body_declaration: Option<Id<Node /*FunctionLikeDeclaration*/>> = None;
        let mut last_seen_non_ambient_declaration: Option<Id<Node /*FunctionLikeDeclaration*/>> =
            None;
        let mut previous_declaration: Option<Id<Node /*SignatureDeclaration*/>> = None;

        let declarations = symbol.ref_(self).maybe_declarations().clone();
        let is_constructor = symbol
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::Constructor);

        let mut duplicate_function_declaration = false;
        let mut multiple_constructor_implementation = false;
        let mut has_non_ambient_class = false;
        let mut function_declarations: Vec<Id<Node /*Declaration*/>> = vec![];
        if let Some(declarations) = declarations.as_ref() {
            for &current in declarations {
                let node = current;
                let in_ambient_context = node.ref_(self).flags().intersects(NodeFlags::Ambient);
                let in_ambient_context_or_interface = matches!(
                    node.ref_(self).maybe_parent(),
                    Some(node_parent) if matches!(
                        node_parent.ref_(self).kind(),
                        SyntaxKind::InterfaceDeclaration | SyntaxKind::TypeLiteral
                    )
                ) || in_ambient_context;
                if in_ambient_context_or_interface {
                    previous_declaration = None;
                }

                if matches!(
                    node.ref_(self).kind(),
                    SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression
                ) && !in_ambient_context
                {
                    has_non_ambient_class = true;
                }

                if matches!(
                    node.ref_(self).kind(),
                    SyntaxKind::FunctionDeclaration
                        | SyntaxKind::MethodDeclaration
                        | SyntaxKind::MethodSignature
                        | SyntaxKind::Constructor
                ) {
                    function_declarations.push(node.clone());
                    let current_node_flags =
                        self.get_effective_declaration_flags(node, flags_to_check);
                    some_node_flags |= current_node_flags;
                    all_node_flags &= current_node_flags;
                    some_have_question_token =
                        some_have_question_token || has_question_token(&node.ref_(self));
                    all_have_question_token =
                        all_have_question_token && has_question_token(&node.ref_(self));
                    let body_is_present = node_is_present(
                        node.ref_(self)
                            .maybe_as_function_like_declaration()
                            .and_then(|node| node.maybe_body())
                            .refed(self)
                            .as_deref(),
                    );

                    if body_is_present && body_declaration.is_some() {
                        if is_constructor {
                            multiple_constructor_implementation = true;
                        } else {
                            duplicate_function_declaration = true;
                        }
                    } else if previous_declaration.and_then(|previous_declaration| {
                        previous_declaration.ref_(self).maybe_parent()
                    }) == node.ref_(self).maybe_parent()
                        && previous_declaration.unwrap().ref_(self).end() != node.ref_(self).pos()
                    {
                        self.report_implementation_expected_error(
                            is_constructor,
                            previous_declaration.unwrap(),
                        );
                    }

                    if body_is_present {
                        if body_declaration.is_none() {
                            body_declaration = Some(node.clone());
                        }
                    } else {
                        has_overloads = true;
                    }

                    previous_declaration = Some(node.clone());

                    if !in_ambient_context_or_interface {
                        last_seen_non_ambient_declaration = Some(node.clone());
                    }
                }
            }
        }

        if multiple_constructor_implementation {
            for_each(
                &function_declarations,
                |&declaration: &Id<Node>, _| -> Option<()> {
                    self.error(
                        Some(declaration),
                        &Diagnostics::Multiple_constructor_implementations_are_not_allowed,
                        None,
                    );
                    None
                },
            );
        }

        if duplicate_function_declaration {
            for_each(
                &function_declarations,
                |&declaration: &Id<Node>, _| -> Option<()> {
                    self.error(
                        Some(
                            get_name_of_declaration(Some(declaration), self).unwrap_or(declaration),
                        ),
                        &Diagnostics::Duplicate_function_implementation,
                        None,
                    );
                    None
                },
            );
        }

        if has_non_ambient_class
            && !is_constructor
            && symbol.ref_(self).flags().intersects(SymbolFlags::Function)
        {
            if let Some(declarations) = declarations.as_ref() {
                let related_diagnostics: Vec<Id<DiagnosticRelatedInformation>> =
                    filter(declarations, |d: &Id<Node>| {
                        d.ref_(self).kind() == SyntaxKind::ClassDeclaration
                    })
                    .iter()
                    .map(|&d| {
                        self.alloc_diagnostic_related_information(
                            create_diagnostic_for_node(
                                d,
                                &Diagnostics::Consider_adding_a_declare_modifier_to_this_class,
                                None,
                                self,
                            )
                            .into(),
                        )
                    })
                    .collect();

                for_each(declarations, |&declaration: &Id<Node>, _| -> Option<()> {
                    let diagnostic = if declaration.ref_(self).kind()
                        == SyntaxKind::ClassDeclaration
                    {
                        Some(&*Diagnostics::Class_declaration_cannot_implement_overload_list_for_0)
                    } else if declaration.ref_(self).kind() == SyntaxKind::FunctionDeclaration {
                        Some(&*Diagnostics::Function_with_bodies_can_only_merge_with_classes_that_are_ambient)
                    } else {
                        None
                    };
                    if let Some(diagnostic) = diagnostic {
                        add_related_info(
                            &self
                                .error(
                                    Some(
                                        get_name_of_declaration(Some(declaration), self)
                                            .unwrap_or(declaration),
                                    ),
                                    diagnostic,
                                    Some(vec![symbol_name(symbol, self)]),
                                )
                                .ref_(self),
                            related_diagnostics.clone(),
                        );
                    }
                    None
                });
            }
        }

        if let Some(last_seen_non_ambient_declaration) =
            last_seen_non_ambient_declaration.filter(|&last_seen_non_ambient_declaration| {
                let last_seen_non_ambient_declaration_ref =
                    last_seen_non_ambient_declaration.ref_(self);
                let last_seen_non_ambient_declaration_as_function_like_declaration =
                    last_seen_non_ambient_declaration_ref.as_function_like_declaration();
                last_seen_non_ambient_declaration_as_function_like_declaration
                    .maybe_body()
                    .is_none()
                    && !has_syntactic_modifier(
                        last_seen_non_ambient_declaration,
                        ModifierFlags::Abstract,
                        self,
                    )
                    && last_seen_non_ambient_declaration_as_function_like_declaration
                        .maybe_question_token()
                        .is_none()
            })
        {
            self.report_implementation_expected_error(
                is_constructor,
                last_seen_non_ambient_declaration,
            );
        }

        if has_overloads {
            if let Some(declarations) = declarations.as_ref() {
                self.check_flag_agreement_between_overloads(
                    declarations,
                    body_declaration,
                    flags_to_check,
                    some_node_flags,
                    all_node_flags,
                );
                self.check_question_token_agreement_between_overloads(
                    declarations,
                    body_declaration,
                    some_have_question_token,
                    all_have_question_token,
                );
            }

            if let Some(body_declaration) = body_declaration {
                let signatures = self.get_signatures_of_symbol(Some(symbol))?;
                let body_signature = self.get_signature_from_declaration_(body_declaration)?;
                for signature in &signatures {
                    if !self.is_implementation_compatible_with_overload(
                        body_signature.clone(),
                        signature.clone(),
                    )? {
                        add_related_info(
                            &self.error(
                                signature.ref_(self).declaration,
                                &Diagnostics::This_overload_signature_is_not_compatible_with_its_implementation_signature,
                                None,
                            ).ref_(self),
                            vec![
                                self.alloc_diagnostic_related_information(
                                    create_diagnostic_for_node(
                                        body_declaration,
                                        &Diagnostics::The_implementation_signature_is_declared_here,
                                        None,
                                        self,
                                    ).into()
                                )
                            ]
                        );
                        break;
                    }
                }
            }
        }

        Ok(())
    }

    pub(super) fn get_canonical_overload(
        &self,
        overloads: &[Id<Node /*Declaration*/>],
        implementation: Option<Id<Node> /*FunctionLikeDeclaration*/>,
    ) -> Id<Node /*Declaration*/> {
        let implementation_shares_container_with_first_overload = matches!(
            implementation,
            Some(implementation) if implementation.ref_(self).maybe_parent() == overloads[0].ref_(self).maybe_parent()
        );
        if implementation_shares_container_with_first_overload {
            implementation.unwrap()
        } else {
            overloads[0]
        }
    }

    pub(super) fn check_flag_agreement_between_overloads(
        &self,
        overloads: &[Id<Node /*Declaration*/>],
        implementation: Option<Id<Node> /*FunctionLikeDeclaration*/>,
        flags_to_check: ModifierFlags,
        some_overload_flags: ModifierFlags,
        all_overload_flags: ModifierFlags,
    ) {
        let some_but_not_all_overload_flags = some_overload_flags ^ all_overload_flags;
        if some_but_not_all_overload_flags != ModifierFlags::None {
            let canonical_flags = self.get_effective_declaration_flags(
                self.get_canonical_overload(overloads, implementation),
                flags_to_check,
            );
            for_each(overloads, |&o: &Id<Node>, _| -> Option<()> {
                let deviation =
                    self.get_effective_declaration_flags(o, flags_to_check) ^ canonical_flags;
                if deviation.intersects(ModifierFlags::Export) {
                    self.error(
                        get_name_of_declaration(Some(o), self),
                        &Diagnostics::Overload_signatures_must_all_be_exported_or_non_exported,
                        None,
                    );
                } else if deviation.intersects(ModifierFlags::Ambient) {
                    self.error(
                        get_name_of_declaration(Some(o), self),
                        &Diagnostics::Overload_signatures_must_all_be_ambient_or_non_ambient,
                        None,
                    );
                } else if deviation.intersects(ModifierFlags::Private | ModifierFlags::Protected) {
                    self.error(
                        Some(get_name_of_declaration(Some(o), self).unwrap_or_else(|| o.clone())),
                        &Diagnostics::Overload_signatures_must_all_be_public_private_or_protected,
                        None,
                    );
                } else if deviation.intersects(ModifierFlags::Abstract) {
                    self.error(
                        get_name_of_declaration(Some(o), self),
                        &Diagnostics::Overload_signatures_must_all_be_abstract_or_non_abstract,
                        None,
                    );
                }
                None
            });
        }
    }

    pub(super) fn check_question_token_agreement_between_overloads(
        &self,
        overloads: &[Id<Node /*Declaration*/>],
        implementation: Option<Id<Node> /*FunctionLikeDeclaration*/>,
        some_have_question_token: bool,
        all_have_question_token: bool,
    ) {
        if some_have_question_token != all_have_question_token {
            let canonical_has_question_token = has_question_token(
                &self
                    .get_canonical_overload(overloads, implementation)
                    .ref_(self),
            );
            for_each(overloads, |&o: &Id<Node>, _| -> Option<()> {
                let deviation = has_question_token(&o.ref_(self)) != canonical_has_question_token;
                if deviation {
                    self.error(
                        get_name_of_declaration(Some(o), self),
                        &Diagnostics::Overload_signatures_must_all_be_optional_or_required,
                        None,
                    );
                }
                None
            });
        }
    }
}
