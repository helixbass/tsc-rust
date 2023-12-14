use std::{borrow::Borrow, io, ptr};

use gc::Gc;
use id_arena::Id;

use super::MappedTypeModifiers;
use crate::{
    add_related_info, are_option_gcs_equal, create_diagnostic_for_node, filter, find_ancestor,
    for_each, get_class_extends_heritage_element, get_combined_modifier_flags,
    get_declaration_modifier_flags_from_symbol, get_declaration_of_kind,
    get_effective_constraint_of_type_parameter, get_effective_modifier_flags,
    get_emit_script_target, get_name_of_declaration, get_object_flags, has_effective_modifier,
    has_question_token, has_syntactic_modifier, is_assignment_target, is_global_scope_augmentation,
    is_in_js_file, is_in_jsdoc, is_module_block, is_module_declaration, is_named_tuple_member,
    is_private_identifier_class_element_declaration, is_prologue_directive, is_static,
    is_super_call, is_type_reference_type, node_is_missing, node_is_present,
    return_ok_default_if_none, some, symbol_name, try_cast, try_for_each, try_for_each_child,
    try_maybe_for_each, try_maybe_map, unescape_leading_underscores, DiagnosticRelatedInformation,
    Diagnostics, ElementFlags, FunctionLikeDeclarationInterface, HasInitializerInterface,
    ModifierFlags, Node, NodeArray, NodeCheckFlags, NodeFlags, NodeInterface, ObjectFlags,
    ReadonlyTextRange, ScriptTarget, SignatureDeclarationInterface, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper, HasArena, InArena,
};

impl TypeChecker {
    pub(super) fn check_class_static_block_declaration(
        &self,
        node: &Node, /*ClassStaticBlockDeclaration*/
    ) -> io::Result<()> {
        self.check_grammar_decorators_and_modifiers(node);

        try_for_each_child(
            node,
            |child: &Node| self.check_source_element(Some(child)),
            Option::<fn(&NodeArray) -> io::Result<()>>::None,
        )?;

        Ok(())
    }

    pub(super) fn check_constructor_declaration(
        &self,
        node: &Node, /*ConstructorDeclaration*/
    ) -> io::Result<()> {
        self.check_signature_declaration(node)?;
        if !self.check_grammar_constructor_type_parameters(node) {
            self.check_grammar_constructor_type_annotation(node);
        }

        let node_as_constructor_declaration = node.as_constructor_declaration();
        self.check_source_element(node_as_constructor_declaration.maybe_body())?;

        let symbol = self.get_symbol_of_node(node)?.unwrap();
        let first_declaration = get_declaration_of_kind(&symbol.ref_(self), node.kind());

        if matches!(
            first_declaration.as_ref(),
            Some(first_declaration) if ptr::eq(node, &**first_declaration)
        ) {
            self.check_function_or_constructor_symbol(symbol)?;
        }

        if node_is_missing(node_as_constructor_declaration.maybe_body()) {
            return Ok(());
        }
        let node_body = node_as_constructor_declaration.maybe_body().unwrap();

        if !self.produce_diagnostics {
            return Ok(());
        }

        let containing_class_decl = node.parent();
        if get_class_extends_heritage_element(&containing_class_decl).is_some() {
            self.capture_lexical_this(&node.parent(), &containing_class_decl);
            let class_extends_null = self.class_declaration_extends_null(&containing_class_decl)?;
            let super_call = self.find_first_super_call(&node_body);
            if let Some(super_call) = super_call.as_ref() {
                if class_extends_null {
                    self.error(
                        Some(&**super_call),
                        &Diagnostics::A_constructor_cannot_contain_a_super_call_when_its_class_extends_null,
                        None,
                    );
                }

                let super_call_should_be_first = (get_emit_script_target(&self.compiler_options)
                    != ScriptTarget::ESNext
                    || !self.use_define_for_class_fields)
                    && (some(
                        Some(&*node.parent().as_class_like_declaration().members()),
                        Some(|member: &Id<Node>| {
                            self.is_instance_property_with_initializer_or_private_identifier_property(member)
                        }),
                    ) || some(
                        Some(&*node_as_constructor_declaration.parameters()),
                        Some(|p: &Id<Node>| {
                            has_syntactic_modifier(p, ModifierFlags::ParameterPropertyModifier)
                        }),
                    ));

                if super_call_should_be_first {
                    let statements = &node_body.as_block().statements;
                    let mut super_call_statement: Option<Id<Node /*ExpressionStatement*/>> = None;

                    for statement in statements {
                        if statement.kind() == SyntaxKind::ExpressionStatement
                            && is_super_call(&statement.as_expression_statement().expression)
                        {
                            super_call_statement = Some(statement.node_wrapper());
                            break;
                        }
                        if !is_prologue_directive(statement) {
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
        n: &Node,
    ) -> bool {
        if is_private_identifier_class_element_declaration(n) {
            return true;
        }
        n.kind() == SyntaxKind::PropertyDeclaration
            && !is_static(n)
            && n.as_property_declaration().maybe_initializer().is_some()
    }

    pub(super) fn check_accessor_declaration(
        &self,
        node: &Node, /*AccessorDeclaration*/
    ) -> io::Result<()> {
        let node_as_function_like_declaration = node.as_function_like_declaration();
        if self.produce_diagnostics {
            if !self.check_grammar_function_like_declaration(node)?
                && !self.check_grammar_accessor(node)
            {
                self.check_grammar_computed_property_name(
                    &node_as_function_like_declaration.name(),
                );
            }

            self.check_decorators(node)?;
            self.check_signature_declaration(node)?;
            if node.kind() == SyntaxKind::GetAccessor {
                if !node.flags().intersects(NodeFlags::Ambient)
                    && node_is_present(node_as_function_like_declaration.maybe_body())
                    && node.flags().intersects(NodeFlags::HasImplicitReturn)
                {
                    if !node.flags().intersects(NodeFlags::HasExplicitReturn) {
                        self.error(
                            node_as_function_like_declaration.maybe_name(),
                            &Diagnostics::A_get_accessor_must_return_a_value,
                            None,
                        );
                    }
                }
            }
            if node_as_function_like_declaration.name().kind() == SyntaxKind::ComputedPropertyName {
                self.check_computed_property_name(&node_as_function_like_declaration.name())?;
            }

            if self.has_bindable_name(node)? {
                let symbol = self.get_symbol_of_node(node)?.unwrap();
                let getter = get_declaration_of_kind(&symbol.ref_(self), SyntaxKind::GetAccessor);
                let setter = get_declaration_of_kind(&symbol.ref_(self), SyntaxKind::SetAccessor);
                if let Some(getter) = getter.as_ref() {
                    if let Some(setter) = setter.as_ref() {
                        if !self
                            .get_node_check_flags(getter)
                            .intersects(NodeCheckFlags::TypeChecked)
                        {
                            self.get_node_links(getter).borrow_mut().flags |=
                                NodeCheckFlags::TypeChecked;
                            let getter_flags = get_effective_modifier_flags(getter);
                            let setter_flags = get_effective_modifier_flags(setter);
                            if getter_flags & ModifierFlags::Abstract
                                != setter_flags & ModifierFlags::Abstract
                            {
                                self.error(
                                    getter.as_named_declaration().maybe_name(),
                                    &Diagnostics::Accessors_must_both_be_abstract_or_non_abstract,
                                    None,
                                );
                                self.error(
                                    setter.as_named_declaration().maybe_name(),
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
                                    getter.as_named_declaration().maybe_name(),
                                    &Diagnostics::A_get_accessor_must_be_at_least_as_accessible_as_the_setter,
                                    None,
                                );
                                self.error(
                                    setter.as_named_declaration().maybe_name(),
                                    &Diagnostics::A_get_accessor_must_be_at_least_as_accessible_as_the_setter,
                                    None,
                                );
                            }

                            let getter_type = self.get_annotated_accessor_type(Some(&**getter))?;
                            let setter_type = self.get_annotated_accessor_type(Some(&**setter))?;
                            if let (Some(getter_type), Some(setter_type)) =
                                (getter_type, setter_type)
                            {
                                self.check_type_assignable_to(
                                    getter_type,
                                    setter_type,
                                    Some(&**getter),
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
            if node.kind() == SyntaxKind::GetAccessor {
                self.check_all_code_paths_in_non_void_function_return_or_throw(
                    node,
                    Some(return_type),
                )?;
            }
        }
        self.check_source_element(node_as_function_like_declaration.maybe_body())?;
        self.set_node_links_for_private_identifier_scope(node);

        Ok(())
    }

    pub(super) fn check_missing_declaration(&self, node: &Node) -> io::Result<()> {
        self.check_decorators(node)?;

        Ok(())
    }

    pub(super) fn get_effective_type_arguments(
        &self,
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
        type_parameters: Option<&[Id<Type /*TypeParameter*/>]>,
    ) -> io::Result<Vec<Id<Type>>> {
        Ok(self
            .fill_missing_type_arguments(
                try_maybe_map(
                    node.as_has_type_arguments().maybe_type_arguments().as_ref(),
                    |type_argument, _| self.get_type_from_type_node_(type_argument),
                )
                .transpose()?,
                type_parameters,
                self.get_min_type_argument_count(type_parameters),
                is_in_js_file(Some(node)),
            )?
            .unwrap())
    }

    pub(super) fn check_type_argument_constraints(
        &self,
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
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
                        node.as_has_type_arguments()
                            .maybe_type_arguments()
                            .as_ref()
                            .unwrap()
                            .get(i)
                            .cloned(),
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
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
    ) -> io::Result<Option<Vec<Id<Type>>>> {
        let type_ = self.get_type_from_type_reference(node)?;
        if !self.is_error_type(type_) {
            let symbol = (*self.get_node_links(node))
                .borrow()
                .resolved_symbol
                .clone();
            if let Some(symbol) = symbol {
                return Ok(if symbol.ref_(self)
                    .flags()
                    .intersects(SymbolFlags::TypeAlias)
                {
                    (*self.get_symbol_links(symbol))
                        .borrow()
                        .type_parameters
                        .clone()
                } else {
                    None
                }
                .or_else(|| {
                    if get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::Reference) {
                        type_.ref_(self).as_type_reference_interface().target().ref_(self)
                            .as_interface_type_interface()
                            .maybe_local_type_parameters()
                            .map(ToOwned::to_owned)
                    } else {
                        None
                    }
                }));
            }
        }
        Ok(None)
    }

    pub(super) fn check_type_reference_node(
        &self,
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
    ) -> io::Result<()> {
        let node_as_has_type_arguments = node.as_has_type_arguments();
        self.check_grammar_type_arguments(
            node,
            node_as_has_type_arguments.maybe_type_arguments().as_deref(),
        );
        if node.kind() == SyntaxKind::TypeReference {
            if let Some(node_type_name_jsdoc_dot_pos) = node
                .as_type_reference_node()
                .type_name
                .as_has_jsdoc_dot_pos()
                .maybe_jsdoc_dot_pos()
            {
                if !is_in_js_file(Some(node)) && !is_in_jsdoc(Some(node)) {
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
            node_as_has_type_arguments.maybe_type_arguments().as_ref(),
            |type_argument, _| -> io::Result<_> {
                self.check_source_element(Some(&**type_argument))?;
                Ok(Option::<()>::None)
            },
        )?;
        let type_ = self.get_type_from_type_reference(node)?;
        if !self.is_error_type(type_) {
            if node_as_has_type_arguments.maybe_type_arguments().is_some()
                && self.produce_diagnostics
            {
                let type_parameters = self.get_type_parameters_for_type_reference(node)?;
                if let Some(type_parameters) = type_parameters.as_ref() {
                    self.check_type_argument_constraints(node, type_parameters)?;
                }
            }
            let symbol = (*self.get_node_links(node))
                .borrow()
                .resolved_symbol
                .clone();
            if let Some(symbol) = symbol {
                if some(
                    symbol.ref_(self).maybe_declarations().as_deref(),
                    Some(|d: &Id<Node>| {
                        self.is_type_declaration(d) && d.flags().intersects(NodeFlags::Deprecated)
                    }),
                ) {
                    self.add_deprecated_suggestion(
                        &self.get_deprecated_suggestion_node(node),
                        symbol.ref_(self).maybe_declarations().as_ref().unwrap(),
                        symbol.ref_(self).escaped_name(),
                    );
                }
                if type_.ref_(self).flags().intersects(TypeFlags::Enum)
                    && symbol.ref_(self)
                        .flags()
                        .intersects(SymbolFlags::EnumMember)
                {
                    self.error(
                        Some(node),
                        &Diagnostics::Enum_type_0_has_members_with_initializers_that_are_not_literals,
                        Some(vec![
                            self.type_to_string_(
                                type_,
                                Option::<&Node>::None,
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
        node: &Node, /*TypeNode*/
    ) -> io::Result<Option<Id<Type>>> {
        let type_reference_node =
            return_ok_default_if_none!(try_cast(node.parent(), |parent: &Id<Node>| {
                is_type_reference_type(parent)
            }));
        let type_parameters = return_ok_default_if_none!(
            self.get_type_parameters_for_type_reference(&type_reference_node)?
        );
        let constraint = return_ok_default_if_none!(self.get_constraint_of_type_parameter(
            type_parameters[type_reference_node
                .as_has_type_arguments()
                .maybe_type_arguments()
                .as_ref()
                .unwrap()
                .into_iter()
                .position(|type_argument| ptr::eq(&**type_argument, node))
                .unwrap()],
        )?);
        Ok(Some(self.instantiate_type(
            constraint,
            Some(self.create_type_mapper(
                type_parameters.clone(),
                Some(
                    self.get_effective_type_arguments(
                        &type_reference_node,
                        Some(&type_parameters),
                    )?,
                ),
            )),
        )?))
    }

    pub(super) fn check_type_query(&self, node: &Node /*TypeQueryNode*/) -> io::Result<()> {
        self.get_type_from_type_query_node(node)?;

        Ok(())
    }

    pub(super) fn check_type_literal(
        &self,
        node: &Node, /*TypeLiteralNode*/
    ) -> io::Result<()> {
        try_for_each(
            &node.as_type_literal_node().members,
            |member: &Id<Node>, _| -> io::Result<Option<()>> {
                self.check_source_element(Some(&**member))?;
                Ok(None)
            },
        )?;
        if self.produce_diagnostics {
            let type_ =
                self.get_type_from_type_literal_or_function_or_constructor_type_node(node)?;
            self.check_index_constraints(type_, type_.ref_(self).symbol(), None)?;
            self.check_type_for_duplicate_index_signatures(node)?;
            self.check_object_type_for_duplicate_declarations(node);
        }

        Ok(())
    }

    pub(super) fn check_array_type(&self, node: &Node /*ArrayTypeNode*/) -> io::Result<()> {
        self.check_source_element(Some(&*node.as_array_type_node().element_type))?;

        Ok(())
    }

    pub(super) fn check_tuple_type(&self, node: &Node /*TupleTypeNode*/) -> io::Result<()> {
        let node_as_tuple_type_node = node.as_tuple_type_node();
        let element_types = &node_as_tuple_type_node.elements;
        let mut seen_optional_element = false;
        let mut seen_rest_element = false;
        let has_named_element = some(
            Some(&**element_types),
            Some(|element_type: &Id<Node>| is_named_tuple_member(element_type)),
        );
        for e in element_types {
            if e.kind() != SyntaxKind::NamedTupleMember && has_named_element {
                self.grammar_error_on_node(
                    e,
                    &Diagnostics::Tuple_members_must_all_have_names_or_all_not_have_names,
                    None,
                );
                break;
            }
            let flags = self.get_tuple_element_flags(e);
            if flags.intersects(ElementFlags::Variadic) {
                let type_ =
                    self.get_type_from_type_node_(&e.as_has_type().maybe_type().unwrap())?;
                if !self.is_array_like_type(type_)? {
                    self.error(
                        Some(&**e),
                        &Diagnostics::A_rest_element_type_must_be_an_array_type,
                        None,
                    );
                    break;
                }
                if self.is_array_type(type_)
                    || self.is_tuple_type(type_)
                        && type_.ref_(self).as_type_reference_interface().target().ref_(self)
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
            &node_as_tuple_type_node.elements,
            |element: &Id<Node>, _| -> io::Result<Option<()>> {
                self.check_source_element(Some(&**element))?;
                Ok(None)
            },
        )?;
        self.get_type_from_type_node_(node)?;

        Ok(())
    }

    pub(super) fn check_union_or_intersection_type(
        &self,
        node: &Node, /*UnionOrIntersectionTypeNode*/
    ) -> io::Result<()> {
        try_for_each(
            &node.as_union_or_intersection_type_node().types(),
            |type_, _| -> io::Result<_> {
                self.check_source_element(Some(&**type_))?;
                Ok(Option::<()>::None)
            },
        )?;
        self.get_type_from_type_node_(node)?;

        Ok(())
    }

    pub(super) fn check_indexed_access_index_type(
        &self,
        type_: Id<Type>,
        access_node: &Node, /*IndexedAccessTypeNode | ElementAccessExpression*/
    ) -> io::Result<Id<Type>> {
        if !type_.ref_(self)
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
            if access_node.kind() == SyntaxKind::ElementAccessExpression
                && is_assignment_target(access_node)
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
                        Option::<&Node>::None,
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
                        self.arena(), &property_symbol.ref_(self),
                        None,
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
                self.type_to_string_(index_type, Option::<&Node>::None, None, None)?,
                self.type_to_string_(object_type, Option::<&Node>::None, None, None)?,
            ]),
        );
        Ok(self.error_type())
    }

    pub(super) fn check_indexed_access_type(
        &self,
        node: &Node, /*IndexedAccessTypeNode*/
    ) -> io::Result<()> {
        let node_as_indexed_access_type_node = node.as_indexed_access_type_node();
        self.check_source_element(Some(&*node_as_indexed_access_type_node.object_type))?;
        self.check_source_element(Some(&*node_as_indexed_access_type_node.index_type))?;
        self.check_indexed_access_index_type(
            self.get_type_from_indexed_access_type_node(node)?,
            node,
        )?;

        Ok(())
    }

    pub(super) fn check_mapped_type(&self, node: &Node /*MappedTypeNode*/) -> io::Result<()> {
        self.check_grammar_mapped_type(node);
        let node_as_mapped_type_node = node.as_mapped_type_node();
        self.check_source_element(Some(&*node_as_mapped_type_node.type_parameter))?;
        self.check_source_element(node_as_mapped_type_node.name_type.as_deref())?;
        self.check_source_element(node_as_mapped_type_node.type_.as_deref())?;

        if node_as_mapped_type_node.type_.is_none() {
            self.report_implicit_any(node, self.any_type(), None)?;
        }

        let type_ = self.get_type_from_mapped_type_node(node)?;
        let name_type = self.get_name_type_from_mapped_type(type_)?;
        if let Some(name_type) = name_type {
            self.check_type_assignable_to(
                name_type,
                self.keyof_constraint_type(),
                node_as_mapped_type_node.name_type.as_deref(),
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
                    &node_as_mapped_type_node.type_parameter,
                ),
                None,
                None,
                None,
            )?;
        }

        Ok(())
    }

    pub(super) fn check_grammar_mapped_type(&self, node: &Node /*MappedTypeNode*/) -> bool {
        if let Some(node_members) = node
            .as_mapped_type_node()
            .members
            .as_ref()
            .filter(|node_members| !node_members.is_empty())
        {
            return self.grammar_error_on_node(
                &node_members[0],
                &Diagnostics::A_mapped_type_may_not_declare_properties_or_methods,
                None,
            );
        }
        false
    }

    pub(super) fn check_this_type(&self, node: &Node /*ThisTypeNode*/) -> io::Result<()> {
        self.get_type_from_this_type_node(node)?;

        Ok(())
    }

    pub(super) fn check_type_operator(
        &self,
        node: &Node, /*TypeOperatorNode*/
    ) -> io::Result<()> {
        self.check_grammar_type_operator_node(node);
        self.check_source_element(Some(&*node.as_type_operator_node().type_))?;

        Ok(())
    }

    pub(super) fn check_conditional_type(
        &self,
        node: &Node, /*ConditionalTypeNode*/
    ) -> io::Result<()> {
        try_for_each_child(
            node,
            |child: &Node| self.check_source_element(Some(child)),
            Option::<fn(&NodeArray) -> io::Result<()>>::None,
        )?;

        Ok(())
    }

    pub(super) fn check_infer_type(&self, node: &Node /*InferTypeNode*/) -> io::Result<()> {
        if find_ancestor(Some(node), |n: &Node| {
            matches!(
                n.maybe_parent().as_ref(),
                Some(n_parent) if n_parent.kind() == SyntaxKind::ConditionalType &&
                    ptr::eq(
                        &*n_parent.as_conditional_type_node().extends_type,
                        n
                    )
            )
        })
        .is_none()
        {
            self.grammar_error_on_node(
                node,
                &Diagnostics::infer_declarations_are_only_permitted_in_the_extends_clause_of_a_conditional_type,
                None,
            );
        }
        self.check_source_element(Some(&*node.as_infer_type_node().type_parameter))?;
        self.register_for_unused_identifiers_check(node);

        Ok(())
    }

    pub(super) fn check_template_literal_type(
        &self,
        node: &Node, /*TemplateLiteralTypeNode*/
    ) -> io::Result<()> {
        for span in &node.as_template_literal_type_node().template_spans {
            let span_as_template_literal_type_span = span.as_template_literal_type_span();
            self.check_source_element(Some(&*span_as_template_literal_type_span.type_))?;
            let type_ = self.get_type_from_type_node_(&span_as_template_literal_type_span.type_)?;
            self.check_type_assignable_to(
                type_,
                self.template_constraint_type(),
                Some(&*span_as_template_literal_type_span.type_),
                None,
                None,
                None,
            )?;
        }
        self.get_type_from_type_node_(node)?;

        Ok(())
    }

    pub(super) fn check_import_type(&self, node: &Node /*ImportTypeNode*/) -> io::Result<()> {
        self.check_source_element(Some(&*node.as_import_type_node().argument))?;
        self.get_type_from_type_node_(node)?;

        Ok(())
    }

    pub(super) fn check_named_tuple_member(
        &self,
        node: &Node, /*NamedTupleMember*/
    ) -> io::Result<()> {
        let node_as_named_tuple_member = node.as_named_tuple_member();
        if node_as_named_tuple_member.dot_dot_dot_token.is_some()
            && node_as_named_tuple_member.question_token.is_some()
        {
            self.grammar_error_on_node(
                node,
                &Diagnostics::A_tuple_member_cannot_be_both_optional_and_rest,
                None,
            );
        }
        if node_as_named_tuple_member.type_.kind() == SyntaxKind::OptionalType {
            self.grammar_error_on_node(
                &node_as_named_tuple_member.type_,
                &Diagnostics::A_labeled_tuple_element_is_declared_as_optional_with_a_question_mark_after_the_name_and_before_the_colon_rather_than_after_the_type,
                None,
            );
        }
        if node_as_named_tuple_member.type_.kind() == SyntaxKind::RestType {
            self.grammar_error_on_node(
                &node_as_named_tuple_member.type_,
                &Diagnostics::A_labeled_tuple_element_is_declared_as_rest_with_a_before_the_name_rather_than_before_the_type,
                None,
            );
        }
        self.check_source_element(Some(&*node_as_named_tuple_member.type_))?;
        self.get_type_from_type_node_(node)?;

        Ok(())
    }

    pub(super) fn is_private_within_ambient(&self, node: &Node) -> bool {
        (has_effective_modifier(node, ModifierFlags::Private)
            || is_private_identifier_class_element_declaration(node))
            && node.flags().intersects(NodeFlags::Ambient)
    }

    pub(super) fn get_effective_declaration_flags(
        &self,
        n: &Node, /*Declaration*/
        flags_to_check: ModifierFlags,
    ) -> ModifierFlags {
        let mut flags = get_combined_modifier_flags(n);

        if !matches!(
            n.parent().kind(),
            SyntaxKind::InterfaceDeclaration
                | SyntaxKind::ClassDeclaration
                | SyntaxKind::ClassExpression
        ) && n.flags().intersects(NodeFlags::Ambient)
        {
            if !flags.intersects(ModifierFlags::Ambient)
                && !(is_module_block(&n.parent())
                    && is_module_declaration(&n.parent().parent())
                    && is_global_scope_augmentation(&n.parent().parent()))
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

        let symbol_ref = symbol.ref_(self);
        let declarations = symbol_ref.maybe_declarations();
        let is_constructor = symbol.ref_(self)
            .flags()
            .intersects(SymbolFlags::Constructor);

        let mut duplicate_function_declaration = false;
        let mut multiple_constructor_implementation = false;
        let mut has_non_ambient_class = false;
        let mut function_declarations: Vec<Id<Node /*Declaration*/>> = vec![];
        if let Some(declarations) = declarations.as_ref() {
            for current in declarations {
                let node = current;
                let in_ambient_context = node.flags().intersects(NodeFlags::Ambient);
                let in_ambient_context_or_interface = matches!(
                    node.maybe_parent().as_ref(),
                    Some(node_parent) if matches!(
                        node_parent.kind(),
                        SyntaxKind::InterfaceDeclaration | SyntaxKind::TypeLiteral
                    )
                ) || in_ambient_context;
                if in_ambient_context_or_interface {
                    previous_declaration = None;
                }

                if matches!(
                    node.kind(),
                    SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression
                ) && !in_ambient_context
                {
                    has_non_ambient_class = true;
                }

                if matches!(
                    node.kind(),
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
                    some_have_question_token = some_have_question_token || has_question_token(node);
                    all_have_question_token = all_have_question_token && has_question_token(node);
                    let body_is_present = node_is_present(
                        node.maybe_as_function_like_declaration()
                            .and_then(|node| node.maybe_body()),
                    );

                    if body_is_present && body_declaration.is_some() {
                        if is_constructor {
                            multiple_constructor_implementation = true;
                        } else {
                            duplicate_function_declaration = true;
                        }
                    } else if are_option_gcs_equal(
                        previous_declaration
                            .as_ref()
                            .and_then(|previous_declaration| previous_declaration.maybe_parent())
                            .as_ref(),
                        node.maybe_parent().as_ref(),
                    ) && previous_declaration.as_ref().unwrap().end() != node.pos()
                    {
                        self.report_implementation_expected_error(
                            is_constructor,
                            previous_declaration.as_ref().unwrap(),
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
                |declaration: &Id<Node>, _| -> Option<()> {
                    self.error(
                        Some(&**declaration),
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
                |declaration: &Id<Node>, _| -> Option<()> {
                    self.error(
                        Some(
                            get_name_of_declaration(Some(&**declaration))
                                .unwrap_or_else(|| declaration.clone()),
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
            && symbol.ref_(self)
                .flags()
                .intersects(SymbolFlags::Function)
        {
            if let Some(declarations) = declarations.as_ref() {
                let related_diagnostics: Vec<Gc<DiagnosticRelatedInformation>> =
                    filter(declarations, |d: &Id<Node>| {
                        d.kind() == SyntaxKind::ClassDeclaration
                    })
                    .iter()
                    .map(|d| {
                        Gc::new(
                            create_diagnostic_for_node(
                                d,
                                &Diagnostics::Consider_adding_a_declare_modifier_to_this_class,
                                None,
                            )
                            .into(),
                        )
                    })
                    .collect();

                for_each(declarations, |declaration: &Id<Node>, _| -> Option<()> {
                    let diagnostic = if declaration.kind() == SyntaxKind::ClassDeclaration {
                        Some(&*Diagnostics::Class_declaration_cannot_implement_overload_list_for_0)
                    } else if declaration.kind() == SyntaxKind::FunctionDeclaration {
                        Some(&*Diagnostics::Function_with_bodies_can_only_merge_with_classes_that_are_ambient)
                    } else {
                        None
                    };
                    if let Some(diagnostic) = diagnostic {
                        add_related_info(
                            &self.error(
                                Some(
                                    get_name_of_declaration(Some(&**declaration))
                                        .unwrap_or_else(|| declaration.clone()),
                                ),
                                diagnostic,
                                Some(vec![symbol_name(&symbol.ref_(self)).into_owned()]),
                            ),
                            related_diagnostics.clone(),
                        );
                    }
                    None
                });
            }
        }

        if let Some(last_seen_non_ambient_declaration) = last_seen_non_ambient_declaration
            .as_ref()
            .filter(|last_seen_non_ambient_declaration| {
                let last_seen_non_ambient_declaration_as_function_like_declaration =
                    last_seen_non_ambient_declaration.as_function_like_declaration();
                last_seen_non_ambient_declaration_as_function_like_declaration
                    .maybe_body()
                    .is_none()
                    && !has_syntactic_modifier(
                        last_seen_non_ambient_declaration,
                        ModifierFlags::Abstract,
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
                    body_declaration.as_deref(),
                    flags_to_check,
                    some_node_flags,
                    all_node_flags,
                );
                self.check_question_token_agreement_between_overloads(
                    declarations,
                    body_declaration.as_deref(),
                    some_have_question_token,
                    all_have_question_token,
                );
            }

            if let Some(body_declaration) = body_declaration.as_ref() {
                let signatures = self.get_signatures_of_symbol(Some(symbol))?;
                let body_signature = self.get_signature_from_declaration_(body_declaration)?;
                for signature in &signatures {
                    if !self.is_implementation_compatible_with_overload(
                        body_signature.clone(),
                        signature.clone(),
                    )? {
                        add_related_info(
                            &self.error(
                                signature.declaration.as_deref(),
                                &Diagnostics::This_overload_signature_is_not_compatible_with_its_implementation_signature,
                                None,
                            ),
                            vec![
                                Gc::new(
                                    create_diagnostic_for_node(
                                        body_declaration,
                                        &Diagnostics::The_implementation_signature_is_declared_here,
                                        None,
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

    pub(super) fn get_canonical_overload<TImplementation: Borrow<Node>>(
        &self,
        overloads: &[Id<Node /*Declaration*/>],
        implementation: Option<TImplementation /*FunctionLikeDeclaration*/>,
    ) -> Id<Node /*Declaration*/> {
        let implementation =
            implementation.map(|implementation| implementation.borrow().node_wrapper());
        let implementation_shares_container_with_first_overload = matches!(
            implementation.as_ref(),
            Some(implementation) if are_option_gcs_equal(
                implementation.maybe_parent().as_ref(),
                overloads[0].maybe_parent().as_ref()
            )
        );
        if implementation_shares_container_with_first_overload {
            implementation.unwrap()
        } else {
            overloads[0].clone()
        }
    }

    pub(super) fn check_flag_agreement_between_overloads<TImplementation: Borrow<Node>>(
        &self,
        overloads: &[Id<Node /*Declaration*/>],
        implementation: Option<TImplementation /*FunctionLikeDeclaration*/>,
        flags_to_check: ModifierFlags,
        some_overload_flags: ModifierFlags,
        all_overload_flags: ModifierFlags,
    ) {
        let some_but_not_all_overload_flags = some_overload_flags ^ all_overload_flags;
        if some_but_not_all_overload_flags != ModifierFlags::None {
            let canonical_flags = self.get_effective_declaration_flags(
                &self.get_canonical_overload(overloads, implementation),
                flags_to_check,
            );
            for_each(overloads, |o: &Id<Node>, _| -> Option<()> {
                let deviation =
                    self.get_effective_declaration_flags(o, flags_to_check) ^ canonical_flags;
                if deviation.intersects(ModifierFlags::Export) {
                    self.error(
                        get_name_of_declaration(Some(&**o)),
                        &Diagnostics::Overload_signatures_must_all_be_exported_or_non_exported,
                        None,
                    );
                } else if deviation.intersects(ModifierFlags::Ambient) {
                    self.error(
                        get_name_of_declaration(Some(&**o)),
                        &Diagnostics::Overload_signatures_must_all_be_ambient_or_non_ambient,
                        None,
                    );
                } else if deviation.intersects(ModifierFlags::Private | ModifierFlags::Protected) {
                    self.error(
                        Some(get_name_of_declaration(Some(&**o)).unwrap_or_else(|| o.clone())),
                        &Diagnostics::Overload_signatures_must_all_be_public_private_or_protected,
                        None,
                    );
                } else if deviation.intersects(ModifierFlags::Abstract) {
                    self.error(
                        get_name_of_declaration(Some(&**o)),
                        &Diagnostics::Overload_signatures_must_all_be_abstract_or_non_abstract,
                        None,
                    );
                }
                None
            });
        }
    }

    pub(super) fn check_question_token_agreement_between_overloads<
        TImplementation: Borrow<Node>,
    >(
        &self,
        overloads: &[Id<Node /*Declaration*/>],
        implementation: Option<TImplementation /*FunctionLikeDeclaration*/>,
        some_have_question_token: bool,
        all_have_question_token: bool,
    ) {
        if some_have_question_token != all_have_question_token {
            let canonical_has_question_token =
                has_question_token(&self.get_canonical_overload(overloads, implementation));
            for_each(overloads, |o: &Id<Node>, _| -> Option<()> {
                let deviation = has_question_token(o) != canonical_has_question_token;
                if deviation {
                    self.error(
                        get_name_of_declaration(Some(&**o)),
                        &Diagnostics::Overload_signatures_must_all_be_optional_or_required,
                        None,
                    );
                }
                None
            });
        }
    }
}
