#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, IterationTypeKind, IterationUse, MappedTypeModifiers, UnusedKind};
use crate::{
    add_related_info, are_option_rcs_equal, create_diagnostic_for_node, declaration_name_to_string,
    filter, find_ancestor, for_each, for_each_child, for_each_child_returns,
    get_class_extends_heritage_element, get_combined_modifier_flags,
    get_containing_function_or_class_static_block, get_declaration_modifier_flags_from_symbol,
    get_declaration_of_kind, get_effective_constraint_of_type_parameter, get_effective_initializer,
    get_effective_modifier_flags, get_emit_script_target,
    get_escaped_text_of_identifier_or_literal, get_function_flags, get_name_of_declaration,
    get_object_flags, has_effective_modifier, has_question_token, has_syntactic_modifier,
    is_assignment_target, is_binding_element, is_computed_property_name,
    is_function_or_module_block, is_global_scope_augmentation, is_in_js_file, is_in_jsdoc,
    is_module_block, is_module_declaration, is_named_tuple_member, is_private_identifier,
    is_private_identifier_class_element_declaration, is_prologue_directive,
    is_property_name_literal, is_static, is_super_call, is_type_reference_type, map,
    maybe_for_each, node_is_missing, node_is_present, some, symbol_name, try_cast,
    unescape_leading_underscores, Diagnostic, DiagnosticMessage, DiagnosticRelatedInformation,
    Diagnostics, ElementFlags, FunctionFlags, FunctionLikeDeclarationInterface,
    HasInitializerInterface, HasTypeParametersInterface, IterationTypes, IterationTypesResolver,
    ModifierFlags, Node, NodeArray, NodeCheckFlags, NodeFlags, NodeInterface, ObjectFlags,
    ReadonlyTextRange, ScriptTarget, SignatureDeclarationInterface, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
};

impl TypeChecker {
    pub(super) fn check_class_static_block_declaration(
        &self,
        node: &Node, /*ClassStaticBlockDeclaration*/
    ) {
        self.check_grammar_decorators_and_modifiers(node);

        for_each_child(
            node,
            |child: &Node| self.check_source_element(Some(child)),
            Option::<fn(&NodeArray)>::None,
        );
    }

    pub(super) fn check_constructor_declaration(
        &self,
        node: &Node, /*ConstructorDeclaration*/
    ) {
        self.check_signature_declaration(node);
        if !self.check_grammar_constructor_type_parameters(node) {
            self.check_grammar_constructor_type_annotation(node);
        }

        let node_as_constructor_declaration = node.as_constructor_declaration();
        self.check_source_element(node_as_constructor_declaration.maybe_body());

        let symbol = self.get_symbol_of_node(node).unwrap();
        let first_declaration = get_declaration_of_kind(&symbol, node.kind());

        if matches!(
            first_declaration.as_ref(),
            Some(first_declaration) if ptr::eq(node, &**first_declaration)
        ) {
            self.check_function_or_constructor_symbol(&symbol);
        }

        if node_is_missing(node_as_constructor_declaration.maybe_body()) {
            return;
        }
        let node_body = node_as_constructor_declaration.maybe_body().unwrap();

        if !self.produce_diagnostics {
            return;
        }

        let containing_class_decl = node.parent();
        if get_class_extends_heritage_element(&containing_class_decl).is_some() {
            self.capture_lexical_this(&node.parent(), &containing_class_decl);
            let class_extends_null = self.class_declaration_extends_null(&containing_class_decl);
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
                        Some(|member: &Rc<Node>| {
                            self.is_instance_property_with_initializer_or_private_identifier_property(member)
                        }),
                    ) || some(
                        Some(&*node_as_constructor_declaration.parameters()),
                        Some(|p: &Rc<Node>| {
                            has_syntactic_modifier(p, ModifierFlags::ParameterPropertyModifier)
                        }),
                    ));

                if super_call_should_be_first {
                    let statements = &node_body.as_block().statements;
                    let mut super_call_statement: Option<Rc<Node /*ExpressionStatement*/>> = None;

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

    pub(super) fn check_accessor_declaration(&self, node: &Node /*AccessorDeclaration*/) {
        let node_as_function_like_declaration = node.as_function_like_declaration();
        if self.produce_diagnostics {
            if !self.check_grammar_function_like_declaration(node)
                && !self.check_grammar_accessor(node)
            {
                self.check_grammar_computed_property_name(
                    &node_as_function_like_declaration.name(),
                );
            }

            self.check_decorators(node);
            self.check_signature_declaration(node);
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
                self.check_computed_property_name(&node_as_function_like_declaration.name());
            }

            if self.has_bindable_name(node) {
                let symbol = self.get_symbol_of_node(node).unwrap();
                let getter = get_declaration_of_kind(&symbol, SyntaxKind::GetAccessor);
                let setter = get_declaration_of_kind(&symbol, SyntaxKind::SetAccessor);
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

                            let getter_type = self.get_annotated_accessor_type(Some(&**getter));
                            let setter_type = self.get_annotated_accessor_type(Some(&**setter));
                            if let (Some(getter_type), Some(setter_type)) =
                                (getter_type.as_ref(), setter_type.as_ref())
                            {
                                self.check_type_assignable_to(
                                    getter_type,
                                    setter_type,
                                    Some(&**getter),
                                    Some(&Diagnostics::The_return_type_of_a_get_accessor_must_be_assignable_to_its_set_accessor_type),
                                    None, None,
                                );
                            }
                        }
                    }
                }
            }
            let return_type = self.get_type_of_accessors(&self.get_symbol_of_node(node).unwrap());
            if node.kind() == SyntaxKind::GetAccessor {
                self.check_all_code_paths_in_non_void_function_return_or_throw(
                    node,
                    Some(&*return_type),
                );
            }
        }
        self.check_source_element(node_as_function_like_declaration.maybe_body());
        self.set_node_links_for_private_identifier_scope(node);
    }

    pub(super) fn check_missing_declaration(&self, node: &Node) {
        self.check_decorators(node);
    }

    pub(super) fn get_effective_type_arguments(
        &self,
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
        type_parameters: &[Rc<Type /*TypeParameter*/>],
    ) -> Vec<Rc<Type>> {
        self.fill_missing_type_arguments(
            Some(map(
                node.as_has_type_arguments().maybe_type_arguments().unwrap(),
                |type_argument, _| self.get_type_from_type_node_(type_argument),
            )),
            Some(type_parameters),
            self.get_min_type_argument_count(Some(type_parameters)),
            is_in_js_file(Some(node)),
        )
        .unwrap()
    }

    pub(super) fn check_type_argument_constraints(
        &self,
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
        type_parameters: &[Rc<Type /*TypeParameter*/>],
    ) -> bool {
        let mut type_arguments: Option<Vec<Rc<Type>>> = None;
        let mut mapper: Option<TypeMapper> = None;
        let mut result = true;
        for i in 0..type_parameters.len() {
            let constraint = self.get_constraint_of_type_parameter(&type_parameters[i]);
            if let Some(constraint) = constraint.as_ref() {
                if type_arguments.is_none() {
                    type_arguments = Some(self.get_effective_type_arguments(node, type_parameters));
                    mapper = Some(
                        self.create_type_mapper(type_parameters.to_owned(), type_arguments.clone()),
                    );
                }
                result = result
                    && self.check_type_assignable_to(
                        &type_arguments.as_ref().unwrap()[i],
                        &self.instantiate_type(constraint, mapper.as_ref()),
                        node.as_has_type_arguments()
                            .maybe_type_arguments()
                            .unwrap()
                            .get(i)
                            .cloned(),
                        Some(&Diagnostics::Type_0_does_not_satisfy_the_constraint_1),
                        None,
                        None,
                    );
            }
        }
        result
    }

    pub(super) fn get_type_parameters_for_type_reference(
        &self,
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
    ) -> Option<Vec<Rc<Type>>> {
        let type_ = self.get_type_from_type_reference(node);
        if !self.is_error_type(&type_) {
            let symbol = (*self.get_node_links(node))
                .borrow()
                .resolved_symbol
                .clone();
            if let Some(symbol) = symbol.as_ref() {
                return if symbol.flags().intersects(SymbolFlags::TypeAlias) {
                    (*self.get_symbol_links(symbol))
                        .borrow()
                        .type_parameters
                        .clone()
                } else {
                    None
                }
                .or_else(|| {
                    if get_object_flags(&type_).intersects(ObjectFlags::Reference) {
                        type_
                            .as_type_reference()
                            .target
                            .as_interface_type_interface()
                            .maybe_local_type_parameters()
                            .map(ToOwned::to_owned)
                    } else {
                        None
                    }
                });
            }
        }
        None
    }

    pub(super) fn check_type_reference_node(
        &self,
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
    ) {
        let node_as_has_type_arguments = node.as_has_type_arguments();
        self.check_grammar_type_arguments(node, node_as_has_type_arguments.maybe_type_arguments());
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
        maybe_for_each(
            node_as_has_type_arguments.maybe_type_arguments(),
            |type_argument, _| {
                self.check_source_element(Some(&**type_argument));
                Option::<()>::None
            },
        );
        let type_ = self.get_type_from_type_reference(node);
        if !self.is_error_type(&type_) {
            if node_as_has_type_arguments.maybe_type_arguments().is_some()
                && self.produce_diagnostics
            {
                let type_parameters = self.get_type_parameters_for_type_reference(node);
                if let Some(type_parameters) = type_parameters.as_ref() {
                    self.check_type_argument_constraints(node, type_parameters);
                }
            }
            let symbol = (*self.get_node_links(node))
                .borrow()
                .resolved_symbol
                .clone();
            if let Some(symbol) = symbol.as_ref() {
                if some(
                    symbol.maybe_declarations().as_deref(),
                    Some(|d: &Rc<Node>| {
                        self.is_type_declaration(d) && d.flags().intersects(NodeFlags::Deprecated)
                    }),
                ) {
                    self.add_deprecated_suggestion(
                        &self.get_deprecated_suggestion_node(node),
                        symbol.maybe_declarations().as_ref().unwrap(),
                        &**symbol.escaped_name(),
                    );
                }
                if type_.flags().intersects(TypeFlags::Enum)
                    && symbol.flags().intersects(SymbolFlags::EnumMember)
                {
                    self.error(
                        Some(node),
                        &Diagnostics::Enum_type_0_has_members_with_initializers_that_are_not_literals,
                        Some(vec![
                            self.type_to_string_(
                                &type_,
                                Option::<&Node>::None,
                                None, None,
                            )
                        ])
                    );
                }
            }
        }
    }

    pub(super) fn get_type_argument_constraint_(
        &self,
        node: &Node, /*TypeNode*/
    ) -> Option<Rc<Type>> {
        let type_reference_node = try_cast(node.parent(), |parent: &Rc<Node>| {
            is_type_reference_type(parent)
        })?;
        let type_parameters = self.get_type_parameters_for_type_reference(&type_reference_node)?;
        let constraint = self.get_constraint_of_type_parameter(
            &type_parameters[type_reference_node
                .as_has_type_arguments()
                .maybe_type_arguments()
                .unwrap()
                .into_iter()
                .position(|type_argument| ptr::eq(&**type_argument, node))
                .unwrap()],
        )?;
        Some(self.instantiate_type(
            &constraint,
            Some(&self.create_type_mapper(
                type_parameters.clone(),
                Some(self.get_effective_type_arguments(&type_reference_node, &type_parameters)),
            )),
        ))
    }

    pub(super) fn check_type_query(&self, node: &Node /*TypeQueryNode*/) {
        self.get_type_from_type_query_node(node);
    }

    pub(super) fn check_type_literal(&self, node: &Node /*TypeLiteralNode*/) {
        for_each(
            &node.as_type_literal_node().members,
            |member: &Rc<Node>, _| -> Option<()> {
                self.check_source_element(Some(&**member));
                None
            },
        );
        if self.produce_diagnostics {
            let type_ = self.get_type_from_type_literal_or_function_or_constructor_type_node(node);
            self.check_index_constraints(&type_, &type_.symbol(), None);
            self.check_type_for_duplicate_index_signatures(node);
            self.check_object_type_for_duplicate_declarations(node);
        }
    }

    pub(super) fn check_array_type(&self, node: &Node /*ArrayTypeNode*/) {
        self.check_source_element(Some(&*node.as_array_type_node().element_type));
    }

    pub(super) fn check_tuple_type(&self, node: &Node /*TupleTypeNode*/) {
        let node_as_tuple_type_node = node.as_tuple_type_node();
        let element_types = &node_as_tuple_type_node.elements;
        let mut seen_optional_element = false;
        let mut seen_rest_element = false;
        let has_named_element = some(
            Some(&**element_types),
            Some(|element_type: &Rc<Node>| is_named_tuple_member(element_type)),
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
                let type_ = self.get_type_from_type_node_(&e.as_has_type().maybe_type().unwrap());
                if !self.is_array_like_type(&type_) {
                    self.error(
                        Some(&**e),
                        &Diagnostics::A_rest_element_type_must_be_an_array_type,
                        None,
                    );
                    break;
                }
                if self.is_array_type(&type_)
                    || self.is_tuple_type(&type_)
                        && type_
                            .as_type_reference()
                            .target
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
        for_each(
            &node_as_tuple_type_node.elements,
            |element: &Rc<Node>, _| -> Option<()> {
                self.check_source_element(Some(&**element));
                None
            },
        );
        self.get_type_from_type_node_(node);
    }

    pub(super) fn check_union_or_intersection_type(
        &self,
        node: &Node, /*UnionOrIntersectionTypeNode*/
    ) {
        for_each(
            node.as_union_or_intersection_type_node().types(),
            |type_, _| {
                self.check_source_element(Some(&**type_));
                Option::<()>::None
            },
        );
        self.get_type_from_type_node_(node);
    }

    pub(super) fn check_indexed_access_index_type(
        &self,
        type_: &Type,
        access_node: &Node, /*IndexedAccessTypeNode | ElementAccessExpression*/
    ) -> Rc<Type> {
        if !type_.flags().intersects(TypeFlags::IndexedAccess) {
            return type_.type_wrapper();
        }
        let type_as_indexed_access_type = type_.as_indexed_access_type();
        let object_type = &type_as_indexed_access_type.object_type;
        let index_type = &type_as_indexed_access_type.index_type;
        if self.is_type_assignable_to(
            index_type,
            &self.get_index_type(object_type, Some(false), None),
        ) {
            if access_node.kind() == SyntaxKind::ElementAccessExpression
                && is_assignment_target(access_node)
                && get_object_flags(object_type).intersects(ObjectFlags::Mapped)
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
                    )]),
                );
            }
            return type_.type_wrapper();
        }
        let apparent_object_type = self.get_apparent_type(object_type);
        if self
            .get_index_info_of_type_(&apparent_object_type, &self.number_type())
            .is_some()
            && self.is_type_assignable_to_kind(index_type, TypeFlags::NumberLike, None)
        {
            return type_.type_wrapper();
        }
        if self.is_generic_object_type(object_type) {
            let property_name = self.get_property_name_from_index(index_type, Some(access_node));
            if let Some(property_name) = property_name.as_ref() {
                let property_symbol = self.for_each_type(&apparent_object_type, |t: &Type| {
                    self.get_property_of_type_(t, property_name, None)
                });
                if matches!(
                    property_symbol.as_ref(),
                    Some(property_symbol) if get_declaration_modifier_flags_from_symbol(
                        property_symbol,
                        None,
                    ).intersects(ModifierFlags::NonPublicAccessibilityModifier)
                ) {
                    self.error(
                        Some(access_node),
                        &Diagnostics::Private_or_protected_member_0_cannot_be_accessed_on_a_type_parameter,
                        Some(vec![
                            unescape_leading_underscores(property_name)
                        ])
                    );
                    return self.error_type();
                }
            }
        }
        self.error(
            Some(access_node),
            &Diagnostics::Type_0_cannot_be_used_to_index_type_1,
            Some(vec![
                self.type_to_string_(index_type, Option::<&Node>::None, None, None),
                self.type_to_string_(object_type, Option::<&Node>::None, None, None),
            ]),
        );
        self.error_type()
    }

    pub(super) fn check_indexed_access_type(&self, node: &Node /*IndexedAccessTypeNode*/) {
        let node_as_indexed_access_type_node = node.as_indexed_access_type_node();
        self.check_source_element(Some(&*node_as_indexed_access_type_node.object_type));
        self.check_source_element(Some(&*node_as_indexed_access_type_node.index_type));
        self.check_indexed_access_index_type(
            &self.get_type_from_indexed_access_type_node(node),
            node,
        );
    }

    pub(super) fn check_mapped_type(&self, node: &Node /*MappedTypeNode*/) {
        self.check_grammar_mapped_type(node);
        let node_as_mapped_type_node = node.as_mapped_type_node();
        self.check_source_element(Some(&*node_as_mapped_type_node.type_parameter));
        self.check_source_element(node_as_mapped_type_node.name_type.as_deref());
        self.check_source_element(node_as_mapped_type_node.type_.as_deref());

        if node_as_mapped_type_node.type_.is_none() {
            self.report_implicit_any(node, &self.any_type(), None);
        }

        let type_ = self.get_type_from_mapped_type_node(node);
        let name_type = self.get_name_type_from_mapped_type(&type_);
        if let Some(name_type) = name_type.as_ref() {
            self.check_type_assignable_to(
                name_type,
                &self.keyof_constraint_type(),
                node_as_mapped_type_node.name_type.as_deref(),
                None,
                None,
                None,
            );
        } else {
            let constraint_type = self.get_constraint_type_from_mapped_type(&type_);
            self.check_type_assignable_to(
                &constraint_type,
                &self.keyof_constraint_type(),
                get_effective_constraint_of_type_parameter(
                    &node_as_mapped_type_node.type_parameter,
                ),
                None,
                None,
                None,
            );
        }
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

    pub(super) fn check_this_type(&self, node: &Node /*ThisTypeNode*/) {
        self.get_type_from_this_type_node(node);
    }

    pub(super) fn check_type_operator(&self, node: &Node /*TypeOperatorNode*/) {
        self.check_grammar_type_operator_node(node);
        self.check_source_element(Some(&*node.as_type_operator_node().type_));
    }

    pub(super) fn check_conditional_type(&self, node: &Node /*ConditionalTypeNode*/) {
        for_each_child(
            node,
            |child: &Node| self.check_source_element(Some(child)),
            Option::<fn(&NodeArray)>::None,
        );
    }

    pub(super) fn check_infer_type(&self, node: &Node /*InferTypeNode*/) {
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
        self.check_source_element(Some(&*node.as_infer_type_node().type_parameter));
        self.register_for_unused_identifiers_check(node);
    }

    pub(super) fn check_template_literal_type(&self, node: &Node /*TemplateLiteralTypeNode*/) {
        for span in &node.as_template_literal_type_node().template_spans {
            let span_as_template_literal_type_span = span.as_template_literal_type_span();
            self.check_source_element(Some(&*span_as_template_literal_type_span.type_));
            let type_ = self.get_type_from_type_node_(&span_as_template_literal_type_span.type_);
            self.check_type_assignable_to(
                &type_,
                &self.template_constraint_type(),
                Some(&*span_as_template_literal_type_span.type_),
                None,
                None,
                None,
            );
        }
        self.get_type_from_type_node_(node);
    }

    pub(super) fn check_import_type(&self, node: &Node /*ImportTypeNode*/) {
        self.check_source_element(Some(&*node.as_import_type_node().argument));
        self.get_type_from_type_node_(node);
    }

    pub(super) fn check_named_tuple_member(&self, node: &Node /*NamedTupleMember*/) {
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
        self.check_source_element(Some(&*node_as_named_tuple_member.type_));
        self.get_type_from_type_node_(node);
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

    pub(super) fn check_function_or_constructor_symbol(&self, symbol: &Symbol) {
        if !self.produce_diagnostics {
            return;
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
        let mut body_declaration: Option<Rc<Node /*FunctionLikeDeclaration*/>> = None;
        let mut last_seen_non_ambient_declaration: Option<Rc<Node /*FunctionLikeDeclaration*/>> =
            None;
        let mut previous_declaration: Option<Rc<Node /*SignatureDeclaration*/>> = None;

        let declarations = symbol.maybe_declarations();
        let is_constructor = symbol.flags().intersects(SymbolFlags::Constructor);

        let mut duplicate_function_declaration = false;
        let mut multiple_constructor_implementation = false;
        let mut has_non_ambient_class = false;
        let mut function_declarations: Vec<Rc<Node /*Declaration*/>> = vec![];
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
                    let body_is_present =
                        node_is_present(node.as_function_like_declaration().maybe_body());

                    if body_is_present && body_declaration.is_some() {
                        if is_constructor {
                            multiple_constructor_implementation = true;
                        } else {
                            duplicate_function_declaration = true;
                        }
                    } else if are_option_rcs_equal(
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
                |declaration: &Rc<Node>, _| -> Option<()> {
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
                |declaration: &Rc<Node>, _| -> Option<()> {
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
            && symbol.flags().intersects(SymbolFlags::Function)
        {
            if let Some(declarations) = declarations.as_ref() {
                let related_diagnostics: Vec<Rc<DiagnosticRelatedInformation>> =
                    filter(declarations, |d: &Rc<Node>| {
                        d.kind() == SyntaxKind::ClassDeclaration
                    })
                    .iter()
                    .map(|d| {
                        Rc::new(
                            create_diagnostic_for_node(
                                d,
                                &Diagnostics::Consider_adding_a_declare_modifier_to_this_class,
                                None,
                            )
                            .into(),
                        )
                    })
                    .collect();

                for_each(declarations, |declaration: &Rc<Node>, _| -> Option<()> {
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
                                Some(vec![symbol_name(symbol)]),
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
                let signatures = self.get_signatures_of_symbol(Some(symbol));
                let body_signature = self.get_signature_from_declaration_(body_declaration);
                for signature in &signatures {
                    if !self.is_implementation_compatible_with_overload(
                        body_signature.clone(),
                        signature.clone(),
                    ) {
                        add_related_info(
                            &self.error(
                                signature.declaration.as_deref(),
                                &Diagnostics::This_overload_signature_is_not_compatible_with_its_implementation_signature,
                                None,
                            ),
                            vec![
                                Rc::new(
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
    }

    pub(super) fn get_canonical_overload<TImplementation: Borrow<Node>>(
        &self,
        overloads: &[Rc<Node /*Declaration*/>],
        implementation: Option<TImplementation /*FunctionLikeDeclaration*/>,
    ) -> Rc<Node /*Declaration*/> {
        let implementation =
            implementation.map(|implementation| implementation.borrow().node_wrapper());
        let implementation_shares_container_with_first_overload = matches!(
            implementation.as_ref(),
            Some(implementation) if are_option_rcs_equal(
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
        overloads: &[Rc<Node /*Declaration*/>],
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
            for_each(overloads, |o: &Rc<Node>, _| -> Option<()> {
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
        overloads: &[Rc<Node /*Declaration*/>],
        implementation: Option<TImplementation /*FunctionLikeDeclaration*/>,
        some_have_question_token: bool,
        all_have_question_token: bool,
    ) {
        if some_have_question_token != all_have_question_token {
            let canonical_has_question_token =
                has_question_token(&self.get_canonical_overload(overloads, implementation));
            for_each(overloads, |o: &Rc<Node>, _| -> Option<()> {
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

    pub(super) fn report_implementation_expected_error(
        &self,
        is_constructor: bool,
        node: &Node, /*SignatureDeclaration*/
    ) {
        let node_as_signature_declaration = node.as_signature_declaration();
        if matches!(
            node_as_signature_declaration.maybe_name().as_ref(),
            Some(node_name) if node_is_missing(Some(&**node_name))
        ) {
            return;
        }

        let mut seen = false;
        let subsequent_node = for_each_child_returns(
            &node.parent(),
            |c: &Node| {
                if seen {
                    Some(c.node_wrapper())
                } else {
                    seen = ptr::eq(c, node);
                    None
                }
            },
            Option::<fn(&NodeArray) -> Option<Rc<Node>>>::None,
        );
        if let Some(subsequent_node) = subsequent_node
            .as_ref()
            .filter(|subsequent_node| subsequent_node.pos() == node.end())
        {
            if subsequent_node.kind() == node.kind() {
                let error_node = subsequent_node
                    .as_named_declaration()
                    .maybe_name()
                    .unwrap_or_else(|| subsequent_node.clone());
                let subsequent_name = subsequent_node.as_named_declaration().maybe_name();
                if let (Some(node_name), Some(subsequent_name)) = (
                    node.as_named_declaration().maybe_name().as_ref(),
                    subsequent_name.as_ref(),
                ) {
                    if is_private_identifier(node_name)
                        && is_private_identifier(subsequent_name)
                        && node_name.as_private_identifier().escaped_text
                            == subsequent_name.as_private_identifier().escaped_text
                        || is_computed_property_name(node_name)
                            && is_computed_property_name(subsequent_name)
                        || is_property_name_literal(node_name)
                            && is_property_name_literal(subsequent_name)
                            && get_escaped_text_of_identifier_or_literal(node_name)
                                == get_escaped_text_of_identifier_or_literal(subsequent_name)
                    {
                        let report_error = matches!(
                            node.kind(),
                            SyntaxKind::MethodDeclaration | SyntaxKind::MethodSignature
                        ) && is_static(node) != is_static(subsequent_node);
                        if report_error {
                            let diagnostic = if is_static(node) {
                                &*Diagnostics::Function_overload_must_be_static
                            } else {
                                &*Diagnostics::Function_overload_must_not_be_static
                            };
                            self.error(Some(&*error_node), diagnostic, None);
                        }
                        return;
                    }
                }
                if node_is_present(subsequent_node.as_function_like_declaration().maybe_body()) {
                    self.error(
                        Some(&*error_node),
                        &Diagnostics::Function_implementation_name_must_be_0,
                        Some(vec![declaration_name_to_string(
                            node.as_named_declaration().maybe_name(),
                        )
                        .into_owned()]),
                    );
                    return;
                }
            }
        }
        let error_node = node
            .as_signature_declaration()
            .maybe_name()
            .unwrap_or_else(|| node.node_wrapper());
        if is_constructor {
            self.error(
                Some(&*error_node),
                &Diagnostics::Constructor_implementation_is_missing,
                None,
            );
        } else {
            if has_syntactic_modifier(node, ModifierFlags::Abstract) {
                self.error(
                    Some(&*error_node),
                    &Diagnostics::All_declarations_of_an_abstract_method_must_be_consecutive,
                    None,
                );
            } else {
                self.error(
                    Some(&*error_node),
                    &Diagnostics::Function_implementation_is_missing_or_not_immediately_following_the_declaration,
                    None,
                );
            }
        }
    }

    pub(super) fn get_awaited_type_of_promise<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_promised_type_of_promise<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn check_awaited_type(
        &self,
        type_: &Type,
        with_alias: bool,
        error_node: &Node,
        diagnostic_message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_awaited_type_instantiation(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Conditional) {
            unimplemented!()
        }
        false
    }

    pub(super) fn unwrap_awaited_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_| Some(self.unwrap_awaited_type(type_)),
                None,
            )
            .unwrap()
        } else if self.is_awaited_type_instantiation(type_) {
            unimplemented!()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_awaited_type_<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_awaited_type_no_alias<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Option<Rc<Type>> {
        if self.is_type_any(Some(type_)) {
            return Some(type_.type_wrapper());
        }

        if self.is_awaited_type_instantiation(type_) {
            return Some(type_.type_wrapper());
        }

        unimplemented!()
    }

    pub(super) fn check_async_function_return_type(
        &self,
        node: &Node,             /*FunctionLikeDeclaration | MethodSignature*/
        return_type_node: &Node, /*TypeNode*/
    ) {
        unimplemented!()
    }

    pub(super) fn check_decorators(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn check_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        if self.produce_diagnostics {
            self.check_function_or_method_declaration(node);
        }
    }

    pub(super) fn check_function_or_method_declaration(
        &self,
        node: &Node, /*FunctionDeclaration | MethodDeclaration | MethodSignature*/
    ) {
        // self.check_decorators(node);
        // self.check_signature_declaration(node);
    }

    pub(super) fn register_for_unused_identifiers_check(
        &self,
        node: &Node, /*PotentiallyUnusedIdentifier*/
    ) {
        unimplemented!()
    }

    pub(super) fn check_unused_identifiers<
        TAddDiagnostic: FnMut(&Node, UnusedKind, Rc<Diagnostic>),
    >(
        &self,
        potentially_unused_identifiers: &[Rc<Node /*PotentiallyUnusedIdentifier*/>],
        add_diagnostic: TAddDiagnostic, /*AddUnusedDiagnostic*/
    ) {
        unimplemented!()
    }

    pub(super) fn check_block(&self, node: &Node /*Block*/) {
        let node_as_block = node.as_block();
        if is_function_or_module_block(node) {
            for_each(&node_as_block.statements, |statement, _| {
                self.check_source_element(Some(statement.clone()));
                Option::<()>::None
            });
        } else {
            for_each(&node_as_block.statements, |statement, _| {
                self.check_source_element(Some(statement.clone()));
                Option::<()>::None
            });
        }
    }

    pub(super) fn check_collision_with_arguments_in_generated_code(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) {
        unimplemented!()
    }

    pub(super) fn check_collisions_for_declaration_name<TName: Borrow<Node>>(
        &self,
        node: &Node,
        name: Option<TName /*Identifier*/>,
    ) {
        unimplemented!()
    }

    pub(super) fn convert_auto_to_any(&self, type_: &Type) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn check_variable_like_declaration(&self, node: &Node) {
        let node_as_variable_like_declaration = node.as_variable_like_declaration();
        if !is_binding_element(node) {
            self.check_source_element(node_as_variable_like_declaration.maybe_type());
        }

        let symbol = self.get_symbol_of_node(node).unwrap();

        let type_ = self.convert_auto_to_any(&self.get_type_of_symbol(&*symbol));
        let value_declaration = symbol.maybe_value_declaration();
        if value_declaration.is_some() && ptr::eq(node, &*value_declaration.unwrap()) {
            let initializer = get_effective_initializer(node);
            if let Some(initializer) = initializer {
                if true {
                    let initializer_type = self.check_expression_cached(&initializer, None);
                    self.check_type_assignable_to_and_optionally_elaborate(
                        &initializer_type,
                        &type_,
                        Some(node),
                        Some(&*initializer),
                        None,
                        None,
                    );
                }
            }
        } else {
            unimplemented!()
        }
    }

    pub(super) fn error_next_variable_or_property_declaration_must_have_same_type<
        TFirstDeclaration: Borrow<Node>,
    >(
        &self,
        first_declaration: Option<TFirstDeclaration /*Declaration*/>,
        first_type: &Type,
        next_declaration: &Node, /*Declaration*/
        next_type: &Type,
    ) {
        unimplemented!()
    }

    pub(super) fn check_variable_declaration(&self, node: &Node /*VariableDeclaration*/) {
        self.check_variable_like_declaration(node);
    }

    pub(super) fn check_variable_statement(&self, node: &Node /*VariableStatement*/) {
        for_each(
            &node
                .as_variable_statement()
                .declaration_list
                .as_variable_declaration_list()
                .declarations,
            |declaration, _| Some(self.check_source_element(Some(&**declaration))),
        );
    }

    pub(super) fn check_expression_statement(&self, node: &Node /*ExpressionStatement*/) {
        let expression = &node.as_expression_statement().expression;
        self.check_expression(expression, None, None);
    }

    pub(super) fn check_if_statement(&self, node: &Node /*IfStatement*/) {
        let node_as_if_statement = node.as_if_statement();
        let type_ = self.check_truthiness_expression(&node_as_if_statement.expression, None);
        self.check_source_element(Some(&*node_as_if_statement.then_statement));

        if node_as_if_statement.then_statement.kind() == SyntaxKind::EmptyStatement {
            self.error(
                Some(&*node_as_if_statement.then_statement),
                &Diagnostics::The_body_of_an_if_statement_cannot_be_the_empty_statement,
                None,
            );
        }

        self.check_source_element(node_as_if_statement.else_statement.clone());
    }

    pub(super) fn check_testing_known_truthy_callable_or_awaitable_type<TBody: Borrow<Node>>(
        &self,
        cond_expr: &Node, /*Expression*/
        type_: &Type,
        body: Option<TBody /*Statement | Expression*/>,
    ) {
        unimplemented!()
    }

    pub(super) fn check_truthiness_of_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Void) {
            self.error(
                Some(node),
                &Diagnostics::An_expression_of_type_void_cannot_be_tested_for_truthiness,
                None,
            );
        }

        type_.type_wrapper()
    }

    pub(super) fn check_truthiness_expression(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.check_truthiness_of_type(&self.check_expression(node, check_mode, None), node)
    }

    pub(super) fn check_right_hand_side_of_for_of(
        &self,
        statement: &Node, /*ForOfStatement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_iterated_type_or_element_type<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        input_type: &Type,
        sent_type: &Type,
        error_node: Option<TErrorNode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_iterated_type_or_element_type<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        input_type: &Type,
        sent_type: &Type,
        error_node: Option<TErrorNode>,
        check_assignability: bool,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_iteration_type_of_iterable<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        type_kind: IterationTypeKind,
        input_type: &Type,
        error_node: Option<TErrorNode>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn create_iteration_types(
        &self,
        yield_type: Option<Rc<Type>>,
        return_type: Option<Rc<Type>>,
        next_type: Option<Rc<Type>>,
    ) -> IterationTypes {
        let yield_type = yield_type.unwrap_or_else(|| self.never_type());
        let return_type = return_type.unwrap_or_else(|| self.never_type());
        let next_type = next_type.unwrap_or_else(|| self.unknown_type());
        IterationTypes::new(yield_type, return_type, next_type)
    }

    pub(super) fn get_iteration_types_of_iterable<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        use_: IterationUse,
        error_node: Option<TErrorNode>,
    ) -> Option<IterationTypes> {
        unimplemented!()
    }

    pub(super) fn get_iteration_types_of_global_iterable_type(
        &self,
        global_type: &Type,
        resolver: &IterationTypesResolver,
    ) -> IterationTypes {
        unimplemented!()
    }

    pub(super) fn get_iteration_type_of_generator_function_return_type(
        &self,
        kind: IterationTypeKind,
        return_type: &Type,
        is_async_generator: bool,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_iteration_types_of_generator_function_return_type(
        &self,
        type_: &Type,
        is_async_generator: bool,
    ) -> Option<IterationTypes> {
        unimplemented!()
    }

    pub(super) fn unwrap_return_type(
        &self,
        return_type: &Type,
        function_flags: FunctionFlags,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_unwrapped_return_type_void_or_any(
        &self,
        func: &Node, /*SignatureDeclaration*/
        return_type: &Type,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_return_statement(&self, node: &Node /*ReturnStatement*/) {
        let container = get_containing_function_or_class_static_block(node);

        if container.is_none() {
            unimplemented!()
        }
        let container = container.unwrap();

        let signature = self.get_signature_from_declaration_(&container);
        let return_type = self.get_return_type_of_signature(signature);
        let function_flags = get_function_flags(Some(&*container));
        let node_as_return_statement = node.as_return_statement();
        if self.strict_null_checks
            || node_as_return_statement.expression.is_some()
            || return_type.flags().intersects(TypeFlags::Never)
        {
            let expr_type = match node_as_return_statement.expression.as_ref() {
                Some(expression) => self.check_expression_cached(&expression, None),
                None => self.undefined_type(),
            };
            if false {
                unimplemented!()
            } else if self.get_return_type_from_annotation(&container).is_some() {
                let unwrapped_return_type = self
                    .unwrap_return_type(&return_type, function_flags)/*.unwrap_or(return_type)*/;
                let unwrapped_expr_type = if function_flags.intersects(FunctionFlags::Async) {
                    self.check_awaited_type(&expr_type, false, node, &Diagnostics::The_return_type_of_an_async_function_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member, None)
                } else {
                    expr_type
                };
                // if unwrappedReturnType {
                self.check_type_assignable_to_and_optionally_elaborate(
                    &unwrapped_expr_type,
                    &unwrapped_return_type,
                    Some(node),
                    node_as_return_statement.expression.clone(),
                    None,
                    None,
                );
                // }
            }
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
}
