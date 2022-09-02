#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use super::{
    CheckMode, IterationTypeKind, IterationUse, ResolveCallContainingMessageChain, UnusedKind,
};
use crate::{
    chain_diagnostic_messages, entity_name_to_string, find_last, for_each,
    get_containing_function_or_class_static_block, get_declaration_of_kind,
    get_effective_initializer, get_effective_return_type_node, get_effective_type_annotation_node,
    get_effective_type_parameter_declarations, get_entity_name_from_type_node,
    get_first_constructor_with_body, get_first_identifier, get_function_flags,
    get_host_signature_from_jsdoc, get_jsdoc_tags, get_parameter_symbol_from_jsdoc,
    get_rest_parameter_element_type, id_text, is_binding_element, is_binding_pattern,
    is_entity_name, is_function_or_module_block, is_identifier, is_jsdoc_construct_signature,
    is_jsdoc_parameter_tag, is_qualified_name, is_rest_parameter, node_can_be_decorated, Debug_,
    Diagnostic, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, ExternalEmitHelpers,
    FunctionFlags, HasTypeInterface, HasTypeParametersInterface, IterationTypes,
    IterationTypesResolver, NamedDeclarationInterface, Node, NodeInterface, ScriptTarget, Symbol,
    SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn check_async_function_return_type(
        &self,
        node: &Node,             /*FunctionLikeDeclaration | MethodSignature*/
        return_type_node: &Node, /*TypeNode*/
    ) {
        let return_type = self.get_type_from_type_node_(return_type_node);

        if self.language_version >= ScriptTarget::ES2015 {
            if self.is_error_type(&return_type) {
                return;
            }
            let global_promise_type = self.get_global_promise_type(true);
            if !Rc::ptr_eq(&global_promise_type, &self.empty_generic_type())
                && !self.is_reference_to_type(&return_type, &global_promise_type)
            {
                self.error(
                    Some(return_type_node),
                    &Diagnostics::The_return_type_of_an_async_function_or_method_must_be_the_global_Promise_T_type_Did_you_mean_to_write_Promise_0,
                    Some(vec![
                        self.type_to_string_(
                            &self.get_awaited_type_no_alias(
                                &return_type,
                                Option::<&Node>::None,
                                None, None,
                            ).unwrap_or_else(|| self.void_type()),
                            Option::<&Node>::None,
                            None, None,
                        )
                    ])
                );
                return;
            }
        } else {
            self.mark_type_node_as_referenced(return_type_node);

            if self.is_error_type(&return_type) {
                return;
            }

            let promise_constructor_name = get_entity_name_from_type_node(return_type_node);
            if promise_constructor_name.is_none() {
                self.error(
                    Some(return_type_node),
                    &Diagnostics::Type_0_is_not_a_valid_async_function_return_type_in_ES5_SlashES3_because_it_does_not_refer_to_a_Promise_compatible_constructor_value,
                    Some(vec![
                        self.type_to_string_(
                            &return_type,
                            Option::<&Node>::None,
                            None, None
                        )
                    ])
                );
                return;
            }
            let promise_constructor_name = promise_constructor_name.unwrap();

            let promise_constructor_symbol = self.resolve_entity_name(
                &promise_constructor_name,
                SymbolFlags::Value,
                Some(true),
                None,
                Option::<&Node>::None,
            );
            let promise_constructor_type =
                if let Some(promise_constructor_symbol) = promise_constructor_symbol.as_ref() {
                    self.get_type_of_symbol(promise_constructor_symbol)
                } else {
                    self.error_type()
                };
            if self.is_error_type(&promise_constructor_type) {
                if promise_constructor_name.kind() == SyntaxKind::Identifier
                    && promise_constructor_name
                        .as_identifier()
                        .escaped_text
                        .eq_str("Promise")
                    && Rc::ptr_eq(
                        &self.get_target_type(&return_type),
                        &self.get_global_promise_type(false),
                    )
                {
                    self.error(
                        Some(return_type_node),
                        &Diagnostics::An_async_function_or_method_in_ES5_SlashES3_requires_the_Promise_constructor_Make_sure_you_have_a_declaration_for_the_Promise_constructor_or_include_ES2015_in_your_lib_option,
                        None,
                    );
                } else {
                    self.error(
                        Some(return_type_node),
                        &Diagnostics::Type_0_is_not_a_valid_async_function_return_type_in_ES5_SlashES3_because_it_does_not_refer_to_a_Promise_compatible_constructor_value,
                        Some(vec![
                            entity_name_to_string(&promise_constructor_name).into_owned()
                        ])
                    );
                }
                return;
            }

            let global_promise_constructor_like_type =
                self.get_global_promise_constructor_like_type(true);
            if Rc::ptr_eq(
                &global_promise_constructor_like_type,
                &self.empty_object_type(),
            ) {
                self.error(
                    Some(return_type_node),
                    &Diagnostics::Type_0_is_not_a_valid_async_function_return_type_in_ES5_SlashES3_because_it_does_not_refer_to_a_Promise_compatible_constructor_value,
                    Some(vec![
                        entity_name_to_string(&promise_constructor_name).into_owned()
                    ])
                );
                return;
            }

            if !self.check_type_assignable_to(
                &promise_constructor_type,
                &global_promise_constructor_like_type,
                Some(return_type_node),
                Some(&Diagnostics::Type_0_is_not_a_valid_async_function_return_type_in_ES5_SlashES3_because_it_does_not_refer_to_a_Promise_compatible_constructor_value),
                None, None,
            ) {
                return;
            }

            let root_name = /*promiseConstructorName &&*/ get_first_identifier(&promise_constructor_name);
            let colliding_symbol = self.get_symbol(
                &(**node.locals()).borrow(),
                &root_name.as_identifier().escaped_text,
                SymbolFlags::Value,
            );
            if let Some(colliding_symbol) = colliding_symbol.as_ref() {
                self.error(
                    colliding_symbol.maybe_value_declaration(),
                    &Diagnostics::Duplicate_identifier_0_Compiler_uses_declaration_1_to_support_async_functions,
                    Some(vec![
                        id_text(&root_name),
                        entity_name_to_string(&promise_constructor_name).into_owned(),
                    ])
                );
                return;
            }
        }
        self.check_awaited_type(
            &return_type,
            false,
            node,
            &Diagnostics::The_return_type_of_an_async_function_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member,
            None,
        );
    }

    pub(super) fn check_decorator(&self, node: &Node /*Decorator*/) {
        let signature = self.get_resolved_signature_(node, None, None);
        self.check_deprecated_signature(&signature, node);
        let return_type = self.get_return_type_of_signature(signature.clone());
        if return_type.flags().intersects(TypeFlags::Any) {
            return;
        }

        let expected_return_type: Rc<Type>;
        let head_message = self.get_diagnostic_head_message_for_decorator_resolution(node);
        let mut error_info: Option<Rc<RefCell<DiagnosticMessageChain>>> = None;
        match node.parent().kind() {
            SyntaxKind::ClassDeclaration => {
                let class_symbol = self.get_symbol_of_node(&node.parent()).unwrap();
                let class_constructor_type = self.get_type_of_symbol(&class_symbol);
                expected_return_type = self.get_union_type(
                    vec![class_constructor_type, self.void_type()],
                    None,
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                );
            }

            SyntaxKind::Parameter => {
                expected_return_type = self.void_type();
                error_info = Some(Rc::new(RefCell::new(
                    chain_diagnostic_messages(
                        None,
                        &Diagnostics::The_return_type_of_a_parameter_decorator_function_must_be_either_void_or_any,
                        None,
                    )
                )));
            }

            SyntaxKind::PropertyDeclaration => {
                expected_return_type = self.void_type();
                error_info = Some(Rc::new(RefCell::new(
                    chain_diagnostic_messages(
                        None,
                        &Diagnostics::The_return_type_of_a_property_decorator_function_must_be_either_void_or_any,
                        None,
                    )
                )));
            }

            SyntaxKind::MethodDeclaration | SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                let method_type = self.get_type_of_node(&node.parent());
                let descriptor_type = self.create_typed_property_descriptor_type(&method_type);
                expected_return_type = self.get_union_type(
                    vec![descriptor_type, self.void_type()],
                    None,
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                );
            }

            _ => Debug_.fail(None),
        }

        self.check_type_assignable_to(
            &return_type,
            &expected_return_type,
            Some(node),
            Some(head_message),
            Some(Rc::new(ResolveCallContainingMessageChain::new(error_info))),
            None,
        );
    }

    pub(super) fn mark_type_node_as_referenced(&self, node: &Node /*TypeNode*/) {
        self.mark_entity_name_or_entity_expression_as_reference(
            /*node &&*/ get_entity_name_from_type_node(node),
        );
    }

    pub(super) fn mark_entity_name_or_entity_expression_as_reference<TTypeName: Borrow<Node>>(
        &self,
        type_name: Option<TTypeName /*EntityNameOrEntityNameExpression*/>,
    ) {
        if type_name.is_none() {
            return;
        }
        let type_name = type_name.unwrap();
        let type_name = type_name.borrow();

        let root_name = get_first_identifier(type_name);
        let meaning = if type_name.kind() == SyntaxKind::Identifier {
            SymbolFlags::Type
        } else {
            SymbolFlags::Namespace
        } | SymbolFlags::Alias;
        let root_symbol = self.resolve_name_(
            Some(&*root_name),
            &root_name.as_identifier().escaped_text,
            meaning,
            None,
            Option::<Rc<Node>>::None,
            true,
            None,
        );
        if let Some(root_symbol) = root_symbol.as_ref().filter(|root_symbol| {
            root_symbol.flags().intersects(SymbolFlags::Alias)
                && self.symbol_is_value(root_symbol)
                && !self.is_const_enum_or_const_enum_only_module(&self.resolve_alias(root_symbol))
                && self.get_type_only_alias_declaration(root_symbol).is_none()
        }) {
            self.mark_alias_symbol_as_referenced(root_symbol);
        }
    }

    pub(super) fn mark_decorator_medata_data_type_node_as_referenced<TNode: Borrow<Node>>(
        &self,
        node: Option<TNode /*TypeNode*/>,
    ) {
        let entity_name = self.get_entity_name_for_decorator_metadata(node);
        if let Some(entity_name) = entity_name
            .as_ref()
            .filter(|entity_name| is_entity_name(entity_name))
        {
            self.mark_entity_name_or_entity_expression_as_reference(Some(&**entity_name));
        }
    }

    pub(super) fn get_entity_name_for_decorator_metadata<TNode: Borrow<Node>>(
        &self,
        node: Option<TNode /*TypeNode*/>,
    ) -> Option<Rc<Node /*EntityName*/>> {
        let node = node?;
        let node: &Node = node.borrow();
        match node.kind() {
            SyntaxKind::IntersectionType | SyntaxKind::UnionType => self
                .get_entity_name_for_decorator_metadata_from_type_list(
                    node.as_union_or_intersection_type_node().types(),
                ),

            SyntaxKind::ConditionalType => {
                let node_as_conditional_type_node = node.as_conditional_type_node();
                self.get_entity_name_for_decorator_metadata_from_type_list(&[
                    node_as_conditional_type_node.true_type.clone(),
                    node_as_conditional_type_node.false_type.clone(),
                ])
            }

            SyntaxKind::ParenthesizedType | SyntaxKind::NamedTupleMember => {
                self.get_entity_name_for_decorator_metadata(node.as_has_type().maybe_type())
            }

            SyntaxKind::TypeReference => Some(node.as_type_reference_node().type_name.clone()),
            _ => None,
        }
    }

    pub(super) fn get_entity_name_for_decorator_metadata_from_type_list(
        &self,
        types: &[Rc<Node /*TypeNode*/>],
    ) -> Option<Rc<Node /*EntityName*/>> {
        let mut common_entity_name: Option<Rc<Node /*EntityName*/>> = None;
        for type_node in types {
            let mut type_node = type_node.clone();
            while matches!(
                type_node.kind(),
                SyntaxKind::ParenthesizedType | SyntaxKind::NamedTupleMember
            ) {
                type_node = type_node.as_has_type().maybe_type().unwrap();
            }
            if type_node.kind() == SyntaxKind::NeverKeyword {
                continue;
            }
            if !self.strict_null_checks
                && (type_node.kind() == SyntaxKind::LiteralType
                    && type_node.as_literal_type_node().literal.kind() == SyntaxKind::NullKeyword
                    || type_node.kind() == SyntaxKind::UndefinedKeyword)
            {
                continue;
            }
            let individual_entity_name =
                self.get_entity_name_for_decorator_metadata(Some(&*type_node))?;

            if let Some(common_entity_name) = common_entity_name.as_ref() {
                if !is_identifier(common_entity_name)
                    || !is_identifier(&individual_entity_name)
                    || common_entity_name.as_identifier().escaped_text
                        != individual_entity_name.as_identifier().escaped_text
                {
                    return None;
                }
            } else {
                common_entity_name = Some(individual_entity_name);
            }
        }
        common_entity_name
    }

    pub(super) fn get_parameter_type_node_for_decorator_check(
        &self,
        node: &Node, /*ParameterDeclaration*/
    ) -> Option<Rc<Node /*TypeNode*/>> {
        let type_node = get_effective_type_annotation_node(node);
        if is_rest_parameter(node) {
            get_rest_parameter_element_type(type_node)
        } else {
            type_node
        }
    }

    pub(super) fn check_decorators(&self, node: &Node) {
        let node_decorators = node.maybe_decorators();
        if node_decorators.is_none() {
            return;
        }
        let node_decorators = node_decorators.as_ref().unwrap();

        if !node_can_be_decorated(node, Some(node.parent()), node.parent().maybe_parent()) {
            return;
        }

        if self.compiler_options.experimental_decorators != Some(true) {
            self.error(
                Some(node),
                &Diagnostics::Experimental_support_for_decorators_is_a_feature_that_is_subject_to_change_in_a_future_release_Set_the_experimentalDecorators_option_in_your_tsconfig_or_jsconfig_to_remove_this_warning,
                None,
            );
        }

        let first_decorator = &node_decorators[0];
        self.check_external_emit_helpers(first_decorator, ExternalEmitHelpers::Decorate);
        if node.kind() == SyntaxKind::Parameter {
            self.check_external_emit_helpers(first_decorator, ExternalEmitHelpers::Param);
        }

        if self.compiler_options.emit_decorator_metadata == Some(true) {
            self.check_external_emit_helpers(first_decorator, ExternalEmitHelpers::Metadata);

            match node.kind() {
                SyntaxKind::ClassDeclaration => {
                    let constructor = get_first_constructor_with_body(node);
                    if let Some(constructor) = constructor.as_ref() {
                        for parameter in constructor.as_signature_declaration().parameters() {
                            self.mark_decorator_medata_data_type_node_as_referenced(
                                self.get_parameter_type_node_for_decorator_check(parameter),
                            );
                        }
                    }
                }

                SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                    let other_kind = if node.kind() == SyntaxKind::GetAccessor {
                        SyntaxKind::SetAccessor
                    } else {
                        SyntaxKind::GetAccessor
                    };
                    let other_accessor = get_declaration_of_kind(
                        &self.get_symbol_of_node(node).unwrap(),
                        other_kind,
                    );
                    self.mark_decorator_medata_data_type_node_as_referenced(
                        self.get_annotated_accessor_type_node(Some(node))
                            .or_else(|| {
                                other_accessor.as_ref().and_then(|other_accessor| {
                                    self.get_annotated_accessor_type_node(Some(&**other_accessor))
                                })
                            }),
                    );
                }
                SyntaxKind::MethodDeclaration => {
                    for parameter in node.as_function_like_declaration().parameters() {
                        self.mark_decorator_medata_data_type_node_as_referenced(
                            self.get_parameter_type_node_for_decorator_check(parameter),
                        );
                    }

                    self.mark_decorator_medata_data_type_node_as_referenced(
                        get_effective_return_type_node(node),
                    );
                }

                SyntaxKind::PropertyDeclaration => {
                    self.mark_decorator_medata_data_type_node_as_referenced(
                        get_effective_type_annotation_node(node),
                    );
                }

                SyntaxKind::Parameter => {
                    self.mark_decorator_medata_data_type_node_as_referenced(
                        self.get_parameter_type_node_for_decorator_check(node),
                    );
                    let containing_signature = node.parent();
                    for parameter in containing_signature.as_signature_declaration().parameters() {
                        self.mark_decorator_medata_data_type_node_as_referenced(
                            self.get_parameter_type_node_for_decorator_check(parameter),
                        );
                    }
                }
                _ => (),
            }
        }

        for_each(node_decorators, |decorator: &Rc<Node>, _| -> Option<()> {
            self.check_decorator(decorator);
            None
        });
    }

    pub(super) fn check_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        if self.produce_diagnostics {
            self.check_function_or_method_declaration(node);
            self.check_grammar_for_generator(node);
            self.check_collisions_for_declaration_name(
                node,
                node.as_function_declaration().maybe_name(),
            );
        }
    }

    pub(super) fn check_jsdoc_type_alias_tag(
        &self,
        node: &Node, /*JSDocTypedefTag | JSDocCallbackTag*/
    ) {
        let node_as_jsdoc_type_like_tag = node.as_jsdoc_type_like_tag();
        let node_as_named_declaration = node.as_named_declaration();
        if node_as_jsdoc_type_like_tag
            .maybe_type_expression()
            .is_none()
        {
            self.error(
                node_as_named_declaration.maybe_name(),
                &Diagnostics::JSDoc_typedef_tag_should_either_have_a_type_annotation_or_be_followed_by_property_or_member_tags,
                None,
            );
        }

        if let Some(node_name) = node_as_named_declaration.maybe_name().as_ref() {
            self.check_type_name_is_reserved(node_name, &Diagnostics::Type_alias_name_cannot_be_0);
        }
        self.check_source_element(node_as_jsdoc_type_like_tag.maybe_type_expression());
        self.check_type_parameters(Some(&get_effective_type_parameter_declarations(node)));
    }

    pub(super) fn check_jsdoc_template_tag(&self, node: &Node /*JSDocTemplateTag*/) {
        let node_as_jsdoc_template_tag = node.as_jsdoc_template_tag();
        self.check_source_element(node_as_jsdoc_template_tag.constraint.as_deref());
        for tp in &node_as_jsdoc_template_tag.type_parameters {
            self.check_source_element(Some(&**tp));
        }
    }

    pub(super) fn check_jsdoc_type_tag(&self, node: &Node /*JSDocTypeTag*/) {
        self.check_source_element(node.as_jsdoc_type_like_tag().maybe_type_expression());
    }

    pub(super) fn check_jsdoc_parameter_tag(&self, node: &Node /*JSDocParameterTag*/) {
        let node_as_jsdoc_property_like_tag = node.as_jsdoc_property_like_tag();
        self.check_source_element(node_as_jsdoc_property_like_tag.type_expression.as_deref());
        if get_parameter_symbol_from_jsdoc(node).is_none() {
            let decl = get_host_signature_from_jsdoc(node);
            if let Some(decl) = decl.as_ref() {
                let i: Option<usize> = get_jsdoc_tags(decl)
                    .iter()
                    .filter(|jsdoc_tag| is_jsdoc_parameter_tag(jsdoc_tag))
                    .position(|jsdoc_tag| ptr::eq(&**jsdoc_tag, node));
                if matches!(
                    i,
                    Some(i) if {
                        let decl_parameters = decl.as_signature_declaration().parameters();
                        i < decl_parameters.len() && is_binding_pattern(decl_parameters[i].as_parameter_declaration().maybe_name())
                    }
                ) {
                    return;
                }
                if !self.contains_arguments_reference(decl) {
                    if is_qualified_name(&node_as_jsdoc_property_like_tag.name) {
                        self.error(
                            Some(&*node_as_jsdoc_property_like_tag.name),
                            &Diagnostics::Qualified_name_0_is_not_allowed_without_a_leading_param_object_1,
                            Some(vec![
                                entity_name_to_string(&node_as_jsdoc_property_like_tag.name).into_owned(),
                                entity_name_to_string(&node_as_jsdoc_property_like_tag.name.as_qualified_name().left).into_owned(),
                            ])
                        );
                    } else {
                        self.error(
                            Some(&*node_as_jsdoc_property_like_tag.name),
                            &Diagnostics::JSDoc_param_tag_has_name_0_but_there_is_no_parameter_with_that_name,
                            Some(vec![
                                id_text(&node_as_jsdoc_property_like_tag.name)
                            ])
                        );
                    }
                } else if matches!(
                    find_last(
                        &*get_jsdoc_tags(decl),
                        |jsdoc_tag: &Rc<Node>, _| is_jsdoc_parameter_tag(jsdoc_tag)
                    ),
                    Some(jsdoc_tag) if ptr::eq(&**jsdoc_tag, node)
                ) && matches!(
                    node_as_jsdoc_property_like_tag.type_expression.as_ref().map(|node_type_expression| {
                        node_type_expression.as_jsdoc_type_expression().type_.clone()
                    }).as_ref(),
                    Some(node_type_expression_type) if !self.is_array_type(
                        &self.get_type_from_type_node_(
                            node_type_expression_type
                        )
                    )
                ) {
                    self.error(
                        Some(&*node_as_jsdoc_property_like_tag.name),
                        &Diagnostics::JSDoc_param_tag_has_name_0_but_there_is_no_parameter_with_that_name_It_would_match_arguments_if_it_had_an_array_type,
                        Some(vec![
                            id_text(
                                &*if node_as_jsdoc_property_like_tag.name.kind() == SyntaxKind::QualifiedName {
                                    node_as_jsdoc_property_like_tag.name.as_qualified_name().right.clone()
                                } else {
                                    node_as_jsdoc_property_like_tag.name.clone()
                                }
                            )
                        ])
                    );
                }
            }
        }
    }

    pub(super) fn check_jsdoc_property_tag(&self, node: &Node /*JSDocPropertyTag*/) {
        let node_as_jsdoc_property_like_tag = node.as_jsdoc_property_like_tag();
        self.check_source_element(node_as_jsdoc_property_like_tag.type_expression.as_deref());
    }

    pub(super) fn check_jsdoc_function_type(&self, node: &Node /*JSDocFunctionType*/) {
        if self.produce_diagnostics
            && node.as_jsdoc_function_type().maybe_type().is_none()
            && !is_jsdoc_construct_signature(node)
        {
            self.report_implicit_any(node, &self.any_type(), None);
        }
        self.check_signature_declaration(node);
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
