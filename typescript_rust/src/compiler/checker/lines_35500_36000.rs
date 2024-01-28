use std::{borrow::Borrow, cell::RefCell, io, ptr, rc::Rc};

use gc::Gc;
use id_arena::Id;

use super::ResolveCallContainingMessageChain;
use crate::{
    chain_diagnostic_messages, entity_name_to_string, get_declaration_of_kind,
    get_effective_return_type_node, get_effective_type_annotation_node,
    get_effective_type_parameter_declarations, get_entity_name_from_type_node,
    get_first_constructor_with_body, get_first_identifier, get_host_signature_from_jsdoc,
    get_jsdoc_tags, get_parameter_symbol_from_jsdoc, get_rest_parameter_element_type, id_text,
    is_binding_pattern, is_entity_name, is_identifier, is_jsdoc_construct_signature,
    is_jsdoc_parameter_tag, is_qualified_name, is_rest_parameter, node_can_be_decorated,
    try_for_each, Debug_, DiagnosticMessageChain, Diagnostics, ExternalEmitHelpers, HasArena,
    HasTypeInterface, InArena, NamedDeclarationInterface, Node, NodeInterface, OptionTry,
    ScriptTarget, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags,
    TypeInterface, OptionInArena,
};

impl TypeChecker {
    pub(super) fn check_async_function_return_type(
        &self,
        node: Id<Node>,             /*FunctionLikeDeclaration | MethodSignature*/
        return_type_node: Id<Node>, /*TypeNode*/
    ) -> io::Result<()> {
        let return_type = self.get_type_from_type_node_(return_type_node)?;

        if self.language_version >= ScriptTarget::ES2015 {
            if self.is_error_type(return_type) {
                return Ok(());
            }
            let global_promise_type = self.get_global_promise_type(true)?;
            if global_promise_type != self.empty_generic_type()
                && !self.is_reference_to_type(return_type, global_promise_type)
            {
                self.error(
                    Some(return_type_node),
                    &Diagnostics::The_return_type_of_an_async_function_or_method_must_be_the_global_Promise_T_type_Did_you_mean_to_write_Promise_0,
                    Some(vec![
                        self.type_to_string_(
                            self.get_awaited_type_no_alias(
                                return_type,
                                Option::<Id<Node>>::None,
                                None, None,
                            )?.unwrap_or_else(|| self.void_type()),
                            Option::<Id<Node>>::None,
                            None, None,
                        )?
                    ])
                );
                return Ok(());
            }
        } else {
            self.mark_type_node_as_referenced(return_type_node)?;

            if self.is_error_type(return_type) {
                return Ok(());
            }

            let Some(promise_constructor_name) = get_entity_name_from_type_node(return_type_node, self) else {
                self.error(
                    Some(return_type_node),
                    &Diagnostics::Type_0_is_not_a_valid_async_function_return_type_in_ES5_SlashES3_because_it_does_not_refer_to_a_Promise_compatible_constructor_value,
                    Some(vec![
                        self.type_to_string_(
                            return_type,
                            Option::<Id<Node>>::None,
                            None, None
                        )?
                    ])
                );
                return Ok(());
            };

            let promise_constructor_symbol = self.resolve_entity_name(
                promise_constructor_name,
                SymbolFlags::Value,
                Some(true),
                None,
                Option::<Id<Node>>::None,
            )?;
            let promise_constructor_type =
                if let Some(promise_constructor_symbol) = promise_constructor_symbol {
                    self.get_type_of_symbol(promise_constructor_symbol)?
                } else {
                    self.error_type()
                };
            if self.is_error_type(promise_constructor_type) {
                if promise_constructor_name.ref_(self).kind() == SyntaxKind::Identifier
                    && promise_constructor_name.ref_(self).as_identifier().escaped_text == "Promise"
                    && self.get_target_type(return_type) == self.get_global_promise_type(false)?
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
                            entity_name_to_string(promise_constructor_name, self).into_owned()
                        ])
                    );
                }
                return Ok(());
            }

            let global_promise_constructor_like_type =
                self.get_global_promise_constructor_like_type(true)?;
            if global_promise_constructor_like_type == self.empty_object_type() {
                self.error(
                    Some(return_type_node),
                    &Diagnostics::Type_0_is_not_a_valid_async_function_return_type_in_ES5_SlashES3_because_it_does_not_refer_to_a_Promise_compatible_constructor_value,
                    Some(vec![
                        entity_name_to_string(promise_constructor_name, self).into_owned()
                    ])
                );
                return Ok(());
            }

            if !self.check_type_assignable_to(
                promise_constructor_type,
                global_promise_constructor_like_type,
                Some(return_type_node),
                Some(&Diagnostics::Type_0_is_not_a_valid_async_function_return_type_in_ES5_SlashES3_because_it_does_not_refer_to_a_Promise_compatible_constructor_value),
                None, None,
            )? {
                return Ok(());
            }

            let root_name = /*promiseConstructorName &&*/ get_first_identifier(promise_constructor_name, self);
            let colliding_symbol = self.get_symbol(
                &node.ref_(self).locals().ref_(self),
                &root_name.ref_(self).as_identifier().escaped_text,
                SymbolFlags::Value,
            )?;
            if let Some(colliding_symbol) = colliding_symbol {
                self.error(
                    colliding_symbol.ref_(self).maybe_value_declaration(),
                    &Diagnostics::Duplicate_identifier_0_Compiler_uses_declaration_1_to_support_async_functions,
                    Some(vec![
                        id_text(&root_name.ref_(self)).to_owned(),
                        entity_name_to_string(promise_constructor_name, self).into_owned(),
                    ])
                );
                return Ok(());
            }
        }
        self.check_awaited_type(
            return_type,
            false,
            node,
            &Diagnostics::The_return_type_of_an_async_function_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member,
            None,
        )?;

        Ok(())
    }

    pub(super) fn check_decorator(&self, node: Id<Node> /*Decorator*/) -> io::Result<()> {
        let signature = self.get_resolved_signature_(node, None, None)?;
        self.check_deprecated_signature(signature.clone(), node)?;
        let return_type = self.get_return_type_of_signature(signature.clone())?;
        if return_type.ref_(self).flags().intersects(TypeFlags::Any) {
            return Ok(());
        }

        let expected_return_type: Id<Type>;
        let head_message = self.get_diagnostic_head_message_for_decorator_resolution(node);
        let mut error_info: Option<Rc<RefCell<DiagnosticMessageChain>>> = None;
        match node.ref_(self).parent().ref_(self).kind() {
            SyntaxKind::ClassDeclaration => {
                let class_symbol = self.get_symbol_of_node(node.ref_(self).parent())?.unwrap();
                let class_constructor_type = self.get_type_of_symbol(class_symbol)?;
                expected_return_type = self.get_union_type(
                    &[class_constructor_type, self.void_type()],
                    None,
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?;
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
                let method_type = self.get_type_of_node(node.ref_(self).parent())?;
                let descriptor_type = self.create_typed_property_descriptor_type(method_type)?;
                expected_return_type = self.get_union_type(
                    &[descriptor_type, self.void_type()],
                    None,
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?;
            }

            _ => Debug_.fail(None),
        }

        self.check_type_assignable_to(
            return_type,
            expected_return_type,
            Some(node),
            Some(head_message),
            Some(Gc::new(Box::new(ResolveCallContainingMessageChain::new(
                error_info,
            )))),
            None,
        )?;

        Ok(())
    }

    pub(super) fn mark_type_node_as_referenced(
        &self,
        node: Id<Node>, /*TypeNode*/
    ) -> io::Result<()> {
        self.mark_entity_name_or_entity_expression_as_reference(
            /*node &&*/ get_entity_name_from_type_node(node, self),
        )?;

        Ok(())
    }

    pub(super) fn mark_entity_name_or_entity_expression_as_reference(
        &self,
        type_name: Option<Id<Node> /*EntityNameOrEntityNameExpression*/>,
    ) -> io::Result<()> {
        let Some(type_name) = type_name else {
            return Ok(());
        };

        let root_name = get_first_identifier(type_name, self);
        let meaning = if type_name.ref_(self).kind() == SyntaxKind::Identifier {
            SymbolFlags::Type
        } else {
            SymbolFlags::Namespace
        } | SymbolFlags::Alias;
        let root_symbol = self.resolve_name_(
            Some(root_name),
            &root_name.ref_(self).as_identifier().escaped_text,
            meaning,
            None,
            Option::<Id<Node>>::None,
            true,
            None,
        )?;
        if let Some(root_symbol) = root_symbol.try_filter(|&root_symbol| -> io::Result<_> {
            Ok(root_symbol
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::Alias)
                && self.symbol_is_value(root_symbol)?
                && !self.is_const_enum_or_const_enum_only_module(self.resolve_alias(root_symbol)?)
                && self.get_type_only_alias_declaration(root_symbol).is_none())
        })? {
            self.mark_alias_symbol_as_referenced(root_symbol)?;
        }

        Ok(())
    }

    pub(super) fn mark_decorator_medata_data_type_node_as_referenced(
        &self,
        node: Option<Id<Node> /*TypeNode*/>,
    ) -> io::Result<()> {
        let entity_name = self.get_entity_name_for_decorator_metadata(node);
        if let Some(entity_name) = entity_name
            .filter(|entity_name| is_entity_name(&entity_name.ref_(self)))
        {
            self.mark_entity_name_or_entity_expression_as_reference(Some(entity_name))?;
        }

        Ok(())
    }

    pub(super) fn get_entity_name_for_decorator_metadata(
        &self,
        node: Option<Id<Node> /*TypeNode*/>,
    ) -> Option<Id<Node /*EntityName*/>> {
        let node = node?;
        match node.ref_(self).kind() {
            SyntaxKind::IntersectionType | SyntaxKind::UnionType => self
                .get_entity_name_for_decorator_metadata_from_type_list(
                    &*node.ref_(self).as_union_or_intersection_type_node().types().ref_(self),
                ),

            SyntaxKind::ConditionalType => {
                let node_ref = node.ref_(self);
                let node_as_conditional_type_node = node_ref.as_conditional_type_node();
                self.get_entity_name_for_decorator_metadata_from_type_list(&[
                    node_as_conditional_type_node.true_type.clone(),
                    node_as_conditional_type_node.false_type.clone(),
                ])
            }

            SyntaxKind::ParenthesizedType | SyntaxKind::NamedTupleMember => {
                self.get_entity_name_for_decorator_metadata(node.ref_(self).as_has_type().maybe_type())
            }

            SyntaxKind::TypeReference => Some(node.ref_(self).as_type_reference_node().type_name),
            _ => None,
        }
    }

    pub(super) fn get_entity_name_for_decorator_metadata_from_type_list(
        &self,
        types: &[Id<Node /*TypeNode*/>],
    ) -> Option<Id<Node /*EntityName*/>> {
        let mut common_entity_name: Option<Id<Node /*EntityName*/>> = None;
        for &type_node in types {
            let mut type_node = type_node;
            while matches!(
                type_node.ref_(self).kind(),
                SyntaxKind::ParenthesizedType | SyntaxKind::NamedTupleMember
            ) {
                type_node = type_node.ref_(self).as_has_type().maybe_type().unwrap();
            }
            if type_node.ref_(self).kind() == SyntaxKind::NeverKeyword {
                continue;
            }
            if !self.strict_null_checks
                && (type_node.ref_(self).kind() == SyntaxKind::LiteralType
                    && type_node.ref_(self).as_literal_type_node().literal.ref_(self).kind() == SyntaxKind::NullKeyword
                    || type_node.ref_(self).kind() == SyntaxKind::UndefinedKeyword)
            {
                continue;
            }
            let individual_entity_name =
                self.get_entity_name_for_decorator_metadata(Some(type_node))?;

            if let Some(common_entity_name) = common_entity_name {
                if !is_identifier(&common_entity_name.ref_(self))
                    || !is_identifier(&individual_entity_name.ref_(self))
                    || common_entity_name.ref_(self).as_identifier().escaped_text
                        != individual_entity_name.ref_(self).as_identifier().escaped_text
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
        node: Id<Node>, /*ParameterDeclaration*/
    ) -> Option<Id<Node /*TypeNode*/>> {
        let type_node = get_effective_type_annotation_node(node, self);
        if is_rest_parameter(node, self) {
            get_rest_parameter_element_type(type_node, self)
        } else {
            type_node
        }
    }

    pub(super) fn check_decorators(&self, node: Id<Node>) -> io::Result<()> {
        let Some(ref node_decorators) = node.ref_(self).maybe_decorators() else {
            return Ok(());
        };

        if !node_can_be_decorated(node, Some(node.ref_(self).parent()), node.ref_(self).parent().ref_(self).maybe_parent(), self) {
            return Ok(());
        }

        if self.compiler_options.ref_(self).experimental_decorators != Some(true) {
            self.error(
                Some(node),
                &Diagnostics::Experimental_support_for_decorators_is_a_feature_that_is_subject_to_change_in_a_future_release_Set_the_experimentalDecorators_option_in_your_tsconfig_or_jsconfig_to_remove_this_warning,
                None,
            );
        }

        let first_decorator = node_decorators.ref_(self)[0];
        self.check_external_emit_helpers(first_decorator, ExternalEmitHelpers::Decorate)?;
        if node.ref_(self).kind() == SyntaxKind::Parameter {
            self.check_external_emit_helpers(first_decorator, ExternalEmitHelpers::Param)?;
        }

        if self.compiler_options.ref_(self).emit_decorator_metadata == Some(true) {
            self.check_external_emit_helpers(first_decorator, ExternalEmitHelpers::Metadata)?;

            match node.ref_(self).kind() {
                SyntaxKind::ClassDeclaration => {
                    let constructor = get_first_constructor_with_body(node, self);
                    if let Some(constructor) = constructor {
                        for &parameter in &*constructor.ref_(self).as_signature_declaration().parameters().ref_(self) {
                            self.mark_decorator_medata_data_type_node_as_referenced(
                                self.get_parameter_type_node_for_decorator_check(parameter),
                            )?;
                        }
                    }
                }

                SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                    let other_kind = if node.ref_(self).kind() == SyntaxKind::GetAccessor {
                        SyntaxKind::SetAccessor
                    } else {
                        SyntaxKind::GetAccessor
                    };
                    let other_accessor = get_declaration_of_kind(
                        self.get_symbol_of_node(node)?.unwrap(),
                        other_kind,
                        self,
                    );
                    self.mark_decorator_medata_data_type_node_as_referenced(
                        self.get_annotated_accessor_type_node(Some(node))
                            .or_else(|| {
                                other_accessor.and_then(|other_accessor| {
                                    self.get_annotated_accessor_type_node(Some(other_accessor))
                                })
                            }),
                    )?;
                }
                SyntaxKind::MethodDeclaration => {
                    for &parameter in &*node.ref_(self).as_function_like_declaration().parameters().ref_(self) {
                        self.mark_decorator_medata_data_type_node_as_referenced(
                            self.get_parameter_type_node_for_decorator_check(parameter),
                        )?;
                    }

                    self.mark_decorator_medata_data_type_node_as_referenced(
                        get_effective_return_type_node(node, self),
                    )?;
                }

                SyntaxKind::PropertyDeclaration => {
                    self.mark_decorator_medata_data_type_node_as_referenced(
                        get_effective_type_annotation_node(node, self),
                    )?;
                }

                SyntaxKind::Parameter => {
                    self.mark_decorator_medata_data_type_node_as_referenced(
                        self.get_parameter_type_node_for_decorator_check(node),
                    )?;
                    let containing_signature = node.ref_(self).parent();
                    for &parameter in &*containing_signature.ref_(self).as_signature_declaration().parameters().ref_(self) {
                        self.mark_decorator_medata_data_type_node_as_referenced(
                            self.get_parameter_type_node_for_decorator_check(parameter),
                        )?;
                    }
                }
                _ => (),
            }
        }

        try_for_each(
            &*node_decorators.ref_(self),
            |&decorator: &Id<Node>, _| -> io::Result<Option<()>> {
                self.check_decorator(decorator)?;
                Ok(None)
            },
        )?;

        Ok(())
    }

    pub(super) fn check_function_declaration(
        &self,
        node: Id<Node>, /*FunctionDeclaration*/
    ) -> io::Result<()> {
        if self.produce_diagnostics {
            self.check_function_or_method_declaration(node)?;
            self.check_grammar_for_generator(node);
            self.check_collisions_for_declaration_name(
                node,
                node.ref_(self).as_function_declaration().maybe_name(),
            );
        }

        Ok(())
    }

    pub(super) fn check_jsdoc_type_alias_tag(
        &self,
        node: Id<Node>, /*JSDocTypedefTag | JSDocCallbackTag*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_jsdoc_type_like_tag = node_ref.as_jsdoc_type_like_tag();
        let node_as_named_declaration = node_ref.maybe_as_named_declaration();
        let node_name = node_as_named_declaration
            .as_ref()
            .and_then(|node_as_named_declaration| node_as_named_declaration.maybe_name());
        if node_as_jsdoc_type_like_tag
            .maybe_type_expression()
            .is_none()
        {
            self.error(
                node_name,
                &Diagnostics::JSDoc_typedef_tag_should_either_have_a_type_annotation_or_be_followed_by_property_or_member_tags,
                None,
            );
        }

        if let Some(node_name) = node_name {
            self.check_type_name_is_reserved(node_name, &Diagnostics::Type_alias_name_cannot_be_0);
        }
        self.check_source_element(node_as_jsdoc_type_like_tag.maybe_type_expression())?;
        self.check_type_parameters(Some(&get_effective_type_parameter_declarations(node, self)))?;

        Ok(())
    }

    pub(super) fn check_jsdoc_template_tag(
        &self,
        node: Id<Node>, /*JSDocTemplateTag*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_jsdoc_template_tag = node_ref.as_jsdoc_template_tag();
        self.check_source_element(node_as_jsdoc_template_tag.constraint)?;
        for &tp in &*node_as_jsdoc_template_tag.type_parameters.ref_(self) {
            self.check_source_element(Some(tp))?;
        }

        Ok(())
    }

    pub(super) fn check_jsdoc_type_tag(
        &self,
        node: Id<Node>, /*JSDocTypeTag*/
    ) -> io::Result<()> {
        self.check_source_element(node.ref_(self).as_jsdoc_type_like_tag().maybe_type_expression())?;

        Ok(())
    }

    pub(super) fn check_jsdoc_parameter_tag(
        &self,
        node: Id<Node>, /*JSDocParameterTag*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_jsdoc_property_like_tag = node_ref.as_jsdoc_property_like_tag();
        self.check_source_element(node_as_jsdoc_property_like_tag.type_expression)?;
        if get_parameter_symbol_from_jsdoc(node, self).is_none() {
            let decl = get_host_signature_from_jsdoc(node, self);
            if let Some(decl) = decl {
                let i = get_jsdoc_tags(decl, self)
                    .filter(|jsdoc_tag| is_jsdoc_parameter_tag(&jsdoc_tag.ref_(self)))
                    .position(|jsdoc_tag| jsdoc_tag == node);
                if matches!(
                    i,
                    Some(i) if {
                        let decl_parameters = decl.ref_(self).as_signature_declaration().parameters();
                        i < decl_parameters.ref_(self).len() && is_binding_pattern(decl_parameters.ref_(self)[i].ref_(self).as_parameter_declaration().maybe_name().refed(self).as_deref())
                    }
                ) {
                    return Ok(());
                }
                if !self.contains_arguments_reference(decl)? {
                    if is_qualified_name(&node_as_jsdoc_property_like_tag.name.ref_(self)) {
                        self.error(
                            Some(node_as_jsdoc_property_like_tag.name),
                            &Diagnostics::Qualified_name_0_is_not_allowed_without_a_leading_param_object_1,
                            Some(vec![
                                entity_name_to_string(node_as_jsdoc_property_like_tag.name, self).into_owned(),
                                entity_name_to_string(node_as_jsdoc_property_like_tag.name.ref_(self).as_qualified_name().left, self).into_owned(),
                            ])
                        );
                    } else {
                        self.error(
                            Some(node_as_jsdoc_property_like_tag.name),
                            &Diagnostics::JSDoc_param_tag_has_name_0_but_there_is_no_parameter_with_that_name,
                            Some(vec![
                                id_text(&node_as_jsdoc_property_like_tag.name.ref_(self)).to_owned()
                            ])
                        );
                    }
                } else if get_jsdoc_tags(decl, self).rfind(
                    |jsdoc_tag| is_jsdoc_parameter_tag(&jsdoc_tag.ref_(self))
                ) == Some(node)
                && matches!(
                    node_as_jsdoc_property_like_tag.type_expression.as_ref().map(|node_type_expression| {
                        node_type_expression.ref_(self).as_jsdoc_type_expression().type_
                    }),
                    Some(node_type_expression_type) if !self.is_array_type(
                        self.get_type_from_type_node_(
                            node_type_expression_type
                        )?
                    )
                ) {
                    self.error(
                        Some(node_as_jsdoc_property_like_tag.name),
                        &Diagnostics::JSDoc_param_tag_has_name_0_but_there_is_no_parameter_with_that_name_It_would_match_arguments_if_it_had_an_array_type,
                        Some(vec![
                            id_text(
                                &if node_as_jsdoc_property_like_tag.name.ref_(self).kind() == SyntaxKind::QualifiedName {
                                    node_as_jsdoc_property_like_tag.name.ref_(self).as_qualified_name().right
                                } else {
                                    node_as_jsdoc_property_like_tag.name
                                }.ref_(self)
                            ).to_owned()
                        ])
                    );
                }
            }
        }

        Ok(())
    }

    pub(super) fn check_jsdoc_property_tag(
        &self,
        node: Id<Node>, /*JSDocPropertyTag*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_jsdoc_property_like_tag = node_ref.as_jsdoc_property_like_tag();
        self.check_source_element(node_as_jsdoc_property_like_tag.type_expression)?;

        Ok(())
    }

    pub(super) fn check_jsdoc_function_type(
        &self,
        node: Id<Node>, /*JSDocFunctionType*/
    ) -> io::Result<()> {
        if self.produce_diagnostics
            && node.ref_(self).as_jsdoc_function_type().maybe_type().is_none()
            && !is_jsdoc_construct_signature(node, self)
        {
            self.report_implicit_any(node, self.any_type(), None)?;
        }
        self.check_signature_declaration(node)?;

        Ok(())
    }
}
