use std::{cell::RefCell, collections::HashMap, io, rc::Rc};

use id_arena::Id;

use super::{
    signature_has_rest_parameter, CheckTypeContainingMessageChain, DeclarationMeaning,
    IterationTypeKind,
};
use crate::{
    declaration_name_to_string, get_containing_class, get_enclosing_block_scope_container,
    has_static_modifier, is_class_expression, HasTypeInterface, NodeCheckFlags, TypeId, __String,
    chain_diagnostic_messages, get_containing_function, get_effective_return_type_node,
    get_effective_type_parameter_declarations, get_function_flags, get_name_of_declaration,
    get_property_name_for_property_name_node, get_text_of_node, has_syntactic_modifier, id_text,
    is_binding_pattern, is_identifier, is_omitted_expression, is_parameter_property_declaration,
    is_private_identifier, is_static, node_is_present, released, return_ok_default_if_none,
    try_for_each, DiagnosticMessageChain, Diagnostics, ExternalEmitHelpers, FunctionFlags,
    HasArena, InArena, ModifierFlags, NamedDeclarationInterface, Node, NodeInterface,
    OptionInArena, ScriptTarget, SignatureDeclarationInterface, SymbolInterface, SyntaxKind, Type,
    TypeChecker, TypePredicateKind,
};

impl TypeChecker {
    pub(super) fn check_type_parameter(
        &self,
        node: Id<Node>, /*TypeParameterDeclaration*/
    ) -> io::Result<()> {
        if let Some(node_expression) = node.ref_(self).as_type_parameter_declaration().expression {
            self.grammar_error_on_first_token(node_expression, &Diagnostics::Type_expected, None);
        }

        self.check_source_element(released!(
            node.ref_(self).as_type_parameter_declaration().constraint
        ))?;
        self.check_source_element(node.ref_(self).as_type_parameter_declaration().default)?;
        let type_parameter =
            self.get_declared_type_of_type_parameter(self.get_symbol_of_node(node)?.unwrap());
        self.get_base_constraint_of_type(type_parameter)?;
        if !self.has_non_circular_type_parameter_default(type_parameter)? {
            self.error(
                released!(node.ref_(self).as_type_parameter_declaration().default),
                &Diagnostics::Type_parameter_0_has_a_circular_default,
                Some(vec![self.type_to_string_(
                    type_parameter,
                    Option::<Id<Node>>::None,
                    None,
                    None,
                )?]),
            );
        }
        let constraint_type = self.get_constraint_of_type_parameter(type_parameter)?;
        let default_type = self.get_default_from_type_parameter_(type_parameter)?;
        if let (Some(constraint_type), Some(default_type)) = (constraint_type, default_type) {
            self.check_type_assignable_to(
                default_type,
                self.get_type_with_this_argument(
                    self.instantiate_type(
                        constraint_type,
                        Some(self.make_unary_type_mapper(type_parameter, default_type)),
                    )?,
                    Some(default_type),
                    None,
                )?,
                released!(node.ref_(self).as_type_parameter_declaration().default),
                Some(&Diagnostics::Type_0_does_not_satisfy_the_constraint_1),
                None,
                None,
            )?;
        }
        if self.produce_diagnostics {
            self.check_type_name_is_reserved(
                node.ref_(self).as_type_parameter_declaration().name(),
                &Diagnostics::Type_parameter_name_cannot_be_0,
            );
        }

        Ok(())
    }

    pub(super) fn check_parameter(
        &self,
        node: Id<Node>, /*ParameterDeclaration*/
    ) -> io::Result<()> {
        self.check_grammar_decorators_and_modifiers(node);

        self.check_variable_like_declaration(node)?;
        let func = get_containing_function(node, self).unwrap();
        let node_ref = node.ref_(self);
        let node_as_parameter_declaration = node_ref.as_parameter_declaration();
        if has_syntactic_modifier(node, ModifierFlags::ParameterPropertyModifier, self) {
            if !(func.ref_(self).kind() == SyntaxKind::Constructor
                && node_is_present(
                    func.ref_(self)
                        .as_function_like_declaration()
                        .maybe_body()
                        .refed(self)
                        .as_deref(),
                ))
            {
                self.error(
                    Some(node),
                    &Diagnostics::A_parameter_property_is_only_allowed_in_a_constructor_implementation,
                    None,
                );
            }
            if func.ref_(self).kind() == SyntaxKind::Constructor
                && is_identifier(&node_as_parameter_declaration.name().ref_(self))
                && node_as_parameter_declaration
                    .name()
                    .ref_(self)
                    .as_identifier()
                    .escaped_text
                    == "constructor"
            {
                self.error(
                    Some(node_as_parameter_declaration.name()),
                    &Diagnostics::constructor_cannot_be_used_as_a_parameter_property_name,
                    None,
                );
            }
        }
        if node_as_parameter_declaration.question_token.is_some()
            && is_binding_pattern(
                node_as_parameter_declaration
                    .maybe_name()
                    .refed(self)
                    .as_deref(),
            )
            && func
                .ref_(self)
                .maybe_as_function_like_declaration()
                .and_then(|func| func.maybe_body())
                .is_some()
        {
            self.error(
                Some(node),
                &Diagnostics::A_binding_pattern_parameter_cannot_be_optional_in_an_implementation_signature,
                None,
            );
        }
        if let Some(node_name) = node_as_parameter_declaration
            .maybe_name()
            .filter(|&node_name| {
                is_identifier(&node_name.ref_(self))
                    && matches!(
                        &*node_name.ref_(self).as_identifier().escaped_text,
                        "this" | "new"
                    )
            })
        {
            if func
                .ref_(self)
                .as_signature_declaration()
                .parameters()
                .ref_(self)
                .into_iter()
                .position(|&parameter| parameter == node)
                != Some(0)
            {
                self.error(
                    Some(node),
                    &Diagnostics::A_0_parameter_must_be_the_first_parameter,
                    Some(vec![node_name
                        .ref_(self)
                        .as_identifier()
                        .escaped_text
                        .clone()]),
                );
            }
            if matches!(
                func.ref_(self).kind(),
                SyntaxKind::Constructor
                    | SyntaxKind::ConstructSignature
                    | SyntaxKind::ConstructorType
            ) {
                self.error(
                    Some(node),
                    &Diagnostics::A_constructor_cannot_have_a_this_parameter,
                    None,
                );
            }
            if func.ref_(self).kind() == SyntaxKind::ArrowFunction {
                self.error(
                    Some(node),
                    &Diagnostics::An_arrow_function_cannot_have_a_this_parameter,
                    None,
                );
            }
            if matches!(
                func.ref_(self).kind(),
                SyntaxKind::GetAccessor | SyntaxKind::SetAccessor
            ) {
                self.error(
                    Some(node),
                    &Diagnostics::get_and_set_accessors_cannot_declare_this_parameters,
                    None,
                );
            }
        }

        if node_as_parameter_declaration.dot_dot_dot_token.is_some()
            && !is_binding_pattern(
                node_as_parameter_declaration
                    .maybe_name()
                    .refed(self)
                    .as_deref(),
            )
            && !self.is_type_assignable_to(
                self.get_reduced_type(self.get_type_of_symbol(node.ref_(self).symbol())?)?,
                self.any_readonly_array_type(),
            )?
        {
            self.error(
                Some(node),
                &Diagnostics::A_rest_parameter_must_be_of_an_array_type,
                None,
            );
        }

        Ok(())
    }

    pub(super) fn check_type_predicate(
        &self,
        node: Id<Node>, /*TypePredicateNode*/
    ) -> io::Result<()> {
        let Some(parent) = self.get_type_predicate_parent(node) else {
            self.error(
                Some(node),
                &Diagnostics::A_type_predicate_is_only_allowed_in_return_type_position_for_functions_and_methods,
                None,
            );
            return Ok(());
        };

        let signature = self.get_signature_from_declaration_(parent)?;
        let type_predicate =
            return_ok_default_if_none!(self.get_type_predicate_of_signature(signature)?);

        self.check_source_element(released!(node.ref_(self).as_type_predicate_node().type_))?;

        let parameter_name = node.ref_(self).as_type_predicate_node().parameter_name;
        if matches!(
            type_predicate.ref_(self).kind,
            TypePredicateKind::This | TypePredicateKind::AssertsThis
        ) {
            self.get_type_from_this_type_node(parameter_name)?;
        } else {
            #[allow(clippy::suspicious_else_formatting)]
            if let Some(type_predicate_parameter_index) = type_predicate.ref_(self).parameter_index
            {
                if signature_has_rest_parameter(&signature.ref_(self))
                    && type_predicate_parameter_index == signature.ref_(self).parameters().len() - 1
                {
                    self.error(
                        Some(parameter_name),
                        &Diagnostics::A_type_predicate_cannot_reference_a_rest_parameter,
                        None,
                    );
                } else {
                    if let Some(type_predicate_type) = type_predicate.ref_(self).type_ {
                        let leading_error: Id<Box<dyn CheckTypeContainingMessageChain>> = self
                            .alloc_check_type_containing_message_chain(Box::new(
                                CheckTypePredicateContainingMessageChain,
                            ));
                        self.check_type_assignable_to(
                            type_predicate_type,
                            self.get_type_of_symbol(released!(
                                signature.ref_(self).parameters()[type_predicate_parameter_index]
                            ))?,
                            released!(node.ref_(self).as_type_predicate_node().type_),
                            None,
                            Some(leading_error),
                            None,
                        )?;
                    }
                }
            } else
            /*if (parameterName)*/
            {
                let mut has_reported_error = false;
                for parameter in &*parent
                    .ref_(self)
                    .as_signature_declaration()
                    .parameters()
                    .ref_(self)
                {
                    let name = parameter.ref_(self).as_named_declaration().name();
                    if is_binding_pattern(Some(&name.ref_(self)))
                        && self.check_if_type_predicate_variable_is_declared_in_binding_pattern(
                            name,
                            parameter_name,
                            type_predicate.ref_(self).parameter_name.as_ref().unwrap(),
                        )
                    {
                        has_reported_error = true;
                        break;
                    }
                }
                if !has_reported_error {
                    self.error(
                        Some(node.ref_(self).as_type_predicate_node().parameter_name),
                        &Diagnostics::Cannot_find_parameter_0,
                        Some(vec![type_predicate
                            .ref_(self)
                            .parameter_name
                            .clone()
                            .unwrap()]),
                    );
                }
            }
        }

        Ok(())
    }

    pub(super) fn get_type_predicate_parent(
        &self,
        node: Id<Node>,
    ) -> Option<Id<Node /*SignatureDeclaration*/>> {
        match node.ref_(self).parent().ref_(self).kind() {
            SyntaxKind::ArrowFunction
            | SyntaxKind::CallSignature
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::FunctionExpression
            | SyntaxKind::FunctionType
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::MethodSignature => {
                let parent = node.ref_(self).parent();
                if parent.ref_(self).as_has_type().maybe_type() == Some(node) {
                    return Some(parent);
                }
            }
            _ => (),
        }
        None
    }

    pub(super) fn check_if_type_predicate_variable_is_declared_in_binding_pattern(
        &self,
        pattern: Id<Node>, /*BindingPattern*/
        predicate_variable_node: Id<Node>,
        predicate_variable_name: &str,
    ) -> bool {
        for element in &*pattern.ref_(self).as_has_elements().elements().ref_(self) {
            if is_omitted_expression(&element.ref_(self)) {
                continue;
            }

            let name = element.ref_(self).as_named_declaration().name();
            if name.ref_(self).kind() == SyntaxKind::Identifier
                && name.ref_(self).as_identifier().escaped_text == predicate_variable_name
            {
                self.error(
                    Some(predicate_variable_node),
                    &Diagnostics::A_type_predicate_cannot_reference_element_0_in_a_binding_pattern,
                    Some(vec![predicate_variable_name.to_owned()]),
                );
                return true;
            } else if matches!(
                name.ref_(self).kind(),
                SyntaxKind::ArrayBindingPattern | SyntaxKind::ObjectBindingPattern
            ) {
                if self.check_if_type_predicate_variable_is_declared_in_binding_pattern(
                    name,
                    predicate_variable_node,
                    predicate_variable_name,
                ) {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn check_signature_declaration(
        &self,
        node: Id<Node>, /*SignatureDeclaration*/
    ) -> io::Result<()> {
        if node.ref_(self).kind() == SyntaxKind::IndexSignature {
            self.check_grammar_index_signature(node)?;
        } else if matches!(
            node.ref_(self).kind(),
            SyntaxKind::FunctionType
                | SyntaxKind::FunctionDeclaration
                | SyntaxKind::ConstructorType
                | SyntaxKind::CallSignature
                | SyntaxKind::Constructor
                | SyntaxKind::ConstructSignature
        ) {
            self.check_grammar_function_like_declaration(node)?;
        }

        let function_flags = get_function_flags(Some(node), self);
        if !function_flags.intersects(FunctionFlags::Invalid) {
            if function_flags & FunctionFlags::AsyncGenerator == FunctionFlags::AsyncGenerator
                && self.language_version < ScriptTarget::ESNext
            {
                self.check_external_emit_helpers(
                    node,
                    ExternalEmitHelpers::AsyncGeneratorIncludes,
                )?;
            }

            if function_flags & FunctionFlags::AsyncGenerator == FunctionFlags::Async
                && self.language_version < ScriptTarget::ES2017
            {
                self.check_external_emit_helpers(node, ExternalEmitHelpers::Awaiter)?;
            }

            if function_flags & FunctionFlags::AsyncGenerator != FunctionFlags::Normal
                && self.language_version < ScriptTarget::ES2015
            {
                self.check_external_emit_helpers(node, ExternalEmitHelpers::Generator)?;
            }
        }

        self.check_type_parameters(Some(&get_effective_type_parameter_declarations(node, self)))?;

        try_for_each(
            &*released!(node
                .ref_(self)
                .as_signature_declaration()
                .parameters()
                .ref_(self)
                .clone()),
            |&parameter: &Id<Node>, _| -> io::Result<Option<()>> {
                self.check_parameter(parameter)?;
                Ok(None)
            },
        )?;

        if let Some(node_type) = released!(node.ref_(self).as_signature_declaration().maybe_type())
        {
            self.check_source_element(Some(node_type))?;
        }

        if self.produce_diagnostics {
            self.check_collision_with_arguments_in_generated_code(node);
            let return_type_node = get_effective_return_type_node(node, self);
            if self.no_implicit_any && return_type_node.is_none() {
                match node.ref_(self).kind() {
                    SyntaxKind::ConstructSignature => {
                        self.error(
                            Some(node),
                            &Diagnostics::Construct_signature_which_lacks_return_type_annotation_implicitly_has_an_any_return_type,
                            None,
                        );
                    }
                    SyntaxKind::CallSignature => {
                        self.error(
                            Some(node),
                            &Diagnostics::Call_signature_which_lacks_return_type_annotation_implicitly_has_an_any_return_type,
                            None,
                        );
                    }
                    _ => (),
                }
            }

            if let Some(return_type_node) = return_type_node {
                let function_flags = get_function_flags(Some(node), self);
                if function_flags & (FunctionFlags::Invalid | FunctionFlags::Generator)
                    == FunctionFlags::Generator
                {
                    let return_type = self.get_type_from_type_node_(return_type_node)?;
                    if return_type == self.void_type() {
                        self.error(
                            Some(return_type_node),
                            &Diagnostics::A_generator_cannot_have_a_void_type_annotation,
                            None,
                        );
                    } else {
                        let generator_yield_type = self
                            .get_iteration_type_of_generator_function_return_type(
                                IterationTypeKind::Yield,
                                return_type,
                                function_flags.intersects(FunctionFlags::Async),
                            )?
                            .unwrap_or_else(|| self.any_type());
                        let generator_return_type = self
                            .get_iteration_type_of_generator_function_return_type(
                                IterationTypeKind::Return,
                                return_type,
                                function_flags.intersects(FunctionFlags::Async),
                            )?
                            .unwrap_or_else(|| generator_yield_type.clone());
                        let generator_next_type = self
                            .get_iteration_type_of_generator_function_return_type(
                                IterationTypeKind::Next,
                                return_type,
                                function_flags.intersects(FunctionFlags::Async),
                            )?
                            .unwrap_or_else(|| self.unknown_type());
                        let generator_instantiation = self.create_generator_return_type(
                            generator_yield_type,
                            generator_return_type,
                            generator_next_type,
                            function_flags.intersects(FunctionFlags::Async),
                        )?;
                        self.check_type_assignable_to(
                            generator_instantiation,
                            return_type,
                            Some(return_type_node),
                            None,
                            None,
                            None,
                        )?;
                    }
                } else if function_flags & FunctionFlags::AsyncGenerator == FunctionFlags::Async {
                    self.check_async_function_return_type(node, return_type_node)?;
                }
            }
            if !matches!(
                node.ref_(self).kind(),
                SyntaxKind::IndexSignature | SyntaxKind::JSDocFunctionType
            ) {
                self.register_for_unused_identifiers_check(node);
            }
        }

        Ok(())
    }

    pub(super) fn check_class_for_duplicate_declarations(
        &self,
        node: Id<Node>, /*ClassLikeDeclaration*/
    ) {
        let mut instance_names: HashMap<__String, DeclarationMeaning> = HashMap::new();
        let mut static_names: HashMap<__String, DeclarationMeaning> = HashMap::new();
        let mut private_identifiers: HashMap<__String, DeclarationMeaning> = HashMap::new();
        for &member in &*node
            .ref_(self)
            .as_class_like_declaration()
            .members()
            .ref_(self)
        {
            if member.ref_(self).kind() == SyntaxKind::Constructor {
                for &param in &*member
                    .ref_(self)
                    .as_constructor_declaration()
                    .parameters()
                    .ref_(self)
                {
                    if is_parameter_property_declaration(param, member, self)
                        && !is_binding_pattern(
                            param
                                .ref_(self)
                                .as_named_declaration()
                                .maybe_name()
                                .refed(self)
                                .as_deref(),
                        )
                    {
                        self.add_name(
                            &mut instance_names,
                            param.ref_(self).as_named_declaration().name(),
                            &param
                                .ref_(self)
                                .as_named_declaration()
                                .name()
                                .ref_(self)
                                .as_identifier()
                                .escaped_text,
                            DeclarationMeaning::GetOrSetAccessor,
                        );
                    }
                }
            } else {
                let is_static_member = is_static(member, self);
                let Some(name) = member.ref_(self).as_named_declaration().maybe_name() else {
                    continue;
                };
                let is_private = is_private_identifier(&name.ref_(self));
                let private_static_flags = if is_private && is_static_member {
                    DeclarationMeaning::PrivateStatic
                } else {
                    DeclarationMeaning::None
                };
                let names = if is_private {
                    &mut private_identifiers
                } else if is_static_member {
                    &mut static_names
                } else {
                    &mut instance_names
                };

                let member_name = /*name &&*/ get_property_name_for_property_name_node(name, self);
                if let Some(member_name) = member_name.as_ref() {
                    match member.ref_(self).kind() {
                        SyntaxKind::GetAccessor => {
                            self.add_name(
                                names,
                                name,
                                member_name,
                                DeclarationMeaning::GetAccessor | private_static_flags,
                            );
                        }

                        SyntaxKind::SetAccessor => {
                            self.add_name(
                                names,
                                name,
                                member_name,
                                DeclarationMeaning::SetAccessor | private_static_flags,
                            );
                        }

                        SyntaxKind::PropertyDeclaration => {
                            self.add_name(
                                names,
                                name,
                                member_name,
                                DeclarationMeaning::GetOrSetAccessor | private_static_flags,
                            );
                        }

                        SyntaxKind::MethodDeclaration => {
                            self.add_name(
                                names,
                                name,
                                member_name,
                                DeclarationMeaning::Method | private_static_flags,
                            );
                        }
                        _ => (),
                    }
                }
            }
        }
    }

    pub(super) fn add_name(
        &self,
        names: &mut HashMap<__String, DeclarationMeaning>,
        location: Id<Node>,
        name: &str, /*__String*/
        meaning: DeclarationMeaning,
    ) {
        let prev = names.get(name);
        if let Some(prev) = prev {
            if *prev & DeclarationMeaning::PrivateStatic
                != meaning & DeclarationMeaning::PrivateStatic
            {
                self.error(
                    Some(location),
                    &Diagnostics::Duplicate_identifier_0_Static_and_instance_elements_cannot_share_the_same_private_name,
                    Some(vec![
                        get_text_of_node(location, None, self).into_owned()
                    ])
                );
            } else {
                let prev_is_method = prev.intersects(DeclarationMeaning::Method);
                let is_method = meaning.intersects(DeclarationMeaning::Method);
                if prev_is_method || is_method {
                    if prev_is_method != is_method {
                        self.error(
                            Some(location),
                            &Diagnostics::Duplicate_identifier_0,
                            Some(vec![get_text_of_node(location, None, self).into_owned()]),
                        );
                    }
                } else if (*prev & meaning).intersects(!DeclarationMeaning::PrivateStatic) {
                    self.error(
                        Some(location),
                        &Diagnostics::Duplicate_identifier_0,
                        Some(vec![get_text_of_node(location, None, self).into_owned()]),
                    );
                } else {
                    names.insert(name.to_owned(), *prev | meaning);
                }
            }
        } else {
            names.insert(name.to_owned(), meaning);
        }
    }

    pub(super) fn check_class_for_static_property_name_conflicts(
        &self,
        node: Id<Node>, /*ClassLikeDeclaration*/
    ) -> io::Result<()> {
        for &member in &*node
            .ref_(self)
            .as_class_like_declaration()
            .members()
            .ref_(self)
        {
            let member_name_node = member.ref_(self).as_named_declaration().maybe_name();
            let is_static_member = is_static(member, self);
            if is_static_member {
                if let Some(member_name_node) = member_name_node {
                    let member_name =
                        get_property_name_for_property_name_node(member_name_node, self);
                    if let Some(member_name) = member_name {
                        if matches!(
                            &*member_name,
                            "name" | "length" | "caller" | "arguments" | "prototype"
                        ) {
                            let message = &Diagnostics::Static_property_0_conflicts_with_built_in_property_Function_0_of_constructor_function_1;
                            let class_name = self
                                .get_name_of_symbol_as_written(
                                    self.get_symbol_of_node(node)?.unwrap(),
                                    None,
                                )
                                .into_owned();
                            self.error(
                                Some(member_name_node),
                                message,
                                Some(vec![member_name, class_name]),
                            );
                        }
                    }
                }
            }
        }

        Ok(())
    }

    pub(super) fn check_object_type_for_duplicate_declarations(
        &self,
        node: Id<Node>, /*TypeLiteralNode | InterfaceDeclaration*/
    ) {
        let mut names: HashMap<String, bool> = HashMap::new();
        for member in &*node.ref_(self).as_has_members().members().ref_(self) {
            if member.ref_(self).kind() == SyntaxKind::PropertySignature {
                let member_name: String;
                let name = member.ref_(self).as_named_declaration().name();
                match name.ref_(self).kind() {
                    SyntaxKind::StringLiteral | SyntaxKind::NumericLiteral => {
                        member_name = name.ref_(self).as_literal_like_node().text().clone();
                    }
                    SyntaxKind::Identifier => {
                        member_name = id_text(&name.ref_(self)).to_owned();
                    }
                    _ => {
                        continue;
                    }
                }

                if names.get(&member_name).cloned() == Some(true) {
                    self.error(
                        get_name_of_declaration(
                            member
                                .ref_(self)
                                .symbol()
                                .ref_(self)
                                .maybe_value_declaration(),
                            self,
                        ),
                        &Diagnostics::Duplicate_identifier_0,
                        Some(vec![member_name.clone()]),
                    );
                    self.error(
                        member.ref_(self).as_named_declaration().maybe_name(),
                        &Diagnostics::Duplicate_identifier_0,
                        Some(vec![member_name]),
                    );
                } else {
                    names.insert(member_name, true);
                }
            }
        }
    }

    pub(super) fn check_type_for_duplicate_index_signatures(
        &self,
        node: Id<Node>,
    ) -> io::Result<()> {
        if node.ref_(self).kind() == SyntaxKind::InterfaceDeclaration {
            let node_symbol = self.get_symbol_of_node(node)?.unwrap();
            if matches!(
                node_symbol.ref_(self).maybe_declarations().as_ref(),
                Some(node_symbol_declarations) if !node_symbol_declarations.is_empty() &&
                    node_symbol_declarations[0] != node
            ) {
                return Ok(());
            }
        }

        let index_symbol = self.get_index_symbol(self.get_symbol_of_node(node)?.unwrap());
        if let Some(index_symbol_declarations) = index_symbol
            .and_then(|index_symbol| index_symbol.ref_(self).maybe_declarations().clone())
            .as_ref()
        {
            let mut index_signature_map: HashMap<TypeId, IndexSignatureMapValue> = HashMap::new();
            for declaration in index_symbol_declarations {
                if declaration
                    .ref_(self)
                    .as_index_signature_declaration()
                    .parameters()
                    .ref_(self)
                    .len()
                    == 1
                {
                    if let Some(declaration_parameters_0_type) = declaration
                        .ref_(self)
                        .as_index_signature_declaration()
                        .parameters()
                        .ref_(self)[0]
                        .ref_(self)
                        .as_parameter_declaration()
                        .maybe_type()
                    {
                        self.for_each_type(
                            self.get_type_from_type_node_(declaration_parameters_0_type)?,
                            |type_: Id<Type>| -> Option<()> {
                                let entry = index_signature_map
                                    .entry(self.get_type_id(type_))
                                    .or_insert_with(|| IndexSignatureMapValue {
                                        type_,
                                        declarations: vec![],
                                    });
                                entry.declarations.push(declaration.clone());
                                None
                            },
                        );
                    }
                }
            }
            try_for_each(
                index_signature_map.values(),
                |entry, _| -> io::Result<Option<()>> {
                    if entry.declarations.len() > 1 {
                        for &declaration in &entry.declarations {
                            self.error(
                                Some(declaration),
                                &Diagnostics::Duplicate_index_signature_for_type_0,
                                Some(vec![self.type_to_string_(
                                    entry.type_,
                                    Option::<Id<Node>>::None,
                                    None,
                                    None,
                                )?]),
                            );
                        }
                    }

                    Ok(None)
                },
            )?;
        }

        Ok(())
    }

    pub(super) fn check_property_declaration(
        &self,
        node: Id<Node>, /*PropertySignature*/
    ) -> io::Result<()> {
        if !self.check_grammar_decorators_and_modifiers(node)
            && !self.check_grammar_property(node)?
        {
            self.check_grammar_computed_property_name(
                node.ref_(self).as_named_declaration().name(),
            );
        }
        self.check_variable_like_declaration(node)?;

        self.set_node_links_for_private_identifier_scope(node);
        if is_private_identifier(&node.ref_(self).as_named_declaration().name().ref_(self))
            && has_static_modifier(node, self)
        {
            if let Some(node_initializer) = node.ref_(self).as_has_initializer().maybe_initializer()
            {
                if self.language_version == ScriptTarget::ESNext
                    && self.compiler_options.ref_(self).use_define_for_class_fields != Some(true)
                {
                    self.error(
                        Some(node_initializer),
                        &Diagnostics::Static_fields_with_private_names_can_t_have_initializers_when_the_useDefineForClassFields_flag_is_not_specified_with_a_target_of_esnext_Consider_adding_the_useDefineForClassFields_flag,
                        None,
                    );
                }
            }
        }
        if has_syntactic_modifier(node, ModifierFlags::Abstract, self)
            && node.ref_(self).kind() == SyntaxKind::PropertyDeclaration
            && node
                .ref_(self)
                .as_has_initializer()
                .maybe_initializer()
                .is_some()
        {
            self.error(
                Some(node),
                &Diagnostics::Property_0_cannot_have_an_initializer_because_it_is_marked_abstract,
                Some(vec![declaration_name_to_string(
                    node.ref_(self).as_named_declaration().maybe_name(),
                    self,
                )
                .into_owned()]),
            );
        }

        Ok(())
    }

    pub(super) fn check_property_signature(
        &self,
        node: Id<Node>, /*PropertySignature*/
    ) -> io::Result<()> {
        if is_private_identifier(&node.ref_(self).as_property_signature().name().ref_(self)) {
            self.error(
                Some(node),
                &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                None,
            );
        }
        self.check_property_declaration(node)?;

        Ok(())
    }

    pub(super) fn check_method_declaration(
        &self,
        node: Id<Node>, /*MethodDeclaration | MethodSignature*/
    ) -> io::Result<()> {
        if !self.check_grammar_method(node)? {
            self.check_grammar_computed_property_name(
                node.ref_(self).as_named_declaration().name(),
            );
        }

        self.check_function_or_method_declaration(node)?;

        if has_syntactic_modifier(node, ModifierFlags::Abstract, self)
            && node.ref_(self).kind() == SyntaxKind::MethodDeclaration
            && node
                .ref_(self)
                .as_function_like_declaration()
                .maybe_body()
                .is_some()
        {
            self.error(
                Some(node),
                &Diagnostics::Method_0_cannot_have_an_implementation_because_it_is_marked_abstract,
                Some(vec![declaration_name_to_string(
                    node.ref_(self).as_named_declaration().maybe_name(),
                    self,
                )
                .into_owned()]),
            );
        }

        if is_private_identifier(&node.ref_(self).as_named_declaration().name().ref_(self))
            && get_containing_class(node, self).is_none()
        {
            self.error(
                Some(node),
                &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                None,
            );
        }

        self.set_node_links_for_private_identifier_scope(node);

        Ok(())
    }

    pub(super) fn set_node_links_for_private_identifier_scope(
        &self,
        node: Id<Node>, /*PropertyDeclaration | PropertySignature | MethodDeclaration | MethodSignature | AccessorDeclaration*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_named_declaration = node_ref.as_named_declaration();
        if is_private_identifier(&node_as_named_declaration.name().ref_(self))
            && self.language_version < ScriptTarget::ESNext
        {
            let mut lexical_scope = get_enclosing_block_scope_container(node, self);
            while let Some(lexical_scope_present) = lexical_scope {
                self.get_node_links(lexical_scope_present)
                    .ref_mut(self)
                    .flags |= NodeCheckFlags::ContainsClassWithPrivateIdentifiers;
                lexical_scope = get_enclosing_block_scope_container(lexical_scope_present, self);
            }

            if is_class_expression(&node.ref_(self).parent().ref_(self)) {
                let enclosing_iteration_statement =
                    self.get_enclosing_iteration_statement(node.ref_(self).parent());
                if let Some(enclosing_iteration_statement) = enclosing_iteration_statement {
                    self.get_node_links(node_as_named_declaration.name())
                        .ref_mut(self)
                        .flags |= NodeCheckFlags::BlockScopedBindingInLoop;
                    self.get_node_links(enclosing_iteration_statement)
                        .ref_mut(self)
                        .flags |= NodeCheckFlags::LoopWithCapturedBlockScopedBinding;
                }
            }
        }
    }
}

struct CheckTypePredicateContainingMessageChain;

impl CheckTypeContainingMessageChain for CheckTypePredicateContainingMessageChain {
    fn get(&self) -> io::Result<Option<Rc<RefCell<DiagnosticMessageChain>>>> {
        Ok(Some(Rc::new(RefCell::new(chain_diagnostic_messages(
            None,
            &Diagnostics::A_type_predicate_s_type_must_be_assignable_to_its_parameter_s_type,
            None,
        )))))
    }
}

struct IndexSignatureMapValue {
    pub type_: Id<Type>,
    pub declarations: Vec<Id<Node /*IndexSignatureDeclaration*/>>,
}
