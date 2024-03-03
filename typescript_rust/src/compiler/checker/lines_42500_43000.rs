use std::{collections::HashMap, convert::TryInto, io};

use id_arena::Id;

use super::DeclarationMeaning;
use crate::{
    add_related_info, create_diagnostic_for_node, file_extension_is_one_of, filter,
    find_use_strict_prologue, get_line_and_character_of_position,
    get_property_name_for_property_name_node, get_source_file_of_node, get_text_of_node,
    has_effective_modifiers, is_array_literal_expression, is_arrow_function, is_binding_pattern,
    is_block, is_function_like_declaration, is_object_literal_expression, is_rest_parameter,
    length, skip_parentheses, skip_trivia, some, token_to_string, Debug_, DiagnosticMessage,
    DiagnosticRelatedInformation, Diagnostics, Extension, HasInitializerInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface,
    InterfaceOrClassLikeDeclarationInterface, LineAndCharacter, NamedDeclarationInterface,
    NodeArray, NodeFlags, ReadonlyTextRange, ScriptTarget, SourceFileLike, SyntaxKind, TypeFlags,
    TypeInterface, __String, for_each, released, HasArena, InArena, Node, NodeInterface,
    OptionInArena, Type, TypeChecker,
};

impl TypeChecker {
    pub(super) fn report_obvious_modifier_errors(&self, node: Id<Node>) -> Option<bool> {
        if node.ref_(self).maybe_modifiers().is_none() {
            Some(false)
        } else if self.should_report_bad_modifier(node) {
            Some(self.grammar_error_on_first_token(
                node,
                &Diagnostics::Modifiers_cannot_appear_here,
                None,
            ))
        } else {
            None
        }
    }

    pub(super) fn should_report_bad_modifier(&self, node: Id<Node>) -> bool {
        match node.ref_(self).kind() {
            SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::Constructor
            | SyntaxKind::PropertyDeclaration
            | SyntaxKind::PropertySignature
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::MethodSignature
            | SyntaxKind::IndexSignature
            | SyntaxKind::ModuleDeclaration
            | SyntaxKind::ImportDeclaration
            | SyntaxKind::ImportEqualsDeclaration
            | SyntaxKind::ExportDeclaration
            | SyntaxKind::ExportAssignment
            | SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction
            | SyntaxKind::Parameter => false,
            _ => {
                if matches!(
                    node.ref_(self).parent().ref_(self).kind(),
                    SyntaxKind::ModuleBlock | SyntaxKind::SourceFile
                ) {
                    return false;
                }
                match node.ref_(self).kind() {
                    SyntaxKind::FunctionDeclaration => {
                        self.node_has_any_modifiers_except(node, SyntaxKind::AsyncKeyword)
                    }
                    SyntaxKind::ClassDeclaration | SyntaxKind::ConstructorType => {
                        self.node_has_any_modifiers_except(node, SyntaxKind::AbstractKeyword)
                    }
                    SyntaxKind::InterfaceDeclaration
                    | SyntaxKind::VariableStatement
                    | SyntaxKind::TypeAliasDeclaration
                    | SyntaxKind::ClassStaticBlockDeclaration => true,
                    SyntaxKind::EnumDeclaration => {
                        self.node_has_any_modifiers_except(node, SyntaxKind::ConstKeyword)
                    }
                    _ => Debug_.fail(None),
                }
            }
        }
    }

    pub(super) fn node_has_any_modifiers_except(
        &self,
        node: Id<Node>,
        allowed_modifier: SyntaxKind,
    ) -> bool {
        let node_modifiers = node.ref_(self).maybe_modifiers();
        let node_modifiers = node_modifiers.as_ref().unwrap();
        node_modifiers.ref_(self).len() > 1
            || node_modifiers.ref_(self)[0].ref_(self).kind() != allowed_modifier
    }

    pub(super) fn check_grammar_async_modifier(
        &self,
        node: Id<Node>,
        async_modifier: Id<Node>,
    ) -> bool {
        if matches!(
            node.ref_(self).kind(),
            SyntaxKind::MethodDeclaration
                | SyntaxKind::FunctionDeclaration
                | SyntaxKind::FunctionExpression
                | SyntaxKind::ArrowFunction
        ) {
            return false;
        }

        self.grammar_error_on_node(
            async_modifier,
            &Diagnostics::_0_modifier_cannot_be_used_here,
            Some(vec!["async".to_owned()]),
        )
    }

    pub(super) fn check_grammar_for_disallowed_trailing_comma(
        &self,
        list: Option<Id<NodeArray>>,
        diag: Option<&'static DiagnosticMessage>,
    ) -> bool {
        let diag = diag.unwrap_or(&Diagnostics::Trailing_comma_not_allowed);
        if let Some(list) = list.filter(|list| list.ref_(self).has_trailing_comma) {
            return self.grammar_error_at_pos(
                list.ref_(self)[0],
                list.ref_(self).end() - TryInto::<isize>::try_into(",".len()).unwrap(),
                TryInto::<isize>::try_into(",".len()).unwrap(),
                diag,
                None,
            );
        }
        false
    }

    pub(super) fn check_grammar_type_parameter_list(
        &self,
        type_parameters: Option<Id<NodeArray> /*<TypeParameterDeclaration>*/>,
        file: Id<Node>, /*SourceFile*/
    ) -> bool {
        if let Some(type_parameters) =
            type_parameters.filter(|type_parameters| type_parameters.ref_(self).is_empty())
        {
            let start =
                type_parameters.ref_(self).pos() - TryInto::<isize>::try_into("<".len()).unwrap();
            let end = skip_trivia(
                &file.ref_(self).as_source_file().text_as_chars(),
                type_parameters.ref_(self).end(),
                None,
                None,
                None,
            ) + TryInto::<isize>::try_into(">".len()).unwrap();
            return self.grammar_error_at_pos(
                file,
                start,
                end - start,
                &Diagnostics::Type_parameter_list_cannot_be_empty,
                None,
            );
        }
        false
    }

    pub(super) fn check_grammar_parameter_list(
        &self,
        parameters: Id<NodeArray>, /*<ParameterDeclaration>*/
    ) -> io::Result<bool> {
        let mut seen_optional_parameter = false;
        let parameter_count = parameters.ref_(self).len();

        for i in 0..parameter_count {
            let parameter = parameters.ref_(self)[i];
            if let Some(parameter_dot_dot_dot_token) = released!(
                parameter
                    .ref_(self)
                    .as_parameter_declaration()
                    .dot_dot_dot_token
            ) {
                if i != parameter_count - 1 {
                    return Ok(self.grammar_error_on_node(
                        parameter_dot_dot_dot_token,
                        &Diagnostics::A_rest_parameter_must_be_last_in_a_parameter_list,
                        None,
                    ));
                }
                if !parameter.ref_(self).flags().intersects(NodeFlags::Ambient) {
                    self.check_grammar_for_disallowed_trailing_comma(
                        Some(parameters),
                        Some(&Diagnostics::A_rest_parameter_or_binding_pattern_may_not_have_a_trailing_comma)
                    );
                }

                if let Some(parameter_question_token) = parameter
                    .ref_(self)
                    .as_parameter_declaration()
                    .question_token
                {
                    return Ok(self.grammar_error_on_node(
                        parameter_question_token,
                        &Diagnostics::A_rest_parameter_cannot_be_optional,
                        None,
                    ));
                }

                if parameter
                    .ref_(self)
                    .as_parameter_declaration()
                    .maybe_initializer()
                    .is_some()
                {
                    return Ok(self.grammar_error_on_node(
                        parameter.ref_(self).as_parameter_declaration().name(),
                        &Diagnostics::A_rest_parameter_cannot_have_an_initializer,
                        None,
                    ));
                }
            } else if self.is_optional_parameter_(parameter)? {
                seen_optional_parameter = true;
                if parameter
                    .ref_(self)
                    .as_parameter_declaration()
                    .question_token
                    .is_some()
                    && parameter
                        .ref_(self)
                        .as_parameter_declaration()
                        .maybe_initializer()
                        .is_some()
                {
                    return Ok(self.grammar_error_on_node(
                        parameter.ref_(self).as_parameter_declaration().name(),
                        &Diagnostics::Parameter_cannot_have_question_mark_and_initializer,
                        None,
                    ));
                }
            } else if seen_optional_parameter
                && parameter
                    .ref_(self)
                    .as_parameter_declaration()
                    .maybe_initializer()
                    .is_none()
            {
                return Ok(self.grammar_error_on_node(
                    parameter.ref_(self).as_parameter_declaration().name(),
                    &Diagnostics::A_required_parameter_cannot_follow_an_optional_parameter,
                    None,
                ));
            }
        }
        Ok(false)
    }

    pub(super) fn get_non_simple_parameters(
        &self,
        parameters: &[Id<Node /*ParameterDeclaration*/>],
    ) -> Vec<Id<Node /*ParameterDeclaration*/>> {
        filter(parameters, |&parameter: &Id<Node>| {
            let parameter_ref = parameter.ref_(self);
            let parameter_as_parameter_declaration = parameter_ref.as_parameter_declaration();
            parameter_as_parameter_declaration
                .maybe_initializer()
                .is_some()
                || is_binding_pattern(
                    parameter_as_parameter_declaration
                        .maybe_name()
                        .refed(self)
                        .as_deref(),
                )
                || is_rest_parameter(parameter, self)
        })
    }

    pub(super) fn check_grammar_for_use_strict_simple_parameter_list(
        &self,
        node: Id<Node>, /*FunctionLikeDeclaration*/
    ) -> bool {
        if self.language_version >= ScriptTarget::ES2016 {
            let node_ref = node.ref_(self);
            let node_as_function_like_declaration = node_ref.as_function_like_declaration();
            let use_strict_directive = node_as_function_like_declaration
                .maybe_body()
                .filter(|node_body| is_block(&node_body.ref_(self)))
                .and_then(|node_body| {
                    find_use_strict_prologue(
                        &node_body.ref_(self).as_block().statements.ref_(self),
                        self,
                    )
                });
            if let Some(use_strict_directive) = use_strict_directive {
                let non_simple_parameters = self.get_non_simple_parameters(
                    &node_as_function_like_declaration.parameters().ref_(self),
                );
                if length(Some(&*non_simple_parameters)) > 0 {
                    for_each(
                        &non_simple_parameters,
                        |&parameter: &Id<Node>, _| -> Option<()> {
                            add_related_info(
                                &self.error(
                                    Some(parameter),
                                    &Diagnostics::This_parameter_is_not_allowed_with_use_strict_directive,
                                    None,
                                ).ref_(self),
                                vec![
                                    self.alloc_diagnostic_related_information(
                                        create_diagnostic_for_node(
                                            use_strict_directive,
                                            &Diagnostics::use_strict_directive_used_here,
                                            None,
                                            self,
                                        ).into()
                                    )
                                ]
                            );
                            None
                        },
                    );

                    let diagnostics: Vec<Id<DiagnosticRelatedInformation>> = non_simple_parameters
                        .iter()
                        .enumerate()
                        .map(|(index, parameter)| {
                            self.alloc_diagnostic_related_information(if index == 0 {
                                create_diagnostic_for_node(
                                    *parameter,
                                    &Diagnostics::Non_simple_parameter_declared_here,
                                    None,
                                    self,
                                )
                                .into()
                            } else {
                                create_diagnostic_for_node(
                                    *parameter,
                                    &Diagnostics::and_here,
                                    None,
                                    self,
                                )
                                .into()
                            })
                        })
                        .collect();
                    add_related_info(
                        &self.error(
                            Some(use_strict_directive),
                            &Diagnostics::use_strict_directive_cannot_be_used_with_non_simple_parameter_list,
                            None,
                        ).ref_(self),
                        diagnostics
                    );
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn check_grammar_function_like_declaration(
        &self,
        node: Id<Node>, /*FunctionLikeDeclaration | MethodSignature*/
    ) -> io::Result<bool> {
        let file = get_source_file_of_node(node, self);
        Ok(self.check_grammar_decorators_and_modifiers(node)
            || self.check_grammar_type_parameter_list(
                node.ref_(self)
                    .as_signature_declaration()
                    .maybe_type_parameters(),
                file,
            )
            || self.check_grammar_parameter_list(released!(node
                .ref_(self)
                .as_signature_declaration()
                .parameters()))?
            || self.check_grammar_arrow_function(node, file)
            || is_function_like_declaration(&node.ref_(self))
                && self.check_grammar_for_use_strict_simple_parameter_list(node))
    }

    pub(super) fn check_grammar_class_like_declaration(
        &self,
        node: Id<Node>, /*ClassLikeDeclaration*/
    ) -> bool {
        let file = get_source_file_of_node(node, self);
        self.check_grammar_class_declaration_heritage_clauses(node)
            || self.check_grammar_type_parameter_list(
                node.ref_(self)
                    .as_has_type_parameters()
                    .maybe_type_parameters(),
                file,
            )
    }

    pub(super) fn check_grammar_arrow_function(
        &self,
        node: Id<Node>,
        file: Id<Node>, /*SourceFile*/
    ) -> bool {
        if !is_arrow_function(&node.ref_(self)) {
            return false;
        }
        let node_ref = node.ref_(self);
        let node_as_arrow_function = node_ref.as_arrow_function();

        if let Some(node_type_parameters) = node_as_arrow_function
            .maybe_type_parameters()
            .as_ref()
            .filter(|node_type_parameters| {
                !(node_type_parameters.ref_(self).len() > 1
                    || node_type_parameters.ref_(self).has_trailing_comma
                    || node_type_parameters.ref_(self)[0]
                        .ref_(self)
                        .as_type_parameter_declaration()
                        .constraint
                        .is_some())
            })
        {
            if
            /*file &&*/
            file_extension_is_one_of(
                &file.ref_(self).as_source_file().file_name(),
                &[Extension::Mts.to_str(), Extension::Cts.to_str()],
            ) {
                self.grammar_error_on_node(
                    node_type_parameters.ref_(self)[0],
                    &Diagnostics::This_syntax_is_reserved_in_files_with_the_mts_or_cts_extension_Add_a_trailing_comma_or_explicit_constraint,
                    None,
                );
            }
        }

        let equals_greater_than_token = node_as_arrow_function.equals_greater_than_token;
        let LineAndCharacter {
            line: start_line, ..
        } = get_line_and_character_of_position(
            file.ref_(self).as_source_file(),
            equals_greater_than_token
                .ref_(self)
                .pos()
                .try_into()
                .unwrap(),
        );
        let LineAndCharacter { line: end_line, .. } = get_line_and_character_of_position(
            file.ref_(self).as_source_file(),
            equals_greater_than_token
                .ref_(self)
                .end()
                .try_into()
                .unwrap(),
        );
        start_line != end_line
            && self.grammar_error_on_node(
                equals_greater_than_token,
                &Diagnostics::Line_terminator_not_permitted_before_arrow,
                None,
            )
    }

    pub(super) fn check_grammar_index_signature_parameters(
        &self,
        node: Id<Node>, /*SignatureDeclaration*/
    ) -> io::Result<bool> {
        let parameter = node
            .ref_(self)
            .as_signature_declaration()
            .parameters()
            .ref_(self)
            .get(0)
            .cloned();
        if node
            .ref_(self)
            .as_signature_declaration()
            .parameters()
            .ref_(self)
            .len()
            != 1
        {
            if let Some(parameter) = parameter.as_ref() {
                return Ok(self.grammar_error_on_node(
                    parameter.ref_(self).as_parameter_declaration().name(),
                    &Diagnostics::An_index_signature_must_have_exactly_one_parameter,
                    None,
                ));
            } else {
                return Ok(self.grammar_error_on_node(
                    node,
                    &Diagnostics::An_index_signature_must_have_exactly_one_parameter,
                    None,
                ));
            }
        }
        self.check_grammar_for_disallowed_trailing_comma(
            Some(node.ref_(self).as_signature_declaration().parameters()),
            Some(&Diagnostics::An_index_signature_cannot_have_a_trailing_comma),
        );
        let parameter = parameter.unwrap();
        if let Some(parameter_dot_dot_dot_token) = parameter
            .ref_(self)
            .as_parameter_declaration()
            .dot_dot_dot_token
        {
            return Ok(self.grammar_error_on_node(
                parameter_dot_dot_dot_token,
                &Diagnostics::An_index_signature_cannot_have_a_rest_parameter,
                None,
            ));
        }
        if has_effective_modifiers(parameter, self) {
            return Ok(self.grammar_error_on_node(
                parameter.ref_(self).as_parameter_declaration().name(),
                &Diagnostics::An_index_signature_parameter_cannot_have_an_accessibility_modifier,
                None,
            ));
        }
        if let Some(parameter_question_token) = parameter
            .ref_(self)
            .as_parameter_declaration()
            .question_token
        {
            return Ok(self.grammar_error_on_node(
                parameter_question_token,
                &Diagnostics::An_index_signature_parameter_cannot_have_a_question_mark,
                None,
            ));
        }
        if parameter
            .ref_(self)
            .as_parameter_declaration()
            .maybe_initializer()
            .is_some()
        {
            return Ok(self.grammar_error_on_node(
                parameter.ref_(self).as_parameter_declaration().name(),
                &Diagnostics::An_index_signature_parameter_cannot_have_an_initializer,
                None,
            ));
        }
        if parameter
            .ref_(self)
            .as_parameter_declaration()
            .maybe_type()
            .is_none()
        {
            return Ok(self.grammar_error_on_node(
                parameter.ref_(self).as_parameter_declaration().name(),
                &Diagnostics::An_index_signature_parameter_must_have_a_type_annotation,
                None,
            ));
        }
        let type_ = self.get_type_from_type_node_(released!(parameter
            .ref_(self)
            .as_parameter_declaration()
            .maybe_type()
            .unwrap()))?;
        if self.some_type(type_, |t: Id<Type>| {
            t.ref_(self)
                .flags()
                .intersects(TypeFlags::StringOrNumberLiteralOrUnique)
        }) || self.is_generic_type(type_)?
        {
            return Ok(self.grammar_error_on_node(
                parameter.ref_(self).as_parameter_declaration().name(),
                &Diagnostics::An_index_signature_parameter_type_cannot_be_a_literal_type_or_generic_type_Consider_using_a_mapped_object_type_instead,
                None,
            ));
        }
        if !self.try_every_type(type_, |type_: Id<Type>| self.is_valid_index_key_type(type_))? {
            return Ok(self.grammar_error_on_node(
                parameter.ref_(self).as_parameter_declaration().name(),
                &Diagnostics::An_index_signature_parameter_type_must_be_string_number_symbol_or_a_template_literal_type,
                None,
            ));
        }
        if node
            .ref_(self)
            .as_signature_declaration()
            .maybe_type()
            .is_none()
        {
            return Ok(self.grammar_error_on_node(
                node,
                &Diagnostics::An_index_signature_must_have_a_type_annotation,
                None,
            ));
        }
        Ok(false)
    }

    pub(super) fn check_grammar_index_signature(
        &self,
        node: Id<Node>, /*SignatureDeclaration*/
    ) -> io::Result<bool> {
        Ok(self.check_grammar_decorators_and_modifiers(node)
            || self.check_grammar_index_signature_parameters(node)?)
    }

    pub(super) fn check_grammar_for_at_least_one_type_argument(
        &self,
        node: Id<Node>,
        type_arguments: Option<Id<NodeArray> /*<TypeNode>*/>,
    ) -> bool {
        if let Some(type_arguments) =
            type_arguments.filter(|type_arguments| type_arguments.ref_(self).is_empty())
        {
            let source_file = get_source_file_of_node(node, self);
            let start =
                type_arguments.ref_(self).pos() - TryInto::<isize>::try_into("<".len()).unwrap();
            let end = skip_trivia(
                &source_file.ref_(self).as_source_file().text_as_chars(),
                type_arguments.ref_(self).end(),
                None,
                None,
                None,
            ) + TryInto::<isize>::try_into(">".len()).unwrap();
            return self.grammar_error_at_pos(
                source_file,
                start,
                end - start,
                &Diagnostics::Type_argument_list_cannot_be_empty,
                None,
            );
        }
        false
    }

    pub(super) fn check_grammar_type_arguments(
        &self,
        node: Id<Node>,
        type_arguments: Option<Id<NodeArray> /*<TypeNode>*/>,
    ) -> bool {
        self.check_grammar_for_disallowed_trailing_comma(type_arguments, None)
            || self.check_grammar_for_at_least_one_type_argument(node, type_arguments)
    }

    pub(super) fn check_grammar_tagged_template_chain(
        &self,
        node: Id<Node>, /*TaggedTemplateExpression*/
    ) -> bool {
        let node_ref = node.ref_(self);
        let node_as_tagged_template_expression = node_ref.as_tagged_template_expression();
        if node_as_tagged_template_expression
            .question_dot_token
            .is_some()
            || node.ref_(self).flags().intersects(NodeFlags::OptionalChain)
        {
            return self.grammar_error_on_node(
                node_as_tagged_template_expression.template,
                &Diagnostics::Tagged_template_expressions_are_not_permitted_in_an_optional_chain,
                None,
            );
        }
        false
    }

    pub(super) fn check_grammar_for_omitted_argument(
        &self,
        args: Option<Id<NodeArray> /*<Expression>*/>,
    ) -> bool {
        if let Some(args) = args {
            for &arg in &*args.ref_(self) {
                if arg.ref_(self).kind() == SyntaxKind::OmittedExpression {
                    return self.grammar_error_at_pos(
                        arg,
                        arg.ref_(self).pos(),
                        0,
                        &Diagnostics::Argument_expression_expected,
                        None,
                    );
                }
            }
        }
        false
    }

    pub(super) fn check_grammar_arguments(
        &self,
        args: Option<Id<NodeArray> /*<Expression>*/>,
    ) -> bool {
        self.check_grammar_for_omitted_argument(args)
    }

    pub(super) fn check_grammar_heritage_clause(
        &self,
        node: Id<Node>, /*HeritageClause*/
    ) -> bool {
        let node_ref = node.ref_(self);
        let node_as_heritage_clause = node_ref.as_heritage_clause();
        let types = node_as_heritage_clause.types;
        if self.check_grammar_for_disallowed_trailing_comma(Some(types), None) {
            return true;
        }
        if
        /*types &&*/
        types.ref_(self).is_empty() {
            let list_type = token_to_string(node_as_heritage_clause.token);
            return self.grammar_error_at_pos(
                node,
                types.ref_(self).pos(),
                0,
                &Diagnostics::_0_list_cannot_be_empty,
                Some(vec![list_type.unwrap().to_owned()]),
            );
        }
        some(
            Some(&*types.ref_(self)),
            Some(|&type_: &Id<Node>| self.check_grammar_expression_with_type_arguments(type_)),
        )
    }

    pub(super) fn check_grammar_expression_with_type_arguments(
        &self,
        node: Id<Node>, /*ExpressionWithTypeArguments*/
    ) -> bool {
        self.check_grammar_type_arguments(
            node,
            node.ref_(self)
                .as_expression_with_type_arguments()
                .maybe_type_arguments(),
        )
    }

    pub(super) fn check_grammar_class_declaration_heritage_clauses(
        &self,
        node: Id<Node>, /*ClassLikeDeclaration*/
    ) -> bool {
        let mut seen_extends_clause = false;
        let mut seen_implements_clause = false;

        if !self.check_grammar_decorators_and_modifiers(node) {
            if let Some(node_heritage_clauses) = node
                .ref_(self)
                .as_class_like_declaration()
                .maybe_heritage_clauses()
            {
                for &heritage_clause in &*node_heritage_clauses.ref_(self) {
                    let heritage_clause_ref = heritage_clause.ref_(self);
                    let heritage_clause_as_heritage_clause =
                        heritage_clause_ref.as_heritage_clause();
                    if heritage_clause_as_heritage_clause.token == SyntaxKind::ExtendsKeyword {
                        if seen_extends_clause {
                            return self.grammar_error_on_first_token(
                                heritage_clause,
                                &Diagnostics::extends_clause_already_seen,
                                None,
                            );
                        }

                        if seen_implements_clause {
                            return self.grammar_error_on_first_token(
                                heritage_clause,
                                &Diagnostics::extends_clause_must_precede_implements_clause,
                                None,
                            );
                        }

                        if heritage_clause_as_heritage_clause.types.ref_(self).len() > 1 {
                            return self.grammar_error_on_first_token(
                                heritage_clause_as_heritage_clause.types.ref_(self)[1],
                                &Diagnostics::Classes_can_only_extend_a_single_class,
                                None,
                            );
                        }

                        seen_extends_clause = true;
                    } else {
                        Debug_.assert(
                            heritage_clause_as_heritage_clause.token
                                == SyntaxKind::ImplementsKeyword,
                            None,
                        );
                        if seen_implements_clause {
                            return self.grammar_error_on_first_token(
                                heritage_clause,
                                &Diagnostics::implements_clause_already_seen,
                                None,
                            );
                        }

                        seen_implements_clause = true;
                    }

                    self.check_grammar_heritage_clause(heritage_clause);
                }
            }
        }
        false
    }

    pub(super) fn check_grammar_interface_declaration(
        &self,
        node: Id<Node>, /*InterfaceDeclaration*/
    ) -> bool {
        let mut seen_extends_clause = false;

        if let Some(node_heritage_clauses) = node
            .ref_(self)
            .as_interface_declaration()
            .maybe_heritage_clauses()
        {
            for &heritage_clause in &*node_heritage_clauses.ref_(self) {
                let heritage_clause_ref = heritage_clause.ref_(self);
                let heritage_clause_as_heritage_clause = heritage_clause_ref.as_heritage_clause();
                if heritage_clause_as_heritage_clause.token == SyntaxKind::ExtendsKeyword {
                    if seen_extends_clause {
                        return self.grammar_error_on_first_token(
                            heritage_clause,
                            &Diagnostics::extends_clause_already_seen,
                            None,
                        );
                    }

                    seen_extends_clause = true;
                } else {
                    Debug_.assert(
                        heritage_clause_as_heritage_clause.token == SyntaxKind::ImplementsKeyword,
                        None,
                    );
                    return self.grammar_error_on_first_token(
                        heritage_clause,
                        &Diagnostics::Interface_declaration_cannot_have_implements_clause,
                        None,
                    );
                }

                self.check_grammar_heritage_clause(heritage_clause);
            }
        }
        false
    }

    pub(super) fn check_grammar_computed_property_name(&self, node: Id<Node>) -> bool {
        if node.ref_(self).kind() != SyntaxKind::ComputedPropertyName {
            return false;
        }

        let node_ref = node.ref_(self);
        let computed_property_name = node_ref.as_computed_property_name();
        if computed_property_name.expression.ref_(self).kind() == SyntaxKind::BinaryExpression
            && computed_property_name
                .expression
                .ref_(self)
                .as_binary_expression()
                .operator_token
                .ref_(self)
                .kind()
                == SyntaxKind::CommaToken
        {
            return self.grammar_error_on_node(
                computed_property_name.expression,
                &Diagnostics::A_comma_expression_is_not_allowed_in_a_computed_property_name,
                None,
            );
        }
        false
    }

    pub(super) fn check_grammar_for_generator(
        &self,
        node: Id<Node>, /*FunctionLikeDeclaration*/
    ) -> bool {
        let node_ref = node.ref_(self);
        let node_as_function_like_declaration = node_ref.as_function_like_declaration();
        if let Some(node_asterisk_token) = node_as_function_like_declaration.maybe_asterisk_token()
        {
            Debug_.assert(
                matches!(
                    node.ref_(self).kind(),
                    SyntaxKind::FunctionDeclaration
                        | SyntaxKind::FunctionExpression
                        | SyntaxKind::MethodDeclaration
                ),
                None,
            );
            if node.ref_(self).flags().intersects(NodeFlags::Ambient) {
                return self.grammar_error_on_node(
                    node_asterisk_token,
                    &Diagnostics::Generators_are_not_allowed_in_an_ambient_context,
                    None,
                );
            }
            if node_as_function_like_declaration.maybe_body().is_none() {
                return self.grammar_error_on_node(
                    node_asterisk_token,
                    &Diagnostics::An_overload_signature_cannot_be_declared_as_a_generator,
                    None,
                );
            }
        }
        false
    }

    pub(super) fn check_grammar_for_invalid_question_mark(
        &self,
        question_token: Option<Id<Node> /*QuestionToken*/>,
        message: &DiagnosticMessage,
    ) -> bool {
        if let Some(question_token) = question_token {
            self.grammar_error_on_node(question_token, message, None)
        } else {
            false
        }
    }

    pub(super) fn check_grammar_for_invalid_exclamation_token(
        &self,
        exclamation_token: Option<Id<Node> /*ExclamationToken*/>,
        message: &DiagnosticMessage,
    ) -> bool {
        if let Some(exclamation_token) = exclamation_token {
            self.grammar_error_on_node(exclamation_token, message, None)
        } else {
            false
        }
    }

    pub(super) fn check_grammar_object_literal_expression(
        &self,
        node: Id<Node>, /*ObjectLiteralExpression*/
        in_destructuring: bool,
    ) -> bool {
        let mut seen: HashMap<__String, DeclarationMeaning> = HashMap::new();

        for prop in &*node
            .ref_(self)
            .as_object_literal_expression()
            .properties
            .ref_(self)
        {
            if prop.ref_(self).kind() == SyntaxKind::SpreadAssignment {
                if in_destructuring {
                    let expression = skip_parentheses(
                        prop.ref_(self).as_spread_assignment().expression,
                        None,
                        self,
                    );
                    if is_array_literal_expression(&expression.ref_(self))
                        || is_object_literal_expression(&expression.ref_(self))
                    {
                        return self.grammar_error_on_node(
                            prop.ref_(self).as_spread_assignment().expression,
                            &Diagnostics::A_rest_element_cannot_contain_a_binding_pattern,
                            None,
                        );
                    }
                }
                continue;
            }
            let name = prop.ref_(self).as_named_declaration().name();
            if name.ref_(self).kind() == SyntaxKind::ComputedPropertyName {
                self.check_grammar_computed_property_name(name);
            }

            if prop.ref_(self).kind() == SyntaxKind::ShorthandPropertyAssignment
                && !in_destructuring
                && prop
                    .ref_(self)
                    .as_shorthand_property_assignment()
                    .object_assignment_initializer
                    .is_some()
            {
                return self.grammar_error_on_node(
                    prop.ref_(self).as_shorthand_property_assignment().equals_token.unwrap(),
                    &Diagnostics::Did_you_mean_to_use_a_Colon_An_can_only_follow_a_property_name_when_the_containing_object_literal_is_part_of_a_destructuring_pattern,
                    None,
                );
            }

            if name.ref_(self).kind() == SyntaxKind::PrivateIdentifier {
                self.grammar_error_on_node(
                    name,
                    &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                    None,
                );
            }

            if let Some(prop_modifiers) = prop.ref_(self).maybe_modifiers() {
                for &mod_ in &*prop_modifiers.ref_(self) {
                    if mod_.ref_(self).kind() != SyntaxKind::AsyncKeyword
                        || prop.ref_(self).kind() != SyntaxKind::MethodDeclaration
                    {
                        self.grammar_error_on_node(
                            mod_,
                            &Diagnostics::_0_modifier_cannot_be_used_here,
                            Some(vec![get_text_of_node(mod_, None, self).into_owned()]),
                        );
                    }
                }
            }

            let current_kind: DeclarationMeaning;
            match prop.ref_(self).kind() {
                SyntaxKind::ShorthandPropertyAssignment => {
                    self.check_grammar_for_invalid_exclamation_token(
                        prop.ref_(self).as_shorthand_property_assignment().exclamation_token,
                        &Diagnostics::A_definite_assignment_assertion_is_not_permitted_in_this_context,
                    );
                    self.check_grammar_for_invalid_question_mark(
                        prop.ref_(self)
                            .as_has_question_token()
                            .maybe_question_token(),
                        &Diagnostics::An_object_member_cannot_be_declared_optional,
                    );
                    if name.ref_(self).kind() == SyntaxKind::NumericLiteral {
                        self.check_grammar_numeric_literal(name);
                    }
                    current_kind = DeclarationMeaning::PropertyAssignment;
                }
                SyntaxKind::PropertyAssignment => {
                    self.check_grammar_for_invalid_question_mark(
                        prop.ref_(self)
                            .as_has_question_token()
                            .maybe_question_token(),
                        &Diagnostics::An_object_member_cannot_be_declared_optional,
                    );
                    if name.ref_(self).kind() == SyntaxKind::NumericLiteral {
                        self.check_grammar_numeric_literal(name);
                    }
                    current_kind = DeclarationMeaning::PropertyAssignment;
                }
                SyntaxKind::MethodDeclaration => {
                    current_kind = DeclarationMeaning::Method;
                }
                SyntaxKind::GetAccessor => {
                    current_kind = DeclarationMeaning::GetAccessor;
                }
                SyntaxKind::SetAccessor => {
                    current_kind = DeclarationMeaning::SetAccessor;
                }
                _ => Debug_.assert_never(
                    prop,
                    Some(&format!(
                        "Unexpected syntax kind:{:?}",
                        prop.ref_(self).kind()
                    )),
                ),
            }

            if !in_destructuring {
                let effective_name = get_property_name_for_property_name_node(name, self);
                if effective_name.is_none() {
                    continue;
                }
                let effective_name = effective_name.unwrap();

                let existing_kind = seen.get(&*effective_name).copied();
                match existing_kind {
                    None => {
                        seen.insert(effective_name, current_kind);
                    }
                    Some(existing_kind) => {
                        if current_kind.intersects(DeclarationMeaning::PropertyAssignmentOrMethod)
                            && existing_kind
                                .intersects(DeclarationMeaning::PropertyAssignmentOrMethod)
                        {
                            self.grammar_error_on_node(
                                name,
                                &Diagnostics::Duplicate_identifier_0,
                                Some(vec![get_text_of_node(name, None, self).into_owned()]),
                            );
                        } else if current_kind.intersects(DeclarationMeaning::GetOrSetAccessor)
                            && existing_kind.intersects(DeclarationMeaning::GetOrSetAccessor)
                        {
                            if existing_kind != DeclarationMeaning::GetOrSetAccessor
                                && current_kind != existing_kind
                            {
                                seen.insert(effective_name, current_kind | existing_kind);
                            } else {
                                return self.grammar_error_on_node(
                                    name,
                                    &Diagnostics::An_object_literal_cannot_have_multiple_get_Slashset_accessors_with_the_same_name,
                                    None,
                                );
                            }
                        } else {
                            return self.grammar_error_on_node(
                                name,
                                &Diagnostics::An_object_literal_cannot_have_property_and_accessor_with_the_same_name,
                                None
                            );
                        }
                    }
                }
            }
        }
        false
    }
}
