#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;

use super::{DeclarationMeaning, EmitResolverCreateResolver, UnusedKind};
use crate::{
    add_related_info, create_diagnostic_for_node, file_extension_is_one_of, filter,
    find_use_strict_prologue, get_line_and_character_of_position,
    get_property_name_for_property_name_node, get_source_file_of_node, get_text_of_node,
    has_effective_modifiers, is_array_literal_expression, is_arrow_function, is_binding_pattern,
    is_block, is_function_like_declaration, is_object_literal_expression, is_rest_parameter,
    length, skip_parentheses, skip_trivia, some, token_to_string, Debug_, DiagnosticMessage,
    DiagnosticRelatedInformation, Diagnostics, Extension, ExternalEmitHelpers,
    HasInitializerInterface, HasTypeInterface, HasTypeParametersInterface,
    InterfaceOrClassLikeDeclarationInterface, LineAndCharacter, NamedDeclarationInterface,
    NodeArray, NodeCheckFlags, NodeFlags, ReadonlyTextRange, ScriptTarget, SourceFileLike,
    SyntaxKind, TypeFlags, TypeInterface, __String, bind_source_file, for_each,
    is_external_or_common_js_module, CancellationTokenDebuggable, Diagnostic,
    EmitResolverDebuggable, IndexInfo, Node, NodeInterface, StringOrNumber, Symbol, SymbolFlags,
    Type, TypeChecker,
};

impl TypeChecker {
    pub(super) fn is_duplicated_common_js_export(&self, declarations: Option<&[Rc<Node>]>) -> bool {
        unimplemented!()
    }

    pub(super) fn check_source_element<TNodeRef: Borrow<Node>>(&self, node: Option<TNodeRef>) {
        if let Some(node) = node {
            let node = node.borrow();
            self.check_source_element_worker(node);
        }
    }

    pub(super) fn check_source_element_worker(&self, node: &Node) {
        match node {
            Node::PropertySignature(_) => self.check_property_signature(node),
            Node::TypeReferenceNode(_) => self.check_type_reference_node(node),
            Node::KeywordTypeNode(_) | Node::LiteralTypeNode(_) => (),
            Node::ArrayTypeNode(_) => self.check_array_type(node),
            Node::UnionTypeNode(_) => self.check_union_or_intersection_type(node),
            Node::FunctionDeclaration(_) => self.check_function_declaration(node),
            Node::Block(_) => self.check_block(node),
            Node::VariableStatement(_) => self.check_variable_statement(node),
            Node::ExpressionStatement(_) => self.check_expression_statement(node),
            Node::IfStatement(_) => self.check_if_statement(node),
            Node::ReturnStatement(_) => self.check_return_statement(node),
            Node::VariableDeclaration(_) => self.check_variable_declaration(node),
            Node::InterfaceDeclaration(_) => self.check_interface_declaration(node),
            Node::TypeAliasDeclaration(_) => self.check_type_alias_declaration(node),
            _ => unimplemented!("{:?}", node.kind()),
        };
    }

    pub(super) fn get_type_from_jsdoc_variadic_type(
        &self,
        node: &Node, /*JSDocVariadicType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_node_deferred(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn check_source_file(&self, source_file: &Node /*SourceFile*/) {
        self.check_source_file_worker(source_file)
    }

    pub(super) fn unused_is_error(&self, kind: UnusedKind, is_ambient: bool) -> bool {
        unimplemented!()
    }

    pub(super) fn get_potentially_unused_identifiers(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Vec<Rc<Node /*PotentiallyUnusedIdentifier*/>> {
        unimplemented!()
    }

    pub(super) fn check_source_file_worker(&self, node: &Node /*SourceFile*/) {
        if true {
            for_each(&node.as_source_file().statements, |statement, _index| {
                self.check_source_element(Some(&**statement));
                Option::<()>::None
            });
        }
    }

    pub fn get_diagnostics(
        &self,
        source_file: &Node, /*SourceFile*/
        ct: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_worker(source_file)
    }

    pub(super) fn get_diagnostics_worker(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Vec<Rc<Diagnostic>> {
        self.check_source_file(source_file);

        let semantic_diagnostics = self
            .diagnostics()
            .get_diagnostics(Some(&source_file.as_source_file().file_name()));

        semantic_diagnostics
    }

    pub(super) fn get_symbols_in_scope_(
        &self,
        location_in: &Node,
        meaning: SymbolFlags,
    ) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn is_type_declaration(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_type_reference_identifier(&self, node: &Node /*EntityName*/) -> bool {
        unimplemented!()
    }

    pub(super) fn for_each_enclosing_class<TReturn, TCallback: FnMut(&Node) -> Option<TReturn>>(
        &self,
        node: &Node,
        callback: TCallback,
    ) -> Option<TReturn> {
        unimplemented!()
    }

    pub(super) fn is_node_used_during_class_initialization(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_node_within_class(
        &self,
        node: &Node,
        class_declaration: &Node, /*ClassLikeDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_in_right_side_of_import_or_export_assignment(
        &self,
        node: &Node, /*EntityName*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_symbol_at_location_(
        &self,
        node: &Node,
        ignore_errors: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_index_infos_at_location_(&self, node: &Node) -> Option<Vec<Rc<IndexInfo>>> {
        unimplemented!()
    }

    pub(super) fn get_shorthand_assignment_value_symbol_<TNode: Borrow<Node>>(
        &self,
        location: Option<TNode>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_export_specifier_local_target_symbol_(
        &self,
        node: &Node, /*Identifier | ExportSpecifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_node(&self, node: &Node) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_of_assignment_pattern_(
        &self,
        expr: &Node, /*AssignmentPattern*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_property_symbol_of_destructuring_assignment_(
        &self,
        location: &Node, /*Identifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_parent_type_of_class_element(
        &self,
        node: &Node, /*ClassElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_class_element_property_key_type(
        &self,
        element: &Node, /*ClassElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_augmented_properties_of_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn type_has_call_or_construct_signatures(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_const_enum_or_const_enum_only_module(&self, s: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn is_implementation_of_overload_(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_node_check_flags(&self, node: &Node) -> NodeCheckFlags {
        unimplemented!()
    }

    pub(super) fn get_enum_member_value(
        &self,
        node: &Node, /*EnumMember*/
    ) -> Option<StringOrNumber> {
        unimplemented!()
    }

    pub(super) fn can_have_constant_value(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn get_constant_value_(
        &self,
        node: &Node, /*EnumMember | AccessExpression*/
    ) -> Option<StringOrNumber> {
        unimplemented!()
    }

    pub(super) fn is_function_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_referenced_value_symbol(
        &self,
        reference: &Node, /*Identifier*/
        start_in_declaration_container: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_factory_entity(
        &self,
        location: &Node,
    ) -> Option<Rc<Node /*EntityName*/>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_fragment_factory_entity(
        &self,
        location: &Node,
    ) -> Option<Rc<Node /*EntityName*/>> {
        unimplemented!()
    }

    pub(super) fn create_resolver(&self) -> Rc<dyn EmitResolverDebuggable> {
        Rc::new(EmitResolverCreateResolver::new())
    }

    pub(super) fn initialize_type_checker(&mut self) {
        for file in self.host.get_source_files() {
            bind_source_file(&*file, self.compiler_options.clone());
            println!("post-binding: {:#?}", file);
        }

        for file in self.host.get_source_files() {
            if !is_external_or_common_js_module(&file) {
                self.merge_symbol_table(
                    &mut *self.globals_mut(),
                    &RefCell::borrow(&file.locals()),
                    None,
                );
            }
        }

        self.global_object_type =
            self.get_global_type(&__String::new("Object".to_owned()), 0, true);
        self.global_boolean_type =
            self.get_global_type(&__String::new("Boolean".to_owned()), 0, true);
    }

    pub(super) fn check_external_emit_helpers(
        &self,
        location: &Node,
        helpers: ExternalEmitHelpers,
    ) {
        unimplemented!()
    }

    pub(super) fn check_grammar_decorators_and_modifiers(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn report_obvious_modifier_errors(&self, node: &Node) -> Option<bool> {
        if node.maybe_modifiers().is_none() {
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

    pub(super) fn should_report_bad_modifier(&self, node: &Node) -> bool {
        match node.kind() {
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
                    node.parent().kind(),
                    SyntaxKind::ModuleBlock | SyntaxKind::SourceFile
                ) {
                    return false;
                }
                match node.kind() {
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
        node: &Node,
        allowed_modifier: SyntaxKind,
    ) -> bool {
        let node_modifiers = node.maybe_modifiers();
        let node_modifiers = node_modifiers.as_ref().unwrap();
        node_modifiers.len() > 1 || node_modifiers[0].kind() != allowed_modifier
    }

    pub(super) fn check_grammar_async_modifier(&self, node: &Node, async_modifier: &Node) -> bool {
        if matches!(
            node.kind(),
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
        list: Option<&NodeArray>,
        diag: Option<&'static DiagnosticMessage>,
    ) -> bool {
        let diag = diag.unwrap_or(&Diagnostics::Trailing_comma_not_allowed);
        if let Some(list) = list.filter(|list| list.has_trailing_comma) {
            return self.grammar_error_at_pos(
                &list[0],
                list.end() - TryInto::<isize>::try_into(",".len()).unwrap(),
                TryInto::<isize>::try_into(",".len()).unwrap(),
                diag,
                None,
            );
        }
        false
    }

    pub(super) fn check_grammar_type_parameter_list(
        &self,
        type_parameters: Option<&NodeArray /*<TypeParameterDeclaration>*/>,
        file: &Node, /*SourceFile*/
    ) -> bool {
        if let Some(type_parameters) =
            type_parameters.filter(|type_parameters| type_parameters.is_empty())
        {
            let start = type_parameters.pos() - TryInto::<isize>::try_into("<".len()).unwrap();
            let end = skip_trivia(
                &file.as_source_file().text_as_chars(),
                type_parameters.end(),
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
        parameters: &NodeArray, /*<ParameterDeclaration>*/
    ) -> bool {
        let mut seen_optional_parameter = false;
        let parameter_count = parameters.len();

        for i in 0..parameter_count {
            let parameter = &parameters[i];
            let parameter_as_parameter_declaration = parameter.as_parameter_declaration();
            if let Some(parameter_dot_dot_dot_token) = parameter_as_parameter_declaration
                .dot_dot_dot_token
                .as_ref()
            {
                if i != parameter_count - 1 {
                    return self.grammar_error_on_node(
                        parameter_dot_dot_dot_token,
                        &Diagnostics::A_rest_parameter_must_be_last_in_a_parameter_list,
                        None,
                    );
                }
                if !parameter.flags().intersects(NodeFlags::Ambient) {
                    self.check_grammar_for_disallowed_trailing_comma(
                        Some(parameters),
                        Some(&Diagnostics::A_rest_parameter_or_binding_pattern_may_not_have_a_trailing_comma)
                    );
                }

                if let Some(parameter_question_token) =
                    parameter_as_parameter_declaration.question_token.as_ref()
                {
                    return self.grammar_error_on_node(
                        parameter_question_token,
                        &Diagnostics::A_rest_parameter_cannot_be_optional,
                        None,
                    );
                }

                if parameter_as_parameter_declaration
                    .maybe_initializer()
                    .is_some()
                {
                    return self.grammar_error_on_node(
                        &parameter_as_parameter_declaration.name(),
                        &Diagnostics::A_rest_parameter_cannot_have_an_initializer,
                        None,
                    );
                }
            } else if self.is_optional_parameter_(parameter) {
                seen_optional_parameter = true;
                if parameter_as_parameter_declaration.question_token.is_some()
                    && parameter_as_parameter_declaration
                        .maybe_initializer()
                        .is_some()
                {
                    return self.grammar_error_on_node(
                        &parameter_as_parameter_declaration.name(),
                        &Diagnostics::Parameter_cannot_have_question_mark_and_initializer,
                        None,
                    );
                }
            } else if seen_optional_parameter
                && parameter_as_parameter_declaration
                    .maybe_initializer()
                    .is_none()
            {
                return self.grammar_error_on_node(
                    &parameter_as_parameter_declaration.name(),
                    &Diagnostics::A_required_parameter_cannot_follow_an_optional_parameter,
                    None,
                );
            }
        }
        false
    }

    pub(super) fn get_non_simple_parameters(
        &self,
        parameters: &[Rc<Node /*ParameterDeclaration*/>],
    ) -> Vec<Rc<Node /*ParameterDeclaration*/>> {
        filter(parameters, |parameter: &Rc<Node>| {
            let parameter_as_parameter_declaration = parameter.as_parameter_declaration();
            parameter_as_parameter_declaration
                .maybe_initializer()
                .is_some()
                || is_binding_pattern(parameter_as_parameter_declaration.maybe_name())
                || is_rest_parameter(parameter)
        })
    }

    pub(super) fn check_grammar_for_use_strict_simple_parameter_list(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> bool {
        if self.language_version >= ScriptTarget::ES2016 {
            let node_as_function_like_declaration = node.as_function_like_declaration();
            let use_strict_directive = node_as_function_like_declaration
                .maybe_body()
                .as_ref()
                .filter(|node_body| is_block(node_body))
                .and_then(|node_body| find_use_strict_prologue(&node_body.as_block().statements));
            if let Some(use_strict_directive) = use_strict_directive.as_ref() {
                let non_simple_parameters =
                    self.get_non_simple_parameters(node_as_function_like_declaration.parameters());
                if length(Some(&*non_simple_parameters)) > 0 {
                    for_each(
                        &non_simple_parameters,
                        |parameter: &Rc<Node>, _| -> Option<()> {
                            add_related_info(
                                &self.error(
                                    Some(&**parameter),
                                    &Diagnostics::This_parameter_is_not_allowed_with_use_strict_directive,
                                    None,
                                ),
                                vec![
                                    Rc::new(
                                        create_diagnostic_for_node(
                                            use_strict_directive,
                                            &Diagnostics::use_strict_directive_used_here,
                                            None,
                                        ).into()
                                    )
                                ]
                            );
                            None
                        },
                    );

                    let diagnostics: Vec<Rc<DiagnosticRelatedInformation>> = non_simple_parameters
                        .iter()
                        .enumerate()
                        .map(|(index, parameter)| {
                            Rc::new(if index == 0 {
                                create_diagnostic_for_node(
                                    parameter,
                                    &Diagnostics::Non_simple_parameter_declared_here,
                                    None,
                                )
                                .into()
                            } else {
                                create_diagnostic_for_node(parameter, &Diagnostics::and_here, None)
                                    .into()
                            })
                        })
                        .collect();
                    add_related_info(
                        &self.error(
                            Some(&**use_strict_directive),
                            &Diagnostics::use_strict_directive_cannot_be_used_with_non_simple_parameter_list,
                            None,
                        ),
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
        node: &Node, /*FunctionLikeDeclaration | MethodSignature*/
    ) -> bool {
        let file = get_source_file_of_node(Some(node)).unwrap();
        let node_as_signature_declaration = node.as_signature_declaration();
        self.check_grammar_decorators_and_modifiers(node)
            || self.check_grammar_type_parameter_list(
                node_as_signature_declaration
                    .maybe_type_parameters()
                    .as_ref(),
                &file,
            )
            || self.check_grammar_parameter_list(node_as_signature_declaration.parameters())
            || self.check_grammar_arrow_function(node, &file)
            || is_function_like_declaration(node)
                && self.check_grammar_for_use_strict_simple_parameter_list(node)
    }

    pub(super) fn check_grammar_class_like_declaration(
        &self,
        node: &Node, /*ClassLikeDeclaration*/
    ) -> bool {
        let file = get_source_file_of_node(Some(node)).unwrap();
        self.check_grammar_class_declaration_heritage_clauses(node)
            || self.check_grammar_type_parameter_list(
                node.as_has_type_parameters()
                    .maybe_type_parameters()
                    .as_ref(),
                &file,
            )
    }

    pub(super) fn check_grammar_arrow_function(
        &self,
        node: &Node,
        file: &Node, /*SourceFile*/
    ) -> bool {
        if !is_arrow_function(node) {
            return false;
        }
        let node_as_arrow_function = node.as_arrow_function();

        if let Some(node_type_parameters) = node_as_arrow_function
            .maybe_type_parameters()
            .as_ref()
            .filter(|node_type_parameters| {
                !(node_type_parameters.len() > 1
                    || node_type_parameters.has_trailing_comma
                    || node_type_parameters[0]
                        .as_type_parameter_declaration()
                        .constraint
                        .is_some())
            })
        {
            if
            /*file &&*/
            file_extension_is_one_of(
                &file.as_source_file().file_name(),
                &[Extension::Mts.to_str(), Extension::Cts.to_str()],
            ) {
                self.grammar_error_on_node(
                    &node_type_parameters[0],
                    &Diagnostics::This_syntax_is_reserved_in_files_with_the_mts_or_cts_extension_Add_a_trailing_comma_or_explicit_constraint,
                    None,
                );
            }
        }

        let equals_greater_than_token = &node_as_arrow_function.equals_greater_than_token;
        let LineAndCharacter {
            line: start_line, ..
        } = get_line_and_character_of_position(
            file.as_source_file(),
            equals_greater_than_token.pos().try_into().unwrap(),
        );
        let LineAndCharacter { line: end_line, .. } = get_line_and_character_of_position(
            file.as_source_file(),
            equals_greater_than_token.end().try_into().unwrap(),
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
        node: &Node, /*SignatureDeclaration*/
    ) -> bool {
        let node_as_signature_declaration = node.as_signature_declaration();
        let parameter = node_as_signature_declaration.parameters().get(0);
        if node_as_signature_declaration.parameters().len() != 1 {
            if let Some(parameter) = parameter {
                return self.grammar_error_on_node(
                    &parameter.as_parameter_declaration().name(),
                    &Diagnostics::An_index_signature_must_have_exactly_one_parameter,
                    None,
                );
            } else {
                return self.grammar_error_on_node(
                    node,
                    &Diagnostics::An_index_signature_must_have_exactly_one_parameter,
                    None,
                );
            }
        }
        self.check_grammar_for_disallowed_trailing_comma(
            Some(node_as_signature_declaration.parameters()),
            Some(&Diagnostics::An_index_signature_cannot_have_a_trailing_comma),
        );
        let parameter = parameter.unwrap();
        let parameter_as_parameter_declaration = parameter.as_parameter_declaration();
        if let Some(parameter_dot_dot_dot_token) = parameter_as_parameter_declaration
            .dot_dot_dot_token
            .as_ref()
        {
            return self.grammar_error_on_node(
                parameter_dot_dot_dot_token,
                &Diagnostics::An_index_signature_cannot_have_a_rest_parameter,
                None,
            );
        }
        if has_effective_modifiers(parameter) {
            return self.grammar_error_on_node(
                &parameter_as_parameter_declaration.name(),
                &Diagnostics::An_index_signature_parameter_cannot_have_an_accessibility_modifier,
                None,
            );
        }
        if let Some(parameter_question_token) =
            parameter_as_parameter_declaration.question_token.as_ref()
        {
            return self.grammar_error_on_node(
                parameter_question_token,
                &Diagnostics::An_index_signature_parameter_cannot_have_a_question_mark,
                None,
            );
        }
        if parameter_as_parameter_declaration
            .maybe_initializer()
            .is_some()
        {
            return self.grammar_error_on_node(
                &parameter_as_parameter_declaration.name(),
                &Diagnostics::An_index_signature_parameter_cannot_have_an_initializer,
                None,
            );
        }
        if parameter_as_parameter_declaration.maybe_type().is_none() {
            return self.grammar_error_on_node(
                &parameter_as_parameter_declaration.name(),
                &Diagnostics::An_index_signature_parameter_must_have_a_type_annotation,
                None,
            );
        }
        let type_ = self
            .get_type_from_type_node_(&parameter_as_parameter_declaration.maybe_type().unwrap());
        if self.some_type(&type_, |t: &Type| {
            t.flags()
                .intersects(TypeFlags::StringOrNumberLiteralOrUnique)
        }) || self.is_generic_type(&type_)
        {
            return self.grammar_error_on_node(
                &parameter_as_parameter_declaration.name(),
                &Diagnostics::An_index_signature_parameter_type_cannot_be_a_literal_type_or_generic_type_Consider_using_a_mapped_object_type_instead,
                None,
            );
        }
        if !self.every_type(&type_, |type_: &Type| self.is_valid_index_key_type(type_)) {
            return self.grammar_error_on_node(
                &parameter_as_parameter_declaration.name(),
                &Diagnostics::An_index_signature_parameter_type_must_be_string_number_symbol_or_a_template_literal_type,
                None,
            );
        }
        if node_as_signature_declaration.maybe_type().is_none() {
            return self.grammar_error_on_node(
                node,
                &Diagnostics::An_index_signature_must_have_a_type_annotation,
                None,
            );
        }
        false
    }

    pub(super) fn check_grammar_index_signature(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) -> bool {
        self.check_grammar_decorators_and_modifiers(node)
            || self.check_grammar_index_signature_parameters(node)
    }

    pub(super) fn check_grammar_for_at_least_one_type_argument(
        &self,
        node: &Node,
        type_arguments: Option<&NodeArray /*<TypeNode>*/>,
    ) -> bool {
        if let Some(type_arguments) =
            type_arguments.filter(|type_arguments| type_arguments.is_empty())
        {
            let source_file = get_source_file_of_node(Some(node)).unwrap();
            let start = type_arguments.pos() - TryInto::<isize>::try_into("<".len()).unwrap();
            let end = skip_trivia(
                &source_file.as_source_file().text_as_chars(),
                type_arguments.end(),
                None,
                None,
                None,
            ) + TryInto::<isize>::try_into(">".len()).unwrap();
            return self.grammar_error_at_pos(
                &source_file,
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
        node: &Node,
        type_arguments: Option<&NodeArray /*<TypeNode>*/>,
    ) -> bool {
        self.check_grammar_for_disallowed_trailing_comma(type_arguments, None)
            || self.check_grammar_for_at_least_one_type_argument(node, type_arguments)
    }

    pub(super) fn check_grammar_tagged_template_chain(
        &self,
        node: &Node, /*TaggedTemplateExpression*/
    ) -> bool {
        let node_as_tagged_template_expression = node.as_tagged_template_expression();
        if node_as_tagged_template_expression
            .question_dot_token
            .is_some()
            || node.flags().intersects(NodeFlags::OptionalChain)
        {
            return self.grammar_error_on_node(
                &node_as_tagged_template_expression.template,
                &Diagnostics::Tagged_template_expressions_are_not_permitted_in_an_optional_chain,
                None,
            );
        }
        false
    }

    pub(super) fn check_grammar_for_omitted_argument(
        &self,
        args: Option<&NodeArray /*<Expression>*/>,
    ) -> bool {
        if let Some(args) = args {
            for arg in args {
                if arg.kind() == SyntaxKind::OmittedExpression {
                    return self.grammar_error_at_pos(
                        arg,
                        arg.pos(),
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
        args: Option<&NodeArray /*<Expression>*/>,
    ) -> bool {
        self.check_grammar_for_omitted_argument(args)
    }

    pub(super) fn check_grammar_heritage_clause(
        &self,
        node: &Node, /*HeritageClause*/
    ) -> bool {
        let node_as_heritage_clause = node.as_heritage_clause();
        let types = &node_as_heritage_clause.types;
        if self.check_grammar_for_disallowed_trailing_comma(Some(types), None) {
            return true;
        }
        if
        /*types &&*/
        types.is_empty() {
            let list_type = token_to_string(node_as_heritage_clause.token);
            return self.grammar_error_at_pos(
                node,
                types.pos(),
                0,
                &Diagnostics::_0_list_cannot_be_empty,
                Some(vec![list_type.unwrap().to_owned()]),
            );
        }
        some(
            Some(&**types),
            Some(|type_: &Rc<Node>| self.check_grammar_expression_with_type_arguments(type_)),
        )
    }

    pub(super) fn check_grammar_expression_with_type_arguments(
        &self,
        node: &Node, /*ExpressionWithTypeArguments*/
    ) -> bool {
        self.check_grammar_type_arguments(
            node,
            node.as_expression_with_type_arguments()
                .type_arguments
                .as_ref(),
        )
    }

    pub(super) fn check_grammar_class_declaration_heritage_clauses(
        &self,
        node: &Node, /*ClassLikeDeclaration*/
    ) -> bool {
        let mut seen_extends_clause = false;
        let mut seen_implements_clause = false;

        if !self.check_grammar_decorators_and_modifiers(node) {
            if let Some(node_heritage_clauses) =
                node.as_class_like_declaration().maybe_heritage_clauses()
            {
                for heritage_clause in node_heritage_clauses {
                    let heritage_clause_as_heritage_clause = heritage_clause.as_heritage_clause();
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

                        if heritage_clause_as_heritage_clause.types.len() > 1 {
                            return self.grammar_error_on_first_token(
                                &heritage_clause_as_heritage_clause.types[1],
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
        node: &Node, /*InterfaceDeclaration*/
    ) -> bool {
        let mut seen_extends_clause = false;

        if let Some(node_heritage_clauses) =
            node.as_interface_declaration().maybe_heritage_clauses()
        {
            for heritage_clause in node_heritage_clauses {
                let heritage_clause_as_heritage_clause = heritage_clause.as_heritage_clause();
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

    pub(super) fn check_grammar_computed_property_name(&self, node: &Node) -> bool {
        if node.kind() != SyntaxKind::ComputedPropertyName {
            return false;
        }

        let computed_property_name = node.as_computed_property_name();
        if computed_property_name.expression.kind() == SyntaxKind::BinaryExpression
            && computed_property_name
                .expression
                .as_binary_expression()
                .operator_token
                .kind()
                == SyntaxKind::CommaToken
        {
            return self.grammar_error_on_node(
                &computed_property_name.expression,
                &Diagnostics::A_comma_expression_is_not_allowed_in_a_computed_property_name,
                None,
            );
        }
        false
    }

    pub(super) fn check_grammar_for_generator(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> bool {
        let node_as_function_like_declaration = node.as_function_like_declaration();
        if let Some(node_asterisk_token) = node_as_function_like_declaration
            .maybe_asterisk_token()
            .as_ref()
        {
            Debug_.assert(
                matches!(
                    node.kind(),
                    SyntaxKind::FunctionDeclaration
                        | SyntaxKind::FunctionExpression
                        | SyntaxKind::MethodDeclaration
                ),
                None,
            );
            if node.flags().intersects(NodeFlags::Ambient) {
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

    pub(super) fn check_grammar_for_invalid_question_mark<TQuestionToken: Borrow<Node>>(
        &self,
        question_token: Option<TQuestionToken /*QuestionToken*/>,
        message: &DiagnosticMessage,
    ) -> bool {
        if let Some(question_token) = question_token {
            self.grammar_error_on_node(question_token.borrow(), message, None)
        } else {
            false
        }
    }

    pub(super) fn check_grammar_for_invalid_exclamation_token<TExclamationToken: Borrow<Node>>(
        &self,
        exclamation_token: Option<TExclamationToken /*ExclamationToken*/>,
        message: &DiagnosticMessage,
    ) -> bool {
        if let Some(exclamation_token) = exclamation_token {
            self.grammar_error_on_node(exclamation_token.borrow(), message, None)
        } else {
            false
        }
    }

    pub(super) fn check_grammar_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        in_destructuring: bool,
    ) -> bool {
        let mut seen: HashMap<__String, DeclarationMeaning> = HashMap::new();

        for prop in &node.as_object_literal_expression().properties {
            if prop.kind() == SyntaxKind::SpreadAssignment {
                if in_destructuring {
                    let expression =
                        skip_parentheses(&prop.as_spread_assignment().expression, None);
                    if is_array_literal_expression(&expression)
                        || is_object_literal_expression(&expression)
                    {
                        return self.grammar_error_on_node(
                            &prop.as_spread_assignment().expression,
                            &Diagnostics::A_rest_element_cannot_contain_a_binding_pattern,
                            None,
                        );
                    }
                }
                continue;
            }
            let name = prop.as_named_declaration().name();
            if name.kind() == SyntaxKind::ComputedPropertyName {
                self.check_grammar_computed_property_name(&name);
            }

            if prop.kind() == SyntaxKind::ShorthandPropertyAssignment
                && !in_destructuring
                && prop
                    .as_shorthand_property_assignment()
                    .object_assignment_initializer
                    .is_some()
            {
                return self.grammar_error_on_node(
                    prop.as_shorthand_property_assignment().equals_token.as_ref().unwrap(),
                    &Diagnostics::Did_you_mean_to_use_a_Colon_An_can_only_follow_a_property_name_when_the_containing_object_literal_is_part_of_a_destructuring_pattern,
                    None,
                );
            }

            if name.kind() == SyntaxKind::PrivateIdentifier {
                self.grammar_error_on_node(
                    &name,
                    &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                    None,
                );
            }

            if let Some(prop_modifiers) = prop.maybe_modifiers().as_ref() {
                for mod_ in prop_modifiers {
                    if mod_.kind() != SyntaxKind::AsyncKeyword
                        || prop.kind() != SyntaxKind::MethodDeclaration
                    {
                        self.grammar_error_on_node(
                            mod_,
                            &Diagnostics::_0_modifier_cannot_be_used_here,
                            Some(vec![get_text_of_node(mod_, None).into_owned()]),
                        );
                    }
                }
            }

            let current_kind: DeclarationMeaning;
            match prop.kind() {
                SyntaxKind::ShorthandPropertyAssignment => {
                    self.check_grammar_for_invalid_exclamation_token(
                        prop.as_shorthand_property_assignment().exclamation_token.as_deref(),
                        &Diagnostics::A_definite_assignment_assertion_is_not_permitted_in_this_context,
                    );
                    self.check_grammar_for_invalid_question_mark(
                        prop.as_has_question_token().maybe_question_token(),
                        &Diagnostics::An_object_member_cannot_be_declared_optional,
                    );
                    if name.kind() == SyntaxKind::NumericLiteral {
                        self.check_grammar_numeric_literal(&name);
                    }
                    current_kind = DeclarationMeaning::PropertyAssignment;
                }
                SyntaxKind::PropertyAssignment => {
                    self.check_grammar_for_invalid_question_mark(
                        prop.as_has_question_token().maybe_question_token(),
                        &Diagnostics::An_object_member_cannot_be_declared_optional,
                    );
                    if name.kind() == SyntaxKind::NumericLiteral {
                        self.check_grammar_numeric_literal(&name);
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
                    Some(&format!("Unexpected syntax kind:{:?}", prop.kind())),
                ),
            }

            if !in_destructuring {
                let effective_name = get_property_name_for_property_name_node(&name);
                if effective_name.is_none() {
                    continue;
                }
                let effective_name = effective_name.unwrap();

                let existing_kind = seen.get(&effective_name).copied();
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
                                &name,
                                &Diagnostics::Duplicate_identifier_0,
                                Some(vec![get_text_of_node(&name, None).into_owned()]),
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
                                    &name,
                                    &Diagnostics::An_object_literal_cannot_have_multiple_get_Slashset_accessors_with_the_same_name,
                                    None,
                                );
                            }
                        } else {
                            return self.grammar_error_on_node(
                                &name,
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
