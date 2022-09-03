#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, IterationTypeKind, IterationUse, UnusedKind};
use crate::{
    create_diagnostic_for_node, for_each, get_class_extends_heritage_element,
    get_containing_function_or_class_static_block, get_effective_initializer,
    get_effective_jsdoc_host, get_effective_return_type_node, get_function_flags, get_jsdoc_host,
    get_jsdoc_tags, get_jsdoc_type_tag, get_name_of_declaration, get_source_file_of_node,
    has_effective_modifier, has_syntactic_modifier, id_text, is_binding_element,
    is_class_declaration, is_class_expression, is_function_or_module_block, is_identifier,
    is_in_js_file, is_jsdoc_augments_tag, is_named_declaration, is_private_identifier,
    is_private_identifier_class_element_declaration, node_is_missing, node_is_present, symbol_name,
    CharacterCodes, Debug_, Diagnostic, DiagnosticMessage, Diagnostics, FunctionFlags,
    HasTypeParametersInterface, IterationTypes, IterationTypesResolver, JSDocTagInterface,
    ModifierFlags, NamedDeclarationInterface, Node, NodeFlags, NodeInterface,
    SignatureDeclarationInterface, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn check_jsdoc_implements_tag(&self, node: &Node /*JSDocImplementsTag*/) {
        let node_as_jsdoc_implements_tag = node.as_jsdoc_implements_tag();
        let class_like = get_effective_jsdoc_host(node);
        if match class_like.as_ref() {
            None => true,
            Some(class_like) => {
                !is_class_declaration(class_like) && !is_class_expression(class_like)
            }
        } {
            self.error(
                class_like,
                &Diagnostics::JSDoc_0_is_not_attached_to_a_class,
                Some(vec![id_text(&node_as_jsdoc_implements_tag.tag_name())]),
            );
        }
    }

    pub(super) fn check_jsdoc_augments_tag(&self, node: &Node /*JSDocAugmentsTag*/) {
        let node_as_jsdoc_augments_tag = node.as_jsdoc_augments_tag();
        let class_like = get_effective_jsdoc_host(node);
        if match class_like.as_ref() {
            None => true,
            Some(class_like) => {
                !is_class_declaration(class_like) && !is_class_expression(class_like)
            }
        } {
            self.error(
                class_like,
                &Diagnostics::JSDoc_0_is_not_attached_to_a_class,
                Some(vec![id_text(&node_as_jsdoc_augments_tag.tag_name())]),
            );
            return;
        }
        let class_like = class_like.unwrap();

        let augments_tags = get_jsdoc_tags(&class_like)
            .into_iter()
            .filter(|jsdoc_tag| is_jsdoc_augments_tag(jsdoc_tag))
            .collect::<Vec<_>>();
        Debug_.assert(!augments_tags.is_empty(), None);
        if augments_tags.len() > 1 {
            self.error(
                Some(&*augments_tags[1]),
                &Diagnostics::Class_declarations_cannot_have_more_than_one_augments_or_extends_tag,
                None,
            );
        }

        let name = self
            .get_identifier_from_entity_name_expression(
                &node_as_jsdoc_augments_tag
                    .class
                    .as_expression_with_type_arguments()
                    .expression,
            )
            .unwrap();
        let extend = get_class_extends_heritage_element(&class_like);
        if let Some(extend) = extend.as_ref() {
            let class_name = self.get_identifier_from_entity_name_expression(
                &extend.as_expression_with_type_arguments().expression,
            );
            if let Some(class_name) = class_name.as_ref().filter(|class_name| {
                name.as_member_name().escaped_text() != class_name.as_member_name().escaped_text()
            }) {
                self.error(
                    Some(&*name),
                    &Diagnostics::JSDoc_0_1_does_not_match_the_extends_2_clause,
                    Some(vec![
                        id_text(&node_as_jsdoc_augments_tag.tag_name()),
                        id_text(&name),
                        id_text(class_name),
                    ]),
                );
            }
        }
    }

    pub(super) fn check_jsdoc_accessibility_modifiers(
        &self,
        node: &Node, /*JSDocPublicTag | JSDocProtectedTag | JSDocPrivateTag*/
    ) {
        let host = get_jsdoc_host(node);
        if matches!(
            host.as_ref(),
            Some(host) if is_private_identifier_class_element_declaration(host)
        ) {
            self.error(
                Some(node),
                &Diagnostics::An_accessibility_modifier_cannot_be_used_with_a_private_identifier,
                None,
            );
        }
    }

    pub(super) fn get_identifier_from_entity_name_expression(
        &self,
        node: &Node, /*Expression*/
    ) -> Option<Rc<Node /*Identifier | PrivateIdentifier*/>> {
        match node.kind() {
            SyntaxKind::Identifier => Some(node.node_wrapper()),
            SyntaxKind::PropertyAccessExpression => {
                Some(node.as_property_access_expression().name.clone())
            }
            _ => None,
        }
    }

    pub(super) fn check_function_or_method_declaration(
        &self,
        node: &Node, /*FunctionDeclaration | MethodDeclaration | MethodSignature*/
    ) {
        self.check_decorators(node);
        self.check_signature_declaration(node);
        let function_flags = get_function_flags(Some(node));

        let node_as_signature_declaration = node.as_signature_declaration();
        if let Some(node_name) = node_as_signature_declaration
            .maybe_name()
            .as_ref()
            .filter(|node_name| node_name.kind() == SyntaxKind::ComputedPropertyName)
        {
            self.check_computed_property_name(node_name);
        }

        if self.has_bindable_name(node) {
            let symbol = self.get_symbol_of_node(node).unwrap();
            let local_symbol = node.maybe_local_symbol().unwrap_or_else(|| symbol.clone());

            let first_declaration =
                local_symbol
                    .maybe_declarations()
                    .as_ref()
                    .and_then(|local_symbol_declarations| {
                        local_symbol_declarations
                            .iter()
                            .find(|declaration| {
                                declaration.kind() == node.kind()
                                    && !declaration.flags().intersects(NodeFlags::JavaScriptFile)
                            })
                            .cloned()
                    });

            if matches!(
                first_declaration.as_ref(),
                Some(first_declaration) if ptr::eq(node, &**first_declaration)
            ) {
                self.check_function_or_constructor_symbol(&local_symbol);
            }

            if symbol.maybe_parent().is_some() {
                self.check_function_or_constructor_symbol(&symbol);
            }
        }

        let body = if node.kind() == SyntaxKind::MethodSignature {
            None
        } else {
            node.as_function_like_declaration().maybe_body()
        };
        self.check_source_element(body.as_deref());
        self.check_all_code_paths_in_non_void_function_return_or_throw(
            node,
            self.get_return_type_from_annotation(node),
        );

        if self.produce_diagnostics && get_effective_return_type_node(node).is_none() {
            if node_is_missing(body.as_deref()) && !self.is_private_within_ambient(node) {
                self.report_implicit_any(node, &self.any_type(), None);
            }

            if function_flags.intersects(FunctionFlags::Generator)
                && node_is_present(body.as_deref())
            {
                self.get_return_type_of_signature(self.get_signature_from_declaration_(node));
            }
        }

        if is_in_js_file(Some(node)) {
            let type_tag = get_jsdoc_type_tag(node);
            if let Some(type_tag_type_expression) = type_tag
                .as_ref()
                .and_then(|type_tag| type_tag.as_jsdoc_type_like_tag().maybe_type_expression())
                .as_ref()
                .filter(|type_tag_type_expression| {
                    self.get_contextual_call_signature(
                        &self.get_type_from_type_node_(type_tag_type_expression),
                        node,
                    )
                    .is_none()
                })
            {
                self.error(
                    Some(&*type_tag_type_expression.as_jsdoc_type_expression().type_),
                    &Diagnostics::The_type_of_a_function_declaration_must_match_the_function_s_signature,
                    None,
                );
            }
        }
    }

    pub(super) fn register_for_unused_identifiers_check(
        &self,
        node: &Node, /*PotentiallyUnusedIdentifier*/
    ) {
        if self.produce_diagnostics {
            let source_file = get_source_file_of_node(Some(node)).unwrap();
            let mut all_potentially_unused_identifiers = self.all_potentially_unused_identifiers();
            let potentially_unused_identifiers = all_potentially_unused_identifiers
                .entry(source_file.as_source_file().path().clone())
                .or_insert_with(|| vec![]);
            potentially_unused_identifiers.push(node.node_wrapper());
        }
    }

    pub(super) fn check_unused_identifiers<
        TAddDiagnostic: FnMut(&Node, UnusedKind, Rc<Diagnostic>),
    >(
        &self,
        potentially_unused_identifiers: &[Rc<Node /*PotentiallyUnusedIdentifier*/>],
        mut add_diagnostic: TAddDiagnostic, /*AddUnusedDiagnostic*/
    ) {
        for node in potentially_unused_identifiers {
            match node.kind() {
                SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression => {
                    self.check_unused_class_members(node, &mut add_diagnostic);
                    self.check_unused_type_parameters(node, &mut add_diagnostic);
                }
                SyntaxKind::SourceFile
                | SyntaxKind::ModuleDeclaration
                | SyntaxKind::Block
                | SyntaxKind::CaseBlock
                | SyntaxKind::ForStatement
                | SyntaxKind::ForInStatement
                | SyntaxKind::ForOfStatement => {
                    self.check_unused_locals_and_parameters(node, &mut add_diagnostic);
                }
                SyntaxKind::Constructor
                | SyntaxKind::FunctionExpression
                | SyntaxKind::FunctionDeclaration
                | SyntaxKind::ArrowFunction
                | SyntaxKind::MethodDeclaration
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor => {
                    if node.as_function_like_declaration().maybe_body().is_some() {
                        self.check_unused_locals_and_parameters(node, &mut add_diagnostic);
                    }
                    self.check_unused_type_parameters(node, &mut add_diagnostic);
                }
                SyntaxKind::MethodSignature
                | SyntaxKind::CallSignature
                | SyntaxKind::ConstructSignature
                | SyntaxKind::FunctionType
                | SyntaxKind::ConstructorType
                | SyntaxKind::TypeAliasDeclaration
                | SyntaxKind::InterfaceDeclaration => {
                    self.check_unused_type_parameters(node, &mut add_diagnostic);
                }
                SyntaxKind::InferType => {
                    self.check_unused_infer_type_parameter(node, &mut add_diagnostic);
                }
                _ => Debug_.assert_never(
                    node,
                    Some("Node should not have been registered for unused identifiers check"),
                ),
            }
        }
    }

    pub(super) fn error_unused_local<TAddDiagnostic: FnMut(&Node, UnusedKind, Rc<Diagnostic>)>(
        &self,
        declaration: &Node, /*Declaration*/
        name: &str,
        add_diagnostic: &mut TAddDiagnostic,
    ) {
        let node = get_name_of_declaration(Some(declaration))
            .unwrap_or_else(|| declaration.node_wrapper());
        let message = if self.is_type_declaration(declaration) {
            &*Diagnostics::_0_is_declared_but_never_used
        } else {
            &*Diagnostics::_0_is_declared_but_its_value_is_never_read
        };
        add_diagnostic(
            declaration,
            UnusedKind::Local,
            Rc::new(create_diagnostic_for_node(&node, message, Some(vec![name.to_owned()])).into()),
        );
    }

    pub(super) fn is_identifier_that_starts_with_underscore(&self, node: &Node) -> bool {
        is_identifier(node) && id_text(node).chars().next() == Some(CharacterCodes::underscore)
    }

    pub(super) fn check_unused_class_members<
        TAddDiagnostic: FnMut(&Node, UnusedKind, Rc<Diagnostic>),
    >(
        &self,
        node: &Node, /*ClassDeclaration | ClassExpression*/
        add_diagnostic: &mut TAddDiagnostic,
    ) {
        for member in node.as_class_like_declaration().members() {
            match member.kind() {
                SyntaxKind::MethodDeclaration
                | SyntaxKind::PropertyDeclaration
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor => {
                    if !(member.kind() == SyntaxKind::SetAccessor
                        && member.symbol().flags().intersects(SymbolFlags::GetAccessor))
                    {
                        let symbol = self.get_symbol_of_node(member).unwrap();
                        if match symbol.maybe_is_referenced() {
                            None => true,
                            Some(symbol_is_referenced) => symbol_is_referenced == SymbolFlags::None,
                        } && (has_effective_modifier(member, ModifierFlags::Private)
                            || is_named_declaration(member)
                                && is_private_identifier(&member.as_named_declaration().name()))
                            && !member.flags().intersects(NodeFlags::Ambient)
                        {
                            add_diagnostic(
                                member,
                                UnusedKind::Local,
                                Rc::new(
                                    create_diagnostic_for_node(
                                        &member.as_named_declaration().name(),
                                        &Diagnostics::_0_is_declared_but_its_value_is_never_read,
                                        Some(vec![self.symbol_to_string_(
                                            &symbol,
                                            Option::<&Node>::None,
                                            None,
                                            None,
                                            None,
                                        )]),
                                    )
                                    .into(),
                                ),
                            );
                        }
                    }
                }
                SyntaxKind::Constructor => {
                    for parameter in member.as_constructor_declaration().parameters() {
                        if match parameter.symbol().maybe_is_referenced() {
                            None => true,
                            Some(parameter_symbol_is_referenced) => {
                                parameter_symbol_is_referenced == SymbolFlags::None
                            }
                        } && has_syntactic_modifier(parameter, ModifierFlags::Private)
                        {
                            add_diagnostic(
                                parameter,
                                UnusedKind::Local,
                                Rc::new(
                                    create_diagnostic_for_node(
                                        &parameter.as_parameter_declaration().name(),
                                        &Diagnostics::Property_0_is_declared_but_its_value_is_never_read,
                                        Some(vec![
                                            symbol_name(&parameter.symbol())
                                        ])
                                    ).into()
                                )
                            );
                        }
                    }
                }
                SyntaxKind::IndexSignature
                | SyntaxKind::SemicolonClassElement
                | SyntaxKind::ClassStaticBlockDeclaration => (),
                _ => Debug_.fail(Some("Unexpected class member")),
            }
        }
    }

    pub(super) fn check_unused_infer_type_parameter<
        TAddDiagnostic: FnMut(&Node, UnusedKind, Rc<Diagnostic>),
    >(
        &self,
        node: &Node, /*InferTypeNode*/
        add_diagnostic: &mut TAddDiagnostic,
    ) {
        unimplemented!()
    }

    pub(super) fn check_unused_type_parameters<
        TAddDiagnostic: FnMut(&Node, UnusedKind, Rc<Diagnostic>),
    >(
        &self,
        node: &Node, /*ClassLikeDeclaration | SignatureDeclaration | InterfaceDeclaration | TypeAliasDeclaration*/
        add_diagnostic: &mut TAddDiagnostic,
    ) {
        unimplemented!()
    }

    pub(super) fn check_unused_locals_and_parameters<
        TAddDiagnostic: FnMut(&Node, UnusedKind, Rc<Diagnostic>),
    >(
        &self,
        node_with_locals: &Node,
        add_diagnostic: &mut TAddDiagnostic,
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
