#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use super::{
    signature_has_rest_parameter, CheckMode, CheckTypeContainingMessageChain, IterationTypeKind,
    IterationUse, UnusedKind,
};
use crate::{
    chain_diagnostic_messages, for_each, get_containing_function,
    get_containing_function_or_class_static_block, get_effective_initializer, get_function_flags,
    has_syntactic_modifier, is_binding_element, is_binding_pattern, is_function_or_module_block,
    is_identifier, is_omitted_expression, is_private_identifier, map, maybe_for_each,
    node_is_present, Diagnostic, DiagnosticMessage, DiagnosticMessageChain, Diagnostics,
    FunctionFlags, HasTypeParametersInterface, IterationTypes, IterationTypesResolver,
    ModifierFlags, NamedDeclarationInterface, Node, NodeArray, NodeInterface, Symbol,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface, TypePredicateKind,
};

impl TypeChecker {
    pub(super) fn check_type_parameter(&self, node: &Node /*TypeParameterDeclaration*/) {
        let node_as_type_parameter_declaration = node.as_type_parameter_declaration();
        if let Some(node_expression) = node_as_type_parameter_declaration.expression.as_ref() {
            self.grammar_error_on_first_token(node_expression, &Diagnostics::Type_expected, None);
        }

        self.check_source_element(node_as_type_parameter_declaration.constraint.as_deref());
        self.check_source_element(node_as_type_parameter_declaration.default.as_deref());
        let type_parameter =
            self.get_declared_type_of_type_parameter(&self.get_symbol_of_node(node).unwrap());
        self.get_base_constraint_of_type(&type_parameter);
        if !self.has_non_circular_type_parameter_default(&type_parameter) {
            self.error(
                node_as_type_parameter_declaration.default.as_deref(),
                &Diagnostics::Type_parameter_0_has_a_circular_default,
                Some(vec![self.type_to_string_(
                    &type_parameter,
                    Option::<&Node>::None,
                    None,
                    None,
                )]),
            );
        }
        let constraint_type = self.get_constraint_of_type_parameter(&type_parameter);
        let default_type = self.get_default_from_type_parameter_(&type_parameter);
        if let (Some(constraint_type), Some(default_type)) =
            (constraint_type.as_ref(), default_type.as_ref())
        {
            self.check_type_assignable_to(
                default_type,
                &self.get_type_with_this_argument(
                    &self.instantiate_type(
                        constraint_type,
                        Some(&self.make_unary_type_mapper(&type_parameter, default_type)),
                    ),
                    Some(&**default_type),
                    None,
                ),
                node_as_type_parameter_declaration.default.as_deref(),
                Some(&Diagnostics::Type_0_does_not_satisfy_the_constraint_1),
                None,
                None,
            );
        }
        if self.produce_diagnostics {
            self.check_type_name_is_reserved(
                &node_as_type_parameter_declaration.name(),
                &Diagnostics::Type_parameter_name_cannot_be_0,
            );
        }
    }

    pub(super) fn check_parameter(&self, node: &Node /*ParameterDeclaration*/) {
        self.check_grammar_decorators_and_modifiers(node);

        self.check_variable_like_declaration(node);
        let func = get_containing_function(node).unwrap();
        let node_as_parameter_declaration = node.as_parameter_declaration();
        if has_syntactic_modifier(node, ModifierFlags::ParameterPropertyModifier) {
            if !(func.kind() == SyntaxKind::Constructor
                && node_is_present(func.as_function_like_declaration().maybe_body()))
            {
                self.error(
                    Some(node),
                    &Diagnostics::A_parameter_property_is_only_allowed_in_a_constructor_implementation,
                    None,
                );
            }
            if func.kind() == SyntaxKind::Constructor
                && is_identifier(&node_as_parameter_declaration.name())
                && node_as_parameter_declaration
                    .name()
                    .as_identifier()
                    .escaped_text
                    .eq_str("constructor")
            {
                self.error(
                    Some(node_as_parameter_declaration.name()),
                    &Diagnostics::constructor_cannot_be_used_as_a_parameter_property_name,
                    None,
                );
            }
        }
        if node_as_parameter_declaration.question_token.is_some()
            && is_binding_pattern(node_as_parameter_declaration.maybe_name())
            && func.as_function_like_declaration().maybe_body().is_some()
        {
            self.error(
                Some(node),
                &Diagnostics::A_binding_pattern_parameter_cannot_be_optional_in_an_implementation_signature,
                None,
            );
        }
        if let Some(node_name) =
            node_as_parameter_declaration
                .maybe_name()
                .as_ref()
                .filter(|node_name| {
                    is_identifier(node_name)
                        && matches!(&*node_name.as_identifier().escaped_text, "this" | "new")
                })
        {
            if func
                .as_signature_declaration()
                .parameters()
                .into_iter()
                .position(|parameter| ptr::eq(&**parameter, node))
                != Some(0)
            {
                self.error(
                    Some(node),
                    &Diagnostics::A_0_parameter_must_be_the_first_parameter,
                    Some(vec![node_name
                        .as_identifier()
                        .escaped_text
                        .clone()
                        .into_string()]),
                );
            }
            if matches!(
                func.kind(),
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
            if func.kind() == SyntaxKind::ArrowFunction {
                self.error(
                    Some(node),
                    &Diagnostics::An_arrow_function_cannot_have_a_this_parameter,
                    None,
                );
            }
            if matches!(
                func.kind(),
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
            && !is_binding_pattern(node_as_parameter_declaration.maybe_name())
            && !self.is_type_assignable_to(
                &self.get_reduced_type(&self.get_type_of_symbol(&node.symbol())),
                &self.any_readonly_array_type(),
            )
        {
            self.error(
                Some(node),
                &Diagnostics::A_rest_parameter_must_be_of_an_array_type,
                None,
            );
        }
    }

    pub(super) fn check_type_predicate(&self, node: &Node /*TypePredicateNode*/) {
        let parent = self.get_type_predicate_parent(node);
        if parent.is_none() {
            self.error(
                Some(node),
                &Diagnostics::A_type_predicate_is_only_allowed_in_return_type_position_for_functions_and_methods,
                None,
            );
            return;
        }
        let parent = parent.unwrap();

        let signature = self.get_signature_from_declaration_(&parent);
        let type_predicate = self.get_type_predicate_of_signature(&signature);
        if type_predicate.is_none() {
            return;
        }
        let type_predicate = type_predicate.unwrap();

        let node_as_type_predicate_node = node.as_type_predicate_node();
        self.check_source_element(node_as_type_predicate_node.type_.as_deref());

        let parameter_name = &node_as_type_predicate_node.parameter_name;
        if matches!(
            type_predicate.kind,
            TypePredicateKind::This | TypePredicateKind::AssertsThis
        ) {
            self.get_type_from_this_type_node(parameter_name);
        } else {
            if let Some(type_predicate_parameter_index) = type_predicate.parameter_index {
                if signature_has_rest_parameter(&signature)
                    && type_predicate_parameter_index == signature.parameters().len() - 1
                {
                    self.error(
                        Some(&**parameter_name),
                        &Diagnostics::A_type_predicate_cannot_reference_a_rest_parameter,
                        None,
                    );
                } else {
                    if let Some(type_predicate_type) = type_predicate.type_.as_ref() {
                        let leading_error: Rc<dyn CheckTypeContainingMessageChain> =
                            Rc::new(CheckTypePredicateContainingMessageChain);
                        self.check_type_assignable_to(
                            type_predicate_type,
                            &self.get_type_of_symbol(
                                &signature.parameters()[type_predicate_parameter_index],
                            ),
                            node_as_type_predicate_node.type_.as_deref(),
                            None,
                            Some(leading_error),
                            None,
                        );
                    }
                }
            } else
            /*if (parameterName)*/
            {
                let mut has_reported_error = false;
                for parameter in parent.as_signature_declaration().parameters() {
                    let name = parameter.as_named_declaration().name();
                    if is_binding_pattern(Some(&*name))
                        && self.check_if_type_predicate_variable_is_declared_in_binding_pattern(
                            &name,
                            parameter_name,
                            type_predicate.parameter_name.as_ref().unwrap(),
                        )
                    {
                        has_reported_error = true;
                        break;
                    }
                }
                if !has_reported_error {
                    self.error(
                        Some(&*node_as_type_predicate_node.parameter_name),
                        &Diagnostics::Cannot_find_parameter_0,
                        Some(vec![type_predicate.parameter_name.clone().unwrap()]),
                    );
                }
            }
        }
    }

    pub(super) fn get_type_predicate_parent(
        &self,
        node: &Node,
    ) -> Option<Rc<Node /*SignatureDeclaration*/>> {
        match node.parent().kind() {
            SyntaxKind::ArrowFunction
            | SyntaxKind::CallSignature
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::FunctionExpression
            | SyntaxKind::FunctionType
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::MethodSignature => {
                let parent = node.parent();
                if matches!(
                    parent.as_has_type().maybe_type().as_ref(),
                    Some(parent_type) if ptr::eq(node, &**parent_type)
                ) {
                    return Some(parent);
                }
            }
            _ => (),
        }
        None
    }

    pub(super) fn check_if_type_predicate_variable_is_declared_in_binding_pattern(
        &self,
        pattern: &Node, /*BindingPattern*/
        predicate_variable_node: &Node,
        predicate_variable_name: &str,
    ) -> bool {
        for element in pattern.as_has_elements().elements() {
            if is_omitted_expression(element) {
                continue;
            }

            let name = element.as_named_declaration().name();
            if name.kind() == SyntaxKind::Identifier
                && name
                    .as_identifier()
                    .escaped_text
                    .eq_str(predicate_variable_name)
            {
                self.error(
                    Some(predicate_variable_node),
                    &Diagnostics::A_type_predicate_cannot_reference_element_0_in_a_binding_pattern,
                    Some(vec![predicate_variable_name.to_owned()]),
                );
                return true;
            } else if matches!(
                name.kind(),
                SyntaxKind::ArrayBindingPattern | SyntaxKind::ObjectBindingPattern
            ) {
                if self.check_if_type_predicate_variable_is_declared_in_binding_pattern(
                    &name,
                    predicate_variable_node,
                    predicate_variable_name,
                ) {
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn check_signature_declaration(&self, node: &Node /*SignatureDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn check_property_declaration(&self, node: &Node /*PropertySignature*/) {
        self.check_variable_like_declaration(node);
    }

    pub(super) fn check_property_signature(&self, node: &Node /*PropertySignature*/) {
        if is_private_identifier(&*node.as_property_signature().name()) {
            self.error(
                Some(node.node_wrapper()),
                &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                None,
            );
        }
        self.check_property_declaration(node)
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
            0, // TODO: this is wrong
            false,
        )
        .unwrap()
    }

    pub(super) fn get_type_parameters_for_type_reference(
        &self,
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
    ) -> Option<Vec<Rc<Type>>> {
        unimplemented!()
    }

    pub(super) fn check_type_reference_node(&self, node: &Node /*TypeReferenceNode*/) {
        maybe_for_each(
            node.as_type_reference_node().type_arguments.as_ref(),
            |type_argument, _| {
                self.check_source_element(Some(&**type_argument));
                Option::<()>::None
            },
        );
        let type_ = self.get_type_from_type_reference(node);
    }

    pub(super) fn get_type_argument_constraint_(
        &self,
        node: &Node, /*TypeNode*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn check_array_type(&self, node: &Node /*ArrayTypeNode*/) {
        self.check_source_element(Some(&*node.as_array_type_node().element_type));
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
        unimplemented!()
    }

    pub(super) fn is_private_within_ambient(&self, node: &Node) -> bool {
        unimplemented!()
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

    pub(super) fn check_type_name_is_reserved(
        &self,
        name: &Node, /*Identifier*/
        message: &'static DiagnosticMessage,
    ) {
        unimplemented!()
    }

    pub(super) fn check_type_parameters(
        &self,
        type_parameter_declarations: Option<&NodeArray /*<TypeParameterDeclaration>*/>,
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
                .as_ref(),
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
                .as_ref(),
        );
        if false {
            unimplemented!()
        } else {
            self.check_source_element(Some(&*node_as_type_alias_declaration.type_));
        }
    }
}

struct CheckTypePredicateContainingMessageChain;

impl CheckTypeContainingMessageChain for CheckTypePredicateContainingMessageChain {
    fn get(&self) -> Option<Rc<RefCell<DiagnosticMessageChain>>> {
        Some(Rc::new(RefCell::new(chain_diagnostic_messages(
            None,
            &Diagnostics::A_type_predicate_s_type_must_be_assignable_to_its_parameter_s_type,
            None,
        ))))
    }
}
