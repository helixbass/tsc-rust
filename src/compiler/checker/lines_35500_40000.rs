#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use super::{
    CheckMode, IterationTypeKind, IterationUse, ResolveCallContainingMessageChain, UnusedKind,
};
use crate::{
    chain_diagnostic_messages, entity_name_to_string, for_each,
    get_containing_function_or_class_static_block, get_effective_initializer,
    get_effective_type_annotation_node, get_entity_name_from_type_node, get_first_identifier,
    get_function_flags, get_rest_parameter_element_type, id_text, is_binding_element,
    is_entity_name, is_function_or_module_block, is_identifier, is_rest_parameter, Debug_,
    Diagnostic, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, FunctionFlags,
    HasTypeParametersInterface, IterationTypes, IterationTypesResolver, Node, NodeInterface,
    ScriptTarget, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags,
    TypeInterface,
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
