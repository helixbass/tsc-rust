#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, IterationTypeKind, IterationUse, UnusedKind};
use crate::{
    for_each, for_each_child, get_class_extends_heritage_element,
    get_containing_function_or_class_static_block, get_declaration_of_kind,
    get_effective_initializer, get_effective_modifier_flags, get_emit_script_target,
    get_function_flags, has_syntactic_modifier, is_binding_element, is_function_or_module_block,
    is_in_js_file, is_private_identifier_class_element_declaration, is_prologue_directive,
    is_static, is_super_call, map, maybe_for_each, node_is_missing, node_is_present, some,
    Diagnostic, DiagnosticMessage, Diagnostics, FunctionFlags, FunctionLikeDeclarationInterface,
    HasInitializerInterface, HasTypeParametersInterface, IterationTypes, IterationTypesResolver,
    ModifierFlags, Node, NodeArray, NodeCheckFlags, NodeFlags, NodeInterface, ScriptTarget,
    SignatureDeclarationInterface, Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker,
    TypeFlags, TypeInterface,
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

    pub(super) fn check_function_or_constructor_symbol(&self, symbol: &Symbol) {
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
