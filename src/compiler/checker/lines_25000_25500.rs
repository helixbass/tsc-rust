#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use crate::{
    add_related_info, contains_rc, create_diagnostic_for_node, find_ancestor,
    for_each_child_returns, for_each_enclosing_block_scope_container, get_ancestor,
    get_assignment_declaration_kind, get_class_extends_heritage_element,
    get_enclosing_block_scope_container, get_jsdoc_this_tag, get_jsdoc_type, get_super_container,
    get_this_container, get_this_parameter, has_static_modifier, has_syntactic_modifier,
    is_assignment_target, is_binary_expression, is_call_expression, is_class_like,
    is_class_static_block_declaration, is_external_or_common_js_module, is_for_statement,
    is_function_like, is_function_like_declaration, is_identifier, is_in_js_file,
    is_iteration_statement, is_method_declaration, is_object_literal_expression,
    is_property_assignment, is_property_declaration, is_source_file, is_static, is_super_call,
    is_super_property, length, node_starts_new_lexical_environment, push_if_unique_rc,
    text_range_contains_position_inclusive, AssignmentDeclarationKind, DiagnosticMessage,
    Diagnostics, FindAncestorCallbackReturn, HasTypeInterface, InterfaceTypeInterface,
    InternalSymbolName, ModifierFlags, NamedDeclarationInterface, Node, NodeArray, NodeCheckFlags,
    NodeInterface, ReadonlyTextRange, ScriptTarget, SignatureDeclarationInterface, Symbol,
    SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeInterface,
};

impl TypeChecker {
    pub(super) fn is_inside_function_or_instance_property_initializer(
        &self,
        node: &Node,
        threshold: &Node,
    ) -> bool {
        find_ancestor(Some(node), |n: &Node| {
            if ptr::eq(n, threshold) {
                FindAncestorCallbackReturn::Quit
            } else {
                (is_function_like(Some(n))
                    || (matches!(
                        n.maybe_parent().as_ref(),
                        Some(n_parent) if is_property_declaration(n_parent) &&
                            !has_static_modifier(n_parent) && matches!(
                                n_parent.as_has_initializer().maybe_initializer().as_deref(),
                                Some(n_parent_initializer) if ptr::eq(
                                    n_parent_initializer,
                                    n
                                )
                            )
                    )))
                .into()
            }
        })
        .is_some()
    }

    pub(super) fn get_part_of_for_statement_containing_node(
        &self,
        node: &Node,
        container: &Node, /*ForStatement*/
    ) -> Option<Rc<Node>> {
        let container_as_for_statement = container.as_for_statement();
        find_ancestor(Some(node), |n: &Node| {
            if ptr::eq(n, container) {
                FindAncestorCallbackReturn::Quit
            } else {
                (matches!(
                    container_as_for_statement.initializer.as_deref(),
                    Some(container_initializer) if ptr::eq(n, container_initializer)
                ) || matches!(
                    container_as_for_statement.condition.as_deref(),
                    Some(container_condition) if ptr::eq(n, container_condition)
                ) || matches!(
                    container_as_for_statement.incrementor.as_deref(),
                    Some(container_incrementor) if ptr::eq(n, container_incrementor)
                ) || ptr::eq(n, &*container_as_for_statement.statement))
                .into()
            }
        })
    }

    pub(super) fn get_enclosing_iteration_statement(&self, node: &Node) -> Option<Rc<Node>> {
        find_ancestor(Some(node), |n: &Node| {
            if
            /* !n ||*/
            node_starts_new_lexical_environment(n) {
                FindAncestorCallbackReturn::Quit
            } else {
                is_iteration_statement(n, false).into()
            }
        })
    }

    pub(super) fn check_nested_block_scoped_binding(
        &self,
        node: &Node, /*Identifier*/
        symbol: &Symbol,
    ) {
        if self.language_version >= ScriptTarget::ES2015
            || !symbol
                .flags()
                .intersects(SymbolFlags::BlockScopedVariable | SymbolFlags::Class)
            || match symbol.maybe_value_declaration().as_deref() {
                None => true,
                Some(symbol_value_declaration) => {
                    is_source_file(symbol_value_declaration)
                        || symbol_value_declaration.parent().kind() == SyntaxKind::CatchClause
                }
            }
        {
            return;
        }
        let symbol_value_declaration = symbol.maybe_value_declaration().unwrap();

        let container = get_enclosing_block_scope_container(&symbol_value_declaration).unwrap();
        let is_captured =
            self.is_inside_function_or_instance_property_initializer(node, &container);

        let enclosing_iteration_statement = self.get_enclosing_iteration_statement(&container);
        if let Some(enclosing_iteration_statement) = enclosing_iteration_statement.as_ref() {
            if is_captured {
                let mut captures_block_scope_binding_in_loop_body = true;
                if is_for_statement(&container) {
                    let var_decl_list = get_ancestor(
                        Some(&*symbol_value_declaration),
                        SyntaxKind::VariableDeclarationList,
                    );
                    if matches!(
                        var_decl_list.as_ref(),
                        Some(var_decl_list) if Rc::ptr_eq(
                            &var_decl_list.parent(),
                            &container
                        )
                    ) {
                        let part = self
                            .get_part_of_for_statement_containing_node(&node.parent(), &container);
                        if let Some(part) = part.as_ref() {
                            let links = self.get_node_links(part);
                            links.borrow_mut().flags |=
                                NodeCheckFlags::ContainsCapturedBlockScopedBinding;

                            if (*links).borrow().captured_block_scope_bindings.is_none() {
                                links.borrow_mut().captured_block_scope_bindings = Some(vec![]);
                            }
                            {
                                let mut links = links.borrow_mut();
                                let captured_bindings =
                                    links.captured_block_scope_bindings.as_mut().unwrap();
                                push_if_unique_rc(captured_bindings, &symbol.symbol_wrapper());
                            }

                            if matches!(
                                container.as_for_statement().initializer.as_ref(),
                                Some(container_initializer) if Rc::ptr_eq(
                                    part,
                                    container_initializer
                                )
                            ) {
                                captures_block_scope_binding_in_loop_body = false;
                            }
                        }
                    }
                }
                if captures_block_scope_binding_in_loop_body {
                    self.get_node_links(enclosing_iteration_statement)
                        .borrow_mut()
                        .flags |= NodeCheckFlags::LoopWithCapturedBlockScopedBinding;
                }
            }

            if is_for_statement(&container) {
                let var_decl_list = get_ancestor(
                    Some(&*symbol_value_declaration),
                    SyntaxKind::VariableDeclarationList,
                );
                if matches!(
                    var_decl_list.as_ref(),
                    Some(var_decl_list) if Rc::ptr_eq(
                        &var_decl_list.parent(),
                        &container
                    ) && self.is_assigned_in_body_of_for_statement(node, &container)
                ) {
                    self.get_node_links(&symbol_value_declaration)
                        .borrow_mut()
                        .flags |= NodeCheckFlags::NeedsLoopOutParameter;
                }
            }

            self.get_node_links(&symbol_value_declaration)
                .borrow_mut()
                .flags |= NodeCheckFlags::BlockScopedBindingInLoop;
        }

        if is_captured {
            self.get_node_links(&symbol_value_declaration)
                .borrow_mut()
                .flags |= NodeCheckFlags::CapturedBlockScopedBinding;
        }
    }

    pub(super) fn is_binding_captured_by_node(
        &self,
        node: &Node,
        decl: &Node, /*VariableDeclaration | BindingElement*/
    ) -> bool {
        let links = self.get_node_links(node);
        /* !!links &&*/
        let ret = contains_rc(
            (*links).borrow().captured_block_scope_bindings.as_deref(),
            &self.get_symbol_of_node(decl).unwrap(),
        );
        ret
    }

    pub(super) fn is_assigned_in_body_of_for_statement(
        &self,
        node: &Node,      /*Identifier*/
        container: &Node, /*ForStatement*/
    ) -> bool {
        let mut current = node.node_wrapper();
        while current.parent().kind() == SyntaxKind::ParenthesizedExpression {
            current = current.parent();
        }

        let mut is_assigned = false;
        if is_assignment_target(&current) {
            is_assigned = true;
        } else if matches!(
            current.parent().kind(),
            SyntaxKind::PrefixUnaryExpression | SyntaxKind::PostfixUnaryExpression
        ) {
            let expr = current.parent();
            is_assigned = matches!(
                expr.as_unary_expression().operator(),
                SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
            );
        }

        if !is_assigned {
            return false;
        }

        let container_as_for_statement = container.as_for_statement();
        find_ancestor(Some(current), |n: &Node| {
            if ptr::eq(n, container) {
                FindAncestorCallbackReturn::Quit
            } else {
                ptr::eq(n, &*container_as_for_statement.statement).into()
            }
        })
        .is_some()
    }

    pub(super) fn capture_lexical_this(&self, node: &Node, container: &Node) {
        self.get_node_links(node).borrow_mut().flags |= NodeCheckFlags::LexicalThis;
        if matches!(
            container.kind(),
            SyntaxKind::PropertyDeclaration | SyntaxKind::Constructor
        ) {
            let class_node = container.parent();
            self.get_node_links(&class_node).borrow_mut().flags |= NodeCheckFlags::CaptureThis;
        } else {
            self.get_node_links(container).borrow_mut().flags |= NodeCheckFlags::CaptureThis;
        }
    }

    pub(super) fn find_first_super_call(&self, node: &Node) -> Option<Rc<Node /*SuperCall*/>> {
        if is_super_call(node) {
            Some(node.node_wrapper())
        } else if is_function_like(Some(node)) {
            None
        } else {
            for_each_child_returns(
                node,
                |node: &Node| self.find_first_super_call(node),
                Option::<fn(&NodeArray) -> Option<Rc<Node>>>::None,
            )
        }
    }

    pub(super) fn class_declaration_extends_null(
        &self,
        class_decl: &Node, /*ClassDeclaration*/
    ) -> bool {
        let class_symbol = self.get_symbol_of_node(class_decl).unwrap();
        let class_instance_type = self.get_declared_type_of_symbol(&class_symbol);
        let base_constructor_type = self.get_base_constructor_type_of_class(&class_instance_type);

        Rc::ptr_eq(&base_constructor_type, &self.null_widening_type())
    }

    pub(super) fn check_this_before_super(
        &self,
        node: &Node,
        container: &Node,
        diagnostic_message: &DiagnosticMessage,
    ) {
        let containing_class_decl = container.parent();
        let base_type_node = get_class_extends_heritage_element(&containing_class_decl);

        if let Some(base_type_node) = base_type_node.as_ref() {
            if !self.class_declaration_extends_null(&containing_class_decl) {
                if matches!(
                    node.maybe_flow_node().as_ref(),
                    Some(node_flow_node) if !self.is_post_super_flow_node(node_flow_node.clone(), false)
                ) {
                    self.error(Some(node), diagnostic_message, None);
                }
            }
        }
    }

    pub(super) fn check_this_in_static_class_field_initializer_in_decorated_class(
        &self,
        this_expression: &Node,
        container: &Node,
    ) {
        if is_property_declaration(container)
            && has_static_modifier(container)
            && matches!(
                container.as_has_initializer().maybe_initializer().as_deref(),
                Some(container_initializer) if text_range_contains_position_inclusive(container_initializer, this_expression.pos())
            )
            && length(container.parent().maybe_decorators().as_deref()) > 0
        {
            self.error(
                Some(this_expression),
                &Diagnostics::Cannot_use_this_in_a_static_property_initializer_of_a_decorated_class,
                None,
            );
        }
    }

    pub(super) fn check_this_expression(&self, node: &Node) -> Rc<Type> {
        let is_node_in_type_query = self.is_in_type_query(node);
        let mut container = get_this_container(node, true);
        let mut captured_by_arrow_function = false;

        if container.kind() == SyntaxKind::Constructor {
            self.check_this_before_super(node, &container, &Diagnostics::super_must_be_called_before_accessing_this_in_the_constructor_of_a_derived_class);
        }

        if container.kind() == SyntaxKind::ArrowFunction {
            container = get_this_container(&container, false);
            captured_by_arrow_function = true;
        }

        self.check_this_in_static_class_field_initializer_in_decorated_class(node, &container);
        match container.kind() {
            SyntaxKind::ModuleDeclaration => {
                self.error(
                    Some(node),
                    &Diagnostics::this_cannot_be_referenced_in_a_module_or_namespace_body,
                    None,
                );
            }
            SyntaxKind::EnumDeclaration => {
                self.error(
                    Some(node),
                    &Diagnostics::this_cannot_be_referenced_in_current_location,
                    None,
                );
            }
            SyntaxKind::Constructor => {
                if self.is_in_constructor_argument_initializer(node, &container) {
                    self.error(
                        Some(node),
                        &Diagnostics::this_cannot_be_referenced_in_constructor_arguments,
                        None,
                    );
                }
            }
            SyntaxKind::ComputedPropertyName => {
                self.error(
                    Some(node),
                    &Diagnostics::this_cannot_be_referenced_in_a_computed_property_name,
                    None,
                );
            }
            _ => (),
        }

        if !is_node_in_type_query
            && captured_by_arrow_function
            && self.language_version < ScriptTarget::ES2015
        {
            self.capture_lexical_this(node, &container);
        }

        let type_ = self.try_get_this_type_at_(node, Some(true), Some(&*container));
        if self.no_implicit_this {
            let global_this_type = self.get_type_of_symbol(&self.global_this_symbol());
            if matches!(
                type_.as_ref(),
                Some(type_) if Rc::ptr_eq(
                    type_,
                    &global_this_type
                )
            ) && captured_by_arrow_function
            {
                self.error(
                    Some(node),
                    &Diagnostics::The_containing_arrow_function_captures_the_global_value_of_this,
                    None,
                );
            } else if type_.is_none() {
                let diag = self.error(
                    Some(node),
                    &Diagnostics::this_implicitly_has_type_any_because_it_does_not_have_a_type_annotation,
                    None,
                );
                if !is_source_file(&container) {
                    let outside_this =
                        self.try_get_this_type_at_(&container, None, Option::<&Node>::None);
                    if matches!(
                        outside_this.as_ref(),
                        Some(outside_this) if !Rc::ptr_eq(
                            outside_this,
                            &self.global_this_type()
                        )
                    ) {
                        add_related_info(
                            &diag,
                            vec![
                                Rc::new(
                                    create_diagnostic_for_node(
                                        &container,
                                        &Diagnostics::An_outer_value_of_this_is_shadowed_by_this_container,
                                        None,
                                    ).into()
                                )
                            ]
                        );
                    }
                }
            }
        }
        type_.unwrap_or_else(|| self.any_type())
    }

    pub(super) fn try_get_this_type_at_<TContainer: Borrow<Node>>(
        &self,
        node: &Node,
        include_global_this: Option<bool>,
        container: Option<TContainer>,
    ) -> Option<Rc<Type>> {
        let include_global_this = include_global_this.unwrap_or(true);
        let container = container.map_or_else(
            || get_this_container(node, false),
            |container| container.borrow().node_wrapper(),
        );
        let is_in_js = is_in_js_file(Some(node));
        if is_function_like(Some(&*container))
            && (!self.is_in_parameter_initializer_before_containing_function(node)
                || get_this_parameter(&container).is_some())
        {
            let mut this_type = self.get_this_type_of_declaration(&container).or_else(|| {
                if is_in_js {
                    self.get_type_for_this_expression_from_jsdoc(&container)
                } else {
                    None
                }
            });
            if this_type.is_none() {
                let class_name = self.get_class_name_from_prototype_method(&container);
                if let Some(class_name) = class_name.as_ref().filter(|_| is_in_js) {
                    let class_symbol = self.check_expression(class_name, None, None).maybe_symbol();
                    if let Some(class_symbol) = class_symbol.as_ref().filter(|class_symbol| {
                        class_symbol.maybe_members().is_some()
                            && class_symbol.flags().intersects(SymbolFlags::Function)
                    }) {
                        this_type = self
                            .get_declared_type_of_symbol(class_symbol)
                            .as_interface_type()
                            .maybe_this_type();
                    }
                } else if self.is_js_constructor(Some(&*container)) {
                    this_type = self
                        .get_declared_type_of_symbol(
                            &self.get_merged_symbol(Some(container.symbol())).unwrap(),
                        )
                        .as_interface_type()
                        .maybe_this_type();
                }
                if this_type.is_none() {
                    this_type = self.get_contextual_this_parameter_type(&container);
                }
            }

            if let Some(this_type) = this_type.as_ref() {
                return Some(self.get_flow_type_of_reference(
                    node,
                    this_type,
                    Option::<&Type>::None,
                    Option::<&Node>::None,
                ));
            }
        }

        if is_class_like(&container.parent()) {
            let symbol = self.get_symbol_of_node(&container.parent()).unwrap();
            let type_ = if is_static(&container) {
                self.get_type_of_symbol(&symbol)
            } else {
                self.get_declared_type_of_symbol(&symbol)
                    .as_interface_type()
                    .maybe_this_type()
                    .unwrap()
            };
            return Some(self.get_flow_type_of_reference(
                node,
                &type_,
                Option::<&Type>::None,
                Option::<&Node>::None,
            ));
        }

        if is_source_file(&container) {
            let container_as_source_file = container.as_source_file();
            if container_as_source_file
                .maybe_common_js_module_indicator()
                .is_some()
            {
                let file_symbol = self.get_symbol_of_node(&container);
                return file_symbol
                    .as_ref()
                    .map(|file_symbol| self.get_type_of_symbol(file_symbol));
            } else if container_as_source_file
                .maybe_external_module_indicator()
                .is_some()
            {
                return Some(self.undefined_type());
            } else if include_global_this {
                return Some(self.get_type_of_symbol(&self.global_this_symbol()));
            }
        }
        None
    }

    pub(super) fn get_explicit_this_type(
        &self,
        node: &Node, /*Expression*/
    ) -> Option<Rc<Type>> {
        let container = get_this_container(node, false);
        if is_function_like(Some(&*container)) {
            let signature = self.get_signature_from_declaration_(&container);
            if let Some(signature_this_parameter) = signature.this_parameter.as_ref() {
                return self.get_explicit_type_of_symbol(signature_this_parameter, None);
            }
        }
        if is_class_like(&container.parent()) {
            let symbol = self.get_symbol_of_node(&container.parent()).unwrap();
            return if is_static(&container) {
                Some(self.get_type_of_symbol(&symbol))
            } else {
                self.get_declared_type_of_symbol(&symbol)
                    .as_interface_type()
                    .maybe_this_type()
            };
        }
        None
    }

    pub(super) fn get_class_name_from_prototype_method(
        &self,
        container: &Node,
    ) -> Option<Rc<Node>> {
        if container.kind() == SyntaxKind::FunctionExpression
            && is_binary_expression(&container.parent())
            && get_assignment_declaration_kind(&container.parent())
                == AssignmentDeclarationKind::PrototypeProperty
        {
            return Some(
                container
                    .parent()
                    .as_binary_expression()
                    .left
                    .as_property_access_expression()
                    .expression
                    .as_property_access_expression()
                    .expression
                    .clone(),
            );
        } else if container.kind() == SyntaxKind::MethodDeclaration
            && container.parent().kind() == SyntaxKind::ObjectLiteralExpression
            && is_binary_expression(&container.parent().parent())
            && get_assignment_declaration_kind(&container.parent().parent())
                == AssignmentDeclarationKind::Prototype
        {
            return Some(
                container
                    .parent()
                    .parent()
                    .as_binary_expression()
                    .left
                    .as_property_access_expression()
                    .expression
                    .clone(),
            );
        } else if container.kind() == SyntaxKind::FunctionExpression
            && container.parent().kind() == SyntaxKind::PropertyAssignment
            && container.parent().parent().kind() == SyntaxKind::ObjectLiteralExpression
            && is_binary_expression(&container.parent().parent().parent())
            && get_assignment_declaration_kind(&container.parent().parent().parent())
                == AssignmentDeclarationKind::Prototype
        {
            return Some(
                container
                    .parent()
                    .parent()
                    .parent()
                    .as_binary_expression()
                    .left
                    .as_property_access_expression()
                    .expression
                    .clone(),
            );
        } else if container.kind() == SyntaxKind::FunctionExpression
            && {
                let container_parent = container.parent();
                is_property_assignment(&container_parent)
                    && {
                        let container_parent_as_property_assignment =
                            container_parent.as_property_assignment();
                        is_identifier(&container_parent_as_property_assignment.name())
                            && matches!(
                                &*container_parent_as_property_assignment
                                    .name()
                                    .as_identifier()
                                    .escaped_text,
                                "value" | "get" | "set"
                            )
                            && {
                                let container_parent_parent = container_parent.parent();
                                is_object_literal_expression(&container_parent_parent)
                                    && {
                                        let container_parent_parent_parent =
                                            container_parent_parent.parent();
                                        is_call_expression(&container_parent_parent_parent) &&
                            matches!(
                                container_parent_parent_parent.as_call_expression().arguments.get(2),
                                Some(argument) if Rc::ptr_eq(
                                    argument,
                                    &container_parent_parent
                                )
                            ) &&
                            get_assignment_declaration_kind(&container_parent_parent_parent) == AssignmentDeclarationKind::ObjectDefinePrototypeProperty
                                    }
                            }
                    }
            }
        {
            return Some(
                container
                    .parent()
                    .parent()
                    .parent()
                    .as_call_expression()
                    .arguments[0]
                    .as_property_access_expression()
                    .expression
                    .clone(),
            );
        } else if is_method_declaration(&container) && {
            let container_as_method_declaration = container.as_method_declaration();
            is_identifier(&container_as_method_declaration.name())
                && matches!(
                    &*container_as_method_declaration
                        .name()
                        .as_identifier()
                        .escaped_text,
                    "value" | "get" | "set"
                )
                && {
                    let container_parent = container.parent();
                    is_object_literal_expression(&container_parent) && {
                        let container_parent_parent = container_parent.parent();
                        is_call_expression(&container_parent_parent)
                            && matches!(
                                container_parent_parent.as_call_expression().arguments.get(2),
                                Some(argument) if Rc::ptr_eq(
                                    argument,
                                    &container_parent
                                )
                            )
                            && get_assignment_declaration_kind(&container_parent_parent)
                                == AssignmentDeclarationKind::ObjectDefinePrototypeProperty
                    }
                }
        } {
            return Some(
                container.parent().parent().as_call_expression().arguments[0]
                    .as_property_access_expression()
                    .expression
                    .clone(),
            );
        }
        None
    }

    pub(super) fn get_type_for_this_expression_from_jsdoc(&self, node: &Node) -> Option<Rc<Type>> {
        let jsdoc_type = get_jsdoc_type(node);
        if let Some(jsdoc_type) = jsdoc_type
            .as_ref()
            .filter(|jsdoc_type| jsdoc_type.kind() == SyntaxKind::JSDocFunctionType)
        {
            let js_doc_function_type = jsdoc_type.as_jsdoc_function_type();
            let js_doc_function_type_parameters = js_doc_function_type.parameters();
            if !js_doc_function_type_parameters.is_empty()
                && matches!(
                    js_doc_function_type_parameters[0].as_parameter_declaration().maybe_name(),
                    Some(name) if name.as_identifier().escaped_text == InternalSymbolName::This()
                )
            {
                return Some(
                    self.get_type_from_type_node_(
                        &js_doc_function_type_parameters[0]
                            .as_parameter_declaration()
                            .maybe_type()
                            .unwrap(),
                    ),
                );
            }
        }
        let this_tag = get_jsdoc_this_tag(node);
        this_tag
            .and_then(|this_tag| {
                this_tag
                    .as_base_jsdoc_type_like_tag()
                    .type_expression
                    .clone()
            })
            .as_ref()
            .map(|this_tag_type_expression| self.get_type_from_type_node_(this_tag_type_expression))
    }

    pub(super) fn is_in_constructor_argument_initializer(
        &self,
        node: &Node,
        constructor_decl: &Node,
    ) -> bool {
        find_ancestor(Some(node), |n: &Node| {
            if is_function_like_declaration(n) {
                FindAncestorCallbackReturn::Quit
            } else {
                (n.kind() == SyntaxKind::Parameter && ptr::eq(&*n.parent(), constructor_decl))
                    .into()
            }
        })
        .is_some()
    }

    pub(super) fn check_super_expression(&self, node: &Node) -> Rc<Type> {
        let is_call_expression = node.parent().kind() == SyntaxKind::CallExpression
            && ptr::eq(&*node.parent().as_call_expression().expression, node);

        let immediate_container = get_super_container(node, true).unwrap();
        let mut container = Some(immediate_container.clone());
        let mut need_to_capture_lexical_this = false;

        if !is_call_expression {
            while let Some(container_present) = container
                .as_ref()
                .filter(|container| container.kind() == SyntaxKind::ArrowFunction)
            {
                container = get_super_container(container_present, true);
                need_to_capture_lexical_this = self.language_version < ScriptTarget::ES2015;
            }
        }

        let can_use_super_expression =
            self.is_legal_usage_of_super_expression(is_call_expression, container.as_deref());
        let mut node_check_flag = NodeCheckFlags::None;

        if !can_use_super_expression {
            let current = find_ancestor(Some(node), |n: &Node| {
                if matches!(
                    container.as_deref(),
                    Some(container) if ptr::eq(n, container)
                ) {
                    FindAncestorCallbackReturn::Quit
                } else {
                    (n.kind() == SyntaxKind::ComputedPropertyName).into()
                }
            });
            if matches!(
                current.as_ref(),
                Some(current) if current.kind() == SyntaxKind::ComputedPropertyName
            ) {
                self.error(
                    Some(node),
                    &Diagnostics::super_cannot_be_referenced_in_a_computed_property_name,
                    None,
                );
            } else if is_call_expression {
                self.error(
                    Some(node),
                    &Diagnostics::Super_calls_are_not_permitted_outside_constructors_or_in_nested_functions_inside_constructors,
                    None,
                );
            } else if match container.as_ref() {
                None => true,
                Some(container) => match container.maybe_parent().as_ref() {
                    None => true,
                    Some(container_parent) => {
                        !(is_class_like(container_parent)
                            || container_parent.kind() == SyntaxKind::ObjectLiteralExpression)
                    }
                },
            } {
                self.error(
                    Some(node),
                    &Diagnostics::super_can_only_be_referenced_in_members_of_derived_classes_or_object_literal_expressions,
                    None,
                );
            } else {
                self.error(
                    Some(node),
                    &Diagnostics::super_property_access_is_permitted_only_in_a_constructor_member_function_or_member_accessor_of_a_derived_class,
                    None,
                );
            }
            return self.error_type();
        }

        if !is_call_expression && immediate_container.kind() == SyntaxKind::Constructor {
            self.check_this_before_super(
                node, container.as_ref().unwrap(),
                &Diagnostics::super_must_be_called_before_accessing_a_property_of_super_in_the_constructor_of_a_derived_class
            );
        }

        if is_static(container.as_ref().unwrap()) || is_call_expression {
            node_check_flag = NodeCheckFlags::SuperStatic;
            if !is_call_expression
                && self.language_version >= ScriptTarget::ES2015
                && self.language_version <= ScriptTarget::ES2021
                && (is_property_declaration(container.as_ref().unwrap())
                    || is_class_static_block_declaration(container.as_ref().unwrap()))
            {
                for_each_enclosing_block_scope_container(&node.parent(), |current: &Node| {
                    if !is_source_file(current) || is_external_or_common_js_module(current) {
                        self.get_node_links(current).borrow_mut().flags |=
                            NodeCheckFlags::ContainsSuperPropertyInStaticInitializer;
                    }
                });
            }
        } else {
            node_check_flag = NodeCheckFlags::SuperInstance;
        }

        self.get_node_links(node).borrow_mut().flags |= node_check_flag;

        let container = container.unwrap();
        if container.kind() == SyntaxKind::MethodDeclaration
            && has_syntactic_modifier(&container, ModifierFlags::Async)
        {
            if is_super_property(&node.parent()) && is_assignment_target(&node.parent()) {
                self.get_node_links(&container).borrow_mut().flags |=
                    NodeCheckFlags::AsyncMethodWithSuperBinding;
            } else {
                self.get_node_links(&container).borrow_mut().flags |=
                    NodeCheckFlags::AsyncMethodWithSuper;
            }
        }

        if need_to_capture_lexical_this {
            self.capture_lexical_this(&node.parent(), &container);
        }

        if container.parent().kind() == SyntaxKind::ObjectLiteralExpression {
            if self.language_version < ScriptTarget::ES2015 {
                self.error(
                    Some(node),
                    &Diagnostics::super_is_only_allowed_in_members_of_object_literal_expressions_when_option_target_is_ES2015_or_higher,
                    None,
                );
                return self.error_type();
            } else {
                return self.any_type();
            }
        }

        let class_like_declaration = container.parent();
        if get_class_extends_heritage_element(&class_like_declaration).is_none() {
            self.error(
                Some(node),
                &Diagnostics::super_can_only_be_referenced_in_a_derived_class,
                None,
            );
            return self.error_type();
        }

        let class_type = self.get_declared_type_of_symbol(
            &self.get_symbol_of_node(&class_like_declaration).unwrap(),
        );
        let base_class_type = /*classType &&*/
            self.get_base_types(&class_type).get(0).cloned();
        if base_class_type.is_none() {
            return self.error_type();
        }
        let base_class_type = base_class_type.unwrap();

        if container.kind() == SyntaxKind::Constructor
            && self.is_in_constructor_argument_initializer(node, &container)
        {
            self.error(
                Some(node),
                &Diagnostics::super_cannot_be_referenced_in_constructor_arguments,
                None,
            );
            return self.error_type();
        }

        if node_check_flag == NodeCheckFlags::SuperStatic {
            self.get_base_constructor_type_of_class(&class_type)
        } else {
            self.get_type_with_this_argument(
                &base_class_type,
                class_type.as_interface_type().maybe_this_type(),
                None,
            )
        }
    }
}
