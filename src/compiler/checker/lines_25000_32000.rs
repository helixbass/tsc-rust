#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use super::{
    signature_has_rest_parameter, CheckMode, MinArgumentCountFlags, ResolveNameNameArg, TypeFacts,
    WideningKind,
};
use crate::{
    add_related_info, contains_rc, create_diagnostic_for_node, filter, find_ancestor,
    for_each_child_returns, for_each_enclosing_block_scope_container, get_ancestor,
    get_assignment_declaration_kind, get_class_extends_heritage_element,
    get_enclosing_block_scope_container, get_jsdoc_this_tag, get_jsdoc_type, get_super_container,
    get_this_container, get_this_parameter, has_static_modifier, has_syntactic_modifier,
    is_assignment_target, is_binary_expression, is_call_expression, is_class_like,
    is_class_static_block_declaration, is_external_or_common_js_module, is_for_statement,
    is_function_expression_or_arrow_function, is_function_like, is_function_like_declaration,
    is_identifier, is_import_call, is_in_js_file, is_iteration_statement, is_method_declaration,
    is_object_literal_method, is_property_assignment, is_property_declaration, is_source_file,
    is_static, is_super_call, is_super_property, length, node_starts_new_lexical_environment,
    push_if_unique_rc, text_range_contains_position_inclusive, AssignmentDeclarationKind,
    ContextFlags, Debug_, DiagnosticMessage, Diagnostics, FindAncestorCallbackReturn,
    FunctionFlags, HasTypeInterface, InterfaceTypeInterface, InternalSymbolName, ModifierFlags,
    NamedDeclarationInterface, NodeArray, NodeCheckFlags, NodeFlags, ReadonlyTextRange,
    ScriptTarget, Signature, SignatureDeclarationInterface, SignatureFlags, SignatureKind,
    StringOrRcNode, SymbolFlags, Ternary, UnionReduction, __String, create_symbol_table,
    get_effective_type_annotation_node, get_function_flags, get_object_flags, has_initializer,
    is_object_literal_expression, HasInitializerInterface, InferenceContext, Node, NodeInterface,
    ObjectFlags, ObjectFlagsTypeInterface, Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker,
    TypeFlags, TypeInterface,
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

    pub(super) fn is_legal_usage_of_super_expression<TContainer: Borrow<Node>>(
        &self,
        is_call_expression: bool,
        container: Option<TContainer>,
    ) -> bool {
        if container.is_none() {
            return false;
        }
        let container = container.unwrap();
        let container = container.borrow();

        if is_call_expression {
            return container.kind() == SyntaxKind::Constructor;
        } else {
            if is_class_like(&container.parent())
                || container.parent().kind() == SyntaxKind::ObjectLiteralExpression
            {
                if is_static(container) {
                    return matches!(
                        container.kind(),
                        SyntaxKind::MethodDeclaration
                            | SyntaxKind::MethodSignature
                            | SyntaxKind::GetAccessor
                            | SyntaxKind::SetAccessor
                            | SyntaxKind::PropertyDeclaration
                            | SyntaxKind::ClassStaticBlockDeclaration
                    );
                } else {
                    return matches!(
                        container.kind(),
                        SyntaxKind::MethodDeclaration
                            | SyntaxKind::MethodSignature
                            | SyntaxKind::GetAccessor
                            | SyntaxKind::SetAccessor
                            | SyntaxKind::PropertyDeclaration
                            | SyntaxKind::PropertySignature
                            | SyntaxKind::Constructor
                    );
                }
            }
        }

        false
    }

    pub(super) fn get_contextual_this_parameter_type(
        &self,
        func: &Node, /*SignatureDeclaration*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_contextually_typed_parameter_type(
        &self,
        parameter: &Node, /*ParameterDeclaration*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_contextual_type_for_variable_like_declaration(
        &self,
        declaration: &Node,
    ) -> Option<Rc<Type>> {
        let type_node = get_effective_type_annotation_node(declaration);
        if let Some(type_node) = type_node {
            return Some(self.get_type_from_type_node_(&*type_node));
        }
        match declaration.kind() {
            _ => None,
        }
    }

    pub(super) fn get_contextual_type_for_initializer_expression(
        &self,
        node: &Node,
    ) -> Option<Rc<Type>> {
        let parent = node.parent();
        let parent_as_variable_declaration = parent.as_variable_declaration();
        if has_initializer(&parent)
            && ptr::eq(
                node,
                &*parent_as_variable_declaration.maybe_initializer().unwrap(),
            )
        {
            let result = self.get_contextual_type_for_variable_like_declaration(&*parent);
            if result.is_some() {
                return result;
            }
        }
        None
    }

    pub(super) fn is_in_parameter_initializer_before_containing_function(
        &self,
        node: &Node,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_contextual_type_for_argument_at_index_(
        &self,
        call_target: &Node, /*CallLikeExpression*/
        arg_index: usize,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_possibly_aliased_this_property(
        &self,
        declaration: &Node, /*BinaryExpression*/
        kind: Option<AssignmentDeclarationKind>,
    ) -> bool {
        let kind = kind.unwrap_or_else(|| get_assignment_declaration_kind(declaration));
        unimplemented!()
    }

    pub(super) fn get_type_of_property_of_contextual_type(
        &self,
        type_: &Type,
        name: &__String,
    ) -> Option<Rc<Type>> {
        self.map_type(
            type_,
            &mut |t| {
                if false {
                    unimplemented!()
                } else if t.flags().intersects(TypeFlags::StructuredType) {
                    let prop = self.get_property_of_type_(t, name, None);
                    if let Some(prop) = prop {
                        return if false {
                            None
                        } else {
                            Some(self.get_type_of_symbol(&*prop))
                        };
                    }
                    return if let Some(found) = Option::<()>::None /*self.find_applicable_index_info(self.get_index_infos_of_structured_type(t), self.get_string_literal_type(unescape_leading_underscores(name)))*/ {
                        unimplemented!()
                    } else {
                        None
                    };
                }
                None
            },
            Some(true),
        )
    }

    pub(super) fn get_contextual_type_for_object_literal_element_(
        &self,
        element: &Node, /*PropertyAssignment*/
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let object_literal = element.parent();
        // let property_assignment_type = if is_property_assignment(element) {
        // } else {
        //     None
        // };
        // if property_assignment_type.is_some() {
        //     return property_assignment_type;
        // }
        let type_ = self.get_apparent_type_of_contextual_type(&object_literal, context_flags);
        if let Some(type_) = type_ {
            if self.has_bindable_name(element) {
                return self.get_type_of_property_of_contextual_type(
                    &type_,
                    self.get_symbol_of_node(element).unwrap().escaped_name(),
                );
            }
            unimplemented!()
        }
        None
    }

    pub(super) fn get_contextual_type_for_jsx_attribute_(
        &self,
        attribute: &Node, /*JsxAttribute | JsxSpreadAttribute*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn is_possibly_discriminant_value(&self, node: &Node /*Expression*/) -> bool {
        unimplemented!()
    }

    pub(super) fn get_apparent_type_of_contextual_type(
        &self,
        node: &Node, /*Expression | MethodDeclaration*/
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let contextual_type = if false {
            unimplemented!()
        } else {
            self.get_contextual_type_(node, context_flags)
        };
        let instantiated_type =
            self.instantiate_contextual_type(contextual_type, node, context_flags);
        if let Some(instantiated_type) = instantiated_type {
            if !(matches!(context_flags, Some(context_flags) if context_flags.intersects(ContextFlags::NoConstraints))
                && instantiated_type
                    .flags()
                    .intersects(TypeFlags::TypeVariable))
            {
                let apparent_type = self
                    .map_type(
                        &instantiated_type,
                        &mut |type_| Some(self.get_apparent_type(type_)),
                        Some(true),
                    )
                    .unwrap();
                return if apparent_type.flags().intersects(TypeFlags::Union)
                    && is_object_literal_expression(node)
                {
                    unimplemented!()
                } else if false {
                    unimplemented!()
                } else {
                    Some(apparent_type)
                };
            }
        }
        None
    }

    pub(super) fn instantiate_contextual_type<TTypeRef: Borrow<Type>, TNode: NodeInterface>(
        &self,
        contextual_type: Option<TTypeRef>,
        node: &TNode,
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        if false {
            unimplemented!()
        }
        contextual_type.map(|contextual_type| contextual_type.borrow().type_wrapper())
    }

    pub(super) fn get_contextual_type_(
        &self,
        node: &Node, /*Expression*/
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let parent = node.parent();
        match &*parent {
            Node::VariableDeclaration(_) => {
                self.get_contextual_type_for_initializer_expression(node)
            }
            Node::PropertyAssignment(_) => {
                self.get_contextual_type_for_object_literal_element_(node, context_flags)
            }
            _ => unimplemented!(),
        }
    }

    pub(super) fn get_intersected_signatures(
        &self,
        signatures: &[Rc<Signature>],
    ) -> Option<Rc<Signature>> {
        unimplemented!()
    }

    pub(super) fn get_contextual_call_signature(
        &self,
        type_: &Type,
        node: &Node, /*SignatureDeclaration*/
    ) -> Option<Rc<Signature>> {
        let signatures = self.get_signatures_of_type(type_, SignatureKind::Call);
        let applicable_by_arity = filter(&signatures, |s| !self.is_arity_smaller(s, node));
        if applicable_by_arity.len() == 1 {
            Some(applicable_by_arity[0].clone())
        } else {
            self.get_intersected_signatures(&applicable_by_arity)
        }
    }

    pub(super) fn is_arity_smaller(
        &self,
        signature: &Signature,
        target: &Node, /*SignatureDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_contextual_signature_for_function_like_declaration(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> Option<Rc<Signature>> {
        if is_function_expression_or_arrow_function(node) || is_object_literal_method(node) {
            self.get_contextual_signature(node)
        } else {
            None
        }
    }

    pub(super) fn get_contextual_signature(
        &self,
        node: &Node, /*FunctionExpression | ArrowFunction | MethodDeclaration*/
    ) -> Option<Rc<Signature>> {
        Debug_.assert(
            node.kind() != SyntaxKind::MethodDeclaration || is_object_literal_method(node),
            None,
        );
        let type_tag_signature = self.get_signature_of_type_tag(node);
        if type_tag_signature.is_some() {
            return type_tag_signature;
        }
        let type_ = self.get_apparent_type_of_contextual_type(node, Some(ContextFlags::Signature));
        if type_.is_none() {
            return None;
        }
        let type_ = type_.unwrap();
        if !type_.flags().intersects(TypeFlags::Union) {
            return self.get_contextual_call_signature(&type_, node);
        }
        let mut signature_list: Option<Vec<Rc<Signature>>> = None;
        let types = type_.as_union_or_intersection_type_interface().types();
        for current in types {
            let signature = self.get_contextual_call_signature(current, node);
            if let Some(signature) = signature {
                match signature_list.as_mut() {
                    None => {
                        signature_list = Some(vec![signature]);
                    }
                    Some(signature_list) => {
                        if self.compare_signatures_identical(
                            signature_list[0].clone(),
                            signature.clone(),
                            false,
                            true,
                            true,
                            |a, b| self.compare_types_identical(a, b),
                        ) == Ternary::False
                        {
                            return None;
                        } else {
                            signature_list.push(signature);
                        }
                    }
                }
            }
        }

        signature_list.map(|signature_list| {
            if signature_list.len() == 1 {
                signature_list[0].clone()
            } else {
                Rc::new(self.create_union_signature(&signature_list[0].clone(), signature_list))
            }
        })
    }

    pub(super) fn has_default_value(
        &self,
        node: &Node, /*BindingElement | Expression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_array_literal(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
        check_mode: Option<CheckMode>,
        force_tuple: Option<bool>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_numeric_literal_name(&self, name: &str) -> bool {
        unimplemented!()
    }

    pub(super) fn check_computed_property_name(
        &self,
        node: &Node, /*ComputedPropertyName*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_object_literal(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) -> Rc<Type> {
        let node_as_object_literal_expression = node.as_object_literal_expression();
        let mut properties_table = create_symbol_table(None);
        let mut properties_array: Vec<Rc<Symbol>> = vec![];

        let object_flags = self.fresh_object_literal_flag;

        for member_decl in &node_as_object_literal_expression.properties {
            let member = self.get_symbol_of_node(&**member_decl).unwrap();
            if member_decl.kind() == SyntaxKind::PropertyAssignment {
            } else {
                unimplemented!()
            }

            if false {
                unimplemented!()
            } else {
                properties_table.insert(member.escaped_name().clone(), member.clone());
            }
            properties_array.push(member);
        }

        let create_object_literal_type = || {
            let result = self.create_anonymous_type(
                Some(node.symbol()),
                Rc::new(RefCell::new(properties_table)),
                vec![],
                vec![],
                vec![], // TODO: this is wrong
            );
            result.set_object_flags(
                result.object_flags()
                    | object_flags
                    | ObjectFlags::ObjectLiteral
                    | ObjectFlags::ContainsObjectOrArrayLiteral,
            );
            result.into()
        };

        create_object_literal_type()
    }

    pub(super) fn is_valid_spread_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_hyphenated_jsx_name(&self, name: &str) -> bool {
        unimplemented!()
    }

    pub(super) fn check_jsx_attribute(
        &self,
        node: &Node, /*JsxAttribute*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_jsx_children(
        &self,
        node: &Node, /*JsxElement | JsxFragment*/
        check_mode: Option<CheckMode>,
    ) -> Vec<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_type<TLocation: Borrow<Node>>(
        &self,
        name: &__String,
        location: Option<TLocation>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_jsx_namespace_at<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
    ) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn get_jsx_element_children_property_name(
        &self,
        jsx_namespace: &Symbol,
    ) -> Option<__String> {
        unimplemented!()
    }

    pub(super) fn is_known_property(
        &self,
        target_type: &Type,
        name: &__String,
        is_comparing_jsx_attributes: bool,
    ) -> bool {
        if target_type.flags().intersects(TypeFlags::Object) {
            if self
                .get_property_of_object_type(target_type, name)
                .is_some()
                || false
            {
                return true;
            }
        } else if target_type
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
            && self.is_excess_property_check_target(target_type)
        {
            unimplemented!()
        }
        false
    }

    pub(super) fn is_excess_property_check_target(&self, type_: &Type) -> bool {
        (type_.flags().intersects(TypeFlags::Object)
            && !(get_object_flags(type_)
                .intersects(ObjectFlags::ObjectLiteralPatternWithComputedProperties)))
            || type_.flags().intersects(TypeFlags::NonPrimitive)
            || (type_.flags().intersects(TypeFlags::Union) && unimplemented!())
            || (type_.flags().intersects(TypeFlags::Intersection) && unimplemented!())
    }

    pub(super) fn get_declaration_node_flags_from_symbol(&self, s: &Symbol) -> NodeFlags {
        unimplemented!()
    }

    pub(super) fn is_prototype_property(&self, symbol: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn check_non_null_expression(
        &self,
        node: &Node, /*Expression | QualifiedName*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_nullable_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_non_nullable_type_if_needed(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_non_null_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn lookup_symbol_for_private_identifier_declaration(
        &self,
        prop_name: &__String,
        location: &Node,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_symbol_for_private_identifier_expression(
        &self,
        priv_id: &Node, /*PrivateIdentifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_private_identifier_property_of_type_(
        &self,
        left_type: &Type,
        lexically_scoped_identifier: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn is_this_property_access_in_constructor(
        &self,
        node: &Node, /*ElementAccessExpression | PropertyAccessExpression | QualifiedName*/
        prop: &Symbol,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_unchecked_js_suggestion<TNode: Borrow<Node>, TSuggestion: Borrow<Symbol>>(
        &self,
        node: Option<TNode>,
        suggestion: Option<TSuggestion>,
        exclude_classes: bool,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_in_property_initializer_or_class_static_block(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn type_has_static_property(
        &self,
        prop_name: &__String,
        containing_type: &Type,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_suggested_lib_for_nonexistent_name(
        &self,
        name: ResolveNameNameArg,
    ) -> Option<String> {
        // unimplemented!()
        None
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_jsx_attribute<
        TName: Into<StringOrRcNode>,
    >(
        &self,
        name: TName, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_suggestion_for_nonexistent_property<TName: Into<StringOrRcNode>>(
        &self,
        name: TName, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_symbol_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        outer_name: &__String,
        meaning: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        // unimplemented!()
        None
    }

    pub(super) fn get_suggestion_for_nonexistent_symbol_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        outer_name: &__String,
        meaning: SymbolFlags,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_module(
        &self,
        name: &Node, /*Identifier*/
        target_module: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_suggestion_for_nonexistent_index_signature(
        &self,
        object_type: &Type,
        name: &Node, /*ElementAccessExpression*/
        keyed_type: &Type,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_type_for_nonexistent_string_literal_type(
        &self,
        source: &Type, /*StringLiteralType*/
        target: &Type, /*UnionType*/
    ) -> Option<Rc<Type /*StringLiteralType*/>> {
        unimplemented!()
    }

    pub(super) fn mark_property_as_referenced<TNodeForCheckWriteOnly: Borrow<Node>>(
        &self,
        prop: &Symbol,
        node_for_check_write_only: Option<TNodeForCheckWriteOnly>,
        is_self_type_access: bool,
    ) {
        unimplemented!()
    }

    pub(super) fn is_self_type_access<TParent: Borrow<Symbol>>(
        &self,
        name: &Node, /*Expression | QualifiedName*/
        parent: Option<TParent>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_valid_property_access_(
        &self,
        node: &Node, /*PropertyAccessExpression | QualifiedName | ImportTypeNode*/
        property_name: &__String,
    ) -> bool {
        unimplemented!()
    }

    pub fn is_valid_property_access_for_completions_(
        &self,
        node_in: &Node, /*PropertyAccessExpression | ImportTypeNode | QualifiedName*/
        type_: &Type,
        property: &Symbol,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn accepts_void(&self, t: &Type) -> bool {
        t.flags().intersects(TypeFlags::Void)
    }

    pub(super) fn get_single_call_signature(&self, type_: &Type) -> Option<Rc<Signature>> {
        unimplemented!()
    }

    pub(super) fn get_single_call_or_construct_signature(
        &self,
        type_: &Type,
    ) -> Option<Rc<Signature>> {
        unimplemented!()
    }

    pub(super) fn instantiate_signature_in_context_of<
        TCompareTypes: FnMut(&Type, &Type, Option<bool>) -> Ternary,
    >(
        &self,
        signature: &Signature,
        contextual_signature: &Signature,
        inference_context: Option<&InferenceContext>,
        compare_types: Option<&mut TCompareTypes>,
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn get_resolved_signature_(
        &self,
        node: &Node, /*CallLikeExpression*/
        candidates_out_array: Option<&[Rc<Signature>]>,
        check_mode: Option<CheckMode>,
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn is_js_constructor<TNode: Borrow<Node>>(&self, node: Option<TNode>) -> bool {
        unimplemented!()
    }

    pub(super) fn merge_js_symbols<TSource: Borrow<Symbol>>(
        &self,
        target: &Symbol,
        source: Option<TSource>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_assigned_class_symbol(
        &self,
        decl: &Node, /*Declaration*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_symbol_of_expando(
        &self,
        node: &Node,
        allow_declaration: bool,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_with_synthetic_default_only(
        &self,
        type_: &Type,
        symbol: &Symbol,
        original_symbol: &Symbol,
        module_specifier: &Node, /*Expression*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_type_with_synthetic_default_import_type(
        &self,
        type_: &Type,
        symbol: &Symbol,
        original_symbol: &Symbol,
        module_specifier: &Node, /*Expression*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_common_js_require(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn get_type_of_parameter(&self, symbol: &Symbol) -> Rc<Type> {
        let type_ = self.get_type_of_symbol(symbol);
        if self.strict_null_checks {
            let declaration = symbol.maybe_value_declaration();
            if matches!(declaration.as_ref(), Some(declaration) if has_initializer(&declaration)) {
                return self.get_optional_type_(&type_, None);
            }
        }
        type_
    }

    pub(super) fn get_tuple_element_label(
        &self,
        d: &Node, /*ParameterDeclaration | NamedTupleMember*/
    ) -> __String {
        unimplemented!()
    }

    pub(super) fn get_parameter_name_at_position<TOverrideRestType: Borrow<Type>>(
        &self,
        signature: &Signature,
        pos: usize,
        override_rest_type: Option<TOverrideRestType>,
    ) -> __String {
        unimplemented!()
    }

    pub(super) fn get_type_at_position(&self, signature: &Signature, pos: usize) -> Rc<Type> {
        self.try_get_type_at_position(signature, pos)
            .unwrap_or_else(|| self.any_type())
    }

    pub(super) fn try_get_type_at_position(
        &self,
        signature: &Signature,
        pos: usize,
    ) -> Option<Rc<Type>> {
        let param_count = signature.parameters().len()
            - if signature_has_rest_parameter(signature) {
                1
            } else {
                0
            };
        if pos < param_count {
            return Some(self.get_type_of_parameter(&signature.parameters()[pos]));
        }
        if signature_has_rest_parameter(signature) {
            let rest_type = self.get_type_of_symbol(&signature.parameters()[param_count]);
            let index = pos - param_count;
            unimplemented!()
        }
        None
    }

    pub(super) fn get_rest_type_at_position(&self, signature: &Signature, pos: usize) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_parameter_count(&self, signature: &Signature) -> usize {
        let length = signature.parameters().len();
        if signature_has_rest_parameter(signature) {
            let rest_type = self.get_type_of_symbol(&signature.parameters()[length - 1]);
            if self.is_tuple_type(&rest_type) {
                unimplemented!()
            }
        }
        length
    }

    pub(super) fn get_min_argument_count(
        &self,
        signature: &Signature,
        flags: Option<MinArgumentCountFlags>,
    ) -> usize {
        let strong_arity_for_untyped_js = match flags {
            None => false,
            Some(flags) => flags.intersects(MinArgumentCountFlags::StrongArityForUntypedJS),
        };
        let void_is_non_optional = match flags {
            None => false,
            Some(flags) => flags.intersects(MinArgumentCountFlags::VoidIsNonOptional),
        };
        if void_is_non_optional || signature.maybe_resolved_min_argument_count().is_none() {
            let mut min_argument_count = None;
            if signature_has_rest_parameter(signature) {
                let rest_type = self
                    .get_type_of_symbol(&signature.parameters()[signature.parameters().len() - 1]);
                if self.is_tuple_type(&rest_type) {
                    unimplemented!()
                }
            }
            if min_argument_count.is_none() {
                if !strong_arity_for_untyped_js
                    && signature
                        .flags
                        .intersects(SignatureFlags::IsUntypedSignatureInJSFile)
                {
                    return 0;
                }
                min_argument_count = Some(signature.min_argument_count());
            }
            let mut min_argument_count = min_argument_count.unwrap();
            if void_is_non_optional {
                return min_argument_count;
            }
            let mut i = min_argument_count - 1;
            while i >= 0 {
                let type_ = self.get_type_at_position(signature, i);
                if self
                    .filter_type(&type_, |type_| self.accepts_void(type_))
                    .flags()
                    .intersects(TypeFlags::Never)
                {
                    break;
                }
                min_argument_count = i;
                i -= 1;
            }
            signature.set_resolved_min_argument_count(min_argument_count);
        }
        signature.resolved_min_argument_count()
    }

    pub(super) fn has_effective_rest_parameter(&self, signature: &Signature) -> bool {
        if signature_has_rest_parameter(signature) {
            let rest_type =
                self.get_type_of_symbol(&signature.parameters()[signature.parameters().len() - 1]);
            return !self.is_tuple_type(&rest_type) || unimplemented!();
        }
        false
    }

    pub(super) fn get_effective_rest_type(&self, signature: &Signature) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_non_array_rest_type(&self, signature: &Signature) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_first_parameter_of_signature(
        &self,
        signature: &Signature,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn create_promise_type(&self, promised_type: &Type) -> Rc<Type> {
        let global_promise_type = self.get_global_promise_type(true);
        if !Rc::ptr_eq(&global_promise_type, &self.empty_generic_type()) {
            let promised_type = self
                .get_awaited_type_no_alias(
                    &self.unwrap_awaited_type(promised_type),
                    Option::<&Node>::None,
                    None,
                    None,
                )
                .unwrap_or_else(|| self.unknown_type());
            return self.create_type_reference(&global_promise_type, Some(vec![promised_type]));
        }

        self.unknown_type()
    }

    pub(super) fn create_promise_return_type(
        &self,
        func: &Node, /*FunctionLikeDeclaration | ImportCall*/
        promised_type: &Type,
    ) -> Rc<Type> {
        let promise_type = self.create_promise_type(promised_type);
        if Rc::ptr_eq(&promise_type, &self.unknown_type()) {
            self.error(
                Some(func),
                if is_import_call(func) {
                    &Diagnostics::A_dynamic_import_call_returns_a_Promise_Make_sure_you_have_a_declaration_for_Promise_or_include_ES2015_in_your_lib_option
                } else {
                    &Diagnostics::An_async_function_or_method_must_return_a_Promise_Make_sure_you_have_a_declaration_for_Promise_or_include_ES2015_in_your_lib_option
                },
                None
            );
            return self.error_type();
        } else if self.get_global_promise_constructor_symbol(true).is_none() {
            self.error(
                Some(func),
                if is_import_call(func) {
                    &Diagnostics::A_dynamic_import_call_in_ES5_SlashES3_requires_the_Promise_constructor_Make_sure_you_have_a_declaration_for_the_Promise_constructor_or_include_ES2015_in_your_lib_option
                } else {
                    &Diagnostics::An_async_function_or_method_in_ES5_SlashES3_requires_the_Promise_constructor_Make_sure_you_have_a_declaration_for_the_Promise_constructor_or_include_ES2015_in_your_lib_option
                },
                None
            );
        }

        promise_type
    }

    pub(super) fn get_return_type_from_body(
        &self,
        func: &Node, /*FunctionLikeDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let func_as_function_like_declaration = func.as_function_like_declaration();
        if func_as_function_like_declaration.maybe_body().is_none() {
            return self.error_type();
        }
        let func_body = func_as_function_like_declaration.maybe_body().unwrap();

        let function_flags = get_function_flags(Some(func));
        let is_async = function_flags.intersects(FunctionFlags::Async);
        let is_generator = function_flags.intersects(FunctionFlags::Generator);

        let mut return_type: Option<Rc<Type>> = None;
        let mut yield_type: Option<Rc<Type>> = None;
        let mut next_type: Option<Rc<Type>> = None;
        let fallback_return_type = self.void_type();
        if func_body.kind() != SyntaxKind::Block {
            return_type = Some(self.check_expression_cached(
                &func_body,
                check_mode.map(|check_mode| check_mode & !CheckMode::SkipGenericFunctions),
            ));
            if is_async {
                unimplemented!()
            }
        } else if is_generator {
            unimplemented!()
        } else {
            let types = self.check_and_aggregate_return_expression_types(func, check_mode);
            if types.is_none() {
                return if function_flags.intersects(FunctionFlags::Async) {
                    self.create_promise_return_type(func, &self.never_type())
                } else {
                    self.never_type()
                };
            }
            let types = types.unwrap();
            if types.is_empty() {
                return if function_flags.intersects(FunctionFlags::Async) {
                    self.create_promise_return_type(func, &self.void_type())
                } else {
                    self.void_type()
                };
            }

            return_type = Some(self.get_union_type(
                types,
                Some(UnionReduction::Subtype),
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            ));
        }

        if return_type.is_some() || yield_type.is_some() || next_type.is_some() {
            if let Some(yield_type) = yield_type.as_ref() {
                self.report_errors_from_widening(
                    func,
                    yield_type,
                    Some(WideningKind::GeneratorYield),
                );
            }
            if let Some(return_type) = return_type.as_ref() {
                self.report_errors_from_widening(
                    func,
                    return_type,
                    Some(WideningKind::FunctionReturn),
                );
            }
            if let Some(next_type) = next_type.as_ref() {
                self.report_errors_from_widening(
                    func,
                    next_type,
                    Some(WideningKind::GeneratorNext),
                );
            }

            if matches!(return_type.as_ref(), Some(return_type) if self.is_unit_type(return_type))
                || matches!(yield_type.as_ref(), Some(yield_type) if self.is_unit_type(yield_type))
                || matches!(next_type.as_ref(), Some(next_type) if self.is_unit_type(next_type))
            {
                unimplemented!()
            }

            if let Some(yield_type_present) = yield_type {
                yield_type = Some(self.get_widened_type(&yield_type_present));
            }
            if let Some(return_type_present) = return_type {
                return_type = Some(self.get_widened_type(&return_type_present));
            }
            if let Some(next_type_present) = next_type {
                next_type = Some(self.get_widened_type(&next_type_present));
            }
        }

        if is_generator {
            unimplemented!()
        } else {
            if is_async {
                self.create_promise_type(&return_type.unwrap_or(fallback_return_type))
            } else {
                return_type.unwrap_or(fallback_return_type)
            }
        }
    }

    pub(super) fn get_facts_from_typeof_switch(
        &self,
        start: usize,
        end: usize,
        witnesses: &[String],
        has_default: bool,
    ) -> TypeFacts {
        unimplemented!()
    }

    pub(super) fn is_exhaustive_switch_statement(
        &self,
        node: &Node, /*SwitchStatement*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_and_aggregate_return_expression_types(
        &self,
        func: &Node, /*FunctionLikeDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> Option<Vec<Rc<Type>>> {
        unimplemented!()
    }
}
