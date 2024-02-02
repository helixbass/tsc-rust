use std::{borrow::Borrow, io, ptr};

use id_arena::Id;

use crate::{
    add_related_info, contains, create_diagnostic_for_node, find_ancestor,
    for_each_child_returns, for_each_enclosing_block_scope_container, get_ancestor,
    get_assignment_declaration_kind, get_class_extends_heritage_element,
    get_enclosing_block_scope_container, get_jsdoc_this_tag, get_jsdoc_type, get_super_container,
    get_this_container, get_this_parameter, has_static_modifier, has_syntactic_modifier,
    is_assignment_target, is_binary_expression, is_call_expression, is_class_like,
    is_class_static_block_declaration, is_external_or_common_js_module, is_for_statement,
    is_function_like, is_function_like_declaration, is_identifier, is_in_js_file,
    is_iteration_statement, is_method_declaration, is_object_literal_expression,
    is_property_assignment, is_property_declaration, is_source_file, is_static, is_super_call,
    is_super_property, length, maybe_is_class_like, node_starts_new_lexical_environment,
    push_if_unique_eq, text_range_contains_position_inclusive, AsDoubleDeref,
    AssignmentDeclarationKind, DiagnosticMessage, Diagnostics, FindAncestorCallbackReturn,
    HasArena, HasTypeInterface, InArena, InterfaceTypeInterface, InternalSymbolName, ModifierFlags,
    NamedDeclarationInterface, Node, NodeArray, NodeCheckFlags, NodeInterface, OptionTry,
    ReadonlyTextRange, ScriptTarget, SignatureDeclarationInterface, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeInterface,
    OptionInArena,
};

impl TypeChecker {
    pub(super) fn is_inside_function_or_instance_property_initializer(
        &self,
        node: Id<Node>,
        threshold: Id<Node>,
    ) -> bool {
        find_ancestor(Some(node), |n: Id<Node>| {
            if n == threshold {
                FindAncestorCallbackReturn::Quit
            } else {
                (is_function_like(Some(&n.ref_(self)))
                    || (matches!(
                        n.ref_(self).maybe_parent(),
                        Some(n_parent) if is_property_declaration(&n_parent.ref_(self)) &&
                            !has_static_modifier(n_parent, self) &&
                                n_parent.ref_(self).as_has_initializer().maybe_initializer() == Some(n)
                    )))
                .into()
            }
        }, self)
        .is_some()
    }

    pub(super) fn get_part_of_for_statement_containing_node(
        &self,
        node: Id<Node>,
        container: Id<Node>, /*ForStatement*/
    ) -> Option<Id<Node>> {
        let container_ref = container.ref_(self);
        let container_as_for_statement = container_ref.as_for_statement();
        find_ancestor(Some(node), |n: Id<Node>| {
            if n == container {
                FindAncestorCallbackReturn::Quit
            } else {
                (container_as_for_statement.initializer == Some(n)
                 || container_as_for_statement.condition == Some(n)
                 || container_as_for_statement.incrementor == Some(n)
                 || n == container_as_for_statement.statement)
                .into()
            }
        }, self)
    }

    pub(super) fn get_enclosing_iteration_statement(&self, node: Id<Node>) -> Option<Id<Node>> {
        find_ancestor(Some(node), |n: Id<Node>| {
            if
            /* !n ||*/
            node_starts_new_lexical_environment(&n.ref_(self)) {
                FindAncestorCallbackReturn::Quit
            } else {
                is_iteration_statement(n, false, self).into()
            }
        }, self)
    }

    pub(super) fn check_nested_block_scoped_binding(
        &self,
        node: Id<Node>, /*Identifier*/
        symbol: Id<Symbol>,
    ) {
        if self.language_version >= ScriptTarget::ES2015
            || !symbol
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::BlockScopedVariable | SymbolFlags::Class)
            || match symbol.ref_(self).maybe_value_declaration() {
                None => true,
                Some(symbol_value_declaration) => {
                    is_source_file(&symbol_value_declaration.ref_(self))
                        || symbol_value_declaration.ref_(self).parent().ref_(self).kind() == SyntaxKind::CatchClause
                }
            }
        {
            return;
        }
        let symbol_value_declaration = symbol.ref_(self).maybe_value_declaration().unwrap();

        let container = get_enclosing_block_scope_container(symbol_value_declaration, self).unwrap();
        let is_captured =
            self.is_inside_function_or_instance_property_initializer(node, container);

        let enclosing_iteration_statement = self.get_enclosing_iteration_statement(container);
        if let Some(enclosing_iteration_statement) = enclosing_iteration_statement {
            if is_captured {
                let mut captures_block_scope_binding_in_loop_body = true;
                if is_for_statement(&container.ref_(self)) {
                    let var_decl_list = get_ancestor(
                        Some(symbol_value_declaration),
                        SyntaxKind::VariableDeclarationList,
                        self,
                    );
                    if matches!(
                        var_decl_list,
                        Some(var_decl_list) if var_decl_list.ref_(self).parent() == container
                    ) {
                        let part = self
                            .get_part_of_for_statement_containing_node(node.ref_(self).parent(), container);
                        if let Some(part) = part {
                            let links = self.get_node_links(part);
                            links.ref_mut(self).flags |=
                                NodeCheckFlags::ContainsCapturedBlockScopedBinding;

                            if links.ref_(self).captured_block_scope_bindings.is_none() {
                                links.ref_mut(self).captured_block_scope_bindings = Some(vec![]);
                            }
                            {
                                let mut links = links.ref_mut(self);
                                let captured_bindings =
                                    links.captured_block_scope_bindings.as_mut().unwrap();
                                push_if_unique_eq(captured_bindings, &symbol);
                            }

                            if container.ref_(self).as_for_statement().initializer == Some(part) {
                                captures_block_scope_binding_in_loop_body = false;
                            }
                        }
                    }
                }
                if captures_block_scope_binding_in_loop_body {
                    self.get_node_links(enclosing_iteration_statement)
                        .ref_mut(self)
                        .flags |= NodeCheckFlags::LoopWithCapturedBlockScopedBinding;
                }
            }

            if is_for_statement(&container.ref_(self)) {
                let var_decl_list = get_ancestor(
                    Some(symbol_value_declaration),
                    SyntaxKind::VariableDeclarationList,
                    self,
                );
                if matches!(
                    var_decl_list,
                    Some(var_decl_list) if var_decl_list.ref_(self).parent() == container
                        && self.is_assigned_in_body_of_for_statement(node, container)
                ) {
                    self.get_node_links(symbol_value_declaration)
                        .ref_mut(self)
                        .flags |= NodeCheckFlags::NeedsLoopOutParameter;
                }
            }

            self.get_node_links(symbol_value_declaration)
                .ref_mut(self)
                .flags |= NodeCheckFlags::BlockScopedBindingInLoop;
        }

        if is_captured {
            self.get_node_links(symbol_value_declaration)
                .ref_mut(self)
                .flags |= NodeCheckFlags::CapturedBlockScopedBinding;
        }
    }

    pub(super) fn is_binding_captured_by_node(
        &self,
        node: Id<Node>,
        decl: Id<Node>, /*VariableDeclaration | BindingElement*/
    ) -> io::Result<bool> {
        let links = self.get_node_links(node);
        /* !!links &&*/
        let ret = contains(
            links.ref_(self).captured_block_scope_bindings.as_deref(),
            &self.get_symbol_of_node(decl)?.unwrap(),
        );
        Ok(ret)
    }

    pub(super) fn is_assigned_in_body_of_for_statement(
        &self,
        node: Id<Node>,      /*Identifier*/
        container: Id<Node>, /*ForStatement*/
    ) -> bool {
        let mut current = node;
        while current.ref_(self).parent().ref_(self).kind() == SyntaxKind::ParenthesizedExpression {
            current = current.ref_(self).parent();
        }

        let mut is_assigned = false;
        if is_assignment_target(current, self) {
            is_assigned = true;
        } else if matches!(
            current.ref_(self).parent().ref_(self).kind(),
            SyntaxKind::PrefixUnaryExpression | SyntaxKind::PostfixUnaryExpression
        ) {
            let expr = current.ref_(self).parent();
            is_assigned = matches!(
                expr.ref_(self).as_unary_expression().operator(),
                SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
            );
        }

        if !is_assigned {
            return false;
        }

        let container_ref = container.ref_(self);
        let container_as_for_statement = container_ref.as_for_statement();
        find_ancestor(Some(current), |n: Id<Node>| {
            if n == container {
                FindAncestorCallbackReturn::Quit
            } else {
                (n == container_as_for_statement.statement).into()
            }
        }, self)
        .is_some()
    }

    pub(super) fn capture_lexical_this(&self, node: Id<Node>, container: Id<Node>) {
        self.get_node_links(node).ref_mut(self).flags |= NodeCheckFlags::LexicalThis;
        if matches!(
            container.ref_(self).kind(),
            SyntaxKind::PropertyDeclaration | SyntaxKind::Constructor
        ) {
            let class_node = container.ref_(self).parent();
            self.get_node_links(class_node).ref_mut(self).flags |= NodeCheckFlags::CaptureThis;
        } else {
            self.get_node_links(container).ref_mut(self).flags |= NodeCheckFlags::CaptureThis;
        }
    }

    pub(super) fn find_first_super_call(&self, node: Id<Node>) -> Option<Id<Node /*SuperCall*/>> {
        if is_super_call(node, self) {
            Some(node)
        } else if is_function_like(Some(&node.ref_(self))) {
            None
        } else {
            for_each_child_returns(
                node,
                |node: Id<Node>| self.find_first_super_call(node),
                Option::<fn(Id<NodeArray>) -> Option<Id<Node>>>::None,
                self,
            )
        }
    }

    pub(super) fn class_declaration_extends_null(
        &self,
        class_decl: Id<Node>, /*ClassDeclaration*/
    ) -> io::Result<bool> {
        let class_symbol = self.get_symbol_of_node(class_decl)?.unwrap();
        let class_instance_type = self.get_declared_type_of_symbol(class_symbol)?;
        let base_constructor_type = self.get_base_constructor_type_of_class(class_instance_type)?;

        Ok(base_constructor_type == self.null_widening_type())
    }

    pub(super) fn check_this_before_super(
        &self,
        node: Id<Node>,
        container: Id<Node>,
        diagnostic_message: &DiagnosticMessage,
    ) -> io::Result<()> {
        let containing_class_decl = container.ref_(self).parent();
        let base_type_node = get_class_extends_heritage_element(containing_class_decl, self);

        if base_type_node.is_some() {
            if !self.class_declaration_extends_null(containing_class_decl)? {
                if matches!(
                    node.ref_(self).maybe_flow_node(),
                    Some(node_flow_node) if !self.is_post_super_flow_node(node_flow_node, false)
                ) {
                    self.error(Some(node), diagnostic_message, None);
                }
            }
        }

        Ok(())
    }

    pub(super) fn check_this_in_static_class_field_initializer_in_decorated_class(
        &self,
        this_expression: Id<Node>,
        container: Id<Node>,
    ) {
        if is_property_declaration(&container.ref_(self))
            && has_static_modifier(container, self)
            && matches!(
                container.ref_(self).as_has_initializer().maybe_initializer(),
                Some(container_initializer) if text_range_contains_position_inclusive(
                    &*container_initializer.ref_(self),
                    this_expression.ref_(self).pos(),
                )
            )
            && length(container.ref_(self).parent().ref_(self).maybe_decorators().refed(self).as_double_deref()) > 0
        {
            self.error(
                Some(this_expression),
                &Diagnostics::Cannot_use_this_in_a_static_property_initializer_of_a_decorated_class,
                None,
            );
        }
    }

    pub(super) fn check_this_expression(&self, node: Id<Node>) -> io::Result<Id<Type>> {
        let is_node_in_type_query = self.is_in_type_query(node);
        let mut container = get_this_container(node, true, self);
        let mut captured_by_arrow_function = false;

        if container.ref_(self).kind() == SyntaxKind::Constructor {
            self.check_this_before_super(node, container, &Diagnostics::super_must_be_called_before_accessing_this_in_the_constructor_of_a_derived_class)?;
        }

        if container.ref_(self).kind() == SyntaxKind::ArrowFunction {
            container = get_this_container(container, false, self);
            captured_by_arrow_function = true;
        }

        self.check_this_in_static_class_field_initializer_in_decorated_class(node, container);
        match container.ref_(self).kind() {
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
                if self.is_in_constructor_argument_initializer(node, container) {
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
            self.capture_lexical_this(node, container);
        }

        let type_ = self.try_get_this_type_at_(node, Some(true), Some(container))?;
        if self.no_implicit_this {
            let global_this_type = self.get_type_of_symbol(self.global_this_symbol())?;
            if matches!(
                type_,
                Some(type_) if type_ == global_this_type
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
                if !is_source_file(&container.ref_(self)) {
                    let outside_this =
                        self.try_get_this_type_at_(container, None, Option::<Id<Node>>::None)?;
                    if matches!(
                        outside_this,
                        Some(outside_this) if outside_this != global_this_type
                    ) {
                        add_related_info(
                            &diag.ref_(self),
                            vec![
                                self.alloc_diagnostic_related_information(
                                    create_diagnostic_for_node(
                                        container,
                                        &Diagnostics::An_outer_value_of_this_is_shadowed_by_this_container,
                                        None,
                                        self,
                                    ).into()
                                )
                            ]
                        );
                    }
                }
            }
        }
        Ok(type_.unwrap_or_else(|| self.any_type()))
    }

    pub(super) fn try_get_this_type_at_(
        &self,
        node: Id<Node>,
        include_global_this: Option<bool>,
        container: Option<Id<Node>>,
    ) -> io::Result<Option<Id<Type>>> {
        let include_global_this = include_global_this.unwrap_or(true);
        let container = container.unwrap_or_else(
            || get_this_container(node, false, self),
        );
        let is_in_js = is_in_js_file(Some(&node.ref_(self)));
        if is_function_like(Some(&container.ref_(self)))
            && (!self.is_in_parameter_initializer_before_containing_function(node)
                || get_this_parameter(container, self).is_some())
        {
            let mut this_type = self.get_this_type_of_declaration(container)?.try_or_else(
                || -> io::Result<_> {
                    Ok(if is_in_js {
                        self.get_type_for_this_expression_from_jsdoc(container)?
                    } else {
                        None
                    })
                },
            )?;
            if this_type.is_none() {
                let class_name = self.get_class_name_from_prototype_method(container);
                if let Some(class_name) = class_name.filter(|_| is_in_js) {
                    let class_symbol = self
                        .check_expression(class_name, None, None)?
                        .ref_(self)
                        .maybe_symbol();
                    if let Some(class_symbol) = class_symbol.filter(|&class_symbol| {
                        class_symbol.ref_(self).maybe_members().is_some()
                            && class_symbol
                                .ref_(self)
                                .flags()
                                .intersects(SymbolFlags::Function)
                    }) {
                        this_type = self
                            .get_declared_type_of_symbol(class_symbol)?
                            .ref_(self)
                            .maybe_as_interface_type()
                            .and_then(|interface_type| interface_type.maybe_this_type());
                    }
                } else if self.is_js_constructor(Some(container))? {
                    this_type = self
                        .get_declared_type_of_symbol(
                            self.get_merged_symbol(Some(container.ref_(self).symbol())).unwrap(),
                        )?
                        .ref_(self)
                        .maybe_as_interface_type()
                        .and_then(|interface_type| interface_type.maybe_this_type());
                }
                if this_type.is_none() {
                    this_type = self.get_contextual_this_parameter_type(container)?;
                }
            }

            if let Some(this_type) = this_type {
                return Ok(Some(self.get_flow_type_of_reference(
                    node,
                    this_type,
                    None,
                    Option::<Id<Node>>::None,
                )?));
            }
        }

        if maybe_is_class_like(container.ref_(self).maybe_parent().refed(self).as_deref()) {
            let symbol = self.get_symbol_of_node(container.ref_(self).parent())?.unwrap();
            let type_ = if is_static(container, self) {
                self.get_type_of_symbol(symbol)?
            } else {
                self.get_declared_type_of_symbol(symbol)?
                    .ref_(self)
                    .as_interface_type()
                    .maybe_this_type()
                    .unwrap()
            };
            return Ok(Some(self.get_flow_type_of_reference(
                node,
                type_,
                None,
                Option::<Id<Node>>::None,
            )?));
        }

        if is_source_file(&container.ref_(self)) {
            let container_ref = container.ref_(self);
            let container_as_source_file = container_ref.as_source_file();
            if container_as_source_file
                .maybe_common_js_module_indicator()
                .is_some()
            {
                let file_symbol = self.get_symbol_of_node(container)?;
                return file_symbol.try_map(|file_symbol| self.get_type_of_symbol(file_symbol));
            } else if container_as_source_file
                .maybe_external_module_indicator()
                .is_some()
            {
                return Ok(Some(self.undefined_type()));
            } else if include_global_this {
                return Ok(Some(self.get_type_of_symbol(self.global_this_symbol())?));
            }
        }
        Ok(None)
    }

    pub(super) fn get_explicit_this_type(
        &self,
        node: Id<Node>, /*Expression*/
    ) -> io::Result<Option<Id<Type>>> {
        let container = get_this_container(node, false, self);
        if is_function_like(Some(&container.ref_(self))) {
            let signature = self.get_signature_from_declaration_(container)?;
            let signature_this_parameter = signature.ref_(self).maybe_this_parameter();
            if let Some(signature_this_parameter) = signature_this_parameter {
                return self.get_explicit_type_of_symbol(signature_this_parameter, None);
            }
        }
        if maybe_is_class_like(container.ref_(self).maybe_parent().refed(self).as_deref()) {
            let symbol = self.get_symbol_of_node(container.ref_(self).parent())?.unwrap();
            return Ok(if is_static(container, self) {
                Some(self.get_type_of_symbol(symbol)?)
            } else {
                self.get_declared_type_of_symbol(symbol)?
                    .ref_(self)
                    .as_interface_type()
                    .maybe_this_type()
            });
        }
        Ok(None)
    }

    pub(super) fn get_class_name_from_prototype_method(
        &self,
        container: Id<Node>,
    ) -> Option<Id<Node>> {
        if container.ref_(self).kind() == SyntaxKind::FunctionExpression
            && is_binary_expression(&container.ref_(self).parent().ref_(self))
            && get_assignment_declaration_kind(container.ref_(self).parent(), self)
                == AssignmentDeclarationKind::PrototypeProperty
        {
            return Some(
                container
                    .ref_(self).parent()
                    .ref_(self).as_binary_expression()
                    .left
                    .ref_(self).as_property_access_expression()
                    .expression
                    .ref_(self).as_property_access_expression()
                    .expression,
            );
        } else if container.ref_(self).kind() == SyntaxKind::MethodDeclaration
            && container.ref_(self).parent().ref_(self).kind() == SyntaxKind::ObjectLiteralExpression
            && is_binary_expression(&container.ref_(self).parent().ref_(self).parent().ref_(self))
            && get_assignment_declaration_kind(container.ref_(self).parent().ref_(self).parent(), self)
                == AssignmentDeclarationKind::Prototype
        {
            return Some(
                container
                    .ref_(self).parent()
                    .ref_(self).parent()
                    .ref_(self).as_binary_expression()
                    .left
                    .ref_(self).as_property_access_expression()
                    .expression,
            );
        } else if container.ref_(self).kind() == SyntaxKind::FunctionExpression
            && container.ref_(self).parent().ref_(self).kind() == SyntaxKind::PropertyAssignment
            && container.ref_(self).parent().ref_(self).parent().ref_(self).kind() == SyntaxKind::ObjectLiteralExpression
            && is_binary_expression(&container.ref_(self).parent().ref_(self).parent().ref_(self).parent().ref_(self))
            && get_assignment_declaration_kind(container.ref_(self).parent().ref_(self).parent().ref_(self).parent(), self)
                == AssignmentDeclarationKind::Prototype
        {
            return Some(
                container
                    .ref_(self).parent()
                    .ref_(self).parent()
                    .ref_(self).parent()
                    .ref_(self).as_binary_expression()
                    .left
                    .ref_(self).as_property_access_expression()
                    .expression,
            );
        } else if container.ref_(self).kind() == SyntaxKind::FunctionExpression
            && {
                let container_parent = container.ref_(self).parent();
                is_property_assignment(&container_parent.ref_(self))
                    && {
                        let container_parent_ref = container_parent.ref_(self);
                        let container_parent_as_property_assignment = container_parent_ref.as_property_assignment();
                        is_identifier(&container_parent_as_property_assignment.name().ref_(self))
                            && matches!(
                                &*container_parent_as_property_assignment
                                    .name()
                                    .ref_(self).as_identifier()
                                    .escaped_text,
                                "value" | "get" | "set"
                            )
                            && {
                                let container_parent_parent = container_parent.ref_(self).parent();
                                is_object_literal_expression(&container_parent_parent.ref_(self))
                                    && {
                                        let container_parent_parent_parent =
                                            container_parent_parent.ref_(self).parent();
                                        is_call_expression(&container_parent_parent_parent.ref_(self)) &&
                                            container_parent_parent_parent.ref_(self).as_call_expression().arguments.ref_(self).get(2).copied() == Some(container_parent_parent)
                                                && get_assignment_declaration_kind(container_parent_parent_parent, self) == AssignmentDeclarationKind::ObjectDefinePrototypeProperty
                                    }
                            }
                    }
            }
        {
            return Some(
                container
                    .ref_(self).parent()
                    .ref_(self).parent()
                    .ref_(self).parent()
                    .ref_(self).as_call_expression()
                    .arguments.ref_(self)[0]
                    .ref_(self).as_property_access_expression()
                    .expression,
            );
        } else if is_method_declaration(&container.ref_(self)) && {
            let container_ref = container.ref_(self);
            let container_as_method_declaration = container_ref.as_method_declaration();
            is_identifier(&container_as_method_declaration.name().ref_(self))
                && matches!(
                    &*container_as_method_declaration
                        .name()
                        .ref_(self).as_identifier()
                        .escaped_text,
                    "value" | "get" | "set"
                )
                && {
                    let container_parent = container.ref_(self).parent();
                    is_object_literal_expression(&container_parent.ref_(self)) && {
                        let container_parent_parent = container_parent.ref_(self).parent();
                        is_call_expression(&container_parent_parent.ref_(self))
                            && container_parent_parent.ref_(self).as_call_expression().arguments.ref_(self).get(2).copied() == Some(container_parent)
                            && get_assignment_declaration_kind(container_parent_parent, self)
                                == AssignmentDeclarationKind::ObjectDefinePrototypeProperty
                    }
                }
        } {
            return Some(
                container.ref_(self).parent().ref_(self).parent().ref_(self).as_call_expression().arguments.ref_(self)[0]
                    .ref_(self).as_property_access_expression()
                    .expression,
            );
        }
        None
    }

    pub(super) fn get_type_for_this_expression_from_jsdoc(
        &self,
        node: Id<Node>,
    ) -> io::Result<Option<Id<Type>>> {
        let jsdoc_type = get_jsdoc_type(node, self);
        if let Some(jsdoc_type) = jsdoc_type
            .filter(|jsdoc_type| jsdoc_type.ref_(self).kind() == SyntaxKind::JSDocFunctionType)
        {
            let jsdoc_type_ref = jsdoc_type.ref_(self);
            let js_doc_function_type = jsdoc_type_ref.as_jsdoc_function_type();
            let js_doc_function_type_parameters = js_doc_function_type.parameters();
            if !js_doc_function_type_parameters.ref_(self).is_empty()
                && matches!(
                    js_doc_function_type_parameters.ref_(self)[0].ref_(self).as_parameter_declaration().maybe_name(),
                    Some(name) if name.ref_(self).as_identifier().escaped_text == InternalSymbolName::This
                )
            {
                return Ok(Some(
                    self.get_type_from_type_node_(
                        js_doc_function_type_parameters.ref_(self)[0]
                            .ref_(self).as_parameter_declaration()
                            .maybe_type()
                            .unwrap(),
                    )?,
                ));
            }
        }
        let this_tag = get_jsdoc_this_tag(node, self);
        this_tag
            .and_then(|this_tag| {
                this_tag
                    .ref_(self).as_base_jsdoc_type_like_tag()
                    .type_expression
            })
            .try_map(|this_tag_type_expression| {
                self.get_type_from_type_node_(this_tag_type_expression)
            })
    }

    pub(super) fn is_in_constructor_argument_initializer(
        &self,
        node: Id<Node>,
        constructor_decl: Id<Node>,
    ) -> bool {
        find_ancestor(Some(node), |n: Id<Node>| {
            if is_function_like_declaration(&n.ref_(self)) {
                FindAncestorCallbackReturn::Quit
            } else {
                (n.ref_(self).kind() == SyntaxKind::Parameter && n.ref_(self).parent() == constructor_decl)
                    .into()
            }
        }, self)
        .is_some()
    }

    pub(super) fn check_super_expression(&self, node: Id<Node>) -> io::Result<Id<Type>> {
        let is_call_expression = node.ref_(self).parent().ref_(self).kind() == SyntaxKind::CallExpression
            && node.ref_(self).parent().ref_(self).as_call_expression().expression == node;

        let immediate_container = get_super_container(node, true, self).unwrap();
        let mut container = Some(immediate_container.clone());
        let mut need_to_capture_lexical_this = false;

        if !is_call_expression {
            while let Some(container_present) = container
                .filter(|container| container.ref_(self).kind() == SyntaxKind::ArrowFunction)
            {
                container = get_super_container(container_present, true, self);
                need_to_capture_lexical_this = self.language_version < ScriptTarget::ES2015;
            }
        }

        let can_use_super_expression =
            self.is_legal_usage_of_super_expression(is_call_expression, container);
        let node_check_flag: NodeCheckFlags;

        if !can_use_super_expression {
            let current = find_ancestor(Some(node), |n: Id<Node>| {
                if container == Some(n) {
                    FindAncestorCallbackReturn::Quit
                } else {
                    (n.ref_(self).kind() == SyntaxKind::ComputedPropertyName).into()
                }
            }, self);
            if matches!(
                current,
                Some(current) if current.ref_(self).kind() == SyntaxKind::ComputedPropertyName
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
            } else if match container {
                None => true,
                Some(container) => match container.ref_(self).maybe_parent() {
                    None => true,
                    Some(container_parent) => {
                        !(is_class_like(&container_parent.ref_(self))
                            || container_parent.ref_(self).kind() == SyntaxKind::ObjectLiteralExpression)
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
            return Ok(self.error_type());
        }

        if !is_call_expression && immediate_container.ref_(self).kind() == SyntaxKind::Constructor {
            self.check_this_before_super(
                node,
                container.unwrap(),
                &Diagnostics::super_must_be_called_before_accessing_a_property_of_super_in_the_constructor_of_a_derived_class
            )?;
        }

        if is_static(container.unwrap(), self) || is_call_expression {
            node_check_flag = NodeCheckFlags::SuperStatic;
            if !is_call_expression
                && self.language_version >= ScriptTarget::ES2015
                && self.language_version <= ScriptTarget::ES2021
                && (is_property_declaration(&container.unwrap().ref_(self))
                    || is_class_static_block_declaration(&container.unwrap().ref_(self)))
            {
                for_each_enclosing_block_scope_container(node.ref_(self).parent(), |current: Id<Node>| {
                    if !is_source_file(&current.ref_(self)) || is_external_or_common_js_module(&current.ref_(self)) {
                        self.get_node_links(current).ref_mut(self).flags |=
                            NodeCheckFlags::ContainsSuperPropertyInStaticInitializer;
                    }
                }, self);
            }
        } else {
            node_check_flag = NodeCheckFlags::SuperInstance;
        }

        self.get_node_links(node).ref_mut(self).flags |= node_check_flag;

        let container = container.unwrap();
        if container.ref_(self).kind() == SyntaxKind::MethodDeclaration
            && has_syntactic_modifier(container, ModifierFlags::Async, self)
        {
            if is_super_property(node.ref_(self).parent(), self) && is_assignment_target(node.ref_(self).parent(), self) {
                self.get_node_links(container).ref_mut(self).flags |=
                    NodeCheckFlags::AsyncMethodWithSuperBinding;
            } else {
                self.get_node_links(container).ref_mut(self).flags |=
                    NodeCheckFlags::AsyncMethodWithSuper;
            }
        }

        if need_to_capture_lexical_this {
            self.capture_lexical_this(node.ref_(self).parent(), container);
        }

        if container.ref_(self).parent().ref_(self).kind() == SyntaxKind::ObjectLiteralExpression {
            if self.language_version < ScriptTarget::ES2015 {
                self.error(
                    Some(node),
                    &Diagnostics::super_is_only_allowed_in_members_of_object_literal_expressions_when_option_target_is_ES2015_or_higher,
                    None,
                );
                return Ok(self.error_type());
            } else {
                return Ok(self.any_type());
            }
        }

        let class_like_declaration = container.ref_(self).parent();
        if get_class_extends_heritage_element(class_like_declaration, self).is_none() {
            self.error(
                Some(node),
                &Diagnostics::super_can_only_be_referenced_in_a_derived_class,
                None,
            );
            return Ok(self.error_type());
        }

        let class_type = self.get_declared_type_of_symbol(
            self.get_symbol_of_node(class_like_declaration)?.unwrap(),
        )?;
        let base_class_type = /*classType &&*/
            self.get_base_types(class_type)?.get(0).cloned();
        if base_class_type.is_none() {
            return Ok(self.error_type());
        }
        let base_class_type = base_class_type.unwrap();

        if container.ref_(self).kind() == SyntaxKind::Constructor
            && self.is_in_constructor_argument_initializer(node, container)
        {
            self.error(
                Some(node),
                &Diagnostics::super_cannot_be_referenced_in_constructor_arguments,
                None,
            );
            return Ok(self.error_type());
        }

        Ok(if node_check_flag == NodeCheckFlags::SuperStatic {
            self.get_base_constructor_type_of_class(class_type)?
        } else {
            self.get_type_with_this_argument(
                base_class_type,
                class_type.ref_(self).as_interface_type().maybe_this_type(),
                None,
            )?
        })
    }
}
