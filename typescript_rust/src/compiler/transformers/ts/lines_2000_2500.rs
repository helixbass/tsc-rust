use std::{io, ptr};

use gc::Gc;
use id_arena::Id;

use super::TransformTypeScript;
use crate::{
    add_emit_flags, add_range, get_initialized_variables,
    get_leading_comment_ranges_of_node, get_parse_tree_node, has_syntactic_modifier,
    insert_statements_after_standard_prologue, is_assertion_expression, is_binding_name,
    is_binding_pattern, is_enum_const, is_expression, is_instantiated_module, is_jsx_attributes,
    is_jsx_tag_name_expression, is_left_hand_side_expression, is_modifier, is_module_declaration,
    maybe_visit_nodes, move_range_past_decorators, move_range_past_modifiers, node_is_missing,
    parameter_is_this_keyword, set_comment_range, set_emit_flags, set_source_map_range,
    set_synthetic_leading_comments, set_synthetic_trailing_comments, set_text_range,
    set_text_range_id_node, should_preserve_const_enums, skip_outer_expressions,
    try_flatten_destructuring_assignment, try_map, try_maybe_visit_each_child,
    try_maybe_visit_node, try_maybe_visit_nodes, try_visit_each_child, try_visit_function_body,
    try_visit_node, try_visit_nodes, try_visit_parameter_list, EmitFlags, FlattenLevel,
    FunctionLikeDeclarationInterface, HasInitializerInterface, ModifierFlags, ModuleKind,
    NamedDeclarationInterface, Node, NodeArray, NodeArrayExt, NodeExt, NodeInterface, NonEmpty,
    OuterExpressionKinds, ReadonlyTextRange, SignatureDeclarationInterface, StringOrNumber,
    SyntaxKind, VisitResult,
    HasArena, InArena, OptionInArena,
    CoreTransformationContext,
};

impl TransformTypeScript {
    pub(super) fn visit_method_declaration(
        &self,
        node: Id<Node>, /*MethodDeclaration*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_method_declaration = node_ref.as_method_declaration();
        if !self.should_emit_function_like_declaration(node) {
            return Ok(None);
        }
        let updated = self.factory.ref_(self).update_method_declaration(
            node,
            Option::<Id<NodeArray>>::None,
            maybe_visit_nodes(
                node.ref_(self).maybe_modifiers(),
                Some(|node: Id<Node>| self.modifier_visitor(node)),
                Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                None,
                None,
                self,
            ),
            node_as_method_declaration.maybe_asterisk_token(),
            self.visit_property_name_of_class_element(node)?,
            None,
            Option::<Id<NodeArray>>::None,
            try_visit_parameter_list(
                Some(node_as_method_declaration.parameters()),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .unwrap(),
            None,
            try_visit_function_body(
                node_as_method_declaration.maybe_body(),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?,
        );
        if updated != node {
            set_comment_range(updated, &*node.ref_(self), self);
            set_source_map_range(
                updated,
                Some(self.alloc_source_map_range((&move_range_past_decorators(node, self)).into())),
                self,
            );
        }
        Ok(Some(updated.into()))
    }

    pub(super) fn should_emit_accessor_declaration(
        &self,
        node: Id<Node>, /*AccessorDeclaration*/
    ) -> bool {
        !(node_is_missing(node.ref_(self).as_function_like_declaration().maybe_body().refed(self).as_deref())
            && has_syntactic_modifier(node, ModifierFlags::Abstract, self))
    }

    pub(super) fn visit_get_accessor(
        &self,
        node: Id<Node>, /*GetAccessorDeclaration*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_get_accessor_declaration = node_ref.as_get_accessor_declaration();
        if !self.should_emit_accessor_declaration(node) {
            return Ok(None);
        }
        let updated = self.factory.ref_(self).update_get_accessor_declaration(
            node,
            Option::<Id<NodeArray>>::None,
            maybe_visit_nodes(
                node.ref_(self).maybe_modifiers(),
                Some(|node: Id<Node>| self.modifier_visitor(node)),
                Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                None,
                None,
                self,
            ),
            self.visit_property_name_of_class_element(node)?,
            try_visit_parameter_list(
                Some(node_as_get_accessor_declaration.parameters()),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .unwrap(),
            None,
            Some(
                try_visit_function_body(
                    node_as_get_accessor_declaration.maybe_body(),
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )?
                .unwrap_or_else(|| self.factory.ref_(self).create_block(vec![], None)),
            ),
        );
        if updated != node {
            set_comment_range(updated, &*node.ref_(self), self);
            set_source_map_range(
                updated,
                Some(self.alloc_source_map_range((&move_range_past_decorators(node, self)).into())),
                self,
            );
        }
        Ok(Some(updated.into()))
    }

    pub(super) fn visit_set_accessor(
        &self,
        node: Id<Node>, /*SetAccessorDeclaration*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_set_accessor_declaration = node_ref.as_set_accessor_declaration();
        if !self.should_emit_accessor_declaration(node) {
            return Ok(None);
        }
        let updated = self.factory.ref_(self).update_set_accessor_declaration(
            node,
            Option::<Id<NodeArray>>::None,
            maybe_visit_nodes(
                node.ref_(self).maybe_modifiers(),
                Some(|node: Id<Node>| self.modifier_visitor(node)),
                Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                None,
                None,
                self,
            ),
            self.visit_property_name_of_class_element(node)?,
            try_visit_parameter_list(
                Some(node_as_set_accessor_declaration.parameters()),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .unwrap(),
            Some(
                try_visit_function_body(
                    node_as_set_accessor_declaration.maybe_body(),
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )?
                .unwrap_or_else(|| self.factory.ref_(self).create_block(vec![], None)),
            ),
        );
        if updated != node {
            set_comment_range(updated, &*node.ref_(self), self);
            set_source_map_range(
                updated,
                Some(self.alloc_source_map_range((&move_range_past_decorators(node, self)).into())),
                self,
            );
        }
        Ok(Some(updated.into()))
    }

    pub(super) fn visit_function_declaration(
        &self,
        node: Id<Node>, /*FunctionDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_function_declaration = node_ref.as_function_declaration();
        if !self.should_emit_function_like_declaration(node) {
            return Ok(Some(
                self.factory
                    .ref_(self).create_not_emitted_statement(node)
                    .into(),
            ));
        }
        let updated = self.factory.ref_(self).update_function_declaration(
            node,
            Option::<Id<NodeArray>>::None,
            maybe_visit_nodes(
                node.ref_(self).maybe_modifiers(),
                Some(|node: Id<Node>| self.modifier_visitor(node)),
                Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                None,
                None,
                self,
            ),
            node_as_function_declaration.maybe_asterisk_token(),
            node_as_function_declaration.maybe_name(),
            Option::<Id<NodeArray>>::None,
            try_visit_parameter_list(
                Some(node_as_function_declaration.parameters()),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .unwrap(),
            None,
            Some(
                try_visit_function_body(
                    node_as_function_declaration.maybe_body(),
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )?
                .unwrap_or_else(|| self.factory.ref_(self).create_block(vec![], None)),
            ),
        );
        if self.is_export_of_namespace(node) {
            let mut statements: Vec<Id<Node /*Statement*/>> = vec![updated];
            self.add_export_member_assignment(&mut statements, node);
            return Ok(Some(statements.into()));
        }
        Ok(Some(updated.into()))
    }

    pub(super) fn visit_function_expression(
        &self,
        node: Id<Node>, /*FunctionExpression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_ref = node.ref_(self);
        let node_as_function_expression = node_ref.as_function_expression();
        if !self.should_emit_function_like_declaration(node) {
            return Ok(self.factory.ref_(self).create_omitted_expression());
        }
        let updated = self.factory.ref_(self).update_function_expression(
            node,
            maybe_visit_nodes(
                node.ref_(self).maybe_modifiers(),
                Some(|node: Id<Node>| self.modifier_visitor(node)),
                Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                None,
                None,
                self,
            ),
            node_as_function_expression.maybe_asterisk_token(),
            node_as_function_expression.maybe_name(),
            Option::<Id<NodeArray>>::None,
            try_visit_parameter_list(
                Some(node_as_function_expression.parameters()),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .unwrap(),
            None,
            try_visit_function_body(
                node_as_function_expression.maybe_body(),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .unwrap_or_else(|| self.factory.ref_(self).create_block(vec![], None)),
        );
        Ok(updated)
    }

    pub(super) fn visit_arrow_function(
        &self,
        node: Id<Node>, /*ArrowFunction*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_arrow_function = node_ref.as_arrow_function();
        let updated = self.factory.ref_(self).update_arrow_function(
            node,
            maybe_visit_nodes(
                node.ref_(self).maybe_modifiers(),
                Some(|node: Id<Node>| self.modifier_visitor(node)),
                Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                None,
                None,
                self,
            ),
            Option::<Id<NodeArray>>::None,
            try_visit_parameter_list(
                Some(node_as_arrow_function.parameters()),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .unwrap(),
            None,
            node_as_arrow_function.equals_greater_than_token.clone(),
            try_visit_function_body(
                node_as_arrow_function.maybe_body(),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .unwrap(),
        );
        Ok(Some(updated.into()))
    }

    pub(super) fn visit_parameter(
        &self,
        node: Id<Node>, /*ParameterDeclaration*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_parameter_declaration = node_ref.as_parameter_declaration();
        if parameter_is_this_keyword(node, self) {
            return Ok(None);
        }

        let updated = self.factory.ref_(self).update_parameter_declaration(
            node,
            Option::<Id<NodeArray>>::None,
            Option::<Id<NodeArray>>::None,
            node_as_parameter_declaration.dot_dot_dot_token.clone(),
            try_maybe_visit_node(
                node_as_parameter_declaration.maybe_name(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node: Id<Node>| is_binding_name(&node.ref_(self))),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            None,
            None,
            try_maybe_visit_node(
                node_as_parameter_declaration.maybe_initializer(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
        );
        if updated != node {
            set_comment_range(updated, &*node.ref_(self), self);
            set_text_range(
                &*updated.ref_(self),
                Some(&move_range_past_modifiers(node, self).into_readonly_text_range()),
            );
            set_source_map_range(
                updated,
                Some(self.alloc_source_map_range((&move_range_past_modifiers(node, self)).into())),
                self,
            );
            set_emit_flags(
                updated.ref_(self).as_parameter_declaration().name(),
                EmitFlags::NoTrailingSourceMap,
                self,
            );
        }
        Ok(Some(updated.into()))
    }

    pub(super) fn visit_variable_statement(
        &self,
        node: Id<Node>, /*VariableStatement*/
    ) -> io::Result<Option<Id<Node /*Statement*/>>> {
        let node_ref = node.ref_(self);
        let node_as_variable_statement = node_ref.as_variable_statement();
        Ok(if self.is_export_of_namespace(node) {
            let variables = get_initialized_variables(node_as_variable_statement.declaration_list, self);
            if variables.is_empty() {
                return Ok(None);
            }

            Some(
                self.factory
                    .ref_(self).create_expression_statement(
                        self.factory
                            .ref_(self).inline_expressions(&try_map(&variables, |&variable: &Id<Node>, _| {
                                self.transform_initialized_variable(variable)
                            })?),
                    )
                    .set_text_range(Some(&*node.ref_(self)), self),
            )
        } else {
            try_maybe_visit_each_child(
                Some(node),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
        })
    }

    pub(super) fn transform_initialized_variable(
        &self,
        node: Id<Node>, /*InitializedVariableDeclaration*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_ref = node.ref_(self);
        let node_as_variable_declaration = node_ref.as_variable_declaration();
        let name = node_as_variable_declaration.name();
        Ok(if is_binding_pattern(Some(&*name.ref_(self))) {
            try_flatten_destructuring_assignment(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                self.context.clone(),
                FlattenLevel::All,
                Some(false),
                Some(
                    |export_name: Id<Node>,
                     export_value: Id<Node>,
                     location: Option<&dyn ReadonlyTextRange>| {
                        Ok(self.create_namespace_export_expression(
                            export_name,
                            export_value,
                            location,
                        ))
                    },
                ),
                self,
            )?
        } else {
            self.factory
                .ref_(self).create_assignment(
                    self.get_namespace_member_name_with_source_maps_and_without_comments(name),
                    try_visit_node(
                        node_as_variable_declaration.maybe_initializer().unwrap(),
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                )
                .set_text_range(Some(&*node.ref_(self)), self)
        })
    }

    pub(super) fn visit_variable_declaration(
        &self,
        node: Id<Node>, /*VariableDeclaration*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_variable_declaration = node_ref.as_variable_declaration();
        Ok(Some(
            self.factory
                .ref_(self).update_variable_declaration(
                    node,
                    try_maybe_visit_node(
                        node_as_variable_declaration.maybe_name(),
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node: Id<Node>| is_binding_name(&node.ref_(self))),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    None,
                    None,
                    try_maybe_visit_node(
                        node_as_variable_declaration.maybe_initializer(),
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_parenthesized_expression(
        &self,
        node: Id<Node>, /*ParenthesizedExpression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_ref = node.ref_(self);
        let node_as_parenthesized_expression = node_ref.as_parenthesized_expression();
        let inner_expression = skip_outer_expressions(
            node_as_parenthesized_expression.expression,
            Some(!OuterExpressionKinds::Assertions),
            self,
        );
        if is_assertion_expression(&inner_expression.ref_(self)) {
            let expression = try_visit_node(
                node_as_parenthesized_expression.expression,
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?;

            if get_leading_comment_ranges_of_node(&expression.ref_(self), &self.current_source_file().ref_(self))
                .is_non_empty()
            {
                return Ok(self
                    .factory
                    .ref_(self).update_parenthesized_expression(node, expression));
            }
            return Ok(self
                .factory
                .ref_(self).create_partially_emitted_expression(expression, Some(node)));
        }

        try_visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self)
    }

    pub(super) fn visit_assertion_expression(
        &self,
        node: Id<Node>, /*AssertionExpression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let expression = try_visit_node(
            node.ref_(self).as_has_expression().expression(),
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;
        Ok(self
            .factory
            .ref_(self).create_partially_emitted_expression(expression, Some(node)))
    }

    pub(super) fn visit_non_null_expression(
        &self,
        node: Id<Node>, /*NonNullExpression*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let expression = try_visit_node(
            node.ref_(self).as_non_null_expression().expression,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_left_hand_side_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;
        Ok(self
            .factory
            .ref_(self).create_partially_emitted_expression(expression, Some(node)))
    }

    pub(super) fn visit_call_expression(
        &self,
        node: Id<Node>, /*CallExpression*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_call_expression = node_ref.as_call_expression();
        Ok(Some(
            self.factory
                .ref_(self).update_call_expression(
                    node,
                    try_visit_node(
                        node_as_call_expression.expression,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    Option::<Id<NodeArray>>::None,
                    try_visit_nodes(
                        node_as_call_expression.arguments,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        None,
                        None,
                        self,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_new_expression(
        &self,
        node: Id<Node>, /*NewExpression*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_new_expression = node_ref.as_new_expression();
        Ok(Some(
            self.factory
                .ref_(self).update_new_expression(
                    node,
                    try_visit_node(
                        node_as_new_expression.expression,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    Option::<Id<NodeArray>>::None,
                    try_maybe_visit_nodes(
                        node_as_new_expression.arguments,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        None,
                        None,
                        self,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_tagged_template_expression(
        &self,
        node: Id<Node>, /*TaggedTemplateExpression*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_tagged_template_expression = node_ref.as_tagged_template_expression();
        Ok(Some(
            self.factory
                .ref_(self).update_tagged_template_expression(
                    node,
                    try_visit_node(
                        node_as_tagged_template_expression.tag,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    Option::<Id<NodeArray>>::None,
                    try_visit_node(
                        node_as_tagged_template_expression.template,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_jsx_self_closing_element(
        &self,
        node: Id<Node>, /*JsxSelfClosingElement*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_jsx_self_closing_element = node_ref.as_jsx_self_closing_element();
        Ok(Some(
            self.factory
                .ref_(self).update_jsx_self_closing_element(
                    node,
                    try_visit_node(
                        node_as_jsx_self_closing_element.tag_name,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node: Id<Node>| is_jsx_tag_name_expression(&node.ref_(self))),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    Option::<Id<NodeArray>>::None,
                    try_visit_node(
                        node_as_jsx_self_closing_element.attributes,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node: Id<Node>| is_jsx_attributes(&node.ref_(self))),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_jsx_jsx_opening_element(
        &self,
        node: Id<Node>, /*JsxOpeningElement*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_jsx_opening_element = node_ref.as_jsx_opening_element();
        Ok(Some(
            self.factory
                .ref_(self).update_jsx_opening_element(
                    node,
                    try_visit_node(
                        node_as_jsx_opening_element.tag_name,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node: Id<Node>| is_jsx_tag_name_expression(&node.ref_(self))),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    Option::<Id<NodeArray>>::None,
                    try_visit_node(
                        node_as_jsx_opening_element.attributes,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node: Id<Node>| is_jsx_attributes(&node.ref_(self))),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn should_emit_enum_declaration(
        &self,
        node: Id<Node>, /*EnumDeclaration*/
    ) -> bool {
        !is_enum_const(node, self) || should_preserve_const_enums(&self.compiler_options.ref_(self))
    }

    pub(super) fn visit_enum_declaration(
        &self,
        node: Id<Node>, /*EnumDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        if !self.should_emit_enum_declaration(node) {
            return Ok(Some(
                self.factory
                    .ref_(self).create_not_emitted_statement(node)
                    .into(),
            ));
        }

        let mut statements: Vec<Id<Node /*Statement*/>> = Default::default();

        let mut emit_flags = EmitFlags::AdviseOnEmitNode;

        let var_added = self.add_var_for_enum_or_module_declaration(&mut statements, node);
        if var_added {
            if self.module_kind != ModuleKind::System
                || self.maybe_current_lexical_scope() != self.maybe_current_source_file()
            {
                emit_flags |= EmitFlags::NoLeadingComments;
            }
        }

        let parameter_name = self.get_namespace_parameter_name(node);

        let container_name = self.get_namespace_container_name(node);

        let export_name = if has_syntactic_modifier(node, ModifierFlags::Export, self) {
            self.factory.ref_(self).get_external_module_or_namespace_export_name(
                self.maybe_current_namespace_container_name(),
                node,
                Some(false),
                Some(true),
            )
        } else {
            self.factory.ref_(self).get_local_name(node, Some(false), Some(true))
        };

        let mut module_arg = self.factory.ref_(self).create_logical_or(
            export_name.clone(),
            self.factory.ref_(self).create_assignment(
                export_name,
                self.factory
                    .ref_(self).create_object_literal_expression(Option::<Id<NodeArray>>::None, None),
            ),
        );

        if self.has_namespace_qualified_export_name(node) {
            let local_name = self.factory.ref_(self).get_local_name(node, Some(false), Some(true));

            module_arg = self.factory.ref_(self).create_assignment(local_name, module_arg);
        }

        let enum_statement = self
            .factory
            .ref_(self).create_expression_statement(self.factory.ref_(self).create_call_expression(
                self.factory.ref_(self).create_function_expression(
                    Option::<Id<NodeArray>>::None,
                    None,
                    Option::<Id<Node>>::None,
                    Option::<Id<NodeArray>>::None,
                    Some(vec![self.factory.ref_(self).create_parameter_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        None,
                        Some(parameter_name),
                        None,
                        None,
                        None,
                    )]),
                    None,
                    self.transform_enum_body(node, container_name)?,
                ),
                Option::<Id<NodeArray>>::None,
                Some(vec![module_arg]),
            ))
            .set_original_node(Some(node), self);
        if var_added {
            set_synthetic_leading_comments(enum_statement, None, self);
            set_synthetic_trailing_comments(enum_statement, None, self);
        }
        set_text_range(&*enum_statement.ref_(self), Some(&*node.ref_(self)));
        add_emit_flags(enum_statement, emit_flags, self);
        statements.push(enum_statement);

        statements.push(
            self.factory
                .ref_(self).create_end_of_declaration_marker(node),
        );
        Ok(Some(statements.into()))
    }

    pub(super) fn transform_enum_body(
        &self,
        node: Id<Node>,       /*EnumDeclaration*/
        local_name: Id<Node>, /*Identifier*/
    ) -> io::Result<Id<Node /*Block*/>> {
        let node_ref = node.ref_(self);
        let node_as_enum_declaration = node_ref.as_enum_declaration();
        let saved_current_namespace_local_name = self.maybe_current_namespace_container_name();
        self.set_current_namespace_container_name(Some(local_name));

        let mut statements: Vec<Id<Node /*Statement*/>> = Default::default();
        self.context.ref_(self).start_lexical_environment();
        let members = try_map(&*node_as_enum_declaration.members.ref_(self), |&member: &Id<Node>, _| {
            self.transform_enum_member(member)
        })?;
        insert_statements_after_standard_prologue(
            &mut statements,
            self.context.ref_(self).end_lexical_environment().as_deref(),
            self,
        );
        add_range(&mut statements, Some(&members), None, None);

        self.set_current_namespace_container_name(saved_current_namespace_local_name);
        Ok(self.factory.ref_(self).create_block(
            self.factory
                .ref_(self).create_node_array(Some(statements), None)
                .set_text_range(Some(&*node_as_enum_declaration.members.ref_(self)), self),
            Some(true),
        ))
    }

    pub(super) fn transform_enum_member(
        &self,
        member: Id<Node>, /*EnumMember*/
    ) -> io::Result<Id<Node /*Statement*/>> {
        let name = self.get_expression_for_property_name(member, false);
        let value_expression = self.transform_enum_member_declaration_value(member)?;
        let inner_assignment = self.factory.ref_(self).create_assignment(
            self.factory.ref_(self).create_element_access_expression(
                self.current_namespace_container_name(),
                name.clone(),
            ),
            value_expression.clone(),
        );
        let outer_assignment = if value_expression.ref_(self).kind() == SyntaxKind::StringLiteral {
            inner_assignment
        } else {
            self.factory.ref_(self).create_assignment(
                self.factory.ref_(self).create_element_access_expression(
                    self.current_namespace_container_name(),
                    inner_assignment,
                ),
                name,
            )
        };
        Ok(self
            .factory
            .ref_(self).create_expression_statement(set_text_range_id_node(outer_assignment, Some(&*member.ref_(self)), self))
            .set_text_range(Some(&*member.ref_(self)), self))
    }

    pub(super) fn transform_enum_member_declaration_value(
        &self,
        member: Id<Node>, /*EnumMember*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let value = self.resolver.ref_(self).get_constant_value(member)?;
        Ok(if let Some(value) = value {
            match value {
                StringOrNumber::String(value) => {
                    self.factory.ref_(self).create_string_literal(value, None, None)
                }
                StringOrNumber::Number(value) => self.factory.ref_(self).create_numeric_literal(value, None),
            }
        } else {
            self.enable_substitution_for_non_qualified_enum_members();
            if let Some(member_initializer) = member.ref_(self).as_enum_member().initializer {
                try_visit_node(
                    member_initializer,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?
            } else {
                self.factory.ref_(self).create_void_zero()
            }
        })
    }

    pub(super) fn should_emit_module_declaration(
        &self,
        node_in: Id<Node>, /*ModuleDeclaration*/
    ) -> bool {
        let Some(node) = get_parse_tree_node(Some(node_in), Some(|node: Id<Node>| is_module_declaration(&node.ref_(self))), self) else {
            return true;
        };
        is_instantiated_module(node, should_preserve_const_enums(&self.compiler_options.ref_(self)), self)
    }
}
