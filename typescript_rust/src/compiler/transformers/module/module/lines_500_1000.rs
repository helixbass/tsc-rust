use std::io;

use id_arena::Id;

use super::TransformModule;
use crate::{
    is_array_literal_expression, is_declaration_name_of_enum_or_namespace,
    is_destructuring_assignment, is_export_name, is_expression, is_for_initializer,
    is_generated_identifier, is_identifier, is_import_call, is_local_name,
    is_object_literal_expression, is_spread_element, length, Debug_, FlattenLevel,
    NamedDeclarationInterface, Node, NodeExt, NodeInterface, ReadonlyTextRange, SyntaxKind,
    TransformFlags, VisitResult, _d, first_or_undefined, get_emit_flags, get_es_module_interop,
    get_export_needs_import_star_helper, get_external_module_name_literal,
    get_import_needs_import_default_helper, get_import_needs_import_star_helper,
    get_namespace_declaration_node, get_node_id, get_original_node_id, is_default_import,
    is_prefix_unary_expression, is_simple_copiable_expression, is_string_literal, released,
    set_emit_flags, single_or_many_node, try_flatten_destructuring_assignment,
    try_maybe_visit_node, try_visit_each_child, try_visit_iteration_body, try_visit_node,
    CoreTransformationContext, EmitFlags, GetOrInsertDefault, InArena, LiteralLikeNodeInterface,
    MapOrDefault, ModuleKind, NodeArray, NodeFlags, ScriptTarget,
};

impl TransformModule {
    pub(super) fn top_level_visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/ {
        Ok(match released!(node.ref_(self).kind()) {
            SyntaxKind::ImportDeclaration => self.visit_import_declaration(node)?,
            SyntaxKind::ImportEqualsDeclaration => self.visit_import_equals_declaration(node)?,
            SyntaxKind::ExportDeclaration => self.visit_export_declaration(node)?,
            SyntaxKind::ExportAssignment => self.visit_export_assignment(node)?,
            SyntaxKind::VariableStatement => self.visit_variable_statement(node)?,
            SyntaxKind::FunctionDeclaration => self.visit_function_declaration(node)?,
            SyntaxKind::ClassDeclaration => self.visit_class_declaration(node)?,
            SyntaxKind::MergeDeclarationMarker => self.visit_merge_declaration_marker(node),
            SyntaxKind::EndOfDeclarationMarker => self.visit_end_of_declaration_marker(node),
            _ => self.visitor(node)?,
        })
    }

    pub(super) fn visitor_worker(
        &self,
        node: Id<Node>,
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> /*<Node>*/ {
        if !node.ref_(self).transform_flags().intersects(
            TransformFlags::ContainsDynamicImport
                | TransformFlags::ContainsDestructuringAssignment
                | TransformFlags::ContainsUpdateExpressionForIdentifier,
        ) {
            return Ok(Some(node.into()));
        }

        match node.ref_(self).kind() {
            SyntaxKind::ForStatement => return self.visit_for_statement(node),
            SyntaxKind::ExpressionStatement => return self.visit_expression_statement(node),
            SyntaxKind::ParenthesizedExpression => {
                return self.visit_parenthesized_expression(node, value_is_discarded)
            }
            SyntaxKind::PartiallyEmittedExpression => {
                return self.visit_partially_emitted_expression(node, value_is_discarded)
            }
            SyntaxKind::CallExpression => {
                if is_import_call(node, self)
                    && self
                        .current_source_file()
                        .ref_(self)
                        .as_source_file()
                        .maybe_implied_node_format()
                        .is_none()
                {
                    return Ok(Some(self.visit_import_call_expression(node)?.into()));
                }
            }
            SyntaxKind::BinaryExpression => {
                if is_destructuring_assignment(node, self) {
                    return Ok(Some(
                        self.visit_destructuring_assignment(node, value_is_discarded)?
                            .into(),
                    ));
                }
            }
            SyntaxKind::PrefixUnaryExpression | SyntaxKind::PostfixUnaryExpression => {
                return self.visit_pre_or_postfix_unary_expression(node, value_is_discarded)
            }
            _ => (),
        }

        Ok(Some(
            try_visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .into(),
        ))
    }

    pub(super) fn visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/ {
        self.visitor_worker(node, false)
    }

    pub(super) fn discarded_value_visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/
    {
        self.visitor_worker(node, true)
    }

    pub(super) fn destructuring_needs_flattening(
        &self,
        node: Id<Node>, /*Expression*/
    ) -> io::Result<bool> {
        if is_object_literal_expression(&node.ref_(self)) {
            for elem in &*node
                .ref_(self)
                .as_object_literal_expression()
                .properties
                .ref_(self)
            {
                match elem.ref_(self).kind() {
                    SyntaxKind::PropertyAssignment => {
                        if self.destructuring_needs_flattening(
                            elem.ref_(self).as_property_assignment().initializer,
                        )? {
                            return Ok(true);
                        }
                    }
                    SyntaxKind::ShorthandPropertyAssignment => {
                        if self.destructuring_needs_flattening(
                            elem.ref_(self).as_shorthand_property_assignment().name(),
                        )? {
                            return Ok(true);
                        }
                    }
                    SyntaxKind::SpreadAssignment => {
                        if self.destructuring_needs_flattening(
                            elem.ref_(self).as_spread_assignment().expression,
                        )? {
                            return Ok(true);
                        }
                    }
                    SyntaxKind::MethodDeclaration
                    | SyntaxKind::GetAccessor
                    | SyntaxKind::SetAccessor => return Ok(false),
                    _ => Debug_.assert_never(elem, Some("Unhandled object member kind")),
                }
            }
        } else if is_array_literal_expression(&node.ref_(self)) {
            for &elem in &*node
                .ref_(self)
                .as_array_literal_expression()
                .elements
                .ref_(self)
            {
                if is_spread_element(&elem.ref_(self)) {
                    if self.destructuring_needs_flattening(
                        elem.ref_(self).as_spread_element().expression,
                    )? {
                        return Ok(true);
                    }
                } else if self.destructuring_needs_flattening(elem)? {
                    return Ok(true);
                }
            }
        } else if is_identifier(&node.ref_(self)) {
            return Ok(length(self.get_exports(node)?.as_deref())
                > if is_export_name(node, self) { 1 } else { 0 });
        }
        Ok(false)
    }

    pub(super) fn visit_destructuring_assignment(
        &self,
        node: Id<Node>, /*DestructuringAssignment*/
        value_is_discarded: bool,
    ) -> io::Result<Id<Node /*Expression*/>> {
        if self.destructuring_needs_flattening(node.ref_(self).as_binary_expression().left)? {
            return try_flatten_destructuring_assignment(
                node,
                Some(|node: Id<Node>| self.visitor(node)),
                self.context.clone(),
                FlattenLevel::All,
                Some(!value_is_discarded),
                Some(
                    |a: Id<Node>, b: Id<Node>, c: Option<&dyn ReadonlyTextRange>| {
                        self.create_all_export_expressions(a, b, c)
                    },
                ),
                self,
            );
        }
        try_visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }

    pub(super) fn visit_for_statement(
        &self,
        node: Id<Node>, /*ForStatement*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_for_statement = node_ref.as_for_statement();
        Ok(Some(
            self.factory
                .ref_(self)
                .update_for_statement(
                    node,
                    try_maybe_visit_node(
                        node_as_for_statement.initializer,
                        Some(|node: Id<Node>| self.discarded_value_visitor(node)),
                        Some(|node| is_for_initializer(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    try_maybe_visit_node(
                        node_as_for_statement.condition,
                        Some(|node: Id<Node>| self.visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    try_maybe_visit_node(
                        node_as_for_statement.incrementor,
                        Some(|node: Id<Node>| self.discarded_value_visitor(node)),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                    try_visit_iteration_body(
                        node_as_for_statement.statement,
                        |node: Id<Node>| self.visitor(node),
                        &*self.context.ref_(self),
                        self,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_expression_statement(
        &self,
        node: Id<Node>, /*ExpressionStatement*/
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_expression_statement = node_ref.as_expression_statement();
        Ok(Some(
            self.factory
                .ref_(self)
                .update_expression_statement(
                    node,
                    try_visit_node(
                        node_as_expression_statement.expression,
                        Some(|node: Id<Node>| self.discarded_value_visitor(node)),
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
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_parenthesized_expression = node_ref.as_parenthesized_expression();
        Ok(Some(
            self.factory
                .ref_(self)
                .update_parenthesized_expression(
                    node,
                    try_visit_node(
                        node_as_parenthesized_expression.expression,
                        Some(|node: Id<Node>| {
                            if value_is_discarded {
                                self.discarded_value_visitor(node)
                            } else {
                                self.visitor(node)
                            }
                        }),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_partially_emitted_expression(
        &self,
        node: Id<Node>, /*PartiallyEmittedExpression*/
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_partially_emitted_expression = node_ref.as_partially_emitted_expression();
        Ok(Some(
            self.factory
                .ref_(self)
                .update_partially_emitted_expression(
                    node,
                    try_visit_node(
                        node_as_partially_emitted_expression.expression,
                        Some(|node: Id<Node>| {
                            if value_is_discarded {
                                self.discarded_value_visitor(node)
                            } else {
                                self.visitor(node)
                            }
                        }),
                        Some(|node| is_expression(node, self)),
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_pre_or_postfix_unary_expression(
        &self,
        node: Id<Node>, /*PrefixUnaryExpression | PostfixUnaryExpression*/
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> {
        let node_ref = node.ref_(self);
        let node_as_unary_expression = node_ref.as_unary_expression();
        if matches!(
            node_as_unary_expression.operator(),
            SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
        ) && is_identifier(&node_as_unary_expression.operand().ref_(self))
            && !is_generated_identifier(&node_as_unary_expression.operand().ref_(self))
            && !is_local_name(node_as_unary_expression.operand(), self)
            && !is_declaration_name_of_enum_or_namespace(node_as_unary_expression.operand(), self)
        {
            let exported_names = self.get_exports(node_as_unary_expression.operand())?;
            if let Some(exported_names) = exported_names {
                let mut temp: Option<Id<Node /*Identifier*/>> = _d();
                let mut expression/*: Expression*/ = try_visit_node(
                    node_as_unary_expression.operand(),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node| is_expression(node, self)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?;
                if is_prefix_unary_expression(&node.ref_(self)) {
                    expression = self
                        .factory
                        .ref_(self)
                        .update_prefix_unary_expression(node, expression);
                } else {
                    expression = self
                        .factory
                        .ref_(self)
                        .update_postfix_unary_expression(node, expression);
                    if !value_is_discarded {
                        temp = Some(self.factory.ref_(self).create_temp_variable(
                            Some(|node: Id<Node>| {
                                self.context.ref_(self).hoist_variable_declaration(node);
                            }),
                            None,
                        ));
                        expression = self
                            .factory
                            .ref_(self)
                            .create_assignment(temp.clone().unwrap(), expression)
                            .set_text_range(Some(&*node.ref_(self)), self);
                    }
                }

                for &export_name in &exported_names {
                    self.no_substitution_mut()
                        .insert(get_node_id(&expression.ref_(self)), true);
                    expression = self
                        .create_export_expression(
                            export_name,
                            expression,
                            Option::<&Node>::None,
                            None,
                        )
                        .set_text_range(Some(&*node.ref_(self)), self);
                }

                if let Some(temp) = temp {
                    self.no_substitution_mut()
                        .insert(get_node_id(&expression.ref_(self)), true);
                    expression = self
                        .factory
                        .ref_(self)
                        .create_comma(expression, temp)
                        .set_text_range(Some(&*node.ref_(self)), self);
                }
                return Ok(Some(expression.into()));
            }
        }

        Ok(Some(
            try_visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .into(),
        ))
    }

    pub(super) fn visit_import_call_expression(
        &self,
        node: Id<Node>, /*ImportCall*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        let node_ref = node.ref_(self);
        let node_as_call_expression = node_ref.as_call_expression();
        let external_module_name = get_external_module_name_literal(
            &self.factory.ref_(self),
            node,
            self.current_source_file(),
            &**self.host.ref_(self),
            &**self.resolver.ref_(self),
            &self.compiler_options.ref_(self),
        )?;
        let first_argument = try_maybe_visit_node(
            first_or_undefined(&node_as_call_expression.arguments.ref_(self)).cloned(),
            Some(|node: Id<Node>| self.visitor(node)),
            Option::<fn(Id<Node>) -> bool>::None,
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;
        let argument = external_module_name
            .filter(|external_module_name| match first_argument.as_ref() {
                None => true,
                Some(first_argument) => {
                    !is_string_literal(&first_argument.ref_(self))
                        || *first_argument.ref_(self).as_string_literal().text()
                            != *external_module_name.ref_(self).as_string_literal().text()
                }
            })
            .or(first_argument);
        let contains_lexical_this = node
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsLexicalThis);
        Ok(match self.compiler_options.ref_(self).module {
            Some(ModuleKind::AMD) => {
                self.create_import_call_expression_amd(argument, contains_lexical_this)
            }
            Some(ModuleKind::UMD) => self.create_import_call_expression_umd(
                argument.unwrap_or_else(|| self.factory.ref_(self).create_void_zero()),
                contains_lexical_this,
            ),
            _ => self.create_import_call_expression_common_js(argument, contains_lexical_this),
        })
    }

    pub(super) fn create_import_call_expression_umd(
        &self,
        arg: Id<Node /*Expression*/>,
        contains_lexical_this: bool,
    ) -> Id<Node /*Expression*/> {
        self.set_need_umd_dynamic_import_helper(true);
        if is_simple_copiable_expression(&arg.ref_(self)) {
            let arg_clone = if is_generated_identifier(&arg.ref_(self)) {
                arg.clone()
            } else if is_string_literal(&arg.ref_(self)) {
                self.factory.ref_(self).create_string_literal_from_node(arg)
            } else {
                self.factory
                    .ref_(self)
                    .clone_node(arg)
                    .set_text_range(Some(&*arg.ref_(self)), self)
                    .set_emit_flags(EmitFlags::NoComments, self)
            };
            self.factory.ref_(self).create_conditional_expression(
                self.factory.ref_(self).create_identifier("__syncRequire"),
                None,
                self.create_import_call_expression_common_js(Some(arg), contains_lexical_this),
                None,
                self.create_import_call_expression_amd(Some(arg_clone), contains_lexical_this),
            )
        } else {
            let temp = self.factory.ref_(self).create_temp_variable(
                Some(|node: Id<Node>| {
                    self.context.ref_(self).hoist_variable_declaration(node);
                }),
                None,
            );
            self.factory.ref_(self).create_comma(
                self.factory.ref_(self).create_assignment(temp.clone(), arg),
                self.factory.ref_(self).create_conditional_expression(
                    self.factory.ref_(self).create_identifier("__syncRequire"),
                    None,
                    self.create_import_call_expression_common_js(
                        Some(temp.clone()),
                        contains_lexical_this,
                    ),
                    None,
                    self.create_import_call_expression_amd(Some(temp), contains_lexical_this),
                ),
            )
        }
    }

    pub(super) fn create_import_call_expression_amd(
        &self,
        arg: Option<Id<Node /*Expression*/>>,
        contains_lexical_this: bool,
    ) -> Id<Node /*Expression*/> {
        let resolve = self.factory.ref_(self).create_unique_name("resolve", None);
        let reject = self.factory.ref_(self).create_unique_name("reject", None);
        let parameters = vec![
            self.factory.ref_(self).create_parameter_declaration(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                None,
                Some(resolve.clone()),
                None,
                None,
                None,
            ),
            self.factory.ref_(self).create_parameter_declaration(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                None,
                Some(reject.clone()),
                None,
                None,
                None,
            ),
        ];
        let body = self.factory.ref_(self).create_block(
            vec![self.factory.ref_(self).create_expression_statement(
                self.factory.ref_(self).create_call_expression(
                    self.factory.ref_(self).create_identifier("require"),
                    Option::<Id<NodeArray>>::None,
                    Some(vec![
                        self.factory.ref_(self).create_array_literal_expression(
                            Some(vec![arg.unwrap_or_else(|| {
                                self.factory.ref_(self).create_omitted_expression()
                            })]),
                            None,
                        ),
                        resolve,
                        reject,
                    ]),
                ),
            )],
            None,
        );

        let func: Id<Node /*FunctionExpression | ArrowFunction*/>;
        if self.language_version >= ScriptTarget::ES2015 {
            func = self.factory.ref_(self).create_arrow_function(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                parameters,
                None,
                None,
                body,
            )
        } else {
            func = self.factory.ref_(self).create_function_expression(
                Option::<Id<NodeArray>>::None,
                None,
                Option::<Id<Node>>::None,
                Option::<Id<NodeArray>>::None,
                Some(parameters),
                None,
                body,
            );

            if contains_lexical_this {
                set_emit_flags(func, EmitFlags::CapturesThis, self);
            }
        }

        let promise = self.factory.ref_(self).create_new_expression(
            self.factory.ref_(self).create_identifier("Promise"),
            Option::<Id<NodeArray>>::None,
            Some(vec![func]),
        );
        if get_es_module_interop(&self.compiler_options.ref_(self)) == Some(true) {
            return self.factory.ref_(self).create_call_expression(
                self.factory.ref_(self).create_property_access_expression(
                    promise,
                    self.factory.ref_(self).create_identifier("then"),
                ),
                Option::<Id<NodeArray>>::None,
                Some(vec![self
                    .emit_helpers()
                    .create_import_star_callback_helper()]),
            );
        }
        promise
    }

    pub(super) fn create_import_call_expression_common_js(
        &self,
        arg: Option<Id<Node /*Expression*/>>,
        contains_lexical_this: bool,
    ) -> Id<Node /*Expression*/> {
        let promise_resolve_call = self.factory.ref_(self).create_call_expression(
            self.factory.ref_(self).create_property_access_expression(
                self.factory.ref_(self).create_identifier("Promise"),
                "resolve",
            ),
            Option::<Id<NodeArray>>::None,
            Some(vec![]),
        );
        let mut require_call/*: Expression*/ = self.factory.ref_(self).create_call_expression(
            self.factory.ref_(self).create_identifier("require"),
            Option::<Id<NodeArray>>::None,
            Some(arg.map_or_default(|arg| vec![arg]))
        );
        if get_es_module_interop(&self.compiler_options.ref_(self)) == Some(true) {
            require_call = self.emit_helpers().create_import_star_helper(require_call);
        }

        let func: Id<Node /*FunctionExpression | ArrowFunction*/>;
        if self.language_version >= ScriptTarget::ES2015 {
            func = self.factory.ref_(self).create_arrow_function(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                vec![],
                None,
                None,
                require_call,
            );
        } else {
            func = self.factory.ref_(self).create_function_expression(
                Option::<Id<NodeArray>>::None,
                None,
                Option::<Id<Node>>::None,
                Option::<Id<NodeArray>>::None,
                Some(vec![]),
                None,
                self.factory.ref_(self).create_block(
                    vec![self
                        .factory
                        .ref_(self)
                        .create_return_statement(Some(require_call))],
                    None,
                ),
            );

            if contains_lexical_this {
                set_emit_flags(func, EmitFlags::CapturesThis, self);
            }
        }

        self.factory.ref_(self).create_call_expression(
            self.factory
                .ref_(self)
                .create_property_access_expression(promise_resolve_call, "then"),
            Option::<Id<NodeArray>>::None,
            Some(vec![func]),
        )
    }

    pub(super) fn get_helper_expression_for_export(
        &self,
        node: Id<Node>, /*ExportDeclaration*/
        inner_expr: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        if get_es_module_interop(&self.compiler_options.ref_(self)) != Some(true)
            || get_emit_flags(node, self).intersects(EmitFlags::NeverApplyImportHelper)
        {
            return inner_expr;
        }
        if get_export_needs_import_star_helper(node, self) {
            return self.emit_helpers().create_import_star_helper(inner_expr);
        }
        inner_expr
    }

    pub(super) fn get_helper_expression_for_import(
        &self,
        node: Id<Node>, /*ImportDeclaration*/
        inner_expr: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        if get_es_module_interop(&self.compiler_options.ref_(self)) != Some(true)
            || get_emit_flags(node, self).intersects(EmitFlags::NeverApplyImportHelper)
        {
            return inner_expr;
        }
        if get_import_needs_import_star_helper(node, self) {
            return self.emit_helpers().create_import_star_helper(inner_expr);
        }
        if get_import_needs_import_default_helper(node, self) {
            return self.emit_helpers().create_import_default_helper(inner_expr);
        }
        inner_expr
    }

    pub(super) fn visit_import_declaration(
        &self,
        node: Id<Node>, /*ImportDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let mut statements: Option<Vec<Id<Node /*Statement*/>>> = _d();
        let namespace_declaration = get_namespace_declaration_node(node, self);
        if self.module_kind != ModuleKind::AMD {
            if node
                .ref_(self)
                .as_import_declaration()
                .import_clause
                .is_none()
            {
                return Ok(Some(
                    self.factory
                        .ref_(self)
                        .create_expression_statement(self.create_require_call(node)?)
                        .set_text_range(Some(&*node.ref_(self)), self)
                        .set_original_node(Some(node), self)
                        .into(),
                ));
            } else {
                let mut variables: Vec<Id<Node /*VariableDeclaration*/>> = _d();
                if let Some(namespace_declaration) =
                    namespace_declaration.filter(|_| !is_default_import(node, self))
                {
                    variables.push(
                        self.factory.ref_(self).create_variable_declaration(
                            Some(self.factory.ref_(self).clone_node(
                                released!(namespace_declaration
                                        .ref_(self)
                                        .as_named_declaration()
                                        .name()),
                            )),
                            None,
                            None,
                            Some(self.get_helper_expression_for_import(
                                node,
                                self.create_require_call(node)?,
                            )),
                        ),
                    );
                } else {
                    variables.push(
                        self.factory.ref_(self).create_variable_declaration(
                            Some(
                                self.factory
                                    .ref_(self)
                                    .get_generated_name_for_node(Some(node), None),
                            ),
                            None,
                            None,
                            Some(self.get_helper_expression_for_import(
                                node,
                                self.create_require_call(node)?,
                            )),
                        ),
                    );

                    if let Some(namespace_declaration) =
                        namespace_declaration.filter(|_| is_default_import(node, self))
                    {
                        variables.push(
                            self.factory.ref_(self).create_variable_declaration(
                                Some(
                                    self.factory.ref_(self).clone_node(
                                        namespace_declaration
                                            .ref_(self)
                                            .as_named_declaration()
                                            .name(),
                                    ),
                                ),
                                None,
                                None,
                                Some(
                                    self.factory
                                        .ref_(self)
                                        .get_generated_name_for_node(Some(node), None),
                                ),
                            ),
                        );
                    }
                }

                statements.get_or_insert_default_().push(
                    self.factory
                        .ref_(self)
                        .create_variable_statement(
                            Option::<Id<NodeArray>>::None,
                            self.factory.ref_(self).create_variable_declaration_list(
                                variables,
                                Some(
                                    (self.language_version >= ScriptTarget::ES2015)
                                        .then_some(NodeFlags::Const)
                                        .unwrap_or_default(),
                                ),
                            ),
                        )
                        .set_text_range(Some(&*node.ref_(self)), self)
                        .set_original_node(Some(node), self),
                );
            }
        } else if let Some(namespace_declaration) =
            namespace_declaration.filter(|_| is_default_import(node, self))
        {
            statements.get_or_insert_default_().push(
                self.factory.ref_(self).create_variable_statement(
                    Option::<Id<NodeArray>>::None,
                    self.factory.ref_(self).create_variable_declaration_list(
                        vec![self.factory.ref_(self).create_variable_declaration(
                            Some(
                                self.factory.ref_(self).clone_node(
                                    namespace_declaration
                                        .ref_(self)
                                        .as_named_declaration()
                                        .name(),
                                ),
                            ),
                            None,
                            None,
                            Some(
                                self.factory
                                    .ref_(self)
                                    .get_generated_name_for_node(Some(node), None),
                            ),
                        )],
                        Some(
                            (self.language_version >= ScriptTarget::ES2015)
                                .then_some(NodeFlags::Const)
                                .unwrap_or_default(),
                        ),
                    ),
                ),
            );
        }

        if self.has_associated_end_of_declaration_marker(node) {
            let id = get_original_node_id(node, self);
            self.deferred_exports_mut().insert(id, {
                self.append_exports_of_import_declaration(&mut statements, node);
                statements.clone()
            });
        } else {
            /*statements = */
            self.append_exports_of_import_declaration(&mut statements, node);
        }

        Ok(statements.map(single_or_many_node))
    }

    pub(super) fn create_require_call(
        &self,
        import_node: Id<Node>, /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/
    ) -> io::Result<Id<Node>> {
        let module_name = get_external_module_name_literal(
            &self.factory.ref_(self),
            import_node,
            self.current_source_file(),
            &**self.host.ref_(self),
            &**self.resolver.ref_(self),
            &self.compiler_options.ref_(self),
        )?;
        let mut args: Vec<Id<Node /*Expression*/>> = _d();
        if let Some(module_name) = module_name {
            args.push(module_name);
        }

        Ok(self.factory.ref_(self).create_call_expression(
            self.factory.ref_(self).create_identifier("require"),
            Option::<Id<NodeArray>>::None,
            Some(args),
        ))
    }
}
