use std::io;

use gc::Gc;

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
    is_prefix_unary_expression, is_simple_copiable_expression, is_string_literal, set_emit_flags,
    single_or_many_node, try_flatten_destructuring_assignment, try_maybe_visit_node,
    try_visit_each_child, try_visit_iteration_body, try_visit_node, EmitFlags, GetOrInsertDefault,
    LiteralLikeNodeInterface, MapOrDefault, ModuleKind, NodeArray, NodeFlags, ScriptTarget,
};

impl TransformModule {
    pub(super) fn top_level_visitor(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/ {
        Ok(match node.kind() {
            SyntaxKind::ImportDeclaration => self.visit_import_declaration(node),
            SyntaxKind::ImportEqualsDeclaration => self.visit_import_equals_declaration(node),
            SyntaxKind::ExportDeclaration => self.visit_export_declaration(node),
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
        node: &Node,
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> /*<Node>*/ {
        if !node.transform_flags().intersects(
            TransformFlags::ContainsDynamicImport
                | TransformFlags::ContainsDestructuringAssignment
                | TransformFlags::ContainsUpdateExpressionForIdentifier,
        ) {
            return Ok(Some(node.node_wrapper().into()));
        }

        match node.kind() {
            SyntaxKind::ForStatement => return self.visit_for_statement(node),
            SyntaxKind::ExpressionStatement => return self.visit_expression_statement(node),
            SyntaxKind::ParenthesizedExpression => {
                return self.visit_parenthesized_expression(node, value_is_discarded)
            }
            SyntaxKind::PartiallyEmittedExpression => {
                return self.visit_partially_emitted_expression(node, value_is_discarded)
            }
            SyntaxKind::CallExpression => {
                if is_import_call(node)
                    && self
                        .current_source_file()
                        .as_source_file()
                        .maybe_implied_node_format()
                        .is_none()
                {
                    return Ok(Some(self.visit_import_call_expression(node)?.into()));
                }
            }
            SyntaxKind::BinaryExpression => {
                if is_destructuring_assignment(node) {
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
            try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?.into(),
        ))
    }

    pub(super) fn visitor(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/ {
        self.visitor_worker(node, false)
    }

    pub(super) fn discarded_value_visitor(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/
    {
        self.visitor_worker(node, true)
    }

    pub(super) fn destructuring_needs_flattening(
        &self,
        node: &Node, /*Expression*/
    ) -> io::Result<bool> {
        if is_object_literal_expression(node) {
            for elem in &node.as_object_literal_expression().properties {
                match elem.kind() {
                    SyntaxKind::PropertyAssignment => {
                        if self.destructuring_needs_flattening(
                            &elem.as_property_assignment().initializer,
                        )? {
                            return Ok(true);
                        }
                    }
                    SyntaxKind::ShorthandPropertyAssignment => {
                        if self.destructuring_needs_flattening(
                            &elem.as_shorthand_property_assignment().name(),
                        )? {
                            return Ok(true);
                        }
                    }
                    SyntaxKind::SpreadAssignment => {
                        if self.destructuring_needs_flattening(
                            &elem.as_spread_assignment().expression,
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
        } else if is_array_literal_expression(node) {
            for elem in &node.as_array_literal_expression().elements {
                if is_spread_element(elem) {
                    if self.destructuring_needs_flattening(&elem.as_spread_element().expression)? {
                        return Ok(true);
                    }
                } else if self.destructuring_needs_flattening(elem)? {
                    return Ok(true);
                }
            }
        } else if is_identifier(node) {
            return Ok(length(self.get_exports(node)?.as_deref())
                > if is_export_name(node) { 1 } else { 0 });
        }
        Ok(false)
    }

    pub(super) fn visit_destructuring_assignment(
        &self,
        node: &Node, /*DestructuringAssignment*/
        value_is_discarded: bool,
    ) -> io::Result<Gc<Node /*Expression*/>> {
        if self.destructuring_needs_flattening(&node.as_binary_expression().left)? {
            return try_flatten_destructuring_assignment(
                node,
                Some(|node: &Node| self.visitor(node)),
                &**self.context,
                FlattenLevel::All,
                Some(!value_is_discarded),
                Some(|a: &Node, b: &Node, c: Option<&dyn ReadonlyTextRange>| {
                    self.create_all_export_expressions(a, b.node_wrapper(), c)
                }),
            );
        }
        try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn visit_for_statement(
        &self,
        node: &Node, /*ForStatement*/
    ) -> io::Result<VisitResult> {
        let node_as_for_statement = node.as_for_statement();
        Ok(Some(
            self.factory
                .update_for_statement(
                    node,
                    try_maybe_visit_node(
                        node_as_for_statement.initializer.as_deref(),
                        Some(|node: &Node| self.discarded_value_visitor(node)),
                        Some(is_for_initializer),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                    try_maybe_visit_node(
                        node_as_for_statement.condition.as_deref(),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                    try_maybe_visit_node(
                        node_as_for_statement.incrementor.as_deref(),
                        Some(|node: &Node| self.discarded_value_visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                    try_visit_iteration_body(
                        &node_as_for_statement.statement,
                        |node: &Node| self.visitor(node),
                        &**self.context,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_expression_statement(
        &self,
        node: &Node, /*ExpressionStatement*/
    ) -> io::Result<VisitResult> {
        let node_as_expression_statement = node.as_expression_statement();
        Ok(Some(
            self.factory
                .update_expression_statement(
                    node,
                    try_visit_node(
                        &node_as_expression_statement.expression,
                        Some(|node: &Node| self.discarded_value_visitor(node)),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_parenthesized_expression(
        &self,
        node: &Node, /*ParenthesizedExpression*/
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> {
        let node_as_parenthesized_expression = node.as_parenthesized_expression();
        Ok(Some(
            self.factory
                .update_parenthesized_expression(
                    node,
                    try_visit_node(
                        &node_as_parenthesized_expression.expression,
                        Some(|node: &Node| {
                            if value_is_discarded {
                                self.discarded_value_visitor(node)
                            } else {
                                self.visitor(node)
                            }
                        }),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_partially_emitted_expression(
        &self,
        node: &Node, /*PartiallyEmittedExpression*/
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> {
        let node_as_partially_emitted_expression = node.as_partially_emitted_expression();
        Ok(Some(
            self.factory
                .update_partially_emitted_expression(
                    node,
                    try_visit_node(
                        &node_as_partially_emitted_expression.expression,
                        Some(|node: &Node| {
                            if value_is_discarded {
                                self.discarded_value_visitor(node)
                            } else {
                                self.visitor(node)
                            }
                        }),
                        Some(is_expression),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?,
                )
                .into(),
        ))
    }

    pub(super) fn visit_pre_or_postfix_unary_expression(
        &self,
        node: &Node, /*PrefixUnaryExpression | PostfixUnaryExpression*/
        value_is_discarded: bool,
    ) -> io::Result<VisitResult> {
        let node_as_unary_expression = node.as_unary_expression();
        if matches!(
            node_as_unary_expression.operator(),
            SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
        ) && is_identifier(&node_as_unary_expression.operand())
            && !is_generated_identifier(&node_as_unary_expression.operand())
            && !is_local_name(&node_as_unary_expression.operand())
            && !is_declaration_name_of_enum_or_namespace(&node_as_unary_expression.operand())
        {
            let exported_names = self.get_exports(&node_as_unary_expression.operand())?;
            if let Some(exported_names) = exported_names {
                let mut temp: Option<Gc<Node /*Identifier*/>> = _d();
                let mut expression/*: Expression*/ = try_visit_node(
                    &node_as_unary_expression.operand(),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )?;
                if is_prefix_unary_expression(node) {
                    expression = self
                        .factory
                        .update_prefix_unary_expression(node, expression);
                } else {
                    expression = self
                        .factory
                        .update_postfix_unary_expression(node, expression);
                    if !value_is_discarded {
                        temp = Some(self.factory.create_temp_variable(
                            Some(|node: &Node| {
                                self.context.hoist_variable_declaration(node);
                            }),
                            None,
                        ));
                        expression = self
                            .factory
                            .create_assignment(temp.clone().unwrap(), expression)
                            .set_text_range(Some(node));
                    }
                }

                for export_name in &exported_names {
                    self.no_substitution_mut()
                        .insert(get_node_id(&expression), true);
                    expression = self
                        .create_export_expression(
                            export_name,
                            &expression,
                            Option::<&Node>::None,
                            None,
                        )
                        .set_text_range(Some(node));
                }

                if let Some(temp) = temp {
                    self.no_substitution_mut()
                        .insert(get_node_id(&expression), true);
                    expression = self
                        .factory
                        .create_comma(expression, temp)
                        .set_text_range(Some(node));
                }
                return Ok(Some(expression.into()));
            }
        }

        Ok(Some(
            try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?.into(),
        ))
    }

    pub(super) fn visit_import_call_expression(
        &self,
        node: &Node, /*ImportCall*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        let node_as_call_expression = node.as_call_expression();
        let external_module_name = get_external_module_name_literal(
            &self.factory,
            node,
            &self.current_source_file(),
            &**self.host,
            &**self.resolver,
            &self.compiler_options,
        );
        let first_argument = try_maybe_visit_node(
            first_or_undefined(&node_as_call_expression.arguments).cloned(),
            Some(|node: &Node| self.visitor(node)),
            Option::<fn(&Node) -> bool>::None,
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?;
        let argument = external_module_name
            .filter(|external_module_name| match first_argument.as_ref() {
                None => true,
                Some(first_argument) => {
                    !is_string_literal(first_argument)
                        || &*first_argument.as_string_literal().text()
                            != &*external_module_name.as_string_literal().text()
                }
            })
            .or(first_argument);
        let contains_lexical_this = node
            .transform_flags()
            .intersects(TransformFlags::ContainsLexicalThis);
        Ok(match self.compiler_options.module {
            Some(ModuleKind::AMD) => {
                self.create_import_call_expression_amd(argument, contains_lexical_this)
            }
            Some(ModuleKind::UMD) => self.create_import_call_expression_umd(
                argument.unwrap_or_else(|| self.factory.create_void_zero()),
                contains_lexical_this,
            ),
            _ => self.create_import_call_expression_common_js(argument, contains_lexical_this),
        })
    }

    pub(super) fn create_import_call_expression_umd(
        &self,
        arg: Gc<Node /*Expression*/>,
        contains_lexical_this: bool,
    ) -> Gc<Node /*Expression*/> {
        self.set_need_umd_dynamic_import_helper(true);
        if is_simple_copiable_expression(&arg) {
            let arg_clone = if is_generated_identifier(&arg) {
                arg.clone()
            } else if is_string_literal(&arg) {
                self.factory.create_string_literal_from_node(&arg)
            } else {
                self.factory
                    .clone_node(&arg)
                    .set_text_range(Some(&*arg))
                    .set_emit_flags(EmitFlags::NoComments)
            };
            self.factory.create_conditional_expression(
                self.factory.create_identifier("__syncRequire"),
                None,
                self.create_import_call_expression_common_js(Some(arg), contains_lexical_this),
                None,
                self.create_import_call_expression_amd(Some(arg_clone), contains_lexical_this),
            )
        } else {
            let temp = self.factory.create_temp_variable(
                Some(|node: &Node| {
                    self.context.hoist_variable_declaration(node);
                }),
                None,
            );
            self.factory.create_comma(
                self.factory.create_assignment(temp.clone(), arg),
                self.factory.create_conditional_expression(
                    self.factory.create_identifier("__syncRequire"),
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
        arg: Option<Gc<Node /*Expression*/>>,
        contains_lexical_this: bool,
    ) -> Gc<Node /*Expression*/> {
        let resolve = self.factory.create_unique_name("resolve", None);
        let reject = self.factory.create_unique_name("reject", None);
        let parameters = vec![
            self.factory.create_parameter_declaration(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                None,
                Some(resolve.clone()),
                None,
                None,
                None,
            ),
            self.factory.create_parameter_declaration(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                None,
                Some(reject.clone()),
                None,
                None,
                None,
            ),
        ];
        let body = self.factory.create_block(
            vec![self
                .factory
                .create_expression_statement(self.factory.create_call_expression(
                    self.factory.create_identifier("require"),
                    Option::<Gc<NodeArray>>::None,
                    Some(vec![
                        self.factory.create_array_literal_expression(
                            Some(vec![
                                arg.unwrap_or_else(|| self.factory.create_omitted_expression()),
                            ]),
                            None,
                        ),
                        resolve,
                        reject,
                    ]),
                ))],
            None,
        );

        let func: Gc<Node /*FunctionExpression | ArrowFunction*/>;
        if self.language_version >= ScriptTarget::ES2015 {
            func = self.factory.create_arrow_function(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                parameters,
                None,
                None,
                body,
            )
        } else {
            func = self.factory.create_function_expression(
                Option::<Gc<NodeArray>>::None,
                None,
                Option::<Gc<Node>>::None,
                Option::<Gc<NodeArray>>::None,
                Some(parameters),
                None,
                body,
            );

            if contains_lexical_this {
                set_emit_flags(&*func, EmitFlags::CapturesThis);
            }
        }

        let promise = self.factory.create_new_expression(
            self.factory.create_identifier("Promise"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![func]),
        );
        if get_es_module_interop(&self.compiler_options) == Some(true) {
            return self.factory.create_call_expression(
                self.factory.create_property_access_expression(
                    promise,
                    self.factory.create_identifier("then"),
                ),
                Option::<Gc<NodeArray>>::None,
                Some(vec![self
                    .emit_helpers()
                    .create_import_star_callback_helper()]),
            );
        }
        promise
    }

    pub(super) fn create_import_call_expression_common_js(
        &self,
        arg: Option<Gc<Node /*Expression*/>>,
        contains_lexical_this: bool,
    ) -> Gc<Node /*Expression*/> {
        let promise_resolve_call = self.factory.create_call_expression(
            self.factory.create_property_access_expression(
                self.factory.create_identifier("Promise"),
                "resolve",
            ),
            Option::<Gc<NodeArray>>::None,
            Some(vec![]),
        );
        let mut require_call/*: Expression*/ = self.factory.create_call_expression(
            self.factory.create_identifier("require"),
            Option::<Gc<NodeArray>>::None,
            Some(arg.map_or_default(|arg| vec![arg]))
        );
        if get_es_module_interop(&self.compiler_options) == Some(true) {
            require_call = self.emit_helpers().create_import_star_helper(require_call);
        }

        let func: Gc<Node /*FunctionExpression | ArrowFunction*/>;
        if self.language_version >= ScriptTarget::ES2015 {
            func = self.factory.create_arrow_function(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                vec![],
                None,
                None,
                require_call,
            );
        } else {
            func = self.factory.create_function_expression(
                Option::<Gc<NodeArray>>::None,
                None,
                Option::<Gc<Node>>::None,
                Option::<Gc<NodeArray>>::None,
                Some(vec![]),
                None,
                self.factory.create_block(
                    vec![self.factory.create_return_statement(Some(require_call))],
                    None,
                ),
            );

            if contains_lexical_this {
                set_emit_flags(&*func, EmitFlags::CapturesThis);
            }
        }

        self.factory.create_call_expression(
            self.factory
                .create_property_access_expression(promise_resolve_call, "then"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![func]),
        )
    }

    pub(super) fn get_helper_expression_for_export(
        &self,
        node: &Node, /*ExportDeclaration*/
        inner_expr: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        if get_es_module_interop(&self.compiler_options) != Some(true)
            || get_emit_flags(node).intersects(EmitFlags::NeverApplyImportHelper)
        {
            return inner_expr;
        }
        if get_export_needs_import_star_helper(node) {
            return self.emit_helpers().create_import_star_helper(inner_expr);
        }
        inner_expr
    }

    pub(super) fn get_helper_expression_for_import(
        &self,
        node: &Node, /*ImportDeclaration*/
        inner_expr: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        if get_es_module_interop(&self.compiler_options) != Some(true)
            || get_emit_flags(node).intersects(EmitFlags::NeverApplyImportHelper)
        {
            return inner_expr;
        }
        if get_import_needs_import_star_helper(node) {
            return self.emit_helpers().create_import_star_helper(inner_expr);
        }
        if get_import_needs_import_default_helper(node) {
            return self.emit_helpers().create_import_default_helper(inner_expr);
        }
        inner_expr
    }

    pub(super) fn visit_import_declaration(
        &self,
        node: &Node, /*ImportDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        let node_as_import_declaration = node.as_import_declaration();
        let mut statements: Option<Vec<Gc<Node /*Statement*/>>> = _d();
        let namespace_declaration = get_namespace_declaration_node(node);
        if self.module_kind != ModuleKind::AMD {
            if node_as_import_declaration.import_clause.is_none() {
                return Some(
                    self.factory
                        .create_expression_statement(self.create_require_call(node))
                        .set_text_range(Some(node))
                        .set_original_node(Some(node.node_wrapper()))
                        .into(),
                );
            } else {
                let mut variables: Vec<Gc<Node /*VariableDeclaration*/>> = _d();
                if let Some(namespace_declaration) = namespace_declaration
                    .as_ref()
                    .filter(|_| !is_default_import(node))
                {
                    variables.push(
                        self.factory.create_variable_declaration(
                            Some(
                                self.factory.clone_node(
                                    &namespace_declaration.as_named_declaration().name(),
                                ),
                            ),
                            None,
                            None,
                            Some(self.get_helper_expression_for_import(
                                node,
                                self.create_require_call(node),
                            )),
                        ),
                    );
                } else {
                    variables.push(self.factory.create_variable_declaration(
                        Some(self.factory.get_generated_name_for_node(Some(node), None)),
                        None,
                        None,
                        Some(self.get_helper_expression_for_import(
                            node,
                            self.create_require_call(node),
                        )),
                    ));

                    if let Some(namespace_declaration) = namespace_declaration
                        .as_ref()
                        .filter(|_| is_default_import(node))
                    {
                        variables.push(self.factory.create_variable_declaration(
                            Some(
                                self.factory.clone_node(
                                    &namespace_declaration.as_named_declaration().name(),
                                ),
                            ),
                            None,
                            None,
                            Some(self.factory.get_generated_name_for_node(Some(node), None)),
                        ));
                    }
                }

                statements.get_or_insert_default_().push(
                    self.factory
                        .create_variable_statement(
                            Option::<Gc<NodeArray>>::None,
                            self.factory.create_variable_declaration_list(
                                variables,
                                Some(
                                    (self.language_version >= ScriptTarget::ES2015)
                                        .then_some(NodeFlags::Const)
                                        .unwrap_or_default(),
                                ),
                            ),
                        )
                        .set_text_range(Some(node))
                        .set_original_node(Some(node.node_wrapper())),
                );
            }
        } else if let Some(namespace_declaration) = namespace_declaration
            .as_ref()
            .filter(|_| is_default_import(node))
        {
            statements.get_or_insert_default_().push(
                self.factory.create_variable_statement(
                    Option::<Gc<NodeArray>>::None,
                    self.factory.create_variable_declaration_list(
                        vec![self.factory.create_variable_declaration(
                            Some(
                                self.factory.clone_node(
                                    &namespace_declaration.as_named_declaration().name(),
                                ),
                            ),
                            None,
                            None,
                            Some(self.factory.get_generated_name_for_node(Some(node), None)),
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
            let id = get_original_node_id(node);
            self.deferred_exports_mut().insert(id, {
                self.append_exports_of_import_declaration(&mut statements, node);
                statements.clone()
            });
        } else {
            /*statements = */
            self.append_exports_of_import_declaration(&mut statements, node);
        }

        statements.map(single_or_many_node)
    }

    pub(super) fn create_require_call(
        &self,
        import_node: &Node, /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/
    ) -> Gc<Node> {
        let module_name = get_external_module_name_literal(
            &self.factory,
            import_node,
            &self.current_source_file(),
            &**self.host,
            &**self.resolver,
            &self.compiler_options,
        );
        let mut args: Vec<Gc<Node /*Expression*/>> = _d();
        if let Some(module_name) = module_name {
            args.push(module_name);
        }

        self.factory.create_call_expression(
            self.factory.create_identifier("require"),
            Option::<Gc<NodeArray>>::None,
            Some(args),
        )
    }
}
