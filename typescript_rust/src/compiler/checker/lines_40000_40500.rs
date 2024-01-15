use std::{borrow::Borrow, io};

use gc::Gc;
use id_arena::Id;
use indexmap::IndexMap;

use super::UnusedKind;
use crate::{
    clear, compare_diagnostics, concatenate, contains_parse_error, for_each,
    get_host_signature_from_jsdoc, get_node_id, get_parameter_symbol_from_jsdoc,
    get_source_file_of_node, is_access_expression, is_exports_identifier, is_external_module,
    is_external_or_common_js_module, is_in_js_file, is_jsdoc_callback_tag, is_jsdoc_function_type,
    is_jsdoc_parameter_tag, is_jsdoc_type_expression, is_logging,
    is_module_exports_access_expression, is_parameter, is_rest_parameter, last, last_or_undefined,
    relative_complement, skip_type_checking, try_for_each, try_for_each_child, try_maybe_for_each,
    CancellationTokenDebuggable, Diagnostic, Diagnostics, HasStatementsInterface,
    ImportsNotUsedAsValues, Node, NodeArray, NodeCheckFlags, NodeFlags, NodeInterface,
    SignatureDeclarationInterface, SyntaxKind, Type, TypeChecker, TypeCheckerHost,
};

impl TypeChecker {
    pub(super) fn is_duplicated_common_js_export(&self, declarations: Option<&[Id<Node>]>) -> bool {
        matches!(
            declarations,
            Some(declarations) if declarations.len() > 1 &&
                declarations.into_iter().all(|d| is_in_js_file(Some(&**d)) &&
                    is_access_expression(d) && {
                        let d_as_has_expression = d.as_has_expression();
                        is_exports_identifier(&d_as_has_expression.expression()) ||
                        is_module_exports_access_expression(&d_as_has_expression.expression())
                    }
                )
        )
    }

    pub(super) fn check_source_element(&self, node: Option<Id<Node>>) -> io::Result<()> {
        if let Some(node) = node {
            let node = node.borrow();
            let save_current_node = self.maybe_current_node();
            self.set_current_node(Some(node.node_wrapper()));
            self.set_instantiation_count(0);
            self.check_source_element_worker(node)?;
            self.set_current_node(save_current_node);
        }

        Ok(())
    }

    pub(super) fn check_source_element_worker(&self, node: Id<Node>) -> io::Result<()> {
        if is_in_js_file(Some(node)) {
            try_maybe_for_each(
                node.maybe_js_doc().as_ref(),
                |jsdoc: &Id<Node>, _| -> io::Result<Option<()>> {
                    let tags = jsdoc.as_jsdoc().tags.as_ref();
                    try_maybe_for_each(tags, |tag: &Id<Node>, _| -> io::Result<Option<()>> {
                        self.check_source_element(Some(&**tag))?;
                        Ok(None)
                    })?;
                    Ok(None)
                },
            )?;
        }

        let kind = node.kind();
        if let Some(cancellation_token) = self.maybe_cancellation_token() {
            if matches!(
                kind,
                SyntaxKind::ModuleDeclaration
                    | SyntaxKind::ClassDeclaration
                    | SyntaxKind::InterfaceDeclaration
                    | SyntaxKind::FunctionDeclaration
            ) {
                cancellation_token.throw_if_cancellation_requested();
            }
        }
        if kind >= SyntaxKind::FirstStatement
            && kind <= SyntaxKind::LastStatement
            && matches!(
                node.maybe_flow_node().as_ref(),
                Some(node_flow_node) if !self.is_reachable_flow_node(node_flow_node.clone())?
            )
        {
            self.error_or_suggestion(
                self.compiler_options.allow_unreachable_code == Some(false),
                node,
                &*Diagnostics::Unreachable_code_detected,
                None,
            );
        }

        match kind {
            SyntaxKind::TypeParameter => {
                self.check_type_parameter(node)?;
            }
            SyntaxKind::Parameter => {
                self.check_parameter(node)?;
            }
            SyntaxKind::PropertyDeclaration => {
                self.check_property_declaration(node)?;
            }
            SyntaxKind::PropertySignature => {
                self.check_property_signature(node)?;
            }
            SyntaxKind::ConstructorType
            | SyntaxKind::FunctionType
            | SyntaxKind::CallSignature
            | SyntaxKind::ConstructSignature
            | SyntaxKind::IndexSignature => {
                self.check_signature_declaration(node)?;
            }
            SyntaxKind::MethodDeclaration | SyntaxKind::MethodSignature => {
                self.check_method_declaration(node)?;
            }
            SyntaxKind::ClassStaticBlockDeclaration => {
                self.check_class_static_block_declaration(node)?;
            }
            SyntaxKind::Constructor => {
                self.check_constructor_declaration(node)?;
            }
            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                self.check_accessor_declaration(node)?;
            }
            SyntaxKind::TypeReference => {
                self.check_type_reference_node(node)?;
            }
            SyntaxKind::TypePredicate => {
                self.check_type_predicate(node)?;
            }
            SyntaxKind::TypeQuery => {
                self.check_type_query(node)?;
            }
            SyntaxKind::TypeLiteral => {
                self.check_type_literal(node)?;
            }
            SyntaxKind::ArrayType => {
                self.check_array_type(node)?;
            }
            SyntaxKind::TupleType => {
                self.check_tuple_type(node)?;
            }
            SyntaxKind::UnionType | SyntaxKind::IntersectionType => {
                self.check_union_or_intersection_type(node)?;
            }
            SyntaxKind::ParenthesizedType | SyntaxKind::OptionalType | SyntaxKind::RestType => {
                self.check_source_element(node.as_has_type().maybe_type())?;
            }
            SyntaxKind::ThisType => {
                self.check_this_type(node)?;
            }
            SyntaxKind::TypeOperator => {
                self.check_type_operator(node)?;
            }
            SyntaxKind::ConditionalType => {
                self.check_conditional_type(node)?;
            }
            SyntaxKind::InferType => {
                self.check_infer_type(node)?;
            }
            SyntaxKind::TemplateLiteralType => {
                self.check_template_literal_type(node)?;
            }
            SyntaxKind::ImportType => {
                self.check_import_type(node)?;
            }
            SyntaxKind::NamedTupleMember => {
                self.check_named_tuple_member(node)?;
            }
            SyntaxKind::JSDocAugmentsTag => {
                self.check_jsdoc_augments_tag(node);
            }
            SyntaxKind::JSDocImplementsTag => {
                self.check_jsdoc_implements_tag(node);
            }
            SyntaxKind::JSDocTypedefTag
            | SyntaxKind::JSDocCallbackTag
            | SyntaxKind::JSDocEnumTag => {
                self.check_jsdoc_type_alias_tag(node)?;
            }
            SyntaxKind::JSDocTemplateTag => {
                self.check_jsdoc_template_tag(node)?;
            }
            SyntaxKind::JSDocTypeTag => {
                self.check_jsdoc_type_tag(node)?;
            }
            SyntaxKind::JSDocParameterTag => {
                self.check_jsdoc_parameter_tag(node)?;
            }
            SyntaxKind::JSDocPropertyTag => {
                self.check_jsdoc_property_tag(node)?;
            }
            SyntaxKind::JSDocFunctionType => {
                self.check_jsdoc_function_type(node)?;
                self.check_jsdoc_type_is_in_js_file(node);
                try_for_each_child(
                    node,
                    |child| self.check_source_element(Some(child)),
                    Option::<fn(&NodeArray) -> io::Result<()>>::None,
                )?;
            }
            SyntaxKind::JSDocNonNullableType
            | SyntaxKind::JSDocNullableType
            | SyntaxKind::JSDocAllType
            | SyntaxKind::JSDocUnknownType
            | SyntaxKind::JSDocTypeLiteral => {
                self.check_jsdoc_type_is_in_js_file(node);
                try_for_each_child(
                    node,
                    |child| self.check_source_element(Some(child)),
                    Option::<fn(&NodeArray) -> io::Result<()>>::None,
                )?;
            }
            SyntaxKind::JSDocVariadicType => {
                self.check_jsdoc_variadic_type(node)?;
            }
            SyntaxKind::JSDocTypeExpression => {
                self.check_source_element(Some(&*node.as_jsdoc_type_expression().type_))?;
            }
            SyntaxKind::JSDocPublicTag
            | SyntaxKind::JSDocProtectedTag
            | SyntaxKind::JSDocPrivateTag => {
                self.check_jsdoc_accessibility_modifiers(node);
            }
            SyntaxKind::IndexedAccessType => {
                self.check_indexed_access_type(node)?;
            }
            SyntaxKind::MappedType => {
                self.check_mapped_type(node)?;
            }
            SyntaxKind::FunctionDeclaration => {
                self.check_function_declaration(node)?;
            }
            SyntaxKind::Block | SyntaxKind::ModuleBlock => {
                self.check_block(node)?;
            }
            SyntaxKind::VariableStatement => {
                self.check_variable_statement(node)?;
            }
            SyntaxKind::ExpressionStatement => {
                self.check_expression_statement(node)?;
            }
            SyntaxKind::IfStatement => {
                self.check_if_statement(node)?;
            }
            SyntaxKind::DoStatement => {
                self.check_do_statement(node)?;
            }
            SyntaxKind::WhileStatement => {
                self.check_while_statement(node)?;
            }
            SyntaxKind::ForStatement => {
                self.check_for_statement(node)?;
            }
            SyntaxKind::ForInStatement => {
                self.check_for_in_statement(node)?;
            }
            SyntaxKind::ForOfStatement => {
                self.check_for_of_statement(node)?;
            }
            SyntaxKind::ContinueStatement | SyntaxKind::BreakStatement => {
                self.check_break_or_continue_statement(node);
            }
            SyntaxKind::ReturnStatement => {
                self.check_return_statement(node)?;
            }
            SyntaxKind::WithStatement => {
                self.check_with_statement(node)?;
            }
            SyntaxKind::SwitchStatement => {
                self.check_switch_statement(node)?;
            }
            SyntaxKind::LabeledStatement => {
                self.check_labeled_statement(node)?;
            }
            SyntaxKind::ThrowStatement => {
                self.check_throw_statement(node)?;
            }
            SyntaxKind::TryStatement => {
                self.check_try_statement(node)?;
            }
            SyntaxKind::VariableDeclaration => {
                self.check_variable_declaration(node)?;
            }
            SyntaxKind::BindingElement => {
                self.check_binding_element(node)?;
            }
            SyntaxKind::ClassDeclaration => {
                self.check_class_declaration(node)?;
            }
            SyntaxKind::InterfaceDeclaration => {
                self.check_interface_declaration(node)?;
            }
            SyntaxKind::TypeAliasDeclaration => {
                self.check_type_alias_declaration(node)?;
            }
            SyntaxKind::EnumDeclaration => {
                self.check_enum_declaration(node)?;
            }
            SyntaxKind::ModuleDeclaration => {
                self.check_module_declaration(node)?;
            }
            SyntaxKind::ImportDeclaration => {
                self.check_import_declaration(node)?;
            }
            SyntaxKind::ImportEqualsDeclaration => {
                self.check_import_equals_declaration(node)?;
            }
            SyntaxKind::ExportDeclaration => {
                self.check_export_declaration(node)?;
            }
            SyntaxKind::ExportAssignment => {
                self.check_export_assignment(node)?;
            }
            SyntaxKind::EmptyStatement | SyntaxKind::DebuggerStatement => {
                self.check_grammar_statement_in_ambient_context(node);
            }
            SyntaxKind::MissingDeclaration => {
                self.check_missing_declaration(node)?;
            }
            _ => (),
        }

        Ok(())
    }

    pub(super) fn check_jsdoc_type_is_in_js_file(&self, node: Id<Node>) {
        if !is_in_js_file(Some(node)) {
            self.grammar_error_on_node(
                node,
                &Diagnostics::JSDoc_types_can_only_be_used_inside_documentation_comments,
                None,
            );
        }
    }

    pub(super) fn check_jsdoc_variadic_type(
        &self,
        node: Id<Node>, /*JSDocVariadicType*/
    ) -> io::Result<()> {
        self.check_jsdoc_type_is_in_js_file(node);
        let node_as_base_jsdoc_unary_type = node.as_base_jsdoc_unary_type();
        self.check_source_element(node_as_base_jsdoc_unary_type.type_.as_deref())?;

        let ref parent = node.parent();
        if is_parameter(parent) && is_jsdoc_function_type(&parent.parent()) {
            if !Gc::ptr_eq(
                last(&parent.parent().as_jsdoc_function_type().parameters()),
                parent,
            ) {
                self.error(
                    Some(node),
                    &Diagnostics::A_rest_parameter_must_be_last_in_a_parameter_list,
                    None,
                );
            }
            return Ok(());
        }

        if !is_jsdoc_type_expression(parent) {
            self.error(
                Some(node),
                &Diagnostics::JSDoc_may_only_appear_in_the_last_parameter_of_a_signature,
                None,
            );
        }

        let ref param_tag = node.parent().parent();
        if !is_jsdoc_parameter_tag(param_tag) {
            self.error(
                Some(node),
                &Diagnostics::JSDoc_may_only_appear_in_the_last_parameter_of_a_signature,
                None,
            );
            return Ok(());
        }

        let param = get_parameter_symbol_from_jsdoc(param_tag, self);
        if param.is_none() {
            return Ok(());
        }
        let param = param.unwrap();

        let host = get_host_signature_from_jsdoc(param_tag);
        if match host.as_ref() {
            None => true,
            Some(host) => !matches!(
                last(&host.as_signature_declaration().parameters()).maybe_symbol(),
                Some(symbol) if symbol == param
            ),
        } {
            self.error(
                Some(node),
                &Diagnostics::A_rest_parameter_must_be_last_in_a_parameter_list,
                None,
            );
        }

        Ok(())
    }

    pub(super) fn get_type_from_jsdoc_variadic_type(
        &self,
        node: Id<Node>, /*JSDocVariadicType*/
    ) -> io::Result<Id<Type>> {
        let type_ =
            self.get_type_from_type_node_(node.as_base_jsdoc_unary_type().type_.as_ref().unwrap())?;
        let ref parent = node.parent();
        let ref param_tag = node.parent().parent();
        if is_jsdoc_type_expression(&node.parent()) && is_jsdoc_parameter_tag(param_tag) {
            let host = get_host_signature_from_jsdoc(param_tag);
            let is_callback_tag = is_jsdoc_callback_tag(&param_tag.parent().parent());
            if host.is_some() || is_callback_tag {
                let last_param_declaration = if is_callback_tag {
                    last_or_undefined(
                        &*param_tag
                            .parent()
                            .parent()
                            .as_jsdoc_callback_tag()
                            .type_expression
                            .as_jsdoc_signature()
                            .parameters,
                    )
                    .cloned()
                } else {
                    last_or_undefined(
                        &**host
                            .as_ref()
                            .unwrap()
                            .as_signature_declaration()
                            .parameters(),
                    )
                    .cloned()
                };
                let symbol = get_parameter_symbol_from_jsdoc(param_tag, self);
                if match last_param_declaration.as_ref() {
                    None => true,
                    Some(last_param_declaration) => {
                        matches!(
                            symbol,
                            Some(symbol) if matches!(
                                last_param_declaration.maybe_symbol(),
                                Some(last_param_declaration_symbol) if last_param_declaration_symbol == symbol
                            )
                        ) && is_rest_parameter(last_param_declaration, self)
                    }
                } {
                    return Ok(self.create_array_type(type_, None));
                }
            }
        }
        if is_parameter(parent) && is_jsdoc_function_type(&parent.parent()) {
            return Ok(self.create_array_type(type_, None));
        }
        self.add_optionality(type_, None, None)
    }

    pub(super) fn check_node_deferred(&self, node: Id<Node>) {
        let ref enclosing_file = get_source_file_of_node(node, self);
        let links = self.get_node_links(enclosing_file);
        if !(*links)
            .borrow()
            .flags
            .intersects(NodeCheckFlags::TypeChecked)
        {
            let mut links = links.borrow_mut();
            if links.deferred_nodes.is_none() {
                links.deferred_nodes = Some(IndexMap::new());
            }
            let id = get_node_id(node);
            links
                .deferred_nodes
                .as_mut()
                .unwrap()
                .insert(id, node.node_wrapper());
        }
    }

    pub(super) fn check_deferred_nodes(
        &self,
        context: Id<Node>, /*SourceFile*/
    ) -> io::Result<()> {
        let links = self.get_node_links(context);
        let links_deferred_nodes_is_some = {
            let value = (*links).borrow().deferred_nodes.is_some();
            value
        };
        if links_deferred_nodes_is_some {
            let mut i = 0;
            while i < {
                let value = (*links).borrow().deferred_nodes.as_ref().unwrap().len();
                value
            } {
                self.check_deferred_node(&*{
                    let value = (*links)
                        .borrow()
                        .deferred_nodes
                        .as_ref()
                        .unwrap()
                        .values()
                        .nth(i)
                        .cloned()
                        .unwrap();
                    value
                })?;
                i += 1;
            }
        }

        Ok(())
    }

    pub(super) fn check_deferred_node(&self, node: Id<Node>) -> io::Result<()> {
        // tracing?.push(tracing.Phase.Check, "checkDeferredNode", { kind: node.kind, pos: node.pos, end: node.end });
        let save_current_node = self.maybe_current_node();
        self.set_current_node(Some(node.node_wrapper()));
        self.set_instantiation_count(0);
        match node.kind() {
            SyntaxKind::CallExpression
            | SyntaxKind::NewExpression
            | SyntaxKind::TaggedTemplateExpression
            | SyntaxKind::Decorator
            | SyntaxKind::JsxOpeningElement => {
                self.resolve_untyped_call(node)?;
            }
            SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::MethodSignature => {
                self.check_function_expression_or_object_literal_method_deferred(node)?;
            }
            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                self.check_accessor_declaration(node)?;
            }
            SyntaxKind::ClassExpression => {
                self.check_class_expression_deferred(node)?;
            }
            SyntaxKind::JsxSelfClosingElement => {
                self.check_jsx_self_closing_element_deferred(node)?;
            }
            SyntaxKind::JsxElement => {
                self.check_jsx_element_deferred(node)?;
            }
            _ => (),
        }
        self.set_current_node(save_current_node);
        // tracing?.pop();

        Ok(())
    }

    pub(super) fn check_source_file(&self, node: Id<Node> /*SourceFile*/) -> io::Result<()> {
        if is_logging {
            println!(
                "checking source file: {}",
                node.as_source_file().file_name()
            );
        }
        // tracing?.push(tracing.Phase.Check, "checkSourceFile", { path: node.path }, /*separateBeginAndEnd*/ true);
        // performance.mark("beforeCheck");
        self.check_source_file_worker(node)?;
        // performance.mark("afterCheck");
        // performance.measure("Check", "beforeCheck", "afterCheck");
        // tracing?.pop();

        Ok(())
    }

    pub(super) fn unused_is_error(&self, kind: UnusedKind, is_ambient: bool) -> bool {
        if is_ambient {
            return false;
        }
        match kind {
            UnusedKind::Local => self.compiler_options.no_unused_locals == Some(true),
            UnusedKind::Parameter => self.compiler_options.no_unused_parameters == Some(true),
            // _ => Debug_.assert_never(kind, None),
        }
    }

    pub(super) fn get_potentially_unused_identifiers(
        &self,
        source_file: Id<Node>, /*SourceFile*/
    ) -> Vec<Id<Node /*PotentiallyUnusedIdentifier*/>> {
        self.all_potentially_unused_identifiers()
            .get(&source_file.as_source_file().path())
            .cloned()
            .unwrap_or_else(|| vec![])
    }

    pub(super) fn check_source_file_worker(
        &self,
        node: Id<Node>, /*SourceFile*/
    ) -> io::Result<()> {
        let links = self.get_node_links(node);
        if !(*links)
            .borrow()
            .flags
            .intersects(NodeCheckFlags::TypeChecked)
        {
            if skip_type_checking(node, &self.compiler_options, |file_name| {
                TypeCheckerHost::is_source_of_project_reference_redirect(&**self.host, file_name)
            }) {
                return Ok(());
            }

            self.check_grammar_source_file(node);

            clear(&mut self.potential_this_collisions());
            clear(&mut self.potential_new_target_collisions());
            clear(&mut self.potential_weak_map_set_collisions());
            clear(&mut self.potential_reflect_collisions());

            let node_as_source_file = node.as_source_file();
            try_for_each(
                &node_as_source_file.statements(),
                |statement, _| -> io::Result<_> {
                    self.check_source_element(Some(&**statement))?;
                    Ok(Option::<()>::None)
                },
            )?;
            self.check_source_element(Some(node_as_source_file.end_of_file_token()))?;

            self.check_deferred_nodes(node)?;

            if is_external_or_common_js_module(node) {
                self.register_for_unused_identifiers_check(node);
            }

            if !node_as_source_file.is_declaration_file()
                && (self.compiler_options.no_unused_locals == Some(true)
                    || self.compiler_options.no_unused_parameters == Some(true))
            {
                self.check_unused_identifiers(
                    &self.get_potentially_unused_identifiers(node),
                    |containing_node, kind, diag| {
                        if !contains_parse_error(containing_node, self)
                            && self.unused_is_error(
                                kind,
                                containing_node.flags().intersects(NodeFlags::Ambient),
                            )
                        {
                            self.diagnostics().add(diag);
                        }
                    },
                )?;
            }

            if self.compiler_options.imports_not_used_as_values
                == Some(ImportsNotUsedAsValues::Error)
                && !node_as_source_file.is_declaration_file()
                && is_external_module(node)
            {
                self.check_imports_for_type_only_conversion(node)?;
            }

            if is_external_or_common_js_module(node) {
                self.check_external_module_exports(node)?;
            }

            {
                let mut potential_this_collisions = self.potential_this_collisions();
                if !potential_this_collisions.is_empty() {
                    for_each(
                        &*potential_this_collisions,
                        |potential_this_collision: &Id<Node>, _| -> Option<()> {
                            self.check_if_this_is_captured_in_enclosing_scope(
                                potential_this_collision,
                            );
                            None
                        },
                    );
                    clear(&mut potential_this_collisions);
                }
            }

            {
                let mut potential_new_target_collisions = self.potential_new_target_collisions();
                if !potential_new_target_collisions.is_empty() {
                    for_each(
                        &*potential_new_target_collisions,
                        |potential_new_target_collision: &Id<Node>, _| -> Option<()> {
                            self.check_if_new_target_is_captured_in_enclosing_scope(
                                potential_new_target_collision,
                            );
                            None
                        },
                    );
                    clear(&mut potential_new_target_collisions);
                }
            }

            {
                let mut potential_weak_map_set_collisions =
                    self.potential_weak_map_set_collisions();
                if !potential_weak_map_set_collisions.is_empty() {
                    for_each(
                        &*potential_weak_map_set_collisions,
                        |potential_weak_map_set_collision: &Id<Node>, _| -> Option<()> {
                            self.check_weak_map_set_collision(potential_weak_map_set_collision);
                            None
                        },
                    );
                    clear(&mut potential_weak_map_set_collisions);
                }
            }

            {
                let mut potential_reflect_collisions = self.potential_reflect_collisions();
                if !potential_reflect_collisions.is_empty() {
                    for_each(
                        &*potential_reflect_collisions,
                        |potential_reflect_collision: &Id<Node>, _| -> Option<()> {
                            self.check_reflect_collision(potential_reflect_collision);
                            None
                        },
                    );
                    clear(&mut potential_reflect_collisions);
                }
            }

            links.borrow_mut().flags |= NodeCheckFlags::TypeChecked;
        }

        Ok(())
    }

    pub fn get_diagnostics(
        &self,
        source_file: Option<Id<Node> /*SourceFile*/>,
        ct: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> io::Result<Vec<Gc<Diagnostic>>> {
        // try {
        self.set_cancellation_token(ct);
        let ret = self.get_diagnostics_worker(source_file)?;
        // }
        // finally {
        self.set_cancellation_token(None);
        // }
        Ok(ret)
    }

    pub(super) fn get_diagnostics_worker(
        &self,
        source_file: Option<Id<Node> /*SourceFile*/>,
    ) -> io::Result<Vec<Gc<Diagnostic>>> {
        self.throw_if_non_diagnostics_producing();
        if let Some(source_file) = source_file {
            let source_file = source_file.borrow();
            let previous_global_diagnostics = self.diagnostics().get_global_diagnostics();
            let previous_global_diagnostics_size = previous_global_diagnostics.len();

            self.check_source_file(source_file)?;

            let semantic_diagnostics = self
                .diagnostics()
                .get_diagnostics(Some(&source_file.as_source_file().file_name()));
            let current_global_diagnostics = self.diagnostics().get_global_diagnostics();
            if current_global_diagnostics.len() != previous_global_diagnostics.len() {
                let deferred_global_diagnostics = relative_complement(
                    &previous_global_diagnostics,
                    &current_global_diagnostics,
                    |a: &Gc<Diagnostic>, b: &Gc<Diagnostic>| compare_diagnostics(&**a, &**b),
                );
                return Ok(concatenate(
                    deferred_global_diagnostics,
                    semantic_diagnostics,
                ));
            } else if previous_global_diagnostics_size == 0 && current_global_diagnostics.len() > 0
            {
                return Ok(concatenate(
                    current_global_diagnostics,
                    semantic_diagnostics,
                ));
            }
            return Ok(semantic_diagnostics);
        }

        try_for_each(
            &*self.host.get_source_files(),
            |source_file: &Id<Node>, _| -> io::Result<Option<()>> {
                self.check_source_file(source_file)?;
                Ok(None)
            },
        )?;
        Ok(self.diagnostics().get_diagnostics(None))
    }
}
