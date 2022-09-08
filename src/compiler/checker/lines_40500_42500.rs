#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{EmitResolverCreateResolver, UnusedKind};
use crate::{
    add_related_info, clear, concatenate, contains_parse_error, create_diagnostic_for_node,
    escape_leading_underscores, external_helpers_module_name_text, for_each_child,
    get_all_accessor_declarations, get_declaration_of_kind, get_external_module_name,
    get_host_signature_from_jsdoc, get_node_id, get_parameter_symbol_from_jsdoc,
    get_source_file_of_node, has_syntactic_modifier, is_access_expression, is_ambient_module,
    is_binding_pattern, is_class_like, is_effective_external_module, is_exports_identifier,
    is_external_module, is_global_scope_augmentation, is_in_js_file, is_jsdoc_callback_tag,
    is_jsdoc_function_type, is_jsdoc_parameter_tag, is_jsdoc_type_expression,
    is_module_exports_access_expression, is_named_declaration, is_parameter,
    is_private_identifier_class_element_declaration, is_property_declaration, is_rest_parameter,
    is_string_literal, last, last_or_undefined, maybe_for_each, modifier_to_flag,
    node_can_be_decorated, node_is_present, skip_type_checking, some, token_to_string, try_cast,
    Debug_, Diagnostics, ExternalEmitHelpers, FunctionLikeDeclarationInterface,
    ImportsNotUsedAsValues, ModifierFlags, NamedDeclarationInterface, NodeArray, NodeCheckFlags,
    NodeFlags, ObjectFlags, Signature, SignatureDeclarationInterface, SymbolInterface, SyntaxKind,
    __String, bind_source_file, for_each, is_external_or_common_js_module,
    CancellationTokenDebuggable, Diagnostic, EmitResolverDebuggable, IndexInfo, Node,
    NodeInterface, StringOrNumber, Symbol, SymbolFlags, Type, TypeChecker,
};

impl TypeChecker {
    pub(super) fn is_duplicated_common_js_export(&self, declarations: Option<&[Rc<Node>]>) -> bool {
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

    pub(super) fn check_source_element<TNode: Borrow<Node>>(&self, node: Option<TNode>) {
        if let Some(node) = node {
            let node = node.borrow();
            let save_current_node = self.maybe_current_node();
            self.set_current_node(Some(node.node_wrapper()));
            self.set_instantiation_count(0);
            self.check_source_element_worker(node);
            self.set_current_node(save_current_node);
        }
    }

    pub(super) fn check_source_element_worker(&self, node: &Node) {
        if is_in_js_file(Some(node)) {
            maybe_for_each(
                node.maybe_js_doc().as_ref(),
                |jsdoc: &Rc<Node>, _| -> Option<()> {
                    let tags = jsdoc.as_jsdoc().tags.as_ref();
                    maybe_for_each(tags, |tag: &Rc<Node>, _| -> Option<()> {
                        self.check_source_element(Some(&**tag));
                        None
                    });
                    None
                },
            );
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
                Some(node_flow_node) if !self.is_reachable_flow_node(node_flow_node.clone())
            )
        {
            self.error_or_suggestion(
                self.compiler_options.allow_unreachable_code == Some(false),
                node,
                Diagnostics::Unreachable_code_detected.clone().into(),
                None,
            );
        }

        match kind {
            SyntaxKind::TypeParameter => {
                self.check_type_parameter(node);
            }
            SyntaxKind::Parameter => {
                self.check_parameter(node);
            }
            SyntaxKind::PropertyDeclaration => {
                self.check_property_declaration(node);
            }
            SyntaxKind::PropertySignature => {
                self.check_property_signature(node);
            }
            SyntaxKind::ConstructorType
            | SyntaxKind::FunctionType
            | SyntaxKind::CallSignature
            | SyntaxKind::ConstructSignature
            | SyntaxKind::IndexSignature => {
                self.check_signature_declaration(node);
            }
            SyntaxKind::MethodDeclaration | SyntaxKind::MethodSignature => {
                self.check_method_declaration(node);
            }
            SyntaxKind::ClassStaticBlockDeclaration => {
                self.check_class_static_block_declaration(node);
            }
            SyntaxKind::Constructor => {
                self.check_constructor_declaration(node);
            }
            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                self.check_accessor_declaration(node);
            }
            SyntaxKind::TypeReference => {
                self.check_type_reference_node(node);
            }
            SyntaxKind::TypePredicate => {
                self.check_type_predicate(node);
            }
            SyntaxKind::TypeQuery => {
                self.check_type_query(node);
            }
            SyntaxKind::TypeLiteral => {
                self.check_type_literal(node);
            }
            SyntaxKind::ArrayType => {
                self.check_array_type(node);
            }
            SyntaxKind::TupleType => {
                self.check_tuple_type(node);
            }
            SyntaxKind::UnionType | SyntaxKind::IntersectionType => {
                self.check_union_or_intersection_type(node);
            }
            SyntaxKind::ParenthesizedType | SyntaxKind::OptionalType | SyntaxKind::RestType => {
                self.check_source_element(node.as_has_type().maybe_type());
            }
            SyntaxKind::ThisType => {
                self.check_this_type(node);
            }
            SyntaxKind::TypeOperator => {
                self.check_type_operator(node);
            }
            SyntaxKind::ConditionalType => {
                self.check_conditional_type(node);
            }
            SyntaxKind::InferType => {
                self.check_infer_type(node);
            }
            SyntaxKind::TemplateLiteralType => {
                self.check_template_literal_type(node);
            }
            SyntaxKind::ImportType => {
                self.check_import_type(node);
            }
            SyntaxKind::NamedTupleMember => {
                self.check_named_tuple_member(node);
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
                self.check_jsdoc_type_alias_tag(node);
            }
            SyntaxKind::JSDocTemplateTag => {
                self.check_jsdoc_template_tag(node);
            }
            SyntaxKind::JSDocTypeTag => {
                self.check_jsdoc_type_tag(node);
            }
            SyntaxKind::JSDocParameterTag => {
                self.check_jsdoc_parameter_tag(node);
            }
            SyntaxKind::JSDocPropertyTag => {
                self.check_jsdoc_property_tag(node);
            }
            SyntaxKind::JSDocFunctionType => {
                self.check_jsdoc_function_type(node);
                self.check_jsdoc_type_is_in_js_file(node);
                for_each_child(
                    node,
                    |child| self.check_source_element(Some(child)),
                    Option::<fn(&NodeArray)>::None,
                );
            }
            SyntaxKind::JSDocNonNullableType
            | SyntaxKind::JSDocNullableType
            | SyntaxKind::JSDocAllType
            | SyntaxKind::JSDocUnknownType
            | SyntaxKind::JSDocTypeLiteral => {
                self.check_jsdoc_type_is_in_js_file(node);
                for_each_child(
                    node,
                    |child| self.check_source_element(Some(child)),
                    Option::<fn(&NodeArray)>::None,
                );
            }
            SyntaxKind::JSDocVariadicType => {
                self.check_jsdoc_variadic_type(node);
            }
            SyntaxKind::JSDocTypeExpression => {
                self.check_source_element(Some(&*node.as_jsdoc_type_expression().type_));
            }
            SyntaxKind::JSDocPublicTag
            | SyntaxKind::JSDocProtectedTag
            | SyntaxKind::JSDocPrivateTag => {
                self.check_jsdoc_accessibility_modifiers(node);
            }
            SyntaxKind::IndexedAccessType => {
                self.check_indexed_access_type(node);
            }
            SyntaxKind::MappedType => {
                self.check_mapped_type(node);
            }
            SyntaxKind::FunctionDeclaration => {
                self.check_function_declaration(node);
            }
            SyntaxKind::Block | SyntaxKind::ModuleBlock => {
                self.check_block(node);
            }
            SyntaxKind::VariableStatement => {
                self.check_variable_statement(node);
            }
            SyntaxKind::ExpressionStatement => {
                self.check_expression_statement(node);
            }
            SyntaxKind::IfStatement => {
                self.check_if_statement(node);
            }
            SyntaxKind::DoStatement => {
                self.check_do_statement(node);
            }
            SyntaxKind::WhileStatement => {
                self.check_while_statement(node);
            }
            SyntaxKind::ForStatement => {
                self.check_for_statement(node);
            }
            SyntaxKind::ForInStatement => {
                self.check_for_in_statement(node);
            }
            SyntaxKind::ForOfStatement => {
                self.check_for_of_statement(node);
            }
            SyntaxKind::ContinueStatement | SyntaxKind::BreakStatement => {
                self.check_break_or_continue_statement(node);
            }
            SyntaxKind::ReturnStatement => {
                self.check_return_statement(node);
            }
            SyntaxKind::WithStatement => {
                self.check_with_statement(node);
            }
            SyntaxKind::SwitchStatement => {
                self.check_switch_statement(node);
            }
            SyntaxKind::LabeledStatement => {
                self.check_labeled_statement(node);
            }
            SyntaxKind::ThrowStatement => {
                self.check_throw_statement(node);
            }
            SyntaxKind::TryStatement => {
                self.check_try_statement(node);
            }
            SyntaxKind::VariableDeclaration => {
                self.check_variable_declaration(node);
            }
            SyntaxKind::BindingElement => {
                self.check_binding_element(node);
            }
            SyntaxKind::ClassDeclaration => {
                self.check_class_declaration(node);
            }
            SyntaxKind::InterfaceDeclaration => {
                self.check_interface_declaration(node);
            }
            SyntaxKind::TypeAliasDeclaration => {
                self.check_type_alias_declaration(node);
            }
            SyntaxKind::EnumDeclaration => {
                self.check_enum_declaration(node);
            }
            SyntaxKind::ModuleDeclaration => {
                self.check_module_declaration(node);
            }
            SyntaxKind::ImportDeclaration => {
                self.check_import_declaration(node);
            }
            SyntaxKind::ImportEqualsDeclaration => {
                self.check_import_equals_declaration(node);
            }
            SyntaxKind::ExportDeclaration => {
                self.check_export_declaration(node);
            }
            SyntaxKind::ExportAssignment => {
                self.check_export_assignment(node);
            }
            SyntaxKind::EmptyStatement | SyntaxKind::DebuggerStatement => {
                self.check_grammar_statement_in_ambient_context(node);
            }
            SyntaxKind::MissingDeclaration => {
                self.check_missing_declaration(node);
            }
            _ => (),
        }
    }

    pub(super) fn check_jsdoc_type_is_in_js_file(&self, node: &Node) {
        if !is_in_js_file(Some(node)) {
            self.grammar_error_on_node(
                node,
                &Diagnostics::JSDoc_types_can_only_be_used_inside_documentation_comments,
                None,
            );
        }
    }

    pub(super) fn check_jsdoc_variadic_type(&self, node: &Node /*JSDocVariadicType*/) {
        self.check_jsdoc_type_is_in_js_file(node);
        let node_as_base_jsdoc_unary_type = node.as_base_jsdoc_unary_type();
        self.check_source_element(node_as_base_jsdoc_unary_type.type_.as_deref());

        let ref parent = node.parent();
        if is_parameter(parent) && is_jsdoc_function_type(&parent.parent()) {
            if !Rc::ptr_eq(
                last(parent.parent().as_jsdoc_function_type().parameters()),
                parent,
            ) {
                self.error(
                    Some(node),
                    &Diagnostics::A_rest_parameter_must_be_last_in_a_parameter_list,
                    None,
                );
            }
            return;
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
            return;
        }

        let param = get_parameter_symbol_from_jsdoc(param_tag);
        if param.is_none() {
            return;
        }
        let param = param.as_ref().unwrap();

        let host = get_host_signature_from_jsdoc(param_tag);
        if match host.as_ref() {
            None => true,
            Some(host) => !matches!(
                last(host.as_signature_declaration().parameters()).maybe_symbol().as_ref(),
                Some(symbol) if Rc::ptr_eq(
                    symbol,
                    param
                )
            ),
        } {
            self.error(
                Some(node),
                &Diagnostics::A_rest_parameter_must_be_last_in_a_parameter_list,
                None,
            );
        }
    }

    pub(super) fn get_type_from_jsdoc_variadic_type(
        &self,
        node: &Node, /*JSDocVariadicType*/
    ) -> Rc<Type> {
        let ref type_ =
            self.get_type_from_type_node_(node.as_base_jsdoc_unary_type().type_.as_ref().unwrap());
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
                let symbol = get_parameter_symbol_from_jsdoc(param_tag);
                if match last_param_declaration.as_ref() {
                    None => true,
                    Some(last_param_declaration) => {
                        matches!(
                            symbol.as_ref(),
                            Some(symbol) if matches!(
                                last_param_declaration.maybe_symbol().as_ref(),
                                Some(last_param_declaration_symbol) if Rc::ptr_eq(
                                    last_param_declaration_symbol,
                                    symbol
                                )
                            )
                        ) && is_rest_parameter(last_param_declaration)
                    }
                } {
                    return self.create_array_type(type_, None);
                }
            }
        }
        if is_parameter(parent) && is_jsdoc_function_type(&parent.parent()) {
            return self.create_array_type(type_, None);
        }
        self.add_optionality(type_, None, None)
    }

    pub(super) fn check_node_deferred(&self, node: &Node) {
        let ref enclosing_file = get_source_file_of_node(Some(node)).unwrap();
        let links = self.get_node_links(enclosing_file);
        if !(*links)
            .borrow()
            .flags
            .intersects(NodeCheckFlags::TypeChecked)
        {
            let mut links = links.borrow_mut();
            if links.deferred_nodes.is_none() {
                links.deferred_nodes = Some(HashMap::new());
            }
            let id = get_node_id(node);
            links
                .deferred_nodes
                .as_mut()
                .unwrap()
                .insert(id, node.node_wrapper());
        }
    }

    pub(super) fn check_deferred_nodes(&self, context: &Node /*SourceFile*/) {
        let links = self.get_node_links(context);
        let links_deferred_nodes = (*links)
            .borrow()
            .deferred_nodes
            .as_ref()
            .map(|links_deferred_nodes| links_deferred_nodes.values().cloned().collect::<Vec<_>>());
        if let Some(links_deferred_nodes) = links_deferred_nodes.as_ref() {
            for links_deferred_node in links_deferred_nodes {
                self.check_deferred_node(links_deferred_node);
            }
        }
    }

    pub(super) fn check_deferred_node(&self, node: &Node) {
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
                self.resolve_untyped_call(node);
            }
            SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::MethodSignature => {
                self.check_function_expression_or_object_literal_method_deferred(node);
            }
            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                self.check_accessor_declaration(node);
            }
            SyntaxKind::ClassExpression => {
                self.check_class_expression_deferred(node);
            }
            SyntaxKind::JsxSelfClosingElement => {
                self.check_jsx_self_closing_element_deferred(node);
            }
            SyntaxKind::JsxElement => {
                self.check_jsx_element_deferred(node);
            }
            _ => (),
        }
        self.set_current_node(save_current_node);
        // tracing?.pop();
    }

    pub(super) fn check_source_file(&self, node: &Node /*SourceFile*/) {
        // tracing?.push(tracing.Phase.Check, "checkSourceFile", { path: node.path }, /*separateBeginAndEnd*/ true);
        // performance.mark("beforeCheck");
        self.check_source_file_worker(node);
        // performance.mark("afterCheck");
        // performance.measure("Check", "beforeCheck", "afterCheck");
        // tracing?.pop();
    }

    pub(super) fn unused_is_error(&self, kind: UnusedKind, is_ambient: bool) -> bool {
        if is_ambient {
            return false;
        }
        match kind {
            UnusedKind::Local => self.compiler_options.no_unused_locals == Some(true),
            UnusedKind::Parameter => self.compiler_options.no_unused_parameters == Some(true),
            _ => Debug_.assert_never(kind, None),
        }
    }

    pub(super) fn get_potentially_unused_identifiers(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Vec<Rc<Node /*PotentiallyUnusedIdentifier*/>> {
        self.all_potentially_unused_identifiers()
            .get(&source_file.as_source_file().path())
            .cloned()
            .unwrap_or_else(|| vec![])
    }

    pub(super) fn check_source_file_worker(&self, node: &Node /*SourceFile*/) {
        let links = self.get_node_links(node);
        if !(*links)
            .borrow()
            .flags
            .intersects(NodeCheckFlags::TypeChecked)
        {
            if skip_type_checking(node, &self.compiler_options, |file_name| {
                self.host.is_source_of_project_reference_redirect(file_name)
            }) {
                return;
            }

            self.check_grammar_source_file(node);

            clear(&mut self.potential_this_collisions());
            clear(&mut self.potential_new_target_collisions());
            clear(&mut self.potential_weak_map_set_collisions());
            clear(&mut self.potential_reflect_collisions());

            let node_as_source_file = node.as_source_file();
            for_each(&node_as_source_file.statements, |statement, _| {
                self.check_source_element(Some(&**statement));
                Option::<()>::None
            });
            self.check_source_element(Some(&*node_as_source_file.end_of_file_token));

            self.check_deferred_nodes(node);

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
                        if !contains_parse_error(containing_node)
                            && self.unused_is_error(
                                kind,
                                containing_node.flags().intersects(NodeFlags::Ambient),
                            )
                        {
                            self.diagnostics().add(diag);
                        }
                    },
                );
            }

            if self.compiler_options.imports_not_used_as_values
                == Some(ImportsNotUsedAsValues::Error)
                && !node_as_source_file.is_declaration_file()
                && is_external_module(node)
            {
                self.check_imports_for_type_only_conversion(node);
            }

            if is_external_or_common_js_module(node) {
                self.check_external_module_exports(node);
            }

            {
                let mut potential_this_collisions = self.potential_this_collisions();
                if !potential_this_collisions.is_empty() {
                    for_each(
                        &*potential_this_collisions,
                        |potential_this_collision: &Rc<Node>, _| -> Option<()> {
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
                        |potential_new_target_collision: &Rc<Node>, _| -> Option<()> {
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
                        |potential_weak_map_set_collision: &Rc<Node>, _| -> Option<()> {
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
                        |potential_reflect_collision: &Rc<Node>, _| -> Option<()> {
                            self.check_reflect_collision(potential_reflect_collision);
                            None
                        },
                    );
                    clear(&mut potential_reflect_collisions);
                }
            }

            links.borrow_mut().flags |= NodeCheckFlags::TypeChecked;
        }
    }

    pub fn get_diagnostics<TSourceFile: Borrow<Node>>(
        &self,
        source_file: Option<TSourceFile /*SourceFile*/>,
        ct: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        // try {
        self.set_cancellation_token(ct);
        let ret = self.get_diagnostics_worker(source_file);
        // }
        // finally {
        self.set_cancellation_token(None);
        // }
        ret
    }

    pub(super) fn get_diagnostics_worker<TSourceFile: Borrow<Node>>(
        &self,
        source_file: Option<TSourceFile /*SourceFile*/>,
    ) -> Vec<Rc<Diagnostic>> {
        self.throw_if_non_diagnostics_producing();
        if let Some(source_file) = source_file {
            let source_file = source_file.borrow();
            let previous_global_diagnostics = self.diagnostics().get_global_diagnostics();
            let previous_global_diagnostics_size = previous_global_diagnostics.len();

            self.check_source_file(source_file);

            let semantic_diagnostics = self
                .diagnostics()
                .get_diagnostics(Some(&source_file.as_source_file().file_name()));
            let current_global_diagnostics = self.diagnostics().get_global_diagnostics();
            if current_global_diagnostics.len() != previous_global_diagnostics.len() {
                // const deferredGlobalDiagnostics = relativeComplement(previousGlobalDiagnostics, currentGlobalDiagnostics, compareDiagnostics);
                // this looks to me (vs the Typescript version above) like we only append to these global diagnostics so should just be the slice at the end?
                let deferred_global_diagnostics =
                    current_global_diagnostics[previous_global_diagnostics.len()..].to_owned();
                return concatenate(deferred_global_diagnostics, semantic_diagnostics);
            }
            // else if previousGlobalDiagnosticsSize === 0 && currentGlobalDiagnostics.length > 0 {
            //     return concatenate(currentGlobalDiagnostics, semanticDiagnostics);
            // }
            return semantic_diagnostics;
        }

        for_each(
            self.host.get_source_files(),
            |source_file: &Rc<Node>, _| -> Option<()> {
                self.check_source_file(source_file);
                None
            },
        );
        self.diagnostics().get_diagnostics(None)
    }

    pub(super) fn throw_if_non_diagnostics_producing(&self) {
        unimplemented!()
    }

    pub(super) fn get_symbols_in_scope_(
        &self,
        location_in: &Node,
        meaning: SymbolFlags,
    ) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn is_type_declaration(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_type_reference_identifier(&self, node: &Node /*EntityName*/) -> bool {
        unimplemented!()
    }

    pub(super) fn for_each_enclosing_class<TReturn, TCallback: FnMut(&Node) -> Option<TReturn>>(
        &self,
        node: &Node,
        callback: TCallback,
    ) -> Option<TReturn> {
        unimplemented!()
    }

    pub(super) fn is_node_used_during_class_initialization(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_node_within_class(
        &self,
        node: &Node,
        class_declaration: &Node, /*ClassLikeDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_in_right_side_of_import_or_export_assignment(
        &self,
        node: &Node, /*EntityName*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_symbol_at_location_(
        &self,
        node: &Node,
        ignore_errors: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_index_infos_at_location_(&self, node: &Node) -> Option<Vec<Rc<IndexInfo>>> {
        unimplemented!()
    }

    pub(super) fn get_shorthand_assignment_value_symbol_<TNode: Borrow<Node>>(
        &self,
        location: Option<TNode>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_export_specifier_local_target_symbol_(
        &self,
        node: &Node, /*Identifier | ExportSpecifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_node(&self, node: &Node) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_of_assignment_pattern_(
        &self,
        expr: &Node, /*AssignmentPattern*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_property_symbol_of_destructuring_assignment_(
        &self,
        location: &Node, /*Identifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_parent_type_of_class_element(
        &self,
        node: &Node, /*ClassElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_class_element_property_key_type(
        &self,
        element: &Node, /*ClassElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_augmented_properties_of_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn type_has_call_or_construct_signatures(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_const_enum_or_const_enum_only_module(&self, s: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn is_referenced_alias_declaration(
        &self,
        node: &Node,
        check_children: Option<bool>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_implementation_of_overload_(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_node_check_flags(&self, node: &Node) -> NodeCheckFlags {
        unimplemented!()
    }

    pub(super) fn get_enum_member_value(
        &self,
        node: &Node, /*EnumMember*/
    ) -> Option<StringOrNumber> {
        unimplemented!()
    }

    pub(super) fn can_have_constant_value(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn get_constant_value_(
        &self,
        node: &Node, /*EnumMember | AccessExpression*/
    ) -> Option<StringOrNumber> {
        unimplemented!()
    }

    pub(super) fn is_function_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_referenced_value_symbol(
        &self,
        reference: &Node, /*Identifier*/
        start_in_declaration_container: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_factory_entity(
        &self,
        location: &Node,
    ) -> Option<Rc<Node /*EntityName*/>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_fragment_factory_entity(
        &self,
        location: &Node,
    ) -> Option<Rc<Node /*EntityName*/>> {
        unimplemented!()
    }

    pub(super) fn create_resolver(&self) -> Rc<dyn EmitResolverDebuggable> {
        Rc::new(EmitResolverCreateResolver::new())
    }

    pub(super) fn get_external_module_file_from_declaration(
        &self,
        declaration: &Node, /*AnyImportOrReExport | ModuleDeclaration | ImportTypeNode | ImportCall*/
    ) -> Option<Rc<Node /*SourceFile*/>> {
        let specifier = if declaration.kind() == SyntaxKind::ModuleDeclaration {
            try_cast(
                declaration.as_module_declaration().name(),
                |node: &Rc<Node>| is_string_literal(node),
            )
        } else {
            get_external_module_name(declaration)
        };
        let module_symbol = self.resolve_external_module_name_worker(
            specifier.as_ref().unwrap(),
            specifier.as_ref().unwrap(),
            None,
            None,
        )?;
        get_declaration_of_kind(&module_symbol, SyntaxKind::SourceFile)
    }

    pub(super) fn initialize_type_checker(&mut self) {
        for file in self.host.get_source_files() {
            bind_source_file(file, self.compiler_options.clone());
            // println!("post-binding: {:#?}", file);
        }

        *self.maybe_amalgamated_duplicates() = Some(HashMap::new());

        let mut augmentations: Option<Vec<Vec<Rc<Node /*StringLiteral | Identifier*/>>>> = None;
        for file in self.host.get_source_files() {
            let file_as_source_file = file.as_source_file();
            if file_as_source_file.maybe_redirect_info().is_some() {
                continue;
            }
            if !is_external_or_common_js_module(file) {
                let file_global_this_symbol = (**file.locals())
                    .borrow()
                    .get(&__String::new("globalThis".to_owned()))
                    .cloned();
                if let Some(ref file_global_this_symbol_declarations) = file_global_this_symbol
                    .as_ref()
                    .and_then(|file_global_this_symbol| {
                        file_global_this_symbol.maybe_declarations().clone()
                    })
                {
                    for declaration in file_global_this_symbol_declarations {
                        self.diagnostics().add(
                            Rc::new(
                                create_diagnostic_for_node(
                                    declaration,
                                    &Diagnostics::Declaration_name_conflicts_with_built_in_global_identifier_0,
                                    Some(vec![
                                        "globalThis".to_owned()
                                    ])
                                ).into()
                            )
                        );
                    }
                }
                self.merge_symbol_table(
                    &mut *self.globals_mut(),
                    &RefCell::borrow(&file.locals()),
                    None,
                );
            }
            if let Some(file_js_global_augmentations) =
                file_as_source_file.maybe_js_global_augmentations().clone()
            {
                self.merge_symbol_table(
                    &mut *self.globals_mut(),
                    &(*file_js_global_augmentations).borrow(),
                    None,
                );
            }
            if let Some(file_pattern_ambient_modules) = file_as_source_file
                .maybe_pattern_ambient_modules()
                .as_ref()
                .filter(|file_pattern_ambient_modules| !file_pattern_ambient_modules.is_empty())
            {
                let mut pattern_ambient_modules = self.maybe_pattern_ambient_modules();
                *pattern_ambient_modules = Some(concatenate(
                    pattern_ambient_modules.clone().unwrap_or_else(|| vec![]),
                    file_pattern_ambient_modules.clone(),
                ));
            }
            let file_module_augmentations = file_as_source_file.maybe_module_augmentations();
            // TODO: this should end up being .unwrap()'able
            // let file_module_augmentations = file_module_augmentations.as_ref().unwrap();
            let file_module_augmentations =
                file_module_augmentations.clone().unwrap_or_else(|| vec![]);
            let file_module_augmentations = &file_module_augmentations;
            if !file_module_augmentations.is_empty() {
                if augmentations.is_none() {
                    augmentations = Some(vec![]);
                }
                augmentations
                    .as_mut()
                    .unwrap()
                    .push(file_module_augmentations.clone());
            }
            if let Some(file_symbol_global_exports) = file
                .maybe_symbol()
                .as_ref()
                .and_then(|file_symbol| file_symbol.maybe_global_exports().clone())
            {
                let source = file_symbol_global_exports;
                let mut globals = self.globals_mut();
                for (id, source_symbol) in &*(*source).borrow() {
                    if !globals.contains_key(id) {
                        globals.insert(id.clone(), source_symbol.clone());
                    }
                }
            }
        }

        if let Some(augmentations) = augmentations.as_ref() {
            for list in augmentations {
                for augmentation in list {
                    if !is_global_scope_augmentation(&augmentation.parent()) {
                        continue;
                    }
                    self.merge_module_augmentation(augmentation);
                }
            }
        }

        self.add_to_symbol_table(
            &mut *self.globals_mut(),
            &self.builtin_globals(),
            &Diagnostics::Declaration_name_conflicts_with_built_in_global_identifier_0,
        );

        self.get_symbol_links(&self.undefined_symbol())
            .borrow_mut()
            .type_ = Some(self.undefined_widening_type());
        self.get_symbol_links(&self.arguments_symbol())
            .borrow_mut()
            .type_ = self.get_global_type(&__String::new("IArguments".to_owned()), 0, true);
        self.get_symbol_links(&self.unknown_symbol())
            .borrow_mut()
            .type_ = Some(self.error_type());
        self.get_symbol_links(&self.global_this_symbol())
            .borrow_mut()
            .type_ = Some(Rc::new(
            self.create_object_type(ObjectFlags::Anonymous, Some(self.global_this_symbol()))
                .into(),
        ));

        self.global_array_type = self.get_global_type(&__String::new("Array".to_owned()), 1, true);
        self.global_object_type =
            self.get_global_type(&__String::new("Object".to_owned()), 0, true);
        self.global_function_type =
            self.get_global_type(&__String::new("Function".to_owned()), 0, true);
        self.global_callable_function_type = Some(
            if self.strict_bind_call_apply {
                self.get_global_type(&__String::new("CallableFunction".to_owned()), 0, true)
            } else {
                None
            }
            .unwrap_or_else(|| self.global_function_type()),
        );
        self.global_newable_function_type = Some(
            if self.strict_bind_call_apply {
                self.get_global_type(&__String::new("NewableFunction".to_owned()), 0, true)
            } else {
                None
            }
            .unwrap_or_else(|| self.global_function_type()),
        );
        self.global_string_type =
            self.get_global_type(&__String::new("String".to_owned()), 0, true);
        self.global_number_type =
            self.get_global_type(&__String::new("Number".to_owned()), 0, true);
        self.global_boolean_type =
            self.get_global_type(&__String::new("Boolean".to_owned()), 0, true);
        self.global_reg_exp_type =
            self.get_global_type(&__String::new("RegExp".to_owned()), 0, true);
        self.any_array_type = Some(self.create_array_type(&self.any_type(), None));

        self.auto_array_type = Some(self.create_array_type(&self.auto_type(), None));
        if Rc::ptr_eq(&self.auto_array_type(), &self.empty_object_type()) {
            self.auto_array_type = Some(Rc::new(
                self.create_anonymous_type(
                    Option::<&Symbol>::None,
                    self.empty_symbols(),
                    vec![],
                    vec![],
                    vec![],
                )
                .into(),
            ));
        }

        self.global_readonly_array_type = self
            .get_global_type_or_undefined(&__String::new("ReadonlyArray".to_owned()), Some(1))
            .or_else(|| self.global_array_type.clone());
        self.any_readonly_array_type = Some(
            if let Some(global_readonly_array_type) = self.global_readonly_array_type.as_ref() {
                self.create_type_from_generic_global_type(
                    global_readonly_array_type,
                    vec![self.any_type()],
                )
            } else {
                self.any_array_type()
            },
        );
        self.global_this_type =
            self.get_global_type_or_undefined(&__String::new("ThisType".to_owned()), Some(1));

        if let Some(augmentations) = augmentations.as_ref() {
            for list in augmentations {
                for augmentation in list {
                    if is_global_scope_augmentation(&augmentation.parent()) {
                        continue;
                    }
                    self.merge_module_augmentation(augmentation);
                }
            }
        }

        for duplicate_info_for_files in self
            .maybe_amalgamated_duplicates()
            .as_ref()
            .unwrap()
            .values()
        {
            let first_file = &duplicate_info_for_files.first_file;
            let second_file = &duplicate_info_for_files.second_file;
            let conflicting_symbols = &duplicate_info_for_files.conflicting_symbols;
            if conflicting_symbols.len() < 8 {
                for (symbol_name, duplicate_info_for_symbol) in conflicting_symbols {
                    let is_block_scoped = duplicate_info_for_symbol.is_block_scoped;
                    let first_file_locations = &duplicate_info_for_symbol.first_file_locations;
                    let second_file_locations = &duplicate_info_for_symbol.second_file_locations;
                    let message = if is_block_scoped {
                        &*Diagnostics::Cannot_redeclare_block_scoped_variable_0
                    } else {
                        &*Diagnostics::Duplicate_identifier_0
                    };
                    for node in first_file_locations {
                        self.add_duplicate_declaration_error(
                            node,
                            message,
                            symbol_name,
                            Some(second_file_locations),
                        );
                    }
                    for node in second_file_locations {
                        self.add_duplicate_declaration_error(
                            node,
                            message,
                            symbol_name,
                            Some(first_file_locations),
                        );
                    }
                }
            } else {
                let list: String = conflicting_symbols
                    .keys()
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", ");
                self.diagnostics().add(
                    {
                        let diagnostic: Rc<Diagnostic> = Rc::new(
                            create_diagnostic_for_node(
                                first_file,
                                &Diagnostics::Definitions_of_the_following_identifiers_conflict_with_those_in_another_file_Colon_0,
                                Some(vec![
                                    list.clone()
                                ])
                            ).into()
                        );
                        add_related_info(
                            &diagnostic,
                            vec![
                                Rc::new(
                                    create_diagnostic_for_node(
                                        second_file,
                                        &Diagnostics::Conflicts_are_in_this_file,
                                        None,
                                    ).into()
                                )
                            ]
                        );
                        diagnostic
                    }
                );
                self.diagnostics().add(
                    {
                        let diagnostic: Rc<Diagnostic> = Rc::new(
                            create_diagnostic_for_node(
                                second_file,
                                &Diagnostics::Definitions_of_the_following_identifiers_conflict_with_those_in_another_file_Colon_0,
                                Some(vec![
                                    list.clone()
                                ])
                            ).into()
                        );
                        add_related_info(
                            &diagnostic,
                            vec![
                                Rc::new(
                                    create_diagnostic_for_node(
                                        first_file,
                                        &Diagnostics::Conflicts_are_in_this_file,
                                        None,
                                    ).into()
                                )
                            ]
                        );
                        diagnostic
                    }
                );
            }
        }
        *self.maybe_amalgamated_duplicates() = None;
    }

    pub(super) fn check_external_emit_helpers(
        &self,
        location: &Node,
        helpers: ExternalEmitHelpers,
    ) {
        if self.requested_external_emit_helpers() & helpers != helpers
            && self.compiler_options.import_helpers == Some(true)
        {
            let source_file = get_source_file_of_node(Some(location)).unwrap();
            if is_effective_external_module(&source_file, &self.compiler_options)
                && !location.flags().intersects(NodeFlags::Ambient)
            {
                let helpers_module = self.resolve_helpers_module(&source_file, location);
                if !Rc::ptr_eq(&helpers_module, &self.unknown_symbol()) {
                    let unchecked_helpers = helpers & !self.requested_external_emit_helpers();
                    let mut helper = ExternalEmitHelpers::FirstEmitHelper;
                    while helper <= ExternalEmitHelpers::LastEmitHelper {
                        if unchecked_helpers.intersects(helper) {
                            let name = self.get_helper_name(helper);
                            let symbol = self.get_symbol(
                                &(*helpers_module.maybe_exports().clone().unwrap()).borrow(),
                                &escape_leading_underscores(name),
                                SymbolFlags::Value,
                            );
                            match symbol.as_ref() {
                                None => {
                                    self.error(
                                        Some(location),
                                        &Diagnostics::This_syntax_requires_an_imported_helper_named_1_which_does_not_exist_in_0_Consider_upgrading_your_version_of_0,
                                        Some(vec![
                                            external_helpers_module_name_text.to_owned(),
                                            name.to_owned(),
                                        ])
                                    );
                                }
                                Some(symbol) => {
                                    if helper.intersects(ExternalEmitHelpers::ClassPrivateFieldGet)
                                    {
                                        if !some(
                                            Some(&*self.get_signatures_of_symbol(Some(&**symbol))),
                                            Some(|signature: &Rc<Signature>| {
                                                self.get_parameter_count(signature) > 3
                                            }),
                                        ) {
                                            self.error(
                                                Some(location),
                                                &Diagnostics::This_syntax_requires_an_imported_helper_named_1_with_2_parameters_which_is_not_compatible_with_the_one_in_0_Consider_upgrading_your_version_of_0,
                                                Some(vec![
                                                    external_helpers_module_name_text.to_owned(),
                                                    name.to_owned(),
                                                    4_usize.to_string(),
                                                ])
                                            );
                                        }
                                    } else if helper
                                        .intersects(ExternalEmitHelpers::ClassPrivateFieldSet)
                                    {
                                        if !some(
                                            Some(&*self.get_signatures_of_symbol(Some(&**symbol))),
                                            Some(|signature: &Rc<Signature>| {
                                                self.get_parameter_count(signature) > 4
                                            }),
                                        ) {
                                            self.error(
                                                Some(location),
                                                &Diagnostics::This_syntax_requires_an_imported_helper_named_1_with_2_parameters_which_is_not_compatible_with_the_one_in_0_Consider_upgrading_your_version_of_0,
                                                Some(vec![
                                                    external_helpers_module_name_text.to_owned(),
                                                    name.to_owned(),
                                                    5_usize.to_string(),
                                                ])
                                            );
                                        }
                                    } else if helper.intersects(ExternalEmitHelpers::SpreadArray) {
                                        if !some(
                                            Some(&*self.get_signatures_of_symbol(Some(&**symbol))),
                                            Some(|signature: &Rc<Signature>| {
                                                self.get_parameter_count(signature) > 2
                                            }),
                                        ) {
                                            self.error(
                                                Some(location),
                                                &Diagnostics::This_syntax_requires_an_imported_helper_named_1_with_2_parameters_which_is_not_compatible_with_the_one_in_0_Consider_upgrading_your_version_of_0,
                                                Some(vec![
                                                    external_helpers_module_name_text.to_owned(),
                                                    name.to_owned(),
                                                    3_usize.to_string(),
                                                ])
                                            );
                                        }
                                    }
                                }
                            }
                        }
                        // helper <<= 1;
                        helper = ExternalEmitHelpers::from_bits(helper.bits() << 1).unwrap();
                    }
                }
                self.set_requested_external_emit_helpers(
                    self.requested_external_emit_helpers() | helpers,
                );
            }
        }
    }

    pub(super) fn get_helper_name(&self, helper: ExternalEmitHelpers) -> &'static str {
        match helper {
            ExternalEmitHelpers::Extends => "__extends",
            ExternalEmitHelpers::Assign => "__assign",
            ExternalEmitHelpers::Rest => "__rest",
            ExternalEmitHelpers::Decorate => "__decorate",
            ExternalEmitHelpers::Metadata => "__metadata",
            ExternalEmitHelpers::Param => "__param",
            ExternalEmitHelpers::Awaiter => "__awaiter",
            ExternalEmitHelpers::Generator => "__generator",
            ExternalEmitHelpers::Values => "__values",
            ExternalEmitHelpers::Read => "__read",
            ExternalEmitHelpers::SpreadArray => "__spreadArray",
            ExternalEmitHelpers::Await => "__await",
            ExternalEmitHelpers::AsyncGenerator => "__asyncGenerator",
            ExternalEmitHelpers::AsyncDelegator => "__asyncDelegator",
            ExternalEmitHelpers::AsyncValues => "__asyncValues",
            ExternalEmitHelpers::ExportStar => "__exportStar",
            ExternalEmitHelpers::ImportStar => "__importStar",
            ExternalEmitHelpers::ImportDefault => "__importDefault",
            ExternalEmitHelpers::MakeTemplateObject => "__makeTemplateObject",
            ExternalEmitHelpers::ClassPrivateFieldGet => "__classPrivateFieldGet",
            ExternalEmitHelpers::ClassPrivateFieldSet => "__classPrivateFieldSet",
            ExternalEmitHelpers::ClassPrivateFieldIn => "__classPrivateFieldIn",
            ExternalEmitHelpers::CreateBinding => "__createBinding",
            _ => Debug_.fail(Some("Unrecognized helper")),
        }
    }

    pub(super) fn resolve_helpers_module(
        &self,
        node: &Node, /*SourceFile*/
        error_node: &Node,
    ) -> Rc<Symbol> {
        let mut external_helpers_module = self.maybe_external_helpers_module();
        if external_helpers_module.is_none() {
            *external_helpers_module = Some(
                self.resolve_external_module(
                    node,
                    external_helpers_module_name_text,
                    Some(&Diagnostics::This_syntax_requires_an_imported_helper_but_module_0_cannot_be_found),
                    error_node,
                    None,
                ).unwrap_or_else(|| self.unknown_symbol())
            );
        }
        external_helpers_module.clone().unwrap()
    }

    pub(super) fn check_grammar_decorators_and_modifiers(&self, node: &Node) -> bool {
        self.check_grammar_decorators(node) || self.check_grammar_modifiers(node)
    }

    pub(super) fn check_grammar_decorators(&self, node: &Node) -> bool {
        let node_decorators = node.maybe_decorators();
        if node_decorators.is_none() {
            return false;
        }
        let node_decorators = node_decorators.as_ref().unwrap();
        if !node_can_be_decorated(node, Some(node.parent()), node.parent().maybe_parent()) {
            if node.kind() == SyntaxKind::MethodDeclaration
                && !node_is_present(node.as_method_declaration().maybe_body())
            {
                return self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::A_decorator_can_only_decorate_a_method_implementation_not_an_overload,
                    None,
                );
            } else {
                return self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::Decorators_are_not_valid_here,
                    None,
                );
            }
        } else if matches!(
            node.kind(),
            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor
        ) {
            let accessors = get_all_accessor_declarations(
                node.parent().as_class_like_declaration().members(),
                node,
            );
            if accessors.first_accessor.maybe_decorators().is_some()
                && matches!(
                    accessors.second_accessor.as_ref(),
                    Some(accessors_second_accessor) if ptr::eq(
                        node,
                        &**accessors_second_accessor
                    )
                )
            {
                return self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::Decorators_cannot_be_applied_to_multiple_get_Slashset_accessors_of_the_same_name,
                    None,
                );
            }
        }
        false
    }

    pub(super) fn check_grammar_modifiers(&self, node: &Node) -> bool {
        let quick_result = self.report_obvious_modifier_errors(node);
        if let Some(quick_result) = quick_result {
            return quick_result;
        }

        let mut last_static: Option<Rc<Node>> = None;
        let mut last_declare: Option<Rc<Node>> = None;
        let mut last_async: Option<Rc<Node>> = None;
        let mut last_readonly: Option<Rc<Node>> = None;
        let mut last_override: Option<Rc<Node>> = None;
        let mut flags = ModifierFlags::None;
        for modifier in node.maybe_modifiers().as_ref().unwrap() {
            if modifier.kind() != SyntaxKind::ReadonlyKeyword {
                if matches!(
                    node.kind(),
                    SyntaxKind::PropertySignature | SyntaxKind::MethodSignature
                ) {
                    return self.grammar_error_on_node(
                        modifier,
                        &Diagnostics::_0_modifier_cannot_appear_on_a_type_member,
                        Some(vec![token_to_string(modifier.kind()).unwrap().to_owned()]),
                    );
                }
                if node.kind() == SyntaxKind::IndexSignature
                    && (modifier.kind() != SyntaxKind::StaticKeyword
                        || !is_class_like(&node.parent()))
                {
                    return self.grammar_error_on_node(
                        modifier,
                        &Diagnostics::_0_modifier_cannot_appear_on_an_index_signature,
                        Some(vec![token_to_string(modifier.kind()).unwrap().to_owned()]),
                    );
                }
            }
            match modifier.kind() {
                SyntaxKind::ConstKeyword => {
                    if node.kind() != SyntaxKind::EnumDeclaration {
                        return self.grammar_error_on_node(
                            node,
                            &Diagnostics::A_class_member_cannot_have_the_0_keyword,
                            Some(vec![token_to_string(SyntaxKind::ConstKeyword)
                                .unwrap()
                                .to_owned()]),
                        );
                    }
                }
                SyntaxKind::OverrideKeyword => {
                    if flags.intersects(ModifierFlags::Override) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["override".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Ambient) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                            Some(vec!["override".to_owned(), "declare".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Readonly) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["override".to_owned(), "readonly".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["override".to_owned(), "async".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Override;
                    last_override = Some(modifier.clone());
                }

                SyntaxKind::PublicKeyword
                | SyntaxKind::ProtectedKeyword
                | SyntaxKind::PrivateKeyword => {
                    let text = self.visibility_to_string(modifier_to_flag(modifier.kind()));

                    if flags.intersects(ModifierFlags::AccessibilityModifier) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::Accessibility_modifier_already_seen,
                            None,
                        );
                    } else if flags.intersects(ModifierFlags::Override) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec![text.to_owned(), "override".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Static) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec![text.to_owned(), "static".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Readonly) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec![text.to_owned(), "readonly".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec![text.to_owned(), "async".to_owned()]),
                        );
                    } else if matches!(
                        node.parent().kind(),
                        SyntaxKind::ModuleBlock | SyntaxKind::SourceFile
                    ) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_module_or_namespace_element,
                            Some(vec![
                                text.to_owned(),
                            ])
                        );
                    } else if flags.intersects(ModifierFlags::Abstract) {
                        if modifier.kind() == SyntaxKind::PrivateKeyword {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                                Some(vec![text.to_owned(), "abstract".to_owned()]),
                            );
                        } else {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_must_precede_1_modifier,
                                Some(vec![text.to_owned(), "abstract".to_owned()]),
                            );
                        }
                    } else if is_private_identifier_class_element_declaration(node) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::An_accessibility_modifier_cannot_be_used_with_a_private_identifier,
                            None
                        );
                    }
                    flags |= modifier_to_flag(modifier.kind());
                }

                SyntaxKind::StaticKeyword => {
                    if flags.intersects(ModifierFlags::Static) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["static".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Readonly) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["static".to_owned(), "readonly".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["static".to_owned(), "async".to_owned()]),
                        );
                    } else if matches!(
                        node.parent().kind(),
                        SyntaxKind::ModuleBlock | SyntaxKind::SourceFile
                    ) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_module_or_namespace_element,
                            Some(vec![
                                "static".to_owned(),
                            ])
                        );
                    } else if node.kind() == SyntaxKind::Parameter {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_parameter,
                            Some(vec!["static".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Abstract) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                            Some(vec!["static".to_owned(), "abstract".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Override) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["static".to_owned(), "override".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Static;
                    last_static = Some(modifier.clone());
                }

                SyntaxKind::ReadonlyKeyword => {
                    if flags.intersects(ModifierFlags::Readonly) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["readonly".to_owned()]),
                        );
                    } else if !matches!(
                        node.kind(),
                        SyntaxKind::PropertyDeclaration
                            | SyntaxKind::PropertySignature
                            | SyntaxKind::IndexSignature
                            | SyntaxKind::Parameter
                    ) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::readonly_modifier_can_only_appear_on_a_property_declaration_or_index_signature,
                            None,
                        );
                    }
                    flags |= ModifierFlags::Readonly;
                    last_readonly = Some(modifier.clone());
                }

                SyntaxKind::ExportKeyword => {
                    if flags.intersects(ModifierFlags::Export) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["export".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Ambient) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["export".to_owned(), "declare".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Abstract) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["export".to_owned(), "abstract".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["export".to_owned(), "async".to_owned()]),
                        );
                    } else if is_class_like(&node.parent()) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_class_elements_of_this_kind,
                            Some(vec!["export".to_owned()]),
                        );
                    } else if node.kind() == SyntaxKind::Parameter {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_parameter,
                            Some(vec!["export".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Export;
                }
                SyntaxKind::DefaultKeyword => {
                    let container = if node.parent().kind() == SyntaxKind::SourceFile {
                        node.parent()
                    } else {
                        node.parent().parent()
                    };
                    if container.kind() == SyntaxKind::ModuleDeclaration
                        && !is_ambient_module(&container)
                    {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::A_default_export_can_only_be_used_in_an_ECMAScript_style_module,
                            None,
                        );
                    } else if !flags.intersects(ModifierFlags::Export) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["export".to_owned(), "default".to_owned()]),
                        );
                    }

                    flags |= ModifierFlags::Default;
                }
                SyntaxKind::DeclareKeyword => {
                    if flags.intersects(ModifierFlags::Ambient) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["declare".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_in_an_ambient_context,
                            Some(vec!["async".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Override) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_in_an_ambient_context,
                            Some(vec!["override".to_owned()]),
                        );
                    } else if is_class_like(&node.parent()) && !is_property_declaration(node) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_class_elements_of_this_kind,
                            Some(vec!["declare".to_owned()]),
                        );
                    } else if node.kind() == SyntaxKind::Parameter {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_parameter,
                            Some(vec!["declare".to_owned()]),
                        );
                    } else if node.parent().flags().intersects(NodeFlags::Ambient)
                        && node.parent().kind() == SyntaxKind::ModuleBlock
                    {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::A_declare_modifier_cannot_be_used_in_an_already_ambient_context,
                            None,
                        );
                    } else if is_private_identifier_class_element_declaration(node) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_a_private_identifier,
                            Some(vec!["declare".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Ambient;
                    last_declare = Some(modifier.clone());
                }

                SyntaxKind::AbstractKeyword => {
                    if flags.intersects(ModifierFlags::Abstract) {
                        self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["abstract".to_owned()]),
                        );
                    }
                    if !matches!(
                        node.kind(),
                        SyntaxKind::ClassDeclaration | SyntaxKind::ConstructorType
                    ) {
                        if !matches!(
                            node.kind(),
                            SyntaxKind::MethodDeclaration
                                | SyntaxKind::PropertyDeclaration
                                | SyntaxKind::GetAccessor
                                | SyntaxKind::SetAccessor
                        ) {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::abstract_modifier_can_only_appear_on_a_class_method_or_property_declaration,
                                None,
                            );
                        }
                        if !(node.parent().kind() == SyntaxKind::ClassDeclaration
                            && has_syntactic_modifier(&node.parent(), ModifierFlags::Abstract))
                        {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::Abstract_methods_can_only_appear_within_an_abstract_class,
                                None,
                            );
                        }
                        if flags.intersects(ModifierFlags::Static) {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                                Some(vec!["static".to_owned(), "abstract".to_owned()]),
                            );
                        }
                        if flags.intersects(ModifierFlags::Private) {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                                Some(vec!["private".to_owned(), "abstract".to_owned()]),
                            );
                        }
                        if flags.intersects(ModifierFlags::Async) {
                            if let Some(last_async) = last_async.as_ref() {
                                return self.grammar_error_on_node(
                                    last_async,
                                    &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                                    Some(vec!["async".to_owned(), "abstract".to_owned()]),
                                );
                            }
                        }
                        if flags.intersects(ModifierFlags::Override) {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_must_precede_1_modifier,
                                Some(vec!["abstract".to_owned(), "override".to_owned()]),
                            );
                        }
                    }
                    if is_named_declaration(node)
                        && node.as_named_declaration().name().kind()
                            == SyntaxKind::PrivateIdentifier
                    {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_a_private_identifier,
                            Some(vec!["abstract".to_owned()]),
                        );
                    }

                    flags |= ModifierFlags::Abstract;
                }

                SyntaxKind::AsyncKeyword => {
                    if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["async".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Ambient)
                        || node.parent().flags().intersects(NodeFlags::Ambient)
                    {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_in_an_ambient_context,
                            Some(vec!["async".to_owned()]),
                        );
                    } else if node.kind() == SyntaxKind::Parameter {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_parameter,
                            Some(vec!["async".to_owned()]),
                        );
                    }
                    if flags.intersects(ModifierFlags::Abstract) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                            Some(vec!["async".to_owned(), "abstract".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Async;
                    last_async = Some(modifier.clone());
                }
                _ => (),
            }
        }

        if node.kind() == SyntaxKind::Constructor {
            if flags.intersects(ModifierFlags::Static) {
                return self.grammar_error_on_node(
                    last_static.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["static".to_owned()]),
                );
            }
            if flags.intersects(ModifierFlags::Abstract) {
                return self.grammar_error_on_node(
                    // TODO: this is what's in the Typescript version but seems like it should be lastDeclare instead?
                    last_static.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["abstract".to_owned()]),
                );
            }
            if flags.intersects(ModifierFlags::Override) {
                return self.grammar_error_on_node(
                    last_override.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["override".to_owned()]),
                );
            } else if flags.intersects(ModifierFlags::Async) {
                return self.grammar_error_on_node(
                    last_async.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["async".to_owned()]),
                );
            } else if flags.intersects(ModifierFlags::Readonly) {
                return self.grammar_error_on_node(
                    last_readonly.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["readonly".to_owned()]),
                );
            }
            return false;
        } else if matches!(
            node.kind(),
            SyntaxKind::ImportDeclaration | SyntaxKind::ImportEqualsDeclaration
        ) && flags.intersects(ModifierFlags::Ambient)
        {
            return self.grammar_error_on_node(
                last_declare.as_ref().unwrap(),
                &Diagnostics::A_0_modifier_cannot_be_used_with_an_import_declaration,
                Some(vec!["declare".to_owned()]),
            );
        } else if node.kind() == SyntaxKind::Parameter
            && flags.intersects(ModifierFlags::ParameterPropertyModifier)
            && is_binding_pattern(node.as_parameter_declaration().maybe_name())
        {
            return self.grammar_error_on_node(
                node,
                &Diagnostics::A_parameter_property_may_not_be_declared_using_a_binding_pattern,
                None,
            );
        } else if node.kind() == SyntaxKind::Parameter
            && flags.intersects(ModifierFlags::ParameterPropertyModifier)
            && node.as_parameter_declaration().dot_dot_dot_token.is_some()
        {
            return self.grammar_error_on_node(
                node,
                &Diagnostics::A_parameter_property_cannot_be_declared_using_a_rest_parameter,
                None,
            );
        }
        if flags.intersects(ModifierFlags::Async) {
            return self.check_grammar_async_modifier(node, last_async.as_ref().unwrap());
        }
        false
    }
}
