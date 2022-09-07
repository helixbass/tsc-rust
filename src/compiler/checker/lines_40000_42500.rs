#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{EmitResolverCreateResolver, UnusedKind};
use crate::{
    add_related_info, concatenate, create_diagnostic_for_node, escape_leading_underscores,
    external_helpers_module_name_text, for_each_child, get_all_accessor_declarations,
    get_declaration_of_kind, get_external_module_name, get_source_file_of_node,
    has_syntactic_modifier, is_access_expression, is_ambient_module, is_binding_pattern,
    is_class_like, is_effective_external_module, is_exports_identifier,
    is_global_scope_augmentation, is_in_js_file, is_module_exports_access_expression,
    is_named_declaration, is_private_identifier_class_element_declaration, is_property_declaration,
    is_string_literal, maybe_for_each, modifier_to_flag, node_can_be_decorated, node_is_present,
    some, token_to_string, try_cast, Debug_, Diagnostics, ExternalEmitHelpers,
    FunctionLikeDeclarationInterface, ModifierFlags, NamedDeclarationInterface, NodeArray,
    NodeCheckFlags, NodeFlags, ObjectFlags, Signature, SymbolInterface, SyntaxKind, __String,
    bind_source_file, for_each, is_external_or_common_js_module, CancellationTokenDebuggable,
    Diagnostic, EmitResolverDebuggable, IndexInfo, Node, NodeInterface, StringOrNumber, Symbol,
    SymbolFlags, Type, TypeChecker,
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
        unimplemented!()
    }

    pub(super) fn check_jsdoc_variadic_type(&self, node: &Node /*JSDocVariadicType*/) {
        unimplemented!()
    }

    pub(super) fn get_type_from_jsdoc_variadic_type(
        &self,
        node: &Node, /*JSDocVariadicType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_node_deferred(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn check_source_file(&self, source_file: &Node /*SourceFile*/) {
        self.check_source_file_worker(source_file)
    }

    pub(super) fn unused_is_error(&self, kind: UnusedKind, is_ambient: bool) -> bool {
        unimplemented!()
    }

    pub(super) fn get_potentially_unused_identifiers(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Vec<Rc<Node /*PotentiallyUnusedIdentifier*/>> {
        unimplemented!()
    }

    pub(super) fn check_source_file_worker(&self, node: &Node /*SourceFile*/) {
        if true {
            for_each(&node.as_source_file().statements, |statement, _index| {
                self.check_source_element(Some(&**statement));
                Option::<()>::None
            });
        }
    }

    pub fn get_diagnostics(
        &self,
        source_file: &Node, /*SourceFile*/
        ct: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_worker(source_file)
    }

    pub(super) fn get_diagnostics_worker(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Vec<Rc<Diagnostic>> {
        self.check_source_file(source_file);

        let semantic_diagnostics = self
            .diagnostics()
            .get_diagnostics(Some(&source_file.as_source_file().file_name()));

        semantic_diagnostics
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
