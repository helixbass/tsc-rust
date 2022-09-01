#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use super::{EmitResolverCreateResolver, UnusedKind};
use crate::{
    escape_leading_underscores, external_helpers_module_name_text, get_all_accessor_declarations,
    get_source_file_of_node, has_syntactic_modifier, is_ambient_module, is_binding_pattern,
    is_class_like, is_effective_external_module, is_named_declaration,
    is_private_identifier_class_element_declaration, is_property_declaration, modifier_to_flag,
    node_can_be_decorated, node_is_present, some, token_to_string, Debug_, Diagnostics,
    ExternalEmitHelpers, FunctionLikeDeclarationInterface, ModifierFlags,
    NamedDeclarationInterface, NodeCheckFlags, NodeFlags, Signature, SymbolInterface, SyntaxKind,
    __String, bind_source_file, for_each, is_external_or_common_js_module,
    CancellationTokenDebuggable, Diagnostic, EmitResolverDebuggable, IndexInfo, Node,
    NodeInterface, StringOrNumber, Symbol, SymbolFlags, Type, TypeChecker,
};

impl TypeChecker {
    pub(super) fn is_duplicated_common_js_export(&self, declarations: Option<&[Rc<Node>]>) -> bool {
        unimplemented!()
    }

    pub(super) fn check_source_element<TNodeRef: Borrow<Node>>(&self, node: Option<TNodeRef>) {
        if let Some(node) = node {
            let node = node.borrow();
            self.check_source_element_worker(node);
        }
    }

    pub(super) fn check_source_element_worker(&self, node: &Node) {
        match node {
            Node::PropertySignature(_) => self.check_property_signature(node),
            Node::TypeReferenceNode(_) => self.check_type_reference_node(node),
            Node::KeywordTypeNode(_) | Node::LiteralTypeNode(_) => (),
            Node::ArrayTypeNode(_) => self.check_array_type(node),
            Node::UnionTypeNode(_) => self.check_union_or_intersection_type(node),
            Node::FunctionDeclaration(_) => self.check_function_declaration(node),
            Node::Block(_) => self.check_block(node),
            Node::VariableStatement(_) => self.check_variable_statement(node),
            Node::ExpressionStatement(_) => self.check_expression_statement(node),
            Node::IfStatement(_) => self.check_if_statement(node),
            Node::ReturnStatement(_) => self.check_return_statement(node),
            Node::VariableDeclaration(_) => self.check_variable_declaration(node),
            Node::InterfaceDeclaration(_) => self.check_interface_declaration(node),
            Node::TypeAliasDeclaration(_) => self.check_type_alias_declaration(node),
            _ => unimplemented!("{:?}", node.kind()),
        };
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

    pub(super) fn initialize_type_checker(&mut self) {
        for file in self.host.get_source_files() {
            bind_source_file(&*file, self.compiler_options.clone());
            println!("post-binding: {:#?}", file);
        }

        for file in self.host.get_source_files() {
            if !is_external_or_common_js_module(&file) {
                self.merge_symbol_table(
                    &mut *self.globals_mut(),
                    &RefCell::borrow(&file.locals()),
                    None,
                );
            }
        }

        self.global_object_type =
            self.get_global_type(&__String::new("Object".to_owned()), 0, true);
        self.global_boolean_type =
            self.get_global_type(&__String::new("Boolean".to_owned()), 0, true);
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
