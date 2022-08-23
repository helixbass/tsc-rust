#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use super::{
    signature_has_rest_parameter, CheckMode, MinArgumentCountFlags, TypeFacts, WideningKind,
};
use crate::{
    create_symbol_table, file_extension_is_one_of, get_declaration_of_kind,
    get_source_file_of_node, is_call_expression, is_identifier, is_import_call,
    is_property_access_expression, is_require_call, is_source_file, Debug_, Diagnostics, Extension,
    ExternalEmitHelpers, FunctionFlags, InternalSymbolName, NodeFlags, ObjectFlags, ScriptTarget,
    Signature, SignatureFlags, SymbolFlags, TransientSymbolInterface, UnionReduction, __String,
    get_function_flags, has_initializer, Node, NodeInterface, Symbol, SymbolInterface, SyntaxKind,
    Type, TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn is_symbol_or_symbol_for_call(&self, node: &Node) -> bool {
        if !is_call_expression(node) {
            return false;
        }
        let node_as_call_expression = node.as_call_expression();
        let mut left = node_as_call_expression.expression.clone();
        if is_property_access_expression(&left)
            && left
                .as_property_access_expression()
                .name
                .as_member_name()
                .escaped_text()
                .eq_str("for")
        {
            left = left.as_property_access_expression().expression.clone();
        }
        if !is_identifier(&left) || !left.as_identifier().escaped_text.eq_str("Symbol") {
            return false;
        }

        let global_es_symbol = self.get_global_es_symbol_constructor_symbol(false);
        if global_es_symbol.is_none() {
            return false;
        }
        let global_es_symbol = global_es_symbol.as_ref().unwrap();

        matches!(
            self.resolve_name_(
                Some(left),
                &__String::new("Symbol".to_owned()),
                SymbolFlags::Value,
                None,
                Option::<Rc<Node>>::None,
                false,
                None,
            ).as_ref(),
            Some(resolved_name) if Rc::ptr_eq(
                global_es_symbol,
                resolved_name
            )
        )
    }

    pub(super) fn check_import_call_expression(&self, node: &Node /*ImportCall*/) -> Rc<Type> {
        let node_as_call_expression = node.as_call_expression();
        if !self.check_grammar_arguments(Some(&node_as_call_expression.arguments)) {
            self.check_grammar_import_call_expression(node);
        }

        if node_as_call_expression.arguments.is_empty() {
            return self.create_promise_return_type(node, &self.any_type());
        }

        let specifier = &node_as_call_expression.arguments[0];
        let specifier_type = self.check_expression_cached(specifier, None);
        let options_type = if node_as_call_expression.arguments.len() > 1 {
            Some(self.check_expression_cached(&node_as_call_expression.arguments[1], None))
        } else {
            None
        };
        for i in 2..node_as_call_expression.arguments.len() {
            self.check_expression_cached(&node_as_call_expression.arguments[i], None);
        }

        if specifier_type.flags().intersects(TypeFlags::Undefined)
            || specifier_type.flags().intersects(TypeFlags::Null)
            || !self.is_type_assignable_to(&specifier_type, &self.string_type())
        {
            self.error(
                Some(&**specifier),
                &Diagnostics::Dynamic_import_s_specifier_must_be_of_type_string_but_here_has_type_0,
                Some(vec![self.type_to_string_(
                    &specifier_type,
                    Option::<&Node>::None,
                    None,
                    None,
                )]),
            );
        }

        if let Some(options_type) = options_type.as_ref() {
            let import_call_options_type = self.get_global_import_call_options_type(true);
            if !Rc::ptr_eq(&import_call_options_type, &self.empty_object_type()) {
                self.check_type_assignable_to(
                    options_type,
                    &self.get_nullable_type(&import_call_options_type, TypeFlags::Undefined),
                    Some(&*node_as_call_expression.arguments[1]),
                    None,
                    None,
                    None,
                );
            }
        }

        let module_symbol = self.resolve_external_module_name_(node, specifier, None);
        if let Some(module_symbol) = module_symbol.as_ref() {
            let es_module_symbol =
                self.resolve_es_module_symbol(Some(&**module_symbol), specifier, true, false);
            if let Some(es_module_symbol) = es_module_symbol.as_ref() {
                return self.create_promise_return_type(
                    node,
                    &self
                        .get_type_with_synthetic_default_only(
                            &self.get_type_of_symbol(es_module_symbol),
                            es_module_symbol,
                            module_symbol,
                            specifier,
                        )
                        .unwrap_or_else(|| {
                            self.get_type_with_synthetic_default_import_type(
                                &self.get_type_of_symbol(es_module_symbol),
                                es_module_symbol,
                                module_symbol,
                                specifier,
                            )
                        }),
                );
            }
        }
        self.create_promise_return_type(node, &self.any_type())
    }

    pub(super) fn create_default_property_wrapper_for_module<TAnonymousSymbol: Borrow<Symbol>>(
        &self,
        symbol: &Symbol,
        original_symbol: &Symbol,
        anonymous_symbol: Option<TAnonymousSymbol>,
    ) -> Rc<Type> {
        let mut member_table = create_symbol_table(None);
        let new_symbol: Rc<Symbol> = self
            .create_symbol(SymbolFlags::Alias, InternalSymbolName::Default(), None)
            .into();
        new_symbol.set_parent(Some(original_symbol.symbol_wrapper()));
        {
            let new_symbol_links = new_symbol.as_transient_symbol().symbol_links();
            let mut new_symbol_links = new_symbol_links.borrow_mut();
            new_symbol_links.name_type = Some(self.get_string_literal_type("default"));
            new_symbol_links.target = self.resolve_symbol(Some(symbol), None);
        }
        member_table.insert(InternalSymbolName::Default(), new_symbol);
        self.create_anonymous_type(
            anonymous_symbol,
            Rc::new(RefCell::new(member_table)),
            vec![],
            vec![],
            vec![],
        )
        .into()
    }

    pub(super) fn get_type_with_synthetic_default_only(
        &self,
        type_: &Type,
        symbol: &Symbol,
        original_symbol: &Symbol,
        module_specifier: &Node, /*Expression*/
    ) -> Option<Rc<Type>> {
        let has_default_only = self.is_only_imported_as_default(module_specifier);
        if has_default_only {
            if
            /*type &&*/
            !self.is_error_type(type_) {
                let synth_type = type_;
                if synth_type.maybe_default_only_type().is_none() {
                    let type_ = self.create_default_property_wrapper_for_module(
                        symbol,
                        original_symbol,
                        Option::<&Symbol>::None,
                    );
                    *synth_type.maybe_default_only_type() = Some(type_);
                }
                return synth_type.maybe_default_only_type().clone();
            }
        }
        None
    }

    pub(super) fn get_type_with_synthetic_default_import_type(
        &self,
        type_: &Type,
        symbol: &Symbol,
        original_symbol: &Symbol,
        module_specifier: &Node, /*Expression*/
    ) -> Rc<Type> {
        if self.allow_synthetic_default_imports && /*type &&*/ !self.is_error_type(type_) {
            let synth_type = type_;
            if synth_type.maybe_synthetic_type().is_none() {
                let file = original_symbol.maybe_declarations().as_ref().and_then(
                    |original_symbol_declarations| {
                        original_symbol_declarations
                            .into_iter()
                            .find(|declaration| is_source_file(declaration))
                            .cloned()
                    },
                );
                let has_synthetic_default = self.can_have_synthetic_default(
                    file.as_deref(),
                    original_symbol,
                    false,
                    module_specifier,
                );
                if has_synthetic_default {
                    let anonymous_symbol: Rc<Symbol> = self
                        .create_symbol(SymbolFlags::TypeLiteral, InternalSymbolName::Type(), None)
                        .into();
                    let default_containing_object = self
                        .create_default_property_wrapper_for_module(
                            symbol,
                            original_symbol,
                            Some(&*anonymous_symbol),
                        );
                    anonymous_symbol
                        .as_transient_symbol()
                        .symbol_links()
                        .borrow_mut()
                        .type_ = Some(default_containing_object.clone());
                    *synth_type.maybe_synthetic_type() =
                        Some(if self.is_valid_spread_type(type_) {
                            self.get_spread_type(
                                type_,
                                &default_containing_object,
                                Some(anonymous_symbol),
                                ObjectFlags::None,
                                false,
                            )
                        } else {
                            default_containing_object
                        });
                } else {
                    *synth_type.maybe_synthetic_type() = Some(type_.type_wrapper());
                }
            }
            return synth_type.maybe_synthetic_type().clone().unwrap();
        }
        type_.type_wrapper()
    }

    pub(super) fn is_common_js_require(&self, node: &Node) -> bool {
        if !is_require_call(node, true) {
            return false;
        }
        let node_as_call_expression = node.as_call_expression();

        if !is_identifier(&node_as_call_expression.expression) {
            Debug_.fail(None);
        }
        let resolved_require = self
            .resolve_name_(
                Some(&*node_as_call_expression.expression),
                &node_as_call_expression
                    .expression
                    .as_identifier()
                    .escaped_text,
                SymbolFlags::Value,
                None,
                Option::<Rc<Node>>::None,
                true,
                None,
            )
            .unwrap();
        if Rc::ptr_eq(&resolved_require, &self.require_symbol()) {
            return true;
        }
        if resolved_require.flags().intersects(SymbolFlags::Alias) {
            return false;
        }

        let target_declaration_kind = if resolved_require.flags().intersects(SymbolFlags::Function)
        {
            SyntaxKind::FunctionDeclaration
        } else if resolved_require.flags().intersects(SymbolFlags::Variable) {
            SyntaxKind::VariableDeclaration
        } else {
            SyntaxKind::Unknown
        };
        if target_declaration_kind != SyntaxKind::Unknown {
            let decl = get_declaration_of_kind(&resolved_require, target_declaration_kind);
            return matches!(
                decl.as_ref(),
                Some(decl) if decl.flags().intersects(NodeFlags::Ambient)
            );
        }
        false
    }

    pub(super) fn check_tagged_template_expression(
        &self,
        node: &Node, /*TaggedTemplateExpression*/
    ) -> Rc<Type> {
        let node_as_tagged_template_expression = node.as_tagged_template_expression();
        if !self.check_grammar_tagged_template_chain(node) {
            self.check_grammar_type_arguments(
                node,
                node_as_tagged_template_expression.type_arguments.as_ref(),
            );
        }
        if self.language_version < ScriptTarget::ES2015 {
            self.check_external_emit_helpers(node, ExternalEmitHelpers::MakeTemplateObject);
        }
        let signature = self.get_resolved_signature_(node, None, None);
        self.check_deprecated_signature(&signature, node);
        self.get_return_type_of_signature(signature)
    }

    pub(super) fn check_assertion(&self, node: &Node /*AssertionExpression*/) -> Rc<Type> {
        if node.kind() == SyntaxKind::TypeAssertionExpression {
            let file = get_source_file_of_node(Some(node));
            if matches!(
                file.as_ref(),
                Some(file) if file_extension_is_one_of(
                    &file.as_source_file().file_name(),
                    &[Extension::Cts.to_str(), Extension::Mts.to_str()]
                )
            ) {
                self.grammar_error_on_node(
                    node,
                    &Diagnostics::This_syntax_is_reserved_in_files_with_the_mts_or_cts_extension_Use_an_as_expression_instead,
                    None,
                );
            }
        }
        self.check_assertion_worker(
            node,
            &node.as_has_type().maybe_type().unwrap(),
            &node.as_has_expression().expression(),
            None,
        )
    }

    pub(super) fn check_assertion_worker(
        &self,
        err_node: &Node,
        type_: &Node,      /*TypeNode*/
        expression: &Node, /*UnaryExpression | Expression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
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

    pub(super) fn get_type_of_first_parameter_of_signature_with_fallback(
        &self,
        signature: &Signature,
        fallback_type: &Type,
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

    pub(super) fn create_promise_like_type(&self, promised_type: &Type) -> Rc<Type> {
        unimplemented!()
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
