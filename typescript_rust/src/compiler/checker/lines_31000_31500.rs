use gc::{Gc, GcCell};
use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::io;
use std::rc::Rc;

use super::{signature_has_rest_parameter, CheckMode, MinArgumentCountFlags};
use crate::{
    create_symbol_table, file_extension_is_one_of, find_index, get_declaration_of_kind,
    get_new_target_container, get_source_file_of_node, is_call_expression, is_const_type_reference,
    is_identifier, is_parameter, is_property_access_expression, is_require_call, is_source_file,
    length, Debug_, Diagnostics, ElementFlags, EnumKind, Extension, ExternalEmitHelpers,
    HasTypeArgumentsInterface, InternalSymbolName, ModuleKind, NamedDeclarationInterface,
    NodeFlags, Number, ObjectFlags, ScriptTarget, Signature, SignatureFlags, SymbolFlags,
    TransientSymbolInterface, __String, has_initializer, maybe_get_source_file_of_node, Node,
    NodeInterface, OptionTry, Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags,
    TypeInterface,
};

impl TypeChecker {
    pub(super) fn is_symbol_or_symbol_for_call(&self, node: &Node) -> io::Result<bool> {
        if !is_call_expression(node) {
            return Ok(false);
        }
        let node_as_call_expression = node.as_call_expression();
        let mut left = node_as_call_expression.expression.clone();
        if is_property_access_expression(&left)
            && left
                .as_property_access_expression()
                .name
                .as_member_name()
                .escaped_text()
                == "for"
        {
            left = left.as_property_access_expression().expression.clone();
        }
        if !is_identifier(&left) || left.as_identifier().escaped_text != "Symbol" {
            return Ok(false);
        }

        let global_es_symbol = self.get_global_es_symbol_constructor_symbol(false)?;
        if global_es_symbol.is_none() {
            return Ok(false);
        }
        let global_es_symbol = global_es_symbol.as_ref().unwrap();

        Ok(matches!(
            self.resolve_name_(
                Some(left),
                "Symbol",
                SymbolFlags::Value,
                None,
                Option::<Gc<Node>>::None,
                false,
                None,
            )?.as_ref(),
            Some(resolved_name) if Gc::ptr_eq(
                global_es_symbol,
                resolved_name
            )
        ))
    }

    pub(super) fn check_import_call_expression(
        &self,
        node: &Node, /*ImportCall*/
    ) -> io::Result<Gc<Type>> {
        let node_as_call_expression = node.as_call_expression();
        if !self.check_grammar_arguments(Some(&node_as_call_expression.arguments)) {
            self.check_grammar_import_call_expression(node);
        }

        if node_as_call_expression.arguments.is_empty() {
            return self.create_promise_return_type(node, &self.any_type());
        }

        let specifier = &node_as_call_expression.arguments[0];
        let specifier_type = self.check_expression_cached(specifier, None)?;
        let options_type = if node_as_call_expression.arguments.len() > 1 {
            Some(self.check_expression_cached(&node_as_call_expression.arguments[1], None)?)
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
                )?]),
            );
        }

        if let Some(options_type) = options_type.as_ref() {
            let import_call_options_type = self.get_global_import_call_options_type(true)?;
            if !Gc::ptr_eq(&import_call_options_type, &self.empty_object_type()) {
                self.check_type_assignable_to(
                    options_type,
                    &*self.get_nullable_type(&import_call_options_type, TypeFlags::Undefined)?,
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
                self.resolve_es_module_symbol(Some(&**module_symbol), specifier, true, false)?;
            if let Some(es_module_symbol) = es_module_symbol.as_ref() {
                return self.create_promise_return_type(
                    node,
                    &*self
                        .get_type_with_synthetic_default_only(
                            &*self.get_type_of_symbol(es_module_symbol)?,
                            es_module_symbol,
                            module_symbol,
                            specifier,
                        )?
                        .try_unwrap_or_else(|| {
                            self.get_type_with_synthetic_default_import_type(
                                &*self.get_type_of_symbol(es_module_symbol)?,
                                es_module_symbol,
                                module_symbol,
                                specifier,
                            )
                        })?,
                );
            }
        }
        self.create_promise_return_type(node, &self.any_type())
    }

    pub(super) fn create_default_property_wrapper_for_module(
        &self,
        symbol: &Symbol,
        original_symbol: &Symbol,
        anonymous_symbol: Option<impl Borrow<Symbol>>,
    ) -> io::Result<Gc<Type>> {
        let mut member_table = create_symbol_table(Option::<&[Gc<Symbol>]>::None);
        let new_symbol: Gc<Symbol> = self
            .create_symbol(
                SymbolFlags::Alias,
                InternalSymbolName::Default.to_owned(),
                None,
            )
            .into();
        new_symbol.set_parent(Some(original_symbol.symbol_wrapper()));
        {
            let new_symbol_links = new_symbol.as_transient_symbol().symbol_links();
            let mut new_symbol_links = new_symbol_links.borrow_mut();
            new_symbol_links.name_type = Some(self.get_string_literal_type("default"));
            new_symbol_links.target = self.resolve_symbol(Some(symbol), None)?;
        }
        member_table.insert(InternalSymbolName::Default.to_owned(), new_symbol);
        Ok(self.create_anonymous_type(
            anonymous_symbol,
            Gc::new(GcCell::new(member_table)),
            vec![],
            vec![],
            vec![],
        ))
    }

    pub(super) fn get_type_with_synthetic_default_only(
        &self,
        type_: &Type,
        symbol: &Symbol,
        original_symbol: &Symbol,
        module_specifier: &Node, /*Expression*/
    ) -> io::Result<Option<Gc<Type>>> {
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
                    )?;
                    *synth_type.maybe_default_only_type() = Some(type_);
                }
                return Ok(synth_type.maybe_default_only_type().clone());
            }
        }
        Ok(None)
    }

    pub(super) fn get_type_with_synthetic_default_import_type(
        &self,
        type_: &Type,
        symbol: &Symbol,
        original_symbol: &Symbol,
        module_specifier: &Node, /*Expression*/
    ) -> io::Result<Gc<Type>> {
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
                )?;
                if has_synthetic_default {
                    let anonymous_symbol: Gc<Symbol> = self
                        .create_symbol(
                            SymbolFlags::TypeLiteral,
                            InternalSymbolName::Type.to_owned(),
                            None,
                        )
                        .into();
                    let default_containing_object = self
                        .create_default_property_wrapper_for_module(
                            symbol,
                            original_symbol,
                            Some(&*anonymous_symbol),
                        )?;
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
                            )?
                        } else {
                            default_containing_object
                        });
                } else {
                    *synth_type.maybe_synthetic_type() = Some(type_.type_wrapper());
                }
            }
            return Ok(synth_type.maybe_synthetic_type().clone().unwrap());
        }
        Ok(type_.type_wrapper())
    }

    pub(super) fn is_common_js_require(&self, node: &Node) -> io::Result<bool> {
        if !is_require_call(node, true) {
            return Ok(false);
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
                Option::<Gc<Node>>::None,
                true,
                None,
            )?
            .unwrap();
        if Gc::ptr_eq(&resolved_require, &self.require_symbol()) {
            return Ok(true);
        }
        if resolved_require.flags().intersects(SymbolFlags::Alias) {
            return Ok(false);
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
            return Ok(matches!(
                decl.as_ref(),
                Some(decl) if decl.flags().intersects(NodeFlags::Ambient)
            ));
        }
        Ok(false)
    }

    pub(super) fn check_tagged_template_expression(
        &self,
        node: &Node, /*TaggedTemplateExpression*/
    ) -> io::Result<Gc<Type>> {
        let node_as_tagged_template_expression = node.as_tagged_template_expression();
        if !self.check_grammar_tagged_template_chain(node) {
            self.check_grammar_type_arguments(
                node,
                node_as_tagged_template_expression
                    .maybe_type_arguments()
                    .as_deref(),
            );
        }
        if self.language_version < ScriptTarget::ES2015 {
            self.check_external_emit_helpers(node, ExternalEmitHelpers::MakeTemplateObject);
        }
        let signature = self.get_resolved_signature_(node, None, None)?;
        self.check_deprecated_signature(signature.clone(), node);
        self.get_return_type_of_signature(signature)
    }

    pub(super) fn check_assertion(
        &self,
        node: &Node, /*AssertionExpression*/
    ) -> io::Result<Gc<Type>> {
        if node.kind() == SyntaxKind::TypeAssertionExpression {
            let file = maybe_get_source_file_of_node(Some(node));
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

    pub(super) fn is_valid_const_assertion_argument(&self, node: &Node) -> io::Result<bool> {
        Ok(match node.kind() {
            SyntaxKind::StringLiteral
            | SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::TrueKeyword
            | SyntaxKind::FalseKeyword
            | SyntaxKind::ArrayLiteralExpression
            | SyntaxKind::ObjectLiteralExpression
            | SyntaxKind::TemplateExpression => true,
            SyntaxKind::ParenthesizedExpression => self.is_valid_const_assertion_argument(
                &node.as_parenthesized_expression().expression,
            )?,
            SyntaxKind::PrefixUnaryExpression => {
                let node_as_prefix_unary_expression = node.as_prefix_unary_expression();
                let op = node_as_prefix_unary_expression.operator;
                let arg = &node_as_prefix_unary_expression.operand;
                op == SyntaxKind::MinusToken
                    && matches!(
                        arg.kind(),
                        SyntaxKind::NumericLiteral | SyntaxKind::BigIntLiteral
                    )
                    || op == SyntaxKind::PlusToken && arg.kind() == SyntaxKind::NumericLiteral
            }
            SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
                let expr = node.as_has_expression().expression();
                let mut symbol = self.get_type_of_node(&expr)?.maybe_symbol();
                if let Some(symbol_present) = symbol
                    .as_ref()
                    .filter(|symbol| symbol.flags().intersects(SymbolFlags::Alias))
                {
                    symbol = Some(self.resolve_alias(symbol_present)?);
                }
                matches!(
                    symbol.as_ref(),
                    Some(symbol) if symbol.flags().intersects(SymbolFlags::Enum) && self.get_enum_kind(symbol)? == EnumKind::Literal
                )
            }
            _ => false,
        })
    }

    pub(super) fn check_assertion_worker(
        &self,
        err_node: &Node,
        type_: &Node,      /*TypeNode*/
        expression: &Node, /*UnaryExpression | Expression*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Gc<Type>> {
        let mut expr_type = self.check_expression(expression, check_mode, None)?;
        if is_const_type_reference(type_) {
            if !self.is_valid_const_assertion_argument(expression)? {
                self.error(
                    Some(expression),
                    &Diagnostics::A_const_assertions_can_only_be_applied_to_references_to_enum_members_or_string_number_boolean_array_or_object_literals,
                    None,
                );
            }
            return Ok(self.get_regular_type_of_literal_type(&expr_type));
        }
        self.check_source_element(Some(type_));
        expr_type = self.get_regular_type_of_object_literal(
            &*self.get_base_type_of_literal_type(&expr_type)?,
        )?;
        let target_type = self.get_type_from_type_node_(type_)?;
        if self.produce_diagnostics && !self.is_error_type(&target_type) {
            let widened_type = self.get_widened_type(&expr_type)?;
            if !self.is_type_comparable_to(&target_type, &widened_type) {
                self.check_type_comparable_to(
                    &expr_type,
                    &target_type,
                    err_node,
                    Some(Cow::Borrowed(&Diagnostics::Conversion_of_type_0_to_type_1_may_be_a_mistake_because_neither_type_sufficiently_overlaps_with_the_other_If_this_was_intentional_convert_the_expression_to_unknown_first)),
                    None,
                );
            }
        }
        Ok(target_type)
    }

    pub(super) fn check_non_null_chain(
        &self,
        node: &Node, /*NonNullChain*/
    ) -> io::Result<Gc<Type>> {
        let left_type =
            self.check_expression(&node.as_has_expression().expression(), None, None)?;
        let non_optional_type =
            self.get_optional_expression_type(&left_type, &node.as_has_expression().expression())?;
        self.propagate_optional_type_marker(
            &*self.get_non_nullable_type(&non_optional_type)?,
            node,
            !Gc::ptr_eq(&non_optional_type, &left_type),
        )
    }

    pub(super) fn check_non_null_assertion(
        &self,
        node: &Node, /*NonNullExpression*/
    ) -> io::Result<Gc<Type>> {
        Ok(if node.flags().intersects(NodeFlags::OptionalChain) {
            self.check_non_null_chain(node)?
        } else {
            self.get_non_nullable_type(&*self.check_expression(
                &node.as_has_expression().expression(),
                None,
                None,
            )?)?
        })
    }

    pub(super) fn check_meta_property(
        &self,
        node: &Node, /*MetaProperty*/
    ) -> io::Result<Gc<Type>> {
        self.check_grammar_meta_property(node);

        let node_as_meta_property = node.as_meta_property();
        if node_as_meta_property.keyword_token == SyntaxKind::NewKeyword {
            return self.check_new_target_meta_property(node);
        }

        if node_as_meta_property.keyword_token == SyntaxKind::ImportKeyword {
            return self.check_import_meta_property(node);
        }

        Debug_.assert_never(node_as_meta_property.keyword_token, None);
    }

    pub(super) fn check_meta_property_keyword(
        &self,
        node: &Node, /*MetaProperty*/
    ) -> io::Result<Gc<Type>> {
        Ok(match node.as_meta_property().keyword_token {
            SyntaxKind::ImportKeyword => self.get_global_import_meta_expression_type()?,
            SyntaxKind::NewKeyword => {
                let type_ = self.check_new_target_meta_property(node)?;
                if self.is_error_type(&type_) {
                    self.error_type()
                } else {
                    self.create_new_target_expression_type(&type_)
                }
            }
            _ => Debug_.assert_never(node.as_meta_property().keyword_token, None),
        })
    }

    pub(super) fn check_new_target_meta_property(
        &self,
        node: &Node, /*MetaProperty*/
    ) -> io::Result<Gc<Type>> {
        let container = get_new_target_container(node);
        Ok(match container.as_ref() {
            None => {
                self.error(
                    Some(node),
                    &Diagnostics::Meta_property_0_is_only_allowed_in_the_body_of_a_function_declaration_function_expression_or_constructor,
                    Some(vec![
                        "new.target".to_owned()
                    ])
                );
                self.error_type()
            }
            Some(container) => {
                if container.kind() == SyntaxKind::Constructor {
                    let symbol = self.get_symbol_of_node(&container.parent())?.unwrap();
                    self.get_type_of_symbol(&symbol)?
                } else {
                    let symbol = self.get_symbol_of_node(container)?.unwrap();
                    self.get_type_of_symbol(&symbol)?
                }
            }
        })
    }

    pub(super) fn check_import_meta_property(
        &self,
        node: &Node, /*MetaProperty*/
    ) -> io::Result<Gc<Type>> {
        if matches!(self.module_kind, ModuleKind::Node12 | ModuleKind::NodeNext) {
            if get_source_file_of_node(node)
                .as_source_file()
                .maybe_implied_node_format()
                != Some(ModuleKind::ESNext)
            {
                self.error(
                    Some(node),
                    &Diagnostics::The_import_meta_meta_property_is_not_allowed_in_files_which_will_build_into_CommonJS_output,
                    None,
                );
            }
        } else if self.module_kind < ModuleKind::ES2020 && self.module_kind != ModuleKind::System {
            self.error(
                Some(node),
                &Diagnostics::The_import_meta_meta_property_is_only_allowed_when_the_module_option_is_es2020_es2022_esnext_system_node12_or_nodenext,
                None,
            );
        }
        let file = get_source_file_of_node(node);
        Debug_.assert(
            file.flags()
                .intersects(NodeFlags::PossiblyContainsImportMeta),
            Some("Containing file is missing import meta node flag."),
        );
        Ok(
            if node.as_meta_property().name.as_identifier().escaped_text == "meta" {
                self.get_global_import_meta_type()?
            } else {
                self.error_type()
            },
        )
    }

    pub(super) fn get_type_of_parameter(&self, symbol: &Symbol) -> io::Result<Gc<Type>> {
        let type_ = self.get_type_of_symbol(symbol)?;
        if self.strict_null_checks {
            let declaration = symbol.maybe_value_declaration();
            if matches!(declaration.as_ref(), Some(declaration) if has_initializer(&declaration)) {
                return self.get_optional_type_(&type_, None);
            }
        }
        Ok(type_)
    }

    pub(super) fn get_tuple_element_label(
        &self,
        d: &Node, /*ParameterDeclaration | NamedTupleMember*/
    ) -> __String {
        Debug_.assert(is_identifier(&d.as_named_declaration().name()), None);
        d.as_named_declaration()
            .name()
            .as_identifier()
            .escaped_text
            .clone()
    }

    pub(super) fn get_parameter_name_at_position(
        &self,
        signature: &Signature,
        pos: usize,
        override_rest_type: Option<impl Borrow<Type>>,
    ) -> io::Result<__String> {
        let param_count = signature.parameters().len()
            - if signature_has_rest_parameter(signature) {
                1
            } else {
                0
            };
        if pos < param_count {
            return Ok(signature.parameters()[pos].escaped_name().to_owned());
        }
        let rest_parameter = signature
            .parameters()
            .get(param_count)
            .cloned()
            .unwrap_or_else(|| self.unknown_symbol());
        let override_rest_type =
            override_rest_type.map(|override_rest_type| override_rest_type.borrow().type_wrapper());
        let rest_type =
            override_rest_type.try_unwrap_or_else(|| self.get_type_of_symbol(&rest_parameter))?;
        if self.is_tuple_type(&rest_type) {
            let associated_names = rest_type
                .as_type_reference()
                .target
                .as_tuple_type()
                .labeled_element_declarations
                .as_ref();
            let index = pos - param_count;
            return Ok(associated_names
                .map(|associated_names| self.get_tuple_element_label(&associated_names[index]))
                .unwrap_or_else(|| format!("{}_{}", rest_parameter.escaped_name(), index)));
        }
        Ok(rest_parameter.escaped_name().to_owned())
    }

    pub fn get_parameter_identifier_name_at_position(
        &self,
        signature: &Signature,
        pos: usize,
    ) -> io::Result<Option<(__String, bool)>> {
        let param_count = signature.parameters().len()
            - if signature_has_rest_parameter(signature) {
                1
            } else {
                0
            };
        if pos < param_count {
            let param = &signature.parameters()[pos];
            return Ok(
                if self.is_parameter_declaration_with_identifier_name(param) {
                    Some((param.escaped_name().to_owned(), false))
                } else {
                    None
                },
            );
        }

        let rest_parameter = signature
            .parameters()
            .get(param_count)
            .cloned()
            .unwrap_or_else(|| self.unknown_symbol());
        if !self.is_parameter_declaration_with_identifier_name(&rest_parameter) {
            return Ok(None);
        }

        let rest_type = self.get_type_of_symbol(&rest_parameter)?;
        if self.is_tuple_type(&rest_type) {
            let associated_names = rest_type
                .as_type_reference()
                .target
                .as_tuple_type()
                .labeled_element_declarations
                .as_ref();
            let index = pos - param_count;
            let associated_name =
                associated_names.and_then(|associated_names| associated_names.get(index).cloned());
            let is_rest_tuple_element = matches!(
                associated_name.as_ref(),
                Some(associated_name) if associated_name.as_has_dot_dot_dot_token().maybe_dot_dot_dot_token().is_some()
            );
            return Ok(associated_name.as_ref().map(|associated_name| {
                (
                    self.get_tuple_element_label(associated_name),
                    is_rest_tuple_element,
                )
            }));
        }

        if pos == param_count {
            return Ok(Some((rest_parameter.escaped_name().to_owned(), true)));
        }
        Ok(None)
    }

    pub(super) fn is_parameter_declaration_with_identifier_name(&self, symbol: &Symbol) -> bool {
        matches!(
            symbol.maybe_value_declaration().as_ref(),
            Some(symbol_value_declaration) if is_parameter(symbol_value_declaration) &&
                is_identifier(&symbol_value_declaration.as_parameter_declaration().name())
        )
    }

    pub(super) fn is_valid_declaration_for_tuple_label(
        &self,
        d: &Node, /*Declaration*/
    ) -> bool {
        d.kind() == SyntaxKind::NamedTupleMember
            || is_parameter(d)
                && matches!(
                    d.as_parameter_declaration().maybe_name().as_ref(),
                    Some(d_name) if is_identifier(d_name)
                )
    }

    pub(super) fn get_nameable_declaration_at_position(
        &self,
        signature: &Signature,
        pos: usize,
    ) -> io::Result<Option<Gc<Node>>> {
        let param_count = signature.parameters().len()
            - if signature_has_rest_parameter(signature) {
                1
            } else {
                0
            };
        if pos < param_count {
            let decl = signature.parameters()[pos].maybe_value_declaration();
            return Ok(decl.filter(|decl| self.is_valid_declaration_for_tuple_label(decl)));
        }
        let rest_parameter = signature
            .parameters()
            .get(param_count)
            .cloned()
            .unwrap_or_else(|| self.unknown_symbol());
        let rest_type = self.get_type_of_symbol(&rest_parameter)?;
        if self.is_tuple_type(&rest_type) {
            let associated_names = rest_type
                .as_type_reference()
                .target
                .as_tuple_type()
                .labeled_element_declarations
                .as_ref();
            let index = pos - param_count;
            return Ok(
                associated_names.and_then(|associated_names| associated_names.get(index).cloned())
            );
        }
        Ok(rest_parameter
            .maybe_value_declaration()
            .filter(|rest_parameter_value_declaration| {
                self.is_valid_declaration_for_tuple_label(rest_parameter_value_declaration)
            }))
    }

    pub(super) fn get_type_at_position(
        &self,
        signature: &Signature,
        pos: usize,
    ) -> io::Result<Gc<Type>> {
        Ok(self
            .try_get_type_at_position(signature, pos)?
            .unwrap_or_else(|| self.any_type()))
    }

    pub(super) fn try_get_type_at_position(
        &self,
        signature: &Signature,
        pos: usize,
    ) -> io::Result<Option<Gc<Type>>> {
        let param_count = signature.parameters().len()
            - if signature_has_rest_parameter(signature) {
                1
            } else {
                0
            };
        if pos < param_count {
            return Ok(Some(
                self.get_type_of_parameter(&signature.parameters()[pos])?,
            ));
        }
        if signature_has_rest_parameter(signature) {
            let rest_type = self.get_type_of_symbol(&signature.parameters()[param_count])?;
            let index = pos - param_count;
            if !self.is_tuple_type(&rest_type) || {
                let rest_type_target = rest_type.as_type_reference_interface().target();
                let rest_type_target_as_tuple_type = rest_type_target.as_tuple_type();
                rest_type_target_as_tuple_type.has_rest_element
                    || index < rest_type_target_as_tuple_type.fixed_length
            } {
                return Ok(Some(self.get_indexed_access_type(
                    &rest_type,
                    &self.get_number_literal_type(Number::new(index as f64)),
                    None,
                    Option::<&Node>::None,
                    Option::<&Symbol>::None,
                    None,
                )));
            }
        }
        Ok(None)
    }

    pub(super) fn get_rest_type_at_position(
        &self,
        source: &Signature,
        pos: usize,
    ) -> io::Result<Gc<Type>> {
        let parameter_count = self.get_parameter_count(source)?;
        let min_argument_count = self.get_min_argument_count(source, None)?;
        let rest_type = self.get_effective_rest_type(source)?;
        if let Some(rest_type) = rest_type.as_ref() {
            if pos >= parameter_count - 1 {
                return Ok(if pos == parameter_count - 1 {
                    rest_type.clone()
                } else {
                    self.create_array_type(
                        &self.get_indexed_access_type(
                            rest_type,
                            &self.number_type(),
                            None,
                            Option::<&Node>::None,
                            Option::<&Symbol>::None,
                            None,
                        ),
                        None,
                    )
                });
            }
        }
        let mut types = vec![];
        let mut flags = vec![];
        let mut names = vec![];
        for i in pos..parameter_count {
            if rest_type.is_none() || i < parameter_count - 1 {
                types.push(self.get_type_at_position(source, i)?);
                flags.push(if i < min_argument_count {
                    ElementFlags::Required
                } else {
                    ElementFlags::Optional
                });
            } else {
                types.push(rest_type.clone().unwrap());
                flags.push(ElementFlags::Variadic);
            }
            let name = self.get_nameable_declaration_at_position(source, i)?;
            if let Some(name) = name {
                names.push(name);
            }
        }
        self.create_tuple_type(
            &types,
            Some(&flags),
            Some(false),
            if length(Some(&names)) == length(Some(&types)) {
                Some(&names)
            } else {
                None
            },
        )
    }

    pub(super) fn get_parameter_count(&self, signature: &Signature) -> io::Result<usize> {
        let length = signature.parameters().len();
        if signature_has_rest_parameter(signature) {
            let rest_type = self.get_type_of_symbol(&signature.parameters()[length - 1])?;
            if self.is_tuple_type(&rest_type) {
                let rest_type_target = rest_type.as_type_reference_interface().target();
                let rest_type_target_as_tuple_type = rest_type_target.as_tuple_type();
                return Ok(length + rest_type_target_as_tuple_type.fixed_length
                    - if rest_type_target_as_tuple_type.has_rest_element {
                        0
                    } else {
                        1
                    });
            }
        }
        Ok(length)
    }

    pub(super) fn get_min_argument_count(
        &self,
        signature: &Signature,
        flags: Option<MinArgumentCountFlags>,
    ) -> io::Result<usize> {
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
                let rest_type = self.get_type_of_symbol(
                    &signature.parameters()[signature.parameters().len() - 1],
                )?;
                if self.is_tuple_type(&rest_type) {
                    let rest_type_target = rest_type.as_type_reference_interface().target();
                    let rest_type_target_as_tuple_type = rest_type_target.as_tuple_type();
                    let first_optional_index = find_index(
                        &rest_type_target_as_tuple_type.element_flags,
                        |f: &ElementFlags, _| !f.intersects(ElementFlags::Required),
                        None,
                    );
                    let required_count =
                        first_optional_index.unwrap_or(rest_type_target_as_tuple_type.fixed_length);
                    if required_count > 0 {
                        min_argument_count =
                            Some(signature.parameters().len() - 1 + required_count);
                    }
                }
            }
            if min_argument_count.is_none() {
                if !strong_arity_for_untyped_js
                    && signature
                        .flags
                        .intersects(SignatureFlags::IsUntypedSignatureInJSFile)
                {
                    return Ok(0);
                }
                min_argument_count = Some(signature.min_argument_count());
            }
            let mut min_argument_count = min_argument_count.unwrap();
            if void_is_non_optional {
                return Ok(min_argument_count);
            }
            let mut i = if min_argument_count == 0 {
                // avoid usize underflow
                0
            } else {
                min_argument_count - 1
            };
            loop
            /*while (i >= 0)*/
            {
                let type_ = self.get_type_at_position(signature, i)?;
                if self
                    .filter_type(&type_, |type_| self.accepts_void(type_))
                    .flags()
                    .intersects(TypeFlags::Never)
                {
                    break;
                }
                min_argument_count = i;
                if i == 0 {
                    break;
                } else {
                    i -= 1;
                }
            }
            signature.set_resolved_min_argument_count(min_argument_count);
        }
        Ok(signature.resolved_min_argument_count())
    }

    pub(super) fn has_effective_rest_parameter(&self, signature: &Signature) -> io::Result<bool> {
        if signature_has_rest_parameter(signature) {
            let rest_type =
                self.get_type_of_symbol(&signature.parameters()[signature.parameters().len() - 1])?;
            return Ok(!self.is_tuple_type(&rest_type)
                || rest_type
                    .as_type_reference_interface()
                    .target()
                    .as_tuple_type()
                    .has_rest_element);
        }
        Ok(false)
    }

    pub(super) fn get_effective_rest_type(
        &self,
        signature: &Signature,
    ) -> io::Result<Option<Gc<Type>>> {
        if signature_has_rest_parameter(signature) {
            let rest_type =
                self.get_type_of_symbol(&signature.parameters()[signature.parameters().len() - 1])?;
            if !self.is_tuple_type(&rest_type) {
                return Ok(Some(rest_type));
            }
            let rest_type_target = rest_type.as_type_reference_interface().target();
            let rest_type_target_as_tuple_type = rest_type_target.as_tuple_type();
            if rest_type_target_as_tuple_type.has_rest_element {
                return Ok(Some(self.slice_tuple_type(
                    &rest_type,
                    rest_type_target_as_tuple_type.fixed_length,
                    None,
                )?));
            }
        }
        Ok(None)
    }

    pub(super) fn get_non_array_rest_type(
        &self,
        signature: &Signature,
    ) -> io::Result<Option<Gc<Type>>> {
        let rest_type = self.get_effective_rest_type(signature)?;
        Ok(rest_type.filter(|rest_type| {
            !self.is_array_type(rest_type)
                && !self.is_type_any(Some(&**rest_type))
                && !self
                    .get_reduced_type(rest_type)?
                    .flags()
                    .intersects(TypeFlags::Never)
        }))
    }

    pub(super) fn get_type_of_first_parameter_of_signature(
        &self,
        signature: &Signature,
    ) -> io::Result<Gc<Type>> {
        self.get_type_of_first_parameter_of_signature_with_fallback(signature, &self.never_type())
    }
}
