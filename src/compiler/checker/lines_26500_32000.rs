#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::cmp;
use std::ptr;
use std::rc::Rc;

use super::{
    signature_has_rest_parameter, CheckMode, IterationUse, JsxNames, MinArgumentCountFlags,
    ResolveNameNameArg, TypeFacts, WideningKind,
};
use crate::{
    concatenate, filter, get_enclosing_block_scope_container, get_jsdoc_enum_tag,
    get_strict_option_value, has_static_modifier, is_assignment_target, is_binary_expression,
    is_class_expression, is_class_like, is_computed_property_name,
    is_function_expression_or_arrow_function, is_import_call, is_in_js_file, is_in_json_file,
    is_interface_declaration, is_known_symbol, is_named_declaration, is_object_literal_method,
    is_property_declaration, is_type_literal_node, length, parameter_is_this_keyword,
    reduce_left_no_initial_value_optional, same_map, unescape_leading_underscores, CheckFlags,
    ContextFlags, Debug_, Diagnostics, ElementFlags, ExternalEmitHelpers, FunctionFlags,
    HasInitializerInterface, IndexInfo, InterfaceTypeInterface, JsxReferenceKind,
    NamedDeclarationInterface, NodeCheckFlags, NodeFlags, ScriptTarget, Signature, SignatureFlags,
    SignatureKind, StringOrRcNode, SymbolFlags, SymbolTable, Ternary, TransientSymbolInterface,
    TypeMapper, UnionReduction, __String, create_symbol_table, get_function_flags,
    get_object_flags, has_initializer, InferenceContext, Node, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags,
    TypeInterface,
};

impl TypeChecker {
    pub(super) fn get_jsx_managed_attributes_from_located_attributes(
        &self,
        context: &Node, /*JsxOpeningLikeElement*/
        ns: &Symbol,
        attributes_type: &Type,
    ) -> Rc<Type> {
        let managed_sym = self.get_jsx_library_managed_attributes(ns);
        if let Some(managed_sym) = managed_sym.as_ref() {
            let declared_managed_type = self.get_declared_type_of_symbol(managed_sym);
            let ctor_type = self.get_static_type_of_referenced_jsx_constructor(context);
            if managed_sym.flags().intersects(SymbolFlags::TypeAlias) {
                let params = (*self.get_symbol_links(managed_sym))
                    .borrow()
                    .type_parameters
                    .clone();
                if length(params.as_deref()) >= 2 {
                    let args = self.fill_missing_type_arguments(
                        Some(vec![ctor_type, attributes_type.type_wrapper()]),
                        params.as_deref(),
                        2,
                        is_in_js_file(Some(context)),
                    );
                    return self.get_type_alias_instantiation(
                        managed_sym,
                        args.as_deref(),
                        Option::<&Symbol>::None,
                        None,
                    );
                }
            }
            if length(
                declared_managed_type
                    .as_interface_type()
                    .maybe_type_parameters(),
            ) >= 2
            {
                let args = self.fill_missing_type_arguments(
                    Some(vec![ctor_type, attributes_type.type_wrapper()]),
                    declared_managed_type
                        .as_interface_type()
                        .maybe_type_parameters(),
                    2,
                    is_in_js_file(Some(context)),
                );
                return self.create_type_reference(&declared_managed_type, args);
            }
        }
        attributes_type.type_wrapper()
    }

    pub(super) fn get_jsx_props_type_from_class_type(
        &self,
        sig: Rc<Signature>,
        context: &Node, /*JsxOpeningLikeElement*/
    ) -> Rc<Type> {
        let ns = self.get_jsx_namespace_at(Some(context));
        let forced_lookup_location = self.get_jsx_element_properties_name(&ns);
        let attributes_type = match forced_lookup_location.as_ref() {
            None => Some(self.get_type_of_first_parameter_of_signature_with_fallback(
                &sig,
                &self.unknown_type(),
            )),
            Some(forced_lookup_location) => {
                if forced_lookup_location.is_empty() {
                    Some(self.get_return_type_of_signature(sig.clone()))
                } else {
                    self.get_jsx_props_type_for_signature_from_member(
                        sig.clone(),
                        forced_lookup_location,
                    )
                }
            }
        };

        if attributes_type.is_none() {
            if let Some(forced_lookup_location) = forced_lookup_location.as_ref() {
                if length(Some(
                    &*context
                        .as_jsx_opening_like_element()
                        .attributes()
                        .as_jsx_attributes()
                        .properties,
                )) > 0
                {
                    self.error(
                        Some(context),
                        &Diagnostics::JSX_element_class_does_not_support_attributes_because_it_does_not_have_a_0_property,
                        Some(vec![
                            unescape_leading_underscores(forced_lookup_location)
                        ])
                    );
                }
            }
            return self.unknown_type();
        }
        let mut attributes_type = attributes_type.unwrap();

        attributes_type =
            self.get_jsx_managed_attributes_from_located_attributes(context, &ns, &attributes_type);

        if self.is_type_any(Some(&*attributes_type)) {
            attributes_type
        } else {
            let mut apparent_attributes_type = attributes_type.clone();
            let intrinsic_class_attribs =
                self.get_jsx_type(&JsxNames::IntrinsicClassAttributes, Some(context));
            if !self.is_error_type(&intrinsic_class_attribs) {
                let type_params = self
                    .get_local_type_parameters_of_class_or_interface_or_type_alias(
                        &intrinsic_class_attribs.symbol(),
                    );
                let host_class_type = self.get_return_type_of_signature(sig.clone());
                apparent_attributes_type = self
                    .intersect_types(
                        Some(if let Some(type_params) = type_params.as_ref() {
                            self.create_type_reference(
                                &intrinsic_class_attribs,
                                self.fill_missing_type_arguments(
                                    Some(vec![host_class_type]),
                                    Some(type_params),
                                    self.get_min_type_argument_count(Some(type_params)),
                                    is_in_js_file(Some(context)),
                                ),
                            )
                        } else {
                            intrinsic_class_attribs
                        }),
                        Some(apparent_attributes_type),
                    )
                    .unwrap();
            }

            let intrinsic_attribs =
                self.get_jsx_type(&JsxNames::IntrinsicAttributes, Some(context));
            if !self.is_error_type(&intrinsic_attribs) {
                apparent_attributes_type = self
                    .intersect_types(Some(intrinsic_attribs), Some(apparent_attributes_type))
                    .unwrap();
            }

            apparent_attributes_type
        }
    }

    pub(super) fn get_intersected_signatures(
        &self,
        signatures: &[Rc<Signature>],
    ) -> Option<Rc<Signature>> {
        if get_strict_option_value(&self.compiler_options, "noImplicitAny") {
            reduce_left_no_initial_value_optional(
                signatures,
                |left: Option<Rc<Signature>>, right: &Rc<Signature>, _| {
                    if match left.as_ref() {
                        None => true,
                        Some(left) => Rc::ptr_eq(left, right),
                    } {
                        left
                    } else if self.compare_type_parameters_identical(
                        left.as_ref().unwrap().type_parameters.as_deref(),
                        right.type_parameters.as_deref(),
                    ) {
                        Some(self.combine_signatures_of_intersection_members(
                            left.clone().unwrap(),
                            right.clone(),
                        ))
                    } else {
                        None
                    }
                },
                None,
                None,
            )
        } else {
            None
        }
    }

    pub(super) fn combine_intersection_this_param<TLeft: Borrow<Symbol>, TRight: Borrow<Symbol>>(
        &self,
        left: Option<TLeft>,
        right: Option<TRight>,
        mapper: Option<&TypeMapper>,
    ) -> Option<Rc<Symbol>> {
        let left = left.map(|left| left.borrow().symbol_wrapper());
        let right = right.map(|right| right.borrow().symbol_wrapper());
        if left.is_none() || right.is_none() {
            return left.or(right);
        }
        let left = left.unwrap();
        let right = right.unwrap();
        let this_type = self.get_union_type(
            vec![
                self.get_type_of_symbol(&left),
                self.instantiate_type(&self.get_type_of_symbol(&right), mapper),
            ],
            None,
            Option::<&Symbol>::None,
            None,
            Option::<&Type>::None,
        );
        Some(self.create_symbol_with_type(&left, Some(this_type)))
    }

    pub(super) fn combine_intersection_parameters(
        &self,
        left: &Signature,
        right: &Signature,
        mapper: Option<&TypeMapper>,
    ) -> Vec<Rc<Symbol>> {
        let left_count = self.get_parameter_count(left);
        let right_count = self.get_parameter_count(right);
        let longest = if left_count >= right_count {
            left
        } else {
            right
        };
        let shorter = if ptr::eq(longest, left) { right } else { left };
        let longest_count = if ptr::eq(longest, left) {
            left_count
        } else {
            right_count
        };
        let either_has_effective_rest =
            self.has_effective_rest_parameter(left) || self.has_effective_rest_parameter(right);
        let needs_extra_rest_element =
            either_has_effective_rest && !self.has_effective_rest_parameter(longest);
        let mut params: Vec<Rc<Symbol>> =
            Vec::with_capacity(longest_count + if needs_extra_rest_element { 1 } else { 0 });
        for i in 0..longest_count {
            let mut longest_param_type = self.try_get_type_at_position(longest, i).unwrap();
            if ptr::eq(longest, right) {
                longest_param_type = self.instantiate_type(&longest_param_type, mapper);
            }
            let mut shorter_param_type = self
                .try_get_type_at_position(shorter, i)
                .unwrap_or_else(|| self.unknown_type());
            if ptr::eq(shorter, right) {
                shorter_param_type = self.instantiate_type(&shorter_param_type, mapper);
            }
            let union_param_type = self.get_union_type(
                vec![longest_param_type, shorter_param_type],
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            );
            let is_rest_param =
                either_has_effective_rest && !needs_extra_rest_element && i == longest_count - 1;
            let is_optional = i >= self.get_min_argument_count(longest, None)
                && i >= self.get_min_argument_count(shorter, None);
            let left_name = if i >= left_count {
                None
            } else {
                Some(self.get_parameter_name_at_position(left, i, Option::<&Type>::None))
            };
            let right_name = if i >= right_count {
                None
            } else {
                Some(self.get_parameter_name_at_position(right, i, Option::<&Type>::None))
            };

            let param_name = if left_name == right_name {
                left_name
            } else if left_name.is_none() {
                right_name
            } else if right_name.is_none() {
                left_name
            } else {
                None
            };
            let param_symbol: Rc<Symbol> = self
                .create_symbol(
                    SymbolFlags::FunctionScopedVariable
                        | if is_optional && !is_rest_param {
                            SymbolFlags::Optional
                        } else {
                            SymbolFlags::None
                        },
                    param_name.unwrap_or_else(|| __String::new(format!("arg{}", i))),
                    None,
                )
                .into();
            param_symbol
                .as_transient_symbol()
                .symbol_links()
                .borrow_mut()
                .type_ = Some(if is_rest_param {
                self.create_array_type(&union_param_type, None)
            } else {
                union_param_type
            });
            params.push(param_symbol);
        }
        if needs_extra_rest_element {
            let rest_param_symbol: Rc<Symbol> = self
                .create_symbol(
                    SymbolFlags::FunctionScopedVariable,
                    __String::new("args".to_owned()),
                    None,
                )
                .into();
            let rest_param_symbol_type =
                self.create_array_type(&self.get_type_at_position(shorter, longest_count), None);
            rest_param_symbol
                .as_transient_symbol()
                .symbol_links()
                .borrow_mut()
                .type_ = Some(rest_param_symbol_type.clone());
            if ptr::eq(shorter, right) {
                rest_param_symbol
                    .as_transient_symbol()
                    .symbol_links()
                    .borrow_mut()
                    .type_ = Some(self.instantiate_type(&rest_param_symbol_type, mapper));
            }
            params.push(rest_param_symbol);
        }
        params
    }

    pub(super) fn combine_signatures_of_intersection_members(
        &self,
        left: Rc<Signature>,
        right: Rc<Signature>,
    ) -> Rc<Signature> {
        let type_params = left
            .type_parameters
            .as_ref()
            .or_else(|| right.type_parameters.as_ref());
        let mut param_mapper: Option<TypeMapper> = None;
        if left.type_parameters.is_some() && right.type_parameters.is_some() {
            param_mapper = Some(self.create_type_mapper(
                right.type_parameters.clone().unwrap(),
                left.type_parameters.clone(),
            ));
        }
        let declaration = left.declaration.as_ref();
        let params = self.combine_intersection_parameters(&left, &right, param_mapper.as_ref());
        let this_param = self.combine_intersection_this_param(
            left.this_parameter.as_deref(),
            right.this_parameter.as_deref(),
            param_mapper.as_ref(),
        );
        let min_arg_count = cmp::max(left.min_argument_count(), right.min_argument_count());
        let mut result = self.create_signature(
            declaration.cloned(),
            type_params.cloned(),
            this_param,
            params,
            None,
            None,
            min_arg_count,
            (left.flags | right.flags) & SignatureFlags::PropagatingFlags,
        );
        result.composite_kind = Some(TypeFlags::Intersection);
        result.composite_signatures = Some(concatenate(
            if left.composite_kind == Some(TypeFlags::Intersection) {
                left.composite_signatures.clone()
            } else {
                None
            }
            .unwrap_or_else(|| vec![left.clone()]),
            vec![right.clone()],
        ));
        if let Some(param_mapper) = param_mapper {
            result.mapper = Some(
                if left.composite_kind == Some(TypeFlags::Intersection)
                    && left.mapper.is_some()
                    && left.composite_signatures.is_some()
                {
                    self.combine_type_mappers(left.mapper.clone(), param_mapper)
                } else {
                    param_mapper
                },
            );
        }
        Rc::new(result)
    }

    pub(super) fn get_contextual_call_signature(
        &self,
        type_: &Type,
        node: &Node, /*SignatureDeclaration*/
    ) -> Option<Rc<Signature>> {
        let signatures = self.get_signatures_of_type(type_, SignatureKind::Call);
        let applicable_by_arity = filter(&signatures, |s| !self.is_arity_smaller(s, node));
        if applicable_by_arity.len() == 1 {
            Some(applicable_by_arity[0].clone())
        } else {
            self.get_intersected_signatures(&applicable_by_arity)
        }
    }

    pub(super) fn is_arity_smaller(
        &self,
        signature: &Signature,
        target: &Node, /*SignatureDeclaration*/
    ) -> bool {
        let mut target_parameter_count = 0;
        let target_as_signature_declaration = target.as_signature_declaration();
        while target_parameter_count < target_as_signature_declaration.parameters().len() {
            let param = &target_as_signature_declaration.parameters()[target_parameter_count];
            let param_as_parameter_declaration = param.as_parameter_declaration();
            if param_as_parameter_declaration.maybe_initializer().is_some()
                || param_as_parameter_declaration.question_token.is_some()
                || param_as_parameter_declaration.dot_dot_dot_token.is_some()
                || self.is_jsdoc_optional_parameter(param)
            {
                break;
            }
            target_parameter_count += 1;
        }
        if !target_as_signature_declaration.parameters().is_empty()
            && parameter_is_this_keyword(&target_as_signature_declaration.parameters()[0])
        {
            target_parameter_count -= 1;
        }
        !self.has_effective_rest_parameter(signature)
            && self.get_parameter_count(signature) < target_parameter_count
    }

    pub(super) fn get_contextual_signature_for_function_like_declaration(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> Option<Rc<Signature>> {
        if is_function_expression_or_arrow_function(node) || is_object_literal_method(node) {
            self.get_contextual_signature(node)
        } else {
            None
        }
    }

    pub(super) fn get_contextual_signature(
        &self,
        node: &Node, /*FunctionExpression | ArrowFunction | MethodDeclaration*/
    ) -> Option<Rc<Signature>> {
        Debug_.assert(
            node.kind() != SyntaxKind::MethodDeclaration || is_object_literal_method(node),
            None,
        );
        let type_tag_signature = self.get_signature_of_type_tag(node);
        if type_tag_signature.is_some() {
            return type_tag_signature;
        }
        let type_ =
            self.get_apparent_type_of_contextual_type(node, Some(ContextFlags::Signature))?;
        if !type_.flags().intersects(TypeFlags::Union) {
            return self.get_contextual_call_signature(&type_, node);
        }
        let mut signature_list: Option<Vec<Rc<Signature>>> = None;
        let types = type_.as_union_or_intersection_type_interface().types();
        for current in types {
            let signature = self.get_contextual_call_signature(current, node);
            if let Some(signature) = signature {
                match signature_list.as_mut() {
                    None => {
                        signature_list = Some(vec![signature]);
                    }
                    Some(signature_list) => {
                        if self.compare_signatures_identical(
                            signature_list[0].clone(),
                            signature.clone(),
                            false,
                            true,
                            true,
                            |a, b| self.compare_types_identical(a, b),
                        ) == Ternary::False
                        {
                            return None;
                        } else {
                            signature_list.push(signature);
                        }
                    }
                }
            }
        }

        signature_list.map(|signature_list| {
            if signature_list.len() == 1 {
                signature_list[0].clone()
            } else {
                Rc::new(self.create_union_signature(&signature_list[0].clone(), signature_list))
            }
        })
    }

    pub(super) fn check_spread_expression(
        &self,
        node: &Node, /*SpreadElement*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        if self.language_version < ScriptTarget::ES2015 {
            self.check_external_emit_helpers(
                node,
                if self.compiler_options.downlevel_iteration == Some(true) {
                    ExternalEmitHelpers::SpreadIncludes
                } else {
                    ExternalEmitHelpers::SpreadArray
                },
            );
        }

        let array_or_iterable_type =
            self.check_expression(&node.as_spread_element().expression, check_mode, None);
        self.check_iterated_type_or_element_type(
            IterationUse::Spread,
            &array_or_iterable_type,
            &self.undefined_type(),
            Some(&*node.as_spread_element().expression),
        )
    }

    pub(super) fn check_synthetic_expression(
        &self,
        node: &Node, /*SyntheticExpression*/
    ) -> Rc<Type> {
        let node_as_synthetic_expression = node.as_synthetic_expression();
        if node_as_synthetic_expression.is_spread {
            self.get_indexed_access_type(
                &node_as_synthetic_expression.type_,
                &self.number_type(),
                None,
                Option::<&Node>::None,
                Option::<&Symbol>::None,
                None,
            )
        } else {
            node_as_synthetic_expression.type_.clone()
        }
    }

    pub(super) fn has_default_value(
        &self,
        node: &Node, /*BindingElement | Expression*/
    ) -> bool {
        node.kind() == SyntaxKind::BindingElement
            && node.as_binding_element().maybe_initializer().is_some()
            || node.kind() == SyntaxKind::BinaryExpression
                && node.as_binary_expression().operator_token.kind() == SyntaxKind::EqualsToken
    }

    pub(super) fn check_array_literal(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
        check_mode: Option<CheckMode>,
        force_tuple: Option<bool>,
    ) -> Rc<Type> {
        let elements = &node.as_array_literal_expression().elements;
        let element_count = elements.len();
        let mut element_types: Vec<Rc<Type>> = vec![];
        let mut element_flags: Vec<ElementFlags> = vec![];
        let contextual_type = self.get_apparent_type_of_contextual_type(node, None);
        let in_destructuring_pattern = is_assignment_target(node);
        let in_const_context = self.is_const_context(node);
        let mut has_omitted_expression = false;
        for i in 0..element_count {
            let e = &elements[i];
            if e.kind() == SyntaxKind::SpreadElement {
                if self.language_version < ScriptTarget::ES2015 {
                    self.check_external_emit_helpers(
                        e,
                        if self.compiler_options.downlevel_iteration == Some(true) {
                            ExternalEmitHelpers::SpreadIncludes
                        } else {
                            ExternalEmitHelpers::SpreadArray
                        },
                    );
                }
                let spread_type = self.check_expression(
                    &e.as_spread_element().expression,
                    check_mode,
                    force_tuple,
                );
                if self.is_array_like_type(&spread_type) {
                    element_types.push(spread_type);
                    element_flags.push(ElementFlags::Variadic);
                } else if in_destructuring_pattern {
                    let rest_element_type = self
                        .get_index_type_of_type_(&spread_type, &self.number_type())
                        .unwrap_or_else(|| {
                            self.get_iterated_type_or_element_type(
                                IterationUse::Destructuring,
                                &spread_type,
                                &self.undefined_type(),
                                Option::<&Node>::None,
                                false,
                            )
                        }); /*|| unknownType*/
                    element_types.push(rest_element_type);
                    element_flags.push(ElementFlags::Rest);
                } else {
                    element_types.push(self.check_iterated_type_or_element_type(
                        IterationUse::Spread,
                        &spread_type,
                        &self.undefined_type(),
                        Some(&*e.as_spread_element().expression),
                    ));
                    element_flags.push(ElementFlags::Rest);
                }
            } else if self.exact_optional_property_types == Some(true)
                && e.kind() == SyntaxKind::OmittedExpression
            {
                has_omitted_expression = true;
                element_types.push(self.missing_type());
                element_flags.push(ElementFlags::Optional);
            } else {
                let element_contextual_type = self.get_contextual_type_for_element_expression(
                    contextual_type.as_deref(),
                    element_types.len(),
                );
                let type_ = self.check_expression_for_mutable_location(
                    e,
                    check_mode,
                    element_contextual_type,
                    force_tuple,
                );
                element_types.push(self.add_optionality(
                    &type_,
                    Some(true),
                    Some(has_omitted_expression),
                ));
                element_flags.push(if has_omitted_expression {
                    ElementFlags::Optional
                } else {
                    ElementFlags::Required
                });
            }
        }
        if in_destructuring_pattern {
            return self.create_tuple_type(&element_types, Some(&element_flags), None, None);
        }
        if force_tuple == Some(true)
            || in_const_context
            || matches!(
                contextual_type.as_ref(),
                Some(contextual_type) if self.some_type(
                    contextual_type,
                    |type_: &Type| self.is_tuple_like_type(type_)
                )
            )
        {
            return self.create_array_literal_type(&self.create_tuple_type(
                &element_types,
                Some(&element_flags),
                Some(in_const_context),
                None,
            ));
        }
        self.create_array_literal_type(&self.create_array_type(
            &*if !element_types.is_empty() {
                self.get_union_type(
                    same_map(&element_types, |t: &Rc<Type>, i| {
                        if element_flags[i].intersects(ElementFlags::Variadic) {
                            self.get_indexed_access_type_or_undefined(
                                t,
                                &self.number_type(),
                                None,
                                Option::<&Node>::None,
                                Option::<&Symbol>::None,
                                None,
                            )
                            .unwrap_or_else(|| self.any_type())
                        } else {
                            t.clone()
                        }
                    }),
                    None,
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                )
            } else if self.strict_null_checks {
                self.implicit_never_type()
            } else {
                self.undefined_widening_type()
            },
            Some(in_const_context),
        ))
    }

    pub(super) fn create_array_literal_type(&self, type_: &Type) -> Rc<Type> {
        if !get_object_flags(type_).intersects(ObjectFlags::Reference) {
            return type_.type_wrapper();
        }
        if type_.as_type_reference().maybe_literal_type().is_none() {
            let literal_type = self.clone_type_reference(type_);
            let literal_type_as_type_reference = literal_type.as_type_reference();
            literal_type_as_type_reference.set_object_flags(
                literal_type_as_type_reference.object_flags()
                    | ObjectFlags::ArrayLiteral
                    | ObjectFlags::ContainsObjectOrArrayLiteral,
            );
            *type_.as_type_reference().maybe_literal_type() = Some(literal_type);
        }
        type_
            .as_type_reference()
            .maybe_literal_type()
            .clone()
            .unwrap()
    }

    pub(super) fn is_numeric_name(&self, name: &Node /*DeclarationName*/) -> bool {
        match name.kind() {
            SyntaxKind::ComputedPropertyName => self.is_numeric_computed_name(name),
            SyntaxKind::Identifier => {
                self.is_numeric_literal_name(&name.as_identifier().escaped_text)
            }
            SyntaxKind::NumericLiteral | SyntaxKind::StringLiteral => {
                self.is_numeric_literal_name(&name.as_literal_like_node().text())
            }
            _ => false,
        }
    }

    pub(super) fn is_numeric_computed_name(
        &self,
        name: &Node, /*ComputedPropertyName*/
    ) -> bool {
        self.is_type_assignable_to_kind(
            &self.check_computed_property_name(name),
            TypeFlags::NumberLike,
            None,
        )
    }

    pub(super) fn is_numeric_literal_name(&self, name: &str) -> bool {
        matches!(
            name.parse::<f64>(),
            Ok(parsed) if parsed.to_string() == name
        )
    }

    pub(super) fn check_computed_property_name(
        &self,
        node: &Node, /*ComputedPropertyName*/
    ) -> Rc<Type> {
        let node_as_computed_property_name = node.as_computed_property_name();
        let links = self.get_node_links(&node_as_computed_property_name.expression);
        if (*links).borrow().resolved_type.is_none() {
            if is_type_literal_node(&node.parent().parent())
                || is_class_like(&node.parent().parent())
                || is_interface_declaration(&node.parent().parent())
                    && is_binary_expression(&node_as_computed_property_name.expression)
                    && node_as_computed_property_name
                        .expression
                        .as_binary_expression()
                        .operator_token
                        .kind()
                        == SyntaxKind::InKeyword
            {
                let ret = self.error_type();
                links.borrow_mut().resolved_type = Some(ret.clone());
                return ret;
            }
            let links_resolved_type =
                self.check_expression(&node_as_computed_property_name.expression, None, None);
            links.borrow_mut().resolved_type = Some(links_resolved_type.clone());
            if is_property_declaration(&node.parent())
                && !has_static_modifier(&node.parent())
                && is_class_expression(&node.parent().parent())
            {
                let container =
                    get_enclosing_block_scope_container(&node.parent().parent()).unwrap();
                let enclosing_iteration_statement =
                    self.get_enclosing_iteration_statement(&container);
                if let Some(enclosing_iteration_statement) = enclosing_iteration_statement.as_ref()
                {
                    self.get_node_links(enclosing_iteration_statement)
                        .borrow_mut()
                        .flags |= NodeCheckFlags::LoopWithCapturedBlockScopedBinding;
                    self.get_node_links(node).borrow_mut().flags |=
                        NodeCheckFlags::BlockScopedBindingInLoop;
                    self.get_node_links(&node.parent().parent())
                        .borrow_mut()
                        .flags |= NodeCheckFlags::BlockScopedBindingInLoop;
                }
            }
            if links_resolved_type.flags().intersects(TypeFlags::Nullable)
                || !self.is_type_assignable_to_kind(
                    &links_resolved_type,
                    TypeFlags::StringLike | TypeFlags::NumberLike | TypeFlags::ESSymbolLike,
                    None,
                ) && !self
                    .is_type_assignable_to(&links_resolved_type, &self.string_number_symbol_type())
            {
                self.error(
                    Some(node),
                    &Diagnostics::A_computed_property_name_must_be_of_type_string_number_symbol_or_any,
                    None,
                );
            }
        }

        let ret = (*links).borrow().resolved_type.clone().unwrap();
        ret
    }

    pub(super) fn is_symbol_with_numeric_name(&self, symbol: &Symbol) -> bool {
        let first_decl = symbol
            .maybe_declarations()
            .as_ref()
            .and_then(|symbol_declarations| symbol_declarations.get(0).cloned());
        self.is_numeric_literal_name(&symbol.escaped_name())
            || matches!(
                first_decl.as_ref(),
                Some(first_decl) if is_named_declaration(first_decl) && self.is_numeric_name(&first_decl.as_named_declaration().name())
            )
    }

    pub(super) fn is_symbol_with_symbol_name(&self, symbol: &Symbol) -> bool {
        let first_decl = symbol
            .maybe_declarations()
            .as_ref()
            .and_then(|symbol_declarations| symbol_declarations.get(0).cloned());
        is_known_symbol(symbol)
            || matches!(
                first_decl.as_ref(),
                Some(first_decl) if is_named_declaration(first_decl) && is_computed_property_name(&first_decl.as_named_declaration().name()) &&
                    self.is_type_assignable_to_kind(
                        &self.check_computed_property_name(&first_decl.as_named_declaration().name()),
                        TypeFlags::ESSymbol,
                        None,
                    )
            )
    }

    pub(super) fn get_object_literal_index_info(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        offset: usize,
        properties: &[Rc<Symbol>],
        key_type: &Type,
    ) -> IndexInfo {
        let mut prop_types: Vec<Rc<Type>> = vec![];
        for i in offset..properties.len() {
            let prop = &properties[i];
            if ptr::eq(key_type, &*self.string_type()) && !self.is_symbol_with_symbol_name(prop)
                || ptr::eq(key_type, &*self.number_type()) && self.is_symbol_with_numeric_name(prop)
                || ptr::eq(key_type, &*self.es_symbol_type())
                    && self.is_symbol_with_symbol_name(prop)
            {
                prop_types.push(self.get_type_of_symbol(&properties[i]));
            }
        }
        let union_type = if !prop_types.is_empty() {
            self.get_union_type(
                prop_types,
                Some(UnionReduction::Subtype),
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )
        } else {
            self.undefined_type()
        };
        self.create_index_info(
            key_type.type_wrapper(),
            union_type,
            self.is_const_context(node),
            None,
        )
    }

    pub(super) fn get_immediate_aliased_symbol(&self, symbol: &Symbol) -> Option<Rc<Symbol>> {
        Debug_.assert(
            symbol.flags().intersects(SymbolFlags::Alias),
            Some("Should only get Alias here."),
        );
        let links = self.get_symbol_links(symbol);
        if (*links).borrow().immediate_target.is_none() {
            let node = self.get_declaration_of_alias_symbol(symbol);
            if node.is_none() {
                Debug_.fail(None);
            }
            let node = node.unwrap();
            links.borrow_mut().immediate_target =
                self.get_target_of_alias_declaration(&node, Some(true));
        }

        let ret = (*links).borrow().immediate_target.clone();
        ret
    }

    pub(super) fn check_object_literal(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let in_destructuring_pattern = is_assignment_target(node);
        self.check_grammar_object_literal_expression(node, in_destructuring_pattern);

        let node_as_object_literal_expression = node.as_object_literal_expression();
        let mut all_properties_table = if self.strict_null_checks {
            Some(create_symbol_table(None))
        } else {
            None
        };
        let mut properties_table = create_symbol_table(None);
        let mut properties_array: Vec<Rc<Symbol>> = vec![];
        let mut spread: Rc<Type> = self.empty_object_type();

        let contextual_type = self.get_apparent_type_of_contextual_type(node, None);
        let contextual_type_has_pattern = matches!(
            contextual_type.as_ref(),
            Some(contextual_type) if matches!(
                contextual_type.maybe_pattern().as_ref(),
                Some(contextual_type_pattern) if matches!(
                    contextual_type_pattern.kind(),
                    SyntaxKind::ObjectBindingPattern |
                    SyntaxKind::ObjectLiteralExpression
                )
            )
        );
        let in_const_context = self.is_const_context(node);
        let check_flags = if in_const_context {
            CheckFlags::Readonly
        } else {
            CheckFlags::None
        };
        let is_in_javascript = is_in_js_file(Some(node)) && !is_in_json_file(Some(node));
        let enum_tag = get_jsdoc_enum_tag(node);
        let is_js_object_literal =
            contextual_type.is_none() && is_in_javascript && enum_tag.is_none();
        let mut object_flags = self.fresh_object_literal_flag;
        let mut pattern_with_computed_properties = false;
        let mut has_computed_string_property = false;
        let mut has_computed_number_property = false;
        let mut has_computed_symbol_property = false;

        for elem in &node_as_object_literal_expression.properties {
            if let Some(elem_name) = elem
                .as_named_declaration()
                .maybe_name()
                .filter(|elem_name| is_computed_property_name(elem_name))
            {
                self.check_computed_property_name(&elem_name);
            }
        }

        let mut offset = 0;
        for member_decl in &node_as_object_literal_expression.properties {
            let mut member = self.get_symbol_of_node(member_decl).unwrap();
            let computed_name_type = member_decl
                .as_named_declaration()
                .maybe_name()
                .as_ref()
                .filter(|member_decl_name| {
                    member_decl_name.kind() == SyntaxKind::ComputedPropertyName
                })
                .map(|member_decl_name| self.check_computed_property_name(member_decl_name));
            if matches!(
                member_decl.kind(),
                SyntaxKind::PropertyAssignment | SyntaxKind::ShorthandPropertyAssignment
            ) || is_object_literal_method(member_decl)
            {
                let mut type_: Rc<Type> = if member_decl.kind() == SyntaxKind::PropertyAssignment {
                    self.check_property_assignment(member_decl, check_mode)
                } else if member_decl.kind() == SyntaxKind::ShorthandPropertyAssignment {
                    let member_decl_as_shorthand_property_assignment =
                        member_decl.as_shorthand_property_assignment();
                    self.check_expression_for_mutable_location(
                        &*if !in_destructuring_pattern
                            && member_decl_as_shorthand_property_assignment
                                .object_assignment_initializer
                                .is_some()
                        {
                            member_decl_as_shorthand_property_assignment
                                .object_assignment_initializer
                                .clone()
                                .unwrap()
                        } else {
                            member_decl_as_shorthand_property_assignment.name()
                        },
                        check_mode,
                        Option::<&Type>::None,
                        None,
                    )
                } else {
                    self.check_object_literal_method(member_decl, check_mode)
                };
                if is_in_javascript {
                    let js_doc_type = self.get_type_for_declaration_from_jsdoc_comment(member_decl);
                    if let Some(js_doc_type) = js_doc_type.as_ref() {
                        self.check_type_assignable_to(
                            &type_,
                            js_doc_type,
                            Some(&**member_decl),
                            None,
                            None,
                            None,
                        );
                        type_ = js_doc_type.clone();
                    } else if let Some(enum_tag) = enum_tag.as_ref()
                    /*&& enumTag.typeExpression*/
                    {
                        self.check_type_assignable_to(
                            &type_,
                            &self.get_type_from_type_node_(
                                enum_tag
                                    .as_base_jsdoc_type_like_tag()
                                    .type_expression
                                    .as_ref()
                                    .unwrap(),
                            ),
                            Some(&**member_decl),
                            None,
                            None,
                            None,
                        );
                    }
                }
                object_flags |= get_object_flags(&type_) & ObjectFlags::PropagatingFlags;
                let name_type = computed_name_type.as_ref().filter(|computed_name_type| {
                    self.is_type_usable_as_property_name(computed_name_type)
                });
                let prop: Rc<Symbol> = if let Some(name_type) = name_type {
                    self.create_symbol(
                        SymbolFlags::Property | member.flags(),
                        self.get_property_name_from_type(name_type),
                        Some(check_flags | CheckFlags::Late),
                    )
                    .into()
                } else {
                    self.create_symbol(
                        SymbolFlags::Property | member.flags(),
                        member.escaped_name().clone(),
                        Some(check_flags | CheckFlags::Late),
                    )
                    .into()
                };
                if let Some(name_type) = name_type {
                    prop.as_transient_symbol()
                        .symbol_links()
                        .borrow_mut()
                        .name_type = Some(name_type.clone());
                }

                if in_destructuring_pattern {
                    let is_optional = member_decl.kind() == SyntaxKind::PropertyAssignment
                        && self.has_default_value(
                            &member_decl
                                .as_has_initializer()
                                .maybe_initializer()
                                .unwrap(),
                        )
                        || member_decl.kind() == SyntaxKind::ShorthandPropertyAssignment
                            && member_decl
                                .as_shorthand_property_assignment()
                                .object_assignment_initializer
                                .is_some();
                    if is_optional {
                        prop.set_flags(prop.flags() | SymbolFlags::Optional);
                    }
                } else if contextual_type_has_pattern
                    && !get_object_flags(contextual_type.as_ref().unwrap())
                        .intersects(ObjectFlags::ObjectLiteralPatternWithComputedProperties)
                {
                    let implied_prop = self.get_property_of_type_(
                        contextual_type.as_ref().unwrap(),
                        member.escaped_name(),
                        None,
                    );
                    if let Some(implied_prop) = implied_prop.as_ref() {
                        prop.set_flags(
                            prop.flags() | (implied_prop.flags() & SymbolFlags::Optional),
                        );
                    } else if self.compiler_options.suppress_excess_property_errors != Some(true)
                        && self
                            .get_index_info_of_type_(
                                contextual_type.as_ref().unwrap(),
                                &self.string_type(),
                            )
                            .is_none()
                    {
                        self.error(
                            member_decl.as_named_declaration().maybe_name(),
                            &Diagnostics::Object_literal_may_only_specify_known_properties_and_0_does_not_exist_in_type_1,
                            Some(vec![
                                self.symbol_to_string_(
                                    &member,
                                    Option::<&Node>::None,
                                    None, None, None,
                                ),
                                self.type_to_string_(
                                    contextual_type.as_ref().unwrap(),
                                    Option::<&Node>::None,
                                    None, None,
                                ),
                            ])
                        );
                    }
                }

                if let Some(member_declarations) = member.maybe_declarations().clone() {
                    prop.set_declarations(member_declarations);
                }
                prop.set_parent(member.maybe_parent());
                if let Some(member_value_declaration) = member.maybe_value_declaration() {
                    prop.set_value_declaration(member_value_declaration);
                }

                {
                    let prop_links = prop.as_transient_symbol().symbol_links();
                    let mut prop_links = prop_links.borrow_mut();
                    prop_links.type_ = Some(type_.type_wrapper());
                    prop_links.target = Some(member.clone());
                }
                member = prop.clone();
                if let Some(all_properties_table) = all_properties_table.as_mut() {
                    all_properties_table.insert(prop.escaped_name().clone(), prop.clone());
                };
            } else if member_decl.kind() == SyntaxKind::SpreadAssignment {
                if self.language_version < ScriptTarget::ES2015 {
                    self.check_external_emit_helpers(member_decl, ExternalEmitHelpers::Assign);
                }
                if !properties_array.is_empty() {
                    spread = self.get_spread_type(
                        &spread,
                        &self.create_object_literal_type(
                            has_computed_string_property,
                            node,
                            offset,
                            &properties_array,
                            has_computed_number_property,
                            has_computed_symbol_property,
                            &properties_table,
                            object_flags,
                            is_js_object_literal,
                            pattern_with_computed_properties,
                            in_destructuring_pattern,
                        ),
                        node.maybe_symbol(),
                        object_flags,
                        in_const_context,
                    );
                    properties_array = vec![];
                    properties_table = create_symbol_table(None);
                    has_computed_string_property = false;
                    has_computed_number_property = false;
                    has_computed_symbol_property = false;
                }
                let type_ = self.get_reduced_type(&self.check_expression(
                    &member_decl.as_has_expression().expression(),
                    None,
                    None,
                ));
                if self.is_valid_spread_type(&type_) {
                    let merged_type = self
                        .try_merge_union_of_object_type_and_empty_object(&type_, in_const_context);
                    if let Some(all_properties_table) = all_properties_table.as_ref() {
                        self.check_spread_prop_overrides(
                            &merged_type,
                            all_properties_table,
                            member_decl,
                        );
                    }
                    offset = properties_array.len();
                    if self.is_error_type(&spread) {
                        continue;
                    }
                    spread = self.get_spread_type(
                        &spread,
                        &merged_type,
                        node.maybe_symbol(),
                        object_flags,
                        in_const_context,
                    );
                } else {
                    self.error(
                        Some(&**member_decl),
                        &Diagnostics::Spread_types_may_only_be_created_from_object_types,
                        None,
                    );
                    spread = self.error_type();
                }
                continue;
            } else {
                Debug_.assert(
                    matches!(
                        member_decl.kind(),
                        SyntaxKind::GetAccessor | SyntaxKind::SetAccessor
                    ),
                    None,
                );
                self.check_node_deferred(member_decl);
            }

            if let Some(computed_name_type) =
                computed_name_type.as_ref().filter(|computed_name_type| {
                    !computed_name_type
                        .flags()
                        .intersects(TypeFlags::StringOrNumberLiteralOrUnique)
                })
            {
                if self.is_type_assignable_to(computed_name_type, &self.string_number_symbol_type())
                {
                    if self.is_type_assignable_to(computed_name_type, &self.number_type()) {
                        has_computed_number_property = true;
                    } else if self.is_type_assignable_to(computed_name_type, &self.es_symbol_type())
                    {
                        has_computed_symbol_property = true;
                    } else {
                        has_computed_string_property = true;
                    }
                    if in_destructuring_pattern {
                        pattern_with_computed_properties = true;
                    }
                }
            } else {
                properties_table.insert(member.escaped_name().clone(), member.clone());
            }
            properties_array.push(member);
        }

        if contextual_type_has_pattern && node.parent().kind() != SyntaxKind::SpreadAssignment {
            for ref prop in self.get_properties_of_type(contextual_type.as_ref().unwrap()) {
                if !properties_table.contains_key(prop.escaped_name())
                    && self
                        .get_property_of_type_(&spread, prop.escaped_name(), None)
                        .is_none()
                {
                    if !prop.flags().intersects(SymbolFlags::Optional) {
                        self.error(
                            prop.maybe_value_declaration().or_else(|| {
                                (*prop.as_transient_symbol().symbol_links()).borrow().binding_element.clone()
                            }),
                            &Diagnostics::Initializer_provides_no_value_for_this_binding_element_and_the_binding_element_has_no_default_value,
                            None,
                        );
                    }
                    properties_table.insert(prop.escaped_name().clone(), prop.clone());
                    properties_array.push(prop.clone());
                }
            }
        }

        if self.is_error_type(&spread) {
            return self.error_type();
        }

        if !Rc::ptr_eq(&spread, &self.empty_object_type()) {
            if !properties_array.is_empty() {
                spread = self.get_spread_type(
                    &spread,
                    &self.create_object_literal_type(
                        has_computed_string_property,
                        node,
                        offset,
                        &properties_array,
                        has_computed_number_property,
                        has_computed_symbol_property,
                        &properties_table,
                        object_flags,
                        is_js_object_literal,
                        pattern_with_computed_properties,
                        in_destructuring_pattern,
                    ),
                    node.maybe_symbol(),
                    object_flags,
                    in_const_context,
                );
                properties_array = vec![];
                properties_table = create_symbol_table(None);
                has_computed_string_property = false;
                has_computed_number_property = false;
            }
            return self
                .map_type(
                    &spread,
                    &mut |t: &Type| {
                        if ptr::eq(t, &*self.empty_object_type()) {
                            Some(self.create_object_literal_type(
                                has_computed_string_property,
                                node,
                                offset,
                                &properties_array,
                                has_computed_number_property,
                                has_computed_symbol_property,
                                &properties_table,
                                object_flags,
                                is_js_object_literal,
                                pattern_with_computed_properties,
                                in_destructuring_pattern,
                            ))
                        } else {
                            Some(t.type_wrapper())
                        }
                    },
                    None,
                )
                .unwrap();
        }

        self.create_object_literal_type(
            has_computed_string_property,
            node,
            offset,
            &properties_array,
            has_computed_number_property,
            has_computed_symbol_property,
            &properties_table,
            object_flags,
            is_js_object_literal,
            pattern_with_computed_properties,
            in_destructuring_pattern,
        )
    }

    pub(super) fn create_object_literal_type(
        &self,
        has_computed_string_property: bool,
        node: &Node,
        offset: usize,
        properties_array: &[Rc<Symbol>],
        has_computed_number_property: bool,
        has_computed_symbol_property: bool,
        properties_table: &SymbolTable,
        object_flags: ObjectFlags,
        is_js_object_literal: bool,
        pattern_with_computed_properties: bool,
        in_destructuring_pattern: bool,
    ) -> Rc<Type> {
        let mut index_infos: Vec<Rc<IndexInfo>> = vec![];
        if has_computed_string_property {
            index_infos.push(Rc::new(self.get_object_literal_index_info(
                node,
                offset,
                &properties_array,
                &self.string_type(),
            )));
        }
        if has_computed_number_property {
            index_infos.push(Rc::new(self.get_object_literal_index_info(
                node,
                offset,
                &properties_array,
                &self.number_type(),
            )));
        }
        if has_computed_symbol_property {
            index_infos.push(Rc::new(self.get_object_literal_index_info(
                node,
                offset,
                &properties_array,
                &self.es_symbol_type(),
            )));
        }
        let result: Rc<Type> = self
            .create_anonymous_type(
                node.maybe_symbol(),
                Rc::new(RefCell::new(properties_table.clone())),
                vec![],
                vec![],
                index_infos,
            )
            .into();
        let result_as_object_flags_type = result.as_object_flags_type();
        result_as_object_flags_type.set_object_flags(
            result_as_object_flags_type.object_flags()
                | object_flags
                | ObjectFlags::ObjectLiteral
                | ObjectFlags::ContainsObjectOrArrayLiteral,
        );
        if is_js_object_literal {
            result_as_object_flags_type.set_object_flags(
                result_as_object_flags_type.object_flags() | ObjectFlags::JSLiteral,
            );
        }
        if pattern_with_computed_properties {
            result_as_object_flags_type.set_object_flags(
                result_as_object_flags_type.object_flags()
                    | ObjectFlags::ObjectLiteralPatternWithComputedProperties,
            );
        }
        if in_destructuring_pattern {
            *result.maybe_pattern() = Some(node.node_wrapper());
        }
        result
    }

    pub(super) fn is_valid_spread_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_hyphenated_jsx_name(&self, name: &str) -> bool {
        unimplemented!()
    }

    pub(super) fn is_jsx_intrinsic_identifier(
        &self,
        tag_name: &Node, /*JsxTagNameExpression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_jsx_attribute(
        &self,
        node: &Node, /*JsxAttribute*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_jsx_children(
        &self,
        node: &Node, /*JsxElement | JsxFragment*/
        check_mode: Option<CheckMode>,
    ) -> Vec<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn check_spread_prop_overrides(
        &self,
        type_: &Type,
        props: &SymbolTable,
        spread: &Node, /*SpreadAssignment | JsxSpreadAttribute*/
    ) {
        unimplemented!()
    }

    pub(super) fn get_jsx_type<TLocation: Borrow<Node>>(
        &self,
        name: &__String,
        location: Option<TLocation>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_jsx_namespace_at<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
    ) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn get_jsx_element_properties_name(
        &self,
        jsx_namespace: &Symbol,
    ) -> Option<__String> {
        unimplemented!()
    }

    pub(super) fn get_jsx_library_managed_attributes(
        &self,
        jsx_namespace: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_element_children_property_name(
        &self,
        jsx_namespace: &Symbol,
    ) -> Option<__String> {
        unimplemented!()
    }

    pub(super) fn get_intrinsic_attributes_type_from_string_literal_type(
        &self,
        type_: &Type, /*StringLiteralType*/
        location: &Node,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_intrinsic_attributes_type_from_jsx_opening_like_element(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_known_property(
        &self,
        target_type: &Type,
        name: &__String,
        is_comparing_jsx_attributes: bool,
    ) -> bool {
        if target_type.flags().intersects(TypeFlags::Object) {
            if self
                .get_property_of_object_type(target_type, name)
                .is_some()
                || false
            {
                return true;
            }
        } else if target_type
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
            && self.is_excess_property_check_target(target_type)
        {
            unimplemented!()
        }
        false
    }

    pub(super) fn is_excess_property_check_target(&self, type_: &Type) -> bool {
        (type_.flags().intersects(TypeFlags::Object)
            && !(get_object_flags(type_)
                .intersects(ObjectFlags::ObjectLiteralPatternWithComputedProperties)))
            || type_.flags().intersects(TypeFlags::NonPrimitive)
            || (type_.flags().intersects(TypeFlags::Union) && unimplemented!())
            || (type_.flags().intersects(TypeFlags::Intersection) && unimplemented!())
    }

    pub(super) fn get_declaration_node_flags_from_symbol(&self, s: &Symbol) -> NodeFlags {
        unimplemented!()
    }

    pub(super) fn is_prototype_property(&self, symbol: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn check_non_null_expression(
        &self,
        node: &Node, /*Expression | QualifiedName*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_nullable_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_non_nullable_type_if_needed(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_non_null_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn lookup_symbol_for_private_identifier_declaration(
        &self,
        prop_name: &__String,
        location: &Node,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_symbol_for_private_identifier_expression(
        &self,
        priv_id: &Node, /*PrivateIdentifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_private_identifier_property_of_type_(
        &self,
        left_type: &Type,
        lexically_scoped_identifier: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn is_this_property_access_in_constructor(
        &self,
        node: &Node, /*ElementAccessExpression | PropertyAccessExpression | QualifiedName*/
        prop: &Symbol,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_unchecked_js_suggestion<TNode: Borrow<Node>, TSuggestion: Borrow<Symbol>>(
        &self,
        node: Option<TNode>,
        suggestion: Option<TSuggestion>,
        exclude_classes: bool,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_in_property_initializer_or_class_static_block(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn type_has_static_property(
        &self,
        prop_name: &__String,
        containing_type: &Type,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_suggested_lib_for_nonexistent_name(
        &self,
        name: ResolveNameNameArg,
    ) -> Option<String> {
        // unimplemented!()
        None
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_jsx_attribute<
        TName: Into<StringOrRcNode>,
    >(
        &self,
        name: TName, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_suggestion_for_nonexistent_property<TName: Into<StringOrRcNode>>(
        &self,
        name: TName, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_symbol_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        outer_name: &__String,
        meaning: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        // unimplemented!()
        None
    }

    pub(super) fn get_suggestion_for_nonexistent_symbol_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        outer_name: &__String,
        meaning: SymbolFlags,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_module(
        &self,
        name: &Node, /*Identifier*/
        target_module: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_suggestion_for_nonexistent_index_signature(
        &self,
        object_type: &Type,
        name: &Node, /*ElementAccessExpression*/
        keyed_type: &Type,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_type_for_nonexistent_string_literal_type(
        &self,
        source: &Type, /*StringLiteralType*/
        target: &Type, /*UnionType*/
    ) -> Option<Rc<Type /*StringLiteralType*/>> {
        unimplemented!()
    }

    pub(super) fn mark_property_as_referenced<TNodeForCheckWriteOnly: Borrow<Node>>(
        &self,
        prop: &Symbol,
        node_for_check_write_only: Option<TNodeForCheckWriteOnly>,
        is_self_type_access: bool,
    ) {
        unimplemented!()
    }

    pub(super) fn is_self_type_access<TParent: Borrow<Symbol>>(
        &self,
        name: &Node, /*Expression | QualifiedName*/
        parent: Option<TParent>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_valid_property_access_(
        &self,
        node: &Node, /*PropertyAccessExpression | QualifiedName | ImportTypeNode*/
        property_name: &__String,
    ) -> bool {
        unimplemented!()
    }

    pub fn is_valid_property_access_for_completions_(
        &self,
        node_in: &Node, /*PropertyAccessExpression | ImportTypeNode | QualifiedName*/
        type_: &Type,
        property: &Symbol,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn accepts_void(&self, t: &Type) -> bool {
        t.flags().intersects(TypeFlags::Void)
    }

    pub(super) fn get_single_call_signature(&self, type_: &Type) -> Option<Rc<Signature>> {
        unimplemented!()
    }

    pub(super) fn get_single_call_or_construct_signature(
        &self,
        type_: &Type,
    ) -> Option<Rc<Signature>> {
        unimplemented!()
    }

    pub(super) fn instantiate_signature_in_context_of<
        TCompareTypes: FnMut(&Type, &Type, Option<bool>) -> Ternary,
    >(
        &self,
        signature: &Signature,
        contextual_signature: &Signature,
        inference_context: Option<&InferenceContext>,
        compare_types: Option<&mut TCompareTypes>,
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn create_signature_for_jsx_intrinsic(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
        result: &Type,
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn get_resolved_signature_(
        &self,
        node: &Node, /*CallLikeExpression*/
        candidates_out_array: Option<&[Rc<Signature>]>,
        check_mode: Option<CheckMode>,
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn get_spread_argument_type<TContext: Borrow<InferenceContext>>(
        &self,
        args: &[Rc<Node /*Expression*/>],
        index: usize,
        arg_count: usize,
        rest_type: &Type,
        context: Option<TContext>,
        check_mode: CheckMode,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_jsx_reference_kind(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> JsxReferenceKind {
        unimplemented!()
    }

    pub(super) fn get_effective_call_arguments(
        &self,
        node: &Node, /*CallLikeExpression*/
    ) -> Vec<Rc<Node /*Expression*/>> {
        unimplemented!()
    }

    pub(super) fn is_js_constructor<TNode: Borrow<Node>>(&self, node: Option<TNode>) -> bool {
        unimplemented!()
    }

    pub(super) fn merge_js_symbols<TSource: Borrow<Symbol>>(
        &self,
        target: &Symbol,
        source: Option<TSource>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_assigned_class_symbol(
        &self,
        decl: &Node, /*Declaration*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_symbol_of_expando(
        &self,
        node: &Node,
        allow_declaration: bool,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_with_synthetic_default_only(
        &self,
        type_: &Type,
        symbol: &Symbol,
        original_symbol: &Symbol,
        module_specifier: &Node, /*Expression*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_type_with_synthetic_default_import_type(
        &self,
        type_: &Type,
        symbol: &Symbol,
        original_symbol: &Symbol,
        module_specifier: &Node, /*Expression*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_common_js_require(&self, node: &Node) -> bool {
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
