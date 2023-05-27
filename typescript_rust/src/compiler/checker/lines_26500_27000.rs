use gc::Gc;
use std::borrow::Borrow;
use std::cmp;
use std::io;
use std::ptr;


use super::{CheckMode, IterationUse, JsxNames};
use crate::debug_fail_if_none;
use crate::return_ok_default_if_none;
use crate::try_filter;
use crate::try_map;
use crate::try_reduce_left_no_initial_value_optional;
use crate::OptionTry;
use crate::{
    concatenate, get_enclosing_block_scope_container, get_jsdoc_enum_tag,
    get_strict_option_value, has_static_modifier, is_assignment_target, is_binary_expression,
    is_class_expression, is_computed_property_name, is_function_expression_or_arrow_function,
    is_in_js_file, is_in_json_file, is_interface_declaration, is_known_symbol,
    is_named_declaration, is_object_literal_method, is_property_declaration, is_type_literal_node,
    length, maybe_is_class_like, parameter_is_this_keyword, unescape_leading_underscores, CheckFlags, ContextFlags, Debug_, Diagnostics,
    ElementFlags, ExternalEmitHelpers, HasInitializerInterface, IndexInfo, InterfaceTypeInterface,
    NamedDeclarationInterface, NodeCheckFlags, ScriptTarget, Signature, SignatureFlags,
    SignatureKind, SymbolFlags, Ternary, TransientSymbolInterface, TypeMapper, UnionReduction, create_symbol_table, get_object_flags, Node, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags,
    TypeInterface,
};

impl TypeChecker {
    pub(super) fn get_jsx_managed_attributes_from_located_attributes(
        &self,
        context: &Node, /*JsxOpeningLikeElement*/
        ns: Option<impl Borrow<Symbol>>,
        attributes_type: &Type,
    ) -> io::Result<Gc<Type>> {
        let managed_sym = self.get_jsx_library_managed_attributes(ns)?;
        if let Some(managed_sym) = managed_sym.as_ref() {
            let declared_managed_type = self.get_declared_type_of_symbol(managed_sym)?;
            let ctor_type = self.get_static_type_of_referenced_jsx_constructor(context)?;
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
                    )?;
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
                )?;
                return Ok(self.create_type_reference(&declared_managed_type, args));
            }
        }
        Ok(attributes_type.type_wrapper())
    }

    pub(super) fn get_jsx_props_type_from_class_type(
        &self,
        sig: Gc<Signature>,
        context: &Node, /*JsxOpeningLikeElement*/
    ) -> io::Result<Gc<Type>> {
        let ns = self.get_jsx_namespace_at(Some(context))?;
        let forced_lookup_location = self.get_jsx_element_properties_name(ns.as_deref())?;
        let attributes_type = match forced_lookup_location.as_ref() {
            None => Some(self.get_type_of_first_parameter_of_signature_with_fallback(
                &sig,
                &self.unknown_type(),
            )?),
            Some(forced_lookup_location) => {
                if forced_lookup_location.is_empty() {
                    Some(self.get_return_type_of_signature(sig.clone())?)
                } else {
                    self.get_jsx_props_type_for_signature_from_member(
                        sig.clone(),
                        forced_lookup_location,
                    )?
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
                            unescape_leading_underscores(forced_lookup_location).to_owned()
                        ])
                    );
                }
            }
            return Ok(self.unknown_type());
        }
        let mut attributes_type = attributes_type.unwrap();

        attributes_type = self.get_jsx_managed_attributes_from_located_attributes(
            context,
            ns.as_deref(),
            &attributes_type,
        )?;

        Ok(if self.is_type_any(Some(&*attributes_type)) {
            attributes_type
        } else {
            let mut apparent_attributes_type = attributes_type.clone();
            let intrinsic_class_attribs =
                self.get_jsx_type(&JsxNames::IntrinsicClassAttributes, Some(context))?;
            if !self.is_error_type(&intrinsic_class_attribs) {
                let type_params = self
                    .get_local_type_parameters_of_class_or_interface_or_type_alias(
                        &intrinsic_class_attribs.symbol(),
                    )?;
                let host_class_type = self.get_return_type_of_signature(sig.clone())?;
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
                                )?,
                            )
                        } else {
                            intrinsic_class_attribs
                        }),
                        Some(apparent_attributes_type),
                    )?
                    .unwrap();
            }

            let intrinsic_attribs =
                self.get_jsx_type(&JsxNames::IntrinsicAttributes, Some(context))?;
            if !self.is_error_type(&intrinsic_attribs) {
                apparent_attributes_type = self
                    .intersect_types(Some(intrinsic_attribs), Some(apparent_attributes_type))?
                    .unwrap();
            }

            apparent_attributes_type
        })
    }

    pub(super) fn get_intersected_signatures(
        &self,
        signatures: &[Gc<Signature>],
    ) -> io::Result<Option<Gc<Signature>>> {
        Ok(
            if get_strict_option_value(&self.compiler_options, "noImplicitAny") {
                try_reduce_left_no_initial_value_optional(
                    signatures,
                    |left: Option<Gc<Signature>>, right: &Gc<Signature>, _| -> io::Result<_> {
                        Ok(
                            if match left.as_ref() {
                                None => true,
                                Some(left) => Gc::ptr_eq(left, right),
                            } {
                                left
                            } else if self.compare_type_parameters_identical(
                                left.as_ref().unwrap().maybe_type_parameters().as_deref(),
                                right.maybe_type_parameters().as_deref(),
                            )? {
                                Some(self.combine_signatures_of_intersection_members(
                                    left.clone().unwrap(),
                                    right.clone(),
                                )?)
                            } else {
                                None
                            },
                        )
                    },
                    None,
                    None,
                )?
            } else {
                None
            },
        )
    }

    pub(super) fn combine_intersection_this_param(
        &self,
        left: Option<impl Borrow<Symbol>>,
        right: Option<impl Borrow<Symbol>>,
        mapper: Option<Gc<TypeMapper>>,
    ) -> io::Result<Option<Gc<Symbol>>> {
        let left = left.map(|left| left.borrow().symbol_wrapper());
        let right = right.map(|right| right.borrow().symbol_wrapper());
        if left.is_none() || right.is_none() {
            return Ok(left.or(right));
        }
        let left = left.unwrap();
        let right = right.unwrap();
        let this_type = self.get_union_type(
            &[
                self.get_type_of_symbol(&left)?,
                self.instantiate_type(&*self.get_type_of_symbol(&right)?, mapper)?,
            ],
            None,
            Option::<&Symbol>::None,
            None,
            Option::<&Type>::None,
        )?;
        Ok(Some(self.create_symbol_with_type(&left, Some(this_type))))
    }

    pub(super) fn combine_intersection_parameters(
        &self,
        left: &Signature,
        right: &Signature,
        mapper: Option<Gc<TypeMapper>>,
    ) -> io::Result<Vec<Gc<Symbol>>> {
        let left_count = self.get_parameter_count(left)?;
        let right_count = self.get_parameter_count(right)?;
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
            self.has_effective_rest_parameter(left)? || self.has_effective_rest_parameter(right)?;
        let needs_extra_rest_element =
            either_has_effective_rest && !self.has_effective_rest_parameter(longest)?;
        let mut params: Vec<Gc<Symbol>> =
            Vec::with_capacity(longest_count + if needs_extra_rest_element { 1 } else { 0 });
        for i in 0..longest_count {
            let mut longest_param_type = self.try_get_type_at_position(longest, i)?.unwrap();
            if ptr::eq(longest, right) {
                longest_param_type = self.instantiate_type(&longest_param_type, mapper.clone())?;
            }
            let mut shorter_param_type = self
                .try_get_type_at_position(shorter, i)?
                .unwrap_or_else(|| self.unknown_type());
            if ptr::eq(shorter, right) {
                shorter_param_type = self.instantiate_type(&shorter_param_type, mapper.clone())?;
            }
            let union_param_type = self.get_union_type(
                &[longest_param_type, shorter_param_type],
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )?;
            let is_rest_param =
                either_has_effective_rest && !needs_extra_rest_element && i == longest_count - 1;
            let is_optional = i >= self.get_min_argument_count(longest, None)?
                && i >= self.get_min_argument_count(shorter, None)?;
            let left_name = if i >= left_count {
                None
            } else {
                Some(self.get_parameter_name_at_position(left, i, Option::<&Type>::None)?)
            };
            let right_name = if i >= right_count {
                None
            } else {
                Some(self.get_parameter_name_at_position(right, i, Option::<&Type>::None)?)
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
            let param_symbol: Gc<Symbol> = self
                .create_symbol(
                    SymbolFlags::FunctionScopedVariable
                        | if is_optional && !is_rest_param {
                            SymbolFlags::Optional
                        } else {
                            SymbolFlags::None
                        },
                    param_name.unwrap_or_else(|| format!("arg{}", i)),
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
            let rest_param_symbol: Gc<Symbol> = self
                .create_symbol(SymbolFlags::FunctionScopedVariable, "args".to_owned(), None)
                .into();
            let rest_param_symbol_type =
                self.create_array_type(&*self.get_type_at_position(shorter, longest_count)?, None);
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
                    .type_ = Some(self.instantiate_type(&rest_param_symbol_type, mapper.clone())?);
            }
            params.push(rest_param_symbol);
        }
        Ok(params)
    }

    pub(super) fn combine_signatures_of_intersection_members(
        &self,
        left: Gc<Signature>,
        right: Gc<Signature>,
    ) -> io::Result<Gc<Signature>> {
        let type_params = left
            .maybe_type_parameters()
            .clone()
            .or_else(|| right.maybe_type_parameters().clone());
        let mut param_mapper: Option<Gc<TypeMapper>> = None;
        if left.maybe_type_parameters().is_some() && right.maybe_type_parameters().is_some() {
            param_mapper = Some(Gc::new(self.create_type_mapper(
                right.maybe_type_parameters().clone().unwrap(),
                left.maybe_type_parameters().clone(),
            )));
        }
        let declaration = left.declaration.as_ref();
        let params = self.combine_intersection_parameters(&left, &right, param_mapper.clone())?;
        let this_param = self.combine_intersection_this_param(
            left.maybe_this_parameter().as_deref(),
            right.maybe_this_parameter().as_deref(),
            param_mapper.clone(),
        )?;
        let min_arg_count = cmp::max(left.min_argument_count(), right.min_argument_count());
        let mut result = self.create_signature(
            declaration.cloned(),
            type_params,
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
        Ok(Gc::new(result))
    }

    pub(super) fn get_contextual_call_signature(
        &self,
        type_: &Type,
        node: &Node, /*SignatureDeclaration*/
    ) -> io::Result<Option<Gc<Signature>>> {
        let signatures = self.get_signatures_of_type(type_, SignatureKind::Call)?;
        let applicable_by_arity = try_filter(&signatures, |s| -> io::Result<_> {
            Ok(!self.is_arity_smaller(s, node)?)
        })?;
        Ok(if applicable_by_arity.len() == 1 {
            Some(applicable_by_arity[0].clone())
        } else {
            self.get_intersected_signatures(&applicable_by_arity)?
        })
    }

    pub(super) fn is_arity_smaller(
        &self,
        signature: &Signature,
        target: &Node, /*SignatureDeclaration*/
    ) -> io::Result<bool> {
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
        Ok(!self.has_effective_rest_parameter(signature)?
            && self.get_parameter_count(signature)? < target_parameter_count)
    }

    pub(super) fn get_contextual_signature_for_function_like_declaration(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> io::Result<Option<Gc<Signature>>> {
        Ok(
            if is_function_expression_or_arrow_function(node) || is_object_literal_method(node) {
                self.get_contextual_signature(node)?
            } else {
                None
            },
        )
    }

    pub(super) fn get_contextual_signature(
        &self,
        node: &Node, /*FunctionExpression | ArrowFunction | MethodDeclaration*/
    ) -> io::Result<Option<Gc<Signature>>> {
        Debug_.assert(
            node.kind() != SyntaxKind::MethodDeclaration || is_object_literal_method(node),
            None,
        );
        let type_tag_signature = self.get_signature_of_type_tag(node)?;
        if type_tag_signature.is_some() {
            return Ok(type_tag_signature);
        }
        let type_ = return_ok_default_if_none!(
            self.get_apparent_type_of_contextual_type(node, Some(ContextFlags::Signature))?
        );
        if !type_.flags().intersects(TypeFlags::Union) {
            return self.get_contextual_call_signature(&type_, node);
        }
        let mut signature_list: Option<Vec<Gc<Signature>>> = None;
        let types = type_.as_union_or_intersection_type_interface().types();
        for current in types {
            let signature = self.get_contextual_call_signature(current, node)?;
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
                        )? == Ternary::False
                        {
                            return Ok(None);
                        } else {
                            signature_list.push(signature);
                        }
                    }
                }
            }
        }

        Ok(signature_list.map(|signature_list| {
            if signature_list.len() == 1 {
                signature_list[0].clone()
            } else {
                Gc::new(self.create_union_signature(&signature_list[0].clone(), signature_list))
            }
        }))
    }

    pub(super) fn check_spread_expression(
        &self,
        node: &Node, /*SpreadElement*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Gc<Type>> {
        if self.language_version < ScriptTarget::ES2015 {
            self.check_external_emit_helpers(
                node,
                if self.compiler_options.downlevel_iteration == Some(true) {
                    ExternalEmitHelpers::SpreadIncludes
                } else {
                    ExternalEmitHelpers::SpreadArray
                },
            )?;
        }

        let array_or_iterable_type =
            self.check_expression(&node.as_spread_element().expression, check_mode, None)?;
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
    ) -> io::Result<Gc<Type>> {
        let node_as_synthetic_expression = node.as_synthetic_expression();
        Ok(if node_as_synthetic_expression.is_spread {
            self.get_indexed_access_type(
                &node_as_synthetic_expression.type_,
                &self.number_type(),
                None,
                Option::<&Node>::None,
                Option::<&Symbol>::None,
                None,
            )?
        } else {
            node_as_synthetic_expression.type_.clone()
        })
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
    ) -> io::Result<Gc<Type>> {
        let elements = &node.as_array_literal_expression().elements;
        let element_count = elements.len();
        let mut element_types: Vec<Gc<Type>> = vec![];
        let mut element_flags: Vec<ElementFlags> = vec![];
        let contextual_type = self.get_apparent_type_of_contextual_type(node, None)?;
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
                    )?;
                }
                let spread_type = self.check_expression(
                    &e.as_spread_element().expression,
                    check_mode,
                    force_tuple,
                )?;
                if self.is_array_like_type(&spread_type)? {
                    element_types.push(spread_type);
                    element_flags.push(ElementFlags::Variadic);
                } else if in_destructuring_pattern {
                    let rest_element_type = self
                        .get_index_type_of_type_(&spread_type, &self.number_type())?
                        .try_or_else(|| {
                            self.get_iterated_type_or_element_type(
                                IterationUse::Destructuring,
                                &spread_type,
                                &self.undefined_type(),
                                Option::<&Node>::None,
                                false,
                            )
                        })?
                        .unwrap_or_else(|| self.unknown_type());
                    element_types.push(rest_element_type);
                    element_flags.push(ElementFlags::Rest);
                } else {
                    element_types.push(self.check_iterated_type_or_element_type(
                        IterationUse::Spread,
                        &spread_type,
                        &self.undefined_type(),
                        Some(&*e.as_spread_element().expression),
                    )?);
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
                )?;
                let type_ = self.check_expression_for_mutable_location(
                    e,
                    check_mode,
                    element_contextual_type,
                    force_tuple,
                )?;
                element_types.push(self.add_optionality(
                    &type_,
                    Some(true),
                    Some(has_omitted_expression),
                )?);
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
                Some(contextual_type) if self.try_some_type(
                    contextual_type,
                    |type_: &Type| self.is_tuple_like_type(type_)
                )?
            )
        {
            return Ok(self.create_array_literal_type(&*self.create_tuple_type(
                &element_types,
                Some(&element_flags),
                Some(in_const_context),
                None,
            )?));
        }
        Ok(self.create_array_literal_type(&self.create_array_type(
            &*if !element_types.is_empty() {
                self.get_union_type(
                    &try_map(&element_types, |t: &Gc<Type>, i| -> io::Result<_> {
                        Ok(if element_flags[i].intersects(ElementFlags::Variadic) {
                            self.get_indexed_access_type_or_undefined(
                                t,
                                &self.number_type(),
                                None,
                                Option::<&Node>::None,
                                Option::<&Symbol>::None,
                                None,
                            )?
                            .unwrap_or_else(|| self.any_type())
                        } else {
                            t.clone()
                        })
                    })?,
                    Some(UnionReduction::Subtype),
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                )?
            } else if self.strict_null_checks {
                self.implicit_never_type()
            } else {
                self.undefined_widening_type()
            },
            Some(in_const_context),
        )))
    }

    pub(super) fn create_array_literal_type(&self, type_: &Type) -> Gc<Type> {
        if !get_object_flags(type_).intersects(ObjectFlags::Reference) {
            return type_.type_wrapper();
        }
        if type_
            .as_type_reference_interface()
            .maybe_literal_type()
            .is_none()
        {
            let literal_type = self.clone_type_reference(type_);
            let literal_type_as_type_reference = literal_type.as_type_reference();
            literal_type_as_type_reference.set_object_flags(
                literal_type_as_type_reference.object_flags()
                    | ObjectFlags::ArrayLiteral
                    | ObjectFlags::ContainsObjectOrArrayLiteral,
            );
            *type_.as_type_reference_interface().maybe_literal_type_mut() = Some(literal_type);
        }
        type_
            .as_type_reference_interface()
            .maybe_literal_type()
            .unwrap()
    }

    pub(super) fn is_numeric_name(&self, name: &Node /*DeclarationName*/) -> io::Result<bool> {
        Ok(match name.kind() {
            SyntaxKind::ComputedPropertyName => self.is_numeric_computed_name(name)?,
            SyntaxKind::Identifier => {
                self.is_numeric_literal_name(&name.as_identifier().escaped_text)
            }
            SyntaxKind::NumericLiteral | SyntaxKind::StringLiteral => {
                self.is_numeric_literal_name(&name.as_literal_like_node().text())
            }
            _ => false,
        })
    }

    pub(super) fn is_numeric_computed_name(
        &self,
        name: &Node, /*ComputedPropertyName*/
    ) -> io::Result<bool> {
        self.is_type_assignable_to_kind(
            &*self.check_computed_property_name(name)?,
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
    ) -> io::Result<Gc<Type>> {
        let node_as_computed_property_name = node.as_computed_property_name();
        let links = self.get_node_links(&node_as_computed_property_name.expression);
        if (*links).borrow().resolved_type.is_none() {
            if (is_type_literal_node(&node.parent().parent())
                || maybe_is_class_like(node.parent().maybe_parent())
                || is_interface_declaration(&node.parent().parent()))
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
                return Ok(ret);
            }
            let links_resolved_type =
                self.check_expression(&node_as_computed_property_name.expression, None, None)?;
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
                )? && !self.is_type_assignable_to(
                    &links_resolved_type,
                    &self.string_number_symbol_type(),
                )?
            {
                self.error(
                    Some(node),
                    &Diagnostics::A_computed_property_name_must_be_of_type_string_number_symbol_or_any,
                    None,
                );
            }
        }

        let ret = (*links).borrow().resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn is_symbol_with_numeric_name(&self, symbol: &Symbol) -> io::Result<bool> {
        let first_decl = symbol
            .maybe_declarations()
            .as_ref()
            .and_then(|symbol_declarations| symbol_declarations.get(0).cloned());
        Ok(self.is_numeric_literal_name(&symbol.escaped_name())
            || matches!(
                first_decl.as_ref(),
                Some(first_decl) if is_named_declaration(first_decl)
                    && self.is_numeric_name(&first_decl.as_named_declaration().name())?
            ))
    }

    pub(super) fn is_symbol_with_symbol_name(&self, symbol: &Symbol) -> io::Result<bool> {
        let first_decl = symbol
            .maybe_declarations()
            .as_ref()
            .and_then(|symbol_declarations| symbol_declarations.get(0).cloned());
        Ok(is_known_symbol(symbol)
            || matches!(
                first_decl.as_ref(),
                Some(first_decl) if is_named_declaration(first_decl) && is_computed_property_name(&first_decl.as_named_declaration().name()) &&
                    self.is_type_assignable_to_kind(
                        &*self.check_computed_property_name(&first_decl.as_named_declaration().name())?,
                        TypeFlags::ESSymbol,
                        None,
                    )?
            ))
    }

    pub(super) fn get_object_literal_index_info(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        offset: usize,
        properties: &[Gc<Symbol>],
        key_type: &Type,
    ) -> io::Result<IndexInfo> {
        let mut prop_types: Vec<Gc<Type>> = vec![];
        for i in offset..properties.len() {
            let prop = &properties[i];
            if ptr::eq(key_type, &*self.string_type()) && !self.is_symbol_with_symbol_name(prop)?
                || ptr::eq(key_type, &*self.number_type())
                    && self.is_symbol_with_numeric_name(prop)?
                || ptr::eq(key_type, &*self.es_symbol_type())
                    && self.is_symbol_with_symbol_name(prop)?
            {
                prop_types.push(self.get_type_of_symbol(&properties[i])?);
            }
        }
        let union_type = if !prop_types.is_empty() {
            self.get_union_type(
                &prop_types,
                Some(UnionReduction::Subtype),
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )?
        } else {
            self.undefined_type()
        };
        Ok(self.create_index_info(
            key_type.type_wrapper(),
            union_type,
            self.is_const_context(node),
            None,
        ))
    }

    pub(super) fn get_immediate_aliased_symbol(
        &self,
        symbol: &Symbol,
    ) -> io::Result<Option<Gc<Symbol>>> {
        Debug_.assert(
            symbol.flags().intersects(SymbolFlags::Alias),
            Some("Should only get Alias here."),
        );
        let links = self.get_symbol_links(symbol);
        if (*links).borrow().immediate_target.is_none() {
            let node = debug_fail_if_none!(self.get_declaration_of_alias_symbol(symbol)?);
            links.borrow_mut().immediate_target =
                self.get_target_of_alias_declaration(&node, Some(true))?;
        }

        let ret = (*links).borrow().immediate_target.clone();
        Ok(ret)
    }

    pub(super) fn check_object_literal(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Gc<Type>> {
        let in_destructuring_pattern = is_assignment_target(node);
        self.check_grammar_object_literal_expression(node, in_destructuring_pattern);

        let node_as_object_literal_expression = node.as_object_literal_expression();
        let mut all_properties_table = if self.strict_null_checks {
            Some(create_symbol_table(Option::<&[Gc<Symbol>]>::None))
        } else {
            None
        };
        let mut properties_table = create_symbol_table(Option::<&[Gc<Symbol>]>::None);
        let mut properties_array: Vec<Gc<Symbol>> = vec![];
        let mut spread: Gc<Type> = self.empty_object_type();

        let contextual_type = self.get_apparent_type_of_contextual_type(node, None)?;
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
                self.check_computed_property_name(&elem_name)?;
            }
        }

        let mut offset = 0;
        for member_decl in &node_as_object_literal_expression.properties {
            let mut member = self.get_symbol_of_node(member_decl)?;
            let computed_name_type = member_decl
                .as_named_declaration()
                .maybe_name()
                .as_ref()
                .filter(|member_decl_name| {
                    member_decl_name.kind() == SyntaxKind::ComputedPropertyName
                })
                .try_map(|member_decl_name| self.check_computed_property_name(member_decl_name))?;
            if matches!(
                member_decl.kind(),
                SyntaxKind::PropertyAssignment | SyntaxKind::ShorthandPropertyAssignment
            ) || is_object_literal_method(member_decl)
            {
                let member_present = member.as_ref().unwrap();
                let mut type_: Gc<Type> = if member_decl.kind() == SyntaxKind::PropertyAssignment {
                    self.check_property_assignment(member_decl, check_mode)?
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
                    )?
                } else {
                    self.check_object_literal_method(member_decl, check_mode)?
                };
                if is_in_javascript {
                    let js_doc_type =
                        self.get_type_for_declaration_from_jsdoc_comment(member_decl)?;
                    if let Some(js_doc_type) = js_doc_type.as_ref() {
                        self.check_type_assignable_to(
                            &type_,
                            js_doc_type,
                            Some(&**member_decl),
                            None,
                            None,
                            None,
                        )?;
                        type_ = js_doc_type.clone();
                    } else if let Some(enum_tag) = enum_tag.as_ref()
                    /*&& enumTag.typeExpression*/
                    {
                        self.check_type_assignable_to(
                            &type_,
                            &*self.get_type_from_type_node_(
                                enum_tag
                                    .as_base_jsdoc_type_like_tag()
                                    .type_expression
                                    .as_ref()
                                    .unwrap(),
                            )?,
                            Some(&**member_decl),
                            None,
                            None,
                            None,
                        )?;
                    }
                }
                object_flags |= get_object_flags(&type_) & ObjectFlags::PropagatingFlags;
                let name_type = computed_name_type.as_ref().filter(|computed_name_type| {
                    self.is_type_usable_as_property_name(computed_name_type)
                });
                let prop: Gc<Symbol> = if let Some(name_type) = name_type {
                    self.create_symbol(
                        SymbolFlags::Property | member_present.flags(),
                        self.get_property_name_from_type(name_type).into_owned(),
                        Some(check_flags | CheckFlags::Late),
                    )
                    .into()
                } else {
                    self.create_symbol(
                        SymbolFlags::Property | member_present.flags(),
                        member_present.escaped_name().to_owned(),
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
                        member_present.escaped_name(),
                        None,
                    )?;
                    if let Some(implied_prop) = implied_prop.as_ref() {
                        prop.set_flags(
                            prop.flags() | (implied_prop.flags() & SymbolFlags::Optional),
                        );
                    } else if self.compiler_options.suppress_excess_property_errors != Some(true)
                        && self
                            .get_index_info_of_type_(
                                contextual_type.as_ref().unwrap(),
                                &self.string_type(),
                            )?
                            .is_none()
                    {
                        self.error(
                            member_decl.as_named_declaration().maybe_name(),
                            &Diagnostics::Object_literal_may_only_specify_known_properties_and_0_does_not_exist_in_type_1,
                            Some(vec![
                                self.symbol_to_string_(
                                    &member_present,
                                    Option::<&Node>::None,
                                    None, None, None,
                                )?,
                                self.type_to_string_(
                                    contextual_type.as_ref().unwrap(),
                                    Option::<&Node>::None,
                                    None, None,
                                )?,
                            ])
                        );
                    }
                }

                if let Some(member_declarations) = member_present.maybe_declarations().clone() {
                    prop.set_declarations(member_declarations);
                }
                prop.set_parent(member_present.maybe_parent());
                if let Some(member_value_declaration) = member_present.maybe_value_declaration() {
                    prop.set_value_declaration(member_value_declaration);
                }

                {
                    let prop_links = prop.as_transient_symbol().symbol_links();
                    let mut prop_links = prop_links.borrow_mut();
                    prop_links.type_ = Some(type_.type_wrapper());
                    prop_links.target = Some(member_present.clone());
                }
                member = Some(prop.clone());
                if let Some(all_properties_table) = all_properties_table.as_mut() {
                    all_properties_table.insert(prop.escaped_name().to_owned(), prop.clone());
                };
            } else if member_decl.kind() == SyntaxKind::SpreadAssignment {
                if self.language_version < ScriptTarget::ES2015 {
                    self.check_external_emit_helpers(member_decl, ExternalEmitHelpers::Assign)?;
                }
                if !properties_array.is_empty() {
                    spread = self.get_spread_type(
                        &spread,
                        &*self.create_object_literal_type(
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
                        )?,
                        node.maybe_symbol(),
                        object_flags,
                        in_const_context,
                    )?;
                    properties_array = vec![];
                    properties_table = create_symbol_table(Option::<&[Gc<Symbol>]>::None);
                    has_computed_string_property = false;
                    has_computed_number_property = false;
                    has_computed_symbol_property = false;
                }
                let type_ = self.get_reduced_type(&*self.check_expression(
                    &member_decl.as_has_expression().expression(),
                    None,
                    None,
                )?)?;
                if self.is_valid_spread_type(&type_)? {
                    let merged_type = self.try_merge_union_of_object_type_and_empty_object(
                        &type_,
                        in_const_context,
                    )?;
                    if let Some(all_properties_table) = all_properties_table.as_ref() {
                        self.check_spread_prop_overrides(
                            &merged_type,
                            all_properties_table,
                            member_decl,
                        )?;
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
                    )?;
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
            let member = member.unwrap();

            if let Some(computed_name_type) =
                computed_name_type.as_ref().filter(|computed_name_type| {
                    !computed_name_type
                        .flags()
                        .intersects(TypeFlags::StringOrNumberLiteralOrUnique)
                })
            {
                if self
                    .is_type_assignable_to(computed_name_type, &self.string_number_symbol_type())?
                {
                    if self.is_type_assignable_to(computed_name_type, &self.number_type())? {
                        has_computed_number_property = true;
                    } else if self
                        .is_type_assignable_to(computed_name_type, &self.es_symbol_type())?
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
                properties_table.insert(member.escaped_name().to_owned(), member.clone());
            }
            properties_array.push(member);
        }

        if contextual_type_has_pattern && node.parent().kind() != SyntaxKind::SpreadAssignment {
            for ref prop in self.get_properties_of_type(contextual_type.as_ref().unwrap())? {
                if !properties_table.contains_key(prop.escaped_name())
                    && self
                        .get_property_of_type_(&spread, prop.escaped_name(), None)?
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
                    properties_table.insert(prop.escaped_name().to_owned(), prop.clone());
                    properties_array.push(prop.clone());
                }
            }
        }

        if self.is_error_type(&spread) {
            return Ok(self.error_type());
        }

        if !Gc::ptr_eq(&spread, &self.empty_object_type()) {
            if !properties_array.is_empty() {
                spread = self.get_spread_type(
                    &spread,
                    &*self.create_object_literal_type(
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
                    )?,
                    node.maybe_symbol(),
                    object_flags,
                    in_const_context,
                )?;
                properties_array = vec![];
                properties_table = create_symbol_table(Option::<&[Gc<Symbol>]>::None);
                has_computed_string_property = false;
                has_computed_number_property = false;
            }
            return Ok(self
                .try_map_type(
                    &spread,
                    &mut |t: &Type| -> io::Result<_> {
                        Ok(if ptr::eq(t, &*self.empty_object_type()) {
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
                            )?)
                        } else {
                            Some(t.type_wrapper())
                        })
                    },
                    None,
                )?
                .unwrap());
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
}
