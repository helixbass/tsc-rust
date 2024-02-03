use std::{cmp, io, ptr};

use id_arena::Id;

use super::{CheckMode, IterationUse, JsxNames};
use crate::{
    concatenate, create_symbol_table, debug_fail_if_none, get_enclosing_block_scope_container,
    get_jsdoc_enum_tag, get_object_flags, get_strict_option_value, has_static_modifier,
    is_assignment_target, is_binary_expression, is_class_expression, is_computed_property_name,
    is_function_expression_or_arrow_function, is_in_js_file, is_in_json_file,
    is_interface_declaration, is_known_symbol, is_named_declaration, is_object_literal_method,
    is_property_declaration, is_type_literal_node, length, maybe_is_class_like,
    parameter_is_this_keyword, return_ok_default_if_none, try_filter, try_map,
    try_reduce_left_no_initial_value_optional, unescape_leading_underscores, CheckFlags,
    ContextFlags, Debug_, Diagnostics, ElementFlags, ExternalEmitHelpers, HasArena,
    HasInitializerInterface, InArena, IndexInfo, InterfaceTypeInterface, NamedDeclarationInterface,
    Node, NodeCheckFlags, NodeInterface, ObjectFlags, ObjectFlagsTypeInterface, OptionInArena,
    OptionTry, ScriptTarget, Signature, SignatureFlags, SignatureKind, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind, Ternary, TransientSymbolInterface, Type, TypeChecker, TypeFlags,
    TypeInterface, TypeMapper, UnionReduction,
};

impl TypeChecker {
    pub(super) fn get_jsx_managed_attributes_from_located_attributes(
        &self,
        context: Id<Node>, /*JsxOpeningLikeElement*/
        ns: Option<Id<Symbol>>,
        attributes_type: Id<Type>,
    ) -> io::Result<Id<Type>> {
        let managed_sym = self.get_jsx_library_managed_attributes(ns)?;
        if let Some(managed_sym) = managed_sym {
            let declared_managed_type = self.get_declared_type_of_symbol(managed_sym)?;
            let ctor_type = self.get_static_type_of_referenced_jsx_constructor(context)?;
            if managed_sym
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::TypeAlias)
            {
                let params = self
                    .get_symbol_links(managed_sym)
                    .ref_(self)
                    .type_parameters
                    .clone();
                if length(params.as_deref()) >= 2 {
                    let args = self.fill_missing_type_arguments(
                        Some(vec![ctor_type, attributes_type]),
                        params.as_deref(),
                        2,
                        is_in_js_file(Some(&context.ref_(self))),
                    )?;
                    return self.get_type_alias_instantiation(
                        managed_sym,
                        args.as_deref(),
                        Option::<Id<Symbol>>::None,
                        None,
                    );
                }
            }
            if length(
                declared_managed_type
                    .ref_(self)
                    .as_interface_type()
                    .maybe_type_parameters(),
            ) >= 2
            {
                let args = self.fill_missing_type_arguments(
                    Some(vec![ctor_type, attributes_type]),
                    declared_managed_type
                        .ref_(self)
                        .as_interface_type()
                        .maybe_type_parameters(),
                    2,
                    is_in_js_file(Some(&context.ref_(self))),
                )?;
                return Ok(self.create_type_reference(declared_managed_type, args));
            }
        }
        Ok(attributes_type)
    }

    pub(super) fn get_jsx_props_type_from_class_type(
        &self,
        sig: Id<Signature>,
        context: Id<Node>, /*JsxOpeningLikeElement*/
    ) -> io::Result<Id<Type>> {
        let ns = self.get_jsx_namespace_at(Some(context))?;
        let forced_lookup_location = self.get_jsx_element_properties_name(ns)?;
        let attributes_type = match forced_lookup_location.as_ref() {
            None => Some(self.get_type_of_first_parameter_of_signature_with_fallback(
                sig,
                self.unknown_type(),
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
                        .ref_(self)
                        .as_jsx_opening_like_element()
                        .attributes()
                        .ref_(self)
                        .as_jsx_attributes()
                        .properties
                        .ref_(self),
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

        attributes_type =
            self.get_jsx_managed_attributes_from_located_attributes(context, ns, attributes_type)?;

        Ok(if self.is_type_any(Some(attributes_type)) {
            attributes_type
        } else {
            let mut apparent_attributes_type = attributes_type.clone();
            let intrinsic_class_attribs =
                self.get_jsx_type(&JsxNames::IntrinsicClassAttributes, Some(context))?;
            if !self.is_error_type(intrinsic_class_attribs) {
                let type_params = self
                    .get_local_type_parameters_of_class_or_interface_or_type_alias(
                        intrinsic_class_attribs.ref_(self).symbol(),
                    )?;
                let host_class_type = self.get_return_type_of_signature(sig.clone())?;
                apparent_attributes_type = self
                    .intersect_types(
                        Some(if let Some(type_params) = type_params.as_ref() {
                            self.create_type_reference(
                                intrinsic_class_attribs,
                                self.fill_missing_type_arguments(
                                    Some(vec![host_class_type]),
                                    Some(type_params),
                                    self.get_min_type_argument_count(Some(type_params)),
                                    is_in_js_file(Some(&context.ref_(self))),
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
            if !self.is_error_type(intrinsic_attribs) {
                apparent_attributes_type = self
                    .intersect_types(Some(intrinsic_attribs), Some(apparent_attributes_type))?
                    .unwrap();
            }

            apparent_attributes_type
        })
    }

    pub(super) fn get_intersected_signatures(
        &self,
        signatures: &[Id<Signature>],
    ) -> io::Result<Option<Id<Signature>>> {
        Ok(
            if get_strict_option_value(&self.compiler_options.ref_(self), "noImplicitAny") {
                try_reduce_left_no_initial_value_optional(
                    signatures,
                    |left: Option<Id<Signature>>, &right: &Id<Signature>, _| -> io::Result<_> {
                        Ok(
                            if match left {
                                None => true,
                                Some(left) => left == right,
                            } {
                                left
                            } else if self.compare_type_parameters_identical(
                                left.unwrap().ref_(self).maybe_type_parameters().as_deref(),
                                right.ref_(self).maybe_type_parameters().as_deref(),
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
        left: Option<Id<Symbol>>,
        right: Option<Id<Symbol>>,
        mapper: Option<Id<TypeMapper>>,
    ) -> io::Result<Option<Id<Symbol>>> {
        if left.is_none() || right.is_none() {
            return Ok(left.or(right));
        }
        let left = left.unwrap();
        let right = right.unwrap();
        let this_type = self.get_union_type(
            &[
                self.get_type_of_symbol(left)?,
                self.instantiate_type(self.get_type_of_symbol(right)?, mapper)?,
            ],
            None,
            Option::<Id<Symbol>>::None,
            None,
            None,
        )?;
        Ok(Some(self.create_symbol_with_type(left, Some(this_type))))
    }

    pub(super) fn combine_intersection_parameters(
        &self,
        left: Id<Signature>,
        right: Id<Signature>,
        mapper: Option<Id<TypeMapper>>,
    ) -> io::Result<Vec<Id<Symbol>>> {
        let left_count = self.get_parameter_count(left)?;
        let right_count = self.get_parameter_count(right)?;
        let longest = if left_count >= right_count {
            left
        } else {
            right
        };
        let shorter = if longest == left { right } else { left };
        let longest_count = if longest == left {
            left_count
        } else {
            right_count
        };
        let either_has_effective_rest =
            self.has_effective_rest_parameter(left)? || self.has_effective_rest_parameter(right)?;
        let needs_extra_rest_element =
            either_has_effective_rest && !self.has_effective_rest_parameter(longest)?;
        let mut params: Vec<Id<Symbol>> =
            Vec::with_capacity(longest_count + if needs_extra_rest_element { 1 } else { 0 });
        for i in 0..longest_count {
            let mut longest_param_type = self.try_get_type_at_position(longest, i)?.unwrap();
            if longest == right {
                longest_param_type = self.instantiate_type(longest_param_type, mapper.clone())?;
            }
            let mut shorter_param_type = self
                .try_get_type_at_position(shorter, i)?
                .unwrap_or_else(|| self.unknown_type());
            if shorter == right {
                shorter_param_type = self.instantiate_type(shorter_param_type, mapper.clone())?;
            }
            let union_param_type = self.get_union_type(
                &[longest_param_type, shorter_param_type],
                None,
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?;
            let is_rest_param =
                either_has_effective_rest && !needs_extra_rest_element && i == longest_count - 1;
            let is_optional = i >= self.get_min_argument_count(longest, None)?
                && i >= self.get_min_argument_count(shorter, None)?;
            let left_name = if i >= left_count {
                None
            } else {
                Some(self.get_parameter_name_at_position(left, i, None)?)
            };
            let right_name = if i >= right_count {
                None
            } else {
                Some(self.get_parameter_name_at_position(right, i, None)?)
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
            let param_symbol = self.alloc_symbol(
                self.create_symbol(
                    SymbolFlags::FunctionScopedVariable
                        | if is_optional && !is_rest_param {
                            SymbolFlags::Optional
                        } else {
                            SymbolFlags::None
                        },
                    param_name.unwrap_or_else(|| format!("arg{}", i)),
                    None,
                )
                .into(),
            );
            param_symbol
                .ref_(self)
                .as_transient_symbol()
                .symbol_links()
                .ref_mut(self)
                .type_ = Some(if is_rest_param {
                self.create_array_type(union_param_type, None)
            } else {
                union_param_type
            });
            params.push(param_symbol);
        }
        if needs_extra_rest_element {
            let rest_param_symbol = self.alloc_symbol(
                self.create_symbol(SymbolFlags::FunctionScopedVariable, "args".to_owned(), None)
                    .into(),
            );
            let rest_param_symbol_type =
                self.create_array_type(self.get_type_at_position(shorter, longest_count)?, None);
            rest_param_symbol
                .ref_(self)
                .as_transient_symbol()
                .symbol_links()
                .ref_mut(self)
                .type_ = Some(rest_param_symbol_type.clone());
            if shorter == right {
                rest_param_symbol
                    .ref_(self)
                    .as_transient_symbol()
                    .symbol_links()
                    .ref_mut(self)
                    .type_ = Some(self.instantiate_type(rest_param_symbol_type, mapper.clone())?);
            }
            params.push(rest_param_symbol);
        }
        Ok(params)
    }

    pub(super) fn combine_signatures_of_intersection_members(
        &self,
        left: Id<Signature>,
        right: Id<Signature>,
    ) -> io::Result<Id<Signature>> {
        let type_params = left
            .ref_(self)
            .maybe_type_parameters()
            .clone()
            .or_else(|| right.ref_(self).maybe_type_parameters().clone());
        let mut param_mapper: Option<Id<TypeMapper>> = None;
        if left.ref_(self).maybe_type_parameters().is_some()
            && right.ref_(self).maybe_type_parameters().is_some()
        {
            param_mapper = Some(self.create_type_mapper(
                right.ref_(self).maybe_type_parameters().clone().unwrap(),
                left.ref_(self).maybe_type_parameters().clone(),
            ));
        }
        let declaration = left.ref_(self).declaration;
        let params = self.combine_intersection_parameters(left, right, param_mapper.clone())?;
        let this_param = self.combine_intersection_this_param(
            left.ref_(self).maybe_this_parameter(),
            right.ref_(self).maybe_this_parameter(),
            param_mapper.clone(),
        )?;
        let min_arg_count = cmp::max(
            left.ref_(self).min_argument_count(),
            right.ref_(self).min_argument_count(),
        );
        let mut result = self.create_signature(
            declaration,
            type_params,
            this_param,
            params,
            None,
            None,
            min_arg_count,
            (left.ref_(self).flags | right.ref_(self).flags) & SignatureFlags::PropagatingFlags,
        );
        result.composite_kind = Some(TypeFlags::Intersection);
        result.composite_signatures = Some(concatenate(
            if left.ref_(self).composite_kind == Some(TypeFlags::Intersection) {
                left.ref_(self).composite_signatures.clone()
            } else {
                None
            }
            .unwrap_or_else(|| vec![left.clone()]),
            vec![right.clone()],
        ));
        if let Some(param_mapper) = param_mapper {
            result.mapper = Some(
                if left.ref_(self).composite_kind == Some(TypeFlags::Intersection)
                    && left.ref_(self).mapper.is_some()
                    && left.ref_(self).composite_signatures.is_some()
                {
                    self.combine_type_mappers(left.ref_(self).mapper.clone(), param_mapper)
                } else {
                    param_mapper
                },
            );
        }
        Ok(self.alloc_signature(result))
    }

    pub(super) fn get_contextual_call_signature(
        &self,
        type_: Id<Type>,
        node: Id<Node>, /*SignatureDeclaration*/
    ) -> io::Result<Option<Id<Signature>>> {
        let signatures = self.get_signatures_of_type(type_, SignatureKind::Call)?;
        let applicable_by_arity = try_filter(&signatures, |&s| -> io::Result<_> {
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
        signature: Id<Signature>,
        target: Id<Node>, /*SignatureDeclaration*/
    ) -> io::Result<bool> {
        let mut target_parameter_count = 0;
        let target_ref = target.ref_(self);
        let target_as_signature_declaration = target_ref.as_signature_declaration();
        while target_parameter_count
            < target_as_signature_declaration
                .parameters()
                .ref_(self)
                .len()
        {
            let param =
                target_as_signature_declaration.parameters().ref_(self)[target_parameter_count];
            let param_ref = param.ref_(self);
            let param_as_parameter_declaration = param_ref.as_parameter_declaration();
            if param_as_parameter_declaration.maybe_initializer().is_some()
                || param_as_parameter_declaration.question_token.is_some()
                || param_as_parameter_declaration.dot_dot_dot_token.is_some()
                || self.is_jsdoc_optional_parameter(param)
            {
                break;
            }
            target_parameter_count += 1;
        }
        if !target_as_signature_declaration
            .parameters()
            .ref_(self)
            .is_empty()
            && parameter_is_this_keyword(
                target_as_signature_declaration.parameters().ref_(self)[0],
                self,
            )
        {
            target_parameter_count -= 1;
        }
        Ok(!self.has_effective_rest_parameter(signature)?
            && self.get_parameter_count(signature)? < target_parameter_count)
    }

    pub(super) fn get_contextual_signature_for_function_like_declaration(
        &self,
        node: Id<Node>, /*FunctionLikeDeclaration*/
    ) -> io::Result<Option<Id<Signature>>> {
        Ok(
            if is_function_expression_or_arrow_function(&node.ref_(self))
                || is_object_literal_method(node, self)
            {
                self.get_contextual_signature(node)?
            } else {
                None
            },
        )
    }

    pub(super) fn get_contextual_signature(
        &self,
        node: Id<Node>, /*FunctionExpression | ArrowFunction | MethodDeclaration*/
    ) -> io::Result<Option<Id<Signature>>> {
        Debug_.assert(
            node.ref_(self).kind() != SyntaxKind::MethodDeclaration
                || is_object_literal_method(node, self),
            None,
        );
        let type_tag_signature = self.get_signature_of_type_tag(node)?;
        if type_tag_signature.is_some() {
            return Ok(type_tag_signature);
        }
        let type_ = return_ok_default_if_none!(
            self.get_apparent_type_of_contextual_type(node, Some(ContextFlags::Signature))?
        );
        if !type_.ref_(self).flags().intersects(TypeFlags::Union) {
            return self.get_contextual_call_signature(type_, node);
        }
        let mut signature_list: Option<Vec<Id<Signature>>> = None;
        let types = type_
            .ref_(self)
            .as_union_or_intersection_type_interface()
            .types()
            .to_owned();
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
                self.alloc_signature(
                    self.create_union_signature(signature_list[0].clone(), signature_list),
                )
            }
        }))
    }

    pub(super) fn check_spread_expression(
        &self,
        node: Id<Node>, /*SpreadElement*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        if self.language_version < ScriptTarget::ES2015 {
            self.check_external_emit_helpers(
                node,
                if self.compiler_options.ref_(self).downlevel_iteration == Some(true) {
                    ExternalEmitHelpers::SpreadIncludes
                } else {
                    ExternalEmitHelpers::SpreadArray
                },
            )?;
        }

        let array_or_iterable_type = self.check_expression(
            node.ref_(self).as_spread_element().expression,
            check_mode,
            None,
        )?;
        self.check_iterated_type_or_element_type(
            IterationUse::Spread,
            array_or_iterable_type,
            self.undefined_type(),
            Some(node.ref_(self).as_spread_element().expression),
        )
    }

    pub(super) fn check_synthetic_expression(
        &self,
        node: Id<Node>, /*SyntheticExpression*/
    ) -> io::Result<Id<Type>> {
        let node_ref = node.ref_(self);
        let node_as_synthetic_expression = node_ref.as_synthetic_expression();
        Ok(if node_as_synthetic_expression.is_spread {
            self.get_indexed_access_type(
                node_as_synthetic_expression.type_,
                self.number_type(),
                None,
                Option::<Id<Node>>::None,
                Option::<Id<Symbol>>::None,
                None,
            )?
        } else {
            node_as_synthetic_expression.type_
        })
    }

    pub(super) fn has_default_value(
        &self,
        node: Id<Node>, /*BindingElement | Expression*/
    ) -> bool {
        node.ref_(self).kind() == SyntaxKind::BindingElement
            && node
                .ref_(self)
                .as_binding_element()
                .maybe_initializer()
                .is_some()
            || node.ref_(self).kind() == SyntaxKind::BinaryExpression
                && node
                    .ref_(self)
                    .as_binary_expression()
                    .operator_token
                    .ref_(self)
                    .kind()
                    == SyntaxKind::EqualsToken
    }

    pub(super) fn check_array_literal(
        &self,
        node: Id<Node>, /*ArrayLiteralExpression*/
        check_mode: Option<CheckMode>,
        force_tuple: Option<bool>,
    ) -> io::Result<Id<Type>> {
        let node_ref = node.ref_(self);
        let elements = &node_ref.as_array_literal_expression().elements;
        let element_count = elements.ref_(self).len();
        let mut element_types: Vec<Id<Type>> = vec![];
        let mut element_flags: Vec<ElementFlags> = vec![];
        let contextual_type = self.get_apparent_type_of_contextual_type(node, None)?;
        let in_destructuring_pattern = is_assignment_target(node, self);
        let in_const_context = self.is_const_context(node);
        let mut has_omitted_expression = false;
        for i in 0..element_count {
            let e = elements.ref_(self)[i];
            if e.ref_(self).kind() == SyntaxKind::SpreadElement {
                if self.language_version < ScriptTarget::ES2015 {
                    self.check_external_emit_helpers(
                        e,
                        if self.compiler_options.ref_(self).downlevel_iteration == Some(true) {
                            ExternalEmitHelpers::SpreadIncludes
                        } else {
                            ExternalEmitHelpers::SpreadArray
                        },
                    )?;
                }
                let spread_type = self.check_expression(
                    e.ref_(self).as_spread_element().expression,
                    check_mode,
                    force_tuple,
                )?;
                if self.is_array_like_type(spread_type)? {
                    element_types.push(spread_type);
                    element_flags.push(ElementFlags::Variadic);
                } else if in_destructuring_pattern {
                    let rest_element_type = self
                        .get_index_type_of_type_(spread_type, self.number_type())?
                        .try_or_else(|| {
                            self.get_iterated_type_or_element_type(
                                IterationUse::Destructuring,
                                spread_type,
                                self.undefined_type(),
                                Option::<Id<Node>>::None,
                                false,
                            )
                        })?
                        .unwrap_or_else(|| self.unknown_type());
                    element_types.push(rest_element_type);
                    element_flags.push(ElementFlags::Rest);
                } else {
                    element_types.push(self.check_iterated_type_or_element_type(
                        IterationUse::Spread,
                        spread_type,
                        self.undefined_type(),
                        Some(e.ref_(self).as_spread_element().expression),
                    )?);
                    element_flags.push(ElementFlags::Rest);
                }
            } else if self.exact_optional_property_types == Some(true)
                && e.ref_(self).kind() == SyntaxKind::OmittedExpression
            {
                has_omitted_expression = true;
                element_types.push(self.missing_type());
                element_flags.push(ElementFlags::Optional);
            } else {
                let element_contextual_type = self.get_contextual_type_for_element_expression(
                    contextual_type,
                    element_types.len(),
                )?;
                let type_ = self.check_expression_for_mutable_location(
                    e,
                    check_mode,
                    element_contextual_type,
                    force_tuple,
                )?;
                element_types.push(self.add_optionality(
                    type_,
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
                contextual_type,
                Some(contextual_type) if self.try_some_type(
                    contextual_type,
                    |type_: Id<Type>| self.is_tuple_like_type(type_)
                )?
            )
        {
            return Ok(self.create_array_literal_type(self.create_tuple_type(
                &element_types,
                Some(&element_flags),
                Some(in_const_context),
                None,
            )?));
        }
        Ok(self.create_array_literal_type(self.create_array_type(
            if !element_types.is_empty() {
                self.get_union_type(
                    &try_map(&element_types, |&t: &Id<Type>, i| -> io::Result<_> {
                        Ok(if element_flags[i].intersects(ElementFlags::Variadic) {
                            self.get_indexed_access_type_or_undefined(
                                t,
                                self.number_type(),
                                None,
                                Option::<Id<Node>>::None,
                                Option::<Id<Symbol>>::None,
                                None,
                            )?
                            .unwrap_or_else(|| self.any_type())
                        } else {
                            t.clone()
                        })
                    })?,
                    Some(UnionReduction::Subtype),
                    Option::<Id<Symbol>>::None,
                    None,
                    None,
                )?
            } else if self.strict_null_checks {
                self.implicit_never_type()
            } else {
                self.undefined_widening_type()
            },
            Some(in_const_context),
        )))
    }

    pub(super) fn create_array_literal_type(&self, type_: Id<Type>) -> Id<Type> {
        if !get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::Reference) {
            return type_;
        }
        if type_
            .ref_(self)
            .as_type_reference_interface()
            .maybe_literal_type()
            .is_none()
        {
            let literal_type = self.clone_type_reference(type_);
            {
                let literal_type = literal_type.ref_(self);
                let literal_type_as_type_reference = literal_type.as_type_reference();
                literal_type_as_type_reference.set_object_flags(
                    literal_type_as_type_reference.object_flags()
                        | ObjectFlags::ArrayLiteral
                        | ObjectFlags::ContainsObjectOrArrayLiteral,
                );
            }
            type_
                .ref_(self)
                .as_type_reference_interface()
                .set_literal_type(Some(literal_type));
        }
        type_
            .ref_(self)
            .as_type_reference_interface()
            .maybe_literal_type()
            .unwrap()
    }

    pub(super) fn is_numeric_name(
        &self,
        name: Id<Node>, /*DeclarationName*/
    ) -> io::Result<bool> {
        Ok(match name.ref_(self).kind() {
            SyntaxKind::ComputedPropertyName => self.is_numeric_computed_name(name)?,
            SyntaxKind::Identifier => {
                self.is_numeric_literal_name(&name.ref_(self).as_identifier().escaped_text)
            }
            SyntaxKind::NumericLiteral | SyntaxKind::StringLiteral => {
                self.is_numeric_literal_name(&name.ref_(self).as_literal_like_node().text())
            }
            _ => false,
        })
    }

    pub(super) fn is_numeric_computed_name(
        &self,
        name: Id<Node>, /*ComputedPropertyName*/
    ) -> io::Result<bool> {
        self.is_type_assignable_to_kind(
            self.check_computed_property_name(name)?,
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
        node: Id<Node>, /*ComputedPropertyName*/
    ) -> io::Result<Id<Type>> {
        let node_ref = node.ref_(self);
        let node_as_computed_property_name = node_ref.as_computed_property_name();
        let links = self.get_node_links(node_as_computed_property_name.expression);
        if links.ref_(self).resolved_type.is_none() {
            if (is_type_literal_node(&node.ref_(self).parent().ref_(self).parent().ref_(self))
                || maybe_is_class_like(
                    node.ref_(self)
                        .parent()
                        .ref_(self)
                        .maybe_parent()
                        .refed(self)
                        .as_deref(),
                )
                || is_interface_declaration(
                    &node.ref_(self).parent().ref_(self).parent().ref_(self),
                ))
                && is_binary_expression(&node_as_computed_property_name.expression.ref_(self))
                && node_as_computed_property_name
                    .expression
                    .ref_(self)
                    .as_binary_expression()
                    .operator_token
                    .ref_(self)
                    .kind()
                    == SyntaxKind::InKeyword
            {
                let ret = self.error_type();
                links.ref_mut(self).resolved_type = Some(ret.clone());
                return Ok(ret);
            }
            let links_resolved_type =
                self.check_expression(node_as_computed_property_name.expression, None, None)?;
            links.ref_mut(self).resolved_type = Some(links_resolved_type.clone());
            if is_property_declaration(&node.ref_(self).parent().ref_(self))
                && !has_static_modifier(node.ref_(self).parent(), self)
                && is_class_expression(&node.ref_(self).parent().ref_(self).parent().ref_(self))
            {
                let container = get_enclosing_block_scope_container(
                    node.ref_(self).parent().ref_(self).parent(),
                    self,
                )
                .unwrap();
                let enclosing_iteration_statement =
                    self.get_enclosing_iteration_statement(container);
                if let Some(enclosing_iteration_statement) = enclosing_iteration_statement {
                    self.get_node_links(enclosing_iteration_statement)
                        .ref_mut(self)
                        .flags |= NodeCheckFlags::LoopWithCapturedBlockScopedBinding;
                    self.get_node_links(node).ref_mut(self).flags |=
                        NodeCheckFlags::BlockScopedBindingInLoop;
                    self.get_node_links(node.ref_(self).parent().ref_(self).parent())
                        .ref_mut(self)
                        .flags |= NodeCheckFlags::BlockScopedBindingInLoop;
                }
            }
            if links_resolved_type
                .ref_(self)
                .flags()
                .intersects(TypeFlags::Nullable)
                || !self.is_type_assignable_to_kind(
                    links_resolved_type,
                    TypeFlags::StringLike | TypeFlags::NumberLike | TypeFlags::ESSymbolLike,
                    None,
                )? && !self
                    .is_type_assignable_to(links_resolved_type, self.string_number_symbol_type())?
            {
                self.error(
                    Some(node),
                    &Diagnostics::A_computed_property_name_must_be_of_type_string_number_symbol_or_any,
                    None,
                );
            }
        }

        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn is_symbol_with_numeric_name(&self, symbol: Id<Symbol>) -> io::Result<bool> {
        let first_decl = symbol
            .ref_(self)
            .maybe_declarations()
            .as_ref()
            .and_then(|symbol_declarations| symbol_declarations.get(0).cloned());
        Ok(
            self.is_numeric_literal_name(&symbol.ref_(self).escaped_name())
                || matches!(
                    first_decl,
                    Some(first_decl) if is_named_declaration(&first_decl.ref_(self))
                        && self.is_numeric_name(first_decl.ref_(self).as_named_declaration().name())?
                ),
        )
    }

    pub(super) fn is_symbol_with_symbol_name(&self, symbol: Id<Symbol>) -> io::Result<bool> {
        let first_decl = symbol
            .ref_(self)
            .maybe_declarations()
            .as_ref()
            .and_then(|symbol_declarations| symbol_declarations.get(0).cloned());
        Ok(is_known_symbol(&symbol.ref_(self))
            || matches!(
                first_decl,
                Some(first_decl) if is_named_declaration(&first_decl.ref_(self)) &&
                    is_computed_property_name(&first_decl.ref_(self).as_named_declaration().name().ref_(self)) &&
                    self.is_type_assignable_to_kind(
                        self.check_computed_property_name(first_decl.ref_(self).as_named_declaration().name())?,
                        TypeFlags::ESSymbol,
                        None,
                    )?
            ))
    }

    pub(super) fn get_object_literal_index_info(
        &self,
        node: Id<Node>, /*ObjectLiteralExpression*/
        offset: usize,
        properties: &[Id<Symbol>],
        key_type: Id<Type>,
    ) -> io::Result<IndexInfo> {
        let mut prop_types: Vec<Id<Type>> = vec![];
        for i in offset..properties.len() {
            let prop = properties[i];
            if key_type == self.string_type() && !self.is_symbol_with_symbol_name(prop)?
                || key_type == self.number_type() && self.is_symbol_with_numeric_name(prop)?
                || key_type == self.es_symbol_type() && self.is_symbol_with_symbol_name(prop)?
            {
                prop_types.push(self.get_type_of_symbol(properties[i])?);
            }
        }
        let union_type = if !prop_types.is_empty() {
            self.get_union_type(
                &prop_types,
                Some(UnionReduction::Subtype),
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?
        } else {
            self.undefined_type()
        };
        Ok(self.create_index_info(key_type, union_type, self.is_const_context(node), None))
    }

    pub(super) fn get_immediate_aliased_symbol(
        &self,
        symbol: Id<Symbol>,
    ) -> io::Result<Option<Id<Symbol>>> {
        Debug_.assert(
            symbol.ref_(self).flags().intersects(SymbolFlags::Alias),
            Some("Should only get Alias here."),
        );
        let links = self.get_symbol_links(symbol);
        if links.ref_(self).immediate_target.is_none() {
            let node = debug_fail_if_none!(self.get_declaration_of_alias_symbol(symbol)?);
            links.ref_mut(self).immediate_target =
                self.get_target_of_alias_declaration(node, Some(true))?;
        }

        let ret = links.ref_(self).immediate_target.clone();
        Ok(ret)
    }

    pub(super) fn check_object_literal(
        &self,
        node: Id<Node>, /*ObjectLiteralExpression*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let in_destructuring_pattern = is_assignment_target(node, self);
        self.check_grammar_object_literal_expression(node, in_destructuring_pattern);

        let node_ref = node.ref_(self);
        let node_as_object_literal_expression = node_ref.as_object_literal_expression();
        let mut all_properties_table = if self.strict_null_checks {
            Some(create_symbol_table(
                self.arena(),
                Option::<&[Id<Symbol>]>::None,
            ))
        } else {
            None
        };
        let mut properties_table = create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None);
        let mut properties_array: Vec<Id<Symbol>> = vec![];
        let mut spread: Id<Type> = self.empty_object_type();

        let contextual_type = self.get_apparent_type_of_contextual_type(node, None)?;
        let contextual_type_has_pattern = matches!(
            contextual_type,
            Some(contextual_type) if matches!(
                contextual_type.ref_(self).maybe_pattern(),
                Some(contextual_type_pattern) if matches!(
                    contextual_type_pattern.ref_(self).kind(),
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
        let is_in_javascript =
            is_in_js_file(Some(&node.ref_(self))) && !is_in_json_file(Some(&node.ref_(self)));
        let enum_tag = get_jsdoc_enum_tag(node, self);
        let is_js_object_literal =
            contextual_type.is_none() && is_in_javascript && enum_tag.is_none();
        let mut object_flags = self.fresh_object_literal_flag;
        let mut pattern_with_computed_properties = false;
        let mut has_computed_string_property = false;
        let mut has_computed_number_property = false;
        let mut has_computed_symbol_property = false;

        for elem in &*node_as_object_literal_expression.properties.ref_(self) {
            if let Some(elem_name) = elem
                .ref_(self)
                .as_named_declaration()
                .maybe_name()
                .filter(|elem_name| is_computed_property_name(&elem_name.ref_(self)))
            {
                self.check_computed_property_name(elem_name)?;
            }
        }

        let mut offset = 0;
        for &member_decl in &*node_as_object_literal_expression.properties.ref_(self) {
            let mut member = self.get_symbol_of_node(member_decl)?;
            let computed_name_type = member_decl
                .ref_(self)
                .as_named_declaration()
                .maybe_name()
                .filter(|member_decl_name| {
                    member_decl_name.ref_(self).kind() == SyntaxKind::ComputedPropertyName
                })
                .try_map(|member_decl_name| self.check_computed_property_name(member_decl_name))?;
            if matches!(
                member_decl.ref_(self).kind(),
                SyntaxKind::PropertyAssignment | SyntaxKind::ShorthandPropertyAssignment
            ) || is_object_literal_method(member_decl, self)
            {
                let member_present = member.unwrap();
                let mut type_: Id<Type> = if member_decl.ref_(self).kind()
                    == SyntaxKind::PropertyAssignment
                {
                    self.check_property_assignment(member_decl, check_mode)?
                } else if member_decl.ref_(self).kind() == SyntaxKind::ShorthandPropertyAssignment {
                    let member_decl_ref = member_decl.ref_(self);
                    let member_decl_as_shorthand_property_assignment =
                        member_decl_ref.as_shorthand_property_assignment();
                    self.check_expression_for_mutable_location(
                        if !in_destructuring_pattern
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
                        None,
                        None,
                    )?
                } else {
                    self.check_object_literal_method(member_decl, check_mode)?
                };
                if is_in_javascript {
                    let js_doc_type =
                        self.get_type_for_declaration_from_jsdoc_comment(member_decl)?;
                    if let Some(js_doc_type) = js_doc_type {
                        self.check_type_assignable_to(
                            type_,
                            js_doc_type,
                            Some(member_decl),
                            None,
                            None,
                            None,
                        )?;
                        type_ = js_doc_type.clone();
                    } else if let Some(enum_tag) = enum_tag
                    /*&& enumTag.typeExpression*/
                    {
                        self.check_type_assignable_to(
                            type_,
                            self.get_type_from_type_node_(
                                enum_tag
                                    .ref_(self)
                                    .as_base_jsdoc_type_like_tag()
                                    .type_expression
                                    .unwrap(),
                            )?,
                            Some(member_decl),
                            None,
                            None,
                            None,
                        )?;
                    }
                }
                object_flags |= get_object_flags(&type_.ref_(self)) & ObjectFlags::PropagatingFlags;
                let name_type = computed_name_type.filter(|&computed_name_type| {
                    self.is_type_usable_as_property_name(computed_name_type)
                });
                let prop = self.alloc_symbol(if let Some(name_type) = name_type {
                    self.create_symbol(
                        SymbolFlags::Property | member_present.ref_(self).flags(),
                        self.get_property_name_from_type(name_type),
                        Some(check_flags | CheckFlags::Late),
                    )
                    .into()
                } else {
                    self.create_symbol(
                        SymbolFlags::Property | member_present.ref_(self).flags(),
                        member_present.ref_(self).escaped_name().to_owned(),
                        Some(check_flags | CheckFlags::Late),
                    )
                    .into()
                });
                if let Some(name_type) = name_type {
                    prop.ref_(self)
                        .as_transient_symbol()
                        .symbol_links()
                        .ref_mut(self)
                        .name_type = Some(name_type.clone());
                }

                if in_destructuring_pattern {
                    let is_optional = member_decl.ref_(self).kind()
                        == SyntaxKind::PropertyAssignment
                        && self.has_default_value(
                            member_decl
                                .ref_(self)
                                .as_has_initializer()
                                .maybe_initializer()
                                .unwrap(),
                        )
                        || member_decl.ref_(self).kind() == SyntaxKind::ShorthandPropertyAssignment
                            && member_decl
                                .ref_(self)
                                .as_shorthand_property_assignment()
                                .object_assignment_initializer
                                .is_some();
                    if is_optional {
                        prop.ref_(self)
                            .set_flags(prop.ref_(self).flags() | SymbolFlags::Optional);
                    }
                } else if contextual_type_has_pattern
                    && !get_object_flags(&contextual_type.unwrap().ref_(self))
                        .intersects(ObjectFlags::ObjectLiteralPatternWithComputedProperties)
                {
                    let implied_prop = self.get_property_of_type_(
                        contextual_type.unwrap(),
                        member_present.ref_(self).escaped_name(),
                        None,
                    )?;
                    if let Some(implied_prop) = implied_prop {
                        prop.ref_(self).set_flags(
                            prop.ref_(self).flags()
                                | (implied_prop.ref_(self).flags() & SymbolFlags::Optional),
                        );
                    } else if self
                        .compiler_options
                        .ref_(self)
                        .suppress_excess_property_errors
                        != Some(true)
                        && self
                            .get_index_info_of_type_(contextual_type.unwrap(), self.string_type())?
                            .is_none()
                    {
                        self.error(
                            member_decl.ref_(self).as_named_declaration().maybe_name(),
                            &Diagnostics::Object_literal_may_only_specify_known_properties_and_0_does_not_exist_in_type_1,
                            Some(vec![
                                self.symbol_to_string_(
                                    member_present,
                                    Option::<Id<Node>>::None,
                                    None, None, None,
                                )?,
                                self.type_to_string_(
                                    contextual_type.unwrap(),
                                    Option::<Id<Node>>::None,
                                    None, None,
                                )?,
                            ])
                        );
                    }
                }

                if let Some(member_declarations) =
                    member_present.ref_(self).maybe_declarations().clone()
                {
                    prop.ref_(self).set_declarations(member_declarations);
                }
                prop.ref_(self)
                    .set_parent(member_present.ref_(self).maybe_parent());
                if let Some(member_value_declaration) =
                    member_present.ref_(self).maybe_value_declaration()
                {
                    prop.ref_(self)
                        .set_value_declaration(member_value_declaration);
                }

                {
                    let prop_links = prop.ref_(self).as_transient_symbol().symbol_links();
                    let mut prop_links = prop_links.ref_mut(self);
                    prop_links.type_ = Some(type_);
                    prop_links.target = Some(member_present.clone());
                }
                member = Some(prop.clone());
                if let Some(all_properties_table) = all_properties_table.as_mut() {
                    all_properties_table
                        .insert(prop.ref_(self).escaped_name().to_owned(), prop.clone());
                };
            } else if member_decl.ref_(self).kind() == SyntaxKind::SpreadAssignment {
                if self.language_version < ScriptTarget::ES2015 {
                    self.check_external_emit_helpers(member_decl, ExternalEmitHelpers::Assign)?;
                }
                if !properties_array.is_empty() {
                    spread = self.get_spread_type(
                        spread,
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
                        )?,
                        node.ref_(self).maybe_symbol(),
                        object_flags,
                        in_const_context,
                    )?;
                    properties_array = vec![];
                    properties_table =
                        create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None);
                    has_computed_string_property = false;
                    has_computed_number_property = false;
                    has_computed_symbol_property = false;
                }
                let type_ = self.get_reduced_type(self.check_expression(
                    member_decl.ref_(self).as_has_expression().expression(),
                    None,
                    None,
                )?)?;
                if self.is_valid_spread_type(type_)? {
                    let merged_type = self
                        .try_merge_union_of_object_type_and_empty_object(type_, in_const_context)?;
                    if let Some(all_properties_table) = all_properties_table.as_ref() {
                        self.check_spread_prop_overrides(
                            merged_type,
                            all_properties_table,
                            member_decl,
                        )?;
                    }
                    offset = properties_array.len();
                    if self.is_error_type(spread) {
                        continue;
                    }
                    spread = self.get_spread_type(
                        spread,
                        merged_type,
                        node.ref_(self).maybe_symbol(),
                        object_flags,
                        in_const_context,
                    )?;
                } else {
                    self.error(
                        Some(member_decl),
                        &Diagnostics::Spread_types_may_only_be_created_from_object_types,
                        None,
                    );
                    spread = self.error_type();
                }
                continue;
            } else {
                Debug_.assert(
                    matches!(
                        member_decl.ref_(self).kind(),
                        SyntaxKind::GetAccessor | SyntaxKind::SetAccessor
                    ),
                    None,
                );
                self.check_node_deferred(member_decl);
            }
            let member = member.unwrap();

            if let Some(computed_name_type) = computed_name_type.filter(|&computed_name_type| {
                !computed_name_type
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::StringOrNumberLiteralOrUnique)
            }) {
                if self
                    .is_type_assignable_to(computed_name_type, self.string_number_symbol_type())?
                {
                    if self.is_type_assignable_to(computed_name_type, self.number_type())? {
                        has_computed_number_property = true;
                    } else if self
                        .is_type_assignable_to(computed_name_type, self.es_symbol_type())?
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
                properties_table
                    .insert(member.ref_(self).escaped_name().to_owned(), member.clone());
            }
            properties_array.push(member);
        }

        if contextual_type_has_pattern
            && node.ref_(self).parent().ref_(self).kind() != SyntaxKind::SpreadAssignment
        {
            for prop in self.get_properties_of_type(contextual_type.unwrap())? {
                if !properties_table.contains_key(prop.ref_(self).escaped_name())
                    && self
                        .get_property_of_type_(spread, prop.ref_(self).escaped_name(), None)?
                        .is_none()
                {
                    if !prop.ref_(self).flags().intersects(SymbolFlags::Optional) {
                        self.error(
                            prop.ref_(self).maybe_value_declaration().or_else(|| {
                                prop.ref_(self).as_transient_symbol().symbol_links().ref_(self).binding_element
                            }),
                            &Diagnostics::Initializer_provides_no_value_for_this_binding_element_and_the_binding_element_has_no_default_value,
                            None,
                        );
                    }
                    properties_table
                        .insert(prop.ref_(self).escaped_name().to_owned(), prop.clone());
                    properties_array.push(prop.clone());
                }
            }
        }

        if self.is_error_type(spread) {
            return Ok(self.error_type());
        }

        if spread != self.empty_object_type() {
            if !properties_array.is_empty() {
                spread = self.get_spread_type(
                    spread,
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
                    )?,
                    node.ref_(self).maybe_symbol(),
                    object_flags,
                    in_const_context,
                )?;
                properties_array = vec![];
                properties_table = create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None);
                has_computed_string_property = false;
                has_computed_number_property = false;
            }
            return Ok(self
                .try_map_type(
                    spread,
                    &mut |t: Id<Type>| -> io::Result<_> {
                        Ok(if t == self.empty_object_type() {
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
                            Some(t)
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
