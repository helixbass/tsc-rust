use std::{collections::HashMap, io, ptr};

use id_arena::Id;
use itertools::Either;

use super::signature_has_rest_parameter;
use crate::{
    declaration_name_to_string, find_index, first_defined, get_declaration_of_kind,
    get_effective_constraint_of_type_parameter, get_effective_return_type_node,
    get_immediately_invoked_function_expression, get_jsdoc_parameter_tags, get_jsdoc_tags,
    get_jsdoc_type, get_jsdoc_type_tag, get_name_of_declaration, has_effective_modifier,
    has_jsdoc_parameter_tags, has_rest_parameter, has_syntactic_modifier, is_binding_pattern,
    is_constructor_declaration, is_constructor_type_node, is_function_like,
    is_function_like_declaration, is_in_js_file, is_jsdoc_construct_signature,
    is_jsdoc_parameter_tag, is_jsdoc_signature, is_jsdoc_variadic_type, is_part_of_type_node,
    is_rest_parameter, is_type_parameter_declaration, is_type_predicate_node,
    is_value_signature_declaration, last_or_undefined, length, map_defined, maybe_filter,
    node_is_missing, node_starts_new_lexical_environment, return_ok_default_if_none,
    return_ok_false_if_none, try_for_each_child_bool, try_map, try_maybe_map, try_some, CheckFlags,
    Debug_, Diagnostics, HasArena, HasInitializerInterface, HasTypeInterface, InArena, IndexInfo,
    InterfaceTypeInterface, InternalSymbolName, ModifierFlags, Node, NodeArray, NodeCheckFlags,
    NodeInterface, ObjectFlags, OptionInArena, OptionTry, ReadonlyTextRange, Signature,
    SignatureDeclarationInterface, SignatureFlags, Symbol, SymbolFlags, SymbolInterface,
    SymbolTable, SyntaxKind, TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface,
    TypeMapper, TypePredicate, TypePredicateKind, TypeSystemPropertyName, UnionReduction,
};

impl TypeChecker {
    pub(super) fn fill_missing_type_arguments(
        &self,
        type_arguments: Option<Vec<Id<Type>>>,
        type_parameters: Option<&[Id<Type /*TypeParameter*/>]>,
        min_type_argument_count: usize,
        is_java_script_implicit_any: bool,
    ) -> io::Result<Option<Vec<Id<Type>>>> {
        let num_type_parameters = length(type_parameters);
        if num_type_parameters == 0 {
            return Ok(Some(vec![]));
        }
        let type_parameters = type_parameters.unwrap();
        let num_type_arguments = length(type_arguments.as_deref());
        if is_java_script_implicit_any
            || num_type_arguments >= min_type_argument_count
                && num_type_arguments <= num_type_parameters
        {
            let mut result = type_arguments
                .as_ref()
                .map_or_else(|| vec![], |type_arguments| type_arguments.clone());
            for _i in num_type_arguments..num_type_parameters {
                result.push(self.error_type());
            }
            let base_default_type =
                self.get_default_type_argument_type(is_java_script_implicit_any);
            for i in num_type_arguments..num_type_parameters {
                let mut default_type = self.get_default_from_type_parameter_(type_parameters[i])?;
                if is_java_script_implicit_any
                    && matches!(
                        default_type,
                        Some(default_type) if self.is_type_identical_to(default_type, self.unknown_type())?
                            || self.is_type_identical_to(default_type, self.empty_object_type())?
                    )
                {
                    default_type = Some(self.any_type());
                }
                result[i] =
                    if let Some(default_type) = default_type {
                        self.instantiate_type(
                            default_type,
                            Some(self.create_type_mapper(
                                type_parameters.to_owned(),
                                Some(result.clone()),
                            )),
                        )?
                    } else {
                        base_default_type.clone()
                    };
            }
            result.truncate(type_parameters.len());
            return Ok(Some(result));
        }
        Ok(type_arguments.map(|type_arguments| type_arguments.clone()))
    }

    pub(super) fn get_signature_from_declaration_(
        &self,
        declaration: Id<Node>, /*SignatureDeclaration | JSDocSignature*/
    ) -> io::Result<Id<Signature>> {
        let links = self.get_node_links(declaration);
        if links.ref_(self).resolved_signature.is_none() {
            let mut parameters: Vec<Id<Symbol>> = vec![];
            let mut flags = SignatureFlags::None;
            let mut min_argument_count = 0;
            let mut this_parameter: Option<Id<Symbol>> = None;
            let mut has_this_parameter = false;
            let iife = get_immediately_invoked_function_expression(declaration, self);
            let is_js_construct_signature = is_jsdoc_construct_signature(declaration, self);
            let is_untyped_signature_in_js_file = iife.is_none()
                && is_in_js_file(Some(&declaration.ref_(self)))
                && is_value_signature_declaration(&declaration.ref_(self))
                && !has_jsdoc_parameter_tags(declaration, self)
                && get_jsdoc_type(declaration, self).is_none();
            if is_untyped_signature_in_js_file {
                flags |= SignatureFlags::IsUntypedSignatureInJSFile;
            }

            let declaration_ref = declaration.ref_(self);
            let declaration_as_signature_declaration = declaration_ref.as_signature_declaration();
            for (i, &param) in declaration_as_signature_declaration
                .parameters()
                .ref_(self)
                .iter()
                .enumerate()
                .skip(if is_js_construct_signature { 1 } else { 0 })
            {
                let mut param_symbol = param.ref_(self).maybe_symbol();
                let type_ = if is_jsdoc_parameter_tag(&param.ref_(self)) {
                    param
                        .ref_(self)
                        .as_jsdoc_property_like_tag()
                        .type_expression
                        .map(|type_expression| {
                            type_expression.ref_(self).as_jsdoc_type_expression().type_
                        })
                } else {
                    param.ref_(self).as_has_type().maybe_type()
                };
                if matches!(
                    param_symbol,
                    Some(param_symbol) if param_symbol.ref_(self).flags().intersects(SymbolFlags::Property)
                ) && !is_binding_pattern(
                    param
                        .ref_(self)
                        .as_named_declaration()
                        .maybe_name()
                        .refed(self)
                        .as_deref(),
                ) {
                    let resolved_symbol = self.resolve_name_(
                        Some(param),
                        param_symbol.unwrap().ref_(self).escaped_name(),
                        SymbolFlags::Value,
                        None,
                        Option::<Id<Node>>::None,
                        false,
                        None,
                    )?;
                    param_symbol = resolved_symbol;
                }
                let param_symbol = param_symbol.unwrap();
                if i == 0 && param_symbol.ref_(self).escaped_name() == InternalSymbolName::This {
                    has_this_parameter = true;
                    this_parameter = param.ref_(self).maybe_symbol();
                } else {
                    parameters.push(param_symbol);
                }

                if matches!(
                    type_,
                    Some(type_) if type_.ref_(self).kind() == SyntaxKind::LiteralType
                ) {
                    flags |= SignatureFlags::HasLiteralTypes;
                }

                let is_optional_parameter = self.is_optional_jsdoc_property_like_tag(param)
                    || {
                        let param_ref = param.ref_(self);
                        let param_as_parameter_declaration = param_ref.as_parameter_declaration();
                        param_as_parameter_declaration.maybe_initializer().is_some()
                            || param_as_parameter_declaration.question_token.is_some()
                            || is_rest_parameter(param, self)
                            || matches!(
                                iife,
                                Some(iife) if parameters.len() > iife.ref_(self).as_call_expression().arguments.ref_(self).len()
                            ) && type_.is_none()
                            || self.is_jsdoc_optional_parameter(param)
                    };
                if !is_optional_parameter {
                    min_argument_count = parameters.len();
                }
            }

            if matches!(
                declaration.ref_(self).kind(),
                SyntaxKind::GetAccessor | SyntaxKind::SetAccessor
            ) && self.has_bindable_name(declaration)?
                && (!has_this_parameter || this_parameter.is_none())
            {
                let other_kind = if declaration.ref_(self).kind() == SyntaxKind::GetAccessor {
                    SyntaxKind::SetAccessor
                } else {
                    SyntaxKind::GetAccessor
                };
                let other = get_declaration_of_kind(
                    self.get_symbol_of_node(declaration)?.unwrap(),
                    other_kind,
                    self,
                );
                if let Some(other) = other {
                    this_parameter = self.get_annotated_accessor_this_parameter(other);
                }
            }

            let class_type: Option<Id<Type>> =
                if declaration.ref_(self).kind() == SyntaxKind::Constructor {
                    Some(
                        self.get_declared_type_of_class_or_interface(
                            self.get_merged_symbol(
                                declaration.ref_(self).parent().ref_(self).maybe_symbol(),
                            )
                            .unwrap(),
                        )?,
                    )
                } else {
                    None
                };
            let type_parameters = match class_type {
                Some(class_type) => class_type
                    .ref_(self)
                    .as_interface_type()
                    .maybe_local_type_parameters()
                    .map(ToOwned::to_owned),
                None => self.get_type_parameters_from_declaration(declaration),
            };
            if has_rest_parameter(declaration, self)
                || is_in_js_file(Some(&declaration.ref_(self)))
                    && self.maybe_add_js_synthetic_rest_parameter(declaration, &mut parameters)?
            {
                flags |= SignatureFlags::HasRestParameter;
            }
            if is_constructor_type_node(&declaration.ref_(self))
                && has_syntactic_modifier(declaration, ModifierFlags::Abstract, self)
                || is_constructor_declaration(&declaration.ref_(self))
                    && has_syntactic_modifier(
                        declaration.ref_(self).parent(),
                        ModifierFlags::Abstract,
                        self,
                    )
            {
                flags |= SignatureFlags::Abstract;
            }
            let resolved_signature = self.alloc_signature(self.create_signature(
                Some(declaration),
                type_parameters,
                this_parameter,
                parameters,
                None,
                None,
                min_argument_count,
                flags,
            ));
            links.ref_mut(self).resolved_signature = Some(resolved_signature);
        }
        let links = links.ref_(self);
        Ok(links.resolved_signature.clone().unwrap())
    }

    pub(super) fn maybe_add_js_synthetic_rest_parameter(
        &self,
        declaration: Id<Node>, /*SignatureDeclaration | JSDocSignature*/
        parameters: &mut Vec<Id<Symbol>>,
    ) -> io::Result<bool> {
        if is_jsdoc_signature(&declaration.ref_(self))
            || !self.contains_arguments_reference(declaration)?
        {
            return Ok(false);
        }
        let last_param = last_or_undefined(
            &declaration
                .ref_(self)
                .as_signature_declaration()
                .parameters()
                .ref_(self),
        )
        .copied();
        let last_param_tags = if let Some(last_param) = last_param {
            get_jsdoc_parameter_tags(last_param, self)
        } else {
            get_jsdoc_tags(declaration, self)
                .ref_(self)
                .iter()
                .copied()
                .filter(|tag: &Id<Node>| is_jsdoc_parameter_tag(&tag.ref_(self)))
                .collect()
        };
        let last_param_variadic_type = first_defined(last_param_tags, |p: Id<Node>, _| {
            p.ref_(self)
                .as_jsdoc_property_like_tag()
                .type_expression
                .filter(|type_expression| {
                    is_jsdoc_variadic_type(
                        &type_expression
                            .ref_(self)
                            .as_jsdoc_type_expression()
                            .type_
                            .ref_(self),
                    )
                })
                .map(|type_expression| type_expression.ref_(self).as_jsdoc_type_expression().type_)
        });
        let synthetic_args_symbol = self.alloc_symbol(
            self.create_symbol(
                SymbolFlags::Variable,
                "args".to_owned(),
                Some(CheckFlags::RestParameter),
            )
            .into(),
        );
        synthetic_args_symbol
            .ref_(self)
            .as_transient_symbol()
            .symbol_links()
            .ref_mut(self)
            .type_ = Some(
            if let Some(last_param_variadic_type) = last_param_variadic_type.as_ref() {
                self.create_array_type(
                    self.get_type_from_type_node_(
                        last_param_variadic_type
                            .ref_(self)
                            .as_base_jsdoc_unary_type()
                            .type_
                            .unwrap(),
                    )?,
                    None,
                )
            } else {
                self.any_array_type()
            },
        );
        if last_param_variadic_type.is_some() {
            parameters.pop();
        };
        parameters.push(synthetic_args_symbol);
        Ok(true)
    }

    pub(super) fn get_signature_of_type_tag(
        &self,
        node: Id<Node>, /*SignatureDeclaration | JSDocSignature*/
    ) -> io::Result<Option<Id<Signature>>> {
        if !(is_in_js_file(Some(&node.ref_(self)))
            && is_function_like_declaration(&node.ref_(self)))
        {
            return Ok(None);
        }
        let type_tag = get_jsdoc_type_tag(node, self);
        type_tag
            .and_then(|type_tag| {
                type_tag
                    .ref_(self)
                    .as_base_jsdoc_type_like_tag()
                    .type_expression
            })
            .try_and_then(|type_expression| {
                self.get_single_call_signature(self.get_type_from_type_node_(type_expression)?)
            })
    }

    pub(super) fn get_return_type_of_type_tag(
        &self,
        node: Id<Node>, /*SignatureDeclaration | JSDocSignature*/
    ) -> io::Result<Option<Id<Type>>> {
        let signature = self.get_signature_of_type_tag(node)?;
        signature.try_map(|signature| self.get_return_type_of_signature(signature))
    }

    pub(super) fn contains_arguments_reference(
        &self,
        declaration: Id<Node>, /*SignatureDeclaration*/
    ) -> io::Result<bool> {
        let links = self.get_node_links(declaration);
        if links.ref_(self).contains_arguments_reference.is_none() {
            let contains_arguments_reference = if links
                .ref_(self)
                .flags
                .intersects(NodeCheckFlags::CaptureArguments)
            {
                true
            } else {
                self.contains_arguments_reference_traverse(
                    declaration
                        .ref_(self)
                        .maybe_as_function_like_declaration()
                        .and_then(|declaration| declaration.maybe_body()),
                )?
            };
            links.ref_mut(self).contains_arguments_reference = Some(contains_arguments_reference);
        }
        let ret = links.ref_(self).contains_arguments_reference.unwrap();
        Ok(ret)
    }

    pub(super) fn contains_arguments_reference_traverse(
        &self,
        node: Option<Id<Node>>,
    ) -> io::Result<bool> {
        let node = return_ok_false_if_none!(node);
        Ok(match node.ref_(self).kind() {
            SyntaxKind::Identifier => {
                &node.ref_(self).as_identifier().escaped_text
                    == self.arguments_symbol().ref_(self).escaped_name()
                    && matches!(
                        self.get_referenced_value_symbol(node, None)?,
                        Some(symbol) if symbol == self.arguments_symbol()
                    )
            }

            SyntaxKind::PropertyDeclaration
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor => {
                let name = node.ref_(self).as_named_declaration().name();
                name.ref_(self).kind() == SyntaxKind::ComputedPropertyName
                    && self.contains_arguments_reference_traverse(Some(name))?
            }

            SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => self
                .contains_arguments_reference_traverse(Some(
                    node.ref_(self).as_has_expression().expression(),
                ))?,

            _ => {
                !node_starts_new_lexical_environment(&node.ref_(self))
                    && !is_part_of_type_node(node, self)
                    && try_for_each_child_bool(
                        node,
                        |child| self.contains_arguments_reference_traverse(Some(child)),
                        Option::<fn(Id<NodeArray>) -> io::Result<bool>>::None,
                        self,
                    )?
            }
        })
    }

    pub(super) fn get_signatures_of_symbol(
        &self,
        symbol: Option<Id<Symbol>>,
    ) -> io::Result<Vec<Id<Signature>>> {
        let symbol = return_ok_default_if_none!(symbol);
        let symbol_ref = symbol.ref_(self);
        let symbol_declarations = symbol_ref.maybe_declarations();
        if symbol_declarations.is_none() {
            return Ok(vec![]);
        }
        let symbol_declarations = symbol_declarations.as_ref().unwrap();
        let mut result: Vec<Id<Signature>> = vec![];
        for (i, &decl) in symbol_declarations.iter().enumerate() {
            if !is_function_like(Some(&decl.ref_(self))) {
                continue;
            }
            if i > 0
                && decl
                    .ref_(self)
                    .maybe_as_function_like_declaration()
                    .and_then(|decl| decl.maybe_body())
                    .is_some()
            {
                let previous = &symbol_declarations[i - 1];
                if decl.ref_(self).parent() == previous.ref_(self).parent()
                    && decl.ref_(self).kind() == previous.ref_(self).kind()
                    && decl.ref_(self).pos() == previous.ref_(self).end()
                {
                    continue;
                }
            }
            result.push(self.get_signature_from_declaration_(decl)?);
        }
        Ok(result)
    }

    pub(super) fn resolve_external_module_type_by_literal(
        &self,
        name: Id<Node>, /*StringLiteral*/
    ) -> io::Result<Id<Type>> {
        let module_sym = self.resolve_external_module_name_(name, name, None)?;
        if let Some(module_sym) = module_sym {
            let resolved_module_symbol =
                self.resolve_external_module_symbol(Some(module_sym), None)?;
            if let Some(resolved_module_symbol) = resolved_module_symbol {
                return self.get_type_of_symbol(resolved_module_symbol);
            }
        }

        Ok(self.any_type())
    }

    pub(super) fn get_this_type_of_signature(
        &self,
        signature: Id<Signature>,
    ) -> io::Result<Option<Id<Type>>> {
        signature
            .ref_(self)
            .maybe_this_parameter()
            .try_map(|this_parameter| self.get_type_of_symbol(this_parameter))
    }

    pub(super) fn get_type_predicate_of_signature(
        &self,
        signature: Id<Signature>,
    ) -> io::Result<Option<Id<TypePredicate>>> {
        if signature
            .ref_(self)
            .maybe_resolved_type_predicate()
            .is_none()
        {
            if let Some(signature_target) = signature.ref_(self).target {
                let target_type_predicate =
                    self.get_type_predicate_of_signature(signature_target)?;
                signature.ref_(self).set_resolved_type_predicate(Some(
                    if let Some(target_type_predicate) = target_type_predicate {
                        self.alloc_type_predicate(self.instantiate_type_predicate(
                            target_type_predicate,
                            signature.ref_(self).mapper.clone().unwrap(),
                        )?)
                    } else {
                        self.no_type_predicate()
                    },
                ));
            } else if let Some(signature_composite_signatures) =
                signature.ref_(self).composite_signatures.as_ref()
            {
                signature.ref_(self).set_resolved_type_predicate(Some(
                    self.get_union_or_intersection_type_predicate(
                        signature_composite_signatures,
                        signature.ref_(self).composite_kind,
                    )?
                    .map_or_else(
                        || self.no_type_predicate(),
                        |type_predicate| self.alloc_type_predicate(type_predicate),
                    ),
                ));
            } else {
                let type_ = signature
                    .ref_(self)
                    .declaration
                    .and_then(|declaration| get_effective_return_type_node(declaration, self));
                let mut jsdoc_predicate: Option<Id<TypePredicate>> = None;
                if type_.is_none()
                    && is_in_js_file(signature.ref_(self).declaration.refed(self).as_deref())
                {
                    let jsdoc_signature =
                        self.get_signature_of_type_tag(signature.ref_(self).declaration.unwrap())?;
                    if let Some(jsdoc_signature) =
                        jsdoc_signature.filter(|&jsdoc_signature| signature != jsdoc_signature)
                    {
                        jsdoc_predicate = self.get_type_predicate_of_signature(jsdoc_signature)?;
                    }
                }
                signature.ref_(self).set_resolved_type_predicate(Some(
                    if let Some(type_) =
                        type_.filter(|type_| is_type_predicate_node(&type_.ref_(self)))
                    {
                        self.alloc_type_predicate(
                            self.create_type_predicate_from_type_predicate_node(type_, signature)?,
                        )
                    } else {
                        jsdoc_predicate.unwrap_or_else(|| self.no_type_predicate())
                    },
                ));
            }
            Debug_.assert(
                signature
                    .ref_(self)
                    .maybe_resolved_type_predicate()
                    .is_some(),
                None,
            );
        }
        Ok(
            if matches!(
                signature.ref_(self).maybe_resolved_type_predicate(),
                Some(resolved_type_predicate) if resolved_type_predicate == self.no_type_predicate()
            ) {
                None
            } else {
                signature.ref_(self).maybe_resolved_type_predicate().clone()
            },
        )
    }

    pub(super) fn create_type_predicate_from_type_predicate_node(
        &self,
        node: Id<Node>, /*TypePredicateNode*/
        signature: Id<Signature>,
    ) -> io::Result<TypePredicate> {
        let node_ref = node.ref_(self);
        let node_as_type_predicate_node = node_ref.as_type_predicate_node();
        let parameter_name = &node_as_type_predicate_node.parameter_name;
        let type_ = node_as_type_predicate_node
            .type_
            .try_map(|type_| self.get_type_from_type_node_(type_))?;
        Ok(
            if parameter_name.ref_(self).kind() == SyntaxKind::ThisType {
                self.create_type_predicate(
                    if node_as_type_predicate_node.asserts_modifier.is_some() {
                        TypePredicateKind::AssertsThis
                    } else {
                        TypePredicateKind::This
                    },
                    None,
                    None,
                    type_,
                )
            } else {
                let parameter_name_ref = parameter_name.ref_(self);
                let parameter_name_as_identifier = parameter_name_ref.as_identifier();
                self.create_type_predicate(
                    if node_as_type_predicate_node.asserts_modifier.is_some() {
                        TypePredicateKind::AssertsIdentifier
                    } else {
                        TypePredicateKind::Identifier
                    },
                    Some(parameter_name_as_identifier.escaped_text.to_owned()),
                    find_index(
                        signature.ref_(self).parameters(),
                        |&p: &Id<Symbol>, _| {
                            p.ref_(self).escaped_name()
                                == &parameter_name_as_identifier.escaped_text
                        },
                        None,
                    ),
                    type_,
                )
            },
        )
    }

    pub(super) fn get_union_or_intersection_type(
        &self,
        types: &[Id<Type>],
        kind: Option<TypeFlags>,
        union_reduction: Option<UnionReduction>,
    ) -> io::Result<Id<Type>> {
        Ok(if !matches!(kind, Some(TypeFlags::Intersection)) {
            self.get_union_type(
                types,
                union_reduction,
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?
        } else {
            self.get_intersection_type(types, Option::<Id<Symbol>>::None, None)?
        })
    }

    pub(super) fn get_return_type_of_signature(
        &self,
        signature: Id<Signature>,
    ) -> io::Result<Id<Type>> {
        if signature.ref_(self).maybe_resolved_return_type().is_none() {
            if !self.push_type_resolution(
                &signature.clone().into(),
                TypeSystemPropertyName::ResolvedReturnType,
            ) {
                return Ok(self.error_type());
            }
            let mut type_: Id<Type> = if let Some(signature_target) = signature.ref_(self).target {
                self.instantiate_type(
                    self.get_return_type_of_signature(signature_target.clone())?,
                    signature.ref_(self).mapper.clone(),
                )?
            } else if let Some(signature_composite_signatures) =
                signature.ref_(self).composite_signatures.as_deref()
            {
                self.instantiate_type(
                    self.get_union_or_intersection_type(
                        &try_map(
                            signature_composite_signatures,
                            |signature: &Id<Signature>, _| {
                                self.get_return_type_of_signature(signature.clone())
                            },
                        )?,
                        signature.ref_(self).composite_kind,
                        Some(UnionReduction::Subtype),
                    )?,
                    signature.ref_(self).mapper.clone(),
                )?
            } else {
                let signature_declaration = signature.ref_(self).declaration.unwrap();
                self.get_return_type_from_annotation(signature_declaration)?
                    .try_unwrap_or_else(|| -> io::Result<_> {
                        Ok(
                            if node_is_missing(
                                signature_declaration
                                    .ref_(self)
                                    .maybe_as_function_like_declaration()
                                    .and_then(|function_like_declaration| {
                                        function_like_declaration.maybe_body()
                                    })
                                    .refed(self)
                                    .as_deref(),
                            ) {
                                self.any_type()
                            } else {
                                self.get_return_type_from_body(signature_declaration, None)?
                            },
                        )
                    })?
            };
            if signature
                .ref_(self)
                .flags
                .intersects(SignatureFlags::IsInnerCallChain)
            {
                type_ = self.add_optional_type_marker(type_)?;
            } else if signature
                .ref_(self)
                .flags
                .intersects(SignatureFlags::IsOuterCallChain)
            {
                type_ = self.get_optional_type_(type_, None)?;
            }
            if !self.pop_type_resolution() {
                if let Some(signature_declaration) = signature.ref_(self).declaration {
                    let type_node = get_effective_return_type_node(signature_declaration, self);
                    if let Some(type_node) = type_node {
                        self.error(
                            Some(type_node),
                            &Diagnostics::Return_type_annotation_circularly_references_itself,
                            None,
                        );
                    } else if self.no_implicit_any {
                        let declaration = signature_declaration;
                        let name = get_name_of_declaration(Some(declaration), self);
                        if let Some(name) = name {
                            self.error(
                                Some(name.clone()),
                                &Diagnostics::_0_implicitly_has_return_type_any_because_it_does_not_have_a_return_type_annotation_and_is_referenced_directly_or_indirectly_in_one_of_its_return_expressions,
                                Some(vec![
                                    declaration_name_to_string(Some(name), self).into_owned()
                                ])
                            );
                        } else {
                            self.error(
                                Some(declaration),
                                &Diagnostics::Function_implicitly_has_return_type_any_because_it_does_not_have_a_return_type_annotation_and_is_referenced_directly_or_indirectly_in_one_of_its_return_expressions,
                                None,
                            );
                        }
                    }
                }
                type_ = self.any_type();
            }
            signature.ref_(self).set_resolved_return_type(Some(type_));
        }
        Ok(signature
            .ref_(self)
            .maybe_resolved_return_type()
            .clone()
            .unwrap())
    }

    pub(super) fn get_return_type_from_annotation(
        &self,
        declaration: Id<Node>, /*SignatureDeclaration | JSDocSignature*/
    ) -> io::Result<Option<Id<Type>>> {
        if declaration.ref_(self).kind() == SyntaxKind::Constructor {
            return Ok(Some(
                self.get_declared_type_of_class_or_interface(
                    self.get_merged_symbol(
                        declaration.ref_(self).parent().ref_(self).maybe_symbol(),
                    )
                    .unwrap(),
                )?,
            ));
        }
        if is_jsdoc_construct_signature(declaration, self) {
            return Ok(Some(
                self.get_type_from_type_node_(
                    declaration
                        .ref_(self)
                        .as_signature_declaration()
                        .parameters()
                        .ref_(self)[0]
                        .ref_(self)
                        .as_parameter_declaration()
                        .maybe_type()
                        .unwrap(),
                )?,
            ));
        }
        let type_node = get_effective_return_type_node(declaration, self);
        if let Some(type_node) = type_node {
            return Ok(Some(self.get_type_from_type_node_(type_node)?));
        }
        if declaration.ref_(self).kind() == SyntaxKind::GetAccessor
            && self.has_bindable_name(declaration)?
        {
            let js_doc_type = if is_in_js_file(Some(&declaration.ref_(self))) {
                self.get_type_for_declaration_from_jsdoc_comment(declaration)?
            } else {
                None
            };
            if js_doc_type.is_some() {
                return Ok(js_doc_type);
            }
            let setter = get_declaration_of_kind(
                self.get_symbol_of_node(declaration)?.unwrap(),
                SyntaxKind::SetAccessor,
                self,
            );
            let setter_type = self.get_annotated_accessor_type(setter)?;
            if setter_type.is_some() {
                return Ok(setter_type);
            }
        }
        self.get_return_type_of_type_tag(declaration)
    }

    pub(super) fn is_resolving_return_type_of_signature(&self, signature: Id<Signature>) -> bool {
        signature.ref_(self).maybe_resolved_return_type().is_none()
            && self.find_resolution_cycle_start_index(
                &signature.clone().into(),
                TypeSystemPropertyName::ResolvedReturnType,
            ) >= 0
    }

    #[allow(dead_code)]
    pub(super) fn get_rest_type_of_signature(
        &self,
        signature: Id<Signature>,
    ) -> io::Result<Id<Type>> {
        Ok(self
            .try_get_rest_type_of_signature(signature)?
            .unwrap_or_else(|| self.any_type()))
    }

    pub(super) fn try_get_rest_type_of_signature(
        &self,
        signature: Id<Signature>,
    ) -> io::Result<Option<Id<Type>>> {
        if signature_has_rest_parameter(&signature.ref_(self)) {
            let signature_ref = signature.ref_(self);
            let signature_parameters = signature_ref.parameters();
            let sig_rest_type =
                self.get_type_of_symbol(signature_parameters[signature_parameters.len() - 1])?;
            let rest_type = if self.is_tuple_type(sig_rest_type) {
                self.get_rest_type_of_tuple_type(sig_rest_type)?
            } else {
                Some(sig_rest_type)
            };
            return rest_type.try_and_then(|rest_type| {
                self.get_index_type_of_type_(rest_type, self.number_type())
            });
        }
        Ok(None)
    }

    pub(super) fn get_signature_instantiation(
        &self,
        signature: Id<Signature>,
        type_arguments: Option<&[Id<Type>]>,
        is_javascript: bool,
        inferred_type_parameters: Option<&[Id<Type /*TypeParameter*/>]>,
    ) -> io::Result<Id<Signature>> {
        let instantiated_signature = self
            .get_signature_instantiation_without_filling_in_type_arguments(
                signature.clone(),
                self.fill_missing_type_arguments(
                    type_arguments.map(ToOwned::to_owned),
                    signature.ref_(self).maybe_type_parameters().as_deref(),
                    self.get_min_type_argument_count(
                        signature.ref_(self).maybe_type_parameters().as_deref(),
                    ),
                    is_javascript,
                )?
                .as_deref(),
            )?;
        if let Some(inferred_type_parameters) = inferred_type_parameters {
            let return_signature = self.get_single_call_or_construct_signature(
                self.get_return_type_of_signature(instantiated_signature.clone())?,
            )?;
            if let Some(return_signature) = return_signature {
                let new_return_signature = self.clone_signature(return_signature);
                *new_return_signature.maybe_type_parameters_mut() =
                    Some(inferred_type_parameters.to_owned());
                let new_instantiated_signature = self.clone_signature(instantiated_signature);
                new_instantiated_signature.set_resolved_return_type(Some(
                    self.get_or_create_type_from_signature(
                        self.alloc_signature(new_return_signature),
                    ),
                ));
                return Ok(self.alloc_signature(new_instantiated_signature));
            }
        }
        Ok(instantiated_signature)
    }

    pub(super) fn get_signature_instantiation_without_filling_in_type_arguments(
        &self,
        signature: Id<Signature>,
        type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Signature>> {
        if signature.ref_(self).maybe_instantiations().is_none() {
            *signature.ref_(self).maybe_instantiations() = Some(HashMap::new());
        }
        let signature_ref = signature.ref_(self);
        let mut instantiations = signature_ref.maybe_instantiations();
        let instantiations = instantiations.as_mut().unwrap();
        let id = self.get_type_list_id(type_arguments);
        let mut instantiation = instantiations.get(&id).map(Clone::clone);
        if instantiation.is_none() {
            instantiation = Some(self.alloc_signature(
                self.create_signature_instantiation(signature.clone(), type_arguments)?,
            ));
            instantiations.insert(id, instantiation.clone().unwrap());
        }
        Ok(instantiation.unwrap())
    }

    pub(super) fn create_signature_instantiation(
        &self,
        signature: Id<Signature>,
        type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Signature> {
        self.instantiate_signature(
            signature.clone(),
            self.create_signature_type_mapper(signature, type_arguments),
            Some(true),
        )
    }

    pub(super) fn create_signature_type_mapper(
        &self,
        signature: Id<Signature>,
        type_arguments: Option<&[Id<Type>]>,
    ) -> Id<TypeMapper> {
        self.create_type_mapper(
            signature
                .ref_(self)
                .maybe_type_parameters()
                .clone()
                .unwrap(),
            type_arguments.map(ToOwned::to_owned),
        )
    }

    pub(super) fn get_erased_signature(
        &self,
        signature: Id<Signature>,
    ) -> io::Result<Id<Signature>> {
        Ok(if signature.ref_(self).maybe_type_parameters().is_some() {
            if signature
                .ref_(self)
                .maybe_erased_signature_cache()
                .is_none()
            {
                signature.ref_(self).set_erased_signature_cache(Some(
                    self.alloc_signature(self.create_erased_signature(signature.clone())?),
                ));
            }
            signature.ref_(self).maybe_erased_signature_cache().unwrap()
        } else {
            signature
        })
    }

    pub(super) fn create_erased_signature(
        &self,
        signature: Id<Signature>,
    ) -> io::Result<Signature> {
        self.instantiate_signature(
            signature.clone(),
            self.create_type_eraser(
                signature
                    .ref_(self)
                    .maybe_type_parameters()
                    .clone()
                    .unwrap(),
            ),
            Some(true),
        )
    }

    pub(super) fn get_canonical_signature(
        &self,
        signature: Id<Signature>,
    ) -> io::Result<Id<Signature>> {
        Ok(if signature.ref_(self).maybe_type_parameters().is_some() {
            if signature
                .ref_(self)
                .maybe_canonical_signature_cache()
                .is_none()
            {
                signature.ref_(self).set_canonical_signature_cache(Some(
                    self.create_canonical_signature(signature.clone())?,
                ));
            }
            signature
                .ref_(self)
                .maybe_canonical_signature_cache()
                .unwrap()
        } else {
            signature
        })
    }

    pub(super) fn create_canonical_signature(
        &self,
        signature: Id<Signature>,
    ) -> io::Result<Id<Signature>> {
        self.get_signature_instantiation(
            signature.clone(),
            try_maybe_map(
                signature.ref_(self).maybe_type_parameters().as_deref(),
                |&tp: &Id<Type>, _| -> io::Result<_> {
                    Ok(tp
                        .ref_(self)
                        .as_type_parameter()
                        .target
                        .try_filter(|&target| -> io::Result<_> {
                            Ok(self.get_constraint_of_type_parameter(target)?.is_none())
                        })?
                        .unwrap_or_else(|| tp.clone()))
                },
            )
            .transpose()?
            .as_deref(),
            is_in_js_file(signature.ref_(self).declaration.refed(self).as_deref()),
            None,
        )
    }

    pub(super) fn get_base_signature(&self, signature: Id<Signature>) -> io::Result<Id<Signature>> {
        let type_parameters = signature.ref_(self).maybe_type_parameters().clone();
        if let Some(ref type_parameters) = type_parameters {
            if let Some(signature_base_signature_cache) =
                signature.ref_(self).maybe_base_signature_cache()
            {
                return Ok(signature_base_signature_cache);
            }
            let type_eraser = self.create_type_eraser(type_parameters.clone());
            let base_constraint_mapper = self.create_type_mapper(
                type_parameters.clone(),
                Some(try_map(
                    type_parameters,
                    |&tp: &Id<Type>, _| -> io::Result<_> {
                        Ok(self
                            .get_constraint_of_type_parameter(tp)?
                            .unwrap_or_else(|| self.unknown_type()))
                    },
                )?),
            );
            let mut base_constraints: Vec<Id<Type>> =
                try_map(type_parameters, |&tp: &Id<Type>, _| -> io::Result<_> {
                    Ok(self
                        .maybe_instantiate_type(Some(tp), Some(base_constraint_mapper.clone()))?
                        .unwrap_or_else(|| self.unknown_type()))
                })?;
            for _i in 0..type_parameters.len() - 1 {
                base_constraints = self
                    .instantiate_types(
                        Some(&base_constraints),
                        Some(base_constraint_mapper.clone()),
                    )?
                    .unwrap();
            }
            base_constraints = self
                .instantiate_types(Some(&base_constraints), Some(type_eraser))?
                .unwrap();
            let ret = self.alloc_signature(self.instantiate_signature(
                signature.clone(),
                self.create_type_mapper(type_parameters.clone(), Some(base_constraints)),
                Some(true),
            )?);
            signature
                .ref_(self)
                .set_base_signature_cache(Some(ret.clone()));
            return Ok(ret);
        }
        Ok(signature)
    }

    pub(super) fn get_or_create_type_from_signature(
        &self,
        signature: Id<Signature>,
    ) -> Id<Type /*ObjectType*/> {
        if signature
            .ref_(self)
            .maybe_isolated_signature_type()
            .is_none()
        {
            let kind = signature.ref_(self).declaration.map_or_else(
                || SyntaxKind::Unknown,
                |declaration| declaration.ref_(self).kind(),
            );
            let is_constructor = matches!(
                kind,
                SyntaxKind::Constructor
                    | SyntaxKind::ConstructSignature
                    | SyntaxKind::ConstructorType
            );
            let type_ = self.alloc_type(
                self.create_object_type(ObjectFlags::Anonymous, Option::<Id<Symbol>>::None)
                    .into(),
            );
            type_.ref_(self).as_resolvable_type().resolve(
                self.empty_symbols(),
                self.alloc_vec_symbol(vec![]),
                if !is_constructor {
                    vec![signature.clone()]
                } else {
                    vec![]
                },
                if is_constructor {
                    vec![signature.clone()]
                } else {
                    vec![]
                },
                vec![],
            );
            signature
                .ref_(self)
                .set_isolated_signature_type(Some(type_));
        }
        signature
            .ref_(self)
            .maybe_isolated_signature_type()
            .unwrap()
    }

    pub(super) fn get_index_symbol(&self, symbol: Id<Symbol>) -> Option<Id<Symbol>> {
        symbol
            .ref_(self)
            .maybe_members()
            .and_then(|members| self.get_index_symbol_from_symbol_table(&members.ref_(self)))
    }

    pub(super) fn get_index_symbol_from_symbol_table(
        &self,
        symbol_table: &SymbolTable,
    ) -> Option<Id<Symbol>> {
        symbol_table.get(InternalSymbolName::Index).cloned()
    }

    pub(super) fn create_index_info(
        &self,
        key_type: Id<Type>,
        type_: Id<Type>,
        is_readonly: bool,
        declaration: Option<Id<Node /*IndexSignatureDeclaration*/>>,
    ) -> IndexInfo {
        IndexInfo {
            key_type,
            type_,
            is_readonly,
            declaration,
        }
    }

    pub(super) fn get_index_infos_of_symbol(
        &self,
        symbol: Id<Symbol>,
    ) -> io::Result<Vec<Id<IndexInfo>>> {
        let index_symbol = self.get_index_symbol(symbol);
        Ok(if let Some(index_symbol) = index_symbol {
            self.get_index_infos_of_index_symbol(index_symbol)?
        } else {
            vec![]
        })
    }

    pub(super) fn get_index_infos_of_index_symbol(
        &self,
        index_symbol: Id<Symbol>,
    ) -> io::Result<Vec<Id<IndexInfo>>> {
        if let Some(index_symbol_declarations) =
            index_symbol.ref_(self).maybe_declarations().as_ref()
        {
            let mut index_infos: Vec<Id<IndexInfo>> = vec![];
            for &declaration in index_symbol_declarations {
                let declaration_ref = declaration.ref_(self);
                let declaration_as_index_signature_declaration =
                    declaration_ref.as_index_signature_declaration();
                if declaration_as_index_signature_declaration
                    .parameters()
                    .ref_(self)
                    .len()
                    == 1
                {
                    let parameter = declaration_as_index_signature_declaration
                        .parameters()
                        .ref_(self)[0];
                    if let Some(parameter_type) =
                        parameter.ref_(self).as_parameter_declaration().maybe_type()
                    {
                        self.try_for_each_type(
                            self.get_type_from_type_node_(parameter_type)?,
                            |key_type| {
                                if self.is_valid_index_key_type(key_type)?
                                    && self.find_index_info(&index_infos, key_type).is_none()
                                {
                                    index_infos.push(
                                        self.alloc_index_info(
                                            self.create_index_info(
                                                key_type,
                                                if let Some(declaration_type) =
                                                    declaration_as_index_signature_declaration
                                                        .maybe_type()
                                                {
                                                    self.get_type_from_type_node_(declaration_type)?
                                                } else {
                                                    self.any_type()
                                                },
                                                has_effective_modifier(
                                                    declaration,
                                                    ModifierFlags::Readonly,
                                                    self,
                                                ),
                                                Some(declaration.clone()),
                                            ),
                                        ),
                                    );
                                }
                                Ok(Option::<()>::None)
                            },
                        )?;
                    }
                }
            }
            return Ok(index_infos);
        }
        Ok(vec![])
    }

    pub(super) fn is_valid_index_key_type(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::String | TypeFlags::Number | TypeFlags::ESSymbol)
            || self.is_pattern_literal_type(type_)
            || type_.ref_(self).flags().intersects(TypeFlags::Intersection)
                && !self.is_generic_type(type_)?
                && try_some(
                    Some(
                        type_
                            .ref_(self)
                            .as_union_or_intersection_type_interface()
                            .types(),
                    ),
                    Some(|&type_: &Id<Type>| self.is_valid_index_key_type(type_)),
                )?)
    }

    pub(super) fn get_constraint_declaration(
        &self,
        type_: Id<Type>, /*TypeParameter*/
    ) -> Option<Id<Node /*TypeNode*/>> {
        map_defined(
            maybe_filter(
                type_
                    .ref_(self)
                    .maybe_symbol()
                    .and_then(|symbol| symbol.ref_(self).maybe_declarations().clone())
                    .as_deref(),
                |node: &Id<Node>| is_type_parameter_declaration(&node.ref_(self)),
            ),
            |node, _| get_effective_constraint_of_type_parameter(node, self),
        )
        .get(0)
        .map(Clone::clone)
    }
}
