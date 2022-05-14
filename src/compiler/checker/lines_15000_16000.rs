#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{get_symbol_id, intrinsic_type_kinds, IntrinsicTypeKind};
use crate::{
    append, capitalize, chain_diagnostic_messages, create_diagnostic_for_node,
    create_diagnostic_for_node_from_message_chain, every, find_ancestor,
    get_assignment_target_kind, get_combined_node_flags, get_object_flags,
    get_property_name_for_property_name_node, get_text_of_node, is_access_expression,
    is_assignment_target, is_call_like_expression, is_call_or_new_expression, is_delete_target,
    is_function_like, is_identifier, is_indexed_access_type_node, is_private_identifier,
    is_property_name, map, maybe_every, some, uncapitalize, unescape_leading_underscores,
    AccessFlags, AssignmentKind, DiagnosticMessageChain, Diagnostics, IndexInfo, IndexedAccessType,
    LiteralType, Node, NodeFlags, NodeInterface, Number, ObjectFlags, StringMappingType, Symbol,
    SymbolFlags, SymbolInterface, SyntaxKind, TemplateLiteralType, Type, TypeChecker,
    UnionOrIntersectionTypeInterface, __String, pseudo_big_int_to_string, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn get_template_string_for_type(&self, type_: &Type) -> Option<String> {
        if type_.flags().intersects(TypeFlags::StringLiteral) {
            Some(type_.as_string_literal_type().value.clone())
        } else if type_.flags().intersects(TypeFlags::NumberLiteral) {
            Some(type_.as_number_literal_type().value.to_string())
        } else if type_.flags().intersects(TypeFlags::BigIntLiteral) {
            Some(pseudo_big_int_to_string(
                &type_.as_big_int_literal_type().value,
            ))
        } else if type_
            .flags()
            .intersects(TypeFlags::BooleanLiteral | TypeFlags::Nullable)
        {
            Some(type_.as_intrinsic_type().intrinsic_name().to_owned())
        } else {
            None
        }
    }

    pub(super) fn create_template_literal_type(
        &self,
        texts: Vec<String>,
        types: Vec<Rc<Type>>,
    ) -> Rc<Type> {
        let type_ = self.create_type(TypeFlags::TemplateLiteral);
        let type_: Rc<Type> = TemplateLiteralType::new(type_, texts, types).into();
        type_
    }

    pub(super) fn get_string_mapping_type(&self, symbol: &Symbol, type_: &Type) -> Rc<Type> {
        if type_
            .flags()
            .intersects(TypeFlags::Union | TypeFlags::Never)
        {
            self.map_type(
                type_,
                &mut |t| Some(self.get_string_mapping_type(symbol, t)),
                None,
            )
            .unwrap()
        } else if self.is_generic_index_type(type_) {
            self.get_string_mapping_type_for_generic_type(symbol, type_)
        } else if type_.flags().intersects(TypeFlags::StringLiteral) {
            self.get_string_literal_type(
                &self.apply_string_mapping(symbol, &type_.as_string_literal_type().value),
            )
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn apply_string_mapping(&self, symbol: &Symbol, str_: &str) -> String {
        match intrinsic_type_kinds.get(&**symbol.escaped_name()) {
            Some(IntrinsicTypeKind::Uppercase) => str_.to_uppercase(),
            Some(IntrinsicTypeKind::Lowercase) => str_.to_lowercase(),
            Some(IntrinsicTypeKind::Capitalize) => capitalize(str_),
            Some(IntrinsicTypeKind::Uncapitalize) => uncapitalize(str_),
            _ => str_.to_owned(),
        }
    }

    pub(super) fn get_string_mapping_type_for_generic_type(
        &self,
        symbol: &Symbol,
        type_: &Type,
    ) -> Rc<Type> {
        let id = format!("{},{}", get_symbol_id(symbol), self.get_type_id(type_));
        let mut result = self.string_mapping_types().get(&id).map(Clone::clone);
        if result.is_none() {
            result = Some(self.create_string_mapping_type(symbol, type_));
            self.string_mapping_types()
                .insert(id, result.clone().unwrap());
        }
        result.unwrap()
    }

    pub(super) fn create_string_mapping_type(&self, symbol: &Symbol, type_: &Type) -> Rc<Type> {
        let result = self.create_type(TypeFlags::StringMapping);
        let result: Rc<Type> =
            StringMappingType::new(result, symbol.symbol_wrapper(), type_.type_wrapper()).into();
        result
    }

    pub(super) fn create_indexed_access_type<TAliasSymbol: Borrow<Symbol>>(
        &self,
        object_type: &Type,
        index_type: &Type,
        access_flags: AccessFlags,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let type_ = self.create_type(TypeFlags::IndexedAccess);
        let type_: Rc<Type> = IndexedAccessType::new(
            type_,
            object_type.type_wrapper(),
            index_type.type_wrapper(),
            access_flags,
        )
        .into();
        *type_.maybe_alias_symbol() =
            alias_symbol.map(|alias_symbol| alias_symbol.borrow().symbol_wrapper());
        *type_.maybe_alias_type_arguments() = alias_type_arguments.map(ToOwned::to_owned);
        type_
    }

    pub(super) fn is_js_literal_type(&self, type_: &Type) -> bool {
        if self.no_implicit_any {
            return false;
        }
        if get_object_flags(type_).intersects(ObjectFlags::JSLiteral) {
            return true;
        }
        if type_.flags().intersects(TypeFlags::Union) {
            return every(type_.as_union_type().types(), |type_: &Rc<Type>, _| {
                self.is_js_literal_type(type_)
            });
        }
        if type_.flags().intersects(TypeFlags::Intersection) {
            return some(
                Some(type_.as_intersection_type().types()),
                Some(|type_: &Rc<Type>| self.is_js_literal_type(type_)),
            );
        }
        if type_.flags().intersects(TypeFlags::Instantiable) {
            let constraint = self.get_resolved_base_constraint(type_);
            return !ptr::eq(&*constraint, type_) && self.is_js_literal_type(&constraint);
        }
        false
    }

    pub(super) fn get_property_name_from_index<TAccessNode: Borrow<Node>>(
        &self,
        index_type: &Type,
        access_node: Option<
            TAccessNode, /*StringLiteral | Identifier | PrivateIdentifier | ObjectBindingPattern | ArrayBindingPattern | ComputedPropertyName | NumericLiteral | IndexedAccessTypeNode | ElementAccessExpression | SyntheticExpression*/
        >,
    ) -> Option<__String> {
        if self.is_type_usable_as_property_name(index_type) {
            return Some(self.get_property_name_from_type(index_type));
        }
        let access_node = access_node.map(|access_node| access_node.borrow().node_wrapper());
        access_node
            .filter(|access_node| is_property_name(access_node))
            .and_then(|access_node| get_property_name_for_property_name_node(&access_node))
    }

    pub(super) fn is_uncalled_function_reference(&self, node: &Node, symbol: &Symbol) -> bool {
        if symbol
            .flags()
            .intersects(SymbolFlags::Function | SymbolFlags::Method)
        {
            let parent = find_ancestor(node.maybe_parent(), |n| !is_access_expression(n))
                .unwrap_or_else(|| node.parent());
            if is_call_like_expression(&parent) {
                return is_call_or_new_expression(&parent)
                    && is_identifier(node)
                    && self.has_matching_argument(&parent, node);
            }
            return maybe_every(symbol.maybe_declarations().as_deref(), |d: &Rc<Node>, _| {
                !is_function_like(Some(&**d))
                    || get_combined_node_flags(d).intersects(NodeFlags::Deprecated)
            });
        }
        true
    }

    pub(super) fn get_property_type_for_index_type<TAccessNode: Borrow<Node>>(
        &self,
        original_object_type: &Type,
        object_type: &Type,
        index_type: &Type,
        full_index_type: &Type,
        access_node: Option<
            TAccessNode, /*ElementAccessExpression | IndexedAccessTypeNode | PropertyName | BindingName | SyntheticExpression*/
        >,
        access_flags: AccessFlags,
    ) -> Option<Rc<Type>> {
        let access_node = access_node.map(|access_node| access_node.borrow().node_wrapper());
        let access_expression = access_node
            .as_deref()
            .filter(|access_node| access_node.kind() == SyntaxKind::ElementAccessExpression);
        let prop_name = if matches!(access_node.as_ref(), Some(access_node) if is_private_identifier(access_node))
        {
            None
        } else {
            self.get_property_name_from_index(index_type, access_node.as_deref())
        };

        if let Some(prop_name) = prop_name.as_ref() {
            if access_flags.intersects(AccessFlags::Contextual) {
                return Some(
                    self.get_type_of_property_of_contextual_type(object_type, prop_name)
                        .unwrap_or_else(|| self.any_type()),
                );
            }
            let prop = self.get_property_of_type_(object_type, &prop_name, None);
            if let Some(prop) = prop {
                if access_flags.intersects(AccessFlags::ReportDeprecated) {
                    if let Some(access_node) = access_node.as_ref() {
                        let prop_declarations = prop.maybe_declarations();
                        if let Some(prop_declarations) = prop_declarations.as_deref() {
                            if self
                                .get_declaration_node_flags_from_symbol(&prop)
                                .intersects(NodeFlags::Deprecated)
                                && self.is_uncalled_function_reference(access_node, &prop)
                            {
                                let deprecated_node = access_expression
                                    .map(|access_expression| {
                                        access_expression
                                            .as_element_access_expression()
                                            .argument_expression
                                            .clone()
                                    })
                                    .unwrap_or_else(|| {
                                        if is_indexed_access_type_node(access_node) {
                                            access_node
                                                .as_indexed_access_type_node()
                                                .index_type
                                                .clone()
                                        } else {
                                            access_node.clone()
                                        }
                                    });
                                self.add_deprecated_suggestion(
                                    &deprecated_node,
                                    prop_declarations,
                                    &**prop_name,
                                );
                            }
                        }
                    }
                }
                if let Some(access_expression) = access_expression {
                    let access_expression_as_element_access_expression =
                        access_expression.as_element_access_expression();
                    self.mark_property_as_referenced(
                        &prop,
                        Some(access_expression),
                        self.is_self_type_access(
                            &access_expression_as_element_access_expression.expression,
                            object_type.maybe_symbol(),
                        ),
                    );
                    if self.is_assignment_to_readonly_entity(
                        access_expression,
                        &prop,
                        get_assignment_target_kind(access_expression),
                    ) {
                        self.error(
                            Some(
                                &*access_expression_as_element_access_expression
                                    .argument_expression,
                            ),
                            &Diagnostics::Cannot_assign_to_0_because_it_is_a_read_only_property,
                            Some(vec![self.symbol_to_string_(
                                &prop,
                                Option::<&Node>::None,
                                None,
                                None,
                                None,
                            )]),
                        );
                        return None;
                    }
                    if access_flags.intersects(AccessFlags::CacheSymbol) {
                        self.get_node_links(access_node.as_ref().unwrap())
                            .borrow_mut()
                            .resolved_symbol = Some(prop.clone());
                    }
                    if self.is_this_property_access_in_constructor(access_expression, &prop) {
                        return Some(self.auto_type());
                    }
                }
                let prop_type = self.get_type_of_symbol(&prop);
                return Some(
                    if let Some(access_expression) = access_expression.filter(|access_expression| {
                        get_assignment_target_kind(access_expression) != AssignmentKind::Definite
                    }) {
                        self.get_flow_type_of_reference(
                            access_expression,
                            &prop_type,
                            Option::<&Type>::None,
                            Option::<&Node>::None,
                        )
                    } else {
                        prop_type
                    },
                );
            }
            if self.every_type(object_type, |type_| self.is_tuple_type(type_))
                && self.is_numeric_literal_name(&**prop_name)
                && Into::<Number>::into(&**prop_name).value() >= 0.0
            {
                if let Some(access_node) = access_node.as_ref() {
                    if self.every_type(object_type, |t| {
                        !t.as_type_reference()
                            .target
                            .as_tuple_type()
                            .has_rest_element
                    }) && !access_flags.intersects(AccessFlags::NoTupleBoundsCheck)
                    {
                        let index_node = self.get_index_node_for_access_expression(access_node);
                        if self.is_tuple_type(object_type) {
                            self.error(
                                Some(index_node),
                                &Diagnostics::Tuple_type_0_of_length_1_has_no_element_at_index_2,
                                Some(vec![
                                    self.type_to_string_(
                                        object_type,
                                        Option::<&Node>::None,
                                        None,
                                        None,
                                    ),
                                    self.get_type_reference_arity(object_type).to_string(),
                                    unescape_leading_underscores(prop_name),
                                ]),
                            );
                        } else {
                            self.error(
                                Some(index_node),
                                &Diagnostics::Property_0_does_not_exist_on_type_1,
                                Some(vec![
                                    unescape_leading_underscores(prop_name),
                                    self.type_to_string_(
                                        object_type,
                                        Option::<&Node>::None,
                                        None,
                                        None,
                                    ),
                                ]),
                            );
                        }
                    }
                }
                self.error_if_writing_to_readonly_index(
                    access_expression,
                    object_type,
                    self.get_index_info_of_type_(object_type, &self.number_type()),
                );
                return self.map_type(
                    object_type,
                    &mut |t| {
                        let rest_type = self
                            .get_rest_type_of_tuple_type(t)
                            .unwrap_or_else(|| self.undefined_type());
                        Some(if access_flags.intersects(AccessFlags::IncludeUndefined) {
                            self.get_union_type(
                                vec![rest_type, self.undefined_type()],
                                None,
                                Option::<&Symbol>::None,
                                None,
                                Option::<&Type>::None,
                            )
                        } else {
                            rest_type
                        })
                    },
                    None,
                );
            }
        }
        if !index_type.flags().intersects(TypeFlags::Nullable)
            && self.is_type_assignable_to_kind(
                index_type,
                TypeFlags::StringLike | TypeFlags::NumberLike | TypeFlags::ESSymbolLike,
                None,
            )
        {
            if object_type
                .flags()
                .intersects(TypeFlags::Any | TypeFlags::Never)
            {
                return Some(object_type.type_wrapper());
            }
            let index_info = self
                .get_applicable_index_info(object_type, index_type)
                .or_else(|| self.get_index_info_of_type_(object_type, &self.string_type()));
            if let Some(index_info) = index_info.as_deref() {
                if access_flags.intersects(AccessFlags::NoIndexSignatures)
                    && !Rc::ptr_eq(&index_info.key_type, &self.number_type())
                {
                    if access_expression.is_some() {
                        self.error(
                            access_expression,
                            &Diagnostics::Type_0_cannot_be_used_to_index_type_1,
                            Some(vec![
                                self.type_to_string_(index_type, Option::<&Node>::None, None, None),
                                self.type_to_string_(
                                    original_object_type,
                                    Option::<&Node>::None,
                                    None,
                                    None,
                                ),
                            ]),
                        );
                    }
                    return None;
                }
                if let Some(access_node) = access_node.as_ref() {
                    if Rc::ptr_eq(&index_info.key_type, &self.string_type())
                        && !self.is_type_assignable_to_kind(
                            index_type,
                            TypeFlags::String | TypeFlags::Number,
                            None,
                        )
                    {
                        let index_node = self.get_index_node_for_access_expression(access_node);
                        self.error(
                            Some(index_node),
                            &Diagnostics::Type_0_cannot_be_used_as_an_index_type,
                            Some(vec![self.type_to_string_(
                                index_type,
                                Option::<&Node>::None,
                                None,
                                None,
                            )]),
                        );
                        return Some(if access_flags.intersects(AccessFlags::IncludeUndefined) {
                            self.get_union_type(
                                vec![index_info.type_.clone(), self.undefined_type()],
                                None,
                                Option::<&Symbol>::None,
                                None,
                                Option::<&Type>::None,
                            )
                        } else {
                            index_info.type_.clone()
                        });
                    }
                }
                self.error_if_writing_to_readonly_index(
                    access_expression,
                    object_type,
                    Some(index_info),
                );
                return Some(if access_flags.intersects(AccessFlags::IncludeUndefined) {
                    self.get_union_type(
                        vec![index_info.type_.clone(), self.undefined_type()],
                        None,
                        Option::<&Symbol>::None,
                        None,
                        Option::<&Type>::None,
                    )
                } else {
                    index_info.type_.clone()
                });
            }
            if index_type.flags().intersects(TypeFlags::Never) {
                return Some(self.never_type());
            }
            if self.is_js_literal_type(object_type) {
                return Some(self.any_type());
            }
            if let Some(access_expression) = access_expression {
                if !self.is_const_enum_object_type(object_type) {
                    if self.is_object_literal_type(object_type) {
                        if self.no_implicit_any
                            && index_type
                                .flags()
                                .intersects(TypeFlags::StringLiteral | TypeFlags::NumberLiteral)
                        {
                            self.diagnostics().add(Rc::new(
                                create_diagnostic_for_node(
                                    access_expression,
                                    &Diagnostics::Property_0_does_not_exist_on_type_1,
                                    Some(vec![
                                        index_type.as_string_literal_type().value.clone(),
                                        self.type_to_string_(
                                            object_type,
                                            Option::<&Node>::None,
                                            None,
                                            None,
                                        ),
                                    ]),
                                )
                                .into(),
                            ));
                            return Some(self.undefined_type());
                        } else if index_type
                            .flags()
                            .intersects(TypeFlags::Number | TypeFlags::String)
                        {
                            let mut types = map(
                                Some(&*object_type.as_resolved_type().properties()),
                                |property: &Rc<Symbol>, _| self.get_type_of_symbol(property),
                            )
                            .unwrap();
                            append(&mut types, Some(self.undefined_type()));
                            return Some(self.get_union_type(
                                types,
                                None,
                                Option::<&Symbol>::None,
                                None,
                                Option::<&Type>::None,
                            ));
                        }
                    }

                    let mut took_if_branch = false;
                    if matches!(
                        object_type.maybe_symbol(),
                        Some(symbol) if Rc::ptr_eq(&symbol, &self.global_this_symbol())
                    ) {
                        if let Some(prop_name) = prop_name.as_ref() {
                            let global_this_symbol_exports =
                                self.global_this_symbol().maybe_exports().clone().unwrap();
                            let global_this_symbol_exports = (*global_this_symbol_exports).borrow();
                            if global_this_symbol_exports.contains_key(prop_name)
                                && global_this_symbol_exports
                                    .get(prop_name)
                                    .unwrap()
                                    .flags()
                                    .intersects(SymbolFlags::BlockScoped)
                            {
                                took_if_branch = true;
                                self.error(
                                    Some(access_expression),
                                    &Diagnostics::Property_0_does_not_exist_on_type_1,
                                    Some(vec![
                                        unescape_leading_underscores(prop_name),
                                        self.type_to_string_(
                                            object_type,
                                            Option::<&Node>::None,
                                            None,
                                            None,
                                        ),
                                    ]),
                                );
                            }
                        }
                    }
                    if !took_if_branch
                        && self.no_implicit_any
                        && !matches!(
                            self.compiler_options.suppress_implicit_any_index_errors,
                            Some(true)
                        )
                        && !access_flags.intersects(AccessFlags::SuppressNoImplicitAnyError)
                    {
                        if let Some(prop_name) = prop_name.as_ref().filter(|prop_name| {
                            self.type_has_static_property(prop_name, object_type)
                        }) {
                            let type_name = self.type_to_string_(
                                object_type,
                                Option::<&Node>::None,
                                None,
                                None,
                            );
                            self.error(
                                Some(access_expression),
                                &Diagnostics::Property_0_does_not_exist_on_type_1_Did_you_mean_to_access_the_static_member_2_instead,
                                Some(vec![
                                    (&**prop_name).to_owned(),
                                    type_name.clone(),
                                    format!("{}[{}]", type_name, get_text_of_node(&access_expression.as_element_access_expression().argument_expression, None))
                                ])
                            );
                        } else if self
                            .get_index_type_of_type_(object_type, &self.number_type())
                            .is_some()
                        {
                            self.error(
                                Some(&*access_expression.as_element_access_expression().argument_expression),
                                &Diagnostics::Element_implicitly_has_an_any_type_because_index_expression_is_not_of_type_number,
                                None,
                            );
                        } else {
                            if let Some(suggestion) = prop_name
                                .as_ref()
                                .and_then(|prop_name| {
                                    self.get_suggestion_for_nonexistent_property(
                                        prop_name.clone().into_string(),
                                        object_type,
                                    )
                                })
                                .filter(|suggestion| !suggestion.is_empty())
                            {
                                self.error(
                                    Some(&*access_expression.as_element_access_expression().argument_expression),
                                    &Diagnostics::Property_0_does_not_exist_on_type_1_Did_you_mean_2,
                                    Some(vec![
                                        prop_name.clone().unwrap().into_string(),
                                        self.type_to_string_(object_type, Option::<&Node>::None, None, None),
                                        suggestion,
                                    ])
                                );
                            } else {
                                let suggestion = self
                                    .get_suggestion_for_nonexistent_index_signature(
                                        object_type,
                                        access_expression,
                                        index_type,
                                    );
                                if let Some(suggestion) = suggestion {
                                    self.error(
                                        Some(access_expression),
                                        &Diagnostics::Element_implicitly_has_an_any_type_because_type_0_has_no_index_signature_Did_you_mean_to_call_1,
                                        Some(vec![
                                            self.type_to_string_(object_type, Option::<&Node>::None, None, None),
                                            suggestion,
                                        ])
                                    );
                                } else {
                                    let mut error_info: Option<DiagnosticMessageChain> = None;
                                    if index_type.flags().intersects(TypeFlags::EnumLiteral) {
                                        error_info = Some(chain_diagnostic_messages(
                                            None,
                                            &Diagnostics::Property_0_does_not_exist_on_type_1,
                                            Some(vec![
                                                format!(
                                                    "[{}]",
                                                    self.type_to_string_(
                                                        index_type,
                                                        Option::<&Node>::None,
                                                        None,
                                                        None
                                                    ),
                                                ),
                                                self.type_to_string_(
                                                    object_type,
                                                    Option::<&Node>::None,
                                                    None,
                                                    None,
                                                ),
                                            ]),
                                        ));
                                    } else if index_type
                                        .flags()
                                        .intersects(TypeFlags::UniqueESSymbol)
                                    {
                                        let symbol_name = self.get_fully_qualified_name(
                                            &index_type.as_unique_es_symbol_type().symbol,
                                            Some(access_expression),
                                        );
                                        error_info = Some(chain_diagnostic_messages(
                                            None,
                                            &Diagnostics::Property_0_does_not_exist_on_type_1,
                                            Some(vec![
                                                format!("[{}]", symbol_name,),
                                                self.type_to_string_(
                                                    object_type,
                                                    Option::<&Node>::None,
                                                    None,
                                                    None,
                                                ),
                                            ]),
                                        ));
                                    } else if index_type
                                        .flags()
                                        .intersects(TypeFlags::StringLiteral)
                                    {
                                        error_info = Some(chain_diagnostic_messages(
                                            None,
                                            &Diagnostics::Property_0_does_not_exist_on_type_1,
                                            Some(vec![
                                                index_type.as_string_literal_type().value.clone(),
                                                self.type_to_string_(
                                                    object_type,
                                                    Option::<&Node>::None,
                                                    None,
                                                    None,
                                                ),
                                            ]),
                                        ));
                                    } else if index_type
                                        .flags()
                                        .intersects(TypeFlags::NumberLiteral)
                                    {
                                        error_info = Some(chain_diagnostic_messages(
                                            None,
                                            &Diagnostics::Property_0_does_not_exist_on_type_1,
                                            Some(vec![
                                                index_type
                                                    .as_number_literal_type()
                                                    .value
                                                    .to_string(),
                                                self.type_to_string_(
                                                    object_type,
                                                    Option::<&Node>::None,
                                                    None,
                                                    None,
                                                ),
                                            ]),
                                        ));
                                    } else if index_type
                                        .flags()
                                        .intersects(TypeFlags::Number | TypeFlags::String)
                                    {
                                        error_info = Some(chain_diagnostic_messages(
                                            None,
                                            &Diagnostics::No_index_signature_with_a_parameter_of_type_0_was_found_on_type_1,
                                            Some(vec![
                                                self.type_to_string_(index_type, Option::<&Node>::None, None, None),
                                                self.type_to_string_(object_type, Option::<&Node>::None, None, None),
                                            ])
                                        ));
                                    }

                                    error_info = Some(chain_diagnostic_messages(
                                        error_info,
                                        &Diagnostics::Element_implicitly_has_an_any_type_because_expression_of_type_0_can_t_be_used_to_index_type_1,
                                        Some(vec![
                                            self.type_to_string_(full_index_type, Option::<&Node>::None, None, None),
                                            self.type_to_string_(object_type, Option::<&Node>::None, None, None),
                                        ])
                                    ));
                                    self.diagnostics().add(Rc::new(
                                        create_diagnostic_for_node_from_message_chain(
                                            access_expression,
                                            error_info.unwrap(),
                                            None,
                                        )
                                        .into(),
                                    ));
                                }
                            }
                        }
                    }
                    return None;
                }
            }
        }
        if self.is_js_literal_type(object_type) {
            return Some(self.any_type());
        }
        if let Some(access_node) = access_node.as_ref() {
            let index_node = self.get_index_node_for_access_expression(access_node);
            if index_type
                .flags()
                .intersects(TypeFlags::StringLiteral | TypeFlags::NumberLiteral)
            {
                self.error(
                    Some(index_node),
                    &Diagnostics::Property_0_does_not_exist_on_type_1,
                    Some(vec![
                        // TODO: put this in a shared helper?
                        match index_type {
                            Type::LiteralType(LiteralType::NumberLiteralType(index_type)) => {
                                index_type.value.to_string()
                            }
                            Type::LiteralType(LiteralType::StringLiteralType(index_type)) => {
                                index_type.value.clone()
                            }
                            _ => panic!("Expected NumberLiteralType or StringLiteralType"),
                        },
                        self.type_to_string_(object_type, Option::<&Node>::None, None, None),
                    ]),
                );
            } else if index_type
                .flags()
                .intersects(TypeFlags::String | TypeFlags::Number)
            {
                self.error(
                    Some(index_node),
                    &Diagnostics::Type_0_has_no_matching_index_signature_for_type_1,
                    Some(vec![
                        self.type_to_string_(object_type, Option::<&Node>::None, None, None),
                        self.type_to_string_(index_type, Option::<&Node>::None, None, None),
                    ]),
                );
            } else {
                self.error(
                    Some(index_node),
                    &Diagnostics::Type_0_cannot_be_used_as_an_index_type,
                    Some(vec![self.type_to_string_(
                        index_type,
                        Option::<&Node>::None,
                        None,
                        None,
                    )]),
                );
            }
        }
        if self.is_type_any(Some(index_type)) {
            return Some(index_type.type_wrapper());
        }
        None
    }

    pub(super) fn error_if_writing_to_readonly_index<TIndexInfo: Borrow<IndexInfo>>(
        &self,
        access_expression: Option<&Node>,
        object_type: &Type,
        index_info: Option<TIndexInfo>,
    ) {
        if let Some(index_info) = index_info {
            let index_info = index_info.borrow();
            if index_info.is_readonly {
                if let Some(access_expression) = access_expression {
                    if is_assignment_target(access_expression)
                        || is_delete_target(access_expression)
                    {
                        self.error(
                            Some(access_expression),
                            &Diagnostics::Index_signature_in_type_0_only_permits_reading,
                            Some(vec![self.type_to_string_(
                                object_type,
                                Option::<&Node>::None,
                                None,
                                None,
                            )]),
                        );
                    }
                }
            }
        }
    }

    pub(super) fn get_index_node_for_access_expression(
        &self,
        access_node: &Node, /*ElementAccessExpression | IndexedAccessTypeNode | PropertyName | BindingName | SyntheticExpression*/
    ) -> Rc<Node> {
        unimplemented!()
    }

    pub(super) fn is_pattern_literal_placeholder_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_pattern_literal_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_generic_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_generic_object_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_generic_index_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_this_type_parameter(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_simplified_type(&self, type_: &Type, writing: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_indexed_access_type<
        TAccessNode: Borrow<Node>,
        TAliasSymbol: Borrow<Symbol>,
    >(
        &self,
        object_type: &Type,
        index_type: &Type,
        access_flags: Option<AccessFlags>,
        access_node: Option<
            TAccessNode, /*ElementAccessExpression | IndexedAccessTypeNode | PropertyName | BindingName | SyntheticExpression*/
        >,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let access_flags = access_flags.unwrap_or(AccessFlags::None);
        unimplemented!()
    }

    pub(super) fn get_indexed_access_type_or_undefined<
        TAccessNode: Borrow<Node>,
        TAliasSymbol: Borrow<Symbol>,
    >(
        &self,
        object_type: &Type,
        index_type: &Type,
        access_flags: Option<AccessFlags>,
        access_node: Option<
            TAccessNode, /*ElementAccessExpression | IndexedAccessTypeNode | PropertyName | BindingName | SyntheticExpression*/
        >,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Option<Rc<Type>> {
        let access_flags = access_flags.unwrap_or(AccessFlags::None);
        let apparent_object_type = self.get_reduced_apparent_type(object_type);
        self.get_property_type_for_index_type(
            object_type,
            &apparent_object_type,
            index_type,
            index_type,
            Option::<&Node>::None, // TODO: this is wrong
            access_flags | AccessFlags::CacheSymbol | AccessFlags::ReportDeprecated,
        )
    }

    pub(super) fn get_type_from_mapped_type_node(
        &self,
        node: &Node, /*MappedTypeNode*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_actual_type_variable(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_true_type_from_conditional_type(
        &self,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_false_type_from_conditional_type(
        &self,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_inferred_true_type_from_conditional_type(
        &self,
        type_: &Type, /*ConditionalType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_infer_type_parameters(
        &self,
        node: &Node, /*ConditionalTypeNode*/
    ) -> Option<Vec<Rc<Type /*TypeParameter*/>>> {
        unimplemented!()
    }

    pub(super) fn get_alias_symbol_for_type_node(&self, node: &Node) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_arguments_for_alias_symbol<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
    ) -> Option<Vec<Rc<Type>>> {
        unimplemented!()
    }
}
