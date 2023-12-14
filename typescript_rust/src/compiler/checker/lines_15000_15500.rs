use std::{
    borrow::{Borrow, Cow},
    convert::TryInto,
    io, ptr,
};

use gc::Gc;
use id_arena::Id;

use super::{get_symbol_id, intrinsic_type_kinds, IntrinsicTypeKind};
use crate::{
    append, capitalize, chain_diagnostic_messages, create_diagnostic_for_node,
    create_diagnostic_for_node_from_message_chain, every, find_ancestor,
    get_assignment_target_kind, get_combined_node_flags, get_object_flags,
    get_property_name_for_property_name_node, get_text_of_node, is_access_expression,
    is_assignment_target, is_call_like_expression, is_call_or_new_expression, is_delete_target,
    is_function_like, is_identifier, is_indexed_access_type_node, is_private_identifier,
    is_property_name, maybe_every, pseudo_big_int_to_string, try_every, try_map, try_reduce_left,
    try_some, uncapitalize, unescape_leading_underscores, AccessFlags, AssignmentKind,
    DiagnosticMessageChain, Diagnostics, HasArena, InArena, IndexInfo, IndexedAccessType,
    LiteralType, Node, NodeFlags, NodeInterface, Number, ObjectFlags, ObjectFlagsTypeInterface,
    ObjectTypeInterface, OptionTry, StringMappingType, Symbol, SymbolFlags, SymbolInterface,
    SyntaxKind, TemplateLiteralType, Type, TypeChecker, TypeFlags, TypeInterface,
    UnionOrIntersectionTypeInterface, UnionReduction,
};

impl TypeChecker {
    pub(super) fn get_template_string_for_type(&self, type_: Id<Type>) -> Option<String> {
        if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::StringLiteral)
        {
            Some(type_.ref_(self).as_string_literal_type().value.clone())
        } else if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::NumberLiteral)
        {
            Some(type_.ref_(self).as_number_literal_type().value.to_string())
        } else if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::BigIntLiteral)
        {
            Some(pseudo_big_int_to_string(
                &type_.ref_(self).as_big_int_literal_type().value,
            ))
        } else if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::BooleanLiteral | TypeFlags::Nullable)
        {
            Some(
                type_
                    .ref_(self)
                    .as_intrinsic_type()
                    .intrinsic_name()
                    .to_owned(),
            )
        } else {
            None
        }
    }

    pub(super) fn create_template_literal_type(
        &self,
        texts: Vec<String>,
        types: Vec<Id<Type>>,
    ) -> Id<Type> {
        let type_ = self.create_type(TypeFlags::TemplateLiteral);
        let type_ = self.alloc_type(TemplateLiteralType::new(type_, texts, types).into());
        type_
    }

    pub(super) fn get_string_mapping_type(
        &self,
        symbol: Id<Symbol>,
        type_: Id<Type>,
    ) -> io::Result<Id<Type>> {
        Ok(
            if self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::Union | TypeFlags::Never)
            {
                self.try_map_type(
                    type_,
                    &mut |t| Ok(Some(self.get_string_mapping_type(symbol, t)?)),
                    None,
                )?
                .unwrap()
            } else if self.is_generic_index_type(type_)? {
                self.get_string_mapping_type_for_generic_type(symbol, type_)
            } else if self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::StringLiteral)
            {
                self.get_string_literal_type(&self.apply_string_mapping(symbol, &{
                    let value = type_.ref_(self).as_string_literal_type().value.clone();
                    value
                }))
            } else {
                type_
            },
        )
    }

    pub(super) fn apply_string_mapping(&self, symbol: Id<Symbol>, str_: &str) -> String {
        match intrinsic_type_kinds.get(self.symbol(symbol).escaped_name()) {
            Some(IntrinsicTypeKind::Uppercase) => str_.to_uppercase(),
            Some(IntrinsicTypeKind::Lowercase) => str_.to_lowercase(),
            Some(IntrinsicTypeKind::Capitalize) => capitalize(str_),
            Some(IntrinsicTypeKind::Uncapitalize) => uncapitalize(str_),
            _ => str_.to_owned(),
        }
    }

    pub(super) fn get_string_mapping_type_for_generic_type(
        &self,
        symbol: Id<Symbol>,
        type_: Id<Type>,
    ) -> Id<Type> {
        let id = format!(
            "{},{}",
            get_symbol_id(&self.symbol(symbol)),
            self.get_type_id(type_)
        );
        let mut result = self.string_mapping_types().get(&id).map(Clone::clone);
        if result.is_none() {
            result = Some(self.create_string_mapping_type(symbol, type_));
            self.string_mapping_types()
                .insert(id, result.clone().unwrap());
        }
        result.unwrap()
    }

    pub(super) fn create_string_mapping_type(
        &self,
        symbol: Id<Symbol>,
        type_: Id<Type>,
    ) -> Id<Type> {
        let result = self.create_type(TypeFlags::StringMapping);
        let result = self.alloc_type(StringMappingType::new(result, type_).into());
        result.ref_(self).set_symbol(Some(symbol));
        result
    }

    pub(super) fn create_indexed_access_type(
        &self,
        object_type: Id<Type>,
        index_type: Id<Type>,
        access_flags: AccessFlags,
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> Id<Type> {
        let type_ = self.create_type(TypeFlags::IndexedAccess);
        let type_ = self.alloc_type(
            IndexedAccessType::new(type_, object_type, index_type, access_flags).into(),
        );
        *type_.ref_(self).maybe_alias_symbol_mut() = alias_symbol;
        *type_.ref_(self).maybe_alias_type_arguments_mut() =
            alias_type_arguments.map(ToOwned::to_owned);
        type_
    }

    pub(super) fn is_js_literal_type(&self, type_: Id<Type>) -> io::Result<bool> {
        if self.no_implicit_any {
            return Ok(false);
        }
        if get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::JSLiteral) {
            return Ok(true);
        }
        if type_.ref_(self).flags().intersects(TypeFlags::Union) {
            return try_every(
                type_.ref_(self).as_union_type().types(),
                |&type_: &Id<Type>, _| self.is_js_literal_type(type_),
            );
        }
        if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Intersection)
        {
            return try_some(
                Some(type_.ref_(self).as_intersection_type().types()),
                Some(|&type_: &Id<Type>| self.is_js_literal_type(type_)),
            );
        }
        if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Instantiable)
        {
            let constraint = self.get_resolved_base_constraint(type_)?;
            return Ok(constraint != type_ && self.is_js_literal_type(constraint)?);
        }
        Ok(false)
    }

    pub(super) fn get_property_name_from_index(
        &self,
        index_type: Id<Type>,
        access_node: Option<
            impl Borrow<Node>, /*StringLiteral | Identifier | PrivateIdentifier | ObjectBindingPattern | ArrayBindingPattern | ComputedPropertyName | NumericLiteral | IndexedAccessTypeNode | ElementAccessExpression | SyntheticExpression*/
        >,
    ) -> Option<String /*__String*/> {
        if self.is_type_usable_as_property_name(index_type) {
            return Some(self.get_property_name_from_type(index_type));
        }
        let access_node = access_node.map(|access_node| access_node.borrow().node_wrapper());
        access_node
            .filter(|access_node| is_property_name(access_node))
            .and_then(|access_node| {
                get_property_name_for_property_name_node(&access_node).map(Cow::into_owned)
            })
    }

    pub(super) fn is_uncalled_function_reference(
        &self,
        node: &Node,
        symbol: Id<Symbol>,
    ) -> io::Result<bool> {
        if self
            .symbol(symbol)
            .flags()
            .intersects(SymbolFlags::Function | SymbolFlags::Method)
        {
            let parent = find_ancestor(node.maybe_parent(), |n| !is_access_expression(n))
                .unwrap_or_else(|| node.parent());
            if is_call_like_expression(&parent) {
                return Ok(is_call_or_new_expression(&parent)
                    && is_identifier(node)
                    && self.has_matching_argument(&parent, node)?);
            }
            return Ok(maybe_every(
                self.symbol(symbol).maybe_declarations().as_deref(),
                |d: &Gc<Node>, _| {
                    !is_function_like(Some(&**d))
                        || get_combined_node_flags(d).intersects(NodeFlags::Deprecated)
                },
            ));
        }
        Ok(true)
    }

    pub(super) fn get_property_type_for_index_type(
        &self,
        original_object_type: Id<Type>,
        object_type: Id<Type>,
        index_type: Id<Type>,
        full_index_type: Id<Type>,
        access_node: Option<
            impl Borrow<Node>, /*ElementAccessExpression | IndexedAccessTypeNode | PropertyName | BindingName | SyntheticExpression*/
        >,
        access_flags: AccessFlags,
    ) -> io::Result<Option<Id<Type>>> {
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
                return Ok(Some(
                    self.get_type_of_property_of_contextual_type(object_type, prop_name)?
                        .unwrap_or_else(|| self.any_type()),
                ));
            }
            let prop = self.get_property_of_type_(object_type, prop_name, None)?;
            if let Some(prop) = prop {
                if access_flags.intersects(AccessFlags::ReportDeprecated) {
                    if let Some(access_node) = access_node.as_ref() {
                        let prop_ref = self.symbol(prop);
                        let prop_declarations = prop_ref.maybe_declarations();
                        if let Some(prop_declarations) = prop_declarations.as_deref() {
                            if self
                                .get_declaration_node_flags_from_symbol(prop)
                                .intersects(NodeFlags::Deprecated)
                                && self.is_uncalled_function_reference(access_node, prop)?
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
                                    prop_name,
                                );
                            }
                        }
                    }
                }
                if let Some(access_expression) = access_expression {
                    let access_expression_as_element_access_expression =
                        access_expression.as_element_access_expression();
                    self.mark_property_as_referenced(
                        prop,
                        Some(access_expression),
                        self.is_self_type_access(
                            &access_expression_as_element_access_expression.expression,
                            object_type.ref_(self).maybe_symbol(),
                        )?,
                    );
                    if self.is_assignment_to_readonly_entity(
                        access_expression,
                        prop,
                        get_assignment_target_kind(access_expression),
                    )? {
                        self.error(
                            Some(
                                &*access_expression_as_element_access_expression
                                    .argument_expression,
                            ),
                            &Diagnostics::Cannot_assign_to_0_because_it_is_a_read_only_property,
                            Some(vec![self.symbol_to_string_(
                                prop,
                                Option::<&Node>::None,
                                None,
                                None,
                                None,
                            )?]),
                        );
                        return Ok(None);
                    }
                    if access_flags.intersects(AccessFlags::CacheSymbol) {
                        self.get_node_links(access_node.as_ref().unwrap())
                            .borrow_mut()
                            .resolved_symbol = Some(prop.clone());
                    }
                    if self.is_this_property_access_in_constructor(access_expression, prop)? {
                        return Ok(Some(self.auto_type()));
                    }
                }
                let prop_type = self.get_type_of_symbol(prop)?;
                return Ok(Some(
                    if let Some(access_expression) = access_expression.filter(|access_expression| {
                        get_assignment_target_kind(access_expression) != AssignmentKind::Definite
                    }) {
                        self.get_flow_type_of_reference(
                            access_expression,
                            prop_type,
                            None,
                            Option::<&Node>::None,
                        )?
                    } else {
                        prop_type
                    },
                ));
            }
            if self.every_type(object_type, |type_| self.is_tuple_type(type_))
                && self.is_numeric_literal_name(prop_name)
                && Into::<Number>::into(&**prop_name).value() >= 0.0
            {
                if let Some(access_node) = access_node.as_ref() {
                    if self.every_type(object_type, |t| {
                        !self
                            .type_(t.ref_(self).as_type_reference().target)
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
                                    )?,
                                    self.get_type_reference_arity(object_type).to_string(),
                                    unescape_leading_underscores(prop_name).to_owned(),
                                ]),
                            );
                        } else {
                            self.error(
                                Some(index_node),
                                &Diagnostics::Property_0_does_not_exist_on_type_1,
                                Some(vec![
                                    unescape_leading_underscores(prop_name).to_owned(),
                                    self.type_to_string_(
                                        object_type,
                                        Option::<&Node>::None,
                                        None,
                                        None,
                                    )?,
                                ]),
                            );
                        }
                    }
                }
                self.error_if_writing_to_readonly_index(
                    access_expression,
                    object_type,
                    self.get_index_info_of_type_(object_type, self.number_type())?,
                )?;
                return self.try_map_type(
                    object_type,
                    &mut |t| -> io::Result<_> {
                        let rest_type = self
                            .get_rest_type_of_tuple_type(t)?
                            .unwrap_or_else(|| self.undefined_type());
                        Ok(Some(
                            if access_flags.intersects(AccessFlags::IncludeUndefined) {
                                self.get_union_type(
                                    &[rest_type, self.undefined_type()],
                                    None,
                                    Option::<Id<Symbol>>::None,
                                    None,
                                    None,
                                )?
                            } else {
                                rest_type
                            },
                        ))
                    },
                    None,
                );
            }
        }
        if !self
            .type_(index_type)
            .flags()
            .intersects(TypeFlags::Nullable)
            && self.is_type_assignable_to_kind(
                index_type,
                TypeFlags::StringLike | TypeFlags::NumberLike | TypeFlags::ESSymbolLike,
                None,
            )?
        {
            if self
                .type_(object_type)
                .flags()
                .intersects(TypeFlags::Any | TypeFlags::Never)
            {
                return Ok(Some(object_type));
            }
            let index_info = self
                .get_applicable_index_info(object_type, index_type)?
                .try_or_else(|| self.get_index_info_of_type_(object_type, self.string_type()))?;
            if let Some(index_info) = index_info.as_deref() {
                if access_flags.intersects(AccessFlags::NoIndexSignatures)
                    && index_info.key_type != self.number_type()
                {
                    if access_expression.is_some() {
                        self.error(
                            access_expression,
                            &Diagnostics::Type_0_cannot_be_used_to_index_type_1,
                            Some(vec![
                                self.type_to_string_(
                                    index_type,
                                    Option::<&Node>::None,
                                    None,
                                    None,
                                )?,
                                self.type_to_string_(
                                    original_object_type,
                                    Option::<&Node>::None,
                                    None,
                                    None,
                                )?,
                            ]),
                        );
                    }
                    return Ok(None);
                }
                if let Some(access_node) = access_node.as_ref() {
                    if index_info.key_type == self.string_type()
                        && !self.is_type_assignable_to_kind(
                            index_type,
                            TypeFlags::String | TypeFlags::Number,
                            None,
                        )?
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
                            )?]),
                        );
                        return Ok(Some(
                            if access_flags.intersects(AccessFlags::IncludeUndefined) {
                                self.get_union_type(
                                    &[index_info.type_.clone(), self.undefined_type()],
                                    None,
                                    Option::<Id<Symbol>>::None,
                                    None,
                                    None,
                                )?
                            } else {
                                index_info.type_.clone()
                            },
                        ));
                    }
                }
                self.error_if_writing_to_readonly_index(
                    access_expression,
                    object_type,
                    Some(index_info),
                )?;
                return Ok(Some(
                    if access_flags.intersects(AccessFlags::IncludeUndefined) {
                        self.get_union_type(
                            &[index_info.type_.clone(), self.undefined_type()],
                            None,
                            Option::<Id<Symbol>>::None,
                            None,
                            None,
                        )?
                    } else {
                        index_info.type_.clone()
                    },
                ));
            }
            if index_type.ref_(self).flags().intersects(TypeFlags::Never) {
                return Ok(Some(self.never_type()));
            }
            if self.is_js_literal_type(object_type)? {
                return Ok(Some(self.any_type()));
            }
            if let Some(access_expression) = access_expression {
                if !self.is_const_enum_object_type(object_type) {
                    if self.is_object_literal_type(object_type) {
                        if self.no_implicit_any
                            && self
                                .type_(index_type)
                                .flags()
                                .intersects(TypeFlags::StringLiteral | TypeFlags::NumberLiteral)
                        {
                            self.diagnostics().add(
                                create_diagnostic_for_node(
                                    access_expression,
                                    &Diagnostics::Property_0_does_not_exist_on_type_1,
                                    Some(vec![
                                        match &*index_type.ref_(self) {
                                            Type::LiteralType(LiteralType::StringLiteralType(
                                                index_type,
                                            )) => index_type.value.clone(),
                                            Type::LiteralType(LiteralType::NumberLiteralType(
                                                index_type,
                                            )) => index_type.value.to_string(),
                                            _ => panic!("Expected string or number literal type"),
                                        },
                                        self.type_to_string_(
                                            object_type,
                                            Option::<&Node>::None,
                                            None,
                                            None,
                                        )?,
                                    ]),
                                )
                                .into(),
                            );
                            return Ok(Some(self.undefined_type()));
                        } else if self
                            .type_(index_type)
                            .flags()
                            .intersects(TypeFlags::Number | TypeFlags::String)
                        {
                            let mut types = try_map(
                                &*object_type.ref_(self).as_resolved_type().properties(),
                                |&property: &Id<Symbol>, _| self.get_type_of_symbol(property),
                            )?;
                            append(&mut types, Some(self.undefined_type()));
                            return Ok(Some(self.get_union_type(
                                &types,
                                None,
                                Option::<Id<Symbol>>::None,
                                None,
                                None,
                            )?));
                        }
                    }

                    let mut took_if_branch = false;
                    if matches!(
                        object_type.ref_(self).maybe_symbol(),
                        Some(symbol) if symbol == self.global_this_symbol()
                    ) {
                        if let Some(prop_name) = prop_name.as_ref() {
                            let global_this_symbol_exports = self
                                .symbol(self.global_this_symbol())
                                .maybe_exports()
                                .clone()
                                .unwrap();
                            let global_this_symbol_exports = (*global_this_symbol_exports).borrow();
                            if global_this_symbol_exports.contains_key(&**prop_name)
                                && self
                                    .symbol(*global_this_symbol_exports.get(&**prop_name).unwrap())
                                    .flags()
                                    .intersects(SymbolFlags::BlockScoped)
                            {
                                took_if_branch = true;
                                self.error(
                                    Some(access_expression),
                                    &Diagnostics::Property_0_does_not_exist_on_type_1,
                                    Some(vec![
                                        unescape_leading_underscores(prop_name).to_owned(),
                                        self.type_to_string_(
                                            object_type,
                                            Option::<&Node>::None,
                                            None,
                                            None,
                                        )?,
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
                        if let Some(prop_name) = prop_name.as_ref().try_filter(|prop_name| {
                            self.type_has_static_property(prop_name, object_type)
                        })? {
                            let type_name = self.type_to_string_(
                                object_type,
                                Option::<&Node>::None,
                                None,
                                None,
                            )?;
                            self.error(
                                Some(access_expression),
                                &Diagnostics::Property_0_does_not_exist_on_type_1_Did_you_mean_to_access_the_static_member_2_instead,
                                Some(vec![
                                    (**prop_name).to_owned(),
                                    type_name.clone(),
                                    format!("{}[{}]", type_name, get_text_of_node(&access_expression.as_element_access_expression().argument_expression, None))
                                ])
                            );
                        } else if self
                            .get_index_type_of_type_(object_type, self.number_type())?
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
                                .try_and_then(|prop_name| {
                                    self.get_suggestion_for_nonexistent_property(
                                        &**prop_name,
                                        object_type,
                                    )
                                })?
                                .filter(|suggestion| !suggestion.is_empty())
                            {
                                self.error(
                                    Some(&*access_expression.as_element_access_expression().argument_expression),
                                    &Diagnostics::Property_0_does_not_exist_on_type_1_Did_you_mean_2,
                                    Some(vec![
                                        (*prop_name.unwrap()).to_owned(),
                                        self.type_to_string_(object_type, Option::<&Node>::None, None, None)?,
                                        suggestion,
                                    ])
                                );
                            } else {
                                let suggestion = self
                                    .get_suggestion_for_nonexistent_index_signature(
                                        object_type,
                                        access_expression,
                                        index_type,
                                    )?;
                                if let Some(suggestion) = suggestion {
                                    self.error(
                                        Some(access_expression),
                                        &Diagnostics::Element_implicitly_has_an_any_type_because_type_0_has_no_index_signature_Did_you_mean_to_call_1,
                                        Some(vec![
                                            self.type_to_string_(object_type, Option::<&Node>::None, None, None)?,
                                            suggestion,
                                        ])
                                    );
                                } else {
                                    let mut error_info: Option<DiagnosticMessageChain> = None;
                                    if self
                                        .type_(index_type)
                                        .flags()
                                        .intersects(TypeFlags::EnumLiteral)
                                    {
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
                                                    )?,
                                                ),
                                                self.type_to_string_(
                                                    object_type,
                                                    Option::<&Node>::None,
                                                    None,
                                                    None,
                                                )?,
                                            ]),
                                        ));
                                    } else if self
                                        .type_(index_type)
                                        .flags()
                                        .intersects(TypeFlags::UniqueESSymbol)
                                    {
                                        let symbol_name = self.get_fully_qualified_name(
                                            index_type.ref_(self).symbol(),
                                            Some(access_expression),
                                        )?;
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
                                                )?,
                                            ]),
                                        ));
                                    } else if self
                                        .type_(index_type)
                                        .flags()
                                        .intersects(TypeFlags::StringLiteral)
                                    {
                                        error_info = Some(chain_diagnostic_messages(
                                            None,
                                            &Diagnostics::Property_0_does_not_exist_on_type_1,
                                            Some(vec![
                                                index_type
                                                    .ref_(self)
                                                    .as_string_literal_type()
                                                    .value
                                                    .clone(),
                                                self.type_to_string_(
                                                    object_type,
                                                    Option::<&Node>::None,
                                                    None,
                                                    None,
                                                )?,
                                            ]),
                                        ));
                                    } else if self
                                        .type_(index_type)
                                        .flags()
                                        .intersects(TypeFlags::NumberLiteral)
                                    {
                                        error_info = Some(chain_diagnostic_messages(
                                            None,
                                            &Diagnostics::Property_0_does_not_exist_on_type_1,
                                            Some(vec![
                                                index_type
                                                    .ref_(self)
                                                    .as_number_literal_type()
                                                    .value
                                                    .to_string(),
                                                self.type_to_string_(
                                                    object_type,
                                                    Option::<&Node>::None,
                                                    None,
                                                    None,
                                                )?,
                                            ]),
                                        ));
                                    } else if self
                                        .type_(index_type)
                                        .flags()
                                        .intersects(TypeFlags::Number | TypeFlags::String)
                                    {
                                        error_info = Some(chain_diagnostic_messages(
                                            None,
                                            &Diagnostics::No_index_signature_with_a_parameter_of_type_0_was_found_on_type_1,
                                            Some(vec![
                                                self.type_to_string_(index_type, Option::<&Node>::None, None, None)?,
                                                self.type_to_string_(object_type, Option::<&Node>::None, None, None)?,
                                            ])
                                        ));
                                    }

                                    error_info = Some(chain_diagnostic_messages(
                                        error_info,
                                        &Diagnostics::Element_implicitly_has_an_any_type_because_expression_of_type_0_can_t_be_used_to_index_type_1,
                                        Some(vec![
                                            self.type_to_string_(full_index_type, Option::<&Node>::None, None, None)?,
                                            self.type_to_string_(object_type, Option::<&Node>::None, None, None)?,
                                        ])
                                    ));
                                    self.diagnostics().add(Gc::new(
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
                    return Ok(None);
                }
            }
        }
        if self.is_js_literal_type(object_type)? {
            return Ok(Some(self.any_type()));
        }
        if let Some(access_node) = access_node.as_ref() {
            let index_node = self.get_index_node_for_access_expression(access_node);
            if self
                .type_(index_type)
                .flags()
                .intersects(TypeFlags::StringLiteral | TypeFlags::NumberLiteral)
            {
                self.error(
                    Some(index_node),
                    &Diagnostics::Property_0_does_not_exist_on_type_1,
                    Some(vec![
                        // TODO: put this in a shared helper?
                        match &*index_type.ref_(self) {
                            Type::LiteralType(LiteralType::NumberLiteralType(index_type)) => {
                                index_type.value.to_string()
                            }
                            Type::LiteralType(LiteralType::StringLiteralType(index_type)) => {
                                index_type.value.clone()
                            }
                            _ => panic!("Expected NumberLiteralType or StringLiteralType"),
                        },
                        self.type_to_string_(object_type, Option::<&Node>::None, None, None)?,
                    ]),
                );
            } else if self
                .type_(index_type)
                .flags()
                .intersects(TypeFlags::String | TypeFlags::Number)
            {
                self.error(
                    Some(index_node),
                    &Diagnostics::Type_0_has_no_matching_index_signature_for_type_1,
                    Some(vec![
                        self.type_to_string_(object_type, Option::<&Node>::None, None, None)?,
                        self.type_to_string_(index_type, Option::<&Node>::None, None, None)?,
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
                    )?]),
                );
            }
        }
        if self.is_type_any(Some(index_type)) {
            return Ok(Some(index_type));
        }
        Ok(None)
    }

    pub(super) fn error_if_writing_to_readonly_index(
        &self,
        access_expression: Option<&Node>,
        object_type: Id<Type>,
        index_info: Option<impl Borrow<IndexInfo>>,
    ) -> io::Result<()> {
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
                            )?]),
                        );
                    }
                }
            }
        }

        Ok(())
    }

    pub(super) fn get_index_node_for_access_expression(
        &self,
        access_node: &Node, /*ElementAccessExpression | IndexedAccessTypeNode | PropertyName | BindingName | SyntheticExpression*/
    ) -> Gc<Node> {
        if access_node.kind() == SyntaxKind::ElementAccessExpression {
            access_node
                .as_element_access_expression()
                .argument_expression
                .clone()
        } else if access_node.kind() == SyntaxKind::IndexedAccessType {
            access_node.as_indexed_access_type_node().index_type.clone()
        } else if access_node.kind() == SyntaxKind::ComputedPropertyName {
            access_node.as_computed_property_name().expression.clone()
        } else {
            access_node.node_wrapper()
        }
    }

    pub(super) fn is_pattern_literal_placeholder_type(&self, type_: Id<Type>) -> bool {
        type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Any | TypeFlags::String | TypeFlags::Number | TypeFlags::BigInt)
    }

    pub(super) fn is_pattern_literal_type(&self, type_: Id<Type>) -> bool {
        type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::TemplateLiteral)
            && every(
                &type_.ref_(self).as_template_literal_type().types,
                |&type_: &Id<Type>, _| self.is_pattern_literal_placeholder_type(type_),
            )
    }

    pub(super) fn is_generic_type(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(self.get_generic_object_flags(type_)? != ObjectFlags::None)
    }

    pub(super) fn is_generic_object_type(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(self
            .get_generic_object_flags(type_)?
            .intersects(ObjectFlags::IsGenericObjectType))
    }

    pub(super) fn is_generic_index_type(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(self
            .get_generic_object_flags(type_)?
            .intersects(ObjectFlags::IsGenericIndexType))
    }

    pub(super) fn get_generic_object_flags(&self, type_: Id<Type>) -> io::Result<ObjectFlags> {
        if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
        {
            if !self
                .type_(type_)
                .as_union_or_intersection_type()
                .object_flags()
                .intersects(ObjectFlags::IsGenericTypeComputed)
            {
                type_
                    .ref_(self)
                    .as_union_or_intersection_type()
                    .set_object_flags(
                        type_
                            .ref_(self)
                            .as_union_or_intersection_type()
                            .object_flags()
                            | ObjectFlags::IsGenericTypeComputed
                            | try_reduce_left(
                                type_.ref_(self).as_union_or_intersection_type().types(),
                                |flags, &t: &Id<Type>, _| -> io::Result<_> {
                                    Ok(flags | self.get_generic_object_flags(t)?)
                                },
                                ObjectFlags::None,
                                None,
                                None,
                            )?,
                    );
            }
            return Ok(self
                .type_(type_)
                .as_union_or_intersection_type()
                .object_flags()
                & ObjectFlags::IsGenericType);
        }
        if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Substitution)
        {
            if !self
                .type_(type_)
                .as_substitution_type()
                .object_flags()
                .intersects(ObjectFlags::IsGenericTypeComputed)
            {
                type_.ref_(self).as_substitution_type().set_object_flags(
                    type_.ref_(self).as_substitution_type().object_flags()
                        | ObjectFlags::IsGenericTypeComputed
                        | self.get_generic_object_flags(
                            type_.ref_(self).as_substitution_type().substitute,
                        )?
                        | self.get_generic_object_flags(
                            type_.ref_(self).as_substitution_type().base_type,
                        )?,
                );
            }
            return Ok(
                type_.ref_(self).as_substitution_type().object_flags() & ObjectFlags::IsGenericType
            );
        }
        Ok((if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::InstantiableNonPrimitive)
            || self.is_generic_mapped_type(type_)?
            || self.is_generic_tuple_type(type_)
        {
            ObjectFlags::IsGenericObjectType
        } else {
            ObjectFlags::None
        }) | if type_.ref_(self).flags().intersects(
            TypeFlags::InstantiableNonPrimitive
                | TypeFlags::Index
                | TypeFlags::TemplateLiteral
                | TypeFlags::StringMapping,
        ) && !self.is_pattern_literal_type(type_)
        {
            ObjectFlags::IsGenericIndexType
        } else {
            ObjectFlags::None
        })
    }

    pub(super) fn is_this_type_parameter(&self, type_: Id<Type>) -> bool {
        type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::TypeParameter)
            && matches!(
                type_.ref_(self).as_type_parameter().is_this_type,
                Some(true)
            )
    }

    pub(super) fn get_simplified_type(
        &self,
        type_: Id<Type>,
        writing: bool,
    ) -> io::Result<Id<Type>> {
        Ok(
            if self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::IndexedAccess)
            {
                self.get_simplified_indexed_access_type(type_, writing)?
            } else if type_.ref_(self).flags().intersects(TypeFlags::Conditional) {
                self.get_simplified_conditional_type(type_, writing)?
            } else {
                type_
            },
        )
    }

    pub(super) fn distribute_index_over_object_type(
        &self,
        object_type: Id<Type>,
        index_type: Id<Type>,
        writing: bool,
    ) -> io::Result<Option<Id<Type>>> {
        if self
            .type_(object_type)
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
        {
            let types = try_map(
                object_type
                    .ref_(self)
                    .as_union_or_intersection_type_interface()
                    .types(),
                |&t: &Id<Type>, _| {
                    self.get_simplified_type(
                        self.get_indexed_access_type(
                            t,
                            index_type,
                            None,
                            Option::<&Node>::None,
                            Option::<Id<Symbol>>::None,
                            None,
                        )?,
                        writing,
                    )
                },
            )?;
            return Ok(Some(
                if self
                    .type_(object_type)
                    .flags()
                    .intersects(TypeFlags::Intersection)
                    || writing
                {
                    self.get_intersection_type(&types, Option::<Id<Symbol>>::None, None)?
                } else {
                    self.get_union_type(&types, None, Option::<Id<Symbol>>::None, None, None)?
                },
            ));
        }
        Ok(None)
    }

    pub(super) fn distribute_object_over_index_type(
        &self,
        object_type: Id<Type>,
        index_type: Id<Type>,
        writing: bool,
    ) -> io::Result<Option<Id<Type>>> {
        if index_type.ref_(self).flags().intersects(TypeFlags::Union) {
            let types = try_map(
                index_type.ref_(self).as_union_type().types(),
                |&t: &Id<Type>, _| {
                    self.get_simplified_type(
                        self.get_indexed_access_type(
                            object_type,
                            t,
                            None,
                            Option::<&Node>::None,
                            Option::<Id<Symbol>>::None,
                            None,
                        )?,
                        writing,
                    )
                },
            )?;
            return Ok(Some(if writing {
                self.get_intersection_type(&types, Option::<Id<Symbol>>::None, None)?
            } else {
                self.get_union_type(&types, None, Option::<Id<Symbol>>::None, None, None)?
            }));
        }
        Ok(None)
    }

    pub(super) fn get_simplified_indexed_access_type(
        &self,
        type_: Id<Type>, /*IndexedAccessType*/
        writing: bool,
    ) -> io::Result<Id<Type>> {
        let read_cache = || {
            if writing {
                type_
                    .ref_(self)
                    .as_indexed_access_type()
                    .maybe_simplified_for_writing()
                    .clone()
            } else {
                type_
                    .ref_(self)
                    .as_indexed_access_type()
                    .maybe_simplified_for_reading()
                    .clone()
            }
        };
        let write_cache = |simplified_type: Id<Type>| {
            if writing {
                *self
                    .type_(type_)
                    .as_indexed_access_type()
                    .maybe_simplified_for_writing() = Some(simplified_type);
            } else {
                *self
                    .type_(type_)
                    .as_indexed_access_type()
                    .maybe_simplified_for_reading() = Some(simplified_type);
            }
        };
        if let Some(type_cache) = read_cache() {
            return Ok(if type_cache == self.circular_constraint_type() {
                type_
            } else {
                type_cache
            });
        }
        write_cache(self.circular_constraint_type());
        let object_type = self.get_simplified_type(
            type_.ref_(self).as_indexed_access_type().object_type,
            writing,
        )?;
        let index_type = self.get_simplified_type(
            type_.ref_(self).as_indexed_access_type().index_type,
            writing,
        )?;
        let distributed_over_index =
            self.distribute_object_over_index_type(object_type, index_type, writing)?;
        if let Some(distributed_over_index) = distributed_over_index {
            write_cache(distributed_over_index.clone());
            return Ok(distributed_over_index);
        }
        if !self
            .type_(index_type)
            .flags()
            .intersects(TypeFlags::Instantiable)
        {
            let distributed_over_object =
                self.distribute_index_over_object_type(object_type, index_type, writing)?;
            if let Some(distributed_over_object) = distributed_over_object {
                write_cache(distributed_over_object.clone());
                return Ok(distributed_over_object);
            }
        }
        if self.is_generic_tuple_type(object_type)
            && self
                .type_(index_type)
                .flags()
                .intersects(TypeFlags::NumberLike)
        {
            let element_type = self.get_element_type_of_slice_of_tuple_type(
                object_type,
                if index_type.ref_(self).flags().intersects(TypeFlags::Number) {
                    0
                } else {
                    object_type
                        .ref_(self)
                        .as_type_reference()
                        .target
                        .ref_(self)
                        .as_tuple_type()
                        .fixed_length
                },
                Some(0),
                Some(writing),
            )?;
            if let Some(element_type) = element_type {
                write_cache(element_type.clone());
                return Ok(element_type);
            }
        }
        if self.is_generic_mapped_type(object_type)? {
            let ret = self
                .try_map_type(
                    self.substitute_indexed_mapped_type(
                        object_type,
                        type_.ref_(self).as_indexed_access_type().index_type,
                    )?,
                    &mut |t| Ok(Some(self.get_simplified_type(t, writing)?)),
                    None,
                )?
                .unwrap();
            write_cache(ret.clone());
            return Ok(ret);
        }
        write_cache(type_);
        Ok(type_)
    }

    pub(super) fn get_simplified_conditional_type(
        &self,
        type_: Id<Type>, /*ConditionalType*/
        writing: bool,
    ) -> io::Result<Id<Type>> {
        let check_type = type_.ref_(self).as_conditional_type().check_type;
        let extends_type = type_.ref_(self).as_conditional_type().extends_type;
        let true_type = self.get_true_type_from_conditional_type(type_)?;
        let false_type = self.get_false_type_from_conditional_type(type_)?;
        if false_type.ref_(self).flags().intersects(TypeFlags::Never)
            && self.get_actual_type_variable(true_type)?
                == self.get_actual_type_variable(check_type)?
        {
            if check_type.ref_(self).flags().intersects(TypeFlags::Any)
                || self.is_type_assignable_to(
                    self.get_restrictive_instantiation(check_type)?,
                    self.get_restrictive_instantiation(extends_type)?,
                )?
            {
                return self.get_simplified_type(true_type, writing);
            } else if self.is_intersection_empty(check_type, extends_type)? {
                return Ok(self.never_type());
            }
        } else if true_type.ref_(self).flags().intersects(TypeFlags::Never)
            && self.get_actual_type_variable(false_type)?
                == self.get_actual_type_variable(check_type)?
        {
            if !check_type.ref_(self).flags().intersects(TypeFlags::Any)
                && self.is_type_assignable_to(
                    self.get_restrictive_instantiation(check_type)?,
                    self.get_restrictive_instantiation(extends_type)?,
                )?
            {
                return Ok(self.never_type());
            } else if check_type.ref_(self).flags().intersects(TypeFlags::Any)
                || self.is_intersection_empty(check_type, extends_type)?
            {
                return self.get_simplified_type(false_type, writing);
            }
        }
        Ok(type_)
    }

    pub(super) fn is_intersection_empty(
        &self,
        type1: Id<Type>,
        type2: Id<Type>,
    ) -> io::Result<bool> {
        Ok(self
            .type_(self.get_union_type(
                &[
                    self.intersect_types(Some(type1), Some(type2))?.unwrap(),
                    self.never_type(),
                ],
                None,
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?)
            .flags()
            .intersects(TypeFlags::Never))
    }

    pub(super) fn substitute_indexed_mapped_type(
        &self,
        object_type: Id<Type>, /*MappedType*/
        index: Id<Type>,
    ) -> io::Result<Id<Type>> {
        let mapper = self.create_type_mapper(
            vec![self.get_type_parameter_from_mapped_type(object_type)?],
            Some(vec![index]),
        );
        let template_mapper = self.combine_type_mappers(
            object_type.ref_(self).as_mapped_type().maybe_mapper(),
            mapper,
        );
        self.instantiate_type(
            self.get_template_type_from_mapped_type(object_type)?,
            Some(template_mapper),
        )
    }

    pub(super) fn get_indexed_access_type(
        &self,
        object_type: Id<Type>,
        index_type: Id<Type>,
        access_flags: Option<AccessFlags>,
        access_node: Option<
            impl Borrow<Node>, /*ElementAccessExpression | IndexedAccessTypeNode | PropertyName | BindingName | SyntheticExpression*/
        >,
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Type>> {
        let access_flags = access_flags.unwrap_or(AccessFlags::None);
        let access_node = access_node.map(|access_node| access_node.borrow().node_wrapper());
        Ok(self
            .get_indexed_access_type_or_undefined(
                object_type,
                index_type,
                Some(access_flags),
                access_node.as_deref(),
                alias_symbol,
                alias_type_arguments,
            )?
            .unwrap_or_else(|| {
                if access_node.is_some() {
                    self.error_type()
                } else {
                    self.unknown_type()
                }
            }))
    }

    pub(super) fn index_type_less_than(&self, index_type: Id<Type>, limit: isize) -> bool {
        self.every_type(index_type, |t| {
            if self
                .type_(t)
                .flags()
                .intersects(TypeFlags::StringOrNumberLiteral)
            {
                let prop_name = self.get_property_name_from_type(t);
                if self.is_numeric_literal_name(&*prop_name) {
                    let index = prop_name.parse::<isize>().unwrap();
                    return index >= 0 && index < limit;
                }
            }
            false
        })
    }

    pub(super) fn get_indexed_access_type_or_undefined(
        &self,
        object_type: Id<Type>,
        mut index_type: Id<Type>,
        access_flags: Option<AccessFlags>,
        access_node: Option<
            impl Borrow<Node>, /*ElementAccessExpression | IndexedAccessTypeNode | PropertyName | BindingName | SyntheticExpression*/
        >,
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Option<Id<Type>>> {
        let mut access_flags = access_flags.unwrap_or(AccessFlags::None);
        if object_type == self.wildcard_type() || index_type == self.wildcard_type() {
            return Ok(Some(self.wildcard_type()));
        }
        if self.is_string_index_signature_only_type(object_type)?
            && !self
                .type_(index_type)
                .flags()
                .intersects(TypeFlags::Nullable)
            && self.is_type_assignable_to_kind(
                index_type,
                TypeFlags::String | TypeFlags::Number,
                None,
            )?
        {
            index_type = self.string_type();
        }
        if matches!(
            self.compiler_options.no_unchecked_indexed_access,
            Some(true)
        ) && access_flags.intersects(AccessFlags::ExpressionPosition)
        {
            access_flags |= AccessFlags::IncludeUndefined;
        }
        let access_node = access_node.map(|access_node| access_node.borrow().node_wrapper());
        if self.is_generic_index_type(index_type)?
            || if matches!(
                access_node.as_ref(),
                Some(access_node) if access_node.kind() != SyntaxKind::IndexedAccessType
            ) {
                self.is_generic_tuple_type(object_type)
                    && !self.index_type_less_than(
                        index_type,
                        object_type
                            .ref_(self)
                            .as_type_reference()
                            .target
                            .ref_(self)
                            .as_tuple_type()
                            .fixed_length
                            .try_into()
                            .unwrap(),
                    )
            } else {
                self.is_generic_object_type(object_type)?
                    && !(self.is_tuple_type(object_type)
                        && self.index_type_less_than(
                            index_type,
                            object_type
                                .ref_(self)
                                .as_type_reference()
                                .target
                                .ref_(self)
                                .as_tuple_type()
                                .fixed_length
                                .try_into()
                                .unwrap(),
                        ))
            }
        {
            if self
                .type_(object_type)
                .flags()
                .intersects(TypeFlags::AnyOrUnknown)
            {
                return Ok(Some(object_type));
            }
            let persistent_access_flags = access_flags & AccessFlags::Persistent;
            let id = format!(
                "{},{},{}{}",
                object_type.ref_(self).id(),
                index_type.ref_(self).id(),
                persistent_access_flags.bits(),
                self.get_alias_id(alias_symbol, alias_type_arguments)
            );
            let mut type_ = self.indexed_access_types().get(&id).map(Clone::clone);
            if type_.is_none() {
                type_ = Some(self.create_indexed_access_type(
                    object_type,
                    index_type,
                    persistent_access_flags,
                    alias_symbol,
                    alias_type_arguments,
                ));
                self.indexed_access_types()
                    .insert(id, type_.clone().unwrap());
            }

            return Ok(type_);
        }
        let apparent_object_type = self.get_reduced_apparent_type(object_type)?;
        if index_type.ref_(self).flags().intersects(TypeFlags::Union)
            && !self
                .type_(index_type)
                .flags()
                .intersects(TypeFlags::Boolean)
        {
            let mut prop_types: Vec<Id<Type>> = vec![];
            let mut was_missing_prop = false;
            for &t in index_type.ref_(self).as_union_type().types() {
                let prop_type = self.get_property_type_for_index_type(
                    object_type,
                    apparent_object_type,
                    t,
                    index_type,
                    access_node.as_deref(),
                    access_flags
                        | if was_missing_prop {
                            AccessFlags::SuppressNoImplicitAnyError
                        } else {
                            AccessFlags::None
                        },
                )?;
                if let Some(prop_type) = prop_type {
                    prop_types.push(prop_type);
                } else if access_node.is_none() {
                    return Ok(None);
                } else {
                    was_missing_prop = true;
                }
            }
            if was_missing_prop {
                return Ok(None);
            }
            return Ok(Some(if access_flags.intersects(AccessFlags::Writing) {
                self.get_intersection_type(&prop_types, alias_symbol, alias_type_arguments)?
            } else {
                self.get_union_type(
                    &prop_types,
                    Some(UnionReduction::Literal),
                    alias_symbol,
                    alias_type_arguments,
                    None,
                )?
            }));
        }
        self.get_property_type_for_index_type(
            object_type,
            apparent_object_type,
            index_type,
            index_type,
            access_node,
            access_flags | AccessFlags::CacheSymbol | AccessFlags::ReportDeprecated,
        )
    }
}
