use std::{collections::HashMap, io, ptr};

use id_arena::Id;
use local_macros::enum_unwrapped;

use crate::{
    find_index, get_assignment_declaration_kind, get_check_flags, get_host_signature_from_jsdoc,
    get_symbol_id, get_this_container, is_binary_expression, is_class_like,
    is_constructor_declaration, is_function_expression, is_node_descendant_of,
    is_object_literal_expression, is_private_identifier_class_element_declaration, is_static,
    is_valid_es_symbol_declaration, map, maybe_add_range, maybe_is_class_like,
    pseudo_big_int_to_string, return_ok_none_if_none, some, try_map, try_maybe_filter, try_some,
    AssignmentDeclarationKind, BaseLiteralType, BigIntLiteralType, CheckFlags, Diagnostics,
    FunctionLikeDeclarationInterface, HasArena, InArena, IndexInfo, InferenceContext,
    InferenceInfo, InterfaceTypeInterface, IntrinsicType, LiteralType, LiteralTypeInterface, Node,
    NodeFlags, NodeInterface, Number, NumberLiteralType, ObjectFlags, ObjectFlagsTypeInterface,
    ObjectTypeInterface, OptionInArena, OptionTry, PseudoBigInt, Signature, SignatureFlags,
    StringLiteralType, StringOrNumber, Symbol, SymbolFlags, SymbolInterface, SyntaxKind,
    TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
    TypeMapperCallback, TypePredicate, TypeReferenceInterface, UniqueESSymbolType,
};

impl TypeChecker {
    pub(super) fn is_spreadable_property(&self, prop: Id<Symbol>) -> bool {
        !some(
            prop.ref_(self).maybe_declarations().as_deref(),
            Some(|&declaration: &Id<Node>| {
                is_private_identifier_class_element_declaration(declaration, self)
            }),
        ) && (!prop
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::Method | SymbolFlags::GetAccessor | SymbolFlags::SetAccessor)
            || !matches!(
                prop.ref_(self).maybe_declarations().as_ref(),
                Some(prop_declarations) if prop_declarations.iter().any(|decl: &Id<Node>| maybe_is_class_like(decl.ref_(self).maybe_parent().refed(self).as_deref()))
            ))
    }

    pub(super) fn get_spread_symbol(
        &self,
        prop: Id<Symbol>,
        readonly: bool,
    ) -> io::Result<Id<Symbol>> {
        let is_setonly_accessor = prop.ref_(self).flags().intersects(SymbolFlags::SetAccessor)
            && !prop.ref_(self).flags().intersects(SymbolFlags::GetAccessor);
        if !is_setonly_accessor && readonly == self.is_readonly_symbol(prop)? {
            return Ok(prop);
        }
        let flags = SymbolFlags::Property | (prop.ref_(self).flags() & SymbolFlags::Optional);
        let result = self.alloc_symbol(
            self.create_symbol(
                flags,
                prop.ref_(self).escaped_name().to_owned(),
                Some(
                    self.get_is_late_check_flag(prop)
                        | if readonly {
                            CheckFlags::Readonly
                        } else {
                            CheckFlags::None
                        },
                ),
            )
            .into(),
        );
        let result_links = result.ref_(self).as_transient_symbol().symbol_links();
        let mut result_links = result_links.ref_mut(self);
        result_links.type_ = Some(if is_setonly_accessor {
            self.undefined_type()
        } else {
            self.get_type_of_symbol(prop)?
        });
        if let Some(prop_declarations) = prop.ref_(self).maybe_declarations().clone() {
            result.ref_(self).set_declarations(prop_declarations);
        }
        result_links.name_type = self.get_symbol_links(prop).ref_(self).name_type;
        result_links.synthetic_origin = Some(prop);
        Ok(result)
    }

    pub(super) fn get_index_info_with_readonly(
        &self,
        info: Id<IndexInfo>,
        readonly: bool,
    ) -> Id<IndexInfo> {
        if info.ref_(self).is_readonly != readonly {
            self.alloc_index_info(self.create_index_info(
                info.ref_(self).key_type.clone(),
                info.ref_(self).type_.clone(),
                readonly,
                info.ref_(self).declaration.clone(),
            ))
        } else {
            info.clone()
        }
    }

    // pub fn create_literal_type(
    pub fn create_string_literal_type(
        &self,
        flags: TypeFlags,
        value: String,
        symbol: Option<Id<Symbol>>,
        regular_type: Option<Id<Type>>,
    ) -> Id<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_ = self.alloc_type(StringLiteralType::new(type_, value).into());
        type_.ref_(self).set_symbol(symbol);
        type_.ref_(self).as_literal_type().set_regular_type(
            if let Some(regular_type) = regular_type {
                regular_type
            } else {
                type_
            },
        );
        type_
    }

    pub fn create_number_literal_type(
        &self,
        flags: TypeFlags,
        value: Number,
        symbol: Option<Id<Symbol>>,
        regular_type: Option<Id<Type>>,
    ) -> Id<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_ = self.alloc_type(NumberLiteralType::new(type_, value).into());
        type_.ref_(self).set_symbol(symbol);
        type_.ref_(self).as_literal_type().set_regular_type(
            if let Some(regular_type) = regular_type {
                regular_type
            } else {
                type_
            },
        );
        type_
    }

    pub fn create_big_int_literal_type(
        &self,
        flags: TypeFlags,
        value: PseudoBigInt,
        symbol: Option<Id<Symbol>>,
        regular_type: Option<Id<Type>>,
    ) -> Id<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_ = self.alloc_type(BigIntLiteralType::new(type_, value).into());
        type_.ref_(self).set_symbol(symbol);
        type_.ref_(self).as_literal_type().set_regular_type(
            if let Some(regular_type) = regular_type {
                regular_type
            } else {
                type_
            },
        );
        type_
    }

    pub(super) fn get_fresh_type_of_literal_type(&self, type_: Id<Type>) -> Id<Type> {
        if type_.ref_(self).flags().intersects(TypeFlags::Literal) {
            if let Type::IntrinsicType(IntrinsicType::FreshableIntrinsicType(type_)) =
                &*type_.ref_(self)
            {
                return type_.fresh_type();
            }
            assert!(matches!(&*type_.ref_(self), Type::LiteralType(_)));
            return LiteralType::get_or_initialize_fresh_type(type_, self);
        }
        type_
    }

    pub(super) fn get_regular_type_of_literal_type(&self, type_: Id<Type>) -> Id<Type> {
        if type_.ref_(self).flags().intersects(TypeFlags::Literal) {
            // TODO: this seems like it should be encapsulated behind an abstraction (also above in
            // get_fresh_type_of_literal_type())?
            match &*type_.ref_(self) {
                Type::LiteralType(type_) => type_.regular_type(),
                Type::IntrinsicType(IntrinsicType::FreshableIntrinsicType(type_)) => {
                    type_.regular_type()
                }
                _ => unreachable!(),
            }
        } else if type_.ref_(self).flags().intersects(TypeFlags::Union) {
            if type_
                .ref_(self)
                .as_union_type()
                .maybe_regular_type()
                .is_none()
            {
                type_.ref_(self).as_union_type().set_regular_type(Some(
                    self.map_type(
                        type_,
                        &mut |type_| Some(self.get_regular_type_of_literal_type(type_)),
                        None,
                    )
                    .unwrap(),
                ));
            }
            type_
                .ref_(self)
                .as_union_type()
                .maybe_regular_type()
                .unwrap()
        } else {
            type_
        }
    }

    pub(super) fn is_fresh_literal_type(&self, type_: Id<Type>) -> bool {
        if !type_.ref_(self).flags().intersects(TypeFlags::Literal) {
            return false;
        }
        // TODO: should this be using eg a Type.as_has_fresh_type() "unwrapper-helper" instead?
        // (same question in is_type_related_to() and get_normalized_type() below, and in
        // remove_redundant_literal_types()) or maybe this looks like it should be a trait that
        // includes `maybe_fresh_type()` that both of these implement?
        match &*type_.ref_(self) {
            Type::IntrinsicType(intrinsic_type) => {
                type_
                    == enum_unwrapped!(intrinsic_type, [IntrinsicType, FreshableIntrinsicType])
                        .fresh_type()
            }
            Type::LiteralType(literal_type) => {
                matches!(
                    literal_type.fresh_type(),
                    Some(fresh_type) if type_ == fresh_type
                )
            }
            _ => panic!("Expected IntrinsicType or LiteralType"),
        }
    }

    pub(super) fn get_string_literal_type(&self, value: &str) -> Id<Type> {
        let mut string_literal_types = self.string_literal_types();
        if string_literal_types.contains_key(value) {
            return string_literal_types.get(value).unwrap().clone();
        }
        let type_ = self.create_string_literal_type(
            TypeFlags::StringLiteral,
            value.to_owned(),
            Option::<Id<Symbol>>::None,
            None,
        );
        string_literal_types.insert(value.to_owned(), type_.clone());
        type_
    }

    pub(super) fn get_number_literal_type(&self, value: Number) -> Id<Type> {
        let mut number_literal_types = self.number_literal_types();
        if number_literal_types.contains_key(&value) {
            return number_literal_types.get(&value).unwrap().clone();
        }
        let type_ = self.create_number_literal_type(
            TypeFlags::NumberLiteral,
            value,
            Option::<Id<Symbol>>::None,
            None,
        );
        number_literal_types.insert(value, type_.clone());
        type_
    }

    pub(super) fn get_big_int_literal_type(&self, value: PseudoBigInt) -> Id<Type> {
        let key = pseudo_big_int_to_string(&value);
        let mut big_int_literal_types = self.big_int_literal_types();
        if big_int_literal_types.contains_key(&key) {
            return big_int_literal_types.get(&key).unwrap().clone();
        }
        let type_ = self.create_big_int_literal_type(
            TypeFlags::BigIntLiteral,
            value,
            Option::<Id<Symbol>>::None,
            None,
        );
        big_int_literal_types.insert(key, type_.clone());
        type_
    }

    pub(super) fn get_enum_literal_type(
        &self,
        value: StringOrNumber,
        enum_id: usize,
        symbol: Id<Symbol>,
    ) -> Id<Type /*LiteralType*/> {
        let key = match value.clone() {
            StringOrNumber::String(value) => {
                format!("{}@{}", enum_id, value)
            }
            StringOrNumber::Number(value) => {
                format!("{}#{}", enum_id, value)
            }
        };
        let mut enum_literal_types = self.enum_literal_types();
        if enum_literal_types.contains_key(&key) {
            return enum_literal_types.get(&key).unwrap().clone();
        }
        let type_: Id<Type> = match value {
            StringOrNumber::String(value) => self.create_string_literal_type(
                TypeFlags::EnumLiteral | TypeFlags::StringLiteral,
                value,
                Some(symbol),
                None,
            ),
            StringOrNumber::Number(value) => self.create_number_literal_type(
                TypeFlags::EnumLiteral | TypeFlags::NumberLiteral,
                value,
                Some(symbol),
                None,
            ),
        };
        enum_literal_types.insert(key, type_.clone());
        type_
    }

    pub(super) fn get_type_from_literal_type_node(
        &self,
        node: Id<Node>, /*LiteralTypeNode*/
    ) -> io::Result<Id<Type>> {
        let node_ref = node.ref_(self);
        let node_as_literal_type_node = node_ref.as_literal_type_node();
        if node_as_literal_type_node.literal.ref_(self).kind() == SyntaxKind::NullKeyword {
            return Ok(self.null_type());
        }
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            links.ref_mut(self).resolved_type = Some(self.get_regular_type_of_literal_type(
                self.check_expression(node_as_literal_type_node.literal, None, None)?,
            ));
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn create_unique_es_symbol_type(
        &self,
        symbol: Id<Symbol>,
    ) -> Id<Type /*UniqueESSymbolType*/> {
        let type_ = self.create_type(TypeFlags::UniqueESSymbol);
        let type_ = self.alloc_type(
            UniqueESSymbolType::new(
                type_,
                symbol,
                format!(
                    "__@{}@{}",
                    symbol.ref_(self).escaped_name(),
                    get_symbol_id(&symbol.ref_(self))
                ),
            )
            .into(),
        );
        type_
    }

    pub(super) fn get_es_symbol_like_type_for_node(&self, node: Id<Node>) -> io::Result<Id<Type>> {
        if is_valid_es_symbol_declaration(node, self) {
            let symbol = self.get_symbol_of_node(node)?.unwrap();
            let links = self.get_symbol_links(symbol);
            if links.ref_(self).unique_es_symbol_type.is_none() {
                links.ref_mut(self).unique_es_symbol_type =
                    Some(self.create_unique_es_symbol_type(symbol));
            }
            return Ok(links.ref_(self).unique_es_symbol_type.clone().unwrap());
        }
        Ok(self.es_symbol_type())
    }

    pub(super) fn get_this_type(&self, node: Id<Node>) -> io::Result<Id<Type>> {
        let container = get_this_container(node, false, self);
        let parent = /*container &&*/ container.ref_(self).maybe_parent();
        if let Some(parent) = parent.filter(|parent| {
            is_class_like(&parent.ref_(self))
                || parent.ref_(self).kind() == SyntaxKind::InterfaceDeclaration
        }) {
            if !is_static(container, self)
                && (!is_constructor_declaration(&container.ref_(self))
                    || is_node_descendant_of(
                        node,
                        container
                            .ref_(self)
                            .as_constructor_declaration()
                            .maybe_body(),
                        self,
                    ))
            {
                return Ok(self
                    .get_declared_type_of_class_or_interface(
                        self.get_symbol_of_node(parent)?.unwrap(),
                    )?
                    .ref_(self)
                    .as_interface_type()
                    .maybe_this_type()
                    .unwrap());
            }
        }

        if let Some(parent) = parent.filter(|parent| {
            is_object_literal_expression(&parent.ref_(self))
                && is_binary_expression(&parent.ref_(self).parent().ref_(self))
                && get_assignment_declaration_kind(parent.ref_(self).parent(), self)
                    == AssignmentDeclarationKind::Prototype
        }) {
            return Ok(self
                .get_declared_type_of_class_or_interface(
                    self.get_symbol_of_node(
                        parent
                            .ref_(self)
                            .parent()
                            .ref_(self)
                            .as_binary_expression()
                            .left,
                    )?
                    .unwrap()
                    .ref_(self)
                    .maybe_parent()
                    .unwrap(),
                )?
                .ref_(self)
                .as_interface_type()
                .maybe_this_type()
                .unwrap());
        }
        let host = if node.ref_(self).flags().intersects(NodeFlags::JSDoc) {
            get_host_signature_from_jsdoc(node, self)
        } else {
            None
        };
        if let Some(host) = host.filter(|host| {
            is_function_expression(&host.ref_(self))
                && is_binary_expression(&host.ref_(self).parent().ref_(self))
                && get_assignment_declaration_kind(host.ref_(self).parent(), self)
                    == AssignmentDeclarationKind::PrototypeProperty
        }) {
            return Ok(self
                .get_declared_type_of_class_or_interface(
                    self.get_symbol_of_node(
                        host.ref_(self)
                            .parent()
                            .ref_(self)
                            .as_binary_expression()
                            .left,
                    )?
                    .unwrap()
                    .ref_(self)
                    .maybe_parent()
                    .unwrap(),
                )?
                .ref_(self)
                .as_interface_type()
                .maybe_this_type()
                .unwrap());
        }
        if self.is_js_constructor(Some(container))?
            && is_node_descendant_of(
                node,
                container
                    .ref_(self)
                    .as_function_like_declaration()
                    .maybe_body(),
                self,
            )
        {
            return Ok(self
                .get_declared_type_of_class_or_interface(
                    self.get_symbol_of_node(container)?.unwrap(),
                )?
                .ref_(self)
                .as_interface_type()
                .maybe_this_type()
                .unwrap());
        }
        self.error(
            Some(node),
            &Diagnostics::A_this_type_is_available_only_in_a_non_static_member_of_a_class_or_interface,
            None
        );
        Ok(self.error_type())
    }

    pub(super) fn get_type_from_this_type_node(
        &self,
        node: Id<Node>, /*ThisExpression | ThisTypeNode*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            links.ref_mut(self).resolved_type = Some(self.get_this_type(node)?);
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_type_from_rest_type_node(
        &self,
        node: Id<Node>, /*RestTypeNode | NamedTupleMember*/
    ) -> io::Result<Id<Type>> {
        let node_ref = node.ref_(self);
        let node_as_has_type = node_ref.as_has_type();
        self.get_type_from_type_node_(
            self.get_array_element_type_node(node_as_has_type.maybe_type().unwrap())
                .unwrap_or_else(|| node_as_has_type.maybe_type().unwrap()),
        )
    }

    pub(super) fn get_array_element_type_node(
        &self,
        node: Id<Node>, /*TypeNode*/
    ) -> Option<Id<Node /*TypeNode*/>> {
        match node.ref_(self).kind() {
            SyntaxKind::ParenthesizedType => {
                return self.get_array_element_type_node(
                    node.ref_(self).as_parenthesized_type_node().type_,
                );
            }
            SyntaxKind::TupleType => {
                let node_ref = node.ref_(self);
                let node_as_tuple_type_node = node_ref.as_tuple_type_node();
                if node_as_tuple_type_node.elements.ref_(self).len() == 1 {
                    let node = node_as_tuple_type_node.elements.ref_(self)[0];
                    if node.ref_(self).kind() == SyntaxKind::RestType
                        || node.ref_(self).kind() == SyntaxKind::NamedTupleMember
                            && node
                                .ref_(self)
                                .as_named_tuple_member()
                                .dot_dot_dot_token
                                .is_some()
                    {
                        return self.get_array_element_type_node(
                            node.ref_(self).as_has_type().maybe_type().unwrap(),
                        );
                    }
                }
            }
            SyntaxKind::ArrayType => {
                return Some(node.ref_(self).as_array_type_node().element_type);
            }
            _ => (),
        }
        None
    }

    pub(super) fn get_type_from_named_tuple_type_node(
        &self,
        node: Id<Node>, /*NamedTupleMember*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            let node_ref = node.ref_(self);
            let node_as_named_tuple_member = node_ref.as_named_tuple_member();
            links.ref_mut(self).resolved_type =
                Some(if node_as_named_tuple_member.dot_dot_dot_token.is_some() {
                    self.get_type_from_rest_type_node(node)?
                } else {
                    self.add_optionality(
                        self.get_type_from_type_node_(node_as_named_tuple_member.type_)?,
                        Some(true),
                        Some(node_as_named_tuple_member.question_token.is_some()),
                    )?
                });
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_type_from_type_node_(
        &self,
        node: Id<Node>, /*TypeNode*/
    ) -> io::Result<Id<Type>> {
        self.get_conditional_flow_type_of_type(self.get_type_from_type_node_worker(node)?, node)
    }

    pub(super) fn get_type_from_type_node_worker(
        &self,
        node: Id<Node>, /*TypeNode*/
    ) -> io::Result<Id<Type>> {
        Ok(match node.ref_(self).kind() {
            SyntaxKind::AnyKeyword | SyntaxKind::JSDocAllType | SyntaxKind::JSDocUnknownType => {
                self.any_type()
            }
            SyntaxKind::UnknownKeyword => self.unknown_type(),
            SyntaxKind::StringKeyword => self.string_type(),
            SyntaxKind::NumberKeyword => self.number_type(),
            SyntaxKind::BigIntKeyword => self.bigint_type(),
            SyntaxKind::BooleanKeyword => self.boolean_type(),
            SyntaxKind::SymbolKeyword => self.es_symbol_type(),
            SyntaxKind::VoidKeyword => self.void_type(),
            SyntaxKind::UndefinedKeyword => self.undefined_type(),
            SyntaxKind::NullKeyword => self.null_type(),
            SyntaxKind::NeverKeyword => self.never_type(),
            SyntaxKind::ObjectKeyword => {
                if node
                    .ref_(self)
                    .flags()
                    .intersects(NodeFlags::JavaScriptFile)
                    && !self.no_implicit_any
                {
                    self.any_type()
                } else {
                    self.non_primitive_type()
                }
            }
            SyntaxKind::IntrinsicKeyword => self.intrinsic_marker_type(),
            SyntaxKind::ThisType | SyntaxKind::ThisKeyword => {
                self.get_type_from_this_type_node(node)?
            }
            SyntaxKind::LiteralType => self.get_type_from_literal_type_node(node)?,
            SyntaxKind::TypeReference => self.get_type_from_type_reference(node)?,
            SyntaxKind::TypePredicate => {
                if node
                    .ref_(self)
                    .as_type_predicate_node()
                    .asserts_modifier
                    .is_some()
                {
                    self.void_type()
                } else {
                    self.boolean_type()
                }
            }
            SyntaxKind::ExpressionWithTypeArguments => self.get_type_from_type_reference(node)?,
            SyntaxKind::TypeQuery => self.get_type_from_type_query_node(node)?,
            SyntaxKind::ArrayType | SyntaxKind::TupleType => {
                self.get_type_from_array_or_tuple_type_node(node)?
            }
            SyntaxKind::OptionalType => self.get_type_from_optional_type_node(node)?,
            SyntaxKind::UnionType => self.get_type_from_union_type_node(node)?,
            SyntaxKind::IntersectionType => self.get_type_from_intersection_type_node(node)?,
            SyntaxKind::JSDocNullableType => self.get_type_from_jsdoc_nullable_type_node(node)?,
            SyntaxKind::JSDocOptionalType => self.add_optionality(
                self.get_type_from_type_node_(
                    node.ref_(self).as_base_jsdoc_unary_type().type_.unwrap(),
                )?,
                None,
                None,
            )?,
            SyntaxKind::NamedTupleMember => self.get_type_from_named_tuple_type_node(node)?,
            SyntaxKind::ParenthesizedType
            | SyntaxKind::JSDocNonNullableType
            | SyntaxKind::JSDocTypeExpression => {
                self.get_type_from_type_node_(node.ref_(self).as_has_type().maybe_type().unwrap())?
            }
            SyntaxKind::RestType => self.get_type_from_rest_type_node(node)?,
            SyntaxKind::JSDocVariadicType => self.get_type_from_jsdoc_variadic_type(node)?,
            SyntaxKind::FunctionType
            | SyntaxKind::ConstructorType
            | SyntaxKind::TypeLiteral
            | SyntaxKind::JSDocTypeLiteral
            | SyntaxKind::JSDocFunctionType
            | SyntaxKind::JSDocSignature => {
                self.get_type_from_type_literal_or_function_or_constructor_type_node(node)?
            }
            SyntaxKind::TypeOperator => self.get_type_from_type_operator_node(node)?,
            SyntaxKind::IndexedAccessType => self.get_type_from_indexed_access_type_node(node)?,
            SyntaxKind::MappedType => self.get_type_from_mapped_type_node(node)?,
            SyntaxKind::ConditionalType => self.get_type_from_conditional_type_node(node)?,
            SyntaxKind::InferType => self.get_type_from_infer_type_node(node)?,
            SyntaxKind::TemplateLiteralType => self.get_type_from_template_type_node(node)?,
            SyntaxKind::ImportType => self.get_type_from_import_type_node(node)?,
            SyntaxKind::Identifier
            | SyntaxKind::QualifiedName
            | SyntaxKind::PropertyAccessExpression => {
                let symbol = self.get_symbol_at_location_(node, None)?;
                if let Some(symbol) = symbol {
                    self.get_declared_type_of_symbol(symbol)?
                } else {
                    self.error_type()
                }
            }
            _ => self.error_type(),
        })
    }

    #[allow(dead_code)]
    pub(super) fn instantiate_list<TItem: PartialEq + Clone>(
        &self,
        items: Option<&[TItem]>,
        mapper: Option<Id<TypeMapper>>,
        mut instantiator: impl FnMut(&TItem, Option<Id<TypeMapper>>) -> TItem,
    ) -> Option<Vec<TItem>> {
        self.try_instantiate_list(items, mapper, |a: &TItem, b: Option<Id<TypeMapper>>| {
            Ok(instantiator(a, b))
        })
        .unwrap()
    }

    pub(super) fn try_instantiate_list<TItem: PartialEq + Clone>(
        &self,
        items: Option<&[TItem]>,
        mapper: Option<Id<TypeMapper>>,
        mut instantiator: impl FnMut(&TItem, Option<Id<TypeMapper>>) -> io::Result<TItem>,
    ) -> io::Result<Option<Vec<TItem>>> {
        let items = return_ok_none_if_none!(items);
        if !items.is_empty() {
            let mut i = 0;
            while i < items.len() {
                let item = &items[i];
                let mapped = instantiator(item, mapper.clone())?;
                if item != &mapped {
                    let mut result = if i == 0 {
                        vec![]
                    } else {
                        items[..i].to_owned()
                    };
                    result.push(mapped);
                    i += 1;
                    while i < items.len() {
                        result.push(instantiator(&items[i], mapper.clone())?);
                        i += 1;
                    }
                    return Ok(Some(result));
                }

                i += 1;
            }
        }
        Ok(Some(items.to_owned()))
    }

    pub(super) fn instantiate_types(
        &self,
        types: Option<&[Id<Type>]>,
        mapper: Option<Id<TypeMapper>>,
    ) -> io::Result<Option<Vec<Id<Type>>>> {
        self.try_instantiate_list(types, mapper, |&type_: &Id<Type>, mapper| {
            self.instantiate_type(type_, mapper)
        })
    }

    pub(super) fn instantiate_signatures(
        &self,
        signatures: &[Id<Signature>],
        mapper: Id<TypeMapper>,
    ) -> io::Result<Vec<Id<Signature>>> {
        Ok(self
            .try_instantiate_list(
                Some(signatures),
                Some(mapper),
                |signature: &Id<Signature>, mapper| {
                    Ok(self.alloc_signature(self.instantiate_signature(
                        signature.clone(),
                        mapper.unwrap(),
                        None,
                    )?))
                },
            )?
            .unwrap())
    }

    pub(super) fn instantiate_index_infos(
        &self,
        index_infos: &[Id<IndexInfo>],
        mapper: Id<TypeMapper>,
    ) -> io::Result<Vec<Id<IndexInfo>>> {
        Ok(self
            .try_instantiate_list(
                Some(index_infos),
                Some(mapper.clone()),
                |&index_info: &Id<IndexInfo>, mapper| {
                    self.instantiate_index_info(index_info, mapper.unwrap())
                },
            )?
            .unwrap())
    }

    pub(super) fn create_type_mapper(
        &self,
        sources: Vec<Id<Type /*TypeParameter*/>>,
        targets: Option<Vec<Id<Type>>>,
    ) -> Id<TypeMapper> {
        if sources.len() == 1 {
            self.make_unary_type_mapper(
                sources[0],
                targets.map_or_else(|| self.any_type(), |targets| targets[0]),
            )
        } else {
            self.make_array_type_mapper(sources, targets)
        }
    }

    pub(super) fn get_mapped_type(
        &self,
        type_: Id<Type>,
        mapper: Id<TypeMapper>,
    ) -> io::Result<Id<Type>> {
        Ok(
            if matches!(&*mapper.ref_(self), TypeMapper::Simple(mapper)) {
                let mapper_ref = mapper.ref_(self);
                if type_ == mapper_ref.as_simple().source {
                    mapper_ref.as_simple().target.clone()
                } else {
                    type_
                }
            } else if matches!(&*mapper.ref_(self), TypeMapper::Array(mapper)) {
                let mapper_ref = mapper.ref_(self);
                let sources = &mapper_ref.as_array().sources;
                let targets = &mapper_ref.as_array().targets;
                for (i, source) in sources.iter().enumerate() {
                    if type_ == *source {
                        return Ok(targets
                            .as_ref()
                            .map_or_else(|| self.any_type(), |targets| targets[i].clone()));
                    }
                }
                type_
            } else if matches!(&*mapper.ref_(self), TypeMapper::Function(mapper)) {
                let func = mapper.ref_(self).as_function().func.clone();
                func.ref_(self).call(self, type_)?
            } else {
                let (mapper_mapper1, mapper_mapper2) = match &*mapper.ref_(self) {
                    TypeMapper::Composite(composite_or_merged_mapper)
                    | TypeMapper::Merged(composite_or_merged_mapper) => (
                        composite_or_merged_mapper.mapper1,
                        composite_or_merged_mapper.mapper2,
                    ),
                    _ => unreachable!(),
                };
                let t1 = self.get_mapped_type(type_, mapper_mapper1)?;
                if t1 != type_ && matches!(&*mapper.ref_(self), TypeMapper::Composite(_)) {
                    self.instantiate_type(t1, Some(mapper_mapper2.clone()))?
                } else {
                    self.get_mapped_type(t1, mapper_mapper2)?
                }
            },
        )
    }

    pub(super) fn make_unary_type_mapper(
        &self,
        source: Id<Type>,
        target: Id<Type>,
    ) -> Id<TypeMapper> {
        self.alloc_type_mapper(TypeMapper::new_simple(source, target))
    }

    pub(super) fn make_array_type_mapper(
        &self,
        sources: Vec<Id<Type /*TypeParameter*/>>,
        targets: Option<Vec<Id<Type>>>,
    ) -> Id<TypeMapper> {
        self.alloc_type_mapper(TypeMapper::new_array(sources, targets))
    }

    pub(super) fn make_function_type_mapper(
        &self,
        func: impl TypeMapperCallback + 'static,
    ) -> Id<TypeMapper> {
        self.alloc_type_mapper(TypeMapper::new_function(func, self))
    }

    pub(super) fn make_composite_type_mapper(
        &self,
        mapper1: Id<TypeMapper>,
        mapper2: Id<TypeMapper>,
    ) -> Id<TypeMapper> {
        self.alloc_type_mapper(TypeMapper::new_composite(mapper1, mapper2))
    }

    pub(super) fn make_merged_type_mapper(
        &self,
        mapper1: Id<TypeMapper>,
        mapper2: Id<TypeMapper>,
    ) -> Id<TypeMapper> {
        self.alloc_type_mapper(TypeMapper::new_merged(mapper1, mapper2))
    }

    pub(super) fn create_type_eraser(
        &self,
        sources: Vec<Id<Type /*TypeParameter*/>>,
    ) -> Id<TypeMapper> {
        self.create_type_mapper(sources, None)
    }

    pub(super) fn create_backreference_mapper(
        &self,
        context: Id<InferenceContext>,
        index: usize,
    ) -> Id<TypeMapper> {
        self.make_function_type_mapper(BackreferenceMapperCallback::new(&context.ref_(self), index))
    }

    pub(super) fn combine_type_mappers(
        &self,
        mapper1: Option<Id<TypeMapper>>,
        mapper2: Id<TypeMapper>,
    ) -> Id<TypeMapper> {
        if let Some(mapper1) = mapper1 {
            self.make_composite_type_mapper(mapper1, mapper2)
        } else {
            mapper2
        }
    }

    pub(super) fn merge_type_mappers(
        &self,
        mapper1: Option<Id<TypeMapper>>,
        mapper2: Id<TypeMapper>,
    ) -> Id<TypeMapper> {
        if let Some(mapper1) = mapper1 {
            self.make_merged_type_mapper(mapper1, mapper2)
        } else {
            mapper2
        }
    }

    pub(super) fn prepend_type_mapping(
        &self,
        source: Id<Type>,
        target: Id<Type>,
        mapper: Option<Id<TypeMapper>>,
    ) -> Id<TypeMapper> {
        match mapper {
            None => self.make_unary_type_mapper(source, target),
            Some(mapper) => {
                self.make_merged_type_mapper(self.make_unary_type_mapper(source, target), mapper)
            }
        }
    }

    pub(super) fn append_type_mapping(
        &self,
        mapper: Option<Id<TypeMapper>>,
        source: Id<Type>,
        target: Id<Type>,
    ) -> Id<TypeMapper> {
        match mapper {
            None => self.make_unary_type_mapper(source, target),
            Some(mapper) => {
                self.make_merged_type_mapper(mapper, self.make_unary_type_mapper(source, target))
            }
        }
    }

    pub(super) fn get_restrictive_type_parameter(
        &self,
        tp: Id<Type>, /*TypeParameter*/
    ) -> Id<Type> {
        if matches!(
            tp.ref_(self).as_type_parameter().maybe_constraint(),
            Some(constraint) if constraint == self.unknown_type()
        ) {
            tp
        } else {
            if tp.ref_(self).maybe_restrictive_instantiation().is_none() {
                let restrictive_instantiation = self.alloc_type(
                    self.create_type_parameter({
                        let symbol = tp.ref_(self).maybe_symbol();
                        symbol
                    })
                    .into(),
                );
                tp.ref_(self)
                    .set_restrictive_instantiation(Some(restrictive_instantiation));
                restrictive_instantiation
                    .ref_(self)
                    .as_type_parameter()
                    .set_constraint(Some(self.unknown_type()));
            }
            tp.ref_(self).maybe_restrictive_instantiation().unwrap()
        }
    }

    pub(super) fn clone_type_parameter(
        &self,
        type_parameter: Id<Type>, /*TypeParameter*/
    ) -> Id<Type /*TypeParameter*/> {
        let mut result = self.create_type_parameter(Some(type_parameter.ref_(self).symbol()));
        result.target = Some(type_parameter);
        self.alloc_type(result.into())
    }

    pub(super) fn instantiate_type_predicate(
        &self,
        predicate: Id<TypePredicate>,
        mapper: Id<TypeMapper>,
    ) -> io::Result<TypePredicate> {
        Ok(self.create_type_predicate(
            predicate.ref_(self).kind,
            predicate.ref_(self).parameter_name.clone(),
            predicate.ref_(self).parameter_index,
            self.maybe_instantiate_type(predicate.ref_(self).type_, Some(mapper))?,
        ))
    }

    pub(super) fn instantiate_signature(
        &self,
        signature: Id<Signature>,
        mut mapper: Id<TypeMapper>,
        erase_type_parameters: Option<bool>,
    ) -> io::Result<Signature> {
        let erase_type_parameters = erase_type_parameters.unwrap_or(false);
        let mut fresh_type_parameters: Option<Vec<Id<Type /*TypeParameter*/>>> = None;
        if let Some(signature_type_parameters) =
            signature.ref_(self).maybe_type_parameters().clone()
        {
            if !erase_type_parameters {
                fresh_type_parameters =
                    Some(map(&signature_type_parameters, |&type_parameter, _| {
                        self.clone_type_parameter(type_parameter)
                    }));
                mapper = self.combine_type_mappers(
                    Some(self.create_type_mapper(
                        signature_type_parameters,
                        fresh_type_parameters.clone(),
                    )),
                    mapper,
                );
                for &tp in fresh_type_parameters.as_ref().unwrap() {
                    tp.ref_(self).as_type_parameter().set_mapper(mapper.clone());
                }
            }
        }
        let mut result = self.create_signature(
            signature.ref_(self).declaration.clone(),
            fresh_type_parameters,
            signature
                .ref_(self)
                .maybe_this_parameter()
                .try_map(|this_parameter| {
                    self.instantiate_symbol(this_parameter, mapper.clone())
                })?,
            self.try_instantiate_list(
                Some(signature.ref_(self).parameters()),
                Some(mapper.clone()),
                |&parameter, mapper| self.instantiate_symbol(parameter, mapper.unwrap()),
            )?
            .unwrap(),
            None,
            None,
            signature.ref_(self).min_argument_count(),
            signature.ref_(self).flags & SignatureFlags::PropagatingFlags,
        );
        result.target = Some(signature);
        result.mapper = Some(mapper);
        Ok(result)
    }

    pub(super) fn instantiate_symbol(
        &self,
        mut symbol: Id<Symbol>,
        mut mapper: Id<TypeMapper>,
    ) -> io::Result<Id<Symbol>> {
        let links = self.get_symbol_links(symbol);
        {
            if let Some(type_) = links.ref_(self).type_ {
                if !self.could_contain_type_variables(type_)? {
                    return Ok(symbol);
                }
            }
        }
        if get_check_flags(&symbol.ref_(self)).intersects(CheckFlags::Instantiated) {
            symbol = links.ref_(self).target.unwrap();
            mapper = self.combine_type_mappers(links.ref_(self).mapper, mapper);
        }
        let result = self.create_symbol(
            symbol.ref_(self).flags(),
            symbol.ref_(self).escaped_name().to_owned(),
            Some(
                CheckFlags::Instantiated
                    | get_check_flags(&symbol.ref_(self))
                        & (CheckFlags::Readonly
                            | CheckFlags::Late
                            | CheckFlags::OptionalParameter
                            | CheckFlags::RestParameter),
            ),
        );
        if let Some(declarations) = &*symbol.ref_(self).maybe_declarations() {
            result.set_declarations(declarations.clone());
        }
        result.set_parent(symbol.ref_(self).maybe_parent());
        let result_links = result.symbol_links();
        let mut result_links = result_links.ref_mut(self);
        result_links.target = Some(symbol.clone());
        result_links.mapper = Some(mapper);
        if let Some(symbol_value_declaration) = symbol.ref_(self).maybe_value_declaration() {
            result.set_value_declaration(symbol_value_declaration);
        }
        if let Some(links_name_type) = links.ref_(self).name_type {
            result_links.name_type = Some(links_name_type);
        }
        Ok(self.alloc_symbol(result.into()))
    }

    pub(super) fn get_object_type_instantiation(
        &self,
        type_: Id<Type>, /*AnonymousType | DeferredTypeReference*/
        mapper: Id<TypeMapper>,
        alias_symbol: Option<Id<Symbol>>,
        alias_type_arguments: Option<&[Id<Type>]>,
    ) -> io::Result<Id<Type>> {
        let declaration = if type_
            .ref_(self)
            .as_object_type()
            .object_flags()
            .intersects(ObjectFlags::Reference)
        {
            type_
                .ref_(self)
                .as_type_reference()
                .maybe_node()
                .clone()
                .unwrap()
        } else {
            type_
                .ref_(self)
                .symbol()
                .ref_(self)
                .maybe_declarations()
                .clone()
                .unwrap()[0]
                .clone()
        };
        let links = self.get_node_links(declaration);
        let target = if type_
            .ref_(self)
            .as_object_type()
            .object_flags()
            .intersects(ObjectFlags::Reference)
        {
            links.ref_(self).resolved_type.clone().unwrap()
        } else if type_
            .ref_(self)
            .as_object_type()
            .object_flags()
            .intersects(ObjectFlags::Instantiated)
        {
            type_.ref_(self).as_object_type().maybe_target().unwrap()
        } else {
            type_
        };
        let mut type_parameters = links.ref_(self).outer_type_parameters.clone();
        if type_parameters.is_none() {
            let mut outer_type_parameters =
                self.get_outer_type_parameters(declaration, Some(true))?;
            if self.is_js_constructor(Some(declaration))? {
                let template_tag_parameters =
                    self.get_type_parameters_from_declaration(declaration);
                outer_type_parameters = maybe_add_range(
                    outer_type_parameters,
                    template_tag_parameters.as_deref(),
                    None,
                    None,
                );
            }
            type_parameters = Some(outer_type_parameters.unwrap_or_else(|| vec![]));
            let all_declarations = if type_
                .ref_(self)
                .as_object_type()
                .object_flags()
                .intersects(ObjectFlags::Reference)
            {
                vec![declaration.clone()]
            } else {
                type_
                    .ref_(self)
                    .symbol()
                    .ref_(self)
                    .maybe_declarations()
                    .clone()
                    .unwrap()
            };
            type_parameters = if (target
                .ref_(self)
                .as_object_type()
                .object_flags()
                .intersects(ObjectFlags::Reference)
                || target
                    .ref_(self)
                    .symbol()
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::Method)
                || target
                    .ref_(self)
                    .symbol()
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::TypeLiteral))
                && target.ref_(self).maybe_alias_type_arguments().is_none()
            {
                try_maybe_filter(type_parameters.as_deref(), |&tp: &Id<Type>| {
                    try_some(
                        Some(&*all_declarations),
                        Some(|&d: &Id<Node>| self.is_type_parameter_possibly_referenced(tp, d)),
                    )
                })
                .transpose()?
            } else {
                type_parameters
            };
            links.ref_mut(self).outer_type_parameters = type_parameters.clone();
        }
        let type_parameters = type_parameters.unwrap();
        if !type_parameters.is_empty() {
            let combined_mapper = self.combine_type_mappers(
                type_.ref_(self).as_object_type().maybe_mapper(),
                mapper.clone(),
            );
            let type_arguments = try_map(&type_parameters, |&t: &Id<Type>, _| {
                self.get_mapped_type(t, combined_mapper)
            })?;
            let new_alias_symbol = alias_symbol
                .clone()
                .or_else(|| type_.ref_(self).maybe_alias_symbol().clone());
            let new_alias_type_arguments = if alias_symbol.is_some() {
                alias_type_arguments.map(ToOwned::to_owned)
            } else {
                self.instantiate_types(
                    type_.ref_(self).maybe_alias_type_arguments().as_deref(),
                    Some(mapper),
                )?
            };
            let id = format!(
                "{}{}",
                self.get_type_list_id(Some(&*type_arguments)),
                self.get_alias_id(new_alias_symbol, new_alias_type_arguments.as_deref())
            );
            if target
                .ref_(self)
                .as_object_type()
                .maybe_instantiations()
                .is_none()
            {
                *target.ref_(self).as_object_type().maybe_instantiations() = Some(HashMap::new());
                target
                    .ref_(self)
                    .as_object_type()
                    .maybe_instantiations()
                    .as_mut()
                    .unwrap()
                    .insert(
                        format!(
                            "{}{}",
                            self.get_type_list_id(Some(&*type_parameters)),
                            self.get_alias_id(
                                target.ref_(self).maybe_alias_symbol(),
                                target.ref_(self).maybe_alias_type_arguments().as_deref()
                            )
                        ),
                        target.clone(),
                    );
            }
            let mut result = target
                .ref_(self)
                .as_object_type()
                .maybe_instantiations()
                .as_ref()
                .unwrap()
                .get(&id)
                .map(Clone::clone);
            if result.is_none() {
                let new_mapper = self.create_type_mapper(type_parameters, Some(type_arguments));
                result = Some(
                    if target
                        .ref_(self)
                        .as_object_type()
                        .object_flags()
                        .intersects(ObjectFlags::Reference)
                    {
                        self.create_deferred_type_reference(
                            {
                                let target = type_.ref_(self).as_type_reference().target;
                                target
                            },
                            {
                                let node = type_
                                    .ref_(self)
                                    .as_type_reference()
                                    .maybe_node()
                                    .clone()
                                    .unwrap();
                                node
                            },
                            Some(new_mapper),
                            new_alias_symbol,
                            new_alias_type_arguments.as_deref(),
                        )?
                    } else if target
                        .ref_(self)
                        .as_object_type()
                        .object_flags()
                        .intersects(ObjectFlags::Mapped)
                    {
                        self.instantiate_mapped_type(
                            target,
                            new_mapper,
                            new_alias_symbol,
                            new_alias_type_arguments.as_deref(),
                        )?
                    } else {
                        self.instantiate_anonymous_type(
                            target,
                            new_mapper,
                            new_alias_symbol,
                            new_alias_type_arguments.as_deref(),
                        )?
                    },
                );
                target
                    .ref_(self)
                    .as_object_type()
                    .maybe_instantiations()
                    .as_mut()
                    .unwrap()
                    .insert(id, result.clone().unwrap());
            }
            return Ok(result.unwrap());
        }
        Ok(type_)
    }
}

struct BackreferenceMapperCallback {
    context_inferences: Vec<Id<InferenceInfo>>,
    index: usize,
}

impl BackreferenceMapperCallback {
    pub fn new(context: &InferenceContext, index: usize) -> Self {
        Self {
            context_inferences: context.inferences().clone(),
            index,
        }
    }
}

impl TypeMapperCallback for BackreferenceMapperCallback {
    fn call(&self, checker: &TypeChecker, t: Id<Type>) -> io::Result<Id<Type>> {
        Ok(
            if matches!(
                find_index(&self.context_inferences, |info: &Id<InferenceInfo>, _| info.ref_(checker).type_parameter == t, None),
                Some(found_index) if found_index >= self.index
            ) {
                checker.unknown_type()
            } else {
                t
            },
        )
    }
}
