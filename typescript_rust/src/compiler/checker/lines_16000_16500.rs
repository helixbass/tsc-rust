#![allow(non_upper_case_globals)]

use gc::{Finalize, Gc, Trace};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use crate::{
    find_index, get_assignment_declaration_kind, get_check_flags, get_host_signature_from_jsdoc,
    get_symbol_id, get_this_container, is_binary_expression, is_class_like,
    is_constructor_declaration, is_function_expression, is_node_descendant_of,
    is_object_literal_expression, is_private_identifier_class_element_declaration, is_static,
    is_valid_es_symbol_declaration, map, maybe_add_range, maybe_filter, maybe_is_class_like,
    pseudo_big_int_to_string, some, AsDoubleDeref, AssignmentDeclarationKind, BaseLiteralType,
    BigIntLiteralType, CheckFlags, Diagnostics, FunctionLikeDeclarationInterface, GcVec, IndexInfo,
    InferenceContext, InferenceInfo, InterfaceTypeInterface, IntrinsicType, LiteralTypeInterface,
    Node, NodeFlags, NodeInterface, Number, NumberLiteralType, ObjectFlags,
    ObjectFlagsTypeInterface, ObjectTypeInterface, PseudoBigInt, Signature, SignatureFlags,
    StringLiteralType, StringOrNumber, Symbol, SymbolFlags, SymbolInterface, SyntaxKind,
    TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
    TypeMapperCallback, TypePredicate, TypeReferenceInterface, UniqueESSymbolType, __String,
};
use local_macros::enum_unwrapped;

impl TypeChecker {
    pub(super) fn is_spreadable_property(&self, prop: &Symbol) -> bool {
        !some(
            prop.maybe_declarations().as_deref(),
            Some(|declaration: &Gc<Node>| {
                is_private_identifier_class_element_declaration(declaration)
            }),
        ) && (!prop
            .flags()
            .intersects(SymbolFlags::Method | SymbolFlags::GetAccessor | SymbolFlags::SetAccessor)
            || !matches!(
                prop.maybe_declarations().as_ref(),
                Some(prop_declarations) if prop_declarations.iter().any(|decl: &Gc<Node>| maybe_is_class_like(decl.maybe_parent()))
            ))
    }

    pub(super) fn get_spread_symbol(&self, prop: &Symbol, readonly: bool) -> Gc<Symbol> {
        let is_setonly_accessor = prop.flags().intersects(SymbolFlags::SetAccessor)
            && !prop.flags().intersects(SymbolFlags::GetAccessor);
        if !is_setonly_accessor && readonly == self.is_readonly_symbol(prop) {
            return prop.symbol_wrapper();
        }
        let flags = SymbolFlags::Property | (prop.flags() & SymbolFlags::Optional);
        let result: Gc<Symbol> = self
            .create_symbol(
                flags,
                prop.escaped_name().to_owned(),
                Some(
                    self.get_is_late_check_flag(prop)
                        | if readonly {
                            CheckFlags::Readonly
                        } else {
                            CheckFlags::None
                        },
                ),
            )
            .into();
        let result_links = result.as_transient_symbol().symbol_links();
        let mut result_links = result_links.borrow_mut();
        result_links.type_ = Some(if is_setonly_accessor {
            self.undefined_type()
        } else {
            self.get_type_of_symbol(prop)
        });
        if let Some(prop_declarations) = prop.maybe_declarations().clone() {
            result.set_declarations(prop_declarations);
        }
        result_links.name_type = (*self.get_symbol_links(prop)).borrow().name_type.clone();
        result_links.synthetic_origin = Some(prop.symbol_wrapper());
        result
    }

    pub(super) fn get_index_info_with_readonly(
        &self,
        info: &Gc<IndexInfo>,
        readonly: bool,
    ) -> Gc<IndexInfo> {
        if info.is_readonly != readonly {
            Gc::new(self.create_index_info(
                info.key_type.clone(),
                info.type_.clone(),
                readonly,
                info.declaration.clone(),
            ))
        } else {
            info.clone()
        }
    }

    // pub fn create_literal_type(
    pub fn create_string_literal_type<TSymbol: Borrow<Symbol>, TRegularType: Borrow<Type>>(
        &self,
        flags: TypeFlags,
        value: String,
        symbol: Option<TSymbol>,
        regular_type: Option<TRegularType>,
    ) -> Gc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Gc<Type> = StringLiteralType::new(type_, value).into();
        type_.set_symbol(symbol.map(|symbol| symbol.borrow().symbol_wrapper()));
        type_
            .as_literal_type()
            .set_regular_type(if let Some(regular_type) = regular_type {
                regular_type.borrow().type_wrapper()
            } else {
                type_.clone()
            });
        type_
    }

    pub fn create_number_literal_type<TSymbol: Borrow<Symbol>, TRegularType: Borrow<Type>>(
        &self,
        flags: TypeFlags,
        value: Number,
        symbol: Option<TSymbol>,
        regular_type: Option<TRegularType>,
    ) -> Gc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Gc<Type> = NumberLiteralType::new(type_, value).into();
        type_.set_symbol(symbol.map(|symbol| symbol.borrow().symbol_wrapper()));
        type_
            .as_literal_type()
            .set_regular_type(if let Some(regular_type) = regular_type {
                regular_type.borrow().type_wrapper()
            } else {
                type_.clone()
            });
        type_
    }

    pub fn create_big_int_literal_type<TSymbol: Borrow<Symbol>, TRegularType: Borrow<Type>>(
        &self,
        flags: TypeFlags,
        value: PseudoBigInt,
        symbol: Option<TSymbol>,
        regular_type: Option<TRegularType>,
    ) -> Gc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Gc<Type> = BigIntLiteralType::new(type_, value).into();
        type_.set_symbol(symbol.map(|symbol| symbol.borrow().symbol_wrapper()));
        type_
            .as_literal_type()
            .set_regular_type(if let Some(regular_type) = regular_type {
                regular_type.borrow().type_wrapper()
            } else {
                type_.clone()
            });
        type_
    }

    pub(super) fn get_fresh_type_of_literal_type(&self, type_: &Type) -> Gc<Type> {
        if type_.flags().intersects(TypeFlags::Literal) {
            return match type_ {
                Type::LiteralType(type_) => type_.get_or_initialize_fresh_type(self),
                Type::IntrinsicType(IntrinsicType::FreshableIntrinsicType(type_)) => {
                    type_.fresh_type()
                }
                _ => unreachable!(),
            };
        }
        type_.type_wrapper()
    }

    pub(super) fn get_regular_type_of_literal_type(&self, type_: &Type) -> Gc<Type> {
        if type_.flags().intersects(TypeFlags::Literal) {
            // TODO: this seems like it should be encapsulated behind an abstraction (also above in
            // get_fresh_type_of_literal_type())?
            match type_ {
                Type::LiteralType(type_) => type_.regular_type(),
                Type::IntrinsicType(IntrinsicType::FreshableIntrinsicType(type_)) => {
                    type_.regular_type()
                }
                _ => unreachable!(),
            }
        } else if type_.flags().intersects(TypeFlags::Union) {
            let type_as_union_type = type_.as_union_type();
            if type_as_union_type.maybe_regular_type().is_none() {
                *type_as_union_type.maybe_regular_type() = Some(
                    self.map_type(
                        type_,
                        &mut |type_| Some(self.get_regular_type_of_literal_type(type_)),
                        None,
                    )
                    .unwrap(),
                );
            }
            type_as_union_type.maybe_regular_type().clone().unwrap()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn is_fresh_literal_type(&self, type_: &Type) -> bool {
        if !type_.flags().intersects(TypeFlags::Literal) {
            return false;
        }
        // TODO: should this be using eg a Type.as_has_fresh_type() "unwrapper-helper" instead?
        // (same question in is_type_related_to() and get_normalized_type() below, and in
        // remove_redundant_literal_types()) or maybe this looks like it should be a trait that
        // includes `maybe_fresh_type()` that both of these implement?
        match type_ {
            Type::IntrinsicType(intrinsic_type) => ptr::eq(
                type_,
                &*enum_unwrapped!(intrinsic_type, [IntrinsicType, FreshableIntrinsicType])
                    .fresh_type(),
            ),
            Type::LiteralType(literal_type) => {
                matches!(
                    literal_type.fresh_type(),
                    Some(fresh_type) if ptr::eq(type_, &*fresh_type)
                )
            }
            _ => panic!("Expected IntrinsicType or LiteralType"),
        }
    }

    pub(super) fn get_string_literal_type(&self, value: &str) -> Gc<Type> {
        let mut string_literal_types = self.string_literal_types();
        if string_literal_types.contains_key(value) {
            return string_literal_types.get(value).unwrap().clone();
        }
        let type_ = self.create_string_literal_type(
            TypeFlags::StringLiteral,
            value.to_owned(),
            Option::<&Symbol>::None,
            Option::<&Type>::None,
        );
        string_literal_types.insert(value.to_owned(), type_.clone());
        type_
    }

    pub(super) fn get_number_literal_type(&self, value: Number) -> Gc<Type> {
        let mut number_literal_types = self.number_literal_types();
        if number_literal_types.contains_key(&value) {
            return number_literal_types.get(&value).unwrap().clone();
        }
        let type_ = self.create_number_literal_type(
            TypeFlags::NumberLiteral,
            value,
            Option::<&Symbol>::None,
            Option::<&Type>::None,
        );
        number_literal_types.insert(value, type_.clone());
        type_
    }

    pub(super) fn get_big_int_literal_type(&self, value: PseudoBigInt) -> Gc<Type> {
        let key = pseudo_big_int_to_string(&value);
        let mut big_int_literal_types = self.big_int_literal_types();
        if big_int_literal_types.contains_key(&key) {
            return big_int_literal_types.get(&key).unwrap().clone();
        }
        let type_ = self.create_big_int_literal_type(
            TypeFlags::BigIntLiteral,
            value,
            Option::<&Symbol>::None,
            Option::<&Type>::None,
        );
        big_int_literal_types.insert(key, type_.clone());
        type_
    }

    pub(super) fn get_enum_literal_type(
        &self,
        value: StringOrNumber,
        enum_id: usize,
        symbol: &Symbol,
    ) -> Gc<Type /*LiteralType*/> {
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
        let type_: Gc<Type> = match value {
            StringOrNumber::String(value) => self.create_string_literal_type(
                TypeFlags::EnumLiteral | TypeFlags::StringLiteral,
                value,
                Some(symbol),
                Option::<&Type>::None,
            ),
            StringOrNumber::Number(value) => self.create_number_literal_type(
                TypeFlags::EnumLiteral | TypeFlags::NumberLiteral,
                value,
                Some(symbol),
                Option::<&Type>::None,
            ),
        };
        enum_literal_types.insert(key, type_.clone());
        type_
    }

    pub(super) fn get_type_from_literal_type_node(
        &self,
        node: &Node, /*LiteralTypeNode*/
    ) -> Gc<Type> {
        let node_as_literal_type_node = node.as_literal_type_node();
        if node_as_literal_type_node.literal.kind() == SyntaxKind::NullKeyword {
            return self.null_type();
        }
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_type.is_none() {
            links.borrow_mut().resolved_type = Some(self.get_regular_type_of_literal_type(
                &self.check_expression(&node_as_literal_type_node.literal, None, None),
            ));
        }
        let ret = (*links).borrow().resolved_type.clone().unwrap();
        ret
    }

    pub(super) fn create_unique_es_symbol_type(
        &self,
        symbol: &Symbol,
    ) -> Gc<Type /*UniqueESSymbolType*/> {
        let type_ = self.create_type(TypeFlags::UniqueESSymbol);
        let type_: Gc<Type> = UniqueESSymbolType::new(
            type_,
            symbol.symbol_wrapper(),
            format!("__@{}@{}", symbol.escaped_name(), get_symbol_id(symbol)),
        )
        .into();
        type_
    }

    pub(super) fn get_es_symbol_like_type_for_node(&self, node: &Node) -> Gc<Type> {
        if is_valid_es_symbol_declaration(node) {
            let symbol = self.get_symbol_of_node(node).unwrap();
            let links = self.get_symbol_links(&symbol);
            if (*links).borrow().unique_es_symbol_type.is_none() {
                links.borrow_mut().unique_es_symbol_type =
                    Some(self.create_unique_es_symbol_type(&symbol));
            }
            return (*links).borrow().unique_es_symbol_type.clone().unwrap();
        }
        self.es_symbol_type()
    }

    pub(super) fn get_this_type(&self, node: &Node) -> Gc<Type> {
        let container = get_this_container(node, false);
        let parent = /*container &&*/ container.maybe_parent();
        if let Some(parent) = parent.as_ref().filter(|parent| {
            is_class_like(parent) || parent.kind() == SyntaxKind::InterfaceDeclaration
        }) {
            if !is_static(&container)
                && (!is_constructor_declaration(&container)
                    || is_node_descendant_of(
                        node,
                        container.as_constructor_declaration().maybe_body(),
                    ))
            {
                return self
                    .get_declared_type_of_class_or_interface(
                        &self.get_symbol_of_node(parent).unwrap(),
                    )
                    .as_interface_type()
                    .maybe_this_type()
                    .unwrap();
            }
        }

        if let Some(parent) = parent.as_ref().filter(|parent| {
            is_object_literal_expression(parent)
                && is_binary_expression(&parent.parent())
                && get_assignment_declaration_kind(&parent.parent())
                    == AssignmentDeclarationKind::Prototype
        }) {
            return self
                .get_declared_type_of_class_or_interface(
                    &self
                        .get_symbol_of_node(&parent.parent().as_binary_expression().left)
                        .unwrap()
                        .maybe_parent()
                        .unwrap(),
                )
                .as_interface_type()
                .maybe_this_type()
                .unwrap();
        }
        let host = if node.flags().intersects(NodeFlags::JSDoc) {
            get_host_signature_from_jsdoc(node)
        } else {
            None
        };
        if let Some(host) = host.as_ref().filter(|host| {
            is_function_expression(host)
                && is_binary_expression(&host.parent())
                && get_assignment_declaration_kind(&host.parent())
                    == AssignmentDeclarationKind::PrototypeProperty
        }) {
            return self
                .get_declared_type_of_class_or_interface(
                    &self
                        .get_symbol_of_node(&host.parent().as_binary_expression().left)
                        .unwrap()
                        .maybe_parent()
                        .unwrap(),
                )
                .as_interface_type()
                .maybe_this_type()
                .unwrap();
        }
        if self.is_js_constructor(Some(&*container))
            && is_node_descendant_of(node, container.as_function_like_declaration().maybe_body())
        {
            return self
                .get_declared_type_of_class_or_interface(
                    &self.get_symbol_of_node(&container).unwrap(),
                )
                .as_interface_type()
                .maybe_this_type()
                .unwrap();
        }
        self.error(
            Some(node),
            &Diagnostics::A_this_type_is_available_only_in_a_non_static_member_of_a_class_or_interface,
            None
        );
        self.error_type()
    }

    pub(super) fn get_type_from_this_type_node(
        &self,
        node: &Node, /*ThisExpression | ThisTypeNode*/
    ) -> Gc<Type> {
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_type.is_none() {
            links.borrow_mut().resolved_type = Some(self.get_this_type(node));
        }
        let ret = (*links).borrow().resolved_type.clone().unwrap();
        ret
    }

    pub(super) fn get_type_from_rest_type_node(
        &self,
        node: &Node, /*RestTypeNode | NamedTupleMember*/
    ) -> Gc<Type> {
        let node_as_has_type = node.as_has_type();
        self.get_type_from_type_node_(
            &self
                .get_array_element_type_node(&node_as_has_type.maybe_type().unwrap())
                .unwrap_or_else(|| node_as_has_type.maybe_type().unwrap()),
        )
    }

    pub(super) fn get_array_element_type_node(
        &self,
        node: &Node, /*TypeNode*/
    ) -> Option<Gc<Node /*TypeNode*/>> {
        match node.kind() {
            SyntaxKind::ParenthesizedType => {
                return self.get_array_element_type_node(&node.as_parenthesized_type_node().type_);
            }
            SyntaxKind::TupleType => {
                let node_as_tuple_type_node = node.as_tuple_type_node();
                if node_as_tuple_type_node.elements.len() == 1 {
                    let node = &node_as_tuple_type_node.elements[0];
                    if node.kind() == SyntaxKind::RestType
                        || node.kind() == SyntaxKind::NamedTupleMember
                            && node.as_named_tuple_member().dot_dot_dot_token.is_some()
                    {
                        return self.get_array_element_type_node(
                            &node.as_has_type().maybe_type().unwrap(),
                        );
                    }
                }
            }
            SyntaxKind::ArrayType => {
                return Some(node.as_array_type_node().element_type.clone());
            }
            _ => (),
        }
        None
    }

    pub(super) fn get_type_from_named_tuple_type_node(
        &self,
        node: &Node, /*NamedTupleMember*/
    ) -> Gc<Type> {
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_type.is_none() {
            let node_as_named_tuple_member = node.as_named_tuple_member();
            links.borrow_mut().resolved_type =
                Some(if node_as_named_tuple_member.dot_dot_dot_token.is_some() {
                    self.get_type_from_rest_type_node(node)
                } else {
                    self.add_optionality(
                        &self.get_type_from_type_node_(&node_as_named_tuple_member.type_),
                        Some(true),
                        Some(node_as_named_tuple_member.question_token.is_some()),
                    )
                });
        }
        let ret = (*links).borrow().resolved_type.clone().unwrap();
        ret
    }

    pub(super) fn get_type_from_type_node_(&self, node: &Node /*TypeNode*/) -> Gc<Type> {
        self.get_conditional_flow_type_of_type(&self.get_type_from_type_node_worker(node), node)
    }

    pub(super) fn get_type_from_type_node_worker(&self, node: &Node /*TypeNode*/) -> Gc<Type> {
        match node.kind() {
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
                if node.flags().intersects(NodeFlags::JavaScriptFile) && !self.no_implicit_any {
                    self.any_type()
                } else {
                    self.non_primitive_type()
                }
            }
            SyntaxKind::IntrinsicKeyword => self.intrinsic_marker_type(),
            SyntaxKind::ThisType | SyntaxKind::ThisKeyword => {
                self.get_type_from_this_type_node(node)
            }
            SyntaxKind::LiteralType => self.get_type_from_literal_type_node(node),
            SyntaxKind::TypeReference => self.get_type_from_type_reference(node),
            SyntaxKind::TypePredicate => {
                if node.as_type_predicate_node().asserts_modifier.is_some() {
                    self.void_type()
                } else {
                    self.boolean_type()
                }
            }
            SyntaxKind::ExpressionWithTypeArguments => self.get_type_from_type_reference(node),
            SyntaxKind::TypeQuery => self.get_type_from_type_query_node(node),
            SyntaxKind::ArrayType | SyntaxKind::TupleType => {
                self.get_type_from_array_or_tuple_type_node(node)
            }
            SyntaxKind::OptionalType => self.get_type_from_optional_type_node(node),
            SyntaxKind::UnionType => self.get_type_from_union_type_node(node),
            SyntaxKind::IntersectionType => self.get_type_from_intersection_type_node(node),
            SyntaxKind::JSDocNullableType => self.get_type_from_jsdoc_nullable_type_node(node),
            SyntaxKind::JSDocOptionalType => self.add_optionality(
                &self.get_type_from_type_node_(
                    node.as_base_jsdoc_unary_type().type_.as_deref().unwrap(),
                ),
                None,
                None,
            ),
            SyntaxKind::NamedTupleMember => self.get_type_from_named_tuple_type_node(node),
            SyntaxKind::ParenthesizedType
            | SyntaxKind::JSDocNonNullableType
            | SyntaxKind::JSDocTypeExpression => {
                self.get_type_from_type_node_(&node.as_has_type().maybe_type().unwrap())
            }
            SyntaxKind::RestType => self.get_type_from_rest_type_node(node),
            SyntaxKind::JSDocVariadicType => self.get_type_from_jsdoc_variadic_type(node),
            SyntaxKind::FunctionType
            | SyntaxKind::ConstructorType
            | SyntaxKind::TypeLiteral
            | SyntaxKind::JSDocTypeLiteral
            | SyntaxKind::JSDocFunctionType
            | SyntaxKind::JSDocSignature => {
                self.get_type_from_type_literal_or_function_or_constructor_type_node(node)
            }
            SyntaxKind::TypeOperator => self.get_type_from_type_operator_node(node),
            SyntaxKind::IndexedAccessType => self.get_type_from_indexed_access_type_node(node),
            SyntaxKind::MappedType => self.get_type_from_mapped_type_node(node),
            SyntaxKind::ConditionalType => self.get_type_from_conditional_type_node(node),
            SyntaxKind::InferType => self.get_type_from_infer_type_node(node),
            SyntaxKind::TemplateLiteralType => self.get_type_from_template_type_node(node),
            SyntaxKind::ImportType => self.get_type_from_import_type_node(node),
            SyntaxKind::Identifier
            | SyntaxKind::QualifiedName
            | SyntaxKind::PropertyAccessExpression => {
                let symbol = self.get_symbol_at_location_(node, None);
                if let Some(symbol) = symbol.as_ref() {
                    self.get_declared_type_of_symbol(symbol)
                } else {
                    self.error_type()
                }
            }
            _ => self.error_type(),
        }
    }

    pub(super) fn instantiate_list<TItem: Trace + Finalize>(
        &self,
        items: Option<GcVec<Gc<TItem>>>,
        mapper: Option<Gc<TypeMapper>>,
        mut instantiator: impl FnMut(&Gc<TItem>, Option<Gc<TypeMapper>>) -> Gc<TItem>,
    ) -> Option<GcVec<Gc<TItem>>> {
        let items = items?;
        if !items.is_empty() {
            let mut i = 0;
            while i < items.len() {
                let item = &items[i];
                let mapped = instantiator(item, mapper.clone());
                if !Gc::ptr_eq(item, &mapped) {
                    let mut result = if i == 0 {
                        vec![]
                    } else {
                        items[..i].to_owned()
                    };
                    result.push(mapped);
                    i += 1;
                    while i < items.len() {
                        result.push(instantiator(&items[i], mapper.clone()));
                        i += 1;
                    }
                    return Some(result.into());
                }

                i += 1;
            }
        }
        Some(items.clone())
    }

    pub(super) fn instantiate_types(
        &self,
        types: Option<GcVec<Gc<Type>>>,
        mapper: Option<Gc<TypeMapper>>,
    ) -> Option<GcVec<Gc<Type>>> {
        self.instantiate_list(types, mapper, |type_: &Gc<Type>, mapper| {
            self.instantiate_type(type_, mapper)
        })
    }

    pub(super) fn instantiate_signatures(
        &self,
        signatures: GcVec<Gc<Signature>>,
        mapper: Gc<TypeMapper>,
    ) -> GcVec<Gc<Signature>> {
        self.instantiate_list(
            Some(signatures),
            Some(mapper),
            |signature: &Gc<Signature>, mapper| {
                Gc::new(self.instantiate_signature(signature.clone(), mapper.unwrap(), None))
            },
        )
        .unwrap()
    }

    pub(super) fn instantiate_index_infos(
        &self,
        index_infos: &[Gc<IndexInfo>],
        mapper: Gc<TypeMapper>,
    ) -> Vec<Gc<IndexInfo>> {
        self.instantiate_list(
            Some(index_infos),
            Some(mapper.clone()),
            |index_info: &Gc<IndexInfo>, mapper| {
                self.instantiate_index_info(index_info, mapper.unwrap())
            },
        )
        .unwrap()
    }

    pub(super) fn create_type_mapper(
        &self,
        sources: GcVec<Gc<Type /*TypeParameter*/>>,
        targets: Option<GcVec<Gc<Type>>>,
    ) -> TypeMapper {
        if sources.len() == 1 {
            self.make_unary_type_mapper(
                &sources[0],
                &*targets.map_or_else(|| self.any_type(), |targets| targets[0].clone()),
            )
        } else {
            self.make_array_type_mapper(sources, targets)
        }
    }

    pub(super) fn get_mapped_type(&self, type_: &Type, mapper: &TypeMapper) -> Gc<Type> {
        match mapper {
            TypeMapper::Simple(mapper) => {
                if ptr::eq(type_, &*mapper.source) {
                    mapper.target.clone()
                } else {
                    type_.type_wrapper()
                }
            }
            TypeMapper::Array(mapper) => {
                let sources = &mapper.sources;
                let targets = &mapper.targets;
                for (i, source) in sources.iter().enumerate() {
                    if ptr::eq(type_, &**source) {
                        return targets
                            .as_ref()
                            .map_or_else(|| self.any_type(), |targets| targets[i].clone());
                    }
                }
                type_.type_wrapper()
            }
            TypeMapper::Function(mapper) => mapper.func.call(self, type_),
            TypeMapper::Composite(composite_or_merged_mapper)
            | TypeMapper::Merged(composite_or_merged_mapper) => {
                let t1 = self.get_mapped_type(type_, &composite_or_merged_mapper.mapper1);
                if !ptr::eq(&*t1, type_) && matches!(mapper, TypeMapper::Composite(_)) {
                    self.instantiate_type(&t1, Some(composite_or_merged_mapper.mapper2.clone()))
                } else {
                    self.get_mapped_type(&t1, &composite_or_merged_mapper.mapper2)
                }
            }
        }
    }

    pub(super) fn make_unary_type_mapper(&self, source: &Type, target: &Type) -> TypeMapper {
        TypeMapper::new_simple(source.type_wrapper(), target.type_wrapper())
    }

    pub(super) fn make_array_type_mapper(
        &self,
        sources: GcVec<Gc<Type /*TypeParameter*/>>,
        targets: Option<GcVec<Gc<Type>>>,
    ) -> TypeMapper {
        TypeMapper::new_array(sources, targets)
    }

    pub(super) fn make_function_type_mapper<TFunc: TypeMapperCallback + 'static>(
        &self,
        func: TFunc,
    ) -> TypeMapper {
        TypeMapper::new_function(func)
    }

    pub(super) fn make_composite_type_mapper(
        &self,
        mapper1: Gc<TypeMapper>,
        mapper2: Gc<TypeMapper>,
    ) -> TypeMapper {
        TypeMapper::new_composite(mapper1, mapper2)
    }

    pub(super) fn make_merged_type_mapper(
        &self,
        mapper1: Gc<TypeMapper>,
        mapper2: Gc<TypeMapper>,
    ) -> TypeMapper {
        TypeMapper::new_merged(mapper1, mapper2)
    }

    pub(super) fn create_type_eraser(
        &self,
        sources: GcVec<Gc<Type /*TypeParameter*/>>,
    ) -> TypeMapper {
        self.create_type_mapper(sources, None)
    }

    pub(super) fn create_backreference_mapper(
        &self,
        context: &InferenceContext,
        index: usize,
    ) -> TypeMapper {
        self.make_function_type_mapper(BackreferenceMapperCallback::new(context, index))
    }

    pub(super) fn combine_type_mappers(
        &self,
        mapper1: Option<Gc<TypeMapper>>,
        mapper2: Gc<TypeMapper>,
    ) -> Gc<TypeMapper> {
        if let Some(mapper1) = mapper1 {
            Gc::new(self.make_composite_type_mapper(mapper1, mapper2))
        } else {
            mapper2
        }
    }

    pub(super) fn merge_type_mappers(
        &self,
        mapper1: Option<Gc<TypeMapper>>,
        mapper2: Gc<TypeMapper>,
    ) -> Gc<TypeMapper> {
        if let Some(mapper1) = mapper1 {
            Gc::new(self.make_merged_type_mapper(mapper1, mapper2))
        } else {
            mapper2
        }
    }

    pub(super) fn prepend_type_mapping(
        &self,
        source: &Type,
        target: &Type,
        mapper: Option<Gc<TypeMapper>>,
    ) -> TypeMapper {
        match mapper {
            None => self.make_unary_type_mapper(source, target),
            Some(mapper) => self.make_merged_type_mapper(
                Gc::new(self.make_unary_type_mapper(source, target)),
                mapper,
            ),
        }
    }

    pub(super) fn append_type_mapping(
        &self,
        mapper: Option<Gc<TypeMapper>>,
        source: &Type,
        target: &Type,
    ) -> TypeMapper {
        match mapper {
            None => self.make_unary_type_mapper(source, target),
            Some(mapper) => self.make_merged_type_mapper(
                mapper,
                Gc::new(self.make_unary_type_mapper(source, target)),
            ),
        }
    }

    pub(super) fn get_restrictive_type_parameter(
        &self,
        tp: &Type, /*TypeParameter*/
    ) -> Gc<Type> {
        if matches!(
            tp.as_type_parameter().maybe_constraint().as_ref(),
            Some(constraint) if Gc::ptr_eq(constraint, &self.unknown_type())
        ) {
            tp.type_wrapper()
        } else {
            if tp.maybe_restrictive_instantiation().is_none() {
                let restrictive_instantiation: Gc<Type> =
                    self.create_type_parameter(tp.maybe_symbol()).into();
                *tp.maybe_restrictive_instantiation() = Some(restrictive_instantiation.clone());
                restrictive_instantiation
                    .as_type_parameter()
                    .set_constraint(self.unknown_type());
            }
            tp.maybe_restrictive_instantiation().clone().unwrap()
        }
    }

    pub(super) fn clone_type_parameter(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> Gc<Type /*TypeParameter*/> {
        let mut result = self.create_type_parameter(Some(type_parameter.symbol()));
        result.target = Some(type_parameter.type_wrapper());
        result.into()
    }

    pub(super) fn instantiate_type_predicate(
        &self,
        predicate: &TypePredicate,
        mapper: Gc<TypeMapper>,
    ) -> TypePredicate {
        self.create_type_predicate(
            predicate.kind,
            predicate.parameter_name.clone(),
            predicate.parameter_index,
            self.maybe_instantiate_type(predicate.type_.as_deref(), Some(mapper)),
        )
    }

    pub(super) fn instantiate_signature(
        &self,
        signature: Gc<Signature>,
        mut mapper: Gc<TypeMapper>,
        erase_type_parameters: Option<bool>,
    ) -> Signature {
        let erase_type_parameters = erase_type_parameters.unwrap_or(false);
        let mut fresh_type_parameters: Option<GcVec<Gc<Type /*TypeParameter*/>>> = None;
        if let Some(signature_type_parameters) = signature.maybe_type_parameters().clone() {
            if !erase_type_parameters {
                fresh_type_parameters = Some(
                    map(&*signature_type_parameters, |type_parameter, _| {
                        self.clone_type_parameter(&type_parameter)
                    })
                    .into(),
                );
                mapper = self.combine_type_mappers(
                    Some(Gc::new(self.create_type_mapper(
                        signature_type_parameters,
                        fresh_type_parameters.clone(),
                    ))),
                    mapper,
                );
                for tp in fresh_type_parameters.as_deref().unwrap() {
                    tp.as_type_parameter().set_mapper(mapper.clone());
                }
            }
        }
        let mut result = self.create_signature(
            signature.declaration.clone(),
            fresh_type_parameters,
            signature
                .maybe_this_parameter()
                .as_ref()
                .map(|this_parameter| self.instantiate_symbol(this_parameter, mapper.clone())),
            self.instantiate_list(
                Some(signature.parameters()),
                Some(mapper.clone()),
                |parameter, mapper| self.instantiate_symbol(parameter, mapper.unwrap()),
            )
            .unwrap(),
            None,
            None,
            signature.min_argument_count(),
            signature.flags & SignatureFlags::PropagatingFlags,
        );
        result.target = Some(signature);
        result.mapper = Some(mapper);
        result
    }

    pub(super) fn instantiate_symbol(
        &self,
        symbol: &Symbol,
        mut mapper: Gc<TypeMapper>,
    ) -> Gc<Symbol> {
        let mut symbol = symbol.symbol_wrapper();
        let links = self.get_symbol_links(&symbol);
        {
            let links = (*links).borrow();
            if let Some(type_) = links.type_.as_ref() {
                if !self.could_contain_type_variables(&type_) {
                    return symbol;
                }
            }
        }
        if get_check_flags(&symbol).intersects(CheckFlags::Instantiated) {
            let links = (*links).borrow();
            symbol = links.target.clone().unwrap();
            mapper = self.combine_type_mappers(links.mapper.clone(), mapper);
        }
        let result = self.create_symbol(
            symbol.flags(),
            symbol.escaped_name().to_owned(),
            Some(
                CheckFlags::Instantiated
                    | get_check_flags(&symbol)
                        & (CheckFlags::Readonly
                            | CheckFlags::Late
                            | CheckFlags::OptionalParameter
                            | CheckFlags::RestParameter),
            ),
        );
        if let Some(declarations) = &*symbol.maybe_declarations() {
            result.set_declarations(declarations.clone());
        }
        result.set_parent(symbol.maybe_parent());
        let result_links = result.symbol_links();
        let mut result_links = result_links.borrow_mut();
        result_links.target = Some(symbol.clone());
        result_links.mapper = Some(mapper);
        if let Some(symbol_value_declaration) = symbol.maybe_value_declaration() {
            result.set_value_declaration(symbol_value_declaration);
        }
        if let Some(links_name_type) = (*links).borrow().name_type.clone() {
            result_links.name_type = Some(links_name_type);
        }
        result.into()
    }

    pub(super) fn get_object_type_instantiation<TAliasSymbol: Borrow<Symbol>>(
        &self,
        type_: &Type, /*AnonymousType | DeferredTypeReference*/
        mapper: Gc<TypeMapper>,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Gc<Type>]>,
    ) -> Gc<Type> {
        let type_as_object_type = type_.as_object_type();
        let declaration = if type_as_object_type
            .object_flags()
            .intersects(ObjectFlags::Reference)
        {
            type_.as_type_reference().maybe_node().clone().unwrap()
        } else {
            type_.symbol().maybe_declarations().clone().unwrap()[0].clone()
        };
        let links = self.get_node_links(&declaration);
        let target = if type_as_object_type
            .object_flags()
            .intersects(ObjectFlags::Reference)
        {
            (*links).borrow().resolved_type.clone().unwrap()
        } else if type_as_object_type
            .object_flags()
            .intersects(ObjectFlags::Instantiated)
        {
            type_as_object_type.maybe_target().unwrap()
        } else {
            type_.type_wrapper()
        };
        let mut type_parameters = (*links).borrow().outer_type_parameters.clone();
        let target_as_object_type = target.as_object_type();
        if type_parameters.is_none() {
            let mut outer_type_parameters =
                self.get_outer_type_parameters(&declaration, Some(true));
            if self.is_js_constructor(Some(&*declaration)) {
                let template_tag_parameters =
                    self.get_type_parameters_from_declaration(&declaration);
                outer_type_parameters = maybe_add_range(
                    outer_type_parameters,
                    template_tag_parameters.as_deref(),
                    None,
                    None,
                );
            }
            type_parameters = Some(
                outer_type_parameters
                    .map(Into::into)
                    .unwrap_or_else(|| vec![].into()),
            );
            let all_declarations = if type_as_object_type
                .object_flags()
                .intersects(ObjectFlags::Reference)
            {
                vec![declaration.clone()]
            } else {
                type_.symbol().maybe_declarations().clone().unwrap()
            };
            type_parameters = if (target_as_object_type
                .object_flags()
                .intersects(ObjectFlags::Reference)
                || target.symbol().flags().intersects(SymbolFlags::Method)
                || target.symbol().flags().intersects(SymbolFlags::TypeLiteral))
                && target.maybe_alias_type_arguments().is_none()
            {
                maybe_filter(type_parameters.as_double_deref(), |tp: &Gc<Type>| {
                    some(
                        Some(&*all_declarations),
                        Some(|d: &Gc<Node>| self.is_type_parameter_possibly_referenced(tp, d)),
                    )
                })
                .map(Into::into)
            } else {
                type_parameters
            };
            links.borrow_mut().outer_type_parameters = type_parameters.clone();
        }
        let type_parameters = type_parameters.unwrap();
        if !type_parameters.is_empty() {
            let combined_mapper =
                self.combine_type_mappers(type_as_object_type.maybe_mapper(), mapper.clone());
            let type_arguments: GcVec<_> = map(&*type_parameters, |t: &Gc<Type>, _| {
                self.get_mapped_type(t, &combined_mapper)
            })
            .into();
            let alias_symbol =
                alias_symbol.map(|alias_symbol| alias_symbol.borrow().symbol_wrapper());
            let new_alias_symbol = alias_symbol
                .clone()
                .or_else(|| type_.maybe_alias_symbol().clone());
            let new_alias_type_arguments = if alias_symbol.is_some() {
                alias_type_arguments.map(ToOwned::to_owned)
            } else {
                self.instantiate_types(type_.maybe_alias_type_arguments().as_deref(), Some(mapper))
            };
            let id = format!(
                "{}{}",
                self.get_type_list_id(Some(&type_arguments)),
                self.get_alias_id(
                    new_alias_symbol.as_deref(),
                    new_alias_type_arguments.as_deref()
                )
            );
            if target_as_object_type.maybe_instantiations().is_none() {
                *target_as_object_type.maybe_instantiations() = Some(HashMap::new());
                target_as_object_type
                    .maybe_instantiations()
                    .as_mut()
                    .unwrap()
                    .insert(
                        format!(
                            "{}{}",
                            self.get_type_list_id(Some(&*type_parameters)),
                            self.get_alias_id(
                                target.maybe_alias_symbol().as_deref(),
                                target.maybe_alias_type_arguments().as_deref()
                            )
                        ),
                        target.clone(),
                    );
            }
            let mut result = target_as_object_type
                .maybe_instantiations()
                .as_ref()
                .unwrap()
                .get(&id)
                .map(Clone::clone);
            if result.is_none() {
                let new_mapper =
                    Gc::new(self.create_type_mapper(type_parameters, Some(type_arguments)));
                result = Some(
                    if target_as_object_type
                        .object_flags()
                        .intersects(ObjectFlags::Reference)
                    {
                        let type_as_type_reference = type_.as_type_reference();
                        self.create_deferred_type_reference(
                            &type_as_type_reference.target,
                            &type_as_type_reference.maybe_node().as_ref().unwrap(),
                            Some(new_mapper),
                            new_alias_symbol,
                            new_alias_type_arguments.as_deref(),
                        )
                    } else if target_as_object_type
                        .object_flags()
                        .intersects(ObjectFlags::Mapped)
                    {
                        self.instantiate_mapped_type(
                            &target,
                            new_mapper,
                            new_alias_symbol,
                            new_alias_type_arguments.as_deref(),
                        )
                    } else {
                        self.instantiate_anonymous_type(
                            &target,
                            new_mapper,
                            new_alias_symbol,
                            new_alias_type_arguments.as_deref(),
                        )
                    },
                );
                target_as_object_type
                    .maybe_instantiations()
                    .as_mut()
                    .unwrap()
                    .insert(id, result.clone().unwrap());
            }
            return result.unwrap();
        }
        type_.type_wrapper()
    }
}

#[derive(Trace, Finalize)]
struct BackreferenceMapperCallback {
    context_inferences: Vec<Gc<InferenceInfo>>,
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
    fn call(&self, checker: &TypeChecker, t: &Type) -> Gc<Type> {
        if matches!(
            find_index(&self.context_inferences, |info: &Gc<InferenceInfo>, _| ptr::eq(&*info.type_parameter, t), None),
            Some(found_index) if found_index >= self.index
        ) {
            checker.unknown_type()
        } else {
            t.type_wrapper()
        }
    }
}
