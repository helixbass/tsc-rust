#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use crate::{
    __String, get_assignment_declaration_kind, get_check_flags, get_host_signature_from_jsdoc,
    get_symbol_id, get_this_container, is_binary_expression, is_class_like,
    is_constructor_declaration, is_function_expression, is_node_descendant_of,
    is_object_literal_expression, is_private_identifier_class_element_declaration, is_static,
    is_valid_es_symbol_declaration, map, pseudo_big_int_to_string, some, AssignmentDeclarationKind,
    BaseLiteralType, BigIntLiteralType, CheckFlags, Diagnostics, FunctionLikeDeclarationInterface,
    IndexInfo, InterfaceTypeInterface, LiteralTypeInterface, Node, NodeFlags, NodeInterface,
    Number, NumberLiteralType, PseudoBigInt, Signature, SignatureFlags, StringLiteralType,
    StringOrNumber, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Ternary,
    TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
    TypePredicate, UniqueESSymbolType,
};
use local_macros::enum_unwrapped;

impl TypeChecker {
    pub(super) fn is_spreadable_property(&self, prop: &Symbol) -> bool {
        !some(
            prop.maybe_declarations().as_deref(),
            Some(|declaration: &Rc<Node>| {
                is_private_identifier_class_element_declaration(declaration)
            }),
        ) && (!prop
            .flags()
            .intersects(SymbolFlags::Method | SymbolFlags::GetAccessor | SymbolFlags::SetAccessor)
            || !matches!(
                prop.maybe_declarations().as_ref(),
                Some(prop_declarations) if prop_declarations.iter().any(|decl: &Rc<Node>| is_class_like(&decl.parent()))
            ))
    }

    pub(super) fn get_spread_symbol(&self, prop: &Symbol, readonly: bool) -> Rc<Symbol> {
        let is_setonly_accessor = prop.flags().intersects(SymbolFlags::SetAccessor)
            && !prop.flags().intersects(SymbolFlags::GetAccessor);
        if !is_setonly_accessor && readonly == self.is_readonly_symbol(prop) {
            return prop.symbol_wrapper();
        }
        let flags = SymbolFlags::Property | (prop.flags() & SymbolFlags::Optional);
        let result: Rc<Symbol> = self
            .create_symbol(
                flags,
                prop.escaped_name().clone(),
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
        info: &Rc<IndexInfo>,
        readonly: bool,
    ) -> Rc<IndexInfo> {
        if info.is_readonly != readonly {
            Rc::new(self.create_index_info(
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
    ) -> Rc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Rc<Type> = StringLiteralType::new(type_, value).into();
        type_.set_symbol(symbol.map(|symbol| symbol.borrow().symbol_wrapper()));
        type_
            .as_literal_type()
            .set_regular_type(&if let Some(regular_type) = regular_type {
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
    ) -> Rc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Rc<Type> = NumberLiteralType::new(type_, value).into();
        type_.set_symbol(symbol.map(|symbol| symbol.borrow().symbol_wrapper()));
        type_
            .as_literal_type()
            .set_regular_type(&if let Some(regular_type) = regular_type {
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
    ) -> Rc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Rc<Type> = BigIntLiteralType::new(type_, value).into();
        type_.set_symbol(symbol.map(|symbol| symbol.borrow().symbol_wrapper()));
        type_
            .as_literal_type()
            .set_regular_type(&if let Some(regular_type) = regular_type {
                regular_type.borrow().type_wrapper()
            } else {
                type_.clone()
            });
        type_
    }

    pub(super) fn get_fresh_type_of_literal_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Literal) {
            return type_.as_literal_type().get_or_initialize_fresh_type(self);
        }
        type_.type_wrapper()
    }

    pub(super) fn get_regular_type_of_literal_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Literal) {
            type_.as_literal_type().regular_type()
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
        // (same question in is_type_related_to() and get_normalized_type() below)
        // or maybe this looks like it should be a trait that includes `maybe_fresh_type()` that
        // both of these implement?
        match type_ {
            Type::IntrinsicType(intrinsic_type) => ptr::eq(
                type_,
                enum_unwrapped!(intrinsic_type, [IntrinsicType, FreshableIntrinsicType])
                    .fresh_type()
                    .as_ptr(),
            ),
            Type::LiteralType(literal_type) => {
                matches!(
                    literal_type.fresh_type(),
                    Some(fresh_type) if ptr::eq(type_, fresh_type.as_ptr())
                )
            }
            _ => panic!("Expected IntrinsicType or LiteralType"),
        }
    }

    pub(super) fn get_string_literal_type(&self, value: &str) -> Rc<Type> {
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

    pub(super) fn get_number_literal_type(&self, value: Number) -> Rc<Type> {
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

    pub(super) fn get_big_int_literal_type(&self, value: PseudoBigInt) -> Rc<Type> {
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
    ) -> Rc<Type /*LiteralType*/> {
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
        let type_: Rc<Type> = match value {
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
    ) -> Rc<Type> {
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
    ) -> Rc<Type /*UniqueESSymbolType*/> {
        let type_ = self.create_type(TypeFlags::UniqueESSymbol);
        let type_: Rc<Type> = UniqueESSymbolType::new(
            type_,
            symbol.symbol_wrapper(),
            __String::new(format!(
                "__@{}@{}",
                &**symbol.escaped_name(), // TODO: should just implement Display on __String
                get_symbol_id(symbol)
            )),
        )
        .into();
        type_
    }

    pub(super) fn get_es_symbol_like_type_for_node(&self, node: &Node) -> Rc<Type> {
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

    pub(super) fn get_this_type(&self, node: &Node) -> Rc<Type> {
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
    ) -> Rc<Type> {
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
    ) -> Rc<Type> {
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
    ) -> Option<Rc<Node /*TypeNode*/>> {
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
    ) -> Rc<Type> {
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

    pub(super) fn get_type_from_type_node_(&self, node: &Node /*TypeNode*/) -> Rc<Type> {
        self.get_conditional_flow_type_of_type(&self.get_type_from_type_node_worker(node), node)
    }

    pub(super) fn get_type_from_type_node_worker(&self, node: &Node /*TypeNode*/) -> Rc<Type> {
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

    pub(super) fn instantiate_list<
        TItem,
        TInstantiator: FnMut(&Rc<TItem>, &TypeMapper) -> Rc<TItem>,
    >(
        &self,
        items: Option<&[Rc<TItem>]>,
        mapper: &TypeMapper,
        mut instantiator: TInstantiator,
    ) -> Option<Vec<Rc<TItem>>> {
        let items = items?;
        if !items.is_empty() {
            let mut i = 0;
            while i < items.len() {
                let item = &items[i];
                let mapped = instantiator(item, mapper);
                if !Rc::ptr_eq(item, &mapped) {
                    let mut result = if i == 0 {
                        vec![]
                    } else {
                        items[..i].to_owned()
                    };
                    result.push(mapped);
                    i += 1;
                    while i < items.len() {
                        result.push(instantiator(&items[i], mapper));
                        i += 1;
                    }
                    return Some(result);
                }

                i += 1;
            }
        }
        Some(items.to_owned())
    }

    pub(super) fn instantiate_types(
        &self,
        types: Option<&[Rc<Type>]>,
        mapper: &TypeMapper,
    ) -> Option<Vec<Rc<Type>>> {
        self.instantiate_list(types, mapper, |type_: &Rc<Type>, mapper| {
            self.instantiate_type(Some(&**type_), Some(mapper)).unwrap()
        })
    }

    pub(super) fn instantiate_signatures(
        &self,
        signatures: &[Rc<Signature>],
        mapper: &TypeMapper,
    ) -> Vec<Rc<Signature>> {
        self.instantiate_list(
            Some(signatures),
            mapper,
            |signature: &Rc<Signature>, mapper| {
                Rc::new(self.instantiate_signature(signature.clone(), mapper, None))
            },
        )
        .unwrap()
    }

    pub(super) fn instantiate_index_infos(
        &self,
        index_infos: &[Rc<IndexInfo>],
        mapper: &TypeMapper,
    ) -> Vec<Rc<IndexInfo>> {
        self.instantiate_list(
            Some(index_infos),
            mapper,
            |index_info: &Rc<IndexInfo>, mapper| self.instantiate_index_info(index_info, mapper),
        )
        .unwrap()
    }

    pub(super) fn create_type_mapper(
        &self,
        sources: Vec<Rc<Type /*TypeParameter*/>>,
        targets: Option<Vec<Rc<Type>>>,
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

    pub(super) fn get_mapped_type(&self, type_: &Type, mapper: &TypeMapper) -> Rc<Type> {
        match mapper {
            TypeMapper::Simple(mapper) => {
                if ptr::eq(type_, Rc::as_ptr(&mapper.source)) {
                    mapper.target.clone()
                } else {
                    type_.type_wrapper()
                }
            }
            TypeMapper::Array(mapper) => {
                let sources = &mapper.sources;
                let targets = &mapper.targets;
                for (i, source) in sources.iter().enumerate() {
                    if ptr::eq(type_, Rc::as_ptr(source)) {
                        return targets
                            .as_ref()
                            .map_or_else(|| self.any_type(), |targets| targets[i].clone());
                    }
                }
                type_.type_wrapper()
            }
            TypeMapper::Function(mapper) => (mapper.func)(self, type_),
            TypeMapper::Composite(composite_or_merged_mapper)
            | TypeMapper::Merged(composite_or_merged_mapper) => {
                let t1 = self.get_mapped_type(type_, &composite_or_merged_mapper.mapper1);
                if !ptr::eq(Rc::as_ptr(&t1), type_) && matches!(mapper, TypeMapper::Composite(_)) {
                    self.instantiate_type(Some(t1), Some(&composite_or_merged_mapper.mapper2))
                        .unwrap()
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
        sources: Vec<Rc<Type /*TypeParameter*/>>,
        targets: Option<Vec<Rc<Type>>>,
    ) -> TypeMapper {
        TypeMapper::new_array(sources, targets)
    }

    pub(super) fn make_function_type_mapper(
        &self,
        func: fn(&TypeChecker, &Type) -> Rc<Type>,
    ) -> TypeMapper {
        TypeMapper::new_function(func)
    }

    pub(super) fn make_composite_type_mapper(
        &self,
        mapper1: TypeMapper,
        mapper2: TypeMapper,
    ) -> TypeMapper {
        TypeMapper::new_composite(mapper1, mapper2)
    }

    pub(super) fn make_merged_type_mapper(
        &self,
        mapper1: TypeMapper,
        mapper2: TypeMapper,
    ) -> TypeMapper {
        TypeMapper::new_merged(mapper1, mapper2)
    }

    pub(super) fn create_type_eraser(
        &self,
        sources: Vec<Rc<Type /*TypeParameter*/>>,
    ) -> TypeMapper {
        self.create_type_mapper(sources, None)
    }

    pub(super) fn combine_type_mappers(
        &self,
        mapper1: Option<TypeMapper>,
        mapper2: TypeMapper,
    ) -> TypeMapper {
        if let Some(mapper1) = mapper1 {
            self.make_composite_type_mapper(mapper1, mapper2)
        } else {
            mapper2
        }
    }

    pub(super) fn merge_type_mappers(
        &self,
        mapper1: Option<TypeMapper>,
        mapper2: TypeMapper,
    ) -> TypeMapper {
        if let Some(mapper1) = mapper1 {
            self.make_merged_type_mapper(mapper1, mapper2)
        } else {
            mapper2
        }
    }

    pub(super) fn prepend_type_mapping(
        &self,
        source: &Type,
        target: &Type,
        mapper: Option<TypeMapper>,
    ) -> TypeMapper {
        unimplemented!()
    }

    pub(super) fn append_type_mapping(
        &self,
        mapper: Option<TypeMapper>,
        source: &Type,
        target: &Type,
    ) -> TypeMapper {
        unimplemented!()
    }

    pub(super) fn get_restrictive_type_parameter(
        &self,
        tp: &Type, /*TypeParameter*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn clone_type_parameter(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> Rc<Type /*TypeParameter*/> {
        let mut result = self.create_type_parameter(Some(type_parameter.symbol()));
        result.target = Some(type_parameter.type_wrapper());
        result.into()
    }

    pub(super) fn instantiate_type_predicate(
        &self,
        predicate: &TypePredicate,
        mapper: &TypeMapper,
    ) -> TypePredicate {
        unimplemented!()
    }

    pub(super) fn instantiate_signature(
        &self,
        signature: Rc<Signature>,
        mapper: &TypeMapper,
        erase_type_parameters: Option<bool>,
    ) -> Signature {
        let mut mapper = mapper.clone();
        let erase_type_parameters = erase_type_parameters.unwrap_or(false);
        let mut fresh_type_parameters: Option<Vec<Rc<Type /*TypeParameter*/>>> = None;
        if let Some(signature_type_parameters) = signature.type_parameters.clone() {
            if !erase_type_parameters {
                fresh_type_parameters = map(
                    Some(signature_type_parameters.clone()),
                    |type_parameter, _| self.clone_type_parameter(&type_parameter),
                );
                mapper = self.combine_type_mappers(
                    Some(self.create_type_mapper(
                        signature_type_parameters,
                        fresh_type_parameters.clone(),
                    )),
                    mapper,
                );
                for tp in fresh_type_parameters.as_ref().unwrap() {
                    tp.as_type_parameter().set_mapper(mapper.clone());
                }
            }
        }
        let mut result = self.create_signature(
            signature.declaration.clone(),
            fresh_type_parameters,
            signature
                .this_parameter
                .as_ref()
                .map(|this_parameter| self.instantiate_symbol(this_parameter, &mapper)),
            self.instantiate_list(
                Some(signature.parameters()),
                &mapper,
                |parameter, mapper| self.instantiate_symbol(parameter, mapper),
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

    pub(super) fn instantiate_symbol(&self, symbol: &Symbol, mapper: &TypeMapper) -> Rc<Symbol> {
        let mut symbol = symbol.symbol_wrapper();
        let links = self.get_symbol_links(&symbol);
        let links = (*links).borrow();
        if let Some(type_) = links.type_.as_ref() {
            if !self.could_contain_type_variables(&type_) {
                return symbol;
            }
        }
        let mut mapper = (*mapper).clone();
        if get_check_flags(&symbol).intersects(CheckFlags::Instantiated) {
            symbol = links.target.clone().unwrap();
            mapper = self.combine_type_mappers(links.mapper.clone(), mapper);
        }
        let result = self.create_symbol(
            symbol.flags(),
            symbol.escaped_name().clone(),
            Some(
                CheckFlags::Instantiated
                    | get_check_flags(&*symbol)
                        & (CheckFlags::Readonly
                            | CheckFlags::Late
                            | CheckFlags::OptionalParameter
                            | CheckFlags::RestParameter),
            ),
        );
        if let Some(declarations) = &*symbol.maybe_declarations() {
            result.set_declarations(declarations.clone());
        }
        let symbol_links = result.symbol_links();
        let mut symbol_links_ref = symbol_links.borrow_mut();
        symbol_links_ref.target = Some(symbol.clone());
        symbol_links_ref.mapper = Some(mapper);
        if let Some(value_declaration) = symbol.maybe_value_declaration() {
            result.set_value_declaration(value_declaration);
        }
        result.into()
    }

    pub(super) fn is_type_parameter_possibly_referenced(
        &self,
        tp: &Type, /*TypeParameter*/
        node: &Node,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_homomorphic_type_variable(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_conditional_type_instantiation<TAliasSymbol: Borrow<Symbol>>(
        &self,
        type_: &Type, /*ConditionalType*/
        mapper: &TypeMapper,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn instantiate_type<TTypeRef: Borrow<Type>>(
        &self,
        type_: Option<TTypeRef>,
        mapper: Option<&TypeMapper>,
    ) -> Option<Rc<Type>> {
        match (type_.as_ref(), mapper) {
            (Some(type_), Some(mapper)) => Some(self.instantiate_type_with_alias(
                type_.borrow(),
                mapper,
                Option::<&Symbol>::None,
                None,
            )),
            _ => type_.map(|type_| type_.borrow().type_wrapper()),
        }
    }

    pub(super) fn instantiate_type_with_alias<TSymbolRef: Borrow<Symbol>>(
        &self,
        type_: &Type,
        mapper: &TypeMapper,
        alias_symbol: Option<TSymbolRef>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let result =
            self.instantiate_type_worker(type_, mapper, alias_symbol, alias_type_arguments);
        result
    }

    pub(super) fn instantiate_type_worker<TSymbolRef: Borrow<Symbol>>(
        &self,
        type_: &Type,
        mapper: &TypeMapper,
        alias_symbol: Option<TSymbolRef>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let flags = type_.flags();
        if flags.intersects(TypeFlags::TypeParameter) {
            return self.get_mapped_type(type_, mapper);
        }
        unimplemented!()
    }

    pub(super) fn get_permissive_instantiation(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_restrictive_instantiation(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn instantiate_index_info(
        &self,
        info: &IndexInfo,
        mapper: &TypeMapper,
    ) -> Rc<IndexInfo> {
        unimplemented!()
    }

    pub(super) fn is_context_sensitive(
        &self,
        node: &Node, /*Expression | MethodDeclaration | ObjectLiteralElementLike | JsxAttributeLike | JsxChild*/
    ) -> bool {
        // match node {
        // }
        false
    }

    pub(super) fn is_type_identical_to(&self, source: &Type, target: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn compare_types_identical(&self, source: &Type, target: &Type) -> Ternary {
        unimplemented!()
    }

    pub(super) fn compare_types_subtype_of(&self, source: &Type, target: &Type) -> Ternary {
        unimplemented!()
    }

    pub(super) fn is_type_subtype_of(&self, source: &Type, target: &Type) -> bool {
        self.is_type_related_to(source, target, &self.subtype_relation())
    }

    pub(super) fn is_type_assignable_to(&self, source: &Type, target: &Type) -> bool {
        self.is_type_related_to(source, target, &self.assignable_relation())
    }

    pub(super) fn is_type_derived_from(&self, source: &Type, target: &Type) -> bool {
        unimplemented!()
    }
}
