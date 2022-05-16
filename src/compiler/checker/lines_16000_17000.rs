#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use crate::{
    get_check_flags, is_class_like, is_private_identifier_class_element_declaration, map,
    pseudo_big_int_to_string, some, BaseLiteralType, BigIntLiteralType, CheckFlags, IndexInfo,
    LiteralTypeInterface, Node, NodeInterface, Number, NumberLiteralType, PseudoBigInt, Signature,
    SignatureFlags, StringLiteralType, StringOrNumber, Symbol, SymbolFlags, SymbolInterface,
    SyntaxKind, Ternary, TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface,
    TypeMapper, TypePredicate, UnionOrIntersectionType,
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
        unimplemented!()
    }

    pub(super) fn get_index_info_with_readonly(
        &self,
        info: &Rc<IndexInfo>,
        readonly: bool,
    ) -> Rc<IndexInfo> {
        unimplemented!()
    }

    // pub fn create_literal_type(
    pub fn create_string_literal_type<TTypeRef: Borrow<Type>>(
        &self,
        flags: TypeFlags,
        value: String,
        regular_type: Option<TTypeRef>,
    ) -> Rc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Rc<Type> = StringLiteralType::new(type_, value).into();
        type_
            .as_literal_type()
            .set_regular_type(&if let Some(regular_type) = regular_type {
                regular_type.borrow().type_wrapper()
            } else {
                type_.clone()
            });
        type_
    }

    pub fn create_number_literal_type<TTypeRef: Borrow<Type>>(
        &self,
        flags: TypeFlags,
        value: Number,
        regular_type: Option<TTypeRef>,
    ) -> Rc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Rc<Type> = NumberLiteralType::new(type_, value).into();
        type_
            .as_literal_type()
            .set_regular_type(&if let Some(regular_type) = regular_type {
                regular_type.borrow().type_wrapper()
            } else {
                type_.clone()
            });
        type_
    }

    pub fn create_big_int_literal_type<TTypeRef: Borrow<Type>>(
        &self,
        flags: TypeFlags,
        value: PseudoBigInt,
        regular_type: Option<TTypeRef>,
    ) -> Rc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Rc<Type> = BigIntLiteralType::new(type_, value).into();
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
        match type_ {
            Type::LiteralType(literal_type) => {
                return literal_type.get_or_initialize_fresh_type(self);
            }
            _ => type_.type_wrapper(),
        }
    }

    pub(super) fn get_regular_type_of_literal_type(&self, type_: &Type) -> Rc<Type> {
        match type_ {
            Type::LiteralType(literal_type) => return literal_type.regular_type(),
            Type::UnionOrIntersectionType(UnionOrIntersectionType::UnionType(union_type)) => {
                unimplemented!()
            }
            _ => type_.type_wrapper(),
        }
    }

    pub(super) fn is_fresh_literal_type(&self, type_: &Type) -> bool {
        if !type_.flags().intersects(TypeFlags::Literal) {
            return false;
        }
        // TODO: should this be using eg a Type.as_has_fresh_type() "unwrapper-helper" instead?
        // (same question in is_type_related_to() and get_normalized_type() below)
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
            value.to_string(),
            Option::<&Type>::None,
        );
        string_literal_types.insert(value.to_string(), type_.clone());
        type_
    }

    pub(super) fn get_number_literal_type(&self, value: Number) -> Rc<Type> {
        let mut number_literal_types = self.number_literal_types();
        if number_literal_types.contains_key(&value) {
            return number_literal_types.get(&value).unwrap().clone();
        }
        let type_ =
            self.create_number_literal_type(TypeFlags::NumberLiteral, value, Option::<&Type>::None);
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
        unimplemented!()
    }

    pub(super) fn get_type_from_literal_type_node(
        &self,
        node: &Node, /*LiteralTypeNode*/
    ) -> Rc<Type> {
        let node_as_literal_type_node = node.as_literal_type_node();
        if node_as_literal_type_node.literal.kind() == SyntaxKind::NullKeyword {
            unimplemented!()
        }
        let links = self.get_node_links(node);
        let mut links_ref = links.borrow_mut();
        if links_ref.resolved_type.is_none() {
            links_ref.resolved_type = Some(self.get_regular_type_of_literal_type(
                &self.check_expression(&node_as_literal_type_node.literal, None, None),
            ));
        }
        links_ref.resolved_type.clone().unwrap()
    }

    pub(super) fn get_es_symbol_like_type_for_node(&self, node: &Node) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_array_element_type_node(
        &self,
        node: &Node, /*ArrayTypeNode*/
    ) -> Option<Rc<Node /*TypeNode*/>> {
        Some(node.as_array_type_node().element_type.clone())
    }

    pub(super) fn get_type_from_type_node_(&self, node: &Node /*TypeNode*/) -> Rc<Type> {
        self.get_conditional_flow_type_of_type(&self.get_type_from_type_node_worker(node), node)
    }

    pub(super) fn get_type_from_type_node_worker(&self, node: &Node /*TypeNode*/) -> Rc<Type> {
        match node {
            Node::KeywordTypeNode(_) => match node.kind() {
                SyntaxKind::StringKeyword => self.string_type(),
                SyntaxKind::NumberKeyword => self.number_type(),
                SyntaxKind::BigIntKeyword => self.bigint_type(),
                SyntaxKind::BooleanKeyword => self.boolean_type(),
                _ => unimplemented!(),
            },
            Node::LiteralTypeNode(_) => self.get_type_from_literal_type_node(node),
            Node::TypeReferenceNode(_) => self.get_type_from_type_reference(node),
            Node::ArrayTypeNode(_) => self.get_type_from_array_or_tuple_type_node(node),
            Node::UnionTypeNode(_) => self.get_type_from_union_type_node(node),
            _ => unimplemented!(),
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
        if items.is_none() {
            return None;
        }
        let items = items.unwrap();
        if
        /*items &&*/
        !items.is_empty() {
            let mut i = 0;
            while i < items.len() {
                let item = &items[i];
                let mapped = instantiator(item, mapper);
                if !Rc::ptr_eq(item, &mapped) {
                    let mut result = if i == 0 { vec![] } else { items[..i].to_vec() };
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
        Some(items.to_vec())
    }

    pub(super) fn instantiate_types(
        &self,
        types: Option<&[Rc<Type>]>,
        mapper: &TypeMapper,
    ) -> Option<Vec<Rc<Type>>> {
        unimplemented!()
    }

    pub(super) fn instantiate_signatures(
        &self,
        signatures: &[Rc<Signature>],
        mapper: &TypeMapper,
    ) -> Vec<Rc<Signature>> {
        self.instantiate_list(Some(signatures), mapper, |signature, mapper| {
            Rc::new(self.instantiate_signature(signature.clone(), mapper, None))
        })
        .unwrap()
    }

    pub(super) fn instantiate_index_infos(
        &self,
        index_infos: &[Rc<IndexInfo>],
        mapper: &TypeMapper,
    ) -> Vec<Rc<IndexInfo>> {
        unimplemented!()
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
