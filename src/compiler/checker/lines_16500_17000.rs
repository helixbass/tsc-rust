#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use crate::{
    filter, find_index, maybe_add_range, InferenceContext, InferenceInfo, ObjectFlags,
    ObjectFlagsTypeInterface, ObjectTypeInterface, TypeMapperCallback, TypeReferenceInterface,
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

    pub(super) fn instantiate_mapped_type<TAliasSymbol: Borrow<Symbol>>(
        &self,
        type_: &Type, /*MappedType*/
        mapper: &TypeMapper,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn instantiate_anonymous_type<TAliasSymbol: Borrow<Symbol>>(
        &self,
        type_: &Type, /*AnonymousType*/
        mapper: TypeMapper,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type /*AnonymousType*/> {
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
