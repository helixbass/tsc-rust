#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::convert::TryInto;
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
    is_property_name, map, maybe_every, reduce_left, some, uncapitalize,
    unescape_leading_underscores, AccessFlags, AssignmentKind, DiagnosticMessageChain, Diagnostics,
    IndexInfo, IndexedAccessType, LiteralType, Node, NodeFlags, NodeInterface, Number, ObjectFlags,
    ObjectFlagsTypeInterface, ObjectTypeInterface, StringMappingType, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind, TemplateLiteralType, Type, TypeChecker,
    UnionOrIntersectionTypeInterface, UnionReduction, __String, pseudo_big_int_to_string,
    TypeFlags, TypeInterface,
};

impl TypeChecker {
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
