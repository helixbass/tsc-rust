#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::IterationUse;
use crate::{
    add_related_info, contains_rc, create_diagnostic_for_node, create_file_diagnostic, every,
    filter, find_ancestor, for_each, for_each_bool, get_check_flags,
    get_effective_return_type_node, get_effective_type_annotation_node, get_object_flags,
    get_source_file_of_node, get_span_of_token_at_position, get_symbol_name_for_private_identifier,
    has_initializer, is_access_expression, is_assignment_target,
    is_function_expression_or_arrow_function, is_function_or_module_block, is_identifier,
    is_in_js_file, is_optional_chain, is_parameter, is_private_identifier,
    is_property_access_expression, is_property_declaration, is_property_signature,
    is_push_or_unshift_identifier, is_string_literal_like, is_variable_declaration, map,
    skip_parentheses, some, CheckFlags, Diagnostic, Diagnostics, EvolvingArrayType, FlowFlags,
    FlowNode, FlowNodeBase, FlowType, HasInitializerInterface, IncompleteType,
    NamedDeclarationInterface, Node, NodeFlags, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, ReadonlyTextRange, Signature, SignatureKind, Symbol, SymbolFlags,
    SymbolInterface, SyntaxKind, TransientSymbolInterface, Type, TypeChecker, TypeFlags,
    TypeInterface, TypePredicate, TypePredicateKind, UnionOrIntersectionTypeInterface,
    UnionReduction,
};

impl TypeChecker {
    pub(super) fn extract_types_of_kind(&self, type_: &Type, kind: TypeFlags) -> Rc<Type> {
        self.filter_type(type_, |t: &Type| t.flags().intersects(kind))
    }

    pub(super) fn get_flow_type_of_reference<
        TInitialType: Borrow<Type>,
        TFlowContainer: Borrow<Node>,
    >(
        &self,
        reference: &Node,
        declared_type: &Type,
        initial_type: Option<TInitialType>,
        flow_container: Option<TFlowContainer>,
    ) -> Rc<Type> {
        let initial_type = initial_type.map_or_else(
            || declared_type.type_wrapper(),
            |initial_type| initial_type.borrow().type_wrapper(),
        );
        unimplemented!()
    }
}
