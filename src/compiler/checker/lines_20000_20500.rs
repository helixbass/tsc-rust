#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::cmp;
use std::collections::HashSet;
use std::ptr;
use std::rc::Rc;

use super::{
    anon, CheckTypeContainingMessageChain, CheckTypeRelatedTo, ErrorCalculationState,
    IntersectionState, RecursionFlags, ReportUnmeasurableMarkers, ReportUnreliableMarkers,
    SignatureCheckMode, TypeFacts,
};
use crate::{
    are_option_rcs_equal, cartesian_product, create_diagnostic_for_node, factory,
    get_declaration_modifier_flags_from_symbol, get_symbol_name_for_private_identifier,
    is_named_declaration, is_private_identifier, length, push_if_unique_rc, reduce_left, some,
    CheckFlags, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, ElementFlags, IndexInfo,
    ModifierFlags, Node, NodeInterface, ObjectFlags, Signature, SignatureFlags, SignatureKind,
    Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Ternary, Type, TypeFlags, TypeFormatFlags,
    TypeInterface, VarianceFlags, __String, get_check_flags, get_object_flags,
};

impl<'type_checker, TContainingMessageChain: CheckTypeContainingMessageChain>
    CheckTypeRelatedTo<'type_checker, TContainingMessageChain>
{
    pub(super) fn index_info_related_to(
        &self,
        source_info: &IndexInfo,
        target_info: &IndexInfo,
        report_errors: bool,
    ) -> Ternary {
        unimplemented!()
    }

    pub(super) fn index_signatures_related_to(
        &self,
        source: &Type,
        target: &Type,
        source_is_primitive: bool,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        unimplemented!()
    }

    pub(super) fn constructor_visibilities_are_compatible(
        &self,
        source_signature: &Signature,
        target_signature: &Signature,
        report_errors: bool,
    ) -> bool {
        unimplemented!()
    }
}
