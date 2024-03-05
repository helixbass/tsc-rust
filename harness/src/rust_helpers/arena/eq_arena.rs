use std::iter;

use speculoos::{AssertionFailure, Spec};
use typescript_rust::{
    id_arena::Id, Diagnostic, DiagnosticInterface, DiagnosticRelatedInformation,
    DiagnosticRelatedInformationInterface, HasArena, InArena,
};

pub trait EqArena {
    type Item;

    fn eq_arena(&self, other: Id<Self::Item>, arena: &impl HasArena) -> bool;
}

fn are_diagnostic_related_information_interface_same<
    TDiagnostic: DiagnosticRelatedInformationInterface,
>(
    a: &TDiagnostic,
    b: &TDiagnostic,
) -> bool {
    a.category() == b.category()
        && a.code() == b.code()
        && a.maybe_file() == b.maybe_file()
        && a.maybe_start() == b.maybe_start()
        && a.maybe_length() == b.maybe_length()
        && a.message_text() == b.message_text()
}

fn are_diagnostic_interface_same<TDiagnostic: DiagnosticInterface>(
    a: &TDiagnostic,
    b: &TDiagnostic,
    arena: &impl HasArena,
) -> bool {
    are_diagnostic_related_information_interface_same(a, b)
        && a.maybe_skipped_on().as_ref() == b.maybe_skipped_on().as_ref()
        && are_option_slice_eq_arenas_eq(
            a.maybe_related_information().as_deref(),
            b.maybe_related_information().as_deref(),
            arena,
        )
}

impl EqArena for Id<Diagnostic> {
    type Item = Diagnostic;

    fn eq_arena(&self, other: Id<Diagnostic>, arena: &impl HasArena) -> bool {
        let self_ref = self.ref_(arena);
        let other_ref = other.ref_(arena);

        match (&*self_ref, &*other_ref) {
            (Diagnostic::DiagnosticWithLocation(a), Diagnostic::DiagnosticWithLocation(b)) => {
                are_diagnostic_interface_same(a, b, arena)
            }
            (
                Diagnostic::DiagnosticWithDetachedLocation(a),
                Diagnostic::DiagnosticWithDetachedLocation(b),
            ) => a.file_name == b.file_name && are_diagnostic_interface_same(a, b, arena),
            (Diagnostic::BaseDiagnostic(a), Diagnostic::BaseDiagnostic(b)) => {
                are_diagnostic_interface_same(a, b, arena)
            }
            _ => false,
        }
    }
}

impl EqArena for Id<DiagnosticRelatedInformation> {
    type Item = DiagnosticRelatedInformation;

    fn eq_arena(&self, other: Id<DiagnosticRelatedInformation>, arena: &impl HasArena) -> bool {
        are_diagnostic_related_information_interface_same(&*self.ref_(arena), &*other.ref_(arena))
    }
}

fn are_slice_eq_arenas_eq<TItem>(a: &[Id<TItem>], b: &[Id<TItem>], arena: &impl HasArena) -> bool
where
    Id<TItem>: EqArena<Item = TItem>,
{
    if a.len() != b.len() {
        return false;
    }
    for (&a_item, &b_item) in iter::zip(a, b) {
        if !a_item.eq_arena(b_item, arena) {
            return false;
        }
    }
    true
}

fn are_option_slice_eq_arenas_eq<TItem>(
    a: Option<&[Id<TItem>]>,
    b: Option<&[Id<TItem>]>,
    arena: &impl HasArena,
) -> bool
where
    Id<TItem>: EqArena<Item = TItem>,
{
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) => are_slice_eq_arenas_eq(a, b, arena),
        _ => false,
    }
}

pub trait VecEqArena {
    type Item;

    fn is_equal_to_arena(&mut self, other: &Vec<Id<Self::Item>>, arena: &impl HasArena);
}

impl<'s, TItem> VecEqArena for Spec<'s, Vec<Id<TItem>>>
where
    Id<TItem>: EqArena<Item = TItem>,
{
    type Item = TItem;

    fn is_equal_to_arena(&mut self, expected: &Vec<Id<TItem>>, arena: &impl HasArena) {
        let subject = self.subject;
        if !are_slice_eq_arenas_eq(subject, expected, arena) {
            AssertionFailure::from_spec(self)
                .with_expected(format!("<{:?}>", expected))
                .with_actual(format!("<{:?}>", expected))
                .fail();
        }
    }
}
