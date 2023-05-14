#![cfg(test)]
use std::iter;

use gc::{Finalize, Gc, Trace};
use speculoos::{AssertionFailure, Spec};

pub trait GcSlicesAreEqual {
    type Item: Trace + Finalize;

    fn gc_slices_are_equal(&mut self, expected: &[Gc<Self::Item>]);
}

impl<'s, TItem> GcSlicesAreEqual for Spec<'s, Vec<Gc<TItem>>>
where
    TItem: Trace + Finalize,
{
    type Item = TItem;

    fn gc_slices_are_equal(&mut self, expected: &[Gc<TItem>]) {
        let subject = self.subject;
        if expected.len() != subject.len()
            || iter::zip(subject, expected)
                .any(|(subject_item, expected_item)| !Gc::ptr_eq(subject_item, expected_item))
        {
            AssertionFailure::from_spec(self)
                .with_expected(format!("slice of Gc's with length {}", expected.len()))
                .with_actual(format!("slice of Gc's with length {}", subject.len()))
                .fail();
        }
    }
}
