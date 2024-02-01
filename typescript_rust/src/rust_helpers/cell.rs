use std::cell::{Ref, RefCell, RefMut};

use gc::{Finalize, GcCell, GcCellRef, GcCellRefMut, Trace};

pub fn ref_unwrapped<TValue>(ref_cell: &RefCell<Option<TValue>>) -> Ref<TValue> {
    Ref::map(ref_cell.borrow(), |value| value.as_ref().unwrap())
}

pub fn ref_mut_unwrapped<TValue>(ref_cell: &RefCell<Option<TValue>>) -> RefMut<TValue> {
    RefMut::map(ref_cell.borrow_mut(), |value| value.as_mut().unwrap())
}
