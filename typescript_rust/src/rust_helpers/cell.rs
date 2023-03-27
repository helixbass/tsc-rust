use gc::{Finalize, GcCell, GcCellRef, GcCellRefMut, Trace};

pub fn gc_cell_ref_unwrapped<TValue: Trace + Finalize>(
    gc_cell: &GcCell<Option<TValue>>,
) -> GcCellRef<TValue> {
    GcCellRef::map(gc_cell.borrow(), |value| value.as_ref().unwrap())
}

pub fn gc_cell_ref_mut_unwrapped<TValue: Trace + Finalize>(
    gc_cell: &GcCell<Option<TValue>>,
) -> GcCellRefMut<Option<TValue>, TValue> {
    GcCellRefMut::map(gc_cell.borrow_mut(), |value| value.as_mut().unwrap())
}
