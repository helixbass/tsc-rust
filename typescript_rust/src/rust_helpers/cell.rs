use gc::{Finalize, GcCell, GcCellRef, Trace};

pub fn gc_cell_ref_unwrapped<TValue: Trace + Finalize>(
    gc_cell: &GcCell<Option<TValue>>,
) -> GcCellRef<TValue> {
    GcCellRef::map(gc_cell.borrow(), |value| value.as_ref().unwrap())
}
