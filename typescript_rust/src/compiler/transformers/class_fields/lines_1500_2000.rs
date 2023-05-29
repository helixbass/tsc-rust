use gc::{Gc, GcCellRefMut};

use super::{PrivateIdentifierInfo, TransformClassFields};
use crate::{Node, VisitResult, _d};

impl TransformClassFields {
    pub(super) fn visit_invalid_super_property(
        &self,
        _name: &Node, /*SuperProperty*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn get_property_name_expression_if_needed(
        &self,
        _name: &Node, /*PropertyName*/
        _should_hoist: bool,
    ) -> Option<Gc<Node /*Expression*/>> {
        unimplemented!()
    }

    pub(super) fn get_pending_expressions(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Node>>>, Vec<Gc<Node>>> {
        self.maybe_pending_expressions_mut()
            .get_or_insert_with(|| _d());
        self.pending_expressions_mut()
    }

    pub(super) fn access_private_identifier(
        &self,
        _name: &Node, /*PrivateIdentifier*/
    ) -> Option<PrivateIdentifierInfo> {
        unimplemented!()
    }
}
