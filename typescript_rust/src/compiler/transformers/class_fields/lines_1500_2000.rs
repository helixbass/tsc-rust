use super::{PrivateIdentifierInfo, TransformClassFields};
use crate::{Node, VisitResult};

impl TransformClassFields {
    pub(super) fn access_private_identifier(
        &self,
        _name: &Node, /*PrivateIdentifier*/
    ) -> Option<PrivateIdentifierInfo> {
        unimplemented!()
    }
}
