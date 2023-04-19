use gc::Gc;

use crate::Node;

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn is_export_of_namespace(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_named_external_module_export(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_default_external_module_export(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn add_export_member_assignment(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        node: &Node, /*ClassDeclaration | FunctionDeclaration*/
    ) {
        unimplemented!()
    }
}
