use gc::Gc;

use crate::Node;

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn is_export_of_namespace(&self, _node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_named_external_module_export(&self, _node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_default_external_module_export(&self, _node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn add_export_member_assignment(
        &self,
        _statements: &mut Vec<Gc<Node /*Statement*/>>,
        _node: &Node, /*ClassDeclaration | FunctionDeclaration*/
    ) {
        unimplemented!()
    }

    pub(super) fn get_class_alias_if_needed(
        &self,
        _node: &Node, /*ClassDeclaration*/
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }
}
