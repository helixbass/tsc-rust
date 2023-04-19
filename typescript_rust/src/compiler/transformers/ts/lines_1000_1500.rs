use gc::Gc;

use crate::Node;

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn add_class_element_decoration_statements(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        node: &Node, /*ClassDeclaration*/
        is_static: bool,
    ) {
        unimplemented!()
    }

    pub(super) fn add_constructor_decoration_statement(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        node: &Node, /*ClassDeclaration*/
    ) {
        unimplemented!()
    }
}
