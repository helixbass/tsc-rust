use gc::{Gc, GcCell, GcCellRefMut};

use super::{
    ClassLexicalEnvironment, PrivateIdentifierEnvironment, PrivateIdentifierInfo,
    TransformClassFields,
};
use crate::{Node, _d};

impl TransformClassFields {
    pub(super) fn visit_invalid_super_property(
        &self,
        _name: &Node, /*SuperProperty*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn get_property_name_expression_if_needed(
        &self,
        _name: &Node, /*PropertyName*/
        _should_hoist: bool,
    ) -> Option<Gc<Node /*Expression*/>> {
        unimplemented!()
    }

    pub(super) fn start_class_lexical_environment(&self) {
        unimplemented!()
    }

    pub(super) fn end_class_lexical_environment(&self) {
        unimplemented!()
    }

    pub(super) fn get_class_lexical_environment(&self) -> Gc<GcCell<ClassLexicalEnvironment>> {
        self.maybe_current_class_lexical_environment()
            .get_or_insert_with(|| _d())
            .clone()
    }

    pub(super) fn get_private_identifier_environment(
        &self,
    ) -> Gc<GcCell<PrivateIdentifierEnvironment>> {
        let lex = self.get_class_lexical_environment();
        let ret = lex
            .borrow_mut()
            .private_identifier_environment
            .get_or_insert_with(|| _d())
            .clone();
        ret
    }

    pub(super) fn get_pending_expressions(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<Node>>>, Vec<Gc<Node>>> {
        self.maybe_pending_expressions_mut()
            .get_or_insert_with(|| _d());
        self.pending_expressions_mut()
    }

    pub(super) fn add_private_identifier_to_environment(
        &self,
        _node: &Node, /*PrivateClassElementDeclaration*/
    ) {
        unimplemented!()
    }

    pub(super) fn create_hoisted_variable_for_class(
        &self,
        _name: &str,  /*PrivateIdentifier*/
        _node: &Node, /*PrivateIdentifier | ClassStaticBlockDeclaration*/
    ) -> Gc<Node /*Identifier*/> {
        unimplemented!()
    }

    pub(super) fn access_private_identifier(
        &self,
        _name: &Node, /*PrivateIdentifier*/
    ) -> Option<PrivateIdentifierInfo> {
        unimplemented!()
    }
}
