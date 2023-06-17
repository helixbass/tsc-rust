use gc::Gc;

use super::TransformModule;
use crate::{Node, ReadonlyTextRange, VisitResult};

impl TransformModule {
    pub(super) fn visit_import_equals_declaration(
        &self,
        _node: &Node, /*ImportEqualsDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_export_declaration(
        &self,
        _node: &Node, /*ExportDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_export_assignment(
        &self,
        _node: &Node, /*ExportAssignment*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_function_declaration(
        &self,
        _node: &Node, /*FunctionDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_class_declaration(
        &self,
        _node: &Node, /*ClassDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_variable_statement(
        &self,
        _node: &Node, /*VariableStatement*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn create_all_export_expressions(
        &self,
        _name: &Node,  /*Identifier*/
        _value: &Node, /*Expression*/
        _location: Option<&(impl ReadonlyTextRange + ?Sized)>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn visit_merge_declaration_marker(
        &self,
        _node: &Node, /*MergeDeclarationMarker*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_end_of_declaration_marker(
        &self,
        _node: &Node, /*EndOfDeclarationMarker*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }
}
