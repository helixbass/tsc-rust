use crate::{Node, VisitResult};

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn record_emitted_declaration_in_scope(
        &self,
        _node: &Node, /*FunctionDeclaration | ClassDeclaration | ModuleDeclaration | EnumDeclaration*/
    ) {
        unimplemented!()
    }

    pub(super) fn visit_module_declaration(
        &self,
        _node: &Node, /*ModuleDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_import_declaration(
        &self,
        _node: &Node, /*ImportDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_export_assignment(
        &self,
        _node: &Node, /*ExportAssignment*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_export_declaration(
        &self,
        _node: &Node, /*ExportDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_import_equals_declaration(
        &self,
        _node: &Node, /*ImportEqualsDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }
}
