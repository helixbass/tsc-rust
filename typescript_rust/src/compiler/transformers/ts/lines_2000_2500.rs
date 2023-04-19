use gc::Gc;

use crate::{Node, VisitResult};

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn visit_method_declaration(
        &self,
        node: &Node, /*MethodDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_get_accessor(
        &self,
        node: &Node, /*GetAccessorDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_set_accessor(
        &self,
        node: &Node, /*SetAccessorDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_function_declaration(
        &self,
        node: &Node, /*FunctionDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_function_expression(
        &self,
        node: &Node, /*FunctionExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_arrow_function(&self, node: &Node /*ArrowFunction*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_parameter(&self, node: &Node /*ParameterDeclaration*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_variable_statement(
        &self,
        node: &Node, /*VariableStatement*/
    ) -> Option<Gc<Node /*Statement*/>> {
        unimplemented!()
    }

    pub(super) fn visit_variable_declaration(
        &self,
        node: &Node, /*VariableDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_parenthesized_expression(
        &self,
        node: &Node, /*ParenthesizedExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_assertion_expression(
        &self,
        node: &Node, /*AssertionExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_non_null_expression(
        &self,
        node: &Node, /*NonNullExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_call_expression(&self, node: &Node /*CallExpression*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_new_expression(&self, node: &Node /*NewExpression*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_tagged_template_expression(
        &self,
        node: &Node, /*TaggedTemplateExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_jsx_self_closing_element(
        &self,
        node: &Node, /*JsxSelfClosingElement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_jsx_jsx_opening_element(
        &self,
        node: &Node, /*JsxOpeningElement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_enum_declaration(
        &self,
        node: &Node, /*EnumDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }
}
