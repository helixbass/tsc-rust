use gc::Gc;

use crate::{Node, VisitResult};

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn visit_method_declaration(
        &self,
        _node: &Node, /*MethodDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_get_accessor(
        &self,
        _node: &Node, /*GetAccessorDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_set_accessor(
        &self,
        _node: &Node, /*SetAccessorDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_function_declaration(
        &self,
        _node: &Node, /*FunctionDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_function_expression(
        &self,
        _node: &Node, /*FunctionExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_arrow_function(&self, _node: &Node /*ArrowFunction*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_parameter(
        &self,
        _node: &Node, /*ParameterDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_variable_statement(
        &self,
        _node: &Node, /*VariableStatement*/
    ) -> Option<Gc<Node /*Statement*/>> {
        unimplemented!()
    }

    pub(super) fn visit_variable_declaration(
        &self,
        _node: &Node, /*VariableDeclaration*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_parenthesized_expression(
        &self,
        _node: &Node, /*ParenthesizedExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_assertion_expression(
        &self,
        _node: &Node, /*AssertionExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_non_null_expression(
        &self,
        _node: &Node, /*NonNullExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_call_expression(
        &self,
        _node: &Node, /*CallExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_new_expression(&self, _node: &Node /*NewExpression*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_tagged_template_expression(
        &self,
        _node: &Node, /*TaggedTemplateExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_jsx_self_closing_element(
        &self,
        _node: &Node, /*JsxSelfClosingElement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_jsx_jsx_opening_element(
        &self,
        _node: &Node, /*JsxOpeningElement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_enum_declaration(
        &self,
        _node: &Node, /*EnumDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }
}
