use gc::Gc;

use super::TransformModule;
use crate::{
    flatten_destructuring_assignment, is_array_literal_expression, is_destructuring_assignment,
    is_export_name, is_identifier, is_import_call, is_object_literal_expression, is_spread_element,
    length, visit_each_child, Debug_, FlattenLevel, NamedDeclarationInterface, Node, NodeInterface,
    ReadonlyTextRange, SyntaxKind, TransformFlags, VisitResult,
};

impl TransformModule {
    pub(super) fn top_level_visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        match node.kind() {
            SyntaxKind::ImportDeclaration => self.visit_import_declaration(node),
            SyntaxKind::ImportEqualsDeclaration => self.visit_import_equals_declaration(node),
            SyntaxKind::ExportDeclaration => self.visit_export_declaration(node),
            SyntaxKind::ExportAssignment => self.visit_export_assignment(node),
            SyntaxKind::VariableStatement => self.visit_variable_statement(node),
            SyntaxKind::FunctionDeclaration => self.visit_function_declaration(node),
            SyntaxKind::ClassDeclaration => self.visit_class_declaration(node),
            SyntaxKind::MergeDeclarationMarker => self.visit_merge_declaration_marker(node),
            SyntaxKind::EndOfDeclarationMarker => self.visit_end_of_declaration_marker(node),
            _ => self.visitor(node),
        }
    }

    pub(super) fn visitor_worker(&self, node: &Node, value_is_discarded: bool) -> VisitResult /*<Node>*/
    {
        if !node.transform_flags().intersects(
            TransformFlags::ContainsDynamicImport
                | TransformFlags::ContainsDestructuringAssignment
                | TransformFlags::ContainsUpdateExpressionForIdentifier,
        ) {
            return Some(node.node_wrapper().into());
        }

        match node.kind() {
            SyntaxKind::ForStatement => return self.visit_for_statement(node),
            SyntaxKind::ExpressionStatement => return self.visit_expression_statement(node),
            SyntaxKind::ParenthesizedExpression => {
                return self.visit_parenthesized_expression(node, value_is_discarded)
            }
            SyntaxKind::PartiallyEmittedExpression => {
                return self.visit_partially_emitted_expression(node, value_is_discarded)
            }
            SyntaxKind::CallExpression => {
                if is_import_call(node)
                    && self
                        .current_source_file()
                        .as_source_file()
                        .maybe_implied_node_format()
                        .is_none()
                {
                    return Some(self.visit_import_call_expression(node).into());
                }
            }
            SyntaxKind::BinaryExpression => {
                if is_destructuring_assignment(node) {
                    return Some(
                        self.visit_destructuring_assignment(node, value_is_discarded)
                            .into(),
                    );
                }
            }
            SyntaxKind::PrefixUnaryExpression | SyntaxKind::PostfixUnaryExpression => {
                return self.visit_pre_or_postfix_unary_expression(node, value_is_discarded)
            }
            _ => (),
        }

        Some(visit_each_child(node, |node: &Node| self.visitor(node), &**self.context).into())
    }

    pub(super) fn visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        self.visitor_worker(node, false)
    }

    pub(super) fn discarded_value_visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        self.visitor_worker(node, true)
    }

    pub(super) fn destructuring_needs_flattening(&self, node: &Node /*Expression*/) -> bool {
        if is_object_literal_expression(node) {
            for elem in &node.as_object_literal_expression().properties {
                match elem.kind() {
                    SyntaxKind::PropertyAssignment => {
                        if self.destructuring_needs_flattening(
                            &elem.as_property_assignment().initializer,
                        ) {
                            return true;
                        }
                    }
                    SyntaxKind::ShorthandPropertyAssignment => {
                        if self.destructuring_needs_flattening(
                            &elem.as_shorthand_property_assignment().name(),
                        ) {
                            return true;
                        }
                    }
                    SyntaxKind::SpreadAssignment => {
                        if self
                            .destructuring_needs_flattening(&elem.as_spread_assignment().expression)
                        {
                            return true;
                        }
                    }
                    SyntaxKind::MethodDeclaration
                    | SyntaxKind::GetAccessor
                    | SyntaxKind::SetAccessor => return false,
                    _ => Debug_.assert_never(elem, Some("Unhandled object member kind")),
                }
            }
        } else if is_array_literal_expression(node) {
            for elem in &node.as_array_literal_expression().elements {
                if is_spread_element(elem) {
                    if self.destructuring_needs_flattening(&elem.as_spread_element().expression) {
                        return true;
                    }
                } else if self.destructuring_needs_flattening(elem) {
                    return true;
                }
            }
        } else if is_identifier(node) {
            return length(self.get_exports(node).as_deref())
                > if is_export_name(node) { 1 } else { 0 };
        }
        false
    }

    pub(super) fn visit_destructuring_assignment(
        &self,
        node: &Node, /*DestructuringAssignment*/
        value_is_discarded: bool,
    ) -> Gc<Node /*Expression*/> {
        if self.destructuring_needs_flattening(&node.as_binary_expression().left) {
            return flatten_destructuring_assignment(
                node,
                Some(|node: &Node| self.visitor(node)),
                &**self.context,
                FlattenLevel::All,
                Some(!value_is_discarded),
                Some(|a: &Node, b: &Node, c: Option<&dyn ReadonlyTextRange>| {
                    self.create_all_export_expressions(a, b, c)
                }),
            );
        }
        visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn visit_for_statement(&self, _node: &Node /*ForStatement*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_expression_statement(
        &self,
        _node: &Node, /*ExpressionStatement*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_parenthesized_expression(
        &self,
        _node: &Node, /*ParenthesizedExpression*/
        _value_is_discarded: bool,
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_partially_emitted_expression(
        &self,
        _node: &Node, /*PartiallyEmittedExpression*/
        _value_is_discarded: bool,
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_pre_or_postfix_unary_expression(
        &self,
        _node: &Node, /*PrefixUnaryExpression | PostfixUnaryExpression*/
        _value_is_discarded: bool,
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_import_call_expression(
        &self,
        _node: &Node, /*ImportCall*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn get_helper_expression_for_import(
        &self,
        _node: &Node,       /*ImportDeclaration*/
        _inner_expr: &Node, /*Expression*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn visit_import_declaration(
        &self,
        _node: &Node, /*ImportDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }
}
