use std::ptr;

use gc::Gc;

use crate::{
    has_syntactic_modifier, is_modifier, move_range_past_decorators, node_is_missing,
    set_comment_range, set_source_map_range, visit_function_body, visit_nodes,
    visit_parameter_list, FunctionLikeDeclarationInterface, ModifierFlags, Node, NodeArray,
    NodeInterface, SignatureDeclarationInterface, VisitResult,
};

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn visit_method_declaration(
        &self,
        node: &Node, /*MethodDeclaration*/
    ) -> VisitResult {
        let node_as_method_declaration = node.as_method_declaration();
        if !self.should_emit_function_like_declaration(node) {
            return None;
        }
        let updated = self.factory.update_method_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            visit_nodes(
                node.maybe_modifiers().as_deref(),
                Some(|node: &Node| self.modifier_visitor(node)),
                Some(is_modifier),
                None,
                None,
            ),
            node_as_method_declaration.maybe_asterisk_token(),
            self.visit_property_name_of_class_element(node),
            None,
            Option::<Gc<NodeArray>>::None,
            visit_parameter_list(
                Some(&node_as_method_declaration.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            None,
            visit_function_body(
                node_as_method_declaration.maybe_body().as_deref(),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            ),
        );
        if !ptr::eq(&*updated, node) {
            set_comment_range(&updated, node);
            set_source_map_range(
                updated.clone(),
                Some((&move_range_past_decorators(node)).into()),
            );
        }
        Some(updated.into())
    }

    pub(super) fn should_emit_accessor_declaration(
        &self,
        node: &Node, /*AccessorDeclaration*/
    ) -> bool {
        !(node_is_missing(node.as_function_like_declaration().maybe_body())
            && has_syntactic_modifier(node, ModifierFlags::Abstract))
    }

    pub(super) fn visit_get_accessor(
        &self,
        node: &Node, /*GetAccessorDeclaration*/
    ) -> VisitResult {
        let node_as_get_accessor_declaration = node.as_get_accessor_declaration();
        if !self.should_emit_accessor_declaration(node) {
            return None;
        }
        let updated = self.factory.update_get_accessor_declaration(
            node,
            Option::<Gc<NodeArray>>::None,
            visit_nodes(
                node.maybe_modifiers().as_deref(),
                Some(|node: &Node| self.modifier_visitor(node)),
                Some(is_modifier),
                None,
                None,
            ),
            self.visit_property_name_of_class_element(node),
            visit_parameter_list(
                Some(&node_as_get_accessor_declaration.parameters()),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            )
            .unwrap(),
            None,
            Some(
                visit_function_body(
                    node_as_get_accessor_declaration.maybe_body().as_deref(),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                    Option::<
                        fn(
                            Option<&Node>,
                            Option<&mut dyn FnMut(&Node) -> VisitResult>,
                            Option<&dyn Fn(&Node) -> bool>,
                            Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                        ) -> Option<Gc<Node>>,
                    >::None,
                )
                .unwrap_or_else(|| self.factory.create_block(vec![], None).wrap()),
            ),
        );
        if !ptr::eq(&*updated, node) {
            set_comment_range(&updated, node);
            set_source_map_range(
                updated.clone(),
                Some((&move_range_past_decorators(node)).into()),
            );
        }
        Some(updated.into())
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
