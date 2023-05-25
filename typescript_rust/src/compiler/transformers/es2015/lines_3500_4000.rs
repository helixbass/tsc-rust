use std::io;

use gc::Gc;

use super::{HierarchyFacts, TransformES2015};
use crate::{
    create_member_access_for_property_name, get_emit_flags, is_binding_pattern,
    is_computed_property_name, is_property_name, is_spread_element, is_statement, move_range_pos,
    some, start_on_new_line, try_flatten_destructuring_binding, try_visit_each_child,
    try_visit_node, try_visit_nodes, try_visit_parameter_list, Debug_, EmitFlags, FlattenLevel,
    NamedDeclarationInterface, Node, NodeArray, NodeExt, NodeInterface, ReadonlyTextRangeConcrete,
    SyntaxKind, VecExt, VisitResult,
};

impl TransformES2015 {
    pub(super) fn transform_object_literal_method_declaration_to_expression(
        &self,
        method: &Node,   /*MethodDeclaration*/
        receiver: &Node, /*Expression*/
        container: &Node,
        starts_on_new_line: Option<bool>,
    ) -> io::Result<Gc<Node>> {
        let method_as_method_declaration = method.as_method_declaration();
        let expression = self
            .factory
            .create_assignment(
                create_member_access_for_property_name(
                    &self.factory,
                    receiver,
                    &try_visit_node(
                        Some(method_as_method_declaration.name()),
                        Some(|node: &Node| self.visitor(node)),
                        Some(is_property_name),
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?
                    .unwrap(),
                    Option::<&Node>::None,
                ),
                self.transform_function_like_to_expression(
                    method,
                    Some(method),
                    Option::<&Node>::None,
                    Some(container),
                )?,
            )
            .wrap()
            .set_text_range(Some(method));
        if starts_on_new_line == Some(true) {
            start_on_new_line(&*expression);
        }
        Ok(expression)
    }

    pub(super) fn visit_catch_clause(
        &self,
        node: &Node, /*CatchClause*/
    ) -> io::Result<Gc<Node /*CatchClause*/>> {
        let node_as_catch_clause = node.as_catch_clause();
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::BlockScopeExcludes,
            HierarchyFacts::BlockScopeIncludes,
        );
        let updated: Gc<Node /*CatchClause*/>;
        Debug_.assert(
            node_as_catch_clause.variable_declaration.is_some(),
            Some("Catch clause variable should always be present when downleveling ES2015."),
        );
        let node_variable_declaration = node_as_catch_clause.variable_declaration.as_ref().unwrap();
        if is_binding_pattern(
            node_variable_declaration
                .as_variable_declaration()
                .maybe_name(),
        ) {
            let temp = self
                .factory
                .create_temp_variable(Option::<fn(&Node)>::None, None);
            let new_variable_declaration = self
                .factory
                .create_variable_declaration(Some(temp.clone()), None, None, None)
                .wrap()
                .set_text_range(Some(&**node_variable_declaration));
            let vars = try_flatten_destructuring_binding(
                node_variable_declaration,
                |node: &Node| self.visitor(node),
                &**self.context,
                FlattenLevel::All,
                Some(&*temp),
                None,
                None,
            )?;
            let list = self
                .factory
                .create_variable_declaration_list(vars, None)
                .wrap()
                .set_text_range(Some(&**node_variable_declaration));
            let destructure = self
                .factory
                .create_variable_statement(Option::<Gc<NodeArray>>::None, list)
                .wrap();
            updated = self.factory.update_catch_clause(
                node,
                Some(new_variable_declaration),
                self.add_statement_to_start_of_block(&node_as_catch_clause.block, destructure)?,
            );
        } else {
            updated =
                try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?;
        }

        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(updated)
    }

    pub(super) fn add_statement_to_start_of_block(
        &self,
        block: &Node, /*Block*/
        statement: Gc<Node /*Statement*/>,
    ) -> io::Result<Gc<Node /*Block*/>> {
        let block_as_block = block.as_block();
        let transformed_statements = try_visit_nodes(
            Some(&block_as_block.statements),
            Some(|node: &Node| self.visitor(node)),
            Some(is_statement),
            None,
            None,
        )?
        .unwrap();
        Ok(self.factory.update_block(
            block,
            vec![statement].and_extend(transformed_statements.owned_iter()),
        ))
    }

    pub(super) fn visit_method_declaration(
        &self,
        node: &Node, /*MethodDeclaration*/
    ) -> io::Result<Gc<Node /*ObjectLiteralElementLike*/>> {
        let node_as_method_declaration = node.as_method_declaration();
        Debug_.assert(
            !is_computed_property_name(&node_as_method_declaration.name()),
            None,
        );
        let function_expression = self
            .transform_function_like_to_expression(
                node,
                Some(&ReadonlyTextRangeConcrete::from(move_range_pos(node, -1))),
                Option::<&Node>::None,
                Option::<&Node>::None,
            )?
            .set_additional_emit_flags(EmitFlags::NoLeadingComments);
        Ok(self
            .factory
            .create_property_assignment(node_as_method_declaration.name(), function_expression)
            .wrap()
            .set_text_range(Some(node)))
    }

    pub(super) fn visit_accessor_declaration(
        &self,
        node: &Node, /*AccessorDeclaration*/
    ) -> io::Result<Gc<Node /*AccessorDeclaration*/>> {
        let ref node_name = node.as_named_declaration().name();
        Debug_.assert(!is_computed_property_name(node_name), None);
        let saved_converted_loop_state = self.maybe_converted_loop_state();
        self.set_converted_loop_state(None);
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::FunctionExcludes,
            HierarchyFacts::FunctionIncludes,
        );
        let updated: Gc<Node /*AccessorDeclaration*/>;
        let parameters = try_visit_parameter_list(
            Some(&node.as_signature_declaration().parameters()),
            |node: &Node| self.visitor(node),
            &**self.context,
        )?
        .unwrap();
        let body = self.transform_function_body(node)?;
        if node.kind() == SyntaxKind::GetAccessor {
            updated = self.factory.update_get_accessor_declaration(
                node,
                node.maybe_decorators(),
                node.maybe_modifiers(),
                node_name.clone(),
                parameters,
                node.as_has_type().maybe_type(),
                Some(body),
            );
        } else {
            updated = self.factory.update_set_accessor_declaration(
                node,
                node.maybe_decorators(),
                node.maybe_modifiers(),
                node_name.clone(),
                parameters,
                Some(body),
            );
        }
        self.exit_subtree(
            ancestor_facts,
            HierarchyFacts::FunctionSubtreeExcludes,
            HierarchyFacts::None,
        );
        self.set_converted_loop_state(saved_converted_loop_state);
        Ok(updated)
    }

    pub(super) fn visit_shorthand_property_assignment(
        &self,
        node: &Node, /*ShorthandPropertyAssignment*/
    ) -> io::Result<Gc<Node /*ObjectLiteralElementLike*/>> {
        let node_as_shorthand_property_assignment = node.as_shorthand_property_assignment();
        Ok(self
            .factory
            .create_property_assignment(
                node_as_shorthand_property_assignment.name(),
                self.visit_identifier(
                    &self
                        .factory
                        .clone_node(&node_as_shorthand_property_assignment.name()),
                )?,
            )
            .wrap()
            .set_text_range(Some(node)))
    }

    pub(super) fn visit_computed_property_name(
        &self,
        node: &Node, /*ComputedPropertyName*/
    ) -> io::Result<VisitResult> {
        Ok(Some(
            try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?.into(),
        ))
    }

    pub(super) fn visit_yield_expression(
        &self,
        node: &Node, /*YieldExpression*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn visit_array_literal_expression(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        let node_as_array_literal_expression = node.as_array_literal_expression();
        if some(
            Some(&node_as_array_literal_expression.elements),
            Some(|element: &Gc<Node>| is_spread_element(element)),
        ) {
            return Ok(self.transform_and_spread_elements(
                &node_as_array_literal_expression.elements,
                false,
                node_as_array_literal_expression.multi_line == Some(true),
                node_as_array_literal_expression.elements.has_trailing_comma,
            ));
        }
        try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn visit_call_expression(
        &self,
        _node: &Node, /*CallExpression*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_immediate_super_call_in_body(
        &self,
        _node: &Node, /*CallExpression*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn visit_new_expression(
        &self,
        _node: &Node, /*CallExpression*/
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        unimplemented!()
    }

    pub(super) fn transform_and_spread_elements(
        &self,
        _elements: &NodeArray, /*<Expression>*/
        _is_argument_list: bool,
        _multi_line: bool,
        _has_trailing_comma: bool,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }
}
