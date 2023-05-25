use std::io;

use gc::Gc;

use super::{HierarchyFacts, TransformES2015};
use crate::{
    create_member_access_for_property_name, is_binding_pattern, is_property_name, is_statement,
    start_on_new_line, try_flatten_destructuring_binding, try_visit_each_child, try_visit_node,
    try_visit_nodes, Debug_, FlattenLevel, NamedDeclarationInterface, Node, NodeArray, NodeExt,
    NodeInterface, VecExt, VisitResult,
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
        _node: &Node, /*MethodDeclaration*/
    ) -> Gc<Node /*ObjectLiteralElementLike*/> {
        unimplemented!()
    }

    pub(super) fn visit_accessor_declaration(
        &self,
        _node: &Node, /*AccessorDeclaration*/
    ) -> Gc<Node /*AccessorDeclaration*/> {
        unimplemented!()
    }

    pub(super) fn visit_shorthand_property_assignment(
        &self,
        _node: &Node, /*ShorthandPropertyAssignment*/
    ) -> Gc<Node /*ObjectLiteralElementLike*/> {
        unimplemented!()
    }

    pub(super) fn visit_computed_property_name(
        &self,
        _node: &Node, /*ComputedPropertyName*/
    ) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_yield_expression(
        &self,
        _node: &Node, /*YieldExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_array_literal_expression(
        &self,
        _node: &Node, /*ArrayLiteralExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
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
}
