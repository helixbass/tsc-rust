use std::io;

use gc::Gc;

use super::{HierarchyFacts, Jump, TransformES2015};
use crate::{
    add_range, concatenate, is_expression, is_statement, try_visit_each_child, try_visit_node,
    try_visit_nodes, visit_each_child, visit_node, visit_nodes, AsDoubleDeref,
    GeneratedIdentifierFlags, HasStatementsInterface, Node, NodeArray, NodeArrayExt, NodeExt,
    NodeInterface, OptionTry, VisitResult,
};

impl TransformES2015 {
    pub(super) fn visit_source_file(
        &self,
        node: &Node, /*SourceFile*/
    ) -> io::Result<Gc<Node /*SourceFile*/>> {
        let node_as_source_file = node.as_source_file();
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::SourceFileExcludes,
            HierarchyFacts::SourceFileIncludes,
        );
        let mut prologue: Vec<Gc<Node /*Statement*/>> = Default::default();
        let mut statements: Vec<Gc<Node /*Statement*/>> = Default::default();
        self.context.start_lexical_environment();
        let statement_offset = self.factory.try_copy_prologue(
            &node_as_source_file.statements(),
            &mut prologue,
            Some(false),
            Some(|node: &Node| self.visitor(node)),
        )?;
        add_range(
            &mut statements,
            try_visit_nodes(
                Some(&node_as_source_file.statements()),
                Some(|node: &Node| self.visitor(node)),
                Some(is_statement),
                Some(statement_offset),
                None,
            )?
            .as_double_deref(),
            None,
            None,
        );
        if let Some(tagged_template_string_declarations) =
            self.maybe_tagged_template_string_declarations().as_ref()
        {
            statements.push(
                self.factory
                    .create_variable_statement(
                        Option::<Gc<NodeArray>>::None,
                        self.factory
                            .create_variable_declaration_list(
                                tagged_template_string_declarations.clone(),
                                None,
                            )
                            .wrap(),
                    )
                    .wrap(),
            );
        }
        prologue = self
            .factory
            .merge_lexical_environment(prologue, self.context.end_lexical_environment().as_deref())
            .as_vec_owned();
        self.insert_capture_this_for_node_if_needed(&mut prologue, node);
        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(self.factory.update_source_file(
            node,
            self.factory
                .create_node_array(Some(concatenate(prologue, statements)), None)
                .set_text_range(Some(&*node_as_source_file.statements())),
            None,
            None,
            None,
            None,
            None,
        ))
    }

    pub(super) fn visit_switch_statement(
        &self,
        node: &Node, /*SwitchStatement*/
    ) -> io::Result<Gc<Node /*SwitchStatement*/>> {
        if self.maybe_converted_loop_state().is_some() {
            let saved_allowed_non_labeled_jumps = self
                .maybe_converted_loop_state()
                .as_ref()
                .unwrap()
                .allowed_non_labeled_jumps
                .clone();
            {
                let mut converted_loop_state = self.maybe_converted_loop_state_mut();
                let converted_loop_state = converted_loop_state.as_mut().unwrap();
                converted_loop_state.allowed_non_labeled_jumps = Some(
                    converted_loop_state
                        .allowed_non_labeled_jumps
                        .unwrap_or_default()
                        | Jump::Break,
                );
            }
            let result =
                try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?;
            self.maybe_converted_loop_state_mut()
                .as_mut()
                .unwrap()
                .allowed_non_labeled_jumps = saved_allowed_non_labeled_jumps;
            return Ok(result);
        }
        try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn visit_case_block(
        &self,
        node: &Node, /*CaseBlock*/
    ) -> io::Result<Gc<Node /*CaseBlock*/>> {
        let ancestor_facts = self.enter_subtree(
            HierarchyFacts::BlockScopeExcludes,
            HierarchyFacts::BlockScopeIncludes,
        );
        let updated =
            try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context)?;
        self.exit_subtree(ancestor_facts, HierarchyFacts::None, HierarchyFacts::None);
        Ok(updated)
    }

    pub(super) fn return_captured_this(&self, node: &Node) -> Gc<Node /*ReturnStatement*/> {
        self.factory
            .create_return_statement(Some(self.factory.create_unique_name(
                "_this",
                Some(GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel),
            )))
            .wrap()
            .set_original_node(Some(node.node_wrapper()))
    }

    pub(super) fn visit_return_statement(
        &self,
        node: &Node, /*ReturnStatement*/
    ) -> io::Result<Gc<Node /*Statement*/>> {
        let mut node = node.node_wrapper();
        if self.maybe_converted_loop_state().is_some() {
            {
                let mut converted_loop_state = self.maybe_converted_loop_state_mut();
                let converted_loop_state = converted_loop_state.as_mut().unwrap();
                converted_loop_state.non_local_jumps =
                    Some(converted_loop_state.non_local_jumps.unwrap_or_default() | Jump::Return);
            }
            if self.is_return_void_statement_in_constructor_with_captured_super(&node) {
                node = self.return_captured_this(&node);
            }
            return Ok(self
                .factory
                .create_return_statement(Some(
                    self.factory
                        .create_object_literal_expression(
                            Some(vec![self
                                .factory
                                .create_property_assignment(
                                    self.factory
                                        .create_identifier(
                                            "value",
                                            Option::<Gc<NodeArray>>::None,
                                            None,
                                        )
                                        .wrap(),
                                    node.as_return_statement()
                                        .expression
                                        .as_ref()
                                        .try_map_or_else(
                                            || Ok(self.factory.create_void_zero()),
                                            |node_expression| -> io::Result<_> {
                                                Ok(try_visit_node(
                                                    Some(&**node_expression),
                                                    Some(|node: &Node| self.visitor(node)),
                                                    Some(is_expression),
                                                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                                )?
                                                .unwrap())
                                            },
                                        )?,
                                )
                                .wrap()]),
                            None,
                        )
                        .wrap(),
                ))
                .wrap());
        } else if self.is_return_void_statement_in_constructor_with_captured_super(&node) {
            return Ok(self.return_captured_this(&node));
        }
        try_visit_each_child(&node, |node: &Node| self.visitor(node), &**self.context)
    }

    pub(super) fn visit_this_keyword(&self, node: &Node) -> Gc<Node> {
        if self
            .maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::ArrowFunction)
            && !self
                .maybe_hierarchy_facts()
                .unwrap_or_default()
                .intersects(HierarchyFacts::StaticInitializer)
        {
            self.set_hierarchy_facts(Some(
                self.maybe_hierarchy_facts().unwrap_or_default()
                    | HierarchyFacts::CapturedLexicalThis,
            ));
        }
        if self.maybe_converted_loop_state().is_some() {
            if self
                .maybe_hierarchy_facts()
                .unwrap_or_default()
                .intersects(HierarchyFacts::ArrowFunction)
            {
                self.converted_loop_state_mut().contains_lexical_this = Some(true);
                return node.node_wrapper();
            }
            return self
                .converted_loop_state_mut()
                .this_name
                .get_or_insert_with(|| self.factory.create_unique_name("this", None))
                .clone();
        }
        node.node_wrapper()
    }

    pub(super) fn visit_void_expression(
        &self,
        node: &Node, /*VoidExpression*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        try_visit_each_child(
            &node,
            |node: &Node| self.visitor_with_unused_expression_result(node),
            &**self.context,
        )
    }

    pub(super) fn visit_identifier(
        &self,
        node: &Node, /*Identifier*/
    ) -> io::Result<Gc<Node /*Identifier*/>> {
        if self.maybe_converted_loop_state().is_none() {
            return Ok(node.node_wrapper());
        }
        if self.resolver.is_arguments_local_binding(node)? {
            return Ok(self
                .converted_loop_state_mut()
                .arguments_name
                .get_or_insert_with(|| self.factory.create_unique_name("arguments", None))
                .clone());
        }
        Ok(node.node_wrapper())
    }

    pub(super) fn visit_break_or_continue_statement(
        &self,
        _node: &Node, /*BreakOrContinueStatement*/
    ) -> Gc<Node /*Statement*/> {
        unimplemented!()
    }

    pub(super) fn visit_class_declaration(
        &self,
        _node: &Node, /*ClassDeclaration*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_class_expression(
        &self,
        _node: &Node, /*ClassExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }
}
