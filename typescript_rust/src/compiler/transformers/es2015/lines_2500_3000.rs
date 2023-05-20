use std::{borrow::Borrow, io};

use gc::Gc;

use super::{ConvertedLoopState, HierarchyFacts, TransformES2015};
use crate::{
    is_expression, is_identifier, try_visit_node, Debug_, EmitFlags, Node, NodeArray, NodeExt,
    NodeInterface, SyntaxKind, TransformFlags, VisitResult, _d, is_object_literal_element_like,
    start_on_new_line, try_visit_each_child, try_visit_nodes,
};

impl TransformES2015 {
    pub(super) fn convert_for_of_statement_for_iterable(
        &self,
        node: &Node, /*ForOfStatement*/
        outermost_labeled_statement: Option<&Node /*LabeledStatement*/>,
        converted_loop_body_statements: Option<&[Gc<Node /*Statement*/>]>,
        ancestor_facts: Option<HierarchyFacts>,
    ) -> io::Result<Gc<Node /*Statement*/>> {
        let node_as_for_of_statement = node.as_for_of_statement();
        let ref expression = try_visit_node(
            Some(&*node_as_for_of_statement.expression),
            Some(|node: &Node| self.visitor(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?
        .unwrap();
        let iterator = if is_identifier(expression) {
            self.factory
                .get_generated_name_for_node(Some(&**expression), None)
        } else {
            self.factory
                .create_temp_variable(Option::<fn(&Node)>::None, None)
        };
        let result = if is_identifier(expression) {
            self.factory
                .get_generated_name_for_node(Some(&*iterator), None)
        } else {
            self.factory
                .create_temp_variable(Option::<fn(&Node)>::None, None)
        };
        let error_record = self.factory.create_unique_name("e", None);
        let catch_variable = self
            .factory
            .get_generated_name_for_node(Some(&*error_record), None);
        let return_method = self
            .factory
            .create_temp_variable(Option::<fn(&Node)>::None, None);
        let values = self
            .emit_helpers()
            .create_values_helper(expression.clone())
            .set_text_range(Some(&*node_as_for_of_statement.expression));
        let next = self
            .factory
            .create_call_expression(
                self.factory
                    .create_property_access_expression(iterator.clone(), "next")
                    .wrap(),
                Option::<Gc<NodeArray>>::None,
                Some(vec![]),
            )
            .wrap();

        self.context.hoist_variable_declaration(&error_record);
        self.context.hoist_variable_declaration(&return_method);

        let initializer = if ancestor_facts
            .unwrap_or_default()
            .intersects(HierarchyFacts::IterationContainer)
        {
            self.factory.inline_expressions(&[
                self.factory
                    .create_assignment(error_record.clone(), self.factory.create_void_zero())
                    .wrap(),
                values.clone(),
            ])
        } else {
            values.clone()
        };

        let for_statement = self
            .factory
            .create_for_statement(
                Some(
                    self.factory
                        .create_variable_declaration_list(
                            vec![
                                self.factory
                                    .create_variable_declaration(
                                        Some(iterator.clone()),
                                        None,
                                        None,
                                        Some(initializer),
                                    )
                                    .wrap()
                                    .set_text_range(Some(&*node_as_for_of_statement.expression)),
                                self.factory
                                    .create_variable_declaration(
                                        Some(result.clone()),
                                        None,
                                        None,
                                        Some(next.clone()),
                                    )
                                    .wrap(),
                            ],
                            None,
                        )
                        .wrap()
                        .set_text_range(Some(&*node_as_for_of_statement.expression))
                        .set_emit_flags(EmitFlags::NoHoisting),
                ),
                Some(
                    self.factory
                        .create_logical_not(
                            self.factory
                                .create_property_access_expression(result.clone(), "done")
                                .wrap(),
                        )
                        .wrap(),
                ),
                Some(self.factory.create_assignment(result.clone(), next).wrap()),
                self.convert_for_of_statement_head(
                    node,
                    &self
                        .factory
                        .create_property_access_expression(result.clone(), "value")
                        .wrap(),
                    converted_loop_body_statements,
                )?,
            )
            .wrap()
            .set_text_range(Some(node))
            .set_emit_flags(EmitFlags::NoTokenTrailingSourceMaps);

        Ok(self
            .factory
            .create_try_statement(
                self.factory
                    .create_block(
                        vec![self.factory.restore_enclosing_label(
                            &for_statement,
                            outermost_labeled_statement,
                            self.maybe_converted_loop_state().as_ref().map(|_| {
                                |node: &Node| {
                                    self.reset_label(node);
                                }
                            }),
                        )],
                        None,
                    )
                    .wrap(),
                Some(
                    self.factory
                        .create_catch_clause(
                            Some(
                                self.factory
                                    .create_variable_declaration(
                                        Some(catch_variable.clone()),
                                        None,
                                        None,
                                        None,
                                    )
                                    .wrap(),
                            ),
                            self.factory
                                .create_block(
                                    vec![self
                                        .factory
                                        .create_expression_statement(
                                            self.factory
                                                .create_assignment(
                                                    error_record.clone(),
                                                    self.factory
                                                        .create_object_literal_expression(
                                                            Some(vec![self
                                                                .factory
                                                                .create_property_assignment(
                                                                    "error",
                                                                    catch_variable,
                                                                )
                                                                .wrap()]),
                                                            None,
                                                        )
                                                        .wrap(),
                                                )
                                                .wrap(),
                                        )
                                        .wrap()],
                                    None,
                                )
                                .wrap()
                                .set_emit_flags(EmitFlags::SingleLine),
                        )
                        .wrap(),
                ),
                Some(
                    self.factory
                        .create_block(
                            vec![self
                                .factory
                                .create_try_statement(
                                    self.factory
                                        .create_block(
                                            vec![
                                                self.factory.create_if_statement(
                                                    self.factory.create_logical_and(
                                                        self.factory.create_logical_and(
                                                            result.clone(),
                                                            self.factory.create_logical_not(
                                                                self.factory.create_property_access_expression(
                                                                    result,
                                                                    "done"
                                                                ).wrap()
                                                            ).wrap()
                                                        ).wrap(),
                                                        self.factory.create_assignment(
                                                            return_method.clone(),
                                                            self.factory.create_property_access_expression(
                                                                iterator.clone(),
                                                                "return"
                                                            ).wrap()
                                                        ).wrap()
                                                    ).wrap(),
                                                    self.factory.create_expression_statement(
                                                        self.factory.create_function_call_call(
                                                            return_method,
                                                            iterator,
                                                            vec![]
                                                        )
                                                    ).wrap(),
                                                    None,
                                                ).wrap()
                                                    .set_emit_flags(EmitFlags::SingleLine)
                                            ],
                                            None,
                                        )
                                        .wrap(),
                                    None,
                                    Some(
                                        self.factory
                                            .create_block(
                                                vec![
                                                    self.factory.create_if_statement(
                                                        error_record.clone(),
                                                        self.factory.create_throw_statement(
                                                            self.factory.create_property_access_expression(
                                                                error_record,
                                                                "error"
                                                            ).wrap()
                                                        ).wrap(),
                                                        None,
                                                    ).wrap()
                                                        .set_emit_flags(EmitFlags::SingleLine)
                                                ],
                                                None,
                                            )
                                            .wrap()
                                            .set_emit_flags(EmitFlags::SingleLine),
                                    ),
                                )
                                .wrap()],
                            None,
                        )
                        .wrap(),
                ),
            )
            .wrap())
    }

    pub(super) fn visit_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        let node_as_object_literal_expression = node.as_object_literal_expression();
        let properties = &node_as_object_literal_expression.properties;

        let mut num_initial_properties: Option<usize> = _d();
        let mut has_computed = false;
        for (i, property) in properties.iter().enumerate() {
            if property
                .transform_flags()
                .intersects(TransformFlags::ContainsYield)
                && self
                    .maybe_hierarchy_facts()
                    .unwrap_or_default()
                    .intersects(HierarchyFacts::AsyncFunctionBody)
                || {
                    has_computed = Debug_
                        .check_defined(property.as_named_declaration().maybe_name(), None)
                        .kind()
                        == SyntaxKind::ComputedPropertyName;
                    has_computed
                }
            {
                num_initial_properties = Some(i);
                break;
            }
        }

        if num_initial_properties.is_none() {
            return try_visit_each_child(node, |node: &Node| self.visitor(node), &**self.context);
        }
        let num_initial_properties = num_initial_properties.unwrap();

        let temp = self.factory.create_temp_variable(
            Some(|node: &Node| {
                self.context.hoist_variable_declaration(node);
            }),
            None,
        );

        let mut expressions: Vec<Gc<Node /*Expression*/>> = _d();
        let assignment = self
            .factory
            .create_assignment(
                temp.clone(),
                self.factory
                    .create_object_literal_expression(
                        try_visit_nodes(
                            Some(properties),
                            Some(|node: &Node| self.visitor(node)),
                            Some(is_object_literal_element_like),
                            Some(0),
                            Some(num_initial_properties),
                        )?,
                        node_as_object_literal_expression.multi_line,
                    )
                    .wrap()
                    .set_emit_flags(if has_computed {
                        EmitFlags::Indented
                    } else {
                        EmitFlags::None
                    }),
            )
            .wrap();

        if node_as_object_literal_expression.multi_line == Some(true) {
            start_on_new_line(&*assignment);
        }

        expressions.push(assignment);

        self.add_object_literal_members(&mut expressions, node, &temp, num_initial_properties);

        expressions.push(
            if node_as_object_literal_expression.multi_line == Some(true) {
                self.factory
                    .clone_node(&temp)
                    .set_text_range(Some(&*temp))
                    .and_set_parent(temp.maybe_parent())
                    .start_on_new_line()
            } else {
                temp
            },
        );
        Ok(self.factory.inline_expressions(&expressions))
    }

    pub(super) fn should_convert_iteration_statement(
        &self,
        _node: &Node, /*IterationStatement*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn hoist_variable_declaration_declared_in_converted_loop(
        &self,
        _state: &mut ConvertedLoopState,
        _node: &Node, /*VariableDeclaration*/
    ) {
        unimplemented!()
    }

    pub(super) fn convert_iteration_statement_body_if_necessary(
        &self,
        node: &Node, /*IterationStatement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
        ancestor_facts: Option<HierarchyFacts>,
        convert: Option<
            impl FnMut(
                &Node,         /*IterationStatement*/
                Option<&Node>, /*LabeledStatement*/
                Option<&[Gc<Node /*Statement*/>]>,
                Option<HierarchyFacts>,
            ) -> Gc<Node /*Statement*/>, /*LoopConverter*/
        >,
    ) -> VisitResult /*<Statement>*/ {
        self.try_convert_iteration_statement_body_if_necessary(
            node,
            outermost_labeled_statement,
            ancestor_facts,
            convert.map(|mut convert| {
                move |a: &Node,
                      b: Option<&Node>,
                      c: Option<&[Gc<Node>]>,
                      d: Option<HierarchyFacts>| Ok(convert(a, b, c, d))
            }),
        )
        .unwrap()
    }

    pub(super) fn try_convert_iteration_statement_body_if_necessary(
        &self,
        _node: &Node, /*IterationStatement*/
        _outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
        _ancestor_facts: Option<HierarchyFacts>,
        _convert: Option<
            impl FnMut(
                &Node,         /*IterationStatement*/
                Option<&Node>, /*LabeledStatement*/
                Option<&[Gc<Node /*Statement*/>]>,
                Option<HierarchyFacts>,
            ) -> io::Result<Gc<Node /*Statement*/>>, /*LoopConverter*/
        >,
    ) -> io::Result<VisitResult /*<Statement>*/> {
        unimplemented!()
    }
}
