use std::io;

use gc::{Gc, GcCell};

use super::{
    ConvertedLoopState, CopyDirection, HierarchyFacts, LoopOutParameter, LoopOutParameterFlags,
    TransformES2015,
};
use crate::{
    EmitFlags, Node, NodeArray, NodeExt, NodeInterface, SyntaxKind, TransformFlags, _d, add_range,
    insert_statements_after_standard_prologue, is_block, is_expression, is_statement, map,
    set_original_node, try_visit_node,
};

impl TransformES2015 {
    pub(super) fn create_out_variable(&self, p: &LoopOutParameter) -> Gc<Node> {
        self.factory
            .create_variable_declaration(
                Some(p.original_name.clone()),
                None,
                None,
                Some(p.out_param_name.clone()),
            )
            .wrap()
    }

    pub(super) fn create_function_for_initializer_of_for_statement(
        &self,
        node: &Node, /*ForStatementWithConvertibleInitializer*/
        current_state: Gc<GcCell<ConvertedLoopState>>,
    ) -> io::Result<IterationStatementPartFunction<Gc<Node /*VariableDeclarationList*/>>> {
        let node_as_for_statement = node.as_for_statement();
        let function_name = self.factory.create_unique_name("_loop_init", None);

        let node_initializer = node_as_for_statement.initializer.as_ref().unwrap();
        let contains_yield = node_initializer
            .transform_flags()
            .intersects(TransformFlags::ContainsYield);
        let mut emit_flags = EmitFlags::None;
        let current_state = (*current_state).borrow();
        if current_state.contains_lexical_this == Some(true) {
            emit_flags |= EmitFlags::CapturesThis;
        }
        if contains_yield
            && self
                .maybe_hierarchy_facts()
                .unwrap_or_default()
                .intersects(HierarchyFacts::AsyncFunctionBody)
        {
            emit_flags |= EmitFlags::AsyncFunctionBody;
        }

        let mut statements: Vec<Gc<Node /*Statement*/>> = _d();
        statements.push(
            self.factory
                .create_variable_statement(Option::<Gc<NodeArray>>::None, node_initializer.clone())
                .wrap(),
        );
        self.copy_out_parameters(
            &current_state.loop_out_parameters,
            LoopOutParameterFlags::Initializer,
            CopyDirection::ToOutParameter,
            &mut statements,
        );

        let function_declaration = self
            .factory
            .create_variable_statement(
                Option::<Gc<NodeArray>>::None,
                self.factory
                    .create_variable_declaration_list(
                        vec![self
                            .factory
                            .create_variable_declaration(
                                Some(function_name.clone()),
                                None,
                                None,
                                Some(
                                    self.factory
                                        .create_function_expression(
                                            Option::<Gc<NodeArray>>::None,
                                            contains_yield.then(|| {
                                                self.factory
                                                    .create_token(SyntaxKind::AsteriskToken)
                                                    .wrap()
                                            }),
                                            Option::<Gc<Node>>::None,
                                            Option::<Gc<NodeArray>>::None,
                                            Option::<Gc<NodeArray>>::None,
                                            None,
                                            try_visit_node(
                                                Some(
                                                    self.factory
                                                        .create_block(statements, Some(true))
                                                        .wrap(),
                                                ),
                                                Some(|node: &Node| self.visitor(node)),
                                                Some(is_block),
                                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                            )?
                                            .unwrap(),
                                        )
                                        .wrap()
                                        .set_emit_flags(emit_flags),
                                ),
                            )
                            .wrap()],
                        None,
                    )
                    .wrap()
                    .set_emit_flags(EmitFlags::NoHoisting),
            )
            .wrap();

        let part = self
            .factory
            .create_variable_declaration_list(
                map(
                    &current_state.loop_out_parameters,
                    |loop_out_parameter: &LoopOutParameter, _| {
                        self.create_out_variable(loop_out_parameter)
                    },
                ),
                None,
            )
            .wrap();

        Ok(IterationStatementPartFunction {
            function_name,
            contains_yield,
            function_declaration,
            part,
        })
    }

    pub(super) fn create_function_for_body_of_iteration_statement(
        &self,
        node: &Node, /*IterationStatement*/
        current_state: Gc<GcCell<ConvertedLoopState>>,
        outer_state: Option<Gc<GcCell<ConvertedLoopState>>>,
    ) -> io::Result<IterationStatementPartFunction<Vec<Gc<Node /*Statement*/>>>> {
        let function_name = self.factory.create_unique_name("_loop", None);
        self.context.start_lexical_environment();
        let statement = try_visit_node(
            Some(node.as_has_statement().statement()),
            Some(|node: &Node| self.visitor(node)),
            Some(is_statement),
            Some(&|nodes: &[Gc<Node>]| self.factory.lift_to_block(nodes)),
        )?
        .unwrap();
        let lexical_environment = self.context.end_lexical_environment();

        let mut statements: Vec<Gc<Node /*Statement*/>> = _d();
        if self.should_convert_condition_of_for_statement(node)
            || self.should_convert_incrementor_of_for_statement(node)
        {
            current_state.borrow_mut().condition_variable =
                Some(self.factory.create_unique_name("inc", None));
            let node_as_for_statement = node.as_for_statement();
            if let Some(node_incrementor) = node_as_for_statement.incrementor.as_ref() {
                statements.push(
                    self.factory
                        .create_if_statement(
                            (*current_state)
                                .borrow()
                                .condition_variable
                                .clone()
                                .unwrap(),
                            self.factory
                                .create_expression_statement(
                                    try_visit_node(
                                        Some(&**node_incrementor),
                                        Some(|node: &Node| self.visitor(node)),
                                        Some(is_expression),
                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                    )?
                                    .unwrap(),
                                )
                                .wrap(),
                            Some(
                                self.factory
                                    .create_expression_statement(
                                        self.factory
                                            .create_assignment(
                                                (*current_state)
                                                    .borrow()
                                                    .condition_variable
                                                    .clone()
                                                    .unwrap(),
                                                self.factory.create_true().wrap(),
                                            )
                                            .wrap(),
                                    )
                                    .wrap(),
                            ),
                        )
                        .wrap(),
                );
            } else {
                statements.push(
                    self.factory
                        .create_if_statement(
                            self.factory
                                .create_logical_not(
                                    (*current_state)
                                        .borrow()
                                        .condition_variable
                                        .clone()
                                        .unwrap(),
                                )
                                .wrap(),
                            self.factory
                                .create_expression_statement(
                                    self.factory
                                        .create_assignment(
                                            (*current_state)
                                                .borrow()
                                                .condition_variable
                                                .clone()
                                                .unwrap(),
                                            self.factory.create_true().wrap(),
                                        )
                                        .wrap(),
                                )
                                .wrap(),
                            None,
                        )
                        .wrap(),
                );
            }

            if self.should_convert_condition_of_for_statement(node) {
                statements.push(
                    self.factory
                        .create_if_statement(
                            self.factory
                                .create_prefix_unary_expression(
                                    SyntaxKind::ExclamationToken,
                                    try_visit_node(
                                        node_as_for_statement.condition.as_deref(),
                                        Some(|node: &Node| self.visitor(node)),
                                        Some(is_expression),
                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                    )?
                                    .unwrap(),
                                )
                                .wrap(),
                            try_visit_node(
                                Some(
                                    self.factory
                                        .create_break_statement(Option::<Gc<Node>>::None)
                                        .wrap(),
                                ),
                                Some(|node: &Node| self.visitor(node)),
                                Some(is_statement),
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            )?
                            .unwrap(),
                            None,
                        )
                        .wrap(),
                );
            }
        }

        if is_block(&statement) {
            add_range(
                &mut statements,
                Some(&statement.as_block().statements),
                None,
                None,
            );
        } else {
            statements.push(statement.clone());
        }

        self.copy_out_parameters(
            &(*current_state).borrow().loop_out_parameters,
            LoopOutParameterFlags::Body,
            CopyDirection::ToOutParameter,
            &mut statements,
        );
        insert_statements_after_standard_prologue(&mut statements, lexical_environment.as_deref());

        let loop_body = self.factory.create_block(statements, Some(true)).wrap();
        if is_block(&statement) {
            set_original_node(&*loop_body, Some(statement.clone()));
        }

        let contains_yield = node
            .as_has_statement()
            .statement()
            .transform_flags()
            .intersects(TransformFlags::ContainsYield);

        let mut emit_flags = EmitFlags::ReuseTempVariableScope;
        if (*current_state).borrow().contains_lexical_this == Some(true) {
            emit_flags |= EmitFlags::CapturesThis;
        }
        if contains_yield
            && self
                .maybe_hierarchy_facts()
                .unwrap_or_default()
                .intersects(HierarchyFacts::AsyncFunctionBody)
        {
            emit_flags |= EmitFlags::AsyncFunctionBody;
        }

        let function_declaration = self
            .factory
            .create_variable_statement(
                Option::<Gc<NodeArray>>::None,
                self.factory
                    .create_variable_declaration_list(
                        vec![self
                            .factory
                            .create_variable_declaration(
                                Some(function_name.clone()),
                                None,
                                None,
                                Some(
                                    self.factory
                                        .create_function_expression(
                                            Option::<Gc<NodeArray>>::None,
                                            contains_yield.then(|| {
                                                self.factory
                                                    .create_token(SyntaxKind::AsteriskToken)
                                                    .wrap()
                                            }),
                                            Option::<Gc<Node>>::None,
                                            Option::<Gc<NodeArray>>::None,
                                            Some((*current_state).borrow().loop_parameters.clone()),
                                            None,
                                            loop_body,
                                        )
                                        .wrap()
                                        .set_emit_flags(emit_flags),
                                ),
                            )
                            .wrap()],
                        None,
                    )
                    .wrap()
                    .set_emit_flags(EmitFlags::NoHoisting),
            )
            .wrap();

        let part = self.generate_call_to_converted_loop(
            &function_name,
            current_state,
            outer_state,
            contains_yield,
        );

        Ok(IterationStatementPartFunction {
            function_name,
            contains_yield,
            function_declaration,
            part,
        })
    }

    pub(super) fn copy_out_parameter(
        &self,
        _out_param: &LoopOutParameter,
        _copy_direction: CopyDirection,
    ) -> Gc<Node /*BinaryExpression*/> {
        unimplemented!()
    }

    pub(super) fn copy_out_parameters(
        &self,
        _out_params: &[LoopOutParameter],
        _part_flags: LoopOutParameterFlags,
        _copy_direction: CopyDirection,
        _statements: &mut Vec<Gc<Node /*Statement*/>>,
    ) {
        unimplemented!()
    }

    pub(super) fn generate_call_to_converted_loop_initializer(
        &self,
        _init_function_expression_name: &Node, /*Identifier*/
        _contains_yield: bool,
    ) -> Gc<Node /*Statement*/> {
        unimplemented!()
    }

    pub(super) fn generate_call_to_converted_loop(
        &self,
        _loop_function_expression_name: &Node, /*Identifier*/
        _state: Gc<GcCell<ConvertedLoopState>>,
        _outer_state: Option<Gc<GcCell<ConvertedLoopState>>>,
        _contains_yield: bool,
    ) -> Vec<Gc<Node /*Statement*/>> {
        unimplemented!()
    }

    pub(super) fn set_labeled_jump(
        &self,
        _state: &mut ConvertedLoopState,
        _is_break: bool,
        _label_text: &str,
        _label_marker: &str,
    ) {
        unimplemented!()
    }

    pub(super) fn process_loop_variable_declaration(
        &self,
        _container: &Node, /*IterationStatement*/
        _decl: &Node,      /*VariableDeclaration | BindingElement*/
        _loop_parameters: &mut Vec<Gc<Node /*ParameterDeclaration*/>>,
        _loop_out_parameters: &mut Vec<LoopOutParameter>,
        _has_captured_bindings_in_for_initializer: bool,
    ) {
        unimplemented!()
    }

    pub(super) fn add_object_literal_members(
        &self,
        _expressions: &mut Vec<Gc<Node /*Expression*/>>,
        _node: &Node,     /*ObjectLiteralExpression*/
        _receiver: &Node, /*Identifier*/
        _start: usize,
    ) {
        unimplemented!()
    }
}

pub(super) struct IterationStatementPartFunction<TPart> {
    pub function_name: Gc<Node /*Identifier*/>,
    pub function_declaration: Gc<Node /*Statement*/>,
    pub contains_yield: bool,
    pub part: TPart,
}
