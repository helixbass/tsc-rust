use std::{borrow::Borrow, io};

use gc::Gc;

use super::{ConvertedLoopState, HierarchyFacts, TransformES2015};
use crate::{Node, VisitResult};

impl TransformES2015 {
    pub(super) fn convert_for_of_statement_for_iterable(
        &self,
        _node: &Node, /*ForOfStatement*/
        _outermost_labeled_statement: Option<&Node /*LabeledStatement*/>,
        _converted_loop_body_statements: Option<&[Gc<Node /*Statement*/>]>,
        _ancestor_facts: Option<HierarchyFacts>,
    ) -> Gc<Node /*Statement*/> {
        unimplemented!()
    }

    pub(super) fn visit_object_literal_expression(
        &self,
        _node: &Node, /*ObjectLiteralExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
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
