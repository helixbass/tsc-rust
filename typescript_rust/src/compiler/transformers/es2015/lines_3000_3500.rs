use gc::{Gc, GcCell};

use super::{ConvertedLoopState, CopyDirection, LoopOutParameter, TransformES2015};
use crate::Node;

impl TransformES2015 {
    pub(super) fn create_function_for_initializer_of_for_statement(
        &self,
        _node: &Node, /*ForStatementWithConvertibleInitializer*/
        _current_state: Gc<GcCell<ConvertedLoopState>>,
    ) -> IterationStatementPartFunction<Gc<Node /*VariableDeclarationList*/>> {
        unimplemented!()
    }

    pub(super) fn create_function_for_body_of_iteration_statement(
        &self,
        _node: &Node, /*IterationStatement*/
        _current_state: Gc<GcCell<ConvertedLoopState>>,
        _outer_state: Option<Gc<GcCell<ConvertedLoopState>>>,
    ) -> IterationStatementPartFunction<Vec<Gc<Node /*Statement*/>>> {
        unimplemented!()
    }

    pub(super) fn copy_out_parameter(
        &self,
        _out_param: &LoopOutParameter,
        _copy_direction: CopyDirection,
    ) -> Gc<Node /*BinaryExpression*/> {
        unimplemented!()
    }

    pub(super) fn generate_call_to_converted_loop_initializer(
        &self,
        _init_function_expression_name: &Node, /*Identifier*/
        _contains_yield: bool,
    ) -> Gc<Node /*Statement*/> {
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
