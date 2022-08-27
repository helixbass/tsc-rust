use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use crate::{
    first_or_undefined, get_emit_flags, get_jsdoc_type_tag, is_assignment_expression,
    is_declaration_binding_element, is_identifier, is_in_js_file, is_object_literal_element_like,
    is_parenthesized_expression, is_prologue_directive, is_spread_element, is_string_literal,
    AssertionLevel, Debug_, EmitFlags, HasInitializerInterface, LiteralLikeNodeInterface,
    NamedDeclarationInterface, Node, NodeInterface, OuterExpressionKinds, SyntaxKind,
};

pub fn is_local_name(node: &Node /*Identifier*/) -> bool {
    get_emit_flags(node).intersects(EmitFlags::LocalName)
}

fn is_use_strict_prologue(node: &Node /*ExpressionStatement*/) -> bool {
    let node_as_expression_statement = node.as_expression_statement();
    is_string_literal(&node_as_expression_statement.expression)
        && &*node_as_expression_statement
            .expression
            .as_string_literal()
            .text()
            == "use strict"
}

pub fn starts_with_use_strict(statements: &[Rc<Node>]) -> bool {
    let first_statement = first_or_undefined(statements);
    if first_statement.is_none() {
        return false;
    }
    let first_statement = first_statement.unwrap();
    is_prologue_directive(first_statement) && is_use_strict_prologue(first_statement)
}

pub fn is_comma_sequence(node: &Node /*Expression*/) -> bool {
    node.kind() == SyntaxKind::BinaryExpression
        && node.as_binary_expression().operator_token.kind() == SyntaxKind::CommaToken
        || node.kind() == SyntaxKind::CommaListExpression
}

pub fn is_jsdoc_type_assertion(node: &Node) -> bool {
    is_parenthesized_expression(node)
        && is_in_js_file(Some(node))
        && get_jsdoc_type_tag(node).is_some()
}

pub fn is_outer_expression(node: &Node, kinds: Option<OuterExpressionKinds>) -> bool {
    let kinds = kinds.unwrap_or(OuterExpressionKinds::All);
    match node.kind() {
        SyntaxKind::ParenthesizedExpression => {
            if kinds.intersects(OuterExpressionKinds::ExcludeJSDocTypeAssertion)
                && is_jsdoc_type_assertion(node)
            {
                return false;
            }
            kinds.intersects(OuterExpressionKinds::Parentheses)
        }
        SyntaxKind::TypeAssertionExpression | SyntaxKind::AsExpression => {
            kinds.intersects(OuterExpressionKinds::TypeAssertions)
        }
        SyntaxKind::NonNullExpression => kinds.intersects(OuterExpressionKinds::NonNullAssertions),
        SyntaxKind::PartiallyEmittedExpression => {
            kinds.intersects(OuterExpressionKinds::PartiallyEmittedExpressions)
        }
        _ => false,
    }
}

pub fn skip_outer_expressions(node: &Node, kinds: Option<OuterExpressionKinds>) -> Rc<Node> {
    let kinds = kinds.unwrap_or(OuterExpressionKinds::All);
    let mut node = node.node_wrapper();
    while is_outer_expression(&node, Some(kinds)) {
        node = node.as_has_expression().expression();
    }
    node
}

pub fn get_target_of_binding_or_assignment_element(
    binding_element: &Node, /*BindingOrAssignmentElement*/
) -> Option<Rc<Node /*BindingOrAssignmentElementTarget*/>> {
    if is_declaration_binding_element(binding_element) {
        return binding_element.as_named_declaration().maybe_name();
    }

    if is_object_literal_element_like(binding_element) {
        match binding_element.kind() {
            SyntaxKind::PropertyAssignment => {
                return get_target_of_binding_or_assignment_element(
                    &binding_element
                        .as_property_assignment()
                        .maybe_initializer()
                        .unwrap(),
                );
            }

            SyntaxKind::ShorthandPropertyAssignment => {
                return binding_element
                    .as_shorthand_property_assignment()
                    .maybe_name();
            }

            SyntaxKind::SpreadAssignment => {
                return get_target_of_binding_or_assignment_element(
                    &binding_element.as_spread_assignment().expression,
                );
            }
            _ => (),
        }

        return None;
    }

    if is_assignment_expression(binding_element, Some(true)) {
        return get_target_of_binding_or_assignment_element(
            &binding_element.as_binary_expression().left,
        );
    }

    if is_spread_element(binding_element) {
        return get_target_of_binding_or_assignment_element(
            &binding_element.as_spread_element().expression,
        );
    }

    Some(binding_element.node_wrapper())
}

pub fn get_elements_of_binding_or_assignment_pattern(
    name: &Node, /*BindingOrAssignmentPattern*/
) -> Vec<Rc<Node /*BindingOrAssignmentElement*/>> {
    match name.kind() {
        SyntaxKind::ObjectBindingPattern
        | SyntaxKind::ArrayBindingPattern
        | SyntaxKind::ArrayLiteralExpression => name.as_has_elements().elements().to_vec(),
        SyntaxKind::ObjectLiteralExpression => {
            name.as_object_literal_expression().properties.to_vec()
        }
        _ => panic!("Unexpected kind"),
    }
}

pub(crate) fn get_jsdoc_type_alias_name<TFullName: Borrow<Node>>(
    full_name: Option<TFullName /*JSDocNamespaceBody*/>,
) -> Option<Rc<Node /*Identifier*/>> {
    full_name.map(|full_name| {
        let full_name = full_name.borrow();
        let mut right_node = full_name.node_wrapper();
        loop {
            if is_identifier(&right_node)
                || right_node.as_jsdoc_namespace_declaration().body.is_none()
            {
                return if is_identifier(&right_node) {
                    right_node
                } else {
                    right_node.as_jsdoc_namespace_declaration().name.clone()
                };
            }
            right_node = right_node
                .as_jsdoc_namespace_declaration()
                .body
                .clone()
                .unwrap();
        }
    })
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum BinaryExpressionState {
    Enter,
    Left,
    Operator,
    Right,
    Exit,
    Done,
}

fn binary_expression_state_call<TMachine: BinaryExpressionStateMachine>(
    state: BinaryExpressionState,
    machine: &TMachine,
    stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    node_stack: &mut Vec<Rc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TMachine::TState>>,
    result_holder: Rc<RefCell<Option<TMachine::TResult>>>,
    outer_state: TMachine::TOuterState,
) -> usize {
    match state {
        BinaryExpressionState::Enter => binary_expression_state_enter(
            machine,
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            result_holder,
            outer_state,
        ),
        BinaryExpressionState::Left => binary_expression_state_left(
            machine,
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            result_holder,
            outer_state,
        ),
        BinaryExpressionState::Operator => binary_expression_state_operator(
            machine,
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            result_holder,
            outer_state,
        ),
        BinaryExpressionState::Right => binary_expression_state_right(
            machine,
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            result_holder,
            outer_state,
        ),
        BinaryExpressionState::Exit => binary_expression_state_exit(
            machine,
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            result_holder,
            outer_state,
        ),
        BinaryExpressionState::Done => binary_expression_state_done(
            machine,
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            result_holder,
            outer_state,
        ),
    }
}

fn binary_expression_state_enter<TMachine: BinaryExpressionStateMachine>(
    machine: &TMachine,
    stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    node_stack: &mut Vec<Rc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TMachine::TState>>,
    result_holder: Rc<RefCell<Option<TMachine::TResult>>>,
    outer_state: TMachine::TOuterState,
) -> usize {
    let prev_user_state = if stack_index > 0 {
        Some(user_state_stack[stack_index - 1].clone().unwrap())
    } else {
        None
    };
    Debug_.assert_equal(
        &state_stack[stack_index],
        &BinaryExpressionState::Enter,
        None,
        None,
    );
    push_or_replace(
        user_state_stack,
        stack_index,
        Some(machine.on_enter(&node_stack[stack_index], prev_user_state, outer_state)),
    );
    push_or_replace(
        state_stack,
        stack_index,
        binary_expression_state_next_state(machine, BinaryExpressionState::Enter),
    );
    stack_index
}

fn binary_expression_state_left<TMachine: BinaryExpressionStateMachine>(
    machine: &TMachine,
    stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    node_stack: &mut Vec<Rc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TMachine::TState>>,
    result_holder: Rc<RefCell<Option<TMachine::TResult>>>,
    outer_state: TMachine::TOuterState,
) -> usize {
    Debug_.assert_equal(
        &state_stack[stack_index],
        &BinaryExpressionState::Left,
        None,
        None,
    );
    Debug_.assert(machine.implements_on_left(), None);
    push_or_replace(
        state_stack,
        stack_index,
        binary_expression_state_next_state(machine, BinaryExpressionState::Left),
    );
    let next_node = machine.on_left(
        &node_stack[stack_index].as_binary_expression().left,
        user_state_stack[stack_index].clone().unwrap(),
        &node_stack[stack_index],
    );
    if let Some(next_node) = next_node.as_ref() {
        binary_expression_state_check_circularity(stack_index, node_stack, next_node);
        return binary_expression_state_push_stack(
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            next_node,
        );
    }
    stack_index
}

fn binary_expression_state_operator<TMachine: BinaryExpressionStateMachine>(
    machine: &TMachine,
    stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    node_stack: &mut Vec<Rc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TMachine::TState>>,
    result_holder: Rc<RefCell<Option<TMachine::TResult>>>,
    outer_state: TMachine::TOuterState,
) -> usize {
    Debug_.assert_equal(
        &state_stack[stack_index],
        &BinaryExpressionState::Operator,
        None,
        None,
    );
    Debug_.assert(machine.implements_on_operator(), None);
    push_or_replace(
        state_stack,
        stack_index,
        binary_expression_state_next_state(machine, BinaryExpressionState::Operator),
    );
    machine.on_operator(
        &node_stack[stack_index]
            .as_binary_expression()
            .operator_token,
        user_state_stack[stack_index].clone().unwrap(),
        &node_stack[stack_index],
    );
    stack_index
}

fn binary_expression_state_right<TMachine: BinaryExpressionStateMachine>(
    machine: &TMachine,
    stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    node_stack: &mut Vec<Rc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TMachine::TState>>,
    result_holder: Rc<RefCell<Option<TMachine::TResult>>>,
    outer_state: TMachine::TOuterState,
) -> usize {
    Debug_.assert_equal(
        &state_stack[stack_index],
        &BinaryExpressionState::Right,
        None,
        None,
    );
    Debug_.assert(machine.implements_on_right(), None);
    push_or_replace(
        state_stack,
        stack_index,
        binary_expression_state_next_state(machine, BinaryExpressionState::Right),
    );
    let next_node = machine.on_right(
        &node_stack[stack_index].as_binary_expression().right,
        user_state_stack[stack_index].clone().unwrap(),
        &node_stack[stack_index],
    );
    if let Some(next_node) = next_node.as_ref() {
        binary_expression_state_check_circularity(stack_index, node_stack, next_node);
        return binary_expression_state_push_stack(
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            next_node,
        );
    }
    stack_index
}

fn binary_expression_state_exit<TMachine: BinaryExpressionStateMachine>(
    machine: &TMachine,
    mut stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    node_stack: &mut Vec<Rc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TMachine::TState>>,
    result_holder: Rc<RefCell<Option<TMachine::TResult>>>,
    _outer_state: TMachine::TOuterState,
) -> usize {
    Debug_.assert_equal(
        &state_stack[stack_index],
        &BinaryExpressionState::Exit,
        None,
        None,
    );
    push_or_replace(
        state_stack,
        stack_index,
        binary_expression_state_next_state(machine, BinaryExpressionState::Exit),
    );
    let result = machine.on_exit(
        &node_stack[stack_index],
        user_state_stack[stack_index].clone().unwrap(),
    );
    if stack_index > 0 {
        stack_index -= 1;
        if machine.implements_fold_state() {
            let side = if state_stack[stack_index] == BinaryExpressionState::Exit {
                LeftOrRight::Right
            } else {
                LeftOrRight::Left
            };
            push_or_replace(
                user_state_stack,
                stack_index,
                Some(machine.fold_state(
                    user_state_stack[stack_index].clone().unwrap(),
                    result,
                    side,
                )),
            );
        }
    } else {
        *result_holder.borrow_mut() = Some(result);
    }
    stack_index
}

fn binary_expression_state_done<TMachine: BinaryExpressionStateMachine>(
    machine: &TMachine,
    stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    node_stack: &mut Vec<Rc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TMachine::TState>>,
    result_holder: Rc<RefCell<Option<TMachine::TResult>>>,
    outer_state: TMachine::TOuterState,
) -> usize {
    Debug_.assert_equal(
        &state_stack[stack_index],
        &BinaryExpressionState::Done,
        None,
        None,
    );
    stack_index
}

fn binary_expression_state_next_state<TMachine: BinaryExpressionStateMachine>(
    machine: &TMachine,
    current_state: BinaryExpressionState,
) -> BinaryExpressionState {
    match current_state {
        BinaryExpressionState::Enter => {
            if machine.implements_on_left() {
                BinaryExpressionState::Left
            } else if machine.implements_on_operator() {
                BinaryExpressionState::Operator
            } else if machine.implements_on_right() {
                BinaryExpressionState::Right
            } else {
                BinaryExpressionState::Exit
            }
        }
        BinaryExpressionState::Left => {
            if machine.implements_on_operator() {
                BinaryExpressionState::Operator
            } else if machine.implements_on_right() {
                BinaryExpressionState::Right
            } else {
                BinaryExpressionState::Exit
            }
        }
        BinaryExpressionState::Operator => {
            if machine.implements_on_right() {
                BinaryExpressionState::Right
            } else {
                BinaryExpressionState::Exit
            }
        }
        BinaryExpressionState::Right => BinaryExpressionState::Exit,
        BinaryExpressionState::Exit => BinaryExpressionState::Done,
        BinaryExpressionState::Done => BinaryExpressionState::Done,
    }
}

fn binary_expression_state_push_stack<TState>(
    mut stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    node_stack: &mut Vec<Rc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TState>>,
    node: &Node, /*BinaryExpression*/
) -> usize {
    stack_index += 1;
    push_or_replace(state_stack, stack_index, BinaryExpressionState::Enter);
    push_or_replace(node_stack, stack_index, node.node_wrapper());
    push_or_replace(user_state_stack, stack_index, None);
    stack_index
}

fn push_or_replace<TValue>(vec: &mut Vec<TValue>, index: usize, value: TValue) {
    if index >= vec.len() {
        vec.push(value);
    } else {
        vec[index] = value;
    }
}

fn binary_expression_state_check_circularity(
    mut stack_index: usize,
    node_stack: &[Rc<Node /*BinaryExpression*/>],
    node: &Node, /*BinaryExpression*/
) {
    if Debug_.should_assert(AssertionLevel::Aggressive) {
        while stack_index >= 0 {
            Debug_.assert(
                !ptr::eq(&*node_stack[stack_index], node),
                Some("Circular traversal detected."),
            );
            stack_index -= 1;
        }
    }
}

pub trait BinaryExpressionStateMachine {
    type TResult: Clone;
    type TOuterState: Clone;
    type TState: Clone;

    fn on_enter(
        &self,
        node: &Node, /*BinaryExpression*/
        prev: Option<Self::TState>,
        outer_state: Self::TOuterState,
    ) -> Self::TState;

    fn on_left(
        &self,
        left: &Node, /*Expression*/
        user_state: Self::TState,
        node: &Node, /*BinaryExpression*/
    ) -> Option<Rc<Node /*BinaryExpression*/>> {
        panic!("Shouldn't call default on_left()")
    }

    fn on_operator(
        &self,
        operator_token: &Node, /*BinaryOperatorToken*/
        user_state: Self::TState,
        node: &Node, /*BinaryExpression*/
    ) {
        panic!("Shouldn't call default on_operator()")
    }

    fn on_right(
        &self,
        right: &Node, /*Expression*/
        user_state: Self::TState,
        node: &Node, /*BinaryExpression*/
    ) -> Option<Rc<Node /*BinaryExpression*/>> {
        panic!("Shouldn't call default on_left()")
    }

    fn on_exit(
        &self,
        node: &Node, /*BinaryExpression*/
        user_state: Self::TState,
    ) -> Self::TResult;

    fn fold_state(
        &self,
        user_state: Self::TState,
        result: Self::TResult,
        side: LeftOrRight,
    ) -> Self::TState {
        panic!("Shouldn't call default fold_state()")
    }

    fn implements_on_left(&self) -> bool;
    fn implements_on_operator(&self) -> bool;
    fn implements_on_right(&self) -> bool;
    fn implements_fold_state(&self) -> bool;
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LeftOrRight {
    Left,
    Right,
}

pub fn create_binary_expression_trampoline<TMachine: BinaryExpressionStateMachine>(
    machine: TMachine,
) -> BinaryExpressionTrampoline<TMachine> {
    BinaryExpressionTrampoline::new(machine)
}

pub struct BinaryExpressionTrampoline<TMachine: BinaryExpressionStateMachine> {
    machine: TMachine,
}

impl<TMachine: BinaryExpressionStateMachine> BinaryExpressionTrampoline<TMachine> {
    pub fn new(machine: TMachine) -> Self {
        Self { machine }
    }

    pub fn call(
        &self,
        node: &Node, /*BinaryExpression*/
        outer_state: TMachine::TOuterState,
    ) -> TMachine::TResult {
        let result_holder: Rc<RefCell<Option<TMachine::TResult>>> = Rc::new(RefCell::new(None));
        let mut state_stack: Vec<BinaryExpressionState> = vec![BinaryExpressionState::Enter];
        let mut node_stack: Vec<Rc<Node /*BinaryExpression*/>> = vec![node.node_wrapper()];
        let mut user_state_stack: Vec<Option<TMachine::TState>> = vec![None];
        let mut stack_index = 0;
        while state_stack[stack_index] != BinaryExpressionState::Done {
            stack_index = binary_expression_state_call(
                state_stack[stack_index],
                &self.machine,
                stack_index,
                &mut state_stack,
                &mut node_stack,
                &mut user_state_stack,
                result_holder.clone(),
                outer_state.clone(),
            );
        }
        Debug_.assert_equal(&stack_index, &0, None, None);
        let ret = (*result_holder).borrow().clone().unwrap();
        ret
    }
}
