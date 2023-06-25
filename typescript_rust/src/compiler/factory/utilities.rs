use std::{borrow::Borrow, cell::RefCell, io, ptr, rc::Rc};

use gc::{Finalize, Gc, Trace};

use crate::{
    first_or_undefined, get_emit_flags, get_jsdoc_type, get_jsdoc_type_tag,
    is_assignment_expression, is_declaration_binding_element, is_exclamation_token, is_identifier,
    is_in_js_file, is_minus_token, is_object_literal_element_like, is_parenthesized_expression,
    is_plus_token, is_prologue_directive, is_question_token, is_readonly_keyword, is_source_file,
    is_spread_element, is_string_literal, is_this_type_node, is_type_node,
    is_type_parameter_declaration, maybe_get_original_node_full, push_or_replace,
    set_starts_on_new_line, AssertionLevel, BaseNodeFactory, CompilerOptions, Debug_, EmitFlags,
    EmitHelperFactory, EmitHost, EmitResolver, HasInitializerInterface, LiteralLikeNodeInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeFactory, NodeInterface, OuterExpressionKinds,
    ReadonlyTextRange, SyntaxKind,
};

pub fn create_empty_exports<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>(
    factory: &NodeFactory<TBaseNodeFactory>,
) -> Gc<Node> {
    factory.create_export_declaration(
        Option::<Gc<NodeArray>>::None,
        Option::<Gc<NodeArray>>::None,
        false,
        Some(factory.create_named_exports(vec![])),
        None,
        None,
    )
}

pub fn create_member_access_for_property_name<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _target: &Node,      /*Expression*/
    _member_name: &Node, /*PropertyName*/
    _location: Option<&impl ReadonlyTextRange>,
) -> Gc<Node /*MemberExpression*/> {
    unimplemented!()
}

pub fn create_jsx_factory_expression<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _jsx_factory_entity: Option<impl Borrow<Node /*EntityName*/>>,
    _react_namespace: &str,
    _parent: &Node, /*JsxOpeningLikeElement | JsxOpeningFragment*/
) -> Gc<Node /*Statement*/> {
    unimplemented!()
}

pub fn create_expression_for_jsx_element<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _callee: &Node,   /*Expression*/
    _tag_name: &Node, /*Expression*/
    _props: Option<impl Borrow<Node /*Expression*/>>,
    _children: Option<&[Gc<Node /*Expression*/>]>,
    _location: &impl ReadonlyTextRange,
) -> Gc<Node /*LeftHandSideExpression*/> {
    unimplemented!()
}

pub fn create_expression_for_jsx_fragment<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _jsx_factory_entity: Option<impl Borrow<Node /*EntityName*/>>,
    _jsx_fragment_factory_entity: Option<impl Borrow<Node /*EntityName*/>>,
    _react_namespace: &str,
    _children: &[Gc<Node /*Expression*/>],
    _parent_element: &Node, /*JsxOpeningFragment*/
    _location: &impl ReadonlyTextRange,
) -> Gc<Node /*LeftHandSideExpression*/> {
    unimplemented!()
}

pub fn create_for_of_binding_statement<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _node: &Node,        /*ForInitializer*/
    _bound_value: &Node, /*Expression*/
) -> Gc<Node /*Statement*/> {
    unimplemented!()
}

pub fn create_expression_from_entity_name<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _node: &Node, /*EntityName | Expression*/
) -> Gc<Node /*Expression*/> {
    unimplemented!()
}

pub fn create_expression_for_property_name<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _member_name: &Node, /*Exclude<PropertyName, PrivateIdentifier>*/
) -> Gc<Node /*Expression*/> {
    unimplemented!()
}

pub fn create_expression_for_object_literal_element_like<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _node: &Node,     /*ObjectLiteralExpression*/
    _property: &Node, /*ObjectLiteralElementLike*/
    _receiver: &Node, /*Expression*/
) -> Option<Gc<Node /*Expression*/>> {
    unimplemented!()
}

pub fn expand_pre_or_postfix_increment_or_decrement_expression<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _node: &Node,       /*PrefixUnaryExpression | PostfixUnaryExpression*/
    _expression: &Node, /*Expression*/
    _record_temp_variable: impl FnMut(&Node /*Expression*/),
    _result_variable: Option<impl Borrow<Node /*Identifier*/>>,
) -> Gc<Node /*Expression*/> {
    unimplemented!()
}

pub fn is_internal_name(node: &Node /*Identifier*/) -> bool {
    get_emit_flags(node).intersects(EmitFlags::InternalName)
}

pub fn is_local_name(node: &Node /*Identifier*/) -> bool {
    get_emit_flags(node).intersects(EmitFlags::LocalName)
}

pub fn is_export_name(node: &Node /*Identifier*/) -> bool {
    get_emit_flags(node).intersects(EmitFlags::ExportName)
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

pub fn find_use_strict_prologue(
    statements: &[Gc<Node /*Statement*/>],
) -> Option<Gc<Node /*Statement*/>> {
    for statement in statements {
        if is_prologue_directive(statement) {
            if is_use_strict_prologue(statement) {
                return Some(statement.clone());
            }
        } else {
            break;
        }
    }
    None
}

pub fn starts_with_use_strict(statements: &[Gc<Node>]) -> bool {
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

pub fn get_jsdoc_type_assertion_type(node: &Node /*JSDocTypeAssertion*/) -> Gc<Node> {
    let type_ = get_jsdoc_type(node);
    Debug_.assert_is_defined(&type_, None);
    type_.unwrap()
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

pub fn skip_outer_expressions(node: &Node, kinds: Option<OuterExpressionKinds>) -> Gc<Node> {
    let kinds = kinds.unwrap_or(OuterExpressionKinds::All);
    let mut node = node.node_wrapper();
    while is_outer_expression(&node, Some(kinds)) {
        node = node.as_has_expression().expression();
    }
    node
}

pub fn start_on_new_line<TNode: Borrow<Node>>(node: TNode) -> TNode {
    set_starts_on_new_line(node.borrow(), true);
    node
}

pub fn get_external_helpers_module_name(node: &Node /*SourceFile*/) -> Option<Gc<Node>> {
    let parse_node = maybe_get_original_node_full(
        Some(node),
        Some(|node: Option<Gc<Node>>| is_source_file(node.as_ref().unwrap())),
    )?;
    let emit_node = parse_node.maybe_emit_node()?;
    let ret = (*emit_node).borrow().external_helpers_module_name.clone();
    ret
}

pub fn has_recorded_external_helpers(source_file: &Node /*SourceFile*/) -> bool {
    let parse_node = maybe_get_original_node_full(
        Some(source_file),
        Some(|node: Option<Gc<Node>>| is_source_file(node.as_ref().unwrap())),
    );
    let emit_node = parse_node.and_then(|parse_node| parse_node.maybe_emit_node());
    matches!(
        emit_node,
        Some(emit_node) if {
            let emit_node = (*emit_node).borrow();
            emit_node.external_helpers_module_name.is_some() ||
                emit_node.external_helpers == Some(true)
        }
    )
}

pub fn create_external_helpers_import_declaration_if_needed<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _node_factory: &NodeFactory<TBaseNodeFactory>,
    _helper_factory: &EmitHelperFactory,
    _source_file: &Node, /*SourceFile*/
    _compiler_options: &CompilerOptions,
    _has_export_stars_to_export_values: Option<bool>,
    _has_import_star: Option<bool>,
    _has_import_default: Option<bool>,
) -> Option<Gc<Node>> {
    unimplemented!()
}

pub fn get_local_name_for_external_import<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _node: &Node, /*ImportDeclaration | ExportDeclaration | ImportEqualsDeclaration*/
    _source_file: &Node, /*SourceFile*/
) -> Option<Gc<Node /*Identifier*/>> {
    unimplemented!()
}

pub fn get_external_module_name_literal<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _import_node: &Node, /*ImportDeclaration | ExportDeclaration | ImportEqualsDeclaration | ImportCall*/
    _source_file: &Node, /*SourceFile*/
    _host: &dyn EmitHost,
    _resolver: &dyn EmitResolver,
    _compiler_options: &CompilerOptions,
) -> Option<Gc<Node>> {
    unimplemented!()
}

pub fn try_get_module_name_from_file<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _file: Option<impl Borrow<Node /*SourceFile*/>>,
    _host: &dyn EmitHost,
    _options: &CompilerOptions,
) -> Option<Gc<Node /*StringLiteral*/>> {
    unimplemented!()
}

pub fn get_initializer_of_binding_or_assignment_element(
    _binding_element: &Node, /*BindingOrAssignmentElement*/
) -> Option<Gc<Node /*Expression*/>> {
    unimplemented!()
}

pub fn get_target_of_binding_or_assignment_element(
    binding_element: &Node, /*BindingOrAssignmentElement*/
) -> Option<Gc<Node /*BindingOrAssignmentElementTarget*/>> {
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
) -> impl Iterator<Item = Gc<Node /*BindingOrAssignmentElement*/>> {
    match name.kind() {
        SyntaxKind::ObjectBindingPattern
        | SyntaxKind::ArrayBindingPattern
        | SyntaxKind::ArrayLiteralExpression => name.as_has_elements().elements().owned_iter(),
        SyntaxKind::ObjectLiteralExpression => {
            name.as_object_literal_expression().properties.owned_iter()
        }
        _ => panic!("Unexpected kind"),
    }
}

pub(crate) fn get_jsdoc_type_alias_name(
    full_name: Option<impl Borrow<Node> /*JSDocNamespaceBody*/>,
) -> Option<Gc<Node /*Identifier*/>> {
    full_name.map(|full_name| {
        let full_name = full_name.borrow();
        let mut right_node = full_name.node_wrapper();
        loop {
            if is_identifier(&right_node) || right_node.as_module_declaration().body.is_none() {
                return if is_identifier(&right_node) {
                    right_node
                } else {
                    right_node.as_module_declaration().name.clone()
                };
            }
            right_node = right_node.as_module_declaration().body.clone().unwrap();
        }
    })
}

pub fn can_have_modifiers(node: &Node) -> bool {
    let kind = node.kind();
    matches!(
        kind,
        SyntaxKind::Parameter
            | SyntaxKind::PropertySignature
            | SyntaxKind::PropertyDeclaration
            | SyntaxKind::MethodSignature
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::Constructor
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::IndexSignature
            | SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction
            | SyntaxKind::ClassExpression
            | SyntaxKind::VariableStatement
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::InterfaceDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::EnumDeclaration
            | SyntaxKind::ModuleDeclaration
            | SyntaxKind::ImportEqualsDeclaration
            | SyntaxKind::ImportDeclaration
            | SyntaxKind::ExportAssignment
            | SyntaxKind::ExportDeclaration
    )
}

pub fn is_type_node_or_type_parameter_declaration(node: &Node) -> bool {
    is_type_node(node) || is_type_parameter_declaration(node)
}

pub fn is_question_or_exclamation_token(node: &Node) -> bool {
    is_question_token(node) || is_exclamation_token(node)
}

pub fn is_identifier_or_this_type_node(node: &Node) -> bool {
    is_identifier(node) || is_this_type_node(node)
}

pub fn is_readonly_keyword_or_plus_or_minus_token(node: &Node) -> bool {
    is_readonly_keyword(node) || is_plus_token(node) || is_minus_token(node)
}

pub fn is_question_or_plus_or_minus_token(node: &Node) -> bool {
    is_question_token(node) || is_plus_token(node) || is_minus_token(node)
}

pub fn is_module_name(node: &Node) -> bool {
    is_identifier(node) || is_string_literal(node)
}

pub fn is_binary_operator_token(_node: &Node) -> bool {
    unimplemented!()
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
    node_stack: &mut Vec<Gc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TMachine::TState>>,
    result_holder: Rc<RefCell<Option<TMachine::TResult>>>,
    outer_state: TMachine::TOuterState,
) -> io::Result<usize> {
    Ok(match state {
        BinaryExpressionState::Enter => binary_expression_state_enter(
            machine,
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            result_holder,
            outer_state,
        )?,
        BinaryExpressionState::Left => binary_expression_state_left(
            machine,
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            result_holder,
            outer_state,
        )?,
        BinaryExpressionState::Operator => binary_expression_state_operator(
            machine,
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            result_holder,
            outer_state,
        )?,
        BinaryExpressionState::Right => binary_expression_state_right(
            machine,
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            result_holder,
            outer_state,
        )?,
        BinaryExpressionState::Exit => binary_expression_state_exit(
            machine,
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            result_holder,
            outer_state,
        )?,
        BinaryExpressionState::Done => binary_expression_state_done(
            machine,
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            result_holder,
            outer_state,
        ),
    })
}

fn binary_expression_state_enter<TMachine: BinaryExpressionStateMachine>(
    machine: &TMachine,
    stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    node_stack: &mut Vec<Gc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TMachine::TState>>,
    _result_holder: Rc<RefCell<Option<TMachine::TResult>>>,
    outer_state: TMachine::TOuterState,
) -> io::Result<usize> {
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
        Some(machine.on_enter(&node_stack[stack_index], prev_user_state, outer_state)?),
    );
    push_or_replace(
        state_stack,
        stack_index,
        binary_expression_state_next_state(machine, BinaryExpressionState::Enter),
    );
    Ok(stack_index)
}

fn binary_expression_state_left<TMachine: BinaryExpressionStateMachine>(
    machine: &TMachine,
    stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    node_stack: &mut Vec<Gc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TMachine::TState>>,
    _result_holder: Rc<RefCell<Option<TMachine::TResult>>>,
    _outer_state: TMachine::TOuterState,
) -> io::Result<usize> {
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
    )?;
    if let Some(next_node) = next_node.as_ref() {
        binary_expression_state_check_circularity(stack_index, node_stack, next_node);
        return Ok(binary_expression_state_push_stack(
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            next_node,
        ));
    }
    Ok(stack_index)
}

fn binary_expression_state_operator<TMachine: BinaryExpressionStateMachine>(
    machine: &TMachine,
    stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    node_stack: &mut Vec<Gc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TMachine::TState>>,
    _result_holder: Rc<RefCell<Option<TMachine::TResult>>>,
    _outer_state: TMachine::TOuterState,
) -> io::Result<usize> {
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
    )?;
    Ok(stack_index)
}

fn binary_expression_state_right<TMachine: BinaryExpressionStateMachine>(
    machine: &TMachine,
    stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    node_stack: &mut Vec<Gc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TMachine::TState>>,
    _result_holder: Rc<RefCell<Option<TMachine::TResult>>>,
    _outer_state: TMachine::TOuterState,
) -> io::Result<usize> {
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
    )?;
    if let Some(next_node) = next_node.as_ref() {
        binary_expression_state_check_circularity(stack_index, node_stack, next_node);
        return Ok(binary_expression_state_push_stack(
            stack_index,
            state_stack,
            node_stack,
            user_state_stack,
            next_node,
        ));
    }
    Ok(stack_index)
}

fn binary_expression_state_exit<TMachine: BinaryExpressionStateMachine>(
    machine: &TMachine,
    mut stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    node_stack: &mut Vec<Gc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TMachine::TState>>,
    result_holder: Rc<RefCell<Option<TMachine::TResult>>>,
    _outer_state: TMachine::TOuterState,
) -> io::Result<usize> {
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
    )?;
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
    Ok(stack_index)
}

fn binary_expression_state_done<TMachine: BinaryExpressionStateMachine>(
    _machine: &TMachine,
    stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    _node_stack: &mut Vec<Gc<Node /*BinaryExpression*/>>,
    _user_state_stack: &mut Vec<Option<TMachine::TState>>,
    _result_holder: Rc<RefCell<Option<TMachine::TResult>>>,
    _outer_state: TMachine::TOuterState,
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
    node_stack: &mut Vec<Gc<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TState>>,
    node: &Node, /*BinaryExpression*/
) -> usize {
    stack_index += 1;
    push_or_replace(state_stack, stack_index, BinaryExpressionState::Enter);
    push_or_replace(node_stack, stack_index, node.node_wrapper());
    push_or_replace(user_state_stack, stack_index, None);
    stack_index
}

fn binary_expression_state_check_circularity(
    mut stack_index: usize,
    node_stack: &[Gc<Node /*BinaryExpression*/>],
    node: &Node, /*BinaryExpression*/
) {
    if Debug_.should_assert(AssertionLevel::Aggressive) {
        loop
        /*while stackIndex >= 0*/
        {
            Debug_.assert(
                !ptr::eq(&*node_stack[stack_index], node),
                Some("Circular traversal detected."),
            );
            if stack_index == 0 {
                break;
            }
            stack_index -= 1;
        }
    }
}

pub trait BinaryExpressionStateMachine: Trace + Finalize {
    type TResult: Clone;
    type TOuterState: Clone;
    type TState: Clone;

    fn on_enter(
        &self,
        node: &Node, /*BinaryExpression*/
        prev: Option<Self::TState>,
        outer_state: Self::TOuterState,
    ) -> io::Result<Self::TState>;

    fn on_left(
        &self,
        _left: &Node, /*Expression*/
        _user_state: Self::TState,
        _node: &Node, /*BinaryExpression*/
    ) -> io::Result<Option<Gc<Node /*BinaryExpression*/>>> {
        panic!("Shouldn't call default on_left()")
    }

    fn on_operator(
        &self,
        _operator_token: &Node, /*BinaryOperatorToken*/
        _user_state: Self::TState,
        _node: &Node, /*BinaryExpression*/
    ) -> io::Result<()> {
        panic!("Shouldn't call default on_operator()")
    }

    fn on_right(
        &self,
        _right: &Node, /*Expression*/
        _user_state: Self::TState,
        _node: &Node, /*BinaryExpression*/
    ) -> io::Result<Option<Gc<Node /*BinaryExpression*/>>> {
        panic!("Shouldn't call default on_left()")
    }

    fn on_exit(
        &self,
        node: &Node, /*BinaryExpression*/
        user_state: Self::TState,
    ) -> io::Result<Self::TResult>;

    fn fold_state(
        &self,
        _user_state: Self::TState,
        _result: Self::TResult,
        _side: LeftOrRight,
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

#[derive(Debug, Trace, Finalize)]
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
    ) -> io::Result<TMachine::TResult> {
        let result_holder: Rc<RefCell<Option<TMachine::TResult>>> = Rc::new(RefCell::new(None));
        let mut state_stack: Vec<BinaryExpressionState> = vec![BinaryExpressionState::Enter];
        let mut node_stack: Vec<Gc<Node /*BinaryExpression*/>> = vec![node.node_wrapper()];
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
            )?;
        }
        Debug_.assert_equal(&stack_index, &0, None, None);
        let ret = (*result_holder).borrow().clone().unwrap();
        Ok(ret)
    }
}
