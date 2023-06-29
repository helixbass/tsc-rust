use std::{borrow::Borrow, cell::RefCell, io, ptr, rc::Rc};

use gc::{Finalize, Gc, Trace};

use crate::{
    first, first_or_undefined, get_all_accessor_declarations, get_emit_flags, get_jsdoc_type,
    get_jsdoc_type_tag, get_or_create_emit_node, get_parse_node_factory, get_parse_tree_node,
    id_text, is_assignment_expression, is_computed_property_name, is_declaration_binding_element,
    is_exclamation_token, is_identifier, is_in_js_file, is_member_name, is_minus_token,
    is_object_literal_element_like, is_parenthesized_expression, is_plus_token,
    is_private_identifier, is_prologue_directive, is_qualified_name, is_question_token,
    is_readonly_keyword, is_source_file, is_spread_element, is_string_literal, is_this_type_node,
    is_type_node, is_type_parameter_declaration, is_variable_declaration_list,
    maybe_get_original_node_full, push_or_replace, set_starts_on_new_line, AllAccessorDeclarations,
    AssertionLevel, BaseNodeFactory, CompilerOptions, Debug_, EmitFlags, EmitHelperFactory,
    EmitHost, EmitResolver, FunctionLikeDeclarationInterface, HasInitializerInterface,
    LiteralLikeNodeInterface, Matches, NamedDeclarationInterface, Node, NodeArray, NodeExt,
    NodeFactory, NodeInterface, NonEmpty, OuterExpressionKinds,
    PropertyDescriptorAttributesBuilder, ReadonlyTextRange, SignatureDeclarationInterface,
    SyntaxKind,
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
    factory: &NodeFactory<TBaseNodeFactory>,
    target: &Node,      /*Expression*/
    member_name: &Node, /*PropertyName*/
    location: Option<&impl ReadonlyTextRange>,
) -> Gc<Node /*MemberExpression*/> {
    if is_computed_property_name(member_name) {
        factory
            .create_element_access_expression(
                target.node_wrapper(),
                member_name.as_computed_property_name().expression.clone(),
            )
            .set_text_range(location)
    } else {
        let expression = if is_member_name(member_name) {
            factory.create_property_access_expression(
                target.node_wrapper(),
                member_name.node_wrapper(),
            )
        } else {
            factory
                .create_element_access_expression(target.node_wrapper(), member_name.node_wrapper())
        }
        .set_text_range(Some(member_name));
        let emit_node = get_or_create_emit_node(&expression);
        let mut emit_node = emit_node.borrow_mut();
        emit_node.flags = Some(emit_node.flags.unwrap_or_default() | EmitFlags::NoNestedSourceMaps);
        expression
    }
}

fn create_react_namespace(
    react_namespace: &str,
    parent: &Node, /*JsxOpeningLikeElement | JsxOpeningFragment*/
) -> Gc<Node> {
    get_parse_node_factory()
        .create_identifier(react_namespace.non_empty().unwrap_or("React"))
        .and_set_parent(get_parse_tree_node(
            Some(parent),
            Option::<fn(&Node) -> bool>::None,
        ))
}

fn create_jsx_factory_expression_from_entity_name<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    factory: &NodeFactory<TBaseNodeFactory>,
    jsx_factory: &Node, /*EntityName*/
    parent: &Node,      /*JsxOpeningLikeElement | JsxOpeningFragment*/
) -> Gc<Node /*Expression*/> {
    if is_qualified_name(jsx_factory) {
        let jsx_factory_as_qualified_name = jsx_factory.as_qualified_name();
        let left = create_jsx_factory_expression_from_entity_name(
            factory,
            &jsx_factory_as_qualified_name.left,
            parent,
        );
        let right = factory.create_identifier(id_text(&jsx_factory_as_qualified_name.right));
        factory.create_property_access_expression(left, right)
    } else {
        create_react_namespace(id_text(jsx_factory), parent)
    }
}

pub fn create_jsx_factory_expression<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    factory: &NodeFactory<TBaseNodeFactory>,
    jsx_factory_entity: Option<impl Borrow<Node /*EntityName*/>>,
    react_namespace: &str,
    parent: &Node, /*JsxOpeningLikeElement | JsxOpeningFragment*/
) -> Gc<Node /*Expression*/> {
    jsx_factory_entity.map_or_else(
        || {
            factory.create_property_access_expression(
                create_react_namespace(react_namespace, parent),
                "createElement",
            )
        },
        |jsx_factory_entity| {
            let jsx_factory_entity = jsx_factory_entity.borrow();
            create_jsx_factory_expression_from_entity_name(factory, jsx_factory_entity, parent)
        },
    )
}

pub fn create_jsx_fragment_factory_expression<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    factory: &NodeFactory<TBaseNodeFactory>,
    jsx_fragment_factory_entity: Option<impl Borrow<Node /*EntityName*/>>,
    react_namespace: &str,
    parent: &Node, /*JsxOpeningLikeElement | JsxOpeningFragment*/
) -> Gc<Node /*Expression*/> {
    jsx_fragment_factory_entity.map_or_else(
        || {
            factory.create_property_access_expression(
                create_react_namespace(react_namespace, parent),
                "Fragment",
            )
        },
        |jsx_fragment_factory_entity| {
            let jsx_fragment_factory_entity = jsx_fragment_factory_entity.borrow();
            create_jsx_factory_expression_from_entity_name(
                factory,
                jsx_fragment_factory_entity,
                parent,
            )
        },
    )
}

pub fn create_expression_for_jsx_element<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    factory: &NodeFactory<TBaseNodeFactory>,
    callee: &Node,   /*Expression*/
    tag_name: &Node, /*Expression*/
    props: Option<impl Borrow<Node /*Expression*/>>,
    children: Option<&[Gc<Node /*Expression*/>]>,
    location: &impl ReadonlyTextRange,
) -> Gc<Node /*LeftHandSideExpression*/> {
    let mut arguments_list = vec![tag_name.node_wrapper()];
    if let Some(props) = props.as_ref() {
        let props = props.borrow();
        arguments_list.push(props.node_wrapper());
    }

    if let Some(children) = children.non_empty() {
        if props.is_none() {
            arguments_list.push(factory.create_null());
        }

        if children.len() > 1 {
            for child in children {
                start_on_new_line(&**child);
                arguments_list.push(child.clone());
            }
        } else {
            arguments_list.push(children[0].clone());
        }
    }

    factory
        .create_call_expression(
            callee.node_wrapper(),
            Option::<Gc<NodeArray>>::None,
            Some(arguments_list),
        )
        .set_text_range(Some(location))
}

pub fn create_expression_for_jsx_fragment<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    factory: &NodeFactory<TBaseNodeFactory>,
    jsx_factory_entity: Option<impl Borrow<Node /*EntityName*/>>,
    jsx_fragment_factory_entity: Option<impl Borrow<Node /*EntityName*/>>,
    react_namespace: &str,
    children: &[Gc<Node /*Expression*/>],
    parent_element: &Node, /*JsxOpeningFragment*/
    location: &impl ReadonlyTextRange,
) -> Gc<Node /*LeftHandSideExpression*/> {
    let tag_name = create_jsx_fragment_factory_expression(
        factory,
        jsx_fragment_factory_entity,
        react_namespace,
        parent_element,
    );
    let mut arguments_list = vec![tag_name, factory.create_null()];

    if
    /*children &&*/
    !children.is_empty() {
        if children.len() > 1 {
            for child in children {
                start_on_new_line(&**child);
                arguments_list.push(child.clone());
            }
        } else {
            arguments_list.push(children[0].clone());
        }
    }

    factory
        .create_call_expression(
            create_jsx_factory_expression(
                factory,
                jsx_factory_entity,
                react_namespace,
                parent_element,
            ),
            Option::<Gc<NodeArray>>::None,
            Some(arguments_list),
        )
        .set_text_range(Some(location))
}

pub fn create_for_of_binding_statement<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    factory: &NodeFactory<TBaseNodeFactory>,
    node: &Node,        /*ForInitializer*/
    bound_value: &Node, /*Expression*/
) -> Gc<Node /*Statement*/> {
    if is_variable_declaration_list(node) {
        let first_declaration = first(&node.as_variable_declaration_list().declarations);
        let updated_declaration = factory.update_variable_declaration(
            first_declaration,
            first_declaration.as_variable_declaration().maybe_name(),
            None,
            None,
            Some(bound_value.node_wrapper()),
        );
        factory
            .create_variable_statement(
                Option::<Gc<NodeArray>>::None,
                factory.update_variable_declaration_list(node, vec![updated_declaration]),
            )
            .set_text_range(Some(node))
    } else {
        let updated_expression = factory
            .create_assignment(node.node_wrapper(), bound_value.node_wrapper())
            .set_text_range(Some(node));
        factory
            .create_expression_statement(updated_expression)
            .set_text_range(Some(node))
    }
}

pub fn create_expression_from_entity_name<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    factory: &NodeFactory<TBaseNodeFactory>,
    node: &Node, /*EntityName | Expression*/
) -> Gc<Node /*Expression*/> {
    if is_qualified_name(node) {
        let node_as_qualified_name = node.as_qualified_name();
        let left = create_expression_from_entity_name(factory, &node_as_qualified_name.left);
        let right = factory
            .clone_node(&node_as_qualified_name.right)
            .set_text_range(Some(&*node_as_qualified_name.right))
            .and_set_parent(node_as_qualified_name.right.maybe_parent());
        factory
            .create_property_access_expression(left, right)
            .set_text_range(Some(node))
    } else {
        factory
            .clone_node(node)
            .set_text_range(Some(node))
            .and_set_parent(node.maybe_parent())
    }
}

pub fn create_expression_for_property_name<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    factory: &NodeFactory<TBaseNodeFactory>,
    member_name: &Node, /*Exclude<PropertyName, PrivateIdentifier>*/
) -> Gc<Node /*Expression*/> {
    if is_identifier(member_name) {
        factory.create_string_literal_from_node(member_name)
    } else if is_computed_property_name(member_name) {
        let member_name_as_computed_property_name = member_name.as_computed_property_name();
        factory
            .clone_node(&member_name_as_computed_property_name.expression)
            .set_text_range(Some(&*member_name_as_computed_property_name.expression))
            .and_set_parent(
                member_name_as_computed_property_name
                    .expression
                    .maybe_parent(),
            )
    } else {
        factory
            .clone_node(member_name)
            .set_text_range(Some(member_name))
            .and_set_parent(member_name.maybe_parent())
    }
}

fn create_expression_for_accessor_declaration<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    factory: &NodeFactory<TBaseNodeFactory>,
    properties: &NodeArray, /*<Declaration>*/
    property: &Node, /*AccessorDeclaration & { readonly name: Exclude<PropertyName, PrivateIdentifier> }*/
    receiver: &Node, /*Expression*/
    multi_line: bool,
) -> Option<Gc<Node>> {
    let AllAccessorDeclarations {
        first_accessor,
        get_accessor,
        set_accessor,
        ..
    } = get_all_accessor_declarations(properties, property);
    if ptr::eq(property, &*first_accessor) {
        return Some(
            factory
                .create_object_define_property_call(
                    receiver.node_wrapper(),
                    create_expression_for_property_name(
                        factory,
                        &property.as_named_declaration().name(),
                    ),
                    factory.create_property_descriptor(
                        PropertyDescriptorAttributesBuilder::default()
                            .enumerable(factory.create_false())
                            .configurable(true)
                            .get(get_accessor.map(|get_accessor| {
                                let get_accessor_as_get_accessor_declaration =
                                    get_accessor.as_get_accessor_declaration();
                                factory
                                    .create_function_expression(
                                        get_accessor.maybe_modifiers(),
                                        None,
                                        Option::<Gc<Node>>::None,
                                        Option::<Gc<NodeArray>>::None,
                                        Some(get_accessor_as_get_accessor_declaration.parameters()),
                                        None,
                                        get_accessor_as_get_accessor_declaration
                                            .maybe_body()
                                            .unwrap(),
                                    )
                                    .set_original_node(Some(get_accessor.clone()))
                                    .set_text_range(Some(&*get_accessor))
                            }))
                            .set(set_accessor.map(|set_accessor| {
                                let set_accessor_as_set_accessor_declaration =
                                    set_accessor.as_set_accessor_declaration();
                                factory
                                    .create_function_expression(
                                        set_accessor.maybe_modifiers(),
                                        None,
                                        Option::<Gc<Node>>::None,
                                        Option::<Gc<NodeArray>>::None,
                                        Some(set_accessor_as_set_accessor_declaration.parameters()),
                                        None,
                                        set_accessor_as_set_accessor_declaration
                                            .maybe_body()
                                            .unwrap(),
                                    )
                                    .set_original_node(Some(set_accessor.clone()))
                                    .set_text_range(Some(&*set_accessor))
                            }))
                            .build()
                            .unwrap(),
                        Some(!multi_line),
                    ),
                )
                .set_text_range(Some(&*first_accessor)),
        );
    }

    None
}

fn create_expression_for_property_assignment<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _property: &Node, /*PropertyAssignment*/
    _receiver: &Node, /*Expression*/
) -> Gc<Node> {
    unimplemented!()
}

fn create_expression_for_shorthand_property_assignment<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _property: &Node, /*ShorthandPropertyAssignment*/
    _receiver: &Node, /*Expression*/
) -> Gc<Node> {
    unimplemented!()
}

fn create_expression_for_method_declaration<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _property: &Node, /*MethodDeclaration*/
    _receiver: &Node, /*Expression*/
) -> Gc<Node> {
    unimplemented!()
}

pub fn create_expression_for_object_literal_element_like<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    factory: &NodeFactory<TBaseNodeFactory>,
    node: &Node,     /*ObjectLiteralExpression*/
    property: &Node, /*ObjectLiteralElementLike*/
    receiver: &Node, /*Expression*/
) -> Option<Gc<Node /*Expression*/>> {
    let node_as_object_literal_expression = node.as_object_literal_expression();
    if property
        .as_named_declaration()
        .maybe_name()
        .matches(|ref property_name| is_private_identifier(property_name))
    {
        Debug_.fail_bad_syntax_kind(
            &property.as_named_declaration().name(),
            Some("Private identifiers are not allowed in object literals."),
        );
    }
    match property.kind() {
        SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
            create_expression_for_accessor_declaration(
                factory,
                &node_as_object_literal_expression.properties,
                property,
                receiver,
                node_as_object_literal_expression.multi_line == Some(true),
            )
        }
        SyntaxKind::PropertyAssignment => Some(create_expression_for_property_assignment(
            factory, property, receiver,
        )),
        SyntaxKind::ShorthandPropertyAssignment => Some(
            create_expression_for_shorthand_property_assignment(factory, property, receiver),
        ),
        SyntaxKind::MethodDeclaration => Some(create_expression_for_method_declaration(
            factory, property, receiver,
        )),
        _ => unreachable!(),
    }
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

pub fn get_rest_indicator_of_binding_or_assignment_element(
    _binding_element: &Node, /*BindingOrAssignmentElement*/
) -> Option<Gc<Node /*BindingOrAssignmentElementRestIndicator*/>> {
    unimplemented!()
}

pub fn get_property_name_of_binding_or_assignment_element(
    _binding_element: &Node, /*BindingOrAssignmentElement*/
) -> Option<Gc<Node /*Exclude<PropertyName, PrivateIdentifier>*/>> {
    unimplemented!()
}

pub fn try_get_property_name_of_binding_or_assignment_element(
    _binding_element: &Node, /*BindingOrAssignmentElement*/
) -> Option<Gc<Node /*Exclude<PropertyName, PrivateIdentifier>*/>> {
    unimplemented!()
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
