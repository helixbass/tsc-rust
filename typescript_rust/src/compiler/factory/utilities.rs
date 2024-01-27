use std::{borrow::Borrow, cell::RefCell, io, ptr, rc::Rc};

use gc::{Finalize, Gc, Trace};
use id_arena::Id;

use crate::{
    first, first_or_undefined, get_all_accessor_declarations, get_emit_flags, get_jsdoc_type,
    get_jsdoc_type_tag, get_or_create_emit_node, get_parse_node_factory, get_parse_tree_node,
    id_text, is_assignment_expression, is_computed_property_name, is_declaration_binding_element,
    is_effective_external_module, is_exclamation_token, is_identifier, is_in_js_file,
    is_member_name, is_minus_token, is_object_literal_element_like, is_parenthesized_expression,
    is_plus_token, is_postfix_unary_expression, is_prefix_unary_expression, is_private_identifier,
    is_prologue_directive, is_qualified_name, is_question_token, is_readonly_keyword,
    is_source_file, is_spread_element, is_string_literal, is_this_type_node, is_type_node,
    is_type_parameter_declaration, is_variable_declaration_list, maybe_get_original_node_full,
    push_or_replace, set_starts_on_new_line, AllAccessorDeclarations, AssertionLevel,
    BaseNodeFactory, CompilerOptions, Debug_, EmitFlags, EmitHelperBase, EmitHelperFactory,
    EmitHost, EmitResolver, FunctionLikeDeclarationInterface, HasInitializerInterface,
    LiteralLikeNodeInterface, Matches, NamedDeclarationInterface, Node, NodeArray, NodeExt,
    NodeFactory, NodeInterface, NonEmpty, OuterExpressionKinds,
    PropertyDescriptorAttributesBuilder, ReadonlyTextRange, SignatureDeclarationInterface,
    SyntaxKind, _d, compare_strings_case_sensitive, comparison_to_ordering,
    external_helpers_module_name_text, get_emit_helpers, get_emit_module_kind,
    get_es_module_interop, get_external_module_name, get_external_module_name_from_path,
    get_namespace_declaration_node, get_source_text_of_node_from_source_file,
    is_assignment_operator, is_default_import, is_export_namespace_as_default_declaration,
    is_file_level_unique_name, is_generated_identifier, is_property_assignment, is_property_name,
    is_shorthand_property_assignment, is_spread_assignment, map, out_file, push_if_unique_eq,
    HasArena, ModuleKind, InArena,
};

pub fn create_empty_exports(
    factory: &NodeFactory,
) -> Id<Node> {
    factory.create_export_declaration(
        Option::<Id<NodeArray>>::None,
        Option::<Id<NodeArray>>::None,
        false,
        Some(factory.create_named_exports(vec![])),
        None,
        None,
    )
}

pub fn create_member_access_for_property_name(
    factory: &NodeFactory,
    target: Id<Node>,      /*Expression*/
    member_name: Id<Node>, /*PropertyName*/
    location: Option<&impl ReadonlyTextRange>,
) -> Id<Node /*MemberExpression*/> {
    if is_computed_property_name(&member_name.ref_(factory)) {
        factory
            .create_element_access_expression(
                target,
                member_name.ref_(factory).as_computed_property_name().expression,
            )
            .set_text_range(location, factory)
    } else {
        let expression = if is_member_name(&member_name.ref_(factory)) {
            factory.create_property_access_expression(
                target,
                member_name,
            )
        } else {
            factory
                .create_element_access_expression(target, member_name)
        }
        .set_text_range(Some(&*member_name.ref_(factory)), factory);
        let emit_node = get_or_create_emit_node(expression, factory);
        let mut emit_node = emit_node.borrow_mut();
        emit_node.flags = Some(emit_node.flags.unwrap_or_default() | EmitFlags::NoNestedSourceMaps);
        expression
    }
}

fn create_react_namespace(
    react_namespace: Option<&str>,
    parent: Id<Node>, /*JsxOpeningLikeElement | JsxOpeningFragment*/
    arena: &impl HasArena,
) -> Id<Node> {
    let factory = get_parse_node_factory(arena);
    factory
        .create_identifier(react_namespace.non_empty().unwrap_or("React"))
        .and_set_parent(get_parse_tree_node(
            Some(parent),
            Option::<fn(Id<Node>) -> bool>::None,
            arena,
        ), arena)
}

fn create_jsx_factory_expression_from_entity_name(
    factory: &NodeFactory,
    jsx_factory: Id<Node>, /*EntityName*/
    parent: Id<Node>,      /*JsxOpeningLikeElement | JsxOpeningFragment*/
) -> Id<Node /*Expression*/> {
    if is_qualified_name(&jsx_factory.ref_(factory)) {
        let jsx_factory_ref = jsx_factory.ref_(factory);
        let jsx_factory_as_qualified_name = jsx_factory_ref.as_qualified_name();
        let left = create_jsx_factory_expression_from_entity_name(
            factory,
            jsx_factory_as_qualified_name.left,
            parent,
        );
        let right = factory.create_identifier(id_text(&jsx_factory_as_qualified_name.right.ref_(factory)));
        factory.create_property_access_expression(left, right)
    } else {
        create_react_namespace(Some(id_text(&jsx_factory.ref_(factory))), parent, factory)
    }
}

pub fn create_jsx_factory_expression(
    factory: &NodeFactory,
    jsx_factory_entity: Option<Id<Node /*EntityName*/>>,
    react_namespace: Option<&str>,
    parent: Id<Node>, /*JsxOpeningLikeElement | JsxOpeningFragment*/
) -> Id<Node /*Expression*/> {
    jsx_factory_entity.map_or_else(
        || {
            factory.create_property_access_expression(
                create_react_namespace(react_namespace, parent, factory),
                "createElement",
            )
        },
        |jsx_factory_entity| {
            create_jsx_factory_expression_from_entity_name(factory, jsx_factory_entity, parent)
        },
    )
}

pub fn create_jsx_fragment_factory_expression(
    factory: &NodeFactory,
    jsx_fragment_factory_entity: Option<Id<Node /*EntityName*/>>,
    react_namespace: &str,
    parent: Id<Node>, /*JsxOpeningLikeElement | JsxOpeningFragment*/
) -> Id<Node /*Expression*/> {
    jsx_fragment_factory_entity.map_or_else(
        || {
            factory.create_property_access_expression(
                create_react_namespace(Some(react_namespace), parent, factory),
                "Fragment",
            )
        },
        |jsx_fragment_factory_entity| {
            create_jsx_factory_expression_from_entity_name(
                factory,
                jsx_fragment_factory_entity,
                parent,
            )
        },
    )
}

pub fn create_expression_for_jsx_element(
    factory: &NodeFactory,
    callee: Id<Node>,   /*Expression*/
    tag_name: Id<Node>, /*Expression*/
    props: Option<Id<Node /*Expression*/>>,
    children: Option<&[Id<Node /*Expression*/>]>,
    location: &impl ReadonlyTextRange,
) -> Id<Node /*LeftHandSideExpression*/> {
    let mut arguments_list = vec![tag_name];
    if let Some(props) = props {
        arguments_list.push(props);
    }

    if let Some(children) = children.non_empty() {
        if props.is_none() {
            arguments_list.push(factory.create_null());
        }

        if children.len() > 1 {
            for &child in children {
                start_on_new_line(child, factory);
                arguments_list.push(child);
            }
        } else {
            arguments_list.push(children[0]);
        }
    }

    factory
        .create_call_expression(
            callee,
            Option::<Id<NodeArray>>::None,
            Some(arguments_list),
        )
        .set_text_range(Some(location), factory)
}

pub fn create_expression_for_jsx_fragment(
    factory: &NodeFactory,
    jsx_factory_entity: Option<Id<Node /*EntityName*/>>,
    jsx_fragment_factory_entity: Option<Id<Node /*EntityName*/>>,
    react_namespace: &str,
    children: &[Id<Node /*Expression*/>],
    parent_element: Id<Node>, /*JsxOpeningFragment*/
    location: &impl ReadonlyTextRange,
) -> Id<Node /*LeftHandSideExpression*/> {
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
            for &child in children {
                start_on_new_line(child, factory);
                arguments_list.push(child);
            }
        } else {
            arguments_list.push(children[0]);
        }
    }

    factory
        .create_call_expression(
            create_jsx_factory_expression(
                factory,
                jsx_factory_entity,
                Some(react_namespace),
                parent_element,
            ),
            Option::<Id<NodeArray>>::None,
            Some(arguments_list),
        )
        .set_text_range(Some(location), factory)
}

pub fn create_for_of_binding_statement(
    factory: &NodeFactory,
    node: Id<Node>,        /*ForInitializer*/
    bound_value: Id<Node>, /*Expression*/
) -> Id<Node /*Statement*/> {
    if is_variable_declaration_list(&node.ref_(factory)) {
        let first_declaration = *first(&node.ref_(factory).as_variable_declaration_list().declarations.ref_(factory));
        let updated_declaration = factory.update_variable_declaration(
            first_declaration,
            first_declaration.ref_(factory).as_variable_declaration().maybe_name(),
            None,
            None,
            Some(bound_value),
        );
        factory
            .create_variable_statement(
                Option::<Id<NodeArray>>::None,
                factory.update_variable_declaration_list(node, vec![updated_declaration]),
            )
            .set_text_range(Some(&*node.ref_(factory)), factory)
    } else {
        let updated_expression = factory
            .create_assignment(node, bound_value)
            .set_text_range(Some(&*node.ref_(factory)), factory);
        factory
            .create_expression_statement(updated_expression)
            .set_text_range(Some(&*node.ref_(factory)), factory)
    }
}

pub fn create_expression_from_entity_name(
    factory: &NodeFactory,
    node: Id<Node>, /*EntityName | Expression*/
) -> Id<Node /*Expression*/> {
    if is_qualified_name(&node.ref_(factory)) {
        let node_ref = node.ref_(factory);
        let node_as_qualified_name = node_ref.as_qualified_name();
        let left = create_expression_from_entity_name(factory, node_as_qualified_name.left);
        let right = factory
            .clone_node(node_as_qualified_name.right)
            .set_text_range(Some(&*node_as_qualified_name.right.ref_(factory)), factory)
            .and_set_parent(node_as_qualified_name.right.ref_(factory).maybe_parent(), factory);
        factory
            .create_property_access_expression(left, right)
            .set_text_range(Some(&*node.ref_(factory)), factory)
    } else {
        factory
            .clone_node(node)
            .set_text_range(Some(&*node.ref_(factory)), factory)
            .and_set_parent(node.ref_(factory).maybe_parent(), factory)
    }
}

pub fn create_expression_for_property_name(
    factory: &NodeFactory,
    member_name: Id<Node>, /*Exclude<PropertyName, PrivateIdentifier>*/
) -> Id<Node /*Expression*/> {
    if is_identifier(&member_name.ref_(factory)) {
        factory.create_string_literal_from_node(member_name)
    } else if is_computed_property_name(&member_name.ref_(factory)) {
        let member_name_ref = member_name.ref_(factory);
        let member_name_as_computed_property_name = member_name_ref.as_computed_property_name();
        factory
            .clone_node(member_name_as_computed_property_name.expression)
            .set_text_range(Some(&*member_name_as_computed_property_name.expression.ref_(factory)), factory)
            .and_set_parent(
                member_name_as_computed_property_name
                    .expression
                    .ref_(factory).maybe_parent(),
                factory,
            )
    } else {
        factory
            .clone_node(member_name)
            .set_text_range(Some(&*member_name.ref_(factory)), factory)
            .and_set_parent(member_name.ref_(factory).maybe_parent(), factory)
    }
}

fn create_expression_for_accessor_declaration(
    factory: &NodeFactory,
    properties: Id<NodeArray>, /*<Declaration>*/
    property: Id<Node>, /*AccessorDeclaration & { readonly name: Exclude<PropertyName, PrivateIdentifier> }*/
    receiver: Id<Node>, /*Expression*/
    multi_line: bool,
) -> Option<Id<Node>> {
    let AllAccessorDeclarations {
        first_accessor,
        get_accessor,
        set_accessor,
        ..
    } = get_all_accessor_declarations(&properties.ref_(factory), property, factory);
    if property == first_accessor {
        return Some(
            factory
                .create_object_define_property_call(
                    receiver,
                    create_expression_for_property_name(
                        factory,
                        property.ref_(factory).as_named_declaration().name(),
                    ),
                    factory.create_property_descriptor(
                        PropertyDescriptorAttributesBuilder::default()
                            .enumerable(factory.create_false())
                            .configurable(true)
                            .get(get_accessor.map(|get_accessor| {
                                let get_accessor_ref = get_accessor.ref_(factory);
                                let get_accessor_as_get_accessor_declaration =
                                    get_accessor_ref.as_get_accessor_declaration();
                                factory
                                    .create_function_expression(
                                        get_accessor_ref.maybe_modifiers(),
                                        None,
                                        Option::<Id<Node>>::None,
                                        Option::<Id<NodeArray>>::None,
                                        Some(get_accessor_as_get_accessor_declaration.parameters()),
                                        None,
                                        get_accessor_as_get_accessor_declaration
                                            .maybe_body()
                                            .unwrap(),
                                    )
                                    .set_original_node(Some(get_accessor), factory)
                                    .set_text_range(Some(&*get_accessor.ref_(factory)), factory)
                            }))
                            .set(set_accessor.map(|set_accessor| {
                                let set_accessor_ref = set_accessor.ref_(factory);
                                let set_accessor_as_set_accessor_declaration =
                                    set_accessor_ref.as_set_accessor_declaration();
                                factory
                                    .create_function_expression(
                                        set_accessor_ref.maybe_modifiers(),
                                        None,
                                        Option::<Id<Node>>::None,
                                        Option::<Id<NodeArray>>::None,
                                        Some(set_accessor_as_set_accessor_declaration.parameters()),
                                        None,
                                        set_accessor_as_set_accessor_declaration
                                            .maybe_body()
                                            .unwrap(),
                                    )
                                    .set_original_node(Some(set_accessor), factory)
                                    .set_text_range(Some(&*set_accessor.ref_(factory)), factory)
                            }))
                            .build()
                            .unwrap(),
                        Some(!multi_line),
                    ),
                )
                .set_text_range(Some(&*first_accessor.ref_(factory)), factory),
        );
    }

    None
}

fn create_expression_for_property_assignment(
    factory: &NodeFactory,
    property: Id<Node>, /*PropertyAssignment*/
    receiver: Id<Node>, /*Expression*/
) -> Id<Node> {
    let property_ref = property.ref_(factory);
    let property_as_property_assignment = property_ref.as_property_assignment();
    factory
        .create_assignment(
            create_member_access_for_property_name(
                factory,
                receiver,
                property_as_property_assignment.name(),
                Some(&*property_as_property_assignment.name().ref_(factory)),
            ),
            property_as_property_assignment.initializer.clone(),
        )
        .set_text_range(Some(&*property.ref_(factory)), factory)
        .set_original_node(Some(property), factory)
}

fn create_expression_for_shorthand_property_assignment(
    factory: &NodeFactory,
    property: Id<Node>, /*ShorthandPropertyAssignment*/
    receiver: Id<Node>, /*Expression*/
) -> Id<Node> {
    let property_ref = property.ref_(factory);
    let property_as_shorthand_property_assignment = property_ref.as_shorthand_property_assignment();
    factory
        .create_assignment(
            create_member_access_for_property_name(
                factory,
                receiver,
                property_as_shorthand_property_assignment.name(),
                Some(&*property_as_shorthand_property_assignment.name().ref_(factory)),
            ),
            factory.clone_node(property_as_shorthand_property_assignment.name()),
        )
        .set_text_range(Some(&*property.ref_(factory)), factory)
        .set_original_node(Some(property), factory)
}

fn create_expression_for_method_declaration(
    factory: &NodeFactory,
    method: Id<Node>,   /*MethodDeclaration*/
    receiver: Id<Node>, /*Expression*/
) -> Id<Node> {
    let method_ref = method.ref_(factory);
    let method_as_method_declaration = method_ref.as_method_declaration();
    factory
        .create_assignment(
            create_member_access_for_property_name(
                factory,
                receiver,
                method_as_method_declaration.name(),
                Some(&*method_as_method_declaration.name().ref_(factory)),
            ),
            factory
                .create_function_expression(
                    method.ref_(factory).maybe_modifiers(),
                    method_as_method_declaration.maybe_asterisk_token(),
                    Option::<Id<Node>>::None,
                    Option::<Id<NodeArray>>::None,
                    Some(method_as_method_declaration.parameters()),
                    None,
                    method_as_method_declaration.maybe_body().unwrap(),
                )
                .set_text_range(Some(&*method.ref_(factory)), factory)
                .set_original_node(Some(method), factory),
        )
        .set_text_range(Some(&*method.ref_(factory)), factory)
        .set_original_node(Some(method), factory)
}

pub fn create_expression_for_object_literal_element_like(
    factory: &NodeFactory,
    node: Id<Node>,     /*ObjectLiteralExpression*/
    property: Id<Node>, /*ObjectLiteralElementLike*/
    receiver: Id<Node>, /*Expression*/
) -> Option<Id<Node /*Expression*/>> {
    let node_ref = node.ref_(factory);
    let node_as_object_literal_expression = node_ref.as_object_literal_expression();
    if property
        .ref_(factory).as_named_declaration()
        .maybe_name()
        .matches(|property_name| is_private_identifier(&property_name.ref_(factory)))
    {
        Debug_.fail_bad_syntax_kind(
            &property.ref_(factory).as_named_declaration().name().ref_(factory),
            Some("Private identifiers are not allowed in object literals."),
        );
    }
    match property.ref_(factory).kind() {
        SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
            create_expression_for_accessor_declaration(
                factory,
                node_as_object_literal_expression.properties,
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

pub fn expand_pre_or_postfix_increment_or_decrement_expression(
    factory: &NodeFactory,
    node: Id<Node>,       /*PrefixUnaryExpression | PostfixUnaryExpression*/
    expression: Id<Node>, /*Expression*/
    record_temp_variable: impl FnMut(Id<Node> /*Expression*/),
    result_variable: Option<Id<Node /*Identifier*/>>,
) -> Id<Node /*Expression*/> {
    let node_ref = node.ref_(factory);
    let node_as_unary_expression = node_ref.as_unary_expression();
    let operator = node_as_unary_expression.operator();
    Debug_.assert(
        matches!(
            operator,
            SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
        ),
        Some("Expected 'node' to be a pre- or post-increment or pre- or post-decrement expression"),
    );

    let temp = factory.create_temp_variable(Some(record_temp_variable), None);
    let mut expression = factory
        .create_assignment(temp, expression)
        .set_text_range(Some(&*node_as_unary_expression.operand().ref_(factory)), factory);

    let mut operation/*: Expression*/ = if is_prefix_unary_expression(&node.ref_(factory)) {
        factory.create_prefix_unary_expression(operator, temp.clone())
    } else {
        factory.create_postfix_unary_expression(temp.clone(), operator)
    }.set_text_range(Some(&*node.ref_(factory)), factory);

    if let Some(result_variable) = result_variable {
        operation = factory
            .create_assignment(result_variable, operation)
            .set_text_range(Some(&*node.ref_(factory)), factory);
    }

    expression = factory.create_comma(expression, operation);

    if is_postfix_unary_expression(&node.ref_(factory)) {
        expression = factory
            .create_comma(expression, temp)
            .set_text_range(Some(&*node.ref_(factory)), factory);
    }

    expression
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

fn is_use_strict_prologue(node: Id<Node> /*ExpressionStatement*/, arena: &impl HasArena) -> bool {
    let node_ref = node.ref_(arena);
    let node_as_expression_statement = node_ref.as_expression_statement();
    is_string_literal(&node_as_expression_statement.expression.ref_(arena))
        && &*node_as_expression_statement
            .expression
            .ref_(arena).as_string_literal()
            .text()
            == "use strict"
}

pub fn find_use_strict_prologue(
    statements: &[Id<Node /*Statement*/>],
    arena: &impl HasArena,
) -> Option<Id<Node /*Statement*/>> {
    for &statement in statements {
        if is_prologue_directive(statement, arena) {
            if is_use_strict_prologue(statement, arena) {
                return Some(statement);
            }
        } else {
            break;
        }
    }
    None
}

pub fn starts_with_use_strict(statements: &[Id<Node>], arena: &impl HasArena) -> bool {
    let Some(&first_statement) = first_or_undefined(statements) else {
        return false;
    };
    is_prologue_directive(first_statement, arena) && is_use_strict_prologue(first_statement, arena)
}

pub fn is_comma_sequence(node: Id<Node> /*Expression*/, arena: &impl HasArena) -> bool {
    node.ref_(arena).kind() == SyntaxKind::BinaryExpression
        && node.ref_(arena).as_binary_expression().operator_token.ref_(arena).kind() == SyntaxKind::CommaToken
        || node.ref_(arena).kind() == SyntaxKind::CommaListExpression
}

pub fn is_jsdoc_type_assertion(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_parenthesized_expression(&node.ref_(arena))
        && is_in_js_file(Some(&node.ref_(arena)))
        && get_jsdoc_type_tag(node, arena).is_some()
}

pub fn get_jsdoc_type_assertion_type(node: Id<Node> /*JSDocTypeAssertion*/, arena: &impl HasArena) -> Id<Node> {
    let type_ = get_jsdoc_type(node, arena);
    Debug_.assert_is_defined(&type_, None);
    type_.unwrap()
}

pub fn is_outer_expression(node: Id<Node>, kinds: Option<OuterExpressionKinds>, arena: &impl HasArena) -> bool {
    let kinds = kinds.unwrap_or(OuterExpressionKinds::All);
    match node.ref_(arena).kind() {
        SyntaxKind::ParenthesizedExpression => {
            if kinds.intersects(OuterExpressionKinds::ExcludeJSDocTypeAssertion)
                && is_jsdoc_type_assertion(node, arena)
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

pub fn skip_outer_expressions(mut node: Id<Node>, kinds: Option<OuterExpressionKinds>, arena: &impl HasArena) -> Id<Node> {
    let kinds = kinds.unwrap_or(OuterExpressionKinds::All);
    while is_outer_expression(node, Some(kinds), arena) {
        node = node.ref_(arena).as_has_expression().expression();
    }
    node
}

pub fn start_on_new_line(node: Id<Node>, arena: &impl HasArena) -> Id<Node> {
    set_starts_on_new_line(node, true, arena);
    node
}

pub fn get_external_helpers_module_name(node: Id<Node> /*SourceFile*/, arena: &impl HasArena) -> Option<Id<Node>> {
    let parse_node = maybe_get_original_node_full(
        Some(node),
        Some(|node: Option<Id<Node>>| is_source_file(&node.unwrap().ref_(arena))),
        arena,
    )?;
    let emit_node = parse_node.ref_(arena).maybe_emit_node()?;
    let ret = (*emit_node).borrow().external_helpers_module_name.clone();
    ret
}

pub fn has_recorded_external_helpers(source_file: Id<Node> /*SourceFile*/, arena: &impl HasArena) -> bool {
    let parse_node = maybe_get_original_node_full(
        Some(source_file),
        Some(|node: Option<Id<Node>>| is_source_file(&node.unwrap().ref_(arena))),
        arena,
    );
    let emit_node = parse_node.and_then(|parse_node| parse_node.ref_(arena).maybe_emit_node());
    matches!(
        emit_node,
        Some(emit_node) if {
            let emit_node = (*emit_node).borrow();
            emit_node.external_helpers_module_name.is_some() ||
                emit_node.external_helpers == Some(true)
        }
    )
}

pub fn create_external_helpers_import_declaration_if_needed(
    node_factory: &NodeFactory,
    helper_factory: &EmitHelperFactory,
    source_file: Id<Node>, /*SourceFile*/
    compiler_options: &CompilerOptions,
    has_export_stars_to_export_values: Option<bool>,
    has_import_star: Option<bool>,
    has_import_default: Option<bool>,
) -> Option<Id<Node>> {
    let source_file_ref = source_file.ref_(node_factory);
    let source_file_as_source_file = source_file_ref.as_source_file();
    if compiler_options.import_helpers == Some(true)
        && is_effective_external_module(&source_file.ref_(node_factory), compiler_options)
    {
        let mut named_bindings: Option<Id<Node /*NamedImportBindings*/>> = _d();
        let module_kind = get_emit_module_kind(compiler_options);
        if module_kind >= ModuleKind::ES2015 && module_kind <= ModuleKind::ESNext
            || source_file_as_source_file.maybe_implied_node_format() == Some(ModuleKind::ESNext)
        {
            let helpers = get_emit_helpers(&source_file.ref_(node_factory));
            if let Some(helpers) = helpers {
                let mut helper_names: Vec<String> = _d();
                for helper in helpers {
                    if !helper.ref_(node_factory).scoped() {
                        let helper_ref = helper.ref_(node_factory);
                        let import_name = helper_ref.as_unscoped_emit_helper().import_name.as_ref();
                        if let Some(import_name) = import_name.non_empty() {
                            push_if_unique_eq(&mut helper_names, import_name);
                        }
                    }
                }
                if !helper_names.is_empty() {
                    helper_names.sort_by(|a, b| {
                        comparison_to_ordering(compare_strings_case_sensitive(a, b))
                    });
                    named_bindings = Some(node_factory.create_named_imports(map(
                        helper_names,
                        |ref name: String, _| {
                            if is_file_level_unique_name(
                                &source_file.ref_(node_factory),
                                name,
                                Option::<fn(&str) -> bool>::None,
                            ) {
                                node_factory.create_import_specifier(
                                    false,
                                    None,
                                    node_factory.create_identifier(name),
                                )
                            } else {
                                node_factory.create_import_specifier(
                                    false,
                                    Some(node_factory.create_identifier(name)),
                                    helper_factory.get_unscoped_helper_name(name),
                                )
                            }
                        },
                    )));
                    let parse_node = maybe_get_original_node_full(
                        Some(source_file),
                        Some(|node: Option<Id<Node>>| is_source_file(&node.unwrap().ref_(node_factory))),
                        node_factory,
                    )
                    .unwrap();
                    let emit_node = get_or_create_emit_node(parse_node, node_factory);
                    emit_node.borrow_mut().external_helpers = Some(true);
                }
            }
        } else {
            let external_helpers_module_name = get_or_create_external_helpers_module_name_if_needed(
                node_factory,
                source_file,
                compiler_options,
                has_export_stars_to_export_values,
                Some(has_import_star == Some(true) || has_import_default == Some(true)),
            );
            if let Some(external_helpers_module_name) = external_helpers_module_name {
                named_bindings =
                    Some(node_factory.create_namespace_import(external_helpers_module_name));
            }
        }
        if let Some(named_bindings) = named_bindings {
            return Some(
                node_factory
                    .create_import_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        Some(node_factory.create_import_clause(false, None, Some(named_bindings))),
                        node_factory.create_string_literal(
                            external_helpers_module_name_text.to_owned(),
                            None,
                            None,
                        ),
                        None,
                    )
                    .add_emit_flags(EmitFlags::NeverApplyImportHelper, node_factory),
            );
        }
    }
    None
}

pub fn get_or_create_external_helpers_module_name_if_needed(
    factory: &NodeFactory,
    node: Id<Node>, /*SourceFile*/
    compiler_options: &CompilerOptions,
    has_export_stars_to_export_values: Option<bool>,
    has_import_star_or_import_default: Option<bool>,
) -> Option<Id<Node>> {
    let node_ref = node.ref_(factory);
    let node_as_source_file = node_ref.as_source_file();
    if compiler_options.import_helpers == Some(true)
        && is_effective_external_module(&node.ref_(factory), compiler_options)
    {
        let external_helpers_module_name = get_external_helpers_module_name(node, factory);
        if external_helpers_module_name.is_some() {
            return external_helpers_module_name;
        }

        let module_kind = get_emit_module_kind(compiler_options);
        let mut create = (has_export_stars_to_export_values == Some(true)
            || get_es_module_interop(compiler_options) == Some(true)
                && has_import_star_or_import_default == Some(true))
            && module_kind != ModuleKind::System
            && (module_kind < ModuleKind::ES2015
                || node_as_source_file.maybe_implied_node_format() == Some(ModuleKind::CommonJS));
        if !create {
            let helpers = get_emit_helpers(&node.ref_(factory));
            if let Some(helpers) = helpers {
                for helper in helpers {
                    if !helper.ref_(factory).scoped() {
                        create = true;
                        break;
                    }
                }
            }
        }

        if create {
            let parse_node = maybe_get_original_node_full(
                Some(node),
                Some(|node: Option<Id<Node>>| is_source_file(&node.unwrap().ref_(factory))),
                factory,
            )
            .unwrap();
            let emit_node = get_or_create_emit_node(parse_node, factory);
            return Some(
                emit_node
                    .borrow_mut()
                    .external_helpers_module_name
                    .get_or_insert_with(|| {
                        factory.create_unique_name(external_helpers_module_name_text, None)
                    })
                    .clone(),
            );
        }
    }
    None
}

pub fn get_local_name_for_external_import(
    factory: &NodeFactory,
    node: Id<Node>, /*ImportDeclaration | ExportDeclaration | ImportEqualsDeclaration*/
    source_file: Id<Node>, /*SourceFile*/
) -> Option<Id<Node /*Identifier*/>> {
    let namespace_declaration = get_namespace_declaration_node(node, factory);
    if let Some(namespace_declaration) = namespace_declaration
        .filter(|_| !is_default_import(node, factory) && !is_export_namespace_as_default_declaration(node, factory))
    {
        let name = namespace_declaration.ref_(factory).as_named_declaration().name();
        return Some(if is_generated_identifier(&name.ref_(factory)) {
            name
        } else {
            factory.create_identifier(
                &get_source_text_of_node_from_source_file(source_file, name, None, factory)
                    .non_empty()
                    .unwrap_or_else(|| id_text(&name.ref_(factory)).to_owned().into()),
            )
        });
    }
    if node.ref_(factory).kind() == SyntaxKind::ImportDeclaration
        && node.ref_(factory).as_import_declaration().import_clause.is_some()
    {
        return Some(factory.get_generated_name_for_node(Some(node), None));
    }
    if node.ref_(factory).kind() == SyntaxKind::ExportDeclaration
        && node.ref_(factory).as_export_declaration().module_specifier.is_some()
    {
        return Some(factory.get_generated_name_for_node(Some(node), None));
    }
    None
}

pub fn get_external_module_name_literal(
    factory: &NodeFactory,
    import_node: Id<Node>, /*ImportDeclaration | ExportDeclaration | ImportEqualsDeclaration | ImportCall*/
    source_file: Id<Node>, /*SourceFile*/
    host: &dyn EmitHost,
    resolver: &dyn EmitResolver,
    compiler_options: &CompilerOptions,
) -> io::Result<Option<Id<Node>>> {
    let module_name = get_external_module_name(import_node, factory);
    if let Some(module_name) = module_name.filter(|module_name| is_string_literal(&module_name.ref_(factory))) {
        return Ok(Some(
            try_get_module_name_from_declaration(
                import_node,
                host,
                factory,
                resolver,
                compiler_options,
            )?
            .or_else(|| try_rename_external_module(factory, module_name, source_file))
            .unwrap_or_else(|| factory.clone_node(module_name)),
        ));
    }

    Ok(None)
}

fn try_rename_external_module(
    factory: &NodeFactory,
    module_name: Id<Node>, /*LiteralExpression*/
    source_file: Id<Node>, /*SourceFile*/
) -> Option<Id<Node>> {
    let rename = source_file
        .ref_(factory).as_source_file()
        .maybe_renamed_dependencies()
        .as_ref()
        .and_then(|source_file_renamed_dependencies| {
            source_file_renamed_dependencies
                .get(&*module_name.ref_(factory).as_literal_like_node().text())
                .cloned()
        });
    rename
        .non_empty()
        .map(|rename| factory.create_string_literal(rename, None, None))
}

pub fn try_get_module_name_from_file(
    factory: &NodeFactory,
    file: Option<Id<Node /*SourceFile*/>>,
    host: &dyn EmitHost,
    options: &CompilerOptions,
) -> Option<Id<Node /*StringLiteral*/>> {
    let file = file?;
    let file_ref = file.ref_(factory);
    let file_as_source_file = file_ref.as_source_file();
    if let Some(file_module_name) = file_as_source_file.maybe_module_name().clone().non_empty() {
        return Some(factory.create_string_literal(file_module_name, None, None));
    }
    if !file_as_source_file.is_declaration_file() && out_file(options).is_non_empty() {
        return Some(factory.create_string_literal(
            get_external_module_name_from_path(host, &file_as_source_file.file_name(), None),
            None,
            None,
        ));
    }
    None
}

fn try_get_module_name_from_declaration(
    declaration: Id<Node>, /*ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration | ImportCall*/
    host: &dyn EmitHost,
    factory: &NodeFactory,
    resolver: &dyn EmitResolver,
    compiler_options: &CompilerOptions,
) -> io::Result<Option<Id<Node>>> {
    Ok(try_get_module_name_from_file(
        factory,
        resolver.get_external_module_file_from_declaration(declaration)?,
        host,
        compiler_options,
    ))
}

pub fn get_initializer_of_binding_or_assignment_element(
    binding_element: Id<Node>, /*BindingOrAssignmentElement*/
    arena: &impl HasArena,
) -> Option<Id<Node /*Expression*/>> {
    if is_declaration_binding_element(&binding_element.ref_(arena)) {
        return binding_element.ref_(arena).as_has_initializer().maybe_initializer();
    }

    if is_property_assignment(&binding_element.ref_(arena)) {
        let initializer = binding_element.ref_(arena).as_property_assignment().initializer;
        return is_assignment_expression(initializer, Some(true), arena)
            .then(|| initializer.ref_(arena).as_binary_expression().right);
    }

    if is_shorthand_property_assignment(&binding_element.ref_(arena)) {
        return binding_element
            .ref_(arena).as_shorthand_property_assignment()
            .object_assignment_initializer;
    }

    if is_assignment_expression(binding_element, Some(true), arena) {
        return Some(binding_element.ref_(arena).as_binary_expression().right);
    }

    if is_spread_element(&binding_element.ref_(arena)) {
        return get_initializer_of_binding_or_assignment_element(
            binding_element.ref_(arena).as_spread_element().expression,
            arena,
        );
    }
    None
}

pub fn get_target_of_binding_or_assignment_element(
    binding_element: Id<Node>, /*BindingOrAssignmentElement*/
    arena: &impl HasArena,
) -> Option<Id<Node /*BindingOrAssignmentElementTarget*/>> {
    if is_declaration_binding_element(&binding_element.ref_(arena)) {
        return binding_element.ref_(arena).as_named_declaration().maybe_name();
    }

    if is_object_literal_element_like(&binding_element.ref_(arena)) {
        match binding_element.ref_(arena).kind() {
            SyntaxKind::PropertyAssignment => {
                return get_target_of_binding_or_assignment_element(
                    binding_element
                        .ref_(arena).as_property_assignment()
                        .maybe_initializer()
                        .unwrap(),
                    arena,
                );
            }

            SyntaxKind::ShorthandPropertyAssignment => {
                return binding_element
                    .ref_(arena).as_shorthand_property_assignment()
                    .maybe_name();
            }

            SyntaxKind::SpreadAssignment => {
                return get_target_of_binding_or_assignment_element(
                    binding_element.ref_(arena).as_spread_assignment().expression,
                    arena,
                );
            }
            _ => (),
        }

        return None;
    }

    if is_assignment_expression(binding_element, Some(true), arena) {
        return get_target_of_binding_or_assignment_element(
            binding_element.ref_(arena).as_binary_expression().left,
            arena,
        );
    }

    if is_spread_element(&binding_element.ref_(arena)) {
        return get_target_of_binding_or_assignment_element(
            binding_element.ref_(arena).as_spread_element().expression,
            arena,
        );
    }

    Some(binding_element)
}

pub fn get_elements_of_binding_or_assignment_pattern(
    name: Id<Node /*BindingOrAssignmentPattern*/>,
    arena: &impl HasArena,
) -> Vec<Id<Node /*BindingOrAssignmentElement*/>> {
    match name.kind() {
        SyntaxKind::ObjectBindingPattern
        | SyntaxKind::ArrayBindingPattern
        | SyntaxKind::ArrayLiteralExpression => name.ref_(arena).as_has_elements().elements().ref_(arena).to_vec(),
        SyntaxKind::ObjectLiteralExpression => {
            name.ref_(arena).as_object_literal_expression().properties.ref_(arena).to_vec()
        }
        _ => panic!("Unexpected kind"),
    }
}

pub fn get_rest_indicator_of_binding_or_assignment_element(
    binding_element: &Node, /*BindingOrAssignmentElement*/
) -> Option<Id<Node /*BindingOrAssignmentElementRestIndicator*/>> {
    match binding_element.kind() {
        SyntaxKind::Parameter | SyntaxKind::BindingElement => binding_element
            .as_has_dot_dot_dot_token()
            .maybe_dot_dot_dot_token(),
        SyntaxKind::SpreadElement | SyntaxKind::SpreadAssignment => {
            Some(binding_element.arena_id())
        }
        _ => None,
    }
}

pub fn get_property_name_of_binding_or_assignment_element(
    binding_element: Id<Node>, /*BindingOrAssignmentElement*/
    arena: &impl HasArena,
) -> Option<Id<Node /*Exclude<PropertyName, PrivateIdentifier>*/>> {
    let property_name = try_get_property_name_of_binding_or_assignment_element(binding_element, arena);
    Debug_.assert(
        property_name.is_some() || is_spread_assignment(&binding_element.ref_(arena)),
        Some("Invalid property name for binding element."),
    );
    property_name
}

pub fn try_get_property_name_of_binding_or_assignment_element(
    binding_element: Id<Node>, /*BindingOrAssignmentElement*/
    arena: &impl HasArena,
) -> Option<Id<Node /*Exclude<PropertyName, PrivateIdentifier>*/>> {
    match binding_element.ref_(arena).kind() {
        SyntaxKind::BindingElement => {
            if let Some(binding_element_property_name) =
                binding_element.ref_(arena).as_binding_element().property_name
            {
                let property_name = binding_element_property_name;
                if is_private_identifier(&property_name.ref_(arena)) {
                    Debug_.fail_bad_syntax_kind(&property_name.ref_(arena), None);
                }
                return Some(
                    if is_computed_property_name(&property_name.ref_(arena))
                        && is_string_or_numeric_literal(
                            &property_name.ref_(arena).as_computed_property_name().expression.ref_(arena),
                        )
                    {
                        property_name.ref_(arena).as_computed_property_name().expression
                    } else {
                        property_name
                    },
                );
            }
        }
        SyntaxKind::PropertyAssignment => {
            if let Some(binding_element_name) =
                binding_element.ref_(arena).as_property_assignment().maybe_name()
            {
                let property_name = binding_element_name;
                if is_private_identifier(&property_name.ref_(arena)) {
                    Debug_.fail_bad_syntax_kind(&property_name.ref_(arena), None);
                }
                return Some(
                    if is_computed_property_name(&property_name.ref_(arena))
                        && is_string_or_numeric_literal(
                            &property_name.ref_(arena).as_computed_property_name().expression.ref_(arena),
                        )
                    {
                        property_name.ref_(arena).as_computed_property_name().expression
                    } else {
                        property_name
                    },
                );
            }
        }
        SyntaxKind::SpreadAssignment => {
            let binding_element_ref = binding_element.ref_(arena);
            let binding_element_as_spread_assignment = binding_element_ref.as_spread_assignment();
            if let Some(binding_element_name) = binding_element_as_spread_assignment
                .maybe_name()
                .filter(|binding_element_name| is_private_identifier(&binding_element_name.ref_(arena)))
            {
                Debug_.fail_bad_syntax_kind(&binding_element_name.ref_(arena), None);
            }
            return binding_element_as_spread_assignment.maybe_name();
        }
        _ => (),
    }

    let target = get_target_of_binding_or_assignment_element(binding_element, arena);
    if target.matches(|target| is_property_name(&target.ref_(arena))) {
        return target;
    }
    None
}

fn is_string_or_numeric_literal(node: &Node) -> bool {
    let kind = node.kind();
    matches!(kind, SyntaxKind::StringLiteral | SyntaxKind::NumericLiteral)
}

pub(crate) fn get_jsdoc_type_alias_name(
    full_name: Option<Id<Node> /*JSDocNamespaceBody*/>,
    arena: &impl HasArena,
) -> Option<Id<Node /*Identifier*/>> {
    full_name.map(|full_name| {
        let mut right_node = full_name;
        loop {
            if is_identifier(&right_node.ref_(arena)) || right_node.ref_(arena).as_module_declaration().body.is_none() {
                return if is_identifier(&right_node.ref_(arena)) {
                    right_node
                } else {
                    right_node.ref_(arena).as_module_declaration().name
                };
            }
            right_node = right_node.ref_(arena).as_module_declaration().body.unwrap();
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

fn is_exponentiation_operator(kind: SyntaxKind) -> bool {
    kind == SyntaxKind::AsteriskAsteriskToken
}

fn is_multiplicative_operator(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::AsteriskToken | SyntaxKind::SlashToken | SyntaxKind::PercentToken
    )
}

fn is_multiplicative_operator_or_higher(kind: SyntaxKind) -> bool {
    is_exponentiation_operator(kind) || is_multiplicative_operator(kind)
}

fn is_additive_operator(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::PlusToken | SyntaxKind::MinusToken)
}

fn is_additive_operator_or_higher(kind: SyntaxKind) -> bool {
    is_additive_operator(kind) || is_multiplicative_operator_or_higher(kind)
}

fn is_shift_operator(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::LessThanLessThanToken
            | SyntaxKind::GreaterThanGreaterThanToken
            | SyntaxKind::GreaterThanGreaterThanGreaterThanToken
    )
}

fn is_shift_operator_or_higher(kind: SyntaxKind) -> bool {
    is_shift_operator(kind) || is_additive_operator_or_higher(kind)
}

fn is_relational_operator(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::LessThanToken
            | SyntaxKind::LessThanEqualsToken
            | SyntaxKind::GreaterThanToken
            | SyntaxKind::GreaterThanEqualsToken
            | SyntaxKind::InstanceOfKeyword
            | SyntaxKind::InKeyword
    )
}

fn is_relational_operator_or_higher(kind: SyntaxKind) -> bool {
    is_relational_operator(kind) || is_shift_operator_or_higher(kind)
}

fn is_equality_operator(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::EqualsEqualsToken
            | SyntaxKind::EqualsEqualsEqualsToken
            | SyntaxKind::ExclamationEqualsToken
            | SyntaxKind::ExclamationEqualsEqualsToken
    )
}

fn is_equality_operator_or_higher(kind: SyntaxKind) -> bool {
    is_equality_operator(kind) || is_relational_operator_or_higher(kind)
}

fn is_bitwise_operator(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::AmpersandToken | SyntaxKind::BarToken | SyntaxKind::CaretToken
    )
}

fn is_bitwise_operator_or_higher(kind: SyntaxKind) -> bool {
    is_bitwise_operator(kind) || is_equality_operator_or_higher(kind)
}

fn is_logical_operator(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::AmpersandAmpersandToken | SyntaxKind::BarBarToken
    )
}

fn is_logical_operator_or_higher(kind: SyntaxKind) -> bool {
    is_logical_operator(kind) || is_bitwise_operator_or_higher(kind)
}

fn is_assignment_operator_or_higher(kind: SyntaxKind) -> bool {
    kind == SyntaxKind::QuestionQuestionToken
        || is_logical_operator_or_higher(kind)
        || is_assignment_operator(kind)
}

fn is_binary_operator(kind: SyntaxKind) -> bool {
    is_assignment_operator_or_higher(kind) || kind == SyntaxKind::CommaToken
}

pub fn is_binary_operator_token(node: &Node) -> bool {
    is_binary_operator(node.kind())
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
    node_stack: &mut Vec<Id<Node /*BinaryExpression*/>>,
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
    node_stack: &mut Vec<Id<Node /*BinaryExpression*/>>,
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
        Some(machine.on_enter(node_stack[stack_index], prev_user_state, outer_state)?),
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
    node_stack: &mut Vec<Id<Node /*BinaryExpression*/>>,
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
        node_stack[stack_index].ref_(machine).as_binary_expression().left,
        user_state_stack[stack_index].clone().unwrap(),
        node_stack[stack_index],
    )?;
    if let Some(next_node) = next_node {
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
    node_stack: &mut Vec<Id<Node /*BinaryExpression*/>>,
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
        node_stack[stack_index]
            .ref_(machine).as_binary_expression()
            .operator_token,
        user_state_stack[stack_index].clone().unwrap(),
        node_stack[stack_index],
    )?;
    Ok(stack_index)
}

fn binary_expression_state_right<TMachine: BinaryExpressionStateMachine>(
    machine: &TMachine,
    stack_index: usize,
    state_stack: &mut Vec<BinaryExpressionState>,
    node_stack: &mut Vec<Id<Node /*BinaryExpression*/>>,
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
        node_stack[stack_index].ref_(machine).as_binary_expression().right,
        user_state_stack[stack_index].clone().unwrap(),
        node_stack[stack_index],
    )?;
    if let Some(next_node) = next_node {
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
    node_stack: &mut Vec<Id<Node /*BinaryExpression*/>>,
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
        node_stack[stack_index],
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
    _node_stack: &mut Vec<Id<Node /*BinaryExpression*/>>,
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
    node_stack: &mut Vec<Id<Node /*BinaryExpression*/>>,
    user_state_stack: &mut Vec<Option<TState>>,
    node: Id<Node>, /*BinaryExpression*/
) -> usize {
    stack_index += 1;
    push_or_replace(state_stack, stack_index, BinaryExpressionState::Enter);
    push_or_replace(node_stack, stack_index, node);
    push_or_replace(user_state_stack, stack_index, None);
    stack_index
}

fn binary_expression_state_check_circularity(
    mut stack_index: usize,
    node_stack: &[Id<Node /*BinaryExpression*/>],
    node: Id<Node>, /*BinaryExpression*/
) {
    if Debug_.should_assert(AssertionLevel::Aggressive) {
        loop
        /*while stackIndex >= 0*/
        {
            Debug_.assert(
                node_stack[stack_index] != node,
                Some("Circular traversal detected."),
            );
            if stack_index == 0 {
                break;
            }
            stack_index -= 1;
        }
    }
}

pub trait BinaryExpressionStateMachine: Trace + Finalize + HasArena {
    type TResult: Clone;
    type TOuterState: Clone;
    type TState: Clone;

    fn on_enter(
        &self,
        node: Id<Node>, /*BinaryExpression*/
        prev: Option<Self::TState>,
        outer_state: Self::TOuterState,
    ) -> io::Result<Self::TState>;

    fn on_left(
        &self,
        _left: Id<Node>, /*Expression*/
        _user_state: Self::TState,
        _node: Id<Node>, /*BinaryExpression*/
    ) -> io::Result<Option<Id<Node /*BinaryExpression*/>>> {
        panic!("Shouldn't call default on_left()")
    }

    fn on_operator(
        &self,
        _operator_token: Id<Node>, /*BinaryOperatorToken*/
        _user_state: Self::TState,
        _node: Id<Node>, /*BinaryExpression*/
    ) -> io::Result<()> {
        panic!("Shouldn't call default on_operator()")
    }

    fn on_right(
        &self,
        _right: Id<Node>, /*Expression*/
        _user_state: Self::TState,
        _node: Id<Node>, /*BinaryExpression*/
    ) -> io::Result<Option<Id<Node /*BinaryExpression*/>>> {
        panic!("Shouldn't call default on_left()")
    }

    fn on_exit(
        &self,
        node: Id<Node>, /*BinaryExpression*/
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
        node: Id<Node>, /*BinaryExpression*/
        outer_state: TMachine::TOuterState,
    ) -> io::Result<TMachine::TResult> {
        let result_holder: Rc<RefCell<Option<TMachine::TResult>>> = Rc::new(RefCell::new(None));
        let mut state_stack: Vec<BinaryExpressionState> = vec![BinaryExpressionState::Enter];
        let mut node_stack: Vec<Id<Node /*BinaryExpression*/>> = vec![node];
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
