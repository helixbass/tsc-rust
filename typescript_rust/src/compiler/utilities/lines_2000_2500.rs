use std::{borrow::Borrow, ops::Deref};

use id_arena::Id;

use crate::{
    get_jsdoc_type_tag, get_text_of_identifier_or_literal, is_effective_module_declaration,
    is_external_module_reference, is_function_like, is_private_identifier, is_property_assignment,
    is_property_name_literal, skip_parentheses, try_cast, try_get_import_from_module_specifier,
    HasTypeArgumentsInterface, NamedDeclarationInterface, NodeFlags, NodeInterface, Symbol,
    SymbolInterface, SyntaxKind, __String, escape_leading_underscores, every, for_each,
    get_leftmost_access_expression, get_source_text_of_node_from_source_file, id_text,
    is_access_expression, is_assignment_expression, is_binary_expression, is_call_expression,
    is_dynamic_name, is_element_access_expression, is_entity_name_expression, is_identifier,
    is_json_source_file, is_namespace_export, is_numeric_literal, is_object_literal_expression,
    is_property_access_expression, is_prototype_access, is_string_literal_like,
    is_string_or_numeric_literal_like, is_type_reference_node, is_variable_declaration,
    is_variable_statement, is_void_expression, length, AssignmentDeclarationKind, CharacterCodes,
    Debug_, HasArena, HasInitializerInterface, InArena, LiteralLikeNodeInterface, Node,
};

pub fn is_part_of_type_query(mut node: Id<Node>, arena: &impl HasArena) -> bool {
    while matches!(
        node.ref_(arena).kind(),
        SyntaxKind::QualifiedName | SyntaxKind::Identifier
    ) {
        node = node.ref_(arena).parent();
    }
    node.ref_(arena).kind() == SyntaxKind::TypeQuery
}

pub fn is_namespace_reexport_declaration(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_namespace_export(&node.ref_(arena))
        && node
            .ref_(arena)
            .parent()
            .ref_(arena)
            .as_export_declaration()
            .module_specifier
            .is_some()
}

pub fn is_external_module_import_equals_declaration(node: Id<Node>, arena: &impl HasArena) -> bool {
    node.ref_(arena).kind() == SyntaxKind::ImportEqualsDeclaration
        && node
            .ref_(arena)
            .as_import_equals_declaration()
            .module_reference
            .ref_(arena)
            .kind()
            == SyntaxKind::ExternalModuleReference
}

pub fn get_external_module_import_equals_declaration_expression(
    node: Id<Node>,
    arena: &impl HasArena,
) -> Id<Node /*Expression*/> {
    Debug_.assert(
        is_external_module_import_equals_declaration(node, arena),
        None,
    );
    node.ref_(arena)
        .as_import_equals_declaration()
        .module_reference
        .ref_(arena)
        .as_external_module_reference()
        .expression
}

pub fn get_external_module_require_argument(
    node: Id<Node>,
    arena: &impl HasArena,
) -> Option<Id<Node /*StringLiteral*/>> {
    if is_require_variable_declaration(node, arena) {
        Some(
            get_leftmost_access_expression(
                node.ref_(arena)
                    .as_variable_declaration()
                    .maybe_initializer()
                    .unwrap(),
                arena,
            )
            .ref_(arena)
            .as_call_expression()
            .arguments
            .ref_(arena)[0],
        )
    } else {
        None
    }
}

pub fn is_internal_module_import_equals_declaration(node: Id<Node>, arena: &impl HasArena) -> bool {
    node.ref_(arena).kind() == SyntaxKind::ImportEqualsDeclaration
        && node
            .ref_(arena)
            .as_import_equals_declaration()
            .module_reference
            .ref_(arena)
            .kind()
            != SyntaxKind::ExternalModuleReference
}

pub fn is_source_file_js(file: &Node /*SourceFile*/) -> bool {
    is_in_js_file(Some(file))
}

pub fn is_source_file_not_js(file: &Node /*SourceFile*/) -> bool {
    !is_in_js_file(Some(file))
}

pub fn is_in_js_file(node: Option<&Node>) -> bool {
    node.map_or(false, |node| {
        node.flags().intersects(NodeFlags::JavaScriptFile)
    })
}

pub fn is_in_json_file(node: Option<&Node>) -> bool {
    node.map_or(false, |node| node.flags().intersects(NodeFlags::JsonFile))
}

pub fn is_source_file_not_json(file: &Node /*SourceFile*/) -> bool {
    !is_json_source_file(file)
}

pub fn is_in_jsdoc(node: Option<&Node>) -> bool {
    node.map_or(false, |node| node.flags().intersects(NodeFlags::JSDoc))
}

pub fn is_jsdoc_index_signature(
    node: Id<Node>, /*TypeReferenceNode | ExpressionWithTypeArguments*/
    arena: &impl HasArena,
) -> bool {
    if !is_type_reference_node(&node.ref_(arena)) {
        return false;
    }
    let node_ref = node.ref_(arena);
    let node_as_type_reference_node = node_ref.as_type_reference_node();
    if !(is_identifier(&node_as_type_reference_node.type_name.ref_(arena))
        && node_as_type_reference_node
            .type_name
            .ref_(arena)
            .as_identifier()
            .escaped_text
            == "Object"
        && node_as_type_reference_node.maybe_type_arguments().is_some())
    {
        return false;
    }
    let node_type_arguments = node_as_type_reference_node.maybe_type_arguments();
    let node_type_arguments = node_type_arguments.as_ref().unwrap();
    node_type_arguments.ref_(arena).len() == 2
        && matches!(
            node_type_arguments.ref_(arena)[0].ref_(arena).kind(),
            SyntaxKind::StringKeyword | SyntaxKind::NumberKeyword
        )
}

pub fn is_require_call(
    call_expression: Id<Node>,
    require_string_literal_like_argument: bool,
    arena: &impl HasArena,
) -> bool {
    if call_expression.ref_(arena).kind() != SyntaxKind::CallExpression {
        return false;
    }
    let call_expression_ref = call_expression.ref_(arena);
    let call_expression_as_call_expression = call_expression_ref.as_call_expression();
    let expression = call_expression_as_call_expression.expression;
    let args = &call_expression_as_call_expression.arguments;

    if expression.ref_(arena).kind() != SyntaxKind::Identifier
        || expression.ref_(arena).as_identifier().escaped_text != "require"
    {
        return false;
    }

    if args.ref_(arena).len() != 1 {
        return false;
    }
    let arg = args.ref_(arena)[0];
    !require_string_literal_like_argument || is_string_literal_like(&arg.ref_(arena))
}

pub fn is_require_variable_declaration(mut node: Id<Node>, arena: &impl HasArena) -> bool {
    if node.ref_(arena).kind() == SyntaxKind::BindingElement {
        node = node.ref_(arena).parent().ref_(arena).parent();
    }
    if !is_variable_declaration(&node.ref_(arena)) {
        return false;
    }
    let node_ref = node.ref_(arena);
    let node_as_variable_declaration = node_ref.as_variable_declaration();
    let Some(node_initializer) = node_as_variable_declaration.maybe_initializer() else {
        return false;
    };
    is_require_call(
        get_leftmost_access_expression(node_initializer, arena),
        true,
        arena,
    )
}

pub fn is_require_variable_statement(node: Id<Node>, arena: &impl HasArena) -> bool {
    if !is_variable_statement(&node.ref_(arena)) {
        return false;
    }
    let node_ref = node.ref_(arena);
    let node_as_variable_statement = node_ref.as_variable_statement();
    let node_declaration_list_ref = node_as_variable_statement.declaration_list.ref_(arena);
    let node_declaration_list_as_variable_declaration_list =
        node_declaration_list_ref.as_variable_declaration_list();
    !node_declaration_list_as_variable_declaration_list
        .declarations
        .ref_(arena)
        .is_empty()
        && every(
            &node_declaration_list_as_variable_declaration_list
                .declarations
                .ref_(arena),
            |&decl, _| is_require_variable_declaration(decl, arena),
        )
}

pub fn is_single_or_double_quote(char_code: char) -> bool {
    matches!(
        char_code,
        CharacterCodes::single_quote | CharacterCodes::double_quote
    )
}

pub fn is_string_double_quoted(
    str: Id<Node>,         /*StringLiteralLike*/
    source_file: Id<Node>, /*SourceFile*/
    arena: &impl HasArena,
) -> bool {
    get_source_text_of_node_from_source_file(source_file, str, None, arena).starts_with("\"")
}

pub fn is_assignment_declaration(decl: &Node /*Declaration*/) -> bool {
    is_binary_expression(decl)
        || is_access_expression(decl)
        || is_identifier(decl)
        || is_call_expression(decl)
}

pub fn get_effective_initializer(
    node: Id<Node>, /*HasExpressionInitializer*/
    arena: &impl HasArena,
) -> Option<Id<Node>> {
    let node_initializer = node.ref_(arena).as_has_initializer().maybe_initializer();
    if is_in_js_file(Some(&node.ref_(arena))) {
        if let Some(node_initializer) = node_initializer {
            if is_binary_expression(&node_initializer.ref_(arena)) {
                let node_initializer_ref = node_initializer.ref_(arena);
                let node_initializer_as_binary_expression =
                    node_initializer_ref.as_binary_expression();
                if matches!(
                    node_initializer_as_binary_expression
                        .operator_token
                        .ref_(arena)
                        .kind(),
                    SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken
                ) {
                    if let Some(node_name) = node.ref_(arena).as_named_declaration().maybe_name() {
                        if is_entity_name_expression(node_name, arena)
                            && is_same_entity_name(
                                node_name,
                                node_initializer_as_binary_expression.left,
                                arena,
                            )
                        {
                            return Some(node_initializer_as_binary_expression.right);
                        }
                    }
                }
            }
        }
    }
    node_initializer
}

pub fn get_declared_expando_initializer(
    node: Id<Node>, /*HasExpressionInitializer*/
    arena: &impl HasArena,
) -> Option<Id<Node /*Expression*/>> {
    let init = get_effective_initializer(node, arena);
    init.and_then(|init| {
        get_expando_initializer(
            init,
            is_prototype_access(node.ref_(arena).as_named_declaration().name(), arena),
            arena,
        )
    })
}

pub fn has_expando_value_property(
    node: Id<Node>, /*ObjectLiteralExpression*/
    is_prototype_assignment: bool,
    arena: &impl HasArena,
) -> Option<Id<Node /*Expression*/>> {
    for_each(
        &*node
            .ref_(arena)
            .as_object_literal_expression()
            .properties
            .ref_(arena),
        |&p: &Id<Node>, _| {
            if !is_property_assignment(&p.ref_(arena)) {
                return None;
            }
            let p_ref = p.ref_(arena);
            let p_as_property_assignment = p_ref.as_property_assignment();
            if !is_identifier(&p_as_property_assignment.name().ref_(arena))
                && p_as_property_assignment
                    .name()
                    .ref_(arena)
                    .as_identifier()
                    .escaped_text
                    == "value"
            {
                return None;
            }
            let p_initializer = p_as_property_assignment.maybe_initializer()?;
            get_expando_initializer(p_initializer, is_prototype_assignment, arena)
        },
    )
}

pub fn get_assigned_expando_initializer(
    node: Option<Id<Node>>,
    arena: &impl HasArena,
) -> Option<Id<Node /*Expression*/>> {
    let node = node?;
    let node_parent = node.ref_(arena).maybe_parent();
    if let Some(node_parent) = node_parent {
        if is_binary_expression(&node_parent.ref_(arena)) {
            let node_parent_ref = node_parent.ref_(arena);
            let node_parent_as_binary_expression = node_parent_ref.as_binary_expression();
            if node_parent_as_binary_expression
                .operator_token
                .ref_(arena)
                .kind()
                == SyntaxKind::EqualsToken
            {
                let is_prototype_assignment =
                    is_prototype_access(node_parent_as_binary_expression.left, arena);
                return get_expando_initializer(
                    node_parent_as_binary_expression.right,
                    is_prototype_assignment,
                    arena,
                )
                .or_else(|| {
                    get_defaulted_expando_initializer(
                        node_parent_as_binary_expression.left,
                        node_parent_as_binary_expression.right,
                        is_prototype_assignment,
                        arena,
                    )
                });
            }
        }
    }
    if is_call_expression(&node.ref_(arena)) && is_bindable_object_define_property_call(node, arena)
    {
        let node_ref = node.ref_(arena);
        let node_as_call_expression = node_ref.as_call_expression();
        let result = has_expando_value_property(
            node_as_call_expression.arguments.ref_(arena)[2],
            &*node_as_call_expression.arguments.ref_(arena)[1]
                .ref_(arena)
                .as_literal_like_node()
                .text()
                == "prototype",
            arena,
        );
        if result.is_some() {
            return result;
        }
    }
    None
}

pub fn get_expando_initializer(
    initializer: Id<Node>,
    is_prototype_assignment: bool,
    arena: &impl HasArena,
) -> Option<Id<Node /*Expression*/>> {
    if is_call_expression(&initializer.ref_(arena)) {
        let e = skip_parentheses(
            initializer.ref_(arena).as_call_expression().expression,
            None,
            arena,
        );
        return if matches!(
            e.ref_(arena).kind(),
            SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction
        ) {
            Some(initializer)
        } else {
            None
        };
    }
    if matches!(
        initializer.ref_(arena).kind(),
        SyntaxKind::FunctionExpression | SyntaxKind::ClassExpression | SyntaxKind::ArrowFunction
    ) {
        return Some(initializer);
    }
    if is_object_literal_expression(&initializer.ref_(arena))
        && (initializer
            .ref_(arena)
            .as_object_literal_expression()
            .properties
            .ref_(arena)
            .is_empty()
            || is_prototype_assignment)
    {
        return Some(initializer);
    }
    None
}

pub fn get_defaulted_expando_initializer(
    name: Id<Node>,        /*Expression*/
    initializer: Id<Node>, /*Expression*/
    is_prototype_assignment: bool,
    arena: &impl HasArena,
) -> Option<Id<Node /*Expression*/>> {
    let e: Option<Id<Node>> = if is_binary_expression(&initializer.ref_(arena)) {
        let initializer_ref = initializer.ref_(arena);
        let initializer_as_binary_expression = initializer_ref.as_binary_expression();
        if matches!(
            initializer_as_binary_expression
                .operator_token
                .ref_(arena)
                .kind(),
            SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken
        ) {
            get_expando_initializer(
                initializer_as_binary_expression.right,
                is_prototype_assignment,
                arena,
            )
        } else {
            None
        }
    } else {
        None
    };
    e.filter(|_| {
        is_same_entity_name(
            name,
            initializer.ref_(arena).as_binary_expression().left,
            arena,
        )
    })
}

pub fn is_defaulted_expando_initializer(
    node: Id<Node>, /*BinaryExpression*/
    arena: &impl HasArena,
) -> bool {
    let node_parent = node.ref_(arena).parent();
    let Some(name) = (if is_variable_declaration(&node_parent.ref_(arena)) {
        Some(node_parent.ref_(arena).as_variable_declaration().name())
    } else if is_binary_expression(&node_parent.ref_(arena))
        && node_parent
            .ref_(arena)
            .as_binary_expression()
            .operator_token
            .ref_(arena)
            .kind()
            == SyntaxKind::EqualsToken
    {
        Some(node_parent.ref_(arena).as_binary_expression().left)
    } else {
        None
    }) else {
        return false;
    };
    let node_ref = node.ref_(arena);
    let node_as_binary_expression = node_ref.as_binary_expression();
    get_expando_initializer(
        node_as_binary_expression.right,
        is_prototype_access(name, arena),
        arena,
    )
    .is_some()
        && is_entity_name_expression(name, arena)
        && is_same_entity_name(name, node_as_binary_expression.left, arena)
}

pub fn get_name_of_expando(
    node: Id<Node>, /*Declaration*/
    arena: &impl HasArena,
) -> Option<Id<Node /*DeclarationName*/>> {
    let node_parent = node.ref_(arena).parent();
    if is_binary_expression(&node_parent.ref_(arena)) {
        let parent = if matches!(
            node_parent
                .ref_(arena)
                .as_binary_expression()
                .operator_token
                .ref_(arena)
                .kind(),
            SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken
        ) && is_binary_expression(&node_parent.ref_(arena).parent().ref_(arena))
        {
            node_parent.ref_(arena).parent()
        } else {
            node_parent
        };
        let parent_ref = parent.ref_(arena);
        let parent_as_binary_expression = parent_ref.as_binary_expression();
        if parent_as_binary_expression
            .operator_token
            .ref_(arena)
            .kind()
            == SyntaxKind::EqualsToken
            && is_identifier(&parent_as_binary_expression.left.ref_(arena))
        {
            return Some(parent_as_binary_expression.left);
        }
    } else if is_variable_declaration(&node_parent.ref_(arena)) {
        return Some(node_parent.ref_(arena).as_variable_declaration().name());
    }
    None
}

pub fn is_same_entity_name(
    name: Id<Node>,        /*Expression*/
    initializer: Id<Node>, /*Expression*/
    arena: &impl HasArena,
) -> bool {
    if is_property_name_literal(&name.ref_(arena))
        && is_property_name_literal(&initializer.ref_(arena))
    {
        return get_text_of_identifier_or_literal(&name.ref_(arena))
            == get_text_of_identifier_or_literal(&initializer.ref_(arena));
    }
    if is_identifier(&name.ref_(arena)) && is_literal_like_access(initializer, arena) {
        let initializer_ref = initializer.ref_(arena);
        let initializer_as_has_expression = initializer_ref.as_has_expression();
        if initializer_as_has_expression
            .expression()
            .ref_(arena)
            .kind()
            == SyntaxKind::ThisKeyword
            || is_identifier(&initializer_as_has_expression.expression().ref_(arena))
                && matches!(
                    initializer_as_has_expression
                        .expression()
                        .ref_(arena)
                        .as_identifier()
                        .escaped_text
                        .deref(),
                    "window" | "self" | "global"
                )
        {
            let name_or_argument = get_name_or_argument(&initializer.ref_(arena));
            if is_private_identifier(&name_or_argument.ref_(arena)) {
                Debug_.fail(Some(
                    "Unexpected PrivateIdentifier in name expression with literal-like access.",
                ));
            }
            return is_same_entity_name(name, name_or_argument, arena);
        }
    }
    if is_literal_like_access(name, arena) && is_literal_like_access(initializer, arena) {
        return get_element_or_property_access_name(name, arena)
            == get_element_or_property_access_name(initializer, arena)
            && is_same_entity_name(
                name.ref_(arena).as_has_expression().expression(),
                initializer.ref_(arena).as_has_expression().expression(),
                arena,
            );
    }
    false
}

pub fn get_right_most_assigned_expression(
    mut node: Id<Node>, /*Expression*/
    arena: &impl HasArena,
) -> Id<Node /*Expression*/> {
    while is_assignment_expression(node, Some(true), arena) {
        node = node.ref_(arena).as_binary_expression().right;
    }
    node
}

pub fn is_exports_identifier(node: &Node) -> bool {
    if !is_identifier(node) {
        return false;
    }
    node.as_identifier().escaped_text == "exports"
}

pub fn is_module_identifier(node: &Node) -> bool {
    if !is_identifier(node) {
        return false;
    }
    node.as_identifier().escaped_text == "module"
}

pub fn is_module_exports_access_expression(node: Id<Node>, arena: &impl HasArena) -> bool {
    if !(is_property_access_expression(&node.ref_(arena))
        || is_literal_like_element_access(node, arena))
    {
        return false;
    }
    is_module_identifier(
        &node
            .ref_(arena)
            .as_has_expression()
            .expression()
            .ref_(arena),
    ) && match get_element_or_property_access_name(node, arena) {
        Some(name) => name == "exports",
        None => false,
    }
}

pub fn get_assignment_declaration_kind(
    expr: Id<Node>, /*BinaryExpression | CallExpression*/
    arena: &impl HasArena,
) -> AssignmentDeclarationKind {
    let special = get_assignment_declaration_kind_worker(expr, arena);
    if special == AssignmentDeclarationKind::Property || is_in_js_file(Some(&expr.ref_(arena))) {
        special
    } else {
        AssignmentDeclarationKind::None
    }
}

pub fn is_bindable_object_define_property_call(
    expr: Id<Node>, /*CallExpression*/
    arena: &impl HasArena,
) -> bool {
    let expr_ref = expr.ref_(arena);
    let expr_as_call_expression = expr_ref.as_call_expression();
    if length(Some(&expr_as_call_expression.arguments.ref_(arena))) != 3 {
        return false;
    }
    let expr_arguments = expr_as_call_expression.arguments;
    if !is_property_access_expression(&expr_as_call_expression.expression.ref_(arena)) {
        return false;
    }
    let expr_expression_ref = expr_as_call_expression.expression.ref_(arena);
    let expr_expression_as_property_access_expression =
        expr_expression_ref.as_property_access_expression();
    if !is_identifier(
        &expr_expression_as_property_access_expression
            .expression
            .ref_(arena),
    ) {
        return false;
    }
    if id_text(
        &expr_expression_as_property_access_expression
            .expression
            .ref_(arena),
    ) != "Object"
    {
        return false;
    }
    if id_text(
        &expr_expression_as_property_access_expression
            .name
            .ref_(arena),
    ) != "defineProperty"
    {
        return false;
    }
    if !is_string_or_numeric_literal_like(&expr_arguments.ref_(arena)[1].ref_(arena)) {
        return false;
    }
    is_bindable_static_name_expression(expr_arguments.ref_(arena)[0], Some(true), arena)
}

pub fn is_literal_like_access(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_property_access_expression(&node.ref_(arena)) || is_literal_like_element_access(node, arena)
}

pub fn is_literal_like_element_access(node: Id<Node>, arena: &impl HasArena) -> bool {
    if !is_element_access_expression(&node.ref_(arena)) {
        return false;
    }
    let node_ref = node.ref_(arena);
    let node_as_element_access_expression = node_ref.as_element_access_expression();
    is_string_or_numeric_literal_like(
        &node_as_element_access_expression
            .argument_expression
            .ref_(arena),
    )
}

pub fn is_bindable_static_access_expression(
    node: Id<Node>,
    exclude_this_keyword: Option<bool>,
    arena: &impl HasArena,
) -> bool {
    let exclude_this_keyword_unwrapped = exclude_this_keyword.unwrap_or(false);
    if is_property_access_expression(&node.ref_(arena)) {
        let node_ref = node.ref_(arena);
        let node_as_property_access_expression = node_ref.as_property_access_expression();
        if !exclude_this_keyword_unwrapped
            && node_as_property_access_expression
                .expression
                .ref_(arena)
                .kind()
                == SyntaxKind::ThisKeyword
            || is_identifier(&node_as_property_access_expression.name.ref_(arena))
                && is_bindable_static_name_expression(
                    node_as_property_access_expression.expression,
                    Some(true),
                    arena,
                )
        {
            return true;
        }
    }
    is_bindable_static_element_access_expression(node, exclude_this_keyword, arena)
}

pub fn is_bindable_static_element_access_expression(
    node: Id<Node>,
    exclude_this_keyword: Option<bool>,
    arena: &impl HasArena,
) -> bool {
    let exclude_this_keyword = exclude_this_keyword.unwrap_or(false);
    if !is_literal_like_element_access(node, arena) {
        return false;
    }
    let node_ref = node.ref_(arena);
    let node_as_element_access_expression = node_ref.as_element_access_expression();
    !exclude_this_keyword
        && node_as_element_access_expression
            .expression
            .ref_(arena)
            .kind()
            == SyntaxKind::ThisKeyword
        || is_entity_name_expression(node_as_element_access_expression.expression, arena)
        || is_bindable_static_access_expression(
            node_as_element_access_expression.expression,
            Some(true),
            arena,
        )
}

pub fn is_bindable_static_name_expression(
    node: Id<Node>,
    exclude_this_keyword: Option<bool>,
    arena: &impl HasArena,
) -> bool {
    is_entity_name_expression(node, arena)
        || is_bindable_static_access_expression(node, exclude_this_keyword, arena)
}

pub fn get_name_or_argument(
    expr: &Node, /*PropertyAccessExpression | LiteralLikeElementAccessExpression*/
) -> Id<Node> {
    if is_property_access_expression(expr) {
        return expr.as_property_access_expression().name();
    }
    expr.as_element_access_expression().argument_expression
}

fn get_assignment_declaration_kind_worker(
    expr: Id<Node>, /*BinaryExpression | CallExpression*/
    arena: &impl HasArena,
) -> AssignmentDeclarationKind {
    if is_call_expression(&expr.ref_(arena)) {
        if !is_bindable_object_define_property_call(expr, arena) {
            return AssignmentDeclarationKind::None;
        }
        let expr_ref = expr.ref_(arena);
        let expr_arguments = &expr_ref.as_call_expression().arguments;
        let entity_name = expr_arguments.ref_(arena)[0];
        if is_exports_identifier(&entity_name.ref_(arena))
            || is_module_exports_access_expression(entity_name, arena)
        {
            return AssignmentDeclarationKind::ObjectDefinePropertyExports;
        }
        if is_bindable_static_access_expression(entity_name, None, arena)
            && match get_element_or_property_access_name(entity_name, arena) {
                Some(name) => name == "prototype",
                None => false,
            }
        {
            return AssignmentDeclarationKind::ObjectDefinePrototypeProperty;
        }
        return AssignmentDeclarationKind::ObjectDefinePropertyValue;
    }
    let expr_ref = expr.ref_(arena);
    let expr_as_binary_expression = expr_ref.as_binary_expression();
    if expr_as_binary_expression.operator_token.ref_(arena).kind() != SyntaxKind::EqualsToken
        || !is_access_expression(&expr_as_binary_expression.left.ref_(arena))
        || is_void_zero(get_right_most_assigned_expression(expr, arena), arena)
    {
        return AssignmentDeclarationKind::None;
    }
    let expr_left_ref = expr_as_binary_expression.left.ref_(arena);
    let expr_left_as_has_expression = expr_left_ref.as_has_expression();
    if is_bindable_static_name_expression(
        expr_left_as_has_expression.expression(),
        Some(true),
        arena,
    ) && match get_element_or_property_access_name(expr_as_binary_expression.left, arena) {
        Some(name) => name == "prototype",
        None => false,
    } && is_object_literal_expression(
        &get_initializer_of_binary_expression(expr, arena).ref_(arena),
    ) {
        return AssignmentDeclarationKind::Prototype;
    }
    get_assignment_declaration_property_access_kind(expr_as_binary_expression.left, arena)
}

fn is_void_zero(node: Id<Node>, arena: &impl HasArena) -> bool {
    if !is_void_expression(&node.ref_(arena)) {
        return false;
    }
    let node_ref = node.ref_(arena);
    let node_as_void_expression = node_ref.as_void_expression();
    if !is_numeric_literal(&node_as_void_expression.expression.ref_(arena)) {
        return false;
    }
    let node_expression_ref = node_as_void_expression.expression.ref_(arena);
    let node_expression_as_numeric_literal = node_expression_ref.as_numeric_literal();
    let ret = &*node_expression_as_numeric_literal.text() == "0";
    ret
}

pub(crate) fn get_element_or_property_access_argument_expression_or_name(
    node: Id<Node>, /*AccessExpression*/
    arena: &impl HasArena,
) -> Option<
    Id<
        Node, /*Identifier | PrivateIdentifier | StringLiteralLike | NumericLiteral | ElementAccessExpression*/
    >,
> {
    if is_property_access_expression(&node.ref_(arena)) {
        return Some(node.ref_(arena).as_property_access_expression().name);
    }
    let node_ref = node.ref_(arena);
    let node_as_element_access_expression = node_ref.as_element_access_expression();
    let arg = skip_parentheses(
        node_as_element_access_expression.argument_expression,
        None,
        arena,
    );
    if is_numeric_literal(&arg.ref_(arena)) || is_string_literal_like(&arg.ref_(arena)) {
        return Some(arg);
    }
    Some(node)
}

pub(crate) fn get_element_or_property_access_name(
    node: Id<Node>, /*AccessExpression*/
    arena: &impl HasArena,
) -> Option<__String> {
    let name = get_element_or_property_access_argument_expression_or_name(node, arena);
    name.and_then(|name| {
        if is_identifier(&name.ref_(arena)) {
            return Some(name.ref_(arena).as_identifier().escaped_text.clone());
        }
        if is_string_literal_like(&name.ref_(arena)) || is_numeric_literal(&name.ref_(arena)) {
            return Some(
                escape_leading_underscores(&name.ref_(arena).as_literal_like_node().text())
                    .into_owned(),
            );
        }
        None
    })
}

pub fn get_assignment_declaration_property_access_kind(
    lhs: Id<Node>, /*AccessExpression*/
    arena: &impl HasArena,
) -> AssignmentDeclarationKind {
    let lhs_ref = lhs.ref_(arena);
    let lhs_as_has_expression = lhs_ref.as_has_expression();
    if lhs_as_has_expression.expression().ref_(arena).kind() == SyntaxKind::ThisKeyword {
        return AssignmentDeclarationKind::ThisProperty;
    } else if is_module_exports_access_expression(lhs, arena) {
        return AssignmentDeclarationKind::ModuleExports;
    } else if is_bindable_static_name_expression(
        lhs_as_has_expression.expression(),
        Some(true),
        arena,
    ) {
        if is_prototype_access(lhs_as_has_expression.expression(), arena) {
            return AssignmentDeclarationKind::PrototypeProperty;
        }

        let mut next_to_last = lhs;
        while !is_identifier(
            &next_to_last
                .ref_(arena)
                .as_has_expression()
                .expression()
                .ref_(arena),
        ) {
            next_to_last = next_to_last.ref_(arena).as_has_expression().expression();
        }
        let id = next_to_last.ref_(arena).as_has_expression().expression();
        let id_ref = id.ref_(arena);
        let id_as_identifier = id_ref.as_identifier();
        if (id_as_identifier.escaped_text == "exports"
            || id_as_identifier.escaped_text == "module"
                && match get_element_or_property_access_name(next_to_last, arena) {
                    Some(name) => name == "exports",
                    None => false,
                })
            && is_bindable_static_access_expression(lhs, None, arena)
        {
            return AssignmentDeclarationKind::ExportsProperty;
        }
        if is_bindable_static_name_expression(lhs, Some(true), arena)
            || is_element_access_expression(&lhs.ref_(arena)) && is_dynamic_name(lhs, arena)
        {
            return AssignmentDeclarationKind::Property;
        }
    }
    AssignmentDeclarationKind::None
}

pub fn get_initializer_of_binary_expression(
    mut expr: Id<Node>, /*BinaryExpression*/
    arena: &impl HasArena,
) -> Id<Node /*Expression*/> {
    while is_binary_expression(&expr.ref_(arena).as_binary_expression().right.ref_(arena)) {
        expr = expr.ref_(arena).as_binary_expression().right;
    }
    expr.ref_(arena).as_binary_expression().right
}

pub fn is_prototype_property_assignment(node: Id<Node>, arena: &impl HasArena) -> bool {
    is_binary_expression(&node.ref_(arena))
        && get_assignment_declaration_kind(node, arena)
            == AssignmentDeclarationKind::PrototypeProperty
}

pub fn is_special_property_declaration(
    expr: Id<Node>, /*PropertyAccessExpression | ElementAccessExpression*/
    arena: &impl HasArena,
) -> bool {
    is_in_js_file(Some(&expr.ref_(arena)))
        && matches!(
            expr.ref_(arena).maybe_parent(),
            Some(parent) if parent.ref_(arena).kind() == SyntaxKind::ExpressionStatement
        )
        && (!is_element_access_expression(&expr.ref_(arena))
            || is_literal_like_element_access(expr, arena))
        && get_jsdoc_type_tag(expr.ref_(arena).parent(), arena).is_some()
}

pub fn set_value_declaration(symbol: Id<Symbol>, node: Id<Node>, arena: &impl HasArena) {
    match symbol.ref_(arena).maybe_value_declaration() {
        None => {
            symbol.ref_(arena).set_value_declaration(node);
        }
        Some(value_declaration)
            if !(node.ref_(arena).flags().intersects(NodeFlags::Ambient)
                && !value_declaration
                    .ref_(arena)
                    .flags()
                    .intersects(NodeFlags::Ambient))
                && (is_assignment_declaration(&value_declaration.ref_(arena))
                    && !is_assignment_declaration(&node.ref_(arena)))
                || (value_declaration.ref_(arena).kind() != node.ref_(arena).kind()
                    && is_effective_module_declaration(&value_declaration.ref_(arena))) =>
        {
            symbol.ref_(arena).set_value_declaration(node);
        }
        _ => (),
    }
}

pub fn is_function_symbol(symbol: Option<Id<Symbol>>, arena: &impl HasArena) -> bool {
    let Some(symbol) = symbol else {
        return false;
    };
    let Some(decl) = symbol.ref_(arena).maybe_value_declaration() else {
        return false;
    };
    if decl.ref_(arena).kind() == SyntaxKind::FunctionDeclaration {
        return true;
    }
    if !is_variable_declaration(&decl.ref_(arena)) {
        return false;
    }
    let Some(decl_initializer) = decl
        .ref_(arena)
        .as_variable_declaration()
        .maybe_initializer()
    else {
        return false;
    };
    is_function_like(Some(&decl_initializer.ref_(arena)))
}

pub fn try_get_module_specifier_from_declaration(
    node: Id<Node>, /*AnyImportOrRequire*/
    arena: &impl HasArena,
) -> Option<String> {
    match node.ref_(arena).kind() {
        SyntaxKind::VariableDeclaration => Some(
            node.ref_(arena)
                .as_variable_declaration()
                .maybe_initializer()
                .unwrap()
                .ref_(arena)
                .as_call_expression()
                .arguments
                .ref_(arena)[0]
                .ref_(arena)
                .as_literal_like_node()
                .text()
                .clone(),
        ),
        SyntaxKind::ImportDeclaration => try_cast(
            node.ref_(arena).as_import_declaration().module_specifier,
            |module_specifier| is_string_literal_like(&module_specifier.ref_(arena)),
        )
        .map(|module_specifier| {
            module_specifier
                .ref_(arena)
                .as_literal_like_node()
                .text()
                .clone()
        }),
        SyntaxKind::ImportEqualsDeclaration => try_cast(
            node.ref_(arena)
                .as_import_equals_declaration()
                .module_reference,
            |module_reference| is_external_module_reference(&module_reference.ref_(arena)),
        )
        .map(|module_reference| {
            module_reference
                .ref_(arena)
                .as_external_module_reference()
                .expression
        })
        .and_then(|expression| {
            try_cast(expression, |expression| {
                is_string_literal_like(&expression.ref_(arena))
            })
        })
        .map(|expression| expression.ref_(arena).as_literal_like_node().text().clone()),
        _ => Debug_.assert_never(node, None),
    }
}

pub fn import_from_module_specifier(
    node: Id<Node>, /*StringLiteralLike*/
    arena: &impl HasArena,
) -> Id<Node /*AnyValidImportOrReExport*/> {
    try_get_import_from_module_specifier(node, arena).unwrap_or_else(|| {
        Debug_.fail_bad_syntax_kind(&node.ref_(arena).parent().ref_(arena), None)
    })
}
