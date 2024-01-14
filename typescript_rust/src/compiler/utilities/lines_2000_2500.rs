use std::{borrow::Borrow, ops::Deref};

use gc::Gc;
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
        node.kind(),
        SyntaxKind::QualifiedName | SyntaxKind::Identifier
    ) {
        node = node.ref_(arena).parent();
    }
    node.kind() == SyntaxKind::TypeQuery
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
            .arguments[0],
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
    node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
) -> bool {
    if !is_type_reference_node(node) {
        return false;
    }
    let node_as_type_reference_node = node.as_type_reference_node();
    if !(is_identifier(&node_as_type_reference_node.type_name)
        && node_as_type_reference_node
            .type_name
            .as_identifier()
            .escaped_text
            == "Object"
        && node_as_type_reference_node.maybe_type_arguments().is_some())
    {
        return false;
    }
    let node_type_arguments = node_as_type_reference_node.maybe_type_arguments();
    let node_type_arguments = node_type_arguments.as_ref().unwrap();
    node_type_arguments.len() == 2
        && matches!(
            node_type_arguments[0].kind(),
            SyntaxKind::StringKeyword | SyntaxKind::NumberKeyword
        )
}

pub fn is_require_call(
    call_expression: Id<Node>,
    require_string_literal_like_argument: bool,
) -> bool {
    if call_expression.kind() != SyntaxKind::CallExpression {
        return false;
    }
    let call_expression_as_call_expression = call_expression.as_call_expression();
    let expression = &call_expression_as_call_expression.expression;
    let args = &call_expression_as_call_expression.arguments;

    if expression.kind() != SyntaxKind::Identifier
        || expression.as_identifier().escaped_text != "require"
    {
        return false;
    }

    if args.len() != 1 {
        return false;
    }
    let arg = &args[0];
    !require_string_literal_like_argument || is_string_literal_like(arg)
}

pub fn is_require_variable_declaration(mut node: Id<Node>, arena: &impl HasArena) -> bool {
    if node.kind() == SyntaxKind::BindingElement {
        node = node.parent().parent();
    }
    if !is_variable_declaration(&node) {
        return false;
    }
    let node_as_variable_declaration = node.as_variable_declaration();
    let node_initializer = node_as_variable_declaration.maybe_initializer();
    if node_initializer.is_none() {
        return false;
    }
    let node_initializer = node_initializer.unwrap();
    is_require_call(&get_leftmost_access_expression(&node_initializer), true)
}

pub fn is_require_variable_statement(node: Id<Node>) -> bool {
    if !is_variable_statement(node) {
        return false;
    }
    let node_as_variable_statement = node.as_variable_statement();
    let node_declaration_list_as_variable_declaration_list = node_as_variable_statement
        .declaration_list
        .as_variable_declaration_list();
    !node_declaration_list_as_variable_declaration_list
        .declarations
        .is_empty()
        && every(
            &node_declaration_list_as_variable_declaration_list.declarations,
            |decl, _| is_require_variable_declaration(decl),
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
) -> bool {
    get_source_text_of_node_from_source_file(source_file, str, None).starts_with("\"")
}

pub fn is_assignment_declaration(decl: &Node /*Declaration*/) -> bool {
    is_binary_expression(decl)
        || is_access_expression(decl)
        || is_identifier(decl)
        || is_call_expression(decl)
}

pub fn get_effective_initializer(node: Id<Node>, /*HasExpressionInitializer*/) -> Option<Id<Node>> {
    let node_initializer = node.as_has_initializer().maybe_initializer();
    if is_in_js_file(Some(node)) {
        if let Some(node_initializer) = node_initializer.as_ref() {
            if is_binary_expression(node_initializer) {
                let node_initializer_as_binary_expression = node_initializer.as_binary_expression();
                if matches!(
                    node_initializer_as_binary_expression.operator_token.kind(),
                    SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken
                ) {
                    if let Some(node_name) = node.as_named_declaration().maybe_name() {
                        if is_entity_name_expression(&node_name)
                            && is_same_entity_name(
                                &node_name,
                                &node_initializer_as_binary_expression.left,
                            )
                        {
                            return Some(node_initializer_as_binary_expression.right.clone());
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
) -> Option<Id<Node /*Expression*/>> {
    let init = get_effective_initializer(node);
    init.and_then(|init| {
        get_expando_initializer(
            &init,
            is_prototype_access(&node.as_named_declaration().name()),
        )
    })
}

pub fn has_expando_value_property(
    node: Id<Node>, /*ObjectLiteralExpression*/
    is_prototype_assignment: bool,
) -> Option<Id<Node /*Expression*/>> {
    for_each(&node.as_object_literal_expression().properties, |p, _| {
        if !is_property_assignment(p) {
            return None;
        }
        let p_as_property_assignment = p.as_property_assignment();
        if !is_identifier(&p_as_property_assignment.name())
            && p_as_property_assignment.name().as_identifier().escaped_text == "value"
        {
            return None;
        }
        let p_initializer = p_as_property_assignment.maybe_initializer();
        if p_initializer.is_none() {
            return None;
        }
        let p_initializer = p_initializer.unwrap();
        get_expando_initializer(&p_initializer, is_prototype_assignment)
    })
}

pub fn get_assigned_expando_initializer(
    node: Option<Id<Node>>,
    arena: &impl HasArena,
) -> Option<Id<Node /*Expression*/>> {
    let node = node?;
    let node_parent = node.ref_(arena).maybe_parent();
    if let Some(node_parent) = node_parent {
        if is_binary_expression(&node_parent) {
            let node_parent_as_binary_expression = node_parent.as_binary_expression();
            if node_parent_as_binary_expression.operator_token.kind() == SyntaxKind::EqualsToken {
                let is_prototype_assignment =
                    is_prototype_access(&node_parent_as_binary_expression.left);
                return get_expando_initializer(
                    &node_parent_as_binary_expression.right,
                    is_prototype_assignment,
                )
                .or_else(|| {
                    get_defaulted_expando_initializer(
                        &node_parent_as_binary_expression.left,
                        &node_parent_as_binary_expression.right,
                        is_prototype_assignment,
                    )
                });
            }
        }
    }
    if is_call_expression(node) && is_bindable_object_define_property_call(node) {
        let node_as_call_expression = node.as_call_expression();
        let result = has_expando_value_property(
            &node_as_call_expression.arguments[2],
            &*node_as_call_expression.arguments[1]
                .as_literal_like_node()
                .text()
                == "prototype",
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
) -> Option<Id<Node /*Expression*/>> {
    if is_call_expression(initializer) {
        let e = skip_parentheses(&initializer.as_call_expression().expression, None);
        return if matches!(
            e.kind(),
            SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction
        ) {
            Some(initializer.node_wrapper())
        } else {
            None
        };
    }
    if matches!(
        initializer.kind(),
        SyntaxKind::FunctionExpression | SyntaxKind::ClassExpression | SyntaxKind::ArrowFunction
    ) {
        return Some(initializer.node_wrapper());
    }
    if is_object_literal_expression(initializer)
        && (initializer
            .as_object_literal_expression()
            .properties
            .is_empty()
            || is_prototype_assignment)
    {
        return Some(initializer.node_wrapper());
    }
    None
}

pub fn get_defaulted_expando_initializer(
    name: Id<Node>,        /*Expression*/
    initializer: Id<Node>, /*Expression*/
    is_prototype_assignment: bool,
) -> Option<Id<Node /*Expression*/>> {
    let e: Option<Id<Node>> = if is_binary_expression(initializer) {
        let initializer_as_binary_expression = initializer.as_binary_expression();
        if matches!(
            initializer_as_binary_expression.operator_token.kind(),
            SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken
        ) {
            get_expando_initializer(
                &initializer_as_binary_expression.right,
                is_prototype_assignment,
            )
        } else {
            None
        }
    } else {
        None
    };
    e.filter(|_| is_same_entity_name(name, &initializer.as_binary_expression().left))
}

pub fn is_defaulted_expando_initializer(node: Id<Node> /*BinaryExpression*/) -> bool {
    let node_parent = node.parent();
    let name: Option<Id<Node>> = if is_variable_declaration(&node_parent) {
        Some(node_parent.as_variable_declaration().name())
    } else if is_binary_expression(&node_parent)
        && node_parent.as_binary_expression().operator_token.kind() == SyntaxKind::EqualsToken
    {
        Some(node_parent.as_binary_expression().left.clone())
    } else {
        None
    };
    if name.is_none() {
        return false;
    }
    let name = name.unwrap();
    let node_as_binary_expression = node.as_binary_expression();
    get_expando_initializer(&node_as_binary_expression.right, is_prototype_access(&name)).is_some()
        && is_entity_name_expression(&name)
        && is_same_entity_name(&name, &node_as_binary_expression.left)
}

pub fn get_name_of_expando(
    node: Id<Node>, /*Declaration*/
) -> Option<Id<Node /*DeclarationName*/>> {
    let node_parent = node.parent();
    if is_binary_expression(&node_parent) {
        let parent = if matches!(
            node_parent.as_binary_expression().operator_token.kind(),
            SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken
        ) && is_binary_expression(&node_parent.parent())
        {
            node_parent.parent()
        } else {
            node_parent
        };
        let parent_as_binary_expression = parent.as_binary_expression();
        if parent_as_binary_expression.operator_token.kind() == SyntaxKind::EqualsToken
            && is_identifier(&parent_as_binary_expression.left)
        {
            return Some(parent_as_binary_expression.left.clone());
        }
    } else if is_variable_declaration(&node_parent) {
        return Some(node_parent.as_variable_declaration().name());
    }
    None
}

pub fn is_same_entity_name(
    name: Id<Node>,        /*Expression*/
    initializer: Id<Node>, /*Expression*/
) -> bool {
    if is_property_name_literal(name) && is_property_name_literal(initializer) {
        return get_text_of_identifier_or_literal(name)
            == get_text_of_identifier_or_literal(initializer);
    }
    if is_identifier(name) && is_literal_like_access(initializer) {
        let initializer_as_has_expression = initializer.as_has_expression();
        if initializer_as_has_expression.expression().kind() == SyntaxKind::ThisKeyword
            || is_identifier(&initializer_as_has_expression.expression())
                && matches!(
                    initializer_as_has_expression
                        .expression()
                        .as_identifier()
                        .escaped_text
                        .deref(),
                    "window" | "self" | "global"
                )
        {
            let name_or_argument = get_name_or_argument(initializer);
            if is_private_identifier(&name_or_argument) {
                Debug_.fail(Some(
                    "Unexpected PrivateIdentifier in name expression with literal-like access.",
                ));
            }
            return is_same_entity_name(name, &name_or_argument);
        }
    }
    if is_literal_like_access(name) && is_literal_like_access(initializer) {
        return get_element_or_property_access_name(name)
            == get_element_or_property_access_name(initializer)
            && is_same_entity_name(
                &name.as_has_expression().expression(),
                &initializer.as_has_expression().expression(),
            );
    }
    false
}

pub fn get_right_most_assigned_expression(
    node: Id<Node>, /*Expression*/
) -> Id<Node /*Expression*/> {
    let mut node = node.node_wrapper();
    while is_assignment_expression(&node, Some(true)) {
        node = node.as_binary_expression().right.clone();
    }
    node
}

pub fn is_exports_identifier(node: Id<Node>) -> bool {
    if !is_identifier(node) {
        return false;
    }
    node.as_identifier().escaped_text == "exports"
}

pub fn is_module_identifier(node: Id<Node>) -> bool {
    if !is_identifier(node) {
        return false;
    }
    node.as_identifier().escaped_text == "module"
}

pub fn is_module_exports_access_expression(node: Id<Node>) -> bool {
    if !(is_property_access_expression(node) || is_literal_like_element_access(node)) {
        return false;
    }
    is_module_identifier(&node.as_has_expression().expression())
        && match get_element_or_property_access_name(node) {
            Some(name) => name == "exports",
            None => false,
        }
}

pub fn get_assignment_declaration_kind(
    expr: &Node, /*BinaryExpression | CallExpression*/
) -> AssignmentDeclarationKind {
    let special = get_assignment_declaration_kind_worker(expr);
    if special == AssignmentDeclarationKind::Property || is_in_js_file(Some(expr)) {
        special
    } else {
        AssignmentDeclarationKind::None
    }
}

pub fn is_bindable_object_define_property_call(expr: &Node /*CallExpression*/) -> bool {
    let expr = expr.as_call_expression();
    if !length(Some(&expr.arguments)) == 3 {
        return false;
    }
    let expr_arguments = &expr.arguments;
    if !is_property_access_expression(&expr.expression) {
        return false;
    }
    let expr_expression_as_property_access_expression =
        expr.expression.as_property_access_expression();
    if !is_identifier(&expr_expression_as_property_access_expression.expression) {
        return false;
    }
    if !(id_text(&expr_expression_as_property_access_expression.expression) == "Object") {
        return false;
    }
    if !(id_text(&expr_expression_as_property_access_expression.name) == "defineProperty") {
        return false;
    }
    if !is_string_or_numeric_literal_like(&expr_arguments[1]) {
        return false;
    }
    is_bindable_static_name_expression(&expr_arguments[0], Some(true))
}

pub fn is_literal_like_access(node: &Node) -> bool {
    is_property_access_expression(node) || is_literal_like_element_access(node)
}

pub fn is_literal_like_element_access(node: &Node) -> bool {
    if !is_element_access_expression(node) {
        return false;
    }
    let node_as_element_access_expression = node.as_element_access_expression();
    is_string_or_numeric_literal_like(&node_as_element_access_expression.argument_expression)
}

pub fn is_bindable_static_access_expression(
    node: &Node,
    exclude_this_keyword: Option<bool>,
) -> bool {
    let exclude_this_keyword_unwrapped = exclude_this_keyword.unwrap_or(false);
    if is_property_access_expression(node) {
        let node_as_property_access_expression = node.as_property_access_expression();
        if !exclude_this_keyword_unwrapped
            && node_as_property_access_expression.expression.kind() == SyntaxKind::ThisKeyword
            || is_identifier(&node_as_property_access_expression.name)
                && is_bindable_static_name_expression(
                    &node_as_property_access_expression.expression,
                    Some(true),
                )
        {
            return true;
        }
    }
    is_bindable_static_element_access_expression(node, exclude_this_keyword)
}

pub fn is_bindable_static_element_access_expression(
    node: &Node,
    exclude_this_keyword: Option<bool>,
) -> bool {
    let exclude_this_keyword = exclude_this_keyword.unwrap_or(false);
    if !is_literal_like_element_access(node) {
        return false;
    }
    let node_as_element_access_expression = node.as_element_access_expression();
    !exclude_this_keyword
        && node_as_element_access_expression.expression.kind() == SyntaxKind::ThisKeyword
        || is_entity_name_expression(&node_as_element_access_expression.expression)
        || is_bindable_static_access_expression(
            &node_as_element_access_expression.expression,
            Some(true),
        )
}

pub fn is_bindable_static_name_expression(node: &Node, exclude_this_keyword: Option<bool>) -> bool {
    is_entity_name_expression(node)
        || is_bindable_static_access_expression(node, exclude_this_keyword)
}

pub fn get_name_or_argument(
    expr: Id<Node>, /*PropertyAccessExpression | LiteralLikeElementAccessExpression*/
) -> Id<Node> {
    if is_property_access_expression(expr) {
        return expr.as_property_access_expression().name();
    }
    expr.as_element_access_expression()
        .argument_expression
        .clone()
}

fn get_assignment_declaration_kind_worker(
    expr: Id<Node>, /*BinaryExpression | CallExpression*/
) -> AssignmentDeclarationKind {
    if is_call_expression(expr) {
        if !is_bindable_object_define_property_call(expr) {
            return AssignmentDeclarationKind::None;
        }
        let expr_arguments = &expr.as_call_expression().arguments;
        let entity_name = &expr_arguments[0];
        if is_exports_identifier(entity_name) || is_module_exports_access_expression(entity_name) {
            return AssignmentDeclarationKind::ObjectDefinePropertyExports;
        }
        if is_bindable_static_access_expression(entity_name, None)
            && match get_element_or_property_access_name(entity_name) {
                Some(name) => name == "prototype",
                None => false,
            }
        {
            return AssignmentDeclarationKind::ObjectDefinePrototypeProperty;
        }
        return AssignmentDeclarationKind::ObjectDefinePropertyValue;
    }
    let expr_as_binary_expression = expr.as_binary_expression();
    if expr_as_binary_expression.operator_token.kind() != SyntaxKind::EqualsToken
        || !is_access_expression(&expr_as_binary_expression.left)
        || is_void_zero(&get_right_most_assigned_expression(expr))
    {
        return AssignmentDeclarationKind::None;
    }
    let expr_left_as_has_expression = expr_as_binary_expression.left.as_has_expression();
    if is_bindable_static_name_expression(&expr_left_as_has_expression.expression(), Some(true))
        && match get_element_or_property_access_name(&expr_as_binary_expression.left) {
            Some(name) => name == "prototype",
            None => false,
        }
        && is_object_literal_expression(&get_initializer_of_binary_expression(expr))
    {
        return AssignmentDeclarationKind::Prototype;
    }
    get_assignment_declaration_property_access_kind(&expr_as_binary_expression.left)
}

fn is_void_zero(node: &Node) -> bool {
    if !is_void_expression(node) {
        return false;
    }
    let node_as_void_expression = node.as_void_expression();
    if !is_numeric_literal(&node_as_void_expression.expression) {
        return false;
    }
    let node_expression_as_numeric_literal =
        node_as_void_expression.expression.as_numeric_literal();
    &*node_expression_as_numeric_literal.text() == "0"
}

pub(crate) fn get_element_or_property_access_argument_expression_or_name(
    node: Id<Node>, /*AccessExpression*/
) -> Option<
    Id<
        Node, /*Identifier | PrivateIdentifier | StringLiteralLike | NumericLiteral | ElementAccessExpression*/
    >,
> {
    if is_property_access_expression(node) {
        return Some(node.as_property_access_expression().name.clone());
    }
    let node_as_element_access_expression = node.as_element_access_expression();
    let arg = skip_parentheses(&node_as_element_access_expression.argument_expression, None);
    if is_numeric_literal(&arg) || is_string_literal_like(&arg) {
        return Some(arg);
    }
    Some(node.node_wrapper())
}

pub(crate) fn get_element_or_property_access_name(
    node: Id<Node>, /*AccessExpression*/
) -> Option<__String> {
    let name = get_element_or_property_access_argument_expression_or_name(node);
    name.and_then(|name| {
        if is_identifier(&name) {
            return Some(name.as_identifier().escaped_text.clone());
        }
        if is_string_literal_like(&name) || is_numeric_literal(&name) {
            return Some(
                escape_leading_underscores(&name.as_literal_like_node().text()).into_owned(),
            );
        }
        None
    })
}

pub fn get_assignment_declaration_property_access_kind(
    lhs: Id<Node>, /*AccessExpression*/
) -> AssignmentDeclarationKind {
    let lhs_as_has_expression = lhs.as_has_expression();
    if lhs_as_has_expression.expression().kind() == SyntaxKind::ThisKeyword {
        return AssignmentDeclarationKind::ThisProperty;
    } else if is_module_exports_access_expression(lhs) {
        return AssignmentDeclarationKind::ModuleExports;
    } else if is_bindable_static_name_expression(&lhs_as_has_expression.expression(), Some(true)) {
        if is_prototype_access(&lhs_as_has_expression.expression()) {
            return AssignmentDeclarationKind::PrototypeProperty;
        }

        let mut next_to_last = lhs.node_wrapper();
        while !is_identifier(&next_to_last.as_has_expression().expression()) {
            next_to_last = next_to_last.as_has_expression().expression();
        }
        let id = next_to_last.as_has_expression().expression();
        let id_as_identifier = id.as_identifier();
        if (id_as_identifier.escaped_text == "exports"
            || id_as_identifier.escaped_text == "module"
                && match get_element_or_property_access_name(&next_to_last) {
                    Some(name) => name == "exports",
                    None => false,
                })
            && is_bindable_static_access_expression(&lhs, None)
        {
            return AssignmentDeclarationKind::ExportsProperty;
        }
        if is_bindable_static_name_expression(&lhs, Some(true))
            || is_element_access_expression(&lhs) && is_dynamic_name(&lhs)
        {
            return AssignmentDeclarationKind::Property;
        }
    }
    AssignmentDeclarationKind::None
}

pub fn get_initializer_of_binary_expression(
    expr: Id<Node>, /*BinaryExpression*/
) -> Id<Node /*Expression*/> {
    let mut expr = expr.node_wrapper();
    while is_binary_expression(&expr.as_binary_expression().right) {
        expr = expr.as_binary_expression().right.clone();
    }
    expr.as_binary_expression().right.clone()
}

pub fn is_prototype_property_assignment(node: Id<Node>) -> bool {
    is_binary_expression(node)
        && get_assignment_declaration_kind(node) == AssignmentDeclarationKind::PrototypeProperty
}

pub fn is_special_property_declaration(
    expr: Id<Node>, /*PropertyAccessExpression | ElementAccessExpression*/
) -> bool {
    is_in_js_file(Some(expr))
        && matches!(expr.maybe_parent(), Some(parent) if parent.kind() == SyntaxKind::ExpressionStatement)
        && (!is_element_access_expression(expr) || is_literal_like_element_access(expr))
        && get_jsdoc_type_tag(&expr.parent()).is_some()
}

pub fn set_value_declaration(symbol: &Symbol, node: Id<Node>) {
    match symbol.maybe_value_declaration() {
        None => {
            symbol.set_value_declaration(node.node_wrapper());
        }
        Some(value_declaration)
            if !(node.flags().intersects(NodeFlags::Ambient)
                && !value_declaration.flags().intersects(NodeFlags::Ambient))
                && (is_assignment_declaration(&value_declaration)
                    && !is_assignment_declaration(node))
                || (value_declaration.kind() != node.kind()
                    && is_effective_module_declaration(&value_declaration)) =>
        {
            symbol.set_value_declaration(node.node_wrapper());
        }
        _ => (),
    }
}

pub fn is_function_symbol(symbol: Option<&Symbol>) -> bool {
    if symbol.is_none() {
        return false;
    }
    let symbol = symbol.unwrap();
    let value_declaration = symbol.maybe_value_declaration();
    if value_declaration.is_none() {
        return false;
    }
    let decl = value_declaration.unwrap();
    if decl.kind() == SyntaxKind::FunctionDeclaration {
        return true;
    }
    if !is_variable_declaration(&decl) {
        return false;
    }
    let decl_initializer = decl.as_variable_declaration().maybe_initializer();
    if decl_initializer.is_none() {
        return false;
    }
    let decl_initializer = decl_initializer.unwrap();
    is_function_like(Some(&*decl_initializer))
}

pub fn try_get_module_specifier_from_declaration(
    node: Id<Node>, /*AnyImportOrRequire*/
) -> Option<String> {
    match node.kind() {
        SyntaxKind::VariableDeclaration => Some(
            node.as_variable_declaration()
                .maybe_initializer()
                .unwrap()
                .as_call_expression()
                .arguments[0]
                .as_literal_like_node()
                .text()
                .clone(),
        ),
        SyntaxKind::ImportDeclaration => try_cast(
            &node.as_import_declaration().module_specifier,
            |module_specifier| is_string_literal_like(module_specifier),
        )
        .map(|module_specifier| module_specifier.as_literal_like_node().text().clone()),
        SyntaxKind::ImportEqualsDeclaration => try_cast(
            &node.as_import_equals_declaration().module_reference,
            |module_reference| is_external_module_reference(module_reference),
        )
        .map(|module_reference| &module_reference.as_external_module_reference().expression)
        .and_then(|expression| {
            try_cast(expression, |expression| is_string_literal_like(expression))
        })
        .map(|expression| expression.as_literal_like_node().text().clone()),
        _ => Debug_.assert_never(node, None),
    }
}

pub fn import_from_module_specifier(
    node: Id<Node>, /*StringLiteralLike*/
    arena: &impl HasArena,
) -> Id<Node /*AnyValidImportOrReExport*/> {
    try_get_import_from_module_specifier(node, arena)
        .unwrap_or_else(|| Debug_.fail_bad_syntax_kind(&node.parent(), None))
}
