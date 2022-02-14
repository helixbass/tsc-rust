#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use crate::{
    add_range, filter, first_or_undefined, get_jsdoc_parameter_tags,
    get_jsdoc_parameter_tags_no_cache, get_jsdoc_type_parameter_tags,
    get_jsdoc_type_parameter_tags_no_cache, has_initializer, has_jsdoc_nodes, id_text,
    is_access_expression, is_assignment_expression, is_binary_expression, is_call_expression,
    is_dynamic_name, is_element_access_expression, is_entity_name_expression,
    is_expression_statement, is_identifier, is_jsdoc, is_jsdoc_type_tag, is_module_declaration,
    is_numeric_literal, is_object_literal_expression, is_parenthesized_expression,
    is_property_access_expression, is_prototype_access, is_string_literal_like,
    is_string_or_numeric_literal_like, is_variable_like, is_variable_statement, is_void_expression,
    last, length, skip_outer_expressions, AssignmentDeclarationKind, LiteralLikeNodeInterface,
    Node, NodeFlags, NodeInterface, OuterExpressionKinds, Symbol, SymbolInterface, SyntaxKind,
    __String, escape_leading_underscores, is_type_alias_declaration,
};

pub fn is_in_js_file<TNode: Borrow<Node>>(node: Option<TNode>) -> bool {
    node.map_or(false, |node| {
        node.borrow().flags().intersects(NodeFlags::JavaScriptFile)
    })
}

pub fn is_in_jsdoc<TNode: Borrow<Node>>(node: Option<TNode>) -> bool {
    node.map_or(false, |node| {
        node.borrow().flags().intersects(NodeFlags::JSDoc)
    })
}

pub fn get_effective_initializer(node: &Node, /*HasExpressionInitializer*/) -> Option<Rc<Node>> {
    node.as_has_initializer().maybe_initializer()
}

pub fn get_right_most_assigned_expression(node: &Node, /*Expression*/) -> Rc<Node /*Expression*/> {
    let mut node = node.node_wrapper();
    while is_assignment_expression(&*node, Some(true)) {
        node = node.as_binary_expression().right.clone();
    }
    node
}

pub fn is_exports_identifier(node: &Node) -> bool {
    if !is_identifier(node) {
        return false;
    }
    node.as_identifier().escaped_text.as_str() == "exports"
}

pub fn is_module_identifier(node: &Node) -> bool {
    if !is_identifier(node) {
        return false;
    }
    node.as_identifier().escaped_text.as_str() == "module"
}

pub fn is_module_exports_access_expression(node: &Node) -> bool {
    if !(is_property_access_expression(node) || is_literal_like_element_access(node)) {
        return false;
    }
    is_module_identifier(&node.as_has_expression().expression())
        && match get_element_or_property_access_name(node) {
            Some(name) => name.eq_str("exports"),
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
    if !is_property_access_expression(&*expr.expression) {
        return false;
    }
    let expr_expression_as_property_access_expression =
        expr.expression.as_property_access_expression();
    if !is_identifier(&*expr_expression_as_property_access_expression.expression) {
        return false;
    }
    if !(id_text(&*expr_expression_as_property_access_expression.expression) == "Object") {
        return false;
    }
    if !(id_text(&*expr_expression_as_property_access_expression.name) == "defineProperty") {
        return false;
    }
    if !is_string_or_numeric_literal_like(&*expr_arguments[1]) {
        return false;
    }
    is_bindable_static_name_expression(&expr_arguments[0], Some(true))
}

pub fn is_literal_like_element_access(node: &Node) -> bool {
    if !is_element_access_expression(node) {
        return false;
    }
    let node_as_element_access_expression = node.as_element_access_expression();
    is_string_or_numeric_literal_like(&*node_as_element_access_expression.argument_expression)
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
            || is_identifier(&*node_as_property_access_expression.name)
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

fn get_assignment_declaration_kind_worker(
    expr: &Node, /*BinaryExpression | CallExpression*/
) -> AssignmentDeclarationKind {
    if is_call_expression(expr) {
        if !is_bindable_object_define_property_call(expr) {
            return AssignmentDeclarationKind::None;
        }
        let expr_as_bindable_object_define_property_call_arguments =
            &expr.as_call_expression().arguments;
        let entity_name = &expr_as_bindable_object_define_property_call_arguments[0];
        if is_exports_identifier(entity_name) || is_module_exports_access_expression(entity_name) {
            return AssignmentDeclarationKind::ObjectDefinePropertyExports;
        }
        if is_bindable_static_access_expression(entity_name, None)
            && match get_element_or_property_access_name(entity_name) {
                Some(name) => name.eq_str("prototype"),
                None => false,
            }
        {
            return AssignmentDeclarationKind::ObjectDefinePrototypeProperty;
        }
        return AssignmentDeclarationKind::ObjectDefinePropertyValue;
    }
    let expr_as_binary_expression = expr.as_binary_expression();
    if expr_as_binary_expression.operator_token.kind() != SyntaxKind::EqualsToken
        || !is_access_expression(&*expr_as_binary_expression.left)
        || is_void_zero(&get_right_most_assigned_expression(expr))
    {
        return AssignmentDeclarationKind::None;
    }
    let expr_left_as_has_expression = expr_as_binary_expression.left.as_has_expression();
    if is_bindable_static_name_expression(&expr_left_as_has_expression.expression(), Some(true))
        && match get_element_or_property_access_name(&expr_as_binary_expression.left) {
            Some(name) => name.eq_str("prototype"),
            None => false,
        }
        && is_object_literal_expression(&*get_initializer_of_binary_expression(expr))
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
    if !is_numeric_literal(&*node_as_void_expression.expression) {
        return false;
    }
    let node_expression_as_numeric_literal =
        node_as_void_expression.expression.as_numeric_literal();
    &*node_expression_as_numeric_literal.text() == "0"
}

pub fn get_element_or_property_access_argument_expression_or_name(
    node: &Node, /*AccessExpression*/
) -> Option<
    Rc<
        Node, /*Identifier | PrivateIdentifier | StringLiteralLike | NumericLiteral | ElementAccessExpression*/
    >,
> {
    if is_property_access_expression(node) {
        return Some(node.as_property_access_expression().name.clone());
    }
    let node_as_element_access_expression = node.as_element_access_expression();
    let arg = skip_parentheses(&node_as_element_access_expression.argument_expression, None);
    if is_numeric_literal(&*arg) || is_string_literal_like(&*arg) {
        return Some(arg);
    }
    Some(node.node_wrapper())
}

pub fn get_element_or_property_access_name(node: &Node, /*AccessExpression*/) -> Option<__String> {
    let name = get_element_or_property_access_argument_expression_or_name(node);
    name.and_then(|name| {
        if is_identifier(&*name) {
            return Some(name.as_identifier().escaped_text.clone());
        }
        if is_string_literal_like(&*name) || is_numeric_literal(&*name) {
            return Some(escape_leading_underscores(
                &*name.as_literal_like_node().text(),
            ));
        }
        None
    })
}

pub fn get_assignment_declaration_property_access_kind(
    lhs: &Node, /*AccessExpression*/
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
        while !is_identifier(&*next_to_last.as_has_expression().expression()) {
            next_to_last = next_to_last.as_has_expression().expression();
        }
        let id = next_to_last.as_has_expression().expression();
        let id_as_identifier = id.as_identifier();
        if (id_as_identifier.escaped_text.eq_str("exports")
            || id_as_identifier.escaped_text.eq_str("module")
                && match get_element_or_property_access_name(&next_to_last) {
                    Some(name) => name.eq_str("exports"),
                    None => false,
                })
            && is_bindable_static_access_expression(&lhs, None)
        {
            return AssignmentDeclarationKind::ExportsProperty;
        }
        if is_bindable_static_name_expression(&lhs, Some(true))
            || is_element_access_expression(&*lhs) && is_dynamic_name(&lhs)
        {
            return AssignmentDeclarationKind::Property;
        }
    }
    AssignmentDeclarationKind::None
}

pub fn get_initializer_of_binary_expression(
    expr: &Node, /*BinaryExpression*/
) -> Rc<Node /*Expression*/> {
    let mut expr = expr.node_wrapper();
    while is_binary_expression(&*expr.as_binary_expression().right) {
        expr = expr.as_binary_expression().right.clone();
    }
    expr.as_binary_expression().right.clone()
}

pub fn set_value_declaration(symbol: &Symbol, node: &Node) {
    {
        if !(symbol.maybe_value_declaration().is_none()) {
            return;
        }
    }
    symbol.set_value_declaration(node.node_wrapper());
}

pub fn is_jsdoc_type_alias(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::JSDocTypedefTag | SyntaxKind::JSDocCallbackTag | SyntaxKind::JSDocEnumTag
    )
}

pub fn is_type_alias(node: &Node) -> bool {
    is_jsdoc_type_alias(node) || is_type_alias_declaration(node)
}

fn get_source_of_defaulted_assignment(node: &Node) -> Option<Rc<Node>> {
    if !is_expression_statement(node) {
        return None;
    }
    let node_as_expression_statement = node.as_expression_statement();
    if !is_binary_expression(&*node_as_expression_statement.expression) {
        return None;
    }
    if !(get_assignment_declaration_kind(&*node_as_expression_statement.expression)
        != AssignmentDeclarationKind::None)
    {
        return None;
    }
    let node_expression_as_binary_expression = node_as_expression_statement
        .expression
        .as_binary_expression();
    if !is_binary_expression(&*node_expression_as_binary_expression.right) {
        return None;
    }
    let node_expression_right_as_binary_expression = node_expression_as_binary_expression
        .right
        .as_binary_expression();
    if matches!(
        node_expression_right_as_binary_expression
            .operator_token
            .kind(),
        SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken
    ) {
        Some(node_expression_right_as_binary_expression.right.clone())
    } else {
        None
    }
}

pub fn get_single_initializer_of_variable_statement_or_property_declaration(
    node: &Node,
) -> Option<Rc<Node /*Expression*/>> {
    match node.kind() {
        SyntaxKind::VariableStatement => {
            let v = get_single_variable_of_variable_statement(node);
            v.and_then(|v| v.as_has_initializer().maybe_initializer())
        }
        SyntaxKind::PropertyDeclaration | SyntaxKind::PropertyAssignment => {
            node.as_has_initializer().maybe_initializer()
        }
        _ => None,
    }
}

pub fn get_single_variable_of_variable_statement(
    node: &Node,
) -> Option<Rc<Node /*VariableDeclaration*/>> {
    if is_variable_statement(node) {
        first_or_undefined(
            &node
                .as_variable_statement()
                .declaration_list
                .as_variable_declaration_list()
                .declarations,
        )
        .map(Clone::clone)
    } else {
        None
    }
}

fn get_nested_module_declaration(node: &Node) -> Option<Rc<Node>> {
    if is_module_declaration(node)
        && matches!(node.as_module_declaration().body.as_ref(), Some(body) if body.kind() == SyntaxKind::ModuleDeclaration)
    {
        node.as_module_declaration().body.clone()
    } else {
        None
    }
}

pub fn get_jsdoc_comments_and_tags(
    host_node: &Node,
    no_cache: Option<bool>,
) -> Vec<Rc<Node /*JSDoc | JSDocTag*/>> {
    let no_cache = no_cache.unwrap_or(false);
    let mut result: Option<Vec<Rc<Node>>> = None;
    if is_variable_like(host_node)
        && has_initializer(host_node)
        && has_jsdoc_nodes(&*host_node.as_has_initializer().maybe_initializer().unwrap())
    {
        if result.is_none() {
            result = Some(vec![]);
        }
        /*result =*/
        add_range(
            result.as_mut().unwrap(),
            filter_owned_jsdoc_tags(
                host_node,
                &last(
                    host_node
                        .as_has_initializer()
                        .maybe_initializer()
                        .unwrap()
                        .maybe_js_doc()
                        .as_deref()
                        .unwrap(),
                ),
            )
            .as_deref(),
            None,
            None,
        );
    }

    let mut node: Option<Rc<Node>> = Some(host_node.node_wrapper());
    while matches!(node.as_ref(), Some(node) if node.maybe_parent().is_some()) {
        let node_present = node.clone().unwrap();
        if has_jsdoc_nodes(&*node_present) {
            if result.is_none() {
                result = Some(vec![]);
            }
            /*result = */
            add_range(
                result.as_mut().unwrap(),
                filter_owned_jsdoc_tags(
                    host_node,
                    &last(node_present.maybe_js_doc().as_deref().unwrap()),
                )
                .as_deref(),
                None,
                None,
            );
        }

        if node_present.kind() == SyntaxKind::Parameter {
            if result.is_none() {
                result = Some(vec![]);
            }
            /*result = */
            add_range(
                result.as_mut().unwrap(),
                Some(&(if no_cache {
                    get_jsdoc_parameter_tags_no_cache
                } else {
                    get_jsdoc_parameter_tags
                })(&node_present)),
                None,
                None,
            );
        }
        if node_present.kind() == SyntaxKind::TypeParameter {
            if result.is_none() {
                result = Some(vec![]);
            }
            /*result = */
            add_range(
                result.as_mut().unwrap(),
                Some(&(if no_cache {
                    get_jsdoc_type_parameter_tags_no_cache
                } else {
                    get_jsdoc_type_parameter_tags
                })(&node_present)),
                None,
                None,
            );
        }
        node = get_next_jsdoc_comment_location(&node_present);
    }
    result.unwrap_or(vec![])
}

fn filter_owned_jsdoc_tags(
    host_node: &Node,
    js_doc: &Node, /*JSDoc | JSDocTag*/
) -> Option<Vec<Rc<Node /*JSDoc | JSDocTag*/>>> {
    if is_jsdoc(js_doc) {
        let owned_tags = filter(js_doc.as_jsdoc().tags.as_deref(), |tag| {
            owns_jsdoc_tag(host_node, tag)
        });
        return if match (js_doc.as_jsdoc().tags.as_ref(), owned_tags.as_ref()) {
            (Some(js_doc_tags), Some(owned_tags)) if js_doc_tags.len() == owned_tags.len() => true,
            (None, None) => true,
            _ => false,
        } {
            Some(vec![js_doc.node_wrapper()])
        } else {
            owned_tags
        };
    }
    if owns_jsdoc_tag(host_node, js_doc) {
        Some(vec![js_doc.node_wrapper()])
    } else {
        None
    }
}

fn owns_jsdoc_tag(host_node: &Node, tag: &Node /*JSDocTag*/) -> bool {
    !is_jsdoc_type_tag(tag)
        || tag.maybe_parent().is_none()
        || !is_jsdoc(&*tag.parent())
        || !matches!(tag.parent().maybe_parent(), Some(grandparent) if is_parenthesized_expression(&*grandparent))
        || matches!(tag.parent().maybe_parent(), Some(grandparent) if ptr::eq(&*grandparent, host_node))
}

pub fn get_next_jsdoc_comment_location(node: &Node) -> Option<Rc<Node>> {
    let parent = node.maybe_parent();
    if matches!(
        parent.as_ref(),
        Some(parent) if parent.kind() == SyntaxKind::PropertyAssignment
            || parent.kind() == SyntaxKind::ExportAssignment
            || parent.kind() == SyntaxKind::PropertyDeclaration
            || parent.kind() == SyntaxKind::ExpressionStatement
                && node.kind() == SyntaxKind::PropertyAccessExpression
            || parent.kind() == SyntaxKind::ReturnStatement
            || get_nested_module_declaration(node).is_some()
            || is_binary_expression(node)
                && node.as_binary_expression().operator_token.kind() == SyntaxKind::EqualsToken
    ) {
        return parent;
    }
    if parent.is_none() {
        return None;
    }
    let parent = parent.unwrap();
    let grandparent = parent.maybe_parent();
    if matches!(
        grandparent.as_ref(),
        Some(grandparent) if matches!(
            get_single_variable_of_variable_statement(&grandparent),
            Some(single_variable) if ptr::eq(
                &*single_variable,
                node,
            )
        ) || is_binary_expression(&*parent)
            && parent.as_binary_expression().operator_token.kind() == SyntaxKind::EqualsToken
    ) {
        return grandparent;
    }
    if grandparent.is_none() {
        return None;
    }
    let grandparent = grandparent.unwrap();
    let great_grandparent = grandparent.maybe_parent();
    if matches!(
        great_grandparent.as_ref(),
        Some(great_grandparent) if get_single_variable_of_variable_statement(&great_grandparent).is_some()
            || matches!(
                get_single_initializer_of_variable_statement_or_property_declaration(&great_grandparent),
                Some(single_initializer) if ptr::eq(
                    &*single_initializer,
                    node,
                )
            )
            || get_source_of_defaulted_assignment(&great_grandparent).is_some()
    ) {
        return great_grandparent;
    }
    None
}

fn walk_up(node: &Node, kind: SyntaxKind) -> Option<Rc<Node>> {
    let mut node = Some(node.node_wrapper());
    loop {
        if let Some(node_present) = node.as_ref() {
            if node_present.kind() == kind {
                node = node_present.maybe_parent();
            } else {
                break;
            }
        } else {
            break;
        }
    }
    node
}

pub fn walk_up_parenthesized_expressions(node: &Node) -> Option<Rc<Node>> {
    walk_up(node, SyntaxKind::ParenthesizedExpression)
}

pub fn skip_parentheses(node: &Node, exclude_jsdoc_type_assertions: Option<bool>) -> Rc<Node> {
    let exclude_jsdoc_type_assertions = exclude_jsdoc_type_assertions.unwrap_or(false);
    let flags = if exclude_jsdoc_type_assertions {
        OuterExpressionKinds::Parentheses | OuterExpressionKinds::ExcludeJSDocTypeAssertion
    } else {
        OuterExpressionKinds::Parentheses
    };
    skip_outer_expressions(node, Some(flags))
}
