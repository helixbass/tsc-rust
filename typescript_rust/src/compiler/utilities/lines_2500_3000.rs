use gc::Gc;
use std::borrow::Borrow;
use std::ptr;

use crate::{
    add_range, find, find_ancestor, first_or_undefined, get_assignment_declaration_kind,
    get_jsdoc_parameter_tags, get_jsdoc_parameter_tags_no_cache, get_jsdoc_type_parameter_tags,
    get_jsdoc_type_parameter_tags_no_cache, get_name_of_declaration,
    get_right_most_assigned_expression, has_initializer, has_jsdoc_nodes, is_arrow_function,
    is_assignment_operator, is_binary_expression, is_binding_pattern, is_computed_property_name,
    is_constructor_declaration, is_declaration, is_expression_statement, is_function_declaration,
    is_function_expression, is_function_like, is_identifier, is_import_call, is_import_type_node,
    is_jsdoc, is_jsdoc_function_type, is_jsdoc_parameter_tag, is_jsdoc_type_tag,
    is_literal_import_type_node, is_logical_or_coalescing_assignment_operator,
    is_method_or_accessor, is_module_declaration, is_namespace_export, is_namespace_import,
    is_parenthesized_expression, is_qualified_name, is_require_call, is_source_file,
    is_string_literal, is_type_alias_declaration, is_variable_like, is_variable_statement, last,
    last_or_undefined, maybe_filter, skip_outer_expressions, try_cast, try_for_each_bool,
    AsDoubleDeref, AssignmentDeclarationKind, Debug_, HasQuestionTokenInterface, HasTypeInterface,
    NamedDeclarationInterface, Node, NodeInterface, OuterExpressionKinds,
    SignatureDeclarationInterface, Symbol, SyntaxKind,
};

pub fn try_get_import_from_module_specifier(
    node: &Node, /*StringLiteralLike*/
) -> Option<Gc<Node /*AnyValidImportOrReExport*/>> {
    let node_parent = node.parent();
    match node_parent.kind() {
        SyntaxKind::ImportDeclaration | SyntaxKind::ExportDeclaration => Some(node_parent),
        SyntaxKind::ExternalModuleReference => Some(node_parent.parent()),
        SyntaxKind::CallExpression => {
            if is_import_call(&node_parent) || is_require_call(&node_parent, false) {
                Some(node_parent)
            } else {
                None
            }
        }
        SyntaxKind::LiteralType => {
            Debug_.assert(is_string_literal(node), None);
            try_cast(node_parent.parent(), |node| is_import_type_node(&node))
        }
        _ => None,
    }
}

pub fn get_external_module_name(
    node: &Node, /*AnyImportOrReExport | ImportTypeNode | ImportCall | ModuleDeclaration*/
) -> Option<Gc<Node /*Expression*/>> {
    match node.kind() {
        SyntaxKind::ImportDeclaration => {
            Some(node.as_import_declaration().module_specifier.clone())
        }
        SyntaxKind::ExportDeclaration => node.as_export_declaration().module_specifier.clone(),
        SyntaxKind::ImportEqualsDeclaration => {
            let node_as_import_equals_declaration = node.as_import_equals_declaration();
            if node_as_import_equals_declaration.module_reference.kind()
                == SyntaxKind::ExternalModuleReference
            {
                Some(
                    node_as_import_equals_declaration
                        .module_reference
                        .as_external_module_reference()
                        .expression
                        .clone(),
                )
            } else {
                None
            }
        }
        SyntaxKind::ImportType => {
            if is_literal_import_type_node(node) {
                Some(
                    node.as_import_type_node()
                        .argument
                        .as_literal_type_node()
                        .literal
                        .clone(),
                )
            } else {
                None
            }
        }
        SyntaxKind::CallExpression => node.as_call_expression().arguments.get(0).map(Clone::clone),
        SyntaxKind::ModuleDeclaration => {
            let node_as_module_declaration = node.as_module_declaration();
            if node_as_module_declaration.name().kind() == SyntaxKind::StringLiteral {
                Some(node_as_module_declaration.name())
            } else {
                None
            }
        }
        _ => Debug_.assert_never(node, None),
    }
}

pub fn get_namespace_declaration_node(
    node: &Node, /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/
) -> Option<Gc<Node /*ImportEqualsDeclaration | NamespaceImport | NamespaceExport*/>> {
    match node.kind() {
        SyntaxKind::ImportDeclaration => {
            let node_import_clause = node.as_import_declaration().import_clause.as_ref();
            node_import_clause
                .and_then(|node_import_clause| {
                    node_import_clause
                        .as_import_clause()
                        .named_bindings
                        .as_ref()
                        .and_then(|named_bindings| {
                            try_cast(named_bindings, |named_bindings| {
                                is_namespace_import(named_bindings)
                            })
                        })
                })
                .map(Clone::clone)
        }
        SyntaxKind::ImportEqualsDeclaration => Some(node.node_wrapper()),
        SyntaxKind::ExportDeclaration => node
            .as_export_declaration()
            .export_clause
            .as_ref()
            .and_then(|export_clause| {
                try_cast(export_clause, |export_clause| {
                    is_namespace_export(export_clause)
                })
            })
            .map(Clone::clone),
        _ => Debug_.assert_never(node, None),
    }
}

pub fn is_default_import(
    node: &Node, /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/
) -> bool {
    node.kind() == SyntaxKind::ImportDeclaration
        && node
            .as_import_declaration()
            .import_clause
            .as_ref()
            .and_then(|import_clause| import_clause.as_import_clause().name.as_ref())
            .is_some()
}

pub fn for_each_import_clause_declaration_bool(
    node: &Node, /*ImportClause*/
    mut action: impl FnMut(&Node) -> bool,
) -> bool {
    try_for_each_import_clause_declaration_bool(node, |node: &Node| -> Result<_, ()> {
        Ok(action(node))
    })
    .unwrap()
}

pub fn try_for_each_import_clause_declaration_bool<TError>(
    node: &Node, /*ImportClause*/
    mut action: impl FnMut(&Node) -> Result<bool, TError>,
) -> Result<bool, TError> {
    let node_as_import_clause = node.as_import_clause();
    if node_as_import_clause.name.is_some() {
        let result = action(node)?;
        if result {
            return Ok(result);
        }
    }
    if let Some(node_named_bindings) = node_as_import_clause.named_bindings.as_ref() {
        let result = if is_namespace_import(node_named_bindings) {
            action(node_named_bindings)?
        } else {
            try_for_each_bool(
                &node_named_bindings.as_named_imports().elements,
                |element, _| action(element),
            )?
        };
        if result {
            return Ok(result);
        }
    }
    Ok(false)
}

pub fn has_question_token(node: &Node) -> bool {
    // if (node) {
    match node.kind() {
        SyntaxKind::Parameter => node.as_parameter_declaration().question_token.is_some(),
        SyntaxKind::MethodDeclaration => node
            .as_method_declaration()
            .maybe_question_token()
            .is_some(),
        SyntaxKind::MethodSignature => node.as_method_signature().question_token.is_some(),
        SyntaxKind::ShorthandPropertyAssignment => node
            .as_shorthand_property_assignment()
            .question_token
            .is_some(),
        SyntaxKind::PropertyAssignment => node.as_property_assignment().question_token.is_some(),
        SyntaxKind::PropertyDeclaration => node.as_property_declaration().question_token.is_some(),
        SyntaxKind::PropertySignature => node.as_property_signature().question_token.is_some(),
        _ => false,
    }
    // }
}

pub fn is_jsdoc_construct_signature(node: &Node) -> bool {
    let param: Option<Gc<Node>> = if is_jsdoc_function_type(node) {
        first_or_undefined(&node.as_jsdoc_function_type().parameters()).cloned()
    } else {
        None
    };
    if param.is_none() {
        return false;
    }
    let param = param.unwrap();
    let name = try_cast(param.as_parameter_declaration().maybe_name(), |name| {
        matches!(
            name.as_ref(),
            Some(name) if is_identifier(name)
        )
    })
    .flatten();
    if name.is_none() {
        return false;
    }
    let name = name.unwrap();
    name.as_identifier().escaped_text == "new"
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

fn get_source_of_assignment(node: &Node) -> Option<Gc<Node>> {
    if !is_expression_statement(node) {
        return None;
    }
    let node_as_expression_statement = node.as_expression_statement();
    if !is_binary_expression(&node_as_expression_statement.expression) {
        return None;
    }
    let node_expression_as_binary_expression = node_as_expression_statement
        .expression
        .as_binary_expression();
    if node_expression_as_binary_expression.operator_token.kind() == SyntaxKind::EqualsToken {
        Some(get_right_most_assigned_expression(
            &node_as_expression_statement.expression,
        ))
    } else {
        None
    }
}

fn get_source_of_defaulted_assignment(node: &Node) -> Option<Gc<Node>> {
    if !is_expression_statement(node) {
        return None;
    }
    let node_as_expression_statement = node.as_expression_statement();
    if !is_binary_expression(&node_as_expression_statement.expression) {
        return None;
    }
    if !(get_assignment_declaration_kind(&node_as_expression_statement.expression)
        != AssignmentDeclarationKind::None)
    {
        return None;
    }
    let node_expression_as_binary_expression = node_as_expression_statement
        .expression
        .as_binary_expression();
    if !is_binary_expression(&node_expression_as_binary_expression.right) {
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
) -> Option<Gc<Node /*Expression*/>> {
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
) -> Option<Gc<Node /*VariableDeclaration*/>> {
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

fn get_nested_module_declaration(node: &Node) -> Option<Gc<Node>> {
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
) -> Vec<Gc<Node /*JSDoc | JSDocTag*/>> {
    let no_cache = no_cache.unwrap_or(false);
    let mut result: Option<Vec<Gc<Node>>> = None;
    if is_variable_like(host_node)
        && has_initializer(host_node)
        && has_jsdoc_nodes(&host_node.as_has_initializer().maybe_initializer().unwrap())
    {
        result = Some(vec![]);
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

    let mut node: Option<Gc<Node>> = Some(host_node.node_wrapper());
    while matches!(node.as_ref(), Some(node) if node.maybe_parent().is_some()) {
        let node_present = node.as_ref().unwrap();
        if has_jsdoc_nodes(node_present) {
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
                Some(&if no_cache {
                    get_jsdoc_parameter_tags_no_cache(node_present).collect::<Vec<_>>()
                } else {
                    get_jsdoc_parameter_tags(node_present).collect::<Vec<_>>()
                }),
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
                Some(&if no_cache {
                    get_jsdoc_type_parameter_tags_no_cache(node_present).collect::<Vec<_>>()
                } else {
                    get_jsdoc_type_parameter_tags(node_present).collect::<Vec<_>>()
                }),
                None,
                None,
            );
        }
        node = get_next_jsdoc_comment_location(node_present);
    }
    result.unwrap_or_else(|| vec![])
}

fn filter_owned_jsdoc_tags(
    host_node: &Node,
    js_doc: &Node, /*JSDoc | JSDocTag*/
) -> Option<Vec<Gc<Node /*JSDoc | JSDocTag*/>>> {
    if is_jsdoc(js_doc) {
        let owned_tags = maybe_filter(
            js_doc.as_jsdoc().tags.as_double_deref(),
            |tag: &Gc<Node>| owns_jsdoc_tag(host_node, tag),
        );
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
        || !is_jsdoc(&tag.parent())
        || !matches!(tag.parent().maybe_parent(), Some(grandparent) if is_parenthesized_expression(&grandparent))
        || matches!(tag.parent().maybe_parent(), Some(grandparent) if ptr::eq(&*grandparent, host_node))
}

pub fn get_next_jsdoc_comment_location(node: &Node) -> Option<Gc<Node>> {
    let parent = node.maybe_parent();
    if matches!(
        parent.as_ref(),
        Some(parent) if matches!(parent.kind(), SyntaxKind::PropertyAssignment | SyntaxKind::ExportAssignment | SyntaxKind::PropertyDeclaration)
            || parent.kind() == SyntaxKind::ExpressionStatement
                && node.kind() == SyntaxKind::PropertyAccessExpression
            || parent.kind() == SyntaxKind::ReturnStatement
            || get_nested_module_declaration(node).is_some()
            || is_binary_expression(node)
                && node.as_binary_expression().operator_token.kind() == SyntaxKind::EqualsToken
    ) {
        return parent;
    }
    let parent = parent?;
    let grandparent = parent.maybe_parent();
    if matches!(
        grandparent.as_ref(),
        Some(grandparent) if matches!(
            get_single_variable_of_variable_statement(grandparent),
            Some(single_variable) if ptr::eq(
                &*single_variable,
                node,
            )
        ) || is_binary_expression(&parent)
            && parent.as_binary_expression().operator_token.kind() == SyntaxKind::EqualsToken
    ) {
        return grandparent;
    }
    let grandparent = grandparent?;
    let great_grandparent = grandparent.maybe_parent();
    if matches!(
        great_grandparent.as_ref(),
        Some(great_grandparent) if get_single_variable_of_variable_statement(great_grandparent).is_some()
            || matches!(
                get_single_initializer_of_variable_statement_or_property_declaration(great_grandparent),
                Some(single_initializer) if ptr::eq(
                    &*single_initializer,
                    node,
                )
            )
            || get_source_of_defaulted_assignment(great_grandparent).is_some()
    ) {
        return great_grandparent;
    }
    None
}

pub fn get_parameter_symbol_from_jsdoc(node: &Node, /*JSDocParameterTag*/) -> Option<Gc<Symbol>> {
    if node.maybe_symbol().is_some() {
        return node.maybe_symbol();
    }
    let node_as_jsdoc_property_like_tag = node.as_jsdoc_property_like_tag();
    if !is_identifier(&node_as_jsdoc_property_like_tag.name) {
        return None;
    }
    let name = &node_as_jsdoc_property_like_tag
        .name
        .as_identifier()
        .escaped_text;
    let decl = get_host_signature_from_jsdoc(node);
    let decl = decl?;
    let parameter = find(
        &decl.as_signature_declaration().parameters(),
        |p: &Gc<Node>, _| {
            let p_name = p.as_parameter_declaration().name();
            p_name.kind() == SyntaxKind::Identifier && &p_name.as_identifier().escaped_text == name
        },
    )
    .cloned();
    parameter.and_then(|parameter| parameter.maybe_symbol())
}

pub fn get_effective_container_for_jsdoc_template_tag(
    node: &Node, /*JSDocTemplateTag*/
) -> Option<Gc<Node>> {
    let node_parent = node.parent();
    if is_jsdoc(&node_parent) {
        if let Some(node_parent_tags) = node_parent.as_jsdoc().tags.as_ref() {
            let type_alias = find(node_parent_tags, |tag, _| is_jsdoc_type_alias(tag));
            if type_alias.is_some() {
                return type_alias.map(Clone::clone);
            }
        }
    }
    get_host_signature_from_jsdoc(node)
}

pub fn get_host_signature_from_jsdoc(node: &Node) -> Option<Gc<Node /*SignatureDeclaration*/>> {
    let host = get_effective_jsdoc_host(node);
    host.filter(|host| is_function_like(Some(&**host)))
}

pub fn get_effective_jsdoc_host(node: &Node) -> Option<Gc<Node>> {
    let host = get_jsdoc_host(node);
    let host = host?;
    get_source_of_defaulted_assignment(&host).or_else(|| {
        get_source_of_assignment(&host).or_else(|| {
            get_single_initializer_of_variable_statement_or_property_declaration(&host).or_else(
                || {
                    get_single_variable_of_variable_statement(&host)
                        .or_else(|| get_nested_module_declaration(&host).or_else(|| Some(host)))
                },
            )
        })
    })
}

pub fn get_jsdoc_host(node: &Node) -> Option<Gc<Node /*HasJSDoc*/>> {
    let js_doc = get_jsdoc_root(node)?;

    let host = js_doc.maybe_parent();
    host.filter(|host| {
        let host_js_doc = host.maybe_js_doc();
        if host_js_doc.is_none() {
            return false;
        }
        let host_js_doc = host_js_doc.unwrap();
        // TODO: seems weird that .maybe_js_doc() is returning a Vec rather than a slice?
        matches!(last_or_undefined(&host_js_doc), Some(last) if Gc::ptr_eq(&js_doc, last))
    })
}

pub fn get_jsdoc_root(node: &Node) -> Option<Gc<Node /*JSDoc*/>> {
    find_ancestor(node.maybe_parent(), |node| is_jsdoc(node))
}

pub fn get_type_parameter_from_js_doc(
    node: &Node, /*TypeParameterDeclaration & { parent: JSDocTemplateTag }*/
) -> Option<Gc<Node /*TypeParameterDeclaration*/>> {
    let node_name = node.as_type_parameter_declaration().name();
    let name = &node_name.as_identifier().escaped_text;
    let node_parent_parent_parent = node.parent().parent().parent();
    let type_parameters = node_parent_parent_parent
        .as_has_type_parameters()
        .maybe_type_parameters();
    type_parameters
        .as_ref()
        .and_then(|type_parameters| {
            find(type_parameters, |p, _| {
                &p.as_type_parameter_declaration()
                    .name()
                    .as_identifier()
                    .escaped_text
                    == name
            })
        })
        .map(Clone::clone)
}

pub fn has_rest_parameter(node: &Node /*SignatureDeclaration | JSDocSignature*/) -> bool {
    let last = match node.kind() {
        SyntaxKind::JSDocSignature => {
            last_or_undefined(&node.as_jsdoc_signature().parameters).cloned()
        }
        _ => last_or_undefined(&node.as_signature_declaration().parameters()).cloned(),
    };
    if last.is_none() {
        return false;
    }
    let ref last = last.unwrap();
    is_rest_parameter(last)
}

pub fn is_rest_parameter(node: &Node /*ParameterDeclaration | JSDocParameterTag*/) -> bool {
    let type_: Option<Gc<Node>> = if is_jsdoc_parameter_tag(node) {
        node.as_jsdoc_property_like_tag()
            .type_expression
            .as_ref()
            .map(|type_expression| type_expression.as_jsdoc_type_expression().type_.clone())
    } else {
        node.as_parameter_declaration().maybe_type()
    };
    node.kind() == SyntaxKind::Parameter
        && node.as_parameter_declaration().dot_dot_dot_token.is_some()
        || type_
            .filter(|type_| type_.kind() == SyntaxKind::JSDocVariadicType)
            .is_some()
}

pub fn has_type_arguments(node: &Node) -> bool {
    node.as_has_type_arguments()
        .maybe_type_arguments()
        .is_some()
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AssignmentKind {
    None,
    Definite,
    Compound,
}

pub fn get_assignment_target_kind(node: &Node) -> AssignmentKind {
    let mut node = node.node_wrapper();
    let mut parent = node.parent();
    loop {
        match parent.kind() {
            SyntaxKind::BinaryExpression => {
                let parent_as_binary_expression = parent.as_binary_expression();
                let binary_operator = parent_as_binary_expression.operator_token.kind();
                return if is_assignment_operator(binary_operator)
                    && Gc::ptr_eq(&parent_as_binary_expression.left, &node)
                {
                    if binary_operator == SyntaxKind::EqualsToken
                        || is_logical_or_coalescing_assignment_operator(binary_operator)
                    {
                        AssignmentKind::Definite
                    } else {
                        AssignmentKind::Compound
                    }
                } else {
                    AssignmentKind::None
                };
            }
            SyntaxKind::PrefixUnaryExpression => {
                let unary_operator = &parent.as_prefix_unary_expression().operator;
                return if matches!(
                    unary_operator,
                    SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
                ) {
                    AssignmentKind::Compound
                } else {
                    AssignmentKind::None
                };
            }
            SyntaxKind::PostfixUnaryExpression => {
                let unary_operator = &parent.as_postfix_unary_expression().operator;
                return if matches!(
                    unary_operator,
                    SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
                ) {
                    AssignmentKind::Compound
                } else {
                    AssignmentKind::None
                };
            }
            SyntaxKind::ForInStatement => {
                return if Gc::ptr_eq(&parent.as_for_in_statement().initializer, &node) {
                    AssignmentKind::Definite
                } else {
                    AssignmentKind::None
                };
            }
            SyntaxKind::ForOfStatement => {
                return if Gc::ptr_eq(&parent.as_for_of_statement().initializer, &node) {
                    AssignmentKind::Definite
                } else {
                    AssignmentKind::None
                };
            }
            SyntaxKind::ParenthesizedExpression
            | SyntaxKind::ArrayLiteralExpression
            | SyntaxKind::SpreadElement
            | SyntaxKind::NonNullExpression => {
                node = parent;
            }
            SyntaxKind::SpreadAssignment => {
                node = parent.parent();
            }
            SyntaxKind::ShorthandPropertyAssignment => {
                if !Gc::ptr_eq(&parent.as_shorthand_property_assignment().name(), &node) {
                    return AssignmentKind::None;
                }
                node = parent.parent();
            }
            SyntaxKind::PropertyAssignment => {
                if Gc::ptr_eq(&parent.as_property_assignment().name(), &node) {
                    return AssignmentKind::None;
                }
                node = parent.parent();
            }
            _ => {
                return AssignmentKind::None;
            }
        }
        parent = node.parent();
    }
}

pub fn is_assignment_target(node: &Node) -> bool {
    get_assignment_target_kind(node) != AssignmentKind::None
}

pub fn is_node_with_possible_hoisted_declaration(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::Block
            | SyntaxKind::VariableStatement
            | SyntaxKind::WithStatement
            | SyntaxKind::IfStatement
            | SyntaxKind::SwitchStatement
            | SyntaxKind::CaseBlock
            | SyntaxKind::CaseClause
            | SyntaxKind::DefaultClause
            | SyntaxKind::LabeledStatement
            | SyntaxKind::ForStatement
            | SyntaxKind::ForInStatement
            | SyntaxKind::ForOfStatement
            | SyntaxKind::DoStatement
            | SyntaxKind::WhileStatement
            | SyntaxKind::TryStatement
            | SyntaxKind::CatchClause
    )
}

pub fn is_value_signature_declaration(node: &Node) -> bool {
    is_function_expression(node)
        || is_arrow_function(node)
        || is_method_or_accessor(node)
        || is_function_declaration(node)
        || is_constructor_declaration(node)
}

fn walk_up(node: &Node, kind: SyntaxKind) -> Option<Gc<Node>> {
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

pub fn walk_up_parenthesized_types(node: &Node) -> Option<Gc<Node>> {
    walk_up(node, SyntaxKind::ParenthesizedType)
}

pub fn walk_up_parenthesized_expressions(node: &Node) -> Option<Gc<Node>> {
    walk_up(node, SyntaxKind::ParenthesizedExpression)
}

pub fn walk_up_parenthesized_types_and_get_parent_and_child(
    node: &Node,
) -> (Option<Gc<Node /*ParenthesizedTypeNode*/>>, Option<Gc<Node>>) {
    let mut child: Option<Gc<Node>> = None;
    let mut node: Option<Gc<Node>> = Some(node.node_wrapper());
    while matches!(node.as_ref(), Some(node) if node.kind() == SyntaxKind::ParenthesizedType) {
        let node_parent = node.as_ref().unwrap().maybe_parent();
        child = node;
        node = node_parent;
    }
    (child, node)
}

pub fn skip_parentheses(node: &Node, exclude_jsdoc_type_assertions: Option<bool>) -> Gc<Node> {
    let exclude_jsdoc_type_assertions = exclude_jsdoc_type_assertions.unwrap_or(false);
    let flags = if exclude_jsdoc_type_assertions {
        OuterExpressionKinds::Parentheses | OuterExpressionKinds::ExcludeJSDocTypeAssertion
    } else {
        OuterExpressionKinds::Parentheses
    };
    skip_outer_expressions(node, Some(flags))
}

pub fn is_delete_target(node: &Node) -> bool {
    if !matches!(
        node.kind(),
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression
    ) {
        return false;
    }
    let node = walk_up_parenthesized_expressions(&node.parent());
    matches!(node, Some(node) if node.kind() == SyntaxKind::DeleteExpression)
}

pub fn is_node_descendant_of<TAncestor: Borrow<Node>>(
    node: &Node,
    ancestor: Option<TAncestor>,
) -> bool {
    if ancestor.is_none() {
        return false;
    }
    let ancestor = ancestor.unwrap();
    let ancestor = ancestor.borrow();
    let mut node = Some(node.node_wrapper());
    while let Some(node_present) = node.as_ref() {
        if ptr::eq(&**node_present, ancestor) {
            return true;
        }
        node = node_present.maybe_parent();
    }
    false
}

pub fn is_declaration_name(name: &Node) -> bool {
    !is_source_file(name)
        && !is_binding_pattern(Some(name))
        && is_declaration(&name.parent())
        && matches!(name.parent().as_named_declaration().maybe_name(), Some(parent_name) if ptr::eq(&*parent_name, name))
}

pub fn get_declaration_from_name(name: &Node) -> Option<Gc<Node /*Declaration*/>> {
    let parent = name.parent();
    match name.kind() {
        SyntaxKind::StringLiteral
        | SyntaxKind::NoSubstitutionTemplateLiteral
        | SyntaxKind::NumericLiteral => {
            if is_computed_property_name(&parent) {
                return parent.maybe_parent();
            }
            if is_declaration(&parent) {
                if matches!(parent.as_named_declaration().maybe_name(), Some(parent_name) if ptr::eq(&*parent_name, name))
                {
                    Some(parent)
                } else {
                    None
                }
            } else if is_qualified_name(&parent) {
                let tag = parent.parent();
                if is_jsdoc_parameter_tag(&tag)
                    && Gc::ptr_eq(&tag.as_jsdoc_property_like_tag().name, &parent)
                {
                    Some(tag)
                } else {
                    None
                }
            } else {
                let bin_exp = parent.parent();
                if is_binary_expression(&bin_exp)
                    && get_assignment_declaration_kind(&bin_exp) != AssignmentDeclarationKind::None
                    && (bin_exp.as_binary_expression().left.maybe_symbol().is_some()
                        || bin_exp.maybe_symbol().is_some())
                    && matches!(get_name_of_declaration(Some(&*bin_exp)), Some(name_of_declaration) if ptr::eq(&*name_of_declaration, name))
                {
                    Some(bin_exp)
                } else {
                    None
                }
            }
        }
        SyntaxKind::Identifier => {
            if is_declaration(&parent) {
                if matches!(parent.as_named_declaration().maybe_name(), Some(parent_name) if ptr::eq(&*parent_name, name))
                {
                    Some(parent)
                } else {
                    None
                }
            } else if is_qualified_name(&parent) {
                let tag = parent.parent();
                if is_jsdoc_parameter_tag(&tag)
                    && Gc::ptr_eq(&tag.as_jsdoc_property_like_tag().name, &parent)
                {
                    Some(tag)
                } else {
                    None
                }
            } else {
                let bin_exp = parent.parent();
                if is_binary_expression(&bin_exp)
                    && get_assignment_declaration_kind(&bin_exp) != AssignmentDeclarationKind::None
                    && (bin_exp.as_binary_expression().left.maybe_symbol().is_some()
                        || bin_exp.maybe_symbol().is_some())
                    && matches!(get_name_of_declaration(Some(&*bin_exp)), Some(name_of_declaration) if ptr::eq(&*name_of_declaration, name))
                {
                    Some(bin_exp)
                } else {
                    None
                }
            }
        }
        SyntaxKind::PrivateIdentifier => {
            if is_declaration(&parent)
                && matches!(parent.as_named_declaration().maybe_name(), Some(parent_name) if ptr::eq(&*parent_name, name))
            {
                Some(parent)
            } else {
                None
            }
        }
        _ => None,
    }
}
