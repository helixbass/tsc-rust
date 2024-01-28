use std::{borrow::Borrow, ptr};

use gc::Gc;
use id_arena::Id;

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
    HasArena, InArena, OptionInArena,
};

pub fn try_get_import_from_module_specifier(
    node: Id<Node>, /*StringLiteralLike*/ arena: &impl HasArena
) -> Option<Id<Node /*AnyValidImportOrReExport*/>> {
    let node_parent = node.ref_(arena).parent();
    match node_parent.ref_(arena).kind() {
        SyntaxKind::ImportDeclaration | SyntaxKind::ExportDeclaration => Some(node_parent),
        SyntaxKind::ExternalModuleReference => Some(node_parent.ref_(arena).parent()),
        SyntaxKind::CallExpression => {
            if is_import_call(node_parent, arena) || is_require_call(node_parent, false, arena) {
                Some(node_parent)
            } else {
                None
            }
        }
        SyntaxKind::LiteralType => {
            Debug_.assert(is_string_literal(&node.ref_(arena)), None);
            try_cast(node_parent.ref_(arena).parent(), |node| is_import_type_node(&node.ref_(arena)))
        }
        _ => None,
    }
}

pub fn get_external_module_name(
    node: Id<Node>, /*AnyImportOrReExport | ImportTypeNode | ImportCall | ModuleDeclaration*/
    arena: &impl HasArena,
) -> Option<Id<Node /*Expression*/>> {
    match node.ref_(arena).kind() {
        SyntaxKind::ImportDeclaration => {
            Some(node.ref_(arena).as_import_declaration().module_specifier)
        }
        SyntaxKind::ExportDeclaration => node.ref_(arena).as_export_declaration().module_specifier,
        SyntaxKind::ImportEqualsDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_import_equals_declaration = node_ref.as_import_equals_declaration();
            if node_as_import_equals_declaration.module_reference.ref_(arena).kind()
                == SyntaxKind::ExternalModuleReference
            {
                Some(
                    node_as_import_equals_declaration
                        .module_reference
                        .ref_(arena).as_external_module_reference()
                        .expression
                )
            } else {
                None
            }
        }
        SyntaxKind::ImportType => {
            if is_literal_import_type_node(node, arena) {
                Some(
                    node.ref_(arena).as_import_type_node()
                        .argument
                        .ref_(arena).as_literal_type_node()
                        .literal,
                )
            } else {
                None
            }
        }
        SyntaxKind::CallExpression => node.ref_(arena).as_call_expression().arguments.ref_(arena).get(0).copied(),
        SyntaxKind::ModuleDeclaration => {
            let node_ref = node.ref_(arena);
            let node_as_module_declaration = node_ref.as_module_declaration();
            if node_as_module_declaration.name().ref_(arena).kind() == SyntaxKind::StringLiteral {
                Some(node_as_module_declaration.name())
            } else {
                None
            }
        }
        _ => Debug_.assert_never(node, None),
    }
}

pub fn get_namespace_declaration_node(
    node: Id<Node>, /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/
    arena: &impl HasArena,
) -> Option<Id<Node /*ImportEqualsDeclaration | NamespaceImport | NamespaceExport*/>> {
    match node.ref_(arena).kind() {
        SyntaxKind::ImportDeclaration => {
            let node_import_clause = node.ref_(arena).as_import_declaration().import_clause;
            node_import_clause
                .and_then(|node_import_clause| {
                    node_import_clause
                        .ref_(arena).as_import_clause()
                        .named_bindings
                        .and_then(|named_bindings| {
                            try_cast(named_bindings, |named_bindings| {
                                is_namespace_import(&named_bindings.ref_(arena))
                            })
                        })
                })
        }
        SyntaxKind::ImportEqualsDeclaration => Some(node),
        SyntaxKind::ExportDeclaration => node
            .ref_(arena).as_export_declaration()
            .export_clause
            .and_then(|export_clause| {
                try_cast(export_clause, |export_clause| {
                    is_namespace_export(&export_clause.ref_(arena))
                })
            }),
        _ => Debug_.assert_never(node, None),
    }
}

pub fn is_default_import(
    node: Id<Node>, /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/
    arena: &impl HasArena,
) -> bool {
    node.ref_(arena).kind() == SyntaxKind::ImportDeclaration
        && node
            .ref_(arena).as_import_declaration()
            .import_clause
            .and_then(|import_clause| import_clause.ref_(arena).as_import_clause().name)
            .is_some()
}

pub fn for_each_import_clause_declaration_bool(
    node: Id<Node>, /*ImportClause*/
    mut action: impl FnMut(Id<Node>) -> bool,
    arena: &impl HasArena,
) -> bool {
    try_for_each_import_clause_declaration_bool(node, |node: Id<Node>| -> Result<_, ()> {
        Ok(action(node))
    }, arena)
    .unwrap()
}

pub fn try_for_each_import_clause_declaration_bool<TError>(
    node: Id<Node>, /*ImportClause*/
    mut action: impl FnMut(Id<Node>) -> Result<bool, TError>,
    arena: &impl HasArena,
) -> Result<bool, TError> {
    let node_ref = node.ref_(arena);
    let node_as_import_clause = node_ref.as_import_clause();
    if node_as_import_clause.name.is_some() {
        let result = action(node)?;
        if result {
            return Ok(result);
        }
    }
    if let Some(node_named_bindings) = node_as_import_clause.named_bindings {
        let result = if is_namespace_import(&node_named_bindings.ref_(arena)) {
            action(node_named_bindings)?
        } else {
            try_for_each_bool(
                &*node_named_bindings.ref_(arena).as_named_imports().elements.ref_(arena),
                |&element, _| action(element),
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

pub fn is_jsdoc_construct_signature(node: Id<Node>, arena: &impl HasArena) -> bool {
    let Some(param) = (if is_jsdoc_function_type(&node.ref_(arena)) {
        first_or_undefined(&node.ref_(arena).as_jsdoc_function_type().parameters().ref_(arena)).cloned()
    } else {
        None
    }) else {
        return false;
    };
    let Some(name) = try_cast(param.ref_(arena).as_parameter_declaration().maybe_name(), |&name| {
        matches!(
            name,
            Some(name) if is_identifier(&name.ref_(arena))
        )
    })
    .flatten() else {
        return false;
    };
    name.ref_(arena).as_identifier().escaped_text == "new"
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

fn get_source_of_assignment(node: Id<Node>, arena: &impl HasArena) -> Option<Id<Node>> {
    if !is_expression_statement(&node.ref_(arena)) {
        return None;
    }
    let node_ref = node.ref_(arena);
    let node_as_expression_statement = node_ref.as_expression_statement();
    if !is_binary_expression(&node_as_expression_statement.expression.ref_(arena)) {
        return None;
    }
    let node_expression_ref = node_as_expression_statement
        .expression
        .ref_(arena);
    let node_expression_as_binary_expression = node_expression_ref
        .as_binary_expression();
    if node_expression_as_binary_expression.operator_token.ref_(arena).kind() == SyntaxKind::EqualsToken {
        Some(get_right_most_assigned_expression(
            node_as_expression_statement.expression,
            arena,
        ))
    } else {
        None
    }
}

fn get_source_of_defaulted_assignment(node: Id<Node>, arena: &impl HasArena) -> Option<Id<Node>> {
    if !is_expression_statement(&node.ref_(arena)) {
        return None;
    }
    let node_ref = node.ref_(arena);
    let node_as_expression_statement = node_ref.as_expression_statement();
    if !is_binary_expression(&node_as_expression_statement.expression.ref_(arena)) {
        return None;
    }
    if !(get_assignment_declaration_kind(node_as_expression_statement.expression, arena)
        != AssignmentDeclarationKind::None)
    {
        return None;
    }
    let node_expression_ref = node_as_expression_statement
        .expression.ref_(arena);
    let node_expression_as_binary_expression = node_expression_ref.as_binary_expression();
    if !is_binary_expression(&node_expression_as_binary_expression.right.ref_(arena)) {
        return None;
    }
    let node_expression_right_ref = node_expression_as_binary_expression
        .right
        .ref_(arena);
    let node_expression_right_as_binary_expression = node_expression_right_ref
        .as_binary_expression();
    if matches!(
        node_expression_right_as_binary_expression
            .operator_token
            .ref_(arena).kind(),
        SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken
    ) {
        Some(node_expression_right_as_binary_expression.right)
    } else {
        None
    }
}

pub fn get_single_initializer_of_variable_statement_or_property_declaration(
    node: Id<Node>,
    arena: &impl HasArena,
) -> Option<Id<Node /*Expression*/>> {
    match node.ref_(arena).kind() {
        SyntaxKind::VariableStatement => {
            let v = get_single_variable_of_variable_statement(node, arena);
            v.and_then(|v| v.ref_(arena).as_has_initializer().maybe_initializer())
        }
        SyntaxKind::PropertyDeclaration | SyntaxKind::PropertyAssignment => {
            node.ref_(arena).as_has_initializer().maybe_initializer()
        }
        _ => None,
    }
}

pub fn get_single_variable_of_variable_statement(
    node: Id<Node>,
    arena: &impl HasArena,
) -> Option<Id<Node /*VariableDeclaration*/>> {
    if is_variable_statement(&node.ref_(arena)) {
        first_or_undefined(
            &node
                .ref_(arena).as_variable_statement()
                .declaration_list
                .ref_(arena).as_variable_declaration_list()
                .declarations.ref_(arena),
        )
        .copied()
    } else {
        None
    }
}

fn get_nested_module_declaration(node: Id<Node>, arena: &impl HasArena) -> Option<Id<Node>> {
    if is_module_declaration(&node.ref_(arena))
        && matches!(
            node.ref_(arena).as_module_declaration().body,
            Some(body) if body.ref_(arena).kind() == SyntaxKind::ModuleDeclaration
        )
    {
        node.ref_(arena).as_module_declaration().body
    } else {
        None
    }
}

pub fn get_jsdoc_comments_and_tags(
    host_node: Id<Node>,
    no_cache: Option<bool>, arena: &impl HasArena,
) -> Vec<Id<Node /*JSDoc | JSDocTag*/>> {
    let no_cache = no_cache.unwrap_or(false);
    let mut result: Option<Vec<Id<Node>>> = None;
    if is_variable_like(&host_node.ref_(arena))
        && has_initializer(&host_node.ref_(arena))
        && has_jsdoc_nodes(&host_node.ref_(arena).as_has_initializer().maybe_initializer().unwrap().ref_(arena))
    {
        result = Some(vec![]);
        /*result =*/
        add_range(
            result.as_mut().unwrap(),
            filter_owned_jsdoc_tags(
                host_node,
                *last(
                    host_node
                        .ref_(arena).as_has_initializer()
                        .maybe_initializer()
                        .unwrap()
                        .ref_(arena).maybe_js_doc()
                        .as_deref()
                        .unwrap(),
                ),
                arena,
            )
            .as_deref(),
            None,
            None,
        );
    }

    let mut node: Option<Id<Node>> = Some(host_node);
    while matches!(node, Some(node) if node.ref_(arena).maybe_parent().is_some()) {
        let node_present = node.unwrap();
        if has_jsdoc_nodes(&node_present.ref_(arena)) {
            if result.is_none() {
                result = Some(vec![]);
            }
            /*result = */
            add_range(
                result.as_mut().unwrap(),
                filter_owned_jsdoc_tags(
                    host_node,
                    *last(node_present.ref_(arena).maybe_js_doc().as_deref().unwrap()),
                    arena,
                )
                .as_deref(),
                None,
                None,
            );
        }

        if node_present.ref_(arena).kind() == SyntaxKind::Parameter {
            if result.is_none() {
                result = Some(vec![]);
            }
            /*result = */
            add_range(
                result.as_mut().unwrap(),
                Some(&if no_cache {
                    get_jsdoc_parameter_tags_no_cache(node_present, arena).collect::<Vec<_>>()
                } else {
                    get_jsdoc_parameter_tags(node_present, arena).collect::<Vec<_>>()
                }),
                None,
                None,
            );
        }
        if node_present.ref_(arena).kind() == SyntaxKind::TypeParameter {
            if result.is_none() {
                result = Some(vec![]);
            }
            /*result = */
            add_range(
                result.as_mut().unwrap(),
                Some(&if no_cache {
                    get_jsdoc_type_parameter_tags_no_cache(node_present, arena).collect::<Vec<_>>()
                } else {
                    get_jsdoc_type_parameter_tags(node_present, arena).collect::<Vec<_>>()
                }),
                None,
                None,
            );
        }
        node = get_next_jsdoc_comment_location(node_present, arena);
    }
    result.unwrap_or_else(|| vec![])
}

fn filter_owned_jsdoc_tags(
    host_node: Id<Node>,
    js_doc: Id<Node>, /*JSDoc | JSDocTag*/
    arena: &impl HasArena
) -> Option<Vec<Id<Node /*JSDoc | JSDocTag*/>>> {
    if is_jsdoc(&js_doc.ref_(arena)) {
        let owned_tags = maybe_filter(
            js_doc.ref_(arena).as_jsdoc().tags.refed(arena).as_double_deref(),
            |&tag: &Id<Node>| owns_jsdoc_tag(host_node, tag, arena),
        );
        return if match (js_doc.ref_(arena).as_jsdoc().tags.as_ref(), owned_tags.as_ref()) {
            (Some(js_doc_tags), Some(owned_tags)) if js_doc_tags.ref_(arena).len() == owned_tags.len() => true,
            (None, None) => true,
            _ => false,
        } {
            Some(vec![js_doc])
        } else {
            owned_tags
        };
    }
    if owns_jsdoc_tag(host_node, js_doc, arena) {
        Some(vec![js_doc])
    } else {
        None
    }
}

fn owns_jsdoc_tag(host_node: Id<Node>, tag: Id<Node> /*JSDocTag*/, arena: &impl HasArena) -> bool {
    !is_jsdoc_type_tag(&tag.ref_(arena))
        || tag.ref_(arena).maybe_parent().is_none()
        || !is_jsdoc(&tag.ref_(arena).parent().ref_(arena))
        || !matches!(
            tag.ref_(arena).parent().ref_(arena).maybe_parent(),
            Some(grandparent) if is_parenthesized_expression(&grandparent.ref_(arena))
        ) || matches!(
            tag.ref_(arena).parent().ref_(arena).maybe_parent(),
            Some(grandparent) if grandparent == host_node
        )
}

pub fn get_next_jsdoc_comment_location(node: Id<Node>, arena: &impl HasArena) -> Option<Id<Node>> {
    let parent = node.ref_(arena).maybe_parent();
    if matches!(
        parent,
        Some(parent) if matches!(
            parent.ref_(arena).kind(),
            SyntaxKind::PropertyAssignment | SyntaxKind::ExportAssignment | SyntaxKind::PropertyDeclaration
        ) || parent.ref_(arena).kind() == SyntaxKind::ExpressionStatement
                && node.ref_(arena).kind() == SyntaxKind::PropertyAccessExpression
            || parent.ref_(arena).kind() == SyntaxKind::ReturnStatement
            || get_nested_module_declaration(node, arena).is_some()
            || is_binary_expression(&node.ref_(arena))
                && node.ref_(arena).as_binary_expression().operator_token.ref_(arena).kind() == SyntaxKind::EqualsToken
    ) {
        return parent;
    }
    let parent = parent?;
    let grandparent = parent.ref_(arena).maybe_parent();
    if matches!(
        grandparent,
        Some(grandparent) if matches!(
            get_single_variable_of_variable_statement(grandparent, arena),
            Some(single_variable) if single_variable == node
        ) || is_binary_expression(&parent.ref_(arena))
            && parent.ref_(arena).as_binary_expression().operator_token.ref_(arena).kind() == SyntaxKind::EqualsToken
    ) {
        return grandparent;
    }
    let grandparent = grandparent?;
    let great_grandparent = grandparent.ref_(arena).maybe_parent();
    if matches!(
        great_grandparent,
        Some(great_grandparent) if get_single_variable_of_variable_statement(great_grandparent, arena).is_some()
            || matches!(
                get_single_initializer_of_variable_statement_or_property_declaration(great_grandparent, arena),
                Some(single_initializer) if single_initializer == node
            )
            || get_source_of_defaulted_assignment(great_grandparent, arena).is_some()
    ) {
        return great_grandparent;
    }
    None
}

pub fn get_parameter_symbol_from_jsdoc(node: Id<Node> /*JSDocParameterTag*/, arena: &impl HasArena) -> Option<Id<Symbol>> {
    if node.ref_(arena).maybe_symbol().is_some() {
        return node.ref_(arena).maybe_symbol();
    }
    let node_ref = node.ref_(arena);
    let node_as_jsdoc_property_like_tag = node_ref.as_jsdoc_property_like_tag();
    if !is_identifier(&node_as_jsdoc_property_like_tag.name.ref_(arena)) {
        return None;
    }
    let node_name_ref = node_as_jsdoc_property_like_tag
        .name
        .ref_(arena);
    let name = &node_name_ref
        .as_identifier()
        .escaped_text;
    let decl = get_host_signature_from_jsdoc(node, arena);
    let decl = decl?;
    let parameter = find(
        &decl.ref_(arena).as_signature_declaration().parameters().ref_(arena),
        |p: &Id<Node>, _| {
            let p_name = p.ref_(arena).as_parameter_declaration().name();
            p_name.ref_(arena).kind() == SyntaxKind::Identifier && &p_name.ref_(arena).as_identifier().escaped_text == name
        },
    ).copied();
    parameter.and_then(|parameter| parameter.ref_(arena).maybe_symbol())
}

pub fn get_effective_container_for_jsdoc_template_tag(
    node: Id<Node>, /*JSDocTemplateTag*/
    arena: &impl HasArena,
) -> Option<Id<Node>> {
    let node_parent = node.ref_(arena).parent();
    if is_jsdoc(&node_parent.ref_(arena)) {
        if let Some(node_parent_tags) = node_parent.ref_(arena).as_jsdoc().tags {
            let type_alias = find(&node_parent_tags.ref_(arena), |tag, _| is_jsdoc_type_alias(&tag.ref_(arena)));
            if type_alias.is_some() {
                return type_alias.copied();
            }
        }
    }
    get_host_signature_from_jsdoc(node, arena)
}

pub fn get_host_signature_from_jsdoc(node: Id<Node>, arena: &impl HasArena) -> Option<Id<Node /*SignatureDeclaration*/>> {
    let host = get_effective_jsdoc_host(node, arena);
    host.filter(|host| is_function_like(Some(&host.ref_(arena))))
}

pub fn get_effective_jsdoc_host(node: Id<Node>, arena: &impl HasArena) -> Option<Id<Node>> {
    let host = get_jsdoc_host(node, arena)?;
    get_source_of_defaulted_assignment(host, arena).or_else(|| {
        get_source_of_assignment(host, arena).or_else(|| {
            get_single_initializer_of_variable_statement_or_property_declaration(host, arena).or_else(
                || {
                    get_single_variable_of_variable_statement(host, arena)
                        .or_else(|| get_nested_module_declaration(host, arena).or_else(|| Some(host)))
                },
            )
        })
    })
}

pub fn get_jsdoc_host(node: Id<Node>, arena: &impl HasArena) -> Option<Id<Node /*HasJSDoc*/>> {
    let js_doc = get_jsdoc_root(node, arena)?;

    let host = js_doc.ref_(arena).maybe_parent()?;
    let host_js_doc = host.ref_(arena).maybe_js_doc()?;
    // TODO: seems weird that .maybe_js_doc() is returning a Vec rather than a slice?
    matches!(
        last_or_undefined(&host_js_doc),
        Some(&last) if js_doc == last
    ).then_some(host)
}

pub fn get_jsdoc_root(node: Id<Node>, arena: &impl HasArena) -> Option<Id<Node /*JSDoc*/>> {
    find_ancestor(node.ref_(arena).maybe_parent(), |node| is_jsdoc(&node.ref_(arena)), arena)
}

pub fn get_type_parameter_from_js_doc(
    node: Id<Node>, /*TypeParameterDeclaration & { parent: JSDocTemplateTag }*/
    arena: &impl HasArena,
) -> Option<Id<Node /*TypeParameterDeclaration*/>> {
    let node_name_ref = node.ref_(arena).as_type_parameter_declaration().name().ref_(arena);
    let name = &node_name_ref.as_identifier().escaped_text;
    let node_parent_parent_parent = node.ref_(arena).parent().ref_(arena).parent().ref_(arena).parent();
    let type_parameters = node_parent_parent_parent
        .ref_(arena).as_has_type_parameters()
        .maybe_type_parameters();
    type_parameters
        .and_then(|type_parameters| {
            find(&type_parameters.ref_(arena), |p, _| {
                &p.ref_(arena).as_type_parameter_declaration()
                    .name()
                    .ref_(arena).as_identifier()
                    .escaped_text
                    == name
            })
        })
        .copied()
}

pub fn has_rest_parameter(node: Id<Node> /*SignatureDeclaration | JSDocSignature*/, arena: &impl HasArena) -> bool {
    let Some(last) = (match node.ref_(arena).kind() {
        SyntaxKind::JSDocSignature => {
            last_or_undefined(&node.ref_(arena).as_jsdoc_signature().parameters.ref_(arena)).copied()
        }
        _ => last_or_undefined(&node.ref_(arena).as_signature_declaration().parameters().ref_(arena)).copied(),
    }) else {
        return false;
    };
    is_rest_parameter(last, arena)
}

pub fn is_rest_parameter(node: Id<Node> /*ParameterDeclaration | JSDocParameterTag*/, arena: &impl HasArena) -> bool {
    let type_: Option<Id<Node>> = if is_jsdoc_parameter_tag(&node.ref_(arena)) {
        node.ref_(arena).as_jsdoc_property_like_tag()
            .type_expression
            .map(|type_expression| type_expression.ref_(arena).as_jsdoc_type_expression().type_)
    } else {
        node.ref_(arena).as_parameter_declaration().maybe_type()
    };
    node.ref_(arena).kind() == SyntaxKind::Parameter
        && node.ref_(arena).as_parameter_declaration().dot_dot_dot_token.is_some()
        || type_
            .filter(|type_| type_.ref_(arena).kind() == SyntaxKind::JSDocVariadicType)
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

pub fn get_assignment_target_kind(mut node: Id<Node>, arena: &impl HasArena) -> AssignmentKind {
    let mut parent = node.ref_(arena).parent();
    loop {
        match parent.ref_(arena).kind() {
            SyntaxKind::BinaryExpression => {
                let parent_ref = parent.ref_(arena);
                let parent_as_binary_expression = parent_ref.as_binary_expression();
                let binary_operator = parent_as_binary_expression.operator_token.ref_(arena).kind();
                return if is_assignment_operator(binary_operator)
                    && parent_as_binary_expression.left == node
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
                let unary_operator = parent.ref_(arena).as_prefix_unary_expression().operator;
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
                let unary_operator = parent.ref_(arena).as_postfix_unary_expression().operator;
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
                return if parent.ref_(arena).as_for_in_statement().initializer == node {
                    AssignmentKind::Definite
                } else {
                    AssignmentKind::None
                };
            }
            SyntaxKind::ForOfStatement => {
                return if parent.ref_(arena).as_for_of_statement().initializer == node {
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
                node = parent.ref_(arena).parent();
            }
            SyntaxKind::ShorthandPropertyAssignment => {
                if parent.ref_(arena).as_shorthand_property_assignment().name() != node {
                    return AssignmentKind::None;
                }
                node = parent.ref_(arena).parent();
            }
            SyntaxKind::PropertyAssignment => {
                if parent.ref_(arena).as_property_assignment().name() == node {
                    return AssignmentKind::None;
                }
                node = parent.ref_(arena).parent();
            }
            _ => {
                return AssignmentKind::None;
            }
        }
        parent = node.ref_(arena).parent();
    }
}

pub fn is_assignment_target(node: Id<Node>, arena: &impl HasArena) -> bool {
    get_assignment_target_kind(node, arena) != AssignmentKind::None
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

fn walk_up(node: Id<Node>, kind: SyntaxKind, arena: &impl HasArena) -> Option<Id<Node>> {
    let mut node = Some(node);
    loop {
        if let Some(node_present) = node {
            if node_present.ref_(arena).kind() == kind {
                node = node_present.ref_(arena).maybe_parent();
            } else {
                break;
            }
        } else {
            break;
        }
    }
    node
}

pub fn walk_up_parenthesized_types(node: Id<Node>, arena: &impl HasArena) -> Option<Id<Node>> {
    walk_up(node, SyntaxKind::ParenthesizedType, arena)
}

pub fn walk_up_parenthesized_expressions(node: Id<Node>, arena: &impl HasArena) -> Option<Id<Node>> {
    walk_up(node, SyntaxKind::ParenthesizedExpression, arena)
}

pub fn walk_up_parenthesized_types_and_get_parent_and_child(
    node: Id<Node>,
    arena: &impl HasArena,
) -> (Option<Id<Node /*ParenthesizedTypeNode*/>>, Option<Id<Node>>) {
    let mut child: Option<Id<Node>> = None;
    let mut node: Option<Id<Node>> = Some(node);
    while matches!(
        node,
        Some(node) if node.ref_(arena).kind() == SyntaxKind::ParenthesizedType
    ) {
        let node_parent = node.unwrap().ref_(arena).maybe_parent();
        child = node;
        node = node_parent;
    }
    (child, node)
}

pub fn skip_parentheses(node: Id<Node>, exclude_jsdoc_type_assertions: Option<bool>, arena: &impl HasArena) -> Id<Node> {
    let exclude_jsdoc_type_assertions = exclude_jsdoc_type_assertions.unwrap_or(false);
    let flags = if exclude_jsdoc_type_assertions {
        OuterExpressionKinds::Parentheses | OuterExpressionKinds::ExcludeJSDocTypeAssertion
    } else {
        OuterExpressionKinds::Parentheses
    };
    skip_outer_expressions(node, Some(flags), arena)
}

pub fn is_delete_target(node: Id<Node>, arena: &impl HasArena) -> bool {
    if !matches!(
        node.ref_(arena).kind(),
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression
    ) {
        return false;
    }
    let node = walk_up_parenthesized_expressions(node.ref_(arena).parent(), arena);
    matches!(
        node,
        Some(node) if node.ref_(arena).kind() == SyntaxKind::DeleteExpression
    )
}

pub fn is_node_descendant_of(node: Id<Node>, ancestor: Option<Id<Node>>, arena: &impl HasArena) -> bool {
    maybe_is_node_descendant_of(Some(node), ancestor, arena)
}

pub fn maybe_is_node_descendant_of(
    mut node: Option<Id<Node>>,
    ancestor: Option<Id<Node>>,
    arena: &impl HasArena,
) -> bool {
    let Some(ancestor) = ancestor else {
        return false;
    };
    while let Some(node_present) = node {
        if node_present == ancestor {
            return true;
        }
        node = node_present.ref_(arena).maybe_parent();
    }
    false
}

pub fn is_declaration_name(name: Id<Node>, arena: &impl HasArena) -> bool {
    !is_source_file(&name.ref_(arena))
        && !is_binding_pattern(Some(&name.ref_(arena)))
        && is_declaration(name.ref_(arena).parent(), arena)
        && matches!(
            name.ref_(arena).parent().ref_(arena).as_named_declaration().maybe_name(),
            Some(parent_name) if parent_name == name
        )
}

pub fn get_declaration_from_name(name: Id<Node>, arena: &impl HasArena) -> Option<Id<Node /*Declaration*/>> {
    let parent = name.ref_(arena).parent();
    match name.ref_(arena).kind() {
        SyntaxKind::StringLiteral
        | SyntaxKind::NoSubstitutionTemplateLiteral
        | SyntaxKind::NumericLiteral => {
            if is_computed_property_name(&parent.ref_(arena)) {
                return parent.ref_(arena).maybe_parent();
            }
            if is_declaration(parent, arena) {
                if matches!(
                    parent.ref_(arena).as_named_declaration().maybe_name(),
                    Some(parent_name) if parent_name == name
                ) {
                    Some(parent)
                } else {
                    None
                }
            } else if is_qualified_name(&parent.ref_(arena)) {
                let tag = parent.ref_(arena).parent();
                if is_jsdoc_parameter_tag(&tag.ref_(arena))
                    && tag.ref_(arena).as_jsdoc_property_like_tag().name == parent
                {
                    Some(tag)
                } else {
                    None
                }
            } else {
                let bin_exp = parent.ref_(arena).parent();
                if is_binary_expression(&bin_exp.ref_(arena))
                    && get_assignment_declaration_kind(bin_exp, arena) != AssignmentDeclarationKind::None
                    && (bin_exp.ref_(arena).as_binary_expression().left.ref_(arena).maybe_symbol().is_some()
                        || bin_exp.ref_(arena).maybe_symbol().is_some())
                    && matches!(
                        get_name_of_declaration(Some(bin_exp), arena),
                        Some(name_of_declaration) if name_of_declaration == name
                    )
                {
                    Some(bin_exp)
                } else {
                    None
                }
            }
        }
        SyntaxKind::Identifier => {
            if is_declaration(parent, arena) {
                if matches!(
                    parent.ref_(arena).as_named_declaration().maybe_name(),
                    Some(parent_name) if parent_name == name
                ) {
                    Some(parent)
                } else {
                    None
                }
            } else if is_qualified_name(&parent.ref_(arena)) {
                let tag = parent.ref_(arena).parent();
                if is_jsdoc_parameter_tag(&tag.ref_(arena))
                    && tag.ref_(arena).as_jsdoc_property_like_tag().name == parent
                {
                    Some(tag)
                } else {
                    None
                }
            } else {
                let bin_exp = parent.ref_(arena).parent();
                if is_binary_expression(&bin_exp.ref_(arena))
                    && get_assignment_declaration_kind(bin_exp, arena) != AssignmentDeclarationKind::None
                    && (bin_exp.ref_(arena).as_binary_expression().left.ref_(arena).maybe_symbol().is_some()
                        || bin_exp.ref_(arena).maybe_symbol().is_some())
                    && matches!(
                        get_name_of_declaration(Some(bin_exp), arena),
                        Some(name_of_declaration) if name_of_declaration == name
                    )
                {
                    Some(bin_exp)
                } else {
                    None
                }
            }
        }
        SyntaxKind::PrivateIdentifier => {
            if is_declaration(parent, arena)
                && matches!(
                    parent.ref_(arena).as_named_declaration().maybe_name(),
                    Some(parent_name) if parent_name == name
                )
            {
                Some(parent)
            } else {
                None
            }
        }
        _ => None,
    }
}
