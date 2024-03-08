use id_arena::Id;

use crate::{
    find, find_ancestor, first_defined, get_first_constructor_with_body, get_text_of_property_name,
    is_array_literal_expression, is_binary_expression, is_class_declaration, is_class_element,
    is_class_like, is_class_static_block_declaration, is_entity_name_expression,
    is_expression_with_type_arguments_in_class_extends_clause, is_function_declaration,
    is_function_like, is_function_like_declaration,
    is_function_like_or_class_static_block_declaration, is_identifier, is_jsdoc_link_like,
    is_jsdoc_member_name, is_jsdoc_name_reference, is_named_declaration,
    is_object_literal_expression, is_private_identifier, is_property_assignment,
    is_shorthand_property_assignment, is_source_file, is_string_literal, is_variable_declaration,
    maybe_is_class_like, some, try_cast, Debug_, FindAncestorCallbackReturn, HasArena,
    HasInitializerInterface, HasMembersInterface, HasStatementsInterface, InArena,
    LiteralLikeNodeInterface, NamedDeclarationInterface, Node, NodeInterface, OptionInArena,
    SyntaxKind, TypePredicate, TypePredicateKind,
};

pub fn introduces_arguments_exotic_object(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::MethodDeclaration
            | SyntaxKind::MethodSignature
            | SyntaxKind::Constructor
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::FunctionExpression
    )
}

pub fn unwrap_innermost_statement_of_label(
    mut node: Id<Node>, /*LabeledStatement*/
    mut before_unwrap_label_callback: Option<impl FnMut(Id<Node>)>,
    arena: &impl HasArena,
) -> Id<Node /*Statement*/> {
    loop {
        if let Some(before_unwrap_label_callback) = before_unwrap_label_callback.as_mut() {
            before_unwrap_label_callback(node);
        }
        let node_ref = node.ref_(arena);
        let node_as_labeled_statement = node_ref.as_labeled_statement();
        if node_as_labeled_statement.statement.ref_(arena).kind() != SyntaxKind::LabeledStatement {
            return node_as_labeled_statement.statement;
        }
        node = node_as_labeled_statement.statement;
    }
}

pub fn is_function_block(node: Id<Node>, arena: &impl HasArena) -> bool {
    /*node &&*/
    node.ref_(arena).kind() == SyntaxKind::Block
        && is_function_like(node.ref_(arena).maybe_parent().refed(arena).as_deref())
}

pub fn is_object_literal_method(node: Id<Node>, arena: &impl HasArena) -> bool {
    /*node &&*/
    node.ref_(arena).kind() == SyntaxKind::MethodDeclaration
        && node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::ObjectLiteralExpression
}

pub fn is_object_literal_or_class_expression_method_or_accessor(
    node: Id<Node>,
    arena: &impl HasArena,
) -> bool {
    matches!(
        node.ref_(arena).kind(),
        SyntaxKind::MethodDeclaration | SyntaxKind::GetAccessor | SyntaxKind::SetAccessor
    ) && matches!(
        node.ref_(arena).parent().ref_(arena).kind(),
        SyntaxKind::ObjectLiteralExpression | SyntaxKind::ClassExpression
    )
}

pub fn is_identifier_type_predicate(predicate: &TypePredicate) -> bool {
    /*predicate &&*/
    predicate.kind == TypePredicateKind::Identifier
}

pub fn is_this_type_predicate(predicate: &TypePredicate) -> bool {
    /*predicate &&*/
    predicate.kind == TypePredicateKind::This
}

pub fn get_property_assignment(
    object_literal: Id<Node>, /*ObjectLiteralExpression*/
    key: &str,
    key2: Option<&str>,
    arena: &impl HasArena,
) -> Vec<Id<Node /*PropertyAssignment*/>> {
    object_literal
        .ref_(arena)
        .as_object_literal_expression()
        .properties
        .ref_(arena)
        .iter()
        .copied()
        .filter(|&property| {
            if property.ref_(arena).kind() == SyntaxKind::PropertyAssignment {
                let property_name = property.ref_(arena).as_property_assignment().name();
                let prop_name = get_text_of_property_name(property_name, arena);
                return prop_name == key || matches!(key2, Some(key2) if prop_name == key2);
            }
            false
        })
        .collect()
}

pub fn get_property_array_element_value(
    object_literal: Id<Node>, /*ObjectLiteralExpression*/
    prop_key: &str,
    element_value: &str,
    arena: &impl HasArena,
) -> Option<Id<Node /*StringLiteral*/>> {
    first_defined(
        get_property_assignment(object_literal, prop_key, None, arena),
        |property, _| {
            let property_ref = property.ref_(arena);
            let property_as_property_assignment = property_ref.as_property_assignment();
            if is_array_literal_expression(
                &property_as_property_assignment
                    .maybe_initializer()
                    .unwrap()
                    .ref_(arena),
            ) {
                find(
                    &property_as_property_assignment
                        .maybe_initializer()
                        .unwrap()
                        .ref_(arena)
                        .as_array_literal_expression()
                        .elements
                        .ref_(arena),
                    |element, _| {
                        is_string_literal(&element.ref_(arena))
                            && &*element.ref_(arena).as_string_literal().text() == element_value
                    },
                )
                .copied()
            } else {
                None
            }
        },
    )
}

pub fn get_ts_config_object_literal_expression(
    ts_config_source_file: Option<Id<Node> /*TsConfigSourceFile*/>,
    arena: &impl HasArena,
) -> Option<Id<Node /*ObjectLiteralExpression*/>> {
    let ts_config_source_file = ts_config_source_file?;
    let ts_config_source_file_ref = ts_config_source_file.ref_(arena);
    let ts_config_source_file_as_source_file = ts_config_source_file_ref.as_source_file();
    if !ts_config_source_file_as_source_file
        .statements()
        .ref_(arena)
        .is_empty()
    {
        let expression = ts_config_source_file_as_source_file
            .statements()
            .ref_(arena)[0]
            .ref_(arena)
            .as_expression_statement()
            .expression;
        return try_cast(expression, |expression| {
            is_object_literal_expression(&expression.ref_(arena))
        });
    }
    None
}

pub fn get_ts_config_prop_array_element_value(
    ts_config_source_file: Option<Id<Node> /*TsConfigSourceFile*/>,
    prop_key: &str,
    element_value: &str,
    arena: &impl HasArena,
) -> Option<Id<Node /*StringLiteral*/>> {
    first_defined(
        get_ts_config_prop_array(ts_config_source_file, prop_key, arena),
        |property, _| {
            let property_ref = property.ref_(arena);
            let property_as_property_assignment = property_ref.as_property_assignment();
            if is_array_literal_expression(
                &property_as_property_assignment
                    .maybe_initializer()
                    .unwrap()
                    .ref_(arena),
            ) {
                find(
                    &property_as_property_assignment
                        .maybe_initializer()
                        .unwrap()
                        .ref_(arena)
                        .as_array_literal_expression()
                        .elements
                        .ref_(arena),
                    |element, _| {
                        is_string_literal(&element.ref_(arena))
                            && &*element.ref_(arena).as_string_literal().text() == element_value
                    },
                )
                .copied()
            } else {
                None
            }
        },
    )
}

pub fn get_ts_config_prop_array(
    ts_config_source_file: Option<Id<Node> /*TsConfigSourceFile*/>,
    prop_key: &str,
    arena: &impl HasArena,
) -> Vec<Id<Node /*PropertyAssignment*/>> {
    let json_object_literal = get_ts_config_object_literal_expression(ts_config_source_file, arena);
    match json_object_literal {
        Some(json_object_literal) => {
            get_property_assignment(json_object_literal, prop_key, None, arena)
        }
        None => Default::default(),
    }
}

pub fn get_containing_function(
    node: Id<Node>,
    arena: &impl HasArena,
) -> Option<Id<Node /*SignatureDeclaration*/>> {
    find_ancestor(
        node.ref_(arena).maybe_parent(),
        |node: Id<Node>| is_function_like(Some(&node.ref_(arena))),
        arena,
    )
}

pub fn get_containing_function_declaration(
    node: Id<Node>,
    arena: &impl HasArena,
) -> Option<Id<Node /*FunctionLikeDeclaration*/>> {
    find_ancestor(
        node.ref_(arena).maybe_parent(),
        |node: Id<Node>| is_function_like_declaration(&node.ref_(arena)),
        arena,
    )
}

pub fn get_containing_class(
    node: Id<Node>,
    arena: &impl HasArena,
) -> Option<Id<Node /*ClassLikeDeclaration*/>> {
    find_ancestor(
        node.ref_(arena).maybe_parent(),
        |node: Id<Node>| is_class_like(&node.ref_(arena)),
        arena,
    )
}

pub fn get_containing_class_static_block(
    node: Id<Node>,
    arena: &impl HasArena,
) -> Option<Id<Node>> {
    find_ancestor(
        node.ref_(arena).maybe_parent(),
        |n: Id<Node>| {
            if is_class_like(&n.ref_(arena)) || is_function_like(Some(&n.ref_(arena))) {
                return FindAncestorCallbackReturn::Quit;
            }
            is_class_static_block_declaration(&n.ref_(arena)).into()
        },
        arena,
    )
}

pub fn get_containing_function_or_class_static_block(
    node: Id<Node>,
    arena: &impl HasArena,
) -> Option<Id<Node /*SignatureDeclaration | ClassStaticBlockDeclaration*/>> {
    find_ancestor(
        node.ref_(arena).maybe_parent(),
        |node: Id<Node>| {
            is_function_like_or_class_static_block_declaration(Some(&node.ref_(arena)))
        },
        arena,
    )
}

pub fn get_this_container(
    mut node: Id<Node>,
    include_arrow_functions: bool,
    arena: &impl HasArena,
) -> Id<Node> {
    Debug_.assert(node.ref_(arena).kind() != SyntaxKind::SourceFile, None);
    loop {
        let Some(parent) = node.ref_(arena).maybe_parent() else {
            Debug_.fail(None);
        };
        node = parent;
        match node.ref_(arena).kind() {
            SyntaxKind::ComputedPropertyName => {
                if maybe_is_class_like(
                    node.ref_(arena)
                        .parent()
                        .ref_(arena)
                        .maybe_parent()
                        .refed(arena)
                        .as_deref(),
                ) {
                    return node;
                }
                node = node.ref_(arena).parent();
            }
            SyntaxKind::Decorator => {
                if node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::Parameter
                    && is_class_element(&node.ref_(arena).parent().ref_(arena).parent().ref_(arena))
                {
                    node = node.ref_(arena).parent().ref_(arena).parent();
                } else if is_class_element(&node.ref_(arena).parent().ref_(arena)) {
                    node = node.ref_(arena).parent();
                }
            }
            SyntaxKind::ArrowFunction => {
                if !include_arrow_functions {
                    continue;
                }
                return node;
            }
            SyntaxKind::FunctionDeclaration
            | SyntaxKind::FunctionExpression
            | SyntaxKind::ModuleDeclaration
            | SyntaxKind::ClassStaticBlockDeclaration
            | SyntaxKind::PropertyDeclaration
            | SyntaxKind::PropertySignature
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::MethodSignature
            | SyntaxKind::Constructor
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::CallSignature
            | SyntaxKind::ConstructSignature
            | SyntaxKind::IndexSignature
            | SyntaxKind::EnumDeclaration
            | SyntaxKind::SourceFile => {
                return node;
            }
            _ => (),
        }
    }
}

pub fn is_in_top_level_context(mut node: Id<Node>, arena: &impl HasArena) -> bool {
    if is_identifier(&node.ref_(arena))
        && (is_class_declaration(&node.ref_(arena).parent().ref_(arena))
            || is_function_declaration(&node.ref_(arena).parent().ref_(arena)))
        && matches!(
            node.ref_(arena).parent().ref_(arena).as_named_declaration().maybe_name(),
            Some(node_parent_name) if node_parent_name == node
        )
    {
        node = node.ref_(arena).parent();
    }
    let container = get_this_container(node, true, arena);
    is_source_file(&container.ref_(arena))
}

pub fn get_new_target_container(node: Id<Node>, arena: &impl HasArena) -> Option<Id<Node>> {
    let container = get_this_container(node, false, arena);
    // if (container) {
    match container.ref_(arena).kind() {
        SyntaxKind::Constructor
        | SyntaxKind::FunctionDeclaration
        | SyntaxKind::FunctionExpression => {
            return Some(container);
        }
        _ => (),
    }
    // }
    None
}

pub fn get_super_container(
    mut node: Id<Node>,
    stop_on_functions: bool,
    arena: &impl HasArena,
) -> Option<Id<Node>> {
    loop {
        node = node.ref_(arena).maybe_parent()?;
        match node.ref_(arena).kind() {
            SyntaxKind::ComputedPropertyName => {
                node = node.ref_(arena).parent();
            }
            SyntaxKind::FunctionDeclaration
            | SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction => {
                if !stop_on_functions {
                    continue;
                }
                return Some(node);
            }
            SyntaxKind::PropertyDeclaration
            | SyntaxKind::PropertySignature
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::MethodSignature
            | SyntaxKind::Constructor
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::ClassStaticBlockDeclaration => {
                return Some(node);
            }
            SyntaxKind::Decorator => {
                if node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::Parameter
                    && is_class_element(&node.ref_(arena).parent().ref_(arena).parent().ref_(arena))
                {
                    node = node.ref_(arena).parent().ref_(arena).parent();
                } else if is_class_element(&node.ref_(arena).parent().ref_(arena)) {
                    node = node.ref_(arena).parent();
                }
            }
            _ => (),
        }
    }
}

pub fn get_immediately_invoked_function_expression(
    func: Id<Node>,
    arena: &impl HasArena,
) -> Option<Id<Node /*CallExpression*/>> {
    if matches!(
        func.ref_(arena).kind(),
        SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction
    ) {
        let mut prev = func;
        let mut parent = func.ref_(arena).parent();
        while parent.ref_(arena).kind() == SyntaxKind::ParenthesizedExpression {
            prev = parent;
            parent = parent.ref_(arena).parent();
        }
        if parent.ref_(arena).kind() == SyntaxKind::CallExpression
            && parent.ref_(arena).as_call_expression().expression == prev
        {
            return Some(parent);
        }
    }
    None
}

pub fn is_super_or_super_property(node: Id<Node>, arena: &impl HasArena) -> bool {
    node.ref_(arena).kind() == SyntaxKind::SuperKeyword || is_super_property(node, arena)
}

pub fn is_super_property(node: Id<Node>, arena: &impl HasArena) -> bool {
    matches!(
        node.ref_(arena).kind(),
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression
    ) && node
        .ref_(arena)
        .as_has_expression()
        .expression()
        .ref_(arena)
        .kind()
        == SyntaxKind::SuperKeyword
}

pub fn is_this_property(node: Id<Node>, arena: &impl HasArena) -> bool {
    matches!(
        node.ref_(arena).kind(),
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression
    ) && node
        .ref_(arena)
        .as_has_expression()
        .expression()
        .ref_(arena)
        .kind()
        == SyntaxKind::ThisKeyword
}

pub fn is_this_initialized_declaration(node: Option<Id<Node>>, arena: &impl HasArena) -> bool {
    let Some(node) = node else {
        return false;
    };
    is_variable_declaration(&node.ref_(arena))
        && matches!(
            node.ref_(arena).as_variable_declaration().maybe_initializer(),
            Some(initializer) if initializer.ref_(arena).kind() == SyntaxKind::ThisKeyword
        )
}

pub fn is_this_initialized_object_binding_expression(
    node: Option<Id<Node>>,
    arena: &impl HasArena,
) -> bool {
    let Some(node) = node else {
        return false;
    };
    if !(is_shorthand_property_assignment(&node.ref_(arena))
        || is_property_assignment(&node.ref_(arena)))
    {
        return false;
    }
    let node_parent_parent = node.ref_(arena).parent().ref_(arena).parent();
    if !is_binary_expression(&node_parent_parent.ref_(arena)) {
        return false;
    }
    let node_parent_parent_ref = node_parent_parent.ref_(arena);
    let node_parent_parent_as_binary_expression = node_parent_parent_ref.as_binary_expression();
    node_parent_parent_as_binary_expression
        .operator_token
        .ref_(arena)
        .kind()
        == SyntaxKind::EqualsToken
        && node_parent_parent_as_binary_expression
            .right
            .ref_(arena)
            .kind()
            == SyntaxKind::ThisKeyword
}

pub fn get_entity_name_from_type_node(
    node: Id<Node>, /*TypeNode*/
    arena: &impl HasArena,
) -> Option<Id<Node /*EntityNameOrEntityNameExpression*/>> {
    match node.ref_(arena).kind() {
        SyntaxKind::TypeReference => {
            return Some(node.ref_(arena).as_type_reference_node().type_name);
        }
        SyntaxKind::ExpressionWithTypeArguments => {
            let node_ref = node.ref_(arena);
            let node_as_expression_with_type_arguments =
                node_ref.as_expression_with_type_arguments();
            return if is_entity_name_expression(
                node_as_expression_with_type_arguments.expression,
                arena,
            ) {
                Some(node_as_expression_with_type_arguments.expression)
            } else {
                None
            };
        }

        SyntaxKind::Identifier | SyntaxKind::QualifiedName => {
            return Some(node);
        }
        _ => (),
    }

    None
}

pub fn get_invoked_expression(node: &Node /*CallLikeExpression*/) -> Id<Node /*Expression*/> {
    match node.kind() {
        SyntaxKind::TaggedTemplateExpression => node.as_tagged_template_expression().tag,
        SyntaxKind::JsxOpeningElement => node.as_jsx_opening_element().tag_name,
        SyntaxKind::JsxSelfClosingElement => node.as_jsx_self_closing_element().tag_name,
        _ => node.as_has_expression().expression(),
    }
}

pub fn node_can_be_decorated(
    node: Id<Node>,
    parent: Option<Id<Node>>,
    grandparent: Option<Id<Node>>,
    arena: &impl HasArena,
) -> bool {
    if is_named_declaration(&node.ref_(arena))
        && is_private_identifier(&node.ref_(arena).as_named_declaration().name().ref_(arena))
    {
        return false;
    }
    match node.ref_(arena).kind() {
        SyntaxKind::ClassDeclaration => true,

        SyntaxKind::PropertyDeclaration => {
            let parent = parent.unwrap();
            parent.ref_(arena).kind() == SyntaxKind::ClassDeclaration
        }

        SyntaxKind::GetAccessor | SyntaxKind::SetAccessor | SyntaxKind::MethodDeclaration => {
            if !node
                .ref_(arena)
                .as_function_like_declaration()
                .maybe_body()
                .is_some()
            {
                return false;
            }
            let parent = parent.unwrap();
            parent.ref_(arena).kind() == SyntaxKind::ClassDeclaration
        }

        SyntaxKind::Parameter => {
            let parent = parent.unwrap();
            if !(parent
                .ref_(arena)
                .as_function_like_declaration()
                .maybe_body()
                .is_some()
                && matches!(
                    parent.ref_(arena).kind(),
                    SyntaxKind::Constructor
                        | SyntaxKind::MethodDeclaration
                        | SyntaxKind::SetAccessor
                ))
            {
                return false;
            }
            let grandparent = grandparent.unwrap();
            grandparent.ref_(arena).kind() == SyntaxKind::ClassDeclaration
        }
        _ => false,
    }
}

pub fn node_is_decorated(
    node: Id<Node>,
    parent: Option<Id<Node>>,
    grandparent: Option<Id<Node>>,
    arena: &impl HasArena,
) -> bool {
    node.ref_(arena).maybe_decorators().is_some()
        && node_can_be_decorated(node, parent, grandparent, arena)
}

pub fn node_or_child_is_decorated(
    node: Id<Node>,
    parent: Option<Id<Node>>,
    grandparent: Option<Id<Node>>,
    arena: &impl HasArena,
) -> bool {
    node_is_decorated(node, parent, grandparent, arena) || child_is_decorated(node, parent, arena)
}

pub fn child_is_decorated(node: Id<Node>, parent: Option<Id<Node>>, arena: &impl HasArena) -> bool {
    match node.ref_(arena).kind() {
        SyntaxKind::ClassDeclaration => some(
            Some(
                &*node
                    .ref_(arena)
                    .as_class_declaration()
                    .members()
                    .ref_(arena),
            ),
            Some(|&m: &Id<Node>| node_or_child_is_decorated(m, Some(node), parent, arena)),
        ),
        SyntaxKind::MethodDeclaration | SyntaxKind::SetAccessor | SyntaxKind::Constructor => some(
            Some(
                &*node
                    .ref_(arena)
                    .as_function_like_declaration()
                    .parameters()
                    .ref_(arena),
            ),
            Some(|&p: &Id<Node>| node_is_decorated(p, Some(node), parent, arena)),
        ),
        _ => false,
    }
}

pub fn class_or_constructor_parameter_is_decorated(
    node: Id<Node>, /*ClassDeclaration*/
    arena: &impl HasArena,
) -> bool {
    if node_is_decorated(node, None, None, arena) {
        return true;
    }
    let constructor = get_first_constructor_with_body(node, arena);
    match constructor {
        Some(constructor) => child_is_decorated(constructor, Some(node), arena),
        None => false,
    }
}

pub fn is_jsx_tag_name(node: Id<Node>, arena: &impl HasArena) -> bool {
    let parent = node.ref_(arena).parent();
    match parent.ref_(arena).kind() {
        SyntaxKind::JsxOpeningElement => {
            parent.ref_(arena).as_jsx_opening_element().tag_name == node
        }
        SyntaxKind::JsxSelfClosingElement => {
            parent.ref_(arena).as_jsx_self_closing_element().tag_name == node
        }
        SyntaxKind::JsxClosingElement => {
            parent.ref_(arena).as_jsx_closing_element().tag_name == node
        }
        _ => false,
    }
}

pub fn is_expression_node(mut node: Id<Node>, arena: &impl HasArena) -> bool {
    match node.ref_(arena).kind() {
        SyntaxKind::SuperKeyword
        | SyntaxKind::NullKeyword
        | SyntaxKind::TrueKeyword
        | SyntaxKind::FalseKeyword
        | SyntaxKind::RegularExpressionLiteral
        | SyntaxKind::ArrayLiteralExpression
        | SyntaxKind::ObjectLiteralExpression
        | SyntaxKind::PropertyAccessExpression
        | SyntaxKind::ElementAccessExpression
        | SyntaxKind::CallExpression
        | SyntaxKind::NewExpression
        | SyntaxKind::TaggedTemplateExpression
        | SyntaxKind::AsExpression
        | SyntaxKind::TypeAssertionExpression
        | SyntaxKind::NonNullExpression
        | SyntaxKind::ParenthesizedExpression
        | SyntaxKind::FunctionExpression
        | SyntaxKind::ClassExpression
        | SyntaxKind::ArrowFunction
        | SyntaxKind::VoidExpression
        | SyntaxKind::DeleteExpression
        | SyntaxKind::TypeOfExpression
        | SyntaxKind::PrefixUnaryExpression
        | SyntaxKind::PostfixUnaryExpression
        | SyntaxKind::BinaryExpression
        | SyntaxKind::ConditionalExpression
        | SyntaxKind::SpreadElement
        | SyntaxKind::TemplateExpression
        | SyntaxKind::OmittedExpression
        | SyntaxKind::JsxElement
        | SyntaxKind::JsxSelfClosingElement
        | SyntaxKind::JsxFragment
        | SyntaxKind::YieldExpression
        | SyntaxKind::AwaitExpression
        | SyntaxKind::MetaProperty => true,
        SyntaxKind::QualifiedName => {
            while node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::QualifiedName {
                node = node.ref_(arena).parent();
            }
            node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::TypeQuery
                || is_jsdoc_link_like(&node.ref_(arena).parent().ref_(arena))
                || is_jsdoc_name_reference(&node.ref_(arena).parent().ref_(arena))
                || is_jsdoc_member_name(&node.ref_(arena).parent().ref_(arena))
                || is_jsx_tag_name(node, arena)
        }
        SyntaxKind::JSDocMemberName => {
            while is_jsdoc_member_name(&node.ref_(arena).parent().ref_(arena)) {
                node = node.ref_(arena).parent();
            }
            node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::TypeQuery
                || is_jsdoc_link_like(&node.ref_(arena).parent().ref_(arena))
                || is_jsdoc_name_reference(&node.ref_(arena).parent().ref_(arena))
                || is_jsdoc_member_name(&node.ref_(arena).parent().ref_(arena))
                || is_jsx_tag_name(node, arena)
        }
        SyntaxKind::PrivateIdentifier => {
            let node_parent = node.ref_(arena).parent();
            if !is_binary_expression(&node_parent.ref_(arena)) {
                return false;
            }
            let node_parent_ref = node_parent.ref_(arena);
            let node_parent_as_binary_expression = node_parent_ref.as_binary_expression();
            node_parent_as_binary_expression.left == node
                && node_parent_as_binary_expression
                    .operator_token
                    .ref_(arena)
                    .kind()
                    == SyntaxKind::InKeyword
        }
        SyntaxKind::Identifier => {
            if node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::TypeQuery
                || is_jsdoc_link_like(&node.ref_(arena).parent().ref_(arena))
                || is_jsdoc_name_reference(&node.ref_(arena).parent().ref_(arena))
                || is_jsdoc_member_name(&node.ref_(arena).parent().ref_(arena))
                || is_jsx_tag_name(node, arena)
            {
                return true;
            }
            is_in_expression_context(node, arena)
        }
        SyntaxKind::NumericLiteral
        | SyntaxKind::BigIntLiteral
        | SyntaxKind::StringLiteral
        | SyntaxKind::NoSubstitutionTemplateLiteral
        | SyntaxKind::ThisKeyword => is_in_expression_context(node, arena),
        _ => false,
    }
}

pub fn is_in_expression_context(node: Id<Node>, arena: &impl HasArena) -> bool {
    let parent = node.ref_(arena).parent();
    match parent.ref_(arena).kind() {
        SyntaxKind::VariableDeclaration
        | SyntaxKind::Parameter
        | SyntaxKind::PropertyDeclaration
        | SyntaxKind::PropertySignature
        | SyntaxKind::EnumMember
        | SyntaxKind::PropertyAssignment
        | SyntaxKind::BindingElement => {
            parent.ref_(arena).as_has_initializer().maybe_initializer() == Some(node)
        }
        SyntaxKind::ExpressionStatement => {
            parent.ref_(arena).as_expression_statement().expression == node
        }
        SyntaxKind::IfStatement => parent.ref_(arena).as_if_statement().expression == node,
        SyntaxKind::DoStatement => parent.ref_(arena).as_do_statement().expression == node,
        SyntaxKind::WhileStatement => parent.ref_(arena).as_while_statement().expression == node,
        SyntaxKind::ReturnStatement => {
            parent.ref_(arena).as_return_statement().expression == Some(node)
        }
        SyntaxKind::WithStatement => parent.ref_(arena).as_with_statement().expression == node,
        SyntaxKind::SwitchStatement => parent.ref_(arena).as_switch_statement().expression == node,
        SyntaxKind::CaseClause => parent.ref_(arena).as_case_clause().expression == node,
        SyntaxKind::ThrowStatement => parent.ref_(arena).as_throw_statement().expression == node,
        SyntaxKind::ForStatement => {
            let parent_ref = parent.ref_(arena);
            let for_statement = parent_ref.as_for_statement();
            matches!(
                for_statement.initializer,
                Some(initializer) if initializer == node && initializer.ref_(arena).kind() != SyntaxKind::VariableDeclarationList
            ) || for_statement.condition == Some(node)
                || for_statement.incrementor == Some(node)
        }
        SyntaxKind::ForInStatement => {
            let parent_ref = parent.ref_(arena);
            let for_in_statement = parent_ref.as_for_in_statement();
            for_in_statement.initializer == node
                && for_in_statement.initializer.ref_(arena).kind()
                    != SyntaxKind::VariableDeclarationList
                || for_in_statement.expression == node
        }
        SyntaxKind::ForOfStatement => {
            let parent_ref = parent.ref_(arena);
            let for_of_statement = parent_ref.as_for_of_statement();
            for_of_statement.initializer == node
                && for_of_statement.initializer.ref_(arena).kind()
                    != SyntaxKind::VariableDeclarationList
                || for_of_statement.expression == node
        }
        SyntaxKind::TypeAssertionExpression => {
            node == parent.ref_(arena).as_type_assertion().expression
        }
        SyntaxKind::AsExpression => node == parent.ref_(arena).as_as_expression().expression,
        SyntaxKind::TemplateSpan => node == parent.ref_(arena).as_template_span().expression,
        SyntaxKind::ComputedPropertyName => {
            node == parent.ref_(arena).as_computed_property_name().expression
        }
        SyntaxKind::Decorator
        | SyntaxKind::JsxExpression
        | SyntaxKind::JsxSpreadAttribute
        | SyntaxKind::SpreadAssignment => true,
        SyntaxKind::ExpressionWithTypeArguments => {
            parent
                .ref_(arena)
                .as_expression_with_type_arguments()
                .expression
                == node
                && is_expression_with_type_arguments_in_class_extends_clause(parent, arena)
        }
        SyntaxKind::ShorthandPropertyAssignment => {
            parent
                .ref_(arena)
                .as_shorthand_property_assignment()
                .object_assignment_initializer
                == Some(node)
        }
        _ => is_expression_node(parent, arena),
    }
}
