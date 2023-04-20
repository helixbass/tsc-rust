#![allow(non_upper_case_globals)]

use gc::Gc;
use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

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
    maybe_is_class_like, some, try_cast, ClassLikeDeclarationInterface, Debug_,
    FindAncestorCallbackReturn, HasInitializerInterface, HasStatementsInterface,
    LiteralLikeNodeInterface, NamedDeclarationInterface, Node, NodeInterface, SyntaxKind,
    TypePredicate, TypePredicateKind,
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
    node: &Node, /*LabeledStatement*/
    mut before_unwrap_label_callback: Option<impl FnMut(&Node)>,
) -> Gc<Node /*Statement*/> {
    let mut node = node.node_wrapper();
    loop {
        if let Some(before_unwrap_label_callback) = before_unwrap_label_callback.as_mut() {
            before_unwrap_label_callback(&node);
        }
        let node_as_labeled_statement = node.as_labeled_statement();
        if node_as_labeled_statement.statement.kind() != SyntaxKind::LabeledStatement {
            return node_as_labeled_statement.statement.clone();
        }
        node = node_as_labeled_statement.statement.clone();
    }
}

pub fn is_function_block(node: &Node) -> bool {
    /*node &&*/
    node.kind() == SyntaxKind::Block && is_function_like(node.maybe_parent())
}

pub fn is_object_literal_method(node: &Node) -> bool {
    /*node &&*/
    node.kind() == SyntaxKind::MethodDeclaration
        && node.parent().kind() == SyntaxKind::ObjectLiteralExpression
}

pub fn is_object_literal_or_class_expression_method_or_accessor(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::MethodDeclaration | SyntaxKind::GetAccessor | SyntaxKind::SetAccessor
    ) && matches!(
        node.parent().kind(),
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
    object_literal: &Node, /*ObjectLiteralExpression*/
    key: &str,
    key2: Option<&str>,
) -> Vec<Gc<Node /*PropertyAssignment*/>> {
    object_literal
        .as_object_literal_expression()
        .properties
        .iter()
        .filter(|property| {
            if property.kind() == SyntaxKind::PropertyAssignment {
                let property_name = property.as_property_assignment().name();
                let prop_name = get_text_of_property_name(&property_name);
                return prop_name == key || matches!(key2, Some(key2) if prop_name == key2);
            }
            false
        })
        .map(Clone::clone)
        .collect()
}

pub fn get_property_array_element_value(
    object_literal: &Node, /*ObjectLiteralExpression*/
    prop_key: &str,
    element_value: &str,
) -> Option<Gc<Node /*StringLiteral*/>> {
    first_defined(
        &get_property_assignment(object_literal, prop_key, None),
        |property, _| {
            let property_as_property_assignment = property.as_property_assignment();
            if is_array_literal_expression(
                &property_as_property_assignment.maybe_initializer().unwrap(),
            ) {
                find(
                    &property_as_property_assignment
                        .maybe_initializer()
                        .unwrap()
                        .as_array_literal_expression()
                        .elements,
                    |element, _| {
                        is_string_literal(element)
                            && &*element.as_string_literal().text() == element_value
                    },
                )
                .map(Clone::clone)
            } else {
                None
            }
        },
    )
}

pub fn get_ts_config_object_literal_expression<TTsConfigSourceFile: Borrow<Node>>(
    ts_config_source_file: Option<TTsConfigSourceFile /*TsConfigSourceFile*/>,
) -> Option<Gc<Node /*ObjectLiteralExpression*/>> {
    if ts_config_source_file.is_none() {
        return None;
    }
    let ts_config_source_file = ts_config_source_file.unwrap();
    let ts_config_source_file = ts_config_source_file.borrow();
    let ts_config_source_file_as_source_file = ts_config_source_file.as_source_file();
    if !ts_config_source_file_as_source_file.statements().is_empty() {
        let ref expression = &ts_config_source_file_as_source_file.statements()[0]
            .as_expression_statement()
            .expression
            .clone();
        return try_cast(expression, |expression| {
            is_object_literal_expression(expression)
        })
        .map(|expression| expression.node_wrapper());
    }
    None
}

pub fn get_ts_config_prop_array_element_value<TTsConfigSourceFile: Borrow<Node>>(
    ts_config_source_file: Option<TTsConfigSourceFile /*TsConfigSourceFile*/>,
    prop_key: &str,
    element_value: &str,
) -> Option<Gc<Node /*StringLiteral*/>> {
    first_defined(
        &get_ts_config_prop_array(ts_config_source_file, prop_key),
        |property, _| {
            let property_as_property_assignment = property.as_property_assignment();
            if is_array_literal_expression(
                &property_as_property_assignment.maybe_initializer().unwrap(),
            ) {
                find(
                    &property_as_property_assignment
                        .maybe_initializer()
                        .unwrap()
                        .as_array_literal_expression()
                        .elements,
                    |element, _| {
                        is_string_literal(element)
                            && &*element.as_string_literal().text() == element_value
                    },
                )
                .map(Clone::clone)
            } else {
                None
            }
        },
    )
}

pub fn get_ts_config_prop_array<TTsConfigSourceFile: Borrow<Node>>(
    ts_config_source_file: Option<TTsConfigSourceFile /*TsConfigSourceFile*/>,
    prop_key: &str,
) -> Vec<Gc<Node /*PropertyAssignment*/>> {
    let json_object_literal = get_ts_config_object_literal_expression(ts_config_source_file);
    match json_object_literal {
        Some(json_object_literal) => get_property_assignment(&json_object_literal, prop_key, None),
        None => vec![],
    }
}

pub fn get_containing_function(node: &Node) -> Option<Gc<Node /*SignatureDeclaration*/>> {
    find_ancestor(node.maybe_parent(), |node: &Node| {
        is_function_like(Some(node))
    })
}

pub fn get_containing_function_declaration(
    node: &Node,
) -> Option<Gc<Node /*FunctionLikeDeclaration*/>> {
    find_ancestor(node.maybe_parent(), |node: &Node| {
        is_function_like_declaration(node)
    })
}

pub fn get_containing_class(node: &Node) -> Option<Gc<Node /*ClassLikeDeclaration*/>> {
    find_ancestor(node.maybe_parent(), |node: &Node| is_class_like(node))
}

pub fn get_containing_class_static_block(node: &Node) -> Option<Gc<Node>> {
    find_ancestor(node.maybe_parent(), |n: &Node| {
        if is_class_like(n) || is_function_like(Some(n)) {
            return FindAncestorCallbackReturn::Quit;
        }
        is_class_static_block_declaration(n).into()
    })
}

pub fn get_containing_function_or_class_static_block(
    node: &Node,
) -> Option<Gc<Node /*SignatureDeclaration | ClassStaticBlockDeclaration*/>> {
    find_ancestor(node.maybe_parent(), |node: &Node| {
        is_function_like_or_class_static_block_declaration(Some(node))
    })
}

pub fn get_this_container(node: &Node, include_arrow_functions: bool) -> Gc<Node> {
    Debug_.assert(node.kind() != SyntaxKind::SourceFile, None);
    let mut node = node.node_wrapper();
    loop {
        let maybe_parent = node.maybe_parent();
        if maybe_parent.is_none() {
            Debug_.fail(None);
        }
        node = maybe_parent.unwrap();
        match node.kind() {
            SyntaxKind::ComputedPropertyName => {
                if maybe_is_class_like(node.parent().maybe_parent()) {
                    return node;
                }
                node = node.parent();
            }
            SyntaxKind::Decorator => {
                if node.parent().kind() == SyntaxKind::Parameter
                    && is_class_element(&node.parent().parent())
                {
                    node = node.parent().parent();
                } else if is_class_element(&node.parent()) {
                    node = node.parent();
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

pub fn is_in_top_level_context(node: &Node) -> bool {
    let mut node = node.node_wrapper();
    if is_identifier(&node)
        && (is_class_declaration(&node.parent()) || is_function_declaration(&node.parent()))
        && matches!(node.parent().as_named_declaration().maybe_name(), Some(node_parent_name) if Gc::ptr_eq(&node_parent_name, &node))
    {
        node = node.parent();
    }
    let container = get_this_container(&node, true);
    is_source_file(&container)
}

pub fn get_new_target_container(node: &Node) -> Option<Gc<Node>> {
    let container = get_this_container(node, false);
    // if (container) {
    match container.kind() {
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

pub fn get_super_container(node: &Node, stop_on_functions: bool) -> Option<Gc<Node>> {
    let mut node = node.node_wrapper();
    loop {
        let maybe_parent = node.maybe_parent();
        if maybe_parent.is_none() {
            return None;
        }
        node = maybe_parent.unwrap();
        match node.kind() {
            SyntaxKind::ComputedPropertyName => {
                node = node.parent();
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
                if node.parent().kind() == SyntaxKind::Parameter
                    && is_class_element(&node.parent().parent())
                {
                    node = node.parent().parent();
                } else if is_class_element(&node.parent()) {
                    node = node.parent();
                }
            }
            _ => (),
        }
    }
}

pub fn get_immediately_invoked_function_expression(
    func: &Node,
) -> Option<Gc<Node /*CallExpression*/>> {
    if matches!(
        func.kind(),
        SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction
    ) {
        let mut prev = func.node_wrapper();
        let mut parent = func.parent();
        while parent.kind() == SyntaxKind::ParenthesizedExpression {
            prev = parent.clone();
            parent = parent.parent();
        }
        if parent.kind() == SyntaxKind::CallExpression
            && Gc::ptr_eq(&parent.as_call_expression().expression, &prev)
        {
            return Some(parent);
        }
    }
    None
}

pub fn is_super_or_super_property(node: &Node) -> bool {
    node.kind() == SyntaxKind::SuperKeyword || is_super_property(node)
}

pub fn is_super_property(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression
    ) && node.as_has_expression().expression().kind() == SyntaxKind::SuperKeyword
}

pub fn is_this_property(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression
    ) && node.as_has_expression().expression().kind() == SyntaxKind::ThisKeyword
}

pub fn is_this_initialized_declaration<TNode: Borrow<Node>>(node: Option<TNode>) -> bool {
    if node.is_none() {
        return false;
    }
    let node = node.unwrap();
    let node = node.borrow();
    is_variable_declaration(node)
        && matches!(node.as_variable_declaration().maybe_initializer(), Some(initializer) if initializer.kind() == SyntaxKind::ThisKeyword)
}

pub fn is_this_initialized_object_binding_expression<TNode: Borrow<Node>>(
    node: Option<TNode>,
) -> bool {
    if node.is_none() {
        return false;
    }
    let node = node.unwrap();
    let node = node.borrow();
    if !(is_shorthand_property_assignment(node) || is_property_assignment(node)) {
        return false;
    }
    let node_parent_parent = node.parent().parent();
    if !is_binary_expression(&node_parent_parent) {
        return false;
    }
    let node_parent_parent_as_binary_expression = node_parent_parent.as_binary_expression();
    node_parent_parent_as_binary_expression
        .operator_token
        .kind()
        == SyntaxKind::EqualsToken
        && node_parent_parent_as_binary_expression.right.kind() == SyntaxKind::ThisKeyword
}

pub fn get_entity_name_from_type_node(
    node: &Node, /*TypeNode*/
) -> Option<Gc<Node /*EntityNameOrEntityNameExpression*/>> {
    match node.kind() {
        SyntaxKind::TypeReference => {
            return Some(node.as_type_reference_node().type_name.clone());
        }
        SyntaxKind::ExpressionWithTypeArguments => {
            let node_as_expression_with_type_arguments = node.as_expression_with_type_arguments();
            return if is_entity_name_expression(&node_as_expression_with_type_arguments.expression)
            {
                Some(node_as_expression_with_type_arguments.expression.clone())
            } else {
                None
            };
        }

        SyntaxKind::Identifier | SyntaxKind::QualifiedName => {
            return Some(node.node_wrapper());
        }
        _ => (),
    }

    None
}

pub fn get_invoked_expression(node: &Node /*CallLikeExpression*/) -> Gc<Node /*Expression*/> {
    match node.kind() {
        SyntaxKind::TaggedTemplateExpression => node.as_tagged_template_expression().tag.clone(),
        SyntaxKind::JsxOpeningElement => node.as_jsx_opening_element().tag_name.clone(),
        SyntaxKind::JsxSelfClosingElement => node.as_jsx_self_closing_element().tag_name.clone(),
        _ => node.as_has_expression().expression(),
    }
}

pub fn node_can_be_decorated<TParent: Borrow<Node>, TGrandparent: Borrow<Node>>(
    node: &Node,
    parent: Option<TParent>,
    grandparent: Option<TGrandparent>,
) -> bool {
    if is_named_declaration(node) && is_private_identifier(&node.as_named_declaration().name()) {
        return false;
    }
    match node.kind() {
        SyntaxKind::ClassDeclaration => true,

        SyntaxKind::PropertyDeclaration => {
            let parent = parent.unwrap();
            let parent = parent.borrow();
            parent.kind() == SyntaxKind::ClassDeclaration
        }

        SyntaxKind::GetAccessor | SyntaxKind::SetAccessor | SyntaxKind::MethodDeclaration => {
            if !node.as_function_like_declaration().maybe_body().is_some() {
                return false;
            }
            let parent = parent.unwrap();
            let parent = parent.borrow();
            parent.kind() == SyntaxKind::ClassDeclaration
        }

        SyntaxKind::Parameter => {
            let parent = parent.unwrap();
            let parent = parent.borrow();
            if !(parent.as_function_like_declaration().maybe_body().is_some()
                && matches!(
                    parent.kind(),
                    SyntaxKind::Constructor
                        | SyntaxKind::MethodDeclaration
                        | SyntaxKind::SetAccessor
                ))
            {
                return false;
            }
            let grandparent = grandparent.unwrap();
            let grandparent = grandparent.borrow();
            grandparent.kind() == SyntaxKind::ClassDeclaration
        }
        _ => false,
    }
}

pub fn node_is_decorated<TParent: Borrow<Node>, TGrandparent: Borrow<Node>>(
    node: &Node,
    parent: Option<TParent>,
    grandparent: Option<TGrandparent>,
) -> bool {
    node.maybe_decorators().is_some() && node_can_be_decorated(node, parent, grandparent)
}

pub fn node_or_child_is_decorated(
    node: &Node,
    parent: Option<impl Borrow<Node> + Clone>,
    grandparent: Option<impl Borrow<Node>>,
) -> bool {
    node_is_decorated(node, parent.clone(), grandparent) || child_is_decorated(node, parent)
}

pub fn child_is_decorated(node: &Node, parent: Option<impl Borrow<Node> + Clone>) -> bool {
    match node.kind() {
        SyntaxKind::ClassDeclaration => some(
            Some(&node.as_class_declaration().members()),
            Some(|m: &Gc<Node>| node_or_child_is_decorated(m, Some(node), parent.clone())), // TODO: figure out how to not clone parent every time
        ),
        SyntaxKind::MethodDeclaration | SyntaxKind::SetAccessor | SyntaxKind::Constructor => some(
            Some(&node.as_function_like_declaration().parameters()),
            Some(|p: &Gc<Node>| node_is_decorated(p, Some(node), parent.clone())),
        ),
        _ => false,
    }
}

pub fn class_or_constructor_parameter_is_decorated(node: &Node /*ClassDeclaration*/) -> bool {
    if node_is_decorated(node, Option::<&Node>::None, Option::<&Node>::None) {
        return true;
    }
    let constructor = get_first_constructor_with_body(node);
    match constructor {
        Some(constructor) => child_is_decorated(&constructor, Some(node)),
        None => false,
    }
}

pub fn is_jsx_tag_name(node: &Node) -> bool {
    let parent = node.parent();
    match parent.kind() {
        SyntaxKind::JsxOpeningElement => ptr::eq(&*parent.as_jsx_opening_element().tag_name, node),
        SyntaxKind::JsxSelfClosingElement => {
            ptr::eq(&*parent.as_jsx_self_closing_element().tag_name, node)
        }
        SyntaxKind::JsxClosingElement => ptr::eq(&*parent.as_jsx_closing_element().tag_name, node),
        _ => false,
    }
}

pub fn is_expression_node(node: &Node) -> bool {
    let mut node = node.node_wrapper();
    match node.kind() {
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
            while node.parent().kind() == SyntaxKind::QualifiedName {
                node = node.parent();
            }
            node.parent().kind() == SyntaxKind::TypeQuery
                || is_jsdoc_link_like(&node.parent())
                || is_jsdoc_name_reference(&node.parent())
                || is_jsdoc_member_name(&node.parent())
                || is_jsx_tag_name(&node)
        }
        SyntaxKind::JSDocMemberName => {
            while is_jsdoc_member_name(&node.parent()) {
                node = node.parent();
            }
            node.parent().kind() == SyntaxKind::TypeQuery
                || is_jsdoc_link_like(&node.parent())
                || is_jsdoc_name_reference(&node.parent())
                || is_jsdoc_member_name(&node.parent())
                || is_jsx_tag_name(&node)
        }
        SyntaxKind::PrivateIdentifier => {
            let node_parent = node.parent();
            if !is_binary_expression(&node_parent) {
                return false;
            }
            let node_parent_as_binary_expression = node_parent.as_binary_expression();
            Gc::ptr_eq(&node_parent_as_binary_expression.left, &node)
                && node_parent_as_binary_expression.operator_token.kind() == SyntaxKind::InKeyword
        }
        SyntaxKind::Identifier => {
            if node.parent().kind() == SyntaxKind::TypeQuery
                || is_jsdoc_link_like(&node.parent())
                || is_jsdoc_name_reference(&node.parent())
                || is_jsdoc_member_name(&node.parent())
                || is_jsx_tag_name(&node)
            {
                return true;
            }
            is_in_expression_context(&node)
        }
        SyntaxKind::NumericLiteral
        | SyntaxKind::BigIntLiteral
        | SyntaxKind::StringLiteral
        | SyntaxKind::NoSubstitutionTemplateLiteral
        | SyntaxKind::ThisKeyword => is_in_expression_context(&node),
        _ => false,
    }
}

pub fn is_in_expression_context(node: &Node) -> bool {
    let parent = node.parent();
    match parent.kind() {
        SyntaxKind::VariableDeclaration
        | SyntaxKind::Parameter
        | SyntaxKind::PropertyDeclaration
        | SyntaxKind::PropertySignature
        | SyntaxKind::EnumMember
        | SyntaxKind::PropertyAssignment
        | SyntaxKind::BindingElement => {
            matches!(parent.as_has_initializer().maybe_initializer(), Some(initializer) if ptr::eq(&*initializer, node))
        }
        SyntaxKind::ExpressionStatement => {
            ptr::eq(&*parent.as_expression_statement().expression, node)
        }
        SyntaxKind::IfStatement => ptr::eq(&*parent.as_if_statement().expression, node),
        SyntaxKind::DoStatement => ptr::eq(&*parent.as_do_statement().expression, node),
        SyntaxKind::WhileStatement => ptr::eq(&*parent.as_while_statement().expression, node),
        SyntaxKind::ReturnStatement => {
            matches!(parent.as_return_statement().expression.as_ref(), Some(expression) if ptr::eq(&**expression, node))
        }
        SyntaxKind::WithStatement => ptr::eq(&*parent.as_with_statement().expression, node),
        SyntaxKind::SwitchStatement => ptr::eq(&*parent.as_switch_statement().expression, node),
        SyntaxKind::CaseClause => ptr::eq(&*parent.as_case_clause().expression, node),
        SyntaxKind::ThrowStatement => ptr::eq(&*parent.as_throw_statement().expression, node),
        SyntaxKind::ForStatement => {
            let for_statement = parent.as_for_statement();
            matches!(for_statement.initializer.as_ref(), Some(initializer) if ptr::eq(&**initializer, node) && initializer.kind() != SyntaxKind::VariableDeclarationList)
                || matches!(for_statement.condition.as_ref(), Some(condition) if ptr::eq(&**condition, node))
                || matches!(for_statement.incrementor.as_ref(), Some(incrementor) if ptr::eq(&**incrementor, node))
        }
        SyntaxKind::ForInStatement => {
            let for_in_statement = parent.as_for_in_statement();
            ptr::eq(&*for_in_statement.initializer, node)
                && for_in_statement.initializer.kind() != SyntaxKind::VariableDeclarationList
                || ptr::eq(&*for_in_statement.expression, node)
        }
        SyntaxKind::ForOfStatement => {
            let for_in_statement = parent.as_for_of_statement();
            ptr::eq(&*for_in_statement.initializer, node)
                && for_in_statement.initializer.kind() != SyntaxKind::VariableDeclarationList
                || ptr::eq(&*for_in_statement.expression, node)
        }
        SyntaxKind::TypeAssertionExpression => {
            ptr::eq(node, &*parent.as_type_assertion().expression)
        }
        SyntaxKind::AsExpression => ptr::eq(node, &*parent.as_as_expression().expression),
        SyntaxKind::TemplateSpan => ptr::eq(node, &*parent.as_template_span().expression),
        SyntaxKind::ComputedPropertyName => {
            ptr::eq(node, &*parent.as_computed_property_name().expression)
        }
        SyntaxKind::Decorator
        | SyntaxKind::JsxExpression
        | SyntaxKind::JsxSpreadAttribute
        | SyntaxKind::SpreadAssignment => true,
        SyntaxKind::ExpressionWithTypeArguments => {
            ptr::eq(
                &*parent.as_expression_with_type_arguments().expression,
                node,
            ) && is_expression_with_type_arguments_in_class_extends_clause(&parent)
        }
        SyntaxKind::ShorthandPropertyAssignment => {
            matches!(parent.as_shorthand_property_assignment().object_assignment_initializer.as_ref(), Some(object_assignment_initializer) if ptr::eq(&**object_assignment_initializer, node))
        }
        _ => is_expression_node(&parent),
    }
}
