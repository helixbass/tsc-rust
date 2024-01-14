use std::{
    borrow::Borrow,
    convert::{TryFrom, TryInto},
};

use gc::Gc;
use id_arena::Id;
use regex::Regex;

use crate::{
    concatenate, contains_gc, create_file_diagnostic, create_scanner, create_text_span,
    create_text_span_from_bounds, every, for_each_child_bool, get_combined_modifier_flags,
    get_combined_node_flags, get_emit_flags, get_end_line_position, get_leading_comment_ranges,
    get_line_and_character_of_position, get_source_file_of_node, get_trailing_comment_ranges,
    has_effective_readonly_modifier, has_static_modifier, is_accessor,
    is_expression_with_type_arguments_in_class_extends_clause, is_function_declaration,
    is_function_like, is_identifier, is_import_type_node, is_jsdoc, is_jsx_text,
    is_literal_type_node, is_meta_property, is_parameter_property_declaration,
    is_property_declaration, is_property_signature, is_string_literal, is_variable_declaration,
    is_variable_statement, maybe_filter, maybe_text_char_at_index, node_is_missing,
    single_or_undefined, skip_trivia, try_for_each_child, AsDoubleDeref, BaseDiagnostic,
    BaseDiagnosticRelatedInformation, CharacterCodes, ClassLikeDeclarationInterface, CommentRange,
    Debug_, DiagnosticMessage, DiagnosticMessageChain, DiagnosticMessageText,
    DiagnosticRelatedInformation, DiagnosticWithLocation, EmitFlags,
    FunctionLikeDeclarationInterface, HasArena, HasInitializerInterface, HasTypeArgumentsInterface,
    ModifierFlags, NamedDeclarationInterface, Node, NodeArray, NodeFlags, NodeInterface,
    ReadonlyTextRange, ScriptKind, SourceFileLike, SourceTextAsChars, SyntaxKind, TextRange,
    TextSpan, contains,
};
use crate::InArena;

pub fn create_diagnostic_for_node(
    node: Id<Node>,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>, arena: &impl HasArena,
) -> DiagnosticWithLocation {
    let source_file = get_source_file_of_node(node, arena);
    create_diagnostic_for_node_in_source_file(&source_file, node, message, args, arena)
}

pub fn create_diagnostic_for_node_array(
    source_file: &Node, /*SourceFile*/
    nodes: &NodeArray,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticWithLocation {
    let start = skip_trivia(
        &source_file.as_source_file().text_as_chars(),
        nodes.pos(),
        None,
        None,
        None,
    );
    create_file_diagnostic(source_file.arena_id(), start, nodes.end() - start, message, args)
}

pub fn create_diagnostic_for_node_in_source_file(
    source_file: Id<Node>, /*SourceFile*/
    node: Id<Node>,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>, arena: &impl HasArena,
) -> DiagnosticWithLocation {
    let span = get_error_span_for_node(source_file, node, arena);
    create_file_diagnostic(source_file, span.start, span.length, message, args)
}

pub fn create_diagnostic_for_node_from_message_chain(
    node: Id<Node>,
    message_chain: DiagnosticMessageChain,
    related_information: Option<Vec<Gc<DiagnosticRelatedInformation>>>, arena: &impl HasArena,
) -> DiagnosticWithLocation {
    let source_file = get_source_file_of_node(node, arena);
    let span = get_error_span_for_node(source_file, node, arena);
    create_file_diagnostic_from_message_chain(
        &source_file.ref_(arena),
        span.start,
        span.length,
        message_chain,
        related_information,
    )
}

fn assert_diagnostic_location(file: Option<&Node /*SourceFile*/>, start: isize, length: isize) {
    Debug_.assert_greater_than_or_equal(start, 0);
    Debug_.assert_greater_than_or_equal(length, 0);

    if let Some(file) = file {
        let file_as_source_file = file.as_source_file();
        Debug_.assert_less_than_or_equal(
            start,
            file_as_source_file
                .text_as_chars()
                .len()
                .try_into()
                .unwrap(),
        );
        Debug_.assert_less_than_or_equal(
            start + length,
            file_as_source_file
                .text_as_chars()
                .len()
                .try_into()
                .unwrap(),
        );
    }
}

pub fn create_file_diagnostic_from_message_chain(
    file: &Node, /*SourceFile*/
    start: isize,
    length: isize,
    message_chain: DiagnosticMessageChain,
    related_information: Option<Vec<Gc<DiagnosticRelatedInformation>>>,
) -> DiagnosticWithLocation {
    assert_diagnostic_location(Some(file), start, length);
    DiagnosticWithLocation::new(BaseDiagnostic::new(
        BaseDiagnosticRelatedInformation::new(
            message_chain.category,
            message_chain.code,
            Some(file.arena_id()),
            Some(start),
            Some(length),
            if message_chain.next.is_some() {
                Into::<DiagnosticMessageText>::into(message_chain)
            } else {
                Into::<DiagnosticMessageText>::into(message_chain.message_text)
            },
        ),
        related_information,
    ))
}

pub fn create_diagnostic_for_file_from_message_chain(
    source_file: Id<Node>, /*SourceFile*/
    message_chain: DiagnosticMessageChain,
    related_information: Option<Vec<Gc<DiagnosticRelatedInformation>>>,
) -> DiagnosticWithLocation {
    DiagnosticWithLocation::new(BaseDiagnostic::new(
        BaseDiagnosticRelatedInformation::new(
            message_chain.category,
            message_chain.code,
            Some(source_file),
            Some(0),
            Some(0),
            if message_chain.next.is_some() {
                Into::<DiagnosticMessageText>::into(message_chain)
            } else {
                Into::<DiagnosticMessageText>::into(message_chain.message_text)
            },
        ),
        related_information,
    ))
}

pub fn create_diagnostic_for_range<TRange: TextRange>(
    source_file: Id<Node>, /*SourceFile*/
    range: &TRange,
    message: &DiagnosticMessage,
) -> DiagnosticWithLocation {
    DiagnosticWithLocation::new(BaseDiagnostic::new(
        BaseDiagnosticRelatedInformation::new(
            message.category,
            message.code,
            Some(source_file),
            Some(range.pos()),
            Some(range.end() - range.pos()),
            message.message.clone().into_owned(),
        ),
        None,
    ))
}

pub fn get_span_of_token_at_position(
    source_file: &Node, /*SourceFile*/
    pos: usize,
) -> TextSpan {
    let source_file_as_source_file = source_file.as_source_file();
    let scanner = create_scanner(
        source_file_as_source_file.language_version(),
        true,
        Some(source_file_as_source_file.language_variant()),
        Some(source_file_as_source_file.text_as_chars().to_owned()),
        Some(source_file_as_source_file.text().to_owned()),
        // /*onError: */ undefined,
        Some(pos),
        None,
    );
    scanner.scan(None);
    let start = scanner.get_token_pos();
    create_text_span_from_bounds(
        start.try_into().unwrap(),
        scanner.get_text_pos().try_into().unwrap(),
    )
}

fn get_error_span_for_arrow_function(
    source_file: Id<Node>, /*SourceFile*/
    node: Id<Node>,        /*ArrowFunction*/
    arena: &impl HasArena
) -> TextSpan {
    let source_file_ref = source_file.ref_(arena);
    let source_file_as_source_file = source_file_ref.as_source_file();
    let pos = skip_trivia(
        &source_file_as_source_file.text_as_chars(),
        node.ref_(arena).pos(),
        None,
        None,
        None,
    );
    let node_ref = node.ref_(arena);
    let node_as_arrow_function = node_ref.as_arrow_function();
    if let Some(node_body) = node_as_arrow_function.maybe_body() {
        if node_body.ref_(arena).kind() == SyntaxKind::Block {
            let start_line = get_line_and_character_of_position(
                source_file_as_source_file,
                node_body.ref_(arena).pos().try_into().unwrap(),
            )
            .line;
            let end_line = get_line_and_character_of_position(
                source_file_as_source_file,
                node_body.ref_(arena).end().try_into().unwrap(),
            )
            .line;
            if start_line < end_line {
                return create_text_span(
                    pos,
                    isize::try_from(get_end_line_position(
                        start_line,
                        source_file_as_source_file,
                    ))
                    .unwrap()
                        - pos
                        + 1,
                );
            }
        }
    }
    create_text_span_from_bounds(pos, node_ref.end())
}

pub fn get_error_span_for_node(
    source_file: Id<Node>, /*SourceFile*/
    node: Id<Node>, arena: &impl HasArena,
) -> TextSpan {
    let mut error_node: Option<Id<Node>> = Some(node);
    let source_file_ref = source_file.ref_(arena);
    let source_file_as_source_file = source_file_ref.as_source_file();
    match node.ref_(arena).kind() {
        SyntaxKind::SourceFile => {
            let pos = skip_trivia(
                &source_file_as_source_file.text_as_chars(),
                0,
                Some(false),
                None,
                None,
            );
            let pos_as_usize = usize::try_from(pos).unwrap();
            if pos_as_usize == source_file_as_source_file.text_as_chars().len() {
                return create_text_span(0, 0);
            }
            return get_span_of_token_at_position(&source_file_ref, pos_as_usize);
        }
        SyntaxKind::VariableDeclaration
        | SyntaxKind::BindingElement
        | SyntaxKind::ClassDeclaration
        | SyntaxKind::ClassExpression
        | SyntaxKind::InterfaceDeclaration
        | SyntaxKind::ModuleDeclaration
        | SyntaxKind::EnumDeclaration
        | SyntaxKind::EnumMember
        | SyntaxKind::FunctionDeclaration
        | SyntaxKind::FunctionExpression
        | SyntaxKind::MethodDeclaration
        | SyntaxKind::GetAccessor
        | SyntaxKind::SetAccessor
        | SyntaxKind::TypeAliasDeclaration
        | SyntaxKind::PropertyDeclaration
        | SyntaxKind::PropertySignature
        | SyntaxKind::NamespaceImport => {
            error_node = node.ref_(arena).as_named_declaration().maybe_name();
        }
        SyntaxKind::ArrowFunction => {
            return get_error_span_for_arrow_function(source_file, node, arena);
        }
        SyntaxKind::CaseClause => {
            let node_ref = node.ref_(arena);
            let node_as_case_clause = node_ref.as_case_clause();
            let start = skip_trivia(
                &source_file_as_source_file.text_as_chars(),
                node_ref.pos(),
                None,
                None,
                None,
            );
            let end = if !node_as_case_clause.statements.is_empty() {
                node_as_case_clause.statements[0].ref_(arena).pos()
            } else {
                node_ref.end()
            };
            return create_text_span_from_bounds(start, end);
        }
        SyntaxKind::DefaultClause => {
            let node_ref = node.ref_(arena);
            let node_as_default_clause = node_ref.as_default_clause();
            let start = skip_trivia(
                &source_file_as_source_file.text_as_chars(),
                node.ref_(arena).pos(),
                None,
                None,
                None,
            );
            let end = if !node_as_default_clause.statements.is_empty() {
                node_as_default_clause.statements[0].ref_(arena).pos()
            } else {
                node_ref.end()
            };
            return create_text_span_from_bounds(start, end);
        }
        _ => (),
    }

    if error_node.is_none() {
        return get_span_of_token_at_position(&source_file_ref, node.ref_(arena).pos().try_into().unwrap());
    }
    let error_node = error_node.unwrap();

    Debug_.assert(!is_jsdoc(&error_node.ref_(arena)), None);

    let is_missing = node_is_missing(Some(&error_node.ref_(arena)));
    let pos = if is_missing || is_jsx_text(&error_node.ref_(arena)) {
        error_node.ref_(arena).pos()
    } else {
        skip_trivia(
            &source_file_as_source_file.text_as_chars(),
            error_node.ref_(arena).pos(),
            None,
            None,
            None,
        )
    };

    if is_missing {
        Debug_.assert(
            pos == error_node.ref_(arena).pos(),
            Some("This failure could trigger https://github.com/Microsoft/TypeScript/issues/20809"),
        );
        Debug_.assert(
            pos == error_node.ref_(arena).end(),
            Some("This failure could trigger https://github.com/Microsoft/TypeScript/issues/20809"),
        );
    } else {
        Debug_.assert(
            pos >= error_node.ref_(arena).pos(),
            Some("This failure could trigger https://github.com/Microsoft/TypeScript/issues/20809"),
        );
        Debug_.assert(
            pos <= error_node.ref_(arena).end(),
            Some("This failure could trigger https://github.com/Microsoft/TypeScript/issues/20809"),
        );
    }

    create_text_span_from_bounds(pos, error_node.ref_(arena).end())
}

pub fn is_external_or_common_js_module(file: &Node /*SourceFile*/) -> bool {
    let file_as_source_file = file.as_source_file();
    file_as_source_file
        .maybe_external_module_indicator()
        .is_some()
        || file_as_source_file
            .maybe_common_js_module_indicator()
            .is_some()
}

pub fn is_json_source_file(file: &Node /*SourceFile*/) -> bool {
    file.as_source_file().script_kind() == ScriptKind::JSON
}

pub fn is_enum_const(node: Id<Node> /*EnumDeclaration*/, arena: &impl HasArena) -> bool {
    get_combined_modifier_flags(node, arena).intersects(ModifierFlags::Const)
}

pub fn is_declaration_readonly(
    declaration: Id<Node>, /*Declaration*/
    arena: &impl HasArena,
) -> bool {
    get_combined_modifier_flags(declaration, arena).intersects(ModifierFlags::Readonly)
        && !is_parameter_property_declaration(declaration, declaration.ref_(arena).parent(), arena)
}

pub fn is_var_const(
    node: Id<Node>, /*VariableDeclaration | VariableDeclarationList*/
    arena: &impl HasArena,
) -> bool {
    get_combined_node_flags(node, arena).intersects(NodeFlags::Const)
}

pub fn is_let(node: Id<Node>, arena: &impl HasArena) -> bool {
    get_combined_node_flags(node, arena).intersects(NodeFlags::Let)
}

pub fn is_super_call(n: Id<Node>, arena: &impl HasArena) -> bool {
    n.ref_(arena).kind() == SyntaxKind::CallExpression
        && n.ref_(arena).as_call_expression().expression.ref_(arena).kind() == SyntaxKind::SuperKeyword
}

pub fn is_import_call(n: Id<Node>, arena: &impl HasArena) -> bool {
    match &*n.ref_(arena) {
        Node::CallExpression(call_expression) => {
            call_expression.expression.ref_(arena).kind() == SyntaxKind::ImportKeyword
        }
        _ => false,
    }
}

pub fn is_import_meta(n: Id<Node>, arena: &impl HasArena) -> bool {
    if !is_meta_property(&n.ref_(arena)) {
        return false;
    }
    let n_ref = n.ref_(arena);
    let n_as_meta_property = n_ref.as_meta_property();
    n_as_meta_property.keyword_token == SyntaxKind::ImportKeyword
        && n_ref.as_meta_property().name.ref_(arena).as_identifier().escaped_text == "meta"
}

pub fn is_literal_import_type_node(n: Id<Node>, arena: &impl HasArena) -> bool {
    if !is_import_type_node(&n.ref_(arena)) {
        return false;
    }
    let n_ref = n.ref_(arena);
    let n_as_import_type_node = n_ref.as_import_type_node();
    if !is_literal_type_node(&n_as_import_type_node.argument.ref_(arena)) {
        return false;
    }
    let n_argument_ref = n_as_import_type_node.argument.ref_(arena);
    let n_argument_as_literal_type_node = n_argument_ref.as_literal_type_node();
    is_string_literal(&n_argument_as_literal_type_node.literal.ref_(arena))
}

pub fn is_prologue_directive(node: Id<Node>, arena: &impl HasArena) -> bool {
    node.ref_(arena).kind() == SyntaxKind::ExpressionStatement
        && node.ref_(arena).as_expression_statement().expression.ref_(arena).kind() == SyntaxKind::StringLiteral
}

pub fn is_custom_prologue(node: &Node /*Statement*/) -> bool {
    get_emit_flags(node).intersects(EmitFlags::CustomPrologue)
}

pub fn is_hoisted_function(node: &Node /*Statement*/) -> bool {
    is_custom_prologue(node) && is_function_declaration(node)
}

fn is_hoisted_variable(node: Id<Node> /*VariableDeclaration*/, arena: &impl HasArena) -> bool {
    let node_ref = node.ref_(arena);
    let node_as_variable_declaration = node_ref.as_variable_declaration();
    is_identifier(&node_as_variable_declaration.name().ref_(arena))
        && node_as_variable_declaration.maybe_initializer().is_none()
}

pub fn is_hoisted_variable_statement(node: Id<Node> /*Statement*/, arena: &impl HasArena) -> bool {
    is_custom_prologue(&node.ref_(arena))
        && is_variable_statement(&node.ref_(arena))
        && every(
            &node
                .ref_(arena).as_variable_statement()
                .declaration_list
                .ref_(arena).as_variable_declaration_list()
                .declarations,
            |&declaration, _| is_hoisted_variable(declaration, arena),
        )
}

pub fn get_leading_comment_ranges_of_node(
    node: &Node,
    source_file_of_node: &Node, /*SourceFile*/
) -> Option<Vec<CommentRange>> {
    if node.kind() != SyntaxKind::JsxText {
        get_leading_comment_ranges(
            &source_file_of_node.as_source_file().text_as_chars(),
            node.pos().try_into().unwrap(),
        )
    } else {
        None
    }
}

pub fn get_jsdoc_comment_ranges<TNode: NodeInterface>(
    node: &TNode,
    text: &SourceTextAsChars,
) -> Option<Vec<CommentRange>> {
    let comment_ranges = if matches!(
        node.kind(),
        SyntaxKind::Parameter
            | SyntaxKind::TypeParameter
            | SyntaxKind::FunctionExpression
            | SyntaxKind::ArrowFunction
            | SyntaxKind::ParenthesizedExpression
            | SyntaxKind::VariableDeclaration
    ) {
        Some(concatenate(
            // TODO: should get_trailing_comment_ranges()/get_leading_comment_ranges() accept isize instead?
            get_trailing_comment_ranges(text, node.pos().try_into().unwrap())
                .unwrap_or_else(|| vec![]),
            get_leading_comment_ranges(text, node.pos().try_into().unwrap())
                .unwrap_or_else(|| vec![]),
        ))
    } else {
        get_leading_comment_ranges(text, node.pos().try_into().unwrap())
    };
    maybe_filter(comment_ranges.as_deref(), |comment| {
        matches!(maybe_text_char_at_index(text, (comment.pos() + 1).try_into().unwrap()), Some(ch) if ch == CharacterCodes::asterisk)
            && matches!(maybe_text_char_at_index(text, (comment.pos() + 2).try_into().unwrap()), Some(ch) if ch == CharacterCodes::asterisk)
            && match maybe_text_char_at_index(text, (comment.pos() + 3).try_into().unwrap()) {
                None => true,
                Some(ch) if ch != CharacterCodes::slash => true,
                _ => false,
            }
    })
}

lazy_static! {
    pub static ref full_triple_slash_reference_path_reg_ex: Regex =
        Regex::new(r#"^(///\s*<reference\s+path\s*=\s*)(('[^']*')|("[^"]*")).*?/>"#).unwrap();
}

lazy_static! {
    pub(super) static ref full_triple_slash_reference_type_reference_directive_reg_ex: Regex =
        Regex::new(r#"^(///\s*<reference\s+types\s*=\s*)(('[^']*')|("[^"]*")).*?/>"#).unwrap();
}

lazy_static! {
    pub static ref full_triple_slash_amd_reference_path_reg_ex: Regex =
        Regex::new(r#"^(///\s*<amd-dependency\s+path\s*=\s*)(('[^']*')|("[^"]*")).*?/>"#).unwrap();
}

lazy_static! {
    pub(super) static ref default_lib_reference_reg_ex: Regex =
        Regex::new(r#"(///\s*<reference\s+no-default-lib\s*=\s*)(('[^']*')|("[^"]*"))\s*/>"#)
            .unwrap();
}

pub fn is_part_of_type_node(mut node: Id<Node>, arena: &impl HasArena) -> bool {
    if SyntaxKind::FirstTypeNode <= node.ref_(arena).kind() && node.ref_(arena).kind() <= SyntaxKind::LastTypeNode {
        return true;
    }

    match node.ref_(arena).kind() {
        SyntaxKind::AnyKeyword
        | SyntaxKind::UnknownKeyword
        | SyntaxKind::NumberKeyword
        | SyntaxKind::BigIntKeyword
        | SyntaxKind::StringKeyword
        | SyntaxKind::BooleanKeyword
        | SyntaxKind::SymbolKeyword
        | SyntaxKind::ObjectKeyword
        | SyntaxKind::UndefinedKeyword
        | SyntaxKind::NeverKeyword => {
            return true;
        }
        SyntaxKind::VoidKeyword => {
            return node.ref_(arena).parent().ref_(arena).kind() != SyntaxKind::VoidExpression;
        }
        SyntaxKind::ExpressionWithTypeArguments => {
            return !is_expression_with_type_arguments_in_class_extends_clause(node, arena)
        }
        SyntaxKind::TypeParameter => {
            return matches!(
                node.ref_(arena).parent().ref_(arena).kind(),
                SyntaxKind::MappedType | SyntaxKind::InferType
            );
        }

        SyntaxKind::Identifier => {
            if node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::QualifiedName
                && node.ref_(arena).parent().ref_(arena).as_qualified_name().right == node
            {
                node = node.ref_(arena).parent();
            } else if node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::PropertyAccessExpression
                && node.ref_(arena).parent().ref_(arena).as_property_access_expression().name == node
            {
                node = node.ref_(arena).parent();
            }
            Debug_.assert(
                matches!(
                    node.ref_(arena).kind(),
                    SyntaxKind::Identifier | SyntaxKind::QualifiedName | SyntaxKind::PropertyAccessExpression
                ),
                Some("'node' was expected to be a qualified name, identifier or property access in 'isPartOfTypeNode'.")
            );
            let parent = node.ref_(arena).parent();
            if parent.ref_(arena).kind() == SyntaxKind::TypeQuery {
                return false;
            }
            if parent.ref_(arena).kind() == SyntaxKind::ImportType {
                return !parent.ref_(arena).as_import_type_node().is_type_of();
            }
            if SyntaxKind::FirstTypeNode <= parent.ref_(arena).kind()
                && parent.ref_(arena).kind() <= SyntaxKind::LastTypeNode
            {
                return true;
            }
            match parent.ref_(arena).kind() {
                SyntaxKind::ExpressionWithTypeArguments => {
                    return !is_expression_with_type_arguments_in_class_extends_clause(parent, arena);
                }
                SyntaxKind::TypeParameter => {
                    return matches!(
                        parent.ref_(arena).as_type_parameter_declaration().constraint,
                        Some(constraint) if node == constraint
                    );
                }
                SyntaxKind::JSDocTemplateTag => {
                    return matches!(
                        parent.ref_(arena).as_jsdoc_template_tag().constraint,
                        Some(constraint) if node == constraint
                    );
                }
                SyntaxKind::PropertyDeclaration
                | SyntaxKind::PropertySignature
                | SyntaxKind::Parameter
                | SyntaxKind::VariableDeclaration => {
                    return matches!(
                        parent.ref_(arena).as_has_type().maybe_type(),
                        Some(type_) if node == type_
                    );
                }
                SyntaxKind::FunctionDeclaration
                | SyntaxKind::FunctionExpression
                | SyntaxKind::ArrowFunction
                | SyntaxKind::Constructor
                | SyntaxKind::MethodDeclaration
                | SyntaxKind::MethodSignature
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor => {
                    return matches!(
                        parent.ref_(arena).as_function_like_declaration().maybe_type(),
                        Some(type_) if node == type_
                    );
                }
                SyntaxKind::CallSignature
                | SyntaxKind::ConstructSignature
                | SyntaxKind::IndexSignature => {
                    return matches!(
                        parent.ref_(arena).as_signature_declaration().maybe_type(),
                        Some(type_) if node == type_
                    );
                }
                SyntaxKind::TypeAssertionExpression => {
                    return node == parent.ref_(arena).as_type_assertion().type_;
                }
                SyntaxKind::CallExpression => {
                    return contains(
                        parent
                            .ref_(arena).as_call_expression()
                            .maybe_type_arguments()
                            .as_double_deref(),
                        &node,
                    );
                }
                SyntaxKind::NewExpression => {
                    return contains(
                        parent
                            .ref_(arena).as_new_expression()
                            .maybe_type_arguments()
                            .as_double_deref(),
                        &node,
                    );
                }
                SyntaxKind::TaggedTemplateExpression => {
                    return false;
                }
                _ => (),
            }
        }
        _ => (),
    }

    false
}

pub fn is_child_of_node_with_kind(node: Id<Node>, kind: SyntaxKind, arena: &impl HasArena) -> bool {
    let mut node: Option<Id<Node>> = Some(node);
    while let Some(node_present) = node {
        if node_present.ref_(arena).kind() == kind {
            return true;
        }
        node = node_present.ref_(arena).maybe_parent();
    }
    false
}

pub fn for_each_return_statement(
    body: Id<Node>, /*Block | Statement*/
    mut visitor: impl FnMut(Id<Node>),
    arena: &impl HasArena,
) {
    try_for_each_return_statement(body, |node: Id<Node>| -> Result<_, ()> {
        Ok(visitor(node))
    }, arena)
    .unwrap()
}

pub fn try_for_each_return_statement<TError>(
    body: Id<Node>, /*Block | Statement*/
    mut visitor: impl FnMut(Id<Node>) -> Result<(), TError>,
    arena: &impl HasArena,
) -> Result<(), TError> {
    try_for_each_return_statement_traverse(body, &mut visitor, arena)
}

fn try_for_each_return_statement_traverse<TError>(
    node: Id<Node>,
    visitor: &mut impl FnMut(Id<Node>) -> Result<(), TError>,
    arena: &impl HasArena,
) -> Result<(), TError> {
    match node.ref_(arena).kind() {
        SyntaxKind::ReturnStatement => {
            visitor(node)?;
        }
        SyntaxKind::CaseBlock
        | SyntaxKind::Block
        | SyntaxKind::IfStatement
        | SyntaxKind::DoStatement
        | SyntaxKind::WhileStatement
        | SyntaxKind::ForStatement
        | SyntaxKind::ForInStatement
        | SyntaxKind::ForOfStatement
        | SyntaxKind::WithStatement
        | SyntaxKind::SwitchStatement
        | SyntaxKind::CaseClause
        | SyntaxKind::DefaultClause
        | SyntaxKind::LabeledStatement
        | SyntaxKind::TryStatement
        | SyntaxKind::CatchClause => {
            try_for_each_child(
                &node.ref_(arena),
                |node| try_for_each_return_statement_traverse(node, visitor, arena),
                Option::<fn(&NodeArray) -> Result<(), TError>>::None,
            )?;
        }
        _ => (),
    };

    return Ok(());
}

pub fn for_each_return_statement_bool(
    body: Id<Node>, /*Block | Statement*/
    mut visitor: impl FnMut(Id<Node>) -> bool,
    arena: &impl HasArena,
) -> bool {
    for_each_return_statement_bool_traverse(body, &mut visitor, arena)
}

fn for_each_return_statement_bool_traverse(
    node: Id<Node>,
    visitor: &mut impl FnMut(Id<Node>) -> bool,
    arena: &impl HasArena,
) -> bool {
    match node.ref_(arena).kind() {
        SyntaxKind::ReturnStatement => visitor(node),
        SyntaxKind::CaseBlock
        | SyntaxKind::Block
        | SyntaxKind::IfStatement
        | SyntaxKind::DoStatement
        | SyntaxKind::WhileStatement
        | SyntaxKind::ForStatement
        | SyntaxKind::ForInStatement
        | SyntaxKind::ForOfStatement
        | SyntaxKind::WithStatement
        | SyntaxKind::SwitchStatement
        | SyntaxKind::CaseClause
        | SyntaxKind::DefaultClause
        | SyntaxKind::LabeledStatement
        | SyntaxKind::TryStatement
        | SyntaxKind::CatchClause => for_each_child_bool(
            &node.ref_(arena),
            |node| for_each_return_statement_bool_traverse(node, visitor, arena),
            Option::<fn(&NodeArray) -> bool>::None,
        ),
        _ => false,
    }
}

pub fn for_each_yield_expression(body: Id<Node> /*Block*/, mut visitor: impl FnMut(Id<Node>), arena: &impl HasArena) {
    try_for_each_yield_expression(body, |node: Id<Node>| -> Result<(), ()> {
        Ok(visitor(node))
    }, arena)
    .unwrap()
}

pub fn try_for_each_yield_expression<TError>(
    body: Id<Node>, /*Block*/
    mut visitor: impl FnMut(Id<Node>) -> Result<(), TError>,
    arena: &impl HasArena,
) -> Result<(), TError> {
    try_for_each_yield_expression_traverse(body, &mut visitor, arena)
}

fn try_for_each_yield_expression_traverse<TError>(
    node: Id<Node>,
    visitor: &mut impl FnMut(Id<Node>) -> Result<(), TError>,
    arena: &impl HasArena,
) -> Result<(), TError> {
    match node.ref_(arena).kind() {
        SyntaxKind::YieldExpression => {
            visitor(node)?;
            let operand = node.ref_(arena).as_yield_expression().expression;
            if let Some(operand) = operand {
                try_for_each_yield_expression_traverse(operand, visitor, arena)?;
            }
            return Ok(());
        }
        SyntaxKind::EnumDeclaration
        | SyntaxKind::InterfaceDeclaration
        | SyntaxKind::ModuleDeclaration
        | SyntaxKind::TypeAliasDeclaration => {
            return Ok(());
        }
        _ => {
            if is_function_like(Some(&node.ref_(arena))) {
                if let Some(node_name) = node.ref_(arena).as_signature_declaration().maybe_name() {
                    if node_name.ref_(arena).kind() == SyntaxKind::ComputedPropertyName {
                        try_for_each_yield_expression_traverse(
                            node_name.ref_(arena).as_computed_property_name().expression,
                            visitor,
                            arena,
                        )?;
                        return Ok(());
                    }
                }
            } else if !is_part_of_type_node(node, arena) {
                try_for_each_child(
                    &node.ref_(arena),
                    |node| try_for_each_yield_expression_traverse(node, visitor, arena),
                    Option::<fn(&NodeArray) -> Result<(), TError>>::None,
                )?;
            }
        }
    };

    Ok(())
}

pub fn get_rest_parameter_element_type(
    node: Option<Id<Node> /*TypeNode*/>,
) -> Option<Id<Node /*TypeNode*/>> {
    if node.is_none() {
        return None;
    }
    let node = node.unwrap();
    let node = node.borrow();
    match node.kind() {
        SyntaxKind::ArrayType => Some(node.as_array_type_node().element_type.clone()),
        SyntaxKind::TypeReference => single_or_undefined(
            node.as_type_reference_node()
                .maybe_type_arguments()
                .as_double_deref(),
        )
        .cloned(),
        _ => None,
    }
}

pub fn get_members_of_declaration(
    node: Id<Node>, /*Declaration*/
) -> Option<Gc<NodeArray> /*<ClassElement | TypeElement | ObjectLiteralElement*/> {
    match node.kind() {
        SyntaxKind::InterfaceDeclaration => Some(node.as_interface_declaration().members.clone()),
        SyntaxKind::ClassDeclaration => Some(node.as_class_declaration().members().clone()),
        SyntaxKind::ClassExpression => Some(node.as_class_expression().members().clone()),
        SyntaxKind::TypeLiteral => Some(node.as_type_literal_node().members.clone()),
        SyntaxKind::ObjectLiteralExpression => {
            Some(node.as_object_literal_expression().properties.clone())
        }
        _ => None,
    }
}

pub fn is_variable_like(node: &Node) -> bool {
    /* if node {*/
    match node.kind() {
        SyntaxKind::BindingElement
        | SyntaxKind::EnumMember
        | SyntaxKind::Parameter
        | SyntaxKind::PropertyAssignment
        | SyntaxKind::PropertyDeclaration
        | SyntaxKind::PropertySignature
        | SyntaxKind::ShorthandPropertyAssignment
        | SyntaxKind::VariableDeclaration => true,
        _ => false,
    }
    /*}*/
}

pub fn is_variable_like_or_accessor(node: Id<Node>) -> bool {
    is_variable_like(node) || is_accessor(node)
}

pub fn is_variable_declaration_in_variable_statement(
    node: Id<Node>,
    /*VariableDeclaration*/ arena: &impl HasArena,
) -> bool {
    node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::VariableDeclarationList
        && node
            .ref_(arena)
            .parent()
            .ref_(arena)
            .parent()
            .ref_(arena)
            .kind()
            == SyntaxKind::VariableStatement
}

pub fn is_valid_es_symbol_declaration(node: Id<Node>, arena: &impl HasArena) -> bool {
    if is_variable_declaration(&node.ref_(arena)) {
        is_var_const(node, arena)
            && is_identifier(
                &node
                    .ref_(arena)
                    .as_variable_declaration()
                    .name()
                    .ref_(arena),
            )
            && is_variable_declaration_in_variable_statement(node, arena)
    } else if is_property_declaration(&node.ref_(arena)) {
        has_effective_readonly_modifier(node, arena) && has_static_modifier(node, arena)
    } else if is_property_signature(&node.ref_(arena)) {
        has_effective_readonly_modifier(node, arena)
    } else {
        false
    }
}
