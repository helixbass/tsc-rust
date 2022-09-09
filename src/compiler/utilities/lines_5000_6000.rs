#![allow(non_upper_case_globals)]

use regex::{Captures, Regex};
use std::borrow::Borrow;
use std::cell::RefCell;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::supported_ts_extensions_for_extract_extension;
use crate::{
    entity_name_to_string, file_extension_is, find, get_element_or_property_access_name,
    get_property_name_for_property_name_node, get_sys, has_syntactic_modifier,
    is_bindable_static_access_expression, is_element_access_expression, is_entity_name_expression,
    is_identifier, is_jsdoc_member_name, is_property_access_expression, is_property_name,
    is_qualified_name, unescape_leading_underscores, walk_up_parenthesized_expressions,
    BaseDiagnostic, BaseDiagnosticRelatedInformation, BaseNode, BaseSymbol, BaseType, CheckFlags,
    CompilerOptions, Debug_, Diagnostic, DiagnosticInterface, DiagnosticMessage,
    DiagnosticRelatedInformation, DiagnosticRelatedInformationInterface,
    DiagnosticWithDetachedLocation, DiagnosticWithLocation, Extension, MapLike, ModifierFlags,
    NewLineKind, Node, NodeFlags, NodeInterface, ObjectFlags, PrefixUnaryExpression, Signature,
    SignatureFlags, SourceFileLike, Symbol, SymbolFlags, SymbolInterface, SyntaxKind,
    TransformFlags, TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface,
    __String,
};

pub fn get_first_identifier(node: &Node) -> Rc<Node /*Identifier*/> {
    match node.kind() {
        SyntaxKind::Identifier => node.node_wrapper(),
        SyntaxKind::QualifiedName => {
            let mut node = node.node_wrapper();
            while {
                node = node.as_qualified_name().left.clone();
                node.kind() != SyntaxKind::Identifier
            } {}
            node
        }
        SyntaxKind::PropertyAccessExpression => {
            let mut node = node.node_wrapper();
            while {
                node = node.as_property_access_expression().expression.clone();
                node.kind() != SyntaxKind::Identifier
            } {}
            node
        }
        _ => panic!("Unexpected syntax kind"),
    }
}

pub fn is_dotted_name(node: &Node /*Expression*/) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::Identifier
            | SyntaxKind::ThisKeyword
            | SyntaxKind::SuperKeyword
            | SyntaxKind::MetaProperty
    ) || node.kind() == SyntaxKind::PropertyAccessExpression
        && is_dotted_name(&node.as_property_access_expression().expression)
        || node.kind() == SyntaxKind::ParenthesizedExpression
            && is_dotted_name(&node.as_parenthesized_expression().expression)
}

pub fn is_property_access_entity_name_expression(node: &Node) -> bool {
    if !is_property_access_expression(node) {
        return false;
    }
    let node_as_property_access_expression = node.as_property_access_expression();
    is_identifier(&node_as_property_access_expression.name)
        && is_entity_name_expression(&node_as_property_access_expression.expression)
}

pub fn try_get_property_access_or_identifier_to_string(
    expr: &Node, /*Expression*/
) -> Option<String> {
    if is_property_access_expression(expr) {
        let expr_as_property_access_expression = expr.as_property_access_expression();
        let base_str = try_get_property_access_or_identifier_to_string(
            &expr_as_property_access_expression.expression,
        );
        if let Some(base_str) = base_str {
            return Some(format!(
                "{}.{}",
                base_str,
                entity_name_to_string(&expr_as_property_access_expression.name)
            ));
        }
    } else if is_element_access_expression(expr) {
        let expr_as_element_access_expression = expr.as_element_access_expression();
        let base_str = try_get_property_access_or_identifier_to_string(
            &expr_as_element_access_expression.expression,
        );
        if let Some(base_str) = base_str {
            if is_property_name(&expr_as_element_access_expression.argument_expression) {
                return Some(format!(
                    "{}.{}",
                    base_str,
                    &*get_property_name_for_property_name_node(
                        &expr_as_element_access_expression.argument_expression
                    )
                    .unwrap()
                ));
            }
        }
    } else if is_identifier(expr) {
        return Some(unescape_leading_underscores(
            &expr.as_identifier().escaped_text,
        ));
    }
    None
}

pub fn is_prototype_access(node: &Node) -> bool {
    is_bindable_static_access_expression(node, None)
        && match get_element_or_property_access_name(node) {
            Some(name) => name.eq_str("prototype"),
            None => false,
        }
}

pub fn is_right_side_of_qualified_name_or_property_access(node: &Node) -> bool {
    node.parent().kind() == SyntaxKind::QualifiedName
        && ptr::eq(&*node.parent().as_qualified_name().right, node)
        || node.parent().kind() == SyntaxKind::PropertyAccessExpression
            && ptr::eq(&*node.parent().as_property_access_expression().name, node)
}

pub fn is_right_side_of_qualified_name_or_property_access_or_jsdoc_member_name(
    node: &Node,
) -> bool {
    is_qualified_name(&node.parent()) && ptr::eq(&*node.parent().as_qualified_name().right, node)
        || is_property_access_expression(&node.parent())
            && ptr::eq(&*node.parent().as_property_access_expression().name, node)
        || is_jsdoc_member_name(&node.parent())
            && ptr::eq(&*node.parent().as_jsdoc_member_name().right, node)
}

pub fn is_empty_object_literal(expression: &Node) -> bool {
    expression.kind() == SyntaxKind::ObjectLiteralExpression
        && expression
            .as_object_literal_expression()
            .properties
            .is_empty()
}

pub fn is_empty_array_literal(expression: &Node) -> bool {
    expression.kind() == SyntaxKind::ArrayLiteralExpression
        && expression.as_array_literal_expression().elements.is_empty()
}

pub fn get_local_symbol_for_export_default(symbol: &Symbol) -> Option<Rc<Symbol>> {
    if !is_export_default_symbol(symbol) || symbol.maybe_declarations().is_none() {
        return None;
    }
    for decl in symbol.maybe_declarations().as_ref().unwrap() {
        if let Some(decl_local_symbol) = decl.maybe_local_symbol() {
            return Some(decl_local_symbol);
        }
    }
    None
}

fn is_export_default_symbol(symbol: &Symbol) -> bool {
    /*symbol &&*/
    match symbol
        .maybe_declarations()
        .as_ref()
        .filter(|declarations| !declarations.is_empty())
    {
        None => false,
        Some(symbol_declarations) => {
            has_syntactic_modifier(&symbol_declarations[0], ModifierFlags::Default)
        }
    }
}

pub fn try_extract_ts_extension(file_name: &str) -> Option<Extension> {
    find(
        &supported_ts_extensions_for_extract_extension,
        |extension, _| file_extension_is(file_name, extension.to_str()),
    )
    .copied()
}

const carriage_return_line_feed: &str = "\r\n";
const line_feed: &str = "\n";
pub fn get_new_line_character<TGetNewLine: Fn() -> String>(
    new_line: Option<NewLineKind>,
    get_new_line: Option<TGetNewLine>,
) -> String {
    match new_line {
        Some(NewLineKind::CarriageReturnLineFeed) => {
            return carriage_return_line_feed.to_owned();
        }
        Some(NewLineKind::LineFeed) => {
            return line_feed.to_owned();
        }
        _ => (),
    }
    if let Some(get_new_line) = get_new_line {
        get_new_line()
    } else {
        get_sys().new_line().to_owned()
    }
}

pub fn is_watch_set(options: &CompilerOptions) -> bool {
    matches!(options.watch, Some(true))
}

pub fn get_check_flags(symbol: &Symbol) -> CheckFlags {
    match symbol {
        Symbol::TransientSymbol(transient_symbol) => transient_symbol.check_flags(),
        _ => CheckFlags::None,
    }
}

pub fn get_declaration_modifier_flags_from_symbol(
    symbol: &Symbol,
    is_write: Option<bool>,
) -> ModifierFlags {
    let is_write = is_write.unwrap_or(false);
    unimplemented!()
}

pub fn get_combined_local_and_export_symbol_flags(symbol: &Symbol) -> SymbolFlags {
    unimplemented!()
}

pub fn is_write_only_access(node: &Node) -> bool {
    access_kind(node) == AccessKind::Write
}

pub fn is_write_access(node: &Node) -> bool {
    access_kind(node) != AccessKind::Read
}

#[derive(PartialEq, Eq)]
enum AccessKind {
    Read,
    Write,
    ReadWrite,
}

fn access_kind(node: &Node) -> AccessKind {
    let parent = node.maybe_parent();
    if parent.is_none() {
        return AccessKind::Read;
    }
    let parent = parent.unwrap();

    let write_or_read_write = || {
        if let Some(grandparent) = parent.maybe_parent() {
            if walk_up_parenthesized_expressions(&*grandparent)
                .unwrap()
                .kind()
                == SyntaxKind::ExpressionStatement
            {
                return AccessKind::Write;
            }
        }
        AccessKind::ReadWrite
    };

    match &*parent {
        /*ParenthesizedExpression*/
        /*PostfixUnaryExpression*/
        Node::PrefixUnaryExpression(PrefixUnaryExpression { operator, .. }) => {
            if matches!(
                operator,
                SyntaxKind::PlusPlusToken | SyntaxKind::MinusMinusToken
            ) {
                write_or_read_write()
            } else {
                AccessKind::Read
            }
        }
        Node::BinaryExpression(_) => unimplemented!(),
        /*PropertyAccessExpression*/
        /*PropertyAssignment*/
        /*ShorthandPropertyAssignment*/
        Node::ArrayLiteralExpression(_) => access_kind(&*parent),
        _ => AccessKind::Read,
    }
}

pub fn get_class_like_declaration_of_symbol(
    symbol: &Symbol,
) -> Option<Rc<Node /*ClassLikeDeclaration*/>> {
    unimplemented!()
}

pub fn get_object_flags(type_: &Type) -> ObjectFlags {
    if type_.flags().intersects(TypeFlags::ObjectFlagsType) {
        type_.as_object_flags_type().object_flags()
    } else {
        ObjectFlags::None
    }
}

pub fn type_has_call_or_construct_signatures(type_: &Type, checker: &TypeChecker) -> bool {
    unimplemented!()
}

pub fn is_umd_export_symbol<TSymbol: Borrow<Symbol>>(symbol: Option<TSymbol>) -> bool {
    unimplemented!()
}

pub fn is_type_node_kind(kind: SyntaxKind) -> bool {
    kind >= SyntaxKind::FirstTypeNode && kind <= SyntaxKind::LastTypeNode
        || matches!(
            kind,
            SyntaxKind::AnyKeyword
                | SyntaxKind::UnknownKeyword
                | SyntaxKind::NumberKeyword
                | SyntaxKind::BigIntKeyword
                | SyntaxKind::ObjectKeyword
                | SyntaxKind::BooleanKeyword
                | SyntaxKind::StringKeyword
                | SyntaxKind::SymbolKeyword
                | SyntaxKind::VoidKeyword
                | SyntaxKind::UndefinedKeyword
                | SyntaxKind::NeverKeyword
                | SyntaxKind::ExpressionWithTypeArguments
                | SyntaxKind::JSDocAllType
                | SyntaxKind::JSDocUnknownType
                | SyntaxKind::JSDocNullableType
                | SyntaxKind::JSDocNonNullableType
                | SyntaxKind::JSDocOptionalType
                | SyntaxKind::JSDocFunctionType
                | SyntaxKind::JSDocVariadicType
        )
}

pub fn is_access_expression(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression
    )
}

pub fn get_leftmost_access_expression(node: &Node /*Expression*/) -> Rc<Node /*Expression*/> {
    unimplemented!()
}

pub fn get_leftmost_expression(
    node: &Node, /*Expression*/
    stop_at_call_expressions: bool,
) -> Rc<Node /*Expression*/> {
    let mut node = node.node_wrapper();
    loop {
        match node.kind() {
            SyntaxKind::PostfixUnaryExpression => {
                node = node.as_postfix_unary_expression().operand.clone();
                continue;
            }

            SyntaxKind::BinaryExpression => {
                node = node.as_binary_expression().left.clone();
                continue;
            }

            SyntaxKind::ConditionalExpression => {
                node = node.as_conditional_expression().condition.clone();
                continue;
            }

            SyntaxKind::TaggedTemplateExpression => {
                node = node.as_tagged_template_expression().tag.clone();
                continue;
            }

            SyntaxKind::CallExpression => {
                if stop_at_call_expressions {
                    return node;
                }
                node = node.as_has_expression().expression();
                continue;
            }
            SyntaxKind::AsExpression
            | SyntaxKind::ElementAccessExpression
            | SyntaxKind::PropertyAccessExpression
            | SyntaxKind::NonNullExpression
            | SyntaxKind::PartiallyEmittedExpression => {
                node = node.as_has_expression().expression();
                continue;
            }
            _ => (),
        }

        return node;
    }
}

#[allow(non_snake_case)]
fn Symbol(flags: SymbolFlags, name: __String) -> BaseSymbol {
    BaseSymbol::new(flags, name)
}

#[allow(non_snake_case)]
fn _Type(flags: TypeFlags) -> BaseType {
    BaseType::new(flags)
}

#[allow(non_snake_case)]
fn _Signature(flags: SignatureFlags) -> Signature {
    Signature::new(flags)
}

#[allow(non_snake_case)]
fn Node(kind: SyntaxKind, pos: isize, end: isize) -> BaseNode {
    BaseNode::new(kind, NodeFlags::None, TransformFlags::None, pos, end)
}

#[allow(non_snake_case)]
fn Token(kind: SyntaxKind, pos: isize, end: isize) -> BaseNode {
    BaseNode::new(kind, NodeFlags::None, TransformFlags::None, pos, end)
}

#[allow(non_snake_case)]
fn Identifier(kind: SyntaxKind, pos: isize, end: isize) -> BaseNode {
    BaseNode::new(kind, NodeFlags::None, TransformFlags::None, pos, end)
}

pub struct ObjectAllocator {}

impl ObjectAllocator {
    pub fn get_node_constructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        Node
    }

    pub fn get_token_constructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        Token
    }

    pub fn get_identifier_constructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        Identifier
    }

    pub fn get_private_identifier_constructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        Node
    }

    pub fn get_source_file_constructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        Node
    }

    pub fn get_symbol_constructor(&self) -> fn(SymbolFlags, __String) -> BaseSymbol {
        Symbol
    }

    pub fn get_type_constructor(&self) -> fn(TypeFlags) -> BaseType {
        _Type
    }

    pub fn get_signature_constructor(&self) -> fn(SignatureFlags) -> Signature {
        _Signature
    }
}

lazy_static! {
    pub static ref object_allocator: ObjectAllocator = ObjectAllocator {};
}

pub fn format_string_from_args(text: &str, args: Vec<String>) -> String {
    let re = Regex::new(r"\{(\d+)\}").unwrap();
    re.replace_all(text, |captures: &Captures| {
        let index = captures.get(1).unwrap().as_str().parse::<usize>().unwrap();
        Debug_.check_defined(args.get(index), None)
    })
    .to_string()
}

thread_local! {
    pub static localized_diagnostic_messages: RefCell<Option<MapLike<String>>> = RefCell::new(None);
}

pub(crate) fn set_localized_diagnostic_messages(messages: Option<MapLike<String>>) {
    localized_diagnostic_messages.with(|localized_diagnostic_messages_| {
        *localized_diagnostic_messages_.borrow_mut() = messages;
    })
}

pub fn get_locale_specific_message(message: &DiagnosticMessage) -> String {
    message.message.to_string()
}

pub fn create_detached_diagnostic(
    file_name: &str,
    start: isize,
    length: isize,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticWithDetachedLocation {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        if !args.is_empty() {
            text = format_string_from_args(&text, args);
        }
    }

    DiagnosticWithDetachedLocation::new(
        BaseDiagnostic::new(
            BaseDiagnosticRelatedInformation::new(
                message.category,
                message.code,
                None,
                Some(start),
                Some(length),
                text,
            ),
            None,
        ),
        file_name.to_string(),
    )
}

fn is_diagnostic_with_detached_location(
    diagnostic: &DiagnosticRelatedInformation, /*DiagnosticRelatedInformation | DiagnosticWithDetachedLocation*/
) -> bool {
    matches!(
        diagnostic,
        DiagnosticRelatedInformation::Diagnostic(Diagnostic::DiagnosticWithDetachedLocation(_))
    )
}

pub fn attach_file_to_diagnostic(
    diagnostic: &DiagnosticWithDetachedLocation,
    file: &Node, /*SourceFile*/
) -> DiagnosticWithLocation {
    let file_as_source_file = file.as_source_file();
    let file_name = file_as_source_file.file_name();
    let length: isize = file_as_source_file.text().len().try_into().unwrap();
    Debug_.assert_equal(&diagnostic.file_name, &*file_name, None, None);
    Debug_.assert_less_than_or_equal(diagnostic.start(), length);
    Debug_.assert_less_than_or_equal(diagnostic.start() + diagnostic.length(), length);
    let mut diagnostic_with_location = DiagnosticWithLocation::new(BaseDiagnostic::new(
        BaseDiagnosticRelatedInformation::new(
            diagnostic.category(),
            diagnostic.code(),
            Some(file.node_wrapper()),
            Some(diagnostic.start()),
            Some(diagnostic.length()),
            diagnostic.message_text().clone(),
        ),
        None,
    ));
    if let Some(related_information) = diagnostic.related_information().as_ref() {
        *diagnostic_with_location.related_information() = Some(
            related_information
                .iter()
                .map(|related| {
                    if is_diagnostic_with_detached_location(related)
                        && &related.as_diagnostic_with_detached_location().file_name == &*file_name
                    {
                        Debug_.assert_less_than_or_equal(related.start(), length);
                        Debug_
                            .assert_less_than_or_equal(related.start() + related.length(), length);
                        Rc::new(
                            attach_file_to_diagnostic(
                                related.as_diagnostic_with_detached_location(),
                                file,
                            )
                            .into(),
                        )
                    } else {
                        related.clone()
                    }
                })
                .collect(),
        );
    }
    diagnostic_with_location
}

pub fn attach_file_to_diagnostics(
    diagnostics: &[Rc<Diagnostic /*DiagnosticWithDetachedLocation*/>],
    file: &Node, /*SourceFile*/
) -> Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>> {
    diagnostics
        .iter()
        .map(|diagnostic| {
            Rc::new(
                attach_file_to_diagnostic(diagnostic.as_diagnostic_with_detached_location(), file)
                    .into(),
            )
        })
        .collect()
}

pub fn create_file_diagnostic(
    file: &Node, /*SourceFile*/
    start: isize,
    length: isize,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticWithLocation {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        if !args.is_empty() {
            text = format_string_from_args(&text, args);
        }
    }

    DiagnosticWithLocation::new(BaseDiagnostic::new(
        BaseDiagnosticRelatedInformation::new(
            message.category,
            message.code,
            Some(file.node_wrapper()),
            Some(start),
            Some(length),
            text,
        ),
        None,
    ))
}

pub fn format_message(
    _dummy: Option<()>,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> String {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        if !args.is_empty() {
            text = format_string_from_args(&text, args);
        }
    }

    text
}

pub fn create_compiler_diagnostic(
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> BaseDiagnostic {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        if !args.is_empty() {
            text = format_string_from_args(&text, args);
        }
    }

    BaseDiagnostic::new(
        BaseDiagnosticRelatedInformation::new(
            message.category,
            message.code,
            None,
            None,
            None,
            text,
        ),
        None,
    )
}
