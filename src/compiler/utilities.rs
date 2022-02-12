#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use regex::{Captures, Regex};
use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::cmp;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use crate::{
    add_range, compare_strings_case_sensitive_maybe, compute_line_starts, concatenate, filter,
    find_ancestor, first_or_undefined, flat_map, for_each_child_bool, get_jsdoc_parameter_tags,
    get_jsdoc_parameter_tags_no_cache, get_jsdoc_tags, get_jsdoc_type_parameter_tags,
    get_jsdoc_type_parameter_tags_no_cache, get_leading_comment_ranges,
    get_trailing_comment_ranges, has_initializer, has_jsdoc_nodes, id_text, is_binary_expression,
    is_call_expression, is_element_access_expression, is_export_declaration,
    is_expression_statement, is_function_like, is_function_like_or_class_static_block_declaration,
    is_identifier, is_jsdoc, is_jsdoc_signature, is_jsdoc_template_tag, is_jsdoc_type_tag,
    is_left_hand_side_expression, is_module_declaration, is_no_substituion_template_literal,
    is_numeric_literal, is_object_literal_expression, is_parenthesized_expression,
    is_private_identifier, is_property_access_expression, is_source_file, is_string_literal_like,
    is_variable_statement, is_void_expression, is_white_space_like, last, length,
    maybe_text_char_at_index, module_resolution_option_declarations,
    options_affecting_program_structure, skip_outer_expressions, some, str_to_source_text_as_chars,
    text_substring, AssignmentDeclarationKind, CommandLineOption, CommandLineOptionInterface,
    CommentRange, CompilerOptions, CompilerOptionsValue, DiagnosticWithDetachedLocation,
    DiagnosticWithLocation, EmitFlags, EmitTextWriter, Extension, LanguageVariant,
    LiteralLikeNodeInterface, MapLike, ModifierFlags, ModuleKind, Node, NodeArray, NodeFlags,
    NodeInterface, ObjectFlags, OuterExpressionKinds, PrefixUnaryExpression, PseudoBigInt,
    ReadonlyTextRange, ScriptKind, ScriptTarget, Signature, SignatureFlags, SortedArray,
    SourceFileLike, SourceTextAsChars, Symbol, SymbolFlags, SymbolInterface, SymbolTable,
    SymbolTracker, SymbolWriter, SyntaxKind, TextRange, TextSpan, TokenFlags, TransformFlags,
    TransientSymbolInterface, Type, TypeFlags, TypeInterface, UnderscoreEscapedMap, __String,
    compare_strings_case_sensitive, compare_values, create_text_span_from_bounds,
    escape_leading_underscores, for_each, get_combined_node_flags, get_name_of_declaration,
    insert_sorted, is_big_int_literal, is_member_name, is_type_alias_declaration, skip_trivia,
    BaseDiagnostic, BaseDiagnosticRelatedInformation, BaseNode, BaseSymbol, BaseType,
    CharacterCodes, CheckFlags, Comparison, Debug_, Diagnostic, DiagnosticCollection,
    DiagnosticInterface, DiagnosticMessage, DiagnosticMessageChain, DiagnosticMessageText,
    DiagnosticRelatedInformation, DiagnosticRelatedInformationInterface,
};
use local_macros::enum_unwrapped;

// resolvingEmptyArray: never[] = [];

pub const external_helpers_module_name_text: &str = "tslib";

pub const default_maximum_truncation_length: usize = 160;
pub const no_truncation_maximum_truncation_length: usize = 1_000_000;

pub fn get_declaration_of_kind(
    symbol: &Symbol,
    kind: SyntaxKind,
) -> Option<Rc<Node /*T extends Declaration*/>> {
    let maybe_declarations = symbol.maybe_declarations();
    let declarations = maybe_declarations.as_ref();
    if let Some(declarations) = declarations {
        for declaration in declarations {
            if declaration.kind() == kind {
                return Some(declaration.clone());
            }
        }
    }

    None
}

pub fn create_underscore_escaped_map<TValue>() -> UnderscoreEscapedMap<TValue> {
    UnderscoreEscapedMap::new()
}

// function hasEntries

pub fn create_symbol_table(symbols: Option<&[Rc<Symbol>]>) -> SymbolTable {
    let mut result = SymbolTable::new();
    if let Some(symbols) = symbols {
        for symbol in symbols {
            result.insert(symbol.escaped_name().clone(), symbol.clone());
        }
    }
    result
}

pub fn is_transient_symbol(symbol: &Symbol) -> bool {
    symbol.flags().intersects(SymbolFlags::Transient)
}

// lazy_static! {
//     static ref string_writer: Rc<RefCell<dyn EmitTextWriter>> = create_single_line_string_writer();
// }

fn string_writer() -> Rc<RefCell<dyn EmitTextWriter>> {
    create_single_line_string_writer()
}

fn create_single_line_string_writer() -> Rc<RefCell<dyn EmitTextWriter>> {
    Rc::new(RefCell::new(SingleLineStringWriter::new()))
}

struct SingleLineStringWriter {
    str: String,
}

impl SingleLineStringWriter {
    pub fn new() -> Self {
        Self {
            str: "".to_string(),
        }
    }

    fn write_text(&mut self, text: &str) {
        self.str.push_str(text);
    }
}

impl EmitTextWriter for SingleLineStringWriter {
    fn get_text(&self) -> String {
        self.str.clone()
    }

    fn write(&mut self, text: &str) {
        self.write_text(text);
    }

    fn raw_write(&mut self, text: &str) {
        self.write_text(text);
    }

    fn write_literal(&mut self, text: &str) {
        self.write_text(text);
    }

    fn write_trailing_semicolon(&mut self, text: &str) {
        self.write_text(text);
    }

    fn write_comment(&mut self, text: &str) {
        self.write_text(text);
    }

    fn get_text_pos(&self) -> usize {
        self.str.len()
    }

    fn get_line(&self) -> usize {
        0
    }

    fn get_column(&self) -> usize {
        0
    }

    fn get_indent(&self) -> usize {
        0
    }

    fn is_at_start_of_line(&self) -> bool {
        false
    }

    fn has_trailing_comment(&self) -> bool {
        false
    }

    fn has_trailing_whitespace(&self) -> bool {
        !self.str.is_empty() && is_white_space_like(self.str.chars().last().unwrap())
    }
}

impl SymbolWriter for SingleLineStringWriter {
    fn write_keyword(&mut self, text: &str) {
        self.write_text(text);
    }

    fn write_operator(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_punctuation(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_space(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_string_literal(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_parameter(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_property(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_symbol(&mut self, s: &str, _: &Symbol) {
        self.write_text(s);
    }

    fn write_line(&mut self, _: Option<bool>) {
        self.str.push_str(" ");
    }

    fn increase_indent(&mut self) {}

    fn decrease_indent(&mut self) {}

    fn clear(&mut self) {
        self.str = "".to_string();
    }
}

impl SymbolTracker for SingleLineStringWriter {
    fn track_symbol(
        &mut self,
        symbol: &Symbol,
        enclosing_declaration: Option<Rc<Node>>,
        meaning: SymbolFlags,
    ) -> Option<bool> {
        Some(false)
    }

    fn report_inaccessible_this_error(&mut self) {}

    fn report_inaccessible_unique_symbol_error(&mut self) {}

    fn report_private_in_base_of_class_expression(&mut self, property_name: &str) {}
}

pub fn changes_affect_module_resolution(
    old_options: &CompilerOptions,
    new_options: &CompilerOptions,
) -> bool {
    old_options.config_file_path != new_options.config_file_path
        || options_have_module_resolution_changes(old_options, new_options)
}

pub fn options_have_module_resolution_changes(
    old_options: &CompilerOptions,
    new_options: &CompilerOptions,
) -> bool {
    module_resolution_option_declarations.with(|module_resolution_option_declarations_| {
        options_have_changes(
            old_options,
            new_options,
            &*module_resolution_option_declarations_,
        )
    })
}

pub fn changes_affecting_program_structure(
    old_options: &CompilerOptions,
    new_options: &CompilerOptions,
) -> bool {
    options_affecting_program_structure.with(|options_affecting_program_structure_| {
        options_have_changes(
            old_options,
            new_options,
            &*options_affecting_program_structure_,
        )
    })
}

pub fn options_have_changes(
    old_options: &CompilerOptions,
    new_options: &CompilerOptions,
    option_declarations: &[Rc<CommandLineOption>],
) -> bool {
    !ptr::eq(old_options, new_options)
        && option_declarations.iter().any(|o| {
            /* !is_json_equal(*/
            get_compiler_option_value(old_options, o) != get_compiler_option_value(new_options, o)
            /*)*/
        })
}

pub enum ForEachAncestorReturn<TReturn> {
    Option(Option<TReturn>),
    Quit,
}

impl<TReturn> From<Option<TReturn>> for ForEachAncestorReturn<TReturn> {
    fn from(value: Option<TReturn>) -> Self {
        Self::Option(value)
    }
}

pub fn for_each_ancestor<
    TReturn,
    TCallbackReturn: Into<ForEachAncestorReturn<TReturn>>,
    TCallback: FnMut(&Node) -> TCallbackReturn,
>(
    node: &Node,
    mut callback: TCallback,
) -> Option<TReturn> {
    let mut node = node.node_wrapper();
    loop {
        let res = callback(&node).into();
        match res {
            ForEachAncestorReturn::Quit => {
                return None;
            }
            ForEachAncestorReturn::Option(option) => {
                if option.is_some() {
                    return option;
                }
            }
        }
        if is_source_file(&*node) {
            return None;
        }
        node = node.parent();
    }
}

fn get_source_text_of_node_from_source_file(
    source_file: &Node, /*SourceFile*/
    node: &Node,
    include_trivia: Option<bool>,
) -> String {
    let include_trivia = include_trivia.unwrap_or(false);
    get_text_of_node_from_source_text(
        &*source_file.as_source_file().text_as_chars(),
        node,
        Some(include_trivia),
    )
}

pub fn get_text_of_node_from_source_text(
    source_text: &SourceTextAsChars,
    node: &Node,
    include_trivia: Option<bool>,
) -> String {
    let include_trivia = include_trivia.unwrap_or(false);
    if node_is_missing(Some(node)) {
        return "".to_string();
    }

    let start = if include_trivia {
        node.pos()
    } else {
        skip_trivia(source_text, node.pos(), None, None, None)
    };
    let end = node.end();
    if !(start >= 0 && end >= 0 && end - start >= 0) {
        return "".to_string();
    }
    let start = start as usize;
    let end = end as usize;
    let text = text_substring(source_text, start, end);

    text
}

fn get_text_of_node(node: &Node, include_trivia: Option<bool>) -> String {
    let include_trivia = include_trivia.unwrap_or(false);
    get_source_text_of_node_from_source_file(
        &*get_source_file_of_node(node),
        node,
        Some(include_trivia),
    )
}

pub fn get_emit_flags(node: &Node) -> EmitFlags {
    EmitFlags::None
}

bitflags! {
    pub struct GetLiteralTextFlags: u32 {
        const None = 0;
        const NeverAsciiEscape = 1 << 0;
        const JsxAttributeEscape = 1 << 1;
        const TerminateUnterminatedLiterals = 1 << 2;
        const AllowNumericSeparator = 1 << 3;
    }
}

pub fn get_literal_text<TNodeRef: Borrow<Node>>(
    node: &Node, /*LiteralLikeNode*/
    source_file: Option<TNodeRef /*SourceFile*/>,
    flags: GetLiteralTextFlags,
) -> String {
    if can_use_original_text(node, flags) {
        return get_source_text_of_node_from_source_file(source_file.unwrap().borrow(), node, None);
    }

    match node {
        Node::StringLiteral(node_as_string_literal) => {
            let escape_text = if flags.intersects(GetLiteralTextFlags::JsxAttributeEscape) {
                unimplemented!()
            } else if flags.intersects(GetLiteralTextFlags::NeverAsciiEscape)
                || get_emit_flags(node).intersects(EmitFlags::NoAsciiEscaping)
            {
                unimplemented!()
            } else {
                escape_non_ascii_string
            };
            if matches!(node_as_string_literal.single_quote, Some(true)) {
                format!(
                    "'{}'",
                    escape_text(
                        &*node_as_string_literal.text(),
                        Some(CharacterCodes::single_quote)
                    )
                )
            } else {
                format!(
                    "\"{}\"",
                    escape_text(
                        &*node_as_string_literal.text(),
                        Some(CharacterCodes::double_quote)
                    )
                )
            }
        }
        Node::TemplateLiteralLikeNode(node_as_template_literal_like_node) => {
            let escape_text = if flags.intersects(GetLiteralTextFlags::NeverAsciiEscape)
                || get_emit_flags(node).intersects(EmitFlags::NoAsciiEscaping)
            {
                unimplemented!()
            } else {
                escape_non_ascii_string
            };

            let raw_text = node_as_template_literal_like_node
                .raw_text
                .clone()
                .unwrap_or_else(|| {
                    escape_template_substitution(&escape_text(
                        &*node_as_template_literal_like_node.text(),
                        Some(CharacterCodes::backtick),
                    ))
                });

            match node.kind() {
                SyntaxKind::NoSubstitutionTemplateLiteral => format!("`{}`", raw_text),
                SyntaxKind::TemplateHead => format!("`{}${{", raw_text),
                SyntaxKind::TemplateMiddle => format!("}}{}${{", raw_text),
                SyntaxKind::TemplateTail => format!("}}{}`", raw_text),
                _ => panic!("Unexpected TemplateLiteralLikeNode kind"),
            }
        }
        Node::NumericLiteral(_) | Node::BigIntLiteral(_) => {
            node.as_literal_like_node().text().to_string()
        }
        _ => Debug_.fail(Some(&format!(
            "Literal kind '{:?}' not accounted for.",
            node.kind()
        ))),
    }
}

fn can_use_original_text(node: &Node /*LiteralLikeNode*/, flags: GetLiteralTextFlags) -> bool {
    if node_is_synthesized(node)
        || node.maybe_parent().is_none()
        || flags.intersects(GetLiteralTextFlags::TerminateUnterminatedLiterals)
            && matches!(node.as_literal_like_node().is_unterminated(), Some(true))
    {
        return false;
    }

    !is_big_int_literal(node)
}

pub fn is_block_or_catch_scoped(declaration: &Node /*Declaration*/) -> bool {
    get_combined_node_flags(declaration).intersects(NodeFlags::BlockScoped)
        || is_catch_clause_variable_declaration_or_binding_element(declaration)
}

fn is_catch_clause_variable_declaration_or_binding_element(
    declaration: &Node, /*Declaration*/
) -> bool {
    let node = get_root_declaration(declaration);
    node.kind() == SyntaxKind::VariableDeclaration
        && node.parent().kind() == SyntaxKind::CatchClause
}

pub fn is_ambient_module(node: &Node) -> bool {
    is_module_declaration(node)
        && (node.as_module_declaration().name.kind() == SyntaxKind::StringLiteral
            || is_global_scope_augmentation(node))
}

pub fn is_global_scope_augmentation(node: &Node /*ModuleDeclaration*/) -> bool {
    node.flags().intersects(NodeFlags::GlobalAugmentation)
}

pub fn using_single_line_string_writer<TAction: FnOnce(Rc<RefCell<dyn EmitTextWriter>>)>(
    action: TAction,
) -> String {
    let string_writer = string_writer();
    let old_string = (*string_writer).borrow().get_text();
    action(string_writer.clone());
    let mut string_writer = string_writer.borrow_mut();
    let ret = string_writer.get_text();
    string_writer.clear();
    string_writer.write_keyword(&old_string);
    ret
}

pub fn get_full_width(node: &Node) -> isize {
    node.end() - node.pos()
}

pub fn contains_parse_error(node: &Node) -> bool {
    aggregate_child_data(node);
    node.flags()
        .intersects(NodeFlags::ThisNodeOrAnySubNodesHasError)
}

fn aggregate_child_data(node: &Node) {
    if !node.flags().intersects(NodeFlags::HasAggregatedChildData) {
        let this_node_or_any_sub_nodes_has_error =
            node.flags().intersects(NodeFlags::ThisNodeHasError)
                || for_each_child_bool(
                    node,
                    |child| contains_parse_error(child),
                    Option::<fn(&NodeArray) -> bool>::None,
                );

        if this_node_or_any_sub_nodes_has_error {
            node.set_flags(node.flags() | NodeFlags::ThisNodeOrAnySubNodesHasError);
        }

        node.set_flags(node.flags() | NodeFlags::HasAggregatedChildData);
    }
}

pub fn get_source_file_of_node(node: &Node) -> Rc<Node /*SourceFile*/> {
    if node.kind() == SyntaxKind::SourceFile {
        unimplemented!()
    }
    let mut parent = node.parent();
    while parent.kind() != SyntaxKind::SourceFile {
        parent = parent.parent();
    }
    parent.clone()
}

pub fn node_is_missing<TNodeRef: Borrow<Node>>(node: Option<TNodeRef>) -> bool {
    if node.is_none() {
        return false;
    }
    let node = node.unwrap();
    let node = node.borrow();
    node.pos() == node.end() && node.pos() >= 0 && node.kind() != SyntaxKind::EndOfFileToken
}

pub fn node_is_present<TNodeRef: Borrow<Node>>(node: Option<TNodeRef>) -> bool {
    !node_is_missing(node)
}

pub fn is_any_import_syntax(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::ImportDeclaration | SyntaxKind::ImportEqualsDeclaration
    )
}

pub fn is_any_import_or_re_export(node: &Node) -> bool {
    is_any_import_syntax(node) || is_export_declaration(node)
}

pub fn declaration_name_to_string<TNodeRef: Borrow<Node>>(name: Option<TNodeRef>) -> String {
    match name {
        None => "(Missing)".to_string(),
        Some(name) => {
            let name = name.borrow();
            if get_full_width(name) == 0 {
                "(Missing)".to_string()
            } else {
                get_text_of_node(name, None)
            }
        }
    }
}

pub fn entity_name_to_string(
    name: &Node, /*EntityNameOrEntityNameExpression | JSDocMemberName | JsxTagNameExpression | PrivateIdentifier*/
) -> Cow<'static, str> {
    match name.kind() {
        SyntaxKind::ThisKeyword => "this".into(),
        SyntaxKind::Identifier | SyntaxKind::PrivateIdentifier => {
            if get_full_width(name) == 0 {
                id_text(name).into()
            } else {
                get_text_of_node(name, None).into()
            }
        }
        SyntaxKind::QualifiedName => {
            let name_as_qualified_name = name.as_qualified_name();
            format!(
                "{}.{}",
                entity_name_to_string(&name_as_qualified_name.left),
                entity_name_to_string(&name_as_qualified_name.right)
            )
            .into()
        }
        SyntaxKind::PropertyAccessExpression => {
            let name_as_property_access_expression = name.as_property_access_expression();
            if is_identifier(&*name_as_property_access_expression.name)
                || is_private_identifier(&*name_as_property_access_expression.name)
            {
                format!(
                    "{}.{}",
                    entity_name_to_string(&name_as_property_access_expression.expression),
                    entity_name_to_string(&name_as_property_access_expression.name)
                )
                .into()
            } else {
                Debug_.assert_never(&name_as_property_access_expression.name, None)
            }
        }
        SyntaxKind::JSDocMemberName => {
            let name_as_jsdoc_member_name = name.as_jsdoc_member_name();
            format!(
                "{}{}",
                entity_name_to_string(&name_as_jsdoc_member_name.left),
                entity_name_to_string(&name_as_jsdoc_member_name.right)
            )
            .into()
        }
        _ => Debug_.assert_never(name, None),
    }
}

pub fn create_diagnostic_for_node(
    node: &Node,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticWithLocation {
    let source_file = get_source_file_of_node(node);
    create_diagnostic_for_node_in_source_file(&source_file, node, message, args)
}

fn create_diagnostic_for_node_in_source_file(
    source_file: &Node, /*SourceFile*/
    node: &Node,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticWithLocation {
    let span = get_error_span_for_node(source_file, node);
    create_file_diagnostic(source_file, span.start, span.length, message, args)
}

pub fn create_diagnostic_for_node_from_message_chain(
    node: &Node,
    message_chain: DiagnosticMessageChain,
    related_information: Option<Vec<Rc<DiagnosticRelatedInformation>>>,
) -> DiagnosticWithLocation {
    let source_file = get_source_file_of_node(node);
    let span = get_error_span_for_node(&source_file, node);
    create_file_diagnostic_from_message_chain(
        &source_file,
        span.start,
        span.length,
        message_chain,
        related_information,
    )
}

fn create_file_diagnostic_from_message_chain(
    file: &Node, /*SourceFile*/
    start: isize,
    length: isize,
    message_chain: DiagnosticMessageChain,
    related_information: Option<Vec<Rc<DiagnosticRelatedInformation>>>,
) -> DiagnosticWithLocation {
    // assert_diagnostic_location(&*file, start, length);
    DiagnosticWithLocation::new(BaseDiagnostic::new(
        BaseDiagnosticRelatedInformation::new(
            message_chain.code,
            Some(file.node_wrapper()),
            Some(start),
            Some(length),
            message_chain,
        ),
        related_information,
    ))
}

fn get_error_span_for_node(source_file: &Node /*SourceFile*/, node: &Node) -> TextSpan {
    let error_node = node;

    let pos = error_node.pos();

    create_text_span_from_bounds(pos, error_node.end())
}

pub fn is_external_or_common_js_module(file: &Node /*SourceFile*/) -> bool {
    false
}

pub fn is_import_call(n: &Node) -> bool {
    match n {
        Node::CallExpression(call_expression) => {
            call_expression.expression.kind() == SyntaxKind::ImportKeyword
        }
        _ => false,
    }
}

pub fn is_object_literal_method(node: &Node) -> bool {
    /*node &&*/
    node.kind() == SyntaxKind::MethodDeclaration
        && node.parent().kind() == SyntaxKind::ObjectLiteralExpression
}

pub fn get_containing_function_or_class_static_block(
    node: &Node,
) -> Option<Rc<Node /*SignatureDeclaration | ClassStaticBlockDeclaration*/>> {
    find_ancestor(node.maybe_parent(), |node: &Node| {
        is_function_like_or_class_static_block_declaration(Some(node))
    })
}

pub fn is_super_property(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression
    ) && node.as_has_expression().expression().kind() == SyntaxKind::SuperKeyword
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
    filter(comment_ranges.as_deref(), |comment| {
        matches!(maybe_text_char_at_index(text, (comment.pos() + 1).try_into().unwrap()), Some(ch) if ch == CharacterCodes::asterisk)
            && matches!(maybe_text_char_at_index(text, (comment.pos() + 2).try_into().unwrap()), Some(ch) if ch == CharacterCodes::asterisk)
            && match maybe_text_char_at_index(text, (comment.pos() + 3).try_into().unwrap()) {
                None => true,
                Some(ch) if ch != CharacterCodes::slash => true,
                _ => false,
            }
    })
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

pub fn is_function_block(node: &Node) -> bool {
    /*node &&*/
    node.kind() == SyntaxKind::Block && is_function_like(node.maybe_parent())
}

pub fn is_in_js_file<TNode: Borrow<Node>>(node: Option<TNode>) -> bool {
    node.map_or(false, |node| {
        node.borrow().flags().intersects(NodeFlags::JavaScriptFile)
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

fn walk_up_parenthesized_expressions(node: &Node) -> Option<Rc<Node>> {
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

pub fn is_keyword(token: SyntaxKind) -> bool {
    SyntaxKind::FirstKeyword <= token && token <= SyntaxKind::LastKeyword
}

bitflags! {
    pub struct FunctionFlags: u32 {
        const Normal = 0;
        const Generator = 1 << 0;
        const Async = 1 << 1;
        const Invalid = 1 << 2;
        const AsyncGenerator = Self::Async.bits | Self::Generator.bits;
    }
}

pub fn get_function_flags<TNodeRef: Borrow<Node>>(
    node: Option<TNodeRef /*SignatureDeclaration*/>,
) -> FunctionFlags {
    if node.is_none() {
        return FunctionFlags::Invalid;
    }
    let node = node.unwrap();
    let node = node.borrow();

    let mut flags = FunctionFlags::Normal;
    match node.kind() {
        SyntaxKind::FunctionDeclaration
        | SyntaxKind::FunctionExpression
        | SyntaxKind::MethodDeclaration => {
            if node
                .as_function_like_declaration()
                .maybe_asterisk_token()
                .is_some()
            {
                flags |= FunctionFlags::Generator;
            }
            if has_syntactic_modifier(node, ModifierFlags::Async) {
                flags |= FunctionFlags::Async;
            }
        }
        SyntaxKind::ArrowFunction => {
            if has_syntactic_modifier(node, ModifierFlags::Async) {
                flags |= FunctionFlags::Async;
            }
        }
        _ => (),
    }

    if node
        .maybe_as_function_like_declaration()
        .and_then(|node| node.maybe_body())
        .is_none()
    {
        flags |= FunctionFlags::Invalid;
    }

    flags
}

pub fn is_string_or_numeric_literal_like(node: &Node) -> bool {
    is_string_literal_like(node) || is_numeric_literal(node)
}

pub fn has_dynamic_name(declaration: &Node /*Declaration*/) -> bool {
    let name = get_name_of_declaration(Some(declaration));
    if let Some(name) = name {
        is_dynamic_name(&name)
    } else {
        false
    }
}

fn is_dynamic_name(name: &Node /*DeclarationName*/) -> bool {
    false
}

pub fn is_property_name_literal(node: &Node) -> bool {
    match node.kind() {
        SyntaxKind::Identifier
        | SyntaxKind::StringLiteral
        | SyntaxKind::NoSubstitutionTemplateLiteral
        | SyntaxKind::NumericLiteral => true,
        _ => false,
    }
}

pub fn get_text_of_identifier_or_literal(node: &Node) -> String {
    if is_member_name(node) {
        id_text(node)
    } else {
        node.as_literal_like_node().text().to_owned()
    }
}

pub fn get_escaped_text_of_identifier_or_literal(node: &Node) -> __String {
    if is_member_name(node) {
        node.as_member_name().escaped_text()
    } else {
        escape_leading_underscores(&*node.as_literal_like_node().text())
    }
}

fn get_root_declaration(node: &Node) -> Rc<Node> {
    let mut node = node.node_wrapper();
    while node.kind() == SyntaxKind::BindingElement {
        node = node.parent().parent();
    }
    node
}

fn node_is_synthesized<TRange: ReadonlyTextRange>(range: &TRange) -> bool {
    position_is_synthesized(range.pos()) || position_is_synthesized(range.end())
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

pub fn get_expression_associativity(expression: &Node /*Expression*/) -> Associativity {
    let operator = get_operator(expression);
    let has_arguments = expression.kind() == SyntaxKind::NewExpression
        && expression.as_new_expression().arguments.is_some();
    get_operator_associativity(expression.kind(), operator, Some(has_arguments))
}

pub fn get_operator_associativity(
    kind: SyntaxKind,
    operator: SyntaxKind,
    has_arguments: Option<bool>,
) -> Associativity {
    let has_arguments = has_arguments.unwrap_or(false);
    match kind {
        SyntaxKind::NewExpression => {
            return if has_arguments {
                Associativity::Left
            } else {
                Associativity::Right
            };
        }

        SyntaxKind::PrefixUnaryExpression
        | SyntaxKind::TypeOfExpression
        | SyntaxKind::VoidExpression
        | SyntaxKind::DeleteExpression
        | SyntaxKind::AwaitExpression
        | SyntaxKind::ConditionalExpression
        | SyntaxKind::YieldExpression => {
            return Associativity::Right;
        }

        SyntaxKind::BinaryExpression => match operator {
            SyntaxKind::AsteriskAsteriskToken
            | SyntaxKind::EqualsToken
            | SyntaxKind::PlusEqualsToken
            | SyntaxKind::MinusEqualsToken
            | SyntaxKind::AsteriskAsteriskEqualsToken
            | SyntaxKind::AsteriskEqualsToken
            | SyntaxKind::SlashEqualsToken
            | SyntaxKind::PercentEqualsToken
            | SyntaxKind::LessThanLessThanEqualsToken
            | SyntaxKind::GreaterThanGreaterThanEqualsToken
            | SyntaxKind::GreaterThanGreaterThanGreaterThanEqualsToken
            | SyntaxKind::AmpersandEqualsToken
            | SyntaxKind::CaretEqualsToken
            | SyntaxKind::BarEqualsToken
            | SyntaxKind::BarBarEqualsToken
            | SyntaxKind::AmpersandAmpersandEqualsToken
            | SyntaxKind::QuestionQuestionEqualsToken => {
                return Associativity::Right;
            }
            _ => (),
        },
        _ => (),
    }
    Associativity::Left
}

pub fn get_expression_precedence(expression: &Node) -> OperatorPrecedence {
    let operator = get_operator(expression);
    let has_arguments = expression.kind() == SyntaxKind::NewExpression
        && expression.as_new_expression().arguments.is_some();
    get_operator_precedence(expression.kind(), operator, Some(has_arguments))
}

pub fn get_operator(expression: &Node) -> SyntaxKind {
    match expression {
        Node::BinaryExpression(expression) => expression.operator_token.kind(),
        Node::PrefixUnaryExpression(expression) => expression.operator,
        Node::PostfixUnaryExpression(expression) => expression.operator,
        _ => expression.kind(),
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum OperatorPrecedence {
    Comma,
    Spread,
    Yield,
    Assignment,
    Conditional,
    LogicalOR,
    LogicalAND,
    BitwiseOR,
    BitwiseXOR,
    BitwiseAND,
    Equality,
    Relational,
    Shift,
    Additive,
    Multiplicative,
    Exponentiation,
    Unary,
    Update,
    LeftHandSide,
    Member,
    Primary,
    Invalid = -1,
}

impl OperatorPrecedence {
    pub const Coalesce: OperatorPrecedence = OperatorPrecedence::Conditional;
    pub const Highest: OperatorPrecedence = OperatorPrecedence::Primary;
    pub const Lowest: OperatorPrecedence = OperatorPrecedence::Comma;
}

impl Ord for OperatorPrecedence {
    fn cmp(&self, other: &Self) -> Ordering {
        (*self as isize).cmp(&(*other as isize))
    }
}

impl PartialOrd for OperatorPrecedence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub fn get_operator_precedence(
    node_kind: SyntaxKind,
    operator_kind: SyntaxKind,
    has_arguments: Option<bool>,
) -> OperatorPrecedence {
    let has_arguments = has_arguments.unwrap_or(false);
    match node_kind {
        SyntaxKind::CommaListExpression => OperatorPrecedence::Comma,
        SyntaxKind::SpreadElement => OperatorPrecedence::Spread,
        SyntaxKind::YieldExpression => OperatorPrecedence::Yield,
        SyntaxKind::ConditionalExpression => OperatorPrecedence::Conditional,
        SyntaxKind::BinaryExpression => match operator_kind {
            SyntaxKind::CommaToken => OperatorPrecedence::Comma,
            SyntaxKind::EqualsToken
            | SyntaxKind::PlusEqualsToken
            | SyntaxKind::MinusEqualsToken
            | SyntaxKind::AsteriskAsteriskEqualsToken
            | SyntaxKind::AsteriskEqualsToken
            | SyntaxKind::SlashEqualsToken
            | SyntaxKind::PercentEqualsToken
            | SyntaxKind::LessThanLessThanEqualsToken
            | SyntaxKind::GreaterThanGreaterThanEqualsToken
            | SyntaxKind::GreaterThanGreaterThanGreaterThanEqualsToken
            | SyntaxKind::AmpersandEqualsToken
            | SyntaxKind::CaretEqualsToken
            | SyntaxKind::BarEqualsToken
            | SyntaxKind::BarBarEqualsToken
            | SyntaxKind::AmpersandAmpersandEqualsToken
            | SyntaxKind::QuestionQuestionEqualsToken => OperatorPrecedence::Assignment,
            _ => get_binary_operator_precedence(operator_kind),
        },
        SyntaxKind::TypeAssertionExpression
        | SyntaxKind::NonNullExpression
        | SyntaxKind::PrefixUnaryExpression
        | SyntaxKind::TypeOfExpression
        | SyntaxKind::VoidExpression
        | SyntaxKind::DeleteExpression
        | SyntaxKind::AwaitExpression => OperatorPrecedence::Unary,

        SyntaxKind::PostfixUnaryExpression => OperatorPrecedence::Update,

        SyntaxKind::CallExpression => OperatorPrecedence::LeftHandSide,

        SyntaxKind::NewExpression => {
            if has_arguments {
                OperatorPrecedence::Member
            } else {
                OperatorPrecedence::LeftHandSide
            }
        }

        SyntaxKind::TaggedTemplateExpression
        | SyntaxKind::PropertyAccessExpression
        | SyntaxKind::ElementAccessExpression
        | SyntaxKind::MetaProperty => OperatorPrecedence::Member,

        SyntaxKind::AsExpression => OperatorPrecedence::Relational,

        SyntaxKind::ThisKeyword
        | SyntaxKind::SuperKeyword
        | SyntaxKind::Identifier
        | SyntaxKind::PrivateIdentifier
        | SyntaxKind::NullKeyword
        | SyntaxKind::TrueKeyword
        | SyntaxKind::FalseKeyword
        | SyntaxKind::NumericLiteral
        | SyntaxKind::BigIntLiteral
        | SyntaxKind::StringLiteral
        | SyntaxKind::ArrayLiteralExpression
        | SyntaxKind::ObjectLiteralExpression
        | SyntaxKind::FunctionExpression
        | SyntaxKind::ArrowFunction
        | SyntaxKind::ClassExpression
        | SyntaxKind::RegularExpressionLiteral
        | SyntaxKind::NoSubstitutionTemplateLiteral
        | SyntaxKind::TemplateExpression
        | SyntaxKind::ParenthesizedExpression
        | SyntaxKind::OmittedExpression
        | SyntaxKind::JsxElement
        | SyntaxKind::JsxSelfClosingElement
        | SyntaxKind::JsxFragment => OperatorPrecedence::Primary,

        _ => OperatorPrecedence::Invalid,
    }
}

pub fn get_binary_operator_precedence(kind: SyntaxKind) -> OperatorPrecedence {
    match kind {
        SyntaxKind::QuestionQuestionToken => OperatorPrecedence::Coalesce,
        SyntaxKind::BarBarToken => OperatorPrecedence::LogicalOR,
        SyntaxKind::AmpersandAmpersandToken => OperatorPrecedence::LogicalAND,
        SyntaxKind::BarToken => OperatorPrecedence::BitwiseOR,
        SyntaxKind::CaretToken => OperatorPrecedence::BitwiseXOR,
        SyntaxKind::AmpersandToken => OperatorPrecedence::BitwiseAND,
        SyntaxKind::EqualsEqualsToken
        | SyntaxKind::ExclamationEqualsToken
        | SyntaxKind::EqualsEqualsEqualsToken
        | SyntaxKind::ExclamationEqualsEqualsToken => OperatorPrecedence::Equality,
        SyntaxKind::LessThanToken
        | SyntaxKind::GreaterThanToken
        | SyntaxKind::LessThanEqualsToken
        | SyntaxKind::GreaterThanEqualsToken
        | SyntaxKind::InstanceOfKeyword
        | SyntaxKind::InKeyword
        | SyntaxKind::AsKeyword => OperatorPrecedence::Relational,
        SyntaxKind::LessThanLessThanToken
        | SyntaxKind::GreaterThanGreaterThanToken
        | SyntaxKind::GreaterThanGreaterThanGreaterThanToken => OperatorPrecedence::Shift,
        SyntaxKind::PlusToken | SyntaxKind::MinusToken => OperatorPrecedence::Additive,
        SyntaxKind::AsteriskToken | SyntaxKind::SlashToken | SyntaxKind::PercentToken => {
            OperatorPrecedence::Multiplicative
        }
        SyntaxKind::AsteriskAsteriskToken => OperatorPrecedence::Exponentiation,
        _ => OperatorPrecedence::Invalid,
    }
}

pub fn create_diagnostic_collection() -> DiagnosticCollection {
    DiagnosticCollection::new()
}

impl DiagnosticCollection {
    pub fn new() -> Self {
        DiagnosticCollection {
            file_diagnostics: HashMap::<String, SortedArray<Rc<Diagnostic>>>::new(),
        }
    }

    pub fn add(&mut self, diagnostic: Rc<Diagnostic>) {
        if let Some(diagnostics) = self
            .file_diagnostics
            .get_mut(&*diagnostic.file().unwrap().as_source_file().file_name())
        {
            insert_sorted(
                diagnostics,
                diagnostic,
                |rc_diagnostic_a: &Rc<Diagnostic>, rc_diagnostic_b: &Rc<Diagnostic>| {
                    compare_diagnostics(&**rc_diagnostic_a, &**rc_diagnostic_b)
                },
            );
            return;
        }
        let diagnostics: SortedArray<Rc<Diagnostic>> = SortedArray::new(vec![]);
        self.file_diagnostics.insert(
            diagnostic
                .file()
                .unwrap()
                .as_source_file()
                .file_name()
                .to_string(),
            diagnostics,
        );
        let diagnostics = self
            .file_diagnostics
            .get_mut(&*diagnostic.file().unwrap().as_source_file().file_name())
            .unwrap();
        insert_sorted(
            diagnostics,
            diagnostic,
            |rc_diagnostic_a: &Rc<Diagnostic>, rc_diagnostic_b: &Rc<Diagnostic>| {
                compare_diagnostics(&**rc_diagnostic_a, &**rc_diagnostic_b)
            },
        );
    }

    pub fn get_diagnostics(&self, file_name: &str) -> Vec<Rc<Diagnostic>> {
        self.file_diagnostics
            .get(file_name)
            .map(|sorted_array| sorted_array.into())
            .unwrap_or(vec![])
    }
}

lazy_static! {
    static ref template_substitution_reg_exp: Regex = Regex::new(r"\$\{").unwrap();
}
fn escape_template_substitution(str: &str) -> String {
    template_substitution_reg_exp
        .replace_all(str, "\\${")
        .to_string()
}

pub(crate) fn has_invalid_escape(template: &Node /*TemplateLiteral*/) -> bool {
    /*template &&*/
    if is_no_substituion_template_literal(template) {
        matches!(template.as_template_literal_like_node().maybe_template_flags(), Some(template_flags) if template_flags != TokenFlags::None)
    } else {
        let template_as_template_expression = template.as_template_expression();
        matches!(template_as_template_expression.head.as_template_literal_like_node().maybe_template_flags(), Some(template_flags) if template_flags != TokenFlags::None)
            || some(
                Some(&template_as_template_expression.template_spans),
                Some(
                    |span: &Rc<Node>| matches!(span.as_template_span().literal.as_template_literal_like_node().maybe_template_flags(), Some(template_flags) if template_flags != TokenFlags::None),
                ),
            )
    }
}

fn escape_non_ascii_string(
    s: &str,
    quote_char: Option<
        char, /*CharacterCodes.doubleQuote | CharacterCodes.singleQuote | CharacterCodes.backtick*/
    >,
) -> String {
    s.to_string()
}

thread_local! {
    static indent_strings: Vec<&'static str> = vec!["", "    "];
}

pub fn get_indent_size() -> usize {
    indent_strings.with(|indent_strings_| indent_strings_[1].len())
}

#[derive(Clone)]
pub struct TextWriter {
    new_line: String,
    output: String,
    indent: usize,
    line_start: bool,
    line_count: usize,
    line_pos: usize,
    has_trailing_comment: bool,
}

impl TextWriter {
    pub fn new(new_line: &str) -> Self {
        Self {
            new_line: new_line.to_string(),
            output: String::new(),
            indent: 0,
            line_start: true,
            line_count: 0,
            line_pos: 0,
            has_trailing_comment: false,
        }
    }

    fn push_output(&mut self, str: &str) {
        self.output.push_str(str);
    }

    fn update_line_count_and_pos_for(&mut self, s: &str) {
        let line_starts_of_s = compute_line_starts(&str_to_source_text_as_chars(s));
        if line_starts_of_s.len() > 1 {
            self.line_count = self.line_count + line_starts_of_s.len() - 1;
            self.line_pos = self.output.len() - s.len() + last(&line_starts_of_s);
            self.line_start = (self.line_pos - self.output.len()) == 0;
        } else {
            self.line_start = false;
        }
    }

    fn write_text(&mut self, s: &str) {
        if !s.is_empty() {
            self.push_output(s);
        }
    }

    fn reset(&mut self) {
        self.output = String::new();
        self.indent = 0;
        self.line_start = true;
        self.line_count = 0;
        self.line_pos = 0;
        self.has_trailing_comment = false;
    }
}

impl EmitTextWriter for TextWriter {
    fn write(&mut self, s: &str) {
        self.write_text(s);
    }

    fn write_comment(&mut self, s: &str) {
        if !s.is_empty() {
            self.has_trailing_comment = true;
        }
        self.write_text(s);
    }

    fn raw_write(&mut self, s: &str) {
        // if (s!== undefined) {
        self.push_output(s);
        self.update_line_count_and_pos_for(s);
        self.has_trailing_comment = false;
        //}
    }

    fn write_literal(&mut self, s: &str) {
        if
        /*s && */
        !s.is_empty() {
            self.write(s);
        }
    }

    fn write_trailing_semicolon(&mut self, text: &str) {
        self.write(text);
    }

    fn get_text(&self) -> String {
        self.output.clone()
    }

    fn get_text_pos(&self) -> usize {
        self.output.len()
    }

    fn get_line(&self) -> usize {
        self.line_count
    }

    fn get_column(&self) -> usize {
        if self.line_start {
            self.indent * get_indent_size()
        } else {
            self.output.len() - self.line_pos
        }
    }

    fn get_indent(&self) -> usize {
        self.indent
    }

    fn is_at_start_of_line(&self) -> bool {
        self.line_start
    }

    fn has_trailing_comment(&self) -> bool {
        self.has_trailing_comment
    }

    fn has_trailing_whitespace(&self) -> bool {
        !self.output.is_empty() && is_white_space_like(self.output.chars().last().unwrap())
    }
}

impl SymbolWriter for TextWriter {
    fn write_line(&mut self, force: Option<bool>) {
        let force = force.unwrap_or(false);
        if !self.line_start || force {
            self.push_output(&self.new_line.clone());
            self.line_count += 1;
            self.line_start = true;
            self.has_trailing_comment = false;
        }
    }

    fn increase_indent(&mut self) {
        self.indent += 1;
    }

    fn decrease_indent(&mut self) {
        self.indent -= 1; // TODO: should use isize to avoid this crashing if misused?
    }

    fn write_keyword(&mut self, text: &str) {
        self.write(text);
    }

    fn write_operator(&mut self, text: &str) {
        self.write(text);
    }

    fn write_parameter(&mut self, text: &str) {
        self.write(text);
    }

    fn write_punctuation(&mut self, s: &str) {
        self.write(s);
    }

    fn write_space(&mut self, s: &str) {
        self.write(s);
    }

    fn write_string_literal(&mut self, s: &str) {
        self.write(s);
    }

    fn write_property(&mut self, s: &str) {
        self.write(s);
    }

    fn write_symbol(&mut self, s: &str, _: &Symbol) {
        self.write(s);
    }

    fn clear(&mut self) {
        self.reset();
    }
}

impl SymbolTracker for TextWriter {}

pub fn create_text_writer(new_line: &str) -> TextWriter {
    TextWriter::new(new_line)
    // text_writer.reset()
}

pub fn is_this_identifier<TNode: Borrow<Node>>(node: Option<TNode>) -> bool {
    if node.is_none() {
        return false;
    }
    let node = node.unwrap();
    let node = node.borrow();
    node.kind() == SyntaxKind::Identifier && identifier_is_this_keyword(node)
}

pub fn identifier_is_this_keyword(id: &Node /*Identifier*/) -> bool {
    matches!(
        id.as_identifier().original_keyword_kind,
        Some(SyntaxKind::ThisKeyword)
    )
}

pub fn get_effective_type_annotation_node(node: &Node) -> Option<Rc<Node /*TypeNode*/>> {
    let type_ = node
        .maybe_as_has_type()
        .and_then(|has_type| has_type.maybe_type());
    type_
}

pub fn get_effective_return_type_node(
    node: &Node, /*SignatureDeclaration | JSDocSignature*/
) -> Option<Rc<Node /*TypeNode*/>> {
    if is_jsdoc_signature(node) {
        unimplemented!()
    } else {
        node.as_signature_declaration().maybe_type().or_else(|| {
            if false {
                unimplemented!()
            } else {
                None
            }
        })
    }
}

pub fn get_jsdoc_type_parameter_declarations(
    node: &Node, /*DeclarationWithTypeParameters*/
) -> Vec<Rc<Node /*TypeParameterDeclaration*/>> {
    flat_map(Some(get_jsdoc_tags(node)), |tag, _| {
        if is_non_type_alias_template(&tag) {
            tag.as_jsdoc_template_tag().type_parameters.to_vec()
        } else {
            vec![]
        }
    })
}

pub fn is_non_type_alias_template(tag: &Node /*JSDocTag*/) -> bool {
    is_jsdoc_template_tag(tag)
        && !(tag.parent().kind() == SyntaxKind::JSDocComment
            && tag.parent().as_jsdoc().tags.as_ref().map_or(false, |tags| {
                tags.iter().any(|tag| is_jsdoc_type_alias(&**tag))
            }))
}

pub fn has_syntactic_modifier<TNode: NodeInterface>(node: &TNode, flags: ModifierFlags) -> bool {
    get_selected_syntactic_modifier_flags(node, flags) != ModifierFlags::None
}

pub fn has_static_modifier<TNode: NodeInterface>(node: &TNode) -> bool {
    has_syntactic_modifier(node, ModifierFlags::Static)
}

fn get_selected_syntactic_modifier_flags<TNode: NodeInterface>(
    node: &TNode,
    flags: ModifierFlags,
) -> ModifierFlags {
    get_syntactic_modifier_flags(node) & flags
}

fn get_modifier_flags_worker<TNode: NodeInterface>(
    node: &TNode,
    include_jsdoc: bool,
    always_include_jsdoc: Option<bool>,
) -> ModifierFlags {
    if node.kind() >= SyntaxKind::FirstToken && node.kind() <= SyntaxKind::LastToken {
        return ModifierFlags::None;
    }

    if !node
        .modifier_flags_cache()
        .intersects(ModifierFlags::HasComputedFlags)
    {
        node.set_modifier_flags_cache(
            get_syntactic_modifier_flags_no_cache(node) | ModifierFlags::HasComputedFlags,
        );
    }

    node.modifier_flags_cache()
        & !(ModifierFlags::HasComputedFlags | ModifierFlags::HasComputedJSDocModifiers)
}

pub fn get_effective_modifier_flags(node: &Node) -> ModifierFlags {
    get_modifier_flags_worker(node, true, None)
}

pub fn get_effective_modifier_flags_always_include_jsdoc(node: &Node) -> ModifierFlags {
    get_modifier_flags_worker(node, true, Some(true))
}

pub fn get_syntactic_modifier_flags<TNode: NodeInterface>(node: &TNode) -> ModifierFlags {
    get_modifier_flags_worker(node, false, None)
}

fn get_syntactic_modifier_flags_no_cache<TNode: NodeInterface>(node: &TNode) -> ModifierFlags {
    let mut flags = modifiers_to_flags(node.maybe_modifiers().as_ref());
    if node.flags().intersects(NodeFlags::NestedNamespace) || false {
        flags |= ModifierFlags::Export;
    }
    flags
}

pub fn modifiers_to_flags(modifiers: Option<&NodeArray /*Modifier[]*/>) -> ModifierFlags {
    let mut flags = ModifierFlags::None;
    if let Some(modifiers) = modifiers {
        for modifier in modifiers.iter() {
            flags |= modifier_to_flag(modifier.kind());
        }
    }
    flags
}

pub fn modifier_to_flag(token: SyntaxKind) -> ModifierFlags {
    match token {
        SyntaxKind::StaticKeyword => ModifierFlags::Static,
        SyntaxKind::PublicKeyword => ModifierFlags::Public,
        SyntaxKind::ProtectedKeyword => ModifierFlags::Protected,
        SyntaxKind::PrivateKeyword => ModifierFlags::Private,
        SyntaxKind::AbstractKeyword => ModifierFlags::Abstract,
        SyntaxKind::ExportKeyword => ModifierFlags::Export,
        SyntaxKind::DeclareKeyword => ModifierFlags::Ambient,
        SyntaxKind::ConstKeyword => ModifierFlags::Const,
        SyntaxKind::DefaultKeyword => ModifierFlags::Default,
        SyntaxKind::AsyncKeyword => ModifierFlags::Async,
        SyntaxKind::ReadonlyKeyword => ModifierFlags::Static,
        SyntaxKind::OverrideKeyword => ModifierFlags::Override,
        _ => ModifierFlags::None,
    }
}

pub fn is_logical_or_coalescing_assignment_operator(token: SyntaxKind) -> bool {
    matches!(
        token,
        SyntaxKind::BarBarEqualsToken
            | SyntaxKind::AmpersandAmpersandEqualsToken
            | SyntaxKind::QuestionQuestionEqualsToken
    )
}

pub fn is_assignment_operator(token: SyntaxKind) -> bool {
    token >= SyntaxKind::FirstAssignment && token <= SyntaxKind::LastAssignment
}

pub fn is_assignment_expression(node: &Node, exclude_compound_assignment: Option<bool>) -> bool {
    let exclude_compound_assignment = exclude_compound_assignment.unwrap_or(false);
    if !is_binary_expression(node) {
        return false;
    }
    let node_as_binary_expression = node.as_binary_expression();
    (if exclude_compound_assignment {
        node_as_binary_expression.operator_token.kind() == SyntaxKind::EqualsToken
    } else {
        is_assignment_operator(node_as_binary_expression.operator_token.kind())
    }) && is_left_hand_side_expression(&*node_as_binary_expression.left)
}

pub fn is_entity_name_expression(node: &Node) -> bool {
    node.kind() == SyntaxKind::Identifier || is_property_access_entity_name_expression(node)
}

pub fn get_first_identifier(node: &Node) -> Rc<Node /*Identifier*/> {
    match node {
        Node::Identifier(_) => node.node_wrapper(),
        _ => unimplemented!(),
    }
}

pub fn is_property_access_entity_name_expression(node: &Node) -> bool {
    if !is_property_access_expression(node) {
        return false;
    }
    let node_as_property_access_expression = node.as_property_access_expression();
    is_identifier(&*node_as_property_access_expression.name)
        && is_entity_name_expression(&*node_as_property_access_expression.expression)
}

pub fn is_prototype_access(node: &Node) -> bool {
    is_bindable_static_access_expression(node, None)
        && match get_element_or_property_access_name(node) {
            Some(name) => name.eq_str("prototype"),
            None => false,
        }
}

pub fn get_check_flags(symbol: &Symbol) -> CheckFlags {
    match symbol {
        Symbol::TransientSymbol(transient_symbol) => transient_symbol.check_flags(),
        _ => CheckFlags::None,
    }
}

pub fn is_write_only_access(node: &Node) -> bool {
    access_kind(node) == AccessKind::Write
}

fn is_write_access(node: &Node) -> bool {
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

pub fn get_object_flags(type_: &Type) -> ObjectFlags {
    if type_.flags().intersects(TypeFlags::ObjectFlagsType) {
        type_.as_object_flags_type().object_flags()
    } else {
        ObjectFlags::None
    }
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

fn format_string_from_args(text: &str, args: Vec<String>) -> String {
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

fn get_locale_specific_message(message: &DiagnosticMessage) -> String {
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

fn create_file_diagnostic(
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
            message.code,
            Some(file.node_wrapper()),
            Some(start),
            Some(length),
            text,
        ),
        None,
    ))
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
        BaseDiagnosticRelatedInformation::new(message.code, None, None, None, text),
        None,
    )
}

pub fn chain_diagnostic_messages(
    details: Option<DiagnosticMessageChain>,
    message: &DiagnosticMessage,
    args: Option<Vec<String>>,
) -> DiagnosticMessageChain {
    let mut text = get_locale_specific_message(message);

    if let Some(args) = args {
        text = format_string_from_args(&text, args);
    }
    DiagnosticMessageChain::new(text, message.code, details.map(|details| vec![details]))
}

fn get_diagnostic_file_path<
    TDiagnosticRelatedInformation: DiagnosticRelatedInformationInterface,
>(
    diagnostic: &TDiagnosticRelatedInformation,
) -> Option<String> {
    diagnostic.file().and_then(|file| {
        file.as_source_file()
            .maybe_path()
            .as_ref()
            .map(|path| path.to_string())
    })
}

pub fn compare_diagnostics<TDiagnosticRelatedInformation: DiagnosticRelatedInformationInterface>(
    d1: &TDiagnosticRelatedInformation,
    d2: &TDiagnosticRelatedInformation,
) -> Comparison {
    let mut compared = compare_diagnostics_skip_related_information(d1, d2);
    if compared != Comparison::EqualTo {
        return compared;
    }
    if let Some(d1) = d1.maybe_as_diagnostic() {
        if let Some(d2) = d2.maybe_as_diagnostic() {
            compared = compare_related_information(d1, d2);
            if compared != Comparison::EqualTo {
                return compared;
            }
        }
    }
    Comparison::EqualTo
}

fn compare_diagnostics_skip_related_information<
    TDiagnosticRelatedInformation: DiagnosticRelatedInformationInterface,
>(
    d1: &TDiagnosticRelatedInformation,
    d2: &TDiagnosticRelatedInformation,
) -> Comparison {
    let mut compared = compare_strings_case_sensitive_maybe(
        get_diagnostic_file_path(d1).as_deref(),
        get_diagnostic_file_path(d2).as_deref(),
    );
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_values(Some(d1.start()), Some(d2.start()));
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_values(Some(d1.length()), Some(d2.length()));
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_values(Some(d1.code()), Some(d2.code()));
    if compared != Comparison::EqualTo {
        return compared;
    }
    compared = compare_message_text(d1.message_text(), d2.message_text());
    if compared != Comparison::EqualTo {
        return compared;
    }
    Comparison::EqualTo
}

fn compare_related_information(d1: &Diagnostic, d2: &Diagnostic) -> Comparison {
    if d1.related_information().is_none() && d2.related_information().is_none() {
        return Comparison::EqualTo;
    }
    if let Some(d1_related_information) = d1.related_information().as_ref() {
        if let Some(d2_related_information) = d2.related_information().as_ref() {
            let compared = compare_values(
                Some(d1_related_information.len()),
                Some(d2_related_information.len()),
            );
            if compared != Comparison::EqualTo {
                return compared;
            }
            let compared_maybe = for_each(d1_related_information, |d1i, index| {
                let d2i = &d2_related_information[index];
                let compared = compare_diagnostics(&**d1i, &**d2i);
                match compared {
                    Comparison::EqualTo => None,
                    compared => Some(compared),
                }
            });
            if let Some(compared) = compared_maybe {
                if compared != Comparison::EqualTo {
                    return compared;
                }
            }
            return Comparison::EqualTo;
        }
    }
    if d1.related_information().is_some() {
        Comparison::LessThan
    } else {
        Comparison::GreaterThan
    }
}

fn compare_message_text(t1: &DiagnosticMessageText, t2: &DiagnosticMessageText) -> Comparison {
    if let DiagnosticMessageText::String(t1) = t1 {
        if let DiagnosticMessageText::String(t2) = t2 {
            return compare_strings_case_sensitive(t1, t2);
        }
    }
    if matches!(t1, DiagnosticMessageText::String(_)) {
        return Comparison::LessThan;
    }
    if matches!(t2, DiagnosticMessageText::String(_)) {
        return Comparison::GreaterThan;
    }
    let t1 = enum_unwrapped!(t1, [DiagnosticMessageText, DiagnosticMessageChain]);
    let t2 = enum_unwrapped!(t2, [DiagnosticMessageText, DiagnosticMessageChain]);
    let mut res = compare_strings_case_sensitive(&t1.message_text, &t2.message_text);
    if res != Comparison::EqualTo {
        return res;
    }
    if t1.next.is_none() && t2.next.is_none() {
        return Comparison::EqualTo;
    }
    if t1.next.is_none() {
        return Comparison::LessThan;
    }
    if t2.next.is_none() {
        return Comparison::GreaterThan;
    }
    let t1_next = t1.next.as_ref().unwrap();
    let t2_next = t2.next.as_ref().unwrap();
    let len = cmp::min(t1_next.len(), t2_next.len());
    for i in 0..len {
        res = compare_message_text(&t1_next[i].clone().into(), &t2_next[i].clone().into());
        if res != Comparison::EqualTo {
            return res;
        }
    }
    if t1_next.len() < t2_next.len() {
        return Comparison::LessThan;
    }
    if t1_next.len() > t2_next.len() {
        return Comparison::GreaterThan;
    }
    Comparison::EqualTo
}

pub fn get_language_variant(script_kind: ScriptKind) -> LanguageVariant {
    match script_kind {
        ScriptKind::TSX | ScriptKind::JSX | ScriptKind::JS | ScriptKind::JSON => {
            LanguageVariant::JSX
        }
        _ => LanguageVariant::Standard,
    }
}

pub fn get_emit_script_target(compiler_options: &CompilerOptions) -> ScriptTarget {
    compiler_options.target.unwrap_or_else(|| {
        if matches!(compiler_options.module, Some(ModuleKind::Node12)) {
            ScriptTarget::ES2020
        } else if matches!(compiler_options.module, Some(ModuleKind::NodeNext)) {
            ScriptTarget::ESNext
        } else {
            ScriptTarget::ES3
        }
    })
}

fn lookup_compiler_option_value(options: &CompilerOptions, name: &str) -> CompilerOptionsValue {
    match name {
        "all" => CompilerOptionsValue::Bool(options.all.clone()),
        "allow_js" => CompilerOptionsValue::Bool(options.allow_js.clone()),
        "allow_non_ts_extensions" => {
            CompilerOptionsValue::Bool(options.allow_non_ts_extensions.clone())
        }
        "allow_synthetic_default_imports" => {
            CompilerOptionsValue::Bool(options.allow_synthetic_default_imports.clone())
        }
        "allow_umd_global_access" => {
            CompilerOptionsValue::Bool(options.allow_umd_global_access.clone())
        }
        "allow_unreachable_code" => {
            CompilerOptionsValue::Bool(options.allow_unreachable_code.clone())
        }
        "allow_unused_labels" => CompilerOptionsValue::Bool(options.allow_unused_labels.clone()),
        "always_strict" => CompilerOptionsValue::Bool(options.always_strict.clone()),
        "base_url" => CompilerOptionsValue::String(options.base_url.clone()),
        "build" => CompilerOptionsValue::Bool(options.build.clone()),
        "charset" => CompilerOptionsValue::String(options.charset.clone()),
        "check_js" => CompilerOptionsValue::Bool(options.check_js.clone()),
        "config_file_path" => CompilerOptionsValue::String(options.config_file_path.clone()),
        "config_file" => CompilerOptionsValue::SourceFile(options.config_file.clone()),
        "declaration" => CompilerOptionsValue::Bool(options.declaration.clone()),
        "declaration_map" => CompilerOptionsValue::Bool(options.declaration_map.clone()),
        "emit_declaration_only" => {
            CompilerOptionsValue::Bool(options.emit_declaration_only.clone())
        }
        "declaration_dir" => CompilerOptionsValue::String(options.declaration_dir.clone()),
        "diagnostics" => CompilerOptionsValue::Bool(options.diagnostics.clone()),
        "extended_diagnostics" => CompilerOptionsValue::Bool(options.extended_diagnostics.clone()),
        "disable_size_limit" => CompilerOptionsValue::Bool(options.disable_size_limit.clone()),
        "disable_source_of_project_reference_redirect" => {
            CompilerOptionsValue::Bool(options.disable_source_of_project_reference_redirect.clone())
        }
        "disable_solution_searching" => {
            CompilerOptionsValue::Bool(options.disable_solution_searching.clone())
        }
        "disable_referenced_project_load" => {
            CompilerOptionsValue::Bool(options.disable_referenced_project_load.clone())
        }
        "downlevel_iteration" => CompilerOptionsValue::Bool(options.downlevel_iteration.clone()),
        "emit_bom" => CompilerOptionsValue::Bool(options.emit_bom.clone()),
        "emit_decorator_metadata" => {
            CompilerOptionsValue::Bool(options.emit_decorator_metadata.clone())
        }
        "exact_optional_property_types" => {
            CompilerOptionsValue::Bool(options.exact_optional_property_types.clone())
        }
        "experimental_decorators" => {
            CompilerOptionsValue::Bool(options.experimental_decorators.clone())
        }
        "force_consistent_casing_in_file_names" => {
            CompilerOptionsValue::Bool(options.force_consistent_casing_in_file_names.clone())
        }
        "generate_cpu_profile" => {
            CompilerOptionsValue::String(options.generate_cpu_profile.clone())
        }
        "generate_trace" => CompilerOptionsValue::String(options.generate_trace.clone()),
        "help" => CompilerOptionsValue::Bool(options.help.clone()),
        "import_helpers" => CompilerOptionsValue::Bool(options.import_helpers.clone()),
        "imports_not_used_as_values" => {
            CompilerOptionsValue::ImportsNotUsedAsValues(options.imports_not_used_as_values.clone())
        }
        "init" => CompilerOptionsValue::Bool(options.init.clone()),
        "inline_source_map" => CompilerOptionsValue::Bool(options.inline_source_map.clone()),
        "inline_sources" => CompilerOptionsValue::Bool(options.inline_sources.clone()),
        "isolated_modules" => CompilerOptionsValue::Bool(options.isolated_modules.clone()),
        "jsx" => CompilerOptionsValue::JsxEmit(options.jsx.clone()),
        "keyof_strings_only" => CompilerOptionsValue::Bool(options.keyof_strings_only.clone()),
        "lib" => CompilerOptionsValue::VecString(options.lib.clone()),
        "list_emitted_files" => CompilerOptionsValue::Bool(options.list_emitted_files.clone()),
        "list_files" => CompilerOptionsValue::Bool(options.list_files.clone()),
        "explain_files" => CompilerOptionsValue::Bool(options.explain_files.clone()),
        "list_files_only" => CompilerOptionsValue::Bool(options.list_files_only.clone()),
        "locale" => CompilerOptionsValue::String(options.locale.clone()),
        "map_root" => CompilerOptionsValue::String(options.map_root.clone()),
        "max_node_module_js_depth" => {
            CompilerOptionsValue::Usize(options.max_node_module_js_depth.clone())
        }
        "module" => CompilerOptionsValue::ModuleKind(options.module.clone()),
        "module_resolution" => {
            CompilerOptionsValue::ModuleResolutionKind(options.module_resolution.clone())
        }
        "new_line" => CompilerOptionsValue::NewLineKind(options.new_line.clone()),
        "no_emit" => CompilerOptionsValue::Bool(options.no_emit.clone()),
        "no_emit_for_js_files" => CompilerOptionsValue::Bool(options.no_emit_for_js_files.clone()),
        "no_emit_helpers" => CompilerOptionsValue::Bool(options.no_emit_helpers.clone()),
        "no_emit_on_error" => CompilerOptionsValue::Bool(options.no_emit_on_error.clone()),
        "no_error_truncation" => CompilerOptionsValue::Bool(options.no_error_truncation.clone()),
        "no_fallthrough_cases_in_switch" => {
            CompilerOptionsValue::Bool(options.no_fallthrough_cases_in_switch.clone())
        }
        "no_implicit_any" => CompilerOptionsValue::Bool(options.no_implicit_any.clone()),
        "no_implicit_returns" => CompilerOptionsValue::Bool(options.no_implicit_returns.clone()),
        "no_implicit_this" => CompilerOptionsValue::Bool(options.no_implicit_this.clone()),
        "no_strict_generic_checks" => {
            CompilerOptionsValue::Bool(options.no_strict_generic_checks.clone())
        }
        "no_unused_locals" => CompilerOptionsValue::Bool(options.no_unused_locals.clone()),
        "no_unused_parameters" => CompilerOptionsValue::Bool(options.no_unused_parameters.clone()),
        "no_implicit_use_strict" => {
            CompilerOptionsValue::Bool(options.no_implicit_use_strict.clone())
        }
        "no_property_access_from_index_signature" => {
            CompilerOptionsValue::Bool(options.no_property_access_from_index_signature.clone())
        }
        "assume_changes_only_affect_direct_dependencies" => CompilerOptionsValue::Bool(
            options
                .assume_changes_only_affect_direct_dependencies
                .clone(),
        ),
        "no_lib" => CompilerOptionsValue::Bool(options.no_lib.clone()),
        "no_resolve" => CompilerOptionsValue::Bool(options.no_resolve.clone()),
        "no_unchecked_indexed_access" => {
            CompilerOptionsValue::Bool(options.no_unchecked_indexed_access.clone())
        }
        "out" => CompilerOptionsValue::String(options.out.clone()),
        "out_dir" => CompilerOptionsValue::String(options.out_dir.clone()),
        "out_file" => CompilerOptionsValue::String(options.out_file.clone()),
        "paths" => CompilerOptionsValue::MapLikeVecString(options.paths.clone()),
        "paths_base_path" => CompilerOptionsValue::String(options.paths_base_path.clone()),
        "plugins" => CompilerOptionsValue::VecPluginImport(options.plugins.clone()),
        "preserve_const_enums" => CompilerOptionsValue::Bool(options.preserve_const_enums.clone()),
        "no_implicit_override" => CompilerOptionsValue::Bool(options.no_implicit_override.clone()),
        "preserve_symlinks" => CompilerOptionsValue::Bool(options.preserve_symlinks.clone()),
        "preserve_value_imports" => {
            CompilerOptionsValue::Bool(options.preserve_value_imports.clone())
        }
        "preserve_watch_output" => {
            CompilerOptionsValue::Bool(options.preserve_watch_output.clone())
        }
        "project" => CompilerOptionsValue::String(options.project.clone()),
        "pretty" => CompilerOptionsValue::Bool(options.pretty.clone()),
        "react_namespace" => CompilerOptionsValue::String(options.react_namespace.clone()),
        "jsx_factory" => CompilerOptionsValue::String(options.jsx_factory.clone()),
        "jsx_fragment_factory" => {
            CompilerOptionsValue::String(options.jsx_fragment_factory.clone())
        }
        "jsx_import_source" => CompilerOptionsValue::String(options.jsx_import_source.clone()),
        "composite" => CompilerOptionsValue::Bool(options.composite.clone()),
        "incremental" => CompilerOptionsValue::Bool(options.incremental.clone()),
        "ts_build_info_file" => CompilerOptionsValue::String(options.ts_build_info_file.clone()),
        "remove_comments" => CompilerOptionsValue::Bool(options.remove_comments.clone()),
        "root_dir" => CompilerOptionsValue::String(options.root_dir.clone()),
        "root_dirs" => CompilerOptionsValue::VecString(options.root_dirs.clone()),
        "skip_lib_check" => CompilerOptionsValue::Bool(options.skip_lib_check.clone()),
        "skip_default_lib_check" => {
            CompilerOptionsValue::Bool(options.skip_default_lib_check.clone())
        }
        "source_map" => CompilerOptionsValue::Bool(options.source_map.clone()),
        "source_root" => CompilerOptionsValue::String(options.source_root.clone()),
        "strict" => CompilerOptionsValue::Bool(options.strict.clone()),
        "strict_function_types" => {
            CompilerOptionsValue::Bool(options.strict_function_types.clone())
        }
        "strict_bind_call_apply" => {
            CompilerOptionsValue::Bool(options.strict_bind_call_apply.clone())
        }
        "strict_null_checks" => CompilerOptionsValue::Bool(options.strict_null_checks.clone()),
        "strict_property_initialization" => {
            CompilerOptionsValue::Bool(options.strict_property_initialization.clone())
        }
        "strip_internal" => CompilerOptionsValue::Bool(options.strip_internal.clone()),
        "suppress_excess_property_errors" => {
            CompilerOptionsValue::Bool(options.suppress_excess_property_errors.clone())
        }
        "suppress_implicit_any_index_errors" => {
            CompilerOptionsValue::Bool(options.suppress_implicit_any_index_errors.clone())
        }
        "suppress_output_path_check" => {
            CompilerOptionsValue::Bool(options.suppress_output_path_check.clone())
        }
        "target" => CompilerOptionsValue::ScriptTarget(options.target.clone()),
        "trace_resolution" => CompilerOptionsValue::Bool(options.trace_resolution.clone()),
        "use_unknown_in_catch_variables" => {
            CompilerOptionsValue::Bool(options.use_unknown_in_catch_variables.clone())
        }
        "resolve_json_module" => CompilerOptionsValue::Bool(options.resolve_json_module.clone()),
        "types" => CompilerOptionsValue::VecString(options.types.clone()),
        "type_roots" => CompilerOptionsValue::VecString(options.type_roots.clone()),
        "version" => CompilerOptionsValue::Bool(options.version.clone()),
        "watch" => CompilerOptionsValue::Bool(options.watch.clone()),
        "es_module_interop" => CompilerOptionsValue::Bool(options.es_module_interop.clone()),
        "show_config" => CompilerOptionsValue::Bool(options.show_config.clone()),
        "use_define_for_class_fields" => {
            CompilerOptionsValue::Bool(options.use_define_for_class_fields.clone())
        }
        _ => panic!("Unknown compiler option: {:?}", name),
    }
}

pub fn get_strict_option_value(
    compiler_options: &CompilerOptions,
    flag: &str, /*StrictOptionName*/
) -> bool {
    match lookup_compiler_option_value(compiler_options, flag).as_option_bool() {
        None => compiler_options.strict.unwrap_or(false),
        Some(bool_) => bool_,
    }
}

pub fn get_compiler_option_value(
    options: &CompilerOptions,
    option: &CommandLineOption,
) -> CompilerOptionsValue {
    if option.strict_flag() {
        CompilerOptionsValue::Bool(Some(get_strict_option_value(options, option.name())))
    } else {
        lookup_compiler_option_value(options, option.name())
    }
}

pub fn ensure_script_kind(file_name: &str, script_kind: Option<ScriptKind>) -> ScriptKind {
    script_kind.unwrap_or_else(|| {
        let script_kind = get_script_kind_from_file_name(file_name);
        if script_kind == ScriptKind::Unknown {
            ScriptKind::TS
        } else {
            script_kind
        }
    })
}

pub fn get_script_kind_from_file_name(file_name: &str) -> ScriptKind {
    let ext = file_name
        .rfind('.')
        .map(|extension_index| file_name[extension_index..].to_owned())
        .and_then(|ext| Extension::maybe_from_str(&ext));
    match ext {
        Some(Extension::Js) | Some(Extension::Cjs) | Some(Extension::Mjs) => ScriptKind::JS,
        Some(Extension::Jsx) => ScriptKind::JSX,
        Some(Extension::Ts) | Some(Extension::Cts) | Some(Extension::Mts) => ScriptKind::TS,
        Some(Extension::Tsx) => ScriptKind::TSX,
        Some(Extension::Json) => ScriptKind::JSON,
        _ => ScriptKind::Unknown,
    }
}

pub fn position_is_synthesized(pos: isize) -> bool {
    !(pos >= 0)
}

pub fn add_related_info(
    diagnostic: &Diagnostic,
    related_information: Vec<Rc<DiagnosticRelatedInformation>>,
) {
    if related_information.is_empty() {
        return /*diagnostic*/;
    }
    let mut diagnostic_related_information = diagnostic.related_information();
    if diagnostic_related_information.is_none() {
        *diagnostic_related_information = Some(vec![]);
    }
    // Debug.assert(diagnostic.relatedInformation !== emptyArray, "Diagnostic had empty array singleton for related info, but is still being constructed!");
    diagnostic_related_information
        .as_mut()
        .unwrap()
        .extend(related_information);
    // return diagnostic;
}

pub fn parse_pseudo_big_int(string_value: &str) -> String {
    string_value.to_string()
}

pub fn pseudo_big_int_to_string(pseudo_big_int: &PseudoBigInt) -> String {
    let negative = pseudo_big_int.negative;
    let base_10_value = &pseudo_big_int.base_10_value;
    format!(
        "{}{}",
        if negative && base_10_value != "0" {
            "-"
        } else {
            ""
        },
        base_10_value
    )
}

fn set_text_range_pos<TRange: ReadonlyTextRange>(range: &TRange, pos: isize) -> &TRange {
    range.set_pos(pos);
    range
}

fn set_text_range_end<TRange: ReadonlyTextRange>(range: &TRange, end: isize) -> &TRange {
    range.set_end(end);
    range
}

pub fn set_text_range_pos_end<TRange: ReadonlyTextRange>(range: &TRange, pos: isize, end: isize) {
    set_text_range_end(set_text_range_pos(range, pos), end);
}

pub fn set_text_range_pos_width<TRange: ReadonlyTextRange>(
    range: &TRange,
    pos: isize,
    width: isize,
) {
    set_text_range_pos_end(range, pos, pos + width);
}

pub fn set_parent(child: &Node, parent: Option<Rc<Node>>) -> &Node {
    if let Some(parent) = parent {
        child.set_parent(parent.clone());
    }
    child
}

pub fn set_parent_recursive<TNode: Borrow<Node>>(root_node: Option<TNode>, incremental: bool) {
    unimplemented!()
}

pub fn is_function_expression_or_arrow_function(node: &Node) -> bool {
    matches!(
        node.kind(),
        SyntaxKind::FunctionExpression | SyntaxKind::ArrowFunction
    )
}
