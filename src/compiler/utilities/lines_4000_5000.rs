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
    is_identifier, is_jsdoc, is_jsdoc_signature, is_jsdoc_template_tag, is_jsdoc_type_alias,
    is_jsdoc_type_tag, is_left_hand_side_expression, is_module_declaration,
    is_no_substituion_template_literal, is_numeric_literal, is_object_literal_expression,
    is_parenthesized_expression, is_private_identifier, is_property_access_entity_name_expression,
    is_property_access_expression, is_source_file, is_string_literal_like, is_variable_statement,
    is_void_expression, is_white_space_like, last, length, maybe_text_char_at_index,
    module_resolution_option_declarations, options_affecting_program_structure,
    skip_outer_expressions, some, str_to_source_text_as_chars, text_substring,
    AssignmentDeclarationKind, CommandLineOption, CommandLineOptionInterface, CommentRange,
    CompilerOptions, CompilerOptionsValue, DiagnosticWithDetachedLocation, DiagnosticWithLocation,
    EmitFlags, EmitTextWriter, Extension, LanguageVariant, LiteralLikeNodeInterface, MapLike,
    ModifierFlags, ModuleKind, Node, NodeArray, NodeFlags, NodeInterface, ObjectFlags,
    OuterExpressionKinds, PrefixUnaryExpression, PseudoBigInt, ReadonlyTextRange, ScriptKind,
    ScriptTarget, Signature, SignatureFlags, SortedArray, SourceFileLike, SourceTextAsChars,
    Symbol, SymbolFlags, SymbolInterface, SymbolTable, SymbolTracker, SymbolWriter, SyntaxKind,
    TextRange, TextSpan, TokenFlags, TransformFlags, TransientSymbolInterface, Type, TypeFlags,
    TypeInterface, UnderscoreEscapedMap, __String, compare_strings_case_sensitive, compare_values,
    create_text_span_from_bounds, escape_leading_underscores, for_each, get_combined_node_flags,
    get_name_of_declaration, insert_sorted, is_big_int_literal, is_member_name,
    is_type_alias_declaration, skip_trivia, BaseDiagnostic, BaseDiagnosticRelatedInformation,
    BaseNode, BaseSymbol, BaseType, CharacterCodes, CheckFlags, Comparison, Debug_, Diagnostic,
    DiagnosticCollection, DiagnosticInterface, DiagnosticMessage, DiagnosticMessageChain,
    DiagnosticMessageText, DiagnosticRelatedInformation, DiagnosticRelatedInformationInterface,
};
use local_macros::enum_unwrapped;

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
