use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::FromIterator;
use std::rc::Rc;

use super::{brackets, PipelinePhase};
use crate::{
    create_text_writer, get_emit_flags, get_literal_text, get_parse_tree_node, id_text,
    is_expression, is_identifier, is_keyword, positions_are_on_same_line, skip_trivia,
    token_to_string, Debug_, EmitFlags, EmitHint, EmitTextWriter, GetLiteralTextFlags,
    HasTypeArgumentsInterface, HasTypeInterface, ListFormat, NamedDeclarationInterface, Node,
    NodeArray, NodeInterface, Printer, ReadonlyTextRange, SourceFileLike, SourceMapGenerator,
    Symbol, SyntaxKind, TextRange,
};

impl Printer {
    pub fn write_bundle(
        &self,
        bundle: &Node, /*Bundle*/
        output: Rc<dyn EmitTextWriter>,
        source_map_generator: Option<Rc<dyn SourceMapGenerator>>,
    ) {
        unimplemented!()
    }

    pub fn write_unparsed_source(
        &self,
        unparsed: &Node, /*UnparsedSource*/
        output: Rc<dyn EmitTextWriter>,
    ) {
        unimplemented!()
    }

    pub fn write_file(
        &self,
        source_file: &Node, /*SourceFile*/
        output: Rc<dyn EmitTextWriter>,
        source_map_generator: Option<Rc<dyn SourceMapGenerator>>,
    ) {
        unimplemented!()
    }

    pub(super) fn begin_print(&self) -> Rc<dyn EmitTextWriter> {
        self.own_writer
            .borrow_mut()
            .get_or_insert_with(|| Rc::new(create_text_writer(&self.new_line)))
            .clone()
    }

    pub(super) fn end_print(&self) -> String {
        let own_writer = self.own_writer.borrow();
        let own_writer = own_writer.as_ref().unwrap();
        let text = own_writer.get_text();
        own_writer.clear();
        text
    }

    pub(super) fn print(
        &self,
        hint: EmitHint,
        node: &Node,
        source_file: Option<&Node /*SourceFile*/>,
    ) {
        if let Some(source_file) = source_file {
            self.set_source_file(Some(source_file));
        }

        self.pipeline_emit(hint, node);
    }

    pub(super) fn set_source_file(&self, source_file: Option<&Node /*SourceFile*/>) {
        *self.current_source_file.borrow_mut() =
            source_file.map(|source_file| source_file.node_wrapper());
    }

    pub(super) fn set_writer(
        &self,
        writer: Option<Rc<dyn EmitTextWriter>>,
        source_map_generator: Option<Rc<dyn SourceMapGenerator>>,
    ) {
        *self.writer.borrow_mut() = writer;
    }

    pub(super) fn reset(&self) {
        self.set_writer(
            None, None, //TODO this might be wrong
        );
    }

    pub(super) fn emit(&self, node: Option<&Node>) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        self.pipeline_emit(EmitHint::Unspecified, node);
    }

    pub(super) fn emit_expression(&self, node: &Node /*Expression*/) {
        self.pipeline_emit(EmitHint::Expression, node);
    }

    pub(super) fn pipeline_emit(&self, emit_hint: EmitHint, node: &Node) {
        let pipeline_phase = self.get_pipeline_phase(PipelinePhase::Notification, emit_hint, node);
        pipeline_phase(self, emit_hint, node);
    }

    pub(super) fn get_pipeline_phase(
        &self,
        phase: PipelinePhase,
        emit_hint: EmitHint,
        node: &Node,
    ) -> fn(&Printer, EmitHint, &Node) {
        if phase == PipelinePhase::Notification {
            if false {
                unimplemented!()
            }
        }
        if phase == PipelinePhase::Notification || phase == PipelinePhase::Emit {
            return Printer::pipeline_emit_with_hint;
        }
        Debug_.assert_never(phase, None);
    }

    pub(super) fn pipeline_emit_with_hint(&self, hint: EmitHint, node: &Node) {
        if false {
            unimplemented!()
        } else {
            self.pipeline_emit_with_hint_worker(hint, node);
        }
    }

    pub(super) fn pipeline_emit_with_hint_worker(&self, mut hint: EmitHint, node: &Node) {
        if hint == EmitHint::Unspecified {
            match node.kind() {
                SyntaxKind::Identifier => return self.emit_identifier(node),
                SyntaxKind::PropertySignature => return self.emit_property_signature(node),
                SyntaxKind::CallSignature => return self.emit_call_signature(node),
                SyntaxKind::TypeReference => return self.emit_type_reference(node),
                SyntaxKind::FunctionType => return self.emit_function_type(node),
                SyntaxKind::ConstructorType => return self.emit_constructor_type(node),
                SyntaxKind::TypeLiteral => return self.emit_type_literal(node),
                SyntaxKind::ArrayType => return self.emit_array_type(node),
                SyntaxKind::TupleType => return self.emit_tuple_type(node),
                SyntaxKind::UnionType => return self.emit_union_type(node),
                SyntaxKind::IntersectionType => return self.emit_intersection_type(node),
                SyntaxKind::ConditionalType => return self.emit_conditional_type(node),
                SyntaxKind::ParenthesizedType => return self.emit_parenthesized_type(node),
                SyntaxKind::TypeOperator => return self.emit_type_operator(node),
                SyntaxKind::IndexedAccessType => return self.emit_indexed_access_type(node),
                SyntaxKind::LiteralType => return self.emit_literal_type(node),
                SyntaxKind::ImportType => return self.emit_import_type(node),
                _ => (),
            }
            if is_expression(node) {
                hint = EmitHint::Expression;
            }
        }
        if hint == EmitHint::Expression {
            match node {
                Node::StringLiteral(_)
                | Node::TemplateLiteralLikeNode(_)
                | Node::NumericLiteral(_)
                | Node::BigIntLiteral(_)
                | Node::RegularExpressionLiteral(_)
                | Node::JsxText(_) => {
                    return self.emit_literal(node, false);
                }
                _ => (),
            }
        }
        if is_keyword(node.kind()) {
            return self.write_token_node(node, Printer::write_keyword);
        }
        unimplemented!("hint: {:?}, kind: {:?}", hint, node.kind());
    }

    pub(super) fn emit_literal(
        &self,
        node: &Node, /*LiteralLikeNode*/
        jsx_attribute_escape: bool,
    ) {
        let text = self.get_literal_text_of_node(node);
        if false {
            unimplemented!()
        } else {
            self.write_string_literal(&text);
        }
    }

    pub(super) fn emit_identifier(&self, node: &Node /*Identifier*/) {
        let text_of_node = self.get_text_of_node(node, Some(false));
        if let Some(symbol) = node.maybe_symbol() {
            self.write_symbol(&text_of_node, &symbol);
        } else {
            self.write(&text_of_node);
        }
    }

    pub(super) fn emit_property_signature(&self, node: &Node /*PropertySignature*/) {
        let node_as_property_signature = node.as_property_signature();
        self.emit_node_with_writer(
            Some(&*node_as_property_signature.name()),
            Printer::write_property,
        );
        self.emit_type_annotation(node_as_property_signature.maybe_type());
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_call_signature(&self, node: &Node /*CallSignature*/) {
        // unimplemented!()
        self.write_punctuation("TODO call signature");
    }

    pub(super) fn emit_type_reference(&self, node: &Node /*TypeReferenceNode*/) {
        let node_as_type_reference_node = node.as_type_reference_node();
        self.emit(Some(&*node_as_type_reference_node.type_name));
        self.emit_type_arguments(
            node,
            node_as_type_reference_node.maybe_type_arguments().as_ref(),
        );
    }

    pub(super) fn emit_function_type(&self, node: &Node /*FunctionTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO function type");
    }

    pub(super) fn emit_constructor_type(&self, node: &Node /*ConstructorTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO constructor type");
    }

    pub(super) fn emit_type_literal(&self, node: &Node /*TypeLiteralNode*/) {
        self.write_punctuation("{");
        let flags = if true {
            ListFormat::SingleLineTypeLiteralMembers
        } else {
            unimplemented!()
        };
        self.emit_list(
            Some(node),
            Some(&node.as_type_literal_node().members),
            flags | ListFormat::NoSpaceIfEmpty,
        );
        self.write_punctuation("}");
    }

    pub(super) fn emit_array_type(&self, node: &Node /*ArrayTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO array type");
    }

    pub(super) fn emit_tuple_type(&self, node: &Node /*TupleTypeNode*/) {
        self.emit_token_with_comment(
            SyntaxKind::OpenBracketToken,
            node.pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        let flags = if get_emit_flags(node).intersects(EmitFlags::SingleLine) {
            ListFormat::SingleLineTupleTypeElements
        } else {
            ListFormat::MultiLineTupleTypeElements
        };
        let node_elements = &node.as_tuple_type_node().elements;
        self.emit_list(
            Some(node),
            Some(&node_elements),
            flags | ListFormat::NoSpaceIfEmpty,
        );
        self.emit_token_with_comment(
            SyntaxKind::CloseBracketToken,
            node_elements.end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
    }

    pub(super) fn emit_union_type(&self, node: &Node /*UnionTypeNode*/) {
        self.emit_list(
            Some(node),
            Some(&node.as_union_or_intersection_type_node().types()),
            ListFormat::UnionTypeConstituents,
        );
    }

    pub(super) fn emit_intersection_type(&self, node: &Node /*IntersectionTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO intersection type");
    }

    pub(super) fn emit_conditional_type(&self, node: &Node /*ConditionalTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO conditional type");
    }

    pub(super) fn emit_parenthesized_type(&self, node: &Node /*ParenthesizedTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO parenthesized type");
    }

    pub(super) fn emit_type_operator(&self, node: &Node /*TypeOperatorNode*/) {
        let node_as_type_operator_node = node.as_type_operator_node();
        self.write_token_text(
            node_as_type_operator_node.operator,
            |text| self.write_keyword(text),
            None,
        );
        self.write_space();
        self.emit(
            Some(&*node_as_type_operator_node.type_),
            // parenthesizer.parenthesizeMemberOfElementType
        );
    }

    pub(super) fn emit_indexed_access_type(&self, node: &Node /*IndexedAccessType*/) {
        // unimplemented!()
        self.write_punctuation("TODO indexed access type");
    }

    pub(super) fn emit_literal_type(&self, node: &Node /*LiteralTypeNode*/) {
        self.emit_expression(&*node.as_literal_type_node().literal);
    }

    pub(super) fn emit_import_type(&self, node: &Node /*ImportTypeNode*/) {
        // unimplemented!()
        self.write_punctuation("TODO import type");
    }

    pub(super) fn emit_token_with_comment<TWriter: FnMut(&str)>(
        &self,
        token: SyntaxKind,
        mut pos: isize,
        writer: TWriter,
        context_node: &Node,
        indent_leading: Option<bool>,
    ) -> isize {
        let node = get_parse_tree_node(Some(context_node), Option::<fn(&Node) -> bool>::None);
        let is_similar_node = matches!(
            node.as_ref(),
            Some(node) if node.kind() == context_node.kind()
        );
        let start_pos = pos;
        if is_similar_node {
            if let Some(current_source_file) = self.maybe_current_source_file().as_ref() {
                pos = skip_trivia(
                    &current_source_file.as_source_file().text_as_chars(),
                    pos,
                    None,
                    None,
                    None,
                );
            }
        }
        if is_similar_node && context_node.pos() != start_pos {
            let needs_indent = indent_leading == Some(true)
                && matches!(
                    self.maybe_current_source_file().as_ref(),
                    Some(current_source_file) if !positions_are_on_same_line(
                        start_pos.try_into().unwrap(),
                        pos.try_into().unwrap(),
                        current_source_file,
                    )
                );
            if needs_indent {
                self.increase_indent();
            }
            self.emit_leading_comments_of_position(start_pos);
            if needs_indent {
                self.decrease_indent();
            }
        }
        pos = self.write_token_text(token, writer, Some(pos)).unwrap();
        if is_similar_node && context_node.end() != pos {
            let is_jsx_expr_context = context_node.kind() == SyntaxKind::JsxExpression;
            self.emit_trailing_comments_of_position(
                pos,
                Some(!is_jsx_expr_context),
                Some(is_jsx_expr_context),
            );
        }
        pos
    }

    pub(super) fn emit_node_with_writer(&self, node: Option<&Node>, writer: fn(&Printer, &str)) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        let saved_write = self.write.get();
        self.write.set(writer);
        self.emit(Some(node));
        self.write.set(saved_write);
    }

    pub(super) fn emit_type_annotation<TNodeRef: Borrow<Node>>(
        &self,
        node: Option<TNodeRef /*TypeNode*/>,
    ) {
        if let Some(node) = node {
            let node = node.borrow();
            self.write_punctuation(":");
            self.write_space();
            self.emit(Some(node));
        }
    }

    pub(super) fn emit_type_arguments(
        &self,
        parent_node: &Node,
        type_arguments: Option<&NodeArray /*<TypeNode>*/>,
    ) {
        self.emit_list(
            Some(parent_node),
            type_arguments,
            ListFormat::TypeArguments,
            // parenthesizer.parenthesizeMemberOfElementType
        );
    }

    pub(super) fn write_delimiter(&self, format: ListFormat) {
        match format & ListFormat::DelimitersMask {
            ListFormat::None => (),
            ListFormat::CommaDelimited => {
                self.write_punctuation(",");
            }
            ListFormat::BarDelimited => {
                self.write_space();
                self.write_punctuation("|");
            }
            _ => unimplemented!(),
        }
    }

    pub(super) fn emit_list<TNode: Borrow<Node>>(
        &self,
        parent_node: Option<TNode>,
        children: Option<&NodeArray>,
        format: ListFormat,
    ) {
        self.emit_node_list(
            Printer::emit,
            parent_node,
            children,
            format,
            // TODO: this is wrong
            None,
            None,
        );
    }

    pub(super) fn emit_node_list<TNode: Borrow<Node>>(
        &self,
        emit: fn(&Printer, Option<&Node>),
        parent_node: Option<TNode>,
        children: Option<&NodeArray>,
        format: ListFormat,
        start: Option<usize>,
        count: Option<usize>,
    ) {
        let start = start.unwrap_or(0);
        let count = count.unwrap_or_else(|| {
            if let Some(children) = children {
                children.len() - start
            } else {
                0
            }
        });
        let is_undefined = children.is_none();
        if is_undefined && format.intersects(ListFormat::OptionalIfUndefined) {
            return;
        }

        let is_empty = match children {
            None => true,
            Some(children) => start >= children.len(),
        } || count == 0;
        if is_empty && format.intersects(ListFormat::OptionalIfEmpty) {
            // TODO
            return;
        }

        if format.intersects(ListFormat::BracketsMask) {
            self.write_punctuation(get_opening_bracket(format));
            if is_empty {
                if let Some(children) = children {
                    self.emit_trailing_comments_of_position(children.pos(), Some(true), None);
                }
            }
        }

        // TODO

        let children = children.unwrap();
        if is_empty {
            unimplemented!()
        } else {
            if false {
                unimplemented!()
            } else if format.intersects(ListFormat::SpaceBetweenBraces) {
                self.write_space();
            }

            let mut previous_sibling: Option<Rc<Node>> = None;
            let children_iter = children.iter();
            for child in children_iter.skip(start) {
                if false {
                    unimplemented!()
                } else if let Some(previous_sibling) = previous_sibling.as_ref() {
                    self.write_delimiter(format);
                }

                if false {
                    unimplemented!()
                } else if previous_sibling.is_some()
                    && format.intersects(ListFormat::SpaceBetweenSiblings)
                {
                    self.write_space();
                }

                emit(self, Some(&**child));

                previous_sibling = Some(child.clone());
            }

            if false {
                unimplemented!()
            } else if format.intersects(ListFormat::SpaceAfterList | ListFormat::SpaceBetweenBraces)
            {
                self.write_space();
            }
        }
    }

    pub(super) fn write_base(&self, s: &str) {
        self.writer().write(s);
    }

    pub(super) fn write_string_literal(&self, s: &str) {
        self.writer().write_string_literal(s);
    }

    pub(super) fn write_symbol(&self, s: &str, sym: &Symbol) {
        self.writer().write_symbol(s, sym);
    }

    pub(super) fn write_punctuation(&self, s: &str) {
        self.writer().write_punctuation(s);
    }

    pub(super) fn write_trailing_semicolon(&self) {
        self.writer().write_trailing_semicolon(";");
    }

    pub(super) fn write_keyword(&self, s: &str) {
        self.writer().write_keyword(s);
    }

    pub(super) fn write_space(&self) {
        self.writer().write_space(" ");
    }

    pub(super) fn write_property(&self, s: &str) {
        self.writer().write_property(s);
    }

    pub(super) fn increase_indent(&self) {
        self.writer().increase_indent();
    }

    pub(super) fn decrease_indent(&self) {
        self.writer().decrease_indent();
    }

    pub(super) fn write_token_node(&self, node: &Node, writer: fn(&Printer, &str)) {
        writer(self, token_to_string(node.kind()).unwrap());
    }

    pub(super) fn write_token_text<TWriter: FnMut(&str)>(
        &self,
        token: SyntaxKind,
        mut writer: TWriter,
        pos: Option<isize>,
    ) -> Option<isize> {
        let token_string = token_to_string(token).unwrap();
        writer(token_string);
        pos.map(|pos| {
            if pos < 0 {
                pos
            } else {
                pos + TryInto::<isize>::try_into(token_string.len()).unwrap()
            }
        })
    }

    pub(super) fn get_text_of_node(&self, node: &Node, include_trivia: Option<bool>) -> String {
        if false {
            unimplemented!()
        } else if (is_identifier(node) || false) && true {
            return id_text(node);
        }

        unimplemented!()
    }

    pub(super) fn get_literal_text_of_node(&self, node: &Node) -> Cow<'static, str> {
        let flags = GetLiteralTextFlags::None;

        get_literal_text(node, self.maybe_current_source_file(), flags)
    }

    pub(super) fn emit_leading_comments_of_position(&self, pos: isize) {
        // unimplemented!()
    }

    pub(super) fn emit_trailing_comments_of_position(
        &self,
        pos: isize,
        prefix_space: Option<bool>,
        force_no_newline: Option<bool>,
    ) {
        // unimplemented!()
    }
}

pub(super) fn create_brackets_map() -> HashMap<ListFormat, (&'static str, &'static str)> {
    HashMap::from_iter(IntoIterator::into_iter([
        (ListFormat::Braces, ("{", "}")),
        (ListFormat::Parenthesis, ("(", ")")),
        (ListFormat::AngleBrackets, ("<", ">")),
        (ListFormat::SquareBrackets, ("[", "]")),
    ]))
}

pub(super) fn get_opening_bracket(format: ListFormat) -> &'static str {
    brackets
        .get(&(format & ListFormat::BracketsMask))
        .unwrap()
        .0
}

pub(super) fn get_closing_bracket(format: ListFormat) -> &'static str {
    brackets
        .get(&(format & ListFormat::BracketsMask))
        .unwrap()
        .1
}

pub enum TempFlags {
    Auto = 0x00000000,
    CountMask = 0x0FFFFFFF,
    _I = 0x10000000,
}
