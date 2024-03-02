use std::{collections::HashSet, convert::TryInto, io};

use id_arena::Id;
use regex::Regex;

use super::ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule;
use crate::{
    find_index, for_each, for_each_leading_comment_range, for_each_trailing_comment_range,
    get_comment_range, get_emit_flags, get_factory, get_line_and_character_of_position,
    get_text_of_jsdoc_comment, is_jsx_closing_element, is_jsx_opening_element,
    is_prologue_directive, is_unparsed_source, node_is_synthesized,
    range_start_positions_are_on_same_line, released, BundleFileSection, BundleFileSectionKind,
    EmitFlags, FileReference, HasArena, HasInitializerInterface, HasStatementsInterface,
    HasTypeArgumentsInterface, HasTypeParametersInterface, InArena, JSDocTagInterface,
    JSDocTypeLikeTagInterface, ListFormat, LiteralLikeNodeInterface, NamedDeclarationInterface,
    Node, NodeArray, NodeInterface, OptionInArena, Printer, ReadonlyTextRange,
    ReadonlyTextRangeConcrete, SourceFileLike, StrOrNodeArray, SyntaxKind, TextRange,
};

impl Printer {
    pub(super) fn emit_jsx_element(&self, node: Id<Node> /*JsxElement*/) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_jsx_element = node_ref.as_jsx_element();
        self.emit(Some(node_as_jsx_element.opening_element), None)?;
        self.emit_list(
            Some(node),
            Some(node_as_jsx_element.children),
            ListFormat::JsxElementOrFragmentChildren,
            None,
            None,
            None,
        )?;
        self.emit(Some(node_as_jsx_element.closing_element), None)?;

        Ok(())
    }

    pub(super) fn emit_jsx_self_closing_element(
        &self,
        node: Id<Node>, /*JsxSelfClosingElement*/
    ) -> io::Result<()> {
        self.write_punctuation("<");
        let node_ref = node.ref_(self);
        let node_as_jsx_self_closing_element = node_ref.as_jsx_self_closing_element();
        self.emit_jsx_tag_name(node_as_jsx_self_closing_element.tag_name)?;
        self.emit_type_arguments(
            node,
            node_as_jsx_self_closing_element.maybe_type_arguments(),
        )?;
        self.write_space();
        self.emit(Some(node_as_jsx_self_closing_element.attributes), None)?;
        self.write_punctuation("/>");

        Ok(())
    }

    pub(super) fn emit_jsx_fragment(&self, node: Id<Node> /*JsxFragment*/) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_jsx_fragment = node_ref.as_jsx_fragment();
        self.emit(Some(node_as_jsx_fragment.opening_fragment), None)?;
        self.emit_list(
            Some(node),
            Some(node_as_jsx_fragment.children),
            ListFormat::JsxElementOrFragmentChildren,
            None,
            None,
            None,
        )?;
        self.emit(Some(node_as_jsx_fragment.closing_fragment), None)?;

        Ok(())
    }

    pub(super) fn emit_jsx_opening_element_or_fragment(
        &self,
        node: Id<Node>, /*JsxOpeningElement | JsxOpeningFragment*/
    ) -> io::Result<()> {
        self.write_punctuation("<");

        if is_jsx_opening_element(&node.ref_(self)) {
            let node_ref = node.ref_(self);
            let node_as_jsx_opening_element = node_ref.as_jsx_opening_element();
            let indented = self.write_line_separators_and_indent_before(
                node_as_jsx_opening_element.tag_name,
                node,
            );
            self.emit_jsx_tag_name(node_as_jsx_opening_element.tag_name)?;
            self.emit_type_arguments(node, node_as_jsx_opening_element.maybe_type_arguments())?;
            if
            /*node.attributes.properties &&*/
            !node_as_jsx_opening_element
                .attributes
                .ref_(self)
                .as_jsx_attributes()
                .properties
                .ref_(self)
                .is_empty()
            {
                self.write_space();
            }
            self.emit(Some(node_as_jsx_opening_element.attributes), None)?;
            self.write_line_separators_after(node_as_jsx_opening_element.attributes, node);
            self.decrease_indent_if(indented, None);
        }

        self.write_punctuation(">");

        Ok(())
    }

    pub(super) fn emit_jsx_text(&self, node: Id<Node> /*JsxText*/) {
        self.writer()
            .write_literal(&node.ref_(self).as_jsx_text().text());
    }

    pub(super) fn emit_jsx_closing_element_or_fragment(
        &self,
        node: Id<Node>, /*JsxClosingElement | JsxClosingFragment*/
    ) -> io::Result<()> {
        self.write_punctuation("</");
        if is_jsx_closing_element(&node.ref_(self)) {
            self.emit_jsx_tag_name(node.ref_(self).as_jsx_closing_element().tag_name)?;
        }
        self.write_punctuation(">");

        Ok(())
    }

    pub(super) fn emit_jsx_attributes(
        &self,
        node: Id<Node>, /*JsxAttributes*/
    ) -> io::Result<()> {
        self.emit_list(
            Some(node),
            Some(node.ref_(self).as_jsx_attributes().properties),
            ListFormat::JsxElementAttributes,
            None,
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_jsx_attribute(
        &self,
        node: Id<Node>, /*JsxAttribute*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_jsx_attribute = node_ref.as_jsx_attribute();
        self.emit(Some(node_as_jsx_attribute.name), None)?;
        self.try_emit_node_with_prefix(
            "=",
            |text: &str| self.write_punctuation(text),
            node_as_jsx_attribute.initializer,
            |node: Id<Node>| self.emit_jsx_attribute_value(node),
        )?;

        Ok(())
    }

    pub(super) fn emit_jsx_spread_attribute(
        &self,
        node: Id<Node>, /*JsxSpreadAttribute*/
    ) -> io::Result<()> {
        self.write_punctuation("{...");
        self.emit_expression(
            Some(node.ref_(self).as_jsx_spread_attribute().expression),
            None,
        )?;
        self.write_punctuation("}");

        Ok(())
    }

    pub(super) fn has_trailing_comments_at_position(&self, pos: isize) -> bool {
        let mut result = false;

        let current_source_file = self.maybe_current_source_file();
        let default_text: Option<Vec<char>> = if current_source_file.is_some() {
            None
        } else {
            Some(vec![])
        };
        let current_source_file_ref = current_source_file.refed(self);
        #[allow(clippy::unused_unit)]
        for_each_trailing_comment_range(
            current_source_file_ref
                .as_ref()
                .map(|current_source_file_ref| {
                    current_source_file_ref.as_source_file().text_as_chars()
                })
                .as_deref()
                .unwrap_or_else(|| default_text.as_ref().unwrap()),
            (pos + 1).try_into().unwrap(),
            |_, _, _, _, _| -> Option<()> {
                result = true;
                None
            },
            &(),
        );
        result
    }

    pub(super) fn has_leading_comments_at_position(&self, pos: isize) -> bool {
        let mut result = false;

        let current_source_file = self.maybe_current_source_file();
        let default_text: Option<Vec<char>> = if current_source_file.is_some() {
            None
        } else {
            Some(vec![])
        };
        let current_source_file_ref = current_source_file.refed(self);
        #[allow(clippy::unused_unit)]
        for_each_leading_comment_range(
            current_source_file_ref
                .as_ref()
                .map(|current_source_file_ref| {
                    current_source_file_ref.as_source_file().text_as_chars()
                })
                .as_deref()
                .unwrap_or_else(|| default_text.as_ref().unwrap()),
            (pos + 1).try_into().unwrap(),
            |_, _, _, _, _| -> Option<()> {
                result = true;
                None
            },
            &(),
        );
        result
    }

    pub(super) fn has_comments_at_position(&self, pos: isize) -> bool {
        self.has_trailing_comments_at_position(pos) || self.has_leading_comments_at_position(pos)
    }

    pub(super) fn emit_jsx_expression(
        &self,
        node: Id<Node>, /*JsxExpression*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_jsx_expression = node_ref.as_jsx_expression();
        Ok(
            if node_as_jsx_expression.expression.is_some()
                || !self.comments_disabled()
                    && !node_is_synthesized(&*node.ref_(self))
                    && self.has_comments_at_position(node.ref_(self).pos())
            {
                let is_multiline = matches!(
                    self.maybe_current_source_file(),
                    Some(current_source_file) if !node_is_synthesized(&*node.ref_(self)) &&
                        get_line_and_character_of_position(
                            current_source_file.ref_(self).as_source_file(),
                            node.ref_(self).pos().try_into().unwrap(),
                        ).line !=
                        get_line_and_character_of_position(
                            current_source_file.ref_(self).as_source_file(),
                            node.ref_(self).end().try_into().unwrap(),
                        ).line
                );
                if is_multiline {
                    self.writer().increase_indent();
                }
                let end = self.emit_token_with_comment(
                    SyntaxKind::OpenBraceToken,
                    node.ref_(self).pos(),
                    |text: &str| self.write_punctuation(text),
                    node,
                    None,
                );
                self.emit(node_as_jsx_expression.dot_dot_dot_token, None)?;
                self.emit_expression(node_as_jsx_expression.expression, None)?;
                self.emit_token_with_comment(
                    SyntaxKind::CloseBraceToken,
                    node_as_jsx_expression
                        .expression
                        .map_or(end, |node_expression| node_expression.ref_(self).end()),
                    |text: &str| self.write_punctuation(text),
                    node,
                    None,
                );
                if is_multiline {
                    self.writer().decrease_indent();
                }
            },
        )
    }

    pub(super) fn emit_jsx_tag_name(
        &self,
        node: Id<Node>, /*JsxTagNameExpression*/
    ) -> io::Result<()> {
        Ok(if node.ref_(self).kind() == SyntaxKind::Identifier {
            self.emit_expression(Some(node), None)?;
        } else {
            self.emit(Some(node), None)?;
        })
    }

    pub(super) fn emit_case_clause(&self, node: Id<Node> /*CaseClause*/) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::CaseKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        self.emit_expression(
            released!(Some(node.ref_(self).as_case_clause().expression)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;

        self.emit_case_or_default_clause_rest(
            node,
            released!(node.ref_(self).as_case_clause().statements),
            released!(node.ref_(self).as_case_clause().expression.ref_(self).end()),
        )?;

        Ok(())
    }

    pub(super) fn emit_default_clause(
        &self,
        node: Id<Node>, /*DefaultClause*/
    ) -> io::Result<()> {
        let pos = self.emit_token_with_comment(
            SyntaxKind::DefaultKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.emit_case_or_default_clause_rest(
            node,
            released!(node.ref_(self).as_default_clause().statements),
            pos,
        )?;

        Ok(())
    }

    pub(super) fn emit_case_or_default_clause_rest(
        &self,
        parent_node: Id<Node>,
        statements: Id<NodeArray>, /*<Statement>*/
        colon_pos: isize,
    ) -> io::Result<()> {
        let emit_as_single_statement = statements.ref_(self).len() == 1
            && (node_is_synthesized(&*parent_node.ref_(self))
                || node_is_synthesized(&*statements.ref_(self)[0].ref_(self))
                || range_start_positions_are_on_same_line(
                    &*parent_node.ref_(self),
                    &*statements.ref_(self)[0].ref_(self),
                    &self.current_source_file().ref_(self),
                ));

        let mut format = ListFormat::CaseOrDefaultClauseStatements;
        if emit_as_single_statement {
            self.write_token(
                SyntaxKind::ColonToken,
                colon_pos,
                |text: &str| self.write_punctuation(text),
                Some(parent_node),
            );
            self.write_space();
            format &= !(ListFormat::MultiLine | ListFormat::Indented);
        } else {
            self.emit_token_with_comment(
                SyntaxKind::ColonToken,
                colon_pos,
                |text: &str| self.write_punctuation(text),
                parent_node,
                None,
            );
        }
        self.emit_list(
            Some(parent_node),
            Some(statements),
            format,
            None,
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_heritage_clause(
        &self,
        node: Id<Node>, /*HeritageClause*/
    ) -> io::Result<()> {
        self.write_space();
        self.write_token_text(
            node.ref_(self).as_heritage_clause().token,
            |text: &str| self.write_keyword(text),
            None,
        );
        self.write_space();
        self.emit_list(
            Some(node),
            released!(Some(node.ref_(self).as_heritage_clause().types)),
            ListFormat::HeritageClauseTypes,
            None,
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_catch_clause(&self, node: Id<Node> /*CatchClause*/) -> io::Result<()> {
        let open_paren_pos = self.emit_token_with_comment(
            SyntaxKind::CatchKeyword,
            node.ref_(self).pos(),
            |text: &str| self.write_keyword(text),
            node,
            None,
        );
        self.write_space();
        let node_ref = node.ref_(self);
        let node_as_catch_clause = node_ref.as_catch_clause();
        if let Some(node_variable_declaration) = node_as_catch_clause.variable_declaration {
            self.emit_token_with_comment(
                SyntaxKind::OpenParenToken,
                open_paren_pos,
                |text: &str| self.write_punctuation(text),
                node,
                None,
            );
            self.emit(Some(node_variable_declaration), None)?;
            self.emit_token_with_comment(
                SyntaxKind::CloseParenToken,
                node_variable_declaration.ref_(self).end(),
                |text: &str| self.write_punctuation(text),
                node,
                None,
            );
            self.write_space();
        }
        self.emit(Some(node_as_catch_clause.block), None)?;

        Ok(())
    }

    pub(super) fn emit_property_assignment(
        &self,
        node: Id<Node>, /*PropertyAssignment*/
    ) -> io::Result<()> {
        self.emit(
            released!(node.ref_(self).as_property_assignment().maybe_name()),
            None,
        )?;
        self.write_punctuation(":");
        self.write_space();
        let initializer = node
            .ref_(self)
            .as_property_assignment()
            .maybe_initializer()
            .unwrap();
        if !get_emit_flags(initializer, self).intersects(EmitFlags::NoLeadingComments) {
            let comment_range = get_comment_range(initializer, self);
            self.emit_trailing_comments_of_position(comment_range.pos(), None, None);
        }
        self.emit_expression(
            Some(initializer),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;

        Ok(())
    }

    pub(super) fn emit_shorthand_property_assignment(
        &self,
        node: Id<Node>, /*ShorthandPropertyAssignment*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_shorthand_property_assignment = node_ref.as_shorthand_property_assignment();
        self.emit(node_as_shorthand_property_assignment.maybe_name(), None)?;
        Ok(
            if let Some(node_object_assignment_initializer) =
                node_as_shorthand_property_assignment.object_assignment_initializer
            {
                self.write_space();
                self.write_punctuation("=");
                self.write_space();
                self.emit_expression(
                    Some(node_object_assignment_initializer),
                    Some(self.alloc_current_parenthesizer_rule(Box::new(
                        ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                            self.parenthesizer(),
                            self,
                        ),
                    ))),
                )?;
            },
        )
    }

    pub(super) fn emit_spread_assignment(
        &self,
        node: Id<Node>, /*SpreadAssignment*/
    ) -> io::Result<()> {
        // if (node.expression) {
        self.emit_token_with_comment(
            SyntaxKind::DotDotDotToken,
            node.ref_(self).pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_expression(
            released!(Some(node.ref_(self).as_spread_assignment().expression)),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        // }

        Ok(())
    }

    pub(super) fn emit_enum_member(&self, node: Id<Node> /*EnumMember*/) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_enum_member = node_ref.as_enum_member();
        self.emit(node_as_enum_member.maybe_name(), None)?;
        self.emit_initializer(
            node_as_enum_member.initializer,
            node_as_enum_member.name.ref_(self).end(),
            node,
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;

        Ok(())
    }

    pub(super) fn emit_jsdoc(&self, node: Id<Node> /*JSDoc*/) -> io::Result<()> {
        self.write("/**");
        let node_ref = node.ref_(self);
        let node_as_jsdoc = node_ref.as_jsdoc();
        if let Some(node_comment) = node_as_jsdoc.comment.as_ref() {
            let text = get_text_of_jsdoc_comment(Some(node_comment), self);
            if let Some(text) = text.filter(|text| !text.is_empty()) {
                lazy_static! {
                    static ref lines_regex: Regex = Regex::new(r"\r\n?|\n").unwrap();
                }
                let lines = lines_regex.split(&text);
                for line in lines {
                    self.write_line(None);
                    self.write_space();
                    self.write_punctuation("*");
                    self.write_space();
                    self.write(line);
                }
            }
        }
        if let Some(node_tags) = node_as_jsdoc.tags {
            if node_tags.ref_(self).len() == 1
                && node_tags.ref_(self)[0].ref_(self).kind() == SyntaxKind::JSDocTypeTag
                && node_as_jsdoc.comment.is_none()
            {
                self.write_space();
                self.emit(Some(node_tags.ref_(self)[0]), None)?;
            } else {
                self.emit_list(
                    Some(node),
                    Some(node_tags),
                    ListFormat::JSDocComment,
                    None,
                    None,
                    None,
                )?;
            }
        }
        self.write_space();
        self.write("*/");

        Ok(())
    }

    pub(super) fn emit_jsdoc_simple_typed_tag(
        &self,
        tag: Id<Node>, /*JSDocTypeTag | JSDocThisTag | JSDocEnumTag | JSDocReturnTag*/
    ) -> io::Result<()> {
        let tag_ref = tag.ref_(self);
        let tag_as_base_jsdoc_type_like_tag = tag_ref.as_base_jsdoc_type_like_tag();
        self.emit_jsdoc_tag_name(tag_as_base_jsdoc_type_like_tag.tag_name())?;
        self.emit_jsdoc_type_expression(tag_as_base_jsdoc_type_like_tag.maybe_type_expression())?;
        self.emit_jsdoc_comment(
            tag_as_base_jsdoc_type_like_tag
                .maybe_comment()
                .map(Into::into),
        );

        Ok(())
    }

    pub(super) fn emit_jsdoc_see_tag(&self, tag: Id<Node> /*JSDocSeeTag*/) -> io::Result<()> {
        let tag_ref = tag.ref_(self);
        let tag_as_jsdoc_see_tag = tag_ref.as_jsdoc_see_tag();
        self.emit_jsdoc_tag_name(tag_as_jsdoc_see_tag.tag_name())?;
        self.emit(tag_as_jsdoc_see_tag.name, None)?;
        self.emit_jsdoc_comment(tag_as_jsdoc_see_tag.maybe_comment().map(Into::into));

        Ok(())
    }

    pub(super) fn emit_jsdoc_name_reference(
        &self,
        node: Id<Node>, /*JSDocNameReference*/
    ) -> io::Result<()> {
        self.write_space();
        self.write_punctuation("{");
        self.emit(Some(node.ref_(self).as_jsdoc_name_reference().name), None)?;
        self.write_punctuation("}");

        Ok(())
    }

    pub(super) fn emit_jsdoc_heritage_tag(
        &self,
        tag: Id<Node>, /*JSDocImplementsTag | JSDocAugmentsTag*/
    ) -> io::Result<()> {
        let tag_ref = tag.ref_(self);
        let tag_as_jsdoc_heritage_tag = tag_ref.as_jsdoc_heritage_tag();
        self.emit_jsdoc_tag_name(tag_as_jsdoc_heritage_tag.tag_name())?;
        self.write_space();
        self.write_punctuation("{");
        self.emit(Some(tag_as_jsdoc_heritage_tag.class()), None)?;
        self.write_punctuation("}");
        self.emit_jsdoc_comment(tag_as_jsdoc_heritage_tag.maybe_comment().map(Into::into));

        Ok(())
    }

    pub(super) fn emit_jsdoc_template_tag(
        &self,
        tag: Id<Node>, /*JSDocTemplateTag*/
    ) -> io::Result<()> {
        let tag_ref = tag.ref_(self);
        let tag_as_jsdoc_template_tag = tag_ref.as_jsdoc_template_tag();
        self.emit_jsdoc_tag_name(tag_as_jsdoc_template_tag.tag_name())?;
        self.emit_jsdoc_type_expression(tag_as_jsdoc_template_tag.constraint)?;
        self.write_space();
        self.emit_list(
            Some(tag),
            Some(tag_as_jsdoc_template_tag.type_parameters),
            ListFormat::CommaListElements,
            None,
            None,
            None,
        )?;
        self.emit_jsdoc_comment(tag_as_jsdoc_template_tag.maybe_comment().map(Into::into));

        Ok(())
    }

    pub(super) fn emit_jsdoc_typedef_tag(
        &self,
        tag: Id<Node>, /*JSDocTypedefTag*/
    ) -> io::Result<()> {
        let tag_ref = tag.ref_(self);
        let tag_as_jsdoc_typedef_tag = tag_ref.as_jsdoc_typedef_tag();
        self.emit_jsdoc_tag_name(tag_as_jsdoc_typedef_tag.tag_name())?;
        if let Some(tag_type_expression) = tag_as_jsdoc_typedef_tag.type_expression {
            if tag_type_expression.ref_(self).kind() == SyntaxKind::JSDocTypeExpression {
                self.emit_jsdoc_type_expression(Some(tag_type_expression))?;
            } else {
                self.write_space();
                self.write_punctuation("{");
                self.write("Object");
                if tag_type_expression
                    .ref_(self)
                    .as_jsdoc_type_literal()
                    .is_array_type
                {
                    self.write_punctuation("[");
                    self.write_punctuation("]");
                }
                self.write_punctuation("}");
            }
        }
        if let Some(tag_full_name) = tag_as_jsdoc_typedef_tag.full_name {
            self.write_space();
            self.emit(Some(tag_full_name), None)?;
        }
        self.emit_jsdoc_comment(tag_as_jsdoc_typedef_tag.maybe_comment().map(Into::into));
        Ok(
            if let Some(tag_type_expression) =
                tag_as_jsdoc_typedef_tag
                    .type_expression
                    .filter(|tag_type_expression| {
                        tag_type_expression.ref_(self).kind() == SyntaxKind::JSDocTypeLiteral
                    })
            {
                self.emit_jsdoc_type_literal(tag_type_expression)?;
            },
        )
    }

    pub(super) fn emit_jsdoc_callback_tag(
        &self,
        tag: Id<Node>, /*JSDocCallbackTag*/
    ) -> io::Result<()> {
        let tag_ref = tag.ref_(self);
        let tag_as_jsdoc_callback_tag = tag_ref.as_jsdoc_callback_tag();
        self.emit_jsdoc_tag_name(tag_as_jsdoc_callback_tag.tag_name())?;
        if let Some(tag_name) = tag_as_jsdoc_callback_tag.name {
            self.write_space();
            self.emit(Some(tag_name), None)?;
        }
        self.emit_jsdoc_comment(tag_as_jsdoc_callback_tag.maybe_comment().map(Into::into));
        self.emit_jsdoc_signature(tag_as_jsdoc_callback_tag.type_expression)?;

        Ok(())
    }

    pub(super) fn emit_jsdoc_simple_tag(&self, tag: Id<Node> /*JSDocTag*/) -> io::Result<()> {
        let tag_ref = tag.ref_(self);
        let tag_as_jsdoc_tag = tag_ref.as_jsdoc_tag();
        self.emit_jsdoc_tag_name(tag_as_jsdoc_tag.tag_name())?;
        self.emit_jsdoc_comment(tag_as_jsdoc_tag.maybe_comment().map(Into::into));

        Ok(())
    }

    pub(super) fn emit_jsdoc_type_literal(
        &self,
        lit: Id<Node>, /*JSDocTypeLiteral*/
    ) -> io::Result<()> {
        self.emit_list(
            Some(lit),
            Some(
                get_factory(self).create_node_array(
                    lit.ref_(self)
                        .as_jsdoc_type_literal()
                        .js_doc_property_tags
                        .clone(),
                    None,
                ),
            ),
            ListFormat::JSDocComment,
            None,
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_jsdoc_signature(
        &self,
        sig: Id<Node>, /*JSDocSignature*/
    ) -> io::Result<()> {
        let sig_ref = sig.ref_(self);
        let sig_as_jsdoc_signature = sig_ref.as_jsdoc_signature();
        if let Some(sig_type_parameters) = sig_as_jsdoc_signature.maybe_type_parameters().as_ref() {
            self.emit_list(
                Some(sig),
                Some(get_factory(self).create_node_array(Some(sig_type_parameters.clone()), None)),
                ListFormat::JSDocComment,
                None,
                None,
                None,
            )?;
        }
        // if (sig.parameters) {
        self.emit_list(
            Some(sig),
            Some(
                get_factory(self)
                    .create_node_array(Some(sig_as_jsdoc_signature.parameters.clone()), None),
            ),
            ListFormat::JSDocComment,
            None,
            None,
            None,
        )?;
        // }
        Ok(if let Some(sig_type) = sig_as_jsdoc_signature.type_ {
            self.write_line(None);
            self.write_space();
            self.write_punctuation("*");
            self.write_space();
            self.emit(Some(sig_type), None)?;
        })
    }

    pub(super) fn emit_jsdoc_property_like_tag(
        &self,
        param: Id<Node>, /*JSDocPropertyLikeTag*/
    ) -> io::Result<()> {
        let param_ref = param.ref_(self);
        let param_as_jsdoc_property_like_tag = param_ref.as_jsdoc_property_like_tag();
        self.emit_jsdoc_tag_name(param_as_jsdoc_property_like_tag.tag_name())?;
        self.emit_jsdoc_type_expression(param_as_jsdoc_property_like_tag.type_expression)?;
        self.write_space();
        if param_as_jsdoc_property_like_tag.is_bracketed {
            self.write_punctuation("[");
        }
        self.emit(Some(param_as_jsdoc_property_like_tag.name), None)?;
        if param_as_jsdoc_property_like_tag.is_bracketed {
            self.write_punctuation("]");
        }
        self.emit_jsdoc_comment(
            param_as_jsdoc_property_like_tag
                .maybe_comment()
                .map(Into::into),
        );

        Ok(())
    }

    pub(super) fn emit_jsdoc_tag_name(
        &self,
        tag_name: Id<Node>, /*Identifier*/
    ) -> io::Result<()> {
        self.write_punctuation("@");
        self.emit(Some(tag_name), None)?;

        Ok(())
    }

    pub(super) fn emit_jsdoc_comment(&self, comment: Option<StrOrNodeArray>) {
        let text = get_text_of_jsdoc_comment(comment, self);
        if let Some(text) = text.filter(|text| !text.is_empty()) {
            self.write_space();
            self.write(&text);
        }
    }

    pub(super) fn emit_jsdoc_type_expression(
        &self,
        type_expression: Option<Id<Node> /*JSDocTypeExpression*/>,
    ) -> io::Result<()> {
        Ok(if let Some(type_expression) = type_expression {
            self.write_space();
            self.write_punctuation("{");
            self.emit(
                Some(type_expression.ref_(self).as_jsdoc_type_expression().type_),
                None,
            )?;
            self.write_punctuation("}");
        })
    }

    pub(super) fn emit_source_file(&self, node: Id<Node> /*SourceFile*/) -> io::Result<()> {
        self.write_line(None);
        let statements = node.ref_(self).as_source_file().statements();
        // if (emitBodyWithDetachedComments) {
        let should_emit_detached_comment = statements.ref_(self).is_empty()
            || !is_prologue_directive(statements.ref_(self)[0], self)
            || node_is_synthesized(&*statements.ref_(self)[0].ref_(self));
        if should_emit_detached_comment {
            self.try_emit_body_with_detached_comments(
                node,
                &released!(ReadonlyTextRangeConcrete::from(&*statements.ref_(self))),
                |node: Id<Node>| self.emit_source_file_worker(node),
            )?;
            return Ok(());
        }
        // }
        self.emit_source_file_worker(node)?;

        Ok(())
    }

    pub(super) fn emit_synthetic_triple_slash_references_if_needed(
        &self,
        node: Id<Node>, /*Bundle*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_bundle = node_ref.as_bundle();
        let default_references: Vec<FileReference> = vec![];
        self.emit_triple_slash_directives(
            node_as_bundle.has_no_default_lib == Some(true),
            node_as_bundle
                .synthetic_file_references
                .as_ref()
                .unwrap_or(&default_references),
            node_as_bundle
                .synthetic_type_references
                .as_ref()
                .unwrap_or(&default_references),
            node_as_bundle
                .synthetic_lib_references
                .as_ref()
                .unwrap_or(&default_references),
        );
        for prepend in &node_as_bundle.prepends {
            if is_unparsed_source(&prepend.ref_(self)) {
                if let Some(prepend_synthetic_references) = prepend
                    .ref_(self)
                    .as_unparsed_source()
                    .synthetic_references
                    .as_ref()
                {
                    for &ref_ in prepend_synthetic_references {
                        self.emit(Some(ref_), None)?;
                        self.write_line(None);
                    }
                }
            }
        }

        Ok(())
    }

    pub(super) fn emit_triple_slash_directives_if_needed(
        &self,
        node: Id<Node>, /*SourceFile*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_source_file = node_ref.as_source_file();
        if node_as_source_file.is_declaration_file() {
            self.emit_triple_slash_directives(
                node_as_source_file.has_no_default_lib(),
                &(*node_as_source_file.referenced_files()).borrow(),
                &(*node_as_source_file.type_reference_directives()).borrow(),
                &(*node_as_source_file.lib_reference_directives()).borrow(),
            );
        }
    }

    pub(super) fn emit_triple_slash_directives(
        &self,
        has_no_default_lib: bool,
        files: &[FileReference],
        types: &[FileReference],
        libs: &[FileReference],
    ) {
        if has_no_default_lib {
            let pos = self.writer().get_text_pos();
            self.write_comment("/// <reference no-default-lib=\"true\"/>");
            if let Some(bundle_file_info) = self.maybe_bundle_file_info() {
                bundle_file_info
                    .ref_mut(self)
                    .sections
                    .push(self.alloc_bundle_file_section(
                        BundleFileSection::new_has_no_default_lib(
                            None,
                            pos.try_into().unwrap(),
                            self.writer().get_text_pos().try_into().unwrap(),
                        ),
                    ));
            }
            self.write_line(None);
        }
        if let Some(current_source_file) = self.maybe_current_source_file().as_ref() {
            if let Some(current_source_file_module_name) = current_source_file
                .ref_(self)
                .as_source_file()
                .maybe_module_name()
                .as_ref()
                .filter(|current_source_file_module_name| {
                    !current_source_file_module_name.is_empty()
                })
            {
                self.write_comment(&format!(
                    "/// <amd-module name=\"{}\" />",
                    current_source_file_module_name,
                ));
                self.write_line(None);
            }
        }
        if let Some(current_source_file) = self.maybe_current_source_file() {
            if let Some(current_source_file_amd_dependencies) = current_source_file
                .ref_(self)
                .as_source_file()
                .maybe_amd_dependencies()
                .as_ref()
            {
                for dep in current_source_file_amd_dependencies {
                    if let Some(dep_name) =
                        dep.name.as_ref().filter(|dep_name| !dep_name.is_empty())
                    {
                        self.write_comment(&format!(
                            "/// <amd-dependency name=\"{}\" path=\"{}\" />",
                            dep_name, dep.path,
                        ));
                    } else {
                        self.write_comment(&format!(
                            "/// <amd-dependency path=\"{}\" />",
                            dep.path,
                        ));
                    }
                    self.write_line(None);
                }
            }
        }
        for directive in files {
            let pos = self.writer().get_text_pos();
            self.write_comment(&format!(
                "/// <reference path=\"{}\" />",
                directive.file_name,
            ));
            if let Some(bundle_file_info) = self.maybe_bundle_file_info() {
                bundle_file_info
                    .ref_mut(self)
                    .sections
                    .push(
                        self.alloc_bundle_file_section(BundleFileSection::new_reference(
                            BundleFileSectionKind::Reference,
                            directive.file_name.clone(),
                            pos.try_into().unwrap(),
                            self.writer().get_text_pos().try_into().unwrap(),
                        )),
                    );
            }
            self.write_line(None);
        }
        for directive in types {
            let pos = self.writer().get_text_pos();
            self.write_comment(&format!(
                "/// <reference types=\"{}\" />",
                directive.file_name,
            ));
            if let Some(bundle_file_info) = self.maybe_bundle_file_info() {
                bundle_file_info
                    .ref_mut(self)
                    .sections
                    .push(
                        self.alloc_bundle_file_section(BundleFileSection::new_reference(
                            BundleFileSectionKind::Type,
                            directive.file_name.clone(),
                            pos.try_into().unwrap(),
                            self.writer().get_text_pos().try_into().unwrap(),
                        )),
                    );
            }
            self.write_line(None);
        }
        for directive in libs {
            let pos = self.writer().get_text_pos();
            self.write_comment(&format!(
                "/// <reference lib=\"{}\" />",
                directive.file_name,
            ));
            if let Some(bundle_file_info) = self.maybe_bundle_file_info() {
                bundle_file_info
                    .ref_mut(self)
                    .sections
                    .push(
                        self.alloc_bundle_file_section(BundleFileSection::new_reference(
                            BundleFileSectionKind::Lib,
                            directive.file_name.clone(),
                            pos.try_into().unwrap(),
                            self.writer().get_text_pos().try_into().unwrap(),
                        )),
                    );
            }
            self.write_line(None);
        }
    }

    pub(super) fn emit_source_file_worker(
        &self,
        node: Id<Node>, /*SourceFile*/
    ) -> io::Result<()> {
        let statements = node.ref_(self).as_source_file().statements();
        self.push_name_generation_scope(Some(node));
        for_each(
            &*statements.ref_(self),
            |&statement: &Id<Node>, _| -> Option<()> {
                self.generate_names(Some(statement));
                None
            },
        );
        self.emit_helpers(node);
        let index = find_index(
            &statements.ref_(self),
            |&statement: &Id<Node>, _| !is_prologue_directive(statement, self),
            None,
        );
        self.emit_triple_slash_directives_if_needed(node);
        self.emit_list(
            Some(node),
            Some(statements),
            ListFormat::MultiLine,
            None,
            Some(index.unwrap_or_else(|| statements.ref_(self).len())),
            None,
        )?;
        self.pop_name_generation_scope(Some(node));

        Ok(())
    }

    pub(super) fn emit_partially_emitted_expression(
        &self,
        node: Id<Node>, /*PartiallyEmittedExpression*/
    ) -> io::Result<()> {
        self.emit_expression(
            released!(Some(
                node.ref_(self).as_partially_emitted_expression().expression
            )),
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_comma_list(
        &self,
        node: Id<Node>, /*CommaListExpression*/
    ) -> io::Result<()> {
        self.emit_expression_list(
            Some(node),
            Some(node.ref_(self).as_comma_list_expression().elements),
            ListFormat::CommaListElements,
            None,
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_prologue_directives(
        &self,
        statements: &[Id<Node>],
        source_file: Option<Id<Node> /*SourceFile*/>,
        seen_prologue_directives: &mut Option<HashSet<String>>,
        record_bundle_file_section: Option<bool /*true*/>,
    ) -> io::Result<usize> {
        let mut needs_to_set_source_file = source_file.is_some();
        for i in 0..statements.len() {
            let statement = statements[i];
            if is_prologue_directive(statement, self) {
                let should_emit_prologue_directive =
                    seen_prologue_directives
                        .as_ref()
                        .map_or(true, |seen_prologue_directives| {
                            !seen_prologue_directives.contains(
                                &*statement
                                    .ref_(self)
                                    .as_expression_statement()
                                    .expression
                                    .ref_(self)
                                    .as_string_literal()
                                    .text(),
                            )
                        });
                if should_emit_prologue_directive {
                    if needs_to_set_source_file {
                        needs_to_set_source_file = false;
                        self.set_source_file(source_file);
                    }
                    self.write_line(None);
                    let pos = self.writer().get_text_pos();
                    self.emit(Some(statement), None)?;
                    if record_bundle_file_section == Some(true) {
                        if let Some(bundle_file_info) = self.maybe_bundle_file_info() {
                            bundle_file_info.ref_mut(self).sections.push(
                                self.alloc_bundle_file_section(BundleFileSection::new_prologue(
                                    statement
                                        .ref_(self)
                                        .as_expression_statement()
                                        .expression
                                        .ref_(self)
                                        .as_string_literal()
                                        .text()
                                        .clone(),
                                    pos.try_into().unwrap(),
                                    self.writer().get_text_pos().try_into().unwrap(),
                                )),
                            );
                        }
                    }
                    if let Some(seen_prologue_directives) = seen_prologue_directives.as_mut() {
                        seen_prologue_directives.insert(
                            statement
                                .ref_(self)
                                .as_expression_statement()
                                .expression
                                .ref_(self)
                                .as_string_literal()
                                .text()
                                .clone(),
                        );
                    }
                }
            } else {
                return Ok(i);
            }
        }

        Ok(statements.len())
    }
}
