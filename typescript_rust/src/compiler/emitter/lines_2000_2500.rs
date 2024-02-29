use std::io;

use id_arena::Id;

use super::{
    ParenthesizeElementTypeOfArrayTypeCurrentParenthesizerRule,
    ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule,
    ParenthesizeLeftSideOfAccessCurrentParenthesizerRule,
    ParenthesizeMemberOfConditionalTypeCurrentParenthesizerRule,
    ParenthesizeMemberOfElementTypeCurrentParenthesizerRule,
};
use crate::{
    for_each, get_constant_value, get_emit_flags, get_factory, is_access_expression, is_finite,
    is_json_source_file, is_numeric_literal, set_text_range_pos_end,
    skip_partially_emitted_expressions, string_contains, token_to_string, EmitFlags, EmitHint,
    FunctionLikeDeclarationInterface, HasArena, HasInitializerInterface, HasQuestionTokenInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface, InArena, ListFormat,
    NamedDeclarationInterface, Node, NodeInterface, Printer, ReadonlyTextRange, ScriptTarget,
    SignatureDeclarationInterface, StringOrNumber, SyntaxKind, TokenFlags,
};

impl Printer {
    pub(super) fn emit_type_parameter(
        &self,
        node: Id<Node>, /*TypeParameterDeclaration*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_type_parameter_declaration = node_ref.as_type_parameter_declaration();
        self.emit(node_as_type_parameter_declaration.maybe_name(), None)?;
        if let Some(node_constraint) = node_as_type_parameter_declaration.constraint {
            self.write_space();
            self.write_keyword("extends");
            self.write_space();
            self.emit(Some(node_constraint), None)?;
        }
        Ok(
            if let Some(node_default) = node_as_type_parameter_declaration.default {
                self.write_space();
                self.write_operator("=");
                self.write_space();
                self.emit(Some(node_default), None)?;
            },
        )
    }

    pub(super) fn emit_parameter(
        &self,
        node: Id<Node>, /*ParameterDeclaration*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_parameter_declaration = node_ref.as_parameter_declaration();
        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.emit(node_as_parameter_declaration.dot_dot_dot_token, None)?;
        self.emit_node_with_writer(
            node_as_parameter_declaration.maybe_name(),
            Printer::write_parameter,
        )?;
        self.emit(node_as_parameter_declaration.question_token, None)?;
        if matches!(
            node.ref_(self).maybe_parent(),
            Some(node_parent) if node_parent.ref_(self).kind() == SyntaxKind::JSDocFunctionType &&
                node_as_parameter_declaration.maybe_name().is_none()
        ) {
            self.emit(node_as_parameter_declaration.maybe_type(), None)?;
        } else {
            self.emit_type_annotation(node_as_parameter_declaration.maybe_type())?;
        }
        self.emit_initializer(
            node_as_parameter_declaration.maybe_initializer(),
            if let Some(node_type) = node_as_parameter_declaration.maybe_type() {
                node_type.ref_(self).end()
            } else if let Some(node_question_token) = node_as_parameter_declaration.question_token {
                node_question_token.ref_(self).end()
            } else if let Some(node_name) = node_as_parameter_declaration.maybe_name() {
                node_name.ref_(self).end()
            } else if let Some(node_modifiers) = node.ref_(self).maybe_modifiers().as_ref() {
                node_modifiers.ref_(self).end()
            } else if let Some(node_decorators) = node.ref_(self).maybe_decorators().as_ref() {
                node_decorators.ref_(self).end()
            } else {
                node.ref_(self).pos()
            },
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

    pub(super) fn emit_decorator(&self, node: Id<Node> /*Decorator*/) -> io::Result<()> {
        self.write_punctuation("@");
        self.emit_expression(
            Some(node.ref_(self).as_decorator().expression),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeLeftSideOfAccessCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;

        Ok(())
    }

    pub(super) fn emit_property_signature(
        &self,
        node: Id<Node>, /*PropertySignature*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_property_signature = node_ref.as_property_signature();
        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.emit_node_with_writer(
            Some(node_as_property_signature.name()),
            Printer::write_property,
        )?;
        self.emit(node_as_property_signature.question_token, None)?;
        self.emit_type_annotation(node_as_property_signature.maybe_type())?;
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_property_declaration(
        &self,
        node: Id<Node>, /*PropertyDeclaration*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_property_declaration = node_ref.as_property_declaration();
        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.emit(node_as_property_declaration.maybe_name(), None)?;
        self.emit(node_as_property_declaration.question_token, None)?;
        self.emit(node_as_property_declaration.exclamation_token, None)?;
        self.emit_type_annotation(node_as_property_declaration.maybe_type())?;
        self.emit_initializer(
            node_as_property_declaration.maybe_initializer(),
            if let Some(node_type) = node_as_property_declaration.maybe_type() {
                node_type.ref_(self).end()
            } else if let Some(node_question_token) = node_as_property_declaration.question_token {
                node_question_token.ref_(self).end()
            } else {
                node_as_property_declaration.name().ref_(self).pos()
            },
            node,
            None,
        )?;
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_method_signature(
        &self,
        node: Id<Node>, /*MethodSignature*/
    ) -> io::Result<()> {
        self.push_name_generation_scope(Some(node));
        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        let node_ref = node.ref_(self);
        let node_as_method_signature = node_ref.as_method_signature();
        self.emit(node_as_method_signature.maybe_name(), None)?;
        self.emit(node_as_method_signature.question_token, None)?;
        self.emit_type_parameters(node, node_as_method_signature.maybe_type_parameters())?;
        self.emit_parameters(node, node_as_method_signature.parameters())?;
        self.emit_type_annotation(node_as_method_signature.maybe_type())?;
        self.write_trailing_semicolon();
        self.pop_name_generation_scope(Some(node));

        Ok(())
    }

    pub(super) fn emit_method_declaration(
        &self,
        node: Id<Node>, /*MethodDeclaration*/
    ) -> io::Result<()> {
        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        let node_ref = node.ref_(self);
        let node_as_method_declaration = node_ref.as_method_declaration();
        self.emit(node_as_method_declaration.maybe_asterisk_token(), None)?;
        self.emit(node_as_method_declaration.maybe_name(), None)?;
        self.emit(node_as_method_declaration.maybe_question_token(), None)?;
        self.emit_signature_and_body(node, |node: Id<Node>| self.emit_signature_head(node))?;

        Ok(())
    }

    pub(super) fn emit_class_static_block_declaration(
        &self,
        node: Id<Node>, /*ClassStaticBlockDeclaration*/
    ) -> io::Result<()> {
        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.write_keyword("static");
        self.emit_block_function_body(node.ref_(self).as_class_static_block_declaration().body)?;

        Ok(())
    }

    pub(super) fn emit_constructor(
        &self,
        node: Id<Node>, /*ConstructorDeclaration*/
    ) -> io::Result<()> {
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.write_keyword("constructor");
        self.emit_signature_and_body(node, |node: Id<Node>| self.emit_signature_head(node))?;

        Ok(())
    }

    pub(super) fn emit_accessor_declaration(
        &self,
        node: Id<Node>, /*AccessorDeclaration*/
    ) -> io::Result<()> {
        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.write_keyword(if node.ref_(self).kind() == SyntaxKind::GetAccessor {
            "get"
        } else {
            "set"
        });
        self.write_space();
        self.emit(node.ref_(self).as_named_declaration().maybe_name(), None)?;
        self.emit_signature_and_body(node, |node: Id<Node>| self.emit_signature_head(node))?;

        Ok(())
    }

    pub(super) fn emit_call_signature(
        &self,
        node: Id<Node>, /*CallSignatureDeclaration*/
    ) -> io::Result<()> {
        self.push_name_generation_scope(Some(node));
        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        let node_ref = node.ref_(self);
        let node_as_call_signature_declaration = node_ref.as_call_signature_declaration();
        self.emit_type_parameters(
            node,
            node_as_call_signature_declaration.maybe_type_parameters(),
        )?;
        self.emit_parameters(node, node_as_call_signature_declaration.parameters())?;
        self.emit_type_annotation(node_as_call_signature_declaration.maybe_type())?;
        self.write_trailing_semicolon();
        self.pop_name_generation_scope(Some(node));

        Ok(())
    }

    pub(super) fn emit_construct_signature(
        &self,
        node: Id<Node>, /*ConstructSignatureDeclaration*/
    ) -> io::Result<()> {
        self.push_name_generation_scope(Some(node));
        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.write_keyword("new");
        self.write_space();
        let node_ref = node.ref_(self);
        let node_as_construct_signature_declaration = node_ref.as_construct_signature_declaration();
        self.emit_type_parameters(
            node,
            node_as_construct_signature_declaration.maybe_type_parameters(),
        )?;
        self.emit_parameters(node, node_as_construct_signature_declaration.parameters())?;
        self.emit_type_annotation(node_as_construct_signature_declaration.maybe_type())?;
        self.write_trailing_semicolon();
        self.pop_name_generation_scope(Some(node));

        Ok(())
    }

    pub(super) fn emit_index_signature(
        &self,
        node: Id<Node>, /*IndexSignatureDeclaration*/
    ) -> io::Result<()> {
        self.emit_decorators(node, node.ref_(self).maybe_decorators())?;
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        let node_ref = node.ref_(self);
        let node_as_index_signature_declaration = node_ref.as_index_signature_declaration();
        self.emit_parameters_for_index_signature(
            node,
            node_as_index_signature_declaration.parameters(),
        )?;
        self.emit_type_annotation(node_as_index_signature_declaration.maybe_type())?;
        self.write_trailing_semicolon();

        Ok(())
    }

    pub(super) fn emit_template_type_span(
        &self,
        node: Id<Node>, /*TemplateLiteralTypeSpan*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_template_literal_type_span = node_ref.as_template_literal_type_span();
        self.emit(Some(node_as_template_literal_type_span.type_), None)?;
        self.emit(Some(node_as_template_literal_type_span.literal), None)?;

        Ok(())
    }

    pub(super) fn emit_semicolon_class_element(&self) {
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_type_predicate(
        &self,
        node: Id<Node>, /*TypePredicateNode*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_type_predicate_node = node_ref.as_type_predicate_node();
        if let Some(node_asserts_modifier) = node_as_type_predicate_node.asserts_modifier {
            self.emit(Some(node_asserts_modifier), None)?;
            self.write_space();
        }
        self.emit(Some(node_as_type_predicate_node.parameter_name), None)?;
        Ok(if let Some(node_type) = node_as_type_predicate_node.type_ {
            self.write_space();
            self.write_keyword("is");
            self.write_space();
            self.emit(Some(node_type), None)?;
        })
    }

    pub(super) fn emit_type_reference(
        &self,
        node: Id<Node>, /*TypeReferenceNode*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_type_reference_node = node_ref.as_type_reference_node();
        self.emit(Some(node_as_type_reference_node.type_name), None)?;
        self.emit_type_arguments(node, node_as_type_reference_node.maybe_type_arguments())?;

        Ok(())
    }

    pub(super) fn emit_function_type(
        &self,
        node: Id<Node>, /*FunctionTypeNode*/
    ) -> io::Result<()> {
        self.push_name_generation_scope(Some(node));
        let node_ref = node.ref_(self);
        let node_as_function_type_node = node_ref.as_function_type_node();
        self.emit_type_parameters(node, node_as_function_type_node.maybe_type_parameters())?;
        self.emit_parameters_for_arrow(node, node_as_function_type_node.parameters())?;
        self.write_space();
        self.write_punctuation("=>");
        self.write_space();
        self.emit(node_as_function_type_node.maybe_type(), None)?;
        self.pop_name_generation_scope(Some(node));

        Ok(())
    }

    pub(super) fn emit_jsdoc_function_type(
        &self,
        node: Id<Node>, /*JSDocFunctionType*/
    ) -> io::Result<()> {
        self.write_keyword("function");
        let node_ref = node.ref_(self);
        let node_as_jsdoc_function_type = node_ref.as_jsdoc_function_type();
        self.emit_parameters(node, node_as_jsdoc_function_type.parameters())?;
        self.write_punctuation(":");
        self.emit(node_as_jsdoc_function_type.maybe_type(), None)?;

        Ok(())
    }

    pub(super) fn emit_jsdoc_nullable_type(
        &self,
        node: Id<Node>, /*JSDocNullableType*/
    ) -> io::Result<()> {
        self.write_punctuation("?");
        self.emit(
            node.ref_(self).as_base_jsdoc_unary_type().maybe_type(),
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_jsdoc_non_nullable_type(
        &self,
        node: Id<Node>, /*JSDocNonNullableType*/
    ) -> io::Result<()> {
        self.write_punctuation("!");
        self.emit(
            node.ref_(self).as_base_jsdoc_unary_type().maybe_type(),
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_jsdoc_optional_type(
        &self,
        node: Id<Node>, /*JSDocOptionalType*/
    ) -> io::Result<()> {
        self.emit(
            node.ref_(self).as_base_jsdoc_unary_type().maybe_type(),
            None,
        )?;
        self.write_punctuation("=");

        Ok(())
    }

    pub(super) fn emit_constructor_type(
        &self,
        node: Id<Node>, /*ConstructorTypeNode*/
    ) -> io::Result<()> {
        self.push_name_generation_scope(Some(node));
        self.emit_modifiers(node, node.ref_(self).maybe_modifiers())?;
        self.write_keyword("new");
        self.write_space();
        let node_ref = node.ref_(self);
        let node_as_constructor_type_node = node_ref.as_constructor_type_node();
        self.emit_type_parameters(node, node_as_constructor_type_node.maybe_type_parameters())?;
        self.emit_parameters(node, node_as_constructor_type_node.parameters())?;
        self.write_space();
        self.write_punctuation("=>");
        self.write_space();
        self.emit(node_as_constructor_type_node.maybe_type(), None)?;
        self.pop_name_generation_scope(Some(node));

        Ok(())
    }

    pub(super) fn emit_type_query(&self, node: Id<Node> /*TypeQueryNode*/) -> io::Result<()> {
        self.write_keyword("typeof");
        self.write_space();
        self.emit(Some(node.ref_(self).as_type_query_node().expr_name), None)?;

        Ok(())
    }

    pub(super) fn emit_type_literal(
        &self,
        node: Id<Node>, /*TypeLiteralNode*/
    ) -> io::Result<()> {
        self.write_punctuation("{");
        let flags = if get_emit_flags(node, self).intersects(EmitFlags::SingleLine) {
            ListFormat::SingleLineTypeLiteralMembers
        } else {
            ListFormat::MultiLineTypeLiteralMembers
        };
        self.emit_list(
            Some(node),
            Some(node.ref_(self).as_type_literal_node().members),
            flags | ListFormat::NoSpaceIfEmpty,
            None,
            None,
            None,
        )?;
        self.write_punctuation("}");

        Ok(())
    }

    pub(super) fn emit_array_type(&self, node: Id<Node> /*ArrayTypeNode*/) -> io::Result<()> {
        self.emit(
            Some(node.ref_(self).as_array_type_node().element_type),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeElementTypeOfArrayTypeCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        self.write_punctuation("[");
        self.write_punctuation("]");

        Ok(())
    }

    pub(super) fn emit_rest_or_jsdoc_variadic_type(
        &self,
        node: Id<Node>, /*RestTypeNode | JSDocVariadicType*/
    ) -> io::Result<()> {
        self.write_punctuation("...");
        self.emit(node.ref_(self).as_has_type().maybe_type(), None)?;

        Ok(())
    }

    pub(super) fn emit_tuple_type(&self, node: Id<Node> /*TupleTypeNode*/) -> io::Result<()> {
        self.emit_token_with_comment(
            SyntaxKind::OpenBracketToken,
            node.ref_(self).pos(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        let flags = if get_emit_flags(node, self).intersects(EmitFlags::SingleLine) {
            ListFormat::SingleLineTupleTypeElements
        } else {
            ListFormat::MultiLineTupleTypeElements
        };
        let node_ref = node.ref_(self);
        let node_elements = node_ref.as_tuple_type_node().elements;
        self.emit_list(
            Some(node),
            Some(node_elements),
            flags | ListFormat::NoSpaceIfEmpty,
            None,
            None,
            None,
        )?;
        self.emit_token_with_comment(
            SyntaxKind::CloseBracketToken,
            node_elements.ref_(self).end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );

        Ok(())
    }

    pub(super) fn emit_named_tuple_member(
        &self,
        node: Id<Node>, /*NamedTupleMember*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_named_tuple_member = node_ref.as_named_tuple_member();
        self.emit(node_as_named_tuple_member.dot_dot_dot_token, None)?;
        self.emit(Some(node_as_named_tuple_member.name), None)?;
        self.emit(node_as_named_tuple_member.question_token, None)?;
        self.emit_token_with_comment(
            SyntaxKind::ColonToken,
            node_as_named_tuple_member.name.ref_(self).end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.write_space();
        self.emit(Some(node_as_named_tuple_member.type_), None)?;

        Ok(())
    }

    pub(super) fn emit_optional_type(
        &self,
        node: Id<Node>, /*OptionalTypeNode*/
    ) -> io::Result<()> {
        self.emit(
            Some(node.ref_(self).as_optional_type_node().type_),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeElementTypeOfArrayTypeCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        self.write_punctuation("?");

        Ok(())
    }

    pub(super) fn emit_union_type(&self, node: Id<Node> /*UnionTypeNode*/) -> io::Result<()> {
        self.emit_list(
            Some(node),
            Some(node.ref_(self).as_union_type_node().types),
            ListFormat::UnionTypeConstituents,
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeMemberOfElementTypeCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_intersection_type(
        &self,
        node: Id<Node>, /*IntersectionTypeNode*/
    ) -> io::Result<()> {
        self.emit_list(
            Some(node),
            Some(node.ref_(self).as_intersection_type_node().types),
            ListFormat::IntersectionTypeConstituents,
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeMemberOfElementTypeCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_conditional_type(
        &self,
        node: Id<Node>, /*ConditionalTypeNode*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_conditional_type_node = node_ref.as_conditional_type_node();
        self.emit(
            Some(node_as_conditional_type_node.check_type),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeMemberOfConditionalTypeCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        self.write_space();
        self.write_keyword("extends");
        self.write_space();
        self.emit(
            Some(node_as_conditional_type_node.extends_type),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeMemberOfConditionalTypeCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        self.write_space();
        self.write_punctuation("?");
        self.write_space();
        self.emit(Some(node_as_conditional_type_node.true_type), None)?;
        self.write_space();
        self.write_punctuation(":");
        self.write_space();
        self.emit(Some(node_as_conditional_type_node.false_type), None)?;

        Ok(())
    }

    pub(super) fn emit_infer_type(&self, node: Id<Node> /*InferTypeNode*/) -> io::Result<()> {
        self.write_keyword("infer");
        self.write_space();
        self.emit(
            Some(node.ref_(self).as_infer_type_node().type_parameter),
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_parenthesized_type(
        &self,
        node: Id<Node>, /*ParenthesizedTypeNode*/
    ) -> io::Result<()> {
        self.write_punctuation("(");
        self.emit(
            Some(node.ref_(self).as_parenthesized_type_node().type_),
            None,
        )?;
        self.write_punctuation(")");

        Ok(())
    }

    pub(super) fn emit_this_type(&self) {
        self.write_keyword("this");
    }

    pub(super) fn emit_type_operator(
        &self,
        node: Id<Node>, /*TypeOperatorNode*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_type_operator_node = node_ref.as_type_operator_node();
        self.write_token_text(
            node_as_type_operator_node.operator,
            |text| self.write_keyword(text),
            None,
        );
        self.write_space();
        self.emit(
            Some(node_as_type_operator_node.type_),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeMemberOfElementTypeCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;

        Ok(())
    }

    pub(super) fn emit_indexed_access_type(
        &self,
        node: Id<Node>, /*IndexedAccessType*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_indexed_access_type_node = node_ref.as_indexed_access_type_node();
        self.emit(
            Some(node_as_indexed_access_type_node.object_type),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeMemberOfElementTypeCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        self.write_punctuation("[");
        self.emit(Some(node_as_indexed_access_type_node.index_type), None)?;
        self.write_punctuation("]");

        Ok(())
    }

    pub(super) fn emit_mapped_type(
        &self,
        node: Id<Node>, /*MappedTypeNode*/
    ) -> io::Result<()> {
        let emit_flags = get_emit_flags(node, self);
        self.write_punctuation("{");
        if emit_flags.intersects(EmitFlags::SingleLine) {
            self.write_space();
        } else {
            self.write_line(None);
            self.increase_indent();
        }
        let node_ref = node.ref_(self);
        let node_as_mapped_type_node = node_ref.as_mapped_type_node();
        if let Some(node_readonly_token) = node_as_mapped_type_node.readonly_token {
            self.emit(Some(node_readonly_token), None)?;
            if node_readonly_token.ref_(self).kind() != SyntaxKind::ReadonlyKeyword {
                self.write_keyword("readonly");
            }
            self.write_space();
        }
        self.write_punctuation("[");

        self.pipeline_emit(
            EmitHint::MappedTypeParameter,
            node_as_mapped_type_node.type_parameter,
            None,
        )?;
        if let Some(node_name_type) = node_as_mapped_type_node.name_type {
            self.write_space();
            self.write_keyword("as");
            self.write_space();
            self.emit(Some(node_name_type), None)?;
        }

        self.write_punctuation("]");
        if let Some(node_question_token) = node_as_mapped_type_node.question_token {
            self.emit(Some(node_question_token), None)?;
            if node_question_token.ref_(self).kind() != SyntaxKind::QuestionToken {
                self.write_punctuation("?");
            }
        }
        self.write_punctuation(":");
        self.write_space();
        self.emit(node_as_mapped_type_node.type_, None)?;
        self.write_trailing_semicolon();
        if emit_flags.intersects(EmitFlags::SingleLine) {
            self.write_space();
        } else {
            self.write_line(None);
            self.decrease_indent();
        }
        self.write_punctuation("}");

        Ok(())
    }

    pub(super) fn emit_literal_type(
        &self,
        node: Id<Node>, /*LiteralTypeNode*/
    ) -> io::Result<()> {
        self.emit_expression(Some(node.ref_(self).as_literal_type_node().literal), None)?;

        Ok(())
    }

    pub(super) fn emit_template_type(
        &self,
        node: Id<Node>, /*TemplateLiteralTypeNode*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_template_literal_type_node = node_ref.as_template_literal_type_node();
        self.emit(Some(node_as_template_literal_type_node.head), None)?;
        self.emit_list(
            Some(node),
            Some(node_as_template_literal_type_node.template_spans),
            ListFormat::TemplateExpressionSpans,
            None,
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_import_type_node(
        &self,
        node: Id<Node>, /*ImportTypeNode*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_import_type_node = node_ref.as_import_type_node();
        if node_as_import_type_node.is_type_of() {
            self.write_keyword("typeof");
            self.write_space();
        }
        self.write_keyword("import");
        self.write_punctuation("(");
        self.emit(Some(node_as_import_type_node.argument), None)?;
        self.write_punctuation(")");
        if let Some(node_qualifier) = node_as_import_type_node.qualifier {
            self.write_punctuation(".");
            self.emit(Some(node_qualifier), None)?;
        }
        self.emit_type_arguments(node, node_as_import_type_node.maybe_type_arguments())?;

        Ok(())
    }

    pub(super) fn emit_object_binding_pattern(
        &self,
        node: Id<Node>, /*ObjectBindingPattern*/
    ) -> io::Result<()> {
        self.write_punctuation("{");
        self.emit_list(
            Some(node),
            Some(node.ref_(self).as_object_binding_pattern().elements),
            ListFormat::ObjectBindingPatternElements,
            None,
            None,
            None,
        )?;
        self.write_punctuation("}");

        Ok(())
    }

    pub(super) fn emit_array_binding_pattern(
        &self,
        node: Id<Node>, /*ArrayBindingPattern*/
    ) -> io::Result<()> {
        self.write_punctuation("[");
        self.emit_list(
            Some(node),
            Some(node.ref_(self).as_array_binding_pattern().elements),
            ListFormat::ArrayBindingPatternElements,
            None,
            None,
            None,
        )?;
        self.write_punctuation("]");

        Ok(())
    }

    pub(super) fn emit_binding_element(
        &self,
        node: Id<Node>, /*BindingElement*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_binding_element = node_ref.as_binding_element();
        self.emit(node_as_binding_element.dot_dot_dot_token, None)?;
        if let Some(node_property_name) = node_as_binding_element.property_name {
            self.emit(Some(node_property_name), None)?;
            self.write_punctuation(":");
            self.write_space();
        }
        self.emit(node_as_binding_element.maybe_name(), None)?;
        self.emit_initializer(
            node_as_binding_element.maybe_initializer(),
            node_as_binding_element.name().ref_(self).end(),
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

    pub(super) fn emit_array_literal_expression(
        &self,
        node: Id<Node>, /*ArrayLiteralExpression*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_array_literal_expression = node_ref.as_array_literal_expression();
        let elements = node_as_array_literal_expression.elements;
        let prefer_new_line = if node_as_array_literal_expression.multi_line == Some(true) {
            ListFormat::PreferNewLine
        } else {
            ListFormat::None
        };
        self.emit_expression_list(
            Some(node),
            Some(elements),
            ListFormat::ArrayLiteralExpressionElements | prefer_new_line,
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
            None,
            None,
        )?;

        Ok(())
    }

    pub(super) fn emit_object_literal_expression(
        &self,
        node: Id<Node>, /*ObjectLiteralExpression*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_object_literal_expression = node_ref.as_object_literal_expression();
        for_each(
            &*node_as_object_literal_expression.properties.ref_(self),
            |&property: &Id<Node>, _| -> Option<()> {
                self.generate_member_names(Some(property));
                None
            },
        );

        let indented_flag = get_emit_flags(node, self).intersects(EmitFlags::Indented);
        if indented_flag {
            self.increase_indent();
        }

        let prefer_new_line = if node_as_object_literal_expression.multi_line == Some(true) {
            ListFormat::PreferNewLine
        } else {
            ListFormat::None
        };
        let allow_trailing_comma = if self
            .current_source_file()
            .ref_(self)
            .as_source_file()
            .language_version()
            >= ScriptTarget::ES5
            && !is_json_source_file(&self.current_source_file().ref_(self))
        {
            ListFormat::AllowTrailingComma
        } else {
            ListFormat::None
        };
        self.emit_list(
            Some(node),
            Some(node_as_object_literal_expression.properties),
            ListFormat::ObjectLiteralExpressionProperties | allow_trailing_comma | prefer_new_line,
            None,
            None,
            None,
        )?;

        Ok(if indented_flag {
            self.decrease_indent();
        })
    }

    pub(super) fn emit_property_access_expression(
        &self,
        node: Id<Node>, /*PropertyAccessExpression*/
    ) -> io::Result<()> {
        self.emit_expression(
            Some(node.ref_(self).as_property_access_expression().expression),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeLeftSideOfAccessCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        let token = node
            .ref_(self)
            .as_property_access_expression()
            .question_dot_token
            .clone()
            .unwrap_or_else(|| {
                let token: Id<Node> = get_factory(self).create_token(SyntaxKind::DotToken);
                set_text_range_pos_end(
                    &*token.ref_(self),
                    node.ref_(self)
                        .as_property_access_expression()
                        .expression
                        .ref_(self)
                        .end(),
                    node.ref_(self)
                        .as_property_access_expression()
                        .name
                        .ref_(self)
                        .pos(),
                );
                token
            });
        let lines_before_dot = self.get_lines_between_nodes(
            node,
            node.ref_(self).as_property_access_expression().expression,
            token,
        );
        let lines_after_dot = self.get_lines_between_nodes(
            node,
            token,
            node.ref_(self).as_property_access_expression().name,
        );

        self.write_lines_and_indent(lines_before_dot, false);

        let should_emit_dot_dot = token.ref_(self).kind() != SyntaxKind::QuestionDotToken
            && self.may_need_dot_dot_for_property_access(
                node.ref_(self).as_property_access_expression().expression,
            )
            && !self.writer().has_trailing_comment()
            && !self.writer().has_trailing_whitespace();

        if should_emit_dot_dot {
            self.write_punctuation(".");
        }

        if node
            .ref_(self)
            .as_property_access_expression()
            .question_dot_token
            .is_some()
        {
            self.emit(Some(token), None)?;
        } else {
            self.emit_token_with_comment(
                token.ref_(self).kind(),
                node.ref_(self)
                    .as_property_access_expression()
                    .expression
                    .ref_(self)
                    .end(),
                |text: &str| self.write_punctuation(text),
                node,
                None,
            );
        }
        self.write_lines_and_indent(lines_after_dot, false);
        self.emit(
            Some(node.ref_(self).as_property_access_expression().name),
            None,
        )?;
        self.decrease_indent_if(lines_before_dot != 0, Some(lines_after_dot != 0));

        Ok(())
    }

    pub(super) fn may_need_dot_dot_for_property_access(
        &self,
        expression: Id<Node>, /*Expression*/
    ) -> bool {
        let expression = skip_partially_emitted_expressions(expression, self);
        if is_numeric_literal(&expression.ref_(self)) {
            let text = self.get_literal_text_of_node(expression, Some(true), false);
            return expression
                .ref_(self)
                .as_numeric_literal()
                .numeric_literal_flags
                == TokenFlags::None
                && !string_contains(&text, token_to_string(SyntaxKind::DotToken).unwrap());
        } else if is_access_expression(&expression.ref_(self)) {
            let constant_value = get_constant_value(expression, self);
            return matches!(
                constant_value,
                Some(StringOrNumber::Number(constant_value)) if is_finite(&constant_value) &&
                    constant_value.value().floor() == constant_value.value()
            );
        }
        false
    }

    pub(super) fn emit_element_access_expression(
        &self,
        node: Id<Node>, /*ElementAccessExpression*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_element_access_expression = node_ref.as_element_access_expression();
        self.emit_expression(
            Some(node_as_element_access_expression.expression),
            Some(self.alloc_current_parenthesizer_rule(Box::new(
                ParenthesizeLeftSideOfAccessCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                    self,
                ),
            ))),
        )?;
        self.emit(node_as_element_access_expression.question_dot_token, None)?;
        self.emit_token_with_comment(
            SyntaxKind::OpenBracketToken,
            node_as_element_access_expression
                .expression
                .ref_(self)
                .end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_expression(
            Some(node_as_element_access_expression.argument_expression),
            None,
        )?;
        self.emit_token_with_comment(
            SyntaxKind::CloseBracketToken,
            node_as_element_access_expression
                .argument_expression
                .ref_(self)
                .end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );

        Ok(())
    }
}
