use gc::Gc;
use std::rc::Rc;

use super::{
    ParenthesizeElementTypeOfArrayTypeCurrentParenthesizerRule,
    ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule,
    ParenthesizeLeftSideOfAccessCurrentParenthesizerRule,
    ParenthesizeMemberOfConditionalTypeCurrentParenthesizerRule,
    ParenthesizeMemberOfElementTypeCurrentParenthesizerRule,
};
use crate::{
    for_each, get_constant_value, get_emit_flags, is_access_expression, is_finite,
    is_json_source_file, is_numeric_literal, set_text_range_pos_end,
    skip_partially_emitted_expressions, string_contains, token_to_string, with_synthetic_factory,
    with_synthetic_factory_and_factory, EmitFlags, EmitHint, FunctionLikeDeclarationInterface,
    HasInitializerInterface, HasQuestionTokenInterface, HasTypeArgumentsInterface,
    HasTypeInterface, HasTypeParametersInterface, ListFormat, NamedDeclarationInterface, Node,
    NodeInterface, Printer, ReadonlyTextRange, ScriptTarget, SignatureDeclarationInterface,
    StringOrNumber, SyntaxKind, TokenFlags,
};

impl Printer {
    pub(super) fn emit_type_parameter(&self, node: &Node /*TypeParameterDeclaration*/) {
        let node_as_type_parameter_declaration = node.as_type_parameter_declaration();
        self.emit(
            node_as_type_parameter_declaration.maybe_name().as_deref(),
            None,
        );
        if let Some(node_constraint) = node_as_type_parameter_declaration.constraint.as_ref() {
            self.write_space();
            self.write_keyword("extends");
            self.write_space();
            self.emit(Some(&**node_constraint), None);
        }
        if let Some(node_default) = node_as_type_parameter_declaration.default.as_ref() {
            self.write_space();
            self.write_operator("=");
            self.write_space();
            self.emit(Some(&**node_default), None);
        }
    }

    pub(super) fn emit_parameter(&self, node: &Node /*ParameterDeclaration*/) {
        let node_as_parameter_declaration = node.as_parameter_declaration();
        self.emit_decorators(node, node.maybe_decorators().as_deref());
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.emit(
            node_as_parameter_declaration.dot_dot_dot_token.as_deref(),
            None,
        );
        self.emit_node_with_writer(
            node_as_parameter_declaration.maybe_name().as_deref(),
            Printer::write_parameter,
        );
        self.emit(
            node_as_parameter_declaration.question_token.as_deref(),
            None,
        );
        if matches!(
            node.maybe_parent(),
            Some(node_parent) if node_parent.kind() == SyntaxKind::JSDocFunctionType &&
                node_as_parameter_declaration.maybe_name().is_none()
        ) {
            self.emit(node_as_parameter_declaration.maybe_type().as_deref(), None);
        } else {
            self.emit_type_annotation(node_as_parameter_declaration.maybe_type().as_deref());
        }
        self.emit_initializer(
            node_as_parameter_declaration.maybe_initializer(),
            if let Some(node_type) = node_as_parameter_declaration.maybe_type() {
                node_type.end()
            } else if let Some(node_question_token) =
                node_as_parameter_declaration.question_token.as_ref()
            {
                node_question_token.end()
            } else if let Some(node_name) = node_as_parameter_declaration.maybe_name() {
                node_name.end()
            } else if let Some(node_modifiers) = node.maybe_modifiers().as_ref() {
                node_modifiers.end()
            } else if let Some(node_decorators) = node.maybe_decorators().as_ref() {
                node_decorators.end()
            } else {
                node.pos()
            },
            node,
            Some(Gc::new(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
        );
    }

    pub(super) fn emit_decorator(&self, node: &Node /*Decorator*/) {
        self.write_punctuation("@");
        self.emit_expression(
            Some(&*node.as_decorator().expression),
            Some(Gc::new(Box::new(
                ParenthesizeLeftSideOfAccessCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
    }

    pub(super) fn emit_property_signature(&self, node: &Node /*PropertySignature*/) {
        let node_as_property_signature = node.as_property_signature();
        self.emit_decorators(node, node.maybe_decorators().as_deref());
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.emit_node_with_writer(
            Some(&*node_as_property_signature.name()),
            Printer::write_property,
        );
        self.emit(node_as_property_signature.question_token.as_deref(), None);
        self.emit_type_annotation(node_as_property_signature.maybe_type());
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_property_declaration(&self, node: &Node /*PropertyDeclaration*/) {
        let node_as_property_declaration = node.as_property_declaration();
        self.emit_decorators(node, node.maybe_decorators().as_deref());
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.emit(node_as_property_declaration.maybe_name().as_deref(), None);
        self.emit(node_as_property_declaration.question_token.as_deref(), None);
        self.emit(
            node_as_property_declaration.exclamation_token.as_deref(),
            None,
        );
        self.emit_type_annotation(node_as_property_declaration.maybe_type().as_deref());
        self.emit_initializer(
            node_as_property_declaration.maybe_initializer(),
            if let Some(node_type) = node_as_property_declaration.maybe_type() {
                node_type.end()
            } else if let Some(node_question_token) =
                node_as_property_declaration.question_token.as_ref()
            {
                node_question_token.end()
            } else {
                node_as_property_declaration.name().pos()
            },
            node,
            None,
        );
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_method_signature(&self, node: &Node /*MethodSignature*/) {
        self.push_name_generation_scope(Some(node));
        self.emit_decorators(node, node.maybe_decorators().as_deref());
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        let node_as_method_signature = node.as_method_signature();
        self.emit(node_as_method_signature.maybe_name().as_deref(), None);
        self.emit(node_as_method_signature.question_token.as_deref(), None);
        self.emit_type_parameters(
            node,
            node_as_method_signature.maybe_type_parameters().as_deref(),
        );
        self.emit_parameters(node, &node_as_method_signature.parameters());
        self.emit_type_annotation(node_as_method_signature.maybe_type());
        self.write_trailing_semicolon();
        self.pop_name_generation_scope(Some(node));
    }

    pub(super) fn emit_method_declaration(&self, node: &Node /*MethodDeclaration*/) {
        self.emit_decorators(node, node.maybe_decorators().as_deref());
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        let node_as_method_declaration = node.as_method_declaration();
        self.emit(
            node_as_method_declaration.maybe_asterisk_token().as_deref(),
            None,
        );
        self.emit(node_as_method_declaration.maybe_name().as_deref(), None);
        self.emit(
            node_as_method_declaration.maybe_question_token().as_deref(),
            None,
        );
        self.emit_signature_and_body(node, |node: &Node| self.emit_signature_head(node));
    }

    pub(super) fn emit_class_static_block_declaration(
        &self,
        node: &Node, /*ClassStaticBlockDeclaration*/
    ) {
        self.emit_decorators(node, node.maybe_decorators().as_deref());
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.write_keyword("static");
        self.emit_block_function_body(&node.as_class_static_block_declaration().body);
    }

    pub(super) fn emit_constructor(&self, node: &Node /*ConstructorDeclaration*/) {
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.write_keyword("constructor");
        self.emit_signature_and_body(node, |node: &Node| self.emit_signature_head(node));
    }

    pub(super) fn emit_accessor_declaration(&self, node: &Node /*AccessorDeclaration*/) {
        self.emit_decorators(node, node.maybe_decorators().as_deref());
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.write_keyword(if node.kind() == SyntaxKind::GetAccessor {
            "get"
        } else {
            "set"
        });
        self.write_space();
        self.emit(node.as_named_declaration().maybe_name().as_deref(), None);
        self.emit_signature_and_body(node, |node: &Node| self.emit_signature_head(node));
    }

    pub(super) fn emit_call_signature(&self, node: &Node /*CallSignatureDeclaration*/) {
        self.push_name_generation_scope(Some(node));
        self.emit_decorators(node, node.maybe_decorators().as_deref());
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        let node_as_call_signature_declaration = node.as_call_signature_declaration();
        self.emit_type_parameters(
            node,
            node_as_call_signature_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        self.emit_parameters(node, &node_as_call_signature_declaration.parameters());
        self.emit_type_annotation(node_as_call_signature_declaration.maybe_type());
        self.write_trailing_semicolon();
        self.pop_name_generation_scope(Some(node));
    }

    pub(super) fn emit_construct_signature(
        &self,
        node: &Node, /*ConstructSignatureDeclaration*/
    ) {
        self.push_name_generation_scope(Some(node));
        self.emit_decorators(node, node.maybe_decorators().as_deref());
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.write_keyword("new");
        self.write_space();
        let node_as_construct_signature_declaration = node.as_construct_signature_declaration();
        self.emit_type_parameters(
            node,
            node_as_construct_signature_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        self.emit_parameters(node, &node_as_construct_signature_declaration.parameters());
        self.emit_type_annotation(node_as_construct_signature_declaration.maybe_type());
        self.write_trailing_semicolon();
        self.pop_name_generation_scope(Some(node));
    }

    pub(super) fn emit_index_signature(&self, node: &Node /*IndexSignatureDeclaration*/) {
        self.emit_decorators(node, node.maybe_decorators().as_deref());
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        let node_as_index_signature_declaration = node.as_index_signature_declaration();
        self.emit_parameters_for_index_signature(
            node,
            &node_as_index_signature_declaration.parameters(),
        );
        self.emit_type_annotation(node_as_index_signature_declaration.maybe_type());
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_template_type_span(&self, node: &Node /*TemplateLiteralTypeSpan*/) {
        let node_as_template_literal_type_span = node.as_template_literal_type_span();
        self.emit(Some(&*node_as_template_literal_type_span.type_), None);
        self.emit(Some(&*node_as_template_literal_type_span.literal), None);
    }

    pub(super) fn emit_semicolon_class_element(&self) {
        self.write_trailing_semicolon();
    }

    pub(super) fn emit_type_predicate(&self, node: &Node /*TypePredicateNode*/) {
        let node_as_type_predicate_node = node.as_type_predicate_node();
        if let Some(node_asserts_modifier) = node_as_type_predicate_node.asserts_modifier.as_ref() {
            self.emit(Some(&**node_asserts_modifier), None);
            self.write_space();
        }
        self.emit(Some(&*node_as_type_predicate_node.parameter_name), None);
        if let Some(node_type) = node_as_type_predicate_node.type_.as_ref() {
            self.write_space();
            self.write_keyword("is");
            self.write_space();
            self.emit(Some(&**node_type), None);
        }
    }

    pub(super) fn emit_type_reference(&self, node: &Node /*TypeReferenceNode*/) {
        let node_as_type_reference_node = node.as_type_reference_node();
        self.emit(Some(&*node_as_type_reference_node.type_name), None);
        self.emit_type_arguments(
            node,
            node_as_type_reference_node
                .maybe_type_arguments()
                .as_deref(),
        );
    }

    pub(super) fn emit_function_type(&self, node: &Node /*FunctionTypeNode*/) {
        self.push_name_generation_scope(Some(node));
        let node_as_function_type_node = node.as_function_type_node();
        self.emit_type_parameters(
            node,
            node_as_function_type_node
                .maybe_type_parameters()
                .as_deref(),
        );
        self.emit_parameters_for_arrow(node, &node_as_function_type_node.parameters());
        self.write_space();
        self.write_punctuation("=>");
        self.write_space();
        self.emit(node_as_function_type_node.maybe_type().as_deref(), None);
        self.pop_name_generation_scope(Some(node));
    }

    pub(super) fn emit_jsdoc_function_type(&self, node: &Node /*JSDocFunctionType*/) {
        self.write_keyword("function");
        let node_as_jsdoc_function_type = node.as_jsdoc_function_type();
        self.emit_parameters(node, &node_as_jsdoc_function_type.parameters());
        self.write_punctuation(":");
        self.emit(node_as_jsdoc_function_type.maybe_type().as_deref(), None);
    }

    pub(super) fn emit_jsdoc_nullable_type(&self, node: &Node /*JSDocNullableType*/) {
        self.write_punctuation("?");
        self.emit(
            node.as_base_jsdoc_unary_type().maybe_type().as_deref(),
            None,
        );
    }

    pub(super) fn emit_jsdoc_non_nullable_type(&self, node: &Node /*JSDocNonNullableType*/) {
        self.write_punctuation("!");
        self.emit(
            node.as_base_jsdoc_unary_type().maybe_type().as_deref(),
            None,
        );
    }

    pub(super) fn emit_jsdoc_optional_type(&self, node: &Node /*JSDocOptionalType*/) {
        self.emit(
            node.as_base_jsdoc_unary_type().maybe_type().as_deref(),
            None,
        );
        self.write_punctuation("=");
    }

    pub(super) fn emit_constructor_type(&self, node: &Node /*ConstructorTypeNode*/) {
        self.push_name_generation_scope(Some(node));
        self.emit_modifiers(node, node.maybe_modifiers().as_deref());
        self.write_keyword("new");
        self.write_space();
        let node_as_constructor_type_node = node.as_constructor_type_node();
        self.emit_type_parameters(
            node,
            node_as_constructor_type_node
                .maybe_type_parameters()
                .as_deref(),
        );
        self.emit_parameters(node, &node_as_constructor_type_node.parameters());
        self.write_space();
        self.write_punctuation("=>");
        self.write_space();
        self.emit(node_as_constructor_type_node.maybe_type().as_deref(), None);
        self.pop_name_generation_scope(Some(node));
    }

    pub(super) fn emit_type_query(&self, node: &Node /*TypeQueryNode*/) {
        self.write_keyword("typeof");
        self.write_space();
        self.emit(Some(&*node.as_type_query_node().expr_name), None);
    }

    pub(super) fn emit_type_literal(&self, node: &Node /*TypeLiteralNode*/) {
        self.write_punctuation("{");
        let flags = if get_emit_flags(node).intersects(EmitFlags::SingleLine) {
            ListFormat::SingleLineTypeLiteralMembers
        } else {
            ListFormat::MultiLineTypeLiteralMembers
        };
        self.emit_list(
            Some(node),
            Some(&node.as_type_literal_node().members),
            flags | ListFormat::NoSpaceIfEmpty,
            None,
            None,
            None,
        );
        self.write_punctuation("}");
    }

    pub(super) fn emit_array_type(&self, node: &Node /*ArrayTypeNode*/) {
        self.emit(
            Some(&*node.as_array_type_node().element_type),
            Some(Gc::new(Box::new(
                ParenthesizeElementTypeOfArrayTypeCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
        );
        self.write_punctuation("[");
        self.write_punctuation("]");
    }

    pub(super) fn emit_rest_or_jsdoc_variadic_type(
        &self,
        node: &Node, /*RestTypeNode | JSDocVariadicType*/
    ) {
        self.write_punctuation("...");
        self.emit(node.as_has_type().maybe_type().as_deref(), None);
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
            None,
            None,
            None,
        );
        self.emit_token_with_comment(
            SyntaxKind::CloseBracketToken,
            node_elements.end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
    }

    pub(super) fn emit_named_tuple_member(&self, node: &Node /*NamedTupleMember*/) {
        let node_as_named_tuple_member = node.as_named_tuple_member();
        self.emit(
            node_as_named_tuple_member.dot_dot_dot_token.as_deref(),
            None,
        );
        self.emit(Some(&*node_as_named_tuple_member.name), None);
        self.emit(node_as_named_tuple_member.question_token.as_deref(), None);
        self.emit_token_with_comment(
            SyntaxKind::ColonToken,
            node_as_named_tuple_member.name.end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.write_space();
        self.emit(Some(&*node_as_named_tuple_member.type_), None);
    }

    pub(super) fn emit_optional_type(&self, node: &Node /*OptionalTypeNode*/) {
        self.emit(
            Some(&*node.as_optional_type_node().type_),
            Some(Gc::new(Box::new(
                ParenthesizeElementTypeOfArrayTypeCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
        );
        self.write_punctuation("?");
    }

    pub(super) fn emit_union_type(&self, node: &Node /*UnionTypeNode*/) {
        self.emit_list(
            Some(node),
            Some(&node.as_union_type_node().types),
            ListFormat::UnionTypeConstituents,
            Some(Gc::new(Box::new(
                ParenthesizeMemberOfElementTypeCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
            None,
            None,
        );
    }

    pub(super) fn emit_intersection_type(&self, node: &Node /*IntersectionTypeNode*/) {
        self.emit_list(
            Some(node),
            Some(&node.as_intersection_type_node().types),
            ListFormat::IntersectionTypeConstituents,
            Some(Gc::new(Box::new(
                ParenthesizeMemberOfElementTypeCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
            None,
            None,
        );
    }

    pub(super) fn emit_conditional_type(&self, node: &Node /*ConditionalTypeNode*/) {
        let node_as_conditional_type_node = node.as_conditional_type_node();
        self.emit(
            Some(&*node_as_conditional_type_node.check_type),
            Some(Gc::new(Box::new(
                ParenthesizeMemberOfConditionalTypeCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
        );
        self.write_space();
        self.write_keyword("extends");
        self.write_space();
        self.emit(
            Some(&*node_as_conditional_type_node.extends_type),
            Some(Gc::new(Box::new(
                ParenthesizeMemberOfConditionalTypeCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
        );
        self.write_space();
        self.write_punctuation("?");
        self.write_space();
        self.emit(Some(&*node_as_conditional_type_node.true_type), None);
        self.write_space();
        self.write_punctuation(":");
        self.write_space();
        self.emit(Some(&*node_as_conditional_type_node.false_type), None);
    }

    pub(super) fn emit_infer_type(&self, node: &Node /*InferTypeNode*/) {
        self.write_keyword("infer");
        self.write_space();
        self.emit(Some(&*node.as_infer_type_node().type_parameter), None);
    }

    pub(super) fn emit_parenthesized_type(&self, node: &Node /*ParenthesizedTypeNode*/) {
        self.write_punctuation("(");
        self.emit(Some(&*node.as_parenthesized_type_node().type_), None);
        self.write_punctuation(")");
    }

    pub(super) fn emit_this_type(&self) {
        self.write_keyword("this");
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
            Some(Gc::new(Box::new(
                ParenthesizeMemberOfElementTypeCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
    }

    pub(super) fn emit_indexed_access_type(&self, node: &Node /*IndexedAccessType*/) {
        let node_as_indexed_access_type_node = node.as_indexed_access_type_node();
        self.emit(
            Some(&*node_as_indexed_access_type_node.object_type),
            Some(Gc::new(Box::new(
                ParenthesizeMemberOfElementTypeCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
        self.write_punctuation("[");
        self.emit(Some(&*node_as_indexed_access_type_node.index_type), None);
        self.write_punctuation("]");
    }

    pub(super) fn emit_mapped_type(&self, node: &Node /*MappedTypeNode*/) {
        let emit_flags = get_emit_flags(node);
        self.write_punctuation("{");
        if emit_flags.intersects(EmitFlags::SingleLine) {
            self.write_space();
        } else {
            self.write_line(None);
            self.increase_indent();
        }
        let node_as_mapped_type_node = node.as_mapped_type_node();
        if let Some(node_readonly_token) = node_as_mapped_type_node.readonly_token.as_ref() {
            self.emit(Some(&**node_readonly_token), None);
            if node_readonly_token.kind() != SyntaxKind::ReadonlyKeyword {
                self.write_keyword("readonly");
            }
            self.write_space();
        }
        self.write_punctuation("[");

        self.pipeline_emit(
            EmitHint::MappedTypeParameter,
            &node_as_mapped_type_node.type_parameter,
            None,
        );
        if let Some(node_name_type) = node_as_mapped_type_node.name_type.as_ref() {
            self.write_space();
            self.write_keyword("as");
            self.write_space();
            self.emit(Some(&**node_name_type), None);
        }

        self.write_punctuation("]");
        if let Some(node_question_token) = node_as_mapped_type_node.question_token.as_ref() {
            self.emit(Some(&**node_question_token), None);
            if node_question_token.kind() != SyntaxKind::QuestionToken {
                self.write_punctuation("?");
            }
        }
        self.write_punctuation(":");
        self.write_space();
        self.emit(node_as_mapped_type_node.type_.as_deref(), None);
        self.write_trailing_semicolon();
        if emit_flags.intersects(EmitFlags::SingleLine) {
            self.write_space();
        } else {
            self.write_line(None);
            self.decrease_indent();
        }
        self.write_punctuation("}");
    }

    pub(super) fn emit_literal_type(&self, node: &Node /*LiteralTypeNode*/) {
        self.emit_expression(Some(&*node.as_literal_type_node().literal), None);
    }

    pub(super) fn emit_template_type(&self, node: &Node /*TemplateLiteralTypeNode*/) {
        let node_as_template_literal_type_node = node.as_template_literal_type_node();
        self.emit(Some(&*node_as_template_literal_type_node.head), None);
        self.emit_list(
            Some(node),
            Some(&node_as_template_literal_type_node.template_spans),
            ListFormat::TemplateExpressionSpans,
            None,
            None,
            None,
        );
    }

    pub(super) fn emit_import_type_node(&self, node: &Node /*ImportTypeNode*/) {
        let node_as_import_type_node = node.as_import_type_node();
        if node_as_import_type_node.is_type_of() {
            self.write_keyword("typeof");
            self.write_space();
        }
        self.write_keyword("import");
        self.write_punctuation("(");
        self.emit(Some(&*node_as_import_type_node.argument), None);
        self.write_punctuation(")");
        if let Some(node_qualifier) = node_as_import_type_node.qualifier.as_ref() {
            self.write_punctuation(".");
            self.emit(Some(&**node_qualifier), None);
        }
        self.emit_type_arguments(
            node,
            node_as_import_type_node.maybe_type_arguments().as_deref(),
        );
    }

    pub(super) fn emit_object_binding_pattern(&self, node: &Node /*ObjectBindingPattern*/) {
        self.write_punctuation("{");
        self.emit_list(
            Some(node),
            Some(&node.as_object_binding_pattern().elements),
            ListFormat::ObjectBindingPatternElements,
            None,
            None,
            None,
        );
        self.write_punctuation("}");
    }

    pub(super) fn emit_array_binding_pattern(&self, node: &Node /*ArrayBindingPattern*/) {
        self.write_punctuation("[");
        self.emit_list(
            Some(node),
            Some(&node.as_array_binding_pattern().elements),
            ListFormat::ArrayBindingPatternElements,
            None,
            None,
            None,
        );
        self.write_punctuation("]");
    }

    pub(super) fn emit_binding_element(&self, node: &Node /*BindingElement*/) {
        let node_as_binding_element = node.as_binding_element();
        self.emit(node_as_binding_element.dot_dot_dot_token.as_deref(), None);
        if let Some(node_property_name) = node_as_binding_element.property_name.as_ref() {
            self.emit(Some(&**node_property_name), None);
            self.write_punctuation(":");
            self.write_space();
        }
        self.emit(node_as_binding_element.maybe_name().as_deref(), None);
        self.emit_initializer(
            node_as_binding_element.maybe_initializer(),
            node_as_binding_element.name().end(),
            node,
            Some(Gc::new(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
        );
    }

    pub(super) fn emit_array_literal_expression(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
    ) {
        let node_as_array_literal_expression = node.as_array_literal_expression();
        let elements = &node_as_array_literal_expression.elements;
        let prefer_new_line = if node_as_array_literal_expression.multi_line == Some(true) {
            ListFormat::PreferNewLine
        } else {
            ListFormat::None
        };
        self.emit_expression_list(
            Some(node),
            Some(elements),
            ListFormat::ArrayLiteralExpressionElements | prefer_new_line,
            Some(Gc::new(Box::new(
                ParenthesizeExpressionForDisallowedCommaCurrentParenthesizerRule::new(
                    self.parenthesizer(),
                ),
            ))),
            None,
            None,
        );
    }

    pub(super) fn emit_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) {
        let node_as_object_literal_expression = node.as_object_literal_expression();
        for_each(
            &node_as_object_literal_expression.properties,
            |property: &Gc<Node>, _| -> Option<()> {
                self.generate_member_names(Some(&**property));
                None
            },
        );

        let indented_flag = get_emit_flags(node).intersects(EmitFlags::Indented);
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
            .as_source_file()
            .language_version()
            >= ScriptTarget::ES5
            && !is_json_source_file(&self.current_source_file())
        {
            ListFormat::AllowTrailingComma
        } else {
            ListFormat::None
        };
        self.emit_list(
            Some(node),
            Some(&node_as_object_literal_expression.properties),
            ListFormat::ObjectLiteralExpressionProperties | allow_trailing_comma | prefer_new_line,
            None,
            None,
            None,
        );

        if indented_flag {
            self.decrease_indent();
        }
    }

    pub(super) fn emit_property_access_expression(
        &self,
        node: &Node, /*PropertyAccessExpression*/
    ) {
        let node_as_property_access_expression = node.as_property_access_expression();
        self.emit_expression(
            Some(&*node_as_property_access_expression.expression),
            Some(Gc::new(Box::new(
                ParenthesizeLeftSideOfAccessCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
        let token = node_as_property_access_expression
            .question_dot_token
            .clone()
            .unwrap_or_else(|| {
                let token: Gc<Node> =
                    with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                        factory
                            .create_token(synthetic_factory, SyntaxKind::DotToken)
                            .wrap()
                    });
                set_text_range_pos_end(
                    &*token,
                    node_as_property_access_expression.expression.end(),
                    node_as_property_access_expression.name.pos(),
                );
                token
            });
        let lines_before_dot = self.get_lines_between_nodes(
            node,
            &node_as_property_access_expression.expression,
            &token,
        );
        let lines_after_dot =
            self.get_lines_between_nodes(node, &token, &node_as_property_access_expression.name);

        self.write_lines_and_indent(lines_before_dot, false);

        let should_emit_dot_dot = token.kind() != SyntaxKind::QuestionDotToken
            && self.may_need_dot_dot_for_property_access(
                &node_as_property_access_expression.expression,
            )
            && !self.writer().has_trailing_comment()
            && !self.writer().has_trailing_whitespace();

        if should_emit_dot_dot {
            self.write_punctuation(".");
        }

        if node_as_property_access_expression
            .question_dot_token
            .is_some()
        {
            self.emit(Some(&*token), None);
        } else {
            self.emit_token_with_comment(
                token.kind(),
                node_as_property_access_expression.expression.end(),
                |text: &str| self.write_punctuation(text),
                node,
                None,
            );
        }
        self.write_lines_and_indent(lines_after_dot, false);
        self.emit(Some(&*node_as_property_access_expression.name), None);
        self.decrease_indent_if(lines_before_dot != 0, Some(lines_after_dot != 0));
    }

    pub(super) fn may_need_dot_dot_for_property_access(
        &self,
        expression: &Node, /*Expression*/
    ) -> bool {
        let ref expression = skip_partially_emitted_expressions(expression);
        if is_numeric_literal(expression) {
            let text = self.get_literal_text_of_node(expression, Some(true), false);
            return expression.as_numeric_literal().numeric_literal_flags == TokenFlags::None
                && !string_contains(&text, token_to_string(SyntaxKind::DotToken).unwrap());
        } else if is_access_expression(expression) {
            let constant_value = get_constant_value(expression);
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
        node: &Node, /*ElementAccessExpression*/
    ) {
        let node_as_element_access_expression = node.as_element_access_expression();
        self.emit_expression(
            Some(&*node_as_element_access_expression.expression),
            Some(Gc::new(Box::new(
                ParenthesizeLeftSideOfAccessCurrentParenthesizerRule::new(self.parenthesizer()),
            ))),
        );
        self.emit(
            node_as_element_access_expression
                .question_dot_token
                .as_deref(),
            None,
        );
        self.emit_token_with_comment(
            SyntaxKind::OpenBracketToken,
            node_as_element_access_expression.expression.end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
        self.emit_expression(
            Some(&*node_as_element_access_expression.argument_expression),
            None,
        );
        self.emit_token_with_comment(
            SyntaxKind::CloseBracketToken,
            node_as_element_access_expression.argument_expression.end(),
            |text: &str| self.write_punctuation(text),
            node,
            None,
        );
    }
}
