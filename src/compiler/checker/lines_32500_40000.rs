#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, IterationTypeKind, IterationUse, TypeFacts, UnusedKind};
use crate::{
    for_each, get_combined_node_flags, get_containing_function_or_class_static_block,
    get_effective_initializer, get_function_flags, is_assignment_operator, is_binding_element,
    is_function_or_module_block, is_private_identifier,
    is_private_identifier_property_access_expression, is_spread_assignment, map, maybe_for_each,
    parse_pseudo_big_int, skip_parentheses, AccessFlags, Diagnostic, DiagnosticMessage,
    Diagnostics, ExternalEmitHelpers, FunctionFlags, HasTypeParametersInterface, InferenceContext,
    InferenceInfo, IterationTypes, IterationTypesResolver, LiteralLikeNodeInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeFlags, NodeInterface, Number, PseudoBigInt,
    ScriptTarget, Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
    UnionOrIntersectionTypeInterface,
};

impl TypeChecker {
    pub(super) fn check_object_literal_assignment(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        source_type: &Type,
        right_is_this: Option<bool>,
    ) -> Rc<Type> {
        let properties = &node.as_object_literal_expression().properties;
        if self.strict_null_checks && properties.is_empty() {
            return self.check_non_null_type(source_type, node);
        }
        for i in 0..properties.len() {
            self.check_object_literal_destructuring_property_assignment(
                node,
                source_type,
                i,
                Some(properties),
                right_is_this,
            );
        }
        source_type.type_wrapper()
    }

    pub(super) fn check_object_literal_destructuring_property_assignment(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        object_literal_type: &Type,
        property_index: usize,
        all_properties: Option<&NodeArray /*<ObjectLiteralElementLike>*/>,
        right_is_this: Option<bool>,
    ) -> Option<Rc<Type>> {
        let right_is_this = right_is_this.unwrap_or(false);
        let properties = &node.as_object_literal_expression().properties;
        let property = &properties[property_index];
        match property.kind() {
            SyntaxKind::PropertyAssignment | SyntaxKind::ShorthandPropertyAssignment => {
                let name = property.as_named_declaration().name();
                let expr_type = self.get_literal_type_from_property_name(&name);
                if self.is_type_usable_as_property_name(&expr_type) {
                    let text = self.get_property_name_from_type(&expr_type);
                    let prop = self.get_property_of_type_(object_literal_type, &text, None);
                    if let Some(prop) = prop.as_ref() {
                        self.mark_property_as_referenced(prop, Some(&**property), right_is_this);
                        self.check_property_accessibility(
                            property,
                            false,
                            true,
                            object_literal_type,
                            prop,
                            None,
                        );
                    }
                }
                let element_type = self.get_indexed_access_type(
                    object_literal_type,
                    &expr_type,
                    Some(AccessFlags::ExpressionPosition),
                    Some(&*name),
                    Option::<&Symbol>::None,
                    None,
                );
                let type_ = self.get_flow_type_of_destructuring(property, &element_type);
                Some(self.check_destructuring_assignment(
                    &*if property.kind() == SyntaxKind::ShorthandPropertyAssignment {
                        property.clone()
                    } else {
                        property.as_has_initializer().maybe_initializer().unwrap()
                    },
                    &type_,
                    None,
                    None,
                ))
            }
            SyntaxKind::SpreadAssignment => {
                if property_index < properties.len() - 1 {
                    self.error(
                        Some(&**property),
                        &Diagnostics::A_rest_element_must_be_last_in_a_destructuring_pattern,
                        None,
                    );
                    None
                } else {
                    if self.language_version < ScriptTarget::ESNext {
                        self.check_external_emit_helpers(property, ExternalEmitHelpers::Rest);
                    }
                    let mut non_rest_names: Vec<Rc<Node /*PropertyName*/>> = vec![];
                    if let Some(all_properties) = all_properties {
                        for other_property in all_properties {
                            if !is_spread_assignment(other_property) {
                                non_rest_names.push(other_property.as_named_declaration().name());
                            }
                        }
                    }
                    let type_ = self.get_rest_type(
                        object_literal_type,
                        &non_rest_names,
                        object_literal_type.maybe_symbol(),
                    );
                    self.check_grammar_for_disallowed_trailing_comma(
                        all_properties,
                        Some(&Diagnostics::A_rest_parameter_or_binding_pattern_may_not_have_a_trailing_comma)
                    );
                    Some(self.check_destructuring_assignment(
                        &property.as_has_expression().expression(),
                        &type_,
                        None,
                        None,
                    ))
                }
            }
            _ => {
                self.error(
                    Some(&**property),
                    &Diagnostics::Property_assignment_expected,
                    None,
                );
                None
            }
        }
    }

    pub(super) fn check_array_literal_assignment(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
        source_type: &Type,
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let node_as_array_literal_expression = node.as_array_literal_expression();
        let elements = &node_as_array_literal_expression.elements;
        if self.language_version < ScriptTarget::ES2015
            && self.compiler_options.downlevel_iteration == Some(true)
        {
            self.check_external_emit_helpers(node, ExternalEmitHelpers::Read);
        }
        let possibly_out_of_bounds_type = self.check_iterated_type_or_element_type(
            IterationUse::Destructuring | IterationUse::PossiblyOutOfBounds,
            source_type,
            &self.undefined_type(),
            Some(node)
        ) /*|| errorType*/;
        let mut in_bounds_type = if self.compiler_options.no_unchecked_indexed_access == Some(true)
        {
            None
        } else {
            Some(possibly_out_of_bounds_type.clone())
        };
        for i in 0..elements.len() {
            let mut type_ = possibly_out_of_bounds_type.clone();
            if node_as_array_literal_expression.elements[i].kind() == SyntaxKind::SpreadElement {
                in_bounds_type = Some(in_bounds_type.unwrap_or_else(|| {
                    self.check_iterated_type_or_element_type(
                        IterationUse::Destructuring,
                        source_type,
                        &self.undefined_type(),
                        Some(node),
                    ) /*|| errorType*/
                }));
                type_ = in_bounds_type.clone().unwrap();
            }
            self.check_array_literal_destructuring_element_assignment(
                node,
                source_type,
                i,
                &type_,
                check_mode,
            );
        }
        source_type.type_wrapper()
    }

    pub(super) fn check_array_literal_destructuring_element_assignment(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
        source_type: &Type,
        element_index: usize,
        element_type: &Type,
        check_mode: Option<CheckMode>,
    ) -> Option<Rc<Type>> {
        let node_as_array_literal_expression = node.as_array_literal_expression();
        let elements = &node_as_array_literal_expression.elements;
        let element = &elements[element_index];
        if element.kind() != SyntaxKind::OmittedExpression {
            if element.kind() != SyntaxKind::SpreadElement {
                let index_type = self.get_number_literal_type(Number::new(element_index as f64));
                if self.is_array_like_type(source_type) {
                    let access_flags = AccessFlags::ExpressionPosition
                        | if self.has_default_value(element) {
                            AccessFlags::NoTupleBoundsCheck
                        } else {
                            AccessFlags::None
                        };
                    let element_type = self
                        .get_indexed_access_type_or_undefined(
                            source_type,
                            &index_type,
                            Some(access_flags),
                            Some(self.create_synthetic_expression(
                                element,
                                &index_type,
                                None,
                                Option::<&Node>::None,
                            )),
                            Option::<&Symbol>::None,
                            None,
                        )
                        .unwrap_or_else(|| self.error_type());
                    let assigned_type = if self.has_default_value(element) {
                        self.get_type_with_facts(&element_type, TypeFacts::NEUndefined)
                    } else {
                        element_type.clone()
                    };
                    let type_ = self.get_flow_type_of_destructuring(element, &assigned_type);
                    return Some(
                        self.check_destructuring_assignment(element, &type_, check_mode, None),
                    );
                }
                return Some(self.check_destructuring_assignment(
                    element,
                    element_type,
                    check_mode,
                    None,
                ));
            }
            if element_index < elements.len() - 1 {
                self.error(
                    Some(&**element),
                    &Diagnostics::A_rest_element_must_be_last_in_a_destructuring_pattern,
                    None,
                );
            } else {
                let rest_expression = &element.as_spread_element().expression;
                if rest_expression.kind() == SyntaxKind::BinaryExpression
                    && rest_expression.as_binary_expression().operator_token.kind()
                        == SyntaxKind::EqualsToken
                {
                    self.error(
                        Some(&*rest_expression.as_binary_expression().operator_token),
                        &Diagnostics::A_rest_element_cannot_have_an_initializer,
                        None,
                    );
                } else {
                    self.check_grammar_for_disallowed_trailing_comma(Some(&node_as_array_literal_expression.elements), Some(&Diagnostics::A_rest_parameter_or_binding_pattern_may_not_have_a_trailing_comma));
                    let type_ =
                        if self.every_type(source_type, |type_: &Type| self.is_tuple_type(type_)) {
                            self.map_type(
                                source_type,
                                &mut |t: &Type| Some(self.slice_tuple_type(t, element_index, None)),
                                None,
                            )
                            .unwrap()
                        } else {
                            self.create_array_type(element_type, None)
                        };
                    return Some(self.check_destructuring_assignment(
                        rest_expression,
                        &type_,
                        check_mode,
                        None,
                    ));
                }
            }
        }
        None
    }

    pub(super) fn check_destructuring_assignment(
        &self,
        expr_or_assignment: &Node, /*Expression | ShorthandPropertyAssignment*/
        source_type: &Type,
        check_mode: Option<CheckMode>,
        right_is_this: Option<bool>,
    ) -> Rc<Type> {
        let mut target: Rc<Node>;
        let mut source_type = source_type.type_wrapper();
        if expr_or_assignment.kind() == SyntaxKind::ShorthandPropertyAssignment {
            let prop = expr_or_assignment.as_shorthand_property_assignment();
            if let Some(prop_object_assignment_initializer) =
                prop.object_assignment_initializer.as_ref()
            {
                if self.strict_null_checks
                    && !self
                        .get_falsy_flags(&self.check_expression(
                            prop_object_assignment_initializer,
                            None,
                            None,
                        ))
                        .intersects(TypeFlags::Undefined)
                {
                    source_type = self.get_type_with_facts(&source_type, TypeFacts::NEUndefined);
                }
                self.check_binary_like_expression(
                    &prop.name(),
                    prop.equals_token.as_ref().unwrap(),
                    prop_object_assignment_initializer,
                    check_mode,
                    Option::<&Node>::None,
                );
            }
            target = expr_or_assignment.as_shorthand_property_assignment().name();
        } else {
            target = expr_or_assignment.node_wrapper();
        }

        if target.kind() == SyntaxKind::BinaryExpression
            && target.as_binary_expression().operator_token.kind() == SyntaxKind::EqualsToken
        {
            self.check_binary_expression().call(&target, check_mode);
            target = target.as_binary_expression().left.clone();
        }
        if target.kind() == SyntaxKind::ObjectLiteralExpression {
            return self.check_object_literal_assignment(&target, &source_type, right_is_this);
        }
        if target.kind() == SyntaxKind::ArrayLiteralExpression {
            return self.check_array_literal_assignment(&target, &source_type, check_mode);
        }
        self.check_reference_assignment(&target, &source_type, check_mode)
    }

    pub(super) fn check_reference_assignment(
        &self,
        target: &Node, /*Expression*/
        source_type: &Type,
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let target_type = self.check_expression(target, check_mode, None);
        let error = if target.parent().kind() == SyntaxKind::SpreadAssignment {
            &*Diagnostics::The_target_of_an_object_rest_assignment_must_be_a_variable_or_a_property_access
        } else {
            &*Diagnostics::The_left_hand_side_of_an_assignment_expression_must_be_a_variable_or_a_property_access
        };
        let optional_error = if target.parent().kind() == SyntaxKind::SpreadAssignment {
            &*Diagnostics::The_target_of_an_object_rest_assignment_may_not_be_an_optional_property_access
        } else {
            &*Diagnostics::The_left_hand_side_of_an_assignment_expression_may_not_be_an_optional_property_access
        };
        if self.check_reference_expression(target, error, optional_error) {
            self.check_type_assignable_to_and_optionally_elaborate(
                source_type,
                &target_type,
                Some(target),
                Some(target),
                None,
                None,
            );
        }
        if is_private_identifier_property_access_expression(target) {
            self.check_external_emit_helpers(
                &target.parent(),
                ExternalEmitHelpers::ClassPrivateFieldSet,
            );
        }
        source_type.type_wrapper()
    }

    pub(super) fn is_side_effect_free(&self, node: &Node) -> bool {
        let node = skip_parentheses(node, None);
        match node.kind() {
            SyntaxKind::Identifier
            | SyntaxKind::StringLiteral
            | SyntaxKind::RegularExpressionLiteral
            | SyntaxKind::TaggedTemplateExpression
            | SyntaxKind::TemplateExpression
            | SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::TrueKeyword
            | SyntaxKind::FalseKeyword
            | SyntaxKind::NullKeyword
            | SyntaxKind::UndefinedKeyword
            | SyntaxKind::FunctionExpression
            | SyntaxKind::ClassExpression
            | SyntaxKind::ArrowFunction
            | SyntaxKind::ArrayLiteralExpression
            | SyntaxKind::ObjectLiteralExpression
            | SyntaxKind::TypeOfExpression
            | SyntaxKind::NonNullExpression
            | SyntaxKind::JsxSelfClosingElement
            | SyntaxKind::JsxElement => true,

            SyntaxKind::ConditionalExpression => {
                let node_as_conditional_expression = node.as_conditional_expression();
                self.is_side_effect_free(&node_as_conditional_expression.when_true)
                    && self.is_side_effect_free(&node_as_conditional_expression.when_false)
            }

            SyntaxKind::BinaryExpression => {
                let node_as_binary_expression = node.as_binary_expression();
                if is_assignment_operator(node_as_binary_expression.operator_token.kind()) {
                    return false;
                }
                self.is_side_effect_free(&node_as_binary_expression.left)
                    && self.is_side_effect_free(&node_as_binary_expression.right)
            }

            SyntaxKind::PrefixUnaryExpression | SyntaxKind::PostfixUnaryExpression => {
                matches!(
                    node.as_unary_expression().operator(),
                    SyntaxKind::ExclamationToken
                        | SyntaxKind::PlusToken
                        | SyntaxKind::MinusToken
                        | SyntaxKind::TildeToken
                )
            }

            _ => false,
        }
    }

    pub(super) fn is_type_equality_comparable_to(&self, source: &Type, target: &Type) -> bool {
        target.flags().intersects(TypeFlags::Nullable) || self.is_type_comparable_to(source, target)
    }

    pub(super) fn create_check_binary_expression(&self) -> CheckBinaryExpression {
        // unimplemented!()
        CheckBinaryExpression
    }

    pub(super) fn check_binary_like_expression<TErrorNode: Borrow<Node>>(
        &self,
        left: &Node, /*Expression*/
        operator_token: &Node,
        right: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
        error_node: Option<TErrorNode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_template_expression(
        &self,
        node: &Node, /*TemplateExpression*/
    ) -> Rc<Type> {
        let node_as_template_expression = node.as_template_expression();
        let mut texts = vec![node_as_template_expression
            .head
            .as_literal_like_node()
            .text()];
        let mut types = vec![];
        for span in node_as_template_expression.template_spans.iter() {
            let span = span.as_template_span();
            let type_ = self.check_expression(&span.expression, None, None);
            texts.push(span.literal.as_literal_like_node().text());
            types.push(
                if self.is_type_assignable_to(&type_, &self.template_constraint_type()) {
                    type_
                } else {
                    self.string_type()
                },
            );
        }
        if false {
            unimplemented!()
        } else {
            self.string_type()
        }
    }

    pub(super) fn check_expression_with_contextual_type(
        &self,
        node: &Node, /*Expression*/
        contextual_type: &Type,
        inference_context: Option<&InferenceContext>,
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_expression_cached(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let links = self.get_node_links(node);
        let links_resolved_type_is_none = (*links).borrow().resolved_type.is_none();
        if links_resolved_type_is_none {
            if let Some(check_mode) = check_mode {
                if check_mode != CheckMode::Normal {
                    return self.check_expression(node, Some(check_mode), None);
                }
            }
            let resolved_type = self.check_expression(node, check_mode, None);
            links.borrow_mut().resolved_type = Some(resolved_type);
        }
        let resolved_type = (*links).borrow().resolved_type.clone().unwrap();
        resolved_type
    }

    pub(super) fn check_declaration_initializer<TType: Borrow<Type>>(
        &self,
        declaration: &Node, /*HasExpressionInitializer*/
        contextual_type: Option<TType>,
    ) -> Rc<Type> {
        let initializer = get_effective_initializer(declaration).unwrap();
        let type_ = self
            .get_quick_type_of_expression(&initializer)
            .unwrap_or_else(|| {
                if let Some(contextual_type) = contextual_type {
                    unimplemented!()
                } else {
                    self.check_expression_cached(&initializer, None)
                }
            });
        if false {
            unimplemented!()
        } else {
            type_
        }
    }

    pub(super) fn widen_type_inferred_from_initializer(
        &self,
        declaration: &Node, /*HasExpressionInitializer*/
        type_: &Type,
    ) -> Rc<Type> {
        let widened = if get_combined_node_flags(declaration).intersects(NodeFlags::Const) {
            type_.type_wrapper()
        } else {
            self.get_widened_literal_type(type_)
        };
        widened
    }

    pub(super) fn is_literal_of_contextual_type<TTypeRef: Borrow<Type>>(
        &self,
        candidate_type: &Type,
        contextual_type: Option<TTypeRef>,
    ) -> bool {
        if let Some(contextual_type) = contextual_type {
            let contextual_type = contextual_type.borrow();
            if let Type::UnionOrIntersectionType(union_or_intersection_type) = contextual_type {
                let types = union_or_intersection_type.types();
                // return some(
                //     types,
                //     Some(Box::new(|t| {
                //         self.is_literal_of_contextual_type(candidate_type, Some(t.clone()))
                //     })),
                // );
                return types
                    .iter()
                    .any(|t| self.is_literal_of_contextual_type(candidate_type, Some(&**t)));
            }
            return contextual_type.flags().intersects(
                TypeFlags::StringLiteral
                    | TypeFlags::Index
                    | TypeFlags::TemplateLiteral
                    | TypeFlags::StringMapping,
            ) && self.maybe_type_of_kind(&*candidate_type, TypeFlags::StringLiteral)
                || contextual_type.flags().intersects(TypeFlags::NumberLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::NumberLiteral)
                || contextual_type.flags().intersects(TypeFlags::BigIntLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::BigIntLiteral)
                || contextual_type
                    .flags()
                    .intersects(TypeFlags::BooleanLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::BooleanLiteral)
                || contextual_type
                    .flags()
                    .intersects(TypeFlags::UniqueESSymbol)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::UniqueESSymbol);
        }
        false
    }

    pub(super) fn is_const_context(&self, node: &Node /*Expression*/) -> bool {
        unimplemented!()
    }

    pub(super) fn check_expression_for_mutable_location<TContextualType: Borrow<Type>>(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
        contextual_type: Option<TContextualType>,
        force_tuple: Option<bool>,
    ) -> Rc<Type> {
        let type_ = self.check_expression(node, check_mode, force_tuple);
        if false {
            unimplemented!()
        } else {
            self.get_widened_literal_like_type_for_contextual_type(
                &type_,
                self.instantiate_contextual_type(
                    if contextual_type.is_none() {
                        self.get_contextual_type_(node, None)
                    } else {
                        Some(contextual_type.unwrap().borrow().type_wrapper())
                    },
                    node,
                    None,
                ),
            )
        }
    }

    pub(super) fn check_property_assignment(
        &self,
        node: &Node, /*PropertyAssignment*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.check_expression_for_mutable_location(
            &node.as_property_assignment().initializer,
            check_mode,
            Option::<&Type>::None,
            None,
        )
    }

    pub(super) fn check_object_literal_method(
        &self,
        node: &Node, /*MethodDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn skipped_generic_function(&self, node: &Node, check_mode: CheckMode) {
        unimplemented!()
    }

    pub(super) fn has_inference_candidates(&self, info: &InferenceInfo) -> bool {
        unimplemented!()
    }

    pub(super) fn get_type_of_expression(&self, node: &Node /*Expression*/) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_quick_type_of_expression(
        &self,
        node: &Node, /*Expression*/
    ) -> Option<Rc<Type>> {
        let expr = node;
        if false {
            unimplemented!()
        } else if matches!(
            node.kind(),
            SyntaxKind::NumericLiteral
                | SyntaxKind::StringLiteral
                | SyntaxKind::TrueKeyword
                | SyntaxKind::FalseKeyword
        ) {
            return Some(self.check_expression(node, None, None));
        }
        None
    }

    pub(super) fn get_context_free_type_of_expression(
        &self,
        node: &Node, /*Expression*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_expression(
        &self,
        node: &Node, /*Expression | QualifiedName*/
        check_mode: Option<CheckMode>,
        force_tuple: Option<bool>,
    ) -> Rc<Type> {
        self.check_expression_worker(node, check_mode)
    }

    pub(super) fn check_expression_worker(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        match node {
            Node::Identifier(_) => self.check_identifier(node, check_mode),
            Node::BaseNode(node_as_base_node) => match node_as_base_node.kind() {
                SyntaxKind::TrueKeyword => self.true_type(),
                _ => unimplemented!(),
            },
            Node::ObjectLiteralExpression(_) => self.check_object_literal(node, check_mode),
            Node::PrefixUnaryExpression(_) => self.check_prefix_unary_expression(node),
            // Node::Expression(Expression::BinaryExpression(_)) => {
            //     return self.check_binary_expression(node);
            // }
            Node::TemplateLiteralLikeNode(template_literal_like_node) => {
                let type_: Rc<Type> =
                    self.get_string_literal_type(&*template_literal_like_node.text());
                self.get_fresh_type_of_literal_type(&type_)
            }
            Node::StringLiteral(string_literal) => {
                let type_: Rc<Type> = self.get_string_literal_type(&*string_literal.text());
                self.get_fresh_type_of_literal_type(&type_)
            }
            Node::NumericLiteral(numeric_literal) => {
                self.check_grammar_numeric_literal(node);
                let type_: Rc<Type> =
                    self.get_number_literal_type(numeric_literal.text().as_str().into());
                self.get_fresh_type_of_literal_type(&type_)
            }
            Node::BigIntLiteral(big_int_literal) => {
                let type_: Rc<Type> = self
                    .get_big_int_literal_type(PseudoBigInt::new(
                        false,
                        parse_pseudo_big_int(&*big_int_literal.text()),
                    ))
                    .into();
                self.get_fresh_type_of_literal_type(&type_)
            }
            Node::TemplateExpression(_) => self.check_template_expression(node),
            _ => unimplemented!("{:#?}", node),
        }
    }

    pub(super) fn check_type_parameter(&self, node: &Node /*TypeParameterDeclaration*/) {
        // TODO
    }

    pub(super) fn check_signature_declaration(&self, node: &Node /*SignatureDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn check_property_declaration(&self, node: &Node /*PropertySignature*/) {
        self.check_variable_like_declaration(node);
    }

    pub(super) fn check_property_signature(&self, node: &Node /*PropertySignature*/) {
        if is_private_identifier(&*node.as_property_signature().name()) {
            self.error(
                Some(node.node_wrapper()),
                &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                None,
            );
        }
        self.check_property_declaration(node)
    }

    pub(super) fn get_effective_type_arguments(
        &self,
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
        type_parameters: &[Rc<Type /*TypeParameter*/>],
    ) -> Vec<Rc<Type>> {
        self.fill_missing_type_arguments(
            Some(map(
                node.as_has_type_arguments().maybe_type_arguments().unwrap(),
                |type_argument, _| self.get_type_from_type_node_(type_argument),
            )),
            Some(type_parameters),
            0, // TODO: this is wrong
            false,
        )
        .unwrap()
    }

    pub(super) fn get_type_parameters_for_type_reference(
        &self,
        node: &Node, /*TypeReferenceNode | ExpressionWithTypeArguments*/
    ) -> Option<Vec<Rc<Type>>> {
        unimplemented!()
    }

    pub(super) fn check_type_reference_node(&self, node: &Node /*TypeReferenceNode*/) {
        maybe_for_each(
            node.as_type_reference_node().type_arguments.as_ref(),
            |type_argument, _| {
                self.check_source_element(Some(&**type_argument));
                Option::<()>::None
            },
        );
        let type_ = self.get_type_from_type_reference(node);
    }

    pub(super) fn get_type_argument_constraint_(
        &self,
        node: &Node, /*TypeNode*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn check_array_type(&self, node: &Node /*ArrayTypeNode*/) {
        self.check_source_element(Some(&*node.as_array_type_node().element_type));
    }

    pub(super) fn check_union_or_intersection_type(
        &self,
        node: &Node, /*UnionOrIntersectionTypeNode*/
    ) {
        for_each(
            node.as_union_or_intersection_type_node().types(),
            |type_, _| {
                self.check_source_element(Some(&**type_));
                Option::<()>::None
            },
        );
        self.get_type_from_type_node_(node);
    }

    pub(super) fn check_indexed_access_index_type(
        &self,
        type_: &Type,
        access_node: &Node, /*IndexedAccessTypeNode | ElementAccessExpression*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_private_within_ambient(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn get_awaited_type_of_promise<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_promised_type_of_promise<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn check_awaited_type(
        &self,
        type_: &Type,
        with_alias: bool,
        error_node: &Node,
        diagnostic_message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_awaited_type_instantiation(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Conditional) {
            unimplemented!()
        }
        false
    }

    pub(super) fn unwrap_awaited_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_| Some(self.unwrap_awaited_type(type_)),
                None,
            )
            .unwrap()
        } else if self.is_awaited_type_instantiation(type_) {
            unimplemented!()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_awaited_type_<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_awaited_type_no_alias<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Option<Rc<Type>> {
        if self.is_type_any(Some(type_)) {
            return Some(type_.type_wrapper());
        }

        if self.is_awaited_type_instantiation(type_) {
            return Some(type_.type_wrapper());
        }

        unimplemented!()
    }

    pub(super) fn check_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        if self.produce_diagnostics {
            self.check_function_or_method_declaration(node);
        }
    }

    pub(super) fn check_function_or_method_declaration(
        &self,
        node: &Node, /*FunctionDeclaration | MethodDeclaration | MethodSignature*/
    ) {
        // self.check_decorators(node);
        // self.check_signature_declaration(node);
    }

    pub(super) fn check_unused_identifiers<
        TAddDiagnostic: FnMut(&Node, UnusedKind, Rc<Diagnostic>),
    >(
        &self,
        potentially_unused_identifiers: &[Rc<Node /*PotentiallyUnusedIdentifier*/>],
        add_diagnostic: TAddDiagnostic, /*AddUnusedDiagnostic*/
    ) {
        unimplemented!()
    }

    pub(super) fn check_block(&self, node: &Node /*Block*/) {
        let node_as_block = node.as_block();
        if is_function_or_module_block(node) {
            for_each(&node_as_block.statements, |statement, _| {
                self.check_source_element(Some(statement.clone()));
                Option::<()>::None
            });
        } else {
            for_each(&node_as_block.statements, |statement, _| {
                self.check_source_element(Some(statement.clone()));
                Option::<()>::None
            });
        }
    }

    pub(super) fn check_collisions_for_declaration_name<TName: Borrow<Node>>(
        &self,
        node: &Node,
        name: Option<TName /*Identifier*/>,
    ) {
        unimplemented!()
    }

    pub(super) fn convert_auto_to_any(&self, type_: &Type) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn check_variable_like_declaration(&self, node: &Node) {
        let node_as_variable_like_declaration = node.as_variable_like_declaration();
        if !is_binding_element(node) {
            self.check_source_element(node_as_variable_like_declaration.maybe_type());
        }

        let symbol = self.get_symbol_of_node(node).unwrap();

        let type_ = self.convert_auto_to_any(&self.get_type_of_symbol(&*symbol));
        let value_declaration = symbol.maybe_value_declaration();
        if value_declaration.is_some() && ptr::eq(node, &*value_declaration.unwrap()) {
            let initializer = get_effective_initializer(node);
            if let Some(initializer) = initializer {
                if true {
                    let initializer_type = self.check_expression_cached(&initializer, None);
                    self.check_type_assignable_to_and_optionally_elaborate(
                        &initializer_type,
                        &type_,
                        Some(node),
                        Some(&*initializer),
                        None,
                        None,
                    );
                }
            }
        } else {
            unimplemented!()
        }
    }

    pub(super) fn error_next_variable_or_property_declaration_must_have_same_type<
        TFirstDeclaration: Borrow<Node>,
    >(
        &self,
        first_declaration: Option<TFirstDeclaration /*Declaration*/>,
        first_type: &Type,
        next_declaration: &Node, /*Declaration*/
        next_type: &Type,
    ) {
        unimplemented!()
    }

    pub(super) fn check_variable_declaration(&self, node: &Node /*VariableDeclaration*/) {
        self.check_variable_like_declaration(node);
    }

    pub(super) fn check_variable_statement(&self, node: &Node /*VariableStatement*/) {
        for_each(
            &node
                .as_variable_statement()
                .declaration_list
                .as_variable_declaration_list()
                .declarations,
            |declaration, _| Some(self.check_source_element(Some(&**declaration))),
        );
    }

    pub(super) fn check_expression_statement(&self, node: &Node /*ExpressionStatement*/) {
        let expression = &node.as_expression_statement().expression;
        self.check_expression(expression, None, None);
    }

    pub(super) fn check_if_statement(&self, node: &Node /*IfStatement*/) {
        let node_as_if_statement = node.as_if_statement();
        let type_ = self.check_truthiness_expression(&node_as_if_statement.expression, None);
        self.check_source_element(Some(&*node_as_if_statement.then_statement));

        if node_as_if_statement.then_statement.kind() == SyntaxKind::EmptyStatement {
            self.error(
                Some(&*node_as_if_statement.then_statement),
                &Diagnostics::The_body_of_an_if_statement_cannot_be_the_empty_statement,
                None,
            );
        }

        self.check_source_element(node_as_if_statement.else_statement.clone());
    }

    pub(super) fn check_truthiness_of_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Void) {
            self.error(
                Some(node),
                &Diagnostics::An_expression_of_type_void_cannot_be_tested_for_truthiness,
                None,
            );
        }

        type_.type_wrapper()
    }

    pub(super) fn check_truthiness_expression(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.check_truthiness_of_type(&self.check_expression(node, check_mode, None), node)
    }

    pub(super) fn check_right_hand_side_of_for_of(
        &self,
        statement: &Node, /*ForOfStatement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_iterated_type_or_element_type<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        input_type: &Type,
        sent_type: &Type,
        error_node: Option<TErrorNode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_iterated_type_or_element_type<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        input_type: &Type,
        sent_type: &Type,
        error_node: Option<TErrorNode>,
        check_assignability: bool,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn create_iteration_types(
        &self,
        yield_type: Option<Rc<Type>>,
        return_type: Option<Rc<Type>>,
        next_type: Option<Rc<Type>>,
    ) -> IterationTypes {
        let yield_type = yield_type.unwrap_or_else(|| self.never_type());
        let return_type = return_type.unwrap_or_else(|| self.never_type());
        let next_type = next_type.unwrap_or_else(|| self.unknown_type());
        IterationTypes::new(yield_type, return_type, next_type)
    }

    pub(super) fn get_iteration_types_of_iterable<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        use_: IterationUse,
        error_node: Option<TErrorNode>,
    ) -> Option<IterationTypes> {
        unimplemented!()
    }

    pub(super) fn get_iteration_types_of_global_iterable_type(
        &self,
        global_type: &Type,
        resolver: &IterationTypesResolver,
    ) -> IterationTypes {
        unimplemented!()
    }

    pub(super) fn get_iteration_type_of_generator_function_return_type(
        &self,
        kind: IterationTypeKind,
        return_type: &Type,
        is_async_generator: bool,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn unwrap_return_type(
        &self,
        return_type: &Type,
        function_flags: FunctionFlags,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_unwrapped_return_type_void_or_any(
        &self,
        func: &Node, /*SignatureDeclaration*/
        return_type: &Type,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_return_statement(&self, node: &Node /*ReturnStatement*/) {
        let container = get_containing_function_or_class_static_block(node);

        if container.is_none() {
            unimplemented!()
        }
        let container = container.unwrap();

        let signature = self.get_signature_from_declaration_(&container);
        let return_type = self.get_return_type_of_signature(signature);
        let function_flags = get_function_flags(Some(&*container));
        let node_as_return_statement = node.as_return_statement();
        if self.strict_null_checks
            || node_as_return_statement.expression.is_some()
            || return_type.flags().intersects(TypeFlags::Never)
        {
            let expr_type = match node_as_return_statement.expression.as_ref() {
                Some(expression) => self.check_expression_cached(&expression, None),
                None => self.undefined_type(),
            };
            if false {
                unimplemented!()
            } else if self.get_return_type_from_annotation(&container).is_some() {
                let unwrapped_return_type = self
                    .unwrap_return_type(&return_type, function_flags)/*.unwrap_or(return_type)*/;
                let unwrapped_expr_type = if function_flags.intersects(FunctionFlags::Async) {
                    self.check_awaited_type(&expr_type, false, node, &Diagnostics::The_return_type_of_an_async_function_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member, None)
                } else {
                    expr_type
                };
                // if unwrappedReturnType {
                self.check_type_assignable_to_and_optionally_elaborate(
                    &unwrapped_expr_type,
                    &unwrapped_return_type,
                    Some(node),
                    node_as_return_statement.expression.clone(),
                    None,
                    None,
                );
                // }
            }
        }
    }

    pub(super) fn check_type_parameters(
        &self,
        type_parameter_declarations: Option<&NodeArray /*<TypeParameterDeclaration>*/>,
    ) {
        if let Some(type_parameter_declarations) = type_parameter_declarations {
            for node in type_parameter_declarations {
                self.check_type_parameter(&node);
            }
        }
    }

    pub(super) fn get_target_symbol(&self, s: &Symbol) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn is_property_without_initializer(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_property_initialized_in_static_blocks(
        &self,
        prop_name: &Node, /*Identifier | PrivateIdentifier*/
        prop_type: &Type,
        static_blocks: &[Rc<Node /*ClassStaticBlockDeclaration*/>],
        start_pos: isize,
        end_pos: isize,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_interface_declaration(&self, node: &Node /*InterfaceDeclaration*/) {
        let node_as_interface_declaration = node.as_interface_declaration();
        self.check_type_parameters(
            node_as_interface_declaration
                .maybe_type_parameters()
                .as_ref(),
        );
        for_each(&node_as_interface_declaration.members, |member, _| {
            self.check_source_element(Some(&**member));
            Option::<()>::None
        });
    }

    pub(super) fn check_type_alias_declaration(&self, node: &Node /*TypeAliasDeclaration*/) {
        let node_as_type_alias_declaration = node.as_type_alias_declaration();
        self.check_type_parameters(
            node_as_type_alias_declaration
                .maybe_type_parameters()
                .as_ref(),
        );
        if false {
            unimplemented!()
        } else {
            self.check_source_element(Some(&*node_as_type_alias_declaration.type_));
        }
    }
}

#[derive(Debug)]
pub struct CheckBinaryExpression;

impl CheckBinaryExpression {
    pub fn call(
        &self,
        node: &Node, /*BinaryExpression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        unimplemented!()
    }
}
