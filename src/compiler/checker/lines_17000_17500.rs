#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::rc::Rc;

use super::{
    signature_has_rest_parameter, CheckMode, SignatureCheckMode, TypeComparerCompareTypesAssignable,
};
use crate::{
    get_source_file_of_node, id_text, is_jsx_spread_attribute, unescape_leading_underscores,
    HasInitializerInterface, SignatureDeclarationInterface, SymbolFlags, SymbolInterface, Ternary,
    TypeComparer, __String, add_related_info, are_rc_slices_equal, create_diagnostic_for_node,
    format_message, get_function_flags, get_semantic_jsx_children, get_text_of_node, has_type,
    is_block, is_computed_non_literal_name, is_identifier_type_predicate, is_jsx_element,
    is_jsx_opening_element, is_omitted_expression, is_spread_assignment, length, map, some, Debug_,
    Diagnostic, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, FunctionFlags,
    FunctionLikeDeclarationInterface, NamedDeclarationInterface, Node, NodeInterface, Number,
    RelationComparisonResult, Signature, SignatureKind, Symbol, SyntaxKind, Type, TypeChecker,
    TypeFlags, TypeInterface, TypeMapper, UnionOrIntersectionTypeInterface,
};

impl TypeChecker {
    pub(super) fn check_type_assignable_to_and_optionally_elaborate<
        TErrorNode: Borrow<Node>,
        TExpr: Borrow<Node>,
    >(
        &self,
        source: &Type,
        target: &Type,
        error_node: Option<TErrorNode>,
        expr: Option<TExpr>,
        head_message: Option<&'static DiagnosticMessage>,
        containing_message_chain: Option<Rc<dyn CheckTypeContainingMessageChain>>,
    ) -> bool {
        self.check_type_related_to_and_optionally_elaborate(
            source,
            target,
            self.assignable_relation.clone(),
            error_node,
            expr,
            head_message,
            containing_message_chain,
            None,
        )
    }

    pub(super) fn check_type_related_to_and_optionally_elaborate<
        TErrorNode: Borrow<Node>,
        TExpr: Borrow<Node>,
    >(
        &self,
        source: &Type,
        target: &Type,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        error_node: Option<TErrorNode>,
        expr: Option<TExpr>,
        head_message: Option<&'static DiagnosticMessage>,
        containing_message_chain: Option<Rc<dyn CheckTypeContainingMessageChain>>,
        error_output_container: Option<Rc<dyn CheckTypeErrorOutputContainer>>,
    ) -> bool {
        if self.is_type_related_to(source, target, relation.clone()) {
            return true;
        }
        if error_node.is_none()
            || !self.elaborate_error(
                expr,
                source,
                target,
                relation.clone(),
                head_message,
                containing_message_chain.clone(),
                error_output_container.clone(),
            )
        {
            return self.check_type_related_to(
                source,
                target,
                relation,
                error_node,
                head_message.map(Cow::Borrowed),
                containing_message_chain,
                error_output_container,
            );
        }
        false
    }

    pub(super) fn is_or_has_generic_conditional(&self, type_: &Type) -> bool {
        type_.flags().intersects(TypeFlags::Conditional)
            || type_.flags().intersects(TypeFlags::Intersection)
                && some(
                    Some(type_.as_intersection_type().types()),
                    Some(|type_: &Rc<Type>| self.is_or_has_generic_conditional(type_)),
                )
    }

    pub(super) fn elaborate_error<TNode: Borrow<Node>>(
        &self,
        node: Option<TNode>,
        source: &Type,
        target: &Type,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        head_message: Option<&'static DiagnosticMessage>,
        containing_message_chain: Option<Rc<dyn CheckTypeContainingMessageChain>>,
        error_output_container: Option<Rc<dyn CheckTypeErrorOutputContainer>>,
    ) -> bool {
        if node.is_none() || self.is_or_has_generic_conditional(target) {
            return false;
        }
        let node = node.unwrap();
        let node = node.borrow();
        if !self.check_type_related_to(
            source,
            target,
            relation.clone(),
            Option::<&Node>::None,
            None,
            None,
            None,
        ) && self.elaborate_did_you_mean_to_call_or_construct(
            node,
            source,
            target,
            relation.clone(),
            head_message,
            containing_message_chain.clone(),
            error_output_container.clone(),
        ) {
            return true;
        }
        match node.kind() {
            SyntaxKind::JsxExpression | SyntaxKind::ParenthesizedExpression => {
                return self.elaborate_error(
                    node.as_has_expression().maybe_expression(),
                    source,
                    target,
                    relation,
                    head_message,
                    containing_message_chain,
                    error_output_container,
                );
            }
            SyntaxKind::BinaryExpression => {
                let node_as_binary_expression = node.as_binary_expression();
                match node_as_binary_expression.operator_token.kind() {
                    SyntaxKind::EqualsToken | SyntaxKind::CommaToken => {
                        return self.elaborate_error(
                            Some(&*node_as_binary_expression.right),
                            source,
                            target,
                            relation,
                            head_message,
                            containing_message_chain,
                            error_output_container,
                        );
                    }
                    _ => (),
                }
            }
            SyntaxKind::ObjectLiteralExpression => {
                return self.elaborate_object_literal(
                    node,
                    source,
                    target,
                    relation,
                    containing_message_chain,
                    error_output_container,
                );
            }
            SyntaxKind::ArrayLiteralExpression => {
                return self.elaborate_array_literal(
                    node,
                    source,
                    target,
                    relation,
                    containing_message_chain,
                    error_output_container,
                );
            }
            SyntaxKind::JsxAttributes => {
                return self.elaborate_jsx_components(
                    node,
                    source,
                    target,
                    relation,
                    containing_message_chain,
                    error_output_container,
                );
            }
            SyntaxKind::ArrowFunction => {
                return self.elaborate_arrow_function(
                    node,
                    source,
                    target,
                    relation,
                    containing_message_chain,
                    error_output_container,
                );
            }
            _ => (),
        }
        false
    }

    pub(super) fn elaborate_did_you_mean_to_call_or_construct(
        &self,
        node: &Node, /*Expression*/
        source: &Type,
        target: &Type,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        head_message: Option<&'static DiagnosticMessage>,
        containing_message_chain: Option<Rc<dyn CheckTypeContainingMessageChain>>,
        error_output_container: Option<Rc<dyn CheckTypeErrorOutputContainer>>,
    ) -> bool {
        let call_signatures = self.get_signatures_of_type(source, SignatureKind::Call);
        let construct_signatures = self.get_signatures_of_type(source, SignatureKind::Construct);
        for (signatures_index, signatures) in vec![call_signatures, construct_signatures]
            .into_iter()
            .enumerate()
        {
            if some(
                Some(&signatures),
                Some(|s: &Rc<Signature>| {
                    let return_type = self.get_return_type_of_signature(s.clone());
                    !return_type
                        .flags()
                        .intersects(TypeFlags::Any | TypeFlags::Never)
                        && self.check_type_related_to(
                            &return_type,
                            target,
                            relation.clone(),
                            Option::<&Node>::None,
                            None,
                            None,
                            None,
                        )
                }),
            ) {
                let result_obj = error_output_container
                    .unwrap_or_else(|| Rc::new(CheckTypeErrorOutputContainerConcrete::new(None)));
                self.check_type_assignable_to(
                    source,
                    target,
                    Some(node),
                    head_message,
                    containing_message_chain.clone(),
                    Some(result_obj.clone()),
                );
                let diagnostic = result_obj.get_error(result_obj.errors_len() - 1).unwrap();
                add_related_info(
                    &diagnostic,
                    vec![Rc::new(
                        create_diagnostic_for_node(
                            node,
                            if
                            /*signatures === constructSignatures*/
                            signatures_index == 1 {
                                &Diagnostics::Did_you_mean_to_use_new_with_this_expression
                            } else {
                                &Diagnostics::Did_you_mean_to_call_this_expression
                            },
                            None,
                        )
                        .into(),
                    )],
                );
                return true;
            }
        }
        false
    }

    pub(super) fn elaborate_arrow_function(
        &self,
        node: &Node, /*ArrowFunction*/
        source: &Type,
        target: &Type,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        containing_message_chain: Option<Rc<dyn CheckTypeContainingMessageChain>>,
        error_output_container: Option<Rc<dyn CheckTypeErrorOutputContainer>>,
    ) -> bool {
        let node_as_arrow_function = node.as_arrow_function();
        if is_block(&node_as_arrow_function.maybe_body().unwrap()) {
            return false;
        }
        if some(
            Some(node_as_arrow_function.parameters()),
            Some(|parameter: &Rc<Node>| has_type(parameter)),
        ) {
            return false;
        }
        let source_sig = self.get_single_call_signature(source);
        if source_sig.is_none() {
            return false;
        }
        let source_sig = source_sig.unwrap();
        let target_signatures = self.get_signatures_of_type(target, SignatureKind::Call);
        if length(Some(&target_signatures)) == 0 {
            return false;
        }
        let return_expression = node_as_arrow_function.maybe_body().unwrap();
        let source_return = self.get_return_type_of_signature(source_sig);
        let target_return = self.get_union_type(
            map(&target_signatures, |signature: &Rc<Signature>, _| {
                self.get_return_type_of_signature(signature.clone())
            }),
            None,
            Option::<&Symbol>::None,
            None,
            Option::<&Type>::None,
        );
        if !self.check_type_related_to(
            &source_return,
            &target_return,
            relation.clone(),
            Option::<&Node>::None,
            None,
            None,
            None,
        ) {
            let elaborated = /*returnExpression &&*/ self.elaborate_error(
                Some(&*return_expression),
                &source_return,
                &target_return,
                relation.clone(),
                None,
                containing_message_chain.clone(),
                error_output_container.clone(),
            );
            if elaborated {
                return elaborated;
            }
            let result_obj = error_output_container
                .unwrap_or_else(|| Rc::new(CheckTypeErrorOutputContainerConcrete::new(None)));
            self.check_type_related_to(
                &source_return,
                &target_return,
                relation.clone(),
                Some(&*return_expression),
                None,
                containing_message_chain,
                Some(result_obj.clone()),
            );
            if result_obj.errors_len() > 0 {
                if let Some(target_symbol) = target.maybe_symbol() {
                    let target_symbol_declarations = target_symbol.maybe_declarations();
                    if let Some(target_symbol_declarations) = target_symbol_declarations
                        .as_ref()
                        .filter(|target_symbol_declarations| !target_symbol_declarations.is_empty())
                    {
                        add_related_info(
                            &result_obj.get_error(result_obj.errors_len() - 1).unwrap(),
                            vec![
                                Rc::new(
                                    create_diagnostic_for_node(
                                        &target_symbol_declarations[0],
                                        &Diagnostics::The_expected_type_comes_from_the_return_type_of_this_signature,
                                        None,
                                    ).into()
                                )
                            ]
                        );
                    }
                }
                if !get_function_flags(Some(node)).intersects(FunctionFlags::Async)
                    && self
                        .get_type_of_property_of_type_(
                            &source_return,
                            &__String::new("then".to_owned()),
                        )
                        .is_none()
                    && self.check_type_related_to(
                        &self.create_promise_type(&source_return),
                        &target_return,
                        relation,
                        Option::<&Node>::None,
                        None,
                        None,
                        None,
                    )
                {
                    add_related_info(
                        &result_obj.get_error(result_obj.errors_len() - 1).unwrap(),
                        vec![Rc::new(
                            create_diagnostic_for_node(
                                node,
                                &Diagnostics::Did_you_mean_to_mark_this_function_as_async,
                                None,
                            )
                            .into(),
                        )],
                    );
                }
                return true;
            }
        }
        false
    }

    pub(super) fn get_best_match_indexed_access_type_or_undefined(
        &self,
        source: &Type,
        target: &Type,
        name_type: &Type,
    ) -> Option<Rc<Type>> {
        let idx = self.get_indexed_access_type_or_undefined(
            target,
            name_type,
            None,
            Option::<&Node>::None,
            Option::<&Symbol>::None,
            None,
        );
        if idx.is_some() {
            return idx;
        }
        if target.flags().intersects(TypeFlags::Union) {
            let best = self.get_best_matching_type(
                source,
                target,
                Option::<fn(&Type, &Type) -> Ternary>::None,
            );
            if let Some(best) = best.as_ref() {
                return self.get_indexed_access_type_or_undefined(
                    best,
                    name_type,
                    None,
                    Option::<&Node>::None,
                    Option::<&Symbol>::None,
                    None,
                );
            }
        }
        None
    }

    pub(super) fn check_expression_for_mutable_location_with_contextual_type(
        &self,
        next: &Node, /*Expression*/
        source_prop_type: &Type,
    ) -> Rc<Type> {
        *next.maybe_contextual_type() = Some(source_prop_type.type_wrapper());
        let ret = self.check_expression_for_mutable_location(
            next,
            Some(CheckMode::Contextual),
            Some(source_prop_type),
            None,
        );
        *next.maybe_contextual_type() = None;
        ret
    }

    pub(super) fn elaborate_elementwise(
        &self,
        iterator: Vec<ElaborationIteratorItem>,
        source: &Type,
        target: &Type,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        containing_message_chain: Option<Rc<dyn CheckTypeContainingMessageChain>>,
        error_output_container: Option<Rc<dyn CheckTypeErrorOutputContainer>>,
    ) -> bool {
        let mut reported_error = false;
        for status in iterator {
            let ElaborationIteratorItem {
                error_node: prop,
                inner_expression: next,
                name_type,
                error_message,
            } = status;
            let target_prop_type =
                self.get_best_match_indexed_access_type_or_undefined(source, target, &name_type);
            if target_prop_type.is_none() {
                continue;
            }
            let mut target_prop_type = target_prop_type.unwrap();
            if target_prop_type
                .flags()
                .intersects(TypeFlags::IndexedAccess)
            {
                continue;
            }
            let source_prop_type = self.get_indexed_access_type_or_undefined(
                source,
                &name_type,
                None,
                Option::<&Node>::None,
                Option::<&Symbol>::None,
                None,
            );
            if source_prop_type.is_none() {
                continue;
            }
            let mut source_prop_type = source_prop_type.unwrap();
            let prop_name = self.get_property_name_from_index(&name_type, Option::<&Node>::None);
            if !self.check_type_related_to(
                &source_prop_type,
                &target_prop_type,
                relation.clone(),
                Option::<&Node>::None,
                None,
                None,
                None,
            ) {
                let elaborated = match next.as_ref() {
                    None => false,
                    Some(next) => self.elaborate_error(
                        Some(&**next),
                        &source_prop_type,
                        &target_prop_type,
                        relation.clone(),
                        None,
                        containing_message_chain.clone(),
                        error_output_container.clone(),
                    ),
                };
                reported_error = true;
                if !elaborated {
                    let result_obj_default: Rc<dyn CheckTypeErrorOutputContainer> =
                        Rc::new(CheckTypeErrorOutputContainerConcrete::new(None));
                    let result_obj: Rc<dyn CheckTypeErrorOutputContainer> =
                        error_output_container.clone().unwrap_or(result_obj_default);
                    let specific_source = if let Some(next) = next.as_ref() {
                        self.check_expression_for_mutable_location_with_contextual_type(
                            next,
                            &source_prop_type,
                        )
                    } else {
                        source_prop_type.clone()
                    };
                    if matches!(self.exact_optional_property_types, Some(true))
                        && self.is_exact_optional_property_mismatch(
                            Some(&*specific_source),
                            Some(&*target_prop_type),
                        )
                    {
                        let diag: Rc<Diagnostic> = Rc::new(
                            create_diagnostic_for_node(
                                &prop,
                                &Diagnostics::Type_0_is_not_assignable_to_type_1_with_exactOptionalPropertyTypes_Colon_true_Consider_adding_undefined_to_the_type_of_the_target,
                                Some(vec![
                                    self.type_to_string_(
                                        &specific_source,
                                        Option::<&Node>::None,
                                        None,
                                        None,
                                    ),
                                    self.type_to_string_(
                                        &target_prop_type,
                                        Option::<&Node>::None,
                                        None,
                                        None,
                                    ),
                                ])
                            ).into()
                        );
                        self.diagnostics().add(diag.clone());
                        result_obj.set_errors(vec![diag]);
                    } else {
                        let target_is_optional = matches!(
                            prop_name.as_ref(),
                            Some(prop_name) if self.get_property_of_type_(target, prop_name, None).unwrap_or_else(|| self.unknown_symbol()).flags().intersects(SymbolFlags::Optional)
                        );
                        let source_is_optional = matches!(
                            prop_name.as_ref(),
                            Some(prop_name) if self.get_property_of_type_(source, prop_name, None).unwrap_or_else(|| self.unknown_symbol()).flags().intersects(SymbolFlags::Optional)
                        );
                        target_prop_type =
                            self.remove_missing_type(&target_prop_type, target_is_optional);
                        source_prop_type = self.remove_missing_type(
                            &source_prop_type,
                            target_is_optional && source_is_optional,
                        );
                        let result = self.check_type_related_to(
                            &specific_source,
                            &target_prop_type,
                            relation.clone(),
                            Some(&*prop),
                            error_message.clone(),
                            containing_message_chain.clone(),
                            Some(result_obj.clone()),
                        );
                        if result && !Rc::ptr_eq(&specific_source, &source_prop_type) {
                            self.check_type_related_to(
                                &source_prop_type,
                                &target_prop_type,
                                relation.clone(),
                                Some(&*prop),
                                error_message,
                                containing_message_chain.clone(),
                                Some(result_obj.clone()),
                            );
                        }
                    }
                    if result_obj.errors_len() > 0 {
                        let reported_diag =
                            result_obj.get_error(result_obj.errors_len() - 1).unwrap();
                        let property_name = if self.is_type_usable_as_property_name(&name_type) {
                            Some(self.get_property_name_from_type(&name_type))
                        } else {
                            None
                        };
                        let target_prop = property_name.as_ref().and_then(|property_name| {
                            self.get_property_of_type_(target, property_name, None)
                        });

                        let mut issued_elaboration = false;
                        if target_prop.is_none() {
                            let index_info = self.get_applicable_index_info(target, &name_type);
                            if let Some(index_info) = index_info.as_ref() {
                                if let Some(index_info_declaration) =
                                    index_info.declaration.as_ref().filter(|declaration| {
                                        !get_source_file_of_node(Some(&***declaration))
                                            .unwrap()
                                            .as_source_file()
                                            .has_no_default_lib()
                                    })
                                {
                                    issued_elaboration = true;
                                    add_related_info(
                                        &reported_diag,
                                        vec![
                                            Rc::new(
                                                create_diagnostic_for_node(
                                                    index_info_declaration,
                                                    &Diagnostics::The_expected_type_comes_from_this_index_signature,
                                                    None,
                                                ).into()
                                            )
                                        ]
                                    );
                                }
                            }
                        }

                        if !issued_elaboration
                            && (matches!(
                                target_prop.as_ref(),
                                Some(target_prop) if length(target_prop.maybe_declarations().as_deref()) > 0
                            ) || matches!(
                                target.maybe_symbol(),
                                Some(target_symbol) if length(target_symbol.maybe_declarations().as_deref()) > 0
                            ))
                        {
                            let target_node = if let Some(target_prop) =
                                target_prop.as_ref().filter(|target_prop| {
                                    length(target_prop.maybe_declarations().as_deref()) > 0
                                }) {
                                target_prop.maybe_declarations().as_ref().unwrap()[0].clone()
                            } else {
                                target.symbol().maybe_declarations().as_ref().unwrap()[0].clone()
                            };
                            if !get_source_file_of_node(Some(&*target_node))
                                .unwrap()
                                .as_source_file()
                                .has_no_default_lib()
                            {
                                add_related_info(
                                    &reported_diag,
                                    vec![
                                        Rc::new(
                                            create_diagnostic_for_node(
                                                &target_node,
                                                &Diagnostics::The_expected_type_comes_from_property_0_which_is_declared_here_on_type_1,
                                                Some(vec![
                                                    if property_name.is_some() && !name_type.flags().intersects(TypeFlags::UniqueESSymbol) {
                                                        unescape_leading_underscores(property_name.as_ref().unwrap())
                                                    } else {
                                                        self.type_to_string_(
                                                            &name_type,
                                                            Option::<&Node>::None,
                                                            None,
                                                            None,
                                                        )
                                                    },
                                                    self.type_to_string_(
                                                        target,
                                                        Option::<&Node>::None,
                                                        None,
                                                        None,
                                                    )
                                                ])
                                            ).into()
                                        )
                                    ]
                                );
                            }
                        }
                    }
                }
            }
        }
        reported_error
    }

    pub(super) fn generate_jsx_attributes(
        &self,
        node: &Node, /*JsxAttributes*/
    ) -> Vec<ElaborationIteratorItem> {
        let node_as_jsx_attributes = node.as_jsx_attributes();
        if length(Some(&node_as_jsx_attributes.properties)) == 0 {
            return vec![];
        }
        node_as_jsx_attributes
            .properties
            .iter()
            .flat_map(|prop| {
                if is_jsx_spread_attribute(prop)
                    || self.is_hyphenated_jsx_name(&id_text(&prop.as_jsx_attribute().name))
                {
                    return vec![];
                }
                let prop_as_jsx_attribute = prop.as_jsx_attribute();
                vec![ElaborationIteratorItem {
                    error_node: prop_as_jsx_attribute.name.clone(),
                    inner_expression: prop_as_jsx_attribute.initializer.clone(),
                    name_type: self.get_string_literal_type(&id_text(&prop_as_jsx_attribute.name)),
                    error_message: None,
                }]
            })
            .collect()
    }

    pub(super) fn generate_jsx_children<
        TGetInvalidTextDiagnostic: FnMut() -> Cow<'static, DiagnosticMessage>,
    >(
        &self,
        node: &Node, /*JsxElement*/
        mut get_invalid_text_diagnostic: TGetInvalidTextDiagnostic,
    ) -> Vec<ElaborationIteratorItem> {
        let node_as_jsx_element = node.as_jsx_element();
        if length(Some(&node_as_jsx_element.children)) == 0 {
            return vec![];
        }
        let mut member_offset = 0;
        node_as_jsx_element
            .children
            .iter()
            .enumerate()
            .flat_map(|(i, child)| {
                let name_type =
                    self.get_number_literal_type(Number::new((i - member_offset) as f64));
                let elem = self.get_elaboration_element_for_jsx_child(
                    child,
                    &name_type,
                    &mut get_invalid_text_diagnostic,
                );
                if let Some(elem) = elem {
                    vec![elem]
                } else {
                    member_offset += 1;
                    vec![]
                }
            })
            .collect()
    }

    pub(super) fn get_elaboration_element_for_jsx_child<
        TGetInvalidTextDiagnostic: FnMut() -> Cow<'static, DiagnosticMessage>,
    >(
        &self,
        child: &Node,     /*JsxChild*/
        name_type: &Type, /*LiteralType*/
        get_invalid_text_diagnostic: &mut TGetInvalidTextDiagnostic,
    ) -> Option<ElaborationIteratorItem> {
        match child.kind() {
            SyntaxKind::JsxExpression => {
                return Some(ElaborationIteratorItem {
                    error_node: child.node_wrapper(),
                    inner_expression: child.as_jsx_expression().expression.clone(),
                    name_type: name_type.type_wrapper(),
                    error_message: None,
                });
            }
            SyntaxKind::JsxText => {
                if child.as_jsx_text().contains_only_trivia_white_spaces {
                } else {
                    return Some(ElaborationIteratorItem {
                        error_node: child.node_wrapper(),
                        inner_expression: None,
                        name_type: name_type.type_wrapper(),
                        error_message: Some(get_invalid_text_diagnostic()),
                    });
                }
            }
            SyntaxKind::JsxElement
            | SyntaxKind::JsxSelfClosingElement
            | SyntaxKind::JsxFragment => {
                return Some(ElaborationIteratorItem {
                    error_node: child.node_wrapper(),
                    inner_expression: Some(child.node_wrapper()),
                    name_type: name_type.type_wrapper(),
                    error_message: None,
                });
            }
            _ => {
                Debug_.assert_never(child, Some("Found invalid jsx child"));
            }
        }
        None
    }

    pub(super) fn elaborate_jsx_components(
        &self,
        node: &Node, /*JsxAttributes*/
        source: &Type,
        target: &Type,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        containing_message_chain: Option<Rc<dyn CheckTypeContainingMessageChain>>,
        error_output_container: Option<Rc<dyn CheckTypeErrorOutputContainer>>,
    ) -> bool {
        let mut result = self.elaborate_elementwise(
            self.generate_jsx_attributes(node),
            source,
            target,
            relation.clone(),
            containing_message_chain.clone(),
            error_output_container.clone(),
        );
        let mut invalid_text_diagnostic: Option<Cow<'static, DiagnosticMessage>> = None;
        if is_jsx_opening_element(&node.parent()) && is_jsx_element(&node.parent().parent()) {
            let containing_element = node.parent().parent();
            let child_prop_name =
                self.get_jsx_element_children_property_name(self.get_jsx_namespace_at(Some(node)));
            let children_prop_name = child_prop_name.map_or_else(
                || "children".to_owned(),
                |child_prop_name| unescape_leading_underscores(&child_prop_name),
            );
            let children_name_type = self.get_string_literal_type(&children_prop_name);
            let children_target_type = self.get_indexed_access_type(
                target,
                &children_name_type,
                None,
                Option::<&Node>::None,
                Option::<&Symbol>::None,
                None,
            );
            let valid_children =
                get_semantic_jsx_children(&containing_element.as_jsx_element().children);
            if length(Some(&valid_children)) == 0 {
                return result;
            }
            let more_than_one_real_children = length(Some(&valid_children)) > 1;
            let array_like_target_parts = self.filter_type(&children_target_type, |type_| {
                self.is_array_or_tuple_like_type(type_)
            });
            let non_array_like_target_parts = self.filter_type(&children_target_type, |type_| {
                !self.is_array_or_tuple_like_type(type_)
            });
            let mut get_invalid_textual_child_diagnostic = || -> Cow<'static, DiagnosticMessage> {
                if invalid_text_diagnostic.is_none() {
                    let tag_name_text = get_text_of_node(
                        &node.parent().as_jsx_opening_like_element().tag_name(),
                        None,
                    );
                    let child_prop_name = self.get_jsx_element_children_property_name(
                        self.get_jsx_namespace_at(Some(node)),
                    );
                    let children_prop_name = child_prop_name.map_or_else(
                        || "children".to_owned(),
                        |child_prop_name| unescape_leading_underscores(&child_prop_name),
                    );
                    let children_target_type = self.get_indexed_access_type(
                        target,
                        &self.get_string_literal_type(&children_prop_name),
                        None,
                        Option::<&Node>::None,
                        Option::<&Symbol>::None,
                        None,
                    );
                    let diagnostic = &Diagnostics::_0_components_don_t_accept_text_as_child_elements_Text_in_JSX_has_the_type_string_but_the_expected_type_of_1_is_2;
                    invalid_text_diagnostic = Some(Cow::Owned(DiagnosticMessage {
                        code: diagnostic.code,
                        category: diagnostic.category,
                        key: "!!ALREADY FORMATTED!!",
                        message: format_message(
                            None,
                            diagnostic,
                            Some(vec![
                                tag_name_text.into_owned(),
                                children_prop_name,
                                self.type_to_string_(
                                    &children_target_type,
                                    Option::<&Node>::None,
                                    None,
                                    None,
                                ),
                            ]),
                        )
                        .into(),
                        elided_in_compatability_pyramid: diagnostic
                            .elided_in_compatability_pyramid
                            .clone(),
                    }));
                }
                invalid_text_diagnostic.clone().unwrap()
            };
            if more_than_one_real_children {
                if !Rc::ptr_eq(&array_like_target_parts, &self.never_type()) {
                    let real_source = self.create_tuple_type(
                        &self.check_jsx_children(&containing_element, Some(CheckMode::Normal)),
                        None,
                        None,
                        None,
                    );
                    let children = self.generate_jsx_children(
                        &containing_element,
                        get_invalid_textual_child_diagnostic,
                    );
                    result = self.elaborate_elementwise(
                        children,
                        &real_source,
                        &array_like_target_parts,
                        relation.clone(),
                        containing_message_chain,
                        error_output_container,
                    ) || result;
                } else if !self.is_type_related_to(
                    &self.get_indexed_access_type(
                        source,
                        &children_name_type,
                        None,
                        Option::<&Node>::None,
                        Option::<&Symbol>::None,
                        None,
                    ),
                    &children_target_type,
                    relation,
                ) {
                    result = true;
                    let diag = self.error(
                        Some(&*containing_element.as_jsx_element().opening_element.as_jsx_opening_element().tag_name),
                        &Diagnostics::This_JSX_tag_s_0_prop_expects_a_single_child_of_type_1_but_multiple_children_were_provided,
                        Some(vec![
                            children_prop_name,
                            self.type_to_string_(
                                &children_target_type,
                                Option::<&Node>::None,
                                None, None,
                            )
                        ])
                    );
                    if let Some(error_output_container) = error_output_container.as_ref() {
                        if matches!(error_output_container.skip_logging(), Some(true)) {
                            error_output_container.push_error(diag);
                        }
                    }
                }
            } else {
                if !Rc::ptr_eq(&non_array_like_target_parts, &self.never_type()) {
                    let child = &valid_children[0];
                    let elem = self.get_elaboration_element_for_jsx_child(
                        child,
                        &children_name_type,
                        &mut get_invalid_textual_child_diagnostic,
                    );
                    if let Some(elem) = elem {
                        result = self.elaborate_elementwise(
                            vec![elem],
                            source,
                            target,
                            relation.clone(),
                            containing_message_chain,
                            error_output_container,
                        ) || result;
                    }
                } else if !self.is_type_related_to(
                    &self.get_indexed_access_type(
                        source,
                        &children_name_type,
                        None,
                        Option::<&Node>::None,
                        Option::<&Symbol>::None,
                        None,
                    ),
                    &children_target_type,
                    relation,
                ) {
                    result = true;
                    let diag = self.error(
                        Some(&*containing_element.as_jsx_element().opening_element.as_jsx_opening_element().tag_name),
                        &Diagnostics::This_JSX_tag_s_0_prop_expects_type_1_which_requires_multiple_children_but_only_a_single_child_was_provided,
                        Some(vec![
                            children_prop_name,
                            self.type_to_string_(
                                &children_target_type,
                                Option::<&Node>::None,
                                None, None,
                            )
                        ])
                    );
                    if let Some(error_output_container) = error_output_container.as_ref() {
                        if matches!(error_output_container.skip_logging(), Some(true)) {
                            error_output_container.push_error(diag);
                        }
                    }
                }
            }
        }
        result
    }

    pub(super) fn generate_limited_tuple_elements(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
        target: &Type,
    ) -> Vec<ElaborationIteratorItem> {
        let node_as_array_literal_expression = node.as_array_literal_expression();
        let len = length(Some(&node_as_array_literal_expression.elements));
        let mut ret = vec![];
        if len == 0 {
            return ret;
        }
        for (i, elem) in node_as_array_literal_expression.elements.iter().enumerate() {
            if self.is_tuple_like_type(target)
                && self
                    .get_property_of_type_(target, &__String::new(i.to_string()), None)
                    .is_none()
            {
                continue;
            }
            if is_omitted_expression(elem) {
                continue;
            }
            let name_type = self.get_number_literal_type(Number::new(i as f64));
            ret.push(ElaborationIteratorItem {
                error_node: elem.clone(),
                inner_expression: Some(elem.clone()),
                name_type,
                error_message: None,
            });
        }
        ret
    }

    pub(super) fn elaborate_array_literal(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
        source: &Type,
        target: &Type,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        containing_message_chain: Option<Rc<dyn CheckTypeContainingMessageChain>>,
        error_output_container: Option<Rc<dyn CheckTypeErrorOutputContainer>>,
    ) -> bool {
        if target.flags().intersects(TypeFlags::Primitive) {
            return false;
        }
        if self.is_tuple_like_type(source) {
            return self.elaborate_elementwise(
                self.generate_limited_tuple_elements(node, target),
                source,
                target,
                relation,
                containing_message_chain,
                error_output_container,
            );
        }
        let old_context = node.maybe_contextual_type().clone();
        *node.maybe_contextual_type() = Some(target.type_wrapper());
        let tupleized_type =
            self.check_array_literal(node, Some(CheckMode::Contextual), Some(true));
        *node.maybe_contextual_type() = old_context;
        if self.is_tuple_like_type(&tupleized_type) {
            return self.elaborate_elementwise(
                self.generate_limited_tuple_elements(node, target),
                &tupleized_type,
                target,
                relation,
                containing_message_chain,
                error_output_container,
            );
        }
        false
    }

    pub(super) fn generate_object_literal_elements(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
                     // ) -> impl Iterator<Item = ElaborationIteratorItem> {
    ) -> Vec<ElaborationIteratorItem> {
        let node_as_object_literal_expression = node.as_object_literal_expression();
        if length(Some(&node_as_object_literal_expression.properties)) == 0 {
            return vec![];
        }
        node_as_object_literal_expression
            .properties
            .iter()
            .flat_map(|prop| {
                if is_spread_assignment(prop) {
                    return vec![];
                }
                let type_ = self.get_literal_type_from_property(
                    &self.get_symbol_of_node(prop).unwrap(),
                    TypeFlags::StringOrNumberLiteralOrUnique,
                    None,
                );
                if /* !type ||*/ type_.flags().intersects(TypeFlags::Never) {
                    return vec![];
                }
                match prop.kind() {
                    SyntaxKind::SetAccessor |
                    SyntaxKind::GetAccessor |
                    SyntaxKind::MethodDeclaration |
                    SyntaxKind::ShorthandPropertyAssignment => {
                        vec![ElaborationIteratorItem {
                            error_node: prop.as_named_declaration().name(),
                            inner_expression: None,
                            name_type: type_,
                            error_message: None,
                        }]
                    }
                    SyntaxKind::PropertyAssignment => {
                        let prop_as_property_assignment = prop.as_property_assignment();
                        vec![ElaborationIteratorItem {
                            error_node: prop_as_property_assignment.name(),
                            inner_expression: prop_as_property_assignment.maybe_initializer(),
                            name_type: type_,
                            error_message: if is_computed_non_literal_name(&prop_as_property_assignment.name()) {
                                Some(Cow::Borrowed(&Diagnostics::Type_of_computed_property_s_value_is_0_which_is_not_assignable_to_type_1))
                            } else {
                                None
                            },
                        }]
                    }
                    _ => Debug_.assert_never(prop, None),
                }
            })
            .collect()
    }

    pub(super) fn elaborate_object_literal(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        source: &Type,
        target: &Type,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        containing_message_chain: Option<Rc<dyn CheckTypeContainingMessageChain>>,
        error_output_container: Option<Rc<dyn CheckTypeErrorOutputContainer>>,
    ) -> bool {
        if target.flags().intersects(TypeFlags::Primitive) {
            return false;
        }
        self.elaborate_elementwise(
            self.generate_object_literal_elements(node),
            source,
            target,
            relation,
            containing_message_chain,
            error_output_container,
        )
    }

    pub(super) fn check_type_comparable_to(
        &self,
        source: &Type,
        target: &Type,
        error_node: &Node,
        head_message: Option<Cow<'static, DiagnosticMessage>>,
        containing_message_chain: Option<Rc<dyn CheckTypeContainingMessageChain>>,
    ) -> bool {
        self.check_type_related_to(
            source,
            target,
            self.comparable_relation.clone(),
            Some(error_node),
            head_message,
            containing_message_chain,
            None,
        )
    }

    pub(super) fn is_signature_assignable_to(
        &self,
        source: Rc<Signature>,
        target: Rc<Signature>,
        ignore_return_types: bool,
    ) -> bool {
        self.compare_signatures_related(
            source,
            target,
            if ignore_return_types {
                SignatureCheckMode::IgnoreReturnTypes
            } else {
                SignatureCheckMode::None
            },
            false,
            &mut None,
            Option::<&fn(&Type, &Type)>::None,
            Rc::new(TypeComparerCompareTypesAssignable::new(self.rc_wrapper())),
            None,
        ) != Ternary::False
    }

    pub(super) fn is_any_signature(&self, s: Rc<Signature>) -> bool {
        s.type_parameters.is_none()
            && match s.this_parameter.as_ref() {
                None => true,
                Some(s_this_parameter) => {
                    self.is_type_any(Some(self.get_type_of_parameter(s_this_parameter)))
                }
            }
            && s.parameters().len() == 1
            && signature_has_rest_parameter(&s)
            && (Rc::ptr_eq(
                &self.get_type_of_parameter(&s.parameters()[0]),
                &self.any_array_type(),
            ) || self.is_type_any(Some(self.get_type_of_parameter(&s.parameters()[0]))))
            && self.is_type_any(Some(self.get_return_type_of_signature(s)))
    }

    pub(super) fn compare_signatures_related<TIncompatibleErrorReporter: Fn(&Type, &Type)>(
        &self,
        mut source: Rc<Signature>,
        mut target: Rc<Signature>,
        check_mode: SignatureCheckMode,
        report_errors: bool,
        error_reporter: &mut Option<ErrorReporter>,
        incompatible_error_reporter: Option<&TIncompatibleErrorReporter>,
        compare_types: Rc<dyn TypeComparer>,
        report_unreliable_markers: Option<&TypeMapper>,
    ) -> Ternary {
        if Rc::ptr_eq(&source, &target) {
            return Ternary::True;
        }

        if self.is_any_signature(target.clone()) {
            return Ternary::True;
        }

        let target_count = self.get_parameter_count(&target);
        let source_has_more_parameters = !self.has_effective_rest_parameter(&target)
            && if check_mode.intersects(SignatureCheckMode::StrictArity) {
                self.has_effective_rest_parameter(&source)
                    || self.get_parameter_count(&source) > target_count
            } else {
                self.get_min_argument_count(&source, None) > target_count
            };
        if source_has_more_parameters {
            return Ternary::False;
        }

        if matches!(
            source.type_parameters.as_ref(),
            Some(source_type_parameters) if !matches!(
                target.type_parameters.as_ref(),
                Some(target_type_parameters) if are_rc_slices_equal(source_type_parameters, target_type_parameters)
            )
        ) {
            target = self.get_canonical_signature(target);
            source = self.instantiate_signature_in_context_of(
                source.clone(),
                target.clone(),
                None,
                Some(compare_types.clone()),
            );
        }

        let source_count = self.get_parameter_count(&source);
        let source_rest_type = self.get_non_array_rest_type(&source);
        let target_rest_type = self.get_non_array_rest_type(&target);
        if source_rest_type.is_some() || target_rest_type.is_some() {
            self.instantiate_type(
                &source_rest_type
                    .clone()
                    .unwrap_or_else(|| target_rest_type.clone().unwrap()),
                report_unreliable_markers,
            );
        }
        if source_rest_type.is_some() && target_rest_type.is_some() && source_count != target_count
        {
            return Ternary::False;
        }

        let kind = target.declaration.as_ref().map_or_else(
            || SyntaxKind::Unknown,
            |target_declaration| target_declaration.kind(),
        );
        let strict_variance = !check_mode.intersects(SignatureCheckMode::Callback)
            && self.strict_function_types
            && !matches!(
                kind,
                SyntaxKind::MethodDeclaration
                    | SyntaxKind::MethodSignature
                    | SyntaxKind::Constructor,
            );
        let mut result = Ternary::True;

        let source_this_type = self.get_this_type_of_signature(&source);
        if let Some(source_this_type) = source_this_type
            .as_ref()
            .filter(|source_this_type| !Rc::ptr_eq(source_this_type, &self.void_type()))
        {
            let target_this_type = self.get_this_type_of_signature(&target);
            if let Some(target_this_type) = target_this_type.as_ref() {
                let related = if !strict_variance {
                    let mut related =
                        compare_types.call(source_this_type, target_this_type, Some(false));
                    if related == Ternary::False {
                        related = compare_types.call(
                            target_this_type,
                            source_this_type,
                            Some(report_errors),
                        );
                    }
                    related
                } else {
                    compare_types.call(target_this_type, source_this_type, Some(report_errors))
                };
                if related == Ternary::False {
                    if report_errors {
                        (error_reporter.as_mut().unwrap())(
                            Cow::Borrowed(
                                &Diagnostics::The_this_types_of_each_signature_are_incompatible,
                            ),
                            None,
                        );
                    }
                    return Ternary::False;
                }
                result &= related;
            }
        }

        let param_count = if source_rest_type.is_some() || target_rest_type.is_some() {
            cmp::min(source_count, target_count)
        } else {
            cmp::max(source_count, target_count)
        };
        let rest_index = if source_rest_type.is_some() || target_rest_type.is_some() {
            Some(param_count - 1)
        } else {
            None
        };

        for i in 0..param_count {
            let source_type = if matches!(
                rest_index,
                Some(rest_index) if i == rest_index
            ) {
                Some(self.get_rest_type_at_position(&source, i))
            } else {
                self.try_get_type_at_position(&source, i)
            };
            let target_type = if matches!(
                rest_index,
                Some(rest_index) if i == rest_index
            ) {
                Some(self.get_rest_type_at_position(&target, i))
            } else {
                self.try_get_type_at_position(&target, i)
            };
            if let Some(source_type) = source_type.as_ref() {
                if let Some(target_type) = target_type.as_ref() {
                    let source_sig = if check_mode.intersects(SignatureCheckMode::Callback) {
                        None
                    } else {
                        self.get_single_call_signature(&self.get_non_nullable_type(source_type))
                    };
                    let target_sig = if check_mode.intersects(SignatureCheckMode::Callback) {
                        None
                    } else {
                        self.get_single_call_signature(&self.get_non_nullable_type(target_type))
                    };
                    let callbacks = matches!(
                        (source_sig.as_ref(), target_sig.as_ref()),
                        (Some(source_sig), Some(target_sig)) if
                            self.get_type_predicate_of_signature(source_sig).is_none() &&
                            self.get_type_predicate_of_signature(target_sig).is_none() &&
                            self.get_falsy_flags(source_type) & TypeFlags::Nullable ==
                            self.get_falsy_flags(target_type) & TypeFlags::Nullable
                    );
                    let mut related = if callbacks {
                        self.compare_signatures_related(
                            target_sig.unwrap(),
                            source_sig.unwrap(),
                            (check_mode & SignatureCheckMode::StrictArity)
                                | if strict_variance {
                                    SignatureCheckMode::StrictCallback
                                } else {
                                    SignatureCheckMode::BivariantCallback
                                },
                            report_errors,
                            error_reporter,
                            incompatible_error_reporter.clone(),
                            compare_types.clone(),
                            report_unreliable_markers,
                        )
                    } else {
                        if !check_mode.intersects(SignatureCheckMode::Callback) && !strict_variance
                        {
                            let mut related =
                                compare_types.call(source_type, target_type, Some(false));
                            if related == Ternary::False {
                                related = compare_types.call(
                                    target_type,
                                    source_type,
                                    Some(report_errors),
                                );
                            }
                            related
                        } else {
                            compare_types.call(target_type, source_type, Some(report_errors))
                        }
                    };
                    if related != Ternary::False
                        && check_mode.intersects(SignatureCheckMode::StrictArity)
                        && i >= self.get_min_argument_count(&source, None)
                        && i < self.get_min_argument_count(&target, None)
                        && compare_types.call(source_type, target_type, Some(false))
                            != Ternary::False
                    {
                        related = Ternary::False;
                    }
                    if related == Ternary::False {
                        if report_errors {
                            (error_reporter.as_mut().unwrap())(
                                Cow::Borrowed(
                                    &Diagnostics::Types_of_parameters_0_and_1_are_incompatible,
                                ),
                                Some(vec![
                                    unescape_leading_underscores(
                                        &self.get_parameter_name_at_position(
                                            &source,
                                            i,
                                            Option::<&Type>::None,
                                        ),
                                    ),
                                    unescape_leading_underscores(
                                        &self.get_parameter_name_at_position(
                                            &target,
                                            i,
                                            Option::<&Type>::None,
                                        ),
                                    ),
                                ]),
                            );
                        }
                        return Ternary::False;
                    }
                    result &= related;
                }
            }
        }

        if !check_mode.intersects(SignatureCheckMode::IgnoreReturnTypes) {
            let target_return_type = if self.is_resolving_return_type_of_signature(target.clone()) {
                self.any_type()
            } else if let Some(target_declaration) = target
                .declaration
                .as_ref()
                .filter(|target_declaration| self.is_js_constructor(Some(&***target_declaration)))
            {
                self.get_declared_type_of_class_or_interface(
                    &self
                        .get_merged_symbol(target_declaration.maybe_symbol())
                        .unwrap(),
                )
            } else {
                self.get_return_type_of_signature(target.clone())
            };
            if Rc::ptr_eq(&target_return_type, &self.void_type()) {
                return result;
            }
            let source_return_type = if self.is_resolving_return_type_of_signature(source.clone()) {
                self.any_type()
            } else if let Some(source_declaration) = source
                .declaration
                .as_ref()
                .filter(|source_declaration| self.is_js_constructor(Some(&***source_declaration)))
            {
                self.get_declared_type_of_class_or_interface(
                    &self
                        .get_merged_symbol(source_declaration.maybe_symbol())
                        .unwrap(),
                )
            } else {
                self.get_return_type_of_signature(source.clone())
            };

            let target_type_predicate = self.get_type_predicate_of_signature(&target);
            if let Some(target_type_predicate) = target_type_predicate.as_ref() {
                let source_type_predicate = self.get_type_predicate_of_signature(&source);
                if let Some(source_type_predicate) = source_type_predicate.as_ref() {
                    result &= self.compare_type_predicate_related_to(
                        source_type_predicate,
                        target_type_predicate,
                        report_errors,
                        error_reporter,
                        &mut |s: &Type, t: &Type, report_errors: Option<bool>| {
                            compare_types.call(s, t, report_errors)
                        },
                    );
                } else if is_identifier_type_predicate(target_type_predicate) {
                    if report_errors {
                        (error_reporter.as_mut().unwrap())(
                            Cow::Borrowed(&Diagnostics::Signature_0_must_be_a_type_predicate),
                            Some(vec![self.signature_to_string_(
                                &source,
                                Option::<&Node>::None,
                                None,
                                None,
                                None,
                            )]),
                        );
                    }
                    return Ternary::False;
                }
            } else {
                result &= if check_mode.intersects(SignatureCheckMode::BivariantCallback) {
                    let mut val =
                        compare_types.call(&target_return_type, &source_return_type, Some(false));
                    if val == Ternary::False {
                        val = compare_types.call(
                            &source_return_type,
                            &target_return_type,
                            Some(report_errors),
                        );
                    }
                    val
                } else {
                    compare_types.call(
                        &source_return_type,
                        &target_return_type,
                        Some(report_errors),
                    )
                };
                if result == Ternary::False && report_errors {
                    if let Some(incompatible_error_reporter) = incompatible_error_reporter {
                        incompatible_error_reporter(&source_return_type, &target_return_type);
                    }
                }
            }
        }

        result
    }
}

#[derive(Debug)]
pub(super) struct ElaborationIteratorItem {
    pub error_node: Rc<Node>,
    pub inner_expression: Option<Rc<Node /*Expression*/>>,
    name_type: Rc<Type>,
    error_message: Option<Cow<'static, DiagnosticMessage>>,
}

pub(super) type ErrorReporter<'a> =
    &'a mut dyn FnMut(Cow<'static, DiagnosticMessage>, Option<Vec<String>>);

pub(super) trait CheckTypeContainingMessageChain {
    fn get(&self) -> Option<Rc<RefCell<DiagnosticMessageChain>>>;
}

pub(super) trait CheckTypeErrorOutputContainer {
    fn push_error(&self, error: Rc<Diagnostic>);
    fn set_errors(&self, errors: Vec<Rc<Diagnostic>>);
    fn get_error(&self, index: usize) -> Option<Rc<Diagnostic>>;
    fn errors_len(&self) -> usize;
    fn skip_logging(&self) -> Option<bool>;
    fn errors(&self) -> Vec<Rc<Diagnostic>>;
}

pub(super) struct CheckTypeErrorOutputContainerConcrete {
    errors: RefCell<Vec<Rc<Diagnostic>>>,
    skip_logging: Option<bool>,
}

impl CheckTypeErrorOutputContainerConcrete {
    pub fn new(skip_logging: Option<bool>) -> Self {
        Self {
            errors: RefCell::new(vec![]),
            skip_logging,
        }
    }
}

impl CheckTypeErrorOutputContainer for CheckTypeErrorOutputContainerConcrete {
    fn push_error(&self, error: Rc<Diagnostic>) {
        self.errors.borrow_mut().push(error);
    }

    fn set_errors(&self, errors: Vec<Rc<Diagnostic>>) {
        *self.errors.borrow_mut() = errors;
    }

    fn get_error(&self, index: usize) -> Option<Rc<Diagnostic>> {
        self.errors.borrow().get(index).map(Clone::clone)
    }

    fn errors_len(&self) -> usize {
        self.errors.borrow().len()
    }

    fn skip_logging(&self) -> Option<bool> {
        self.skip_logging
    }

    fn errors(&self) -> Vec<Rc<Diagnostic>> {
        self.errors.borrow().clone()
    }
}
