#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, CheckTypeRelatedTo};
use crate::{
    SignatureDeclarationInterface, SymbolInterface, Ternary, __String, add_related_info,
    create_diagnostic_for_node, get_function_flags, has_type, is_block, length, map, some, Debug_,
    Diagnostic, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, FunctionFlags,
    FunctionLikeDeclarationInterface, LiteralTypeInterface, NamedDeclarationInterface, Node,
    NodeInterface, RelationComparisonResult, Signature, SignatureKind, Symbol, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeInterface, UnionOrIntersectionTypeInterface,
};
use local_macros::enum_unwrapped;

impl TypeChecker {
    pub(super) fn check_type_assignable_to_and_optionally_elaborate<
        TErrorNode: Borrow<Node>,
        TExpr: Borrow<Node>,
        TContainingMessageChain: CheckTypeContainingMessageChain,
    >(
        &self,
        source: &Type,
        target: &Type,
        error_node: Option<TErrorNode>,
        expr: Option<TExpr>,
        head_message: Option<&'static DiagnosticMessage>,
        containing_message_chain: Option<TContainingMessageChain>,
    ) -> bool {
        self.check_type_related_to_and_optionally_elaborate(
            source,
            target,
            &self.assignable_relation(),
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
        TContainingMessageChain: CheckTypeContainingMessageChain,
    >(
        &self,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        error_node: Option<TErrorNode>,
        expr: Option<TExpr>,
        head_message: Option<&'static DiagnosticMessage>,
        containing_message_chain: Option<TContainingMessageChain>,
        error_output_container: Option<&dyn CheckTypeErrorOutputContainer>,
    ) -> bool {
        if self.is_type_related_to(source, target, relation) {
            return true;
        }
        if error_node.is_none()
            || !self.elaborate_error(
                expr,
                source,
                target,
                relation,
                head_message,
                containing_message_chain.clone(),
                error_output_container,
            )
        {
            return self.check_type_related_to(
                source,
                target,
                relation,
                error_node,
                head_message,
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

    pub(super) fn elaborate_error<
        TNode: Borrow<Node>,
        TContainingMessageChain: CheckTypeContainingMessageChain,
    >(
        &self,
        node: Option<TNode>,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        head_message: Option<&'static DiagnosticMessage>,
        containing_message_chain: Option<TContainingMessageChain>,
        error_output_container: Option<&dyn CheckTypeErrorOutputContainer>,
    ) -> bool {
        if node.is_none() || self.is_or_has_generic_conditional(target) {
            return false;
        }
        let node = node.unwrap();
        let node = node.borrow();
        if !self.check_type_related_to(
            source,
            target,
            relation,
            Option::<&Node>::None,
            None,
            Option::<CheckTypeContainingMessageChainDummy>::None,
            None,
        ) && self.elaborate_did_you_mean_to_call_or_construct(
            node,
            source,
            target,
            relation,
            head_message,
            containing_message_chain.clone(),
            error_output_container,
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

    pub(super) fn elaborate_did_you_mean_to_call_or_construct<
        TContainingMessageChain: CheckTypeContainingMessageChain,
    >(
        &self,
        node: &Node, /*Expression*/
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        head_message: Option<&'static DiagnosticMessage>,
        containing_message_chain: Option<TContainingMessageChain>,
        error_output_container: Option<&dyn CheckTypeErrorOutputContainer>,
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
                            relation,
                            Option::<&Node>::None,
                            None,
                            Option::<CheckTypeContainingMessageChainDummy>::None,
                            None,
                        )
                }),
            ) {
                let result_obj_default = CheckTypeErrorOutputContainerConcrete::new(None);
                let result_obj: &dyn CheckTypeErrorOutputContainer =
                    error_output_container.unwrap_or(&result_obj_default);
                self.check_type_assignable_to(
                    source,
                    target,
                    Some(node),
                    head_message,
                    containing_message_chain.clone(),
                    Some(result_obj),
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

    pub(super) fn elaborate_arrow_function<
        TContainingMessageChain: CheckTypeContainingMessageChain,
    >(
        &self,
        node: &Node, /*ArrowFunction*/
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        containing_message_chain: Option<TContainingMessageChain>,
        error_output_container: Option<&dyn CheckTypeErrorOutputContainer>,
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
            map(Some(&target_signatures), |signature: &Rc<Signature>, _| {
                self.get_return_type_of_signature(signature.clone())
            })
            .unwrap(),
            None,
            Option::<&Symbol>::None,
            None,
            Option::<&Type>::None,
        );
        if !self.check_type_related_to(
            &source_return,
            &target_return,
            relation,
            Option::<&Node>::None,
            None,
            Option::<CheckTypeContainingMessageChainDummy>::None,
            None,
        ) {
            let elaborated = /*returnExpression &&*/ self.elaborate_error(
                Some(&*return_expression),
                &source_return,
                &target_return,
                relation,
                None,
                containing_message_chain.clone(),
                error_output_container,
            );
            if elaborated {
                return elaborated;
            }
            let result_obj_default = CheckTypeErrorOutputContainerConcrete::new(None);
            let result_obj: &dyn CheckTypeErrorOutputContainer =
                error_output_container.unwrap_or(&result_obj_default);
            self.check_type_related_to(
                &source_return,
                &target_return,
                relation,
                Some(&*return_expression),
                None,
                containing_message_chain,
                Some(result_obj),
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
                        Option::<CheckTypeContainingMessageChainDummy>::None,
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
        relation: &HashMap<String, RelationComparisonResult>,
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
            let target_prop_type = target_prop_type.unwrap();
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
            let source_prop_type = source_prop_type.unwrap();
            if !self.check_type_related_to(
                &source_prop_type,
                &target_prop_type,
                relation,
                Option::<&Node>::None,
                None,
                Option::<CheckTypeContainingMessageChainDummy>::None,
                None,
            ) {
                reported_error = true;
                if true {
                    let specific_source = if let Some(next) = next {
                        self.check_expression_for_mutable_location_with_contextual_type(
                            &*next,
                            &source_prop_type,
                        )
                    } else {
                        source_prop_type
                    };
                    if false {
                        unimplemented!()
                    } else {
                        let result = self.check_type_related_to(
                            &specific_source,
                            &target_prop_type,
                            relation,
                            Some(&*prop),
                            error_message,
                            Option::<CheckTypeContainingMessageChainDummy>::None, // TODO: these are wrong
                            None,
                        );
                    }
                }
            }
        }
        reported_error
    }

    pub(super) fn elaborate_jsx_components<
        TContainingMessageChain: CheckTypeContainingMessageChain,
    >(
        &self,
        node: &Node, /*JsxAttributes*/
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        containing_message_chain: Option<TContainingMessageChain>,
        error_output_container: Option<&dyn CheckTypeErrorOutputContainer>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn elaborate_array_literal<
        TContainingMessageChain: CheckTypeContainingMessageChain,
    >(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        containing_message_chain: Option<TContainingMessageChain>,
        error_output_container: Option<&dyn CheckTypeErrorOutputContainer>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn generate_object_literal_elements(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
                     // ) -> impl Iterator<Item = ElaborationIteratorItem> {
    ) -> Vec<ElaborationIteratorItem> {
        // if node.properties.is_empty() {
        //     return vec![];
        // }
        node.as_object_literal_expression()
            .properties
            .iter()
            .flat_map(|prop| {
                let type_ = self.get_literal_type_from_property(
                    &self.get_symbol_of_node(&**prop).unwrap(),
                    TypeFlags::StringOrNumberLiteralOrUnique,
                    None,
                );
                if type_.flags().intersects(TypeFlags::Never) {
                    return vec![];
                }
                match &**prop {
                    Node::PropertyAssignment(property_assignment) => {
                        vec![ElaborationIteratorItem {
                            error_node: property_assignment.name(),
                            inner_expression: Some(property_assignment.initializer.clone()),
                            name_type: type_,
                            error_message: if false { unimplemented!() } else { None },
                        }]
                    }
                    _ => Debug_.assert_never(prop, None),
                }
            })
            .collect()
    }

    pub(super) fn elaborate_object_literal<
        TContainingMessageChain: CheckTypeContainingMessageChain,
    >(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        containing_message_chain: Option<TContainingMessageChain>,
        error_output_container: Option<&dyn CheckTypeErrorOutputContainer>,
    ) -> bool {
        if target.flags().intersects(TypeFlags::Primitive) {
            return false;
        }
        self.elaborate_elementwise(
            self.generate_object_literal_elements(node),
            source,
            target,
            relation,
        )
    }

    pub(super) fn is_empty_resolved_type(&self, t: &Type /*ResolvedType*/) -> bool {
        unimplemented!()
    }

    pub(super) fn is_empty_object_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_empty_anonymous_object_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_string_index_signature_only_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_simple_type_related_to(
        &self,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        error_reporter: Option<ErrorReporter>,
    ) -> bool {
        let s = source.flags();
        let t = target.flags();
        if s.intersects(TypeFlags::NumberLike) && t.intersects(TypeFlags::Number) {
            return true;
        }
        if s.intersects(TypeFlags::BigIntLike) && t.intersects(TypeFlags::BigInt) {
            return true;
        }
        if ptr::eq(relation, &*self.assignable_relation())
            || ptr::eq(relation, &*self.comparable_relation())
        {
            if s.intersects(TypeFlags::Any) {
                return true;
            }
        }
        false
    }

    pub(super) fn is_type_related_to(
        &self,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
    ) -> bool {
        let mut source = source.type_wrapper();
        if self.is_fresh_literal_type(&source) {
            source = match &*source {
                Type::IntrinsicType(intrinsic_type) => {
                    enum_unwrapped!(intrinsic_type, [IntrinsicType, FreshableIntrinsicType])
                        .regular_type()
                        .upgrade()
                        .unwrap()
                }
                Type::LiteralType(literal_type) => literal_type.regular_type(),
                _ => panic!("Expected IntrinsicType or LiteralType"),
            };
        }
        let mut target = target.type_wrapper();
        if self.is_fresh_literal_type(&target) {
            target = match &*target {
                Type::IntrinsicType(intrinsic_type) => {
                    enum_unwrapped!(intrinsic_type, [IntrinsicType, FreshableIntrinsicType])
                        .regular_type()
                        .upgrade()
                        .unwrap()
                }
                Type::LiteralType(literal_type) => literal_type.regular_type(),
                _ => panic!("Expected IntrinsicType or LiteralType"),
            };
        }
        if Rc::ptr_eq(&source, &target) {
            return true;
        }
        if true {
            if self.is_simple_type_related_to(&source, &target, relation, None) {
                return true;
            }
        } else {
            unimplemented!()
        }
        if source
            .flags()
            .intersects(TypeFlags::StructuredOrInstantiable)
            || target
                .flags()
                .intersects(TypeFlags::StructuredOrInstantiable)
        {
            return self.check_type_related_to(
                &source,
                &target,
                relation,
                Option::<&Node>::None,
                None,
                Option::<CheckTypeContainingMessageChainDummy>::None,
                None,
            );
        }
        false
    }

    pub(super) fn get_normalized_type(&self, type_: &Type) -> Rc<Type> {
        let mut type_ = type_.type_wrapper();
        loop {
            let t: Rc<Type> = if self.is_fresh_literal_type(&type_) {
                match &*type_ {
                    Type::IntrinsicType(intrinsic_type) => {
                        enum_unwrapped!(intrinsic_type, [IntrinsicType, FreshableIntrinsicType])
                            .regular_type()
                            .upgrade()
                            .unwrap()
                    }
                    Type::LiteralType(literal_type) => literal_type.regular_type(),
                    _ => panic!("Expected IntrinsicType or LiteralType"),
                }
            } else {
                type_.type_wrapper()
            };
            if Rc::ptr_eq(&t, &type_) {
                break;
            }
            type_ = t.clone();
        }
        type_
    }

    pub(super) fn check_type_related_to<
        TErrorNode: Borrow<Node>,
        TContainingMessageChain: CheckTypeContainingMessageChain,
    >(
        &self,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        error_node: Option<TErrorNode>,
        head_message: Option<&'static DiagnosticMessage>,
        containing_message_chain: Option<TContainingMessageChain>,
        error_output_object: Option<&dyn CheckTypeErrorOutputContainer>,
    ) -> bool {
        CheckTypeRelatedTo::new(
            self,
            source,
            target,
            relation,
            error_node.map(|error_node| error_node.borrow().node_wrapper()),
            head_message,
        )
        .call()
    }
}

#[derive(Debug)]
pub(super) struct ElaborationIteratorItem {
    pub error_node: Rc<Node>,
    pub inner_expression: Option<Rc<Node /*Expression*/>>,
    name_type: Rc<Type>,
    error_message: Option<&'static DiagnosticMessage>,
}

type ErrorReporter<'a> = &'a dyn FnMut(DiagnosticMessage, Option<Vec<String>>);

pub(super) trait CheckTypeContainingMessageChain: Clone {
    fn get(&self) -> Option<Rc<RefCell<DiagnosticMessageChain>>>;
}

#[derive(Clone)]
pub(super) struct CheckTypeContainingMessageChainDummy;

impl CheckTypeContainingMessageChain for CheckTypeContainingMessageChainDummy {
    fn get(&self) -> Option<Rc<RefCell<DiagnosticMessageChain>>> {
        panic!("dummy implementation")
    }
}

pub(super) trait CheckTypeErrorOutputContainer {
    fn push_error(&self, error: Rc<Diagnostic>);
    fn get_error(&self, index: usize) -> Option<Rc<Diagnostic>>;
    fn errors_len(&self) -> usize;
    fn skip_logging(&self) -> Option<bool>;
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

    fn get_error(&self, index: usize) -> Option<Rc<Diagnostic>> {
        self.errors.borrow().get(index).map(Clone::clone)
    }

    fn errors_len(&self) -> usize {
        self.errors.borrow().len()
    }

    fn skip_logging(&self) -> Option<bool> {
        self.skip_logging
    }
}
