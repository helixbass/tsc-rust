#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, CheckTypeRelatedTo};
use crate::{
    Debug_, DiagnosticMessage, LiteralTypeInterface, NamedDeclarationInterface, Node,
    NodeInterface, RelationComparisonResult, Symbol, Type, TypeChecker, TypeFlags, TypeInterface,
};
use local_macros::enum_unwrapped;

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
        head_message: Option<DiagnosticMessage>,
    ) -> bool {
        self.check_type_related_to_and_optionally_elaborate(
            source,
            target,
            &self.assignable_relation(),
            error_node,
            expr,
            head_message,
        )
    }

    pub(super) fn check_type_related_to_and_optionally_elaborate<
        TErrorNode: Borrow<Node>,
        TExpr: Borrow<Node>,
    >(
        &self,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        error_node: Option<TErrorNode>,
        expr: Option<TExpr>,
        head_message: Option<DiagnosticMessage>,
    ) -> bool {
        if self.is_type_related_to(source, target, relation) {
            return true;
        }
        if error_node.is_none()
            || !self.elaborate_error(expr, source, target, relation, head_message.clone())
        {
            return self.check_type_related_to(source, target, relation, error_node, head_message);
        }
        false
    }

    pub(super) fn elaborate_error<TNode: Borrow<Node>>(
        &self,
        node: Option<TNode>,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        head_message: Option<DiagnosticMessage>,
    ) -> bool {
        if node.is_none() || false {
            return false;
        }
        let node = node.unwrap();
        let node = node.borrow();
        match node {
            Node::ObjectLiteralExpression(_) => {
                return self.elaborate_object_literal(node, source, target, relation);
            }
            _ => (),
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
            unimplemented!()
        }
        None
    }

    pub(super) fn check_expression_for_mutable_location_with_contextual_type(
        &self,
        next: &Node, /*Expression*/
        source_prop_type: &Type,
    ) -> Rc<Type> {
        self.check_expression_for_mutable_location(
            next,
            Some(CheckMode::Contextual),
            Some(source_prop_type),
            None,
        )
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
                        );
                    }
                }
            }
        }
        reported_error
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

    pub(super) fn elaborate_object_literal(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
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

    pub(super) fn check_type_related_to<TErrorNode: Borrow<Node>>(
        &self,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        error_node: Option<TErrorNode>,
        head_message: Option<DiagnosticMessage>,
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
    error_message: Option<DiagnosticMessage>,
}

type ErrorReporter<'a> = &'a dyn FnMut(DiagnosticMessage, Option<Vec<String>>);
