#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{
    CheckTypeContainingMessageChain, CheckTypeContainingMessageChainDummy,
    CheckTypeErrorOutputContainer, CheckTypeRelatedTo, ErrorReporter,
};
use crate::{
    DiagnosticMessage, LiteralTypeInterface, Node, NodeInterface, RelationComparisonResult,
    Ternary, Type, TypeChecker, TypeFlags, TypeInterface, TypePredicate,
};
use local_macros::enum_unwrapped;

impl TypeChecker {
    pub(super) fn compare_type_predicate_related_to<
        TCompareTypes: FnMut(&Type, &Type, Option<bool>) -> Ternary,
    >(
        &self,
        source: &TypePredicate,
        target: &TypePredicate,
        report_errors: bool,
        error_reporter: &mut Option<ErrorReporter>,
        compare_types: &mut TCompareTypes,
    ) -> Ternary {
        unimplemented!()
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
        head_message: Option<Cow<'static, DiagnosticMessage>>,
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
