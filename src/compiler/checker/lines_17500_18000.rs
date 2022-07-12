#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{
    CheckTypeContainingMessageChain, CheckTypeContainingMessageChainDummy,
    CheckTypeErrorOutputContainer, CheckTypeRelatedTo, ErrorReporter, IntersectionState,
};
use crate::{
    are_option_rcs_equal, every, get_object_flags, get_symbol_id, some, symbol_name,
    DiagnosticMessage, Diagnostics, LiteralTypeInterface, Node, NodeInterface, ObjectFlags,
    ObjectTypeInterface, RelationComparisonResult, Signature, Symbol, SymbolFlags, SymbolInterface,
    Ternary, Type, TypeChecker, TypeFlags, TypeFormatFlags, TypeInterface, TypePredicate,
    TypePredicateKind,
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
        if source.kind != target.kind {
            if report_errors {
                (error_reporter.as_mut().unwrap())(
                    Cow::Borrowed(&Diagnostics::A_this_based_type_guard_is_not_compatible_with_a_parameter_based_type_guard),
                    None
                );
                (error_reporter.as_mut().unwrap())(
                    Cow::Borrowed(&Diagnostics::Type_predicate_0_is_not_assignable_to_1),
                    Some(vec![
                        self.type_predicate_to_string_(source, Option::<&Node>::None, None, None),
                        self.type_predicate_to_string_(target, Option::<&Node>::None, None, None),
                    ]),
                );
            }
            return Ternary::False;
        }

        if matches!(
            source.kind,
            TypePredicateKind::Identifier | TypePredicateKind::AssertsIdentifier
        ) {
            if source.parameter_index != target.parameter_index {
                if report_errors {
                    (error_reporter.as_mut().unwrap())(
                        Cow::Borrowed(
                            &Diagnostics::Parameter_0_is_not_in_the_same_position_as_parameter_1,
                        ),
                        Some(vec![
                            source.parameter_name.clone().unwrap(),
                            target.parameter_name.clone().unwrap(),
                        ]),
                    );
                    (error_reporter.as_mut().unwrap())(
                        Cow::Borrowed(&Diagnostics::Type_predicate_0_is_not_assignable_to_1),
                        Some(vec![
                            self.type_predicate_to_string_(
                                source,
                                Option::<&Node>::None,
                                None,
                                None,
                            ),
                            self.type_predicate_to_string_(
                                target,
                                Option::<&Node>::None,
                                None,
                                None,
                            ),
                        ]),
                    );
                }
                return Ternary::False;
            }
        }

        let related = if are_option_rcs_equal(source.type_.as_ref(), target.type_.as_ref()) {
            Ternary::True
        } else if source.type_.is_some() && target.type_.is_some() {
            compare_types(
                source.type_.as_ref().unwrap(),
                target.type_.as_ref().unwrap(),
                Some(report_errors),
            )
        } else {
            Ternary::False
        };
        if related == Ternary::False && report_errors {
            (error_reporter.as_mut().unwrap())(
                Cow::Borrowed(&Diagnostics::Type_predicate_0_is_not_assignable_to_1),
                Some(vec![
                    self.type_predicate_to_string_(source, Option::<&Node>::None, None, None),
                    self.type_predicate_to_string_(target, Option::<&Node>::None, None, None),
                ]),
            );
        }
        related
    }

    pub(super) fn is_implementation_compatible_with_overload(
        &self,
        implementation: Rc<Signature>,
        overload: Rc<Signature>,
    ) -> bool {
        let erased_source = self.get_erased_signature(implementation.clone());
        let erased_target = self.get_erased_signature(overload.clone());

        let source_return_type = self.get_return_type_of_signature(erased_source.clone());
        let target_return_type = self.get_return_type_of_signature(erased_target.clone());
        if Rc::ptr_eq(&target_return_type, &self.void_type())
            || self.is_type_related_to(
                &target_return_type,
                &source_return_type,
                &self.assignable_relation(),
            )
            || self.is_type_related_to(
                &source_return_type,
                &target_return_type,
                &self.assignable_relation(),
            )
        {
            return self.is_signature_assignable_to(erased_source, erased_target, true);
        }

        false
    }

    pub(super) fn is_empty_resolved_type(&self, t: &Type /*ResolvedType*/) -> bool {
        !ptr::eq(t, &*self.any_function_type()) && {
            let t_as_resolved_type = t.as_resolved_type();
            t_as_resolved_type.properties().is_empty()
                && t_as_resolved_type.call_signatures().is_empty()
                && t_as_resolved_type.construct_signatures().is_empty()
                && t_as_resolved_type.index_infos().is_empty()
        }
    }

    pub(super) fn is_empty_object_type(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Object) {
            !self.is_generic_mapped_type(type_)
                && self.is_empty_resolved_type(&self.resolve_structured_type_members(type_))
        } else if type_.flags().intersects(TypeFlags::NonPrimitive) {
            true
        } else if type_.flags().intersects(TypeFlags::Union) {
            some(
                Some(type_.as_union_or_intersection_type_interface().types()),
                Some(|type_: &Rc<Type>| self.is_empty_object_type(type_)),
            )
        } else if type_.flags().intersects(TypeFlags::Intersection) {
            every(
                type_.as_union_or_intersection_type_interface().types(),
                |type_: &Rc<Type>, _| self.is_empty_object_type(type_),
            )
        } else {
            false
        }
    }

    pub(super) fn is_empty_anonymous_object_type(&self, type_: &Type) -> bool {
        get_object_flags(type_).intersects(ObjectFlags::Anonymous)
            && (type_.as_object_type().maybe_members().is_some()
                && self.is_empty_resolved_type(type_)
                || matches!(
                    type_.maybe_symbol(),
                    Some(type_symbol) if type_symbol.flags().intersects(SymbolFlags::TypeLiteral) && (*self.get_members_of_symbol(&type_symbol)).borrow().len() == 0
                ))
    }

    pub(super) fn is_string_index_signature_only_type(&self, type_: &Type) -> bool {
        type_.flags().intersects(TypeFlags::Object)
            && !self.is_generic_mapped_type(type_)
            && self.get_properties_of_type(type_).is_empty()
            && self.get_index_infos_of_type(type_).len() == 1
            && self
                .get_index_info_of_type_(type_, &self.string_type())
                .is_some()
            || type_.flags().intersects(TypeFlags::UnionOrIntersection)
                && every(
                    type_.as_union_or_intersection_type_interface().types(),
                    |type_: &Rc<Type>, _| self.is_string_index_signature_only_type(type_),
                )
            || false
    }

    pub(super) fn is_enum_type_related_to(
        &self,
        source_symbol: &Symbol,
        target_symbol: &Symbol,
        error_reporter: &mut Option<ErrorReporter>,
    ) -> bool {
        if ptr::eq(source_symbol, target_symbol) {
            return true;
        }
        let id = format!(
            "{},{}",
            get_symbol_id(source_symbol),
            get_symbol_id(target_symbol)
        );
        let entry = self.enum_relation().get(&id).map(Clone::clone);
        if let Some(entry) = entry.filter(|entry| {
            !(!entry.intersects(RelationComparisonResult::Reported)
                && entry.intersects(RelationComparisonResult::Failed)
                && error_reporter.is_some())
        }) {
            return entry.intersects(RelationComparisonResult::Succeeded);
        }
        if source_symbol.escaped_name() != target_symbol.escaped_name()
            || !source_symbol.flags().intersects(SymbolFlags::RegularEnum)
            || !target_symbol.flags().intersects(SymbolFlags::RegularEnum)
        {
            self.enum_relation().insert(
                id,
                RelationComparisonResult::Failed | RelationComparisonResult::Reported,
            );
            return false;
        }
        let target_enum_type = self.get_type_of_symbol(target_symbol);
        for property in self.get_properties_of_type(&self.get_type_of_symbol(source_symbol)) {
            if property.flags().intersects(SymbolFlags::EnumMember) {
                let target_property =
                    self.get_property_of_type_(&target_enum_type, property.escaped_name(), None);
                if match target_property {
                    None => true,
                    Some(target_property) => {
                        !target_property.flags().intersects(SymbolFlags::EnumMember)
                    }
                } {
                    if let Some(error_reporter) = error_reporter.as_mut() {
                        error_reporter(
                            Cow::Borrowed(&Diagnostics::Property_0_is_missing_in_type_1),
                            Some(vec![
                                symbol_name(&property),
                                self.type_to_string_(
                                    &self.get_declared_type_of_symbol(target_symbol),
                                    Option::<&Node>::None,
                                    Some(TypeFormatFlags::UseFullyQualifiedType),
                                    None,
                                ),
                            ]),
                        );
                        self.enum_relation().insert(
                            id,
                            RelationComparisonResult::Failed | RelationComparisonResult::Reported,
                        );
                    } else {
                        self.enum_relation()
                            .insert(id, RelationComparisonResult::Failed);
                    }
                    return false;
                }
            }
        }
        self.enum_relation()
            .insert(id, RelationComparisonResult::Succeeded);
        true
    }

    pub(super) fn is_simple_type_related_to(
        &self,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        mut error_reporter: Option<ErrorReporter>,
    ) -> bool {
        let s = source.flags();
        let t = target.flags();
        if t.intersects(TypeFlags::AnyOrUnknown)
            || s.intersects(TypeFlags::Never)
            || ptr::eq(source, &*self.wildcard_type())
        {
            return true;
        }
        if t.intersects(TypeFlags::Never) {
            return false;
        }
        if s.intersects(TypeFlags::StringLike) && t.intersects(TypeFlags::String) {
            return true;
        }
        if s.intersects(TypeFlags::StringLiteral)
            && s.intersects(TypeFlags::EnumLiteral)
            && t.intersects(TypeFlags::StringLiteral)
            && !t.intersects(TypeFlags::EnumLiteral)
            && source.as_string_literal_type().value == target.as_string_literal_type().value
        {
            return true;
        }
        if s.intersects(TypeFlags::NumberLike) && t.intersects(TypeFlags::Number) {
            return true;
        }
        if s.intersects(TypeFlags::NumberLiteral)
            && s.intersects(TypeFlags::EnumLiteral)
            && t.intersects(TypeFlags::NumberLiteral)
            && !t.intersects(TypeFlags::EnumLiteral)
            && source.as_number_literal_type().value == target.as_number_literal_type().value
        {
            return true;
        }
        if s.intersects(TypeFlags::BigIntLike) && t.intersects(TypeFlags::BigInt) {
            return true;
        }
        if s.intersects(TypeFlags::BooleanLike) && t.intersects(TypeFlags::Boolean) {
            return true;
        }
        if s.intersects(TypeFlags::ESSymbolLike) && t.intersects(TypeFlags::ESSymbol) {
            return true;
        }
        if s.intersects(TypeFlags::Enum)
            && t.intersects(TypeFlags::Enum)
            && self.is_enum_type_related_to(&source.symbol(), &target.symbol(), &mut error_reporter)
        {
            return true;
        }
        if s.intersects(TypeFlags::EnumLiteral) && t.intersects(TypeFlags::EnumLiteral) {
            if s.intersects(TypeFlags::Union)
                && t.intersects(TypeFlags::Union)
                && self.is_enum_type_related_to(
                    &source.symbol(),
                    &target.symbol(),
                    &mut error_reporter,
                )
            {
                return true;
            }
            if s.intersects(TypeFlags::Literal)
                && t.intersects(TypeFlags::Literal)
                && source.as_literal_type().is_value_eq(target)
                && self.is_enum_type_related_to(
                    &self.get_parent_of_symbol(&source.symbol()).unwrap(),
                    &self.get_parent_of_symbol(&target.symbol()).unwrap(),
                    &mut error_reporter,
                )
            {
                return true;
            }
        }
        if s.intersects(TypeFlags::Undefined)
            && (!self.strict_null_checks || t.intersects(TypeFlags::Undefined | TypeFlags::Void))
        {
            return true;
        }
        if s.intersects(TypeFlags::Null)
            && (!self.strict_null_checks || t.intersects(TypeFlags::Null))
        {
            return true;
        }
        if s.intersects(TypeFlags::Object) && t.intersects(TypeFlags::NonPrimitive) {
            return true;
        }
        if ptr::eq(relation, &*self.assignable_relation())
            || ptr::eq(relation, &*self.comparable_relation())
        {
            if s.intersects(TypeFlags::Any) {
                return true;
            }
            if s.intersects(TypeFlags::Number | TypeFlags::NumberLiteral)
                && !s.intersects(TypeFlags::EnumLiteral)
                && (t.intersects(TypeFlags::Enum)
                    || ptr::eq(relation, &*self.assignable_relation())
                        && t.intersects(TypeFlags::NumberLiteral)
                        && t.intersects(TypeFlags::EnumLiteral))
            {
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
        if relation != &*self.identity_relation() {
            if relation == &*self.comparable_relation()
                && !target.flags().intersects(TypeFlags::Never)
                && self.is_simple_type_related_to(&source, &target, relation, None)
                || self.is_simple_type_related_to(&target, &source, relation, None)
            {
                return true;
            }
        } else {
            if source.flags() != target.flags() {
                return false;
            }
            if source.flags().intersects(TypeFlags::Singleton) {
                return true;
            }
        }
        if source.flags().intersects(TypeFlags::Object)
            && target.flags().intersects(TypeFlags::Object)
        {
            let related = relation.get(&self.get_relation_key(
                &source,
                &target,
                IntersectionState::None,
                relation,
            ));
            if let Some(related) = related {
                return related.intersects(RelationComparisonResult::Succeeded);
            }
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

    pub(super) fn is_ignored_jsx_property(&self, source: &Type, source_prop: &Symbol) -> bool {
        get_object_flags(source).intersects(ObjectFlags::JsxAttributes)
            && self.is_hyphenated_jsx_name(&**source_prop.escaped_name())
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
