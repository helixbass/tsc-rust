use std::{
    borrow::{Borrow, Cow},
    cell::RefCell,
    collections::HashMap,
    io, ptr,
    rc::Rc,
};

use gc::Gc;
use id_arena::Id;
use local_macros::enum_unwrapped;

use super::{
    CheckTypeContainingMessageChain, CheckTypeErrorOutputContainer, CheckTypeRelatedTo,
    ErrorReporter, IntersectionState,
};
use crate::{
    are_option_gcs_equal, get_object_flags, get_symbol_id, symbol_name, try_every, try_some,
    DiagnosticMessage, Diagnostics, HasArena, InArena, IntrinsicType, LiteralTypeInterface, Node,
    NodeInterface, ObjectFlags, ObjectTypeInterface, RelationComparisonResult, Signature, Symbol,
    SymbolFlags, SymbolInterface, Ternary, Type, TypeChecker, TypeFlags, TypeFormatFlags,
    TypeInterface, TypePredicate, TypePredicateKind,
};

impl TypeChecker {
    pub(super) fn compare_type_predicate_related_to(
        &self,
        source: Id<TypePredicate>,
        target: Id<TypePredicate>,
        report_errors: bool,
        error_reporter: &mut Option<ErrorReporter>,
        compare_types: &mut impl FnMut(Id<Type>, Id<Type>, Option<bool>) -> io::Result<Ternary>,
    ) -> io::Result<Ternary> {
        if source.ref_(self).kind != target.ref_(self).kind {
            if report_errors {
                (error_reporter.as_mut().unwrap())(
                    Cow::Borrowed(&Diagnostics::A_this_based_type_guard_is_not_compatible_with_a_parameter_based_type_guard),
                    None
                )?;
                (error_reporter.as_mut().unwrap())(
                    Cow::Borrowed(&Diagnostics::Type_predicate_0_is_not_assignable_to_1),
                    Some(vec![
                        self.type_predicate_to_string_(
                            source,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                        self.type_predicate_to_string_(
                            target,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                        )?,
                    ]),
                )?;
            }
            return Ok(Ternary::False);
        }

        if matches!(
            source.ref_(self).kind,
            TypePredicateKind::Identifier | TypePredicateKind::AssertsIdentifier
        ) {
            if source.ref_(self).parameter_index != target.ref_(self).parameter_index {
                if report_errors {
                    (error_reporter.as_mut().unwrap())(
                        Cow::Borrowed(
                            &Diagnostics::Parameter_0_is_not_in_the_same_position_as_parameter_1,
                        ),
                        Some(vec![
                            source.ref_(self).parameter_name.clone().unwrap(),
                            target.ref_(self).parameter_name.clone().unwrap(),
                        ]),
                    )?;
                    (error_reporter.as_mut().unwrap())(
                        Cow::Borrowed(&Diagnostics::Type_predicate_0_is_not_assignable_to_1),
                        Some(vec![
                            self.type_predicate_to_string_(
                                source,
                                Option::<Id<Node>>::None,
                                None,
                                None,
                            )?,
                            self.type_predicate_to_string_(
                                target,
                                Option::<Id<Node>>::None,
                                None,
                                None,
                            )?,
                        ]),
                    )?;
                }
                return Ok(Ternary::False);
            }
        }

        let related = if source.ref_(self).type_ == target.ref_(self).type_ {
            Ternary::True
        } else if source.ref_(self).type_.is_some() && target.ref_(self).type_.is_some() {
            compare_types(
                source.ref_(self).type_.unwrap(),
                target.ref_(self).type_.unwrap(),
                Some(report_errors),
            )?
        } else {
            Ternary::False
        };
        if related == Ternary::False && report_errors {
            (error_reporter.as_mut().unwrap())(
                Cow::Borrowed(&Diagnostics::Type_predicate_0_is_not_assignable_to_1),
                Some(vec![
                    self.type_predicate_to_string_(source, Option::<Id<Node>>::None, None, None)?,
                    self.type_predicate_to_string_(target, Option::<Id<Node>>::None, None, None)?,
                ]),
            )?;
        }
        Ok(related)
    }

    pub(super) fn is_implementation_compatible_with_overload(
        &self,
        implementation: Id<Signature>,
        overload: Id<Signature>,
    ) -> io::Result<bool> {
        let erased_source = self.get_erased_signature(implementation.clone())?;
        let erased_target = self.get_erased_signature(overload.clone())?;

        let source_return_type = self.get_return_type_of_signature(erased_source.clone())?;
        let target_return_type = self.get_return_type_of_signature(erased_target.clone())?;
        if target_return_type == self.void_type()
            || self.is_type_related_to(
                target_return_type,
                source_return_type,
                self.assignable_relation.clone(),
            )?
            || self.is_type_related_to(
                source_return_type,
                target_return_type,
                self.assignable_relation.clone(),
            )?
        {
            return self.is_signature_assignable_to(erased_source, erased_target, true);
        }

        Ok(false)
    }

    pub(super) fn is_empty_resolved_type(&self, t: Id<Type> /*ResolvedType*/) -> bool {
        t != self.any_function_type() && {
            t.ref_(self).as_resolved_type().properties().is_empty()
                && t.ref_(self).as_resolved_type().call_signatures().is_empty()
                && t.ref_(self)
                    .as_resolved_type()
                    .construct_signatures()
                    .is_empty()
                && t.ref_(self).as_resolved_type().index_infos().is_empty()
        }
    }

    pub(super) fn is_empty_object_type(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(if type_.ref_(self).flags().intersects(TypeFlags::Object) {
            !self.is_generic_mapped_type(type_)?
                && self.is_empty_resolved_type(self.resolve_structured_type_members(type_)?)
        } else if type_.ref_(self).flags().intersects(TypeFlags::NonPrimitive) {
            true
        } else if type_.ref_(self).flags().intersects(TypeFlags::Union) {
            try_some(
                Some(
                    type_
                        .ref_(self)
                        .as_union_or_intersection_type_interface()
                        .types(),
                ),
                Some(|&type_: &Id<Type>| self.is_empty_object_type(type_)),
            )?
        } else if type_.ref_(self).flags().intersects(TypeFlags::Intersection) {
            try_every(
                type_
                    .ref_(self)
                    .as_union_or_intersection_type_interface()
                    .types(),
                |&type_: &Id<Type>, _| self.is_empty_object_type(type_),
            )?
        } else {
            false
        })
    }

    pub(super) fn is_empty_anonymous_object_type(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(
            get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::Anonymous)
                && (type_.ref_(self).as_object_type().maybe_members().is_some()
                    && self.is_empty_resolved_type(type_)
                    || matches!(
                        type_.ref_(self).maybe_symbol(),
                        Some(type_symbol) if type_symbol.ref_(self).flags().intersects(SymbolFlags::TypeLiteral)
                            && self.get_members_of_symbol(type_symbol)?.ref_(self).len() == 0
                    )),
        )
    }

    pub(super) fn is_string_index_signature_only_type(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(type_.ref_(self).flags().intersects(TypeFlags::Object)
            && !self.is_generic_mapped_type(type_)?
            && self.get_properties_of_type(type_)?.len() == 0
            && self.get_index_infos_of_type(type_)?.len() == 1
            && self
                .get_index_info_of_type_(type_, self.string_type())?
                .is_some()
            || type_
                .ref_(self)
                .flags()
                .intersects(TypeFlags::UnionOrIntersection)
                && try_every(
                    &{
                        let types = type_
                            .ref_(self)
                            .as_union_or_intersection_type_interface()
                            .types()
                            .to_owned();
                        types
                    },
                    |&type_: &Id<Type>, _| self.is_string_index_signature_only_type(type_),
                )?
            || false)
    }

    pub(super) fn is_enum_type_related_to(
        &self,
        source_symbol: Id<Symbol>,
        target_symbol: Id<Symbol>,
        error_reporter: &mut Option<ErrorReporter>,
    ) -> io::Result<bool> {
        if source_symbol == target_symbol {
            return Ok(true);
        }
        let id = format!(
            "{},{}",
            get_symbol_id(&source_symbol.ref_(self)),
            get_symbol_id(&target_symbol.ref_(self))
        );
        let entry = self.enum_relation().get(&id).map(Clone::clone);
        if let Some(entry) = entry.filter(|entry| {
            !(!entry.intersects(RelationComparisonResult::Reported)
                && entry.intersects(RelationComparisonResult::Failed)
                && error_reporter.is_some())
        }) {
            return Ok(entry.intersects(RelationComparisonResult::Succeeded));
        }
        if source_symbol.ref_(self).escaped_name() != target_symbol.ref_(self).escaped_name()
            || !source_symbol
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::RegularEnum)
            || !target_symbol
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::RegularEnum)
        {
            self.enum_relation().insert(
                id,
                RelationComparisonResult::Failed | RelationComparisonResult::Reported,
            );
            return Ok(false);
        }
        let target_enum_type = self.get_type_of_symbol(target_symbol)?;
        for property in self.get_properties_of_type(self.get_type_of_symbol(source_symbol)?)? {
            if property
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::EnumMember)
            {
                let target_property = self.get_property_of_type_(
                    target_enum_type,
                    property.ref_(self).escaped_name(),
                    None,
                )?;
                if match target_property {
                    None => true,
                    Some(target_property) => !target_property
                        .ref_(self)
                        .flags()
                        .intersects(SymbolFlags::EnumMember),
                } {
                    if let Some(error_reporter) = error_reporter.as_mut() {
                        error_reporter(
                            Cow::Borrowed(&Diagnostics::Property_0_is_missing_in_type_1),
                            Some(vec![
                                symbol_name(property, self),
                                self.type_to_string_(
                                    self.get_declared_type_of_symbol(target_symbol)?,
                                    Option::<Id<Node>>::None,
                                    Some(TypeFormatFlags::UseFullyQualifiedType),
                                    None,
                                )?,
                            ]),
                        )?;
                        self.enum_relation().insert(
                            id,
                            RelationComparisonResult::Failed | RelationComparisonResult::Reported,
                        );
                    } else {
                        self.enum_relation()
                            .insert(id, RelationComparisonResult::Failed);
                    }
                    return Ok(false);
                }
            }
        }
        self.enum_relation()
            .insert(id, RelationComparisonResult::Succeeded);
        Ok(true)
    }

    pub(super) fn is_simple_type_related_to(
        &self,
        source: Id<Type>,
        target: Id<Type>,
        relation: &HashMap<String, RelationComparisonResult>,
        mut error_reporter: Option<ErrorReporter>,
    ) -> io::Result<bool> {
        let s = source.ref_(self).flags();
        let t = target.ref_(self).flags();
        if t.intersects(TypeFlags::AnyOrUnknown)
            || s.intersects(TypeFlags::Never)
            || source == self.wildcard_type()
        {
            return Ok(true);
        }
        if t.intersects(TypeFlags::Never) {
            return Ok(false);
        }
        if s.intersects(TypeFlags::StringLike) && t.intersects(TypeFlags::String) {
            return Ok(true);
        }
        if s.intersects(TypeFlags::StringLiteral)
            && s.intersects(TypeFlags::EnumLiteral)
            && t.intersects(TypeFlags::StringLiteral)
            && !t.intersects(TypeFlags::EnumLiteral)
            && source.ref_(self).as_string_literal_type().value
                == target.ref_(self).as_string_literal_type().value
        {
            return Ok(true);
        }
        if s.intersects(TypeFlags::NumberLike) && t.intersects(TypeFlags::Number) {
            return Ok(true);
        }
        if s.intersects(TypeFlags::NumberLiteral)
            && s.intersects(TypeFlags::EnumLiteral)
            && t.intersects(TypeFlags::NumberLiteral)
            && !t.intersects(TypeFlags::EnumLiteral)
            && source.ref_(self).as_number_literal_type().value
                == target.ref_(self).as_number_literal_type().value
        {
            return Ok(true);
        }
        if s.intersects(TypeFlags::BigIntLike) && t.intersects(TypeFlags::BigInt) {
            return Ok(true);
        }
        if s.intersects(TypeFlags::BooleanLike) && t.intersects(TypeFlags::Boolean) {
            return Ok(true);
        }
        if s.intersects(TypeFlags::ESSymbolLike) && t.intersects(TypeFlags::ESSymbol) {
            return Ok(true);
        }
        if s.intersects(TypeFlags::Enum)
            && t.intersects(TypeFlags::Enum)
            && self.is_enum_type_related_to(
                source.ref_(self).symbol(),
                target.ref_(self).symbol(),
                &mut error_reporter,
            )?
        {
            return Ok(true);
        }
        if s.intersects(TypeFlags::EnumLiteral) && t.intersects(TypeFlags::EnumLiteral) {
            if s.intersects(TypeFlags::Union)
                && t.intersects(TypeFlags::Union)
                && self.is_enum_type_related_to(
                    source.ref_(self).symbol(),
                    target.ref_(self).symbol(),
                    &mut error_reporter,
                )?
            {
                return Ok(true);
            }
            if s.intersects(TypeFlags::Literal)
                && t.intersects(TypeFlags::Literal)
                && source
                    .ref_(self)
                    .as_literal_type()
                    .is_value_eq(&target.ref_(self))
                && self.is_enum_type_related_to(
                    self.get_parent_of_symbol(source.ref_(self).symbol())?
                        .unwrap(),
                    self.get_parent_of_symbol(target.ref_(self).symbol())?
                        .unwrap(),
                    &mut error_reporter,
                )?
            {
                return Ok(true);
            }
        }
        if s.intersects(TypeFlags::Undefined)
            && (!self.strict_null_checks || t.intersects(TypeFlags::Undefined | TypeFlags::Void))
        {
            return Ok(true);
        }
        if s.intersects(TypeFlags::Null)
            && (!self.strict_null_checks || t.intersects(TypeFlags::Null))
        {
            return Ok(true);
        }
        if s.intersects(TypeFlags::Object) && t.intersects(TypeFlags::NonPrimitive) {
            return Ok(true);
        }
        if ptr::eq(relation, &*self.assignable_relation())
            || ptr::eq(relation, &*self.comparable_relation())
        {
            if s.intersects(TypeFlags::Any) {
                return Ok(true);
            }
            if s.intersects(TypeFlags::Number | TypeFlags::NumberLiteral)
                && !s.intersects(TypeFlags::EnumLiteral)
                && (t.intersects(TypeFlags::Enum)
                    || ptr::eq(relation, &*self.assignable_relation())
                        && t.intersects(TypeFlags::NumberLiteral)
                        && t.intersects(TypeFlags::EnumLiteral))
            {
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(super) fn is_type_related_to(
        &self,
        mut source: Id<Type>,
        mut target: Id<Type>,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
    ) -> io::Result<bool> {
        if self.is_fresh_literal_type(source) {
            source = match &*source.ref_(self) {
                Type::IntrinsicType(intrinsic_type) => {
                    enum_unwrapped!(intrinsic_type, [IntrinsicType, FreshableIntrinsicType])
                        .regular_type()
                }
                Type::LiteralType(literal_type) => literal_type.regular_type(),
                _ => panic!("Expected IntrinsicType or LiteralType"),
            };
        }
        if self.is_fresh_literal_type(target) {
            target = match &*target.ref_(self) {
                Type::IntrinsicType(intrinsic_type) => {
                    enum_unwrapped!(intrinsic_type, [IntrinsicType, FreshableIntrinsicType])
                        .regular_type()
                }
                Type::LiteralType(literal_type) => literal_type.regular_type(),
                _ => panic!("Expected IntrinsicType or LiteralType"),
            };
        }
        if source == target {
            return Ok(true);
        }
        if !Rc::ptr_eq(&relation, &self.identity_relation) {
            if Rc::ptr_eq(&relation, &self.comparable_relation)
                && !target.ref_(self).flags().intersects(TypeFlags::Never)
                && self.is_simple_type_related_to(target, source, &(*relation).borrow(), None)?
                || self.is_simple_type_related_to(source, target, &(*relation).borrow(), None)?
            {
                return Ok(true);
            }
        } else {
            if source.ref_(self).flags() != target.ref_(self).flags() {
                return Ok(false);
            }
            if source.ref_(self).flags().intersects(TypeFlags::Singleton) {
                return Ok(true);
            }
        }
        if source.ref_(self).flags().intersects(TypeFlags::Object)
            && target.ref_(self).flags().intersects(TypeFlags::Object)
        {
            let related = (*relation)
                .borrow()
                .get(&*self.get_relation_key(
                    source,
                    target,
                    IntersectionState::None,
                    &(*relation).borrow(),
                )?)
                .map(Clone::clone);
            if let Some(related) = related {
                return Ok(related.intersects(RelationComparisonResult::Succeeded));
            }
        }
        if source
            .ref_(self)
            .flags()
            .intersects(TypeFlags::StructuredOrInstantiable)
            || target
                .ref_(self)
                .flags()
                .intersects(TypeFlags::StructuredOrInstantiable)
        {
            return self.check_type_related_to(
                source,
                target,
                relation,
                Option::<Id<Node>>::None,
                None,
                None,
                None,
            );
        }
        Ok(false)
    }

    pub(super) fn is_ignored_jsx_property(
        &self,
        source: Id<Type>,
        source_prop: Id<Symbol>,
    ) -> bool {
        get_object_flags(&source.ref_(self)).intersects(ObjectFlags::JsxAttributes)
            && self.is_hyphenated_jsx_name(source_prop.ref_(self).escaped_name())
    }

    pub(super) fn get_normalized_type(
        &self,
        mut type_: Id<Type>,
        writing: bool,
    ) -> io::Result<Id<Type>> {
        loop {
            let mut t = if self.is_fresh_literal_type(type_) {
                match &*type_.ref_(self) {
                    Type::IntrinsicType(intrinsic_type) => {
                        enum_unwrapped!(intrinsic_type, [IntrinsicType, FreshableIntrinsicType])
                            .regular_type()
                    }
                    Type::LiteralType(literal_type) => literal_type.regular_type(),
                    _ => panic!("Expected IntrinsicType or LiteralType"),
                }
            } else if get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::Reference)
                && type_
                    .ref_(self)
                    .as_type_reference_interface()
                    .maybe_node()
                    .is_some()
            {
                self.create_type_reference(
                    {
                        let target = type_.ref_(self).as_type_reference().target;
                        target
                    },
                    Some(self.get_type_arguments(type_)?),
                )
            } else if type_
                .ref_(self)
                .flags()
                .intersects(TypeFlags::UnionOrIntersection)
            {
                self.get_reduced_type(type_)?
            } else if type_.ref_(self).flags().intersects(TypeFlags::Substitution) {
                if writing {
                    type_.ref_(self).as_substitution_type().base_type.clone()
                } else {
                    type_.ref_(self).as_substitution_type().substitute.clone()
                }
            } else if type_.ref_(self).flags().intersects(TypeFlags::Simplifiable) {
                self.get_simplified_type(type_, writing)?
            } else {
                type_
            };
            t = self
                .get_single_base_for_non_augmenting_subtype(t)?
                .unwrap_or(t);
            if t == type_ {
                break;
            }
            type_ = t.clone();
        }
        Ok(type_)
    }

    pub(super) fn check_type_related_to(
        &self,
        source: Id<Type>,
        target: Id<Type>,
        relation: Rc<RefCell<HashMap<String, RelationComparisonResult>>>,
        error_node: Option<Id<Node>>,
        head_message: Option<Cow<'static, DiagnosticMessage>>,
        containing_message_chain: Option<Gc<Box<dyn CheckTypeContainingMessageChain>>>,
        error_output_container: Option<Gc<Box<dyn CheckTypeErrorOutputContainer>>>,
    ) -> io::Result<bool> {
        CheckTypeRelatedTo::new(
            self,
            source,
            target,
            relation,
            error_node,
            head_message,
            containing_message_chain,
            error_output_container,
        )
        .call()
    }
}
