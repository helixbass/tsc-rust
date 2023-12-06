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
    DiagnosticMessage, Diagnostics, IntrinsicType, LiteralTypeInterface, Node, NodeInterface,
    ObjectFlags, ObjectTypeInterface, RelationComparisonResult, Signature, Symbol, SymbolFlags,
    SymbolInterface, Ternary, Type, TypeChecker, TypeFlags, TypeFormatFlags, TypeInterface,
    TypePredicate, TypePredicateKind,
};

impl TypeChecker {
    pub(super) fn compare_type_predicate_related_to(
        &self,
        source: &TypePredicate,
        target: &TypePredicate,
        report_errors: bool,
        error_reporter: &mut Option<ErrorReporter>,
        compare_types: &mut impl FnMut(Id<Type>, Id<Type>, Option<bool>) -> io::Result<Ternary>,
    ) -> io::Result<Ternary> {
        if source.kind != target.kind {
            if report_errors {
                (error_reporter.as_mut().unwrap())(
                    Cow::Borrowed(&Diagnostics::A_this_based_type_guard_is_not_compatible_with_a_parameter_based_type_guard),
                    None
                )?;
                (error_reporter.as_mut().unwrap())(
                    Cow::Borrowed(&Diagnostics::Type_predicate_0_is_not_assignable_to_1),
                    Some(vec![
                        self.type_predicate_to_string_(source, Option::<&Node>::None, None, None)?,
                        self.type_predicate_to_string_(target, Option::<&Node>::None, None, None)?,
                    ]),
                )?;
            }
            return Ok(Ternary::False);
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
                    )?;
                    (error_reporter.as_mut().unwrap())(
                        Cow::Borrowed(&Diagnostics::Type_predicate_0_is_not_assignable_to_1),
                        Some(vec![
                            self.type_predicate_to_string_(
                                source,
                                Option::<&Node>::None,
                                None,
                                None,
                            )?,
                            self.type_predicate_to_string_(
                                target,
                                Option::<&Node>::None,
                                None,
                                None,
                            )?,
                        ]),
                    )?;
                }
                return Ok(Ternary::False);
            }
        }

        let related = if source.type_ == target.type_ {
            Ternary::True
        } else if source.type_.is_some() && target.type_.is_some() {
            compare_types(
                source.type_.unwrap(),
                target.type_.unwrap(),
                Some(report_errors),
            )?
        } else {
            Ternary::False
        };
        if related == Ternary::False && report_errors {
            (error_reporter.as_mut().unwrap())(
                Cow::Borrowed(&Diagnostics::Type_predicate_0_is_not_assignable_to_1),
                Some(vec![
                    self.type_predicate_to_string_(source, Option::<&Node>::None, None, None)?,
                    self.type_predicate_to_string_(target, Option::<&Node>::None, None, None)?,
                ]),
            )?;
        }
        Ok(related)
    }

    pub(super) fn is_implementation_compatible_with_overload(
        &self,
        implementation: Gc<Signature>,
        overload: Gc<Signature>,
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
            self.type_(t).as_resolved_type().properties().is_empty()
                && self
                    .type_(t)
                    .as_resolved_type()
                    .call_signatures()
                    .is_empty()
                && self
                    .type_(t)
                    .as_resolved_type()
                    .construct_signatures()
                    .is_empty()
                && self.type_(t).as_resolved_type().index_infos().is_empty()
        }
    }

    pub(super) fn is_empty_object_type(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(if self.type_(type_).flags().intersects(TypeFlags::Object) {
            !self.is_generic_mapped_type(type_)?
                && self.is_empty_resolved_type(self.resolve_structured_type_members(type_)?)
        } else if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::NonPrimitive)
        {
            true
        } else if self.type_(type_).flags().intersects(TypeFlags::Union) {
            try_some(
                Some(
                    self.type_(type_)
                        .as_union_or_intersection_type_interface()
                        .types(),
                ),
                Some(|&type_: &Id<Type>| self.is_empty_object_type(type_)),
            )?
        } else if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::Intersection)
        {
            try_every(
                self.type_(type_)
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
            get_object_flags(self.type_(type_)).intersects(ObjectFlags::Anonymous)
                && (self.type_(type_).as_object_type().maybe_members().is_some()
                    && self.is_empty_resolved_type(type_)
                    || matches!(
                        self.type_(type_).maybe_symbol(),
                        Some(type_symbol) if type_symbol.flags().intersects(SymbolFlags::TypeLiteral)
                            && (*self.get_members_of_symbol(&type_symbol)?).borrow().len() == 0
                    )),
        )
    }

    pub(super) fn is_string_index_signature_only_type(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(self.type_(type_).flags().intersects(TypeFlags::Object)
            && !self.is_generic_mapped_type(type_)?
            && self.get_properties_of_type(type_)?.len() == 0
            && self.get_index_infos_of_type(type_)?.len() == 1
            && self
                .get_index_info_of_type_(type_, self.string_type())?
                .is_some()
            || self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::UnionOrIntersection)
                && try_every(
                    self.type_(type_)
                        .as_union_or_intersection_type_interface()
                        .types(),
                    |&type_: &Id<Type>, _| self.is_string_index_signature_only_type(type_),
                )?
            || false)
    }

    pub(super) fn is_enum_type_related_to(
        &self,
        source_symbol: &Symbol,
        target_symbol: &Symbol,
        error_reporter: &mut Option<ErrorReporter>,
    ) -> io::Result<bool> {
        if ptr::eq(source_symbol, target_symbol) {
            return Ok(true);
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
            return Ok(entry.intersects(RelationComparisonResult::Succeeded));
        }
        if source_symbol.escaped_name() != target_symbol.escaped_name()
            || !source_symbol.flags().intersects(SymbolFlags::RegularEnum)
            || !target_symbol.flags().intersects(SymbolFlags::RegularEnum)
        {
            self.enum_relation().insert(
                id,
                RelationComparisonResult::Failed | RelationComparisonResult::Reported,
            );
            return Ok(false);
        }
        let target_enum_type = self.get_type_of_symbol(target_symbol)?;
        for property in self.get_properties_of_type(self.get_type_of_symbol(source_symbol)?)? {
            if property.flags().intersects(SymbolFlags::EnumMember) {
                let target_property =
                    self.get_property_of_type_(target_enum_type, property.escaped_name(), None)?;
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
                                symbol_name(&property).into_owned(),
                                self.type_to_string_(
                                    self.get_declared_type_of_symbol(target_symbol)?,
                                    Option::<&Node>::None,
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
        let s = self.type_(source).flags();
        let t = self.type_(target).flags();
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
            && self.type_(source).as_string_literal_type().value
                == self.type_(target).as_string_literal_type().value
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
            && self.type_(source).as_number_literal_type().value
                == self.type_(target).as_number_literal_type().value
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
                &self.type_(source).symbol(),
                &self.type_(target).symbol(),
                &mut error_reporter,
            )?
        {
            return Ok(true);
        }
        if s.intersects(TypeFlags::EnumLiteral) && t.intersects(TypeFlags::EnumLiteral) {
            if s.intersects(TypeFlags::Union)
                && t.intersects(TypeFlags::Union)
                && self.is_enum_type_related_to(
                    &self.type_(source).symbol(),
                    &self.type_(target).symbol(),
                    &mut error_reporter,
                )?
            {
                return Ok(true);
            }
            if s.intersects(TypeFlags::Literal)
                && t.intersects(TypeFlags::Literal)
                && self
                    .type_(source)
                    .as_literal_type()
                    .is_value_eq(self.type_(target))
                && self.is_enum_type_related_to(
                    &self
                        .get_parent_of_symbol(&self.type_(source).symbol())?
                        .unwrap(),
                    &self
                        .get_parent_of_symbol(&self.type_(target).symbol())?
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
            source = match self.type_(source) {
                Type::IntrinsicType(intrinsic_type) => {
                    enum_unwrapped!(intrinsic_type, [IntrinsicType, FreshableIntrinsicType])
                        .regular_type()
                }
                Type::LiteralType(literal_type) => literal_type.regular_type(),
                _ => panic!("Expected IntrinsicType or LiteralType"),
            };
        }
        if self.is_fresh_literal_type(target) {
            target = match self.type_(target) {
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
                && !self.type_(target).flags().intersects(TypeFlags::Never)
                && self.is_simple_type_related_to(target, source, &(*relation).borrow(), None)?
                || self.is_simple_type_related_to(source, target, &(*relation).borrow(), None)?
            {
                return Ok(true);
            }
        } else {
            if self.type_(source).flags() != self.type_(target).flags() {
                return Ok(false);
            }
            if self.type_(source).flags().intersects(TypeFlags::Singleton) {
                return Ok(true);
            }
        }
        if self.type_(source).flags().intersects(TypeFlags::Object)
            && self.type_(target).flags().intersects(TypeFlags::Object)
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
        if self
            .type_(source)
            .flags()
            .intersects(TypeFlags::StructuredOrInstantiable)
            || self
                .type_(target)
                .flags()
                .intersects(TypeFlags::StructuredOrInstantiable)
        {
            return self.check_type_related_to(
                source,
                target,
                relation,
                Option::<&Node>::None,
                None,
                None,
                None,
            );
        }
        Ok(false)
    }

    pub(super) fn is_ignored_jsx_property(&self, source: Id<Type>, source_prop: &Symbol) -> bool {
        get_object_flags(self.type_(source)).intersects(ObjectFlags::JsxAttributes)
            && self.is_hyphenated_jsx_name(source_prop.escaped_name())
    }

    pub(super) fn get_normalized_type(
        &self,
        mut type_: Id<Type>,
        writing: bool,
    ) -> io::Result<Id<Type>> {
        loop {
            let mut t = if self.is_fresh_literal_type(type_) {
                match self.type_(type_) {
                    Type::IntrinsicType(intrinsic_type) => {
                        enum_unwrapped!(intrinsic_type, [IntrinsicType, FreshableIntrinsicType])
                            .regular_type()
                    }
                    Type::LiteralType(literal_type) => literal_type.regular_type(),
                    _ => panic!("Expected IntrinsicType or LiteralType"),
                }
            } else if get_object_flags(self.type_(type_)).intersects(ObjectFlags::Reference)
                && self
                    .type_(type_)
                    .as_type_reference_interface()
                    .maybe_node()
                    .is_some()
            {
                self.create_type_reference(
                    self.type_(type_).as_type_reference().target,
                    Some(self.get_type_arguments(type_)?),
                )
            } else if self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::UnionOrIntersection)
            {
                self.get_reduced_type(type_)?
            } else if self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::Substitution)
            {
                if writing {
                    self.type_(type_).as_substitution_type().base_type.clone()
                } else {
                    self.type_(type_).as_substitution_type().substitute.clone()
                }
            } else if self
                .type_(type_)
                .flags()
                .intersects(TypeFlags::Simplifiable)
            {
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
        error_node: Option<impl Borrow<Node>>,
        head_message: Option<Cow<'static, DiagnosticMessage>>,
        containing_message_chain: Option<Gc<Box<dyn CheckTypeContainingMessageChain>>>,
        error_output_container: Option<Gc<Box<dyn CheckTypeErrorOutputContainer>>>,
    ) -> io::Result<bool> {
        CheckTypeRelatedTo::new(
            self,
            source,
            target,
            relation,
            error_node.map(|error_node| error_node.borrow().node_wrapper()),
            head_message,
            containing_message_chain,
            error_output_container,
        )
        .call()
    }
}
