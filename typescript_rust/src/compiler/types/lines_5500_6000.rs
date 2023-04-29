use bitflags::bitflags;
use derive_builder::Builder;
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use serde::Serialize;
use std::borrow::Cow;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::ops::{BitAnd, BitAndAssign};
use std::rc::Rc;
use std::sync::{Arc, Mutex};

use super::{
    BaseObjectType, BaseType, BaseUnionOrIntersectionType, Node, ObjectFlagsTypeInterface,
    ObjectTypeInterface, ResolvableTypeInterface, Symbol, SymbolTable, Type, TypeChecker,
    TypePredicate,
};
use crate::{Debug_, GcVec, ObjectFlags, ScriptKind, TypeFlags, __String, are_option_gcs_equal};
use local_macros::{enum_unwrapped, type_type};

#[derive(Clone, Debug, Trace, Finalize)]
#[type_type(
    ancestors = "UnionOrIntersectionType",
    interfaces = "UnionOrIntersectionTypeInterface, ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface, FreshObjectLiteralTypeInterface"
)]
pub struct IntersectionType {
    _union_or_intersection_type: BaseUnionOrIntersectionType,
    resolved_apparent_type: GcCell<Option<Gc<Type>>>,
}

impl IntersectionType {
    pub fn new(union_or_intersection_type: BaseUnionOrIntersectionType) -> Self {
        Self {
            _union_or_intersection_type: union_or_intersection_type,
            resolved_apparent_type: Default::default(),
        }
    }

    pub(crate) fn maybe_resolved_apparent_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.resolved_apparent_type.borrow_mut()
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
#[type_type(
    ancestors = "ObjectType",
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface, FreshObjectLiteralTypeInterface"
)]
pub struct MappedType {
    _object_type: BaseObjectType,
    pub declaration: Gc<Node /*MappedTypeNode*/>,
    type_parameter: GcCell<Option<Gc<Type /*TypeParameter*/>>>,
    constraint_type: GcCell<Option<Gc<Type>>>,
    name_type: GcCell<Option<Gc<Type>>>,
    template_type: GcCell<Option<Gc<Type>>>,
    modifiers_type: GcCell<Option<Gc<Type>>>,
    resolved_apparent_type: GcCell<Option<Gc<Type>>>,
    #[unsafe_ignore_trace]
    contains_error: Cell<Option<bool>>,
}

impl MappedType {
    pub fn new(object_type: BaseObjectType, declaration: Gc<Node>) -> Self {
        Self {
            _object_type: object_type,
            declaration,
            type_parameter: Default::default(),
            constraint_type: Default::default(),
            name_type: Default::default(),
            template_type: Default::default(),
            modifiers_type: Default::default(),
            resolved_apparent_type: Default::default(),
            contains_error: Default::default(),
        }
    }

    pub fn maybe_type_parameter(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.type_parameter.borrow_mut()
    }

    pub fn maybe_constraint_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.constraint_type.borrow_mut()
    }

    pub fn maybe_name_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.name_type.borrow_mut()
    }

    pub fn maybe_template_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.template_type.borrow_mut()
    }

    pub fn maybe_modifiers_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.modifiers_type.borrow_mut()
    }

    pub fn maybe_resolved_apparent_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.resolved_apparent_type.borrow_mut()
    }

    pub fn maybe_contains_error(&self) -> Option<bool> {
        self.contains_error.get()
    }

    pub fn set_contains_error(&self, contains_error: Option<bool>) {
        self.contains_error.set(contains_error);
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
#[type_type(
    ancestors = "ObjectType",
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface, FreshObjectLiteralTypeInterface"
)]
pub struct EvolvingArrayType {
    _object_type: BaseObjectType,
    pub element_type: Gc<Type>,
    final_array_type: GcCell<Option<Gc<Type>>>,
}

impl EvolvingArrayType {
    pub fn new(base_object_type: BaseObjectType, element_type: Gc<Type>) -> Self {
        Self {
            _object_type: base_object_type,
            element_type,
            final_array_type: Default::default(),
        }
    }

    pub fn maybe_final_array_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.final_array_type.borrow_mut()
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
#[type_type(
    ancestors = "ObjectType",
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface, FreshObjectLiteralTypeInterface"
)]
pub struct ReverseMappedType {
    _object_type: BaseObjectType,
    pub source: Gc<Type>,
    pub mapped_type: Gc<Type /*MappedType*/>,
    pub constraint_type: Gc<Type /*IndexType*/>,
}

impl ReverseMappedType {
    pub fn new(
        base_object_type: BaseObjectType,
        source: Gc<Type>,
        mapped_type: Gc<Type /*MappedType*/>,
        constraint_type: Gc<Type /*IndexType*/>,
    ) -> Self {
        Self {
            _object_type: base_object_type,
            source,
            mapped_type,
            constraint_type,
        }
    }
}

pub trait ResolvedTypeInterface:
    ObjectFlagsTypeInterface + ObjectTypeInterface + ResolvableTypeInterface
{
    fn members(&self) -> Gc<GcCell<SymbolTable>>;
    fn properties(&self) -> GcVec<Gc<Symbol>>;
    fn properties_mut(&self) -> GcCellRefMut<Option<GcVec<Gc<Symbol>>>, GcVec<Gc<Symbol>>>;
    fn set_properties(&self, properties: GcVec<Gc<Symbol>>);
    fn call_signatures(&self) -> GcCellRef<Vec<Gc<Signature>>>;
    fn set_call_signatures(&self, call_signatures: Vec<Gc<Signature>>);
    fn construct_signatures(&self) -> GcCellRef<Vec<Gc<Signature>>>;
    fn set_construct_signatures(&self, construct_signatures: Vec<Gc<Signature>>);
    fn index_infos(&self) -> GcCellRef<Vec<Gc<IndexInfo>>>;
    fn maybe_object_type_without_abstract_construct_signatures(&self) -> Option<Gc<Type>>;
    fn set_object_type_without_abstract_construct_signatures(
        &self,
        object_type_without_abstract_construct_signatures: Option<Gc<Type>>,
    );
}

pub trait FreshObjectLiteralTypeInterface: ResolvedTypeInterface {
    fn maybe_regular_type(&self) -> GcCellRefMut<Option<Gc<Type /*ResolvedType*/>>>;
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum IterationTypesKey {
    YieldType,
    ReturnType,
    NextType,
}

#[derive(Debug, Trace, Finalize)]
pub struct IterationTypes {
    yield_type: Option<Gc<Type>>,
    return_type: Option<Gc<Type>>,
    next_type: Option<Gc<Type>>,
}

impl IterationTypes {
    pub fn new(yield_type: Gc<Type>, return_type: Gc<Type>, next_type: Gc<Type>) -> Self {
        Self {
            yield_type: Some(yield_type),
            return_type: Some(return_type),
            next_type: Some(next_type),
        }
    }

    pub fn new_no_iteration_types() -> Self {
        Self {
            yield_type: None,
            return_type: None,
            next_type: None,
        }
    }

    pub fn yield_type(&self) -> Gc<Type> {
        let cloned = self.yield_type.clone();
        if cloned.is_none() {
            Debug_.fail(Some("Not supported"));
        }
        cloned.unwrap()
    }

    pub fn return_type(&self) -> Gc<Type> {
        let cloned = self.return_type.clone();
        if cloned.is_none() {
            Debug_.fail(Some("Not supported"));
        }
        cloned.unwrap()
    }

    pub fn next_type(&self) -> Gc<Type> {
        let cloned = self.next_type.clone();
        if cloned.is_none() {
            Debug_.fail(Some("Not supported"));
        }
        cloned.unwrap()
    }

    pub fn get_by_key(&self, key: IterationTypesKey) -> Gc<Type> {
        match key {
            IterationTypesKey::YieldType => self.yield_type(),
            IterationTypesKey::ReturnType => self.return_type(),
            IterationTypesKey::NextType => self.next_type(),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum IterationTypeCacheKey {
    IterationTypesOfGeneratorReturnType,
    IterationTypesOfAsyncGeneratorReturnType,
    IterationTypesOfIterable,
    IterationTypesOfIterator,
    IterationTypesOfAsyncIterable,
    IterationTypesOfAsyncIterator,
    IterationTypesOfIteratorResult,
}

#[derive(Clone, Debug, Trace, Finalize)]
#[type_type]
pub struct TypeParameter {
    _type: BaseType,
    pub constraint: GcCell<Option<Gc<Type>>>,
    pub default: GcCell<Option<Gc<Type>>>,
    pub target: Option<Gc<Type /*TypeParameter*/>>,
    pub mapper: GcCell<Option<Gc<TypeMapper>>>,
    pub is_this_type: Option<bool>,
}

impl TypeParameter {
    pub fn new(base_type: BaseType) -> Self {
        Self {
            _type: base_type,
            constraint: Default::default(),
            default: Default::default(),
            target: Default::default(),
            mapper: Default::default(),
            is_this_type: Default::default(),
        }
    }

    pub fn maybe_constraint(&self) -> Option<Gc<Type>> {
        self.constraint.borrow().clone()
    }

    pub fn set_constraint(&self, constraint: Gc<Type>) {
        *self.constraint.borrow_mut() = Some(constraint);
    }

    pub fn maybe_default(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.default.borrow_mut()
    }

    pub fn maybe_mapper(&self) -> Option<Gc<TypeMapper>> {
        self.mapper.borrow().clone()
    }

    pub fn set_mapper(&self, mapper: Gc<TypeMapper>) {
        *self.mapper.borrow_mut() = Some(mapper);
    }
}

bitflags! {
    pub/*(crate)*/ struct AccessFlags: u32 {
        const None = 0;
        const IncludeUndefined = 1 << 0;
        const NoIndexSignatures = 1 << 1;
        const Writing = 1 << 2;
        const CacheSymbol = 1 << 3;
        const NoTupleBoundsCheck = 1 << 4;
        const ExpressionPosition = 1 << 5;
        const ReportDeprecated = 1 << 6;
        const SuppressNoImplicitAnyError = 1 << 7;
        const Contextual = 1 << 8;
        const Persistent = Self::IncludeUndefined.bits;
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
#[type_type]
pub struct IndexedAccessType {
    _type: BaseType,
    pub object_type: Gc<Type>,
    pub index_type: Gc<Type>,
    #[unsafe_ignore_trace]
    pub(crate) access_flags: AccessFlags,
    pub(crate) constraint: Option<Gc<Type>>,
    simplified_for_reading: GcCell<Option<Gc<Type>>>,
    simplified_for_writing: GcCell<Option<Gc<Type>>>,
}

impl IndexedAccessType {
    pub fn new(
        base_type: BaseType,
        object_type: Gc<Type>,
        index_type: Gc<Type>,
        access_flags: AccessFlags,
    ) -> Self {
        Self {
            _type: base_type,
            object_type,
            index_type,
            access_flags,
            constraint: Default::default(),
            simplified_for_reading: Default::default(),
            simplified_for_writing: Default::default(),
        }
    }

    pub fn maybe_simplified_for_reading(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.simplified_for_reading.borrow_mut()
    }

    pub fn maybe_simplified_for_writing(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.simplified_for_writing.borrow_mut()
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
#[type_type]
pub struct IndexType {
    _type: BaseType,
    pub type_: Gc<Type /*InstantiableType | UnionOrIntersectionType*/>,
    pub(crate) strings_only: bool,
}

impl IndexType {
    pub fn new(_type: BaseType, type_: Gc<Type>, strings_only: bool) -> Self {
        Self {
            _type,
            type_,
            strings_only,
        }
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
pub struct ConditionalRoot {
    pub node: Gc<Node /*ConditionalTypeNode*/>,
    pub check_type: Gc<Type>,
    pub extends_type: Gc<Type>,
    pub is_distributive: bool,
    pub infer_type_parameters: Option<Vec<Gc<Type /*TypeParameter*/>>>,
    pub outer_type_parameters: Option<Vec<Gc<Type /*TypeParameter*/>>>,
    instantiations: GcCell<Option<HashMap<String, Gc<Type>>>>,
    pub alias_symbol: Option<Gc<Symbol>>,
    pub alias_type_arguments: Option<Vec<Gc<Type>>>,
}

impl ConditionalRoot {
    pub fn new(
        node: Gc<Node>,
        check_type: Gc<Type>,
        extends_type: Gc<Type>,
        is_distributive: bool,
        infer_type_parameters: Option<Vec<Gc<Type>>>,
        outer_type_parameters: Option<Vec<Gc<Type>>>,
        alias_symbol: Option<Gc<Symbol>>,
        alias_type_arguments: Option<Vec<Gc<Type>>>,
    ) -> Self {
        Self {
            node,
            check_type,
            extends_type,
            is_distributive,
            infer_type_parameters,
            outer_type_parameters,
            instantiations: Default::default(),
            alias_symbol,
            alias_type_arguments,
        }
    }

    pub fn maybe_instantiations(&self) -> GcCellRefMut<Option<HashMap<String, Gc<Type>>>> {
        self.instantiations.borrow_mut()
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
#[type_type]
pub struct ConditionalType {
    _type: BaseType,
    pub root: Gc<GcCell<ConditionalRoot>>,
    pub check_type: Gc<Type>,
    pub extends_type: Gc<Type>,
    resolved_true_type: GcCell<Option<Gc<Type>>>,
    resolved_false_type: GcCell<Option<Gc<Type>>>,
    resolved_inferred_true_type: GcCell<Option<Gc<Type>>>,
    resolved_default_constraint: GcCell<Option<Gc<Type>>>,
    pub(crate) mapper: Option<Gc<TypeMapper>>,
    pub(crate) combined_mapper: Option<Gc<TypeMapper>>,
}

impl ConditionalType {
    pub fn new(
        base_type: BaseType,
        root: Gc<GcCell<ConditionalRoot>>,
        check_type: Gc<Type>,
        extends_type: Gc<Type>,
        mapper: Option<Gc<TypeMapper>>,
        combined_mapper: Option<Gc<TypeMapper>>,
    ) -> Self {
        Self {
            _type: base_type,
            root,
            check_type,
            extends_type,
            resolved_true_type: Default::default(),
            resolved_false_type: Default::default(),
            resolved_inferred_true_type: Default::default(),
            resolved_default_constraint: Default::default(),
            mapper,
            combined_mapper,
        }
    }

    pub(crate) fn maybe_resolved_true_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.resolved_true_type.borrow_mut()
    }

    pub(crate) fn maybe_resolved_false_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.resolved_false_type.borrow_mut()
    }

    pub(crate) fn maybe_resolved_inferred_true_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.resolved_inferred_true_type.borrow_mut()
    }

    pub(crate) fn maybe_resolved_default_constraint(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.resolved_default_constraint.borrow_mut()
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
#[type_type]
pub struct TemplateLiteralType {
    _type: BaseType,
    pub texts: Vec<String>,
    pub types: Vec<Gc<Type>>,
}

impl TemplateLiteralType {
    pub fn new(_type: BaseType, texts: Vec<String>, types: Vec<Gc<Type>>) -> Self {
        Self {
            _type,
            texts,
            types,
        }
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
#[type_type]
pub struct StringMappingType {
    _type: BaseType,
    pub type_: Gc<Type>,
}

impl StringMappingType {
    pub fn new(base_type: BaseType, type_: Gc<Type>) -> Self {
        Self {
            _type: base_type,
            type_,
        }
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
#[type_type]
pub struct SubstitutionType {
    _type: BaseType,
    #[unsafe_ignore_trace]
    object_flags: Cell<ObjectFlags>,
    pub base_type: Gc<Type>,
    pub substitute: Gc<Type>,
}

impl SubstitutionType {
    pub fn new(_type: BaseType, base_type: Gc<Type>, substitute: Gc<Type>) -> Self {
        Self {
            _type,
            object_flags: Cell::new(
                ObjectFlags::None, // this is made up
            ),
            base_type,
            substitute,
        }
    }
}

impl ObjectFlagsTypeInterface for SubstitutionType {
    fn object_flags(&self) -> ObjectFlags {
        self.object_flags.get()
    }

    fn set_object_flags(&self, object_flags: ObjectFlags) {
        self.object_flags.set(object_flags);
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) enum JsxReferenceKind {
    Component,
    Function,
    Mixed,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum SignatureKind {
    Call,
    Construct,
}

bitflags! {
    pub struct SignatureFlags: u32 {
        const None = 0;

        const HasRestParameter = 1 << 0;
        const HasLiteralTypes = 1 << 1;
        const Abstract = 1 << 2;

        const IsInnerCallChain = 1 << 3;
        const IsOuterCallChain = 1 << 4;
        const IsUntypedSignatureInJSFile = 1 << 5;

        const PropagatingFlags = Self::HasRestParameter.bits | Self::HasLiteralTypes.bits | Self::Abstract.bits | Self::IsUntypedSignatureInJSFile.bits;

        const CallChainFlags = Self::IsInnerCallChain.bits | Self::IsOuterCallChain.bits;
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct Signature {
    #[unsafe_ignore_trace]
    pub flags: SignatureFlags,
    pub declaration: Option<Gc<Node /*SignatureDeclaration | JSDocSignature*/>>,
    type_parameters: GcCell<Option<Vec<Gc<Type /*TypeParameter*/>>>>,
    parameters: Option<Vec<Gc<Symbol>>>,
    this_parameter: GcCell<Option<Gc<Symbol>>>,
    resolved_return_type: GcCell<Option<Gc<Type>>>,
    resolved_type_predicate: GcCell<Option<Gc<TypePredicate>>>,
    min_argument_count: Option<usize>,
    #[unsafe_ignore_trace]
    resolved_min_argument_count: Cell<Option<usize>>,
    pub target: Option<Gc<Signature>>,
    pub mapper: Option<Gc<TypeMapper>>,
    pub composite_signatures: Option<Vec<Gc<Signature>>>,
    #[unsafe_ignore_trace]
    pub composite_kind: Option<TypeFlags>,
    erased_signature_cache: GcCell<Option<Gc<Signature>>>,
    canonical_signature_cache: GcCell<Option<Gc<Signature>>>,
    base_signature_cache: GcCell<Option<Gc<Signature>>>,
    optional_call_signature_cache: GcCell<Option<SignatureOptionalCallSignatureCache>>,
    isolated_signature_type: GcCell<Option<Gc<Type /*ObjectType*/>>>,
    instantiations: GcCell<Option<HashMap<String, Gc<Signature>>>>,
}

impl Signature {
    pub fn new(flags: SignatureFlags) -> Self {
        Self {
            flags,
            declaration: None,
            type_parameters: Default::default(),
            parameters: Default::default(),
            this_parameter: Default::default(),
            resolved_return_type: Default::default(),
            resolved_type_predicate: Default::default(),
            min_argument_count: Default::default(),
            resolved_min_argument_count: Default::default(),
            target: Default::default(),
            mapper: Default::default(),
            composite_signatures: Default::default(),
            composite_kind: Default::default(),
            erased_signature_cache: Default::default(),
            canonical_signature_cache: Default::default(),
            base_signature_cache: Default::default(),
            optional_call_signature_cache: Default::default(),
            isolated_signature_type: Default::default(),
            instantiations: Default::default(),
        }
    }

    pub fn maybe_type_parameters(&self) -> GcCellRef<Option<Vec<Gc<Type>>>> {
        self.type_parameters.borrow()
    }

    pub fn maybe_type_parameters_mut(&self) -> GcCellRefMut<Option<Vec<Gc<Type>>>> {
        self.type_parameters.borrow_mut()
    }

    pub fn parameters(&self) -> &[Gc<Symbol>] {
        self.parameters.as_ref().unwrap()
    }

    pub fn set_parameters(&mut self, parameters: Vec<Gc<Symbol>>) {
        self.parameters = Some(parameters);
    }

    pub fn maybe_this_parameter(&self) -> GcCellRef<Option<Gc<Symbol>>> {
        self.this_parameter.borrow()
    }

    pub fn maybe_this_parameter_mut(&self) -> GcCellRefMut<Option<Gc<Symbol>>> {
        self.this_parameter.borrow_mut()
    }

    pub fn maybe_resolved_return_type(&self) -> Option<Gc<Type>> {
        self.resolved_return_type.borrow().clone()
    }

    pub fn maybe_resolved_return_type_mut(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.resolved_return_type.borrow_mut()
    }

    pub fn maybe_resolved_type_predicate(&self) -> Option<Gc<TypePredicate>> {
        self.resolved_type_predicate.borrow().clone()
    }

    pub fn maybe_resolved_type_predicate_mut(&self) -> GcCellRefMut<Option<Gc<TypePredicate>>> {
        self.resolved_type_predicate.borrow_mut()
    }

    pub fn min_argument_count(&self) -> usize {
        self.min_argument_count.unwrap()
    }

    pub fn set_min_argument_count(&mut self, min_argument_count: usize) {
        self.min_argument_count = Some(min_argument_count);
    }

    pub fn maybe_resolved_min_argument_count(&self) -> Option<usize> {
        self.resolved_min_argument_count.get()
    }

    pub fn resolved_min_argument_count(&self) -> usize {
        self.resolved_min_argument_count.get().unwrap()
    }

    pub fn set_resolved_min_argument_count(&self, min_argument_count: usize) {
        self.resolved_min_argument_count
            .set(Some(min_argument_count));
    }

    pub fn maybe_erased_signature_cache(&self) -> GcCellRefMut<Option<Gc<Signature>>> {
        self.erased_signature_cache.borrow_mut()
    }

    pub fn maybe_canonical_signature_cache(&self) -> GcCellRefMut<Option<Gc<Signature>>> {
        self.canonical_signature_cache.borrow_mut()
    }

    pub fn maybe_base_signature_cache(&self) -> GcCellRefMut<Option<Gc<Signature>>> {
        self.base_signature_cache.borrow_mut()
    }

    pub fn maybe_optional_call_signature_cache(
        &self,
    ) -> GcCellRefMut<Option<SignatureOptionalCallSignatureCache>> {
        self.optional_call_signature_cache.borrow_mut()
    }

    pub fn maybe_isolated_signature_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.isolated_signature_type.borrow_mut()
    }

    pub fn maybe_instantiations(&self) -> GcCellRefMut<Option<HashMap<String, Gc<Signature>>>> {
        self.instantiations.borrow_mut()
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct SignatureOptionalCallSignatureCache {
    pub inner: Option<Gc<Signature>>,
    pub outer: Option<Gc<Signature>>,
}

impl SignatureOptionalCallSignatureCache {
    pub fn new() -> Self {
        Self {
            inner: None,
            outer: None,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum IndexKind {
    String,
    Number,
}

#[derive(Debug, Trace, Finalize)]
pub struct IndexInfo {
    pub key_type: Gc<Type>,
    pub type_: Gc<Type>,
    pub is_readonly: bool,
    pub declaration: Option<Gc<Node /*IndexSignatureDeclaration*/>>,
}

#[derive(Debug, Finalize, Trace)]
pub enum TypeMapper {
    Simple(TypeMapperSimple),
    Array(TypeMapperArray),
    Function(TypeMapperFunction),
    Composite(TypeMapperCompositeOrMerged),
    Merged(TypeMapperCompositeOrMerged),
}

impl TypeMapper {
    pub fn as_simple(&self) -> &TypeMapperSimple {
        match self {
            Self::Simple(value) => value,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, Finalize, Trace)]
pub struct TypeMapperSimple {
    pub source: Gc<Type>,
    pub target: Gc<Type>,
}

#[derive(Clone, Debug, Finalize, Trace)]
pub struct TypeMapperArray {
    pub sources: Vec<Gc<Type>>,
    pub targets: Option<Vec<Gc<Type>>>,
}

pub trait TypeMapperCallback: Trace + Finalize {
    // TODO: now that TypeChecker is wrapped in Rc should remove the checker argument here?
    fn call(&self, checker: &TypeChecker, type_: &Type) -> Gc<Type>;
}

#[derive(Clone, Finalize, Trace)]
pub struct TypeMapperFunction {
    pub func: Gc<Box<dyn TypeMapperCallback>>,
}

impl fmt::Debug for TypeMapperFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("TypeMapperFunction")
    }
}

#[derive(Clone, Debug, Finalize, Trace)]
pub struct TypeMapperCompositeOrMerged {
    pub mapper1: Gc<TypeMapper>,
    pub mapper2: Gc<TypeMapper>,
}

impl TypeMapper {
    pub fn new_simple(source: Gc<Type>, target: Gc<Type>) -> Self {
        Self::Simple(TypeMapperSimple { source, target })
    }

    pub fn new_array(sources: Vec<Gc<Type>>, targets: Option<Vec<Gc<Type>>>) -> Self {
        Self::Array(TypeMapperArray { sources, targets })
    }

    pub fn new_function<TFunc: 'static + TypeMapperCallback>(func: TFunc) -> Self {
        Self::Function(TypeMapperFunction {
            func: Gc::new(Box::new(func)),
        })
    }

    pub fn new_composite(mapper1: Gc<TypeMapper>, mapper2: Gc<TypeMapper>) -> Self {
        Self::Composite(TypeMapperCompositeOrMerged { mapper1, mapper2 })
    }

    pub fn new_merged(mapper1: Gc<TypeMapper>, mapper2: Gc<TypeMapper>) -> Self {
        Self::Merged(TypeMapperCompositeOrMerged { mapper1, mapper2 })
    }
}

bitflags! {
    pub struct InferencePriority: i32 {
        const None = 0;
        const NakedTypeVariable = 1 << 0;
        const SpeculativeTuple = 1 << 1;
        const SubstituteSource = 1 << 2;
        const HomomorphicMappedType = 1 << 3;
        const PartialHomomorphicMappedType = 1 << 4;
        const MappedTypeConstraint = 1 << 5;
        const ContravariantConditional = 1 << 6;
        const ReturnType = 1 << 7;
        const LiteralKeyof = 1 << 8;
        const NoConstraints = 1 << 9;
        const AlwaysStrict = 1 << 10;
        const MaxValue = 1 << 11;

        const PriorityImpliesCombination = Self::ReturnType.bits | Self::MappedTypeConstraint.bits | Self::LiteralKeyof.bits;
        const Circularity = -1;
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct InferenceInfo {
    pub type_parameter: Gc<Type /*TypeParameter*/>,
    candidates: GcCell<Option<Vec<Gc<Type>>>>,
    contra_candidates: GcCell<Option<Vec<Gc<Type>>>>,
    inferred_type: GcCell<Option<Gc<Type>>>,
    #[unsafe_ignore_trace]
    priority: Cell<Option<InferencePriority>>,
    #[unsafe_ignore_trace]
    top_level: Cell<bool>,
    #[unsafe_ignore_trace]
    is_fixed: Cell<bool>,
    #[unsafe_ignore_trace]
    implied_arity: Cell<Option<usize>>,
}

impl InferenceInfo {
    pub fn new(
        type_parameter: Gc<Type /*TypeParameter*/>,
        candidates: Option<Vec<Gc<Type>>>,
        contra_candidates: Option<Vec<Gc<Type>>>,
        inferred_type: Option<Gc<Type>>,
        priority: Option<InferencePriority>,
        top_level: bool,
        is_fixed: bool,
        implied_arity: Option<usize>,
    ) -> Self {
        Self {
            type_parameter,
            candidates: GcCell::new(candidates),
            contra_candidates: GcCell::new(contra_candidates),
            inferred_type: GcCell::new(inferred_type),
            priority: Cell::new(priority),
            top_level: Cell::new(top_level),
            is_fixed: Cell::new(is_fixed),
            implied_arity: Cell::new(implied_arity),
        }
    }

    pub fn maybe_candidates(&self) -> GcCellRef<Option<Vec<Gc<Type>>>> {
        self.candidates.borrow()
    }

    pub fn maybe_candidates_mut(&self) -> GcCellRefMut<Option<Vec<Gc<Type>>>> {
        self.candidates.borrow_mut()
    }

    pub fn maybe_contra_candidates(&self) -> GcCellRef<Option<Vec<Gc<Type>>>> {
        self.contra_candidates.borrow()
    }

    pub fn maybe_contra_candidates_mut(&self) -> GcCellRefMut<Option<Vec<Gc<Type>>>> {
        self.contra_candidates.borrow_mut()
    }

    pub fn maybe_inferred_type(&self) -> Option<Gc<Type>> {
        self.inferred_type.borrow().clone()
    }

    pub fn maybe_inferred_type_mut(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.inferred_type.borrow_mut()
    }

    pub fn maybe_priority(&self) -> Option<InferencePriority> {
        self.priority.get()
    }

    pub fn set_priority(&self, priority: Option<InferencePriority>) {
        self.priority.set(priority);
    }

    pub fn top_level(&self) -> bool {
        self.top_level.get()
    }

    pub fn set_top_level(&self, top_level: bool) {
        self.top_level.set(top_level);
    }

    pub fn is_fixed(&self) -> bool {
        self.is_fixed.get()
    }

    pub fn set_is_fixed(&self, is_fixed: bool) {
        self.is_fixed.set(is_fixed);
    }

    pub fn maybe_implied_arity(&self) -> Option<usize> {
        self.implied_arity.get()
    }

    pub fn set_implied_arity(&self, implied_arity: Option<usize>) {
        self.implied_arity.set(implied_arity);
    }
}

bitflags! {
    pub struct InferenceFlags: u32 {
        const None = 0;
        const NoDefault = 1 << 0;
        const AnyDefault = 1 << 1;
        const SkippedGenericFunction = 1 << 2;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Ternary {
    False = 0,
    Unknown = 1,
    Maybe = 3,
    True = -1,
}

impl TryFrom<i32> for Ternary {
    type Error = ();

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            value if value == Ternary::False as i32 => Ok(Ternary::False),
            value if value == Ternary::Unknown as i32 => Ok(Ternary::Unknown),
            value if value == Ternary::Maybe as i32 => Ok(Ternary::Maybe),
            value if value == Ternary::True as i32 => Ok(Ternary::True),
            _ => Err(()),
        }
    }
}

impl BitAnd for Ternary {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        (self as i32 & rhs as i32).try_into().unwrap()
    }
}

impl BitAndAssign for Ternary {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

pub trait TypeComparer: Trace + Finalize {
    fn call(&self, s: &Type, t: &Type, report_errors: Option<bool>) -> Ternary;
}

#[derive(Trace, Finalize)]
pub struct InferenceContext {
    inferences: GcCell<Vec<Gc<InferenceInfo>>>,
    pub signature: Option<Gc<Signature>>,
    #[unsafe_ignore_trace]
    flags: Cell<InferenceFlags>,
    pub compare_types: Gc<Box<dyn TypeComparer>>,
    mapper: GcCell<Option<Gc<TypeMapper>>>,
    non_fixing_mapper: GcCell<Option<Gc<TypeMapper>>>,
    return_mapper: GcCell<Option<Gc<TypeMapper>>>,
    inferred_type_parameters: GcCell<Option<Vec<Gc<Type /*TypeParameter*/>>>>,
}

impl InferenceContext {
    pub fn new(
        inferences: Vec<Gc<InferenceInfo>>,
        signature: Option<Gc<Signature>>,
        flags: InferenceFlags,
        compare_types: Gc<Box<dyn TypeComparer>>,
        mapper: Option<Gc<TypeMapper>>,
        non_fixing_mapper: Option<Gc<TypeMapper>>,
        return_mapper: Option<Gc<TypeMapper>>,
        inferred_type_parameters: Option<Vec<Gc<Type>>>,
    ) -> Self {
        Self {
            inferences: GcCell::new(inferences),
            signature,
            flags: Cell::new(flags),
            compare_types,
            mapper: GcCell::new(mapper),
            non_fixing_mapper: GcCell::new(non_fixing_mapper),
            return_mapper: GcCell::new(return_mapper),
            inferred_type_parameters: GcCell::new(inferred_type_parameters),
        }
    }

    pub fn inferences(&self) -> GcCellRef<Vec<Gc<InferenceInfo>>> {
        self.inferences.borrow()
    }

    pub fn inferences_mut(&self) -> GcCellRefMut<Vec<Gc<InferenceInfo>>> {
        self.inferences.borrow_mut()
    }

    pub fn flags(&self) -> InferenceFlags {
        self.flags.get()
    }

    pub fn set_flags(&self, flags: InferenceFlags) {
        self.flags.set(flags);
    }

    pub fn mapper(&self) -> Gc<TypeMapper> {
        self.mapper.borrow().clone().unwrap()
    }

    pub fn set_mapper(&self, mapper: Gc<TypeMapper>) {
        *self.mapper.borrow_mut() = Some(mapper);
    }

    pub fn non_fixing_mapper(&self) -> Gc<TypeMapper> {
        self.non_fixing_mapper.borrow().clone().unwrap()
    }

    pub fn set_non_fixing_mapper(&self, non_fixing_mapper: Gc<TypeMapper>) {
        *self.non_fixing_mapper.borrow_mut() = Some(non_fixing_mapper);
    }

    pub fn maybe_return_mapper(&self) -> Option<Gc<TypeMapper>> {
        self.return_mapper.borrow().clone()
    }

    pub fn set_return_mapper(&self, return_mapper: Option<Gc<TypeMapper>>) {
        *self.return_mapper.borrow_mut() = return_mapper;
    }

    pub fn maybe_inferred_type_parameters(&self) -> GcCellRef<Option<Vec<Gc<Type>>>> {
        self.inferred_type_parameters.borrow()
    }

    pub fn maybe_inferred_type_parameters_mut(&self) -> GcCellRefMut<Option<Vec<Gc<Type>>>> {
        self.inferred_type_parameters.borrow_mut()
    }
}

impl fmt::Debug for InferenceContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("InferenceContext")
            .field("inference", &self.inferences)
            .field("signature", &self.signature)
            .field("flags", &self.flags)
            .field("mapper", &self.mapper)
            .field("non_fixing_mapper", &self.non_fixing_mapper)
            .field("return_mapper", &self.return_mapper)
            .field("inferred_type_parameters", &self.inferred_type_parameters)
            .finish()
    }
}

pub(crate) struct WideningContext {
    pub parent: Option<Rc<RefCell<WideningContext>>>,
    pub property_name: Option<__String>,
    pub siblings: Option<Vec<Gc<Type>>>,
    pub resolved_properties: Option<Vec<Gc<Symbol>>>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AssignmentDeclarationKind {
    None,
    ExportsProperty,
    ModuleExports,
    PrototypeProperty,
    ThisProperty,
    Property,
    Prototype,
    ObjectDefinePropertyValue,
    ObjectDefinePropertyExports,
    ObjectDefinePrototypeProperty,
}

pub struct FileExtensionInfo {
    pub extension: String,
    pub is_mixed_content: bool,
    pub script_kind: Option<ScriptKind>,
}

#[derive(Clone, Debug)]
pub struct DiagnosticMessage {
    pub key: &'static str,
    pub category: DiagnosticCategory,
    pub code: u32,
    pub message: Cow<'static, str>,
    pub(crate) elided_in_compatability_pyramid: Arc<Mutex<Option<bool>>>,
}

impl DiagnosticMessage {
    pub fn new(
        code: u32,
        category: DiagnosticCategory,
        key: &'static str,
        message: Cow<'static, str>,
        elided_in_compatability_pyramid: Option<bool>,
    ) -> Self {
        Self {
            code,
            category,
            key,
            message,
            elided_in_compatability_pyramid: Arc::new(Mutex::new(elided_in_compatability_pyramid)),
        }
    }

    pub fn maybe_elided_in_compatability_pyramid(&self) -> Option<bool> {
        self.elided_in_compatability_pyramid.lock().unwrap().clone()
    }

    pub fn set_elided_in_compatability_pyramid(
        &self,
        elided_in_compatability_pyramid: Option<bool>,
    ) {
        *self.elided_in_compatability_pyramid.lock().unwrap() = elided_in_compatability_pyramid;
    }
}

impl PartialEq for DiagnosticMessage {
    fn eq(&self, other: &DiagnosticMessage) -> bool {
        self.code == other.code
    }
}

impl Eq for DiagnosticMessage {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DiagnosticMessageChain {
    pub message_text: String,
    pub category: DiagnosticCategory,
    pub code: u32,
    pub next: Option<Vec<DiagnosticMessageChain>>,
}

impl DiagnosticMessageChain {
    pub fn new(
        message_text: String,
        category: DiagnosticCategory,
        code: u32,
        next: Option<Vec<DiagnosticMessageChain>>,
    ) -> Self {
        Self {
            message_text,
            category,
            code,
            next,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Trace, Finalize)]
pub enum Diagnostic {
    DiagnosticWithLocation(DiagnosticWithLocation),
    DiagnosticWithDetachedLocation(DiagnosticWithDetachedLocation),
    BaseDiagnostic(BaseDiagnostic),
}

impl Diagnostic {
    pub fn as_diagnostic_with_detached_location(&self) -> &DiagnosticWithDetachedLocation {
        enum_unwrapped!(self, [Diagnostic, DiagnosticWithDetachedLocation])
    }
}

pub trait DiagnosticInterface: DiagnosticRelatedInformationInterface {
    fn maybe_related_information(&self)
        -> GcCellRef<Option<Vec<Gc<DiagnosticRelatedInformation>>>>;
    fn maybe_related_information_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<DiagnosticRelatedInformation>>>>;
    fn maybe_skipped_on(&self) -> Ref<Option<String>>;
    fn maybe_skipped_on_mut(&self) -> RefMut<Option<String>>;
}

#[derive(Clone, Debug, Eq, PartialEq, Trace, Finalize)]
pub struct BaseDiagnostic {
    _diagnostic_related_information: BaseDiagnosticRelatedInformation,
    related_information: GcCell<Option<Vec<Gc<DiagnosticRelatedInformation>>>>,
    #[unsafe_ignore_trace]
    skipped_on: RefCell<Option<String /*keyof CompilerOptions*/>>,
}

impl BaseDiagnostic {
    pub fn new(
        diagnostic_related_information: BaseDiagnosticRelatedInformation,
        related_information: Option<Vec<Gc<DiagnosticRelatedInformation>>>,
    ) -> Self {
        Self {
            _diagnostic_related_information: diagnostic_related_information,
            related_information: GcCell::new(related_information),
            skipped_on: Default::default(),
        }
    }
}

impl DiagnosticRelatedInformationInterface for BaseDiagnostic {
    fn maybe_as_diagnostic(&self) -> Option<&Diagnostic> {
        panic!("Shouldn't call maybe_as_diagnostic() on BaseDiagnostic")
    }

    fn category(&self) -> DiagnosticCategory {
        self._diagnostic_related_information.category()
    }

    fn set_category(&self, category: DiagnosticCategory) {
        self._diagnostic_related_information.set_category(category)
    }

    fn code(&self) -> u32 {
        self._diagnostic_related_information.code()
    }

    fn maybe_file(&self) -> Option<Gc<Node>> {
        self._diagnostic_related_information.maybe_file()
    }

    fn maybe_start(&self) -> Option<isize> {
        self._diagnostic_related_information.maybe_start()
    }

    fn start(&self) -> isize {
        self._diagnostic_related_information.start()
    }

    fn set_start(&self, start: Option<isize>) {
        self._diagnostic_related_information.set_start(start);
    }

    fn maybe_length(&self) -> Option<isize> {
        self._diagnostic_related_information.maybe_length()
    }

    fn length(&self) -> isize {
        self._diagnostic_related_information.length()
    }

    fn set_length(&self, length: Option<isize>) {
        self._diagnostic_related_information.set_length(length);
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        self._diagnostic_related_information.message_text()
    }
}

impl DiagnosticInterface for BaseDiagnostic {
    fn maybe_related_information(
        &self,
    ) -> GcCellRef<Option<Vec<Gc<DiagnosticRelatedInformation>>>> {
        self.related_information.borrow()
    }

    fn maybe_related_information_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<DiagnosticRelatedInformation>>>> {
        self.related_information.borrow_mut()
    }

    fn maybe_skipped_on(&self) -> Ref<Option<String>> {
        self.skipped_on.borrow()
    }

    fn maybe_skipped_on_mut(&self) -> RefMut<Option<String>> {
        self.skipped_on.borrow_mut()
    }
}

impl From<BaseDiagnostic> for Diagnostic {
    fn from(base_diagnostic: BaseDiagnostic) -> Self {
        Diagnostic::BaseDiagnostic(base_diagnostic)
    }
}

impl From<BaseDiagnostic> for DiagnosticRelatedInformation {
    fn from(base_diagnostic: BaseDiagnostic) -> Self {
        DiagnosticRelatedInformation::Diagnostic(Diagnostic::BaseDiagnostic(base_diagnostic))
    }
}

impl DiagnosticRelatedInformationInterface for Diagnostic {
    fn maybe_as_diagnostic(&self) -> Option<&Diagnostic> {
        Some(self)
    }

    fn category(&self) -> DiagnosticCategory {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.category(),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => diagnostic.category(),
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.category(),
        }
    }

    fn set_category(&self, category: DiagnosticCategory) {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.set_category(category),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => {
                diagnostic.set_category(category)
            }
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.set_category(category),
        }
    }

    fn code(&self) -> u32 {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.code(),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => diagnostic.code(),
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.code(),
        }
    }

    fn maybe_file(&self) -> Option<Gc<Node>> {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.maybe_file(),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => diagnostic.maybe_file(),
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.maybe_file(),
        }
    }

    fn maybe_start(&self) -> Option<isize> {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.maybe_start(),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => diagnostic.maybe_start(),
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.maybe_start(),
        }
    }

    fn start(&self) -> isize {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.start(),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => diagnostic.start(),
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.start(),
        }
    }

    fn set_start(&self, start: Option<isize>) {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.set_start(start),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => diagnostic.set_start(start),
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.set_start(start),
        }
    }

    fn maybe_length(&self) -> Option<isize> {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.maybe_length(),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => diagnostic.maybe_length(),
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.maybe_length(),
        }
    }

    fn length(&self) -> isize {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.length(),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => diagnostic.length(),
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.length(),
        }
    }

    fn set_length(&self, length: Option<isize>) {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.set_length(length),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => diagnostic.set_length(length),
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.set_length(length),
        }
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.message_text(),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => diagnostic.message_text(),
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.message_text(),
        }
    }
}

impl DiagnosticInterface for Diagnostic {
    fn maybe_related_information(
        &self,
    ) -> GcCellRef<Option<Vec<Gc<DiagnosticRelatedInformation>>>> {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => {
                diagnostic.maybe_related_information()
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => {
                diagnostic.maybe_related_information()
            }
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.maybe_related_information(),
        }
    }

    fn maybe_related_information_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<DiagnosticRelatedInformation>>>> {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => {
                diagnostic.maybe_related_information_mut()
            }
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => {
                diagnostic.maybe_related_information_mut()
            }
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.maybe_related_information_mut(),
        }
    }

    fn maybe_skipped_on(&self) -> Ref<Option<String>> {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.maybe_skipped_on(),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => diagnostic.maybe_skipped_on(),
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.maybe_skipped_on(),
        }
    }

    fn maybe_skipped_on_mut(&self) -> RefMut<Option<String>> {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.maybe_skipped_on_mut(),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => {
                diagnostic.maybe_skipped_on_mut()
            }
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.maybe_skipped_on_mut(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DiagnosticMessageText {
    String(String),
    DiagnosticMessageChain(DiagnosticMessageChain),
}

impl From<String> for DiagnosticMessageText {
    fn from(string: String) -> Self {
        DiagnosticMessageText::String(string)
    }
}

impl From<DiagnosticMessageChain> for DiagnosticMessageText {
    fn from(diagnostic_message_chain: DiagnosticMessageChain) -> Self {
        DiagnosticMessageText::DiagnosticMessageChain(diagnostic_message_chain)
    }
}

pub trait DiagnosticRelatedInformationInterface {
    fn maybe_as_diagnostic(&self) -> Option<&Diagnostic>;
    fn category(&self) -> DiagnosticCategory;
    fn set_category(&self, category: DiagnosticCategory);
    fn code(&self) -> u32;
    fn maybe_file(&self) -> Option<Gc<Node>>;
    fn maybe_start(&self) -> Option<isize>;
    fn start(&self) -> isize;
    fn set_start(&self, start: Option<isize>);
    fn maybe_length(&self) -> Option<isize>;
    fn length(&self) -> isize;
    fn set_length(&self, length: Option<isize>);
    fn message_text(&self) -> &DiagnosticMessageText;
}

#[derive(Clone, Debug, Eq, PartialEq, Trace, Finalize)]
pub enum DiagnosticRelatedInformation {
    BaseDiagnosticRelatedInformation(BaseDiagnosticRelatedInformation),
    Diagnostic(Diagnostic),
}

impl DiagnosticRelatedInformation {
    pub fn as_diagnostic_with_detached_location(&self) -> &DiagnosticWithDetachedLocation {
        enum_unwrapped!(
            self,
            [
                DiagnosticRelatedInformation,
                Diagnostic,
                DiagnosticWithDetachedLocation
            ]
        )
    }
}

impl DiagnosticRelatedInformationInterface for DiagnosticRelatedInformation {
    fn maybe_as_diagnostic(&self) -> Option<&Diagnostic> {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.maybe_as_diagnostic(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => {
                diagnostic.maybe_as_diagnostic()
            }
        }
    }

    fn category(&self) -> DiagnosticCategory {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.category(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.category(),
        }
    }

    fn set_category(&self, category: DiagnosticCategory) {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.set_category(category),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => {
                diagnostic.set_category(category)
            }
        }
    }

    fn code(&self) -> u32 {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.code(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.code(),
        }
    }

    fn maybe_file(&self) -> Option<Gc<Node>> {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.maybe_file(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.maybe_file(),
        }
    }

    fn maybe_start(&self) -> Option<isize> {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.maybe_start(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.maybe_start(),
        }
    }

    fn start(&self) -> isize {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.start(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.start(),
        }
    }

    fn set_start(&self, start: Option<isize>) {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.set_start(start),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.set_start(start),
        }
    }

    fn maybe_length(&self) -> Option<isize> {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.maybe_length(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.maybe_length(),
        }
    }

    fn length(&self) -> isize {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.length(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.length(),
        }
    }

    fn set_length(&self, length: Option<isize>) {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.set_length(length),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.set_length(length),
        }
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.message_text(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.message_text(),
        }
    }
}

impl From<Diagnostic> for DiagnosticRelatedInformation {
    fn from(value: Diagnostic) -> Self {
        Self::Diagnostic(value)
    }
}

#[derive(Builder, Clone, Debug, Trace, Finalize)]
#[builder(setter(into))]
pub struct BaseDiagnosticRelatedInformation {
    #[unsafe_ignore_trace]
    category: Cell<DiagnosticCategory>,
    code: u32,
    #[builder(default)]
    file: Option<Gc<Node /*SourceFile*/>>,
    #[builder(default, setter(custom))]
    #[unsafe_ignore_trace]
    start: Cell<Option<isize>>,
    #[builder(default, setter(custom))]
    #[unsafe_ignore_trace]
    length: Cell<Option<isize>>,
    #[unsafe_ignore_trace]
    message_text: DiagnosticMessageText,
}

impl Eq for BaseDiagnosticRelatedInformation {}

impl PartialEq for BaseDiagnosticRelatedInformation {
    fn eq(&self, other: &Self) -> bool {
        self.category == other.category
            && self.code == other.code
            && are_option_gcs_equal(self.file.as_ref(), other.file.as_ref())
            && self.start == other.start
            && self.length == other.length
            && self.message_text == other.message_text
    }
}

impl BaseDiagnosticRelatedInformation {
    pub fn new(
        category: DiagnosticCategory,
        code: u32,
        file: Option<Gc<Node>>,
        start: Option<isize>,
        length: Option<isize>,
        message_text: impl Into<DiagnosticMessageText>,
    ) -> Self {
        Self {
            category: Cell::new(category),
            code,
            file,
            start: Cell::new(start),
            length: Cell::new(length),
            message_text: message_text.into(),
        }
    }
}

impl DiagnosticRelatedInformationInterface for BaseDiagnosticRelatedInformation {
    fn maybe_as_diagnostic(&self) -> Option<&Diagnostic> {
        None
    }

    fn category(&self) -> DiagnosticCategory {
        self.category.get()
    }

    fn set_category(&self, category: DiagnosticCategory) {
        self.category.set(category)
    }

    fn code(&self) -> u32 {
        self.code
    }

    fn maybe_file(&self) -> Option<Gc<Node>> {
        self.file.clone()
    }

    fn maybe_start(&self) -> Option<isize> {
        self.start.get()
    }

    fn start(&self) -> isize {
        self.start.get().unwrap()
    }

    fn set_start(&self, start: Option<isize>) {
        self.start.set(start);
    }

    fn maybe_length(&self) -> Option<isize> {
        self.length.get()
    }

    fn length(&self) -> isize {
        self.length.get().unwrap()
    }

    fn set_length(&self, length: Option<isize>) {
        self.length.set(length);
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        &self.message_text
    }
}

impl BaseDiagnosticRelatedInformationBuilder {
    pub fn start(&mut self, value: impl Into<Option<isize>>) -> &mut Self {
        let value = value.into();
        self.start = Some(Cell::new(value));
        self
    }

    pub fn length(&mut self, value: impl Into<Option<isize>>) -> &mut Self {
        let value = value.into();
        self.length = Some(Cell::new(value));
        self
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Trace, Finalize)]
pub struct DiagnosticWithLocation {
    _diagnostic: BaseDiagnostic,
}

impl DiagnosticWithLocation {
    pub fn new(base_diagnostic: BaseDiagnostic) -> Self {
        Self {
            _diagnostic: base_diagnostic,
        }
    }

    pub fn file(&self) -> Gc<Node> {
        self.maybe_file().unwrap()
    }
}

impl DiagnosticRelatedInformationInterface for DiagnosticWithLocation {
    fn maybe_as_diagnostic(&self) -> Option<&Diagnostic> {
        panic!("Shouldn't call maybe_as_diagnostic() on DiagnosticWithLocation")
    }

    fn category(&self) -> DiagnosticCategory {
        self._diagnostic.category()
    }

    fn set_category(&self, category: DiagnosticCategory) {
        self._diagnostic.set_category(category)
    }

    fn code(&self) -> u32 {
        self._diagnostic.code()
    }

    fn maybe_file(&self) -> Option<Gc<Node>> {
        self._diagnostic.maybe_file()
    }

    fn maybe_start(&self) -> Option<isize> {
        self._diagnostic.maybe_start()
    }

    fn start(&self) -> isize {
        self._diagnostic.start()
    }

    fn set_start(&self, start: Option<isize>) {
        self._diagnostic.set_start(start)
    }

    fn maybe_length(&self) -> Option<isize> {
        self._diagnostic.maybe_length()
    }

    fn length(&self) -> isize {
        self._diagnostic.length()
    }

    fn set_length(&self, length: Option<isize>) {
        self._diagnostic.set_length(length)
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        self._diagnostic.message_text()
    }
}

impl DiagnosticInterface for DiagnosticWithLocation {
    fn maybe_related_information(
        &self,
    ) -> GcCellRef<Option<Vec<Gc<DiagnosticRelatedInformation>>>> {
        self._diagnostic.maybe_related_information()
    }

    fn maybe_related_information_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<DiagnosticRelatedInformation>>>> {
        self._diagnostic.maybe_related_information_mut()
    }

    fn maybe_skipped_on(&self) -> Ref<Option<String>> {
        self._diagnostic.maybe_skipped_on()
    }

    fn maybe_skipped_on_mut(&self) -> RefMut<Option<String>> {
        self._diagnostic.maybe_skipped_on_mut()
    }
}

impl From<DiagnosticWithLocation> for Diagnostic {
    fn from(diagnostic_with_location: DiagnosticWithLocation) -> Self {
        Diagnostic::DiagnosticWithLocation(diagnostic_with_location)
    }
}

impl From<DiagnosticWithLocation> for Gc<Diagnostic> {
    fn from(diagnostic_with_location: DiagnosticWithLocation) -> Self {
        Gc::new(diagnostic_with_location.into())
    }
}

impl From<DiagnosticWithLocation> for DiagnosticRelatedInformation {
    fn from(diagnostic_with_location: DiagnosticWithLocation) -> Self {
        DiagnosticRelatedInformation::Diagnostic(Diagnostic::DiagnosticWithLocation(
            diagnostic_with_location,
        ))
    }
}

impl From<DiagnosticWithLocation> for Gc<DiagnosticRelatedInformation> {
    fn from(diagnostic_with_location: DiagnosticWithLocation) -> Self {
        Gc::new(diagnostic_with_location.into())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Trace, Finalize)]
pub struct DiagnosticWithDetachedLocation {
    _diagnostic: BaseDiagnostic,
    pub file_name: String,
}

impl DiagnosticWithDetachedLocation {
    pub fn new(base_diagnostic: BaseDiagnostic, file_name: String) -> Self {
        Self {
            _diagnostic: base_diagnostic,
            file_name,
        }
    }
}

impl DiagnosticRelatedInformationInterface for DiagnosticWithDetachedLocation {
    fn maybe_as_diagnostic(&self) -> Option<&Diagnostic> {
        panic!("Shouldn't call maybe_as_diagnostic() on DiagnosticWithDetachedLocation")
    }

    fn category(&self) -> DiagnosticCategory {
        self._diagnostic.category()
    }

    fn set_category(&self, category: DiagnosticCategory) {
        self._diagnostic.set_category(category)
    }

    fn code(&self) -> u32 {
        self._diagnostic.code()
    }

    fn maybe_file(&self) -> Option<Gc<Node>> {
        self._diagnostic.maybe_file()
    }

    fn maybe_start(&self) -> Option<isize> {
        self._diagnostic.maybe_start()
    }

    fn start(&self) -> isize {
        self._diagnostic.start()
    }

    fn set_start(&self, start: Option<isize>) {
        self._diagnostic.set_start(start)
    }

    fn maybe_length(&self) -> Option<isize> {
        self._diagnostic.maybe_length()
    }

    fn length(&self) -> isize {
        self._diagnostic.length()
    }

    fn set_length(&self, length: Option<isize>) {
        self._diagnostic.set_length(length)
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        self._diagnostic.message_text()
    }
}

impl DiagnosticInterface for DiagnosticWithDetachedLocation {
    fn maybe_related_information(
        &self,
    ) -> GcCellRef<Option<Vec<Gc<DiagnosticRelatedInformation>>>> {
        self._diagnostic.maybe_related_information()
    }

    fn maybe_related_information_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<DiagnosticRelatedInformation>>>> {
        self._diagnostic.maybe_related_information_mut()
    }

    fn maybe_skipped_on(&self) -> Ref<Option<String>> {
        self._diagnostic.maybe_skipped_on()
    }

    fn maybe_skipped_on_mut(&self) -> RefMut<Option<String>> {
        self._diagnostic.maybe_skipped_on_mut()
    }
}

impl From<DiagnosticWithDetachedLocation> for Diagnostic {
    fn from(diagnostic_with_detached_location: DiagnosticWithDetachedLocation) -> Self {
        Diagnostic::DiagnosticWithDetachedLocation(diagnostic_with_detached_location)
    }
}

impl From<DiagnosticWithDetachedLocation> for DiagnosticRelatedInformation {
    fn from(diagnostic_with_detached_location: DiagnosticWithDetachedLocation) -> Self {
        DiagnosticRelatedInformation::Diagnostic(Diagnostic::DiagnosticWithDetachedLocation(
            diagnostic_with_detached_location,
        ))
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DiagnosticCategory {
    Warning,
    Error,
    Suggestion,
    Message,
}

pub fn diagnostic_category_name(category: DiagnosticCategory, lower_case: Option<bool>) -> String {
    let lower_case = lower_case.unwrap_or(true);
    let name = match category {
        DiagnosticCategory::Warning => "Warning",
        DiagnosticCategory::Error => "Error",
        DiagnosticCategory::Suggestion => "Suggestion",
        DiagnosticCategory::Message => "Message",
    };
    if lower_case {
        name.to_lowercase()
    } else {
        name.to_owned()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum ModuleResolutionKind {
    Classic = 1,
    NodeJs = 2,
    Node12 = 3,
    NodeNext = 99,
}
