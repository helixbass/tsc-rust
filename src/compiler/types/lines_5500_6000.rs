#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use serde::Serialize;
use std::borrow::Cow;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::ops::{BitAnd, BitAndAssign};
use std::rc::{Rc, Weak};
use std::sync::{Arc, Mutex};

use super::{
    BaseObjectType, BaseType, BaseUnionOrIntersectionType, Node, ObjectFlagsTypeInterface,
    ObjectTypeInterface, ResolvableTypeInterface, Symbol, SymbolTable, Type, TypeChecker,
    TypePredicate,
};
use crate::{Debug_, ObjectFlags, ScriptKind, TypeFlags};
use local_macros::{enum_unwrapped, type_type};

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "UnionOrIntersectionType",
    interfaces = "UnionOrIntersectionTypeInterface, ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface"
)]
pub struct IntersectionType {
    _union_or_intersection_type: BaseUnionOrIntersectionType,
    resolved_apparent_type: RefCell<Option<Rc<Type>>>,
}

impl IntersectionType {
    pub fn new(union_or_intersection_type: BaseUnionOrIntersectionType) -> Self {
        Self {
            _union_or_intersection_type: union_or_intersection_type,
            resolved_apparent_type: RefCell::new(None),
        }
    }

    pub(crate) fn maybe_resolved_apparent_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.resolved_apparent_type.borrow_mut()
    }
}

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "ObjectType",
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface"
)]
pub struct MappedType {
    _object_type: BaseObjectType,
    pub declaration: Rc<Node /*MappedTypeNode*/>,
    type_parameter: RefCell<Option<Rc<Type /*TypeParameter*/>>>,
    constraint_type: RefCell<Option<Rc<Type>>>,
    name_type: RefCell<Option<Rc<Type>>>,
    template_type: RefCell<Option<Rc<Type>>>,
    modifiers_type: RefCell<Option<Rc<Type>>>,
    resolved_apparent_type: RefCell<Option<Rc<Type>>>,
    contains_error: Cell<Option<bool>>,
}

impl MappedType {
    pub fn new(object_type: BaseObjectType, declaration: Rc<Node>) -> Self {
        Self {
            _object_type: object_type,
            declaration,
            type_parameter: RefCell::new(None),
            constraint_type: RefCell::new(None),
            name_type: RefCell::new(None),
            template_type: RefCell::new(None),
            modifiers_type: RefCell::new(None),
            resolved_apparent_type: RefCell::new(None),
            contains_error: Cell::new(None),
        }
    }

    pub fn maybe_type_parameter(&self) -> RefMut<Option<Rc<Type>>> {
        self.type_parameter.borrow_mut()
    }

    pub fn maybe_constraint_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.constraint_type.borrow_mut()
    }

    pub fn maybe_name_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.name_type.borrow_mut()
    }

    pub fn maybe_template_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.template_type.borrow_mut()
    }

    pub fn maybe_modifiers_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.modifiers_type.borrow_mut()
    }

    pub fn maybe_resolved_apparent_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.resolved_apparent_type.borrow_mut()
    }

    pub fn maybe_contains_error(&self) -> Option<bool> {
        self.contains_error.get()
    }

    pub fn set_contains_error(&self, contains_error: Option<bool>) {
        self.contains_error.set(contains_error);
    }
}

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "ObjectType",
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface"
)]
pub struct ReverseMappedType {
    _object_type: BaseObjectType,
    pub source: Rc<Type>,
    pub mapped_type: Rc<Type /*MappedType*/>,
    pub constraint_type: Rc<Type /*IndexType*/>,
}

pub trait ResolvedTypeInterface:
    ObjectFlagsTypeInterface + ObjectTypeInterface + ResolvableTypeInterface
{
    fn members(&self) -> Rc<RefCell<SymbolTable>>;
    fn properties(&self) -> RefMut<Vec<Rc<Symbol>>>;
    fn set_properties(&self, properties: Vec<Rc<Symbol>>);
    fn call_signatures(&self) -> Ref<Vec<Rc<Signature>>>;
    fn set_call_signatures(&self, call_signatures: Vec<Rc<Signature>>);
    fn construct_signatures(&self) -> Ref<Vec<Rc<Signature>>>;
    fn set_construct_signatures(&self, construct_signatures: Vec<Rc<Signature>>);
    fn index_infos(&self) -> Ref<Vec<Rc<IndexInfo>>>;
    fn maybe_object_type_without_abstract_construct_signatures(&self) -> Option<Rc<Type>>;
    fn set_object_type_without_abstract_construct_signatures(
        &self,
        object_type_without_abstract_construct_signatures: Option<Rc<Type>>,
    );
}

#[derive(Debug)]
pub(crate) struct IterationTypes {
    yield_type: Option<Rc<Type>>,
    return_type: Option<Rc<Type>>,
    next_type: Option<Rc<Type>>,
}

impl IterationTypes {
    pub fn new(yield_type: Rc<Type>, return_type: Rc<Type>, next_type: Rc<Type>) -> Self {
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

    pub fn yield_type(&self) -> Rc<Type> {
        let cloned = self.yield_type.clone();
        if cloned.is_none() {
            Debug_.fail(Some("Not supported"));
        }
        cloned.unwrap()
    }

    pub fn return_type(&self) -> Rc<Type> {
        let cloned = self.return_type.clone();
        if cloned.is_none() {
            Debug_.fail(Some("Not supported"));
        }
        cloned.unwrap()
    }

    pub fn next_type(&self) -> Rc<Type> {
        let cloned = self.next_type.clone();
        if cloned.is_none() {
            Debug_.fail(Some("Not supported"));
        }
        cloned.unwrap()
    }
}

#[derive(Clone, Debug)]
#[type_type]
pub struct TypeParameter {
    _type: BaseType,
    pub constraint: RefCell<Option<Weak<Type>>>, // TODO: is it correct that this is weak?
    pub default: RefCell<Option<Rc<Type>>>,
    pub target: Option<Rc<Type /*TypeParameter*/>>,
    pub mapper: RefCell<Option<TypeMapper>>,
    pub is_this_type: Option<bool>,
}

impl TypeParameter {
    pub fn new(base_type: BaseType) -> Self {
        Self {
            _type: base_type,
            constraint: RefCell::new(None),
            default: RefCell::new(None),
            target: None,
            mapper: RefCell::new(None),
            is_this_type: None,
        }
    }

    pub fn maybe_constraint(&self) -> Option<Rc<Type>> {
        self.constraint
            .borrow()
            .as_ref()
            .map(|weak| weak.upgrade().unwrap())
    }

    pub fn set_constraint(&self, constraint: Rc<Type>) {
        *self.constraint.borrow_mut() = Some(Rc::downgrade(&constraint));
    }

    pub fn maybe_default(&self) -> RefMut<Option<Rc<Type>>> {
        self.default.borrow_mut()
    }

    pub fn maybe_mapper(&self) -> Ref<Option<TypeMapper>> {
        self.mapper.borrow()
    }

    pub fn set_mapper(&self, mapper: TypeMapper) {
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

#[derive(Clone, Debug)]
#[type_type]
pub struct IndexedAccessType {
    _type: BaseType,
    pub object_type: Rc<Type>,
    pub index_type: Rc<Type>,
    pub(crate) access_flags: AccessFlags,
    pub(crate) constraint: Option<Rc<Type>>,
    simplified_for_reading: RefCell<Option<Rc<Type>>>,
    simplified_for_writing: RefCell<Option<Rc<Type>>>,
}

impl IndexedAccessType {
    pub fn new(
        base_type: BaseType,
        object_type: Rc<Type>,
        index_type: Rc<Type>,
        access_flags: AccessFlags,
    ) -> Self {
        Self {
            _type: base_type,
            object_type,
            index_type,
            access_flags,
            constraint: None,
            simplified_for_reading: RefCell::new(None),
            simplified_for_writing: RefCell::new(None),
        }
    }

    pub fn maybe_simplified_for_reading(&self) -> RefMut<Option<Rc<Type>>> {
        self.simplified_for_reading.borrow_mut()
    }

    pub fn maybe_simplified_for_writing(&self) -> RefMut<Option<Rc<Type>>> {
        self.simplified_for_writing.borrow_mut()
    }
}

#[derive(Clone, Debug)]
#[type_type]
pub struct IndexType {
    _type: BaseType,
    pub type_: Rc<Type /*InstantiableType | UnionOrIntersectionType*/>,
    pub(crate) strings_only: bool,
}

impl IndexType {
    pub fn new(_type: BaseType, type_: Rc<Type>, strings_only: bool) -> Self {
        Self {
            _type,
            type_,
            strings_only,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ConditionalRoot {
    pub node: Rc<Node /*ConditionalTypeNode*/>,
    pub check_type: Rc<Type>,
    pub extends_type: Rc<Type>,
    pub is_distributive: bool,
    pub infer_type_parameters: Option<Vec<Rc<Type /*TypeParameter*/>>>,
    pub outer_type_parameters: Option<Vec<Rc<Type /*TypeParameter*/>>>,
    instantiations: RefCell<Option<HashMap<String, Rc<Type>>>>,
    pub alias_symbol: Option<Rc<Symbol>>,
    pub alias_type_arguments: Option<Vec<Rc<Type>>>,
}

impl ConditionalRoot {
    pub fn new(
        node: Rc<Node>,
        check_type: Rc<Type>,
        extends_type: Rc<Type>,
        is_distributive: bool,
        infer_type_parameters: Option<Vec<Rc<Type>>>,
        outer_type_parameters: Option<Vec<Rc<Type>>>,
        alias_symbol: Option<Rc<Symbol>>,
        alias_type_arguments: Option<Vec<Rc<Type>>>,
    ) -> Self {
        Self {
            node,
            check_type,
            extends_type,
            is_distributive,
            infer_type_parameters,
            outer_type_parameters,
            instantiations: RefCell::new(None),
            alias_symbol,
            alias_type_arguments,
        }
    }

    pub fn maybe_instantiations(&self) -> RefMut<Option<HashMap<String, Rc<Type>>>> {
        self.instantiations.borrow_mut()
    }
}

#[derive(Clone, Debug)]
#[type_type]
pub struct ConditionalType {
    _type: BaseType,
    pub root: ConditionalRoot,
    pub check_type: Rc<Type>,
    pub extends_type: Rc<Type>,
    resolved_true_type: RefCell<Option<Rc<Type>>>,
    resolved_false_type: RefCell<Option<Rc<Type>>>,
    resolved_inferred_true_type: RefCell<Option<Rc<Type>>>,
    resolved_default_constraint: RefCell<Option<Rc<Type>>>,
    pub(crate) mapper: Option<TypeMapper>,
    pub(crate) combined_mapper: Option<TypeMapper>,
}

impl ConditionalType {
    pub fn new(
        base_type: BaseType,
        root: ConditionalRoot,
        check_type: Rc<Type>,
        extends_type: Rc<Type>,
        mapper: Option<TypeMapper>,
        combined_mapper: Option<TypeMapper>,
    ) -> Self {
        Self {
            _type: base_type,
            root,
            check_type,
            extends_type,
            resolved_true_type: RefCell::new(None),
            resolved_false_type: RefCell::new(None),
            resolved_inferred_true_type: RefCell::new(None),
            resolved_default_constraint: RefCell::new(None),
            mapper,
            combined_mapper,
        }
    }

    pub(crate) fn maybe_resolved_true_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.resolved_true_type.borrow_mut()
    }

    pub(crate) fn maybe_resolved_false_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.resolved_false_type.borrow_mut()
    }

    pub(crate) fn maybe_resolved_inferred_true_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.resolved_inferred_true_type.borrow_mut()
    }

    pub(crate) fn maybe_resolved_default_constraint(&self) -> RefMut<Option<Rc<Type>>> {
        self.resolved_default_constraint.borrow_mut()
    }
}

#[derive(Clone, Debug)]
#[type_type]
pub struct TemplateLiteralType {
    _type: BaseType,
    pub texts: Vec<String>,
    pub types: Vec<Rc<Type>>,
}

impl TemplateLiteralType {
    pub fn new(_type: BaseType, texts: Vec<String>, types: Vec<Rc<Type>>) -> Self {
        Self {
            _type,
            texts,
            types,
        }
    }
}

#[derive(Clone, Debug)]
#[type_type]
pub struct StringMappingType {
    _type: BaseType,
    pub type_: Rc<Type>,
}

impl StringMappingType {
    pub fn new(base_type: BaseType, type_: Rc<Type>) -> Self {
        Self {
            _type: base_type,
            type_,
        }
    }
}

#[derive(Clone, Debug)]
#[type_type]
pub struct SubstitutionType {
    _type: BaseType,
    object_flags: Cell<ObjectFlags>,
    pub base_type: Rc<Type>,
    pub substitute: Rc<Type>,
}

impl SubstitutionType {
    pub fn new(_type: BaseType, base_type: Rc<Type>, substitute: Rc<Type>) -> Self {
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

#[derive(Debug)]
pub struct Signature {
    pub flags: SignatureFlags,
    pub declaration: Option<Rc<Node /*SignatureDeclaration | JSDocSignature*/>>,
    pub type_parameters: Option<Vec<Rc<Type /*TypeParameter*/>>>,
    parameters: Option<Vec<Rc<Symbol>>>,
    pub this_parameter: Option<Rc<Symbol>>,
    resolved_return_type: RefCell<Option<Rc<Type>>>,
    resolved_type_predicate: RefCell<Option<Rc<TypePredicate>>>,
    min_argument_count: Option<usize>,
    resolved_min_argument_count: Cell<Option<usize>>,
    pub target: Option<Rc<Signature>>,
    pub mapper: Option<TypeMapper>, // TODO: should this be Rc-wrapped since it gets cloned eg in clone_signature()?
    pub composite_signatures: Option<Vec<Rc<Signature>>>,
    pub composite_kind: Option<TypeFlags>,
    erased_signature_cache: RefCell<Option<Rc<Signature>>>,
    canonical_signature_cache: RefCell<Option<Rc<Signature>>>,
    base_signature_cache: RefCell<Option<Rc<Signature>>>,
    optional_call_signature_cache: RefCell<Option<SignatureOptionalCallSignatureCache>>,
    isolated_signature_type: RefCell<Option<Rc<Type /*ObjectType*/>>>,
    instantiations: RefCell<Option<HashMap<String, Rc<Signature>>>>,
}

impl Signature {
    pub fn new(flags: SignatureFlags) -> Self {
        Self {
            flags,
            declaration: None,
            type_parameters: None,
            parameters: None,
            this_parameter: None,
            resolved_return_type: RefCell::new(None),
            resolved_type_predicate: RefCell::new(None),
            min_argument_count: None,
            resolved_min_argument_count: Cell::new(None),
            target: None,
            mapper: None,
            composite_signatures: None,
            composite_kind: None,
            erased_signature_cache: RefCell::new(None),
            canonical_signature_cache: RefCell::new(None),
            base_signature_cache: RefCell::new(None),
            optional_call_signature_cache: RefCell::new(None),
            isolated_signature_type: RefCell::new(None),
            instantiations: RefCell::new(None),
        }
    }

    pub fn parameters(&self) -> &[Rc<Symbol>] {
        self.parameters.as_ref().unwrap()
    }

    pub fn set_parameters(&mut self, parameters: Vec<Rc<Symbol>>) {
        self.parameters = Some(parameters);
    }

    pub fn maybe_resolved_return_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.resolved_return_type.borrow_mut()
    }

    pub fn maybe_resolved_type_predicate(&self) -> RefMut<Option<Rc<TypePredicate>>> {
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

    pub fn maybe_erased_signature_cache(&self) -> RefMut<Option<Rc<Signature>>> {
        self.erased_signature_cache.borrow_mut()
    }

    pub fn maybe_canonical_signature_cache(&self) -> RefMut<Option<Rc<Signature>>> {
        self.canonical_signature_cache.borrow_mut()
    }

    pub fn maybe_base_signature_cache(&self) -> RefMut<Option<Rc<Signature>>> {
        self.base_signature_cache.borrow_mut()
    }

    pub fn maybe_optional_call_signature_cache(
        &self,
    ) -> RefMut<Option<SignatureOptionalCallSignatureCache>> {
        self.optional_call_signature_cache.borrow_mut()
    }

    pub fn maybe_isolated_signature_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.isolated_signature_type.borrow_mut()
    }

    pub fn maybe_instantiations(&self) -> RefMut<Option<HashMap<String, Rc<Signature>>>> {
        self.instantiations.borrow_mut()
    }
}

#[derive(Debug)]
pub struct SignatureOptionalCallSignatureCache {
    pub inner: Option<Rc<Signature>>,
    pub outer: Option<Rc<Signature>>,
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

#[derive(Debug)]
pub struct IndexInfo {
    pub key_type: Rc<Type>,
    pub type_: Rc<Type>,
    pub is_readonly: bool,
    pub declaration: Option<Rc<Node /*IndexSignatureDeclaration*/>>,
}

#[derive(Clone, Debug)]
pub enum TypeMapper {
    Simple(TypeMapperSimple),
    Array(TypeMapperArray),
    Function(TypeMapperFunction),
    Composite(TypeMapperCompositeOrMerged),
    Merged(TypeMapperCompositeOrMerged),
}

#[derive(Clone, Debug)]
pub struct TypeMapperSimple {
    pub source: Rc<Type>, // TODO: should all of these be weak?
    pub target: Rc<Type>,
}

#[derive(Clone, Debug)]
pub struct TypeMapperArray {
    pub sources: Vec<Rc<Type>>,
    pub targets: Option<Vec<Rc<Type>>>,
}

pub trait TypeMapperCallback {
    fn call(&self, checker: &TypeChecker, type_: &Type) -> Rc<Type>;
}

#[derive(Clone)]
pub struct TypeMapperFunction {
    pub func: Rc<dyn TypeMapperCallback>,
}

impl fmt::Debug for TypeMapperFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("TypeMapperFunction")
    }
}

#[derive(Clone, Debug)]
pub struct TypeMapperCompositeOrMerged {
    pub mapper1: Box<TypeMapper>,
    pub mapper2: Box<TypeMapper>,
}

impl TypeMapper {
    pub fn new_simple(source: Rc<Type>, target: Rc<Type>) -> Self {
        Self::Simple(TypeMapperSimple { source, target })
    }

    pub fn new_array(sources: Vec<Rc<Type>>, targets: Option<Vec<Rc<Type>>>) -> Self {
        Self::Array(TypeMapperArray { sources, targets })
    }

    pub fn new_function<TFunc: 'static + TypeMapperCallback>(func: TFunc) -> Self {
        Self::Function(TypeMapperFunction {
            func: Rc::new(func),
        })
    }

    pub fn new_composite(mapper1: TypeMapper, mapper2: TypeMapper) -> Self {
        Self::Composite(TypeMapperCompositeOrMerged {
            mapper1: Box::new(mapper1),
            mapper2: Box::new(mapper2),
        })
    }

    pub fn new_merged(mapper1: TypeMapper, mapper2: TypeMapper) -> Self {
        Self::Merged(TypeMapperCompositeOrMerged {
            mapper1: Box::new(mapper1),
            mapper2: Box::new(mapper2),
        })
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

pub(crate) struct InferenceInfo {
    pub type_parameter: Rc<Type /*TypeParameter*/>,
}

bitflags! {
    pub(crate) struct InferenceFlags: u32 {
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

pub(crate) struct InferenceContext {
    pub inferences: Vec<Rc<InferenceInfo>>,
    pub mapper: TypeMapper,
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
    ) -> Self {
        Self {
            code,
            category,
            key,
            message,
            elided_in_compatability_pyramid: Arc::new(Mutex::new(None)),
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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
    fn related_information(&self) -> RefMut<Option<Vec<Rc<DiagnosticRelatedInformation>>>>;
    fn maybe_skipped_on(&self) -> RefMut<Option<String>>;
}

#[derive(Clone, Debug)]
pub struct BaseDiagnostic {
    _diagnostic_related_information: BaseDiagnosticRelatedInformation,
    related_information: RefCell<Option<Vec<Rc<DiagnosticRelatedInformation>>>>,
    skipped_on: RefCell<Option<String /*keyof CompilerOptions*/>>,
}

impl BaseDiagnostic {
    pub fn new(
        diagnostic_related_information: BaseDiagnosticRelatedInformation,
        related_information: Option<Vec<Rc<DiagnosticRelatedInformation>>>,
    ) -> Self {
        Self {
            _diagnostic_related_information: diagnostic_related_information,
            related_information: RefCell::new(related_information),
            skipped_on: RefCell::new(None),
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

    fn maybe_file(&self) -> Option<Rc<Node>> {
        self._diagnostic_related_information.maybe_file()
    }

    fn maybe_start(&self) -> Option<isize> {
        self._diagnostic_related_information.maybe_start()
    }

    fn start(&self) -> isize {
        self._diagnostic_related_information.start()
    }

    fn maybe_length(&self) -> Option<isize> {
        self._diagnostic_related_information.maybe_length()
    }

    fn length(&self) -> isize {
        self._diagnostic_related_information.length()
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        self._diagnostic_related_information.message_text()
    }
}

impl DiagnosticInterface for BaseDiagnostic {
    fn related_information(&self) -> RefMut<Option<Vec<Rc<DiagnosticRelatedInformation>>>> {
        self.related_information.borrow_mut()
    }

    fn maybe_skipped_on(&self) -> RefMut<Option<String>> {
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

    fn maybe_file(&self) -> Option<Rc<Node>> {
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

    fn message_text(&self) -> &DiagnosticMessageText {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.message_text(),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => diagnostic.message_text(),
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.message_text(),
        }
    }
}

impl DiagnosticInterface for Diagnostic {
    fn related_information(&self) -> RefMut<Option<Vec<Rc<DiagnosticRelatedInformation>>>> {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.related_information(),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => {
                diagnostic.related_information()
            }
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.related_information(),
        }
    }

    fn maybe_skipped_on(&self) -> RefMut<Option<String>> {
        match self {
            Diagnostic::DiagnosticWithLocation(diagnostic) => diagnostic.maybe_skipped_on(),
            Diagnostic::DiagnosticWithDetachedLocation(diagnostic) => diagnostic.maybe_skipped_on(),
            Diagnostic::BaseDiagnostic(diagnostic) => diagnostic.maybe_skipped_on(),
        }
    }
}

#[derive(Clone, Debug)]
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
    fn maybe_file(&self) -> Option<Rc<Node>>;
    fn maybe_start(&self) -> Option<isize>;
    fn start(&self) -> isize;
    fn maybe_length(&self) -> Option<isize>;
    fn length(&self) -> isize;
    fn message_text(&self) -> &DiagnosticMessageText;
}

#[derive(Clone, Debug)]
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

    fn maybe_file(&self) -> Option<Rc<Node>> {
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

    fn message_text(&self) -> &DiagnosticMessageText {
        match self {
            DiagnosticRelatedInformation::BaseDiagnosticRelatedInformation(
                base_diagnostic_related_information,
            ) => base_diagnostic_related_information.message_text(),
            DiagnosticRelatedInformation::Diagnostic(diagnostic) => diagnostic.message_text(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct BaseDiagnosticRelatedInformation {
    category: Cell<DiagnosticCategory>,
    code: u32,
    file: Option<Weak<Node /*SourceFile*/>>,
    start: Option<isize>,
    length: Option<isize>,
    message_text: DiagnosticMessageText,
}

impl BaseDiagnosticRelatedInformation {
    pub fn new<TDiagnosticMessageText: Into<DiagnosticMessageText>>(
        category: DiagnosticCategory,
        code: u32,
        file: Option<Rc<Node>>,
        start: Option<isize>,
        length: Option<isize>,
        message_text: TDiagnosticMessageText,
    ) -> Self {
        Self {
            category: Cell::new(category),
            code,
            file: file.map(|file| Rc::downgrade(&file)),
            start,
            length,
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

    fn maybe_file(&self) -> Option<Rc<Node>> {
        self.file.as_ref().map(|weak| weak.upgrade().unwrap())
    }

    fn maybe_start(&self) -> Option<isize> {
        self.start
    }

    fn start(&self) -> isize {
        self.start.unwrap()
    }

    fn maybe_length(&self) -> Option<isize> {
        self.length
    }

    fn length(&self) -> isize {
        self.length.unwrap()
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        &self.message_text
    }
}

#[derive(Clone, Debug)]
pub struct DiagnosticWithLocation {
    _diagnostic: BaseDiagnostic,
}

impl DiagnosticWithLocation {
    pub fn new(base_diagnostic: BaseDiagnostic) -> Self {
        Self {
            _diagnostic: base_diagnostic,
        }
    }

    pub fn file(&self) -> Rc<Node> {
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

    fn maybe_file(&self) -> Option<Rc<Node>> {
        self._diagnostic.maybe_file()
    }

    fn maybe_start(&self) -> Option<isize> {
        self._diagnostic.maybe_start()
    }

    fn start(&self) -> isize {
        self._diagnostic.start()
    }

    fn maybe_length(&self) -> Option<isize> {
        self._diagnostic.maybe_length()
    }

    fn length(&self) -> isize {
        self._diagnostic.length()
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        self._diagnostic.message_text()
    }
}

impl DiagnosticInterface for DiagnosticWithLocation {
    fn related_information(&self) -> RefMut<Option<Vec<Rc<DiagnosticRelatedInformation>>>> {
        self._diagnostic.related_information()
    }

    fn maybe_skipped_on(&self) -> RefMut<Option<String>> {
        self._diagnostic.maybe_skipped_on()
    }
}

impl From<DiagnosticWithLocation> for Diagnostic {
    fn from(diagnostic_with_location: DiagnosticWithLocation) -> Self {
        Diagnostic::DiagnosticWithLocation(diagnostic_with_location)
    }
}

impl From<DiagnosticWithLocation> for DiagnosticRelatedInformation {
    fn from(diagnostic_with_location: DiagnosticWithLocation) -> Self {
        DiagnosticRelatedInformation::Diagnostic(Diagnostic::DiagnosticWithLocation(
            diagnostic_with_location,
        ))
    }
}

#[derive(Clone, Debug)]
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

    fn maybe_file(&self) -> Option<Rc<Node>> {
        self._diagnostic.maybe_file()
    }

    fn maybe_start(&self) -> Option<isize> {
        self._diagnostic.maybe_start()
    }

    fn start(&self) -> isize {
        self._diagnostic.start()
    }

    fn maybe_length(&self) -> Option<isize> {
        self._diagnostic.maybe_length()
    }

    fn length(&self) -> isize {
        self._diagnostic.length()
    }

    fn message_text(&self) -> &DiagnosticMessageText {
        self._diagnostic.message_text()
    }
}

impl DiagnosticInterface for DiagnosticWithDetachedLocation {
    fn related_information(&self) -> RefMut<Option<Vec<Rc<DiagnosticRelatedInformation>>>> {
        self._diagnostic.related_information()
    }

    fn maybe_skipped_on(&self) -> RefMut<Option<String>> {
        self._diagnostic.maybe_skipped_on()
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

pub(crate) fn diagnostic_category_name(
    category: DiagnosticCategory,
    lower_case: Option<bool>,
) -> String {
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
