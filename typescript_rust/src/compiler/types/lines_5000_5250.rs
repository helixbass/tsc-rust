use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRefMut, Trace};
use indexmap::IndexMap;
use std::cell::{Cell, RefCell, RefMut};
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

use super::{
    BaseInterfaceType, BigIntLiteralType, ConditionalType, IndexType, IndexedAccessType,
    InterfaceType, InterfaceTypeWithDeclaredMembersInterface, IntersectionType, LiteralType,
    MappedType, NumberLiteralType, ObjectFlagsTypeInterface, ObjectType, ResolvableTypeInterface,
    ResolvedTypeInterface, ReverseMappedType, Signature, StringLiteralType, StringMappingType,
    SubstitutionType, Symbol, TemplateLiteralType, TupleType, TypeParameter, TypeReference,
    UnionOrIntersectionType, UnionOrIntersectionTypeInterface, UnionType, UniqueESSymbolType,
};
use crate::{
    BaseTransientSymbol, EvolvingArrayType, FreshObjectLiteralTypeInterface, GenericTypeInterface,
    InterfaceTypeInterface, IterationTypeCacheKey, IterationTypes, JsxFlags, Node, NodeId,
    NotActuallyInterfaceType, ObjectFlags, Pattern, StringOrNumber, TypeReferenceInterface,
};
use local_macros::{enum_unwrapped, symbol_type, type_type};

#[derive(Debug, Trace, Finalize)]
#[symbol_type(ancestors = "TransientSymbol", interfaces = "TransientSymbolInterface")]
pub struct MappedSymbol {
    _transient_symbol: BaseTransientSymbol,
    pub mapped_type: Gc<Type /*MappedType*/>,
    key_type: GcCell<Gc<Type>>,
}

impl MappedSymbol {
    pub fn new(
        transient_symbol: BaseTransientSymbol,
        mapped_type: Gc<Type>,
        key_type: Gc<Type>,
    ) -> Self {
        Self {
            _transient_symbol: transient_symbol,
            mapped_type,
            key_type: GcCell::new(key_type),
        }
    }

    pub fn key_type(&self) -> Gc<Type> {
        self.key_type.borrow().clone()
    }

    pub fn set_key_type(&self, key_type: Gc<Type>) {
        *self.key_type.borrow_mut() = key_type;
    }
}

#[derive(Debug, Trace, Finalize)]
#[symbol_type(ancestors = "TransientSymbol", interfaces = "TransientSymbolInterface")]
pub struct ReverseMappedSymbol {
    _transient_symbol: BaseTransientSymbol,
    pub property_type: Gc<Type>,
    pub mapped_type: Gc<Type /*MappedType*/>,
    pub constraint_type: Gc<Type /*IndexType*/>,
}

impl ReverseMappedSymbol {
    pub fn new(
        transient_symbol: BaseTransientSymbol,
        property_type: Gc<Type>,
        mapped_type: Gc<Type>,
        constraint_type: Gc<Type>,
    ) -> Self {
        Self {
            _transient_symbol: transient_symbol,
            property_type,
            mapped_type,
            constraint_type,
        }
    }
}

#[allow(non_snake_case)]
pub mod InternalSymbolName {
    pub const Call: &'static str/*__String*/ = "__call";
    pub const Constructor: &'static str/*__String*/ = "__constructor";
    pub const New: &'static str/*__String*/ = "__new";
    pub const Index: &'static str/*__String*/ = "__index";
    pub const ExportStar: &'static str/*__String*/ = "__export";
    pub const Global: &'static str/*__String*/ = "__global";
    pub const Missing: &'static str/*__String*/ = "__missing";
    pub const Type: &'static str/*__String*/ = "__type";
    pub const Object: &'static str/*__String*/ = "__object";
    pub const JSXAttributes: &'static str/*__String*/ = "__jsxAttributes";
    pub const Class: &'static str/*__String*/ = "__class";
    pub const Function: &'static str/*__String*/ = "__function";
    pub const Computed: &'static str/*__String*/ = "__computed";
    pub const Resolving: &'static str/*__String*/ = "__resolving__";
    pub const ExportEquals: &'static str/*__String*/ = "export=";
    pub const Default: &'static str/*__String*/ = "default";
    pub const This: &'static str/*__String*/ = "this";
}

pub type __String = String;

pub type UnderscoreEscapedMap<TValue> = HashMap<__String, TValue>;

pub type SymbolTable = IndexMap<__String, Gc<Symbol>>;

#[derive(Clone, Debug, Trace, Finalize)]
pub struct PatternAmbientModule {
    #[unsafe_ignore_trace]
    pub pattern: Rc<Pattern>,
    pub symbol: Gc<Symbol>,
}

impl PatternAmbientModule {
    pub fn new(pattern: Rc<Pattern>, symbol: Gc<Symbol>) -> Self {
        Self { pattern, symbol }
    }
}

bitflags! {
    #[derive(Default)]
    pub struct NodeCheckFlags: u32 {
        const None = 0;
        const TypeChecked = 0x00000001;
        const LexicalThis = 0x00000002;
        const CaptureThis = 0x00000004;
        const CaptureNewTarget = 0x00000008;
        const SuperInstance = 0x00000100;
        const SuperStatic = 0x00000200;
        const ContextChecked = 0x00000400;
        const AsyncMethodWithSuper = 0x00000800;
        const AsyncMethodWithSuperBinding = 0x00001000;
        const CaptureArguments = 0x00002000;
        const EnumValuesComputed = 0x00004000;
        const LexicalModuleMergesWithClass = 0x00008000;
        const LoopWithCapturedBlockScopedBinding = 0x00010000;
        const ContainsCapturedBlockScopedBinding = 0x00020000;
        const CapturedBlockScopedBinding = 0x00040000;
        const BlockScopedBindingInLoop = 0x00080000;
        const ClassWithBodyScopedClassBinding = 0x00100000;
        const BodyScopedClassBinding = 0x00200000;
        const NeedsLoopOutParameter = 0x00400000;
        const AssignmentsMarked = 0x00800000;
        const ClassWithConstructorReference = 0x01000000;
        const ConstructorReferenceInClass = 0x02000000;
        const ContainsClassWithPrivateIdentifiers = 0x04000000;
        const ContainsSuperPropertyInStaticInitializer = 0x08000000;
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
pub struct NodeLinksSerializedType {
    pub truncating: Option<bool>,
    pub added_length: usize,
    pub node: Gc<Node /*TypeNode*/>,
}

#[derive(Debug, Trace, Finalize)]
pub struct NodeLinks {
    #[unsafe_ignore_trace]
    pub flags: NodeCheckFlags,
    pub resolved_type: Option<Gc<Type>>,
    pub resolved_enum_type: Option<Gc<Type>>,
    pub resolved_signature: Option<Gc<Signature>>,
    pub resolved_symbol: Option<Gc<Symbol>>,
    pub effects_signature: Option<Gc<Signature>>,
    #[unsafe_ignore_trace]
    pub enum_member_value: Option<StringOrNumber>,
    pub is_visible: Option<bool>,
    pub contains_arguments_reference: Option<bool>,
    pub has_reported_statement_in_ambient_context: Option<bool>,
    #[unsafe_ignore_trace]
    pub jsx_flags: JsxFlags,
    pub resolved_jsx_element_attributes_type: Option<Gc<Type>>,
    pub resolved_jsdoc_type: Option<Gc<Type>>,
    pub switch_types: Option<Vec<Gc<Type>>>,
    pub jsx_namespace: Option<Option<Gc<Symbol>>>,
    pub jsx_implicit_import_container: Option<Option<Gc<Symbol>>>,
    pub context_free_type: Option<Gc<Type>>,
    pub deferred_nodes: Option<IndexMap<NodeId, Gc<Node>>>,
    pub captured_block_scope_bindings: Option<Vec<Gc<Symbol>>>,
    pub outer_type_parameters: Option<Vec<Gc<Type /*TypeParameter*/>>>,
    pub is_exhaustive: Option<bool>,
    pub skip_direct_inference: Option<bool /*true*/>,
    pub declaration_requires_scope_change: Option<bool>,
    pub serialized_types: Option<HashMap<String, NodeLinksSerializedType>>,
}

impl NodeLinks {
    pub fn new() -> Self {
        Self {
            flags: NodeCheckFlags::None,
            resolved_type: None,
            resolved_enum_type: None,
            resolved_symbol: None,
            resolved_signature: None,
            enum_member_value: None,
            is_visible: None,
            effects_signature: None,
            contains_arguments_reference: None,
            has_reported_statement_in_ambient_context: None,
            jsx_flags: JsxFlags::None,
            resolved_jsx_element_attributes_type: None,
            resolved_jsdoc_type: None,
            switch_types: None,
            jsx_namespace: None,
            jsx_implicit_import_container: None,
            context_free_type: None,
            deferred_nodes: None,
            captured_block_scope_bindings: None,
            outer_type_parameters: None,
            is_exhaustive: None,
            skip_direct_inference: None,
            declaration_requires_scope_change: None,
            serialized_types: None,
        }
    }
}

bitflags! {
    pub struct TypeFlags: u32 {
        const None = 0;
        const Any = 1 << 0;
        const Unknown = 1 << 1;
        const String = 1 << 2;
        const Number = 1 << 3;
        const Boolean = 1 << 4;
        const Enum = 1 << 5;
        const BigInt = 1 << 6;
        const StringLiteral = 1 << 7;
        const NumberLiteral = 1 << 8;
        const BooleanLiteral = 1 << 9;
        const EnumLiteral = 1 << 10;
        const BigIntLiteral = 1 << 11;
        const ESSymbol = 1 << 12;
        const UniqueESSymbol = 1 << 13;
        const Void = 1 << 14;
        const Undefined = 1 << 15;
        const Null = 1 << 16;
        const Never = 1 << 17;
        const TypeParameter = 1 << 18;
        const Object = 1 << 19;
        const Union = 1 << 20;
        const Intersection = 1 << 21;
        const Index = 1 << 22;
        const IndexedAccess = 1 << 23;
        const Conditional = 1 << 24;
        const Substitution = 1 << 25;
        const NonPrimitive = 1 << 26;
        const TemplateLiteral = 1 << 27;
        const StringMapping = 1 << 28;

        const AnyOrUnknown = Self::Any.bits | Self::Unknown.bits;
        const Nullable = Self::Undefined.bits | Self::Null.bits;
        const Literal = Self::StringLiteral.bits | Self::NumberLiteral.bits | Self::BigIntLiteral.bits | Self::BooleanLiteral.bits;
        const Unit = Self::Literal.bits | Self::UniqueESSymbol.bits | Self::Nullable.bits;
        const StringOrNumberLiteral = Self::StringLiteral.bits | Self::NumberLiteral.bits;
        const StringOrNumberLiteralOrUnique = Self::StringLiteral.bits | Self::NumberLiteral.bits | Self::UniqueESSymbol.bits;
        const DefinitelyFalsy = Self::StringLiteral.bits | Self::NumberLiteral.bits | Self::BigIntLiteral.bits | Self::BooleanLiteral.bits | Self::Void.bits | Self::Undefined.bits | Self::Null.bits;
        const PossiblyFalsy = Self::DefinitelyFalsy.bits | Self::String.bits | Self::Number.bits | Self::BigInt.bits | Self::Boolean.bits;
        const Intrinsic = Self::Any.bits | Self::Unknown.bits | Self::String.bits | Self::Number.bits | Self::BigInt.bits | Self::Boolean.bits | Self::BooleanLiteral.bits | Self::ESSymbol.bits | Self::Void.bits | Self::Undefined.bits | Self::Null.bits | Self::Never.bits | Self::NonPrimitive.bits;
        const Primitive = Self::String.bits | Self::Number.bits | Self::BigInt.bits | Self::Boolean.bits | Self::Enum.bits | Self::EnumLiteral.bits | Self::ESSymbol.bits | Self::Void.bits | Self::Undefined.bits | Self::Null.bits | Self::Literal.bits | Self::UniqueESSymbol.bits;
        const StringLike = Self::String.bits | Self::StringLiteral.bits | Self::TemplateLiteral.bits | Self::StringMapping.bits;
        const NumberLike = Self::Number.bits | Self::NumberLiteral.bits | Self::Enum.bits;
        const BigIntLike = Self::BigInt.bits | Self::BigIntLiteral.bits;
        const BooleanLike = Self::Boolean.bits | Self::BooleanLiteral.bits;
        const EnumLike = Self::Enum.bits | Self::EnumLiteral.bits;
        const ESSymbolLike = Self::ESSymbol.bits | Self::UniqueESSymbol.bits;
        const VoidLike = Self::Void.bits | Self::Undefined.bits;
        const DisjointDomains = Self::NonPrimitive.bits | Self::StringLike.bits | Self::NumberLike.bits | Self::BigIntLike.bits | Self::BooleanLike.bits | Self::ESSymbolLike.bits | Self::VoidLike.bits | Self::Null.bits;
        const UnionOrIntersection =  Self::Union.bits | Self::Intersection.bits;
        const StructuredType = Self::Object.bits | Self::Union.bits | Self::Intersection.bits;
        const TypeVariable = Self::TypeParameter.bits | Self::IndexedAccess.bits;
        const InstantiableNonPrimitive = Self::TypeVariable.bits | Self::Conditional.bits | Self::Substitution.bits;
        const InstantiablePrimitive = Self::Index.bits | Self::TemplateLiteral.bits | Self::StringMapping.bits;
        const Instantiable = Self::InstantiableNonPrimitive.bits | Self::InstantiablePrimitive.bits;
        const StructuredOrInstantiable = Self::StructuredType.bits | Self::Instantiable.bits;
        const ObjectFlagsType = Self::Any.bits | Self::Nullable.bits | Self::Never.bits | Self::Object.bits | Self::Union.bits | Self::Intersection.bits;
        const Simplifiable = Self::IndexedAccess.bits | Self::Conditional.bits;
        const Singleton = Self::Any.bits | Self::Unknown.bits | Self::String.bits | Self::Number.bits | Self::Boolean.bits | Self::BigInt.bits | Self::ESSymbol.bits | Self::Void.bits | Self::Undefined.bits | Self::Null.bits | Self::Never.bits | Self::NonPrimitive.bits;
        const Narrowable = Self::Any.bits | Self::Unknown.bits | Self::StructuredOrInstantiable.bits | Self::StringLike.bits | Self::NumberLike.bits | Self::BigIntLike.bits | Self::BooleanLike.bits | Self::ESSymbol.bits | Self::UniqueESSymbol.bits | Self::NonPrimitive.bits;
        const IncludesMask = Self::Any.bits | Self::Unknown.bits | Self::Primitive.bits | Self::Never.bits | Self::Object.bits | Self::Union.bits | Self::Intersection.bits | Self::NonPrimitive.bits | Self::TemplateLiteral.bits;
        const IncludesMissingType = Self::TypeParameter.bits;
        const IncludesNonWideningType = Self::Index.bits;
        const IncludesWildcard = Self::IndexedAccess.bits;
        const IncludesEmptyObject = Self::Conditional.bits;
        const IncludesInstantiable = Self::Substitution.bits;
        const NotPrimitiveUnion = Self::Any.bits | Self::Unknown.bits | Self::Enum.bits | Self::Void.bits | Self::Never.bits | Self::Object.bits | Self::Intersection.bits | Self::IncludesInstantiable.bits;
    }
}

pub type TypeId = u32;

#[derive(Clone, Debug, Finalize, Trace)]
#[type_type(impl_from = false)]
pub enum Type {
    BaseType(BaseType),
    IntrinsicType(IntrinsicType),
    LiteralType(LiteralType),
    ObjectType(ObjectType),
    UnionOrIntersectionType(UnionOrIntersectionType),
    TypeParameter(TypeParameter),
    SubstitutionType(SubstitutionType),
    IndexedAccessType(IndexedAccessType),
    StringMappingType(StringMappingType),
    TemplateLiteralType(TemplateLiteralType),
    IndexType(IndexType),
    ConditionalType(ConditionalType),
    UniqueESSymbolType(UniqueESSymbolType),
}

impl Type {
    pub fn as_intrinsic_type(&self) -> &dyn IntrinsicTypeInterface {
        match self {
            Type::IntrinsicType(intrinsic_type) => intrinsic_type,
            _ => panic!("Expected intrinsic type"),
        }
    }

    pub fn as_union_or_intersection_type_interface(&self) -> &dyn UnionOrIntersectionTypeInterface {
        match self {
            Type::UnionOrIntersectionType(union_or_intersection_type) => union_or_intersection_type,
            _ => panic!("Expected union or intersection type"),
        }
    }

    pub fn as_resolved_type(&self) -> &dyn ResolvedTypeInterface {
        match self {
            Type::ObjectType(type_) => {
                if !type_.is_resolved() {
                    panic!("Not resolved")
                }
                type_
            }
            Type::UnionOrIntersectionType(type_) => {
                if !type_.is_resolved() {
                    panic!("Not resolved")
                }
                type_
            }
            _ => panic!("Expected resolved type"),
        }
    }

    pub fn as_fresh_object_literal_type(&self) -> &dyn FreshObjectLiteralTypeInterface {
        match self {
            Type::ObjectType(object_type) => {
                if !object_type.is_resolved() {
                    panic!("Not resolved")
                }
                object_type
            }
            _ => panic!("Expected fresh object literal type"),
        }
    }

    pub fn as_resolvable_type(&self) -> &dyn ResolvableTypeInterface {
        match self {
            Type::ObjectType(type_) => type_,
            Type::UnionOrIntersectionType(type_) => type_,
            _ => panic!("Expected resolvable type"),
        }
    }

    pub fn as_object_flags_type(&self) -> &dyn ObjectFlagsTypeInterface {
        match self {
            Type::ObjectType(object_type) => object_type,
            Type::UnionOrIntersectionType(union_or_intersection_type) => union_or_intersection_type,
            Type::IntrinsicType(intrinsic_type) => intrinsic_type,
            _ => panic!("Expected object flags type"),
        }
    }

    pub fn as_interface_type_with_declared_members(
        &self,
    ) -> &dyn InterfaceTypeWithDeclaredMembersInterface {
        match self {
            Type::ObjectType(ObjectType::InterfaceType(interface_type)) => interface_type,
            _ => panic!("Expected interface type with declared members"),
        }
    }

    pub fn as_generic_type(&self) -> &dyn GenericTypeInterface {
        match self {
            Type::ObjectType(ObjectType::InterfaceType(interface_type)) => interface_type,
            _ => panic!("Expected generic type"),
        }
    }

    pub fn as_interface_type_interface(&self) -> &dyn InterfaceTypeInterface {
        match self {
            Type::ObjectType(ObjectType::InterfaceType(interface_type)) => interface_type,
            _ => panic!("Expected interface type"),
        }
    }

    pub fn maybe_as_type_reference_interface(&self) -> Option<&dyn TypeReferenceInterface> {
        match self {
            Type::ObjectType(ObjectType::InterfaceType(type_)) => Some(type_),
            Type::ObjectType(ObjectType::TypeReference(type_)) => Some(type_),
            _ => None,
        }
    }

    pub fn as_type_reference_interface(&self) -> &dyn TypeReferenceInterface {
        self.maybe_as_type_reference_interface()
            .expect("Expected type reference interface type")
    }

    pub fn as_base_interface_type(&self) -> &BaseInterfaceType {
        enum_unwrapped!(self, [Type, ObjectType, InterfaceType, BaseInterfaceType])
    }

    pub fn as_type_parameter(&self) -> &TypeParameter {
        enum_unwrapped!(self, [Type, TypeParameter])
    }

    pub fn as_interface_type(&self) -> &InterfaceType {
        enum_unwrapped!(self, [Type, ObjectType, InterfaceType])
    }

    pub fn maybe_as_interface_type(&self) -> Option<&InterfaceType> {
        match self {
            Type::ObjectType(ObjectType::InterfaceType(type_)) => Some(type_),
            _ => None,
        }
    }

    pub fn as_not_actually_interface_type<'a>(&'a self) -> NotActuallyInterfaceType<'a> {
        match self {
            Type::ObjectType(ObjectType::InterfaceType(value)) => {
                NotActuallyInterfaceType::InterfaceType(value)
            }
            Type::ObjectType(ObjectType::BaseObjectType(value)) => {
                NotActuallyInterfaceType::BaseObjectType(value)
            }
            _ => panic!("Expected not actually interface type"),
        }
    }

    pub fn as_object_type(&self) -> &ObjectType {
        enum_unwrapped!(self, [Type, ObjectType])
    }

    pub fn maybe_as_type_reference(&self) -> Option<&TypeReference> {
        match self {
            Type::ObjectType(ObjectType::TypeReference(type_)) => Some(type_),
            _ => None,
        }
    }

    pub fn as_type_reference(&self) -> &TypeReference {
        enum_unwrapped!(self, [Type, ObjectType, TypeReference])
    }

    pub fn as_string_literal_type(&self) -> &StringLiteralType {
        enum_unwrapped!(self, [Type, LiteralType, StringLiteralType])
    }

    pub fn as_number_literal_type(&self) -> &NumberLiteralType {
        enum_unwrapped!(self, [Type, LiteralType, NumberLiteralType])
    }

    pub fn as_big_int_literal_type(&self) -> &BigIntLiteralType {
        enum_unwrapped!(self, [Type, LiteralType, BigIntLiteralType])
    }

    pub fn as_union_or_intersection_type(&self) -> &UnionOrIntersectionType {
        enum_unwrapped!(self, [Type, UnionOrIntersectionType])
    }

    pub fn as_literal_type(&self) -> &LiteralType {
        enum_unwrapped!(self, [Type, LiteralType])
    }

    pub fn as_freshable_intrinsic_type(&self) -> &FreshableIntrinsicType {
        enum_unwrapped!(self, [Type, IntrinsicType, FreshableIntrinsicType])
    }

    pub fn as_substitution_type(&self) -> &SubstitutionType {
        enum_unwrapped!(self, [Type, SubstitutionType])
    }

    pub fn as_indexed_access_type(&self) -> &IndexedAccessType {
        enum_unwrapped!(self, [Type, IndexedAccessType])
    }

    pub fn as_string_mapping_type(&self) -> &StringMappingType {
        enum_unwrapped!(self, [Type, StringMappingType])
    }

    pub fn as_template_literal_type(&self) -> &TemplateLiteralType {
        enum_unwrapped!(self, [Type, TemplateLiteralType])
    }

    pub fn as_index_type(&self) -> &IndexType {
        enum_unwrapped!(self, [Type, IndexType])
    }

    pub fn as_union_type(&self) -> &UnionType {
        enum_unwrapped!(self, [Type, UnionOrIntersectionType, UnionType])
    }

    pub fn as_conditional_type(&self) -> &ConditionalType {
        enum_unwrapped!(self, [Type, ConditionalType])
    }

    pub fn maybe_as_mapped_type(&self) -> Option<&MappedType> {
        match self {
            Type::ObjectType(ObjectType::MappedType(type_)) => Some(type_),
            _ => None,
        }
    }

    pub fn as_mapped_type(&self) -> &MappedType {
        enum_unwrapped!(self, [Type, ObjectType, MappedType])
    }

    pub fn as_tuple_type(&self) -> &TupleType {
        enum_unwrapped!(self, [Type, ObjectType, InterfaceType, TupleType])
    }

    pub fn as_unique_es_symbol_type(&self) -> &UniqueESSymbolType {
        enum_unwrapped!(self, [Type, UniqueESSymbolType])
    }

    pub fn as_intersection_type(&self) -> &IntersectionType {
        enum_unwrapped!(self, [Type, UnionOrIntersectionType, IntersectionType])
    }

    pub fn as_reverse_mapped_type(&self) -> &ReverseMappedType {
        enum_unwrapped!(self, [Type, ObjectType, ReverseMappedType])
    }

    pub fn as_evolving_array_type(&self) -> &EvolvingArrayType {
        enum_unwrapped!(self, [Type, ObjectType, EvolvingArrayType])
    }
}

pub trait TypeInterface {
    fn type_wrapper(&self) -> Gc<Type>;
    fn set_type_wrapper(&self, wrapper: Gc<Type>);
    fn flags(&self) -> TypeFlags;
    fn set_flags(&self, flags: TypeFlags);
    fn id(&self) -> TypeId;
    fn maybe_symbol(&self) -> Option<Gc<Symbol>>;
    fn symbol(&self) -> Gc<Symbol>;
    fn set_symbol(&self, symbol: Option<Gc<Symbol>>);
    fn maybe_pattern(&self) -> GcCellRefMut<Option<Gc<Node /*DestructuringPattern*/>>>;
    fn maybe_alias_symbol(&self) -> Option<Gc<Symbol>>;
    fn maybe_alias_symbol_mut(&self) -> GcCellRefMut<Option<Gc<Symbol>>>;
    fn maybe_alias_type_arguments(&self) -> Option<Vec<Gc<Type>>>;
    fn maybe_alias_type_arguments_mut(&self) -> GcCellRefMut<Option<Vec<Gc<Type>>>>;
    fn maybe_alias_type_arguments_contains_marker(&self) -> Option<bool>;
    fn set_alias_type_arguments_contains_marker(
        &self,
        alias_type_arguments_contains_marker: Option<bool>,
    );
    fn maybe_permissive_instantiation(&self) -> GcCellRefMut<Option<Gc<Type>>>;
    fn maybe_restrictive_instantiation(&self) -> GcCellRefMut<Option<Gc<Type>>>;
    fn maybe_immediate_base_constraint(&self) -> GcCellRefMut<Option<Gc<Type>>>;
    fn maybe_widened(&self) -> GcCellRefMut<Option<Gc<Type>>>;
    // InstantiableType fields
    fn maybe_resolved_base_constraint(&self) -> GcCellRefMut<Option<Gc<Type>>>;
    fn maybe_resolved_index_type(&self) -> GcCellRefMut<Option<Gc<Type /*IndexType*/>>>;
    fn maybe_resolved_string_index_type(&self) -> GcCellRefMut<Option<Gc<Type /*IndexType*/>>>;
    // SyntheticDefaultModuleType fields
    fn maybe_synthetic_type(&self) -> GcCellRefMut<Option<Gc<Type>>>;
    fn maybe_default_only_type(&self) -> GcCellRefMut<Option<Gc<Type>>>;
    // PromiseOrAwaitableType fields
    fn maybe_promise_type_of_promise_constructor(&self) -> GcCellRefMut<Option<Gc<Type>>>;
    fn maybe_promised_type_of_promise(&self) -> GcCellRefMut<Option<Gc<Type>>>;
    fn maybe_awaited_type_of_type(&self) -> GcCellRefMut<Option<Gc<Type>>>;
    // IterableOrIteratorType fields
    fn maybe_iteration_types_of_generator_return_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<IterationTypes>>>;
    fn maybe_iteration_types_of_async_generator_return_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<IterationTypes>>>;
    fn maybe_iteration_types_of_iterable(&self) -> GcCellRefMut<Option<Gc<IterationTypes>>>;
    fn maybe_iteration_types_of_iterator(&self) -> GcCellRefMut<Option<Gc<IterationTypes>>>;
    fn maybe_iteration_types_of_async_iterable(&self) -> GcCellRefMut<Option<Gc<IterationTypes>>>;
    fn maybe_iteration_types_of_async_iterator(&self) -> GcCellRefMut<Option<Gc<IterationTypes>>>;
    fn maybe_iteration_types_of_iterator_result(&self) -> GcCellRefMut<Option<Gc<IterationTypes>>>;
    fn get_by_iteration_type_cache_key(
        &self,
        key: IterationTypeCacheKey,
    ) -> Option<Gc<IterationTypes>>;
    fn set_by_iteration_type_cache_key(
        &self,
        key: IterationTypeCacheKey,
        value: Option<Gc<IterationTypes>>,
    );
}

#[derive(Clone, Debug, Trace, Finalize)]
pub struct BaseType {
    _type_wrapper: GcCell<Option<Gc<Type>>>,
    #[unsafe_ignore_trace]
    flags: Cell<TypeFlags>,
    #[unsafe_ignore_trace]
    pub id: Option<TypeId>,
    symbol: GcCell<Option<Gc<Symbol>>>,
    pattern: GcCell<Option<Gc<Node>>>,
    alias_symbol: GcCell<Option<Gc<Symbol>>>,
    alias_type_arguments: GcCell<Option<Vec<Gc<Type>>>>,
    #[unsafe_ignore_trace]
    alias_type_arguments_contains_marker: Cell<Option<bool>>,
    permissive_instantiation: GcCell<Option<Gc<Type>>>,
    restrictive_instantiation: GcCell<Option<Gc<Type>>>,
    immediate_base_constraint: GcCell<Option<Gc<Type>>>,
    widened: GcCell<Option<Gc<Type>>>,
    // InstantiableType fields
    resolved_base_constraint: GcCell<Option<Gc<Type>>>,
    resolved_index_type: GcCell<Option<Gc<Type /*IndexType*/>>>,
    resolved_string_index_type: GcCell<Option<Gc<Type /*IndexType*/>>>,
    // SyntheticDefaultModuleType fields
    synthetic_type: GcCell<Option<Gc<Type>>>,
    default_only_type: GcCell<Option<Gc<Type>>>,
    // PromiseOrAwaitableType fields
    promise_type_of_promise_constructor: GcCell<Option<Gc<Type>>>,
    promised_type_of_promise: GcCell<Option<Gc<Type>>>,
    awaited_type_of_type: GcCell<Option<Gc<Type>>>,
    // IterableOrIteratorType fields
    iteration_types_of_generator_return_type: GcCell<Option<Gc<IterationTypes>>>,
    iteration_types_of_async_generator_return_type: GcCell<Option<Gc<IterationTypes>>>,
    iteration_types_of_iterable: GcCell<Option<Gc<IterationTypes>>>,
    iteration_types_of_iterator: GcCell<Option<Gc<IterationTypes>>>,
    iteration_types_of_async_iterable: GcCell<Option<Gc<IterationTypes>>>,
    iteration_types_of_async_iterator: GcCell<Option<Gc<IterationTypes>>>,
    iteration_types_of_iterator_result: GcCell<Option<Gc<IterationTypes>>>,
}

impl BaseType {
    pub fn new(flags: TypeFlags) -> Self {
        Self {
            _type_wrapper: GcCell::new(None),
            flags: Cell::new(flags),
            id: None,
            symbol: GcCell::new(None),
            pattern: GcCell::new(None),
            alias_symbol: GcCell::new(None),
            alias_type_arguments: GcCell::new(None),
            alias_type_arguments_contains_marker: Cell::new(None),
            permissive_instantiation: GcCell::new(None),
            restrictive_instantiation: GcCell::new(None),
            immediate_base_constraint: GcCell::new(None),
            widened: GcCell::new(None),
            resolved_base_constraint: GcCell::new(None),
            resolved_index_type: GcCell::new(None),
            resolved_string_index_type: GcCell::new(None),
            synthetic_type: GcCell::new(None),
            default_only_type: GcCell::new(None),
            promise_type_of_promise_constructor: GcCell::new(None),
            promised_type_of_promise: GcCell::new(None),
            awaited_type_of_type: GcCell::new(None),
            iteration_types_of_generator_return_type: GcCell::new(None),
            iteration_types_of_async_generator_return_type: GcCell::new(None),
            iteration_types_of_iterable: GcCell::new(None),
            iteration_types_of_iterator: GcCell::new(None),
            iteration_types_of_async_iterable: GcCell::new(None),
            iteration_types_of_async_iterator: GcCell::new(None),
            iteration_types_of_iterator_result: GcCell::new(None),
        }
    }
}

impl TypeInterface for BaseType {
    fn type_wrapper(&self) -> Gc<Type> {
        self._type_wrapper.borrow().clone().unwrap()
    }

    fn set_type_wrapper(&self, wrapper: Gc<Type>) {
        *self._type_wrapper.borrow_mut() = Some(wrapper);
    }

    fn flags(&self) -> TypeFlags {
        self.flags.get()
    }

    fn set_flags(&self, flags: TypeFlags) {
        self.flags.set(flags)
    }

    fn id(&self) -> TypeId {
        self.id.unwrap()
    }

    fn maybe_symbol(&self) -> Option<Gc<Symbol>> {
        self.symbol.borrow().as_ref().map(Clone::clone)
    }

    fn symbol(&self) -> Gc<Symbol> {
        self.symbol.borrow().as_ref().unwrap().clone()
    }

    fn set_symbol(&self, symbol: Option<Gc<Symbol>>) {
        *self.symbol.borrow_mut() = symbol;
    }

    fn maybe_pattern(&self) -> GcCellRefMut<Option<Gc<Node>>> {
        self.pattern.borrow_mut()
    }

    fn maybe_alias_symbol(&self) -> Option<Gc<Symbol>> {
        self.alias_symbol.borrow().clone()
    }

    fn maybe_alias_symbol_mut(&self) -> GcCellRefMut<Option<Gc<Symbol>>> {
        self.alias_symbol.borrow_mut()
    }

    fn maybe_alias_type_arguments(&self) -> Option<Vec<Gc<Type>>> {
        self.alias_type_arguments.borrow().clone()
    }

    fn maybe_alias_type_arguments_mut(&self) -> GcCellRefMut<Option<Vec<Gc<Type>>>> {
        self.alias_type_arguments.borrow_mut()
    }

    fn maybe_alias_type_arguments_contains_marker(&self) -> Option<bool> {
        self.alias_type_arguments_contains_marker.get()
    }

    fn set_alias_type_arguments_contains_marker(
        &self,
        alias_type_arguments_contains_marker: Option<bool>,
    ) {
        self.alias_type_arguments_contains_marker
            .set(alias_type_arguments_contains_marker);
    }

    fn maybe_permissive_instantiation(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.permissive_instantiation.borrow_mut()
    }

    fn maybe_restrictive_instantiation(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.restrictive_instantiation.borrow_mut()
    }

    fn maybe_immediate_base_constraint(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.immediate_base_constraint.borrow_mut()
    }

    fn maybe_widened(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.widened.borrow_mut()
    }

    fn maybe_resolved_base_constraint(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.resolved_base_constraint.borrow_mut()
    }

    fn maybe_resolved_index_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.resolved_index_type.borrow_mut()
    }

    fn maybe_resolved_string_index_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.resolved_string_index_type.borrow_mut()
    }

    fn maybe_synthetic_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.synthetic_type.borrow_mut()
    }

    fn maybe_default_only_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.default_only_type.borrow_mut()
    }

    fn maybe_promise_type_of_promise_constructor(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.promise_type_of_promise_constructor.borrow_mut()
    }

    fn maybe_promised_type_of_promise(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.promised_type_of_promise.borrow_mut()
    }

    fn maybe_awaited_type_of_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.awaited_type_of_type.borrow_mut()
    }

    fn maybe_iteration_types_of_generator_return_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<IterationTypes>>> {
        self.iteration_types_of_generator_return_type.borrow_mut()
    }

    fn maybe_iteration_types_of_async_generator_return_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<IterationTypes>>> {
        self.iteration_types_of_async_generator_return_type
            .borrow_mut()
    }

    fn maybe_iteration_types_of_iterable(&self) -> GcCellRefMut<Option<Gc<IterationTypes>>> {
        self.iteration_types_of_iterable.borrow_mut()
    }

    fn maybe_iteration_types_of_iterator(&self) -> GcCellRefMut<Option<Gc<IterationTypes>>> {
        self.iteration_types_of_iterator.borrow_mut()
    }

    fn maybe_iteration_types_of_async_iterable(&self) -> GcCellRefMut<Option<Gc<IterationTypes>>> {
        self.iteration_types_of_async_iterable.borrow_mut()
    }

    fn maybe_iteration_types_of_async_iterator(&self) -> GcCellRefMut<Option<Gc<IterationTypes>>> {
        self.iteration_types_of_async_iterator.borrow_mut()
    }

    fn maybe_iteration_types_of_iterator_result(&self) -> GcCellRefMut<Option<Gc<IterationTypes>>> {
        self.iteration_types_of_iterator_result.borrow_mut()
    }

    fn get_by_iteration_type_cache_key(
        &self,
        key: IterationTypeCacheKey,
    ) -> Option<Gc<IterationTypes>> {
        match key {
            IterationTypeCacheKey::IterationTypesOfGeneratorReturnType => self
                .maybe_iteration_types_of_generator_return_type()
                .clone(),
            IterationTypeCacheKey::IterationTypesOfAsyncGeneratorReturnType => self
                .maybe_iteration_types_of_async_generator_return_type()
                .clone(),
            IterationTypeCacheKey::IterationTypesOfIterable => {
                self.maybe_iteration_types_of_iterable().clone()
            }
            IterationTypeCacheKey::IterationTypesOfIterator => {
                self.maybe_iteration_types_of_iterator().clone()
            }
            IterationTypeCacheKey::IterationTypesOfAsyncIterable => {
                self.maybe_iteration_types_of_async_iterable().clone()
            }
            IterationTypeCacheKey::IterationTypesOfAsyncIterator => {
                self.maybe_iteration_types_of_async_iterator().clone()
            }
            IterationTypeCacheKey::IterationTypesOfIteratorResult => {
                self.maybe_iteration_types_of_iterator_result().clone()
            }
        }
    }

    fn set_by_iteration_type_cache_key(
        &self,
        key: IterationTypeCacheKey,
        value: Option<Gc<IterationTypes>>,
    ) {
        match key {
            IterationTypeCacheKey::IterationTypesOfGeneratorReturnType => {
                *self.maybe_iteration_types_of_generator_return_type() = value;
            }
            IterationTypeCacheKey::IterationTypesOfAsyncGeneratorReturnType => {
                *self.maybe_iteration_types_of_async_generator_return_type() = value;
            }
            IterationTypeCacheKey::IterationTypesOfIterable => {
                *self.maybe_iteration_types_of_iterable() = value;
            }
            IterationTypeCacheKey::IterationTypesOfIterator => {
                *self.maybe_iteration_types_of_iterator() = value;
            }
            IterationTypeCacheKey::IterationTypesOfAsyncIterable => {
                *self.maybe_iteration_types_of_async_iterable() = value;
            }
            IterationTypeCacheKey::IterationTypesOfAsyncIterator => {
                *self.maybe_iteration_types_of_async_iterator() = value;
            }
            IterationTypeCacheKey::IterationTypesOfIteratorResult => {
                *self.maybe_iteration_types_of_iterator_result() = value
            }
        }
    }
}

impl From<BaseType> for Type {
    fn from(value: BaseType) -> Self {
        Self::BaseType(value)
    }
}

impl From<BaseType> for Gc<Type> {
    fn from(value: BaseType) -> Gc<Type> {
        let rc = Gc::new(Type::BaseType(value));
        rc.set_type_wrapper(rc.clone());
        rc
    }
}

pub trait IntrinsicTypeInterface: TypeInterface {
    fn intrinsic_name(&self) -> &str;
}

#[derive(Clone, Debug, Trace, Finalize)]
#[type_type(interfaces = "IntrinsicTypeInterface, ObjectFlagsTypeInterface")]
pub enum IntrinsicType {
    BaseIntrinsicType(BaseIntrinsicType),
    FreshableIntrinsicType(FreshableIntrinsicType),
}

#[derive(Clone, Debug, Trace, Finalize)]
#[type_type(ancestors = "IntrinsicType")]
pub struct BaseIntrinsicType {
    _type: BaseType,
    intrinsic_name: String,
    #[unsafe_ignore_trace]
    object_flags: Cell<ObjectFlags>,
}

impl BaseIntrinsicType {
    pub fn new(type_: BaseType, intrinsic_name: String, object_flags: ObjectFlags) -> Self {
        Self {
            _type: type_,
            intrinsic_name,
            object_flags: Cell::new(object_flags),
        }
    }
}

impl IntrinsicTypeInterface for BaseIntrinsicType {
    fn intrinsic_name(&self) -> &str {
        &self.intrinsic_name
    }
}

impl ObjectFlagsTypeInterface for BaseIntrinsicType {
    fn object_flags(&self) -> ObjectFlags {
        self.object_flags.get()
    }

    fn set_object_flags(&self, object_flags: ObjectFlags) {
        self.object_flags.set(object_flags);
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
#[type_type(
    ancestors = "IntrinsicType",
    interfaces = "IntrinsicTypeInterface, ObjectFlagsTypeInterface"
)]
pub struct FreshableIntrinsicType {
    _intrinsic_type: BaseIntrinsicType,
    pub fresh_type: GcCell<Option<Gc<Type>>>,
    pub regular_type: GcCell<Option<Gc<Type>>>,
}

impl FreshableIntrinsicType {
    pub fn new(intrinsic_type: BaseIntrinsicType) -> Self {
        Self {
            _intrinsic_type: intrinsic_type,
            fresh_type: Default::default(),
            regular_type: Default::default(),
        }
    }

    pub fn fresh_type(&self) -> Gc<Type> {
        self.fresh_type.borrow().clone().unwrap()
    }

    pub fn set_fresh_type(&self, fresh_type: Gc<Type>) {
        *self.fresh_type.borrow_mut() = Some(fresh_type);
    }

    pub fn regular_type(&self) -> Gc<Type> {
        self.regular_type.borrow().clone().unwrap()
    }

    pub fn set_regular_type(&self, regular_type: Gc<Type>) {
        *self.regular_type.borrow_mut() = Some(regular_type);
    }
}
