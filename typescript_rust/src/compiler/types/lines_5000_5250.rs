use std::{cell::{Cell, RefCell}, collections::HashMap, rc::Rc};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRefMut, Trace};
use id_arena::Id;
use indexmap::IndexMap;
use local_macros::{enum_unwrapped, symbol_type, type_type};

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
    NotActuallyInterfaceType, ObjectFlags, Pattern, StringOrNumber, TypeReferenceInterface, _d,
};

#[derive(Debug, Trace, Finalize)]
#[symbol_type(ancestors = "TransientSymbol", interfaces = "TransientSymbolInterface")]
pub struct MappedSymbol {
    _transient_symbol: BaseTransientSymbol,
    pub mapped_type: Id<Type /*MappedType*/>,
    key_type: Cell<Id<Type>>,
}

impl MappedSymbol {
    pub fn new(
        transient_symbol: BaseTransientSymbol,
        mapped_type: Id<Type>,
        key_type: Id<Type>,
    ) -> Self {
        Self {
            _transient_symbol: transient_symbol,
            mapped_type,
            key_type: Cell::new(key_type),
        }
    }

    pub fn key_type(&self) -> Id<Type> {
        self.key_type.get()
    }

    pub fn set_key_type(&self, key_type: Id<Type>) {
        self.key_type.set(key_type);
    }
}

#[derive(Debug, Trace, Finalize)]
#[symbol_type(ancestors = "TransientSymbol", interfaces = "TransientSymbolInterface")]
pub struct ReverseMappedSymbol {
    _transient_symbol: BaseTransientSymbol,
    pub property_type: Id<Type>,
    pub mapped_type: Id<Type /*MappedType*/>,
    pub constraint_type: Id<Type /*IndexType*/>,
}

impl ReverseMappedSymbol {
    pub fn new(
        transient_symbol: BaseTransientSymbol,
        property_type: Id<Type>,
        mapped_type: Id<Type>,
        constraint_type: Id<Type>,
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
    pub const Call: &str/*__String*/ = "__call";
    pub const Constructor: &str/*__String*/ = "__constructor";
    pub const New: &str/*__String*/ = "__new";
    pub const Index: &str/*__String*/ = "__index";
    pub const ExportStar: &str/*__String*/ = "__export";
    pub const Global: &str/*__String*/ = "__global";
    pub const Missing: &str/*__String*/ = "__missing";
    pub const Type: &str/*__String*/ = "__type";
    pub const Object: &str/*__String*/ = "__object";
    pub const JSXAttributes: &str/*__String*/ = "__jsxAttributes";
    pub const Class: &str/*__String*/ = "__class";
    pub const Function: &str/*__String*/ = "__function";
    pub const Computed: &str/*__String*/ = "__computed";
    pub const Resolving: &str/*__String*/ = "__resolving__";
    pub const ExportEquals: &str/*__String*/ = "export=";
    pub const Default: &str/*__String*/ = "default";
    pub const This: &str/*__String*/ = "this";
}

pub type __String = String;

pub type UnderscoreEscapedMap<TValue> = HashMap<__String, TValue>;

pub type SymbolTable = IndexMap<__String, Id<Symbol>>;

#[derive(Clone, Debug, Trace, Finalize)]
pub struct PatternAmbientModule {
    #[unsafe_ignore_trace]
    pub pattern: Rc<Pattern>,
    pub symbol: Id<Symbol>,
}

impl PatternAmbientModule {
    pub fn new(pattern: Rc<Pattern>, symbol: Id<Symbol>) -> Self {
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
    pub node: Id<Node /*TypeNode*/>,
}

#[derive(Debug, Trace, Finalize)]
pub struct NodeLinks {
    #[unsafe_ignore_trace]
    pub flags: NodeCheckFlags,
    pub resolved_type: Option<Id<Type>>,
    pub resolved_enum_type: Option<Id<Type>>,
    pub resolved_signature: Option<Id<Signature>>,
    pub resolved_symbol: Option<Id<Symbol>>,
    pub effects_signature: Option<Id<Signature>>,
    #[unsafe_ignore_trace]
    pub enum_member_value: Option<StringOrNumber>,
    pub is_visible: Option<bool>,
    pub contains_arguments_reference: Option<bool>,
    pub has_reported_statement_in_ambient_context: Option<bool>,
    #[unsafe_ignore_trace]
    pub jsx_flags: JsxFlags,
    pub resolved_jsx_element_attributes_type: Option<Id<Type>>,
    pub resolved_jsdoc_type: Option<Id<Type>>,
    pub switch_types: Option<Vec<Id<Type>>>,
    pub jsx_namespace: Option<Option<Id<Symbol>>>,
    pub jsx_implicit_import_container: Option<Option<Id<Symbol>>>,
    pub context_free_type: Option<Id<Type>>,
    pub deferred_nodes: Option<IndexMap<NodeId, Id<Node>>>,
    pub captured_block_scope_bindings: Option<Vec<Id<Symbol>>>,
    pub outer_type_parameters: Option<Vec<Id<Type /*TypeParameter*/>>>,
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
    fn arena_id(&self) -> Id<Type>;
    fn set_arena_id(&self, id: Id<Type>);
    fn flags(&self) -> TypeFlags;
    fn set_flags(&self, flags: TypeFlags);
    fn id(&self) -> TypeId;
    fn maybe_symbol(&self) -> Option<Id<Symbol>>;
    fn symbol(&self) -> Id<Symbol>;
    fn set_symbol(&self, symbol: Option<Id<Symbol>>);
    fn maybe_pattern(&self) -> Option<Id<Node /*DestructuringPattern*/>>;
    fn set_pattern(&self, pattern: Option<Id<Node /*DestructuringPattern*/>>);
    fn maybe_alias_symbol(&self) -> Option<Id<Symbol>>;
    fn set_alias_symbol(&self, alias_symbol: Option<Id<Symbol>>);
    fn maybe_alias_type_arguments(&self) -> Option<Vec<Id<Type>>>;
    fn set_alias_type_arguments(&self, alias_type_arguments: Option<Vec<Id<Type>>>);
    fn maybe_alias_type_arguments_contains_marker(&self) -> Option<bool>;
    fn set_alias_type_arguments_contains_marker(
        &self,
        alias_type_arguments_contains_marker: Option<bool>,
    );
    fn maybe_permissive_instantiation(&self) -> Option<Id<Type>>;
    fn set_permissive_instantiation(&self, permissive_instantiation: Option<Id<Type>>);
    fn maybe_restrictive_instantiation(&self) -> Option<Id<Type>>;
    fn set_restrictive_instantiation(&self, restrictive_instantiation: Option<Id<Type>>);
    fn maybe_immediate_base_constraint(&self) -> Option<Id<Type>>;
    fn set_immediate_base_constraint(&self, immediate_base_constraint: Option<Id<Type>>);
    fn maybe_widened(&self) -> Option<Id<Type>>;
    fn set_widened(&self, widened: Option<Id<Type>>);
    // InstantiableType fields
    fn maybe_resolved_base_constraint(&self) -> Option<Id<Type>>;
    fn set_resolved_base_constraint(&self, resolved_base_constraint: Option<Id<Type>>);
    fn maybe_resolved_index_type(&self) -> Option<Id<Type /*IndexType*/>>;
    fn set_resolved_index_type(&self, resolved_index_type: Option<Id<Type>>);
    fn maybe_resolved_string_index_type(&self) -> Option<Id<Type /*IndexType*/>>;
    fn set_resolved_string_index_type(&self, resolved_string_index_type: Option<Id<Type>>);
    // SyntheticDefaultModuleType fields
    fn maybe_synthetic_type(&self) -> Option<Id<Type>>;
    fn set_synthetic_type(&self, synthetic_type: Option<Id<Type>>);
    fn maybe_default_only_type(&self) -> Option<Id<Type>>;
    fn set_default_only_type(&self, default_only_type: Option<Id<Type>>);
    // PromiseOrAwaitableType fields
    fn maybe_promise_type_of_promise_constructor(&self) -> Option<Id<Type>>;
    fn set_promise_type_of_promise_constructor(&self, promise_type_of_promise_constructor: Option<Id<Type>>);
    fn maybe_promised_type_of_promise(&self) -> Option<Id<Type>>;
    fn set_promised_type_of_promise(&self, promised_type_of_promise: Option<Id<Type>>);
    fn maybe_awaited_type_of_type(&self) -> Option<Id<Type>>;
    fn set_awaited_type_of_type(&self, awaited_type_of_type: Option<Id<Type>>);
    // IterableOrIteratorType fields
    fn maybe_iteration_types_of_generator_return_type(
        &self,
    ) -> Option<Id<IterationTypes>>;
    fn set_iteration_types_of_generator_return_type(
        &self,
        iteration_types_of_generator_return_type: Option<Id<IterationTypes>>,
    );
    fn maybe_iteration_types_of_async_generator_return_type(
        &self,
    ) -> Option<Id<IterationTypes>>;
    fn set_iteration_types_of_async_generator_return_type(
        &self,
        iteration_types_of_async_generator_return_type: Option<Id<IterationTypes>>,
    );
    fn maybe_iteration_types_of_iterable(&self) -> Option<Id<IterationTypes>>;
    fn set_iteration_types_of_iterable(&self, iteration_types_of_iterable: Option<Id<IterationTypes>>);
    fn maybe_iteration_types_of_iterator(&self) -> Option<Id<IterationTypes>>;
    fn set_iteration_types_of_iterator(&self, iteration_types_of_iterator: Option<Id<IterationTypes>>);
    fn maybe_iteration_types_of_async_iterable(&self) -> Option<Id<IterationTypes>>;
    fn set_iteration_types_of_async_iterable(&self, iteration_types_of_async_iterable: Option<Id<IterationTypes>>);
    fn maybe_iteration_types_of_async_iterator(&self) -> Option<Id<IterationTypes>>;
    fn set_iteration_types_of_async_iterator(&self, iteration_types_of_async_iterator: Option<Id<IterationTypes>>);
    fn maybe_iteration_types_of_iterator_result(&self) -> Option<Id<IterationTypes>>;
    fn set_iteration_types_of_iterator_result(&self, iteration_types_of_iterator_result: Option<Id<IterationTypes>>);
    fn get_by_iteration_type_cache_key(
        &self,
        key: IterationTypeCacheKey,
    ) -> Option<Id<IterationTypes>>;
    fn set_by_iteration_type_cache_key(
        &self,
        key: IterationTypeCacheKey,
        value: Option<Id<IterationTypes>>,
    );
}

#[derive(Clone, Debug, Trace, Finalize)]
pub struct BaseType {
    _arena_id: Cell<Option<Id<Type>>>,
    #[unsafe_ignore_trace]
    flags: Cell<TypeFlags>,
    #[unsafe_ignore_trace]
    pub id: Option<TypeId>,
    symbol: Cell<Option<Id<Symbol>>>,
    pattern: Cell<Option<Id<Node>>>,
    alias_symbol: Cell<Option<Id<Symbol>>>,
    alias_type_arguments: RefCell<Option<Vec<Id<Type>>>>,
    #[unsafe_ignore_trace]
    alias_type_arguments_contains_marker: Cell<Option<bool>>,
    permissive_instantiation: Cell<Option<Id<Type>>>,
    restrictive_instantiation: Cell<Option<Id<Type>>>,
    immediate_base_constraint: Cell<Option<Id<Type>>>,
    widened: Cell<Option<Id<Type>>>,
    // InstantiableType fields
    resolved_base_constraint: Cell<Option<Id<Type>>>,
    resolved_index_type: Cell<Option<Id<Type /*IndexType*/>>>,
    resolved_string_index_type: Cell<Option<Id<Type /*IndexType*/>>>,
    // SyntheticDefaultModuleType fields
    synthetic_type: Cell<Option<Id<Type>>>,
    default_only_type: Cell<Option<Id<Type>>>,
    // PromiseOrAwaitableType fields
    promise_type_of_promise_constructor: Cell<Option<Id<Type>>>,
    promised_type_of_promise: Cell<Option<Id<Type>>>,
    awaited_type_of_type: Cell<Option<Id<Type>>>,
    // IterableOrIteratorType fields
    iteration_types_of_generator_return_type: Cell<Option<Id<IterationTypes>>>,
    iteration_types_of_async_generator_return_type: Cell<Option<Id<IterationTypes>>>,
    iteration_types_of_iterable: Cell<Option<Id<IterationTypes>>>,
    iteration_types_of_iterator: Cell<Option<Id<IterationTypes>>>,
    iteration_types_of_async_iterable: Cell<Option<Id<IterationTypes>>>,
    iteration_types_of_async_iterator: Cell<Option<Id<IterationTypes>>>,
    iteration_types_of_iterator_result: Cell<Option<Id<IterationTypes>>>,
}

impl BaseType {
    pub fn new(flags: TypeFlags) -> Self {
        Self {
            _arena_id: _d(),
            flags: Cell::new(flags),
            id: _d(),
            symbol: _d(),
            pattern: _d(),
            alias_symbol: _d(),
            alias_type_arguments: _d(),
            alias_type_arguments_contains_marker: _d(),
            permissive_instantiation: _d(),
            restrictive_instantiation: _d(),
            immediate_base_constraint: _d(),
            widened: _d(),
            resolved_base_constraint: _d(),
            resolved_index_type: _d(),
            resolved_string_index_type: _d(),
            synthetic_type: _d(),
            default_only_type: _d(),
            promise_type_of_promise_constructor: _d(),
            promised_type_of_promise: _d(),
            awaited_type_of_type: _d(),
            iteration_types_of_generator_return_type: _d(),
            iteration_types_of_async_generator_return_type: _d(),
            iteration_types_of_iterable: _d(),
            iteration_types_of_iterator: _d(),
            iteration_types_of_async_iterable: _d(),
            iteration_types_of_async_iterator: _d(),
            iteration_types_of_iterator_result: _d(),
        }
    }
}

impl TypeInterface for BaseType {
    fn arena_id(&self) -> Id<Type> {
        self._arena_id.get().unwrap()
    }

    fn set_arena_id(&self, id: Id<Type>) {
        self._arena_id.set(Some(id));
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

    fn maybe_symbol(&self) -> Option<Id<Symbol>> {
        self.symbol.get()
    }

    fn symbol(&self) -> Id<Symbol> {
        self.symbol.get().unwrap()
    }

    fn set_symbol(&self, symbol: Option<Id<Symbol>>) {
        self.symbol.set(symbol);
    }

    fn maybe_pattern(&self) -> Option<Id<Node>> {
        self.pattern.get()
    }

    fn set_pattern(&self, pattern: Option<Id<Node /*DestructuringPattern*/>>) {
        self.pattern.set(pattern);
    }

    fn maybe_alias_symbol(&self) -> Option<Id<Symbol>> {
        self.alias_symbol.get()
    }

    fn set_alias_symbol(&self, alias_symbol: Option<Id<Symbol>>) {
        self.alias_symbol.set(alias_symbol);
    }

    fn maybe_alias_type_arguments(&self) -> Option<Vec<Id<Type>>> {
        self.alias_type_arguments.get()
    }

    fn set_alias_type_arguments(&self, alias_type_arguments: Option<Vec<Id<Type>>>) {
        *self.alias_type_arguments.borrow_mut() = alias_type_arguments;
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

    fn maybe_permissive_instantiation(&self) -> Option<Id<Type>> {
        self.permissive_instantiation.get()
    }

    fn set_permissive_instantiation(&self, permissive_instantiation: Option<Id<Type>>) {
        self.permissive_instantiation.set(permissive_instantiation);
    }

    fn maybe_restrictive_instantiation(&self) -> Option<Id<Type>> {
        self.restrictive_instantiation.get()
    }

    fn set_restrictive_instantiation(&self, restrictive_instantiation: Option<Id<Type>>) {
        self.restrictive_instantiation.set(restrictive_instantiation);
    }

    fn maybe_immediate_base_constraint(&self) -> Option<Id<Type>> {
        self.immediate_base_constraint.get()
    }

    fn set_immediate_base_constraint(&self, immediate_base_constraint: Option<Id<Type>>) {
        self.immediate_base_constraint.set(immediate_base_constraint);
    }

    fn maybe_widened(&self) -> Option<Id<Type>> {
        self.widened.get()
    }

    fn set_widened(&self, widened: Option<Id<Type>>) {
        self.widened.set(widened);
    }

    fn maybe_resolved_base_constraint(&self) -> Option<Id<Type>> {
        self.resolved_base_constraint.get()
    }

    fn set_resolved_base_constraint(&self, resolved_base_constraint: Option<Id<Type>>) {
        self.resolved_base_constraint.set(resolved_base_constraint);
    }

    fn maybe_resolved_index_type(&self) -> Option<Id<Type>> {
        self.resolved_index_type.get()
    }

    fn set_resolved_index_type(&self, resolved_index_type: Option<Id<Type>>) {
        self.resolved_index_type.set(resolved_index_type);
    }

    fn maybe_resolved_string_index_type(&self) -> Option<Id<Type>> {
        self.resolved_string_index_type.get()
    }

    fn set_resolved_string_index_type(&self, resolved_string_index_type: Option<Id<Type>>) {
        self.resolved_string_index_type.set(resolved_string_index_type);
    }

    fn maybe_synthetic_type(&self) -> Option<Id<Type>> {
        self.synthetic_type.get()
    }

    fn set_synthetic_type(&self, synthetic_type: Option<Id<Type>>) {
        self.synthetic_type.set(synthetic_type);
    }

    fn maybe_default_only_type(&self) -> Option<Id<Type>> {
        self.default_only_type.get()
    }

    fn set_default_only_type(&self, default_only_type: Option<Id<Type>>) {
        self.default_only_type.set(default_only_type);
    }

    fn maybe_promise_type_of_promise_constructor(&self) -> Option<Id<Type>> {
        self.promise_type_of_promise_constructor.get()
    }

    fn set_promise_type_of_promise_constructor(&self, promise_type_of_promise_constructor: Option<Id<Type>>) {
        self.promise_type_of_promise_constructor.set(promise_type_of_promise_constructor);
    }

    fn maybe_promised_type_of_promise(&self) -> Option<Id<Type>> {
        self.promised_type_of_promise.get()
    }

    fn set_promised_type_of_promise(&self, promised_type_of_promise: Option<Id<Type>>) {
        self.promised_type_of_promise.set(promised_type_of_promise);
    }

    fn maybe_awaited_type_of_type(&self) -> Option<Id<Type>> {
        self.awaited_type_of_type.get()
    }

    fn set_awaited_type_of_type(&self, awaited_type_of_type: Option<Id<Type>>) {
        self.awaited_type_of_type.set(awaited_type_of_type);
    }

    fn maybe_iteration_types_of_generator_return_type(
        &self,
    ) -> Option<Id<IterationTypes>> {
        self.iteration_types_of_generator_return_type.get()
    }

    fn set_iteration_types_of_generator_return_type(
        &self,
        iteration_types_of_generator_return_type: Option<Id<IterationTypes>>,
    ) {
        self.iteration_types_of_generator_return_type.set(iteration_types_of_generator_return_type);
    }

    fn maybe_iteration_types_of_async_generator_return_type(
        &self,
    ) -> Option<Id<IterationTypes>> {
        self.iteration_types_of_async_generator_return_type
            .get()
    }

    fn maybe_iteration_types_of_iterable(&self) -> Option<Id<IterationTypes>> {
        self.iteration_types_of_iterable.get()
    }

    fn set_iteration_types_of_iterable(&self, iteration_types_of_iterable: Option<Id<IterationTypes>>) {
        self.iteration_types_of_iterable.set(iteration_types_of_iterable);
    }

    fn maybe_iteration_types_of_iterator(&self) -> Option<Id<IterationTypes>> {
        self.iteration_types_of_iterator.get()
    }

    fn set_iteration_types_of_iterator(&self, iteration_types_of_iterator: Option<Id<IterationTypes>>) {
        self.iteration_types_of_iterator.set(iteration_types_of_iterator);
    }

    fn maybe_iteration_types_of_async_iterable(&self) -> Option<Id<IterationTypes>> {
        self.iteration_types_of_async_iterable.get()
    }

    fn set_iteration_types_of_async_iterable(&self, iteration_types_of_async_iterable: Option<Id<IterationTypes>>) {
        self.iteration_types_of_async_iterable.set(iteration_types_of_async_iterable);
    }

    fn maybe_iteration_types_of_async_iterator(&self) -> Option<Id<IterationTypes>> {
        self.iteration_types_of_async_iterator.get()
    }

    fn set_iteration_types_of_async_iterator(&self, iteration_types_of_async_iterator: Option<Id<IterationTypes>>) {
        self.iteration_types_of_async_iterator.set(iteration_types_of_async_iterator);
    }

    fn maybe_iteration_types_of_iterator_result(&self) -> Option<Id<IterationTypes>> {
        self.iteration_types_of_iterator_result.get()
    }

    fn set_iteration_types_of_iterator_result(&self, iteration_types_of_iterator_result: Option<Id<IterationTypes>>) {
        self.iteration_types_of_iterator_result.set(iteration_types_of_iterator_result);
    }

    fn get_by_iteration_type_cache_key(
        &self,
        key: IterationTypeCacheKey,
    ) -> Option<Id<IterationTypes>> {
        match key {
            IterationTypeCacheKey::IterationTypesOfGeneratorReturnType => self
                .maybe_iteration_types_of_generator_return_type(),
            IterationTypeCacheKey::IterationTypesOfAsyncGeneratorReturnType => self
                .maybe_iteration_types_of_async_generator_return_type(),
            IterationTypeCacheKey::IterationTypesOfIterable => {
                self.maybe_iteration_types_of_iterable()
            }
            IterationTypeCacheKey::IterationTypesOfIterator => {
                self.maybe_iteration_types_of_iterator()
            }
            IterationTypeCacheKey::IterationTypesOfAsyncIterable => {
                self.maybe_iteration_types_of_async_iterable()
            }
            IterationTypeCacheKey::IterationTypesOfAsyncIterator => {
                self.maybe_iteration_types_of_async_iterator()
            }
            IterationTypeCacheKey::IterationTypesOfIteratorResult => {
                self.maybe_iteration_types_of_iterator_result()
            }
        }
    }

    fn set_by_iteration_type_cache_key(
        &self,
        key: IterationTypeCacheKey,
        value: Option<Id<IterationTypes>>,
    ) {
        match key {
            IterationTypeCacheKey::IterationTypesOfGeneratorReturnType => {
                self.set_iteration_types_of_generator_return_type(value);
            }
            IterationTypeCacheKey::IterationTypesOfAsyncGeneratorReturnType => {
                self.set_iteration_types_of_async_generator_return_type(value);
            }
            IterationTypeCacheKey::IterationTypesOfIterable => {
                self.set_iteration_types_of_iterable(value);
            }
            IterationTypeCacheKey::IterationTypesOfIterator => {
                self.set_iteration_types_of_iterator(value);
            }
            IterationTypeCacheKey::IterationTypesOfAsyncIterable => {
                self.set_iteration_types_of_async_iterable(value);
            }
            IterationTypeCacheKey::IterationTypesOfAsyncIterator => {
                self.set_iteration_types_of_async_iterator(value);
            }
            IterationTypeCacheKey::IterationTypesOfIteratorResult => {
                self.set_iteration_types_of_iterator_result(value);
            }
        }
    }
}

impl From<BaseType> for Type {
    fn from(value: BaseType) -> Self {
        Self::BaseType(value)
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
    pub fresh_type: Cell<Option<Id<Type>>>,
    pub regular_type: Cell<Option<Id<Type>>>,
}

impl FreshableIntrinsicType {
    pub fn new(intrinsic_type: BaseIntrinsicType) -> Self {
        Self {
            _intrinsic_type: intrinsic_type,
            fresh_type: Default::default(),
            regular_type: Default::default(),
        }
    }

    pub fn fresh_type(&self) -> Id<Type> {
        self.fresh_type.get().unwrap()
    }

    pub fn set_fresh_type(&self, fresh_type: Id<Type>) {
        self.fresh_type.set(Some(fresh_type));
    }

    pub fn regular_type(&self) -> Id<Type> {
        self.regular_type.get().unwrap()
    }

    pub fn set_regular_type(&self, regular_type: Id<Type>) {
        self.regular_type.set(Some(regular_type));
    }
}
