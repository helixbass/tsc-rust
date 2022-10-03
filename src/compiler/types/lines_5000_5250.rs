#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, RefCell, RefMut};
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::{Rc, Weak};

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
    ObjectFlags, Pattern, StringOrNumber, TypeReferenceInterface, WeakSelf,
};
use local_macros::{enum_unwrapped, symbol_type, type_type};

#[derive(Debug)]
#[symbol_type(ancestors = "TransientSymbol", interfaces = "TransientSymbolInterface")]
pub struct MappedSymbol {
    _transient_symbol: BaseTransientSymbol,
    pub mapped_type: Rc<Type /*MappedType*/>,
    key_type: RefCell<Rc<Type>>,
}

impl MappedSymbol {
    pub fn new(
        transient_symbol: BaseTransientSymbol,
        mapped_type: Rc<Type>,
        key_type: Rc<Type>,
    ) -> Self {
        Self {
            _transient_symbol: transient_symbol,
            mapped_type,
            key_type: RefCell::new(key_type),
        }
    }

    pub fn key_type(&self) -> Rc<Type> {
        self.key_type.borrow().clone()
    }

    pub fn set_key_type(&self, key_type: Rc<Type>) {
        *self.key_type.borrow_mut() = key_type;
    }
}

#[derive(Debug)]
#[symbol_type(ancestors = "TransientSymbol", interfaces = "TransientSymbolInterface")]
pub struct ReverseMappedSymbol {
    _transient_symbol: BaseTransientSymbol,
    pub property_type: Rc<Type>,
    pub mapped_type: Rc<Type /*MappedType*/>,
    pub constraint_type: Rc<Type /*IndexType*/>,
}

impl ReverseMappedSymbol {
    pub fn new(
        transient_symbol: BaseTransientSymbol,
        property_type: Rc<Type>,
        mapped_type: Rc<Type>,
        constraint_type: Rc<Type>,
    ) -> Self {
        Self {
            _transient_symbol: transient_symbol,
            property_type,
            mapped_type,
            constraint_type,
        }
    }
}

pub struct InternalSymbolName;

#[allow(non_snake_case)]
impl InternalSymbolName {
    pub fn Call() -> __String {
        __String::new("__call".to_string())
    }

    pub fn Constructor() -> __String {
        __String::new("__constructor".to_string())
    }

    pub fn New() -> __String {
        __String::new("__new".to_string())
    }

    pub fn Index() -> __String {
        __String::new("__index".to_string())
    }

    pub fn ExportStar() -> __String {
        __String::new("__export".to_string())
    }

    pub fn Global() -> __String {
        __String::new("__global".to_string())
    }

    pub fn Missing() -> __String {
        __String::new("__missing".to_string())
    }

    pub fn Type() -> __String {
        __String::new("__type".to_string())
    }

    pub fn Object() -> __String {
        __String::new("__object".to_string())
    }

    pub fn JSXAttributes() -> __String {
        __String::new("__jsxAttributes".to_string())
    }

    pub fn Class() -> __String {
        __String::new("__class".to_string())
    }

    pub fn Function() -> __String {
        __String::new("__function".to_string())
    }

    pub fn Computed() -> __String {
        __String::new("__computed".to_string())
    }

    pub fn Resolving() -> __String {
        __String::new("__resolving__".to_string())
    }

    pub fn ExportEquals() -> __String {
        __String::new("export=".to_string())
    }

    pub fn Default() -> __String {
        __String::new("default".to_string())
    }

    pub fn This() -> __String {
        __String::new("this".to_string())
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct __String(String);

impl __String {
    pub fn new(string: String) -> Self {
        Self(string)
    }

    pub fn chars(&self) -> std::str::Chars {
        self.0.chars()
    }

    // TODO: should implement via some trait?
    pub fn eq_str(&self, str: &str) -> bool {
        &self.0 == str
    }

    pub fn into_string(self) -> String {
        self.0
    }
}

impl Deref for __String {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub type UnderscoreEscapedMap<TValue> = HashMap<__String, TValue>;

pub type SymbolTable = UnderscoreEscapedMap<Rc<Symbol>>;

#[derive(Clone, Debug)]
pub struct PatternAmbientModule {
    pub pattern: Rc<Pattern>,
    pub symbol: Rc<Symbol>,
}

impl PatternAmbientModule {
    pub fn new(pattern: Rc<Pattern>, symbol: Rc<Symbol>) -> Self {
        Self { pattern, symbol }
    }
}

bitflags! {
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

#[derive(Clone, Debug)]
pub struct NodeLinksSerializedType {
    pub truncating: Option<bool>,
    pub added_length: usize,
    pub node: Rc<Node /*TypeNode*/>,
}

#[derive(Debug)]
pub struct NodeLinks {
    pub flags: NodeCheckFlags,
    pub resolved_type: Option<Rc<Type>>,
    pub resolved_enum_type: Option<Rc<Type>>,
    pub resolved_signature: Option<Rc<Signature>>,
    pub resolved_symbol: Option<Rc<Symbol>>,
    pub effects_signature: Option<Rc<Signature>>,
    pub enum_member_value: Option<StringOrNumber>,
    pub is_visible: Option<bool>,
    pub contains_arguments_reference: Option<bool>,
    pub has_reported_statement_in_ambient_context: Option<bool>,
    pub jsx_flags: JsxFlags,
    pub resolved_jsx_element_attributes_type: Option<Rc<Type>>,
    pub resolved_jsdoc_type: Option<Rc<Type>>,
    pub switch_types: Option<Vec<Rc<Type>>>,
    pub jsx_namespace: Option<Option<Rc<Symbol>>>,
    pub jsx_implicit_import_container: Option<Option<Rc<Symbol>>>,
    pub context_free_type: Option<Rc<Type>>,
    pub deferred_nodes: Option<HashMap<NodeId, Rc<Node>>>,
    pub captured_block_scope_bindings: Option<Vec<Rc<Symbol>>>,
    pub outer_type_parameters: Option<Vec<Rc<Type /*TypeParameter*/>>>,
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

#[derive(Clone, Debug)]
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

    pub fn as_type_reference_interface(&self) -> &dyn TypeReferenceInterface {
        match self {
            Type::ObjectType(ObjectType::InterfaceType(type_)) => type_,
            Type::ObjectType(ObjectType::TypeReference(type_)) => type_,
            _ => panic!("Expected type reference interface type"),
        }
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
    fn type_wrapper(&self) -> Rc<Type>;
    fn set_type_wrapper(&self, wrapper: Rc<Type>);
    fn flags(&self) -> TypeFlags;
    fn set_flags(&self, flags: TypeFlags);
    fn id(&self) -> TypeId;
    fn maybe_symbol(&self) -> Option<Rc<Symbol>>;
    fn symbol(&self) -> Rc<Symbol>;
    fn set_symbol(&self, symbol: Option<Rc<Symbol>>);
    fn maybe_pattern(&self) -> RefMut<Option<Rc<Node /*DestructuringPattern*/>>>;
    fn maybe_alias_symbol(&self) -> Option<Rc<Symbol>>;
    fn maybe_alias_symbol_mut(&self) -> RefMut<Option<Rc<Symbol>>>;
    fn maybe_alias_type_arguments(&self) -> Option<Vec<Rc<Type>>>;
    fn maybe_alias_type_arguments_mut(&self) -> RefMut<Option<Vec<Rc<Type>>>>;
    fn maybe_alias_type_arguments_contains_marker(&self) -> Option<bool>;
    fn set_alias_type_arguments_contains_marker(
        &self,
        alias_type_arguments_contains_marker: Option<bool>,
    );
    fn maybe_permissive_instantiation(&self) -> RefMut<Option<Rc<Type>>>;
    fn maybe_restrictive_instantiation(&self) -> RefMut<Option<Rc<Type>>>;
    fn maybe_immediate_base_constraint(&self) -> RefMut<Option<Rc<Type>>>;
    fn maybe_widened(&self) -> RefMut<Option<Rc<Type>>>;
    // InstantiableType fields
    fn maybe_resolved_base_constraint(&self) -> RefMut<Option<Rc<Type>>>;
    fn maybe_resolved_index_type(&self) -> RefMut<Option<Rc<Type /*IndexType*/>>>;
    fn maybe_resolved_string_index_type(&self) -> RefMut<Option<Rc<Type /*IndexType*/>>>;
    // SyntheticDefaultModuleType fields
    fn maybe_synthetic_type(&self) -> RefMut<Option<Rc<Type>>>;
    fn maybe_default_only_type(&self) -> RefMut<Option<Rc<Type>>>;
    // PromiseOrAwaitableType fields
    fn maybe_promise_type_of_promise_constructor(&self) -> RefMut<Option<Rc<Type>>>;
    fn maybe_promised_type_of_promise(&self) -> RefMut<Option<Rc<Type>>>;
    fn maybe_awaited_type_of_type(&self) -> RefMut<Option<Rc<Type>>>;
    // IterableOrIteratorType fields
    fn maybe_iteration_types_of_generator_return_type(&self) -> RefMut<Option<Rc<IterationTypes>>>;
    fn maybe_iteration_types_of_async_generator_return_type(
        &self,
    ) -> RefMut<Option<Rc<IterationTypes>>>;
    fn maybe_iteration_types_of_iterable(&self) -> RefMut<Option<Rc<IterationTypes>>>;
    fn maybe_iteration_types_of_iterator(&self) -> RefMut<Option<Rc<IterationTypes>>>;
    fn maybe_iteration_types_of_async_iterable(&self) -> RefMut<Option<Rc<IterationTypes>>>;
    fn maybe_iteration_types_of_async_iterator(&self) -> RefMut<Option<Rc<IterationTypes>>>;
    fn maybe_iteration_types_of_iterator_result(&self) -> RefMut<Option<Rc<IterationTypes>>>;
    fn get_by_iteration_type_cache_key(
        &self,
        key: IterationTypeCacheKey,
    ) -> Option<Rc<IterationTypes>>;
    fn set_by_iteration_type_cache_key(
        &self,
        key: IterationTypeCacheKey,
        value: Option<Rc<IterationTypes>>,
    );
}

#[derive(Clone, Debug)]
pub struct BaseType {
    _type_wrapper: RefCell<Option<Rc<Type>>>,
    flags: Cell<TypeFlags>,
    pub id: Option<TypeId>,
    symbol: RefCell<Option<Rc<Symbol>>>,
    pattern: RefCell<Option<Rc<Node>>>,
    alias_symbol: RefCell<Option<Rc<Symbol>>>,
    alias_type_arguments: RefCell<Option<Vec<Rc<Type>>>>,
    alias_type_arguments_contains_marker: Cell<Option<bool>>,
    permissive_instantiation: RefCell<Option<Rc<Type>>>,
    restrictive_instantiation: RefCell<Option<Rc<Type>>>,
    immediate_base_constraint: RefCell<Option<Rc<Type>>>,
    widened: RefCell<Option<Rc<Type>>>,
    // InstantiableType fields
    resolved_base_constraint: RefCell<Option<Rc<Type>>>,
    resolved_index_type: RefCell<Option<Rc<Type /*IndexType*/>>>,
    resolved_string_index_type: RefCell<Option<Rc<Type /*IndexType*/>>>,
    // SyntheticDefaultModuleType fields
    synthetic_type: RefCell<Option<Rc<Type>>>,
    default_only_type: RefCell<Option<Rc<Type>>>,
    // PromiseOrAwaitableType fields
    promise_type_of_promise_constructor: RefCell<Option<Rc<Type>>>,
    promised_type_of_promise: RefCell<Option<Rc<Type>>>,
    awaited_type_of_type: RefCell<Option<Rc<Type>>>,
    // IterableOrIteratorType fields
    iteration_types_of_generator_return_type: RefCell<Option<Rc<IterationTypes>>>,
    iteration_types_of_async_generator_return_type: RefCell<Option<Rc<IterationTypes>>>,
    iteration_types_of_iterable: RefCell<Option<Rc<IterationTypes>>>,
    iteration_types_of_iterator: RefCell<Option<Rc<IterationTypes>>>,
    iteration_types_of_async_iterable: RefCell<Option<Rc<IterationTypes>>>,
    iteration_types_of_async_iterator: RefCell<Option<Rc<IterationTypes>>>,
    iteration_types_of_iterator_result: RefCell<Option<Rc<IterationTypes>>>,
}

impl BaseType {
    pub fn new(flags: TypeFlags) -> Self {
        Self {
            _type_wrapper: RefCell::new(None),
            flags: Cell::new(flags),
            id: None,
            symbol: RefCell::new(None),
            pattern: RefCell::new(None),
            alias_symbol: RefCell::new(None),
            alias_type_arguments: RefCell::new(None),
            alias_type_arguments_contains_marker: Cell::new(None),
            permissive_instantiation: RefCell::new(None),
            restrictive_instantiation: RefCell::new(None),
            immediate_base_constraint: RefCell::new(None),
            widened: RefCell::new(None),
            resolved_base_constraint: RefCell::new(None),
            resolved_index_type: RefCell::new(None),
            resolved_string_index_type: RefCell::new(None),
            synthetic_type: RefCell::new(None),
            default_only_type: RefCell::new(None),
            promise_type_of_promise_constructor: RefCell::new(None),
            promised_type_of_promise: RefCell::new(None),
            awaited_type_of_type: RefCell::new(None),
            iteration_types_of_generator_return_type: RefCell::new(None),
            iteration_types_of_async_generator_return_type: RefCell::new(None),
            iteration_types_of_iterable: RefCell::new(None),
            iteration_types_of_iterator: RefCell::new(None),
            iteration_types_of_async_iterable: RefCell::new(None),
            iteration_types_of_async_iterator: RefCell::new(None),
            iteration_types_of_iterator_result: RefCell::new(None),
        }
    }
}

impl TypeInterface for BaseType {
    fn type_wrapper(&self) -> Rc<Type> {
        self._type_wrapper.borrow().clone().unwrap()
    }

    fn set_type_wrapper(&self, wrapper: Rc<Type>) {
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

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        self.symbol.borrow().as_ref().map(Clone::clone)
    }

    fn symbol(&self) -> Rc<Symbol> {
        self.symbol.borrow().as_ref().unwrap().clone()
    }

    fn set_symbol(&self, symbol: Option<Rc<Symbol>>) {
        *self.symbol.borrow_mut() = symbol;
    }

    fn maybe_pattern(&self) -> RefMut<Option<Rc<Node>>> {
        self.pattern.borrow_mut()
    }

    fn maybe_alias_symbol(&self) -> Option<Rc<Symbol>> {
        self.alias_symbol.borrow().clone()
    }

    fn maybe_alias_symbol_mut(&self) -> RefMut<Option<Rc<Symbol>>> {
        self.alias_symbol.borrow_mut()
    }

    fn maybe_alias_type_arguments(&self) -> Option<Vec<Rc<Type>>> {
        self.alias_type_arguments.borrow().clone()
    }

    fn maybe_alias_type_arguments_mut(&self) -> RefMut<Option<Vec<Rc<Type>>>> {
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

    fn maybe_permissive_instantiation(&self) -> RefMut<Option<Rc<Type>>> {
        self.permissive_instantiation.borrow_mut()
    }

    fn maybe_restrictive_instantiation(&self) -> RefMut<Option<Rc<Type>>> {
        self.restrictive_instantiation.borrow_mut()
    }

    fn maybe_immediate_base_constraint(&self) -> RefMut<Option<Rc<Type>>> {
        self.immediate_base_constraint.borrow_mut()
    }

    fn maybe_widened(&self) -> RefMut<Option<Rc<Type>>> {
        self.widened.borrow_mut()
    }

    fn maybe_resolved_base_constraint(&self) -> RefMut<Option<Rc<Type>>> {
        self.resolved_base_constraint.borrow_mut()
    }

    fn maybe_resolved_index_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.resolved_index_type.borrow_mut()
    }

    fn maybe_resolved_string_index_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.resolved_string_index_type.borrow_mut()
    }

    fn maybe_synthetic_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.synthetic_type.borrow_mut()
    }

    fn maybe_default_only_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.default_only_type.borrow_mut()
    }

    fn maybe_promise_type_of_promise_constructor(&self) -> RefMut<Option<Rc<Type>>> {
        self.promise_type_of_promise_constructor.borrow_mut()
    }

    fn maybe_promised_type_of_promise(&self) -> RefMut<Option<Rc<Type>>> {
        self.promised_type_of_promise.borrow_mut()
    }

    fn maybe_awaited_type_of_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.awaited_type_of_type.borrow_mut()
    }

    fn maybe_iteration_types_of_generator_return_type(&self) -> RefMut<Option<Rc<IterationTypes>>> {
        self.iteration_types_of_generator_return_type.borrow_mut()
    }

    fn maybe_iteration_types_of_async_generator_return_type(
        &self,
    ) -> RefMut<Option<Rc<IterationTypes>>> {
        self.iteration_types_of_async_generator_return_type
            .borrow_mut()
    }

    fn maybe_iteration_types_of_iterable(&self) -> RefMut<Option<Rc<IterationTypes>>> {
        self.iteration_types_of_iterable.borrow_mut()
    }

    fn maybe_iteration_types_of_iterator(&self) -> RefMut<Option<Rc<IterationTypes>>> {
        self.iteration_types_of_iterator.borrow_mut()
    }

    fn maybe_iteration_types_of_async_iterable(&self) -> RefMut<Option<Rc<IterationTypes>>> {
        self.iteration_types_of_async_iterable.borrow_mut()
    }

    fn maybe_iteration_types_of_async_iterator(&self) -> RefMut<Option<Rc<IterationTypes>>> {
        self.iteration_types_of_async_iterator.borrow_mut()
    }

    fn maybe_iteration_types_of_iterator_result(&self) -> RefMut<Option<Rc<IterationTypes>>> {
        self.iteration_types_of_iterator_result.borrow_mut()
    }

    fn get_by_iteration_type_cache_key(
        &self,
        key: IterationTypeCacheKey,
    ) -> Option<Rc<IterationTypes>> {
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
        value: Option<Rc<IterationTypes>>,
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

impl From<BaseType> for Rc<Type> {
    fn from(value: BaseType) -> Rc<Type> {
        let rc = Rc::new(Type::BaseType(value));
        rc.set_type_wrapper(rc.clone());
        rc
    }
}

pub trait IntrinsicTypeInterface: TypeInterface {
    fn intrinsic_name(&self) -> &str;
}

#[derive(Clone, Debug)]
#[type_type(interfaces = "IntrinsicTypeInterface, ObjectFlagsTypeInterface")]
pub enum IntrinsicType {
    BaseIntrinsicType(BaseIntrinsicType),
    FreshableIntrinsicType(FreshableIntrinsicType),
}

#[derive(Clone, Debug)]
#[type_type(ancestors = "IntrinsicType")]
pub struct BaseIntrinsicType {
    _type: BaseType,
    intrinsic_name: String,
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

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "IntrinsicType",
    interfaces = "IntrinsicTypeInterface, ObjectFlagsTypeInterface"
)]
pub struct FreshableIntrinsicType {
    _intrinsic_type: BaseIntrinsicType,
    pub fresh_type: WeakSelf<Type>,
    pub regular_type: WeakSelf<Type>,
}

impl FreshableIntrinsicType {
    pub fn new(intrinsic_type: BaseIntrinsicType) -> Self {
        Self {
            _intrinsic_type: intrinsic_type,
            fresh_type: WeakSelf::new(),
            regular_type: WeakSelf::new(),
        }
    }

    pub fn fresh_type(&self) -> Weak<Type> {
        self.fresh_type.get()
    }

    pub fn regular_type(&self) -> Weak<Type> {
        self.regular_type.get()
    }
}
