#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use super::{
    BaseInterfaceType, BigIntLiteralType, InterfaceType, LiteralType, NumberLiteralType,
    ObjectFlagsTypeInterface, ObjectType, ResolvableTypeInterface, ResolvedTypeInterface,
    Signature, StringLiteralType, Symbol, TypeParameter, TypeReference, UnionOrIntersectionType,
    UnionOrIntersectionTypeInterface,
};
use crate::WeakSelf;
use local_macros::{enum_unwrapped, type_type};

pub struct InternalSymbolName;

#[allow(non_snake_case)]
impl InternalSymbolName {
    pub fn Object() -> __String {
        __String::new("__object".to_string())
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
}

pub type UnderscoreEscapedMap<TValue> = HashMap<__String, TValue>;

pub type SymbolTable = UnderscoreEscapedMap<Rc<Symbol>>;

bitflags! {
    pub struct NodeCheckFlags: u32 {
        const None = 0;
    }
}

#[derive(Debug)]
pub struct NodeLinks {
    pub flags: NodeCheckFlags,
    pub resolved_type: Option<Rc<Type>>,
    pub resolved_signature: Option<Rc<Signature>>,
    pub resolved_symbol: Option<Rc<Symbol>>,
}

impl NodeLinks {
    pub fn new() -> Self {
        Self {
            flags: NodeCheckFlags::None,
            resolved_type: None,
            resolved_symbol: None,
            resolved_signature: None,
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

        const Nullable = Self::Undefined.bits | Self::Null.bits;
        const Literal = Self::StringLiteral.bits | Self::NumberLiteral.bits | Self::BigIntLiteral.bits | Self::BooleanLiteral.bits;
        const Unit = Self::Literal.bits | Self::UniqueESSymbol.bits | Self::Nullable.bits;
        const StringOrNumberLiteralOrUnique = Self::StringLiteral.bits | Self::NumberLiteral.bits | Self::UniqueESSymbol.bits;
        const Primitive = Self::String.bits | Self::Number.bits | Self::BigInt.bits | Self::Boolean.bits | Self::Enum.bits | Self::EnumLiteral.bits | Self::ESSymbol.bits | Self::Void.bits | Self::Undefined.bits | Self::Null.bits | Self::Literal.bits | Self::UniqueESSymbol.bits;
        const NumberLike = Self::Number.bits | Self::NumberLiteral.bits | Self::Enum.bits;
        const BigIntLike = Self::BigInt.bits | Self::BigIntLiteral.bits;
        const UnionOrIntersection =  Self::Union.bits | Self::Intersection.bits;
        const StructuredType = Self::Object.bits | Self::Union.bits | Self::Intersection.bits;
        const TypeVariable = Self::TypeParameter.bits | Self::IndexedAccess.bits;
        const InstantiableNonPrimitive = Self::TypeVariable.bits | Self::Conditional.bits | Self::Substitution.bits;
        const InstantiablePrimitive = Self::Index.bits | Self::TemplateLiteral.bits | Self::StringMapping.bits;
        const Instantiable = Self::InstantiableNonPrimitive.bits | Self::InstantiablePrimitive.bits;
        const StructuredOrInstantiable = Self::StructuredType.bits | Self::Instantiable.bits;
        const ObjectFlagsType = Self::Any.bits | Self::Nullable.bits | Self::Never.bits | Self::Object.bits | Self::Union.bits | Self::Intersection.bits;
        const IncludesMask = Self::Any.bits | Self::Unknown.bits | Self::Primitive.bits | Self::Never.bits | Self::Object.bits | Self::Union.bits | Self::Intersection.bits | Self::NonPrimitive.bits | Self::TemplateLiteral.bits;
        const IncludesInstantiable = Self::Substitution.bits;
        const NotPrimitiveUnion = Self::Any.bits | Self::Unknown.bits | Self::Enum.bits | Self::Void.bits | Self::Never.bits | Self::Object.bits | Self::Intersection.bits | Self::IncludesInstantiable.bits;
    }
}

pub type TypeId = u32;

#[derive(Clone, Debug)]
#[type_type(impl_from = false)]
pub enum Type {
    IntrinsicType(IntrinsicType),
    LiteralType(LiteralType),
    ObjectType(ObjectType),
    UnionOrIntersectionType(UnionOrIntersectionType),
    TypeParameter(TypeParameter),
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
            Type::ObjectType(object_type) => {
                if !object_type.is_resolved() {
                    panic!("Not resolved")
                }
                object_type
            }
            _ => panic!("Expected resolved type"),
        }
    }

    pub fn as_resolvable_type(&self) -> &dyn ResolvableTypeInterface {
        match self {
            Type::ObjectType(object_type) => object_type,
            _ => panic!("Expected resolvable type"),
        }
    }

    pub fn as_object_flags_type(&self) -> &dyn ObjectFlagsTypeInterface {
        match self {
            Type::ObjectType(object_type) => object_type,
            Type::UnionOrIntersectionType(union_or_intersection_type) => union_or_intersection_type,
            _ => panic!("Expected object flags type"),
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

    pub fn as_object_type(&self) -> &ObjectType {
        enum_unwrapped!(self, [Type, ObjectType])
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
}

pub trait TypeInterface {
    fn type_wrapper(&self) -> Rc<Type>;
    fn set_type_wrapper(&self, wrapper: Rc<Type>);
    fn flags(&self) -> TypeFlags;
    fn id(&self) -> TypeId;
    fn maybe_symbol(&self) -> Option<Rc<Symbol>>;
    fn symbol(&self) -> Rc<Symbol>;
    fn set_symbol(&mut self, symbol: Option<Rc<Symbol>>);
}

#[derive(Clone, Debug)]
pub struct BaseType {
    _type_wrapper: RefCell<Option<Weak<Type>>>,
    pub flags: TypeFlags,
    pub id: Option<TypeId>,
    symbol: Option<Rc<Symbol>>,
}

impl BaseType {
    pub fn new(flags: TypeFlags) -> Self {
        Self {
            _type_wrapper: RefCell::new(None),
            flags,
            id: None,
            symbol: None,
        }
    }
}

impl TypeInterface for BaseType {
    fn type_wrapper(&self) -> Rc<Type> {
        self._type_wrapper
            .borrow()
            .as_ref()
            .unwrap()
            .upgrade()
            .unwrap()
    }

    fn set_type_wrapper(&self, wrapper: Rc<Type>) {
        *self._type_wrapper.borrow_mut() = Some(Rc::downgrade(&wrapper));
    }

    fn flags(&self) -> TypeFlags {
        self.flags
    }

    fn id(&self) -> TypeId {
        self.id.unwrap()
    }

    fn maybe_symbol(&self) -> Option<Rc<Symbol>> {
        self.symbol.as_ref().map(Clone::clone)
    }

    fn symbol(&self) -> Rc<Symbol> {
        self.symbol.as_ref().unwrap().clone()
    }

    fn set_symbol(&mut self, symbol: Option<Rc<Symbol>>) {
        self.symbol = symbol;
    }
}

pub trait IntrinsicTypeInterface: TypeInterface {
    fn intrinsic_name(&self) -> &str;
}

#[derive(Clone, Debug)]
#[type_type(interfaces = "IntrinsicTypeInterface")]
pub enum IntrinsicType {
    BaseIntrinsicType(BaseIntrinsicType),
    FreshableIntrinsicType(FreshableIntrinsicType),
}

#[derive(Clone, Debug)]
#[type_type(ancestors = "IntrinsicType")]
pub struct BaseIntrinsicType {
    _type: BaseType,
    intrinsic_name: String,
}

impl BaseIntrinsicType {
    pub fn new(type_: BaseType, intrinsic_name: String) -> Self {
        Self {
            _type: type_,
            intrinsic_name,
        }
    }
}

impl IntrinsicTypeInterface for BaseIntrinsicType {
    fn intrinsic_name(&self) -> &str {
        &self.intrinsic_name
    }
}

#[derive(Clone, Debug)]
#[type_type(ancestors = "IntrinsicType", interfaces = "IntrinsicTypeInterface")]
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
