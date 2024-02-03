use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

use bitflags::bitflags;
use id_arena::Id;
use local_macros::type_type;

use super::{
    BaseType, IndexInfo, IntersectionType, MappedType, Node, PseudoBigInt, ResolvedTypeInterface,
    ReverseMappedType, Signature, Symbol, SymbolTable, Type, TypeChecker, TypeInterface,
};
use crate::{
    EvolvingArrayType, FreshObjectLiteralTypeInterface, InArena, Number, TypeId, TypeMapper,
    __String, ref_mut_unwrapped, ref_unwrapped,
};

pub trait LiteralTypeInterface: TypeInterface {
    fn fresh_type(&self) -> Option<Id<Type>>;
    fn set_fresh_type(&self, fresh_type: Id<Type>);
    fn regular_type(&self) -> Id<Type>;
    fn set_regular_type(&self, regular_type: Id<Type>);
}

#[derive(Clone, Debug)]
#[type_type(interfaces = "LiteralTypeInterface")]
pub enum LiteralType {
    StringLiteralType(StringLiteralType),
    NumberLiteralType(NumberLiteralType),
    BigIntLiteralType(BigIntLiteralType),
}

impl LiteralType {
    pub fn is_value_eq(&self, other: &Type) -> bool {
        match (self, other.as_literal_type()) {
            (Self::StringLiteralType(a), Self::StringLiteralType(b)) => a.value == b.value,
            (Self::NumberLiteralType(a), Self::NumberLiteralType(b)) => a.value == b.value,
            (Self::BigIntLiteralType(a), Self::BigIntLiteralType(b)) => a.value == b.value,
            _ => false,
        }
    }

    pub fn get_or_initialize_fresh_type(self_: Id<Type>, type_checker: &TypeChecker) -> Id<Type> {
        if matches!(
            &*self_.ref_(type_checker),
            Type::LiteralType(LiteralType::StringLiteralType(_))
        ) {
            return StringLiteralType::get_or_initialize_fresh_type(self_, type_checker);
        }
        if matches!(
            &*self_.ref_(type_checker),
            Type::LiteralType(LiteralType::NumberLiteralType(_))
        ) {
            return NumberLiteralType::get_or_initialize_fresh_type(self_, type_checker);
        }
        if matches!(
            &*self_.ref_(type_checker),
            Type::LiteralType(LiteralType::BigIntLiteralType(_))
        ) {
            return BigIntLiteralType::get_or_initialize_fresh_type(self_, type_checker);
        }
        unreachable!()
    }
}

#[derive(Clone, Debug)]
#[type_type(impl_from = false)]
pub struct BaseLiteralType {
    _type: BaseType,
    fresh_type: Cell<Option<Id<Type>>>,
    regular_type: Cell<Option<Id<Type>>>,
}

impl BaseLiteralType {
    pub fn new(type_: BaseType) -> Self {
        Self {
            _type: type_,
            fresh_type: Default::default(),
            regular_type: Default::default(),
        }
    }
}

impl LiteralTypeInterface for BaseLiteralType {
    fn fresh_type(&self) -> Option<Id<Type>> {
        self.fresh_type.get()
    }

    fn set_fresh_type(&self, fresh_type: Id<Type>) {
        self.fresh_type.set(Some(fresh_type));
    }

    fn regular_type(&self) -> Id<Type> {
        self.regular_type.get().unwrap()
    }

    fn set_regular_type(&self, regular_type: Id<Type>) {
        self.regular_type.set(Some(regular_type));
    }
}

#[derive(Clone, Debug)]
#[type_type]
pub struct UniqueESSymbolType {
    _type: BaseType,
    pub escaped_name: __String,
}

impl UniqueESSymbolType {
    pub fn new(base_type: BaseType, symbol: Id<Symbol>, escaped_name: __String) -> Self {
        let ret = Self {
            _type: base_type,
            escaped_name,
        };
        ret.set_symbol(Some(symbol));
        ret
    }
}

#[derive(Clone, Debug)]
#[type_type(ancestors = "LiteralType")]
pub struct StringLiteralType {
    _literal_type: BaseLiteralType,
    pub value: String,
}

impl StringLiteralType {
    pub fn new(literal_type: BaseLiteralType, value: String) -> Self {
        Self {
            _literal_type: literal_type,
            value,
        }
    }

    fn create_fresh_type_from_self(self_: Id<Type>, type_checker: &TypeChecker) -> Id<Type> {
        let flags = self_.ref_(type_checker).flags();
        let value = self_
            .ref_(type_checker)
            .as_string_literal_type()
            .value
            .clone();
        let symbol = self_.ref_(type_checker).maybe_symbol();
        let fresh_type = type_checker.create_string_literal_type(flags, value, symbol, Some(self_));
        fresh_type
            .ref_(type_checker)
            .as_literal_type()
            .set_fresh_type(fresh_type.clone());
        self_
            .ref_(type_checker)
            .as_string_literal_type()
            .set_fresh_type(fresh_type.clone());
        fresh_type
    }

    pub fn get_or_initialize_fresh_type(self_: Id<Type>, type_checker: &TypeChecker) -> Id<Type> {
        if self_
            .ref_(type_checker)
            .as_string_literal_type()
            .fresh_type()
            .is_none()
        {
            let fresh_type = Self::create_fresh_type_from_self(self_, type_checker);
            self_
                .ref_(type_checker)
                .as_string_literal_type()
                .set_fresh_type(fresh_type.clone());
            return fresh_type;
        }
        self_
            .ref_(type_checker)
            .as_string_literal_type()
            .fresh_type()
            .unwrap()
    }
}

impl LiteralTypeInterface for StringLiteralType {
    fn fresh_type(&self) -> Option<Id<Type>> {
        self._literal_type.fresh_type()
    }

    fn set_fresh_type(&self, fresh_type: Id<Type>) {
        self._literal_type.set_fresh_type(fresh_type);
    }

    fn regular_type(&self) -> Id<Type> {
        self._literal_type.regular_type()
    }

    fn set_regular_type(&self, regular_type: Id<Type>) {
        self._literal_type.set_regular_type(regular_type);
    }
}

#[derive(Clone, Debug)]
#[type_type(ancestors = "LiteralType")]
pub struct NumberLiteralType {
    _literal_type: BaseLiteralType,
    pub value: Number,
}

impl NumberLiteralType {
    pub fn new(literal_type: BaseLiteralType, value: Number) -> Self {
        Self {
            _literal_type: literal_type,
            value,
        }
    }

    fn create_fresh_type_from_self(self_: Id<Type>, type_checker: &TypeChecker) -> Id<Type> {
        let flags = self_.ref_(type_checker).flags();
        let value = self_.ref_(type_checker).as_number_literal_type().value;
        let symbol = self_.ref_(type_checker).maybe_symbol();
        let fresh_type = type_checker.create_number_literal_type(flags, value, symbol, Some(self_));
        fresh_type
            .ref_(type_checker)
            .as_literal_type()
            .set_fresh_type(fresh_type.clone());
        self_
            .ref_(type_checker)
            .as_number_literal_type()
            .set_fresh_type(fresh_type.clone());
        fresh_type
    }

    pub fn get_or_initialize_fresh_type(self_: Id<Type>, type_checker: &TypeChecker) -> Id<Type> {
        if self_
            .ref_(type_checker)
            .as_number_literal_type()
            .fresh_type()
            .is_none()
        {
            let fresh_type = Self::create_fresh_type_from_self(self_, type_checker);
            self_
                .ref_(type_checker)
                .as_number_literal_type()
                .set_fresh_type(fresh_type.clone());
            return fresh_type;
        }
        self_
            .ref_(type_checker)
            .as_number_literal_type()
            .fresh_type()
            .unwrap()
    }
}

impl LiteralTypeInterface for NumberLiteralType {
    fn fresh_type(&self) -> Option<Id<Type>> {
        self._literal_type.fresh_type()
    }

    fn set_fresh_type(&self, fresh_type: Id<Type>) {
        self._literal_type.set_fresh_type(fresh_type);
    }

    fn regular_type(&self) -> Id<Type> {
        self._literal_type.regular_type()
    }

    fn set_regular_type(&self, regular_type: Id<Type>) {
        self._literal_type.set_regular_type(regular_type);
    }
}

#[derive(Clone, Debug)]
#[type_type(ancestors = "LiteralType")]
pub struct BigIntLiteralType {
    _literal_type: BaseLiteralType,
    pub value: PseudoBigInt,
}

impl BigIntLiteralType {
    pub fn new(literal_type: BaseLiteralType, value: PseudoBigInt) -> Self {
        Self {
            _literal_type: literal_type,
            value,
        }
    }

    fn create_fresh_type_from_self(self_: Id<Type>, type_checker: &TypeChecker) -> Id<Type> {
        let flags = self_.ref_(type_checker).flags();
        let value = self_
            .ref_(type_checker)
            .as_big_int_literal_type()
            .value
            .clone();
        let symbol = self_.ref_(type_checker).maybe_symbol();
        let fresh_type =
            type_checker.create_big_int_literal_type(flags, value, symbol, Some(self_));
        fresh_type
            .ref_(type_checker)
            .as_literal_type()
            .set_fresh_type(fresh_type.clone());
        self_
            .ref_(type_checker)
            .as_big_int_literal_type()
            .set_fresh_type(fresh_type.clone());
        fresh_type
    }

    pub fn get_or_initialize_fresh_type(self_: Id<Type>, type_checker: &TypeChecker) -> Id<Type> {
        if self_
            .ref_(type_checker)
            .as_big_int_literal_type()
            .fresh_type()
            .is_none()
        {
            let fresh_type = Self::create_fresh_type_from_self(self_, type_checker);
            self_
                .ref_(type_checker)
                .as_big_int_literal_type()
                .set_fresh_type(fresh_type.clone());
            return fresh_type;
        }
        self_
            .ref_(type_checker)
            .as_big_int_literal_type()
            .fresh_type()
            .unwrap()
    }
}

impl LiteralTypeInterface for BigIntLiteralType {
    fn fresh_type(&self) -> Option<Id<Type>> {
        self._literal_type.fresh_type()
    }

    fn set_fresh_type(&self, fresh_type: Id<Type>) {
        self._literal_type.set_fresh_type(fresh_type);
    }

    fn regular_type(&self) -> Id<Type> {
        self._literal_type.regular_type()
    }

    fn set_regular_type(&self, regular_type: Id<Type>) {
        self._literal_type.set_regular_type(regular_type);
    }
}

bitflags! {
    pub struct ObjectFlags: u32 {
        const None = 0;
        const Class = 1 << 0;
        const Interface = 1 << 1;
        const Reference = 1 << 2;
        const Tuple = 1 << 3;
        const Anonymous = 1 << 4;
        const Mapped = 1 << 5;
        const Instantiated = 1 << 6;
        const ObjectLiteral = 1 << 7;
        const EvolvingArray = 1 << 8;
        const ObjectLiteralPatternWithComputedProperties = 1 << 9;
        const ReverseMapped = 1 << 10;
        const JsxAttributes = 1 << 11;
        const MarkerType = 1 << 12;
        const JSLiteral = 1 << 13;
        const FreshLiteral = 1 << 14;
        const ArrayLiteral = 1 << 15;
        const PrimitiveUnion = 1 << 16;
        const ContainsWideningType = 1 << 17;
        const ContainsObjectOrArrayLiteral = 1 << 18;
        const NonInferrableType = 1 << 19;
        const CouldContainTypeVariablesComputed = 1 << 20;
        const CouldContainTypeVariables = 1 << 21;

        const ClassOrInterface = Self::Class.bits | Self::Interface.bits;
        const RequiresWidening = Self::ContainsWideningType.bits | Self::ContainsObjectOrArrayLiteral.bits;
        const PropagatingFlags = Self::ContainsWideningType.bits | Self::ContainsObjectOrArrayLiteral.bits | Self::NonInferrableType.bits;
        const ObjectTypeKindMask = Self::ClassOrInterface.bits | Self::Reference.bits | Self::Tuple.bits | Self::Anonymous.bits | Self::Mapped.bits | Self::ReverseMapped.bits | Self::EvolvingArray.bits;

        const ContainsSpread = 1 << 22;
        const ObjectRestType = 1 << 23;
        const IsClassInstanceClone = 1 << 24;
        const IdenticalBaseTypeCalculated = 1 << 25;
        const IdenticalBaseTypeExists = 1 << 26;

        const IsGenericTypeComputed = 1 << 22;
        const IsGenericObjectType = 1 << 23;
        const IsGenericIndexType = 1 << 24;
        const IsGenericType = Self::IsGenericObjectType.bits | Self::IsGenericIndexType.bits;

        const ContainsIntersections = 1 << 25;

        const IsNeverIntersectionComputed = 1 << 25;
        const IsNeverIntersection = 1 << 26;
    }
}

pub trait ObjectFlagsTypeInterface {
    fn object_flags(&self) -> ObjectFlags;
    fn set_object_flags(&self, object_flags: ObjectFlags);
}

pub trait ObjectTypeInterface: ObjectFlagsTypeInterface {
    fn maybe_members(&self) -> Option<Id<SymbolTable>>;
    fn set_members(&self, members: Option<Id<SymbolTable>>);
    fn maybe_properties(&self) -> Option<Id<Vec<Id<Symbol>>>>;
    fn maybe_call_signatures(&self) -> Ref<Option<Vec<Id<Signature>>>>;
    // fn maybe_properties(&self) -> Option<&[Id<Symbol>]>;
    // fn properties(&self) -> &[Id<Symbol>];
    // fn set_properties(&self, properties: Vec<Id<Symbol>>);
    fn maybe_target(&self) -> Option<Id<Type>>;
    fn maybe_mapper(&self) -> Option<Id<TypeMapper>>;
    fn maybe_instantiations(&self) -> RefMut<Option<HashMap<String, Id<Type>>>>;
}

#[derive(Clone, Debug)]
#[type_type(
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface, FreshObjectLiteralTypeInterface"
)]
pub enum ObjectType {
    BaseObjectType(BaseObjectType),
    InterfaceType(InterfaceType),
    TypeReference(TypeReference),
    MappedType(MappedType),
    EvolvingArrayType(EvolvingArrayType),
    ReverseMappedType(ReverseMappedType),
}

#[derive(Clone, Debug)]
#[type_type(ancestors = "ObjectType")]
pub struct BaseObjectType {
    _type: BaseType,
    object_flags: Cell<ObjectFlags>,
    members: Cell<Option<Id<SymbolTable>>>,
    properties: Cell<Option<Id<Vec<Id<Symbol>>>>>,
    call_signatures: RefCell<Option<Vec<Id<Signature>>>>,
    construct_signatures: RefCell<Option<Vec<Id<Signature>>>>,
    index_infos: RefCell<Option<Vec<Id<IndexInfo>>>>,
    object_type_without_abstract_construct_signatures: Cell<Option<Id<Type>>>,
    // AnonymousType fields
    pub target: Option<Id<Type>>,
    pub mapper: Option<Id<TypeMapper>>,
    instantiations: RefCell<Option<HashMap<String, Id<Type>>>>,
    // FreshObjectLiteralType fields
    regular_type: Cell<Option<Id<Type /*ResolvedType*/>>>,
    // "not actually interface type" fields
    not_actually_interface_type_resolved_base_types: Cell<Option<Id<Vec<Id<Type>>>>>,
    not_actually_interface_type_base_types_resolved: Cell<Option<bool>>,
    not_actually_interface_type_resolved_base_constructor_type: Cell<Option<Id<Type>>>,
}

impl BaseObjectType {
    pub fn new(base_type: BaseType, object_flags: ObjectFlags) -> Self {
        Self {
            _type: base_type,
            object_flags: Cell::new(object_flags),
            members: Default::default(),
            properties: Default::default(),
            call_signatures: Default::default(),
            construct_signatures: Default::default(),
            index_infos: Default::default(),
            object_type_without_abstract_construct_signatures: Default::default(),
            target: Default::default(),
            mapper: Default::default(),
            instantiations: Default::default(),
            regular_type: Default::default(),
            not_actually_interface_type_resolved_base_types: Default::default(),
            not_actually_interface_type_base_types_resolved: Default::default(),
            not_actually_interface_type_resolved_base_constructor_type: Default::default(),
        }
    }

    pub fn not_actually_interface_type_maybe_resolved_base_types(
        &self,
    ) -> Option<Id<Vec<Id<Type>>>> {
        self.not_actually_interface_type_resolved_base_types.get()
    }

    pub fn not_actually_interface_type_set_resolved_base_types(
        &self,
        resolved_base_types: Option<Id<Vec<Id<Type>>>>,
    ) {
        self.not_actually_interface_type_resolved_base_types
            .set(resolved_base_types);
    }

    pub fn not_actually_interface_type_maybe_base_types_resolved(&self) -> Option<bool> {
        self.not_actually_interface_type_base_types_resolved.get()
    }

    pub fn not_actually_interface_type_set_base_types_resolved(
        &self,
        base_types_resolved: Option<bool>,
    ) {
        self.not_actually_interface_type_base_types_resolved
            .set(base_types_resolved);
    }

    pub fn not_actually_interface_type_maybe_resolved_base_constructor_type(
        &self,
    ) -> Option<Id<Type>> {
        self.not_actually_interface_type_resolved_base_constructor_type
            .get()
    }

    pub fn not_actually_interface_type_set_resolved_base_constructor_type(
        &self,
        resolved_base_constructor_type: Option<Id<Type>>,
    ) {
        self.not_actually_interface_type_resolved_base_constructor_type
            .set(resolved_base_constructor_type);
    }
}

impl ObjectFlagsTypeInterface for BaseObjectType {
    fn object_flags(&self) -> ObjectFlags {
        self.object_flags.get()
    }

    fn set_object_flags(&self, object_flags: ObjectFlags) {
        self.object_flags.set(object_flags);
    }
}

impl ObjectTypeInterface for BaseObjectType {
    fn maybe_members(&self) -> Option<Id<SymbolTable>> {
        self.members.get()
    }

    fn set_members(&self, members: Option<Id<SymbolTable>>) {
        self.members.set(members);
    }

    fn maybe_properties(&self) -> Option<Id<Vec<Id<Symbol>>>> {
        self.properties.get()
    }

    fn maybe_call_signatures(&self) -> Ref<Option<Vec<Id<Signature>>>> {
        self.call_signatures.borrow()
    }

    fn maybe_target(&self) -> Option<Id<Type>> {
        self.target
    }

    fn maybe_mapper(&self) -> Option<Id<TypeMapper>> {
        self.mapper
    }

    fn maybe_instantiations(&self) -> RefMut<Option<HashMap<String, Id<Type>>>> {
        self.instantiations.borrow_mut()
    }
}

pub trait ResolvableTypeInterface {
    fn resolve(
        &self,
        members: Id<SymbolTable>,
        properties: Id<Vec<Id<Symbol>>>,
        call_signatures: Vec<Id<Signature>>,
        construct_signatures: Vec<Id<Signature>>,
        index_infos: Vec<Id<IndexInfo>>,
    );
    fn is_resolved(&self) -> bool;
}

impl ResolvableTypeInterface for BaseObjectType {
    fn resolve(
        &self,
        members: Id<SymbolTable>,
        properties: Id<Vec<Id<Symbol>>>,
        call_signatures: Vec<Id<Signature>>,
        construct_signatures: Vec<Id<Signature>>,
        index_infos: Vec<Id<IndexInfo>>,
    ) {
        self.members.set(Some(members));
        self.properties.set(Some(properties));
        *self.call_signatures.borrow_mut() = Some(call_signatures);
        *self.construct_signatures.borrow_mut() = Some(construct_signatures);
        *self.index_infos.borrow_mut() = Some(index_infos);
    }

    fn is_resolved(&self) -> bool {
        self.members.get().is_some()
    }
}

impl ResolvedTypeInterface for BaseObjectType {
    fn members(&self) -> Id<SymbolTable> {
        self.members.get().unwrap()
    }

    fn properties(&self) -> Id<Vec<Id<Symbol>>> {
        self.properties.get().unwrap()
    }

    fn set_properties(&self, properties: Id<Vec<Id<Symbol>>>) {
        self.properties.set(Some(properties));
    }

    fn call_signatures(&self) -> Ref<Vec<Id<Signature>>> {
        ref_unwrapped(&self.call_signatures)
    }

    fn set_call_signatures(&self, call_signatures: Vec<Id<Signature>>) {
        *self.call_signatures.borrow_mut() = Some(call_signatures);
    }

    fn construct_signatures(&self) -> Ref<Vec<Id<Signature>>> {
        ref_unwrapped(&self.construct_signatures)
    }

    fn set_construct_signatures(&self, construct_signatures: Vec<Id<Signature>>) {
        *self.construct_signatures.borrow_mut() = Some(construct_signatures);
    }

    fn index_infos(&self) -> Ref<Vec<Id<IndexInfo>>> {
        ref_unwrapped(&self.index_infos)
    }

    fn maybe_object_type_without_abstract_construct_signatures(&self) -> Option<Id<Type>> {
        self.object_type_without_abstract_construct_signatures.get()
    }

    fn set_object_type_without_abstract_construct_signatures(
        &self,
        object_type_without_abstract_construct_signatures: Option<Id<Type>>,
    ) {
        self.object_type_without_abstract_construct_signatures
            .set(object_type_without_abstract_construct_signatures);
    }
}

impl FreshObjectLiteralTypeInterface for BaseObjectType {
    fn maybe_regular_type(&self) -> Option<Id<Type /*ResolvedType*/>> {
        self.regular_type.get()
    }

    fn set_regular_type(&self, regular_type: Option<Id<Type /*ResolvedType*/>>) {
        self.regular_type.set(regular_type);
    }
}

pub enum NotActuallyInterfaceType<'a> {
    InterfaceType(&'a InterfaceType),
    BaseObjectType(&'a BaseObjectType),
}

impl NotActuallyInterfaceType<'_> {
    pub fn maybe_resolved_base_types(&self) -> Option<Id<Vec<Id<Type>>>> {
        match self {
            Self::InterfaceType(value) => value.maybe_resolved_base_types(),
            Self::BaseObjectType(value) => {
                value.not_actually_interface_type_maybe_resolved_base_types()
            }
        }
    }

    pub fn set_resolved_base_types(&self, resolved_base_types: Option<Id<Vec<Id<Type>>>>) {
        match self {
            Self::InterfaceType(value) => value.set_resolved_base_types(resolved_base_types),
            Self::BaseObjectType(value) => {
                value.not_actually_interface_type_set_resolved_base_types(resolved_base_types)
            }
        }
    }

    pub fn maybe_base_types_resolved(&self) -> Option<bool> {
        match self {
            Self::InterfaceType(value) => value.maybe_base_types_resolved(),
            Self::BaseObjectType(value) => {
                value.not_actually_interface_type_maybe_base_types_resolved()
            }
        }
    }

    pub fn set_base_types_resolved(&self, base_types_resolved: Option<bool>) {
        match self {
            Self::InterfaceType(value) => value.set_base_types_resolved(base_types_resolved),
            Self::BaseObjectType(value) => {
                value.not_actually_interface_type_set_base_types_resolved(base_types_resolved)
            }
        }
    }

    pub fn object_flags(&self) -> ObjectFlags {
        match self {
            Self::InterfaceType(value) => value.object_flags(),
            Self::BaseObjectType(value) => value.object_flags(),
        }
    }

    pub fn set_members(&self, members: Option<Id<SymbolTable>>) {
        match self {
            Self::InterfaceType(value) => value.set_members(members),
            Self::BaseObjectType(value) => value.set_members(members),
        }
    }

    pub fn maybe_resolved_base_constructor_type(&self) -> Option<Id<Type>> {
        match self {
            Self::InterfaceType(value) => value.maybe_resolved_base_constructor_type(),
            Self::BaseObjectType(value) => {
                value.not_actually_interface_type_maybe_resolved_base_constructor_type()
            }
        }
    }

    pub fn set_resolved_base_constructor_type(
        &self,
        resolved_base_constructor_type: Option<Id<Type>>,
    ) {
        match self {
            Self::InterfaceType(value) => {
                value.set_resolved_base_constructor_type(resolved_base_constructor_type)
            }
            Self::BaseObjectType(value) => value
                .not_actually_interface_type_set_resolved_base_constructor_type(
                    resolved_base_constructor_type,
                ),
        }
    }
}

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "ObjectType",
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface, InterfaceTypeWithDeclaredMembersInterface, InterfaceTypeInterface, TypeReferenceInterface, GenericTypeInterface, GenericableTypeInterface, FreshObjectLiteralTypeInterface"
)]
pub enum InterfaceType {
    BaseInterfaceType(BaseInterfaceType),
    TupleType(TupleType),
}

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "InterfaceType, ObjectType",
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface, FreshObjectLiteralTypeInterface"
)]
pub struct BaseInterfaceType {
    _object_type: BaseObjectType,
    type_parameters: Option<Vec<Id<Type /*TypeParameter*/>>>,
    outer_type_parameters: Option<Vec<Id<Type /*TypeParameter*/>>>,
    local_type_parameters: Option<Vec<Id<Type /*TypeParameter*/>>>,
    this_type: Cell<Option<Id<Type /*TypeParameter*/>>>,
    resolved_base_constructor_type: Cell<Option<Id<Type /*TypeParameter*/>>>,
    resolved_base_types: Cell<Option<Id<Vec<Id<Type /*BaseType*/>>>>>,
    base_types_resolved: Cell<Option<bool>>,
    // InterfaceTypeWithDeclaredMembers fields
    declared_properties: RefCell<Option<Vec<Id<Symbol>>>>,
    declared_call_signatures: RefCell<Option<Vec<Id<Signature>>>>,
    declared_construct_signatures: RefCell<Option<Vec<Id<Signature>>>>,
    declared_index_infos: RefCell<Option<Vec<Id<IndexInfo>>>>,
    // GenericType fields
    instantiations: RefCell<Option<HashMap<String, Id<Type /*TypeReference*/>>>>,
    variances: Rc<RefCell<Option<Vec<VarianceFlags>>>>,
    // TypeReference fields (for GenericType)
    pub target: Cell<Option<Id<Type /*GenericType*/>>>,
    pub node: Cell<Option<Id<Node /*TypeReferenceNode | ArrayTypeNode | TupleTypeNode*/>>>,
    pub resolved_type_arguments: RefCell<Option<Vec<Id<Type>>>>,
    literal_type: Cell<Option<Id<Type /*TypeReference*/>>>,
    cached_equivalent_base_type: Cell<Option<Id<Type>>>,
}

impl BaseInterfaceType {
    pub fn new(
        object_type: BaseObjectType,
        type_parameters: Option<Vec<Id<Type>>>,
        outer_type_parameters: Option<Vec<Id<Type>>>,
        local_type_parameters: Option<Vec<Id<Type>>>,
        this_type: Option<Id<Type>>,
    ) -> Self {
        Self {
            _object_type: object_type,
            type_parameters,
            outer_type_parameters,
            local_type_parameters,
            this_type: Cell::new(this_type),
            resolved_base_constructor_type: Default::default(),
            resolved_base_types: Default::default(),
            base_types_resolved: Default::default(),
            declared_properties: Default::default(),
            declared_call_signatures: Default::default(),
            declared_construct_signatures: Default::default(),
            declared_index_infos: Default::default(),
            instantiations: Default::default(),
            variances: Default::default(),
            target: Default::default(),
            node: Default::default(),
            resolved_type_arguments: Default::default(),
            literal_type: Default::default(),
            cached_equivalent_base_type: Default::default(),
        }
    }
}

pub trait InterfaceTypeInterface:
    ObjectFlagsTypeInterface
    + ObjectTypeInterface
    + ResolvableTypeInterface
    + ResolvedTypeInterface
    + FreshObjectLiteralTypeInterface
{
    fn maybe_type_parameters(&self) -> Option<&[Id<Type>]>;
    fn maybe_outer_type_parameters(&self) -> Option<&[Id<Type>]>;
    fn maybe_local_type_parameters(&self) -> Option<&[Id<Type>]>;
    fn maybe_this_type(&self) -> Option<Id<Type>>;
    fn maybe_resolved_base_constructor_type(&self) -> Option<Id<Type>>;
    fn set_resolved_base_constructor_type(&self, resolved_base_constructor_type: Option<Id<Type>>);
    fn maybe_resolved_base_types(&self) -> Option<Id<Vec<Id<Type>>>>;
    fn set_resolved_base_types(&self, resolved_base_types: Option<Id<Vec<Id<Type>>>>);
    fn maybe_base_types_resolved(&self) -> Option<bool>;
    fn set_base_types_resolved(&self, base_types_resolved: Option<bool>);
}

impl InterfaceTypeInterface for BaseInterfaceType {
    fn maybe_type_parameters(&self) -> Option<&[Id<Type>]> {
        self.type_parameters.as_deref()
    }

    fn maybe_outer_type_parameters(&self) -> Option<&[Id<Type>]> {
        self.outer_type_parameters.as_deref()
    }

    fn maybe_local_type_parameters(&self) -> Option<&[Id<Type>]> {
        self.local_type_parameters.as_deref()
    }

    fn maybe_this_type(&self) -> Option<Id<Type>> {
        self.this_type.get()
    }

    fn maybe_resolved_base_constructor_type(&self) -> Option<Id<Type>> {
        self.resolved_base_constructor_type.get()
    }

    fn set_resolved_base_constructor_type(&self, resolved_base_constructor_type: Option<Id<Type>>) {
        self.resolved_base_constructor_type
            .set(resolved_base_constructor_type);
    }

    fn maybe_resolved_base_types(&self) -> Option<Id<Vec<Id<Type>>>> {
        self.resolved_base_types.get()
    }

    fn set_resolved_base_types(&self, resolved_base_types: Option<Id<Vec<Id<Type>>>>) {
        self.resolved_base_types.set(resolved_base_types);
    }

    fn maybe_base_types_resolved(&self) -> Option<bool> {
        self.base_types_resolved.get()
    }

    fn set_base_types_resolved(&self, base_types_resolved: Option<bool>) {
        self.base_types_resolved.set(base_types_resolved)
    }
}

impl InterfaceTypeWithDeclaredMembersInterface for BaseInterfaceType {
    fn maybe_declared_properties(&self) -> Ref<Option<Vec<Id<Symbol>>>> {
        self.declared_properties.borrow()
    }

    fn set_declared_properties(&self, declared_properties: Vec<Id<Symbol>>) {
        *self.declared_properties.borrow_mut() = Some(declared_properties);
    }

    fn declared_call_signatures(&self) -> Ref<Vec<Id<Signature>>> {
        ref_unwrapped(&self.declared_call_signatures)
    }

    fn set_declared_call_signatures(&self, declared_call_signatures: Vec<Id<Signature>>) {
        *self.declared_call_signatures.borrow_mut() = Some(declared_call_signatures);
    }

    fn declared_construct_signatures(&self) -> Ref<Vec<Id<Signature>>> {
        ref_unwrapped(&self.declared_construct_signatures)
    }

    fn set_declared_construct_signatures(&self, declared_construct_signatures: Vec<Id<Signature>>) {
        *self.declared_construct_signatures.borrow_mut() = Some(declared_construct_signatures);
    }

    fn declared_index_infos(&self) -> Ref<Vec<Id<IndexInfo>>> {
        ref_unwrapped(&self.declared_index_infos)
    }

    fn set_declared_index_infos(&self, declared_index_infos: Vec<Id<IndexInfo>>) {
        *self.declared_index_infos.borrow_mut() = Some(declared_index_infos);
    }
}

pub trait InterfaceTypeWithDeclaredMembersInterface {
    fn maybe_declared_properties(&self) -> Ref<Option<Vec<Id<Symbol>>>>;
    fn set_declared_properties(&self, declared_properties: Vec<Id<Symbol>>);
    fn declared_call_signatures(&self) -> Ref<Vec<Id<Signature>>>;
    fn set_declared_call_signatures(&self, declared_call_signatures: Vec<Id<Signature>>);
    fn declared_construct_signatures(&self) -> Ref<Vec<Id<Signature>>>;
    fn set_declared_construct_signatures(&self, declared_construct_signatures: Vec<Id<Signature>>);
    fn declared_index_infos(&self) -> Ref<Vec<Id<IndexInfo>>>;
    fn set_declared_index_infos(&self, declared_index_infos: Vec<Id<IndexInfo>>);
}

impl GenericableTypeInterface for BaseInterfaceType {
    fn genericize(&self, instantiations: HashMap<String, Id<Type /*TypeReference*/>>) {
        *self.instantiations.borrow_mut() = Some(instantiations);
    }
}

impl GenericTypeInterface for BaseInterfaceType {
    fn instantiations(&self) -> RefMut<HashMap<String, Id<Type /*TypeReference*/>>> {
        ref_mut_unwrapped(&self.instantiations)
    }

    fn maybe_variances(&self) -> Rc<RefCell<Option<Vec<VarianceFlags>>>> {
        self.variances.clone()
    }

    fn set_variances(&self, variances: Vec<VarianceFlags>) {
        *self.variances.borrow_mut() = Some(variances);
    }
}

impl TypeReferenceInterface for BaseInterfaceType {
    fn target(&self) -> Id<Type> {
        self.target.get().unwrap()
    }

    fn set_target(&self, target: Id<Type>) {
        self.target.set(Some(target));
    }

    fn maybe_node(&self) -> Option<Id<Node>> {
        self.node.get()
    }

    fn set_node(&self, node: Option<Id<Node>>) {
        self.node.set(node);
    }

    fn maybe_literal_type(&self) -> Option<Id<Type>> {
        self.literal_type.get()
    }

    fn set_literal_type(&self, literal_type: Option<Id<Type>>) {
        self.literal_type.set(literal_type);
    }

    fn maybe_resolved_type_arguments(&self) -> Ref<Option<Vec<Id<Type>>>> {
        self.resolved_type_arguments.borrow()
    }

    fn maybe_resolved_type_arguments_mut(&self) -> RefMut<Option<Vec<Id<Type>>>> {
        self.resolved_type_arguments.borrow_mut()
    }

    fn maybe_cached_equivalent_base_type(&self) -> Option<Id<Type>> {
        self.cached_equivalent_base_type.get()
    }

    fn set_cached_equivalent_base_type(&self, cached_equivalent_base_type: Option<Id<Type>>) {
        self.cached_equivalent_base_type
            .set(cached_equivalent_base_type);
    }
}

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "ObjectType",
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface, FreshObjectLiteralTypeInterface"
)]
pub struct TypeReference {
    _object_type: BaseObjectType,
    pub target: Id<Type /*GenericType*/>,
    pub node: Cell<Option<Id<Node /*TypeReferenceNode | ArrayTypeNode | TupleTypeNode*/>>>, // TODO: should be weak?
    pub resolved_type_arguments: RefCell<Option<Vec<Id<Type>>>>,
    literal_type: Cell<Option<Id<Type /*TypeReference*/>>>,
    cached_equivalent_base_type: Cell<Option<Id<Type>>>,
}

impl TypeReference {
    pub fn new(
        object_type: BaseObjectType,
        target: Id<Type>,
        resolved_type_arguments: Option<Vec<Id<Type>>>,
    ) -> Self {
        Self {
            _object_type: object_type,
            target,
            node: Default::default(),
            resolved_type_arguments: RefCell::new(resolved_type_arguments),
            literal_type: Default::default(),
            cached_equivalent_base_type: Default::default(),
        }
    }
}

pub trait TypeReferenceInterface: ObjectTypeInterface {
    fn target(&self) -> Id<Type>;
    fn set_target(&self, target: Id<Type>);
    fn maybe_node(&self) -> Option<Id<Node>>;
    fn set_node(&self, node: Option<Id<Node>>);
    fn maybe_resolved_type_arguments(&self) -> Ref<Option<Vec<Id<Type>>>>;
    fn maybe_resolved_type_arguments_mut(&self) -> RefMut<Option<Vec<Id<Type>>>>;
    fn maybe_literal_type(&self) -> Option<Id<Type>>;
    fn set_literal_type(&self, literal_type: Option<Id<Type>>);
    fn maybe_cached_equivalent_base_type(&self) -> Option<Id<Type>>;
    fn set_cached_equivalent_base_type(&self, cached_equivalent_base_type: Option<Id<Type>>);
}

impl TypeReferenceInterface for TypeReference {
    fn target(&self) -> Id<Type> {
        self.target.clone()
    }

    fn set_target(&self, _target: Id<Type>) {
        panic!("Shouldn't call set_target() on a TypeReference")
    }

    fn maybe_node(&self) -> Option<Id<Node>> {
        self.node.get()
    }

    fn set_node(&self, node: Option<Id<Node>>) {
        self.node.set(node);
    }

    fn maybe_resolved_type_arguments(&self) -> Ref<Option<Vec<Id<Type>>>> {
        self.resolved_type_arguments.borrow()
    }

    fn maybe_resolved_type_arguments_mut(&self) -> RefMut<Option<Vec<Id<Type>>>> {
        self.resolved_type_arguments.borrow_mut()
    }

    fn maybe_literal_type(&self) -> Option<Id<Type>> {
        self.literal_type.get()
    }

    fn set_literal_type(&self, literal_type: Option<Id<Type>>) {
        self.literal_type.set(literal_type);
    }

    fn maybe_cached_equivalent_base_type(&self) -> Option<Id<Type>> {
        self.cached_equivalent_base_type.get()
    }

    fn set_cached_equivalent_base_type(&self, cached_equivalent_base_type: Option<Id<Type>>) {
        self.cached_equivalent_base_type
            .set(cached_equivalent_base_type);
    }
}

bitflags! {
    pub struct VarianceFlags: u32 {
        const Invariant = 0;
        const Covariant = 1 << 0;
        const Contravariant = 1 << 1;
        const Bivariant = Self::Covariant.bits | Self::Contravariant.bits;
        const Independent = 1 << 2;
        const VarianceMask = Self::Invariant.bits | Self::Covariant.bits | Self::Contravariant.bits | Self::Independent.bits;
        const Unmeasurable = 1 << 3;
        const Unreliable = 1 << 4;
        const AllowsStructuralFallback = Self::Unmeasurable.bits | Self::Unreliable.bits;
    }
}

pub trait GenericableTypeInterface: TypeInterface {
    fn genericize(&self, instantiations: HashMap<String, Id<Type /*TypeReference*/>>);
}

pub trait GenericTypeInterface:
    ObjectFlagsTypeInterface
    + ObjectTypeInterface
    + InterfaceTypeWithDeclaredMembersInterface
    + InterfaceTypeInterface
    + TypeReferenceInterface
{
    fn instantiations(&self) -> RefMut<HashMap<String, Id<Type /*TypeReference*/>>>;
    fn maybe_variances(&self) -> Rc<RefCell<Option<Vec<VarianceFlags>>>>;
    fn set_variances(&self, variances: Vec<VarianceFlags>);
}

bitflags! {
    pub struct ElementFlags: u32 {
        const None = 0;
        const Required = 1 << 0;
        const Optional = 1 << 1;
        const Rest = 1 << 2;
        const Variadic = 1 << 3;
        const Fixed = Self::Required.bits | Self::Optional.bits;
        const Variable = Self::Rest.bits | Self::Variadic.bits;
        const NonRequired = Self::Optional.bits | Self::Rest.bits | Self::Variadic.bits;
        const NonRest = Self::Required.bits | Self::Optional.bits | Self::Variadic.bits;
    }
}

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "InterfaceType, ObjectType",
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface, InterfaceTypeWithDeclaredMembersInterface, GenericableTypeInterface, GenericTypeInterface, InterfaceTypeInterface, TypeReferenceInterface, FreshObjectLiteralTypeInterface"
)]
pub struct TupleType {
    _interface_type: BaseInterfaceType,
    pub element_flags: Vec<ElementFlags>,
    pub min_length: usize,
    pub fixed_length: usize,
    pub has_rest_element: bool,
    pub combined_flags: ElementFlags,
    pub readonly: bool,
    pub labeled_element_declarations:
        Option<Vec<Id<Node /*NamedTupleMember | ParameterDeclaration*/>>>,
}

impl TupleType {
    pub fn new(
        interface_type: BaseInterfaceType,
        element_flags: Vec<ElementFlags>,
        min_length: usize,
        fixed_length: usize,
        has_rest_element: bool,
        combined_flags: ElementFlags,
        readonly: bool,
        labeled_element_declarations: Option<Vec<Id<Node>>>,
    ) -> Self {
        Self {
            _interface_type: interface_type,
            element_flags,
            min_length,
            fixed_length,
            has_rest_element,
            combined_flags,
            readonly,
            labeled_element_declarations,
        }
    }
}

pub trait UnionOrIntersectionTypeInterface: TypeInterface {
    fn types(&self) -> &[Id<Type>];
    fn maybe_property_cache(&self) -> RefMut<Option<SymbolTable>>;
    fn maybe_property_cache_without_object_function_property_augment(
        &self,
    ) -> RefMut<Option<SymbolTable>>;
    fn maybe_resolved_properties(&self) -> Option<Id<Vec<Id<Symbol>>>>;
    fn set_resolved_properties(&self, resolved_properties: Option<Id<Vec<Id<Symbol>>>>);
}

#[derive(Clone, Debug)]
#[type_type(
    interfaces = "UnionOrIntersectionTypeInterface, ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface, FreshObjectLiteralTypeInterface"
)]
pub enum UnionOrIntersectionType {
    UnionType(UnionType),
    IntersectionType(IntersectionType),
}

#[derive(Clone, Debug)]
#[type_type(impl_from = false)]
pub struct BaseUnionOrIntersectionType {
    _type: BaseType,
    pub types: Vec<Id<Type>>,
    pub object_flags: Cell<ObjectFlags>,
    property_cache: RefCell<Option<SymbolTable>>,
    property_cache_without_object_function_property_augment: RefCell<Option<SymbolTable>>,
    resolved_properties: Cell<Option<Id<Vec<Id<Symbol>>>>>,
    // ResolvedType Fields
    members: Cell<Option<Id<SymbolTable>>>,
    properties: Cell<Option<Id<Vec<Id<Symbol>>>>>,
    call_signatures: RefCell<Option<Vec<Id<Signature>>>>,
    construct_signatures: RefCell<Option<Vec<Id<Signature>>>>,
    index_infos: RefCell<Option<Vec<Id<IndexInfo>>>>,
}

impl BaseUnionOrIntersectionType {
    pub fn new(base_type: BaseType, types: Vec<Id<Type>>, object_flags: ObjectFlags) -> Self {
        Self {
            _type: base_type,
            types,
            object_flags: Cell::new(object_flags),
            resolved_properties: Default::default(),
            members: Default::default(),
            property_cache: Default::default(),
            property_cache_without_object_function_property_augment: Default::default(),
            properties: Default::default(),
            call_signatures: Default::default(),
            construct_signatures: Default::default(),
            index_infos: Default::default(),
        }
    }
}

impl UnionOrIntersectionTypeInterface for BaseUnionOrIntersectionType {
    fn types(&self) -> &[Id<Type>] {
        &self.types
    }

    fn maybe_property_cache(&self) -> RefMut<Option<SymbolTable>> {
        self.property_cache.borrow_mut()
    }

    fn maybe_property_cache_without_object_function_property_augment(
        &self,
    ) -> RefMut<Option<SymbolTable>> {
        self.property_cache_without_object_function_property_augment
            .borrow_mut()
    }

    fn maybe_resolved_properties(&self) -> Option<Id<Vec<Id<Symbol>>>> {
        self.resolved_properties.get()
    }

    fn set_resolved_properties(&self, resolved_properties: Option<Id<Vec<Id<Symbol>>>>) {
        self.resolved_properties.set(resolved_properties);
    }
}

impl ObjectFlagsTypeInterface for BaseUnionOrIntersectionType {
    fn object_flags(&self) -> ObjectFlags {
        self.object_flags.get()
    }

    fn set_object_flags(&self, object_flags: ObjectFlags) {
        self.object_flags.set(object_flags);
    }
}

impl ResolvableTypeInterface for BaseUnionOrIntersectionType {
    fn resolve(
        &self,
        members: Id<SymbolTable>,
        properties: Id<Vec<Id<Symbol>>>,
        call_signatures: Vec<Id<Signature>>,
        construct_signatures: Vec<Id<Signature>>,
        index_infos: Vec<Id<IndexInfo>>,
    ) {
        self.members.set(Some(members));
        self.properties.set(Some(properties));
        *self.call_signatures.borrow_mut() = Some(call_signatures);
        *self.construct_signatures.borrow_mut() = Some(construct_signatures);
        *self.index_infos.borrow_mut() = Some(index_infos);
    }

    fn is_resolved(&self) -> bool {
        self.members.get().is_some()
    }
}

impl ResolvedTypeInterface for BaseUnionOrIntersectionType {
    fn members(&self) -> Id<SymbolTable> {
        self.members.get().unwrap()
    }

    fn properties(&self) -> Id<Vec<Id<Symbol>>> {
        self.properties.get().unwrap()
    }

    fn set_properties(&self, properties: Id<Vec<Id<Symbol>>>) {
        self.properties.set(Some(properties));
    }

    fn call_signatures(&self) -> Ref<Vec<Id<Signature>>> {
        ref_unwrapped(&self.call_signatures)
    }

    fn set_call_signatures(&self, call_signatures: Vec<Id<Signature>>) {
        *self.call_signatures.borrow_mut() = Some(call_signatures);
    }

    fn construct_signatures(&self) -> Ref<Vec<Id<Signature>>> {
        ref_unwrapped(&self.construct_signatures)
    }

    fn set_construct_signatures(&self, construct_signatures: Vec<Id<Signature>>) {
        *self.construct_signatures.borrow_mut() = Some(construct_signatures);
    }

    fn index_infos(&self) -> Ref<Vec<Id<IndexInfo>>> {
        ref_unwrapped(&self.index_infos)
    }

    fn maybe_object_type_without_abstract_construct_signatures(&self) -> Option<Id<Type>> {
        None
    }

    fn set_object_type_without_abstract_construct_signatures(
        &self,
        _object_type_without_abstract_construct_signatures: Option<Id<Type>>,
    ) {
        panic!("Shouldn't call set_object_type_without_abstract_construct_signatures() on BaseUnionOrIntersectionType?")
    }
}

impl FreshObjectLiteralTypeInterface for BaseUnionOrIntersectionType {
    fn maybe_regular_type(&self) -> Option<Id<Type /*ResolvedType*/>> {
        panic!("Shouldn't call maybe_regular_type() on BaseUnionOrIntersectionType?")
    }

    fn set_regular_type(&self, _regular_type: Option<Id<Type /*ResolvedType*/>>) {
        panic!("Shouldn't call set_regular_type() on BaseUnionOrIntersectionType?")
    }
}

impl ObjectTypeInterface for BaseUnionOrIntersectionType {
    fn maybe_members(&self) -> Option<Id<SymbolTable>> {
        self.members.get()
    }

    fn set_members(&self, members: Option<Id<SymbolTable>>) {
        self.members.set(members);
    }

    fn maybe_properties(&self) -> Option<Id<Vec<Id<Symbol>>>> {
        self.properties.get()
    }

    fn maybe_call_signatures(&self) -> Ref<Option<Vec<Id<Signature>>>> {
        self.call_signatures.borrow()
    }

    fn maybe_target(&self) -> Option<Id<Type>> {
        panic!("Shouldn't call maybe_target() on BaseUnionOrIntersectionType?")
    }

    fn maybe_mapper(&self) -> Option<Id<TypeMapper>> {
        panic!("Shouldn't call maybe_mapper() on BaseUnionOrIntersectionType?")
    }

    fn maybe_instantiations(&self) -> RefMut<Option<HashMap<String, Id<Type>>>> {
        panic!("Shouldn't call maybe_instantiations() on BaseUnionOrIntersectionType?")
    }
}

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "UnionOrIntersectionType",
    interfaces = "UnionOrIntersectionTypeInterface, ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface, FreshObjectLiteralTypeInterface"
)]
pub struct UnionType {
    _union_or_intersection_type: BaseUnionOrIntersectionType,
    resolved_reduced_type: Cell<Option<Id<Type>>>,
    regular_type: Cell<Option<Id<Type /*UnionType*/>>>,
    pub(crate) origin: Option<Id<Type>>,
    key_property_name: RefCell<Option<__String>>,
    constituent_map: RefCell<Option<HashMap<TypeId, Id<Type>>>>,
}

impl UnionType {
    pub fn new(union_or_intersection_type: BaseUnionOrIntersectionType) -> Self {
        Self {
            _union_or_intersection_type: union_or_intersection_type,
            resolved_reduced_type: Default::default(),
            regular_type: Default::default(),
            origin: None,
            key_property_name: Default::default(),
            constituent_map: Default::default(),
        }
    }

    pub fn maybe_resolved_reduced_type(&self) -> Option<Id<Type>> {
        self.resolved_reduced_type.get()
    }

    pub fn set_resolved_reduced_type(&self, resolved_reduced_type: Option<Id<Type>>) {
        self.resolved_reduced_type.set(resolved_reduced_type);
    }

    pub fn maybe_regular_type(&self) -> Option<Id<Type>> {
        self.regular_type.get()
    }

    pub fn set_regular_type(&self, regular_type: Option<Id<Type>>) {
        self.regular_type.set(regular_type);
    }

    pub fn maybe_key_property_name(&self) -> RefMut<Option<__String>> {
        self.key_property_name.borrow_mut()
    }

    pub fn maybe_constituent_map(&self) -> RefMut<Option<HashMap<TypeId, Id<Type>>>> {
        self.constituent_map.borrow_mut()
    }
}
