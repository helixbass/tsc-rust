#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::ops::BitAndAssign;
use std::ops::Deref;
use std::rc::{Rc, Weak};

use super::{
    BaseType, DiagnosticCollection, ModuleSpecifierResolutionHost, Node, NodeId, PseudoBigInt,
    RelationComparisonResult, ResolvedTypeInterface, SourceFile, Symbol, SymbolTable,
    SymbolTracker, Type, TypeChecker, TypeInterface,
};
use crate::{NodeBuilder, Number, SortedArray, WeakSelf};
use local_macros::{ast_type, enum_unwrapped, symbol_type, type_type};

pub trait LiteralTypeInterface: TypeInterface {
    fn fresh_type(&self) -> Option<&Weak<Type>>;
    fn set_fresh_type(&self, fresh_type: &Rc<Type>);
    fn get_or_initialize_fresh_type(&self, type_checker: &TypeChecker) -> Rc<Type>;
    fn regular_type(&self) -> Rc<Type>;
    fn set_regular_type(&self, regular_type: &Rc<Type>);
}

#[derive(Clone, Debug)]
#[type_type(interfaces = "LiteralTypeInterface")]
pub enum LiteralType {
    StringLiteralType(StringLiteralType),
    NumberLiteralType(NumberLiteralType),
    BigIntLiteralType(BigIntLiteralType),
}

#[derive(Clone, Debug)]
#[type_type(impl_from = false)]
pub struct BaseLiteralType {
    _type: BaseType,
    fresh_type: WeakSelf<Type>,
    regular_type: WeakSelf<Type>,
}

impl BaseLiteralType {
    pub fn new(type_: BaseType) -> Self {
        Self {
            _type: type_,
            fresh_type: WeakSelf::new(),
            regular_type: WeakSelf::new(),
        }
    }
}

impl LiteralTypeInterface for BaseLiteralType {
    fn fresh_type(&self) -> Option<&Weak<Type>> {
        self.fresh_type.try_get()
    }

    fn set_fresh_type(&self, fresh_type: &Rc<Type>) {
        self.fresh_type.init(fresh_type, false);
    }

    fn get_or_initialize_fresh_type(&self, type_checker: &TypeChecker) -> Rc<Type> {
        panic!("Shouldn't call get_or_initialize_fresh_type() on base BaseLiteralType");
    }

    fn regular_type(&self) -> Rc<Type> {
        self.regular_type.get().upgrade().unwrap()
    }

    fn set_regular_type(&self, regular_type: &Rc<Type>) {
        self.regular_type.init(regular_type, false);
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

    fn create_fresh_type_from_self(&self, type_checker: &TypeChecker) -> Rc<Type> {
        let fresh_type = type_checker.create_string_literal_type(
            self.flags(),
            self.value.clone(),
            Some(self.type_wrapper()),
        );
        fresh_type.as_literal_type().set_fresh_type(&fresh_type);
        self.set_fresh_type(&fresh_type);
        type_checker.keep_strong_reference_to_type(fresh_type);
        self.fresh_type().unwrap().upgrade().unwrap()
    }
}

impl LiteralTypeInterface for StringLiteralType {
    fn fresh_type(&self) -> Option<&Weak<Type>> {
        self._literal_type.fresh_type()
    }

    fn set_fresh_type(&self, fresh_type: &Rc<Type>) {
        self._literal_type.set_fresh_type(fresh_type);
    }

    fn get_or_initialize_fresh_type(&self, type_checker: &TypeChecker) -> Rc<Type> {
        if self.fresh_type().is_none() {
            let fresh_type = self.create_fresh_type_from_self(type_checker);
            self.set_fresh_type(&fresh_type);
            return self.fresh_type().unwrap().upgrade().unwrap();
        }
        return self.fresh_type().unwrap().upgrade().unwrap();
    }

    fn regular_type(&self) -> Rc<Type> {
        self._literal_type.regular_type()
    }

    fn set_regular_type(&self, regular_type: &Rc<Type>) {
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

    fn create_fresh_type_from_self(&self, type_checker: &TypeChecker) -> Rc<Type> {
        let fresh_type = type_checker.create_number_literal_type(
            self.flags(),
            self.value,
            Some(self.type_wrapper()),
        );
        fresh_type.as_literal_type().set_fresh_type(&fresh_type);
        self.set_fresh_type(&fresh_type);
        type_checker.keep_strong_reference_to_type(fresh_type);
        self.fresh_type().unwrap().upgrade().unwrap()
    }
}

impl LiteralTypeInterface for NumberLiteralType {
    fn fresh_type(&self) -> Option<&Weak<Type>> {
        self._literal_type.fresh_type()
    }

    fn set_fresh_type(&self, fresh_type: &Rc<Type>) {
        self._literal_type.set_fresh_type(fresh_type);
    }

    fn get_or_initialize_fresh_type(&self, type_checker: &TypeChecker) -> Rc<Type> {
        if self.fresh_type().is_none() {
            let fresh_type = self.create_fresh_type_from_self(type_checker);
            self.set_fresh_type(&fresh_type);
            return self.fresh_type().unwrap().upgrade().unwrap();
        }
        return self.fresh_type().unwrap().upgrade().unwrap();
    }

    fn regular_type(&self) -> Rc<Type> {
        self._literal_type.regular_type()
    }

    fn set_regular_type(&self, regular_type: &Rc<Type>) {
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

    fn create_fresh_type_from_self(&self, type_checker: &TypeChecker) -> Rc<Type> {
        let fresh_type = type_checker.create_big_int_literal_type(
            self.flags(),
            self.value.clone(),
            Some(self.type_wrapper()),
        );
        fresh_type.as_literal_type().set_fresh_type(&fresh_type);
        self.set_fresh_type(&fresh_type);
        type_checker.keep_strong_reference_to_type(fresh_type);
        self.fresh_type().unwrap().upgrade().unwrap()
    }
}

impl LiteralTypeInterface for BigIntLiteralType {
    fn fresh_type(&self) -> Option<&Weak<Type>> {
        self._literal_type.fresh_type()
    }

    fn set_fresh_type(&self, fresh_type: &Rc<Type>) {
        self._literal_type.set_fresh_type(fresh_type);
    }

    fn get_or_initialize_fresh_type(&self, type_checker: &TypeChecker) -> Rc<Type> {
        if self.fresh_type().is_none() {
            let fresh_type = self.create_fresh_type_from_self(type_checker);
            self.set_fresh_type(&fresh_type);
            return self.fresh_type().unwrap().upgrade().unwrap();
        }
        return self.fresh_type().unwrap().upgrade().unwrap();
    }

    fn regular_type(&self) -> Rc<Type> {
        self._literal_type.regular_type()
    }

    fn set_regular_type(&self, regular_type: &Rc<Type>) {
        self._literal_type.set_regular_type(regular_type);
    }
}

bitflags! {
    pub struct ObjectFlags: u32 {
        const None = 0;
        const Class = 1 << 0;
        const Interface = 1 << 1;
        const Reference = 1 << 2;
        const Anonymous = 1 << 4;
        const Mapped = 1 << 5;
        const ObjectLiteral = 1 << 7;
        const ObjectLiteralPatternWithComputedProperties = 1 << 9;
        const FreshLiteral = 1 << 14;
        const PrimitiveUnion = 1 << 16;
        const ContainsWideningType = 1 << 17;
        const ContainsObjectOrArrayLiteral = 1 << 18;
        const NonInferrableType = 1 << 19;
        const CouldContainTypeVariablesComputed = 1 << 20;
        const CouldContainTypeVariables = 1 << 21;
        const ContainsIntersections = 1 << 25;

        const ClassOrInterface = Self::Class.bits | Self::Interface.bits;
        const PropagatingFlags = Self::ContainsWideningType.bits | Self::ContainsObjectOrArrayLiteral.bits | Self::NonInferrableType.bits;
    }
}

pub trait ObjectFlagsTypeInterface {
    fn object_flags(&self) -> ObjectFlags;
    fn set_object_flags(&self, object_flags: ObjectFlags);
}

pub trait ObjectTypeInterface: ObjectFlagsTypeInterface {
    // fn maybe_properties(&self) -> Option<&[Rc<Symbol>]>;
    // fn properties(&self) -> &[Rc<Symbol>];
    // fn set_properties(&self, properties: Vec<Rc<Symbol>>);
}

#[derive(Clone, Debug)]
#[type_type(
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface"
)]
pub enum ObjectType {
    BaseObjectType(BaseObjectType),
    InterfaceType(InterfaceType),
    TypeReference(TypeReference),
}

#[derive(Clone, Debug)]
#[type_type(ancestors = "ObjectType")]
pub struct BaseObjectType {
    _type: BaseType,
    object_flags: Cell<ObjectFlags>,
    members: RefCell<Option<Rc<RefCell<SymbolTable>>>>,
    properties: RefCell<Option<Vec<Rc<Symbol>>>>,
}

impl BaseObjectType {
    pub fn new(base_type: BaseType, object_flags: ObjectFlags) -> Self {
        Self {
            _type: base_type,
            object_flags: Cell::new(object_flags),
            members: RefCell::new(None),
            properties: RefCell::new(None),
        }
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

impl ObjectTypeInterface for BaseObjectType {}

pub trait ResolvableTypeInterface {
    fn resolve(&self, members: Rc<RefCell<SymbolTable>>, properties: Vec<Rc<Symbol>>);
    fn is_resolved(&self) -> bool;
}

impl ResolvableTypeInterface for BaseObjectType {
    fn resolve(&self, members: Rc<RefCell<SymbolTable>>, properties: Vec<Rc<Symbol>>) {
        *self.members.borrow_mut() = Some(members);
        *self.properties.borrow_mut() = Some(properties);
    }

    fn is_resolved(&self) -> bool {
        self.members.borrow().is_some()
    }
}

impl ResolvedTypeInterface for BaseObjectType {
    fn members(&self) -> Rc<RefCell<SymbolTable>> {
        self.members.borrow_mut().as_ref().unwrap().clone()
    }

    fn properties(&self) -> RefMut<Vec<Rc<Symbol>>> {
        RefMut::map(self.properties.borrow_mut(), |option| {
            option.as_mut().unwrap()
        })
    }

    fn set_properties(&self, properties: Vec<Rc<Symbol>>) {
        *self.properties.borrow_mut() = Some(properties);
    }
}

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "ObjectType",
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface"
)]
pub enum InterfaceType {
    BaseInterfaceType(BaseInterfaceType),
}

impl InterfaceTypeWithDeclaredMembersInterface for InterfaceType {
    fn maybe_declared_properties(&self) -> Ref<Option<Vec<Rc<Symbol>>>> {
        match self {
            InterfaceType::BaseInterfaceType(base_interface_type) => {
                base_interface_type.maybe_declared_properties()
            }
        }
    }

    fn set_declared_properties(&self, declared_properties: Vec<Rc<Symbol>>) {
        match self {
            InterfaceType::BaseInterfaceType(base_interface_type) => {
                base_interface_type.set_declared_properties(declared_properties)
            }
        }
    }
}

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "InterfaceType, ObjectType",
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface"
)]
pub struct BaseInterfaceType {
    _object_type: BaseObjectType,
    pub type_parameters: Option<Vec<Rc<Type /*TypeParameter*/>>>,
    pub outer_type_parameters: Option<Vec<Rc<Type /*TypeParameter*/>>>,
    pub local_type_parameters: Option<Vec<Rc<Type /*TypeParameter*/>>>,
    pub this_type: RefCell<Option<Rc<Type /*TypeParameter*/>>>,
    declared_properties: RefCell<Option<Vec<Rc<Symbol>>>>,
}

impl BaseInterfaceType {
    pub fn new(
        object_type: BaseObjectType,
        type_parameters: Option<Vec<Rc<Type>>>,
        outer_type_parameters: Option<Vec<Rc<Type>>>,
        local_type_parameters: Option<Vec<Rc<Type>>>,
        this_type: Option<Rc<Type>>,
    ) -> Self {
        Self {
            _object_type: object_type,
            type_parameters,
            outer_type_parameters,
            local_type_parameters,
            this_type: RefCell::new(this_type),
            declared_properties: RefCell::new(None),
        }
    }
}

impl InterfaceTypeWithDeclaredMembersInterface for BaseInterfaceType {
    fn maybe_declared_properties(&self) -> Ref<Option<Vec<Rc<Symbol>>>> {
        self.declared_properties.borrow()
    }

    fn set_declared_properties(&self, declared_properties: Vec<Rc<Symbol>>) {
        *self.declared_properties.borrow_mut() = Some(declared_properties);
    }
}

pub trait InterfaceTypeWithDeclaredMembersInterface {
    fn maybe_declared_properties(&self) -> Ref<Option<Vec<Rc<Symbol>>>>;
    fn set_declared_properties(&self, declared_properties: Vec<Rc<Symbol>>);
}

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "ObjectType",
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface"
)]
pub struct TypeReference {
    _object_type: BaseObjectType,
    pub target: Rc<Type /*GenericType*/>,
    pub node: RefCell<Option<Rc<Node /*TypeReferenceNode | ArrayTypeNode | TupleTypeNode*/>>>, // TODO: should be weak?
    pub resolved_type_arguments: RefCell<Option<Vec<Rc<Type>>>>,
}

impl TypeReference {
    pub fn new(
        object_type: BaseObjectType,
        target: Rc<Type>,
        resolved_type_arguments: Option<Vec<Rc<Type>>>,
    ) -> Self {
        Self {
            _object_type: object_type,
            target,
            node: RefCell::new(None),
            resolved_type_arguments: RefCell::new(resolved_type_arguments),
        }
    }
}

pub trait UnionOrIntersectionTypeInterface: TypeInterface {
    fn types(&self) -> &[Rc<Type>];
}

#[derive(Clone, Debug)]
#[type_type(interfaces = "UnionOrIntersectionTypeInterface, ObjectFlagsTypeInterface")]
pub enum UnionOrIntersectionType {
    UnionType(UnionType),
}

#[derive(Clone, Debug)]
#[type_type(impl_from = false)]
pub struct BaseUnionOrIntersectionType {
    _type: BaseType,
    pub types: Vec<Rc<Type>>,
    pub object_flags: Cell<ObjectFlags>,
}

impl BaseUnionOrIntersectionType {
    pub fn new(base_type: BaseType, types: Vec<Rc<Type>>, object_flags: ObjectFlags) -> Self {
        Self {
            _type: base_type,
            types,
            object_flags: Cell::new(object_flags),
        }
    }
}

impl UnionOrIntersectionTypeInterface for BaseUnionOrIntersectionType {
    fn types(&self) -> &[Rc<Type>] {
        &self.types
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

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "UnionOrIntersectionType",
    interfaces = "UnionOrIntersectionTypeInterface, ObjectFlagsTypeInterface"
)]
pub struct UnionType {
    _union_or_intersection_type: BaseUnionOrIntersectionType,
}

impl UnionType {
    pub fn new(union_or_intersection_type: BaseUnionOrIntersectionType) -> Self {
        Self {
            _union_or_intersection_type: union_or_intersection_type,
        }
    }
}
