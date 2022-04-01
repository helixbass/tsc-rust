#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use super::{
    BaseType, IndexInfo, IntersectionType, MappedType, Node, PseudoBigInt, ResolvedTypeInterface,
    Signature, Symbol, SymbolTable, Type, TypeChecker, TypeInterface,
};
use crate::{Number, TypeMapper, WeakSelf, __String};
use local_macros::type_type;

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
#[type_type]
pub struct UniqueESSymbolType {
    _type: BaseType,
    pub symbol: Rc<Symbol>,
    pub escaped_name: __String,
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
    fn set_members(&self, members: Option<Rc<RefCell<SymbolTable>>>);
    fn maybe_properties(&self) -> Ref<Option<Vec<Rc<Symbol>>>>;
    fn maybe_call_signatures(&self) -> Ref<Option<Vec<Rc<Signature>>>>;
    // fn maybe_properties(&self) -> Option<&[Rc<Symbol>]>;
    // fn properties(&self) -> &[Rc<Symbol>];
    // fn set_properties(&self, properties: Vec<Rc<Symbol>>);
    fn maybe_target(&self) -> Option<Rc<Type>>;
    fn maybe_mapper(&self) -> Option<&TypeMapper>;
}

#[derive(Clone, Debug)]
#[type_type(
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface"
)]
pub enum ObjectType {
    BaseObjectType(BaseObjectType),
    InterfaceType(InterfaceType),
    TypeReference(TypeReference),
    MappedType(MappedType),
}

#[derive(Clone, Debug)]
#[type_type(ancestors = "ObjectType")]
pub struct BaseObjectType {
    _type: BaseType,
    object_flags: Cell<ObjectFlags>,
    members: RefCell<Option<Rc<RefCell<SymbolTable>>>>,
    properties: RefCell<Option<Vec<Rc<Symbol>>>>,
    call_signatures: RefCell<Option<Vec<Rc<Signature>>>>,
    construct_signatures: RefCell<Option<Vec<Rc<Signature>>>>,
    index_infos: RefCell<Option<Vec<Rc<IndexInfo>>>>,
    object_type_without_abstract_construct_signatures: RefCell<Option<Rc<Type>>>,
    // AnonymousType fields
    target: Option<Rc<Type>>,
    mapper: Option<TypeMapper>,
    instantiations: Option<HashMap<String, Rc<Type>>>,
}

impl BaseObjectType {
    pub fn new(base_type: BaseType, object_flags: ObjectFlags) -> Self {
        Self {
            _type: base_type,
            object_flags: Cell::new(object_flags),
            members: RefCell::new(None),
            properties: RefCell::new(None),
            call_signatures: RefCell::new(None),
            construct_signatures: RefCell::new(None),
            index_infos: RefCell::new(None),
            object_type_without_abstract_construct_signatures: RefCell::new(None),
            target: None,
            mapper: None,
            instantiations: None,
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

impl ObjectTypeInterface for BaseObjectType {
    fn set_members(&self, members: Option<Rc<RefCell<SymbolTable>>>) {
        *self.members.borrow_mut() = members;
    }

    fn maybe_properties(&self) -> Ref<Option<Vec<Rc<Symbol>>>> {
        self.properties.borrow()
    }

    fn maybe_call_signatures(&self) -> Ref<Option<Vec<Rc<Signature>>>> {
        self.call_signatures.borrow()
    }

    fn maybe_target(&self) -> Option<Rc<Type>> {
        self.target.clone()
    }

    fn maybe_mapper(&self) -> Option<&TypeMapper> {
        self.mapper.as_ref()
    }
}

pub trait ResolvableTypeInterface {
    fn resolve(
        &self,
        members: Rc<RefCell<SymbolTable>>,
        properties: Vec<Rc<Symbol>>,
        call_signatures: Vec<Rc<Signature>>,
        construct_signatures: Vec<Rc<Signature>>,
        index_infos: Vec<Rc<IndexInfo>>,
    );
    fn is_resolved(&self) -> bool;
}

impl ResolvableTypeInterface for BaseObjectType {
    fn resolve(
        &self,
        members: Rc<RefCell<SymbolTable>>,
        properties: Vec<Rc<Symbol>>,
        call_signatures: Vec<Rc<Signature>>,
        construct_signatures: Vec<Rc<Signature>>,
        index_infos: Vec<Rc<IndexInfo>>,
    ) {
        *self.members.borrow_mut() = Some(members);
        *self.properties.borrow_mut() = Some(properties);
        *self.call_signatures.borrow_mut() = Some(call_signatures);
        *self.construct_signatures.borrow_mut() = Some(construct_signatures);
        *self.index_infos.borrow_mut() = Some(index_infos);
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

    fn call_signatures(&self) -> Ref<Vec<Rc<Signature>>> {
        Ref::map(self.call_signatures.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    fn set_call_signatures(&self, call_signatures: Vec<Rc<Signature>>) {
        *self.call_signatures.borrow_mut() = Some(call_signatures);
    }

    fn construct_signatures(&self) -> Ref<Vec<Rc<Signature>>> {
        Ref::map(self.construct_signatures.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    fn set_construct_signatures(&self, construct_signatures: Vec<Rc<Signature>>) {
        *self.construct_signatures.borrow_mut() = Some(construct_signatures);
    }

    fn index_infos(&self) -> Ref<Vec<Rc<IndexInfo>>> {
        Ref::map(self.index_infos.borrow(), |option| option.as_ref().unwrap())
    }

    fn maybe_object_type_without_abstract_construct_signatures(&self) -> Option<Rc<Type>> {
        self.object_type_without_abstract_construct_signatures
            .borrow()
            .clone()
    }

    fn set_object_type_without_abstract_construct_signatures(
        &self,
        object_type_without_abstract_construct_signatures: Option<Rc<Type>>,
    ) {
        *self
            .object_type_without_abstract_construct_signatures
            .borrow_mut() = object_type_without_abstract_construct_signatures;
    }
}

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "ObjectType",
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface, InterfaceTypeWithDeclaredMembersInterface, InterfaceTypeInterface, TypeReferenceInterface, GenericTypeInterface, GenericableTypeInterface"
)]
pub enum InterfaceType {
    BaseInterfaceType(BaseInterfaceType),
    TupleType(TupleType),
}

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "InterfaceType, ObjectType",
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface"
)]
pub struct BaseInterfaceType {
    _object_type: BaseObjectType,
    type_parameters: Option<Vec<Rc<Type /*TypeParameter*/>>>,
    outer_type_parameters: Option<Vec<Rc<Type /*TypeParameter*/>>>,
    local_type_parameters: Option<Vec<Rc<Type /*TypeParameter*/>>>,
    this_type: RefCell<Option<Rc<Type /*TypeParameter*/>>>,
    resolved_base_constructor_type: RefCell<Option<Rc<Type /*TypeParameter*/>>>,
    resolved_base_types: RefCell<Option<Rc<Vec<Rc<Type /*BaseType*/>>>>>,
    base_types_resolved: Cell<Option<bool>>,
    // InterfaceTypeWithDeclaredMembers fields
    declared_properties: RefCell<Option<Vec<Rc<Symbol>>>>,
    declared_call_signatures: RefCell<Option<Vec<Rc<Signature>>>>,
    declared_construct_signatures: RefCell<Option<Vec<Rc<Signature>>>>,
    declared_index_infos: RefCell<Option<Vec<Rc<IndexInfo>>>>,
    // GenericType fields
    instantiations: RefCell<Option<HashMap<String, Rc<Type /*TypeReference*/>>>>,
    variances: RefCell<Option<Vec<VarianceFlags>>>,
    // TypeReference fields (for GenericType)
    pub target: RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub node: RefCell<Option<Rc<Node /*TypeReferenceNode | ArrayTypeNode | TupleTypeNode*/>>>,
    pub resolved_type_arguments: RefCell<Option<Vec<Rc<Type>>>>,
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
            resolved_base_constructor_type: RefCell::new(None),
            resolved_base_types: RefCell::new(None),
            base_types_resolved: Cell::new(None),
            declared_properties: RefCell::new(None),
            declared_call_signatures: RefCell::new(None),
            declared_construct_signatures: RefCell::new(None),
            declared_index_infos: RefCell::new(None),
            instantiations: RefCell::new(None),
            variances: RefCell::new(None),
            target: RefCell::new(None),
            node: RefCell::new(None),
            resolved_type_arguments: RefCell::new(None),
        }
    }
}

pub trait InterfaceTypeInterface {
    fn maybe_type_parameters(&self) -> Option<&[Rc<Type>]>;
    fn maybe_outer_type_parameters(&self) -> Option<&[Rc<Type>]>;
    fn maybe_local_type_parameters(&self) -> Option<&[Rc<Type>]>;
    fn maybe_this_type(&self) -> Option<Rc<Type>>;
    fn maybe_this_type_mut(&self) -> RefMut<Option<Rc<Type>>>;
    fn maybe_resolved_base_constructor_type(&self) -> RefMut<Option<Rc<Type>>>;
    fn maybe_resolved_base_types(&self) -> RefMut<Option<Rc<Vec<Rc<Type>>>>>;
    fn maybe_base_types_resolved(&self) -> Option<bool>;
    fn set_base_types_resolved(&self, base_types_resolved: Option<bool>);
}

impl InterfaceTypeInterface for BaseInterfaceType {
    fn maybe_type_parameters(&self) -> Option<&[Rc<Type>]> {
        self.type_parameters.as_deref()
    }

    fn maybe_outer_type_parameters(&self) -> Option<&[Rc<Type>]> {
        self.outer_type_parameters.as_deref()
    }

    fn maybe_local_type_parameters(&self) -> Option<&[Rc<Type>]> {
        self.local_type_parameters.as_deref()
    }

    fn maybe_this_type(&self) -> Option<Rc<Type>> {
        self.this_type.borrow().clone()
    }

    fn maybe_this_type_mut(&self) -> RefMut<Option<Rc<Type>>> {
        self.this_type.borrow_mut()
    }

    fn maybe_resolved_base_constructor_type(&self) -> RefMut<Option<Rc<Type>>> {
        self.resolved_base_constructor_type.borrow_mut()
    }

    fn maybe_resolved_base_types(&self) -> RefMut<Option<Rc<Vec<Rc<Type>>>>> {
        self.resolved_base_types.borrow_mut()
    }

    fn maybe_base_types_resolved(&self) -> Option<bool> {
        self.base_types_resolved.get()
    }

    fn set_base_types_resolved(&self, base_types_resolved: Option<bool>) {
        self.base_types_resolved.set(base_types_resolved)
    }
}

impl InterfaceTypeWithDeclaredMembersInterface for BaseInterfaceType {
    fn maybe_declared_properties(&self) -> Ref<Option<Vec<Rc<Symbol>>>> {
        self.declared_properties.borrow()
    }

    fn set_declared_properties(&self, declared_properties: Vec<Rc<Symbol>>) {
        *self.declared_properties.borrow_mut() = Some(declared_properties);
    }

    fn declared_call_signatures(&self) -> Ref<Vec<Rc<Signature>>> {
        Ref::map(
            self.declared_call_signatures.borrow(),
            |declared_call_signatures| declared_call_signatures.as_ref().unwrap(),
        )
    }

    fn set_declared_call_signatures(&self, declared_call_signatures: Vec<Rc<Signature>>) {
        *self.declared_call_signatures.borrow_mut() = Some(declared_call_signatures);
    }

    fn declared_construct_signatures(&self) -> Ref<Vec<Rc<Signature>>> {
        Ref::map(
            self.declared_construct_signatures.borrow(),
            |declared_construct_signatures| declared_construct_signatures.as_ref().unwrap(),
        )
    }

    fn set_declared_construct_signatures(&self, declared_construct_signatures: Vec<Rc<Signature>>) {
        *self.declared_construct_signatures.borrow_mut() = Some(declared_construct_signatures);
    }

    fn declared_index_infos(&self) -> Ref<Vec<Rc<IndexInfo>>> {
        Ref::map(self.declared_index_infos.borrow(), |declared_index_infos| {
            declared_index_infos.as_ref().unwrap()
        })
    }

    fn set_declared_index_infos(&self, declared_index_infos: Vec<Rc<IndexInfo>>) {
        *self.declared_index_infos.borrow_mut() = Some(declared_index_infos);
    }
}

pub trait InterfaceTypeWithDeclaredMembersInterface {
    fn maybe_declared_properties(&self) -> Ref<Option<Vec<Rc<Symbol>>>>;
    fn set_declared_properties(&self, declared_properties: Vec<Rc<Symbol>>);
    fn declared_call_signatures(&self) -> Ref<Vec<Rc<Signature>>>;
    fn set_declared_call_signatures(&self, declared_call_signatures: Vec<Rc<Signature>>);
    fn declared_construct_signatures(&self) -> Ref<Vec<Rc<Signature>>>;
    fn set_declared_construct_signatures(&self, declared_construct_signatures: Vec<Rc<Signature>>);
    fn declared_index_infos(&self) -> Ref<Vec<Rc<IndexInfo>>>;
    fn set_declared_index_infos(&self, declared_index_infos: Vec<Rc<IndexInfo>>);
}

impl GenericableTypeInterface for BaseInterfaceType {
    fn genericize(&self, instantiations: HashMap<String, Rc<Type /*TypeReference*/>>) {
        *self.instantiations.borrow_mut() = Some(instantiations);
    }
}

impl GenericTypeInterface for BaseInterfaceType {
    fn instantiations(&self) -> RefMut<HashMap<String, Rc<Type /*TypeReference*/>>> {
        RefMut::map(self.instantiations.borrow_mut(), |instantiations| {
            instantiations.as_mut().unwrap()
        })
    }

    fn maybe_variances(&self) -> RefMut<Option<Vec<VarianceFlags>>> {
        self.variances.borrow_mut()
    }

    fn set_variances(&self, variances: Vec<VarianceFlags>) {
        *self.variances.borrow_mut() = Some(variances);
    }
}

impl TypeReferenceInterface for BaseInterfaceType {
    fn target(&self) -> Rc<Type> {
        self.target.borrow().clone().unwrap()
    }

    fn set_target(&self, target: Rc<Type>) {
        *self.target.borrow_mut() = Some(target);
    }

    fn maybe_node(&self) -> RefMut<Option<Rc<Node>>> {
        self.node.borrow_mut()
    }

    fn maybe_resolved_type_arguments(&self) -> RefMut<Option<Vec<Rc<Type>>>> {
        self.resolved_type_arguments.borrow_mut()
    }
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

pub trait TypeReferenceInterface {
    fn target(&self) -> Rc<Type>;
    fn set_target(&self, target: Rc<Type>);
    fn maybe_node(&self) -> RefMut<Option<Rc<Node>>>;
    fn maybe_resolved_type_arguments(&self) -> RefMut<Option<Vec<Rc<Type>>>>;
}

impl TypeReferenceInterface for TypeReference {
    fn target(&self) -> Rc<Type> {
        self.target.clone()
    }

    fn set_target(&self, _target: Rc<Type>) {
        panic!("Shouldn't call set_target() on a TypeReference")
    }

    fn maybe_node(&self) -> RefMut<Option<Rc<Node>>> {
        self.node.borrow_mut()
    }

    fn maybe_resolved_type_arguments(&self) -> RefMut<Option<Vec<Rc<Type>>>> {
        self.resolved_type_arguments.borrow_mut()
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
    fn genericize(&self, instantiations: HashMap<String, Rc<Type /*TypeReference*/>>);
}

pub trait GenericTypeInterface: TypeReferenceInterface {
    fn instantiations(&self) -> RefMut<HashMap<String, Rc<Type /*TypeReference*/>>>;
    fn maybe_variances(&self) -> RefMut<Option<Vec<VarianceFlags>>>;
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
    interfaces = "ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface, InterfaceTypeWithDeclaredMembersInterface, GenericableTypeInterface, GenericTypeInterface, InterfaceTypeInterface, TypeReferenceInterface"
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
        Option<Vec<Rc<Node /*NamedTupleMember | ParameterDeclaration*/>>>,
}

pub trait UnionOrIntersectionTypeInterface: TypeInterface {
    fn types(&self) -> &[Rc<Type>];
}

#[derive(Clone, Debug)]
#[type_type(interfaces = "UnionOrIntersectionTypeInterface, ObjectFlagsTypeInterface")]
pub enum UnionOrIntersectionType {
    UnionType(UnionType),
    IntersectionType(IntersectionType),
}

#[derive(Clone, Debug)]
#[type_type(impl_from = false)]
pub struct BaseUnionOrIntersectionType {
    _type: BaseType,
    pub types: Vec<Rc<Type>>,
    pub object_flags: Cell<ObjectFlags>,
    members: RefCell<Option<Rc<RefCell<SymbolTable>>>>,
    properties: RefCell<Option<Vec<Rc<Symbol>>>>,
    call_signatures: RefCell<Option<Vec<Rc<Signature>>>>,
    construct_signatures: RefCell<Option<Vec<Rc<Signature>>>>,
    index_infos: RefCell<Option<Vec<Rc<IndexInfo>>>>,
}

impl BaseUnionOrIntersectionType {
    pub fn new(base_type: BaseType, types: Vec<Rc<Type>>, object_flags: ObjectFlags) -> Self {
        Self {
            _type: base_type,
            types,
            object_flags: Cell::new(object_flags),
            members: RefCell::new(None),
            properties: RefCell::new(None),
            call_signatures: RefCell::new(None),
            construct_signatures: RefCell::new(None),
            index_infos: RefCell::new(None),
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

impl ResolvableTypeInterface for BaseUnionOrIntersectionType {
    fn resolve(
        &self,
        members: Rc<RefCell<SymbolTable>>,
        properties: Vec<Rc<Symbol>>,
        call_signatures: Vec<Rc<Signature>>,
        construct_signatures: Vec<Rc<Signature>>,
        index_infos: Vec<Rc<IndexInfo>>,
    ) {
        *self.members.borrow_mut() = Some(members);
        *self.properties.borrow_mut() = Some(properties);
        *self.call_signatures.borrow_mut() = Some(call_signatures);
        *self.construct_signatures.borrow_mut() = Some(construct_signatures);
        *self.index_infos.borrow_mut() = Some(index_infos);
    }

    fn is_resolved(&self) -> bool {
        self.members.borrow().is_some()
    }
}

impl ResolvedTypeInterface for BaseUnionOrIntersectionType {
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

    fn call_signatures(&self) -> Ref<Vec<Rc<Signature>>> {
        Ref::map(self.call_signatures.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    fn set_call_signatures(&self, call_signatures: Vec<Rc<Signature>>) {
        *self.call_signatures.borrow_mut() = Some(call_signatures);
    }

    fn construct_signatures(&self) -> Ref<Vec<Rc<Signature>>> {
        Ref::map(self.construct_signatures.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    fn set_construct_signatures(&self, construct_signatures: Vec<Rc<Signature>>) {
        *self.construct_signatures.borrow_mut() = Some(construct_signatures);
    }

    fn index_infos(&self) -> Ref<Vec<Rc<IndexInfo>>> {
        Ref::map(self.index_infos.borrow(), |option| option.as_ref().unwrap())
    }

    fn maybe_object_type_without_abstract_construct_signatures(&self) -> Option<Rc<Type>> {
        None
    }

    fn set_object_type_without_abstract_construct_signatures(
        &self,
        object_type_without_abstract_construct_signatures: Option<Rc<Type>>,
    ) {
        panic!("Shouldn't call set_object_type_without_abstract_construct_signatures() on BaseUnionOrIntersectionType?")
    }
}

impl ObjectTypeInterface for BaseUnionOrIntersectionType {
    fn set_members(&self, members: Option<Rc<RefCell<SymbolTable>>>) {
        *self.members.borrow_mut() = members;
    }

    fn maybe_properties(&self) -> Ref<Option<Vec<Rc<Symbol>>>> {
        self.properties.borrow()
    }

    fn maybe_call_signatures(&self) -> Ref<Option<Vec<Rc<Signature>>>> {
        self.call_signatures.borrow()
    }

    fn maybe_target(&self) -> Option<Rc<Type>> {
        panic!("Shouldn't call maybe_target() on BaseUnionOrIntersectionType?")
    }

    fn maybe_mapper(&self) -> Option<&TypeMapper> {
        panic!("Shouldn't call maybe_mapper() on BaseUnionOrIntersectionType?")
    }
}

#[derive(Clone, Debug)]
#[type_type(
    ancestors = "UnionOrIntersectionType",
    interfaces = "UnionOrIntersectionTypeInterface, ObjectFlagsTypeInterface, ObjectTypeInterface, ResolvableTypeInterface, ResolvedTypeInterface"
)]
pub struct UnionType {
    _union_or_intersection_type: BaseUnionOrIntersectionType,
    pub(crate) origin: Option<Rc<Type>>,
}

impl UnionType {
    pub fn new(union_or_intersection_type: BaseUnionOrIntersectionType) -> Self {
        Self {
            _union_or_intersection_type: union_or_intersection_type,
            origin: None,
        }
    }
}
