#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use super::{
    BaseType, DiagnosticCollection, ModuleSpecifierResolutionHost, Node, NodeId, NodeLinks,
    ObjectFlags, RelationComparisonResult, Signature, SignatureFlags, SymbolTable, SymbolTracker,
    Type, TypeFlags, TypeMapper, __String,
};
use crate::{NodeBuilder, Number};
use local_macros::symbol_type;

#[derive(Eq, PartialEq)]
pub enum StructureIsReused {
    Not,
    Completely,
}

#[allow(non_camel_case_types)]
pub enum ExitStatus {
    Success,
    DiagnosticsPresent_OutputsGenerated,
}

pub trait TypeCheckerHost: ModuleSpecifierResolutionHost {
    fn get_source_files(&self) -> Vec<Rc<Node>>;
}

#[derive(Debug)]
#[allow(non_snake_case)]
pub struct TypeChecker {
    pub _types_needing_strong_references: RefCell<Vec<Rc<Type>>>,
    pub produce_diagnostics: bool,
    pub Symbol: fn(SymbolFlags, __String) -> BaseSymbol,
    pub Type: fn(TypeFlags) -> BaseType,
    pub Signature: fn(SignatureFlags) -> Signature,
    pub(crate) type_count: Cell<u32>,
    pub(crate) empty_symbols: Rc<RefCell<SymbolTable>>,
    pub strict_null_checks: bool,
    pub fresh_object_literal_flag: ObjectFlags,
    pub exact_optional_property_types: bool,
    pub node_builder: NodeBuilder,
    pub globals: RefCell<SymbolTable>,
    pub string_literal_types: RefCell<HashMap<String, Rc</*StringLiteralType*/ Type>>>,
    pub number_literal_types: RefCell<HashMap<Number, Rc</*NumberLiteralType*/ Type>>>,
    pub big_int_literal_types: RefCell<HashMap<String, Rc</*BigIntLiteralType*/ Type>>>,
    pub unknown_symbol: Option<Rc<Symbol>>,
    pub any_type: Option<Rc<Type>>,
    pub error_type: Option<Rc<Type>>,
    pub unknown_type: Option<Rc<Type>>,
    pub undefined_type: Option<Rc<Type>>,
    pub null_type: Option<Rc<Type>>,
    pub string_type: Option<Rc<Type>>,
    pub number_type: Option<Rc<Type>>,
    pub bigint_type: Option<Rc<Type>>,
    pub true_type: Option<Rc<Type>>,
    pub regular_true_type: Option<Rc<Type>>,
    pub false_type: Option<Rc<Type>>,
    pub regular_false_type: Option<Rc<Type>>,
    pub boolean_type: Option<Rc<Type>>,
    pub void_type: Option<Rc<Type>>,
    pub never_type: Option<Rc<Type>>,
    pub number_or_big_int_type: Option<Rc<Type>>,
    pub template_constraint_type: Option<Rc<Type>>,
    pub empty_generic_type: Option<Rc<Type /*GenericType*/>>,
    pub global_array_type: Option<Rc<Type /*GenericType*/>>,
    pub deferred_global_promise_type: RefCell<Option<Rc<Type /*GenericType*/>>>,
    pub deferred_global_promise_constructor_symbol: RefCell<Option<Rc<Symbol>>>,
    pub symbol_links: RefCell<HashMap<SymbolId, Rc<RefCell<SymbolLinks>>>>,
    pub node_links: RefCell<HashMap<NodeId, Rc<RefCell<NodeLinks>>>>,
    pub diagnostics: RefCell<DiagnosticCollection>,
    pub assignable_relation: HashMap<String, RelationComparisonResult>,
    pub comparable_relation: HashMap<String, RelationComparisonResult>,
}

#[derive(PartialEq, Eq)]
pub enum UnionReduction {
    None,
    Literal,
    Subtype,
}

bitflags! {
    pub struct NodeBuilderFlags: u32 {
        const None = 0;
        const NoTruncation = 1 << 0;
        const WriteArrayAsGenericType = 1 << 1;
        const UseStructuralFallback = 1 << 3;
        const WriteTypeArgumentsOfSignature = 1 << 5;
        const UseFullyQualifiedType = 1 << 6;
        const UseOnlyExternalAliasing = 1 << 7;
        const SuppressAnyReturnType = 1 << 8;
        const WriteTypeParametersInQualifiedName = 1 << 9;
        const MultilineObjectLiterals = 1 << 10;
        const WriteClassExpressionAsTypeLiteral = 1 << 11;
        const UseTypeOfFunction = 1 << 12;
        const OmitParameterModifiers = 1 << 13;
        const UseAliasDefinedOutsideCurrentScope = 1 << 14;
        const UseSingleQuotesForStringLiteralType = 1 << 28;
        const NoTypeReduction = 1 << 29;

        const AllowThisInObjectLiteral = 1 << 15;
        const AllowQualifiedNameInPlaceOfIdentifier = 1 << 16;
        const AllowAnonymousIdentifier = 1 << 17;
        const AllowEmptyUnionOrIntersection = 1 << 18;
        const AllowEmptyTuple = 1 << 19;
        const AllowUniqueESSymbolType = 1 << 20;
        const AllowEmptyIndexInfoType = 1 << 21;

        const AllowNodeModulesRelativePaths = 1 << 26;
        const DoNotIncludeSymbolChain = 1 << 27;

        const InTypeAlias = 1 << 23;

        const IgnoreErrors = Self::AllowThisInObjectLiteral.bits | Self::AllowQualifiedNameInPlaceOfIdentifier.bits | Self::AllowAnonymousIdentifier.bits | Self::AllowEmptyUnionOrIntersection.bits | Self::AllowEmptyTuple.bits | Self::AllowEmptyIndexInfoType.bits | Self::AllowNodeModulesRelativePaths.bits;
    }
}

bitflags! {
    pub struct TypeFormatFlags: u32 {
        const None = 0;
        const NoTruncation = 1 << 0;
        const WriteArrayAsGenericType = 1 << 1;
        const UseStructuralFallback = 1 << 3;
        const WriteTypeArgumentsOfSignature = 1 << 5;
        const UseFullyQualifiedType = 1 << 6;
        const SuppressAnyReturnType = 1 << 8;
        const MultilineObjectLiterals = 1 << 10;
        const WriteClassExpressionAsTypeLiteral = 1 << 11;
        const UseTypeOfFunction = 1 << 12;
        const OmitParameterModifiers = 1 << 13;

        const UseAliasDefinedOutsideCurrentScope = 1 << 14;
        const UseSingleQuotesForStringLiteralType = 1 << 28;
        const NoTypeReduction = 1 << 29;

        const AllowUniqueESSymbolType = 1 << 20;

        const InTypeAlias = 1 << 23;

        const NodeBuilderFlagsMask = Self::NoTruncation.bits | Self::WriteArrayAsGenericType.bits | Self::UseStructuralFallback.bits | Self::WriteTypeArgumentsOfSignature.bits | Self::UseFullyQualifiedType.bits | Self::SuppressAnyReturnType.bits | Self::MultilineObjectLiterals.bits | Self::WriteClassExpressionAsTypeLiteral.bits | Self::UseTypeOfFunction.bits | Self::OmitParameterModifiers.bits | Self::UseAliasDefinedOutsideCurrentScope.bits | Self::AllowUniqueESSymbolType.bits | Self::InTypeAlias.bits | Self::UseSingleQuotesForStringLiteralType.bits | Self::NoTypeReduction.bits;
    }
}

bitflags! {
    pub struct SymbolFormatFlags: u32 {
        const None = 0;
        const WriteTypeParametersOrArguments = 1 << 0;
        const UseOnlyExternalAliasing = 1 << 1;
        const AllowAnyNodeKind = 1 << 2;
        const UseAliasDefinedOutsideCurrentScope = 1 << 3;
        const DoNotIncludeSymbolChain = 1 << 4;
    }
}

pub trait SymbolWriter: SymbolTracker {
    fn write_keyword(&mut self, text: &str);
    fn write_punctuation(&mut self, text: &str);
    fn write_space(&mut self, text: &str);
    fn write_string_literal(&mut self, text: &str);
    fn write_property(&mut self, text: &str);
    fn write_symbol(&mut self, text: &str, symbol: &Symbol);
    fn clear(&mut self);
}

pub type TypePredicate = ();

bitflags! {
    pub struct SymbolFlags: u32 {
        const None = 0;
        const FunctionScopedVariable = 1 << 0;
        const BlockScopedVariable = 1 << 1;
        const Property = 1 << 2;
        const EnumMember = 1 << 3;
        const Function = 1 << 4;
        const Class = 1 << 5;
        const Interface = 1 << 6;
        const ConstEnum = 1 << 7;
        const RegularEnum = 1 << 8;
        const ValueModule = 1 << 9;
        const TypeLiteral = 1 << 11;
        const ObjectLiteral = 1 << 12;
        const Method = 1 << 13;
        const GetAccessor = 1 << 15;
        const SetAccessor = 1 << 16;
        const TypeParameter = 1 << 18;
        const TypeAlias = 1 << 19;
        const ExportValue = 1 << 20;
        const Optional = 1 << 24;
        const Transient = 1 << 25;

        const Enum = Self::RegularEnum.bits | Self::ConstEnum.bits;
        const Variable = Self::FunctionScopedVariable.bits | Self::BlockScopedVariable.bits;
        const Value = Self::Variable.bits | Self::Property.bits | Self::EnumMember.bits | Self::ObjectLiteral.bits | Self::Function.bits | Self::Class.bits | Self::Enum.bits | Self::ValueModule.bits | Self::Method.bits | Self::GetAccessor.bits | Self::SetAccessor.bits;
        const Type = Self::Class.bits | Self::Interface.bits | Self::Enum.bits | Self::EnumMember.bits | Self::TypeLiteral.bits | Self::TypeParameter.bits | Self::TypeAlias.bits;

        const FunctionScopedVariableExcludes = Self::Value.bits & !Self::FunctionScopedVariable.bits;

        const BlockScopedVariableExcludes = Self::Value.bits;

        const ParameterExcludes = Self::Value.bits;
        const PropertyExcludes = Self::None.bits;
        const FunctionExcludes = Self::Value.bits & !(Self::Function.bits | Self::ValueModule.bits | Self::Class.bits);
        const InterfaceExcludes = Self::Type.bits & !(Self::Interface.bits | Self::Class.bits);
        const TypeParameterExcludes = Self::Type.bits & !Self::TypeParameter.bits;
        const TypeAliasExcludes = Self::Type.bits;
    }
}

pub type SymbolId = u32;

pub trait SymbolInterface {
    fn symbol_wrapper(&self) -> Rc<Symbol>;
    fn set_symbol_wrapper(&self, wrapper: Rc<Symbol>);
    fn flags(&self) -> SymbolFlags;
    fn set_flags(&self, flags: SymbolFlags);
    fn escaped_name(&self) -> &__String;
    fn maybe_declarations(&self) -> Ref<Option<Vec<Rc<Node>>>>;
    fn set_declarations(&self, declarations: Vec<Rc<Node>>);
    fn maybe_value_declaration(&self) -> Ref<Option<Weak<Node>>>;
    fn set_value_declaration(&self, node: Rc<Node>);
    fn maybe_members(&self) -> RefMut<Option<Rc<RefCell<SymbolTable>>>>;
    fn members(&self) -> Rc<RefCell<SymbolTable>>;
    fn maybe_id(&self) -> Option<SymbolId>;
    fn id(&self) -> SymbolId;
    fn set_id(&self, id: SymbolId);
}

#[derive(Debug)]
#[symbol_type(impl_from = false)]
pub enum Symbol {
    BaseSymbol(BaseSymbol),
    TransientSymbol(TransientSymbol),
}

impl Symbol {
    pub fn wrap(self) -> Rc<Symbol> {
        let rc = Rc::new(self);
        rc.set_symbol_wrapper(rc.clone());
        rc
    }
}

#[derive(Debug)]
pub struct BaseSymbol {
    _symbol_wrapper: RefCell<Option<Weak<Symbol>>>,
    flags: Cell<SymbolFlags>,
    escaped_name: __String,
    declarations: RefCell<Option<Vec<Rc<Node /*Declaration*/>>>>, // TODO: should be Vec<Weak<Node>> instead of Vec<Rc<Node>>?
    value_declaration: RefCell<Option<Weak<Node>>>,
    members: RefCell<Option<Rc<RefCell<SymbolTable>>>>,
    id: Cell<Option<SymbolId>>,
}

impl BaseSymbol {
    pub fn new(flags: SymbolFlags, name: __String) -> Self {
        Self {
            _symbol_wrapper: RefCell::new(None),
            flags: Cell::new(flags),
            escaped_name: name,
            declarations: RefCell::new(None),
            value_declaration: RefCell::new(None),
            members: RefCell::new(None),
            id: Cell::new(None),
        }
    }
}

impl SymbolInterface for BaseSymbol {
    fn symbol_wrapper(&self) -> Rc<Symbol> {
        self._symbol_wrapper
            .borrow()
            .as_ref()
            .unwrap()
            .upgrade()
            .unwrap()
    }

    fn set_symbol_wrapper(&self, wrapper: Rc<Symbol>) {
        *self._symbol_wrapper.borrow_mut() = Some(Rc::downgrade(&wrapper));
    }

    fn flags(&self) -> SymbolFlags {
        self.flags.get()
    }

    fn set_flags(&self, flags: SymbolFlags) {
        self.flags.set(flags);
    }

    fn escaped_name(&self) -> &__String {
        &self.escaped_name
    }

    fn maybe_declarations(&self) -> Ref<Option<Vec<Rc<Node>>>> {
        self.declarations.borrow()
    }

    fn set_declarations(&self, declarations: Vec<Rc<Node>>) {
        *self.declarations.borrow_mut() = Some(declarations);
    }

    fn maybe_value_declaration(&self) -> Ref<Option<Weak<Node>>> {
        self.value_declaration.borrow()
    }

    fn set_value_declaration(&self, node: Rc<Node>) {
        *self.value_declaration.borrow_mut() = Some(Rc::downgrade(&node));
    }

    fn maybe_members(&self) -> RefMut<Option<Rc<RefCell<SymbolTable>>>> {
        self.members.borrow_mut()
    }

    fn members(&self) -> Rc<RefCell<SymbolTable>> {
        self.members.borrow_mut().as_ref().unwrap().clone()
    }

    fn maybe_id(&self) -> Option<SymbolId> {
        self.id.get()
    }

    fn id(&self) -> SymbolId {
        self.id.get().unwrap()
    }

    fn set_id(&self, id: SymbolId) {
        self.id.set(Some(id));
    }
}

impl From<BaseSymbol> for Symbol {
    fn from(base_symbol: BaseSymbol) -> Self {
        Symbol::BaseSymbol(base_symbol)
    }
}

#[derive(Debug)]
pub struct SymbolLinks {
    pub target: Option<Rc<Symbol>>,
    pub type_: Option<Rc<Type>>,
    pub declared_type: Option<Rc<Type>>,
    pub mapper: Option<TypeMapper>,
}

impl SymbolLinks {
    pub fn new() -> Self {
        Self {
            target: None,
            type_: None,
            declared_type: None,
            mapper: None,
        }
    }
}

bitflags! {
    pub struct CheckFlags: u32 {
        const None = 0;
        const Instantiated = 1 << 0;
        const SyntheticProperty = 1 << 1;
        const SyntheticMethod = 1 << 2;
        const Readonly = 1 << 3;
        const Late = 1 << 12;
        const OptionalParameter = 1 << 14;
        const RestParameter = 1 << 15;

        const Synthetic = Self::SyntheticProperty.bits | Self::SyntheticMethod.bits;
    }
}

pub trait TransientSymbolInterface: SymbolInterface {
    fn symbol_links(&self) -> Rc<RefCell<SymbolLinks>>;
    fn check_flags(&self) -> CheckFlags;
}

#[derive(Debug)]
#[symbol_type(interfaces = "TransientSymbolInterface")]
pub enum TransientSymbol {
    BaseTransientSymbol(BaseTransientSymbol),
}

#[derive(Debug)]
#[symbol_type(ancestors = "TransientSymbol")]
pub struct BaseTransientSymbol {
    _symbol: BaseSymbol,
    _symbol_links: Rc<RefCell<SymbolLinks>>,
    check_flags: CheckFlags,
}

impl BaseTransientSymbol {
    pub fn new(base_symbol: BaseSymbol, check_flags: CheckFlags) -> Self {
        Self {
            _symbol: base_symbol,
            _symbol_links: Rc::new(RefCell::new(SymbolLinks::new())),
            check_flags,
        }
    }
}

impl TransientSymbolInterface for BaseTransientSymbol {
    fn symbol_links(&self) -> Rc<RefCell<SymbolLinks>> {
        self._symbol_links.clone()
    }

    fn check_flags(&self) -> CheckFlags {
        self.check_flags
    }
}
