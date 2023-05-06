use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use indexmap::IndexMap;
use regex::Regex;
use std::borrow::Borrow;
use std::cell::{Cell, Ref, RefMut};
use std::collections::HashMap;
use std::iter::FromIterator;
use std::ptr;
use std::{fmt, io};

use super::{is_not_accessor, is_not_overload};
use crate::{
    add_range, contains_parse_error, create_diagnostic_collection, create_symbol_table,
    escape_leading_underscores, find_ancestor, get_allow_synthetic_default_imports,
    get_emit_module_kind, get_emit_script_target, get_first_identifier, get_module_instance_state,
    get_parse_tree_node, get_strict_option_value, get_use_define_for_class_fields,
    is_assignment_pattern, is_call_like_expression, is_export_specifier, is_expression,
    is_function_like, is_identifier, is_jsx_attribute_like, is_object_literal_element_like,
    is_parameter, is_property_access_expression,
    is_property_access_or_qualified_name_or_import_type_node, is_source_file, is_type_node,
    object_allocator, parse_pseudo_big_int, return_ok_none_if_none, skip_type_checking, sum,
    unescape_leading_underscores, BaseInterfaceType, CancellationTokenDebuggable,
    CheckBinaryExpression, CheckFlags, ContextFlags, Debug_, Diagnostic, DiagnosticCategory,
    DiagnosticCollection, DiagnosticMessage, DiagnosticRelatedInformationInterface, Diagnostics,
    EmitResolver, EmitTextWriter, Extension, ExternalEmitHelpers, FlowNode, FlowType,
    FreshableIntrinsicType, GenericableTypeInterface, IndexInfo, IndexKind, InternalSymbolName,
    IterationTypeCacheKey, IterationTypes, JsxEmit, ModuleInstanceState, Node, NodeArray,
    NodeBuilder, NodeBuilderFlags, NodeCheckFlags, NodeFlags, NodeId, NodeInterface, NodeLinks,
    Number, ObjectFlags, OptionTry, OutofbandVarianceMarkerHandler, Path, PatternAmbientModule,
    PseudoBigInt, RelationComparisonResult, Signature, SignatureFlags, SignatureKind,
    StringOrNumber, Symbol, SymbolFlags, SymbolFormatFlags, SymbolId, SymbolInterface, SymbolTable,
    SymbolTracker, SymbolWalker, SyntaxKind, Type, TypeChecker, TypeCheckerHost,
    TypeCheckerHostDebuggable, TypeFlags, TypeFormatFlags, TypeId, TypeInterface,
    TypeMapperCallback, TypePredicate, TypePredicateKind, VarianceFlags,
};

lazy_static! {
    pub(super) static ref ambient_module_symbol_regex: Regex = Regex::new(r#"^".+"$"#).unwrap();
}

pub(super) const anon: &'static str/*__String*/ = "(anonymous)";

thread_local! {
    pub(super) static next_symbol_id: Cell<SymbolId> = Cell::new(1);
}

pub(super) fn get_next_symbol_id() -> SymbolId {
    next_symbol_id.with(|_next_symbol_id| _next_symbol_id.get())
}

pub(super) fn increment_next_symbol_id() {
    next_symbol_id.with(|_next_symbol_id| {
        _next_symbol_id.set(_next_symbol_id.get() + 1);
    });
}

thread_local! {
    pub(super) static next_node_id: Cell<NodeId> = Cell::new(1);
}

pub(super) fn get_next_node_id() -> NodeId {
    next_node_id.with(|_next_node_id| _next_node_id.get())
}

pub(super) fn increment_next_node_id() {
    next_node_id.with(|_next_node_id| {
        _next_node_id.set(_next_node_id.get() + 1);
    });
}

thread_local! {
    pub(super) static next_merge_id: Cell<u32> = Cell::new(1);
}

pub(super) fn get_next_merge_id() -> u32 {
    next_merge_id.with(|_next_merge_id| _next_merge_id.get())
}

pub(super) fn increment_next_merge_id() {
    next_merge_id.with(|_next_merge_id| {
        _next_merge_id.set(_next_merge_id.get() + 1);
    });
}

thread_local! {
    pub(super) static next_flow_id: Cell<usize> = Cell::new(1);
}

pub(super) fn get_next_flow_id() -> usize {
    next_flow_id.with(|_next_flow_id| _next_flow_id.get())
}

pub(super) fn increment_next_flow_id() {
    next_flow_id.with(|_next_flow_id| {
        _next_flow_id.set(_next_flow_id.get() + 1);
    });
}

bitflags! {
    pub(super) struct IterationUse: u32 {
        const None = 0;
        const AllowsSyncIterablesFlag = 1 << 0;
        const AllowsAsyncIterablesFlag = 1 << 1;
        const AllowsStringInputFlag = 1 << 2;
        const ForOfFlag = 1 << 3;
        const YieldStarFlag = 1 << 4;
        const SpreadFlag = 1 << 5;
        const DestructuringFlag = 1 << 6;
        const PossiblyOutOfBounds = 1 << 7;

        const Element = Self::AllowsSyncIterablesFlag.bits;
        const Spread = Self::AllowsSyncIterablesFlag.bits | Self::SpreadFlag.bits;
        const Destructuring = Self::AllowsSyncIterablesFlag.bits | Self::DestructuringFlag.bits;

        const ForOf = Self::AllowsSyncIterablesFlag.bits | Self::AllowsStringInputFlag.bits | Self::ForOfFlag.bits;
        const ForAwaitOf = Self::AllowsSyncIterablesFlag.bits | Self::AllowsAsyncIterablesFlag.bits | Self::AllowsStringInputFlag.bits | Self::ForOfFlag.bits;

        const YieldStar = Self::AllowsSyncIterablesFlag.bits | Self::YieldStarFlag.bits;
        const AsyncYieldStar = Self::AllowsSyncIterablesFlag.bits | Self::AllowsAsyncIterablesFlag.bits | Self::YieldStarFlag.bits;

        const GeneratorReturnType = Self::AllowsSyncIterablesFlag.bits;
        const AsyncGeneratorReturnType = Self::AllowsAsyncIterablesFlag.bits;
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum IterationTypeKind {
    Yield,
    Return,
    Next,
}

pub(crate) struct IterationTypesResolver {
    pub iterable_cache_key: IterationTypeCacheKey, /*"iterationTypesOfAsyncIterable" | "iterationTypesOfIterable"*/
    pub iterator_cache_key: IterationTypeCacheKey, /*"iterationTypesOfAsyncIterator" | "iterationTypesOfIterator"*/
    pub iterator_symbol_name: &'static str,        /*"asyncIterator" | "iterator"*/
    pub get_global_iterator_type: fn(&TypeChecker, report_errors: bool) -> Gc<Type /*GenericType*/>,
    pub get_global_iterable_type: fn(&TypeChecker, report_errors: bool) -> Gc<Type /*GenericType*/>,
    pub get_global_iterable_iterator_type:
        fn(&TypeChecker, report_errors: bool) -> Gc<Type /*GenericType*/>,
    pub get_global_generator_type:
        fn(&TypeChecker, report_errors: bool) -> Gc<Type /*GenericType*/>,
    pub resolve_iteration_type: fn(
        &TypeChecker,
        type_: &Type,
        error_node: Option<Gc<Node>>,
    ) -> io::Result<Option<Gc<Type>>>,
    pub must_have_a_next_method_diagnostic: &'static DiagnosticMessage,
    pub must_be_a_method_diagnostic: &'static DiagnosticMessage,
    pub must_have_a_value_diagnostic: &'static DiagnosticMessage,
}

impl fmt::Debug for IterationTypesResolver {
    fn fmt(&self, _f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum WideningKind {
    #[allow(dead_code)]
    Normal,
    FunctionReturn,
    GeneratorNext,
    GeneratorYield,
}

bitflags! {
    pub(super) struct TypeFacts: u32 {
        const None = 0;
        const TypeofEQString = 1 << 0;
        const TypeofEQNumber = 1 << 1;
        const TypeofEQBigInt = 1 << 2;
        const TypeofEQBoolean = 1 << 3;
        const TypeofEQSymbol = 1 << 4;
        const TypeofEQObject = 1 << 5;
        const TypeofEQFunction = 1 << 6;
        const TypeofEQHostObject = 1 << 7;
        const TypeofNEString = 1 << 8;
        const TypeofNENumber = 1 << 9;
        const TypeofNEBigInt = 1 << 10;
        const TypeofNEBoolean = 1 << 11;
        const TypeofNESymbol = 1 << 12;
        const TypeofNEObject = 1 << 13;
        const TypeofNEFunction = 1 << 14;
        const TypeofNEHostObject = 1 << 15;
        const EQUndefined = 1 << 16;
        const EQNull = 1 << 17;
        const EQUndefinedOrNull = 1 << 18;
        const NEUndefined = 1 << 19;
        const NENull = 1 << 20;
        const NEUndefinedOrNull = 1 << 21;
        const Truthy = 1 << 22;
        const Falsy = 1 << 23;
        const All = (1 << 24) - 1;
        const BaseStringStrictFacts = Self::TypeofEQString.bits | Self::TypeofNENumber.bits | Self::TypeofNEBigInt.bits | Self::TypeofNEBoolean.bits | Self::TypeofNESymbol.bits | Self::TypeofNEObject.bits | Self::TypeofNEFunction.bits | Self::TypeofNEHostObject.bits | Self::NEUndefined.bits | Self::NENull.bits | Self::NEUndefinedOrNull.bits;
        const BaseStringFacts = Self::BaseStringStrictFacts.bits | Self::EQUndefined.bits | Self::EQNull.bits | Self::EQUndefinedOrNull.bits | Self::Falsy.bits;
        const StringStrictFacts = Self::BaseStringStrictFacts.bits | Self::Truthy.bits | Self::Falsy.bits;
        const StringFacts = Self::BaseStringFacts.bits | Self::Truthy.bits;
        const EmptyStringStrictFacts = Self::BaseStringStrictFacts.bits | Self::Falsy.bits;
        const EmptyStringFacts = Self::BaseStringFacts.bits;
        const NonEmptyStringStrictFacts = Self::BaseStringStrictFacts.bits | Self::Truthy.bits;
        const NonEmptyStringFacts = Self::BaseStringFacts.bits | Self::Truthy.bits;
        const BaseNumberStrictFacts = Self::TypeofEQNumber.bits | Self::TypeofNEString.bits | Self::TypeofNEBigInt.bits | Self::TypeofNEBoolean.bits | Self::TypeofNESymbol.bits | Self::TypeofNEObject.bits | Self::TypeofNEFunction.bits | Self::TypeofNEHostObject.bits | Self::NEUndefined.bits | Self::NENull.bits | Self::NEUndefinedOrNull.bits;
        const BaseNumberFacts = Self::BaseNumberStrictFacts.bits | Self::EQUndefined.bits | Self::EQNull.bits | Self::EQUndefinedOrNull.bits | Self::Falsy.bits;
        const NumberStrictFacts = Self::BaseNumberStrictFacts.bits | Self::Truthy.bits | Self::Falsy.bits;
        const NumberFacts = Self::BaseNumberFacts.bits | Self::Truthy.bits;
        const ZeroNumberStrictFacts = Self::BaseNumberStrictFacts.bits | Self::Falsy.bits;
        const ZeroNumberFacts = Self::BaseNumberFacts.bits;
        const NonZeroNumberStrictFacts = Self::BaseNumberStrictFacts.bits | Self::Truthy.bits;
        const NonZeroNumberFacts = Self::BaseNumberFacts.bits | Self::Truthy.bits;
        const BaseBigIntStrictFacts = Self::TypeofEQBigInt.bits | Self::TypeofNEString.bits | Self::TypeofNENumber.bits | Self::TypeofNEBoolean.bits | Self::TypeofNESymbol.bits | Self::TypeofNEObject.bits | Self::TypeofNEFunction.bits | Self::TypeofNEHostObject.bits | Self::NEUndefined.bits | Self::NENull.bits | Self::NEUndefinedOrNull.bits;
        const BaseBigIntFacts = Self::BaseBigIntStrictFacts.bits | Self::EQUndefined.bits | Self::EQNull.bits | Self::EQUndefinedOrNull.bits | Self::Falsy.bits;
        const BigIntStrictFacts = Self::BaseBigIntStrictFacts.bits | Self::Truthy.bits | Self::Falsy.bits;
        const BigIntFacts = Self::BaseBigIntFacts.bits | Self::Truthy.bits;
        const ZeroBigIntStrictFacts = Self::BaseBigIntStrictFacts.bits | Self::Falsy.bits;
        const ZeroBigIntFacts = Self::BaseBigIntFacts.bits;
        const NonZeroBigIntStrictFacts = Self::BaseBigIntStrictFacts.bits | Self::Truthy.bits;
        const NonZeroBigIntFacts = Self::BaseBigIntFacts.bits | Self::Truthy.bits;
        const BaseBooleanStrictFacts = Self::TypeofEQBoolean.bits | Self::TypeofNEString.bits | Self::TypeofNENumber.bits | Self::TypeofNEBigInt.bits | Self::TypeofNESymbol.bits | Self::TypeofNEObject.bits | Self::TypeofNEFunction.bits | Self::TypeofNEHostObject.bits | Self::NEUndefined.bits | Self::NENull.bits | Self::NEUndefinedOrNull.bits;
        const BaseBooleanFacts = Self::BaseBooleanStrictFacts.bits | Self::EQUndefined.bits | Self::EQNull.bits | Self::EQUndefinedOrNull.bits | Self::Falsy.bits;
        const BooleanStrictFacts = Self::BaseBooleanStrictFacts.bits | Self::Truthy.bits | Self::Falsy.bits;
        const BooleanFacts = Self::BaseBooleanFacts.bits | Self::Truthy.bits;
        const FalseStrictFacts = Self::BaseBooleanStrictFacts.bits | Self::Falsy.bits;
        const FalseFacts = Self::BaseBooleanFacts.bits;
        const TrueStrictFacts = Self::BaseBooleanStrictFacts.bits | Self::Truthy.bits;
        const TrueFacts = Self::BaseBooleanFacts.bits | Self::Truthy.bits;
        const SymbolStrictFacts = Self::TypeofEQSymbol.bits | Self::TypeofNEString.bits | Self::TypeofNENumber.bits | Self::TypeofNEBigInt.bits | Self::TypeofNEBoolean.bits | Self::TypeofNEObject.bits | Self::TypeofNEFunction.bits | Self::TypeofNEHostObject.bits | Self::NEUndefined.bits | Self::NENull.bits | Self::NEUndefinedOrNull.bits | Self::Truthy.bits;
        const SymbolFacts = Self::SymbolStrictFacts.bits | Self::EQUndefined.bits | Self::EQNull.bits | Self::EQUndefinedOrNull.bits | Self::Falsy.bits;
        const ObjectStrictFacts = Self::TypeofEQObject.bits | Self::TypeofEQHostObject.bits | Self::TypeofNEString.bits | Self::TypeofNENumber.bits | Self::TypeofNEBigInt.bits | Self::TypeofNEBoolean.bits | Self::TypeofNESymbol.bits | Self::TypeofNEFunction.bits | Self::NEUndefined.bits | Self::NENull.bits | Self::NEUndefinedOrNull.bits | Self::Truthy.bits;
        const ObjectFacts = Self::ObjectStrictFacts.bits | Self::EQUndefined.bits | Self::EQNull.bits | Self::EQUndefinedOrNull.bits | Self::Falsy.bits;
        const FunctionStrictFacts = Self::TypeofEQFunction.bits | Self::TypeofEQHostObject.bits | Self::TypeofNEString.bits | Self::TypeofNENumber.bits | Self::TypeofNEBigInt.bits | Self::TypeofNEBoolean.bits | Self::TypeofNESymbol.bits | Self::TypeofNEObject.bits | Self::NEUndefined.bits | Self::NENull.bits | Self::NEUndefinedOrNull.bits | Self::Truthy.bits;
        const FunctionFacts = Self::FunctionStrictFacts.bits | Self::EQUndefined.bits | Self::EQNull.bits | Self::EQUndefinedOrNull.bits | Self::Falsy.bits;
        const UndefinedFacts = Self::TypeofNEString.bits | Self::TypeofNENumber.bits | Self::TypeofNEBigInt.bits | Self::TypeofNEBoolean.bits | Self::TypeofNESymbol.bits | Self::TypeofNEObject.bits | Self::TypeofNEFunction.bits | Self::TypeofNEHostObject.bits | Self::EQUndefined.bits | Self::EQUndefinedOrNull.bits | Self::NENull.bits | Self::Falsy.bits;
        const NullFacts = Self::TypeofEQObject.bits | Self::TypeofNEString.bits | Self::TypeofNENumber.bits | Self::TypeofNEBigInt.bits | Self::TypeofNEBoolean.bits | Self::TypeofNESymbol.bits | Self::TypeofNEFunction.bits | Self::TypeofNEHostObject.bits | Self::EQNull.bits | Self::EQUndefinedOrNull.bits | Self::NEUndefined.bits | Self::Falsy.bits;
        const EmptyObjectStrictFacts = Self::All.bits & !(Self::EQUndefined.bits | Self::EQNull.bits | Self::EQUndefinedOrNull.bits);
        const AllTypeofNE = Self::TypeofNEString.bits | Self::TypeofNENumber.bits | Self::TypeofNEBigInt.bits | Self::TypeofNEBoolean.bits | Self::TypeofNESymbol.bits | Self::TypeofNEObject.bits | Self::TypeofNEFunction.bits | Self::NEUndefined.bits;
        const EmptyObjectFacts = Self::All.bits;
    }
}

lazy_static! {
    pub(super) static ref typeof_eq_facts: HashMap<&'static str, TypeFacts> =
        HashMap::from_iter(IntoIterator::into_iter([
            ("string", TypeFacts::TypeofEQString),
            ("number", TypeFacts::TypeofEQNumber),
            ("bigint", TypeFacts::TypeofEQBigInt),
            ("boolean", TypeFacts::TypeofEQBoolean),
            ("symbol", TypeFacts::TypeofEQSymbol),
            ("undefined", TypeFacts::EQUndefined),
            ("object", TypeFacts::TypeofEQObject),
            ("function", TypeFacts::TypeofEQFunction),
        ]));
}

lazy_static! {
    pub(super) static ref typeof_ne_facts: HashMap<&'static str, TypeFacts> =
        HashMap::from_iter(IntoIterator::into_iter([
            ("string", TypeFacts::TypeofNEString),
            ("number", TypeFacts::TypeofNENumber),
            ("bigint", TypeFacts::TypeofNEBigInt),
            ("boolean", TypeFacts::TypeofNEBoolean),
            ("symbol", TypeFacts::TypeofNESymbol),
            ("undefined", TypeFacts::NEUndefined),
            ("object", TypeFacts::TypeofNEObject),
            ("function", TypeFacts::TypeofNEFunction),
        ]));
}

#[derive(Clone, Debug, Trace, Finalize)]
pub(crate) enum TypeSystemEntity {
    Node(Gc<Node>),
    Symbol(Gc<Symbol>),
    Type(Gc<Type>),
    Signature(Gc<Signature>),
}

impl TypeSystemEntity {
    pub fn as_node(&self) -> &Node {
        match self {
            Self::Node(node) => &*node,
            _ => panic!("Expected node"),
        }
    }

    pub fn as_symbol(&self) -> &Symbol {
        match self {
            Self::Symbol(symbol) => &*symbol,
            _ => panic!("Expected symbol"),
        }
    }

    pub fn as_type(&self) -> &Type {
        match self {
            Self::Type(type_) => &*type_,
            _ => panic!("Expected type"),
        }
    }

    pub fn as_signature(&self) -> &Signature {
        match self {
            Self::Signature(signature) => &*signature,
            _ => panic!("Expected signature"),
        }
    }
}

impl PartialEq for TypeSystemEntity {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Node(a), Self::Node(b)) => Gc::ptr_eq(a, b),
            (Self::Symbol(a), Self::Symbol(b)) => Gc::ptr_eq(a, b),
            (Self::Type(a), Self::Type(b)) => Gc::ptr_eq(a, b),
            (Self::Signature(a), Self::Signature(b)) => Gc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Eq for TypeSystemEntity {}

impl From<Gc<Node>> for TypeSystemEntity {
    fn from(value: Gc<Node>) -> Self {
        Self::Node(value)
    }
}

impl From<Gc<Symbol>> for TypeSystemEntity {
    fn from(value: Gc<Symbol>) -> Self {
        Self::Symbol(value)
    }
}

impl From<Gc<Type>> for TypeSystemEntity {
    fn from(value: Gc<Type>) -> Self {
        Self::Type(value)
    }
}

impl From<Gc<Signature>> for TypeSystemEntity {
    fn from(value: Gc<Signature>) -> Self {
        Self::Signature(value)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum TypeSystemPropertyName {
    Type,
    ResolvedBaseConstructorType,
    DeclaredType,
    ResolvedReturnType,
    ImmediateBaseConstraint,
    #[allow(dead_code)]
    EnumTagType,
    ResolvedTypeArguments,
    ResolvedBaseTypes,
}

bitflags! {
    pub struct CheckMode: u32 {
        const Normal = 0;
        const Contextual = 1 << 0;
        const Inferential = 1 << 1;
        const SkipContextSensitive = 1 << 2;
        const SkipGenericFunctions = 1 << 3;
        const IsForSignatureHelp = 1 << 4;
    }
}

bitflags! {
    pub(super) struct SignatureCheckMode: u32 {
        const None = 0;
        const BivariantCallback = 1 << 0;
        const StrictCallback = 1 << 1;
        const IgnoreReturnTypes = 1 << 2;
        const StrictArity = 1 << 3;
        const Callback = Self::BivariantCallback.bits | Self::StrictCallback.bits;
    }
}

bitflags! {
    pub(super) struct IntersectionState: u32 {
        const None = 0;
        const Source = 1 << 0;
        const Target = 1 << 1;
        const PropertyCheck = 1 << 2;
        const UnionIntersectionCheck = 1 << 3;
        const InPropertyCheck = 1 << 4;
    }
}

bitflags! {
    pub(super) struct RecursionFlags: u32 {
        const None = 0;
        const Source = 1 << 0;
        const Target = 1 << 1;
        const Both = Self::Source.bits | Self::Target.bits;
    }
}

bitflags! {
    pub(super) struct MappedTypeModifiers: u32 {
        const None = 0;
        const IncludeReadonly = 1 << 0;
        const ExcludeReadonly = 1 << 1;
        const IncludeOptional = 1 << 2;
        const ExcludeOptional = 1 << 3;
    }
}

bitflags! {
    pub(super) struct ExpandingFlags: u32 {
        const None = 0;
        const Source = 1;
        const Target = 1 << 1;
        const Both = Self::Source.bits | Self::Target.bits;
    }
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum MembersOrExportsResolutionKind {
    resolved_exports, // = "resolvedExports"
    resolved_members, // = "resolvedMembers"
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum UnusedKind {
    Local,
    Parameter,
}

// type AddUnusedDiagnostic = (containingNode: Node, type: UnusedKind, diagnostic: DiagnosticWithLocation) => void;

pub(super) fn is_not_overload_and_not_accessor(declaration: &Node /*Declaration*/) -> bool {
    is_not_overload(declaration) && is_not_accessor(declaration)
}

bitflags! {
    pub(super) struct DeclarationMeaning: u32 {
        const None = 0;
        const GetAccessor = 1;
        const SetAccessor = 2;
        const PropertyAssignment = 4;
        const Method = 8;
        const PrivateStatic = 16;
        const GetOrSetAccessor = Self::GetAccessor.bits | Self::SetAccessor.bits;
        const PropertyAssignmentOrMethod = Self::PropertyAssignment.bits | Self::Method.bits;
    }
}

bitflags! {
    pub(super) struct DeclarationSpaces: u32 {
        const None = 0;
        const ExportValue = 1 << 0;
        const ExportType = 1 << 1;
        const ExportNamespace = 1 << 2;
    }
}

bitflags! {
    pub(super) struct MinArgumentCountFlags: u32 {
        const None = 0;
        const StrongArityForUntypedJS = 1 << 0;
        const VoidIsNonOptional = 1 << 1;
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum IntrinsicTypeKind {
    Uppercase,
    Lowercase,
    Capitalize,
    Uncapitalize,
}

lazy_static! {
    pub(super) static ref intrinsic_type_kinds: HashMap<&'static str, IntrinsicTypeKind> =
        HashMap::from_iter(IntoIterator::into_iter([
            ("Uppercase", IntrinsicTypeKind::Uppercase),
            ("Lowercase", IntrinsicTypeKind::Lowercase),
            ("Capitalize", IntrinsicTypeKind::Capitalize),
            ("Uncapitalize", IntrinsicTypeKind::Uncapitalize),
        ]));
}

pub fn get_node_id(node: &Node) -> NodeId {
    if node.maybe_id().is_none() {
        node.set_id(get_next_node_id());
        increment_next_node_id();
    }

    node.id()
}

pub fn get_symbol_id(symbol: &Symbol) -> SymbolId {
    if symbol.maybe_id().is_none() {
        symbol.set_id(get_next_symbol_id());
        increment_next_symbol_id();
    }

    symbol.id()
}

pub fn is_instantiated_module(
    node: &Node, /*ModuleDeclaration*/
    preserve_const_enums: bool,
) -> bool {
    let module_state = get_module_instance_state(node, None);
    module_state == ModuleInstanceState::Instantiated
        || preserve_const_enums && module_state == ModuleInstanceState::ConstEnumOnly
}

pub fn create_type_checker(
    host: Gc<Box<dyn TypeCheckerHostDebuggable>>,
    produce_diagnostics: bool,
) -> io::Result<Gc<TypeChecker>> {
    let compiler_options = host.get_compiler_options();
    let mut type_checker = TypeChecker {
        host,
        produce_diagnostics,
        _rc_wrapper: Default::default(),
        _packages_map: Default::default(),
        cancellation_token: Default::default(),
        requested_external_emit_helpers: Cell::new(ExternalEmitHelpers::None),
        external_helpers_module: Default::default(),

        Symbol: object_allocator.get_symbol_constructor(),
        Type: object_allocator.get_type_constructor(),
        Signature: object_allocator.get_signature_constructor(),

        type_count: Default::default(),
        symbol_count: Default::default(),
        enum_count: Default::default(),
        total_instantiation_count: Default::default(),
        instantiation_count: Default::default(),
        instantiation_depth: Default::default(),
        inline_level: Default::default(),
        current_node: Default::default(),

        empty_symbols: Gc::new(GcCell::new(create_symbol_table(Option::<&[Gc<Symbol>]>::None))),

        compiler_options: compiler_options.clone(),
        language_version: get_emit_script_target(&compiler_options),
        module_kind: get_emit_module_kind(&compiler_options),
        use_define_for_class_fields: get_use_define_for_class_fields(&compiler_options),
        allow_synthetic_default_imports: get_allow_synthetic_default_imports(&compiler_options),
        strict_null_checks: get_strict_option_value(&compiler_options, "strictNullChecks"),
        strict_function_types: get_strict_option_value(&compiler_options, "strictFunctionTypes"),
        strict_bind_call_apply: get_strict_option_value(&compiler_options, "strictBindCallApply"),
        strict_property_initialization: get_strict_option_value(
            &compiler_options,
            "strictPropertyInitialization",
        ),
        no_implicit_any: get_strict_option_value(&compiler_options, "noImplicitAny"),
        no_implicit_this: get_strict_option_value(&compiler_options, "noImplicitThis"),
        use_unknown_in_catch_variables: get_strict_option_value(
            &compiler_options,
            "useUnknownInCatchVariables",
        ),
        keyof_strings_only: matches!(compiler_options.keyof_strings_only, Some(true)),
        fresh_object_literal_flag: if matches!(
            compiler_options.suppress_excess_property_errors,
            Some(true)
        ) {
            ObjectFlags::None
        } else {
            ObjectFlags::FreshLiteral
        },
        exact_optional_property_types: compiler_options.exact_optional_property_types,

        check_binary_expression: Default::default(),
        emit_resolver: Default::default(),
        node_builder: Default::default(),

        globals: Gc::new(GcCell::new(create_symbol_table(Option::<&[Gc<Symbol>]>::None))),
        undefined_symbol: Default::default(),
        global_this_symbol: Default::default(),

        arguments_symbol: Default::default(),
        require_symbol: Default::default(),

        apparent_argument_count: Default::default(),

        tuple_types: Default::default(),
        union_types: Default::default(),
        intersection_types: Default::default(),
        string_literal_types: Default::default(),
        number_literal_types: Default::default(),
        big_int_literal_types: Default::default(),
        enum_literal_types: Default::default(),
        indexed_access_types: Default::default(),
        template_literal_types: Default::default(),
        string_mapping_types: Default::default(),
        substitution_types: Default::default(),
        subtype_reduction_cache: Default::default(),
        evolving_array_types: Default::default(),
        undefined_properties: Default::default(),

        unknown_symbol: Default::default(),
        resolving_symbol: Default::default(),
        unresolved_symbols: Default::default(),
        error_types: Default::default(),

        any_type: Default::default(),
        auto_type: Default::default(),
        wildcard_type: Default::default(),
        error_type: Default::default(),
        unresolved_type: Default::default(),
        non_inferrable_any_type: Default::default(),
        intrinsic_marker_type: Default::default(),
        unknown_type: Default::default(),
        non_null_unknown_type: Default::default(),
        undefined_type: Default::default(),
        undefined_widening_type: Default::default(),
        optional_type: Default::default(),
        missing_type: Default::default(),
        null_type: Default::default(),
        null_widening_type: Default::default(),
        string_type: Default::default(),
        number_type: Default::default(),
        bigint_type: Default::default(),
        false_type: Default::default(),
        regular_false_type: Default::default(),
        true_type: Default::default(),
        regular_true_type: Default::default(),
        boolean_type: Default::default(),
        es_symbol_type: Default::default(),
        void_type: Default::default(),
        never_type: Default::default(),
        silent_never_type: Default::default(),
        non_inferrable_type: Default::default(),
        implicit_never_type: Default::default(),
        unreachable_never_type: Default::default(),
        non_primitive_type: Default::default(),
        string_or_number_type: Default::default(),
        string_number_symbol_type: Default::default(),
        keyof_constraint_type: Default::default(),
        number_or_big_int_type: Default::default(),
        template_constraint_type: Default::default(),

        restrictive_mapper: Default::default(),
        permissive_mapper: Default::default(),

        empty_object_type: Default::default(),
        empty_jsx_object_type: Default::default(),
        empty_type_literal_symbol: Default::default(),
        empty_type_literal_type: Default::default(),

        empty_generic_type: Default::default(),

        any_function_type: Default::default(),

        no_constraint_type: Default::default(),
        circular_constraint_type: Default::default(),
        resolving_default_type: Default::default(),

        marker_super_type: Default::default(),
        marker_sub_type: Default::default(),
        marker_other_type: Default::default(),

        no_type_predicate: Default::default(),

        any_signature: Default::default(),
        unknown_signature: Default::default(),
        resolving_signature: Default::default(),
        silent_never_signature: Default::default(),

        enum_number_index_info: Default::default(),

        iteration_types_cache: Default::default(),
        no_iteration_types: Gc::new(IterationTypes::new_no_iteration_types()),

        any_iteration_types: Default::default(),
        any_iteration_types_except_next: Default::default(),
        default_iteration_types: Default::default(),

        async_iteration_types_resolver:
            IterationTypesResolver {
                iterable_cache_key: IterationTypeCacheKey::IterationTypesOfAsyncIterable,
                iterator_cache_key: IterationTypeCacheKey::IterationTypesOfAsyncIterator,
                iterator_symbol_name: "asyncIterator",
                get_global_iterator_type: TypeChecker::get_global_async_iterator_type,
                get_global_iterable_type: TypeChecker::get_global_async_iterable_type,
                get_global_iterable_iterator_type: TypeChecker::get_global_async_iterable_iterator_type,
                get_global_generator_type: TypeChecker::get_global_async_generator_type,
                resolve_iteration_type: async_iteration_types_resolver_resolve_iteration_type,
                must_have_a_next_method_diagnostic: &Diagnostics::An_async_iterator_must_have_a_next_method,
                must_be_a_method_diagnostic: &Diagnostics::The_0_property_of_an_async_iterator_must_be_a_method,
                must_have_a_value_diagnostic: &Diagnostics::The_type_returned_by_the_0_method_of_an_async_iterator_must_be_a_promise_for_a_type_with_a_value_property,
            },

        sync_iteration_types_resolver:
            IterationTypesResolver {
                iterable_cache_key: IterationTypeCacheKey::IterationTypesOfIterable,
                iterator_cache_key: IterationTypeCacheKey::IterationTypesOfIterator,
                iterator_symbol_name: "iterator",
                get_global_iterator_type: TypeChecker::get_global_iterator_type,
                get_global_iterable_type: TypeChecker::get_global_iterable_type,
                get_global_iterable_iterator_type: TypeChecker::get_global_iterable_iterator_type,
                get_global_generator_type: TypeChecker::get_global_generator_type,
                resolve_iteration_type: sync_iteration_types_resolver_resolve_iteration_type,
                must_have_a_next_method_diagnostic: &Diagnostics::An_iterator_must_have_a_next_method,
                must_be_a_method_diagnostic: &Diagnostics::The_0_property_of_an_iterator_must_be_a_method,
                must_have_a_value_diagnostic: &Diagnostics::The_type_returned_by_the_0_method_of_an_iterator_must_have_a_value_property,
            },

        amalgamated_duplicates: Default::default(),

        reverse_mapped_cache: Default::default(),
        in_infer_type_for_homomorphic_mapped_type: Default::default(),
        ambient_modules_cache: Default::default(),
        pattern_ambient_modules: Default::default(),
        pattern_ambient_module_augmentations: Default::default(),

        global_object_type: Default::default(),
        global_function_type: Default::default(),
        global_callable_function_type: Default::default(),
        global_newable_function_type: Default::default(),
        global_array_type: Default::default(),
        global_readonly_array_type: Default::default(),
        global_string_type: Default::default(),
        global_number_type: Default::default(),
        global_boolean_type: Default::default(),
        global_reg_exp_type: Default::default(),
        global_this_type: Default::default(),
        any_array_type: Default::default(),
        auto_array_type: Default::default(),
        any_readonly_array_type: Default::default(),
        deferred_global_non_nullable_type_alias: Default::default(),

        deferred_global_es_symbol_constructor_symbol: Default::default(),
        deferred_global_es_symbol_constructor_type_symbol: Default::default(),
        deferred_global_es_symbol_type: Default::default(),
        deferred_global_typed_property_descriptor_type: Default::default(),
        deferred_global_promise_type: Default::default(),
        deferred_global_promise_like_type: Default::default(),
        deferred_global_promise_constructor_symbol: Default::default(),
        deferred_global_promise_constructor_like_type: Default::default(),
        deferred_global_iterable_type: Default::default(),
        deferred_global_iterator_type: Default::default(),
        deferred_global_iterable_iterator_type: Default::default(),
        deferred_global_generator_type: Default::default(),
        deferred_global_iterator_yield_result_type: Default::default(),
        deferred_global_iterator_return_result_type: Default::default(),
        deferred_global_async_iterable_type: Default::default(),
        deferred_global_async_iterator_type: Default::default(),
        deferred_global_async_iterable_iterator_type: Default::default(),
        deferred_global_async_generator_type: Default::default(),
        deferred_global_template_strings_array_type: Default::default(),
        deferred_global_import_meta_type: Default::default(),
        deferred_global_import_meta_expression_type: Default::default(),
        deferred_global_import_call_options_type: Default::default(),
        deferred_global_extract_symbol: Default::default(),
        deferred_global_omit_symbol: Default::default(),
        deferred_global_awaited_symbol: Default::default(),
        deferred_global_big_int_type: Default::default(),

        all_potentially_unused_identifiers: Default::default(),

        flow_loop_start: Default::default(),
        flow_loop_count: Default::default(),
        shared_flow_count: Default::default(),
        flow_analysis_disabled: Default::default(),
        flow_invocation_count: Default::default(),
        last_flow_node: Default::default(),
        last_flow_node_reachable: Default::default(),
        flow_type_cache: Default::default(),

        empty_string_type: Default::default(),
        zero_type: Default::default(),
        zero_big_int_type: Default::default(),

        resolution_targets: Default::default(),
        resolution_results: Default::default(),
        resolution_property_names: Default::default(),

        suggestion_count: Default::default(),
        maximum_suggestion_count: 10,
        merged_symbols: Default::default(),
        symbol_links: Default::default(),
        node_links: Default::default(),
        flow_loop_caches: Default::default(),
        flow_loop_nodes: Default::default(),
        flow_loop_keys: Default::default(),
        flow_loop_types: Default::default(),
        shared_flow_nodes: Default::default(),
        shared_flow_types: Default::default(),
        flow_node_reachable: Default::default(),
        flow_node_post_super: Default::default(),
        potential_this_collisions: Default::default(),
        potential_new_target_collisions: Default::default(),
        potential_weak_map_set_collisions: Default::default(),
        potential_reflect_collisions: Default::default(),
        awaited_type_stack: Default::default(),

        diagnostics: GcCell::new(create_diagnostic_collection()),
        suggestion_diagnostics: GcCell::new(create_diagnostic_collection()),

        typeof_types_by_name: Default::default(),
        typeof_type: Default::default(),

        _jsx_namespace: Default::default(),
        _jsx_factory_entity: Default::default(),
        outofband_variance_marker_handler: Default::default(),

        subtype_relation: Default::default(),
        strict_subtype_relation: Default::default(),
        assignable_relation: Default::default(),
        comparable_relation: Default::default(),
        identity_relation: Default::default(),
        enum_relation: Default::default(),

        builtin_globals: Default::default(),

        suggested_extensions: vec![
            (".mts", ".mjs"),
            (".ts", ".js"),
            (".cts", ".cjs"),
            (".mjs", ".mjs"),
            (".js", ".js"),
            (".cjs", ".cjs"),
            (".tsx", if matches!(compiler_options.jsx, Some(JsxEmit::Preserve)) {
                ".jsx"
            } else {
                ".js"
            }),
            (".jsx", ".jsx"),
            (".json", ".json"),
        ],
    };
    type_checker.undefined_symbol = Some(
        type_checker
            .create_symbol(SymbolFlags::Property, "undefined".to_owned(), None)
            .into(),
    );
    type_checker
        .undefined_symbol
        .as_ref()
        .unwrap()
        .set_declarations(vec![]);
    type_checker.global_this_symbol = Some(
        type_checker
            .create_symbol(
                SymbolFlags::Module,
                "globalThis".to_owned(),
                Some(CheckFlags::Readonly),
            )
            .into(),
    );
    let global_this_symbol = type_checker.global_this_symbol();
    {
        let mut global_this_symbol_exports = global_this_symbol.maybe_exports_mut();
        *global_this_symbol_exports = Some(type_checker.globals_rc());
    }
    global_this_symbol.set_declarations(vec![]);
    type_checker.globals_mut().insert(
        global_this_symbol.escaped_name().to_owned(),
        global_this_symbol,
    );
    type_checker.arguments_symbol = Some(
        type_checker
            .create_symbol(SymbolFlags::Property, "arguments".to_owned(), None)
            .into(),
    );
    type_checker.require_symbol = Some(
        type_checker
            .create_symbol(SymbolFlags::Property, "require".to_owned(), None)
            .into(),
    );
    type_checker.unknown_symbol = Some(
        type_checker
            .create_symbol(SymbolFlags::Property, "unknown".to_owned(), None)
            .into(),
    );
    type_checker.resolving_symbol = Some(
        type_checker
            .create_symbol(
                SymbolFlags::None,
                InternalSymbolName::Resolving.to_owned(),
                None,
            )
            .into(),
    );

    type_checker.any_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Any, "any", None)
            .into(),
    );
    type_checker.auto_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Any, "any", None)
            .into(),
    );
    type_checker.wildcard_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Any, "any", None)
            .into(),
    );
    type_checker.error_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Any, "error", None)
            .into(),
    );
    type_checker.unresolved_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Any, "unresolved", None)
            .into(),
    );
    type_checker.non_inferrable_any_type = Some(
        type_checker
            .create_intrinsic_type(
                TypeFlags::Any,
                "any",
                Some(ObjectFlags::ContainsWideningType),
            )
            .into(),
    );
    type_checker.intrinsic_marker_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Any, "intrinsic", None)
            .into(),
    );
    type_checker.unknown_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Unknown, "unknown", None)
            .into(),
    );
    type_checker.non_null_unknown_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Unknown, "unknown", None)
            .into(),
    );
    type_checker.undefined_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Undefined, "undefined", None)
            .into(),
    );
    type_checker.undefined_widening_type = Some(if type_checker.strict_null_checks {
        type_checker.undefined_type()
    } else {
        type_checker
            .create_intrinsic_type(
                TypeFlags::Undefined,
                "undefined",
                Some(ObjectFlags::ContainsWideningType),
            )
            .into()
    });
    type_checker.optional_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Undefined, "undefined", None)
            .into(),
    );
    type_checker.missing_type = Some(
        if matches!(type_checker.exact_optional_property_types, Some(true)) {
            type_checker
                .create_intrinsic_type(TypeFlags::Undefined, "undefined", None)
                .into()
        } else {
            type_checker.undefined_type()
        },
    );
    type_checker.null_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Null, "null", None)
            .into(),
    );
    type_checker.null_widening_type = Some(if type_checker.strict_null_checks {
        type_checker.null_type()
    } else {
        type_checker
            .create_intrinsic_type(
                TypeFlags::Null,
                "null",
                Some(ObjectFlags::ContainsWideningType),
            )
            .into()
    });
    type_checker.string_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::String, "string", None)
            .into(),
    );
    type_checker.number_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Number, "number", None)
            .into(),
    );
    type_checker.bigint_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::BigInt, "bigint", None)
            .into(),
    );
    let false_type: Gc<Type> = FreshableIntrinsicType::new(type_checker.create_intrinsic_type(
        TypeFlags::BooleanLiteral,
        "false",
        None,
    ))
    .into();
    let regular_false_type: Gc<Type> = FreshableIntrinsicType::new(
        type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "false", None),
    )
    .into();
    let false_type_as_freshable_intrinsic_type = false_type.as_freshable_intrinsic_type();
    false_type_as_freshable_intrinsic_type.set_regular_type(regular_false_type.clone());
    false_type_as_freshable_intrinsic_type.set_fresh_type(false_type.clone());
    type_checker.false_type = Some(false_type);
    let regular_false_type_as_freshable_intrinsic_type =
        regular_false_type.as_freshable_intrinsic_type();
    regular_false_type_as_freshable_intrinsic_type.set_regular_type(regular_false_type.clone());
    regular_false_type_as_freshable_intrinsic_type.set_fresh_type(type_checker.false_type());
    type_checker.regular_false_type = Some(regular_false_type);
    let true_type: Gc<Type> = FreshableIntrinsicType::new(type_checker.create_intrinsic_type(
        TypeFlags::BooleanLiteral,
        "true",
        None,
    ))
    .into();
    let regular_true_type: Gc<Type> = FreshableIntrinsicType::new(
        type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "true", None),
    )
    .into();
    let true_type_as_freshable_intrinsic_type = true_type.as_freshable_intrinsic_type();
    true_type_as_freshable_intrinsic_type.set_regular_type(regular_true_type.clone());
    true_type_as_freshable_intrinsic_type.set_fresh_type(true_type.clone());
    type_checker.true_type = Some(true_type);
    let regular_true_type_as_freshable_intrinsic_type =
        regular_true_type.as_freshable_intrinsic_type();
    regular_true_type_as_freshable_intrinsic_type.set_regular_type(regular_true_type.clone());
    regular_true_type_as_freshable_intrinsic_type.set_fresh_type(type_checker.true_type());
    type_checker.regular_true_type = Some(regular_true_type);
    type_checker.boolean_type = Some(type_checker.get_union_type(
        &[
            type_checker.regular_false_type(),
            type_checker.regular_true_type(),
        ],
        None,
        Option::<&Symbol>::None,
        None,
        Option::<&Type>::None,
    )?);
    type_checker.es_symbol_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::ESSymbol, "symbol", None)
            .into(),
    );
    type_checker.void_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Void, "void", None)
            .into(),
    );
    type_checker.never_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Never, "never", None)
            .into(),
    );
    type_checker.silent_never_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Never, "never", None)
            .into(),
    );
    type_checker.non_inferrable_type = Some(
        type_checker
            .create_intrinsic_type(
                TypeFlags::Never,
                "never",
                Some(ObjectFlags::NonInferrableType),
            )
            .into(),
    );
    type_checker.implicit_never_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Never, "never", None)
            .into(),
    );
    type_checker.unreachable_never_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Never, "never", None)
            .into(),
    );
    type_checker.non_primitive_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::NonPrimitive, "object", None)
            .into(),
    );
    type_checker.string_or_number_type = Some(type_checker.get_union_type(
        &[type_checker.string_type(), type_checker.number_type()],
        None,
        Option::<&Symbol>::None,
        None,
        Option::<&Type>::None,
    )?);
    type_checker.string_number_symbol_type = Some(type_checker.get_union_type(
        &[
            type_checker.string_type(),
            type_checker.number_type(),
            type_checker.es_symbol_type(),
        ],
        None,
        Option::<&Symbol>::None,
        None,
        Option::<&Type>::None,
    )?);
    type_checker.keyof_constraint_type = Some(if type_checker.keyof_strings_only {
        type_checker.string_type()
    } else {
        type_checker.string_number_symbol_type()
    });
    type_checker.number_or_big_int_type = Some(type_checker.get_union_type(
        &[type_checker.number_type(), type_checker.bigint_type()],
        None,
        Option::<&Symbol>::None,
        None,
        Option::<&Type>::None,
    )?);
    type_checker.template_constraint_type = Some(type_checker.get_union_type(
        &[
            type_checker.string_type(),
            type_checker.number_type(),
            type_checker.boolean_type(),
            type_checker.bigint_type(),
            type_checker.null_type(),
            type_checker.undefined_type(),
        ],
        None,
        Option::<&Symbol>::None,
        None,
        Option::<&Type>::None,
    )?);

    type_checker.restrictive_mapper = Some(Gc::new(
        type_checker.make_function_type_mapper(RestrictiveMapperFunc::default()),
    ));
    type_checker.permissive_mapper = Some(Gc::new(
        type_checker.make_function_type_mapper(PermissiveMapperFunc::default()),
    ));

    type_checker.empty_object_type = Some(type_checker.create_anonymous_type(
        Option::<&Symbol>::None,
        type_checker.empty_symbols(),
        vec![],
        vec![],
        vec![],
    ));
    let empty_jsx_object_type = type_checker.create_anonymous_type(
        Option::<&Symbol>::None,
        type_checker.empty_symbols(),
        vec![],
        vec![],
        vec![],
    );
    let empty_jsx_object_type_as_object_flags_type = empty_jsx_object_type.as_object_flags_type();
    empty_jsx_object_type_as_object_flags_type.set_object_flags(
        empty_jsx_object_type_as_object_flags_type.object_flags() | ObjectFlags::JsxAttributes,
    );
    type_checker.empty_jsx_object_type = Some(empty_jsx_object_type);

    let empty_type_literal_symbol = type_checker.create_symbol(
        SymbolFlags::TypeLiteral,
        InternalSymbolName::Type.to_owned(),
        None,
    );
    *empty_type_literal_symbol.maybe_members_mut() = Some(Gc::new(GcCell::new(
        create_symbol_table(Option::<&[Gc<Symbol>]>::None),
    )));
    type_checker.empty_type_literal_symbol = Some(empty_type_literal_symbol.into());
    type_checker.empty_type_literal_type = Some(type_checker.create_anonymous_type(
        Some(type_checker.empty_type_literal_symbol()),
        type_checker.empty_symbols(),
        vec![],
        vec![],
        vec![],
    ));

    let empty_generic_type = type_checker.create_anonymous_type_returning_base_object_type(
        Option::<&Symbol>::None,
        type_checker.empty_symbols(),
        vec![],
        vec![],
        vec![],
    );
    let empty_generic_type = BaseInterfaceType::new(empty_generic_type, None, None, None, None);
    empty_generic_type.genericize(HashMap::new());
    type_checker.empty_generic_type = Some(empty_generic_type.into());

    let any_function_type = type_checker.create_anonymous_type(
        Option::<&Symbol>::None,
        type_checker.empty_symbols(),
        vec![],
        vec![],
        vec![],
    );
    let any_function_type_as_object_flags_type = any_function_type.as_object_flags_type();
    any_function_type_as_object_flags_type.set_object_flags(
        any_function_type_as_object_flags_type.object_flags() | ObjectFlags::NonInferrableType,
    );
    type_checker.any_function_type = Some(any_function_type);

    type_checker.no_constraint_type = Some(type_checker.create_anonymous_type(
        Option::<&Symbol>::None,
        type_checker.empty_symbols(),
        vec![],
        vec![],
        vec![],
    ));
    type_checker.circular_constraint_type = Some(type_checker.create_anonymous_type(
        Option::<&Symbol>::None,
        type_checker.empty_symbols(),
        vec![],
        vec![],
        vec![],
    ));
    type_checker.resolving_default_type = Some(type_checker.create_anonymous_type(
        Option::<&Symbol>::None,
        type_checker.empty_symbols(),
        vec![],
        vec![],
        vec![],
    ));

    type_checker.marker_super_type = Some(
        type_checker
            .create_type_parameter(Option::<&Symbol>::None)
            .into(),
    );
    let marker_sub_type = type_checker.create_type_parameter(Option::<&Symbol>::None);
    marker_sub_type.set_constraint(type_checker.marker_super_type());
    type_checker.marker_sub_type = Some(marker_sub_type.into());
    type_checker.marker_other_type = Some(
        type_checker
            .create_type_parameter(Option::<&Symbol>::None)
            .into(),
    );

    type_checker.no_type_predicate = Some(Gc::new(type_checker.create_type_predicate(
        TypePredicateKind::Identifier,
        Some("<<unresolved>>".to_owned()),
        Some(0),
        Some(type_checker.any_type()),
    )));

    type_checker.any_signature = Some(Gc::new(type_checker.create_signature(
        None,
        None,
        None,
        vec![],
        Some(type_checker.any_type()),
        None,
        0,
        SignatureFlags::None,
    )));
    type_checker.unknown_signature = Some(Gc::new(type_checker.create_signature(
        None,
        None,
        None,
        vec![],
        Some(type_checker.error_type()),
        None,
        0,
        SignatureFlags::None,
    )));
    type_checker.resolving_signature = Some(Gc::new(type_checker.create_signature(
        None,
        None,
        None,
        vec![],
        Some(type_checker.any_type()),
        None,
        0,
        SignatureFlags::None,
    )));
    type_checker.silent_never_signature = Some(Gc::new(type_checker.create_signature(
        None,
        None,
        None,
        vec![],
        Some(type_checker.silent_never_type()),
        None,
        0,
        SignatureFlags::None,
    )));

    type_checker.enum_number_index_info = Some(Gc::new(type_checker.create_index_info(
        type_checker.number_type(),
        type_checker.string_type(),
        true,
        None,
    )));

    type_checker.any_iteration_types = Some(type_checker.create_iteration_types(
        Some(type_checker.any_type()),
        Some(type_checker.any_type()),
        Some(type_checker.any_type()),
    ));
    type_checker.any_iteration_types_except_next = Some(type_checker.create_iteration_types(
        Some(type_checker.any_type()),
        Some(type_checker.any_type()),
        Some(type_checker.unknown_type()),
    ));
    type_checker.default_iteration_types = Some(type_checker.create_iteration_types(
        Some(type_checker.never_type()),
        Some(type_checker.any_type()),
        Some(type_checker.undefined_type()),
    ));

    type_checker.empty_string_type = Some(type_checker.get_string_literal_type(""));
    type_checker.zero_type = Some(type_checker.get_number_literal_type(Number::new(0.0)));
    type_checker.zero_big_int_type = Some(
        type_checker.get_big_int_literal_type(PseudoBigInt::new(false, parse_pseudo_big_int("0"))),
    );

    type_checker.typeof_types_by_name = Some(HashMap::from_iter(IntoIterator::into_iter([
        ("string", type_checker.string_type()),
        ("number", type_checker.number_type()),
        ("bigint", type_checker.bigint_type()),
        ("boolean", type_checker.boolean_type()),
        ("symbol", type_checker.es_symbol_type()),
        ("undefined", type_checker.undefined_type()),
    ])));
    type_checker.typeof_type = Some(type_checker.create_typeof_type()?);

    let mut builtin_globals: SymbolTable = SymbolTable::new();
    builtin_globals.insert(
        type_checker.undefined_symbol().escaped_name().to_owned(),
        type_checker.undefined_symbol(),
    );
    *type_checker.builtin_globals.borrow_mut() = Some(builtin_globals);

    let rc_wrapped = Gc::new(type_checker);
    rc_wrapped.set_rc_wrapper(rc_wrapped.clone());

    *rc_wrapped.node_builder.borrow_mut() = Some(rc_wrapped.create_node_builder());

    rc_wrapped.initialize_type_checker();

    *rc_wrapped.check_binary_expression.borrow_mut() =
        Some(Gc::new(rc_wrapped.create_check_binary_expression()));
    *rc_wrapped.emit_resolver.borrow_mut() = Some(rc_wrapped.create_resolver());

    Ok(rc_wrapped)
}

#[derive(Default, Trace, Finalize)]
struct RestrictiveMapperFunc {}

impl TypeMapperCallback for RestrictiveMapperFunc {
    fn call(&self, type_checker: &TypeChecker, t: &Type) -> Gc<Type> {
        if t.flags().intersects(TypeFlags::TypeParameter) {
            type_checker.get_restrictive_type_parameter(t)
        } else {
            t.type_wrapper()
        }
    }
}

#[derive(Default, Trace, Finalize)]
struct PermissiveMapperFunc {}

impl TypeMapperCallback for PermissiveMapperFunc {
    fn call(&self, type_checker: &TypeChecker, t: &Type) -> Gc<Type> {
        if t.flags().intersects(TypeFlags::TypeParameter) {
            type_checker.wildcard_type()
        } else {
            t.type_wrapper()
        }
    }
}

fn async_iteration_types_resolver_resolve_iteration_type(
    type_checker: &TypeChecker,
    type_: &Type,
    error_node: Option<Gc<Node>>,
) -> io::Result<Option<Gc<Type>>> {
    type_checker.get_awaited_type_(type_, error_node, None, None)
}

fn sync_iteration_types_resolver_resolve_iteration_type(
    _type_checker: &TypeChecker,
    type_: &Type,
    _error_node: Option<Gc<Node>>,
) -> io::Result<Option<Gc<Type>>> {
    Ok(Some(type_.type_wrapper()))
}

#[derive(Debug, Trace, Finalize)]
pub(crate) struct DuplicateInfoForSymbol {
    pub first_file_locations: Vec<Gc<Node /*Declaration*/>>,
    pub second_file_locations: Vec<Gc<Node /*Declaration*/>>,
    pub is_block_scoped: bool,
}

#[derive(Debug, Trace, Finalize)]
pub(crate) struct DuplicateInfoForFiles {
    pub first_file: Gc<Node /*SourceFile*/>,
    pub second_file: Gc<Node /*SourceFile*/>,
    pub conflicting_symbols: IndexMap<String, DuplicateInfoForSymbol>,
}

impl TypeChecker {
    pub fn rc_wrapper(&self) -> Gc<TypeChecker> {
        self._rc_wrapper.borrow().clone().unwrap()
    }

    fn set_rc_wrapper(&self, wrapper: Gc<TypeChecker>) {
        *self._rc_wrapper.borrow_mut() = Some(wrapper);
    }

    pub(super) fn get_packages_map(&self) -> Ref<HashMap<String, bool>> {
        {
            let mut packages_map = self._packages_map.borrow_mut();
            if packages_map.is_none() {
                *packages_map = Some(HashMap::new());
            }
            let map = packages_map.as_mut().unwrap();
            self.host.get_source_files().iter().for_each(|sf| {
                let sf_as_source_file = sf.as_source_file();
                let sf_resolved_modules = sf_as_source_file.maybe_resolved_modules();
                if sf_resolved_modules.is_none() {
                    return;
                }

                sf_resolved_modules.as_ref().unwrap().for_each(|r, _, _| {
                    if let Some(r) = r.as_ref() {
                        if let Some(r_package_id) = r.package_id.as_ref() {
                            map.insert(
                                r_package_id.name.clone(),
                                matches!(r.extension, Some(Extension::Dts))
                                    || matches!(map.get(&r_package_id.name), Some(true)),
                            );
                        }
                    }
                })
            });
        }
        Ref::map(self._packages_map.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub(super) fn maybe_cancellation_token(
        &self,
    ) -> Option<Gc<Box<dyn CancellationTokenDebuggable>>> {
        self.cancellation_token.borrow().clone()
    }

    pub(super) fn set_cancellation_token(
        &self,
        cancellation_token: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) {
        *self.cancellation_token.borrow_mut() = cancellation_token;
    }

    pub(super) fn requested_external_emit_helpers(&self) -> ExternalEmitHelpers {
        self.requested_external_emit_helpers.get()
    }

    pub(super) fn set_requested_external_emit_helpers(
        &self,
        requested_external_emit_helpers: ExternalEmitHelpers,
    ) {
        self.requested_external_emit_helpers
            .set(requested_external_emit_helpers);
    }

    pub(super) fn maybe_external_helpers_module(&self) -> GcCellRefMut<Option<Gc<Symbol>>> {
        self.external_helpers_module.borrow_mut()
    }

    pub(super) fn type_count(&self) -> u32 {
        self.type_count.get()
    }

    pub(super) fn increment_type_count(&self) {
        self.type_count.set(self.type_count.get() + 1);
    }

    pub(super) fn symbol_count(&self) -> usize {
        self.symbol_count.get()
    }

    pub(super) fn increment_symbol_count(&self) {
        self.symbol_count.set(self.symbol_count() + 1);
    }

    pub(super) fn enum_count(&self) -> usize {
        self.enum_count.get()
    }

    pub(super) fn increment_enum_count(&self) {
        self.enum_count.set(self.enum_count() + 1);
    }

    pub(super) fn total_instantiation_count(&self) -> usize {
        self.total_instantiation_count.get()
    }

    pub(super) fn set_total_instantiation_count(&self, total_instantiation_count: usize) {
        self.total_instantiation_count
            .set(total_instantiation_count);
    }

    pub(super) fn instantiation_count(&self) -> usize {
        self.instantiation_count.get()
    }

    pub(super) fn set_instantiation_count(&self, instantiation_count: usize) {
        self.instantiation_count.set(instantiation_count);
    }

    pub(super) fn instantiation_depth(&self) -> usize {
        self.instantiation_depth.get()
    }

    pub(super) fn set_instantiation_depth(&self, instantiation_depth: usize) {
        self.instantiation_depth.set(instantiation_depth);
    }

    pub(super) fn inline_level(&self) -> usize {
        self.inline_level.get()
    }

    pub(super) fn set_inline_level(&self, inline_level: usize) {
        self.inline_level.set(inline_level);
    }

    pub(super) fn maybe_current_node(&self) -> Option<Gc<Node>> {
        self.current_node.borrow().clone()
    }

    pub(super) fn set_current_node(&self, current_node: Option<Gc<Node>>) {
        *self.current_node.borrow_mut() = current_node;
    }

    pub(super) fn empty_symbols(&self) -> Gc<GcCell<SymbolTable>> {
        self.empty_symbols.clone()
    }

    pub(super) fn array_variances(&self) -> Vec<VarianceFlags> {
        vec![VarianceFlags::Covariant]
    }

    pub(super) fn emit_resolver(&self) -> Gc<Box<dyn EmitResolver>> {
        self.emit_resolver.borrow().clone().unwrap()
    }

    pub(super) fn check_binary_expression(&self) -> Gc<CheckBinaryExpression> {
        self.check_binary_expression.borrow().clone().unwrap()
    }

    pub(super) fn node_builder(&self) -> Gc<NodeBuilder> {
        self.node_builder.borrow().clone().unwrap()
    }

    pub(super) fn globals(&self) -> GcCellRef<SymbolTable> {
        (*self.globals).borrow()
    }

    pub(super) fn globals_mut(&self) -> GcCellRefMut<SymbolTable> {
        self.globals.borrow_mut()
    }

    pub(super) fn globals_rc(&self) -> Gc<GcCell<SymbolTable>> {
        self.globals.clone()
    }

    pub(super) fn undefined_symbol(&self) -> Gc<Symbol> {
        self.undefined_symbol.clone().unwrap()
    }

    pub(super) fn global_this_symbol(&self) -> Gc<Symbol> {
        self.global_this_symbol.clone().unwrap()
    }

    pub(super) fn require_symbol(&self) -> Gc<Symbol> {
        self.require_symbol.clone().unwrap()
    }

    pub(super) fn arguments_symbol(&self) -> Gc<Symbol> {
        self.arguments_symbol.clone().unwrap()
    }

    pub(super) fn apparent_argument_count(&self) -> Option<usize> {
        self.apparent_argument_count.get()
    }

    pub(super) fn set_apparent_argument_count(&self, apparent_argument_count: Option<usize>) {
        self.apparent_argument_count.set(apparent_argument_count)
    }

    pub fn get_node_count(&self) -> usize {
        sum(&*self.host.get_source_files(), |source_file| {
            source_file.as_source_file().node_count()
        })
    }

    pub fn get_identifier_count(&self) -> usize {
        sum(&*self.host.get_source_files(), |source_file| {
            source_file.as_source_file().identifier_count()
        })
    }

    pub fn get_symbol_count(&self) -> usize {
        sum(&*self.host.get_source_files(), |source_file| {
            source_file.as_source_file().symbol_count()
        }) + self.symbol_count()
    }

    pub fn get_type_count(&self) -> u32 {
        self.type_count()
    }

    pub fn get_instantiation_count(&self) -> usize {
        self.total_instantiation_count()
    }

    pub fn get_relation_cache_sizes(&self) -> RelationCacheSizes {
        RelationCacheSizes {
            assignable: self.assignable_relation().len(),
            identity: self.identity_relation().len(),
            subtype: self.subtype_relation().len(),
            strict_subtype: self.strict_subtype_relation().len(),
        }
    }

    pub fn is_undefined_symbol(&self, symbol: &Symbol) -> bool {
        ptr::eq(symbol, &*self.undefined_symbol())
    }

    pub fn is_arguments_symbol(&self, symbol: &Symbol) -> bool {
        ptr::eq(symbol, &*self.arguments_symbol())
    }

    pub fn is_unknown_symbol(&self, symbol: &Symbol) -> bool {
        ptr::eq(symbol, &*self.unknown_symbol())
    }

    pub fn get_type_of_symbol_at_location(
        &self,
        symbol: &Symbol,
        location_in: &Node,
    ) -> io::Result<Gc<Type>> {
        let location = get_parse_tree_node(Some(location_in), Option::<fn(&Node) -> bool>::None);
        Ok(match location {
            Some(location) => self.get_type_of_symbol_at_location_(symbol, &location)?,
            None => self.error_type(),
        })
    }

    pub fn get_symbols_of_parameter_property_declaration(
        &self,
        parameter_in: &Node, /*ParameterDeclaration*/
        parameter_name: &str,
    ) -> Vec<Gc<Symbol>> {
        let parameter =
            get_parse_tree_node(Some(parameter_in), Some(|node: &Node| is_parameter(node)));
        if parameter.is_none() {
            Debug_.fail(Some("Cannot get symbols of a synthetic parameter that cannot be resolved to a parse-tree node."));
        }
        let parameter = parameter.unwrap();
        self.get_symbols_of_parameter_property_declaration_(
            &parameter,
            &escape_leading_underscores(parameter_name),
        )
    }

    pub fn get_property_of_type(&self, type_: &Type, name: &str) -> io::Result<Option<Gc<Symbol>>> {
        self.get_property_of_type_(type_, &escape_leading_underscores(name), None)
    }

    pub fn get_private_identifier_property_of_type(
        &self,
        left_type: &Type,
        name: &str,
        location: &Node,
    ) -> io::Result<Option<Gc<Symbol>>> {
        let node = return_ok_none_if_none!(get_parse_tree_node(
            Some(location),
            Option::<fn(&Node) -> bool>::None
        ));
        let prop_name = escape_leading_underscores(name);
        let lexically_scoped_identifier =
            self.lookup_symbol_for_private_identifier_declaration(&prop_name, &node);
        lexically_scoped_identifier.try_and_then(|lexically_scoped_identifier| {
            self.get_private_identifier_property_of_type_(left_type, &lexically_scoped_identifier)
        })
    }

    pub fn get_type_of_property_of_type(
        &self,
        type_: &Type,
        name: &str,
    ) -> io::Result<Option<Gc<Type>>> {
        self.get_type_of_property_of_type_(type_, &escape_leading_underscores(name))
    }

    pub fn get_index_info_of_type(&self, type_: &Type, kind: IndexKind) -> Option<Gc<IndexInfo>> {
        self.get_index_info_of_type_(
            type_,
            &*if kind == IndexKind::String {
                self.string_type()
            } else {
                self.number_type()
            },
        )
    }

    pub fn get_index_type_of_type(&self, type_: &Type, kind: IndexKind) -> Option<Gc<Type>> {
        self.get_index_type_of_type_(
            type_,
            &*if kind == IndexKind::String {
                self.string_type()
            } else {
                self.number_type()
            },
        )
    }

    pub fn get_type_from_type_node(
        &self,
        node_in: &Node, /*TypeNode*/
    ) -> io::Result<Gc<Type>> {
        let node = get_parse_tree_node(Some(node_in), Some(|node: &Node| is_type_node(node)));
        Ok(if let Some(node) = node {
            self.get_type_from_type_node_(&node)?
        } else {
            self.error_type()
        })
    }

    pub fn get_parameter_type(&self, signature: &Signature, parameter_index: usize) -> Gc<Type> {
        self.get_type_at_position(signature, parameter_index)
    }

    pub fn get_awaited_type(&self, type_: &Type) -> io::Result<Option<Gc<Type>>> {
        self.get_awaited_type_(type_, Option::<&Node>::None, None, None)
    }

    pub fn get_non_optional_type(&self, type_: &Type) -> Gc<Type> {
        self.remove_optional_type_marker(type_)
    }

    pub fn type_to_type_node(
        &self,
        type_: &Type,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Gc<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Gc<Node /*TypeNode*/>>> {
        self.node_builder()
            .type_to_type_node(type_, enclosing_declaration, flags, tracker)
    }

    pub fn index_info_to_index_signature_declaration(
        &self,
        index_info: &IndexInfo,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Gc<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Gc<Node /*IndexSignatureDeclaration*/>>> {
        self.node_builder()
            .index_info_to_index_signature_declaration(
                index_info,
                enclosing_declaration,
                flags,
                tracker,
            )
    }

    pub fn signature_to_signature_declaration(
        &self,
        signature: Gc<Signature>,
        kind: SyntaxKind,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Gc<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Gc<Node /*SignatureDeclaration & {typeArguments?: NodeArray<TypeNode>}*/>>>
    {
        self.node_builder().signature_to_signature_declaration(
            signature,
            kind,
            enclosing_declaration,
            flags,
            tracker,
        )
    }

    pub fn symbol_to_entity_name(
        &self,
        symbol: &Symbol,
        meaning: SymbolFlags,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
    ) -> io::Result<Option<Gc<Node /*EntityName*/>>> {
        self.node_builder().symbol_to_entity_name(
            symbol,
            Some(meaning),
            enclosing_declaration,
            flags,
            None,
        )
    }

    pub fn symbol_to_expression(
        &self,
        symbol: &Symbol,
        meaning: SymbolFlags,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
    ) -> io::Result<Option<Gc<Node /*Expression*/>>> {
        self.node_builder().symbol_to_expression(
            symbol,
            Some(meaning),
            enclosing_declaration,
            flags,
            None,
        )
    }

    pub fn symbol_to_type_parameter_declarations(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
    ) -> io::Result<Option<Gc<NodeArray> /*<TypeParameterDeclaration>*/>> {
        self.node_builder().symbol_to_type_parameter_declarations(
            symbol,
            enclosing_declaration,
            flags,
            None,
        )
    }

    pub fn symbol_to_parameter_declaration(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
    ) -> io::Result<Option<Gc<Node /*ParameterDeclaration*/>>> {
        self.node_builder().symbol_to_parameter_declaration(
            symbol,
            enclosing_declaration,
            flags,
            None,
        )
    }

    pub fn type_parameter_to_declaration(
        &self,
        parameter: &Type, /*TypeParameter*/
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
    ) -> io::Result<Option<Gc<Node /*TypeParameterDeclaration*/>>> {
        self.node_builder().type_parameter_to_declaration(
            parameter,
            enclosing_declaration,
            flags,
            None,
        )
    }

    pub fn get_symbols_in_scope(
        &self,
        location_in: &Node,
        meaning: SymbolFlags,
    ) -> Vec<Gc<Symbol>> {
        let location = get_parse_tree_node(Some(location_in), Option::<fn(&Node) -> bool>::None);
        location.map_or_else(
            || vec![],
            |location| self.get_symbols_in_scope_(&location, meaning),
        )
    }

    pub fn get_symbol_at_location(&self, node_in: &Node) -> io::Result<Option<Gc<Symbol>>> {
        let node = return_ok_none_if_none!(get_parse_tree_node(
            Some(node_in),
            Option::<fn(&Node) -> bool>::None
        ));
        self.get_symbol_at_location_(&node, Some(true))
    }

    pub fn get_index_infos_at_location(&self, node_in: &Node) -> Option<Vec<Gc<IndexInfo>>> {
        let node = get_parse_tree_node(Some(node_in), Option::<fn(&Node) -> bool>::None)?;
        self.get_index_infos_at_location_(&node)
    }

    pub fn get_shorthand_assignment_value_symbol(
        &self,
        node_in: Option<impl Borrow<Node>>,
    ) -> io::Result<Option<Gc<Symbol>>> {
        let node = return_ok_none_if_none!(get_parse_tree_node(
            node_in,
            Option::<fn(&Node) -> bool>::None
        ));
        self.get_shorthand_assignment_value_symbol_(Some(node))
    }

    pub fn get_export_specifier_local_target_symbol(
        &self,
        node_in: &Node, /*Identifier | ExportSpecifier*/
    ) -> io::Result<Option<Gc<Symbol>>> {
        let node = return_ok_none_if_none!(get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| is_export_specifier(node))
        ));
        self.get_export_specifier_local_target_symbol_(&node)
    }

    pub fn get_export_symbol_of_symbol(&self, symbol: &Symbol) -> Gc<Symbol> {
        self.get_merged_symbol(Some(
            symbol
                .maybe_export_symbol()
                .unwrap_or_else(|| symbol.symbol_wrapper()),
        ))
        .unwrap()
    }

    pub fn get_type_at_location(&self, node_in: &Node) -> io::Result<Gc<Type>> {
        let node = get_parse_tree_node(Some(node_in), Option::<fn(&Node) -> bool>::None);
        Ok(if let Some(node) = node {
            self.get_type_of_node(&node)?
        } else {
            self.error_type()
        })
    }

    pub fn get_type_of_assignment_pattern(
        &self,
        node_in: &Node, /*AssignmentPattern*/
    ) -> io::Result<Gc<Type>> {
        let node = get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| is_assignment_pattern(node)),
        );
        Ok(node
            .try_and_then(|node| self.get_type_of_assignment_pattern_(&node))?
            .unwrap_or_else(|| self.error_type()))
    }

    pub fn get_property_symbol_of_destructuring_assignment(
        &self,
        location_in: &Node, /*Identifier*/
    ) -> io::Result<Option<Gc<Symbol>>> {
        let location = return_ok_none_if_none!(get_parse_tree_node(
            Some(location_in),
            Some(|node: &Node| is_identifier(node))
        ));
        self.get_property_symbol_of_destructuring_assignment_(&location)
    }

    pub fn signature_to_string<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        signature: Gc<Signature>,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
        kind: Option<SignatureKind>,
    ) -> io::Result<String> {
        self.signature_to_string_(
            signature,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            flags,
            kind,
            None,
        )
    }

    pub fn type_to_string(
        &self,
        type_: &Type,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<TypeFormatFlags>,
    ) -> io::Result<String> {
        self.type_to_string_(
            type_,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            flags,
            None,
        )
    }

    pub fn symbol_to_string(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<impl Borrow<Node>>,
        meaning: Option<SymbolFlags>,
        flags: Option<SymbolFormatFlags>,
    ) -> io::Result<String> {
        self.symbol_to_string_(
            symbol,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            meaning,
            flags,
            None,
        )
    }

    pub fn type_predicate_to_string(
        &self,
        predicate: &TypePredicate,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<TypeFormatFlags>,
    ) -> io::Result<String> {
        self.type_predicate_to_string_(
            predicate,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            flags,
            None,
        )
    }

    pub fn write_signature(
        &self,
        signature: Gc<Signature>,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<TypeFormatFlags>,
        kind: Option<SignatureKind>,
        writer: Option<Gc<Box<dyn EmitTextWriter>>>,
    ) -> io::Result<String> {
        self.signature_to_string_(
            signature,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            flags,
            kind,
            writer,
        )
    }

    pub fn write_type(
        &self,
        type_: &Type,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<TypeFormatFlags>,
        writer: Option<Gc<Box<dyn EmitTextWriter>>>,
    ) -> io::Result<String> {
        self.type_to_string_(
            type_,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            flags,
            writer,
        )
    }

    pub fn write_symbol(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<impl Borrow<Node>>,
        meaning: Option<SymbolFlags>,
        flags: Option<SymbolFormatFlags>,
        writer: Option<Gc<Box<dyn EmitTextWriter>>>,
    ) -> io::Result<String> {
        self.symbol_to_string_(
            symbol,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            meaning,
            flags,
            writer,
        )
    }

    pub fn write_type_predicate(
        &self,
        predicate: &TypePredicate,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<TypeFormatFlags>,
        writer: Option<Gc<Box<dyn EmitTextWriter>>>,
    ) -> io::Result<String> {
        self.type_predicate_to_string_(
            predicate,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            flags,
            writer,
        )
    }

    pub fn get_contextual_type(
        &self,
        node_in: &Node, /*Expression*/
        context_flags: Option<ContextFlags>,
    ) -> io::Result<Option<Gc<Type>>> {
        let node = return_ok_none_if_none!(get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| is_expression(node))
        ));
        let containing_call =
            find_ancestor(Some(&*node), |node: &Node| is_call_like_expression(node));
        let containing_call_resolved_signature: Option<Gc<Signature>> =
            containing_call.as_ref().and_then(|containing_call| {
                (*self.get_node_links(&containing_call))
                    .borrow()
                    .resolved_signature
                    .clone()
            });
        if matches!(context_flags, Some(context_flags) if context_flags.intersects(ContextFlags::Completions))
        {
            if let Some(containing_call) = containing_call.as_ref() {
                let mut to_mark_skip: Option<Gc<Node>> = Some(node.node_wrapper());
                while {
                    let to_mark_skip_present = to_mark_skip.as_ref().unwrap();
                    self.get_node_links(to_mark_skip_present)
                        .borrow_mut()
                        .skip_direct_inference = Some(true);
                    to_mark_skip = to_mark_skip_present.maybe_parent();
                    matches!(to_mark_skip.as_ref(), Some(to_mark_skip) if !Gc::ptr_eq(to_mark_skip, containing_call))
                } {}
                self.get_node_links(containing_call)
                    .borrow_mut()
                    .resolved_signature = None;
            }
        }
        let result = self.get_contextual_type_(&node, context_flags)?;
        if matches!(context_flags, Some(context_flags) if context_flags.intersects(ContextFlags::Completions))
        {
            if let Some(containing_call) = containing_call.as_ref() {
                let mut to_mark_skip: Option<Gc<Node>> = Some(node.node_wrapper());
                while {
                    let to_mark_skip_present = to_mark_skip.as_ref().unwrap();
                    self.get_node_links(to_mark_skip_present)
                        .borrow_mut()
                        .skip_direct_inference = None;
                    to_mark_skip = to_mark_skip_present.maybe_parent();
                    matches!(to_mark_skip.as_ref(), Some(to_mark_skip) if !Gc::ptr_eq(to_mark_skip, containing_call))
                } {}
                self.get_node_links(containing_call)
                    .borrow_mut()
                    .resolved_signature = containing_call_resolved_signature;
            }
        }
        Ok(result)
    }

    pub fn get_contextual_type_for_object_literal_element(
        &self,
        node_in: &Node, /*ObjectLiteralElementLike*/
    ) -> Option<Gc<Type>> {
        let node = get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| is_object_literal_element_like(node)),
        )?;
        self.get_contextual_type_for_object_literal_element_(&node, None)
    }

    pub fn get_contextual_type_for_argument_at_index(
        &self,
        node_in: &Node, /*CallLikeExpression*/
        arg_index: usize,
    ) -> io::Result<Option<Gc<Type>>> {
        let node = return_ok_none_if_none!(get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| is_call_like_expression(node)),
        ));
        Ok(Some(self.get_contextual_type_for_argument_at_index_(
            &node, arg_index,
        )?))
    }

    pub fn get_contextual_type_for_jsx_attribute(
        &self,
        node_in: &Node, /*ObjectLiteralElementLike*/
    ) -> Option<Gc<Type>> {
        let node = get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| is_jsx_attribute_like(node)),
        )?;
        self.get_contextual_type_for_jsx_attribute_(&node)
    }

    pub fn get_resolved_signature(
        &self,
        node: &Node, /*CallLikeExpression*/
        candidates_out_array: Option<&mut Vec<Gc<Signature>>>,
        argument_count: Option<usize>,
    ) -> Option<Gc<Signature>> {
        self.get_resolved_signature_worker(
            node,
            candidates_out_array,
            argument_count,
            CheckMode::Normal,
        )
    }

    pub fn get_resolved_signature_for_signature_help(
        &self,
        node: &Node, /*CallLikeExpression*/
        candidates_out_array: Option<&mut Vec<Gc<Signature>>>,
        argument_count: Option<usize>,
    ) -> Option<Gc<Signature>> {
        self.get_resolved_signature_worker(
            node,
            candidates_out_array,
            argument_count,
            CheckMode::IsForSignatureHelp,
        )
    }

    pub fn get_constant_value(
        &self,
        node_in: &Node, /*EnumMember | PropertyAccessExpression | ElementAccessExpression*/
    ) -> Option<StringOrNumber> {
        let node = get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| self.can_have_constant_value(node)),
        )?;
        self.get_constant_value_(&node)
    }

    pub fn is_valid_property_access(
        &self,
        node_in: &Node, /*QualifiedName | PropertyAccessExpression | ImportTypeNode*/
        property_name: &str,
    ) -> io::Result<bool> {
        let node = get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| is_property_access_or_qualified_name_or_import_type_node(node)),
        );
        Ok(match node {
            None => false,
            Some(node) => {
                self.is_valid_property_access_(&node, &escape_leading_underscores(property_name))?
            }
        })
    }

    pub fn is_valid_property_access_for_completions(
        &self,
        node_in: &Node, /*PropertyAccessExpression | QualifiedName | ImportTypeNode*/
        type_: &Type,
        property: &Symbol,
    ) -> bool {
        let node = get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| is_property_access_expression(node)),
        );
        match node {
            None => false,
            Some(node) => self.is_valid_property_access_for_completions_(&node, type_, property),
        }
    }

    pub fn get_signature_from_declaration(
        &self,
        declaration_in: &Node, /*SignatureDeclaration*/
    ) -> io::Result<Option<Gc<Signature>>> {
        let declaration = return_ok_none_if_none!(get_parse_tree_node(
            Some(declaration_in),
            Some(|node: &Node| is_function_like(Some(node))),
        ));
        Ok(Some(self.get_signature_from_declaration_(&declaration)?))
    }

    pub fn is_implementation_of_overload(
        &self,
        node_in: &Node, /*SignatureDeclaration*/
    ) -> Option<bool> {
        let node = get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| is_function_like(Some(node))),
        )?;
        Some(self.is_implementation_of_overload_(&node))
    }

    pub fn get_aliased_symbol(&self, symbol: &Symbol) -> io::Result<Gc<Symbol>> {
        self.resolve_alias(symbol)
    }

    pub fn get_exports_of_module(&self, module_symbol: &Symbol) -> io::Result<Vec<Gc<Symbol>>> {
        self.get_exports_of_module_as_array(module_symbol)
    }

    pub fn get_symbol_walker(&self, _accept: Option<impl FnMut(&Symbol) -> bool>) -> SymbolWalker {
        unimplemented!() // TODO: figure out how to implement this
    }

    pub fn is_optional_parameter(&self, node_in: &Node /*ParameterDeclaration*/) -> bool {
        let node = get_parse_tree_node(Some(node_in), Some(|node: &Node| is_parameter(node)));
        match node {
            None => false,
            Some(node) => self.is_optional_parameter_(&node),
        }
    }

    pub fn try_get_member_in_module_exports(
        &self,
        name: &str,
        symbol: &Symbol,
    ) -> io::Result<Option<Gc<Symbol>>> {
        self.try_get_member_in_module_exports_(&escape_leading_underscores(name), symbol)
    }

    pub fn try_get_member_in_module_exports_and_properties(
        &self,
        name: &str,
        symbol: &Symbol,
    ) -> io::Result<Option<Gc<Symbol>>> {
        self.try_get_member_in_module_exports_and_properties_(
            &escape_leading_underscores(name),
            symbol,
        )
    }

    pub fn try_find_ambient_module(&self, module_name: &str) -> Option<Gc<Symbol>> {
        self.try_find_ambient_module_(module_name, true)
    }

    pub fn try_find_ambient_module_without_augmentations(
        &self,
        module_name: &str,
    ) -> Option<Gc<Symbol>> {
        self.try_find_ambient_module_(module_name, false)
    }

    pub fn get_any_type(&self) -> Gc<Type> {
        self.any_type()
    }

    pub fn get_string_type(&self) -> Gc<Type> {
        self.string_type()
    }

    pub fn get_number_type(&self) -> Gc<Type> {
        self.number_type()
    }

    pub fn get_boolean_type(&self) -> Gc<Type> {
        self.boolean_type()
    }

    pub fn get_false_type(&self, fresh: Option<bool>) -> Gc<Type> {
        if matches!(fresh, Some(true)) {
            self.false_type()
        } else {
            self.regular_false_type()
        }
    }

    pub fn get_true_type(&self, fresh: Option<bool>) -> Gc<Type> {
        if matches!(fresh, Some(true)) {
            self.true_type()
        } else {
            self.regular_true_type()
        }
    }

    pub fn get_void_type(&self) -> Gc<Type> {
        self.void_type()
    }

    pub fn get_undefined_type(&self) -> Gc<Type> {
        self.undefined_type()
    }

    pub fn get_null_type(&self) -> Gc<Type> {
        self.null_type()
    }

    pub fn get_es_symbol_type(&self) -> Gc<Type> {
        self.es_symbol_type()
    }

    pub fn get_never_type(&self) -> Gc<Type> {
        self.never_type()
    }

    pub fn get_optional_type(&self) -> Gc<Type> {
        self.optional_type()
    }

    pub fn get_promise_type(&self) -> Gc<Type> {
        self.get_global_promise_type(false)
    }

    pub fn get_promise_like_type(&self) -> Gc<Type> {
        self.get_global_promise_like_type(false)
    }

    pub fn get_suggested_symbol_for_nonexistent_symbol(
        &self,
        location: &Node,
        name: &str,
        meaning: SymbolFlags,
    ) -> Option<Gc<Symbol>> {
        self.get_suggested_symbol_for_nonexistent_symbol_(
            Some(location),
            &escape_leading_underscores(name),
            meaning,
        )
    }

    pub fn get_suggestion_for_nonexistent_symbol(
        &self,
        location: &Node,
        name: &str,
        meaning: SymbolFlags,
    ) -> Option<String> {
        self.get_suggestion_for_nonexistent_symbol_(
            Some(location),
            &escape_leading_underscores(name),
            meaning,
        )
    }

    pub fn get_default_from_type_parameter(&self, type_: &Type) -> Option<Gc<Type>> {
        /*type &&*/
        if type_.flags().intersects(TypeFlags::TypeParameter) {
            self.get_default_from_type_parameter_(type_)
        } else {
            None
        }
    }

    pub fn resolve_name(
        &self,
        name: &str,
        location: Option<impl Borrow<Node>>,
        meaning: SymbolFlags,
        exclude_globals: bool,
    ) -> io::Result<Option<Gc<Symbol>>> {
        self.resolve_name_(
            location,
            &escape_leading_underscores(name),
            meaning,
            None,
            Option::<Gc<Node>>::None,
            false,
            Some(exclude_globals),
        )
    }

    pub fn get_jsx_namespace<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
    ) -> String {
        unescape_leading_underscores(&self.get_jsx_namespace_(location)).to_owned()
    }

    pub fn get_jsx_fragment_factory(&self, n: &Node) -> Option<String> {
        let jsx_fragment_factory = self.get_jsx_fragment_factory_entity(n)?;
        Some(
            unescape_leading_underscores(
                &get_first_identifier(&jsx_fragment_factory)
                    .as_identifier()
                    .escaped_text,
            )
            .to_owned(),
        )
    }

    pub fn resolve_external_module_name(
        &self,
        module_specifier_in: &Node, /*Expression*/
    ) -> Option<Gc<Symbol>> {
        let module_specifier = get_parse_tree_node(
            Some(module_specifier_in),
            Some(|node: &Node| is_expression(node)),
        )?;
        self.resolve_external_module_name_(&module_specifier, &module_specifier, Some(true))
    }

    pub fn try_get_this_type_at(
        &self,
        node_in: &Node,
        include_global_this: Option<bool>,
    ) -> io::Result<Option<Gc<Type>>> {
        let node = return_ok_none_if_none!(get_parse_tree_node(
            Some(node_in),
            Option::<fn(&Node) -> bool>::None
        ));
        self.try_get_this_type_at_(&node, include_global_this, Option::<&Node>::None)
    }

    pub fn get_type_argument_constraint(
        &self,
        node_in: &Node, /*TypeNode*/
    ) -> Option<Gc<Type>> {
        let node = get_parse_tree_node(Some(node_in), Some(|node: &Node| is_type_node(node)))?;
        self.get_type_argument_constraint_(&node)
    }

    pub fn get_suggestion_diagnostics(
        &self,
        file_in: &Node, /*SourceFile*/
        ct: Option<Gc<Box<dyn CancellationTokenDebuggable>>>,
    ) -> Vec<Gc<Diagnostic /*DiagnosticWithLocation*/>> {
        let file = get_parse_tree_node(Some(file_in), Some(|node: &Node| is_source_file(node)))
            .unwrap_or_else(|| Debug_.fail(Some("Could not determine parsed source file.")));
        if skip_type_checking(&file, &self.compiler_options, |file_name| {
            TypeCheckerHost::is_source_of_project_reference_redirect(&**self.host, file_name)
        }) {
            return vec![];
        }

        let mut diagnostics: Option<Vec<Gc<Diagnostic>>>;
        self.set_cancellation_token(ct);

        self.check_source_file(&file);
        Debug_.assert(
            (*self.get_node_links(&file))
                .borrow()
                .flags
                .intersects(NodeCheckFlags::TypeChecked),
            None,
        );

        diagnostics = Some(vec![]);
        add_range(
            diagnostics.as_mut().unwrap(),
            Some(
                &self
                    .suggestion_diagnostics()
                    .get_diagnostics(Some(&file.as_source_file().file_name())),
            ),
            None,
            None,
        );
        self.check_unused_identifiers(
            &self.get_potentially_unused_identifiers(&file),
            |containing_node, kind, diag| {
                if !contains_parse_error(containing_node)
                    && !self.unused_is_error(
                        kind,
                        containing_node.flags().intersects(NodeFlags::Ambient),
                    )
                {
                    diag.set_category(DiagnosticCategory::Suggestion); // TODO: is it a problem that this is being mutated?
                    diagnostics.as_mut().unwrap().push(diag);
                }
            },
        );

        self.set_cancellation_token(None);

        diagnostics.unwrap_or_else(|| vec![])
    }

    pub fn run_with_cancellation_token<TReturn, TCallback: FnMut(&TypeChecker) -> TReturn>(
        &self,
        token: Gc<Box<dyn CancellationTokenDebuggable>>,
        mut callback: TCallback,
    ) -> TReturn {
        self.set_cancellation_token(Some(token));
        let ret = callback(self);
        self.set_cancellation_token(None);
        ret
    }

    pub(super) fn get_resolved_signature_worker(
        &self,
        node_in: &Node, /*CallLikeExpression*/
        candidates_out_array: Option<&mut Vec<Gc<Signature>>>,
        argument_count: Option<usize>,
        check_mode: CheckMode,
    ) -> Option<Gc<Signature>> {
        let node = get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| is_call_like_expression(node)),
        );
        self.set_apparent_argument_count(argument_count);
        let res = node.map(|node| {
            self.get_resolved_signature_(&node, candidates_out_array, Some(check_mode))
        });
        self.set_apparent_argument_count(None);
        res
    }

    pub(super) fn tuple_types(&self) -> GcCellRefMut<HashMap<String, Gc</*GenericType*/ Type>>> {
        self.tuple_types.borrow_mut()
    }

    pub(super) fn union_types(&self) -> GcCellRefMut<HashMap<String, Gc</*UnionType*/ Type>>> {
        self.union_types.borrow_mut()
    }

    pub(super) fn intersection_types(&self) -> GcCellRefMut<HashMap<String, Gc<Type>>> {
        self.intersection_types.borrow_mut()
    }

    pub(super) fn string_literal_types(
        &self,
    ) -> GcCellRefMut<HashMap<String, Gc</*NumberLiteralType*/ Type>>> {
        self.string_literal_types.borrow_mut()
    }

    pub(super) fn number_literal_types(
        &self,
    ) -> GcCellRefMut<HashMap<Number, Gc</*NumberLiteralType*/ Type>>> {
        self.number_literal_types.borrow_mut()
    }

    pub(super) fn big_int_literal_types(
        &self,
    ) -> GcCellRefMut<HashMap<String, Gc</*BigIntLiteralType*/ Type>>> {
        self.big_int_literal_types.borrow_mut()
    }

    pub(super) fn enum_literal_types(
        &self,
    ) -> GcCellRefMut<HashMap<String, Gc</*LiteralType*/ Type>>> {
        self.enum_literal_types.borrow_mut()
    }

    pub(super) fn string_mapping_types(
        &self,
    ) -> GcCellRefMut<HashMap<String, Gc<Type /*StringMappingType*/>>> {
        self.string_mapping_types.borrow_mut()
    }

    pub(super) fn indexed_access_types(
        &self,
    ) -> GcCellRefMut<HashMap<String, Gc<Type /*IndexedAccessType*/>>> {
        self.indexed_access_types.borrow_mut()
    }

    pub(super) fn template_literal_types(
        &self,
    ) -> GcCellRefMut<HashMap<String, Gc<Type /*TemplateLiteralType*/>>> {
        self.template_literal_types.borrow_mut()
    }

    pub(super) fn substitution_types(
        &self,
    ) -> GcCellRefMut<HashMap<String, Gc<Type /*SubstitutionType*/>>> {
        self.substitution_types.borrow_mut()
    }

    pub(super) fn subtype_reduction_cache(&self) -> GcCellRefMut<HashMap<String, Vec<Gc<Type>>>> {
        self.subtype_reduction_cache.borrow_mut()
    }

    pub(super) fn evolving_array_types(&self) -> GcCellRefMut<HashMap<TypeId, Gc<Type>>> {
        self.evolving_array_types.borrow_mut()
    }

    pub(super) fn undefined_properties(&self) -> GcCellRefMut<SymbolTable> {
        self.undefined_properties.borrow_mut()
    }

    pub(super) fn unknown_symbol(&self) -> Gc<Symbol> {
        self.unknown_symbol.as_ref().unwrap().clone()
    }

    pub(super) fn resolving_symbol(&self) -> Gc<Symbol> {
        self.resolving_symbol.as_ref().unwrap().clone()
    }

    pub(super) fn unresolved_symbols(&self) -> GcCellRefMut<HashMap<String, Gc<Symbol>>> {
        self.unresolved_symbols.borrow_mut()
    }

    pub(super) fn error_types(&self) -> GcCellRefMut<HashMap<String, Gc<Type>>> {
        self.error_types.borrow_mut()
    }

    pub(super) fn any_type(&self) -> Gc<Type> {
        self.any_type.as_ref().unwrap().clone()
    }

    pub(super) fn auto_type(&self) -> Gc<Type> {
        self.auto_type.as_ref().unwrap().clone()
    }

    pub(super) fn wildcard_type(&self) -> Gc<Type> {
        self.wildcard_type.as_ref().unwrap().clone()
    }

    pub(super) fn error_type(&self) -> Gc<Type> {
        self.error_type.as_ref().unwrap().clone()
    }

    pub(super) fn unresolved_type(&self) -> Gc<Type> {
        self.unresolved_type.as_ref().unwrap().clone()
    }

    pub(super) fn non_inferrable_any_type(&self) -> Gc<Type> {
        self.non_inferrable_any_type.as_ref().unwrap().clone()
    }

    pub(super) fn intrinsic_marker_type(&self) -> Gc<Type> {
        self.intrinsic_marker_type.as_ref().unwrap().clone()
    }

    pub(super) fn unknown_type(&self) -> Gc<Type> {
        self.unknown_type.as_ref().unwrap().clone()
    }

    pub(super) fn non_null_unknown_type(&self) -> Gc<Type> {
        self.non_null_unknown_type.as_ref().unwrap().clone()
    }

    pub(super) fn undefined_type(&self) -> Gc<Type> {
        self.undefined_type.as_ref().unwrap().clone()
    }

    pub(super) fn undefined_widening_type(&self) -> Gc<Type> {
        self.undefined_widening_type.as_ref().unwrap().clone()
    }

    pub(super) fn optional_type(&self) -> Gc<Type> {
        self.optional_type.as_ref().unwrap().clone()
    }

    pub(super) fn missing_type(&self) -> Gc<Type> {
        self.missing_type.as_ref().unwrap().clone()
    }

    pub(super) fn null_type(&self) -> Gc<Type> {
        self.null_type.as_ref().unwrap().clone()
    }

    pub(super) fn null_widening_type(&self) -> Gc<Type> {
        self.null_widening_type.as_ref().unwrap().clone()
    }

    pub(super) fn string_type(&self) -> Gc<Type> {
        self.string_type.as_ref().unwrap().clone()
    }

    pub(super) fn number_type(&self) -> Gc<Type> {
        self.number_type.as_ref().unwrap().clone()
    }

    pub(super) fn bigint_type(&self) -> Gc<Type> {
        self.bigint_type.as_ref().unwrap().clone()
    }

    pub(super) fn true_type(&self) -> Gc<Type> {
        self.true_type.as_ref().unwrap().clone()
    }

    pub(super) fn regular_true_type(&self) -> Gc<Type> {
        self.regular_true_type.as_ref().unwrap().clone()
    }

    pub(super) fn false_type(&self) -> Gc<Type> {
        self.false_type.as_ref().unwrap().clone()
    }

    pub(super) fn regular_false_type(&self) -> Gc<Type> {
        self.regular_false_type.as_ref().unwrap().clone()
    }

    pub(super) fn boolean_type(&self) -> Gc<Type> {
        self.boolean_type.as_ref().unwrap().clone()
    }

    pub(super) fn es_symbol_type(&self) -> Gc<Type> {
        self.es_symbol_type.as_ref().unwrap().clone()
    }

    pub(super) fn void_type(&self) -> Gc<Type> {
        self.void_type.as_ref().unwrap().clone()
    }

    pub(super) fn never_type(&self) -> Gc<Type> {
        self.never_type.as_ref().unwrap().clone()
    }

    pub(super) fn silent_never_type(&self) -> Gc<Type> {
        self.silent_never_type.as_ref().unwrap().clone()
    }

    pub(super) fn non_inferrable_type(&self) -> Gc<Type> {
        self.non_inferrable_type.as_ref().unwrap().clone()
    }

    pub(super) fn implicit_never_type(&self) -> Gc<Type> {
        self.implicit_never_type.as_ref().unwrap().clone()
    }

    pub(super) fn unreachable_never_type(&self) -> Gc<Type> {
        self.unreachable_never_type.as_ref().unwrap().clone()
    }

    pub(super) fn non_primitive_type(&self) -> Gc<Type> {
        self.non_primitive_type.as_ref().unwrap().clone()
    }

    pub(super) fn string_or_number_type(&self) -> Gc<Type> {
        self.string_or_number_type.as_ref().unwrap().clone()
    }

    pub(super) fn string_number_symbol_type(&self) -> Gc<Type> {
        self.string_number_symbol_type.as_ref().unwrap().clone()
    }

    pub(super) fn keyof_constraint_type(&self) -> Gc<Type> {
        self.keyof_constraint_type.clone().unwrap()
    }

    pub(super) fn number_or_big_int_type(&self) -> Gc<Type> {
        self.number_or_big_int_type.as_ref().unwrap().clone()
    }

    pub(super) fn template_constraint_type(&self) -> Gc<Type> {
        self.template_constraint_type.as_ref().unwrap().clone()
    }

    pub(super) fn empty_type_literal_symbol(&self) -> Gc<Symbol> {
        self.empty_type_literal_symbol.as_ref().unwrap().clone()
    }

    pub(super) fn empty_object_type(&self) -> Gc<Type> {
        self.empty_object_type.as_ref().unwrap().clone()
    }

    pub(super) fn empty_jsx_object_type(&self) -> Gc<Type> {
        self.empty_jsx_object_type.as_ref().unwrap().clone()
    }

    pub(super) fn empty_type_literal_type(&self) -> Gc<Type> {
        self.empty_type_literal_type.as_ref().unwrap().clone()
    }

    pub(super) fn empty_generic_type(&self) -> Gc<Type> {
        self.empty_generic_type.as_ref().unwrap().clone()
    }

    pub(super) fn any_function_type(&self) -> Gc<Type> {
        self.any_function_type.as_ref().unwrap().clone()
    }

    pub(super) fn no_constraint_type(&self) -> Gc<Type> {
        self.no_constraint_type.as_ref().unwrap().clone()
    }

    pub(super) fn circular_constraint_type(&self) -> Gc<Type> {
        self.circular_constraint_type.as_ref().unwrap().clone()
    }

    pub(super) fn resolving_default_type(&self) -> Gc<Type> {
        self.resolving_default_type.as_ref().unwrap().clone()
    }

    pub(super) fn marker_super_type(&self) -> Gc<Type> {
        self.marker_super_type.as_ref().unwrap().clone()
    }

    pub(super) fn marker_sub_type(&self) -> Gc<Type> {
        self.marker_sub_type.as_ref().unwrap().clone()
    }

    pub(super) fn marker_other_type(&self) -> Gc<Type> {
        self.marker_other_type.as_ref().unwrap().clone()
    }

    pub(super) fn no_type_predicate(&self) -> Gc<TypePredicate> {
        self.no_type_predicate.as_ref().unwrap().clone()
    }

    pub(super) fn any_signature(&self) -> Gc<Signature> {
        self.any_signature.as_ref().unwrap().clone()
    }

    pub(super) fn unknown_signature(&self) -> Gc<Signature> {
        self.unknown_signature.as_ref().unwrap().clone()
    }

    pub(super) fn resolving_signature(&self) -> Gc<Signature> {
        self.resolving_signature.as_ref().unwrap().clone()
    }

    pub(super) fn silent_never_signature(&self) -> Gc<Signature> {
        self.silent_never_signature.as_ref().unwrap().clone()
    }

    pub(super) fn enum_number_index_info(&self) -> Gc<IndexInfo> {
        self.enum_number_index_info.as_ref().unwrap().clone()
    }

    pub(super) fn iteration_types_cache(
        &self,
    ) -> GcCellRefMut<HashMap<String, Gc<IterationTypes>>> {
        self.iteration_types_cache.borrow_mut()
    }

    pub(super) fn no_iteration_types(&self) -> Gc<IterationTypes> {
        self.no_iteration_types.clone()
    }

    pub(super) fn any_iteration_types(&self) -> Gc<IterationTypes> {
        self.any_iteration_types.clone().unwrap()
    }

    pub(super) fn any_iteration_types_except_next(&self) -> Gc<IterationTypes> {
        self.any_iteration_types_except_next.clone().unwrap()
    }

    pub(super) fn default_iteration_types(&self) -> Gc<IterationTypes> {
        self.default_iteration_types.clone().unwrap()
    }

    pub(super) fn maybe_amalgamated_duplicates(
        &self,
    ) -> GcCellRefMut<Option<HashMap<String, DuplicateInfoForFiles>>> {
        self.amalgamated_duplicates.borrow_mut()
    }

    pub(super) fn reverse_mapped_cache(&self) -> GcCellRefMut<HashMap<String, Option<Gc<Type>>>> {
        self.reverse_mapped_cache.borrow_mut()
    }

    pub(super) fn in_infer_type_for_homomorphic_mapped_type(&self) -> bool {
        self.in_infer_type_for_homomorphic_mapped_type.get()
    }

    pub(super) fn set_in_infer_type_for_homomorphic_mapped_type(
        &self,
        in_infer_type_for_homomorphic_mapped_type: bool,
    ) {
        self.in_infer_type_for_homomorphic_mapped_type
            .set(in_infer_type_for_homomorphic_mapped_type);
    }

    pub(super) fn maybe_pattern_ambient_modules(
        &self,
    ) -> GcCellRefMut<Option<Vec<Gc<PatternAmbientModule>>>> {
        self.pattern_ambient_modules.borrow_mut()
    }

    pub(super) fn maybe_pattern_ambient_module_augmentations(
        &self,
    ) -> GcCellRefMut<Option<HashMap<String, Gc<Symbol>>>> {
        self.pattern_ambient_module_augmentations.borrow_mut()
    }

    pub(super) fn maybe_global_object_type(&self) -> Option<Gc<Type>> {
        self.global_object_type.borrow().clone()
    }

    pub(super) fn global_object_type(&self) -> Gc<Type> {
        self.global_object_type.borrow().clone().unwrap()
    }

    pub(super) fn global_function_type(&self) -> Gc<Type> {
        self.global_function_type.borrow().as_ref().unwrap().clone()
    }

    pub(super) fn global_callable_function_type(&self) -> Gc<Type> {
        self.global_callable_function_type
            .borrow()
            .as_ref()
            .unwrap()
            .clone()
    }

    pub(super) fn global_newable_function_type(&self) -> Gc<Type> {
        self.global_newable_function_type
            .borrow()
            .as_ref()
            .unwrap()
            .clone()
    }

    pub(super) fn global_array_type(&self) -> Gc<Type> {
        self.global_array_type.borrow().as_ref().unwrap().clone()
    }

    pub(super) fn global_readonly_array_type(&self) -> Gc<Type> {
        self.global_readonly_array_type
            .borrow()
            .as_ref()
            .unwrap()
            .clone()
    }

    pub(super) fn global_string_type(&self) -> Gc<Type> {
        self.global_string_type.borrow().as_ref().unwrap().clone()
    }

    pub(super) fn global_number_type(&self) -> Gc<Type> {
        self.global_number_type.borrow().as_ref().unwrap().clone()
    }

    pub(super) fn global_boolean_type(&self) -> Gc<Type> {
        self.global_boolean_type.borrow().as_ref().unwrap().clone()
    }

    pub(super) fn global_reg_exp_type(&self) -> Gc<Type> {
        self.global_reg_exp_type.borrow().as_ref().unwrap().clone()
    }

    pub(super) fn global_this_type(&self) -> Gc<Type> {
        self.global_this_type.borrow().as_ref().unwrap().clone()
    }

    pub(super) fn any_array_type(&self) -> Gc<Type> {
        self.any_array_type.borrow().as_ref().unwrap().clone()
    }

    pub(super) fn auto_array_type(&self) -> Gc<Type> {
        self.auto_array_type.borrow().as_ref().unwrap().clone()
    }

    pub(super) fn any_readonly_array_type(&self) -> Gc<Type> {
        self.any_readonly_array_type
            .borrow()
            .as_ref()
            .unwrap()
            .clone()
    }

    pub(super) fn maybe_deferred_global_non_nullable_type_alias(
        &self,
    ) -> GcCellRefMut<Option<Gc<Symbol>>> {
        self.deferred_global_non_nullable_type_alias.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_es_symbol_constructor_symbol(
        &self,
    ) -> GcCellRefMut<Option<Gc<Symbol>>> {
        self.deferred_global_es_symbol_constructor_symbol
            .borrow_mut()
    }

    pub(super) fn maybe_deferred_global_es_symbol_constructor_type_symbol(
        &self,
    ) -> GcCellRefMut<Option<Gc<Symbol>>> {
        self.deferred_global_es_symbol_constructor_type_symbol
            .borrow_mut()
    }

    pub(super) fn maybe_deferred_global_es_symbol_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_es_symbol_type.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_typed_property_descriptor_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_typed_property_descriptor_type
            .borrow_mut()
    }

    pub(super) fn maybe_deferred_global_promise_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_promise_type.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_promise_like_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_promise_like_type.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_promise_constructor_symbol(
        &self,
    ) -> GcCellRefMut<Option<Gc<Symbol>>> {
        self.deferred_global_promise_constructor_symbol.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_promise_constructor_like_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_promise_constructor_like_type
            .borrow_mut()
    }

    pub(super) fn maybe_deferred_global_iterable_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_iterable_type.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_iterator_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_iterator_type.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_iterable_iterator_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_iterable_iterator_type.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_generator_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_generator_type.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_iterator_yield_result_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_iterator_yield_result_type.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_iterator_return_result_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_iterator_return_result_type
            .borrow_mut()
    }

    pub(super) fn maybe_deferred_global_async_iterable_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_async_iterable_type.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_async_iterator_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_async_iterator_type.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_async_iterable_iterator_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_async_iterable_iterator_type
            .borrow_mut()
    }

    pub(super) fn maybe_deferred_global_async_generator_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_async_generator_type.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_template_strings_array_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_template_strings_array_type
            .borrow_mut()
    }

    pub(super) fn maybe_deferred_global_import_meta_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_import_meta_type.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_import_meta_expression_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_import_meta_expression_type
            .borrow_mut()
    }

    pub(super) fn maybe_deferred_global_import_call_options_type(
        &self,
    ) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_import_call_options_type.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_extract_symbol(&self) -> GcCellRefMut<Option<Gc<Symbol>>> {
        self.deferred_global_extract_symbol.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_omit_symbol(&self) -> GcCellRefMut<Option<Gc<Symbol>>> {
        self.deferred_global_omit_symbol.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_awaited_symbol(&self) -> GcCellRefMut<Option<Gc<Symbol>>> {
        self.deferred_global_awaited_symbol.borrow_mut()
    }

    pub(super) fn maybe_deferred_global_big_int_type(&self) -> GcCellRefMut<Option<Gc<Type>>> {
        self.deferred_global_big_int_type.borrow_mut()
    }

    pub(super) fn all_potentially_unused_identifiers(
        &self,
    ) -> GcCellRefMut<HashMap<Path, Vec<Gc<Node /*PotentiallyUnusedIdentifier*/>>>> {
        self.all_potentially_unused_identifiers.borrow_mut()
    }

    pub(crate) fn flow_loop_start(&self) -> usize {
        self.flow_loop_start.get()
    }

    pub(crate) fn set_flow_loop_start(&self, flow_loop_start: usize) {
        self.flow_loop_start.set(flow_loop_start);
    }

    pub(crate) fn flow_loop_count(&self) -> usize {
        self.flow_loop_count.get()
    }

    pub(crate) fn set_flow_loop_count(&self, flow_loop_count: usize) {
        self.flow_loop_count.set(flow_loop_count);
    }

    pub(crate) fn shared_flow_count(&self) -> usize {
        self.shared_flow_count.get()
    }

    pub(crate) fn set_shared_flow_count(&self, shared_flow_count: usize) {
        self.shared_flow_count.set(shared_flow_count);
    }

    pub(crate) fn flow_analysis_disabled(&self) -> bool {
        self.flow_analysis_disabled.get()
    }

    pub(crate) fn set_flow_analysis_disabled(&self, flow_analysis_disabled: bool) {
        self.flow_analysis_disabled.set(flow_analysis_disabled);
    }

    pub(crate) fn flow_invocation_count(&self) -> usize {
        self.flow_invocation_count.get()
    }

    pub(crate) fn set_flow_invocation_count(&self, flow_invocation_count: usize) {
        self.flow_invocation_count.set(flow_invocation_count);
    }

    pub(super) fn maybe_last_flow_node(&self) -> GcCellRefMut<Option<Gc<FlowNode>>> {
        self.last_flow_node.borrow_mut()
    }

    pub(super) fn last_flow_node_reachable(&self) -> bool {
        self.last_flow_node_reachable.get()
    }

    pub(super) fn set_last_flow_node_reachable(&self, last_flow_node_reachable: bool) {
        self.last_flow_node_reachable.set(last_flow_node_reachable);
    }

    pub(super) fn maybe_flow_type_cache(&self) -> GcCellRefMut<Option<HashMap<NodeId, Gc<Type>>>> {
        self.flow_type_cache.borrow_mut()
    }

    pub(super) fn empty_string_type(&self) -> Gc<Type> {
        self.empty_string_type.clone().unwrap()
    }

    pub(super) fn zero_type(&self) -> Gc<Type> {
        self.zero_type.clone().unwrap()
    }

    pub(super) fn zero_big_int_type(&self) -> Gc<Type> {
        self.zero_big_int_type.clone().unwrap()
    }

    pub(super) fn resolution_targets(&self) -> GcCellRefMut<Vec<TypeSystemEntity>> {
        self.resolution_targets.borrow_mut()
    }

    pub(super) fn resolution_results(&self) -> RefMut<Vec<bool>> {
        self.resolution_results.borrow_mut()
    }

    pub(super) fn resolution_property_names(&self) -> RefMut<Vec<TypeSystemPropertyName>> {
        self.resolution_property_names.borrow_mut()
    }

    pub(super) fn suggestion_count(&self) -> usize {
        self.suggestion_count.get()
    }

    pub(super) fn increment_suggestion_count(&self) {
        self.suggestion_count.set(self.suggestion_count.get() + 1)
    }

    pub(super) fn merged_symbols(&self) -> GcCellRefMut<HashMap<u32, Gc<Symbol>>> {
        self.merged_symbols.borrow_mut()
    }

    pub(super) fn node_links(&self) -> GcCellRefMut<HashMap<NodeId, Gc<GcCell<NodeLinks>>>> {
        self.node_links.borrow_mut()
    }

    pub(super) fn flow_loop_caches(
        &self,
    ) -> GcCellRefMut<HashMap<usize, Gc<GcCell<HashMap<String, Gc<Type>>>>>> {
        self.flow_loop_caches.borrow_mut()
    }

    pub(super) fn flow_loop_nodes(&self) -> GcCellRefMut<HashMap<usize, Gc<FlowNode>>> {
        self.flow_loop_nodes.borrow_mut()
    }

    pub(super) fn flow_loop_keys(&self) -> RefMut<HashMap<usize, String>> {
        self.flow_loop_keys.borrow_mut()
    }

    pub(super) fn flow_loop_types(&self) -> GcCellRefMut<HashMap<usize, Vec<Gc<Type>>>> {
        self.flow_loop_types.borrow_mut()
    }

    pub(super) fn shared_flow_nodes(&self) -> GcCellRefMut<HashMap<usize, Gc<FlowNode>>> {
        self.shared_flow_nodes.borrow_mut()
    }

    pub(super) fn shared_flow_types(&self) -> GcCellRefMut<HashMap<usize, FlowType>> {
        self.shared_flow_types.borrow_mut()
    }

    pub(super) fn flow_node_reachable(&self) -> RefMut<HashMap<usize, bool>> {
        self.flow_node_reachable.borrow_mut()
    }

    pub(super) fn flow_node_post_super(&self) -> RefMut<HashMap<usize, bool>> {
        self.flow_node_post_super.borrow_mut()
    }

    pub(super) fn potential_this_collisions(&self) -> GcCellRefMut<Vec<Gc<Node>>> {
        self.potential_this_collisions.borrow_mut()
    }

    pub(super) fn potential_new_target_collisions(&self) -> GcCellRefMut<Vec<Gc<Node>>> {
        self.potential_new_target_collisions.borrow_mut()
    }

    pub(super) fn potential_weak_map_set_collisions(&self) -> GcCellRefMut<Vec<Gc<Node>>> {
        self.potential_weak_map_set_collisions.borrow_mut()
    }

    pub(super) fn potential_reflect_collisions(&self) -> GcCellRefMut<Vec<Gc<Node>>> {
        self.potential_reflect_collisions.borrow_mut()
    }

    pub(super) fn awaited_type_stack(&self) -> RefMut<Vec<TypeId>> {
        self.awaited_type_stack.borrow_mut()
    }

    pub(super) fn diagnostics(&self) -> GcCellRefMut<DiagnosticCollection> {
        self.diagnostics.borrow_mut()
    }

    pub(super) fn suggestion_diagnostics(&self) -> GcCellRefMut<DiagnosticCollection> {
        self.suggestion_diagnostics.borrow_mut()
    }

    pub(super) fn typeof_types_by_name(&self) -> &HashMap<&'static str, Gc<Type>> {
        self.typeof_types_by_name.as_ref().unwrap()
    }

    pub(super) fn typeof_type(&self) -> Gc<Type> {
        self.typeof_type.clone().unwrap()
    }

    pub(super) fn maybe_outofband_variance_marker_handler(
        &self,
    ) -> Option<Gc<Box<dyn OutofbandVarianceMarkerHandler>>> {
        self.outofband_variance_marker_handler.borrow().clone()
    }

    pub(super) fn set_outofband_variance_marker_handler(
        &self,
        outofband_variance_marker_handler: Option<Gc<Box<dyn OutofbandVarianceMarkerHandler>>>,
    ) {
        *self.outofband_variance_marker_handler.borrow_mut() = outofband_variance_marker_handler;
    }

    pub(super) fn subtype_relation(&self) -> Ref<HashMap<String, RelationComparisonResult>> {
        (*self.subtype_relation).borrow()
    }

    pub(super) fn strict_subtype_relation(&self) -> Ref<HashMap<String, RelationComparisonResult>> {
        (*self.strict_subtype_relation).borrow()
    }

    pub(super) fn assignable_relation(&self) -> Ref<HashMap<String, RelationComparisonResult>> {
        (*self.assignable_relation).borrow()
    }

    pub(super) fn comparable_relation(&self) -> Ref<HashMap<String, RelationComparisonResult>> {
        (*self.comparable_relation).borrow()
    }

    pub(super) fn identity_relation(&self) -> Ref<HashMap<String, RelationComparisonResult>> {
        (*self.identity_relation).borrow()
    }

    pub(super) fn enum_relation(&self) -> RefMut<HashMap<String, RelationComparisonResult>> {
        self.enum_relation.borrow_mut()
    }

    pub(super) fn builtin_globals(&self) -> GcCellRef<SymbolTable> {
        GcCellRef::map(self.builtin_globals.borrow(), |builtin_globals| {
            builtin_globals.as_ref().unwrap()
        })
    }
}

pub struct RelationCacheSizes {
    pub assignable: usize,
    pub identity: usize,
    pub subtype: usize,
    pub strict_subtype: usize,
}
