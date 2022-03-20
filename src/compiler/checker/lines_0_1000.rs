#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use regex::Regex;
use std::array::IntoIter;
use std::borrow::Borrow;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::iter::FromIterator;
use std::ptr;
use std::rc::Rc;
use std::fmt;

use super::{create_node_builder, is_not_accessor, is_not_overload};
use crate::{
Diagnostics,    add_range, contains_parse_error, get_first_identifier, is_function_like,
    is_property_access_expression, is_property_access_or_qualified_name_or_import_type_node,
    is_source_file, skip_type_checking, unescape_leading_underscores, BaseInterfaceType,
    CancellationToken, CancellationTokenDebuggable, CheckFlags, ContextFlags, Debug_, Diagnostic,
    DiagnosticCategory, DiagnosticCollection, DiagnosticMessage,
    DiagnosticRelatedInformationInterface, EmitTextWriter, Extension, FreshableIntrinsicType,
    GenericableTypeInterface, IndexInfo, IndexKind, InternalSymbolName, IterationTypes,
    ModuleInstanceState, Node, NodeArray, NodeBuilderFlags, NodeCheckFlags, NodeFlags, NodeId,
    NodeInterface, Number, ObjectFlags, ObjectFlagsTypeInterface, RelationComparisonResult,
    Signature, SignatureFlags, SignatureKind, StringOrNumber, Symbol, SymbolFlags,
    SymbolFormatFlags, SymbolId, SymbolInterface, SymbolTable, SymbolTracker, SymbolWalker,
    SyntaxKind, Type, TypeChecker, TypeCheckerHostDebuggable, TypeFlags, TypeFormatFlags,
    TypeInterface, TypePredicate, TypePredicateKind, VarianceFlags, __String,
    create_diagnostic_collection, create_symbol_table, escape_leading_underscores, find_ancestor,
    get_allow_synthetic_default_imports, get_emit_module_kind, get_emit_script_target,
    get_module_instance_state, get_parse_tree_node, get_strict_option_value,
    get_use_define_for_class_fields, is_assignment_pattern, is_call_like_expression,
    is_export_specifier, is_expression, is_identifier, is_jsx_attribute_like,
    is_object_literal_element_like, is_parameter, is_type_node, object_allocator, sum,
};

lazy_static! {
    pub(super) static ref ambient_module_symbol_regex: Regex = Regex::new(r#"^".+"$"#).unwrap();
}

lazy_static! {
    pub(super) static ref anon: __String = __String::new("(anonymous)".to_owned());
}

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
    pub(super) static next_flow_id: Cell<u32> = Cell::new(1);
}

pub(super) fn get_next_flow_id() -> u32 {
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
    pub iterable_cache_key: &'static str, /*"iterationTypesOfAsyncIterable" | "iterationTypesOfIterable"*/
    pub iterator_cache_key: &'static str, /*"iterationTypesOfAsyncIterator" | "iterationTypesOfIterator"*/
    pub iterator_symbol_name: &'static str, /*"asyncIterator" | "iterator"*/
    pub get_global_iterator_type: fn(&TypeChecker, report_errors: bool) -> Rc<Type /*GenericType*/>,
    pub get_global_iterable_type: fn(&TypeChecker, report_errors: bool) -> Rc<Type /*GenericType*/>,
    pub get_global_iterable_iterator_type:
        fn(&TypeChecker, report_errors: bool) -> Rc<Type /*GenericType*/>,
    pub get_global_generator_type:
        fn(&TypeChecker, report_errors: bool) -> Rc<Type /*GenericType*/>,
    pub resolve_iteration_type:
        fn(&TypeChecker, type_: &Type, error_node: Option<Rc<Node>>) -> Option<Rc<Type>>,
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
        const EmptyObjectStrictFacts = Self::All.bits | !(Self::EQUndefined.bits | Self::EQNull.bits | Self::EQUndefinedOrNull.bits);
        const AllTypeofNE = Self::TypeofNEString.bits | Self::TypeofNENumber.bits | Self::TypeofNEBigInt.bits | Self::TypeofNEBoolean.bits | Self::TypeofNESymbol.bits | Self::TypeofNEObject.bits | Self::TypeofNEFunction.bits | Self::NEUndefined.bits;
        const EmptyObjectFacts = Self::All.bits;
    }
}

lazy_static! {
    pub(super) static ref typeof_eq_facts: HashMap<&'static str, TypeFacts> =
        HashMap::from_iter(IntoIter::new([
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
        HashMap::from_iter(IntoIter::new([
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

#[derive(Debug)]
pub(super) enum TypeSystemEntity {
    Node(Rc<Node>),
    Symbol(Rc<Symbol>),
    Type(Rc<Type>),
    Signature(Rc<Signature>),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum TypeSystemPropertyName {
    Type,
    ResolvedBaseConstructorType,
    DeclaredType,
    ResolvedReturnType,
    ImmediateBaseConstraint,
    EnumTagType,
    ResolvedTypeArguments,
    ResolvedBaseTypes,
}

bitflags! {
    pub(super) struct CheckMode: u32 {
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
        HashMap::from_iter(IntoIter::new([
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
    host: Rc<dyn TypeCheckerHostDebuggable>,
    produce_diagnostics: bool,
) -> TypeChecker {
    let compiler_options = host.get_compiler_options();
    let mut type_checker = TypeChecker {
        host,
        produce_diagnostics,
        _types_needing_strong_references: RefCell::new(vec![]),
        _packages_map: RefCell::new(None),
        cancellation_token: RefCell::new(None),
        requested_external_emit_helpers: Cell::new(None),
        external_helpers_module: RefCell::new(None),

        Symbol: object_allocator.get_symbol_constructor(),
        Type: object_allocator.get_type_constructor(),
        Signature: object_allocator.get_signature_constructor(),

        type_count: Cell::new(0),
        symbol_count: Cell::new(0),
        enum_count: Cell::new(0),
        total_instantiation_count: Cell::new(0),
        instantiation_count: Cell::new(0),
        instantiation_depth: Cell::new(0),
        inline_level: Cell::new(0),
        current_node: RefCell::new(None),

        empty_symbols: Rc::new(RefCell::new(create_symbol_table(None))),

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

        // const checkBinaryExpression = createCheckBinaryExpression();
        emit_resolver: None,
        node_builder: create_node_builder(),

        globals: Rc::new(RefCell::new(create_symbol_table(None))),
        undefined_symbol: None,
        global_this_symbol: None,

        arguments_symbol: None,
        require_symbol: None,

        apparent_argument_count: Cell::new(None),

        tuple_types: RefCell::new(HashMap::new()),
        union_types: RefCell::new(HashMap::new()),
        intersection_types: RefCell::new(HashMap::new()),
        string_literal_types: RefCell::new(HashMap::new()),
        number_literal_types: RefCell::new(HashMap::new()),
        big_int_literal_types: RefCell::new(HashMap::new()),
        enum_literal_types: RefCell::new(HashMap::new()),
        indexed_access_types: RefCell::new(HashMap::new()),
        template_literal_types: RefCell::new(HashMap::new()),
        string_mapping_types: RefCell::new(HashMap::new()),
        substitution_types: RefCell::new(HashMap::new()),
        subtype_reduction_cache: RefCell::new(HashMap::new()),
        evolving_array_types: RefCell::new(vec![]),
        undefined_properties: RefCell::new(HashMap::new()),

        unknown_symbol: None,
        resolving_symbol: None,
        unresolved_symbols: RefCell::new(HashMap::new()),
        error_types: RefCell::new(HashMap::new()),

        any_type: None,
        auto_type: None,
        wildcard_type: None,
        error_type: None,
        unresolved_type: None,
        non_inferrable_any_type: None,
        intrinsic_marker_type: None,
        unknown_type: None,
        non_null_unknown_type: None,
        undefined_type: None,
        undefined_widening_type: None,
        optional_type: None,
        missing_type: None,
        null_type: None,
        null_widening_type: None,
        string_type: None,
        number_type: None,
        bigint_type: None,
        false_type: None,
        regular_false_type: None,
        true_type: None,
        regular_true_type: None,
        boolean_type: None,
        es_symbol_type: None,
        void_type: None,
        never_type: None,
        silent_never_type: None,
        non_inferrable_type: None,
        implicit_never_type: None,
        unreachable_never_type: None,
        non_primitive_type: None,
        string_or_number_type: None,
        string_number_symbol_type: None,
        keyof_constraint_type: None,
        number_or_big_int_type: None,
        template_constraint_type: None,

        restrictive_mapper: None,
        permissive_mapper: None,

        empty_object_type: None,
        empty_jsx_object_type: None,
        empty_type_literal_symbol: None,
        empty_type_literal_type: None,

        empty_generic_type: None,

        any_function_type: None,

        no_constraint_type: None,
        circular_constraint_type: None,
        resolving_default_type: None,

        marker_super_type: None,
        marker_sub_type: None,
        marker_other_type: None,

        no_type_predicate: None,

        any_signature: None,
        unknown_signature: None,
        resolving_signature: None,
        silent_never_signature: None,

        enum_number_index_info: None,

        iteration_types_cache: RefCell::new(HashMap::new()),
        no_iteration_types: Rc::new(IterationTypes::new_no_iteration_types()),

        any_iteration_types: None,
        any_iteration_types_except_next: None,
        default_iteration_types: None,

        async_iteration_types_resolver: 
            IterationTypesResolver {
                iterable_cache_key: "iterationTypesOfAsyncIterable",
                iterator_cache_key: "iterationTypesOfAsyncIterator",
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
                iterable_cache_key: "iterationTypesOfIterable",
                iterator_cache_key: "iterationTypesOfIterator",
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

        global_array_type: None,

        deferred_global_promise_type: RefCell::new(None),
        deferred_global_promise_constructor_symbol: RefCell::new(None),

        symbol_links: RefCell::new(HashMap::new()),
        node_links: RefCell::new(HashMap::new()),

        diagnostics: RefCell::new(create_diagnostic_collection()),
        suggestion_diagnostics: RefCell::new(create_diagnostic_collection()),

        subtype_relation: RefCell::new(HashMap::new()),
        strict_subtype_relation: RefCell::new(HashMap::new()),
        assignable_relation: RefCell::new(HashMap::new()),
        comparable_relation: RefCell::new(HashMap::new()),
        identity_relation: RefCell::new(HashMap::new()),
        enum_relation: RefCell::new(HashMap::new()),
    };
    type_checker.emit_resolver = Some(type_checker.create_resolver());
    type_checker.undefined_symbol = Some(
        type_checker
            .create_symbol(
                SymbolFlags::Property,
                __String::new("undefined".to_owned()),
                None,
            )
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
                __String::new("globalThis".to_owned()),
                Some(CheckFlags::Readonly),
            )
            .into(),
    );
    let global_this_symbol = type_checker.global_this_symbol();
    {
        let mut global_this_symbol_exports = global_this_symbol.maybe_exports();
        *global_this_symbol_exports = Some(type_checker.globals_rc());
    }
    global_this_symbol.set_declarations(vec![]);
    type_checker.globals().insert(
        global_this_symbol.escaped_name().clone(),
        global_this_symbol,
    );
    type_checker.arguments_symbol = Some(
        type_checker
            .create_symbol(
                SymbolFlags::Property,
                __String::new("arguments".to_owned()),
                None,
            )
            .into(),
    );
    type_checker.require_symbol = Some(
        type_checker
            .create_symbol(
                SymbolFlags::Property,
                __String::new("require".to_owned()),
                None,
            )
            .into(),
    );
    type_checker.unknown_symbol = Some(
        type_checker
            .create_symbol(
                SymbolFlags::Property,
                __String::new("unknown".to_string()),
                None,
            )
            .into(),
    );
    type_checker.resolving_symbol = Some(
        type_checker
            .create_symbol(SymbolFlags::None, InternalSymbolName::Resolving(), None)
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
    let false_type: Rc<Type> = FreshableIntrinsicType::new(type_checker.create_intrinsic_type(
        TypeFlags::BooleanLiteral,
        "false",
        None,
    ))
    .into();
    let regular_false_type: Rc<Type> = FreshableIntrinsicType::new(
        type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "false", None),
    )
    .into();
    let false_type_as_freshable_intrinsic_type = false_type.as_freshable_intrinsic_type();
    false_type_as_freshable_intrinsic_type
        .regular_type
        .init(&regular_false_type, false);
    false_type_as_freshable_intrinsic_type
        .fresh_type
        .init(&false_type, false);
    type_checker.false_type = Some(false_type);
    let regular_false_type_as_freshable_intrinsic_type =
        regular_false_type.as_freshable_intrinsic_type();
    regular_false_type_as_freshable_intrinsic_type
        .regular_type
        .init(&regular_false_type, false);
    regular_false_type_as_freshable_intrinsic_type
        .fresh_type
        .init(type_checker.false_type.as_ref().unwrap(), false);
    type_checker.regular_false_type = Some(regular_false_type);
    let true_type: Rc<Type> = FreshableIntrinsicType::new(type_checker.create_intrinsic_type(
        TypeFlags::BooleanLiteral,
        "true",
        None,
    ))
    .into();
    let regular_true_type: Rc<Type> = FreshableIntrinsicType::new(
        type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "true", None),
    )
    .into();
    let true_type_as_freshable_intrinsic_type = true_type.as_freshable_intrinsic_type();
    true_type_as_freshable_intrinsic_type
        .regular_type
        .init(&regular_true_type, false);
    true_type_as_freshable_intrinsic_type
        .fresh_type
        .init(&true_type, false);
    type_checker.true_type = Some(true_type);
    let regular_true_type_as_freshable_intrinsic_type =
        regular_true_type.as_freshable_intrinsic_type();
    regular_true_type_as_freshable_intrinsic_type
        .regular_type
        .init(&regular_true_type, false);
    regular_true_type_as_freshable_intrinsic_type
        .fresh_type
        .init(type_checker.true_type.as_ref().unwrap(), false);
    type_checker.regular_true_type = Some(regular_true_type);
    type_checker.boolean_type = Some(type_checker.get_union_type(
        vec![
            type_checker.regular_false_type(),
            type_checker.regular_true_type(),
        ],
        None,
    ));
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
        vec![type_checker.string_type(), type_checker.number_type()],
        None,
    ));
    type_checker.string_number_symbol_type = Some(type_checker.get_union_type(
        vec![
            type_checker.string_type(),
            type_checker.number_type(),
            type_checker.es_symbol_type(),
        ],
        None,
    ));
    type_checker.keyof_constraint_type = Some(if type_checker.keyof_strings_only {
        type_checker.string_type()
    } else {
        type_checker.string_number_symbol_type()
    });
    type_checker.number_or_big_int_type = Some(type_checker.get_union_type(
        vec![type_checker.number_type(), type_checker.bigint_type()],
        None,
    ));
    type_checker.template_constraint_type = Some(type_checker.get_union_type(
        vec![
            type_checker.string_type(),
            type_checker.number_type(),
            type_checker.boolean_type(),
            type_checker.bigint_type(),
            type_checker.null_type(),
            type_checker.undefined_type(),
        ],
        None,
    ));

    type_checker.restrictive_mapper = Some(Rc::new(
        type_checker.make_function_type_mapper(restrictive_mapper_func),
    ));
    type_checker.permissive_mapper = Some(Rc::new(
        type_checker.make_function_type_mapper(permissive_mapper_func),
    ));

    type_checker.empty_object_type = Some(
        type_checker
            .create_anonymous_type(
                Option::<&Symbol>::None,
                type_checker.empty_symbols(),
                vec![],
                vec![],
                vec![],
            )
            .into(),
    );
    let empty_jsx_object_type = type_checker.create_anonymous_type(
        Option::<&Symbol>::None,
        type_checker.empty_symbols(),
        vec![],
        vec![],
        vec![],
    );
    empty_jsx_object_type
        .set_object_flags(empty_jsx_object_type.object_flags() | ObjectFlags::JsxAttributes);
    type_checker.empty_jsx_object_type = Some(empty_jsx_object_type.into());

    let empty_type_literal_symbol =
        type_checker.create_symbol(SymbolFlags::TypeLiteral, InternalSymbolName::Type(), None);
    *empty_type_literal_symbol.maybe_members() =
        Some(Rc::new(RefCell::new(create_symbol_table(None))));
    type_checker.empty_type_literal_symbol = Some(empty_type_literal_symbol.into());
    type_checker.empty_type_literal_type = Some(
        type_checker
            .create_anonymous_type(
                Some(type_checker.empty_type_literal_symbol()),
                type_checker.empty_symbols(),
                vec![],
                vec![],
                vec![],
            )
            .into(),
    );

    let empty_generic_type = type_checker.create_anonymous_type(
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
    any_function_type
        .set_object_flags(any_function_type.object_flags() | ObjectFlags::NonInferrableType);
    type_checker.any_function_type = Some(any_function_type.into());

    type_checker.no_constraint_type = Some(
        type_checker
            .create_anonymous_type(
                Option::<&Symbol>::None,
                type_checker.empty_symbols(),
                vec![],
                vec![],
                vec![],
            )
            .into(),
    );
    type_checker.circular_constraint_type = Some(
        type_checker
            .create_anonymous_type(
                Option::<&Symbol>::None,
                type_checker.empty_symbols(),
                vec![],
                vec![],
                vec![],
            )
            .into(),
    );
    type_checker.resolving_default_type = Some(
        type_checker
            .create_anonymous_type(
                Option::<&Symbol>::None,
                type_checker.empty_symbols(),
                vec![],
                vec![],
                vec![],
            )
            .into(),
    );

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

    type_checker.no_type_predicate = Some(Rc::new(type_checker.create_type_predicate(
        TypePredicateKind::Identifier,
        Some("<<unresolved>>".to_owned()),
        Some(0),
        Some(type_checker.any_type()),
    )));

    type_checker.any_signature = Some(Rc::new(type_checker.create_signature(
        None,
        None,
        None,
        vec![],
        Some(type_checker.any_type()),
        None,
        0,
        SignatureFlags::None,
    )));
    type_checker.unknown_signature = Some(Rc::new(type_checker.create_signature(
        None,
        None,
        None,
        vec![],
        Some(type_checker.error_type()),
        None,
        0,
        SignatureFlags::None,
    )));
    type_checker.resolving_signature = Some(Rc::new(type_checker.create_signature(
        None,
        None,
        None,
        vec![],
        Some(type_checker.any_type()),
        None,
        0,
        SignatureFlags::None,
    )));
    type_checker.silent_never_signature = Some(Rc::new(type_checker.create_signature(
        None,
        None,
        None,
        vec![],
        Some(type_checker.silent_never_type()),
        None,
        0,
        SignatureFlags::None,
    )));

    type_checker.enum_number_index_info = Some(Rc::new(type_checker.create_index_info(
        type_checker.number_type(),
        type_checker.string_type(),
        true,
        None,
    )));

    type_checker.any_iteration_types = Some(Rc::new(type_checker.create_iteration_types(
        Some(type_checker.any_type()),
        Some(type_checker.any_type()),
        Some(type_checker.any_type()),
    )));
    type_checker.any_iteration_types_except_next =
        Some(Rc::new(type_checker.create_iteration_types(
            Some(type_checker.any_type()),
            Some(type_checker.any_type()),
            Some(type_checker.unknown_type()),
        )));
    type_checker.default_iteration_types = Some(Rc::new(type_checker.create_iteration_types(
        Some(type_checker.never_type()),
        Some(type_checker.any_type()),
        Some(type_checker.undefined_type()),
    )));

    type_checker.initialize_type_checker();

    type_checker
}

fn restrictive_mapper_func(type_checker: &TypeChecker, t: &Type) -> Rc<Type> {
    if t.flags().intersects(TypeFlags::TypeParameter) {
        type_checker.get_restrictive_type_parameter(t)
    } else {
        t.type_wrapper()
    }
}

fn permissive_mapper_func(type_checker: &TypeChecker, t: &Type) -> Rc<Type> {
    if t.flags().intersects(TypeFlags::TypeParameter) {
        type_checker.wildcard_type()
    } else {
        t.type_wrapper()
    }
}

fn async_iteration_types_resolver_resolve_iteration_type(type_checker: &TypeChecker, type_: &Type, error_node: Option<Rc<Node>>) -> Option<Rc<Type>> {
    type_checker.get_awaited_type_(type_, error_node, None, None)
}

fn sync_iteration_types_resolver_resolve_iteration_type(_type_checker: &TypeChecker, type_: &Type, _error_node: Option<Rc<Node>>) -> Option<Rc<Type>> {
    Some(type_.type_wrapper())
}

impl TypeChecker {
    pub fn keep_strong_reference_to_type(&self, type_: Rc<Type>) {
        self._types_needing_strong_references
            .borrow_mut()
            .push(type_);
    }

    pub(super) fn get_packages_map(&self) -> Ref<HashMap<String, bool>> {
        {
            let mut packages_map = self._packages_map.borrow_mut();
            if packages_map.is_none() {
                *packages_map = Some(HashMap::new());
            }
            let mut map = packages_map.as_mut().unwrap();
            self.host.get_source_files().iter().for_each(|sf| {
                let sf_as_source_file = sf.as_source_file();
                let sf_resolved_modules = sf_as_source_file.maybe_resolved_modules();
                if sf_resolved_modules.is_none() {
                    return;
                }

                sf_resolved_modules.as_ref().unwrap().for_each(|r, _, _| {
                    // if let Some(r) = r {
                    if let Some(r_package_id) = r.package_id.as_ref() {
                        map.insert(
                            r_package_id.name.clone(),
                            matches!(r.extension, Some(Extension::Dts))
                                || matches!(map.get(&r_package_id.name), Some(true)),
                        );
                    }
                    // }
                })
            });
        }
        Ref::map(self._packages_map.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub(super) fn set_cancellation_token(
        &self,
        cancellation_token: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) {
        *self.cancellation_token.borrow_mut() = cancellation_token;
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

    pub(super) fn total_instantiation_count(&self) -> usize {
        self.total_instantiation_count.get()
    }

    pub(super) fn empty_symbols(&self) -> Rc<RefCell<SymbolTable>> {
        self.empty_symbols.clone()
    }

    pub(super) fn array_variances(&self) -> Vec<VarianceFlags> {
        vec![VarianceFlags::Covariant]
    }

    pub(super) fn globals(&self) -> RefMut<SymbolTable> {
        self.globals.borrow_mut()
    }

    pub(super) fn globals_rc(&self) -> Rc<RefCell<SymbolTable>> {
        self.globals.clone()
    }

    pub(super) fn undefined_symbol(&self) -> Rc<Symbol> {
        self.undefined_symbol.clone().unwrap()
    }

    pub(super) fn global_this_symbol(&self) -> Rc<Symbol> {
        self.global_this_symbol.clone().unwrap()
    }

    pub(super) fn arguments_symbol(&self) -> Rc<Symbol> {
        self.arguments_symbol.clone().unwrap()
    }

    pub(super) fn apparent_argument_count(&self) -> Option<usize> {
        self.apparent_argument_count.get()
    }

    pub(super) fn set_apparent_argument_count(&self, apparent_argument_count: Option<usize>) {
        self.apparent_argument_count.set(apparent_argument_count)
    }

    pub fn get_node_count(&self) -> usize {
        sum(self.host.get_source_files(), |source_file| {
            source_file.as_source_file().node_count()
        })
    }

    pub fn get_identifier_count(&self) -> usize {
        sum(self.host.get_source_files(), |source_file| {
            source_file.as_source_file().identifier_count()
        })
    }

    pub fn get_symbol_count(&self) -> usize {
        sum(self.host.get_source_files(), |source_file| {
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

    pub fn get_type_of_symbol_at_location(&self, symbol: &Symbol, location_in: &Node) -> Rc<Type> {
        let location = get_parse_tree_node(Some(location_in), Option::<fn(&Node) -> bool>::None);
        match location {
            Some(location) => self.get_type_of_symbol_at_location_(symbol, &location),
            None => self.error_type(),
        }
    }

    pub fn get_symbols_of_parameter_property_declaration(
        &self,
        parameter_in: &Node, /*ParameterDeclaration*/
        parameter_name: &str,
    ) -> Vec<Rc<Symbol>> {
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

    pub fn get_property_of_type(&self, type_: &Type, name: &str) -> Option<Rc<Symbol>> {
        self.get_property_of_type_(type_, &escape_leading_underscores(name))
    }

    pub fn get_private_identifier_property_of_type(
        &self,
        left_type: &Type,
        name: &str,
        location: &Node,
    ) -> Option<Rc<Symbol>> {
        let node = get_parse_tree_node(Some(location), Option::<fn(&Node) -> bool>::None)?;
        let prop_name = escape_leading_underscores(name);
        let lexically_scoped_identifier =
            self.lookup_symbol_for_private_identifier_declaration(&prop_name, &node);
        lexically_scoped_identifier.and_then(|lexically_scoped_identifier| {
            self.get_private_identifier_property_of_type_(left_type, &lexically_scoped_identifier)
        })
    }

    pub fn get_type_of_property_of_type(&self, type_: &Type, name: &str) -> Option<Rc<Type>> {
        self.get_type_of_property_of_type_(type_, &escape_leading_underscores(name))
    }

    pub fn get_index_info_of_type(&self, type_: &Type, kind: IndexKind) -> Option<Rc<IndexInfo>> {
        self.get_index_info_of_type_(
            type_,
            &*if kind == IndexKind::String {
                self.string_type()
            } else {
                self.number_type()
            },
        )
    }

    pub fn get_index_type_of_type(&self, type_: &Type, kind: IndexKind) -> Option<Rc<Type>> {
        self.get_index_type_of_type_(
            type_,
            &*if kind == IndexKind::String {
                self.string_type()
            } else {
                self.number_type()
            },
        )
    }

    pub fn get_type_from_type_node(&self, node_in: &Node /*TypeNode*/) -> Rc<Type> {
        let node = get_parse_tree_node(Some(node_in), Some(|node: &Node| is_type_node(node)));
        if let Some(node) = node {
            self.get_type_from_type_node_(&node)
        } else {
            self.error_type()
        }
    }

    pub fn get_parameter_type(&self, signature: &Signature, parameter_index: usize) -> Rc<Type> {
        self.get_type_at_position(signature, parameter_index)
    }

    pub fn get_awaited_type(&self, type_: &Type) -> Option<Rc<Type>> {
        self.get_awaited_type_(type_, Option::<&Node>::None, None, None)
    }

    pub fn get_non_optional_type(&self, type_: &Type) -> Rc<Type> {
        self.remove_optional_type_marker(type_)
    }

    pub fn type_to_type_node<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_: &Type,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node /*TypeNode*/> {
        self.node_builder
            .type_to_type_node(self, type_, enclosing_declaration, flags, tracker)
    }

    pub fn index_info_to_index_signature_declaration<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        index_info: &IndexInfo,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node /*IndexSignatureDeclaration*/> {
        self.node_builder.index_info_to_index_signature_declaration(
            index_info,
            enclosing_declaration,
            flags,
            tracker,
        )
    }

    pub fn signature_to_signature_declaration<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        signature: &Signature,
        kind: SyntaxKind,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node /*SignatureDeclaration & {typeArguments?: NodeArray<TypeNode>}*/> {
        self.node_builder.signature_to_signature_declaration(
            signature,
            kind,
            enclosing_declaration,
            flags,
            tracker,
        )
    }

    pub fn symbol_to_entity_name<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        meaning: SymbolFlags,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
    ) -> Option<Node /*EntityName*/> {
        self.node_builder
            .symbol_to_entity_name(symbol, meaning, enclosing_declaration, flags, None)
    }

    pub fn symbol_to_expression<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        meaning: SymbolFlags,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
    ) -> Option<Node /*Expression*/> {
        self.node_builder.symbol_to_expression(
            self,
            symbol,
            Some(meaning),
            enclosing_declaration,
            flags,
            None,
        )
    }

    pub fn symbol_to_type_parameter_declarations<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
    ) -> Option<NodeArray /*<TypeParameterDeclaration>*/> {
        self.node_builder.symbol_to_type_parameter_declarations(
            symbol,
            enclosing_declaration,
            flags,
            None,
        )
    }

    pub fn symbol_to_parameter_declaration<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
    ) -> Option<Node /*ParameterDeclaration*/> {
        self.node_builder.symbol_to_parameter_declaration(
            symbol,
            enclosing_declaration,
            flags,
            None,
        )
    }

    pub fn type_parameter_to_declaration<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        parameter: &Node, /*TypeParameter*/
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
    ) -> Option<Node /*TypeParameterDeclaration*/> {
        self.node_builder.type_parameter_to_declaration(
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
    ) -> Vec<Rc<Symbol>> {
        let location = get_parse_tree_node(Some(location_in), Option::<fn(&Node) -> bool>::None);
        location.map_or_else(
            || vec![],
            |location| self.get_symbols_in_scope_(&location, meaning),
        )
    }

    pub fn get_symbol_at_location(&self, node_in: &Node) -> Option<Rc<Symbol>> {
        let node = get_parse_tree_node(Some(node_in), Option::<fn(&Node) -> bool>::None)?;
        self.get_symbol_at_location_(&node, Some(true))
    }

    pub fn get_index_infos_at_location(&self, node_in: &Node) -> Option<Vec<Rc<IndexInfo>>> {
        let node = get_parse_tree_node(Some(node_in), Option::<fn(&Node) -> bool>::None)?;
        self.get_index_infos_at_location_(&node)
    }

    pub fn get_shorthand_assignment_value_symbol<TNode: Borrow<Node>>(
        &self,
        node_in: Option<TNode>,
    ) -> Option<Rc<Symbol>> {
        let node = get_parse_tree_node(node_in, Option::<fn(&Node) -> bool>::None)?;
        self.get_shorthand_assignment_value_symbol_(Some(node))
    }

    pub fn get_export_specifier_local_target_symbol(
        &self,
        node_in: &Node, /*Identifier | ExportSpecifier*/
    ) -> Option<Rc<Symbol>> {
        let node =
            get_parse_tree_node(Some(node_in), Some(|node: &Node| is_export_specifier(node)))?;
        self.get_export_specifier_local_target_symbol_(&node)
    }

    pub fn get_export_symbol_of_symbol(&self, symbol: &Symbol) -> Rc<Symbol> {
        self.get_merged_symbol(Some(
            symbol
                .maybe_export_symbol()
                .unwrap_or_else(|| symbol.symbol_wrapper()),
        ))
        .unwrap()
    }

    pub fn get_type_at_location(&self, node_in: &Node) -> Rc<Type> {
        let node = get_parse_tree_node(Some(node_in), Option::<fn(&Node) -> bool>::None);
        if let Some(node) = node {
            self.get_type_of_node(&node)
        } else {
            self.error_type()
        }
    }

    pub fn get_type_of_assignment_pattern(
        &self,
        node_in: &Node, /*AssignmentPattern*/
    ) -> Rc<Type> {
        let node = get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| is_assignment_pattern(node)),
        );
        node.and_then(|node| self.get_type_of_assignment_pattern_(&node))
            .unwrap_or_else(|| self.error_type())
    }

    pub fn get_property_symbol_of_destructuring_assignment(
        &self,
        location_in: &Node, /*Identifier*/
    ) -> Option<Rc<Symbol>> {
        let location =
            get_parse_tree_node(Some(location_in), Some(|node: &Node| is_identifier(node)))?;
        self.get_property_symbol_of_destructuring_assignment_(&location)
    }

    pub fn signature_to_string<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        signature: &Signature,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
        kind: Option<SignatureKind>,
    ) -> String {
        self.signature_to_string_(
            signature,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            flags,
            kind,
            None,
        )
    }

    pub fn type_to_string<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_: &Type,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
    ) -> String {
        self.type_to_string_(
            type_,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            flags,
            None,
        )
    }

    pub fn symbol_to_string<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        meaning: Option<SymbolFlags>,
        flags: Option<SymbolFormatFlags>,
    ) -> String {
        self.symbol_to_string_(
            symbol,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            meaning,
            flags,
            None,
        )
    }

    pub fn type_predicate_to_string<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        predicate: &TypePredicate,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
    ) -> String {
        self.type_predicate_to_string_(
            predicate,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            flags,
            None,
        )
    }

    pub fn write_signature<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        signature: &Signature,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
        kind: Option<SignatureKind>,
        writer: Option<&dyn EmitTextWriter>,
    ) -> String {
        self.signature_to_string_(
            signature,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            flags,
            kind,
            writer,
        )
    }

    pub fn write_type<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_: &Type,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
        writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
    ) -> String {
        self.type_to_string_(
            type_,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            flags,
            writer,
        )
    }

    pub fn write_symbol<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        meaning: Option<SymbolFlags>,
        flags: Option<SymbolFormatFlags>,
        writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
    ) -> String {
        self.symbol_to_string_(
            symbol,
            get_parse_tree_node(enclosing_declaration, Option::<fn(&Node) -> bool>::None),
            meaning,
            flags,
            writer,
        )
    }

    pub fn write_type_predicate<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        predicate: &TypePredicate,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
        writer: Option<&dyn EmitTextWriter>,
    ) -> String {
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
    ) -> Option<Rc<Type>> {
        let node = get_parse_tree_node(Some(node_in), Some(|node: &Node| is_expression(node)))?;
        let containing_call =
            find_ancestor(Some(&*node), |node: &Node| is_call_like_expression(node));
        let containing_call_resolved_signature: Option<Rc<Signature>> =
            containing_call.as_ref().and_then(|containing_call| {
                RefCell::borrow(&self.get_node_links(&containing_call))
                    .resolved_signature
                    .clone()
            });
        if matches!(context_flags, Some(context_flags) if context_flags.intersects(ContextFlags::Completions))
        {
            if let Some(containing_call) = containing_call.as_ref() {
                let mut to_mark_skip: Option<Rc<Node>> = Some(node.node_wrapper());
                while {
                    let to_mark_skip_present = to_mark_skip.as_ref().unwrap();
                    self.get_node_links(to_mark_skip_present)
                        .borrow_mut()
                        .skip_direct_inference = Some(true);
                    to_mark_skip = to_mark_skip_present.maybe_parent();
                    matches!(to_mark_skip.as_ref(), Some(to_mark_skip) if !Rc::ptr_eq(to_mark_skip, containing_call))
                } {}
                self.get_node_links(containing_call)
                    .borrow_mut()
                    .resolved_signature = None;
            }
        }
        let result = self.get_contextual_type_(&node, context_flags);
        if matches!(context_flags, Some(context_flags) if context_flags.intersects(ContextFlags::Completions))
        {
            if let Some(containing_call) = containing_call.as_ref() {
                let mut to_mark_skip: Option<Rc<Node>> = Some(node.node_wrapper());
                while {
                    let to_mark_skip_present = to_mark_skip.as_ref().unwrap();
                    self.get_node_links(to_mark_skip_present)
                        .borrow_mut()
                        .skip_direct_inference = None;
                    to_mark_skip = to_mark_skip_present.maybe_parent();
                    matches!(to_mark_skip.as_ref(), Some(to_mark_skip) if !Rc::ptr_eq(to_mark_skip, containing_call))
                } {}
                self.get_node_links(containing_call)
                    .borrow_mut()
                    .resolved_signature = containing_call_resolved_signature;
            }
        }
        result
    }

    pub fn get_contextual_type_for_object_literal_element(
        &self,
        node_in: &Node, /*ObjectLiteralElementLike*/
    ) -> Option<Rc<Type>> {
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
    ) -> Option<Rc<Type>> {
        let node = get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| is_call_like_expression(node)),
        )?;
        Some(self.get_contextual_type_for_argument_at_index_(&node, arg_index))
    }

    pub fn get_contextual_type_for_jsx_attribute(
        &self,
        node_in: &Node, /*ObjectLiteralElementLike*/
    ) -> Option<Rc<Type>> {
        let node = get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| is_jsx_attribute_like(node)),
        )?;
        self.get_contextual_type_for_jsx_attribute_(&node)
    }

    pub fn get_resolved_signature(
        &self,
        node: &Node, /*CallLikeExpression*/
        candidates_out_array: Option<&[Rc<Signature>]>,
        argument_count: Option<usize>,
    ) -> Option<Rc<Signature>> {
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
        candidates_out_array: Option<&[Rc<Signature>]>,
        argument_count: Option<usize>,
    ) -> Option<Rc<Signature>> {
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
    ) -> bool {
        let node = get_parse_tree_node(
            Some(node_in),
            Some(|node: &Node| is_property_access_or_qualified_name_or_import_type_node(node)),
        );
        match node {
            None => false,
            Some(node) => {
                self.is_valid_property_access_(&node, &escape_leading_underscores(property_name))
            }
        }
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
    ) -> Option<Rc<Signature>> {
        let declaration = get_parse_tree_node(
            Some(declaration_in),
            Some(|node: &Node| is_function_like(Some(node))),
        )?;
        Some(self.get_signature_from_declaration_(&declaration))
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

    pub fn get_aliased_symbol(&self, symbol: &Symbol) -> Rc<Symbol> {
        self.resolve_alias(symbol)
    }

    pub fn get_exports_of_module(&self, module_symbol: &Symbol) -> Vec<Rc<Symbol>> {
        self.get_exports_of_module_as_array(module_symbol)
    }

    pub fn get_symbol_walker<TAccept: FnMut(&Symbol) -> bool>(
        &self,
        accept: Option<TAccept>,
    ) -> SymbolWalker {
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
    ) -> Option<Rc<Symbol>> {
        self.try_get_member_in_module_exports_(&escape_leading_underscores(name), symbol)
    }

    pub fn try_get_member_in_module_exports_and_properties(
        &self,
        name: &str,
        symbol: &Symbol,
    ) -> Option<Rc<Symbol>> {
        self.try_get_member_in_module_exports_and_properties_(
            &escape_leading_underscores(name),
            symbol,
        )
    }

    pub fn try_find_ambient_module(&self, module_name: &str) -> Option<Rc<Symbol>> {
        self.try_find_ambient_module_(module_name, true)
    }

    pub fn try_find_ambient_module_without_augmentations(
        &self,
        module_name: &str,
    ) -> Option<Rc<Symbol>> {
        self.try_find_ambient_module_(module_name, false)
    }

    pub fn get_any_type(&self) -> Rc<Type> {
        self.any_type()
    }

    pub fn get_string_type(&self) -> Rc<Type> {
        self.string_type()
    }

    pub fn get_number_type(&self) -> Rc<Type> {
        self.number_type()
    }

    pub fn get_boolean_type(&self) -> Rc<Type> {
        self.boolean_type()
    }

    pub fn get_false_type(&self, fresh: Option<bool>) -> Rc<Type> {
        if matches!(fresh, Some(true)) {
            self.false_type()
        } else {
            self.regular_false_type()
        }
    }

    pub fn get_true_type(&self, fresh: Option<bool>) -> Rc<Type> {
        if matches!(fresh, Some(true)) {
            self.true_type()
        } else {
            self.regular_true_type()
        }
    }

    pub fn get_void_type(&self) -> Rc<Type> {
        self.void_type()
    }

    pub fn get_undefined_type(&self) -> Rc<Type> {
        self.undefined_type()
    }

    pub fn get_null_type(&self) -> Rc<Type> {
        self.null_type()
    }

    pub fn get_es_symbol_type(&self) -> Rc<Type> {
        self.es_symbol_type()
    }

    pub fn get_never_type(&self) -> Rc<Type> {
        self.never_type()
    }

    pub fn get_optional_type(&self) -> Rc<Type> {
        self.optional_type()
    }

    pub fn get_promise_type(&self) -> Rc<Type> {
        self.get_global_promise_type(false)
    }

    pub fn get_promise_like_type(&self) -> Rc<Type> {
        self.get_global_promise_like_type(false)
    }

    pub fn get_suggested_symbol_for_nonexistent_symbol(
        &self,
        location: &Node,
        name: &str,
        meaning: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
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

    pub fn get_default_from_type_parameter(&self, type_: &Type) -> Option<Rc<Type>> {
        /*type &&*/
        if type_.flags().intersects(TypeFlags::TypeParameter) {
            self.get_default_from_type_parameter_(type_)
        } else {
            None
        }
    }

    pub fn resolve_name<TLocation: Borrow<Node>>(
        &self,
        name: &str,
        location: Option<TLocation>,
        meaning: SymbolFlags,
        exclude_globals: bool,
    ) -> Option<Rc<Symbol>> {
        self.resolve_name_(
            location,
            &escape_leading_underscores(name),
            meaning,
            None,
            Option::<Rc<Node>>::None,
            false,
            Some(exclude_globals),
        )
    }

    pub fn get_jsx_namespace<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
    ) -> String {
        unescape_leading_underscores(&self.get_jsx_namespace_(location))
    }

    pub fn get_jsx_fragment_factory(&self, n: &Node) -> Option<String> {
        let jsx_fragment_factory = self.get_jsx_fragment_factory_entity(n)?;
        Some(unescape_leading_underscores(
            &get_first_identifier(&jsx_fragment_factory)
                .as_identifier()
                .escaped_text,
        ))
    }

    pub fn resolve_external_module_name(
        &self,
        module_specifier_in: &Node, /*Expression*/
    ) -> Option<Rc<Symbol>> {
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
    ) -> Option<Rc<Type>> {
        let node = get_parse_tree_node(Some(node_in), Option::<fn(&Node) -> bool>::None)?;
        self.try_get_this_type_at_(&node, include_global_this, Option::<&Node>::None)
    }

    pub fn get_type_argument_constraint(
        &self,
        node_in: &Node, /*TypeNode*/
    ) -> Option<Rc<Type>> {
        let node = get_parse_tree_node(Some(node_in), Some(|node: &Node| is_type_node(node)))?;
        self.get_type_argument_constraint_(&node)
    }

    pub fn get_suggestion_diagnostics(
        &self,
        file_in: &Node, /*SourceFile*/
        ct: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>> {
        let file = get_parse_tree_node(Some(file_in), Some(|node: &Node| is_source_file(node)))
            .unwrap_or_else(|| Debug_.fail(Some("Could not determine parsed source file.")));
        if skip_type_checking(&file, &self.compiler_options, |file_name| {
            self.host.is_source_of_project_reference_redirect(file_name)
        }) {
            return vec![];
        }

        let mut diagnostics: Option<Vec<Rc<Diagnostic>>> = None;
        self.set_cancellation_token(ct);

        self.check_source_file(&file);
        Debug_.assert(
            RefCell::borrow(&self.get_node_links(&file))
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
        token: Rc<dyn CancellationTokenDebuggable>,
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
        candidates_out_array: Option<&[Rc<Signature>]>,
        argument_count: Option<usize>,
        check_mode: CheckMode,
    ) -> Option<Rc<Signature>> {
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

    pub(super) fn string_literal_types(
        &self,
    ) -> RefMut<HashMap<String, Rc</*NumberLiteralType*/ Type>>> {
        self.string_literal_types.borrow_mut()
    }

    pub(super) fn number_literal_types(
        &self,
    ) -> RefMut<HashMap<Number, Rc</*NumberLiteralType*/ Type>>> {
        self.number_literal_types.borrow_mut()
    }

    pub(super) fn big_int_literal_types(
        &self,
    ) -> RefMut<HashMap<String, Rc</*BigIntLiteralType*/ Type>>> {
        self.big_int_literal_types.borrow_mut()
    }

    pub(super) fn unknown_symbol(&self) -> Rc<Symbol> {
        self.unknown_symbol.as_ref().unwrap().clone()
    }

    pub(super) fn any_type(&self) -> Rc<Type> {
        self.any_type.as_ref().unwrap().clone()
    }

    pub(super) fn wildcard_type(&self) -> Rc<Type> {
        self.wildcard_type.as_ref().unwrap().clone()
    }

    pub(super) fn error_type(&self) -> Rc<Type> {
        self.error_type.as_ref().unwrap().clone()
    }

    pub(super) fn unknown_type(&self) -> Rc<Type> {
        self.unknown_type.as_ref().unwrap().clone()
    }

    pub(super) fn undefined_type(&self) -> Rc<Type> {
        self.undefined_type.as_ref().unwrap().clone()
    }

    pub(super) fn optional_type(&self) -> Rc<Type> {
        self.optional_type.as_ref().unwrap().clone()
    }

    pub(super) fn null_type(&self) -> Rc<Type> {
        self.null_type.as_ref().unwrap().clone()
    }

    pub(super) fn string_type(&self) -> Rc<Type> {
        self.string_type.as_ref().unwrap().clone()
    }

    pub(super) fn number_type(&self) -> Rc<Type> {
        self.number_type.as_ref().unwrap().clone()
    }

    pub(super) fn bigint_type(&self) -> Rc<Type> {
        self.bigint_type.as_ref().unwrap().clone()
    }

    pub(super) fn true_type(&self) -> Rc<Type> {
        self.true_type.as_ref().unwrap().clone()
    }

    pub(super) fn regular_true_type(&self) -> Rc<Type> {
        self.regular_true_type.as_ref().unwrap().clone()
    }

    pub(super) fn false_type(&self) -> Rc<Type> {
        self.false_type.as_ref().unwrap().clone()
    }

    pub(super) fn regular_false_type(&self) -> Rc<Type> {
        self.regular_false_type.as_ref().unwrap().clone()
    }

    pub(super) fn boolean_type(&self) -> Rc<Type> {
        self.boolean_type.as_ref().unwrap().clone()
    }

    pub(super) fn es_symbol_type(&self) -> Rc<Type> {
        self.es_symbol_type.as_ref().unwrap().clone()
    }

    pub(super) fn void_type(&self) -> Rc<Type> {
        self.void_type.as_ref().unwrap().clone()
    }

    pub(super) fn never_type(&self) -> Rc<Type> {
        self.never_type.as_ref().unwrap().clone()
    }

    pub(super) fn silent_never_type(&self) -> Rc<Type> {
        self.silent_never_type.as_ref().unwrap().clone()
    }

    pub(super) fn string_number_symbol_type(&self) -> Rc<Type> {
        self.string_number_symbol_type.as_ref().unwrap().clone()
    }

    pub(super) fn keyof_constraint_type(&self) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn number_or_big_int_type(&self) -> Rc<Type> {
        self.number_or_big_int_type.as_ref().unwrap().clone()
    }

    pub(super) fn template_constraint_type(&self) -> Rc<Type> {
        self.template_constraint_type.as_ref().unwrap().clone()
    }

    pub(super) fn empty_type_literal_symbol(&self) -> Rc<Symbol> {
        self.empty_type_literal_symbol.as_ref().unwrap().clone()
    }

    pub(super) fn empty_generic_type(&self) -> Rc<Type> {
        self.empty_generic_type.as_ref().unwrap().clone()
    }

    pub(super) fn no_constraint_type(&self) -> Rc<Type> {
        self.no_constraint_type.as_ref().unwrap().clone()
    }

    pub(super) fn circular_constraint_type(&self) -> Rc<Type> {
        self.circular_constraint_type.as_ref().unwrap().clone()
    }

    pub(super) fn marker_super_type(&self) -> Rc<Type> {
        self.marker_super_type.as_ref().unwrap().clone()
    }

    pub(super) fn global_array_type(&self) -> Rc<Type> {
        self.global_array_type.as_ref().unwrap().clone()
    }

    pub(super) fn diagnostics(&self) -> RefMut<DiagnosticCollection> {
        self.diagnostics.borrow_mut()
    }

    pub(super) fn suggestion_diagnostics(&self) -> RefMut<DiagnosticCollection> {
        self.suggestion_diagnostics.borrow_mut()
    }

    pub(super) fn subtype_relation(&self) -> Ref<HashMap<String, RelationComparisonResult>> {
        self.subtype_relation.borrow()
    }

    pub(super) fn strict_subtype_relation(&self) -> Ref<HashMap<String, RelationComparisonResult>> {
        self.strict_subtype_relation.borrow()
    }

    pub(super) fn assignable_relation(&self) -> Ref<HashMap<String, RelationComparisonResult>> {
        self.assignable_relation.borrow()
    }

    pub(super) fn comparable_relation(&self) -> Ref<HashMap<String, RelationComparisonResult>> {
        self.comparable_relation.borrow()
    }

    pub(super) fn identity_relation(&self) -> Ref<HashMap<String, RelationComparisonResult>> {
        self.identity_relation.borrow()
    }

    pub(super) fn enum_relation(&self) -> Ref<HashMap<String, RelationComparisonResult>> {
        self.enum_relation.borrow()
    }
}

pub struct RelationCacheSizes {
    pub assignable: usize,
    pub identity: usize,
    pub subtype: usize,
    pub strict_subtype: usize,
}
