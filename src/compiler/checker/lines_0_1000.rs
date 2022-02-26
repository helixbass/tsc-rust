#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use regex::Regex;
use std::array::IntoIter;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::iter::FromIterator;
use std::rc::Rc;

use super::{create_node_builder, is_not_accessor, is_not_overload};
use crate::{
    get_allow_synthetic_default_imports, get_emit_module_kind, get_emit_script_target,
    get_module_instance_state, get_strict_option_value, get_use_define_for_class_fields,
    BaseInterfaceType, Extension, GenericableTypeInterface, ModuleInstanceState, Signature,
    TypeCheckerHostDebuggable, VarianceFlags, __String, create_diagnostic_collection,
    create_symbol_table, object_allocator, DiagnosticCollection, DiagnosticMessage,
    FreshableIntrinsicType, Node, NodeId, NodeInterface, Number, ObjectFlags, Symbol, SymbolFlags,
    SymbolId, SymbolInterface, SymbolTable, Type, TypeChecker, TypeFlags,
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

pub(super) trait IterationTypesResolver {
    fn iterable_cache_key(&self) -> &str /*"iterationTypesOfAsyncIterable" | "iterationTypesOfIterable"*/;
    fn iterator_cache_key(&self) -> &str /*"iterationTypesOfAsyncIterator" | "iterationTypesOfIterator"*/;
    fn iterator_symbol_name(&self) -> &str /*"asyncIterator" | "iterator"*/;
    fn get_global_iterator_type(&self, report_errors: bool) -> Rc<Type /*GenericType*/>;
    fn get_global_iterable_type(&self, report_errors: bool) -> Rc<Type /*GenericType*/>;
    fn get_global_iterable_iterator_type(&self, report_errors: bool) -> Rc<Type /*GenericType*/>;
    fn get_global_generator_type(&self, report_errors: bool) -> Rc<Type /*GenericType*/>;
    fn resolve_iteration_type(
        &self,
        type_: &Type,
        error_node: Option<Rc<Node>>,
    ) -> Option<Rc<Type>>;
    fn must_have_a_next_method_diagnostic(&self) -> &'static DiagnosticMessage;
    fn must_be_a_method_diagnostic(&self) -> &'static DiagnosticMessage;
    fn must_have_a_value_diagnostic(&self) -> &'static DiagnosticMessage;
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

        globals: RefCell::new(create_symbol_table(None)),

        string_literal_types: RefCell::new(HashMap::new()),
        number_literal_types: RefCell::new(HashMap::new()),
        big_int_literal_types: RefCell::new(HashMap::new()),

        unknown_symbol: None,

        any_type: None,
        error_type: None,
        unknown_type: None,
        undefined_type: None,
        null_type: None,
        string_type: None,
        number_type: None,
        bigint_type: None,
        true_type: None,
        regular_true_type: None,
        false_type: None,
        regular_false_type: None,
        boolean_type: None,
        void_type: None,
        never_type: None,
        number_or_big_int_type: None,
        template_constraint_type: None,

        empty_generic_type: None,

        no_constraint_type: None,
        circular_constraint_type: None,

        global_array_type: None,

        deferred_global_promise_type: RefCell::new(None),
        deferred_global_promise_constructor_symbol: RefCell::new(None),

        symbol_links: RefCell::new(HashMap::new()),
        node_links: RefCell::new(HashMap::new()),

        diagnostics: RefCell::new(create_diagnostic_collection()),

        assignable_relation: HashMap::new(),
        comparable_relation: HashMap::new(),
    };
    type_checker.emit_resolver = Some(type_checker.create_resolver());
    type_checker.unknown_symbol = Some(
        type_checker
            .create_symbol(
                SymbolFlags::Property,
                __String::new("unknown".to_string()),
                None,
            )
            .into(),
    );
    type_checker.any_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Any, "any")
            .into(),
    );
    type_checker.error_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Any, "error")
            .into(),
    );
    type_checker.unknown_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Unknown, "unknown")
            .into(),
    );
    type_checker.undefined_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Undefined, "undefined")
            .into(),
    );
    type_checker.null_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Null, "null")
            .into(),
    );
    type_checker.string_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::String, "string")
            .into(),
    );
    type_checker.number_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Number, "number")
            .into(),
    );
    type_checker.bigint_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::BigInt, "bigint")
            .into(),
    );
    let true_type: Rc<Type> = FreshableIntrinsicType::new(
        type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "true"),
    )
    .into();
    let regular_true_type: Rc<Type> = FreshableIntrinsicType::new(
        type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "true"),
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
    let false_type: Rc<Type> = FreshableIntrinsicType::new(
        type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "false"),
    )
    .into();
    let regular_false_type: Rc<Type> = FreshableIntrinsicType::new(
        type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "false"),
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
    type_checker.boolean_type = Some(type_checker.get_union_type(
        vec![
            type_checker.regular_false_type(),
            type_checker.regular_true_type(),
        ],
        None,
    ));
    type_checker.void_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Void, "void")
            .into(),
    );
    type_checker.never_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Never, "never")
            .into(),
    );
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
    let empty_generic_type = type_checker.create_anonymous_type(
        Option::<&Symbol>::None,
        type_checker.empty_symbols(),
        vec![],
        vec![],
    );
    let empty_generic_type = BaseInterfaceType::new(empty_generic_type, None, None, None, None);
    empty_generic_type.genericize(HashMap::new());
    type_checker.empty_generic_type = Some(empty_generic_type.into());

    type_checker.no_constraint_type = Some(
        type_checker
            .create_anonymous_type(
                Option::<&Symbol>::None,
                type_checker.empty_symbols(),
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
            )
            .into(),
    );
    type_checker.initialize_type_checker();
    type_checker
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

    pub(super) fn type_count(&self) -> u32 {
        self.type_count.get()
    }

    pub(super) fn increment_type_count(&self) {
        self.type_count.set(self.type_count.get() + 1);
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

    pub(super) fn error_type(&self) -> Rc<Type> {
        self.error_type.as_ref().unwrap().clone()
    }

    pub(super) fn unknown_type(&self) -> Rc<Type> {
        self.unknown_type.as_ref().unwrap().clone()
    }

    pub(super) fn undefined_type(&self) -> Rc<Type> {
        self.undefined_type.as_ref().unwrap().clone()
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

    pub(super) fn void_type(&self) -> Rc<Type> {
        self.void_type.as_ref().unwrap().clone()
    }

    pub(super) fn never_type(&self) -> Rc<Type> {
        self.never_type.as_ref().unwrap().clone()
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

    pub(super) fn empty_generic_type(&self) -> Rc<Type> {
        self.empty_generic_type.as_ref().unwrap().clone()
    }

    pub(super) fn no_constraint_type(&self) -> Rc<Type> {
        self.no_constraint_type.as_ref().unwrap().clone()
    }

    pub(super) fn circular_constraint_type(&self) -> Rc<Type> {
        self.circular_constraint_type.as_ref().unwrap().clone()
    }

    pub(super) fn global_array_type(&self) -> Rc<Type> {
        self.global_array_type.as_ref().unwrap().clone()
    }

    pub(super) fn diagnostics(&self) -> RefMut<DiagnosticCollection> {
        self.diagnostics.borrow_mut()
    }
}
