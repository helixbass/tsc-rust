#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Cell, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

use super::create_node_builder;
use crate::{
    __String, create_diagnostic_collection, create_symbol_table, object_allocator,
    DiagnosticCollection, FreshableIntrinsicType, IntrinsicType, NodeId, NodeInterface, Number,
    ObjectFlags, Symbol, SymbolFlags, SymbolId, SymbolInterface, SymbolTable, Type, TypeChecker,
    TypeCheckerHost, TypeFlags,
};

thread_local! {
    pub(super) static next_symbol_id: RefCell<SymbolId> = RefCell::new(1);
}

pub(super) fn get_next_symbol_id() -> SymbolId {
    next_symbol_id.with(|_next_symbol_id| *_next_symbol_id.borrow())
}

pub(super) fn increment_next_symbol_id() {
    next_symbol_id.with(|_next_symbol_id| {
        *_next_symbol_id.borrow_mut() += 1;
    });
}

thread_local! {
    pub(super) static next_node_id: RefCell<NodeId> = RefCell::new(1);
}

pub(super) fn get_next_node_id() -> NodeId {
    next_node_id.with(|_next_node_id| *_next_node_id.borrow())
}

pub(super) fn increment_next_node_id() {
    next_node_id.with(|_next_node_id| {
        *_next_node_id.borrow_mut() += 1;
    });
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
    pub(super) struct ExpandingFlags: u32 {
        const None = 0;
        const Source = 1;
        const Target = 1 << 1;

        const Both = Self::Source.bits | Self::Target.bits;
    }
}

pub(super) fn get_symbol_id(symbol: &Symbol) -> SymbolId {
    if symbol.maybe_id().is_none() {
        symbol.set_id(get_next_symbol_id());
        increment_next_symbol_id();
    }

    symbol.id()
}

pub(super) fn get_node_id<TNode: NodeInterface>(node: &TNode) -> NodeId {
    if node.maybe_id().is_none() {
        node.set_id(get_next_node_id());
        increment_next_node_id();
    }

    node.id()
}

pub fn create_type_checker<TTypeCheckerHost: TypeCheckerHost>(
    host: &TTypeCheckerHost,
    produce_diagnostics: bool,
) -> TypeChecker {
    let mut type_checker = TypeChecker {
        _types_needing_strong_references: RefCell::new(vec![]),
        Symbol: object_allocator.get_symbol_constructor(),
        Type: object_allocator.get_type_constructor(),

        type_count: Cell::new(0),

        empty_symbols: Rc::new(RefCell::new(create_symbol_table())),

        strict_null_checks: true,
        fresh_object_literal_flag: if false {
            unimplemented!()
        } else {
            ObjectFlags::FreshLiteral
        },
        exact_optional_property_types: false,

        node_builder: create_node_builder(),

        globals: RefCell::new(create_symbol_table()),

        string_literal_types: RefCell::new(HashMap::new()),
        number_literal_types: RefCell::new(HashMap::new()),

        unknown_symbol: None,

        any_type: None,
        number_type: None,
        bigint_type: None,
        true_type: None,
        regular_true_type: None,
        false_type: None,
        regular_false_type: None,
        boolean_type: None,
        never_type: None,
        number_or_big_int_type: None,

        global_array_type: None,

        symbol_links: RefCell::new(HashMap::new()),
        node_links: RefCell::new(HashMap::new()),

        diagnostics: RefCell::new(create_diagnostic_collection()),

        assignable_relation: HashMap::new(),
    };
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
    match &*true_type {
        Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
            IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                freshable_intrinsic_type
                    .regular_type
                    .init(&regular_true_type, false);
                freshable_intrinsic_type.fresh_type.init(&true_type, false);
            }
            _ => panic!("Expected FreshableIntrinsicType"),
        },
        _ => panic!("Expected IntrinsicType"),
    }
    type_checker.true_type = Some(true_type);
    match &*regular_true_type {
        Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
            IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                freshable_intrinsic_type
                    .regular_type
                    .init(&regular_true_type, false);
                freshable_intrinsic_type
                    .fresh_type
                    .init(type_checker.true_type.as_ref().unwrap(), false);
            }
            _ => panic!("Expected FreshableIntrinsicType"),
        },
        _ => panic!("Expected IntrinsicType"),
    }
    type_checker.regular_true_type = Some(regular_true_type);
    let false_type: Rc<Type> = FreshableIntrinsicType::new(
        type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "false"),
    )
    .into();
    let regular_false_type: Rc<Type> = FreshableIntrinsicType::new(
        type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "false"),
    )
    .into();
    match &*false_type {
        Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
            IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                freshable_intrinsic_type
                    .regular_type
                    .init(&regular_false_type, false);
                freshable_intrinsic_type.fresh_type.init(&false_type, false);
            }
            _ => panic!("Expected FreshableIntrinsicType"),
        },
        _ => panic!("Expected IntrinsicType"),
    }
    type_checker.false_type = Some(false_type);
    match &*regular_false_type {
        Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
            IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                freshable_intrinsic_type
                    .regular_type
                    .init(&regular_false_type, false);
                freshable_intrinsic_type
                    .fresh_type
                    .init(type_checker.false_type.as_ref().unwrap(), false);
            }
            _ => panic!("Expected FreshableIntrinsicType"),
        },
        _ => panic!("Expected IntrinsicType"),
    }
    type_checker.regular_false_type = Some(regular_false_type);
    type_checker.boolean_type = Some(type_checker.get_union_type(
        vec![
            type_checker.regular_false_type(),
            type_checker.regular_true_type(),
        ],
        None,
    ));
    type_checker.never_type = Some(
        type_checker
            .create_intrinsic_type(TypeFlags::Never, "never")
            .into(),
    );
    type_checker.number_or_big_int_type = Some(type_checker.get_union_type(
        vec![type_checker.number_type(), type_checker.bigint_type()],
        None,
    ));
    type_checker.initialize_type_checker(host);
    type_checker
}

impl TypeChecker {
    pub fn keep_strong_reference_to_type(&self, type_: Rc<Type>) {
        self._types_needing_strong_references
            .borrow_mut()
            .push(type_);
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

    pub(super) fn unknown_symbol(&self) -> Rc<Symbol> {
        self.unknown_symbol.as_ref().unwrap().clone()
    }

    pub(super) fn any_type(&self) -> Rc<Type> {
        self.any_type.as_ref().unwrap().clone()
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

    pub(super) fn never_type(&self) -> Rc<Type> {
        self.never_type.as_ref().unwrap().clone()
    }

    pub(super) fn number_or_big_int_type(&self) -> Rc<Type> {
        self.number_or_big_int_type.as_ref().unwrap().clone()
    }

    pub(super) fn global_array_type(&self) -> Rc<Type> {
        self.global_array_type.as_ref().unwrap().clone()
    }

    pub(super) fn diagnostics(&self) -> RefMut<DiagnosticCollection> {
        self.diagnostics.borrow_mut()
    }
}
