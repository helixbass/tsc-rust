#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::ptr;
use std::rc::Rc;

use super::create_node_builder;
use crate::{
    TypeNode, UnionOrIntersectionType, UnionOrIntersectionTypeInterface, UnionType,
    VariableDeclaration, VariableStatement, __String, bind_source_file, chain_diagnostic_messages,
    create_diagnostic_collection, create_diagnostic_for_node,
    create_diagnostic_for_node_from_message_chain, create_printer, create_symbol_table,
    create_text_writer, declaration_name_to_string, escape_leading_underscores, every, factory,
    first_defined, first_or_undefined, for_each, get_effective_initializer,
    get_effective_type_annotation_node, get_first_identifier, get_name_of_declaration,
    get_object_flags, get_source_file_of_node, get_synthetic_factory, has_dynamic_name,
    has_initializer, is_binding_element, is_external_or_common_js_module, is_identifier_text,
    is_object_literal_expression, is_private_identifier, is_property_assignment,
    is_property_declaration, is_property_signature, is_variable_declaration, node_is_missing,
    object_allocator, unescape_leading_underscores, using_single_line_string_writer, ArrayTypeNode,
    BaseInterfaceType, BaseIntrinsicType, BaseLiteralType, BaseNodeFactorySynthetic,
    BaseObjectType, BaseType, BaseUnionOrIntersectionType, CharacterCodes, Debug_, Diagnostic,
    DiagnosticCollection, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, EmitHint,
    EmitTextWriter, Expression, ExpressionStatement, FreshableIntrinsicType,
    HasExpressionInitializerInterface, InterfaceDeclaration, InterfaceType, IntrinsicType,
    KeywordTypeNode, LiteralLikeNode, LiteralLikeNodeInterface, LiteralType, LiteralTypeInterface,
    NamedDeclarationInterface, Node, NodeInterface, Number, NumberLiteralType, NumericLiteral,
    ObjectFlags, ObjectFlagsTypeInterface, ObjectLiteralExpression, PrefixUnaryExpression,
    PrinterOptions, PropertyAssignment, PropertySignature, RelationComparisonResult,
    ResolvableTypeInterface, ResolvedTypeInterface, SourceFile, Statement, StringLiteralType,
    Symbol, SymbolFlags, SymbolFormatFlags, SymbolTable, SymbolTracker, SyntaxKind, Ternary, Type,
    TypeChecker, TypeCheckerHost, TypeElement, TypeFlags, TypeInterface, TypeReferenceNode,
    UnionReduction, VariableLikeDeclarationInterface,
};

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

pub fn create_type_checker<TTypeCheckerHost: TypeCheckerHost>(
    host: &TTypeCheckerHost,
    produce_diagnostics: bool,
) -> TypeChecker {
    let mut type_checker = TypeChecker {
        _types_needing_strong_references: RefCell::new(vec![]),
        Symbol: object_allocator.get_symbol_constructor(),
        Type: object_allocator.get_type_constructor(),

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

        diagnostics: RefCell::new(create_diagnostic_collection()),

        assignable_relation: HashMap::new(),
    };
    type_checker.unknown_symbol = Some(Rc::new(
        type_checker.create_symbol(SymbolFlags::Property, __String::new("unknown".to_string())),
    ));
    type_checker.number_type = Some(Rc::new(
        type_checker
            .create_intrinsic_type(TypeFlags::Number, "number")
            .into(),
    ));
    type_checker.bigint_type = Some(Rc::new(
        type_checker
            .create_intrinsic_type(TypeFlags::BigInt, "bigint")
            .into(),
    ));
    let true_type: Rc<Type> = Rc::new(
        FreshableIntrinsicType::new(
            type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "true"),
        )
        .into(),
    );
    let regular_true_type: Rc<Type> = Rc::new(
        FreshableIntrinsicType::new(
            type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "true"),
        )
        .into(),
    );
    match &*true_type {
        Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
            IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                freshable_intrinsic_type
                    .regular_type
                    .init(&regular_true_type, true);
                freshable_intrinsic_type.fresh_type.init(&true_type, true);
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
    let false_type: Rc<Type> = Rc::new(
        FreshableIntrinsicType::new(
            type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "false"),
        )
        .into(),
    );
    let regular_false_type: Rc<Type> = Rc::new(
        FreshableIntrinsicType::new(
            type_checker.create_intrinsic_type(TypeFlags::BooleanLiteral, "false"),
        )
        .into(),
    );
    match &*false_type {
        Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
            IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                freshable_intrinsic_type
                    .regular_type
                    .init(&regular_false_type, false);
                freshable_intrinsic_type.fresh_type.init(&false_type, true);
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
    type_checker.never_type = Some(Rc::new(
        type_checker
            .create_intrinsic_type(TypeFlags::Never, "never")
            .into(),
    ));
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
