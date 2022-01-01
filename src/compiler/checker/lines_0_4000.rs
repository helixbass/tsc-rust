#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::ptr;
use std::rc::Rc;

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
    pub(crate) struct IntersectionState: u32 {
        const None = 0;
        const Source = 1 << 0;
        const Target = 1 << 1;
        const PropertyCheck = 1 << 2;
        const UnionIntersectionCheck = 1 << 3;
        const InPropertyCheck = 1 << 4;
    }
}

bitflags! {
    pub(crate) struct RecursionFlags: u32 {
        const None = 0;
        const Source = 1 << 0;
        const Target = 1 << 1;

        const Both = Self::Source.bits | Self::Target.bits;
    }
}

bitflags! {
    pub(crate) struct ExpandingFlags: u32 {
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

    pub(crate) fn globals(&self) -> RefMut<SymbolTable> {
        self.globals.borrow_mut()
    }

    pub(crate) fn string_literal_types(
        &self,
    ) -> RefMut<HashMap<String, Rc</*NumberLiteralType*/ Type>>> {
        self.string_literal_types.borrow_mut()
    }

    pub(crate) fn number_literal_types(
        &self,
    ) -> RefMut<HashMap<Number, Rc</*NumberLiteralType*/ Type>>> {
        self.number_literal_types.borrow_mut()
    }

    pub(crate) fn unknown_symbol(&self) -> Rc<Symbol> {
        self.unknown_symbol.as_ref().unwrap().clone()
    }

    pub(crate) fn number_type(&self) -> Rc<Type> {
        self.number_type.as_ref().unwrap().clone()
    }

    pub(crate) fn bigint_type(&self) -> Rc<Type> {
        self.bigint_type.as_ref().unwrap().clone()
    }

    pub(crate) fn true_type(&self) -> Rc<Type> {
        self.true_type.as_ref().unwrap().clone()
    }

    pub(crate) fn regular_true_type(&self) -> Rc<Type> {
        self.regular_true_type.as_ref().unwrap().clone()
    }

    pub(crate) fn false_type(&self) -> Rc<Type> {
        self.false_type.as_ref().unwrap().clone()
    }

    pub(crate) fn regular_false_type(&self) -> Rc<Type> {
        self.regular_false_type.as_ref().unwrap().clone()
    }

    pub(crate) fn boolean_type(&self) -> Rc<Type> {
        self.boolean_type.as_ref().unwrap().clone()
    }

    pub(crate) fn never_type(&self) -> Rc<Type> {
        self.never_type.as_ref().unwrap().clone()
    }

    pub(crate) fn number_or_big_int_type(&self) -> Rc<Type> {
        self.number_or_big_int_type.as_ref().unwrap().clone()
    }

    pub(crate) fn global_array_type(&self) -> Rc<Type> {
        self.global_array_type.as_ref().unwrap().clone()
    }

    pub(crate) fn diagnostics(&self) -> RefMut<DiagnosticCollection> {
        self.diagnostics.borrow_mut()
    }

    pub(crate) fn create_error<TNode: NodeInterface>(
        &self,
        location: Option<&TNode>,
        message: &DiagnosticMessage,
    ) -> Rc<Diagnostic> {
        if let Some(location) = location {
            Rc::new(create_diagnostic_for_node(location, message).into())
        } else {
            unimplemented!()
        }
    }

    pub(crate) fn error<TNode: NodeInterface>(
        &self,
        location: Option<&TNode>,
        message: &DiagnosticMessage,
    ) -> Rc<Diagnostic> {
        let diagnostic = self.create_error(location, message);
        self.diagnostics().add(diagnostic.clone());
        diagnostic
    }

    pub(crate) fn error_and_maybe_suggest_await<TNode: NodeInterface>(
        &self,
        location: &TNode,
        message: &DiagnosticMessage,
    ) -> Rc<Diagnostic> {
        let diagnostic = self.error(Some(location), message);
        diagnostic
    }

    pub(crate) fn create_symbol(&self, flags: SymbolFlags, name: __String) -> Symbol {
        let symbol = (self.Symbol)(flags | SymbolFlags::Transient, name);
        symbol
    }

    pub(crate) fn merge_symbol_table(
        &self,
        target: &mut SymbolTable,
        source: &SymbolTable,
        unidirectional: Option<bool>,
    ) {
        let unidirectional = unidirectional.unwrap_or(false);
        for (id, source_symbol) in source {
            let target_symbol = target.get(id);
            let value = if let Some(target_symbol) = target_symbol {
                unimplemented!()
            } else {
                source_symbol.clone()
            };
            target.insert(id.clone(), value);
        }
    }

    pub(crate) fn is_global_source_file(&self, node: &Node) -> bool {
        node.kind() == SyntaxKind::SourceFile && true
    }

    pub(crate) fn get_symbol(
        &self,
        symbols: &SymbolTable,
        name: &__String,
        meaning: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        if meaning != SymbolFlags::None {
            let symbol = self.get_merged_symbol(symbols.get(name).map(Clone::clone));
            if let Some(symbol) = symbol {
                if symbol.flags().intersects(meaning) {
                    return Some(symbol);
                }
            }
        }
        None
    }

    pub(crate) fn resolve_name<TLocation: NodeInterface, TNameArg: Into<ResolveNameNameArg>>(
        &self,
        location: Option<&TLocation>,
        name: &__String,
        meaning: SymbolFlags,
        name_not_found_message: Option<DiagnosticMessage>,
        name_arg: Option<TNameArg>,
        is_use: bool,
        exclude_globals: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let exclude_globals = exclude_globals.unwrap_or(false);
        self.resolve_name_helper(
            location,
            name,
            meaning,
            name_not_found_message,
            name_arg,
            is_use,
            exclude_globals,
            TypeChecker::get_symbol,
        )
    }

    pub(crate) fn resolve_name_helper<
        TLocation: NodeInterface,
        TNameArg: Into<ResolveNameNameArg>,
    >(
        &self,
        location: Option<&TLocation>,
        name: &__String,
        meaning: SymbolFlags,
        name_not_found_message: Option<DiagnosticMessage>,
        name_arg: Option<TNameArg>,
        is_use: bool,
        exclude_globals: bool,
        lookup: fn(&TypeChecker, &SymbolTable, &__String, SymbolFlags) -> Option<Rc<Symbol>>,
    ) -> Option<Rc<Symbol>> {
        let mut location: Option<Rc<Node>> = location.map(|node| node.node_wrapper());
        let mut result: Option<Rc<Symbol>> = None;
        let mut last_location: Option<Rc<Node>> = None;

        while let Some(location_unwrapped) = location {
            if location_unwrapped.maybe_locals().is_some()
                && !self.is_global_source_file(&*location_unwrapped)
            {
                unimplemented!()
            }
            last_location = Some(location_unwrapped.clone());
            location = location_unwrapped.maybe_parent();
        }

        if result.is_none() {
            if let Some(last_location) = last_location {
                Debug_.assert(last_location.kind() == SyntaxKind::SourceFile, None);
            }

            if !exclude_globals {
                result = lookup(self, &self.globals(), name, meaning);
            }
        }

        result
    }

    pub(crate) fn resolve_alias(&self, symbol: Rc<Symbol>) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(crate) fn resolve_entity_name(
        &self,
        name: &Node, /*EntityNameOrEntityNameExpression*/
        meaning: SymbolFlags,
        ignore_errors: Option<bool>,
        dont_resolve_alias: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let ignore_errors = ignore_errors.unwrap_or(false);
        let dont_resolve_alias = dont_resolve_alias.unwrap_or(false);
        if node_is_missing(name) {
            return None;
        }

        let symbol: Option<Rc<Symbol>>;
        match name {
            Node::Expression(Expression::Identifier(name)) => {
                let message = if false {
                    unimplemented!()
                } else {
                    self.get_cannot_find_name_diagnostic_for_name(&*get_first_identifier(name))
                };
                let symbol_from_js_prototype: Option<Rc<Symbol>> =
                    if false { unimplemented!() } else { None };
                symbol = self.get_merged_symbol(self.resolve_name(
                    Some(name),
                    &name.escaped_text,
                    meaning,
                    if ignore_errors || symbol_from_js_prototype.is_some() {
                        None
                    } else {
                        Some(message)
                    },
                    Some(name.node_wrapper()),
                    true,
                    Some(false),
                ));
                if symbol.is_none() {
                    unimplemented!()
                }
            }
            // else if name.kind() == SyntaxKind::QualifiedName
            //     || name.kind() == SyntaxKind::PropertyAccessExpression
            //     unimplemented!()
            _ => Debug_.assert_never(name, Some("Unknown entity name kind.")),
        }
        let symbol = symbol.unwrap();
        if symbol.flags().intersects(meaning) || dont_resolve_alias {
            Some(symbol)
        } else {
            Some(self.resolve_alias(symbol))
        }
    }

    pub(crate) fn get_merged_symbol(&self, symbol: Option<Rc<Symbol>>) -> Option<Rc<Symbol>> {
        symbol
    }

    pub(crate) fn get_symbol_of_node<TNode: NodeInterface>(
        &self,
        node: &TNode,
    ) -> Option<Rc<Symbol>> {
        self.get_merged_symbol(node.maybe_symbol())
    }
}

fn create_node_builder() -> NodeBuilder {
    NodeBuilder::new()
}

pub struct NodeBuilder {
    synthetic_factory: BaseNodeFactorySynthetic,
}

impl NodeBuilder {
    pub fn new() -> Self {
        Self {
            synthetic_factory: get_synthetic_factory(),
        }
    }

    pub fn type_to_type_node(
        &self,
        type_checker: &TypeChecker,
        type_: Rc<Type>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<TypeNode> {
        self.with_context(tracker, |context| {
            self.type_to_type_node_helper(type_checker, type_, context)
        })
    }

    pub fn symbol_to_expression(
        &self,
        type_checker: &TypeChecker,
        symbol: Rc<Symbol>,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Expression> {
        self.with_context(tracker, |context| {
            self._symbol_to_expression(type_checker, symbol, context, meaning)
        })
    }

    fn with_context<TReturn, TCallback: FnOnce(&NodeBuilderContext) -> TReturn>(
        &self,
        tracker: Option<&dyn SymbolTracker>,
        cb: TCallback,
    ) -> Option<TReturn> {
        let default_tracker: Option<DefaultNodeBuilderContextSymbolTracker> = if tracker.is_some() {
            None
        } else {
            Some(DefaultNodeBuilderContextSymbolTracker::new())
        };
        let context =
            NodeBuilderContext::new(tracker.unwrap_or_else(|| default_tracker.as_ref().unwrap()));
        let resulting_node = cb(&context);
        Some(resulting_node)
    }

    pub fn type_to_type_node_helper(
        &self,
        type_checker: &TypeChecker,
        type_: Rc<Type>,
        context: &NodeBuilderContext,
    ) -> TypeNode {
        if type_.flags().intersects(TypeFlags::Number) {
            return Into::<KeywordTypeNode>::into(
                factory
                    .create_keyword_type_node(&self.synthetic_factory, SyntaxKind::NumberKeyword),
            )
            .into();
        }
        if type_.flags().intersects(TypeFlags::Boolean) {
            return Into::<KeywordTypeNode>::into(
                factory
                    .create_keyword_type_node(&self.synthetic_factory, SyntaxKind::BooleanKeyword),
            )
            .into();
        }
        if type_.flags().intersects(TypeFlags::BooleanLiteral) {
            return factory
                .create_literal_type_node(
                    &self.synthetic_factory,
                    &*Into::<Rc<Node>>::into(
                        if type_.as_intrinsic_type().intrinsic_name() == "true" {
                            factory.create_true(&self.synthetic_factory)
                        } else {
                            factory.create_false(&self.synthetic_factory)
                        },
                    ),
                )
                .into();
        }

        let object_flags = get_object_flags(&*type_);

        if type_.flags().intersects(TypeFlags::TypeParameter)
            || object_flags.intersects(ObjectFlags::ClassOrInterface)
        {
            return if let Some(symbol) = type_.maybe_symbol() {
                self.symbol_to_type_node(type_checker, symbol, context, SymbolFlags::Type)
            } else {
                unimplemented!()
            };
        }
        if object_flags.intersects(ObjectFlags::Anonymous | ObjectFlags::Mapped) {
            Debug_.assert(type_.flags().intersects(TypeFlags::Object), None);
            return self.create_anonymous_type_node(type_checker, context, type_);
        }

        unimplemented!()
    }

    fn create_anonymous_type_node(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        type_: Rc<Type /*ObjectType*/>,
    ) -> TypeNode {
        let symbol = type_.maybe_symbol();
        if let Some(symbol) = symbol {
            if false {
                unimplemented!()
            } else {
                return self.visit_and_transform_type(
                    type_checker,
                    context,
                    type_,
                    NodeBuilder::create_type_node_from_object_type,
                );
            }
        } else {
            unimplemented!()
        }
    }

    fn visit_and_transform_type(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        type_: Rc<Type>,
        transform: fn(&NodeBuilder, &TypeChecker, &NodeBuilderContext, Rc<Type>) -> TypeNode,
    ) -> TypeNode {
        let result = transform(self, type_checker, context, type_);
        result
    }

    fn create_type_node_from_object_type(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        type_: Rc<Type /*ObjectType*/>,
    ) -> TypeNode {
        let resolved = type_checker.resolve_structured_type_members(type_);

        let members = self.create_type_nodes_from_resolved_type(type_checker, context, resolved);
        let type_literal_node = factory.create_type_literal_node(&self.synthetic_factory, members);
        type_literal_node.into()
    }

    fn create_type_nodes_from_resolved_type(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        resolved_type: Rc<Type /*ResolvedType*/>,
    ) -> Option<Vec<Rc<Node /*TypeElement*/>>> {
        let mut type_elements: Vec<Rc<Node>> = vec![];

        let properties = resolved_type.as_resolved_type().properties();

        for property_symbol in &*properties {
            self.add_property_to_element_list(
                type_checker,
                property_symbol.clone(),
                context,
                &mut type_elements,
            );
        }
        if !type_elements.is_empty() {
            Some(type_elements)
        } else {
            None
        }
    }

    fn add_property_to_element_list(
        &self,
        type_checker: &TypeChecker,
        property_symbol: Rc<Symbol>,
        context: &NodeBuilderContext,
        type_elements: &mut Vec<Rc<Node /*TypeElement*/>>,
    ) {
        let property_type = if false {
            unimplemented!()
        } else {
            type_checker.get_non_missing_type_of_symbol(property_symbol.clone())
        };
        let property_name =
            self.get_property_name_node_for_symbol(property_symbol.clone(), context);
        if false {
            unimplemented!()
        } else {
            let property_type_node: TypeNode;
            if false {
                unimplemented!()
            } else {
                property_type_node = if true {
                    self.serialize_type_for_declaration(
                        type_checker,
                        context,
                        property_type,
                        property_symbol,
                    )
                } else {
                    unimplemented!()
                };
            }

            let property_signature = factory.create_property_signature(
                &self.synthetic_factory,
                property_name,
                Some(property_type_node.into()),
            );

            type_elements.push(property_signature.into());
        }
    }

    fn lookup_symbol_chain(
        &self,
        symbol: Rc<Symbol>,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
    ) -> Vec<Rc<Symbol>> {
        self.lookup_symbol_chain_worker(symbol, context, meaning)
    }

    fn lookup_symbol_chain_worker(
        &self,
        symbol: Rc<Symbol>,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
    ) -> Vec<Rc<Symbol>> {
        let chain: Vec<Rc<Symbol>>;
        if false {
            unimplemented!()
        } else {
            chain = vec![symbol];
        }
        chain
    }

    fn symbol_to_type_node(
        &self,
        type_checker: &TypeChecker,
        symbol: Rc<Symbol>,
        context: &NodeBuilderContext,
        meaning: SymbolFlags,
    ) -> TypeNode {
        let chain = self.lookup_symbol_chain(symbol, context, Some(meaning));

        let chain_index = chain.len() - 1;
        let entity_name =
            self.create_access_from_symbol_chain(type_checker, context, chain, chain_index, 0);
        if false {
            unimplemented!()
        } else {
            factory
                .create_type_reference_node(&self.synthetic_factory, entity_name)
                .into()
        }
    }

    fn create_access_from_symbol_chain(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        chain: Vec<Rc<Symbol>>,
        index: usize,
        stopper: usize,
    ) -> Rc<Node> {
        let symbol = chain[index].clone();

        let mut symbol_name: Option<String>;
        if index == 0 {
            symbol_name =
                Some(type_checker.get_name_of_symbol_as_written(symbol.clone(), Some(context)));
        } else {
            unimplemented!()
        }
        if symbol_name.is_none() {
            symbol_name =
                Some(type_checker.get_name_of_symbol_as_written(symbol.clone(), Some(context)));
        }
        let symbol_name = symbol_name.unwrap();

        let identifier = factory.create_identifier(&self.synthetic_factory, &symbol_name);
        identifier.set_symbol(symbol);

        identifier.into()
    }

    fn _symbol_to_expression(
        &self,
        type_checker: &TypeChecker,
        symbol: Rc<Symbol>,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
    ) -> Expression {
        let chain = self.lookup_symbol_chain(symbol, context, meaning);
        let index = chain.len() - 1;
        self.create_expression_from_symbol_chain(type_checker, context, chain, index)
    }

    fn get_property_name_node_for_symbol(
        &self,
        symbol: Rc<Symbol>,
        context: &NodeBuilderContext,
    ) -> Rc<Node> {
        let single_quote = false;
        let string_named = false;
        let raw_name = unescape_leading_underscores(&symbol.escaped_name);
        self.create_property_name_node_for_identifier_or_literal(
            raw_name,
            Some(string_named),
            Some(single_quote),
        )
    }

    fn create_property_name_node_for_identifier_or_literal(
        &self,
        name: String,
        string_named: Option<bool>,
        single_quote: Option<bool>,
    ) -> Rc<Node> {
        if is_identifier_text(&name) {
            factory.create_identifier(&self.synthetic_factory, &name)
        } else {
            unimplemented!()
        }
        .into()
    }

    fn create_expression_from_symbol_chain(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        chain: Vec<Rc<Symbol>>,
        index: usize,
    ) -> Expression {
        let symbol = (&chain)[index].clone();

        let symbol_name = type_checker.get_name_of_symbol_as_written(symbol.clone(), Some(context));

        if index == 0 || false {
            let identifier = factory.create_identifier(&self.synthetic_factory, &symbol_name);
            identifier.set_symbol(symbol);
            return identifier.into();
        } else {
            unimplemented!()
        }
    }

    fn serialize_type_for_declaration(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        type_: Rc<Type>,
        symbol: Rc<Symbol>,
    ) -> TypeNode {
        let result = self.type_to_type_node_helper(type_checker, type_, context);
        result
    }
}

struct DefaultNodeBuilderContextSymbolTracker {}

impl DefaultNodeBuilderContextSymbolTracker {
    pub fn new() -> Self {
        Self {}
    }
}

impl SymbolTracker for DefaultNodeBuilderContextSymbolTracker {}

pub struct NodeBuilderContext<'symbol_tracker> {
    tracker: &'symbol_tracker dyn SymbolTracker,
}

impl<'symbol_tracker> NodeBuilderContext<'symbol_tracker> {
    pub fn new(tracker: &'symbol_tracker dyn SymbolTracker) -> Self {
        Self { tracker }
    }
}

pub(crate) enum ResolveNameNameArg {
    Node(Rc<Node>),
    __String(__String),
}

impl From<Rc<Node>> for ResolveNameNameArg {
    fn from(node: Rc<Node>) -> Self {
        ResolveNameNameArg::Node(node)
    }
}

impl From<__String> for ResolveNameNameArg {
    fn from(string: __String) -> Self {
        ResolveNameNameArg::__String(string)
    }
}
