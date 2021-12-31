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
    struct IntersectionState: u32 {
        const None = 0;
        const Source = 1 << 0;
        const Target = 1 << 1;
        const PropertyCheck = 1 << 2;
        const UnionIntersectionCheck = 1 << 3;
        const InPropertyCheck = 1 << 4;
    }
}

bitflags! {
    struct RecursionFlags: u32 {
        const None = 0;
        const Source = 1 << 0;
        const Target = 1 << 1;

        const Both = Self::Source.bits | Self::Target.bits;
    }
}

bitflags! {
    struct ExpandingFlags: u32 {
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

    fn globals(&self) -> RefMut<SymbolTable> {
        self.globals.borrow_mut()
    }

    fn string_literal_types(&self) -> RefMut<HashMap<String, Rc</*NumberLiteralType*/ Type>>> {
        self.string_literal_types.borrow_mut()
    }

    fn number_literal_types(&self) -> RefMut<HashMap<Number, Rc</*NumberLiteralType*/ Type>>> {
        self.number_literal_types.borrow_mut()
    }

    fn unknown_symbol(&self) -> Rc<Symbol> {
        self.unknown_symbol.as_ref().unwrap().clone()
    }

    fn number_type(&self) -> Rc<Type> {
        self.number_type.as_ref().unwrap().clone()
    }

    fn bigint_type(&self) -> Rc<Type> {
        self.bigint_type.as_ref().unwrap().clone()
    }

    fn true_type(&self) -> Rc<Type> {
        self.true_type.as_ref().unwrap().clone()
    }

    fn regular_true_type(&self) -> Rc<Type> {
        self.regular_true_type.as_ref().unwrap().clone()
    }

    fn false_type(&self) -> Rc<Type> {
        self.false_type.as_ref().unwrap().clone()
    }

    fn regular_false_type(&self) -> Rc<Type> {
        self.regular_false_type.as_ref().unwrap().clone()
    }

    fn boolean_type(&self) -> Rc<Type> {
        self.boolean_type.as_ref().unwrap().clone()
    }

    fn never_type(&self) -> Rc<Type> {
        self.never_type.as_ref().unwrap().clone()
    }

    fn number_or_big_int_type(&self) -> Rc<Type> {
        self.number_or_big_int_type.as_ref().unwrap().clone()
    }

    fn global_array_type(&self) -> Rc<Type> {
        self.global_array_type.as_ref().unwrap().clone()
    }

    fn diagnostics(&self) -> RefMut<DiagnosticCollection> {
        self.diagnostics.borrow_mut()
    }

    fn create_error<TNode: NodeInterface>(
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

    fn error<TNode: NodeInterface>(
        &self,
        location: Option<&TNode>,
        message: &DiagnosticMessage,
    ) -> Rc<Diagnostic> {
        let diagnostic = self.create_error(location, message);
        self.diagnostics().add(diagnostic.clone());
        diagnostic
    }

    fn error_and_maybe_suggest_await<TNode: NodeInterface>(
        &self,
        location: &TNode,
        message: &DiagnosticMessage,
    ) -> Rc<Diagnostic> {
        let diagnostic = self.error(Some(location), message);
        diagnostic
    }

    fn create_symbol(&self, flags: SymbolFlags, name: __String) -> Symbol {
        let symbol = (self.Symbol)(flags | SymbolFlags::Transient, name);
        symbol
    }

    fn merge_symbol_table(
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

    fn is_global_source_file(&self, node: &Node) -> bool {
        node.kind() == SyntaxKind::SourceFile && true
    }

    fn get_symbol(
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

    fn resolve_name<TLocation: NodeInterface, TNameArg: Into<ResolveNameNameArg>>(
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

    fn resolve_name_helper<TLocation: NodeInterface, TNameArg: Into<ResolveNameNameArg>>(
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

    fn resolve_alias(&self, symbol: Rc<Symbol>) -> Rc<Symbol> {
        unimplemented!()
    }

    fn resolve_entity_name(
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

    fn get_merged_symbol(&self, symbol: Option<Rc<Symbol>>) -> Option<Rc<Symbol>> {
        symbol
    }

    fn get_symbol_of_node<TNode: NodeInterface>(&self, node: &TNode) -> Option<Rc<Symbol>> {
        self.get_merged_symbol(node.maybe_symbol())
    }

    fn symbol_is_value(&self, symbol: Rc<Symbol>) -> bool {
        symbol.flags().intersects(SymbolFlags::Value)
    }

    fn create_type(&self, flags: TypeFlags) -> BaseType {
        let result = (self.Type)(flags);
        result
    }

    fn create_intrinsic_type(&self, kind: TypeFlags, intrinsic_name: &str) -> BaseIntrinsicType {
        let type_ = self.create_type(kind);
        let type_ = BaseIntrinsicType::new(type_, intrinsic_name.to_string());
        type_
    }

    fn create_object_type(&self, object_flags: ObjectFlags, symbol: Rc<Symbol>) -> BaseObjectType {
        let mut type_ = self.create_type(TypeFlags::Object);
        type_.set_symbol(symbol);
        let type_ = BaseObjectType::new(type_, object_flags);
        type_
    }

    fn is_reserved_member_name(&self, name: &__String) -> bool {
        let mut chars = name.chars();
        let mut current_char: Option<char> = chars.next();
        if let Some(current_char) = current_char {
            if current_char != CharacterCodes::underscore {
                return false;
            }
        } else {
            return false;
        }
        current_char = chars.next();
        if let Some(current_char) = current_char {
            if current_char != CharacterCodes::underscore {
                return false;
            }
        } else {
            return false;
        }
        current_char = chars.next();
        if let Some(current_char) = current_char {
            if current_char == CharacterCodes::underscore
                || current_char == CharacterCodes::at
                || current_char == CharacterCodes::hash
            {
                return false;
            }
        } else {
            return false;
        }
        true
    }

    fn get_named_members(&self, members: &SymbolTable) -> Vec<Rc<Symbol>> {
        members
            .iter()
            .filter(|(id, symbol)| self.is_named_member((*symbol).clone(), id))
            .map(|(_, symbol)| symbol.clone())
            .collect()
    }

    fn is_named_member(&self, member: Rc<Symbol>, escaped_name: &__String) -> bool {
        !self.is_reserved_member_name(escaped_name) && self.symbol_is_value(member)
    }

    fn set_structured_type_members<TType: ResolvableTypeInterface + ResolvedTypeInterface>(
        &self,
        type_: &TType,
        members: Rc<RefCell<SymbolTable>>,
    ) /*-> BaseObjectType*/
    {
        type_.resolve(members.clone(), vec![]);
        if true {
            type_.set_properties(self.get_named_members(&*(*members).borrow()));
        }
        // type_
    }

    fn create_anonymous_type(
        &self,
        symbol: Rc<Symbol>,
        members: Rc<RefCell<SymbolTable>>,
    ) -> BaseObjectType {
        let type_ = self.create_object_type(ObjectFlags::Anonymous, symbol);
        self.set_structured_type_members(&type_, members);
        type_
    }

    fn symbol_to_string(
        &self,
        symbol: Rc<Symbol>,
        enclosing_declaration: Option<&Node>,
        meaning: Option<SymbolFlags>,
        flags: Option<SymbolFormatFlags>,
        writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
    ) -> String {
        let flags = flags.unwrap_or(SymbolFormatFlags::AllowAnyNodeKind);
        let builder = if flags.intersects(SymbolFormatFlags::AllowAnyNodeKind) {
            NodeBuilder::symbol_to_expression
        } else {
            unimplemented!()
        };
        let symbol_to_string_worker = |writer: Rc<RefCell<dyn EmitTextWriter>>| {
            let entity = builder(
                &self.node_builder,
                self,
                symbol,
                // meaning.unwrap() TODO: this is ! in the Typescript code but would be undefined at runtime when called from propertyRelatedTo()?
                meaning,
                None,
            )
            .unwrap();
            let entity: Rc<Node> = entity.into();
            let mut printer = if false {
                unimplemented!()
            } else {
                create_printer(PrinterOptions {/*remove_comments: true*/})
            };
            let source_file = if let Some(enclosing_declaration) = enclosing_declaration {
                Some(get_source_file_of_node(enclosing_declaration))
            } else {
                None
            };
            printer.write_node(EmitHint::Unspecified, &*entity, source_file, writer);
            // writer
        };
        if let Some(writer) = writer {
            unimplemented!()
        } else {
            using_single_line_string_writer(symbol_to_string_worker)
        }
    }

    fn type_to_string(&self, type_: Rc<Type>) -> String {
        let writer = Rc::new(RefCell::new(create_text_writer("")));
        let type_node =
            self.node_builder
                .type_to_type_node(self, type_, Some(&*(*writer).borrow()));
        let type_node: Rc<Node> = match type_node {
            None => Debug_.fail(Some("should always get typenode")),
            Some(type_node) => type_node.into(),
        };
        let options = PrinterOptions {};
        let mut printer = create_printer(options);
        let source_file: Option<Rc<SourceFile>> = if false { unimplemented!() } else { None };
        printer.write_node(
            EmitHint::Unspecified,
            &*type_node,
            source_file,
            writer.clone(),
        );
        let result = (*writer).borrow().get_text();

        result
    }

    fn get_type_names_for_error_display(
        &self,
        left: Rc<Type>,
        right: Rc<Type>,
    ) -> (String, String) {
        let left_str = if false {
            unimplemented!()
        } else {
            self.type_to_string(left)
        };
        let right_str = if false {
            unimplemented!()
        } else {
            self.type_to_string(right)
        };
        (left_str, right_str)
    }

    fn get_type_name_for_error_display(&self, type_: Rc<Type>) -> String {
        self.type_to_string(type_)
    }

    fn get_name_of_symbol_as_written(
        &self,
        symbol: Rc<Symbol>,
        context: Option<&NodeBuilderContext>,
    ) -> String {
        if let Some(declarations) = &*symbol.maybe_declarations() {
            if !declarations.is_empty() {
                let declaration = first_defined(declarations, |d, _| {
                    if get_name_of_declaration(&**d).is_some() {
                        Some(d)
                    } else {
                        None
                    }
                });
                let name = if let Some(declaration) = declaration {
                    get_name_of_declaration(&**declaration)
                } else {
                    None
                };
                if let Some(name) = name {
                    return declaration_name_to_string(Some(&*name));
                }
            }
        }
        unimplemented!()
    }

    fn add_optionality(
        &self,
        type_: Rc<Type>,
        is_property: Option<bool>,
        is_optional: Option<bool>,
    ) -> Rc<Type> {
        let is_property = is_property.unwrap_or(false);
        let is_optional = is_optional.unwrap_or(true);
        if self.strict_null_checks && is_optional {
            self.get_optional_type(type_, Some(is_property))
        } else {
            type_
        }
    }

    fn get_type_for_variable_like_declaration(&self, declaration: &Node) -> Option<Rc<Type>> {
        let is_property =
            is_property_declaration(declaration) || is_property_signature(declaration);
        let is_optional = false;

        let declared_type = self.try_get_type_from_effective_type_node(declaration);
        if let Some(declared_type) = declared_type {
            return Some(self.add_optionality(declared_type, Some(is_property), Some(is_optional)));
        }
        unimplemented!()
    }

    fn get_widened_type_for_variable_like_declaration(&self, declaration: &Node) -> Rc<Type> {
        self.widen_type_for_variable_like_declaration(
            self.get_type_for_variable_like_declaration(declaration),
            declaration,
        )
    }

    fn widen_type_for_variable_like_declaration(
        &self,
        type_: Option<Rc<Type>>,
        declaration: &Node,
    ) -> Rc<Type> {
        if let Some(type_) = type_ {
            return self.get_widened_type(type_);
        }
        unimplemented!()
    }

    fn try_get_type_from_effective_type_node(
        &self,
        declaration: &Node, /*Declaration*/
    ) -> Option<Rc<Type>> {
        let type_node = get_effective_type_annotation_node(declaration);
        type_node.map(|type_node| self.get_type_from_type_node(&*type_node))
    }

    fn get_type_of_variable_or_parameter_or_property(&self, symbol: &Symbol) -> Rc<Type> {
        // let links = self.get_symbol_links(symbol);
        // if links.type_.is_none() {
        self.get_type_of_variable_or_parameter_or_property_worker(symbol)
        // }
        // links.type.unwrap().clone()
    }

    fn get_type_of_variable_or_parameter_or_property_worker(&self, symbol: &Symbol) -> Rc<Type> {
        Debug_.assert_is_defined(&symbol.maybe_value_declaration(), None);
        let declaration = symbol
            .maybe_value_declaration()
            .as_ref()
            .unwrap()
            .upgrade()
            .unwrap();

        let type_: Rc<Type>;
        if false {
            unimplemented!()
        } else if is_property_assignment(&*declaration) {
            type_ = self
                .try_get_type_from_effective_type_node(&*declaration)
                .unwrap_or_else(|| {
                    self.check_property_assignment(match &*declaration {
                        Node::PropertyAssignment(property_assignment) => property_assignment,
                        _ => panic!("Expected PropertyAssignment"),
                    })
                });
        } else if is_property_signature(&*declaration) || is_variable_declaration(&*declaration) {
            type_ = self.get_widened_type_for_variable_like_declaration(&*declaration);
        } else {
            unimplemented!()
        }

        type_
    }

    fn get_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        if symbol
            .flags()
            .intersects(SymbolFlags::Variable | SymbolFlags::Property)
        {
            return self.get_type_of_variable_or_parameter_or_property(symbol);
        }
        unimplemented!()
    }

    fn get_non_missing_type_of_symbol(&self, symbol: Rc<Symbol>) -> Rc<Type> {
        self.remove_missing_type(
            self.get_type_of_symbol(&*symbol),
            symbol.flags().intersects(SymbolFlags::Optional),
        )
    }

    fn get_declared_type_of_class_or_interface(
        &self,
        symbol: Rc<Symbol>,
    ) -> Rc<Type /*InterfaceType*/> {
        let kind = if symbol.flags().intersects(SymbolFlags::Class) {
            ObjectFlags::Class
        } else {
            ObjectFlags::Interface
        };

        let type_: InterfaceType =
            BaseInterfaceType::new(self.create_object_type(kind, symbol)).into();
        Rc::new(type_.into())
    }

    fn get_declared_type_of_symbol(&self, symbol: Rc<Symbol>) -> Rc<Type> {
        self.try_get_declared_type_of_symbol(symbol)
            .unwrap_or_else(|| unimplemented!())
    }

    fn try_get_declared_type_of_symbol(&self, symbol: Rc<Symbol>) -> Option<Rc<Type>> {
        if symbol
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface)
        {
            return Some(self.get_declared_type_of_class_or_interface(symbol));
        }
        unimplemented!()
    }

    fn resolve_declared_members(&self, type_: Rc<Type /*InterfaceType*/>) -> Rc<Type> {
        type_
    }

    fn is_type_usable_as_property_name(&self, type_: Rc<Type>) -> bool {
        type_
            .flags()
            .intersects(TypeFlags::StringOrNumberLiteralOrUnique)
    }

    fn has_bindable_name<TNode: NodeInterface>(&self, node: &TNode /*Declaration*/) -> bool {
        !has_dynamic_name(node) || unimplemented!()
    }

    fn get_property_name_from_type(
        &self,
        type_: Rc<Type /*StringLiteralType | NumberLiteralType | UniqueESSymbolType*/>,
    ) -> __String {
        if type_
            .flags()
            .intersects(TypeFlags::StringLiteral | TypeFlags::NumberLiteral)
        {
            return match &*type_ {
                Type::LiteralType(LiteralType::NumberLiteralType(number_literal_type)) => {
                    escape_leading_underscores(&number_literal_type.value.to_string())
                }
                Type::LiteralType(LiteralType::StringLiteralType(string_literal_type)) => {
                    escape_leading_underscores(&string_literal_type.value)
                }
                _ => panic!("Expected NumberLiteralType or StringLiteralType"),
            };
        }
        Debug_.fail(None)
    }

    fn get_members_of_symbol(&self, symbol: Rc<Symbol>) -> Rc<RefCell<SymbolTable>> {
        if false {
            unimplemented!()
        } else {
            symbol.members()
        }
    }

    fn resolve_object_type_members(
        &self,
        type_: Rc<Type /*ObjectType*/>,
        source: Rc<Type /*InterfaceTypeWithDeclaredMembers*/>,
    ) {
        let members: Rc<RefCell<SymbolTable>>;
        if true {
            members = if let Some(source_symbol) = source.maybe_symbol() {
                self.get_members_of_symbol(source_symbol)
            } else {
                unimplemented!()
            };
        } else {
            unimplemented!()
        }
        self.set_structured_type_members(
            match &*type_ {
                Type::ObjectType(object_type) => object_type,
                _ => panic!("Expected ObjectType"),
            },
            members,
        );
    }

    fn resolve_class_or_interface_members(&self, type_: Rc<Type /*InterfaceType*/>) {
        self.resolve_object_type_members(type_.clone(), self.resolve_declared_members(type_));
    }

    fn resolve_structured_type_members(
        &self,
        type_: Rc<Type /*StructuredType*/>,
    ) -> Rc<Type /*ResolvedType*/> {
        if !type_.as_resolvable_type().is_resolved() {
            if let Type::ObjectType(object_type) = &*type_
            /*type_.flags().intersects(TypeFlags::Object)*/
            {
                if false {
                    unimplemented!()
                } else if object_type
                    .object_flags()
                    .intersects(ObjectFlags::ClassOrInterface)
                {
                    self.resolve_class_or_interface_members(type_.clone());
                } else {
                    unimplemented!()
                }
            } else {
                unimplemented!()
            }
        }
        type_
    }

    fn get_properties_of_object_type(&self, type_: Rc<Type>) -> Vec<Rc<Symbol>> {
        if type_.flags().intersects(TypeFlags::Object) {
            return self
                .resolve_structured_type_members(type_)
                .as_resolved_type()
                .properties()
                .iter()
                .map(Clone::clone)
                .collect();
        }
        unimplemented!()
    }

    fn get_property_of_object_type(&self, type_: Rc<Type>, name: &__String) -> Option<Rc<Symbol>> {
        if type_.flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(type_);
            let symbol = (*resolved.as_resolved_type().members())
                .borrow()
                .get(name)
                .map(Clone::clone);
            if let Some(symbol) = symbol {
                if self.symbol_is_value(symbol.clone()) {
                    return Some(symbol);
                }
            }
        }
        None
    }

    fn get_properties_of_type(&self, type_: Rc<Type>) -> Vec<Rc<Symbol>> {
        let type_ = self.get_reduced_apparent_type(type_);
        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            unimplemented!()
        } else {
            self.get_properties_of_object_type(type_)
        }
    }

    fn get_apparent_type(&self, type_: Rc<Type>) -> Rc<Type> {
        let t = if type_.flags().intersects(TypeFlags::Instantiable) {
            unimplemented!()
        } else {
            type_
        };
        if false {
            unimplemented!()
        } else {
            t
        }
    }

    fn get_reduced_apparent_type(&self, type_: Rc<Type>) -> Rc<Type> {
        self.get_reduced_type(self.get_apparent_type(self.get_reduced_type(type_)))
    }

    fn get_reduced_type(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    fn get_property_of_type(&self, type_: Rc<Type>, name: &__String) -> Option<Rc<Symbol>> {
        let type_ = self.get_reduced_apparent_type(type_);
        if type_.flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(type_);
            let symbol = (*resolved.as_resolved_type().members())
                .borrow()
                .get(name)
                .map(Clone::clone);
            if let Some(symbol) = symbol {
                if self.symbol_is_value(symbol.clone()) {
                    return Some(symbol);
                }
            }
            return /*self.get_property_of_object_type(self.global_object_type(), name)*/ None;
        }
        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            unimplemented!()
        }
        None
    }

    fn get_propagating_flags_of_types(
        &self,
        types: &[Rc<Type>],
        exclude_kinds: TypeFlags,
    ) -> ObjectFlags {
        let mut result = ObjectFlags::None;
        for type_ in types {
            if !type_.flags().intersects(exclude_kinds) {
                result |= get_object_flags(&*type_);
            }
        }
        result & ObjectFlags::PropagatingFlags
    }

    fn get_type_from_class_or_interface_reference<TNode: NodeInterface>(
        &self,
        node: &TNode,
        symbol: Rc<Symbol>,
    ) -> Rc<Type> {
        let type_ = self.get_declared_type_of_symbol(self.get_merged_symbol(Some(symbol)).unwrap());
        if true {
            type_
        } else {
            unimplemented!()
        }
    }

    fn get_type_reference_name(
        &self,
        node: &TypeReferenceNode,
    ) -> Option<Rc<Node /*EntityNameOrEntityNameExpression*/>> {
        match node.kind() {
            SyntaxKind::TypeReference => {
                return Some(node.type_name.clone());
            }
            SyntaxKind::ExpressionWithTypeArguments => unimplemented!(),
            _ => (),
        }
        None
    }

    fn resolve_type_reference_name(
        &self,
        type_reference: &TypeReferenceNode,
        meaning: SymbolFlags,
        ignore_errors: Option<bool>,
    ) -> Rc<Symbol> {
        let ignore_errors = ignore_errors.unwrap_or(false);
        let name = self.get_type_reference_name(type_reference);
        let name = match name {
            Some(name) => name,
            None => {
                return self.unknown_symbol();
            }
        };
        let symbol = self.resolve_entity_name(&*name, meaning, Some(ignore_errors), None);
        if symbol.is_some() && !Rc::ptr_eq(symbol.as_ref().unwrap(), &self.unknown_symbol()) {
            symbol.unwrap()
        } else if ignore_errors {
            self.unknown_symbol()
        } else {
            unimplemented!()
        }
    }

    fn get_type_reference_type<TNode: NodeInterface>(
        &self,
        node: &TNode,
        symbol: Rc<Symbol>,
    ) -> Rc<Type> {
        if Rc::ptr_eq(&symbol, &self.unknown_symbol()) {
            unimplemented!()
        }
        if symbol
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface)
        {
            return self.get_type_from_class_or_interface_reference(node, symbol);
        }
        unimplemented!()
    }

    fn get_conditional_flow_type_of_type(&self, type_: Rc<Type>, node: &Node) -> Rc<Type> {
        type_
    }

    fn get_type_from_type_reference(&self, node: &TypeReferenceNode) -> Rc<Type> {
        let mut symbol: Option<Rc<Symbol>> = None;
        let mut type_: Option<Rc<Type>> = None;
        let meaning = SymbolFlags::Type;
        if type_.is_none() {
            symbol = Some(self.resolve_type_reference_name(node, meaning, None));
            type_ = Some(self.get_type_reference_type(node, symbol.unwrap()));
        }
        let type_ = type_.unwrap();
        type_
    }

    fn get_type_of_global_symbol(&self, symbol: Option<Rc<Symbol>>) -> Rc<Type /*ObjectType*/> {
        unimplemented!()
    }

    fn get_global_type_symbol(&self, name: &__String, report_errors: bool) -> Option<Rc<Symbol>> {
        self.get_global_symbol(
            name,
            SymbolFlags::Type,
            if report_errors {
                Some(Diagnostics::Cannot_find_global_type_0)
            } else {
                None
            },
        )
    }

    fn get_global_symbol(
        &self,
        name: &__String,
        meaning: SymbolFlags,
        diagnostic: Option<DiagnosticMessage>,
    ) -> Option<Rc<Symbol>> {
        self.resolve_name(
            Option::<&Node>::None,
            name,
            meaning,
            diagnostic,
            Some(name.clone()),
            false,
            None,
        )
    }

    fn get_global_type(&self, name: &__String, report_errors: bool) -> Option<Rc<Type>> {
        let symbol = self.get_global_type_symbol(name, report_errors);
        if true {
            Some(self.get_type_of_global_symbol(symbol))
        } else {
            None
        }
    }

    fn get_array_or_tuple_target_type(&self, node: &ArrayTypeNode) -> Rc<Type /*GenericType*/> {
        let element_type = self.get_array_element_type_node(node);
        if let Some(element_type) = element_type {
            return self.global_array_type();
        }
        unimplemented!()
    }

    fn get_type_from_array_or_tuple_type_node(&self, node: &ArrayTypeNode) -> Rc<Type> {
        let target = self.get_array_or_tuple_target_type(node);
        if false {
            unimplemented!()
        } else if false {
            unimplemented!()
        } else {
            let element_types = vec![self.get_type_from_type_node(&*node.element_type)];
            return self.create_normalized_type_reference(target, Some(element_types));
        }
    }

    fn create_normalized_type_reference(
        &self,
        target: Rc<Type /*GenericType*/>,
        type_arguments: Option<Vec<Rc<Type>>>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    fn add_type_to_union(
        &self,
        type_set: &mut Vec<Rc<Type>>,
        mut includes: TypeFlags,
        type_: Rc<Type>,
    ) -> TypeFlags {
        let flags = type_.flags();
        if flags.intersects(TypeFlags::Union) {
            unimplemented!()
        }
        if !flags.intersects(TypeFlags::Never) {
            includes |= flags & TypeFlags::IncludesMask;
            if flags.intersects(TypeFlags::Instantiable) {
                includes |= TypeFlags::IncludesInstantiable;
            }
            if false {
                unimplemented!()
            } else {
                type_set.push(type_);
            }
        }
        includes
    }

    fn add_types_to_union(
        &self,
        type_set: &mut Vec<Rc<Type>>,
        mut includes: TypeFlags,
        types: &[Rc<Type>],
    ) -> TypeFlags {
        for type_ in types {
            includes = self.add_type_to_union(type_set, includes, type_.clone());
        }
        includes
    }

    fn get_union_type(
        &self,
        types: Vec<Rc<Type>>,
        union_reduction: Option<UnionReduction>,
    ) -> Rc<Type> {
        let union_reduction = union_reduction.unwrap_or(UnionReduction::Literal);
        if types.is_empty() {
            return self.never_type();
        }
        if types.len() == 1 {
            return types[0].clone();
        }
        let mut type_set: Vec<Rc<Type>> = vec![];
        let includes = self.add_types_to_union(&mut type_set, TypeFlags::None, &types);
        if union_reduction != UnionReduction::None {}
        let object_flags = (if includes.intersects(TypeFlags::NotPrimitiveUnion) {
            ObjectFlags::None
        } else {
            ObjectFlags::PrimitiveUnion
        }) | (if includes.intersects(TypeFlags::Intersection) {
            ObjectFlags::ContainsIntersections
        } else {
            ObjectFlags::None
        });
        self.get_union_type_from_sorted_list(type_set, object_flags)
    }

    fn get_union_type_from_sorted_list(
        &self,
        types: Vec<Rc<Type>>,
        object_flags: ObjectFlags,
    ) -> Rc<Type> {
        let mut type_: Option<Rc<Type>> = None;
        if type_.is_none() {
            let is_boolean = types.len() == 2
                && types[0].flags().intersects(TypeFlags::BooleanLiteral)
                && types[1].flags().intersects(TypeFlags::BooleanLiteral);
            let base_type = self.create_type(if is_boolean {
                TypeFlags::Union | TypeFlags::Boolean
            } else {
                TypeFlags::Union
            });
            let object_flags_to_set =
                object_flags | self.get_propagating_flags_of_types(&types, TypeFlags::Nullable);
            type_ = Some(Rc::new(
                UnionType::new(BaseUnionOrIntersectionType::new(
                    base_type,
                    types,
                    object_flags_to_set,
                ))
                .into(),
            ));
            // TODO: also treat union type as intrinsic type with intrinsic_name = "boolean" if
            // is_boolean - should expose maybe_intrinsic_name on UnionType or something?
        }
        type_.unwrap()
    }

    fn get_literal_type_from_property_name(&self, name: &Node /*PropertyName*/) -> Rc<Type> {
        if let Node::Expression(Expression::Identifier(identifier)) = name {
            self.get_string_literal_type(&unescape_leading_underscores(&identifier.escaped_text))
        } else {
            unimplemented!()
        }
    }

    fn get_literal_type_from_property(
        &self,
        prop: Rc<Symbol>,
        include: TypeFlags,
        include_non_public: Option<bool>,
    ) -> Rc<Type> {
        let include_non_public = include_non_public.unwrap_or(false);
        if include_non_public || true {
            let mut type_ = None;
            if type_.is_none() {
                let name = prop
                    .maybe_value_declaration()
                    .as_ref()
                    .and_then(|value_declaration| {
                        get_name_of_declaration(&*value_declaration.upgrade().unwrap())
                    });
                type_ = if false {
                    unimplemented!()
                } else if let Some(name) = name {
                    Some(self.get_literal_type_from_property_name(&*name))
                } else {
                    unimplemented!()
                }
            }
            if let Some(type_) = type_ {
                if type_.flags().intersects(include) {
                    return type_;
                }
            }
        }
        unimplemented!()
    }

    fn get_property_name_from_index(&self, index_type: Rc<Type>) -> Option<__String> {
        if self.is_type_usable_as_property_name(index_type.clone()) {
            Some(self.get_property_name_from_type(index_type))
        } else {
            unimplemented!()
        }
    }

    fn get_property_type_for_index_type(
        &self,
        original_object_type: Rc<Type>,
        object_type: Rc<Type>,
        index_type: Rc<Type>,
        full_index_type: Rc<Type>,
    ) -> Option<Rc<Type>> {
        let prop_name = if false {
            unimplemented!()
        } else {
            self.get_property_name_from_index(index_type)
        };
        if let Some(prop_name) = prop_name {
            let prop = self.get_property_of_type(object_type, &prop_name);
            if let Some(prop) = prop {
                let prop_type = self.get_type_of_symbol(&*prop);
                return if false {
                    unimplemented!()
                } else {
                    Some(prop_type)
                };
            }
        }
        None
    }

    fn get_indexed_access_type_or_undefined(
        &self,
        object_type: Rc<Type>,
        index_type: Rc<Type>,
    ) -> Option<Rc<Type>> {
        let apparent_object_type = self.get_reduced_apparent_type(object_type.clone());
        self.get_property_type_for_index_type(
            object_type,
            apparent_object_type,
            index_type.clone(),
            index_type.clone(),
        )
    }

    // pub fn create_literal_type(
    pub fn create_string_literal_type(
        &self,
        flags: TypeFlags,
        value: String,
        regular_type: Option<Rc<Type>>,
    ) -> Rc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Rc<Type> = Rc::new(StringLiteralType::new(type_, value).into());
        match &*type_ {
            Type::LiteralType(literal_type) => {
                literal_type.set_regular_type(&regular_type.unwrap_or_else(|| type_.clone()));
            }
            _ => panic!("Expected LiteralType"),
        }
        type_
    }

    pub fn create_number_literal_type(
        &self,
        flags: TypeFlags,
        value: Number,
        regular_type: Option<Rc<Type>>,
    ) -> Rc<Type> {
        let type_ = self.create_type(flags);
        let type_ = BaseLiteralType::new(type_);
        let type_: Rc<Type> = Rc::new(NumberLiteralType::new(type_, value).into());
        match &*type_ {
            Type::LiteralType(literal_type) => {
                literal_type.set_regular_type(&regular_type.unwrap_or_else(|| type_.clone()));
            }
            _ => panic!("Expected LiteralType"),
        }
        type_
    }

    fn get_fresh_type_of_literal_type(&self, type_: Rc<Type>) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Literal) {
            match &*type_ {
                Type::LiteralType(literal_type) => {
                    return literal_type.get_or_initialize_fresh_type(self, &type_);
                }
                _ => panic!("Expected LiteralType"),
            }
        }
        type_
    }

    fn is_fresh_literal_type(&self, type_: Rc<Type>) -> bool {
        if !type_.flags().intersects(TypeFlags::Literal) {
            return false;
        }
        match &*type_ {
            Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
                IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                    ptr::eq(&*type_, freshable_intrinsic_type.fresh_type().as_ptr())
                }
                _ => panic!("Expected FreshableIntrinsicType"),
            },
            Type::LiteralType(literal_type) => {
                ptr::eq(&*type_, literal_type.fresh_type().unwrap().as_ptr())
            }
            _ => panic!("Expected IntrinsicType or LiteralType"),
        }
    }

    fn get_string_literal_type(&self, value: &str) -> Rc<Type> {
        let mut string_literal_types = self.string_literal_types();
        if string_literal_types.contains_key(value) {
            return string_literal_types.get(value).unwrap().clone();
        }
        let type_ =
            self.create_string_literal_type(TypeFlags::StringLiteral, value.to_string(), None);
        string_literal_types.insert(value.to_string(), type_.clone());
        type_
    }

    fn get_number_literal_type(&self, value: Number) -> Rc<Type> {
        let mut number_literal_types = self.number_literal_types();
        if number_literal_types.contains_key(&value) {
            return number_literal_types.get(&value).unwrap().clone();
        }
        let type_ = self.create_number_literal_type(TypeFlags::NumberLiteral, value, None);
        number_literal_types.insert(value, type_.clone());
        type_
    }

    fn get_array_element_type_node(&self, node: &ArrayTypeNode) -> Option<Rc<Node /*TypeNode*/>> {
        Some(node.element_type.clone())
    }

    fn get_type_from_type_node(&self, node: &Node /*TypeNode*/) -> Rc<Type> {
        self.get_conditional_flow_type_of_type(self.get_type_from_type_node_worker(node), node)
    }

    fn get_type_from_type_node_worker(&self, node: &Node /*TypeNode*/) -> Rc<Type> {
        let node = match node {
            Node::TypeNode(type_node) => type_node,
            _ => panic!("Expected TypeNode"),
        };
        match node {
            TypeNode::KeywordTypeNode(_) => match node.kind() {
                SyntaxKind::NumberKeyword => self.number_type(),
                _ => unimplemented!(),
            },
            TypeNode::TypeReferenceNode(type_reference_node) => {
                self.get_type_from_type_reference(type_reference_node)
            }
            TypeNode::ArrayTypeNode(array_type_node) => {
                self.get_type_from_array_or_tuple_type_node(array_type_node)
            }
            _ => unimplemented!(),
        }
    }

    fn is_type_assignable_to(&self, source: Rc<Type>, target: Rc<Type>) -> bool {
        self.is_type_related_to(source, target, &self.assignable_relation)
    }

    fn check_type_assignable_to_and_optionally_elaborate(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        error_node: Option<&Node>,
        expr: Option<&Expression>,
        head_message: Option<DiagnosticMessage>,
    ) -> bool {
        self.check_type_related_to_and_optionally_elaborate(
            source,
            target,
            &self.assignable_relation,
            error_node,
            expr,
            head_message,
        )
    }

    fn check_type_related_to_and_optionally_elaborate(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        relation: &HashMap<String, RelationComparisonResult>,
        error_node: Option<&Node>,
        expr: Option<&Expression>,
        head_message: Option<DiagnosticMessage>,
    ) -> bool {
        if self.is_type_related_to(source.clone(), target.clone(), relation) {
            return true;
        }
        if error_node.is_none()
            || !self.elaborate_error(
                expr,
                source.clone(),
                target.clone(),
                relation,
                head_message.clone(),
            )
        {
            return self.check_type_related_to(source, target, relation, error_node, head_message);
        }
        false
    }

    fn elaborate_error(
        &self,
        node: Option<&Expression>,
        source: Rc<Type>,
        target: Rc<Type>,
        relation: &HashMap<String, RelationComparisonResult>,
        head_message: Option<DiagnosticMessage>,
    ) -> bool {
        if node.is_none() || false {
            return false;
        }
        let node = node.unwrap();
        match node {
            Expression::ObjectLiteralExpression(object_literal_expression) => {
                return self.elaborate_object_literal(
                    object_literal_expression,
                    source,
                    target,
                    relation,
                );
            }
            _ => (),
        }
        false
    }

    fn get_best_match_indexed_access_type_or_undefined(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        name_type: Rc<Type>,
    ) -> Option<Rc<Type>> {
        let idx = self.get_indexed_access_type_or_undefined(target.clone(), name_type);
        if idx.is_some() {
            return idx;
        }
        if target.flags().intersects(TypeFlags::Union) {
            unimplemented!()
        }
        None
    }

    fn check_expression_for_mutable_location_with_contextual_type(
        &self,
        next: &Node, /*Expression*/
        source_prop_type: Rc<Type>,
    ) -> Rc<Type> {
        self.check_expression_for_mutable_location(
            match next {
                Node::Expression(expression) => expression,
                _ => panic!("Expected Expression"),
            },
            Some(source_prop_type),
        )
    }

    fn elaborate_elementwise(
        &self,
        iterator: Vec<ElaborationIteratorItem>,
        source: Rc<Type>,
        target: Rc<Type>,
        relation: &HashMap<String, RelationComparisonResult>,
    ) -> bool {
        let mut reported_error = false;
        for status in iterator {
            let ElaborationIteratorItem {
                error_node: prop,
                inner_expression: next,
                name_type,
                error_message,
            } = status;
            let target_prop_type = self.get_best_match_indexed_access_type_or_undefined(
                source.clone(),
                target.clone(),
                name_type.clone(),
            );
            if target_prop_type.is_none() {
                continue;
            }
            let target_prop_type = target_prop_type.unwrap();
            if target_prop_type
                .flags()
                .intersects(TypeFlags::IndexedAccess)
            {
                continue;
            }
            let source_prop_type =
                self.get_indexed_access_type_or_undefined(source.clone(), name_type);
            if source_prop_type.is_none() {
                continue;
            }
            let source_prop_type = source_prop_type.unwrap();
            if !self.check_type_related_to(
                source_prop_type.clone(),
                target_prop_type.clone(),
                relation,
                None,
                None,
            ) {
                reported_error = true;
                if true {
                    let specific_source = if let Some(next) = next {
                        self.check_expression_for_mutable_location_with_contextual_type(
                            &*next,
                            source_prop_type,
                        )
                    } else {
                        source_prop_type
                    };
                    if false {
                        unimplemented!()
                    } else {
                        let result = self.check_type_related_to(
                            specific_source,
                            target_prop_type,
                            relation,
                            Some(&*prop),
                            error_message,
                        );
                    }
                }
            }
        }
        reported_error
    }

    fn generate_object_literal_elements(
        &self,
        node: &ObjectLiteralExpression,
        // ) -> impl Iterator<Item = ElaborationIteratorItem> {
    ) -> Vec<ElaborationIteratorItem> {
        // if node.properties.is_empty() {
        //     return vec![];
        // }
        node.properties
            .iter()
            .flat_map(|prop| {
                let type_ = self.get_literal_type_from_property(
                    self.get_symbol_of_node(&**prop).unwrap(),
                    TypeFlags::StringOrNumberLiteralOrUnique,
                    None,
                );
                if type_.flags().intersects(TypeFlags::Never) {
                    return vec![];
                }
                match &**prop {
                    Node::PropertyAssignment(property_assignment) => {
                        vec![ElaborationIteratorItem {
                            error_node: property_assignment.name(),
                            inner_expression: Some(property_assignment.initializer.clone()),
                            name_type: type_,
                            error_message: if false { unimplemented!() } else { None },
                        }]
                    }
                    _ => Debug_.assert_never(prop, None),
                }
            })
            .collect()
    }

    fn elaborate_object_literal(
        &self,
        node: &ObjectLiteralExpression,
        source: Rc<Type>,
        target: Rc<Type>,
        relation: &HashMap<String, RelationComparisonResult>,
    ) -> bool {
        if target.flags().intersects(TypeFlags::Primitive) {
            return false;
        }
        self.elaborate_elementwise(
            self.generate_object_literal_elements(node),
            source,
            target,
            relation,
        )
    }

    fn is_simple_type_related_to(
        &self,
        source: &Type,
        target: &Type,
        relation: &HashMap<String, RelationComparisonResult>,
        error_reporter: Option<ErrorReporter>,
    ) -> bool {
        let s = source.flags();
        let t = target.flags();
        if s.intersects(TypeFlags::NumberLike) && t.intersects(TypeFlags::Number) {
            return true;
        }
        false
    }

    fn is_type_related_to(
        &self,
        mut source: Rc<Type>,
        target: Rc<Type>,
        relation: &HashMap<String, RelationComparisonResult>,
    ) -> bool {
        if self.is_fresh_literal_type(source.clone()) {
            source = match &*source {
                Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
                    IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                        freshable_intrinsic_type.regular_type().upgrade().unwrap()
                    }
                    _ => panic!("Expected IntrinsicType"),
                },
                Type::LiteralType(literal_type) => literal_type.regular_type(),
                _ => panic!("Expected IntrinsicType or LiteralType"),
            };
        }
        if true {
            if self.is_simple_type_related_to(&*source, &*target, relation, None) {
                return true;
            }
        } else {
            unimplemented!()
        }
        if source
            .flags()
            .intersects(TypeFlags::StructuredOrInstantiable)
            || target
                .flags()
                .intersects(TypeFlags::StructuredOrInstantiable)
        {
            return self.check_type_related_to(source, target, relation, None, None);
        }
        false
    }

    fn get_normalized_type(&self, mut type_: Rc<Type>) -> Rc<Type> {
        loop {
            let t: Rc<Type> = if self.is_fresh_literal_type(type_.clone()) {
                match &*type_ {
                    Type::IntrinsicType(intrinsic_type) => match intrinsic_type {
                        IntrinsicType::FreshableIntrinsicType(freshable_intrinsic_type) => {
                            freshable_intrinsic_type.regular_type().upgrade().unwrap()
                        }
                        _ => panic!("Expected FreshableIntrinsicType"),
                    },
                    Type::LiteralType(literal_type) => literal_type.regular_type(),
                    _ => panic!("Expected IntrinsicType or LiteralType"),
                }
            } else {
                type_.clone()
            };
            if Rc::ptr_eq(&t, &type_) {
                break;
            }
            type_ = t.clone();
        }
        type_
    }

    fn check_type_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        relation: &HashMap<String, RelationComparisonResult>,
        error_node: Option<&Node>,
        head_message: Option<DiagnosticMessage>,
    ) -> bool {
        CheckTypeRelatedTo::new(self, source, target, relation, error_node, head_message).call()
    }

    fn type_could_have_top_level_singleton_types(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Boolean) {
            return false;
        }

        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            return for_each(type_.as_union_or_intersection_type().types(), |type_, _| {
                if self.type_could_have_top_level_singleton_types(type_) {
                    Some(())
                } else {
                    None
                }
            })
            .is_some();
        }

        if type_.flags().intersects(TypeFlags::Instantiable) {
            unimplemented!()
        }

        self.is_unit_type(type_) || type_.flags().intersects(TypeFlags::TemplateLiteral)
    }

    fn is_unit_type(&self, type_: &Type) -> bool {
        type_.flags().intersects(TypeFlags::Unit)
    }

    fn is_literal_type(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Boolean) {
            true
        } else if type_.flags().intersects(TypeFlags::Union) {
            if type_.flags().intersects(TypeFlags::EnumLiteral) {
                true
            } else {
                every(type_.as_union_or_intersection_type().types(), |type_, _| {
                    self.is_unit_type(&**type_)
                })
            }
        } else {
            self.is_unit_type(type_)
        }
    }

    fn get_base_type_of_literal_type(&self, type_: Rc<Type>) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::EnumLiteral) {
            unimplemented!()
        } else if type_.flags().intersects(TypeFlags::StringLiteral) {
            unimplemented!()
        } else if type_.flags().intersects(TypeFlags::NumberLiteral) {
            self.number_type()
        } else if type_.flags().intersects(TypeFlags::BigIntLiteral) {
            self.bigint_type()
        } else if type_.flags().intersects(TypeFlags::BooleanLiteral) {
            self.boolean_type()
        } else if type_.flags().intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_| Some(self.get_base_type_of_literal_type(type_)),
                None,
            )
            .unwrap()
        } else {
            type_
        }
    }

    fn get_widened_literal_type(&self, type_: Rc<Type>) -> Rc<Type> {
        let flags = type_.flags();
        if flags.intersects(TypeFlags::EnumLiteral) && self.is_fresh_literal_type(type_.clone()) {
            unimplemented!()
        } else if flags.intersects(TypeFlags::StringLiteral)
            && self.is_fresh_literal_type(type_.clone())
        {
            unimplemented!()
        } else if flags.intersects(TypeFlags::NumberLiteral)
            && self.is_fresh_literal_type(type_.clone())
        {
            self.number_type()
        } else if flags.intersects(TypeFlags::BigIntLiteral)
            && self.is_fresh_literal_type(type_.clone())
        {
            self.bigint_type()
        } else if flags.intersects(TypeFlags::BooleanLiteral)
            && self.is_fresh_literal_type(type_.clone())
        {
            self.boolean_type()
        } else if flags.intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_| Some(self.get_widened_literal_type(type_)),
                None,
            )
            .unwrap()
        } else {
            type_
        }
    }

    fn get_widened_unique_es_symbol_type(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    fn get_widened_literal_like_type_for_contextual_type(
        &self,
        mut type_: Rc<Type>,
        contextual_type: Option<Rc<Type>>,
    ) -> Rc<Type> {
        if !self.is_literal_of_contextual_type(type_.clone(), contextual_type) {
            type_ = self.get_widened_unique_es_symbol_type(self.get_widened_literal_type(type_));
        }
        type_
    }

    fn get_optional_type(&self, type_: Rc<Type>, is_property: Option<bool>) -> Rc<Type> {
        let is_property = is_property.unwrap_or(false);
        Debug_.assert(self.strict_null_checks, None);
        if type_.flags().intersects(TypeFlags::Undefined) {
            type_
        } else {
            unimplemented!()
        }
    }

    fn remove_missing_type(&self, type_: Rc<Type>, is_optional: bool) -> Rc<Type> {
        if self.exact_optional_property_types && is_optional {
            unimplemented!()
        } else {
            type_
        }
    }

    fn get_regular_type_of_object_literal(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    fn get_widened_type(&self, type_: Rc<Type>) -> Rc<Type> {
        self.get_widened_type_with_context(type_)
    }

    fn get_widened_type_with_context(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    fn is_object_literal_type(&self, type_: Rc<Type>) -> bool {
        get_object_flags(&*type_).intersects(ObjectFlags::ObjectLiteral)
    }

    fn get_cannot_find_name_diagnostic_for_name(&self, node: &Node) -> DiagnosticMessage {
        match node {
            Node::Expression(Expression::Identifier(identifier)) => match identifier.escaped_text {
                _ => {
                    if false {
                        unimplemented!()
                    } else {
                        Diagnostics::Cannot_find_name_0
                    }
                }
            },
            _ => panic!("Expected Identifier"),
        }
    }

    fn filter_type(&self, type_: Rc<Type>, f: fn(&TypeChecker, Rc<Type>) -> bool) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union) {
            unimplemented!()
        }
        if type_.flags().intersects(TypeFlags::Never) || f(self, type_.clone()) {
            type_
        } else {
            self.never_type()
        }
    }

    fn map_type<TMapper: FnMut(Rc<Type>) -> Option<Rc<Type>>>(
        &self,
        type_: Rc<Type>,
        mapper: &mut TMapper,
        no_reductions: Option<bool>,
    ) -> Option<Rc<Type>> {
        let no_reductions = no_reductions.unwrap_or(false);
        if type_.flags().intersects(TypeFlags::Never) {
            return Some(type_);
        }
        if !type_.flags().intersects(TypeFlags::Union) {
            return mapper(type_);
        }
        let types = type_.as_union_or_intersection_type().types();
        let mut mapped_types: Vec<Rc<Type>> = vec![];
        let mut changed = false;
        for t in types {
            let mapped = if t.flags().intersects(TypeFlags::Union) {
                self.map_type(t.clone(), mapper, Some(no_reductions))
            } else {
                mapper(t.clone())
            };
            changed = changed
                || match mapped.as_ref() {
                    None => true,
                    Some(mapped) => !Rc::ptr_eq(t, mapped),
                };
            if let Some(mapped) = mapped {
                mapped_types.push(mapped);
            }
        }
        if changed {
            if !mapped_types.is_empty() {
                Some(self.get_union_type(
                    mapped_types,
                    Some(if no_reductions {
                        UnionReduction::None
                    } else {
                        UnionReduction::Literal
                    }),
                ))
            } else {
                None
            }
        } else {
            Some(type_)
        }
    }

    fn get_constituent_count(&self, type_: Rc<Type>) -> usize {
        if type_.flags().intersects(TypeFlags::Union) {
            type_.as_union_or_intersection_type().types().len()
        } else {
            1
        }
    }

    fn get_contextual_type_for_variable_like_declaration(
        &self,
        declaration: &Node,
    ) -> Option<Rc<Type>> {
        let type_node = get_effective_type_annotation_node(declaration);
        if let Some(type_node) = type_node {
            return Some(self.get_type_from_type_node(&*type_node));
        }
        match declaration.kind() {
            _ => None,
        }
    }

    fn get_contextual_type_for_initializer_expression<TNode: NodeInterface>(
        &self,
        node: &TNode,
    ) -> Option<Rc<Type>> {
        let parent = node.parent();
        let declaration = match &*parent {
            Node::VariableDeclaration(variable_declaration) => variable_declaration,
            _ => panic!("Expected VariableDeclaration"),
        };
        if has_initializer(declaration)
            && Rc::ptr_eq(&node.node_wrapper(), &declaration.initializer().unwrap())
        {
            let result = self.get_contextual_type_for_variable_like_declaration(&*parent);
            if result.is_some() {
                return result;
            }
        }
        None
    }

    fn get_type_of_property_of_contextual_type(
        &self,
        type_: Rc<Type>,
        name: &__String,
    ) -> Option<Rc<Type>> {
        self.map_type(
            type_,
            &mut |t| {
                if false {
                    unimplemented!()
                } else if t.flags().intersects(TypeFlags::StructuredType) {
                    let prop = self.get_property_of_type(t, name);
                    if let Some(prop) = prop {
                        return if false {
                            None
                        } else {
                            Some(self.get_type_of_symbol(&*prop))
                        };
                    }
                    return if let Some(found) = Option::<()>::None /*self.find_applicable_index_info(self.get_index_infos_of_structured_type(t), self.get_string_literal_type(unescape_leading_underscores(name)))*/ {
                        unimplemented!()
                    } else {
                        None
                    };
                }
                None
            },
            Some(true),
        )
    }

    fn get_contextual_type_for_object_literal_element(
        &self,
        element: &PropertyAssignment,
    ) -> Option<Rc<Type>> {
        let parent = element.parent();
        let object_literal = match &*parent {
            Node::Expression(Expression::ObjectLiteralExpression(object_literal_expression)) => {
                object_literal_expression
            }
            _ => panic!("Expected ObjectLiteralExpression"),
        };
        // let property_assignment_type = if is_property_assignment(element) {
        // } else {
        //     None
        // };
        // if property_assignment_type.is_some() {
        //     return property_assignment_type;
        // }
        let type_ = self.get_apparent_type_of_contextual_type(object_literal);
        if let Some(type_) = type_ {
            if self.has_bindable_name(element) {
                return self.get_type_of_property_of_contextual_type(
                    type_,
                    &self.get_symbol_of_node(element).unwrap().escaped_name,
                );
            }
            unimplemented!()
        }
        None
    }

    fn get_apparent_type_of_contextual_type<TNode: NodeInterface>(
        &self,
        node: &TNode, /*Expression | MethodDeclaration*/
    ) -> Option<Rc<Type>> {
        let contextual_type = if false {
            unimplemented!()
        } else {
            self.get_contextual_type(node)
        };
        let instantiated_type = self.instantiate_contextual_type(contextual_type, node);
        if let Some(instantiated_type) = instantiated_type {
            if true {
                let apparent_type = self
                    .map_type(
                        instantiated_type,
                        &mut |type_| Some(self.get_apparent_type(type_)),
                        Some(true),
                    )
                    .unwrap();
                return if apparent_type.flags().intersects(TypeFlags::Union)
                    && is_object_literal_expression(node)
                {
                    unimplemented!()
                } else if false {
                    unimplemented!()
                } else {
                    Some(apparent_type)
                };
            }
        }
        None
    }

    fn instantiate_contextual_type<TNode: NodeInterface>(
        &self,
        contextual_type: Option<Rc<Type>>,
        node: &TNode,
    ) -> Option<Rc<Type>> {
        if false {
            unimplemented!()
        }
        contextual_type
    }

    fn get_contextual_type<TNode: NodeInterface>(
        &self,
        node: &TNode, /*Expression*/
    ) -> Option<Rc<Type>> {
        let parent = node.parent();
        match &*parent {
            Node::VariableDeclaration(variable_declaration) => {
                self.get_contextual_type_for_initializer_expression(node)
            }
            Node::PropertyAssignment(property_assignment) => {
                self.get_contextual_type_for_object_literal_element(property_assignment)
            }
            _ => unimplemented!(),
        }
    }

    fn check_object_literal(&self, node: &ObjectLiteralExpression) -> Rc<Type> {
        let mut properties_table = create_symbol_table();
        let mut properties_array: Vec<Rc<Symbol>> = vec![];

        let object_flags = self.fresh_object_literal_flag;

        for member_decl in &node.properties {
            let member = self.get_symbol_of_node(&**member_decl).unwrap();
            if member_decl.kind() == SyntaxKind::PropertyAssignment {
            } else {
                unimplemented!()
            }

            if false {
                unimplemented!()
            } else {
                properties_table.insert(member.escaped_name.clone(), member.clone());
            }
            properties_array.push(member);
        }

        let create_object_literal_type = || {
            let mut result =
                self.create_anonymous_type(node.symbol(), Rc::new(RefCell::new(properties_table)));
            result.set_object_flags(
                result.object_flags()
                    | object_flags
                    | ObjectFlags::ObjectLiteral
                    | ObjectFlags::ContainsObjectOrArrayLiteral,
            );
            Rc::new(result.into())
        };

        create_object_literal_type()
    }

    fn is_known_property(
        &self,
        target_type: Rc<Type>,
        name: &__String,
        is_comparing_jsx_attributes: bool,
    ) -> bool {
        if target_type.flags().intersects(TypeFlags::Object) {
            if self
                .get_property_of_object_type(target_type, name)
                .is_some()
                || false
            {
                return true;
            }
        } else if target_type
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
            && self.is_excess_property_check_target(target_type)
        {
            unimplemented!()
        }
        false
    }

    fn is_excess_property_check_target(&self, type_: Rc<Type>) -> bool {
        (type_.flags().intersects(TypeFlags::Object)
            && !(get_object_flags(&*type_)
                .intersects(ObjectFlags::ObjectLiteralPatternWithComputedProperties)))
            || type_.flags().intersects(TypeFlags::NonPrimitive)
            || (type_.flags().intersects(TypeFlags::Union) && unimplemented!())
            || (type_.flags().intersects(TypeFlags::Intersection) && unimplemented!())
    }

    fn check_arithmetic_operand_type(
        &self,
        operand: /*&Node*/ &Expression,
        type_: Rc<Type>,
        diagnostic: &DiagnosticMessage,
    ) -> bool {
        if !self.is_type_assignable_to(type_, self.number_or_big_int_type()) {
            self.error_and_maybe_suggest_await(operand, diagnostic);
            return false;
        }
        true
    }

    fn check_prefix_unary_expression(&self, node: &PrefixUnaryExpression) -> Rc<Type> {
        let operand_expression = match &*node.operand {
            Node::Expression(expression) => expression,
            _ => panic!("Expected Expression"),
        };
        let operand_type = self.check_expression(operand_expression);
        match node.operator {
            SyntaxKind::PlusPlusToken => {
                self.check_arithmetic_operand_type(operand_expression, operand_type.clone(), &Diagnostics::An_arithmetic_operand_must_be_of_type_any_number_bigint_or_an_enum_type);
                return self.get_unary_result_type(&operand_type);
            }
            _ => {
                unimplemented!();
            }
        }
    }

    fn get_unary_result_type(&self, operand_type: &Type) -> Rc<Type> {
        self.number_type()
    }

    fn maybe_type_of_kind(&self, type_: &Type, kind: TypeFlags) -> bool {
        if type_.flags().intersects(kind) {
            return true;
        }
        if let Type::UnionOrIntersectionType(type_) = type_ {
            for t in type_.types() {
                if self.maybe_type_of_kind(&**t, kind) {
                    return true;
                }
            }
        }
        false
    }

    fn check_expression_cached(&mut self, node: &Expression) -> Rc<Type> {
        self.check_expression(node)
    }

    fn is_literal_of_contextual_type(
        &self,
        candidate_type: Rc<Type>,
        contextual_type: Option<Rc<Type>>,
    ) -> bool {
        if let Some(contextual_type) = contextual_type {
            if let Type::UnionOrIntersectionType(union_or_intersection_type) = &*contextual_type {
                let types = union_or_intersection_type.types();
                // return some(
                //     types,
                //     Some(Box::new(|t| {
                //         self.is_literal_of_contextual_type(candidate_type, Some(t.clone()))
                //     })),
                // );
                return types.iter().any(|t| {
                    self.is_literal_of_contextual_type(candidate_type.clone(), Some(t.clone()))
                });
            }
            return contextual_type.flags().intersects(
                TypeFlags::StringLiteral
                    | TypeFlags::Index
                    | TypeFlags::TemplateLiteral
                    | TypeFlags::StringMapping,
            ) && self.maybe_type_of_kind(&*candidate_type, TypeFlags::StringLiteral)
                || contextual_type.flags().intersects(TypeFlags::NumberLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::NumberLiteral)
                || contextual_type.flags().intersects(TypeFlags::BigIntLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::BigIntLiteral)
                || contextual_type
                    .flags()
                    .intersects(TypeFlags::BooleanLiteral)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::BooleanLiteral)
                || contextual_type
                    .flags()
                    .intersects(TypeFlags::UniqueESSymbol)
                    && self.maybe_type_of_kind(&*candidate_type, TypeFlags::UniqueESSymbol);
        }
        false
    }

    fn check_expression_for_mutable_location(
        &self,
        node: &Expression,
        contextual_type: Option<Rc<Type>>,
    ) -> Rc<Type> {
        let type_ = self.check_expression(node);
        if false {
            unimplemented!()
        } else {
            self.get_widened_literal_like_type_for_contextual_type(
                type_,
                self.instantiate_contextual_type(
                    if contextual_type.is_none() {
                        self.get_contextual_type(node)
                    } else {
                        Some(contextual_type.unwrap())
                    },
                    node,
                ),
            )
        }
    }

    fn check_property_assignment(&self, node: &PropertyAssignment) -> Rc<Type> {
        self.check_expression_for_mutable_location(
            match &*node.initializer {
                Node::Expression(expression) => expression,
                _ => panic!("Expected Expression"),
            },
            None,
        )
    }

    fn check_expression(&self, node: &Expression) -> Rc<Type> {
        self.check_expression_worker(node)
    }

    fn check_expression_worker(&self, node: &Expression) -> Rc<Type> {
        match node {
            Expression::TokenExpression(token_expression) => match token_expression.kind() {
                SyntaxKind::TrueKeyword => self.true_type(),
                _ => unimplemented!(),
            },
            Expression::ObjectLiteralExpression(object_literal_expression) => {
                self.check_object_literal(object_literal_expression)
            }
            Expression::PrefixUnaryExpression(prefix_unary_expression) => {
                self.check_prefix_unary_expression(prefix_unary_expression)
            }
            // Expression::BinaryExpression(binary_expression) => {
            //     return self.check_binary_expression(binary_expression);
            // }
            Expression::LiteralLikeNode(LiteralLikeNode::NumericLiteral(numeric_literal)) => {
                self.check_grammar_numeric_literal(numeric_literal);
                let type_: Rc<Type> = self.get_number_literal_type(numeric_literal.text().into());
                self.get_fresh_type_of_literal_type(type_)
            }
            _ => unimplemented!(),
        }
    }

    fn check_property_declaration(&mut self, node: &PropertySignature) {
        self.check_variable_like_declaration(node);
    }

    fn check_property_signature(&mut self, node: &PropertySignature) {
        if is_private_identifier(&*node.name()) {
            self.error(
                Some(node),
                &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
            );
        }
        self.check_property_declaration(node)
    }

    fn check_type_reference_node(&mut self, node: &TypeReferenceNode) {
        let type_ = self.get_type_from_type_reference(node);
    }

    fn check_array_type(&mut self, node: &ArrayTypeNode) {
        self.check_source_element(Some(&*node.element_type));
    }

    fn convert_auto_to_any(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    fn check_variable_like_declaration<TNode: VariableLikeDeclarationInterface>(
        &mut self,
        node: &TNode,
    ) {
        if !is_binding_element(node) {
            self.check_source_element(node.type_());
        }

        let symbol = self.get_symbol_of_node(node).unwrap();

        let type_ = self.convert_auto_to_any(self.get_type_of_symbol(&*symbol));
        let value_declaration = symbol.maybe_value_declaration();
        let wrapper = node.node_wrapper();
        if value_declaration.is_some()
            && Rc::ptr_eq(
                &wrapper,
                &value_declaration.as_ref().unwrap().upgrade().unwrap(),
            )
        {
            let initializer = get_effective_initializer(node);
            if let Some(initializer) = initializer {
                if true {
                    let initializer_type = self.check_expression_cached(match &*initializer {
                        Node::Expression(expression) => expression,
                        _ => panic!("Expected Expression"),
                    });
                    self.check_type_assignable_to_and_optionally_elaborate(
                        initializer_type,
                        type_,
                        Some(&*wrapper),
                        Some(match &*initializer {
                            Node::Expression(expression) => expression,
                            _ => panic!("Expected Expression"),
                        }),
                        None,
                    );
                }
            }
        } else {
            unimplemented!()
        }
    }

    fn check_variable_declaration(&mut self, node: &VariableDeclaration) {
        self.check_variable_like_declaration(node);
    }

    fn check_variable_statement(&mut self, node: &VariableStatement) {
        for_each(
            &match &*node.declaration_list {
                Node::VariableDeclarationList(variable_declaration_list) => {
                    variable_declaration_list
                }
                _ => panic!("Expected VariableDeclarationList"),
            }
            .declarations,
            |declaration, _| Some(self.check_source_element(Some(&**declaration))),
        );
    }

    fn check_expression_statement(&mut self, node: &ExpressionStatement) {
        let expression = match &*node.expression {
            Node::Expression(expression) => expression,
            _ => panic!("Expected Expression"),
        };
        self.check_expression(expression);
    }

    fn check_interface_declaration(&mut self, node: &InterfaceDeclaration) {
        for_each(&node.members, |member, _| {
            self.check_source_element(Some(&**member));
            Option::<()>::None
        });
    }

    fn check_source_element<TNodeRef: Borrow<Node>>(&mut self, node: Option<TNodeRef>) {
        if let Some(node) = node {
            let node = node.borrow();
            self.check_source_element_worker(node);
        }
    }

    fn check_source_element_worker(&mut self, node: &Node) {
        match node {
            Node::TypeElement(TypeElement::PropertySignature(property_signature)) => {
                self.check_property_signature(property_signature)
            }
            Node::TypeNode(TypeNode::TypeReferenceNode(type_reference_node)) => {
                self.check_type_reference_node(type_reference_node)
            }
            Node::TypeNode(TypeNode::KeywordTypeNode(_)) => (),
            Node::TypeNode(TypeNode::ArrayTypeNode(array_type_node)) => {
                self.check_array_type(array_type_node)
            }
            Node::Statement(Statement::VariableStatement(variable_statement)) => {
                self.check_variable_statement(variable_statement)
            }
            Node::Statement(Statement::ExpressionStatement(expression_statement)) => {
                self.check_expression_statement(expression_statement)
            }
            Node::VariableDeclaration(variable_declaration) => {
                self.check_variable_declaration(variable_declaration)
            }
            Node::Statement(Statement::InterfaceDeclaration(interface_declaration)) => {
                self.check_interface_declaration(interface_declaration)
            }
            _ => unimplemented!("{:?}", node.kind()),
        };
    }

    fn check_source_file(&mut self, source_file: &SourceFile) {
        self.check_source_file_worker(source_file)
    }

    fn check_source_file_worker(&mut self, node: &SourceFile) {
        if true {
            for_each(&node.statements, |statement, _index| {
                self.check_source_element(Some(&**statement));
                Option::<()>::None
            });
        }
    }

    pub fn get_diagnostics(&mut self, source_file: &SourceFile) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_worker(source_file)
    }

    fn get_diagnostics_worker(&mut self, source_file: &SourceFile) -> Vec<Rc<Diagnostic>> {
        self.check_source_file(source_file);

        let semantic_diagnostics = self.diagnostics().get_diagnostics(&source_file.file_name);

        semantic_diagnostics
    }

    fn initialize_type_checker<TTypeCheckerHost: TypeCheckerHost>(
        &mut self,
        host: &TTypeCheckerHost,
    ) {
        for file in host.get_source_files() {
            bind_source_file(&*file);
            println!("post-binding: {:#?}", file);
        }

        for file in host.get_source_files() {
            if !is_external_or_common_js_module(match &*file {
                Node::SourceFile(source_file) => source_file,
                _ => panic!("Expected SourceFile"),
            }) {
                self.merge_symbol_table(&mut *self.globals(), &*file.locals(), None);
            }
        }

        // self.global_array_type = self.get_global_type(__String::new("Array".to_string()));
    }

    fn check_grammar_numeric_literal(&self, node: &NumericLiteral) -> bool {
        false
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

#[derive(Debug)]
struct ElaborationIteratorItem {
    pub error_node: Rc<Node>,
    pub inner_expression: Option<Rc<Node /*Expression*/>>,
    name_type: Rc<Type>,
    error_message: Option<DiagnosticMessage>,
}

struct DefaultNodeBuilderContextSymbolTracker {}

impl DefaultNodeBuilderContextSymbolTracker {
    pub fn new() -> Self {
        Self {}
    }
}

impl SymbolTracker for DefaultNodeBuilderContextSymbolTracker {}

enum ResolveNameNameArg {
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

pub struct NodeBuilderContext<'symbol_tracker> {
    tracker: &'symbol_tracker dyn SymbolTracker,
}

impl<'symbol_tracker> NodeBuilderContext<'symbol_tracker> {
    pub fn new(tracker: &'symbol_tracker dyn SymbolTracker) -> Self {
        Self { tracker }
    }
}

type ErrorReporter<'a> = &'a dyn FnMut(DiagnosticMessage, Option<Vec<String>>);

struct CheckTypeRelatedTo<'type_checker> {
    type_checker: &'type_checker TypeChecker,
    source: Rc<Type>,
    target: Rc<Type>,
    relation: &'type_checker HashMap<String, RelationComparisonResult>,
    error_node: Option<&'type_checker Node>,
    head_message: Option<DiagnosticMessage>,
    error_info: RefCell<Option<DiagnosticMessageChain>>,
    expanding_flags: ExpandingFlags,
    incompatible_stack: RefCell<Vec<(DiagnosticMessage, Option<Vec<String>>)>>,
}

impl<'type_checker> CheckTypeRelatedTo<'type_checker> {
    fn new(
        type_checker: &'type_checker TypeChecker,
        source: Rc<Type>,
        target: Rc<Type>,
        relation: &'type_checker HashMap<String, RelationComparisonResult>,
        error_node: Option<&'type_checker Node>,
        head_message: Option<DiagnosticMessage>,
    ) -> Self {
        Self {
            type_checker,
            source,
            target,
            relation,
            error_node,
            head_message,
            error_info: RefCell::new(None),
            expanding_flags: ExpandingFlags::None,
            incompatible_stack: RefCell::new(vec![]),
        }
    }

    fn error_info(&self) -> Ref<Option<DiagnosticMessageChain>> {
        self.error_info.borrow()
    }

    fn set_error_info(&self, error_info: DiagnosticMessageChain) {
        *self.error_info.borrow_mut() = Some(error_info);
    }

    fn incompatible_stack(&self) -> RefMut<Vec<(DiagnosticMessage, Option<Vec<String>>)>> {
        self.incompatible_stack.borrow_mut()
    }

    fn call(&self) -> bool {
        let result = self.is_related_to(
            self.source.clone(),
            self.target.clone(),
            Some(RecursionFlags::Both),
            self.error_node.is_some(),
            self.head_message.as_ref(),
            None,
        );

        if !self.incompatible_stack().is_empty() {
            self.report_incompatible_stack();
        } else if false {
            unimplemented!()
        } else if self.error_info().is_some() {
            let diag = create_diagnostic_for_node_from_message_chain(
                &*self.error_node.unwrap(),
                self.error_info().clone().unwrap(),
            );
            if true {
                self.type_checker.diagnostics().add(Rc::new(diag.into()));
            }
        }

        result != Ternary::False
    }

    fn report_incompatible_error(&self, message: DiagnosticMessage, args: Option<Vec<String>>) {
        self.incompatible_stack().push((message, args));
    }

    fn report_incompatible_stack(&self) {
        unimplemented!()
    }

    fn report_error(&self, message: &DiagnosticMessage, args: Option<Vec<String>>) {
        Debug_.assert(self.error_node.is_some(), None);
        let error_info = { chain_diagnostic_messages(self.error_info().clone(), message, args) };
        self.set_error_info(error_info);
    }

    fn report_relation_error(
        &self,
        mut message: Option<&DiagnosticMessage>,
        source: Rc<Type>,
        target: Rc<Type>,
    ) {
        let (source_type, target_type) = self
            .type_checker
            .get_type_names_for_error_display(source.clone(), target.clone());
        let mut generalized_source = source.clone();
        let mut generalized_source_type = source_type;

        if self.type_checker.is_literal_type(&*source)
            && !self
                .type_checker
                .type_could_have_top_level_singleton_types(&*target)
        {
            generalized_source = self
                .type_checker
                .get_base_type_of_literal_type(source.clone());
            Debug_.assert(
                !self
                    .type_checker
                    .is_type_assignable_to(generalized_source.clone(), target),
                Some("generalized source shouldn't be assignable"),
            );
            generalized_source_type = self
                .type_checker
                .get_type_name_for_error_display(generalized_source);
        }

        if message.is_none() {
            if false {
            } else {
                message = Some(&Diagnostics::Type_0_is_not_assignable_to_type_1);
            }
        }

        self.report_error(
            message.unwrap(),
            Some(vec![generalized_source_type, target_type]),
        );
    }

    fn is_related_to(
        &self,
        original_source: Rc<Type>,
        original_target: Rc<Type>,
        recursion_flags: Option<RecursionFlags>,
        report_errors: bool,
        head_message: Option<&DiagnosticMessage>,
        intersection_state: Option<IntersectionState>,
    ) -> Ternary {
        let intersection_state = intersection_state.unwrap_or(IntersectionState::None);
        let recursion_flags = recursion_flags.unwrap_or(RecursionFlags::Both);

        let source = self.type_checker.get_normalized_type(original_source);
        let target = self
            .type_checker
            .get_normalized_type(original_target.clone());

        let report_error_results = |source, target, result| {
            if result == Ternary::False && report_errors {
                let source = source;
                let target = target;
                self.report_relation_error(head_message, source, target);
            }
        };

        let report_error = |message: DiagnosticMessage, args: Option<Vec<String>>| {
            self.report_error(&message, args);
        };

        if false
            || self.type_checker.is_simple_type_related_to(
                &*source,
                &*target,
                self.relation,
                if report_errors {
                    Some(&report_error)
                } else {
                    None
                },
            )
        {
            return Ternary::True;
        }

        let is_performing_excess_property_checks = !intersection_state
            .intersects(IntersectionState::Target)
            && (self.type_checker.is_object_literal_type(source.clone())
                && get_object_flags(&*source).intersects(ObjectFlags::FreshLiteral));
        if is_performing_excess_property_checks {
            if self.has_excess_properties(source.clone(), target.clone(), report_errors) {
                if report_errors {
                    self.report_relation_error(
                        head_message,
                        source,
                        if false { original_target } else { target },
                    );
                }
                return Ternary::False;
            }
        }

        let mut result = Ternary::False;

        if (source.flags().intersects(TypeFlags::Union)
            || target.flags().intersects(TypeFlags::Union))
            && self.type_checker.get_constituent_count(source.clone())
                * self.type_checker.get_constituent_count(target.clone())
                < 4
        {
            result = self.structured_type_related_to(
                source.clone(),
                target.clone(),
                report_errors,
                intersection_state | IntersectionState::UnionIntersectionCheck,
            );
        }
        if result == Ternary::False
            && !(source.flags().intersects(TypeFlags::Union))
            && (source
                .flags()
                .intersects(TypeFlags::StructuredOrInstantiable)
                || source
                    .flags()
                    .intersects(TypeFlags::StructuredOrInstantiable))
        {
            result = self.recursive_type_related_to(
                source.clone(),
                target.clone(),
                report_errors,
                intersection_state,
                recursion_flags,
            );
        }

        report_error_results(source, target, result);

        result
    }

    fn has_excess_properties(
        &self,
        source: Rc<Type /*FreshObjectLiteralType*/>,
        target: Rc<Type>,
        report_errors: bool,
    ) -> bool {
        if !self
            .type_checker
            .is_excess_property_check_target(target.clone())
            || false
        {
            return false;
        }
        let is_comparing_jsx_attributes = false;
        let reduced_target = target.clone();
        for prop in self.type_checker.get_properties_of_type(source.clone()) {
            if self.should_check_as_excess_property(prop.clone(), source.symbol()) && true {
                if !self.type_checker.is_known_property(
                    reduced_target.clone(),
                    &prop.escaped_name,
                    is_comparing_jsx_attributes,
                ) {
                    if report_errors {
                        let error_target = self.type_checker.filter_type(
                            reduced_target,
                            TypeChecker::is_excess_property_check_target,
                        );
                        let error_node = match self.error_node {
                            None => Debug_.fail(None),
                            Some(error_node) => error_node,
                        };
                        if false {
                            unimplemented!()
                        } else {
                            let object_literal_declaration =
                                if let Some(symbol) = source.maybe_symbol() {
                                    if let Some(declarations) = &*symbol.maybe_declarations() {
                                        first_or_undefined(declarations).map(Clone::clone)
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                };
                            if false {
                                unimplemented!()
                            } else {
                                self.report_error(
                                    &Diagnostics::Object_literal_may_only_specify_known_properties_and_0_does_not_exist_in_type_1,
                                    Some(vec![self.type_checker.symbol_to_string(prop, None, None, None, None), self.type_checker.type_to_string(error_target)])
                                );
                            }
                        }
                    }
                    return true;
                }
            }
        }
        false
    }

    fn should_check_as_excess_property(&self, prop: Rc<Symbol>, container: Rc<Symbol>) -> bool {
        if let Some(prop_value_declaration) = prop.maybe_value_declaration().as_ref() {
            if let Some(container_value_declaration) = container.maybe_value_declaration().as_ref()
            {
                return Rc::ptr_eq(
                    &prop_value_declaration.upgrade().unwrap().parent(),
                    &container_value_declaration.upgrade().unwrap(),
                );
            }
        }
        false
    }

    fn type_related_to_some_type(
        &self,
        source: Rc<Type>,
        target: &UnionOrIntersectionType,
    ) -> Ternary {
        let target_types = target.types();

        for type_ in target_types {
            let related = self.is_related_to(
                source.clone(),
                type_.clone(),
                Some(RecursionFlags::Target),
                false,
                None,
                None,
            );
            if related != Ternary::False {
                return related;
            }
        }

        Ternary::False
    }

    fn each_type_related_to_type(
        &self,
        source: &UnionOrIntersectionType,
        target: Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let mut result = Ternary::True;
        let source_types = source.types();
        for source_type in source_types {
            let related = self.is_related_to(
                source_type.clone(),
                target.clone(),
                Some(RecursionFlags::Source),
                report_errors,
                None,
                Some(intersection_state),
            );
            if related == Ternary::False {
                return Ternary::False;
            }
            result &= related;
        }
        result
    }

    fn recursive_type_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
        recursion_flags: RecursionFlags,
    ) -> Ternary {
        let result = if self.expanding_flags != ExpandingFlags::Both {
            self.structured_type_related_to(source, target, report_errors, intersection_state)
        } else {
            Ternary::Maybe
        };
        result
    }

    fn structured_type_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let result = self.structured_type_related_to_worker(
            source,
            target,
            report_errors,
            intersection_state,
        );
        result
    }

    fn structured_type_related_to_worker(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        if intersection_state.intersects(IntersectionState::UnionIntersectionCheck) {
            if source.flags().intersects(TypeFlags::Union) {
                return if false {
                    unimplemented!()
                } else {
                    self.each_type_related_to_type(
                        match &*source {
                            Type::UnionOrIntersectionType(union_or_intersection_type) => {
                                union_or_intersection_type
                            }
                            _ => panic!("Expected UnionOrIntersectionType"),
                        },
                        target,
                        report_errors,
                        intersection_state & !IntersectionState::UnionIntersectionCheck,
                    )
                };
            }
            if target.flags().intersects(TypeFlags::Union) {
                return self.type_related_to_some_type(
                    self.type_checker.get_regular_type_of_object_literal(source),
                    match &*target {
                        Type::UnionOrIntersectionType(union_or_intersection_type) => {
                            union_or_intersection_type
                        }
                        _ => panic!("Expected UnionOrIntersectionType"),
                    },
                );
            }
            unimplemented!()
        }

        let result: Ternary;

        if false {
            unimplemented!()
        } else {
            if source
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::Intersection)
                && target.flags().intersects(TypeFlags::Object)
            {
                let report_structural_errors = report_errors && true;
                result = self.properties_related_to(
                    source,
                    target,
                    report_structural_errors,
                    None,
                    intersection_state,
                );
                if false && result != Ternary::False {
                    unimplemented!()
                } else if result != Ternary::False {
                    return result;
                }
            }
        }

        Ternary::False
    }

    fn exclude_properties(
        &self,
        properties: Vec<Rc<Symbol>>,
        excluded_properties: Option<HashSet<__String>>,
    ) -> Vec<Rc<Symbol>> {
        properties
    }

    fn is_property_symbol_type_related(
        &self,
        source_prop: Rc<Symbol>,
        target_prop: Rc<Symbol>,
        get_type_of_source_property: fn(&TypeChecker, Rc<Symbol>) -> Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let target_is_optional = false;
        let effective_target = self.type_checker.add_optionality(
            self.type_checker
                .get_non_missing_type_of_symbol(target_prop),
            Some(false),
            Some(target_is_optional),
        );
        let effective_source = get_type_of_source_property(self.type_checker, source_prop);
        self.is_related_to(
            effective_source,
            effective_target,
            Some(RecursionFlags::Both),
            report_errors,
            None,
            Some(intersection_state),
        )
    }

    fn property_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        source_prop: Rc<Symbol>,
        target_prop: Rc<Symbol>,
        get_type_of_source_property: fn(&TypeChecker, Rc<Symbol>) -> Rc<Type>,
        report_errors: bool,
        intersection_state: IntersectionState,
        skip_optional: bool,
    ) -> Ternary {
        let related = self.is_property_symbol_type_related(
            source_prop,
            target_prop.clone(),
            get_type_of_source_property,
            report_errors,
            intersection_state,
        );
        if related == Ternary::False {
            if report_errors {
                self.report_incompatible_error(
                    Diagnostics::Types_of_property_0_are_incompatible,
                    Some(vec![self.type_checker.symbol_to_string(
                        target_prop,
                        None,
                        None,
                        None,
                        None,
                    )]),
                );
            }
            return Ternary::False;
        }
        related
    }

    fn properties_related_to(
        &self,
        source: Rc<Type>,
        target: Rc<Type>,
        report_errors: bool,
        excluded_properties: Option<HashSet<__String>>,
        intersection_state: IntersectionState,
    ) -> Ternary {
        let mut result = Ternary::True;
        let require_optional_properties = false;
        // let unmatched_property =
        //     self.get_unmatched_property(source, target, require_optional_properties, false);
        // if let Some(unmatched_property) = unmatched_property {
        //     if report_errors {
        //         self.report_unmatched_property(
        //             source,
        //             target,
        //             unmatched_property,
        //             require_optional_properties,
        //         );
        //     }
        //     return Ternary::False;
        // }
        let properties = self.type_checker.get_properties_of_type(target.clone());
        for target_prop in self.exclude_properties(properties, excluded_properties) {
            let name = &target_prop.escaped_name;
            if true {
                let source_prop = self.type_checker.get_property_of_type(source.clone(), name);
                if let Some(source_prop) = source_prop {
                    if !Rc::ptr_eq(&source_prop, &target_prop) {
                        let related = self.property_related_to(
                            source.clone(),
                            target.clone(),
                            source_prop,
                            target_prop,
                            TypeChecker::get_non_missing_type_of_symbol,
                            report_errors,
                            intersection_state,
                            true,
                        );
                        if related == Ternary::False {
                            return Ternary::False;
                        }
                        result &= related;
                    }
                }
            }
        }
        result
    }
}
