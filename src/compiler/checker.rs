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
    create_text_writer, every, factory, for_each, get_effective_initializer,
    get_effective_type_annotation_node, get_first_identifier, get_synthetic_factory,
    has_dynamic_name, has_initializer, is_binding_element, is_external_or_common_js_module,
    is_object_literal_expression, is_private_identifier, is_property_assignment,
    is_property_declaration, is_property_signature, is_variable_declaration, node_is_missing,
    object_allocator, ArrayTypeNode, BaseInterfaceType, BaseIntrinsicType, BaseLiteralType,
    BaseObjectType, BaseType, BaseUnionOrIntersectionType, CharacterCodes, Debug_, Diagnostic,
    DiagnosticCollection, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, EmitHint,
    EmitTextWriter, Expression, ExpressionStatement, FreshableIntrinsicType,
    HasExpressionInitializerInterface, InterfaceDeclaration, InterfaceType, IntrinsicType,
    KeywordTypeNode, LiteralLikeNode, LiteralLikeNodeInterface, LiteralTypeInterface,
    NamedDeclarationInterface, Node, NodeInterface, Number, NumberLiteralType, NumericLiteral,
    ObjectFlags, ObjectLiteralExpression, ObjectTypeInterface, PrefixUnaryExpression,
    PrinterOptions, PropertyAssignment, PropertySignature, RelationComparisonResult,
    ResolvableTypeInterface, ResolvedTypeInterface, SourceFile, Statement, Symbol, SymbolFlags,
    SymbolTable, SymbolTracker, SyntaxKind, Ternary, Type, TypeChecker, TypeCheckerHost,
    TypeElement, TypeFlags, TypeInterface, TypeReferenceNode, VariableLikeDeclarationInterface,
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

        number_literal_types: RefCell::new(HashMap::new()),

        unknown_symbol: None,

        number_type: None,
        bigint_type: None,
        true_type: None,
        regular_true_type: None,
        false_type: None,
        regular_false_type: None,
        boolean_type: None,
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
    type_checker.boolean_type = Some(type_checker.get_union_type(vec![
        type_checker.regular_false_type(),
        type_checker.regular_true_type(),
    ]));
    type_checker.number_or_big_int_type = Some(
        type_checker.get_union_type(vec![type_checker.number_type(), type_checker.bigint_type()]),
    );
    type_checker.initialize_type_checker(host);
    type_checker
}

impl TypeChecker {
    fn globals(&self) -> RefMut<SymbolTable> {
        self.globals.borrow_mut()
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

    fn symbol_to_string(&self, symbol: Rc<Symbol>) -> String {
        unimplemented!()
    }

    fn type_to_string(&self, type_: &Type) -> String {
        let writer = Rc::new(RefCell::new(create_text_writer("")));
        let type_node = self
            .node_builder
            .type_to_type_node(type_, Some(&*(*writer).borrow()));
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

    fn get_type_names_for_error_display(&self, left: &Type, right: &Type) -> (String, String) {
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

    fn get_type_name_for_error_display(&self, type_: &Type) -> String {
        self.type_to_string(type_)
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

    fn has_bindable_name<TNode: NodeInterface>(&self, node: &TNode /*Declaration*/) -> bool {
        !has_dynamic_name(node) || unimplemented!()
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
                .map(|rc| rc.clone());
            if let Some(symbol) = symbol {
                if self.symbol_is_value(symbol.clone()) {
                    return Some(symbol);
                }
            }
            unimplemented!()
        }
        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            unimplemented!()
        }
        None
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

    fn add_type_to_union(&self, type_set: &mut Vec<Rc<Type>>, type_: Rc<Type>) {
        type_set.push(type_);
    }

    fn add_types_to_union(&self, type_set: &mut Vec<Rc<Type>>, types: &[Rc<Type>]) {
        for type_ in types {
            self.add_type_to_union(type_set, type_.clone());
        }
    }

    fn get_union_type(&self, types: Vec<Rc<Type>>) -> Rc<Type> {
        let mut type_set: Vec<Rc<Type>> = vec![];
        self.add_types_to_union(&mut type_set, /*TypeFlags::empty(), */ &types);
        self.get_union_type_from_sorted_list(type_set)
    }

    fn get_union_type_from_sorted_list(&self, types: Vec<Rc<Type>>) -> Rc<Type> {
        let mut type_: Option<Rc<Type>> = None;
        if type_.is_none() {
            let base_type = self.create_type(TypeFlags::Union);
            type_ = Some(Rc::new(
                (UnionType {
                    _union_or_intersection_type: BaseUnionOrIntersectionType {
                        _type: base_type,
                        types,
                    },
                })
                .into(),
            ));
        }
        type_.unwrap()
    }

    // pub fn create_literal_type(
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
        if true {
            return self.check_type_related_to(source, target, relation, error_node, head_message);
        }
        false
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
                |type_| Some(self.get_base_type_of_literal_type(type_)),
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
                |type_| Some(self.get_widened_literal_type(type_)),
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

    fn map_type<TMapper: FnMut(Rc<Type>) -> Option<Rc<Type>>>(
        &self,
        type_: Rc<Type>,
        mut mapper: TMapper,
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
        unimplemented!()
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
            |t| {
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
                    unimplemented!()
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
                        |type_| Some(self.get_apparent_type(type_)),
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

    fn check_expression_for_mutable_location(&self, node: &Expression) -> Rc<Type> {
        let type_ = self.check_expression(node);
        if false {
            unimplemented!()
        } else {
            self.get_widened_literal_like_type_for_contextual_type(
                type_,
                self.instantiate_contextual_type(
                    if true {
                        self.get_contextual_type(node)
                    } else {
                        unimplemented!()
                    },
                    node,
                ),
            )
        }
    }

    fn check_property_assignment(&self, node: &PropertyAssignment) -> Rc<Type> {
        self.check_expression_for_mutable_location(match &*node.initializer {
            Node::Expression(expression) => expression,
            _ => panic!("Expected Expression"),
        })
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

pub struct NodeBuilder {}

impl NodeBuilder {
    pub fn new() -> Self {
        Self {}
    }

    pub fn type_to_type_node(
        &self,
        type_: &Type,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<TypeNode> {
        self.with_context(tracker, |context| {
            self.type_to_type_node_helper(type_, context)
        })
    }

    fn with_context<TReturn, TCallback: FnMut(&NodeBuilderContext) -> TReturn>(
        &self,
        tracker: Option<&dyn SymbolTracker>,
        mut cb: TCallback,
    ) -> Option<TReturn> {
        let context = NodeBuilderContext::new(tracker.unwrap());
        let resulting_node = cb(&context);
        Some(resulting_node)
    }

    pub fn type_to_type_node_helper(&self, type_: &Type, context: &NodeBuilderContext) -> TypeNode {
        let synthetic_factory = get_synthetic_factory();
        if type_.flags().intersects(TypeFlags::Number) {
            return Into::<KeywordTypeNode>::into(
                factory.create_keyword_type_node(&synthetic_factory, SyntaxKind::NumberKeyword),
            )
            .into();
        }
        if type_.flags().intersects(TypeFlags::BooleanLiteral) {
            return factory
                .create_literal_type_node(
                    &synthetic_factory,
                    &*Into::<Rc<Node>>::into(
                        if type_.as_intrinsic_type().intrinsic_name() == "true" {
                            factory.create_true(&synthetic_factory)
                        } else {
                            factory.create_false(&synthetic_factory)
                        },
                    ),
                )
                .into();
        }
        unimplemented!()
    }
}

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
            .get_type_names_for_error_display(&*source, &*target);
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
                    .is_type_assignable_to(generalized_source, target),
                Some("generalized source shouldn't be assignable"),
            );
            generalized_source_type = self.type_checker.get_type_name_for_error_display(&*source);
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
        let target = self.type_checker.get_normalized_type(original_target);

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

        if self.type_checker.is_simple_type_related_to(
            &*source,
            &*target,
            self.relation,
            if report_errors {
                Some(&report_error)
            } else {
                None
            },
        ) {
            return Ternary::True;
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
        if related != Ternary::False {
            if report_errors {
                self.report_incompatible_error(
                    Diagnostics::Types_of_property_0_are_incompatible,
                    Some(vec![self.type_checker.symbol_to_string(target_prop)]),
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
