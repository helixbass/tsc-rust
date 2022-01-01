#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::ptr;
use std::rc::Rc;

use super::NodeBuilderContext;
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

impl TypeChecker {
    pub(crate) fn get_name_of_symbol_as_written(
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

    pub(crate) fn add_optionality(
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

    pub(crate) fn get_type_for_variable_like_declaration(
        &self,
        declaration: &Node,
    ) -> Option<Rc<Type>> {
        let is_property =
            is_property_declaration(declaration) || is_property_signature(declaration);
        let is_optional = false;

        let declared_type = self.try_get_type_from_effective_type_node(declaration);
        if let Some(declared_type) = declared_type {
            return Some(self.add_optionality(declared_type, Some(is_property), Some(is_optional)));
        }
        unimplemented!()
    }

    pub(crate) fn get_widened_type_for_variable_like_declaration(
        &self,
        declaration: &Node,
    ) -> Rc<Type> {
        self.widen_type_for_variable_like_declaration(
            self.get_type_for_variable_like_declaration(declaration),
            declaration,
        )
    }

    pub(crate) fn widen_type_for_variable_like_declaration(
        &self,
        type_: Option<Rc<Type>>,
        declaration: &Node,
    ) -> Rc<Type> {
        if let Some(type_) = type_ {
            return self.get_widened_type(type_);
        }
        unimplemented!()
    }

    pub(crate) fn try_get_type_from_effective_type_node(
        &self,
        declaration: &Node, /*Declaration*/
    ) -> Option<Rc<Type>> {
        let type_node = get_effective_type_annotation_node(declaration);
        type_node.map(|type_node| self.get_type_from_type_node(&*type_node))
    }

    pub(crate) fn get_type_of_variable_or_parameter_or_property(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type> {
        // let links = self.get_symbol_links(symbol);
        // if links.type_.is_none() {
        self.get_type_of_variable_or_parameter_or_property_worker(symbol)
        // }
        // links.type.unwrap().clone()
    }

    pub(crate) fn get_type_of_variable_or_parameter_or_property_worker(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type> {
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

    pub(crate) fn get_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        if symbol
            .flags()
            .intersects(SymbolFlags::Variable | SymbolFlags::Property)
        {
            return self.get_type_of_variable_or_parameter_or_property(symbol);
        }
        unimplemented!()
    }

    pub(crate) fn get_non_missing_type_of_symbol(&self, symbol: Rc<Symbol>) -> Rc<Type> {
        self.remove_missing_type(
            self.get_type_of_symbol(&*symbol),
            symbol.flags().intersects(SymbolFlags::Optional),
        )
    }

    pub(crate) fn get_declared_type_of_class_or_interface(
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

    pub(crate) fn get_declared_type_of_symbol(&self, symbol: Rc<Symbol>) -> Rc<Type> {
        self.try_get_declared_type_of_symbol(symbol)
            .unwrap_or_else(|| unimplemented!())
    }

    pub(crate) fn try_get_declared_type_of_symbol(&self, symbol: Rc<Symbol>) -> Option<Rc<Type>> {
        if symbol
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface)
        {
            return Some(self.get_declared_type_of_class_or_interface(symbol));
        }
        unimplemented!()
    }

    pub(crate) fn resolve_declared_members(&self, type_: Rc<Type /*InterfaceType*/>) -> Rc<Type> {
        type_
    }

    pub(crate) fn is_type_usable_as_property_name(&self, type_: Rc<Type>) -> bool {
        type_
            .flags()
            .intersects(TypeFlags::StringOrNumberLiteralOrUnique)
    }

    pub(crate) fn has_bindable_name<TNode: NodeInterface>(
        &self,
        node: &TNode, /*Declaration*/
    ) -> bool {
        !has_dynamic_name(node) || unimplemented!()
    }

    pub(crate) fn get_property_name_from_type(
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

    pub(crate) fn get_members_of_symbol(&self, symbol: Rc<Symbol>) -> Rc<RefCell<SymbolTable>> {
        if false {
            unimplemented!()
        } else {
            symbol.members()
        }
    }

    pub(crate) fn resolve_object_type_members(
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

    pub(crate) fn resolve_class_or_interface_members(&self, type_: Rc<Type /*InterfaceType*/>) {
        self.resolve_object_type_members(type_.clone(), self.resolve_declared_members(type_));
    }

    pub(crate) fn resolve_structured_type_members(
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

    pub(crate) fn get_properties_of_object_type(&self, type_: Rc<Type>) -> Vec<Rc<Symbol>> {
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

    pub(crate) fn get_property_of_object_type(
        &self,
        type_: Rc<Type>,
        name: &__String,
    ) -> Option<Rc<Symbol>> {
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

    pub(crate) fn get_properties_of_type(&self, type_: Rc<Type>) -> Vec<Rc<Symbol>> {
        let type_ = self.get_reduced_apparent_type(type_);
        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            unimplemented!()
        } else {
            self.get_properties_of_object_type(type_)
        }
    }

    pub(crate) fn get_apparent_type(&self, type_: Rc<Type>) -> Rc<Type> {
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

    pub(crate) fn get_reduced_apparent_type(&self, type_: Rc<Type>) -> Rc<Type> {
        self.get_reduced_type(self.get_apparent_type(self.get_reduced_type(type_)))
    }

    pub(crate) fn get_reduced_type(&self, type_: Rc<Type>) -> Rc<Type> {
        type_
    }

    pub(crate) fn get_property_of_type(
        &self,
        type_: Rc<Type>,
        name: &__String,
    ) -> Option<Rc<Symbol>> {
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

    pub(crate) fn get_propagating_flags_of_types(
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

    pub(crate) fn get_type_from_class_or_interface_reference<TNode: NodeInterface>(
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

    pub(crate) fn get_type_reference_name(
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

    pub(crate) fn resolve_type_reference_name(
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

    pub(crate) fn get_type_reference_type<TNode: NodeInterface>(
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

    pub(crate) fn get_conditional_flow_type_of_type(
        &self,
        type_: Rc<Type>,
        node: &Node,
    ) -> Rc<Type> {
        type_
    }

    pub(crate) fn get_type_from_type_reference(&self, node: &TypeReferenceNode) -> Rc<Type> {
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

    pub(crate) fn get_type_of_global_symbol(
        &self,
        symbol: Option<Rc<Symbol>>,
    ) -> Rc<Type /*ObjectType*/> {
        unimplemented!()
    }

    pub(crate) fn get_global_type_symbol(
        &self,
        name: &__String,
        report_errors: bool,
    ) -> Option<Rc<Symbol>> {
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

    pub(crate) fn get_global_symbol(
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

    pub(crate) fn get_global_type(&self, name: &__String, report_errors: bool) -> Option<Rc<Type>> {
        let symbol = self.get_global_type_symbol(name, report_errors);
        if true {
            Some(self.get_type_of_global_symbol(symbol))
        } else {
            None
        }
    }

    pub(crate) fn get_array_or_tuple_target_type(
        &self,
        node: &ArrayTypeNode,
    ) -> Rc<Type /*GenericType*/> {
        let element_type = self.get_array_element_type_node(node);
        if let Some(element_type) = element_type {
            return self.global_array_type();
        }
        unimplemented!()
    }

    pub(crate) fn get_type_from_array_or_tuple_type_node(&self, node: &ArrayTypeNode) -> Rc<Type> {
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

    pub(crate) fn create_normalized_type_reference(
        &self,
        target: Rc<Type /*GenericType*/>,
        type_arguments: Option<Vec<Rc<Type>>>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(crate) fn add_type_to_union(
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

    pub(crate) fn add_types_to_union(
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

    pub(crate) fn get_union_type(
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

    pub(crate) fn get_union_type_from_sorted_list(
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

    pub(crate) fn get_literal_type_from_property_name(
        &self,
        name: &Node, /*PropertyName*/
    ) -> Rc<Type> {
        if let Node::Expression(Expression::Identifier(identifier)) = name {
            self.get_string_literal_type(&unescape_leading_underscores(&identifier.escaped_text))
        } else {
            unimplemented!()
        }
    }

    pub(crate) fn get_literal_type_from_property(
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

    pub(crate) fn get_property_name_from_index(&self, index_type: Rc<Type>) -> Option<__String> {
        if self.is_type_usable_as_property_name(index_type.clone()) {
            Some(self.get_property_name_from_type(index_type))
        } else {
            unimplemented!()
        }
    }

    pub(crate) fn get_property_type_for_index_type(
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

    pub(crate) fn get_indexed_access_type_or_undefined(
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
}
