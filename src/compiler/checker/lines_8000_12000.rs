#![allow(non_upper_case_globals)]

use std::cell::RefCell;
use std::rc::Rc;

use super::NodeBuilderContext;
use crate::{
    __String, append_if_unique, concatenate, create_symbol_table, declaration_name_to_string,
    escape_leading_underscores, first_defined, get_declaration_of_kind,
    get_effective_type_annotation_node, get_effective_type_parameter_declarations,
    get_name_of_declaration, has_dynamic_name, is_property_assignment, is_property_declaration,
    is_property_signature, is_variable_declaration, range_equals, BaseInterfaceType, Debug_,
    InterfaceType, InterfaceTypeWithDeclaredMembersInterface, LiteralType, Node, NodeInterface,
    ObjectFlags, ObjectFlagsTypeInterface, ObjectType, Symbol, SymbolFlags, SymbolTable,
    SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
};

impl TypeChecker {
    pub(super) fn get_name_of_symbol_as_written(
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

    pub(super) fn add_optionality(
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

    pub(super) fn get_type_for_variable_like_declaration(
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

    pub(super) fn get_widened_type_for_variable_like_declaration(
        &self,
        declaration: &Node,
    ) -> Rc<Type> {
        self.widen_type_for_variable_like_declaration(
            self.get_type_for_variable_like_declaration(declaration),
            declaration,
        )
    }

    pub(super) fn widen_type_for_variable_like_declaration(
        &self,
        type_: Option<Rc<Type>>,
        declaration: &Node,
    ) -> Rc<Type> {
        if let Some(type_) = type_ {
            return self.get_widened_type(type_);
        }
        unimplemented!()
    }

    pub(super) fn try_get_type_from_effective_type_node(
        &self,
        declaration: &Node, /*Declaration*/
    ) -> Option<Rc<Type>> {
        let type_node = get_effective_type_annotation_node(declaration);
        type_node.map(|type_node| self.get_type_from_type_node(&*type_node))
    }

    pub(super) fn get_type_of_variable_or_parameter_or_property(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type> {
        // let links = self.get_symbol_links(symbol);
        // if links.type_.is_none() {
        self.get_type_of_variable_or_parameter_or_property_worker(symbol)
        // }
        // links.type.unwrap().clone()
    }

    pub(super) fn get_type_of_variable_or_parameter_or_property_worker(
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

    pub(super) fn get_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        if symbol
            .flags()
            .intersects(SymbolFlags::Variable | SymbolFlags::Property)
        {
            return self.get_type_of_variable_or_parameter_or_property(symbol);
        }
        unimplemented!()
    }

    pub(super) fn get_non_missing_type_of_symbol(&self, symbol: Rc<Symbol>) -> Rc<Type> {
        self.remove_missing_type(
            self.get_type_of_symbol(&*symbol),
            symbol.flags().intersects(SymbolFlags::Optional),
        )
    }

    pub(super) fn append_type_parameters(
        &self,
        mut type_parameters: Option<Vec<Rc<Type>>>,
        declarations: &[Rc<Node>],
    ) -> Option<Vec<Rc<Type>>> {
        for declaration in declarations {
            type_parameters = Some(append_if_unique(
                type_parameters,
                self.get_declared_type_of_type_parameter(
                    self.get_symbol_of_node(&**declaration).unwrap(),
                ),
            ));
        }
        type_parameters
    }

    pub(super) fn get_outer_type_parameters(
        &self,
        node: &Node,
    ) -> Option<Vec<Rc<Type /*TypeParameter*/>>> {
        None
    }

    pub(super) fn get_outer_type_parameters_of_class_or_interface(
        &self,
        symbol: Rc<Symbol>,
    ) -> Option<Vec<Rc<Type /*TypeParameter*/>>> {
        let declaration = if symbol.flags().intersects(SymbolFlags::Class) {
            symbol
                .maybe_value_declaration()
                .as_ref()
                .map(|weak| weak.upgrade().unwrap())
        } else {
            get_declaration_of_kind(symbol, SyntaxKind::InterfaceDeclaration)
        };
        Debug_.assert(
            declaration.is_some(),
            Some("Class was missing valueDeclaration -OR- non-class had no interface declarations"),
        );
        let declaration = declaration.unwrap();
        self.get_outer_type_parameters(&*declaration)
    }

    pub(super) fn get_local_type_parameters_of_class_or_interface_or_type_alias(
        &self,
        symbol: Rc<Symbol>,
    ) -> Option<Vec<Rc<Type /*TypeParameter*/>>> {
        let declarations = symbol.maybe_declarations();
        if declarations.is_none() {
            return None;
        }
        let declarations = declarations.as_ref().unwrap();
        let mut result: Option<Vec<Rc<Type /*TypeParameter*/>>> = None;
        for node in declarations {
            if node.kind() == SyntaxKind::InterfaceDeclaration
                || node.kind() == SyntaxKind::ClassDeclaration
                || node.kind() == SyntaxKind::ClassExpression
                || false
            {
                let declaration = node;
                result = self.append_type_parameters(
                    result,
                    &get_effective_type_parameter_declarations(&*declaration),
                );
            }
        }
        result
    }

    pub(super) fn get_declared_type_of_class_or_interface(
        &self,
        symbol: Rc<Symbol>,
    ) -> Rc<Type /*InterfaceType*/> {
        let kind = if symbol.flags().intersects(SymbolFlags::Class) {
            ObjectFlags::Class
        } else {
            ObjectFlags::Interface
        };

        let mut type_ = self.create_object_type(kind, symbol.clone());
        let outer_type_parameters =
            self.get_outer_type_parameters_of_class_or_interface(symbol.clone());
        let local_type_parameters =
            self.get_local_type_parameters_of_class_or_interface_or_type_alias(symbol);
        let type_: InterfaceType = if outer_type_parameters.is_some()
            || local_type_parameters.is_some()
            || kind == ObjectFlags::Class
            || false
        {
            type_.set_object_flags(type_.object_flags() | ObjectFlags::Reference);
            BaseInterfaceType::new(
                type_,
                Some(concatenate(
                    outer_type_parameters.clone().unwrap_or_else(|| vec![]),
                    local_type_parameters.clone().unwrap_or_else(|| vec![]),
                )),
                outer_type_parameters,
                local_type_parameters,
            )
        } else {
            BaseInterfaceType::new(type_, None, None, None)
        }
        .into();
        Rc::new(type_.into())
    }

    pub(super) fn get_declared_type_of_type_parameter(
        &self,
        symbol: Rc<Symbol>,
    ) -> Rc<Type /*TypeParameter*/> {
        Rc::new(self.create_type_parameter(Some(symbol)).into())
    }

    pub(super) fn get_declared_type_of_symbol(&self, symbol: Rc<Symbol>) -> Rc<Type> {
        self.try_get_declared_type_of_symbol(symbol)
            .unwrap_or_else(|| unimplemented!())
    }

    pub(super) fn try_get_declared_type_of_symbol(&self, symbol: Rc<Symbol>) -> Option<Rc<Type>> {
        if symbol
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface)
        {
            return Some(self.get_declared_type_of_class_or_interface(symbol));
        }
        if symbol.flags().intersects(SymbolFlags::TypeParameter) {
            return Some(self.get_declared_type_of_type_parameter(symbol));
        }
        unimplemented!()
    }

    pub(super) fn create_instantiated_symbol_table(
        &self,
        symbols: &[Rc<Symbol>],
        mapper: &TypeMapper,
        mapping_this_only: bool,
    ) -> SymbolTable {
        let mut result = create_symbol_table();
        for symbol in symbols {
            result.insert(
                symbol.escaped_name.clone(),
                if mapping_this_only && true {
                    symbol.clone()
                } else {
                    self.instantiate_symbol(symbol.clone(), mapper)
                },
            );
        }
        result
    }

    pub(super) fn resolve_declared_members(&self, type_: Rc<Type /*InterfaceType*/>) -> Rc<Type> {
        let type_as_interface_type = match &*type_ {
            Type::ObjectType(ObjectType::InterfaceType(interface_type)) => interface_type,
            _ => panic!("Expected InterfaceType"),
        };
        if type_as_interface_type.maybe_declared_properties().is_none() {
            let symbol = type_.symbol();
            let members = self.get_members_of_symbol(symbol);
            type_as_interface_type
                .set_declared_properties(self.get_named_members(&*members.borrow()));
        }
        type_
    }

    pub(super) fn is_type_usable_as_property_name(&self, type_: Rc<Type>) -> bool {
        type_
            .flags()
            .intersects(TypeFlags::StringOrNumberLiteralOrUnique)
    }

    pub(super) fn has_bindable_name<TNode: NodeInterface>(
        &self,
        node: &TNode, /*Declaration*/
    ) -> bool {
        !has_dynamic_name(node) || unimplemented!()
    }

    pub(super) fn get_property_name_from_type(
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

    pub(super) fn get_members_of_symbol(&self, symbol: Rc<Symbol>) -> Rc<RefCell<SymbolTable>> {
        if false {
            unimplemented!()
        } else {
            symbol
                .maybe_members()
                .clone()
                .unwrap_or_else(|| unimplemented!())
        }
    }

    pub(super) fn resolve_object_type_members(
        &self,
        type_: Rc<Type /*ObjectType*/>,
        source: Rc<Type /*InterfaceTypeWithDeclaredMembers*/>,
        type_parameters: Vec<Rc<Type /*TypeParameter*/>>,
        type_arguments: Vec<Rc<Type>>,
    ) {
        let members: Rc<RefCell<SymbolTable>>;
        if range_equals(&type_parameters, &type_arguments, 0, type_parameters.len()) {
            members = if let Some(source_symbol) = source.maybe_symbol() {
                self.get_members_of_symbol(source_symbol)
            } else {
                unimplemented!()
            };
        } else {
            let type_parameters_len_is_1 = type_parameters.len() == 1;
            let mapper = self.create_type_mapper(type_parameters, Some(type_arguments));
            members = Rc::new(RefCell::new(
                self.create_instantiated_symbol_table(
                    match &*source {
                        Type::ObjectType(ObjectType::InterfaceType(
                            InterfaceType::BaseInterfaceType(base_interface_type),
                        )) => base_interface_type,
                        _ => panic!("Expected BaseInterfaceType"),
                    }
                    .maybe_declared_properties()
                    .as_ref()
                    .unwrap(),
                    &mapper,
                    type_parameters_len_is_1,
                ),
            ));
        }
        self.set_structured_type_members(
            match &*type_ {
                Type::ObjectType(object_type) => object_type,
                _ => panic!("Expected ObjectType"),
            },
            members,
        );
    }

    pub(super) fn resolve_type_reference_members(&self, type_: Rc<Type /*TypeReference*/>) {
        let type_as_type_reference = match &*type_ {
            Type::ObjectType(ObjectType::TypeReference(type_reference)) => type_reference,
            _ => panic!("Expected TypeReference"),
        };
        let source = self.resolve_declared_members(type_as_type_reference.target.clone());
        let type_parameters = /*concatenate(*/match &*source {
            Type::ObjectType(ObjectType::InterfaceType(InterfaceType::BaseInterfaceType(base_interface_type))) => base_interface_type,
            _ => panic!("Expected BaseInterfaceType"),
        }.type_parameters.clone().unwrap()/*, [source.thisType!])*/;
        let type_arguments = self.get_type_arguments(type_as_type_reference);
        let padded_type_arguments = if type_arguments.len() == type_parameters.len() {
            type_arguments
        } else {
            concatenate(type_arguments, vec![type_.clone()])
        };
        self.resolve_object_type_members(type_, source, type_parameters, padded_type_arguments);
    }

    pub(super) fn resolve_class_or_interface_members(&self, type_: Rc<Type /*InterfaceType*/>) {
        self.resolve_object_type_members(
            type_.clone(),
            self.resolve_declared_members(type_),
            vec![],
            vec![],
        );
    }

    pub(super) fn resolve_structured_type_members(
        &self,
        type_: Rc<Type /*StructuredType*/>,
    ) -> Rc<Type /*ResolvedType*/> {
        if !type_.as_resolvable_type().is_resolved() {
            if let Type::ObjectType(object_type) = &*type_
            /*type_.flags().intersects(TypeFlags::Object)*/
            {
                if object_type
                    .object_flags()
                    .intersects(ObjectFlags::Reference)
                {
                    self.resolve_type_reference_members(type_.clone());
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

    pub(super) fn get_properties_of_object_type(&self, type_: Rc<Type>) -> Vec<Rc<Symbol>> {
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

    pub(super) fn get_property_of_object_type(
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

    pub(super) fn get_properties_of_type(&self, type_: Rc<Type>) -> Vec<Rc<Symbol>> {
        let type_ = self.get_reduced_apparent_type(type_);
        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            unimplemented!()
        } else {
            self.get_properties_of_object_type(type_)
        }
    }
}
