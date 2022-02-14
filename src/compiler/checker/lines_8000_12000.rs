#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::rc::Rc;

use super::NodeBuilderContext;
use crate::{
    __String, append_if_unique, concatenate, create_symbol_table, declaration_name_to_string,
    escape_leading_underscores, first_defined, get_check_flags, get_declaration_of_kind,
    get_effective_type_annotation_node, get_effective_type_parameter_declarations,
    get_name_of_declaration, has_dynamic_name, has_only_expression_initializer,
    is_property_assignment, is_property_declaration, is_property_signature, is_type_alias,
    is_variable_declaration, range_equals, BaseInterfaceType, CheckFlags, Debug_, InterfaceType,
    InterfaceTypeWithDeclaredMembersInterface, LiteralType, Node, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, Symbol, SymbolFlags, SymbolInterface, SymbolTable, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeInterface, TypeMapper, UnionOrIntersectionType,
    UnionOrIntersectionTypeInterface,
};

impl TypeChecker {
    pub(super) fn format_union_types(&self, types: &[Rc<Type>]) -> Vec<Rc<Type>> {
        let mut result: Vec<Rc<Type>> = vec![];
        let mut flags: TypeFlags = TypeFlags::None;
        let mut i = 0;
        while i < types.len() {
            let t = types[i].clone();
            flags |= t.flags();
            if !t.flags().intersects(TypeFlags::Nullable) {
                if t.flags()
                    .intersects(TypeFlags::BooleanLiteral | TypeFlags::EnumLiteral)
                {
                    let base_type = if t.flags().intersects(TypeFlags::BooleanLiteral) {
                        self.boolean_type()
                    } else {
                        unimplemented!()
                    };
                    if let Type::UnionOrIntersectionType(UnionOrIntersectionType::UnionType(
                        base_type_as_union_type,
                    )) = &*base_type
                    {
                        let count = base_type_as_union_type.types().len();
                        if i + count <= types.len()
                            && Rc::ptr_eq(
                                &self.get_regular_type_of_literal_type(&*types[i + count - 1]),
                                &self.get_regular_type_of_literal_type(
                                    &*base_type_as_union_type.types()[count - 1],
                                ),
                            )
                        {
                            result.push(base_type);
                            i += count - 1;
                            continue;
                        }
                    }
                }
                result.push(t.clone());
            }
            i += 1;
        }
        result /*|| types*/
    }

    pub(super) fn get_name_of_symbol_as_written(
        &self,
        symbol: &Symbol,
        context: Option<&NodeBuilderContext>,
    ) -> Cow<'static, str> {
        if let Some(declarations) = &*symbol.maybe_declarations() {
            if !declarations.is_empty() {
                let declaration = first_defined(declarations, |d, _| {
                    if get_name_of_declaration(Some(&**d)).is_some() {
                        Some(d)
                    } else {
                        None
                    }
                });
                let name = if let Some(declaration) = declaration {
                    get_name_of_declaration(Some(&**declaration))
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
        type_: &Type,
        is_property: Option<bool>,
        is_optional: Option<bool>,
    ) -> Rc<Type> {
        let is_property = is_property.unwrap_or(false);
        let is_optional = is_optional.unwrap_or(true);
        if self.strict_null_checks && is_optional {
            self.get_optional_type(type_, Some(is_property))
        } else {
            type_.type_wrapper()
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
            return Some(self.add_optionality(
                &declared_type,
                Some(is_property),
                Some(is_optional),
            ));
        }

        if has_only_expression_initializer(declaration)
            && declaration
                .as_has_initializer()
                .maybe_initializer()
                .is_some()
        {
            let type_ = self.check_declaration_initializer(declaration, Option::<&Type>::None);
            let type_ = self.widen_type_inferred_from_initializer(declaration, &type_);
            return Some(self.add_optionality(&type_, Some(is_property), Some(is_optional)));
        }

        None
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

    pub(super) fn widen_type_for_variable_like_declaration<TTypeRef: Borrow<Type>>(
        &self,
        type_: Option<TTypeRef>,
        declaration: &Node,
    ) -> Rc<Type> {
        if let Some(type_) = type_ {
            return self.get_widened_type(type_.borrow());
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
        let links = self.get_symbol_links(symbol);
        let links_type_is_none = { (*links).borrow().type_.is_none() };
        if links_type_is_none {
            let type_ = self.get_type_of_variable_or_parameter_or_property_worker(symbol);
            let mut links_ref = links.borrow_mut();
            if links_ref.type_.is_none() {
                links_ref.type_ = Some(type_);
            }
        }
        let links_ref = (*links).borrow();
        links_ref.type_.clone().unwrap()
    }

    pub(super) fn get_type_of_variable_or_parameter_or_property_worker(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type> {
        Debug_.assert_is_defined(&symbol.maybe_value_declaration(), None);
        let declaration = symbol.maybe_value_declaration().unwrap();

        let type_: Rc<Type>;
        if false {
            unimplemented!()
        } else if is_property_assignment(&declaration) {
            type_ = self
                .try_get_type_from_effective_type_node(&declaration)
                .unwrap_or_else(|| self.check_property_assignment(&declaration, None));
        } else if is_property_signature(&declaration) || is_variable_declaration(&declaration) {
            type_ = self.get_widened_type_for_variable_like_declaration(&declaration);
        } else {
            unimplemented!()
        }

        type_
    }

    pub(super) fn get_type_of_instantiated_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        let mut links = links.borrow_mut();
        if links.type_.is_none() {
            let type_ = self.instantiate_type(
                Some(self.get_type_of_symbol(links.target.as_ref().unwrap())),
                links.mapper.as_ref(),
            );
            links.type_ = type_;
        }
        links.type_.clone().unwrap()
    }

    pub(super) fn get_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        let check_flags = get_check_flags(symbol);
        if check_flags.intersects(CheckFlags::Instantiated) {
            return self.get_type_of_instantiated_symbol(symbol);
        }
        if symbol
            .flags()
            .intersects(SymbolFlags::Variable | SymbolFlags::Property)
        {
            return self.get_type_of_variable_or_parameter_or_property(symbol);
        }
        unimplemented!()
    }

    pub(super) fn get_non_missing_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        self.remove_missing_type(
            &self.get_type_of_symbol(symbol),
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
                    &self.get_symbol_of_node(&**declaration).unwrap(),
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
        symbol: &Symbol,
    ) -> Option<Vec<Rc<Type /*TypeParameter*/>>> {
        let declaration = if symbol.flags().intersects(SymbolFlags::Class) {
            symbol.maybe_value_declaration()
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
        symbol: &Symbol,
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
                || is_type_alias(&**node)
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
        symbol: &Symbol,
    ) -> Rc<Type /*InterfaceType*/> {
        let links = self.get_symbol_links(symbol);
        let original_links = links.clone();
        let mut original_links_ref = original_links.borrow_mut();
        if original_links_ref.declared_type.is_none() {
            let kind = if symbol.flags().intersects(SymbolFlags::Class) {
                ObjectFlags::Class
            } else {
                ObjectFlags::Interface
            };

            let type_ = self.create_object_type(kind, symbol);
            let outer_type_parameters =
                self.get_outer_type_parameters_of_class_or_interface(symbol);
            let local_type_parameters =
                self.get_local_type_parameters_of_class_or_interface_or_type_alias(symbol);
            let mut need_to_set_constraint = false;
            let type_: InterfaceType = if outer_type_parameters.is_some()
                || local_type_parameters.is_some()
                || kind == ObjectFlags::Class
                || false
            {
                need_to_set_constraint = true;
                type_.set_object_flags(type_.object_flags() | ObjectFlags::Reference);
                let mut this_type = self.create_type_parameter(Some(symbol.symbol_wrapper()));
                this_type.is_this_type = Some(true);
                BaseInterfaceType::new(
                    type_,
                    Some(concatenate(
                        outer_type_parameters.clone().unwrap_or_else(|| vec![]),
                        local_type_parameters.clone().unwrap_or_else(|| vec![]),
                    )),
                    outer_type_parameters,
                    local_type_parameters,
                    Some(this_type.into()),
                )
            } else {
                BaseInterfaceType::new(type_, None, None, None, None)
            }
            .into();
            let type_rc: Rc<Type> = type_.into();
            if need_to_set_constraint {
                *type_rc
                    .as_base_interface_type()
                    .this_type
                    .borrow_mut()
                    .as_ref()
                    .unwrap()
                    .as_type_parameter()
                    .constraint
                    .borrow_mut() = Some(Rc::downgrade(&type_rc));
            }
            original_links_ref.declared_type = Some(type_rc.clone());
            if !Rc::ptr_eq(&links, &original_links) {
                let mut links_ref = links.borrow_mut();
                links_ref.declared_type = Some(type_rc);
            }
        }
        original_links_ref.declared_type.clone().unwrap()
    }

    pub(super) fn get_declared_type_of_type_alias(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        let mut links = links.borrow_mut();
        if links.declared_type.is_none() {
            let declaration = Debug_.check_defined(
                symbol
                    .maybe_declarations()
                    .as_ref()
                    .and_then(|declarations| {
                        declarations
                            .iter()
                            .find(|declaration| is_type_alias(&***declaration))
                            .map(|rc| rc.clone())
                    }),
                None,
            );
            let type_node = if false {
                unimplemented!()
            } else {
                Some(declaration.as_type_alias_declaration().type_.clone())
            };
            let type_ = type_node.map_or_else(
                || self.error_type(),
                |type_node| self.get_type_from_type_node(&type_node),
            );
            if true {
                let type_parameters =
                    self.get_local_type_parameters_of_class_or_interface_or_type_alias(symbol);
                if let Some(type_parameters) = type_parameters {
                    unimplemented!()
                }
            } else {
                unimplemented!()
            }
            links.declared_type = Some(type_);
        }
        links.declared_type.clone().unwrap()
    }

    pub(super) fn get_declared_type_of_type_parameter(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type /*TypeParameter*/> {
        let links = self.get_symbol_links(symbol);
        let mut links = links.borrow_mut();
        if links.declared_type.is_none() {
            links.declared_type = Some(
                self.create_type_parameter(Some(symbol.symbol_wrapper()))
                    .into(),
            );
        }
        links.declared_type.clone().unwrap()
    }

    pub(super) fn get_declared_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        self.try_get_declared_type_of_symbol(symbol)
            .unwrap_or_else(|| unimplemented!())
    }

    pub(super) fn try_get_declared_type_of_symbol(&self, symbol: &Symbol) -> Option<Rc<Type>> {
        if symbol
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface)
        {
            return Some(self.get_declared_type_of_class_or_interface(symbol));
        }
        if symbol.flags().intersects(SymbolFlags::TypeAlias) {
            return Some(self.get_declared_type_of_type_alias(symbol));
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
        let mut result = create_symbol_table(None);
        for symbol in symbols {
            result.insert(
                symbol.escaped_name().clone(),
                if mapping_this_only && true {
                    symbol.clone()
                } else {
                    self.instantiate_symbol(&symbol, mapper)
                },
            );
        }
        result
    }

    pub(super) fn resolve_declared_members(&self, type_: &Type /*InterfaceType*/) -> Rc<Type> {
        let type_as_interface_type = type_.as_interface_type();
        if type_as_interface_type.maybe_declared_properties().is_none() {
            let symbol = type_.symbol();
            let members = self.get_members_of_symbol(&symbol);
            type_as_interface_type
                .set_declared_properties(self.get_named_members(&*(*members).borrow()));
        }
        type_.type_wrapper()
    }

    pub(super) fn is_type_usable_as_property_name(&self, type_: &Type) -> bool {
        type_
            .flags()
            .intersects(TypeFlags::StringOrNumberLiteralOrUnique)
    }

    pub(super) fn has_bindable_name(&self, node: &Node /*Declaration*/) -> bool {
        !has_dynamic_name(node) || unimplemented!()
    }

    pub(super) fn get_property_name_from_type(
        &self,
        type_: &Type, /*StringLiteralType | NumberLiteralType | UniqueESSymbolType*/
    ) -> __String {
        if type_
            .flags()
            .intersects(TypeFlags::StringLiteral | TypeFlags::NumberLiteral)
        {
            return match type_ {
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

    pub(super) fn get_members_of_symbol(&self, symbol: &Symbol) -> Rc<RefCell<SymbolTable>> {
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
        type_: &Type,  /*ObjectType*/
        source: &Type, /*InterfaceTypeWithDeclaredMembers*/
        type_parameters: Vec<Rc<Type /*TypeParameter*/>>,
        type_arguments: Vec<Rc<Type>>,
    ) {
        let members: Rc<RefCell<SymbolTable>>;
        let mut mapper: Option<TypeMapper> = None;
        if range_equals(&type_parameters, &type_arguments, 0, type_parameters.len()) {
            members = if let Some(source_symbol) = source.maybe_symbol() {
                self.get_members_of_symbol(&source_symbol)
            } else {
                unimplemented!()
            };
        } else {
            let type_parameters_len_is_1 = type_parameters.len() == 1;
            mapper = Some(self.create_type_mapper(type_parameters, Some(type_arguments)));
            members = Rc::new(RefCell::new(
                self.create_instantiated_symbol_table(
                    source
                        .as_base_interface_type()
                        .maybe_declared_properties()
                        .as_ref()
                        .unwrap(),
                    mapper.as_ref().unwrap(),
                    type_parameters_len_is_1,
                ),
            ));
        }
        self.set_structured_type_members(type_.as_object_type(), members);
    }

    pub(super) fn resolve_type_reference_members(&self, type_: &Type /*TypeReference*/) {
        let type_as_type_reference = type_.as_type_reference();
        let source = self.resolve_declared_members(&type_as_type_reference.target);
        let source_as_base_interface_type = source.as_base_interface_type();
        let type_parameters = concatenate(
            source_as_base_interface_type
                .type_parameters
                .clone()
                .unwrap(),
            vec![source_as_base_interface_type
                .this_type
                .borrow()
                .clone()
                .unwrap()],
        );
        let type_arguments = self.get_type_arguments(type_as_type_reference);
        let padded_type_arguments = if type_arguments.len() == type_parameters.len() {
            type_arguments
        } else {
            concatenate(type_arguments, vec![type_.type_wrapper()])
        };
        self.resolve_object_type_members(type_, &source, type_parameters, padded_type_arguments);
    }

    pub(super) fn resolve_class_or_interface_members(&self, type_: &Type /*InterfaceType*/) {
        self.resolve_object_type_members(
            type_,
            &self.resolve_declared_members(type_),
            vec![],
            vec![],
        );
    }

    pub(super) fn resolve_structured_type_members(
        &self,
        type_: &Type, /*StructuredType*/
    ) -> Rc<Type /*ResolvedType*/> {
        if !type_.as_resolvable_type().is_resolved() {
            if let Type::ObjectType(object_type) = &*type_
            /*type_.flags().intersects(TypeFlags::Object)*/
            {
                if object_type
                    .object_flags()
                    .intersects(ObjectFlags::Reference)
                {
                    self.resolve_type_reference_members(type_);
                } else if object_type
                    .object_flags()
                    .intersects(ObjectFlags::ClassOrInterface)
                {
                    self.resolve_class_or_interface_members(type_);
                } else {
                    unimplemented!()
                }
            } else {
                unimplemented!()
            }
        }
        type_.type_wrapper()
    }

    pub(super) fn get_properties_of_object_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
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
        type_: &Type,
        name: &__String,
    ) -> Option<Rc<Symbol>> {
        if type_.flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(type_);
            let symbol = (*resolved.as_resolved_type().members())
                .borrow()
                .get(name)
                .map(Clone::clone);
            if let Some(symbol) = symbol {
                if self.symbol_is_value(&symbol) {
                    return Some(symbol);
                }
            }
        }
        None
    }

    pub(super) fn get_properties_of_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        let type_ = self.get_reduced_apparent_type(type_);
        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            unimplemented!()
        } else {
            self.get_properties_of_object_type(&type_)
        }
    }
}
