#![allow(non_upper_case_globals)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{MappedTypeModifiers, MembersOrExportsResolutionKind};
use crate::{
    concatenate, create_symbol_table, escape_leading_underscores, every,
    get_interface_base_type_nodes, has_dynamic_name, is_entity_name_expression, is_type_alias,
    range_equals_rc, BaseInterfaceType, Debug_, Diagnostics, GenericableTypeInterface,
    InterfaceTypeInterface, InterfaceTypeWithDeclaredMembersInterface, LiteralType, Node,
    NodeFlags, NodeInterface, ObjectFlags, ObjectFlagsTypeInterface, Signature, SignatureFlags,
    Symbol, SymbolFlags, SymbolInterface, SymbolTable, SyntaxKind, TransientSymbolInterface, Type,
    TypeChecker, TypeFlags, TypeInterface, TypeMapper, TypePredicate, TypeReferenceInterface,
    UnderscoreEscapedMap, __String,
};

impl TypeChecker {
    pub(super) fn are_all_outer_type_parameters_applied(&self, type_: &Type) -> bool {
        let outer_type_parameters = type_
            .maybe_as_interface_type()
            .and_then(|type_| type_.maybe_outer_type_parameters());
        if let Some(outer_type_parameters) = outer_type_parameters {
            let last = outer_type_parameters.len() - 1;
            let type_arguments = self.get_type_arguments(type_);
            return !match (
                outer_type_parameters[last].maybe_symbol(),
                type_arguments[last].maybe_symbol(),
            ) {
                (None, None) => true,
                (Some(symbol_a), Some(symbol_b)) => Rc::ptr_eq(&symbol_a, &symbol_b),
                _ => false,
            };
        }
        true
    }

    pub(super) fn is_valid_base_type(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::TypeParameter) {
            if type_.flags().intersects(TypeFlags::TypeParameter) {
                let constraint = self.get_base_constraint_of_type(type_);
                if let Some(constraint) = constraint {
                    return self.is_valid_base_type(&constraint);
                }
            }
        }
        type_
            .flags()
            .intersects(TypeFlags::Object | TypeFlags::NonPrimitive | TypeFlags::Any)
            && !self.is_generic_mapped_type(type_)
            || type_.flags().intersects(TypeFlags::Intersection)
                && every(
                    type_.as_union_or_intersection_type_interface().types(),
                    |type_: &Rc<Type>, _| self.is_valid_base_type(type_),
                )
    }

    pub(super) fn resolve_base_types_of_interface(&self, type_: &Type /*InterfaceType*/) {
        let type_as_interface_type = type_.as_interface_type();
        if type_as_interface_type.maybe_resolved_base_types().is_none() {
            *type_as_interface_type.maybe_resolved_base_types() = Some(Rc::new(vec![]));
        }
        if let Some(type_symbol_declarations) = type_.symbol().maybe_declarations().as_deref() {
            for declaration in type_symbol_declarations {
                if declaration.kind() == SyntaxKind::InterfaceDeclaration
                    && get_interface_base_type_nodes(declaration).is_some()
                {
                    for node in get_interface_base_type_nodes(declaration)
                        .as_deref()
                        .unwrap()
                    {
                        let base_type = self.get_reduced_type(&self.get_type_from_type_node_(node));
                        if !self.is_error_type(&base_type) {
                            if self.is_valid_base_type(&base_type) {
                                if !ptr::eq(type_, &*base_type)
                                    && !self.has_base_type(&base_type, Some(type_))
                                {
                                    let mut resolved_base_types = Vec::clone(
                                        type_as_interface_type
                                            .maybe_resolved_base_types()
                                            .as_ref()
                                            .unwrap(),
                                    );
                                    resolved_base_types.push(base_type);
                                    *type_as_interface_type.maybe_resolved_base_types() =
                                        Some(Rc::new(resolved_base_types));
                                } else {
                                    self.report_circular_base_type(declaration, type_);
                                }
                            } else {
                                self.error(
                                    Some(&**node),
                                    &Diagnostics::An_interface_can_only_extend_an_object_type_or_intersection_of_object_types_with_statically_known_members,
                                    None,
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    pub(super) fn is_thisless_interface(&self, symbol: &Symbol) -> bool {
        let symbol_declarations = symbol.maybe_declarations();
        if symbol_declarations.is_none() {
            return true;
        }
        let symbol_declarations = symbol_declarations.as_deref().unwrap();
        for declaration in symbol_declarations {
            if declaration.kind() == SyntaxKind::InterfaceDeclaration {
                if declaration.flags().intersects(NodeFlags::ContainsThis) {
                    return false;
                }
                let base_type_nodes = get_interface_base_type_nodes(declaration);
                if let Some(base_type_nodes) = base_type_nodes {
                    for node in &*base_type_nodes {
                        let node_as_expression_with_type_arguments =
                            node.as_expression_with_type_arguments();
                        if is_entity_name_expression(
                            &node_as_expression_with_type_arguments.expression,
                        ) {
                            let base_symbol = self.resolve_entity_name(
                                &node_as_expression_with_type_arguments.expression,
                                SymbolFlags::Type,
                                Some(true),
                                None,
                                Option::<&Node>::None,
                            );
                            if match base_symbol {
                                None => true,
                                Some(base_symbol) => {
                                    !base_symbol.flags().intersects(SymbolFlags::Interface)
                                        || self
                                            .get_declared_type_of_class_or_interface(&base_symbol)
                                            .as_interface_type()
                                            .maybe_this_type()
                                            .is_some()
                                }
                            } {
                                return false;
                            }
                        }
                    }
                }
            }
        }
        true
    }

    pub(super) fn get_declared_type_of_class_or_interface(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type /*InterfaceType*/> {
        let mut links = self.get_symbol_links(symbol);
        let original_links = links.clone();
        let mut symbol = symbol.symbol_wrapper();
        if (*links).borrow().declared_type.is_none() {
            let kind = if symbol.flags().intersects(SymbolFlags::Class) {
                ObjectFlags::Class
            } else {
                ObjectFlags::Interface
            };
            let merged = self.merge_js_symbols(
                &symbol,
                symbol
                    .maybe_value_declaration()
                    .as_ref()
                    .and_then(|value_declaration| {
                        self.get_assigned_class_symbol(value_declaration)
                    }),
            );
            if let Some(merged) = merged {
                symbol = merged.clone();
                links = merged.as_transient_symbol().symbol_links();
            }

            let type_ = self.create_object_type(kind, Some(&*symbol));
            let outer_type_parameters =
                self.get_outer_type_parameters_of_class_or_interface(&symbol);
            let local_type_parameters =
                self.get_local_type_parameters_of_class_or_interface_or_type_alias(&symbol);
            let mut need_to_set_constraint = false;
            let type_: Rc<Type> = if outer_type_parameters.is_some()
                || local_type_parameters.is_some()
                || kind == ObjectFlags::Class
                || !self.is_thisless_interface(&symbol)
            {
                need_to_set_constraint = true;
                type_.set_object_flags(type_.object_flags() | ObjectFlags::Reference);
                let mut this_type = self.create_type_parameter(Some(symbol.clone()));
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
            let type_as_interface_type = type_.as_interface_type();
            if need_to_set_constraint {
                *type_as_interface_type
                    .maybe_this_type_mut()
                    .as_ref()
                    .unwrap()
                    .as_type_parameter()
                    .constraint
                    .borrow_mut() = Some(Rc::downgrade(&type_));
            }
            let mut instantiations: HashMap<String, Rc<Type /*TypeReference*/>> = HashMap::new();
            instantiations.insert(
                self.get_type_list_id(type_as_interface_type.maybe_type_parameters()),
                type_.clone(),
            );
            type_as_interface_type.genericize(instantiations);
            type_as_interface_type.set_target(type_.clone());
            *type_as_interface_type.maybe_resolved_type_arguments() = type_as_interface_type
                .maybe_type_parameters()
                .map(ToOwned::to_owned);
            original_links.borrow_mut().declared_type = Some(type_.clone());
            links.borrow_mut().declared_type = Some(type_.clone());
        }
        let ret = (*links).borrow().declared_type.clone().unwrap();
        ret
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
                |type_node| self.get_type_from_type_node_(&type_node),
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

    pub(super) fn get_base_type_of_enum_literal_type(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_declared_type_of_enum_member(&self, symbol: &Symbol) -> Rc<Type> {
        unimplemented!()
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

    pub(super) fn get_resolved_members_or_exports_of_symbol(
        &self,
        symbol: &Symbol,
        resolution_kind: MembersOrExportsResolutionKind,
    ) -> Rc<RefCell<UnderscoreEscapedMap<Rc<Symbol>>>> {
        unimplemented!()
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

    pub(super) fn get_late_bound_symbol(&self, symbol: &Symbol) -> Rc<Symbol> {
        symbol.symbol_wrapper()
    }

    pub(super) fn resolve_object_type_members(
        &self,
        type_: &Type,  /*ObjectType*/
        source: &Type, /*InterfaceTypeWithDeclaredMembers*/
        type_parameters: Vec<Rc<Type /*TypeParameter*/>>,
        type_arguments: Vec<Rc<Type>>,
    ) {
        let mut mapper: Option<TypeMapper> = None;
        let members: Rc<RefCell<SymbolTable>>;
        let call_signatures: Vec<Rc<Signature>>;
        let construct_signatures: Vec<Rc<Signature>>;
        let source_as_interface_type_with_declared_members =
            source.as_interface_type_with_declared_members();
        if range_equals_rc(&type_parameters, &type_arguments, 0, type_parameters.len()) {
            members = if let Some(source_symbol) = source.maybe_symbol() {
                self.get_members_of_symbol(&source_symbol)
            } else {
                unimplemented!()
            };
            call_signatures = source_as_interface_type_with_declared_members
                .declared_call_signatures()
                .clone();
            construct_signatures = source_as_interface_type_with_declared_members
                .declared_construct_signatures()
                .clone();
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
            call_signatures = self.instantiate_signatures(
                &*source_as_interface_type_with_declared_members.declared_call_signatures(),
                mapper.as_ref().unwrap(),
            );
            construct_signatures = self.instantiate_signatures(
                &*source_as_interface_type_with_declared_members.declared_construct_signatures(),
                mapper.as_ref().unwrap(),
            );
        }
        self.set_structured_type_members(
            type_.as_object_type(),
            members,
            call_signatures,
            construct_signatures,
            vec![], // TODO: this is wrong
        );
    }

    pub(super) fn resolve_class_or_interface_members(&self, type_: &Type /*InterfaceType*/) {
        self.resolve_object_type_members(
            type_,
            &self.resolve_declared_members(type_),
            vec![],
            vec![],
        );
    }

    pub(super) fn resolve_type_reference_members(&self, type_: &Type /*TypeReference*/) {
        let type_as_type_reference = type_.as_type_reference();
        let source = self.resolve_declared_members(&type_as_type_reference.target);
        let source_as_interface_type = source.as_interface_type();
        let type_parameters = concatenate(
            source_as_interface_type
                .maybe_type_parameters()
                .map(|type_parameters| type_parameters.to_owned())
                .unwrap(),
            vec![source_as_interface_type.maybe_this_type().unwrap()],
        );
        let type_arguments = self.get_type_arguments(type_);
        let padded_type_arguments = if type_arguments.len() == type_parameters.len() {
            type_arguments
        } else {
            concatenate(type_arguments, vec![type_.type_wrapper()])
        };
        self.resolve_object_type_members(type_, &source, type_parameters, padded_type_arguments);
    }

    pub(super) fn create_signature(
        &self,
        declaration: Option<Rc<Node>>,
        type_parameters: Option<Vec<Rc<Type>>>,
        this_parameter: Option<Rc<Symbol>>,
        parameters: Vec<Rc<Symbol>>,
        resolved_return_type: Option<Rc<Type>>,
        resolved_type_predicate: Option<TypePredicate>,
        min_argument_count: usize,
        flags: SignatureFlags,
    ) -> Signature {
        let mut sig = (self.Signature)(flags);
        sig.declaration = declaration;
        sig.type_parameters = type_parameters;
        sig.set_parameters(parameters);
        sig.this_parameter = this_parameter;
        *sig.resolved_return_type.borrow_mut() = resolved_return_type;
        sig.resolved_type_predicate = resolved_type_predicate;
        sig.set_min_argument_count(min_argument_count);
        sig
    }

    pub(super) fn create_union_signature(
        &self,
        signature: Rc<Signature>,
        union_signatures: &[Rc<Signature>],
    ) -> Signature {
        unimplemented!()
    }

    pub(super) fn get_type_of_mapped_symbol(
        &self,
        symbol: &Symbol, /*MappedSymbol*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_parameter_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_constraint_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_name_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_template_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_mapped_type_with_keyof_constraint_declaration(
        &self,
        type_: &Type, /*MappedType*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_modifiers_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_mapped_type_modifiers(
        &self,
        type_: &Type, /*MappedType*/
    ) -> MappedTypeModifiers {
        unimplemented!()
    }

    pub(super) fn is_generic_mapped_type(&self, type_: &Type) -> bool {
        unimplemented!()
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

    pub(super) fn for_each_property_of_type<TAction: FnMut(&Symbol, &__String)>(
        &self,
        type_: &Type,
        action: TAction,
    ) {
        unimplemented!()
    }

    pub(super) fn get_constraint_of_type_parameter(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> Option<Rc<Type>> {
        if self.has_non_circular_base_constraint(type_parameter) {
            self.get_constraint_from_type_parameter(type_parameter)
        } else {
            None
        }
    }

    pub(super) fn get_base_constraint_of_type(&self, type_: &Type) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_base_constraint_or_type(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn has_non_circular_base_constraint(
        &self,
        type_: &Type, /*InstantiableType*/
    ) -> bool {
        !Rc::ptr_eq(
            &self.get_resolved_base_constraint(type_),
            &self.circular_constraint_type(),
        )
    }

    pub(super) fn get_resolved_base_constraint(
        &self,
        type_: &Type, /*InstantiableType | UnionOrIntersectionType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_default_from_type_parameter_(
        &self,
        type_: &Type, /*TypeParameter*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }
}
