#![allow(non_upper_case_globals)]

use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use super::{MappedTypeModifiers, MembersOrExportsResolutionKind};
use crate::{
    concatenate, create_symbol_table, declaration_name_to_string, escape_leading_underscores,
    get_assignment_declaration_kind, get_check_flags, get_members_of_declaration,
    get_name_of_declaration, has_dynamic_name, has_static_modifier, is_binary_expression,
    is_element_access_expression, maybe_concatenate, maybe_for_each, range_equals_rc,
    unescape_leading_underscores, AssignmentDeclarationKind, CheckFlags, Debug_, Diagnostics,
    InterfaceTypeInterface, InterfaceTypeWithDeclaredMembersInterface, LiteralType, Node,
    NodeInterface, ObjectFlags, ObjectFlagsTypeInterface, Signature, SignatureFlags, Symbol,
    SymbolFlags, SymbolInterface, SymbolLinks, SymbolTable, TransientSymbolInterface, Type,
    TypeChecker, TypeFlags, TypeInterface, TypeMapper, TypePredicate, UnderscoreEscapedMap,
    __String,
};

impl TypeChecker {
    pub(super) fn has_late_bindable_name(&self, node: &Node /*Declaration*/) -> bool {
        let name = get_name_of_declaration(Some(node));
        match name {
            None => false,
            Some(name) => self.is_late_bindable_name(&name),
        }
    }

    pub(super) fn has_bindable_name(&self, node: &Node /*Declaration*/) -> bool {
        !has_dynamic_name(node) || self.has_late_bindable_name(node)
    }

    pub(super) fn get_property_name_from_type(
        &self,
        type_: &Type, /*StringLiteralType | NumberLiteralType | UniqueESSymbolType*/
    ) -> __String {
        if type_.flags().intersects(TypeFlags::UniqueESSymbol) {
            return type_.as_unique_es_symbol_type().escaped_name.clone();
        }
        if type_
            .flags()
            .intersects(TypeFlags::StringLiteral | TypeFlags::NumberLiteral)
        {
            return match type_ {
                Type::LiteralType(LiteralType::NumberLiteralType(type_)) => {
                    escape_leading_underscores(&type_.value.to_string())
                }
                Type::LiteralType(LiteralType::StringLiteralType(type_)) => {
                    escape_leading_underscores(&type_.value)
                }
                _ => panic!("Expected NumberLiteralType or StringLiteralType"),
            };
        }
        Debug_.fail(None)
    }

    pub(super) fn add_declaration_to_late_bound_symbol(
        &self,
        symbol: &Symbol,
        member: &Node, /*LateBoundDeclaration | BinaryExpression*/
        symbol_flags: SymbolFlags,
    ) {
        Debug_.assert(
            get_check_flags(symbol).intersects(CheckFlags::Late),
            Some("Expected a late-bound symbol."),
        );
        symbol.set_flags(symbol.flags() | symbol_flags);
        self.get_symbol_links(&member.symbol())
            .borrow_mut()
            .late_symbol = Some(symbol.symbol_wrapper());
        if symbol.maybe_declarations().is_none() {
            symbol.set_declarations(vec![member.node_wrapper()]);
        } else if !matches!(member.symbol().maybe_is_replaceable_by_method(), Some(true)) {
            symbol
                .maybe_declarations_mut()
                .as_mut()
                .unwrap()
                .push(member.node_wrapper());
        }
        if symbol_flags.intersects(SymbolFlags::Value) {
            if match symbol.maybe_value_declaration().as_ref() {
                None => true,
                Some(value_declaration) => value_declaration.kind() != member.kind(),
            } {
                symbol.set_value_declaration(member.node_wrapper());
            }
        }
    }

    pub(super) fn late_bind_member(
        &self,
        parent: &Symbol,
        early_symbols: Option<&SymbolTable>,
        late_symbols: &mut UnderscoreEscapedMap<Rc<Symbol /*TransientSymbol*/>>,
        decl: &Node, /*LateBoundDeclaration | LateBoundBinaryExpressionDeclaration*/
    ) -> Rc<Symbol> {
        Debug_.assert(
            decl.maybe_symbol().is_some(),
            Some("The member is expected to have a symbol."),
        );
        let links = self.get_node_links(decl);
        if (*links).borrow().resolved_symbol.is_none() {
            links.borrow_mut().resolved_symbol = decl.maybe_symbol();
            let decl_name = if is_binary_expression(decl) {
                decl.as_binary_expression().left.clone()
            } else {
                decl.as_named_declaration().name()
            };
            let type_ = if is_element_access_expression(&decl_name) {
                self.check_expression_cached(
                    &decl_name.as_element_access_expression().argument_expression,
                    None,
                )
            } else {
                self.check_computed_property_name(&decl_name)
            };
            if self.is_type_usable_as_property_name(&type_) {
                let member_name = self.get_property_name_from_type(&type_);
                let symbol_flags = decl.symbol().flags();

                let mut late_symbol: Option<Rc<Symbol>> =
                    late_symbols.get(&member_name).map(Clone::clone);
                if late_symbol.is_none() {
                    late_symbol = Some(
                        self.create_symbol(
                            SymbolFlags::None,
                            member_name.clone(),
                            Some(CheckFlags::Late),
                        )
                        .into(),
                    );
                    late_symbols.insert(member_name.clone(), late_symbol.clone().unwrap());
                }
                let mut late_symbol = late_symbol.unwrap();

                let early_symbol =
                    early_symbols.and_then(|early_symbols| early_symbols.get(&member_name));
                if late_symbol
                    .flags()
                    .intersects(self.get_excluded_symbol_flags(symbol_flags))
                    || early_symbol.is_some()
                {
                    let declarations = if let Some(early_symbol) = early_symbol.as_ref() {
                        maybe_concatenate(
                            early_symbol.maybe_declarations().clone(),
                            late_symbol.maybe_declarations().clone(),
                        )
                    } else {
                        late_symbol.maybe_declarations().clone()
                    };
                    let name = if !type_.flags().intersects(TypeFlags::UniqueESSymbol) {
                        unescape_leading_underscores(&member_name)
                    } else {
                        declaration_name_to_string(Some(&*decl_name)).into_owned()
                    };
                    maybe_for_each(declarations.as_deref(), |declaration: &Rc<Node>, _| {
                        self.error(
                            Some(
                                get_name_of_declaration(Some(&**declaration))
                                    .unwrap_or_else(|| declaration.clone()),
                            ),
                            &Diagnostics::Property_0_was_also_declared_here,
                            Some(vec![name.clone()]),
                        );
                        Option::<()>::None
                    });
                    self.error(
                        Some(decl_name), /*|| decl*/
                        &Diagnostics::Duplicate_property_0,
                        Some(vec![name]),
                    );
                    late_symbol = self
                        .create_symbol(SymbolFlags::None, member_name, Some(CheckFlags::Late))
                        .into();
                }
                late_symbol
                    .as_transient_symbol()
                    .symbol_links()
                    .borrow_mut()
                    .name_type = Some(type_.type_wrapper());
                self.add_declaration_to_late_bound_symbol(&late_symbol, decl, symbol_flags);
                if let Some(late_symbol_parent) = late_symbol.maybe_parent() {
                    Debug_.assert(
                        ptr::eq(&*late_symbol_parent, parent),
                        Some("Existing symbol parent should match new one"),
                    );
                } else {
                    late_symbol.set_parent(Some(parent.symbol_wrapper()));
                }
                links.borrow_mut().resolved_symbol = Some(late_symbol.clone());
                return late_symbol;
            }
        }
        let ret = (*links).borrow().resolved_symbol.clone().unwrap();
        ret
    }

    pub(super) fn get_resolved_members_or_exports_of_symbol(
        &self,
        symbol: &Symbol,
        resolution_kind: MembersOrExportsResolutionKind,
    ) -> Rc<RefCell<UnderscoreEscapedMap<Rc<Symbol>>>> {
        let links = self.get_symbol_links(symbol);
        if self
            .get_symbol_links_members_or_exports_resolution_field_value(&links, resolution_kind)
            .is_none()
        {
            let is_static = resolution_kind == MembersOrExportsResolutionKind::resolved_exports;
            let early_symbols: Option<Rc<RefCell<SymbolTable>>> = if !is_static {
                symbol.maybe_members().clone()
            } else if symbol.flags().intersects(SymbolFlags::Module) {
                Some(self.get_exports_of_module_worker(symbol))
            } else {
                symbol.maybe_exports().clone()
            };

            self.set_symbol_links_members_or_exports_resolution_field_value(
                &links,
                resolution_kind,
                Some(
                    early_symbols
                        .clone()
                        .unwrap_or_else(|| self.empty_symbols()),
                ),
            );

            let mut late_symbols = create_symbol_table(None);
            let early_symbols_ref = early_symbols
                .as_ref()
                .map(|early_symbols| (*early_symbols).borrow());
            if let Some(symbol_declarations) = symbol.maybe_declarations().as_deref() {
                for decl in symbol_declarations {
                    let members = get_members_of_declaration(decl);
                    if let Some(members) = members {
                        for member in &members {
                            if is_static == has_static_modifier(member)
                                && self.has_late_bindable_name(member)
                            {
                                self.late_bind_member(
                                    symbol,
                                    early_symbols_ref.as_deref(),
                                    &mut late_symbols,
                                    member,
                                );
                            }
                        }
                    }
                }
                let assignments = symbol.maybe_assignment_declaration_members();
                if let Some(assignments) = assignments.as_ref() {
                    let decls = assignments.values();
                    for member in decls {
                        let assignment_kind = get_assignment_declaration_kind(member);
                        let is_instance_member = assignment_kind
                            == AssignmentDeclarationKind::PrototypeProperty
                            || is_binary_expression(member)
                                && self.is_possibly_aliased_this_property(
                                    member,
                                    Some(assignment_kind),
                                )
                            || matches!(
                                assignment_kind,
                                AssignmentDeclarationKind::ObjectDefinePrototypeProperty
                                    | AssignmentDeclarationKind::Prototype
                            );
                        if is_static != is_instance_member && self.has_late_bindable_name(member) {
                            self.late_bind_member(
                                symbol,
                                early_symbols_ref.as_deref(),
                                &mut late_symbols,
                                member,
                            );
                        }
                    }
                }

                self.set_symbol_links_members_or_exports_resolution_field_value(
                    &links,
                    resolution_kind,
                    Some(
                        self.combine_symbol_tables(
                            early_symbols.clone(),
                            Some(Rc::new(RefCell::new(late_symbols))),
                        )
                        .unwrap_or_else(|| self.empty_symbols()),
                    ),
                )
            }
        }
        self.get_symbol_links_members_or_exports_resolution_field_value(&links, resolution_kind)
            .unwrap()
    }

    pub(super) fn get_symbol_links_members_or_exports_resolution_field_value(
        &self,
        symbol_links: &RefCell<SymbolLinks>,
        resolution_kind: MembersOrExportsResolutionKind,
    ) -> Option<Rc<RefCell<SymbolTable>>> {
        match resolution_kind {
            MembersOrExportsResolutionKind::resolved_exports => {
                symbol_links.borrow().resolved_exports.clone()
            }
            MembersOrExportsResolutionKind::resolved_members => {
                symbol_links.borrow().resolved_members.clone()
            }
        }
    }

    pub(super) fn set_symbol_links_members_or_exports_resolution_field_value(
        &self,
        symbol_links: &RefCell<SymbolLinks>,
        resolution_kind: MembersOrExportsResolutionKind,
        value: Option<Rc<RefCell<SymbolTable>>>,
    ) {
        match resolution_kind {
            MembersOrExportsResolutionKind::resolved_exports => {
                symbol_links.borrow_mut().resolved_exports = value;
            }
            MembersOrExportsResolutionKind::resolved_members => {
                symbol_links.borrow_mut().resolved_members = value;
            }
        }
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
