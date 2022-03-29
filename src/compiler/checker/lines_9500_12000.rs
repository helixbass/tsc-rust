#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use super::{MappedTypeModifiers, MembersOrExportsResolutionKind};
use crate::{
    concatenate, create_symbol_table, escape_leading_underscores, find, get_check_flags,
    get_declaration_of_kind, get_effective_type_annotation_node,
    get_effective_type_parameter_declarations, get_object_flags, has_dynamic_name,
    is_access_expression, is_export_assignment, is_shorthand_ambient_module_symbol, is_source_file,
    is_type_alias, maybe_first_defined, range_equals_rc, some, BaseInterfaceType, CheckFlags,
    Debug_, Diagnostics, InterfaceType, InterfaceTypeInterface,
    InterfaceTypeWithDeclaredMembersInterface, InternalSymbolName, LiteralType, Node,
    NodeInterface, ObjectFlags, ObjectFlagsTypeInterface, Signature, SignatureFlags, Symbol,
    SymbolFlags, SymbolInterface, SymbolTable, SyntaxKind, TransientSymbolInterface, Type,
    TypeChecker, TypeFlags, TypeInterface, TypeMapper, TypePredicate, TypeSystemPropertyName,
    UnderscoreEscapedMap, __String, maybe_append_if_unique_rc,
};

impl TypeChecker {
    pub(super) fn get_base_type_variable_of_class(&self, symbol: &Symbol) -> Option<Rc<Type>> {
        let base_constructor_type = self.get_base_constructor_type_of_class(
            &self.get_declared_type_of_class_or_interface(symbol),
        );
        if base_constructor_type
            .flags()
            .intersects(TypeFlags::TypeVariable)
        {
            Some(base_constructor_type)
        } else if base_constructor_type
            .flags()
            .intersects(TypeFlags::Intersection)
        {
            find(
                base_constructor_type
                    .as_union_or_intersection_type_interface()
                    .types(),
                |t: &Rc<Type>, _| t.flags().intersects(TypeFlags::TypeVariable),
            )
            .map(Clone::clone)
        } else {
            None
        }
    }

    pub(super) fn get_type_of_func_class_enum_module(&self, symbol: &Symbol) -> Rc<Type> {
        let mut links = self.get_symbol_links(symbol);
        let original_links = links.clone();
        let mut symbol = symbol.symbol_wrapper();
        if (*links).borrow().type_.is_none() {
            let expando = symbol
                .maybe_value_declaration()
                .and_then(|value_declaration| {
                    self.get_symbol_of_expando(&value_declaration, false)
                });
            if let Some(expando) = expando {
                let merged = self.merge_js_symbols(&symbol, Some(expando));
                if let Some(merged) = merged {
                    symbol = merged.clone();
                    links = merged.as_transient_symbol().symbol_links();
                }
            }
            let type_ = self.get_type_of_func_class_enum_module_worker(&symbol);
            original_links.borrow_mut().type_ = Some(type_.clone());
            links.borrow_mut().type_ = Some(type_);
        }
        let ret = (*links).borrow().type_.clone().unwrap();
        ret
    }

    pub(super) fn get_type_of_func_class_enum_module_worker(&self, symbol: &Symbol) -> Rc<Type> {
        let declaration = symbol.maybe_value_declaration();
        if symbol.flags().intersects(SymbolFlags::Module)
            && is_shorthand_ambient_module_symbol(symbol)
        {
            return self.any_type();
        } else if matches!(
            declaration.as_ref(),
            Some(declaration) if declaration.kind() == SyntaxKind::BinaryExpression ||
                is_access_expression(declaration) &&
                declaration.parent().kind() == SyntaxKind::BinaryExpression
        ) {
            return self
                .get_widened_type_for_assignment_declaration(symbol, Option::<&Symbol>::None);
        } else if symbol.flags().intersects(SymbolFlags::ValueModule)
            && matches!(
                declaration.as_ref(),
                Some(declaration) if is_source_file(declaration) && declaration.as_source_file().maybe_common_js_module_indicator().is_some()
            )
        {
            let resolved_module = self
                .resolve_external_module_symbol(Some(symbol), None)
                .unwrap();
            if !ptr::eq(&*resolved_module, symbol) {
                if !self.push_type_resolution(
                    &symbol.symbol_wrapper().into(),
                    TypeSystemPropertyName::Type,
                ) {
                    return self.error_type();
                }
                let export_equals = self
                    .get_merged_symbol(
                        (*symbol.exports())
                            .borrow()
                            .get(&InternalSymbolName::ExportEquals())
                            .map(Clone::clone),
                    )
                    .unwrap();
                let type_ = self.get_widened_type_for_assignment_declaration(
                    &export_equals,
                    if Rc::ptr_eq(&export_equals, &resolved_module) {
                        None
                    } else {
                        Some(resolved_module)
                    },
                );
                if !self.pop_type_resolution() {
                    return self.report_circularity_error(symbol);
                }
                return type_;
            }
        }
        let type_: Rc<Type> = self
            .create_object_type(ObjectFlags::Anonymous, Some(symbol))
            .into();
        if symbol.flags().intersects(SymbolFlags::Class) {
            let base_type_variable = self.get_base_type_variable_of_class(symbol);
            if let Some(base_type_variable) = base_type_variable {
                self.get_intersection_type(
                    &vec![type_, base_type_variable],
                    Option::<&Symbol>::None,
                    None,
                )
            } else {
                type_
            }
        } else {
            if self.strict_null_checks && symbol.flags().intersects(SymbolFlags::Optional) {
                self.get_optional_type_(&type_, None)
            } else {
                type_
            }
        }
    }

    pub(super) fn get_type_of_enum_member(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        if let Some(links_type) = (*links).borrow().type_.clone() {
            return links_type;
        }
        let ret = self.get_declared_type_of_enum_member(symbol);
        links.borrow_mut().type_ = Some(ret.clone());
        ret
    }

    pub(super) fn get_type_of_alias(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        if (*links).borrow().type_.is_none() {
            let target_symbol = self.resolve_alias(symbol);
            let export_symbol = symbol.maybe_declarations().as_ref().and_then(|_| {
                self.get_target_of_alias_declaration(
                    &self.get_declaration_of_alias_symbol(symbol).unwrap(),
                    Some(true),
                )
            });
            let declared_type = export_symbol.as_ref().and_then(|export_symbol| {
                maybe_first_defined(
                    export_symbol.maybe_declarations().as_deref(),
                    |d: &Rc<Node>, _| {
                        if is_export_assignment(d) {
                            self.try_get_type_from_effective_type_node(d)
                        } else {
                            None
                        }
                    },
                )
            });
            links.borrow_mut().type_ = Some(
                if let Some(export_symbol) = export_symbol.as_ref().filter(|export_symbol| {
                    matches!(
                        export_symbol.maybe_declarations().as_deref(),
                        Some(export_symbol_declarations) if self.is_duplicated_common_js_export(Some(export_symbol_declarations))
                    ) && !symbol.maybe_declarations().as_ref().unwrap().is_empty()
                }) {
                    self.get_flow_type_from_common_js_export(export_symbol)
                } else if self.is_duplicated_common_js_export(symbol.maybe_declarations().as_deref()) {
                    self.auto_type()
                } else if let Some(declared_type) = declared_type {
                    declared_type
                } else if target_symbol.flags().intersects(SymbolFlags::Value) {
                    self.get_type_of_symbol(&target_symbol)
                } else {
                    self.error_type()
                }
            );
        }
        let ret = (*links).borrow().type_.clone().unwrap();
        ret
    }

    pub(super) fn get_type_of_instantiated_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        let mut links = links.borrow_mut();
        if links.type_.is_none() {
            if !self.push_type_resolution(
                &symbol.symbol_wrapper().into(),
                TypeSystemPropertyName::Type,
            ) {
                links.type_ = Some(self.error_type());
                return self.error_type();
            }
            let mut type_ = self
                .instantiate_type(
                    Some(self.get_type_of_symbol(links.target.as_ref().unwrap())),
                    links.mapper.as_ref(),
                )
                .unwrap();
            if !self.pop_type_resolution() {
                type_ = self.report_circularity_error(symbol);
            }
            links.type_ = Some(type_);
        }
        links.type_.clone().unwrap()
    }

    pub(super) fn report_circularity_error(&self, symbol: &Symbol) -> Rc<Type> {
        let declaration = symbol.maybe_value_declaration().unwrap();
        if get_effective_type_annotation_node(&declaration).is_some() {
            self.error(
                symbol.maybe_value_declaration(),
                &Diagnostics::_0_is_referenced_directly_or_indirectly_in_its_own_type_annotation,
                Some(vec![self.symbol_to_string_(
                    symbol,
                    Option::<&Node>::None,
                    None,
                    None,
                    None,
                )]),
            );
            return self.error_type();
        }
        if self.no_implicit_any
            && (declaration.kind() != SyntaxKind::Parameter
                || declaration
                    .as_has_initializer()
                    .maybe_initializer()
                    .is_some())
        {
            self.error(
                symbol.maybe_value_declaration(),
                &Diagnostics::_0_implicitly_has_type_any_because_it_does_not_have_a_type_annotation_and_is_referenced_directly_or_indirectly_in_its_own_initializer,
                Some(vec![
                    self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None)
                ])
            );
        }
        self.any_type()
    }

    pub(super) fn get_type_of_symbol_with_deferred_type(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        let mut links = links.borrow_mut();
        if links.type_.is_none() {
            Debug_.assert_is_defined(&links.deferral_parent.as_ref(), None);
            Debug_.assert_is_defined(&links.deferral_constituents.as_ref(), None);
            links.type_ = Some(
                if links
                    .deferral_parent
                    .as_ref()
                    .unwrap()
                    .flags()
                    .intersects(TypeFlags::Union)
                {
                    self.get_union_type(links.deferral_constituents.clone().unwrap(), None)
                } else {
                    self.get_intersection_type(
                        links.deferral_constituents.as_deref().unwrap(),
                        Option::<&Symbol>::None,
                        None,
                    )
                },
            );
        }
        links.type_.clone().unwrap()
    }

    pub(super) fn get_set_accessor_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        if symbol.flags().intersects(SymbolFlags::Accessor) {
            let type_ = self.get_type_of_set_accessor(symbol);
            if let Some(type_) = type_ {
                return type_;
            }
        }
        self.get_type_of_symbol(symbol)
    }

    pub(super) fn get_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        let check_flags = get_check_flags(symbol);
        if check_flags.intersects(CheckFlags::DeferredType) {
            return self.get_type_of_symbol_with_deferred_type(symbol);
        }
        if check_flags.intersects(CheckFlags::Instantiated) {
            return self.get_type_of_instantiated_symbol(symbol);
        }
        if check_flags.intersects(CheckFlags::Mapped) {
            return self.get_type_of_mapped_symbol(symbol);
        }
        if check_flags.intersects(CheckFlags::ReverseMapped) {
            return self.get_type_of_reverse_mapped_symbol(symbol);
        }
        if symbol
            .flags()
            .intersects(SymbolFlags::Variable | SymbolFlags::Property)
        {
            return self.get_type_of_variable_or_parameter_or_property(symbol);
        }
        if symbol.flags().intersects(
            SymbolFlags::Function
                | SymbolFlags::Method
                | SymbolFlags::Class
                | SymbolFlags::Enum
                | SymbolFlags::ValueModule,
        ) {
            return self.get_type_of_func_class_enum_module(symbol);
        }
        if symbol.flags().intersects(SymbolFlags::EnumMember) {
            return self.get_type_of_enum_member(symbol);
        }
        if symbol.flags().intersects(SymbolFlags::Accessor) {
            return self.get_type_of_accessors(symbol);
        }
        if symbol.flags().intersects(SymbolFlags::Alias) {
            return self.get_type_of_alias(symbol);
        }
        self.error_type()
    }

    pub(super) fn get_non_missing_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        self.remove_missing_type(
            &self.get_type_of_symbol(symbol),
            symbol.flags().intersects(SymbolFlags::Optional),
        )
    }

    pub(super) fn is_reference_to_type(&self, type_: &Type, target: &Type) -> bool {
        /*type !== undefined && target !== undefined &&*/
        get_object_flags(type_).intersects(ObjectFlags::Reference)
            && ptr::eq(&*type_.as_type_reference().target, target)
    }

    pub(super) fn get_target_type(&self, type_: &Type) -> Rc<Type> {
        if get_object_flags(type_).intersects(ObjectFlags::Reference) {
            type_.as_type_reference().target.clone()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn has_base_type<TCheckBase: Borrow<Type>>(
        &self,
        type_: &Type,
        check_base: Option<TCheckBase>,
    ) -> bool {
        let check_base = check_base.map(|check_base| check_base.borrow().type_wrapper());
        self.has_base_type_check(check_base.as_deref(), type_)
    }

    pub(super) fn has_base_type_check(&self, check_base: Option<&Type>, type_: &Type) -> bool {
        if get_object_flags(type_)
            .intersects(ObjectFlags::ClassOrInterface | ObjectFlags::Reference)
        {
            let target = self.get_target_type(type_);
            matches!(check_base, Some(check_base) if ptr::eq(&*target, check_base))
                || some(
                    Some(&*self.get_base_types(&target)),
                    Some(|type_: &Rc<Type>| self.has_base_type_check(check_base, type_)),
                )
        } else if type_.flags().intersects(TypeFlags::Intersection) {
            some(
                Some(type_.as_union_or_intersection_type_interface().types()),
                Some(|type_: &Rc<Type>| self.has_base_type_check(check_base, type_)),
            )
        } else {
            false
        }
    }

    pub(super) fn append_type_parameters(
        &self,
        mut type_parameters: Option<Vec<Rc<Type>>>,
        declarations: &[Rc<Node>],
    ) -> Option<Vec<Rc<Type>>> {
        for declaration in declarations {
            type_parameters = Some(maybe_append_if_unique_rc(
                type_parameters,
                &self.get_declared_type_of_type_parameter(
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

    pub(super) fn get_base_constructor_type_of_class(
        &self,
        type_: &Type, /*InterfaceType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_base_types(
        &self,
        type_: &Type, /*InterfaceType*/
    ) -> Vec<Rc<Type /*BaseType*/>> {
        unimplemented!()
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

            let type_ = self.create_object_type(kind, Some(symbol));
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
                    .as_interface_type()
                    .maybe_this_type_mut()
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
