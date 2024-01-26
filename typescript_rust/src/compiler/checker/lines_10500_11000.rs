use std::{
    borrow::{Borrow, Cow},
    convert::TryInto,
    io, ptr,
};

use gc::{Gc, GcCell};
use id_arena::Id;

use super::{signature_has_rest_parameter, MembersOrExportsResolutionKind};
use crate::{
    append_if_unique_gc, are_gc_slices_equal, concatenate, create_symbol_table,
    declaration_name_to_string, escape_leading_underscores, every, for_each,
    get_assignment_declaration_kind, get_check_flags, get_class_like_declaration_of_symbol,
    get_members_of_declaration, get_name_of_declaration, get_object_flags, has_dynamic_name,
    has_static_modifier, has_syntactic_modifier, is_binary_expression, is_dynamic_name,
    is_element_access_expression, is_in_js_file, last_or_undefined, length, maybe_concatenate,
    maybe_for_each, range_equals, range_equals_gc, return_ok_default_if_none, some, try_map,
    try_map_defined, try_maybe_map, try_some, unescape_leading_underscores,
    AssignmentDeclarationKind, CheckFlags, Debug_, Diagnostics, ElementFlags, HasArena, InArena,
    IndexInfo, InterfaceTypeInterface, InterfaceTypeWithDeclaredMembersInterface,
    InternalSymbolName, LiteralType, ModifierFlags, Node, NodeInterface, ObjectFlags, OptionTry,
    Signature, SignatureFlags, SignatureKind, SignatureOptionalCallSignatureCache, Symbol,
    SymbolFlags, SymbolInterface, SymbolLinks, SymbolTable, Ternary, TransientSymbolInterface,
    Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper, TypePredicate,
    append_if_unique_eq,
};

impl TypeChecker {
    pub(super) fn has_late_bindable_name(
        &self,
        node: Id<Node>, /*Declaration*/
    ) -> io::Result<bool> {
        let name = get_name_of_declaration(Some(node), self);
        Ok(match name {
            None => false,
            Some(name) => self.is_late_bindable_name(name)?,
        })
    }

    pub(super) fn has_bindable_name(
        &self,
        node: Id<Node>, /*Declaration*/
    ) -> io::Result<bool> {
        Ok(!has_dynamic_name(node, self) || self.has_late_bindable_name(node)?)
    }

    pub(super) fn is_non_bindable_dynamic_name(
        &self,
        node: Id<Node>, /*DeclarationName*/
    ) -> io::Result<bool> {
        Ok(is_dynamic_name(node, self) && !self.is_late_bindable_name(node)?)
    }

    pub(super) fn get_property_name_from_type(
        &self,
        type_: Id<Type>, /*StringLiteralType | NumberLiteralType | UniqueESSymbolType*/
    ) -> String /*__String*/ {
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::UniqueESSymbol)
        {
            return type_
                .ref_(self)
                .as_unique_es_symbol_type()
                .escaped_name
                .clone();
        }
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::StringLiteral | TypeFlags::NumberLiteral)
        {
            return match &*type_.ref_(self) {
                Type::LiteralType(LiteralType::NumberLiteralType(type_)) => {
                    escape_leading_underscores(&type_.value.to_string()).into_owned()
                }
                Type::LiteralType(LiteralType::StringLiteralType(type_)) => {
                    escape_leading_underscores(&type_.value).into_owned()
                }
                _ => panic!("Expected NumberLiteralType or StringLiteralType"),
            };
        }
        Debug_.fail(None)
    }

    pub(super) fn add_declaration_to_late_bound_symbol(
        &self,
        symbol: Id<Symbol>,
        member: Id<Node>, /*LateBoundDeclaration | BinaryExpression*/
        symbol_flags: SymbolFlags,
    ) {
        Debug_.assert(
            get_check_flags(&symbol.ref_(self)).intersects(CheckFlags::Late),
            Some("Expected a late-bound symbol."),
        );
        symbol
            .ref_(self)
            .set_flags(symbol.ref_(self).flags() | symbol_flags);
        self.get_symbol_links(member.ref_(self).symbol())
            .borrow_mut()
            .late_symbol = Some(symbol);
        if symbol.ref_(self).maybe_declarations().is_none() {
            symbol
                .ref_(self)
                .set_declarations(vec![member]);
        } else if member.ref_(self).symbol().ref_(self).maybe_is_replaceable_by_method() != Some(true) {
            symbol
                .ref_(self)
                .maybe_declarations_mut()
                .as_mut()
                .unwrap()
                .push(member);
        }
        if symbol_flags.intersects(SymbolFlags::Value) {
            if match symbol.ref_(self).maybe_value_declaration() {
                None => true,
                Some(value_declaration) => value_declaration.ref_(self).kind() != member.ref_(self).kind(),
            } {
                symbol
                    .ref_(self)
                    .set_value_declaration(member);
            }
        }
    }

    pub(super) fn late_bind_member(
        &self,
        parent: Id<Symbol>,
        early_symbols: Option<&SymbolTable>,
        late_symbols: &mut SymbolTable,
        decl: Id<Node>, /*LateBoundDeclaration | LateBoundBinaryExpressionDeclaration*/
    ) -> io::Result<Id<Symbol>> {
        Debug_.assert(
            decl.ref_(self).maybe_symbol().is_some(),
            Some("The member is expected to have a symbol."),
        );
        let links = self.get_node_links(decl);
        if (*links).borrow().resolved_symbol.is_none() {
            links.borrow_mut().resolved_symbol = decl.ref_(self).maybe_symbol();
            let decl_name = if is_binary_expression(&decl.ref_(self)) {
                decl.ref_(self).as_binary_expression().left
            } else {
                decl.ref_(self).as_named_declaration().name()
            };
            let type_ = if is_element_access_expression(&decl_name.ref_(self)) {
                self.check_expression_cached(
                    decl_name.ref_(self).as_element_access_expression().argument_expression,
                    None,
                )?
            } else {
                self.check_computed_property_name(decl_name)?
            };
            if self.is_type_usable_as_property_name(type_) {
                let member_name = self.get_property_name_from_type(type_);
                let symbol_flags = decl.ref_(self).symbol().ref_(self).flags();

                let mut late_symbol: Option<Id<Symbol>> =
                    late_symbols.get(&*member_name).map(Clone::clone);
                if late_symbol.is_none() {
                    late_symbol = Some(
                        self.alloc_symbol(
                            self.create_symbol(
                                SymbolFlags::None,
                                (*member_name).to_owned(),
                                Some(CheckFlags::Late),
                            )
                            .into(),
                        ),
                    );
                    late_symbols.insert((*member_name).to_owned(), late_symbol.clone().unwrap());
                }
                let mut late_symbol = late_symbol.unwrap();

                let early_symbol =
                    early_symbols.and_then(|early_symbols| early_symbols.get(&*member_name));
                if late_symbol
                    .ref_(self)
                    .flags()
                    .intersects(self.get_excluded_symbol_flags(symbol_flags))
                    || early_symbol.is_some()
                {
                    let declarations = if let Some(&early_symbol) = early_symbol {
                        maybe_concatenate(
                            early_symbol.ref_(self).maybe_declarations().clone(),
                            late_symbol.ref_(self).maybe_declarations().clone(),
                        )
                    } else {
                        late_symbol.ref_(self).maybe_declarations().clone()
                    };
                    let name = if !type_
                        .ref_(self)
                        .flags()
                        .intersects(TypeFlags::UniqueESSymbol)
                    {
                        unescape_leading_underscores(&member_name).to_owned()
                    } else {
                        declaration_name_to_string(Some(decl_name), self).into_owned()
                    };
                    maybe_for_each(declarations.as_deref(), |&declaration: &Id<Node>, _| {
                        self.error(
                            Some(
                                get_name_of_declaration(Some(declaration), self)
                                    .unwrap_or(declaration),
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
                    late_symbol = self.alloc_symbol(
                        self.create_symbol(SymbolFlags::None, member_name, Some(CheckFlags::Late))
                            .into(),
                    );
                }
                late_symbol
                    .ref_(self)
                    .as_transient_symbol()
                    .symbol_links()
                    .borrow_mut()
                    .name_type = Some(type_);
                self.add_declaration_to_late_bound_symbol(late_symbol, decl, symbol_flags);
                if let Some(late_symbol_parent) = late_symbol.ref_(self).maybe_parent() {
                    Debug_.assert(
                        late_symbol_parent == parent,
                        Some("Existing symbol parent should match new one"),
                    );
                } else {
                    late_symbol.ref_(self).set_parent(Some(parent));
                }
                links.borrow_mut().resolved_symbol = Some(late_symbol.clone());
                return Ok(late_symbol);
            }
        }
        let ret = (*links).borrow().resolved_symbol.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_resolved_members_or_exports_of_symbol(
        &self,
        symbol: Id<Symbol>,
        resolution_kind: MembersOrExportsResolutionKind,
    ) -> io::Result<Gc<GcCell<SymbolTable>>> {
        let links = self.get_symbol_links(symbol);
        if self
            .get_symbol_links_members_or_exports_resolution_field_value(&links, resolution_kind)
            .is_none()
        {
            let is_static = resolution_kind == MembersOrExportsResolutionKind::resolved_exports;
            let early_symbols: Option<Gc<GcCell<SymbolTable>>> = if !is_static {
                symbol.ref_(self).maybe_members().clone()
            } else if symbol.ref_(self).flags().intersects(SymbolFlags::Module) {
                Some(self.get_exports_of_module_worker(symbol)?)
            } else {
                symbol.ref_(self).maybe_exports().clone()
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

            let mut late_symbols = create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None);
            let early_symbols_ref = early_symbols
                .as_ref()
                .map(|early_symbols| (**early_symbols).borrow());
            if let Some(symbol_declarations) = symbol.ref_(self).maybe_declarations().as_deref() {
                for &decl in symbol_declarations {
                    let members = get_members_of_declaration(&decl.ref_(self));
                    if let Some(members) = members {
                        for &member in &members {
                            if is_static == has_static_modifier(member, self)
                                && self.has_late_bindable_name(member)?
                            {
                                self.late_bind_member(
                                    symbol,
                                    early_symbols_ref.as_deref(),
                                    &mut late_symbols,
                                    member,
                                )?;
                            }
                        }
                    }
                }
                let symbol_ref = symbol.ref_(self);
                let assignments = symbol_ref.maybe_assignment_declaration_members();
                if let Some(assignments) = assignments.as_ref() {
                    let decls = assignments.values();
                    for &member in decls {
                        let assignment_kind = get_assignment_declaration_kind(member, self);
                        let is_instance_member = assignment_kind
                            == AssignmentDeclarationKind::PrototypeProperty
                            || is_binary_expression(&member.ref_(self))
                                && self.is_possibly_aliased_this_property(
                                    member,
                                    Some(assignment_kind),
                                )?
                            || matches!(
                                assignment_kind,
                                AssignmentDeclarationKind::ObjectDefinePrototypeProperty
                                    | AssignmentDeclarationKind::Prototype
                            );
                        if is_static != is_instance_member && self.has_late_bindable_name(member)? {
                            self.late_bind_member(
                                symbol,
                                early_symbols_ref.as_deref(),
                                &mut late_symbols,
                                member,
                            )?;
                        }
                    }
                }

                self.set_symbol_links_members_or_exports_resolution_field_value(
                    &links,
                    resolution_kind,
                    Some(
                        self.combine_symbol_tables(
                            early_symbols.clone(),
                            Some(Gc::new(GcCell::new(late_symbols))),
                        )?
                        .unwrap_or_else(|| self.empty_symbols()),
                    ),
                )
            }
        }
        Ok(self
            .get_symbol_links_members_or_exports_resolution_field_value(&links, resolution_kind)
            .unwrap())
    }

    pub(super) fn get_symbol_links_members_or_exports_resolution_field_value(
        &self,
        symbol_links: &GcCell<SymbolLinks>,
        resolution_kind: MembersOrExportsResolutionKind,
    ) -> Option<Gc<GcCell<SymbolTable>>> {
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
        symbol_links: &GcCell<SymbolLinks>,
        resolution_kind: MembersOrExportsResolutionKind,
        value: Option<Gc<GcCell<SymbolTable>>>,
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

    pub(super) fn get_members_of_symbol(
        &self,
        symbol: Id<Symbol>,
    ) -> io::Result<Gc<GcCell<SymbolTable>>> {
        Ok(
            if symbol
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::LateBindingContainer)
            {
                self.get_resolved_members_or_exports_of_symbol(
                    symbol,
                    MembersOrExportsResolutionKind::resolved_members,
                )?
            } else {
                symbol
                    .ref_(self)
                    .maybe_members()
                    .clone()
                    .unwrap_or_else(|| self.empty_symbols())
            },
        )
    }

    pub(super) fn get_late_bound_symbol(&self, symbol: Id<Symbol>) -> io::Result<Id<Symbol>> {
        if symbol
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::ClassMember)
            && symbol.ref_(self).escaped_name() == InternalSymbolName::Computed
        {
            let links = self.get_symbol_links(symbol);
            if {
                let value = (*links).borrow().late_symbol.is_none();
                value
            } && try_some(
                symbol.ref_(self).maybe_declarations().as_deref(),
                Some(|&declaration: &Id<Node>| self.has_late_bindable_name(declaration)),
            )? {
                let parent = self
                    .get_merged_symbol(symbol.ref_(self).maybe_parent())
                    .unwrap();
                if some(
                    symbol.ref_(self).maybe_declarations().as_deref(),
                    Some(|&declaration: &Id<Node>| has_static_modifier(declaration, self)),
                ) {
                    self.get_exports_of_symbol(parent)?;
                } else {
                    self.get_members_of_symbol(parent)?;
                }
            }
            let mut links = links.borrow_mut();
            if links.late_symbol.is_none() {
                links.late_symbol = Some(symbol);
            }
            return Ok(links.late_symbol.clone().unwrap());
        }
        Ok(symbol)
    }

    pub(super) fn get_type_with_this_argument(
        &self,
        type_: Id<Type>,
        this_argument: Option<Id<Type>>,
        need_apparent_type: Option<bool>,
    ) -> io::Result<Id<Type>> {
        if get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::Reference) {
            let target = type_.ref_(self).as_type_reference_interface().target();
            let type_arguments = self.get_type_arguments(type_)?;
            if length(
                target
                    .ref_(self)
                    .as_interface_type()
                    .maybe_type_parameters(),
            ) == length(Some(&type_arguments))
            {
                let ref_ = self.create_type_reference(
                    target,
                    Some(concatenate(
                        type_arguments,
                        vec![this_argument.clone().unwrap_or_else(|| {
                            target
                                .ref_(self)
                                .as_interface_type()
                                .maybe_this_type()
                                .unwrap()
                        })],
                    )),
                );
                return Ok(if matches!(need_apparent_type, Some(true)) {
                    self.get_apparent_type(ref_)?
                } else {
                    ref_
                });
            }
        } else if type_.ref_(self).flags().intersects(TypeFlags::Intersection) {
            let types = try_map(
                {
                    let types = type_
                        .ref_(self)
                        .as_union_or_intersection_type_interface()
                        .types()
                        .to_owned();
                    types
                },
                |t: Id<Type>, _| {
                    self.get_type_with_this_argument(t, this_argument, need_apparent_type)
                },
            )?;
            return Ok(
                if types
                    != type_
                        .ref_(self)
                        .as_union_or_intersection_type_interface()
                        .types()
                {
                    self.get_intersection_type(&types, Option::<Id<Symbol>>::None, None)?
                } else {
                    type_
                },
            );
        }
        Ok(if matches!(need_apparent_type, Some(true)) {
            self.get_apparent_type(type_)?
        } else {
            type_
        })
    }

    pub(super) fn resolve_object_type_members(
        &self,
        type_: Id<Type>,  /*ObjectType*/
        source: Id<Type>, /*InterfaceTypeWithDeclaredMembers*/
        type_parameters: Vec<Id<Type /*TypeParameter*/>>,
        type_arguments: Vec<Id<Type>>,
    ) -> io::Result<()> {
        let mut mapper: Option<Id<TypeMapper>> = None;
        let mut members: Gc<GcCell<SymbolTable>>;
        let mut call_signatures: Vec<Id<Signature>>;
        let mut construct_signatures: Vec<Id<Signature>>;
        let mut index_infos: Vec<Gc<IndexInfo>>;
        if range_equals(&type_parameters, &type_arguments, 0, type_parameters.len()) {
            members = if let Some(source_symbol) = source.ref_(self).maybe_symbol() {
                self.get_members_of_symbol(source_symbol)?
            } else {
                Gc::new(GcCell::new(create_symbol_table(
                    self.arena(),
                    source
                        .ref_(self)
                        .as_interface_type_with_declared_members()
                        .maybe_declared_properties()
                        .as_deref(),
                )))
            };
            call_signatures = source
                .ref_(self)
                .as_interface_type_with_declared_members()
                .declared_call_signatures()
                .clone();
            construct_signatures = source
                .ref_(self)
                .as_interface_type_with_declared_members()
                .declared_construct_signatures()
                .clone();
            index_infos = source
                .ref_(self)
                .as_interface_type_with_declared_members()
                .declared_index_infos()
                .clone();
        } else {
            let type_parameters_len_is_1 = type_parameters.len() == 1;
            mapper = Some(self.create_type_mapper(type_parameters, Some(type_arguments.clone())));
            members = Gc::new(GcCell::new(
                self.create_instantiated_symbol_table(
                    source
                        .ref_(self)
                        .as_interface_type()
                        .maybe_declared_properties()
                        .as_ref()
                        .unwrap(),
                    mapper.clone().unwrap(),
                    type_parameters_len_is_1,
                )?,
            ));
            call_signatures = self.instantiate_signatures(
                &*source
                    .ref_(self)
                    .as_interface_type_with_declared_members()
                    .declared_call_signatures(),
                mapper.clone().unwrap(),
            )?;
            construct_signatures = self.instantiate_signatures(
                &*source
                    .ref_(self)
                    .as_interface_type_with_declared_members()
                    .declared_construct_signatures(),
                mapper.clone().unwrap(),
            )?;
            index_infos = self.instantiate_index_infos(
                &*source
                    .ref_(self)
                    .as_interface_type_with_declared_members()
                    .declared_index_infos(),
                mapper.clone().unwrap(),
            )?;
        }
        let base_types = self.get_base_types(source)?;
        if !base_types.is_empty() {
            if matches!(source.ref_(self).maybe_symbol(), Some(symbol) if Gc::ptr_eq(&members, &self.get_members_of_symbol(symbol)?))
            {
                members = Gc::new(GcCell::new(create_symbol_table(
                    self.arena(),
                    source
                        .ref_(self)
                        .as_interface_type_with_declared_members()
                        .maybe_declared_properties()
                        .as_deref(),
                )));
            }
            self.set_structured_type_members(
                type_.ref_(self).as_object_type(),
                members.clone(),
                call_signatures.clone(),
                construct_signatures.clone(),
                index_infos.clone(),
            )?;
            let this_argument = last_or_undefined(&type_arguments).copied();
            for base_type in base_types {
                let instantiated_base_type = if let Some(this_argument) = this_argument {
                    self.get_type_with_this_argument(
                        self.instantiate_type(base_type, mapper.clone())?,
                        Some(this_argument),
                        None,
                    )?
                } else {
                    base_type.clone()
                };
                let properties = self.get_properties_of_type(instantiated_base_type)?;
                self.add_inherited_members(&mut members.borrow_mut(), properties);
                call_signatures = concatenate(
                    call_signatures,
                    self.get_signatures_of_type(instantiated_base_type, SignatureKind::Call)?,
                );
                construct_signatures = concatenate(
                    construct_signatures,
                    self.get_signatures_of_type(instantiated_base_type, SignatureKind::Construct)?,
                );
                let inherited_index_infos = if instantiated_base_type != self.any_type() {
                    self.get_index_infos_of_type(instantiated_base_type)?
                } else {
                    vec![Gc::new(self.create_index_info(
                        self.string_type(),
                        self.any_type(),
                        false,
                        None,
                    ))]
                };
                let inherited_index_infos_filtered = inherited_index_infos
                    .into_iter()
                    .filter(|info| self.find_index_info(&index_infos, info.key_type).is_none())
                    .collect::<Vec<_>>();
                index_infos.extend(inherited_index_infos_filtered);
            }
        }
        self.set_structured_type_members(
            type_.ref_(self).as_object_type(),
            members,
            call_signatures,
            construct_signatures,
            index_infos,
        )?;

        Ok(())
    }

    pub(super) fn resolve_class_or_interface_members(
        &self,
        type_: Id<Type>, /*InterfaceType*/
    ) -> io::Result<()> {
        self.resolve_object_type_members(
            type_,
            self.resolve_declared_members(type_)?,
            vec![],
            vec![],
        )?;

        Ok(())
    }

    pub(super) fn resolve_type_reference_members(
        &self,
        type_: Id<Type>, /*TypeReference*/
    ) -> io::Result<()> {
        let source = self.resolve_declared_members({
            let target = type_.ref_(self).as_type_reference_interface().target();
            target
        })?;
        let type_parameters = maybe_concatenate(
            source
                .ref_(self)
                .as_interface_type()
                .maybe_type_parameters()
                .map(|type_parameters| type_parameters.to_owned()),
            Some(vec![source
                .ref_(self)
                .as_interface_type()
                .maybe_this_type()
                .unwrap()]),
        )
        .unwrap();
        let type_arguments = self.get_type_arguments(type_)?;
        let padded_type_arguments = if type_arguments.len() == type_parameters.len() {
            type_arguments
        } else {
            concatenate(type_arguments, vec![type_])
        };
        self.resolve_object_type_members(type_, source, type_parameters, padded_type_arguments)?;

        Ok(())
    }

    pub(super) fn create_signature(
        &self,
        declaration: Option<Id<Node>>,
        type_parameters: Option<Vec<Id<Type>>>,
        this_parameter: Option<Id<Symbol>>,
        parameters: Vec<Id<Symbol>>,
        resolved_return_type: Option<Id<Type>>,
        resolved_type_predicate: Option<Gc<TypePredicate>>,
        min_argument_count: usize,
        flags: SignatureFlags,
    ) -> Signature {
        let mut sig = (self.Signature)(flags);
        sig.declaration = declaration;
        *sig.maybe_type_parameters_mut() = type_parameters;
        sig.set_parameters(parameters);
        *sig.maybe_this_parameter_mut() = this_parameter;
        *sig.maybe_resolved_return_type_mut() = resolved_return_type;
        *sig.maybe_resolved_type_predicate_mut() = resolved_type_predicate;
        sig.set_min_argument_count(min_argument_count);
        sig
    }

    pub(super) fn clone_signature(&self, sig: Id<Signature>) -> Signature {
        let mut result = self.create_signature(
            sig.declaration.clone(),
            sig.maybe_type_parameters().clone(),
            sig.maybe_this_parameter().clone(),
            sig.parameters().to_owned(),
            None,
            None,
            sig.min_argument_count(),
            sig.flags & SignatureFlags::PropagatingFlags,
        );
        result.target = sig.target.clone();
        result.mapper = sig.mapper.clone();
        result.composite_signatures = sig.composite_signatures.clone();
        result.composite_kind = sig.composite_kind;
        result
    }

    pub(super) fn create_union_signature(
        &self,
        signature: Id<Signature>,
        union_signatures: Vec<Id<Signature>>,
    ) -> Signature {
        let mut result = self.clone_signature(signature);
        result.composite_signatures = Some(union_signatures);
        result.composite_kind = Some(TypeFlags::Union);
        result.target = None;
        result.mapper = None;
        result
    }

    pub(super) fn get_optional_call_signature(
        &self,
        signature: Id<Signature>,
        call_chain_flags: SignatureFlags,
    ) -> Id<Signature> {
        if signature.ref_(self).flags & SignatureFlags::CallChainFlags == call_chain_flags {
            return signature;
        }
        if signature.ref_(self).maybe_optional_call_signature_cache().is_none() {
            *signature.ref_(self).maybe_optional_call_signature_cache() =
                Some(SignatureOptionalCallSignatureCache::new());
        }
        let key = if call_chain_flags == SignatureFlags::IsInnerCallChain {
            "inner"
        } else {
            "outer"
        };
        let existing = if key == "inner" {
            signature
                .ref_(self).maybe_optional_call_signature_cache()
                .as_ref()
                .unwrap()
                .inner
                .clone()
        } else {
            signature
                .ref_(self).maybe_optional_call_signature_cache()
                .as_ref()
                .unwrap()
                .outer
                .clone()
        };
        if let Some(existing) = existing {
            return existing;
        }
        let ret = self.alloc_signature(self.create_optional_call_signature(&signature.ref_(self), call_chain_flags));
        if key == "inner" {
            signature
                .ref_(self).maybe_optional_call_signature_cache()
                .as_mut()
                .unwrap()
                .inner = Some(ret.clone());
        } else {
            signature
                .ref_(self).maybe_optional_call_signature_cache()
                .as_mut()
                .unwrap()
                .outer = Some(ret.clone());
        };
        ret
    }

    pub(super) fn create_optional_call_signature(
        &self,
        signature: Id<Signature>,
        call_chain_flags: SignatureFlags,
    ) -> Signature {
        Debug_.assert(
            call_chain_flags == SignatureFlags::IsInnerCallChain || call_chain_flags == SignatureFlags::IsOuterCallChain,
            Some("An optional call signature can either be for an inner call chain or an outer call chain, but not both.")
        );
        let mut result = self.clone_signature(signature);
        result.flags |= call_chain_flags;
        result
    }

    pub(super) fn get_expanded_parameters(
        &self,
        sig: Id<Signature>,
        skip_union_expanding: Option<bool>,
    ) -> io::Result<Vec<Vec<Id<Symbol>>>> {
        let skip_union_expanding = skip_union_expanding.unwrap_or(false);
        if signature_has_rest_parameter(sig) {
            let rest_index = sig.parameters().len() - 1;
            let rest_type = self.get_type_of_symbol(sig.parameters()[rest_index])?;
            if self.is_tuple_type(rest_type) {
                return Ok(vec![self.expand_signature_parameters_with_tuple_members(
                    sig, rest_type, rest_index,
                )?]);
            } else if !skip_union_expanding
                && rest_type.ref_(self).flags().intersects(TypeFlags::Union)
                && every(
                    rest_type
                        .ref_(self)
                        .as_union_or_intersection_type_interface()
                        .types(),
                    |&type_: &Id<Type>, _| self.is_tuple_type(type_),
                )
            {
                return try_map(
                    rest_type
                        .ref_(self)
                        .as_union_or_intersection_type_interface()
                        .types()
                        .to_owned(),
                    |t: Id<Type>, _| {
                        self.expand_signature_parameters_with_tuple_members(sig, t, rest_index)
                    },
                );
            }
        }
        Ok(vec![sig.parameters().to_owned()])
    }

    pub(super) fn expand_signature_parameters_with_tuple_members(
        &self,
        sig: Id<Signature>,
        rest_type: Id<Type>, /*TupleTypeReference*/
        rest_index: usize,
    ) -> io::Result<Vec<Id<Symbol>>> {
        let element_types = self.get_type_arguments(rest_type)?;
        let rest_type_target = rest_type.ref_(self).as_type_reference().target;
        let rest_type_target_ref = rest_type_target.ref_(self);
        let associated_names = rest_type_target_ref
            .as_tuple_type()
            .labeled_element_declarations
            .as_ref();
        let rest_params = try_map(&element_types, |&t: &Id<Type>, i| -> io::Result<_> {
            let tuple_label_name = associated_names
                .map(|associated_names| self.get_tuple_element_label(associated_names[i]));
            let name = tuple_label_name.try_unwrap_or_else(|| {
                self.get_parameter_name_at_position(sig, rest_index + i, Some(rest_type))
            })?;
            let flags = rest_type_target.ref_(self).as_tuple_type().element_flags[i];
            let check_flags = if flags.intersects(ElementFlags::Variable) {
                CheckFlags::RestParameter
            } else if flags.intersects(ElementFlags::Optional) {
                CheckFlags::OptionalParameter
            } else {
                CheckFlags::None
            };
            let symbol = self.alloc_symbol(
                self.create_symbol(SymbolFlags::FunctionScopedVariable, name, Some(check_flags))
                    .into(),
            );
            symbol
                .ref_(self)
                .as_transient_symbol()
                .symbol_links()
                .borrow_mut()
                .type_ = Some(if flags.intersects(ElementFlags::Rest) {
                self.create_array_type(t, None)
            } else {
                t.clone()
            });
            Ok(symbol)
        })?;
        Ok(concatenate(
            sig.parameters()[0..rest_index].to_owned(),
            rest_params,
        ))
    }

    pub(super) fn get_default_construct_signatures(
        &self,
        class_type: Id<Type>, /*InterfaceType*/
    ) -> io::Result<Vec<Id<Signature>>> {
        let base_constructor_type = self.get_base_constructor_type_of_class(class_type)?;
        let base_signatures =
            self.get_signatures_of_type(base_constructor_type, SignatureKind::Construct)?;
        let declaration =
            get_class_like_declaration_of_symbol(class_type.ref_(self).symbol(), self);
        let is_abstract = matches!(
            declaration,
            Some(declaration) if has_syntactic_modifier(declaration, ModifierFlags::Abstract, self)
        );
        if base_signatures.is_empty() {
            return Ok(vec![self.alloc_signature(
                self.create_signature(
                    None,
                    class_type
                        .ref_(self)
                        .as_interface_type()
                        .maybe_local_type_parameters()
                        .map(ToOwned::to_owned),
                    None,
                    vec![],
                    Some(class_type),
                    None,
                    0,
                    if is_abstract {
                        SignatureFlags::Abstract
                    } else {
                        SignatureFlags::None
                    },
                ),
            )]);
        }
        let base_type_node = self.get_base_type_node_of_class(class_type).unwrap();
        let is_java_script = is_in_js_file(Some(&base_type_node.ref_(self)));
        let type_arguments = self.type_arguments_from_type_reference_node(base_type_node)?;
        let type_arg_count = length(type_arguments.as_deref());
        let mut result: Vec<Id<Signature>> = vec![];
        for base_sig in base_signatures {
            let min_type_argument_count =
                self.get_min_type_argument_count(base_sig.ref_(self).maybe_type_parameters().as_deref());
            let type_param_count = length(base_sig.ref_(self).maybe_type_parameters().as_deref());
            if is_java_script
                || type_arg_count >= min_type_argument_count && type_arg_count <= type_param_count
            {
                let mut sig = if type_param_count > 0 {
                    self.create_signature_instantiation(
                        base_sig.clone(),
                        self.fill_missing_type_arguments(
                            type_arguments.clone(),
                            base_sig.ref_(self).maybe_type_parameters().as_deref(),
                            min_type_argument_count,
                            is_java_script,
                        )?
                        .as_deref(),
                    )?
                } else {
                    self.clone_signature(&base_sig.ref_(self))
                };
                *sig.maybe_type_parameters_mut() = class_type
                    .ref_(self)
                    .as_interface_type()
                    .maybe_local_type_parameters()
                    .map(ToOwned::to_owned);
                *sig.maybe_resolved_return_type_mut() = Some(class_type);
                sig.flags = if is_abstract {
                    sig.flags | SignatureFlags::Abstract
                } else {
                    sig.flags & !SignatureFlags::Abstract
                };
                result.push(self.alloc_signature(sig));
            }
        }
        Ok(result)
    }

    pub(super) fn find_matching_signature(
        &self,
        signature_list: &[Id<Signature>],
        signature: Id<Signature>,
        partial_match: bool,
        ignore_this_types: bool,
        ignore_return_types: bool,
    ) -> io::Result<Option<Id<Signature>>> {
        for s in signature_list {
            if self.compare_signatures_identical(
                s.clone(),
                signature.clone(),
                partial_match,
                ignore_this_types,
                ignore_return_types,
                |source, target| {
                    Ok(if partial_match {
                        self.compare_types_subtype_of(source, target)?
                    } else {
                        self.compare_types_identical(source, target)?
                    })
                },
            )? != Ternary::False
            {
                return Ok(Some(s.clone()));
            }
        }
        Ok(None)
    }

    pub(super) fn find_matching_signatures(
        &self,
        signature_lists: &[Vec<Id<Signature>>],
        signature: Id<Signature>,
        list_index: usize,
    ) -> io::Result<Option<Vec<Id<Signature>>>> {
        if signature.ref_(self).maybe_type_parameters().is_some() {
            if list_index > 0 {
                return Ok(None);
            }
            for signature_list in signature_lists.iter().skip(1) {
                if self
                    .find_matching_signature(
                        signature_list,
                        signature.clone(),
                        false,
                        false,
                        false,
                    )?
                    .is_none()
                {
                    return Ok(None);
                }
            }
            return Ok(Some(vec![signature]));
        }
        let mut result: Option<Vec<Id<Signature>>> = None;
        for (i, signature_list) in signature_lists.iter().enumerate() {
            let match_ = return_ok_default_if_none!(if i == list_index {
                Some(signature.clone())
            } else {
                self.find_matching_signature(signature_list, signature.clone(), true, false, true)?
            });
            if result.is_none() {
                result = Some(vec![]);
            }
            append_if_unique_eq(result.as_mut().unwrap(), &match_);
        }
        Ok(result)
    }

    pub(super) fn get_union_signatures(
        &self,
        signature_lists: &[Vec<Id<Signature>>],
    ) -> io::Result<Vec<Id<Signature>>> {
        let mut result: Option<Vec<Id<Signature>>> = None;
        let mut index_with_length_over_one: Option<isize> = None;
        for (i, signature_list) in signature_lists.iter().enumerate() {
            if signature_list.is_empty() {
                return Ok(vec![]);
            }
            if signature_list.len() > 1 {
                index_with_length_over_one = Some(if index_with_length_over_one.is_none() {
                    i.try_into().unwrap()
                } else {
                    -1
                });
            }
            for signature in signature_list {
                if match result.as_deref() {
                    None => true,
                    Some(result) => self
                        .find_matching_signature(result, signature.clone(), false, false, true)?
                        .is_none(),
                } {
                    let union_signatures =
                        self.find_matching_signatures(signature_lists, signature.clone(), i)?;
                    if let Some(union_signatures) = union_signatures {
                        let mut s = signature.clone();
                        if union_signatures.len() > 1 {
                            let mut this_parameter = signature.ref_(self).maybe_this_parameter().clone();
                            let first_this_parameter_of_union_signatures =
                                for_each(&union_signatures, |sig: &Id<Signature>, _| {
                                    sig.ref_(self).maybe_this_parameter().clone()
                                });
                            if let Some(first_this_parameter_of_union_signatures) =
                                first_this_parameter_of_union_signatures
                            {
                                let this_type = self.get_intersection_type(
                                    &try_map_defined(
                                        Some(&union_signatures),
                                        |sig: &Id<Signature>, _| {
                                            sig.ref_(self).maybe_this_parameter().try_map(|this_parameter| {
                                                self.get_type_of_symbol(this_parameter)
                                            })
                                        },
                                    )?,
                                    Option::<Id<Symbol>>::None,
                                    None,
                                )?;
                                this_parameter = Some(self.create_symbol_with_type(
                                    first_this_parameter_of_union_signatures,
                                    Some(this_type),
                                ));
                            }
                            let s_not_wrapped =
                                self.create_union_signature(&signature.ref_(self), union_signatures);
                            *s_not_wrapped.maybe_this_parameter_mut() = this_parameter;
                            s = self.alloc_signature(s_not_wrapped);
                        }
                        if result.is_none() {
                            result = Some(vec![]);
                        }
                        result.as_mut().unwrap().push(s);
                    }
                }
            }
        }
        if length(result.as_deref()) == 0 && index_with_length_over_one != Some(-1) {
            let master_list = &signature_lists[if let Some(index_with_length_over_one) =
                index_with_length_over_one
            {
                TryInto::<usize>::try_into(index_with_length_over_one).unwrap()
            } else {
                0
            }];
            let mut results: Option<Vec<Id<Signature>>> = Some(master_list.clone());
            for signatures in signature_lists {
                if !ptr::eq(signatures, master_list) {
                    let signature = signatures.get(0);
                    Debug_.assert(signature.is_some(), Some("getUnionSignatures bails early on empty signature lists and should not have empty lists on second pass"));
                    let signature = signature.unwrap();
                    results = if matches!(
                        signature.ref_(self).maybe_type_parameters().as_ref(),
                        Some(signature_type_parameters) if try_some(
                            results.as_deref(),
                            Some(|s: &Id<Signature>| -> io::Result<bool> {
                                Ok(
                                    matches!(
                                        s.ref_(self).maybe_type_parameters().as_ref(),
                                        Some(s_type_parameters) if !self.compare_type_parameters_identical(
                                            Some(signature_type_parameters),
                                            Some(s_type_parameters),
                                        )?
                                    )
                                )
                            })
                        )?
                    ) {
                        None
                    } else {
                        try_maybe_map(
                            results.as_ref(),
                            |sig: &Id<Signature>, _| -> io::Result<_> {
                                Ok(self.alloc_signature(self.combine_signatures_of_union_members(
                                    sig.clone(),
                                    signature.clone(),
                                )?))
                            },
                        )
                        .transpose()?
                    };
                    if results.is_none() {
                        break;
                    }
                }
            }
            result = results;
        }
        Ok(result.unwrap_or_else(|| vec![]))
    }

    pub(super) fn compare_type_parameters_identical(
        &self,
        source_params: Option<&[Id<Type /*TypeParameter*/>]>,
        target_params: Option<&[Id<Type /*TypeParameter*/>]>,
    ) -> io::Result<bool> {
        if length(source_params) != length(target_params) {
            return Ok(false);
        }
        if source_params.is_none() || target_params.is_none() {
            return Ok(true);
        }
        let source_params = source_params.unwrap();
        let target_params = target_params.unwrap();

        let mapper =
            self.create_type_mapper(target_params.to_owned(), Some(source_params.to_owned()));
        for (i, source) in source_params.iter().enumerate() {
            let target = target_params[i];
            if *source == target {
                continue;
            }
            if !self.is_type_identical_to(
                self.get_constraint_from_type_parameter(*source)?
                    .unwrap_or_else(|| self.unknown_type()),
                self.instantiate_type(
                    self.get_constraint_from_type_parameter(target)?
                        .unwrap_or_else(|| self.unknown_type()),
                    Some(mapper.clone()),
                )?,
            )? {
                return Ok(false);
            }
        }

        Ok(true)
    }
}
