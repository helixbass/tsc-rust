#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use super::{signature_has_rest_parameter, MappedTypeModifiers, MembersOrExportsResolutionKind};
use crate::{
    are_rc_slices_equal, concatenate, create_symbol_table, declaration_name_to_string,
    escape_leading_underscores, every, filter, get_assignment_declaration_kind, get_check_flags,
    get_class_like_declaration_of_symbol, get_members_of_declaration, get_name_of_declaration,
    get_object_flags, has_dynamic_name, has_static_modifier, has_syntactic_modifier,
    is_binary_expression, is_element_access_expression, is_in_js_file, last_or_undefined, length,
    map, maybe_concatenate, maybe_for_each, range_equals_rc, same_map, some,
    unescape_leading_underscores, AssignmentDeclarationKind, CheckFlags, Debug_, Diagnostics,
    ElementFlags, IndexInfo, InterfaceTypeInterface, InterfaceTypeWithDeclaredMembersInterface,
    InternalSymbolName, LiteralType, ModifierFlags, Node, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, Signature, SignatureFlags, SignatureKind,
    SignatureOptionalCallSignatureCache, Symbol, SymbolFlags, SymbolInterface, SymbolLinks,
    SymbolTable, TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
    TypePredicate, UnderscoreEscapedMap, __String,
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
                .map(|early_symbols| (**early_symbols).borrow());
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
        if symbol.flags().intersects(SymbolFlags::LateBindingContainer) {
            self.get_resolved_members_or_exports_of_symbol(
                symbol,
                MembersOrExportsResolutionKind::resolved_members,
            )
        } else {
            symbol
                .maybe_members()
                .clone()
                .unwrap_or_else(|| self.empty_symbols())
        }
    }

    pub(super) fn get_late_bound_symbol(&self, symbol: &Symbol) -> Rc<Symbol> {
        if symbol.flags().intersects(SymbolFlags::ClassMember)
            && symbol.escaped_name() == &InternalSymbolName::Computed()
        {
            let links = self.get_symbol_links(symbol);
            if (*links).borrow().late_symbol.is_none()
                && some(
                    symbol.maybe_declarations().as_deref(),
                    Some(|declaration: &Rc<Node>| self.has_late_bindable_name(declaration)),
                )
            {
                let parent = self.get_merged_symbol(symbol.maybe_parent()).unwrap();
                if some(
                    symbol.maybe_declarations().as_deref(),
                    Some(|declaration: &Rc<Node>| has_static_modifier(declaration)),
                ) {
                    self.get_exports_of_symbol(&parent);
                } else {
                    self.get_members_of_symbol(&parent);
                }
            }
            let mut links = links.borrow_mut();
            if links.late_symbol.is_none() {
                links.late_symbol = Some(symbol.symbol_wrapper());
            }
            return links.late_symbol.clone().unwrap();
        }
        symbol.symbol_wrapper()
    }

    pub(super) fn get_type_with_this_argument<TThisArgument: Borrow<Type>>(
        &self,
        type_: &Type,
        this_argument: Option<TThisArgument>,
        need_apparent_type: Option<bool>,
    ) -> Rc<Type> {
        let this_argument =
            this_argument.map(|this_argument| this_argument.borrow().type_wrapper());
        if get_object_flags(type_).intersects(ObjectFlags::Reference) {
            let target = &type_.as_type_reference().target;
            let type_arguments = self.get_type_arguments(type_);
            let target_as_interface_type = target.as_interface_type();
            if length(target_as_interface_type.maybe_type_parameters())
                == length(Some(&type_arguments))
            {
                let ref_: Rc<Type> = self
                    .create_type_reference(
                        target,
                        Some(concatenate(
                            type_arguments,
                            vec![this_argument.clone().unwrap_or_else(|| {
                                target_as_interface_type.maybe_this_type().unwrap()
                            })],
                        )),
                    )
                    .into();
                return if matches!(need_apparent_type, Some(true)) {
                    self.get_apparent_type(&ref_)
                } else {
                    ref_
                };
            }
        } else if type_.flags().intersects(TypeFlags::Intersection) {
            let types = same_map(
                Some(type_.as_union_or_intersection_type_interface().types()),
                |t: &Rc<Type>, _| {
                    self.get_type_with_this_argument(
                        t,
                        this_argument.as_deref(),
                        need_apparent_type,
                    )
                },
            )
            .unwrap();
            return if !are_rc_slices_equal(
                &types,
                type_.as_union_or_intersection_type_interface().types(),
            ) {
                self.get_intersection_type(&types, Option::<&Symbol>::None, None)
            } else {
                type_.type_wrapper()
            };
        }
        if matches!(need_apparent_type, Some(true)) {
            self.get_apparent_type(type_)
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn resolve_object_type_members(
        &self,
        type_: &Type,  /*ObjectType*/
        source: &Type, /*InterfaceTypeWithDeclaredMembers*/
        type_parameters: Vec<Rc<Type /*TypeParameter*/>>,
        type_arguments: Vec<Rc<Type>>,
    ) {
        let mut mapper: Option<TypeMapper> = None;
        let mut members: Rc<RefCell<SymbolTable>>;
        let mut call_signatures: Vec<Rc<Signature>>;
        let mut construct_signatures: Vec<Rc<Signature>>;
        let mut index_infos: Vec<Rc<IndexInfo>>;
        let source_as_interface_type_with_declared_members =
            source.as_interface_type_with_declared_members();
        if range_equals_rc(&type_parameters, &type_arguments, 0, type_parameters.len()) {
            members = if let Some(source_symbol) = source.maybe_symbol() {
                self.get_members_of_symbol(&source_symbol)
            } else {
                Rc::new(RefCell::new(create_symbol_table(
                    source_as_interface_type_with_declared_members
                        .maybe_declared_properties()
                        .as_deref(),
                )))
            };
            call_signatures = source_as_interface_type_with_declared_members
                .declared_call_signatures()
                .clone();
            construct_signatures = source_as_interface_type_with_declared_members
                .declared_construct_signatures()
                .clone();
            index_infos = source_as_interface_type_with_declared_members
                .declared_index_infos()
                .clone();
        } else {
            let type_parameters_len_is_1 = type_parameters.len() == 1;
            mapper = Some(self.create_type_mapper(type_parameters, Some(type_arguments.clone())));
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
            index_infos = self.instantiate_index_infos(
                &*source_as_interface_type_with_declared_members.declared_index_infos(),
                mapper.as_ref().unwrap(),
            );
        }
        let base_types = self.get_base_types(source);
        if !base_types.is_empty() {
            if matches!(source.maybe_symbol(), Some(symbol) if Rc::ptr_eq(&members, &self.get_members_of_symbol(&symbol)))
            {
                members = Rc::new(RefCell::new(create_symbol_table(
                    source_as_interface_type_with_declared_members
                        .maybe_declared_properties()
                        .as_deref(),
                )));
            }
            self.set_structured_type_members(
                type_.as_object_type(),
                members.clone(),
                call_signatures.clone(),
                construct_signatures.clone(),
                index_infos.clone(),
            );
            let this_argument = last_or_undefined(&type_arguments);
            for base_type in base_types {
                let instantiated_base_type = if let Some(this_argument) = this_argument {
                    self.get_type_with_this_argument(
                        &self
                            .instantiate_type(Some(base_type), mapper.as_ref())
                            .unwrap(),
                        Some(&**this_argument),
                        None,
                    )
                } else {
                    base_type.clone()
                };
                self.add_inherited_members(
                    &mut members.borrow_mut(),
                    &self.get_properties_of_type(&instantiated_base_type),
                );
                call_signatures = concatenate(
                    call_signatures,
                    self.get_signatures_of_type(&instantiated_base_type, SignatureKind::Call),
                );
                construct_signatures = concatenate(
                    construct_signatures,
                    self.get_signatures_of_type(&instantiated_base_type, SignatureKind::Construct),
                );
                let inherited_index_infos =
                    if !Rc::ptr_eq(&instantiated_base_type, &self.any_type()) {
                        self.get_index_infos_of_type(&instantiated_base_type)
                    } else {
                        vec![Rc::new(self.create_index_info(
                            self.string_type(),
                            self.any_type(),
                            false,
                            None,
                        ))]
                    };
                let inherited_index_infos_filtered =
                    filter(Some(&*inherited_index_infos), |info: &Rc<IndexInfo>| {
                        self.find_index_info(&index_infos, &info.key_type).is_none()
                    })
                    .unwrap();
                index_infos = concatenate(index_infos, inherited_index_infos_filtered);
            }
        }
        self.set_structured_type_members(
            type_.as_object_type(),
            members,
            call_signatures,
            construct_signatures,
            index_infos,
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
        *sig.maybe_resolved_return_type() = resolved_return_type;
        sig.resolved_type_predicate = resolved_type_predicate;
        sig.set_min_argument_count(min_argument_count);
        sig
    }

    pub(super) fn clone_signature(&self, sig: &Signature) -> Signature {
        let mut result = self.create_signature(
            sig.declaration.clone(),
            sig.type_parameters.clone(),
            sig.this_parameter.clone(),
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
        signature: &Signature,
        union_signatures: Vec<Rc<Signature>>,
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
        signature: Rc<Signature>,
        call_chain_flags: SignatureFlags,
    ) -> Rc<Signature> {
        if signature.flags & SignatureFlags::CallChainFlags == call_chain_flags {
            return signature;
        }
        if signature.maybe_optional_call_signature_cache().is_none() {
            *signature.maybe_optional_call_signature_cache() =
                Some(SignatureOptionalCallSignatureCache::new());
        }
        let key = if call_chain_flags == SignatureFlags::IsInnerCallChain {
            "inner"
        } else {
            "outer"
        };
        let existing = if key == "inner" {
            signature
                .maybe_optional_call_signature_cache()
                .as_ref()
                .unwrap()
                .inner
                .clone()
        } else {
            signature
                .maybe_optional_call_signature_cache()
                .as_ref()
                .unwrap()
                .outer
                .clone()
        };
        if let Some(existing) = existing {
            return existing;
        }
        let ret = Rc::new(self.create_optional_call_signature(&signature, call_chain_flags));
        if key == "inner" {
            signature
                .maybe_optional_call_signature_cache()
                .as_mut()
                .unwrap()
                .inner = Some(ret.clone());
        } else {
            signature
                .maybe_optional_call_signature_cache()
                .as_mut()
                .unwrap()
                .outer = Some(ret.clone());
        };
        ret
    }

    pub(super) fn create_optional_call_signature(
        &self,
        signature: &Signature,
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
        sig: &Signature,
        skip_union_expanding: Option<bool>,
    ) -> Vec<Vec<Rc<Symbol>>> {
        let skip_union_expanding = skip_union_expanding.unwrap_or(false);
        if signature_has_rest_parameter(sig) {
            let rest_index = sig.parameters().len() - 1;
            let rest_type = self.get_type_of_symbol(&sig.parameters()[rest_index]);
            if self.is_tuple_type(&rest_type) {
                return vec![self
                    .expand_signature_parameters_with_tuple_members(sig, &rest_type, rest_index)];
            } else if !skip_union_expanding
                && rest_type.flags().intersects(TypeFlags::Union)
                && every(
                    rest_type.as_union_or_intersection_type_interface().types(),
                    |type_: &Rc<Type>, _| self.is_tuple_type(type_),
                )
            {
                return map(
                    Some(rest_type.as_union_or_intersection_type_interface().types()),
                    |t: &Rc<Type>, _| {
                        self.expand_signature_parameters_with_tuple_members(sig, t, rest_index)
                    },
                )
                .unwrap();
            }
        }
        return vec![sig.parameters().to_owned()];
    }

    pub(super) fn expand_signature_parameters_with_tuple_members(
        &self,
        sig: &Signature,
        rest_type: &Type, /*TupleTypeReference*/
        rest_index: usize,
    ) -> Vec<Rc<Symbol>> {
        let element_types = self.get_type_arguments(rest_type);
        let rest_type_as_type_reference = rest_type.as_type_reference();
        let rest_type_target_as_tuple_type = rest_type_as_type_reference.target.as_tuple_type();
        let associated_names = rest_type_target_as_tuple_type
            .labeled_element_declarations
            .as_ref();
        let rest_params = map(Some(&element_types), |t: &Rc<Type>, i| {
            let tuple_label_name = associated_names
                .map(|associated_names| self.get_tuple_element_label(&associated_names[i]));
            let name = tuple_label_name.unwrap_or_else(|| {
                self.get_parameter_name_at_position(sig, rest_index + i, Some(rest_type))
            });
            let flags = rest_type_target_as_tuple_type.element_flags[i];
            let check_flags = if flags.intersects(ElementFlags::Variable) {
                CheckFlags::RestParameter
            } else if flags.intersects(ElementFlags::Optional) {
                CheckFlags::OptionalParameter
            } else {
                CheckFlags::None
            };
            let symbol: Rc<Symbol> = self
                .create_symbol(SymbolFlags::FunctionScopedVariable, name, Some(check_flags))
                .into();
            symbol
                .as_transient_symbol()
                .symbol_links()
                .borrow_mut()
                .type_ = Some(if flags.intersects(ElementFlags::Rest) {
                self.create_array_type(t, None)
            } else {
                t.clone()
            });
            symbol
        })
        .unwrap();
        concatenate(sig.parameters()[0..rest_index].to_owned(), rest_params)
    }

    pub(super) fn get_default_construct_signatures(
        &self,
        class_type: &Type, /*InterfaceType*/
    ) -> Vec<Rc<Signature>> {
        let base_constructor_type = self.get_base_constructor_type_of_class(class_type);
        let base_signatures =
            self.get_signatures_of_type(&base_constructor_type, SignatureKind::Construct);
        let declaration = get_class_like_declaration_of_symbol(&class_type.symbol());
        let is_abstract = matches!(declaration.as_ref(), Some(declaration) if has_syntactic_modifier(declaration, ModifierFlags::Abstract));
        if base_signatures.is_empty() {
            return vec![Rc::new(
                self.create_signature(
                    None,
                    class_type
                        .as_interface_type()
                        .maybe_local_type_parameters()
                        .map(ToOwned::to_owned),
                    None,
                    vec![],
                    Some(class_type.type_wrapper()),
                    None,
                    0,
                    if is_abstract {
                        SignatureFlags::Abstract
                    } else {
                        SignatureFlags::None
                    },
                ),
            )];
        }
        let base_type_node = self.get_base_type_node_of_class(class_type).unwrap();
        let is_java_script = is_in_js_file(Some(&*base_type_node));
        let type_arguments = self.type_arguments_from_type_reference_node(&base_type_node);
        let type_arg_count = length(type_arguments.as_deref());
        let mut result: Vec<Rc<Signature>> = vec![];
        let class_type_as_interface_type = class_type.as_interface_type();
        for base_sig in base_signatures {
            let min_type_argument_count =
                self.get_min_type_argument_count(base_sig.type_parameters.as_deref());
            let type_param_count = length(base_sig.type_parameters.as_deref());
            if is_java_script
                || type_arg_count >= min_type_argument_count && type_arg_count <= type_param_count
            {
                let mut sig = if type_param_count > 0 {
                    self.create_signature_instantiation(
                        &base_sig,
                        self.fill_missing_type_arguments(
                            type_arguments.clone(),
                            base_sig.type_parameters.as_deref(),
                            min_type_argument_count,
                            is_java_script,
                        )
                        .as_deref(),
                    )
                } else {
                    self.clone_signature(&base_sig)
                };
                sig.type_parameters = class_type_as_interface_type
                    .maybe_local_type_parameters()
                    .map(ToOwned::to_owned);
                *sig.maybe_resolved_return_type() = Some(class_type.type_wrapper());
                sig.flags = if is_abstract {
                    sig.flags | SignatureFlags::Abstract
                } else {
                    sig.flags & !SignatureFlags::Abstract
                };
                result.push(Rc::new(sig));
            }
        }
        result
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
