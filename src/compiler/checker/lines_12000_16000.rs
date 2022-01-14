#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use crate::{
    UnionType, __String, concatenate, get_name_of_declaration, get_object_flags, map,
    unescape_leading_underscores, ArrayTypeNode, BaseUnionOrIntersectionType, DiagnosticMessage,
    Diagnostics, Expression, InterfaceType, Node, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, ObjectType, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeId, TypeInterface, TypeNode, TypeReference, TypeReferenceNode,
    UnionReduction, UnionTypeNode,
};

impl TypeChecker {
    pub(super) fn get_apparent_type(&self, type_: &Type) -> Rc<Type> {
        let t = if type_.flags().intersects(TypeFlags::Instantiable) {
            unimplemented!()
        } else {
            type_.type_wrapper()
        };
        if false {
            unimplemented!()
        } else {
            t
        }
    }

    pub(super) fn get_reduced_apparent_type(&self, type_: &Type) -> Rc<Type> {
        self.get_reduced_type(&self.get_apparent_type(&self.get_reduced_type(type_)))
    }

    pub(super) fn get_reduced_type(&self, type_: &Type) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn get_property_of_type(&self, type_: &Type, name: &__String) -> Option<Rc<Symbol>> {
        let type_ = self.get_reduced_apparent_type(type_);
        if type_.flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(&type_);
            let symbol = (*resolved.as_resolved_type().members())
                .borrow()
                .get(name)
                .map(Clone::clone);
            if let Some(symbol) = symbol {
                if self.symbol_is_value(&symbol) {
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

    pub(super) fn fill_missing_type_arguments(
        &self,
        type_arguments: Option<Vec<Rc<Type>>>,
        type_parameters: Option<&[Rc<Type /*TypeParameter*/>]>,
    ) -> Option<Vec<Rc<Type>>> {
        type_arguments.map(|vec| vec.clone())
    }

    pub(super) fn get_propagating_flags_of_types(
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

    pub(super) fn create_type_reference(
        &self,
        target: &Type, /*GenericType*/
        type_arguments: Option<Vec<Rc<Type>>>,
    ) -> TypeReference {
        let type_ = self.create_object_type(ObjectFlags::Reference, &target.symbol());
        type_.set_object_flags(
            type_.object_flags()
                | if let Some(type_arguments) = type_arguments.as_ref() {
                    self.get_propagating_flags_of_types(type_arguments, TypeFlags::None)
                } else {
                    ObjectFlags::None
                },
        );
        let type_ = TypeReference::new(type_, target.type_wrapper(), type_arguments);
        type_
    }

    pub(super) fn get_type_arguments(&self, type_: &TypeReference) -> Vec<Rc<Type>> {
        let mut resolved_type_arguments = type_.resolved_type_arguments.borrow_mut();
        if resolved_type_arguments.is_none() {
            let node = type_.node.borrow();
            let type_arguments = match &*node {
                None => vec![],
                Some(node) => match &**node {
                    Node::TypeNode(TypeNode::TypeReferenceNode(type_reference_node)) => {
                        let target_as_base_interface_type = match &*type_.target {
                            Type::ObjectType(ObjectType::InterfaceType(
                                InterfaceType::BaseInterfaceType(base_interface_type),
                            )) => base_interface_type,
                            _ => panic!("Expected BaseInterfaceType"),
                        };
                        concatenate(
                            target_as_base_interface_type
                                .outer_type_parameters
                                .clone()
                                .unwrap_or_else(|| vec![]),
                            self.get_effective_type_arguments(
                                node,
                                target_as_base_interface_type
                                    .local_type_parameters
                                    .as_ref()
                                    .unwrap(),
                            ),
                        )
                    }
                    Node::TypeNode(TypeNode::ArrayTypeNode(array_type_node)) => unimplemented!(),
                    _ => unimplemented!(),
                },
            };
            if true {
                *resolved_type_arguments = if false {
                    unimplemented!()
                } else {
                    Some(type_arguments)
                };
            } else {
                unimplemented!()
            }
        }
        (*resolved_type_arguments).clone().unwrap()
    }

    pub(super) fn get_type_from_class_or_interface_reference<TNode: NodeInterface>(
        &self,
        node: &TNode,
        symbol: &Symbol,
    ) -> Rc<Type> {
        let type_ =
            self.get_declared_type_of_symbol(&self.get_merged_symbol(Some(symbol)).unwrap());
        let type_as_interface_type = match &*type_ {
            Type::ObjectType(ObjectType::InterfaceType(InterfaceType::BaseInterfaceType(
                base_interface_type,
            ))) => base_interface_type,
            _ => panic!("Expected BaseInterfaceType"),
        };
        let type_parameters = type_as_interface_type.type_parameters.as_ref();
        if let Some(type_parameters) = type_parameters {
            let type_arguments = concatenate(
                type_as_interface_type
                    .outer_type_parameters
                    .clone()
                    .unwrap_or_else(|| vec![]),
                self.fill_missing_type_arguments(
                    self.type_arguments_from_type_reference_node(&*node.node_wrapper()),
                    Some(type_parameters),
                )
                .unwrap_or_else(|| vec![]),
            );
            return self
                .create_type_reference(&type_, Some(type_arguments))
                .into();
        }
        if self.check_no_type_arguments(node, symbol) {
            type_
        } else {
            unimplemented!()
        }
    }

    pub(super) fn get_type_reference_name(
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

    pub(super) fn resolve_type_reference_name(
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

    pub(super) fn get_type_reference_type<TNode: NodeInterface>(
        &self,
        node: &TNode,
        symbol: &Symbol,
    ) -> Rc<Type> {
        if ptr::eq(symbol, Rc::as_ptr(&self.unknown_symbol())) {
            unimplemented!()
        }
        if symbol
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface)
        {
            return self.get_type_from_class_or_interface_reference(node, symbol);
        }
        let res = self.try_get_declared_type_of_symbol(symbol);
        if let Some(res) = res {
            return if self.check_no_type_arguments(node, symbol) {
                self.get_regular_type_of_literal_type(&res)
            } else {
                unimplemented!()
            };
        }
        unimplemented!()
    }

    pub(super) fn get_conditional_flow_type_of_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn check_no_type_arguments<TNode: NodeInterface>(
        &self,
        node: &TNode, /*NodeWithTypeArguments*/
        symbol: &Symbol,
    ) -> bool {
        if let Some(type_arguments) = (*node.node_wrapper())
            .as_has_type_arguments()
            .maybe_type_arguments()
        {
            unimplemented!()
        }
        true
    }

    pub(super) fn get_type_from_type_reference(&self, node: &TypeReferenceNode) -> Rc<Type> {
        let mut symbol: Option<Rc<Symbol>> = None;
        let mut type_: Option<Rc<Type>> = None;
        let meaning = SymbolFlags::Type;
        if type_.is_none() {
            symbol = Some(self.resolve_type_reference_name(node, meaning, None));
            type_ = Some(self.get_type_reference_type(node, &symbol.unwrap()));
        }
        let type_ = type_.unwrap();
        type_
    }

    pub(super) fn type_arguments_from_type_reference_node(
        &self,
        node: &Node, /*NodeWithTypeArguments*/
    ) -> Option<Vec<Rc<Type>>> {
        map(
            node.as_has_type_arguments().maybe_type_arguments(),
            |type_argument, _| self.get_type_from_type_node(&**type_argument),
        )
    }

    pub(super) fn get_type_of_global_symbol<TSymbolRef: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbolRef>,
    ) -> Rc<Type /*ObjectType*/> {
        unimplemented!()
    }

    pub(super) fn get_global_type_symbol(
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

    pub(super) fn get_global_symbol(
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

    pub(super) fn get_global_type(&self, name: &__String, report_errors: bool) -> Option<Rc<Type>> {
        let symbol = self.get_global_type_symbol(name, report_errors);
        if true {
            Some(self.get_type_of_global_symbol(symbol))
        } else {
            None
        }
    }

    pub(super) fn get_array_or_tuple_target_type(
        &self,
        node: &ArrayTypeNode,
    ) -> Rc<Type /*GenericType*/> {
        let element_type = self.get_array_element_type_node(node);
        if let Some(element_type) = element_type {
            return self.global_array_type();
        }
        unimplemented!()
    }

    pub(super) fn get_type_from_array_or_tuple_type_node(&self, node: &ArrayTypeNode) -> Rc<Type> {
        let target = self.get_array_or_tuple_target_type(node);
        if false {
            unimplemented!()
        } else if false {
            unimplemented!()
        } else {
            let element_types = vec![self.get_type_from_type_node(&*node.element_type)];
            return self.create_normalized_type_reference(&target, Some(element_types));
        }
    }

    pub(super) fn create_normalized_type_reference(
        &self,
        target: &Type, /*GenericType*/
        type_arguments: Option<Vec<Rc<Type>>>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_id(&self, type_: &Type) -> TypeId {
        type_.id()
    }

    pub(super) fn contains_type(&self, types: &[Rc<Type>], type_: &Type) -> bool {
        /*binary_search(...)*/
        types
            .iter()
            .any(|t| self.get_type_id(&t) == self.get_type_id(type_))
    }

    pub(super) fn add_type_to_union(
        &self,
        type_set: &mut Vec<Rc<Type>>,
        mut includes: TypeFlags,
        type_: &Type,
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
                type_set.push(type_.type_wrapper());
            }
        }
        includes
    }

    pub(super) fn add_types_to_union(
        &self,
        type_set: &mut Vec<Rc<Type>>,
        mut includes: TypeFlags,
        types: &[Rc<Type>],
    ) -> TypeFlags {
        for type_ in types {
            includes = self.add_type_to_union(type_set, includes, &type_);
        }
        includes
    }

    pub(super) fn get_union_type(
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

    pub(super) fn get_union_type_from_sorted_list(
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
            type_ = Some(
                UnionType::new(BaseUnionOrIntersectionType::new(
                    base_type,
                    types,
                    object_flags_to_set,
                ))
                .into(),
            );
            // TODO: also treat union type as intrinsic type with intrinsic_name = "boolean" if
            // is_boolean - should expose maybe_intrinsic_name on UnionType or something?
        }
        type_.unwrap()
    }

    pub(super) fn get_type_from_union_type_node(&self, node: &UnionTypeNode) -> Rc<Type> {
        let links = self.get_node_links(node);
        let mut links_ref = links.borrow_mut();
        if links_ref.resolved_type.is_none() {
            // let alias_symbol = self.get_alias_symbol_for_type_node(node);
            links_ref.resolved_type = Some(
                self.get_union_type(
                    map(Some(&node.types), |type_, _| {
                        self.get_type_from_type_node(type_)
                    })
                    .unwrap(),
                    Some(UnionReduction::Literal),
                ),
            );
        }
        links_ref.resolved_type.clone().unwrap()
    }

    pub(super) fn get_literal_type_from_property_name(
        &self,
        name: &Node, /*PropertyName*/
    ) -> Rc<Type> {
        if let Node::Expression(Expression::Identifier(identifier)) = name {
            self.get_string_literal_type(&unescape_leading_underscores(&identifier.escaped_text))
        } else {
            unimplemented!()
        }
    }

    pub(super) fn get_literal_type_from_property(
        &self,
        prop: &Symbol,
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

    pub(super) fn get_property_name_from_index(&self, index_type: &Type) -> Option<__String> {
        if self.is_type_usable_as_property_name(index_type) {
            Some(self.get_property_name_from_type(index_type))
        } else {
            unimplemented!()
        }
    }

    pub(super) fn get_property_type_for_index_type(
        &self,
        original_object_type: &Type,
        object_type: &Type,
        index_type: &Type,
        full_index_type: &Type,
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

    pub(super) fn get_indexed_access_type_or_undefined(
        &self,
        object_type: &Type,
        index_type: &Type,
    ) -> Option<Rc<Type>> {
        let apparent_object_type = self.get_reduced_apparent_type(object_type);
        self.get_property_type_for_index_type(
            object_type,
            &apparent_object_type,
            index_type,
            index_type,
        )
    }
}
