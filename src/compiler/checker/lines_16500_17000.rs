#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use crate::{
    for_each_child_bool, is_part_of_type_node, some, IndexInfo, Node, NodeArray, NodeInterface,
    Symbol, SymbolInterface, SyntaxKind, Ternary, Type, TypeChecker, TypeFlags, TypeInterface,
    TypeMapper,
};

impl TypeChecker {
    pub(super) fn maybe_type_parameter_reference(&self, node: &Node) -> bool {
        !(node.parent().kind() == SyntaxKind::TypeReference && {
            let node_parent = node.parent();
            let node_parent_as_type_reference_node = node_parent.as_type_reference_node();
            node_parent_as_type_reference_node.type_arguments.is_some()
                && ptr::eq(node, &*node_parent_as_type_reference_node.type_name)
        } || node.parent().kind() == SyntaxKind::ImportType && {
            let node_parent = node.parent();
            let node_parent_as_import_type_node = node_parent.as_import_type_node();
            node_parent_as_import_type_node.type_arguments.is_some()
                && matches!(
                    node_parent_as_import_type_node.qualifier.as_deref(),
                    Some(qualifier) if ptr::eq(node, qualifier)
                )
        })
    }

    pub(super) fn is_type_parameter_possibly_referenced(
        &self,
        tp: &Type, /*TypeParameter*/
        node: &Node,
    ) -> bool {
        if let Some(tp_symbol) = tp.maybe_symbol() {
            let tp_symbol_declarations = tp_symbol.maybe_declarations();
            if let Some(tp_symbol_declarations) = tp_symbol_declarations.as_ref() {
                if tp_symbol_declarations.len() == 1 {
                    let container = tp_symbol_declarations[0].parent();
                    let mut n = Some(node.node_wrapper());
                    while !match n.as_ref() {
                        None => false,
                        Some(n) => Rc::ptr_eq(n, &container),
                    } {
                        if match n.as_ref() {
                            None => true,
                            Some(n) => {
                                n.kind() == SyntaxKind::Block
                                    || n.kind() == SyntaxKind::ConditionalType
                                        && for_each_child_bool(
                                            &n.as_conditional_type_node().extends_type,
                                            |child| self.contains_reference(tp, child),
                                            Option::<fn(&NodeArray) -> bool>::None,
                                        )
                            }
                        } {
                            return true;
                        }
                        n = n.as_ref().unwrap().maybe_parent();
                    }
                    return self.contains_reference(tp, node);
                }
            }
        }
        true
    }

    pub(super) fn contains_reference(&self, tp: &Type /*TypeParameter*/, node: &Node) -> bool {
        let tp_as_type_parameter = tp.as_type_parameter();
        match node.kind() {
            SyntaxKind::ThisType => matches!(tp_as_type_parameter.is_this_type, Some(true)),
            SyntaxKind::Identifier => {
                !matches!(tp_as_type_parameter.is_this_type, Some(true))
                    && is_part_of_type_node(node)
                    && self.maybe_type_parameter_reference(node)
                    && ptr::eq(&*self.get_type_from_type_node_worker(node), tp)
            }
            SyntaxKind::TypeQuery => true,
            SyntaxKind::MethodDeclaration | SyntaxKind::MethodSignature => {
                let node_as_function_like_declaration = node.as_function_like_declaration();
                node_as_function_like_declaration.maybe_type().is_none()
                    && node_as_function_like_declaration.maybe_body().is_some()
                    || some(
                        node_as_function_like_declaration
                            .maybe_type_parameters()
                            .as_deref(),
                        Some(|type_parameter: &Rc<Node>| {
                            self.contains_reference(tp, type_parameter)
                        }),
                    )
                    || some(
                        Some(node_as_function_like_declaration.parameters()),
                        Some(|parameter: &Rc<Node>| self.contains_reference(tp, parameter)),
                    )
                    || matches!(
                        node_as_function_like_declaration.maybe_type(),
                        Some(type_) if self.contains_reference(tp, &type_)
                    )
            }
            _ => for_each_child_bool(
                node,
                |child| self.contains_reference(tp, child),
                Option::<fn(&NodeArray) -> bool>::None,
            ),
        }
    }

    pub(super) fn get_homomorphic_type_variable(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn instantiate_mapped_type<TAliasSymbol: Borrow<Symbol>>(
        &self,
        type_: &Type, /*MappedType*/
        mapper: &TypeMapper,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn instantiate_anonymous_type<TAliasSymbol: Borrow<Symbol>>(
        &self,
        type_: &Type, /*AnonymousType*/
        mapper: TypeMapper,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type /*AnonymousType*/> {
        unimplemented!()
    }

    pub(super) fn get_conditional_type_instantiation<TAliasSymbol: Borrow<Symbol>>(
        &self,
        type_: &Type, /*ConditionalType*/
        mapper: &TypeMapper,
        alias_symbol: Option<TAliasSymbol>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn instantiate_type<TTypeRef: Borrow<Type>>(
        &self,
        type_: Option<TTypeRef>,
        mapper: Option<&TypeMapper>,
    ) -> Option<Rc<Type>> {
        match (type_.as_ref(), mapper) {
            (Some(type_), Some(mapper)) => Some(self.instantiate_type_with_alias(
                type_.borrow(),
                mapper,
                Option::<&Symbol>::None,
                None,
            )),
            _ => type_.map(|type_| type_.borrow().type_wrapper()),
        }
    }

    pub(super) fn instantiate_type_with_alias<TSymbolRef: Borrow<Symbol>>(
        &self,
        type_: &Type,
        mapper: &TypeMapper,
        alias_symbol: Option<TSymbolRef>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let result =
            self.instantiate_type_worker(type_, mapper, alias_symbol, alias_type_arguments);
        result
    }

    pub(super) fn instantiate_type_worker<TSymbolRef: Borrow<Symbol>>(
        &self,
        type_: &Type,
        mapper: &TypeMapper,
        alias_symbol: Option<TSymbolRef>,
        alias_type_arguments: Option<&[Rc<Type>]>,
    ) -> Rc<Type> {
        let flags = type_.flags();
        if flags.intersects(TypeFlags::TypeParameter) {
            return self.get_mapped_type(type_, mapper);
        }
        unimplemented!()
    }

    pub(super) fn get_permissive_instantiation(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_restrictive_instantiation(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn instantiate_index_info(
        &self,
        info: &IndexInfo,
        mapper: &TypeMapper,
    ) -> Rc<IndexInfo> {
        unimplemented!()
    }

    pub(super) fn is_context_sensitive(
        &self,
        node: &Node, /*Expression | MethodDeclaration | ObjectLiteralElementLike | JsxAttributeLike | JsxChild*/
    ) -> bool {
        // match node {
        // }
        false
    }

    pub(super) fn is_type_identical_to(&self, source: &Type, target: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn compare_types_identical(&self, source: &Type, target: &Type) -> Ternary {
        unimplemented!()
    }

    pub(super) fn compare_types_subtype_of(&self, source: &Type, target: &Type) -> Ternary {
        unimplemented!()
    }

    pub(super) fn is_type_subtype_of(&self, source: &Type, target: &Type) -> bool {
        self.is_type_related_to(source, target, &self.subtype_relation())
    }

    pub(super) fn is_type_assignable_to(&self, source: &Type, target: &Type) -> bool {
        self.is_type_related_to(source, target, &self.assignable_relation())
    }

    pub(super) fn is_type_derived_from(&self, source: &Type, target: &Type) -> bool {
        unimplemented!()
    }
}
