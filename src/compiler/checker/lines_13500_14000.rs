#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::convert::{TryFrom, TryInto};
use std::ptr;
use std::rc::Rc;

use super::{anon, get_symbol_id, intrinsic_type_kinds};
use crate::{
    append, declaration_name_to_string, get_check_flags, get_containing_function,
    get_declaration_of_kind, get_effective_container_for_jsdoc_template_tag, index_of_rc,
    is_entity_name_expression, is_expression_with_type_arguments, is_identifier, is_in_js_file,
    is_jsdoc_augments_tag, is_jsdoc_index_signature, is_jsdoc_template_tag, is_statement,
    is_type_alias, length, maybe_concatenate, skip_parentheses,
    walk_up_parenthesized_types_and_get_parent_and_child, BaseObjectType, CheckFlags, ElementFlags,
    InterfaceTypeInterface, NodeFlags, ObjectTypeInterface, SubstitutionType, TypeFormatFlags,
    TypeId, TypeMapper, TypeReferenceInterface, TypeSystemPropertyName, __String, concatenate,
    get_object_flags, map, DiagnosticMessage, Diagnostics, Node, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, Type, TypeChecker,
    TypeFlags, TypeInterface, TypeReference,
};

impl TypeChecker {
    pub(super) fn get_type_from_type_reference(
        &self,
        node: &Node, /*TypeReferenceNode*/
    ) -> Rc<Type> {
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
            |type_argument, _| self.get_type_from_type_node_(&**type_argument),
        )
    }

    pub(super) fn get_type_of_global_symbol<TSymbolRef: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbolRef>,
    ) -> Rc<Type /*ObjectType*/> {
        unimplemented!()
    }

    pub(super) fn get_global_value_symbol(
        &self,
        name: &__String,
        report_errors: bool,
    ) -> Option<Rc<Symbol>> {
        self.get_global_symbol(
            name,
            SymbolFlags::Value,
            if report_errors {
                Some(&Diagnostics::Cannot_find_global_value_0)
            } else {
                None
            },
        )
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
                Some(&Diagnostics::Cannot_find_global_type_0)
            } else {
                None
            },
        )
    }

    pub(super) fn get_global_symbol(
        &self,
        name: &__String,
        meaning: SymbolFlags,
        diagnostic: Option<&DiagnosticMessage>,
    ) -> Option<Rc<Symbol>> {
        self.resolve_name_(
            Option::<&Node>::None,
            name,
            meaning,
            diagnostic,
            Some(name.clone()),
            false,
            None,
        )
    }

    pub(super) fn get_global_type(
        &self,
        name: &__String,
        arity: usize,
        report_errors: bool,
    ) -> Option<Rc<Type>> {
        let symbol = self.get_global_type_symbol(name, report_errors);
        if true {
            Some(self.get_type_of_global_symbol(symbol))
        } else {
            None
        }
    }

    pub(super) fn get_global_es_symbol_constructor_type_symbol(
        &self,
        report_errors: bool,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_global_es_symbol_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_promise_type(&self, report_errors: bool) -> Rc<Type /*GenericType*/> {
        let mut deferred_global_promise_type_ref = self.deferred_global_promise_type.borrow_mut();
        if let Some(deferred_global_promise_type) = deferred_global_promise_type_ref.as_ref() {
            return deferred_global_promise_type.clone();
        }
        *deferred_global_promise_type_ref =
            self.get_global_type(&__String::new("Promise".to_string()), 1, report_errors);
        deferred_global_promise_type_ref.as_ref().map_or_else(
            || self.empty_generic_type(),
            |deferred_global_promise_type| deferred_global_promise_type.clone(),
        )
    }

    pub(super) fn get_global_promise_like_type(
        &self,
        report_errors: bool,
    ) -> Rc<Type /*GenericType*/> {
        unimplemented!()
    }

    pub(super) fn get_global_promise_constructor_symbol(
        &self,
        report_errors: bool,
    ) -> Option<Rc<Symbol>> {
        let mut deferred_global_promise_constructor_symbol_ref =
            self.deferred_global_promise_constructor_symbol.borrow_mut();
        if let Some(deferred_global_promise_constructor_symbol) =
            deferred_global_promise_constructor_symbol_ref.as_ref()
        {
            return Some(deferred_global_promise_constructor_symbol.clone());
        }
        *deferred_global_promise_constructor_symbol_ref =
            self.get_global_value_symbol(&__String::new("Promise".to_string()), report_errors);
        deferred_global_promise_constructor_symbol_ref.as_ref().map(
            |deferred_global_promise_constructor_symbol| {
                deferred_global_promise_constructor_symbol.clone()
            },
        )
    }

    pub(super) fn get_global_async_iterable_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_async_iterator_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_async_iterable_iterator_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_async_generator_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_iterable_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_iterator_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_iterable_iterator_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_generator_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_global_omit_symbol(&self) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_global_big_int_type(&self, report_errors: bool) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn create_iterable_type(&self, iterated_type: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn create_array_type(
        &self,
        element_type: &Type,
        readonly: Option<bool>,
    ) -> Rc<Type /*ObjectType*/> {
        unimplemented!()
    }

    pub(super) fn get_array_or_tuple_target_type(
        &self,
        node: &Node, /*ArrayTypeNode*/
    ) -> Rc<Type /*GenericType*/> {
        let element_type = self.get_array_element_type_node(node);
        if let Some(element_type) = element_type {
            return self.global_array_type();
        }
        unimplemented!()
    }

    pub(super) fn is_deferred_type_reference_node(
        &self,
        node: &Node, /*TypeReferenceNode | ArrayTypeNode | TupleTypeNode*/
        has_default_type_arguments: Option<bool>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_type_from_array_or_tuple_type_node(
        &self,
        node: &Node, /*ArrayTypeNode*/
    ) -> Rc<Type> {
        let node_as_array_type_node = node.as_array_type_node();
        let target = self.get_array_or_tuple_target_type(node);
        if false {
            unimplemented!()
        } else if false {
            unimplemented!()
        } else {
            let element_types =
                vec![self.get_type_from_type_node_(&*node_as_array_type_node.element_type)];
            return self.create_normalized_type_reference(&target, Some(element_types));
        }
    }

    pub(super) fn create_tuple_type(
        &self,
        element_types: &[Rc<Type>],
        element_flags: Option<&[ElementFlags]>,
        readonly: Option<bool>,
        named_member_declarations: Option<&[Rc<Node /*NamedTupleMember | ParameterDeclaration*/>]>,
    ) -> Rc<Type> {
        let readonly = readonly.unwrap_or(false);
        unimplemented!()
    }
}
