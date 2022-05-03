#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use crate::{
    ElementFlags, TypeFlags, __String, map, DiagnosticMessage, Diagnostics, Node, Symbol,
    SymbolFlags, Type, TypeChecker,
};

impl TypeChecker {
    pub(super) fn get_type_from_jsdoc_nullable_type_node(
        &self,
        node: &Node, /*JSDocNullableType*/
    ) -> Rc<Type> {
        let type_ =
            self.get_type_from_type_node_(node.as_base_jsdoc_unary_type().type_.as_ref().unwrap());
        if self.strict_null_checks {
            self.get_nullable_type(&type_, TypeFlags::Null)
        } else {
            type_
        }
    }

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
