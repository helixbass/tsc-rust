#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use crate::{
    create_symbol_table, find, is_assertion_expression, is_const_type_reference,
    is_this_identifier, is_type_alias_declaration, length, symbol_name, CheckFlags, ElementFlags,
    InterfaceTypeInterface, NodeInterface, SymbolInterface, SyntaxKind, TransientSymbolInterface,
    TypeFlags, TypeInterface, __String, map, DiagnosticMessage, Diagnostics, Node, Symbol,
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
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_type.is_none() {
            if is_const_type_reference(node) && is_assertion_expression(&node.parent()) {
                links.borrow_mut().resolved_symbol = Some(self.unknown_symbol());
                let ret = self
                    .check_expression_cached(&node.parent().as_has_expression().expression(), None);
                links.borrow_mut().resolved_type = Some(ret.clone());
                return ret;
            }
            let mut symbol: Option<Rc<Symbol>> = None;
            let mut type_: Option<Rc<Type>> = None;
            let meaning = SymbolFlags::Type;
            if self.is_jsdoc_type_reference(node) {
                type_ = self.get_intended_type_from_jsdoc_type_reference(node);
                if type_.is_none() {
                    symbol = Some(self.resolve_type_reference_name(node, meaning, Some(true)));
                    if Rc::ptr_eq(symbol.as_ref().unwrap(), &self.unknown_symbol()) {
                        symbol = Some(self.resolve_type_reference_name(
                            node,
                            meaning | SymbolFlags::Value,
                            None,
                        ));
                    } else {
                        self.resolve_type_reference_name(node, meaning, None);
                    }
                    type_ = Some(self.get_type_reference_type(node, symbol.as_ref().unwrap()));
                }
            }
            if type_.is_none() {
                symbol = Some(self.resolve_type_reference_name(node, meaning, None));
                type_ = Some(self.get_type_reference_type(node, symbol.as_ref().unwrap()));
            }
            {
                let mut links = links.borrow_mut();
                links.resolved_symbol = symbol;
                links.resolved_type = type_;
            }
        }
        let ret = (*links).borrow().resolved_type.clone().unwrap();
        ret
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

    pub(super) fn get_type_from_type_query_node(
        &self,
        node: &Node, /*TypeQueryNode*/
    ) -> Rc<Type> {
        let links = self.get_node_links(node);
        if (*links).borrow().resolved_type.is_none() {
            let node_as_type_query_node = node.as_type_query_node();
            let type_ = if is_this_identifier(Some(&*node_as_type_query_node.expr_name)) {
                self.check_this_expression(&node_as_type_query_node.expr_name)
            } else {
                self.check_expression(&node_as_type_query_node.expr_name, None, None)
            };
            links.borrow_mut().resolved_type =
                Some(self.get_regular_type_of_literal_type(&self.get_widened_type(&type_)));
        }
        let ret = (*links).borrow().resolved_type.clone().unwrap();
        ret
    }

    pub(super) fn get_type_of_global_symbol<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
        arity: usize,
    ) -> Rc<Type /*ObjectType*/> {
        if symbol.is_none() {
            return if arity != 0 {
                self.empty_generic_type()
            } else {
                self.empty_object_type()
            };
        }
        let symbol = symbol.unwrap();
        let symbol = symbol.borrow();
        let type_ = self.get_declared_type_of_symbol(symbol);
        if !type_.flags().intersects(TypeFlags::Object) {
            self.error(
                self.get_type_declaration(symbol),
                &Diagnostics::Global_type_0_must_be_a_class_or_interface_type,
                Some(vec![symbol_name(symbol)]),
            );
            return if arity != 0 {
                self.empty_generic_type()
            } else {
                self.empty_object_type()
            };
        }
        if length(
            type_
                .maybe_as_interface_type()
                .and_then(|type_| type_.maybe_type_parameters()),
        ) != arity
        {
            self.error(
                self.get_type_declaration(symbol),
                &Diagnostics::Global_type_0_must_have_1_type_parameter_s,
                Some(vec![symbol_name(symbol), arity.to_string()]),
            );
            return if arity != 0 {
                self.empty_generic_type()
            } else {
                self.empty_object_type()
            };
        }
        type_
    }

    pub(super) fn get_type_declaration(&self, symbol: &Symbol) -> Option<Rc<Node /*Declaration*/>> {
        let declarations = symbol.maybe_declarations();
        declarations.as_ref().and_then(|declarations| {
            for declaration in declarations {
                match declaration.kind() {
                    SyntaxKind::ClassDeclaration
                    | SyntaxKind::InterfaceDeclaration
                    | SyntaxKind::EnumDeclaration => {
                        return Some(declaration.clone());
                    }
                    _ => (),
                }
            }
            None
        })
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

    pub(super) fn get_global_type_alias_symbol(
        &self,
        name: &__String,
        arity: usize,
        report_errors: bool,
    ) -> Option<Rc<Symbol>> {
        let symbol = self.get_global_symbol(
            name,
            SymbolFlags::Type,
            if report_errors {
                Some(&Diagnostics::Cannot_find_global_type_0)
            } else {
                None
            },
        );
        if let Some(symbol) = symbol.as_ref() {
            self.get_declared_type_of_symbol(symbol);
            if length(
                (*self.get_symbol_links(symbol))
                    .borrow()
                    .type_parameters
                    .as_deref(),
            ) != arity
            {
                let symbol_declarations = symbol.maybe_declarations();
                let decl = symbol_declarations
                    .as_deref()
                    .and_then(|symbol_declarations| {
                        find(symbol_declarations, |declaration: &Rc<Node>, _| {
                            is_type_alias_declaration(declaration)
                        })
                        .map(Clone::clone)
                    });
                self.error(
                    decl,
                    &Diagnostics::Global_type_0_must_have_1_type_parameter_s,
                    Some(vec![symbol_name(symbol), arity.to_string()]),
                );
                return None;
            }
        }
        symbol
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
        if symbol.is_some() || report_errors {
            Some(self.get_type_of_global_symbol(symbol, arity))
        } else {
            None
        }
    }

    pub(super) fn get_global_typed_property_descriptor_type(&self) -> Rc<Type> {
        if self
            .maybe_deferred_global_typed_property_descriptor_type()
            .is_none()
        {
            *self.maybe_deferred_global_typed_property_descriptor_type() = Some(
                self.get_global_type(
                    &__String::new("TypedPropertyDescriptor".to_owned()),
                    1,
                    true,
                )
                .unwrap_or_else(|| self.empty_generic_type()),
            );
        }
        self.maybe_deferred_global_typed_property_descriptor_type()
            .clone()
            .unwrap()
    }

    pub(super) fn get_global_template_strings_array_type(&self) -> Rc<Type> {
        if self
            .maybe_deferred_global_template_strings_array_type()
            .is_none()
        {
            *self.maybe_deferred_global_template_strings_array_type() = Some(
                self.get_global_type(&__String::new("TemplateStringsArray".to_owned()), 0, true)
                    .unwrap_or_else(|| self.empty_object_type()),
            );
        }
        self.maybe_deferred_global_template_strings_array_type()
            .clone()
            .unwrap()
    }

    pub(super) fn get_global_import_meta_type(&self) -> Rc<Type> {
        if self.maybe_deferred_global_import_meta_type().is_none() {
            *self.maybe_deferred_global_import_meta_type() = Some(
                self.get_global_type(&__String::new("ImportMeta".to_owned()), 0, true)
                    .unwrap_or_else(|| self.empty_object_type()),
            );
        }
        self.maybe_deferred_global_import_meta_type()
            .clone()
            .unwrap()
    }

    pub(super) fn get_global_import_meta_expression_type(&self) -> Rc<Type> {
        if self
            .maybe_deferred_global_import_meta_expression_type()
            .is_none()
        {
            let symbol: Rc<Symbol> = self
                .create_symbol(
                    SymbolFlags::None,
                    __String::new("ImportMetaExpression".to_owned()),
                    None,
                )
                .into();
            let import_meta_type = self.get_global_import_meta_type();

            let meta_property_symbol: Rc<Symbol> = self
                .create_symbol(
                    SymbolFlags::Property,
                    __String::new("meta".to_owned()),
                    Some(CheckFlags::Readonly),
                )
                .into();
            meta_property_symbol.set_parent(Some(symbol.clone()));
            meta_property_symbol
                .as_transient_symbol()
                .symbol_links()
                .borrow_mut()
                .type_ = Some(import_meta_type);

            let members = Rc::new(RefCell::new(create_symbol_table(Some(&vec![
                meta_property_symbol,
            ]))));
            *symbol.maybe_members() = Some(members.clone());

            *self.maybe_deferred_global_import_meta_expression_type() = Some(
                self.create_anonymous_type(Some(symbol), members, vec![], vec![], vec![])
                    .into(),
            );
        }
        self.maybe_deferred_global_import_meta_expression_type()
            .clone()
            .unwrap()
    }

    pub(super) fn get_global_import_call_options_type(&self, report_errors: bool) -> Rc<Type> {
        if self
            .maybe_deferred_global_import_call_options_type()
            .is_none()
        {
            *self.maybe_deferred_global_import_call_options_type() = self.get_global_type(
                &__String::new("ImportCallOptions".to_owned()),
                0,
                report_errors,
            );
        }
        self.maybe_deferred_global_import_call_options_type()
            .clone()
            .unwrap_or_else(|| self.empty_object_type())
    }

    pub(super) fn get_global_es_symbol_constructor_symbol(
        &self,
        report_errors: bool,
    ) -> Option<Rc<Symbol>> {
        if self
            .maybe_deferred_global_es_symbol_constructor_symbol()
            .is_none()
        {
            *self.maybe_deferred_global_es_symbol_constructor_symbol() =
                self.get_global_value_symbol(&__String::new("Symbol".to_owned()), report_errors);
        }
        self.maybe_deferred_global_es_symbol_constructor_symbol()
            .clone()
    }

    pub(super) fn get_global_es_symbol_constructor_type_symbol(
        &self,
        report_errors: bool,
    ) -> Option<Rc<Symbol>> {
        if self
            .maybe_deferred_global_es_symbol_constructor_type_symbol()
            .is_none()
        {
            *self.maybe_deferred_global_es_symbol_constructor_type_symbol() = self
                .get_global_type_symbol(
                    &__String::new("SymbolConstructor".to_owned()),
                    report_errors,
                );
        }
        self.maybe_deferred_global_es_symbol_constructor_type_symbol()
            .clone()
    }

    pub(super) fn get_global_es_symbol_type(&self, report_errors: bool) -> Rc<Type> {
        if self.maybe_deferred_global_es_symbol_type().is_none() {
            *self.maybe_deferred_global_es_symbol_type() =
                self.get_global_type(&__String::new("Symbol".to_owned()), 0, report_errors);
        }
        self.maybe_deferred_global_es_symbol_type()
            .clone()
            .unwrap_or_else(|| self.empty_object_type())
    }

    pub(super) fn get_global_promise_type(&self, report_errors: bool) -> Rc<Type /*GenericType*/> {
        if self.maybe_deferred_global_promise_type().is_none() {
            *self.maybe_deferred_global_promise_type() =
                self.get_global_type(&__String::new("Promise".to_owned()), 1, report_errors);
        }
        self.maybe_deferred_global_promise_type()
            .clone()
            .unwrap_or_else(|| self.empty_generic_type())
    }

    pub(super) fn get_global_promise_like_type(
        &self,
        report_errors: bool,
    ) -> Rc<Type /*GenericType*/> {
        if self.maybe_deferred_global_promise_like_type().is_none() {
            *self.maybe_deferred_global_promise_like_type() =
                self.get_global_type(&__String::new("PromiseLike".to_owned()), 1, report_errors);
        }
        self.maybe_deferred_global_promise_like_type()
            .clone()
            .unwrap_or_else(|| self.empty_generic_type())
    }

    pub(super) fn get_global_promise_constructor_symbol(
        &self,
        report_errors: bool,
    ) -> Option<Rc<Symbol>> {
        if self
            .maybe_deferred_global_promise_constructor_symbol()
            .is_none()
        {
            *self.maybe_deferred_global_promise_constructor_symbol() =
                self.get_global_value_symbol(&__String::new("Promise".to_owned()), report_errors);
        }
        self.maybe_deferred_global_promise_constructor_symbol()
            .clone()
    }

    pub(super) fn get_global_promise_constructor_like_type(
        &self,
        report_errors: bool,
    ) -> Rc<Type /*GenericType*/> {
        if self
            .maybe_deferred_global_promise_constructor_like_type()
            .is_none()
        {
            *self.maybe_deferred_global_promise_constructor_like_type() = self.get_global_type(
                &__String::new("PromiseConstructorLike".to_owned()),
                0,
                report_errors,
            );
        }
        self.maybe_deferred_global_promise_constructor_like_type()
            .clone()
            .unwrap_or_else(|| self.empty_object_type())
    }

    pub(super) fn get_global_async_iterable_type(&self, report_errors: bool) -> Rc<Type> {
        if self.maybe_deferred_global_async_iterable_type().is_none() {
            *self.maybe_deferred_global_async_iterable_type() =
                self.get_global_type(&__String::new("AsyncIterable".to_owned()), 1, report_errors);
        }
        self.maybe_deferred_global_async_iterable_type()
            .clone()
            .unwrap_or_else(|| self.empty_generic_type())
    }

    pub(super) fn get_global_async_iterator_type(&self, report_errors: bool) -> Rc<Type> {
        if self.maybe_deferred_global_async_iterator_type().is_none() {
            *self.maybe_deferred_global_async_iterator_type() =
                self.get_global_type(&__String::new("AsyncIterator".to_owned()), 3, report_errors);
        }
        self.maybe_deferred_global_async_iterator_type()
            .clone()
            .unwrap_or_else(|| self.empty_generic_type())
    }

    pub(super) fn get_global_async_iterable_iterator_type(&self, report_errors: bool) -> Rc<Type> {
        if self
            .maybe_deferred_global_async_iterable_iterator_type()
            .is_none()
        {
            *self.maybe_deferred_global_async_iterable_iterator_type() = self.get_global_type(
                &__String::new("AsyncIterableIterator".to_owned()),
                1,
                report_errors,
            );
        }
        self.maybe_deferred_global_async_iterable_iterator_type()
            .clone()
            .unwrap_or_else(|| self.empty_generic_type())
    }

    pub(super) fn get_global_async_generator_type(&self, report_errors: bool) -> Rc<Type> {
        if self.maybe_deferred_global_async_generator_type().is_none() {
            *self.maybe_deferred_global_async_generator_type() = self.get_global_type(
                &__String::new("AsyncGenerator".to_owned()),
                3,
                report_errors,
            );
        }
        self.maybe_deferred_global_async_generator_type()
            .clone()
            .unwrap_or_else(|| self.empty_generic_type())
    }

    pub(super) fn get_global_iterable_type(&self, report_errors: bool) -> Rc<Type> {
        if self.maybe_deferred_global_iterable_type().is_none() {
            *self.maybe_deferred_global_iterable_type() =
                self.get_global_type(&__String::new("Iterable".to_owned()), 1, report_errors);
        }
        self.maybe_deferred_global_iterable_type()
            .clone()
            .unwrap_or_else(|| self.empty_generic_type())
    }

    pub(super) fn get_global_iterator_type(&self, report_errors: bool) -> Rc<Type> {
        if self.maybe_deferred_global_iterator_type().is_none() {
            *self.maybe_deferred_global_iterator_type() =
                self.get_global_type(&__String::new("Iterator".to_owned()), 3, report_errors);
        }
        self.maybe_deferred_global_iterator_type()
            .clone()
            .unwrap_or_else(|| self.empty_generic_type())
    }

    pub(super) fn get_global_iterable_iterator_type(&self, report_errors: bool) -> Rc<Type> {
        if self
            .maybe_deferred_global_iterable_iterator_type()
            .is_none()
        {
            *self.maybe_deferred_global_iterable_iterator_type() = self.get_global_type(
                &__String::new("IterableIterator".to_owned()),
                1,
                report_errors,
            );
        }
        self.maybe_deferred_global_iterable_iterator_type()
            .clone()
            .unwrap_or_else(|| self.empty_generic_type())
    }

    pub(super) fn get_global_generator_type(&self, report_errors: bool) -> Rc<Type> {
        if self.maybe_deferred_global_generator_type().is_none() {
            *self.maybe_deferred_global_generator_type() =
                self.get_global_type(&__String::new("Generator".to_owned()), 3, report_errors);
        }
        self.maybe_deferred_global_generator_type()
            .clone()
            .unwrap_or_else(|| self.empty_generic_type())
    }

    pub(super) fn get_global_iterator_yield_result_type(&self, report_errors: bool) -> Rc<Type> {
        if self
            .maybe_deferred_global_iterator_yield_result_type()
            .is_none()
        {
            *self.maybe_deferred_global_iterator_yield_result_type() = self.get_global_type(
                &__String::new("IteratorYieldResult".to_owned()),
                1,
                report_errors,
            );
        }
        self.maybe_deferred_global_iterator_yield_result_type()
            .clone()
            .unwrap_or_else(|| self.empty_generic_type())
    }

    pub(super) fn get_global_iterator_return_result_type(&self, report_errors: bool) -> Rc<Type> {
        if self
            .maybe_deferred_global_iterator_return_result_type()
            .is_none()
        {
            *self.maybe_deferred_global_iterator_return_result_type() = self.get_global_type(
                &__String::new("IteratorReturnResult".to_owned()),
                1,
                report_errors,
            );
        }
        self.maybe_deferred_global_iterator_return_result_type()
            .clone()
            .unwrap_or_else(|| self.empty_generic_type())
    }

    pub(super) fn get_global_type_or_undefined(
        &self,
        name: &__String,
        arity: Option<usize>,
    ) -> Option<Rc<Type /*ObjectType*/>> {
        let arity = arity.unwrap_or(0);
        let symbol = self.get_global_symbol(name, SymbolFlags::Type, None);
        symbol.map(|symbol| self.get_type_of_global_symbol(Some(symbol), arity))
    }

    pub(super) fn get_global_extract_symbol(&self) -> Option<Rc<Symbol>> {
        if self.maybe_deferred_global_extract_symbol().is_none() {
            *self.maybe_deferred_global_extract_symbol() = Some(
                self.get_global_type_alias_symbol(&__String::new("Extract".to_owned()), 2, true)
                    .unwrap_or_else(|| self.unknown_symbol()),
            );
        }
        self.maybe_deferred_global_extract_symbol()
            .as_ref()
            .map(Clone::clone)
            .filter(|deferred_global_extract_symbol| {
                !Rc::ptr_eq(deferred_global_extract_symbol, &self.unknown_symbol())
            })
    }

    pub(super) fn get_global_omit_symbol(&self) -> Option<Rc<Symbol>> {
        if self.maybe_deferred_global_omit_symbol().is_none() {
            *self.maybe_deferred_global_omit_symbol() = Some(
                self.get_global_type_alias_symbol(&__String::new("Omit".to_owned()), 2, true)
                    .unwrap_or_else(|| self.unknown_symbol()),
            );
        }
        self.maybe_deferred_global_omit_symbol()
            .as_ref()
            .map(Clone::clone)
            .filter(|deferred_global_omit_symbol| {
                !Rc::ptr_eq(deferred_global_omit_symbol, &self.unknown_symbol())
            })
    }

    pub(super) fn get_global_awaited_symbol(&self) -> Option<Rc<Symbol>> {
        if self.maybe_deferred_global_awaited_symbol().is_none() {
            *self.maybe_deferred_global_awaited_symbol() = Some(
                self.get_global_type_alias_symbol(&__String::new("Awaited".to_owned()), 1, true)
                    .unwrap_or_else(|| self.unknown_symbol()),
            );
        }
        self.maybe_deferred_global_awaited_symbol()
            .as_ref()
            .map(Clone::clone)
            .filter(|deferred_global_awaited_symbol| {
                !Rc::ptr_eq(deferred_global_awaited_symbol, &self.unknown_symbol())
            })
    }

    pub(super) fn get_global_big_int_type(&self, report_errors: bool) -> Rc<Type> {
        if self.maybe_deferred_global_big_int_type().is_none() {
            *self.maybe_deferred_global_big_int_type() =
                self.get_global_type(&__String::new("BigInt".to_owned()), 0, report_errors);
        }
        self.maybe_deferred_global_big_int_type()
            .clone()
            .unwrap_or_else(|| self.empty_object_type())
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
