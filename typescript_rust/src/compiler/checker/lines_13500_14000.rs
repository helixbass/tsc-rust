use std::{collections::HashMap, io};

use id_arena::Id;

use super::get_node_id;
use crate::{
    count_where, create_symbol_table, find, is_assertion_expression, is_const_type_reference,
    is_this_identifier, is_type_alias_declaration, is_type_operator_node, length, map, released,
    some, symbol_name, try_map, try_maybe_map, try_some, AsDoubleDeref, BaseInterfaceType,
    CheckFlags, DiagnosticMessage, Diagnostics, ElementFlags, GenericableTypeInterface, HasArena,
    HasTypeArgumentsInterface, InArena, InterfaceTypeInterface,
    InterfaceTypeWithDeclaredMembersInterface, Node, NodeInterface, Number, ObjectFlags,
    OptionInArena, OptionTry, Symbol, SymbolFlags, SymbolInterface, SyntaxKind,
    TransientSymbolInterface, TupleType, Type, TypeChecker, TypeFlags, TypeInterface,
    TypeReferenceInterface,
};

impl TypeChecker {
    pub(super) fn get_type_from_jsdoc_nullable_type_node(
        &self,
        node: Id<Node>, /*JSDocNullableType*/
    ) -> io::Result<Id<Type>> {
        let type_ = self
            .get_type_from_type_node_(node.ref_(self).as_base_jsdoc_unary_type().type_.unwrap())?;
        Ok(if self.strict_null_checks {
            self.get_nullable_type(type_, TypeFlags::Null)?
        } else {
            type_
        })
    }

    pub(super) fn get_type_from_type_reference(
        &self,
        node: Id<Node>, /*TypeReferenceNode*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            if is_const_type_reference(node, self)
                && is_assertion_expression(&node.ref_(self).parent().ref_(self))
            {
                links.ref_mut(self).resolved_symbol = Some(self.unknown_symbol());
                let ret = self.check_expression_cached(
                    node.ref_(self)
                        .parent()
                        .ref_(self)
                        .as_has_expression()
                        .expression(),
                    None,
                )?;
                links.ref_mut(self).resolved_type = Some(ret.clone());
                return Ok(ret);
            }
            let mut symbol: Option<Id<Symbol>> = None;
            let mut type_: Option<Id<Type>> = None;
            let meaning = SymbolFlags::Type;
            if self.is_jsdoc_type_reference(node) {
                type_ = self.get_intended_type_from_jsdoc_type_reference(node)?;
                if type_.is_none() {
                    symbol = Some(self.resolve_type_reference_name(node, meaning, Some(true))?);
                    if symbol.unwrap() == self.unknown_symbol() {
                        symbol = Some(self.resolve_type_reference_name(
                            node,
                            meaning | SymbolFlags::Value,
                            None,
                        )?);
                    } else {
                        self.resolve_type_reference_name(node, meaning, None)?;
                    }
                    type_ = Some(self.get_type_reference_type(node, symbol.unwrap())?);
                }
            }
            if type_.is_none() {
                symbol = Some(self.resolve_type_reference_name(node, meaning, None)?);
                type_ = Some(self.get_type_reference_type(node, symbol.unwrap())?);
            }
            {
                let mut links = links.ref_mut(self);
                links.resolved_symbol = symbol;
                links.resolved_type = type_;
            }
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn type_arguments_from_type_reference_node(
        &self,
        node: Id<Node>, /*NodeWithTypeArguments*/
    ) -> io::Result<Option<Vec<Id<Type>>>> {
        try_maybe_map(
            node.ref_(self)
                .as_has_type_arguments()
                .maybe_type_arguments()
                .refed(self)
                .as_deref(),
            |&type_argument, _| self.get_type_from_type_node_(type_argument),
        )
        .transpose()
    }

    pub(super) fn get_type_from_type_query_node(
        &self,
        node: Id<Node>, /*TypeQueryNode*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            let type_ = if is_this_identifier(Some(
                &node.ref_(self).as_type_query_node().expr_name.ref_(self),
            )) {
                self.check_this_expression(node.ref_(self).as_type_query_node().expr_name)?
            } else {
                self.check_expression(
                    released!(node.ref_(self).as_type_query_node().expr_name),
                    None,
                    None,
                )?
            };
            links.ref_mut(self).resolved_type =
                Some(self.get_regular_type_of_literal_type(self.get_widened_type(type_)?));
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_type_of_global_symbol(
        &self,
        symbol: Option<Id<Symbol>>,
        arity: usize,
    ) -> io::Result<Id<Type /*ObjectType*/>> {
        if symbol.is_none() {
            return Ok(if arity != 0 {
                self.empty_generic_type()
            } else {
                self.empty_object_type()
            });
        }
        let symbol = symbol.unwrap();
        let type_ = self.get_declared_type_of_symbol(symbol)?;
        if !type_.ref_(self).flags().intersects(TypeFlags::Object) {
            self.error(
                self.get_type_declaration(symbol),
                &Diagnostics::Global_type_0_must_be_a_class_or_interface_type,
                Some(vec![symbol_name(symbol, self)]),
            );
            return Ok(if arity != 0 {
                self.empty_generic_type()
            } else {
                self.empty_object_type()
            });
        }
        if length(
            type_
                .ref_(self)
                .maybe_as_interface_type()
                .and_then(|type_| type_.maybe_type_parameters()),
        ) != arity
        {
            self.error(
                self.get_type_declaration(symbol),
                &Diagnostics::Global_type_0_must_have_1_type_parameter_s,
                Some(vec![symbol_name(symbol, self), arity.to_string()]),
            );
            return Ok(if arity != 0 {
                self.empty_generic_type()
            } else {
                self.empty_object_type()
            });
        }
        Ok(type_)
    }

    pub(super) fn get_type_declaration(
        &self,
        symbol: Id<Symbol>,
    ) -> Option<Id<Node /*Declaration*/>> {
        let symbol_ref = symbol.ref_(self);
        let declarations = symbol_ref.maybe_declarations();
        declarations.as_ref().and_then(|declarations| {
            for declaration in declarations {
                match declaration.ref_(self).kind() {
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
        name: &str, /*__String*/
        report_errors: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
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
        name: &str, /*__String*/
        report_errors: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
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
        name: &str, /*__String*/
        arity: usize,
        report_errors: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        let symbol = self.get_global_symbol(
            name,
            SymbolFlags::Type,
            if report_errors {
                Some(&Diagnostics::Cannot_find_global_type_0)
            } else {
                None
            },
        )?;
        if let Some(symbol) = symbol {
            self.get_declared_type_of_symbol(symbol)?;
            if length(
                self.get_symbol_links(symbol)
                    .ref_(self)
                    .type_parameters
                    .as_deref(),
            ) != arity
            {
                let symbol_ref = symbol.ref_(self);
                let symbol_declarations = symbol_ref.maybe_declarations();
                let decl = symbol_declarations
                    .as_deref()
                    .and_then(|symbol_declarations| {
                        find(symbol_declarations, |declaration: &Id<Node>, _| {
                            is_type_alias_declaration(&declaration.ref_(self))
                        })
                        .copied()
                    });
                self.error(
                    decl,
                    &Diagnostics::Global_type_0_must_have_1_type_parameter_s,
                    Some(vec![symbol_name(symbol, self), arity.to_string()]),
                );
                return Ok(None);
            }
        }
        Ok(symbol)
    }

    pub(super) fn get_global_symbol(
        &self,
        name: &str, /*__String*/
        meaning: SymbolFlags,
        diagnostic: Option<&DiagnosticMessage>,
    ) -> io::Result<Option<Id<Symbol>>> {
        self.resolve_name_(
            Option::<Id<Node>>::None,
            name,
            meaning,
            diagnostic,
            Some(name),
            false,
            None,
        )
    }

    pub(super) fn get_global_type(
        &self,
        name: &str, /*__String*/
        arity: usize,
        report_errors: bool,
    ) -> io::Result<Option<Id<Type>>> {
        let symbol = self.get_global_type_symbol(name, report_errors)?;
        Ok(if symbol.is_some() || report_errors {
            Some(self.get_type_of_global_symbol(symbol, arity)?)
        } else {
            None
        })
    }

    pub(super) fn get_global_typed_property_descriptor_type(&self) -> io::Result<Id<Type>> {
        if self
            .maybe_deferred_global_typed_property_descriptor_type()
            .is_none()
        {
            self.set_deferred_global_typed_property_descriptor_type(Some(
                self.get_global_type("TypedPropertyDescriptor", 1, true)?
                    .unwrap_or_else(|| self.empty_generic_type()),
            ));
        }
        Ok(self
            .maybe_deferred_global_typed_property_descriptor_type()
            .unwrap())
    }

    pub(super) fn get_global_template_strings_array_type(&self) -> io::Result<Id<Type>> {
        if self
            .maybe_deferred_global_template_strings_array_type()
            .is_none()
        {
            self.set_deferred_global_template_strings_array_type(Some(
                self.get_global_type("TemplateStringsArray", 0, true)?
                    .unwrap_or_else(|| self.empty_object_type()),
            ));
        }
        Ok(self
            .maybe_deferred_global_template_strings_array_type()
            .unwrap())
    }

    pub(super) fn get_global_import_meta_type(&self) -> io::Result<Id<Type>> {
        if self.maybe_deferred_global_import_meta_type().is_none() {
            self.set_deferred_global_import_meta_type(Some(
                self.get_global_type("ImportMeta", 0, true)?
                    .unwrap_or_else(|| self.empty_object_type()),
            ));
        }
        Ok(self.maybe_deferred_global_import_meta_type().unwrap())
    }

    pub(super) fn get_global_import_meta_expression_type(&self) -> io::Result<Id<Type>> {
        if self
            .maybe_deferred_global_import_meta_expression_type()
            .is_none()
        {
            let symbol = self.alloc_symbol(
                self.create_symbol(SymbolFlags::None, "ImportMetaExpression".to_owned(), None)
                    .into(),
            );
            let import_meta_type = self.get_global_import_meta_type()?;

            let meta_property_symbol = self.alloc_symbol(
                self.create_symbol(
                    SymbolFlags::Property,
                    "meta".to_owned(),
                    Some(CheckFlags::Readonly),
                )
                .into(),
            );
            meta_property_symbol
                .ref_(self)
                .set_parent(Some(symbol.clone()));
            meta_property_symbol
                .ref_(self)
                .as_transient_symbol()
                .symbol_links()
                .ref_mut(self)
                .type_ = Some(import_meta_type);

            let members = self
                .alloc_symbol_table(create_symbol_table(Some(&vec![meta_property_symbol]), self));
            symbol.ref_(self).set_members(Some(members));

            self.set_deferred_global_import_meta_expression_type(Some(
                self.create_anonymous_type(Some(symbol), members, vec![], vec![], vec![])?,
            ));
        }
        Ok(self
            .maybe_deferred_global_import_meta_expression_type()
            .unwrap())
    }

    pub(super) fn get_global_import_call_options_type(
        &self,
        report_errors: bool,
    ) -> io::Result<Id<Type>> {
        if self
            .maybe_deferred_global_import_call_options_type()
            .is_none()
        {
            self.set_deferred_global_import_call_options_type(self.get_global_type(
                "ImportCallOptions",
                0,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_import_call_options_type()
            .unwrap_or_else(|| self.empty_object_type()))
    }

    pub(super) fn get_global_es_symbol_constructor_symbol(
        &self,
        report_errors: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        if self
            .maybe_deferred_global_es_symbol_constructor_symbol()
            .is_none()
        {
            self.set_deferred_global_es_symbol_constructor_symbol(
                self.get_global_value_symbol("Symbol", report_errors)?,
            );
        }
        Ok(self.maybe_deferred_global_es_symbol_constructor_symbol())
    }

    pub(super) fn get_global_es_symbol_constructor_type_symbol(
        &self,
        report_errors: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        if self
            .maybe_deferred_global_es_symbol_constructor_type_symbol()
            .is_none()
        {
            self.set_deferred_global_es_symbol_constructor_type_symbol(
                self.get_global_type_symbol("SymbolConstructor", report_errors)?,
            );
        }
        Ok(self.maybe_deferred_global_es_symbol_constructor_type_symbol())
    }

    pub(super) fn get_global_es_symbol_type(&self, report_errors: bool) -> io::Result<Id<Type>> {
        if self.maybe_deferred_global_es_symbol_type().is_none() {
            self.set_deferred_global_es_symbol_type(self.get_global_type(
                "Symbol",
                0,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_es_symbol_type()
            .unwrap_or_else(|| self.empty_object_type()))
    }

    pub(super) fn get_global_promise_type(
        &self,
        report_errors: bool,
    ) -> io::Result<Id<Type /*GenericType*/>> {
        if self.maybe_deferred_global_promise_type().is_none() {
            self.set_deferred_global_promise_type(self.get_global_type(
                "Promise",
                1,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_promise_type()
            .unwrap_or_else(|| self.empty_generic_type()))
    }

    pub(super) fn get_global_promise_like_type(
        &self,
        report_errors: bool,
    ) -> io::Result<Id<Type /*GenericType*/>> {
        if self.maybe_deferred_global_promise_like_type().is_none() {
            self.set_deferred_global_promise_like_type(self.get_global_type(
                "PromiseLike",
                1,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_promise_like_type()
            .unwrap_or_else(|| self.empty_generic_type()))
    }

    pub(super) fn get_global_promise_constructor_symbol(
        &self,
        report_errors: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        if self
            .maybe_deferred_global_promise_constructor_symbol()
            .is_none()
        {
            self.set_deferred_global_promise_constructor_symbol(
                self.get_global_value_symbol("Promise", report_errors)?,
            );
        }
        Ok(self.maybe_deferred_global_promise_constructor_symbol())
    }

    pub(super) fn get_global_promise_constructor_like_type(
        &self,
        report_errors: bool,
    ) -> io::Result<Id<Type /*GenericType*/>> {
        if self
            .maybe_deferred_global_promise_constructor_like_type()
            .is_none()
        {
            self.set_deferred_global_promise_constructor_like_type(self.get_global_type(
                "PromiseConstructorLike",
                0,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_promise_constructor_like_type()
            .unwrap_or_else(|| self.empty_object_type()))
    }

    pub(super) fn get_global_async_iterable_type(
        &self,
        report_errors: bool,
    ) -> io::Result<Id<Type>> {
        if self.maybe_deferred_global_async_iterable_type().is_none() {
            self.set_deferred_global_async_iterable_type(self.get_global_type(
                "AsyncIterable",
                1,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_async_iterable_type()
            .unwrap_or_else(|| self.empty_generic_type()))
    }

    pub(super) fn get_global_async_iterator_type(
        &self,
        report_errors: bool,
    ) -> io::Result<Id<Type>> {
        if self.maybe_deferred_global_async_iterator_type().is_none() {
            self.set_deferred_global_async_iterator_type(self.get_global_type(
                "AsyncIterator",
                3,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_async_iterator_type()
            .unwrap_or_else(|| self.empty_generic_type()))
    }

    pub(super) fn get_global_async_iterable_iterator_type(
        &self,
        report_errors: bool,
    ) -> io::Result<Id<Type>> {
        if self
            .maybe_deferred_global_async_iterable_iterator_type()
            .is_none()
        {
            self.set_deferred_global_async_iterable_iterator_type(self.get_global_type(
                "AsyncIterableIterator",
                1,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_async_iterable_iterator_type()
            .clone()
            .unwrap_or_else(|| self.empty_generic_type()))
    }

    pub(super) fn get_global_async_generator_type(
        &self,
        report_errors: bool,
    ) -> io::Result<Id<Type>> {
        if self.maybe_deferred_global_async_generator_type().is_none() {
            self.set_deferred_global_async_generator_type(self.get_global_type(
                "AsyncGenerator",
                3,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_async_generator_type()
            .unwrap_or_else(|| self.empty_generic_type()))
    }

    pub(super) fn get_global_iterable_type(&self, report_errors: bool) -> io::Result<Id<Type>> {
        if self.maybe_deferred_global_iterable_type().is_none() {
            self.set_deferred_global_iterable_type(self.get_global_type(
                "Iterable",
                1,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_iterable_type()
            .unwrap_or_else(|| self.empty_generic_type()))
    }

    pub(super) fn get_global_iterator_type(&self, report_errors: bool) -> io::Result<Id<Type>> {
        if self.maybe_deferred_global_iterator_type().is_none() {
            self.set_deferred_global_iterator_type(self.get_global_type(
                "Iterator",
                3,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_iterator_type()
            .unwrap_or_else(|| self.empty_generic_type()))
    }

    pub(super) fn get_global_iterable_iterator_type(
        &self,
        report_errors: bool,
    ) -> io::Result<Id<Type>> {
        if self
            .maybe_deferred_global_iterable_iterator_type()
            .is_none()
        {
            self.set_deferred_global_iterable_iterator_type(self.get_global_type(
                "IterableIterator",
                1,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_iterable_iterator_type()
            .unwrap_or_else(|| self.empty_generic_type()))
    }

    pub(super) fn get_global_generator_type(&self, report_errors: bool) -> io::Result<Id<Type>> {
        if self.maybe_deferred_global_generator_type().is_none() {
            self.set_deferred_global_generator_type(self.get_global_type(
                "Generator",
                3,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_generator_type()
            .unwrap_or_else(|| self.empty_generic_type()))
    }

    pub(super) fn get_global_iterator_yield_result_type(
        &self,
        report_errors: bool,
    ) -> io::Result<Id<Type>> {
        if self
            .maybe_deferred_global_iterator_yield_result_type()
            .is_none()
        {
            self.set_deferred_global_iterator_yield_result_type(self.get_global_type(
                "IteratorYieldResult",
                1,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_iterator_yield_result_type()
            .unwrap_or_else(|| self.empty_generic_type()))
    }

    pub(super) fn get_global_iterator_return_result_type(
        &self,
        report_errors: bool,
    ) -> io::Result<Id<Type>> {
        if self
            .maybe_deferred_global_iterator_return_result_type()
            .is_none()
        {
            self.set_deferred_global_iterator_return_result_type(self.get_global_type(
                "IteratorReturnResult",
                1,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_iterator_return_result_type()
            .unwrap_or_else(|| self.empty_generic_type()))
    }

    pub(super) fn get_global_type_or_undefined(
        &self,
        name: &str, /*__String*/
        arity: Option<usize>,
    ) -> io::Result<Option<Id<Type /*ObjectType*/>>> {
        let arity = arity.unwrap_or(0);
        let symbol = self.get_global_symbol(name, SymbolFlags::Type, None)?;
        symbol.try_map(|symbol| self.get_type_of_global_symbol(Some(symbol), arity))
    }

    pub(super) fn get_global_extract_symbol(&self) -> io::Result<Option<Id<Symbol>>> {
        if self.maybe_deferred_global_extract_symbol().is_none() {
            self.set_deferred_global_extract_symbol(Some(
                self.get_global_type_alias_symbol("Extract", 2, true)?
                    .unwrap_or_else(|| self.unknown_symbol()),
            ));
        }
        Ok(self
            .maybe_deferred_global_extract_symbol()
            .filter(|&deferred_global_extract_symbol| {
                deferred_global_extract_symbol != self.unknown_symbol()
            }))
    }

    pub(super) fn get_global_omit_symbol(&self) -> io::Result<Option<Id<Symbol>>> {
        if self.maybe_deferred_global_omit_symbol().is_none() {
            self.set_deferred_global_omit_symbol(Some(
                self.get_global_type_alias_symbol("Omit", 2, true)?
                    .unwrap_or_else(|| self.unknown_symbol()),
            ));
        }
        Ok(self
            .maybe_deferred_global_omit_symbol()
            .filter(|&deferred_global_omit_symbol| {
                deferred_global_omit_symbol != self.unknown_symbol()
            }))
    }

    pub(super) fn get_global_awaited_symbol(
        &self,
        report_errors: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        if self.maybe_deferred_global_awaited_symbol().is_none() {
            self.set_deferred_global_awaited_symbol(
                self.get_global_type_alias_symbol("Awaited", 1, report_errors)?
                    .or_else(|| {
                        if report_errors {
                            Some(self.unknown_symbol())
                        } else {
                            None
                        }
                    }),
            );
        }
        Ok(self
            .maybe_deferred_global_awaited_symbol()
            .filter(|&deferred_global_awaited_symbol| {
                deferred_global_awaited_symbol != self.unknown_symbol()
            }))
    }

    pub(super) fn get_global_big_int_type(&self, report_errors: bool) -> io::Result<Id<Type>> {
        if self.maybe_deferred_global_big_int_type().is_none() {
            self.set_deferred_global_big_int_type(self.get_global_type(
                "BigInt",
                0,
                report_errors,
            )?);
        }
        Ok(self
            .maybe_deferred_global_big_int_type()
            .unwrap_or_else(|| self.empty_object_type()))
    }

    pub(super) fn create_type_from_generic_global_type(
        &self,
        generic_global_type: Id<Type>, /*GenericType*/
        type_arguments: Vec<Id<Type>>,
    ) -> Id<Type /*ObjectType*/> {
        if generic_global_type != self.empty_generic_type() {
            self.create_type_reference(generic_global_type, Some(type_arguments))
        } else {
            self.empty_object_type()
        }
    }

    pub(super) fn create_typed_property_descriptor_type(
        &self,
        property_type: Id<Type>,
    ) -> io::Result<Id<Type>> {
        Ok(self.create_type_from_generic_global_type(
            self.get_global_typed_property_descriptor_type()?,
            vec![property_type],
        ))
    }

    pub(super) fn create_iterable_type(&self, iterated_type: Id<Type>) -> io::Result<Id<Type>> {
        Ok(self.create_type_from_generic_global_type(
            self.get_global_iterable_type(true)?,
            vec![iterated_type],
        ))
    }

    pub(super) fn create_array_type(
        &self,
        element_type: Id<Type>,
        readonly: Option<bool>,
    ) -> Id<Type /*ObjectType*/> {
        self.create_type_from_generic_global_type(
            if matches!(readonly, Some(true)) {
                self.global_readonly_array_type()
            } else {
                self.global_array_type()
            },
            vec![element_type],
        )
    }

    pub(super) fn get_tuple_element_flags(&self, node: Id<Node> /*TypeNode*/) -> ElementFlags {
        match node.ref_(self).kind() {
            SyntaxKind::OptionalType => ElementFlags::Optional,
            SyntaxKind::RestType => self.get_rest_type_element_flags(node),
            SyntaxKind::NamedTupleMember => {
                let node_ref = node.ref_(self);
                let node_as_named_tuple_member = node_ref.as_named_tuple_member();
                if node_as_named_tuple_member.question_token.is_some() {
                    ElementFlags::Optional
                } else if node_as_named_tuple_member.dot_dot_dot_token.is_some() {
                    self.get_rest_type_element_flags(node)
                } else {
                    ElementFlags::Required
                }
            }
            _ => ElementFlags::Required,
        }
    }

    pub(super) fn get_rest_type_element_flags(
        &self,
        node: Id<Node>, /*RestTypeNode | NamedTupleMember*/
    ) -> ElementFlags {
        if self
            .get_array_element_type_node(node.ref_(self).as_has_type().maybe_type().unwrap())
            .is_some()
        {
            ElementFlags::Rest
        } else {
            ElementFlags::Variadic
        }
    }

    pub(super) fn get_array_or_tuple_target_type(
        &self,
        node: Id<Node>, /*ArrayTypeNode | TupleTypeNode*/
    ) -> io::Result<Id<Type /*GenericType*/>> {
        let readonly = self.is_readonly_type_operator(node.ref_(self).parent());
        let element_type = self.get_array_element_type_node(node);
        if element_type.is_some() {
            return Ok(if readonly {
                self.global_readonly_array_type()
            } else {
                self.global_array_type()
            });
        }
        let node_ref = node.ref_(self);
        let node_as_tuple_type_node = node_ref.as_tuple_type_node();
        let element_flags = map(
            &*node_as_tuple_type_node.elements.ref_(self),
            |&element: &Id<Node>, _| self.get_tuple_element_flags(element),
        );
        let missing_name = some(
            Some(&*node_as_tuple_type_node.elements.ref_(self)),
            Some(|e: &Id<Node>| e.ref_(self).kind() != SyntaxKind::NamedTupleMember),
        );
        let node_elements_ref = node_as_tuple_type_node.elements.ref_(self);
        self.get_tuple_target_type(
            &element_flags,
            readonly,
            if missing_name {
                None
            } else {
                Some(&node_elements_ref)
            },
        )
    }

    pub(super) fn is_deferred_type_reference_node(
        &self,
        node: Id<Node>, /*TypeReferenceNode | ArrayTypeNode | TupleTypeNode*/
        has_default_type_arguments: Option<bool>,
    ) -> io::Result<bool> {
        Ok(self.get_alias_symbol_for_type_node(node)?.is_some()
            || self.is_resolved_by_type_alias(node)
                && if node.ref_(self).kind() == SyntaxKind::ArrayType {
                    self.may_resolve_type_alias(node.ref_(self).as_array_type_node().element_type)?
                } else if node.ref_(self).kind() == SyntaxKind::TupleType {
                    try_some(
                        Some(&*node.ref_(self).as_tuple_type_node().elements.ref_(self)),
                        Some(|&element: &Id<Node>| self.may_resolve_type_alias(element)),
                    )?
                } else {
                    matches!(has_default_type_arguments, Some(true))
                        || try_some(
                            node.ref_(self)
                                .as_type_reference_node()
                                .maybe_type_arguments()
                                .refed(self)
                                .as_double_deref(),
                            Some(|&type_argument: &Id<Node>| {
                                self.may_resolve_type_alias(type_argument)
                            }),
                        )?
                })
    }

    pub(super) fn is_resolved_by_type_alias(&self, node: Id<Node>) -> bool {
        let parent = node.ref_(self).parent();
        match parent.ref_(self).kind() {
            SyntaxKind::ParenthesizedType
            | SyntaxKind::NamedTupleMember
            | SyntaxKind::TypeReference
            | SyntaxKind::UnionType
            | SyntaxKind::IntersectionType
            | SyntaxKind::IndexedAccessType
            | SyntaxKind::ConditionalType
            | SyntaxKind::TypeOperator
            | SyntaxKind::ArrayType
            | SyntaxKind::TupleType => self.is_resolved_by_type_alias(parent),
            SyntaxKind::TypeAliasDeclaration => true,
            _ => false,
        }
    }

    pub(super) fn may_resolve_type_alias(&self, node: Id<Node>) -> io::Result<bool> {
        Ok(match node.ref_(self).kind() {
            SyntaxKind::TypeReference => {
                self.is_jsdoc_type_reference(node)
                    || self
                        .resolve_type_reference_name(node, SymbolFlags::Type, None)?
                        .ref_(self)
                        .flags()
                        .intersects(SymbolFlags::TypeAlias)
            }
            SyntaxKind::TypeQuery => true,
            SyntaxKind::TypeOperator => {
                let node_ref = node.ref_(self);
                let node_as_type_operator_node = node_ref.as_type_operator_node();
                node_as_type_operator_node.operator != SyntaxKind::UniqueKeyword
                    && self.may_resolve_type_alias(node_as_type_operator_node.type_)?
            }
            SyntaxKind::ParenthesizedType
            | SyntaxKind::OptionalType
            | SyntaxKind::NamedTupleMember
            | SyntaxKind::JSDocOptionalType
            | SyntaxKind::JSDocNullableType
            | SyntaxKind::JSDocNonNullableType
            | SyntaxKind::JSDocTypeExpression => {
                self.may_resolve_type_alias(node.ref_(self).as_has_type().maybe_type().unwrap())?
            }
            SyntaxKind::RestType => {
                let node_ref = node.ref_(self);
                let node_as_rest_type_node = node_ref.as_rest_type_node();
                node_as_rest_type_node.type_.ref_(self).kind() != SyntaxKind::ArrayType
                    || self.may_resolve_type_alias(
                        node_as_rest_type_node
                            .type_
                            .ref_(self)
                            .as_array_type_node()
                            .element_type,
                    )?
            }
            SyntaxKind::UnionType | SyntaxKind::IntersectionType => try_some(
                Some(
                    &*node
                        .ref_(self)
                        .as_union_or_intersection_type_node()
                        .types()
                        .ref_(self),
                ),
                Some(|&type_: &Id<Node>| self.may_resolve_type_alias(type_)),
            )?,
            SyntaxKind::IndexedAccessType => {
                let node_ref = node.ref_(self);
                let node_as_indexed_access_type_node = node_ref.as_indexed_access_type_node();
                self.may_resolve_type_alias(node_as_indexed_access_type_node.object_type)?
                    || self.may_resolve_type_alias(node_as_indexed_access_type_node.index_type)?
            }
            SyntaxKind::ConditionalType => {
                let node_ref = node.ref_(self);
                let node_as_conditional_type_node = node_ref.as_conditional_type_node();
                self.may_resolve_type_alias(node_as_conditional_type_node.check_type)?
                    || self.may_resolve_type_alias(node_as_conditional_type_node.extends_type)?
                    || self.may_resolve_type_alias(node_as_conditional_type_node.true_type)?
                    || self.may_resolve_type_alias(node_as_conditional_type_node.false_type)?
            }
            _ => false,
        })
    }

    pub(super) fn get_type_from_array_or_tuple_type_node(
        &self,
        node: Id<Node>, /*ArrayTypeNode*/
    ) -> io::Result<Id<Type>> {
        let links = self.get_node_links(node);
        if links.ref_(self).resolved_type.is_none() {
            let target = self.get_array_or_tuple_target_type(node)?;
            if target == self.empty_generic_type() {
                links.ref_mut(self).resolved_type = Some(self.empty_object_type());
            } else if !(node.ref_(self).kind() == SyntaxKind::TupleType
                && some(
                    Some(&*node.ref_(self).as_tuple_type_node().elements.ref_(self)),
                    Some(|&e: &Id<Node>| {
                        self.get_tuple_element_flags(e)
                            .intersects(ElementFlags::Variadic)
                    }),
                ))
                && self.is_deferred_type_reference_node(node, None)?
            {
                links.ref_mut(self).resolved_type = Some(
                    if node.ref_(self).kind() == SyntaxKind::TupleType
                        && node
                            .ref_(self)
                            .as_tuple_type_node()
                            .elements
                            .ref_(self)
                            .is_empty()
                    {
                        target
                    } else {
                        self.create_deferred_type_reference(
                            target,
                            node,
                            None,
                            Option::<Id<Symbol>>::None,
                            None,
                        )?
                    },
                );
            } else {
                let element_types = if node.ref_(self).kind() == SyntaxKind::ArrayType {
                    vec![self.get_type_from_type_node_(
                        node.ref_(self).as_array_type_node().element_type,
                    )?]
                } else {
                    try_map(
                        &*node.ref_(self).as_tuple_type_node().elements.ref_(self),
                        |&element: &Id<Node>, _| self.get_type_from_type_node_(element),
                    )?
                };
                links.ref_mut(self).resolved_type =
                    Some(self.create_normalized_type_reference(target, Some(element_types))?);
            }
        }
        let ret = links.ref_(self).resolved_type.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn is_readonly_type_operator(&self, node: Id<Node>) -> bool {
        is_type_operator_node(&node.ref_(self))
            && node.ref_(self).as_type_operator_node().operator == SyntaxKind::ReadonlyKeyword
    }

    pub(super) fn create_tuple_type(
        &self,
        element_types: &[Id<Type>],
        element_flags: Option<&[ElementFlags]>,
        readonly: Option<bool>,
        named_member_declarations: Option<&[Id<Node /*NamedTupleMember | ParameterDeclaration*/>]>,
    ) -> io::Result<Id<Type>> {
        let readonly = readonly.unwrap_or(false);
        let tuple_target = self.get_tuple_target_type(
            &element_flags
                .map(ToOwned::to_owned)
                .unwrap_or_else(|| map(element_types, |_, _| ElementFlags::Required)),
            readonly,
            named_member_declarations,
        )?;
        Ok(if tuple_target == self.empty_generic_type() {
            self.empty_object_type()
        } else if !element_types.is_empty() {
            self.create_normalized_type_reference(tuple_target, Some(element_types.to_owned()))?
        } else {
            tuple_target
        })
    }

    pub(super) fn get_tuple_target_type(
        &self,
        element_flags: &[ElementFlags],
        readonly: bool,
        named_member_declarations: Option<&[Id<Node /*NamedTupleMember | ParameterDeclaration*/>]>,
    ) -> io::Result<Id<Type /*GenericType*/>> {
        if element_flags.len() == 1 && element_flags[0].intersects(ElementFlags::Rest) {
            return Ok(if readonly {
                self.global_readonly_array_type()
            } else {
                self.global_array_type()
            });
        }
        let key = format!(
            "{}{}{}",
            map(element_flags, |f: &ElementFlags, _| {
                if f.intersects(ElementFlags::Required) {
                    "#"
                } else if f.intersects(ElementFlags::Optional) {
                    "?"
                } else if f.intersects(ElementFlags::Rest) {
                    "."
                } else {
                    "*"
                }
            })
            .join(""),
            if readonly { "R" } else { "" },
            if let Some(named_member_declarations) = named_member_declarations
                .filter(|named_member_declarations| !named_member_declarations.is_empty())
            {
                format!(
                    ",{}",
                    map(
                        named_member_declarations,
                        |named_member_declaration: &Id<Node>, _| get_node_id(
                            &named_member_declaration.ref_(self)
                        )
                        .to_string()
                    )
                    .join(",")
                )
            } else {
                "".to_owned()
            }
        );
        let mut type_ = self.tuple_types().get(&key).map(Clone::clone);
        if type_.is_none() {
            type_ = Some(self.create_tuple_target_type(
                element_flags,
                readonly,
                named_member_declarations,
            )?);
            self.tuple_types().insert(key, type_.clone().unwrap());
        }
        Ok(type_.unwrap())
    }

    pub(super) fn create_tuple_target_type(
        &self,
        element_flags: &[ElementFlags],
        readonly: bool,
        named_member_declarations: Option<&[Id<Node /*NamedTupleMember | ParameterDeclaration*/>]>,
    ) -> io::Result<Id<Type /*TupleType*/>> {
        let arity = element_flags.len();
        let min_length = count_where(Some(element_flags), |f: &ElementFlags, _| {
            f.intersects(ElementFlags::Required | ElementFlags::Variadic)
        });
        let mut type_parameters: Option<Vec<Id<Type /*TypeParameter*/>>> = None;
        let mut properties: Vec<Id<Symbol>> = vec![];
        let mut combined_flags = ElementFlags::None;
        if arity > 0 {
            type_parameters = Some(vec![]);
            let type_parameters = type_parameters.as_mut().unwrap();
            for i in 0..arity {
                type_parameters.push(
                    self.alloc_type(
                        self.create_type_parameter(Option::<Id<Symbol>>::None)
                            .into(),
                    ),
                );
                let type_parameter = type_parameters[i];
                let flags = element_flags[i];
                combined_flags |= flags;
                if !combined_flags.intersects(ElementFlags::Variable) {
                    let property = self.alloc_symbol(
                        self.create_symbol(
                            SymbolFlags::Property
                                | if flags.intersects(ElementFlags::Optional) {
                                    SymbolFlags::Optional
                                } else {
                                    SymbolFlags::None
                                },
                            i.to_string(),
                            Some(if readonly {
                                CheckFlags::Readonly
                            } else {
                                CheckFlags::None
                            }),
                        )
                        .into(),
                    );
                    let property_links = property.ref_(self).as_transient_symbol().symbol_links();
                    let mut property_links = property_links.ref_mut(self);
                    property_links.tuple_label_declaration =
                        named_member_declarations.and_then(|named_member_declarations| {
                            named_member_declarations.get(i).map(Clone::clone)
                        });
                    property_links.type_ = Some(type_parameter.clone());
                    properties.push(property);
                }
            }
        }
        let fixed_length = properties.len();
        let length_symbol = self.alloc_symbol(
            self.create_symbol(SymbolFlags::Property, "length".to_owned(), None)
                .into(),
        );
        if combined_flags.intersects(ElementFlags::Variable) {
            length_symbol
                .ref_(self)
                .as_transient_symbol()
                .symbol_links()
                .ref_mut(self)
                .type_ = Some(self.number_type());
        } else {
            let mut literal_types = vec![];
            for i in min_length..=arity {
                literal_types.push(self.get_number_literal_type(Number::new(i as f64)));
            }
            length_symbol
                .ref_(self)
                .as_transient_symbol()
                .symbol_links()
                .ref_mut(self)
                .type_ = Some(self.get_union_type(
                &literal_types,
                None,
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?);
        }
        properties.push(length_symbol);
        let type_ = self.create_object_type(
            ObjectFlags::Tuple | ObjectFlags::Reference,
            Option::<Id<Symbol>>::None,
        );
        let mut this_type = self.create_type_parameter(Option::<Id<Symbol>>::None);
        this_type.is_this_type = Some(true);
        let this_type = self.alloc_type(this_type.into());
        let type_ = BaseInterfaceType::new(
            type_,
            type_parameters.clone(),
            None,
            type_parameters.clone(),
            Some(this_type.clone()),
        );
        let type_ = self.alloc_type(
            TupleType::new(
                type_,
                element_flags.to_owned(),
                min_length,
                fixed_length,
                combined_flags.intersects(ElementFlags::Variable),
                combined_flags,
                readonly,
                named_member_declarations.map(ToOwned::to_owned),
            )
            .into(),
        );
        this_type
            .ref_(self)
            .as_type_parameter()
            .set_constraint(Some(type_.clone()));
        let mut instantiations = HashMap::new();
        instantiations.insert(
            self.get_type_list_id(type_parameters.as_deref()),
            type_.clone(),
        );
        type_
            .ref_(self)
            .as_interface_type()
            .set_target(type_.clone());
        *type_
            .ref_(self)
            .as_interface_type()
            .maybe_resolved_type_arguments_mut() = type_parameters;
        type_
            .ref_(self)
            .as_interface_type()
            .set_declared_properties(properties);
        type_
            .ref_(self)
            .as_interface_type()
            .set_declared_call_signatures(vec![]);
        type_
            .ref_(self)
            .as_interface_type()
            .set_declared_construct_signatures(vec![]);
        type_
            .ref_(self)
            .as_interface_type()
            .set_declared_index_infos(vec![]);
        type_
            .ref_(self)
            .as_interface_type()
            .genericize(instantiations);
        Ok(type_)
    }
}
