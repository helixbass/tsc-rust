use std::borrow::Borrow;

use gc::{Finalize, Gc, Trace};

use crate::{
    get_export_assignment_expression, get_property_assignment_alias_like_expression,
    get_source_file_of_node, id_text, is_binary_expression, is_class_expression,
    is_entity_name_expression, is_export_assignment, length, some, symbol_name,
    unescape_leading_underscores, with_synthetic_factory_and_factory, InternalSymbolName,
    ModifierFlags, Node, NodeArray, NodeArrayOrVec, NodeFlags, NodeWrappered, SignatureKind,
    StrOrRcNode, Symbol, SymbolFlags, SymbolInterface, SyntaxKind, ThenAnd, Type,
};

use super::SymbolTableToDeclarationStatements;

impl SymbolTableToDeclarationStatements {
    pub(super) fn serialize_export_specifier(
        &self,
        local_name: &str,
        target_name: &str,
        specifier: Option<impl Borrow<Node /*Expression*/>>,
    ) {
        self.add_result(
            &with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                Gc::<Node>::from(
                    factory_.create_export_declaration(
                        synthetic_factory_,
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                        false,
                        Some(
                            factory_
                                .create_named_exports(
                                    synthetic_factory_,
                                    vec![factory_
                                        .create_export_specifier(
                                            synthetic_factory_,
                                            false,
                                            (local_name != target_name).then_some(target_name),
                                            local_name,
                                        )
                                        .into()],
                                )
                                .into(),
                        ),
                        specifier.node_wrappered(),
                        None,
                    ),
                )
            }),
            ModifierFlags::None,
        );
    }

    pub(super) fn serialize_maybe_alias_assignment(&self, symbol: &Symbol) -> bool {
        if symbol.flags().intersects(SymbolFlags::Prototype) {
            return false;
        }
        let name = unescape_leading_underscores(symbol.escaped_name());
        let is_export_equals = name == InternalSymbolName::ExportEquals;
        let is_default = name == InternalSymbolName::Default;
        let is_export_assignment_compatible_symbol_name = is_export_equals || is_default;
        let alias_decl = symbol
            .maybe_declarations()
            .is_some()
            .then_and(|| self.type_checker.get_declaration_of_alias_symbol(symbol));
        let target = alias_decl.as_ref().and_then(|alias_decl| {
            self.type_checker
                .get_target_of_alias_declaration(alias_decl, Some(true))
        });
        if let Some(target) = target.as_ref().filter(|target| {
            length(target.maybe_declarations().as_deref()) > 0
                && some(
                    target.maybe_declarations().as_deref(),
                    Some(|d: &Gc<Node>| {
                        Gc::ptr_eq(
                            &get_source_file_of_node(d),
                            &get_source_file_of_node(&self.enclosing_declaration),
                        )
                    }),
                )
        }) {
            let expr = alias_decl.as_ref().map(|alias_decl| {
                if is_export_assignment(alias_decl) || is_binary_expression(alias_decl) {
                    get_export_assignment_expression(alias_decl)
                } else {
                    get_property_assignment_alias_like_expression(alias_decl)
                }
            });
            let first = expr
                .as_ref()
                .filter(|expr| is_entity_name_expression(expr))
                .map(|expr| {
                    self.type_checker
                        .get_first_non_module_exports_identifier(expr)
                });
            let referenced = first.as_ref().and_then(|first| {
                self.type_checker.resolve_entity_name(
                    first,
                    SymbolFlags::All,
                    Some(true),
                    Some(true),
                    Some(&*self.enclosing_declaration),
                )
            });
            // if (referenced || target) {
            self.include_private_symbol(referenced.as_ref().unwrap_or(target));
            // }
            self.context().tracker().disable_track_symbol();
            if is_export_assignment_compatible_symbol_name {
                self.results_mut().push(with_synthetic_factory_and_factory(
                    |synthetic_factory_, factory_| {
                        factory_
                            .create_export_assignment(
                                synthetic_factory_,
                                Option::<Gc<NodeArray>>::None,
                                Option::<Gc<NodeArray>>::None,
                                Some(is_export_equals),
                                self.node_builder.symbol_to_expression_(
                                    target,
                                    &self.context(),
                                    Some(SymbolFlags::All),
                                ),
                            )
                            .into()
                    },
                ));
            } else {
                if let Some(first) = first.as_ref().filter(|first| {
                    matches!(
                        expr.as_ref(),
                        Some(expr) if Gc::ptr_eq(*first, expr)
                    )
                }) {
                    self.serialize_export_specifier(name, id_text(first), Option::<&Node>::None);
                } else if let Some(expr) = expr.as_ref().filter(|expr| is_class_expression(expr)) {
                    self.serialize_export_specifier(
                        name,
                        &self.get_internal_symbol_name(target, &symbol_name(target)),
                        Option::<&Node>::None,
                    );
                } else {
                    let var_name = self.get_unused_name(name, Some(symbol));
                    self.add_result(
                        &with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                            Gc::<Node>::from(factory_.create_import_equals_declaration(
                                synthetic_factory_,
                                Option::<Gc<NodeArray>>::None,
                                Option::<Gc<NodeArray>>::None,
                                false,
                                Gc::<Node>::from(factory_.create_identifier(
                                    synthetic_factory_,
                                    &var_name,
                                    Option::<Gc<NodeArray>>::None,
                                    None,
                                )),
                                self.node_builder.symbol_to_name(
                                    target,
                                    &self.context(),
                                    Some(SymbolFlags::All),
                                    false,
                                ),
                            ))
                        }),
                        ModifierFlags::None,
                    );
                    self.serialize_export_specifier(name, &var_name, Option::<&Node>::None);
                }
            }
            self.context().tracker().reenable_track_symbol();
            true
        } else {
            let var_name = self.get_unused_name(name, Some(symbol));
            let ref type_to_serialize =
                self.type_checker
                    .get_widened_type(&self.type_checker.get_type_of_symbol(
                        &self.type_checker.get_merged_symbol(Some(symbol)).unwrap(),
                    ));
            if self.is_type_representable_as_function_namespace_merge(type_to_serialize, symbol) {
                self.serialize_as_function_namespace_merge(
                    type_to_serialize,
                    symbol,
                    &var_name,
                    if is_export_assignment_compatible_symbol_name {
                        ModifierFlags::None
                    } else {
                        ModifierFlags::Export
                    },
                );
            } else {
                let statement: Gc<Node> =
                    with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                        factory_
                            .create_variable_statement(
                                synthetic_factory_,
                                Option::<Gc<NodeArray>>::None,
                                Gc::<Node>::from(factory_.create_variable_declaration_list(
                                    synthetic_factory_,
                                    vec![
                                    factory_.create_variable_declaration(
                                        synthetic_factory_,
                                        Some(&*var_name),
                                        None,
                                        Some(self.node_builder.serialize_type_for_declaration(
                                            &self.context(),
                                            type_to_serialize,
                                            symbol,
                                            Some(&*self.enclosing_declaration),
                                            Some(&|symbol: &Symbol| {
                                                self.include_private_symbol(symbol);
                                            }),
                                            self.bundled,
                                        )),
                                        None,
                                    ).into()
                                ],
                                    Some(NodeFlags::Const),
                                )),
                            )
                            .into()
                    });
                self.add_result(
                    &statement,
                    if matches!(
                        target.as_ref(),
                        Some(target) if target.flags().intersects(SymbolFlags::Property) &&
                            target.escaped_name() == InternalSymbolName::ExportEquals
                    ) {
                        ModifierFlags::Ambient
                    } else if name == var_name {
                        ModifierFlags::Export
                    } else {
                        ModifierFlags::None
                    },
                );
            }
            if is_export_assignment_compatible_symbol_name {
                self.results_mut().push(with_synthetic_factory_and_factory(
                    |synthetic_factory_, factory_| {
                        factory_
                            .create_export_assignment(
                                synthetic_factory_,
                                Option::<Gc<NodeArray>>::None,
                                Option::<Gc<NodeArray>>::None,
                                Some(is_export_equals),
                                factory_
                                    .create_identifier(
                                        synthetic_factory_,
                                        &var_name,
                                        Option::<Gc<NodeArray>>::None,
                                        None,
                                    )
                                    .into(),
                            )
                            .into()
                    },
                ));
                return true;
            } else if name != var_name {
                self.serialize_export_specifier(name, &var_name, Option::<&Node>::None);
                return true;
            }
            false
        }
    }

    pub(super) fn is_type_representable_as_function_namespace_merge(
        &self,
        type_to_serialize: &Type,
        host_symbol: &Symbol,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn make_serialize_property_symbol(
        &self,
        create_property: Gc<Box<dyn MakeSerializePropertySymbolCreateProperty>>,
        method_kind: SyntaxKind,
        use_accessors: bool,
    ) -> MakeSerializePropertySymbol {
        MakeSerializePropertySymbol::new(create_property, method_kind, use_accessors)
    }

    pub(super) fn serialize_property_symbol_for_interface(
        &self,
        p: &Symbol,
        base_type: Option<impl Borrow<Type>>,
    ) -> Vec<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn serialize_signatures(
        &self,
        kind: SignatureKind,
        input: &Type,
        base_type: Option<impl Borrow<Type>>,
        output_kind: SyntaxKind,
    ) -> Vec<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn serialize_index_signatures(
        &self,
        input: &Type,
        base_type: Option<impl Borrow<Type>>,
    ) -> Vec<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn serialize_base_type(
        &self,
        t: &Type,
        static_type: &Type,
        root_name: &str,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn try_serialize_as_type_reference(
        &self,
        t: &Type,
        flags: SymbolFlags,
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn serialize_implemented_type(&self, t: &Type) -> Option<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn get_unused_name(
        &self,
        input: &str,
        symbol: Option<impl Borrow<Symbol>>,
    ) -> String {
        unimplemented!()
    }

    pub(super) fn get_internal_symbol_name(&self, symbol: &Symbol, local_name: &str) -> String {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
pub(super) struct MakeSerializePropertySymbol {
    create_property: Gc<Box<dyn MakeSerializePropertySymbolCreateProperty>>,
    method_kind: SyntaxKind,
    use_accessors: bool,
}

impl MakeSerializePropertySymbol {
    pub(super) fn new(
        create_property: Gc<Box<dyn MakeSerializePropertySymbolCreateProperty>>,
        method_kind: SyntaxKind,
        use_accessors: bool,
    ) -> Self {
        Self {
            create_property,
            method_kind,
            use_accessors,
        }
    }

    pub(super) fn call(
        &self,
        p: &Symbol,
        is_static: bool,
        base_type: Option<&Type>,
    ) -> Vec<Gc<Node>> {
        unimplemented!()
    }
}

pub(super) trait MakeSerializePropertySymbolCreateProperty: Trace + Finalize {
    fn call(
        &self,
        decorators: Option<NodeArrayOrVec /*Decorator*/>,
        modifiers: Option<NodeArrayOrVec /*Modifier*/>,
        name: StrOrRcNode<'_>, /*PropertyName*/
        question_or_exclamation_token: Option<Gc<Node /*QuestionToken*/>>,
        type_: Option<Gc<Node /*TypeNode*/>>,
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node>;
}
