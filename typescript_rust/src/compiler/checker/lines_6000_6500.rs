#![allow(non_upper_case_globals)]

use gc::{Gc, GcCell};
use regex::{Captures, Regex};
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::ptr;

use super::{wrap_symbol_tracker_to_report_for_context, NodeBuilderContext};
use crate::{
    SymbolInterface, SyntaxKind, Type, TypeFlags, TypeInterface, VisitResult, __String, every,
    find, find_ancestor, for_each_entry_bool, get_effective_return_type_node,
    get_effective_type_annotation_node, get_emit_script_target, get_first_identifier,
    get_line_and_character_of_position, get_name_of_declaration, get_object_flags,
    get_text_of_node, is_entity_name, is_entity_name_expression, is_exports_identifier,
    is_expression_with_type_arguments, is_function_like_declaration, is_get_accessor_declaration,
    is_identifier, is_identifier_start, is_identifier_text, is_in_js_file, is_in_jsdoc,
    is_indexed_access_type_node, is_jsdoc_all_type, is_jsdoc_construct_signature,
    is_jsdoc_function_type, is_jsdoc_index_signature, is_jsdoc_non_nullable_type,
    is_jsdoc_nullable_type, is_jsdoc_optional_type, is_jsdoc_type_literal, is_jsdoc_unknown_type,
    is_jsdoc_variadic_type, is_literal_import_type_node, is_module_exports_access_expression,
    is_module_identifier, is_qualified_name, is_single_or_double_quote, is_string_literal,
    is_tuple_type_node, is_type_reference_node, length, map, map_defined,
    maybe_get_source_file_of_node, maybe_map, node_is_synthesized, null_transformation_context,
    set_emit_flags, set_original_node, set_text_range, some, starts_with,
    unescape_leading_underscores, visit_each_child, visit_node, visit_nodes,
    with_synthetic_factory_and_factory, AsDoubleDeref, CharacterCodes, Debug_, EmitFlags,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface, InternalSymbolName,
    LiteralType, NamedDeclarationInterface, Node, NodeArray, NodeBuilder, NodeBuilderFlags,
    NodeInterface, Number, ObjectFlags, ReadonlyTextRange, Signature,
    SignatureDeclarationInterface, Symbol, SymbolAccessibility, SymbolFlags,
};

impl NodeBuilder {
    pub(super) fn create_access_from_symbol_chain(
        &self,
        context: &NodeBuilderContext,
        override_type_arguments: Option<&[Gc<Node /*TypeNode*/>]>,
        chain: &[Gc<Symbol>],
        index: usize,
        stopper: usize,
    ) -> Gc<Node> {
        let type_parameter_nodes = if index == chain.len() - 1 {
            override_type_arguments.map(ToOwned::to_owned)
        } else {
            self.lookup_type_parameter_nodes(chain, index, context)
        };
        let symbol = &chain[index];

        let parent = if index == 0 {
            None
        } else {
            chain.get(index - 1)
        };
        let mut symbol_name: Option<String> = None;
        if index == 0 {
            context.set_flags(context.flags() | NodeBuilderFlags::InInitialEntityName);
            symbol_name = Some(
                self.type_checker
                    .get_name_of_symbol_as_written(&symbol, Some(context))
                    .into_owned(),
            );
            context.increment_approximate_length_by(
                symbol_name
                    .as_ref()
                    .map_or(0, |symbol_name| symbol_name.len())
                    + 1,
            );
            context.set_flags(context.flags() ^ NodeBuilderFlags::InInitialEntityName);
        } else {
            if let Some(parent) = parent.as_ref()
            /*&& getExportsOfSymbol(parent)*/
            {
                let exports = self.type_checker.get_exports_of_symbol(parent);
                for_each_entry_bool(&*(*exports).borrow(), |ex: &Gc<Symbol>, name: &__String| {
                    if self
                        .type_checker
                        .get_symbol_if_same_reference(ex, symbol)
                        .is_some()
                        && !self.type_checker.is_late_bound_name(name)
                        && name != InternalSymbolName::ExportEquals
                    {
                        symbol_name = Some(unescape_leading_underscores(name).to_owned());
                        return true;
                    }
                    false
                });
            }
        }
        if symbol_name.is_none() {
            symbol_name = Some(
                self.type_checker
                    .get_name_of_symbol_as_written(&symbol, Some(context))
                    .into_owned(),
            );
        }
        let symbol_name = symbol_name.unwrap();
        context.increment_approximate_length_by(symbol_name.len() + 1);

        if !context
            .flags()
            .intersects(NodeBuilderFlags::ForbidIndexedAccessSymbolReferences)
        {
            if let Some(parent) = parent.filter(|parent| {
                matches!(
                    (*self.type_checker.get_members_of_symbol(parent))
                        .borrow()
                        .get(symbol.escaped_name()),
                    Some(got_member_of_symbol) if self.type_checker.get_symbol_if_same_reference(
                        got_member_of_symbol,
                        symbol,
                    ).is_some()
                )
            }) {
                let ref lhs = self.create_access_from_symbol_chain(
                    context,
                    override_type_arguments,
                    chain,
                    index - 1,
                    stopper,
                );
                if is_indexed_access_type_node(lhs) {
                    return with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                        factory_
                            .create_indexed_access_type_node(
                                synthetic_factory_,
                                lhs.clone(),
                                factory_
                                    .create_literal_type_node(
                                        synthetic_factory_,
                                        factory_
                                            .create_string_literal(
                                                synthetic_factory_,
                                                symbol_name,
                                                None,
                                                None,
                                            )
                                            .into(),
                                    )
                                    .into(),
                            )
                            .into()
                    });
                } else {
                    return with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                        factory_
                            .create_indexed_access_type_node(
                                synthetic_factory_,
                                factory_
                                    .create_type_reference_node(
                                        synthetic_factory_,
                                        lhs.clone(),
                                        type_parameter_nodes,
                                    )
                                    .into(),
                                factory_
                                    .create_literal_type_node(
                                        synthetic_factory_,
                                        factory_
                                            .create_string_literal(
                                                synthetic_factory_,
                                                symbol_name,
                                                None,
                                                None,
                                            )
                                            .into(),
                                    )
                                    .into(),
                            )
                            .into()
                    });
                }
            }
        }

        let identifier: Gc<Node> =
            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                set_emit_flags(
                    factory_
                        .create_identifier(
                            synthetic_factory_,
                            &symbol_name,
                            type_parameter_nodes,
                            None,
                        )
                        .into(),
                    EmitFlags::NoAsciiEscaping,
                )
            });
        identifier.set_symbol(symbol.clone());

        if index > stopper {
            let ref lhs = self.create_access_from_symbol_chain(
                context,
                override_type_arguments,
                chain,
                index - 1,
                stopper,
            );
            if !is_entity_name(lhs) {
                Debug_.fail(Some(
                    "Impossible construct - an export of an indexed access cannot be reachable",
                ));
            }
            return with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_qualified_name(synthetic_factory_, lhs.clone(), identifier)
                    .into()
            });
        }
        identifier
    }

    pub(super) fn type_parameter_shadows_name_in_scope(
        &self,
        escaped_name: &str, /*__String*/
        context: &NodeBuilderContext,
        type_: &Type, /*TypeParameter*/
    ) -> bool {
        let result = self.type_checker.resolve_name_(
            context.maybe_enclosing_declaration(),
            escaped_name,
            SymbolFlags::Type,
            None,
            Some(escaped_name),
            false,
            None,
        );
        if let Some(result) = result.as_ref() {
            if result.flags().intersects(SymbolFlags::TypeParameter)
                && matches!(
                    type_.maybe_symbol().as_ref(),
                    Some(type_symbol) if Gc::ptr_eq(
                        result,
                        type_symbol,
                    )
                )
            {
                return false;
            }
            return true;
        }
        false
    }

    pub(super) fn type_parameter_to_name(
        &self,
        type_: &Type, /*TypeParameter*/
        context: &NodeBuilderContext,
    ) -> Gc<Node> {
        if context
            .flags()
            .intersects(NodeBuilderFlags::GenerateNamesForShadowedTypeParams)
        {
            if let Some(context_type_parameter_names) =
                (*context.type_parameter_names).borrow().as_ref()
            {
                let cached =
                    context_type_parameter_names.get(&self.type_checker.get_type_id(type_));
                if let Some(cached) = cached {
                    return cached.clone();
                }
            }
        }
        let mut result =
            self.symbol_to_name(&type_.symbol(), context, Some(SymbolFlags::Type), true);
        // TODO: the Typescript version has & SyntaxKind.Identifier which is presumably a bug?
        if result.kind() != SyntaxKind::Identifier {
            return with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_identifier(
                        synthetic_factory_,
                        "(Missing type parameter)",
                        Option::<Gc<NodeArray>>::None,
                        None,
                    )
                    .into()
            });
        }
        if context
            .flags()
            .intersects(NodeBuilderFlags::GenerateNamesForShadowedTypeParams)
        {
            let rawtext = (&*result.as_identifier().escaped_text).to_owned();
            let mut i = (*context.type_parameter_names_by_text_next_name_count)
                .borrow()
                .as_ref()
                .and_then(|context_type_parameter_names_by_text_next_name_count| {
                    context_type_parameter_names_by_text_next_name_count
                        .get(&rawtext)
                        .copied()
                })
                .unwrap_or(0);
            let mut text = rawtext.clone();
            while matches!(
                (*context.type_parameter_names_by_text_next_name_count).borrow().as_ref(),
                Some(context_type_parameter_names_by_text_next_name_count) if context_type_parameter_names_by_text_next_name_count.contains_key(&text)
            ) || self.type_parameter_shadows_name_in_scope(&text, context, type_)
            {
                i += 1;
                text = format!("{}_{}", rawtext, i);
            }
            if text != rawtext {
                result = with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                    factory_
                        .create_identifier(
                            synthetic_factory_,
                            &text,
                            result.as_identifier().maybe_type_arguments().clone(),
                            None,
                        )
                        .into()
                });
            }
            context
                .type_parameter_names_by_text_next_name_count
                .borrow_mut()
                .get_or_insert_with(|| HashMap::new())
                .insert(rawtext.clone(), i);
            context
                .type_parameter_names
                .borrow_mut()
                .get_or_insert_with(|| HashMap::new())
                .insert(self.type_checker.get_type_id(type_), result.clone());
            context
                .type_parameter_names_by_text
                .borrow_mut()
                .get_or_insert_with(|| HashSet::new())
                .insert(rawtext.clone());
        }
        result
    }

    pub(super) fn symbol_to_name(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        expects_identifier: bool,
    ) -> Gc<Node /*EntityName*/> {
        let chain = self.lookup_symbol_chain(symbol, context, meaning, None);

        if expects_identifier
            && chain.len() != 1
            && !context.encountered_error.get()
            && !context
                .flags()
                .intersects(NodeBuilderFlags::AllowQualifiedNameInPlaceOfIdentifier)
        {
            context.encountered_error.set(true);
        }
        self.create_entity_name_from_symbol_chain(context, &chain, chain.len() - 1)
    }

    pub(super) fn create_entity_name_from_symbol_chain(
        &self,
        context: &NodeBuilderContext,
        chain: &[Gc<Symbol>],
        index: usize,
    ) -> Gc<Node /*EntityName*/> {
        let type_parameter_nodes = self.lookup_type_parameter_nodes(chain, index, context);
        let symbol = &chain[index];

        if index == 0 {
            context.set_flags(context.flags() | NodeBuilderFlags::InInitialEntityName);
        }
        let symbol_name = self
            .type_checker
            .get_name_of_symbol_as_written(symbol, Some(context));
        if index == 0 {
            context.set_flags(context.flags() ^ NodeBuilderFlags::InInitialEntityName);
        }

        let identifier: Gc<Node> = set_emit_flags(
            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_identifier(synthetic_factory_, &symbol_name, type_parameter_nodes, None)
                    .into()
            }),
            EmitFlags::NoAsciiEscaping,
        );
        identifier.set_symbol(symbol.clone());

        if index > 0 {
            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_qualified_name(
                        synthetic_factory_,
                        self.create_entity_name_from_symbol_chain(context, chain, index - 1),
                        identifier,
                    )
                    .into()
            })
        } else {
            identifier
        }
    }

    pub(super) fn symbol_to_expression_(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
    ) -> Gc<Node> {
        let chain = self.lookup_symbol_chain(symbol, context, meaning, None);

        self.create_expression_from_symbol_chain(context, &chain, chain.len() - 1)
    }

    pub(super) fn create_expression_from_symbol_chain(
        &self,
        context: &NodeBuilderContext,
        chain: &[Gc<Symbol>],
        index: usize,
    ) -> Gc<Node /*Expression*/> {
        let type_parameter_nodes = self.lookup_type_parameter_nodes(chain, index, context);
        let symbol = &chain[index];

        if index == 0 {
            context.set_flags(context.flags() | NodeBuilderFlags::InInitialEntityName);
        }
        let mut symbol_name = self
            .type_checker
            .get_name_of_symbol_as_written(symbol, Some(context))
            .into_owned();
        if index == 0 {
            context.set_flags(context.flags() ^ NodeBuilderFlags::InInitialEntityName);
        }
        let mut first_char = symbol_name.chars().next().unwrap();

        if is_single_or_double_quote(first_char)
            && some(
                symbol.maybe_declarations().as_deref(),
                Some(|declaration: &Gc<Node>| {
                    self.type_checker
                        .has_non_global_augmentation_external_module_symbol(declaration)
                }),
            )
        {
            return with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_string_literal(
                        synthetic_factory_,
                        self.get_specifier_for_module_symbol(symbol, context),
                        None,
                        None,
                    )
                    .into()
            });
        }
        let can_use_property_access = if first_char == CharacterCodes::hash {
            symbol_name.len() > 1
                && is_identifier_start(
                    symbol_name.chars().skip(1).next().unwrap(),
                    Some(self.type_checker.language_version),
                )
        } else {
            is_identifier_start(first_char, Some(self.type_checker.language_version))
        };

        if index == 0 || can_use_property_access {
            let identifier = with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                set_emit_flags(
                    Gc::<Node>::from(factory_.create_identifier(
                        synthetic_factory_,
                        &symbol_name,
                        type_parameter_nodes,
                        None,
                    )),
                    EmitFlags::NoAsciiEscaping,
                )
            });
            identifier.set_symbol(symbol.symbol_wrapper());

            if index > 0 {
                with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                    factory_
                        .create_property_access_expression(
                            synthetic_factory_,
                            self.create_expression_from_symbol_chain(context, chain, index - 1),
                            identifier,
                        )
                        .into()
                })
            } else {
                identifier
            }
        } else {
            if first_char == CharacterCodes::open_bracket {
                symbol_name = symbol_name[1..symbol_name.len() - 1].to_owned();
                first_char = symbol_name.chars().next().unwrap();
            }
            let mut expression: Option<Gc<Node /*Expression*/>> = None;
            if is_single_or_double_quote(first_char) {
                expression = Some(with_synthetic_factory_and_factory(
                    |synthetic_factory_, factory_| {
                        factory_
                            .create_string_literal(
                                synthetic_factory_,
                                {
                                    lazy_static! {
                                        static ref escaped_char_regex: Regex =
                                            Regex::new(r"\\(.)").unwrap();
                                    }
                                    escaped_char_regex
                                        .replace_all(
                                            &symbol_name[1..symbol_name.len() - 1],
                                            |captures: &Captures| {
                                                captures.get(1).unwrap().as_str().to_owned()
                                            },
                                        )
                                        .into_owned()
                                },
                                Some(first_char == CharacterCodes::single_quote),
                                None,
                            )
                            .into()
                    },
                ));
            } else if matches!(
                symbol_name.parse::<f64>(),
                Ok(symbol_name_parsed) if symbol_name_parsed.to_string() == symbol_name
            ) {
                expression = Some(with_synthetic_factory_and_factory(
                    |synthetic_factory_, factory_| {
                        factory_
                            .create_numeric_literal(
                                synthetic_factory_,
                                Into::<Number>::into(&*symbol_name),
                                None,
                            )
                            .into()
                    },
                ));
            }
            if expression.is_none() {
                expression = Some(with_synthetic_factory_and_factory(
                    |synthetic_factory_, factory_| {
                        set_emit_flags(
                            factory_
                                .create_identifier(
                                    synthetic_factory_,
                                    &symbol_name,
                                    type_parameter_nodes,
                                    None,
                                )
                                .into(),
                            EmitFlags::NoAsciiEscaping,
                        )
                    },
                ));
                expression.as_ref().unwrap().set_symbol(symbol.clone());
            }
            let expression = expression.unwrap();
            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_element_access_expression(
                        synthetic_factory_,
                        self.create_expression_from_symbol_chain(context, chain, index - 1),
                        expression,
                    )
                    .into()
            })
        }
    }

    pub(super) fn is_string_named(&self, d: &Node /*Declaration*/) -> bool {
        let name = get_name_of_declaration(Some(d));
        matches!(
            name.as_ref(),
            Some(name) if is_string_literal(name)
        )
    }

    pub(super) fn is_single_quoted_string_named(&self, d: &Node /*Declaration*/) -> bool {
        let name = get_name_of_declaration(Some(d));
        matches!(
            name.as_ref(),
            Some(name) if is_string_literal(name) && (
                name.as_string_literal().single_quote == Some(true) ||
                !node_is_synthesized(&**name) && starts_with(
                    &get_text_of_node(
                        name,
                        Some(false)
                    ),
                    "'"
                )
            )
        )
    }

    pub(super) fn get_property_name_node_for_symbol(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
    ) -> Gc<Node> {
        let single_quote = length(symbol.maybe_declarations().as_deref()) > 0
            && every(
                symbol.maybe_declarations().as_deref().unwrap(),
                |declaration: &Gc<Node>, _| self.is_single_quoted_string_named(declaration),
            );
        let from_name_type = self.get_property_name_node_for_symbol_from_name_type(
            symbol,
            context,
            Some(single_quote),
        );
        if let Some(from_name_type) = from_name_type {
            return from_name_type;
        }
        let raw_name = unescape_leading_underscores(symbol.escaped_name());
        let string_named = length(symbol.maybe_declarations().as_deref()) > 0
            && every(
                symbol.maybe_declarations().as_deref().unwrap(),
                |declaration: &Gc<Node>, _| self.is_string_named(declaration),
            );
        self.create_property_name_node_for_identifier_or_literal(
            raw_name.to_owned(),
            Some(string_named),
            Some(single_quote),
        )
    }

    pub(super) fn get_property_name_node_for_symbol_from_name_type(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        single_quote: Option<bool>,
    ) -> Option<Gc<Node>> {
        let name_type = (*self.type_checker.get_symbol_links(symbol))
            .borrow()
            .name_type
            .clone()?;
        if name_type
            .flags()
            .intersects(TypeFlags::StringOrNumberLiteral)
        {
            let name = match name_type.as_ref() {
                Type::LiteralType(LiteralType::StringLiteralType(name_type)) => {
                    name_type.value.clone()
                }
                Type::LiteralType(LiteralType::NumberLiteralType(name_type)) => {
                    name_type.value.to_string()
                }
                _ => panic!("Expected string or number literal type"),
            };
            if !is_identifier_text(
                &name,
                Some(get_emit_script_target(&self.type_checker.compiler_options)),
                None,
            ) && !self.type_checker.is_numeric_literal_name(&name)
            {
                return Some(with_synthetic_factory_and_factory(
                    |synthetic_factory_, factory_| {
                        factory_
                            .create_string_literal(
                                synthetic_factory_,
                                name,
                                Some(single_quote == Some(true)),
                                None,
                            )
                            .into()
                    },
                ));
            }
            if self.type_checker.is_numeric_literal_name(&name) && starts_with(&name, "-") {
                return Some(with_synthetic_factory_and_factory(
                    |synthetic_factory_, factory_| {
                        factory_
                            .create_computed_property_name(
                                synthetic_factory_,
                                factory_
                                    .create_numeric_literal(
                                        synthetic_factory_,
                                        Into::<Number>::into(&*name),
                                        None,
                                    )
                                    .into(),
                            )
                            .into()
                    },
                ));
            }
            return Some(
                self.create_property_name_node_for_identifier_or_literal(name, None, None),
            );
        }
        if name_type.flags().intersects(TypeFlags::UniqueESSymbol) {
            return Some(with_synthetic_factory_and_factory(
                |synthetic_factory_, factory_| {
                    factory_
                        .create_computed_property_name(
                            synthetic_factory_,
                            self.symbol_to_expression_(
                                &name_type.symbol(),
                                context,
                                Some(SymbolFlags::Value),
                            ),
                        )
                        .into()
                },
            ));
        }
        None
    }

    pub(super) fn create_property_name_node_for_identifier_or_literal(
        &self,
        name: String,
        string_named: Option<bool>,
        single_quote: Option<bool>,
    ) -> Gc<Node> {
        if is_identifier_text(
            &name,
            Some(get_emit_script_target(&self.type_checker.compiler_options)),
            None,
        ) {
            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_identifier(
                        synthetic_factory_,
                        &name,
                        Option::<Gc<NodeArray>>::None,
                        None,
                    )
                    .into()
            })
        } else if string_named != Some(true)
            && self.type_checker.is_numeric_literal_name(&name)
            && name.parse::<f64>().unwrap() >= 0.0
        {
            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_numeric_literal(synthetic_factory_, Into::<Number>::into(&*name), None)
                    .into()
            })
        } else {
            with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                factory_
                    .create_string_literal(
                        synthetic_factory_,
                        name,
                        Some(single_quote == Some(true)),
                        None,
                    )
                    .into()
            })
        }
    }

    pub(super) fn clone_node_builder_context(
        &self,
        context: Gc<NodeBuilderContext>,
    ) -> Gc<NodeBuilderContext> {
        let mut initial = Gc::new((*context).clone());
        {
            let mut initial_type_parameter_names = initial.type_parameter_names.borrow_mut();
            if initial_type_parameter_names.is_some() {
                *initial_type_parameter_names =
                    Some(initial_type_parameter_names.as_ref().unwrap().clone());
            }
        }
        {
            let mut initial_type_parameter_names_by_text =
                initial.type_parameter_names_by_text.borrow_mut();
            if initial_type_parameter_names_by_text.is_some() {
                *initial_type_parameter_names_by_text = Some(
                    initial_type_parameter_names_by_text
                        .as_ref()
                        .unwrap()
                        .clone(),
                );
            }
        }
        {
            let mut initial_type_parameter_symbol_list =
                initial.type_parameter_symbol_list.borrow_mut();
            if initial_type_parameter_symbol_list.is_some() {
                *initial_type_parameter_symbol_list =
                    Some(initial_type_parameter_symbol_list.as_ref().unwrap().clone());
            }
        }
        let initial_tracker =
            wrap_symbol_tracker_to_report_for_context(context.clone(), initial.tracker());
        initial.set_tracker(Gc::new(Box::new(initial_tracker)));
        initial
    }

    pub(super) fn get_declaration_with_type_annotation<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<TEnclosingDeclaration>,
    ) -> Option<Gc<Node>> {
        let enclosing_declaration = enclosing_declaration
            .map(|enclosing_declaration| enclosing_declaration.borrow().node_wrapper());
        symbol
            .maybe_declarations()
            .as_deref()
            .and_then(|symbol_declarations| {
                find(symbol_declarations, |s: &Gc<Node>, _| {
                    get_effective_type_annotation_node(s).is_some()
                        && (match enclosing_declaration.as_ref() {
                            None => true,
                            Some(enclosing_declaration) => {
                                find_ancestor(Some(&**s), |n| ptr::eq(n, &**enclosing_declaration))
                                    .is_some()
                            }
                        })
                })
                .cloned()
            })
    }

    pub(super) fn existing_type_node_is_not_reference_or_is_reference_with_compatible_type_argument_count(
        &self,
        existing: &Node, /*TypeNode*/
        type_: &Type,
    ) -> bool {
        !get_object_flags(type_).intersects(ObjectFlags::Reference)
            || !is_type_reference_node(existing)
            || length(
                existing
                    .as_type_reference_node()
                    .maybe_type_arguments()
                    .as_double_deref(),
            ) >= self.type_checker.get_min_type_argument_count(
                type_
                    .as_type_reference_interface()
                    .target()
                    .as_interface_type_interface()
                    .maybe_type_parameters(),
            )
    }

    pub(super) fn serialize_type_for_declaration(
        &self,
        context: &NodeBuilderContext,
        type_: &Type,
        symbol: &Symbol,
        enclosing_declaration: Option<impl Borrow<Node>>,
        include_private_symbol: Option<&impl Fn(&Symbol)>,
        bundled: Option<bool>,
    ) -> Gc<Node> {
        if !self.type_checker.is_error_type(type_) {
            if let Some(enclosing_declaration) = enclosing_declaration {
                let enclosing_declaration: &Node = enclosing_declaration.borrow();
                let decl_with_existing_annotation =
                    self.get_declaration_with_type_annotation(symbol, Some(enclosing_declaration));
                if let Some(decl_with_existing_annotation) = decl_with_existing_annotation
                    .as_ref()
                    .filter(|decl_with_existing_annotation| {
                        !is_function_like_declaration(decl_with_existing_annotation)
                            && !is_get_accessor_declaration(decl_with_existing_annotation)
                    })
                {
                    let ref existing =
                        get_effective_type_annotation_node(decl_with_existing_annotation).unwrap();
                    if ptr::eq(
                        &*self.type_checker.get_type_from_type_node_(
                            existing,
                        ),
                        type_,
                    ) && self.existing_type_node_is_not_reference_or_is_reference_with_compatible_type_argument_count(
                        existing,
                        type_
                    ) {
                        let result = self.serialize_existing_type_node(
                            context,
                            existing,
                            include_private_symbol,
                            bundled,
                        );
                        if let Some(result) = result {
                            return result;
                        }
                    }
                }
            }
        }
        let old_flags = context.flags();
        if type_.flags().intersects(TypeFlags::UniqueESSymbol)
            && matches!(
                type_.maybe_symbol().as_ref(), Some(type_symbol) if ptr::eq(
                    &**type_symbol,
                    symbol,
                )
            )
            && match context.maybe_enclosing_declaration().as_ref() {
                None => true,
                Some(context_enclosing_declaration) => some(
                    symbol.maybe_declarations().as_deref(),
                    Some(|d: &Gc<Node>| {
                        Gc::ptr_eq(
                            maybe_get_source_file_of_node(Some(&**d)).as_ref().unwrap(),
                            maybe_get_source_file_of_node(Some(&**context_enclosing_declaration))
                                .as_ref()
                                .unwrap(),
                        )
                    }),
                ),
            }
        {
            context.set_flags(context.flags() | NodeBuilderFlags::AllowUniqueESSymbolType);
        }
        let result = self.type_to_type_node_helper(Some(type_), context);
        context.set_flags(old_flags);
        result.unwrap()
    }

    pub(super) fn serialize_return_type_for_signature<TIncludePrivateSymbol: Fn(&Symbol)>(
        &self,
        context: &NodeBuilderContext,
        type_: &Type,
        signature: &Signature,
        include_private_symbol: Option<&TIncludePrivateSymbol>,
        bundled: Option<bool>,
    ) -> Gc<Node> {
        if !self.type_checker.is_error_type(type_) {
            if let Some(context_enclosing_declaration) =
                context.maybe_enclosing_declaration().as_ref()
            {
                let annotation = signature
                    .declaration
                    .as_ref()
                    .and_then(|signature_declaration| {
                        get_effective_return_type_node(signature_declaration)
                    });
                if let Some(annotation) = annotation.as_ref() {
                    if find_ancestor(Some(&**annotation), |n| {
                        ptr::eq(n, &**context_enclosing_declaration)
                    })
                    .is_some()
                    {
                        let annotated = self.type_checker.get_type_from_type_node_(annotation);
                        let this_instantiated =
                            if annotated.flags().intersects(TypeFlags::TypeParameter)
                                && annotated.as_type_parameter().is_this_type == Some(true)
                            {
                                self.type_checker
                                    .instantiate_type(&annotated, signature.mapper.clone())
                            } else {
                                annotated
                            };
                        if ptr::eq(
                            &*this_instantiated,
                            type_
                        ) && self.existing_type_node_is_not_reference_or_is_reference_with_compatible_type_argument_count(
                            annotation,
                            type_
                        ) {
                            let result = self.serialize_existing_type_node(
                                context,
                                annotation,
                                include_private_symbol,
                                bundled
                            );
                            if let Some(result) = result {
                                return result;
                            }
                        }
                    }
                }
            }
        }
        self.type_to_type_node_helper(Some(type_), context).unwrap()
    }

    pub(super) fn track_existing_entity_name(
        &self,
        node: &Node, /*EntityNameOrEntityNameExpression*/
        context: &NodeBuilderContext,
        include_private_symbol: Option<&impl Fn(&Symbol)>,
    ) -> TrackExistingEntityNameReturn {
        let mut introduces_error = false;
        let ref leftmost = get_first_identifier(node);
        let ref leftmost_parent = leftmost.parent();
        if is_in_js_file(Some(node))
            && (is_exports_identifier(leftmost)
                || is_module_exports_access_expression(leftmost_parent)
                || is_qualified_name(leftmost_parent) && {
                    let leftmost_parent_as_qualified_name = leftmost_parent.as_qualified_name();
                    is_module_identifier(&leftmost_parent_as_qualified_name.left)
                        && is_exports_identifier(&leftmost_parent_as_qualified_name.right)
                })
        {
            introduces_error = true;
            return TrackExistingEntityNameReturn {
                introduces_error,
                node: node.node_wrapper(),
            };
        }
        let sym = self.type_checker.resolve_entity_name(
            leftmost,
            SymbolFlags::All,
            Some(true),
            Some(true),
            Option::<&Node>::None,
        );
        if let Some(sym) = sym.as_ref() {
            if self
                .type_checker
                .is_symbol_accessible(
                    Some(&**sym),
                    context.maybe_enclosing_declaration(),
                    SymbolFlags::All,
                    false,
                )
                .accessibility
                != SymbolAccessibility::Accessible
            {
                introduces_error = true;
            } else {
                context.tracker().track_symbol(
                    sym,
                    context.maybe_enclosing_declaration(),
                    SymbolFlags::All,
                );
                if let Some(include_private_symbol) = include_private_symbol {
                    include_private_symbol(sym);
                }
            }
            if is_identifier(node) {
                let name = if sym.flags().intersects(SymbolFlags::TypeParameter) {
                    self.type_parameter_to_name(
                        &self.type_checker.get_declared_type_of_symbol(sym),
                        context,
                    )
                } else {
                    with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                        factory.clone_node(synthetic_factory, node)
                    })
                };
                name.set_symbol(sym.symbol_wrapper());
                return TrackExistingEntityNameReturn {
                    introduces_error,
                    node: set_emit_flags(
                        set_original_node(name, Some(node.node_wrapper())),
                        EmitFlags::NoAsciiEscaping,
                    ),
                };
            }
        }

        TrackExistingEntityNameReturn {
            introduces_error,
            node: node.node_wrapper(),
        }
    }

    pub(super) fn serialize_existing_type_node(
        &self,
        context: &NodeBuilderContext,
        existing: &Node, /*TypeNode*/
        include_private_symbol: Option<&impl Fn(&Symbol)>,
        bundled: Option<bool>,
    ) -> Option<Gc<Node>> {
        if let Some(cancellation_token) = self.type_checker.maybe_cancellation_token() {
            cancellation_token.throw_if_cancellation_requested();
        }
        let mut had_error = false;
        let file = maybe_get_source_file_of_node(Some(existing));
        let transformed = visit_node(
            Some(existing),
            Some(|node: &Node| {
                self.visit_existing_node_tree_symbols(
                    context,
                    &mut had_error,
                    include_private_symbol,
                    file.as_deref(),
                    node,
                )
            }),
            Option::<fn(&Node) -> bool>::None,
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )
        .unwrap();
        if had_error {
            return None;
        }
        if ptr::eq(&*transformed, existing) {
            let ret = with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                factory.clone_node(synthetic_factory, existing)
            });
            set_text_range(&*ret, Some(existing));
            Some(ret)
        } else {
            Some(transformed)
        }
    }

    pub(super) fn visit_existing_node_tree_symbols<TIncludePrivateSymbol: Fn(&Symbol)>(
        &self,
        context: &NodeBuilderContext,
        had_error: &mut bool,
        include_private_symbol: Option<&TIncludePrivateSymbol>,
        file: Option<&Node>,
        node: &Node,
    ) -> VisitResult {
        if is_jsdoc_all_type(node) || node.kind() == SyntaxKind::JSDocNamepathType {
            return with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                Some(
                    Into::<Gc<Node>>::into(
                        factory.create_keyword_type_node(synthetic_factory, SyntaxKind::AnyKeyword),
                    )
                    .into(),
                )
            });
        }
        if is_jsdoc_unknown_type(node) {
            return with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                Some(
                    Into::<Gc<Node>>::into(
                        factory.create_keyword_type_node(
                            synthetic_factory,
                            SyntaxKind::UnknownKeyword,
                        ),
                    )
                    .into(),
                )
            });
        }
        if is_jsdoc_nullable_type(node) {
            return with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                Some(
                    Into::<Gc<Node>>::into(factory.create_union_type_node(
                        synthetic_factory,
                        vec![
                            visit_node(
                                node.as_base_jsdoc_unary_type().type_.as_deref(),
                                Some(|node: &Node| {
                                    self.visit_existing_node_tree_symbols(
                                        context,
                                        had_error,
                                        include_private_symbol,
                                        file,
                                        node,
                                    )
                                }),
                                Option::<fn(&Node) -> bool>::None,
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            )
                            .unwrap(),
                            factory
                                .create_literal_type_node(
                                    synthetic_factory,
                                    factory.create_null(synthetic_factory).into(),
                                )
                                .into(),
                        ],
                    ))
                    .into(),
                )
            });
        }
        if is_jsdoc_optional_type(node) {
            return with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                Some(
                    Into::<Gc<Node>>::into(factory.create_union_type_node(
                        synthetic_factory,
                        vec![
                            visit_node(
                                node.as_base_jsdoc_unary_type().type_.as_deref(),
                                Some(|node: &Node| {
                                    self.visit_existing_node_tree_symbols(
                                        context,
                                        had_error,
                                        include_private_symbol,
                                        file,
                                        node,
                                    )
                                }),
                                Option::<fn(&Node) -> bool>::None,
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            )
                            .unwrap(),
                            factory
                                .create_keyword_type_node(
                                    synthetic_factory,
                                    SyntaxKind::UndefinedKeyword,
                                )
                                .into(),
                        ],
                    ))
                    .into(),
                )
            });
        }
        if is_jsdoc_non_nullable_type(node) {
            return Some(
                visit_node(
                    node.as_base_jsdoc_unary_type().type_.as_deref(),
                    Some(|node: &Node| {
                        self.visit_existing_node_tree_symbols(
                            context,
                            had_error,
                            include_private_symbol,
                            file,
                            node,
                        )
                    }),
                    Option::<fn(&Node) -> bool>::None,
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )
                .unwrap()
                .into(),
            );
        }
        if is_jsdoc_variadic_type(node) {
            return with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                Some(
                    Into::<Gc<Node>>::into(
                        factory.create_array_type_node(
                            synthetic_factory,
                            visit_node(
                                node.as_base_jsdoc_unary_type().type_.as_deref(),
                                Some(|node: &Node| {
                                    self.visit_existing_node_tree_symbols(
                                        context,
                                        had_error,
                                        include_private_symbol,
                                        file,
                                        node,
                                    )
                                }),
                                Option::<fn(&Node) -> bool>::None,
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            )
                            .unwrap(),
                        ),
                    )
                    .into(),
                )
            });
        }
        if is_jsdoc_type_literal(node) {
            return with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                Some(
                    Into::<Gc<Node>>::into(
                        factory.create_type_literal_node(
                            synthetic_factory,
                            maybe_map(
                                node.as_jsdoc_type_literal().js_doc_property_tags.as_deref(),
                                |t: &Gc<Node>, _| -> Gc<Node> {
                                    let t_as_jsdoc_property_like_tag = t.as_jsdoc_property_like_tag();
                                    let name = if is_identifier(
                                        &t_as_jsdoc_property_like_tag.name
                                    ) {
                                        t_as_jsdoc_property_like_tag.name.clone()
                                    } else {
                                        t_as_jsdoc_property_like_tag.name.as_qualified_name().right.clone()
                                    };
                                    let type_via_parent = self.type_checker.get_type_of_property_of_type_(
                                        &self.type_checker.get_type_from_type_node_(
                                            node
                                        ),
                                        &name.as_identifier().escaped_text
                                    );
                                    let override_type_node = type_via_parent.as_ref().filter(|type_via_parent| {
                                        matches!(
                                            t_as_jsdoc_property_like_tag.type_expression.as_ref(),
                                            Some(t_type_expression) if !Gc::ptr_eq(
                                                &self.type_checker.get_type_from_type_node_(&t_type_expression.as_jsdoc_type_expression().type_),
                                                *type_via_parent
                                            )
                                        )
                                    }).and_then(|type_via_parent| {
                                        self.type_to_type_node_helper(
                                            Some(&**type_via_parent),
                                            context,
                                        )
                                    });

                                    factory.create_property_signature(
                                        synthetic_factory,
                                        Option::<Gc<NodeArray>>::None,
                                        name,
                                        if t_as_jsdoc_property_like_tag.is_bracketed ||
                                            matches!(
                                                t_as_jsdoc_property_like_tag.type_expression.as_ref(),
                                                Some(t_type_expression) if is_jsdoc_optional_type(t_type_expression)
                                            ) {
                                            Some(factory.create_token(
                                                synthetic_factory,
                                                SyntaxKind::QuestionToken,
                                            ).into())
                                        } else {
                                            None
                                        },
                                        Some(override_type_node.or_else(|| {
                                            t_as_jsdoc_property_like_tag.type_expression.as_ref().and_then(|t_type_expression| {
                                                visit_node(
                                                    Some(&*t_type_expression.as_jsdoc_type_expression().type_),
                                                    Some(|node: &Node| self.visit_existing_node_tree_symbols(context, had_error, include_private_symbol, file, node)),
                                                    Option::<fn(&Node) -> bool>::None,
                                                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                                )
                                            })
                                        }).unwrap_or_else(|| {
                                            factory.create_keyword_type_node(
                                                synthetic_factory,
                                                SyntaxKind::AnyKeyword
                                            ).into()
                                        }))
                                    ).into()
                                }
                            )
                        )
                    ).into()
                )
            });
        }
        if is_type_reference_node(node)
            && is_identifier(&node.as_type_reference_node().type_name)
            && node
                .as_type_reference_node()
                .type_name
                .as_identifier()
                .escaped_text
                .is_empty()
        {
            return Some(
                set_original_node(
                    with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                        factory
                            .create_keyword_type_node(synthetic_factory, SyntaxKind::AnyKeyword)
                            .into()
                    }),
                    Some(node.node_wrapper()),
                )
                .into(),
            );
        }
        if (is_expression_with_type_arguments(node) || is_type_reference_node(node))
            && is_jsdoc_index_signature(node)
        {
            return Some(
                with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                    Into::<Gc<Node>>::into(factory.create_type_literal_node(
                        synthetic_factory,
                        Some(vec![factory
                                .create_index_signature(
                                    synthetic_factory,
                                    Option::<Gc<NodeArray>>::None,
                                    Option::<Gc<NodeArray>>::None,
                                    vec![factory
                                        .create_parameter_declaration(
                                            synthetic_factory,
                                            Option::<Gc<NodeArray>>::None,
                                            Option::<Gc<NodeArray>>::None,
                                            None,
                                            Some("x"),
                                            None,
                                            visit_node(
                                                node.as_has_type_arguments()
                                                    .maybe_type_arguments()
                                                    .as_ref()
                                                    .unwrap()
                                                    .get(0)
                                                    .cloned(),
                                                Some(|node: &Node| {
                                                    self.visit_existing_node_tree_symbols(
                                                        context,
                                                        had_error,
                                                        include_private_symbol,
                                                        file,
                                                        node,
                                                    )
                                                }),
                                                Option::<fn(&Node) -> bool>::None,
                                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                            ),
                                            None,
                                        )
                                        .into()],
                                    visit_node(
                                        node.as_has_type_arguments()
                                            .maybe_type_arguments()
                                            .as_ref()
                                            .unwrap()
                                            .get(1)
                                            .cloned(),
                                        Some(|node: &Node| {
                                            self.visit_existing_node_tree_symbols(
                                                context,
                                                had_error,
                                                include_private_symbol,
                                                file,
                                                node,
                                            )
                                        }),
                                        Option::<fn(&Node) -> bool>::None,
                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                    ),
                                )
                                .into()]),
                    ))
                })
                .into(),
            );
        }
        if is_jsdoc_function_type(node) {
            let node_as_jsdoc_function_type = node.as_jsdoc_function_type();
            if is_jsdoc_construct_signature(node) {
                let mut new_type_node: Option<Gc<Node>> = None;
                return Some(with_synthetic_factory_and_factory(
                    |synthetic_factory, factory| {
                        Into::<Gc<Node>>::into(factory.create_constructor_type_node(
                            synthetic_factory,
                            node_as_jsdoc_function_type.maybe_modifiers().clone(),
                            visit_nodes(
                                node_as_jsdoc_function_type.maybe_type_parameters().as_deref(),
                                Some(|node: &Node| self.visit_existing_node_tree_symbols(context, had_error, include_private_symbol, file, node)),
                                Option::<fn(&Node) -> bool>::None,
                                None, None,
                            ),
                            map_defined(
                                Some(&node_as_jsdoc_function_type.parameters()),
                                |p: &Gc<Node>, i| -> Option<Gc<Node>> {
                                    let p_as_parameter_declaration = p.as_parameter_declaration();
                                    if matches!(
                                        p_as_parameter_declaration.maybe_name().as_ref(),
                                        Some(p_name) if is_identifier(p_name) && p_name.as_identifier().escaped_text == "new",
                                    ) {
                                        new_type_node = p_as_parameter_declaration.maybe_type();
                                        None
                                    } else {
                                        Some(with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                                            factory.create_parameter_declaration(
                                                synthetic_factory,
                                                Option::<Gc<NodeArray>>::None,
                                                Option::<Gc<NodeArray>>::None,
                                                self.get_effective_dot_dot_dot_for_parameter(p),
                                                self.get_name_for_jsdoc_function_parameter(p, i),
                                                p_as_parameter_declaration.question_token.clone(),
                                                visit_node(
                                                    p_as_parameter_declaration.maybe_type(),
                                                    Some(|node: &Node| self.visit_existing_node_tree_symbols(context, had_error, include_private_symbol, file, node)),
                                                    Option::<fn(&Node) -> bool>::None,
                                                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                                ),
                                                None,
                                            ).into()
                                        }))
                                    }
                                }
                            ),
                            Some(
                                visit_node(
                                    new_type_node.clone().or_else(|| node_as_jsdoc_function_type.maybe_type()),
                                    Some(|node: &Node| self.visit_existing_node_tree_symbols(context, had_error, include_private_symbol, file, node)),
                                    Option::<fn(&Node) -> bool>::None,
                                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                ).unwrap_or_else(|| {
                                    with_synthetic_factory_and_factory(|synthetic_factory, factory| {
                                        factory.create_keyword_type_node(
                                            synthetic_factory,
                                            SyntaxKind::AnyKeyword
                                        ).into()
                                    })
                                }),
                            ),
                        )).into()
                    },
                ));
            } else {
                return Some(with_synthetic_factory_and_factory(
                    |synthetic_factory, factory| {
                        Into::<Gc<Node>>::into(
                            factory.create_function_type_node(
                                synthetic_factory,
                                visit_nodes(
                                    node_as_jsdoc_function_type
                                        .maybe_type_parameters()
                                        .as_deref(),
                                    Some(|node: &Node| {
                                        self.visit_existing_node_tree_symbols(
                                            context,
                                            had_error,
                                            include_private_symbol,
                                            file,
                                            node,
                                        )
                                    }),
                                    Option::<fn(&Node) -> bool>::None,
                                    None,
                                    None,
                                ),
                                map(
                                    &node_as_jsdoc_function_type.parameters(),
                                    |p: &Gc<Node>, i| -> Gc<Node> {
                                        let p_as_parameter_declaration =
                                            p.as_parameter_declaration();
                                        with_synthetic_factory_and_factory(
                                            |synthetic_factory, factory| {
                                                factory
                                                .create_parameter_declaration(
                                                    synthetic_factory,
                                                    Option::<Gc<NodeArray>>::None,
                                                    Option::<Gc<NodeArray>>::None,
                                                    self.get_effective_dot_dot_dot_for_parameter(p),
                                                    self.get_name_for_jsdoc_function_parameter(
                                                        p, i,
                                                    ),
                                                    p_as_parameter_declaration
                                                        .question_token
                                                        .clone(),
                                                    visit_node(
                                                        node_as_jsdoc_function_type.maybe_type(),
                                                        Some(|node: &Node| {
                                                            self.visit_existing_node_tree_symbols(
                                                                context, had_error,
                                                                include_private_symbol, file, node,
                                                            )
                                                        }),
                                                        Option::<fn(&Node) -> bool>::None,
                                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                                    ),
                                                    None,
                                                )
                                                .into()
                                            },
                                        )
                                    },
                                ),
                                Some(
                                    visit_node(
                                        node_as_jsdoc_function_type.maybe_type(),
                                        Some(|node: &Node| {
                                            self.visit_existing_node_tree_symbols(
                                                context,
                                                had_error,
                                                include_private_symbol,
                                                file,
                                                node,
                                            )
                                        }),
                                        Option::<fn(&Node) -> bool>::None,
                                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                                    )
                                    .unwrap_or_else(|| {
                                        with_synthetic_factory_and_factory(
                                            |synthetic_factory, factory| {
                                                factory
                                                    .create_keyword_type_node(
                                                        synthetic_factory,
                                                        SyntaxKind::AnyKeyword,
                                                    )
                                                    .into()
                                            },
                                        )
                                    }),
                                ),
                            ),
                        )
                        .into()
                    },
                ));
            }
        }
        if is_type_reference_node(node) && is_in_jsdoc(Some(node)) && (
            !self.existing_type_node_is_not_reference_or_is_reference_with_compatible_type_argument_count(
                node,
                &self.type_checker.get_type_from_type_node_(node)
            ) || self.type_checker.get_intended_type_from_jsdoc_type_reference(node).is_some() ||
            Gc::ptr_eq(
                &self.type_checker.unknown_symbol(),
                &self.type_checker.resolve_type_reference_name(
                    node,
                    SymbolFlags::Type,
                    Some(true)
                ),
            )
        ) {
            return Some(
                set_original_node(
                    self.type_to_type_node_helper(
                        Some(self.type_checker.get_type_from_type_node_(node)),
                        context,
                    ).unwrap(),
                    Some(node.node_wrapper()),
                ).into()
            );
        }
        if is_literal_import_type_node(node) {
            let node_symbol = (*self.type_checker.get_node_links(node))
                .borrow()
                .resolved_symbol
                .clone();
            let node_as_import_type_node = node.as_import_type_node();
            if is_in_jsdoc(Some(node))
                && matches!(
                    node_symbol.as_ref(),
                    Some(node_symbol) if !node_as_import_type_node.is_type_of() &&
                        !node_symbol.flags().intersects(SymbolFlags::Type) ||
                        !(length(node_as_import_type_node.maybe_type_arguments().as_double_deref()) >=
                            self.type_checker.get_min_type_argument_count(
                                self.type_checker.get_local_type_parameters_of_class_or_interface_or_type_alias(
                                    node_symbol
                                ).as_deref()
                            )
                        )
                )
            {
                return Some(
                    set_original_node(
                        self.type_to_type_node_helper(
                            Some(self.type_checker.get_type_from_type_node_(node)),
                            context,
                        )
                        .unwrap(),
                        Some(node.node_wrapper()),
                    )
                    .into(),
                );
            }
            return with_synthetic_factory_and_factory(
                |synthetic_factory, factory| unimplemented!(),
            );
        }

        if is_entity_name(node) || is_entity_name_expression(node) {
            let TrackExistingEntityNameReturn {
                introduces_error,
                node: result,
            } = self.track_existing_entity_name(node, context, include_private_symbol);
            *had_error = *had_error || introduces_error;
            if !ptr::eq(&*result, node) {
                return Some(result.into());
            }
        }

        if matches!(
            file,
            Some(file) if is_tuple_type_node(node) && {
                let file_as_source_file = file.as_source_file();
                get_line_and_character_of_position(
                    file_as_source_file,
                    node.pos().try_into().unwrap()
                ).line ==
                get_line_and_character_of_position(
                    file_as_source_file,
                    node.end().try_into().unwrap()
                ).line
            }
        ) {
            set_emit_flags(node.node_wrapper(), EmitFlags::SingleLine);
        }

        Some(
            visit_each_child(
                Some(node),
                |node: &Node| {
                    self.visit_existing_node_tree_symbols(
                        context,
                        had_error,
                        include_private_symbol,
                        file,
                        node,
                    )
                },
                &*null_transformation_context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
                Option::<fn(&Node) -> VisitResult>::None,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            )
            .unwrap()
            .into(),
        )
    }
}

pub(super) struct TrackExistingEntityNameReturn {
    pub introduces_error: bool,
    pub node: Gc<Node>,
}
