use std::{borrow::Borrow, convert::TryInto, io, ptr};

use gc::{Gc, GcCell};

use super::{IterationUse, TypeFacts};
use crate::{
    add_related_info, concatenate, copy_entries, create_diagnostic_for_node, create_symbol_table,
    every, factory, filter, get_assigned_expando_initializer, get_assignment_declaration_kind,
    get_assignment_declaration_property_access_kind, get_combined_modifier_flags,
    get_combined_node_flags, get_declaration_of_kind, get_declared_expando_initializer,
    get_effective_modifier_flags, get_effective_type_annotation_node, get_jsdoc_type,
    get_jsdoc_type_tag, get_object_flags, get_source_file_of_node, get_this_container,
    has_only_expression_initializer, has_static_modifier, index_of_gc, is_access_expression,
    is_binary_expression, is_binding_pattern, is_call_expression,
    is_class_static_block_declaration, is_function_type_node, is_in_js_file, is_jsx_attribute,
    is_module_exports_access_expression, is_named_declaration, is_object_literal_expression,
    is_parameter, is_parameter_declaration, is_property_access_expression, is_property_declaration,
    is_property_signature, is_string_or_numeric_literal_like, is_variable_declaration, length,
    return_ok_default_if_none, return_ok_none_if_none, set_parent, skip_parentheses, some,
    starts_with, try_cast, try_maybe_every, unescape_leading_underscores,
    walk_up_binding_elements_and_patterns, AccessFlags, AssignmentDeclarationKind, Debug_,
    Diagnostics, HasInitializerInterface, HasTypeInterface, InternalSymbolName, LiteralType,
    ModifierFlags, NamedDeclarationInterface, Node, NodeFlags, NodeInterface, Number, ObjectFlags,
    ObjectFlagsTypeInterface, OptionTry, StrOrRcNode, Symbol, SymbolFlags, SymbolInterface,
    SyntaxKind, TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface,
    UnionReduction,
};

impl TypeChecker {
    pub(super) fn get_parent_element_access(
        &self,
        node: &Node, /*BindingElement | PropertyAssignment | ShorthandPropertyAssignment | Expression*/
    ) -> io::Result<Option<Gc<Node>>> {
        let ancestor = node.parent().parent();
        Ok(match ancestor.kind() {
            SyntaxKind::BindingElement | SyntaxKind::PropertyAssignment => {
                self.get_synthetic_element_access(&ancestor)?
            }
            SyntaxKind::ArrayLiteralExpression => {
                self.get_synthetic_element_access(&node.parent())?
            }
            SyntaxKind::VariableDeclaration => {
                ancestor.as_variable_declaration().maybe_initializer()
            }
            SyntaxKind::BinaryExpression => Some(ancestor.as_binary_expression().right.clone()),
            _ => None,
        })
    }

    pub(super) fn get_destructuring_property_name(
        &self,
        node: &Node, /*BindingElement | PropertyAssignment | ShorthandPropertyAssignment | Expression*/
    ) -> io::Result<Option<String>> {
        let parent = node.parent();
        if node.kind() == SyntaxKind::BindingElement
            && parent.kind() == SyntaxKind::ObjectBindingPattern
        {
            let node_as_binding_element = node.as_binding_element();
            return self.get_literal_property_name_text(
                &node_as_binding_element
                    .property_name
                    .clone()
                    .unwrap_or_else(|| node_as_binding_element.name()),
            );
        }
        if matches!(
            node.kind(),
            SyntaxKind::PropertyAssignment | SyntaxKind::ShorthandPropertyAssignment
        ) {
            return self.get_literal_property_name_text(&node.as_named_declaration().name());
        }
        Ok(Some(format!(
            "{}",
            index_of_gc(&parent.as_has_elements().elements(), &node.node_wrapper())
        )))
    }

    pub(super) fn get_literal_property_name_text(
        &self,
        name: &Node, /*PropertyName*/
    ) -> io::Result<Option<String>> {
        let type_ = self.get_literal_type_from_property_name(name)?;
        Ok(
            if type_
                .flags()
                .intersects(TypeFlags::StringLiteral | TypeFlags::NumberLiteral)
            {
                Some(format!(
                    "{}",
                    match &*type_ {
                        Type::LiteralType(LiteralType::NumberLiteralType(type_)) => {
                            type_.value.to_string()
                        }
                        Type::LiteralType(LiteralType::StringLiteralType(type_)) => {
                            type_.value.clone()
                        }
                        _ => panic!("Expected NumberLiteralType or StringLiteralType"),
                    }
                ))
            } else {
                None
            },
        )
    }

    pub(super) fn get_type_for_binding_element(
        &self,
        declaration: &Node, /*BindingElement*/
    ) -> io::Result<Option<Gc<Type>>> {
        let pattern = declaration.parent();
        let mut parent_type =
            return_ok_none_if_none!(self.get_type_for_binding_element_parent(&pattern.parent())?);
        if self.is_type_any(Some(&*parent_type)) {
            return Ok(Some(parent_type));
        }
        if self.strict_null_checks
            && declaration.flags().intersects(NodeFlags::Ambient)
            && is_parameter_declaration(declaration)
        {
            parent_type = self.get_non_nullable_type(&parent_type)?;
        } else if self.strict_null_checks
            && matches!(
                pattern.parent().as_has_initializer().maybe_initializer(),
                Some(initializer) if !self.get_type_facts(
                    &*self.get_type_of_initializer(&initializer)?,
                    None
                )?.intersects(TypeFacts::EQUndefined)
            )
        {
            parent_type = self.get_type_with_facts(&parent_type, TypeFacts::NEUndefined)?;
        }
        let type_: Option<Gc<Type>>;
        let declaration_as_binding_element = declaration.as_binding_element();
        if pattern.kind() == SyntaxKind::ObjectBindingPattern {
            if declaration_as_binding_element.dot_dot_dot_token.is_some() {
                parent_type = self.get_reduced_type(&parent_type)?;
                if parent_type.flags().intersects(TypeFlags::Unknown)
                    || !self.is_valid_spread_type(&parent_type)?
                {
                    self.error(
                        Some(declaration),
                        &Diagnostics::Rest_types_may_only_be_created_from_object_types,
                        None,
                    );
                    return Ok(Some(self.error_type()));
                }
                let mut literal_members: Vec<Gc<Node /*PropertyName*/>> = vec![];
                for element in &pattern.as_object_binding_pattern().elements {
                    let element_as_binding_element = element.as_binding_element();
                    if element_as_binding_element.dot_dot_dot_token.is_none() {
                        literal_members.push(
                            element_as_binding_element
                                .property_name
                                .clone()
                                .unwrap_or_else(|| element_as_binding_element.name()),
                        );
                    }
                }
                type_ = Some(self.get_rest_type(
                    &parent_type,
                    &literal_members,
                    declaration.maybe_symbol(),
                )?);
            } else {
                let name = declaration_as_binding_element
                    .property_name
                    .clone()
                    .unwrap_or_else(|| declaration_as_binding_element.name());
                let index_type = self.get_literal_type_from_property_name(&name)?;
                let declared_type = self.get_indexed_access_type(
                    &parent_type,
                    &index_type,
                    Some(AccessFlags::ExpressionPosition),
                    Some(&*name),
                    Option::<&Symbol>::None,
                    None,
                )?;
                type_ = Some(self.get_flow_type_of_destructuring(declaration, &declared_type)?);
            }
        } else {
            let element_type = self.check_iterated_type_or_element_type(
                IterationUse::Destructuring
                    | if declaration_as_binding_element.dot_dot_dot_token.is_some() {
                        IterationUse::None
                    } else {
                        IterationUse::PossiblyOutOfBounds
                    },
                &parent_type,
                &self.undefined_type(),
                Some(&*pattern),
            )?;
            let index: usize = index_of_gc(
                &pattern.as_array_binding_pattern().elements,
                &declaration.node_wrapper(),
            )
            .try_into()
            .unwrap();
            if declaration_as_binding_element.dot_dot_dot_token.is_some() {
                type_ = if self.every_type(&parent_type, |type_| self.is_tuple_type(type_)) {
                    self.try_map_type(
                        &parent_type,
                        &mut |t| -> io::Result<_> {
                            Ok(Some(self.slice_tuple_type(t, index, None)?))
                        },
                        None,
                    )?
                } else {
                    Some(self.create_array_type(&element_type, None))
                };
            } else if self.is_array_like_type(&parent_type)? {
                let index_type = self.get_number_literal_type(Number::new(index as f64));
                let access_flags = AccessFlags::ExpressionPosition
                    | if self.has_default_value(declaration) {
                        AccessFlags::NoTupleBoundsCheck
                    } else {
                        AccessFlags::None
                    };
                let declared_type = self
                    .get_indexed_access_type_or_undefined(
                        &parent_type,
                        &index_type,
                        Some(access_flags),
                        declaration_as_binding_element.maybe_name(),
                        Option::<&Symbol>::None,
                        None,
                    )?
                    .unwrap_or_else(|| self.error_type());
                type_ = Some(self.get_flow_type_of_destructuring(declaration, &declared_type)?);
            } else {
                type_ = Some(element_type);
            }
        }
        if declaration_as_binding_element.maybe_initializer().is_none() {
            return Ok(type_);
        }
        let type_ = type_.unwrap();
        if get_effective_type_annotation_node(&walk_up_binding_elements_and_patterns(declaration))
            .is_some()
        {
            return Ok(Some(
                if self.strict_null_checks
                    && !self
                        .get_falsy_flags(
                            &*self.check_declaration_initializer(
                                declaration,
                                Option::<&Type>::None,
                            )?,
                        )
                        .intersects(TypeFlags::Undefined)
                {
                    self.get_non_undefined_type(&type_)?
                } else {
                    type_
                },
            ));
        }
        Ok(Some(self.widen_type_inferred_from_initializer(
            declaration,
            &*self.get_union_type(
                &[
                    self.get_non_undefined_type(&type_)?,
                    self.check_declaration_initializer(declaration, Option::<&Type>::None)?,
                ],
                Some(UnionReduction::Subtype),
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )?,
        )?))
    }

    pub(super) fn get_type_for_declaration_from_jsdoc_comment(
        &self,
        declaration: &Node,
    ) -> io::Result<Option<Gc<Type>>> {
        let jsdoc_type = get_jsdoc_type(declaration);
        if jsdoc_type.is_none() {
            return Ok(None);
        }
        let jsdoc_type = jsdoc_type.unwrap();
        Ok(Some(self.get_type_from_type_node_(&jsdoc_type)?))
    }

    pub(super) fn is_null_or_undefined(&self, node: &Node /*Expression*/) -> io::Result<bool> {
        let expr = skip_parentheses(node, Some(true));
        Ok(expr.kind() == SyntaxKind::NullKeyword
            || expr.kind() == SyntaxKind::Identifier
                && Gc::ptr_eq(&self.get_resolved_symbol(&expr)?, &self.undefined_symbol()))
    }

    pub(super) fn is_empty_array_literal(&self, node: &Node /*Expression*/) -> bool {
        let expr = skip_parentheses(node, Some(true));
        expr.kind() == SyntaxKind::ArrayLiteralExpression
            && expr.as_array_literal_expression().elements.is_empty()
    }

    pub(super) fn add_optionality(
        &self,
        type_: &Type,
        is_property: Option<bool>,
        is_optional: Option<bool>,
    ) -> io::Result<Gc<Type>> {
        let is_property = is_property.unwrap_or(false);
        let is_optional = is_optional.unwrap_or(true);
        Ok(if self.strict_null_checks && is_optional {
            self.get_optional_type_(type_, Some(is_property))?
        } else {
            type_.type_wrapper()
        })
    }

    pub(super) fn get_type_for_variable_like_declaration(
        &self,
        declaration: &Node, /*ParameterDeclaration | PropertyDeclaration | PropertySignature | VariableDeclaration | BindingElement | JSDocPropertyLikeTag*/
        include_optionality: bool,
    ) -> io::Result<Option<Gc<Type>>> {
        if is_variable_declaration(declaration)
            && declaration.parent().parent().kind() == SyntaxKind::ForInStatement
        {
            let index_type = self.get_index_type(
                &*self.get_non_nullable_type_if_needed(
                    &*self.check_expression(
                        &declaration
                            .parent()
                            .parent()
                            .as_for_in_statement()
                            .expression,
                        None,
                        None,
                    )?,
                )?,
                None,
                None,
            )?;
            return Ok(Some(
                if index_type
                    .flags()
                    .intersects(TypeFlags::TypeParameter | TypeFlags::Index)
                {
                    self.get_extract_string_type(&index_type)?
                } else {
                    self.string_type()
                },
            ));
        }

        if is_variable_declaration(declaration)
            && declaration.parent().parent().kind() == SyntaxKind::ForOfStatement
        {
            let for_of_statement = declaration.parent().parent();
            return Ok(Some(
                self.check_right_hand_side_of_for_of(&for_of_statement)?,
            ));
            /*|| anyType*/
        }

        if is_binding_pattern(Some(declaration.parent())) {
            return self.get_type_for_binding_element(declaration);
        }

        let is_property =
            is_property_declaration(declaration) || is_property_signature(declaration);
        let is_optional = include_optionality
            && (is_property
                && declaration
                    .as_has_question_token()
                    .maybe_question_token()
                    .is_some()
                || is_parameter(declaration)
                    && (declaration
                        .as_parameter_declaration()
                        .question_token
                        .is_some()
                        || self.is_jsdoc_optional_parameter(declaration))
                || self.is_optional_jsdoc_property_like_tag(declaration));

        let declared_type = self.try_get_type_from_effective_type_node(declaration)?;
        if let Some(declared_type) = declared_type {
            return Ok(Some(self.add_optionality(
                &declared_type,
                Some(is_property),
                Some(is_optional),
            )?));
        }

        if (self.no_implicit_any || is_in_js_file(Some(declaration)))
            && is_variable_declaration(declaration)
            && !is_binding_pattern(declaration.as_variable_declaration().maybe_name())
            && !get_combined_modifier_flags(declaration).intersects(ModifierFlags::Export)
            && !declaration.flags().intersects(NodeFlags::Ambient)
        {
            let declaration_as_variable_declaration = declaration.as_variable_declaration();
            if !get_combined_node_flags(declaration).intersects(NodeFlags::Const)
                && match declaration_as_variable_declaration.maybe_initializer() {
                    None => true,
                    Some(initializer) => self.is_null_or_undefined(&initializer)?,
                }
            {
                return Ok(Some(self.auto_type()));
            }

            if matches!(declaration_as_variable_declaration.maybe_initializer(), Some(initializer) if self.is_empty_array_literal(&initializer))
            {
                return Ok(Some(self.auto_array_type()));
            }
        }

        if is_parameter(declaration) {
            let func = declaration.parent();
            if func.kind() == SyntaxKind::SetAccessor && self.has_bindable_name(&func)? {
                let getter = get_declaration_of_kind(
                    &self.get_symbol_of_node(&declaration.parent())?.unwrap(),
                    SyntaxKind::GetAccessor,
                );
                if let Some(getter) = getter {
                    let getter_signature = self.get_signature_from_declaration_(&getter)?;
                    let this_parameter = self.get_accessor_this_parameter(&func);
                    if let Some(this_parameter) = this_parameter {
                        if ptr::eq(declaration, &*this_parameter) {
                            Debug_.assert(
                                this_parameter
                                    .as_parameter_declaration()
                                    .maybe_type()
                                    .is_none(),
                                None,
                            );
                            return Ok(Some(self.get_type_of_symbol(
                                getter_signature.maybe_this_parameter().as_ref().unwrap(),
                            )?));
                        }
                    }
                    return Ok(Some(self.get_return_type_of_signature(getter_signature)?));
                }
            }
            if is_in_js_file(Some(declaration)) {
                let type_tag = get_jsdoc_type(&func);
                if let Some(type_tag) = type_tag {
                    if is_function_type_node(&type_tag) {
                        let signature = self.get_signature_from_declaration_(&type_tag)?;
                        let pos: usize = index_of_gc(
                            &func.as_function_like_declaration().parameters(),
                            &declaration.node_wrapper(),
                        )
                        .try_into()
                        .unwrap();
                        return Ok(Some(
                            if declaration
                                .as_parameter_declaration()
                                .dot_dot_dot_token
                                .is_some()
                            {
                                self.get_rest_type_at_position(&signature, pos)?
                            } else {
                                self.get_type_at_position(&signature, pos)?
                            },
                        ));
                    }
                }
            }
            let type_ = if declaration.symbol().escaped_name() == InternalSymbolName::This {
                self.get_contextual_this_parameter_type(&func)?
            } else {
                self.get_contextually_typed_parameter_type(declaration)?
            };
            if let Some(type_) = type_ {
                return Ok(Some(self.add_optionality(
                    &type_,
                    Some(false),
                    Some(is_optional),
                )?));
            }
        }

        if has_only_expression_initializer(declaration)
            && declaration
                .as_has_initializer()
                .maybe_initializer()
                .is_some()
        {
            if is_in_js_file(Some(declaration)) && !is_parameter(declaration) {
                let container_object_type = self.get_js_container_object_type(
                    declaration,
                    self.get_symbol_of_node(declaration)?,
                    get_declared_expando_initializer(declaration),
                )?;
                if container_object_type.is_some() {
                    return Ok(container_object_type);
                }
            }
            let type_ = self.widen_type_inferred_from_initializer(
                declaration,
                &*self.check_declaration_initializer(declaration, Option::<&Type>::None)?,
            )?;
            return Ok(Some(self.add_optionality(
                &type_,
                Some(is_property),
                Some(is_optional),
            )?));
        }

        if is_property_declaration(declaration)
            && (self.no_implicit_any || is_in_js_file(Some(declaration)))
        {
            if !has_static_modifier(declaration) {
                let constructor = self.find_constructor_declaration(&declaration.parent());
                let type_ = if let Some(constructor) = constructor {
                    self.get_flow_type_in_constructor(&declaration.symbol(), &constructor)?
                } else if get_effective_modifier_flags(declaration)
                    .intersects(ModifierFlags::Ambient)
                {
                    self.get_type_of_property_in_base_class(&declaration.symbol())?
                } else {
                    None
                };
                return type_
                    .try_map(|type_| self.add_optionality(&type_, Some(true), Some(is_optional)));
            } else {
                let static_blocks = filter(
                    &declaration.parent().as_class_like_declaration().members(),
                    |member: &Gc<Node>| is_class_static_block_declaration(member),
                );
                let type_ = if !static_blocks.is_empty() {
                    self.get_flow_type_in_static_blocks(&declaration.symbol(), &static_blocks)?
                } else if get_effective_modifier_flags(declaration)
                    .intersects(ModifierFlags::Ambient)
                {
                    self.get_type_of_property_in_base_class(&declaration.symbol())?
                } else {
                    None
                };
                return type_
                    .try_map(|type_| self.add_optionality(&type_, Some(true), Some(is_optional)));
            }
        }

        if is_jsx_attribute(declaration) {
            return Ok(Some(self.true_type()));
        }

        if is_binding_pattern(declaration.as_named_declaration().maybe_name()) {
            return Ok(Some(self.get_type_from_binding_pattern(
                &declaration.as_named_declaration().name(),
                Some(false),
                Some(true),
            )?));
        }

        Ok(None)
    }

    pub(super) fn is_constructor_declared_property(&self, symbol: &Symbol) -> io::Result<bool> {
        if matches!(
            symbol.maybe_value_declaration(),
            Some(value_declaration) if is_binary_expression(&value_declaration)
        ) {
            let links = self.get_symbol_links(symbol);
            if (*links).borrow().is_constructor_declared_property.is_none() {
                links.borrow_mut().is_constructor_declared_property = Some(false);
                let is_constructor_declared_property =
                    self.get_declaring_constructor(symbol)?.is_some()
                        && try_maybe_every(
                            symbol.maybe_declarations().as_deref(),
                            |declaration: &Gc<Node>, _| -> io::Result<_> {
                                Ok(is_binary_expression(declaration)
                                    && self.is_possibly_aliased_this_property(declaration, None)?
                                    && {
                                        let declaration_as_binary_expression =
                                            declaration.as_binary_expression();
                                        declaration_as_binary_expression.left.kind()
                                            != SyntaxKind::ElementAccessExpression
                                            || is_string_or_numeric_literal_like(
                                                &declaration_as_binary_expression
                                                    .left
                                                    .as_element_access_expression()
                                                    .argument_expression,
                                            )
                                    }
                                    && self
                                        .get_annotated_type_for_assignment_declaration(
                                            Option::<&Type>::None,
                                            declaration,
                                            symbol,
                                            declaration,
                                        )?
                                        .is_none())
                            },
                        )?;
                links.borrow_mut().is_constructor_declared_property =
                    Some(is_constructor_declared_property);
            }
            return Ok((*links).borrow().is_constructor_declared_property.unwrap());
        }
        Ok(false)
    }

    pub(super) fn is_auto_typed_property(&self, symbol: &Symbol) -> bool {
        let declaration = symbol.maybe_value_declaration();
        matches!(
            declaration,
            Some(declaration) if is_property_declaration(&declaration) && get_effective_type_annotation_node(&declaration).is_none() &&
                declaration.as_property_declaration().maybe_initializer().is_none() && (self.no_implicit_any || is_in_js_file(Some(&*declaration)))
        )
    }

    pub(super) fn get_declaring_constructor(
        &self,
        symbol: &Symbol,
    ) -> io::Result<Option<Gc<Node>>> {
        let symbol_declarations = symbol.maybe_declarations();
        let symbol_declarations = return_ok_default_if_none!(symbol_declarations.as_deref());
        for declaration in symbol_declarations {
            let container = get_this_container(&declaration, false);
            if
            /*container &&*/
            container.kind() == SyntaxKind::Constructor
                || self.is_js_constructor(Some(&*container))?
            {
                return Ok(Some(container));
            }
        }
        Ok(None)
    }

    pub(super) fn get_flow_type_from_common_js_export(
        &self,
        symbol: &Symbol,
    ) -> io::Result<Gc<Type>> {
        let file = get_source_file_of_node(
            symbol
                .maybe_declarations()
                .as_ref()
                .unwrap()
                .get(0)
                .unwrap(),
        );
        let access_name = unescape_leading_underscores(symbol.escaped_name());
        let are_all_module_exports = every(
            symbol.maybe_declarations().as_deref().unwrap(),
            |d: &Gc<Node>, _| {
                is_in_js_file(Some(&**d))
                    && is_access_expression(d)
                    && is_module_exports_access_expression(d)
            },
        );
        let reference: Gc<Node> = if are_all_module_exports {
            factory.with(|factory_| {
                factory_
                    .create_property_access_expression(
                        factory_
                            .create_property_access_expression(
                                factory_.create_identifier("module"),
                                factory_.create_identifier("exports"),
                            )
                            .wrap(),
                        access_name,
                    )
                    .wrap()
            })
        } else {
            factory.with(|factory_| {
                factory_
                    .create_property_access_expression(
                        factory_.create_identifier("exports"),
                        access_name,
                    )
                    .wrap()
            })
        };
        if are_all_module_exports {
            set_parent(
                &reference
                    .as_property_access_expression()
                    .expression
                    .as_property_access_expression()
                    .expression,
                Some(&*reference.as_property_access_expression().expression),
            );
        }
        set_parent(
            &reference.as_property_access_expression().expression,
            Some(&*reference),
        );
        set_parent(&reference, Some(&*file));
        reference.set_flow_node(file.as_source_file().maybe_end_flow_node().clone());
        self.get_flow_type_of_reference(
            &reference,
            &self.auto_type(),
            Some(self.undefined_type()),
            Option::<&Node>::None,
        )
    }

    pub(super) fn get_flow_type_in_static_blocks(
        &self,
        symbol: &Symbol,
        static_blocks: &[Gc<Node /*ClassStaticBlockDeclaration*/>],
    ) -> io::Result<Option<Gc<Type>>> {
        let access_name: StrOrRcNode<'_> = if starts_with(symbol.escaped_name(), "__#") {
            factory.with(|factory_| {
                factory_
                    .create_private_identifier((&*symbol.escaped_name()).split("@").nth(1).unwrap())
                    .wrap()
                    .into()
            })
        } else {
            unescape_leading_underscores(symbol.escaped_name()).into()
        };
        for static_block in static_blocks {
            let reference = factory.with(|factory_| {
                factory_
                    .create_property_access_expression(
                        factory_.create_this().wrap(),
                        access_name.clone(),
                    )
                    .wrap()
            });
            set_parent(
                &reference.as_property_access_expression().expression,
                Some(&*reference),
            );
            set_parent(&reference, Some(&**static_block));
            reference.set_flow_node(
                static_block
                    .as_class_static_block_declaration()
                    .maybe_return_flow_node(),
            );
            let flow_type = self.get_flow_type_of_property(&reference, Some(symbol))?;
            if self.no_implicit_any
                && (Gc::ptr_eq(&flow_type, &self.auto_type())
                    || Gc::ptr_eq(&flow_type, &self.auto_array_type()))
            {
                self.error(
                    symbol.maybe_value_declaration(),
                    &Diagnostics::Member_0_implicitly_has_an_1_type,
                    Some(vec![
                        self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None)?,
                        self.type_to_string_(&flow_type, Option::<&Node>::None, None, None)?,
                    ]),
                );
            }
            if self.every_type(&flow_type, |type_| self.is_nullable_type(type_)) {
                continue;
            }
            return Ok(Some(self.convert_auto_to_any(&flow_type)));
        }
        Ok(None)
    }

    pub(super) fn get_flow_type_in_constructor(
        &self,
        symbol: &Symbol,
        constructor: &Node, /*ConstructorDeclaration*/
    ) -> io::Result<Option<Gc<Type>>> {
        let access_name: StrOrRcNode<'_> = if starts_with(symbol.escaped_name(), "__#") {
            factory.with(|factory_| {
                factory_
                    .create_private_identifier((&*symbol.escaped_name()).split("@").nth(1).unwrap())
                    .wrap()
                    .into()
            })
        } else {
            unescape_leading_underscores(symbol.escaped_name()).into()
        };
        let reference = factory.with(|factory_| {
            factory_
                .create_property_access_expression(factory_.create_this().wrap(), access_name)
                .wrap()
        });
        set_parent(
            &reference.as_property_access_expression().expression,
            Some(&*reference),
        );
        set_parent(&reference, Some(constructor));
        reference.set_flow_node(
            constructor
                .as_function_like_declaration()
                .maybe_return_flow_node(),
        );
        let flow_type = self.get_flow_type_of_property(&reference, Some(symbol))?;
        if self.no_implicit_any
            && (Gc::ptr_eq(&flow_type, &self.auto_type())
                || Gc::ptr_eq(&flow_type, &self.auto_array_type()))
        {
            self.error(
                symbol.maybe_value_declaration(),
                &Diagnostics::Member_0_implicitly_has_an_1_type,
                Some(vec![
                    self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None)?,
                    self.type_to_string_(&flow_type, Option::<&Node>::None, None, None)?,
                ]),
            );
        }
        Ok(
            if self.every_type(&flow_type, |type_| self.is_nullable_type(type_)) {
                None
            } else {
                Some(self.convert_auto_to_any(&flow_type))
            },
        )
    }

    pub(super) fn get_flow_type_of_property(
        &self,
        reference: &Node,
        prop: Option<impl Borrow<Symbol>>,
    ) -> io::Result<Gc<Type>> {
        let initial_type = prop.try_and_then(|prop| -> io::Result<_> {
            let prop = prop.borrow();
            Ok(if matches!(
                prop.maybe_value_declaration(),
                Some(value_declaration) if !self.is_auto_typed_property(prop) || get_effective_modifier_flags(&value_declaration).intersects(ModifierFlags::Ambient)
            ) {
                self.get_type_of_property_in_base_class(prop)?
            } else {
                None
            })
        })?.unwrap_or_else(|| self.undefined_type());
        self.get_flow_type_of_reference(
            reference,
            &self.auto_type(),
            Some(initial_type),
            Option::<&Node>::None,
        )
    }

    pub(super) fn get_widened_type_for_assignment_declaration(
        &self,
        symbol: &Symbol,
        resolved_symbol: Option<impl Borrow<Symbol>>,
    ) -> io::Result<Gc<Type>> {
        let container = get_assigned_expando_initializer(symbol.maybe_value_declaration());
        if let Some(container) = container {
            let tag = get_jsdoc_type_tag(&container);
            if let Some(tag) = tag
            /*&& tag.typeExpression*/
            {
                return self
                    .get_type_from_type_node_(&tag.as_jsdoc_type_like_tag().type_expression());
            }
            let container_object_type =
                symbol
                    .maybe_value_declaration()
                    .try_and_then(|value_declaration| {
                        self.get_js_container_object_type(
                            &value_declaration,
                            Some(symbol),
                            Some(&*container),
                        )
                    })?;
            return container_object_type.try_unwrap_or_else(|| {
                self.get_widened_literal_type(&*self.check_expression_cached(&container, None)?)
            });
        }
        let mut type_: Option<Gc<Type>> = None;
        let mut defined_in_constructor = false;
        let mut defined_in_method = false;
        if self.is_constructor_declared_property(symbol)? {
            type_ = self.get_flow_type_in_constructor(
                symbol,
                &self.get_declaring_constructor(symbol)?.unwrap(),
            )?;
        }
        let resolved_symbol =
            resolved_symbol.map(|resolved_symbol| resolved_symbol.borrow().symbol_wrapper());
        if type_.is_none() {
            let mut types: Option<Vec<Gc<Type>>> = None;
            if let Some(symbol_declarations) = symbol.maybe_declarations().as_deref() {
                let mut jsdoc_type: Option<Gc<Type>> = None;
                for declaration in symbol_declarations {
                    let expression =
                        if is_binary_expression(declaration) || is_call_expression(declaration) {
                            Some(declaration.clone())
                        } else if is_access_expression(declaration) {
                            Some(if is_binary_expression(&declaration.parent()) {
                                declaration.parent()
                            } else {
                                declaration.clone()
                            })
                        } else {
                            None
                        };
                    if expression.is_none() {
                        continue;
                    }
                    let expression = expression.unwrap();

                    let kind = if is_access_expression(&expression) {
                        get_assignment_declaration_property_access_kind(&expression)
                    } else {
                        get_assignment_declaration_kind(&expression)
                    };
                    if kind == AssignmentDeclarationKind::ThisProperty
                        || is_binary_expression(&expression)
                            && self.is_possibly_aliased_this_property(&expression, Some(kind))?
                    {
                        if self.is_declaration_in_constructor(&expression) {
                            defined_in_constructor = true;
                        } else {
                            defined_in_method = true;
                        }
                    }
                    if !is_call_expression(&expression) {
                        jsdoc_type = self.get_annotated_type_for_assignment_declaration(
                            jsdoc_type,
                            &expression,
                            symbol,
                            declaration,
                        )?;
                    }
                    if jsdoc_type.is_none() {
                        if types.is_none() {
                            types = Some(vec![]);
                        }
                        types.as_mut().unwrap().push(
                            if is_binary_expression(&expression) || is_call_expression(&expression)
                            {
                                self.get_initializer_type_from_assignment_declaration(
                                    symbol,
                                    resolved_symbol.as_deref(),
                                    &expression,
                                    kind,
                                )?
                            } else {
                                self.never_type()
                            },
                        );
                    }
                }
                type_ = jsdoc_type;
            }
            if type_.is_none() {
                if length(types.as_deref()) == 0 {
                    return Ok(self.error_type());
                }
                let types = types.unwrap();
                let mut constructor_types = if defined_in_constructor {
                    symbol
                        .maybe_declarations()
                        .as_ref()
                        .and_then(|declarations| {
                            self.get_constructor_defined_this_assignment_types(
                                &types,
                                &declarations,
                            )
                        })
                } else {
                    None
                };
                if defined_in_method {
                    let prop_type = self.get_type_of_property_in_base_class(symbol)?;
                    if let Some(prop_type) = prop_type {
                        if constructor_types.is_none() {
                            constructor_types = Some(vec![]);
                        }
                        constructor_types.as_mut().unwrap().push(prop_type);
                        defined_in_constructor = true;
                    }
                }
                let source_types = if some(
                    constructor_types.as_deref(),
                    Some(|t: &Gc<Type>| t.flags().intersects(!TypeFlags::Nullable)),
                ) {
                    constructor_types.unwrap()
                } else {
                    types
                };
                type_ = Some(self.get_union_type(
                    &source_types,
                    Some(UnionReduction::Subtype),
                    Option::<&Symbol>::None,
                    None,
                    Option::<&Type>::None,
                )?);
            }
        }
        let type_ = type_.unwrap();
        let widened = self.get_widened_type(&*self.add_optionality(
            &type_,
            Some(false),
            Some(defined_in_method && !defined_in_constructor),
        )?)?;
        if let Some(symbol_value_declaration) = symbol.maybe_value_declaration() {
            if Gc::ptr_eq(
                &self.filter_type(&widened, |t| t.flags().intersects(!TypeFlags::Nullable)),
                &self.never_type(),
            ) {
                self.report_implicit_any(&symbol_value_declaration, &self.any_type(), None)?;
                return Ok(self.any_type());
            }
        }
        Ok(widened)
    }

    pub(super) fn get_js_container_object_type(
        &self,
        decl: &Node,
        symbol: Option<impl Borrow<Symbol>>,
        init: Option<impl Borrow<Node>>,
    ) -> io::Result<Option<Gc<Type>>> {
        if !is_in_js_file(Some(decl)) {
            return Ok(None);
        }
        let init = return_ok_default_if_none!(init);
        let init = init.borrow();
        if !is_object_literal_expression(init)
            || !init.as_object_literal_expression().properties.is_empty()
        {
            return Ok(None);
        }
        let exports = Gc::new(GcCell::new(create_symbol_table(
            Option::<&[Gc<Symbol>]>::None,
        )));
        let mut decl = decl.node_wrapper();
        while is_binary_expression(&decl) || is_property_access_expression(&decl) {
            let s = self.get_symbol_of_node(&decl)?;
            if let Some(s) = s {
                if let Some(s_exports) = s.maybe_exports().as_deref() {
                    let s_exports = (*s_exports).borrow();
                    if !s_exports.is_empty() {
                        self.merge_symbol_table(exports.clone(), &s_exports, None)?;
                    }
                }
            }
            decl = if is_binary_expression(&decl) {
                decl.parent()
            } else {
                decl.parent().parent()
            };
        }
        let s = self.get_symbol_of_node(&decl)?;
        if let Some(s) = s {
            if let Some(s_exports) = s.maybe_exports().as_deref() {
                let s_exports = (*s_exports).borrow();
                if !s_exports.is_empty() {
                    self.merge_symbol_table(exports.clone(), &s_exports, None)?;
                }
            }
        }
        let type_ = self.create_anonymous_type(symbol, exports, vec![], vec![], vec![])?;
        let type_as_object_type = type_.as_object_type();
        type_as_object_type
            .set_object_flags(type_as_object_type.object_flags() | ObjectFlags::JSLiteral);
        Ok(Some(type_))
    }

    pub(super) fn get_annotated_type_for_assignment_declaration(
        &self,
        declared_type: Option<impl Borrow<Type>>,
        expression: &Node, /*Expression*/
        symbol: &Symbol,
        declaration: &Node, /*Declaration*/
    ) -> io::Result<Option<Gc<Type>>> {
        let type_node = get_effective_type_annotation_node(&expression.parent());
        let declared_type =
            declared_type.map(|declared_type| declared_type.borrow().type_wrapper());
        if let Some(type_node) = type_node {
            let type_ = self.get_widened_type(&*self.get_type_from_type_node_(&type_node)?)?;
            if declared_type.is_none() {
                return Ok(Some(type_));
            }
            let declared_type = declared_type.as_ref().unwrap();
            if !self.is_error_type(declared_type)
                && !self.is_error_type(&type_)
                && !self.is_type_identical_to(&declared_type, &type_)?
            {
                self.error_next_variable_or_property_declaration_must_have_same_type(
                    Option::<&Node>::None,
                    &declared_type,
                    declaration,
                    &type_,
                )?;
            }
        }
        if let Some(symbol_parent) = symbol.maybe_parent() {
            if let Some(symbol_parent_value_declaration) = symbol_parent.maybe_value_declaration() {
                let type_node =
                    get_effective_type_annotation_node(&symbol_parent_value_declaration);
                if let Some(type_node) = type_node {
                    let annotation_symbol = self.get_property_of_type_(
                        &*self.get_type_from_type_node_(&type_node)?,
                        symbol.escaped_name(),
                        None,
                    )?;
                    if let Some(annotation_symbol) = annotation_symbol {
                        return Ok(Some(
                            self.get_non_missing_type_of_symbol(&annotation_symbol)?,
                        ));
                    }
                }
            }
        }

        Ok(declared_type)
    }

    pub(super) fn get_initializer_type_from_assignment_declaration(
        &self,
        symbol: &Symbol,
        resolved_symbol: Option<impl Borrow<Symbol>>,
        expression: &Node, /*BinaryExpression | CallExpression*/
        kind: AssignmentDeclarationKind,
    ) -> io::Result<Gc<Type>> {
        let resolved_symbol =
            resolved_symbol.map(|resolved_symbol| resolved_symbol.borrow().symbol_wrapper());
        if is_call_expression(expression) {
            if let Some(resolved_symbol) = resolved_symbol.as_ref() {
                return self.get_type_of_symbol(resolved_symbol);
            }
            let object_lit_type =
                self.check_expression_cached(&expression.as_call_expression().arguments[2], None)?;
            let value_type = self.get_type_of_property_of_type_(&object_lit_type, "value")?;
            if let Some(value_type) = value_type {
                return Ok(value_type);
            }
            let get_func = self.get_type_of_property_of_type_(&object_lit_type, "get")?;
            if let Some(get_func) = get_func {
                let get_sig = self.get_single_call_signature(&get_func)?;
                if let Some(get_sig) = get_sig {
                    return self.get_return_type_of_signature(get_sig);
                }
            }
            let set_func = self.get_type_of_property_of_type_(&object_lit_type, "set")?;
            if let Some(set_func) = set_func {
                let set_sig = self.get_single_call_signature(&set_func)?;
                if let Some(set_sig) = set_sig {
                    return self.get_type_of_first_parameter_of_signature(&set_sig);
                }
            }
            return Ok(self.any_type());
        }
        let expression_as_binary_expression = expression.as_binary_expression();
        if self.contains_same_named_this_property(
            &expression_as_binary_expression.left,
            &expression_as_binary_expression.right,
        )? {
            return Ok(self.any_type());
        }
        let type_ = if let Some(resolved_symbol) = resolved_symbol.as_ref() {
            self.get_type_of_symbol(resolved_symbol)?
        } else {
            self.get_widened_literal_type(
                &*self.check_expression_cached(&expression_as_binary_expression.right, None)?,
            )?
        };
        if type_.flags().intersects(TypeFlags::Object)
            && kind == AssignmentDeclarationKind::ModuleExports
            && symbol.escaped_name() == InternalSymbolName::ExportEquals
        {
            let exported_type = self.resolve_structured_type_members(&type_)?;
            let mut members = create_symbol_table(Option::<&[Gc<Symbol>]>::None);
            let exported_type_as_resolved_type = exported_type.as_resolved_type();
            copy_entries(
                &*(*exported_type_as_resolved_type.members()).borrow(),
                &mut members,
            );
            let initial_size = members.len();
            if let Some(resolved_symbol) = resolved_symbol.as_ref() {
                let mut resolved_symbol_exports = resolved_symbol.maybe_exports_mut();
                if resolved_symbol_exports.is_none() {
                    *resolved_symbol_exports = Some(Gc::new(GcCell::new(create_symbol_table(
                        Option::<&[Gc<Symbol>]>::None,
                    ))));
                }
            }
            for (name, s) in &*(*resolved_symbol.as_deref().unwrap_or(symbol).exports()).borrow() {
                let exported_member = members.get(name).cloned();
                if let Some(exported_member) =
                    exported_member.filter(|exported_member| !Gc::ptr_eq(exported_member, s))
                {
                    if s.flags().intersects(SymbolFlags::Value)
                        && exported_member.flags().intersects(SymbolFlags::Value)
                    {
                        if let Some(s_value_declaration) = s.maybe_value_declaration() {
                            if let Some(exported_member_value_declaration) =
                                exported_member.maybe_value_declaration()
                            {
                                if !Gc::ptr_eq(
                                    &get_source_file_of_node(&s_value_declaration),
                                    &get_source_file_of_node(&exported_member_value_declaration),
                                ) {
                                    let unescaped_name =
                                        unescape_leading_underscores(s.escaped_name());
                                    let exported_member_name = try_cast(
                                        &exported_member_value_declaration,
                                        |node: &&Gc<Node>| is_named_declaration(node),
                                    )
                                    .and_then(|named_declaration: &Gc<Node>| {
                                        named_declaration.as_named_declaration().maybe_name()
                                    })
                                    .unwrap_or_else(|| exported_member_value_declaration);
                                    add_related_info(
                                        &self.error(
                                            Some(&*s_value_declaration),
                                            &Diagnostics::Duplicate_identifier_0,
                                            Some(vec![unescaped_name.to_owned()]),
                                        ),
                                        vec![create_diagnostic_for_node(
                                            &exported_member_name,
                                            &Diagnostics::_0_was_also_declared_here,
                                            Some(vec![unescaped_name.to_owned()]),
                                        )
                                        .into()],
                                    );
                                    add_related_info(
                                        &self.error(
                                            Some(&*exported_member_name),
                                            &Diagnostics::Duplicate_identifier_0,
                                            Some(vec![unescaped_name.to_owned()]),
                                        ),
                                        vec![create_diagnostic_for_node(
                                            &s_value_declaration,
                                            &Diagnostics::_0_was_also_declared_here,
                                            Some(vec![unescaped_name.to_owned()]),
                                        )
                                        .into()],
                                    );
                                }
                            }
                        }
                        let union: Gc<Symbol> = self
                            .create_symbol(s.flags() | exported_member.flags(), name.clone(), None)
                            .into();
                        union
                            .as_transient_symbol()
                            .symbol_links()
                            .borrow_mut()
                            .type_ = Some(self.get_union_type(
                            &[
                                self.get_type_of_symbol(s)?,
                                self.get_type_of_symbol(&exported_member)?,
                            ],
                            None,
                            Option::<&Symbol>::None,
                            None,
                            Option::<&Type>::None,
                        )?);
                        if let Some(exported_member_value_declaration) =
                            exported_member.maybe_value_declaration()
                        {
                            union.set_value_declaration(exported_member_value_declaration);
                        }
                        union.set_declarations(concatenate(
                            exported_member
                                .maybe_declarations()
                                .as_ref()
                                .map_or_else(|| vec![], |declarations| declarations.clone()),
                            s.maybe_declarations()
                                .as_ref()
                                .map_or_else(|| vec![], |declarations| declarations.clone()),
                        ));
                        members.insert(name.clone(), union);
                    } else {
                        members.insert(name.clone(), self.merge_symbol(s, &exported_member, None)?);
                    }
                } else {
                    members.insert(name.clone(), s.clone());
                }
            }
            let result = self.create_anonymous_type(
                if initial_size != members.len() {
                    None
                } else {
                    exported_type.maybe_symbol()
                },
                Gc::new(GcCell::new(members)),
                exported_type_as_resolved_type.call_signatures().clone(),
                exported_type_as_resolved_type
                    .construct_signatures()
                    .clone(),
                exported_type_as_resolved_type.index_infos().clone(),
            )?;
            let result_as_object_type = result.as_object_type();
            result_as_object_type.set_object_flags(
                result_as_object_type.object_flags()
                    | get_object_flags(&type_) & ObjectFlags::JSLiteral,
            );
            if let Some(result_symbol) = result.maybe_symbol() {
                if result_symbol.flags().intersects(SymbolFlags::Class)
                    && Gc::ptr_eq(
                        &type_,
                        &self.get_declared_type_of_class_or_interface(&result_symbol)?,
                    )
                {
                    result_as_object_type.set_object_flags(
                        result_as_object_type.object_flags() | ObjectFlags::IsClassInstanceClone,
                    );
                }
            }
            return Ok(result);
        }
        if self.is_empty_array_literal_type(&type_)? {
            self.report_implicit_any(expression, &self.any_array_type(), None)?;
            return Ok(self.any_array_type());
        }
        Ok(type_)
    }
}
