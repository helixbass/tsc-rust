#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::{IterationUse, MappedTypeModifiers, MembersOrExportsResolutionKind, TypeFacts};
use crate::{
    add_related_info, concatenate, copy_entries, create_diagnostic_for_node, create_symbol_table,
    escape_leading_underscores, every, factory, filter, get_assigned_expando_initializer,
    get_assignment_declaration_kind, get_assignment_declaration_property_access_kind,
    get_check_flags, get_combined_modifier_flags, get_combined_node_flags, get_declaration_of_kind,
    get_declared_expando_initializer, get_effective_modifier_flags,
    get_effective_type_annotation_node, get_effective_type_parameter_declarations, get_jsdoc_type,
    get_jsdoc_type_tag, get_object_flags, get_source_file_of_node, get_this_container,
    has_dynamic_name, has_only_expression_initializer, has_static_modifier, index_of_rc,
    is_access_expression, is_binary_expression, is_binding_pattern, is_call_expression,
    is_class_static_block_declaration, is_function_type_node, is_in_js_file, is_jsx_attribute,
    is_module_exports_access_expression, is_named_declaration, is_object_literal_expression,
    is_parameter, is_parameter_declaration, is_property_access_expression, is_property_assignment,
    is_property_declaration, is_property_signature, is_string_or_numeric_literal_like,
    is_type_alias, is_variable_declaration, length, maybe_every, range_equals_rc, set_parent,
    skip_parentheses, some, starts_with, synthetic_factory, try_cast, unescape_leading_underscores,
    walk_up_binding_elements_and_patterns, AccessFlags, AssignmentDeclarationKind,
    BaseInterfaceType, CheckFlags, Debug_, Diagnostics, HasInitializerInterface, HasTypeInterface,
    InterfaceType, InterfaceTypeInterface, InterfaceTypeWithDeclaredMembersInterface,
    InternalSymbolName, LiteralType, ModifierFlags, NamedDeclarationInterface, Node, NodeArray,
    NodeFlags, NodeInterface, Number, ObjectFlags, ObjectFlagsTypeInterface, Signature,
    SignatureFlags, StringOrRcNode, Symbol, SymbolFlags, SymbolInterface, SymbolTable, SyntaxKind,
    TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
    TypePredicate, UnderscoreEscapedMap, UnionReduction, __String, maybe_append_if_unique_rc,
};

impl TypeChecker {
    pub(super) fn get_parent_element_access(
        &self,
        node: &Node, /*BindingElement | PropertyAssignment | ShorthandPropertyAssignment | Expression*/
    ) -> Option<Rc<Node>> {
        let ancestor = node.parent().parent();
        match ancestor.kind() {
            SyntaxKind::BindingElement | SyntaxKind::PropertyAssignment => {
                self.get_synthetic_element_access(&ancestor)
            }
            SyntaxKind::ArrayLiteralExpression => self.get_synthetic_element_access(&node.parent()),
            SyntaxKind::VariableDeclaration => {
                ancestor.as_variable_declaration().maybe_initializer()
            }
            SyntaxKind::BinaryExpression => Some(ancestor.as_binary_expression().right.clone()),
            _ => None,
        }
    }

    pub(super) fn get_destructuring_property_name(
        &self,
        node: &Node, /*BindingElement | PropertyAssignment | ShorthandPropertyAssignment | Expression*/
    ) -> Option<String> {
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
        Some(format!(
            "{}",
            index_of_rc(parent.as_has_elements().elements(), &node.node_wrapper())
        ))
    }

    pub(super) fn get_literal_property_name_text(
        &self,
        name: &Node, /*PropertyName*/
    ) -> Option<String> {
        let type_ = self.get_literal_type_from_property_name(name);
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
        }
    }

    pub(super) fn get_type_for_binding_element(
        &self,
        declaration: &Node, /*BindingElement*/
    ) -> Option<Rc<Type>> {
        let pattern = declaration.parent();
        let mut parent_type = self.get_type_for_binding_element_parent(&pattern.parent())?;
        if self.is_type_any(Some(&*parent_type)) {
            return Some(parent_type);
        }
        if self.strict_null_checks
            && declaration.flags().intersects(NodeFlags::Ambient)
            && is_parameter_declaration(declaration)
        {
            parent_type = self.get_non_nullable_type(&parent_type);
        } else if self.strict_null_checks
            && matches!(
                pattern.parent().as_has_initializer().maybe_initializer(),
                Some(initializer) if !self.get_type_facts(&self.get_type_of_initializer(&initializer), None).intersects(TypeFacts::EQUndefined)
            )
        {
            parent_type = self.get_type_with_facts(&parent_type, TypeFacts::NEUndefined);
        }
        let type_: Option<Rc<Type>>;
        let declaration_as_binding_element = declaration.as_binding_element();
        if pattern.kind() == SyntaxKind::ObjectBindingPattern {
            if declaration_as_binding_element.dot_dot_dot_token.is_some() {
                parent_type = self.get_reduced_type(&parent_type);
                if parent_type.flags().intersects(TypeFlags::Unknown)
                    || !self.is_valid_spread_type(&parent_type)
                {
                    self.error(
                        Some(declaration),
                        &Diagnostics::Rest_types_may_only_be_created_from_object_types,
                        None,
                    );
                    return Some(self.error_type());
                }
                let mut literal_members: Vec<Rc<Node /*PropertyName*/>> = vec![];
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
                ));
            } else {
                let name = declaration_as_binding_element
                    .property_name
                    .clone()
                    .unwrap_or_else(|| declaration_as_binding_element.name());
                let index_type = self.get_literal_type_from_property_name(&name);
                let declared_type = self.get_indexed_access_type(
                    &parent_type,
                    &index_type,
                    Some(AccessFlags::ExpressionPosition),
                    Some(&*name),
                    Option::<&Symbol>::None,
                    None,
                );
                type_ = Some(self.get_flow_type_of_destructuring(declaration, &declared_type));
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
            );
            let index: usize = index_of_rc(
                &pattern.as_array_binding_pattern().elements,
                &declaration.node_wrapper(),
            )
            .try_into()
            .unwrap();
            if declaration_as_binding_element.dot_dot_dot_token.is_some() {
                type_ = if self.every_type(&parent_type, |type_| self.is_tuple_type(type_)) {
                    self.map_type(
                        &parent_type,
                        &mut |t| Some(self.slice_tuple_type(t, index, None)),
                        None,
                    )
                } else {
                    Some(self.create_array_type(&element_type, None))
                };
            } else if self.is_array_like_type(&parent_type) {
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
                    )
                    .unwrap_or_else(|| self.error_type());
                type_ = Some(self.get_flow_type_of_destructuring(declaration, &declared_type));
            } else {
                type_ = Some(element_type);
            }
        }
        if declaration_as_binding_element.maybe_initializer().is_none() {
            return type_;
        }
        let type_ = type_.unwrap();
        if get_effective_type_annotation_node(&walk_up_binding_elements_and_patterns(declaration))
            .is_some()
        {
            return Some(
                if self.strict_null_checks
                    && !self
                        .get_falsy_flags(
                            &self.check_declaration_initializer(declaration, Option::<&Type>::None),
                        )
                        .intersects(TypeFlags::Undefined)
                {
                    self.get_non_undefined_type(&type_)
                } else {
                    type_
                },
            );
        }
        Some(self.widen_type_inferred_from_initializer(
            declaration,
            &self.get_union_type(
                vec![
                    self.get_non_undefined_type(&type_),
                    self.check_declaration_initializer(declaration, Option::<&Type>::None),
                ],
                Some(UnionReduction::Subtype),
            ),
        ))
    }

    pub(super) fn get_type_for_declaration_from_jsdoc_comment(
        &self,
        declaration: &Node,
    ) -> Option<Rc<Type>> {
        let jsdoc_type = get_jsdoc_type(declaration)?;
        Some(self.get_type_from_type_node_(&jsdoc_type))
    }

    pub(super) fn is_null_or_undefined(&self, node: &Node /*Expression*/) -> bool {
        let expr = skip_parentheses(node, Some(true));
        expr.kind() == SyntaxKind::NullKeyword
            || expr.kind() == SyntaxKind::Identifier
                && Rc::ptr_eq(&self.get_resolved_symbol(&expr), &self.undefined_symbol())
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
    ) -> Rc<Type> {
        let is_property = is_property.unwrap_or(false);
        let is_optional = is_optional.unwrap_or(true);
        if self.strict_null_checks && is_optional {
            self.get_optional_type_(type_, Some(is_property))
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_type_for_variable_like_declaration(
        &self,
        declaration: &Node, /*ParameterDeclaration | PropertyDeclaration | PropertySignature | VariableDeclaration | BindingElement | JSDocPropertyLikeTag*/
        include_optionality: bool,
    ) -> Option<Rc<Type>> {
        if is_variable_declaration(declaration)
            && declaration.parent().parent().kind() == SyntaxKind::ForInStatement
        {
            let index_type = self.get_index_type(
                &self.get_non_nullable_type_if_needed(
                    &self.check_expression(
                        &declaration
                            .parent()
                            .parent()
                            .as_for_in_statement()
                            .expression,
                        None,
                        None,
                    ),
                ),
                None,
                None,
            );
            return Some(
                if index_type
                    .flags()
                    .intersects(TypeFlags::TypeParameter | TypeFlags::Index)
                {
                    self.get_extract_string_type(&index_type)
                } else {
                    self.string_type()
                },
            );
        }

        if is_variable_declaration(declaration)
            && declaration.parent().parent().kind() == SyntaxKind::ForOfStatement
        {
            let for_of_statement = declaration.parent().parent();
            return Some(self.check_right_hand_side_of_for_of(&for_of_statement));
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

        let declared_type = self.try_get_type_from_effective_type_node(declaration);
        if let Some(declared_type) = declared_type {
            return Some(self.add_optionality(
                &declared_type,
                Some(is_property),
                Some(is_optional),
            ));
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
                    Some(initializer) => self.is_null_or_undefined(&initializer),
                }
            {
                return Some(self.auto_type());
            }

            if matches!(declaration_as_variable_declaration.maybe_initializer(), Some(initializer) if self.is_empty_array_literal(&initializer))
            {
                return Some(self.auto_array_type());
            }
        }

        if is_parameter(declaration) {
            let func = declaration.parent();
            if func.kind() == SyntaxKind::SetAccessor && self.has_bindable_name(&func) {
                let getter = get_declaration_of_kind(
                    &self.get_symbol_of_node(&declaration.parent()).unwrap(),
                    SyntaxKind::GetAccessor,
                );
                if let Some(getter) = getter {
                    let getter_signature = self.get_signature_from_declaration_(&getter);
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
                            return Some(self.get_type_of_symbol(
                                getter_signature.this_parameter.as_ref().unwrap(),
                            ));
                        }
                    }
                    return Some(self.get_return_type_of_signature(&getter_signature));
                }
            }
            if is_in_js_file(Some(declaration)) {
                let type_tag = get_jsdoc_type(&func);
                if let Some(type_tag) = type_tag {
                    if is_function_type_node(&type_tag) {
                        let signature = self.get_signature_from_declaration_(&type_tag);
                        let pos: usize = index_of_rc(
                            func.as_function_like_declaration().parameters(),
                            &declaration.node_wrapper(),
                        )
                        .try_into()
                        .unwrap();
                        return Some(
                            if declaration
                                .as_parameter_declaration()
                                .dot_dot_dot_token
                                .is_some()
                            {
                                self.get_rest_type_at_position(&signature, pos)
                            } else {
                                self.get_type_at_position(&signature, pos)
                            },
                        );
                    }
                }
            }
            let type_ = if declaration.symbol().escaped_name() == &InternalSymbolName::This() {
                self.get_contextual_this_parameter_type(&func)
            } else {
                self.get_contextually_typed_parameter_type(declaration)
            };
            if let Some(type_) = type_ {
                return Some(self.add_optionality(&type_, Some(false), Some(is_optional)));
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
                    &self.get_symbol_of_node(declaration).unwrap(),
                    get_declared_expando_initializer(declaration),
                );
                if container_object_type.is_some() {
                    return container_object_type;
                }
            }
            let type_ = self.widen_type_inferred_from_initializer(
                declaration,
                &self.check_declaration_initializer(declaration, Option::<&Type>::None),
            );
            return Some(self.add_optionality(&type_, Some(is_property), Some(is_optional)));
        }

        if is_property_declaration(declaration)
            && (self.no_implicit_any || is_in_js_file(Some(declaration)))
        {
            if !has_static_modifier(declaration) {
                let constructor = self.find_constructor_declaration(&declaration.parent());
                let type_ = if let Some(constructor) = constructor {
                    self.get_flow_type_in_constructor(&declaration.symbol(), &constructor)
                } else if get_effective_modifier_flags(declaration)
                    .intersects(ModifierFlags::Ambient)
                {
                    self.get_type_of_property_in_base_class(&declaration.symbol())
                } else {
                    None
                };
                return type_
                    .map(|type_| self.add_optionality(&type_, Some(true), Some(is_optional)));
            } else {
                let static_blocks = filter(
                    Some(declaration.parent().as_class_like_declaration().members()),
                    |member: &Rc<Node>| is_class_static_block_declaration(member),
                )
                .unwrap();
                let type_ = if !static_blocks.is_empty() {
                    self.get_flow_type_in_static_blocks(&declaration.symbol(), &static_blocks)
                } else if get_effective_modifier_flags(declaration)
                    .intersects(ModifierFlags::Ambient)
                {
                    self.get_type_of_property_in_base_class(&declaration.symbol())
                } else {
                    None
                };
                return type_
                    .map(|type_| self.add_optionality(&type_, Some(true), Some(is_optional)));
            }
        }

        if is_jsx_attribute(declaration) {
            return Some(self.true_type());
        }

        if is_binding_pattern(declaration.as_named_declaration().maybe_name()) {
            return Some(self.get_type_from_binding_pattern(
                &declaration.as_named_declaration().name(),
                Some(false),
                Some(true),
            ));
        }

        None
    }

    pub(super) fn is_constructor_declared_property(&self, symbol: &Symbol) -> bool {
        if matches!(symbol.maybe_value_declaration(), Some(value_declaration) if is_binary_expression(&value_declaration))
        {
            let links = self.get_symbol_links(symbol);
            if (*links).borrow().is_constructor_declared_property.is_none() {
                links.borrow_mut().is_constructor_declared_property = Some(false);
                let is_constructor_declared_property =
                    self.get_declaring_constructor(symbol).is_some()
                        && maybe_every(
                            symbol.maybe_declarations().as_deref(),
                            |declaration: &Rc<Node>, _| {
                                is_binary_expression(declaration)
                                    && self.is_possibly_aliased_this_property(declaration, None)
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
                                        )
                                        .is_none()
                            },
                        );
                links.borrow_mut().is_constructor_declared_property =
                    Some(is_constructor_declared_property);
            }
            return (*links).borrow().is_constructor_declared_property.unwrap();
        }
        false
    }

    pub(super) fn is_auto_typed_property(&self, symbol: &Symbol) -> bool {
        let declaration = symbol.maybe_value_declaration();
        matches!(
            declaration,
            Some(declaration) if is_property_declaration(&declaration) && get_effective_type_annotation_node(&declaration).is_none() &&
                declaration.as_property_declaration().maybe_initializer().is_none() && (self.no_implicit_any || is_in_js_file(Some(&*declaration)))
        )
    }

    pub(super) fn get_declaring_constructor(&self, symbol: &Symbol) -> Option<Rc<Node>> {
        let symbol_declarations = symbol.maybe_declarations();
        let symbol_declarations = symbol_declarations.as_deref()?;
        for declaration in symbol_declarations {
            let container = get_this_container(&declaration, false);
            if
            /*container &&*/
            container.kind() == SyntaxKind::Constructor
                || self.is_js_constructor(Some(&*container))
            {
                return Some(container);
            }
        }
        None
    }

    pub(super) fn get_flow_type_from_common_js_export(&self, symbol: &Symbol) -> Rc<Type> {
        let file = get_source_file_of_node(
            symbol
                .maybe_declarations()
                .as_ref()
                .unwrap()
                .get(0)
                .map(Clone::clone),
        )
        .unwrap();
        let access_name = unescape_leading_underscores(symbol.escaped_name());
        let are_all_module_exports = every(
            symbol.maybe_declarations().as_deref().unwrap(),
            |d: &Rc<Node>, _| {
                is_in_js_file(Some(&**d))
                    && is_access_expression(d)
                    && is_module_exports_access_expression(d)
            },
        );
        let reference: Rc<Node> = if are_all_module_exports {
            synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_property_access_expression(
                            synthetic_factory_,
                            factory_
                                .create_property_access_expression(
                                    synthetic_factory_,
                                    factory_
                                        .create_identifier(
                                            synthetic_factory_,
                                            "module",
                                            Option::<NodeArray>::None,
                                            None,
                                        )
                                        .into(),
                                    Into::<Rc<Node>>::into(factory_.create_identifier(
                                        synthetic_factory_,
                                        "exports",
                                        Option::<NodeArray>::None,
                                        None,
                                    )),
                                )
                                .into(),
                            access_name,
                        )
                        .into()
                })
            })
        } else {
            synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_property_access_expression(
                            synthetic_factory_,
                            factory_
                                .create_identifier(
                                    synthetic_factory_,
                                    "exports",
                                    Option::<NodeArray>::None,
                                    None,
                                )
                                .into(),
                            access_name,
                        )
                        .into()
                })
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
        static_blocks: &[Rc<Node /*ClassStaticBlockDeclaration*/>],
    ) -> Option<Rc<Type>> {
        let access_name: StringOrRcNode = if starts_with(&**symbol.escaped_name(), "__#") {
            synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    Into::<Rc<Node>>::into(factory_.create_private_identifier(
                        synthetic_factory_,
                        (&*symbol.escaped_name()).split("@").nth(1).unwrap(),
                    ))
                    .into()
                })
            })
        } else {
            unescape_leading_underscores(symbol.escaped_name()).into()
        };
        for static_block in static_blocks {
            let reference: Rc<Node> = synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_property_access_expression(
                            synthetic_factory_,
                            factory_.create_this(synthetic_factory_).into(),
                            access_name.clone(),
                        )
                        .into()
                })
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
            let flow_type = self.get_flow_type_of_property(&reference, Some(symbol));
            if self.no_implicit_any
                && (Rc::ptr_eq(&flow_type, &self.auto_type())
                    || Rc::ptr_eq(&flow_type, &self.auto_array_type()))
            {
                self.error(
                    symbol.maybe_value_declaration(),
                    &Diagnostics::Member_0_implicitly_has_an_1_type,
                    Some(vec![
                        self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None),
                        self.type_to_string_(&flow_type, Option::<&Node>::None, None, None),
                    ]),
                );
            }
            if self.every_type(&flow_type, |type_| self.is_nullable_type(type_)) {
                continue;
            }
            return Some(self.convert_auto_to_any(&flow_type));
        }
        None
    }

    pub(super) fn get_flow_type_in_constructor(
        &self,
        symbol: &Symbol,
        constructor: &Node, /*ConstructorDeclaration*/
    ) -> Option<Rc<Type>> {
        let access_name: StringOrRcNode = if starts_with(&**symbol.escaped_name(), "__#") {
            synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    Into::<Rc<Node>>::into(factory_.create_private_identifier(
                        synthetic_factory_,
                        (&*symbol.escaped_name()).split("@").nth(1).unwrap(),
                    ))
                    .into()
                })
            })
        } else {
            unescape_leading_underscores(symbol.escaped_name()).into()
        };
        let reference: Rc<Node> = synthetic_factory.with(|synthetic_factory_| {
            factory.with(|factory_| {
                factory_
                    .create_property_access_expression(
                        synthetic_factory_,
                        factory_.create_this(synthetic_factory_).into(),
                        access_name.clone(),
                    )
                    .into()
            })
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
        let flow_type = self.get_flow_type_of_property(&reference, Some(symbol));
        if self.no_implicit_any
            && (Rc::ptr_eq(&flow_type, &self.auto_type())
                || Rc::ptr_eq(&flow_type, &self.auto_array_type()))
        {
            self.error(
                symbol.maybe_value_declaration(),
                &Diagnostics::Member_0_implicitly_has_an_1_type,
                Some(vec![
                    self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None),
                    self.type_to_string_(&flow_type, Option::<&Node>::None, None, None),
                ]),
            );
        }
        if self.every_type(&flow_type, |type_| self.is_nullable_type(type_)) {
            None
        } else {
            Some(self.convert_auto_to_any(&flow_type))
        }
    }

    pub(super) fn get_flow_type_of_property<TProp: Borrow<Symbol>>(
        &self,
        reference: &Node,
        prop: Option<TProp>,
    ) -> Rc<Type> {
        let initial_type = prop.and_then(|prop| {
            let prop = prop.borrow();
            if matches!(
                prop.maybe_value_declaration(),
                Some(value_declaration) if !self.is_auto_typed_property(prop) || get_effective_modifier_flags(&value_declaration).intersects(ModifierFlags::Ambient)
            ) {
                self.get_type_of_property_in_base_class(prop)
            } else {
                None
            }
        }).unwrap_or_else(|| self.undefined_type());
        self.get_flow_type_of_reference(
            reference,
            &self.auto_type(),
            Some(initial_type),
            Option::<&Node>::None,
        )
    }

    pub(super) fn get_widened_type_for_assignment_declaration<TResolvedSymbol: Borrow<Symbol>>(
        &self,
        symbol: &Symbol,
        resolved_symbol: Option<TResolvedSymbol>,
    ) -> Rc<Type> {
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
                    .and_then(|value_declaration| {
                        self.get_js_container_object_type(
                            &value_declaration,
                            symbol,
                            Some(&*container),
                        )
                    });
            return container_object_type.unwrap_or_else(|| {
                self.get_widened_literal_type(&self.check_expression_cached(&container, None))
            });
        }
        let mut type_: Option<Rc<Type>> = None;
        let mut defined_in_constructor = false;
        let mut defined_in_method = false;
        if self.is_constructor_declared_property(symbol) {
            type_ = self.get_flow_type_in_constructor(
                symbol,
                &self.get_declaring_constructor(symbol).unwrap(),
            );
        }
        let resolved_symbol =
            resolved_symbol.map(|resolved_symbol| resolved_symbol.borrow().symbol_wrapper());
        if type_.is_none() {
            let mut types: Option<Vec<Rc<Type>>> = None;
            if let Some(symbol_declarations) = symbol.maybe_declarations().as_deref() {
                let mut jsdoc_type: Option<Rc<Type>> = None;
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
                            && self.is_possibly_aliased_this_property(&expression, Some(kind))
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
                        );
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
                                )
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
                    return self.error_type();
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
                    let prop_type = self.get_type_of_property_in_base_class(symbol);
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
                    Some(|t: &Rc<Type>| t.flags().intersects(!TypeFlags::Nullable)),
                ) {
                    constructor_types.unwrap()
                } else {
                    types
                };
                type_ = Some(self.get_union_type(source_types, Some(UnionReduction::Subtype)));
            }
        }
        let type_ = type_.unwrap();
        let widened = self.get_widened_type(&self.add_optionality(
            &type_,
            Some(false),
            Some(defined_in_method && !defined_in_constructor),
        ));
        if let Some(symbol_value_declaration) = symbol.maybe_value_declaration() {
            if Rc::ptr_eq(
                &self.filter_type(&widened, |t| t.flags().intersects(!TypeFlags::Nullable)),
                &self.never_type(),
            ) {
                self.report_implicit_any(&symbol_value_declaration, &self.any_type(), None);
                return self.any_type();
            }
        }
        widened
    }

    pub(super) fn get_js_container_object_type<TInit: Borrow<Node>>(
        &self,
        decl: &Node,
        symbol: &Symbol,
        init: Option<TInit>,
    ) -> Option<Rc<Type>> {
        if !is_in_js_file(Some(decl)) {
            return None;
        }
        let init = init?;
        let init = init.borrow();
        if !is_object_literal_expression(init)
            || !init.as_object_literal_expression().properties.is_empty()
        {
            return None;
        }
        let mut exports = create_symbol_table(None);
        let mut decl = decl.node_wrapper();
        while is_binary_expression(&decl) || is_property_access_expression(&decl) {
            let s = self.get_symbol_of_node(&decl);
            if let Some(s) = s {
                if let Some(s_exports) = s.maybe_exports().as_deref() {
                    let s_exports = RefCell::borrow(s_exports);
                    if !s_exports.is_empty() {
                        self.merge_symbol_table(&mut exports, &s_exports, None);
                    }
                }
            }
            decl = if is_binary_expression(&decl) {
                decl.parent()
            } else {
                decl.parent().parent()
            };
        }
        let s = self.get_symbol_of_node(&decl);
        if let Some(s) = s {
            if let Some(s_exports) = s.maybe_exports().as_deref() {
                let s_exports = RefCell::borrow(s_exports);
                if !s_exports.is_empty() {
                    self.merge_symbol_table(&mut exports, &s_exports, None);
                }
            }
        }
        let type_: Rc<Type> = self
            .create_anonymous_type(
                Some(symbol),
                Rc::new(RefCell::new(exports)),
                vec![],
                vec![],
                vec![],
            )
            .into();
        let type_as_object_type = type_.as_object_type();
        type_as_object_type
            .set_object_flags(type_as_object_type.object_flags() | ObjectFlags::JSLiteral);
        Some(type_)
    }

    pub(super) fn get_annotated_type_for_assignment_declaration<TDeclaredType: Borrow<Type>>(
        &self,
        declared_type: Option<TDeclaredType>,
        expression: &Node, /*Expression*/
        symbol: &Symbol,
        declaration: &Node, /*Declaration*/
    ) -> Option<Rc<Type>> {
        let type_node = get_effective_type_annotation_node(&expression.parent());
        let declared_type =
            declared_type.map(|declared_type| declared_type.borrow().type_wrapper());
        if let Some(type_node) = type_node {
            let type_ = self.get_widened_type(&self.get_type_from_type_node_(&type_node));
            if declared_type.is_none() {
                return Some(type_);
            }
            let declared_type = declared_type.as_ref().unwrap();
            if !self.is_error_type(declared_type)
                && !self.is_error_type(&type_)
                && !self.is_type_identical_to(&declared_type, &type_)
            {
                self.error_next_variable_or_property_declaration_must_have_same_type(
                    Option::<&Node>::None,
                    &declared_type,
                    declaration,
                    &type_,
                );
            }
        }
        if let Some(symbol_parent) = symbol.maybe_parent() {
            if let Some(symbol_parent_value_declaration) = symbol_parent.maybe_value_declaration() {
                let type_node =
                    get_effective_type_annotation_node(&symbol_parent_value_declaration);
                if let Some(type_node) = type_node {
                    let annotation_symbol = self.get_property_of_type_(
                        &self.get_type_from_type_node_(&type_node),
                        symbol.escaped_name(),
                        None,
                    );
                    if let Some(annotation_symbol) = annotation_symbol {
                        return Some(self.get_non_missing_type_of_symbol(&annotation_symbol));
                    }
                }
            }
        }

        declared_type
    }

    pub(super) fn get_initializer_type_from_assignment_declaration<
        TResolvedSymbol: Borrow<Symbol>,
    >(
        &self,
        symbol: &Symbol,
        resolved_symbol: Option<TResolvedSymbol>,
        expression: &Node, /*BinaryExpression | CallExpression*/
        kind: AssignmentDeclarationKind,
    ) -> Rc<Type> {
        let resolved_symbol =
            resolved_symbol.map(|resolved_symbol| resolved_symbol.borrow().symbol_wrapper());
        if is_call_expression(expression) {
            if let Some(resolved_symbol) = resolved_symbol.as_ref() {
                return self.get_type_of_symbol(resolved_symbol);
            }
            let object_lit_type =
                self.check_expression_cached(&expression.as_call_expression().arguments[2], None);
            let value_type = self.get_type_of_property_of_type_(
                &object_lit_type,
                &__String::new("value".to_owned()),
            );
            if let Some(value_type) = value_type {
                return value_type;
            }
            let get_func = self
                .get_type_of_property_of_type_(&object_lit_type, &__String::new("get".to_owned()));
            if let Some(get_func) = get_func {
                let get_sig = self.get_single_call_signature(&get_func);
                if let Some(get_sig) = get_sig {
                    return self.get_return_type_of_signature(&get_sig);
                }
            }
            let set_func = self
                .get_type_of_property_of_type_(&object_lit_type, &__String::new("set".to_owned()));
            if let Some(set_func) = set_func {
                let set_sig = self.get_single_call_signature(&set_func);
                if let Some(set_sig) = set_sig {
                    return self.get_type_of_first_parameter_of_signature(&set_sig);
                }
            }
            return self.any_type();
        }
        let expression_as_binary_expression = expression.as_binary_expression();
        if self.contains_same_named_this_property(
            &expression_as_binary_expression.left,
            &expression_as_binary_expression.right,
        ) {
            return self.any_type();
        }
        let type_ = if let Some(resolved_symbol) = resolved_symbol.as_ref() {
            self.get_type_of_symbol(resolved_symbol)
        } else {
            self.get_widened_literal_type(
                &self.check_expression_cached(&expression_as_binary_expression.right, None),
            )
        };
        if type_.flags().intersects(TypeFlags::Object)
            && kind == AssignmentDeclarationKind::ModuleExports
            && symbol.escaped_name() == &InternalSymbolName::ExportEquals()
        {
            let exported_type = self.resolve_structured_type_members(&type_);
            let mut members = create_symbol_table(None);
            let exported_type_as_resolved_type = exported_type.as_resolved_type();
            copy_entries(
                &RefCell::borrow(&exported_type_as_resolved_type.members()),
                &mut members,
            );
            let initial_size = members.len();
            if let Some(resolved_symbol) = resolved_symbol.as_ref() {
                let mut resolved_symbol_exports = resolved_symbol.maybe_exports();
                if resolved_symbol_exports.is_none() {
                    *resolved_symbol_exports =
                        Some(Rc::new(RefCell::new(create_symbol_table(None))));
                }
            }
            for (name, s) in
                &*RefCell::borrow(&resolved_symbol.as_deref().unwrap_or(symbol).exports())
            {
                let exported_member = members.get(name).map(Clone::clone);
                if let Some(exported_member) =
                    exported_member.filter(|exported_member| !Rc::ptr_eq(exported_member, s))
                {
                    if s.flags().intersects(SymbolFlags::Value)
                        && exported_member.flags().intersects(SymbolFlags::Value)
                    {
                        if let Some(s_value_declaration) = s.maybe_value_declaration() {
                            if let Some(exported_member_value_declaration) =
                                exported_member.maybe_value_declaration()
                            {
                                if !Rc::ptr_eq(
                                    &get_source_file_of_node(Some(&*s_value_declaration)).unwrap(),
                                    &get_source_file_of_node(Some(
                                        &*exported_member_value_declaration,
                                    ))
                                    .unwrap(),
                                ) {
                                    let unescaped_name =
                                        unescape_leading_underscores(s.escaped_name());
                                    let exported_member_name = try_cast(
                                        &exported_member_value_declaration,
                                        |node: &&Rc<Node>| is_named_declaration(node),
                                    )
                                    .and_then(|named_declaration: &Rc<Node>| {
                                        named_declaration.as_named_declaration().maybe_name()
                                    })
                                    .unwrap_or_else(|| exported_member_value_declaration);
                                    add_related_info(
                                        &self.error(
                                            Some(&*s_value_declaration),
                                            &Diagnostics::Duplicate_identifier_0,
                                            Some(vec![unescaped_name.clone()]),
                                        ),
                                        vec![Rc::new(
                                            create_diagnostic_for_node(
                                                &exported_member_name,
                                                &Diagnostics::_0_was_also_declared_here,
                                                Some(vec![unescaped_name.clone()]),
                                            )
                                            .into(),
                                        )],
                                    );
                                    add_related_info(
                                        &self.error(
                                            Some(&*exported_member_name),
                                            &Diagnostics::Duplicate_identifier_0,
                                            Some(vec![unescaped_name.clone()]),
                                        ),
                                        vec![Rc::new(
                                            create_diagnostic_for_node(
                                                &s_value_declaration,
                                                &Diagnostics::_0_was_also_declared_here,
                                                Some(vec![unescaped_name.clone()]),
                                            )
                                            .into(),
                                        )],
                                    );
                                }
                            }
                        }
                        let union: Rc<Symbol> = self
                            .create_symbol(s.flags() | exported_member.flags(), name.clone(), None)
                            .into();
                        union
                            .as_transient_symbol()
                            .symbol_links()
                            .borrow_mut()
                            .type_ = Some(self.get_union_type(
                            vec![
                                self.get_type_of_symbol(s),
                                self.get_type_of_symbol(&exported_member),
                            ],
                            None,
                        ));
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
                        members.insert(name.clone(), self.merge_symbol(s, &exported_member, None));
                    }
                } else {
                    members.insert(name.clone(), s.clone());
                }
            }
            let result: Rc<Type> = self
                .create_anonymous_type(
                    if initial_size != members.len() {
                        None
                    } else {
                        exported_type.maybe_symbol()
                    },
                    Rc::new(RefCell::new(members)),
                    exported_type_as_resolved_type.call_signatures().clone(),
                    exported_type_as_resolved_type
                        .construct_signatures()
                        .clone(),
                    exported_type_as_resolved_type.index_infos().clone(),
                )
                .into();
            let result_as_object_type = result.as_object_type();
            result_as_object_type.set_object_flags(
                result_as_object_type.object_flags()
                    | get_object_flags(&type_) & ObjectFlags::JSLiteral,
            );
            if let Some(result_symbol) = result.maybe_symbol() {
                if result_symbol.flags().intersects(SymbolFlags::Class)
                    && Rc::ptr_eq(
                        &type_,
                        &self.get_declared_type_of_class_or_interface(&result_symbol),
                    )
                {
                    result_as_object_type.set_object_flags(
                        result_as_object_type.object_flags() | ObjectFlags::IsClassInstanceClone,
                    );
                }
            }
            return result;
        }
        if self.is_empty_array_literal_type(&type_) {
            self.report_implicit_any(expression, &self.any_array_type(), None);
            return self.any_array_type();
        }
        type_
    }

    pub(super) fn contains_same_named_this_property(
        &self,
        this_expression: &Node, /*Expression*/
        expression: &Node,      /*Expression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_declaration_in_constructor(
        &self,
        expression: &Node, /*Expression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_constructor_defined_this_assignment_types(
        &self,
        types: &[Rc<Type>],
        declarations: &[Rc<Node /*Declaration*/>],
    ) -> Option<Vec<Rc<Type>>> {
        unimplemented!()
    }

    pub(super) fn get_type_from_binding_pattern(
        &self,
        pattern: &Node, /*BindingPattern*/
        include_pattern_in_type: Option<bool>,
        report_errors: Option<bool>,
    ) -> Rc<Type> {
        let include_pattern_in_type = include_pattern_in_type.unwrap_or(false);
        let report_errors = report_errors.unwrap_or(false);
        unimplemented!()
    }

    pub(super) fn get_widened_type_for_variable_like_declaration(
        &self,
        declaration: &Node,
    ) -> Rc<Type> {
        self.widen_type_for_variable_like_declaration(
            self.get_type_for_variable_like_declaration(declaration, true),
            declaration,
        )
    }

    pub(super) fn widen_type_for_variable_like_declaration<TTypeRef: Borrow<Type>>(
        &self,
        type_: Option<TTypeRef>,
        declaration: &Node,
    ) -> Rc<Type> {
        if let Some(type_) = type_ {
            return self.get_widened_type(type_.borrow());
        }
        unimplemented!()
    }

    pub(super) fn try_get_type_from_effective_type_node(
        &self,
        declaration: &Node, /*Declaration*/
    ) -> Option<Rc<Type>> {
        let type_node = get_effective_type_annotation_node(declaration);
        type_node.map(|type_node| self.get_type_from_type_node_(&*type_node))
    }

    pub(super) fn get_type_of_variable_or_parameter_or_property(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        let links_type_is_none = { (*links).borrow().type_.is_none() };
        if links_type_is_none {
            let type_ = self.get_type_of_variable_or_parameter_or_property_worker(symbol);
            let mut links_ref = links.borrow_mut();
            if links_ref.type_.is_none() {
                links_ref.type_ = Some(type_);
            }
        }
        let links_ref = (*links).borrow();
        links_ref.type_.clone().unwrap()
    }

    pub(super) fn get_type_of_variable_or_parameter_or_property_worker(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type> {
        Debug_.assert_is_defined(&symbol.maybe_value_declaration(), None);
        let declaration = symbol.maybe_value_declaration().unwrap();

        let type_: Rc<Type>;
        if false {
            unimplemented!()
        } else if is_property_assignment(&declaration) {
            type_ = self
                .try_get_type_from_effective_type_node(&declaration)
                .unwrap_or_else(|| self.check_property_assignment(&declaration, None));
        } else if is_property_signature(&declaration) || is_variable_declaration(&declaration) {
            type_ = self.get_widened_type_for_variable_like_declaration(&declaration);
        } else {
            unimplemented!()
        }

        type_
    }

    pub(super) fn get_base_type_variable_of_class(&self, symbol: &Symbol) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_instantiated_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        let mut links = links.borrow_mut();
        if links.type_.is_none() {
            let type_ = self.instantiate_type(
                Some(self.get_type_of_symbol(links.target.as_ref().unwrap())),
                links.mapper.as_ref(),
            );
            links.type_ = type_;
        }
        links.type_.clone().unwrap()
    }

    pub(super) fn get_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        let check_flags = get_check_flags(symbol);
        if check_flags.intersects(CheckFlags::Instantiated) {
            return self.get_type_of_instantiated_symbol(symbol);
        }
        if symbol
            .flags()
            .intersects(SymbolFlags::Variable | SymbolFlags::Property)
        {
            return self.get_type_of_variable_or_parameter_or_property(symbol);
        }
        unimplemented!()
    }

    pub(super) fn get_non_missing_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        self.remove_missing_type(
            &self.get_type_of_symbol(symbol),
            symbol.flags().intersects(SymbolFlags::Optional),
        )
    }

    pub(super) fn append_type_parameters(
        &self,
        mut type_parameters: Option<Vec<Rc<Type>>>,
        declarations: &[Rc<Node>],
    ) -> Option<Vec<Rc<Type>>> {
        for declaration in declarations {
            type_parameters = Some(maybe_append_if_unique_rc(
                type_parameters,
                &self.get_declared_type_of_type_parameter(
                    &self.get_symbol_of_node(&**declaration).unwrap(),
                ),
            ));
        }
        type_parameters
    }

    pub(super) fn get_outer_type_parameters(
        &self,
        node: &Node,
    ) -> Option<Vec<Rc<Type /*TypeParameter*/>>> {
        None
    }

    pub(super) fn get_outer_type_parameters_of_class_or_interface(
        &self,
        symbol: &Symbol,
    ) -> Option<Vec<Rc<Type /*TypeParameter*/>>> {
        let declaration = if symbol.flags().intersects(SymbolFlags::Class) {
            symbol.maybe_value_declaration()
        } else {
            get_declaration_of_kind(symbol, SyntaxKind::InterfaceDeclaration)
        };
        Debug_.assert(
            declaration.is_some(),
            Some("Class was missing valueDeclaration -OR- non-class had no interface declarations"),
        );
        let declaration = declaration.unwrap();
        self.get_outer_type_parameters(&*declaration)
    }

    pub(super) fn get_local_type_parameters_of_class_or_interface_or_type_alias(
        &self,
        symbol: &Symbol,
    ) -> Option<Vec<Rc<Type /*TypeParameter*/>>> {
        let declarations = symbol.maybe_declarations();
        if declarations.is_none() {
            return None;
        }
        let declarations = declarations.as_ref().unwrap();
        let mut result: Option<Vec<Rc<Type /*TypeParameter*/>>> = None;
        for node in declarations {
            if node.kind() == SyntaxKind::InterfaceDeclaration
                || node.kind() == SyntaxKind::ClassDeclaration
                || node.kind() == SyntaxKind::ClassExpression
                || false
                || is_type_alias(&**node)
            {
                let declaration = node;
                result = self.append_type_parameters(
                    result,
                    &get_effective_type_parameter_declarations(&*declaration),
                );
            }
        }
        result
    }

    pub(super) fn get_declared_type_of_class_or_interface(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type /*InterfaceType*/> {
        let links = self.get_symbol_links(symbol);
        let original_links = links.clone();
        let mut original_links_ref = original_links.borrow_mut();
        if original_links_ref.declared_type.is_none() {
            let kind = if symbol.flags().intersects(SymbolFlags::Class) {
                ObjectFlags::Class
            } else {
                ObjectFlags::Interface
            };

            let type_ = self.create_object_type(kind, Some(symbol));
            let outer_type_parameters =
                self.get_outer_type_parameters_of_class_or_interface(symbol);
            let local_type_parameters =
                self.get_local_type_parameters_of_class_or_interface_or_type_alias(symbol);
            let mut need_to_set_constraint = false;
            let type_: InterfaceType = if outer_type_parameters.is_some()
                || local_type_parameters.is_some()
                || kind == ObjectFlags::Class
                || false
            {
                need_to_set_constraint = true;
                type_.set_object_flags(type_.object_flags() | ObjectFlags::Reference);
                let mut this_type = self.create_type_parameter(Some(symbol.symbol_wrapper()));
                this_type.is_this_type = Some(true);
                BaseInterfaceType::new(
                    type_,
                    Some(concatenate(
                        outer_type_parameters.clone().unwrap_or_else(|| vec![]),
                        local_type_parameters.clone().unwrap_or_else(|| vec![]),
                    )),
                    outer_type_parameters,
                    local_type_parameters,
                    Some(this_type.into()),
                )
            } else {
                BaseInterfaceType::new(type_, None, None, None, None)
            }
            .into();
            let type_rc: Rc<Type> = type_.into();
            if need_to_set_constraint {
                *type_rc
                    .as_interface_type()
                    .maybe_this_type_mut()
                    .as_ref()
                    .unwrap()
                    .as_type_parameter()
                    .constraint
                    .borrow_mut() = Some(Rc::downgrade(&type_rc));
            }
            original_links_ref.declared_type = Some(type_rc.clone());
            if !Rc::ptr_eq(&links, &original_links) {
                let mut links_ref = links.borrow_mut();
                links_ref.declared_type = Some(type_rc);
            }
        }
        original_links_ref.declared_type.clone().unwrap()
    }

    pub(super) fn get_declared_type_of_type_alias(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        let mut links = links.borrow_mut();
        if links.declared_type.is_none() {
            let declaration = Debug_.check_defined(
                symbol
                    .maybe_declarations()
                    .as_ref()
                    .and_then(|declarations| {
                        declarations
                            .iter()
                            .find(|declaration| is_type_alias(&***declaration))
                            .map(|rc| rc.clone())
                    }),
                None,
            );
            let type_node = if false {
                unimplemented!()
            } else {
                Some(declaration.as_type_alias_declaration().type_.clone())
            };
            let type_ = type_node.map_or_else(
                || self.error_type(),
                |type_node| self.get_type_from_type_node_(&type_node),
            );
            if true {
                let type_parameters =
                    self.get_local_type_parameters_of_class_or_interface_or_type_alias(symbol);
                if let Some(type_parameters) = type_parameters {
                    unimplemented!()
                }
            } else {
                unimplemented!()
            }
            links.declared_type = Some(type_);
        }
        links.declared_type.clone().unwrap()
    }

    pub(super) fn get_base_type_of_enum_literal_type(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_declared_type_of_type_parameter(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type /*TypeParameter*/> {
        let links = self.get_symbol_links(symbol);
        let mut links = links.borrow_mut();
        if links.declared_type.is_none() {
            links.declared_type = Some(
                self.create_type_parameter(Some(symbol.symbol_wrapper()))
                    .into(),
            );
        }
        links.declared_type.clone().unwrap()
    }

    pub(super) fn get_declared_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        self.try_get_declared_type_of_symbol(symbol)
            .unwrap_or_else(|| unimplemented!())
    }

    pub(super) fn try_get_declared_type_of_symbol(&self, symbol: &Symbol) -> Option<Rc<Type>> {
        if symbol
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface)
        {
            return Some(self.get_declared_type_of_class_or_interface(symbol));
        }
        if symbol.flags().intersects(SymbolFlags::TypeAlias) {
            return Some(self.get_declared_type_of_type_alias(symbol));
        }
        if symbol.flags().intersects(SymbolFlags::TypeParameter) {
            return Some(self.get_declared_type_of_type_parameter(symbol));
        }
        unimplemented!()
    }

    pub(super) fn create_instantiated_symbol_table(
        &self,
        symbols: &[Rc<Symbol>],
        mapper: &TypeMapper,
        mapping_this_only: bool,
    ) -> SymbolTable {
        let mut result = create_symbol_table(None);
        for symbol in symbols {
            result.insert(
                symbol.escaped_name().clone(),
                if mapping_this_only && true {
                    symbol.clone()
                } else {
                    self.instantiate_symbol(&symbol, mapper)
                },
            );
        }
        result
    }

    pub(super) fn resolve_declared_members(&self, type_: &Type /*InterfaceType*/) -> Rc<Type> {
        let type_as_interface_type = type_.as_interface_type();
        if type_as_interface_type.maybe_declared_properties().is_none() {
            let symbol = type_.symbol();
            let members = self.get_members_of_symbol(&symbol);
            type_as_interface_type
                .set_declared_properties(self.get_named_members(&*(*members).borrow()));
        }
        type_.type_wrapper()
    }

    pub(super) fn is_type_usable_as_property_name(&self, type_: &Type) -> bool {
        type_
            .flags()
            .intersects(TypeFlags::StringOrNumberLiteralOrUnique)
    }

    pub(super) fn has_bindable_name(&self, node: &Node /*Declaration*/) -> bool {
        !has_dynamic_name(node) || unimplemented!()
    }

    pub(super) fn get_property_name_from_type(
        &self,
        type_: &Type, /*StringLiteralType | NumberLiteralType | UniqueESSymbolType*/
    ) -> __String {
        if type_
            .flags()
            .intersects(TypeFlags::StringLiteral | TypeFlags::NumberLiteral)
        {
            return match type_ {
                Type::LiteralType(LiteralType::NumberLiteralType(number_literal_type)) => {
                    escape_leading_underscores(&number_literal_type.value.to_string())
                }
                Type::LiteralType(LiteralType::StringLiteralType(string_literal_type)) => {
                    escape_leading_underscores(&string_literal_type.value)
                }
                _ => panic!("Expected NumberLiteralType or StringLiteralType"),
            };
        }
        Debug_.fail(None)
    }

    pub(super) fn get_resolved_members_or_exports_of_symbol(
        &self,
        symbol: &Symbol,
        resolution_kind: MembersOrExportsResolutionKind,
    ) -> Rc<RefCell<UnderscoreEscapedMap<Rc<Symbol>>>> {
        unimplemented!()
    }

    pub(super) fn get_members_of_symbol(&self, symbol: &Symbol) -> Rc<RefCell<SymbolTable>> {
        if false {
            unimplemented!()
        } else {
            symbol
                .maybe_members()
                .clone()
                .unwrap_or_else(|| unimplemented!())
        }
    }

    pub(super) fn get_late_bound_symbol(&self, symbol: &Symbol) -> Rc<Symbol> {
        symbol.symbol_wrapper()
    }

    pub(super) fn resolve_object_type_members(
        &self,
        type_: &Type,  /*ObjectType*/
        source: &Type, /*InterfaceTypeWithDeclaredMembers*/
        type_parameters: Vec<Rc<Type /*TypeParameter*/>>,
        type_arguments: Vec<Rc<Type>>,
    ) {
        let mut mapper: Option<TypeMapper> = None;
        let members: Rc<RefCell<SymbolTable>>;
        let call_signatures: Vec<Rc<Signature>>;
        let construct_signatures: Vec<Rc<Signature>>;
        let source_as_interface_type_with_declared_members =
            source.as_interface_type_with_declared_members();
        if range_equals_rc(&type_parameters, &type_arguments, 0, type_parameters.len()) {
            members = if let Some(source_symbol) = source.maybe_symbol() {
                self.get_members_of_symbol(&source_symbol)
            } else {
                unimplemented!()
            };
            call_signatures = source_as_interface_type_with_declared_members
                .declared_call_signatures()
                .clone();
            construct_signatures = source_as_interface_type_with_declared_members
                .declared_construct_signatures()
                .clone();
        } else {
            let type_parameters_len_is_1 = type_parameters.len() == 1;
            mapper = Some(self.create_type_mapper(type_parameters, Some(type_arguments)));
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
        }
        self.set_structured_type_members(
            type_.as_object_type(),
            members,
            call_signatures,
            construct_signatures,
            vec![], // TODO: this is wrong
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
        *sig.resolved_return_type.borrow_mut() = resolved_return_type;
        sig.resolved_type_predicate = resolved_type_predicate;
        sig.set_min_argument_count(min_argument_count);
        sig
    }

    pub(super) fn create_union_signature(
        &self,
        signature: Rc<Signature>,
        union_signatures: &[Rc<Signature>],
    ) -> Signature {
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
