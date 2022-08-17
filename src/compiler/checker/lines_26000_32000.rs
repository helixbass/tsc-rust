#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::{
    signature_has_rest_parameter, CheckMode, IterationUse, MinArgumentCountFlags,
    ResolveNameNameArg, TypeFacts, WideningKind,
};
use crate::{
    cast, concatenate, filter, find_ancestor, get_assignment_declaration_kind, get_check_flags,
    get_effective_type_annotation_node, get_element_or_property_access_name, get_jsdoc_type_tag,
    get_semantic_jsx_children, get_this_container, index_of_node, is_access_expression,
    is_const_type_reference, is_function_expression_or_arrow_function, is_identifier,
    is_import_call, is_in_js_file, is_jsdoc_type_tag, is_jsx_attribute, is_jsx_attribute_like,
    is_jsx_attributes, is_jsx_element, is_object_literal_method, is_property_assignment,
    is_property_declaration, is_property_signature, is_this_initialized_declaration, map, some,
    unescape_leading_underscores, AssignmentDeclarationKind, CheckFlags, ContextFlags, Debug_,
    Diagnostics, FunctionFlags, InferenceInfo, NodeFlags, Number, Signature, SignatureFlags,
    SignatureKind, StringOrRcNode, SymbolFlags, Ternary, TransientSymbolInterface, TypeMapper,
    TypeSystemPropertyName, UnionOrIntersectionTypeInterface, UnionReduction, __String,
    create_symbol_table, get_function_flags, get_object_flags, has_initializer,
    is_object_literal_expression, InferenceContext, Node, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags,
    TypeInterface,
};

impl TypeChecker {
    pub(super) fn get_contextual_type_for_assignment_declaration(
        &self,
        binary_expression: &Node, /*BinaryExpression*/
    ) -> Option<Rc<Type>> {
        let kind = get_assignment_declaration_kind(binary_expression);
        let binary_expression_as_binary_expression = binary_expression.as_binary_expression();
        match kind {
            AssignmentDeclarationKind::None | AssignmentDeclarationKind::ThisProperty => {
                let lhs_symbol =
                    self.get_symbol_for_expression(&binary_expression_as_binary_expression.left);
                let decl = lhs_symbol
                    .as_ref()
                    .and_then(|lhs_symbol| lhs_symbol.maybe_value_declaration());
                if let Some(decl) = decl
                    .as_ref()
                    .filter(|decl| is_property_declaration(decl) || is_property_signature(decl))
                {
                    let overall_annotation = get_effective_type_annotation_node(decl);
                    return if let Some(overall_annotation) = overall_annotation.as_ref() {
                        Some(
                            self.instantiate_type(
                                &self.get_type_from_type_node_(overall_annotation),
                                (*self.get_symbol_links(lhs_symbol.as_ref().unwrap()))
                                    .borrow()
                                    .mapper
                                    .clone()
                                    .as_ref(),
                            ),
                        )
                    } else {
                        None
                    }
                    .or_else(|| {
                        if decl.as_has_initializer().maybe_initializer().is_some() {
                            Some(self.get_type_of_expression(
                                &binary_expression_as_binary_expression.left,
                            ))
                        } else {
                            None
                        }
                    });
                }
                if kind == AssignmentDeclarationKind::None {
                    return Some(
                        self.get_type_of_expression(&binary_expression_as_binary_expression.left),
                    );
                }
                self.get_contextual_type_for_this_property_assignment(binary_expression)
            }
            AssignmentDeclarationKind::Property => {
                if self.is_possibly_aliased_this_property(binary_expression, Some(kind)) {
                    self.get_contextual_type_for_this_property_assignment(binary_expression)
                } else if binary_expression_as_binary_expression
                    .left
                    .maybe_symbol()
                    .is_none()
                {
                    Some(self.get_type_of_expression(&binary_expression_as_binary_expression.left))
                } else {
                    let decl = binary_expression_as_binary_expression
                        .left
                        .symbol()
                        .maybe_value_declaration()?;
                    let lhs = cast(
                        Some(&*binary_expression_as_binary_expression.left),
                        |node: &&Node| is_access_expression(node),
                    );
                    let overall_annotation = get_effective_type_annotation_node(&decl);
                    if let Some(overall_annotation) = overall_annotation.as_ref() {
                        return Some(self.get_type_from_type_node_(overall_annotation));
                    } else if is_identifier(&lhs.as_has_expression().expression()) {
                        let id = &lhs.as_has_expression().expression();
                        let parent_symbol = self.resolve_name_(
                            Some(&**id),
                            &id.as_identifier().escaped_text,
                            SymbolFlags::Value,
                            None,
                            Some(id.as_identifier().escaped_text.clone()),
                            true,
                            None,
                        );
                        if let Some(parent_symbol) = parent_symbol.as_ref() {
                            let annotated = parent_symbol
                                .maybe_value_declaration()
                                .as_ref()
                                .and_then(|parent_symbol_value_declaration| {
                                    get_effective_type_annotation_node(
                                        parent_symbol_value_declaration,
                                    )
                                });
                            if let Some(annotated) = annotated.as_ref() {
                                let name_str = get_element_or_property_access_name(lhs);
                                if let Some(name_str) = name_str.as_ref() {
                                    return self.get_type_of_property_of_contextual_type(
                                        &self.get_type_from_type_node_(annotated),
                                        name_str,
                                    );
                                }
                            }
                            return None;
                        }
                    }
                    if is_in_js_file(Some(&*decl)) {
                        None
                    } else {
                        Some(
                            self.get_type_of_expression(
                                &binary_expression_as_binary_expression.left,
                            ),
                        )
                    }
                }
            }
            AssignmentDeclarationKind::ExportsProperty
            | AssignmentDeclarationKind::Prototype
            | AssignmentDeclarationKind::PrototypeProperty => {
                let mut value_declaration = binary_expression_as_binary_expression
                    .left
                    .maybe_symbol()
                    .as_ref()
                    .and_then(|binary_expression_left_symbol| {
                        binary_expression_left_symbol.maybe_value_declaration()
                    });
                value_declaration = value_declaration.or_else(|| {
                    binary_expression_as_binary_expression
                        .maybe_symbol()
                        .as_ref()
                        .and_then(|binary_expression_symbol| {
                            binary_expression_symbol.maybe_value_declaration()
                        })
                });
                let annotated = value_declaration.as_ref().and_then(|value_declaration| {
                    get_effective_type_annotation_node(value_declaration)
                });
                annotated
                    .as_ref()
                    .map(|annotated| self.get_type_from_type_node_(annotated))
            }
            AssignmentDeclarationKind::ModuleExports => {
                let mut value_declaration: Option<Rc<Node>> = None;
                value_declaration = value_declaration.or_else(|| {
                    binary_expression_as_binary_expression
                        .maybe_symbol()
                        .as_ref()
                        .and_then(|binary_expression_symbol| {
                            binary_expression_symbol.maybe_value_declaration()
                        })
                });
                let annotated = value_declaration.as_ref().and_then(|value_declaration| {
                    get_effective_type_annotation_node(value_declaration)
                });
                annotated
                    .as_ref()
                    .map(|annotated| self.get_type_from_type_node_(annotated))
            }
            AssignmentDeclarationKind::ObjectDefinePropertyValue
            | AssignmentDeclarationKind::ObjectDefinePropertyExports
            | AssignmentDeclarationKind::ObjectDefinePrototypeProperty => {
                Debug_.fail(Some("Does not apply"));
            } // _ => Debug_.assert_never(kind, None)
        }
    }

    pub(super) fn is_possibly_aliased_this_property(
        &self,
        declaration: &Node, /*BinaryExpression*/
        kind: Option<AssignmentDeclarationKind>,
    ) -> bool {
        let kind = kind.unwrap_or_else(|| get_assignment_declaration_kind(declaration));
        if kind == AssignmentDeclarationKind::ThisProperty {
            return true;
        }
        let declaration_as_binary_expression = declaration.as_binary_expression();
        if !is_in_js_file(Some(declaration))
            || kind != AssignmentDeclarationKind::Property
            || !is_identifier(
                &declaration_as_binary_expression
                    .left
                    .as_has_expression()
                    .expression(),
            )
        {
            return false;
        }
        let name = &declaration_as_binary_expression
            .left
            .as_has_expression()
            .expression()
            .as_identifier()
            .escaped_text
            .clone();
        let symbol = self.resolve_name_(
            Some(&*declaration_as_binary_expression.left),
            &name,
            SymbolFlags::Value,
            None,
            Option::<Rc<Node>>::None,
            true,
            Some(true),
        );
        is_this_initialized_declaration(
            symbol
                .as_ref()
                .and_then(|symbol| symbol.maybe_value_declaration()),
        )
    }

    pub(super) fn get_contextual_type_for_this_property_assignment(
        &self,
        binary_expression: &Node, /*BinaryExpression*/
    ) -> Option<Rc<Type>> {
        let binary_expression_symbol = binary_expression.maybe_symbol();
        let binary_expression_as_binary_expression = binary_expression.as_binary_expression();
        if binary_expression_symbol.is_none() {
            return Some(self.get_type_of_expression(&binary_expression_as_binary_expression.left));
        }
        let binary_expression_symbol = binary_expression_symbol.unwrap();
        if let Some(binary_expression_symbol_value_declaration) =
            binary_expression_symbol.maybe_value_declaration()
        {
            let annotated =
                get_effective_type_annotation_node(&binary_expression_symbol_value_declaration);
            if let Some(annotated) = annotated.as_ref() {
                let type_ = self.get_type_from_type_node_(annotated);
                // if (type) {
                return Some(type_);
                // }
            }
        }
        let this_access = cast(
            Some(&*binary_expression_as_binary_expression.left),
            |node: &&Node| is_access_expression(node),
        );
        if !is_object_literal_method(&get_this_container(
            &this_access.as_has_expression().expression(),
            false,
        )) {
            return None;
        }
        let this_type = self.check_this_expression(&this_access.as_has_expression().expression());
        let name_str = get_element_or_property_access_name(&this_access);
        name_str
            .as_ref()
            .and_then(|name_str| self.get_type_of_property_of_contextual_type(&this_type, name_str))
    }

    pub(super) fn is_circular_mapped_property(&self, symbol: &Symbol) -> bool {
        get_check_flags(symbol).intersects(CheckFlags::Mapped)
            && (*symbol.as_mapped_symbol().symbol_links())
                .borrow()
                .type_
                .is_none()
            && self.find_resolution_cycle_start_index(
                &symbol.symbol_wrapper().into(),
                TypeSystemPropertyName::Type,
            ) >= 0
    }

    pub(super) fn get_type_of_property_of_contextual_type(
        &self,
        type_: &Type,
        name: &__String,
    ) -> Option<Rc<Type>> {
        self.map_type(
            type_,
            &mut |t| {
                if self.is_generic_mapped_type(t) {
                    let constraint = self.get_constraint_type_from_mapped_type(t);
                    let constraint_of_constraint = self
                        .get_base_constraint_of_type(&constraint)
                        .unwrap_or(constraint);
                    let property_name_type =
                        self.get_string_literal_type(&unescape_leading_underscores(name));
                    if self.is_type_assignable_to(&property_name_type, &constraint_of_constraint) {
                        return Some(self.substitute_indexed_mapped_type(t, &property_name_type));
                    }
                } else if t.flags().intersects(TypeFlags::StructuredType) {
                    let prop = self.get_property_of_type_(t, name, None);
                    if let Some(prop) = prop.as_ref() {
                        return if self.is_circular_mapped_property(prop) {
                            None
                        } else {
                            Some(self.get_type_of_symbol(prop))
                        };
                    }
                    if self.is_tuple_type(t) {
                        let rest_type = self.get_rest_type_of_tuple_type(t);
                        if rest_type.is_some()
                            && self.is_numeric_literal_name(name)
                            && (&**name).parse::<f64>().unwrap() >= 0.0
                        {
                            return rest_type;
                        }
                    }
                    return self
                        .find_applicable_index_info(
                            &self.get_index_infos_of_structured_type(t),
                            &self.get_string_literal_type(&unescape_leading_underscores(name)),
                        )
                        .map(|index_info| index_info.type_.clone());
                }
                None
            },
            Some(true),
        )
    }

    pub(super) fn get_contextual_type_for_object_literal_method(
        &self,
        node: &Node, /*MethodDeclaration*/
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        Debug_.assert(is_object_literal_method(node), None);
        if node.flags().intersects(NodeFlags::InWithStatement) {
            return None;
        }
        self.get_contextual_type_for_object_literal_element_(node, context_flags)
    }

    pub(super) fn get_contextual_type_for_object_literal_element_(
        &self,
        element: &Node, /*ObjectLiteralElementLike*/
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let object_literal = element.parent();
        let property_assignment_type = if is_property_assignment(element) {
            self.get_contextual_type_for_variable_like_declaration(element)
        } else {
            None
        };
        if property_assignment_type.is_some() {
            return property_assignment_type;
        }
        let type_ = self.get_apparent_type_of_contextual_type(&object_literal, context_flags);
        if let Some(type_) = type_.as_ref() {
            if self.has_bindable_name(element) {
                return self.get_type_of_property_of_contextual_type(
                    type_,
                    self.get_symbol_of_node(element).unwrap().escaped_name(),
                );
            }
            if let Some(element_name) = element.as_named_declaration().maybe_name() {
                let name_type = self.get_literal_type_from_property_name(&element_name);
                return self.map_type(
                    type_,
                    &mut |t: &Type| {
                        self.find_applicable_index_info(
                            &self.get_index_infos_of_structured_type(t),
                            &name_type,
                        )
                        .map(|index_info| index_info.type_.clone())
                    },
                    Some(true),
                );
            }
        }
        None
    }

    pub(super) fn get_contextual_type_for_element_expression<TArrayContextualType: Borrow<Type>>(
        &self,
        array_contextual_type: Option<TArrayContextualType>,
        index: usize,
    ) -> Option<Rc<Type>> {
        let array_contextual_type = array_contextual_type?;
        let array_contextual_type = array_contextual_type.borrow();
        self.get_type_of_property_of_contextual_type(
            array_contextual_type,
            &__String::new(index.to_string()),
        )
        .or_else(|| {
            self.map_type(
                array_contextual_type,
                &mut |t: &Type| {
                    Some(self.get_iterated_type_or_element_type(
                        IterationUse::Element,
                        t,
                        &self.undefined_type(),
                        Option::<&Node>::None,
                        false,
                    ))
                },
                Some(true),
            )
        })
    }

    pub(super) fn get_contextual_type_for_conditional_operand(
        &self,
        node: &Node, /*Expression*/
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let conditional = node.parent();
        let conditional_as_conditional_expression = conditional.as_conditional_expression();
        if ptr::eq(node, &*conditional_as_conditional_expression.when_true)
            || ptr::eq(node, &*conditional_as_conditional_expression.when_false)
        {
            self.get_contextual_type_(&conditional, context_flags)
        } else {
            None
        }
    }

    pub(super) fn get_contextual_type_for_child_jsx_expression(
        &self,
        node: &Node,  /*JsxElement*/
        child: &Node, /*JsxChild*/
    ) -> Option<Rc<Type>> {
        let node_as_jsx_element = node.as_jsx_element();
        let attributes_type = self.get_apparent_type_of_contextual_type(
            &node_as_jsx_element
                .opening_element
                .as_jsx_opening_element()
                .tag_name,
            None,
        );
        let jsx_children_property_name =
            self.get_jsx_element_children_property_name(&self.get_jsx_namespace_at(Some(node)));
        if !(matches!(
            attributes_type.as_ref(),
            Some(attributes_type) if !self.is_type_any(Some(&**attributes_type))
        ) && matches!(
            jsx_children_property_name.as_ref(),
            Some(jsx_children_property_name) if !jsx_children_property_name.is_empty()
        )) {
            return None;
        }
        let attributes_type = attributes_type.unwrap();
        let jsx_children_property_name = jsx_children_property_name.unwrap();
        let real_children = get_semantic_jsx_children(&node_as_jsx_element.children);
        let child_index = real_children
            .iter()
            .position(|real_child| ptr::eq(&**real_child, child))
            .unwrap();
        let child_field_type = self
            .get_type_of_property_of_contextual_type(&attributes_type, &jsx_children_property_name);
        child_field_type.as_ref().and_then(|child_field_type| {
            if real_children.len() == 1 {
                Some(child_field_type.clone())
            } else {
                self.map_type(
                    child_field_type,
                    &mut |t: &Type| {
                        if self.is_array_like_type(t) {
                            Some(self.get_indexed_access_type(
                                t,
                                &self.get_number_literal_type(Number::new(child_index as f64)),
                                None,
                                Option::<&Node>::None,
                                Option::<&Symbol>::None,
                                None,
                            ))
                        } else {
                            Some(t.type_wrapper())
                        }
                    },
                    Some(true),
                )
            }
        })
    }

    pub(super) fn get_contextual_type_for_jsx_expression(
        &self,
        node: &Node, /*JsxExpression*/
    ) -> Option<Rc<Type>> {
        let expr_parent = node.parent();
        if is_jsx_attribute_like(&expr_parent) {
            self.get_contextual_type_(node, None)
        } else if is_jsx_element(&expr_parent) {
            self.get_contextual_type_for_child_jsx_expression(&expr_parent, node)
        } else {
            None
        }
    }

    pub(super) fn get_contextual_type_for_jsx_attribute_(
        &self,
        attribute: &Node, /*JsxAttribute | JsxSpreadAttribute*/
    ) -> Option<Rc<Type>> {
        if is_jsx_attribute(attribute) {
            let attributes_type =
                self.get_apparent_type_of_contextual_type(&attribute.parent(), None)?;
            if self.is_type_any(Some(&*attributes_type)) {
                return None;
            }
            self.get_type_of_property_of_contextual_type(
                &attributes_type,
                &attribute
                    .as_jsx_attribute()
                    .name
                    .as_identifier()
                    .escaped_text,
            )
        } else {
            self.get_contextual_type_(&attribute.parent(), None)
        }
    }

    pub(super) fn is_possibly_discriminant_value(&self, node: &Node /*Expression*/) -> bool {
        match node.kind() {
            SyntaxKind::StringLiteral
            | SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::TrueKeyword
            | SyntaxKind::FalseKeyword
            | SyntaxKind::NullKeyword
            | SyntaxKind::Identifier
            | SyntaxKind::UndefinedKeyword => true,
            SyntaxKind::PropertyAccessExpression | SyntaxKind::ParenthesizedExpression => {
                self.is_possibly_discriminant_value(&node.as_has_expression().expression())
            }
            SyntaxKind::JsxExpression => match node.as_jsx_expression().expression.as_ref() {
                None => true,
                Some(node_expression) => self.is_possibly_discriminant_value(node_expression),
            },
            _ => false,
        }
    }

    pub(super) fn discriminate_contextual_type_by_object_members(
        &self,
        node: &Node,            /*ObjectLiteralExpression*/
        contextual_type: &Type, /*UnionType*/
    ) -> Rc<Type> {
        self.get_matching_union_constituent_for_object_literal(
            contextual_type,
            node,
        ).unwrap_or_else(|| {
            self.discriminate_type_by_discriminable_items(
                contextual_type,
                &concatenate(
                    map(
                        &filter(
                            &node.as_object_literal_expression().properties,
                            |p: &Rc<Node>| {
                                p.maybe_symbol().is_some() &&
                                    p.kind() == SyntaxKind::PropertyAssignment &&
                                    self.is_possibly_discriminant_value(&p.as_has_initializer().maybe_initializer().unwrap()) &&
                                    self.is_discriminant_property(Some(contextual_type), p.symbol().escaped_name())
                            }
                        ),
                        |prop: &Rc<Node>, _| {
                            let type_checker = self.rc_wrapper();
                            let prop_clone = prop.clone();
                            (
                                Box::new(move || {
                                    type_checker.get_context_free_type_of_expression(&prop_clone.as_has_initializer().maybe_initializer().unwrap())
                                }) as Box<dyn Fn() -> Rc<Type>>,
                                prop.symbol().escaped_name().clone(),
                            )
                        }
                    ),
                    map(
                        &filter(
                            &self.get_properties_of_type(contextual_type),
                            |s: &Rc<Symbol>| {
                                s.flags().intersects(SymbolFlags::Optional) &&
                                    matches!(
                                        node.maybe_symbol().as_ref(),
                                        Some(node_symbol) if matches!(
                                            node_symbol.maybe_members().clone(),
                                            Some(node_symbol_members) if (*node_symbol_members).borrow().contains_key(s.escaped_name())
                                        )
                                    ) &&
                                    self.is_discriminant_property(
                                        Some(contextual_type),
                                        s.escaped_name()
                                    )
                            }
                        ),
                        |s: &Rc<Symbol>, _| {
                            let type_checker = self.rc_wrapper();
                            (
                                Box::new(move || {
                                    type_checker.undefined_type()
                                }) as Box<dyn Fn() -> Rc<Type>>,
                                s.escaped_name().clone(),
                            )
                        }
                    ),
                ),
                |source: &Type, target: &Type| self.is_type_assignable_to(source, target),
                Some(contextual_type),
                None,
            ).unwrap()
        })
    }

    pub(super) fn discriminate_contextual_type_by_jsx_attributes(
        &self,
        node: &Node,            /*JsxAttributes*/
        contextual_type: &Type, /*UnionType*/
    ) -> Rc<Type> {
        self.discriminate_type_by_discriminable_items(
            contextual_type,
            &concatenate(
                map(
                    &filter(
                        &node.as_jsx_attributes().properties,
                        |p: &Rc<Node>| {
                            p.maybe_symbol().is_some() &&
                                p.kind() == SyntaxKind::JsxAttribute &&
                                self.is_discriminant_property(Some(contextual_type), p.symbol().escaped_name()) &&
                                match p.as_jsx_attribute().initializer.as_ref() {
                                    None => true,
                                    Some(p_initializer) => self.is_possibly_discriminant_value(p_initializer)
                                }
                        }
                    ),
                    |prop: &Rc<Node>, _| {
                        let type_checker = self.rc_wrapper();
                        let prop_clone = prop.clone();
                        (
                            Box::new(move || {
                                if prop_clone.kind() != SyntaxKind::JsxAttribute {
                                    return type_checker.true_type();
                                }
                                let prop_as_jsx_attribute = prop_clone.as_jsx_attribute();
                                let prop_initializer = prop_as_jsx_attribute.initializer.as_ref();
                                match prop_initializer {
                                    None => type_checker.true_type(),
                                    Some(prop_initializer) =>
                                        type_checker.get_context_free_type_of_expression(prop_initializer)
                                }
                            }) as Box<dyn Fn() -> Rc<Type>>,
                            prop.symbol().escaped_name().clone(),
                        )
                    }
                ),
                map(
                    &filter(
                        &self.get_properties_of_type(contextual_type),
                        |s: &Rc<Symbol>| {
                            s.flags().intersects(SymbolFlags::Optional) &&
                                matches!(
                                    node.maybe_symbol().as_ref(),
                                    Some(node_symbol) if matches!(
                                        node_symbol.maybe_members().clone(),
                                        Some(node_symbol_members) if (*node_symbol_members).borrow().contains_key(s.escaped_name())
                                    )
                                ) &&
                                self.is_discriminant_property(
                                    Some(contextual_type),
                                    s.escaped_name()
                                )
                        }
                    ),
                    |s: &Rc<Symbol>, _| {
                        let type_checker = self.rc_wrapper();
                        (
                            Box::new(move || {
                                type_checker.undefined_type()
                            }) as Box<dyn Fn() -> Rc<Type>>,
                            s.escaped_name().clone(),
                        )
                    }
                ),
            ),
            |source: &Type, target: &Type| self.is_type_assignable_to(source, target),
            Some(contextual_type),
            None,
        ).unwrap()
    }

    pub(super) fn get_apparent_type_of_contextual_type(
        &self,
        node: &Node, /*Expression | MethodDeclaration*/
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let contextual_type = if is_object_literal_method(node) {
            self.get_contextual_type_for_object_literal_method(node, context_flags)
        } else {
            self.get_contextual_type_(node, context_flags)
        };
        let instantiated_type =
            self.instantiate_contextual_type(contextual_type, node, context_flags);
        if let Some(instantiated_type) = instantiated_type {
            if !(matches!(context_flags, Some(context_flags) if context_flags.intersects(ContextFlags::NoConstraints))
                && instantiated_type
                    .flags()
                    .intersects(TypeFlags::TypeVariable))
            {
                let apparent_type = self
                    .map_type(
                        &instantiated_type,
                        &mut |type_| Some(self.get_apparent_type(type_)),
                        Some(true),
                    )
                    .unwrap();
                return if apparent_type.flags().intersects(TypeFlags::Union)
                    && is_object_literal_expression(node)
                {
                    Some(self.discriminate_contextual_type_by_object_members(node, &apparent_type))
                } else if apparent_type.flags().intersects(TypeFlags::Union)
                    && is_jsx_attributes(node)
                {
                    Some(self.discriminate_contextual_type_by_jsx_attributes(node, &apparent_type))
                } else {
                    Some(apparent_type)
                };
            }
        }
        None
    }

    pub(super) fn instantiate_contextual_type<TContextualType: Borrow<Type>>(
        &self,
        contextual_type: Option<TContextualType>,
        node: &Node,
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        let contextual_type =
            contextual_type.map(|contextual_type| contextual_type.borrow().type_wrapper());
        if let Some(contextual_type) = contextual_type.as_ref().filter(|contextual_type| {
            self.maybe_type_of_kind(contextual_type, TypeFlags::Instantiable)
        }) {
            let inference_context = self.get_inference_context(node);
            if let Some(inference_context) =
                inference_context.as_ref().filter(|inference_context| {
                    some(
                        Some(&inference_context.inferences),
                        Some(|inference: &Rc<InferenceInfo>| {
                            self.has_inference_candidates(inference)
                        }),
                    )
                })
            {
                if matches!(
                    context_flags,
                    Some(context_flags) if context_flags.intersects(ContextFlags::Signature)
                ) {
                    return Some(self.instantiate_instantiable_types(
                        contextual_type,
                        &inference_context.non_fixing_mapper(),
                    ));
                }
                if let Some(inference_context_return_mapper) =
                    inference_context.return_mapper.as_ref()
                {
                    return Some(self.instantiate_instantiable_types(
                        contextual_type,
                        inference_context_return_mapper,
                    ));
                }
            }
        }
        contextual_type
    }

    pub(super) fn instantiate_instantiable_types(
        &self,
        type_: &Type,
        mapper: &TypeMapper,
    ) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Instantiable) {
            return self.instantiate_type(type_, Some(mapper));
        }
        if type_.flags().intersects(TypeFlags::Union) {
            return self.get_union_type(
                map(type_.as_union_type().types(), |t: &Rc<Type>, _| {
                    self.instantiate_instantiable_types(t, mapper)
                }),
                Some(UnionReduction::None),
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            );
        }
        if type_.flags().intersects(TypeFlags::Intersection) {
            return self.get_intersection_type(
                &map(type_.as_intersection_type().types(), |t: &Rc<Type>, _| {
                    self.instantiate_instantiable_types(t, mapper)
                }),
                Option::<&Symbol>::None,
                None,
            );
        }
        type_.type_wrapper()
    }

    pub(super) fn get_contextual_type_(
        &self,
        node: &Node, /*Expression*/
        context_flags: Option<ContextFlags>,
    ) -> Option<Rc<Type>> {
        if node.flags().intersects(NodeFlags::InWithStatement) {
            return None;
        }
        if let Some(node_contextual_type) = node.maybe_contextual_type().clone() {
            return Some(node_contextual_type);
        }
        let parent = node.parent();
        match parent.kind() {
            SyntaxKind::VariableDeclaration
            | SyntaxKind::Parameter
            | SyntaxKind::PropertyDeclaration
            | SyntaxKind::PropertySignature
            | SyntaxKind::BindingElement => {
                self.get_contextual_type_for_initializer_expression(node, context_flags)
            }
            SyntaxKind::ArrowFunction | SyntaxKind::ReturnStatement => {
                self.get_contextual_type_for_return_expression(node)
            }
            SyntaxKind::YieldExpression => self.get_contextual_type_for_yield_operand(&parent),
            SyntaxKind::AwaitExpression => {
                self.get_contextual_type_for_await_operand(&parent, context_flags)
            }
            SyntaxKind::CallExpression | SyntaxKind::NewExpression => {
                self.get_contextual_type_for_argument(&parent, node)
            }
            SyntaxKind::TypeAssertionExpression | SyntaxKind::AsExpression => {
                let parent_type = parent.as_has_type().maybe_type().unwrap();
                if is_const_type_reference(&parent_type) {
                    self.try_find_when_const_type_reference(&parent)
                } else {
                    Some(self.get_type_from_type_node_(&parent_type))
                }
            }
            SyntaxKind::BinaryExpression => {
                self.get_contextual_type_for_binary_operand(node, context_flags)
            }
            SyntaxKind::PropertyAssignment | SyntaxKind::ShorthandPropertyAssignment => {
                self.get_contextual_type_for_object_literal_element_(&parent, context_flags)
            }
            SyntaxKind::SpreadAssignment => {
                self.get_contextual_type_(&parent.parent(), context_flags)
            }
            SyntaxKind::ArrayLiteralExpression => {
                let array_literal = &parent;
                let type_ = self.get_apparent_type_of_contextual_type(array_literal, context_flags);
                self.get_contextual_type_for_element_expression(
                    type_,
                    index_of_node(&array_literal.as_array_literal_expression().elements, node)
                        .try_into()
                        .unwrap(),
                )
            }
            SyntaxKind::ConditionalExpression => {
                self.get_contextual_type_for_conditional_operand(node, context_flags)
            }
            SyntaxKind::TemplateSpan => {
                Debug_.assert(
                    parent.parent().kind() == SyntaxKind::TemplateExpression,
                    None,
                );
                self.get_contextual_type_for_substitution_expression(&parent.parent(), node)
            }
            SyntaxKind::ParenthesizedExpression => {
                let tag = if is_in_js_file(Some(&*parent)) {
                    get_jsdoc_type_tag(&parent)
                } else {
                    None
                };
                match tag.as_ref() {
                    None => self.get_contextual_type_(&parent, context_flags),
                    Some(tag) => {
                        if is_jsdoc_type_tag(tag)
                            && is_const_type_reference(
                                &tag.as_base_jsdoc_type_like_tag()
                                    .type_expression
                                    .as_ref()
                                    .unwrap()
                                    .as_jsdoc_type_expression()
                                    .type_,
                            )
                        {
                            self.try_find_when_const_type_reference(&parent)
                        } else {
                            Some(
                                self.get_type_from_type_node_(
                                    &tag.as_base_jsdoc_type_like_tag()
                                        .type_expression
                                        .as_ref()
                                        .unwrap()
                                        .as_jsdoc_type_expression()
                                        .type_,
                                ),
                            )
                        }
                    }
                }
            }
            SyntaxKind::NonNullExpression => self.get_contextual_type_(&parent, context_flags),
            SyntaxKind::JsxExpression => self.get_contextual_type_for_jsx_expression(&parent),
            SyntaxKind::JsxAttribute | SyntaxKind::JsxSpreadAttribute => {
                self.get_contextual_type_for_jsx_attribute_(&parent)
            }
            SyntaxKind::JsxOpeningElement | SyntaxKind::JsxSelfClosingElement => {
                Some(self.get_contextual_jsx_element_attributes_type(&parent, context_flags))
            }
            _ => None,
        }
    }

    pub(super) fn try_find_when_const_type_reference(
        &self,
        node: &Node, /*Expression*/
    ) -> Option<Rc<Type>> {
        self.get_contextual_type_(node, None)
    }

    pub(super) fn get_inference_context(&self, node: &Node) -> Option<Rc<InferenceContext>> {
        let ancestor = find_ancestor(Some(node), |n: &Node| n.maybe_inference_context().is_some());
        ancestor.map(|ancestor| ancestor.maybe_inference_context().clone().unwrap())
    }

    pub(super) fn get_contextual_jsx_element_attributes_type(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
        context_flags: Option<ContextFlags>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_effective_first_argument_for_jsx_signature(
        &self,
        signature: &Signature,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_intersected_signatures(
        &self,
        signatures: &[Rc<Signature>],
    ) -> Option<Rc<Signature>> {
        unimplemented!()
    }

    pub(super) fn get_contextual_call_signature(
        &self,
        type_: &Type,
        node: &Node, /*SignatureDeclaration*/
    ) -> Option<Rc<Signature>> {
        let signatures = self.get_signatures_of_type(type_, SignatureKind::Call);
        let applicable_by_arity = filter(&signatures, |s| !self.is_arity_smaller(s, node));
        if applicable_by_arity.len() == 1 {
            Some(applicable_by_arity[0].clone())
        } else {
            self.get_intersected_signatures(&applicable_by_arity)
        }
    }

    pub(super) fn is_arity_smaller(
        &self,
        signature: &Signature,
        target: &Node, /*SignatureDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_contextual_signature_for_function_like_declaration(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> Option<Rc<Signature>> {
        if is_function_expression_or_arrow_function(node) || is_object_literal_method(node) {
            self.get_contextual_signature(node)
        } else {
            None
        }
    }

    pub(super) fn get_contextual_signature(
        &self,
        node: &Node, /*FunctionExpression | ArrowFunction | MethodDeclaration*/
    ) -> Option<Rc<Signature>> {
        Debug_.assert(
            node.kind() != SyntaxKind::MethodDeclaration || is_object_literal_method(node),
            None,
        );
        let type_tag_signature = self.get_signature_of_type_tag(node);
        if type_tag_signature.is_some() {
            return type_tag_signature;
        }
        let type_ = self.get_apparent_type_of_contextual_type(node, Some(ContextFlags::Signature));
        if type_.is_none() {
            return None;
        }
        let type_ = type_.unwrap();
        if !type_.flags().intersects(TypeFlags::Union) {
            return self.get_contextual_call_signature(&type_, node);
        }
        let mut signature_list: Option<Vec<Rc<Signature>>> = None;
        let types = type_.as_union_or_intersection_type_interface().types();
        for current in types {
            let signature = self.get_contextual_call_signature(current, node);
            if let Some(signature) = signature {
                match signature_list.as_mut() {
                    None => {
                        signature_list = Some(vec![signature]);
                    }
                    Some(signature_list) => {
                        if self.compare_signatures_identical(
                            signature_list[0].clone(),
                            signature.clone(),
                            false,
                            true,
                            true,
                            |a, b| self.compare_types_identical(a, b),
                        ) == Ternary::False
                        {
                            return None;
                        } else {
                            signature_list.push(signature);
                        }
                    }
                }
            }
        }

        signature_list.map(|signature_list| {
            if signature_list.len() == 1 {
                signature_list[0].clone()
            } else {
                Rc::new(self.create_union_signature(&signature_list[0].clone(), signature_list))
            }
        })
    }

    pub(super) fn has_default_value(
        &self,
        node: &Node, /*BindingElement | Expression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_array_literal(
        &self,
        node: &Node, /*ArrayLiteralExpression*/
        check_mode: Option<CheckMode>,
        force_tuple: Option<bool>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_numeric_literal_name(&self, name: &str) -> bool {
        unimplemented!()
    }

    pub(super) fn check_computed_property_name(
        &self,
        node: &Node, /*ComputedPropertyName*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_object_literal(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) -> Rc<Type> {
        let node_as_object_literal_expression = node.as_object_literal_expression();
        let mut properties_table = create_symbol_table(None);
        let mut properties_array: Vec<Rc<Symbol>> = vec![];

        let object_flags = self.fresh_object_literal_flag;

        for member_decl in &node_as_object_literal_expression.properties {
            let member = self.get_symbol_of_node(&**member_decl).unwrap();
            if member_decl.kind() == SyntaxKind::PropertyAssignment {
            } else {
                unimplemented!()
            }

            if false {
                unimplemented!()
            } else {
                properties_table.insert(member.escaped_name().clone(), member.clone());
            }
            properties_array.push(member);
        }

        let create_object_literal_type = || {
            let result = self.create_anonymous_type(
                Some(node.symbol()),
                Rc::new(RefCell::new(properties_table)),
                vec![],
                vec![],
                vec![], // TODO: this is wrong
            );
            result.set_object_flags(
                result.object_flags()
                    | object_flags
                    | ObjectFlags::ObjectLiteral
                    | ObjectFlags::ContainsObjectOrArrayLiteral,
            );
            result.into()
        };

        create_object_literal_type()
    }

    pub(super) fn is_valid_spread_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_hyphenated_jsx_name(&self, name: &str) -> bool {
        unimplemented!()
    }

    pub(super) fn check_jsx_attribute(
        &self,
        node: &Node, /*JsxAttribute*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_jsx_children(
        &self,
        node: &Node, /*JsxElement | JsxFragment*/
        check_mode: Option<CheckMode>,
    ) -> Vec<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_type<TLocation: Borrow<Node>>(
        &self,
        name: &__String,
        location: Option<TLocation>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_jsx_namespace_at<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
    ) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn get_jsx_element_children_property_name(
        &self,
        jsx_namespace: &Symbol,
    ) -> Option<__String> {
        unimplemented!()
    }

    pub(super) fn is_known_property(
        &self,
        target_type: &Type,
        name: &__String,
        is_comparing_jsx_attributes: bool,
    ) -> bool {
        if target_type.flags().intersects(TypeFlags::Object) {
            if self
                .get_property_of_object_type(target_type, name)
                .is_some()
                || false
            {
                return true;
            }
        } else if target_type
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
            && self.is_excess_property_check_target(target_type)
        {
            unimplemented!()
        }
        false
    }

    pub(super) fn is_excess_property_check_target(&self, type_: &Type) -> bool {
        (type_.flags().intersects(TypeFlags::Object)
            && !(get_object_flags(type_)
                .intersects(ObjectFlags::ObjectLiteralPatternWithComputedProperties)))
            || type_.flags().intersects(TypeFlags::NonPrimitive)
            || (type_.flags().intersects(TypeFlags::Union) && unimplemented!())
            || (type_.flags().intersects(TypeFlags::Intersection) && unimplemented!())
    }

    pub(super) fn get_declaration_node_flags_from_symbol(&self, s: &Symbol) -> NodeFlags {
        unimplemented!()
    }

    pub(super) fn is_prototype_property(&self, symbol: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn check_non_null_expression(
        &self,
        node: &Node, /*Expression | QualifiedName*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_nullable_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_non_nullable_type_if_needed(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_non_null_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn lookup_symbol_for_private_identifier_declaration(
        &self,
        prop_name: &__String,
        location: &Node,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_symbol_for_private_identifier_expression(
        &self,
        priv_id: &Node, /*PrivateIdentifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_private_identifier_property_of_type_(
        &self,
        left_type: &Type,
        lexically_scoped_identifier: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn is_this_property_access_in_constructor(
        &self,
        node: &Node, /*ElementAccessExpression | PropertyAccessExpression | QualifiedName*/
        prop: &Symbol,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_unchecked_js_suggestion<TNode: Borrow<Node>, TSuggestion: Borrow<Symbol>>(
        &self,
        node: Option<TNode>,
        suggestion: Option<TSuggestion>,
        exclude_classes: bool,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_in_property_initializer_or_class_static_block(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn type_has_static_property(
        &self,
        prop_name: &__String,
        containing_type: &Type,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_suggested_lib_for_nonexistent_name(
        &self,
        name: ResolveNameNameArg,
    ) -> Option<String> {
        // unimplemented!()
        None
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_jsx_attribute<
        TName: Into<StringOrRcNode>,
    >(
        &self,
        name: TName, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_suggestion_for_nonexistent_property<TName: Into<StringOrRcNode>>(
        &self,
        name: TName, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_symbol_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        outer_name: &__String,
        meaning: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        // unimplemented!()
        None
    }

    pub(super) fn get_suggestion_for_nonexistent_symbol_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        outer_name: &__String,
        meaning: SymbolFlags,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_module(
        &self,
        name: &Node, /*Identifier*/
        target_module: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_suggestion_for_nonexistent_index_signature(
        &self,
        object_type: &Type,
        name: &Node, /*ElementAccessExpression*/
        keyed_type: &Type,
    ) -> Option<String> {
        unimplemented!()
    }

    pub(super) fn get_suggested_type_for_nonexistent_string_literal_type(
        &self,
        source: &Type, /*StringLiteralType*/
        target: &Type, /*UnionType*/
    ) -> Option<Rc<Type /*StringLiteralType*/>> {
        unimplemented!()
    }

    pub(super) fn mark_property_as_referenced<TNodeForCheckWriteOnly: Borrow<Node>>(
        &self,
        prop: &Symbol,
        node_for_check_write_only: Option<TNodeForCheckWriteOnly>,
        is_self_type_access: bool,
    ) {
        unimplemented!()
    }

    pub(super) fn is_self_type_access<TParent: Borrow<Symbol>>(
        &self,
        name: &Node, /*Expression | QualifiedName*/
        parent: Option<TParent>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_valid_property_access_(
        &self,
        node: &Node, /*PropertyAccessExpression | QualifiedName | ImportTypeNode*/
        property_name: &__String,
    ) -> bool {
        unimplemented!()
    }

    pub fn is_valid_property_access_for_completions_(
        &self,
        node_in: &Node, /*PropertyAccessExpression | ImportTypeNode | QualifiedName*/
        type_: &Type,
        property: &Symbol,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn accepts_void(&self, t: &Type) -> bool {
        t.flags().intersects(TypeFlags::Void)
    }

    pub(super) fn get_single_call_signature(&self, type_: &Type) -> Option<Rc<Signature>> {
        unimplemented!()
    }

    pub(super) fn get_single_call_or_construct_signature(
        &self,
        type_: &Type,
    ) -> Option<Rc<Signature>> {
        unimplemented!()
    }

    pub(super) fn instantiate_signature_in_context_of<
        TCompareTypes: FnMut(&Type, &Type, Option<bool>) -> Ternary,
    >(
        &self,
        signature: &Signature,
        contextual_signature: &Signature,
        inference_context: Option<&InferenceContext>,
        compare_types: Option<&mut TCompareTypes>,
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn get_resolved_signature_(
        &self,
        node: &Node, /*CallLikeExpression*/
        candidates_out_array: Option<&[Rc<Signature>]>,
        check_mode: Option<CheckMode>,
    ) -> Rc<Signature> {
        unimplemented!()
    }

    pub(super) fn get_spread_argument_type<TContext: Borrow<InferenceContext>>(
        &self,
        args: &[Rc<Node /*Expression*/>],
        index: usize,
        arg_count: usize,
        rest_type: &Type,
        context: Option<TContext>,
        check_mode: CheckMode,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_effective_call_arguments(
        &self,
        node: &Node, /*CallLikeExpression*/
    ) -> Vec<Rc<Node /*Expression*/>> {
        unimplemented!()
    }

    pub(super) fn is_js_constructor<TNode: Borrow<Node>>(&self, node: Option<TNode>) -> bool {
        unimplemented!()
    }

    pub(super) fn merge_js_symbols<TSource: Borrow<Symbol>>(
        &self,
        target: &Symbol,
        source: Option<TSource>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_assigned_class_symbol(
        &self,
        decl: &Node, /*Declaration*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_symbol_of_expando(
        &self,
        node: &Node,
        allow_declaration: bool,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_with_synthetic_default_only(
        &self,
        type_: &Type,
        symbol: &Symbol,
        original_symbol: &Symbol,
        module_specifier: &Node, /*Expression*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_type_with_synthetic_default_import_type(
        &self,
        type_: &Type,
        symbol: &Symbol,
        original_symbol: &Symbol,
        module_specifier: &Node, /*Expression*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_common_js_require(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn get_type_of_parameter(&self, symbol: &Symbol) -> Rc<Type> {
        let type_ = self.get_type_of_symbol(symbol);
        if self.strict_null_checks {
            let declaration = symbol.maybe_value_declaration();
            if matches!(declaration.as_ref(), Some(declaration) if has_initializer(&declaration)) {
                return self.get_optional_type_(&type_, None);
            }
        }
        type_
    }

    pub(super) fn get_tuple_element_label(
        &self,
        d: &Node, /*ParameterDeclaration | NamedTupleMember*/
    ) -> __String {
        unimplemented!()
    }

    pub(super) fn get_parameter_name_at_position<TOverrideRestType: Borrow<Type>>(
        &self,
        signature: &Signature,
        pos: usize,
        override_rest_type: Option<TOverrideRestType>,
    ) -> __String {
        unimplemented!()
    }

    pub(super) fn get_type_at_position(&self, signature: &Signature, pos: usize) -> Rc<Type> {
        self.try_get_type_at_position(signature, pos)
            .unwrap_or_else(|| self.any_type())
    }

    pub(super) fn try_get_type_at_position(
        &self,
        signature: &Signature,
        pos: usize,
    ) -> Option<Rc<Type>> {
        let param_count = signature.parameters().len()
            - if signature_has_rest_parameter(signature) {
                1
            } else {
                0
            };
        if pos < param_count {
            return Some(self.get_type_of_parameter(&signature.parameters()[pos]));
        }
        if signature_has_rest_parameter(signature) {
            let rest_type = self.get_type_of_symbol(&signature.parameters()[param_count]);
            let index = pos - param_count;
            unimplemented!()
        }
        None
    }

    pub(super) fn get_rest_type_at_position(&self, signature: &Signature, pos: usize) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_parameter_count(&self, signature: &Signature) -> usize {
        let length = signature.parameters().len();
        if signature_has_rest_parameter(signature) {
            let rest_type = self.get_type_of_symbol(&signature.parameters()[length - 1]);
            if self.is_tuple_type(&rest_type) {
                unimplemented!()
            }
        }
        length
    }

    pub(super) fn get_min_argument_count(
        &self,
        signature: &Signature,
        flags: Option<MinArgumentCountFlags>,
    ) -> usize {
        let strong_arity_for_untyped_js = match flags {
            None => false,
            Some(flags) => flags.intersects(MinArgumentCountFlags::StrongArityForUntypedJS),
        };
        let void_is_non_optional = match flags {
            None => false,
            Some(flags) => flags.intersects(MinArgumentCountFlags::VoidIsNonOptional),
        };
        if void_is_non_optional || signature.maybe_resolved_min_argument_count().is_none() {
            let mut min_argument_count = None;
            if signature_has_rest_parameter(signature) {
                let rest_type = self
                    .get_type_of_symbol(&signature.parameters()[signature.parameters().len() - 1]);
                if self.is_tuple_type(&rest_type) {
                    unimplemented!()
                }
            }
            if min_argument_count.is_none() {
                if !strong_arity_for_untyped_js
                    && signature
                        .flags
                        .intersects(SignatureFlags::IsUntypedSignatureInJSFile)
                {
                    return 0;
                }
                min_argument_count = Some(signature.min_argument_count());
            }
            let mut min_argument_count = min_argument_count.unwrap();
            if void_is_non_optional {
                return min_argument_count;
            }
            let mut i = min_argument_count - 1;
            while i >= 0 {
                let type_ = self.get_type_at_position(signature, i);
                if self
                    .filter_type(&type_, |type_| self.accepts_void(type_))
                    .flags()
                    .intersects(TypeFlags::Never)
                {
                    break;
                }
                min_argument_count = i;
                i -= 1;
            }
            signature.set_resolved_min_argument_count(min_argument_count);
        }
        signature.resolved_min_argument_count()
    }

    pub(super) fn has_effective_rest_parameter(&self, signature: &Signature) -> bool {
        if signature_has_rest_parameter(signature) {
            let rest_type =
                self.get_type_of_symbol(&signature.parameters()[signature.parameters().len() - 1]);
            return !self.is_tuple_type(&rest_type) || unimplemented!();
        }
        false
    }

    pub(super) fn get_effective_rest_type(&self, signature: &Signature) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_non_array_rest_type(&self, signature: &Signature) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_first_parameter_of_signature(
        &self,
        signature: &Signature,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn create_promise_type(&self, promised_type: &Type) -> Rc<Type> {
        let global_promise_type = self.get_global_promise_type(true);
        if !Rc::ptr_eq(&global_promise_type, &self.empty_generic_type()) {
            let promised_type = self
                .get_awaited_type_no_alias(
                    &self.unwrap_awaited_type(promised_type),
                    Option::<&Node>::None,
                    None,
                    None,
                )
                .unwrap_or_else(|| self.unknown_type());
            return self.create_type_reference(&global_promise_type, Some(vec![promised_type]));
        }

        self.unknown_type()
    }

    pub(super) fn create_promise_like_type(&self, promised_type: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn create_promise_return_type(
        &self,
        func: &Node, /*FunctionLikeDeclaration | ImportCall*/
        promised_type: &Type,
    ) -> Rc<Type> {
        let promise_type = self.create_promise_type(promised_type);
        if Rc::ptr_eq(&promise_type, &self.unknown_type()) {
            self.error(
                Some(func),
                if is_import_call(func) {
                    &Diagnostics::A_dynamic_import_call_returns_a_Promise_Make_sure_you_have_a_declaration_for_Promise_or_include_ES2015_in_your_lib_option
                } else {
                    &Diagnostics::An_async_function_or_method_must_return_a_Promise_Make_sure_you_have_a_declaration_for_Promise_or_include_ES2015_in_your_lib_option
                },
                None
            );
            return self.error_type();
        } else if self.get_global_promise_constructor_symbol(true).is_none() {
            self.error(
                Some(func),
                if is_import_call(func) {
                    &Diagnostics::A_dynamic_import_call_in_ES5_SlashES3_requires_the_Promise_constructor_Make_sure_you_have_a_declaration_for_the_Promise_constructor_or_include_ES2015_in_your_lib_option
                } else {
                    &Diagnostics::An_async_function_or_method_in_ES5_SlashES3_requires_the_Promise_constructor_Make_sure_you_have_a_declaration_for_the_Promise_constructor_or_include_ES2015_in_your_lib_option
                },
                None
            );
        }

        promise_type
    }

    pub(super) fn get_return_type_from_body(
        &self,
        func: &Node, /*FunctionLikeDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let func_as_function_like_declaration = func.as_function_like_declaration();
        if func_as_function_like_declaration.maybe_body().is_none() {
            return self.error_type();
        }
        let func_body = func_as_function_like_declaration.maybe_body().unwrap();

        let function_flags = get_function_flags(Some(func));
        let is_async = function_flags.intersects(FunctionFlags::Async);
        let is_generator = function_flags.intersects(FunctionFlags::Generator);

        let mut return_type: Option<Rc<Type>> = None;
        let mut yield_type: Option<Rc<Type>> = None;
        let mut next_type: Option<Rc<Type>> = None;
        let fallback_return_type = self.void_type();
        if func_body.kind() != SyntaxKind::Block {
            return_type = Some(self.check_expression_cached(
                &func_body,
                check_mode.map(|check_mode| check_mode & !CheckMode::SkipGenericFunctions),
            ));
            if is_async {
                unimplemented!()
            }
        } else if is_generator {
            unimplemented!()
        } else {
            let types = self.check_and_aggregate_return_expression_types(func, check_mode);
            if types.is_none() {
                return if function_flags.intersects(FunctionFlags::Async) {
                    self.create_promise_return_type(func, &self.never_type())
                } else {
                    self.never_type()
                };
            }
            let types = types.unwrap();
            if types.is_empty() {
                return if function_flags.intersects(FunctionFlags::Async) {
                    self.create_promise_return_type(func, &self.void_type())
                } else {
                    self.void_type()
                };
            }

            return_type = Some(self.get_union_type(
                types,
                Some(UnionReduction::Subtype),
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            ));
        }

        if return_type.is_some() || yield_type.is_some() || next_type.is_some() {
            if let Some(yield_type) = yield_type.as_ref() {
                self.report_errors_from_widening(
                    func,
                    yield_type,
                    Some(WideningKind::GeneratorYield),
                );
            }
            if let Some(return_type) = return_type.as_ref() {
                self.report_errors_from_widening(
                    func,
                    return_type,
                    Some(WideningKind::FunctionReturn),
                );
            }
            if let Some(next_type) = next_type.as_ref() {
                self.report_errors_from_widening(
                    func,
                    next_type,
                    Some(WideningKind::GeneratorNext),
                );
            }

            if matches!(return_type.as_ref(), Some(return_type) if self.is_unit_type(return_type))
                || matches!(yield_type.as_ref(), Some(yield_type) if self.is_unit_type(yield_type))
                || matches!(next_type.as_ref(), Some(next_type) if self.is_unit_type(next_type))
            {
                unimplemented!()
            }

            if let Some(yield_type_present) = yield_type {
                yield_type = Some(self.get_widened_type(&yield_type_present));
            }
            if let Some(return_type_present) = return_type {
                return_type = Some(self.get_widened_type(&return_type_present));
            }
            if let Some(next_type_present) = next_type {
                next_type = Some(self.get_widened_type(&next_type_present));
            }
        }

        if is_generator {
            unimplemented!()
        } else {
            if is_async {
                self.create_promise_type(&return_type.unwrap_or(fallback_return_type))
            } else {
                return_type.unwrap_or(fallback_return_type)
            }
        }
    }

    pub(super) fn get_facts_from_typeof_switch(
        &self,
        start: usize,
        end: usize,
        witnesses: &[String],
        has_default: bool,
    ) -> TypeFacts {
        unimplemented!()
    }

    pub(super) fn is_exhaustive_switch_statement(
        &self,
        node: &Node, /*SwitchStatement*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_and_aggregate_return_expression_types(
        &self,
        func: &Node, /*FunctionLikeDeclaration*/
        check_mode: Option<CheckMode>,
    ) -> Option<Vec<Rc<Type>>> {
        unimplemented!()
    }
}
