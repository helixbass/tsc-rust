#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::{IterationUse, JsxNames};
use crate::{
    cast, concatenate, filter, find_ancestor, get_assignment_declaration_kind, get_check_flags,
    get_effective_type_annotation_node, get_element_or_property_access_name, get_jsdoc_type_tag,
    get_semantic_jsx_children, get_this_container, index_of_node, is_access_expression,
    is_const_type_reference, is_identifier, is_in_js_file, is_jsdoc_type_tag, is_jsx_attribute,
    is_jsx_attribute_like, is_jsx_attributes, is_jsx_element, is_jsx_opening_element,
    is_object_literal_method, is_property_assignment, is_property_declaration,
    is_property_signature, is_this_initialized_declaration, map, some,
    unescape_leading_underscores, AssignmentDeclarationKind, CheckFlags, ContextFlags, Debug_,
    InferenceInfo, JsxReferenceKind, NodeFlags, Number, Signature, SymbolFlags,
    TransientSymbolInterface, TypeMapper, TypeSystemPropertyName, UnionOrIntersectionTypeInterface,
    UnionReduction, __String, is_object_literal_expression, InferenceContext, Node, NodeInterface,
    Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
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
                    self.get_iterated_type_or_element_type(
                        IterationUse::Element,
                        t,
                        &self.undefined_type(),
                        Option::<&Node>::None,
                        false,
                    )
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
            self.get_jsx_element_children_property_name(self.get_jsx_namespace_at(Some(node)));
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
                                            Some(node_symbol_members) if !(*node_symbol_members).borrow().contains_key(s.escaped_name())
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
                                        Some(node_symbol_members) if !(*node_symbol_members).borrow().contains_key(s.escaped_name())
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
                        Some(&inference_context.inferences()),
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
                let inference_context_return_mapper = inference_context.maybe_return_mapper();
                if let Some(inference_context_return_mapper) =
                    inference_context_return_mapper.as_ref()
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
        if is_jsx_opening_element(node)
            && node.parent().maybe_contextual_type().is_some()
            && context_flags != Some(ContextFlags::Completions)
        {
            return node.parent().maybe_contextual_type().clone().unwrap();
        }
        self.get_contextual_type_for_argument_at_index_(node, 0)
    }

    pub(super) fn get_effective_first_argument_for_jsx_signature(
        &self,
        signature: Rc<Signature>,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> Rc<Type> {
        if self.get_jsx_reference_kind(node) != JsxReferenceKind::Component {
            self.get_jsx_props_type_from_call_signature(&signature, node)
        } else {
            self.get_jsx_props_type_from_class_type(signature, node)
        }
    }

    pub(super) fn get_jsx_props_type_from_call_signature(
        &self,
        sig: &Signature,
        context: &Node, /*JsxOpeningLikeElement*/
    ) -> Rc<Type> {
        let mut props_type =
            self.get_type_of_first_parameter_of_signature_with_fallback(sig, &self.unknown_type());
        props_type = self.get_jsx_managed_attributes_from_located_attributes(
            context,
            self.get_jsx_namespace_at(Some(context)),
            &props_type,
        );
        let intrinsic_attribs = self.get_jsx_type(&JsxNames::IntrinsicAttributes, Some(context));
        if !self.is_error_type(&intrinsic_attribs) {
            props_type = self
                .intersect_types(Some(intrinsic_attribs), Some(&*props_type))
                .unwrap();
        }
        props_type
    }

    pub(super) fn get_jsx_props_type_for_signature_from_member(
        &self,
        sig: Rc<Signature>,
        forced_lookup_location: &__String,
    ) -> Option<Rc<Type>> {
        if let Some(sig_composite_signatures) = sig.composite_signatures.as_ref() {
            let mut results: Vec<Rc<Type>> = vec![];
            for signature in sig_composite_signatures {
                let instance = self.get_return_type_of_signature(signature.clone());
                if self.is_type_any(Some(&*instance)) {
                    return Some(instance);
                }
                let prop_type =
                    self.get_type_of_property_of_type_(&instance, forced_lookup_location)?;
                results.push(prop_type);
            }
            return Some(self.get_intersection_type(&results, Option::<&Symbol>::None, None));
        }
        let instance_type = self.get_return_type_of_signature(sig.clone());
        if self.is_type_any(Some(&*instance_type)) {
            Some(instance_type)
        } else {
            self.get_type_of_property_of_type_(&instance_type, forced_lookup_location)
        }
    }

    pub(super) fn get_static_type_of_referenced_jsx_constructor(
        &self,
        context: &Node, /*JsxOpeningLikeElement*/
    ) -> Rc<Type> {
        let context_as_jsx_opening_like_element = context.as_jsx_opening_like_element();
        if self.is_jsx_intrinsic_identifier(&context_as_jsx_opening_like_element.tag_name()) {
            let result = self.get_intrinsic_attributes_type_from_jsx_opening_like_element(context);
            let fake_signature = self.create_signature_for_jsx_intrinsic(context, &result);
            return self.get_or_create_type_from_signature(fake_signature);
        }
        let tag_type =
            self.check_expression_cached(&context_as_jsx_opening_like_element.tag_name(), None);
        if tag_type.flags().intersects(TypeFlags::StringLiteral) {
            let result =
                self.get_intrinsic_attributes_type_from_string_literal_type(&tag_type, context);
            if result.is_none() {
                return self.error_type();
            }
            let result = result.unwrap();
            let fake_signature = self.create_signature_for_jsx_intrinsic(context, &result);
            return self.get_or_create_type_from_signature(fake_signature);
        }
        tag_type
    }
}
