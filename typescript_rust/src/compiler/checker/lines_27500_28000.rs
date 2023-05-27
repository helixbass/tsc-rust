use gc::{Finalize, Gc, Trace};
use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::io;
use std::rc::Rc;

use super::{CheckMode, CheckTypeContainingMessageChain, JsxNames};
use crate::{
    chain_diagnostic_messages, escape_leading_underscores, every, get_assignment_declaration_kind,
    get_check_flags, get_class_like_declaration_of_symbol, get_combined_node_flags,
    get_declaration_modifier_flags_from_symbol, get_source_file_of_node,
    get_text_of_identifier_or_literal, get_text_of_node, is_binary_expression, is_in_js_file,
    is_jsx_opening_fragment, is_jsx_opening_like_element, is_object_binding_pattern,
    is_this_initialized_declaration, is_this_initialized_object_binding_expression,
    is_this_property, some, unescape_leading_underscores, AssignmentDeclarationKind,
    CheckFlags, Debug_, DiagnosticMessageChain, Diagnostics, HasTypeInterface, JsxEmit, JsxFlags,
    JsxReferenceKind, ModifierFlags, NodeFlags, ScriptTarget, Signature, SignatureKind,
    SymbolFlags, UnionOrIntersectionTypeInterface, __String, get_object_flags, try_map, Node,
    NodeInterface, ObjectFlags, OptionTry, Symbol, SymbolInterface, SyntaxKind, Type, TypeChecker,
    TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn get_name_from_jsx_element_attributes_container(
        &self,
        name_of_attrib_prop_container: &str, /*__String*/
        jsx_namespace: Option<impl Borrow<Symbol>>,
    ) -> io::Result<Option<__String>> {
        let jsx_namespace =
            jsx_namespace.map(|jsx_namespace| jsx_namespace.borrow().symbol_wrapper());
        let jsx_element_attrib_prop_interface_sym =
            jsx_namespace.as_ref().try_and_then(|jsx_namespace| {
                self.get_symbol(
                    &(**jsx_namespace.maybe_exports().as_ref().unwrap()).borrow(),
                    name_of_attrib_prop_container,
                    SymbolFlags::Type,
                )
            })?;
        let jsx_element_attrib_prop_interface_type = jsx_element_attrib_prop_interface_sym
            .as_ref()
            .try_map(|jsx_element_attrib_prop_interface_sym| {
                self.get_declared_type_of_symbol(jsx_element_attrib_prop_interface_sym)
            })?;
        let properties_of_jsx_element_attrib_prop_interface =
            jsx_element_attrib_prop_interface_type.as_ref().try_map(
                |jsx_element_attrib_prop_interface_type| {
                    self.get_properties_of_type(jsx_element_attrib_prop_interface_type)
                },
            )?;
        Ok(properties_of_jsx_element_attrib_prop_interface.and_then(
            |mut properties_of_jsx_element_attrib_prop_interface| {
                if properties_of_jsx_element_attrib_prop_interface.len() == 0 {
                    return Some("".to_owned());
                } else if properties_of_jsx_element_attrib_prop_interface.len() == 1 {
                    return Some(
                        properties_of_jsx_element_attrib_prop_interface
                            .next()
                            .unwrap()
                            .escaped_name()
                            .to_owned(),
                    );
                } else if properties_of_jsx_element_attrib_prop_interface.len() > 1 {
                    let jsx_element_attrib_prop_interface_sym =
                        jsx_element_attrib_prop_interface_sym.as_ref().unwrap();
                    if let Some(jsx_element_attrib_prop_interface_sym_declarations) =
                        jsx_element_attrib_prop_interface_sym
                            .maybe_declarations()
                            .as_ref()
                    {
                        self.error(
                            jsx_element_attrib_prop_interface_sym_declarations
                                .get(0)
                                .cloned(),
                            &Diagnostics::The_global_type_JSX_0_may_not_have_more_than_one_property,
                            Some(vec![unescape_leading_underscores(
                                name_of_attrib_prop_container,
                            )
                            .to_owned()]),
                        );
                    }
                }
                None
            },
        ))
    }

    pub(super) fn get_jsx_library_managed_attributes(
        &self,
        jsx_namespace: Option<impl Borrow<Symbol>>,
    ) -> io::Result<Option<Gc<Symbol>>> {
        if jsx_namespace.is_none() {
            return Ok(None);
        }
        let jsx_namespace = jsx_namespace.unwrap();
        let jsx_namespace = jsx_namespace.borrow();
        let ret = self.get_symbol(
            &(**jsx_namespace.maybe_exports().as_ref().unwrap()).borrow(),
            &JsxNames::LibraryManagedAttributes,
            SymbolFlags::Type,
        )?;
        Ok(ret)
    }

    pub(super) fn get_jsx_element_properties_name(
        &self,
        jsx_namespace: Option<impl Borrow<Symbol>>,
    ) -> io::Result<Option<__String>> {
        self.get_name_from_jsx_element_attributes_container(
            &JsxNames::ElementAttributesPropertyNameContainer,
            jsx_namespace,
        )
    }

    pub(super) fn get_jsx_element_children_property_name(
        &self,
        jsx_namespace: Option<impl Borrow<Symbol>>,
    ) -> io::Result<Option<__String>> {
        self.get_name_from_jsx_element_attributes_container(
            &JsxNames::ElementChildrenAttributeNameContainer,
            jsx_namespace,
        )
    }

    pub(super) fn get_uninstantiated_jsx_signatures_of_type(
        &self,
        element_type: &Type,
        caller: &Node, /*JsxOpeningLikeElement*/
    ) -> io::Result<Vec<Gc<Signature>>> {
        if element_type.flags().intersects(TypeFlags::String) {
            return Ok(vec![self.any_signature()]);
        } else if element_type.flags().intersects(TypeFlags::StringLiteral) {
            let intrinsic_type =
                self.get_intrinsic_attributes_type_from_string_literal_type(element_type, caller)?;
            match intrinsic_type.as_ref() {
                None => {
                    self.error(
                        Some(caller),
                        &Diagnostics::Property_0_does_not_exist_on_type_1,
                        Some(vec![
                            element_type.as_string_literal_type().value.clone(),
                            format!("JSX.{}", JsxNames::IntrinsicElements),
                        ]),
                    );
                    return Ok(vec![]);
                }
                Some(intrinsic_type) => {
                    let fake_signature =
                        self.create_signature_for_jsx_intrinsic(caller, intrinsic_type)?;
                    return Ok(vec![fake_signature]);
                }
            }
        }
        let apparent_elem_type = self.get_apparent_type(element_type)?;
        let mut signatures =
            self.get_signatures_of_type(&apparent_elem_type, SignatureKind::Construct)?;
        if signatures.is_empty() {
            signatures = self.get_signatures_of_type(&apparent_elem_type, SignatureKind::Call)?;
        }
        if signatures.is_empty() && apparent_elem_type.flags().intersects(TypeFlags::Union) {
            signatures = self.get_union_signatures(&try_map(
                apparent_elem_type.as_union_type().types(),
                |t: &Gc<Type>, _| self.get_uninstantiated_jsx_signatures_of_type(t, caller),
            )?)?;
        }
        Ok(signatures)
    }

    pub(super) fn get_intrinsic_attributes_type_from_string_literal_type(
        &self,
        type_: &Type, /*StringLiteralType*/
        location: &Node,
    ) -> io::Result<Option<Gc<Type>>> {
        let intrinsic_elements_type =
            self.get_jsx_type(&JsxNames::IntrinsicElements, Some(location))?;
        if !self.is_error_type(&intrinsic_elements_type) {
            let string_literal_type_name = &type_.as_string_literal_type().value;
            let intrinsic_prop = self.get_property_of_type_(
                &intrinsic_elements_type,
                &escape_leading_underscores(string_literal_type_name),
                None,
            )?;
            if let Some(intrinsic_prop) = intrinsic_prop.as_ref() {
                return Ok(Some(self.get_type_of_symbol(intrinsic_prop)?));
            }
            let index_signature_type =
                self.get_index_type_of_type_(&intrinsic_elements_type, &self.string_type())?;
            if index_signature_type.is_some() {
                return Ok(index_signature_type);
            }
            return Ok(None);
        }
        Ok(Some(self.any_type()))
    }

    pub(super) fn check_jsx_return_assignable_to_appropriate_bound(
        &self,
        ref_kind: JsxReferenceKind,
        elem_instance_type: &Type,
        opening_like_element: &Node, /*JsxOpeningLikeElement*/
    ) -> io::Result<()> {
        if ref_kind == JsxReferenceKind::Function {
            let sfc_return_constraint =
                self.get_jsx_stateless_element_type_at(opening_like_element)?;
            if let Some(sfc_return_constraint) = sfc_return_constraint.as_ref() {
                self.check_type_related_to(
                    elem_instance_type,
                    sfc_return_constraint,
                    self.assignable_relation.clone(),
                    Some(
                        opening_like_element
                            .as_jsx_opening_like_element()
                            .tag_name(),
                    ),
                    Some(Cow::Borrowed(
                        &Diagnostics::Its_return_type_0_is_not_a_valid_JSX_element,
                    )),
                    Some(Gc::new(Box::new(GenerateInitialErrorChain::new(
                        opening_like_element.node_wrapper(),
                    )))),
                    None,
                )?;
            }
        } else if ref_kind == JsxReferenceKind::Component {
            let class_constraint = self.get_jsx_element_class_type_at(opening_like_element)?;
            if let Some(class_constraint) = class_constraint.as_ref() {
                self.check_type_related_to(
                    elem_instance_type,
                    class_constraint,
                    self.assignable_relation.clone(),
                    Some(
                        opening_like_element
                            .as_jsx_opening_like_element()
                            .tag_name(),
                    ),
                    Some(Cow::Borrowed(
                        &Diagnostics::Its_instance_type_0_is_not_a_valid_JSX_element,
                    )),
                    Some(Gc::new(Box::new(GenerateInitialErrorChain::new(
                        opening_like_element.node_wrapper(),
                    )))),
                    None,
                )?;
            }
        } else {
            let sfc_return_constraint =
                self.get_jsx_stateless_element_type_at(opening_like_element)?;
            let class_constraint = self.get_jsx_element_class_type_at(opening_like_element)?;
            if sfc_return_constraint.is_none() || class_constraint.is_none() {
                return Ok(());
            }
            let sfc_return_constraint = sfc_return_constraint.unwrap();
            let class_constraint = class_constraint.unwrap();
            let combined = self.get_union_type(
                &[sfc_return_constraint, class_constraint],
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )?;
            self.check_type_related_to(
                elem_instance_type,
                &combined,
                self.assignable_relation.clone(),
                Some(
                    opening_like_element
                        .as_jsx_opening_like_element()
                        .tag_name(),
                ),
                Some(Cow::Borrowed(
                    &Diagnostics::Its_element_type_0_is_not_a_valid_JSX_element,
                )),
                Some(Gc::new(Box::new(GenerateInitialErrorChain::new(
                    opening_like_element.node_wrapper(),
                )))),
                None,
            )?;
        }

        Ok(())
    }

    pub(super) fn get_intrinsic_attributes_type_from_jsx_opening_like_element(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> io::Result<Gc<Type>> {
        let node_as_jsx_opening_like_element = node.as_jsx_opening_like_element();
        Debug_.assert(
            self.is_jsx_intrinsic_identifier(&node_as_jsx_opening_like_element.tag_name()),
            None,
        );
        let links = self.get_node_links(node);
        if (*links)
            .borrow()
            .resolved_jsx_element_attributes_type
            .is_none()
        {
            let symbol = self.get_intrinsic_tag_symbol(node)?;
            if (*links)
                .borrow()
                .jsx_flags
                .intersects(JsxFlags::IntrinsicNamedElement)
            {
                let ret = self.get_type_of_symbol(&symbol)?; /*|| errorType*/
                links.borrow_mut().resolved_jsx_element_attributes_type = Some(ret.clone());
                return Ok(ret);
            } else if (*links)
                .borrow()
                .jsx_flags
                .intersects(JsxFlags::IntrinsicIndexedElement)
            {
                let ret = self
                    .get_index_type_of_type_(
                        &*self.get_jsx_type(&JsxNames::IntrinsicElements, Some(node))?,
                        &self.string_type(),
                    )?
                    .unwrap_or_else(|| self.error_type());
                links.borrow_mut().resolved_jsx_element_attributes_type = Some(ret.clone());
                return Ok(ret);
            } else {
                let ret = self.error_type();
                links.borrow_mut().resolved_jsx_element_attributes_type = Some(ret.clone());
                return Ok(ret);
            }
        }
        let ret = (*links)
            .borrow()
            .resolved_jsx_element_attributes_type
            .clone()
            .unwrap();
        Ok(ret)
    }

    pub(super) fn get_jsx_element_class_type_at(
        &self,
        location: &Node,
    ) -> io::Result<Option<Gc<Type>>> {
        let type_ = self.get_jsx_type(&JsxNames::ElementClass, Some(location))?;
        if self.is_error_type(&type_) {
            return Ok(None);
        }
        Ok(Some(type_))
    }

    pub(super) fn get_jsx_element_type_at(&self, location: &Node) -> io::Result<Gc<Type>> {
        self.get_jsx_type(&JsxNames::Element, Some(location))
    }

    pub(super) fn get_jsx_stateless_element_type_at(
        &self,
        location: &Node,
    ) -> io::Result<Option<Gc<Type>>> {
        let jsx_element_type = self.get_jsx_element_type_at(location)?;
        // if (jsxElementType) {
        Ok(Some(self.get_union_type(
            &[jsx_element_type, self.null_type()],
            None,
            Option::<&Symbol>::None,
            None,
            Option::<&Type>::None,
        )?))
        // }
    }

    pub fn get_jsx_intrinsic_tag_names_at(
        &self,
        location: &Node,
    ) -> io::Result<impl Iterator<Item = Gc<Symbol>>> {
        let intrinsics = self.get_jsx_type(&JsxNames::IntrinsicElements, Some(location))?;
        /*intrinsics ?*/
        self.get_properties_of_type(&intrinsics) /*: emptyArray*/
    }

    pub(super) fn check_jsx_preconditions(&self, error_node: &Node) {
        if self.compiler_options.jsx.unwrap_or(JsxEmit::None) == JsxEmit::None {
            self.error(
                Some(error_node),
                &Diagnostics::Cannot_use_JSX_unless_the_jsx_flag_is_provided,
                None,
            );
        }

        // if (getJsxElementTypeAt(errorNode) === undefined) {
        //     if (noImplicitAny) {
        //         error(errorNode, Diagnostics.JSX_element_implicitly_has_type_any_because_the_global_type_JSX_Element_does_not_exist);
        //     }
        // }
    }

    pub(super) fn check_jsx_opening_like_element_or_opening_fragment(
        &self,
        node: &Node, /*JsxOpeningLikeElement | JsxOpeningFragment*/
    ) -> io::Result<()> {
        let is_node_opening_like_element = is_jsx_opening_like_element(node);

        if is_node_opening_like_element {
            self.check_grammar_jsx_element(node);
        }

        self.check_jsx_preconditions(node);

        if self
            .get_jsx_namespace_container_for_implicit_import(Some(node))?
            .is_none()
        {
            let jsx_factory_ref_err = if
            /*diagnostics &&*/
            self.compiler_options.jsx == Some(JsxEmit::React) {
                Some(&*Diagnostics::Cannot_find_name_0)
            } else {
                None
            };
            let jsx_factory_namespace = self.get_jsx_namespace_(Some(node));
            let jsx_factory_location = if is_node_opening_like_element {
                node.as_jsx_opening_like_element().tag_name()
            } else {
                node.node_wrapper()
            };

            let mut jsx_factory_sym: Option<Gc<Symbol>> = None;
            if !(is_jsx_opening_fragment(node) && jsx_factory_namespace == "null") {
                jsx_factory_sym = self.resolve_name_(
                    Some(&*jsx_factory_location),
                    &jsx_factory_namespace,
                    SymbolFlags::Value,
                    jsx_factory_ref_err,
                    Some(&*jsx_factory_namespace),
                    true,
                    None,
                )?;
            }

            if let Some(jsx_factory_sym) = jsx_factory_sym.as_ref() {
                jsx_factory_sym.set_is_referenced(Some(SymbolFlags::All));

                if jsx_factory_sym.flags().intersects(SymbolFlags::Alias)
                    && self
                        .get_type_only_alias_declaration(jsx_factory_sym)
                        .is_none()
                {
                    self.mark_alias_symbol_as_referenced(jsx_factory_sym)?;
                }
            }

            if is_jsx_opening_fragment(node) {
                let file = get_source_file_of_node(node);
                let local_jsx_namespace = self.get_local_jsx_namespace(&file);
                if let Some(local_jsx_namespace) = local_jsx_namespace.as_ref() {
                    self.resolve_name_(
                        Some(&*jsx_factory_location),
                        local_jsx_namespace,
                        SymbolFlags::Value,
                        jsx_factory_ref_err,
                        Some(&**local_jsx_namespace),
                        true,
                        None,
                    )?;
                }
            }
        }

        if is_node_opening_like_element {
            let jsx_opening_like_node = node;
            let sig = self.get_resolved_signature_(jsx_opening_like_node, None, None)?;
            self.check_deprecated_signature(sig.clone(), node)?;
            self.check_jsx_return_assignable_to_appropriate_bound(
                self.get_jsx_reference_kind(jsx_opening_like_node)?,
                &*self.get_return_type_of_signature(sig.clone())?,
                jsx_opening_like_node,
            )?;
        }

        Ok(())
    }

    pub(super) fn is_known_property(
        &self,
        target_type: &Type,
        name: &str, /*__String*/
        is_comparing_jsx_attributes: bool,
    ) -> io::Result<bool> {
        if target_type.flags().intersects(TypeFlags::Object) {
            if self
                .get_property_of_object_type(target_type, name)?
                .is_some()
                || self
                    .get_applicable_index_info_for_name(target_type, name)?
                    .is_some()
                || self.is_late_bound_name(name)
                    && self
                        .get_index_info_of_type_(target_type, &self.string_type())?
                        .is_some()
                || is_comparing_jsx_attributes && self.is_hyphenated_jsx_name(name)
            {
                return Ok(true);
            }
        } else if target_type
            .flags()
            .intersects(TypeFlags::UnionOrIntersection)
            && self.is_excess_property_check_target(target_type)
        {
            for t in target_type
                .as_union_or_intersection_type_interface()
                .types()
            {
                if self.is_known_property(t, name, is_comparing_jsx_attributes)? {
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }

    pub(super) fn is_excess_property_check_target(&self, type_: &Type) -> bool {
        (type_.flags().intersects(TypeFlags::Object)
            && !(get_object_flags(type_)
                .intersects(ObjectFlags::ObjectLiteralPatternWithComputedProperties)))
            || type_.flags().intersects(TypeFlags::NonPrimitive)
            || (type_.flags().intersects(TypeFlags::Union)
                && some(
                    Some(type_.as_union_type().types()),
                    Some(|type_: &Gc<Type>| self.is_excess_property_check_target(type_)),
                ))
            || (type_.flags().intersects(TypeFlags::Intersection)
                && every(
                    type_.as_intersection_type().types(),
                    |type_: &Gc<Type>, _| self.is_excess_property_check_target(type_),
                ))
    }

    pub(super) fn check_jsx_expression(
        &self,
        node: &Node, /*JsxExpression*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Gc<Type>> {
        self.check_grammar_jsx_expression(node);
        let node_as_jsx_expression = node.as_jsx_expression();
        Ok(
            if let Some(node_expression) = node_as_jsx_expression.expression.as_ref() {
                let type_ = self.check_expression(node_expression, check_mode, None)?;
                if node_as_jsx_expression.dot_dot_dot_token.is_some()
                    && !Gc::ptr_eq(&type_, &self.any_type())
                    && !self.is_array_type(&type_)
                {
                    self.error(
                        Some(node),
                        &Diagnostics::JSX_spread_child_must_be_an_array_type,
                        None,
                    );
                }
                type_
            } else {
                self.error_type()
            },
        )
    }

    pub(super) fn get_declaration_node_flags_from_symbol(&self, s: &Symbol) -> NodeFlags {
        if let Some(s_value_declaration) = s.maybe_value_declaration() {
            get_combined_node_flags(&s_value_declaration)
        } else {
            NodeFlags::None
        }
    }

    pub(super) fn is_prototype_property(&self, symbol: &Symbol) -> bool {
        if symbol.flags().intersects(SymbolFlags::Method)
            || get_check_flags(symbol).intersects(CheckFlags::SyntheticMethod)
        {
            return true;
        }
        if is_in_js_file(symbol.maybe_value_declaration()) {
            let parent = symbol.maybe_value_declaration().unwrap().maybe_parent();
            return matches!(
                parent.as_ref(),
                Some(parent) if is_binary_expression(parent) &&
                    get_assignment_declaration_kind(parent) == AssignmentDeclarationKind::PrototypeProperty
            );
        }
        false
    }

    pub(super) fn check_property_accessibility(
        &self,
        node: &Node, /*PropertyAccessExpression | QualifiedName | VariableDeclaration | ParameterDeclaration | ImportTypeNode | PropertyAssignment | ShorthandPropertyAssignment | BindingElement*/
        is_super: bool,
        writing: bool,
        type_: &Type,
        prop: &Symbol,
        report_error: Option<bool>,
    ) -> io::Result<bool> {
        let report_error = report_error.unwrap_or(true);
        let error_node = if !report_error {
            None
        } else if node.kind() == SyntaxKind::QualifiedName {
            Some(node.as_qualified_name().right.clone())
        } else if node.kind() == SyntaxKind::ImportType {
            Some(node.node_wrapper())
        } else if node.kind() == SyntaxKind::BindingElement
            && node.as_binding_element().property_name.is_some()
        {
            Some(node.as_binding_element().property_name.clone().unwrap())
        } else {
            node.as_named_declaration().maybe_name()
        };

        self.check_property_accessibility_at_location(
            node, is_super, writing, type_, prop, error_node,
        )
    }

    pub(super) fn check_property_accessibility_at_location(
        &self,
        location: &Node,
        is_super: bool,
        writing: bool,
        containing_type: &Type,
        prop: &Symbol,
        error_node: Option<impl Borrow<Node>>,
    ) -> io::Result<bool> {
        let flags = get_declaration_modifier_flags_from_symbol(prop, Some(writing));

        let error_node = error_node.map(|error_node| error_node.borrow().node_wrapper());
        if is_super {
            if self.language_version < ScriptTarget::ES2015 {
                if self.symbol_has_non_method_declaration(prop)? {
                    if error_node.is_some() {
                        self.error(
                            error_node.as_deref(),
                            &Diagnostics::Only_public_and_protected_methods_of_the_base_class_are_accessible_via_the_super_keyword,
                            None,
                        );
                    }
                    return Ok(false);
                }
            }
            if flags.intersects(ModifierFlags::Abstract) {
                if error_node.is_some() {
                    self.error(
                        error_node.as_deref(),
                        &Diagnostics::Abstract_method_0_in_class_1_cannot_be_accessed_via_super_expression,
                        Some(vec![
                            self.symbol_to_string_(
                                prop,
                                Option::<&Node>::None,
                                None, None, None
                            )?,
                            self.type_to_string_(
                                &self.get_declaring_class(prop)?.unwrap(),
                                Option::<&Node>::None,
                                None, None,
                            )?,
                        ])
                    );
                }
                return Ok(false);
            }
        }

        if flags.intersects(ModifierFlags::Abstract)
            && self.symbol_has_non_method_declaration(prop)?
            && (is_this_property(location)
                || is_this_initialized_object_binding_expression(Some(location))
                || is_object_binding_pattern(&location.parent())
                    && is_this_initialized_declaration(location.parent().maybe_parent()))
        {
            let declaring_class_declaration =
                get_class_like_declaration_of_symbol(&self.get_parent_of_symbol(prop)?.unwrap());
            if let Some(declaring_class_declaration) = declaring_class_declaration.as_ref() {
                if self.is_node_used_during_class_initialization(location) {
                    if error_node.is_some() {
                        self.error(
                            error_node.as_deref(),
                            &Diagnostics::Abstract_property_0_in_class_1_cannot_be_accessed_in_the_constructor,
                            Some(vec![
                                self.symbol_to_string_(
                                    prop,
                                    Option::<&Node>::None, None, None, None,
                                )?,
                                get_text_of_identifier_or_literal(&declaring_class_declaration.as_named_declaration().name()).into_owned()
                            ])
                        );
                    }
                    return Ok(false);
                }
            }
        }

        if !flags.intersects(ModifierFlags::NonPublicAccessibilityModifier) {
            return Ok(true);
        }

        if flags.intersects(ModifierFlags::Private) {
            let declaring_class_declaration =
                get_class_like_declaration_of_symbol(&self.get_parent_of_symbol(prop)?.unwrap())
                    .unwrap();
            if !self.is_node_within_class(location, &declaring_class_declaration) {
                if error_node.is_some() {
                    self.error(
                        error_node.as_deref(),
                        &Diagnostics::Property_0_is_private_and_only_accessible_within_class_1,
                        Some(vec![
                            self.symbol_to_string_(prop, Option::<&Node>::None, None, None, None)?,
                            self.type_to_string_(
                                &self.get_declaring_class(prop)?.unwrap(),
                                Option::<&Node>::None,
                                None,
                                None,
                            )?,
                        ]),
                    );
                }
                return Ok(false);
            }
            return Ok(true);
        }

        if is_super {
            return Ok(true);
        }

        let mut enclosing_class =
            self.try_for_each_enclosing_class(location, |enclosing_declaration: &Node| {
                let enclosing_class = self.get_declared_type_of_symbol(
                    &self.get_symbol_of_node(enclosing_declaration)?.unwrap(),
                )?;
                Ok(
                    if self
                        .is_class_derived_from_declaring_classes(&enclosing_class, prop, writing)?
                        .is_some()
                    {
                        Some(enclosing_class)
                    } else {
                        None
                    },
                )
            })?;
        if enclosing_class.is_none() {
            let mut this_parameter: Option<Gc<Node /*ParameterDeclaration*/>> = None;
            if flags.intersects(ModifierFlags::Static) || {
                this_parameter = self.get_this_parameter_from_node_context(location);
                match this_parameter.as_ref() {
                    None => true,
                    Some(this_parameter) => this_parameter
                        .as_parameter_declaration()
                        .maybe_type()
                        .is_none(),
                }
            } {
                if error_node.is_some() {
                    self.error(
                        error_node.as_deref(),
                        &Diagnostics::Property_0_is_protected_and_only_accessible_within_class_1_and_its_subclasses,
                        Some(vec![
                            self.symbol_to_string_(
                                prop,
                                Option::<&Node>::None,
                                None, None, None,
                            )?,
                            self.type_to_string_(
                                &self.get_declaring_class(prop)?.unwrap_or_else(|| containing_type.type_wrapper()),
                                Option::<&Node>::None,
                                None, None,
                            )?,
                        ])
                    );
                }
                return Ok(false);
            }

            let this_type = self.get_type_from_type_node_(
                &this_parameter
                    .as_ref()
                    .unwrap()
                    .as_parameter_declaration()
                    .maybe_type()
                    .unwrap(),
            )?;
            enclosing_class = Some(
                if this_type.flags().intersects(TypeFlags::TypeParameter) {
                    self.get_constraint_of_type_parameter(&this_type)?.unwrap()
                } else {
                    this_type
                }
                .as_type_reference_interface()
                .target(),
            );
        }
        let enclosing_class = enclosing_class.unwrap();
        if flags.intersects(ModifierFlags::Static) {
            return Ok(true);
        }
        let mut containing_type = Some(containing_type.type_wrapper());
        if containing_type
            .as_ref()
            .unwrap()
            .flags()
            .intersects(TypeFlags::TypeParameter)
        {
            containing_type = if containing_type
                .as_ref()
                .unwrap()
                .as_type_parameter()
                .is_this_type
                == Some(true)
            {
                self.get_constraint_of_type_parameter(containing_type.as_ref().unwrap())?
            } else {
                self.get_base_constraint_of_type(containing_type.as_ref().unwrap())?
            };
        }
        if match containing_type.as_ref() {
            None => true,
            Some(containing_type) => {
                !self.has_base_type(containing_type, Some(&*enclosing_class))?
            }
        } {
            if error_node.is_some() {
                self.error(
                    error_node.as_deref(),
                    &Diagnostics::Property_0_is_protected_and_only_accessible_through_an_instance_of_class_1_This_is_an_instance_of_class_2,
                    Some(vec![
                        self.symbol_to_string_(
                            prop,
                            Option::<&Node>::None,
                            None, None, None,
                        )?,
                        self.type_to_string_(
                            &enclosing_class,
                            Option::<&Node>::None,
                            None, None,
                        )?,
                        self.type_to_string_(
                            // TODO: this looks like type_to_string_() actually should accept an Option<Type>
                            containing_type.as_ref().unwrap(),
                            Option::<&Node>::None,
                            None, None,
                        )?,
                    ])
                );
                return Ok(false);
            }
        }
        Ok(true)
    }
}

#[derive(Trace, Finalize)]
pub(super) struct GenerateInitialErrorChain {
    opening_like_element: Gc<Node>,
}

impl GenerateInitialErrorChain {
    pub fn new(opening_like_element: Gc<Node>) -> Self {
        Self {
            opening_like_element,
        }
    }
}

impl CheckTypeContainingMessageChain for GenerateInitialErrorChain {
    fn get(&self) -> io::Result<Option<Rc<RefCell<DiagnosticMessageChain>>>> {
        let component_name = get_text_of_node(
            &self
                .opening_like_element
                .as_jsx_opening_like_element()
                .tag_name(),
            None,
        );
        Ok(Some(Rc::new(RefCell::new(chain_diagnostic_messages(
            None,
            &Diagnostics::_0_cannot_be_used_as_a_JSX_component,
            Some(vec![component_name.into_owned()]),
        )))))
    }
}
