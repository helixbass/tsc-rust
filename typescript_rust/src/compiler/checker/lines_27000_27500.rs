#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, JsxNames};
use crate::{
    add_related_info, create_diagnostic_for_node, create_symbol_table, every, factory,
    get_emit_module_resolution_kind, get_jsx_implicit_import_base, get_jsx_runtime_import,
    get_jsx_transform_enabled, get_source_file_of_node, id_text, is_identifier,
    is_intrinsic_jsx_name, is_jsx_attribute, set_parent, string_contains, synthetic_factory,
    unescape_leading_underscores, Debug_, Diagnostics, IndexInfo, JsxFlags, ModuleResolutionKind,
    NodeArray, PragmaName, SymbolFlags, SymbolTable, TransientSymbolInterface, __String,
    get_object_flags, Node, NodeInterface, ObjectFlags, Symbol, SymbolInterface, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn create_object_literal_type(
        &self,
        has_computed_string_property: bool,
        node: &Node,
        offset: usize,
        properties_array: &[Rc<Symbol>],
        has_computed_number_property: bool,
        has_computed_symbol_property: bool,
        properties_table: &SymbolTable,
        object_flags: ObjectFlags,
        is_js_object_literal: bool,
        pattern_with_computed_properties: bool,
        in_destructuring_pattern: bool,
    ) -> Rc<Type> {
        let mut index_infos: Vec<Rc<IndexInfo>> = vec![];
        if has_computed_string_property {
            index_infos.push(Rc::new(self.get_object_literal_index_info(
                node,
                offset,
                &properties_array,
                &self.string_type(),
            )));
        }
        if has_computed_number_property {
            index_infos.push(Rc::new(self.get_object_literal_index_info(
                node,
                offset,
                &properties_array,
                &self.number_type(),
            )));
        }
        if has_computed_symbol_property {
            index_infos.push(Rc::new(self.get_object_literal_index_info(
                node,
                offset,
                &properties_array,
                &self.es_symbol_type(),
            )));
        }
        let result = self.create_anonymous_type(
            node.maybe_symbol(),
            Rc::new(RefCell::new(properties_table.clone())),
            vec![],
            vec![],
            index_infos,
        );
        let result_as_object_flags_type = result.as_object_flags_type();
        result_as_object_flags_type.set_object_flags(
            result_as_object_flags_type.object_flags()
                | object_flags
                | ObjectFlags::ObjectLiteral
                | ObjectFlags::ContainsObjectOrArrayLiteral,
        );
        if is_js_object_literal {
            result_as_object_flags_type.set_object_flags(
                result_as_object_flags_type.object_flags() | ObjectFlags::JSLiteral,
            );
        }
        if pattern_with_computed_properties {
            result_as_object_flags_type.set_object_flags(
                result_as_object_flags_type.object_flags()
                    | ObjectFlags::ObjectLiteralPatternWithComputedProperties,
            );
        }
        if in_destructuring_pattern {
            *result.maybe_pattern() = Some(node.node_wrapper());
        }
        result
    }

    pub(super) fn is_valid_spread_type(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Instantiable) {
            let constraint = self.get_base_constraint_of_type(type_);
            if let Some(constraint) = constraint.as_ref() {
                return self.is_valid_spread_type(constraint);
            }
        }
        type_.flags().intersects(
            TypeFlags::Any
                | TypeFlags::NonPrimitive
                | TypeFlags::Object
                | TypeFlags::InstantiableNonPrimitive,
        ) || self
            .get_falsy_flags(type_)
            .intersects(TypeFlags::DefinitelyFalsy)
            && self.is_valid_spread_type(&self.remove_definitely_falsy_types(type_))
            || type_.flags().intersects(TypeFlags::UnionOrIntersection)
                && every(
                    &type_.as_union_or_intersection_type_interface().types(),
                    |type_: &Rc<Type>, _| self.is_valid_spread_type(type_),
                )
    }

    pub(super) fn check_jsx_self_closing_element_deferred(
        &self,
        node: &Node, /*JsxSelfClosingElement*/
    ) {
        self.check_jsx_opening_like_element_or_opening_fragment(node);
    }

    pub(super) fn check_jsx_self_closing_element(
        &self,
        node: &Node, /*JsxSelfClosingElement*/
        _check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.check_node_deferred(node);
        self.get_jsx_element_type_at(node) /*|| anyType*/
    }

    pub(super) fn check_jsx_element_deferred(&self, node: &Node /*JsxElement*/) {
        let node_as_jsx_element = node.as_jsx_element();
        self.check_jsx_opening_like_element_or_opening_fragment(
            &node_as_jsx_element.opening_element,
        );

        if self.is_jsx_intrinsic_identifier(
            &node_as_jsx_element
                .closing_element
                .as_jsx_closing_element()
                .tag_name,
        ) {
            self.get_intrinsic_tag_symbol(&node_as_jsx_element.closing_element);
        } else {
            self.check_expression(
                &node_as_jsx_element
                    .closing_element
                    .as_jsx_closing_element()
                    .tag_name,
                None,
                None,
            );
        }

        self.check_jsx_children(node, None);
    }

    pub(super) fn check_jsx_element(
        &self,
        node: &Node, /*JsxElement*/
        _check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.check_node_deferred(node);

        self.get_jsx_element_type_at(node) /*|| anyType*/
    }

    pub(super) fn check_jsx_fragment(&self, node: &Node /*JsxFragment*/) -> Rc<Type> {
        let node_as_jsx_fragment = node.as_jsx_fragment();
        self.check_jsx_opening_like_element_or_opening_fragment(
            &node_as_jsx_fragment.opening_fragment,
        );

        let node_source_file = get_source_file_of_node(Some(node)).unwrap();
        let node_source_file_as_source_file = node_source_file.as_source_file();
        if get_jsx_transform_enabled(&self.compiler_options)
            && (self.compiler_options.jsx_factory.is_some()
                || node_source_file_as_source_file
                    .pragmas()
                    .contains_key(&PragmaName::Jsx))
            && self.compiler_options.jsx_fragment_factory.is_none()
            && !node_source_file_as_source_file
                .pragmas()
                .contains_key(&PragmaName::Jsxfrag)
        {
            self.error(
                Some(node),
                if self.compiler_options.jsx_factory.is_some() {
                    &Diagnostics::The_jsxFragmentFactory_compiler_option_must_be_provided_to_use_JSX_fragments_with_the_jsxFactory_compiler_option
                } else {
                    &Diagnostics::An_jsxFrag_pragma_is_required_when_using_an_jsx_pragma_with_JSX_fragments
                },
                None,
            );
        }

        self.check_jsx_children(node, None);
        self.get_jsx_element_type_at(node) /*|| anyType*/
    }

    pub(super) fn is_hyphenated_jsx_name(&self, name: &str) -> bool {
        string_contains(name, "-")
    }

    pub(super) fn is_jsx_intrinsic_identifier(
        &self,
        tag_name: &Node, /*JsxTagNameExpression*/
    ) -> bool {
        tag_name.kind() == SyntaxKind::Identifier
            && is_intrinsic_jsx_name(&tag_name.as_identifier().escaped_text)
    }

    pub(super) fn check_jsx_attribute(
        &self,
        node: &Node, /*JsxAttribute*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        if let Some(node_initializer) = node.as_jsx_attribute().initializer.as_ref() {
            self.check_expression_for_mutable_location(
                node_initializer,
                check_mode,
                Option::<&Type>::None,
                None,
            )
        } else {
            self.true_type()
        }
    }

    pub(super) fn create_jsx_attributes_type_from_attributes_property(
        &self,
        opening_like_element: &Node, /*JsxOpeningLikeElement*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let opening_like_element_as_jsx_opening_like_element =
            opening_like_element.as_jsx_opening_like_element();
        let attributes = opening_like_element_as_jsx_opening_like_element.attributes();
        let mut all_attributes_table = if self.strict_null_checks {
            Some(create_symbol_table(None))
        } else {
            None
        };
        let attributes_table = Rc::new(RefCell::new(create_symbol_table(None)));
        let mut spread = self.empty_jsx_object_type();
        let mut has_spread_any_type = false;
        let mut type_to_intersect: Option<Rc<Type>> = None;
        let mut explicitly_specify_children_attribute = false;
        let mut object_flags = ObjectFlags::JsxAttributes;
        let jsx_children_property_name = self.get_jsx_element_children_property_name(
            self.get_jsx_namespace_at(Some(opening_like_element)),
        );

        for attribute_decl in &attributes.as_jsx_attributes().properties {
            let member = attribute_decl.maybe_symbol();
            if is_jsx_attribute(attribute_decl) {
                let expr_type = self.check_jsx_attribute(attribute_decl, check_mode);
                object_flags |= get_object_flags(&expr_type) & ObjectFlags::PropagatingFlags;

                let member = member.unwrap();
                let attribute_symbol: Rc<Symbol> = self
                    .create_symbol(
                        SymbolFlags::Property | member.flags(),
                        member.escaped_name().to_owned(),
                        None,
                    )
                    .into();
                if let Some(member_declarations) = member.maybe_declarations().clone() {
                    attribute_symbol.set_declarations(member_declarations);
                }
                attribute_symbol.set_parent(member.maybe_parent());
                if let Some(member_value_declaration) = member.maybe_value_declaration() {
                    attribute_symbol.set_value_declaration(member_value_declaration);
                }
                {
                    let attribute_symbol_links =
                        attribute_symbol.as_transient_symbol().symbol_links();
                    let mut attribute_symbol_links = attribute_symbol_links.borrow_mut();
                    attribute_symbol_links.type_ = Some(expr_type.clone());
                    attribute_symbol_links.target = Some(member.clone());
                }
                attributes_table.borrow_mut().insert(
                    attribute_symbol.escaped_name().to_owned(),
                    attribute_symbol.clone(),
                );
                if let Some(all_attributes_table) = all_attributes_table.as_mut() {
                    all_attributes_table.insert(
                        attribute_symbol.escaped_name().to_owned(),
                        attribute_symbol.clone(),
                    );
                }
                if matches!(
                    jsx_children_property_name.as_ref(),
                    Some(jsx_children_property_name) if
                        &attribute_decl
                        .as_jsx_attribute()
                        .name
                        .as_identifier()
                        .escaped_text
                        == jsx_children_property_name
                ) {
                    explicitly_specify_children_attribute = true;
                }
            } else {
                Debug_.assert(
                    attribute_decl.kind() == SyntaxKind::JsxSpreadAttribute,
                    None,
                );
                if !(*attributes_table).borrow().is_empty() {
                    spread = self.get_spread_type(
                        &spread,
                        &self.create_jsx_attributes_type(
                            &mut object_flags,
                            &attributes,
                            attributes_table.clone(),
                        ),
                        attributes.maybe_symbol(),
                        object_flags,
                        false,
                    );
                    *attributes_table.borrow_mut() = create_symbol_table(None);
                }
                let expr_type = self.get_reduced_type(&self.check_expression_cached(
                    &attribute_decl.as_jsx_spread_attribute().expression,
                    check_mode,
                ));
                if self.is_type_any(Some(&*expr_type)) {
                    has_spread_any_type = true;
                }
                if self.is_valid_spread_type(&expr_type) {
                    spread = self.get_spread_type(
                        &spread,
                        &expr_type,
                        attributes.maybe_symbol(),
                        object_flags,
                        false,
                    );
                    if let Some(all_attributes_table) = all_attributes_table.as_ref() {
                        self.check_spread_prop_overrides(
                            &expr_type,
                            all_attributes_table,
                            attribute_decl,
                        );
                    }
                } else {
                    type_to_intersect = Some(
                        if let Some(type_to_intersect) = type_to_intersect.as_ref() {
                            self.get_intersection_type(
                                &[type_to_intersect.clone(), expr_type],
                                Option::<&Symbol>::None,
                                None,
                            )
                        } else {
                            expr_type
                        },
                    );
                }
            }
        }

        if !has_spread_any_type {
            if !(*attributes_table).borrow().is_empty() {
                spread = self.get_spread_type(
                    &spread,
                    &self.create_jsx_attributes_type(
                        &mut object_flags,
                        &attributes,
                        attributes_table.clone(),
                    ),
                    attributes.maybe_symbol(),
                    object_flags,
                    false,
                );
            }
        }

        let parent = if opening_like_element.parent().kind() == SyntaxKind::JsxElement {
            Some(opening_like_element.parent())
        } else {
            None
        };
        if let Some(parent) = parent.as_ref().filter(|parent| {
            let parent_as_jsx_element = parent.as_jsx_element();
            ptr::eq(
                &*parent_as_jsx_element.opening_element,
                opening_like_element,
            ) && !parent_as_jsx_element.children.is_empty()
        }) {
            let children_types = self.check_jsx_children(parent, check_mode);

            if !has_spread_any_type {
                if let Some(jsx_children_property_name) = jsx_children_property_name
                    .as_ref()
                    .filter(|jsx_children_property_name| !jsx_children_property_name.is_empty())
                {
                    if explicitly_specify_children_attribute {
                        self.error(
                            Some(&*attributes),
                            &Diagnostics::_0_are_specified_twice_The_attribute_named_0_will_be_overwritten,
                            Some(vec![
                                unescape_leading_underscores(jsx_children_property_name).to_owned()
                            ])
                        );
                    }

                    let contextual_type = self.get_apparent_type_of_contextual_type(
                        &opening_like_element_as_jsx_opening_like_element.attributes(),
                        None,
                    );
                    let children_contextual_type =
                        contextual_type.as_ref().and_then(|contextual_type| {
                            self.get_type_of_property_of_contextual_type(
                                contextual_type,
                                jsx_children_property_name,
                            )
                        });
                    let children_prop_symbol: Rc<Symbol> = self
                        .create_symbol(
                            SymbolFlags::Property,
                            jsx_children_property_name.clone(),
                            None,
                        )
                        .into();
                    children_prop_symbol
                        .as_transient_symbol()
                        .symbol_links()
                        .borrow_mut()
                        .type_ = Some(if children_types.len() == 1 {
                        children_types[0].clone()
                    } else if matches!(
                        children_contextual_type.as_ref(),
                        Some(children_contextual_type) if self.some_type(
                            children_contextual_type,
                            |type_: &Type| self.is_tuple_like_type(type_)
                        )
                    ) {
                        self.create_tuple_type(&children_types, None, None, None)
                    } else {
                        self.create_array_type(
                            &self.get_union_type(
                                children_types,
                                None,
                                Option::<&Symbol>::None,
                                None,
                                Option::<&Type>::None,
                            ),
                            None,
                        )
                    });
                    children_prop_symbol.set_value_declaration(synthetic_factory.with(
                        |synthetic_factory_| {
                            factory.with(|factory_| {
                                factory_
                                    .create_property_signature(
                                        synthetic_factory_,
                                        Option::<NodeArray>::None,
                                        unescape_leading_underscores(jsx_children_property_name)
                                            .to_owned(),
                                        None,
                                        None,
                                    )
                                    .into()
                            })
                        },
                    ));
                    set_parent(
                        children_prop_symbol
                            .maybe_value_declaration()
                            .as_ref()
                            .unwrap(),
                        Some(&*attributes),
                    );
                    children_prop_symbol
                        .maybe_value_declaration()
                        .unwrap()
                        .set_symbol(children_prop_symbol.clone());
                    let mut child_prop_map = create_symbol_table(None);
                    child_prop_map.insert(
                        jsx_children_property_name.clone(),
                        children_prop_symbol.clone(),
                    );
                    spread = self.get_spread_type(
                        &spread,
                        &self.create_anonymous_type(
                            attributes.maybe_symbol(),
                            Rc::new(RefCell::new(child_prop_map)),
                            vec![],
                            vec![],
                            vec![],
                        ),
                        attributes.maybe_symbol(),
                        object_flags,
                        false,
                    );
                }
            }
        }

        if has_spread_any_type {
            return self.any_type();
        }
        if let Some(type_to_intersect) = type_to_intersect.as_ref() {
            if !Rc::ptr_eq(&spread, &self.empty_jsx_object_type()) {
                return self.get_intersection_type(
                    &[type_to_intersect.clone(), spread.clone()],
                    Option::<&Symbol>::None,
                    None,
                );
            }
        }
        type_to_intersect.unwrap_or_else(|| {
            if Rc::ptr_eq(&spread, &self.empty_jsx_object_type()) {
                self.create_jsx_attributes_type(
                    &mut object_flags,
                    &attributes,
                    attributes_table.clone(),
                )
            } else {
                spread
            }
        })
    }

    pub(super) fn create_jsx_attributes_type(
        &self,
        object_flags: &mut ObjectFlags,
        attributes: &Node,
        attributes_table: Rc<RefCell<SymbolTable>>,
    ) -> Rc<Type> {
        *object_flags |= self.fresh_object_literal_flag;
        let result = self.create_anonymous_type(
            attributes.maybe_symbol(),
            attributes_table,
            vec![],
            vec![],
            vec![],
        );
        let result_as_object_flags_type = result.as_object_flags_type();
        result_as_object_flags_type.set_object_flags(
            result_as_object_flags_type.object_flags()
                | *object_flags
                | ObjectFlags::ObjectLiteral
                | ObjectFlags::ContainsObjectOrArrayLiteral,
        );
        result
    }

    pub(super) fn check_jsx_children(
        &self,
        node: &Node, /*JsxElement | JsxFragment*/
        check_mode: Option<CheckMode>,
    ) -> Vec<Rc<Type>> {
        let mut children_types: Vec<Rc<Type>> = vec![];
        for child in node.as_has_children().children() {
            if child.kind() == SyntaxKind::JsxText {
                if !child.as_jsx_text().contains_only_trivia_white_spaces {
                    children_types.push(self.string_type());
                }
            } else if child.kind() == SyntaxKind::JsxExpression
                && child.as_jsx_expression().expression.is_none()
            {
                continue;
            } else {
                children_types.push(self.check_expression_for_mutable_location(
                    child,
                    check_mode,
                    Option::<&Type>::None,
                    None,
                ));
            }
        }
        children_types
    }

    pub(super) fn check_spread_prop_overrides(
        &self,
        type_: &Type,
        props: &SymbolTable,
        spread: &Node, /*SpreadAssignment | JsxSpreadAttribute*/
    ) {
        for right in &self.get_properties_of_type(type_) {
            if !right.flags().intersects(SymbolFlags::Optional) {
                let left = props.get(right.escaped_name());
                if let Some(left) = left {
                    let diagnostic = self.error(
                        left.maybe_value_declaration(),
                        &Diagnostics::_0_is_specified_more_than_once_so_this_usage_will_be_overwritten,
                        Some(vec![
                            unescape_leading_underscores(left.escaped_name()).to_owned()
                        ])
                    );
                    add_related_info(
                        &diagnostic,
                        vec![Rc::new(
                            create_diagnostic_for_node(
                                spread,
                                &Diagnostics::This_spread_always_overwrites_this_property,
                                None,
                            )
                            .into(),
                        )],
                    );
                }
            }
        }
    }

    pub(super) fn check_jsx_attributes(
        &self,
        node: &Node, /*JsxAttributes*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.create_jsx_attributes_type_from_attributes_property(&node.parent(), check_mode)
    }

    pub(super) fn get_jsx_type<TLocation: Borrow<Node>>(
        &self,
        name: &str, /*__String*/
        location: Option<TLocation>,
    ) -> Rc<Type> {
        let location = location.map(|location| location.borrow().node_wrapper());
        let namespace = self.get_jsx_namespace_at(location.as_deref());
        let exports = namespace
            .as_ref()
            .map(|namespace| self.get_exports_of_symbol(namespace));
        let type_symbol = exports
            .as_ref()
            .and_then(|exports| self.get_symbol(&(**exports).borrow(), name, SymbolFlags::Type));
        if let Some(type_symbol) = type_symbol.as_ref() {
            self.get_declared_type_of_symbol(type_symbol)
        } else {
            self.error_type()
        }
    }

    pub(super) fn get_intrinsic_tag_symbol(
        &self,
        node: &Node, /*JsxOpeningLikeElement | JsxClosingElement*/
    ) -> Rc<Symbol> {
        let links = self.get_node_links(node);
        let node_as_has_tag_name = node.as_has_tag_name();
        if (*links).borrow().resolved_symbol.is_none() {
            let intrinsic_elements_type =
                self.get_jsx_type(&JsxNames::IntrinsicElements, Some(node));
            if !self.is_error_type(&intrinsic_elements_type) {
                if !is_identifier(&node_as_has_tag_name.tag_name()) {
                    Debug_.fail(None);
                }
                let intrinsic_prop = self.get_property_of_type_(
                    &intrinsic_elements_type,
                    &node_as_has_tag_name.tag_name().as_identifier().escaped_text,
                    None,
                );
                if let Some(intrinsic_prop) = intrinsic_prop.as_ref() {
                    links.borrow_mut().jsx_flags |= JsxFlags::IntrinsicNamedElement;
                    links.borrow_mut().resolved_symbol = Some(intrinsic_prop.clone());
                    return intrinsic_prop.clone();
                }

                let index_signature_type =
                    self.get_index_type_of_type_(&intrinsic_elements_type, &self.string_type());
                if let Some(index_signature_type) = index_signature_type.as_ref() {
                    links.borrow_mut().jsx_flags |= JsxFlags::IntrinsicIndexedElement;
                    links.borrow_mut().resolved_symbol = Some(intrinsic_elements_type.symbol());
                    return intrinsic_elements_type.symbol();
                }

                self.error(
                    Some(node),
                    &Diagnostics::Property_0_does_not_exist_on_type_1,
                    Some(vec![
                        id_text(&node_as_has_tag_name.tag_name()).to_owned(),
                        format!("JSX.{}", JsxNames::IntrinsicElements),
                    ]),
                );
                let ret = self.unknown_symbol();
                links.borrow_mut().resolved_symbol = Some(ret.clone());
                return ret;
            } else {
                if self.no_implicit_any {
                    self.error(
                        Some(node),
                        &Diagnostics::JSX_element_implicitly_has_type_any_because_no_interface_JSX_0_exists,
                        Some(vec![
                            unescape_leading_underscores(JsxNames::IntrinsicElements).to_owned()
                        ])
                    );
                }
                let ret = self.unknown_symbol();
                links.borrow_mut().resolved_symbol = Some(ret.clone());
                return ret;
            }
        }
        let ret = (*links).borrow().resolved_symbol.clone().unwrap();
        ret
    }

    pub(super) fn get_jsx_namespace_container_for_implicit_import<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
    ) -> Option<Rc<Symbol>> {
        let location = location.map(|location| location.borrow().node_wrapper());
        let file = location
            .as_ref()
            .and_then(|location| get_source_file_of_node(Some(&**location)));
        let links = file.as_ref().map(|file| self.get_node_links(file));
        if matches!(
            links.as_ref(),
            Some(links) if matches!(
                (**links).borrow().jsx_implicit_import_container,
                Some(None)
            )
        ) {
            return None;
        }
        if let Some(Some(links_jsx_implicit_import_container)) = links
            .as_ref()
            .and_then(|links| (**links).borrow().jsx_implicit_import_container.clone())
        {
            return Some(links_jsx_implicit_import_container);
        }
        let runtime_import_specifier = get_jsx_runtime_import(
            get_jsx_implicit_import_base(&self.compiler_options, file.as_deref()).as_deref(),
            &self.compiler_options,
        )?;
        let is_classic = get_emit_module_resolution_kind(&self.compiler_options)
            == ModuleResolutionKind::Classic;
        let error_message = if is_classic {
            &*Diagnostics::Cannot_find_module_0_Did_you_mean_to_set_the_moduleResolution_option_to_node_or_to_add_aliases_to_the_paths_option
        } else {
            &*Diagnostics::Cannot_find_module_0_or_its_corresponding_type_declarations
        };
        let mod_ = self.resolve_external_module(
            location.as_ref().unwrap(),
            &runtime_import_specifier,
            Some(error_message),
            location.as_ref().unwrap(),
            None,
        );
        let result = mod_
            .as_ref()
            .filter(|mod_| !Rc::ptr_eq(mod_, &self.unknown_symbol()))
            .map(|mod_| {
                self.get_merged_symbol(self.resolve_symbol(Some(&**mod_), None))
                    .unwrap()
            });
        if let Some(links) = links.as_ref() {
            links.borrow_mut().jsx_implicit_import_container = Some(result.clone());
        }
        result
    }

    pub(super) fn get_jsx_namespace_at<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
    ) -> Option<Rc<Symbol>> {
        let location = location.map(|location| location.borrow().node_wrapper());
        let links = location
            .as_ref()
            .map(|location| self.get_node_links(location));
        if let Some(Some(links_jsx_namespace)) = links
            .as_ref()
            .and_then(|links| (**links).borrow().jsx_namespace.clone())
        {
            return Some(links_jsx_namespace);
        }
        if match links
            .as_ref()
            .and_then(|links| (**links).borrow().jsx_namespace.clone())
        {
            None => true,
            Some(None) => false,
            _ => true,
        } {
            let mut resolved_namespace =
                self.get_jsx_namespace_container_for_implicit_import(location.as_deref());

            if match resolved_namespace.as_ref() {
                None => true,
                Some(resolved_namespace) => Rc::ptr_eq(resolved_namespace, &self.unknown_symbol()),
            } {
                let namespace_name = self.get_jsx_namespace_(location.as_deref());
                resolved_namespace = self.resolve_name_(
                    location.as_deref(),
                    &namespace_name,
                    SymbolFlags::Namespace,
                    None,
                    Some(&*namespace_name),
                    false,
                    None,
                );
            }

            if let Some(resolved_namespace) = resolved_namespace.as_ref() {
                let candidate = self.resolve_symbol(
                    self.get_symbol(
                        &(*self.get_exports_of_symbol(
                            &self
                                .resolve_symbol(Some(&**resolved_namespace), None)
                                .unwrap(),
                        ))
                        .borrow(),
                        &JsxNames::JSX,
                        SymbolFlags::Namespace,
                    ),
                    None,
                );
                if let Some(candidate) = candidate
                    .as_ref()
                    .filter(|candidate| !Rc::ptr_eq(candidate, &self.unknown_symbol()))
                {
                    if let Some(links) = links.as_ref() {
                        links.borrow_mut().jsx_namespace = Some(Some(candidate.clone()));
                    }
                    return Some(candidate.clone());
                }
            }
            if let Some(links) = links.as_ref() {
                links.borrow_mut().jsx_namespace = Some(None);
            }
        }
        let s = self.resolve_symbol(
            self.get_global_symbol(&JsxNames::JSX, SymbolFlags::Namespace, None),
            None,
        );
        if matches!(
            s.as_ref(),
            Some(s) if Rc::ptr_eq(
                s,
                &self.unknown_symbol()
            )
        ) {
            return None;
        }
        s
    }
}
