#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::ptr;
use std::rc::Rc;

use super::{
JsxNames,    signature_has_rest_parameter, CheckMode, MinArgumentCountFlags, ResolveNameNameArg, TypeFacts,
    WideningKind,
};
use crate::{
id_text,JsxFlags,is_identifier,    add_related_info, create_diagnostic_for_node, create_symbol_table, every, factory,
    get_jsx_transform_enabled, get_source_file_of_node, is_import_call, is_intrinsic_jsx_name,
    is_jsx_attribute, set_parent, string_contains, synthetic_factory, unescape_leading_underscores,
    Debug_, Diagnostics, FunctionFlags, IndexInfo, JsxReferenceKind, NodeArray, NodeFlags,
    Signature, SignatureFlags, StringOrRcNode, SymbolFlags, SymbolTable, Ternary,
    TransientSymbolInterface, UnionReduction, __String, get_function_flags, get_object_flags,
    has_initializer, InferenceContext, Node, NodeInterface, ObjectFlags, Symbol, SymbolInterface,
    SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
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
        let result: Rc<Type> = self
            .create_anonymous_type(
                node.maybe_symbol(),
                Rc::new(RefCell::new(properties_table.clone())),
                vec![],
                vec![],
                index_infos,
            )
            .into();
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
                    .contains_key("jsx"))
            && self.compiler_options.jsx_fragment_factory.is_none()
            && !node_source_file_as_source_file
                .pragmas()
                .contains_key("jsxfrag")
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
            &self.get_jsx_namespace_at(Some(opening_like_element)),
        );

        for attribute_decl in &attributes.as_jsx_attributes().properties {
            let member = attribute_decl.symbol();
            if is_jsx_attribute(attribute_decl) {
                let expr_type = self.check_jsx_attribute(attribute_decl, check_mode);
                object_flags |= get_object_flags(&expr_type) & ObjectFlags::PropagatingFlags;

                let attribute_symbol: Rc<Symbol> = self
                    .create_symbol(
                        SymbolFlags::Property | member.flags(),
                        member.escaped_name().clone(),
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
                    attribute_symbol.escaped_name().clone(),
                    attribute_symbol.clone(),
                );
                if let Some(all_attributes_table) = all_attributes_table.as_mut() {
                    all_attributes_table.insert(
                        attribute_symbol.escaped_name().clone(),
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
                                unescape_leading_underscores(jsx_children_property_name)
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
                                        unescape_leading_underscores(jsx_children_property_name),
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
                        &Into::<Rc<Type>>::into(self.create_anonymous_type(
                            attributes.maybe_symbol(),
                            Rc::new(RefCell::new(child_prop_map)),
                            vec![],
                            vec![],
                            vec![],
                        )),
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
        let result: Rc<Type> = self
            .create_anonymous_type(
                attributes.maybe_symbol(),
                attributes_table,
                vec![],
                vec![],
                vec![],
            )
            .into();
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
                            unescape_leading_underscores(left.escaped_name())
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
        name: &__String,
        location: Option<TLocation>,
    ) -> Rc<Type> {
        let location = location.map(|location| location.borrow().node_wrapper());
        let namespace = self.get_jsx_namespace_at(location.as_deref());
        let exports = /*namespace &&*/ 
            self.get_exports_of_symbol(&namespace);
        let type_symbol = /*exports &&*/ self.get_symbol(
            &(*exports).borrow(),
            name,
            SymbolFlags::Type
        );
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
            let intrinsic_elements_type = self.get_jsx_type(
                &JsxNames::IntrinsicElements,
                Some(node)
            );
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

                let index_signature_type = self.get_index_type_of_type_(
                    &intrinsic_elements_type,
                    &self.string_type()
                );
                if let Some(index_signature_type) = index_signature_type.as_ref() {
                    links.borrow_mut().jsx_flags |= JsxFlags::IntrinsicIndexedElement;
                    links.borrow_mut().resolved_symbol = Some(intrinsic_elements_type.symbol());
                    return intrinsic_elements_type.symbol();
                }

                self.error(
                    Some(node),
                    &Diagnostics::Property_0_does_not_exist_on_type_1,
                    Some(vec![
                        id_text(&node_as_has_tag_name.tag_name()),
                        format!("JSX.{}", &**JsxNames::IntrinsicElements)
                    ])
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
                            unescape_leading_underscores(&JsxNames::IntrinsicElements)
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

    pub(super) fn get_jsx_namespace_at<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
    ) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn get_jsx_library_managed_attributes(
        &self,
        jsx_namespace: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_element_properties_name(
        &self,
        jsx_namespace: &Symbol,
    ) -> Option<__String> {
        unimplemented!()
    }

    pub(super) fn get_jsx_element_children_property_name(
        &self,
        jsx_namespace: &Symbol,
    ) -> Option<__String> {
        unimplemented!()
    }

    pub(super) fn get_intrinsic_attributes_type_from_string_literal_type(
        &self,
        type_: &Type, /*StringLiteralType*/
        location: &Node,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_intrinsic_attributes_type_from_jsx_opening_like_element(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_jsx_element_type_at(&self, location: &Node) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_jsx_opening_like_element_or_opening_fragment(
        &self,
        node: &Node, /*JsxOpeningLikeElement | JsxOpeningFragment*/
    ) {
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

    pub(super) fn create_signature_for_jsx_intrinsic(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
        result: &Type,
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

    pub(super) fn get_jsx_reference_kind(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> JsxReferenceKind {
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

    pub(super) fn get_type_of_first_parameter_of_signature_with_fallback(
        &self,
        signature: &Signature,
        fallback_type: &Type,
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
