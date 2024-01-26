use std::{borrow::Borrow, io, ptr};

use gc::{Gc, GcCell};
use id_arena::Id;

use super::{CheckMode, JsxNames};
use crate::{
    add_related_info, create_diagnostic_for_node, create_symbol_table,
    get_emit_module_resolution_kind, get_factory, get_jsx_implicit_import_base,
    get_jsx_runtime_import, get_jsx_transform_enabled, get_object_flags, get_source_file_of_node,
    id_text, is_identifier, is_intrinsic_jsx_name, is_jsx_attribute, maybe_get_source_file_of_node,
    set_parent, string_contains, try_every, unescape_leading_underscores, Debug_, Diagnostics,
    HasArena, InArena, IndexInfo, JsxFlags, ModuleResolutionKind, Node, NodeArray, NodeInterface,
    ObjectFlags, OptionTry, PragmaName, Symbol, SymbolFlags, SymbolInterface, SymbolTable,
    SyntaxKind, TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface,
    OptionInArena,
};

impl TypeChecker {
    pub(super) fn create_object_literal_type(
        &self,
        has_computed_string_property: bool,
        node: Id<Node>,
        offset: usize,
        properties_array: &[Id<Symbol>],
        has_computed_number_property: bool,
        has_computed_symbol_property: bool,
        properties_table: &SymbolTable,
        object_flags: ObjectFlags,
        is_js_object_literal: bool,
        pattern_with_computed_properties: bool,
        in_destructuring_pattern: bool,
    ) -> io::Result<Id<Type>> {
        let mut index_infos: Vec<Id<IndexInfo>> = vec![];
        if has_computed_string_property {
            index_infos.push(self.alloc_index_info(self.get_object_literal_index_info(
                node,
                offset,
                &properties_array,
                self.string_type(),
            )?));
        }
        if has_computed_number_property {
            index_infos.push(self.alloc_index_info(self.get_object_literal_index_info(
                node,
                offset,
                &properties_array,
                self.number_type(),
            )?));
        }
        if has_computed_symbol_property {
            index_infos.push(self.alloc_index_info(self.get_object_literal_index_info(
                node,
                offset,
                &properties_array,
                self.es_symbol_type(),
            )?));
        }
        let result = self.create_anonymous_type(
            node.ref_(self).maybe_symbol(),
            Gc::new(GcCell::new(properties_table.clone())),
            vec![],
            vec![],
            index_infos,
        )?;
        result.ref_(self).as_object_flags_type().set_object_flags(
            result.ref_(self).as_object_flags_type().object_flags()
                | object_flags
                | ObjectFlags::ObjectLiteral
                | ObjectFlags::ContainsObjectOrArrayLiteral,
        );
        if is_js_object_literal {
            result.ref_(self).as_object_flags_type().set_object_flags(
                result.ref_(self).as_object_flags_type().object_flags() | ObjectFlags::JSLiteral,
            );
        }
        if pattern_with_computed_properties {
            result.ref_(self).as_object_flags_type().set_object_flags(
                result.ref_(self).as_object_flags_type().object_flags()
                    | ObjectFlags::ObjectLiteralPatternWithComputedProperties,
            );
        }
        if in_destructuring_pattern {
            *result.ref_(self).maybe_pattern() = Some(node);
        }
        Ok(result)
    }

    pub(super) fn is_valid_spread_type(&self, type_: Id<Type>) -> io::Result<bool> {
        if type_.ref_(self).flags().intersects(TypeFlags::Instantiable) {
            let constraint = self.get_base_constraint_of_type(type_)?;
            if let Some(constraint) = constraint {
                return self.is_valid_spread_type(constraint);
            }
        }
        Ok(type_.ref_(self).flags().intersects(
            TypeFlags::Any
                | TypeFlags::NonPrimitive
                | TypeFlags::Object
                | TypeFlags::InstantiableNonPrimitive,
        ) || self
            .get_falsy_flags(type_)
            .intersects(TypeFlags::DefinitelyFalsy)
            && self.is_valid_spread_type(self.remove_definitely_falsy_types(type_))?
            || type_
                .ref_(self)
                .flags()
                .intersects(TypeFlags::UnionOrIntersection)
                && try_every(
                    &type_
                        .ref_(self)
                        .as_union_or_intersection_type_interface()
                        .types(),
                    |&type_: &Id<Type>, _| self.is_valid_spread_type(type_),
                )?)
    }

    pub(super) fn check_jsx_self_closing_element_deferred(
        &self,
        node: Id<Node>, /*JsxSelfClosingElement*/
    ) -> io::Result<()> {
        self.check_jsx_opening_like_element_or_opening_fragment(node)?;

        Ok(())
    }

    pub(super) fn check_jsx_self_closing_element(
        &self,
        node: Id<Node>, /*JsxSelfClosingElement*/
        _check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        self.check_node_deferred(node);
        self.get_jsx_element_type_at(node) /*|| anyType*/
    }

    pub(super) fn check_jsx_element_deferred(
        &self,
        node: Id<Node>, /*JsxElement*/
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_jsx_element = node_ref.as_jsx_element();
        self.check_jsx_opening_like_element_or_opening_fragment(
            node_as_jsx_element.opening_element,
        )?;

        if self.is_jsx_intrinsic_identifier(
            node_as_jsx_element
                .closing_element
                .ref_(self).as_jsx_closing_element()
                .tag_name,
        ) {
            self.get_intrinsic_tag_symbol(node_as_jsx_element.closing_element)?;
        } else {
            self.check_expression(
                node_as_jsx_element
                    .closing_element
                    .ref_(self).as_jsx_closing_element()
                    .tag_name,
                None,
                None,
            )?;
        }

        self.check_jsx_children(node, None)?;

        Ok(())
    }

    pub(super) fn check_jsx_element(
        &self,
        node: Id<Node>, /*JsxElement*/
        _check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        self.check_node_deferred(node);

        self.get_jsx_element_type_at(node) /*|| anyType*/
    }

    pub(super) fn check_jsx_fragment(
        &self,
        node: Id<Node>, /*JsxFragment*/
    ) -> io::Result<Id<Type>> {
        let node_ref = node.ref_(self);
        let node_as_jsx_fragment = node_ref.as_jsx_fragment();
        self.check_jsx_opening_like_element_or_opening_fragment(
            node_as_jsx_fragment.opening_fragment,
        )?;

        let node_source_file = get_source_file_of_node(node, self);
        let node_source_file_ref = node_source_file.ref_(self);
        let node_source_file_as_source_file = node_source_file_ref.as_source_file();
        if get_jsx_transform_enabled(&self.compiler_options.ref_(self))
            && (self.compiler_options.ref_(self).jsx_factory.is_some()
                || node_source_file_as_source_file
                    .pragmas()
                    .contains_key(&PragmaName::Jsx))
            && self.compiler_options.ref_(self).jsx_fragment_factory.is_none()
            && !node_source_file_as_source_file
                .pragmas()
                .contains_key(&PragmaName::Jsxfrag)
        {
            self.error(
                Some(node),
                if self.compiler_options.ref_(self).jsx_factory.is_some() {
                    &Diagnostics::The_jsxFragmentFactory_compiler_option_must_be_provided_to_use_JSX_fragments_with_the_jsxFactory_compiler_option
                } else {
                    &Diagnostics::An_jsxFrag_pragma_is_required_when_using_an_jsx_pragma_with_JSX_fragments
                },
                None,
            );
        }

        self.check_jsx_children(node, None)?;
        self.get_jsx_element_type_at(node) /*|| anyType*/
    }

    pub(super) fn is_hyphenated_jsx_name(&self, name: &str) -> bool {
        string_contains(name, "-")
    }

    pub(super) fn is_jsx_intrinsic_identifier(
        &self,
        tag_name: Id<Node>, /*JsxTagNameExpression*/
    ) -> bool {
        tag_name.ref_(self).kind() == SyntaxKind::Identifier
            && is_intrinsic_jsx_name(&tag_name.ref_(self).as_identifier().escaped_text)
    }

    pub(super) fn check_jsx_attribute(
        &self,
        node: Id<Node>, /*JsxAttribute*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        Ok(
            if let Some(node_initializer) = node.ref_(self).as_jsx_attribute().initializer {
                self.check_expression_for_mutable_location(
                    node_initializer,
                    check_mode,
                    None,
                    None,
                )?
            } else {
                self.true_type()
            },
        )
    }

    pub(super) fn create_jsx_attributes_type_from_attributes_property(
        &self,
        opening_like_element: Id<Node>, /*JsxOpeningLikeElement*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let opening_like_element_ref = opening_like_element.ref_(self);
        let opening_like_element_as_jsx_opening_like_element = opening_like_element_ref.as_jsx_opening_like_element();
        let attributes = opening_like_element_as_jsx_opening_like_element.attributes();
        let mut all_attributes_table = if self.strict_null_checks {
            Some(create_symbol_table(
                self.arena(),
                Option::<&[Id<Symbol>]>::None,
            ))
        } else {
            None
        };
        let attributes_table = Gc::new(GcCell::new(create_symbol_table(
            self.arena(),
            Option::<&[Id<Symbol>]>::None,
        )));
        let mut spread = self.empty_jsx_object_type();
        let mut has_spread_any_type = false;
        let mut type_to_intersect: Option<Id<Type>> = None;
        let mut explicitly_specify_children_attribute = false;
        let mut object_flags = ObjectFlags::JsxAttributes;
        let jsx_children_property_name = self.get_jsx_element_children_property_name(
            self.get_jsx_namespace_at(Some(opening_like_element))?,
        )?;

        for &attribute_decl in &attributes.ref_(self).as_jsx_attributes().properties {
            let member = attribute_decl.ref_(self).maybe_symbol();
            if is_jsx_attribute(&attribute_decl.ref_(self)) {
                let expr_type = self.check_jsx_attribute(attribute_decl, check_mode)?;
                object_flags |=
                    get_object_flags(&expr_type.ref_(self)) & ObjectFlags::PropagatingFlags;

                let member = member.unwrap();
                let attribute_symbol = self.alloc_symbol(
                    self.create_symbol(
                        SymbolFlags::Property | member.ref_(self).flags(),
                        member.ref_(self).escaped_name().to_owned(),
                        None,
                    )
                    .into(),
                );
                if let Some(member_declarations) = member.ref_(self).maybe_declarations().clone() {
                    attribute_symbol
                        .ref_(self)
                        .set_declarations(member_declarations);
                }
                attribute_symbol
                    .ref_(self)
                    .set_parent(member.ref_(self).maybe_parent());
                if let Some(member_value_declaration) = member.ref_(self).maybe_value_declaration()
                {
                    attribute_symbol
                        .ref_(self)
                        .set_value_declaration(member_value_declaration);
                }
                {
                    let attribute_symbol_links = attribute_symbol
                        .ref_(self)
                        .as_transient_symbol()
                        .symbol_links();
                    let attribute_symbol_links_ref = attribute_symbol_links.ref_(self);
                    let mut attribute_symbol_links = attribute_symbol_links_ref.borrow_mut();
                    attribute_symbol_links.type_ = Some(expr_type.clone());
                    attribute_symbol_links.target = Some(member.clone());
                }
                attributes_table.borrow_mut().insert(
                    attribute_symbol.ref_(self).escaped_name().to_owned(),
                    attribute_symbol.clone(),
                );
                if let Some(all_attributes_table) = all_attributes_table.as_mut() {
                    all_attributes_table.insert(
                        attribute_symbol.ref_(self).escaped_name().to_owned(),
                        attribute_symbol.clone(),
                    );
                }
                if matches!(
                    jsx_children_property_name.as_ref(),
                    Some(jsx_children_property_name) if
                        &attribute_decl
                        .ref_(self).as_jsx_attribute()
                        .name
                        .ref_(self).as_identifier()
                        .escaped_text
                        == jsx_children_property_name
                ) {
                    explicitly_specify_children_attribute = true;
                }
            } else {
                Debug_.assert(
                    attribute_decl.ref_(self).kind() == SyntaxKind::JsxSpreadAttribute,
                    None,
                );
                if !(*attributes_table).borrow().is_empty() {
                    spread = self.get_spread_type(
                        spread,
                        self.create_jsx_attributes_type(
                            &mut object_flags,
                            attributes,
                            attributes_table.clone(),
                        )?,
                        attributes.ref_(self).maybe_symbol(),
                        object_flags,
                        false,
                    )?;
                    *attributes_table.borrow_mut() =
                        create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None);
                }
                let expr_type = self.get_reduced_type(self.check_expression_cached(
                    attribute_decl.ref_(self).as_jsx_spread_attribute().expression,
                    check_mode,
                )?)?;
                if self.is_type_any(Some(expr_type)) {
                    has_spread_any_type = true;
                }
                if self.is_valid_spread_type(expr_type)? {
                    spread = self.get_spread_type(
                        spread,
                        expr_type,
                        attributes.ref_(self).maybe_symbol(),
                        object_flags,
                        false,
                    )?;
                    if let Some(all_attributes_table) = all_attributes_table.as_ref() {
                        self.check_spread_prop_overrides(
                            expr_type,
                            all_attributes_table,
                            attribute_decl,
                        )?;
                    }
                } else {
                    type_to_intersect = Some(
                        if let Some(type_to_intersect) = type_to_intersect.as_ref() {
                            self.get_intersection_type(
                                &[type_to_intersect.clone(), expr_type],
                                Option::<Id<Symbol>>::None,
                                None,
                            )?
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
                    spread,
                    self.create_jsx_attributes_type(
                        &mut object_flags,
                        attributes,
                        attributes_table.clone(),
                    )?,
                    attributes.ref_(self).maybe_symbol(),
                    object_flags,
                    false,
                )?;
            }
        }

        let parent = if opening_like_element.ref_(self).parent().ref_(self).kind() == SyntaxKind::JsxElement {
            Some(opening_like_element.ref_(self).parent())
        } else {
            None
        };
        if let Some(parent) = parent.filter(|parent| {
            let parent_ref = parent.ref_(self);
            let parent_as_jsx_element = parent_ref.as_jsx_element();
            parent_as_jsx_element.opening_element == opening_like_element &&
                !parent_as_jsx_element.children.is_empty()
        }) {
            let children_types = self.check_jsx_children(parent, check_mode)?;

            if !has_spread_any_type {
                if let Some(jsx_children_property_name) = jsx_children_property_name
                    .as_ref()
                    .filter(|jsx_children_property_name| !jsx_children_property_name.is_empty())
                {
                    if explicitly_specify_children_attribute {
                        self.error(
                            Some(attributes),
                            &Diagnostics::_0_are_specified_twice_The_attribute_named_0_will_be_overwritten,
                            Some(vec![
                                unescape_leading_underscores(jsx_children_property_name).to_owned()
                            ])
                        );
                    }

                    let contextual_type = self.get_apparent_type_of_contextual_type(
                        opening_like_element_as_jsx_opening_like_element.attributes(),
                        None,
                    )?;
                    let children_contextual_type =
                        contextual_type.try_and_then(|contextual_type| {
                            self.get_type_of_property_of_contextual_type(
                                contextual_type,
                                jsx_children_property_name,
                            )
                        })?;
                    let children_prop_symbol = self.alloc_symbol(
                        self.create_symbol(
                            SymbolFlags::Property,
                            jsx_children_property_name.clone(),
                            None,
                        )
                        .into(),
                    );
                    children_prop_symbol
                        .ref_(self)
                        .as_transient_symbol()
                        .symbol_links()
                        .ref_(self).borrow_mut()
                        .type_ = Some(if children_types.len() == 1 {
                        children_types[0].clone()
                    } else if matches!(
                        children_contextual_type,
                        Some(children_contextual_type) if self.try_some_type(
                            children_contextual_type,
                            |type_: Id<Type>| self.is_tuple_like_type(type_)
                        )?
                    ) {
                        self.create_tuple_type(&children_types, None, None, None)?
                    } else {
                        self.create_array_type(
                            self.get_union_type(
                                &children_types,
                                None,
                                Option::<Id<Symbol>>::None,
                                None,
                                None,
                            )?,
                            None,
                        )
                    });
                    children_prop_symbol.ref_(self).set_value_declaration(
                        get_factory(self).create_property_signature(
                            Option::<Gc<NodeArray>>::None,
                            unescape_leading_underscores(jsx_children_property_name),
                            None,
                            None,
                        ),
                    );
                    set_parent(
                        &children_prop_symbol
                            .ref_(self)
                            .maybe_value_declaration()
                            .as_ref()
                            .unwrap().ref_(self),
                        Some(attributes),
                    );
                    children_prop_symbol
                        .ref_(self)
                        .maybe_value_declaration()
                        .unwrap()
                        .ref_(self).set_symbol(children_prop_symbol.clone());
                    let mut child_prop_map =
                        create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None);
                    child_prop_map.insert(
                        jsx_children_property_name.clone(),
                        children_prop_symbol.clone(),
                    );
                    spread = self.get_spread_type(
                        spread,
                        self.create_anonymous_type(
                            attributes.ref_(self).maybe_symbol(),
                            Gc::new(GcCell::new(child_prop_map)),
                            vec![],
                            vec![],
                            vec![],
                        )?,
                        attributes.ref_(self).maybe_symbol(),
                        object_flags,
                        false,
                    )?;
                }
            }
        }

        if has_spread_any_type {
            return Ok(self.any_type());
        }
        if let Some(type_to_intersect) = type_to_intersect {
            if spread != self.empty_jsx_object_type() {
                return self.get_intersection_type(
                    &[type_to_intersect.clone(), spread.clone()],
                    Option::<Id<Symbol>>::None,
                    None,
                );
            }
        }
        type_to_intersect.try_unwrap_or_else(|| -> io::Result<_> {
            Ok(if spread == self.empty_jsx_object_type() {
                self.create_jsx_attributes_type(
                    &mut object_flags,
                    attributes,
                    attributes_table.clone(),
                )?
            } else {
                spread
            })
        })
    }

    pub(super) fn create_jsx_attributes_type(
        &self,
        object_flags: &mut ObjectFlags,
        attributes: Id<Node>,
        attributes_table: Gc<GcCell<SymbolTable>>,
    ) -> io::Result<Id<Type>> {
        *object_flags |= self.fresh_object_literal_flag;
        let result = self.create_anonymous_type(
            attributes.ref_(self).maybe_symbol(),
            attributes_table,
            vec![],
            vec![],
            vec![],
        )?;
        result.ref_(self).as_object_flags_type().set_object_flags(
            result.ref_(self).as_object_flags_type().object_flags()
                | *object_flags
                | ObjectFlags::ObjectLiteral
                | ObjectFlags::ContainsObjectOrArrayLiteral,
        );
        Ok(result)
    }

    pub(super) fn check_jsx_children(
        &self,
        node: Id<Node>, /*JsxElement | JsxFragment*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Vec<Id<Type>>> {
        let mut children_types: Vec<Id<Type>> = vec![];
        for &child in &node.ref_(self).as_has_children().children() {
            if child.ref_(self).kind() == SyntaxKind::JsxText {
                if !child.ref_(self).as_jsx_text().contains_only_trivia_white_spaces {
                    children_types.push(self.string_type());
                }
            } else if child.ref_(self).kind() == SyntaxKind::JsxExpression
                && child.ref_(self).as_jsx_expression().expression.is_none()
            {
                continue;
            } else {
                children_types.push(
                    self.check_expression_for_mutable_location(child, check_mode, None, None)?,
                );
            }
        }
        Ok(children_types)
    }

    pub(super) fn check_spread_prop_overrides(
        &self,
        type_: Id<Type>,
        props: &SymbolTable,
        spread: Id<Node>, /*SpreadAssignment | JsxSpreadAttribute*/
    ) -> io::Result<()> {
        for right in self.get_properties_of_type(type_)? {
            if !right.ref_(self).flags().intersects(SymbolFlags::Optional) {
                let left = props.get(right.ref_(self).escaped_name());
                if let Some(&left) = left {
                    let diagnostic = self.error(
                        left.ref_(self).maybe_value_declaration(),
                        &Diagnostics::_0_is_specified_more_than_once_so_this_usage_will_be_overwritten,
                        Some(vec![
                            unescape_leading_underscores(left.ref_(self).escaped_name()).to_owned()
                        ])
                    );
                    add_related_info(
                        &diagnostic.ref_(self),
                        vec![self.alloc_diagnostic_related_information(
                            create_diagnostic_for_node(
                                spread,
                                &Diagnostics::This_spread_always_overwrites_this_property,
                                None,
                                self,
                            )
                            .into(),
                        )],
                    );
                }
            }
        }

        Ok(())
    }

    pub(super) fn check_jsx_attributes(
        &self,
        node: Id<Node>, /*JsxAttributes*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        self.create_jsx_attributes_type_from_attributes_property(node.ref_(self).parent(), check_mode)
    }

    pub(super) fn get_jsx_type(
        &self,
        name: &str, /*__String*/
        location: Option<Id<Node>>,
    ) -> io::Result<Id<Type>> {
        let namespace = self.get_jsx_namespace_at(location)?;
        let exports = namespace.try_map(|namespace| self.get_exports_of_symbol(namespace))?;
        let type_symbol = exports.as_ref().try_and_then(|exports| {
            self.get_symbol(&(**exports).borrow(), name, SymbolFlags::Type)
        })?;
        Ok(if let Some(type_symbol) = type_symbol {
            self.get_declared_type_of_symbol(type_symbol)?
        } else {
            self.error_type()
        })
    }

    pub(super) fn get_intrinsic_tag_symbol(
        &self,
        node: Id<Node>, /*JsxOpeningLikeElement | JsxClosingElement*/
    ) -> io::Result<Id<Symbol>> {
        let links = self.get_node_links(node);
        let node_ref = node.ref_(self);
        let node_as_has_tag_name = node_ref.as_has_tag_name();
        if (*links).borrow().resolved_symbol.is_none() {
            let intrinsic_elements_type =
                self.get_jsx_type(&JsxNames::IntrinsicElements, Some(node))?;
            if !self.is_error_type(intrinsic_elements_type) {
                if !is_identifier(&node_as_has_tag_name.tag_name().ref_(self)) {
                    Debug_.fail(None);
                }
                let intrinsic_prop = self.get_property_of_type_(
                    intrinsic_elements_type,
                    &node_as_has_tag_name.tag_name().ref_(self).as_identifier().escaped_text,
                    None,
                )?;
                if let Some(intrinsic_prop) = intrinsic_prop.as_ref() {
                    links.borrow_mut().jsx_flags |= JsxFlags::IntrinsicNamedElement;
                    links.borrow_mut().resolved_symbol = Some(intrinsic_prop.clone());
                    return Ok(intrinsic_prop.clone());
                }

                let index_signature_type =
                    self.get_index_type_of_type_(intrinsic_elements_type, self.string_type())?;
                if index_signature_type.is_some() {
                    links.borrow_mut().jsx_flags |= JsxFlags::IntrinsicIndexedElement;
                    links.borrow_mut().resolved_symbol =
                        Some(intrinsic_elements_type.ref_(self).symbol());
                    return Ok(intrinsic_elements_type.ref_(self).symbol());
                }

                self.error(
                    Some(node),
                    &Diagnostics::Property_0_does_not_exist_on_type_1,
                    Some(vec![
                        id_text(&node_as_has_tag_name.tag_name().ref_(self)).to_owned(),
                        format!("JSX.{}", JsxNames::IntrinsicElements),
                    ]),
                );
                let ret = self.unknown_symbol();
                links.borrow_mut().resolved_symbol = Some(ret.clone());
                return Ok(ret);
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
                return Ok(ret);
            }
        }
        let ret = (*links).borrow().resolved_symbol.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_jsx_namespace_container_for_implicit_import(
        &self,
        location: Option<Id<Node>>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let file = location
            .and_then(|location| maybe_get_source_file_of_node(Some(location), self));
        let links = file.map(|file| self.get_node_links(file));
        if matches!(
            links.as_ref(),
            Some(links) if matches!(
                (**links).borrow().jsx_implicit_import_container,
                Some(None)
            )
        ) {
            return Ok(None);
        }
        if let Some(Some(links_jsx_implicit_import_container)) = links
            .as_ref()
            .and_then(|links| (**links).borrow().jsx_implicit_import_container.clone())
        {
            return Ok(Some(links_jsx_implicit_import_container));
        }
        let runtime_import_specifier = get_jsx_runtime_import(
            get_jsx_implicit_import_base(&self.compiler_options.ref_(self), file.refed(self).as_deref()).as_deref(),
            &self.compiler_options.ref_(self),
        );
        if runtime_import_specifier.is_none() {
            return Ok(None);
        }
        let runtime_import_specifier = runtime_import_specifier.unwrap();
        let is_classic = get_emit_module_resolution_kind(&self.compiler_options.ref_(self))
            == ModuleResolutionKind::Classic;
        let error_message = if is_classic {
            &*Diagnostics::Cannot_find_module_0_Did_you_mean_to_set_the_moduleResolution_option_to_node_or_to_add_aliases_to_the_paths_option
        } else {
            &*Diagnostics::Cannot_find_module_0_or_its_corresponding_type_declarations
        };
        let mod_ = self.resolve_external_module(
            location.unwrap(),
            &runtime_import_specifier,
            Some(error_message),
            location.unwrap(),
            None,
        )?;
        let result = mod_.filter(|&mod_| mod_ != self.unknown_symbol()).try_map(
            |mod_| -> io::Result<_> {
                Ok(self
                    .get_merged_symbol(self.resolve_symbol(Some(mod_), None)?)
                    .unwrap())
            },
        )?;
        if let Some(links) = links.as_ref() {
            links.borrow_mut().jsx_implicit_import_container = Some(result.clone());
        }
        Ok(result)
    }

    pub(super) fn get_jsx_namespace_at(
        &self,
        location: Option<Id<Node>>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let links = location
            .map(|location| self.get_node_links(location));
        if let Some(Some(links_jsx_namespace)) = links
            .as_ref()
            .and_then(|links| (**links).borrow().jsx_namespace.clone())
        {
            return Ok(Some(links_jsx_namespace));
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
                self.get_jsx_namespace_container_for_implicit_import(location)?;

            if match resolved_namespace {
                None => true,
                Some(resolved_namespace) => resolved_namespace == self.unknown_symbol(),
            } {
                let namespace_name = self.get_jsx_namespace_(location);
                resolved_namespace = self.resolve_name_(
                    location,
                    &namespace_name,
                    SymbolFlags::Namespace,
                    None,
                    Some(&*namespace_name),
                    false,
                    None,
                )?;
            }

            if let Some(resolved_namespace) = resolved_namespace {
                let candidate = self.resolve_symbol(
                    self.get_symbol(
                        &(*self.get_exports_of_symbol(
                            self.resolve_symbol(Some(resolved_namespace), None)?
                                .unwrap(),
                        )?)
                        .borrow(),
                        &JsxNames::JSX,
                        SymbolFlags::Namespace,
                    )?,
                    None,
                )?;
                if let Some(candidate) =
                    candidate.filter(|&candidate| candidate != self.unknown_symbol())
                {
                    if let Some(links) = links.as_ref() {
                        links.borrow_mut().jsx_namespace = Some(Some(candidate.clone()));
                    }
                    return Ok(Some(candidate.clone()));
                }
            }
            if let Some(links) = links.as_ref() {
                links.borrow_mut().jsx_namespace = Some(None);
            }
        }
        let s = self.resolve_symbol(
            self.get_global_symbol(&JsxNames::JSX, SymbolFlags::Namespace, None)?,
            None,
        )?;
        if matches!(
            s,
            Some(s) if s == self.unknown_symbol()
        ) {
            return Ok(None);
        }
        Ok(s)
    }
}
