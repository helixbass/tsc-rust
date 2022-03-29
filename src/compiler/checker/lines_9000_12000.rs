#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, MappedTypeModifiers, MembersOrExportsResolutionKind};
use crate::{
    concatenate, create_symbol_table, escape_leading_underscores, find, find_last_index, for_each,
    for_each_child_recursively_bool, get_check_flags, get_declaration_of_kind,
    get_effective_return_type_node, get_effective_set_accessor_type_annotation_node,
    get_effective_type_annotation_node, get_effective_type_parameter_declarations,
    get_root_declaration, get_source_file_of_node, get_this_container, has_dynamic_name,
    is_accessor, is_binary_expression, is_bindable_static_element_access_expression,
    is_binding_element, is_binding_pattern, is_call_expression,
    is_catch_clause_variable_declaration_or_binding_element, is_class_declaration,
    is_element_access_expression, is_enum_declaration, is_enum_member, is_function_declaration,
    is_identifier, is_in_js_file, is_jsdoc_property_like_tag, is_json_source_file,
    is_jsx_attribute, is_method_declaration, is_method_signature, is_numeric_literal,
    is_object_literal_method, is_omitted_expression, is_parameter, is_property_access_expression,
    is_property_assignment, is_property_declaration, is_property_signature,
    is_prototype_property_assignment, is_shorthand_property_assignment, is_source_file,
    is_string_literal_like, is_type_alias, is_variable_declaration, last_or_undefined, map,
    range_equals_rc, BaseInterfaceType, CheckFlags, Debug_, Diagnostics, ElementFlags,
    HasInitializerInterface, IndexInfo, InterfaceType, InterfaceTypeInterface,
    InterfaceTypeWithDeclaredMembersInterface, LiteralType, NamedDeclarationInterface, Node,
    NodeArray, NodeInterface, ObjectFlags, ObjectFlagsTypeInterface, ScriptTarget, Signature,
    SignatureFlags, Symbol, SymbolFlags, SymbolInterface, SymbolTable, SyntaxKind,
    TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface, TypeMapper,
    TypePredicate, TypeSystemPropertyName, UnderscoreEscapedMap, __String,
    maybe_append_if_unique_rc,
};

impl TypeChecker {
    pub(super) fn contains_same_named_this_property(
        &self,
        this_property: &Node, /*Expression*/
        expression: &Node,    /*Expression*/
    ) -> bool {
        is_property_access_expression(this_property)
            && this_property
                .as_property_access_expression()
                .expression
                .kind()
                == SyntaxKind::ThisKeyword
            && for_each_child_recursively_bool(
                expression,
                |n, _| self.is_matching_reference(this_property, n),
                Option::<fn(&NodeArray, &Node) -> bool>::None,
            )
    }

    pub(super) fn is_declaration_in_constructor(
        &self,
        expression: &Node, /*Expression*/
    ) -> bool {
        let this_container = get_this_container(expression, false);
        matches!(
            this_container.kind(),
            SyntaxKind::Constructor | SyntaxKind::FunctionDeclaration
        ) || this_container.kind() == SyntaxKind::FunctionExpression
            && !is_prototype_property_assignment(&this_container.parent())
    }

    pub(super) fn get_constructor_defined_this_assignment_types(
        &self,
        types: &[Rc<Type>],
        declarations: &[Rc<Node /*Declaration*/>],
    ) -> Option<Vec<Rc<Type>>> {
        Debug_.assert(types.len() == declarations.len(), None);
        Some(types.iter().enumerate().filter(|(i, _)| {
            let declaration = &declarations[*i];
            let expression = if is_binary_expression(declaration) {
                Some(declaration.node_wrapper())
            } else if is_binary_expression(&declaration.parent()) {
                Some(declaration.parent())
            } else {
                None
            };
            matches!(expression, Some(expression) if self.is_declaration_in_constructor(&expression))
        }).map(|(_, type_)| type_.clone()).collect())
    }

    pub(super) fn get_type_from_binding_element(
        &self,
        element: &Node, /*BindingElement*/
        include_pattern_in_type: Option<bool>,
        report_errors: Option<bool>,
    ) -> Rc<Type> {
        let element_as_binding_element = element.as_binding_element();
        if element_as_binding_element.maybe_initializer().is_some() {
            let contextual_type = if is_binding_pattern(Some(element_as_binding_element.name())) {
                self.get_type_from_binding_pattern(
                    &element_as_binding_element.name(),
                    Some(true),
                    Some(false),
                )
            } else {
                self.unknown_type()
            };
            return self.add_optionality(
                &self.widen_type_inferred_from_initializer(
                    element,
                    &self.check_declaration_initializer(element, Some(contextual_type)),
                ),
                None,
                None,
            );
        }
        if is_binding_pattern(Some(element_as_binding_element.name())) {
            return self.get_type_from_binding_pattern(
                &element_as_binding_element.name(),
                include_pattern_in_type,
                report_errors,
            );
        }
        if matches!(report_errors, Some(true))
            && !self.declaration_belongs_to_private_ambient_member(element)
        {
            self.report_implicit_any(element, &self.any_type(), None);
        }
        if matches!(include_pattern_in_type, Some(true)) {
            self.non_inferrable_any_type()
        } else {
            self.any_type()
        }
    }

    pub(super) fn get_type_from_object_binding_pattern(
        &self,
        pattern: &Node, /*ObjectBindingPattern*/
        include_pattern_in_type: bool,
        report_errors: bool,
    ) -> Rc<Type> {
        let mut members = create_symbol_table(None);
        let mut string_index_info: Option<Rc<IndexInfo>> = None;
        let mut object_flags =
            ObjectFlags::ObjectLiteral | ObjectFlags::ContainsObjectOrArrayLiteral;
        for_each(
            &pattern.as_object_binding_pattern().elements,
            |e: &Rc<Node>, _| {
                let e_as_binding_element = e.as_binding_element();
                let name = e_as_binding_element
                    .property_name
                    .as_ref()
                    .map_or_else(|| e_as_binding_element.name(), Clone::clone);
                if e_as_binding_element.dot_dot_dot_token.is_some() {
                    string_index_info = Some(Rc::new(self.create_index_info(
                        self.string_type(),
                        self.any_type(),
                        false,
                        None,
                    )));
                    return Option::<()>::None;
                }

                let expr_type = self.get_literal_type_from_property_name(&name);
                if !self.is_type_usable_as_property_name(&expr_type) {
                    object_flags |= ObjectFlags::ObjectLiteralPatternWithComputedProperties;
                    return Option::<()>::None;
                }
                let text = self.get_property_name_from_type(&expr_type);
                let flags = SymbolFlags::Property
                    | if e_as_binding_element.maybe_initializer().is_some() {
                        SymbolFlags::Optional
                    } else {
                        SymbolFlags::None
                    };
                let symbol: Rc<Symbol> = self.create_symbol(flags, text, None).into();
                symbol
                    .as_transient_symbol()
                    .symbol_links()
                    .borrow_mut()
                    .type_ = Some(self.get_type_from_binding_element(
                    e,
                    Some(include_pattern_in_type),
                    Some(report_errors),
                ));
                symbol
                    .as_transient_symbol()
                    .symbol_links()
                    .borrow_mut()
                    .binding_element = Some(e.clone());
                members.insert(symbol.escaped_name().clone(), symbol);
                Option::<()>::None
            },
        );
        let result: Rc<Type> = self
            .create_anonymous_type(
                Option::<&Symbol>::None,
                Rc::new(RefCell::new(members)),
                vec![],
                vec![],
                if let Some(string_index_info) = string_index_info {
                    vec![string_index_info]
                } else {
                    vec![]
                },
            )
            .into();
        let result_as_object_type = result.as_object_type();
        result_as_object_type.set_object_flags(result_as_object_type.object_flags() | object_flags);
        if include_pattern_in_type {
            *result.maybe_pattern() = Some(pattern.node_wrapper());
            result_as_object_type.set_object_flags(
                result_as_object_type.object_flags() | ObjectFlags::ContainsObjectOrArrayLiteral,
            );
        }
        result
    }

    pub(super) fn get_type_from_array_binding_pattern(
        &self,
        pattern: &Node, /*BindingPattern*/
        include_pattern_in_type: bool,
        report_errors: bool,
    ) -> Rc<Type> {
        let elements = pattern.as_has_elements().elements();
        let last_element = last_or_undefined(&**elements);
        let rest_element = last_element.filter(|last_element| {
            last_element.kind() == SyntaxKind::BindingElement
                && last_element
                    .as_binding_element()
                    .dot_dot_dot_token
                    .is_some()
        });
        if elements.is_empty() || elements.len() == 1 && rest_element.is_some() {
            return if self.language_version >= ScriptTarget::ES2015 {
                self.create_iterable_type(&self.any_type())
            } else {
                self.any_array_type()
            };
        }
        let element_types = map(Some(&**elements), |e: &Rc<Node>, _| {
            if is_omitted_expression(e) {
                self.any_type()
            } else {
                self.get_type_from_binding_element(
                    e,
                    Some(include_pattern_in_type),
                    Some(report_errors),
                )
            }
        })
        .unwrap();
        let min_length: usize = (find_last_index(
            &**elements,
            |e: &Rc<Node>, _| {
                !(matches!(rest_element.as_ref(), Some(rest_element) if Rc::ptr_eq(e, rest_element))
                    || is_omitted_expression(e)
                    || self.has_default_value(e))
            },
            Some(elements.len() - 1),
        ) + 1)
            .try_into()
            .unwrap();
        let element_flags = map(Some(&**elements), |e: &Rc<Node>, i| {
            if matches!(rest_element.as_ref(), Some(rest_element) if Rc::ptr_eq(e, rest_element)) {
                ElementFlags::Rest
            } else if i >= min_length {
                ElementFlags::Optional
            } else {
                ElementFlags::Required
            }
        })
        .unwrap();
        let mut result: Rc<Type> =
            self.create_tuple_type(&element_types, Some(&element_flags), None, None);
        if include_pattern_in_type {
            result = self.clone_type_reference(&result);
            *result.maybe_pattern() = Some(pattern.node_wrapper());
            let result_as_object_type = result.as_object_type();
            result_as_object_type.set_object_flags(
                result_as_object_type.object_flags() | ObjectFlags::ContainsObjectOrArrayLiteral,
            );
        }
        result
    }

    pub(super) fn get_type_from_binding_pattern(
        &self,
        pattern: &Node, /*BindingPattern*/
        include_pattern_in_type: Option<bool>,
        report_errors: Option<bool>,
    ) -> Rc<Type> {
        let include_pattern_in_type = include_pattern_in_type.unwrap_or(false);
        let report_errors = report_errors.unwrap_or(false);
        if pattern.kind() == SyntaxKind::ObjectBindingPattern {
            self.get_type_from_object_binding_pattern(
                pattern,
                include_pattern_in_type,
                report_errors,
            )
        } else {
            self.get_type_from_array_binding_pattern(
                pattern,
                include_pattern_in_type,
                report_errors,
            )
        }
    }

    pub(super) fn get_widened_type_for_variable_like_declaration(
        &self,
        declaration: &Node, /*ParameterDeclaration | PropertyDeclaration | PropertySignature | VariableDeclaration | BindingElement | JSDocPropertyLikeTag*/
        report_errors: Option<bool>,
    ) -> Rc<Type> {
        self.widen_type_for_variable_like_declaration(
            self.get_type_for_variable_like_declaration(declaration, true),
            declaration,
            report_errors,
        )
    }

    pub(super) fn is_global_symbol_constructor(&self, node: &Node) -> bool {
        let symbol = self.get_symbol_of_node(node);
        let global_symbol = self.get_global_es_symbol_constructor_type_symbol(false);
        matches!(
            (global_symbol, symbol),
            (Some(global_symbol), Some(symbol)) if Rc::ptr_eq(&symbol, &global_symbol)
        )
    }

    pub(super) fn widen_type_for_variable_like_declaration<TType: Borrow<Type>>(
        &self,
        type_: Option<TType>,
        declaration: &Node,
        report_errors: Option<bool>,
    ) -> Rc<Type> {
        let report_errors = report_errors.unwrap_or(false);
        if let Some(type_) = type_ {
            let mut type_ = type_.borrow().type_wrapper();
            if type_.flags().intersects(TypeFlags::ESSymbol)
                && self.is_global_symbol_constructor(&declaration.parent())
            {
                type_ = self.get_es_symbol_like_type_for_node(declaration);
            }
            if report_errors {
                self.report_errors_from_widening(declaration, &type_, None);
            }

            if type_.flags().intersects(TypeFlags::UniqueESSymbol)
                && (is_binding_element(declaration)
                    || declaration.as_has_type().maybe_type().is_none())
                && !matches!(
                    type_.maybe_symbol(),
                    Some(type_symbol) if matches!(self.get_symbol_of_node(declaration), Some(symbol_of_node) if Rc::ptr_eq(&type_symbol, &symbol_of_node))
                )
            {
                type_ = self.es_symbol_type();
            }

            return self.get_widened_type(&type_);
        }

        let type_ = if is_parameter(declaration)
            && declaration
                .as_parameter_declaration()
                .dot_dot_dot_token
                .is_some()
        {
            self.any_array_type()
        } else {
            self.any_type()
        };

        if report_errors {
            if !self.declaration_belongs_to_private_ambient_member(declaration) {
                self.report_implicit_any(declaration, &type_, None);
            }
        }
        type_
    }

    pub(super) fn declaration_belongs_to_private_ambient_member(
        &self,
        declaration: &Node, /*VariableLikeDeclaration*/
    ) -> bool {
        let root = get_root_declaration(declaration);
        let member_declaration = if root.kind() == SyntaxKind::Parameter {
            root.parent()
        } else {
            root
        };
        self.is_private_within_ambient(&member_declaration)
    }

    pub(super) fn try_get_type_from_effective_type_node(
        &self,
        declaration: &Node, /*Declaration*/
    ) -> Option<Rc<Type>> {
        let type_node = get_effective_type_annotation_node(declaration);
        type_node.map(|type_node| self.get_type_from_type_node_(&type_node))
    }

    pub(super) fn get_type_of_variable_or_parameter_or_property(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        let links_type_is_none = { (*links).borrow().type_.is_none() };
        if links_type_is_none {
            let type_ = self.get_type_of_variable_or_parameter_or_property_worker(symbol);
            let mut links = links.borrow_mut();
            if links.type_.is_none() {
                links.type_ = Some(type_);
            }
        }
        let links = (*links).borrow();
        links.type_.clone().unwrap()
    }

    pub(super) fn get_type_of_variable_or_parameter_or_property_worker(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type> {
        if symbol.flags().intersects(SymbolFlags::Prototype) {
            return self.get_type_of_prototype_property(symbol);
        }
        if ptr::eq(symbol, &*self.require_symbol()) {
            return self.any_type();
        }
        if symbol.flags().intersects(SymbolFlags::ModuleExports) {
            if let Some(symbol_value_declaration) = symbol.maybe_value_declaration() {
                let file_symbol = self
                    .get_symbol_of_node(
                        &get_source_file_of_node(Some(&*symbol_value_declaration)).unwrap(),
                    )
                    .unwrap();
                let result: Rc<Symbol> = self
                    .create_symbol(
                        file_symbol.flags(),
                        __String::new("exports".to_owned()),
                        None,
                    )
                    .into();
                result.set_declarations(
                    file_symbol
                        .maybe_declarations()
                        .as_ref()
                        .map_or_else(|| vec![], Clone::clone),
                );
                result.set_parent(Some(symbol.symbol_wrapper()));
                result
                    .as_transient_symbol()
                    .symbol_links()
                    .borrow_mut()
                    .target = Some(file_symbol.clone());
                if let Some(file_symbol_value_declaration) = file_symbol.maybe_value_declaration() {
                    result.set_value_declaration(file_symbol_value_declaration);
                }
                if let Some(file_symbol_members) = file_symbol.maybe_members().as_ref() {
                    *result.maybe_members() = Some(Rc::new(RefCell::new(
                        RefCell::borrow(file_symbol_members).clone(),
                    )));
                }
                if let Some(file_symbol_exports) = file_symbol.maybe_exports().as_ref() {
                    *result.maybe_exports() = Some(Rc::new(RefCell::new(
                        RefCell::borrow(file_symbol_exports).clone(),
                    )));
                }
                let mut members = create_symbol_table(None);
                members.insert(__String::new("exports".to_owned()), result);
                return self
                    .create_anonymous_type(
                        Some(symbol),
                        Rc::new(RefCell::new(members)),
                        vec![],
                        vec![],
                        vec![],
                    )
                    .into();
            }
        }
        Debug_.assert_is_defined(&symbol.maybe_value_declaration(), None);
        let declaration = symbol.maybe_value_declaration();
        let declaration = declaration.as_deref().unwrap();
        if is_catch_clause_variable_declaration_or_binding_element(declaration) {
            let type_node = get_effective_type_annotation_node(declaration);
            if type_node.is_none() {
                return if self.use_unknown_in_catch_variables {
                    self.unknown_type()
                } else {
                    self.any_type()
                };
            }
            let type_node = type_node.unwrap();
            let type_ = self.get_type_of_node(&type_node);
            return if self.is_type_any(Some(&*type_)) || Rc::ptr_eq(&type_, &self.unknown_type()) {
                type_
            } else {
                self.error_type()
            };
        }
        if is_source_file(declaration) && is_json_source_file(declaration) {
            let declaration_as_source_file = declaration.as_source_file();
            if declaration_as_source_file.statements.is_empty() {
                return self.empty_object_type();
            }
            return self.get_widened_type(
                &self.get_widened_literal_type(
                    &self.check_expression(
                        &declaration_as_source_file.statements[0]
                            .as_expression_statement()
                            .expression,
                        None,
                        None,
                    ),
                ),
            );
        }

        if !self.push_type_resolution(
            &symbol.symbol_wrapper().into(),
            TypeSystemPropertyName::Type,
        ) {
            if symbol.flags().intersects(SymbolFlags::ValueModule)
                && !symbol.flags().intersects(SymbolFlags::Assignment)
            {
                return self.get_type_of_func_class_enum_module(symbol);
            }
            return self.report_circularity_error(symbol);
        }
        let type_: Rc<Type>;
        if declaration.kind() == SyntaxKind::ExportAssignment {
            type_ = self.widen_type_for_variable_like_declaration(
                Some(
                    self.try_get_type_from_effective_type_node(declaration)
                        .unwrap_or_else(|| {
                            self.check_expression_cached(
                                &declaration.as_export_assignment().expression,
                                None,
                            )
                        }),
                ),
                declaration,
                None,
            );
        } else if is_binary_expression(declaration)
            || is_in_js_file(Some(declaration))
                && (is_call_expression(declaration)
                    || (is_property_access_expression(declaration)
                        || is_bindable_static_element_access_expression(declaration, None))
                        && is_binary_expression(&declaration.parent()))
        {
            type_ =
                self.get_widened_type_for_assignment_declaration(symbol, Option::<&Symbol>::None);
        } else if is_property_access_expression(declaration)
            || is_element_access_expression(declaration)
            || is_identifier(declaration)
            || is_string_literal_like(declaration)
            || is_numeric_literal(declaration)
            || is_class_declaration(declaration)
            || is_function_declaration(declaration)
            || (is_method_declaration(declaration) && !is_object_literal_method(declaration))
            || is_method_signature(declaration)
            || is_source_file(declaration)
        {
            if symbol.flags().intersects(
                SymbolFlags::Function
                    | SymbolFlags::Method
                    | SymbolFlags::Class
                    | SymbolFlags::Enum
                    | SymbolFlags::ValueModule,
            ) {
                return self.get_type_of_func_class_enum_module(symbol);
            }
            type_ = if is_binary_expression(&declaration.parent()) {
                self.get_widened_type_for_assignment_declaration(symbol, Option::<&Symbol>::None)
            } else {
                self.try_get_type_from_effective_type_node(declaration)
                    .unwrap_or_else(|| self.any_type())
            };
        } else if is_property_assignment(declaration) {
            type_ = self
                .try_get_type_from_effective_type_node(declaration)
                .unwrap_or_else(|| self.check_property_assignment(declaration, None));
        } else if is_jsx_attribute(declaration) {
            type_ = self
                .try_get_type_from_effective_type_node(declaration)
                .unwrap_or_else(|| self.check_jsx_attribute(declaration, None));
        } else if is_shorthand_property_assignment(declaration) {
            type_ = self
                .try_get_type_from_effective_type_node(declaration)
                .unwrap_or_else(|| {
                    self.check_expression_for_mutable_location(
                        &declaration.as_shorthand_property_assignment().name(),
                        Some(CheckMode::Normal),
                        Option::<&Type>::None,
                        None,
                    )
                });
        } else if is_object_literal_method(declaration) {
            type_ = self
                .try_get_type_from_effective_type_node(declaration)
                .unwrap_or_else(|| {
                    self.check_object_literal_method(declaration, Some(CheckMode::Normal))
                });
        } else if is_parameter(declaration)
            || is_property_declaration(declaration)
            || is_property_signature(declaration)
            || is_variable_declaration(declaration)
            || is_binding_element(declaration)
            || is_jsdoc_property_like_tag(declaration)
        {
            type_ = self.get_widened_type_for_variable_like_declaration(declaration, Some(true));
        } else if is_enum_declaration(declaration) {
            type_ = self.get_type_of_func_class_enum_module(symbol);
        } else if is_enum_member(declaration) {
            type_ = self.get_type_of_enum_member(symbol);
        } else if is_accessor(declaration) {
            type_ = self
                .resolve_type_of_accessors(symbol, None)
                .unwrap_or_else(|| {
                    Debug_.fail(Some(
                        "Non-write accessor resolution must always produce a type",
                    ))
                });
        } else {
            Debug_.fail(Some(&format!(
                "Unhandled declaration kind! {} for {}",
                Debug_.format_syntax_kind(Some(declaration.kind())),
                Debug_.format_symbol(symbol)
            )));
        }

        if !self.pop_type_resolution() {
            if symbol.flags().intersects(SymbolFlags::ValueModule)
                && !symbol.flags().intersects(SymbolFlags::Assignment)
            {
                return self.get_type_of_func_class_enum_module(symbol);
            }
            return self.report_circularity_error(symbol);
        }
        type_
    }

    pub(super) fn get_annotated_accessor_type_node<TAccessor: Borrow<Node>>(
        &self,
        accessor: Option<TAccessor /*AccessorDeclaration*/>,
    ) -> Option<Rc<Node /*TypeNode*/>> {
        let accessor = accessor?;
        let accessor = accessor.borrow();
        if accessor.kind() == SyntaxKind::GetAccessor {
            let getter_type_annotation = get_effective_return_type_node(accessor);
            return getter_type_annotation;
        } else {
            let setter_type_annotation = get_effective_set_accessor_type_annotation_node(accessor);
            return setter_type_annotation;
        }
    }

    pub(super) fn get_annotated_accessor_type<TAccessor: Borrow<Node>>(
        &self,
        accessor: Option<TAccessor /*AccessorDeclaration*/>,
    ) -> Option<Rc<Type>> {
        let node = self.get_annotated_accessor_type_node(accessor)?;
        Some(self.get_type_from_type_node_(&node))
    }

    pub(super) fn get_annotated_accessor_this_parameter(
        &self,
        accessor: &Node, /*AccessorDeclaration*/
    ) -> Option<Rc<Symbol>> {
        let parameter = self.get_accessor_this_parameter(accessor)?;
        parameter.maybe_symbol()
    }

    pub(super) fn get_this_type_of_declaration(
        &self,
        declaration: &Node, /*SignatureDeclaration*/
    ) -> Option<Rc<Type>> {
        self.get_this_type_of_signature(&self.get_signature_from_declaration_(declaration))
    }

    pub(super) fn get_type_of_accessors(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        if let Some(links_type) = (*links).borrow().type_.clone() {
            return links_type;
        }
        let ret = self
            .get_type_of_accessors_worker(symbol, None)
            .unwrap_or_else(|| {
                Debug_.fail(Some("Read type of accessor must always produce a type"))
            });
        links.borrow_mut().type_ = Some(ret.clone());
        ret
    }

    pub(super) fn get_type_of_set_accessor(&self, symbol: &Symbol) -> Option<Rc<Type>> {
        let links = self.get_symbol_links(symbol);
        if let Some(links_write_type) = (*links).borrow().write_type.clone() {
            return Some(links_write_type);
        }
        let ret = self.get_type_of_accessors_worker(symbol, Some(true));
        links.borrow_mut().write_type = ret.clone();
        ret
    }

    pub(super) fn get_type_of_accessors_worker(
        &self,
        symbol: &Symbol,
        writing: Option<bool>,
    ) -> Option<Rc<Type>> {
        if !self.push_type_resolution(
            &symbol.symbol_wrapper().into(),
            TypeSystemPropertyName::Type,
        ) {
            return Some(self.error_type());
        }

        let mut type_ = self.resolve_type_of_accessors(symbol, writing);

        if !self.pop_type_resolution() {
            type_ = Some(self.any_type());
            if self.no_implicit_any {
                let getter = get_declaration_of_kind(symbol, SyntaxKind::GetAccessor);
                self.error(
                    getter,
                    &Diagnostics::_0_implicitly_has_return_type_any_because_it_does_not_have_a_return_type_annotation_and_is_referenced_directly_or_indirectly_in_one_of_its_return_expressions,
                    Some(vec![
                        self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None)
                    ])
                );
            }
        }
        type_
    }

    pub(super) fn resolve_type_of_accessors(
        &self,
        symbol: &Symbol,
        writing: Option<bool>,
    ) -> Option<Rc<Type>> {
        let writing = writing.unwrap_or(false);
        let getter = get_declaration_of_kind(symbol, SyntaxKind::GetAccessor);
        let setter = get_declaration_of_kind(symbol, SyntaxKind::SetAccessor);

        let setter_type = self.get_annotated_accessor_type(setter.as_deref());

        if writing {
            if let Some(setter_type) = setter_type.as_ref() {
                return Some(self.instantiate_type_if_needed(setter_type, symbol));
            }
        }

        if let Some(getter) = getter.as_deref() {
            if is_in_js_file(Some(getter)) {
                let js_doc_type = self.get_type_for_declaration_from_jsdoc_comment(getter);
                if let Some(js_doc_type) = js_doc_type {
                    return Some(self.instantiate_type_if_needed(&js_doc_type, symbol));
                }
            }
        }

        let getter_type = self.get_annotated_accessor_type(getter.as_deref());
        if let Some(getter_type) = getter_type.as_ref() {
            return Some(self.instantiate_type_if_needed(getter_type, symbol));
        }

        if setter_type.is_some() {
            return setter_type;
        }

        if let Some(getter) = getter.as_ref() {
            if getter.as_function_like_declaration().maybe_body().is_some() {
                let return_type_from_body = self.get_return_type_from_body(getter, None);
                return Some(self.instantiate_type_if_needed(&return_type_from_body, symbol));
            }
        }

        if let Some(setter) = setter.as_ref() {
            if !self.is_private_within_ambient(setter) {
                self.error_or_suggestion(
                    self.no_implicit_any,
                    setter,
                    Diagnostics::Property_0_implicitly_has_type_any_because_its_set_accessor_lacks_a_parameter_type_annotation.clone().into(),
                    Some(vec![
                        self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None)
                    ])
                );
            }
            return Some(self.any_type());
        } else if let Some(getter) = getter.as_ref() {
            // Debug.assert(!!getter, ...);
            if !self.is_private_within_ambient(getter) {
                self.error_or_suggestion(
                    self.no_implicit_any,
                    getter,
                    Diagnostics::Property_0_implicitly_has_type_any_because_its_get_accessor_lacks_a_return_type_annotation.clone().into(),
                    Some(vec![
                        self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None)
                    ])
                );
            }
            return Some(self.any_type());
        }
        None
    }

    pub(super) fn instantiate_type_if_needed(&self, type_: &Type, symbol: &Symbol) -> Rc<Type> {
        if get_check_flags(symbol).intersects(CheckFlags::Instantiated) {
            let links = self.get_symbol_links(symbol);
            return self
                .instantiate_type(Some(type_), (*links).borrow().mapper.as_ref())
                .unwrap();
        }

        type_.type_wrapper()
    }

    pub(super) fn get_base_type_variable_of_class(&self, symbol: &Symbol) -> Option<Rc<Type>> {
        let base_constructor_type = self.get_base_constructor_type_of_class(
            &self.get_declared_type_of_class_or_interface(symbol),
        );
        if base_constructor_type
            .flags()
            .intersects(TypeFlags::TypeVariable)
        {
            Some(base_constructor_type)
        } else if base_constructor_type
            .flags()
            .intersects(TypeFlags::Intersection)
        {
            find(
                base_constructor_type
                    .as_union_or_intersection_type_interface()
                    .types(),
                |t: &Rc<Type>, _| t.flags().intersects(TypeFlags::TypeVariable),
            )
            .map(Clone::clone)
        } else {
            None
        }
    }

    pub(super) fn get_type_of_func_class_enum_module(&self, symbol: &Symbol) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_of_enum_member(&self, symbol: &Symbol) -> Rc<Type> {
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

    pub(super) fn report_circularity_error(&self, symbol: &Symbol) -> Rc<Type> {
        unimplemented!()
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

    pub(super) fn get_base_constructor_type_of_class(
        &self,
        type_: &Type, /*InterfaceType*/
    ) -> Rc<Type> {
        unimplemented!()
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
