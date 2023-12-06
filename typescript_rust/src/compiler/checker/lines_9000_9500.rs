use std::{borrow::Borrow, convert::TryInto, io, ptr};

use gc::{Gc, GcCell};
use id_arena::Id;

use super::CheckMode;
use crate::{
    are_option_gcs_equal, create_symbol_table, find_last_index_returns_isize, get_check_flags,
    get_declaration_of_kind, get_effective_return_type_node,
    get_effective_set_accessor_type_annotation_node, get_effective_type_annotation_node,
    get_root_declaration, get_source_file_of_node, get_this_container, is_accessor,
    is_binary_expression, is_bindable_static_element_access_expression, is_binding_element,
    is_binding_pattern, is_call_expression,
    is_catch_clause_variable_declaration_or_binding_element, is_class_declaration,
    is_element_access_expression, is_enum_declaration, is_enum_member, is_function_declaration,
    is_identifier, is_in_js_file, is_jsdoc_property_like_tag, is_json_source_file,
    is_jsx_attribute, is_method_declaration, is_method_signature, is_numeric_literal,
    is_object_literal_method, is_omitted_expression, is_parameter, is_property_access_expression,
    is_property_assignment, is_property_declaration, is_property_signature,
    is_prototype_property_assignment, is_shorthand_property_assignment, is_source_file,
    is_string_literal_like, is_variable_declaration, last_or_undefined, map,
    return_ok_none_if_none, try_for_each, try_for_each_child_recursively_bool, try_map, CheckFlags,
    Debug_, Diagnostics, ElementFlags, HasInitializerInterface, HasStatementsInterface, IndexInfo,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, OptionTry, ScriptTarget, Symbol, SymbolFlags, SymbolInterface,
    SyntaxKind, TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface,
    TypeSystemPropertyName,
};

impl TypeChecker {
    pub(super) fn contains_same_named_this_property(
        &self,
        this_property: &Node, /*Expression*/
        expression: &Node,    /*Expression*/
    ) -> io::Result<bool> {
        Ok(is_property_access_expression(this_property)
            && this_property
                .as_property_access_expression()
                .expression
                .kind()
                == SyntaxKind::ThisKeyword
            && try_for_each_child_recursively_bool(
                expression,
                |n, _| self.is_matching_reference(this_property, n),
                Option::<fn(&NodeArray, &Node) -> io::Result<bool>>::None,
            )?)
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
        types: &[Id<Type>],
        declarations: &[Gc<Node /*Declaration*/>],
    ) -> Option<Vec<Id<Type>>> {
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
    ) -> io::Result<Id<Type>> {
        let element_as_binding_element = element.as_binding_element();
        if element_as_binding_element.maybe_initializer().is_some() {
            let contextual_type = if is_binding_pattern(Some(element_as_binding_element.name())) {
                self.get_type_from_binding_pattern(
                    &element_as_binding_element.name(),
                    Some(true),
                    Some(false),
                )?
            } else {
                self.unknown_type()
            };
            return self.add_optionality(
                self.widen_type_inferred_from_initializer(
                    element,
                    self.check_declaration_initializer(element, Some(contextual_type))?,
                )?,
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
            self.report_implicit_any(element, self.any_type(), None)?;
        }
        Ok(if matches!(include_pattern_in_type, Some(true)) {
            self.non_inferrable_any_type()
        } else {
            self.any_type()
        })
    }

    pub(super) fn get_type_from_object_binding_pattern(
        &self,
        pattern: &Node, /*ObjectBindingPattern*/
        include_pattern_in_type: bool,
        report_errors: bool,
    ) -> io::Result<Id<Type>> {
        let mut members = create_symbol_table(Option::<&[Gc<Symbol>]>::None);
        let mut string_index_info: Option<Gc<IndexInfo>> = None;
        let mut object_flags =
            ObjectFlags::ObjectLiteral | ObjectFlags::ContainsObjectOrArrayLiteral;
        try_for_each(
            &pattern.as_object_binding_pattern().elements,
            |e: &Gc<Node>, _| -> io::Result<_> {
                let e_as_binding_element = e.as_binding_element();
                let name = e_as_binding_element
                    .property_name
                    .as_ref()
                    .map_or_else(|| e_as_binding_element.name(), Clone::clone);
                if e_as_binding_element.dot_dot_dot_token.is_some() {
                    string_index_info = Some(Gc::new(self.create_index_info(
                        self.string_type(),
                        self.any_type(),
                        false,
                        None,
                    )));
                    return Ok(Option::<()>::None);
                }

                let expr_type = self.get_literal_type_from_property_name(&name)?;
                if !self.is_type_usable_as_property_name(expr_type) {
                    object_flags |= ObjectFlags::ObjectLiteralPatternWithComputedProperties;
                    return Ok(Option::<()>::None);
                }
                let text = self.get_property_name_from_type(expr_type);
                let flags = SymbolFlags::Property
                    | if e_as_binding_element.maybe_initializer().is_some() {
                        SymbolFlags::Optional
                    } else {
                        SymbolFlags::None
                    };
                let symbol: Gc<Symbol> = self.create_symbol(flags, text, None).into();
                symbol
                    .as_transient_symbol()
                    .symbol_links()
                    .borrow_mut()
                    .type_ = Some(self.get_type_from_binding_element(
                    e,
                    Some(include_pattern_in_type),
                    Some(report_errors),
                )?);
                symbol
                    .as_transient_symbol()
                    .symbol_links()
                    .borrow_mut()
                    .binding_element = Some(e.clone());
                members.insert(symbol.escaped_name().to_owned(), symbol);
                Ok(Option::<()>::None)
            },
        )?;
        let result = self.create_anonymous_type(
            Option::<&Symbol>::None,
            Gc::new(GcCell::new(members)),
            vec![],
            vec![],
            if let Some(string_index_info) = string_index_info {
                vec![string_index_info]
            } else {
                vec![]
            },
        )?;
        self.type_(result)
            .as_object_type()
            .set_object_flags(self.type_(result).as_object_type().object_flags() | object_flags);
        if include_pattern_in_type {
            *self.type_(result).maybe_pattern() = Some(pattern.node_wrapper());
            self.type_(result).as_object_type().set_object_flags(
                self.type_(result).as_object_type().object_flags() | ObjectFlags::ContainsObjectOrArrayLiteral,
            );
        }
        Ok(result)
    }

    pub(super) fn get_type_from_array_binding_pattern(
        &self,
        pattern: &Node, /*BindingPattern*/
        include_pattern_in_type: bool,
        report_errors: bool,
    ) -> io::Result<Id<Type>> {
        let ref elements = pattern.as_has_elements().elements();
        let last_element = last_or_undefined(&**elements);
        let rest_element = last_element.filter(|last_element| {
            last_element.kind() == SyntaxKind::BindingElement
                && last_element
                    .as_binding_element()
                    .dot_dot_dot_token
                    .is_some()
        });
        if elements.is_empty() || elements.len() == 1 && rest_element.is_some() {
            return Ok(if self.language_version >= ScriptTarget::ES2015 {
                self.create_iterable_type(self.any_type())?
            } else {
                self.any_array_type()
            });
        }
        let element_types = try_map(elements, |e: &Gc<Node>, _| -> io::Result<_> {
            Ok(if is_omitted_expression(e) {
                self.any_type()
            } else {
                self.get_type_from_binding_element(
                    e,
                    Some(include_pattern_in_type),
                    Some(report_errors),
                )?
            })
        })?;
        let min_length: usize = (find_last_index_returns_isize(
            &**elements,
            |e: &Gc<Node>, _| {
                !(matches!(rest_element.as_ref(), Some(rest_element) if Gc::ptr_eq(e, rest_element))
                    || is_omitted_expression(e)
                    || self.has_default_value(e))
            },
            Some(elements.len() - 1),
        ) + 1)
            .try_into()
            .unwrap();
        let element_flags = map(elements, |e: &Gc<Node>, i| {
            if matches!(rest_element.as_ref(), Some(rest_element) if Gc::ptr_eq(e, rest_element)) {
                ElementFlags::Rest
            } else if i >= min_length {
                ElementFlags::Optional
            } else {
                ElementFlags::Required
            }
        });
        let mut result: Id<Type> =
            self.create_tuple_type(&element_types, Some(&element_flags), None, None)?;
        if include_pattern_in_type {
            result = self.clone_type_reference(result);
            *self.type_(result).maybe_pattern() = Some(pattern.node_wrapper());
            self.type_(result).as_object_type().set_object_flags(
                self.type_(result).as_object_type().object_flags() | ObjectFlags::ContainsObjectOrArrayLiteral,
            );
        }
        Ok(result)
    }

    pub(super) fn get_type_from_binding_pattern(
        &self,
        pattern: &Node, /*BindingPattern*/
        include_pattern_in_type: Option<bool>,
        report_errors: Option<bool>,
    ) -> io::Result<Id<Type>> {
        let include_pattern_in_type = include_pattern_in_type.unwrap_or(false);
        let report_errors = report_errors.unwrap_or(false);
        Ok(if pattern.kind() == SyntaxKind::ObjectBindingPattern {
            self.get_type_from_object_binding_pattern(
                pattern,
                include_pattern_in_type,
                report_errors,
            )?
        } else {
            self.get_type_from_array_binding_pattern(
                pattern,
                include_pattern_in_type,
                report_errors,
            )?
        })
    }

    pub(super) fn get_widened_type_for_variable_like_declaration(
        &self,
        declaration: &Node, /*ParameterDeclaration | PropertyDeclaration | PropertySignature | VariableDeclaration | BindingElement | JSDocPropertyLikeTag*/
        report_errors: Option<bool>,
    ) -> io::Result<Id<Type>> {
        self.widen_type_for_variable_like_declaration(
            self.get_type_for_variable_like_declaration(declaration, true)?,
            declaration,
            report_errors,
        )
    }

    pub(super) fn is_global_symbol_constructor(&self, node: &Node) -> io::Result<bool> {
        let symbol = self.get_symbol_of_node(node)?;
        let global_symbol = self.get_global_es_symbol_constructor_type_symbol(false)?;
        Ok(matches!(
            (global_symbol, symbol),
            (Some(global_symbol), Some(symbol)) if Gc::ptr_eq(&symbol, &global_symbol)
        ))
    }

    pub(super) fn widen_type_for_variable_like_declaration(
        &self,
        type_: Option<Id<Type>>,
        declaration: &Node,
        report_errors: Option<bool>,
    ) -> io::Result<Id<Type>> {
        let report_errors = report_errors.unwrap_or(false);
        if let Some(mut type_) = type_ {
            if self.type_(type_).flags().intersects(TypeFlags::ESSymbol)
                && self.is_global_symbol_constructor(&declaration.parent())?
            {
                type_ = self.get_es_symbol_like_type_for_node(declaration)?;
            }
            if report_errors {
                self.report_errors_from_widening(declaration, &type_, None)?;
            }

            if self.type_(type_).flags().intersects(TypeFlags::UniqueESSymbol)
                && (is_binding_element(declaration)
                    || declaration.as_has_type().maybe_type().is_none())
                && !are_option_gcs_equal(
                    type_.maybe_symbol().as_ref(),
                    self.get_symbol_of_node(declaration)?.as_ref(),
                )
            {
                type_ = self.es_symbol_type();
            }

            return self.get_widened_type(type_);
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
                self.report_implicit_any(declaration, type_, None)?;
            }
        }
        Ok(type_)
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
    ) -> io::Result<Option<Id<Type>>> {
        let type_node = get_effective_type_annotation_node(declaration);
        type_node.try_map(|type_node| self.get_type_from_type_node_(&type_node))
    }

    pub(super) fn get_type_of_variable_or_parameter_or_property(
        &self,
        symbol: &Symbol,
    ) -> io::Result<Id<Type>> {
        let links = self.get_symbol_links(symbol);
        let links_type_is_none = { (*links).borrow().type_.is_none() };
        if links_type_is_none {
            let type_ = self.get_type_of_variable_or_parameter_or_property_worker(symbol)?;
            let mut links = links.borrow_mut();
            if links.type_.is_none() {
                links.type_ = Some(type_);
            }
        }
        let links = (*links).borrow();
        Ok(links.type_.clone().unwrap())
    }

    pub(super) fn get_type_of_variable_or_parameter_or_property_worker(
        &self,
        symbol: &Symbol,
    ) -> io::Result<Id<Type>> {
        if symbol.flags().intersects(SymbolFlags::Prototype) {
            return self.get_type_of_prototype_property(symbol);
        }
        if ptr::eq(symbol, &*self.require_symbol()) {
            return Ok(self.any_type());
        }
        if symbol.flags().intersects(SymbolFlags::ModuleExports) {
            if let Some(symbol_value_declaration) = symbol.maybe_value_declaration() {
                let file_symbol = self
                    .get_symbol_of_node(&get_source_file_of_node(&symbol_value_declaration))?
                    .unwrap();
                let result: Gc<Symbol> = self
                    .create_symbol(file_symbol.flags(), "exports".to_owned(), None)
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
                    *result.maybe_members_mut() = Some(Gc::new(GcCell::new(
                        (**file_symbol_members).borrow().clone(),
                    )));
                }
                if let Some(file_symbol_exports) = file_symbol.maybe_exports().as_ref() {
                    *result.maybe_exports_mut() = Some(Gc::new(GcCell::new(
                        (**file_symbol_exports).borrow().clone(),
                    )));
                }
                let mut members = create_symbol_table(Option::<&[Gc<Symbol>]>::None);
                members.insert("exports".to_owned(), result);
                return self.create_anonymous_type(
                    Some(symbol),
                    Gc::new(GcCell::new(members)),
                    vec![],
                    vec![],
                    vec![],
                );
            }
        }
        Debug_.assert_is_defined(&symbol.maybe_value_declaration(), None);
        let declaration = symbol.maybe_value_declaration();
        let declaration = declaration.as_deref().unwrap();
        if is_catch_clause_variable_declaration_or_binding_element(declaration) {
            let type_node = get_effective_type_annotation_node(declaration);
            if type_node.is_none() {
                return Ok(if self.use_unknown_in_catch_variables {
                    self.unknown_type()
                } else {
                    self.any_type()
                });
            }
            let type_node = type_node.unwrap();
            let type_ = self.get_type_of_node(&type_node)?;
            return Ok(
                if self.is_type_any(Some(type_)) || type_ == self.unknown_type() {
                    type_
                } else {
                    self.error_type()
                },
            );
        }
        if is_source_file(declaration) && is_json_source_file(declaration) {
            let declaration_as_source_file = declaration.as_source_file();
            if declaration_as_source_file.statements().is_empty() {
                return Ok(self.empty_object_type());
            }
            return self.get_widened_type(
                self.get_widened_literal_type(
                    self.check_expression(
                        &declaration_as_source_file.statements()[0]
                            .as_expression_statement()
                            .expression,
                        None,
                        None,
                    )?,
                )?,
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
        let type_: Id<Type>;
        if declaration.kind() == SyntaxKind::ExportAssignment {
            type_ = self.widen_type_for_variable_like_declaration(
                Some(
                    self.try_get_type_from_effective_type_node(declaration)?
                        .try_unwrap_or_else(|| {
                            self.check_expression_cached(
                                &declaration.as_export_assignment().expression,
                                None,
                            )
                        })?,
                ),
                declaration,
                None,
            )?;
        } else if is_binary_expression(declaration)
            || is_in_js_file(Some(declaration))
                && (is_call_expression(declaration)
                    || (is_property_access_expression(declaration)
                        || is_bindable_static_element_access_expression(declaration, None))
                        && is_binary_expression(&declaration.parent()))
        {
            type_ =
                self.get_widened_type_for_assignment_declaration(symbol, Option::<&Symbol>::None)?;
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
                self.get_widened_type_for_assignment_declaration(symbol, Option::<&Symbol>::None)?
            } else {
                self.try_get_type_from_effective_type_node(declaration)?
                    .unwrap_or_else(|| self.any_type())
            };
        } else if is_property_assignment(declaration) {
            type_ = self
                .try_get_type_from_effective_type_node(declaration)?
                .try_unwrap_or_else(|| self.check_property_assignment(declaration, None))?;
        } else if is_jsx_attribute(declaration) {
            type_ = self
                .try_get_type_from_effective_type_node(declaration)?
                .try_unwrap_or_else(|| self.check_jsx_attribute(declaration, None))?;
        } else if is_shorthand_property_assignment(declaration) {
            type_ = self
                .try_get_type_from_effective_type_node(declaration)?
                .try_unwrap_or_else(|| {
                    self.check_expression_for_mutable_location(
                        &declaration.as_shorthand_property_assignment().name(),
                        Some(CheckMode::Normal),
                        None,
                        None,
                    )
                })?;
        } else if is_object_literal_method(declaration) {
            type_ = self
                .try_get_type_from_effective_type_node(declaration)?
                .try_unwrap_or_else(|| {
                    self.check_object_literal_method(declaration, Some(CheckMode::Normal))
                })?;
        } else if is_parameter(declaration)
            || is_property_declaration(declaration)
            || is_property_signature(declaration)
            || is_variable_declaration(declaration)
            || is_binding_element(declaration)
            || is_jsdoc_property_like_tag(declaration)
        {
            type_ = self.get_widened_type_for_variable_like_declaration(declaration, Some(true))?;
        } else if is_enum_declaration(declaration) {
            type_ = self.get_type_of_func_class_enum_module(symbol)?;
        } else if is_enum_member(declaration) {
            type_ = self.get_type_of_enum_member(symbol)?;
        } else if is_accessor(declaration) {
            type_ = self
                .resolve_type_of_accessors(symbol, None)?
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
        Ok(type_)
    }

    pub(super) fn get_annotated_accessor_type_node(
        &self,
        accessor: Option<impl Borrow<Node> /*AccessorDeclaration*/>,
    ) -> Option<Gc<Node /*TypeNode*/>> {
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

    pub(super) fn get_annotated_accessor_type(
        &self,
        accessor: Option<impl Borrow<Node> /*AccessorDeclaration*/>,
    ) -> io::Result<Option<Id<Type>>> {
        let node = return_ok_none_if_none!(self.get_annotated_accessor_type_node(accessor));
        Ok(Some(self.get_type_from_type_node_(&node)?))
    }

    pub(super) fn get_annotated_accessor_this_parameter(
        &self,
        accessor: &Node, /*AccessorDeclaration*/
    ) -> Option<Gc<Symbol>> {
        let parameter = self.get_accessor_this_parameter(accessor)?;
        parameter.maybe_symbol()
    }

    pub(super) fn get_this_type_of_declaration(
        &self,
        declaration: &Node, /*SignatureDeclaration*/
    ) -> io::Result<Option<Id<Type>>> {
        self.get_this_type_of_signature(&*self.get_signature_from_declaration_(declaration)?)
    }

    pub(super) fn get_type_of_accessors(&self, symbol: &Symbol) -> io::Result<Id<Type>> {
        let links = self.get_symbol_links(symbol);
        if let Some(links_type) = (*links).borrow().type_.clone() {
            return Ok(links_type);
        }
        let ret = self
            .get_type_of_accessors_worker(symbol, None)?
            .unwrap_or_else(|| {
                Debug_.fail(Some("Read type of accessor must always produce a type"))
            });
        links.borrow_mut().type_ = Some(ret.clone());
        Ok(ret)
    }

    pub(super) fn get_type_of_set_accessor(&self, symbol: &Symbol) -> io::Result<Option<Id<Type>>> {
        let links = self.get_symbol_links(symbol);
        if let Some(links_write_type) = (*links).borrow().write_type.clone() {
            return Ok(Some(links_write_type));
        }
        let ret = self.get_type_of_accessors_worker(symbol, Some(true))?;
        links.borrow_mut().write_type = ret.clone();
        Ok(ret)
    }

    pub(super) fn get_type_of_accessors_worker(
        &self,
        symbol: &Symbol,
        writing: Option<bool>,
    ) -> io::Result<Option<Id<Type>>> {
        if !self.push_type_resolution(
            &symbol.symbol_wrapper().into(),
            TypeSystemPropertyName::Type,
        ) {
            return Ok(Some(self.error_type()));
        }

        let mut type_ = self.resolve_type_of_accessors(symbol, writing)?;

        if !self.pop_type_resolution() {
            type_ = Some(self.any_type());
            if self.no_implicit_any {
                let getter = get_declaration_of_kind(symbol, SyntaxKind::GetAccessor);
                self.error(
                    getter,
                    &Diagnostics::_0_implicitly_has_return_type_any_because_it_does_not_have_a_return_type_annotation_and_is_referenced_directly_or_indirectly_in_one_of_its_return_expressions,
                    Some(vec![
                        self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None)?
                    ])
                );
            }
        }
        Ok(type_)
    }

    pub(super) fn resolve_type_of_accessors(
        &self,
        symbol: &Symbol,
        writing: Option<bool>,
    ) -> io::Result<Option<Id<Type>>> {
        let writing = writing.unwrap_or(false);
        let getter = get_declaration_of_kind(symbol, SyntaxKind::GetAccessor);
        let setter = get_declaration_of_kind(symbol, SyntaxKind::SetAccessor);

        let setter_type = self.get_annotated_accessor_type(setter.as_deref())?;

        if writing {
            if let Some(setter_type) = setter_type {
                return Ok(Some(self.instantiate_type_if_needed(setter_type, symbol)?));
            }
        }

        if let Some(getter) = getter.as_deref() {
            if is_in_js_file(Some(getter)) {
                let js_doc_type = self.get_type_for_declaration_from_jsdoc_comment(getter)?;
                if let Some(js_doc_type) = js_doc_type {
                    return Ok(Some(self.instantiate_type_if_needed(js_doc_type, symbol)?));
                }
            }
        }

        let getter_type = self.get_annotated_accessor_type(getter.as_deref())?;
        if let Some(getter_type) = getter_type {
            return Ok(Some(self.instantiate_type_if_needed(getter_type, symbol)?));
        }

        if setter_type.is_some() {
            return Ok(setter_type);
        }

        if let Some(getter) = getter.as_ref() {
            if getter.as_function_like_declaration().maybe_body().is_some() {
                let return_type_from_body = self.get_return_type_from_body(getter, None)?;
                return Ok(Some(
                    self.instantiate_type_if_needed(return_type_from_body, symbol)?,
                ));
            }
        }

        if let Some(setter) = setter.as_ref() {
            if !self.is_private_within_ambient(setter) {
                self.error_or_suggestion(
                    self.no_implicit_any,
                    setter,
                    &*Diagnostics::Property_0_implicitly_has_type_any_because_its_set_accessor_lacks_a_parameter_type_annotation,
                    Some(vec![
                        self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None)?
                    ])
                );
            }
            return Ok(Some(self.any_type()));
        } else if let Some(getter) = getter.as_ref() {
            // Debug.assert(!!getter, ...);
            if !self.is_private_within_ambient(getter) {
                self.error_or_suggestion(
                    self.no_implicit_any,
                    getter,
                    &*Diagnostics::Property_0_implicitly_has_type_any_because_its_get_accessor_lacks_a_return_type_annotation,
                    Some(vec![
                        self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None)?
                    ])
                );
            }
            return Ok(Some(self.any_type()));
        }
        Ok(None)
    }

    pub(super) fn instantiate_type_if_needed(
        &self,
        type_: Id<Type>,
        symbol: &Symbol,
    ) -> io::Result<Id<Type>> {
        if get_check_flags(symbol).intersects(CheckFlags::Instantiated) {
            let links = self.get_symbol_links(symbol);
            return self.instantiate_type(type_, (*links).borrow().mapper.clone());
        }

        Ok(type_)
    }
}
