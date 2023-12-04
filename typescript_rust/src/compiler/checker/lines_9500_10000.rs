use std::{borrow::Borrow, io, ptr};

use gc::Gc;
use id_arena::Id;

use super::signature_has_rest_parameter;
use crate::{
    add_related_info, append, chain_diagnostic_messages, concatenate, create_diagnostic_for_node,
    create_diagnostic_for_node_from_message_chain, filter, find, find_ancestor, flat_map,
    get_assignment_declaration_kind, get_check_flags, get_declaration_of_kind,
    get_effective_base_type_node, get_effective_implements_type_nodes,
    get_effective_type_annotation_node, get_effective_type_parameter_declarations,
    get_object_flags, get_parameter_symbol_from_jsdoc, is_access_expression, is_binary_expression,
    is_export_assignment, is_in_js_file, is_jsdoc_template_tag, is_shorthand_ambient_module_symbol,
    is_source_file, is_type_alias, length, maybe_append_if_unique_gc, resolving_empty_array,
    return_ok_default_if_none, some, try_map, try_maybe_first_defined, try_maybe_map, try_some,
    AsDoubleDeref, AssignmentDeclarationKind, CheckFlags, Debug_, Diagnostics, ElementFlags,
    HasTypeArgumentsInterface, InterfaceTypeInterface, InternalSymbolName, Node, NodeInterface,
    ObjectFlags, OptionTry, Signature, SignatureKind, Symbol, SymbolFlags, SymbolInterface,
    SyntaxKind, TransientSymbolInterface, Type, TypeChecker, TypeFlags, TypeFormatFlags,
    TypeInterface, TypeSystemPropertyName,
};

impl TypeChecker {
    pub(super) fn get_base_type_variable_of_class(
        &self,
        symbol: &Symbol,
    ) -> io::Result<Option<Id<Type>>> {
        let base_constructor_type = self.get_base_constructor_type_of_class(
            &*self.get_declared_type_of_class_or_interface(symbol)?,
        )?;
        Ok(
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
                    |t: &Id<Type>, _| t.flags().intersects(TypeFlags::TypeVariable),
                )
                .cloned()
            } else {
                None
            },
        )
    }

    pub(super) fn get_type_of_func_class_enum_module(
        &self,
        symbol: &Symbol,
    ) -> io::Result<Id<Type>> {
        let mut links = self.get_symbol_links(symbol);
        let original_links = links.clone();
        let mut symbol = symbol.symbol_wrapper();
        if (*links).borrow().type_.is_none() {
            let expando = symbol
                .maybe_value_declaration()
                .try_and_then(|value_declaration| {
                    self.get_symbol_of_expando(&value_declaration, false)
                })?;
            if let Some(expando) = expando {
                let merged = self.merge_js_symbols(&symbol, Some(expando))?;
                if let Some(merged) = merged {
                    symbol = merged.clone();
                    links = merged.as_transient_symbol().symbol_links();
                }
            }
            let type_ = self.get_type_of_func_class_enum_module_worker(&symbol)?;
            original_links.borrow_mut().type_ = Some(type_.clone());
            links.borrow_mut().type_ = Some(type_);
        }
        let ret = (*links).borrow().type_.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_type_of_func_class_enum_module_worker(
        &self,
        symbol: &Symbol,
    ) -> io::Result<Id<Type>> {
        let declaration = symbol.maybe_value_declaration();
        if symbol.flags().intersects(SymbolFlags::Module)
            && is_shorthand_ambient_module_symbol(symbol)
        {
            return Ok(self.any_type());
        } else if matches!(
            declaration.as_ref(),
            Some(declaration) if declaration.kind() == SyntaxKind::BinaryExpression ||
                is_access_expression(declaration) &&
                declaration.parent().kind() == SyntaxKind::BinaryExpression
        ) {
            return self
                .get_widened_type_for_assignment_declaration(symbol, Option::<&Symbol>::None);
        } else if symbol.flags().intersects(SymbolFlags::ValueModule)
            && matches!(
                declaration.as_ref(),
                Some(declaration) if is_source_file(declaration) && declaration.as_source_file().maybe_common_js_module_indicator().is_some()
            )
        {
            let resolved_module = self
                .resolve_external_module_symbol(Some(symbol), None)?
                .unwrap();
            if !ptr::eq(&*resolved_module, symbol) {
                if !self.push_type_resolution(
                    &symbol.symbol_wrapper().into(),
                    TypeSystemPropertyName::Type,
                ) {
                    return Ok(self.error_type());
                }
                let export_equals = self
                    .get_merged_symbol(
                        (*symbol.exports())
                            .borrow()
                            .get(InternalSymbolName::ExportEquals)
                            .cloned(),
                    )
                    .unwrap();
                let type_ = self.get_widened_type_for_assignment_declaration(
                    &export_equals,
                    if Gc::ptr_eq(&export_equals, &resolved_module) {
                        None
                    } else {
                        Some(resolved_module)
                    },
                )?;
                if !self.pop_type_resolution() {
                    return self.report_circularity_error(symbol);
                }
                return Ok(type_);
            }
        }
        let type_: Id<Type> = self
            .create_object_type(ObjectFlags::Anonymous, Some(symbol))
            .into();
        Ok(if symbol.flags().intersects(SymbolFlags::Class) {
            let base_type_variable = self.get_base_type_variable_of_class(symbol)?;
            if let Some(base_type_variable) = base_type_variable {
                self.get_intersection_type(
                    &vec![type_, base_type_variable],
                    Option::<&Symbol>::None,
                    None,
                )?
            } else {
                type_
            }
        } else {
            if self.strict_null_checks && symbol.flags().intersects(SymbolFlags::Optional) {
                self.get_optional_type_(&type_, None)?
            } else {
                type_
            }
        })
    }

    pub(super) fn get_type_of_enum_member(&self, symbol: &Symbol) -> io::Result<Id<Type>> {
        let links = self.get_symbol_links(symbol);
        if let Some(links_type) = (*links).borrow().type_.clone() {
            return Ok(links_type);
        }
        let ret = self.get_declared_type_of_enum_member(symbol)?;
        links.borrow_mut().type_ = Some(ret.clone());
        Ok(ret)
    }

    pub(super) fn get_type_of_alias(&self, symbol: &Symbol) -> io::Result<Id<Type>> {
        let links = self.get_symbol_links(symbol);
        if (*links).borrow().type_.is_none() {
            let target_symbol = self.resolve_alias(symbol)?;
            let export_symbol = symbol.maybe_declarations().as_ref().try_and_then(|_| {
                self.get_target_of_alias_declaration(
                    &self.get_declaration_of_alias_symbol(symbol)?.unwrap(),
                    Some(true),
                )
            })?;
            let declared_type = export_symbol.as_ref().try_and_then(|export_symbol| {
                try_maybe_first_defined(
                    export_symbol.maybe_declarations().as_deref(),
                    |d: &Gc<Node>, _| -> io::Result<_> {
                        Ok(if is_export_assignment(d) {
                            self.try_get_type_from_effective_type_node(d)?
                        } else {
                            None
                        })
                    },
                )
            })?;
            links.borrow_mut().type_ = Some(
                if let Some(export_symbol) = export_symbol.as_ref().filter(|export_symbol| {
                    matches!(
                        export_symbol.maybe_declarations().as_deref(),
                        Some(export_symbol_declarations) if self.is_duplicated_common_js_export(Some(export_symbol_declarations))
                    ) && !symbol.maybe_declarations().as_ref().unwrap().is_empty()
                }) {
                    self.get_flow_type_from_common_js_export(export_symbol)?
                } else if self.is_duplicated_common_js_export(symbol.maybe_declarations().as_deref()) {
                    self.auto_type()
                } else if let Some(declared_type) = declared_type {
                    declared_type
                } else if target_symbol.flags().intersects(SymbolFlags::Value) {
                    self.get_type_of_symbol(&target_symbol)?
                } else {
                    self.error_type()
                }
            );
        }
        let ret = (*links).borrow().type_.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn get_type_of_instantiated_symbol(&self, symbol: &Symbol) -> io::Result<Id<Type>> {
        let links = self.get_symbol_links(symbol);
        if (*links).borrow().type_.is_none() {
            if !self.push_type_resolution(
                &symbol.symbol_wrapper().into(),
                TypeSystemPropertyName::Type,
            ) {
                links.borrow_mut().type_ = Some(self.error_type());
                return Ok(self.error_type());
            }
            let mut type_ = self.instantiate_type(
                &*self.get_type_of_symbol(&*{
                    let target = (*links).borrow().target.clone().unwrap();
                    target
                })?,
                {
                    let mapper = (*links).borrow().mapper.clone();
                    mapper
                },
            )?;
            if !self.pop_type_resolution() {
                type_ = self.report_circularity_error(symbol)?;
            }
            links.borrow_mut().type_ = Some(type_);
        }
        let ret = (*links).borrow().type_.clone().unwrap();
        Ok(ret)
    }

    pub(super) fn report_circularity_error(&self, symbol: &Symbol) -> io::Result<Id<Type>> {
        let declaration = symbol.maybe_value_declaration().unwrap();
        if get_effective_type_annotation_node(&declaration).is_some() {
            self.error(
                symbol.maybe_value_declaration(),
                &Diagnostics::_0_is_referenced_directly_or_indirectly_in_its_own_type_annotation,
                Some(vec![self.symbol_to_string_(
                    symbol,
                    Option::<&Node>::None,
                    None,
                    None,
                    None,
                )?]),
            );
            return Ok(self.error_type());
        }
        if self.no_implicit_any
            && (declaration.kind() != SyntaxKind::Parameter
                || declaration
                    .as_has_initializer()
                    .maybe_initializer()
                    .is_some())
        {
            self.error(
                symbol.maybe_value_declaration(),
                &Diagnostics::_0_implicitly_has_type_any_because_it_does_not_have_a_type_annotation_and_is_referenced_directly_or_indirectly_in_its_own_initializer,
                Some(vec![
                    self.symbol_to_string_(symbol, Option::<&Node>::None, None, None, None)?
                ])
            );
        }
        Ok(self.any_type())
    }

    pub(super) fn get_type_of_symbol_with_deferred_type(
        &self,
        symbol: &Symbol,
    ) -> io::Result<Id<Type>> {
        let links = self.get_symbol_links(symbol);
        let mut links = links.borrow_mut();
        if links.type_.is_none() {
            Debug_.assert_is_defined(&links.deferral_parent.as_ref(), None);
            Debug_.assert_is_defined(&links.deferral_constituents.as_ref(), None);
            links.type_ = Some(
                if links
                    .deferral_parent
                    .as_ref()
                    .unwrap()
                    .flags()
                    .intersects(TypeFlags::Union)
                {
                    self.get_union_type(
                        links.deferral_constituents.as_ref().unwrap(),
                        None,
                        Option::<&Symbol>::None,
                        None,
                        None,
                    )?
                } else {
                    self.get_intersection_type(
                        links.deferral_constituents.as_deref().unwrap(),
                        Option::<&Symbol>::None,
                        None,
                    )?
                },
            );
        }
        Ok(links.type_.clone().unwrap())
    }

    pub(super) fn get_set_accessor_type_of_symbol(&self, symbol: &Symbol) -> io::Result<Id<Type>> {
        if symbol.flags().intersects(SymbolFlags::Accessor) {
            let type_ = self.get_type_of_set_accessor(symbol)?;
            if let Some(type_) = type_ {
                return Ok(type_);
            }
        }
        self.get_type_of_symbol(symbol)
    }

    pub(super) fn get_type_of_symbol(&self, symbol: &Symbol) -> io::Result<Id<Type>> {
        let check_flags = get_check_flags(symbol);
        if check_flags.intersects(CheckFlags::DeferredType) {
            return self.get_type_of_symbol_with_deferred_type(symbol);
        }
        if check_flags.intersects(CheckFlags::Instantiated) {
            return self.get_type_of_instantiated_symbol(symbol);
        }
        if check_flags.intersects(CheckFlags::Mapped) {
            return self.get_type_of_mapped_symbol(symbol);
        }
        if check_flags.intersects(CheckFlags::ReverseMapped) {
            return self.get_type_of_reverse_mapped_symbol(symbol);
        }
        if symbol
            .flags()
            .intersects(SymbolFlags::Variable | SymbolFlags::Property)
        {
            return self.get_type_of_variable_or_parameter_or_property(symbol);
        }
        if symbol.flags().intersects(
            SymbolFlags::Function
                | SymbolFlags::Method
                | SymbolFlags::Class
                | SymbolFlags::Enum
                | SymbolFlags::ValueModule,
        ) {
            return self.get_type_of_func_class_enum_module(symbol);
        }
        if symbol.flags().intersects(SymbolFlags::EnumMember) {
            return self.get_type_of_enum_member(symbol);
        }
        if symbol.flags().intersects(SymbolFlags::Accessor) {
            return self.get_type_of_accessors(symbol);
        }
        if symbol.flags().intersects(SymbolFlags::Alias) {
            return self.get_type_of_alias(symbol);
        }
        Ok(self.error_type())
    }

    pub(super) fn get_non_missing_type_of_symbol(&self, symbol: &Symbol) -> io::Result<Id<Type>> {
        Ok(self.remove_missing_type(
            &*self.get_type_of_symbol(symbol)?,
            symbol.flags().intersects(SymbolFlags::Optional),
        ))
    }

    pub(super) fn is_reference_to_type(&self, type_: Id<Type>, target: Id<Type>) -> bool {
        /*type !== undefined && target !== undefined &&*/
        get_object_flags(type_).intersects(ObjectFlags::Reference)
            && ptr::eq(&*type_.as_type_reference_interface().target(), target)
    }

    pub(super) fn get_target_type(&self, type_: Id<Type>) -> Id<Type> {
        if get_object_flags(type_).intersects(ObjectFlags::Reference) {
            type_.as_type_reference_interface().target()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn has_base_type(
        &self,
        type_: Id<Type>,
        check_base: Option<Id<Type>>,
    ) -> io::Result<bool> {
        let check_base = check_base.map(|check_base| check_base.borrow().type_wrapper());
        self.has_base_type_check(check_base.as_deref(), type_)
    }

    pub(super) fn has_base_type_check(
        &self,
        check_base: Option<Id<Type>,
        type_: Id<Type>,
    ) -> io::Result<bool> {
        Ok(
            if get_object_flags(type_)
                .intersects(ObjectFlags::ClassOrInterface | ObjectFlags::Reference)
            {
                let target = self.get_target_type(type_);
                matches!(check_base, Some(check_base) if ptr::eq(&*target, check_base))
                    || try_some(
                        Some(&*self.get_base_types(&target)?),
                        Some(|type_: &Id<Type>| self.has_base_type_check(check_base, type_)),
                    )?
            } else if type_.flags().intersects(TypeFlags::Intersection) {
                try_some(
                    Some(type_.as_union_or_intersection_type_interface().types()),
                    Some(|type_: &Id<Type>| self.has_base_type_check(check_base, type_)),
                )?
            } else {
                false
            },
        )
    }

    pub(super) fn append_type_parameters(
        &self,
        mut type_parameters: Option<Vec<Id<Type>>>,
        declarations: &[Gc<Node>],
    ) -> io::Result<Option<Vec<Id<Type>>>> {
        for declaration in declarations {
            type_parameters = Some(maybe_append_if_unique_gc(
                type_parameters,
                &self.get_declared_type_of_type_parameter(
                    &self.get_symbol_of_node(&**declaration)?.unwrap(),
                ),
            ));
        }
        Ok(type_parameters)
    }

    pub(super) fn get_outer_type_parameters(
        &self,
        node: &Node,
        include_this_types: Option<bool>,
    ) -> io::Result<Option<Vec<Id<Type /*TypeParameter*/>>>> {
        let mut node = Some(node.node_wrapper());
        loop {
            node = node.unwrap().maybe_parent();
            if let Some(node_present) = node.as_ref() {
                if is_binary_expression(node_present) {
                    let assignment_kind = get_assignment_declaration_kind(node_present);
                    if matches!(
                        assignment_kind,
                        AssignmentDeclarationKind::Prototype
                            | AssignmentDeclarationKind::PrototypeProperty
                    ) {
                        let symbol =
                            self.get_symbol_of_node(&node_present.as_binary_expression().left)?;
                        if let Some(symbol) = symbol {
                            if let Some(symbol_parent) = symbol.maybe_parent() {
                                if find_ancestor(symbol_parent.maybe_value_declaration(), |d| {
                                    ptr::eq(&**node_present, d)
                                })
                                .is_none()
                                {
                                    node = symbol_parent.maybe_value_declaration();
                                }
                            }
                        }
                    }
                }
            }
            if node.is_none() {
                return Ok(None);
            }
            let node_present = node.as_deref().unwrap();
            match node_present.kind() {
                SyntaxKind::ClassDeclaration
                | SyntaxKind::ClassExpression
                | SyntaxKind::InterfaceDeclaration
                | SyntaxKind::CallSignature
                | SyntaxKind::ConstructSignature
                | SyntaxKind::MethodSignature
                | SyntaxKind::FunctionType
                | SyntaxKind::ConstructorType
                | SyntaxKind::JSDocFunctionType
                | SyntaxKind::FunctionDeclaration
                | SyntaxKind::MethodDeclaration
                | SyntaxKind::FunctionExpression
                | SyntaxKind::ArrowFunction
                | SyntaxKind::TypeAliasDeclaration
                | SyntaxKind::JSDocTemplateTag
                | SyntaxKind::JSDocTypedefTag
                | SyntaxKind::JSDocEnumTag
                | SyntaxKind::JSDocCallbackTag
                | SyntaxKind::MappedType
                | SyntaxKind::ConditionalType => {
                    let mut outer_type_parameters =
                        self.get_outer_type_parameters(node_present, include_this_types)?;
                    if node_present.kind() == SyntaxKind::MappedType {
                        if outer_type_parameters.is_none() {
                            outer_type_parameters = Some(vec![]);
                        }
                        append(
                            outer_type_parameters.as_mut().unwrap(),
                            Some(
                                self.get_declared_type_of_type_parameter(
                                    &self
                                        .get_symbol_of_node(
                                            &node_present.as_mapped_type_node().type_parameter,
                                        )?
                                        .unwrap(),
                                ),
                            ),
                        );
                        return Ok(outer_type_parameters);
                    } else if node_present.kind() == SyntaxKind::ConditionalType {
                        let infer_type_parameters = self.get_infer_type_parameters(node_present)?;
                        if outer_type_parameters.is_none() && infer_type_parameters.is_none() {
                            return Ok(None);
                        }
                        return Ok(Some(concatenate(
                            outer_type_parameters.unwrap_or_else(|| vec![]),
                            infer_type_parameters.unwrap_or_else(|| vec![]),
                        )));
                    }
                    let mut outer_and_own_type_parameters = self.append_type_parameters(
                        outer_type_parameters,
                        &get_effective_type_parameter_declarations(node_present),
                    )?;
                    let this_type = if matches!(include_this_types, Some(true))
                        && (matches!(
                            node_present.kind(),
                            SyntaxKind::ClassDeclaration
                                | SyntaxKind::ClassExpression
                                | SyntaxKind::InterfaceDeclaration
                        ) || self.is_js_constructor(Some(node_present))?)
                    {
                        self.get_declared_type_of_class_or_interface(
                            &*self.get_symbol_of_node(node_present)?.unwrap(),
                        )?
                        .as_interface_type()
                        .maybe_this_type()
                    } else {
                        None
                    };
                    return Ok(if let Some(this_type) = this_type {
                        if outer_and_own_type_parameters.is_none() {
                            outer_and_own_type_parameters = Some(vec![]);
                        }
                        append(
                            outer_and_own_type_parameters.as_mut().unwrap(),
                            Some(this_type),
                        );
                        outer_and_own_type_parameters
                    } else {
                        outer_and_own_type_parameters
                    });
                }
                SyntaxKind::JSDocParameterTag => {
                    let param_symbol = get_parameter_symbol_from_jsdoc(node_present);
                    if let Some(param_symbol) = param_symbol {
                        node = param_symbol.maybe_value_declaration();
                    }
                }
                SyntaxKind::JSDocComment => {
                    let outer_type_parameters =
                        self.get_outer_type_parameters(node_present, include_this_types)?;
                    return Ok(
                        if let Some(node_tags) = node_present.as_jsdoc().tags.as_deref() {
                            self.append_type_parameters(
                                outer_type_parameters,
                                &flat_map(Some(node_tags), |t: &Gc<Node>, _| {
                                    if is_jsdoc_template_tag(t) {
                                        t.as_jsdoc_template_tag().type_parameters.to_vec()
                                    } else {
                                        vec![]
                                    }
                                }),
                            )?
                        } else {
                            outer_type_parameters
                        },
                    );
                }
                _ => (),
            }
        }
    }

    pub(super) fn get_outer_type_parameters_of_class_or_interface(
        &self,
        symbol: &Symbol,
    ) -> io::Result<Option<Vec<Id<Type /*TypeParameter*/>>>> {
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
        self.get_outer_type_parameters(&declaration, None)
    }

    pub(super) fn get_local_type_parameters_of_class_or_interface_or_type_alias(
        &self,
        symbol: &Symbol,
    ) -> io::Result<Option<Vec<Id<Type /*TypeParameter*/>>>> {
        let symbol_declarations = symbol.maybe_declarations();
        let symbol_declarations = return_ok_default_if_none!(symbol_declarations.as_ref());
        let mut result: Option<Vec<Id<Type /*TypeParameter*/>>> = None;
        for node in symbol_declarations {
            if matches!(
                node.kind(),
                SyntaxKind::InterfaceDeclaration
                    | SyntaxKind::ClassDeclaration
                    | SyntaxKind::ClassExpression
            ) || self.is_js_constructor(Some(&**node))?
                || is_type_alias(node)
            {
                let declaration = node;
                result = self.append_type_parameters(
                    result,
                    &get_effective_type_parameter_declarations(declaration),
                )?;
            }
        }
        Ok(result)
    }

    pub(super) fn get_type_parameters_of_class_or_interface(
        &self,
        symbol: &Symbol,
    ) -> io::Result<Option<Vec<Id<Type /*TypeParameter*/>>>> {
        let outer_type_parameters = self.get_outer_type_parameters_of_class_or_interface(symbol)?;
        let local_type_parameters =
            self.get_local_type_parameters_of_class_or_interface_or_type_alias(symbol)?;
        if outer_type_parameters.is_none() && local_type_parameters.is_none() {
            return Ok(None);
        }
        Ok(Some(concatenate(
            outer_type_parameters.unwrap_or_else(|| vec![]),
            local_type_parameters.unwrap_or_else(|| vec![]),
        )))
    }

    pub(super) fn is_mixin_constructor_type(&self, type_: Id<Type>) -> io::Result<bool> {
        let signatures = self.get_signatures_of_type(type_, SignatureKind::Construct)?;
        if signatures.len() == 1 {
            let s = &signatures[0];
            if s.maybe_type_parameters().is_none()
                && s.parameters().len() == 1
                && signature_has_rest_parameter(s)
            {
                let param_type = self.get_type_of_parameter(&s.parameters()[0])?;
                return Ok(self.is_type_any(Some(&*param_type))
                    || matches!(
                        self.get_element_type_of_array_type(&param_type)?,
                        Some(element_type) if Gc::ptr_eq(&element_type, &self.any_type())
                    ));
            }
        }
        Ok(false)
    }

    pub(super) fn is_constructor_type(&self, type_: Id<Type>) -> io::Result<bool> {
        if !self
            .get_signatures_of_type(type_, SignatureKind::Construct)?
            .is_empty()
        {
            return Ok(true);
        }
        if type_.flags().intersects(TypeFlags::TypeVariable) {
            let constraint = self.get_base_constraint_of_type(type_)?;
            return Ok(matches!(
                constraint,
                Some(constraint) if self.is_mixin_constructor_type(&constraint)?
            ));
        }
        Ok(false)
    }

    pub(super) fn get_base_type_node_of_class(
        &self,
        type_: Id<Type>, /*InterfaceType*/
    ) -> Option<Gc<Node /*ExpressionWithTypeArguments*/>> {
        get_effective_base_type_node(&type_.symbol().maybe_value_declaration().unwrap())
    }

    pub(super) fn get_constructors_for_type_arguments(
        &self,
        type_: Id<Type>,
        type_argument_nodes: Option<&[Gc<Node /*TypeNode*/>]>,
        location: &Node,
    ) -> io::Result<Vec<Gc<Signature>>> {
        let type_arg_count = length(type_argument_nodes);
        let is_javascript = is_in_js_file(Some(location));
        Ok(filter(
            &self.get_signatures_of_type(type_, SignatureKind::Construct)?,
            |sig: &Gc<Signature>| {
                (is_javascript
                    || type_arg_count
                        >= self.get_min_type_argument_count(sig.maybe_type_parameters().as_deref()))
                    && type_arg_count <= length(sig.maybe_type_parameters().as_deref())
            },
        ))
    }

    pub(super) fn get_instantiated_constructors_for_type_arguments(
        &self,
        type_: Id<Type>,
        type_argument_nodes: Option<&[Gc<Node /*TypeNode*/>]>,
        location: &Node,
    ) -> io::Result<Vec<Gc<Signature>>> {
        let signatures =
            self.get_constructors_for_type_arguments(type_, type_argument_nodes, location)?;
        let type_arguments =
            try_maybe_map(type_argument_nodes, |type_argument_node: &Gc<Node>, _| {
                self.get_type_from_type_node_(type_argument_node)
            })
            .transpose()?;
        try_map(&signatures, |sig: &Gc<Signature>, _| -> io::Result<_> {
            Ok(
                if some(
                    sig.maybe_type_parameters().as_deref(),
                    Option::<fn(&Id<Type>) -> bool>::None,
                ) {
                    self.get_signature_instantiation(
                        sig.clone(),
                        type_arguments.as_deref(),
                        is_in_js_file(Some(location)),
                        None,
                    )?
                } else {
                    sig.clone()
                },
            )
        })
    }

    pub(super) fn get_base_constructor_type_of_class(
        &self,
        type_: Id<Type>, /*InterfaceType*/
    ) -> io::Result<Id<Type>> {
        let type_as_not_actually_interface_type = type_.as_not_actually_interface_type();
        if type_as_not_actually_interface_type
            .maybe_resolved_base_constructor_type()
            .is_none()
        {
            let decl = type_.symbol().maybe_value_declaration().unwrap();
            let extended = get_effective_base_type_node(&decl);
            let base_type_node = self.get_base_type_node_of_class(type_);
            if base_type_node.is_none() {
                let ret = self.undefined_type();
                *type_as_not_actually_interface_type.maybe_resolved_base_constructor_type() =
                    Some(ret.clone());
                return Ok(ret);
            }
            let base_type_node = base_type_node.unwrap();
            if !self.push_type_resolution(
                &type_.type_wrapper().into(),
                TypeSystemPropertyName::ResolvedBaseConstructorType,
            ) {
                return Ok(self.error_type());
            }
            let base_constructor_type = self.check_expression(
                &base_type_node
                    .as_expression_with_type_arguments()
                    .expression,
                None,
                None,
            )?;
            if let Some(extended) = extended
                .as_ref()
                .filter(|extended| !Gc::ptr_eq(&base_type_node, *extended))
            {
                Debug_.assert(
                    extended
                        .as_expression_with_type_arguments()
                        .maybe_type_arguments()
                        .is_none(),
                    None,
                );
                self.check_expression(
                    &extended.as_expression_with_type_arguments().expression,
                    None,
                    None,
                )?;
            }
            if base_constructor_type
                .flags()
                .intersects(TypeFlags::Object | TypeFlags::Intersection)
            {
                self.resolve_structured_type_members(&base_constructor_type)?;
            }
            if !self.pop_type_resolution() {
                self.error(
                    type_.symbol().maybe_value_declaration(),
                    &Diagnostics::_0_is_referenced_directly_or_indirectly_in_its_own_base_expression,
                    Some(vec![
                        self.symbol_to_string_(&type_.symbol(), Option::<&Node>::None, None, None, None)?
                    ])
                );
                let ret = self.error_type();
                *type_as_not_actually_interface_type.maybe_resolved_base_constructor_type() =
                    Some(ret.clone());
                return Ok(ret);
            }
            if !base_constructor_type.flags().intersects(TypeFlags::Any)
                && !Gc::ptr_eq(&base_constructor_type, &self.null_widening_type())
                && !self.is_constructor_type(&base_constructor_type)?
            {
                let err = self.error(
                    Some(
                        &*base_type_node
                            .as_expression_with_type_arguments()
                            .expression,
                    ),
                    &Diagnostics::Type_0_is_not_a_constructor_function_type,
                    Some(vec![self.type_to_string_(
                        &base_constructor_type,
                        Option::<&Node>::None,
                        None,
                        None,
                    )?]),
                );
                if base_constructor_type
                    .flags()
                    .intersects(TypeFlags::TypeParameter)
                {
                    let constraint =
                        self.get_constraint_from_type_parameter(&base_constructor_type)?;
                    let mut ctor_return = self.unknown_type();
                    if let Some(constraint) = constraint {
                        let ctor_sig =
                            self.get_signatures_of_type(&constraint, SignatureKind::Construct)?;
                        if let Some(ctor_sig_0) = ctor_sig.get(0) {
                            ctor_return = self.get_return_type_of_signature(ctor_sig_0.clone())?;
                        }
                    }
                    if let Some(base_constructor_type_symbol_declarations) = base_constructor_type
                        .symbol()
                        .maybe_declarations()
                        .as_deref()
                    {
                        add_related_info(
                            &err,
                            vec![create_diagnostic_for_node(&base_constructor_type_symbol_declarations[0], &Diagnostics::Did_you_mean_for_0_to_be_constrained_to_type_new_args_Colon_any_1, Some(vec![
                                        self.symbol_to_string_(&base_constructor_type.symbol(), Option::<&Node>::None, None, None, None)?,
                                        self.type_to_string_(&ctor_return, Option::<&Node>::None, None, None)?
                                    ])).into()]
                        );
                    }
                }
                let ret = self.error_type();
                *type_as_not_actually_interface_type.maybe_resolved_base_constructor_type() =
                    Some(ret.clone());
                return Ok(ret);
            }
            *type_as_not_actually_interface_type.maybe_resolved_base_constructor_type() =
                Some(base_constructor_type);
        }
        let ret = type_as_not_actually_interface_type
            .maybe_resolved_base_constructor_type()
            .clone()
            .unwrap();
        Ok(ret)
    }

    pub(super) fn get_implements_types(
        &self,
        type_: Id<Type>, /*InterfaceType*/
    ) -> io::Result<Vec<Id<Type /*BaseType*/>>> {
        let mut resolved_implements_types: Vec<Id<Type /*BaseType*/>> = vec![];
        if let Some(type_symbol_declarations) = type_.symbol().maybe_declarations().as_deref() {
            for declaration in type_symbol_declarations {
                let implements_type_nodes = get_effective_implements_type_nodes(declaration);
                if implements_type_nodes.is_none() {
                    continue;
                }
                let implements_type_nodes = implements_type_nodes.unwrap();
                for node in implements_type_nodes {
                    let implements_type = self.get_type_from_type_node_(&node)?;
                    if !self.is_error_type(&implements_type) {
                        resolved_implements_types.push(implements_type);
                    }
                }
            }
        }
        Ok(resolved_implements_types)
    }

    pub(super) fn report_circular_base_type(&self, node: &Node, type_: Id<Type>) -> io::Result<()> {
        self.error(
            Some(node),
            &Diagnostics::Type_0_recursively_references_itself_as_a_base_type,
            Some(vec![self.type_to_string_(
                type_,
                Option::<&Node>::None,
                Some(TypeFormatFlags::WriteArrayAsGenericType),
                None,
            )?]),
        );

        Ok(())
    }

    pub(super) fn get_base_types(
        &self,
        type_: Id<Type>, /*InterfaceType*/
    ) -> io::Result<Vec<Id<Type /*BaseType*/>>> {
        let type_as_not_actually_interface_type = type_.as_not_actually_interface_type();
        if !matches!(
            type_as_not_actually_interface_type.maybe_base_types_resolved(),
            Some(true)
        ) {
            if self.push_type_resolution(
                &type_.type_wrapper().into(),
                TypeSystemPropertyName::ResolvedBaseTypes,
            ) {
                if type_as_not_actually_interface_type
                    .object_flags()
                    .intersects(ObjectFlags::Tuple)
                {
                    *type_as_not_actually_interface_type.maybe_resolved_base_types() =
                        Some(Gc::new(vec![self.get_tuple_base_type(type_)?]));
                } else if type_
                    .symbol()
                    .flags()
                    .intersects(SymbolFlags::Class | SymbolFlags::Interface)
                {
                    if type_.symbol().flags().intersects(SymbolFlags::Class) {
                        self.resolve_base_types_of_class(type_)?;
                    }
                    if type_.symbol().flags().intersects(SymbolFlags::Interface) {
                        self.resolve_base_types_of_interface(type_)?;
                    }
                } else {
                    Debug_.fail(Some("type must be class or interface"));
                }
                if !self.pop_type_resolution() {
                    if let Some(type_symbol_declarations) =
                        type_.symbol().maybe_declarations().as_deref()
                    {
                        for declaration in type_symbol_declarations {
                            if matches!(
                                declaration.kind(),
                                SyntaxKind::ClassDeclaration | SyntaxKind::InterfaceDeclaration
                            ) {
                                self.report_circular_base_type(declaration, type_)?;
                            }
                        }
                    }
                }
            }
            type_as_not_actually_interface_type.set_base_types_resolved(Some(true));
        }
        let ret = Vec::clone(
            type_as_not_actually_interface_type
                .maybe_resolved_base_types()
                .as_ref()
                .unwrap(),
        );
        Ok(ret)
    }

    pub(super) fn get_tuple_base_type(
        &self,
        type_: Id<Type>, /*TupleType*/
    ) -> io::Result<Id<Type>> {
        let type_as_tuple_type = type_.as_tuple_type();
        let element_types = try_maybe_map(
            type_as_tuple_type.maybe_type_parameters(),
            |t: &Id<Type>, i| -> io::Result<_> {
                Ok(
                    if type_as_tuple_type.element_flags[i].intersects(ElementFlags::Variadic) {
                        self.get_indexed_access_type(
                            t,
                            &self.number_type(),
                            None,
                            Option::<&Node>::None,
                            Option::<&Symbol>::None,
                            None,
                        )?
                    } else {
                        t.type_wrapper()
                    },
                )
            },
        )
        .transpose()?;
        Ok(self.create_array_type(
            &*self.get_union_type(
                &element_types.unwrap_or_else(|| vec![]),
                None,
                Option::<&Symbol>::None,
                None,
                None,
            )?,
            Some(type_as_tuple_type.readonly),
        ))
    }

    pub(super) fn resolve_base_types_of_class(
        &self,
        type_: Id<Type>, /*InterfaceType*/
    ) -> io::Result<Gc<Vec<Id<Type /*BaseType*/>>>> {
        let type_as_not_actually_interface_type = type_.as_not_actually_interface_type();
        *type_as_not_actually_interface_type.maybe_resolved_base_types() =
            Some(resolving_empty_array());
        let base_constructor_type =
            self.get_apparent_type(&*self.get_base_constructor_type_of_class(type_)?)?;
        if !base_constructor_type
            .flags()
            .intersects(TypeFlags::Object | TypeFlags::Intersection | TypeFlags::Any)
        {
            let ret = Gc::new(vec![]);
            *type_as_not_actually_interface_type.maybe_resolved_base_types() = Some(ret.clone());
            return Ok(ret);
        }
        let base_type_node = self.get_base_type_node_of_class(type_).unwrap();
        let base_type: Id<Type>;
        let original_base_type = base_constructor_type
            .maybe_symbol()
            .map(|symbol| self.get_declared_type_of_symbol(&symbol))
            .transpose()?;
        if let Some(base_constructor_type_symbol) = base_constructor_type
            .maybe_symbol()
            .as_ref()
            .try_filter(|base_constructor_type_symbol| -> io::Result<_> {
                Ok(base_constructor_type_symbol
                    .flags()
                    .intersects(SymbolFlags::Class)
                    && self.are_all_outer_type_parameters_applied(
                        original_base_type.as_ref().unwrap(),
                    )?)
            })?
        {
            base_type = self.get_type_from_class_or_interface_reference(
                &base_type_node,
                base_constructor_type_symbol,
            )?;
        } else if base_constructor_type.flags().intersects(TypeFlags::Any) {
            base_type = base_constructor_type;
        } else {
            let constructors = self.get_instantiated_constructors_for_type_arguments(
                &base_constructor_type,
                base_type_node
                    .as_expression_with_type_arguments()
                    .maybe_type_arguments()
                    .as_double_deref(),
                &base_type_node,
            )?;
            if constructors.is_empty() {
                self.error(
                    Some(
                        &*base_type_node
                            .as_expression_with_type_arguments()
                            .expression,
                    ),
                    &Diagnostics::No_base_constructor_has_the_specified_number_of_type_arguments,
                    None,
                );
                let ret = Gc::new(vec![]);
                *type_as_not_actually_interface_type.maybe_resolved_base_types() =
                    Some(ret.clone());
                return Ok(ret);
            }
            base_type = self.get_return_type_of_signature(constructors[0].clone())?;
        }

        if self.is_error_type(&base_type) {
            let ret = Gc::new(vec![]);
            *type_as_not_actually_interface_type.maybe_resolved_base_types() = Some(ret.clone());
            return Ok(ret);
        }
        let reduced_base_type = self.get_reduced_type(&base_type)?;
        if !self.is_valid_base_type(&reduced_base_type)? {
            let elaboration = self.elaborate_never_intersection(None, &base_type)?;
            let diagnostic = chain_diagnostic_messages(
                elaboration,
                &Diagnostics::Base_constructor_return_type_0_is_not_an_object_type_or_intersection_of_object_types_with_statically_known_members,
                Some(vec![
                    self.type_to_string_(&reduced_base_type, Option::<&Node>::None, None, None)?
                ])
            );
            self.diagnostics().add(Gc::new(
                create_diagnostic_for_node_from_message_chain(
                    &base_type_node
                        .as_expression_with_type_arguments()
                        .expression,
                    diagnostic,
                    None,
                )
                .into(),
            ));
            let ret = Gc::new(vec![]);
            *type_as_not_actually_interface_type.maybe_resolved_base_types() = Some(ret.clone());
            return Ok(ret);
        }
        if ptr::eq(type_, &*reduced_base_type)
            || self.has_base_type(&reduced_base_type, Some(type_))?
        {
            self.error(
                type_.symbol().maybe_value_declaration(),
                &Diagnostics::Type_0_recursively_references_itself_as_a_base_type,
                Some(vec![self.type_to_string_(
                    type_,
                    Option::<&Node>::None,
                    Some(TypeFormatFlags::WriteArrayAsGenericType),
                    None,
                )?]),
            );
            let ret = Gc::new(vec![]);
            *type_as_not_actually_interface_type.maybe_resolved_base_types() = Some(ret.clone());
            return Ok(ret);
        }
        if Gc::ptr_eq(
            type_as_not_actually_interface_type
                .maybe_resolved_base_types()
                .as_ref()
                .unwrap(),
            &resolving_empty_array(),
        ) {
            type_as_not_actually_interface_type.set_members(None);
        }
        let ret = Gc::new(vec![reduced_base_type]);
        *type_as_not_actually_interface_type.maybe_resolved_base_types() = Some(ret.clone());
        Ok(ret)
    }
}
