use gc::Gc;
use itertools::{Either, Itertools};
use regex::Regex;
use std::borrow::{Borrow, Cow};
use std::convert::TryInto;
use std::{io, ptr};

use super::{signature_has_literal_types, CheckMode, ResolveNameNameArg};
use crate::{
    capitalize, contains, find, find_ancestor, get_assignment_target_kind, get_check_flags,
    get_containing_class, get_first_identifier, get_script_target_features,
    get_spelling_suggestion, has_effective_modifier, id_text, is_assignment_target,
    is_binding_pattern, is_call_or_new_expression, is_entity_name_expression,
    is_function_like_declaration, is_jsx_opening_like_element, is_named_declaration,
    is_optional_chain, is_private_identifier, is_private_identifier_class_element_declaration,
    is_property_access_expression, is_static, is_string_literal_like,
    is_tagged_template_expression, is_write_only_access, map_defined, maybe_for_each,
    return_ok_default_if_none, skip_parentheses, starts_with, symbol_name, try_filter,
    try_get_property_access_or_identifier_to_string, try_get_spelling_suggestion,
    try_maybe_for_each, unescape_leading_underscores, AccessFlags, AssignmentKind, CheckFlags,
    Debug_, Diagnostics, ModifierFlags, NamedDeclarationInterface, Node, NodeFlags, NodeInterface,
    OptionTry, Signature, SignatureFlags, StrOrRcNode, Symbol, SymbolFlags, SymbolInterface,
    SymbolTable, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
    UnionOrIntersectionTypeInterface,
};
use local_macros::enum_unwrapped;

impl TypeChecker {
    pub(super) fn container_seems_to_be_empty_dom_element(
        &self,
        containing_type: &Type,
    ) -> io::Result<bool> {
        Ok(matches!(
            self.compiler_options.lib.as_ref(),
            Some(compiler_options_lib) if compiler_options_lib.into_iter().position(|lib_item| lib_item == "dom").is_none()
        ) &&
            self.every_contained_type(
                containing_type,
                |type_: &Type| matches!(
                    type_.maybe_symbol(),
                    Some(type_symbol) if {
                        lazy_static! {
                            static ref element_regex: Regex = Regex::new("^(EventTarget|Node|((HTML[a-zA-Z]*)?Element))$").unwrap();
                        }
                        element_regex.is_match(&unescape_leading_underscores(type_symbol.escaped_name()))
                    }
                )
            ) &&
            self.is_empty_object_type(containing_type)?)
    }

    pub(super) fn type_has_static_property(
        &self,
        prop_name: &str, /*__String*/
        containing_type: &Type,
    ) -> io::Result<bool> {
        let prop =
            containing_type
                .maybe_symbol()
                .as_ref()
                .try_and_then(|containing_type_symbol| {
                    self.get_property_of_type_(
                        &*self.get_type_of_symbol(containing_type_symbol)?,
                        prop_name,
                        None,
                    )
                })?;
        Ok(matches!(
            prop.as_ref().and_then(|prop| prop.maybe_value_declaration()).as_ref(),
            Some(prop_value_declaration) if is_static(prop_value_declaration)
        ))
    }

    pub(super) fn get_suggested_lib_for_non_existent_name<'name>(
        &self,
        name: ResolveNameNameArg<'name>,
    ) -> Option<String> {
        let missing_name = self.diagnostic_name(name).into_owned();
        let all_features = get_script_target_features();
        let lib_targets = all_features.keys();
        for lib_target in lib_targets {
            let containing_types = all_features.get(lib_target).unwrap();
            let containing_types: Vec<_> = containing_types.keys().map(|key| *key).collect();
            if
            /*containingTypes !== undefined &&*/
            contains(Some(&containing_types), &&*missing_name) {
                return Some((*lib_target).to_owned());
            }
        }
        None
    }

    pub(super) fn get_suggested_lib_for_non_existent_property(
        &self,
        missing_property: &str,
        containing_type: &Type,
    ) -> io::Result<Option<String>> {
        let container =
            return_ok_default_if_none!(self.get_apparent_type(containing_type)?.maybe_symbol());
        let all_features = get_script_target_features();
        let lib_targets = all_features.keys();
        for lib_target in lib_targets {
            let features_of_lib = all_features.get(lib_target).unwrap();
            let container_name = symbol_name(&container);
            let features_of_containing_type = features_of_lib.get(&&*container_name);
            if matches!(
                features_of_containing_type,
                Some(features_of_containing_type) if contains(Some(features_of_containing_type), &missing_property)
            ) {
                return Ok(Some((*lib_target).to_owned()));
            }
        }
        Ok(None)
    }

    pub fn get_suggested_symbol_for_nonexistent_class_member(
        &self,
        name: &str,
        base_type: &Type,
    ) -> io::Result<Option<Gc<Symbol>>> {
        self.get_spelling_suggestion_for_name(
            name,
            self.get_properties_of_type(base_type)?,
            SymbolFlags::ClassMember,
        )
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_property<'name>(
        &self,
        name: impl Into<StrOrRcNode<'name>>, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> io::Result<Option<Gc<Symbol>>> {
        let props = self.get_properties_of_type(containing_type)?;
        let mut name = name.into();
        let name_inner_rc_node = match name.clone() {
            StrOrRcNode::RcNode(name) => Some(name),
            _ => None,
        };
        let mut props_: Option<Either<_, _>> = None;
        let mut did_set_props = false;
        if let Some(name_inner_rc_node) = name_inner_rc_node.as_ref() {
            let parent = name_inner_rc_node.parent();
            if is_property_access_expression(&parent) {
                did_set_props = true;
                props_ = Some(Either::Right(
                    try_filter(&props.collect_vec(), |prop: &Gc<Symbol>| {
                        self.is_valid_property_access_for_completions_(
                            &parent,
                            containing_type,
                            prop,
                        )
                    })?
                    .into_iter(),
                ));
            }
            name = StrOrRcNode::Str(id_text(name_inner_rc_node));
        }
        if !did_set_props {
            props_ = Some(Either::Left(props));
        }
        let props = props_.unwrap();
        let name = enum_unwrapped!(name, [StrOrRcNode, Str]);
        self.get_spelling_suggestion_for_name(name, props, SymbolFlags::Value)
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_jsx_attribute<'name>(
        &self,
        name: impl Into<StrOrRcNode<'name>>, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> io::Result<Option<Gc<Symbol>>> {
        let name = name.into();
        let str_name = match &name {
            StrOrRcNode::Str(name) => *name,
            StrOrRcNode::RcNode(name) => id_text(name),
        };
        let mut properties = self.get_properties_of_type(containing_type)?;
        let jsx_specific = if str_name == "for" {
            properties.find(|x| symbol_name(x) == "htmlFor")
        } else if str_name == "class" {
            properties.find(|x| symbol_name(x) == "className")
        } else {
            None
        };
        jsx_specific.try_or_else(|| {
            self.get_spelling_suggestion_for_name(&str_name, properties, SymbolFlags::Value)
        })
    }

    pub(super) fn get_suggestion_for_nonexistent_property<'name>(
        &self,
        name: impl Into<StrOrRcNode<'name>>, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> io::Result<Option<String>> {
        let suggestion =
            self.get_suggested_symbol_for_nonexistent_property(name, containing_type)?;
        Ok(suggestion
            .as_ref()
            .map(|suggestion| symbol_name(suggestion).into_owned()))
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_symbol_(
        &self,
        location: Option<impl Borrow<Node>>,
        outer_name: &str, /*__String*/
        meaning: SymbolFlags,
    ) -> io::Result<Option<Gc<Symbol>>> {
        // Debug.assert(outerName !== undefined, "outername should always be defined");
        let result = self.resolve_name_helper(
            location,
            outer_name,
            meaning,
            None,
            Some(outer_name),
            false,
            false,
            |symbols: &SymbolTable, name: &str /*__String*/, meaning: SymbolFlags| {
                Debug_.assert_equal(
                    &outer_name,
                    &name,
                    Some("name should equal outername"),
                    None,
                );
                let symbol = self.get_symbol(symbols, name, meaning)?;
                if symbol.is_some() {
                    return Ok(symbol);
                }
                let candidates: Vec<Gc<Symbol>>;
                if ptr::eq(symbols, &*self.globals()) {
                    let primitives = map_defined(
                        Some(["string", "number", "boolean", "object", "bigint", "symbol"]),
                        |s: &str, _| {
                            if symbols.contains_key(&capitalize(s)) {
                                Some(
                                    self.create_symbol(SymbolFlags::TypeAlias, s.to_owned(), None)
                                        .into(),
                                )
                            } else {
                                None
                            }
                        },
                    );
                    candidates = primitives
                        .into_iter()
                        .chain(symbols.values().cloned())
                        .collect();
                } else {
                    candidates = symbols.values().cloned().collect();
                }
                self.get_spelling_suggestion_for_name(
                    &unescape_leading_underscores(name),
                    &candidates,
                    meaning,
                )
            },
        )?;
        Ok(result)
    }

    pub(super) fn get_suggestion_for_nonexistent_symbol_(
        &self,
        location: Option<impl Borrow<Node>>,
        outer_name: &str, /*__String*/
        meaning: SymbolFlags,
    ) -> io::Result<Option<String>> {
        let symbol_result =
            self.get_suggested_symbol_for_nonexistent_symbol_(location, outer_name, meaning)?;
        Ok(symbol_result
            .as_ref()
            .map(|symbol_result| symbol_name(symbol_result).into_owned()))
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_module(
        &self,
        name: &Node, /*Identifier*/
        target_module: &Symbol,
    ) -> io::Result<Option<Gc<Symbol>>> {
        Ok(if target_module.maybe_exports().is_some() {
            self.get_spelling_suggestion_for_name(
                &id_text(name),
                &*self.get_exports_of_module_as_array(target_module)?,
                SymbolFlags::ModuleMember,
            )?
        } else {
            None
        })
    }

    pub fn get_suggestion_for_nonexistent_export(
        &self,
        name: &Node, /*Identifier*/
        target_module: &Symbol,
    ) -> io::Result<Option<String>> {
        let suggestion = self.get_suggested_symbol_for_nonexistent_module(name, target_module)?;
        Ok(suggestion
            .as_ref()
            .map(|suggestion| symbol_name(suggestion).into_owned()))
    }

    pub(super) fn get_suggestion_for_nonexistent_index_signature(
        &self,
        object_type: &Type,
        expr: &Node, /*ElementAccessExpression*/
        keyed_type: &Type,
    ) -> io::Result<Option<String>> {
        let suggested_method = if is_assignment_target(expr) {
            "set"
        } else {
            "get"
        };
        if !self.has_prop(object_type, keyed_type, suggested_method)? {
            return Ok(None);
        }

        let mut suggestion = try_get_property_access_or_identifier_to_string(
            &expr.as_element_access_expression().expression,
        )
        .map(Cow::into_owned);
        match suggestion.as_mut() {
            None => {
                suggestion = Some(suggested_method.to_owned());
            }
            Some(suggestion) => {
                suggestion.push_str(&format!(".{}", suggested_method));
            }
        }

        Ok(suggestion)
    }

    pub(super) fn has_prop(
        &self,
        object_type: &Type,
        keyed_type: &Type,
        name: &str, /*"set | "get"*/
    ) -> io::Result<bool> {
        let prop = self.get_property_of_object_type(object_type, name)?;
        if let Some(prop) = prop.as_ref() {
            let s = self.get_single_call_signature(&*self.get_type_of_symbol(prop)?);
            return Ok(matches!(
                s.as_ref(),
                Some(s) if self.get_min_argument_count(s, None)? >= 1 &&
                    self.is_type_assignable_to(keyed_type, &*self.get_type_at_position(s, 0)?)?
            ));
        }
        Ok(false)
    }

    pub(super) fn get_suggested_type_for_nonexistent_string_literal_type(
        &self,
        source: &Type, /*StringLiteralType*/
        target: &Type, /*UnionType*/
    ) -> Option<Gc<Type /*StringLiteralType*/>> {
        let candidates = target
            .as_union_type()
            .types()
            .into_iter()
            .filter(|type_| type_.flags().intersects(TypeFlags::StringLiteral))
            .cloned()
            .collect::<Vec<_>>();
        get_spelling_suggestion(
            &source.as_string_literal_type().value,
            &candidates,
            |type_: &Gc<Type>| Some(type_.as_string_literal_type().value.clone()),
        )
    }

    pub(super) fn get_spelling_suggestion_for_name(
        &self,
        name: &str,
        symbols: impl IntoIterator<Item = impl Borrow<Gc<Symbol>>>,
        meaning: SymbolFlags,
    ) -> io::Result<Option<Gc<Symbol>>> {
        let get_candidate_name = |candidate: &Gc<Symbol>| -> io::Result<_> {
            let candidate_name = symbol_name(candidate);
            if starts_with(&candidate_name, "\"") {
                return Ok(None);
            }

            if candidate.flags().intersects(meaning) {
                return Ok(Some(candidate_name.into_owned()));
            }

            if candidate.flags().intersects(SymbolFlags::Alias) {
                let alias = self.try_resolve_alias(candidate)?;
                if matches!(
                    alias.as_ref(),
                    Some(alias) if alias.flags().intersects(meaning)
                ) {
                    return Ok(Some(candidate_name.into_owned()));
                }
            }
            Ok(None)
        };

        try_get_spelling_suggestion(name, symbols, get_candidate_name)
    }

    pub(super) fn mark_property_as_referenced(
        &self,
        prop: &Symbol,
        node_for_check_write_only: Option<impl Borrow<Node>>,
        is_self_type_access: bool,
    ) {
        let value_declaration = /*prop &&*/ if prop.flags().intersects(SymbolFlags::ClassMember) {
            prop.maybe_value_declaration()
        } else {
            None
        };
        if value_declaration.is_none() {
            return;
        }
        let value_declaration = value_declaration.unwrap();
        let has_private_modifier =
            has_effective_modifier(&value_declaration, ModifierFlags::Private);
        let has_private_identifier = matches!(
            prop.maybe_value_declaration().as_ref(),
            Some(prop_value_declaration) if is_named_declaration(prop_value_declaration) &&
                is_private_identifier(&prop_value_declaration.as_named_declaration().name())
        );
        if !has_private_modifier && !has_private_identifier {
            return;
        }
        let node_for_check_write_only = node_for_check_write_only
            .map(|node_for_check_write_only| node_for_check_write_only.borrow().node_wrapper());
        if matches!(
            node_for_check_write_only.as_ref(),
            Some(node_for_check_write_only) if is_write_only_access(node_for_check_write_only)
        ) && !prop.flags().intersects(SymbolFlags::SetAccessor)
        {
            return;
        }
        if is_self_type_access {
            let containing_method = find_ancestor(
                node_for_check_write_only.as_deref(),
                is_function_like_declaration,
            );
            if matches!(
                containing_method.as_ref(),
                Some(containing_method) if matches!(
                    containing_method.maybe_symbol().as_ref(),
                    Some(containing_method_symbol) if ptr::eq(
                        &**containing_method_symbol,
                        prop
                    )
                )
            ) {
                return;
            }
        }

        if get_check_flags(prop).intersects(CheckFlags::Instantiated) {
            (*self.get_symbol_links(prop))
                .borrow()
                .target
                .clone()
                .unwrap()
        } else {
            prop.symbol_wrapper()
        }
        .set_is_referenced(Some(SymbolFlags::All));
    }

    pub(super) fn is_self_type_access(
        &self,
        name: &Node, /*Expression | QualifiedName*/
        parent: Option<impl Borrow<Symbol>>,
    ) -> io::Result<bool> {
        let parent = parent.map(|parent| parent.borrow().symbol_wrapper());
        Ok(name.kind() == SyntaxKind::ThisKeyword
            || matches!(
                parent.as_ref(),
                Some(parent) if is_entity_name_expression(name) &&
                    Gc::ptr_eq(
                        parent,
                        &self.get_resolved_symbol(
                            &get_first_identifier(name)
                        )?,
                    )
            ))
    }

    pub(super) fn is_valid_property_access_(
        &self,
        node: &Node,         /*PropertyAccessExpression | QualifiedName | ImportTypeNode*/
        property_name: &str, /*__String*/
    ) -> io::Result<bool> {
        Ok(match node.kind() {
            SyntaxKind::PropertyAccessExpression => {
                let node_as_property_access_expression = node.as_property_access_expression();
                self.is_valid_property_access_with_type(
                    node,
                    node_as_property_access_expression.expression.kind()
                        == SyntaxKind::SuperKeyword,
                    property_name,
                    &*self.get_widened_type(&*self.check_expression(
                        &node_as_property_access_expression.expression,
                        None,
                        None,
                    )?)?,
                )?
            }
            SyntaxKind::QualifiedName => self.is_valid_property_access_with_type(
                node,
                false,
                property_name,
                &*self.get_widened_type(&*self.check_expression(
                    &node.as_qualified_name().left,
                    None,
                    None,
                )?)?,
            )?,
            SyntaxKind::ImportType => self.is_valid_property_access_with_type(
                node,
                false,
                property_name,
                &*self.get_type_from_type_node_(node)?,
            )?,
            _ => unreachable!(),
        })
    }

    pub fn is_valid_property_access_for_completions_(
        &self,
        node: &Node, /*PropertyAccessExpression | ImportTypeNode | QualifiedName*/
        type_: &Type,
        property: &Symbol,
    ) -> io::Result<bool> {
        self.is_property_accessible(
            node,
            node.kind() == SyntaxKind::PropertyAccessExpression
                && node.as_property_access_expression().expression.kind()
                    == SyntaxKind::SuperKeyword,
            false,
            type_,
            property,
        )
    }

    pub(super) fn is_valid_property_access_with_type(
        &self,
        node: &Node, /*PropertyAccessExpression | QualifiedName | ImportTypeNode*/
        is_super: bool,
        property_name: &str, /*__String*/
        type_: &Type,
    ) -> io::Result<bool> {
        if self.is_type_any(Some(type_)) {
            return Ok(true);
        }

        let prop = self.get_property_of_type_(type_, property_name, None)?;
        Ok(matches!(
            prop.as_ref(),
            Some(prop) if self.is_property_accessible(
                node,
                is_super,
                false,
                type_,
                prop,
            )?
        ))
    }

    pub(super) fn is_property_accessible(
        &self,
        node: &Node,
        is_super: bool,
        is_write: bool,
        containing_type: &Type,
        property: &Symbol,
    ) -> io::Result<bool> {
        if self.is_type_any(Some(containing_type)) {
            return Ok(true);
        }

        if let Some(property_value_declaration) = property
            .maybe_value_declaration()
            .as_ref()
            .filter(|property_value_declaration| {
                is_private_identifier_class_element_declaration(property_value_declaration)
            })
        {
            let decl_class = get_containing_class(property_value_declaration);
            return Ok(!is_optional_chain(node)
                && find_ancestor(Some(node), |parent: &Node| {
                    matches!(
                        decl_class.as_ref(),
                        Some(decl_class) if ptr::eq(
                            parent,
                            &**decl_class
                        )
                    )
                })
                .is_some());
        }

        self.check_property_accessibility_at_location(
            node,
            is_super,
            is_write,
            containing_type,
            property,
            Option::<&Node>::None,
        )
    }

    pub(super) fn get_for_in_variable_symbol(
        &self,
        node: &Node, /*ForInStatement*/
    ) -> io::Result<Option<Gc<Symbol>>> {
        let initializer = &node.as_for_in_statement().initializer;
        if initializer.kind() == SyntaxKind::VariableDeclarationList {
            let variable = initializer
                .as_variable_declaration_list()
                .declarations
                .get(0)
                .cloned();
            if let Some(variable) = variable.as_ref().filter(|variable| {
                !is_binding_pattern(variable.as_variable_declaration().maybe_name())
            }) {
                return self.get_symbol_of_node(variable);
            }
        } else if initializer.kind() == SyntaxKind::Identifier {
            return Ok(Some(self.get_resolved_symbol(initializer)?));
        }
        Ok(None)
    }

    pub(super) fn has_numeric_property_names(&self, type_: &Type) -> io::Result<bool> {
        Ok(self.get_index_infos_of_type(type_)?.len() == 1
            && self
                .get_index_info_of_type_(type_, &self.number_type())?
                .is_some())
    }

    pub(super) fn is_for_in_variable_for_numeric_property_names(
        &self,
        expr: &Node, /*Expression*/
    ) -> io::Result<bool> {
        let e = skip_parentheses(expr, None);
        if e.kind() == SyntaxKind::Identifier {
            let symbol = self.get_resolved_symbol(&e)?;
            if symbol.flags().intersects(SymbolFlags::Variable) {
                let mut child = expr.node_wrapper();
                let mut node = expr.maybe_parent();
                while let Some(node_present) = node.as_ref() {
                    if node_present.kind() == SyntaxKind::ForInStatement && {
                        let node_as_for_in_statement = node_present.as_for_in_statement();
                        Gc::ptr_eq(&child, &node_as_for_in_statement.statement)
                            && matches!(
                                self.get_for_in_variable_symbol(node_present)?.as_ref(),
                                Some(for_in_variable_symbol) if Gc::ptr_eq(
                                    for_in_variable_symbol,
                                    &symbol
                                )
                            )
                            && self
                                .has_numeric_property_names(&*self.get_type_of_expression(
                                    &node_as_for_in_statement.expression,
                                )?)?
                    } {
                        return Ok(true);
                    }
                    child = node_present.clone();
                    node = node_present.maybe_parent();
                }
            }
        }
        Ok(false)
    }

    pub(super) fn check_indexed_access(
        &self,
        node: &Node, /*ElementAccessExpression*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Gc<Type>> {
        Ok(if node.flags().intersects(NodeFlags::OptionalChain) {
            self.check_element_access_chain(node, check_mode)?
        } else {
            self.check_element_access_expression(
                node,
                &*self
                    .check_non_null_expression(&node.as_element_access_expression().expression)?,
                check_mode,
            )?
        })
    }

    pub(super) fn check_element_access_chain(
        &self,
        node: &Node, /*ElementAccessChain*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Gc<Type>> {
        let node_as_element_access_expression = node.as_element_access_expression();
        let expr_type =
            self.check_expression(&node_as_element_access_expression.expression, None, None)?;
        let non_optional_type = self.get_optional_expression_type(
            &expr_type,
            &node_as_element_access_expression.expression,
        )?;
        self.propagate_optional_type_marker(
            &*self.check_element_access_expression(
                node,
                &*self.check_non_null_type(
                    &non_optional_type,
                    &node_as_element_access_expression.expression,
                )?,
                check_mode,
            )?,
            node,
            !Gc::ptr_eq(&non_optional_type, &expr_type),
        )
    }

    pub(super) fn check_element_access_expression(
        &self,
        node: &Node, /*ElementAccessExpression*/
        expr_type: &Type,
        check_mode: Option<CheckMode>,
    ) -> io::Result<Gc<Type>> {
        let object_type = if get_assignment_target_kind(node) != AssignmentKind::None
            || self.is_method_access_for_call(node)
        {
            self.get_widened_type(expr_type)?
        } else {
            expr_type.type_wrapper()
        };
        let node_as_element_access_expression = node.as_element_access_expression();
        let index_expression = &node_as_element_access_expression.argument_expression;
        let index_type = self.check_expression(index_expression, None, None)?;

        if self.is_error_type(&object_type) || Gc::ptr_eq(&object_type, &self.silent_never_type()) {
            return Ok(object_type);
        }

        if self.is_const_enum_object_type(&object_type) && !is_string_literal_like(index_expression)
        {
            self.error(
                Some(&**index_expression),
                &Diagnostics::A_const_enum_member_can_only_be_accessed_using_a_string_literal,
                None,
            );
            return Ok(self.error_type());
        }

        let effective_index_type =
            if self.is_for_in_variable_for_numeric_property_names(index_expression)? {
                self.number_type()
            } else {
                index_type.clone()
            };
        let access_flags = if is_assignment_target(node) {
            AccessFlags::Writing
                | if self.is_generic_object_type(&object_type)?
                    && !self.is_this_type_parameter(&object_type)
                {
                    AccessFlags::NoIndexSignatures
                } else {
                    AccessFlags::None
                }
        } else {
            AccessFlags::ExpressionPosition
        };
        let indexed_access_type = self
            .get_indexed_access_type_or_undefined(
                &object_type,
                &effective_index_type,
                Some(access_flags),
                Some(node),
                Option::<&Symbol>::None,
                None,
            )?
            .unwrap_or_else(|| self.error_type());
        self.check_indexed_access_index_type(
            &*self.get_flow_type_of_access_expression(
                node,
                (*self.get_node_links(node))
                    .borrow()
                    .resolved_symbol
                    .clone(),
                &indexed_access_type,
                index_expression,
                check_mode,
            )?,
            node,
        )
    }

    pub(super) fn call_like_expression_may_have_type_arguments(
        &self,
        node: &Node, /*CallLikeExpression*/
    ) -> bool {
        is_call_or_new_expression(node)
            || is_tagged_template_expression(node)
            || is_jsx_opening_like_element(node)
    }

    pub(super) fn resolve_untyped_call(
        &self,
        node: &Node, /*CallLikeExpression*/
    ) -> io::Result<Gc<Signature>> {
        if self.call_like_expression_may_have_type_arguments(node) {
            maybe_for_each(
                node.as_has_type_arguments().maybe_type_arguments().as_ref(),
                |type_argument: &Gc<Node>, _| -> Option<()> {
                    self.check_source_element(Some(&**type_argument));
                    None
                },
            );
        }

        if node.kind() == SyntaxKind::TaggedTemplateExpression {
            self.check_expression(&node.as_tagged_template_expression().template, None, None)?;
        } else if is_jsx_opening_like_element(node) {
            self.check_expression(&node.as_jsx_opening_like_element().attributes(), None, None)?;
        } else if node.kind() != SyntaxKind::Decorator {
            try_maybe_for_each(
                node.as_has_arguments().maybe_arguments().as_deref(),
                |argument: &Gc<Node>, _| -> io::Result<Option<()>> {
                    self.check_expression(argument, None, None)?;
                    Ok(None)
                },
            )?;
        }
        Ok(self.any_signature())
    }

    pub(super) fn resolve_error_call(
        &self,
        node: &Node, /*CallLikeExpression*/
    ) -> Gc<Signature> {
        self.resolve_untyped_call(node);
        self.unknown_signature()
    }

    pub(super) fn reorder_candidates(
        &self,
        signatures: &[Gc<Signature>],
        result: &mut Vec<Gc<Signature>>,
        call_chain_flags: SignatureFlags,
    ) -> io::Result<()> {
        let mut last_parent: Option<Gc<Node>> = None;
        let mut last_symbol: Option<Gc<Symbol>> = None;
        let mut cutoff_index = 0;
        let mut index: Option<usize> = None;
        let mut specialized_index: isize = -1;
        let mut splice_index: usize;
        Debug_.assert(result.is_empty(), None);
        for signature in signatures {
            let symbol = signature
                .declaration
                .as_ref()
                .try_and_then(|signature_declaration| {
                    self.get_symbol_of_node(signature_declaration)
                })?;
            let parent = signature
                .declaration
                .as_ref()
                .and_then(|signature_declaration| signature_declaration.maybe_parent());
            if match last_symbol.as_ref() {
                None => true,
                Some(last_symbol) => matches!(
                    symbol.as_ref(),
                    Some(symbol) if Gc::ptr_eq(
                        symbol,
                        last_symbol,
                    )
                ),
            } {
                if matches!(
                    last_parent.as_ref(),
                    Some(last_parent) if matches!(
                        parent.as_ref(),
                        Some(parent) if Gc::ptr_eq(
                            parent,
                            last_parent,
                        )
                    )
                ) {
                    index = Some(index.unwrap() + 1);
                } else {
                    last_parent = parent.clone();
                    index = Some(cutoff_index);
                }
            } else {
                cutoff_index = result.len();
                index = Some(cutoff_index);
                last_parent = parent.clone();
            }
            last_symbol = symbol.clone();

            if signature_has_literal_types(signature) {
                specialized_index += 1;
                splice_index = specialized_index.try_into().unwrap();
                cutoff_index += 1;
            } else {
                splice_index = index.unwrap();
            }

            result.insert(
                splice_index,
                if call_chain_flags != SignatureFlags::None {
                    self.get_optional_call_signature(signature.clone(), call_chain_flags)
                } else {
                    signature.clone()
                },
            );
        }

        Ok(())
    }
}
