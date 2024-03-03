use std::{convert::TryInto, io};

use id_arena::Id;
use local_macros::enum_unwrapped;
use regex::Regex;

use super::{signature_has_literal_types, CheckMode, ResolveNameNameArg};
use crate::{
    capitalize, contains, find_ancestor, get_assignment_target_kind, get_check_flags,
    get_containing_class, get_first_identifier, get_script_target_features,
    get_spelling_suggestion, has_effective_modifier, id_text, is_assignment_target,
    is_binding_pattern, is_call_or_new_expression, is_entity_name_expression,
    is_function_like_declaration, is_jsx_opening_like_element, is_named_declaration,
    is_optional_chain, is_private_identifier, is_private_identifier_class_element_declaration,
    is_property_access_expression, is_static, is_string_literal_like,
    is_tagged_template_expression, is_write_only_access, map_defined, released,
    return_ok_default_if_none, skip_parentheses, starts_with, symbol_name, try_filter,
    try_get_property_access_or_identifier_to_string, try_get_spelling_suggestion,
    try_maybe_for_each, unescape_leading_underscores, AccessFlags, AssignmentKind, CheckFlags,
    Debug_, Diagnostics, HasArena, InArena, ModifierFlags, NamedDeclarationInterface, Node,
    NodeFlags, NodeInterface, OptionInArena, OptionTry, Signature, SignatureFlags, StrOrRcNode,
    Symbol, SymbolFlags, SymbolInterface, SymbolTable, SyntaxKind, Type, TypeChecker, TypeFlags,
    TypeInterface, UnionOrIntersectionTypeInterface,
};

impl TypeChecker {
    pub(super) fn container_seems_to_be_empty_dom_element(
        &self,
        containing_type: Id<Type>,
    ) -> io::Result<bool> {
        Ok(matches!(
            self.compiler_options.ref_(self).lib.as_ref(),
            Some(compiler_options_lib) if compiler_options_lib.into_iter().position(|lib_item| lib_item == "dom").is_none()
        ) &&
            self.every_contained_type(
                containing_type,
                |type_: Id<Type>| matches!(
                    type_.ref_(self).maybe_symbol(),
                    Some(type_symbol) if {
                        lazy_static! {
                            static ref element_regex: Regex = Regex::new("^(EventTarget|Node|((HTML[a-zA-Z]*)?Element))$").unwrap();
                        }
                        element_regex.is_match(&unescape_leading_underscores(type_symbol.ref_(self).escaped_name()))
                    }
                )
            ) &&
            self.is_empty_object_type(containing_type)?)
    }

    pub(super) fn type_has_static_property(
        &self,
        prop_name: &str, /*__String*/
        containing_type: Id<Type>,
    ) -> io::Result<bool> {
        let prop = released!(containing_type.ref_(self).maybe_symbol()).try_and_then(
            |containing_type_symbol| {
                self.get_property_of_type_(
                    self.get_type_of_symbol(containing_type_symbol)?,
                    prop_name,
                    None,
                )
            },
        )?;
        Ok(matches!(
            prop.and_then(|prop| prop.ref_(self).maybe_value_declaration()),
            Some(prop_value_declaration) if is_static(prop_value_declaration, self)
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
        containing_type: Id<Type>,
    ) -> io::Result<Option<String>> {
        let container = return_ok_default_if_none!(self
            .get_apparent_type(containing_type)?
            .ref_(self)
            .maybe_symbol());
        let all_features = get_script_target_features();
        let lib_targets = all_features.keys();
        for lib_target in lib_targets {
            let features_of_lib = all_features.get(lib_target).unwrap();
            let container_name = symbol_name(container, self);
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
        base_type: Id<Type>,
    ) -> io::Result<Option<Id<Symbol>>> {
        self.get_spelling_suggestion_for_name(
            name,
            &self.get_properties_of_type(base_type)?,
            SymbolFlags::ClassMember,
        )
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_property<'name>(
        &self,
        name: impl Into<StrOrRcNode<'name>>, /*Identifier | PrivateIdentifier*/
        containing_type: Id<Type>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let props = self.get_properties_of_type(containing_type)?;
        let mut name = name.into();
        let name_inner_rc_node = match name.clone() {
            StrOrRcNode::RcNode(name) => Some(name),
            _ => None,
        };
        let mut props_: Option<Vec<Id<Symbol>>> = None;
        let mut did_set_props = false;
        let name_inner_rc_node_ref = name_inner_rc_node.refed(self);
        if let Some(name_inner_rc_node_ref) = name_inner_rc_node_ref.as_ref() {
            let parent = name_inner_rc_node_ref.parent();
            if is_property_access_expression(&parent.ref_(self)) {
                did_set_props = true;
                props_ = Some(try_filter(&props, |&prop: &Id<Symbol>| {
                    self.is_valid_property_access_for_completions_(parent, containing_type, prop)
                })?);
            }
            name = StrOrRcNode::Str(id_text(&name_inner_rc_node_ref));
        }
        if !did_set_props {
            props_ = Some(props);
        }
        let props = props_.unwrap();
        let name = enum_unwrapped!(name, [StrOrRcNode, Str]);
        self.get_spelling_suggestion_for_name(name, &props, SymbolFlags::Value)
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_jsx_attribute<'name>(
        &self,
        name: impl Into<StrOrRcNode<'name>>, /*Identifier | PrivateIdentifier*/
        containing_type: Id<Type>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let name = name.into();
        let str_name = match &name {
            StrOrRcNode::Str(name) => (*name).to_owned(),
            StrOrRcNode::RcNode(name) => id_text(&name.ref_(self)).to_owned(),
        };
        let properties = self.get_properties_of_type(containing_type)?;
        let jsx_specific = if str_name == "for" {
            properties
                .iter()
                .copied()
                .find(|&x| symbol_name(x, self) == "htmlFor")
        } else if str_name == "class" {
            properties
                .iter()
                .copied()
                .find(|&x| symbol_name(x, self) == "className")
        } else {
            None
        };
        jsx_specific.try_or_else(|| {
            self.get_spelling_suggestion_for_name(&str_name, &properties, SymbolFlags::Value)
        })
    }

    pub(super) fn get_suggestion_for_nonexistent_property<'name>(
        &self,
        name: impl Into<StrOrRcNode<'name>>, /*Identifier | PrivateIdentifier*/
        containing_type: Id<Type>,
    ) -> io::Result<Option<String>> {
        let suggestion =
            self.get_suggested_symbol_for_nonexistent_property(name, containing_type)?;
        Ok(suggestion.map(|suggestion| symbol_name(suggestion, self)))
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_symbol_(
        &self,
        location: Option<Id<Node>>,
        outer_name: &str, /*__String*/
        meaning: SymbolFlags,
    ) -> io::Result<Option<Id<Symbol>>> {
        // Debug.assert(outerName !== undefined, "outername should always be defined");
        let result = self.resolve_name_helper(
            location,
            outer_name,
            meaning,
            None,
            Some(outer_name),
            false,
            false,
            |symbols: Id<SymbolTable>, name: &str /*__String*/, meaning: SymbolFlags| {
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
                let candidates: Vec<Id<Symbol>>;
                if symbols == self.globals {
                    let primitives = map_defined(
                        Some(["string", "number", "boolean", "object", "bigint", "symbol"]),
                        |s: &str, _| {
                            if symbols.ref_(self).contains_key(&capitalize(s)) {
                                Some(
                                    self.alloc_symbol(
                                        self.create_symbol(
                                            SymbolFlags::TypeAlias,
                                            s.to_owned(),
                                            None,
                                        )
                                        .into(),
                                    ),
                                )
                            } else {
                                None
                            }
                        },
                    );
                    candidates = primitives
                        .into_iter()
                        .chain(symbols.ref_(self).values().cloned())
                        .collect();
                } else {
                    candidates = symbols.ref_(self).values().cloned().collect();
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
        location: Option<Id<Node>>,
        outer_name: &str, /*__String*/
        meaning: SymbolFlags,
    ) -> io::Result<Option<String>> {
        let symbol_result =
            self.get_suggested_symbol_for_nonexistent_symbol_(location, outer_name, meaning)?;
        Ok(symbol_result.map(|symbol_result| symbol_name(symbol_result, self)))
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_module(
        &self,
        name: Id<Node>, /*Identifier*/
        target_module: Id<Symbol>,
    ) -> io::Result<Option<Id<Symbol>>> {
        Ok(if target_module.ref_(self).maybe_exports().is_some() {
            self.get_spelling_suggestion_for_name(
                &id_text(&name.ref_(self)),
                &self.get_exports_of_module_as_array(target_module)?,
                SymbolFlags::ModuleMember,
            )?
        } else {
            None
        })
    }

    pub fn get_suggestion_for_nonexistent_export(
        &self,
        name: Id<Node>, /*Identifier*/
        target_module: Id<Symbol>,
    ) -> io::Result<Option<String>> {
        let suggestion = self.get_suggested_symbol_for_nonexistent_module(name, target_module)?;
        Ok(suggestion.map(|suggestion| symbol_name(suggestion, self)))
    }

    pub(super) fn get_suggestion_for_nonexistent_index_signature(
        &self,
        object_type: Id<Type>,
        expr: Id<Node>, /*ElementAccessExpression*/
        keyed_type: Id<Type>,
    ) -> io::Result<Option<String>> {
        let suggested_method = if is_assignment_target(expr, self) {
            "set"
        } else {
            "get"
        };
        if !self.has_prop(object_type, keyed_type, suggested_method)? {
            return Ok(None);
        }

        let mut suggestion = try_get_property_access_or_identifier_to_string(
            expr.ref_(self).as_element_access_expression().expression,
            self,
        );
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
        object_type: Id<Type>,
        keyed_type: Id<Type>,
        name: &str, /*"set | "get"*/
    ) -> io::Result<bool> {
        let prop = self.get_property_of_object_type(object_type, name)?;
        if let Some(prop) = prop {
            let s = self.get_single_call_signature(self.get_type_of_symbol(prop)?)?;
            return Ok(matches!(
                s,
                Some(s) if self.get_min_argument_count(s, None)? >= 1 &&
                    self.is_type_assignable_to(keyed_type, self.get_type_at_position(s, 0)?)?
            ));
        }
        Ok(false)
    }

    pub(super) fn get_suggested_type_for_nonexistent_string_literal_type(
        &self,
        source: Id<Type>, /*StringLiteralType*/
        target: Id<Type>, /*UnionType*/
    ) -> Option<Id<Type /*StringLiteralType*/>> {
        let candidates = target
            .ref_(self)
            .as_union_type()
            .types()
            .into_iter()
            .filter(|&&type_| {
                type_
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::StringLiteral)
            })
            .cloned()
            .collect::<Vec<_>>();
        get_spelling_suggestion(
            &source.ref_(self).as_string_literal_type().value,
            &candidates,
            |&type_: &Id<Type>| Some(type_.ref_(self).as_string_literal_type().value.clone()),
        )
        .copied()
    }

    pub(super) fn get_spelling_suggestion_for_name(
        &self,
        name: &str,
        symbols: &[Id<Symbol>],
        meaning: SymbolFlags,
    ) -> io::Result<Option<Id<Symbol>>> {
        let get_candidate_name = |&candidate: &Id<Symbol>| -> io::Result<_> {
            let candidate_name = symbol_name(candidate, self);
            if starts_with(&candidate_name, "\"") {
                return Ok(None);
            }

            if candidate.ref_(self).flags().intersects(meaning) {
                return Ok(Some(candidate_name));
            }

            if candidate.ref_(self).flags().intersects(SymbolFlags::Alias) {
                let alias = self.try_resolve_alias(candidate)?;
                if matches!(
                    alias,
                    Some(alias) if alias.ref_(self).flags().intersects(meaning)
                ) {
                    return Ok(Some(candidate_name));
                }
            }
            Ok(None)
        };

        Ok(try_get_spelling_suggestion(name, symbols, get_candidate_name)?.copied())
    }

    pub(super) fn mark_property_as_referenced(
        &self,
        prop: Id<Symbol>,
        node_for_check_write_only: Option<Id<Node>>,
        is_self_type_access: bool,
    ) {
        let value_declaration = /*prop &&*/ if prop.ref_(self).flags().intersects(SymbolFlags::ClassMember) {
            prop.ref_(self).maybe_value_declaration()
        } else {
            None
        };
        if value_declaration.is_none() {
            return;
        }
        let value_declaration = value_declaration.unwrap();
        let has_private_modifier =
            has_effective_modifier(value_declaration, ModifierFlags::Private, self);
        let has_private_identifier = matches!(
            prop.ref_(self).maybe_value_declaration(),
            Some(prop_value_declaration) if is_named_declaration(&prop_value_declaration.ref_(self)) &&
                is_private_identifier(&prop_value_declaration.ref_(self).as_named_declaration().name().ref_(self))
        );
        if !has_private_modifier && !has_private_identifier {
            return;
        }
        if matches!(
            node_for_check_write_only,
            Some(node_for_check_write_only) if is_write_only_access(node_for_check_write_only, self)
        ) && !prop.ref_(self).flags().intersects(SymbolFlags::SetAccessor)
        {
            return;
        }
        if is_self_type_access {
            let containing_method = find_ancestor(
                node_for_check_write_only,
                |node: Id<Node>| is_function_like_declaration(&node.ref_(self)),
                self,
            );
            if matches!(
                containing_method,
                Some(containing_method) if matches!(
                    containing_method.ref_(self).maybe_symbol(),
                    Some(containing_method_symbol) if containing_method_symbol == prop
                )
            ) {
                return;
            }
        }

        if get_check_flags(&prop.ref_(self)).intersects(CheckFlags::Instantiated) {
            self.get_symbol_links(prop).ref_(self).target.unwrap()
        } else {
            prop
        }
        .ref_(self)
        .set_is_referenced(Some(SymbolFlags::All));
    }

    pub(super) fn is_self_type_access(
        &self,
        name: Id<Node>, /*Expression | QualifiedName*/
        parent: Option<Id<Symbol>>,
    ) -> io::Result<bool> {
        Ok(name.ref_(self).kind() == SyntaxKind::ThisKeyword
            || matches!(
                parent,
                Some(parent) if is_entity_name_expression(name, self) &&
                    parent == self.get_resolved_symbol(
                        get_first_identifier(name, self)
                    )?
            ))
    }

    pub(super) fn is_valid_property_access_(
        &self,
        node: Id<Node>, /*PropertyAccessExpression | QualifiedName | ImportTypeNode*/
        property_name: &str, /*__String*/
    ) -> io::Result<bool> {
        Ok(match node.ref_(self).kind() {
            SyntaxKind::PropertyAccessExpression => {
                let node_ref = node.ref_(self);
                let node_as_property_access_expression = node_ref.as_property_access_expression();
                self.is_valid_property_access_with_type(
                    node,
                    node_as_property_access_expression
                        .expression
                        .ref_(self)
                        .kind()
                        == SyntaxKind::SuperKeyword,
                    property_name,
                    self.get_widened_type(self.check_expression(
                        node_as_property_access_expression.expression,
                        None,
                        None,
                    )?)?,
                )?
            }
            SyntaxKind::QualifiedName => self.is_valid_property_access_with_type(
                node,
                false,
                property_name,
                self.get_widened_type(self.check_expression(
                    node.ref_(self).as_qualified_name().left,
                    None,
                    None,
                )?)?,
            )?,
            SyntaxKind::ImportType => self.is_valid_property_access_with_type(
                node,
                false,
                property_name,
                self.get_type_from_type_node_(node)?,
            )?,
            _ => unreachable!(),
        })
    }

    pub fn is_valid_property_access_for_completions_(
        &self,
        node: Id<Node>, /*PropertyAccessExpression | ImportTypeNode | QualifiedName*/
        type_: Id<Type>,
        property: Id<Symbol>,
    ) -> io::Result<bool> {
        self.is_property_accessible(
            node,
            node.ref_(self).kind() == SyntaxKind::PropertyAccessExpression
                && node
                    .ref_(self)
                    .as_property_access_expression()
                    .expression
                    .ref_(self)
                    .kind()
                    == SyntaxKind::SuperKeyword,
            false,
            type_,
            property,
        )
    }

    pub(super) fn is_valid_property_access_with_type(
        &self,
        node: Id<Node>, /*PropertyAccessExpression | QualifiedName | ImportTypeNode*/
        is_super: bool,
        property_name: &str, /*__String*/
        type_: Id<Type>,
    ) -> io::Result<bool> {
        if self.is_type_any(Some(type_)) {
            return Ok(true);
        }

        let prop = self.get_property_of_type_(type_, property_name, None)?;
        Ok(matches!(
            prop,
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
        node: Id<Node>,
        is_super: bool,
        is_write: bool,
        containing_type: Id<Type>,
        property: Id<Symbol>,
    ) -> io::Result<bool> {
        if self.is_type_any(Some(containing_type)) {
            return Ok(true);
        }

        if let Some(property_value_declaration) = property
            .ref_(self)
            .maybe_value_declaration()
            .filter(|&property_value_declaration| {
                is_private_identifier_class_element_declaration(property_value_declaration, self)
            })
        {
            let decl_class = get_containing_class(property_value_declaration, self);
            return Ok(!is_optional_chain(&node.ref_(self))
                && find_ancestor(
                    Some(node),
                    |parent: Id<Node>| decl_class == Some(parent),
                    self,
                )
                .is_some());
        }

        self.check_property_accessibility_at_location(
            node,
            is_super,
            is_write,
            containing_type,
            property,
            Option::<Id<Node>>::None,
        )
    }

    pub(super) fn get_for_in_variable_symbol(
        &self,
        node: Id<Node>, /*ForInStatement*/
    ) -> io::Result<Option<Id<Symbol>>> {
        let initializer = node.ref_(self).as_for_in_statement().initializer;
        if initializer.ref_(self).kind() == SyntaxKind::VariableDeclarationList {
            let variable = initializer
                .ref_(self)
                .as_variable_declaration_list()
                .declarations
                .ref_(self)
                .get(0)
                .copied();
            if let Some(variable) = variable.filter(|variable| {
                !is_binding_pattern(
                    variable
                        .ref_(self)
                        .as_variable_declaration()
                        .maybe_name()
                        .refed(self)
                        .as_deref(),
                )
            }) {
                return self.get_symbol_of_node(variable);
            }
        } else if initializer.ref_(self).kind() == SyntaxKind::Identifier {
            return Ok(Some(self.get_resolved_symbol(initializer)?));
        }
        Ok(None)
    }

    pub(super) fn has_numeric_property_names(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(self.get_index_infos_of_type(type_)?.len() == 1
            && self
                .get_index_info_of_type_(type_, self.number_type())?
                .is_some())
    }

    pub(super) fn is_for_in_variable_for_numeric_property_names(
        &self,
        expr: Id<Node>, /*Expression*/
    ) -> io::Result<bool> {
        let e = skip_parentheses(expr, None, self);
        if e.ref_(self).kind() == SyntaxKind::Identifier {
            let symbol = self.get_resolved_symbol(e)?;
            if symbol.ref_(self).flags().intersects(SymbolFlags::Variable) {
                let mut child = expr;
                let mut node = expr.ref_(self).maybe_parent();
                while let Some(node_present) = node {
                    if node_present.ref_(self).kind() == SyntaxKind::ForInStatement && {
                        let node_present_ref = node_present.ref_(self);
                        let node_as_for_in_statement = node_present_ref.as_for_in_statement();
                        child == node_as_for_in_statement.statement
                            && matches!(
                                self.get_for_in_variable_symbol(node_present)?,
                                Some(for_in_variable_symbol) if for_in_variable_symbol == symbol
                            )
                            && self.has_numeric_property_names(
                                self.get_type_of_expression(node_as_for_in_statement.expression)?,
                            )?
                    } {
                        return Ok(true);
                    }
                    child = node_present;
                    node = node_present.ref_(self).maybe_parent();
                }
            }
        }
        Ok(false)
    }

    pub(super) fn check_indexed_access(
        &self,
        node: Id<Node>, /*ElementAccessExpression*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        Ok(
            if node.ref_(self).flags().intersects(NodeFlags::OptionalChain) {
                self.check_element_access_chain(node, check_mode)?
            } else {
                self.check_element_access_expression(
                    node,
                    self.check_non_null_expression(released!(
                        node.ref_(self).as_element_access_expression().expression
                    ))?,
                    check_mode,
                )?
            },
        )
    }

    pub(super) fn check_element_access_chain(
        &self,
        node: Id<Node>, /*ElementAccessChain*/
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let node_ref = node.ref_(self);
        let node_as_element_access_expression = node_ref.as_element_access_expression();
        let expr_type =
            self.check_expression(node_as_element_access_expression.expression, None, None)?;
        let non_optional_type = self.get_optional_expression_type(
            expr_type,
            node_as_element_access_expression.expression,
        )?;
        self.propagate_optional_type_marker(
            self.check_element_access_expression(
                node,
                self.check_non_null_type(
                    non_optional_type,
                    node_as_element_access_expression.expression,
                )?,
                check_mode,
            )?,
            node,
            non_optional_type != expr_type,
        )
    }

    pub(super) fn check_element_access_expression(
        &self,
        node: Id<Node>, /*ElementAccessExpression*/
        expr_type: Id<Type>,
        check_mode: Option<CheckMode>,
    ) -> io::Result<Id<Type>> {
        let object_type = if get_assignment_target_kind(node, self) != AssignmentKind::None
            || self.is_method_access_for_call(node)
        {
            self.get_widened_type(expr_type)?
        } else {
            expr_type
        };
        let index_expression = node
            .ref_(self)
            .as_element_access_expression()
            .argument_expression;
        let index_type = self.check_expression(index_expression, None, None)?;

        if self.is_error_type(object_type) || object_type == self.silent_never_type() {
            return Ok(object_type);
        }

        if self.is_const_enum_object_type(object_type)
            && !is_string_literal_like(&index_expression.ref_(self))
        {
            self.error(
                Some(index_expression),
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
        let access_flags = if is_assignment_target(node, self) {
            AccessFlags::Writing
                | if self.is_generic_object_type(object_type)?
                    && !self.is_this_type_parameter(object_type)
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
                object_type,
                effective_index_type,
                Some(access_flags),
                Some(node),
                Option::<Id<Symbol>>::None,
                None,
            )?
            .unwrap_or_else(|| self.error_type());
        self.check_indexed_access_index_type(
            self.get_flow_type_of_access_expression(
                node,
                released!(self.get_node_links(node).ref_(self).resolved_symbol),
                indexed_access_type,
                index_expression,
                check_mode,
            )?,
            node,
        )
    }

    pub(super) fn call_like_expression_may_have_type_arguments(
        &self,
        node: Id<Node>, /*CallLikeExpression*/
    ) -> bool {
        is_call_or_new_expression(&node.ref_(self))
            || is_tagged_template_expression(&node.ref_(self))
            || is_jsx_opening_like_element(&node.ref_(self))
    }

    pub(super) fn resolve_untyped_call(
        &self,
        node: Id<Node>, /*CallLikeExpression*/
    ) -> io::Result<Id<Signature>> {
        if self.call_like_expression_may_have_type_arguments(node) {
            try_maybe_for_each(
                node.ref_(self)
                    .as_has_type_arguments()
                    .maybe_type_arguments()
                    .refed(self)
                    .as_deref(),
                |&type_argument: &Id<Node>, _| -> io::Result<Option<()>> {
                    self.check_source_element(Some(type_argument))?;
                    Ok(None)
                },
            )?;
        }

        if node.ref_(self).kind() == SyntaxKind::TaggedTemplateExpression {
            self.check_expression(
                released!(node.ref_(self).as_tagged_template_expression().template),
                None,
                None,
            )?;
        } else if is_jsx_opening_like_element(&node.ref_(self)) {
            self.check_expression(
                released!(node.ref_(self).as_jsx_opening_like_element().attributes()),
                None,
                None,
            )?;
        } else if node.ref_(self).kind() != SyntaxKind::Decorator {
            try_maybe_for_each(
                released!(node
                    .ref_(self)
                    .as_has_arguments()
                    .maybe_arguments()
                    .refed(self)
                    .as_deref()
                    .cloned())
                .as_deref(),
                |&argument: &Id<Node>, _| -> io::Result<Option<()>> {
                    self.check_expression(argument, None, None)?;
                    Ok(None)
                },
            )?;
        }
        Ok(self.any_signature())
    }

    pub(super) fn resolve_error_call(
        &self,
        node: Id<Node>, /*CallLikeExpression*/
    ) -> io::Result<Id<Signature>> {
        self.resolve_untyped_call(node)?;
        Ok(self.unknown_signature())
    }

    pub(super) fn reorder_candidates(
        &self,
        signatures: &[Id<Signature>],
        result: &mut Vec<Id<Signature>>,
        call_chain_flags: SignatureFlags,
    ) -> io::Result<()> {
        let mut last_parent: Option<Id<Node>> = None;
        let mut last_symbol: Option<Id<Symbol>> = None;
        let mut cutoff_index = 0;
        let mut index: Option<usize> = None;
        let mut specialized_index: isize = -1;
        let mut splice_index: usize;
        Debug_.assert(result.is_empty(), None);
        for &signature in signatures {
            let symbol =
                signature
                    .ref_(self)
                    .declaration
                    .try_and_then(|signature_declaration| {
                        self.get_symbol_of_node(signature_declaration)
                    })?;
            let parent = signature
                .ref_(self)
                .declaration
                .and_then(|signature_declaration| signature_declaration.ref_(self).maybe_parent());
            if match last_symbol {
                None => true,
                Some(last_symbol) => matches!(
                    symbol,
                    Some(symbol) if symbol == last_symbol
                ),
            } {
                if matches!(
                    last_parent,
                    Some(last_parent) if parent == Some(last_parent)
                ) {
                    index = Some(index.unwrap() + 1);
                } else {
                    last_parent = parent;
                    index = Some(cutoff_index);
                }
            } else {
                cutoff_index = result.len();
                index = Some(cutoff_index);
                last_parent = parent;
            }
            last_symbol = symbol.clone();

            if signature_has_literal_types(&signature.ref_(self)) {
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
