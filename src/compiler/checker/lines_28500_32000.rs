#![allow(non_upper_case_globals)]

use regex::Regex;
use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{
    signature_has_rest_parameter, CheckMode, MinArgumentCountFlags, ResolveNameNameArg, TypeFacts,
    WideningKind,
};
use crate::{
    capitalize, contains, filter, find, find_ancestor, get_check_flags, get_containing_class,
    get_first_identifier, get_script_target_features, get_spelling_suggestion,
    has_effective_modifier, id_text, is_assignment_target, is_binding_pattern,
    is_entity_name_expression, is_function_like_declaration, is_import_call, is_named_declaration,
    is_optional_chain, is_private_identifier, is_private_identifier_class_element_declaration,
    is_property_access_expression, is_static, is_write_only_access, map_defined, skip_parentheses,
    starts_with, symbol_name, try_get_property_access_or_identifier_to_string,
    unescape_leading_underscores, CheckFlags, Debug_, Diagnostics, FunctionFlags, JsxReferenceKind,
    ModifierFlags, NamedDeclarationInterface, NodeFlags, Signature, SignatureFlags, StringOrRcNode,
    SymbolFlags, SymbolTable, Ternary, UnionOrIntersectionTypeInterface, UnionReduction, __String,
    get_function_flags, has_initializer, InferenceContext, Node, NodeInterface, Symbol,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
};
use local_macros::enum_unwrapped;

impl TypeChecker {
    pub(super) fn container_seems_to_be_empty_dom_element(&self, containing_type: &Type) -> bool {
        matches!(
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
            self.is_empty_object_type(containing_type)
    }

    pub(super) fn type_has_static_property(
        &self,
        prop_name: &__String,
        containing_type: &Type,
    ) -> bool {
        let prop = containing_type
            .maybe_symbol()
            .as_ref()
            .and_then(|containing_type_symbol| {
                self.get_property_of_type_(
                    &self.get_type_of_symbol(containing_type_symbol),
                    prop_name,
                    None,
                )
            });
        matches!(
            prop.as_ref().and_then(|prop| prop.maybe_value_declaration()).as_ref(),
            Some(prop_value_declaration) if is_static(prop_value_declaration)
        )
    }

    pub(super) fn get_suggested_lib_for_non_existent_name(
        &self,
        name: ResolveNameNameArg,
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
    ) -> Option<String> {
        let container = self.get_apparent_type(containing_type).maybe_symbol()?;
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
                return Some((*lib_target).to_owned());
            }
        }
        None
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_class_member(
        &self,
        name: &str,
        base_type: &Type,
    ) -> Option<Rc<Symbol>> {
        self.get_spelling_suggestion_for_name(
            name,
            &self.get_properties_of_type(base_type),
            SymbolFlags::ClassMember,
        )
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_property<TName: Into<StringOrRcNode>>(
        &self,
        name: TName, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> Option<Rc<Symbol>> {
        let mut props = self.get_properties_of_type(containing_type);
        let mut name: StringOrRcNode = name.into();
        if let StringOrRcNode::RcNode(ref name_ref) = name {
            let parent = name_ref.parent();
            if is_property_access_expression(&parent) {
                props = filter(&props, |prop: &Rc<Symbol>| {
                    self.is_valid_property_access_for_completions_(&parent, containing_type, prop)
                });
            }
            name = StringOrRcNode::String(id_text(name_ref));
        }
        let name = enum_unwrapped!(name, [StringOrRcNode, String]);
        self.get_spelling_suggestion_for_name(&name, &props, SymbolFlags::Value)
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_jsx_attribute<
        TName: Into<StringOrRcNode>,
    >(
        &self,
        name: TName, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> Option<Rc<Symbol>> {
        let name: StringOrRcNode = name.into();
        let str_name = match name {
            StringOrRcNode::String(name) => name,
            StringOrRcNode::RcNode(name) => id_text(&name),
        };
        let properties = self.get_properties_of_type(containing_type);
        let jsx_specific = if str_name == "for" {
            find(&properties, |x: &Rc<Symbol>, _| symbol_name(x) == "htmlFor").cloned()
        } else if str_name == "class" {
            find(&properties, |x: &Rc<Symbol>, _| {
                symbol_name(x) == "className"
            })
            .cloned()
        } else {
            None
        };
        jsx_specific.or_else(|| {
            self.get_spelling_suggestion_for_name(&str_name, &properties, SymbolFlags::Value)
        })
    }

    pub(super) fn get_suggestion_for_nonexistent_property<TName: Into<StringOrRcNode>>(
        &self,
        name: TName, /*Identifier | PrivateIdentifier*/
        containing_type: &Type,
    ) -> Option<String> {
        let suggestion = self.get_suggested_symbol_for_nonexistent_property(name, containing_type);
        suggestion
            .as_ref()
            .map(|suggestion| symbol_name(suggestion))
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_symbol_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        outer_name: &__String,
        meaning: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        // Debug.assert(outerName !== undefined, "outername should always be defined");
        let result = self.resolve_name_helper(
            location,
            outer_name,
            meaning,
            None,
            Some(outer_name.clone()),
            false,
            false,
            |symbols: &SymbolTable, name: &__String, meaning: SymbolFlags| {
                Debug_.assert_equal(outer_name, name, Some("name should equal outername"), None);
                let symbol = self.get_symbol(symbols, name, meaning);
                if symbol.is_some() {
                    return symbol;
                }
                let candidates: Vec<Rc<Symbol>>;
                if ptr::eq(symbols, &*self.globals()) {
                    let primitives = map_defined(
                        Some(["string", "number", "boolean", "object", "bigint", "symbol"]),
                        |s: &str, _| {
                            if symbols.contains_key(&__String::new(capitalize(s))) {
                                Some(
                                    self.create_symbol(
                                        SymbolFlags::TypeAlias,
                                        __String::new(s.to_owned()),
                                        None,
                                    )
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
        );
        result
    }

    pub(super) fn get_suggestion_for_nonexistent_symbol_<TLocation: Borrow<Node>>(
        &self,
        location: Option<TLocation>,
        outer_name: &__String,
        meaning: SymbolFlags,
    ) -> Option<String> {
        let symbol_result =
            self.get_suggested_symbol_for_nonexistent_symbol_(location, outer_name, meaning);
        symbol_result
            .as_ref()
            .map(|symbol_result| symbol_name(symbol_result))
    }

    pub(super) fn get_suggested_symbol_for_nonexistent_module(
        &self,
        name: &Node, /*Identifier*/
        target_module: &Symbol,
    ) -> Option<Rc<Symbol>> {
        if target_module.maybe_exports().is_some() {
            self.get_spelling_suggestion_for_name(
                &id_text(name),
                &self.get_exports_of_module_as_array(target_module),
                SymbolFlags::ModuleMember,
            )
        } else {
            None
        }
    }

    pub(super) fn get_suggestion_for_nonexistent_export(
        &self,
        name: &Node, /*Identifier*/
        target_module: &Symbol,
    ) -> Option<String> {
        let suggestion = self.get_suggested_symbol_for_nonexistent_module(name, target_module);
        suggestion
            .as_ref()
            .map(|suggestion| symbol_name(suggestion))
    }

    pub(super) fn get_suggestion_for_nonexistent_index_signature(
        &self,
        object_type: &Type,
        expr: &Node, /*ElementAccessExpression*/
        keyed_type: &Type,
    ) -> Option<String> {
        let suggested_method = if is_assignment_target(expr) {
            "set"
        } else {
            "get"
        };
        if !self.has_prop(object_type, keyed_type, suggested_method) {
            return None;
        }

        let mut suggestion = try_get_property_access_or_identifier_to_string(
            &expr.as_element_access_expression().expression,
        );
        match suggestion.as_mut() {
            None => {
                suggestion = Some(suggested_method.to_owned());
            }
            Some(suggestion) => {
                suggestion.push_str(&format!(".{}", suggested_method));
            }
        }

        suggestion
    }

    pub(super) fn has_prop(
        &self,
        object_type: &Type,
        keyed_type: &Type,
        name: &str, /*"set | "get"*/
    ) -> bool {
        let prop = self.get_property_of_object_type(object_type, &__String::new(name.to_owned()));
        if let Some(prop) = prop.as_ref() {
            let s = self.get_single_call_signature(&self.get_type_of_symbol(prop));
            return matches!(
                s.as_ref(),
                Some(s) if self.get_min_argument_count(s, None) >= 1 &&
                    self.is_type_assignable_to(keyed_type, &self.get_type_at_position(s, 0))
            );
        }
        false
    }

    pub(super) fn get_suggested_type_for_nonexistent_string_literal_type(
        &self,
        source: &Type, /*StringLiteralType*/
        target: &Type, /*UnionType*/
    ) -> Option<Rc<Type /*StringLiteralType*/>> {
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
            |type_: &Rc<Type>| Some(type_.as_string_literal_type().value.clone()),
        )
        .cloned()
    }

    pub(super) fn get_spelling_suggestion_for_name(
        &self,
        name: &str,
        symbols: &[Rc<Symbol>],
        meaning: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        let get_candidate_name = |candidate: &Rc<Symbol>| {
            let candidate_name = symbol_name(candidate);
            if starts_with(&candidate_name, "\"") {
                return None;
            }

            if candidate.flags().intersects(meaning) {
                return Some(candidate_name);
            }

            if candidate.flags().intersects(SymbolFlags::Alias) {
                let alias = self.try_resolve_alias(candidate);
                if matches!(
                    alias.as_ref(),
                    Some(alias) if alias.flags().intersects(meaning)
                ) {
                    return Some(candidate_name);
                }
            }
            None
        };

        get_spelling_suggestion(name, symbols, get_candidate_name).cloned()
    }

    pub(super) fn mark_property_as_referenced<TNodeForCheckWriteOnly: Borrow<Node>>(
        &self,
        prop: &Symbol,
        node_for_check_write_only: Option<TNodeForCheckWriteOnly>,
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

    pub(super) fn is_self_type_access<TParent: Borrow<Symbol>>(
        &self,
        name: &Node, /*Expression | QualifiedName*/
        parent: Option<TParent>,
    ) -> bool {
        let parent = parent.map(|parent| parent.borrow().symbol_wrapper());
        name.kind() == SyntaxKind::ThisKeyword
            || matches!(
                parent.as_ref(),
                Some(parent) if is_entity_name_expression(name) &&
                    Rc::ptr_eq(
                        parent,
                        &self.get_resolved_symbol(
                            &get_first_identifier(name)
                        ),
                    )
            )
    }

    pub(super) fn is_valid_property_access_(
        &self,
        node: &Node, /*PropertyAccessExpression | QualifiedName | ImportTypeNode*/
        property_name: &__String,
    ) -> bool {
        match node.kind() {
            SyntaxKind::PropertyAccessExpression => {
                let node_as_property_access_expression = node.as_property_access_expression();
                self.is_valid_property_access_with_type(
                    node,
                    node_as_property_access_expression.expression.kind()
                        == SyntaxKind::SuperKeyword,
                    property_name,
                    &self.get_widened_type(&self.check_expression(
                        &node_as_property_access_expression.expression,
                        None,
                        None,
                    )),
                )
            }
            SyntaxKind::QualifiedName => self.is_valid_property_access_with_type(
                node,
                false,
                property_name,
                &self.get_widened_type(&self.check_expression(
                    &node.as_qualified_name().left,
                    None,
                    None,
                )),
            ),
            SyntaxKind::ImportType => self.is_valid_property_access_with_type(
                node,
                false,
                property_name,
                &self.get_type_from_type_node_(node),
            ),
            _ => unreachable!(),
        }
    }

    pub fn is_valid_property_access_for_completions_(
        &self,
        node: &Node, /*PropertyAccessExpression | ImportTypeNode | QualifiedName*/
        type_: &Type,
        property: &Symbol,
    ) -> bool {
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
        property_name: &__String,
        type_: &Type,
    ) -> bool {
        if self.is_type_any(Some(type_)) {
            return true;
        }

        let prop = self.get_property_of_type_(type_, property_name, None);
        matches!(
            prop.as_ref(),
            Some(prop) if self.is_property_accessible(
                node,
                is_super,
                false,
                type_,
                prop,
            )
        )
    }

    pub(super) fn is_property_accessible(
        &self,
        node: &Node,
        is_super: bool,
        is_write: bool,
        containing_type: &Type,
        property: &Symbol,
    ) -> bool {
        if self.is_type_any(Some(containing_type)) {
            return true;
        }

        if let Some(property_value_declaration) = property
            .maybe_value_declaration()
            .as_ref()
            .filter(|property_value_declaration| {
                is_private_identifier_class_element_declaration(property_value_declaration)
            })
        {
            let decl_class = get_containing_class(property_value_declaration);
            return !is_optional_chain(node)
                && find_ancestor(Some(node), |parent: &Node| {
                    matches!(
                        decl_class.as_ref(),
                        Some(decl_class) if ptr::eq(
                            parent,
                            &**decl_class
                        )
                    )
                })
                .is_some();
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
    ) -> Option<Rc<Symbol>> {
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
            return Some(self.get_resolved_symbol(initializer));
        }
        None
    }

    pub(super) fn has_numeric_property_names(&self, type_: &Type) -> bool {
        self.get_index_infos_of_type(type_).len() == 1
            && self
                .get_index_info_of_type_(type_, &self.number_type())
                .is_some()
    }

    pub(super) fn is_for_in_variable_for_numeric_property_names(
        &self,
        expr: &Node, /*Expression*/
    ) -> bool {
        let e = skip_parentheses(expr, None);
        if e.kind() == SyntaxKind::Identifier {
            let symbol = self.get_resolved_symbol(&e);
            if symbol.flags().intersects(SymbolFlags::Variable) {
                let mut child = expr.node_wrapper();
                let mut node = expr.maybe_parent();
                while let Some(node_present) = node.as_ref() {
                    if node_present.kind() == SyntaxKind::ForInStatement && {
                        let node_as_for_in_statement = node_present.as_for_in_statement();
                        Rc::ptr_eq(&child, &node_as_for_in_statement.statement)
                            && matches!(
                                self.get_for_in_variable_symbol(node_present).as_ref(),
                                Some(for_in_variable_symbol) if Rc::ptr_eq(
                                    for_in_variable_symbol,
                                    &symbol
                                )
                            )
                            && self.has_numeric_property_names(
                                &self.get_type_of_expression(&node_as_for_in_statement.expression),
                            )
                    } {
                        return true;
                    }
                    child = node_present.clone();
                    node = node_present.maybe_parent();
                }
            }
        }
        false
    }

    pub(super) fn check_indexed_access(
        &self,
        node: &Node, /*ElementAccessExpression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        if node.flags().intersects(NodeFlags::OptionalChain) {
            self.check_element_access_chain(node, check_mode)
        } else {
            self.check_element_access_expression(
                node,
                &self.check_non_null_expression(&node.as_element_access_expression().expression),
                check_mode,
            )
        }
    }

    pub(super) fn check_element_access_chain(
        &self,
        node: &Node, /*ElementAccessChain*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        let node_as_element_access_expression = node.as_element_access_expression();
        let expr_type =
            self.check_expression(&node_as_element_access_expression.expression, None, None);
        let non_optional_type = self.get_optional_expression_type(
            &expr_type,
            &node_as_element_access_expression.expression,
        );
        self.propagate_optional_type_marker(
            &self.check_element_access_expression(
                node,
                &self.check_non_null_type(
                    &non_optional_type,
                    &node_as_element_access_expression.expression,
                ),
                check_mode,
            ),
            node,
            !Rc::ptr_eq(&non_optional_type, &expr_type),
        )
    }

    pub(super) fn check_element_access_expression(
        &self,
        node: &Node, /*ElementAccessExpression*/
        expr_type: &Type,
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
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

    pub(super) fn check_deprecated_signature(
        &self,
        signature: &Signature,
        node: &Node, /*CallLikeExpression*/
    ) {
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
