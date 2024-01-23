use std::{
    borrow::{Borrow, Cow},
    collections::HashMap,
    io,
};

use derive_builder::Builder;
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use id_arena::Id;
use itertools::Itertools;
use once_cell::sync::Lazy;
use regex::Captures;

use crate::{
    chain_bundle, gc_cell_ref_mut_unwrapped, gc_cell_ref_unwrapped, BaseNodeFactorySynthetic,
    CompilerOptions, EmitHelperFactory, GeneratedIdentifierFlags, JsxEmit,
    NamedDeclarationInterface, Node, NodeFactory, NodeInterface, TransformationContext,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface, _d,
    add_emit_helpers, create_expression_for_jsx_element, create_expression_for_jsx_fragment,
    create_expression_from_entity_name, create_jsx_factory_expression, filter, flatten,
    get_emit_script_target, get_jsx_implicit_import_base, get_jsx_runtime_import,
    get_line_and_character_of_position, get_semantic_jsx_children, id_text,
    insert_statement_after_custom_prologue, is_expression, is_external_module,
    is_external_or_common_js_module, is_identifier, is_intrinsic_jsx_name, is_jsx_attribute,
    is_jsx_spread_attribute, is_line_break, is_source_file, is_string_double_quoted,
    is_white_space_single_line, map, map_defined, maybe_get_original_node, maybe_visit_node, regex,
    single_or_undefined, span_map, start_on_new_line, utf16_encode_as_string, visit_each_child,
    visit_node, Debug_, GetOrInsertDefault, HasStatementsInterface, LiteralLikeNodeInterface,
    MapOrDefault, Matches, NodeArray, NodeArrayOrVec, NodeExt, NodeFlags, NonEmpty, Number,
    ReadonlyTextRange, ScriptTarget, SyntaxKind, TransformFlags, VisitResult,
    HasArena, AllArenas, InArena, static_arena,
    TransformNodesTransformationResult, CoreTransformationContext,
};

#[derive(Builder, Default, Trace, Finalize)]
#[builder(setter(into))]
pub(super) struct PerFileState {
    #[builder(default)]
    pub import_specifier: Option<String>,
    #[builder(default)]
    pub filename_declaration: Option<Id<Node /*VariableDeclaration & { name: Identifier; }*/>>,
    #[builder(default)]
    pub utilized_implicit_runtime_imports:
        Option<HashMap<String, HashMap<String, Id<Node /*ImportSpecifier*/>>>>,
}

#[derive(Trace, Finalize)]
pub(super) struct TransformJsx {
    #[unsafe_ignore_trace]
    _arena: *const AllArenas,
    context: Id<TransformNodesTransformationResult>,
    compiler_options: Gc<CompilerOptions>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    current_source_file: GcCell<Option<Id<Node /*SourceFile*/>>>,
    current_file_state: GcCell<Option<PerFileState>>,
}

impl TransformJsx {
    fn new(context: Id<TransformNodesTransformationResult>, arena: *const AllArenas) -> Self {
        let arena_ref = unsafe { &*arena };
        let context_ref = context.ref_(arena_ref);
        Self {
            _arena: arena,
            factory: context_ref.factory(),
            compiler_options: context_ref.get_compiler_options(),
            context,
            current_source_file: _d(),
            current_file_state: _d(),
        }
    }

    pub(super) fn maybe_current_source_file(&self) -> Option<Id<Node /*SourceFile*/>> {
        self.current_source_file.borrow().clone()
    }

    pub(super) fn current_source_file(&self) -> Id<Node /*SourceFile*/> {
        self.current_source_file.borrow().clone().unwrap()
    }

    pub(super) fn set_current_source_file(
        &self,
        current_source_file: Option<Id<Node /*SourceFile*/>>,
    ) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    pub(super) fn current_file_state(&self) -> GcCellRef<PerFileState> {
        gc_cell_ref_unwrapped(&self.current_file_state)
    }

    pub(super) fn current_file_state_mut(
        &self,
    ) -> GcCellRefMut<Option<PerFileState>, PerFileState> {
        gc_cell_ref_mut_unwrapped(&self.current_file_state)
    }

    pub(super) fn set_current_file_state(&self, current_file_state: Option<PerFileState>) {
        *self.current_file_state.borrow_mut() = current_file_state;
    }

    fn emit_helpers(&self) -> Gc<EmitHelperFactory> {
        self.context.ref_(self).get_emit_helper_factory()
    }

    fn get_current_file_name_expression(&self) -> Id<Node /*Identifier*/> {
        if let Some(current_file_state_filename_declaration) =
            self.current_file_state().filename_declaration
        {
            return current_file_state_filename_declaration
                .ref_(self).as_variable_declaration()
                .name();
        }
        let declaration = self.factory.create_variable_declaration(
            Some(self.factory.create_unique_name(
                "_jsxFileName",
                Some(GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel),
            )),
            None,
            None,
            Some(
                self.factory.create_string_literal(
                    self.current_source_file()
                        .ref_(self).as_source_file()
                        .file_name()
                        .clone(),
                    None,
                    None,
                ),
            ),
        );
        self.current_file_state_mut().filename_declaration = Some(declaration);
        declaration.ref_(self).as_variable_declaration().name()
    }

    fn get_jsx_factory_callee_primitive(&self, is_static_children: bool) -> &'static str /*"jsx" | "jsxs" | "jsxDEV"*/
    {
        if self.compiler_options.jsx == Some(JsxEmit::ReactJSXDev) {
            "jsxDEV"
        } else if is_static_children {
            "jsxs"
        } else {
            "jsx"
        }
    }

    fn get_jsx_factory_callee(&self, is_static_children: bool) -> Id<Node> {
        let type_ = self.get_jsx_factory_callee_primitive(is_static_children);
        self.get_implicit_import_for_name(type_)
    }

    fn get_implicit_jsx_fragment_reference(&self) -> Id<Node> {
        self.get_implicit_import_for_name("Fragment")
    }

    fn get_implicit_import_for_name(&self, name: &str) -> Id<Node> {
        let import_source = if name == "createElement" {
            self.current_file_state().import_specifier.clone().unwrap()
        } else {
            get_jsx_runtime_import(
                self.current_file_state().import_specifier.as_deref(),
                &self.compiler_options,
            )
            .unwrap()
        };
        let existing = self
            .current_file_state()
            .utilized_implicit_runtime_imports
            .as_ref()
            .and_then(|current_file_state_utilized_implicit_runtime_imports| {
                current_file_state_utilized_implicit_runtime_imports.get(&import_source)
            })
            .and_then(
                |current_file_state_utilized_implicit_runtime_imports_value| {
                    current_file_state_utilized_implicit_runtime_imports_value.get(name)
                },
            )
            .cloned();
        if let Some(existing) = existing {
            return existing.ref_(self).as_import_specifier().name;
        }
        let mut current_file_state = self.current_file_state_mut();
        let specifier_source_imports = current_file_state
            .utilized_implicit_runtime_imports
            .get_or_insert_default_()
            .entry(import_source)
            .or_default();
        let generated_name = self.factory.create_unique_name(
            &format!("_{name}"),
            Some(
                GeneratedIdentifierFlags::Optimistic
                    | GeneratedIdentifierFlags::FileLevel
                    | GeneratedIdentifierFlags::AllowNameSubstitution,
            ),
        );
        let specifier = self.factory.create_import_specifier(
            false,
            Some(self.factory.create_identifier(name)),
            generated_name.clone(),
        );
        generated_name
            .ref_(self).as_identifier()
            .set_generated_import_reference(Some(specifier));
        specifier_source_imports.insert(name.to_owned(), specifier);
        generated_name
    }

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_source_file = node_ref.as_source_file();
        if node_as_source_file.is_declaration_file() {
            return node;
        }

        self.set_current_source_file(Some(node));
        self.set_current_file_state(Some(
            PerFileStateBuilder::default()
                .import_specifier(get_jsx_implicit_import_base(
                    &self.compiler_options,
                    Some(&node.ref_(self)),
                ))
                .build()
                .unwrap(),
        ));
        let mut visited =
            visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self);
        add_emit_helpers(visited, self.context.ref_(self).read_emit_helpers().as_deref(), self);
        let mut statements: NodeArrayOrVec = visited.ref_(self).as_source_file().statements().into();
        if let Some(current_file_state_filename_declaration) =
            self.current_file_state().filename_declaration
        {
            let mut statements_as_vec: Vec<Id<Node>> = (&statements).into();
            insert_statement_after_custom_prologue(
                &mut statements_as_vec,
                Some(self.factory.create_variable_statement(
                    Option::<Gc<NodeArray>>::None,
                    self.factory.create_variable_declaration_list(
                        vec![current_file_state_filename_declaration],
                        Some(NodeFlags::Const),
                    ),
                )),
                self,
            );
            statements = statements_as_vec.into();
        }
        if let Some(current_file_state_utilized_implicit_runtime_imports) = self
            .current_file_state()
            .utilized_implicit_runtime_imports
            .as_ref()
        {
            for (import_source, import_specifiers_map) in
                current_file_state_utilized_implicit_runtime_imports
            {
                if is_external_module(&node.ref_(self)) {
                    let import_statement = self
                        .factory
                        .create_import_declaration(
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            Some(self.factory.create_import_clause(
                                false,
                                None,
                                Some(self.factory.create_named_imports(
                                    import_specifiers_map.values().cloned().collect_vec(),
                                )),
                            )),
                            self.factory
                                .create_string_literal(import_source.clone(), None, None),
                            None,
                        )
                        .set_parent_recursive(false, self);
                    let mut statements_as_vec: Vec<Id<Node>> = (&statements).into();
                    insert_statement_after_custom_prologue(
                        &mut statements_as_vec,
                        Some(import_statement),
                        self,
                    );
                    statements = statements_as_vec.into();
                } else if is_external_or_common_js_module(&node.ref_(self)) {
                    let require_statement = self
                        .factory
                        .create_variable_statement(
                            Option::<Gc<NodeArray>>::None,
                            self.factory.create_variable_declaration_list(
                                vec![self.factory.create_variable_declaration(
                                    Some(
                                        self.factory.create_object_binding_pattern(
                                            import_specifiers_map
                                                .values()
                                                .map(|s| {
                                                    let s_ref = s.ref_(self);
                                                    let s_as_import_specifier = s_ref.as_import_specifier();
                                                    self.factory.create_binding_element(
                                                        None,
                                                        s_as_import_specifier.property_name.clone(),
                                                        s_as_import_specifier.name.clone(),
                                                        None,
                                                    )
                                                })
                                                .collect_vec(),
                                        ),
                                    ),
                                    None,
                                    None,
                                    Some(self.factory.create_call_expression(
                                        self.factory.create_identifier("require"),
                                        Option::<Gc<NodeArray>>::None,
                                        Some(vec![self.factory.create_string_literal(
                                            import_source.clone(),
                                            None,
                                            None,
                                        )]),
                                    )),
                                )],
                                Some(NodeFlags::Const),
                            ),
                        )
                        .set_parent_recursive(false, self);
                    let mut statements_as_vec: Vec<Id<Node>> = (&statements).into();
                    insert_statement_after_custom_prologue(
                        &mut statements_as_vec,
                        Some(require_statement),
                        self,
                    );
                    statements = statements_as_vec.into();
                } else {
                }
            }
        }
        if !matches!(
            &statements,
            NodeArrayOrVec::NodeArray(statements) if Gc::ptr_eq(
                statements,
                &visited.ref_(self).as_source_file().statements()
            )
        ) {
            visited = self
                .factory
                .update_source_file(visited, statements, None, None, None, None, None);
        }
        self.set_current_file_state(None);
        visited
    }

    fn visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        if node
            .ref_(self).transform_flags()
            .intersects(TransformFlags::ContainsJsx)
        {
            self.visitor_worker(node)
        } else {
            Some(node.into())
        }
    }

    fn visitor_worker(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        match node.ref_(self).kind() {
            SyntaxKind::JsxElement => self.visit_jsx_element(node, false).map(Into::into),
            SyntaxKind::JsxSelfClosingElement => self
                .visit_jsx_self_closing_element(node, false)
                .map(Into::into),
            SyntaxKind::JsxFragment => self.visit_jsx_fragment(node, false).map(Into::into),
            SyntaxKind::JsxExpression => self.visit_jsx_expression(node).map(Into::into),
            _ => Some(
                visit_each_child(node, |node: Id<Node>| self.visitor(node), &*self.context.ref_(self), self).into(),
            ),
        }
    }

    fn transform_jsx_child_to_expression(
        &self,
        node: Id<Node>, /*JsxChild*/
    ) -> Option<Id<Node /*Expression*/>> {
        match node.ref_(self).kind() {
            SyntaxKind::JsxText => self.visit_jsx_text(node),
            SyntaxKind::JsxExpression => self.visit_jsx_expression(node),
            SyntaxKind::JsxElement => self.visit_jsx_element(node, true),
            SyntaxKind::JsxSelfClosingElement => self.visit_jsx_self_closing_element(node, true),
            SyntaxKind::JsxFragment => self.visit_jsx_fragment(node, true),
            _ => Debug_.fail_bad_syntax_kind(&node.ref_(self), None),
        }
    }

    fn has_key_after_props_spread(&self, node: Id<Node> /*JsxOpeningLikeElement*/) -> bool {
        let node_ref = node.ref_(self);
        let node_as_jsx_opening_like_element = node_ref.as_jsx_opening_like_element();
        let mut spread = false;
        for elem in &node_as_jsx_opening_like_element
            .attributes()
            .ref_(self).as_jsx_attributes()
            .properties
        {
            if is_jsx_spread_attribute(&elem.ref_(self)) {
                spread = true;
            } else if spread
                && is_jsx_attribute(&elem.ref_(self))
                && elem.ref_(self).as_jsx_attribute().name.ref_(self).as_identifier().escaped_text == "key"
            {
                return true;
            }
        }
        false
    }

    fn should_use_create_element(&self, node: Id<Node> /*JsxOpeningLikeElement*/) -> bool {
        self.current_file_state().import_specifier.is_none()
            || self.has_key_after_props_spread(node)
    }

    fn visit_jsx_element(
        &self,
        node: Id<Node>, /*JsxElement*/
        is_child: bool,
    ) -> Option<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_jsx_element = node_ref.as_jsx_element();
        if self.should_use_create_element(node_as_jsx_element.opening_element) {
            self.visit_jsx_opening_like_element_create_element(
                node_as_jsx_element.opening_element,
                Some(&node_as_jsx_element.children),
                is_child,
                &*node.ref_(self),
            )
        } else {
            self.visit_jsx_opening_like_element_jsx(
                node_as_jsx_element.opening_element,
                Some(&node_as_jsx_element.children),
                is_child,
                &*node.ref_(self),
            )
        }
    }

    fn visit_jsx_self_closing_element(
        &self,
        node: Id<Node>, /*JsxSelfClosingElement*/
        is_child: bool,
    ) -> Option<Id<Node>> {
        if self.should_use_create_element(node) {
            self.visit_jsx_opening_like_element_create_element(node, None, is_child, &*node.ref_(self))
        } else {
            self.visit_jsx_opening_like_element_jsx(node, None, is_child, &*node.ref_(self))
        }
    }

    fn visit_jsx_fragment(
        &self,
        node: Id<Node>, /*JsxFragment*/
        is_child: bool,
    ) -> Option<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_jsx_fragment = node_ref.as_jsx_fragment();
        if self.current_file_state().import_specifier.is_none() {
            self.visit_jsx_opening_fragment_create_element(
                node_as_jsx_fragment.opening_fragment,
                &node_as_jsx_fragment.children,
                is_child,
                &*node.ref_(self),
            )
        } else {
            self.visit_jsx_opening_fragment_jsx(
                node_as_jsx_fragment.opening_fragment,
                &node_as_jsx_fragment.children,
                is_child,
                &*node.ref_(self),
            )
        }
    }

    fn convert_jsx_children_to_children_prop_object(
        &self,
        children: &[Id<Node /*JsxChild*/>],
    ) -> Option<Id<Node>> {
        let prop = self.convert_jsx_children_to_children_prop_assignment(children);
        prop.map(|prop| {
            self.factory
                .create_object_literal_expression(Some(vec![prop]), None)
        })
    }

    fn convert_jsx_children_to_children_prop_assignment(
        &self,
        children: &[Id<Node /*JsxChild*/>],
    ) -> Option<Id<Node>> {
        let non_whitespace_children = get_semantic_jsx_children(children, self);
        if non_whitespace_children.len() == 1
            && non_whitespace_children[0]
                .ref_(self).as_jsx_expression()
                .dot_dot_dot_token
                .is_none()
        {
            let result = self.transform_jsx_child_to_expression(non_whitespace_children[0]);
            return result
                .map(|result| self.factory.create_property_assignment("children", result));
        }
        let result = map_defined(Some(children), |&child: &Id<Node>, _| {
            self.transform_jsx_child_to_expression(child)
        });
        (!result.is_empty()).then(|| {
            self.factory.create_property_assignment(
                "children",
                self.factory
                    .create_array_literal_expression(Some(result), None),
            )
        })
    }

    fn visit_jsx_opening_like_element_jsx(
        &self,
        node: Id<Node>, /*JsxOpeningLikeElement*/
        children: Option<&[Id<Node /*JsxChild*/>]>,
        is_child: bool,
        location: &impl ReadonlyTextRange,
    ) -> Option<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_jsx_opening_like_element = node_ref.as_jsx_opening_like_element();
        let tag_name = self.get_tag_name(node);
        let children_prop = children
            .non_empty()
            .and_then(|children| self.convert_jsx_children_to_children_prop_assignment(children));
        let key_attr = node_as_jsx_opening_like_element
            .attributes()
            .ref_(self).as_jsx_attributes()
            .properties
            .iter()
            .find(|p| {
                p.ref_(self).as_named_declaration().maybe_name().matches(|p_name| {
                    is_identifier(&p_name.ref_(self)) && p_name.ref_(self).as_identifier().escaped_text == "key"
                })
            })
            .cloned();
        let attrs: NodeArrayOrVec = key_attr.map_or_else(
            || {
                node_as_jsx_opening_like_element
                    .attributes()
                    .ref_(self).as_jsx_attributes()
                    .properties
                    .clone()
                    .into()
            },
            |key_attr| {
                filter(
                    &node_as_jsx_opening_like_element
                        .attributes()
                        .ref_(self).as_jsx_attributes()
                        .properties,
                    |&p: &Id<Node>| p != key_attr,
                )
                .into()
            },
        );
        let object_properties = if !attrs.is_empty() {
            self.transform_jsx_attributes_to_object_props(&attrs, children_prop)
        } else {
            self.factory.create_object_literal_expression(
                Some(children_prop.map_or_default(|children_prop| vec![children_prop])),
                None,
            )
        };
        Some(self.visit_jsx_opening_like_element_or_fragment_jsx(
            tag_name,
            object_properties,
            key_attr,
            children.unwrap_or(&[]),
            is_child,
            location,
        ))
    }

    fn visit_jsx_opening_like_element_or_fragment_jsx(
        &self,
        tag_name: Id<Node>,          /*Expression*/
        object_properties: Id<Node>, /*Expression*/
        key_attr: Option<Id<Node /*JsxAttribute*/>>,
        children: &[Id<Node /*JsxChild*/>],
        is_child: bool,
        location: &impl ReadonlyTextRange,
    ) -> Id<Node> {
        let non_whitespace_children = get_semantic_jsx_children(children, self);
        let is_static_children = non_whitespace_children.len() > 1
            || non_whitespace_children
                .get(0)
                .matches(|non_whitespace_children_0| {
                    non_whitespace_children_0
                        .ref_(self).as_jsx_expression()
                        .dot_dot_dot_token
                        .is_some()
                });
        let mut args/*: Expression[]*/ = vec![
            tag_name,
            object_properties,
            key_attr.map_or_else(
                || self.factory.create_void_zero(),
                |key_attr| {
                    self.transform_jsx_attribute_initializer(key_attr.ref_(self).as_jsx_attribute().initializer)
                }
            )
        ];
        if self.compiler_options.jsx == Some(JsxEmit::ReactJSXDev) {
            let original_file = maybe_get_original_node(self.maybe_current_source_file(), self);
            if let Some(original_file) =
                original_file.filter(|original_file| is_source_file(&original_file.ref_(self)))
            {
                args.push(if is_static_children {
                    self.factory.create_true()
                } else {
                    self.factory.create_false()
                });
                let line_col = get_line_and_character_of_position(
                    original_file.ref_(self).as_source_file(),
                    usize::try_from(location.pos()).unwrap(),
                );
                args.push(self.factory.create_object_literal_expression(
                    Some(vec![
                        self.factory.create_property_assignment(
                            "fileName",
                            self.get_current_file_name_expression(),
                        ),
                        self.factory.create_property_assignment(
                            "lineNumber",
                            self.factory.create_numeric_literal(
                                Number::new((line_col.line + 1) as f64),
                                None,
                            ),
                        ),
                        self.factory.create_property_assignment(
                            "columnNumber",
                            self.factory.create_numeric_literal(
                                Number::new((line_col.character + 1) as f64),
                                None,
                            ),
                        ),
                    ]),
                    None,
                ));
                args.push(self.factory.create_this());
            }
        }
        let element = self
            .factory
            .create_call_expression(
                self.get_jsx_factory_callee(is_static_children),
                Option::<Gc<NodeArray>>::None,
                Some(args),
            )
            .set_text_range(Some(location), self);

        if is_child {
            start_on_new_line(element, self);
        }

        element
    }

    fn visit_jsx_opening_like_element_create_element(
        &self,
        node: Id<Node>, /*JsxOpeningLikeElement*/
        children: Option<&[Id<Node /*JsxChild*/>]>,
        is_child: bool,
        location: &impl ReadonlyTextRange,
    ) -> Option<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_jsx_opening_like_element = node_ref.as_jsx_opening_like_element();
        let tag_name = self.get_tag_name(node);
        let attrs = node_as_jsx_opening_like_element
            .attributes()
            .ref_(self).as_jsx_attributes()
            .properties
            .clone();
        let object_properties = if !attrs.is_empty() {
            self.transform_jsx_attributes_to_object_props(&attrs, Option::<Id<Node>>::None)
        } else {
            self.factory.create_null()
        };

        let callee = if self.current_file_state().import_specifier.is_none() {
            create_jsx_factory_expression(
                &self.factory,
                self.context
                    .ref_(self).get_emit_resolver()
                    .get_jsx_factory_entity(self.maybe_current_source_file()),
                self.compiler_options.react_namespace.as_deref(),
                node,
            )
        } else {
            self.get_implicit_import_for_name("createElement")
        };

        let element = create_expression_for_jsx_element(
            &self.factory,
            callee,
            tag_name,
            Some(object_properties),
            Some(&map_defined(children, |&child: &Id<Node>, _| {
                self.transform_jsx_child_to_expression(child)
            })),
            location,
        );

        if is_child {
            start_on_new_line(element, self);
        }

        Some(element)
    }

    fn visit_jsx_opening_fragment_jsx(
        &self,
        _node: Id<Node>, /*JsxOpeningFragment*/
        children: &[Id<Node /*JsxChild*/>],
        is_child: bool,
        location: &impl ReadonlyTextRange,
    ) -> Option<Id<Node>> {
        let mut children_props: Option<Id<Node /*Expression*/>> = _d();
        if
        /*children &&*/
        !children.is_empty() {
            let result = self.convert_jsx_children_to_children_prop_object(children);
            if result.is_some() {
                children_props = result;
            }
        }
        Some(self.visit_jsx_opening_like_element_or_fragment_jsx(
            self.get_implicit_jsx_fragment_reference(),
            children_props.unwrap_or_else(|| {
                self.factory
                    .create_object_literal_expression(Some(vec![]), None)
            }),
            Option::<Id<Node>>::None,
            children,
            is_child,
            location,
        ))
    }

    fn visit_jsx_opening_fragment_create_element(
        &self,
        node: Id<Node>, /*JsxOpeningFragment*/
        children: &[Id<Node /*JsxChild*/>],
        is_child: bool,
        location: &impl ReadonlyTextRange,
    ) -> Option<Id<Node>> {
        let element = create_expression_for_jsx_fragment(
            &self.factory,
            self.context
                .ref_(self).get_emit_resolver()
                .get_jsx_factory_entity(self.maybe_current_source_file()),
            self.context
                .ref_(self).get_emit_resolver()
                .get_jsx_fragment_factory_entity(self.maybe_current_source_file()),
            self.compiler_options.react_namespace.as_deref().unwrap(),
            &map_defined(Some(children), |&child: &Id<Node>, _| {
                self.transform_jsx_child_to_expression(child)
            }),
            node,
            location,
        );

        if is_child {
            start_on_new_line(element, self);
        }

        Some(element)
    }

    fn transform_jsx_spread_attribute_to_spread_assignment(
        &self,
        node: Id<Node>, /*JsxSpreadAttribute*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_jsx_spread_attribute = node_ref.as_jsx_spread_attribute();
        self.factory.create_spread_assignment(visit_node(
            node_as_jsx_spread_attribute.expression,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        ))
    }

    fn transform_jsx_attributes_to_object_props(
        &self,
        attrs: &[Id<Node /*JsxSpreadAttribute | JsxAttribute*/>],
        children: Option<Id<Node /*PropertyAssignment*/>>,
    ) -> Id<Node> {
        let target = get_emit_script_target(&self.compiler_options);
        if
        /*target &&*/
        target >= ScriptTarget::ES2018 {
            self.factory.create_object_literal_expression(
                Some(self.transform_jsx_attributes_to_props(attrs, children)),
                None,
            )
        } else {
            self.transform_jsx_attributes_to_expression(attrs, children)
        }
    }

    fn transform_jsx_attributes_to_props(
        &self,
        attrs: &[Id<Node /*JsxSpreadAttribute | JsxAttribute*/>],
        children: Option<Id<Node /*PropertyAssignment*/>>,
    ) -> Vec<Id<Node>> {
        let mut props = flatten(&span_map(
            attrs,
            |node: &Id<Node>, _| is_jsx_spread_attribute(&node.ref_(self)),
            |attrs: &[Id<Node>], &is_spread: &bool, _, _| {
                map(attrs, |&attr: &Id<Node>, _| {
                    if is_spread {
                        self.transform_jsx_spread_attribute_to_spread_assignment(attr)
                    } else {
                        self.transform_jsx_attribute_to_object_literal_element(attr)
                    }
                })
            },
        ));
        if let Some(children) = children {
            props.push(children);
        }
        props
    }

    fn transform_jsx_attributes_to_expression(
        &self,
        attrs: &[Id<Node /*JsxSpreadAttribute | JsxAttribute*/>],
        children: Option<Id<Node /*PropertyAssignment*/>>,
    ) -> Id<Node> {
        let mut expressions = flatten(&span_map(
            attrs,
            |node: &Id<Node>, _| is_jsx_spread_attribute(&node.ref_(self)),
            |attrs: &[Id<Node>], &is_spread: &bool, _, _| {
                if is_spread {
                    map(attrs, |&attr: &Id<Node>, _| {
                        self.transform_jsx_spread_attribute_to_expression(attr)
                    })
                } else {
                    vec![self.factory.create_object_literal_expression(
                        Some(map(attrs, |&attr: &Id<Node>, _| {
                            self.transform_jsx_attribute_to_object_literal_element(attr)
                        })),
                        None,
                    )]
                }
            },
        ));

        if is_jsx_spread_attribute(&attrs[0].ref_(self)) {
            expressions.insert(
                0,
                self.factory
                    .create_object_literal_expression(Option::<Gc<NodeArray>>::None, None),
            );
        }

        if let Some(children) = children {
            expressions.push(
                self.factory
                    .create_object_literal_expression(Some(vec![children]), None),
            );
        }

        single_or_undefined(Some(&expressions))
            .cloned()
            .unwrap_or_else(|| self.emit_helpers().create_assign_helper(expressions))
    }

    fn transform_jsx_spread_attribute_to_expression(
        &self,
        node: Id<Node>, /*JsxSpreadAttribute*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_jsx_spread_attribute = node_ref.as_jsx_spread_attribute();
        visit_node(
            node_as_jsx_spread_attribute.expression,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )
    }

    fn transform_jsx_attribute_to_object_literal_element(
        &self,
        node: Id<Node>, /*JsxAttribute*/
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_jsx_attribute = node_ref.as_jsx_attribute();
        let name = self.get_attribute_name(node);
        let expression =
            self.transform_jsx_attribute_initializer(node_as_jsx_attribute.initializer);
        self.factory.create_property_assignment(name, expression)
    }

    fn transform_jsx_attribute_initializer(
        &self,
        node: Option<Id<Node /*StringLiteral | JsxExpression*/>>,
    ) -> Id<Node /*Expression*/> {
        let Some(node) = node else {
            return self.factory.create_true();
        };
        if node.ref_(self).kind() == SyntaxKind::StringLiteral {
            let node_ref = node.ref_(self);
            let node_as_string_literal = node_ref.as_string_literal();
            let single_quote = node_as_string_literal
                .single_quote
                .unwrap_or_else(|| !is_string_double_quoted(node, self.current_source_file(), self));
            let ret = self.factory
                .create_string_literal(
                    self.try_decode_entities(&node_as_string_literal.text())
                        .map(Cow::into_owned)
                        .unwrap_or_else(|| node_as_string_literal.text().clone()),
                    Some(single_quote),
                    None,
                )
                .set_text_range(Some(&*node.ref_(self)), self);
            ret
        } else if node.ref_(self).kind() == SyntaxKind::JsxExpression {
            let node_ref = node.ref_(self);
            let node_as_jsx_expression = node_ref.as_jsx_expression();
            if node_as_jsx_expression.expression.is_none() {
                return self.factory.create_true();
            }
            visit_node(
                node_as_jsx_expression.expression.unwrap(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node| is_expression(node, self)),
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )
        } else {
            Debug_.fail_bad_syntax_kind(&node.ref_(self), None);
        }
    }

    fn visit_jsx_text(
        &self,
        node: Id<Node>, /*JsxText*/
    ) -> Option<Id<Node /*StringLiteral*/>> {
        let node_ref = node.ref_(self);
        let node_as_jsx_text = node_ref.as_jsx_text();
        let fixed = self.fixup_whitespace_and_decode_entities(&node_as_jsx_text.text());
        fixed.map(|fixed| self.factory.create_string_literal(fixed, None, None))
    }

    fn fixup_whitespace_and_decode_entities(&self, text: &str) -> Option<String> {
        let mut acc: Option<String> = _d();
        let mut first_non_whitespace = Some(0);
        let mut last_non_whitespace = None;
        let text_as_chars = text.chars().collect_vec();
        for (i, c) in text_as_chars.iter().copied().enumerate() {
            if is_line_break(c) {
                if let (Some(first_non_whitespace), Some(last_non_whitespace)) =
                    (first_non_whitespace, last_non_whitespace)
                {
                    acc = Some(
                        self.add_line_of_jsx_text(
                            acc.as_deref(),
                            &text_as_chars[first_non_whitespace..last_non_whitespace + 1]
                                .into_iter()
                                .collect::<String>(),
                        )
                        .into_owned(),
                    );
                }

                first_non_whitespace = None;
            } else if !is_white_space_single_line(c) {
                last_non_whitespace = Some(i);
                if first_non_whitespace.is_none() {
                    first_non_whitespace = Some(i);
                }
            }
        }

        match first_non_whitespace {
            Some(first_non_whitespace) => Some(
                self.add_line_of_jsx_text(
                    acc.as_deref(),
                    &text_as_chars[first_non_whitespace..]
                        .into_iter()
                        .collect::<String>(),
                )
                .into_owned(),
            ),
            None => acc,
        }
    }

    fn add_line_of_jsx_text<'trimmed_line>(
        &self,
        acc: Option<&str>,
        trimmed_line: &'trimmed_line str,
    ) -> Cow<'trimmed_line, str> {
        let decoded = self.decode_entities(trimmed_line);
        match acc {
            Some(acc) => format!("{acc}_{decoded}").into(),
            None => decoded,
        }
    }

    fn decode_entities<'text>(&self, text: &'text str) -> Cow<'text, str> {
        regex!(r#"&((#((\d+)|x([\da-fA-F]+)))|(\w+));"#).replace_all(text, |captures: &Captures| {
            let match_ = &captures[0];
            let decimal = captures.get(4);
            let hex = captures.get(5);
            let word = captures.get(6);
            if let Some(decimal) = decimal {
                utf16_encode_as_string(decimal.as_str().parse().unwrap())
            } else if let Some(hex) = hex {
                utf16_encode_as_string(u32::from_str_radix(hex.as_str(), 16).unwrap())
            } else {
                let word = word.unwrap();
                let ch = entities.get(&word.as_str()).copied();
                ch.map_or_else(|| match_.to_owned(), utf16_encode_as_string)
            }
        })
    }

    fn try_decode_entities<'text>(&self, text: &'text str) -> Option<Cow<'text, str>> {
        let decoded = self.decode_entities(text);
        if &*decoded == text {
            None
        } else {
            Some(decoded)
        }
    }

    fn get_tag_name(
        &self,
        node: Id<Node>, /*JsxElement | JsxOpeningLikeElement*/
    ) -> Id<Node /*Expression*/> {
        if node.ref_(self).kind() == SyntaxKind::JsxElement {
            self.get_tag_name(node.ref_(self).as_jsx_element().opening_element)
        } else {
            let name = node.ref_(self).as_jsx_opening_like_element().tag_name();
            if is_identifier(&name.ref_(self)) && is_intrinsic_jsx_name(&name.ref_(self).as_identifier().escaped_text) {
                self.factory
                    .create_string_literal(id_text(&name.ref_(self)).to_owned(), None, None)
            } else {
                create_expression_from_entity_name(&self.factory, name)
            }
        }
    }

    fn get_attribute_name(
        &self,
        node: Id<Node>, /*JsxAttribute*/
    ) -> Id<Node /*StringLiteral | Identifier*/> {
        let node_ref = node.ref_(self);
        let node_as_jsx_attribute = node_ref.as_jsx_attribute();
        let name = node_as_jsx_attribute.name;
        let name_ref = name.ref_(self);
        let text = id_text(&name_ref);
        if regex!(r#"^[A-Za-z_]\w*$"#).is_match(text) {
            name
        } else {
            self.factory
                .create_string_literal(text.to_owned(), None, None)
        }
    }

    fn visit_jsx_expression(&self, node: Id<Node> /*JsxExpression*/) -> Option<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_jsx_expression = node_ref.as_jsx_expression();
        let expression = maybe_visit_node(
            node_as_jsx_expression.expression,
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node| is_expression(node, self)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        );
        if node_as_jsx_expression.dot_dot_dot_token.is_some() {
            Some(self.factory.create_spread_element(expression.unwrap()))
        } else {
            expression
        }
    }
}

impl TransformerInterface for TransformJsx {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(self.transform_source_file(node))
    }
}

impl HasArena for TransformJsx {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
pub(super) struct TransformJsxFactory {}

impl TransformJsxFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformJsxFactory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            Gc::new(Box::new(TransformJsx::new(context, &*static_arena()))),
        )
    }
}

pub fn transform_jsx() -> TransformerFactory {
    Gc::new(Box::new(TransformJsxFactory::new()))
}

static entities: Lazy<HashMap<&'static str, u32>> = Lazy::new(|| {
    HashMap::from_iter([
        ("quot", 0x0022),
        ("amp", 0x0026),
        ("apos", 0x0027),
        ("lt", 0x003C),
        ("gt", 0x003E),
        ("nbsp", 0x00A0),
        ("iexcl", 0x00A1),
        ("cent", 0x00A2),
        ("pound", 0x00A3),
        ("curren", 0x00A4),
        ("yen", 0x00A5),
        ("brvbar", 0x00A6),
        ("sect", 0x00A7),
        ("uml", 0x00A8),
        ("copy", 0x00A9),
        ("ordf", 0x00AA),
        ("laquo", 0x00AB),
        ("not", 0x00AC),
        ("shy", 0x00AD),
        ("reg", 0x00AE),
        ("macr", 0x00AF),
        ("deg", 0x00B0),
        ("plusmn", 0x00B1),
        ("sup2", 0x00B2),
        ("sup3", 0x00B3),
        ("acute", 0x00B4),
        ("micro", 0x00B5),
        ("para", 0x00B6),
        ("middot", 0x00B7),
        ("cedil", 0x00B8),
        ("sup1", 0x00B9),
        ("ordm", 0x00BA),
        ("raquo", 0x00BB),
        ("frac14", 0x00BC),
        ("frac12", 0x00BD),
        ("frac34", 0x00BE),
        ("iquest", 0x00BF),
        ("Agrave", 0x00C0),
        ("Aacute", 0x00C1),
        ("Acirc", 0x00C2),
        ("Atilde", 0x00C3),
        ("Auml", 0x00C4),
        ("Aring", 0x00C5),
        ("AElig", 0x00C6),
        ("Ccedil", 0x00C7),
        ("Egrave", 0x00C8),
        ("Eacute", 0x00C9),
        ("Ecirc", 0x00CA),
        ("Euml", 0x00CB),
        ("Igrave", 0x00CC),
        ("Iacute", 0x00CD),
        ("Icirc", 0x00CE),
        ("Iuml", 0x00CF),
        ("ETH", 0x00D0),
        ("Ntilde", 0x00D1),
        ("Ograve", 0x00D2),
        ("Oacute", 0x00D3),
        ("Ocirc", 0x00D4),
        ("Otilde", 0x00D5),
        ("Ouml", 0x00D6),
        ("times", 0x00D7),
        ("Oslash", 0x00D8),
        ("Ugrave", 0x00D9),
        ("Uacute", 0x00DA),
        ("Ucirc", 0x00DB),
        ("Uuml", 0x00DC),
        ("Yacute", 0x00DD),
        ("THORN", 0x00DE),
        ("szlig", 0x00DF),
        ("agrave", 0x00E0),
        ("aacute", 0x00E1),
        ("acirc", 0x00E2),
        ("atilde", 0x00E3),
        ("auml", 0x00E4),
        ("aring", 0x00E5),
        ("aelig", 0x00E6),
        ("ccedil", 0x00E7),
        ("egrave", 0x00E8),
        ("eacute", 0x00E9),
        ("ecirc", 0x00EA),
        ("euml", 0x00EB),
        ("igrave", 0x00EC),
        ("iacute", 0x00ED),
        ("icirc", 0x00EE),
        ("iuml", 0x00EF),
        ("eth", 0x00F0),
        ("ntilde", 0x00F1),
        ("ograve", 0x00F2),
        ("oacute", 0x00F3),
        ("ocirc", 0x00F4),
        ("otilde", 0x00F5),
        ("ouml", 0x00F6),
        ("divide", 0x00F7),
        ("oslash", 0x00F8),
        ("ugrave", 0x00F9),
        ("uacute", 0x00FA),
        ("ucirc", 0x00FB),
        ("uuml", 0x00FC),
        ("yacute", 0x00FD),
        ("thorn", 0x00FE),
        ("yuml", 0x00FF),
        ("OElig", 0x0152),
        ("oelig", 0x0153),
        ("Scaron", 0x0160),
        ("scaron", 0x0161),
        ("Yuml", 0x0178),
        ("fnof", 0x0192),
        ("circ", 0x02C6),
        ("tilde", 0x02DC),
        ("Alpha", 0x0391),
        ("Beta", 0x0392),
        ("Gamma", 0x0393),
        ("Delta", 0x0394),
        ("Epsilon", 0x0395),
        ("Zeta", 0x0396),
        ("Eta", 0x0397),
        ("Theta", 0x0398),
        ("Iota", 0x0399),
        ("Kappa", 0x039A),
        ("Lambda", 0x039B),
        ("Mu", 0x039C),
        ("Nu", 0x039D),
        ("Xi", 0x039E),
        ("Omicron", 0x039F),
        ("Pi", 0x03A0),
        ("Rho", 0x03A1),
        ("Sigma", 0x03A3),
        ("Tau", 0x03A4),
        ("Upsilon", 0x03A5),
        ("Phi", 0x03A6),
        ("Chi", 0x03A7),
        ("Psi", 0x03A8),
        ("Omega", 0x03A9),
        ("alpha", 0x03B1),
        ("beta", 0x03B2),
        ("gamma", 0x03B3),
        ("delta", 0x03B4),
        ("epsilon", 0x03B5),
        ("zeta", 0x03B6),
        ("eta", 0x03B7),
        ("theta", 0x03B8),
        ("iota", 0x03B9),
        ("kappa", 0x03BA),
        ("lambda", 0x03BB),
        ("mu", 0x03BC),
        ("nu", 0x03BD),
        ("xi", 0x03BE),
        ("omicron", 0x03BF),
        ("pi", 0x03C0),
        ("rho", 0x03C1),
        ("sigmaf", 0x03C2),
        ("sigma", 0x03C3),
        ("tau", 0x03C4),
        ("upsilon", 0x03C5),
        ("phi", 0x03C6),
        ("chi", 0x03C7),
        ("psi", 0x03C8),
        ("omega", 0x03C9),
        ("thetasym", 0x03D1),
        ("upsih", 0x03D2),
        ("piv", 0x03D6),
        ("ensp", 0x2002),
        ("emsp", 0x2003),
        ("thinsp", 0x2009),
        ("zwnj", 0x200C),
        ("zwj", 0x200D),
        ("lrm", 0x200E),
        ("rlm", 0x200F),
        ("ndash", 0x2013),
        ("mdash", 0x2014),
        ("lsquo", 0x2018),
        ("rsquo", 0x2019),
        ("sbquo", 0x201A),
        ("ldquo", 0x201C),
        ("rdquo", 0x201D),
        ("bdquo", 0x201E),
        ("dagger", 0x2020),
        ("Dagger", 0x2021),
        ("bull", 0x2022),
        ("hellip", 0x2026),
        ("permil", 0x2030),
        ("prime", 0x2032),
        ("Prime", 0x2033),
        ("lsaquo", 0x2039),
        ("rsaquo", 0x203A),
        ("oline", 0x203E),
        ("frasl", 0x2044),
        ("euro", 0x20AC),
        ("image", 0x2111),
        ("weierp", 0x2118),
        ("real", 0x211C),
        ("trade", 0x2122),
        ("alefsym", 0x2135),
        ("larr", 0x2190),
        ("uarr", 0x2191),
        ("rarr", 0x2192),
        ("darr", 0x2193),
        ("harr", 0x2194),
        ("crarr", 0x21B5),
        ("lArr", 0x21D0),
        ("uArr", 0x21D1),
        ("rArr", 0x21D2),
        ("dArr", 0x21D3),
        ("hArr", 0x21D4),
        ("forall", 0x2200),
        ("part", 0x2202),
        ("exist", 0x2203),
        ("empty", 0x2205),
        ("nabla", 0x2207),
        ("isin", 0x2208),
        ("notin", 0x2209),
        ("ni", 0x220B),
        ("prod", 0x220F),
        ("sum", 0x2211),
        ("minus", 0x2212),
        ("lowast", 0x2217),
        ("radic", 0x221A),
        ("prop", 0x221D),
        ("infin", 0x221E),
        ("ang", 0x2220),
        ("and", 0x2227),
        ("or", 0x2228),
        ("cap", 0x2229),
        ("cup", 0x222A),
        ("int", 0x222B),
        ("there4", 0x2234),
        ("sim", 0x223C),
        ("cong", 0x2245),
        ("asymp", 0x2248),
        ("ne", 0x2260),
        ("equiv", 0x2261),
        ("le", 0x2264),
        ("ge", 0x2265),
        ("sub", 0x2282),
        ("sup", 0x2283),
        ("nsub", 0x2284),
        ("sube", 0x2286),
        ("supe", 0x2287),
        ("oplus", 0x2295),
        ("otimes", 0x2297),
        ("perp", 0x22A5),
        ("sdot", 0x22C5),
        ("lceil", 0x2308),
        ("rceil", 0x2309),
        ("lfloor", 0x230A),
        ("rfloor", 0x230B),
        ("lang", 0x2329),
        ("rang", 0x232A),
        ("loz", 0x25CA),
        ("spades", 0x2660),
        ("clubs", 0x2663),
        ("hearts", 0x2665),
        ("diams", 0x2666),
    ])
});
