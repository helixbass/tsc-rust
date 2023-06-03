use std::{borrow::Borrow, collections::HashMap, io, rc::Rc};

use derive_builder::Builder;
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use itertools::Itertools;

use crate::{
    chain_bundle, gc_cell_ref_mut_unwrapped, gc_cell_ref_unwrapped, BaseNodeFactorySynthetic,
    CompilerOptions, EmitHelperFactory, GeneratedIdentifierFlags, JsxEmit,
    NamedDeclarationInterface, Node, NodeFactory, NodeInterface, TransformationContext,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface, _d,
    add_emit_helpers, filter, get_jsx_implicit_import_base, get_jsx_runtime_import,
    get_semantic_jsx_children, insert_statement_after_custom_prologue, is_external_module,
    is_external_or_common_js_module, is_identifier, is_jsx_attribute, is_jsx_spread_attribute,
    map_defined, visit_each_child, Debug_, HasStatementsInterface, MapOrDefault, Matches,
    NodeArray, NodeArrayOrVec, NodeExt, NodeFlags, NonEmpty, ReadonlyTextRange, SyntaxKind,
    TransformFlags, VisitResult,
};

#[derive(Builder, Default, Trace, Finalize)]
#[builder(setter(into))]
pub(super) struct PerFileState {
    #[builder(default)]
    pub import_specifier: Option<String>,
    #[builder(default)]
    pub filename_declaration: Option<Gc<Node /*VariableDeclaration & { name: Identifier; }*/>>,
    #[builder(default)]
    pub utilized_implicit_runtime_imports:
        Option<HashMap<String, HashMap<String, Gc<Node /*ImportSpecifier*/>>>>,
}

#[derive(Trace, Finalize)]
pub(super) struct TransformJsx {
    context: Gc<Box<dyn TransformationContext>>,
    compiler_options: Gc<CompilerOptions>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    current_source_file: GcCell<Option<Gc<Node /*SourceFile*/>>>,
    current_file_state: GcCell<Option<PerFileState>>,
}

impl TransformJsx {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self {
            factory: context.factory(),
            compiler_options: context.get_compiler_options(),
            context,
            current_source_file: _d(),
            current_file_state: _d(),
        }
    }

    pub(super) fn maybe_current_source_file(&self) -> GcCellRef<Option<Gc<Node /*SourceFile*/>>> {
        self.current_source_file.borrow()
    }

    pub(super) fn current_source_file(&self) -> GcCellRef<Gc<Node /*SourceFile*/>> {
        gc_cell_ref_unwrapped(&self.current_source_file)
    }

    pub(super) fn maybe_current_source_file_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<Node /*SourceFile*/>>> {
        self.current_source_file.borrow_mut()
    }

    pub(super) fn current_source_file_mut(
        &self,
    ) -> GcCellRefMut<Option<Gc<Node /*SourceFile*/>>, Gc<Node /*SourceFile*/>> {
        gc_cell_ref_mut_unwrapped(&self.current_source_file)
    }

    pub(super) fn set_current_source_file(
        &self,
        current_source_file: Option<Gc<Node /*SourceFile*/>>,
    ) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    pub(super) fn maybe_current_file_state(&self) -> GcCellRef<Option<PerFileState>> {
        self.current_file_state.borrow()
    }

    pub(super) fn current_file_state(&self) -> GcCellRef<PerFileState> {
        gc_cell_ref_unwrapped(&self.current_file_state)
    }

    pub(super) fn maybe_current_file_state_mut(&self) -> GcCellRefMut<Option<PerFileState>> {
        self.current_file_state.borrow_mut()
    }

    pub(super) fn current_file_state_mut(
        &self,
    ) -> GcCellRefMut<Option<PerFileState>, PerFileState> {
        gc_cell_ref_mut_unwrapped(&self.current_file_state)
    }

    pub(super) fn set_current_file_state(&self, current_file_state: Option<PerFileState>) {
        *self.current_file_state.borrow_mut() = current_file_state;
    }

    fn emit_helpers(&self) -> Rc<EmitHelperFactory> {
        self.context.get_emit_helper_factory()
    }

    fn get_current_file_name_expression(&self) -> Gc<Node /*Identifier*/> {
        if let Some(current_file_state_filename_declaration) =
            self.current_file_state().filename_declaration.clone()
        {
            return current_file_state_filename_declaration
                .as_variable_declaration()
                .name();
        }
        let declaration = self
            .factory
            .create_variable_declaration(
                Some(self.factory.create_unique_name(
                    "_jsxFileName",
                    Some(
                        GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel,
                    ),
                )),
                None,
                None,
                Some(
                    self.factory
                        .create_string_literal(
                            self.current_source_file()
                                .as_source_file()
                                .file_name()
                                .clone(),
                            None,
                            None,
                        )
                        .wrap(),
                ),
            )
            .wrap();
        self.current_file_state_mut().filename_declaration = Some(declaration.clone());
        declaration.as_variable_declaration().name()
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

    fn get_jsx_factory_callee(&self, is_static_children: bool) -> Gc<Node> {
        let type_ = self.get_jsx_factory_callee_primitive(is_static_children);
        self.get_implicit_import_for_name(type_)
    }

    fn get_implicit_jsx_fragment_reference(&self) -> Gc<Node> {
        self.get_implicit_import_for_name("Fragment")
    }

    fn get_implicit_import_for_name(&self, name: &str) -> Gc<Node> {
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
            return existing.as_import_specifier().name.clone();
        }
        let mut current_file_state = self.current_file_state_mut();
        let specifier_source_imports = current_file_state
            .utilized_implicit_runtime_imports
            .get_or_insert_with(|| _d())
            .entry(import_source)
            .or_insert_with(|| _d());
        let generated_name = self.factory.create_unique_name(
            &format!("_{name}"),
            Some(
                GeneratedIdentifierFlags::Optimistic
                    | GeneratedIdentifierFlags::FileLevel
                    | GeneratedIdentifierFlags::AllowNameSubstitution,
            ),
        );
        let specifier = self
            .factory
            .create_import_specifier(
                false,
                Some(
                    self.factory
                        .create_identifier(name, Option::<Gc<NodeArray>>::None, None)
                        .wrap(),
                ),
                generated_name.clone(),
            )
            .wrap();
        generated_name
            .as_identifier()
            .set_generated_import_reference(Some(specifier.clone()));
        specifier_source_imports.insert(name.to_owned(), specifier);
        generated_name
    }

    fn transform_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        let node_as_source_file = node.as_source_file();
        if node_as_source_file.is_declaration_file() {
            return node.node_wrapper();
        }

        self.set_current_source_file(Some(node.node_wrapper()));
        self.set_current_file_state(Some(
            PerFileStateBuilder::default()
                .import_specifier(get_jsx_implicit_import_base(
                    &self.compiler_options,
                    Some(node),
                ))
                .build()
                .unwrap(),
        ));
        let mut visited = visit_each_child(node, |node: &Node| self.visitor(node), &**self.context);
        add_emit_helpers(&visited, self.context.read_emit_helpers().as_deref());
        let mut statements: NodeArrayOrVec = visited.as_source_file().statements().into();
        if let Some(current_file_state_filename_declaration) =
            self.current_file_state().filename_declaration.clone()
        {
            let mut statements_as_vec: Vec<Gc<Node>> = (&statements).into();
            insert_statement_after_custom_prologue(
                &mut statements_as_vec,
                Some(
                    self.factory
                        .create_variable_statement(
                            Option::<Gc<NodeArray>>::None,
                            self.factory
                                .create_variable_declaration_list(
                                    vec![current_file_state_filename_declaration],
                                    Some(NodeFlags::Const),
                                )
                                .wrap(),
                        )
                        .wrap(),
                ),
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
                if is_external_module(node) {
                    let import_statement = self
                        .factory
                        .create_import_declaration(
                            Option::<Gc<NodeArray>>::None,
                            Option::<Gc<NodeArray>>::None,
                            Some(
                                self.factory
                                    .create_import_clause(
                                        false,
                                        None,
                                        Some(
                                            self.factory
                                                .create_named_imports(
                                                    import_specifiers_map
                                                        .values()
                                                        .cloned()
                                                        .collect_vec(),
                                                )
                                                .wrap(),
                                        ),
                                    )
                                    .wrap(),
                            ),
                            self.factory
                                .create_string_literal(import_source.clone(), None, None)
                                .wrap(),
                            None,
                        )
                        .wrap()
                        .set_parent_recursive(false);
                    let mut statements_as_vec: Vec<Gc<Node>> = (&statements).into();
                    insert_statement_after_custom_prologue(
                        &mut statements_as_vec,
                        Some(import_statement),
                    );
                    statements = statements_as_vec.into();
                } else if is_external_or_common_js_module(node) {
                    let require_statement = self
                        .factory
                        .create_variable_statement(
                            Option::<Gc<NodeArray>>::None,
                            self.factory
                                .create_variable_declaration_list(
                                    vec![self
                                        .factory
                                        .create_variable_declaration(
                                            Some(
                                                self.factory
                                                    .create_object_binding_pattern(
                                                        import_specifiers_map
                                                            .values()
                                                            .map(|s| {
                                                                let s_as_import_specifier =
                                                                    s.as_import_specifier();
                                                                self.factory
                                                                    .create_binding_element(
                                                                        None,
                                                                        s_as_import_specifier
                                                                            .property_name
                                                                            .clone(),
                                                                        s_as_import_specifier
                                                                            .name
                                                                            .clone(),
                                                                        None,
                                                                    )
                                                                    .wrap()
                                                            })
                                                            .collect_vec(),
                                                    )
                                                    .wrap(),
                                            ),
                                            None,
                                            None,
                                            Some(
                                                self.factory
                                                    .create_call_expression(
                                                        self.factory
                                                            .create_identifier(
                                                                "require",
                                                                Option::<Gc<NodeArray>>::None,
                                                                None,
                                                            )
                                                            .wrap(),
                                                        Option::<Gc<NodeArray>>::None,
                                                        Some(vec![self
                                                            .factory
                                                            .create_string_literal(
                                                                import_source.clone(),
                                                                None,
                                                                None,
                                                            )
                                                            .wrap()]),
                                                    )
                                                    .wrap(),
                                            ),
                                        )
                                        .wrap()],
                                    Some(NodeFlags::Const),
                                )
                                .wrap(),
                        )
                        .wrap()
                        .set_parent_recursive(false);
                    let mut statements_as_vec: Vec<Gc<Node>> = (&statements).into();
                    insert_statement_after_custom_prologue(
                        &mut statements_as_vec,
                        Some(require_statement),
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
                &visited.as_source_file().statements()
            )
        ) {
            visited = self
                .factory
                .update_source_file(&visited, statements, None, None, None, None, None);
        }
        self.set_current_file_state(None);
        visited
    }

    fn visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        if node
            .transform_flags()
            .intersects(TransformFlags::ContainsJsx)
        {
            self.visitor_worker(node)
        } else {
            Some(node.node_wrapper().into())
        }
    }

    fn visitor_worker(&self, node: &Node) -> VisitResult /*<Node>*/ {
        match node.kind() {
            SyntaxKind::JsxElement => self.visit_jsx_element(node, false).map(Into::into),
            SyntaxKind::JsxSelfClosingElement => self
                .visit_jsx_self_closing_element(node, false)
                .map(Into::into),
            SyntaxKind::JsxFragment => self.visit_jsx_fragment(node, false).map(Into::into),
            SyntaxKind::JsxExpression => self.visit_jsx_expression(node).map(Into::into),
            _ => Some(
                visit_each_child(node, |node: &Node| self.visitor(node), &**self.context).into(),
            ),
        }
    }

    fn transform_jsx_child_to_expression(
        &self,
        node: &Node, /*JsxChild*/
    ) -> Option<Gc<Node /*Expression*/>> {
        match node.kind() {
            SyntaxKind::JsxText => self.visit_jsx_text(node),
            SyntaxKind::JsxExpression => self.visit_jsx_expression(node),
            SyntaxKind::JsxElement => self.visit_jsx_element(node, true),
            SyntaxKind::JsxSelfClosingElement => self.visit_jsx_self_closing_element(node, true),
            SyntaxKind::JsxFragment => self.visit_jsx_fragment(node, true),
            _ => Debug_.fail_bad_syntax_kind(node, None),
        }
    }

    fn has_key_after_props_spread(&self, node: &Node /*JsxOpeningLikeElement*/) -> bool {
        let node_as_jsx_opening_like_element = node.as_jsx_opening_like_element();
        let mut spread = false;
        for elem in &node_as_jsx_opening_like_element
            .attributes()
            .as_jsx_attributes()
            .properties
        {
            if is_jsx_spread_attribute(elem) {
                spread = true;
            } else if spread
                && is_jsx_attribute(elem)
                && elem.as_jsx_attribute().name.as_identifier().escaped_text == "key"
            {
                return true;
            }
        }
        false
    }

    fn should_use_create_element(&self, node: &Node /*JsxOpeningLikeElement*/) -> bool {
        self.current_file_state().import_specifier.is_none()
            || self.has_key_after_props_spread(node)
    }

    fn visit_jsx_element(
        &self,
        node: &Node, /*JsxElement*/
        is_child: bool,
    ) -> Option<Gc<Node>> {
        let node_as_jsx_element = node.as_jsx_element();
        if self.should_use_create_element(&node_as_jsx_element.opening_element) {
            self.visit_jsx_opening_like_element_create_element(
                &node_as_jsx_element.opening_element,
                Some(&node_as_jsx_element.children),
                is_child,
                node,
            )
        } else {
            self.visit_jsx_opening_like_element_jsx(
                &node_as_jsx_element.opening_element,
                Some(&node_as_jsx_element.children),
                is_child,
                node,
            )
        }
    }

    fn visit_jsx_self_closing_element(
        &self,
        node: &Node, /*JsxSelfClosingElement*/
        is_child: bool,
    ) -> Option<Gc<Node>> {
        if self.should_use_create_element(node) {
            self.visit_jsx_opening_like_element_create_element(node, None, is_child, node)
        } else {
            self.visit_jsx_opening_like_element_jsx(node, None, is_child, node)
        }
    }

    fn visit_jsx_fragment(
        &self,
        node: &Node, /*JsxFragment*/
        is_child: bool,
    ) -> Option<Gc<Node>> {
        let node_as_jsx_fragment = node.as_jsx_fragment();
        if self.current_file_state().import_specifier.is_none() {
            self.visit_jsx_opening_fragment_create_element(
                &node_as_jsx_fragment.opening_fragment,
                &node_as_jsx_fragment.children,
                is_child,
                node,
            )
        } else {
            self.visit_jsx_opening_fragment_jsx(
                &node_as_jsx_fragment.opening_fragment,
                &node_as_jsx_fragment.children,
                is_child,
                node,
            )
        }
    }

    fn convert_jsx_children_to_children_prop_object(
        &self,
        children: &[Gc<Node /*JsxChild*/>],
    ) -> Option<Gc<Node>> {
        let prop = self.convert_jsx_children_to_children_prop_assignment(children);
        prop.map(|prop| {
            self.factory
                .create_object_literal_expression(Some(vec![prop]), None)
                .wrap()
        })
    }

    fn convert_jsx_children_to_children_prop_assignment(
        &self,
        children: &[Gc<Node /*JsxChild*/>],
    ) -> Option<Gc<Node>> {
        let non_whitespace_children = get_semantic_jsx_children(children);
        if non_whitespace_children.len() == 1
            && non_whitespace_children[0]
                .as_jsx_expression()
                .dot_dot_dot_token
                .is_none()
        {
            let result = self.transform_jsx_child_to_expression(&non_whitespace_children[0]);
            return result.map(|result| {
                self.factory
                    .create_property_assignment("children", result)
                    .wrap()
            });
        }
        let result = map_defined(Some(children), |child: &Gc<Node>, _| {
            self.transform_jsx_child_to_expression(child)
        });
        (!result.is_empty()).then(|| {
            self.factory
                .create_property_assignment(
                    "children",
                    self.factory
                        .create_array_literal_expression(Some(result), None)
                        .wrap(),
                )
                .wrap()
        })
    }

    fn visit_jsx_opening_like_element_jsx(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
        children: Option<&[Gc<Node /*JsxChild*/>]>,
        is_child: bool,
        location: &impl ReadonlyTextRange,
    ) -> Option<Gc<Node>> {
        let node_as_jsx_opening_like_element = node.as_jsx_opening_like_element();
        let tag_name = self.get_tag_name(node);
        let children_prop = children
            .non_empty()
            .and_then(|children| self.convert_jsx_children_to_children_prop_assignment(children));
        let key_attr = node_as_jsx_opening_like_element
            .attributes()
            .as_jsx_attributes()
            .properties
            .iter()
            .find(|p| {
                p.as_named_declaration().maybe_name().matches(|ref p_name| {
                    is_identifier(p_name) && p_name.as_identifier().escaped_text == "key"
                })
            })
            .cloned();
        let attrs: NodeArrayOrVec = key_attr.as_ref().map_or_else(
            || {
                node_as_jsx_opening_like_element
                    .attributes()
                    .as_jsx_attributes()
                    .properties
                    .clone()
                    .into()
            },
            |key_attr| {
                filter(
                    &node_as_jsx_opening_like_element
                        .attributes()
                        .as_jsx_attributes()
                        .properties,
                    |p: &Gc<Node>| !Gc::ptr_eq(p, key_attr),
                )
                .into()
            },
        );
        let object_properties = if !attrs.is_empty() {
            self.transform_jsx_attributes_to_object_props(&attrs, children_prop)
        } else {
            self.factory
                .create_object_literal_expression(
                    Some(children_prop.map_or_default(|children_prop| vec![children_prop])),
                    None,
                )
                .wrap()
        };
        Some(self.visit_jsx_opening_like_element_or_fragment_jsx(
            &tag_name,
            &object_properties,
            key_attr,
            children.unwrap_or(&[]),
            is_child,
            location,
        ))
    }

    fn visit_jsx_opening_like_element_or_fragment_jsx(
        &self,
        _tag_name: &Node,          /*Expression*/
        _object_properties: &Node, /*Expression*/
        _key_attr: Option<impl Borrow<Node /*JsxAttribute*/>>,
        _children: &[Gc<Node /*JsxChild*/>],
        _is_child: bool,
        _location: &impl ReadonlyTextRange,
    ) -> Gc<Node> {
        unimplemented!()
    }

    fn visit_jsx_opening_like_element_create_element(
        &self,
        _node: &Node, /*JsxOpeningLikeElement*/
        _children: Option<&[Gc<Node /*JsxChild*/>]>,
        _is_child: bool,
        _location: &impl ReadonlyTextRange,
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    fn visit_jsx_opening_fragment_jsx(
        &self,
        _node: &Node, /*JsxOpeningFragment*/
        _children: &[Gc<Node /*JsxChild*/>],
        _is_child: bool,
        _location: &impl ReadonlyTextRange,
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    fn visit_jsx_opening_fragment_create_element(
        &self,
        _node: &Node, /*JsxOpeningFragment*/
        _children: &[Gc<Node /*JsxChild*/>],
        _is_child: bool,
        _location: &impl ReadonlyTextRange,
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    fn transform_jsx_attributes_to_object_props(
        &self,
        _attrs: &[Gc<Node /*JsxSpreadAttribute | JsxAttribute*/>],
        _children: Option<impl Borrow<Node /*PropertyAssignment*/>>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    fn visit_jsx_text(&self, _node: &Node /*JsxText*/) -> Option<Gc<Node /*StringLiteral*/>> {
        unimplemented!()
    }

    fn get_tag_name(
        &self,
        _node: &Node, /*JsxElement | JsxOpeningLikeElement*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    fn visit_jsx_expression(&self, _node: &Node /*JsxExpression*/) -> Option<Gc<Node>> {
        unimplemented!()
    }
}

impl TransformerInterface for TransformJsx {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        Ok(self.transform_source_file(node))
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
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            Gc::new(Box::new(TransformJsx::new(context))),
        )
    }
}

pub fn transform_jsx() -> TransformerFactory {
    Gc::new(Box::new(TransformJsxFactory::new()))
}
