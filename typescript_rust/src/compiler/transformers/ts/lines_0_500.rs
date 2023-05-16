use std::{cell::Cell, collections::HashMap, mem, ptr, rc::Rc};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use crate::{
    add_emit_helpers, are_option_gcs_equal, create_unparsed_source_file, gc_cell_ref_mut_unwrapped,
    get_emit_module_kind, get_emit_script_target, get_original_node, get_parse_tree_node,
    get_strict_option_value, has_syntactic_modifier, is_class_declaration,
    is_shorthand_property_assignment, is_source_file, is_statement, map_defined, modifier_to_flag,
    try_visit_each_child, visit_each_child, BaseNodeFactorySynthetic, CompilerOptions, Debug_,
    EmitHelperFactory, EmitHint, EmitResolver, Matches, ModifierFlags, ModuleKind,
    NamedDeclarationInterface, Node, NodeArray, NodeCheckFlags, NodeExt, NodeFactory, NodeId,
    NodeInterface, ScriptTarget, SyntaxKind, TransformFlags, TransformationContext,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface,
    UnderscoreEscapedMap, VisitResult,
};
use std::io;

pub(super) const USE_NEW_TYPE_METADATA_FORMAT: bool = false;

bitflags! {
    #[derive(Default)]
    pub(super) struct TypeScriptSubstitutionFlags: u32 {
        const None = 0;
        const ClassAliases = 1 << 0;
        const NamespaceExports = 1 << 1;
        const NonQualifiedEnumMembers = 1 << 2;
    }
}

bitflags! {
    pub(super) struct ClassFacts: u32 {
        const None = 0;
        const HasStaticInitializedProperties = 1 << 0;
        const HasConstructorDecorators = 1 << 1;
        const HasMemberDecorators = 1 << 2;
        const IsExportOfNamespace = 1 << 3;
        const IsNamedExternalExport = 1 << 4;
        const IsDefaultExternalExport = 1 << 5;
        const IsDerivedClass = 1 << 6;
        const UseImmediatelyInvokedFunctionExpression = 1 << 7;

        const HasAnyDecorators = Self::HasConstructorDecorators.bits | Self::HasMemberDecorators.bits;
        const NeedsName = Self::HasStaticInitializedProperties.bits | Self::HasMemberDecorators.bits;
        const MayNeedImmediatelyInvokedFunctionExpression = Self::HasAnyDecorators.bits | Self::HasStaticInitializedProperties.bits;
        const IsExported = Self::IsExportOfNamespace.bits | Self::IsDefaultExternalExport.bits | Self::IsNamedExternalExport.bits;
    }
}

#[derive(Trace, Finalize)]
pub(super) struct TransformTypeScript {
    pub(super) _transformer_wrapper: GcCell<Option<Transformer>>,
    pub(super) context: Gc<Box<dyn TransformationContext>>,
    pub(super) factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    pub(super) base_factory: Gc<BaseNodeFactorySynthetic>,
    pub(super) resolver: Gc<Box<dyn EmitResolver>>,
    pub(super) compiler_options: Gc<CompilerOptions>,
    pub(super) strict_null_checks: bool,
    #[unsafe_ignore_trace]
    pub(super) language_version: ScriptTarget,
    #[unsafe_ignore_trace]
    pub(super) module_kind: ModuleKind,
    pub(super) current_source_file: GcCell<Option<Gc<Node /*SourceFile*/>>>,
    pub(super) current_namespace: GcCell<Option<Gc<Node /*ModuleDeclaration*/>>>,
    pub(super) current_namespace_container_name: GcCell<Option<Gc<Node /*Identifier*/>>>,
    pub(super) current_lexical_scope:
        GcCell<Option<Gc<Node /*SourceFile | Block | ModuleBlock | CaseBlock*/>>>,
    pub(super) current_name_scope: GcCell<Option<Gc<Node /*ClassDeclaration*/>>>,
    pub(super) current_scope_first_declarations_of_name:
        GcCell<Option<UnderscoreEscapedMap<Gc<Node>>>>,
    #[unsafe_ignore_trace]
    pub(super) current_class_has_parameter_properties: Cell<Option<bool>>,
    #[unsafe_ignore_trace]
    pub(super) enabled_substitutions: Cell<TypeScriptSubstitutionFlags>,
    pub(super) class_aliases: GcCell<Option<HashMap<NodeId, Gc<Node /*Identifier*/>>>>,
    #[unsafe_ignore_trace]
    pub(super) applicable_substitutions: Cell<TypeScriptSubstitutionFlags>,
}

impl TransformTypeScript {
    pub(super) fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let compiler_options = context.get_compiler_options();
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: Default::default(),
            factory: context.factory(),
            base_factory: context.base_factory(),
            resolver: context.get_emit_resolver(),
            strict_null_checks: get_strict_option_value(&compiler_options, "strictNullChecks"),
            language_version: get_emit_script_target(&compiler_options),
            module_kind: get_emit_module_kind(&compiler_options),
            compiler_options,
            current_source_file: Default::default(),
            context: context.clone(),
            current_namespace: Default::default(),
            current_namespace_container_name: Default::default(),
            current_lexical_scope: Default::default(),
            current_name_scope: Default::default(),
            current_scope_first_declarations_of_name: Default::default(),
            current_class_has_parameter_properties: Default::default(),
            enabled_substitutions: Default::default(),
            class_aliases: Default::default(),
            applicable_substitutions: Default::default(),
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        context.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformTypeScriptOnEmitNodeOverrider::new(
                downcasted.clone(),
                previous_on_emit_node,
            )))
        });
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(TransformTypeScriptOnSubstituteNodeOverrider::new(
                downcasted.clone(),
                previous_on_substitute_node,
            )))
        });

        context.enable_substitution(SyntaxKind::PropertyAccessExpression);
        context.enable_substitution(SyntaxKind::ElementAccessExpression);

        downcasted
    }

    pub(super) fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    pub(super) fn maybe_current_source_file(&self) -> Option<Gc<Node>> {
        self.current_source_file.borrow().clone()
    }

    pub(super) fn current_source_file(&self) -> Gc<Node> {
        self.current_source_file.borrow().clone().unwrap()
    }

    pub(super) fn set_current_source_file(&self, current_source_file: Option<Gc<Node>>) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    pub(super) fn maybe_current_namespace(&self) -> Option<Gc<Node>> {
        self.current_namespace.borrow().clone()
    }

    pub(super) fn set_current_namespace(&self, current_namespace: Option<Gc<Node>>) {
        *self.current_namespace.borrow_mut() = current_namespace;
    }

    pub(super) fn maybe_current_namespace_container_name(&self) -> Option<Gc<Node>> {
        self.current_namespace_container_name.borrow().clone()
    }

    pub(super) fn current_namespace_container_name(&self) -> Gc<Node> {
        self.current_namespace_container_name
            .borrow()
            .clone()
            .unwrap()
    }

    pub(super) fn set_current_namespace_container_name(
        &self,
        current_namespace_container_name: Option<Gc<Node>>,
    ) {
        *self.current_namespace_container_name.borrow_mut() = current_namespace_container_name;
    }

    pub(super) fn current_lexical_scope(&self) -> Gc<Node> {
        self.current_lexical_scope.borrow().clone().unwrap()
    }

    pub(super) fn maybe_current_lexical_scope(&self) -> Option<Gc<Node>> {
        self.current_lexical_scope.borrow().clone()
    }

    pub(super) fn set_current_lexical_scope(&self, current_lexical_scope: Option<Gc<Node>>) {
        *self.current_lexical_scope.borrow_mut() = current_lexical_scope;
    }

    pub(super) fn maybe_current_name_scope(&self) -> Option<Gc<Node>> {
        self.current_name_scope.borrow().clone()
    }

    pub(super) fn set_current_name_scope(&self, current_name_scope: Option<Gc<Node>>) {
        *self.current_name_scope.borrow_mut() = current_name_scope;
    }

    pub(super) fn maybe_current_scope_first_declarations_of_name(
        &self,
    ) -> GcCellRef<Option<UnderscoreEscapedMap<Gc<Node>>>> {
        self.current_scope_first_declarations_of_name.borrow()
    }

    pub(super) fn maybe_current_scope_first_declarations_of_name_mut(
        &self,
    ) -> GcCellRefMut<Option<UnderscoreEscapedMap<Gc<Node>>>> {
        self.current_scope_first_declarations_of_name.borrow_mut()
    }

    pub(super) fn set_current_scope_first_declarations_of_name(
        &self,
        current_scope_first_declarations_of_name: Option<UnderscoreEscapedMap<Gc<Node>>>,
    ) {
        *self.current_scope_first_declarations_of_name.borrow_mut() =
            current_scope_first_declarations_of_name;
    }

    pub(super) fn maybe_current_class_has_parameter_properties(&self) -> Option<bool> {
        self.current_class_has_parameter_properties.get()
    }

    pub(super) fn set_current_class_has_parameter_properties(
        &self,
        current_class_has_parameter_properties: Option<bool>,
    ) {
        self.current_class_has_parameter_properties
            .set(current_class_has_parameter_properties);
    }

    pub(super) fn enabled_substitutions(&self) -> TypeScriptSubstitutionFlags {
        self.enabled_substitutions.get()
    }

    pub(super) fn set_enabled_substitutions(
        &self,
        enabled_substitutions: TypeScriptSubstitutionFlags,
    ) {
        self.enabled_substitutions.set(enabled_substitutions);
    }

    pub(super) fn maybe_class_aliases(&self) -> GcCellRef<Option<HashMap<NodeId, Gc<Node>>>> {
        self.class_aliases.borrow()
    }

    pub(super) fn class_aliases_mut(
        &self,
    ) -> GcCellRefMut<Option<HashMap<NodeId, Gc<Node>>>, HashMap<NodeId, Gc<Node>>> {
        gc_cell_ref_mut_unwrapped(&self.class_aliases)
    }

    pub(super) fn set_class_aliases(&self, class_aliases: Option<HashMap<NodeId, Gc<Node>>>) {
        *self.class_aliases.borrow_mut() = class_aliases;
    }

    pub(super) fn applicable_substitutions(&self) -> TypeScriptSubstitutionFlags {
        self.applicable_substitutions.get()
    }

    pub(super) fn set_applicable_substitutions(
        &self,
        applicable_substitutions: TypeScriptSubstitutionFlags,
    ) {
        self.applicable_substitutions.set(applicable_substitutions);
    }

    pub(super) fn emit_helpers(&self) -> Rc<EmitHelperFactory> {
        self.context.get_emit_helper_factory()
    }

    pub(super) fn transform_source_file_or_bundle(
        &self,
        node: &Node, /*SourceFile | Bundle*/
    ) -> io::Result<Gc<Node>> {
        if node.kind() == SyntaxKind::Bundle {
            return self.transform_bundle(node);
        }
        self.transform_source_file(node)
    }

    pub(super) fn transform_bundle(&self, node: &Node /*Bundle*/) -> io::Result<Gc<Node>> {
        let node_as_bundle = node.as_bundle();
        Ok(self
            .factory
            .create_bundle(
                node_as_bundle
                    .source_files
                    .iter()
                    .map(|source_file| -> io::Result<_> {
                        Ok(Some(
                            self.transform_source_file(source_file.as_ref().unwrap())?,
                        ))
                    })
                    .collect::<Result<Vec<_>, _>>()?,
                Some(map_defined(
                    Some(&node_as_bundle.prepends),
                    |prepend: &Gc<Node>, _| {
                        if prepend.kind() == SyntaxKind::InputFiles {
                            return Some(create_unparsed_source_file(
                                prepend.clone(),
                                Some("js"),
                                Option::<String>::None,
                            ));
                        }
                        Some(prepend.clone())
                    },
                )),
            )
            .wrap())
    }

    pub(super) fn transform_source_file(
        &self,
        node: &Node, /*SourceFile*/
    ) -> io::Result<Gc<Node>> {
        if node.as_source_file().is_declaration_file() {
            return Ok(node.node_wrapper());
        }

        self.set_current_source_file(Some(node.node_wrapper()));

        let visited =
            self.try_save_state_and_invoke(node, |node: &Node| self.visit_source_file(node))?;
        add_emit_helpers(&visited, self.context.read_emit_helpers().as_deref());

        self.set_current_source_file(None);
        Ok(visited)
    }

    #[allow(dead_code)]
    pub(super) fn save_state_and_invoke<TReturn>(
        &self,
        node: &Node,
        mut f: impl FnMut(&Node) -> TReturn,
    ) -> TReturn {
        self.try_save_state_and_invoke(node, |a| Ok(f(a))).unwrap()
    }

    pub(super) fn try_save_state_and_invoke<TReturn>(
        &self,
        node: &Node,
        mut f: impl FnMut(&Node) -> io::Result<TReturn>,
    ) -> io::Result<TReturn> {
        let saved_current_scope = self.maybe_current_lexical_scope();
        let saved_current_name_scope = self.maybe_current_name_scope();
        let saved_current_scope_first_declarations_of_name = self
            .maybe_current_scope_first_declarations_of_name()
            .clone();
        let saved_current_class_has_parameter_properties =
            self.maybe_current_class_has_parameter_properties();

        self.on_before_visit_node(node);

        let visited = f(node)?;

        if !are_option_gcs_equal(
            self.maybe_current_lexical_scope().as_ref(),
            saved_current_scope.as_ref(),
        ) {
            self.set_current_scope_first_declarations_of_name(
                saved_current_scope_first_declarations_of_name,
            );
        }

        self.set_current_lexical_scope(saved_current_scope);
        self.set_current_name_scope(saved_current_name_scope);
        self.set_current_class_has_parameter_properties(
            saved_current_class_has_parameter_properties,
        );
        Ok(visited)
    }

    pub(super) fn on_before_visit_node(&self, node: &Node) {
        match node.kind() {
            SyntaxKind::SourceFile
            | SyntaxKind::CaseBlock
            | SyntaxKind::ModuleBlock
            | SyntaxKind::Block => {
                self.set_current_lexical_scope(Some(node.node_wrapper()));
                self.set_current_name_scope(None);
                self.set_current_scope_first_declarations_of_name(None);
            }
            SyntaxKind::ClassDeclaration | SyntaxKind::FunctionDeclaration => 'case: {
                if has_syntactic_modifier(node, ModifierFlags::Ambient) {
                    break 'case;
                }

                if node.as_named_declaration().maybe_name().is_some() {
                    self.record_emitted_declaration_in_scope(node);
                } else {
                    Debug_.assert(
                        node.kind() == SyntaxKind::ClassDeclaration
                            || has_syntactic_modifier(node, ModifierFlags::Default),
                        None,
                    );
                }
                if is_class_declaration(node) {
                    self.set_current_name_scope(Some(node.node_wrapper()));
                }
            }
            _ => (),
        }
    }

    pub(super) fn visitor(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/ {
        self.try_save_state_and_invoke(node, |node: &Node| self.visitor_worker(node))
    }

    pub(super) fn visitor_worker(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/ {
        if node
            .transform_flags()
            .intersects(TransformFlags::ContainsTypeScript)
        {
            return self.visit_type_script(node);
        }
        Ok(Some(node.node_wrapper().into()))
    }

    pub(super) fn source_element_visitor(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/ {
        self.try_save_state_and_invoke(node, |node: &Node| self.source_element_visitor_worker(node))
    }

    pub(super) fn source_element_visitor_worker(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/
    {
        Ok(match node.kind() {
            SyntaxKind::ImportDeclaration
            | SyntaxKind::ImportEqualsDeclaration
            | SyntaxKind::ExportAssignment
            | SyntaxKind::ExportDeclaration => self.visit_elidable_statement(node)?,
            _ => self.visitor_worker(node)?,
        })
    }

    pub(super) fn visit_elidable_statement(
        &self,
        node: &Node, /*ImportDeclaration | ImportEqualsDeclaration | ExportAssignment | ExportDeclaration*/
    ) -> io::Result<VisitResult> /*<Node>*/ {
        let parsed = get_parse_tree_node(Some(node), Option::<fn(&Node) -> bool>::None);
        if !parsed.matches(|parsed| ptr::eq(&*parsed, node)) {
            if node
                .transform_flags()
                .intersects(TransformFlags::ContainsTypeScript)
            {
                return Ok(try_visit_each_child(
                    Some(node),
                    |node: &Node| self.visitor(node),
                    &**self.context,
                )?
                .map(Into::into));
            }
            return Ok(Some(node.node_wrapper().into()));
        }
        Ok(match node.kind() {
            SyntaxKind::ImportDeclaration => self.visit_import_declaration(node),
            SyntaxKind::ImportEqualsDeclaration => self.visit_import_equals_declaration(node)?,
            SyntaxKind::ExportAssignment => self.visit_export_assignment(node)?,
            SyntaxKind::ExportDeclaration => self.visit_export_declaration(node)?,
            _ => Debug_.fail(Some("Unhandled ellided statement")),
        })
    }

    pub(super) fn namespace_element_visitor(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/
    {
        self.try_save_state_and_invoke(node, |node: &Node| {
            self.namespace_element_visitor_worker(node)
        })
    }

    pub(super) fn namespace_element_visitor_worker(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/
    {
        if matches!(
            node.kind(),
            SyntaxKind::ExportDeclaration
                | SyntaxKind::ImportDeclaration
                | SyntaxKind::ImportClause
        ) || node.kind() == SyntaxKind::ImportEqualsDeclaration
            && node.as_import_equals_declaration().module_reference.kind()
                == SyntaxKind::ExternalModuleReference
        {
            return Ok(None);
        } else if node
            .transform_flags()
            .intersects(TransformFlags::ContainsTypeScript)
            || has_syntactic_modifier(node, ModifierFlags::Export)
        {
            return self.visit_type_script(node);
        }

        Ok(Some(node.node_wrapper().into()))
    }

    pub(super) fn class_element_visitor(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/ {
        self.try_save_state_and_invoke(node, |node: &Node| self.class_element_visitor_worker(node))
    }

    pub(super) fn class_element_visitor_worker(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/
    {
        Ok(match node.kind() {
            SyntaxKind::Constructor => self.visit_constructor(node)?,
            SyntaxKind::PropertyDeclaration => self.visit_property_declaration(node)?,
            SyntaxKind::IndexSignature
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::ClassStaticBlockDeclaration => self.visitor_worker(node)?,
            SyntaxKind::SemicolonClassElement => Some(node.node_wrapper().into()),
            _ => Debug_.fail_bad_syntax_kind(node, None),
        })
    }

    pub(super) fn modifier_visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        if modifier_to_flag(node.kind()).intersects(ModifierFlags::TypeScriptModifier) {
            return None;
        } else if self.maybe_current_namespace().is_some()
            && node.kind() == SyntaxKind::ExportKeyword
        {
            return None;
        }

        Some(node.node_wrapper().into())
    }

    pub(super) fn visit_type_script(&self, node: &Node) -> io::Result<VisitResult> /*<Node>*/ {
        if is_statement(node) && has_syntactic_modifier(node, ModifierFlags::Ambient) {
            return Ok(Some(
                self.factory
                    .create_not_emitted_statement(node.node_wrapper())
                    .into(),
            ));
        }

        Ok(match node.kind() {
            SyntaxKind::ExportKeyword | SyntaxKind::DefaultKeyword => {
                if self.maybe_current_namespace().is_some() {
                    None
                } else {
                    Some(node.node_wrapper().into())
                }
            }
            SyntaxKind::PublicKeyword
            | SyntaxKind::PrivateKeyword
            | SyntaxKind::ProtectedKeyword
            | SyntaxKind::AbstractKeyword
            | SyntaxKind::OverrideKeyword
            | SyntaxKind::ConstKeyword
            | SyntaxKind::DeclareKeyword
            | SyntaxKind::ReadonlyKeyword
            | SyntaxKind::ArrayType
            | SyntaxKind::TupleType
            | SyntaxKind::OptionalType
            | SyntaxKind::RestType
            | SyntaxKind::TypeLiteral
            | SyntaxKind::TypePredicate
            | SyntaxKind::TypeParameter
            | SyntaxKind::AnyKeyword
            | SyntaxKind::UnknownKeyword
            | SyntaxKind::BooleanKeyword
            | SyntaxKind::StringKeyword
            | SyntaxKind::NumberKeyword
            | SyntaxKind::NeverKeyword
            | SyntaxKind::VoidKeyword
            | SyntaxKind::SymbolKeyword
            | SyntaxKind::ConstructorType
            | SyntaxKind::FunctionType
            | SyntaxKind::TypeQuery
            | SyntaxKind::TypeReference
            | SyntaxKind::UnionType
            | SyntaxKind::IntersectionType
            | SyntaxKind::ConditionalType
            | SyntaxKind::ParenthesizedType
            | SyntaxKind::ThisType
            | SyntaxKind::TypeOperator
            | SyntaxKind::IndexedAccessType
            | SyntaxKind::MappedType
            | SyntaxKind::LiteralType
            | SyntaxKind::IndexSignature
            | SyntaxKind::Decorator => None,
            SyntaxKind::TypeAliasDeclaration => Some(
                self.factory
                    .create_not_emitted_statement(node.node_wrapper())
                    .into(),
            ),
            SyntaxKind::PropertyDeclaration => self.visit_property_declaration(node)?,
            SyntaxKind::NamespaceExportDeclaration => None,
            SyntaxKind::Constructor => self.visit_constructor(node)?,
            SyntaxKind::InterfaceDeclaration => Some(
                self.factory
                    .create_not_emitted_statement(node.node_wrapper())
                    .into(),
            ),
            SyntaxKind::ClassDeclaration => self.visit_class_declaration(node)?,
            SyntaxKind::ClassExpression => Some(self.visit_class_expression(node)?.into()),
            SyntaxKind::HeritageClause => self.visit_heritage_clause(node)?.map(Into::into),
            SyntaxKind::ExpressionWithTypeArguments => {
                Some(self.visit_expression_with_type_arguments(node)?.into())
            }
            SyntaxKind::MethodDeclaration => self.visit_method_declaration(node)?,
            SyntaxKind::GetAccessor => self.visit_get_accessor(node)?,
            SyntaxKind::SetAccessor => self.visit_set_accessor(node)?,
            SyntaxKind::FunctionDeclaration => self.visit_function_declaration(node)?,
            SyntaxKind::FunctionExpression => Some(self.visit_function_expression(node)?.into()),
            SyntaxKind::ArrowFunction => self.visit_arrow_function(node)?,
            SyntaxKind::Parameter => self.visit_parameter(node)?,
            SyntaxKind::ParenthesizedExpression => {
                Some(self.visit_parenthesized_expression(node)?.into())
            }
            SyntaxKind::TypeAssertionExpression | SyntaxKind::AsExpression => {
                Some(self.visit_assertion_expression(node)?.into())
            }
            SyntaxKind::CallExpression => self.visit_call_expression(node)?,
            SyntaxKind::NewExpression => self.visit_new_expression(node)?,
            SyntaxKind::TaggedTemplateExpression => self.visit_tagged_template_expression(node)?,
            SyntaxKind::NonNullExpression => Some(self.visit_non_null_expression(node)?.into()),
            SyntaxKind::EnumDeclaration => self.visit_enum_declaration(node)?,
            SyntaxKind::VariableStatement => self.visit_variable_statement(node)?.map(Into::into),
            SyntaxKind::VariableDeclaration => self.visit_variable_declaration(node)?,
            SyntaxKind::ModuleDeclaration => self.visit_module_declaration(node)?,
            SyntaxKind::ImportEqualsDeclaration => self.visit_import_equals_declaration(node)?,
            SyntaxKind::JsxSelfClosingElement => self.visit_jsx_self_closing_element(node)?,
            SyntaxKind::JsxOpeningElement => self.visit_jsx_jsx_opening_element(node)?,
            _ => try_visit_each_child(
                Some(node),
                |node: &Node| self.visitor(node),
                &**self.context,
            )?
            .map(Into::into),
        })
    }
}

impl TransformerInterface for TransformTypeScript {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        self.transform_source_file_or_bundle(node)
    }
}

#[derive(Trace, Finalize)]
struct TransformTypeScriptOnEmitNodeOverrider {
    transform_type_script: Gc<Box<TransformTypeScript>>,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformTypeScriptOnEmitNodeOverrider {
    fn new(
        transform_type_script: Gc<Box<TransformTypeScript>>,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_type_script,
            previous_on_emit_node,
        }
    }

    pub(super) fn is_transformed_module_declaration(&self, node: &Node) -> bool {
        get_original_node(Some(node), Option::<fn(Option<Gc<Node>>) -> bool>::None)
            .unwrap()
            .kind()
            == SyntaxKind::ModuleDeclaration
    }

    pub(super) fn is_transformed_enum_declaration(&self, node: &Node) -> bool {
        get_original_node(Some(node), Option::<fn(Option<Gc<Node>>) -> bool>::None)
            .unwrap()
            .kind()
            == SyntaxKind::EnumDeclaration
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformTypeScriptOnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()> {
        let saved_applicable_substitutions = self.transform_type_script.applicable_substitutions();
        let saved_current_source_file = self.transform_type_script.maybe_current_source_file();

        if is_source_file(node) {
            self.transform_type_script
                .set_current_source_file(Some(node.node_wrapper()));
        }

        if self
            .transform_type_script
            .enabled_substitutions()
            .intersects(TypeScriptSubstitutionFlags::NamespaceExports)
            && self.is_transformed_module_declaration(node)
        {
            self.transform_type_script.set_applicable_substitutions(
                self.transform_type_script.applicable_substitutions()
                    | TypeScriptSubstitutionFlags::NamespaceExports,
            );
        }

        if self
            .transform_type_script
            .enabled_substitutions()
            .intersects(TypeScriptSubstitutionFlags::NonQualifiedEnumMembers)
            && self.is_transformed_enum_declaration(node)
        {
            self.transform_type_script.set_applicable_substitutions(
                self.transform_type_script.applicable_substitutions()
                    | TypeScriptSubstitutionFlags::NonQualifiedEnumMembers,
            );
        }

        self.previous_on_emit_node
            .on_emit_node(hint, node, emit_callback)?;

        self.transform_type_script
            .set_applicable_substitutions(saved_applicable_substitutions);
        self.transform_type_script
            .set_current_source_file(saved_current_source_file);

        Ok(())
    }
}

#[derive(Trace, Finalize)]
struct TransformTypeScriptOnSubstituteNodeOverrider {
    transform_type_script: Gc<Box<TransformTypeScript>>,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformTypeScriptOnSubstituteNodeOverrider {
    fn new(
        transform_type_script: Gc<Box<TransformTypeScript>>,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_type_script,
            previous_on_substitute_node,
        }
    }

    fn substitute_shorthand_property_assignment(
        &self,
        node: &Node, /*ShorthandPropertyAssignment*/
    ) -> Gc<Node /*ObjectLiteralElementLike*/> {
        let node_as_shorthand_property_assignment = node.as_shorthand_property_assignment();
        if self
            .transform_type_script
            .enabled_substitutions()
            .intersects(TypeScriptSubstitutionFlags::NamespaceExports)
        {
            let name = node_as_shorthand_property_assignment.name();
            let exported_name = self.try_substitute_namespace_exported_name(&name);
            if let Some(exported_name) = exported_name {
                if let Some(node_object_assignment_initializer) =
                    node_as_shorthand_property_assignment
                        .object_assignment_initializer
                        .as_ref()
                {
                    let initializer = self
                        .transform_type_script
                        .factory
                        .create_assignment(
                            exported_name,
                            node_object_assignment_initializer.clone(),
                        )
                        .wrap();
                    return self
                        .transform_type_script
                        .factory
                        .create_property_assignment(name, initializer)
                        .wrap()
                        .set_text_range(Some(node));
                }
                return self
                    .transform_type_script
                    .factory
                    .create_property_assignment(name, exported_name)
                    .wrap()
                    .set_text_range(Some(node));
            }
        }
        node.node_wrapper()
    }

    fn substitute_expression(&self, node: &Node /*Expression*/) -> io::Result<Gc<Node>> {
        match node.kind() {
            SyntaxKind::Identifier => {
                return self.substitute_expression_identifier(node);
            }
            SyntaxKind::PropertyAccessExpression => {
                return Ok(self.substitute_property_access_expression(node));
            }
            SyntaxKind::ElementAccessExpression => {
                return Ok(self.substitute_element_access_expression(node));
            }
            _ => (),
        }

        Ok(node.node_wrapper())
    }

    fn substitute_expression_identifier(
        &self,
        node: &Node, /*Identifier*/
    ) -> io::Result<Gc<Node /*Expression*/>> {
        Ok(self
            .try_substitute_class_alias(node)?
            .or_else(|| self.try_substitute_namespace_exported_name(node))
            .unwrap_or_else(|| node.node_wrapper()))
    }

    fn try_substitute_class_alias(
        &self,
        node: &Node, /*Identifier*/
    ) -> io::Result<Option<Gc<Node /*Expression*/>>> {
        if self
            .transform_type_script
            .enabled_substitutions()
            .intersects(TypeScriptSubstitutionFlags::ClassAliases)
        {
            if self
                .transform_type_script
                .resolver
                .get_node_check_flags(node)
                .intersects(NodeCheckFlags::ConstructorReferenceInClass)
            {
                let declaration = self
                    .transform_type_script
                    .resolver
                    .get_referenced_value_declaration(node)?;
            }
        }

        Ok(None)
    }

    fn try_substitute_namespace_exported_name(
        &self,
        _node: &Node, /*Identifier*/
    ) -> Option<Gc<Node /*Expression*/>> {
        unimplemented!()
    }

    fn substitute_property_access_expression(
        &self,
        _node: &Node, /*PropertyAccessExpression*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    fn substitute_element_access_expression(
        &self,
        _node: &Node, /*ElementAccessExpression*/
    ) -> Gc<Node> {
        unimplemented!()
    }
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformTypeScriptOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, hint: EmitHint, node: &Node) -> io::Result<Gc<Node>> {
        let node = self
            .previous_on_substitute_node
            .on_substitute_node(hint, node)?;
        if hint == EmitHint::Expression {
            return self.substitute_expression(&node);
        } else if is_shorthand_property_assignment(&node) {
            return Ok(self.substitute_shorthand_property_assignment(&node));
        }

        Ok(node)
    }
}

#[derive(Trace, Finalize)]
struct TransformTypeScriptFactory {}

impl TransformTypeScriptFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformTypeScriptFactory {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        TransformTypeScript::new(context).as_transformer()
    }
}

pub fn transform_type_script() -> TransformerFactory {
    Gc::new(Box::new(TransformTypeScriptFactory::new()))
}
