use std::{
    any::Any,
    cell::{Cell, Ref, RefCell, RefMut},
    collections::HashMap,
    io,
};

use bitflags::bitflags;
use id_arena::Id;

use crate::{
    add_emit_helpers, add_synthetic_trailing_comment, create_unparsed_source_file,
    declaration_name_to_string, downcast_transformer_ref, get_emit_module_kind,
    get_emit_script_target, get_original_node, get_parse_tree_node, get_strict_option_value,
    get_text_of_node, has_syntactic_modifier, impl_has_arena, is_access_expression,
    is_class_declaration, is_element_access_expression, is_generated_identifier, is_local_name,
    is_property_access_expression, is_shorthand_property_assignment, is_source_file, is_statement,
    map_defined, maybe_get_original_node_full, modifier_to_flag, ref_mut_unwrapped, ref_unwrapped,
    released, set_constant_value, try_maybe_visit_each_child, AllArenas, BoolExt, CompilerOptions,
    CoreTransformationContext, Debug_, EmitHelperFactory, EmitHint, EmitResolver, HasArena,
    InArena, ModifierFlags, ModuleKind, NamedDeclarationInterface, Node, NodeCheckFlags, NodeExt,
    NodeFactory, NodeId, NodeInterface, OptionTry, ScriptTarget, StringOrNumber, SyntaxKind,
    TransformFlags, TransformNodesTransformationResult, TransformationContext,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface,
    UnderscoreEscapedMap, VisitResult,
};

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

pub(super) struct TransformTypeScript {
    pub(super) arena: *const AllArenas,
    pub(super) context: Id<TransformNodesTransformationResult>,
    pub(super) factory: Id<NodeFactory>,
    pub(super) resolver: Id<Box<dyn EmitResolver>>,
    pub(super) compiler_options: Id<CompilerOptions>,
    pub(super) strict_null_checks: bool,
    pub(super) language_version: ScriptTarget,
    pub(super) module_kind: ModuleKind,
    pub(super) current_source_file: Cell<Option<Id<Node /*SourceFile*/>>>,
    pub(super) current_namespace: Cell<Option<Id<Node /*ModuleDeclaration*/>>>,
    pub(super) current_namespace_container_name: Cell<Option<Id<Node /*Identifier*/>>>,
    pub(super) current_lexical_scope:
        Cell<Option<Id<Node /*SourceFile | Block | ModuleBlock | CaseBlock*/>>>,
    pub(super) current_name_scope: Cell<Option<Id<Node /*ClassDeclaration*/>>>,
    pub(super) current_scope_first_declarations_of_name:
        RefCell<Option<UnderscoreEscapedMap<Id<Node>>>>,
    pub(super) current_class_has_parameter_properties: Cell<Option<bool>>,
    pub(super) enabled_substitutions: Cell<TypeScriptSubstitutionFlags>,
    pub(super) class_aliases: RefCell<Option<HashMap<NodeId, Id<Node /*Identifier*/>>>>,
    pub(super) applicable_substitutions: Cell<TypeScriptSubstitutionFlags>,
}

impl TransformTypeScript {
    pub(super) fn new(
        context: Id<TransformNodesTransformationResult>,
        arena: *const AllArenas,
    ) -> Transformer {
        let arena_ref = unsafe { &*arena };
        let context_ref = context.ref_(arena_ref);
        let compiler_options = context_ref.get_compiler_options();
        let ret = arena_ref.alloc_transformer(Box::new(Self {
            arena,
            factory: context_ref.factory(),
            resolver: context_ref.get_emit_resolver(),
            strict_null_checks: get_strict_option_value(
                &compiler_options.ref_(arena_ref),
                "strictNullChecks",
            ),
            language_version: get_emit_script_target(&compiler_options.ref_(arena_ref)),
            module_kind: get_emit_module_kind(&compiler_options.ref_(arena_ref)),
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
        context_ref.override_on_emit_node(&mut |previous_on_emit_node| {
            arena_ref.alloc_transformation_context_on_emit_node_overrider(Box::new(
                TransformTypeScriptOnEmitNodeOverrider::new(ret, previous_on_emit_node, arena_ref),
            ))
        });
        context_ref.override_on_substitute_node(&mut |previous_on_substitute_node| {
            arena_ref.alloc_transformation_context_on_substitute_node_overrider(Box::new(
                TransformTypeScriptOnSubstituteNodeOverrider::new(
                    ret,
                    previous_on_substitute_node,
                    arena_ref,
                ),
            ))
        });

        context_ref.enable_substitution(SyntaxKind::PropertyAccessExpression);
        context_ref.enable_substitution(SyntaxKind::ElementAccessExpression);

        ret
    }

    pub(super) fn maybe_current_source_file(&self) -> Option<Id<Node>> {
        self.current_source_file.get()
    }

    pub(super) fn current_source_file(&self) -> Id<Node> {
        self.current_source_file.get().unwrap()
    }

    pub(super) fn set_current_source_file(&self, current_source_file: Option<Id<Node>>) {
        self.current_source_file.set(current_source_file);
    }

    pub(super) fn maybe_current_namespace(&self) -> Option<Id<Node>> {
        self.current_namespace.get()
    }

    pub(super) fn set_current_namespace(&self, current_namespace: Option<Id<Node>>) {
        self.current_namespace.set(current_namespace);
    }

    pub(super) fn maybe_current_namespace_container_name(&self) -> Option<Id<Node>> {
        self.current_namespace_container_name.get()
    }

    pub(super) fn current_namespace_container_name(&self) -> Id<Node> {
        self.current_namespace_container_name.get().unwrap()
    }

    pub(super) fn set_current_namespace_container_name(
        &self,
        current_namespace_container_name: Option<Id<Node>>,
    ) {
        self.current_namespace_container_name
            .set(current_namespace_container_name);
    }

    pub(super) fn current_lexical_scope(&self) -> Id<Node> {
        self.current_lexical_scope.get().unwrap()
    }

    pub(super) fn maybe_current_lexical_scope(&self) -> Option<Id<Node>> {
        self.current_lexical_scope.get()
    }

    pub(super) fn set_current_lexical_scope(&self, current_lexical_scope: Option<Id<Node>>) {
        self.current_lexical_scope.set(current_lexical_scope);
    }

    pub(super) fn maybe_current_name_scope(&self) -> Option<Id<Node>> {
        self.current_name_scope.get()
    }

    pub(super) fn set_current_name_scope(&self, current_name_scope: Option<Id<Node>>) {
        self.current_name_scope.set(current_name_scope);
    }

    pub(super) fn maybe_current_scope_first_declarations_of_name(
        &self,
    ) -> Ref<Option<UnderscoreEscapedMap<Id<Node>>>> {
        self.current_scope_first_declarations_of_name.borrow()
    }

    pub(super) fn maybe_current_scope_first_declarations_of_name_mut(
        &self,
    ) -> RefMut<Option<UnderscoreEscapedMap<Id<Node>>>> {
        self.current_scope_first_declarations_of_name.borrow_mut()
    }

    pub(super) fn set_current_scope_first_declarations_of_name(
        &self,
        current_scope_first_declarations_of_name: Option<UnderscoreEscapedMap<Id<Node>>>,
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

    pub(super) fn maybe_class_aliases(&self) -> Ref<Option<HashMap<NodeId, Id<Node>>>> {
        self.class_aliases.borrow()
    }

    pub(super) fn class_aliases(&self) -> Ref<HashMap<NodeId, Id<Node>>> {
        ref_unwrapped(&self.class_aliases)
    }

    pub(super) fn class_aliases_mut(&self) -> RefMut<HashMap<NodeId, Id<Node>>> {
        ref_mut_unwrapped(&self.class_aliases)
    }

    pub(super) fn set_class_aliases(&self, class_aliases: Option<HashMap<NodeId, Id<Node>>>) {
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

    pub(super) fn emit_helpers(&self) -> debug_cell::Ref<'_, EmitHelperFactory> {
        self.context.ref_(self).get_emit_helper_factory().ref_(self)
    }

    pub(super) fn transform_source_file_or_bundle(
        &self,
        node: Id<Node>, /*SourceFile | Bundle*/
    ) -> io::Result<Id<Node>> {
        if node.ref_(self).kind() == SyntaxKind::Bundle {
            return self.transform_bundle(node);
        }
        self.transform_source_file(node)
    }

    pub(super) fn transform_bundle(&self, node: Id<Node> /*Bundle*/) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_bundle = node_ref.as_bundle();
        Ok(self.factory.ref_(self).create_bundle(
            node_as_bundle
                .source_files
                .iter()
                .map(|source_file| -> io::Result<_> {
                    Ok(Some(self.transform_source_file(source_file.unwrap())?))
                })
                .collect::<Result<Vec<_>, _>>()?,
            Some(map_defined(
                Some(&node_as_bundle.prepends),
                |prepend: &Id<Node>, _| {
                    if prepend.ref_(self).kind() == SyntaxKind::InputFiles {
                        return Some(create_unparsed_source_file(
                            prepend.clone(),
                            Some("js"),
                            Option::<String>::None,
                        ));
                    }
                    Some(prepend.clone())
                },
            )),
        ))
    }

    pub(super) fn transform_source_file(
        &self,
        node: Id<Node>, /*SourceFile*/
    ) -> io::Result<Id<Node>> {
        if node.ref_(self).as_source_file().is_declaration_file() {
            return Ok(node);
        }

        self.set_current_source_file(Some(node));

        let visited =
            self.try_save_state_and_invoke(node, |node: Id<Node>| self.visit_source_file(node))?;
        add_emit_helpers(
            visited,
            self.context.ref_(self).read_emit_helpers().as_deref(),
            self,
        );

        self.set_current_source_file(None);
        Ok(visited)
    }

    #[allow(dead_code)]
    pub(super) fn save_state_and_invoke<TReturn>(
        &self,
        node: Id<Node>,
        mut f: impl FnMut(Id<Node>) -> TReturn,
    ) -> TReturn {
        self.try_save_state_and_invoke(node, |a| Ok(f(a))).unwrap()
    }

    pub(super) fn try_save_state_and_invoke<TReturn>(
        &self,
        node: Id<Node>,
        mut f: impl FnMut(Id<Node>) -> io::Result<TReturn>,
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

        if self.maybe_current_lexical_scope() != saved_current_scope {
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

    pub(super) fn on_before_visit_node(&self, node: Id<Node>) {
        match node.ref_(self).kind() {
            SyntaxKind::SourceFile
            | SyntaxKind::CaseBlock
            | SyntaxKind::ModuleBlock
            | SyntaxKind::Block => {
                self.set_current_lexical_scope(Some(node));
                self.set_current_name_scope(None);
                self.set_current_scope_first_declarations_of_name(None);
            }
            SyntaxKind::ClassDeclaration | SyntaxKind::FunctionDeclaration => 'case: {
                if has_syntactic_modifier(node, ModifierFlags::Ambient, self) {
                    break 'case;
                }

                if node
                    .ref_(self)
                    .as_named_declaration()
                    .maybe_name()
                    .is_some()
                {
                    self.record_emitted_declaration_in_scope(node);
                } else {
                    Debug_.assert(
                        node.ref_(self).kind() == SyntaxKind::ClassDeclaration
                            || has_syntactic_modifier(node, ModifierFlags::Default, self),
                        None,
                    );
                }
                if is_class_declaration(&node.ref_(self)) {
                    self.set_current_name_scope(Some(node));
                }
            }
            _ => (),
        }
    }

    pub(super) fn visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/ {
        self.try_save_state_and_invoke(node, |node: Id<Node>| self.visitor_worker(node))
    }

    pub(super) fn visitor_worker(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/ {
        if node
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsTypeScript)
        {
            return self.visit_type_script(node);
        }
        Ok(Some(node.into()))
    }

    pub(super) fn source_element_visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/
    {
        self.try_save_state_and_invoke(node, |node: Id<Node>| {
            self.source_element_visitor_worker(node)
        })
    }

    pub(super) fn source_element_visitor_worker(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/
    {
        Ok(match released!(node.ref_(self).kind()) {
            SyntaxKind::ImportDeclaration
            | SyntaxKind::ImportEqualsDeclaration
            | SyntaxKind::ExportAssignment
            | SyntaxKind::ExportDeclaration => self.visit_elidable_statement(node)?,
            _ => self.visitor_worker(node)?,
        })
    }

    pub(super) fn visit_elidable_statement(
        &self,
        node: Id<Node>, /*ImportDeclaration | ImportEqualsDeclaration | ExportAssignment | ExportDeclaration*/
    ) -> io::Result<VisitResult> /*<Node>*/ {
        let parsed = get_parse_tree_node(Some(node), Option::<fn(Id<Node>) -> bool>::None, self);
        if parsed != Some(node) {
            if node
                .ref_(self)
                .transform_flags()
                .intersects(TransformFlags::ContainsTypeScript)
            {
                return Ok(try_maybe_visit_each_child(
                    Some(node),
                    |node: Id<Node>| self.visitor(node),
                    &*self.context.ref_(self),
                    self,
                )?
                .map(Into::into));
            }
            return Ok(Some(node.into()));
        }
        Ok(match node.ref_(self).kind() {
            SyntaxKind::ImportDeclaration => self.visit_import_declaration(node)?,
            SyntaxKind::ImportEqualsDeclaration => self.visit_import_equals_declaration(node)?,
            SyntaxKind::ExportAssignment => self.visit_export_assignment(node)?,
            SyntaxKind::ExportDeclaration => self.visit_export_declaration(node)?,
            _ => Debug_.fail(Some("Unhandled ellided statement")),
        })
    }

    pub(super) fn namespace_element_visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/
    {
        self.try_save_state_and_invoke(node, |node: Id<Node>| {
            self.namespace_element_visitor_worker(node)
        })
    }

    pub(super) fn namespace_element_visitor_worker(
        &self,
        node: Id<Node>,
    ) -> io::Result<VisitResult> /*<Node>*/ {
        if matches!(
            node.ref_(self).kind(),
            SyntaxKind::ExportDeclaration
                | SyntaxKind::ImportDeclaration
                | SyntaxKind::ImportClause
        ) || node.ref_(self).kind() == SyntaxKind::ImportEqualsDeclaration
            && node
                .ref_(self)
                .as_import_equals_declaration()
                .module_reference
                .ref_(self)
                .kind()
                == SyntaxKind::ExternalModuleReference
        {
            return Ok(None);
        } else if node
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsTypeScript)
            || has_syntactic_modifier(node, ModifierFlags::Export, self)
        {
            return self.visit_type_script(node);
        }

        Ok(Some(node.into()))
    }

    pub(super) fn class_element_visitor(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/
    {
        self.try_save_state_and_invoke(node, |node: Id<Node>| {
            self.class_element_visitor_worker(node)
        })
    }

    pub(super) fn class_element_visitor_worker(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/
    {
        Ok(match released!(node.ref_(self).kind()) {
            SyntaxKind::Constructor => self.visit_constructor(node)?,
            SyntaxKind::PropertyDeclaration => self.visit_property_declaration(node)?,
            SyntaxKind::IndexSignature
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::ClassStaticBlockDeclaration => self.visitor_worker(node)?,
            SyntaxKind::SemicolonClassElement => Some(node.into()),
            _ => Debug_.fail_bad_syntax_kind(&node.ref_(self), None),
        })
    }

    pub(super) fn modifier_visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        #[allow(clippy::if_same_then_else)]
        if modifier_to_flag(node.ref_(self).kind()).intersects(ModifierFlags::TypeScriptModifier) {
            return None;
        } else if self.maybe_current_namespace().is_some()
            && node.ref_(self).kind() == SyntaxKind::ExportKeyword
        {
            return None;
        }

        Some(node.into())
    }

    pub(super) fn visit_type_script(&self, node: Id<Node>) -> io::Result<VisitResult> /*<Node>*/ {
        if is_statement(node, self) && has_syntactic_modifier(node, ModifierFlags::Ambient, self) {
            return Ok(Some(
                self.factory
                    .ref_(self)
                    .create_not_emitted_statement(node)
                    .into(),
            ));
        }

        Ok(match released!(node.ref_(self).kind()) {
            SyntaxKind::ExportKeyword | SyntaxKind::DefaultKeyword => {
                if self.maybe_current_namespace().is_some() {
                    None
                } else {
                    Some(node.into())
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
                    .ref_(self)
                    .create_not_emitted_statement(node)
                    .into(),
            ),
            SyntaxKind::PropertyDeclaration => self.visit_property_declaration(node)?,
            SyntaxKind::NamespaceExportDeclaration => None,
            SyntaxKind::Constructor => self.visit_constructor(node)?,
            SyntaxKind::InterfaceDeclaration => Some(
                self.factory
                    .ref_(self)
                    .create_not_emitted_statement(node)
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
            _ => try_maybe_visit_each_child(
                Some(node),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .map(Into::into),
        })
    }
}

impl TransformerInterface for TransformTypeScript {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        self.transform_source_file_or_bundle(node)
    }

    fn as_dyn_any(&self) -> &dyn Any {
        self
    }
}

impl_has_arena!(TransformTypeScript);

struct TransformTypeScriptOnEmitNodeOverrider {
    arena: *const AllArenas,
    transform_type_script: Transformer,
    previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformTypeScriptOnEmitNodeOverrider {
    fn new(
        transform_type_script: Transformer,
        previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            transform_type_script,
            previous_on_emit_node,
        }
    }

    pub(super) fn transform_type_script(&self) -> debug_cell::Ref<'_, TransformTypeScript> {
        downcast_transformer_ref(self.transform_type_script, self)
    }

    pub(super) fn is_transformed_module_declaration(&self, node: Id<Node>) -> bool {
        get_original_node(node, self).ref_(self).kind() == SyntaxKind::ModuleDeclaration
    }

    pub(super) fn is_transformed_enum_declaration(&self, node: Id<Node>) -> bool {
        get_original_node(node, self).ref_(self).kind() == SyntaxKind::EnumDeclaration
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformTypeScriptOnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        let saved_applicable_substitutions =
            self.transform_type_script().applicable_substitutions();
        let saved_current_source_file = self.transform_type_script().maybe_current_source_file();

        if is_source_file(&node.ref_(self)) {
            self.transform_type_script()
                .set_current_source_file(Some(node));
        }

        if self
            .transform_type_script()
            .enabled_substitutions()
            .intersects(TypeScriptSubstitutionFlags::NamespaceExports)
            && self.is_transformed_module_declaration(node)
        {
            self.transform_type_script().set_applicable_substitutions(
                self.transform_type_script().applicable_substitutions()
                    | TypeScriptSubstitutionFlags::NamespaceExports,
            );
        }

        if self
            .transform_type_script()
            .enabled_substitutions()
            .intersects(TypeScriptSubstitutionFlags::NonQualifiedEnumMembers)
            && self.is_transformed_enum_declaration(node)
        {
            self.transform_type_script().set_applicable_substitutions(
                self.transform_type_script().applicable_substitutions()
                    | TypeScriptSubstitutionFlags::NonQualifiedEnumMembers,
            );
        }

        self.previous_on_emit_node
            .ref_(self)
            .on_emit_node(hint, node, emit_callback)?;

        self.transform_type_script()
            .set_applicable_substitutions(saved_applicable_substitutions);
        self.transform_type_script()
            .set_current_source_file(saved_current_source_file);

        Ok(())
    }
}

impl_has_arena!(TransformTypeScriptOnEmitNodeOverrider);

struct TransformTypeScriptOnSubstituteNodeOverrider {
    arena: *const AllArenas,
    transform_type_script: Transformer,
    previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformTypeScriptOnSubstituteNodeOverrider {
    fn new(
        transform_type_script: Transformer,
        previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            transform_type_script,
            previous_on_substitute_node,
        }
    }

    pub(super) fn transform_type_script(&self) -> debug_cell::Ref<'_, TransformTypeScript> {
        downcast_transformer_ref(self.transform_type_script, self)
    }

    fn substitute_shorthand_property_assignment(
        &self,
        node: Id<Node>, /*ShorthandPropertyAssignment*/
    ) -> io::Result<Id<Node /*ObjectLiteralElementLike*/>> {
        let node_ref = node.ref_(self);
        let node_as_shorthand_property_assignment = node_ref.as_shorthand_property_assignment();
        if self
            .transform_type_script()
            .enabled_substitutions()
            .intersects(TypeScriptSubstitutionFlags::NamespaceExports)
        {
            let name = node_as_shorthand_property_assignment.name();
            let exported_name = self.try_substitute_namespace_exported_name(name)?;
            if let Some(exported_name) = exported_name {
                if let Some(node_object_assignment_initializer) =
                    node_as_shorthand_property_assignment
                        .object_assignment_initializer
                        .as_ref()
                {
                    let initializer = self
                        .transform_type_script()
                        .factory
                        .ref_(self)
                        .create_assignment(
                            exported_name,
                            node_object_assignment_initializer.clone(),
                        );
                    return Ok(self
                        .transform_type_script()
                        .factory
                        .ref_(self)
                        .create_property_assignment(name, initializer)
                        .set_text_range(Some(&*node.ref_(self)), self));
                }
                return Ok(self
                    .transform_type_script()
                    .factory
                    .ref_(self)
                    .create_property_assignment(name, exported_name)
                    .set_text_range(Some(&*node.ref_(self)), self));
            }
        }
        Ok(node)
    }

    fn substitute_expression(&self, node: Id<Node> /*Expression*/) -> io::Result<Id<Node>> {
        match node.ref_(self).kind() {
            SyntaxKind::Identifier => {
                return self.substitute_expression_identifier(node);
            }
            SyntaxKind::PropertyAccessExpression => {
                return self.substitute_property_access_expression(node);
            }
            SyntaxKind::ElementAccessExpression => {
                return self.substitute_element_access_expression(node);
            }
            _ => (),
        }

        Ok(node)
    }

    fn substitute_expression_identifier(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<Id<Node /*Expression*/>> {
        Ok(self
            .try_substitute_class_alias(node)?
            .try_or_else(|| self.try_substitute_namespace_exported_name(node))?
            .unwrap_or(node))
    }

    fn try_substitute_class_alias(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<Option<Id<Node /*Expression*/>>> {
        if self
            .transform_type_script()
            .enabled_substitutions()
            .intersects(TypeScriptSubstitutionFlags::ClassAliases)
        {
            if self
                .transform_type_script()
                .resolver
                .ref_(self)
                .get_node_check_flags(node)
                .intersects(NodeCheckFlags::ConstructorReferenceInClass)
            {
                let declaration = self
                    .transform_type_script()
                    .resolver
                    .ref_(self)
                    .get_referenced_value_declaration(node)?;
                if let Some(declaration) = declaration {
                    let transform_type_script_ref = self.transform_type_script();
                    let class_aliases = transform_type_script_ref.class_aliases();
                    let class_alias = class_aliases.get(&declaration.ref_(self).id()).copied();
                    if let Some(class_alias) = class_alias {
                        return Ok(Some(
                            self.transform_type_script()
                                .factory
                                .ref_(self)
                                .clone_node(class_alias)
                                .set_source_map_range(
                                    Some(self.alloc_source_map_range((&*node.ref_(self)).into())),
                                    self,
                                )
                                .set_comment_range(&*node.ref_(self), self),
                        ));
                    }
                }
            }
        }

        Ok(None)
    }

    fn try_substitute_namespace_exported_name(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> io::Result<Option<Id<Node /*Expression*/>>> {
        if self.transform_type_script().enabled_substitutions()
            & self.transform_type_script().applicable_substitutions()
            != TypeScriptSubstitutionFlags::None
            && !is_generated_identifier(&node.ref_(self))
            && !is_local_name(node, self)
        {
            let container = self
                .transform_type_script()
                .resolver
                .ref_(self)
                .get_referenced_export_container(node, Some(false))?;
            if let Some(container) =
                container.filter(|container| container.ref_(self).kind() != SyntaxKind::SourceFile)
            {
                let substitute = self
                    .transform_type_script()
                    .applicable_substitutions()
                    .intersects(TypeScriptSubstitutionFlags::NamespaceExports)
                    && container.ref_(self).kind() == SyntaxKind::ModuleDeclaration
                    || self
                        .transform_type_script()
                        .applicable_substitutions()
                        .intersects(TypeScriptSubstitutionFlags::NonQualifiedEnumMembers)
                        && container.ref_(self).kind() == SyntaxKind::EnumDeclaration;
                if substitute {
                    return Ok(Some(
                        self.transform_type_script()
                            .factory
                            .ref_(self)
                            .create_property_access_expression(
                                self.transform_type_script()
                                    .factory
                                    .ref_(self)
                                    .get_generated_name_for_node(Some(container), None),
                                node,
                            )
                            .set_text_range(Some(&*node.ref_(self)), self),
                    ));
                }
            }
        }

        Ok(None)
    }

    fn substitute_property_access_expression(
        &self,
        node: Id<Node>, /*PropertyAccessExpression*/
    ) -> io::Result<Id<Node>> {
        self.substitute_constant_value(node)
    }

    fn substitute_element_access_expression(
        &self,
        node: Id<Node>, /*ElementAccessExpression*/
    ) -> io::Result<Id<Node>> {
        self.substitute_constant_value(node)
    }

    fn substitute_constant_value(
        &self,
        node: Id<Node>, /*PropertyAccessExpression | ElementAccessExpression*/
    ) -> io::Result<Id<Node /*LeftHandSideExpression*/>> {
        let constant_value = self.try_get_const_enum_value(node)?;

        if let Some(constant_value) = constant_value {
            set_constant_value(node, constant_value.clone(), self);

            let substitute = match constant_value {
                StringOrNumber::String(constant_value) => self
                    .transform_type_script()
                    .factory
                    .ref_(self)
                    .create_string_literal(constant_value, None, None),
                StringOrNumber::Number(constant_value) => self
                    .transform_type_script()
                    .factory
                    .ref_(self)
                    .create_numeric_literal(constant_value, None),
            };
            if self
                .transform_type_script()
                .compiler_options
                .ref_(self)
                .remove_comments
                != Some(true)
            {
                let original_node = maybe_get_original_node_full(
                    Some(node),
                    Some(|node: Option<Id<Node>>| is_access_expression(&node.unwrap().ref_(self))),
                    self,
                )
                // TODO: this looks unsafe, the Typescript version seems to have a wrong typing
                // where the present node argument + present nodeTest should be typed as
                // returning T | undefined (not T), upstream?
                .unwrap();
                let property_name = if is_property_access_expression(&original_node.ref_(self)) {
                    declaration_name_to_string(
                        original_node
                            .ref_(self)
                            .as_property_access_expression()
                            .maybe_name(),
                        self,
                    )
                } else {
                    get_text_of_node(
                        original_node
                            .ref_(self)
                            .as_element_access_expression()
                            .argument_expression,
                        None,
                        self,
                    )
                };

                add_synthetic_trailing_comment(
                    substitute,
                    SyntaxKind::MultiLineCommentTrivia,
                    &format!(" {} ", property_name),
                    None,
                    self,
                );
            }

            return Ok(substitute);
        }

        Ok(node)
    }

    fn try_get_const_enum_value(&self, node: Id<Node>) -> io::Result<Option<StringOrNumber>> {
        if self
            .transform_type_script()
            .compiler_options
            .ref_(self)
            .isolated_modules
            == Some(true)
        {
            return Ok(None);
        }

        (is_property_access_expression(&node.ref_(self))
            || is_element_access_expression(&node.ref_(self)))
        .try_then_and(|| {
            self.transform_type_script()
                .resolver
                .ref_(self)
                .get_constant_value(node)
        })
    }
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformTypeScriptOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        let node = self
            .previous_on_substitute_node
            .ref_(self)
            .on_substitute_node(hint, node)?;
        if hint == EmitHint::Expression {
            return self.substitute_expression(node);
        } else if is_shorthand_property_assignment(&node.ref_(self)) {
            return self.substitute_shorthand_property_assignment(node);
        }

        Ok(node)
    }
}

impl_has_arena!(TransformTypeScriptOnSubstituteNodeOverrider);

struct TransformTypeScriptFactory {
    arena: *const AllArenas,
}

impl TransformTypeScriptFactory {
    fn new(arena: &impl HasArena) -> Self {
        Self {
            arena: arena.arena(),
        }
    }
}

impl TransformerFactoryInterface for TransformTypeScriptFactory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        TransformTypeScript::new(context, self.arena)
    }
}

pub fn transform_type_script(arena: &impl HasArena) -> TransformerFactory {
    arena.alloc_transformer_factory(Box::new(TransformTypeScriptFactory::new(arena)))
}
