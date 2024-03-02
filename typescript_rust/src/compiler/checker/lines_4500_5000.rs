use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    io,
    rc::Rc,
};

use id_arena::Id;

use super::SignatureToSignatureDeclarationOptions;
use crate::{
    add_synthetic_leading_comment, append_if_unique_eq, contains, create_printer,
    create_text_writer, default_maximum_truncation_length, every, get_factory,
    get_first_identifier, get_object_flags, get_text_of_node,
    get_trailing_semicolon_deferring_writer, has_syntactic_modifier, id_text, impl_has_arena,
    is_binding_element, is_expression, is_expression_with_type_arguments_in_class_extends_clause,
    is_external_or_common_js_module, is_identifier_text, is_import_type_node, is_in_js_file,
    is_late_visibility_painted_statement, is_module_with_string_literal_name,
    is_type_reference_node, is_variable_declaration, is_variable_statement, maybe_filter,
    maybe_get_source_file_of_node, no_truncation_maximum_truncation_length,
    pseudo_big_int_to_string, ref_mut_unwrapped, ref_unwrapped, released, set_emit_flags,
    symbol_name, try_map, try_using_single_line_string_writer, using_single_line_string_writer,
    AllArenas, Debug_, EmitFlags, EmitHint, EmitTextWriter, FileIncludeReason, HasArena,
    IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory, InArena, IndexInfo,
    KeywordTypeNode, ModifierFlags, ModuleSpecifierResolutionHost,
    ModuleSpecifierResolutionHostAndGetCommonSourceDirectory, MultiMap, Node, NodeArray,
    NodeBuilderFlags, NodeFlags, NodeInterface, ObjectFlags, Path, PrinterOptionsBuilder, Program,
    RedirectTargetsMap, ScriptTarget, Signature, SignatureKind, Symbol, SymbolAccessibility,
    SymbolFlags, SymbolFormatFlags, SymbolId, SymbolInterface, SymbolTable, SymbolTracker,
    SymbolVisibilityResult, SymlinkCache, SyntaxKind, Type, TypeChecker, TypeCheckerHost,
    TypeFlags, TypeFormatFlags, TypeId, TypeInterface,
};

impl TypeChecker {
    pub(super) fn has_non_global_augmentation_external_module_symbol(
        &self,
        declaration: Id<Node>,
    ) -> bool {
        is_module_with_string_literal_name(declaration, self)
            || declaration.ref_(self).kind() == SyntaxKind::SourceFile
                && is_external_or_common_js_module(&declaration.ref_(self))
    }

    pub(super) fn has_visible_declarations(
        &self,
        symbol: Id<Symbol>,
        should_compute_aliases_to_make_visible: bool,
    ) -> Option<SymbolVisibilityResult> {
        let aliases_to_make_visible: RefCell<
            Option<Vec<Id<Node /*LateVisibilityPaintedStatement*/>>>,
        > = RefCell::new(None);
        if !every(
            &maybe_filter(
                symbol.ref_(self).maybe_declarations().as_deref(),
                |d: &Id<Node>| d.ref_(self).kind() != SyntaxKind::Identifier,
            )
            .unwrap_or_else(|| vec![]),
            |&d: &Id<Node>, _| {
                self.get_is_declaration_visible(
                    symbol,
                    should_compute_aliases_to_make_visible,
                    &aliases_to_make_visible,
                    d,
                )
            },
        ) {
            return None;
        }
        Some(SymbolVisibilityResult {
            accessibility: SymbolAccessibility::Accessible,
            aliases_to_make_visible: aliases_to_make_visible.into_inner(),
            error_symbol_name: None,
            error_node: None,
        })
    }

    pub(super) fn get_is_declaration_visible(
        &self,
        symbol: Id<Symbol>,
        should_compute_aliases_to_make_visible: bool,
        aliases_to_make_visible: &RefCell<Option<Vec<Id<Node>>>>,
        declaration: Id<Node>, /*Declaration*/
    ) -> bool {
        if !self.is_declaration_visible(declaration) {
            let any_import_syntax = self.get_any_import_syntax(declaration);
            if matches!(
                any_import_syntax,
                Some(any_import_syntax) if !has_syntactic_modifier(any_import_syntax, ModifierFlags::Export, self)
                    && self.is_declaration_visible(any_import_syntax.ref_(self).parent())
            ) {
                return self.add_visible_alias(
                    should_compute_aliases_to_make_visible,
                    aliases_to_make_visible,
                    declaration,
                    any_import_syntax.unwrap(),
                );
            } else if is_variable_declaration(&declaration.ref_(self))
                && is_variable_statement(
                    &declaration
                        .ref_(self)
                        .parent()
                        .ref_(self)
                        .parent()
                        .ref_(self),
                )
                && !has_syntactic_modifier(
                    declaration.ref_(self).parent().ref_(self).parent(),
                    ModifierFlags::Export,
                    self,
                )
                && self.is_declaration_visible(
                    declaration
                        .ref_(self)
                        .parent()
                        .ref_(self)
                        .parent()
                        .ref_(self)
                        .parent(),
                )
            {
                return self.add_visible_alias(
                    should_compute_aliases_to_make_visible,
                    aliases_to_make_visible,
                    declaration,
                    declaration.ref_(self).parent().ref_(self).parent(),
                );
            } else if is_late_visibility_painted_statement(&declaration.ref_(self))
                && !has_syntactic_modifier(declaration, ModifierFlags::Export, self)
                && self.is_declaration_visible(declaration.ref_(self).parent())
            {
                return self.add_visible_alias(
                    should_compute_aliases_to_make_visible,
                    aliases_to_make_visible,
                    declaration,
                    declaration,
                );
            } else if symbol.ref_(self).flags().intersects(SymbolFlags::Alias)
                && is_binding_element(&declaration.ref_(self))
                && is_in_js_file(Some(&declaration.ref_(self)))
            {
                let declaration_parent_parent = declaration
                    .ref_(self)
                    .maybe_parent()
                    .and_then(|parent| parent.ref_(self).maybe_parent());
                if let Some(declaration_parent_parent) = declaration_parent_parent {
                    if is_variable_declaration(&declaration_parent_parent.ref_(self)) {
                        let declaration_parent_parent_parent_parent = declaration_parent_parent
                            .ref_(self)
                            .maybe_parent()
                            .and_then(|parent| parent.ref_(self).maybe_parent());
                        if let Some(declaration_parent_parent_parent_parent) =
                            declaration_parent_parent_parent_parent
                        {
                            if is_variable_statement(
                                &declaration_parent_parent_parent_parent.ref_(self),
                            ) && !has_syntactic_modifier(
                                declaration_parent_parent_parent_parent,
                                ModifierFlags::Export,
                                self,
                            ) {
                                let declaration_parent_parent_parent_parent_parent =
                                    declaration_parent_parent_parent_parent
                                        .ref_(self)
                                        .maybe_parent();
                                if let Some(declaration_parent_parent_parent_parent_parent) =
                                    declaration_parent_parent_parent_parent_parent
                                {
                                    if self.is_declaration_visible(
                                        declaration_parent_parent_parent_parent_parent,
                                    ) {
                                        return self.add_visible_alias(
                                            should_compute_aliases_to_make_visible,
                                            aliases_to_make_visible,
                                            declaration,
                                            declaration_parent_parent_parent_parent,
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }

            return false;
        }

        true
    }

    pub(super) fn add_visible_alias(
        &self,
        should_compute_aliases_to_make_visible: bool,
        aliases_to_make_visible: &RefCell<Option<Vec<Id<Node>>>>,
        declaration: Id<Node>,        /*Declaration*/
        aliasing_statement: Id<Node>, /*LateVisibilityPaintedStatement*/
    ) -> bool {
        if should_compute_aliases_to_make_visible {
            self.get_node_links(declaration).ref_mut(self).is_visible = Some(true);
            let mut aliases_to_make_visible = aliases_to_make_visible.borrow_mut();
            if aliases_to_make_visible.is_none() {
                *aliases_to_make_visible = Some(vec![]);
            }
            append_if_unique_eq(
                aliases_to_make_visible.as_mut().unwrap(),
                &aliasing_statement,
            );
        }
        true
    }

    pub(super) fn is_entity_name_visible(
        &self,
        entity_name: Id<Node>, /*EntityNameOrEntityNameExpression*/
        enclosing_declaration: Id<Node>,
    ) -> io::Result<SymbolVisibilityResult> {
        let meaning: SymbolFlags;
        if entity_name.ref_(self).parent().ref_(self).kind() == SyntaxKind::TypeQuery
            || is_expression_with_type_arguments_in_class_extends_clause(
                entity_name.ref_(self).parent(),
                self,
            )
            || entity_name.ref_(self).parent().ref_(self).kind() == SyntaxKind::ComputedPropertyName
        {
            meaning = SymbolFlags::Value | SymbolFlags::ExportValue;
        } else if matches!(
            entity_name.ref_(self).kind(),
            SyntaxKind::QualifiedName | SyntaxKind::PropertyAccessExpression
        ) || entity_name.ref_(self).parent().ref_(self).kind()
            == SyntaxKind::ImportEqualsDeclaration
        {
            meaning = SymbolFlags::Namespace;
        } else {
            meaning = SymbolFlags::Type;
        }

        let first_identifier = get_first_identifier(entity_name, self);
        let symbol = self.resolve_name_(
            Some(enclosing_declaration),
            &first_identifier.ref_(self).as_identifier().escaped_text,
            meaning,
            None,
            Option::<Id<Node>>::None,
            false,
            None,
        )?;
        if matches!(
            symbol,
            Some(symbol) if symbol.ref_(self).flags().intersects(SymbolFlags::TypeParameter) && meaning.intersects(SymbolFlags::Type)
        ) {
            return Ok(SymbolVisibilityResult {
                accessibility: SymbolAccessibility::Accessible,
                aliases_to_make_visible: None,
                error_symbol_name: None,
                error_node: None,
            });
        }

        Ok(symbol
            .and_then(|symbol| self.has_visible_declarations(symbol, true))
            .unwrap_or_else(|| SymbolVisibilityResult {
                accessibility: SymbolAccessibility::NotAccessible,
                aliases_to_make_visible: None,
                error_symbol_name: Some(
                    get_text_of_node(first_identifier, None, self).into_owned(),
                ),
                error_node: Some(first_identifier),
            }))
    }

    pub(super) fn symbol_to_string_(
        &self,
        symbol: Id<Symbol>,
        enclosing_declaration: Option<Id<Node>>,
        meaning: Option<SymbolFlags>,
        flags: Option<SymbolFormatFlags>,
        writer: Option<Id<Box<dyn EmitTextWriter>>>,
    ) -> io::Result<String> {
        let flags = flags.unwrap_or(SymbolFormatFlags::AllowAnyNodeKind);
        let mut node_flags = NodeBuilderFlags::IgnoreErrors;
        if flags.intersects(SymbolFormatFlags::UseOnlyExternalAliasing) {
            node_flags |= NodeBuilderFlags::UseOnlyExternalAliasing;
        }
        if flags.intersects(SymbolFormatFlags::WriteTypeParametersOrArguments) {
            node_flags |= NodeBuilderFlags::WriteTypeParametersInQualifiedName;
        }
        if flags.intersects(SymbolFormatFlags::UseAliasDefinedOutsideCurrentScope) {
            node_flags |= NodeBuilderFlags::UseAliasDefinedOutsideCurrentScope;
        }
        if flags.intersects(SymbolFormatFlags::DoNotIncludeSymbolChain) {
            node_flags |= NodeBuilderFlags::DoNotIncludeSymbolChain;
        }
        let builder = if flags.intersects(SymbolFormatFlags::AllowAnyNodeKind) {
            NodeBuilder::symbol_to_expression
        } else {
            NodeBuilder::symbol_to_entity_name
        };
        let symbol_to_string_worker = |writer: Id<Box<dyn EmitTextWriter>>| -> io::Result<_> {
            let entity = builder(
                &self.node_builder().ref_(self),
                symbol,
                // meaning.unwrap() TODO: this is ! in the Typescript code but would be undefined at runtime when called from propertyRelatedTo()?
                meaning,
                enclosing_declaration,
                Some(node_flags),
                None,
            )?
            .unwrap();
            let printer = if matches!(
                enclosing_declaration,
                Some(enclosing_declaration) if enclosing_declaration.ref_(self).kind() == SyntaxKind::SourceFile
            ) {
                create_printer(
                    PrinterOptionsBuilder::default()
                        .remove_comments(Some(true))
                        .never_ascii_escape(Some(true))
                        .build()
                        .unwrap(),
                    None,
                    self,
                )
            } else {
                create_printer(
                    PrinterOptionsBuilder::default()
                        .remove_comments(Some(true))
                        .build()
                        .unwrap(),
                    None,
                    self,
                )
            };
            let source_file = enclosing_declaration.and_then(|enclosing_declaration| {
                maybe_get_source_file_of_node(Some(enclosing_declaration), self)
            });
            printer
                .ref_(self)
                .write_node(EmitHint::Unspecified, entity, source_file, writer)?;
            // writer

            Ok(())
        };
        Ok(if let Some(writer) = writer {
            symbol_to_string_worker(writer.clone())?;
            writer.ref_(self).get_text()
        } else {
            using_single_line_string_writer(symbol_to_string_worker, self)?
        })
    }

    pub(super) fn signature_to_string_(
        &self,
        signature: Id<Signature>,
        enclosing_declaration: Option<Id<Node>>,
        flags: Option<TypeFormatFlags>,
        kind: Option<SignatureKind>,
        writer: Option<Id<Box<dyn EmitTextWriter>>>,
    ) -> io::Result<String> {
        let flags = flags.unwrap_or(TypeFormatFlags::None);
        Ok(if let Some(writer) = writer {
            self.signature_to_string_worker(
                signature,
                enclosing_declaration,
                flags,
                kind,
                writer.clone(),
            )?;
            writer.ref_(self).get_text()
        } else {
            try_using_single_line_string_writer(
                |writer: Id<Box<dyn EmitTextWriter>>| {
                    self.signature_to_string_worker(
                        signature,
                        enclosing_declaration,
                        flags,
                        kind,
                        writer,
                    )
                },
                self,
            )?
        })
    }

    pub(super) fn signature_to_string_worker(
        &self,
        signature: Id<Signature>,
        enclosing_declaration: Option<Id<Node>>,
        flags: TypeFormatFlags,
        kind: Option<SignatureKind>,
        writer: Id<Box<dyn EmitTextWriter>>,
    ) -> io::Result<()> {
        let sig_output: SyntaxKind;
        if flags.intersects(TypeFormatFlags::WriteArrowStyleSignature) {
            sig_output = if matches!(kind, Some(SignatureKind::Construct)) {
                SyntaxKind::ConstructorType
            } else {
                SyntaxKind::FunctionType
            };
        } else {
            sig_output = if matches!(kind, Some(SignatureKind::Construct)) {
                SyntaxKind::ConstructSignature
            } else {
                SyntaxKind::CallSignature
            };
        }
        let sig = self
            .node_builder()
            .ref_(self)
            .signature_to_signature_declaration(
                signature,
                sig_output,
                enclosing_declaration,
                Some(
                    self.to_node_builder_flags(Some(flags))
                        | NodeBuilderFlags::IgnoreErrors
                        | NodeBuilderFlags::WriteTypeParametersInQualifiedName,
                ),
                None,
            )?;
        let printer = create_printer(
            PrinterOptionsBuilder::default()
                .remove_comments(Some(true))
                .omit_trailing_semicolon(Some(true))
                .build()
                .unwrap(),
            None,
            self,
        );
        let source_file = enclosing_declaration.and_then(|enclosing_declaration| {
            maybe_get_source_file_of_node(Some(enclosing_declaration), self)
        });
        printer.ref_(self).write_node(
            EmitHint::Unspecified,
            sig.unwrap(),
            source_file,
            get_trailing_semicolon_deferring_writer(writer, self),
        )?;
        // writer

        Ok(())
    }

    pub(super) fn type_to_string_(
        &self,
        type_: Id<Type>,
        enclosing_declaration: Option<Id<Node>>,
        flags: Option<TypeFormatFlags>,
        writer: Option<Id<Box<dyn EmitTextWriter>>>,
    ) -> io::Result<String> {
        let flags = flags.unwrap_or(
            TypeFormatFlags::AllowUniqueESSymbolType
                | TypeFormatFlags::UseAliasDefinedOutsideCurrentScope,
        );
        let writer = writer.unwrap_or_else(|| create_text_writer("", self));
        let no_truncation = matches!(
            self.compiler_options.ref_(self).no_error_truncation,
            Some(true)
        ) || flags.intersects(TypeFormatFlags::NoTruncation);
        let type_node = self.node_builder().ref_(self).type_to_type_node(
            type_,
            enclosing_declaration,
            Some(
                self.to_node_builder_flags(Some(flags))
                    | NodeBuilderFlags::IgnoreErrors
                    | if no_truncation {
                        NodeBuilderFlags::NoTruncation
                    } else {
                        NodeBuilderFlags::None
                    },
            ),
            released!(Some(writer.ref_(self).as_symbol_tracker())),
        )?;
        let type_node: Id<Node> = match type_node {
            None => Debug_.fail(Some("should always get typenode")),
            Some(type_node) => type_node,
        };
        let options = PrinterOptionsBuilder::default()
            .remove_comments(Some(type_ != self.unresolved_type()))
            .build()
            .unwrap();
        let printer = create_printer(options, None, self);
        let source_file = enclosing_declaration.and_then(|enclosing_declaration| {
            maybe_get_source_file_of_node(Some(enclosing_declaration), self)
        });
        printer.ref_(self).write_node(
            EmitHint::Unspecified,
            type_node,
            source_file,
            writer.clone(),
        )?;
        let result = writer.ref_(self).get_text();

        let max_length = if no_truncation {
            no_truncation_maximum_truncation_length * 2
        } else {
            default_maximum_truncation_length * 2
        };
        if max_length != 0 && !result.is_empty() && result.len() >= max_length {
            return Ok(format!("{}...", &result[0..max_length - "...".len()]));
        }
        Ok(result)
    }

    pub(super) fn get_type_names_for_error_display(
        &self,
        left: Id<Type>,
        right: Id<Type>,
    ) -> io::Result<(String, String)> {
        let mut left_str = if let Some(symbol) = released!(left.ref_(self).maybe_symbol()) {
            if self.symbol_value_declaration_is_context_sensitive(Some(symbol))? {
                let enclosing_declaration = symbol.ref_(self).maybe_value_declaration();
                self.type_to_string_(left, enclosing_declaration, None, None)?
            } else {
                self.type_to_string_(left, Option::<Id<Node>>::None, None, None)?
            }
        } else {
            self.type_to_string_(left, Option::<Id<Node>>::None, None, None)?
        };
        let mut right_str = if let Some(symbol) = released!(right.ref_(self).maybe_symbol()) {
            if self.symbol_value_declaration_is_context_sensitive(Some(symbol))? {
                let enclosing_declaration = symbol.ref_(self).maybe_value_declaration();
                self.type_to_string_(right, enclosing_declaration, None, None)?
            } else {
                self.type_to_string_(right, Option::<Id<Node>>::None, None, None)?
            }
        } else {
            self.type_to_string_(right, Option::<Id<Node>>::None, None, None)?
        };
        if left_str == right_str {
            left_str = self.get_type_name_for_error_display(left)?;
            right_str = self.get_type_name_for_error_display(right)?;
        }
        Ok((left_str, right_str))
    }

    pub(super) fn get_type_name_for_error_display(&self, type_: Id<Type>) -> io::Result<String> {
        self.type_to_string_(
            type_,
            Option::<Id<Node>>::None,
            Some(TypeFormatFlags::UseFullyQualifiedType),
            None,
        )
    }

    pub(super) fn symbol_value_declaration_is_context_sensitive(
        &self,
        symbol: Option<Id<Symbol>>,
    ) -> io::Result<bool> {
        if symbol.is_none() {
            return Ok(false);
        }
        let symbol = symbol.unwrap();
        Ok(matches!(
            symbol.ref_(self).maybe_value_declaration(),
            Some(value_declaration) if is_expression(value_declaration, self)
                && !self.is_context_sensitive(value_declaration)?
        ))
    }

    pub(super) fn to_node_builder_flags(&self, flags: Option<TypeFormatFlags>) -> NodeBuilderFlags {
        let flags = flags.unwrap_or(TypeFormatFlags::None);
        NodeBuilderFlags::from_bits((flags & TypeFormatFlags::NodeBuilderFlagsMask).bits()).unwrap()
    }

    pub(super) fn is_class_instance_side(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(matches!(
            type_.ref_(self).maybe_symbol(),
            Some(type_symbol) if type_symbol.ref_(self).flags().intersects(SymbolFlags::Class) && (
                type_ == self.get_declared_type_of_class_or_interface(type_symbol)? ||
                type_.ref_(self).flags().intersects(TypeFlags::Object) && get_object_flags(&type_.ref_(self)).intersects(ObjectFlags::IsClassInstanceClone)
            )
        ))
    }

    pub(super) fn create_node_builder(&self) -> Id<NodeBuilder> {
        NodeBuilder::new(self.arena_id(), self)
    }
}

#[derive(Clone, Debug)]
pub struct NodeBuilder {
    arena: *const AllArenas,
    pub(super) _arena_id: Cell<Option<Id<Self>>>,
    pub type_checker: Id<TypeChecker>,
}

impl NodeBuilder {
    pub fn new(type_checker: Id<TypeChecker>, arena: &impl HasArena) -> Id<Self> {
        arena.alloc_node_builder(Self {
            arena: arena.arena(),
            type_checker,
            _arena_id: Default::default(),
        })
    }

    pub fn arena_id(&self) -> Id<Self> {
        self._arena_id.get().unwrap()
    }

    pub fn set_arena_id(&self, id: Id<Self>) {
        self._arena_id.set(Some(id));
    }

    pub fn type_to_type_node(
        &self,
        type_: Id<Type>,
        enclosing_declaration: Option<Id<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Id<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Id<Node>>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            self.type_to_type_node_helper(Some(type_), context)
        })
    }

    pub fn index_info_to_index_signature_declaration(
        &self,
        index_info: Id<IndexInfo>,
        enclosing_declaration: Option<Id<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Id<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Id<Node>>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            Ok(Some(
                self.index_info_to_index_signature_declaration_helper(
                    index_info,
                    context,
                    Option::<Id<Node>>::None,
                )?,
            ))
        })
    }

    pub fn signature_to_signature_declaration(
        &self,
        signature: Id<Signature>,
        kind: SyntaxKind,
        enclosing_declaration: Option<Id<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Id<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Id<Node /*SignatureDeclaration & {typeArguments?: NodeArray<TypeNode>}*/>>>
    {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            Ok(Some(self.signature_to_signature_declaration_helper(
                signature,
                kind,
                context,
                Option::<SignatureToSignatureDeclarationOptions<fn(Id<Symbol>)>>::None,
            )?))
        })
    }

    pub fn symbol_to_entity_name(
        &self,
        symbol: Id<Symbol>,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        enclosing_declaration: Option<Id<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Id<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Id<Node /*EntityName*/>>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            Ok(Some(self.symbol_to_name(symbol, context, meaning, false)?))
        })
    }

    pub fn symbol_to_expression(
        &self,
        symbol: Id<Symbol>,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        enclosing_declaration: Option<Id<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Id<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Id<Node>>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            Ok(Some(self.symbol_to_expression_(symbol, context, meaning)?))
        })
    }

    pub fn symbol_to_type_parameter_declarations(
        &self,
        symbol: Id<Symbol>,
        enclosing_declaration: Option<Id<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Id<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Id<NodeArray> /*<TypeParameterDeclaration>*/>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            self.type_parameters_to_type_parameter_declarations(symbol, context)
        })
    }

    pub fn symbol_to_parameter_declaration(
        &self,
        symbol: Id<Symbol>,
        enclosing_declaration: Option<Id<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Id<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Id<Node /*ParameterDeclaration*/>>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            Ok(Some(self.symbol_to_parameter_declaration_(
                symbol,
                context,
                None,
                Option::<&fn(Id<Symbol>)>::None,
                None,
            )?))
        })
    }

    pub fn type_parameter_to_declaration(
        &self,
        parameter: Id<Type>, /*TypeParameter*/
        enclosing_declaration: Option<Id<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Id<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Id<Node /*TypeParameterDeclaration*/>>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            Ok(Some(self.type_parameter_to_declaration_(
                parameter, context, None,
            )?))
        })
    }

    pub fn symbol_table_to_declaration_statements(
        &self,
        symbol_table: Id<SymbolTable>,
        enclosing_declaration: Option<Id<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Id<Box<dyn SymbolTracker>>>,
        bundled: Option<bool>,
    ) -> io::Result<Option<Vec<Id<Node /*Statement*/>>>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            Ok(Some(self.symbol_table_to_declaration_statements_(
                symbol_table,
                context,
                bundled,
            )?))
        })
    }

    #[allow(dead_code)]
    pub(super) fn with_context<TReturn>(
        &self,
        enclosing_declaration: Option<Id<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Id<Box<dyn SymbolTracker>>>,
        cb: impl FnOnce(Id<NodeBuilderContext>) -> Option<TReturn>,
    ) -> Option<TReturn> {
        Debug_.assert(
            match enclosing_declaration {
                None => true,
                Some(enclosing_declaration) => !enclosing_declaration
                    .ref_(self)
                    .flags()
                    .intersects(NodeFlags::Synthesized),
            },
            None,
        );
        let tracker = tracker
            .filter(|tracker| tracker.ref_(self).is_track_symbol_supported())
            .unwrap_or_else(|| {
                DefaultNodeBuilderContextSymbolTracker::new(
                    self.type_checker.ref_(self).host.clone(),
                    flags,
                    self,
                )
            });
        let context = NodeBuilderContext::new(
            enclosing_declaration,
            flags.unwrap_or(NodeBuilderFlags::None),
            tracker.clone(),
            self,
        );
        let context_tracker =
            wrap_symbol_tracker_to_report_for_context(context.clone(), tracker, self);
        context
            .ref_(self)
            .set_tracker(self.alloc_symbol_tracker(Box::new(context_tracker)));
        let resulting_node = cb(context);
        if context.ref_(self).truncating.get() == Some(true)
            && context
                .ref_(self)
                .flags
                .get()
                .intersects(NodeBuilderFlags::NoTruncation)
        {
            context.ref_(self).tracker_ref().report_truncation_error();
        }
        if context.ref_(self).encountered_error() {
            None
        } else {
            resulting_node
        }
    }

    pub(super) fn try_with_context<TReturn>(
        &self,
        enclosing_declaration: Option<Id<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Id<Box<dyn SymbolTracker>>>,
        cb: impl FnOnce(Id<NodeBuilderContext>) -> io::Result<Option<TReturn>>,
    ) -> io::Result<Option<TReturn>> {
        Debug_.assert(
            match enclosing_declaration {
                None => true,
                Some(enclosing_declaration) => !enclosing_declaration
                    .ref_(self)
                    .flags()
                    .intersects(NodeFlags::Synthesized),
            },
            None,
        );
        let tracker = tracker
            .filter(|tracker| tracker.ref_(self).is_track_symbol_supported())
            .unwrap_or_else(|| {
                DefaultNodeBuilderContextSymbolTracker::new(
                    self.type_checker.ref_(self).host.clone(),
                    flags,
                    self,
                )
            });
        let context = NodeBuilderContext::new(
            enclosing_declaration,
            flags.unwrap_or(NodeBuilderFlags::None),
            tracker.clone(),
            self,
        );
        let context_tracker =
            wrap_symbol_tracker_to_report_for_context(context.clone(), tracker, self);
        context
            .ref_(self)
            .set_tracker(self.alloc_symbol_tracker(Box::new(context_tracker)));
        let resulting_node = cb(context)?;
        if context.ref_(self).truncating.get() == Some(true)
            && context
                .ref_(self)
                .flags
                .get()
                .intersects(NodeBuilderFlags::NoTruncation)
        {
            context.ref_(self).tracker_ref().report_truncation_error();
        }
        Ok(if context.ref_(self).encountered_error() {
            None
        } else {
            resulting_node
        })
    }

    pub(super) fn check_truncation_length(&self, context: Id<NodeBuilderContext>) -> bool {
        if matches!(context.ref_(self).truncating.get(), Some(true)) {
            return true;
        }
        context.ref_(self).truncating.set(Some(
            context.ref_(self).approximate_length.get()
                > if context
                    .ref_(self)
                    .flags()
                    .intersects(NodeBuilderFlags::NoTruncation)
                {
                    no_truncation_maximum_truncation_length
                } else {
                    default_maximum_truncation_length
                },
        ));
        context.ref_(self).truncating.get().unwrap()
    }

    pub(super) fn type_to_type_node_helper(
        &self,
        type_: Option<Id<Type>>,
        context: Id<NodeBuilderContext>,
    ) -> io::Result<Option<Id<Node>>> {
        if let Some(_cancellation_token) = self.type_checker.ref_(self).maybe_cancellation_token()
        /*&& cancellationToken.throwIfCancellationRequested*/
        {
            // cancellationToken.throwIfCancellationRequested();
        }
        let in_type_alias = context
            .ref_(self)
            .flags()
            .intersects(NodeBuilderFlags::InTypeAlias);
        context
            .ref_(self)
            .set_flags(context.ref_(self).flags() & !NodeBuilderFlags::InTypeAlias);

        if type_.is_none() {
            if !context
                .ref_(self)
                .flags()
                .intersects(NodeBuilderFlags::AllowEmptyUnionOrIntersection)
            {
                context.ref_(self).set_encountered_error(true);
                return Ok(None);
            }
            context.ref_(self).increment_approximate_length_by(3);
            return Ok(Some(
                self.alloc_node(
                    KeywordTypeNode::from(
                        get_factory(self).create_keyword_type_node_raw(SyntaxKind::AnyKeyword),
                    )
                    .into(),
                ),
            ));
        }
        let mut type_ = type_.unwrap();

        if !context
            .ref_(self)
            .flags()
            .intersects(NodeBuilderFlags::NoTypeReduction)
        {
            type_ = self.type_checker.ref_(self).get_reduced_type(type_)?;
        }

        if type_.ref_(self).flags().intersects(TypeFlags::Any) {
            if let Some(type_alias_symbol) = type_.ref_(self).maybe_alias_symbol() {
                return Ok(Some(get_factory(self).create_type_reference_node(
                    self.symbol_to_entity_name_node(type_alias_symbol),
                    self.map_to_type_nodes(
                        type_.ref_(self).maybe_alias_type_arguments().as_deref(),
                        context,
                        None,
                    )?,
                )));
            }
            if type_ == self.type_checker.ref_(self).unresolved_type() {
                let ret = self.alloc_node(
                    KeywordTypeNode::from(
                        get_factory(self).create_keyword_type_node_raw(SyntaxKind::AnyKeyword),
                    )
                    .into(),
                );
                add_synthetic_leading_comment(
                    ret,
                    SyntaxKind::MultiLineCommentTrivia,
                    "unresolved",
                    None,
                    self,
                );
                return Ok(Some(ret));
            }
            context.ref_(self).increment_approximate_length_by(3);
            return Ok(Some(
                self.alloc_node(
                    KeywordTypeNode::from(get_factory(self).create_keyword_type_node_raw(
                        if type_ == self.type_checker.ref_(self).intrinsic_marker_type() {
                            SyntaxKind::IntrinsicKeyword
                        } else {
                            SyntaxKind::AnyKeyword
                        },
                    ))
                    .into(),
                ),
            ));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::Unknown) {
            return Ok(Some(
                self.alloc_node(
                    KeywordTypeNode::from(
                        get_factory(self).create_keyword_type_node_raw(SyntaxKind::UnknownKeyword),
                    )
                    .into(),
                ),
            ));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::String) {
            context.ref_(self).increment_approximate_length_by(6);
            return Ok(Some(
                self.alloc_node(
                    KeywordTypeNode::from(
                        get_factory(self).create_keyword_type_node_raw(SyntaxKind::StringKeyword),
                    )
                    .into(),
                ),
            ));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::Number) {
            context.ref_(self).increment_approximate_length_by(6);
            return Ok(Some(
                self.alloc_node(
                    KeywordTypeNode::from(
                        get_factory(self).create_keyword_type_node_raw(SyntaxKind::NumberKeyword),
                    )
                    .into(),
                ),
            ));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::BigInt) {
            context.ref_(self).increment_approximate_length_by(6);
            return Ok(Some(
                self.alloc_node(
                    KeywordTypeNode::from(
                        get_factory(self).create_keyword_type_node_raw(SyntaxKind::BigIntKeyword),
                    )
                    .into(),
                ),
            ));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::Boolean)
            && type_.ref_(self).maybe_alias_symbol().is_none()
        {
            context.ref_(self).increment_approximate_length_by(7);
            return Ok(Some(
                self.alloc_node(
                    KeywordTypeNode::from(
                        get_factory(self).create_keyword_type_node_raw(SyntaxKind::BooleanKeyword),
                    )
                    .into(),
                ),
            ));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::EnumLiteral)
            && !type_.ref_(self).flags().intersects(TypeFlags::Union)
        {
            let parent_symbol = self
                .type_checker
                .ref_(self)
                .get_parent_of_symbol(type_.ref_(self).symbol())?
                .unwrap();
            let parent_name =
                self.symbol_to_type_node(parent_symbol, context, SymbolFlags::Type, None)?;
            if self
                .type_checker
                .ref_(self)
                .get_declared_type_of_symbol(parent_symbol)?
                == type_
            {
                return Ok(Some(parent_name));
            }
            let type_symbol = type_.ref_(self).symbol();
            let member_name = symbol_name(type_symbol, self);
            if is_identifier_text(&member_name, Some(ScriptTarget::ES3), None) {
                return Ok(Some(
                    self.append_reference_to_type(
                        parent_name,
                        self.alloc_node(
                            get_factory(self)
                                .create_type_reference_node_raw(
                                    &*member_name,
                                    Option::<Id<NodeArray>>::None,
                                )
                                .into(),
                        ),
                    ),
                ));
            }
            if is_import_type_node(&parent_name.ref_(self)) {
                parent_name
                    .ref_(self)
                    .as_import_type_node()
                    .set_is_type_of(true);
                return Ok(Some(get_factory(self).create_indexed_access_type_node(
                    parent_name,
                    get_factory(self).create_literal_type_node(
                        get_factory(self).create_string_literal(member_name, None, None),
                    ),
                )));
            } else if is_type_reference_node(&parent_name.ref_(self)) {
                return Ok(Some(get_factory(self).create_indexed_access_type_node(
                    get_factory(self).create_type_query_node(
                        parent_name.ref_(self).as_type_reference_node().type_name,
                    ),
                    get_factory(self).create_literal_type_node(
                        get_factory(self).create_string_literal(member_name, None, None),
                    ),
                )));
            } else {
                Debug_.fail(Some(
                    "Unhandled type node kind returned from `symbolToTypeNode`.",
                ));
            }
        }
        if type_.ref_(self).flags().intersects(TypeFlags::EnumLike) {
            return Ok(Some(self.symbol_to_type_node(
                type_.ref_(self).symbol(),
                context,
                SymbolFlags::Type,
                None,
            )?));
        }
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::StringLiteral)
        {
            let value = type_.ref_(self).as_string_literal_type().value.clone();
            context
                .ref_(self)
                .increment_approximate_length_by(value.len() + 2);
            return Ok(Some(
                get_factory(self).create_literal_type_node(set_emit_flags(
                    get_factory(self).create_string_literal(
                        value,
                        Some(
                            context
                                .ref_(self)
                                .flags()
                                .intersects(NodeBuilderFlags::UseSingleQuotesForStringLiteralType),
                        ),
                        None,
                    ),
                    EmitFlags::NoAsciiEscaping,
                    self,
                )),
            ));
        }
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::NumberLiteral)
        {
            let value = type_.ref_(self).as_number_literal_type().value.value();
            context
                .ref_(self)
                .increment_approximate_length_by(value.to_string().len());
            return Ok(Some(get_factory(self).create_literal_type_node(
                if value < 0.0 {
                    get_factory(self).create_prefix_unary_expression(
                        SyntaxKind::MinusToken,
                        get_factory(self).create_numeric_literal((-value).to_string(), None),
                    )
                } else {
                    get_factory(self).create_numeric_literal(value.to_string(), None)
                },
            )));
        }
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::BigIntLiteral)
        {
            let type_ref = type_.ref_(self);
            let value = &type_ref.as_big_int_literal_type().value;
            context
                .ref_(self)
                .increment_approximate_length_by(pseudo_big_int_to_string(value).len() + 1);
            return Ok(Some(get_factory(self).create_literal_type_node(
                get_factory(self).create_big_int_literal(value.clone()),
            )));
        }
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::BooleanLiteral)
        {
            let type_ref = type_.ref_(self);
            let type_intrinsic_name = type_ref.as_intrinsic_type().intrinsic_name();
            context
                .ref_(self)
                .increment_approximate_length_by(type_intrinsic_name.len());
            return Ok(Some(
                get_factory(self).create_literal_type_node(
                    self.alloc_node(
                        if type_intrinsic_name == "true" {
                            get_factory(self).create_true_raw()
                        } else {
                            get_factory(self).create_false_raw()
                        }
                        .into(),
                    ),
                ),
            ));
        }
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::UniqueESSymbol)
        {
            if !context
                .ref_(self)
                .flags()
                .intersects(NodeBuilderFlags::AllowUniqueESSymbolType)
            {
                if self.type_checker.ref_(self).is_value_symbol_accessible(
                    type_.ref_(self).symbol(),
                    context.ref_(self).maybe_enclosing_declaration(),
                )? {
                    context.ref_(self).increment_approximate_length_by(6);
                    return Ok(Some(self.symbol_to_type_node(
                        type_.ref_(self).symbol(),
                        context,
                        SymbolFlags::Value,
                        None,
                    )?));
                }
                // if (context.tracker.reportInaccessibleUniqueSymbolError) {
                context
                    .ref_(self)
                    .tracker_ref()
                    .report_inaccessible_unique_symbol_error();
                // }
            }
            context.ref_(self).increment_approximate_length_by(13);
            return Ok(Some(
                get_factory(self).create_type_operator_node(
                    SyntaxKind::UniqueKeyword,
                    self.alloc_node(
                        KeywordTypeNode::from(
                            get_factory(self)
                                .create_keyword_type_node_raw(SyntaxKind::SymbolKeyword),
                        )
                        .into(),
                    ),
                ),
            ));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::Void) {
            context.ref_(self).increment_approximate_length_by(4);
            return Ok(Some(
                self.alloc_node(
                    KeywordTypeNode::from(
                        get_factory(self).create_keyword_type_node_raw(SyntaxKind::VoidKeyword),
                    )
                    .into(),
                ),
            ));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::Undefined) {
            context.ref_(self).increment_approximate_length_by(9);
            return Ok(Some(
                self.alloc_node(
                    KeywordTypeNode::from(
                        get_factory(self)
                            .create_keyword_type_node_raw(SyntaxKind::UndefinedKeyword),
                    )
                    .into(),
                ),
            ));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::Null) {
            context.ref_(self).increment_approximate_length_by(9);
            return Ok(Some(
                get_factory(self).create_literal_type_node(get_factory(self).create_null()),
            ));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::Never) {
            context.ref_(self).increment_approximate_length_by(5);
            return Ok(Some(
                self.alloc_node(
                    KeywordTypeNode::from(
                        get_factory(self).create_keyword_type_node_raw(SyntaxKind::NeverKeyword),
                    )
                    .into(),
                ),
            ));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::ESSymbol) {
            context.ref_(self).increment_approximate_length_by(6);
            return Ok(Some(
                self.alloc_node(
                    KeywordTypeNode::from(
                        get_factory(self).create_keyword_type_node_raw(SyntaxKind::SymbolKeyword),
                    )
                    .into(),
                ),
            ));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::NonPrimitive) {
            context.ref_(self).increment_approximate_length_by(6);
            return Ok(Some(
                self.alloc_node(
                    KeywordTypeNode::from(
                        get_factory(self).create_keyword_type_node_raw(SyntaxKind::ObjectKeyword),
                    )
                    .into(),
                ),
            ));
        }
        if self.type_checker.ref_(self).is_this_type_parameter(type_) {
            if context
                .ref_(self)
                .flags()
                .intersects(NodeBuilderFlags::InObjectTypeLiteral)
            {
                if !context.ref_(self).encountered_error()
                    && !context
                        .ref_(self)
                        .flags()
                        .intersects(NodeBuilderFlags::AllowThisInObjectLiteral)
                {
                    context.ref_(self).set_encountered_error(true);
                }
                // if (context.tracker.reportInaccessibleUniqueSymbolError) {
                context
                    .ref_(self)
                    .tracker_ref()
                    .report_inaccessible_unique_symbol_error();
                // }
            }
            context.ref_(self).increment_approximate_length_by(4);
            return Ok(Some(get_factory(self).create_this_type_node()));
        }

        if !in_type_alias {
            if let Some(type_alias_symbol) = type_.ref_(self).maybe_alias_symbol() {
                if context
                    .ref_(self)
                    .flags()
                    .intersects(NodeBuilderFlags::UseAliasDefinedOutsideCurrentScope)
                    || self.type_checker.ref_(self).is_type_symbol_accessible(
                        type_alias_symbol,
                        context.ref_(self).maybe_enclosing_declaration(),
                    )?
                {
                    let type_argument_nodes = self.map_to_type_nodes(
                        type_.ref_(self).maybe_alias_type_arguments().as_deref(),
                        context,
                        None,
                    )?;
                    if self
                        .type_checker
                        .ref_(self)
                        .is_reserved_member_name(type_alias_symbol.ref_(self).escaped_name())
                        && !type_alias_symbol
                            .ref_(self)
                            .flags()
                            .intersects(SymbolFlags::Class)
                    {
                        return Ok(Some(get_factory(self).create_type_reference_node(
                            get_factory(self).create_identifier(""),
                            type_argument_nodes,
                        )));
                    }
                    return Ok(Some(self.symbol_to_type_node(
                        type_alias_symbol,
                        context,
                        SymbolFlags::Type,
                        type_argument_nodes.as_deref(),
                    )?));
                }
            }
        }

        let object_flags = get_object_flags(&type_.ref_(self));

        if object_flags.intersects(ObjectFlags::Reference) {
            Debug_.assert(type_.ref_(self).flags().intersects(TypeFlags::Object), None);
            return Ok(Some(
                if type_
                    .ref_(self)
                    .as_type_reference_interface()
                    .maybe_node()
                    .is_some()
                {
                    self.try_visit_and_transform_type(context, type_, |type_| {
                        Ok(self.type_reference_to_type_node(context, type_)?.unwrap())
                    })?
                } else {
                    self.type_reference_to_type_node(context, type_)?.unwrap()
                },
            ));
        }
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::TypeParameter)
            || object_flags.intersects(ObjectFlags::ClassOrInterface)
        {
            if type_
                .ref_(self)
                .flags()
                .intersects(TypeFlags::TypeParameter)
                && contains(
                    context
                        .ref_(self)
                        .infer_type_parameters
                        .ref_(self)
                        .as_deref(),
                    &type_,
                )
            {
                context.ref_(self).increment_approximate_length_by(
                    symbol_name(type_.ref_(self).symbol(), self).len() + 6,
                );
                return Ok(Some(get_factory(self).create_infer_type_node(
                    self.type_parameter_to_declaration_with_constraint(type_, context, None)?,
                )));
            }
            if context
                .ref_(self)
                .flags()
                .intersects(NodeBuilderFlags::GenerateNamesForShadowedTypeParams)
                && type_
                    .ref_(self)
                    .flags()
                    .intersects(TypeFlags::TypeParameter)
                && !self.type_checker.ref_(self).is_type_symbol_accessible(
                    type_.ref_(self).symbol(),
                    released!(context.ref_(self).maybe_enclosing_declaration()),
                )?
            {
                let name = self.type_parameter_to_name(type_, context)?;
                context
                    .ref_(self)
                    .increment_approximate_length_by(id_text(&name.ref_(self)).len());
                return Ok(Some(get_factory(self).create_type_reference_node(
                    get_factory(self).create_identifier(&id_text(&name.ref_(self))),
                    Option::<Id<NodeArray>>::None,
                )));
            }
            return Ok(Some(
                if let Some(type_symbol) = type_.ref_(self).maybe_symbol() {
                    self.symbol_to_type_node(type_symbol, context, SymbolFlags::Type, None)?
                } else {
                    get_factory(self).create_type_reference_node(
                        get_factory(self).create_identifier("?"),
                        Option::<Id<NodeArray>>::None,
                    )
                },
            ));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::Union) {
            if let Some(type_origin) = type_.ref_(self).as_union_type().origin.as_ref() {
                type_ = type_origin.clone();
            }
        }
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::Union | TypeFlags::Intersection)
        {
            let types = {
                let type_ref = type_.ref_(self);
                let types = type_ref.as_union_or_intersection_type_interface().types();
                if type_.ref_(self).flags().intersects(TypeFlags::Union) {
                    self.type_checker.ref_(self).format_union_types(types)?
                } else {
                    types.to_vec()
                }
            };
            if types.len() == 1 {
                return self.type_to_type_node_helper(Some(types[0]), context);
            }
            let type_nodes = self.map_to_type_nodes(Some(&types), context, Some(true))?;
            if let Some(type_nodes) = type_nodes {
                if !type_nodes.is_empty() {
                    return Ok(Some(
                        if type_.ref_(self).flags().intersects(TypeFlags::Union) {
                            get_factory(self).create_union_type_node(type_nodes)
                        } else {
                            get_factory(self).create_intersection_type_node(type_nodes)
                        },
                    ));
                }
            }
            if !context.ref_(self).encountered_error()
                && !context
                    .ref_(self)
                    .flags()
                    .intersects(NodeBuilderFlags::AllowEmptyUnionOrIntersection)
            {
                context.ref_(self).set_encountered_error(true);
            }
            return Ok(None);
        }
        if object_flags.intersects(ObjectFlags::Anonymous | ObjectFlags::Mapped) {
            Debug_.assert(type_.ref_(self).flags().intersects(TypeFlags::Object), None);
            return Ok(Some(self.create_anonymous_type_node(context, type_)?));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::Index) {
            let indexed_type = type_.ref_(self).as_index_type().type_;
            context.ref_(self).increment_approximate_length_by(6);
            let index_type_node = self
                .type_to_type_node_helper(Some(indexed_type), context)?
                .unwrap();
            return Ok(Some(get_factory(self).create_type_operator_node(
                SyntaxKind::KeyOfKeyword,
                index_type_node,
            )));
        }
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::TemplateLiteral)
        {
            let type_ref = type_.ref_(self);
            let texts = &type_ref.as_template_literal_type().texts;
            let types = &type_ref.as_template_literal_type().types;
            let template_head: Id<Node> =
                get_factory(self).create_template_head(Some(texts[0].clone()), None, None);
            let template_spans = get_factory(self).create_node_array(
                Some(try_map(
                    types,
                    |&t: &Id<Type>, i| -> io::Result<Id<Node>> {
                        Ok(get_factory(self).create_template_literal_type_span(
                            self.type_to_type_node_helper(Some(t), context)?.unwrap(),
                            if i < types.len() - 1 {
                                get_factory(self).create_template_middle(
                                    Some(texts[i + 1].clone()),
                                    None,
                                    None,
                                )
                            } else {
                                get_factory(self).create_template_tail(
                                    Some(texts[i + 1].clone()),
                                    None,
                                    None,
                                )
                            },
                        ))
                    },
                )?),
                None,
            );
            context.ref_(self).increment_approximate_length_by(2);
            return Ok(Some(
                get_factory(self).create_template_literal_type(template_head, template_spans),
            ));
        }
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::StringMapping)
        {
            let type_node = self
                .type_to_type_node_helper(
                    Some(type_.ref_(self).as_string_mapping_type().type_),
                    context,
                )?
                .unwrap();
            return Ok(Some(self.symbol_to_type_node(
                type_.ref_(self).symbol(),
                context,
                SymbolFlags::Type,
                Some(&vec![type_node]),
            )?));
        }
        if type_
            .ref_(self)
            .flags()
            .intersects(TypeFlags::IndexedAccess)
        {
            let object_type_node = self
                .type_to_type_node_helper(
                    Some(type_.ref_(self).as_indexed_access_type().object_type),
                    context,
                )?
                .unwrap();
            let index_type_node = self
                .type_to_type_node_helper(
                    Some(type_.ref_(self).as_indexed_access_type().index_type),
                    context,
                )?
                .unwrap();
            context.ref_(self).increment_approximate_length_by(2);
            return Ok(Some(get_factory(self).create_indexed_access_type_node(
                object_type_node,
                index_type_node,
            )));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::Conditional) {
            return Ok(Some(self.try_visit_and_transform_type(
                context,
                type_,
                |type_| self.conditional_type_to_type_node(context, type_),
            )?));
        }
        if type_.ref_(self).flags().intersects(TypeFlags::Substitution) {
            return self.type_to_type_node_helper(
                Some(type_.ref_(self).as_substitution_type().base_type),
                context,
            );
        }

        Debug_.fail(Some("Should be unreachable."));
    }
}

impl_has_arena!(NodeBuilder);

struct DefaultNodeBuilderContextSymbolTracker {
    pub module_resolver_host:
        Option<Id<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>>>,
}

impl DefaultNodeBuilderContextSymbolTracker {
    pub fn new(
        host: Id<Program /*TypeCheckerHostDebuggable*/>,
        flags: Option<NodeBuilderFlags>,
        arena: &impl HasArena,
    ) -> Id<Box<dyn SymbolTracker>> {
        arena.alloc_symbol_tracker(Box::new(Self {
            module_resolver_host: if matches!(
                flags,
                Some(flags) if flags.intersects(NodeBuilderFlags::DoNotIncludeSymbolChain)
            ) {
                Some(
                    arena.alloc_module_specifier_resolution_host_and_get_common_source_directory(
                        Box::new(
                            DefaultNodeBuilderContextSymbolTrackerModuleResolverHost::new(
                                host, arena,
                            ),
                        ),
                    ),
                )
            } else {
                None
            },
        }))
    }
}

impl SymbolTracker for DefaultNodeBuilderContextSymbolTracker {
    fn track_symbol(
        &self,
        _symbol: Id<Symbol>,
        _enclosing_declaration: Option<Id<Node>>,
        _meaning: SymbolFlags,
    ) -> Option<io::Result<bool>> {
        Some(Ok(false))
    }

    fn is_track_symbol_supported(&self) -> bool {
        true
    }

    fn disable_track_symbol(&self) {}

    fn reenable_track_symbol(&self) {}

    fn module_resolver_host(
        &self,
    ) -> Option<IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory> {
        self.module_resolver_host.map(Into::into)
    }

    fn is_module_resolver_host_supported(&self) -> bool {
        self.module_resolver_host.as_ref().is_some()
    }

    // TODO: are these correct?
    fn is_report_inaccessible_this_error_supported(&self) -> bool {
        false
    }

    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool {
        false
    }

    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool {
        false
    }

    fn is_report_cyclic_structure_error_supported(&self) -> bool {
        false
    }

    fn is_report_likely_unsafe_import_required_error_supported(&self) -> bool {
        false
    }

    fn is_report_nonlocal_augmentation_supported(&self) -> bool {
        false
    }

    fn is_report_non_serializable_property_supported(&self) -> bool {
        false
    }

    fn is_track_referenced_ambient_module_supported(&self) -> bool {
        false
    }
}

struct DefaultNodeBuilderContextSymbolTrackerModuleResolverHost {
    arena: *const AllArenas,
    pub host: Id<Program /*TypeCheckerHost*/>,
}

impl DefaultNodeBuilderContextSymbolTrackerModuleResolverHost {
    pub fn new(host: Id<Program /*TypeCheckerHost*/>, arena: &impl HasArena) -> Self {
        Self {
            host,
            arena: arena.arena(),
        }
    }
}

impl ModuleSpecifierResolutionHostAndGetCommonSourceDirectory
    for DefaultNodeBuilderContextSymbolTrackerModuleResolverHost
{
    fn get_common_source_directory(&self) -> String {
        TypeCheckerHost::get_common_source_directory(&*self.host.ref_(self))
            .unwrap_or_else(|| "".to_owned())
    }

    fn as_dyn_module_specifier_resolution_host(&self) -> &dyn ModuleSpecifierResolutionHost {
        self
    }
}

impl ModuleSpecifierResolutionHost for DefaultNodeBuilderContextSymbolTrackerModuleResolverHost {
    fn get_current_directory(&self) -> String {
        self.host.ref_(self).get_current_directory()
    }

    fn get_symlink_cache(&self) -> Option<Id<SymlinkCache>> {
        ModuleSpecifierResolutionHost::get_symlink_cache(&*self.host.ref_(self))
    }

    fn use_case_sensitive_file_names(&self) -> Option<bool> {
        ModuleSpecifierResolutionHost::use_case_sensitive_file_names(&*self.host.ref_(self))
    }

    fn redirect_targets_map(&self) -> Rc<RefCell<RedirectTargetsMap>> {
        self.host.ref_(self).redirect_targets_map()
    }

    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String> {
        ModuleSpecifierResolutionHost::get_project_reference_redirect(
            &*self.host.ref_(self),
            file_name,
        )
    }

    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool {
        ModuleSpecifierResolutionHost::is_source_of_project_reference_redirect(
            &*self.host.ref_(self),
            file_name,
        )
    }

    fn file_exists(&self, file_name: &str) -> bool {
        self.host.ref_(self).file_exists(file_name)
    }

    fn get_file_include_reasons(&self) -> Id<MultiMap<Path, Id<FileIncludeReason>>> {
        self.host.ref_(self).get_file_include_reasons()
    }

    fn read_file(&self, file_name: &str) -> Option<io::Result<Option<String>>> {
        self.host.ref_(self).read_file(file_name)
    }

    fn is_read_file_supported(&self) -> bool {
        self.host.ref_(self).is_read_file_supported()
    }

    fn is_get_nearest_ancestor_directory_with_package_json_supported(&self) -> bool {
        false
    }
}

impl_has_arena!(DefaultNodeBuilderContextSymbolTrackerModuleResolverHost);

pub(super) fn wrap_symbol_tracker_to_report_for_context(
    context: Id<NodeBuilderContext>,
    tracker: Id<Box<dyn SymbolTracker>>,
    arena: &impl HasArena,
) -> NodeBuilderContextWrappedSymbolTracker {
    NodeBuilderContextWrappedSymbolTracker::new(tracker, context, arena)
}

pub(super) struct NodeBuilderContextWrappedSymbolTracker {
    arena: *const AllArenas,
    is_track_symbol_disabled: Cell<bool>,
    tracker: Id<Box<dyn SymbolTracker>>,
    context: Id<NodeBuilderContext>,
}

impl NodeBuilderContextWrappedSymbolTracker {
    pub(super) fn new(
        tracker: Id<Box<dyn SymbolTracker>>,
        context: Id<NodeBuilderContext>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            is_track_symbol_disabled: Default::default(),
            tracker,
            context,
        }
    }

    fn mark_context_reported_diagnostic(&self) {
        self.context.ref_(self).reported_diagnostic.set(true);
    }
}

impl SymbolTracker for NodeBuilderContextWrappedSymbolTracker {
    fn report_cyclic_structure_error(&self) {
        if self
            .tracker
            .ref_(self)
            .is_report_cyclic_structure_error_supported()
        {
            self.mark_context_reported_diagnostic();
        }
        self.tracker.ref_(self).report_cyclic_structure_error()
    }

    fn is_report_cyclic_structure_error_supported(&self) -> bool {
        self.tracker
            .ref_(self)
            .is_report_cyclic_structure_error_supported()
    }

    fn report_inaccessible_this_error(&self) {
        if self
            .tracker
            .ref_(self)
            .is_report_inaccessible_this_error_supported()
        {
            self.mark_context_reported_diagnostic();
        }
        self.tracker.ref_(self).report_inaccessible_this_error()
    }

    fn is_report_inaccessible_this_error_supported(&self) -> bool {
        self.tracker
            .ref_(self)
            .is_report_inaccessible_this_error_supported()
    }

    fn report_inaccessible_unique_symbol_error(&self) {
        if self
            .tracker
            .ref_(self)
            .is_report_inaccessible_unique_symbol_error_supported()
        {
            self.mark_context_reported_diagnostic();
        }
        self.tracker
            .ref_(self)
            .report_inaccessible_unique_symbol_error()
    }

    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool {
        self.tracker
            .ref_(self)
            .is_report_inaccessible_unique_symbol_error_supported()
    }

    fn report_likely_unsafe_import_required_error(&self, specifier: &str) {
        if self
            .tracker
            .ref_(self)
            .is_report_likely_unsafe_import_required_error_supported()
        {
            self.mark_context_reported_diagnostic();
        }
        self.tracker
            .ref_(self)
            .report_likely_unsafe_import_required_error(specifier)
    }

    fn is_report_likely_unsafe_import_required_error_supported(&self) -> bool {
        self.tracker
            .ref_(self)
            .is_report_likely_unsafe_import_required_error_supported()
    }

    fn report_nonlocal_augmentation(
        &self,
        containing_file: Id<Node>, /*SourceFile*/
        parent_symbol: Id<Symbol>,
        augmenting_symbol: Id<Symbol>,
    ) {
        if self
            .tracker
            .ref_(self)
            .is_report_nonlocal_augmentation_supported()
        {
            self.mark_context_reported_diagnostic();
        }
        self.tracker.ref_(self).report_nonlocal_augmentation(
            containing_file,
            parent_symbol,
            augmenting_symbol,
        )
    }

    fn is_report_nonlocal_augmentation_supported(&self) -> bool {
        self.tracker
            .ref_(self)
            .is_report_nonlocal_augmentation_supported()
    }

    fn report_private_in_base_of_class_expression(&self, property_name: &str) {
        if self
            .tracker
            .ref_(self)
            .is_report_private_in_base_of_class_expression_supported()
        {
            self.mark_context_reported_diagnostic();
        }
        self.tracker
            .ref_(self)
            .report_private_in_base_of_class_expression(property_name)
    }

    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool {
        self.tracker
            .ref_(self)
            .is_report_private_in_base_of_class_expression_supported()
    }

    fn report_non_serializable_property(&self, property_name: &str) {
        if self
            .tracker
            .ref_(self)
            .is_report_non_serializable_property_supported()
        {
            self.mark_context_reported_diagnostic();
        }
        self.tracker
            .ref_(self)
            .report_non_serializable_property(property_name)
    }

    fn is_report_non_serializable_property_supported(&self) -> bool {
        self.tracker
            .ref_(self)
            .is_report_non_serializable_property_supported()
    }

    fn track_symbol(
        &self,
        symbol: Id<Symbol>,
        enclosing_declaration: Option<Id<Node>>,
        meaning: SymbolFlags,
    ) -> Option<io::Result<bool>> {
        if self.is_track_symbol_disabled.get() {
            return Some(Ok(false));
        }
        let result = self
            .tracker
            .ref_(self)
            .track_symbol(symbol, enclosing_declaration, meaning);
        if matches!(
            result.as_ref(),
            Some(result) if matches!(
                result,
                Ok(result) if *result
            )
        ) {
            self.mark_context_reported_diagnostic();
        }
        result
    }

    fn is_track_symbol_supported(&self) -> bool {
        self.tracker.ref_(self).is_track_symbol_supported()
    }

    fn disable_track_symbol(&self) {
        self.is_track_symbol_disabled.set(true);
    }

    fn reenable_track_symbol(&self) {
        self.is_track_symbol_disabled.set(false);
    }

    fn report_truncation_error(&self) {
        self.tracker.ref_(self).report_truncation_error()
    }

    fn module_resolver_host(
        &self,
    ) -> Option<IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory> {
        self.tracker.ref_(self).module_resolver_host()
    }

    fn is_module_resolver_host_supported(&self) -> bool {
        self.tracker.ref_(self).is_module_resolver_host_supported()
    }

    fn track_referenced_ambient_module(
        &self,
        decl: Id<Node>, /*ModuleDeclaration*/
        symbol: Id<Symbol>,
    ) -> io::Result<()> {
        self.tracker
            .ref_(self)
            .track_referenced_ambient_module(decl, symbol)
    }

    fn is_track_referenced_ambient_module_supported(&self) -> bool {
        self.tracker
            .ref_(self)
            .is_track_referenced_ambient_module_supported()
    }

    fn track_external_module_symbol_of_import_type_node(&self, symbol: Id<Symbol>) {
        self.tracker
            .ref_(self)
            .track_external_module_symbol_of_import_type_node(symbol)
    }
}

impl_has_arena!(NodeBuilderContextWrappedSymbolTracker);

pub struct NodeBuilderContext {
    arena: *const AllArenas,
    pub(super) _arena_id: Cell<Option<Id<NodeBuilderContext>>>,
    pub(super) enclosing_declaration: Cell<Option<Id<Node>>>,
    pub flags: Cell<NodeBuilderFlags>,
    pub(super) tracker: Cell<Id<Box<dyn SymbolTracker>>>,

    pub encountered_error: Cell<bool>,
    pub reported_diagnostic: Cell<bool>,
    pub visited_types: Rc<RefCell<Option<HashSet<TypeId>>>>,
    pub symbol_depth: Rc<RefCell<Option<HashMap<String, usize>>>>,
    pub infer_type_parameters: Id<Option<Vec<Id<Type /*TypeParameter*/>>>>,
    pub approximate_length: Cell<usize>,
    pub truncating: Cell<Option<bool>>,
    pub type_parameter_symbol_list: Rc<RefCell<Option<HashSet<SymbolId>>>>,
    pub type_parameter_names: Id<Option<HashMap<TypeId, Id<Node /*Identifier*/>>>>,
    pub type_parameter_names_by_text: Rc<RefCell<Option<HashSet<String>>>>,
    pub type_parameter_names_by_text_next_name_count: Rc<RefCell<Option<HashMap<String, usize>>>>,
    pub used_symbol_names: Rc<RefCell<Option<HashSet<String>>>>,
    pub remapped_symbol_names: Rc<RefCell<Option<HashMap<SymbolId, String>>>>,
    pub reverse_mapped_stack: Id<Option<Vec<Id<Symbol /*ReverseMappedSymbol*/>>>>,
}

impl NodeBuilderContext {
    pub fn new(
        enclosing_declaration: Option<Id<Node>>,
        flags: NodeBuilderFlags,
        tracker: Id<Box<dyn SymbolTracker>>,
        arena: &impl HasArena,
    ) -> Id<Self> {
        arena.alloc_node_builder_context(Self {
            arena: arena.arena(),
            _arena_id: Default::default(),
            enclosing_declaration: Cell::new(enclosing_declaration),
            flags: Cell::new(flags),
            tracker: Cell::new(tracker),
            encountered_error: Default::default(),
            reported_diagnostic: Default::default(),
            visited_types: Default::default(),
            symbol_depth: Default::default(),
            infer_type_parameters: arena.alloc_option_vec_type(Default::default()),
            approximate_length: Default::default(),
            truncating: Default::default(),
            type_parameter_symbol_list: Default::default(),
            type_parameter_names: arena.alloc_option_type_parameter_names(Default::default()),
            type_parameter_names_by_text: Default::default(),
            type_parameter_names_by_text_next_name_count: Default::default(),
            used_symbol_names: Default::default(),
            remapped_symbol_names: Default::default(),
            reverse_mapped_stack: arena.alloc_option_vec_symbol(Default::default()),
        })
    }

    pub fn arena_id(&self) -> Id<Self> {
        self._arena_id.get().unwrap()
    }

    pub fn set_arena_id(&self, id: Id<Self>) {
        self._arena_id.set(Some(id));
    }

    pub fn maybe_enclosing_declaration(&self) -> Option<Id<Node>> {
        self.enclosing_declaration.get()
    }

    pub fn enclosing_declaration(&self) -> Id<Node> {
        self.enclosing_declaration.get().unwrap()
    }

    pub fn set_enclosing_declaration(&self, enclosing_declaration: Option<Id<Node>>) {
        self.enclosing_declaration.set(enclosing_declaration);
    }

    pub fn flags(&self) -> NodeBuilderFlags {
        self.flags.get()
    }

    pub fn set_flags(&self, flags: NodeBuilderFlags) {
        self.flags.set(flags);
    }

    pub fn tracker_ref(&self) -> debug_cell::Ref<'_, Box<dyn SymbolTracker>> {
        self.tracker.get().ref_(self)
    }

    pub fn tracker(&self) -> Id<Box<dyn SymbolTracker>> {
        self.tracker.get()
    }

    pub fn set_tracker(&self, tracker: Id<Box<dyn SymbolTracker>>) {
        self.tracker.set(tracker);
    }

    pub fn encountered_error(&self) -> bool {
        self.encountered_error.get()
    }

    pub fn set_encountered_error(&self, encountered_error: bool) {
        self.encountered_error.set(encountered_error);
    }

    pub fn reported_diagnostic(&self) -> bool {
        self.reported_diagnostic.get()
    }

    pub fn set_reported_diagnostic(&self, reported_diagnostic: bool) {
        self.reported_diagnostic.set(reported_diagnostic);
    }

    pub fn increment_approximate_length_by(&self, amount: usize) {
        self.approximate_length
            .set(self.approximate_length.get() + amount);
    }

    pub fn maybe_used_symbol_names(&self) -> Ref<Option<HashSet<String>>> {
        (*self.used_symbol_names).borrow()
    }

    pub fn maybe_used_symbol_names_mut(&self) -> RefMut<Option<HashSet<String>>> {
        (*self.used_symbol_names).borrow_mut()
    }

    pub fn remapped_symbol_names(&self) -> Ref<HashMap<SymbolId, String>> {
        ref_unwrapped(&*self.remapped_symbol_names)
    }

    pub fn remapped_symbol_names_mut(&self) -> RefMut<HashMap<SymbolId, String>> {
        ref_mut_unwrapped(&*self.remapped_symbol_names)
    }
}

impl Clone for NodeBuilderContext {
    fn clone(&self) -> Self {
        Self {
            arena: self.arena,
            _arena_id: Default::default(),
            enclosing_declaration: Cell::new(self.maybe_enclosing_declaration()),
            flags: self.flags.clone(),
            tracker: self.tracker.clone(),
            encountered_error: self.encountered_error.clone(),
            reported_diagnostic: self.reported_diagnostic.clone(),
            visited_types: self.visited_types.clone(),
            symbol_depth: self.symbol_depth.clone(),
            infer_type_parameters: self.infer_type_parameters.clone(),
            approximate_length: self.approximate_length.clone(),
            truncating: self.truncating.clone(),
            type_parameter_symbol_list: self.type_parameter_symbol_list.clone(),
            type_parameter_names: self.type_parameter_names.clone(),
            type_parameter_names_by_text: self.type_parameter_names_by_text.clone(),
            type_parameter_names_by_text_next_name_count: self
                .type_parameter_names_by_text_next_name_count
                .clone(),
            used_symbol_names: self.used_symbol_names.clone(),
            remapped_symbol_names: self.remapped_symbol_names.clone(),
            reverse_mapped_stack: self.reverse_mapped_stack.clone(),
        }
    }
}

impl_has_arena!(NodeBuilderContext);
